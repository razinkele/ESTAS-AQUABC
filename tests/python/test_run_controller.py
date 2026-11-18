import subprocess as _subprocess

from shiny import reactive

try:
    from shiny_app.app_state import RunController
except ImportError:
    from app_state import RunController


def test_fresh_controller_state():
    rc = RunController(root="/tmp")
    assert rc.root == "/tmp"
    assert rc.process is None
    assert rc.running is False
    assert rc.last_run_time is None
    assert rc.progress["status"] == "idle"
    assert rc.build_log_lines == []
    assert rc.run_log_lines == ["Ready.\n"]
    # reactive fields are wired later by server()
    assert rc.exe_list_version is None
    assert rc.active_executable is None
    assert rc.command_config is None


def test_is_running_false_when_no_process():
    rc = RunController(root="/tmp")
    assert rc.is_running() is False


class _FakeProc:
    def __init__(self, poll_value):
        self._poll = poll_value

    def poll(self):
        return self._poll


def test_is_running_true_while_process_alive():
    rc = RunController(root="/tmp")
    rc.process = _FakeProc(poll_value=None)   # None => still running
    assert rc.is_running() is True
    rc.process = _FakeProc(poll_value=0)       # 0 => exited
    assert rc.is_running() is False


class _FakePopen:
    """Stand-in for subprocess.Popen yielding canned stdout lines + returncode."""
    def __init__(self, lines, returncode=0):
        self.stdout = iter(lines)
        self.returncode = returncode

    def wait(self):
        return self.returncode


def test_execute_build_success_markers(monkeypatch):
    rc = RunController(root="/tmp")
    monkeypatch.setattr(
        _subprocess, "Popen",
        lambda *a, **k: _FakePopen(["compiling...\n", "linking...\n"], returncode=0),
    )
    rc.execute_build(
        compiler_path="/usr/bin/gfortran", build_type="release",
        exe_name="ESTAS_II", clean_first=False, action_name="Build",
    )
    joined = "".join(rc.build_log_lines)
    assert "Build completed successfully" in joined
    assert "ESTAS_II" in joined
    assert "compiling..." in joined


def test_execute_build_failure_marker(monkeypatch):
    rc = RunController(root="/tmp")
    monkeypatch.setattr(
        _subprocess, "Popen",
        lambda *a, **k: _FakePopen(["boom\n"], returncode=2),
    )
    rc.execute_build(
        compiler_path="/usr/bin/gfortran", build_type="release",
        exe_name="ESTAS_II", clean_first=False, action_name="Build",
    )
    joined = "".join(rc.build_log_lines)
    assert "Build failed with return code 2" in joined


def test_execute_build_clean_first(monkeypatch):
    rc = RunController(root="/tmp")
    monkeypatch.setattr(
        _subprocess, "Popen",
        lambda *a, **k: _FakePopen(["cleaning\n"], returncode=0),
    )
    rc.execute_build(
        compiler_path="/usr/bin/gfortran", build_type="release",
        exe_name="ESTAS_II", clean_first=True, action_name="Rebuild",
    )
    # clean_first=True runs `make clean-lib` first (Popen called twice)
    assert "Cleaning all build artifacts" in "".join(rc.build_log_lines)


def test_stop_no_process_message():
    rc = RunController(root="/tmp")
    rc.stop()
    assert "No model is currently running." in "".join(rc.run_log_lines)


class _KillableProc:
    def __init__(self):
        self._alive = True
        self.terminated = False
    def poll(self):
        return None if self._alive else 0
    def terminate(self):
        self.terminated = True
        self._alive = False
    def wait(self, timeout=None):
        return 0
    def kill(self):
        self._alive = False


def test_stop_terminates_running_process():
    rc = RunController(root="/tmp")
    proc = _KillableProc()
    rc.process = proc
    rc.running = True
    rc.stop(reset_progress=True)
    assert proc.terminated is True
    assert rc.process is None
    assert rc.running is False
    assert rc.progress["status"] == "idle"
    assert "terminating model" in "".join(rc.run_log_lines)


try:
    import shiny_app.app_state as _app_state_mod
except ImportError:      # running from inside shiny_app/
    import app_state as _app_state_mod


class _RunFakePopen:
    """Fake Popen for start_run: emits one line, then exits cleanly."""
    def __init__(self):
        self._polls = [None, 0]        # running once, then exited
        self._lines = ["running...\n"]
        self.returncode = 0
        self.stdout = self              # p.stdout.readline()/read()/fileno()
    def poll(self):
        return self._polls.pop(0) if self._polls else 0
    def readline(self):
        return self._lines.pop(0) if self._lines else ""
    def read(self):
        return ""
    def fileno(self):
        return 0
    def wait(self):
        return self.returncode


def test_start_run_success_path(monkeypatch):
    rc = RunController(root="/tmp")
    monkeypatch.setattr(_subprocess, "Popen", lambda *a, **k: _RunFakePopen())
    # keep the read-loop deterministic and offline
    monkeypatch.setattr(_app_state_mod.select, "select", lambda r, w, x, t: (r, [], []))
    monkeypatch.setattr(_app_state_mod.compiler_env, "is_intel_executable", lambda name: False)
    monkeypatch.setattr(_app_state_mod.compiler_env, "get_run_environment", lambda: {})
    monkeypatch.setattr(_app_state_mod.output_data, "get_output_files_info",
                        lambda: {"file_count": 0, "out_files": 0, "bin_files": 0,
                                 "size_kb": 0.0, "folder": "OUTPUTS"})
    monkeypatch.setattr(_app_state_mod.output_data, "format_elapsed", lambda s: "0.0s")

    rc.start_run(estas_cmd=["./ESTAS_II"], exe_name="ESTAS_II")

    joined = "".join(rc.run_log_lines)
    assert "Model run completed successfully" in joined
    assert rc.process is None      # finally-block cleared it
    assert rc.running is False


try:
    from shiny_app.app_state import AppState
except ImportError:
    from app_state import AppState


def test_appstate_holds_fields():
    rc = RunController(root="/tmp")
    st = AppState(
        run=rc, navigate=lambda nav_id: None,
        output_config_version=0, sim_config_version=0,
    )
    assert st.run is rc
    assert callable(st.navigate)
    assert st.output_config_version == 0
    assert st.sim_config_version == 0


def test_cross_module_bridges_assignable_and_readable():
    """Behavior-pin for the Phase-4 Task-2 contract: server() wires these four
    attributes onto RunController (command_config/run_executable_name/
    constants_config/active_executable) so dashboard reads never touch sibling-tab
    inputs directly. Pin that they're plain assignable attrs, readable without a
    live Shiny session (reactive.Value/Calc read via reactive.isolate())."""
    rc = RunController(root="/tmp")

    # Wired later by server() (None until then) — matches RunController.__init__.
    assert rc.command_config is None
    assert rc.active_executable is None
    # run_executable_name / constants_config are NEW bridges (Task 2, Step 3);
    # RunController doesn't pre-declare them, so they simply don't exist yet.
    assert not hasattr(rc, "run_executable_name")
    assert not hasattr(rc, "constants_config")

    # Simulate server()'s registrations (Steps 1-3 of the Task-2 brief).
    rc.command_config = reactive.calc(lambda: ["./ESTAS_II", "INPUT.txt"])
    rc.run_executable_name = reactive.Value("ESTAS_II")
    rc.constants_config = reactive.calc(lambda: ("WCONST_01.txt", False, ""))
    rc.active_executable = reactive.Value("ESTAS_II_gf_release")

    with reactive.isolate():
        # command_config: a List[str] argv, NOT a bare executable name.
        cmd = rc.command_config()
        assert isinstance(cmd, list)
        assert cmd == ["./ESTAS_II", "INPUT.txt"]

        # run_executable_name: bare exe-name STRING (Run-Model tab), defaults sensibly.
        assert rc.run_executable_name() == "ESTAS_II"
        assert isinstance(rc.run_executable_name(), str)

        # constants_config: the (const_file, binary_enabled, shear_file) triple.
        const_file, binary_enabled, shear_file = rc.constants_config()
        assert const_file == "WCONST_01.txt"
        assert binary_enabled is False
        assert shear_file == ""

        # active_executable: the Model-Build tab's selector (a DIFFERENT widget
        # from run_executable_name — must not collapse to the same value).
        assert rc.active_executable() == "ESTAS_II_gf_release"
        assert rc.active_executable() != rc.run_executable_name()


# --- one run per output folder ---------------------------------------------------

def _root_with_control(tmp_path, name="INPUT.txt", folder="OUTPUTS/"):
    (tmp_path / name).write_text(
        f"# PELAGIC MODEL OUTPUT FOLDER write the folder ...\n{folder}\n")
    return str(tmp_path)


def test_second_click_is_refused_until_the_run_releases(tmp_path, monkeypatch):
    root = _root_with_control(tmp_path)
    rc = RunController(root=root)
    monkeypatch.setattr(rc, "output_key", lambda input_file=None: root + "/OUTPUTS")
    assert rc.claim_run("INPUT.txt") == (True, "")
    ok, why = rc.claim_run("INPUT.txt")        # process is not even started yet
    assert not ok and "already in progress" in why
    rc.release_run()
    assert rc.claim_run("INPUT.txt")[0]
    rc.release_run()


def test_running_process_blocks_a_new_claim(tmp_path, monkeypatch):
    rc = RunController(root=str(tmp_path))
    monkeypatch.setattr(rc, "output_key", lambda input_file=None: str(tmp_path / "OUTPUTS"))
    rc.process = _FakeProc(poll_value=None)
    assert rc.claim_run()[0] is False
    rc.process = None
    assert rc.claim_run()[0] is True
    rc.release_run()


def test_other_session_cannot_run_into_the_same_output_folder(tmp_path, monkeypatch):
    a, b = RunController(root=str(tmp_path)), RunController(root=str(tmp_path))
    for rc in (a, b):
        monkeypatch.setattr(
            rc, "output_key",
            lambda input_file=None: str(tmp_path / ("OUT_B" if input_file == "B.txt" else "OUT_A")))
    assert a.claim_run("A.txt")[0]
    ok, why = b.claim_run("A.txt")
    assert not ok and "Another session" in why and "OUT_A" in why
    assert b.claim_run("B.txt")[0]             # a different output folder is free
    b.release_run()
    a.release_run()
    assert b.claim_run("A.txt")[0]             # ... and A's folder once A is done
    b.release_run()


def test_start_run_releases_the_claim_even_when_the_launch_fails(tmp_path, monkeypatch):
    rc = RunController(root=str(tmp_path))
    monkeypatch.setattr(rc, "output_key", lambda input_file=None: str(tmp_path / "OUTPUTS"))
    assert rc.claim_run()[0]
    rc.start_run(["./no_such_executable"], "no_such_executable")
    assert rc.running is False and rc.claim_run()[0]
    rc.release_run()


def test_run_handlers_claim_before_clearing_the_log():
    """A refused click must not wipe the log of the run in progress."""
    import os
    mods = os.path.join(os.path.dirname(__file__), "..", "..", "shiny_app", "modules")
    for name, func in (("run_control.py", "def on_run():"),
                       ("dashboard.py", "def handle_quick_run():")):
        src = open(os.path.join(mods, name)).read()
        body = src[src.index(func):]
        body = body[:body.index("\n    def ", 10)]
        assert "run.claim_run(" in body and "run.release_run()" in body, name
        assert "run_log_lines.clear()" not in body, name


def test_symlinked_output_folder_is_the_same_claim(tmp_path):
    import os
    (tmp_path / "OUT").mkdir()
    os.symlink(tmp_path / "OUT", tmp_path / "OUT_ALIAS")
    (tmp_path / "A.txt").write_text("# PELAGIC MODEL OUTPUT FOLDER write ...\nOUT/\n")
    (tmp_path / "B.txt").write_text("# PELAGIC MODEL OUTPUT FOLDER write ...\nOUT_ALIAS/\n")
    a, b = RunController(root=str(tmp_path)), RunController(root=str(tmp_path))
    assert a.output_key("A.txt") == str(tmp_path / "OUT")
    assert a.output_key("A.txt") == b.output_key("B.txt")
    assert a.claim_run("A.txt")[0] and not b.claim_run("B.txt")[0]
    a.release_run()


def test_control_file_is_in_use_only_while_its_run_holds_the_claim(tmp_path, monkeypatch):
    try:
        from shiny_app.app_state import control_file_in_use
    except ImportError:
        from app_state import control_file_in_use
    rc = RunController(root=str(tmp_path))
    monkeypatch.setattr(rc, "output_key", lambda input_file=None: str(tmp_path / "OUTPUTS"))
    ctl = str(tmp_path / "INPUT_x.txt")
    assert not control_file_in_use(ctl)
    assert rc.claim_run("INPUT_x.txt")[0]
    assert control_file_in_use(ctl) and not control_file_in_use(str(tmp_path / "INPUT.txt"))
    rc.release_run()
    assert not control_file_in_use(ctl)


def _folder_guard():
    try:
        from shiny_app.app_state import output_folder_change_blocked
    except ImportError:
        from app_state import output_folder_change_blocked
    return output_folder_change_blocked


_CONTROL = "# PELAGIC MODEL OUTPUT FOLDER write ...\nOUT/\n"


def test_output_folder_of_a_running_control_file_cannot_be_changed(tmp_path):
    """Real control files and the real claim: nothing about the folder is stubbed."""
    blocked = _folder_guard()
    (tmp_path / "INPUT_a.txt").write_text(_CONTROL)
    (tmp_path / "INPUT_b.txt").write_text(_CONTROL.replace("OUT/", "OUT_B/"))
    a, b = str(tmp_path / "INPUT_a.txt"), str(tmp_path / "INPUT_b.txt")
    rc = RunController(root=str(tmp_path))

    assert not blocked(a, "ELSEWHERE/")                  # no run: any edit is allowed
    assert rc.claim_run("INPUT_a.txt")[0]
    try:
        assert blocked(a, "ELSEWHERE/") and blocked(a, "ELSEWHERE")
        assert not blocked(a, "OUT/") and not blocked(a, "OUT")     # the folder it already names
        assert not blocked(b, "ELSEWHERE/")              # another control file is not this run's
    finally:
        rc.release_run()
    assert not blocked(a, "ELSEWHERE/")                  # the run has ended


def test_folder_guard_sees_the_claim_through_a_symlinked_root(tmp_path):
    """Simulation Config builds its path from its own ROOT; the claim stores a realpath."""
    import os
    blocked = _folder_guard()
    (tmp_path / "real").mkdir()
    os.symlink(tmp_path / "real", tmp_path / "alias")
    (tmp_path / "real" / "INPUT.txt").write_text(_CONTROL)
    rc = RunController(root=str(tmp_path / "real"))
    assert rc.claim_run("INPUT.txt")[0]
    try:
        assert blocked(str(tmp_path / "alias" / "INPUT.txt"), "ELSEWHERE/")
    finally:
        rc.release_run()


def test_sim_config_asks_the_folder_guard_before_saving():
    import os
    src = open(os.path.join(os.path.dirname(__file__), "..", "..", "shiny_app", "modules",
                            "sim_config.py")).read()
    guard = src.index("if output_folder_change_blocked(control_path, cfg.output_folder):")
    save = src.index("scf.save(backup=True)")
    assert guard < save and "return" in src[guard:save]
