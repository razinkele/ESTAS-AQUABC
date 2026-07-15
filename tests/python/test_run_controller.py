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
    assert rc.build_config is None
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
    """Behavior-pin for the Phase-4 Task-2 contract: server() wires these five
    attributes onto RunController (command_config/build_config/run_executable_name/
    constants_config/active_executable) so dashboard reads never touch sibling-tab
    inputs directly. Pin that they're plain assignable attrs, readable without a
    live Shiny session (reactive.Value/Calc read via reactive.isolate())."""
    rc = RunController(root="/tmp")

    # Wired later by server() (None until then) — matches RunController.__init__.
    assert rc.command_config is None
    assert rc.build_config is None
    assert rc.active_executable is None
    # run_executable_name / constants_config are NEW bridges (Task 2, Step 3);
    # RunController doesn't pre-declare them, so they simply don't exist yet.
    assert not hasattr(rc, "run_executable_name")
    assert not hasattr(rc, "constants_config")

    # Simulate server()'s registrations (Steps 1-3 of the Task-2 brief).
    rc.command_config = reactive.calc(lambda: ["./ESTAS_II", "INPUT.txt"])
    rc.build_config = reactive.calc(lambda: {
        "compiler": "gfortran", "build_type": "release",
        "exe_name": "ESTAS_II_gf_release", "clean_first": False,
    })
    rc.run_executable_name = reactive.Value("ESTAS_II")
    rc.constants_config = reactive.calc(lambda: ("WCONST_01.txt", False, ""))
    rc.active_executable = reactive.Value("ESTAS_II_gf_release")

    with reactive.isolate():
        # command_config: a List[str] argv, NOT a bare executable name.
        cmd = rc.command_config()
        assert isinstance(cmd, list)
        assert cmd == ["./ESTAS_II", "INPUT.txt"]

        # build_config: the build-tab config dict.
        assert rc.build_config()["compiler"] == "gfortran"

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
