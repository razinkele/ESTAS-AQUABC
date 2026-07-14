# Shiny-Modules Rearchitecture — Phase 0 (Shared Contract) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Introduce the `RunController` + `AppState` shared-state contract and rewire the still-monolithic `server()` to use it — with **zero id changes and a DOM-identical app** — so the plumbing every future module needs is proven before any namespacing.

**Architecture:** Extract the per-session run/build state (subprocess handle, thread-appended log buffers, run/build methods, executable/config signals) out of `server()`'s closure into a plain `RunController` class in a new `shiny_app/app_state.py`, plus an `AppState` dataclass carrying the cross-tab reactive signals. `server()` constructs both once and the existing handlers call into them instead of touching closure globals. No `@module.ui`/`@module.server`, no id renames — that starts in Phase 1.

**Tech Stack:** Python 3.10+, Shiny for Python 1.5.x (`shiny.reactive`), pytest, Playwright + Selenium integration tests.

**Spec:** `docs/superpowers/specs/2026-07-14-app-py-shiny-modules-rearchitecture-design.md` (§4–§5, §7 Phase 0).

## Global Constraints

- **DOM-identical / behavior-identical:** no widget id changes, no UI layout changes in this phase. The full Playwright + Selenium suite must stay green at the final gate.
- **Import-fallback pattern (every new import):** `try: from shiny_app.<mod> import <names>` / `except ImportError: from <mod> import <names>` — supports running as a script from inside `shiny_app/`.
- **Named logger:** modules use `logger = logging.getLogger("AQUABC")` (same named logger as `app.py`).
- **Per-session only:** `RunController` and `AppState` are constructed **inside `server()`**, never at import/module level.
- **Threading semantics preserved verbatim:** log buffers stay plain lists (atomic `append`), background threads append, renders poll via `reactive.invalidate_later(0.5)`. Reactive `.set()` calls stay in the reactive context (never inside a worker thread).
- **Commit after every task** (frequent commits). Run on branch `refactor/shiny-modules-rearchitecture`.

## File Structure

- **Create** `shiny_app/app_state.py` — `RunController` (run/build session: process, log buffers, `execute_build`/`start_run`/`stop`/`is_running`, plus `exe_list_version`/`active_executable`/`build_config`/`command_config` attributes wired by `server()`) and `AppState` (dataclass of cross-tab reactive signals). One responsibility: the shared per-session state contract.
- **Create** `tests/python/test_run_controller.py` — unit tests for `RunController` (buffers, `is_running`, `execute_build`/`start_run`/`stop` via a monkeypatched `subprocess.Popen`, and `AppState` construction).
- **Modify** `shiny_app/app.py` — remove the six closure state boxes and the `_execute_build_process` def; construct `state = AppState(...)` at the top of `server()`; rewire build/run/stop handlers and log renders to `state.run`; publish output-selection + config-version signals into `state`; register `build_config`/`command_config`/`active_executable`.
- **Modify** `shiny_app/app.py` (Task 7) — route the goto handlers through `state.navigate` (which wraps the existing `ui.update_radio_buttons("navigation", …)`). The `ui_scripts.py` `aquabc_navigate` JS handler + `send_custom_message` upgrade is **deferred to Phase 1**, where namespacing first requires it.

---

### Task 1: `RunController` — state, buffers, `is_running`

**Files:**
- Create: `shiny_app/app_state.py`
- Test: `tests/python/test_run_controller.py`

**Interfaces:**
- Produces: `RunController(root: str)` with attributes `process`, `running: bool`, `last_run_time`, `progress: dict`, `build_log_lines: list[str]`, `run_log_lines: list[str]`, `exe_list_version`, `active_executable`, `build_config`, `command_config` (last four default `None`, set by `server()`); method `is_running() -> bool`.

- [ ] **Step 1: Write the failing test**

```python
# tests/python/test_run_controller.py
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m pytest tests/python/test_run_controller.py -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'shiny_app.app_state'` (or `app_state`).

- [ ] **Step 3: Write minimal implementation**

```python
# shiny_app/app_state.py
"""Shared per-session state contract for the AQUABC Shiny app.

Holds the run/build session (``RunController``) and the cross-tab reactive
signal bundle (``AppState``). Constructed once inside ``server()`` — never at
import time.
"""
import logging
from dataclasses import dataclass
from typing import Callable, Optional

logger = logging.getLogger("AQUABC")


class RunController:
    """Per-session run/build engine: subprocess handle, thread-appended log
    buffers, and the build/run/stop methods. The reactive-context fields
    (``exe_list_version``, ``active_executable``, ``build_config``,
    ``command_config``) are assigned by ``server()`` after construction.
    """

    def __init__(self, root: str):
        self.root = root
        self.process = None
        self.running = False
        self.last_run_time = None
        self.progress = {"elapsed": "", "rows": 0, "size_kb": 0, "status": "idle"}
        self.build_log_lines: list = []
        self.run_log_lines: list = ["Ready.\n"]
        # Wired in server() (need a reactive context):
        self.exe_list_version = None    # reactive.Value(int)
        self.active_executable = None   # reactive.Value(str | None)
        self.build_config = None        # Callable[[], dict]  (registered by build handlers)
        self.command_config = None      # Callable[[], list]  (registered = build_estas_command)

    def is_running(self) -> bool:
        return self.process is not None and self.process.poll() is None
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m pytest tests/python/test_run_controller.py -v`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add shiny_app/app_state.py tests/python/test_run_controller.py
git commit -m "feat(shiny): RunController skeleton — run/build session state + is_running"
```

---

### Task 2: `RunController.execute_build` (port `_execute_build_process`)

**Files:**
- Modify: `shiny_app/app_state.py`
- Test: `tests/python/test_run_controller.py`

**Interfaces:**
- Consumes: `RunController` (Task 1); leaf module `build_commands` (not needed here), stdlib `subprocess`/`time`/`traceback`.
- Produces: `RunController.execute_build(compiler_path, build_type, exe_name, clean_first, action_name) -> None` — synchronous; appends to `self.build_log_lines`; safe to call as a thread target.

**Port note:** move the body of `_execute_build_process` (`shiny_app/app.py:1007-1080`) into this method **verbatim**, applying exactly these renames: `_build_log_lines` → `self.build_log_lines`, `ROOT` → `self.root`. Keep the `subprocess`/`time`/`traceback`/`logger` references (add the imports).

- [ ] **Step 1: Write the failing test**

```python
# append to tests/python/test_run_controller.py
import subprocess as _subprocess


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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m pytest tests/python/test_run_controller.py -k execute_build -v`
Expected: FAIL — `AttributeError: 'RunController' object has no attribute 'execute_build'`.

- [ ] **Step 3: Write minimal implementation**

Add the stdlib imports to the top of `shiny_app/app_state.py`:

```python
import subprocess
import time
import traceback
```

Add the method to `RunController` (body copied verbatim from `app.py:1007-1080` with the two renames; note `self.build_log_lines` and `self.root`):

```python
    def execute_build(self, compiler_path, build_type, exe_name,
                      clean_first, action_name):
        """Shared build/rebuild subprocess logic — call as a thread target."""
        start_time = time.time()
        try:
            logger.info(f"{action_name} thread started for {exe_name}")
            if clean_first:
                clean_msg = ("Cleaning all build artifacts" if action_name == "Rebuild"
                             else "Cleaning previous build")
                self.build_log_lines.append(f"\n=== {clean_msg} ===\n")
                p = subprocess.Popen(["make", "clean-lib"], cwd=self.root,
                                     stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
                for line in p.stdout:
                    self.build_log_lines.append(line)
                p.wait()
            build_verb = "Rebuilding" if action_name == "Rebuild" else "Building"
            self.build_log_lines.append(f"\n=== {build_verb} library and executable ===\n")
            cmd = ["make", f"FC={compiler_path}", f"BUILD_TYPE={build_type}", "build-named"]
            logger.info(f"Running: {' '.join(cmd)}")
            p = subprocess.Popen(cmd, cwd=self.root, stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT, text=True)
            for line in p.stdout:
                self.build_log_lines.append(line)
                if len(self.build_log_lines) > 500:
                    del self.build_log_lines[:100]
            p.wait()
            elapsed = time.time() - start_time
            self.build_log_lines.append("-" * 50 + "\n")
            if p.returncode == 0:
                self.build_log_lines.append(f"✓ {action_name} completed successfully!\n")
                self.build_log_lines.append(f"  Executable: {exe_name}\n")
                self.build_log_lines.append(f"  Time: {elapsed:.1f}s\n")
            else:
                self.build_log_lines.append(
                    f"✗ {action_name} failed with return code {p.returncode}\n")
            self.build_log_lines.append("=" * 50 + "\n")
            logger.info(f"{action_name} thread completed for {exe_name}")
        except Exception as e:
            logger.error(f"{action_name} thread error: {e}\n{traceback.format_exc()}")
            self.build_log_lines.append(f"\nError: {e}\n")
            self.build_log_lines.append(traceback.format_exc())
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m pytest tests/python/test_run_controller.py -k execute_build -v`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add shiny_app/app_state.py tests/python/test_run_controller.py
git commit -m "feat(shiny): RunController.execute_build (port _execute_build_process)"
```

---

### Task 3: `RunController.start_run` and `.stop` (port `on_run` worker + stop handlers)

**Files:**
- Modify: `shiny_app/app_state.py`
- Test: `tests/python/test_run_controller.py`

**Interfaces:**
- Consumes: leaf module `compiler_env` (`is_intel_executable`, `get_intel_setvars_path`, `build_intel_wrapped_command`, `get_run_environment`), `output_data` (`get_output_files_info`, `format_elapsed`); stdlib `select`, `os`, `datetime`.
- Produces: `RunController.start_run(estas_cmd: list[str], exe_name: str) -> None` (synchronous worker body — call as a thread target; sets `self.process`/`self.running`/`self.last_run_time` and appends to `self.run_log_lines` — it does **not** clear them; the one `_log_lines.clear()` stays in `on_run`'s prep) and `RunController.stop(reset_progress: bool = False) -> None` (terminate/kill the process, append status).

**Port note:** `start_run` is the body of `on_run`'s inner `_work` (`app.py:3986-4123`) with `_log_lines`→`self.run_log_lines`, `_model_process[0]`→`self.process`, `_model_running[0]`→`self.running`, `_last_run_time[0]`→`self.last_run_time`, and `exe_name`/`estas_cmd` becoming parameters (the original's `is_release` is used only in `on_run`'s prep for a log line, so it stays in `on_run` and is **not** a `start_run` parameter). `stop` is the shared body of `on_stop_run`/`on_dashboard_stop` (`app.py:4468-4490`) with the same renames; `reset_progress=True` also resets `self.progress` (the dashboard-stop variant, `app.py:4515`).

- [ ] **Step 1: Write the failing test**

```python
# append to tests/python/test_run_controller.py
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m pytest tests/python/test_run_controller.py -k "stop or start_run" -v`
Expected: FAIL — `AttributeError: 'RunController' object has no attribute 'stop'` / `'start_run'`.

- [ ] **Step 3: Write minimal implementation**

Add imports at the top of `shiny_app/app_state.py`:

```python
import os
import select
from datetime import datetime

try:
    from shiny_app import compiler_env, output_data
except ImportError:
    import compiler_env, output_data
```

Add `stop` (copied from `app.py:4468-4490` + the `4515` progress reset, renamed):

```python
    def stop(self, reset_progress: bool = False):
        """Terminate the running model (graceful, then force). Appends status."""
        process = self.process
        if process and process.poll() is None:
            try:
                process.terminate()
                self.run_log_lines.append("\n⚠️ Stop requested - terminating model...\n")
                try:
                    process.wait(timeout=3)
                    self.run_log_lines.append("Model terminated gracefully.\n")
                except subprocess.TimeoutExpired:
                    process.kill()
                    self.run_log_lines.append("Model force killed.\n")
                self.running = False
                self.process = None
                if reset_progress:
                    self.progress = {"elapsed": "", "rows": 0, "size_kb": 0, "status": "idle"}
            except Exception as e:
                self.run_log_lines.append(f"Error stopping model: {e}\n")
                logger.error(f"Error stopping model: {e}")
        else:
            self.run_log_lines.append("No model is currently running.\n")
```

Add `start_run` (body of `on_run`'s `_work`, `app.py:3986-4123`, renamed; `estas_cmd`/`exe_name` are params):

```python
    def start_run(self, estas_cmd, exe_name):
        """Model-run worker — call as a thread target. Ported from on_run._work."""
        start_time = time.time()
        logger.info("Run thread started")
        exec_cmd = [c for c in estas_cmd if c]
        try:
            use_shell = False
            final_cmd = exec_cmd
            run_env = os.environ.copy()
            if compiler_env.is_intel_executable(exe_name):
                setvars_path = compiler_env.get_intel_setvars_path()
                if setvars_path:
                    final_cmd, use_shell = compiler_env.build_intel_wrapped_command(exec_cmd)
                    self.run_log_lines.append(f"ℹ️  Sourcing Intel environment: {setvars_path}\n")
                    logger.info(f"Using Intel wrapper with setvars: {setvars_path}")
                else:
                    run_env = compiler_env.get_run_environment()
            else:
                run_env = compiler_env.get_run_environment()
            logger.info(f"Executing: {final_cmd if isinstance(final_cmd, str) else ' '.join(final_cmd)}")
            p = subprocess.Popen(final_cmd, cwd=self.root, stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT, text=True, bufsize=1, env=run_env,
                                 shell=use_shell, executable="/bin/bash" if use_shell else None)
            self.process = p
            self.running = True
            self.last_run_time = datetime.now()
            last_update = time.time()
            spinner = ['|', '/', '-', '\\']
            spinner_idx = 0
            last_lines = 0
            while p.poll() is None:
                if p.stdout:
                    try:
                        readable, _, _ = select.select([p.stdout], [], [], 0.5)
                        if readable:
                            line = p.stdout.readline()
                            if line:
                                self.run_log_lines.append(line)
                                while len(self.run_log_lines) > 500:
                                    self.run_log_lines.pop(0)
                    except Exception:
                        time.sleep(0.5)
                now = time.time()
                if now - last_update >= 2.0:
                    elapsed = now - start_time
                    output_info = output_data.get_output_files_info()
                    file_count = output_info.get("file_count", 0)
                    size_kb = output_info.get("size_kb", 0)
                    out_folder = output_info.get("folder", "OUTPUTS")
                    new_files = file_count - last_lines if file_count > last_lines else 0
                    if new_files > 0:
                        last_lines = file_count
                    spinner_char = spinner[spinner_idx % len(spinner)]
                    spinner_idx += 1
                    progress_msg = (f"\r{spinner_char} Running... "
                                    f"Elapsed: {output_data.format_elapsed(elapsed)} | "
                                    f"{out_folder}/: {file_count} files ({size_kb:.1f} KB)\n")
                    if self.run_log_lines and self.run_log_lines[-1].startswith(("\r", "|", "/", "-", "\\")):
                        self.run_log_lines[-1] = progress_msg
                    else:
                        self.run_log_lines.append(progress_msg)
                    last_update = now
            if p.stdout:
                remaining = p.stdout.read()
                if remaining:
                    self.run_log_lines.append(remaining)
            p.wait()
            rc = p.returncode
            elapsed = time.time() - start_time
            self.run_log_lines.append("-" * 50 + "\n")
            final_output_info = output_data.get_output_files_info()
            final_files = final_output_info.get("file_count", 0)
            final_out = final_output_info.get("out_files", 0)
            final_bin = final_output_info.get("bin_files", 0)
            final_size = final_output_info.get("size_kb", 0)
            out_folder = final_output_info.get("folder", "OUTPUTS")
            if rc == 0:
                self.run_log_lines.append("✓ Model run completed successfully!\n")
                self.run_log_lines.append(f"  Total time: {output_data.format_elapsed(elapsed)}\n")
                self.run_log_lines.append(f"  Output folder: {out_folder}/\n")
                self.run_log_lines.append(f"  Files: {final_files} total ({final_out} .out, {final_bin} .bin), {final_size:.1f} KB\n")
            else:
                self.run_log_lines.append(f"✗ Model run failed with return code {rc}\n")
                self.run_log_lines.append(f"  Total time: {output_data.format_elapsed(elapsed)}\n")
            self.run_log_lines.append("=" * 50 + "\n")
            logger.info(f"Model run finished: rc={rc}, elapsed={elapsed:.1f}s")
        except Exception as e:
            self.run_log_lines.append(f"\n❌ Error running model: {e}\n")
            logger.error(f"Model run error: {e}")
        finally:
            self.process = None
            self.running = False
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m pytest tests/python/test_run_controller.py -v`
Expected: PASS (all tests, incl. the 2 `stop` tests and `test_start_run_success_path`).

- [ ] **Step 5: Commit**

```bash
git add shiny_app/app_state.py tests/python/test_run_controller.py
git commit -m "feat(shiny): RunController.start_run + stop (port on_run worker + stop handlers)"
```

---

### Task 4: `AppState` dataclass

**Files:**
- Modify: `shiny_app/app_state.py`
- Test: `tests/python/test_run_controller.py`

**Interfaces:**
- Produces: `AppState` dataclass with fields `run: RunController`, `navigate: Callable[[str], None]`, `selected_output_dir`, `selected_output_file`, `selected_output_format`, `output_config_version`, `sim_config_version` (the reactive fields hold `reactive.Value`s in `server()`; the test uses plain stand-ins).

- [ ] **Step 1: Write the failing test**

```python
# append to tests/python/test_run_controller.py
try:
    from shiny_app.app_state import AppState
except ImportError:
    from app_state import AppState


def test_appstate_holds_fields():
    rc = RunController(root="/tmp")
    st = AppState(
        run=rc, navigate=lambda nav_id: None,
        selected_output_dir="OUTPUTS", selected_output_file="OUTPUT.csv",
        selected_output_format="text", output_config_version=0, sim_config_version=0,
    )
    assert st.run is rc
    assert callable(st.navigate)
    assert st.selected_output_dir == "OUTPUTS"
    assert st.selected_output_format == "text"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m pytest tests/python/test_run_controller.py -k appstate -v`
Expected: FAIL — `ImportError: cannot import name 'AppState'`.

- [ ] **Step 3: Write minimal implementation**

```python
# shiny_app/app_state.py — append at module level
@dataclass
class AppState:
    """Cross-tab reactive signal bundle. Reactive fields hold reactive.Value
    in server(); one RunController carries the run/build session."""
    run: RunController
    navigate: Callable[[str], None]
    selected_output_dir: object          # reactive.Value(str)   — published by output_browser
    selected_output_file: object         # reactive.Value(str)   — published by output_browser
    selected_output_format: object       # reactive.Value(str)   — published by output_browser
    output_config_version: object        # reactive.Value(int)   — output-config save → dashboard
    sim_config_version: object           # reactive.Value(int)   — sim-config save → dashboard
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m pytest tests/python/test_run_controller.py -k appstate -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add shiny_app/app_state.py tests/python/test_run_controller.py
git commit -m "feat(shiny): AppState dataclass (cross-tab reactive signal bundle)"
```

---

### Task 5: Construct `state` in `server()` and rewire the build handlers

**Files:**
- Modify: `shiny_app/app.py` (import; `server()` top ~538-546; `_build_log_lines` box @753; `_execute_build_process` @990-1081; `on_build` @1084-1132; `on_rebuild` @1136-1181; `build_log` render @937-943)

**Interfaces:**
- Consumes: `RunController`, `AppState` (Tasks 1-4).
- Produces: a `state: AppState` local in `server()` whose `state.run` owns the build log + build execution; `state.run.exe_list_version` / `state.run.active_executable` are the `reactive.Value`s previously named `_exe_list_version` (@756).

- [ ] **Step 1: Add the import** (top of `app.py`, near the other leaf imports ~line 211)

```python
try:
    from shiny_app.app_state import RunController, AppState
except ImportError:
    from app_state import RunController, AppState
```

- [ ] **Step 2: Construct `state` at the top of `server()`** (immediately after the init logging block, ~line 546)

```python
    # --- Shared per-session state contract (Phase 0) ---
    # navigate() wraps the proven sync mechanism the goto handlers already use
    # (ui.update_radio_buttons on the app-level "navigation" input). Phase 1
    # upgrades it to session.send_custom_message + a nav-JS handler once modules
    # are namespaced and can no longer reach the global input by id.
    state = AppState(
        run=RunController(root=ROOT),
        navigate=lambda nav_id: ui.update_radio_buttons("navigation", selected=nav_id),
        selected_output_dir=reactive.Value("OUTPUTS"),
        selected_output_file=reactive.Value(""),
        selected_output_format=reactive.Value("text"),
        output_config_version=reactive.Value(0),
        sim_config_version=reactive.Value(0),
    )
    state.run.exe_list_version = reactive.Value(0)      # replaces _exe_list_version
    state.run.active_executable = reactive.Value(None)
    run = state.run                                     # local alias for brevity
```

- [ ] **Step 3: Delete the moved boxes/defs and repoint references**

- Delete `_build_log_lines = []` (@753) and the whole `_execute_build_process` def (@990-1081).
- Delete `_exe_list_version = reactive.Value(0)` (@756); replace its two references (`_exe_list_version.get()` @828, `_exe_list_version.set(...)` @956) with `run.exe_list_version.get()` / `run.exe_list_version.set(run.exe_list_version.get() + 1)`.
- In `build_log` render (@937-943), replace `_build_log_lines` with `run.build_log_lines`.
- In `on_build` (@1084-1132) and `on_rebuild` (@1136-1181): replace every `_build_log_lines` with `run.build_log_lines`, and replace the `_do_build`/`_do_rebuild` thread bodies' call to `_execute_build_process(...)` with `run.execute_build(...)` (identical keyword args). After a successful build the code already re-renders the exe list via `run.exe_list_version`; leave that behavior as-is.

Resulting `on_build` thread body (for reference):

```python
        def _do_build():
            run.execute_build(compiler_path=_compiler_path, build_type=_build_type,
                              exe_name=_exe_name, clean_first=_clean_first, action_name="Build")
```

- [ ] **Step 4: Verify import + compile + suite**

Run: `python -m py_compile shiny_app/app.py && python -c "import shiny_app.app_state" && python -m pytest tests/python -q`
Expected: compiles; all tests pass (155 existing + new `test_run_controller.py`).

- [ ] **Step 5: Commit**

```bash
git add shiny_app/app.py
git commit -m "refactor(shiny): construct AppState in server(); build handlers use run.execute_build/build_log"
```

---

### Task 6: Rewire the run + stop handlers to `state.run`

**Files:**
- Modify: `shiny_app/app.py` (`_log_lines` box @669; run-state boxes @3911-3914; `run_log` render @4127-4131; `run_log_mini` @660; `on_run` @3918-4125; `on_stop_run` @4465-4490; `on_dashboard_stop` @4494-4520; and the dashboard renders reading `_model_process`/`_last_run_time`/`_model_running`/`_model_progress`)

**Interfaces:**
- Consumes: `state.run` (`run_log_lines`, `start_run`, `stop`, `process`, `running`, `last_run_time`, `progress`, `is_running`, `command_config`).

- [ ] **Step 1: Delete the moved boxes and repoint log references**

- Delete `_log_lines = ["Ready.\n"]` (@669) and `_model_process`/`_model_running`/`_last_run_time`/`_model_progress` (@3911-3914).
- Replace every `_log_lines` with `run.run_log_lines`, `_model_process[0]` with `run.process`, `_model_running[0]` with `run.running`, `_last_run_time[0]` with `run.last_run_time`, `_model_progress[0]` with `run.progress` throughout `server()` (grep to confirm none remain).

- [ ] **Step 2: Rewire `on_run` (@3918-4125)**

Keep the reactive prep verbatim (it reads inputs, validates constants, appends header lines to `run.run_log_lines`, computes `estas_cmd`/`exe_name`/`is_release`). Replace the inner `_work` def + `threading.Thread(target=_work...)` with a call into the controller:

```python
        import threading  # already imported at module top; shown for clarity
        threading.Thread(
            target=run.start_run, args=(estas_cmd, exe_name),
            daemon=True, name="RunThread",
        ).start()
```

(Delete the now-moved `_work` body — it lives in `RunController.start_run`.)

- [ ] **Step 3: Rewire the stop handlers**

Replace the body of `on_stop_run` (@4465-4490) with `run.stop()` and `on_dashboard_stop` (@4494-4520) with `run.stop(reset_progress=True)`, keeping the `logger.info(...)` lines and the `@reactive.event` decorators.

```python
    @reactive.effect
    @reactive.event(input.stop_run)
    def on_stop_run():
        logger.info("User clicked Stop button")
        run.stop()

    @reactive.effect
    @reactive.event(input.dashboard_stop)
    def on_dashboard_stop():
        logger.info("User clicked Dashboard Stop button")
        run.stop(reset_progress=True)
```

- [ ] **Step 4: Verify compile + suite**

Run: `python -m py_compile shiny_app/app.py && python -m pytest tests/python -q`
Expected: compiles; all tests pass.

- [ ] **Step 5: Commit**

```bash
git add shiny_app/app.py
git commit -m "refactor(shiny): run + stop handlers delegate to run.start_run/run.stop; drop state boxes"
```

---

### Task 7: Route goto navigation through `state.navigate`

**Files:**
- Modify: `shiny_app/app.py` (`navigate_to_build` @980-982; `navigate_to_model_config` @986-988)

**Interfaces:**
- Consumes: `state.navigate` (wired in Task 5 to `ui.update_radio_buttons("navigation", selected=nav_id)` — the exact mechanism these two handlers already use, so behavior is identical).

**Why no JS this phase:** `ui.update_radio_buttons("navigation", ...)` resolves the `navigation` id in the *current* namespace. In the Phase-0 monolith that's the global namespace, so it works — and it's literally the current code. Once modules are namespaced (Phase 1), a module can no longer reach the global `navigation` input this way; the Phase 1 plan upgrades `state.navigate` to `session.send_custom_message("aquabc_navigate", …)` and adds the ~3-line `Shiny.addCustomMessageHandler("aquabc_navigate", …)` to `nav_script`. Building the JS handler now would be dead code, so it is deferred to the phase that needs it.

- [ ] **Step 1: Repoint the two goto handlers**

Replace the body of `navigate_to_build` (@980-982) with `state.navigate("nav_model_build")` and `navigate_to_model_config` (@986-988) with `state.navigate("nav_model_control")`, keeping the `@reactive.effect`/`@reactive.event` decorators:

```python
    @reactive.effect
    @reactive.event(input.goto_build)
    def navigate_to_build():
        """Navigate to the Model Build panel"""
        state.navigate("nav_model_build")

    @reactive.effect
    @reactive.event(input.goto_model_config)
    def navigate_to_model_config():
        """Navigate to the Model Config panel from dashboard"""
        state.navigate("nav_model_control")
```

- [ ] **Step 2: Verify compile + suite**

Run: `python -m py_compile shiny_app/app.py && python -m pytest tests/python -q`
Expected: compiles; all tests pass.

- [ ] **Step 3: Commit**

```bash
git add shiny_app/app.py
git commit -m "refactor(shiny): goto handlers route through state.navigate"
```

---

### Task 8: Publish output selection + config-version signals into `state`

**Files:**
- Modify: `shiny_app/app.py` (output-dir/file/format reads; `output_config_version` @2793 + `.set` @2879; `input_txt_variables` @4332; `sim_config_save_msg` trigger read @4338; register `build_config`/`command_config`)

**Interfaces:**
- Consumes: `state.selected_output_dir/file/format`, `state.output_config_version`, `state.sim_config_version`, `state.run.build_config/command_config/active_executable`.

- [ ] **Step 1: Publish the output selection (one writer)**

Add a publisher effect near the output-dir handlers (the output_browser region, ~4612):

```python
    @reactive.effect
    def _publish_output_selection():
        state.selected_output_dir.set(input.output_dir_select())
        try:
            state.selected_output_file.set(input.plot_output_file())
        except Exception:
            pass
        try:
            state.selected_output_format.set(input.output_format())
        except Exception:
            pass
```

Leave the existing `input.output_dir_select()` reads in place for this phase (behavior-identical); they will switch to `state.selected_output_*()` when their modules convert in Phases 3-4. (Publishing now proves the bus with zero behavior change.)

- [ ] **Step 2: Route the config-version signals through `state`**

- Delete `output_config_version = reactive.Value(0)` (@2793); replace its `.set(...)` (@2879) and its reader in `input_txt_variables` (@4332) with `state.output_config_version`.
- In `save_simulation_config` (@2701-2784), after a successful save, add `state.sim_config_version.set(state.sim_config_version.get() + 1)`.
- In `input_txt_variables` (@4332), replace the trigger read `_ = sim_config_save_msg.get()` (@4338) with `_ = state.sim_config_version.get()`.
- **Behavior note (intentional narrowing).** `sim_config_save_msg` is `.set()` on ~7 paths (reset `""`, parse/not-found, validation error, success, save-failed, exception), so today *any* of them re-fires `input_txt_variables`; the counter bumps only on a **successful** save. This narrowing is benign and DOM-invisible: `input_txt_variables` also calls `reactive.invalidate_later(5.0)` and re-reads `INPUT.txt` from disk each render, and **none** of the dropped paths write `INPUT.txt`, so displayed content is unchanged (and the 5 s poll re-fires within seconds regardless). If exact trigger-parity is ever wanted, bump `sim_config_version` on every path that could change `INPUT.txt`.

- [ ] **Step 3: Register `command_config`, `build_config`, `active_executable`**

- After `build_estas_command` is defined (@742), add: `run.command_config = build_estas_command`.
- Where `status_info`/`handle_quick_run` currently call `build_estas_command()` for the preview, leave as-is for this phase (same module); the registration documents the bus for Phase 4.
- Add a `build_config` reader near the build handlers:

```python
    def _current_build_config():
        return {
            "compiler": input.build_compiler(),
            "build_type": input.build_type(),
            "exe_name": get_target_exe_name(),
            "clean_first": input.build_clean_first(),
        }
    run.build_config = _current_build_config
```

- In `on_build_run` (@3821), replace the direct `input.build_type()` read with `run.build_config()["build_type"]` (proves the cross-module bus; behavior-identical).
- **`active_executable`: no clean producer this phase — defer the writer to Phase 4.** There is no existing "active exe" scalar: `active_executable` is only a `ui.input_select` widget, and `refresh_executables`/`init_executable_list` merely call `ui.update_select("active_executable", …)` (both manually triggered — `input.btn_refresh_executables` / init — not build-completion hooks). So in Phase 0: create `run.active_executable = reactive.Value(None)` (Task 5) as the reserved bus slot, but wire **no** writer, and keep `dashboard_exe_text` (@4319) reading `input.active_executable()` unchanged — behavior-identical. **Do not** place a `run.active_executable.set(...)` inside the build worker thread (`execute_build`/`_do_build`): that violates the "no reactive `.set()` from a worker thread" Global Constraint. The reader/writer wiring lands when `model_build`/`dashboard` convert in Phase 4.

- [ ] **Step 4: Verify compile + suite**

Run: `python -m py_compile shiny_app/app.py && python -m pytest tests/python -q`
Expected: compiles; all tests pass.

- [ ] **Step 5: Commit**

```bash
git add shiny_app/app.py
git commit -m "refactor(shiny): publish output selection + config-version + build/command config into state"
```

---

### Task 9: Full Phase-0 regression gate (DOM-identical proof)

**Files:** none changed — this task is the verification gate.

- [ ] **Step 1: Static + unit**

Run: `python -m py_compile shiny_app/app.py && python -c "import shiny_app.app_state" && python -m pytest tests/python -q`
Expected: compiles; **all** tests pass (155 existing + the new `test_run_controller.py`).

- [ ] **Step 2: Confirm no orphaned references remain**

Run: `grep -nE "_log_lines|_build_log_lines|_model_process|_model_running|_last_run_time|_model_progress|_exe_list_version|_execute_build_process" shiny_app/app.py`
Expected: **no output** (every closure box/def removed and repointed).

- [ ] **Step 3: Integration tests (the DOM-identical safety net)**

Run: `python -m pytest tests/python/test_app_playwright.py tests/python/test_app_selenium.py -v`
Expected: all pass (no id/layout changed this phase).

- [ ] **Step 4: Boot smoke via the run/verify skill**

Launch the app (per the project `run` skill), confirm: it starts without error; the Model Build tab builds/logs; the Model Config run/stop works; the Dashboard mirrors run status; nav goto buttons switch tabs. Observe behavior — do not just trust the tests.

- [ ] **Step 5: Tag the phase release**

```bash
git add -A && git commit -m "chore(release): v0.4.0 — Phase 0 shared contract (RunController + AppState)" --allow-empty
git tag v0.4.0
```

(Follow the repo's existing release process — `release.yml` will sync the README marker on `main` after merge; `git pull` afterward.)

---

## Self-Review

**Spec coverage (§5 contract → tasks):** `RunController` state/methods → Tasks 1-3; `AppState` 7 fields → Task 4; construction-in-`server()` + build rewire → Task 5; run/stop rewire → Task 6; goto navigation via `state.navigate` → Task 7 (the spec's nav-JS/`send_custom_message` upgrade is deferred to Phase 1, where namespacing first requires it — a documented refinement, not a gap); output-selection bus + `output_config_version`/`sim_config_version` + `build_config`/`command_config`/`active_executable` → Task 8; the §7 Phase-0 "DOM byte-identical" gate → Task 9. The §5.1 audit's 10 cross-readers are each lifted: output_dir/file/format (Task 8 Step 1), sim_output_dir (published alongside — the output-dir publisher covers it, consumers switch in Phase 3), build_type/build config (Task 8 Step 3), cmd_*/run_executable via `command_config` (Task 8 Step 3), active_executable (Task 8 Step 3).

**Placeholder scan:** every code step shows full code or an exact move+rename with the target shown; no TBD/TODO. Verbatim ports name exact source line ranges + the complete rename list.

**Type consistency:** `run.execute_build(compiler_path, build_type, exe_name, clean_first, action_name)` (Task 2) matches the call site (Task 5 Step 3). `run.start_run(estas_cmd, exe_name)` (Task 3) matches the thread target (Task 6 Step 2). `run.stop(reset_progress=False)` (Task 3) matches both stop handlers (Task 6 Step 3). `AppState` field names (Task 4) match the construction kwargs (Task 5 Step 2) and the publisher/reader repoints (Task 8).

**Note carried to Phase 1+ plans:** `sim_output_dir` (sim_config → output_browser) is published in Phase 0 via the output-selection effect but its *consumer* (`refresh_sim_output_dirs`) keeps its direct read until `output_browser` converts (Phase 3); flagged so the Phase 3 plan switches it to `state.selected_output_dir()`.
