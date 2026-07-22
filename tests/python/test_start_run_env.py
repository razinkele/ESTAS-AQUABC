import sys
from pathlib import Path

APP = Path(__file__).resolve().parents[2] / "shiny_app"
sys.path.insert(0, str(APP))
import app_state  # noqa: E402


def test_env_extra_is_in_popen_env_after_get_run_environment(monkeypatch):
    captured = {}

    class FakePopen:
        def __init__(self, *a, **kw):
            captured["env"] = kw.get("env")
            self.stdout = None
            self.returncode = 0
        def poll(self): return 0
        def wait(self): return 0

    monkeypatch.setattr(app_state.subprocess, "Popen", FakePopen)
    monkeypatch.setattr(app_state.compiler_env, "is_intel_executable", lambda n: False)
    monkeypatch.setattr(app_state.compiler_env, "get_run_environment",
                        lambda: {"PATH": "/x"})  # reassigns run_env, no HOLD_VOLUME
    rc = app_state.RunController(root=str(APP))
    rc.start_run(["./ESTAS_II", "INPUT_CL29.txt"], "ESTAS_II",
                 env_extra={"ESTAS_HOLD_VOLUME": "1"})
    assert captured["env"].get("ESTAS_HOLD_VOLUME") == "1"   # survived the reassignment


def test_env_extra_none_is_noop(monkeypatch):
    captured = {}

    class FakePopen:
        def __init__(self, *a, **kw):
            captured["env"] = kw.get("env")
            self.stdout = None
            self.returncode = 0
        def poll(self): return 0
        def wait(self): return 0
    monkeypatch.setattr(app_state.subprocess, "Popen", FakePopen)
    monkeypatch.setattr(app_state.compiler_env, "is_intel_executable", lambda n: False)
    monkeypatch.setattr(app_state.compiler_env, "get_run_environment", lambda: {"PATH": "/x"})
    rc = app_state.RunController(root=str(APP))
    rc.start_run(["./ESTAS_II"], "ESTAS_II")
    assert "ESTAS_HOLD_VOLUME" not in captured["env"]
