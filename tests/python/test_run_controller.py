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
