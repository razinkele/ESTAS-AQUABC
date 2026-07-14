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
