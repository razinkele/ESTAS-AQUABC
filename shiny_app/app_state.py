"""Shared per-session state contract for the AQUABC Shiny app.

Holds the run/build session (``RunController``) and the cross-tab reactive
signal bundle (``AppState``). Constructed once inside ``server()`` — never at
import time.
"""
import logging
import subprocess
import time
import traceback
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
