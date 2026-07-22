"""Shared per-session state contract for the AQUABC Shiny app.

Holds the run/build session (``RunController``) and the cross-tab reactive
signal bundle (``AppState``). Constructed once inside ``server()`` — never at
import time.
"""
import logging
import os
import select
import subprocess
import time
import traceback

try:
    from shiny_app.config import PROCESS_SHUTDOWN_TIMEOUT
except ImportError:
    from config import PROCESS_SHUTDOWN_TIMEOUT
from collections.abc import Awaitable, Callable
from dataclasses import dataclass
from datetime import datetime

try:
    from shiny_app import compiler_env, output_data
except ImportError:
    import compiler_env
    import output_data

try:
    from shiny_app.setups import default_setup as _default_setup
except ImportError:
    from setups import default_setup as _default_setup

logger = logging.getLogger("AQUABC")


class RunController:
    """Per-session run/build engine: subprocess handle, thread-appended log
    buffers, and the build/run/stop methods. The reactive-context fields
    (``exe_list_version``, ``active_executable``, ``command_config``) are
    assigned by ``server()`` after construction.
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
        self.command_config = None      # Callable[[], list]  (registered = build_estas_command)
        # current_setup: () -> Setup ; degrades to Standard until run_control assigns the reactive
        self.current_setup = lambda: _default_setup()

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

    def stop(self, reset_progress: bool = False):
        """Terminate the running model (graceful, then force). Appends status."""
        process = self.process
        if process and process.poll() is None:
            try:
                process.terminate()
                self.run_log_lines.append("\n⚠️ Stop requested - terminating model...\n")
                try:
                    process.wait(timeout=PROCESS_SHUTDOWN_TIMEOUT)
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

    def start_run(self, estas_cmd, exe_name, env_extra=None, input_file=None):
        """Model-run worker — call as a thread target. Ported from on_run._work."""
        start_time = time.time()
        logger.info("Run thread started")
        exec_cmd = [c for c in estas_cmd if c]
        input_txt = os.path.join(self.root, input_file) if input_file else None
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
            if env_extra:
                run_env.update(env_extra)   # additive, after get_run_environment(), before Popen
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
                    output_info = output_data.get_output_files_info(input_txt_path=input_txt) if input_txt \
                        else output_data.get_output_files_info()
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
            final_output_info = output_data.get_output_files_info(input_txt_path=input_txt) if input_txt \
                else output_data.get_output_files_info()
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


@dataclass
class AppState:
    """Cross-tab reactive signal bundle. Reactive fields hold reactive.Value
    in server(); one RunController carries the run/build session."""
    run: RunController
    navigate: Callable[[str], Awaitable[None]]
    output_config_version: object        # reactive.Value(int)   — output-config save → dashboard
    sim_config_version: object           # reactive.Value(int)   — sim-config save → dashboard
