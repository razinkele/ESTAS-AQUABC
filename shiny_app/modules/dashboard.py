"""Dashboard tab as a true Shiny module (Phase 4, Task 5 -- the last inline tab).

`dashboard_ui(id)` returns `panel_dashboard`'s **content** (its internal
`panel_conditional` is stripped -- the app-level wrapper now lives in
`create_ui`: `ui.panel_conditional("input.navigation === 'nav_dashboard'",
dashboard_ui("dashboard"))`). `dashboard_server(id, state)` registers the
handlers ported verbatim from app.py's server(): `copy_dashboard_log`,
`navigate_to_model_config` (`goto_model_config` -> `state.navigate(
"nav_model_control")`), `handle_quick_run`, `dashboard_run_log`,
`run_timer_display`, `system_status_compact`, `dashboard_status_text`,
`dashboard_exe_text`, `dashboard_last_run_text`, `input_txt_variables`,
`on_dashboard_stop`.

Dashboard is a pure consumer of the shared `RunController` (`run = state.run`):
all cross-tab reads were already routed through `run.command_config()` /
`run.run_executable_name()` / `run.constants_config()` /
`run.active_executable()` in Task 2, so this module reads NO sibling
`input.X` values -- only its own namespaced inputs (`quick_run`,
`dashboard_stop`, `goto_model_config`, `btn_copy_dashboard_log`).

Keeps a module-local `DEFAULT_CONSTANTS_FILE` for the Quick Run fallback
(run_control.py keeps its own separate copy for its own handlers).
`input_txt_variables` reads `state.output_config_version` /
`state.sim_config_version` -- namespace-agnostic reactive counters, so no
bridge is needed there either.

Self-contained: imports `input_analysis` (`validate_required_inputs`),
`compiler_env` (Intel-executable helpers), `utils`
(`validate_constants_file`); self-computes ROOT/OUTPUT_CSV. Imports nothing
from app.py or ui_panels.py.
"""
import logging
import os
import select
import subprocess
import threading
import time
import traceback
from datetime import date, datetime, timedelta

from shiny import module, reactive, render, ui

try:
    from shiny_app import setups
    from shiny_app.compiler_env import (
        build_intel_wrapped_command,
        check_intel_libs_available,
        get_intel_setvars_path,
        get_run_environment,
        is_intel_executable,
    )
    from shiny_app.input_analysis import validate_required_inputs
    from shiny_app.utils import validate_constants_file
except ImportError:  # running as a script from inside shiny_app/
    import setups
    from compiler_env import (
        build_intel_wrapped_command,
        check_intel_libs_available,
        get_intel_setvars_path,
        get_run_environment,
        is_intel_executable,
    )
    from input_analysis import validate_required_inputs
    from utils import validate_constants_file

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
OUTPUT_CSV = os.path.join(ROOT, "OUTPUT.csv")

# Local copy for the Quick Run (dashboard) fallback -- run_control.py owns its
# own copy for its own handlers.
DEFAULT_CONSTANTS_FILE = "WCONST_01.txt"


@module.ui
def dashboard_ui():
    return ui.card(
        ui.card_header("Dashboard"),
        # Status bar — horizontal summary strip
        ui.div(
            {"class": "dashboard-status-bar"},
            ui.div(
                {"class": "status-item"},
                ui.div({"class": "status-dot idle", "id": "run-status-dot"}),
                ui.div(
                    ui.div("Status", class_="status-label"),
                    ui.div(ui.output_text("dashboard_status_text", inline=True), class_="status-value"),
                ),
            ),
            ui.div(
                {"class": "status-item"},
                ui.tags.i(class_="bi bi-cpu", style="color: var(--text-muted);"),
                ui.div(
                    ui.div("Executable", class_="status-label"),
                    ui.div(ui.output_text("dashboard_exe_text", inline=True), class_="status-value"),
                ),
            ),
            ui.div(
                {"class": "status-item"},
                ui.tags.i(class_="bi bi-clock-history", style="color: var(--text-muted);"),
                ui.div(
                    ui.div("Last Run", class_="status-label"),
                    ui.div(ui.output_text("dashboard_last_run_text", inline=True), class_="status-value"),
                ),
            ),
            ui.div(
                {"class": "status-item", "style": "margin-left: auto;"},
                ui.output_ui("run_timer_display"),
            ),
        ),
        # Setup selection — surfaced on the landing page, synced to the Run Model tab
        ui.div(
            {"class": "dashboard-setup-row", "style": "max-width: 460px; margin-bottom: 0.75rem;"},
            ui.input_select("dash_setup_select", "Setup:",
                            choices={s.id: s.name for s in setups.list_setups()},
                            selected="standard"),
            ui.output_ui("dash_setup_availability"),
        ),
        # Two-column layout: actions + system | run log
        ui.layout_columns(
            # Left: Quick actions + system info
            ui.div(
                ui.layout_columns(
                    ui.tooltip(
                        ui.input_action_button("quick_run", "Quick Run", class_="btn-success btn-lg w-100"),
                        "Run the model with current settings using the selected executable"
                    ),
                    ui.tooltip(
                        ui.input_action_button("dashboard_stop", "Stop", class_="btn-danger btn-lg w-100"),
                        "Stop the currently running model simulation"
                    ),
                    col_widths=[6, 6],
                    class_="mb-3"
                ),
                ui.layout_columns(
                    ui.card(
                        ui.card_header("System Status"),
                        ui.div(
                            ui.output_ui("system_status_compact"),
                            style="max-height: 280px; overflow-y: auto; font-size: 0.78rem;"
                        ),
                        fill=False
                    ),
                    ui.card(
                        ui.card_header("Simulation Config"),
                        ui.div(
                            ui.output_ui("input_txt_variables"),
                            style="max-height: 280px; overflow-y: auto; font-size: 0.78rem;"
                        ),
                        fill=False
                    ),
                    col_widths=[6, 6]
                ),
                ui.tooltip(
                    ui.input_action_button("goto_model_config", "Model Config", class_="btn-primary btn-sm w-100 mt-2"),
                    "Navigate to Model Control panel to configure simulation settings"
                ),
            ),
            # Right: Run log
            ui.card(
                ui.card_header(
                    ui.div(
                        "Run Log",
                        ui.input_action_button("btn_copy_dashboard_log", "Copy", class_="btn-sm btn-outline-secondary float-end"),
                        class_="d-flex justify-content-between align-items-center w-100"
                    )
                ),
                ui.div(
                    ui.output_ui("dashboard_run_log"),
                    style="height: 420px; overflow-y: auto; padding: 10px; border-radius: 4px;",
                    class_="run-log-container",
                    id="dashboard_log_container"
                ),
                fill=False
            ),
            col_widths=[5, 7]
        )
    )


@module.server
def dashboard_server(input, output, session, state):
    run = state.run

    # ---------------------------------------------------------------
    # Setup selector sync: Dashboard <-> Run Model tab.
    # Single source of truth is run.current_setup (a reactive.calc over
    # run_control's input.setup_select). The Dashboard's dash_setup_select
    # is a mirror kept in sync through the run_control scope bridge.
    # Each effect updates the OTHER widget only when the value differs,
    # so a change converges in one hop and never loops.
    # ---------------------------------------------------------------
    rc = session.root_scope().make_scope("run_control")

    @reactive.effect
    def _dash_setup_to_run_control():
        """Dashboard selector drives run_control's authoritative setup_select."""
        chosen = input.dash_setup_select()
        if not chosen:
            return
        with reactive.isolate():
            current = rc.input.setup_select()
        if chosen != current:
            ui.update_select("setup_select", selected=chosen, session=rc)

    @reactive.effect
    def _run_control_to_dash_setup():
        """Mirror the authoritative current setup back into the Dashboard selector."""
        target = run.current_setup().id
        with reactive.isolate():
            shown = input.dash_setup_select()
        if target != shown:
            ui.update_select("dash_setup_select", selected=target)

    # =================
    # Log Copy Handlers
    # =================
    @reactive.effect
    @reactive.event(input.btn_copy_dashboard_log)
    async def copy_dashboard_log():
        """Copy dashboard run log to clipboard via client-side JS"""
        log_content = "".join(run.run_log_lines)
        if not log_content:
            log_content = "Log is empty."
        await session.send_custom_message("copy_to_clipboard", log_content)
        ui.notification_show("Run log copied to clipboard!", type="message", duration=2)

    @reactive.effect
    @reactive.event(input.goto_model_config)
    async def navigate_to_model_config():
        """Navigate to the Model Config panel from dashboard"""
        await state.navigate("nav_model_control")

    # Quick action handlers (defined early to access run.run_log_lines)
    @reactive.effect
    @reactive.event(input.quick_run)
    def handle_quick_run():
        """Quick run action from dashboard"""
        logger.info("User clicked Quick Run from dashboard")
        run.run_log_lines.clear()
        run.run_log_lines.append("Starting quick run...\n")

        cur = run.current_setup()
        if not setups.is_available(cur, ROOT):
            run.run_log_lines.append(f"⚠ Inputs for “{cur.name}” not found. {cur.unavailable_hint}\n")
            return

        # Validate required input files first
        run.run_log_lines.append("Validating input files...\n")
        is_valid, errors, warnings = validate_required_inputs()

        if warnings:
            for w in warnings:
                run.run_log_lines.append(f"⚠ {w}\n")

        if not is_valid:
            run.run_log_lines.append("❌ INPUT VALIDATION FAILED:\n")
            for e in errors:
                run.run_log_lines.append(f"  • {e}\n")
            run.run_log_lines.append("\nModel run aborted. Please ensure all required input files exist.\n")
            logger.error(f"Input validation failed: {errors}")
            return

        run.run_log_lines.append("✓ Input files validated\n")

        try:
            # Capture current widget values (must be done in reactive context)
            estas_cmd = run.command_config()

            # Check if executable exists
            exe_name = run.run_executable_name()

            exe_path = os.path.join(ROOT, exe_name)
            if not os.path.exists(exe_path):
                run.run_log_lines.append(f"❌ ERROR: Executable '{exe_name}' not found.\n")
                run.run_log_lines.append("Please go to Model Build to compile the model first.\n")
                return

            # Check Intel library requirements for Intel-compiled executables
            if is_intel_executable(exe_name):
                setvars_path = get_intel_setvars_path()
                if setvars_path:
                    run.run_log_lines.append("ℹ️  Intel executable detected. Will source Intel environment.\n")
                else:
                    intel_available, intel_path = check_intel_libs_available()
                    if intel_available:
                        run.run_log_lines.append("ℹ️  Intel executable detected. Using runtime libs from:\n")
                        run.run_log_lines.append(f"   {intel_path}\n")
                    else:
                        run.run_log_lines.append("⚠️  WARNING: Intel-compiled executable selected but Intel runtime\n")
                        run.run_log_lines.append("   libraries (libimf.so) and setvars.sh not found.\n")
                        run.run_log_lines.append("   The model may fail to start. Consider:\n")
                        run.run_log_lines.append("   • Installing Intel oneAPI or using a gfortran executable\n")
                        run.run_log_lines.append("-" * 50 + "\n")

            # Validate constants file before running
            const_file, binary_enabled, shear_file = run.constants_config()

            if not const_file and (binary_enabled or shear_file):
                const_file = DEFAULT_CONSTANTS_FILE

            if const_file:
                is_valid, actual_count, error_msg = validate_constants_file(const_file)
                if not is_valid:
                    run.run_log_lines.append(f"❌ VALIDATION ERROR:\n{error_msg}\n")
                    run.run_log_lines.append("Model run aborted. Please select a constants file with all required parameters.\n")
                    logger.error(f"Constants file validation failed: {error_msg}")
                    return
                else:
                    run.run_log_lines.append(f"✓ Constants file validated: {const_file} ({actual_count} constants)\n")

            # Show command before starting
            cmd_display = " ".join([c if c else '""' for c in estas_cmd])
            run.run_log_lines.append(f"\nCommand: {cmd_display}\n")
            run.run_log_lines.append("-" * 40 + "\n")

        except Exception as e:
            run.run_log_lines.append(f"\n❌ Error preparing quick run: {e}\n")
            run.run_log_lines.append(f"Traceback:\n{traceback.format_exc()}\n")
            logger.error(f"Error in quick_run setup: {e}\n{traceback.format_exc()}")
            return

        def _work():
            start_time = time.time()
            logger.info("Quick Run thread started")
            run.run_log_lines.append("Starting model execution...\n")

            # Filter out empty strings for actual execution
            exec_cmd = [c for c in estas_cmd if c]

            def format_time(seconds):
                """Format seconds into HH:MM:SS or MM:SS"""
                hours = int(seconds // 3600)
                minutes = int((seconds % 3600) // 60)
                secs = int(seconds % 60)
                if hours > 0:
                    return f"{hours:02d}:{minutes:02d}:{secs:02d}"
                else:
                    return f"{minutes:02d}:{secs:02d}"

            def get_csv_info():
                """Get info about OUTPUT.csv file for progress tracking"""
                try:
                    if os.path.exists(OUTPUT_CSV):
                        stat = os.stat(OUTPUT_CSV)
                        size_kb = stat.st_size / 1024
                        with open(OUTPUT_CSV, 'rb') as f:
                            lines = sum(1 for _ in f)
                        return {"exists": True, "size_kb": size_kb, "lines": lines}
                except Exception:
                    pass
                return {"exists": False, "size_kb": 0, "lines": 0}

            try:
                # For Intel executables, wrap command to source Intel environment
                use_shell = False
                final_cmd = exec_cmd
                run_env = os.environ.copy()  # Start with current environment

                if is_intel_executable(exe_name):
                    setvars_path = get_intel_setvars_path()
                    if setvars_path:
                        # Use shell command that sources Intel environment first
                        final_cmd, use_shell = build_intel_wrapped_command(exec_cmd)
                        run.run_log_lines.append(f"ℹ️  Sourcing Intel environment: {setvars_path}\n")
                        logger.info(f"Using Intel wrapper with setvars: {setvars_path}")
                    else:
                        # Fall back to LD_LIBRARY_PATH approach
                        run_env = get_run_environment()
                        ld_path = run_env.get("LD_LIBRARY_PATH", "NOT SET")
                        run.run_log_lines.append(f"LD_LIBRARY_PATH: {ld_path[:200]}...\n")
                        logger.info(f"Starting process with LD_LIBRARY_PATH: {ld_path[:100]}...")
                else:
                    # For non-Intel executables, use standard environment
                    run_env = get_run_environment()

                if cur.env:
                    run_env.update(cur.env)

                logger.info(f"Executing: {final_cmd if isinstance(final_cmd, str) else ' '.join(final_cmd)}")
                p = subprocess.Popen(
                    final_cmd,
                    cwd=ROOT,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True,
                    bufsize=1,
                    env=run_env,
                    shell=use_shell,
                    executable="/bin/bash" if use_shell else None
                )
                run.process = p
                run.running = True
                run.last_run_time = datetime.now()
                run.progress = ({"elapsed": "00:00", "rows": 0, "size_kb": 0, "status": "running"})

                last_progress_update = time.time()

                # Read output with progress updates
                while p.poll() is None:
                    if p.stdout:
                        try:
                            readable, _, _ = select.select([p.stdout], [], [], 0.5)
                            if readable:
                                line = p.stdout.readline()
                                if line:
                                    run.run_log_lines.append(line)
                                    while len(run.run_log_lines) > 1000:
                                        run.run_log_lines.pop(0)
                        except Exception:
                            time.sleep(0.5)

                    # Update progress every second
                    now = time.time()
                    if now - last_progress_update >= 1.0:
                        elapsed = now - start_time
                        output_info = get_csv_info()
                        run.progress = ({
                            "elapsed": format_time(elapsed),
                            "rows": output_info.get("lines", 0),
                            "size_kb": output_info.get("size_kb", 0),
                            "status": "running"
                        })
                        last_progress_update = now

                # Read any remaining output
                if p.stdout:
                    remaining = p.stdout.read()
                    if remaining:
                        run.run_log_lines.append(remaining)

                p.wait()
                rc = p.returncode

                elapsed = time.time() - start_time
                output_info = get_csv_info()
                run.run_log_lines.append("-" * 40 + "\n")
                if rc == 0:
                    run.run_log_lines.append("✓ Model run completed successfully!\n")
                    run.progress = ({
                        "elapsed": format_time(elapsed),
                        "rows": output_info.get("lines", 0),
                        "size_kb": output_info.get("size_kb", 0),
                        "status": "completed"
                    })
                else:
                    run.run_log_lines.append(f"✗ Model run failed with return code {rc}\n")
                    run.progress = ({
                        "elapsed": format_time(elapsed),
                        "rows": output_info.get("lines", 0),
                        "size_kb": output_info.get("size_kb", 0),
                        "status": "failed"
                    })
                run.run_log_lines.append(f"Total time: {format_time(elapsed)}\n")
                logger.info(f"Quick Run finished: rc={rc}, elapsed={elapsed:.1f}s")

            except Exception as e:
                run.run_log_lines.append(f"\n❌ Error running model: {e}\n")
                logger.error(f"Quick Run error: {e}")
                run.progress = ({"elapsed": "", "rows": 0, "size_kb": 0, "status": "error"})
            finally:
                run.process = None
                run.running = False

        threading.Thread(target=_work, daemon=True, name="QuickRunThread").start()

    @render.ui
    def dash_setup_availability():
        st = run.current_setup()
        if setups.is_available(st, ROOT):
            return ui.TagList()
        return ui.div(
            ui.tags.small(f"⚠ Inputs for “{st.name}” not found. {st.unavailable_hint}"),
            class_="text-warning",
        )

    @render.ui
    def dashboard_run_log():
        """Run log for Dashboard panel with scrollable output"""
        reactive.invalidate_later(0.5)
        # Show last 300 lines in the dashboard log
        log_content = ''.join(run.run_log_lines[-300:])

        # Format the log with proper HTML styling
        return ui.tags.pre(
            log_content,
            style="margin: 0; padding: 0; font-family: 'Consolas', 'Monaco', 'Courier New', monospace; "
                  "font-size: 12px; line-height: 1.4; color: #d4d4d4; white-space: pre-wrap; "
                  "word-wrap: break-word; background: transparent;"
        )

    @render.ui
    def run_timer_display():
        """Large prominent timer display for dashboard"""
        reactive.invalidate_later(0.5)
        progress = run.progress
        status = progress.get("status", "idle")
        elapsed = progress.get("elapsed", "00:00")
        rows = progress.get("rows", 0)
        size_kb = progress.get("size_kb", 0)

        base_style = "display: flex; align-items: center; justify-content: center; height: 100%; min-height: 48px; padding: 8px 16px; border-radius: 6px; width: 100%;"

        if status == "running":
            return ui.div(
                ui.tags.span("⏱ ", style="font-size: 1.5em; color: #ffc107;"),
                ui.tags.span(elapsed, style="font-size: 1.8em; font-weight: bold; color: #ffc107; font-family: monospace;"),
                ui.tags.span(f"  {rows:,} rows", style="font-size: 0.9em; color: #17a2b8; margin-left: 12px;"),
                ui.tags.span(f" ({size_kb:.1f} KB)", style="font-size: 0.85em; color: #6c757d;"),
                style=base_style + "background: linear-gradient(135deg, #1a3d1a 0%, #2d5a2d 100%); border: 2px solid #4caf50;"
            )
        elif status == "completed":
            return ui.div(
                ui.tags.span("✓ ", style="font-size: 1.5em; color: #4caf50;"),
                ui.tags.span(elapsed, style="font-size: 1.8em; font-weight: bold; color: #4caf50; font-family: monospace;"),
                ui.tags.span(f"  {rows:,} rows", style="font-size: 0.9em; color: #17a2b8; margin-left: 12px;"),
                ui.tags.span(" Done", style="font-size: 0.9em; color: #4caf50; margin-left: 8px;"),
                style=base_style + "background: linear-gradient(135deg, #1a3d1a 0%, #2d5a2d 100%); border: 2px solid #4caf50;"
            )
        elif status == "failed":
            return ui.div(
                ui.tags.span("✗ ", style="font-size: 1.5em; color: #f44336;"),
                ui.tags.span(elapsed, style="font-size: 1.8em; font-weight: bold; color: #f44336; font-family: monospace;"),
                ui.tags.span(" Failed", style="font-size: 0.9em; color: #f44336; margin-left: 12px;"),
                style=base_style + "background: linear-gradient(135deg, #3d1a1a 0%, #5a2d2d 100%); border: 2px solid #f44336;"
            )
        else:
            return ui.div(
                ui.tags.span("○ ", style="font-size: 1.5em; color: #6c757d;"),
                ui.tags.span("Ready", style="font-size: 1.2em; color: #6c757d; font-family: monospace;"),
                style=base_style + "background: #2d2d2d; border: 2px solid #444;"
            )

    @render.ui
    def system_status_compact():
        """Compact system status for dashboard"""
        items = []

        # Working directory
        items.append(ui.div(
            ui.tags.strong("Directory: "),
            ui.tags.span(os.path.basename(ROOT), class_="text-info"),
            class_="mb-1"
        ))

        # Last run info
        if os.path.exists(OUTPUT_CSV):
            mtime = datetime.fromtimestamp(os.path.getmtime(OUTPUT_CSV))
            items.append(ui.div(
                ui.tags.strong("Last Run: "),
                ui.tags.span(mtime.strftime('%m-%d %H:%M'), class_="text-muted"),
                class_="mb-1"
            ))
            try:
                # Efficient line count using file size estimate
                file_size = os.path.getsize(OUTPUT_CSV)
                # Estimate lines (avg ~100 bytes per line in CSV)
                estimated_lines = file_size // 100
                items.append(ui.div(
                    ui.tags.strong("Output: "),
                    ui.tags.span(f"~{estimated_lines:,} rows ({file_size // 1024:,} KB)", class_="text-success"),
                    class_="mb-1"
                ))
            except Exception:
                pass
        else:
            items.append(ui.div(
                ui.tags.strong("Last Run: "),
                ui.tags.span("Never", class_="text-muted"),
                class_="mb-1"
            ))

        # Executable
        try:
            exe_name = run.run_executable_name()
        except Exception:
            exe_name = "ESTAS_II"
        exe_exists = os.path.exists(os.path.join(ROOT, exe_name))
        items.append(ui.div(
            ui.tags.strong("Exe: "),
            ui.tags.span(exe_name, class_="text-success" if exe_exists else "text-danger"),
            class_="mb-1"
        ))

        # Command preview
        try:
            cmd = run.command_config()
            cmd_str = " ".join(cmd)
        except Exception:
            cmd_str = "(error)"
        items.append(ui.div(
            ui.tags.strong("Cmd: "),
            ui.tags.code(cmd_str, style="font-size: 10px; word-break: break-all;"),
            class_="mb-1"
        ))

        return ui.div(*items)

    @render.text
    def dashboard_status_text():
        return "Running" if run.running else "Ready"

    @render.text
    def dashboard_exe_text():
        try:
            return run.active_executable()
        except Exception:
            return "ESTAS_II"

    @render.text
    def dashboard_last_run_text():
        if run.last_run_time:
            return run.last_run_time.strftime("%Y-%m-%d %H:%M")
        return "Never"

    @render.ui
    def input_txt_variables():
        """Display INPUT.txt variables with labels"""
        reactive.invalidate_later(5.0)  # Refresh every 5 seconds
        # Also refresh when output config is saved
        _ = state.output_config_version.get()
        # Also refresh when simulation config is saved
        _ = state.sim_config_version.get()

        def make_row(label, value, unit=""):
            return ui.div(
                ui.tags.span(label + ": ", class_="text-muted", style="font-size: 11px;"),
                ui.tags.span(str(value), class_="fw-bold text-info"),
                ui.tags.span(f" {unit}" if unit else "", class_="text-muted", style="font-size: 10px;"),
                class_="mb-1", style="line-height: 1.3;"
            )

        items = []
        try:
            input_path = os.path.join(ROOT, run.current_setup().input_file)
            if os.path.exists(input_path):
                with open(input_path) as f:
                    lines = f.readlines()

                # First pass: get base_year for date conversion
                base_year = 1998  # default
                for i, line in enumerate(lines):
                    if line.strip().startswith("# BASE_YEAR") and i + 1 < len(lines):
                        try:
                            base_year = int(lines[i+1].strip())
                        except Exception:
                            pass
                        break

                def julian_to_date(julian_day, base_year):
                    """Convert Julian day to actual date string"""
                    try:
                        base_date = date(base_year, 1, 1)
                        actual_date = base_date + timedelta(days=int(float(julian_day)) - 1)
                        return actual_date.strftime("%d-%b-%Y")
                    except Exception:
                        return str(julian_day)

                # Parse key variables (skip base year display)
                i = 0
                while i < len(lines):
                    line = lines[i].strip()
                    if line.startswith("# SIMULATION_START") and i + 1 < len(lines):
                        julian = lines[i+1].strip()
                        items.append(make_row("Start Date", julian_to_date(julian, base_year)))
                    elif line.startswith("# SIMULATION_END") and i + 1 < len(lines):
                        julian = lines[i+1].strip()
                        items.append(make_row("End Date", julian_to_date(julian, base_year)))
                    elif line.startswith("# NUM_REPEATS") and i + 1 < len(lines):
                        items.append(make_row("Repeats", lines[i+1].strip()))
                    elif line.startswith("# TIME_STEPS_PER_DAY") and i + 1 < len(lines):
                        items.append(make_row("Steps/Day", lines[i+1].strip()))
                    elif line.startswith("# PRINT_INTERVAL") and i + 1 < len(lines):
                        items.append(make_row("Print Interval", lines[i+1].strip(), "steps"))
                    elif line.startswith("# PELAGIC MODEL INPUT FOLDER") and i + 1 < len(lines):
                        items.append(make_row("Input Folder", lines[i+1].strip()))
                    elif line.startswith("# PELAGIC MODEL OUTPUT FOLDER") and i + 1 < len(lines):
                        items.append(make_row("Output Folder", lines[i+1].strip()))
                    elif line.startswith("# RESUSPENSION_OPTION") and i + 1 < len(lines):
                        val = lines[i+1].strip()
                        label = {"0": "Off", "1": "Prescribed", "2": "Semi-prescribed"}.get(val, val)
                        items.append(make_row("Resuspension", label))
                    elif line.startswith("# MODEL_SEDIMENTS") and i + 1 < len(lines):
                        val = lines[i+1].strip()
                        label = "Yes" if val != "0" else "No"
                        items.append(make_row("Sediments", label))
                    i += 1

                # Calculate simulation days
                try:
                    start_idx = next(i for i, l in enumerate(lines) if "SIMULATION_START" in l)
                    end_idx = next(i for i, l in enumerate(lines) if "SIMULATION_END" in l)
                    start = float(lines[start_idx + 1].strip())
                    end = float(lines[end_idx + 1].strip())
                    days = int(end - start)
                    items.append(ui.tags.hr(style="margin: 5px 0;"))
                    items.append(make_row("Duration", days, "days"))
                except Exception:
                    pass

                # Read output box settings from PELAGIC_OUTPUT_INFORMATION_FILE.txt
                try:
                    output_info_path = os.path.join(ROOT, run.current_setup().inputs_dir, "PELAGIC_OUTPUT_INFORMATION_FILE.txt")
                    if os.path.exists(output_info_path):
                        with open(output_info_path) as f:
                            output_lines = f.readlines()
                        output_boxes = []
                        for line in output_lines[1:]:  # Skip header
                            parts = line.split()
                            if len(parts) >= 4:
                                box_num = parts[0]
                                # Include box if ANY output type is enabled
                                state_var = parts[1] == "1"
                                process_rate = parts[2] == "1"
                                mass_balance = parts[3] == "1"
                                if state_var or process_rate or mass_balance:
                                    output_boxes.append(box_num)
                        if output_boxes:
                            items.append(ui.tags.hr(style="margin: 5px 0;"))
                            items.append(make_row("Output Boxes", ", ".join(output_boxes)))
                except Exception:
                    pass

        except Exception as e:
            items.append(ui.div(f"Error reading INPUT.txt: {e}", class_="text-danger"))

        return ui.div(*items)

    @reactive.effect
    @reactive.event(input.dashboard_stop)
    def on_dashboard_stop():
        logger.info("User clicked Dashboard Stop button")
        run.stop(reset_progress=True)
