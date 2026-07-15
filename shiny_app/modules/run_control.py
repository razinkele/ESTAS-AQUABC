"""Run Model + Output Config sub-tabs as a fat-tab Shiny module (Phase 4, Task 4).

`run_control_ui(id)` returns a plain **list** of the two nav_panels (Run Model,
Output Config) — the former sub-tabs 2 and 3 of `panel_model_control`'s
`navset_card_tab` (content moved verbatim from `ui_panels.py:220-393`; the
`panel_conditional` + `navset_card_tab` wrapper stays in `create_ui`, together
with `sim_config_ui`). It must be `*`-unpacked at the call site —
`ui.navset_card_tab(sim_config_ui("sim_config"), *run_control_ui("run_control"),
id="model_control_tabs")`. Passing the list as a single positional arg instead
raises `AttributeError: 'TagList' object has no attribute 'resolve'` at
`.tagify()` (empirically verified against Shiny 1.5.1).

`run_control_server(id, state)` registers the handlers ported verbatim from
app.py: `init_cmd_dropdowns`, `build_estas_command`, `cmd_preview`,
`run_executable_info` (thin `get_executable_info` wrapper, imported from
`build_commands`), `constants_validation_status`, `navigate_to_build` (the
`goto_build` button handler), `on_run` (launches `run.start_run` via thread),
`on_stop_run`, `copy_mini_log`, `run_log_mini`, `run_status_indicator`, plus
the Output Config cluster (`output_config_msg`, `OUTPUT_INFO_FILE`,
`load_output_config`, `save_output_config` [bumps
`state.output_config_version`], `output_config_status`,
`refresh_sim_output_dirs`, `sim_output_dir_info`). It also carries the three
run_control-owned contract registrations that were placed in `server()` in
Task 2 (they read this module's namespaced inputs, so they must live here
now): the `run.command_config` `reactive.Calc` (wrapping
`build_estas_command`), the `_publish_run_executable_name` effect (publishes
`input.run_executable()` as a bare string for the dashboard/quick-run bridge),
and the `run.constants_config` `reactive.Calc`.

Bucket-mismatch note: despite thematic names, `copy_mini_log`, `run_log_mini`,
`run_status_indicator`, and `navigate_to_build` (the `goto_build` button)
belong here, not dashboard — their widget ids physically live in the Run
Model sub-tab (`ui_panels.py:117-225`, now moved into this module's UI).

Cross-namespace note: this module now OWNS `sim_output_dir` and
`run_executable` (namespaced `run_control-sim_output_dir` /
`run_control-run_executable`). `refresh_sim_output_dirs` and
`sim_output_dir_info` read/write `sim_output_dir` directly (bare, local —
no bridge needed here). The bridge runs the OTHER direction: `model_build.py`,
`plot.py`, and `sim_config.py` reach INTO this module's `sim_output_dir` /
`run_executable` via `session.root_scope().make_scope("run_control")` plus a
bare id (NOT the hyphenated literal `"run_control-sim_output_dir"` form,
which raises `ValueError` in `validate_id` on read).

Self-contained: imports `build_commands`, `compiler_env`
(`is_intel_executable`/`check_intel_libs_available`), `utils`
(`validate_constants_file`/`REQUIRED_MODEL_CONSTANTS`), `output_data`
(`get_output_directories`); self-computes `ROOT`/`INPUTS_DIR`. Imports
nothing from app.py or ui_panels.py.
"""
import logging
import os
import threading
import traceback

from shiny import module, reactive, render, ui

try:
    from shiny_app import build_commands, output_data
    from shiny_app.compiler_env import check_intel_libs_available, is_intel_executable
    from shiny_app.utils import REQUIRED_MODEL_CONSTANTS, validate_constants_file
except ImportError:  # running as a script from inside shiny_app/
    import build_commands
    import output_data
    from compiler_env import check_intel_libs_available, is_intel_executable
    from utils import REQUIRED_MODEL_CONSTANTS, validate_constants_file

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")

# Default constants file to use when Arg 3 is set but Arg 2 is not.
DEFAULT_CONSTANTS_FILE = "WCONST_01.txt"


@module.ui
def run_control_ui():
    run_model_nav_panel = ui.nav_panel(
        "Run Model",
        ui.layout_columns(
            # Left column: Run Parameters
            ui.card(
                {"class": "run-params-compact"},
                ui.card_header("Run Parameters"),

                # Build options button at top
                ui.tooltip(
                    ui.input_action_button("goto_build", "Build Options", class_="btn-outline-primary btn-sm w-100 mb-2"),
                    "Go to Model Build panel to compile with different compilers or settings"
                ),
                ui.tags.hr(class_="my-2"),

                # Executable selection
                ui.tags.strong("Executable", class_="small"),
                ui.tooltip(
                    ui.input_select(
                        "run_executable",
                        None,
                        choices=["ESTAS_II"],
                        selected="ESTAS_II"
                    ),
                    "Select which compiled executable to run"
                ),
                ui.output_ui("run_executable_info"),

                # Input file selection (Arg 1 - required)
                ui.tags.strong("Command Line Arguments", class_="small mt-2"),
                ui.tooltip(
                    ui.input_select(
                        "cmd_input_file",
                        "Input Configuration File:",
                        choices={"INPUT.txt": "INPUT.txt (default)"},
                        selected="INPUT.txt"
                    ),
                    "Main model configuration file (INPUT.txt). Contains simulation time, folders, and model options."
                ),

                # Constants file (Arg 2 - optional)
                ui.tooltip(
                    ui.input_select(
                        "cmd_constants_file",
                        "Pelagic Constants File:",
                        choices={"WCONST_04.txt": "WCONST_04.txt"},
                        selected="WCONST_04.txt"
                    ),
                    "Override model constants. WCONST_04.txt is recommended."
                ),

                # Binary output (Arg 3)
                ui.tooltip(
                    ui.input_switch(
                        "cmd_binary_enabled",
                        "Enable Binary Output",
                        value=False
                    ),
                    "Write binary output file for faster I/O."
                ),
                ui.panel_conditional(
                    "input.cmd_binary_enabled",
                    ui.input_text(
                        "cmd_binary_filename",
                        "Binary Filename:",
                        value="PELAGIC_OUTPUT.bin"
                    ),
                ),

                # Command preview
                ui.tags.strong("Command Preview", class_="small mt-2"),
                ui.output_text_verbatim("cmd_preview", placeholder=True),
                ui.output_ui("constants_validation_status"),

                ui.tags.hr(class_="my-2"),

                # Run controls
                ui.layout_columns(
                    ui.tooltip(
                        ui.input_action_button("run", "Run Model", class_="btn-success btn-lg w-100"),
                        "Start the model simulation with current configuration"
                    ),
                    ui.tooltip(
                        ui.input_action_button("stop_run", "Stop", class_="btn-danger btn-lg w-100"),
                        "Terminate the running model process"
                    ),
                    col_widths=[8, 4]
                ),
                ui.output_ui("run_status_indicator"),

                fill=False
            ),

            # Right column: Run Log
            ui.card(
                ui.card_header(
                    ui.div(
                        "Run Log",
                        ui.input_action_button("btn_copy_mini_log", "Copy", class_="btn-sm btn-outline-secondary float-end"),
                        class_="d-flex justify-content-between align-items-center w-100"
                    )
                ),
                ui.output_text_verbatim("run_log_mini", placeholder=True),

                fill=False
            ),
            col_widths=[5, 7]
        )
    )

    output_config_nav_panel = ui.nav_panel(
        "Output Config",
        ui.layout_columns(
            ui.card(
                ui.card_header("Output Boxes"),
                ui.p("Select which boxes should produce output:", class_="text-muted"),
                ui.div(
                    ui.input_checkbox_group(
                        "output_boxes",
                        None,
                        choices={str(i): f"Box {i}" for i in range(1, 26)},
                        selected=["5", "6", "8", "9", "14", "17", "25"]
                    ),
                    style="column-count: 2; column-gap: 1rem;"
                ),
                fill=False
            ),
            ui.card(
                ui.card_header("Output Directory"),
                ui.tooltip(
                    ui.input_select(
                        "sim_output_dir",
                        "Output Directory:",
                        choices={"OUTPUTS": "OUTPUTS (default)"},
                        selected="OUTPUTS"
                    ),
                    "Folder where model output files will be saved (.out, .bin, .csv)"
                ),
                ui.tooltip(
                    ui.input_action_button("refresh_sim_output_dirs", "Refresh", class_="btn-secondary btn-sm w-100 mt-2"),
                    "Scan for available output directories"
                ),
                ui.output_text("sim_output_dir_info"),
                ui.tags.hr(),
                ui.card_header("Output Types"),
                ui.p("Select output types for selected boxes:", class_="text-muted small"),
                ui.tooltip(
                    ui.input_checkbox_group(
                        "output_types",
                        None,
                        choices={
                            "state_vars": "State Variables",
                            "process_rates": "Process Rates",
                            "mass_balance": "Mass Balance"
                        },
                        selected=["state_vars"]
                    ),
                    "State Variables: concentrations. Process Rates: fluxes. Mass Balance: conservation checks."
                ),
                ui.tags.hr(),
                ui.tooltip(
                    ui.input_action_button("load_output_config", "Load Current", class_="btn-secondary me-2"),
                    "Load output box selection from current configuration"
                ),
                ui.tooltip(
                    ui.input_action_button("save_output_config", "Save Configuration", class_="btn-success"),
                    "Save output box selection to configuration file"
                ),
                ui.output_text("output_config_status"),
                fill=False
            ),
            col_widths=[8, 4]
        )
    )

    return [run_model_nav_panel, output_config_nav_panel]


@module.server
def run_control_server(input, output, session, state):
    run = state.run

    # =========================================================================
    # ESTAS_II Command Line Parameter Controls
    # =========================================================================

    @reactive.effect
    def init_cmd_dropdowns():
        """Initialize command line parameter dropdown choices"""
        # Get available INPUT*.txt files
        input_files = {"INPUT.txt": "INPUT.txt (default)"}
        for f in sorted(os.listdir(ROOT)):
            if f.startswith("INPUT") and f.endswith(".txt") and f != "INPUT.txt":
                input_files[f] = f
        ui.update_select("cmd_input_file", choices=input_files)

        # Get available WCONST*.txt files for constants override (Arg 2)
        # Note: Fortran code prepends PELAGIC_INPUT_FOLDER, so just use filename
        const_files = {"": "(not used - use defaults)"}
        for f in sorted(os.listdir(INPUTS_DIR)):
            if f.startswith("WCONST") and f.endswith(".txt"):
                const_files[f] = f  # Just filename, not path
        ui.update_select("cmd_constants_file", choices=const_files)

        # Get available shear stress files (Arg 4)
        # Note: Fortran code uses this path directly
        shear_files = {"": "(not used)"}
        for f in sorted(os.listdir(INPUTS_DIR)):
            if "SHEAR" in f.upper() and f.endswith(".txt"):
                shear_files[os.path.join("INPUTS", f)] = f
        ui.update_select("cmd_shear_stress_file", choices=shear_files)

    def build_estas_command():
        """Build the model command from current widget values (thin wrapper).

        Reads are raw (input-not-ready -> falsy sentinel); all defaulting lives in
        build_commands.assemble_estas_command. Command format: 0 args uses INPUT.txt;
        1: INPUT_FILE; 2: +CONSTANTS; 3: +BINARY; 4: +SHEAR.
        """
        try:
            exe_name = input.run_executable()
        except Exception:
            exe_name = None
        try:
            input_file = input.cmd_input_file()
        except Exception:
            input_file = None
        try:
            const_file = input.cmd_constants_file()
        except Exception:
            const_file = None
        try:
            binary_enabled = input.cmd_binary_enabled()
        except Exception:
            binary_enabled = False
        # Read the binary filename ONLY when the switch is on (preserve original reactive deps)
        binary_filename = None
        if binary_enabled:
            try:
                binary_filename = input.cmd_binary_filename()
            except Exception:
                binary_filename = None
        try:
            shear_file = input.cmd_shear_stress_file()
        except Exception:
            shear_file = None
        return build_commands.assemble_estas_command(
            exe_name, input_file, const_file, binary_enabled,
            binary_filename, shear_file, DEFAULT_CONSTANTS_FILE)

    @reactive.calc
    def _command_config():
        return build_estas_command()
    run.command_config = _command_config

    @render.text
    def cmd_preview():
        """Show preview of the command that will be executed"""
        cmd = build_estas_command()
        return " ".join(cmd)

    # run_control -> dashboard: the Run Model tab's run_executable name (bare string)
    run.run_executable_name = reactive.Value("ESTAS_II")

    @reactive.effect
    def _publish_run_executable_name():
        run.run_executable_name.set(input.run_executable() or "ESTAS_II")

    # run_control -> dashboard: the quick-run constants-validation inputs
    @reactive.calc
    def _constants_config():
        return (input.cmd_constants_file(), input.cmd_binary_enabled(), input.cmd_shear_stress_file())
    run.constants_config = _constants_config

    def get_executable_info(exe_name):
        """Get information about an executable (thin wrapper)."""
        return build_commands.get_executable_info(exe_name, ROOT)

    @render.ui
    def run_executable_info():
        """Display info about the selected run executable"""
        exe_name = input.run_executable()
        info = get_executable_info(exe_name)

        if not info["exists"]:
            return ui.div(
                ui.tags.small(f"✗ {exe_name} not found. Go to Model Build to compile.", class_="text-danger")
            )

        # Determine build type from executable name
        if "_debug" in exe_name or "_gf_debug" in exe_name:
            build_info = "debug build"
        elif "_fast" in exe_name or "_gf_fast" in exe_name:
            build_info = "fast build (optimized)"
        elif "_release" in exe_name or "_gf_release" in exe_name:
            build_info = "release build"
        else:
            build_info = "release build"

        # Check if Intel executable needs runtime libraries
        if is_intel_executable(exe_name):
            intel_available, intel_path = check_intel_libs_available()
            if intel_available:
                return ui.div(
                    ui.tags.small(f"✓ {build_info} (Intel), {info['size'] / 1024:.1f} KB", class_="text-success"),
                    ui.tags.br(),
                    ui.tags.small(f"Intel libs: {intel_path[:50]}...", class_="text-muted", style="font-size: 9px;")
                )
            else:
                return ui.div(
                    ui.tags.small(f"⚠ {build_info} (Intel), {info['size'] / 1024:.1f} KB", class_="text-warning"),
                    ui.tags.br(),
                    ui.tags.small("⚠ Intel runtime libraries not found!", class_="text-warning"),
                    ui.tags.br(),
                    ui.tags.small("Run 'source /opt/intel/oneapi/setvars.sh' first, or use gfortran builds.", class_="text-muted", style="font-size: 9px;")
                )

        return ui.div(
            ui.tags.small(f"✓ {build_info}, {info['size'] / 1024:.1f} KB", class_="text-success")
        )

    @reactive.effect
    @reactive.event(input.goto_build)
    async def navigate_to_build():
        """Navigate to the Model Build panel"""
        await state.navigate("nav_model_build")

    @render.ui
    def constants_validation_status():
        """Display real-time validation status of the selected constants file"""
        const_file = input.cmd_constants_file() or ""

        # Check if binary/shear file is set but not constants - use default
        try:
            binary_enabled = input.cmd_binary_enabled()
            shear_file = input.cmd_shear_stress_file()
            if (binary_enabled or shear_file) and not const_file:
                const_file = DEFAULT_CONSTANTS_FILE
        except Exception:
            pass

        if not const_file:
            return ui.div(
                ui.tags.small("ℹ️ No constants file selected (using model defaults)",
                             class_="text-muted")
            )

        is_valid, actual_count, error_msg = validate_constants_file(const_file)

        if is_valid:
            return ui.div(
                ui.tags.small(
                    f"✓ {const_file}: {actual_count}/{REQUIRED_MODEL_CONSTANTS} constants",
                    class_="text-success"
                )
            )
        else:
            return ui.div(
                ui.tags.small(f"❌ {error_msg}", class_="text-danger"),
                ui.tags.br(),
                ui.tags.small("💡 Recommended: Use WCONST_04.txt", class_="text-warning")
            )

    @reactive.effect
    @reactive.event(input.btn_copy_mini_log)
    async def copy_mini_log():
        """Copy mini run log to clipboard via client-side JS"""
        log_content = "".join(run.run_log_lines)
        if not log_content:
            log_content = "Log is empty."
        await session.send_custom_message("copy_to_clipboard", log_content)
        ui.notification_show("Run log copied to clipboard!", type="message", duration=2)

    # Mini run log for Model Control panel
    @render.text
    def run_log_mini():
        """Abbreviated run log for sidebar"""
        # Poll every 500ms to catch updates from background threads
        reactive.invalidate_later(0.5)
        # Return last 50 lines from shared list
        return ''.join(run.run_log_lines[-50:])

    @reactive.effect
    @reactive.event(input.run)
    def on_run():
        logger.info("User clicked 'Run' button")
        run.run_log_lines.clear()
        run.run_log_lines.append("=" * 50 + "\n")
        run.run_log_lines.append("Starting model run...\n")
        run.run_log_lines.append("=" * 50 + "\n")

        try:
            # Capture current widget values (must be done in reactive context)
            estas_cmd = build_estas_command()

            # Check if executable exists
            try:
                exe_name = input.run_executable()
            except Exception:
                exe_name = "ESTAS_II"

            exe_path = os.path.join(ROOT, exe_name)
            if not os.path.exists(exe_path):
                run.run_log_lines.append(f"❌ ERROR: Executable '{exe_name}' not found.\n")
                run.run_log_lines.append("Please go to Model Build to compile the model first.\n")
                return

            # Check if it's a release build (stripped = no debug output)
            exe_info = get_executable_info(exe_name)
            is_release = exe_info.get("stripped", False) or not exe_info.get("has_debug", True)

            run.run_log_lines.append(f"Executable: {exe_name}\n")
            if is_release:
                run.run_log_lines.append("Build type: Release (optimized, minimal console output)\n")
            else:
                run.run_log_lines.append("Build type: Debug (with diagnostic output)\n")

            # Validate constants file before running
            const_file = input.cmd_constants_file() or ""
            if not const_file:
                try:
                    if input.cmd_binary_enabled():
                        const_file = DEFAULT_CONSTANTS_FILE
                except Exception:
                    pass

            if const_file:
                is_valid, actual_count, error_msg = validate_constants_file(const_file)
                if not is_valid:
                    run.run_log_lines.append(f"❌ VALIDATION ERROR:\n{error_msg}\n")
                    run.run_log_lines.append("Model run aborted. Please select a constants file with all required parameters.\n")
                    logger.error(f"Constants file validation failed: {error_msg}")
                    return
                else:
                    run.run_log_lines.append(f"✓ Constants file validated: {const_file} ({actual_count} constants)\n")

            # Show command
            cmd_display = " ".join([c if c else '""' for c in estas_cmd])
            run.run_log_lines.append(f"\nCommand: {cmd_display}\n")
            run.run_log_lines.append("-" * 50 + "\n")

            if is_release:
                run.run_log_lines.append("ℹ️  Release builds produce minimal output.\n")
                run.run_log_lines.append("    Progress is tracked via OUTPUT.csv file.\n")
                run.run_log_lines.append("-" * 50 + "\n")

        except Exception as e:
            run.run_log_lines.append(f"\n❌ Error preparing model run: {e}\n")
            run.run_log_lines.append(f"Traceback:\n{traceback.format_exc()}\n")
            logger.error(f"Error in on_run setup: {e}\n{traceback.format_exc()}")
            return

        threading.Thread(
            target=run.start_run, args=(estas_cmd, exe_name),
            daemon=True, name="RunThread",
        ).start()

    @render.ui
    def run_status_indicator():
        """Show running status indicator"""
        reactive.invalidate_later(1.0)
        is_running = run.running

        if is_running:
            return ui.div(
                ui.tags.span("● ", class_="text-success", style="font-size: 1.2em;"),
                ui.tags.span("Model is running...", class_="text-success fw-bold"),
                class_="mt-2 mb-2"
            )
        else:
            return ui.div(
                ui.tags.span("○ ", class_="text-muted", style="font-size: 1.2em;"),
                ui.tags.span("Ready", class_="text-muted"),
                class_="mt-2 mb-2"
            )

    @reactive.effect
    @reactive.event(input.stop_run)
    def on_stop_run():
        logger.info("User clicked Stop button")
        run.stop()

    # ========== OUTPUT CONFIGURATION ==========
    output_config_msg = reactive.Value("")
    OUTPUT_INFO_FILE = os.path.join(ROOT, "INPUTS", "PELAGIC_OUTPUT_INFORMATION_FILE.txt")

    @reactive.effect
    @reactive.event(input.load_output_config)
    def load_output_config():
        """Load current output configuration from file"""
        try:
            if not os.path.exists(OUTPUT_INFO_FILE):
                output_config_msg.set("Output config file not found")
                return

            with open(OUTPUT_INFO_FILE) as f:
                lines = f.readlines()

            selected_boxes = []
            has_state_vars = False
            has_process_rates = False
            has_mass_balance = False

            for line in lines[1:]:  # Skip header
                parts = line.split()
                if len(parts) >= 4:
                    box_num = parts[0]
                    state_var = parts[1] == "1"
                    process_rate = parts[2] == "1"
                    mass_bal = parts[3] == "1"

                    if state_var or process_rate or mass_bal:
                        selected_boxes.append(box_num)
                        if state_var:
                            has_state_vars = True
                        if process_rate:
                            has_process_rates = True
                        if mass_bal:
                            has_mass_balance = True

            # Update UI
            ui.update_checkbox_group("output_boxes", selected=selected_boxes)
            output_types = []
            if has_state_vars:
                output_types.append("state_vars")
            if has_process_rates:
                output_types.append("process_rates")
            if has_mass_balance:
                output_types.append("mass_balance")
            ui.update_checkbox_group("output_types", selected=output_types)

            output_config_msg.set(f"Loaded: {len(selected_boxes)} boxes")
            logger.info(f"Loaded output config: {len(selected_boxes)} boxes selected")

        except Exception as e:
            logger.error(f"Error loading output config: {e}")
            output_config_msg.set(f"Error: {e}")

    @reactive.effect
    @reactive.event(input.save_output_config)
    def save_output_config():
        """Save output configuration to file"""
        try:
            selected_boxes = set(input.output_boxes() or [])
            output_types = set(input.output_types() or [])

            state_vars_enabled = "state_vars" in output_types
            process_rates_enabled = "process_rates" in output_types
            mass_balance_enabled = "mass_balance" in output_types

            # Build new file content
            lines = ["#     PELAGIC BOX NO      PRODUCE_PEL_STATE_VAR_OUTPUTS     PRODUCE_PEL_PROCESS_RATE_OUTPUTS     PRODUCE_PEL_MASS_BALANCE_OUTPUTS\n"]

            for box in range(1, 26):
                box_str = str(box)
                if box_str in selected_boxes:
                    sv = "1" if state_vars_enabled else "0"
                    pr = "1" if process_rates_enabled else "0"
                    mb = "1" if mass_balance_enabled else "0"
                else:
                    sv = pr = mb = "0"

                lines.append(f"{box:20d}{sv:>37s}{pr:>37s}{mb:>37s}\n")

            # Write file
            with open(OUTPUT_INFO_FILE, 'w') as f:
                f.writelines(lines)

            # Increment version to trigger dashboard refresh
            state.output_config_version.set(state.output_config_version.get() + 1)

            output_config_msg.set(f"Saved: {len(selected_boxes)} boxes")
            ui.notification_show(f"Output config saved ({len(selected_boxes)} boxes)", type="message")
            logger.info(f"Saved output config: {len(selected_boxes)} boxes")

        except Exception as e:
            logger.error(f"Error saving output config: {e}")
            output_config_msg.set(f"Error: {e}")
            ui.notification_show(f"Error: {e}", type="error")

    @render.text
    def output_config_status():
        """Display output config status"""
        return output_config_msg.get()

    # ========== END OUTPUT CONFIGURATION ==========

    @reactive.effect
    @reactive.event(input.refresh_sim_output_dirs)
    def refresh_sim_output_dirs():
        """Refresh the output directory list in Model Config"""
        dirs = output_data.get_output_directories()
        current = input.sim_output_dir()
        selected = current if current in dirs else (list(dirs.keys())[0] if dirs else None)
        ui.update_select("sim_output_dir", choices=dirs, selected=selected)

    @render.text
    def sim_output_dir_info():
        """Show info about selected output directory in Model Config"""
        dir_name = input.sim_output_dir()
        if not dir_name:
            return ""
        if dir_name == "ROOT":
            dir_path = ROOT
        else:
            dir_path = os.path.join(ROOT, dir_name)
        if os.path.exists(dir_path):
            files = [f for f in os.listdir(dir_path) if f.endswith(('.bin', '.out', '.csv'))]
            return f"📁 {len(files)} output files"
        return "Directory not found"
