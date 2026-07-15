"""Content-panel fragments for the AQUABC UI (extracted from create_ui())."""
from shiny import ui

try:
    from shiny_app.modules.sim_config import sim_config_ui
except ImportError:
    from modules.sim_config import sim_config_ui


def panel_dashboard():
    return ui.panel_conditional(
        "input.navigation === 'nav_dashboard'",
        ui.card(
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
    )


def panel_model_build(compilers, build_types):
    return ui.panel_conditional(
        "input.navigation === 'nav_model_build'",
        ui.layout_columns(
            # Left column: Build Configuration
            ui.card(
                ui.card_header("Build Configuration"),

                # Compiler selection
                ui.h6("Compiler"),
                ui.input_radio_buttons(
                    "build_compiler",
                    None,
                    choices={k: v["name"] for k, v in compilers.items()},
                    selected="gfortran"
                ),
                ui.output_ui("compiler_status"),

                ui.tags.hr(),

                # Build type selection
                ui.h6("Build Type"),
                ui.input_radio_buttons(
                    "build_type",
                    None,
                    choices={k: v["name"] for k, v in build_types.items()},
                    selected="release"
                ),
                ui.output_text("build_flags_info"),

                ui.tags.hr(),

                # Build options
                ui.h6("Build Options"),
                ui.tooltip(
                    ui.input_switch("build_clean_first", "Clean before build", value=False),
                    "Remove all object files and rebuild from scratch. Enable when switching compilers or build types."
                ),

                ui.tags.hr(),

                # Target executable name
                ui.h6("Target Executable"),
                ui.output_ui("target_exe_name"),

                ui.tags.hr(),

                # Build actions
                ui.layout_columns(
                    ui.tooltip(
                        ui.input_action_button("btn_build", "Build", class_="btn-primary w-100"),
                        "Compile changed source files and link the executable"
                    ),
                    ui.tooltip(
                        ui.input_action_button("btn_rebuild", "Rebuild All", class_="btn-warning w-100"),
                        "Clean and recompile all source files from scratch"
                    ),
                    col_widths=[6, 6]
                ),
                ui.tags.small("Creates named executable based on compiler and build type.", class_="text-muted mt-2"),

                fill=False
            ),

            # Middle column: Executable Selection
            ui.card(
                ui.card_header("Available Executables"),
                ui.output_ui("executable_list"),
                ui.input_action_button("btn_refresh_executables", "Refresh List", class_="btn-secondary w-100 mt-2"),

                ui.tags.hr(),

                ui.h6("Select for Run"),
                ui.input_select(
                    "active_executable",
                    None,
                    choices=["ESTAS_II"],
                    selected="ESTAS_II"
                ),
                ui.output_ui("executable_info"),

                fill=False
            ),

            # Right column: Build Log
            ui.card(
                ui.card_header(
                    ui.div(
                        "Build Log",
                        ui.input_action_button("btn_clear_build_log", "Clear", class_="btn-sm btn-outline-secondary float-end"),
                        class_="d-flex justify-content-between align-items-center w-100"
                    )
                ),
                ui.output_text_verbatim("build_log", placeholder=True),
                style="height: 500px; overflow-y: auto;",
                fill=True
            ),

            col_widths=[3, 3, 6]
        )
    )


def panel_model_control():
    return ui.panel_conditional(
        "input.navigation === 'nav_model_control'",
        ui.navset_card_tab(
            # Tab 1: Simulation Configuration (extracted Shiny module — ids namespaced sim_config-*)
            sim_config_ui("sim_config"),
            # Tab 2: Run Model
            ui.nav_panel(
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
            ),
            # Tab 3: Output Configuration
            ui.nav_panel(
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
            ),
            id="model_control_tabs"
        )
    )
