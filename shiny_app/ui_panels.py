"""Content-panel fragments for the AQUABC UI (extracted from create_ui())."""
from shiny import ui
from shinywidgets import output_widget  # third-party — plain import, mirrors app.py:42

try:
    from shiny_app.input_analysis import get_input_file_categories
    from shiny_app.simulation_config import OUTPUT_INTERVAL_PRESETS, TIME_STEP_PRESETS
except ImportError:
    from input_analysis import get_input_file_categories
    from simulation_config import OUTPUT_INTERVAL_PRESETS, TIME_STEP_PRESETS


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
            # Tab 1: Simulation Configuration
            ui.nav_panel(
                "Simulation Config",
                ui.tooltip(
                    ui.input_action_button("load_sim_config", "Load Configuration", class_="btn-secondary mb-3"),
                    "Load settings from INPUT.txt file"
                ),
                ui.layout_columns(
                    ui.card(
                        ui.card_header("Time Period"),
                        ui.tooltip(
                            ui.input_numeric("sim_base_year", "Base Year:", value=1998, min=1900, max=2100),
                            "Reference year for input forcing data (e.g., meteorology files)"
                        ),
                        ui.tooltip(
                            ui.input_date("sim_start_date", "Start Date:", value="2015-01-01"),
                            "Simulation start date (converted to day of year)"
                        ),
                        ui.tooltip(
                            ui.input_date("sim_end_date", "End Date:", value="2016-01-01"),
                            "Simulation end date (converted to day of year)"
                        ),
                        ui.output_text("sim_duration_info"),
                        fill=False
                    ),
                    ui.card(
                        ui.card_header("Time Stepping"),
                        ui.tooltip(
                            ui.input_select(
                                "sim_timestep_preset",
                                "Time Step:",
                                choices=list(TIME_STEP_PRESETS.keys()),
                                selected="6 minutes"
                            ),
                            "Preset time step intervals. 6 minutes (240 steps/day) is recommended."
                        ),
                        ui.tooltip(
                            ui.input_numeric("sim_timesteps_per_day", "Steps/Day:", value=240, min=1, max=1440),
                            "Number of model time steps per day. Higher values = more precision but slower."
                        ),
                        ui.output_text("sim_timestep_info"),
                        fill=False
                    ),
                    ui.card(
                        ui.card_header("Output Interval"),
                        ui.tooltip(
                            ui.input_select(
                                "sim_output_preset",
                                "Output Frequency:",
                                choices=list(OUTPUT_INTERVAL_PRESETS.keys()),
                                selected="Hourly"
                            ),
                            "How often to write output. More frequent = larger files."
                        ),
                        ui.tooltip(
                            ui.input_numeric("sim_print_interval", "Print Interval (steps):", value=24, min=1),
                            "Number of time steps between output writes. 24 steps = hourly at 240 steps/day."
                        ),
                        ui.output_text("sim_output_info"),
                        fill=False
                    ),
                    col_widths=[4, 4, 4]
                ),
                ui.tags.hr(),
                ui.layout_columns(
                    ui.card(
                        ui.card_header("Model Options"),
                        ui.tooltip(
                            ui.input_switch("sim_model_sediments", "Enable Sediment Model", value=False),
                            "Enable bottom sediment diagenesis model. Increases computation time significantly."
                        ),
                        ui.tooltip(
                            ui.input_select(
                                "sim_resuspension",
                                "Resuspension Option:",
                                choices={"0": "Disabled", "1": "Fully Prescribed", "2": "Semi-Prescribed"},
                                selected="2"
                            ),
                            "0=No resuspension, 1=Fully prescribed rates, 2=Semi-prescribed (recommended)"
                        ),
                        fill=False
                    ),
                    col_widths=[6, 6]
                ),
                ui.layout_columns(
                    ui.tooltip(
                        ui.input_action_button("save_sim_config", "Save Configuration", class_="btn-success"),
                        "Save current settings to INPUT.txt file"
                    ),
                    ui.output_text("sim_config_save_status"),
                    col_widths=[3, 9]
                )
            ),
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


def panel_input_files():
    return ui.panel_conditional(
        "input.navigation === 'nav_input_files'",
        ui.layout_columns(
            # Left column: File selection and info
            ui.card(
                ui.card_header("File Browser"),
                ui.layout_columns(
                    ui.tooltip(
                        ui.input_select(
                            "file_category_filter",
                            "Filter by category:",
                            choices=["All Categories"] + get_input_file_categories(),
                            selected="All Categories"
                        ),
                        "Filter files by type: Forcing, Constants, Initial Conditions, etc."
                    ),
                    ui.tooltip(
                        ui.input_action_button("refresh_files", "Refresh List", class_="btn-sm btn-secondary mt-4 w-100"),
                        "Rescan input files directory"
                    ),
                    col_widths=[9, 3]
                ),
                ui.tooltip(
                    ui.input_select("file_select", "Select file:", choices=[], size=12),
                    "Click to preview file contents in the right panel"
                ),
                ui.tags.hr(),
                ui.card(
                    ui.card_header("File Information"),
                    ui.output_ui("file_info_panel"),
                    style="max-height: 350px; overflow-y: auto;"
                ),
            ),
            # Right column: File contents + Map Display tabs
            ui.navset_tab(
                ui.nav_panel(
                    "File Contents",
                    ui.card(
                        ui.card_header(ui.output_text("file_header_text")),
                        ui.input_text_area("file_contents", "File contents:", value="", rows=28, width="100%"),
                        ui.tags.small("Read-only preview. Edit files in Parameters, Initial Conditions, or Model Config tabs.", class_="text-muted")
                    ),
                ),
                ui.nav_panel(
                    "Map Display",
                    ui.card(
                        ui.card_header("Box Network & Bathymetry"),
                        ui.layout_columns(
                            ui.input_select(
                                "map_display_view",
                                "View:",
                                choices=["Box Network", "Bathymetry Profile", "Box Depths Overview"],
                                selected="Box Network"
                            ),
                            ui.input_select(
                                "map_bathymetry_box",
                                "Bathymetry box:",
                                choices={str(i): f"Box {i}" for i in range(1, 26)},
                                selected="1"
                            ),
                            col_widths=[6, 6]
                        ),
                        output_widget("map_display_plot", height="1000px"),
                        ui.output_ui("map_display_info"),
                    ),
                ),
                id="input_files_tabs"
            ),
            col_widths=[4, 8]
        )
    )


def panel_sim_config():
    # Simulation Config Panel - now integrated into Model Control
    # (kept as empty conditional for backward compatibility with any references)
    return ui.panel_conditional(
        "input.navigation === 'nav_sim_config_disabled'",
        ui.tags.div()  # Empty placeholder
    )


def panel_scenarios():
    return ui.panel_conditional(
        "input.navigation === 'nav_scenarios'",
        ui.card(
            ui.card_header("Scenario Presets"),
            ui.layout_columns(
                # Load Scenario Section
                ui.card(
                    ui.card_header("Load Scenario"),
                    ui.tooltip(
                        ui.input_select(
                            "scenario_select",
                            "Select Scenario:",
                            choices=[],
                            selected=None
                        ),
                        "Previously saved parameter configurations"
                    ),
                    ui.layout_columns(
                        ui.tooltip(
                            ui.input_action_button("load_scenario", "Load", class_="btn-primary"),
                            "Apply selected scenario to current configuration"
                        ),
                        ui.tooltip(
                            ui.input_action_button("delete_scenario", "Delete", class_="btn-danger"),
                            "Permanently delete selected scenario"
                        ),
                        ui.tooltip(
                            ui.input_action_button("refresh_scenarios", "Refresh", class_="btn-secondary"),
                            "Rescan scenarios directory"
                        ),
                        col_widths=[4, 4, 4]
                    ),
                    ui.tags.hr(),
                    ui.output_ui("scenario_info"),
                    fill=False
                ),
                # Save Scenario Section
                ui.card(
                    ui.card_header("Save Current Configuration"),
                    ui.input_text("new_scenario_name", "Name:", placeholder="Enter scenario name"),
                    ui.input_text_area(
                        "new_scenario_desc",
                        "Description:",
                        placeholder="Brief description of this scenario",
                        rows=2
                    ),
                    ui.tags.p("Include:", class_="fw-bold mt-2 mb-1"),
                    ui.input_checkbox("scenario_include_params", "Parameters (WCONST_04.txt)", value=True),
                    ui.layout_columns(
                        ui.input_checkbox("scenario_include_ics", "Initial Conditions:", value=True),
                        ui.input_select(
                            "save_ic_file",
                            "",
                            choices=["INIT_CONC_1.txt", "INIT_CONC_2.txt"],
                            selected="INIT_CONC_1.txt"
                        ),
                        col_widths=[6, 6]
                    ),
                    ui.input_checkbox("scenario_include_options", "Model Options & Constants", value=True),
                    ui.tooltip(
                        ui.input_action_button("save_scenario", "Save as New Scenario", class_="btn-success mt-2"),
                        "Save current configuration as a named scenario preset"
                    ),
                    fill=False
                ),
                col_widths=[6, 6]
            ),
            ui.tags.hr(),
            ui.output_text("scenario_status")
        )
    )


def panel_plot(min_smooth_window):
    return ui.panel_conditional(
        "input.navigation === 'nav_plot'",
        ui.card(
            ui.card_header("Plot & Visualization"),
            ui.navset_card_tab(
                # Tab 0: Output Directory Selection
                ui.nav_panel(
                    "Output Directory",
                    ui.layout_columns(
                        ui.card(
                            ui.card_header("Select Output Directory"),
                            ui.tooltip(
                                ui.input_select(
                                    "output_dir_select",
                                    "Output Directory:",
                                    choices={}  # Will be populated dynamically
                                ),
                                "Select folder containing model output files"
                            ),
                            ui.tooltip(
                                ui.input_action_button("refresh_output_dirs", "Refresh Directories", class_="btn-secondary w-100 mt-2"),
                                "Rescan for output directories"
                            ),
                            ui.tooltip(
                                ui.input_action_button("analyze_output_dir", "Analyze Directory", class_="btn-info w-100 mt-2"),
                                "Analyze files in selected directory"
                            ),
                            fill=False
                        ),
                        col_widths=[12]
                    ),
                    ui.tags.hr(),
                    ui.card(
                        ui.card_header("Files Summary"),
                        ui.output_ui("output_files_summary"),
                        style="max-height: 400px; overflow-y: auto;"
                    )
                ),
                # Tab 1: Model Output
                ui.nav_panel(
                    "Model Output",
                    ui.layout_columns(
                        ui.card(
                            ui.card_header("Data Source"),
                            ui.tooltip(
                                ui.input_radio_buttons(
                                    "output_format",
                                    "File format:",
                                    choices={"text": "Text (.out)", "binary": "Binary (.bin)", "csv": "CSV"},
                                    selected="text",
                                    inline=True
                                ),
                                "Select output file format to read"
                            ),
                            ui.tooltip(
                                ui.input_select(
                                    "plot_output_file",
                                    "Output file:",
                                    choices={}  # Will be populated from selected output directory
                                ),
                                "Select specific output file to plot"
                            ),
                            ui.output_ui("plot_output_file_info"),
                            ui.tooltip(
                                ui.input_action_button("refresh_plot_files", "Refresh Files", class_="btn-secondary btn-sm w-100 mt-2"),
                                "Rescan output directory for files"
                            ),
                            fill=False
                        ),
                        ui.card(
                            ui.card_header("Selected File Preview"),
                            ui.output_ui("output_file_preview"),
                            fill=False
                        ),
                        ui.card(
                            ui.card_header("Variables"),
                            ui.tooltip(
                                ui.input_selectize("left_vars", "Left axis:", choices=[], multiple=True),
                                "Variables to plot on left Y-axis"
                            ),
                            ui.tooltip(
                                ui.input_selectize("right_vars", "Right axis:", choices=[], multiple=True),
                                "Variables to plot on right Y-axis (different scale)"
                            ),
                            ui.tooltip(
                                ui.input_checkbox("log_left", "Log scale left"),
                                "Use logarithmic scale for left axis"
                            ),
                            ui.tooltip(
                                ui.input_checkbox("log_right", "Log scale right"),
                                "Use logarithmic scale for right axis"
                            ),
                            fill=False
                        ),
                        ui.card(
                            ui.card_header("Options"),
                            ui.tooltip(
                                ui.input_checkbox("smooth", "Apply rolling mean"),
                                "Smooth data using rolling average window"
                            ),
                            ui.tooltip(
                                ui.input_slider("smooth_window", "Window size:", min=min_smooth_window, max=101, value=5, step=1),
                                "Number of data points for rolling mean calculation"
                            ),
                            ui.tooltip(
                                ui.input_slider("nrows", "Preview rows:", min=10, max=1000, value=200, step=10),
                                "Number of rows to load for preview (affects performance)"
                            ),
                            ui.tooltip(
                                ui.input_action_button("refresh_plot", "Refresh Plot", class_="btn-info w-100"),
                                "Update plot with current settings"
                            ),
                            fill=False
                        ),
                        col_widths=[3, 3, 3, 3]
                    ),
                    ui.tags.hr(),
                    ui.div(
                        output_widget("main_plot"),
                        style="min-height: 400px;"
                    )
                ),
                # Tab 2: Input Timeseries
                ui.nav_panel(
                    "Input Timeseries",
                    ui.layout_columns(
                        ui.card(
                            ui.card_header("Select Data"),
                            ui.tooltip(
                                ui.input_select(
                                    "input_ts_file",
                                    "Timeseries file:",
                                    choices={
                                        "TEMP_TS.txt": "Temperature",
                                        "SALT_TS.txt": "Salinity",
                                        "FLOW_TS.txt": "Flow",
                                        "SOLAR_RAD_TS.txt": "Solar Radiation",
                                        "WIND_SPEED_TS.txt": "Wind Speed",
                                        "AIR_TEMP_TS.txt": "Air Temperature",
                                        "SHEAR_STRESSES_TS.txt": "Shear Stress",
                                    }
                                ),
                                "Select forcing input timeseries file to visualize"
                            ),
                            ui.input_selectize(
                                "input_ts_boxes",
                                "Select boxes:",
                                choices=[],
                                multiple=True
                            ),
                            ui.input_action_button("plot_input_ts", "Plot Timeseries", class_="btn-info w-100 mt-2"),
                            fill=False
                        ),
                        ui.card(
                            ui.card_header("Time Range"),
                            ui.output_text("input_ts_info"),
                            ui.input_checkbox("input_ts_subset", "Limit time range"),
                            ui.output_ui("input_ts_date_range"),
                            fill=False
                        ),
                        col_widths=[6, 6]
                    ),
                    ui.tags.hr(),
                    ui.div(
                        output_widget("input_ts_plot"),
                        style="min-height: 400px;"
                    )
                ),
                # Tab 3: Output Preview (table)
                ui.nav_panel(
                    "Data Preview",
                    ui.div(
                        ui.output_table("out_preview"),
                        style="max-height: 500px; overflow-y: auto;"
                    )
                ),
                id="plot_tabs"
            )
        )
    )


def panel_mass_balance():
    return ui.panel_conditional(
        "input.navigation === 'nav_mass_balance'",
        ui.card(
            ui.card_header("Mass Balance"),
            ui.tooltip(
                ui.input_action_button("calc_mass_balance", "Calculate Mass Balance", class_="btn-primary mb-3"),
                "Calculate element mass balance from model output"
            ),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Summary"),
                    ui.output_table("mass_balance_summary"),
                ),
                ui.card(
                    ui.card_header("Element Details"),
                    ui.tooltip(
                        ui.input_select(
                            "mb_element",
                            "Element:",
                            choices=["Nitrogen", "Carbon", "Phosphorus", "Silicon"],
                            selected="Nitrogen"
                        ),
                        "Select element for detailed mass balance breakdown"
                    ),
                    ui.output_ui("mass_balance_details"),
                ),
                col_widths=[6, 6]
            ),
            ui.card(
                ui.card_header("Time Series"),
                ui.output_ui("mass_balance_plot_ui"),
            )
        )
    )


def panel_observations():
    return ui.panel_conditional(
        "input.navigation === 'nav_observations'",
        ui.card(
            ui.card_header("Model Validation - Observations"),
            ui.layout_columns(
                # Left column: File selection
                ui.card(
                    ui.card_header(
                        ui.tags.i(class_="bi bi-folder2-open me-2"),
                        "Observation Files"
                    ),
                    ui.tooltip(
                        ui.input_action_button("obs_scan_dir", "Scan OBSERVATIONS Directory",
                                              class_="btn-outline-primary btn-sm w-100 mb-2"),
                        "Scan the OBSERVATIONS folder for available data files"
                    ),
                    ui.input_select("obs_file_select", "Select file:", choices=[], width="100%"),
                    ui.tooltip(
                        ui.input_action_button("obs_load_file", "Load Selected File",
                                              class_="btn-primary btn-sm w-100 mb-2"),
                        "Load the selected observation file"
                    ),
                    ui.hr(),
                    ui.tags.small("Or upload your own file:", class_="text-muted d-block mb-1"),
                    ui.tooltip(
                        ui.input_file("obs_file", "Upload CSV/Excel:",
                                     accept=[".csv", ".xlsx", ".dates"], multiple=False),
                        "Upload CSV, Excel, or .dates observation file"
                    ),
                    ui.hr(),
                    ui.tooltip(
                        ui.input_action_button("generate_sample_obs", "Generate Sample Data",
                                              class_="btn-outline-secondary btn-sm w-100"),
                        "Generate synthetic observation data for testing"
                    ),
                    fill=False
                ),
                # Right column: File preview
                ui.card(
                    ui.card_header(
                        ui.tags.i(class_="bi bi-table me-2"),
                        "File Preview"
                    ),
                    ui.output_ui("obs_file_info"),
                    ui.output_ui("obs_variables_table"),
                ),
                col_widths=[4, 8]
            ),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Comparison Summary"),
                    ui.output_table("obs_comparison_summary"),
                ),
                col_widths=[12]
            ),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Variable Details"),
                    ui.input_select("obs_variable", "Variable:", choices=[], selected=None),
                    ui.output_ui("obs_metrics_detail"),
                ),
                ui.card(
                    ui.card_header("Scatter Plot Info"),
                    ui.output_ui("obs_scatter_info"),
                ),
                col_widths=[6, 6]
            )
        )
    )

