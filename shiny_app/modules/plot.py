"""Plots tab as a Shiny module (Phase 3, Task 3 — largest module; merged plot + output_browser).

`plot_ui(id, min_smooth_window)` returns the panel *content* of the former
`panel_plot` (the app-level panel_conditional stays in create_ui);
`plot_server(id, state)` registers ALL 18 handlers (plotting + output-directory
browsing + file preview + input-timeseries) ported verbatim from app.py, plus
the 3 csv-cache reactive values. Since the whole Plots tab is ONE module, every
id namespaces uniformly to `plot-*` and the output selection stays internal
(no cross-module bus — the dead Phase-0 `state.selected_output_*` was removed).

Cross-namespace note: `init_output_dirs` also initialises `sim_output_dir`,
a widget that lives in the `run_control` module's "Output Config" sub-tab.
That single update is routed through
`session.root_scope().make_scope("run_control")` with the bare id
`"sim_output_dir"` so it reaches `run_control-sim_output_dir` instead of a
nonexistent `plot-sim_output_dir` (and instead of the banned hyphenated
literal `"run_control-sim_output_dir"`, which raises `ValueError` in
`validate_id`). Everything else is a verbatim port.

Self-contained: imports output_data, ic_parser
(get_grouped_variable_choices/get_variable_info), utils
(read_pelagic_binary/read_pelagic_text), input_analysis (analyze_input_file)
and simulation_config (SimulationConfigFile); self-computes
ROOT/INPUTS_DIR/OUTPUT_CSV/INPUT_TXT_PATH; imports nothing from app.py.
`state` is accepted for convention but unused.
"""
import logging
import os
import time
from datetime import date, datetime, timedelta

import pandas as pd
import plotly.graph_objects as go
from shiny import module, reactive, render, ui
from shinywidgets import output_widget, render_widget

try:
    from shiny_app import output_data
    from shiny_app.ic_parser import get_grouped_variable_choices, get_variable_info
    from shiny_app.input_analysis import analyze_input_file
    from shiny_app.simulation_config import SimulationConfigFile
    from shiny_app.utils import read_pelagic_binary, read_pelagic_text
except ImportError:  # running as a script from inside shiny_app/
    import output_data
    from ic_parser import get_grouped_variable_choices, get_variable_info
    from input_analysis import analyze_input_file
    from simulation_config import SimulationConfigFile
    from utils import read_pelagic_binary, read_pelagic_text

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")
OUTPUT_CSV = os.path.join(ROOT, "OUTPUT.csv")
INPUT_TXT_PATH = os.path.join(ROOT, "INPUT.txt")
DEFAULT_PLOT_ROWS = 10000  # Max rows to read for plotting to avoid OOM


@module.ui
def plot_ui(min_smooth_window):
    return ui.card(
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


@module.server
def plot_server(input, output, session, state):
    run = state.run
    # `sim_output_dir` lives in the run_control module's Output Config tab;
    # reach it through session.root_scope().make_scope("run_control") + bare id.
    rc = session.root_scope().make_scope("run_control")

    # CSV caching - reactive values to cache CSV data
    csv_cache = reactive.Value(None)
    csv_cache_mtime = reactive.Value(0)
    csv_cache_path = reactive.Value(None)

    def _get_cached_data(max_rows=None, file_path=None, file_format=None):
        """Get cached output data or reload if modified.

        Args:
            max_rows: Maximum number of rows to read
            file_path: Path to output file
            file_format: 'csv', 'text' (.out), or 'binary' (.bin). Auto-detect if None.
        """
        try:
            target_path = file_path or OUTPUT_CSV

            if not os.path.exists(target_path):
                logger.warning(f"Output file does not exist: {target_path}")
                return None

            # Auto-detect format from extension if not specified
            if file_format is None:
                if target_path.endswith('.bin'):
                    file_format = 'binary'
                elif target_path.endswith('.out'):
                    file_format = 'text'
                else:
                    file_format = 'csv'

            current_mtime = os.path.getmtime(target_path)
            cached_mtime = csv_cache_mtime.get()
            cached_path = csv_cache_path.get()

            # Check if cache is valid (same file, same mtime)
            if cached_path == target_path and cached_mtime == current_mtime and csv_cache.get() is not None:
                logger.debug("Using cached output data")
                return csv_cache.get()

            logger.info(f"Loading {file_format} output (max_rows={max_rows}, file={os.path.basename(target_path)})")
            start_time = time.time()

            if file_format == 'binary':
                # Read Fortran binary file
                df = read_pelagic_binary(target_path, max_rows=max_rows)
            elif file_format == 'text':
                # Read PELAGIC_BOX whitespace-separated file
                df = read_pelagic_text(target_path, max_rows=max_rows)
            else:
                # Read CSV file
                df = pd.read_csv(target_path, comment='#', skip_blank_lines=True, nrows=max_rows)
                df.columns = [c.strip() for c in df.columns]

            load_time = time.time() - start_time
            logger.info(f"Loaded {len(df)} rows, {len(df.columns)} columns in {load_time:.2f}s")

            if max_rows is None:  # Only cache full reads
                csv_cache.set(df)
                csv_cache_mtime.set(current_mtime)
                csv_cache_path.set(target_path)
                logger.debug("Output data cached for future use")
            return df
        except Exception as e:
            logger.error(f"Error reading output file: {e}", exc_info=True)
            return None

    # Keep old function for compatibility
    def _get_cached_csv(max_rows=None, file_path=None):
        """Legacy wrapper for _get_cached_data."""
        return _get_cached_data(max_rows=max_rows, file_path=file_path)

    # load available Y variables from output file header
    @reactive.Effect
    @reactive.event(input.plot_output_file, input.output_format, input.output_dir_select)
    def _update_variable_choices():
        """Update variable choices when output file, format, or directory changes"""
        try:
            file_format = input.output_format() if hasattr(input, 'output_format') else None
            selected_file = get_selected_output_file_path()

            if not selected_file or not os.path.exists(selected_file):
                ui.update_selectize("left_vars", choices=[], selected=[])
                ui.update_selectize("right_vars", choices=[], selected=[])
                return

            cols = output_data.get_output_columns(file_path=selected_file, file_format=file_format)

            if cols:
                # TIME is first column, get the rest
                y_cols = cols[1:]

                # Create grouped choices with descriptive names
                grouped_choices = get_grouped_variable_choices(y_cols)

                # Default select first variable on left, none on right
                first_var = y_cols[0] if y_cols else None
                ui.update_selectize("left_vars", choices=grouped_choices, selected=[first_var] if first_var else [])
                ui.update_selectize("right_vars", choices=grouped_choices, selected=[])
                logger.info(f"Updated variable choices: {len(y_cols)} variables available")
            else:
                ui.update_selectize("left_vars", choices=[], selected=[])
                ui.update_selectize("right_vars", choices=[], selected=[])
        except Exception as e:
            logger.debug(f"Variable choices update deferred: {e}")

    @render.table
    def out_preview():
        try:
            # Limit rows using nrows to avoid full read
            n = input.nrows()
            if n is None:
                n = 10
            df = pd.read_csv(OUTPUT_CSV, comment='#', nrows=n)
            # Strip whitespace from column names
            df.columns = [c.strip() for c in df.columns]
            return df
        except Exception as e:
            logger.error(f"Error reading OUTPUT.csv preview: {e}")
            return pd.DataFrame([["error reading OUTPUT.csv", str(e)]])

    # ========== OUTPUT DIRECTORY MANAGEMENT ==========
    def analyze_output_directory(dir_name):
        """Analyze files in an output directory and return summary"""
        if dir_name == "ROOT":
            dir_path = ROOT
            files_to_check = ["OUTPUT.csv"]
        else:
            dir_path = os.path.join(ROOT, dir_name)
            if not os.path.isdir(dir_path):
                return {"error": f"Directory not found: {dir_name}"}
            files_to_check = os.listdir(dir_path)

        summary = {
            "path": dir_path,
            "files": [],
            "total_size": 0,
            "csv_files": 0,
            "out_files": 0,
            "mtrx_files": 0,
            "bin_files": 0,
        }

        for fname in files_to_check:
            if dir_name == "ROOT":
                fpath = os.path.join(ROOT, fname)
            else:
                fpath = os.path.join(dir_path, fname)

            if not os.path.isfile(fpath):
                continue

            try:
                stat = os.stat(fpath)
                size = stat.st_size
                mtime = datetime.fromtimestamp(stat.st_mtime).strftime('%Y-%m-%d %H:%M:%S')

                file_info = {
                    "name": fname,
                    "size": size,
                    "size_str": f"{size / 1024:.1f} KB" if size < 1024*1024 else f"{size / (1024*1024):.2f} MB",
                    "modified": mtime,
                    "type": "unknown"
                }

                # Categorize file
                if fname.endswith(".csv"):
                    file_info["type"] = "csv"
                    summary["csv_files"] += 1
                    # Try to get row count for CSV
                    try:
                        with open(fpath, 'rb') as f:
                            lines = sum(1 for _ in f)
                        file_info["rows"] = lines
                    except (OSError, UnicodeDecodeError):
                        file_info["rows"] = "?"
                elif fname.endswith(".out"):
                    file_info["type"] = "output"
                    summary["out_files"] += 1
                elif fname.endswith(".mtrx"):
                    file_info["type"] = "matrix"
                    summary["mtrx_files"] += 1
                elif fname.endswith(".bin"):
                    file_info["type"] = "binary"
                    summary["bin_files"] += 1

                summary["files"].append(file_info)
                summary["total_size"] += size
            except Exception as e:
                logger.warning(f"Error analyzing file {fname}: {e}")

        # Sort files by type then name
        summary["files"].sort(key=lambda x: (x["type"], x["name"]))
        summary["total_size_str"] = f"{summary['total_size'] / (1024*1024):.2f} MB"
        return summary

    @reactive.effect
    def init_output_dirs():
        """Initialize output directory selection on startup - uses INPUT.txt value"""
        dirs = output_data.get_output_directories()
        if dirs:
            # Try to get output folder from INPUT.txt config
            default_dir = "OUTPUTS"  # fallback
            try:
                if os.path.exists(INPUT_TXT_PATH):
                    scf = SimulationConfigFile(INPUT_TXT_PATH)
                    if scf.parse():
                        # Get output folder from config (strip trailing slash)
                        config_output = scf.config.output_folder.rstrip('/')
                        if config_output in dirs:
                            default_dir = config_output
                            logger.info(f"Using output folder from INPUT.txt: {default_dir}")
                        else:
                            logger.warning(f"Output folder '{config_output}' from INPUT.txt not found, using OUTPUTS")
            except Exception as e:
                logger.warning(f"Could not read output folder from INPUT.txt: {e}")

            # Fall back to OUTPUTS if no config or folder not found
            if default_dir not in dirs:
                default_dir = "OUTPUTS" if "OUTPUTS" in dirs else list(dirs.keys())[0]

            ui.update_select("output_dir_select", choices=dirs, selected=default_dir)
            # Also update Model Config output directory (run_control module's tab)
            ui.update_select("sim_output_dir", choices=dirs, selected=default_dir, session=rc)

    @reactive.effect
    @reactive.event(input.refresh_output_dirs)
    def refresh_output_dirs():
        """Refresh the list of output directories"""
        dirs = output_data.get_output_directories()
        current = input.output_dir_select()
        selected = current if current in dirs else (list(dirs.keys())[0] if dirs else None)
        ui.update_select("output_dir_select", choices=dirs, selected=selected)

    @render.ui
    def output_file_preview():
        """Display preview info about the selected output file"""
        dir_name = input.output_dir_select()
        file_name = input.plot_output_file()

        if not dir_name or not file_name:
            return ui.div(ui.tags.em("Select a file to preview", class_="text-muted"))

        # Build file path
        if dir_name == "ROOT":
            file_path = os.path.join(ROOT, file_name)
        else:
            file_path = os.path.join(ROOT, dir_name, file_name)

        if not os.path.exists(file_path):
            return ui.div(ui.tags.em(f"File not found: {file_name}", class_="text-danger"))

        try:
            # Get file stats
            stat = os.stat(file_path)
            mtime = datetime.fromtimestamp(stat.st_mtime).strftime('%Y-%m-%d %H:%M:%S')
            size = stat.st_size
            size_str = f"{size / 1024:.1f} KB" if size < 1024*1024 else f"{size / (1024*1024):.2f} MB"

            # Detect file format and read metadata
            file_ext = os.path.splitext(file_path)[1].lower()
            num_vars = 0
            num_rows = 0
            time_range = ""

            if file_ext == '.bin':
                # Binary file - calculate from file size
                num_cols = 37  # PELAGIC_BOX binary format
                bytes_per_row = num_cols * 8  # float64
                num_rows = size // bytes_per_row
                num_vars = num_cols - 1  # Exclude TIME column
                # Read first and last time values
                try:
                    import numpy as np
                    data = np.fromfile(file_path, dtype=np.float64)
                    if len(data) >= num_cols:
                        first_time = data[0]
                        last_time = data[-num_cols]
                        from datetime import datetime as dt
                        from datetime import timedelta
                        ref_date = dt(1997, 1, 1)
                        start_date = (ref_date + timedelta(days=float(first_time))).strftime('%Y-%m-%d')
                        end_date = (ref_date + timedelta(days=float(last_time))).strftime('%Y-%m-%d')
                        time_range = f"{start_date} to {end_date}"
                except (OSError, ValueError, IndexError):
                    pass
            elif file_ext == '.csv':
                # CSV file
                df = pd.read_csv(file_path, comment='#', nrows=5)
                num_vars = len(df.columns) - 1  # Exclude TIME
                # Count rows
                with open(file_path, 'rb') as f:
                    num_rows = sum(1 for _ in f) - 1  # Subtract header
            elif file_ext in ['.out', '.txt', '.dat']:
                # Whitespace-delimited file
                df = pd.read_csv(file_path, sep=r'\s+', nrows=5)
                num_vars = len(df.columns) - 1  # Exclude TIME
                # Count rows
                with open(file_path, 'rb') as f:
                    num_rows = sum(1 for _ in f) - 1  # Subtract header
                # Try to get time range
                try:
                    df_full = pd.read_csv(file_path, sep=r'\s+', usecols=[0])
                    first_time = df_full.iloc[0, 0]
                    last_time = df_full.iloc[-1, 0]
                    from datetime import datetime as dt
                    from datetime import timedelta
                    ref_date = dt(1997, 1, 1)
                    start_date = (ref_date + timedelta(days=float(first_time))).strftime('%Y-%m-%d')
                    end_date = (ref_date + timedelta(days=float(last_time))).strftime('%Y-%m-%d')
                    time_range = f"{start_date} to {end_date}"
                except (OSError, ValueError, IndexError, KeyError):
                    pass

            # Build preview display
            items = [
                ui.tags.p(ui.tags.strong("📄 File: "), file_name),
                ui.tags.p(ui.tags.strong("📊 Variables: "), str(num_vars)),
                ui.tags.p(ui.tags.strong("📈 Data points: "), f"{num_rows:,}"),
                ui.tags.p(ui.tags.strong("💾 Size: "), size_str),
            ]

            if time_range:
                items.append(ui.tags.p(ui.tags.strong("📅 Period: "), time_range))

            items.append(ui.tags.p(ui.tags.strong("🕐 Modified: "), mtime))

            return ui.div(*items)

        except Exception as e:
            logger.warning(f"Error previewing file {file_path}: {e}")
            return ui.div(ui.tags.em(f"Error reading file: {str(e)}", class_="text-danger"))

    @render.ui
    @reactive.event(input.analyze_output_dir)
    def output_files_summary():
        """Analyze and display detailed file summary for selected directory"""
        dir_name = input.output_dir_select()
        if not dir_name:
            return ui.div(ui.tags.em("Select a directory and click 'Analyze Directory'", class_="text-muted"))

        summary = analyze_output_directory(dir_name)

        if "error" in summary:
            return ui.div(ui.tags.span(summary["error"], class_="text-danger"))

        if not summary["files"]:
            return ui.div(ui.tags.em("No files found in directory", class_="text-muted"))

        # Build summary cards
        stats_row = ui.layout_columns(
            ui.value_box(
                "Total Files",
                str(len(summary["files"])),
                theme="primary"
            ),
            ui.value_box(
                "CSV Files",
                str(summary["csv_files"]),
                theme="success"
            ),
            ui.value_box(
                "Output Files",
                str(summary["out_files"]),
                theme="info"
            ),
            ui.value_box(
                "Total Size",
                summary["total_size_str"],
                theme="secondary"
            ),
            col_widths=[3, 3, 3, 3]
        )

        # Build file table
        table_rows = []
        for f in summary["files"]:
            type_badge = {
                "csv": ("CSV", "bg-success"),
                "output": ("OUT", "bg-info"),
                "matrix": ("MTRX", "bg-warning"),
                "binary": ("BIN", "bg-secondary"),
                "unknown": ("?", "bg-light text-dark"),
            }.get(f["type"], ("?", "bg-light"))

            row_content = [
                ui.tags.td(ui.tags.span(type_badge[0], class_=f"badge {type_badge[1]}")),
                ui.tags.td(f["name"]),
                ui.tags.td(f["size_str"]),
                ui.tags.td(f["modified"]),
            ]
            if "rows" in f:
                row_content.append(ui.tags.td(str(f["rows"]) + " rows"))
            else:
                row_content.append(ui.tags.td("-"))

            table_rows.append(ui.tags.tr(*row_content))

        file_table = ui.tags.table(
            ui.tags.thead(
                ui.tags.tr(
                    ui.tags.th("Type"),
                    ui.tags.th("Filename"),
                    ui.tags.th("Size"),
                    ui.tags.th("Modified"),
                    ui.tags.th("Info"),
                )
            ),
            ui.tags.tbody(*table_rows),
            class_="table table-sm table-striped"
        )

        return ui.div(
            stats_row,
            ui.tags.hr(),
            file_table
        )

    # ========== PLOT OUTPUT FILE SELECTION ==========
    def get_selected_output_file_path():
        """Get full path to selected output file"""
        dir_name = input.output_dir_select()
        file_name = input.plot_output_file()

        if not dir_name or not file_name:
            return None

        if dir_name == "ROOT":
            return os.path.join(ROOT, file_name)
        else:
            return os.path.join(ROOT, dir_name, file_name)

    @reactive.effect
    def update_plot_output_files():
        """Update file selection when output directory or format changes"""
        dir_name = input.output_dir_select()
        file_format = input.output_format() if hasattr(input, 'output_format') else "text"
        files = output_data.get_output_files_from_dir(dir_name, file_format)

        # Select first file by default
        selected = None
        if "OUTPUT.csv" in files:
            selected = "OUTPUT.csv"
        elif files:
            selected = list(files.keys())[0]

        ui.update_select("plot_output_file", choices=files, selected=selected)

    @reactive.effect
    @reactive.event(input.refresh_plot_files, input.output_format)
    def refresh_plot_output_files():
        """Refresh the list of output files when format changes"""
        dir_name = input.output_dir_select()
        file_format = input.output_format() if hasattr(input, 'output_format') else "text"
        files = output_data.get_output_files_from_dir(dir_name, file_format)
        current = input.plot_output_file()
        selected = current if current in files else (list(files.keys())[0] if files else None)
        ui.update_select("plot_output_file", choices=files, selected=selected)

    @render.ui
    def plot_output_file_info():
        """Display info about selected output file"""
        file_path = get_selected_output_file_path()

        if not file_path or not os.path.exists(file_path):
            return ui.div(ui.tags.small("No file selected", class_="text-muted"))

        try:
            stat = os.stat(file_path)
            size = stat.st_size
            mtime = datetime.fromtimestamp(stat.st_mtime).strftime('%Y-%m-%d %H:%M')

            # Count lines
            with open(file_path, 'rb') as f:
                lines = sum(1 for _ in f)

            size_str = f"{size / 1024:.1f} KB" if size < 1024*1024 else f"{size / (1024*1024):.2f} MB"

            return ui.div(
                ui.tags.small(f"📄 {size_str} | {lines:,} rows | {mtime}", class_="text-muted")
            )
        except Exception as e:
            return ui.div(ui.tags.small(f"Error: {e}", class_="text-danger"))

    # NOTE: update_plot_variables was removed — _update_variable_choices (earlier)
    # now handles all three triggers: plot_output_file, output_format, output_dir_select

    # ========== INPUT TIMESERIES PLOTTING ==========
    @reactive.effect
    def update_input_ts_boxes():
        """Update box selection when timeseries file changes"""
        ts_file = input.input_ts_file()
        if not ts_file:
            return

        # Get available boxes from the timeseries file
        filepath = os.path.join(INPUTS_DIR, ts_file)
        if os.path.exists(filepath):
            analysis = analyze_input_file(filepath)
            num_vars = analysis.get("num_variables", 0)
            if num_vars > 0:
                # Create box choices (1-indexed)
                boxes = {str(i): f"Box {i}" for i in range(1, min(num_vars + 1, run.current_setup().box_count + 1))}
                ui.update_selectize("input_ts_boxes", choices=boxes, selected=["1"])

    @render.text
    def input_ts_info():
        """Display info about selected timeseries file"""
        ts_file = input.input_ts_file()
        if not ts_file:
            return "Select a timeseries file"

        filepath = os.path.join(INPUTS_DIR, ts_file)
        if not os.path.exists(filepath):
            return f"File not found: {ts_file}"

        analysis = analyze_input_file(filepath)
        info_parts = []

        if analysis.get("date_start") and analysis.get("date_end"):
            info_parts.append(f"Period: {analysis['date_start']} to {analysis['date_end']}")

        if analysis.get("data_size"):
            info_parts.append(f"Data points: {analysis['data_size']:,}")

        if analysis.get("num_variables"):
            info_parts.append(f"Variables: {analysis['num_variables']}")

        return " | ".join(info_parts) if info_parts else "File info unavailable"

    @render.ui
    def input_ts_date_range():
        """Show date range selector if subset is enabled"""
        if not input.input_ts_subset():
            return ui.TagList()

        ts_file = input.input_ts_file()
        if not ts_file:
            return ui.TagList()

        filepath = os.path.join(INPUTS_DIR, ts_file)
        analysis = analyze_input_file(filepath)

        start_date = analysis.get("date_start", "2008-01-01")
        end_date = analysis.get("date_end", "2015-01-01")

        return ui.TagList(
            ui.input_date("input_ts_start", "Start:", value=start_date),
            ui.input_date("input_ts_end", "End:", value=end_date)
        )

    @render_widget
    @reactive.event(input.plot_input_ts)
    def input_ts_plot():
        """Plot selected input timeseries for selected boxes"""
        ts_file = input.input_ts_file()
        selected_boxes = list(input.input_ts_boxes() or [])

        if not ts_file or not selected_boxes:
            logger.info("No timeseries file or boxes selected")
            return None

        filepath = os.path.join(INPUTS_DIR, ts_file)
        if not os.path.exists(filepath):
            logger.warning(f"Timeseries file not found: {filepath}")
            return None

        logger.info(f"Plotting input timeseries: {ts_file}, boxes: {selected_boxes}")

        try:
            # Read the timeseries file
            # Skip header lines (lines starting with #)
            with open(filepath) as f:
                lines = f.readlines()

            # Find data start - look for the column header line "# TIME" and start after it
            # The data format has headers with comments, then "# TIME TEMP1 TEMP2 ..."
            # followed by actual data rows
            data_start = 0
            for i, line in enumerate(lines):
                stripped = line.strip()
                # Look for the column header line that contains "TIME" as a comment
                if stripped.startswith('#') and 'TIME' in stripped.upper():
                    # Data starts on the next line
                    data_start = i + 1
                    # Don't break - keep looking for the LAST such header

            # If no TIME header found, fall back to finding first data line
            if data_start == 0:
                for i, line in enumerate(lines):
                    stripped = line.strip()
                    if stripped and not stripped.startswith('#'):
                        parts = stripped.split()
                        if len(parts) >= 2:
                            try:
                                float(parts[0])
                                float(parts[1])
                                data_start = i
                                break
                            except (ValueError, IndexError):
                                continue

            logger.debug(f"Input timeseries data starts at line {data_start}")

            # Read as DataFrame
            df = pd.read_csv(filepath, skiprows=data_start, sep=r'\s+', header=None)

            if df.empty:
                logger.warning("No data in timeseries file")
                return None

            # First column is TIME
            time_col = df.iloc[:, 0]

            # Convert Julian days to dates
            reference_date = date(1997, 1, 1)
            dates = [reference_date + timedelta(days=float(t)) for t in time_col]

            # Apply time subsetting if enabled
            if input.input_ts_subset():
                try:
                    start = input.input_ts_start()
                    end = input.input_ts_end()
                    if start and end:
                        mask = [(d >= start and d <= end) for d in dates]
                        df = df[mask]
                        dates = [d for d, m in zip(dates, mask) if m]
                except Exception as e:
                    logger.warning(f"Error applying date filter: {e}")

            # Create plot
            fig = go.Figure()

            # Get variable name base from filename
            var_name = ts_file.replace("_TS.txt", "").replace(".txt", "")

            for box in selected_boxes:
                box_idx = int(box)  # 1-indexed
                if box_idx < df.shape[1]:
                    y_data = df.iloc[:, box_idx]
                    fig.add_trace(go.Scatter(
                        x=dates,
                        y=y_data,
                        mode='lines',
                        name=f"Box {box_idx}"
                    ))

            fig.update_layout(
                title=f"{var_name} - Input Timeseries",
                xaxis_title="Date",
                yaxis_title=var_name,
                hovermode='x unified'
            )

            logger.info(f"Input timeseries plot generated with {len(selected_boxes)} traces")
            return go.FigureWidget(fig)

        except Exception as e:
            logger.error(f"Error plotting input timeseries: {e}")
            return None

    @render_widget
    @reactive.event(input.refresh_plot, ignore_none=False)
    def main_plot():
        logger.info("Generating plot (triggered by 'Refresh plot' button)")
        left = list(input.left_vars() or [])
        right = list(input.right_vars() or [])
        logger.debug(f"Left vars: {left}, Right vars: {right}")

        if not left and not right:
            logger.info("No variables selected, skipping plot")
            return None

        # Get the selected output file path and format
        selected_file = get_selected_output_file_path()
        if not selected_file:
            logger.warning("No output file selected")
            return None

        file_format = input.output_format() if hasattr(input, 'output_format') else None
        logger.info(f"Plotting from: {selected_file} (format: {file_format})")

        # Use format-aware data loading
        logger.debug(f"Fetching data (max {DEFAULT_PLOT_ROWS} rows, format={file_format})")
        df = _get_cached_data(max_rows=DEFAULT_PLOT_ROWS, file_path=selected_file, file_format=file_format)
        if df is None or df.empty:
            logger.warning("No data available for plotting")
            return None

        xcol = df.columns[0]
        apply_smooth = input.smooth()
        win = input.smooth_window() if apply_smooth else 1
        logger.debug(f"X-axis: {xcol}, Smoothing: {apply_smooth} (window={win})")

        # Convert Julian days to actual dates for the x-axis
        # Reference date: 1997-01-01 (used in AQUABC model)
        from datetime import datetime, timedelta
        reference_date = datetime(1997, 1, 1)
        try:
            # Create date column from Julian days
            x_dates = [reference_date + timedelta(days=float(jd)) for jd in df[xcol]]
            x_data = x_dates
            x_label = 'Date'
            x_is_date = True
            logger.debug(f"Date conversion successful: {x_dates[0]} to {x_dates[-1]}")
        except (ValueError, TypeError) as e:
            logger.warning(f"Could not convert Julian days to dates: {e}")
            x_data = df[xcol].tolist()
            x_label = xcol
            x_is_date = False

        fig = go.Figure()
        trace_count = 0

        # Helper function to get just the description (no code, no units)
        def get_var_description(var):
            info = get_variable_info(var)
            if info:
                return info['description']
            return var

        # left axis traces
        for var in left:
            if var not in df.columns:
                logger.warning(f"Variable '{var}' not found in CSV columns")
                continue
            y = df[var]
            if apply_smooth and win > 1:
                y = y.rolling(window=win, min_periods=1).mean()
            fig.add_trace(go.Scatter(x=x_data, y=y.tolist(), mode='lines', name=get_var_description(var), yaxis='y'))
            trace_count += 1

        # right axis traces
        for var in right:
            if var not in df.columns:
                logger.warning(f"Variable '{var}' not found in CSV columns")
                continue
            y = df[var]
            if apply_smooth and win > 1:
                y = y.rolling(window=win, min_periods=1).mean()
            fig.add_trace(go.Scatter(x=x_data, y=y.tolist(), mode='lines', name=get_var_description(var), yaxis='y2'))
            trace_count += 1

        logger.debug(f"Created {trace_count} traces")

        # Create descriptive axis titles using just descriptions
        left_descriptions = [get_var_description(var) for var in left]
        right_descriptions = [get_var_description(var) for var in right]
        left_title = ', '.join(left_descriptions) if left else 'Left axis'
        right_title = ', '.join(right_descriptions) if right else 'Right axis'

        # Plot title with variable descriptions
        all_vars = [get_var_description(v) for v in (left + right)]
        plot_title = f"{', '.join(all_vars)} vs {x_label}"

        # Configure xaxis based on whether we have date data
        if x_is_date:
            xaxis_config = dict(
                title=x_label,
                type='date',
                tickformat='%Y-%m-%d',
                tickmode='auto',
                nticks=10  # Show approximately 10 ticks
            )
        else:
            xaxis_config = dict(title=x_label)

        layout = dict(
            title=plot_title,
            yaxis=dict(title=left_title),
            xaxis=xaxis_config,
            # Horizontal legend at the bottom to save space
            legend=dict(
                orientation='h',
                yanchor='top',
                y=-0.15,
                xanchor='center',
                x=0.5
            ),
            margin=dict(b=80)  # Extra bottom margin for legend
        )

        if right:
            layout['yaxis2'] = dict(title=right_title, overlaying='y', side='right')

        log_scale_info = []
        if input.log_left():
            layout['yaxis']['type'] = 'log'
            log_scale_info.append("left")

        if right and input.log_right():
            if 'yaxis2' not in layout:
                layout['yaxis2'] = {}
            layout['yaxis2']['type'] = 'log'
            log_scale_info.append("right")

        if log_scale_info:
            logger.debug(f"Log scale applied to: {', '.join(log_scale_info)}")

        fig.update_layout(**layout)
        logger.info(f"Plot generated successfully with {trace_count} traces")
        return go.FigureWidget(fig)
