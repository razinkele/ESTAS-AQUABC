"""Observations tab as a Shiny module (Phase 3, Task 2).

`observations_ui(id)` returns the panel *content* (the app-level
panel_conditional stays in create_ui); `observations_server(id, state)`
registers the handlers, ported verbatim from app.py. Self-contained: imports
the `observation_compare` and `obs_loader` leaf modules and self-computes
ROOT/OUTPUT_CSV; imports nothing from app.py.
"""
import asyncio
import logging
import os

import pandas as pd
from shiny import module, reactive, render, ui

try:
    from shiny_app.observation_compare import ModelObservationComparison, ObservationData, create_sample_observations
except ImportError:  # running as a script from inside shiny_app/
    from observation_compare import ModelObservationComparison, ObservationData, create_sample_observations

try:
    from shiny_app.obs_loader import get_file_preview, scan_observations_directory
    from shiny_app.obs_loader import load_observation_file as load_obs_file
except ImportError:  # running as a script from inside shiny_app/
    from obs_loader import get_file_preview, scan_observations_directory
    from obs_loader import load_observation_file as load_obs_file

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
OUTPUT_CSV = os.path.join(ROOT, "OUTPUT.csv")


def _compare_blocking(output_csv, obs):
    """Build the model-vs-observation comparison + metrics (TODO 2.4).

    Runs in a worker thread (via ``asyncio.to_thread``) so the full OUTPUT.csv
    read and the metric computation never block the Shiny event loop. Returns
    ``(comparison, metrics)``, or ``(None, None)`` if OUTPUT.csv is absent.
    """
    if not os.path.exists(output_csv):
        return None, None
    comparison = ModelObservationComparison(output_csv, obs)
    comparison.load_model_data()
    metrics = comparison.calculate_all_metrics()
    return comparison, metrics


def _sample_and_compare_blocking(output_csv):
    """Generate sample observations from OUTPUT.csv and build the comparison (TODO 2.4).

    Runs in a worker thread; the two full OUTPUT.csv reads (sampling + model load)
    and the metric computation stay off the event loop. Returns a result dict:
    ``{"ok": True, "obs", "comparison", "metrics", "n"}`` on success, else
    ``{"ok": False, "reason": "empty"|"load", "msg"?}``.
    """
    sample_df = create_sample_observations(output_csv, noise_level=0.1, sample_fraction=0.1)
    if len(sample_df) == 0:
        return {"ok": False, "reason": "empty"}
    obs = ObservationData()
    success, msg = obs.load_from_dataframe(sample_df)
    if not success:
        return {"ok": False, "reason": "load", "msg": msg}
    comparison = ModelObservationComparison(output_csv, obs)
    comparison.load_model_data()
    metrics = comparison.calculate_all_metrics()
    return {"ok": True, "obs": obs, "comparison": comparison, "metrics": metrics, "n": len(sample_df)}


@module.ui
def observations_ui():
    return ui.card(
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


@module.server
def observations_server(input, output, session, state):
    # `state` is accepted for the uniform x_server(id, state) convention; the
    # observations tab is self-contained and uses nothing from it.
    obs_data_obj = reactive.Value(None)
    obs_comparison_obj = reactive.Value(None)
    obs_metrics_results = reactive.Value(None)
    obs_files_list = reactive.Value([])  # List of ObservationFile objects
    obs_loaded_file = reactive.Value(None)  # Currently loaded observation file
    obs_file_preview = reactive.Value(None)  # Preview data for selected file

    # --- Background tasks for the heavy OUTPUT.csv comparison (TODO 2.4) ---
    @reactive.extended_task
    async def compare_task(output_csv, obs):
        """Off-loop model-vs-observation comparison for the upload path."""
        return await asyncio.to_thread(_compare_blocking, output_csv, obs)

    @reactive.extended_task
    async def sample_task(output_csv):
        """Off-loop sample-generation + comparison for the sample-data path."""
        return await asyncio.to_thread(_sample_and_compare_blocking, output_csv)

    @reactive.effect
    def collect_comparison():
        """Publish the upload-path comparison once its background task finishes."""
        status = compare_task.status()
        if status == "error":
            logger.error("Observation comparison failed in background task")
            ui.notification_show("Failed to compare with model output", type="error")
            return
        if status != "success":
            return
        comparison, metrics = compare_task.result()
        if comparison is None:
            return  # OUTPUT.csv absent: observations still loaded, just no comparison
        obs_comparison_obj.set(comparison)
        obs_metrics_results.set(metrics)
        logger.info(f"Calculated metrics for {len(metrics)} variables")

    @reactive.effect
    def collect_sample():
        """Publish the sample-path result once its background task finishes."""
        status = sample_task.status()
        if status == "error":
            logger.error("Sample observation generation failed in background task")
            ui.notification_show("Failed to generate sample data", type="error")
            return
        if status != "success":
            return
        res = sample_task.result()
        if not res["ok"]:
            if res["reason"] == "empty":
                ui.notification_show("Failed to generate sample data", type="error")
            else:
                ui.notification_show(f"Error: {res.get('msg', '')}", type="error")
            return
        obs = res["obs"]
        obs_data_obj.set(obs)
        ui.update_select("obs_variable", choices=obs.variables,
                       selected=obs.variables[0] if obs.variables else None)
        obs_comparison_obj.set(res["comparison"])
        obs_metrics_results.set(res["metrics"])
        ui.notification_show(
            f"Generated {res['n']} sample observations with {len(obs.variables)} variables",
            type="message"
        )

    @reactive.effect
    @reactive.event(input.obs_scan_dir)
    def scan_observations_dir():
        """Scan OBSERVATIONS directory for observation files"""
        obs_dir = os.path.join(ROOT, "OBSERVATIONS")
        logger.info(f"Scanning observations directory: {obs_dir}")

        if not os.path.isdir(obs_dir):
            ui.notification_show(f"OBSERVATIONS directory not found: {obs_dir}", type="warning")
            return

        files = scan_observations_directory(obs_dir)
        obs_files_list.set(files)

        # Update file selector
        if files:
            choices = {f.filepath: f"{f.filename} ({f.file_type})" for f in files}
            ui.update_select("obs_file_select", choices=choices,
                            selected=files[0].filepath if files else None)
            ui.notification_show(f"Found {len(files)} observation files", type="message")
        else:
            ui.notification_show("No observation files found", type="warning")

    @reactive.effect
    @reactive.event(input.obs_file_select)
    def preview_selected_file():
        """Preview selected observation file"""
        selected = input.obs_file_select()
        if not selected:
            obs_file_preview.set(None)
            return

        # Get preview
        preview = get_file_preview(selected)
        obs_file_preview.set(preview)

    @reactive.effect
    @reactive.event(input.obs_load_file)
    def load_selected_obs_file():
        """Load the selected observation file"""
        selected = input.obs_file_select()
        if not selected or not os.path.exists(selected):
            ui.notification_show("No file selected", type="warning")
            return

        logger.info(f"Loading observation file: {selected}")

        # Load based on file type
        loaded = load_obs_file(selected)

        if loaded:
            obs_loaded_file.set(loaded)

            # Update variable selector with available variables
            available = loaded.get_available_variables()
            if available:
                choices = {str(idx): f"{idx}: {name} ({count} pts)"
                          for idx, name, count in available}
                ui.update_select("obs_variable", choices=choices,
                               selected=str(available[0][0]) if available else None)

            ui.notification_show(
                f"Loaded {loaded.file_info.filename}: {len(available)} variables with data",
                type="message"
            )
        else:
            ui.notification_show(f"Could not load file: {os.path.basename(selected)}", type="error")

    @render.ui
    def obs_file_info():
        """Render file information"""
        preview = obs_file_preview.get()

        if preview is None:
            return ui.tags.div(
                ui.tags.p("Click 'Scan OBSERVATIONS Directory' to discover files",
                         class_="text-muted text-center"),
                class_="p-3"
            )

        info = preview.get("info", {})

        if "error" in info:
            return ui.tags.div(
                ui.tags.p(f"Error: {info['error']}", class_="text-danger"),
            )

        # File info summary
        return ui.tags.div(
            ui.tags.div(
                ui.tags.div(
                    ui.tags.small("Records", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{info.get('n_records', 'N/A')}"),
                    class_="col-4 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Variables", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{info.get('n_variables', 'N/A')}"),
                    class_="col-4 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Date Range", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(
                        f"{info.get('date_range', ('N/A', 'N/A'))[0] or 'N/A'} to {info.get('date_range', ('N/A', 'N/A'))[1] or 'N/A'}"
                        if isinstance(info.get('date_range'), tuple) else "N/A"
                    ),
                    class_="col-4 text-center"
                ),
                class_="row mb-3"
            ),
            class_="p-2"
        )

    @render.ui
    def obs_variables_table():
        """Render table of available variables in selected file"""
        preview = obs_file_preview.get()

        if preview is None or "info" not in preview:
            return ui.tags.div()

        info = preview.get("info", {})
        variables = info.get("variables_with_data", [])

        if not variables:
            # Show data preview table instead
            data = preview.get("data", [])
            if data:
                df = pd.DataFrame(data)
                return ui.tags.div(
                    ui.tags.h6("Data Preview:", class_="mb-2"),
                    ui.tags.div(
                        ui.HTML(df.head(5).to_html(classes="table table-sm table-striped",
                                                   index=False, border=0)),
                        style="overflow-x: auto; font-size: 11px;"
                    )
                )
            return ui.tags.p("No data available", class_="text-muted")

        # Create variable summary table
        rows = []
        for idx, name, count in variables[:20]:  # Limit to 20 variables
            rows.append({
                "Index": idx,
                "Variable": name.split(" - ")[0] if " - " in name else name,
                "Description": name.split(" - ")[1].split(" (")[0] if " - " in name else "",
                "N": count
            })

        if len(variables) > 20:
            rows.append({
                "Index": "...",
                "Variable": f"(+{len(variables)-20} more)",
                "Description": "",
                "N": ""
            })

        df = pd.DataFrame(rows)

        return ui.tags.div(
            ui.tags.h6("Available Measurements:", class_="mb-2"),
            ui.tags.div(
                ui.HTML(df.to_html(classes="table table-sm table-striped table-hover",
                                   index=False, border=0)),
                style="max-height: 300px; overflow-y: auto; font-size: 11px;"
            )
        )

    @reactive.effect
    @reactive.event(input.obs_file)
    def load_observation_file():
        """Load uploaded observation file, then compare in the background."""
        file_info = input.obs_file()
        if file_info is None or len(file_info) == 0:
            return

        file_path = file_info[0]["datapath"]
        logger.info(f"Loading observation file: {file_info[0]['name']}")

        # The uploaded file is small (user field data); loading it stays on the
        # event loop. Only the OUTPUT.csv comparison below is offloaded (TODO 2.4).
        obs = ObservationData()
        success, msg = obs.load_csv(file_path)

        if not success:
            logger.error(f"Failed to load observations: {msg}")
            ui.notification_show(f"Error: {msg}", type="error")
            return

        obs_data_obj.set(obs)
        logger.info(f"Loaded observations: {len(obs.variables)} variables")
        ui.update_select("obs_variable", choices=obs.variables,
                       selected=obs.variables[0] if obs.variables else None)
        ui.notification_show(f"Loaded {len(obs.variables)} observation variables", type="message")

        # Offload the heavy model comparison (full OUTPUT.csv read + metrics).
        compare_task(OUTPUT_CSV, obs)

    @reactive.effect
    @reactive.event(input.generate_sample_obs)
    def generate_sample_observations():
        """Launch background sample-data generation + comparison."""
        if not os.path.exists(OUTPUT_CSV):
            ui.notification_show("OUTPUT.csv not found. Run the model first.", type="warning")
            return

        logger.info("Generating sample observations...")
        # Sampling reads OUTPUT.csv and the comparison reads it again; both plus
        # metric computation run off the event loop (TODO 2.4).
        sample_task(OUTPUT_CSV)

    @render.table
    def obs_comparison_summary():
        """Render comparison summary table"""
        if compare_task.status() == "running" or sample_task.status() == "running":
            return pd.DataFrame({
                "Message": ["Comparing with model output… (running in background)"]
            })

        comparison = obs_comparison_obj.get()

        if comparison is None:
            return pd.DataFrame({
                "Message": ["Upload observations or generate sample data to compare with model"]
            })

        return comparison.get_summary_table()

    @render.ui
    def obs_metrics_detail():
        """Render detailed metrics for selected variable"""
        metrics = obs_metrics_results.get()
        variable = input.obs_variable()

        if metrics is None or variable is None or variable not in metrics:
            return ui.tags.p("Select a variable to see detailed metrics", class_="text-muted")

        m = metrics[variable]

        # Create metrics display
        return ui.tags.div(
            ui.tags.div(
                ui.tags.div(
                    ui.tags.small("N Points", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{m.n_points}"),
                    class_="col-4 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("R²", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{m.r_squared:.3f}"),
                    class_="col-4 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Rating", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(
                        m.get_rating(),
                        class_="text-success" if m.get_rating() == "Excellent"
                              else "text-info" if m.get_rating() == "Good"
                              else "text-warning" if m.get_rating() == "Fair"
                              else "text-danger"
                    ),
                    class_="col-4 text-center"
                ),
                class_="row mb-3"
            ),
            ui.tags.table(
                ui.tags.tbody(
                    ui.tags.tr(
                        ui.tags.td("Observed Mean", class_="small"),
                        ui.tags.td(f"{m.obs_mean:.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("Modeled Mean", class_="small"),
                        ui.tags.td(f"{m.model_mean:.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("Bias", class_="small"),
                        ui.tags.td(f"{m.bias:+.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("MAE", class_="small"),
                        ui.tags.td(f"{m.mae:.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("RMSE", class_="small"),
                        ui.tags.td(f"{m.rmse:.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("NRMSE (%)", class_="small"),
                        ui.tags.td(f"{m.nrmse:.1f}%", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("Correlation", class_="small"),
                        ui.tags.td(f"{m.correlation:.3f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("Skill Score", class_="small"),
                        ui.tags.td(f"{m.skill_score:.3f}", class_="text-end")
                    ),
                ),
                class_="table table-sm table-borderless"
            ),
            class_="p-2 border rounded"
        )

    @render.ui
    def obs_scatter_info():
        """Render scatter plot information"""
        comparison = obs_comparison_obj.get()
        variable = input.obs_variable()

        if comparison is None or variable is None:
            return ui.tags.p("Load observations to see scatter plot data", class_="text-muted")

        # Get comparison data
        comp_data = comparison.get_comparison_data(variable)

        if comp_data is None or len(comp_data) == 0:
            return ui.tags.p("No matching data for this variable", class_="text-warning")

        # Show scatter plot statistics
        obs_vals = comp_data['Observed']
        mod_vals = comp_data['Modeled']

        return ui.tags.div(
            ui.tags.small(f"Data points: {len(comp_data)}", class_="text-muted d-block"),
            ui.tags.small(f"Observed range: {obs_vals.min():.4f} - {obs_vals.max():.4f}", class_="text-muted d-block"),
            ui.tags.small(f"Modeled range: {mod_vals.min():.4f} - {mod_vals.max():.4f}", class_="text-muted d-block"),
            ui.tags.hr(),
            ui.tags.small(
                "Tip: Use Plot & Visualization to compare time series",
                class_="text-info"
            ),
            class_="p-2"
        )
