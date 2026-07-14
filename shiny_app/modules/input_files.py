"""Input Files tab as a Shiny module (Phase 2, Task 4 — largest leaf).

`input_files_ui(id)` returns the panel *content* (the app-level
panel_conditional stays in create_ui); `input_files_server(id, state)`
registers the handlers, ported verbatim from app.py, including the
anonymous file-list-populating effect. Self-contained: imports
input_analysis (INPUT_FILE_CATEGORIES/analyze_input_file/
get_input_file_categories), box_network (map display), and safe_resolve;
self-computes ROOT/INPUTS_DIR; imports nothing from app.py.
"""
import logging
import os
import shutil
from datetime import datetime

import plotly.graph_objects as go
from shiny import module, reactive, render, ui
from shinywidgets import output_widget, render_widget

try:
    from shiny_app import box_network
    from shiny_app.input_analysis import (
        INPUT_FILE_CATEGORIES,
        analyze_input_file,
        get_input_file_categories,
    )
    from shiny_app.safe_resolve import safe_resolve
except ImportError:  # running as a script from inside shiny_app/
    import box_network
    from input_analysis import INPUT_FILE_CATEGORIES, analyze_input_file, get_input_file_categories
    from safe_resolve import safe_resolve

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")


@module.ui
def input_files_ui():
    return ui.layout_columns(
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


@module.server
def input_files_server(input, output, session, state):
    # `state` is accepted for the uniform x_server(id, state) convention; the
    # input_files tab is self-contained and uses nothing from it.

    # Reactive value to track file list refresh
    file_list_version = reactive.Value(0)

    # populate file list on start and when refresh button is clicked or category changes
    @reactive.Effect
    def _():
        # Depend on file_list_version and category filter to trigger refresh
        file_list_version.get()
        category_filter = input.file_category_filter()

        try:
            all_files = sorted([f for f in os.listdir(INPUTS_DIR) if os.path.isfile(os.path.join(INPUTS_DIR, f))])

            # Apply category filter
            if category_filter and category_filter != "All Categories":
                filtered_files = []
                for f in all_files:
                    if f in INPUT_FILE_CATEGORIES:
                        if INPUT_FILE_CATEGORIES[f].get("category") == category_filter:
                            filtered_files.append(f)
                    else:
                        # For uncatalogued files, use pattern matching
                        info = analyze_input_file(os.path.join(INPUTS_DIR, f))
                        if info.get("category") == category_filter:
                            filtered_files.append(f)
                files = filtered_files
            else:
                files = all_files

            logger.info(f"Populating file list with {len(files)} files (category: {category_filter})")
            ui.update_select("file_select", choices=files)
        except Exception as e:
            logger.error(f"Error populating file list: {e}")

    # Refresh file list button
    @reactive.effect
    @reactive.event(input.refresh_files)
    def refresh_files():
        logger.info("User clicked 'Refresh file list' button")
        file_list_version.set(file_list_version.get() + 1)

    # load selected file contents
    @reactive.effect
    @reactive.event(input.file_select)
    def load_file():
        f = input.file_select()
        if not f:
            logger.debug("load_file called but no file selected")
            return
        logger.info(f"Loading file: {f}")

        try:
            path = safe_resolve(INPUTS_DIR, f)
            with open(path) as fh:
                txt = fh.read()
            logger.info(f"Successfully loaded {f} ({len(txt)} characters)")
            ui.update_text_area("file_contents", value=txt)
        except Exception as e:
            logger.error(f"Error reading file {f}: {e}")
            ui.update_text_area("file_contents", value=f"Error reading file: {e}")

    # Render file header text
    @render.text
    def file_header_text():
        f = input.file_select()
        if not f:
            return "File Contents"
        return f"File Contents: {f}"

    # Render file info panel - directly depends on file_select for reactivity
    @render.ui
    def file_info_panel():
        f = input.file_select()
        if not f:
            return ui.tags.div(
                ui.tags.p("Select a file to view its information", class_="text-muted"),
                class_="p-2"
            )

        # Analyze the file
        try:
            path = safe_resolve(INPUTS_DIR, f)
        except ValueError as e:
            return ui.tags.div(
                ui.tags.p(f"Invalid file: {e}", class_="text-danger"),
                class_="p-2"
            )
        analysis = analyze_input_file(path)

        # Build info rows
        rows = []

        # Category badge
        category = analysis.get("category", "Unknown")
        category_colors = {
            "Forcing Timeseries": "primary",
            "Meteorological Timeseries": "info",
            "Boundary Forcing Timeseries": "success",
            "Sediment Forcing Timeseries": "warning",
            "Settling Velocity Timeseries": "secondary",
            "Box Geometry": "dark",
            "Model Constants": "danger",
            "Model Configuration": "primary",
            "Initial Conditions": "info",
            "Transport Configuration": "success",
            "Sediment Fluxes": "warning",
            "Sediment Parameters": "secondary",
            "Process Parameters": "dark",
            "Boundary Conditions": "success",
        }
        badge_color = category_colors.get(category, "secondary")
        rows.append(
            ui.tags.div(
                ui.tags.span(category, class_=f"badge bg-{badge_color}"),
                class_="mb-2"
            )
        )

        # Description
        desc = analysis.get("description", "")
        if desc:
            rows.append(ui.tags.p(ui.tags.strong("Description: "), desc))

        # Structure
        structure = analysis.get("structure", "")
        if structure:
            rows.append(ui.tags.p(ui.tags.strong("Structure: "), structure))

        # Model use
        model_use = analysis.get("model_use", "")
        if model_use:
            rows.append(ui.tags.p(ui.tags.strong("Model Use: "), model_use))

        # File statistics
        stats = []
        if analysis.get("num_lines"):
            stats.append(f"Lines: {analysis['num_lines']:,}")
        if analysis.get("num_variables"):
            stats.append(f"Variables: {analysis['num_variables']}")
        if analysis.get("data_size"):
            stats.append(f"Data rows: {analysis['data_size']:,}")

        if stats:
            rows.append(ui.tags.p(ui.tags.strong("Statistics: "), " | ".join(stats)))

        # Timespan (for timeseries files)
        if analysis.get("is_timeseries") and analysis.get("time_start") is not None:
            timespan_info = []
            if analysis.get("date_start") and analysis.get("date_end"):
                timespan_info.append(f"Date range: {analysis['date_start']} to {analysis['date_end']}")
            if analysis.get("time_start") is not None and analysis.get("time_end") is not None:
                duration = analysis['time_end'] - analysis['time_start']
                years = duration / 365.25
                timespan_info.append(f"Duration: {duration:.1f} days ({years:.1f} years)")
                timespan_info.append(f"Julian days: {analysis['time_start']:.1f} - {analysis['time_end']:.1f}")

            if timespan_info:
                rows.append(
                    ui.tags.div(
                        ui.tags.strong("Timespan:"),
                        ui.tags.ul(
                            *[ui.tags.li(info) for info in timespan_info],
                            class_="mb-0 ps-3"
                        ),
                        class_="mb-2"
                    )
                )

        # Error if any
        if analysis.get("error"):
            rows.append(
                ui.tags.div(
                    ui.tags.span("Error: ", class_="text-danger"),
                    analysis["error"],
                    class_="text-danger"
                )
            )

        return ui.tags.div(*rows, class_="p-2")

    # Reactive value for save status feedback
    save_status_msg = reactive.Value("")

    # save file
    @reactive.event(input.save_file)
    def save_file():
        f = input.file_select()
        logger.info("User clicked 'Save file' button")
        if not f:
            logger.warning("Save attempted but no file selected")
            save_status_msg.set("Error: No file selected")
            return

        logger.info(f"Saving file: {f}")
        try:
            path = safe_resolve(INPUTS_DIR, f)
        except ValueError as e:
            save_status_msg.set(f"Error: {e}")
            return
        content_length = len(input.file_contents())
        logger.debug(f"Content length: {content_length} characters")

        # Create timestamped backup
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        bak = f"{path}.{timestamp}.bak"
        try:
            if os.path.exists(path):
                original_size = os.path.getsize(path)
                shutil.copy(path, bak)
                logger.info(f"Backup created: {os.path.basename(bak)} (original size: {original_size} bytes)")

            with open(path, 'w') as fh:
                fh.write(input.file_contents())

            new_size = os.path.getsize(path)
            save_status_msg.set(f"✓ Saved successfully at {datetime.now().strftime('%H:%M:%S')}")
            logger.info(f"File saved successfully: {f} (new size: {new_size} bytes)")
        except Exception as e:
            error_msg = f"✗ Save failed: {e}"
            save_status_msg.set(error_msg)
            logger.error(f"Error saving file {f}: {e}", exc_info=True)

    @render.text
    def save_status():
        return save_status_msg.get()

    # ========== MAP DISPLAY TAB (Input Files) ==========

    @render_widget
    def map_display_plot():
        """Render the Map Display plotly figure based on selected view."""
        view = input.map_display_view()
        box_no = int(input.map_bathymetry_box())

        boxes = box_network.parse_pelagic_inputs(INPUTS_DIR)
        if view == "Box Network":
            links = box_network.parse_advective_links(INPUTS_DIR)
            fig = box_network.build_box_network_figure(boxes, links)
        elif view == "Bathymetry Profile":
            layers = box_network.parse_bathymetry(box_no, INPUTS_DIR)
            fig = box_network.build_bathymetry_figure(box_no, layers, boxes)
        elif view == "Box Depths Overview":
            fig = box_network.build_depths_overview(boxes)
        else:
            fig = go.Figure()
            fig.update_layout(height=700, template='plotly_dark')
        return go.FigureWidget(fig)

    @render.ui
    def map_display_info():
        """Contextual info for the current map view."""
        view = input.map_display_view()
        if view == "Box Network":
            return ui.tags.div(
                ui.tags.small(
                    ui.tags.strong("Mosaic box model: "),
                    "Touching boxes share an advective link (water exchange). "
                    "Dark gaps separate grid-neighbours with no connection. "
                    "Green dashed lines = non-adjacent links (8 of 42). "
                    "Border colour: blue = Sand, brown = Mud. "
                    "Box numbering is an ID, not geographic order "
                    "(e.g. Box 2 sits at the Nemunas inflow, not near Baltic). "
                    "Hover for details.",
                    class_="text-muted"
                ),
                class_="mt-2"
            )
        elif view == "Bathymetry Profile":
            box_no = int(input.map_bathymetry_box())
            boxes = box_network.parse_pelagic_inputs(INPUTS_DIR)
            info = boxes.get(box_no, {})
            return ui.tags.div(
                ui.tags.small(
                    ui.tags.strong(f"Box {box_no}: "),
                    f"Depth {info.get('depth', 0):.1f} m, "
                    f"{info.get('sediment', 'Unknown')} substrate. "
                    "Horizontal bars show layer area at each elevation.",
                    class_="text-muted"
                ),
                class_="mt-2"
            )
        elif view == "Box Depths Overview":
            return ui.tags.div(
                ui.tags.small(
                    ui.tags.strong("Overview: "),
                    "Bottom elevations for all 25 boxes. "
                    "Blue = Sand substrate, Brown = Mud. "
                    "Diamonds = surface water elevation.",
                    class_="text-muted"
                ),
                class_="mt-2"
            )
        return ui.tags.div()
