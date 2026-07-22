"""Parameters tab as a true Shiny module (Phase 1 pilot).

`parameters_ui(id)` returns the panel *content* (the app-level panel_conditional
stays in create_ui); `parameters_server(id, state)` registers the handlers. Both
ids namespace to `parameters-*`. Self-contained: imports parameter_parser and
self-computes INPUTS_DIR; imports nothing from app.py.
"""
import logging
import os
from datetime import datetime

from shiny import module, reactive, render, ui

try:
    from shiny_app.parameter_parser import PARAMETER_CATEGORIES, ParameterFile
except ImportError:  # running as a script from inside shiny_app/
    from parameter_parser import PARAMETER_CATEGORIES, ParameterFile

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")


@module.ui
def parameters_ui():
    return ui.card(
        ui.card_header("Parameters"),
        ui.output_ui("setup_notice"),
        ui.layout_columns(
            ui.tooltip(
                ui.input_select("param_file", "Constants file:",
                                choices=["WCONST_04.txt"], selected="WCONST_04.txt"),
                "WCONST_04.txt contains calibrated model parameters",
            ),
            ui.tooltip(
                ui.input_select("param_category", "Category:",
                                choices=list(PARAMETER_CATEGORIES.keys()), selected="Diatoms"),
                "Select parameter category: Diatoms, Cyanobacteria, Zooplankton, etc.",
            ),
            ui.tooltip(
                ui.input_action_button("load_params", "Load", class_="btn-secondary mt-4"),
                "Load parameters from selected file and category",
            ),
            col_widths=[3, 7, 2],
        ),
        ui.tags.hr(),
        ui.div(
            ui.output_text("param_category_info"),
            style="font-size: 0.78rem; padding: 0.4rem 0.75rem; background: rgba(14, 165, 233, 0.04); border-radius: 4px; margin-bottom: 0.75rem; border: 1px solid rgba(14, 165, 233, 0.1);",
        ),
        ui.card(
            ui.card_header("Parameters"),
            ui.output_ui("param_table"),
            style="max-height: 550px; overflow-y: auto;",
        ),
        ui.layout_columns(
            ui.tooltip(
                ui.input_action_button("save_params", "Save All Changes", class_="btn-success"),
                "Save modified parameters to file (creates backup)",
            ),
            ui.output_text("param_save_status"),
            col_widths=[3, 9],
        ),
    )


@module.server
def parameters_server(input, output, session, state):
    # This tab always reads/writes the Standard INPUTS/ dir (module-level
    # INPUTS_DIR), regardless of `state.run.current_setup()`; under a
    # non-standard setup (e.g. CL29) it is a reference-only view, flagged by
    # setup_notice() below (deferred: no CL29-specific viewer wired yet).
    param_file_obj = reactive.Value(None)
    param_save_msg = reactive.Value("")

    @render.ui
    def setup_notice():
        """Flag that this panel always shows the Standard-model INPUTS/, not the active setup's."""
        if state.run.current_setup().id != "standard":
            return ui.div(
                ui.tags.small(
                    "Showing Standard-model reference data; the CL29-specific view is not yet wired.",
                    class_="text-warning"
                ),
                class_="mb-2"
            )
        return ui.TagList()

    @reactive.effect
    @reactive.event(input.load_params, input.param_category, input.param_file)
    def load_param_file():
        """Load parameter file when category or file changes"""
        param_filename = input.param_file()
        if not param_filename:
            return
        filepath = os.path.join(INPUTS_DIR, param_filename)
        if not os.path.exists(filepath):
            logger.error(f"Parameter file not found: {filepath}")
            return
        logger.info(f"Loading parameter file: {param_filename}")
        pf = ParameterFile(filepath)
        if pf.parse():
            param_file_obj.set(pf)
            param_save_msg.set("")
            logger.info(f"Loaded {len(pf.parameters)} parameters")
        else:
            logger.error("Failed to parse parameter file")

    @render.text
    def param_category_info():
        """Display category information"""
        category = input.param_category()
        pf = param_file_obj.get()
        if not category:
            return "Select a category"
        if category in PARAMETER_CATEGORIES:
            start, end = PARAMETER_CATEGORIES[category]
            count = end - start + 1
            info = f"Category: {category}\n"
            info += f"Parameters: {count} ({start}-{end})\n"
            if pf:
                params = pf.get_parameters_by_category(category)
                info += f"Loaded: {len(params)} parameters"
            return info
        return "Unknown category"

    @render.ui
    def param_table():
        """Render parameter table for editing"""
        category = input.param_category()
        pf = param_file_obj.get()
        if not pf:
            return ui.tags.div(
                ui.tags.p("Click 'Load Parameters' to load the parameter file", class_="text-muted"),
                class_="mt-2",
            )
        params = pf.get_parameters_by_category(category)
        if not params:
            return ui.tags.p(f"No parameters found for category: {category}", class_="text-warning")
        param_inputs = []
        for p in params:
            param_row = ui.tags.div(
                ui.tags.div(
                    ui.tags.strong(p.name, class_="small"),
                    ui.tags.br(),
                    ui.tags.small(p.comment[:60] + "..." if len(p.comment) > 60 else p.comment, class_="text-muted"),
                    class_="col-7",
                ),
                ui.tags.div(
                    ui.input_numeric(f"param_{p.id}", "", value=p.value, width="100%"),
                    class_="col-5",
                ),
                class_="row mb-2 align-items-center border-bottom pb-2",
            )
            param_inputs.append(param_row)
        return ui.tags.div(
            ui.tags.div(
                ui.tags.small(f"Showing {len(params)} parameters", class_="text-muted"),
                class_="mb-2",
            ),
            *param_inputs,
            style="max-height: 400px; overflow-y: auto;",
        )

    @reactive.effect
    @reactive.event(input.save_params)
    def save_parameters():
        """Save modified parameters"""
        if state.run.current_setup().id != "standard":
            logger.info(
                "Parameter save blocked: active setup is '%s', not 'standard' "
                "(would overwrite Standard's INPUTS/)", state.run.current_setup().id
            )
            param_save_msg.set("Parameter editing is available for the Standard model only.")
            return
        pf = param_file_obj.get()
        if not pf:
            param_save_msg.set("Error: No parameter file loaded")
            return
        category = input.param_category()
        params = pf.get_parameters_by_category(category)
        updates = {}
        for p in params:
            input_id = f"param_{p.id}"
            try:
                new_value = input[input_id]()
                if new_value is not None and new_value != p.value:
                    updates[p.id] = float(new_value)
            except Exception as e:
                logger.debug(f"Could not get value for {input_id}: {e}")
        if not updates:
            param_save_msg.set("No changes to save")
            return
        logger.info(f"Saving {len(updates)} parameter changes")
        success_count, fail_count, messages = pf.update_parameters(updates)
        save_ok, save_msg = pf.save(backup=True)
        if save_ok:
            param_save_msg.set(f"Saved {success_count} changes at {datetime.now().strftime('%H:%M:%S')}")
            ui.notification_show(f"Successfully saved {success_count} parameter changes", type="message", duration=3)
        else:
            param_save_msg.set(f"Save failed: {save_msg}")
            ui.notification_show(f"Failed to save parameters: {save_msg}", type="error", duration=5)

    @render.text
    def param_save_status():
        """Display save status"""
        return param_save_msg.get()
