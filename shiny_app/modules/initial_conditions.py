"""Initial Conditions tab as a Shiny module (Phase 2, Task 3).

`initial_conditions_ui(id)` returns the panel *content* (the app-level
panel_conditional stays in create_ui); `initial_conditions_server(id, state)`
registers the handlers, ported verbatim from app.py. Self-contained: imports
ic_parser and self-computes INPUTS_DIR; imports nothing from app.py.
"""
import logging
import os
from datetime import datetime

from shiny import module, reactive, render, ui

try:
    from shiny_app.ic_parser import STATE_VARIABLE_CATEGORIES, ICFile
except ImportError:  # running as a script from inside shiny_app/
    from ic_parser import STATE_VARIABLE_CATEGORIES, ICFile

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")


@module.ui
def initial_conditions_ui():
    return ui.card(
        ui.card_header("Initial Conditions"),
        ui.layout_columns(
            ui.tooltip(
                ui.input_select(
                    "ic_file",
                    "IC File:",
                    choices=["INIT_CONC_1.txt", "INIT_CONC_2.txt"],
                    selected="INIT_CONC_1.txt"
                ),
                "Initial concentration file for state variables"
            ),
            ui.tooltip(
                ui.input_select(
                    "ic_category",
                    "Category:",
                    choices=list(STATE_VARIABLE_CATEGORIES.keys()),
                    selected="Nutrients"
                ),
                "Select variable category: Nutrients, Phytoplankton, Oxygen, etc."
            ),
            ui.tooltip(
                ui.input_action_button("load_ics", "Load", class_="btn-secondary mt-4"),
                "Load initial conditions from selected file"
            ),
            col_widths=[5, 5, 2]
        ),
        ui.tags.hr(),
        ui.layout_columns(
            ui.card(
                ui.card_header("Category Info"),
                ui.output_text("ic_category_info"),
                fill=False
            ),
            ui.card(
                ui.card_header("State Variables"),
                ui.output_ui("ic_table"),
                style="max-height: 500px; overflow-y: auto;"
            ),
            col_widths=[4, 8]
        ),
        ui.layout_columns(
            ui.tooltip(
                ui.input_action_button("save_ics", "Save All Changes", class_="btn-success"),
                "Save modified initial conditions to file (creates backup)"
            ),
            ui.output_text("ic_save_status"),
            col_widths=[3, 9]
        )
    )


@module.server
def initial_conditions_server(input, output, session, state):
    # `state` is accepted for the uniform x_server(id, state) convention; the
    # initial_conditions tab is self-contained and uses nothing from it.
    ic_file_obj = reactive.Value(None)
    ic_save_msg = reactive.Value("")

    @reactive.effect
    @reactive.event(input.load_ics, input.ic_category, input.ic_file)
    def load_ic_file():
        """Load IC file when category or file changes"""
        ic_filename = input.ic_file()
        if not ic_filename:
            return

        filepath = os.path.join(INPUTS_DIR, ic_filename)
        if not os.path.exists(filepath):
            logger.error(f"IC file not found: {filepath}")
            return

        logger.info(f"Loading IC file: {ic_filename}")
        ic = ICFile(filepath)
        if ic.parse():
            ic_file_obj.set(ic)
            ic_save_msg.set("")
            logger.info(f"Loaded {len(ic.conditions)} initial conditions")
        else:
            logger.error("Failed to parse IC file")

    @render.text
    def ic_category_info():
        """Display category information"""
        category = input.ic_category()
        ic = ic_file_obj.get()

        if not category:
            return "Select a category"

        if category in STATE_VARIABLE_CATEGORIES:
            var_ids = STATE_VARIABLE_CATEGORIES[category]
            count = len(var_ids)

            info = f"Category: {category}\n"
            info += f"Variables: {count}\n"

            if ic:
                conditions = ic.get_conditions_by_category(category)
                info += f"Loaded: {len(conditions)} variables"

            return info
        return "Unknown category"

    @render.ui
    def ic_table():
        """Render IC table for editing"""
        category = input.ic_category()
        ic = ic_file_obj.get()

        if not ic:
            return ui.tags.div(
                ui.tags.p("Click 'Load Initial Conditions' to load the IC file", class_="text-muted"),
                class_="mt-2"
            )

        conditions = ic.get_conditions_by_category(category)

        if not conditions:
            return ui.tags.p(f"No variables found for category: {category}", class_="text-warning")

        # Create input fields for each IC
        ic_inputs = []
        for cond in conditions:
            ic_row = ui.tags.div(
                ui.tags.div(
                    ui.tags.strong(cond.name, class_="small"),
                    ui.tags.br(),
                    ui.tags.small(f"{cond.description} ({cond.units})", class_="text-muted"),
                    class_="col-7"
                ),
                ui.tags.div(
                    ui.input_numeric(
                        f"ic_{cond.var_id}",
                        "",
                        value=cond.value,
                        width="100%"
                    ),
                    class_="col-5"
                ),
                class_="row mb-2 align-items-center border-bottom pb-2"
            )
            ic_inputs.append(ic_row)

        return ui.tags.div(
            ui.tags.div(
                ui.tags.small(f"Showing {len(conditions)} state variables", class_="text-muted"),
                class_="mb-2"
            ),
            *ic_inputs,
            style="max-height: 400px; overflow-y: auto;"
        )

    @reactive.effect
    @reactive.event(input.save_ics)
    def save_initial_conditions():
        """Save modified initial conditions"""
        ic = ic_file_obj.get()
        if not ic:
            ic_save_msg.set("Error: No IC file loaded")
            return

        # Collect all modified values
        category = input.ic_category()
        conditions = ic.get_conditions_by_category(category)

        updates = {}
        for cond in conditions:
            input_id = f"ic_{cond.var_id}"
            try:
                new_value = input[input_id]()
                if new_value is not None and new_value != cond.value:
                    updates[cond.var_id] = float(new_value)
            except Exception as e:
                logger.debug(f"Could not get value for {input_id}: {e}")

        if not updates:
            ic_save_msg.set("No changes to save")
            return

        logger.info(f"Saving {len(updates)} IC changes")

        # Apply updates
        success_count, fail_count, messages = ic.update_conditions(updates)

        # Save to file
        save_ok, save_msg = ic.save(backup=True)

        if save_ok:
            ic_save_msg.set(f"Saved {success_count} changes at {datetime.now().strftime('%H:%M:%S')}")
            ui.notification_show(
                f"Successfully saved {success_count} IC changes",
                type="message",
                duration=3
            )
        else:
            ic_save_msg.set(f"Save failed: {save_msg}")
            ui.notification_show(
                f"Failed to save ICs: {save_msg}",
                type="error",
                duration=5
            )

    @render.text
    def ic_save_status():
        """Display IC save status"""
        return ic_save_msg.get()
