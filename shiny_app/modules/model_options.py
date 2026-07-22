"""Model Options tab as a Shiny module (Phase 2, Task 2).

`model_options_ui(id)` returns the panel *content* (the app-level
panel_conditional stays in create_ui); `model_options_server(id, state)`
registers the handlers, ported verbatim from app.py. Self-contained: imports
options_parser and self-computes INPUTS_DIR; imports nothing from app.py.
"""
import logging
import os
from datetime import datetime

from shiny import module, reactive, render, ui

try:
    from shiny_app.options_parser import OPTION_CATEGORIES, ExtraConstantsFile, ModelOptionsFile
except ImportError:  # running as a script from inside shiny_app/
    from options_parser import OPTION_CATEGORIES, ExtraConstantsFile, ModelOptionsFile

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")


@module.ui
def model_options_ui():
    return ui.card(
        ui.card_header("Model Options"),
        ui.output_ui("setup_notice"),
        ui.layout_columns(
            ui.tooltip(
                ui.input_select(
                    "options_category",
                    "Category:",
                    choices=list(OPTION_CATEGORIES.keys()),
                    selected="Cyanobacteria"
                ),
                "Select option category: Cyanobacteria, Zooplankton, Oxygen, etc."
            ),
            ui.tooltip(
                ui.input_action_button("load_options", "Load Options", class_="btn-secondary mt-4"),
                "Load model options and switches"
            ),
            col_widths=[10, 2]
        ),
        ui.tags.hr(),
        ui.layout_columns(
            ui.card(
                ui.card_header("Model Switches"),
                ui.output_ui("options_switches"),
                style="max-height: 400px; overflow-y: auto;"
            ),
            ui.card(
                ui.card_header("Extra Constants"),
                ui.output_ui("options_constants"),
                style="max-height: 400px; overflow-y: auto;"
            ),
            col_widths=[6, 6]
        ),
        ui.layout_columns(
            ui.tooltip(
                ui.input_action_button("save_options", "Save All Changes", class_="btn-success"),
                "Save model switches and extra constants (creates backup)"
            ),
            ui.output_text("options_save_status"),
            col_widths=[3, 9]
        )
    )


@module.server
def model_options_server(input, output, session, state):
    # This tab always reads/writes the Standard INPUTS/ dir (module-level
    # INPUTS_DIR), regardless of `state.run.current_setup()`; under a
    # non-standard setup (e.g. CL29) it is a reference-only view, flagged by
    # setup_notice() below (deferred: no CL29-specific viewer wired yet).
    options_file_obj = reactive.Value(None)
    extra_const_file_obj = reactive.Value(None)
    options_save_msg = reactive.Value("")

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
    @reactive.event(input.load_options, input.options_category)
    def load_options_files():
        """Load model options files"""
        # Load PELAGIC_MODEL_OPTIONS.txt
        options_path = os.path.join(INPUTS_DIR, "PELAGIC_MODEL_OPTIONS.txt")
        if os.path.exists(options_path):
            logger.info(f"Loading model options: {options_path}")
            mof = ModelOptionsFile(options_path)
            if mof.parse():
                options_file_obj.set(mof)
                logger.info(f"Loaded {len(mof.options)} model options")

        # Load EXTRA_WCONST.txt
        extra_path = os.path.join(INPUTS_DIR, "EXTRA_WCONST.txt")
        if os.path.exists(extra_path):
            logger.info(f"Loading extra constants: {extra_path}")
            ecf = ExtraConstantsFile(extra_path)
            if ecf.parse():
                extra_const_file_obj.set(ecf)
                logger.info(f"Loaded {len(ecf.constants)} extra constants")

        options_save_msg.set("")

    @render.ui
    def options_switches():
        """Render model option switches"""
        category = input.options_category()
        mof = options_file_obj.get()

        if not mof:
            return ui.tags.div(
                ui.tags.p("Click 'Load Options' to load model options", class_="text-muted"),
                class_="mt-2"
            )

        # Get options for this category
        category_option_names = OPTION_CATEGORIES.get(category, [])

        # Filter to boolean options only for switches
        switch_inputs = []
        for opt_name in category_option_names:
            opt = mof.get_option(opt_name)
            if opt and opt.is_boolean():
                switch_row = ui.tags.div(
                    ui.tags.div(
                        ui.input_switch(
                            f"opt_{opt_name}",
                            opt.description,
                            value=bool(opt.value)
                        ),
                        ui.tags.small(opt.help_text, class_="text-muted d-block"),
                        class_="mb-2"
                    ),
                    class_="border-bottom pb-2 mb-2"
                )
                switch_inputs.append(switch_row)
            elif opt and opt.option_type == "string":
                # String option (like filename)
                string_row = ui.tags.div(
                    ui.tags.div(
                        ui.tags.strong(opt.description, class_="small"),
                        ui.input_text(
                            f"opt_{opt_name}",
                            "",
                            value=str(opt.value),
                            width="100%"
                        ),
                        ui.tags.small(opt.help_text, class_="text-muted"),
                        class_="mb-2"
                    ),
                    class_="border-bottom pb-2 mb-2"
                )
                switch_inputs.append(string_row)

        if not switch_inputs:
            return ui.tags.p("No switches in this category", class_="text-muted")

        return ui.tags.div(
            ui.tags.small(f"Showing {len(switch_inputs)} options", class_="text-muted mb-2 d-block"),
            *switch_inputs
        )

    @render.ui
    def options_constants():
        """Render extra constants for editing"""
        category = input.options_category()
        ecf = extra_const_file_obj.get()

        if not ecf:
            return ui.tags.div()

        # Get constants for this category
        category_option_names = OPTION_CATEGORIES.get(category, [])

        # Filter to numeric constants
        const_inputs = []
        for const_name in category_option_names:
            const = ecf.get_constant(const_name)
            if const and const.option_type == "float":
                const_row = ui.tags.div(
                    ui.tags.div(
                        ui.tags.strong(const.description, class_="small"),
                        ui.tags.br(),
                        ui.tags.small(const.help_text, class_="text-muted"),
                        class_="col-7"
                    ),
                    ui.tags.div(
                        ui.input_numeric(
                            f"const_{const_name}",
                            "",
                            value=const.value,
                            width="100%"
                        ),
                        class_="col-5"
                    ),
                    class_="row mb-2 align-items-center border-bottom pb-2"
                )
                const_inputs.append(const_row)

        if not const_inputs:
            return ui.tags.div()

        return ui.tags.div(
            ui.tags.small(f"Showing {len(const_inputs)} constants", class_="text-muted mb-2 d-block"),
            *const_inputs,
            style="max-height: 300px; overflow-y: auto;"
        )

    @reactive.effect
    @reactive.event(input.save_options)
    def save_model_options():
        """Save modified model options"""
        mof = options_file_obj.get()
        ecf = extra_const_file_obj.get()
        category = input.options_category()

        if not mof and not ecf:
            options_save_msg.set("Error: No options loaded")
            return

        total_updates = 0
        errors = []

        # Save model options switches
        if mof:
            category_option_names = OPTION_CATEGORIES.get(category, [])
            for opt_name in category_option_names:
                opt = mof.get_option(opt_name)
                if opt and opt.is_boolean():
                    input_id = f"opt_{opt_name}"
                    try:
                        new_value = 1 if input[input_id]() else 0
                        if new_value != opt.value:
                            success, msg = mof.update_option(opt_name, new_value)
                            if success:
                                total_updates += 1
                            else:
                                errors.append(msg)
                    except Exception as e:
                        logger.debug(f"Could not get value for {input_id}: {e}")
                elif opt and opt.option_type == "string":
                    input_id = f"opt_{opt_name}"
                    try:
                        new_value = input[input_id]()
                        if new_value and new_value != opt.value:
                            success, msg = mof.update_option(opt_name, new_value)
                            if success:
                                total_updates += 1
                            else:
                                errors.append(msg)
                    except Exception as e:
                        logger.debug(f"Could not get value for {input_id}: {e}")

            # Save options file if there were updates
            if total_updates > 0:
                save_ok, save_msg = mof.save(backup=True)
                if not save_ok:
                    errors.append(save_msg)

        # Save extra constants
        if ecf:
            category_option_names = OPTION_CATEGORIES.get(category, [])
            const_updates = 0
            for const_name in category_option_names:
                const = ecf.get_constant(const_name)
                if const and const.option_type == "float":
                    input_id = f"const_{const_name}"
                    try:
                        new_value = input[input_id]()
                        if new_value is not None and new_value != const.value:
                            success, msg = ecf.update_constant(const_name, new_value)
                            if success:
                                const_updates += 1
                                total_updates += 1
                            else:
                                errors.append(msg)
                    except Exception as e:
                        logger.debug(f"Could not get value for {input_id}: {e}")

            # Save constants file if there were updates
            if const_updates > 0:
                save_ok, save_msg = ecf.save(backup=True)
                if not save_ok:
                    errors.append(save_msg)

        # Report results
        if errors:
            options_save_msg.set(f"Errors: {'; '.join(errors)}")
            ui.notification_show(
                f"Some errors occurred: {'; '.join(errors)}",
                type="error",
                duration=5
            )
        elif total_updates == 0:
            options_save_msg.set("No changes to save")
        else:
            options_save_msg.set(f"Saved {total_updates} changes at {datetime.now().strftime('%H:%M:%S')}")
            ui.notification_show(
                f"Successfully saved {total_updates} option changes",
                type="message",
                duration=3
            )

    @render.text
    def options_save_status():
        """Display options save status"""
        return options_save_msg.get()
