"""Scenarios tab as a Shiny module (Phase 2, Task 5).

`scenarios_ui(id)` returns the panel *content* (the app-level panel_conditional
stays in create_ui); `scenarios_server(id, state)` registers the handlers,
ported verbatim from app.py. Self-contained: imports the `scenarios` leaf
module and self-computes INPUTS_DIR; imports nothing from app.py.

NAME-COLLISION NOTE: this file is `shiny_app/modules/scenarios.py` and must
import from the LEAF module `shiny_app/scenarios.py`. The fallback branch
uses the exact fully-qualified module path app.py uses
(`shiny_app.scenarios` / `scenarios`, resolved from `shiny_app/` being on
sys.path when running as a script) rather than a bare/relative import, so
this module never shadows or self-imports the leaf. Verified (no recursion):
`.venv/bin/python -c "import shiny_app.modules.scenarios"` -> prints without
RecursionError/ImportError. Only `load_scenario_manager` is actually used by
the ported handlers below; `Scenario`/`ScenarioManager`/`get_scenarios_dir`
(present in app.py's equivalent import for parity) are omitted here to keep
this fully lint-gated subpackage F401-clean.
"""
import logging
import os

from shiny import module, reactive, render, ui

try:
    from shiny_app.scenarios import load_scenario_manager
except ImportError:  # running as a script from inside shiny_app/
    from scenarios import load_scenario_manager

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")


@module.ui
def scenarios_ui():
    return ui.card(
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


@module.server
def scenarios_server(input, output, session, state):
    # `state` is accepted for the uniform x_server(id, state) convention; the
    # scenarios tab is self-contained and uses nothing from it.
    scenario_mgr = reactive.Value(None)
    scenario_status_msg = reactive.Value("")

    # Initialize scenario manager on session start
    @reactive.effect
    def init_scenario_manager():
        """Initialize the scenario manager"""
        mgr = load_scenario_manager(INPUTS_DIR)
        scenario_mgr.set(mgr)
        logger.info(f"Scenario manager initialized with {len(mgr.list_scenarios())} scenarios")

    @reactive.effect
    @reactive.event(input.refresh_scenarios)
    def refresh_scenario_list():
        """Refresh the scenario list"""
        mgr = scenario_mgr.get()
        if mgr:
            mgr.refresh()
            scenario_status_msg.set("Scenarios refreshed")
            logger.info("Scenarios refreshed")

    @render.ui
    def scenario_info():
        """Display information about selected scenario"""
        mgr = scenario_mgr.get()
        scenario_name = input.scenario_select()

        if not mgr or not scenario_name:
            return ui.tags.p("Select a scenario to view details", class_="text-muted")

        scenario = mgr.get_scenario(scenario_name)
        if not scenario:
            return ui.tags.p("Scenario not found", class_="text-warning")

        # Build info display
        info_items = [
            ui.tags.p(ui.tags.strong("Description: "), scenario.description or "No description"),
            ui.tags.p(
                ui.tags.strong("Created: "),
                scenario.created[:10] if scenario.created else "Unknown"
            ),
            ui.tags.p(ui.tags.strong("Contents:")),
            ui.tags.ul(
                ui.tags.li(f"{len(scenario.parameters)} parameters") if scenario.parameters else None,
                ui.tags.li(f"{len(scenario.initial_conditions)} initial conditions ({scenario.ic_file})") if scenario.initial_conditions else None,
                ui.tags.li(f"{len(scenario.model_options)} model options") if scenario.model_options else None,
                ui.tags.li(f"{len(scenario.extra_constants)} extra constants") if scenario.extra_constants else None,
            ),
        ]

        if scenario.is_builtin:
            info_items.append(ui.tags.span("Built-in preset (read-only)", class_="badge bg-secondary"))

        return ui.tags.div(*[item for item in info_items if item is not None])

    @reactive.effect
    def update_scenario_choices():
        """Update scenario dropdown choices"""
        mgr = scenario_mgr.get()
        if mgr:
            names = mgr.get_scenario_names()
            if names:
                ui.update_select("scenario_select", choices=names, selected=names[0])
            else:
                ui.update_select("scenario_select", choices=["No scenarios available"], selected=None)

    @reactive.effect
    @reactive.event(input.load_scenario)
    def load_selected_scenario():
        """Load and apply the selected scenario"""
        mgr = scenario_mgr.get()
        scenario_name = input.scenario_select()

        if not mgr or not scenario_name:
            scenario_status_msg.set("No scenario selected")
            return

        scenario = mgr.get_scenario(scenario_name)
        if not scenario:
            scenario_status_msg.set(f"Scenario '{scenario_name}' not found")
            return

        logger.info(f"Applying scenario: {scenario_name}")

        success, message = mgr.apply_scenario(scenario)

        if success:
            scenario_status_msg.set(f"Loaded: {message}")
            ui.notification_show(
                f"Successfully applied scenario '{scenario_name}'",
                type="message",
                duration=4
            )
        else:
            scenario_status_msg.set(f"Error: {message}")
            ui.notification_show(
                f"Failed to apply scenario: {message}",
                type="error",
                duration=5
            )

    @reactive.effect
    @reactive.event(input.save_scenario)
    def save_new_scenario():
        """Save current configuration as a new scenario"""
        mgr = scenario_mgr.get()
        name = input.new_scenario_name()
        description = input.new_scenario_desc()

        if not mgr:
            scenario_status_msg.set("Scenario manager not initialized")
            return

        if not name or not name.strip():
            scenario_status_msg.set("Please enter a scenario name")
            ui.notification_show("Please enter a scenario name", type="warning")
            return

        name = name.strip()

        # Check if scenario already exists and is builtin
        existing = mgr.get_scenario(name)
        if existing and existing.is_builtin:
            scenario_status_msg.set("Cannot overwrite built-in scenarios")
            ui.notification_show("Cannot overwrite built-in scenarios", type="error")
            return

        logger.info(f"Capturing current state as scenario: {name}")

        # Capture current state
        scenario, capture_msg = mgr.capture_current_state(
            name=name,
            description=description,
            include_params=input.scenario_include_params(),
            include_ics=input.scenario_include_ics(),
            include_options=input.scenario_include_options(),
            ic_file=input.save_ic_file()
        )

        if not scenario:
            scenario_status_msg.set(f"Failed to capture: {capture_msg}")
            return

        # Save scenario
        success, save_msg = mgr.save_scenario(scenario)

        if success:
            scenario_status_msg.set(f"Saved scenario '{name}'")
            ui.notification_show(
                f"Successfully saved scenario '{name}'",
                type="message",
                duration=4
            )
            # Refresh the dropdown
            mgr.refresh()
            names = mgr.get_scenario_names()
            ui.update_select("scenario_select", choices=names, selected=name)
            # Clear input fields
            ui.update_text("new_scenario_name", value="")
            ui.update_text_area("new_scenario_desc", value="")
        else:
            scenario_status_msg.set(f"Save failed: {save_msg}")
            ui.notification_show(f"Failed to save: {save_msg}", type="error", duration=5)

    @reactive.effect
    @reactive.event(input.delete_scenario)
    def delete_selected_scenario():
        """Delete the selected scenario"""
        mgr = scenario_mgr.get()
        scenario_name = input.scenario_select()

        if not mgr or not scenario_name:
            scenario_status_msg.set("No scenario selected")
            return

        scenario = mgr.get_scenario(scenario_name)
        if not scenario:
            scenario_status_msg.set(f"Scenario '{scenario_name}' not found")
            return

        if scenario.is_builtin:
            scenario_status_msg.set("Cannot delete built-in scenarios")
            ui.notification_show("Cannot delete built-in scenarios", type="warning")
            return

        success, message = mgr.delete_scenario(scenario_name)

        if success:
            scenario_status_msg.set(f"Deleted scenario '{scenario_name}'")
            ui.notification_show(f"Deleted scenario '{scenario_name}'", type="message", duration=3)
            # Refresh dropdown
            mgr.refresh()
            names = mgr.get_scenario_names()
            ui.update_select("scenario_select", choices=names, selected=names[0] if names else None)
        else:
            scenario_status_msg.set(f"Delete failed: {message}")
            ui.notification_show(f"Failed to delete: {message}", type="error", duration=5)

    @render.text
    def scenario_status():
        """Display scenario status message"""
        return scenario_status_msg.get()
