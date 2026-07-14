try:
    from shiny_app.modules.scenarios import scenarios_ui
except ImportError:
    from modules.scenarios import scenarios_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_scenarios_ui_namespaces_ids():
    html = str(scenarios_ui("scenarios"))
    for raw in (
        "scenario_select",
        "load_scenario",
        "delete_scenario",
        "refresh_scenarios",
        "scenario_info",
        "new_scenario_name",
        "new_scenario_desc",
        "scenario_include_params",
        "save_ic_file",
        "scenario_include_ics",
        "scenario_include_options",
        "save_scenario",
        "scenario_status",
    ):
        assert nid("scenarios", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
