try:
    from shiny_app.modules.parameters import parameters_ui
except ImportError:
    from modules.parameters import parameters_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_parameters_ui_namespaces_ids():
    html = str(parameters_ui("parameters"))
    # within-tab widgets get the "parameters-" prefix; the panel_conditional/nav is NOT here
    for raw in ("param_file", "param_category", "load_params", "save_params",
                "param_category_info", "param_table", "param_save_status"):
        assert nid("parameters", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
