try:
    from shiny_app.modules.initial_conditions import initial_conditions_ui
except ImportError:
    from modules.initial_conditions import initial_conditions_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_initial_conditions_ui_namespaces_ids():
    html = str(initial_conditions_ui("initial_conditions"))
    for raw in (
        "ic_file",
        "ic_category",
        "load_ics",
        "ic_category_info",
        "ic_table",
        "save_ics",
        "ic_save_status",
    ):
        assert nid("initial_conditions", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
