try:
    from shiny_app.modules.model_options import model_options_ui
except ImportError:
    from modules.model_options import model_options_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_model_options_ui_namespaces_ids():
    html = str(model_options_ui("model_options"))
    for raw in (
        "options_category",
        "load_options",
        "options_switches",
        "options_constants",
        "save_options",
        "options_save_status",
    ):
        assert nid("model_options", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
