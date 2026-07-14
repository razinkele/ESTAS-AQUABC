try:
    from shiny_app.modules.model_structure import model_structure_ui
except ImportError:
    from modules.model_structure import model_structure_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_model_structure_ui_namespaces_ids():
    html = str(model_structure_ui("model_structure"))
    assert nid("model_structure", "model_structure_iframe") in html
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
