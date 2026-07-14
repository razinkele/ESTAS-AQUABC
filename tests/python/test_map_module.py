try:
    from shiny_app.modules.map import map_ui
except ImportError:
    from modules.map import map_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_map_ui_namespaces_ids():
    html = str(map_ui("map"))
    for raw in ("pydeck_map", "map_info"):
        assert nid("map", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
