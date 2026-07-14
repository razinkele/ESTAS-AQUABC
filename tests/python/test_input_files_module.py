try:
    from shiny_app.modules.input_files import input_files_ui
except ImportError:
    from modules.input_files import input_files_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_input_files_ui_namespaces_ids():
    html = str(input_files_ui("input_files"))
    for raw in (
        "file_category_filter",
        "refresh_files",
        "file_select",
        "file_info_panel",
        "file_header_text",
        "file_contents",
        "map_display_view",
        "map_bathymetry_box",
        "map_display_plot",
        "map_display_info",
    ):
        assert nid("input_files", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
