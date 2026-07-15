try:
    from shiny_app.modules.observations import observations_ui
except ImportError:
    from modules.observations import observations_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_observations_ui_namespaces_ids():
    html = str(observations_ui("observations"))
    for raw in (
        "obs_scan_dir",
        "obs_file_select",
        "obs_load_file",
        "obs_file",
        "generate_sample_obs",
        "obs_file_info",
        "obs_variables_table",
        "obs_comparison_summary",
        "obs_variable",
        "obs_metrics_detail",
        "obs_scatter_info",
    ):
        assert nid("observations", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
