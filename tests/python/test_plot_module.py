try:
    from shiny_app.modules.plot import plot_ui
except ImportError:
    from modules.plot import plot_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_plot_ui_namespaces_ids():
    # plot_ui takes min_smooth_window after the id (@module.ui extra arg)
    html = str(plot_ui("plot", 2))
    for raw in (
        # Output Directory tab
        "output_dir_select",
        "refresh_output_dirs",
        "analyze_output_dir",
        "output_files_summary",
        # Model Output tab
        "output_format",
        "plot_output_file",
        "plot_output_file_info",
        "output_file_preview",
        "refresh_plot_files",
        "left_vars",
        "right_vars",
        "log_left",
        "log_right",
        "smooth",
        "smooth_window",
        "nrows",
        "refresh_plot",
        "main_plot",
        # Input Timeseries tab
        "input_ts_file",
        "input_ts_boxes",
        "plot_input_ts",
        "input_ts_info",
        "input_ts_subset",
        "input_ts_date_range",
        "input_ts_plot",
        # Data Preview tab
        "out_preview",
    ):
        assert nid("plot", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
