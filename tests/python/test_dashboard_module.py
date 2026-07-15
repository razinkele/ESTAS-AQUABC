try:
    from shiny_app.modules.dashboard import dashboard_ui
except ImportError:
    from modules.dashboard import dashboard_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_dashboard_ui_namespaces_ids_and_renders_content():
    html = str(dashboard_ui("dashboard"))

    for raw in ("quick_run", "dashboard_stop", "goto_model_config", "btn_copy_dashboard_log"):
        assert nid("dashboard", raw) in html, f"missing namespaced id for {raw}"

    for raw in (
        "dashboard_status_text",
        "dashboard_exe_text",
        "dashboard_last_run_text",
        "run_timer_display",
        "system_status_compact",
        "input_txt_variables",
        "dashboard_run_log",
    ):
        assert nid("dashboard", raw) in html, f"missing namespaced output id for {raw}"

    # content markers migrated from the deleted test_ui_panels.py panel_dashboard case
    for marker in ("Dashboard", "System Status", "Simulation Config"):
        assert marker in html, f"dashboard_ui missing marker {marker!r}"

    # the module UI must NOT carry the app-level nav wrapper (that stays in create_ui)
    assert "input.navigation" not in html
