from shiny import ui

try:
    from shiny_app.modules.run_control import run_control_ui
except ImportError:
    from modules.run_control import run_control_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_run_control_ui_namespaces_ids():
    # run_control_ui returns a plain list of two nav_panels (fat-tab hazard 1),
    # NOT a single ui.TagList — must be *-unpacked into navset_card_tab.
    html = str(ui.navset_card_tab(*run_control_ui("run_control")).tagify())

    # Run Model sub-tab ids
    for raw in (
        "goto_build",
        "run_executable",
        "cmd_input_file",
        "cmd_constants_file",
        "cmd_binary_enabled",
        "cmd_binary_filename",
        "solver_select",
        "run",
        "stop_run",
        "btn_copy_mini_log",
    ):
        assert nid("run_control", raw) in html, f"missing namespaced id for {raw}"
    for raw in (
        "run_executable_info",
        "cmd_preview",
        "constants_validation_status",
        "run_status_indicator",
        "run_log_mini",
    ):
        assert nid("run_control", raw) in html, f"missing namespaced output id for {raw}"

    # Output Config sub-tab ids
    for raw in (
        "output_boxes",
        "sim_output_dir",
        "refresh_sim_output_dirs",
        "output_types",
        "load_output_config",
        "save_output_config",
    ):
        assert nid("run_control", raw) in html, f"missing namespaced id for {raw}"
    for raw in ("sim_output_dir_info", "output_config_status"):
        assert nid("run_control", raw) in html, f"missing namespaced output id for {raw}"

    # both sub-tab titles survive
    assert "Run Model" in html
    assert "Output Config" in html

    # the module UI must NOT carry the app-level nav wrapper (that stays in create_ui)
    assert "input.navigation" not in html
