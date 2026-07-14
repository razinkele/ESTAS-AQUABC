from shiny import ui

try:
    from shiny_app.modules.sim_config import sim_config_ui
except ImportError:
    from modules.sim_config import sim_config_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_sim_config_ui_namespaces_ids():
    # sim_config_ui returns a bare nav_panel, which does NOT render via str()
    # (returns a Python repr). Compose into a navset and tagify to get real HTML.
    html = str(ui.navset_card_tab(sim_config_ui("sim_config")).tagify())
    for raw in (
        "load_sim_config",
        "sim_base_year",
        "sim_start_date",
        "sim_end_date",
        "sim_timestep_preset",
        "sim_timesteps_per_day",
        "sim_output_preset",
        "sim_print_interval",
        "sim_model_sediments",
        "sim_resuspension",
        "save_sim_config",
    ):
        assert nid("sim_config", raw) in html, f"missing namespaced id for {raw}"
    # the sub-tab title survives
    assert "Simulation Config" in html
    # the module UI must NOT carry the app-level nav wrapper (that stays in create_ui)
    assert "input.navigation" not in html
