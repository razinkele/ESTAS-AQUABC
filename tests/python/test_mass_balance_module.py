try:
    from shiny_app.modules.mass_balance import mass_balance_ui
except ImportError:
    from modules.mass_balance import mass_balance_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_mass_balance_ui_namespaces_ids():
    html = str(mass_balance_ui("mass_balance"))
    for raw in (
        "calc_mass_balance",
        "mass_balance_summary",
        "mb_element",
        "mass_balance_details",
        "mass_balance_plot_ui",
    ):
        assert nid("mass_balance", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html
