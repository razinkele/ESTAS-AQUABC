from shiny_app import ui_panels

# arg-free panels: name -> MULTIPLE markers (nav condition + card headers) that must ALL appear,
# so a verbatim move that drops/reorders a sub-card fails the test (single nav_x marker is too weak).
ARGFREE = {
    "panel_dashboard": ["nav_dashboard", "Dashboard", "System Status", "Simulation Config"],
    "panel_model_control": ["nav_model_control", "Time Period", "Time Stepping", "Output Interval"],
}


def test_argfree_panels_render_with_all_markers():
    for name, markers in ARGFREE.items():
        html = str(getattr(ui_panels, name)())
        for m in markers:
            assert m in html, f"{name} missing marker {m!r}"
