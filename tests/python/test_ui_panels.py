from shiny_app import ui_panels

# arg-free panels: name -> MULTIPLE markers (nav condition + card headers) that must ALL appear,
# so a verbatim move that drops/reorders a sub-card fails the test (single nav_x marker is too weak).
ARGFREE = {
    "panel_dashboard": ["nav_dashboard", "Dashboard", "System Status", "Simulation Config"],
    "panel_model_control": ["nav_model_control", "Time Period", "Time Stepping", "Output Interval"],
    "panel_mass_balance": ["nav_mass_balance", "Mass Balance", "Summary", "Element Details"],
    "panel_observations": ["nav_observations", "Model Validation - Observations", "Comparison Summary", "Variable Details"],
}


def test_argfree_panels_render_with_all_markers():
    for name, markers in ARGFREE.items():
        html = str(getattr(ui_panels, name)())
        for m in markers:
            assert m in html, f"{name} missing marker {m!r}"


def test_panel_model_build_takes_consts_and_renders():
    compilers = {"gfortran": {"name": "GNU Fortran"}}
    build_types = {"release": {"name": "Release"}}
    html = str(ui_panels.panel_model_build(compilers, build_types))
    for m in ("nav_model_build", "Build Configuration", "Available Executables", "GNU Fortran", "Release"):
        assert m in html, f"panel_model_build missing {m!r}"


def test_panel_plot_takes_min_smooth_window_and_renders():
    html = str(ui_panels.panel_plot(2))
    for m in ("nav_plot", "Plot &amp; Visualization", "Select Output Directory", "Files Summary"):
        assert m in html, f"panel_plot missing {m!r}"
