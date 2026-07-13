from shiny_app import ui_chrome

NAV_CHOICES = {
    "nav_dashboard": ("bi-speedometer2", "Dashboard"),
    "nav_plot": ("bi-graph-up", "Plot"),
}


def test_build_sidebar_renders_nav_links_from_choices():
    html = str(ui_chrome.build_sidebar(NAV_CHOICES))
    assert "sidebar-nav" in html
    assert "Dashboard" in html and "Plot" in html


def test_argfree_chrome_fragments_render_with_markers():
    cases = [
        (ui_chrome.app_header, "app-header"),
        (ui_chrome.external_css, "bootstrap-icons"),
        (ui_chrome.settings_offcanvas, "settingsOffcanvas"),
        (ui_chrome.help_offcanvas, "help_content"),
        (ui_chrome.changelog_offcanvas, "changelog_content"),
    ]
    for fn, marker in cases:
        assert marker in str(fn()), f"{fn.__name__} missing {marker!r}"
