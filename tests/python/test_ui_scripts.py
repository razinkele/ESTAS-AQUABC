from shiny_app.ui_scripts import (
    reload_script, nav_script, settings_script,
    help_script, changelog_script, theme_script,
)

_MARKERS = [
    (reload_script, "reload_page"),
    (nav_script, "initSidebar"),
    (settings_script, "settingsOffcanvas"),
    (help_script, "helpOffcanvas"),
    (changelog_script, "changelogOffcanvas"),
    (theme_script, "classList"),
]


def test_each_script_renders_a_script_tag_with_its_marker():
    for fn, marker in _MARKERS:
        html = str(fn())
        assert html.lstrip().startswith("<script"), f"{fn.__name__} is not a <script> tag"
        assert marker in html, f"{fn.__name__} missing marker {marker!r}"
