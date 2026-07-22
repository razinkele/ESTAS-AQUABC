"""Chrome fragments (sidebar, header, css, offcanvas) for the AQUABC UI (extracted from create_ui())."""
from shiny import ui


def build_sidebar(nav_choices):
    nav_links = []
    for nav_id, (icon, label) in nav_choices.items():
        is_active = "active" if nav_id == "nav_dashboard" else ""
        nav_links.append(
            ui.tags.a(
                {"class": f"nav-link {is_active}", "href": "#", "data-nav-id": nav_id},
                ui.tags.i(class_=f"bi {icon}"),
                ui.tags.span(label)
            )
        )

    # === SIDEBAR: Custom Navigation Menu ===
    sidebar_content = ui.div(
        {"class": "custom-sidebar", "id": "custom-sidebar"},
        # Sidebar header with title and collapse button
        ui.div(
            {"class": "sidebar-header"},
            ui.tags.span("AQUABC Menu", class_="sidebar-title"),
            ui.tags.button(
                ui.tags.i(class_="bi bi-list"),
                id="sidebar-collapse-btn",
                class_="sidebar-toggle",
                type="button",
                title="Collapse menu",
            ),
        ),
        # Navigation links
        ui.div({"class": "sidebar-nav"}, *nav_links),
        # Tutorial link at bottom of sidebar
        ui.div(
            {"class": "sidebar-nav", "style": "border-top: 1px solid rgba(255,255,255,0.15); margin-top: auto; padding-top: 0.5rem;"},
            ui.tags.a(
                {"class": "nav-link", "href": "tutorial.html", "target": "_blank",
                 "style": "opacity: 0.8;"},
                ui.tags.i(class_="bi bi-book"),
                ui.tags.span("Tutorial"),
                ui.tags.i(class_="bi bi-box-arrow-up-right", style="font-size: 0.7em; margin-left: 4px; opacity: 0.6;"),
            ),
        ),
    )
    return sidebar_content


def app_header():
    # App header bar
    app_header = ui.div(
        {"class": "app-header"},
        ui.div(
            {"class": "app-header-title"},
            ui.tags.i(class_="bi bi-water me-2"),
            "AQUABC",
            ui.tags.span("v0.6.0", class_="version-badge"),
        ),
        # Right side buttons container (tutorial + changelog + help + settings)
        ui.div(
            {"class": "d-flex align-items-center gap-2"},
            # Tutorial button - opens in new window
            ui.tooltip(
                ui.tags.a(
                    ui.tags.i(class_="bi bi-book"),
                    href="tutorial.html",
                    target="_blank",
                    class_="btn btn-link text-light p-1",
                    title="Tutorial",
                    style="font-size: inherit; text-decoration: none;",
                ),
                "Open Getting Started Tutorial in a new window"
            ),
            # Changelog button
            ui.tooltip(
                ui.input_action_button(
                    "changelog_toggle",
                    ui.tags.i(class_="bi bi-journal-text"),
                    class_="btn btn-link text-light p-1",
                    title="Changelog"
                ),
                "View recent changes and updates"
            ),
            # Help button
            ui.input_action_button(
                "help_toggle",
                ui.tags.i(class_="bi bi-question-circle-fill"),
                class_="btn btn-link text-light p-1",
                title="State Variables Help"
            ),
            # Theme toggle button (light / dark)
            ui.tooltip(
                ui.tags.button(
                    ui.tags.i(class_="bi bi-sun-fill", id="theme-icon"),
                    id="theme-toggle-btn",
                    class_="btn btn-link text-light p-1",
                    type="button",
                    title="Toggle light/dark theme",
                    style="font-size: inherit;",
                ),
                "Switch between light and dark theme"
            ),
            # Settings gear icon button
            ui.input_action_button(
                "settings_toggle",
                ui.tags.i(class_="bi bi-gear-fill"),
                class_="btn btn-link text-light p-1",
                title="Settings"
            ),
        ),
    )
    return app_header


def external_css():
    # External CSS resources
    external_css = ui.TagList(
        ui.tags.link(
            rel="stylesheet",
            href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
        ),
        ui.tags.link(
            rel="preconnect",
            href="https://fonts.googleapis.com"
        ),
        ui.tags.link(
            rel="preconnect",
            href="https://fonts.gstatic.com",
            crossorigin=""
        ),
        ui.tags.link(
            rel="stylesheet",
            href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@400;500;600;700&family=Instrument+Serif&display=swap"
        ),
        ui.tags.link(
            rel="stylesheet",
            href="aquabc.css"
        ),
    )
    return external_css


def settings_offcanvas():
    # Settings offcanvas
    settings_offcanvas = ui.tags.div(
        ui.tags.div(
            ui.tags.div(
                ui.tags.h5("Settings", class_="offcanvas-title"),
                ui.tags.button(
                    type="button",
                    class_="btn-close btn-close-white",
                    **{"data-bs-dismiss": "offcanvas", "aria-label": "Close"}
                ),
                class_="offcanvas-header"
            ),
            ui.tags.div(
                ui.card(
                    ui.card_header("About"),
                    ui.tags.h5("AQUABC v0.6.0"),
                    ui.tags.p("Aquatic Biogeochemical Model"),
                    ui.tags.p("A sophisticated water quality simulation tool with:"),
                    ui.tags.ul(
                        ui.tags.li("318 calibratable parameters"),
                        ui.tags.li("36 state variables"),
                        ui.tags.li("Complex biogeochemical processes"),
                    ),
                    fill=False
                ),
                class_="offcanvas-body"
            ),
            class_="offcanvas offcanvas-end",
            tabindex="-1",
            id="settingsOffcanvas",
            **{"aria-labelledby": "settingsOffcanvasLabel"}
        )
    )
    return settings_offcanvas


def help_offcanvas():
    # Help offcanvas for state variables reference
    help_offcanvas = ui.tags.div(
        ui.tags.div(
            ui.tags.div(
                ui.tags.h5("State Variables Reference", class_="offcanvas-title"),
                ui.tags.button(
                    type="button",
                    class_="btn-close btn-close-white",
                    **{"data-bs-dismiss": "offcanvas", "aria-label": "Close"}
                ),
                class_="offcanvas-header bg-primary text-light"
            ),
            ui.tags.div(
                ui.output_ui("help_content"),
                class_="offcanvas-body",
                style="overflow-y: auto; max-height: calc(100vh - 60px);"
            ),
            class_="offcanvas offcanvas-end",
            tabindex="-1",
            id="helpOffcanvas",
            style="width: 700px;",  # Wide enough for tables
            **{"aria-labelledby": "helpOffcanvasLabel"}
        )
    )
    return help_offcanvas


def changelog_offcanvas():
    # Changelog offcanvas for displaying CHANGELOG.md
    changelog_offcanvas = ui.tags.div(
        ui.tags.div(
            ui.tags.div(
                ui.tags.h5("Changelog", class_="offcanvas-title"),
                ui.tags.button(
                    type="button",
                    class_="btn-close btn-close-white",
                    **{"data-bs-dismiss": "offcanvas", "aria-label": "Close"}
                ),
                class_="offcanvas-header bg-info text-light"
            ),
            ui.tags.div(
                ui.output_ui("changelog_content"),
                class_="offcanvas-body",
                style="overflow-y: auto; max-height: calc(100vh - 60px);"
            ),
            class_="offcanvas offcanvas-end",
            tabindex="-1",
            id="changelogOffcanvas",
            style="width: 600px;",
            **{"aria-labelledby": "changelogOffcanvasLabel"}
        )
    )
    return changelog_offcanvas
