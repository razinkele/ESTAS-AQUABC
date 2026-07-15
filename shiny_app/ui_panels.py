"""Content-panel fragments for the AQUABC UI (extracted from create_ui())."""
from shiny import ui


def panel_dashboard():
    return ui.panel_conditional(
        "input.navigation === 'nav_dashboard'",
        ui.card(
            ui.card_header("Dashboard"),
            # Status bar — horizontal summary strip
            ui.div(
                {"class": "dashboard-status-bar"},
                ui.div(
                    {"class": "status-item"},
                    ui.div({"class": "status-dot idle", "id": "run-status-dot"}),
                    ui.div(
                        ui.div("Status", class_="status-label"),
                        ui.div(ui.output_text("dashboard_status_text", inline=True), class_="status-value"),
                    ),
                ),
                ui.div(
                    {"class": "status-item"},
                    ui.tags.i(class_="bi bi-cpu", style="color: var(--text-muted);"),
                    ui.div(
                        ui.div("Executable", class_="status-label"),
                        ui.div(ui.output_text("dashboard_exe_text", inline=True), class_="status-value"),
                    ),
                ),
                ui.div(
                    {"class": "status-item"},
                    ui.tags.i(class_="bi bi-clock-history", style="color: var(--text-muted);"),
                    ui.div(
                        ui.div("Last Run", class_="status-label"),
                        ui.div(ui.output_text("dashboard_last_run_text", inline=True), class_="status-value"),
                    ),
                ),
                ui.div(
                    {"class": "status-item", "style": "margin-left: auto;"},
                    ui.output_ui("run_timer_display"),
                ),
            ),
            # Two-column layout: actions + system | run log
            ui.layout_columns(
                # Left: Quick actions + system info
                ui.div(
                    ui.layout_columns(
                        ui.tooltip(
                            ui.input_action_button("quick_run", "Quick Run", class_="btn-success btn-lg w-100"),
                            "Run the model with current settings using the selected executable"
                        ),
                        ui.tooltip(
                            ui.input_action_button("dashboard_stop", "Stop", class_="btn-danger btn-lg w-100"),
                            "Stop the currently running model simulation"
                        ),
                        col_widths=[6, 6],
                        class_="mb-3"
                    ),
                    ui.layout_columns(
                        ui.card(
                            ui.card_header("System Status"),
                            ui.div(
                                ui.output_ui("system_status_compact"),
                                style="max-height: 280px; overflow-y: auto; font-size: 0.78rem;"
                            ),
                            fill=False
                        ),
                        ui.card(
                            ui.card_header("Simulation Config"),
                            ui.div(
                                ui.output_ui("input_txt_variables"),
                                style="max-height: 280px; overflow-y: auto; font-size: 0.78rem;"
                            ),
                            fill=False
                        ),
                        col_widths=[6, 6]
                    ),
                    ui.tooltip(
                        ui.input_action_button("goto_model_config", "Model Config", class_="btn-primary btn-sm w-100 mt-2"),
                        "Navigate to Model Control panel to configure simulation settings"
                    ),
                ),
                # Right: Run log
                ui.card(
                    ui.card_header(
                        ui.div(
                            "Run Log",
                            ui.input_action_button("btn_copy_dashboard_log", "Copy", class_="btn-sm btn-outline-secondary float-end"),
                            class_="d-flex justify-content-between align-items-center w-100"
                        )
                    ),
                    ui.div(
                        ui.output_ui("dashboard_run_log"),
                        style="height: 420px; overflow-y: auto; padding: 10px; border-radius: 4px;",
                        class_="run-log-container",
                        id="dashboard_log_container"
                    ),
                    fill=False
                ),
                col_widths=[5, 7]
            )
        )
    )
