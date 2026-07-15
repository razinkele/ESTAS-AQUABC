"""Mass Balance tab as a Shiny module (Phase 3, Task 1).

`mass_balance_ui(id)` returns the panel *content* (the app-level
panel_conditional stays in create_ui); `mass_balance_server(id, state)`
registers the handlers, ported verbatim from app.py. Self-contained: imports
the `mass_balance` leaf module and self-computes ROOT/INPUTS_DIR/OUTPUT_CSV;
imports nothing from app.py.

NAME-COLLISION NOTE: this file is `shiny_app/modules/mass_balance.py` and
must import from the LEAF module `shiny_app/mass_balance.py`. The fallback
branch uses the exact fully-qualified module path app.py uses
(`shiny_app.mass_balance` / `mass_balance`, resolved from `shiny_app/` being
on sys.path when running as a script) rather than a bare/relative import, so
this module never shadows or self-imports the leaf. Verified (no recursion):
`.venv/bin/python -c "import shiny_app.modules.mass_balance"` -> prints
without RecursionError/ImportError.
"""
import logging
import os

import pandas as pd
from shiny import module, reactive, render, ui

try:
    from shiny_app.mass_balance import MassBalanceCalculator, load_stoichiometry_from_params
except ImportError:  # running as a script from inside shiny_app/
    from mass_balance import MassBalanceCalculator, load_stoichiometry_from_params

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")
OUTPUT_CSV = os.path.join(ROOT, "OUTPUT.csv")


@module.ui
def mass_balance_ui():
    return ui.card(
        ui.card_header("Mass Balance"),
        ui.tooltip(
            ui.input_action_button("calc_mass_balance", "Calculate Mass Balance", class_="btn-primary mb-3"),
            "Calculate element mass balance from model output"
        ),
        ui.layout_columns(
            ui.card(
                ui.card_header("Summary"),
                ui.output_table("mass_balance_summary"),
            ),
            ui.card(
                ui.card_header("Element Details"),
                ui.tooltip(
                    ui.input_select(
                        "mb_element",
                        "Element:",
                        choices=["Nitrogen", "Carbon", "Phosphorus", "Silicon"],
                        selected="Nitrogen"
                    ),
                    "Select element for detailed mass balance breakdown"
                ),
                ui.output_ui("mass_balance_details"),
            ),
            col_widths=[6, 6]
        ),
        ui.card(
            ui.card_header("Time Series"),
            ui.output_ui("mass_balance_plot_ui"),
        )
    )


@module.server
def mass_balance_server(input, output, session, state):
    # `state` is accepted for the uniform x_server(id, state) convention; the
    # mass balance tab is self-contained and uses nothing from it.
    mb_results = reactive.Value(None)
    mb_calculator = reactive.Value(None)

    @reactive.effect
    @reactive.event(input.calc_mass_balance)
    def calculate_mass_balance():
        """Calculate mass balance when button is clicked"""
        if not os.path.exists(OUTPUT_CSV):
            logger.warning("OUTPUT.csv not found for mass balance calculation")
            ui.notification_show(
                "OUTPUT.csv not found. Run the model first.",
                type="warning",
                duration=3
            )
            return

        logger.info("Calculating mass balance...")

        # Load stoichiometry from parameters
        param_file = os.path.join(INPUTS_DIR, "WCONST_04.txt")
        stoich = load_stoichiometry_from_params(param_file)

        # Create calculator and calculate
        calc = MassBalanceCalculator(OUTPUT_CSV, stoich)
        if calc.load_data():
            results = calc.calculate_all()
            mb_calculator.set(calc)
            mb_results.set(results)
            logger.info("Mass balance calculation complete")
            ui.notification_show(
                "Mass balance calculated successfully",
                type="message",
                duration=2
            )
        else:
            logger.error("Failed to load data for mass balance")
            ui.notification_show(
                "Failed to load output data",
                type="error",
                duration=3
            )

    @render.table
    def mass_balance_summary():
        """Render mass balance summary table"""
        results = mb_results.get()
        if results is None:
            return pd.DataFrame({
                "Message": ["Click 'Calculate Mass Balance' to analyze model output"]
            })

        # Create summary table
        data = []
        for element, result in results.items():
            status = "✓" if result.is_conserved() else "⚠"
            data.append({
                "Element": element,
                "Initial": f"{result.initial_total:.4f}",
                "Final": f"{result.final_total:.4f}",
                "Change": f"{result.percent_change:+.2f}%",
                "Status": status
            })

        return pd.DataFrame(data)

    @render.ui
    def mass_balance_details():
        """Render detailed pool breakdown for selected element"""
        results = mb_results.get()
        element = input.mb_element()

        if results is None or element not in results:
            return ui.tags.p("Calculate mass balance to see details", class_="text-muted")

        result = results[element]

        # Create pool breakdown
        pool_rows = []
        for pool_name, pool_series in result.pool_breakdown.items():
            initial = pool_series.iloc[0] if len(pool_series) > 0 else 0
            final = pool_series.iloc[-1] if len(pool_series) > 0 else 0
            change = final - initial

            pool_row = ui.tags.div(
                ui.tags.div(
                    ui.tags.strong(pool_name, class_="small"),
                    class_="col-4"
                ),
                ui.tags.div(
                    ui.tags.small(f"{initial:.4f}", class_="text-muted"),
                    class_="col-3 text-end"
                ),
                ui.tags.div(
                    ui.tags.small(f"{final:.4f}", class_="text-info"),
                    class_="col-3 text-end"
                ),
                ui.tags.div(
                    ui.tags.small(
                        f"{change:+.4f}",
                        class_="text-success" if change >= 0 else "text-danger"
                    ),
                    class_="col-2 text-end"
                ),
                class_="row border-bottom py-1"
            )
            pool_rows.append(pool_row)

        return ui.tags.div(
            ui.tags.div(
                ui.tags.div(ui.tags.strong("Pool", class_="small"), class_="col-4"),
                ui.tags.div(ui.tags.strong("Initial", class_="small"), class_="col-3 text-end"),
                ui.tags.div(ui.tags.strong("Final", class_="small"), class_="col-3 text-end"),
                ui.tags.div(ui.tags.strong("Δ", class_="small"), class_="col-2 text-end"),
                class_="row border-bottom py-1 bg-light"
            ),
            *pool_rows,
            ui.tags.div(
                ui.tags.small(
                    f"Total change: {result.percent_change:+.2f}%",
                    class_="text-warning" if not result.is_conserved() else "text-success"
                ),
                class_="mt-2"
            ),
            style="max-height: 300px; overflow-y: auto;"
        )

    @render.ui
    def mass_balance_plot_ui():
        """Render mass balance time series info"""
        results = mb_results.get()
        calc = mb_calculator.get()
        element = input.mb_element()

        if results is None or calc is None or element not in results:
            return ui.tags.p("Calculate mass balance to see time series", class_="text-muted")

        result = results[element]
        time_col = calc.get_time_column()

        # Show statistics
        return ui.tags.div(
            ui.tags.div(
                ui.tags.div(
                    ui.tags.small("Min", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{result.min_total:.4f}"),
                    class_="col-3 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Max", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{result.max_total:.4f}"),
                    class_="col-3 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Mean", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{result.mean_total:.4f}"),
                    class_="col-3 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Range", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{result.max_total - result.min_total:.4f}"),
                    class_="col-3 text-center"
                ),
                class_="row mb-2"
            ),
            ui.tags.div(
                ui.tags.small(
                    f"Time span: {time_col.iloc[0]:.1f} - {time_col.iloc[-1]:.1f}",
                    class_="text-muted"
                ),
                ui.tags.br(),
                ui.tags.small(
                    f"Data points: {len(result.time_series)}",
                    class_="text-muted"
                ),
            ),
            class_="p-2 border rounded"
        )
