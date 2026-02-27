#!/usr/bin/env python3
"""
Diagnostics panel for the AQUABC Shiny UI.

Provides ``diagnostics_ui()`` (returns a ``panel_conditional``) and
``diagnostics_server()`` (registers all reactive/render functions for the
Diagnostics tab).

The module wraps ``tools/deep_process_rate_analysis.run_analysis()`` and
surfaces results as interactive tables, Plotly charts, and downloadable
PDF reports — all inside a four-tab layout (Overview · Detailed Results ·
Visualisations · Reports).
"""

import os
import sys
import logging
import threading
import time
import traceback
from pathlib import Path

import pandas as pd
import numpy as np
from shiny import ui, reactive, render, req

logger = logging.getLogger("aquabc.diagnostics")

# ── locate analysis engine ──────────────────────────────────────────────────
_script_dir = os.path.dirname(os.path.realpath(__file__))
_parent_dir = os.path.dirname(_script_dir)
_tools_dir  = os.path.join(_parent_dir, "tools")

# Ensure tools/ is on sys.path so we can import the analysis engine
if _tools_dir not in sys.path:
    sys.path.insert(0, _tools_dir)

from deep_process_rate_analysis import run_analysis  # noqa: E402
from aquabc_analysis_utils import BOX_IDS, BOX_TYPES, SEV_ERROR, SEV_WARNING, SEV_INFO, SEV_OK  # noqa: E402

# Plot helpers
try:
    from shiny_app.diagnostics_plots import (
        severity_bar_chart, box_health_heatmap,
        findings_per_check_chart, findings_per_box_chart,
        limitation_radar,
    )
except ImportError:
    from diagnostics_plots import (
        severity_bar_chart, box_health_heatmap,
        findings_per_check_chart, findings_per_box_chart,
        limitation_radar,
    )


# ─────────────────────────────────────────────────────────────────────────────
# Check metadata (key → display name)
# ─────────────────────────────────────────────────────────────────────────────
CHECK_DISPLAY = {
    "rate_statistics":          "1 — Rate Statistics",
    "derivative_consistency":   "2 — Derivative Consistency",
    "cross_variable":           "3 — Cross-Variable Consistency",
    "bug_fixes":                "4 — Bug-Fix Verification",
    "dominant_processes":       "5 — Dominant Processes",
    "seasonal_patterns":        "6 — Seasonal Patterns",
    "zero_slots":               "7 — Zero-Slot Analysis",
    "limitation_factors":       "8 — Limitation Factors",
    "negatives":                "9 — Negative Concentrations",
    "nan_inf":                  "10 — NaN / Inf Detection",
    "stoichiometry":            "11 — Stoichiometry",
    "sign_violations":          "12 — Sign Violations",
    "euler_integration":        "13 — Euler Integration",
    "allelopathy":              "14 — Allelopathy",
    "smoothness":               "15 — Smoothness",
    "mass_balance":             "16 — Mass Balance",
}

SEV_DISPLAY = {"ERROR": SEV_ERROR, "WARNING": SEV_WARNING, "INFO": SEV_INFO, "OK": SEV_OK}
SEV_INV = {v: k for k, v in SEV_DISPLAY.items()}


# ═════════════════════════════════════════════════════════════════════════════
# UI builder
# ═════════════════════════════════════════════════════════════════════════════
def diagnostics_ui():
    """Return the ``panel_conditional`` for the Diagnostics nav entry."""

    # ── Tab 1: Overview ─────────────────────────────────────────────────
    tab_overview = ui.nav_panel(
        "Overview",
        ui.layout_columns(
            # Run Analysis button + output-dir selector
            ui.card(
                ui.card_header(
                    ui.tags.i(class_="bi bi-play-circle me-2"),
                    "Run Analysis"
                ),
                ui.tooltip(
                    ui.input_select(
                        "diag_output_dir", "Output Directory:",
                        choices={"OUTPUTS": "OUTPUTS"},
                        width="100%",
                    ),
                    "Select directory containing model output files"
                ),
                ui.tooltip(
                    ui.input_action_button(
                        "diag_run_btn", "Run Diagnostics",
                        class_="btn-primary w-100 mt-2",
                        icon=ui.tags.i(class_="bi bi-shield-check me-1"),
                    ),
                    "Launch the 16-check diagnostic analysis on the selected output"
                ),
                ui.output_ui("diag_run_status"),
                fill=False,
            ),
            # Severity summary cards
            ui.card(
                ui.card_header(
                    ui.tags.i(class_="bi bi-bar-chart me-2"),
                    "Severity Summary"
                ),
                ui.output_ui("diag_severity_cards"),
            ),
            col_widths=[4, 8],
        ),
        ui.card(
            ui.card_header(
                ui.tags.i(class_="bi bi-table me-2"),
                "Per-Box Summary"
            ),
            ui.output_ui("diag_box_summary_table"),
        ),
    )

    # ── Tab 2: Detailed Results ─────────────────────────────────────────
    tab_detailed = ui.nav_panel(
        "Detailed Results",
        ui.layout_columns(
            ui.input_select(
                "diag_filter_box", "Box:",
                choices={"ALL": "All Boxes"},
                width="100%",
            ),
            ui.input_select(
                "diag_filter_severity", "Severity:",
                choices={"ALL": "All", "ERROR": "ERROR", "WARNING": "WARNING",
                         "INFO": "INFO", "OK": "OK"},
                width="100%",
            ),
            ui.input_select(
                "diag_filter_check", "Check:",
                choices={"ALL": "All Checks"},
                width="100%",
            ),
            col_widths=[4, 4, 4],
        ),
        ui.card(
            ui.card_header("Findings"),
            ui.output_data_frame("diag_findings_table"),
        ),
    )

    # ── Tab 3: Visualisations ───────────────────────────────────────────
    tab_viz = ui.nav_panel(
        "Visualisations",
        ui.layout_columns(
            ui.card(
                ui.card_header("Severity Distribution"),
                ui.output_ui("diag_plot_severity"),
            ),
            ui.card(
                ui.card_header("Findings per Check"),
                ui.output_ui("diag_plot_per_check"),
            ),
            col_widths=[6, 6],
        ),
        ui.layout_columns(
            ui.card(
                ui.card_header("Findings per Box"),
                ui.output_ui("diag_plot_per_box"),
            ),
            ui.card(
                ui.card_header("Box Health Matrix"),
                ui.output_ui("diag_plot_heatmap"),
            ),
            col_widths=[6, 6],
        ),
    )

    # ── Tab 4: Reports ──────────────────────────────────────────────────
    tab_reports = ui.nav_panel(
        "Reports",
        ui.card(
            ui.card_header(
                ui.tags.i(class_="bi bi-file-earmark-pdf me-2"),
                "Generate PDF Reports"
            ),
            ui.layout_columns(
                ui.div(
                    ui.tooltip(
                        ui.input_action_button(
                            "diag_gen_results_pdf", "Generate Results Report",
                            class_="btn-outline-primary w-100 mb-2",
                            icon=ui.tags.i(class_="bi bi-file-earmark-pdf me-1"),
                        ),
                        "Generate a PDF report of the analysis results"
                    ),
                    ui.tooltip(
                        ui.input_action_button(
                            "diag_gen_deep_pdf", "Generate Deep Analysis Report",
                            class_="btn-outline-primary w-100 mb-2",
                            icon=ui.tags.i(class_="bi bi-file-earmark-pdf me-1"),
                        ),
                        "Generate a detailed PDF of the deep process rate analysis"
                    ),
                ),
                ui.div(
                    ui.output_ui("diag_pdf_status"),
                ),
                col_widths=[6, 6],
            ),
        ),
    )

    # ── Assemble panel ──────────────────────────────────────────────────
    panel = ui.panel_conditional(
        "input.navigation === 'nav_diagnostics'",
        ui.card(
            ui.card_header(
                ui.tags.i(class_="bi bi-shield-check me-2"),
                "Process Rate Diagnostics",
                ui.tags.span(
                    "16 checks · 25 boxes",
                    class_="badge bg-secondary ms-2",
                ),
            ),
            ui.navset_card_tab(
                tab_overview,
                tab_detailed,
                tab_viz,
                tab_reports,
                id="diag_tabs",
            ),
        ),
    )
    return panel


# ═════════════════════════════════════════════════════════════════════════════
# Helper — flatten results into a list-of-dicts for the findings table
# ═════════════════════════════════════════════════════════════════════════════
def _flatten_results(all_results):
    """Convert nested analysis dict into a flat list of finding dicts."""
    rows = []
    for box_id in sorted(k for k in all_results if k != "cross_box"):
        for check_key, findings in all_results[box_id].items():
            if isinstance(findings, dict):
                # Some checks (bug_fixes, rate_statistics) return dicts — wrap
                _add_dict_findings(rows, box_id, check_key, findings)
                continue
            if not isinstance(findings, list):
                continue
            for f in findings:
                if not isinstance(f, dict):
                    continue
                row = {
                    "Box": int(box_id),
                    "Box Type": BOX_TYPES.get(box_id, "?"),
                    "Check": CHECK_DISPLAY.get(check_key, check_key),
                    "Severity": f.get("severity", SEV_OK),
                    "Message": _build_message(f),
                }
                rows.append(row)

    # Cross-box spatial
    cross = all_results.get("cross_box", [])
    if isinstance(cross, list):
        for f in cross:
            if isinstance(f, dict):
                rows.append({
                    "Box": "—",
                    "Box Type": "cross-box",
                    "Check": "Spatial Consistency",
                    "Severity": f.get("severity", SEV_OK),
                    "Message": _build_message(f),
                })
    return rows


def _add_dict_findings(rows, box_id, check_key, d):
    """Flatten a dict-style check result (e.g. bug_fixes) into rows."""
    for sub_key, sub_val in d.items():
        if isinstance(sub_val, dict):
            status = sub_val.get("status", "")
            msg_parts = [f"{sub_key}:"]
            for k, v in sub_val.items():
                if k != "status":
                    msg_parts.append(f"{k}={v}")
            rows.append({
                "Box": int(box_id),
                "Box Type": BOX_TYPES.get(box_id, "?"),
                "Check": CHECK_DISPLAY.get(check_key, check_key),
                "Severity": SEV_OK if "OK" in str(status).upper() else SEV_INFO,
                "Message": " ".join(msg_parts),
            })


def _build_message(f):
    """Build a human-readable message string from a finding dict."""
    # Use 'message' field if available
    if "message" in f:
        return str(f["message"])
    # Otherwise concatenate key fields
    parts = []
    for k in ("variable", "slot_name", "slot", "note", "check", "detail",
              "element", "ratio", "expected", "actual"):
        if k in f:
            parts.append(f"{k}={f[k]}")
    return "; ".join(parts) if parts else "(no details)"


def _count_severities(flat_rows):
    """Count occurrences of each severity in flat rows."""
    counts = {"ERROR": 0, "WARNING": 0, "INFO": 0, "OK": 0}
    for r in flat_rows:
        sev = r.get("Severity", SEV_OK)
        label = SEV_INV.get(sev, sev)
        if label in counts:
            counts[label] += 1
    return counts


# ═════════════════════════════════════════════════════════════════════════════
# Server
# ═════════════════════════════════════════════════════════════════════════════
def diagnostics_server(input, output, session, root_dir):
    """Register all reactive/render functions for the Diagnostics panel.

    Parameters
    ----------
    root_dir : str
        Absolute path to the AQUABC project root (same as ``ROOT`` in app.py).
    """

    # ── reactive values ─────────────────────────────────────────────────
    diag_results     = reactive.Value(None)   # raw all_results dict
    diag_flat        = reactive.Value([])     # flattened finding rows
    diag_status_msg  = reactive.Value("")     # status line shown in Overview
    diag_running     = reactive.Value(False)  # True while analysis thread runs
    diag_pdf_msg     = reactive.Value("")     # PDF generation status

    # ── initialise output-dir dropdown ──────────────────────────────────
    @reactive.effect
    def _init_diag_dirs():
        dirs = {}
        try:
            for item in sorted(os.listdir(root_dir)):
                if item.startswith("OUTPUTS") and os.path.isdir(os.path.join(root_dir, item)):
                    dirs[item] = item
        except Exception:
            pass
        if not dirs:
            dirs["OUTPUTS"] = "OUTPUTS"
        ui.update_select("diag_output_dir", choices=dirs)

    # ── Run analysis ────────────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.diag_run_btn)
    def _run_analysis():
        if diag_running.get():
            ui.notification_show("Analysis already running…", type="warning", duration=2)
            return

        sel_dir = input.diag_output_dir()
        output_path = os.path.join(root_dir, sel_dir)
        if not os.path.isdir(output_path):
            ui.notification_show(f"Directory not found: {sel_dir}", type="error", duration=3)
            return

        diag_running.set(True)
        diag_status_msg.set("⏳ Running 16-check analysis …")
        ui.notification_show("Diagnostics started — this may take a minute.", type="message", duration=3)

        def _work():
            try:
                # Redirect stdout so run_analysis print() calls don't pollute the server log
                import io
                _old_stdout = sys.stdout
                sys.stdout = io.StringIO()
                try:
                    results = run_analysis(output_path)
                finally:
                    sys.stdout = _old_stdout

                flat = _flatten_results(results)
                counts = _count_severities(flat)

                diag_results.set(results)
                diag_flat.set(flat)
                diag_status_msg.set(
                    f"✓ Analysis complete — "
                    f"{counts['ERROR']} errors, {counts['WARNING']} warnings, "
                    f"{counts['INFO']} info, {counts['OK']} ok"
                )
                logger.info(f"Diagnostics complete: {counts}")
            except Exception as exc:
                diag_status_msg.set(f"❌ Analysis failed: {exc}")
                logger.error(f"Diagnostics error: {exc}\n{traceback.format_exc()}")
            finally:
                diag_running.set(False)

        threading.Thread(target=_work, daemon=True, name="DiagnosticsThread").start()

    # ── Overview: status display ────────────────────────────────────────
    @output
    @render.ui
    def diag_run_status():
        reactive.invalidate_later(1.0)
        msg = diag_status_msg.get()
        if not msg:
            return ui.tags.p("Press 'Run Diagnostics' to start.", class_="text-muted mt-2 small")
        css = "text-success" if msg.startswith("✓") else ("text-danger" if msg.startswith("❌") else "text-warning")
        return ui.tags.p(msg, class_=f"{css} mt-2 small fw-bold")

    # ── Overview: severity value boxes ──────────────────────────────────
    @output
    @render.ui
    def diag_severity_cards():
        flat = diag_flat.get()
        if not flat:
            return ui.tags.p("No results yet.", class_="text-muted")
        counts = _count_severities(flat)
        sev_colours = {"ERROR": "danger", "WARNING": "warning", "INFO": "info", "OK": "success"}
        sev_icons   = {"ERROR": "bi-x-circle-fill", "WARNING": "bi-exclamation-triangle-fill",
                       "INFO": "bi-info-circle-fill", "OK": "bi-check-circle-fill"}
        cards = []
        for sev in ("ERROR", "WARNING", "INFO", "OK"):
            cards.append(
                ui.div(
                    ui.tags.div(
                        ui.tags.i(class_=f"bi {sev_icons[sev]} me-2"),
                        ui.tags.span(sev, class_="fw-bold"),
                        class_=f"text-{sev_colours[sev]} mb-1",
                    ),
                    ui.tags.h3(str(counts[sev]), class_=f"text-{sev_colours[sev]} mb-0"),
                    class_="text-center p-3 border rounded",
                )
            )
        return ui.layout_columns(*cards, col_widths=[3, 3, 3, 3])

    # ── Overview: per-box summary table ─────────────────────────────────
    @output
    @render.ui
    def diag_box_summary_table():
        results = diag_results.get()
        if results is None:
            return ui.tags.p("Run analysis to see per-box summary.", class_="text-muted")

        box_ids = sorted(k for k in results if k != "cross_box")
        rows_data = []
        for bid in box_ids:
            errs = warns = infos = oks = 0
            for ck, findings in results[bid].items():
                if isinstance(findings, dict):
                    oks += len(findings)
                    continue
                if not isinstance(findings, list):
                    continue
                for f in findings:
                    sev = f.get("severity", SEV_OK) if isinstance(f, dict) else SEV_OK
                    label = SEV_INV.get(sev, str(sev))
                    if label == "ERROR":
                        errs += 1
                    elif label == "WARNING":
                        warns += 1
                    elif label == "INFO":
                        infos += 1
                    else:
                        oks += 1
            rows_data.append({
                "Box": bid,
                "Type": BOX_TYPES.get(bid, "?"),
                "Errors": errs,
                "Warnings": warns,
                "Info": infos,
                "OK": oks,
                "Total": errs + warns + infos + oks,
            })

        df = pd.DataFrame(rows_data)
        # Build an HTML table with colour badges
        header = ui.tags.tr(
            *[ui.tags.th(c, class_="small") for c in df.columns]
        )
        body_rows = []
        for _, r in df.iterrows():
            cells = [
                ui.tags.td(str(r["Box"]), class_="small"),
                ui.tags.td(str(r["Type"]), class_="small"),
                ui.tags.td(
                    ui.tags.span(str(r["Errors"]),
                                 class_="badge bg-danger" if r["Errors"] > 0 else "badge bg-secondary"),
                    class_="small",
                ),
                ui.tags.td(
                    ui.tags.span(str(r["Warnings"]),
                                 class_="badge bg-warning text-dark" if r["Warnings"] > 0 else "badge bg-secondary"),
                    class_="small",
                ),
                ui.tags.td(str(r["Info"]), class_="small text-info"),
                ui.tags.td(str(r["OK"]), class_="small text-success"),
                ui.tags.td(str(r["Total"]), class_="small fw-bold"),
            ]
            body_rows.append(ui.tags.tr(*cells))

        return ui.tags.div(
            ui.tags.table(
                ui.tags.thead(header),
                ui.tags.tbody(*body_rows),
                class_="table table-sm table-hover table-dark",
            ),
            style="max-height: 450px; overflow-y: auto;",
        )

    # ── Detailed Results: update filter dropdowns ───────────────────────
    @reactive.effect
    def _update_filters():
        results = diag_results.get()
        if results is None:
            return
        box_choices = {"ALL": "All Boxes"}
        for bid in sorted(k for k in results if k != "cross_box"):
            box_choices[str(bid)] = f"Box {bid} ({BOX_TYPES.get(bid, '?')})"
        ui.update_select("diag_filter_box", choices=box_choices)

        check_choices = {"ALL": "All Checks"}
        check_choices.update(CHECK_DISPLAY)
        ui.update_select("diag_filter_check", choices=check_choices)

    # ── Detailed Results: findings data frame ───────────────────────────
    @output
    @render.data_frame
    def diag_findings_table():
        flat = diag_flat.get()
        if not flat:
            return pd.DataFrame({"Message": ["Run analysis first."]})

        df = pd.DataFrame(flat)

        # Apply filters
        box_filter = input.diag_filter_box()
        sev_filter = input.diag_filter_severity()
        chk_filter = input.diag_filter_check()

        if box_filter != "ALL":
            try:
                bval = int(box_filter)
                df = df[df["Box"] == bval]
            except ValueError:
                pass

        if sev_filter != "ALL":
            expected_sev = SEV_DISPLAY.get(sev_filter, sev_filter)
            df = df[df["Severity"] == expected_sev]

        if chk_filter != "ALL":
            display = CHECK_DISPLAY.get(chk_filter, chk_filter)
            df = df[df["Check"] == display]

        # Map severity codes to labels for display
        df = df.copy()
        df["Severity"] = df["Severity"].map(lambda s: SEV_INV.get(s, str(s)))

        return render.DataTable(df, height="500px", filters=True)

    # ── Visualisations ──────────────────────────────────────────────────
    @output
    @render.ui
    def diag_plot_severity():
        flat = diag_flat.get()
        if not flat:
            return ui.tags.p("No data.", class_="text-muted")
        counts = _count_severities(flat)
        fig = severity_bar_chart(counts)
        return ui.HTML(fig.to_html(full_html=False, include_plotlyjs="cdn"))

    @output
    @render.ui
    def diag_plot_per_check():
        results = diag_results.get()
        if results is None:
            return ui.tags.p("No data.", class_="text-muted")
        fig = findings_per_check_chart(results)
        return ui.HTML(fig.to_html(full_html=False, include_plotlyjs="cdn"))

    @output
    @render.ui
    def diag_plot_per_box():
        results = diag_results.get()
        if results is None:
            return ui.tags.p("No data.", class_="text-muted")
        fig = findings_per_box_chart(results)
        return ui.HTML(fig.to_html(full_html=False, include_plotlyjs="cdn"))

    @output
    @render.ui
    def diag_plot_heatmap():
        results = diag_results.get()
        if results is None:
            return ui.tags.p("No data.", class_="text-muted")
        fig = box_health_heatmap(results)
        return ui.HTML(fig.to_html(full_html=False, include_plotlyjs="cdn"))

    # ── Reports: PDF generation ─────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.diag_gen_results_pdf)
    def _gen_results_pdf():
        results = diag_results.get()
        if results is None:
            ui.notification_show("Run the analysis first.", type="warning", duration=3)
            return
        diag_pdf_msg.set("⏳ Generating results PDF …")

        def _work():
            try:
                gen_script = os.path.join(_tools_dir, "generate_analysis_results_pdf.py")
                if os.path.exists(gen_script):
                    import subprocess
                    subprocess.run(
                        [sys.executable, gen_script],
                        cwd=_parent_dir,
                        capture_output=True, timeout=60,
                    )
                    diag_pdf_msg.set("✓ Results PDF saved to docs/AQUABC_Analysis_Results_Report.pdf")
                else:
                    diag_pdf_msg.set("❌ PDF generator script not found.")
            except Exception as exc:
                diag_pdf_msg.set(f"❌ PDF generation failed: {exc}")

        threading.Thread(target=_work, daemon=True, name="PDFGenThread").start()

    @reactive.effect
    @reactive.event(input.diag_gen_deep_pdf)
    def _gen_deep_pdf():
        results = diag_results.get()
        if results is None:
            ui.notification_show("Run the analysis first.", type="warning", duration=3)
            return
        diag_pdf_msg.set("⏳ Generating deep analysis PDF …")

        def _work():
            try:
                gen_script = os.path.join(_tools_dir, "generate_deep_pr_pdf.py")
                if os.path.exists(gen_script):
                    import subprocess
                    subprocess.run(
                        [sys.executable, gen_script],
                        cwd=_parent_dir,
                        capture_output=True, timeout=120,
                    )
                    diag_pdf_msg.set("✓ Deep analysis PDF saved to docs/Deep_Process_Rate_Analysis_Report.pdf")
                else:
                    diag_pdf_msg.set("❌ PDF generator script not found.")
            except Exception as exc:
                diag_pdf_msg.set(f"❌ PDF generation failed: {exc}")

        threading.Thread(target=_work, daemon=True, name="PDFDeepThread").start()

    @output
    @render.ui
    def diag_pdf_status():
        reactive.invalidate_later(1.0)
        msg = diag_pdf_msg.get()
        if not msg:
            return ui.tags.p("Generate a PDF report after running the analysis.", class_="text-muted small")
        css = "text-success" if msg.startswith("✓") else ("text-danger" if msg.startswith("❌") else "text-warning")
        return ui.tags.p(msg, class_=f"{css} small fw-bold")
