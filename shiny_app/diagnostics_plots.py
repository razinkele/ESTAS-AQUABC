#!/usr/bin/env python3
"""
Plotly chart generators for the AQUABC Diagnostics panel.

Each function returns a ``plotly.graph_objects.Figure`` ready for
``shinywidgets.render_widget`` or plain ``render.plot``.
"""

import numpy as np
import plotly.graph_objects as go

# ── colour palette ──────────────────────────────────────────────────────────
SEV_COLOURS = {
    "ERROR": "#dc3545",
    "WARNING": "#ffc107",
    "INFO": "#17a2b8",
    "OK": "#28a745",
}

CHECK_NAMES = [
    "Rate stats", "Deriv. consist.", "Cross-var.", "Bug-fix",
    "Dominant proc.", "Seasonal", "Zero slots", "Limitations",
    "Negatives", "NaN/Inf", "Stoichiometry", "Sign violations",
    "Euler integ.", "Allelopathy", "Smoothness", "Mass balance",
]


# ═════════════════════════════════════════════════════════════════════════════
# 1. Severity bar chart — counts per severity across all boxes
# ═════════════════════════════════════════════════════════════════════════════
def severity_bar_chart(summary_counts):
    """
    Parameters
    ----------
    summary_counts : dict
        ``{"ERROR": int, "WARNING": int, "INFO": int, "OK": int}``
    """
    labels = list(summary_counts.keys())
    values = list(summary_counts.values())
    colours = [SEV_COLOURS.get(l, "#6c757d") for l in labels]

    fig = go.Figure(go.Bar(
        x=labels, y=values,
        marker_color=colours,
        text=values, textposition="auto",
    ))
    fig.update_layout(
        title="Finding Severity Distribution",
        xaxis_title="Severity",
        yaxis_title="Count",
        template="plotly_dark",
        height=350,
        margin=dict(l=40, r=20, t=50, b=40),
    )
    return fig


# ═════════════════════════════════════════════════════════════════════════════
# 2. Per-box health heatmap
# ═════════════════════════════════════════════════════════════════════════════
def box_health_heatmap(all_results):
    """Heatmap: rows = check names, columns = box IDs.

    Cell value encodes worst severity found for that check in that box:
      3 = ERROR, 2 = WARNING, 1 = INFO, 0 = OK / no findings.
    """
    sev_rank = {"ERROR": 3, "WARNING": 2, "INFO": 1, "OK": 0}
    box_ids = sorted(k for k in all_results if k != "cross_box")

    check_keys = [
        "rate_statistics", "derivative_consistency", "cross_variable",
        "bug_fixes", "dominant_processes", "seasonal_patterns",
        "zero_slots", "limitation_factors", "negatives", "nan_inf",
        "stoichiometry", "sign_violations", "euler_integration",
        "allelopathy", "smoothness", "mass_balance",
    ]

    z = np.zeros((len(check_keys), len(box_ids)), dtype=int)

    for j, bid in enumerate(box_ids):
        box_res = all_results.get(bid, {})
        for i, ck in enumerate(check_keys):
            findings = box_res.get(ck, [])
            if isinstance(findings, dict):
                # Some checks return dicts rather than lists — skip
                continue
            worst = 0
            for f in findings:
                sev = f.get("severity", "OK") if isinstance(f, dict) else "OK"
                worst = max(worst, sev_rank.get(sev, 0))
            z[i, j] = worst

    # Custom colour scale: 0=green, 1=cyan, 2=yellow, 3=red
    colorscale = [
        [0.0, "#28a745"], [0.33, "#17a2b8"],
        [0.66, "#ffc107"], [1.0, "#dc3545"],
    ]

    fig = go.Figure(go.Heatmap(
        z=z,
        x=[str(b) for b in box_ids],
        y=CHECK_NAMES[:len(check_keys)],
        colorscale=colorscale,
        zmin=0, zmax=3,
        colorbar=dict(
            tickvals=[0, 1, 2, 3],
            ticktext=["OK", "INFO", "WARN", "ERROR"],
        ),
        hovertemplate="Box %{x}<br>Check: %{y}<br>Severity: %{z}<extra></extra>",
    ))
    fig.update_layout(
        title="Per-Box Health Matrix",
        xaxis_title="Box ID",
        yaxis_title="Check",
        template="plotly_dark",
        height=500,
        margin=dict(l=140, r=20, t=50, b=40),
    )
    return fig


# ═════════════════════════════════════════════════════════════════════════════
# 3. Findings-per-check bar chart
# ═════════════════════════════════════════════════════════════════════════════
def findings_per_check_chart(all_results):
    """Stacked bar chart: x = check name, colour = severity."""
    check_keys = [
        "rate_statistics", "derivative_consistency", "cross_variable",
        "bug_fixes", "dominant_processes", "seasonal_patterns",
        "zero_slots", "limitation_factors", "negatives", "nan_inf",
        "stoichiometry", "sign_violations", "euler_integration",
        "allelopathy", "smoothness", "mass_balance",
    ]
    sev_labels = ["ERROR", "WARNING", "INFO", "OK"]
    counts = {s: [] for s in sev_labels}

    box_ids = sorted(k for k in all_results if k != "cross_box")

    for ck in check_keys:
        per_sev = {s: 0 for s in sev_labels}
        for bid in box_ids:
            findings = all_results.get(bid, {}).get(ck, [])
            if isinstance(findings, dict):
                continue
            for f in findings:
                sev = f.get("severity", "OK") if isinstance(f, dict) else "OK"
                if sev in per_sev:
                    per_sev[sev] += 1
        for s in sev_labels:
            counts[s].append(per_sev[s])

    fig = go.Figure()
    for s in sev_labels:
        fig.add_trace(go.Bar(
            name=s, x=CHECK_NAMES[:len(check_keys)], y=counts[s],
            marker_color=SEV_COLOURS[s],
        ))
    fig.update_layout(
        barmode="stack",
        title="Findings per Check (all boxes)",
        xaxis_title="Check", yaxis_title="Finding count",
        template="plotly_dark",
        height=400,
        margin=dict(l=40, r=20, t=50, b=100),
        xaxis_tickangle=-45,
        legend=dict(orientation="h", y=1.12),
    )
    return fig


# ═════════════════════════════════════════════════════════════════════════════
# 4. Findings-per-box bar chart
# ═════════════════════════════════════════════════════════════════════════════
def findings_per_box_chart(all_results):
    """Stacked bar chart: x = box ID, colour = severity."""
    sev_labels = ["ERROR", "WARNING", "INFO", "OK"]
    box_ids = sorted(k for k in all_results if k != "cross_box")
    counts = {s: [] for s in sev_labels}

    for bid in box_ids:
        per_sev = {s: 0 for s in sev_labels}
        for _ck, findings in all_results.get(bid, {}).items():
            if isinstance(findings, dict):
                continue
            for f in findings:
                sev = f.get("severity", "OK") if isinstance(f, dict) else "OK"
                if sev in per_sev:
                    per_sev[sev] += 1
        for s in sev_labels:
            counts[s].append(per_sev[s])

    fig = go.Figure()
    for s in sev_labels:
        fig.add_trace(go.Bar(
            name=s, x=[str(b) for b in box_ids], y=counts[s],
            marker_color=SEV_COLOURS[s],
        ))
    fig.update_layout(
        barmode="stack",
        title="Findings per Box",
        xaxis_title="Box ID", yaxis_title="Finding count",
        template="plotly_dark",
        height=400,
        margin=dict(l=40, r=20, t=50, b=40),
        legend=dict(orientation="h", y=1.12),
    )
    return fig


# ═════════════════════════════════════════════════════════════════════════════
# 5. Limitation radar — per box
# ═════════════════════════════════════════════════════════════════════════════
def limitation_radar(all_results, box_id):
    """Radar chart of mean limitation factors for a single box.

    Uses data from check_8 (limitation_factors) if it returned a
    ``mean_limitations`` dict, otherwise returns an empty figure.
    """
    lim_data = all_results.get(box_id, {}).get("limitation_factors", [])
    # check_8 returns a list of findings; each finding may have e.g. 'mean' key
    # We aggregate mean limitation for each category
    cats, vals = [], []
    for f in lim_data:
        if isinstance(f, dict) and "slot_name" in f:
            cats.append(f["slot_name"])
            vals.append(f.get("mean", 0))

    if not cats:
        fig = go.Figure()
        fig.update_layout(
            title=f"Box {box_id} — no limitation data",
            template="plotly_dark", height=350,
        )
        return fig

    # Close the polygon
    cats_closed = cats + [cats[0]]
    vals_closed = vals + [vals[0]]

    fig = go.Figure(go.Scatterpolar(
        r=vals_closed, theta=cats_closed,
        fill="toself", fillcolor="rgba(23,162,184,0.3)",
        line_color="#17a2b8",
    ))
    fig.update_layout(
        polar=dict(radialaxis=dict(visible=True, range=[0, max(vals) * 1.1 + 0.01])),
        title=f"Limitation Factors — Box {box_id}",
        template="plotly_dark",
        height=400,
        margin=dict(l=60, r=60, t=50, b=40),
    )
    return fig
