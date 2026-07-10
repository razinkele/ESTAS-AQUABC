#!/usr/bin/env python3
"""
Generate a PDF document planning the integration of the Deep Process Rate
Analysis into the AQUABC Shiny UI framework, with guidance for automatic
outputs and user-facing controls.

Usage:
    python tools/generate_ui_integration_plan_pdf.py
"""

import sys, os
from datetime import datetime

try:
    from reportlab.lib.pagesizes import A4
    from reportlab.lib.units import mm
    from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
    from reportlab.lib.colors import HexColor, black, white
    from reportlab.platypus import (
        SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle,
        PageBreak, KeepTogether,
    )
    from reportlab.lib.enums import TA_LEFT, TA_CENTER
except ImportError:
    print("ERROR: reportlab required — pip install reportlab")
    sys.exit(1)

# ── Styles ────────────────────────────────────────────────────────────────
BLUE   = HexColor('#1a5276')
DARK   = HexColor('#2c3e50')
GREEN  = HexColor('#27ae60')
RED    = HexColor('#c0392b')
ORANGE = HexColor('#e67e22')
L_GRAY = HexColor('#ecf0f1')
M_GRAY = HexColor('#bdc3c7')
CODE_BG = HexColor('#f8f9fa')

styles = getSampleStyleSheet()
title_style  = ParagraphStyle('T',  parent=styles['Title'],    fontSize=18, textColor=BLUE, spaceAfter=6*mm)
h1_style     = ParagraphStyle('H1', parent=styles['Heading1'], fontSize=14, textColor=BLUE, spaceBefore=8*mm, spaceAfter=4*mm)
h2_style     = ParagraphStyle('H2', parent=styles['Heading2'], fontSize=12, textColor=DARK, spaceBefore=6*mm, spaceAfter=3*mm)
h3_style     = ParagraphStyle('H3', parent=styles['Heading3'], fontSize=10, textColor=DARK, spaceBefore=4*mm, spaceAfter=2*mm)
body_style   = ParagraphStyle('B',  parent=styles['BodyText'], fontSize=9,  leading=12, spaceAfter=2*mm)
small_style  = ParagraphStyle('S',  parent=styles['BodyText'], fontSize=7.5, leading=9.5, spaceAfter=1*mm)
cell_style   = ParagraphStyle('C',  fontSize=7.5, leading=9)
cell_bold    = ParagraphStyle('CB', fontSize=7.5, leading=9, textColor=BLUE)
code_style   = ParagraphStyle('Code', fontName='Courier', fontSize=7.5, leading=10, spaceAfter=2*mm)
bullet_style = ParagraphStyle('BL', parent=body_style, bulletFontSize=9,
                               bulletIndent=4*mm, leftIndent=10*mm)


def make_table(headers, rows, col_widths=None, font_size=7.5):
    hdr = [Paragraph(f'<b>{h}</b>', cell_bold) for h in headers]
    data = [hdr]
    for row in rows:
        data.append([Paragraph(str(c), cell_style) if isinstance(c, str) else c for c in row])
    t = Table(data, colWidths=col_widths, repeatRows=1)
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), BLUE),
        ('TEXTCOLOR',  (0, 0), (-1, 0), white),
        ('FONTSIZE',   (0, 0), (-1, -1), font_size),
        ('ALIGN',      (0, 0), (-1, 0), 'CENTER'),
        ('VALIGN',     (0, 0), (-1, -1), 'TOP'),
        ('GRID',       (0, 0), (-1, -1), 0.5, M_GRAY),
        ('ROWBACKGROUNDS', (0, 1), (-1, -1), [white, L_GRAY]),
        ('TOPPADDING',    (0, 0), (-1, -1), 2),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 2),
        ('LEFTPADDING',   (0, 0), (-1, -1), 3),
        ('RIGHTPADDING',  (0, 0), (-1, -1), 3),
    ]))
    return t


def sev_para(text, color):
    return Paragraph(f'<font color="{color}"><b>{text}</b></font>', cell_style)


def build_document(pdf_path):
    os.makedirs(os.path.dirname(pdf_path) or '.', exist_ok=True)
    doc = SimpleDocTemplate(
        pdf_path, pagesize=A4,
        leftMargin=15*mm, rightMargin=15*mm,
        topMargin=15*mm, bottomMargin=15*mm,
    )
    story = []

    # ══════════════════════════════════════════════════════════════════════
    # Title
    # ══════════════════════════════════════════════════════════════════════
    story.append(Paragraph(
        'AQUABC UI Integration Plan — Deep Process Rate Analysis', title_style))
    story.append(Paragraph(
        f'Generated: {datetime.now().strftime("%Y-%m-%d %H:%M")} &nbsp;|&nbsp; '
        f'ESTAS-AQUABC v0.3 &nbsp;|&nbsp; '
        f'Shiny for Python (PyShiny) Framework',
        small_style))
    story.append(Spacer(1, 6*mm))

    # ══════════════════════════════════════════════════════════════════════
    # 1. Executive Summary
    # ══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('1. Executive Summary', h1_style))
    story.append(Paragraph(
        'This document plans the integration of the AQUABC Deep Process Rate Analysis toolkit '
        'into the existing PyShiny web UI (shiny_app/app.py). The goal is to provide users with '
        '<b>automatic post-simulation diagnostics</b> — including health checks, interactive '
        'visualisations, and downloadable PDF reports — directly from the browser-based interface, '
        'with no command-line usage required.', body_style))

    story.append(Paragraph(
        'The integration adds a new <b>"Diagnostics"</b> navigation panel to the existing sidebar '
        '(alongside Dashboard, Plots, Mass Balance, etc.) and optionally triggers analysis '
        'automatically after each simulation run completes.', body_style))

    # ══════════════════════════════════════════════════════════════════════
    # 2. Current UI Architecture
    # ══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('2. Current UI Architecture', h1_style))
    story.append(Paragraph(
        'The AQUABC UI is built with <b>Shiny for Python</b> (PyShiny) and served via an R Shiny '
        'wrapper (app.R) that embeds the Python backend in an iframe. The app uses a custom sidebar '
        'navigation system with JavaScript-driven panel switching.', body_style))

    nav_items = [
        ('nav_dashboard', 'Dashboard', 'Model status overview'),
        ('nav_model_structure', 'Model Structure', 'State variable and process diagrams'),
        ('nav_model_build', 'Model Build', 'Compiler selection and build log'),
        ('nav_model_control', 'Model Config', 'Simulation config, run model, output config (3 tabs)'),
        ('nav_input_files', 'Input Files', 'File browser + map display'),
        ('nav_parameters', 'Parameters', 'Model parameter editor (WCONST_04.txt)'),
        ('nav_initial_conditions', 'Initial Cond.', 'State variable initial values'),
        ('nav_model_options', 'Model Options', 'Toggles and extra constants'),
        ('nav_scenarios', 'Scenarios', 'Multi-scenario management'),
        ('nav_plot', 'Plots', 'State vars, process rates, mass balance (4 tabs)'),
        ('nav_mass_balance', 'Mass Balance', 'Conservation analysis'),
        ('nav_observations', 'Observations', 'Model-observation comparison'),
        ('nav_map', 'Map', 'Geographic visualization'),
    ]
    story.append(make_table(
        ['Nav ID', 'Label', 'Description'],
        [[n[0], n[1], n[2]] for n in nav_items],
        col_widths=[120, 80, 260]))

    story.append(Spacer(1, 2*mm))
    story.append(Paragraph(
        '<b>Key architectural patterns:</b> '
        '(1) NAV_CHOICES dict defines sidebar entries with Bootstrap icons. '
        '(2) panel_conditional() shows/hides content based on navigation input. '
        '(3) Reactive values track simulation state, output directory, loaded data. '
        '(4) Long-running operations use threading (subprocess for model run). '
        '(5) UI is ~8,500 lines in a single app.py file.',
        body_style))

    # ══════════════════════════════════════════════════════════════════════
    # 3. Integration Architecture
    # ══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('3. Integration Architecture', h1_style))

    story.append(Paragraph('3.1 New Navigation Entry', h2_style))
    story.append(Paragraph(
        'Add a new entry to NAV_CHOICES in app.py:', body_style))
    story.append(Paragraph(
        '<font face="Courier" size="8">'
        '"nav_diagnostics": ("bi-shield-check", "Diagnostics"),</font>',
        code_style))
    story.append(Paragraph(
        'This places a "Diagnostics" item in the sidebar with a shield-check icon, '
        'positioned after "Mass Balance" and before "Observations".', body_style))

    story.append(Paragraph('3.2 Panel Structure', h2_style))
    story.append(Paragraph(
        'The Diagnostics panel uses a tabbed layout (navset_card_tab) with 4 sub-tabs:', body_style))

    tabs = [
        ('Overview', 'Summary dashboard with severity counts (ERROR/WARNING/INFO/OK), '
         'per-box health status indicators, and a quick-action "Run Analysis" button. '
         'Shows colour-coded cards: green (0 errors), amber (warnings present), red (errors found).'),
        ('Detailed Results', 'Per-check expandable sections showing findings tables. '
         'Users can filter by box, severity, and check number. Tables are sortable. '
         'Shows derivative consistency, stoichiometry, sign violations, smoothness, etc.'),
        ('Visualisations', 'Interactive Plotly charts: '
         '(a) spike timeline per variable, '
         '(b) limitation factor radar charts, '
         '(c) mass-balance bar charts, '
         '(d) cross-box heatmaps, '
         '(e) derivative consistency scatter plots. '
         'All reactive to box and variable selection.'),
        ('Reports', 'One-click PDF generation and download for: '
         '(a) Analysis Results Report, '
         '(b) Detailed Analysis Report (existing generate_deep_pr_pdf.py), '
         '(c) Crosscheck Report. '
         'Shows generation status and file sizes. '
         'Download buttons return the generated PDF files.'),
    ]
    story.append(make_table(
        ['Tab', 'Content'],
        [[t[0], t[1]] for t in tabs],
        col_widths=[70, 390]))

    story.append(Paragraph('3.3 Module Design', h2_style))
    story.append(Paragraph(
        'To avoid further bloating app.py (already 8,500 lines), the implementation will use '
        'a <b>separate module</b>:', body_style))

    modules = [
        ('shiny_app/diagnostics.py', '~400 lines',
         'Contains UI builder function (diagnostics_ui()), server logic function '
         '(diagnostics_server(input, output, session, rv)), and helper functions. '
         'Imported into app.py with a single import statement.'),
        ('shiny_app/diagnostics_plots.py', '~200 lines',
         'Plotly chart generators for the Visualisations tab. '
         'Pure functions taking analysis results dict and returning Plotly figures.'),
        ('tools/ (existing)', 'No changes',
         'The analysis engine (deep_process_rate_analysis.py) and PDF generators '
         'remain in tools/ and are imported by the diagnostics module.'),
    ]
    story.append(make_table(
        ['File', 'Est. Size', 'Responsibility'],
        [[m[0], m[1], m[2]] for m in modules],
        col_widths=[130, 55, 275]))

    # ══════════════════════════════════════════════════════════════════════
    # 4. Automatic Post-Run Analysis
    # ══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('4. Automatic Post-Run Analysis', h1_style))

    story.append(Paragraph(
        'The analysis can be triggered automatically when a simulation completes. '
        'This requires integration with the existing model run workflow in app.py.', body_style))

    story.append(Paragraph('4.1 Trigger Mechanism', h2_style))
    story.append(Paragraph(
        'The existing "Run Model" tab launches the Fortran executable via subprocess in a '
        'background thread. When the process completes (returncode 0), a reactive value '
        '<font face="Courier">rv_run_status</font> is set. The diagnostics module hooks into '
        'this reactive chain:', body_style))

    steps = [
        ('1', 'User clicks "Run Model"', 'Existing code launches ESTAS_II subprocess'),
        ('2', 'Subprocess completes (rc=0)', 'Existing code updates rv_run_status = "completed"'),
        ('3', 'Auto-analysis trigger', 'New @reactive.effect watches rv_run_status. '
         'If "completed" AND auto_diagnostics switch is ON, starts analysis in background thread.'),
        ('4', 'Analysis runs', 'Calls run_analysis(output_dir) from deep_process_rate_analysis. '
         'Progress updates via reactive value rv_diag_progress (0-100%).'),
        ('5', 'Results stored', 'Analysis results dict stored in rv_diag_results reactive value. '
         'UI automatically renders the Overview tab with updated severity counts.'),
        ('6', 'Notification', 'Toast notification: "Analysis complete: 0 ERRORS, N WARNINGS". '
         'If errors found, notification is red and suggests reviewing the Diagnostics panel.'),
    ]
    story.append(make_table(
        ['Step', 'Event', 'Action'],
        [[s[0], s[1], s[2]] for s in steps],
        col_widths=[25, 130, 305]))

    story.append(Paragraph('4.2 User Controls', h2_style))
    controls = [
        ('Auto-run diagnostics', 'Switch (ON/OFF)',
         'Toggle automatic analysis after simulation. Default: ON. '
         'When OFF, user must click "Run Analysis" manually.'),
        ('Analysis scope', 'Checkbox group',
         'Select which checks to run (default: all 16). Power users can disable '
         'slow checks (Check 2: derivative consistency) for faster turnaround.'),
        ('Box selection', 'Checkbox group',
         'Select which boxes to analyse. Synced with output config box selection.'),
        ('Output directory', 'Dropdown',
         'Defaults to current simulation output dir. Can select historical output dirs '
         'for retrospective analysis.'),
        ('Severity filter', 'Dropdown',
         'Filter displayed results: Show All, Errors Only, Warnings+, Errors+Warnings.'),
    ]
    story.append(make_table(
        ['Control', 'Widget', 'Behaviour'],
        [[c[0], c[1], c[2]] for c in controls],
        col_widths=[100, 80, 280]))

    # ══════════════════════════════════════════════════════════════════════
    # 5. UI Mockup
    # ══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('5. UI Component Specification', h1_style))

    story.append(Paragraph('5.1 Overview Tab', h2_style))
    story.append(Paragraph(
        'The Overview tab shows a grid of severity cards at the top, followed by a per-box '
        'summary table and a quick-action bar.', body_style))

    overview_comps = [
        ('Severity cards (4)', 'ui.value_box()',
         'Green/amber/red/blue cards showing counts for ERROR/WARNING/INFO/OK. '
         'Each card shows the count as a large number with a trend indicator '
         '(compared to previous run if available). Uses layout_columns(col_widths=[3,3,3,3]).'),
        ('Per-box health table', 'ui.output_data_frame()',
         'Table with columns: Box, Type (sand/mud), Errors, Warnings, Infos, Status. '
         'Status column shows a coloured badge (PASS/WARN/FAIL). Sortable by any column.'),
        ('Action bar', 'ui.layout_columns()',
         'Contains: "Run Analysis" button (btn-primary), auto-diagnostics switch, '
         'progress bar (shown during analysis), "Download All Reports" button.'),
        ('Last run info', 'ui.output_text()',
         'Shows: "Last analysis: 2026-02-27 14:30 | Duration: 12s | Output dir: OUTPUTS"'),
    ]
    story.append(make_table(
        ['Component', 'Shiny Widget', 'Specification'],
        [[c[0], c[1], c[2]] for c in overview_comps],
        col_widths=[90, 90, 280]))

    story.append(Paragraph('5.2 Detailed Results Tab', h2_style))
    detail_comps = [
        ('Filter bar', 'ui.layout_columns()',
         'Box selector (dropdown), severity filter (dropdown), check selector (dropdown). '
         'Changes update the findings table reactively.'),
        ('Findings table', 'ui.output_data_frame()',
         'Columns: Box, Check #, Check Name, Variable, Severity, Key Value, Note. '
         'Rows are coloured by severity. Sortable. Paginated (25 rows/page).'),
        ('Detail panel', 'ui.output_ui() (conditional)',
         'When a row is clicked, shows full finding details in an expandable panel: '
         'all dict keys as a key-value table, with float values formatted to 6 significant figures.'),
    ]
    story.append(make_table(
        ['Component', 'Widget', 'Spec'],
        [[c[0], c[1], c[2]] for c in detail_comps],
        col_widths=[90, 100, 270]))

    story.append(Paragraph('5.3 Visualisations Tab', h2_style))
    viz_comps = [
        ('Spike Timeline', 'Plotly scatter',
         'X = time (days), Y = |ΔC|, coloured by severity. One subplot per selected variable. '
         'Threshold line shown as horizontal dashed line. Hover shows timestep, value, spike count.'),
        ('Limitation Radar', 'Plotly polar',
         'One radar chart per phytoplankton group showing mean limitation factors '
         '(light, N, P, T, Si) as spokes. Values 0-1 scale. Selector for box.'),
        ('Mass Balance Bars', 'Plotly bar',
         'Grouped bar chart: N/P/C/O₂ budgets across boxes. Each bar shows mean dX/dt '
         'with error bars for min/max. Hover shows detailed breakdown.'),
        ('Spatial Heatmap', 'Plotly heatmap',
         'Boxes on X-axis, key variables on Y-axis. Colour = normalised mean concentration '
         'or CV across boxes. Helps identify spatial anomalies.'),
        ('Derivative Scatter', 'Plotly scatter',
         'X = kinetic dC/dt, Y = numerical dC/dt. One point per timestep, coloured by season. '
         'Perfect consistency = y=x line. Selector for variable and box.'),
    ]
    story.append(make_table(
        ['Chart', 'Type', 'Specification'],
        [[v[0], v[1], v[2]] for v in viz_comps],
        col_widths=[80, 60, 320]))

    story.append(Paragraph('5.4 Reports Tab', h2_style))
    report_comps = [
        ('Report selector', 'ui.input_checkbox_group()',
         'Select which reports to generate: Results Report, Detailed Analysis Report, '
         'Crosscheck Report. Multiple selection allowed.'),
        ('Generate button', 'ui.input_action_button()',
         '"Generate Selected Reports" — triggers PDF generation in background thread. '
         'Progress indicator shows current report being generated.'),
        ('Download area', 'ui.download_button() × 3',
         'One download button per report type. Disabled until PDF exists. '
         'Shows file size and generation timestamp.'),
        ('Preview area', 'ui.output_ui()',
         'Shows the most recently generated PDF in an iframe preview (if browser supports it). '
         'Fallback: "Download to view" message.'),
    ]
    story.append(make_table(
        ['Component', 'Widget', 'Spec'],
        [[r[0], r[1], r[2]] for r in report_comps],
        col_widths=[90, 120, 250]))

    # ══════════════════════════════════════════════════════════════════════
    # 6. Implementation Plan
    # ══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('6. Implementation Plan', h1_style))

    phases = [
        ('Phase 1: Core Integration', '2-3 days', [
            ('1.1', 'Create shiny_app/diagnostics.py with diagnostics_ui() and diagnostics_server()'),
            ('1.2', 'Add "nav_diagnostics" to NAV_CHOICES and wire panel_conditional in app.py'),
            ('1.3', 'Implement Overview tab: severity cards, per-box health table, run analysis button'),
            ('1.4', 'Implement background analysis execution with progress reactive value'),
            ('1.5', 'Test manual "Run Analysis" from the UI'),
        ]),
        ('Phase 2: Detailed Results & Reports', '2-3 days', [
            ('2.1', 'Implement Detailed Results tab with filter bar and findings table'),
            ('2.2', 'Implement Reports tab with PDF generation and download buttons'),
            ('2.3', 'Wire up existing generate_*_pdf.py scripts as background tasks'),
            ('2.4', 'Test PDF download flow end-to-end'),
        ]),
        ('Phase 3: Visualisations', '3-4 days', [
            ('3.1', 'Create shiny_app/diagnostics_plots.py with Plotly chart generators'),
            ('3.2', 'Implement spike timeline chart (most informative single visualisation)'),
            ('3.3', 'Implement limitation radar and mass-balance bar charts'),
            ('3.4', 'Implement spatial heatmap and derivative scatter'),
            ('3.5', 'Add reactive box/variable selectors for all charts'),
        ]),
        ('Phase 4: Auto-Run & Polish', '1-2 days', [
            ('4.1', 'Hook analysis into post-simulation completion trigger'),
            ('4.2', 'Add auto-diagnostics switch and toast notifications'),
            ('4.3', 'Cache analysis results to avoid re-running on tab switch'),
            ('4.4', 'Add scope selector (choose which checks to run)'),
            ('4.5', 'Documentation and user-facing help text'),
        ]),
    ]

    for phase_name, est, tasks in phases:
        story.append(Paragraph(f'<b>{phase_name}</b> (estimated: {est})', h2_style))
        rows = [[t[0], t[1]] for t in tasks]
        story.append(make_table(['#', 'Task'], rows, col_widths=[25, 435]))

    # ══════════════════════════════════════════════════════════════════════
    # 7. Technical Considerations
    # ══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('7. Technical Considerations', h1_style))

    story.append(Paragraph('7.1 Performance', h2_style))
    story.append(Paragraph(
        'The analysis runs in ~10-15 seconds for 7 boxes × 8,761 timesteps on a modern machine. '
        'This is acceptable for a one-time post-simulation check. For interactive use, consider:', body_style))
    perf = [
        ('Caching', 'Store results in a reactive value that persists across tab switches. '
         'Invalidate only when output directory changes or user clicks "Re-run".'),
        ('Progress', 'Update a progress bar reactive (0-100%) using box completion as increments '
         '(each box = 14.3%). Display "Analysing Box N of 7..." text.'),
        ('Thread safety', 'Run analysis in a background thread (matching existing model run pattern). '
         'Use reactive.isolate() and session.flush_reactive() to update UI safely.'),
        ('Lazy loading', 'Only load heavy data (process rate files) when user navigates to Diagnostics '
         'tab. Avoid auto-loading on app startup.'),
    ]
    story.append(make_table(
        ['Topic', 'Approach'],
        [[p[0], p[1]] for p in perf],
        col_widths=[80, 380]))

    story.append(Paragraph('7.2 Error Handling', h2_style))
    error_cases = [
        ('No output files', 'Show "No model output found. Run a simulation first." '
         'with a link to the Model Config → Run Model tab.'),
        ('No PROCESS_RATES files', 'Show "Process rate output not enabled. Go to Model Config → '
         'Output Config and check \'Process Rates\'." with a link.'),
        ('Analysis exception', 'Catch all exceptions in the background thread. '
         'Display the traceback in a collapsible error panel. Log to console.'),
        ('Partial results', 'If some boxes fail but others succeed, show partial results '
         'with a banner: "Analysis completed for N of M boxes. Box X failed: [reason]."'),
    ]
    story.append(make_table(
        ['Scenario', 'Handling'],
        [[e[0], e[1]] for e in error_cases],
        col_widths=[120, 340]))

    story.append(Paragraph('7.3 Integration Points in app.py', h2_style))
    integration_points = [
        ('NAV_CHOICES dict (line ~307)', 'Add "nav_diagnostics" entry after "nav_mass_balance"'),
        ('panel_conditional blocks (~line 1228+)', 'Add panel_diagnostics with conditional '
         'on input.navigation === "nav_diagnostics"'),
        ('Server function (~line 2600+)', 'Import and call diagnostics_server() passing '
         'input, output, session, and reactive values (rv_output_dir, rv_run_status)'),
        ('Post-run callback', 'In the model run completion handler, add conditional call '
         'to trigger auto-analysis if switch is ON'),
        ('Requirements.txt', 'Ensure plotly and reportlab are in shiny_app/requirements.txt '
         '(plotly is already present; reportlab may need to be added)'),
    ]
    story.append(make_table(
        ['Location', 'Change'],
        [[i[0], i[1]] for i in integration_points],
        col_widths=[160, 300]))

    # ══════════════════════════════════════════════════════════════════════
    # 8. Code Skeleton
    # ══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('8. Code Skeleton', h1_style))

    story.append(Paragraph('8.1 diagnostics.py — UI Builder', h2_style))
    story.append(Paragraph(
        '<font face="Courier" size="7">'
        'def diagnostics_ui():<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;"""Build the Diagnostics panel UI."""<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;return ui.panel_conditional(<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"input.navigation === \'nav_diagnostics\'",<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ui.navset_card_tab(<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ui.nav_panel("Overview", _overview_tab()),<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ui.nav_panel("Detailed Results", _details_tab()),<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ui.nav_panel("Visualisations", _viz_tab()),<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ui.nav_panel("Reports", _reports_tab()),<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;id="diagnostics_tabs"<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;)<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;)<br/>'
        '</font>', small_style))

    story.append(Paragraph('8.2 diagnostics.py — Server Logic', h2_style))
    story.append(Paragraph(
        '<font face="Courier" size="7">'
        'def diagnostics_server(input, output, session, rv):<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;"""Register all Diagnostics reactive/render functions."""<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;diag_results = reactive.Value(None)  # Cached results<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;diag_running = reactive.Value(False)<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;diag_progress = reactive.Value(0)<br/><br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;@reactive.effect<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;@reactive.event(input.btn_run_diagnostics)<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;def _run_diagnostics():<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;output_dir = rv.output_dir()<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;diag_running.set(True)<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Run in background thread<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;threading.Thread(target=_bg_analysis,<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args=(output_dir,)).start()<br/><br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;def _bg_analysis(output_dir):<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;from tools.deep_process_rate_analysis import run_analysis<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;results = run_analysis(output_dir)<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;diag_results.set(results)<br/>'
        '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;diag_running.set(False)<br/>'
        '</font>', small_style))

    story.append(Paragraph('8.3 app.py Integration', h2_style))
    story.append(Paragraph(
        '<font face="Courier" size="7">'
        '# In NAV_CHOICES (line ~320):<br/>'
        '"nav_diagnostics": ("bi-shield-check", "Diagnostics"),<br/><br/>'
        '# In app_ui function (after panel_mass_balance):<br/>'
        'from shiny_app.diagnostics import diagnostics_ui, diagnostics_server<br/>'
        'panel_diagnostics = diagnostics_ui()<br/><br/>'
        '# In the main layout (add to panels list):<br/>'
        'panel_diagnostics,<br/><br/>'
        '# In the server function:<br/>'
        'diagnostics_server(input, output, session, rv)<br/>'
        '</font>', small_style))

    # ══════════════════════════════════════════════════════════════════════
    # 9. Automatic Output Integration
    # ══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('9. Automatic Output Generation', h1_style))
    story.append(Paragraph(
        'Beyond interactive diagnostics, the system should automatically generate standardised '
        'analysis outputs after each simulation. These serve as permanent records for model '
        'quality assurance and are stored alongside the simulation outputs.', body_style))

    auto_outputs = [
        ('diagnostics_summary.txt', 'Text',
         'Auto-generated text summary (same as CLI output). Saved in the output directory '
         'alongside .out files. Includes timestamp, box count, error/warning counts.'),
        ('diagnostics_summary.json', 'JSON',
         'Machine-readable results dict. Enables programmatic querying of results, '
         'comparison between runs, and integration with external tools.'),
        ('AQUABC_Analysis_Results_Report.pdf', 'PDF',
         'Publication-quality results PDF. Generated on-demand from Reports tab or '
         'automatically if auto-generate-reports switch is ON.'),
        ('diagnostics_badge.html', 'HTML',
         'Small HTML badge showing "0 ERRORS | 55 WARNINGS" that can be displayed on the '
         'Dashboard panel as a status indicator.'),
    ]
    story.append(make_table(
        ['File', 'Format', 'Description'],
        [[a[0], a[1], a[2]] for a in auto_outputs],
        col_widths=[150, 35, 275]))

    story.append(Paragraph(
        '<b>Storage location:</b> All auto-generated diagnostic files are saved in a '
        '<font face="Courier">diagnostics/</font> subdirectory within the output directory '
        '(e.g., <font face="Courier">OUTPUTS/diagnostics/</font>). This keeps diagnostics '
        'associated with their simulation but separate from model output files.', body_style))

    # ══════════════════════════════════════════════════════════════════════
    # 10. User Guidance
    # ══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('10. User Guidance Integration', h1_style))
    story.append(Paragraph(
        'The UI should provide contextual help for non-expert users:', body_style))

    guidance = [
        ('Tooltips', 'Every control and severity badge has a tooltip explaining what it means. '
         'Tooltips use ui.tooltip() wrapper around each widget.'),
        ('Help panel', 'A collapsible "Understanding Diagnostics" panel at the top of the Overview tab. '
         'Contains: (a) What each severity level means, (b) What to do for each type of finding, '
         '(c) Links to the process rate slot reference and AQUABC manual.'),
        ('Contextual actions', 'When an ERROR or WARNING is shown, include suggested actions: '
         '"Check parameter X", "Verify initial condition Y", "See AQUABC Reference Manual §Z". '
         'These are generated from a recommendations dict keyed by check number.'),
        ('Comparison mode', 'If multiple output directories exist (e.g., OUTPUTS_30day, OUTPUTS_365day), '
         'allow side-by-side comparison of diagnostic results to track improvement over time.'),
    ]
    story.append(make_table(
        ['Feature', 'Description'],
        [[g[0], g[1]] for g in guidance],
        col_widths=[80, 380]))

    # ══════════════════════════════════════════════════════════════════════
    # 11. Testing Strategy
    # ══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('11. Testing Strategy', h1_style))

    tests = [
        ('Unit tests', 'tests/test_diagnostics.py',
         'Test diagnostics_ui() renders without errors. Test severity counting function. '
         'Test result filtering logic. Mock run_analysis() for fast tests.'),
        ('Integration tests', 'tests/test_diagnostics_integration.py',
         'Test full pipeline: load real output files → run analysis → verify results dict structure. '
         'Test PDF generation produces valid files. Requires OUTPUTS/ directory.'),
        ('UI tests', 'Manual / Playwright',
         'Navigate to Diagnostics tab. Click Run Analysis. Verify tables populate. '
         'Download PDF. Check toast notifications. Test with no output files (error state).'),
        ('Regression', 'tests/test_analysis_regression.py',
         'Golden-file comparison: run analysis on OUTPUTS/ and compare error/warning counts '
         'against expected values (currently 0 ERR, 55 WARN). Fails if model changes introduce regressions.'),
    ]
    story.append(make_table(
        ['Type', 'Location', 'Coverage'],
        [[t[0], t[1], t[2]] for t in tests],
        col_widths=[70, 160, 230]))

    # ══════════════════════════════════════════════════════════════════════
    # 12. Summary & Next Steps
    # ══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('12. Summary & Next Steps', h1_style))

    story.append(Paragraph(
        'This plan provides a complete roadmap for integrating the analysis toolkit into the '
        'AQUABC UI. The 4-phase approach allows incremental delivery with testable milestones:',
        body_style))

    summary = [
        ('Phase 1', '2-3 days', 'Core panel + Run Analysis button + Overview tab',
         'Users can trigger analysis from the UI and see severity counts'),
        ('Phase 2', '2-3 days', 'Detailed results + PDF reports',
         'Full findings browsing and downloadable reports'),
        ('Phase 3', '3-4 days', 'Interactive visualisations',
         'Plotly charts for spike timelines, limitation factors, mass balance'),
        ('Phase 4', '1-2 days', 'Auto-run + polish',
         'Automatic post-simulation diagnostics with notifications'),
    ]
    story.append(make_table(
        ['Phase', 'Estimate', 'Deliverable', 'User Value'],
        [[s[0], s[1], s[2], s[3]] for s in summary],
        col_widths=[40, 50, 200, 170]))

    story.append(Spacer(1, 4*mm))
    story.append(Paragraph(
        '<b>Total estimated effort:</b> 8-12 developer-days for complete implementation. '
        'Phase 1 alone delivers 80% of the user value (ability to run analysis from UI).', body_style))

    story.append(Spacer(1, 3*mm))
    story.append(Paragraph(
        '<b>Immediate next steps:</b>', body_style))
    next_steps = [
        '1. Create shiny_app/diagnostics.py skeleton with UI and server functions',
        '2. Add nav_diagnostics to NAV_CHOICES and test sidebar navigation',
        '3. Implement Overview tab with hardcoded sample data to validate layout',
        '4. Wire up run_analysis() background execution and populate real results',
        '5. Implement Reports tab for PDF download (quick win — reuses existing generators)',
    ]
    for step in next_steps:
        story.append(Paragraph(step, bullet_style))

    doc.build(story)
    return pdf_path


def main():
    pdf_path = 'docs/AQUABC_UI_Integration_Plan.pdf'
    print(f"Generating UI integration plan PDF: {pdf_path}")
    build_document(pdf_path)
    fsize = os.path.getsize(pdf_path) / 1024
    print(f"PDF generated: {pdf_path}")
    print(f"File size: {fsize:.0f} KB")


if __name__ == '__main__':
    main()
