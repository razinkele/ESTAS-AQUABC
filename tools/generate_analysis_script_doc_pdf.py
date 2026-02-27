#!/usr/bin/env python3
"""
Generate a PDF document describing the AQUABC Deep Process Rate Analysis
**script architecture** — how it works, what each check does, how to use it,
and how the shared utilities and PDF generators are structured.

Usage:
    python tools/generate_analysis_script_doc_pdf.py
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
        PageBreak, KeepTogether, Preformatted,
    )
    from reportlab.lib.enums import TA_LEFT, TA_CENTER
except ImportError:
    print("ERROR: reportlab required — pip install reportlab")
    sys.exit(1)

# ── Styles ────────────────────────────────────────────────────────────────
BLUE   = HexColor('#1a5276')
DARK   = HexColor('#2c3e50')
GREEN  = HexColor('#27ae60')
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
code_style   = ParagraphStyle('Code', fontName='Courier', fontSize=7.5, leading=10, spaceAfter=2*mm,
                               backColor=CODE_BG, borderWidth=0.5, borderColor=M_GRAY,
                               borderPadding=4, leftIndent=8*mm)
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


def build_document(pdf_path):
    os.makedirs(os.path.dirname(pdf_path) or '.', exist_ok=True)
    doc = SimpleDocTemplate(
        pdf_path, pagesize=A4,
        leftMargin=15*mm, rightMargin=15*mm,
        topMargin=15*mm, bottomMargin=15*mm,
    )
    story = []

    # ── Title ──
    story.append(Paragraph('AQUABC Deep Process Rate Analysis — Script Documentation', title_style))
    story.append(Paragraph(
        f'Generated: {datetime.now().strftime("%Y-%m-%d %H:%M")} &nbsp;|&nbsp; '
        f'Version: 2.0 &nbsp;|&nbsp; '
        f'Python 3.8+ &nbsp;|&nbsp; '
        f'Dependencies: numpy, reportlab',
        small_style))
    story.append(Spacer(1, 6*mm))

    # ═══════════════════════════════════════════════════════════════════════
    # 1. Overview
    # ═══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('1. Overview', h1_style))
    story.append(Paragraph(
        'The AQUABC Deep Process Rate Analysis toolkit is a suite of Python scripts that '
        'provides comprehensive post-simulation quality assurance for the ESTAS-AQUABC '
        'ecological water quality model. It reads model output files (state variables and '
        'process rates), performs 16 systematic checks, and generates structured text reports '
        'and publication-quality PDF documents.', body_style))

    story.append(Paragraph(
        'The toolkit validates numerical correctness (NaN/Inf, sign violations, Euler integration), '
        'biogeochemical consistency (stoichiometric ratios, mass-balance closure, cross-variable coupling), '
        'and ecological plausibility (seasonal patterns, limitation factors, spatial coherence).', body_style))

    # ═══════════════════════════════════════════════════════════════════════
    # 2. File Architecture
    # ═══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('2. File Architecture', h1_style))

    files = [
        ('aquabc_analysis_utils.py', 'Shared utilities',
         'Constants (state variable names, box IDs, stoichiometric ratios), data loading functions '
         '(load_process_rates, load_state_vars), helper functions (get_slot_col, compute_kinetic_deriv, '
         'compute_numerical_derivative). Imported by all other scripts.'),
        ('process_rate_slot_map.py', 'Slot mapping',
         'Complete mapping of all 1,080 process rate slots to their Fortran descriptions, derivative '
         'formulas, and sign conventions. Extracted from aquabc_II_pelagic_model.f90. Contains SLOT_MAP '
         'dict (36 state variables × 30 slots) and DERIVATIVE_SIGNS dict.'),
        ('deep_process_rate_analysis.py', 'Main analysis',
         '16-check analysis engine. Contains check_1 through check_16 functions plus '
         'check_cross_box_spatial. run_analysis() orchestrates all checks across all boxes. '
         'print_summary() generates structured text output. Can run standalone via CLI.'),
        ('deep_state_vs_process_crosscheck.py', 'Cross-check script',
         '10-check independent crosscheck focusing on state-vs-process consistency. Overlaps '
         'with checks 9-16 of the main analysis but uses different thresholds and detection algorithms '
         'for independent verification.'),
        ('generate_deep_pr_pdf.py', 'PDF generator (main)',
         'Generates comprehensive PDF report from the 16-check analysis. Includes executive summary, '
         'per-check detailed tables, findings, and recommendations. Uses reportlab.'),
        ('generate_crosscheck_pdf.py', 'PDF generator (crosscheck)',
         'Generates PDF report for the 10-check crosscheck analysis.'),
        ('generate_analysis_results_pdf.py', 'PDF generator (results)',
         'Generates focused results report documenting what was found, what was fixed, and '
         'what remains. Emphasizes interpretation over raw data.'),
    ]
    rows = [[f[0], f[1], f[2]] for f in files]
    story.append(make_table(['File', 'Role', 'Description'], rows, col_widths=[130, 70, 260]))

    story.append(Spacer(1, 3*mm))
    story.append(Paragraph('<b>Dependency graph:</b>', body_style))
    story.append(Paragraph(
        'All scripts import from <font face="Courier">aquabc_analysis_utils</font> and '
        '<font face="Courier">process_rate_slot_map</font>. PDF generators import from '
        'their respective analysis scripts. No circular dependencies exist.',
        body_style))

    # ═══════════════════════════════════════════════════════════════════════
    # 3. Data Flow
    # ═══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('3. Data Flow', h1_style))

    story.append(Paragraph(
        '<b>Input files</b> (per box, in OUTPUTS/ directory):', body_style))

    inputs = [
        ('PELAGIC_BOX_XXXXX.out', 'State variables',
         'Header line + N rows × 37 columns (TIME + 36 state vars). Daily output.'),
        ('PELAGIC_BOX_XXXXX_PROCESS_RATES.out', 'Process rates',
         'N rows × 1081 columns (TIME + 36 vars × 30 slots = 1080). Daily output.'),
    ]
    story.append(make_table(['File Pattern', 'Content', 'Format'], inputs, col_widths=[160, 80, 220]))

    story.append(Spacer(1, 3*mm))
    story.append(Paragraph('<b>Processing pipeline:</b>', body_style))
    steps = [
        ('1', 'Load', 'numpy.loadtxt reads state and process rate files for each box'),
        ('2', 'Validate', 'Check for NaN, Inf, unexpected dimensions'),
        ('3', 'Compute derivatives', 'Kinetic dC/dt from signed process rate sums; '
         'numerical dC/dt from concentration forward differences'),
        ('4', 'Run 16 checks', 'Each check returns a list of finding dicts with severity, '
         'variable name, and diagnostic values'),
        ('5', 'Aggregate', 'Collect per-box results; run cross-box spatial comparison'),
        ('6', 'Report', 'Print structured text summary; optionally generate PDF'),
    ]
    story.append(make_table(['Step', 'Phase', 'Description'], steps, col_widths=[25, 60, 375]))

    # ═══════════════════════════════════════════════════════════════════════
    # 4. Check Details
    # ═══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('4. Detailed Check Descriptions', h1_style))

    checks = [
        ('Check 1: Rate Statistics', 'check_1_rate_statistics(time, rates, box_id)',
         'Computes min, max, mean, std, percent-nonzero for each of the 1,080 process rate slots. '
         'Returns a list of dicts keyed by (var_name, slot_number). Severity is always INFO — '
         'this is a data exploration check.',
         'KEY_VARS subset, all 30 slots per variable'),

        ('Check 2: Derivative Consistency', 'check_2_derivative_consistency(time, rates, sv_time, sv_concs, sv_names, box_id)',
         'For each key state variable: (a) sums signed process rates to get kinetic dC/dt, '
         '(b) computes numerical dC/dt from ΔC/Δt, (c) computes Pearson correlation, RMSE, '
         '(d) decomposes into %kinetic vs %transport contribution. A low correlation indicates '
         'transport dominance (expected for well-mixed variables like INORG_C).',
         'KEY_VARS, correlation > 0.1 threshold for inclusion'),

        ('Check 3: Cross-Variable Consistency', 'check_3_cross_variable_consistency(time, rates, box_id)',
         'Fifteen+ sub-checks validating inter-variable relationships: '
         '(a) O₂ reaeration sign and magnitude, '
         '(b) DIA/CYN/OPA/FIX_CYN/NOST DOC excretion = sum of individual phyto excretions, '
         '(c) NH₄ production from respiration ≈ growth × N:C, '
         '(d) ZOO feeding = sum of prey grazing rates, '
         '(e) DET production = sum of death + non-grazed feeding, '
         'and more. Uses both absolute tolerances and relative percentage thresholds.',
         'Tolerances: DOC excretion max(1e-5, 1e-4 × max_total); checks may be INACTIVE when '
         'underlying rates are zero'),

        ('Check 4: Bug-Fix Verification', 'check_4_bug_fix_verification(time, rates, box_id)',
         'Verifies that previously-identified bugs remain fixed: '
         '(a) FIX_CYN O₂ production (slot 19) is non-zero when FIX_CYN is active, '
         '(b) N-fixation rates for FIX_CYN and NOST are populated.',
         'Specific slots hardcoded from FORTRAN audit'),

        ('Check 5: Dominant Processes', 'check_5_dominant_processes(time, rates, box_id)',
         'For each key variable, ranks all contributing process rate slots by mean absolute '
         'magnitude and reports the top 3 slots with their percentage contribution to total flux. '
         'Helps identify which biogeochemical pathways drive each variable.',
         'Uses DERIVATIVE_SIGNS to select only slots that contribute to dC/dt'),

        ('Check 6: Seasonal Patterns', 'check_6_seasonal_patterns(time, rates, box_id)',
         'Computes seasonal means (DJF/MAM/JJA/SON) for key process rates (DIA growth, '
         'DIA respiration, ZOO feeding, nitrification, O₂ reaeration). Uses day-of-year '
         'from the time column (assuming 365-day years).',
         'Reports are INFO level; no automated anomaly detection'),

        ('Check 7: Zero-Slot Analysis', 'check_7_zero_slot_analysis(rates, box_id)',
         'Scans all NSTATE×NDIAGVAR slots for all-zero columns. Flags slots that (a) contribute '
         'to the derivative formula AND (b) are not expected to be zero given the model configuration '
         '(e.g., ZOO grazing on CYN being zero suggests low prey availability). '
         'Expected-zero slots (disabled features, AUX diagnostics) are marked as such.',
         'Uses SLOT_MAP derivative_slots_used to identify derivative-contributing slots'),

        ('Check 8: Limitation Factors', 'check_8_limitation_factors(rates, box_id)',
         'Extracts phytoplankton limitation factor slots (light, N, P, temperature, Si) '
         'from process rate AUX slots and reports their time-averaged min/mean/max. '
         'Values near 0 = strong limitation, 1.0 = no limitation. Covers DIA, CYN, OPA, '
         'FIX_CYN, NOST groups.',
         'Slot locations are hardcoded from SLOT_MAP AUX entries'),

        ('Check 9: Negative Concentrations', 'check_9_negative_concentrations(sv_time, sv_concs, sv_names, box_id)',
         'Checks all variables in NON_NEGATIVE_VARS for any negative values. Reports count, '
         'percentage, minimum value, and the timestep of first occurrence. Also checks '
         'dissolved oxygen saturation upper bound.',
         'ERROR if any negatives; WARNING if super-saturation detected'),

        ('Check 10: NaN/Inf Detection', 'check_10_nan_inf(sv_concs, sv_names, pr_rates, box_id)',
         'Scans all state variable columns and the bulk process rate matrix for NaN and ±Inf values. '
         'Reports per-variable counts.',
         'ERROR severity for any detection'),

        ('Check 11: Stoichiometric Ratios', 'check_11_stoichiometry(sv_concs, sv_names, pr_rates, box_id)',
         'Validates organism and detritus C:N:P ratios: ZOO N:C (expected ~0.22), '
         'ZOO P:C (~0.024), DET N:C, DET P:C, DOM N:C, DOM P:C. Also checks DIA growth '
         'O₂:C production ratio and ZOO respiratory N:C ratio. '
         'Uses median (robust to outliers) for main ratios.',
         'Thresholds: ±50% for ZOO, INFO for dynamic DET/DOM ratios. '
         'DET P:C classified as INFO (expected to be ≪ Redfield)'),

        ('Check 12: Sign Violations', 'check_12_sign_violations(pr_rates, box_id)',
         'Checks all process rate slots listed in NONNEG_RATE_SLOTS for any negative values. '
         'This includes phytoplankton growth, respiration, excretion, death, ZOO feeding, '
         'akinete dynamics, and nitrification. Reports count, minimum value, and affected timesteps.',
         'WARNING if any negatives detected (previously found 18 violations from '
         'negative light limitation at dusk; now resolved)'),

        ('Check 13: Euler Integration', 'check_13_euler_integration(sv_time, sv_concs, sv_names, pr_rates, box_id)',
         'Reconstructs ΔC using forward Euler from kinetic dC/dt and compares to actual ΔC. '
         'The difference = transport flux. Reports %kinetic, %transport, correlation, '
         'and mean signed transport residual for each key variable.',
         'INFO level; transport-dominated variables (INORG_C, TOT_ALK) expected to have low %kinetic'),

        ('Check 14: Allelopathy', 'check_14_allelopathy(sv_concs, sv_names, pr_rates, box_id)',
         'Checks secondary metabolite state variables (SEC_METAB_*) for: '
         '(a) always-zero concentrations (suggests feature may be disabled), '
         '(b) negative values, (c) correlation with parent phytoplankton biomass. '
         'Also reports process rate magnitudes for SEC_METAB variables.',
         'Current config: all allelopathy SEC_METAB slots are zero (feature inactive)'),

        ('Check 15: Smoothness / Spikes', 'check_15_smoothness(sv_time, sv_concs, sv_names, box_id)',
         'For each key variable, computes day-to-day absolute changes |ΔC|. A "spike" is a '
         'timestep where |ΔC| > threshold, where threshold = max(10 × mean|ΔC|, min_spike_size) '
         'and min_spike_size = max(1e-10, 0.001 × dynamic_range). '
         'Severity: WARNING if n_spikes > 10 AND spike magnitude > 1% of mean concentration; '
         'otherwise INFO.',
         'Also detects monotonic trends (>80% same direction = persistent drift)'),

        ('Check 16: Mass Balance', 'check_16_mass_balance(pr_rates, box_id)',
         'Computes total kinetic dN/dt, dP/dt, dOrgC/dt, d(AllC)/dt, and dO₂/dt by summing '
         'all process rate contributions across all state variables for each element. '
         'A perfectly closed model would give zero total kinetic flux. Non-zero values indicate '
         'external exchange (atmosphere, sediment, rivers) or numerical error.',
         'Uses default stoichiometric ratios to convert phytoplankton C to N/P equivalents'),
    ]

    for check_name, func_sig, desc, notes in checks:
        story.append(Paragraph(check_name, h2_style))
        story.append(Paragraph(f'<font face="Courier" size="8">{func_sig}</font>', small_style))
        story.append(Paragraph(desc, body_style))
        story.append(Paragraph(f'<i>Implementation notes:</i> {notes}', small_style))

    # ═══════════════════════════════════════════════════════════════════════
    # 5. Shared Utilities
    # ═══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('5. Shared Utilities (aquabc_analysis_utils.py)', h1_style))

    story.append(Paragraph('<b>Constants:</b>', h2_style))
    consts = [
        ('NDIAGVAR', '30', 'Number of diagnostic/process rate slots per variable'),
        ('NSTATE', '32', 'Core state variables'),
        ('NUM_ALLELOPATHY', '4', 'Allelopathy secondary metabolite variables'),
        ('NSTATE_TOTAL', '36', 'Total state variables (32 + 4)'),
        ('NUM_PROCESS_RATES', '1080', '36 × 30 process rate slots per box'),
        ('BOX_IDS', '[5,6,8,9,14,17,25]', 'Boxes producing output'),
        ('BOX_TYPES', '{5:sand, ..., 25:mud}', 'Sand vs mud classification'),
        ('DEFAULT_N_TO_C', '0.220', 'Default N:C ratio (mg N / mg C)'),
        ('DEFAULT_P_TO_C', '0.024', 'Default P:C ratio (mg P / mg C)'),
        ('DEFAULT_O2_TO_C', '2.66', 'Default O₂:C ratio'),
        ('DEFAULT_Si_TO_C', '0.25', 'Default Si:C ratio (diatoms)'),
    ]
    story.append(make_table(['Name', 'Value', 'Description'], consts, col_widths=[110, 100, 250]))

    story.append(Paragraph('<b>Functions:</b>', h2_style))
    funcs = [
        ('get_slot_col(var_idx, slot)', 'Returns 0-based column index in the process rate array'),
        ('load_process_rates(dir, box)', 'Loads PROCESS_RATES.out file; returns (time, rates_2d)'),
        ('load_state_vars(dir, box)', 'Loads PELAGIC_BOX.out file; returns (time, concs, header_names)'),
        ('compute_kinetic_deriv(rates, var, slot_map, signs)', 'Sums signed process rates for dC/dt'),
        ('compute_numerical_derivative(time, conc)', 'Forward differences ΔC/Δt'),
        ('find_sv_column(var_name, sv_names)', 'Case-insensitive column lookup in state var headers'),
    ]
    story.append(make_table(['Function', 'Description'], funcs, col_widths=[200, 260]))

    # ═══════════════════════════════════════════════════════════════════════
    # 6. Process Rate Slot Map
    # ═══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('6. Process Rate Slot Map (process_rate_slot_map.py)', h1_style))
    story.append(Paragraph(
        'This file contains the complete mapping from AQUABC Fortran source code to Python analysis. '
        'It defines two main data structures:', body_style))

    story.append(Paragraph('<b>SLOT_MAP</b> dictionary:', h3_style))
    story.append(Paragraph(
        'Keys are state variable names (e.g., "NH4_N", "DIA_C"). Each entry contains: '
        '<font face="Courier">var_index</font> (1-based Fortran index), '
        '<font face="Courier">slots</font> (dict mapping slot number → human-readable description), '
        '<font face="Courier">derivative_formula</font> (string like "+1 +2 -3 -4"), '
        '<font face="Courier">derivative_slots_used</font> (list of slot numbers), '
        '<font face="Courier">conditions</font> (compile-time flags like DO_NOSTOCALES).',
        body_style))

    story.append(Paragraph('<b>DERIVATIVE_SIGNS</b> dictionary:', h3_style))
    story.append(Paragraph(
        'Keys are state variable names. Values are dicts mapping slot_number → sign (+1 or -1). '
        'Used by compute_kinetic_deriv() to reconstruct the DERIVATIVES array from PROCESS_RATES.',
        body_style))

    # ═══════════════════════════════════════════════════════════════════════
    # 7. Usage Guide
    # ═══════════════════════════════════════════════════════════════════════
    story.append(PageBreak())
    story.append(Paragraph('7. Usage Guide', h1_style))

    story.append(Paragraph('<b>Prerequisites:</b>', h2_style))
    story.append(Paragraph(
        '• Python 3.8+ &nbsp;&nbsp; • numpy &nbsp;&nbsp; • reportlab (for PDF generation only)<br/>'
        '• Model output files in OUTPUTS/ directory (must include PROCESS_RATES.out files — '
        'enable "Process Rates" output in Model Config → Output Config tab)',
        body_style))

    story.append(Paragraph('<b>Command-line usage:</b>', h2_style))

    cmds = [
        ('Run 16-check analysis (text output)',
         'python tools/deep_process_rate_analysis.py --output-dir OUTPUTS'),
        ('Generate analysis results PDF',
         'python tools/generate_analysis_results_pdf.py --output-dir OUTPUTS'),
        ('Generate detailed analysis PDF',
         'python tools/generate_deep_pr_pdf.py --output-dir OUTPUTS'),
        ('Run 10-check crosscheck',
         'python tools/deep_state_vs_process_crosscheck.py --output-dir OUTPUTS'),
        ('Generate crosscheck PDF',
         'python tools/generate_crosscheck_pdf.py --output-dir OUTPUTS'),
    ]
    for desc, cmd in cmds:
        story.append(Paragraph(f'<b>{desc}:</b>', body_style))
        story.append(Paragraph(f'<font face="Courier" size="8">{cmd}</font>', small_style))
        story.append(Spacer(1, 1*mm))

    story.append(Paragraph('<b>Programmatic usage:</b>', h2_style))
    story.append(Paragraph(
        'The analysis can be invoked programmatically from Python:', body_style))
    story.append(Paragraph(
        '<font face="Courier" size="8">'
        'from tools.deep_process_rate_analysis import run_analysis<br/>'
        'results = run_analysis("OUTPUTS")<br/>'
        '# results is a dict: {box_id: {check_name: findings_list}}<br/>'
        '# Each finding is a dict with "severity", "variable", and check-specific keys<br/>'
        '</font>', small_style))

    # ═══════════════════════════════════════════════════════════════════════
    # 8. Severity Levels
    # ═══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('8. Severity Classification', h1_style))
    sevs = [
        ('ERROR', '#c0392b', 'Model-critical bug requiring code fix',
         'NaN/Inf values, negative concentrations in variables that must be ≥ 0, '
         'fundamental stoichiometric failures'),
        ('WARNING', '#e67e22', 'Ecological concern worth investigating',
         'Significant smoothness spikes (>10 occurrences, >1% of mean concentration), '
         'unexpected zero slots for active processes, sign violations in process rates'),
        ('INFO', '#3498db', 'Informational note, no action needed',
         'Rate statistics, seasonal patterns, dominant processes, expected ratio deviations '
         '(e.g., DET P:C ≪ Redfield), minor spikes in low-biomass species'),
        ('OK', '#27ae60', 'Check passed without issues',
         'No negatives, no NaN/Inf, mass balance within tolerance'),
    ]
    rows = []
    for sev, color, meaning, examples in sevs:
        rows.append([
            Paragraph(f'<font color="{color}"><b>{sev}</b></font>', cell_style),
            meaning, examples
        ])
    story.append(make_table(['Level', 'Meaning', 'Examples'], rows, col_widths=[55, 140, 265]))

    # ═══════════════════════════════════════════════════════════════════════
    # 9. Extending the Analysis
    # ═══════════════════════════════════════════════════════════════════════
    story.append(Paragraph('9. Extending the Analysis', h1_style))
    story.append(Paragraph(
        'To add a new check:', body_style))
    story.append(Paragraph(
        '1. Create a function <font face="Courier">check_N_name(args)</font> in '
        '<font face="Courier">deep_process_rate_analysis.py</font> that returns a list of '
        'finding dicts, each containing at minimum a <font face="Courier">severity</font> key.<br/>'
        '2. Add the call in <font face="Courier">run_analysis()</font> within the per-box loop.<br/>'
        '3. Add reporting in <font face="Courier">print_summary()</font>.<br/>'
        '4. (Optional) Add a PDF section in <font face="Courier">generate_deep_pr_pdf.py</font>.<br/>'
        '5. Update the check count in the docstring and report titles.',
        body_style))

    story.append(Spacer(1, 3*mm))
    story.append(Paragraph(
        'To modify thresholds, edit the constants in each check function. Key thresholds:', body_style))
    thresholds = [
        ('Spike detection', '10 × mean|dC| with floor of 0.001 × range', 'check_15'),
        ('Spike severity', 'n_spikes > 10 AND magnitude > 1% of mean conc', 'check_15'),
        ('DOC excretion tolerance', 'max(1e-5, 1e-4 × max_total)', 'check_3'),
        ('Mass balance imbalance', '10% of mean individual variable flux', 'check_16'),
        ('Stoichiometric match', '±50% for ZOO, INFO for DET/DOM', 'check_11'),
    ]
    story.append(make_table(
        ['Threshold', 'Value', 'Location'],
        thresholds, col_widths=[120, 200, 60]))

    doc.build(story)
    return pdf_path


def main():
    pdf_path = 'docs/AQUABC_Analysis_Script_Documentation.pdf'
    print(f"Generating script documentation PDF: {pdf_path}")
    build_document(pdf_path)
    fsize = os.path.getsize(pdf_path) / 1024
    print(f"PDF generated: {pdf_path}")
    print(f"File size: {fsize:.0f} KB")


if __name__ == '__main__':
    main()
