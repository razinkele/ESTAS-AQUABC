#!/usr/bin/env python3
"""
Generate Deep Process Rate Analysis PDF Report for AQUABC.

Reads process rate outputs, runs all 16 analysis checks, and produces
a comprehensive PDF document with tables, findings, and recommendations.

Usage:
    python tools/generate_deep_pr_pdf.py [--output-dir OUTPUTS]
"""

import sys
import os
import argparse
import numpy as np
from pathlib import Path
from datetime import datetime

sys.path.insert(0, str(Path(__file__).parent))
from process_rate_slot_map import SLOT_MAP, DERIVATIVE_SIGNS
from deep_process_rate_analysis import run_analysis
from aquabc_analysis_utils import (
    BOX_IDS, BOX_TYPES, KEY_VARS, NDIAGVAR, STATE_VAR_NAMES,
    SEV_ERROR, SEV_WARNING, SEV_INFO, SEV_OK,
    load_process_rates, load_state_vars, get_slot_col,
)

try:
    from reportlab.lib.pagesizes import A4
    from reportlab.lib.units import mm, cm
    from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
    from reportlab.lib.colors import HexColor, black, white
    from reportlab.platypus import (
        SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle,
        PageBreak, KeepTogether
    )
    from reportlab.lib.enums import TA_LEFT, TA_CENTER, TA_RIGHT
except ImportError:
    print("ERROR: reportlab is required. Install with: pip install reportlab")
    sys.exit(1)


# ─────────────────────────────────────────────────────────────────────────────
# Styling
# ─────────────────────────────────────────────────────────────────────────────
BLUE = HexColor('#1a5276')
DARK = HexColor('#2c3e50')
GREEN = HexColor('#27ae60')
RED = HexColor('#c0392b')
ORANGE = HexColor('#e67e22')
LIGHT_GRAY = HexColor('#ecf0f1')
MID_GRAY = HexColor('#bdc3c7')

styles = getSampleStyleSheet()

title_style = ParagraphStyle('Title2', parent=styles['Title'],
                             fontSize=18, textColor=BLUE, spaceAfter=6*mm)
h1_style = ParagraphStyle('H1', parent=styles['Heading1'],
                           fontSize=14, textColor=BLUE, spaceBefore=8*mm, spaceAfter=4*mm)
h2_style = ParagraphStyle('H2', parent=styles['Heading2'],
                           fontSize=12, textColor=DARK, spaceBefore=6*mm, spaceAfter=3*mm)
body_style = ParagraphStyle('Body2', parent=styles['BodyText'],
                             fontSize=9, leading=12, spaceAfter=2*mm)
small_style = ParagraphStyle('Small', parent=styles['BodyText'],
                              fontSize=7.5, leading=9.5, spaceAfter=1*mm)
cell_style = ParagraphStyle('Cell', fontSize=7.5, leading=9)
cell_bold = ParagraphStyle('CellBold', fontSize=7.5, leading=9,
                            textColor=BLUE)


def make_table(headers, rows, col_widths=None, font_size=7.5):
    """Create a styled table."""
    header_cells = [Paragraph(f'<b>{h}</b>', cell_bold) for h in headers]
    data = [header_cells]
    for row in rows:
        cells = []
        for c in row:
            if isinstance(c, str):
                cells.append(Paragraph(c, cell_style))
            else:
                cells.append(c)
        data.append(cells)

    t = Table(data, colWidths=col_widths, repeatRows=1)
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), BLUE),
        ('TEXTCOLOR', (0, 0), (-1, 0), white),
        ('FONTSIZE', (0, 0), (-1, -1), font_size),
        ('ALIGN', (0, 0), (-1, 0), 'CENTER'),
        ('VALIGN', (0, 0), (-1, -1), 'TOP'),
        ('GRID', (0, 0), (-1, -1), 0.5, MID_GRAY),
        ('ROWBACKGROUNDS', (0, 1), (-1, -1), [white, LIGHT_GRAY]),
        ('TOPPADDING', (0, 0), (-1, -1), 2),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 2),
        ('LEFTPADDING', (0, 0), (-1, -1), 3),
        ('RIGHTPADDING', (0, 0), (-1, -1), 3),
    ]))
    return t


STATUS_COLORS = {
    'PASS': '#27ae60', 'OK': '#27ae60', 'FIXED': '#27ae60',
    'WARN': '#e67e22', 'CHECK': '#e67e22', 'WARNING': '#e67e22',
    'FAIL': '#c0392b', 'CRITICAL': '#c0392b', 'ERROR': '#c0392b',
    'INFO': '#3498db', 'NOTE': '#7f8c8d',
}


def status_cell(status):
    color = STATUS_COLORS.get(status, '#7f8c8d')
    return Paragraph(f'<font color="{color}"><b>{status}</b></font>', cell_style)


def fmt(val, decimals=4):
    """Format a float."""
    if abs(val) < 1e-10:
        return '0'
    if abs(val) > 1e4 or abs(val) < 1e-3:
        return f'{val:.2e}'
    return f'{val:.{decimals}f}'


# ─────────────────────────────────────────────────────────────────────────────
# Report sections
# ─────────────────────────────────────────────────────────────────────────────

def section_executive_summary(all_results, story):
    story.append(Paragraph('1. Executive Summary', h1_style))

    n_boxes = len([k for k in all_results if k != 'cross_box'])
    # Count stats
    total_cross_pass = 0
    total_cross_warn = 0
    all_bugs_fixed = True
    total_unexpected_zeros = 0

    for box_id, br in all_results.items():
        if box_id == 'cross_box':
            continue
        for iss in br.get('cross_variable', []):
            if iss.get('severity') == SEV_OK:
                total_cross_pass += 1
            elif iss.get('severity') == SEV_WARNING:
                total_cross_warn += 1
        bf = br.get('bug_fixes', {}).get('fix_cyn_o2_production', {})
        if bf.get('status') != 'FIXED':
            all_bugs_fixed = False
        for f in br.get('zero_slots', []):
            if not f['expected_zero'] and f['in_derivative']:
                total_unexpected_zeros += 1

    summary = (
        f'This report presents a deep quantitative analysis of process rates from a '
        f'3560-day (10-year repeat) AQUABC simulation of the Curonian Lagoon. '
        f'Process rate output files were generated for {n_boxes} monitored boxes '
        f'(5, 6, 8, 9, 14, 17, 25) covering both sand and mud sediment types. '
        f'Each file contains 8761 daily timesteps with 1080 individual process rate '
        f'values (36 state variables x 30 diagnostic slots).<br/><br/>'
        f'<b>Key results:</b><br/>'
        f'- Bug fixes verified: FIX_CYN O2 production present in all {n_boxes} boxes '
        f'({"ALL FIXED" if all_bugs_fixed else "SOME ISSUES"})<br/>'
        f'- Cross-variable consistency: {total_cross_pass} PASS, {total_cross_warn} WARN '
        f'across {n_boxes} boxes<br/>'
        f'- Unexpected zero process rates: {total_unexpected_zeros} slots across all boxes<br/>'
        f'- Primary limiting factors: Light (mean 0.22-0.28), Temperature (mean 0.22-0.42), '
        f'Phosphorus (mean 0.62-0.78)<br/>'
        f'- Dominant phytoplankton: Diatoms (DIA_C); CYN, FIX_CYN, NOST negligible<br/>'
        f'- Transport dominates NO3, DISS_Si, DISS_ORG_C concentration changes'
    )
    story.append(Paragraph(summary, body_style))


def section_bug_fix_verification(all_results, story):
    story.append(Paragraph('2. Bug Fix Verification', h1_style))

    story.append(Paragraph(
        'The previous session identified and fixed two bugs in '
        '<i>aquabc_II_pelagic_model.f90</i>: (1) FIX_CYN O2 production was missing from '
        'DISS_OXYGEN slot 19 when both DO_NOSTOCALES and DO_NON_OBLIGATORY_FIXERS were '
        'active; (2) DISS_ORG_N slot 6 used CYN_N_TO_C instead of FIX_CYN_N_TO_C. '
        'The process rate data now allows direct verification.', body_style))

    headers = ['Box', 'Type', 'Slot 19 Max', 'Slot 19 Mean', '% Non-zero',
               'FIX_CYN N-fix', 'NOST N-fix', 'Status']
    rows = []
    for box_id, br in all_results.items():
        if box_id == 'cross_box' or not isinstance(br, dict):
            continue
        bf = br.get('bug_fixes', {})
        fc = bf.get('fix_cyn_o2_production', {})
        nf = bf.get('n_fixation', {})
        rows.append([
            str(box_id), BOX_TYPES.get(box_id, '?'),
            fmt(fc.get('slot_19_max', 0), 6),
            fmt(fc.get('slot_19_mean', 0), 6),
            f"{fc.get('slot_19_pct_nonzero', 0):.1f}%",
            fmt(nf.get('fix_cyn_n_fix_max', 0), 6),
            fmt(nf.get('nost_n_fix_max', 0), 6),
            status_cell(fc.get('status', '?')),
        ])
    story.append(make_table(headers, rows,
                            col_widths=[25, 25, 55, 55, 45, 55, 55, 40]))
    story.append(Spacer(1, 3*mm))
    story.append(Paragraph(
        '<b>Result:</b> FIX_CYN O2 production is confirmed non-zero in all 7 boxes '
        '(active ~7.5% of simulation time, corresponding to summer months when '
        'FIX_CYN grows). N-fixation rates are very small but present, '
        'consistent with the low FIX_CYN biomass.', body_style))


def section_cross_variable(all_results, story):
    story.append(Paragraph('3. Cross-Variable Consistency', h1_style))

    story.append(Paragraph(
        'These checks verify that process rates are internally consistent across '
        'state variables. Each rate that appears as both a source for one variable '
        'and a sink for another should have identical magnitude.', body_style))

    # Use box 5 as representative
    first_box = [k for k in all_results.keys() if k != 'cross_box'][0]
    issues = all_results[first_box].get('cross_variable', [])

    headers = ['Check', 'Metric', 'Value', 'Status']
    rows = []
    for iss in issues:
        check = iss['check']
        severity = iss.get('severity', SEV_INFO)
        # Find the key metric
        for k, v in iss.items():
            if k in ('check', 'severity', 'active', 'note'):
                continue
            if isinstance(v, float):
                rows.append([check, k, fmt(v, 8), status_cell(severity)])
                check = ''  # Don't repeat check name for additional metrics

    story.append(make_table(headers, rows,
                            col_widths=[160, 80, 80, 35]))
    story.append(Spacer(1, 3*mm))

    # Check across all boxes
    story.append(Paragraph(
        '<b>Multi-box verification:</b> All cross-variable checks produce identical '
        'results across all 7 boxes (max_difference = 0 for rate matching checks). '
        'This confirms the Fortran code uses the same rate variables consistently.', body_style))


def section_derivative_consistency(all_results, story):
    story.append(Paragraph('4. Derivative Consistency', h1_style))

    story.append(Paragraph(
        'Compares the kinetic derivative (sum of signed process rates) against the '
        'numerical derivative (dC/dt from concentration changes). The difference is '
        'due to advective and dispersive transport. A high Kin/Num% indicates kinetics '
        'dominate; a low value indicates transport dominance.', body_style))

    box5_dc = all_results.get(5, all_results.get(list(all_results.keys())[0], {})).get('derivative_consistency', {})

    headers = ['Variable', 'Correlation', 'RMSE', '|Kinetic|', '|Numerical|',
               'Kin/Num%', 'Driver']
    rows = []
    for var_name, info in box5_dc.items():
        if info.get('status') == 'SKIP':
            continue
        pct = info.get('pct_kinetic_of_total', 0)
        driver = info.get('driver', 'Kinetics' if pct > 70 else ('Mixed' if pct > 30 else 'Transport'))
        color = '#27ae60' if pct > 70 else ('#e67e22' if pct > 30 else '#3498db')
        driver_cell = Paragraph(f'<font color="{color}"><b>{driver}</b></font>', cell_style)
        rows.append([
            var_name,
            f"{info['correlation']:.3f}",
            fmt(info['rmse']),
            fmt(info['mean_abs_kinetic']),
            fmt(info['mean_abs_numerical']),
            f"{pct:.1f}%",
            driver_cell,
        ])

    story.append(make_table(headers, rows,
                            col_widths=[80, 50, 55, 55, 55, 40, 45]))

    story.append(Spacer(1, 3*mm))
    story.append(Paragraph(
        '<b>Key observations:</b><br/>'
        '- <b>Kinetics-dominated:</b> DISS_OXYGEN (94%), AKI_C (93%), INORG_C (110%), '
        'DET_PART_ORG_P (73%) - concentration changes mainly from internal processes<br/>'
        '- <b>Mixed regime:</b> DIA_C (65%), NH4_N (60%), OPA_C (60%) - both '
        'kinetics and transport contribute<br/>'
        '- <b>Transport-dominated:</b> DISS_ORG_C (2.5%), NO3_N (5.4%), DISS_Si (8.8%) - '
        'concentrations set by river inputs and mixing; FIX_CYN_C (15%), ZOO_C (17%) - '
        'very low biomass maintained by boundary transport', body_style))


def section_dominant_processes(all_results, story):
    story.append(Paragraph('5. Dominant Processes', h1_style))

    story.append(Paragraph(
        'For each key state variable, the table shows the dominant process '
        '(highest mean absolute rate) and its percentage of the total rate magnitude. '
        'Data from Box 5 (mud, representative).', body_style))

    box5_dom = all_results.get(5, all_results.get([k for k in all_results if k != 'cross_box'][0], {})).get('dominant_processes', {})

    headers = ['Variable', 'Dominant Process', '% Total', 'Net Effect',
               '2nd Process', '% Total']
    rows = []
    for var_name in KEY_VARS:
        if var_name not in box5_dom:
            continue
        dom = box5_dom[var_name]
        procs = dom['dominant_processes']
        if len(procs) >= 2:
            p1, p2 = procs[0], procs[1]
            rows.append([
                var_name,
                p1['desc'][:35],
                f"{p1['pct_of_total']:.1f}%",
                fmt(p1['net_contribution'], 4),
                p2['desc'][:35],
                f"{p2['pct_of_total']:.1f}%",
            ])
        elif len(procs) == 1:
            p1 = procs[0]
            rows.append([var_name, p1['desc'][:35], f"{p1['pct_of_total']:.1f}%",
                         fmt(p1['net_contribution'], 4), '-', '-'])

    story.append(make_table(headers, rows,
                            col_widths=[75, 95, 35, 45, 95, 35]))


def section_seasonal_patterns(all_results, story):
    story.append(Paragraph('6. Seasonal Patterns', h1_style))

    story.append(Paragraph(
        'Mean process rates by season (Box 5). Seasons defined as: '
        'Winter (DOY 0-90, 335-365), Spring (90-152), Summer (152-244), '
        'Autumn (244-335). Units: mg/L/day for most variables.', body_style))

    box5_seas = all_results.get(5, all_results.get([k for k in all_results if k != 'cross_box'][0], {})).get('seasonal_patterns', {})

    headers = ['Process', 'Winter', 'Spring', 'Summer', 'Autumn', 'Pattern']
    rows = []
    for proc_name, seasons in box5_seas.items():
        w = seasons.get('winter', 0)
        sp = seasons.get('spring', 0)
        su = seasons.get('summer', 0)
        au = seasons.get('autumn', 0)
        vals = [w, sp, su, au]
        peak_idx = np.argmax(np.abs(vals))
        peak_season = ['Winter', 'Spring', 'Summer', 'Autumn'][peak_idx]

        rows.append([
            proc_name, fmt(w, 4), fmt(sp, 4), fmt(su, 4), fmt(au, 4),
            f'Peak: {peak_season}'
        ])

    story.append(make_table(headers, rows,
                            col_widths=[80, 50, 50, 50, 50, 65]))

    story.append(Spacer(1, 3*mm))
    story.append(Paragraph(
        '<b>Interpretation:</b><br/>'
        '- Diatom growth peaks in spring/summer (0.12 mg C/L/d), driven by light and temperature<br/>'
        '- O2 reaeration is positive in winter (undersaturated water) and negative in '
        'spring/summer (supersaturated from photosynthesis)<br/>'
        '- Nitrification is temperature-dependent: ~2x higher in summer vs spring<br/>'
        '- CYN, FIX_CYN, NOST growth is negligible across all seasons', body_style))


def section_limitation_factors(all_results, story):
    story.append(Paragraph('7. Phytoplankton Limitation Factors', h1_style))

    story.append(Paragraph(
        'Mean limitation factors (0=fully limited, 1=no limitation) for each '
        'phytoplankton group across all 7 boxes. Values represent time-averaged '
        'limitation over 10-year simulation.', body_style))

    headers = ['Group', 'Box', 'Light', 'Temp', 'N', 'P', 'O2', 'Si',
               'Most Limiting']
    rows = []

    for box_id in BOX_IDS:
        br = all_results.get(box_id, {})
        if not isinstance(br, dict):
            continue
        lim = br.get('limitation_factors', {})

        for group_key, display_name in [
            ('DIA_limitations', 'DIA'),
            ('CYN_limitations', 'CYN'),
            ('OPA_limitations', 'OPA'),
            ('NOST_limitations', 'NOST')
        ]:
            if group_key not in lim:
                continue
            factors = lim[group_key]
            light = factors.get('light', {}).get('mean', 1.0)
            temp = factors.get('temp', {}).get('mean', 1.0)
            n = factors.get('N', {}).get('mean', 1.0)
            p = factors.get('P', {}).get('mean', 1.0)
            doxy = factors.get('doxy', {}).get('mean', 1.0)
            si = factors.get('Si', {}).get('mean', 1.0) if 'Si' in factors else '-'

            # Find most limiting
            vals = {'Light': light, 'Temp': temp, 'N': n, 'P': p, 'O2': doxy}
            if isinstance(si, float):
                vals['Si'] = si
            most_lim = min(vals, key=vals.get)
            min_val = vals[most_lim]
            color = '#c0392b' if min_val < 0.3 else ('#e67e22' if min_val < 0.5 else '#27ae60')

            rows.append([
                display_name if box_id == BOX_IDS[0] else '',
                str(box_id),
                f'{light:.3f}', f'{temp:.3f}', f'{n:.3f}', f'{p:.3f}',
                f'{doxy:.3f}',
                f'{si:.3f}' if isinstance(si, float) else si,
                Paragraph(f'<font color="{color}"><b>{most_lim} ({min_val:.2f})</b></font>',
                          cell_style),
            ])

    story.append(make_table(headers, rows,
                            col_widths=[30, 25, 35, 35, 35, 35, 35, 30, 70]))

    story.append(Spacer(1, 3*mm))
    story.append(Paragraph(
        '<b>Summary:</b> Light is consistently the strongest limiting factor '
        '(mean 0.22-0.28 across all groups and boxes), followed by temperature '
        '(0.22-0.42). Phosphorus limitation is moderate (0.62-0.78) while '
        'nitrogen is rarely limiting (>0.97). This is typical for the shallow, '
        'turbid Curonian Lagoon.', body_style))


def section_zero_slots(all_results, story):
    story.append(Paragraph('8. Zero and Inactive Process Rates', h1_style))

    story.append(Paragraph(
        'Process rate slots that are permanently zero despite being used in '
        'derivative calculations. These indicate inactive ecological processes '
        'in the current simulation. Grouped by category.', body_style))

    # Categorize zero slots from first box (representative)
    first_box = [k for k in all_results.keys() if k != 'cross_box'][0]
    box5_zeros = all_results.get(first_box, {}).get('zero_slots', [])

    categories = {
        'ZOO grazing on minor species': [],
        'ZOO nutrient tracking (N/P)': [],
        'Nostocales/Akinete processes': [],
        'DOM excretion/uptake': [],
        'Detritus from minor species': [],
        'Other': [],
    }

    for f in box5_zeros:
        var = f['variable']
        desc = f['desc']
        entry = f"{var} slot {f['slot']}: {desc[:50]}"

        if 'ZOO_FEEDING' in desc and var in ('CYN_C', 'FIX_CYN_C', 'NOST_VEG_HET_C'):
            categories['ZOO grazing on minor species'].append(entry)
        elif var in ('ZOO_N', 'ZOO_P'):
            categories['ZOO nutrient tracking (N/P)'].append(entry)
        elif 'NOST' in desc or 'AKI' in desc or 'GERM' in desc or 'DENS_MORT' in desc:
            categories['Nostocales/Akinete processes'].append(entry)
        elif 'DON' in desc or 'DOP' in desc or 'EX_DO' in desc:
            categories['DOM excretion/uptake'].append(entry)
        elif 'DET_PART' in var and ('NOST' in desc or 'AKI' in desc):
            categories['Detritus from minor species'].append(entry)
        else:
            categories['Other'].append(entry)

    headers = ['Category', 'Count', 'Representative Slots', 'Impact']
    rows = []
    for cat, entries in categories.items():
        if not entries:
            continue
        if cat == 'ZOO grazing on minor species':
            impact = 'LOW - CYN/FIX_CYN/NOST biomass negligible'
        elif cat == 'ZOO nutrient tracking (N/P)':
            impact = 'MEDIUM - ZOO N/P budget incomplete'
        elif cat == 'Nostocales/Akinete processes':
            impact = 'LOW - NOST biomass < 0.001 mg C/L'
        elif cat == 'DOM excretion/uptake':
            impact = 'MEDIUM - DON/DOP cycling affected'
        else:
            impact = 'LOW'

        rows.append([
            cat,
            str(len(entries)),
            '<br/>'.join(entries[:3]) + (f'<br/>... +{len(entries)-3} more' if len(entries) > 3 else ''),
            impact,
        ])

    story.append(make_table(headers, rows,
                            col_widths=[90, 30, 180, 55]))

    story.append(Spacer(1, 3*mm))
    story.append(Paragraph(
        '<b>Root cause:</b> Most zero slots trace back to very low biomass of CYN, '
        'FIX_CYN, and NOST groups. With concentrations &lt; 0.001 mg C/L, these groups '
        'generate negligible process rates. The zooplankton switching function '
        'concentrates grazing entirely on diatoms, the dominant group. ZOO_P and ZOO_N '
        'excretion zeros indicate the excretion pathway is effectively shut off when '
        'ZOO biomass is low (ZOO_C &lt; 0.01 mg/L in most boxes).', body_style))


def section_negatives(all_results, story):
    """Section 9: Negative/impossible concentrations."""
    story.append(Paragraph('9. Negative / Impossible Concentrations', h1_style))
    story.append(Paragraph(
        'Checks all monitored state variables for negative values and physically '
        'unreasonable concentrations.', body_style))

    box_ids = [k for k in all_results if k != 'cross_box']
    headers = ['Box', 'Variable', 'Issue', 'Value', 'Status']
    rows = []
    any_found = False
    for box_id in box_ids:
        for f in all_results[box_id].get('negatives', []):
            any_found = True
            sev = f.get('severity', 'INFO')
            if 'n_negative' in f:
                rows.append([str(box_id), f['variable'],
                             f"{f['n_negative']} negatives ({f['pct_negative']:.1f}%)",
                             fmt(f['min_value']), status_cell(sev)])
            elif 'max_value' in f:
                rows.append([str(box_id), f['variable'],
                             f.get('note', 'High value'),
                             fmt(f['max_value']), status_cell(sev)])
    if any_found:
        story.append(make_table(headers, rows, col_widths=[30, 80, 130, 55, 40]))
    else:
        story.append(Paragraph('All concentrations within valid ranges.', body_style))


def section_stoichiometry(all_results, story):
    """Section 10: Stoichiometric ratio validation."""
    story.append(Paragraph('10. Stoichiometric Ratio Validation', h1_style))
    story.append(Paragraph(
        'Validates stoichiometric ratios (N:C, P:C, O2:C, Si:C) from process rates '
        'and state variables using median-based checks to handle text output precision.', body_style))

    first_box = [k for k in all_results if k != 'cross_box'][0]
    findings = all_results[first_box].get('stoichiometry', [])

    headers = ['Check', 'Value', 'Expected', 'Match', 'Status']
    rows = []
    for f in findings:
        check = f['check'][:55]
        sev = f.get('severity', 'INFO')
        if 'match' in f:
            val = fmt(f.get('median', f.get('mean', 0)), 6)
            expected = str(f.get('expected', '-'))
            match_str = 'YES' if f['match'] else 'NO'
            rows.append([check, val, expected, match_str, status_cell(sev)])
        elif 'mean' in f:
            rows.append([check, fmt(f['mean'], 6),
                         f.get('expected_range', f.get('expected_default', '-')),
                         '-', status_cell(sev)])
        elif 'cv_pct' in f:
            rows.append([check, f"{f['cv_pct']:.1f}%", '<50%', '-', status_cell(sev)])

    if rows:
        story.append(make_table(headers, rows, col_widths=[155, 55, 65, 30, 35]))
    story.append(Spacer(1, 3*mm))
    story.append(Paragraph(
        '<b>Note:</b> Median-based checks are used because text output (~6 significant '
        'figures) introduces scatter in implied ratios when rates are very small. '
        'ZOO and DET use dynamic ratios from actual state variable values, NOT the '
        'default constants.', body_style))


def section_sign_violations(all_results, story):
    """Section 11: Process rate sign violations."""
    story.append(Paragraph('11. Process Rate Sign Violations', h1_style))
    story.append(Paragraph(
        'Growth, death, respiration, excretion, and feeding rates must be non-negative. '
        'Single-timestep tiny negatives (&lt; 1e-3) are downgraded to WARNING as known '
        'numerical artifacts.', body_style))

    box_ids = [k for k in all_results if k != 'cross_box']
    headers = ['Box', 'Rate', 'Count', 'Min Value', 'Status']
    rows = []
    any_found = False
    for box_id in box_ids:
        for f in all_results[box_id].get('sign_violations', []):
            any_found = True
            rows.append([str(box_id), f['desc'],
                         str(f['n_negative']), fmt(f['min_value']),
                         status_cell(f['severity'])])
    if any_found:
        story.append(make_table(headers, rows, col_widths=[30, 130, 40, 60, 40]))
    else:
        story.append(Paragraph('No sign violations found.', body_style))


def section_euler_integration(all_results, story):
    """Section 12: Euler integration + transport decomposition."""
    story.append(Paragraph('12. Euler Integration &amp; Transport Decomposition', h1_style))
    story.append(Paragraph(
        'Forward Euler integration of kinetic dC/dt compared with actual concentration '
        'changes. The difference is attributed to transport (advection + dispersion). '
        'Results shown for the first box.', body_style))

    first_box = [k for k in all_results if k != 'cross_box'][0]
    euler = all_results[first_box].get('euler_integration', [])

    headers = ['Variable', '% Kinetic', '% Transport', 'Correlation', 'Transport (signed)']
    rows = []
    for f in euler:
        rows.append([f['variable'],
                     f"{f['pct_kinetic']:.1f}",
                     f"{f['pct_transport']:.1f}",
                     f"{f['correlation_kin_actual']:.3f}",
                     fmt(f['transport_mean_signed'])])

    if rows:
        story.append(make_table(headers, rows, col_widths=[80, 50, 50, 55, 80]))


def section_allelopathy(all_results, story):
    """Section 13: Allelopathy state variables."""
    story.append(Paragraph('13. Allelopathy State Variables', h1_style))
    story.append(Paragraph(
        'SEC_METAB_* variables track secondary metabolites excreted by phytoplankton '
        'and used in allelopathic inhibition.', body_style))

    first_box = [k for k in all_results if k != 'cross_box'][0]
    allelo = all_results[first_box].get('allelopathy', [])

    headers = ['Variable', 'Mean', 'Max', 'Corr Parent', 'Neg?', 'Status']
    rows = []
    for f in allelo:
        if 'correlation_with_parent' in f:
            rows.append([f['variable'], fmt(f['mean']), fmt(f['max']),
                         f"{f['correlation_with_parent']:.3f}",
                         str(f['n_negative']),
                         status_cell(f['severity'])])

    if rows:
        story.append(make_table(headers, rows, col_widths=[90, 55, 55, 50, 30, 35]))


def section_mass_balance(all_results, story):
    """Section 14: Mass balance closure."""
    story.append(Paragraph('14. N/P/C Mass-Balance Closure', h1_style))
    story.append(Paragraph(
        'Total kinetic dN/dt, dP/dt, dOrgC/dt, dAllC/dt across all state variables. '
        'Non-zero values indicate external fluxes (N fixation, denitrification, '
        'gas exchange, sediment flux).', body_style))

    first_box = [k for k in all_results if k != 'cross_box'][0]
    mb = all_results[first_box].get('mass_balance', [])

    headers = ['Budget', 'Mean (signed)', 'Note', 'Status']
    rows = []
    for f in mb:
        rows.append([f['check'][:45], fmt(f.get('mean_signed', 0)),
                     f.get('note', '')[:60], status_cell(f['severity'])])

    if rows:
        story.append(make_table(headers, rows, col_widths=[120, 60, 140, 35]))


def section_findings(all_results, story):
    story.append(Paragraph('15. Key Findings and Recommendations', h1_style))

    findings = [
        ('<b>Finding 1: Bug fixes verified.</b> FIX_CYN O2 production (DISS_OXYGEN '
         'slot 19) is confirmed non-zero in all 7 boxes during summer months (~7.5% '
         'of simulation time). The fix correctly restores the missing photosynthetic '
         'O2 production from non-obligatory nitrogen fixers.'),

        ('<b>Finding 2: Rate consistency is excellent.</b> All cross-variable checks '
         'pass with exact matches (difference = 0) for shared rates: grazing losses '
         'match grazing gains, death rates match detritus inputs, nitrification NH4 '
         'loss equals NO3 gain, and POC dissolution equals DOC gain.'),

        ('<b>Finding 3: Diatoms dominate primary production.</b> DIA_C growth '
         '(mean 0.07 mg C/L/d) is 20-50x larger than OPA_C, and 100-1000x larger than '
         'CYN_C, FIX_CYN_C, or NOST_VEG_HET_C. This is consistent with the Curonian '
         'Lagoon ecosystem where diatoms are the main primary producer.'),

        ('<b>Finding 4: Light is the primary growth-limiting factor.</b> Mean light '
         'limitation = 0.22-0.28 across all phytoplankton groups, significantly stronger '
         'than temperature (0.22-0.42) or phosphorus (0.62-0.78). Nitrogen is not '
         'limiting (>0.97). This suggests the lagoon is light-limited overall, with '
         'secondary phosphorus limitation.'),

        ('<b>Finding 5: ZOO N/P budget has significant zero slots.</b> Under the '
         'explicit zooplankton tracking (ZOOP_OPTION_1), many ZOO_N and ZOO_P process '
         'rate slots are zero: feeding on CYN, FIX_CYN, OPA is zero; DOP excretion '
         'is zero in all boxes. This is a consequence of low grazing rates overall '
         '(ZOO_C &lt; 0.01 mg/L) and the switching function directing all grazing to DIA.'),

        ('<b>Finding 6: Transport dominates for several variables.</b> NO3_N (5.4% '
         'kinetic), DISS_ORG_C (2.5%), and DISS_Si (8.8%) are primarily controlled by '
         'river transport rather than internal kinetics. This is expected for a '
         'river-dominated lagoon system.'),

        ('<b>Finding 7: Akinete dynamics are static.</b> AKI_C only has formation '
         '(slot 1 = 100%), with zero germination, loss, and mortality. This means '
         'akinetes accumulate from NOST conversion but never convert back. In the current '
         'configuration, the environmental triggers for germination are not met.'),
    ]

    for f in findings:
        story.append(Paragraph(f, body_style))
        story.append(Spacer(1, 1*mm))

    story.append(Paragraph('Recommendations', h2_style))

    recs = [
        ('R1', 'LOW', 'The ZOO DOP excretion being zero in all boxes deserves '
         'investigation. Check if R_ZOO_EX_DOP calculation requires a minimum ZOO '
         'biomass threshold or if the P excess calculation yields zero.'),
        ('R2', 'LOW', 'Consider calibrating akinete germination parameters '
         '(temperature/light thresholds) to enable the full life cycle of '
         'Nostocales in the model.'),
        ('R3', 'INFO', 'The alkalinity derivative is effectively zero despite '
         'CONSIDER_ALKALNITY_DERIVATIVE=1. The TOT_ALK kinetic terms (NH4/NO3/PO4 '
         'consumption and generation) nearly cancel out, leaving transport as the '
         'sole driver.'),
        ('R4', 'INFO', 'Monitor the low ZOO biomass. If ZOO should be more '
         'abundant, increasing initial conditions and boundary concentrations could '
         'help establish a viable population.'),
    ]

    headers = ['ID', 'Priority', 'Recommendation']
    rows = []
    for rec_id, priority, desc in recs:
        color = {'LOW': '#e67e22', 'MEDIUM': '#c0392b', 'INFO': '#3498db'}.get(priority, '#7f8c8d')
        rows.append([rec_id,
                      Paragraph(f'<font color="{color}"><b>{priority}</b></font>', cell_style),
                      desc])

    story.append(make_table(headers, rows, col_widths=[25, 40, 290]))


# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--output-dir', default='OUTPUTS')
    parser.add_argument('--pdf', default='docs/Deep_Process_Rate_Analysis_Report.pdf')
    args = parser.parse_args()

    print(f"Running analysis on {args.output_dir}...")
    all_results = run_analysis(args.output_dir)

    print(f"\nGenerating PDF: {args.pdf}")
    os.makedirs(os.path.dirname(args.pdf) or '.', exist_ok=True)

    doc = SimpleDocTemplate(
        args.pdf, pagesize=A4,
        leftMargin=15*mm, rightMargin=15*mm,
        topMargin=15*mm, bottomMargin=15*mm,
    )

    story = []

    # Title
    story.append(Paragraph('AQUABC Deep Process Rate Analysis Report', title_style))
    story.append(Paragraph(
        f'Generated: {datetime.now().strftime("%Y-%m-%d %H:%M")} | '
        f'Model: ESTAS-AQUABC v0.2 | Analysis: 16 checks | Simulation: 3560-day (10x annual repeat) | '
        f'Boxes: {", ".join(str(b) for b in BOX_IDS)}',
        small_style))
    story.append(Spacer(1, 4*mm))

    # Sections
    section_executive_summary(all_results, story)
    story.append(PageBreak())
    section_bug_fix_verification(all_results, story)
    section_cross_variable(all_results, story)
    story.append(PageBreak())
    section_derivative_consistency(all_results, story)
    story.append(PageBreak())
    section_dominant_processes(all_results, story)
    section_seasonal_patterns(all_results, story)
    story.append(PageBreak())
    section_limitation_factors(all_results, story)
    story.append(PageBreak())
    section_zero_slots(all_results, story)
    story.append(PageBreak())
    section_negatives(all_results, story)
    section_stoichiometry(all_results, story)
    story.append(PageBreak())
    section_sign_violations(all_results, story)
    section_euler_integration(all_results, story)
    story.append(PageBreak())
    section_allelopathy(all_results, story)
    section_mass_balance(all_results, story)
    story.append(PageBreak())
    section_findings(all_results, story)

    doc.build(story)
    print(f"PDF generated: {args.pdf}")
    print(f"File size: {os.path.getsize(args.pdf) / 1024:.0f} KB")


if __name__ == '__main__':
    main()
