#!/usr/bin/env python3
"""
Deep Process Rate Analysis for AQUABC Pelagic Model  (v2 — comprehensive)
==========================================================================
Reads process rate output files (PELAGIC_BOX_XXXXX_PROCESS_RATES.out)
and state variable files (PELAGIC_BOX_XXXXX.out), performs 16 checks
spanning data quality, stoichiometry, mass balance, spatial consistency,
and ecological diagnostics, and generates a structured analysis report.

Checks
------
 1  Rate statistics (basic per-slot stats)
 2  Derivative consistency (kinetic vs numerical dC/dt + transport residual)
 3  Cross-variable consistency (extended — 15+ sub-checks)
 4  Bug-fix verification (FIX_CYN O2, DON NOST)
 5  Dominant processes per variable
 6  Seasonal patterns
 7  Zero-slot analysis
 8  Limitation factors (phytoplankton)
 9  Negative / impossible concentrations
10  NaN / Inf detection
11  Stoichiometric ratio validation (median-based)
12  Process rate sign violations (smart downgrade)
13  Euler integration + transport residual decomposition
14  Allelopathy state-variable consistency
15  State-variable smoothness / spike detection
16  N/P/C mass-balance closure

Usage:
    python tools/deep_process_rate_analysis.py [--output-dir OUTPUTS]
"""

import sys
import os
import argparse
import numpy as np
from pathlib import Path

# Shared utilities
sys.path.insert(0, str(Path(__file__).parent))
from process_rate_slot_map import SLOT_MAP, DERIVATIVE_SIGNS
from aquabc_analysis_utils import (
    NDIAGVAR, NSTATE, NUM_ALLELOPATHY, NSTATE_TOTAL, NUM_PROCESS_RATES,
    BOX_IDS, BOX_TYPES, STATE_VAR_NAMES, KEY_VARS,
    DEFAULT_N_TO_C, DEFAULT_P_TO_C, DEFAULT_O2_TO_C, DEFAULT_Si_TO_C,
    NON_NEGATIVE_VARS, NONNEG_RATE_SLOTS,
    SEV_ERROR, SEV_WARNING, SEV_INFO, SEV_OK,
    get_slot_col, load_process_rates, load_state_vars,
    compute_kinetic_deriv, compute_numerical_derivative, find_sv_column,
)


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 1: Rate statistics
# ═════════════════════════════════════════════════════════════════════════════
def check_1_rate_statistics(time, rates, box_id):
    """Basic statistics of process rates per variable."""
    results = {}
    for var_name in KEY_VARS:
        if var_name not in SLOT_MAP:
            continue
        info = SLOT_MAP[var_name]
        var_idx = info['var_index']
        var_stats = {}

        for slot_num, slot_desc in sorted(info['slots'].items()):
            col = get_slot_col(var_idx, slot_num)
            if col >= rates.shape[1]:
                continue
            data = rates[:, col]
            is_aux = '(AUX' in slot_desc

            var_stats[slot_num] = {
                'desc': slot_desc,
                'is_aux': is_aux,
                'min': float(np.nanmin(data)),
                'max': float(np.nanmax(data)),
                'mean': float(np.nanmean(data)),
                'std': float(np.nanstd(data)),
                'always_zero': bool(np.all(data == 0.0)),
                'has_nan': bool(np.any(np.isnan(data))),
                'has_inf': bool(np.any(np.isinf(data))),
                'pct_nonzero': float(np.count_nonzero(data) / len(data) * 100),
            }
        results[var_name] = var_stats
    return results


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 2: Derivative consistency  (enhanced — with transport residual)
# ═════════════════════════════════════════════════════════════════════════════
def check_2_derivative_consistency(time, rates, state_time, state_concs, state_names, box_id):
    """Compare kinetic dC/dt vs numerical dC/dt, decompose into kinetic + transport."""
    results = {}
    dt = np.median(np.diff(state_time))  # typically 1 day

    for var_name in KEY_VARS:
        if var_name not in SLOT_MAP:
            continue

        var_col = find_sv_column(var_name, state_names)
        if var_col is None:
            results[var_name] = {'status': 'SKIP', 'reason': 'not found in state output'}
            continue

        kin_deriv = compute_kinetic_deriv(rates, var_name, SLOT_MAP, DERIVATIVE_SIGNS)
        conc = state_concs[:, var_col]
        actual_dC = np.diff(conc)
        kin_dC = kin_deriv[:len(actual_dC)] * dt

        n = min(len(kin_dC), len(actual_dC))
        if n == 0:
            results[var_name] = {'status': 'SKIP', 'reason': 'no data'}
            continue
        kin_dC = kin_dC[:n]
        actual_dC = actual_dC[:n]

        transport = actual_dC - kin_dC
        mean_abs_actual = float(np.mean(np.abs(actual_dC)))
        mean_abs_kin = float(np.mean(np.abs(kin_dC)))
        mean_abs_transport = float(np.mean(np.abs(transport)))
        corr = float(np.corrcoef(kin_dC, actual_dC)[0, 1]) \
            if np.std(kin_dC) > 0 and np.std(actual_dC) > 0 else 0.0
        rmse = float(np.sqrt(np.mean((kin_dC - actual_dC)**2)))

        pct_kin = mean_abs_kin / mean_abs_actual * 100 if mean_abs_actual > 1e-20 else 0.0
        pct_transport = mean_abs_transport / mean_abs_actual * 100 if mean_abs_actual > 1e-20 else 0.0

        results[var_name] = {
            'status': SEV_OK if not np.isnan(corr) else SEV_WARNING,
            'correlation': float(corr) if not np.isnan(corr) else 0.0,
            'rmse': rmse,
            'mean_abs_kinetic': mean_abs_kin,
            'mean_abs_numerical': mean_abs_actual,
            'mean_abs_transport': mean_abs_transport,
            'pct_kinetic_of_total': pct_kin,
            'pct_transport': pct_transport,
            'transport_mean_signed': float(np.mean(transport)),
            'driver': 'Kinetics' if pct_kin > 70 else ('Mixed' if pct_kin > 30 else 'Transport'),
        }

    return results


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 3: Cross-variable consistency  (extended — 15+ sub-checks)
# ═════════════════════════════════════════════════════════════════════════════
def check_3_cross_variable_consistency(time, rates, box_id):
    """Cross-variable process rate consistency — extended version."""
    issues = []

    # ── 3a. DIA growth ~ NH4 uptake correlation ────────────────────────────
    dia_c_growth = rates[:, get_slot_col(5, 1)]
    dia_nh4_uptake = rates[:, get_slot_col(1, 6)]
    if np.std(dia_c_growth) > 0 and np.std(dia_nh4_uptake) > 0:
        corr = float(np.corrcoef(dia_c_growth, dia_nh4_uptake)[0, 1])
        issues.append({
            'check': 'DIA_C growth ~ NH4 uptake by DIA',
            'correlation': corr,
            'severity': SEV_OK if corr > 0.9 else SEV_WARNING,
        })

    # ── 3b. ALL phyto death == DET_PART_ORG_C source ──────────────────────
    phyto_death_checks = [
        ('DIA_C', 5, 4, 9, 1, 'DIA death == DET_C DIA death input'),
        ('CYN_C', 15, 4, 9, 2, 'CYN death == DET_C CYN death input'),
        ('OPA_C', 16, 4, 9, 3, 'OPA death == DET_C OPA death input'),
        ('FIX_CYN_C', 19, 4, 9, 4, 'FIX_CYN death == DET_C FIX_CYN death input'),
    ]
    for name, src_vidx, src_slot, dst_vidx, dst_slot, desc in phyto_death_checks:
        src = rates[:, get_slot_col(src_vidx, src_slot)]
        dst = rates[:, get_slot_col(dst_vidx, dst_slot)]
        max_diff = float(np.max(np.abs(src - dst)))
        issues.append({
            'check': desc,
            'max_difference': max_diff,
            'severity': SEV_OK if max_diff < 1e-10 else SEV_ERROR,
            'active': bool(np.any(np.abs(src) > 1e-15)),
        })

    # ── 3c. ZOO feeding matchups (all 5 prey + DET) ──────────────────────
    feeding_checks = [
        (6, 5, 5, 5, 'ZOO feeding DIA == DIA grazing loss'),
        (6, 6, 15, 5, 'ZOO feeding CYN == CYN grazing loss'),
        (6, 7, 16, 5, 'ZOO feeding OPA == OPA grazing loss'),
        (6, 8, 19, 5, 'ZOO feeding FIX_CYN == FIX_CYN grazing loss'),
        (6, 9, 9, 6, 'ZOO feeding DET == DET grazing loss'),
    ]
    for zoo_vidx, zoo_slot, prey_vidx, prey_slot, desc in feeding_checks:
        zoo_data = rates[:, get_slot_col(zoo_vidx, zoo_slot)]
        prey_data = rates[:, get_slot_col(prey_vidx, prey_slot)]
        max_diff = float(np.max(np.abs(zoo_data - prey_data)))
        issues.append({
            'check': desc,
            'max_difference': max_diff,
            'severity': SEV_OK if max_diff < 1e-10 else SEV_ERROR,
            'active': bool(np.any(np.abs(zoo_data) > 1e-15)),
        })

    # ── 3d. ZOO death == DET_C input from zoo death ──────────────────────
    zoo_death = rates[:, get_slot_col(6, 4)]
    det_zoo_death = rates[:, get_slot_col(9, 5)]
    max_diff = float(np.max(np.abs(zoo_death - det_zoo_death)))
    issues.append({
        'check': 'ZOO_C death == DET_C zoo death input',
        'max_difference': max_diff,
        'severity': SEV_OK if max_diff < 1e-10 else SEV_ERROR,
        'active': bool(np.any(np.abs(zoo_death) > 1e-15)),
    })

    # ── 3e. Nitrification: NH4 loss == NO3 gain ─────────────────────────
    nh4_nitr = rates[:, get_slot_col(1, 10)]
    no3_nitr = rates[:, get_slot_col(2, 1)]
    max_diff = float(np.max(np.abs(nh4_nitr - no3_nitr)))
    issues.append({
        'check': 'Nitrification: NH4 loss == NO3 gain',
        'max_difference': max_diff,
        'severity': SEV_OK if max_diff < 1e-10 else SEV_ERROR,
    })

    # ── 3f. DOC dissolution: DET_C diss == DOC gain from diss ────────────
    det_diss = rates[:, get_slot_col(9, 7)]
    doc_from_diss = rates[:, get_slot_col(12, 1)]
    max_diff = float(np.max(np.abs(det_diss - doc_from_diss)))
    issues.append({
        'check': 'DET_C dissolution == DOC gain from dissolution',
        'max_difference': max_diff,
        'severity': SEV_OK if max_diff < 1e-10 else SEV_WARNING,
    })

    # ── 3g. DOC aerobic mineralization O2:C ──────────────────────────────
    doc_total_min = rates[:, get_slot_col(12, 3)]   # DOC slot 3 = total min
    do_doc_min = rates[:, get_slot_col(4, 12)]       # DO slot 12 = aerobic DOC min
    mask = np.abs(doc_total_min) > 1e-15
    if np.any(mask):
        implied = do_doc_min[mask] / doc_total_min[mask]
        mean_ratio = float(np.mean(implied))
        reasonable = 0.0 <= mean_ratio <= (DEFAULT_O2_TO_C + 0.1)
        issues.append({
            'check': 'DOC aerobic O2:C = DO_slot12/DOC_slot3 (total min)',
            'mean_ratio': mean_ratio,
            'max_possible': DEFAULT_O2_TO_C,
            'severity': SEV_OK if reasonable else SEV_WARNING,
            'note': 'Ratio <= 2.66 expected (aerobic fraction of total)',
        })

    # ── 3h. DET dissolution stoichiometry (N:C, P:C) ────────────────────
    det_c_diss = rates[:, get_slot_col(9, 7)]
    det_n_diss = rates[:, get_slot_col(10, 7)]
    det_p_diss = rates[:, get_slot_col(11, 7)]
    mask_d = np.abs(det_c_diss) > 1e-15
    if np.any(mask_d):
        nc_diss = det_n_diss[mask_d] / det_c_diss[mask_d]
        pc_diss = det_p_diss[mask_d] / det_c_diss[mask_d]
        issues.append({
            'check': 'DET dissolution N:C ratio',
            'mean': float(np.mean(nc_diss)),
            'min': float(np.min(nc_diss)),
            'max': float(np.max(nc_diss)),
            'severity': SEV_INFO,
            'note': 'Uses dynamic DET N:C (variable over time)',
        })
        issues.append({
            'check': 'DET dissolution P:C ratio',
            'mean': float(np.mean(pc_diss)),
            'min': float(np.min(pc_diss)),
            'max': float(np.max(pc_diss)),
            'severity': SEV_INFO,
        })

    # ── 3i. Phyto excretion sum == DOC total excretion ───────────────────
    dia_excr = rates[:, get_slot_col(5, 3)]
    doc_total_excr = rates[:, get_slot_col(12, 4)]  # DOC slot 4 = total phyto DOC excretion
    doc_dia_excr = rates[:, get_slot_col(12, 5)]
    doc_cyn_excr = rates[:, get_slot_col(12, 6)]
    doc_opa_excr = rates[:, get_slot_col(12, 7)]
    doc_fix_excr = rates[:, get_slot_col(12, 8)]
    doc_nost_excr = rates[:, get_slot_col(12, 9)]
    sum_aux = doc_dia_excr + doc_cyn_excr + doc_opa_excr + doc_fix_excr + doc_nost_excr
    max_diff = float(np.max(np.abs(sum_aux - doc_total_excr)))
    # Use tolerance appropriate for text output precision (6 decimal places).
    # Differences up to ~1e-5 are expected from independent rounding of each slot.
    max_total = float(np.max(np.abs(doc_total_excr)))
    tol = max(1e-5, 1e-4 * max_total)  # absolute or 0.01% relative
    issues.append({
        'check': 'Sum(DOC phyto excr slots 5-9) == DOC slot 4 (total excretion)',
        'max_difference': max_diff,
        'severity': SEV_OK if max_diff < tol else SEV_WARNING,
        'active': bool(np.any(np.abs(doc_total_excr) > 1e-15)),
    })
    # DIA excretion cross-check
    max_diff_dia = float(np.max(np.abs(dia_excr - doc_dia_excr)))
    issues.append({
        'check': 'DIA_C excretion (slot 3) == DOC slot 5 (DIA excr aux)',
        'max_difference': max_diff_dia,
        'severity': SEV_OK if max_diff_dia < 1e-10 else SEV_ERROR,
        'active': bool(np.any(np.abs(dia_excr) > 1e-15)),
    })

    # ── 3j. DIA N uptake (NH4+NO3) == growth × N:C ──────────────────────
    nh4_dia_uptake = rates[:, get_slot_col(1, 6)]
    no3_dia_uptake = rates[:, get_slot_col(2, 3)]
    dia_growth = rates[:, get_slot_col(5, 1)]
    expected_n = dia_growth * DEFAULT_N_TO_C
    actual_n = nh4_dia_uptake + no3_dia_uptake
    mask_g = np.abs(dia_growth) > 1e-15
    if np.any(mask_g):
        max_diff_n = float(np.max(np.abs(expected_n[mask_g] - actual_n[mask_g])))
        issues.append({
            'check': 'DIA N uptake (NH4+NO3) == DIA growth × N:C',
            'max_difference': max_diff_n,
            'severity': SEV_OK if max_diff_n < 1e-5 else SEV_ERROR,
            'note': 'Tolerance 1e-5 for text output precision',
        })

    # ── 3k. NOST death → DET (probe slots 8-10) ─────────────────────────
    nost_death = rates[:, get_slot_col(31, 4)]
    for s in [8, 9, 10]:
        det_s = rates[:, get_slot_col(9, s)]
        c = float(np.corrcoef(nost_death, det_s)[0, 1]) \
            if np.std(nost_death) > 0 and np.std(det_s) > 0 else 0.0
        if c > 0.9:
            max_diff = float(np.max(np.abs(nost_death - det_s)))
            issues.append({
                'check': f'NOST death → DET_C slot {s} (corr={c:.4f})',
                'max_difference': max_diff,
                'severity': SEV_OK if max_diff < 1e-10 else SEV_WARNING,
            })
            break

    # ── 3l. ZOO_N budget ────────────────────────────────────────────────
    zoo_n_in_dia  = rates[:, get_slot_col(7, 1)]
    zoo_n_in_cyn  = rates[:, get_slot_col(7, 2)]
    zoo_n_in_opa  = rates[:, get_slot_col(7, 3)]
    zoo_n_in_fix  = rates[:, get_slot_col(7, 4)]
    zoo_n_in_det  = rates[:, get_slot_col(7, 5)]
    zoo_n_in_nost = rates[:, get_slot_col(7, 10)]
    zoo_n_out_don = rates[:, get_slot_col(7, 6)]
    zoo_n_out_resp = rates[:, get_slot_col(7, 7)]
    zoo_n_out_mort = rates[:, get_slot_col(7, 8)]

    total_n_in = zoo_n_in_dia + zoo_n_in_cyn + zoo_n_in_opa + zoo_n_in_fix + zoo_n_in_det + zoo_n_in_nost
    total_n_out = zoo_n_out_don + zoo_n_out_resp + zoo_n_out_mort
    net_zoo_n = total_n_in - total_n_out

    if np.any(np.abs(total_n_in) > 1e-20):
        issues.append({
            'check': 'ZOO_N budget: ingestion vs losses',
            'mean_N_ingestion': float(np.mean(total_n_in)),
            'mean_N_losses': float(np.mean(total_n_out)),
            'mean_net_change': float(np.mean(net_zoo_n)),
            'severity': SEV_INFO,
        })

    # ── 3m. O2 reaeration ───────────────────────────────────────────────
    o2_reaer = rates[:, get_slot_col(4, 1)]
    issues.append({
        'check': f'O2 reaeration in box {box_id}',
        'mean_reaeration': float(np.mean(o2_reaer)),
        'max_reaeration': float(np.max(o2_reaer)),
        'severity': SEV_OK if abs(np.mean(o2_reaer)) > 0 else SEV_INFO,
    })

    return issues


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 4: Bug fix verification
# ═════════════════════════════════════════════════════════════════════════════
def check_4_bug_fix_verification(time, rates, box_id):
    """Verify the FIX_CYN O2 production bug fix and DON NOST uptake."""
    results = {}

    fix_cyn_o2 = rates[:, get_slot_col(4, 19)]
    nost_o2_prod = rates[:, get_slot_col(4, 5)]
    fix_cyn_growth = rates[:, get_slot_col(19, 1)]

    results['fix_cyn_o2_production'] = {
        'slot_19_max': float(np.max(fix_cyn_o2)),
        'slot_19_mean': float(np.mean(fix_cyn_o2)),
        'slot_19_pct_nonzero': float(np.count_nonzero(fix_cyn_o2) / len(fix_cyn_o2) * 100),
        'fix_cyn_growth_max': float(np.max(fix_cyn_growth)),
        'nost_o2_prod_slot5_max': float(np.max(nost_o2_prod)),
        'status': 'FIXED' if np.max(fix_cyn_o2) > 1e-10 else 'CHECK',
    }

    don_slot6 = rates[:, get_slot_col(13, 6)]
    results['don_nost_uptake'] = {
        'slot_6_max': float(np.max(np.abs(don_slot6))),
        'slot_6_mean': float(np.mean(don_slot6)),
        'status': SEV_INFO,
    }

    fix_cyn_n_fix = rates[:, get_slot_col(19, 8)]
    nost_n_fix_slot9 = rates[:, get_slot_col(31, 9)]
    results['n_fixation'] = {
        'fix_cyn_n_fix_max': float(np.max(fix_cyn_n_fix)),
        'fix_cyn_n_fix_mean': float(np.mean(fix_cyn_n_fix)),
        'nost_n_fix_max': float(np.max(nost_n_fix_slot9)),
        'nost_n_fix_mean': float(np.mean(nost_n_fix_slot9)),
        'status': SEV_INFO,
    }

    return results


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 5: Dominant processes
# ═════════════════════════════════════════════════════════════════════════════
def check_5_dominant_processes(time, rates, box_id):
    """Identify dominant processes per key variable."""
    results = {}

    for var_name in KEY_VARS:
        if var_name not in SLOT_MAP:
            continue
        info = SLOT_MAP[var_name]
        var_idx = info['var_index']
        signs = DERIVATIVE_SIGNS.get(var_name, {})
        if not signs:
            continue

        slot_contributions = {}
        for slot, sign in signs.items():
            col = get_slot_col(var_idx, slot)
            if col < rates.shape[1]:
                data = rates[:, col]
                mean_abs = float(np.mean(np.abs(data)))
                slot_desc = info['slots'].get(slot, f'slot {slot}')
                clean_desc = slot_desc.split('(')[0].strip() if slot_desc else f'slot_{slot}'
                slot_contributions[slot] = {
                    'mean_abs': mean_abs,
                    'sign': sign,
                    'desc': clean_desc,
                    'signed_mean': float(np.mean(data)) * sign,
                }

        sorted_slots = sorted(slot_contributions.items(), key=lambda x: x[1]['mean_abs'], reverse=True)
        total = sum(v['mean_abs'] for v in slot_contributions.values())
        dominant = []
        for slot, sc in sorted_slots[:5]:
            pct = sc['mean_abs'] / total * 100 if total > 0 else 0
            dominant.append({
                'slot': slot, 'desc': sc['desc'],
                'mean_abs_rate': sc['mean_abs'],
                'pct_of_total': pct,
                'net_contribution': sc['signed_mean'],
            })

        results[var_name] = {'total_rate_magnitude': total, 'dominant_processes': dominant}

    return results


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 6: Seasonal patterns
# ═════════════════════════════════════════════════════════════════════════════
def check_6_seasonal_patterns(time, rates, box_id):
    """Seasonal patterns of key processes."""
    results = {}
    doy = (time - time[0]) % 365.0
    winter = (doy < 90) | (doy >= 335)
    spring = (doy >= 90) & (doy < 152)
    summer = (doy >= 152) & (doy < 244)
    autumn = (doy >= 244) & (doy < 335)
    season_masks = {'winter': winter, 'spring': spring, 'summer': summer, 'autumn': autumn}

    processes = [
        ('DIA growth', 5, 1), ('CYN growth', 15, 1), ('OPA growth', 16, 1),
        ('FIX_CYN growth', 19, 1), ('NOST growth', 31, 1),
        ('O2 reaeration', 4, 1), ('ZOO growth', 6, 1), ('Nitrification', 1, 10),
    ]

    for desc, var_idx, slot in processes:
        col = get_slot_col(var_idx, slot)
        if col >= rates.shape[1]:
            continue
        data = rates[:, col]
        seasonal = {}
        for sname, mask in season_masks.items():
            if np.any(mask):
                seasonal[sname] = float(np.mean(data[mask]))
        results[desc] = seasonal

    return results


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 7: Zero slot analysis
# ═════════════════════════════════════════════════════════════════════════════
def check_7_zero_slot_analysis(rates, box_id):
    """Identify derivative slots that are perpetually zero."""
    findings = []
    for var_name in KEY_VARS:
        if var_name not in SLOT_MAP:
            continue
        info = SLOT_MAP[var_name]
        var_idx = info['var_index']
        signs = DERIVATIVE_SIGNS.get(var_name, {})

        for slot in info.get('derivative_slots_used', []):
            col = get_slot_col(var_idx, slot)
            if col >= rates.shape[1]:
                continue
            data = rates[:, col]
            if np.all(data == 0.0):
                desc = info['slots'].get(slot, f'slot {slot}')
                expected_zero = 'ADVANCED_REDOX' in desc or var_name in ('CA', 'MG')
                findings.append({
                    'variable': var_name, 'slot': slot, 'desc': desc,
                    'expected_zero': expected_zero,
                    'in_derivative': slot in signs,
                })
    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 8: Limitation factors
# ═════════════════════════════════════════════════════════════════════════════
def check_8_limitation_factors(rates, box_id):
    """Phytoplankton limitation factors."""
    results = {}

    # DIA (var 5, slots 6-11)
    dia_lims = {}
    for i, name in enumerate(['temp', 'doxy', 'N', 'P', 'Si', 'light']):
        col = get_slot_col(5, 6 + i)
        data = rates[:, col]
        dia_lims[name] = {'mean': float(np.mean(data)), 'min': float(np.min(data)), 'max': float(np.max(data))}
    results['DIA_limitations'] = dia_lims

    # CYN (var 15, slots 6-10)
    cyn_lims = {}
    for i, name in enumerate(['temp', 'doxy', 'N', 'P', 'light']):
        col = get_slot_col(15, 6 + i)
        data = rates[:, col]
        cyn_lims[name] = {'mean': float(np.mean(data)), 'min': float(np.min(data)), 'max': float(np.max(data))}
    results['CYN_limitations'] = cyn_lims

    # OPA (var 16, slots 6-10)
    opa_lims = {}
    for i, name in enumerate(['temp', 'doxy', 'N', 'P', 'light']):
        col = get_slot_col(16, 6 + i)
        data = rates[:, col]
        opa_lims[name] = {'mean': float(np.mean(data)), 'min': float(np.min(data)), 'max': float(np.max(data))}
    results['OPA_limitations'] = opa_lims

    # NOST (var 31, slots 11-16)
    nost_lim_map = {'light': 11, 'temp': 12, 'doxy': 13, 'P': 14, 'N': 16}
    nost_lims = {}
    for name, slot in nost_lim_map.items():
        col = get_slot_col(31, slot)
        data = rates[:, col]
        nost_lims[name] = {'mean': float(np.mean(data)), 'min': float(np.min(data)), 'max': float(np.max(data))}
    results['NOST_limitations'] = nost_lims

    return results


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 9: Negative / impossible concentrations  [NEW]
# ═════════════════════════════════════════════════════════════════════════════
def check_9_negative_concentrations(sv_time, sv_concs, sv_names, box_id):
    """Detect negative and physically unreasonable concentrations."""
    findings = []
    for var in NON_NEGATIVE_VARS:
        if var not in sv_names:
            continue
        col = sv_names.index(var)
        vals = sv_concs[:, col]
        neg_mask = vals < 0.0
        n_neg = int(np.sum(neg_mask))
        if n_neg > 0:
            findings.append({
                'variable': var,
                'n_negative': n_neg,
                'pct_negative': n_neg / len(vals) * 100,
                'min_value': float(np.min(vals)),
                'first_time': float(sv_time[np.argmax(neg_mask)]),
                'severity': SEV_ERROR,
            })
        max_val = float(np.max(vals))
        if var in ('DIA_C', 'CYN_C', 'OPA_C', 'FIX_CYN_C', 'NOST_VEG_HET_C') and max_val > 100:
            findings.append({
                'variable': var, 'max_value': max_val,
                'note': 'Phyto C > 100 mg/L seems very high',
                'severity': SEV_WARNING,
            })
        if var == 'DISS_OXYGEN':
            if max_val > 25:
                findings.append({
                    'variable': var, 'max_value': max_val,
                    'note': 'DO > 25 mg/L is physically unreasonable',
                    'severity': SEV_WARNING,
                })
            if float(np.min(vals)) < 0:
                findings.append({
                    'variable': var, 'min_value': float(np.min(vals)),
                    'note': 'Negative dissolved oxygen',
                    'severity': SEV_ERROR,
                })
    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 10: NaN / Inf detection  [NEW]
# ═════════════════════════════════════════════════════════════════════════════
def check_10_nan_inf(sv_concs, sv_names, pr_rates, box_id):
    """Systematic NaN/Inf scan of state variables and process rates."""
    findings = []
    for i, var in enumerate(sv_names):
        vals = sv_concs[:, i]
        n_nan = int(np.sum(np.isnan(vals)))
        n_inf = int(np.sum(np.isinf(vals)))
        if n_nan > 0 or n_inf > 0:
            findings.append({
                'type': 'state_variable', 'variable': var,
                'n_nan': n_nan, 'n_inf': n_inf,
                'severity': SEV_ERROR,
            })

    n_nan_pr = int(np.sum(np.isnan(pr_rates)))
    n_inf_pr = int(np.sum(np.isinf(pr_rates)))
    if n_nan_pr > 0 or n_inf_pr > 0:
        for var_name, info in SLOT_MAP.items():
            var_idx = info['var_index']
            for slot in info['slots']:
                col = get_slot_col(var_idx, slot)
                if col < pr_rates.shape[1]:
                    data = pr_rates[:, col]
                    nn = int(np.sum(np.isnan(data)))
                    ni = int(np.sum(np.isinf(data)))
                    if nn > 0 or ni > 0:
                        findings.append({
                            'type': 'process_rate', 'variable': var_name, 'slot': slot,
                            'n_nan': nn, 'n_inf': ni,
                            'severity': SEV_ERROR,
                        })
    else:
        findings.append({'type': 'process_rates_bulk', 'n_nan': 0, 'n_inf': 0, 'severity': SEV_OK})
    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 11: Stoichiometric ratio validation  [NEW — median-based]
# ═════════════════════════════════════════════════════════════════════════════
def check_11_stoichiometry(sv_concs, sv_names, pr_rates, box_id):
    """Validate stoichiometric ratios from state variables and process rates."""
    findings = []

    def _sv_col(name):
        return sv_names.index(name) if name in sv_names else None

    # ── ZOO actual N:C and P:C ──────────────────────────────────────────
    zc, zn, zp = _sv_col('ZOO_C'), _sv_col('ZOO_N'), _sv_col('ZOO_P')
    if zc is not None and zn is not None and zp is not None:
        zoo_c = sv_concs[:, zc]
        zoo_n = sv_concs[:, zn]
        zoo_p = sv_concs[:, zp]
        mask = zoo_c > 1e-6
        if np.any(mask):
            nc = zoo_n[mask] / zoo_c[mask]
            pc = zoo_p[mask] / zoo_c[mask]
            findings.append({
                'check': 'ZOO N:C ratio', 'mean': float(np.mean(nc)),
                'min': float(np.min(nc)), 'max': float(np.max(nc)),
                'expected_default': DEFAULT_N_TO_C,
                'note': 'Dynamic ratio, may differ from default 0.22',
                'severity': SEV_INFO if 0.05 < np.mean(nc) < 0.6 else SEV_WARNING,
            })
            findings.append({
                'check': 'ZOO P:C ratio', 'mean': float(np.mean(pc)),
                'min': float(np.min(pc)), 'max': float(np.max(pc)),
                'expected_default': DEFAULT_P_TO_C,
                'severity': SEV_INFO if 0.005 < np.mean(pc) < 0.1 else SEV_WARNING,
            })
            nc_cv = float(np.std(nc) / np.mean(nc) * 100)
            findings.append({
                'check': 'ZOO N:C coefficient of variation',
                'cv_pct': nc_cv,
                'severity': SEV_INFO if nc_cv < 50 else SEV_WARNING,
            })

    # ── DET actual N:C and P:C ──────────────────────────────────────────
    dc, dn, dp = _sv_col('DET_PART_ORG_C'), _sv_col('DET_PART_ORG_N'), _sv_col('DET_PART_ORG_P')
    if dc is not None and dn is not None and dp is not None:
        det_c = sv_concs[:, dc]
        det_n = sv_concs[:, dn]
        det_p = sv_concs[:, dp]
        mask = det_c > 1e-6
        if np.any(mask):
            nc = det_n[mask] / det_c[mask]
            pc = det_p[mask] / det_c[mask]
            findings.append({
                'check': 'DET N:C ratio', 'mean': float(np.mean(nc)),
                'min': float(np.min(nc)), 'max': float(np.max(nc)),
                'expected_default': DEFAULT_N_TO_C,
                'severity': SEV_INFO if 0.05 < np.mean(nc) < 0.6 else SEV_WARNING,
            })
            findings.append({
                'check': 'DET P:C ratio', 'mean': float(np.mean(pc)),
                'min': float(np.min(pc)), 'max': float(np.max(pc)),
                'expected_default': DEFAULT_P_TO_C,
                # DET P:C can be much lower than phyto Redfield because P
                # dissolves faster than C (preferential recycling).  Only
                # flag WARNING if the ratio is implausibly outside [0, 0.2].
                'severity': SEV_INFO if np.mean(pc) < 0.2 and np.mean(pc) >= 0.0 else SEV_WARNING,
                'note': 'Detritus P:C is dynamic — typically << Redfield due to faster P dissolution',
            })

    # ── DON:DOC and DOP:DOC ─────────────────────────────────────────────
    doc_col, don_col, dop_col = _sv_col('DISS_ORG_C'), _sv_col('DISS_ORG_N'), _sv_col('DISS_ORG_P')
    if doc_col is not None and don_col is not None and dop_col is not None:
        diss_c = sv_concs[:, doc_col]
        diss_n = sv_concs[:, don_col]
        diss_p = sv_concs[:, dop_col]
        mask = diss_c > 1e-6
        if np.any(mask):
            findings.append({
                'check': 'DISS_ORG N:C ratio',
                'mean': float(np.mean(diss_n[mask] / diss_c[mask])),
                'min': float(np.min(diss_n[mask] / diss_c[mask])),
                'max': float(np.max(diss_n[mask] / diss_c[mask])),
                'severity': SEV_INFO,
            })
            findings.append({
                'check': 'DISS_ORG P:C ratio',
                'mean': float(np.mean(diss_p[mask] / diss_c[mask])),
                'min': float(np.min(diss_p[mask] / diss_c[mask])),
                'max': float(np.max(diss_p[mask] / diss_c[mask])),
                'severity': SEV_INFO,
            })

    # ── DIA resp implied N:C (median-based) ─────────────────────────────
    nh4_dia_resp = pr_rates[:, get_slot_col(1, 1)]
    dia_c_resp = pr_rates[:, get_slot_col(5, 2)]
    mask = np.abs(dia_c_resp) > 1e-15
    if np.any(mask):
        implied_nc = nh4_dia_resp[mask] / dia_c_resp[mask]
        median_nc = float(np.median(implied_nc))
        match_ok = abs(median_nc - DEFAULT_N_TO_C) < 0.005
        findings.append({
            'check': 'DIA resp implied N:C = NH4_slot1/DIA_C_slot2',
            'median': median_nc, 'mean': float(np.mean(implied_nc)),
            'std': float(np.std(implied_nc)),
            'expected': DEFAULT_N_TO_C, 'match': match_ok,
            'severity': SEV_OK if match_ok else SEV_ERROR,
            'note': 'Median-based (text precision causes scatter at small rates)',
        })

    # ── DIA resp implied P:C (median-based) ─────────────────────────────
    po4_dia_resp = pr_rates[:, get_slot_col(3, 1)]
    if np.any(mask):
        implied_pc = po4_dia_resp[mask] / dia_c_resp[mask]
        median_pc = float(np.median(implied_pc))
        match_ok = abs(median_pc - DEFAULT_P_TO_C) < 0.002
        findings.append({
            'check': 'DIA resp implied P:C = PO4_slot1/DIA_C_slot2',
            'median': median_pc, 'expected': DEFAULT_P_TO_C, 'match': match_ok,
            'severity': SEV_OK if match_ok else SEV_ERROR,
            'note': 'Median-based check',
        })

    # ── DIA growth O2:C (includes PREF factor) ─────────────────────────
    dia_growth = pr_rates[:, get_slot_col(5, 1)]
    do_dia_prod = pr_rates[:, get_slot_col(4, 2)]
    mask_g = np.abs(dia_growth) > 1e-15
    if np.any(mask_g):
        implied_o2c = do_dia_prod[mask_g] / dia_growth[mask_g]
        findings.append({
            'check': 'DIA growth O2:C = DO_slot2/DIA_slot1 (includes PREF)',
            'mean': float(np.mean(implied_o2c)),
            'min': float(np.min(implied_o2c)),
            'max': float(np.max(implied_o2c)),
            'expected_range': '2.66 (all NH4) to 3.458 (all NO3)',
            'severity': SEV_INFO,
            'note': 'DO slot2 = growth*(1.3-0.3*PREF)*O2:C — varies with NH4 preference',
        })

    # ── DIA death Si:C (PART_Si slot 1 = DIA death × Si:C) ─────────────
    part_si_death = pr_rates[:, get_slot_col(18, 1)]
    dia_death = pr_rates[:, get_slot_col(5, 4)]
    mask_dd = np.abs(dia_death) > 1e-15
    if np.any(mask_dd):
        implied_sic = part_si_death[mask_dd] / dia_death[mask_dd]
        median_sic = float(np.median(implied_sic))
        match_ok = abs(median_sic - DEFAULT_Si_TO_C) < 0.01
        findings.append({
            'check': 'DIA death Si:C = PART_Si_slot1/DIA_slot4',
            'median': median_sic, 'expected': DEFAULT_Si_TO_C, 'match': match_ok,
            'severity': SEV_OK if match_ok else SEV_ERROR,
            'note': 'PART_Si slot 1 = DIA death → particulate Si. Median-based.',
        })

    # ── CYN resp implied N:C ────────────────────────────────────────────
    nh4_cyn_resp = pr_rates[:, get_slot_col(1, 2)]
    cyn_c_resp = pr_rates[:, get_slot_col(15, 2)]
    mask_c = np.abs(cyn_c_resp) > 1e-15
    if np.any(mask_c):
        implied_nc = nh4_cyn_resp[mask_c] / cyn_c_resp[mask_c]
        median_nc = float(np.median(implied_nc))
        match_ok = abs(median_nc - DEFAULT_N_TO_C) < 0.005
        findings.append({
            'check': 'CYN resp implied N:C',
            'median': median_nc, 'expected': DEFAULT_N_TO_C, 'match': match_ok,
            'severity': SEV_OK if match_ok else SEV_ERROR,
        })

    # ── OPA resp implied N:C ────────────────────────────────────────────
    nh4_opa_resp = pr_rates[:, get_slot_col(1, 3)]
    opa_c_resp = pr_rates[:, get_slot_col(16, 2)]
    mask_o = np.abs(opa_c_resp) > 1e-15
    if np.any(mask_o):
        implied_nc = nh4_opa_resp[mask_o] / opa_c_resp[mask_o]
        median_nc = float(np.median(implied_nc))
        match_ok = abs(median_nc - DEFAULT_N_TO_C) < 0.005
        findings.append({
            'check': 'OPA resp implied N:C',
            'median': median_nc, 'expected': DEFAULT_N_TO_C, 'match': match_ok,
            'severity': SEV_OK if match_ok else SEV_ERROR,
        })

    # ── ZOO resp N:C (actual — variable) ────────────────────────────────
    nh4_zoo_resp = pr_rates[:, get_slot_col(1, 5)]
    zoo_c_resp = pr_rates[:, get_slot_col(6, 3)]
    mask_z = np.abs(zoo_c_resp) > 1e-15
    if np.any(mask_z):
        actual_nc = nh4_zoo_resp[mask_z] / zoo_c_resp[mask_z]
        findings.append({
            'check': 'ZOO resp actual N:C = NH4_slot5/ZOO_C_slot3',
            'mean': float(np.mean(actual_nc)),
            'min': float(np.min(actual_nc)),
            'max': float(np.max(actual_nc)),
            'note': 'Should be >= 0.5*default (0.11) and variable',
            'severity': SEV_INFO if np.mean(actual_nc) > 0.05 else SEV_WARNING,
        })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 12: Process rate sign violations  [NEW — with smart downgrade]
# ═════════════════════════════════════════════════════════════════════════════
def check_12_sign_violations(pr_rates, box_id):
    """Verify growth/death/feeding rates are non-negative; smart downgrade for artifacts."""
    findings = []
    for var_name, slot, desc in NONNEG_RATE_SLOTS:
        if var_name not in SLOT_MAP:
            continue
        var_idx = SLOT_MAP[var_name]['var_index']
        col = get_slot_col(var_idx, slot)
        if col >= pr_rates.shape[1]:
            continue
        data = pr_rates[:, col]
        n_neg = int(np.sum(data < -1e-15))
        if n_neg > 0:
            min_val = float(np.min(data))
            # Smart downgrade: single-timestep tiny negatives are known artifacts
            if n_neg <= 2 and abs(min_val) < 1e-3:
                severity = SEV_WARNING
            else:
                severity = SEV_ERROR
            findings.append({
                'variable': var_name, 'slot': slot, 'desc': desc,
                'n_negative': n_neg,
                'pct_negative': n_neg / len(data) * 100,
                'min_value': min_val,
                'severity': severity,
            })
    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 13: Euler integration + transport decomposition  [NEW]
# ═════════════════════════════════════════════════════════════════════════════
def check_13_euler_integration(sv_time, sv_concs, sv_names, pr_rates, box_id):
    """Integrate kinetic dC/dt forward (Euler) and decompose actual vs kinetic."""
    findings = []
    dt = np.median(np.diff(sv_time))

    euler_vars = [
        'NH4_N', 'NO3_N', 'PO4_P', 'DISS_OXYGEN', 'DIA_C',
        'ZOO_C', 'DET_PART_ORG_C', 'DISS_ORG_C', 'CYN_C', 'OPA_C',
        'FIX_CYN_C', 'INORG_C', 'TOT_ALK', 'DISS_Si', 'PART_Si',
        'NOST_VEG_HET_C', 'AKI_C',
    ]

    for var in euler_vars:
        col = find_sv_column(var, sv_names)
        if col is None or var not in SLOT_MAP:
            continue
        conc = sv_concs[:, col]
        kin_deriv = compute_kinetic_deriv(pr_rates, var, SLOT_MAP, DERIVATIVE_SIGNS)

        n = min(len(conc) - 1, len(kin_deriv))
        if n <= 0:
            continue

        actual_dC = np.diff(conc[:n + 1])
        kin_dC = kin_deriv[:n] * dt
        transport = actual_dC - kin_dC

        mean_actual = float(np.mean(np.abs(actual_dC)))
        mean_kin = float(np.mean(np.abs(kin_dC)))
        mean_transport = float(np.mean(np.abs(transport)))
        corr = float(np.corrcoef(kin_dC, actual_dC)[0, 1]) \
            if np.std(kin_dC) > 0 and np.std(actual_dC) > 0 else 0.0

        pct_kin = mean_kin / mean_actual * 100 if mean_actual > 1e-20 else 0.0
        pct_transport = mean_transport / mean_actual * 100 if mean_actual > 1e-20 else 0.0

        findings.append({
            'variable': var, 'dt': float(dt),
            'mean_abs_actual_dC': mean_actual,
            'mean_abs_kinetic_dC': mean_kin,
            'mean_abs_transport_residual': mean_transport,
            'pct_kinetic': pct_kin,
            'pct_transport': pct_transport,
            'correlation_kin_actual': float(corr) if not np.isnan(corr) else 0.0,
            'transport_mean_signed': float(np.mean(transport)),
            'severity': SEV_OK,
        })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 14: Allelopathy state-variable consistency  [NEW]
# ═════════════════════════════════════════════════════════════════════════════
def check_14_allelopathy(sv_concs, sv_names, pr_rates, box_id):
    """Validate SEC_METAB_* variables and their process rates."""
    findings = []
    allelo_vars = ['SEC_METAB_DIA', 'SEC_METAB_NOFIX_CYN', 'SEC_METAB_FIX_CYN', 'SEC_METAB_NOST']
    parent_map = {
        'SEC_METAB_DIA': 'DIA_C',
        'SEC_METAB_NOFIX_CYN': 'CYN_C',
        'SEC_METAB_FIX_CYN': 'FIX_CYN_C',
        'SEC_METAB_NOST': 'NOST_VEG_HET_C',
    }
    allelo_idx_map = {
        'SEC_METAB_DIA': 33, 'SEC_METAB_NOFIX_CYN': 34,
        'SEC_METAB_FIX_CYN': 35, 'SEC_METAB_NOST': 36,
    }

    for var in allelo_vars:
        if var not in sv_names:
            continue
        col = sv_names.index(var)
        vals = sv_concs[:, col]
        n_neg = int(np.sum(vals < 0))

        parent = parent_map[var]
        parent_col = sv_names.index(parent) if parent in sv_names else None
        corr_parent = 0.0
        if parent_col is not None:
            pv = sv_concs[:, parent_col]
            if np.std(vals) > 0 and np.std(pv) > 0:
                corr_parent = float(np.corrcoef(vals, pv)[0, 1])

        findings.append({
            'variable': var, 'parent': parent,
            'mean': float(np.mean(vals)), 'max': float(np.max(vals)),
            'n_negative': n_neg,
            'correlation_with_parent': corr_parent,
            'always_zero': bool(np.all(np.abs(vals) < 1e-15)),
            'severity': SEV_OK if n_neg == 0 else SEV_WARNING,
        })

        a_idx = allelo_idx_map[var]
        total_rate = 0.0
        for s in range(1, NDIAGVAR + 1):
            col_pr = get_slot_col(a_idx, s)
            if col_pr < pr_rates.shape[1]:
                total_rate += np.mean(np.abs(pr_rates[:, col_pr]))
        findings.append({
            'variable': f'{var} process rates (idx {a_idx})',
            'total_rate_magnitude': total_rate,
            'severity': SEV_INFO,
        })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 15: State-variable smoothness / spike detection  [NEW]
# ═════════════════════════════════════════════════════════════════════════════
def check_15_smoothness(sv_time, sv_concs, sv_names, box_id):
    """Detect sudden concentration jumps and monotonic trends."""
    findings = []
    smooth_vars = [
        'DIA_C', 'DISS_OXYGEN', 'NH4_N', 'NO3_N', 'PO4_P',
        'ZOO_C', 'INORG_C', 'TOT_ALK', 'DET_PART_ORG_C',
        'CYN_C', 'OPA_C', 'FIX_CYN_C', 'NOST_VEG_HET_C', 'AKI_C',
    ]

    for var in smooth_vars:
        if var not in sv_names:
            continue
        col = sv_names.index(var)
        vals = sv_concs[:, col]
        if len(vals) < 3:
            continue

        dC = np.diff(vals)
        mean_abs_dC = np.mean(np.abs(dC))

        # Spike detection — improved to handle near-zero-change variables.
        # Use 10×mean(|dC|) as the primary threshold (original approach), but
        # enforce a minimum absolute threshold based on the variable's dynamic
        # range so that tiny fluctuations in low-biomass species (where the
        # mean change is near zero) are not falsely flagged as spikes.
        n_spikes = 0
        spike_mask = np.zeros(len(dC), dtype=bool)
        abs_range = float(np.max(vals) - np.min(vals))
        # At minimum, a spike must exceed 0.1% of the variable's dynamic range
        min_spike_size = max(1e-10, 0.001 * abs_range)
        if mean_abs_dC > 1e-15:
            spike_threshold = max(10 * mean_abs_dC, min_spike_size)
        else:
            spike_threshold = min_spike_size
        spike_mask = np.abs(dC) > spike_threshold
        n_spikes = int(np.sum(spike_mask))

        n_pos = np.sum(dC > 0)
        n_neg_chg = np.sum(dC < 0)
        total = len(dC)
        monotonic_pct = max(n_pos, n_neg_chg) / total * 100 if total > 0 else 0

        mean_conc = float(np.mean(vals))
        range_conc = float(np.max(vals) - np.min(vals))

        if n_spikes > 0:
            spike_indices = np.where(spike_mask)[0]
            spike_times = sv_time[spike_indices + 1]
            spike_magnitudes = dC[spike_indices]
            # Spikes are WARNING only when both frequent (>10) AND
            # ecologically significant (mean spike magnitude > 1% of mean
            # concentration).  Otherwise INFO — the spikes exist but are
            # negligible relative to the variable's magnitude.
            mean_spike_mag = float(np.mean(np.abs(spike_magnitudes)))
            spike_pct = (mean_spike_mag / abs(mean_conc) * 100) if abs(mean_conc) > 1e-15 else 0.0
            sev = SEV_WARNING if n_spikes > 10 and spike_pct > 1.0 else SEV_INFO
            findings.append({
                'variable': var, 'type': 'spike',
                'n_spikes': n_spikes,
                'spike_times_first5': [float(t) for t in spike_times[:5]],
                'spike_magnitudes_first5': [float(m) for m in spike_magnitudes[:5]],
                'mean_conc': mean_conc,
                'mean_abs_daily_change': float(mean_abs_dC),
                'spike_pct_of_mean': spike_pct,
                'severity': sev,
            })

        threshold = 0.01 * abs(mean_conc) if mean_conc != 0 else 1e-6
        if monotonic_pct > 95 and range_conc > threshold:
            findings.append({
                'variable': var, 'type': 'monotonic_trend',
                'pct_same_direction': float(monotonic_pct),
                'direction': 'increasing' if n_pos > n_neg_chg else 'decreasing',
                'range': range_conc,
                'severity': SEV_INFO,
            })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 16: N/P/C mass-balance closure  [NEW]
# ═════════════════════════════════════════════════════════════════════════════
def check_16_mass_balance(pr_rates, box_id):
    """Check total N, P, C, O2 conservation via kinetic process rates."""
    findings = []

    def _kd(var):
        return compute_kinetic_deriv(pr_rates, var, SLOT_MAP, DERIVATIVE_SIGNS)

    # ── Total kinetic dN/dt ─────────────────────────────────────────────
    n_vars = ['NH4_N', 'NO3_N', 'ZOO_N', 'DET_PART_ORG_N', 'DISS_ORG_N']
    total_dN = np.zeros(pr_rates.shape[0])
    for v in n_vars:
        total_dN += _kd(v)
    phyto_n = [('DIA_C', DEFAULT_N_TO_C), ('CYN_C', DEFAULT_N_TO_C),
               ('OPA_C', DEFAULT_N_TO_C), ('FIX_CYN_C', DEFAULT_N_TO_C),
               ('NOST_VEG_HET_C', DEFAULT_N_TO_C), ('AKI_C', DEFAULT_N_TO_C)]
    for v, nc in phyto_n:
        total_dN += _kd(v) * nc

    mean_total_dN = float(np.mean(total_dN))
    mean_abs_dN = float(np.mean(np.abs(total_dN)))
    indiv_n = [np.mean(np.abs(_kd(v))) for v in n_vars]
    mean_indiv_n = float(np.mean(indiv_n))

    findings.append({
        'check': 'Total kinetic dN/dt',
        'mean_signed': mean_total_dN,
        'mean_abs': mean_abs_dN,
        'mean_individual_var_abs': mean_indiv_n,
        'ratio_total_to_individual': float(mean_abs_dN / mean_indiv_n) if mean_indiv_n > 1e-20 else 0,
        'note': 'Non-zero = N fixation, denitrification, ammonia volatilization',
        'severity': SEV_INFO,
    })

    # ── Total kinetic dP/dt ─────────────────────────────────────────────
    p_vars = ['PO4_P', 'ZOO_P', 'DET_PART_ORG_P', 'DISS_ORG_P']
    total_dP = np.zeros(pr_rates.shape[0])
    for v in p_vars:
        total_dP += _kd(v)
    for v, pc in [('DIA_C', DEFAULT_P_TO_C), ('CYN_C', DEFAULT_P_TO_C),
                   ('OPA_C', DEFAULT_P_TO_C), ('FIX_CYN_C', DEFAULT_P_TO_C),
                   ('NOST_VEG_HET_C', DEFAULT_P_TO_C), ('AKI_C', DEFAULT_P_TO_C)]:
        total_dP += _kd(v) * pc

    mean_total_dP = float(np.mean(total_dP))
    mean_abs_dP = float(np.mean(np.abs(total_dP)))
    indiv_p = [np.mean(np.abs(_kd(v))) for v in p_vars]
    mean_indiv_p = float(np.mean(indiv_p))

    findings.append({
        'check': 'Total kinetic dP/dt',
        'mean_signed': mean_total_dP,
        'mean_abs': mean_abs_dP,
        'mean_individual_var_abs': mean_indiv_p,
        'ratio_total_to_individual': float(mean_abs_dP / mean_indiv_p) if mean_indiv_p > 1e-20 else 0,
        'note': 'Non-zero = sediment-water exchange or calculation error',
        'severity': SEV_INFO if mean_abs_dP < 0.1 * mean_indiv_p else SEV_WARNING,
    })

    # ── Total kinetic dOrgC/dt and dAllC/dt ─────────────────────────────
    c_vars = ['DET_PART_ORG_C', 'DISS_ORG_C', 'DIA_C', 'CYN_C', 'OPA_C',
              'FIX_CYN_C', 'NOST_VEG_HET_C', 'AKI_C', 'ZOO_C']
    total_dOrgC = np.zeros(pr_rates.shape[0])
    for v in c_vars:
        total_dOrgC += _kd(v)
    total_dAllC = total_dOrgC + _kd('INORG_C') + _kd('CH4_C')

    indiv_c = [np.mean(np.abs(_kd(v))) for v in c_vars]
    mean_indiv_c = float(np.mean(indiv_c))

    findings.append({
        'check': 'Total kinetic dOrgC/dt (organic C budget)',
        'mean_signed': float(np.mean(total_dOrgC)),
        'note': 'Non-zero = CO2 fixation/respiration/gas exchange',
        'severity': SEV_INFO,
    })
    findings.append({
        'check': 'Total kinetic d(AllC)/dt (includes DIC+CH4)',
        'mean_signed': float(np.mean(total_dAllC)),
        'mean_abs_individual': mean_indiv_c,
        'note': 'Non-zero = atmospheric CO2/CH4 exchange, sediment flux',
        'severity': SEV_INFO,
    })

    # ── O2 budget ───────────────────────────────────────────────────────
    findings.append({
        'check': 'Mean kinetic dO2/dt',
        'mean_signed': float(np.mean(_kd('DISS_OXYGEN'))),
        'note': 'Positive = net O2 production, negative = net consumption',
        'severity': SEV_INFO,
    })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# Cross-box spatial consistency  (run after all boxes)
# ═════════════════════════════════════════════════════════════════════════════
def check_cross_box_spatial(all_sv_data):
    """Compare state variable ranges across boxes."""
    findings = []
    spatial_vars = ['DIA_C', 'DISS_OXYGEN', 'NH4_N', 'NO3_N', 'PO4_P', 'INORG_C']

    for var in spatial_vars:
        means = {}
        for box_id, (sv_time, sv_concs, sv_names) in all_sv_data.items():
            if var not in sv_names:
                continue
            col = sv_names.index(var)
            means[box_id] = float(np.mean(sv_concs[:, col]))

        if len(means) < 2:
            continue
        vals = list(means.values())
        cv = np.std(vals) / np.mean(vals) * 100 if np.mean(vals) > 1e-10 else 0
        max_ratio = max(vals) / min(vals) if min(vals) > 1e-10 else float('inf')

        findings.append({
            'variable': var, 'box_means': means,
            'cv_across_boxes_pct': float(cv),
            'max_min_ratio': float(max_ratio),
            'severity': SEV_INFO if max_ratio < 10 else SEV_WARNING,
        })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# Main analysis orchestrator
# ═════════════════════════════════════════════════════════════════════════════

def run_analysis(output_dir):
    """Run all 16 checks for all boxes and return structured results."""
    all_results = {}
    all_sv_data = {}

    for box_id in BOX_IDS:
        print(f"\n{'=' * 74}")
        print(f"  Analysing Box {box_id} ({BOX_TYPES.get(box_id, '?')})")
        print(f"{'=' * 74}")

        time, rates = load_process_rates(output_dir, box_id)
        if time is None:
            print(f"  [SKIP] No process rate file for box {box_id}")
            continue

        sv_result = load_state_vars(output_dir, box_id)
        if sv_result[0] is None:
            print(f"  [SKIP] No state variable file for box {box_id}")
            continue
        state_time, state_concs, state_names = sv_result
        all_sv_data[box_id] = (state_time, state_concs, state_names)

        print(f"  Loaded: {len(time)} timesteps, {rates.shape[1]} process rates")
        print(f"  Time range: {time[0]:.1f} - {time[-1]:.1f} days")

        box_results = {}

        # Original checks (1-8) — enhanced
        print("  [ 1/16] Rate statistics...")
        box_results['rate_statistics'] = check_1_rate_statistics(time, rates, box_id)

        print("  [ 2/16] Derivative consistency + transport...")
        box_results['derivative_consistency'] = check_2_derivative_consistency(
            time, rates, state_time, state_concs, state_names, box_id)

        print("  [ 3/16] Cross-variable consistency (extended)...")
        box_results['cross_variable'] = check_3_cross_variable_consistency(time, rates, box_id)

        print("  [ 4/16] Bug fix verification...")
        box_results['bug_fixes'] = check_4_bug_fix_verification(time, rates, box_id)

        print("  [ 5/16] Dominant processes...")
        box_results['dominant_processes'] = check_5_dominant_processes(time, rates, box_id)

        print("  [ 6/16] Seasonal patterns...")
        box_results['seasonal_patterns'] = check_6_seasonal_patterns(time, rates, box_id)

        print("  [ 7/16] Zero slot analysis...")
        box_results['zero_slots'] = check_7_zero_slot_analysis(rates, box_id)

        print("  [ 8/16] Limitation factors...")
        box_results['limitation_factors'] = check_8_limitation_factors(rates, box_id)

        # New checks (9-16)
        print("  [ 9/16] Negative concentrations...")
        box_results['negatives'] = check_9_negative_concentrations(
            state_time, state_concs, state_names, box_id)

        print("  [10/16] NaN/Inf detection...")
        box_results['nan_inf'] = check_10_nan_inf(
            state_concs, state_names, rates, box_id)

        print("  [11/16] Stoichiometric ratios (median-based)...")
        box_results['stoichiometry'] = check_11_stoichiometry(
            state_concs, state_names, rates, box_id)

        print("  [12/16] Process rate sign violations...")
        box_results['sign_violations'] = check_12_sign_violations(rates, box_id)

        print("  [13/16] Euler integration + transport...")
        box_results['euler_integration'] = check_13_euler_integration(
            state_time, state_concs, state_names, rates, box_id)

        print("  [14/16] Allelopathy state variables...")
        box_results['allelopathy'] = check_14_allelopathy(
            state_concs, state_names, rates, box_id)

        print("  [15/16] State-variable smoothness...")
        box_results['smoothness'] = check_15_smoothness(
            state_time, state_concs, state_names, box_id)

        print("  [16/16] Mass-balance closure...")
        box_results['mass_balance'] = check_16_mass_balance(rates, box_id)

        all_results[box_id] = box_results

    # Cross-box spatial consistency
    if len(all_sv_data) > 1:
        print(f"\n{'=' * 74}")
        print("  Cross-box spatial consistency")
        print(f"{'=' * 74}")
        all_results['cross_box'] = check_cross_box_spatial(all_sv_data)

    return all_results


# ═════════════════════════════════════════════════════════════════════════════
# Report printer
# ═════════════════════════════════════════════════════════════════════════════

def print_summary(all_results):
    """Print a human-readable summary of all 16 checks."""
    box_ids = [k for k in all_results if k != 'cross_box']
    first_box = box_ids[0] if box_ids else None

    print("\n" + "=" * 78)
    print("  DEEP PROCESS RATE ANALYSIS — COMPREHENSIVE REPORT (16 checks)")
    print("=" * 78)

    # ── CHECK 4: Bug fix verification ───────────────────────────────────
    print("\n--- BUG FIX VERIFICATION (Check 4) ---")
    for box_id in box_ids:
        bf = all_results[box_id].get('bug_fixes', {})
        fc = bf.get('fix_cyn_o2_production', {})
        nf = bf.get('n_fixation', {})
        print(f"\n  Box {box_id} ({BOX_TYPES.get(box_id, '?')}):")
        print(f"    FIX_CYN O2 slot 19: max={fc.get('slot_19_max', 0):.6f}, "
              f"mean={fc.get('slot_19_mean', 0):.6f}, "
              f"non-zero={fc.get('slot_19_pct_nonzero', 0):.1f}% => {fc.get('status', '?')}")
        print(f"    FIX_CYN N fixation: max={nf.get('fix_cyn_n_fix_max', 0):.6f}")
        print(f"    NOST N fixation:    max={nf.get('nost_n_fix_max', 0):.6f}")

    # ── CHECK 9: Negative concentrations ────────────────────────────────
    print("\n--- NEGATIVE / IMPOSSIBLE CONCENTRATIONS (Check 9) ---")
    any_neg = False
    for box_id in box_ids:
        for f in all_results[box_id].get('negatives', []):
            any_neg = True
            sev = f['severity']
            if 'n_negative' in f:
                print(f"  [{sev}] Box {box_id}: {f['variable']} has {f['n_negative']} negatives "
                      f"({f['pct_negative']:.1f}%), min={f['min_value']:.6e}")
            elif 'max_value' in f:
                print(f"  [{sev}] Box {box_id}: {f['variable']} max={f['max_value']:.4f} {f.get('note', '')}")
    if not any_neg:
        print("  All concentrations within valid ranges.")

    # ── CHECK 10: NaN/Inf ───────────────────────────────────────────────
    print("\n--- NaN / Inf VALUES (Check 10) ---")
    any_bad = False
    for box_id in box_ids:
        for f in all_results[box_id].get('nan_inf', []):
            if f.get('severity') == SEV_ERROR:
                any_bad = True
                print(f"  [ERROR] Box {box_id}: {f.get('variable', 'bulk')}: NaN={f['n_nan']}, Inf={f['n_inf']}")
    if not any_bad:
        print("  No NaN or Inf values found.")

    # ── CHECK 11: Stoichiometry ─────────────────────────────────────────
    if first_box:
        print("\n--- STOICHIOMETRIC RATIO VALIDATION (Check 11) ---")
        for f in all_results[first_box].get('stoichiometry', []):
            sev = f['severity']
            check = f['check']
            if 'match' in f:
                match_str = 'MATCH' if f['match'] else 'MISMATCH'
                val = f.get('median', f.get('mean', 0))
                print(f"  [{sev}] {check}: {val:.6f} expected={f.get('expected', '?')} [{match_str}]")
            elif 'mean' in f:
                print(f"  [{sev}] {check}: mean={f['mean']:.6f} {f.get('note', '')}")
            elif 'cv_pct' in f:
                print(f"  [{sev}] {check}: CV={f['cv_pct']:.1f}%")

    # ── CHECK 12: Sign violations ───────────────────────────────────────
    print("\n--- PROCESS RATE SIGN VIOLATIONS (Check 12) ---")
    any_viol = False
    for box_id in box_ids:
        for f in all_results[box_id].get('sign_violations', []):
            any_viol = True
            print(f"  [{f['severity']}] Box {box_id}: {f['desc']} ({f['n_negative']} neg, "
                  f"min={f['min_value']:.6e})")
    if not any_viol:
        print("  No sign violations found.")

    # ── CHECK 3: Cross-variable consistency ─────────────────────────────
    if first_box:
        print("\n--- CROSS-VARIABLE CONSISTENCY (Check 3, extended) ---")
        for iss in all_results[first_box].get('cross_variable', []):
            sev = iss['severity']
            active = iss.get('active', True)
            marker = f"[{'INACT' if not active else sev:5s}]"
            check = iss['check']
            if 'max_difference' in iss:
                print(f"  {marker} {check}: diff={iss['max_difference']:.2e}")
            elif 'correlation' in iss:
                print(f"  {marker} {check}: corr={iss['correlation']:.4f}")
            elif 'mean_ratio' in iss:
                print(f"  {marker} {check}: ratio={iss['mean_ratio']:.4f}")
            else:
                vals = [f"{k}={v:.6e}" if isinstance(v, float) else f"{k}={v}"
                        for k, v in iss.items() if k not in ('check', 'severity', 'active', 'note')]
                print(f"  {marker} {check}: {', '.join(vals[:3])}")

    # ── CHECK 2: Derivative consistency ─────────────────────────────────
    if first_box:
        print(f"\n--- DERIVATIVE CONSISTENCY (Check 2, Box {first_box}) ---")
        dc = all_results[first_box].get('derivative_consistency', {})
        print(f"  {'Variable':22s} {'Corr':>8s} {'RMSE':>12s} {'%Kin':>8s} {'%Trans':>8s} {'Driver':>10s}")
        for var_name, info in dc.items():
            if info.get('status') == 'SKIP':
                continue
            print(f"  {var_name:22s} {info['correlation']:8.3f} {info['rmse']:12.6f} "
                  f"{info['pct_kinetic_of_total']:8.1f} {info['pct_transport']:8.1f} "
                  f"{info['driver']:>10s}")

    # ── CHECK 5: Dominant processes (first box) ─────────────────────────
    if first_box:
        print(f"\n--- DOMINANT PROCESSES (Check 5, Box {first_box}) ---")
        dom = all_results[first_box].get('dominant_processes', {})
        for var_name, d in dom.items():
            procs = d['dominant_processes']
            if procs:
                top = procs[0]
                print(f"  {var_name:22s}: {top['desc'][:40]:40s} ({top['pct_of_total']:.1f}%)")

    # ── CHECK 6: Seasonal patterns (first box) ─────────────────────────
    if first_box:
        print(f"\n--- SEASONAL PATTERNS (Check 6, Box {first_box}) ---")
        seas = all_results[first_box].get('seasonal_patterns', {})
        for proc_name, seasons in seas.items():
            vals = [f"{s}: {v:.4f}" for s, v in seasons.items()]
            print(f"  {proc_name:20s}: {', '.join(vals)}")

    # ── CHECK 8: Limitation factors (first box) ────────────────────────
    if first_box:
        print(f"\n--- PHYTOPLANKTON LIMITATION FACTORS (Check 8, Box {first_box}, means) ---")
        lim = all_results[first_box].get('limitation_factors', {})
        for group, factors in lim.items():
            vals = [f"{k}={v['mean']:.3f}" for k, v in factors.items()]
            print(f"  {group:20s}: {', '.join(vals)}")

    # ── CHECK 13: Euler integration (first box) ────────────────────────
    if first_box:
        print(f"\n--- EULER INTEGRATION + TRANSPORT (Check 13, Box {first_box}) ---")
        print(f"  {'Variable':22s} {'%Kinetic':>10s} {'%Transport':>10s} {'Corr':>8s} {'TransportSigned':>16s}")
        for f in all_results[first_box].get('euler_integration', []):
            print(f"  {f['variable']:22s} {f['pct_kinetic']:10.1f} {f['pct_transport']:10.1f} "
                  f"{f['correlation_kin_actual']:8.3f} {f['transport_mean_signed']:16.6e}")

    # ── CHECK 14: Allelopathy (first box) ──────────────────────────────
    if first_box:
        print(f"\n--- ALLELOPATHY STATE VARIABLES (Check 14, Box {first_box}) ---")
        for f in all_results[first_box].get('allelopathy', []):
            var = f['variable']
            if 'correlation_with_parent' in f:
                print(f"  [{f['severity']}] {var}: mean={f['mean']:.6e}, corr_parent={f['correlation_with_parent']:.3f}, "
                      f"neg={f['n_negative']}, zero={f['always_zero']}")
            else:
                print(f"  [{f['severity']}] {var}: total_rate_mag={f.get('total_rate_magnitude', 0):.6e}")

    # ── CHECK 15: Smoothness (across boxes) ────────────────────────────
    print("\n--- STATE-VARIABLE SMOOTHNESS (Check 15) ---")
    any_smooth = False
    for box_id in box_ids:
        for f in all_results[box_id].get('smoothness', []):
            any_smooth = True
            var = f['variable']
            typ = f['type']
            if typ == 'spike':
                print(f"  [{f['severity']}] Box {box_id} {var}: {f['n_spikes']} spikes "
                      f"(mean_dC={f['mean_abs_daily_change']:.4e})")
            elif typ == 'monotonic_trend':
                print(f"  [{f['severity']}] Box {box_id} {var}: {f['pct_same_direction']:.0f}% {f['direction']}")
    if not any_smooth:
        print("  No significant spikes or monotonic trends detected.")

    # ── CHECK 16: Mass balance (first box) ─────────────────────────────
    if first_box:
        print(f"\n--- MASS-BALANCE CLOSURE (Check 16, Box {first_box}) ---")
        for f in all_results[first_box].get('mass_balance', []):
            extra = ""
            if 'ratio_total_to_individual' in f:
                extra = f" ratio={f['ratio_total_to_individual']:.4f}"
            print(f"  [{f['severity']}] {f['check']}: mean={f.get('mean_signed', 0):.6e}{extra}")
            if f.get('note'):
                print(f"         {f['note']}")

    # ── Cross-box (if available) ────────────────────────────────────────
    if 'cross_box' in all_results:
        print("\n--- CROSS-BOX SPATIAL CONSISTENCY ---")
        for f in all_results['cross_box']:
            var = f['variable']
            means = f['box_means']
            means_str = ", ".join(f"B{b}={v:.3f}" for b, v in sorted(means.items()))
            print(f"  [{f['severity']}] {var:15s}: CV={f['cv_across_boxes_pct']:.1f}%, "
                  f"max/min={f['max_min_ratio']:.2f}  ({means_str})")

    # ── Zero slots ──────────────────────────────────────────────────────
    if first_box:
        print("\n--- UNEXPECTED ZERO SLOTS (Check 7) ---")
        unexpected = [f for f in all_results[first_box].get('zero_slots', [])
                      if not f['expected_zero'] and f['in_derivative']]
        if unexpected:
            for f in unexpected:
                print(f"  {f['variable']} slot {f['slot']} = {f['desc'][:60]}")
        else:
            print("  None found — all derivative slots have non-zero values (or expected-zero)")

    # ── Grand totals ────────────────────────────────────────────────────
    n_errors = 0
    n_warnings = 0
    for box_id, box in all_results.items():
        if box_id == 'cross_box':
            for f in box:
                if isinstance(f, dict):
                    if f.get('severity') == SEV_ERROR:
                        n_errors += 1
                    elif f.get('severity') == SEV_WARNING:
                        n_warnings += 1
            continue
        for check_name, findings in box.items():
            if isinstance(findings, list):
                for f in findings:
                    if isinstance(f, dict):
                        if f.get('severity') == SEV_ERROR:
                            n_errors += 1
                        elif f.get('severity') == SEV_WARNING:
                            n_warnings += 1
            elif isinstance(findings, dict):
                for key, val in findings.items():
                    if isinstance(val, dict) and val.get('severity') == SEV_ERROR:
                        n_errors += 1
                    if isinstance(val, dict) and val.get('severity') == SEV_WARNING:
                        n_warnings += 1
                    if isinstance(val, dict) and val.get('status') == SEV_ERROR:
                        n_errors += 1

    print(f"\n{'=' * 78}")
    print(f"  TOTALS: {n_errors} ERRORs, {n_warnings} WARNINGs across {len(box_ids)} boxes, 16 checks")
    print(f"{'=' * 78}")

    return all_results


def main():
    parser = argparse.ArgumentParser(description='Deep Process Rate Analysis for AQUABC (comprehensive)')
    parser.add_argument('--output-dir', default='OUTPUTS', help='Model output directory')
    args = parser.parse_args()

    print("AQUABC Deep Process Rate Analysis v2 (16 checks)")
    print(f"Output directory: {args.output_dir}")

    results = run_analysis(args.output_dir)
    print_summary(results)

    return results


if __name__ == '__main__':
    main()
