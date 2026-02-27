#!/usr/bin/env python3
"""
Deep Process Rate Analysis for AQUABC Pelagic Model
====================================================
Reads process rate output files (PELAGIC_BOX_XXXXX_PROCESS_RATES.out)
and state variable files (PELAGIC_BOX_XXXXX.out), performs comprehensive
cross-checks and generates a structured analysis report.

Usage:
    python tools/deep_process_rate_analysis.py [--output-dir OUTPUTS]
"""

import sys
import os
import argparse
import numpy as np
from pathlib import Path

# Add tools dir to path for slot map import
sys.path.insert(0, str(Path(__file__).parent))
from process_rate_slot_map import SLOT_MAP, DERIVATIVE_SIGNS

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────
NDIAGVAR = 30
NSTATE = 32
NUM_ALLELOPATHY = 4
NSTATE_TOTAL = NSTATE + NUM_ALLELOPATHY  # 36
NUM_PROCESS_RATES = NSTATE_TOTAL * NDIAGVAR  # 1080

BOX_IDS = [5, 6, 8, 9, 14, 17, 25]
BOX_TYPES = {5: 'mud', 6: 'sand', 8: 'mud', 9: 'sand',
             14: 'mud', 17: 'mud', 25: 'mud'}

STATE_VAR_NAMES = [
    'NH4_N', 'NO3_N', 'PO4_P', 'DISS_OXYGEN', 'DIA_C', 'ZOO_C',
    'ZOO_N', 'ZOO_P', 'DET_PART_ORG_C', 'DET_PART_ORG_N', 'DET_PART_ORG_P',
    'DISS_ORG_C', 'DISS_ORG_N', 'DISS_ORG_P', 'CYN_C', 'OPA_C',
    'DISS_Si', 'PART_Si', 'FIX_CYN_C', 'INORG_C', 'TOT_ALK',
    'FE_II', 'FE_III', 'MN_II', 'MN_IV', 'CA', 'MG',
    'S_PLUS_6', 'S_MINUS_2', 'CH4_C', 'NOST_VEG_HET_C', 'AKI_C',
    'SEC_METAB_DIA', 'SEC_METAB_NOFIX_CYN', 'SEC_METAB_FIX_CYN', 'SEC_METAB_NOST'
]

# Key variables for deeper analysis
KEY_VARS = [
    'NH4_N', 'NO3_N', 'PO4_P', 'DISS_OXYGEN',
    'DIA_C', 'CYN_C', 'OPA_C', 'FIX_CYN_C', 'NOST_VEG_HET_C',
    'ZOO_C', 'ZOO_N', 'ZOO_P',
    'DET_PART_ORG_C', 'DET_PART_ORG_N', 'DET_PART_ORG_P',
    'DISS_ORG_C', 'DISS_ORG_N', 'DISS_ORG_P',
    'DISS_Si', 'PART_Si',
    'INORG_C', 'TOT_ALK',
    'AKI_C',
]


def get_slot_col(var_idx_1based, slot_1based):
    """Get 0-based column index in process rate data array (excluding TIME col)."""
    return (var_idx_1based - 1) * NDIAGVAR + (slot_1based - 1)


def load_process_rates(output_dir, box_id):
    """Load process rate file for a given box. Returns (time, rates_2d)."""
    fname = os.path.join(output_dir, f'PELAGIC_BOX_{box_id:05d}_PROCESS_RATES.out')
    if not os.path.exists(fname):
        return None, None
    data = np.loadtxt(fname)
    time = data[:, 0]
    rates = data[:, 1:]  # shape (ntime, 1080)
    return time, rates


def load_state_vars(output_dir, box_id):
    """Load state variable file for a given box. Returns (time, concs)."""
    fname = os.path.join(output_dir, f'PELAGIC_BOX_{box_id:05d}.out')
    if not os.path.exists(fname):
        return None, None
    # First line is header
    with open(fname) as f:
        header = f.readline().split()
    data = np.loadtxt(fname, skiprows=1)
    time = data[:, 0]
    concs = data[:, 1:]
    return time, concs, header[1:]


def compute_derivative_from_rates(rates, var_name):
    """Compute the expected derivative as sum of signed process rates for a variable."""
    if var_name not in SLOT_MAP:
        return np.zeros(rates.shape[0])

    info = SLOT_MAP[var_name]
    var_idx = info['var_index']
    signs = DERIVATIVE_SIGNS.get(var_name, {})

    deriv = np.zeros(rates.shape[0])
    for slot, sign in signs.items():
        col = get_slot_col(var_idx, slot)
        if col < rates.shape[1]:
            deriv += sign * rates[:, col]
    return deriv


def compute_numerical_derivative(time, conc):
    """Compute numerical dC/dt from concentration data using centered differences."""
    dt = np.diff(time)
    dc = np.diff(conc)
    # Forward differences
    dCdt = dc / dt
    return dCdt


# ─────────────────────────────────────────────────────────────────────────────
# Analysis functions
# ─────────────────────────────────────────────────────────────────────────────

def check_1_rate_statistics(time, rates, box_id):
    """Check 1: Basic statistics of process rates per variable."""
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


def check_2_derivative_consistency(time, rates, state_time, state_concs, state_names, box_id):
    """Check 2: Compare process-rate-derived dC/dt vs actual concentration changes."""
    results = {}

    for var_name in KEY_VARS:
        if var_name not in SLOT_MAP:
            continue
        info = SLOT_MAP[var_name]
        var_idx = info['var_index']

        # Find the variable in state output
        # The state var names might differ slightly
        var_col = None
        for i, sn in enumerate(state_names):
            if sn == var_name or sn.upper() == var_name.upper():
                var_col = i
                break
            # NOST_VEG_HET_C might be listed differently
            if var_name == 'NOST_VEG_HET_C' and 'NOST' in sn.upper() and 'VEG' in sn.upper():
                var_col = i
                break

        if var_col is None:
            results[var_name] = {'status': 'SKIP', 'reason': 'not found in state output'}
            continue

        # Compute kinetic derivative from process rates
        kinetic_deriv = compute_derivative_from_rates(rates, var_name)

        # Compute numerical derivative from concentrations
        conc = state_concs[:, var_col]
        num_deriv = compute_numerical_derivative(state_time, conc)

        # Align time arrays - process rates and state vars should have same timestamps
        # Use the shorter array
        n = min(len(kinetic_deriv), len(num_deriv))
        if n == 0:
            results[var_name] = {'status': 'SKIP', 'reason': 'no data'}
            continue

        kin = kinetic_deriv[:n]
        num = num_deriv[:n]

        # Note: numerical derivative includes transport, so they won't match exactly.
        # We check the magnitude and correlation.
        corr = np.corrcoef(kin, num)[0, 1] if np.std(kin) > 0 and np.std(num) > 0 else 0.0
        rmse = np.sqrt(np.mean((kin - num)**2))
        mean_abs_kin = np.mean(np.abs(kin))
        mean_abs_num = np.mean(np.abs(num))

        results[var_name] = {
            'status': 'OK' if not np.isnan(corr) else 'WARN',
            'correlation': float(corr) if not np.isnan(corr) else 0.0,
            'rmse': float(rmse),
            'mean_abs_kinetic': float(mean_abs_kin),
            'mean_abs_numerical': float(mean_abs_num),
            'pct_kinetic_of_total': float(mean_abs_kin / mean_abs_num * 100) if mean_abs_num > 1e-20 else 0.0,
        }

    return results


def check_3_cross_variable_consistency(time, rates, box_id):
    """Check 3: Cross-variable process rate consistency."""
    issues = []

    # 3a. DIA growth: C growth rate should match N uptake pattern
    dia_c_growth = rates[:, get_slot_col(5, 1)]  # DIA_C slot 1 = R_DIA_GROWTH
    dia_nh4_uptake = rates[:, get_slot_col(1, 6)]  # NH4_N slot 6 = DIA NH4 uptake

    # These should correlate (both driven by DIA growth)
    if np.std(dia_c_growth) > 0 and np.std(dia_nh4_uptake) > 0:
        corr = np.corrcoef(dia_c_growth, dia_nh4_uptake)[0, 1]
        issues.append({
            'check': 'DIA_C growth ~ NH4 uptake by DIA',
            'correlation': float(corr),
            'status': 'OK' if corr > 0.9 else 'WARN'
        })

    # 3b. ZOO grazing balance: sum of grazing from ZOO_C == sum of grazing losses from phyto+detritus
    zoo_c_growth = rates[:, get_slot_col(6, 1)]  # ZOO_C slot 1 = total feeding gain
    zoo_feed_dia = rates[:, get_slot_col(6, 5)]  # ZOO_C slot 5 = R_ZOO_FEEDING_DIA
    zoo_feed_cyn = rates[:, get_slot_col(6, 6)]  # ZOO_C slot 6 = R_ZOO_FEEDING_CYN
    zoo_feed_opa = rates[:, get_slot_col(6, 7)]  # ZOO_C slot 7 = R_ZOO_FEEDING_OPA
    zoo_feed_fix = rates[:, get_slot_col(6, 8)]  # ZOO_C slot 8 = R_ZOO_FEEDING_FIX_CYN
    zoo_feed_det = rates[:, get_slot_col(6, 9)]  # ZOO_C slot 9 = R_ZOO_FEEDING_DET
    zoo_feed_nost = rates[:, get_slot_col(6, 10)]  # ZOO_C slot 10 = R_ZOO_FEEDING_NOST

    total_feeding = zoo_feed_dia + zoo_feed_cyn + zoo_feed_opa + zoo_feed_fix + zoo_feed_det + zoo_feed_nost

    # Match DIA_C grazing loss
    dia_grazing_loss = rates[:, get_slot_col(5, 5)]  # DIA_C slot 5 = R_ZOO_FEEDING_DIA
    if np.any(np.abs(zoo_feed_dia) > 1e-20):
        max_diff = float(np.max(np.abs(zoo_feed_dia - dia_grazing_loss)))
        issues.append({
            'check': 'ZOO_C grazing on DIA == DIA_C grazing loss',
            'max_difference': max_diff,
            'status': 'OK' if max_diff < 1e-10 else 'WARN'
        })

    # 3c. CYN grazing consistency
    cyn_grazing_loss = rates[:, get_slot_col(15, 5)]  # CYN_C slot 5 = R_ZOO_FEEDING_CYN
    if np.any(np.abs(zoo_feed_cyn) > 1e-20):
        max_diff = float(np.max(np.abs(zoo_feed_cyn - cyn_grazing_loss)))
        issues.append({
            'check': 'ZOO_C grazing on CYN == CYN_C grazing loss',
            'max_difference': max_diff,
            'status': 'OK' if max_diff < 1e-10 else 'WARN'
        })

    # 3d. DIA death: DIA_C death == DET_PART_ORG_C source from DIA death
    dia_death_from_dia = rates[:, get_slot_col(5, 4)]  # DIA_C slot 4 = R_DIA_DEATH
    det_from_dia_death = rates[:, get_slot_col(9, 1)]  # DET_PART_ORG_C slot 1 = R_DIA_DEATH
    if np.any(np.abs(dia_death_from_dia) > 1e-20):
        max_diff = float(np.max(np.abs(dia_death_from_dia - det_from_dia_death)))
        issues.append({
            'check': 'DIA_C death rate == DET_PART_ORG_C DIA death input',
            'max_difference': max_diff,
            'status': 'OK' if max_diff < 1e-10 else 'WARN'
        })

    # 3e. Nitrification: NH4_N nitrification loss == NO3_N nitrification gain
    nh4_nitr = rates[:, get_slot_col(1, 10)]  # NH4_N slot 10 = R_ABIOTIC_NITR
    no3_nitr = rates[:, get_slot_col(2, 1)]  # NO3_N slot 1 = R_ABIOTIC_NITR
    if np.any(np.abs(nh4_nitr) > 1e-20):
        max_diff = float(np.max(np.abs(nh4_nitr - no3_nitr)))
        issues.append({
            'check': 'NH4_N nitrification loss == NO3_N nitrification gain',
            'max_difference': max_diff,
            'status': 'OK' if max_diff < 1e-10 else 'WARN'
        })

    # 3f. DOC dissolution: DET_PART_ORG_C dissolution == DISS_ORG_C gain from dissolution
    det_diss = rates[:, get_slot_col(9, 7)]  # DET_PART_ORG_C slot 7 = dissolution loss
    doc_from_diss = rates[:, get_slot_col(12, 1)]  # DISS_ORG_C slot 1 = dissolution gain
    if np.any(np.abs(det_diss) > 1e-20):
        max_diff = float(np.max(np.abs(det_diss - doc_from_diss)))
        issues.append({
            'check': 'DET_PART_ORG_C dissolution == DISS_ORG_C gain from dissolution',
            'max_difference': max_diff,
            'status': 'OK' if max_diff < 1e-10 else 'WARN'
        })

    # 3g. ZOO_N explicit budget (ZOOP_OPTION_1)
    # N ingested = sum of all N-ingestion slots
    zoo_n_in_dia  = rates[:, get_slot_col(7, 1)]  # ZOO_N slot 1
    zoo_n_in_cyn  = rates[:, get_slot_col(7, 2)]  # ZOO_N slot 2
    zoo_n_in_opa  = rates[:, get_slot_col(7, 3)]  # ZOO_N slot 3
    zoo_n_in_fix  = rates[:, get_slot_col(7, 4)]  # ZOO_N slot 4
    zoo_n_in_det  = rates[:, get_slot_col(7, 5)]  # ZOO_N slot 5
    zoo_n_in_nost = rates[:, get_slot_col(7, 10)]  # ZOO_N slot 10
    zoo_n_out_don = rates[:, get_slot_col(7, 6)]  # ZOO_N slot 6 = excretion
    zoo_n_out_resp = rates[:, get_slot_col(7, 7)]  # ZOO_N slot 7 = resp
    zoo_n_out_mort = rates[:, get_slot_col(7, 8)]  # ZOO_N slot 8 = mortality

    total_n_in = zoo_n_in_dia + zoo_n_in_cyn + zoo_n_in_opa + zoo_n_in_fix + zoo_n_in_det + zoo_n_in_nost
    total_n_out = zoo_n_out_don + zoo_n_out_resp + zoo_n_out_mort
    net_zoo_n = total_n_in - total_n_out

    if np.any(np.abs(total_n_in) > 1e-20):
        mean_in = float(np.mean(total_n_in))
        mean_out = float(np.mean(total_n_out))
        mean_net = float(np.mean(net_zoo_n))
        issues.append({
            'check': 'ZOO_N budget: ingestion vs losses',
            'mean_N_ingestion': mean_in,
            'mean_N_losses': mean_out,
            'mean_net_change': mean_net,
            'status': 'INFO'
        })

    # 3h. DISS_OXYGEN: reaeration should be significant in surface boxes
    o2_reaer = rates[:, get_slot_col(4, 1)]  # DISS_OXYGEN slot 1 = reaeration
    mean_reaer = float(np.mean(o2_reaer))
    max_reaer = float(np.max(o2_reaer))
    issues.append({
        'check': f'O2 reaeration in box {box_id}',
        'mean_reaeration': mean_reaer,
        'max_reaeration': max_reaer,
        'status': 'OK' if abs(mean_reaer) > 0 else 'NOTE'
    })

    return issues


def check_4_bug_fix_verification(time, rates, box_id):
    """Check 4: Verify the FIX_CYN O2 production bug fix (slot 19 of DISS_OXYGEN)."""
    results = {}

    # Bug fix 1: FIX_CYN O2 production should be in DO slot 19
    fix_cyn_o2 = rates[:, get_slot_col(4, 19)]  # DISS_OXYGEN slot 19
    nost_o2_prod = rates[:, get_slot_col(4, 5)]  # DISS_OXYGEN slot 5 (NOST O2 prod)
    fix_cyn_growth = rates[:, get_slot_col(19, 1)]  # FIX_CYN_C slot 1 = growth

    results['fix_cyn_o2_production'] = {
        'slot_19_max': float(np.max(fix_cyn_o2)),
        'slot_19_mean': float(np.mean(fix_cyn_o2)),
        'slot_19_pct_nonzero': float(np.count_nonzero(fix_cyn_o2) / len(fix_cyn_o2) * 100),
        'fix_cyn_growth_max': float(np.max(fix_cyn_growth)),
        'nost_o2_prod_slot5_max': float(np.max(nost_o2_prod)),
        'status': 'FIXED' if np.max(fix_cyn_o2) > 1e-10 else 'CHECK'
    }

    # Bug fix 2: DISS_ORG_N slot 6 should use correct N:C ratio for NOST DON uptake
    don_slot6 = rates[:, get_slot_col(13, 6)]  # DISS_ORG_N slot 6
    results['don_nost_uptake'] = {
        'slot_6_max': float(np.max(np.abs(don_slot6))),
        'slot_6_mean': float(np.mean(don_slot6)),
        'status': 'INFO'
    }

    # Check N fixation rates (FIX_CYN_C slot 8)
    fix_cyn_n_fix = rates[:, get_slot_col(19, 8)]  # FIX_CYN_C slot 8 = N fixation
    nost_n_fix_slot9 = rates[:, get_slot_col(31, 9)]  # NOST slot 9 = N fixation

    results['n_fixation'] = {
        'fix_cyn_n_fix_max': float(np.max(fix_cyn_n_fix)),
        'fix_cyn_n_fix_mean': float(np.mean(fix_cyn_n_fix)),
        'nost_n_fix_max': float(np.max(nost_n_fix_slot9)),
        'nost_n_fix_mean': float(np.mean(nost_n_fix_slot9)),
        'status': 'INFO'
    }

    return results


def check_5_dominant_processes(time, rates, box_id):
    """Check 5: Identify dominant processes per key variable."""
    results = {}

    for var_name in KEY_VARS:
        if var_name not in SLOT_MAP:
            continue
        info = SLOT_MAP[var_name]
        var_idx = info['var_index']
        signs = DERIVATIVE_SIGNS.get(var_name, {})

        if not signs:
            continue

        # Compute mean absolute contribution of each slot to derivative
        slot_contributions = {}
        for slot, sign in signs.items():
            col = get_slot_col(var_idx, slot)
            if col < rates.shape[1]:
                data = rates[:, col]
                mean_abs = float(np.mean(np.abs(data)))
                slot_desc = info['slots'].get(slot, f'slot {slot}')
                # Strip AUX label and condition info for cleaner display
                clean_desc = slot_desc.split('(')[0].strip() if slot_desc else f'slot_{slot}'
                slot_contributions[slot] = {
                    'mean_abs': mean_abs,
                    'sign': sign,
                    'desc': clean_desc,
                    'signed_mean': float(np.mean(data)) * sign,
                }

        # Sort by mean absolute contribution
        sorted_slots = sorted(slot_contributions.items(), key=lambda x: x[1]['mean_abs'], reverse=True)

        total = sum(v['mean_abs'] for v in slot_contributions.values())
        dominant = []
        for slot, sc in sorted_slots[:5]:  # top 5
            pct = sc['mean_abs'] / total * 100 if total > 0 else 0
            dominant.append({
                'slot': slot,
                'desc': sc['desc'],
                'mean_abs_rate': sc['mean_abs'],
                'pct_of_total': pct,
                'net_contribution': sc['signed_mean'],
            })

        results[var_name] = {
            'total_rate_magnitude': total,
            'dominant_processes': dominant,
        }

    return results


def check_6_seasonal_patterns(time, rates, box_id):
    """Check 6: Seasonal patterns of key processes."""
    results = {}

    # Convert time to day-of-year (approximately)
    # Time is in days since base (6209 = start). Assume 365-day year.
    doy = (time - time[0]) % 365.0

    # Define seasons (Northern hemisphere, approximate)
    winter = (doy < 90) | (doy >= 335)
    spring = (doy >= 90) & (doy < 152)
    summer = (doy >= 152) & (doy < 244)
    autumn = (doy >= 244) & (doy < 335)
    season_masks = {'winter': winter, 'spring': spring, 'summer': summer, 'autumn': autumn}

    # Key photosynthetic processes
    phyto_vars = [
        ('DIA_C', 5, 1, 'DIA growth'),
        ('CYN_C', 15, 1, 'CYN growth'),
        ('OPA_C', 16, 1, 'OPA growth'),
        ('FIX_CYN_C', 19, 1, 'FIX_CYN growth'),
        ('NOST_VEG_HET_C', 31, 1, 'NOST growth'),
    ]

    for name, var_idx, slot, desc in phyto_vars:
        col = get_slot_col(var_idx, slot)
        if col >= rates.shape[1]:
            continue
        data = rates[:, col]
        seasonal = {}
        for sname, mask in season_masks.items():
            if np.any(mask):
                seasonal[sname] = float(np.mean(data[mask]))
        results[desc] = seasonal

    # O2 reaeration
    o2_reaer = rates[:, get_slot_col(4, 1)]
    for sname, mask in season_masks.items():
        if 'O2 reaeration' not in results:
            results['O2 reaeration'] = {}
        if np.any(mask):
            results['O2 reaeration'][sname] = float(np.mean(o2_reaer[mask]))

    # ZOO grazing
    zoo_growth = rates[:, get_slot_col(6, 1)]
    for sname, mask in season_masks.items():
        if 'ZOO growth' not in results:
            results['ZOO growth'] = {}
        if np.any(mask):
            results['ZOO growth'][sname] = float(np.mean(zoo_growth[mask]))

    # Nitrification
    nitr = rates[:, get_slot_col(1, 10)]
    for sname, mask in season_masks.items():
        if 'Nitrification' not in results:
            results['Nitrification'] = {}
        if np.any(mask):
            results['Nitrification'][sname] = float(np.mean(nitr[mask]))

    return results


def check_7_zero_slot_analysis(rates, box_id):
    """Check 7: Identify slots that should be non-zero but are perpetually zero."""
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
                # Check if this is expected (ADVANCED_REDOX=0 etc.)
                expected_zero = False
                if 'ADVANCED_REDOX' in desc:
                    expected_zero = True
                if var_name in ('CA', 'MG'):
                    expected_zero = True

                findings.append({
                    'variable': var_name,
                    'slot': slot,
                    'desc': desc,
                    'expected_zero': expected_zero,
                    'in_derivative': slot in signs,
                })

    return findings


def check_8_limitation_factors(rates, box_id):
    """Check 8: Analyze phytoplankton limitation factors."""
    results = {}

    # DIA limitations (slots 6-11 of DIA_C = var 5)
    lim_names = ['temp', 'doxy', 'N', 'P', 'Si', 'light']
    dia_lims = {}
    for i, name in enumerate(lim_names):
        col = get_slot_col(5, 6 + i)
        data = rates[:, col]
        dia_lims[name] = {
            'mean': float(np.mean(data)),
            'min': float(np.min(data)),
            'max': float(np.max(data)),
        }
    results['DIA_limitations'] = dia_lims

    # CYN limitations (slots 6-10 of CYN_C = var 15)
    cyn_lim_names = ['temp', 'doxy', 'N', 'P', 'light']
    cyn_lims = {}
    for i, name in enumerate(cyn_lim_names):
        col = get_slot_col(15, 6 + i)
        data = rates[:, col]
        cyn_lims[name] = {
            'mean': float(np.mean(data)),
            'min': float(np.min(data)),
            'max': float(np.max(data)),
        }
    results['CYN_limitations'] = cyn_lims

    # OPA limitations (slots 6-10 of OPA_C = var 16)
    opa_lim_names = ['temp', 'doxy', 'N', 'P', 'light']
    opa_lims = {}
    for i, name in enumerate(opa_lim_names):
        col = get_slot_col(16, 6 + i)
        data = rates[:, col]
        opa_lims[name] = {
            'mean': float(np.mean(data)),
            'min': float(np.min(data)),
            'max': float(np.max(data)),
        }
    results['OPA_limitations'] = opa_lims

    # NOST limitations (slots 11-16 of NOST_VEG_HET_C = var 31)
    nost_lim_map = {
        'light': 11, 'temp': 12, 'doxy': 13, 'P': 14, 'N': 16
    }
    nost_lims = {}
    for name, slot in nost_lim_map.items():
        col = get_slot_col(31, slot)
        data = rates[:, col]
        nost_lims[name] = {
            'mean': float(np.mean(data)),
            'min': float(np.min(data)),
            'max': float(np.max(data)),
        }
    results['NOST_limitations'] = nost_lims

    return results


# ─────────────────────────────────────────────────────────────────────────────
# Main analysis
# ─────────────────────────────────────────────────────────────────────────────

def run_analysis(output_dir):
    """Run all checks for all boxes and return structured results."""
    all_results = {}

    for box_id in BOX_IDS:
        print(f"\n{'='*70}")
        print(f"  Analysing Box {box_id} ({BOX_TYPES.get(box_id, '?')})")
        print(f"{'='*70}")

        time, rates = load_process_rates(output_dir, box_id)
        if time is None:
            print(f"  [SKIP] No process rate file for box {box_id}")
            continue

        sv_result = load_state_vars(output_dir, box_id)
        if sv_result[0] is None:
            print(f"  [SKIP] No state variable file for box {box_id}")
            continue
        state_time, state_concs, state_names = sv_result

        print(f"  Loaded: {len(time)} timesteps, {rates.shape[1]} process rates")
        print(f"  Time range: {time[0]:.1f} - {time[-1]:.1f} days")

        box_results = {}

        # Check 1: Rate statistics
        print("  [1/8] Rate statistics...")
        box_results['rate_statistics'] = check_1_rate_statistics(time, rates, box_id)

        # Check 2: Derivative consistency
        print("  [2/8] Derivative consistency...")
        box_results['derivative_consistency'] = check_2_derivative_consistency(
            time, rates, state_time, state_concs, state_names, box_id)

        # Check 3: Cross-variable consistency
        print("  [3/8] Cross-variable consistency...")
        box_results['cross_variable'] = check_3_cross_variable_consistency(time, rates, box_id)

        # Check 4: Bug fix verification
        print("  [4/8] Bug fix verification...")
        box_results['bug_fixes'] = check_4_bug_fix_verification(time, rates, box_id)

        # Check 5: Dominant processes
        print("  [5/8] Dominant processes...")
        box_results['dominant_processes'] = check_5_dominant_processes(time, rates, box_id)

        # Check 6: Seasonal patterns
        print("  [6/8] Seasonal patterns...")
        box_results['seasonal_patterns'] = check_6_seasonal_patterns(time, rates, box_id)

        # Check 7: Zero slot analysis
        print("  [7/8] Zero slot analysis...")
        box_results['zero_slots'] = check_7_zero_slot_analysis(rates, box_id)

        # Check 8: Limitation factors
        print("  [8/8] Limitation factors...")
        box_results['limitation_factors'] = check_8_limitation_factors(rates, box_id)

        all_results[box_id] = box_results

    return all_results


def print_summary(all_results):
    """Print a human-readable summary of all findings."""

    # ── Summary of critical findings ────────────────────────────────────────
    print("\n" + "=" * 78)
    print("  DEEP PROCESS RATE ANALYSIS SUMMARY")
    print("=" * 78)

    # Bug fix verification (check across all boxes)
    print("\n--- BUG FIX VERIFICATION ---")
    for box_id, br in all_results.items():
        bf = br.get('bug_fixes', {})
        fix_cyn = bf.get('fix_cyn_o2_production', {})
        nfix = bf.get('n_fixation', {})
        print(f"\n  Box {box_id} ({BOX_TYPES.get(box_id, '?')}):")
        print(f"    FIX_CYN O2 production (slot 19): max={fix_cyn.get('slot_19_max', 0):.6f}, "
              f"mean={fix_cyn.get('slot_19_mean', 0):.6f}, "
              f"non-zero={fix_cyn.get('slot_19_pct_nonzero', 0):.1f}% => {fix_cyn.get('status', '?')}")
        print(f"    FIX_CYN N fixation: max={nfix.get('fix_cyn_n_fix_max', 0):.6f} mgN/L/d")
        print(f"    NOST N fixation:    max={nfix.get('nost_n_fix_max', 0):.6f} mgN/L/d")

    # Cross-variable consistency (first box as representative)
    print("\n--- CROSS-VARIABLE CONSISTENCY ---")
    first_box = list(all_results.keys())[0]
    for iss in all_results[first_box].get('cross_variable', []):
        status_marker = {'OK': 'PASS', 'WARN': 'WARN', 'INFO': 'INFO', 'NOTE': 'NOTE'}.get(iss['status'], '?')
        print(f"  [{status_marker}] {iss['check']}")
        for k, v in iss.items():
            if k not in ('check', 'status'):
                if isinstance(v, float):
                    print(f"         {k}: {v:.8e}")
                else:
                    print(f"         {k}: {v}")

    # Zero slot analysis
    print("\n--- UNEXPECTED ZERO SLOTS ---")
    unexpected_zeros = []
    for box_id, br in all_results.items():
        for f in br.get('zero_slots', []):
            if not f['expected_zero'] and f['in_derivative']:
                unexpected_zeros.append((box_id, f))

    if unexpected_zeros:
        for box_id, f in unexpected_zeros:
            print(f"  Box {box_id}: {f['variable']} slot {f['slot']} = {f['desc'][:60]}")
    else:
        print("  None found - all derivative slots have non-zero values (or are expected-zero)")

    # Dominant processes (summary across key variables for first box)
    print("\n--- DOMINANT PROCESSES (Box 5 representative) ---")
    box5_dom = all_results.get(5, {}).get('dominant_processes', {})
    for var_name, dom_info in box5_dom.items():
        procs = dom_info['dominant_processes']
        if procs:
            top = procs[0]
            print(f"  {var_name:22s}: {top['desc'][:40]:40s} ({top['pct_of_total']:.1f}% of total)")

    # Seasonal patterns (Box 5)
    print("\n--- SEASONAL PATTERNS (Box 5) ---")
    box5_seas = all_results.get(5, {}).get('seasonal_patterns', {})
    for proc_name, seasons in box5_seas.items():
        vals = [f"{s}: {v:.4f}" for s, v in seasons.items()]
        print(f"  {proc_name:20s}: {', '.join(vals)}")

    # Limitation factors (Box 5)
    print("\n--- PHYTOPLANKTON LIMITATION FACTORS (Box 5, means) ---")
    box5_lim = all_results.get(5, {}).get('limitation_factors', {})
    for group_name, factors in box5_lim.items():
        vals = [f"{k}={v['mean']:.3f}" for k, v in factors.items()]
        print(f"  {group_name:20s}: {', '.join(vals)}")

    # Derivative consistency (Box 5)
    print("\n--- DERIVATIVE CONSISTENCY (Box 5, kinetic vs total dC/dt) ---")
    box5_dc = all_results.get(5, {}).get('derivative_consistency', {})
    print(f"  {'Variable':22s} {'Corr':>8s} {'RMSE':>12s} {'|Kin|':>12s} {'|Num|':>12s} {'Kin/Num%':>10s}")
    for var_name, info in box5_dc.items():
        if info.get('status') == 'SKIP':
            continue
        print(f"  {var_name:22s} {info['correlation']:8.3f} {info['rmse']:12.6f} "
              f"{info['mean_abs_kinetic']:12.6f} {info['mean_abs_numerical']:12.6f} "
              f"{info['pct_kinetic_of_total']:10.1f}")

    # Count issues across all boxes
    total_warns = 0
    for box_id, br in all_results.items():
        for iss in br.get('cross_variable', []):
            if iss['status'] == 'WARN':
                total_warns += 1

    print(f"\n{'='*78}")
    print(f"  TOTAL: {len(all_results)} boxes analysed, {total_warns} WARNings, "
          f"{len(unexpected_zeros)} unexpected zero slots")
    print(f"{'='*78}")

    return all_results


def main():
    parser = argparse.ArgumentParser(description='Deep Process Rate Analysis for AQUABC')
    parser.add_argument('--output-dir', default='OUTPUTS', help='Model output directory')
    args = parser.parse_args()

    print("AQUABC Deep Process Rate Analysis")
    print(f"Output directory: {args.output_dir}")

    results = run_analysis(args.output_dir)
    print_summary(results)

    return results


if __name__ == '__main__':
    main()
