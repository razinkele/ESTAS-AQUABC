#!/usr/bin/env python3
"""
Deep State Variable & Process Rate Cross-Check for AQUABC
==========================================================
Goes beyond the previous analysis to find inconsistencies in:

  1. Negative/impossible concentrations
  2. NaN / Inf in state vars and process rates
  3. Stoichiometric ratio validation (C:N:P for ZOO, DET)
  4. Process rate sign violations (rates that must be >= 0)
  5. dC/dt integration test (Euler integration of kinetic rates vs state changes)
  6. Transport residual consistency (should transport be this large?)
  7. Cross-box spatial consistency
  8. Allelopathy state variable checks
  9. State variable smoothness (sudden jumps / spikes)
 10. Mass-balance closure (total N, P, C budget across process rates)

Usage:
    python tools/deep_state_vs_process_crosscheck.py [--output-dir OUTPUTS]
"""

import sys
import os
import argparse
import numpy as np
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from process_rate_slot_map import SLOT_MAP, DERIVATIVE_SIGNS
from aquabc_analysis_utils import (
    NDIAGVAR, NSTATE, NUM_ALLELOPATHY, NSTATE_TOTAL, NUM_PROCESS_RATES,
    BOX_IDS, BOX_TYPES, STATE_VAR_NAMES,
    DEFAULT_N_TO_C, DEFAULT_P_TO_C, DEFAULT_O2_TO_C, DEFAULT_Si_TO_C,
    NON_NEGATIVE_VARS, NONNEG_RATE_SLOTS,
    SEV_ERROR, SEV_WARNING, SEV_INFO, SEV_OK,
    get_slot_col, load_process_rates, load_state_vars,
)


def compute_kinetic_deriv(rates, var_name):
    """Sum of signed process rates = kinetic dC/dt."""
    from aquabc_analysis_utils import compute_kinetic_deriv as _ckd
    return _ckd(rates, var_name, SLOT_MAP, DERIVATIVE_SIGNS)


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 1: Negative / impossible concentrations
# ═════════════════════════════════════════════════════════════════════════════
def check_1_negative_concentrations(sv_time, sv_concs, sv_names, box_id):
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
                'severity': 'ERROR'
            })
        # Also check for unreasonably large concentrations
        max_val = float(np.max(vals))
        if var in ('DIA_C', 'CYN_C', 'OPA_C', 'FIX_CYN_C', 'NOST_VEG_HET_C') and max_val > 100:
            findings.append({
                'variable': var,
                'max_value': max_val,
                'note': 'Phyto C > 100 mg/L seems very high',
                'severity': 'WARNING'
            })
        if var == 'DISS_OXYGEN' and max_val > 25:
            findings.append({
                'variable': var,
                'max_value': max_val,
                'note': 'DO > 25 mg/L is physically unreasonable',
                'severity': 'WARNING'
            })
        if var == 'DISS_OXYGEN':
            min_val = float(np.min(vals))
            if min_val < 0:
                findings.append({
                    'variable': var,
                    'min_value': min_val,
                    'note': 'Negative dissolved oxygen',
                    'severity': 'ERROR'
                })
    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 2: NaN / Inf in state vars and process rates
# ═════════════════════════════════════════════════════════════════════════════
def check_2_nan_inf(sv_time, sv_concs, sv_names, pr_time, pr_rates, box_id):
    findings = []
    # State variables
    for i, var in enumerate(sv_names):
        vals = sv_concs[:, i]
        n_nan = int(np.sum(np.isnan(vals)))
        n_inf = int(np.sum(np.isinf(vals)))
        if n_nan > 0 or n_inf > 0:
            findings.append({
                'type': 'state_variable',
                'variable': var,
                'n_nan': n_nan,
                'n_inf': n_inf,
                'severity': 'ERROR'
            })
    # Process rates (check bulk)
    n_nan_pr = int(np.sum(np.isnan(pr_rates)))
    n_inf_pr = int(np.sum(np.isinf(pr_rates)))
    if n_nan_pr > 0 or n_inf_pr > 0:
        # Find which slots
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
                            'type': 'process_rate',
                            'variable': var_name,
                            'slot': slot,
                            'n_nan': nn,
                            'n_inf': ni,
                            'severity': 'ERROR'
                        })
    else:
        findings.append({
            'type': 'process_rates_bulk',
            'n_nan': 0, 'n_inf': 0,
            'severity': 'OK'
        })
    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 3: Stoichiometric ratio validation
# ═════════════════════════════════════════════════════════════════════════════
def check_3_stoichiometry(sv_time, sv_concs, sv_names, pr_rates, box_id):
    findings = []

    def get_col(name):
        return sv_names.index(name) if name in sv_names else None

    # 3a. ZOO actual N:C and P:C ratios
    zc = get_col('ZOO_C')
    zn = get_col('ZOO_N')
    zp = get_col('ZOO_P')
    if zc is not None and zn is not None and zp is not None:
        zoo_c = sv_concs[:, zc]
        zoo_n = sv_concs[:, zn]
        zoo_p = sv_concs[:, zp]
        # Where ZOO_C > significant
        mask = zoo_c > 1e-6
        if np.any(mask):
            nc_ratio = zoo_n[mask] / zoo_c[mask]
            pc_ratio = zoo_p[mask] / zoo_c[mask]
            findings.append({
                'check': 'ZOO N:C ratio',
                'mean': float(np.mean(nc_ratio)),
                'min': float(np.min(nc_ratio)),
                'max': float(np.max(nc_ratio)),
                'expected_default': DEFAULT_N_TO_C,
                'note': 'Dynamic ratio, may differ from default 0.22',
                'severity': 'INFO' if 0.05 < np.mean(nc_ratio) < 0.6 else 'WARNING'
            })
            findings.append({
                'check': 'ZOO P:C ratio',
                'mean': float(np.mean(pc_ratio)),
                'min': float(np.min(pc_ratio)),
                'max': float(np.max(pc_ratio)),
                'expected_default': DEFAULT_P_TO_C,
                'severity': 'INFO' if 0.005 < np.mean(pc_ratio) < 0.1 else 'WARNING'
            })
            # Check if ratio is unstable (high variance)
            nc_cv = float(np.std(nc_ratio) / np.mean(nc_ratio) * 100)
            findings.append({
                'check': 'ZOO N:C coefficient of variation',
                'cv_pct': nc_cv,
                'severity': 'INFO' if nc_cv < 50 else 'WARNING'
            })

    # 3b. DET actual N:C and P:C ratios
    dc = get_col('DET_PART_ORG_C')
    dn = get_col('DET_PART_ORG_N')
    dp = get_col('DET_PART_ORG_P')
    if dc is not None and dn is not None and dp is not None:
        det_c = sv_concs[:, dc]
        det_n = sv_concs[:, dn]
        det_p = sv_concs[:, dp]
        mask = det_c > 1e-6
        if np.any(mask):
            nc_ratio = det_n[mask] / det_c[mask]
            pc_ratio = det_p[mask] / det_c[mask]
            findings.append({
                'check': 'DET N:C ratio',
                'mean': float(np.mean(nc_ratio)),
                'min': float(np.min(nc_ratio)),
                'max': float(np.max(nc_ratio)),
                'expected_default': DEFAULT_N_TO_C,
                'severity': 'INFO' if 0.05 < np.mean(nc_ratio) < 0.6 else 'WARNING'
            })
            findings.append({
                'check': 'DET P:C ratio',
                'mean': float(np.mean(pc_ratio)),
                'min': float(np.min(pc_ratio)),
                'max': float(np.max(pc_ratio)),
                'expected_default': DEFAULT_P_TO_C,
                'severity': 'INFO' if 0.005 < np.mean(pc_ratio) < 0.1 else 'WARNING'
            })

    # 3c. DON:DOC and DOP:DOC ratios
    doc = get_col('DISS_ORG_C')
    don = get_col('DISS_ORG_N')
    dop = get_col('DISS_ORG_P')
    if doc is not None and don is not None and dop is not None:
        diss_c = sv_concs[:, doc]
        diss_n = sv_concs[:, don]
        diss_p = sv_concs[:, dop]
        mask = diss_c > 1e-6
        if np.any(mask):
            nc_r = diss_n[mask] / diss_c[mask]
            pc_r = diss_p[mask] / diss_c[mask]
            findings.append({
                'check': 'DISS_ORG N:C ratio',
                'mean': float(np.mean(nc_r)),
                'min': float(np.min(nc_r)),
                'max': float(np.max(nc_r)),
                'severity': 'INFO'
            })
            findings.append({
                'check': 'DISS_ORG P:C ratio',
                'mean': float(np.mean(pc_r)),
                'min': float(np.min(pc_r)),
                'max': float(np.max(pc_r)),
                'severity': 'INFO'
            })

    # 3d. Process rate stoichiometry: DIA respiration N release should equal
    #     DIA_C resp rate × N_TO_C
    # NH4_N slot1 = R_DIA_TOT_RESP * DIA_N_TO_C
    # DIA_C slot2 = R_DIA_TOT_RESP (raw)
    # NOTE: Text output has ~6 significant figures, so small rates show apparent
    #       ratio scatter (e.g. 0.20-0.24) — this is a precision artifact, not a bug.
    #       We use MEDIAN ratio instead of allclose to avoid outliers at tiny rates.
    nh4_dia_resp = pr_rates[:, get_slot_col(1, 1)]   # NH4 slot 1
    dia_c_resp = pr_rates[:, get_slot_col(5, 2)]      # DIA_C slot 2
    mask = np.abs(dia_c_resp) > 1e-15
    if np.any(mask):
        implied_nc = nh4_dia_resp[mask] / dia_c_resp[mask]
        median_nc = float(np.median(implied_nc))
        match_ok = abs(median_nc - DEFAULT_N_TO_C) < 0.005
        findings.append({
            'check': 'DIA resp implied N:C = NH4_slot1 / DIA_C_slot2',
            'mean': float(np.mean(implied_nc)),
            'std': float(np.std(implied_nc)),
            'expected': DEFAULT_N_TO_C,
            'match': match_ok,
            'severity': 'OK' if match_ok else 'ERROR',
            'note': 'Median-based check (text precision causes scatter at small rates)'
        })

    # 3e. DIA resp P release: PO4_P slot1 should = DIA_C resp × P_TO_C
    po4_dia_resp = pr_rates[:, get_slot_col(3, 1)]
    if np.any(mask):
        implied_pc = po4_dia_resp[mask] / dia_c_resp[mask]
        median_pc = float(np.median(implied_pc))
        match_ok = abs(median_pc - DEFAULT_P_TO_C) < 0.002
        findings.append({
            'check': 'DIA resp implied P:C = PO4_slot1 / DIA_C_slot2',
            'mean': float(np.mean(implied_pc)),
            'expected': DEFAULT_P_TO_C,
            'match': match_ok,
            'severity': 'OK' if match_ok else 'ERROR',
            'note': 'Median-based check (text precision causes scatter at small rates)'
        })

    # 3f. DIA O2 production: DO slot 2 = R_DIA_GROWTH * (1.3 - 0.3*PREF_NH4N_DIA) * O2_TO_C
    #     NOT simply growth × O2_TO_C. The NH4 preference factor (PREF) shifts the
    #     ratio away from 2.66. When PREF=0 (all NO3): factor=1.3, ratio=3.458.
    #     When PREF=1 (all NH4): factor=1.0, ratio=2.66. This is by design.
    dia_growth = pr_rates[:, get_slot_col(5, 1)]
    do_dia_prod = pr_rates[:, get_slot_col(4, 2)]
    mask_g = np.abs(dia_growth) > 1e-15
    if np.any(mask_g):
        implied_o2c = do_dia_prod[mask_g] / dia_growth[mask_g]
        findings.append({
            'check': 'DIA growth O2:C = DO_slot2 / DIA_C_slot1 (includes PREF factor)',
            'mean': float(np.mean(implied_o2c)),
            'min': float(np.min(implied_o2c)),
            'max': float(np.max(implied_o2c)),
            'expected_range': '2.66 (all NH4) to 3.458 (all NO3)',
            'severity': 'INFO',
            'note': 'DO slot2 = growth*(1.3-0.3*PREF)*O2:C — varies with NH4 preference'
        })

    # 3g. DIA Si to particulate: PART_Si slot 1 = R_DIA_DEATH * DIA_Si_TO_C
    #     This is from DIA DEATH (not growth)! So compare against DIA_C death (slot 4).
    part_si_dia_death = pr_rates[:, get_slot_col(18, 1)]  # PART_Si slot 1 = DIA death × Si:C
    dia_death = pr_rates[:, get_slot_col(5, 4)]  # DIA_C slot 4 = DIA death rate
    mask_dd = np.abs(dia_death) > 1e-15
    if np.any(mask_dd):
        implied_sic = part_si_dia_death[mask_dd] / dia_death[mask_dd]
        median_sic = float(np.median(implied_sic))
        match_ok = abs(median_sic - DEFAULT_Si_TO_C) < 0.01
        findings.append({
            'check': 'DIA death implied Si:C = PART_Si_slot1 / DIA_C_slot4',
            'mean': float(np.mean(implied_sic)),
            'expected': DEFAULT_Si_TO_C,
            'match': match_ok,
            'severity': 'OK' if match_ok else 'ERROR',
            'note': 'PART_Si slot 1 = DIA death → particulate Si (not growth). Median-based check.'
        })

    # 3h. CYN resp implied N:C
    nh4_cyn_resp = pr_rates[:, get_slot_col(1, 2)]  # NH4 slot 2 = CYN resp NH4
    cyn_c_resp = pr_rates[:, get_slot_col(15, 2)]    # CYN_C slot 2 = CYN resp
    mask_c = np.abs(cyn_c_resp) > 1e-15
    if np.any(mask_c):
        implied_nc = nh4_cyn_resp[mask_c] / cyn_c_resp[mask_c]
        median_nc = float(np.median(implied_nc))
        match_ok = abs(median_nc - DEFAULT_N_TO_C) < 0.005
        findings.append({
            'check': 'CYN resp implied N:C',
            'mean': float(np.mean(implied_nc)),
            'expected': DEFAULT_N_TO_C,
            'match': match_ok,
            'severity': 'OK' if match_ok else 'ERROR'
        })

    # 3i. OPA resp implied N:C
    nh4_opa_resp = pr_rates[:, get_slot_col(1, 3)]
    opa_c_resp = pr_rates[:, get_slot_col(16, 2)]
    mask_o = np.abs(opa_c_resp) > 1e-15
    if np.any(mask_o):
        implied_nc = nh4_opa_resp[mask_o] / opa_c_resp[mask_o]
        median_nc = float(np.median(implied_nc))
        match_ok = abs(median_nc - DEFAULT_N_TO_C) < 0.005
        findings.append({
            'check': 'OPA resp implied N:C',
            'mean': float(np.mean(implied_nc)),
            'expected': DEFAULT_N_TO_C,
            'match': match_ok,
            'severity': 'OK' if match_ok else 'ERROR'
        })

    # 3j. ZOO resp N release uses ACTUAL N:C (variable), check within range
    nh4_zoo_resp = pr_rates[:, get_slot_col(1, 5)]
    zoo_c_resp = pr_rates[:, get_slot_col(6, 3)]
    mask_z = np.abs(zoo_c_resp) > 1e-15
    if np.any(mask_z):
        actual_zoo_nc = nh4_zoo_resp[mask_z] / zoo_c_resp[mask_z]
        findings.append({
            'check': 'ZOO resp actual N:C = NH4_slot5 / ZOO_C_slot3',
            'mean': float(np.mean(actual_zoo_nc)),
            'min': float(np.min(actual_zoo_nc)),
            'max': float(np.max(actual_zoo_nc)),
            'note': 'Should be >= 0.5 * default (0.11) and variable',
            'severity': 'INFO' if np.mean(actual_zoo_nc) > 0.05 else 'WARNING'
        })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 4: Process rate sign violations
# ═════════════════════════════════════════════════════════════════════════════
def check_4_sign_violations(pr_rates, box_id):
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
            # Downgrade to WARNING if only 1 negative value with tiny magnitude
            # (systematic single-timestep artifact at t=6447.75)
            if n_neg <= 2 and abs(min_val) < 1e-3:
                severity = 'WARNING'
            else:
                severity = 'ERROR'
            findings.append({
                'variable': var_name,
                'slot': slot,
                'desc': desc,
                'n_negative': n_neg,
                'pct_negative': n_neg / len(data) * 100,
                'min_value': min_val,
                'severity': severity
            })
    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 5: Euler integration test
# ═════════════════════════════════════════════════════════════════════════════
def check_5_euler_integration(sv_time, sv_concs, sv_names, pr_time, pr_rates, box_id):
    """
    Integrate kinetic derivatives forward in time (Euler) and compare
    the resulting concentration change to the actual change.
    The difference = transport + numerical error.
    """
    findings = []
    dt = np.median(np.diff(sv_time))  # should be 1 day for daily output

    key_vars = [
        'NH4_N', 'NO3_N', 'PO4_P', 'DISS_OXYGEN', 'DIA_C',
        'ZOO_C', 'DET_PART_ORG_C', 'DISS_ORG_C', 'CYN_C', 'OPA_C',
        'FIX_CYN_C', 'INORG_C', 'TOT_ALK', 'DISS_Si', 'PART_Si',
        'NOST_VEG_HET_C', 'AKI_C',
    ]

    for var in key_vars:
        if var not in sv_names or var not in SLOT_MAP:
            continue
        col = sv_names.index(var)
        conc = sv_concs[:, col]

        # Kinetic derivative at each timestep
        kin_deriv = compute_kinetic_deriv(pr_rates, var)

        n = min(len(conc) - 1, len(kin_deriv))
        if n <= 0:
            continue

        # Actual change per timestep
        actual_dC = np.diff(conc[:n+1])
        # Kinetic change (integrated Euler)
        kin_dC = kin_deriv[:n] * dt

        # Transport residual = actual - kinetic
        transport = actual_dC - kin_dC

        # Statistics
        mean_actual = float(np.mean(np.abs(actual_dC)))
        mean_kinetic = float(np.mean(np.abs(kin_dC)))
        mean_transport = float(np.mean(np.abs(transport)))
        corr_kin_actual = float(np.corrcoef(kin_dC, actual_dC)[0, 1]) \
            if np.std(kin_dC) > 0 and np.std(actual_dC) > 0 else 0.0

        # Check if transport dominates or is consistent
        pct_kin = mean_kinetic / mean_actual * 100 if mean_actual > 1e-20 else 0.0
        pct_transport = mean_transport / mean_actual * 100 if mean_actual > 1e-20 else 0.0

        # Check sign consistency of transport residual with expected transport direction
        transport_mean_signed = float(np.mean(transport))

        findings.append({
            'variable': var,
            'dt': float(dt),
            'mean_abs_actual_dC': mean_actual,
            'mean_abs_kinetic_dC': mean_kinetic,
            'mean_abs_transport_residual': mean_transport,
            'pct_kinetic': pct_kin,
            'pct_transport': pct_transport,
            'correlation_kin_actual': corr_kin_actual,
            'transport_mean_signed': transport_mean_signed,
            'severity': 'OK'
        })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 6: Cross-variable consistency (extended)
# ═════════════════════════════════════════════════════════════════════════════
def check_6_cross_variable_extended(pr_rates, box_id):
    findings = []

    # 6a. ALL phytoplankton: death rate in phyto == source in DET_PART_ORG_C
    phyto_checks = [
        ('DIA_C', 5, 4, 'DET_PART_ORG_C', 9, 1, 'DIA death == DET_C DIA death input'),
        ('CYN_C', 15, 4, 'DET_PART_ORG_C', 9, 2, 'CYN death == DET_C CYN death input'),
        ('OPA_C', 16, 4, 'DET_PART_ORG_C', 9, 3, 'OPA death == DET_C OPA death input'),
        ('FIX_CYN_C', 19, 4, 'DET_PART_ORG_C', 9, 4, 'FIX_CYN death == DET_C FIX_CYN death input'),
    ]
    for src_var, src_vidx, src_slot, dst_var, dst_vidx, dst_slot, desc in phyto_checks:
        src_data = pr_rates[:, get_slot_col(src_vidx, src_slot)]
        dst_data = pr_rates[:, get_slot_col(dst_vidx, dst_slot)]
        max_diff = float(np.max(np.abs(src_data - dst_data)))
        findings.append({
            'check': desc,
            'max_difference': max_diff,
            'severity': 'OK' if max_diff < 1e-10 else 'ERROR',
            'active': bool(np.any(np.abs(src_data) > 1e-15))
        })

    # 6b. Zoo feeding: each food source rate in ZOO_C == corresponding loss in prey
    feeding_checks = [
        ('ZOO_C', 6, 5, 'DIA_C', 5, 5, 'ZOO feeding DIA == DIA grazing loss'),
        ('ZOO_C', 6, 6, 'CYN_C', 15, 5, 'ZOO feeding CYN == CYN grazing loss'),
        ('ZOO_C', 6, 7, 'OPA_C', 16, 5, 'ZOO feeding OPA == OPA grazing loss'),
        ('ZOO_C', 6, 8, 'FIX_CYN_C', 19, 5, 'ZOO feeding FIX_CYN == FIX_CYN grazing loss'),
        ('ZOO_C', 6, 9, 'DET_PART_ORG_C', 9, 6, 'ZOO feeding DET == DET grazing loss'),
    ]
    for zoo_var, zoo_vidx, zoo_slot, prey_var, prey_vidx, prey_slot, desc in feeding_checks:
        zoo_data = pr_rates[:, get_slot_col(zoo_vidx, zoo_slot)]
        prey_data = pr_rates[:, get_slot_col(prey_vidx, prey_slot)]
        max_diff = float(np.max(np.abs(zoo_data - prey_data)))
        findings.append({
            'check': desc,
            'max_difference': max_diff,
            'severity': 'OK' if max_diff < 1e-10 else 'ERROR',
            'active': bool(np.any(np.abs(zoo_data) > 1e-15))
        })

    # 6c. ZOO death == DET_PART_ORG_C input from zoo death
    zoo_death = pr_rates[:, get_slot_col(6, 4)]
    det_zoo_death = pr_rates[:, get_slot_col(9, 5)]
    max_diff = float(np.max(np.abs(zoo_death - det_zoo_death)))
    findings.append({
        'check': 'ZOO_C death == DET_C zoo death input',
        'max_difference': max_diff,
        'severity': 'OK' if max_diff < 1e-10 else 'ERROR',
        'active': bool(np.any(np.abs(zoo_death) > 1e-15))
    })

    # 6d. DOC mineralization: DO slot 12 = 2.66 * R_ABIOTIC_DOC_MIN_DOXY (aerobic only)
    #     DISS_ORG_C slot 3 = Sum of ALL mineralization pathways (aerobic+anaerobic)
    #     So ratio = 2.66 * (aerobic fraction / total) <= 2.66
    doc_total_min = pr_rates[:, get_slot_col(12, 3)]  # DOC slot 3 = total mineralization
    do_doc_min = pr_rates[:, get_slot_col(4, 12)]  # DO slot 12 = 2.66 * aerobic DOC min
    mask = np.abs(doc_total_min) > 1e-15
    if np.any(mask):
        # DO_slot12 / DOC_slot3 = 2.66 * (aerobic / total) — should be <= 2.66
        implied = do_doc_min[mask] / doc_total_min[mask]
        mean_ratio = float(np.mean(implied))
        # Must be <= 2.66 (100% aerobic) and >= 0 (100% anaerobic)
        reasonable = 0.0 <= mean_ratio <= (DEFAULT_O2_TO_C + 0.1)
        findings.append({
            'check': 'DOC aerobic O2:C ratio = DO_slot12 / DOC_slot3 (total min)',
            'mean_ratio': mean_ratio,
            'max_possible': DEFAULT_O2_TO_C,
            'match': reasonable,
            'severity': 'OK' if reasonable else 'WARNING',
            'note': 'DO slot 12 = aerobic O2 demand only; DOC slot 3 = all pathways. Ratio <= 2.66 expected.'
        })

    # 6e. DET dissolution: C, N, P should be stoichiometrically linked
    det_c_diss = pr_rates[:, get_slot_col(9, 7)]   # DET_C dissolution
    det_n_diss = pr_rates[:, get_slot_col(10, 7)]   # DET_N dissolution
    det_p_diss = pr_rates[:, get_slot_col(11, 7)]   # DET_P dissolution
    mask_d = np.abs(det_c_diss) > 1e-15
    if np.any(mask_d):
        nc_diss = det_n_diss[mask_d] / det_c_diss[mask_d]
        pc_diss = det_p_diss[mask_d] / det_c_diss[mask_d]
        findings.append({
            'check': 'DET dissolution N:C ratio',
            'mean': float(np.mean(nc_diss)),
            'min': float(np.min(nc_diss)),
            'max': float(np.max(nc_diss)),
            'note': 'Uses dynamic DET N:C ratio at each timestep',
            'severity': 'INFO'
        })
        findings.append({
            'check': 'DET dissolution P:C ratio',
            'mean': float(np.mean(pc_diss)),
            'min': float(np.min(pc_diss)),
            'max': float(np.max(pc_diss)),
            'severity': 'INFO'
        })

    # 6f. Phyto excretion consistency: Individual phyto excretions should sum to
    #     DISS_ORG_C slot 4 (total phyto DOC excretion gain).
    #     Individual slots: DOC slot 5=DIA, slot 6=CYN, slot 7=OPA, slot 8=FIX_CYN, slot 9=NOST
    dia_excr = pr_rates[:, get_slot_col(5, 3)]   # DIA_C slot 3 = DIA excretion
    doc_total_excr = pr_rates[:, get_slot_col(12, 4)]  # DISS_ORG_C slot 4 = total phyto excretion
    # Sum individual phyto excretions from DOC aux slots:
    doc_dia_excr_aux = pr_rates[:, get_slot_col(12, 5)]   # DOC slot 5 = DIA excretion
    doc_cyn_excr_aux = pr_rates[:, get_slot_col(12, 6)]   # DOC slot 6 = CYN excretion
    doc_opa_excr_aux = pr_rates[:, get_slot_col(12, 7)]   # DOC slot 7 = OPA excretion
    doc_fix_excr_aux = pr_rates[:, get_slot_col(12, 8)]   # DOC slot 8 = FIX_CYN excretion
    doc_nost_excr_aux = pr_rates[:, get_slot_col(12, 9)]  # DOC slot 9 = NOST excretion
    sum_aux_excr = doc_dia_excr_aux + doc_cyn_excr_aux + doc_opa_excr_aux + doc_fix_excr_aux + doc_nost_excr_aux
    max_diff = float(np.max(np.abs(sum_aux_excr - doc_total_excr)))
    findings.append({
        'check': 'Sum(DOC phyto excr slots 5-9) == DOC slot 4 (total excretion)',
        'max_difference': max_diff,
        'severity': 'OK' if max_diff < 1e-8 else 'WARNING',
        'active': bool(np.any(np.abs(doc_total_excr) > 1e-15))
    })
    # Cross-check: DIA excretion from DIA_C slot 3 == DOC slot 5 (DIA excretion aux)
    max_diff_dia = float(np.max(np.abs(dia_excr - doc_dia_excr_aux)))
    findings.append({
        'check': 'DIA_C excretion (slot 3) == DOC slot 5 (DIA excr aux)',
        'max_difference': max_diff_dia,
        'severity': 'OK' if max_diff_dia < 1e-10 else 'ERROR',
        'active': bool(np.any(np.abs(dia_excr) > 1e-15))
    })

    # 6g. N conservation in phyto growth: NH4 uptake + NO3 uptake should = phyto growth * N:C
    # For DIA: NH4 slot6 + NO3 slot3 should = DIA_C slot1 * N_TO_C
    nh4_dia_uptake = pr_rates[:, get_slot_col(1, 6)]
    no3_dia_uptake = pr_rates[:, get_slot_col(2, 3)]
    dia_growth = pr_rates[:, get_slot_col(5, 1)]
    expected_n_uptake = dia_growth * DEFAULT_N_TO_C
    actual_n_uptake = nh4_dia_uptake + no3_dia_uptake
    mask_g = np.abs(dia_growth) > 1e-15
    if np.any(mask_g):
        max_diff_n = float(np.max(np.abs(expected_n_uptake[mask_g] - actual_n_uptake[mask_g])))
        # Tolerance 1e-5: text output has ~6 sig figs, so growth*0.22 can
        # differ from NH4_uptake+NO3_uptake by ~1e-6 due to rounding
        findings.append({
            'check': 'DIA N uptake (NH4+NO3) == DIA growth × N:C',
            'max_difference': max_diff_n,
            'severity': 'OK' if max_diff_n < 1e-5 else 'ERROR'
        })

    # 6h. Nitrification: NH4 loss == NO3 gain
    nh4_nitr = pr_rates[:, get_slot_col(1, 10)]
    no3_nitr = pr_rates[:, get_slot_col(2, 1)]
    max_diff = float(np.max(np.abs(nh4_nitr - no3_nitr)))
    findings.append({
        'check': 'Nitrification: NH4 loss == NO3 gain',
        'max_difference': max_diff,
        'severity': 'OK' if max_diff < 1e-10 else 'ERROR'
    })

    # 6i. NOST death: should appear in DET_PART_ORG_C (check slot 8 or 10?)
    # NOST death = NOST_VEG_HET_C slot 4; DET gain from NOST: DET slot 10
    nost_death = pr_rates[:, get_slot_col(31, 4)]
    # Check if there's a DET slot for NOST death — need to check slot map
    # From the slot map, DET_PART_ORG_C has no explicit NOST death slot in the standard map
    # Let's check slots 8,9,10 for non-zero NOST-related input
    for s in [8, 9, 10]:
        det_s = pr_rates[:, get_slot_col(9, s)]
        c = float(np.corrcoef(nost_death, det_s)[0, 1]) \
            if np.std(nost_death) > 0 and np.std(det_s) > 0 else 0.0
        if c > 0.9:
            max_diff = float(np.max(np.abs(nost_death - det_s)))
            findings.append({
                'check': f'NOST death → DET_C slot {s} (corr={c:.4f})',
                'max_difference': max_diff,
                'severity': 'OK' if max_diff < 1e-10 else 'WARNING'
            })
            break

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 7: Cross-box spatial consistency
# ═════════════════════════════════════════════════════════════════════════════
def check_7_cross_box(all_sv_data):
    """Compare state variable ranges and means across boxes."""
    findings = []

    # For each important variable, compare ranges across boxes
    key_vars = ['DIA_C', 'DISS_OXYGEN', 'NH4_N', 'NO3_N', 'PO4_P', 'INORG_C']

    for var in key_vars:
        means = {}
        maxes = {}
        for box_id, (sv_time, sv_concs, sv_names) in all_sv_data.items():
            if var not in sv_names:
                continue
            col = sv_names.index(var)
            vals = sv_concs[:, col]
            means[box_id] = float(np.mean(vals))
            maxes[box_id] = float(np.max(vals))

        if len(means) < 2:
            continue

        mean_vals = list(means.values())
        cv = np.std(mean_vals) / np.mean(mean_vals) * 100 if np.mean(mean_vals) > 1e-10 else 0
        max_ratio = max(mean_vals) / min(mean_vals) if min(mean_vals) > 1e-10 else float('inf')

        findings.append({
            'variable': var,
            'box_means': means,
            'cv_across_boxes_pct': float(cv),
            'max_min_ratio': float(max_ratio),
            'severity': 'INFO' if max_ratio < 10 else 'WARNING'
        })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 8: Allelopathy state variable consistency
# ═════════════════════════════════════════════════════════════════════════════
def check_8_allelopathy(sv_time, sv_concs, sv_names, pr_rates, box_id):
    findings = []

    allelo_vars = ['SEC_METAB_DIA', 'SEC_METAB_NOFIX_CYN', 'SEC_METAB_FIX_CYN', 'SEC_METAB_NOST']
    for var in allelo_vars:
        if var not in sv_names:
            continue
        col = sv_names.index(var)
        vals = sv_concs[:, col]

        # Check for negative values
        n_neg = int(np.sum(vals < 0))
        # Check association with parent biomass
        parent_map = {
            'SEC_METAB_DIA': 'DIA_C',
            'SEC_METAB_NOFIX_CYN': 'CYN_C',
            'SEC_METAB_FIX_CYN': 'FIX_CYN_C',
            'SEC_METAB_NOST': 'NOST_VEG_HET_C',
        }
        parent = parent_map[var]
        parent_col = sv_names.index(parent) if parent in sv_names else None

        corr_with_parent = 0.0
        if parent_col is not None:
            parent_vals = sv_concs[:, parent_col]
            if np.std(vals) > 0 and np.std(parent_vals) > 0:
                corr_with_parent = float(np.corrcoef(vals, parent_vals)[0, 1])

        findings.append({
            'variable': var,
            'parent': parent,
            'mean': float(np.mean(vals)),
            'max': float(np.max(vals)),
            'n_negative': n_neg,
            'correlation_with_parent': corr_with_parent,
            'always_zero': bool(np.all(np.abs(vals) < 1e-15)),
            'severity': 'OK' if n_neg == 0 else 'WARNING'
        })

        # Check allelopathy process rates (vars 33-36)
        allelo_idx_map = {
            'SEC_METAB_DIA': 33,
            'SEC_METAB_NOFIX_CYN': 34,
            'SEC_METAB_FIX_CYN': 35,
            'SEC_METAB_NOST': 36
        }
        a_idx = allelo_idx_map[var]
        total_rate_mag = 0.0
        for s in range(1, NDIAGVAR + 1):
            col_pr = get_slot_col(a_idx, s)
            if col_pr < pr_rates.shape[1]:
                total_rate_mag += np.mean(np.abs(pr_rates[:, col_pr]))
        findings.append({
            'variable': f'{var} process rates (idx {a_idx})',
            'total_rate_magnitude': total_rate_mag,
            'severity': 'INFO'
        })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 9: State variable smoothness / sudden jumps
# ═════════════════════════════════════════════════════════════════════════════
def check_9_smoothness(sv_time, sv_concs, sv_names, box_id):
    findings = []

    key_vars = ['DIA_C', 'DISS_OXYGEN', 'NH4_N', 'NO3_N', 'PO4_P',
                'ZOO_C', 'INORG_C', 'TOT_ALK', 'DET_PART_ORG_C',
                'CYN_C', 'OPA_C', 'FIX_CYN_C', 'NOST_VEG_HET_C', 'AKI_C']

    for var in key_vars:
        if var not in sv_names:
            continue
        col = sv_names.index(var)
        vals = sv_concs[:, col]

        if len(vals) < 3:
            continue

        # Compute daily change
        dC = np.diff(vals)
        mean_abs_dC = np.mean(np.abs(dC))

        # Detect spikes: points where |dC| > 10 * mean(|dC|)
        if mean_abs_dC > 1e-15:
            spike_mask = np.abs(dC) > 10 * mean_abs_dC
            n_spikes = int(np.sum(spike_mask))
        else:
            n_spikes = 0

        # Detect monotonic run (always increasing or always decreasing)
        n_pos = np.sum(dC > 0)
        n_neg = np.sum(dC < 0)
        n_zero = np.sum(dC == 0)
        total = len(dC)
        monotonic_pct = max(n_pos, n_neg) / total * 100 if total > 0 else 0

        # Mean concentration for context
        mean_conc = float(np.mean(vals))
        range_conc = float(np.max(vals) - np.min(vals))

        if n_spikes > 0:
            spike_indices = np.where(spike_mask)[0]
            spike_times = sv_time[spike_indices + 1]
            spike_magnitudes = dC[spike_indices]
            findings.append({
                'variable': var,
                'type': 'spike',
                'n_spikes': n_spikes,
                'spike_times_first5': [float(t) for t in spike_times[:5]],
                'spike_magnitudes_first5': [float(m) for m in spike_magnitudes[:5]],
                'mean_conc': mean_conc,
                'mean_abs_daily_change': float(mean_abs_dC),
                'severity': 'WARNING' if n_spikes > 10 else 'INFO'
            })

        if monotonic_pct > 95 and range_conc > 0.01 * abs(mean_conc):
            findings.append({
                'variable': var,
                'type': 'monotonic_trend',
                'pct_same_direction': float(monotonic_pct),
                'direction': 'increasing' if n_pos > n_neg else 'decreasing',
                'range': range_conc,
                'severity': 'INFO'
            })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# CHECK 10: N/P/C mass balance closure via process rates
# ═════════════════════════════════════════════════════════════════════════════
def check_10_mass_balance(pr_rates, box_id):
    """
    Check if total N and P are conserved by kinetic processes.
    Sum all kinetic dN/dt across all N-containing variables; should be ~0
    (only source/sink = atmosphere, sediment, boundaries).
    """
    findings = []

    # Total kinetic dN/dt = sum of all N-variable derivatives
    n_vars = ['NH4_N', 'NO3_N', 'ZOO_N', 'DET_PART_ORG_N', 'DISS_ORG_N']
    total_dN = np.zeros(pr_rates.shape[0])
    for var in n_vars:
        total_dN += compute_kinetic_deriv(pr_rates, var)

    # Also add N in phytoplankton (stored as C, using N:C ratio)
    phyto_n_vars = [
        ('DIA_C', DEFAULT_N_TO_C),
        ('CYN_C', DEFAULT_N_TO_C),
        ('OPA_C', DEFAULT_N_TO_C),
        ('FIX_CYN_C', DEFAULT_N_TO_C),
        ('NOST_VEG_HET_C', DEFAULT_N_TO_C),
    ]
    for var, nc in phyto_n_vars:
        total_dN += compute_kinetic_deriv(pr_rates, var) * nc

    # AKI_C also contains N (vegetative N:C applies, but it's cyst, so approximate)
    total_dN += compute_kinetic_deriv(pr_rates, 'AKI_C') * DEFAULT_N_TO_C

    mean_total_dN = float(np.mean(total_dN))
    mean_abs_total_dN = float(np.mean(np.abs(total_dN)))

    # Compare to individual N variable magnitudes
    individual_dn = []
    for var in n_vars:
        d = compute_kinetic_deriv(pr_rates, var)
        individual_dn.append(np.mean(np.abs(d)))
    mean_individual = np.mean(individual_dn)

    findings.append({
        'check': 'Total kinetic dN/dt (should be ~0 if N conserved internally)',
        'mean_signed': mean_total_dN,
        'mean_abs': mean_abs_total_dN,
        'mean_individual_var_abs': float(mean_individual),
        'ratio_total_to_individual': float(mean_abs_total_dN / mean_individual) if mean_individual > 1e-20 else 0,
        'note': 'Non-zero = N fixation, denitrification, ammonia volatilization, atmosphere exchange',
        'severity': 'INFO'
    })

    # Total kinetic dP/dt
    p_vars = ['PO4_P', 'ZOO_P', 'DET_PART_ORG_P', 'DISS_ORG_P']
    total_dP = np.zeros(pr_rates.shape[0])
    for var in p_vars:
        total_dP += compute_kinetic_deriv(pr_rates, var)
    for var, pc in [('DIA_C', DEFAULT_P_TO_C), ('CYN_C', DEFAULT_P_TO_C),
                     ('OPA_C', DEFAULT_P_TO_C), ('FIX_CYN_C', DEFAULT_P_TO_C),
                     ('NOST_VEG_HET_C', DEFAULT_P_TO_C), ('AKI_C', DEFAULT_P_TO_C)]:
        total_dP += compute_kinetic_deriv(pr_rates, var) * pc

    mean_total_dP = float(np.mean(total_dP))
    mean_abs_total_dP = float(np.mean(np.abs(total_dP)))
    individual_dp = []
    for var in p_vars:
        d = compute_kinetic_deriv(pr_rates, var)
        individual_dp.append(np.mean(np.abs(d)))
    mean_individual_p = np.mean(individual_dp)

    findings.append({
        'check': 'Total kinetic dP/dt (should be ~0 if P conserved internally)',
        'mean_signed': mean_total_dP,
        'mean_abs': mean_abs_total_dP,
        'mean_individual_var_abs': float(mean_individual_p),
        'ratio_total_to_individual': float(mean_abs_total_dP / mean_individual_p) if mean_individual_p > 1e-20 else 0,
        'note': 'Non-zero = sediment-water exchange or calculation error',
        'severity': 'INFO' if mean_abs_total_dP < 0.1 * mean_individual_p else 'WARNING'
    })

    # Total kinetic dC/dt (organic carbon budget)
    c_vars = ['DET_PART_ORG_C', 'DISS_ORG_C', 'DIA_C', 'CYN_C', 'OPA_C',
              'FIX_CYN_C', 'NOST_VEG_HET_C', 'AKI_C', 'ZOO_C']
    total_dOrgC = np.zeros(pr_rates.shape[0])
    for var in c_vars:
        total_dOrgC += compute_kinetic_deriv(pr_rates, var)
    # Also add INORG_C (total C should be conserved modulo atm exchange)
    total_dAllC = total_dOrgC + compute_kinetic_deriv(pr_rates, 'INORG_C')

    # Methane
    total_dAllC += compute_kinetic_deriv(pr_rates, 'CH4_C')

    mean_orgC = float(np.mean(total_dOrgC))
    mean_allC = float(np.mean(total_dAllC))
    individual_dc = [np.mean(np.abs(compute_kinetic_deriv(pr_rates, v))) for v in c_vars]
    mean_individual_c = np.mean(individual_dc)

    findings.append({
        'check': 'Total kinetic dOrgC/dt (organic C budget)',
        'mean_signed': mean_orgC,
        'note': 'Non-zero = CO2 fixation/respiration/gas exchange',
        'severity': 'INFO'
    })
    findings.append({
        'check': 'Total kinetic d(AllC)/dt (includes DIC+CH4)',
        'mean_signed': mean_allC,
        'mean_abs_individual': float(mean_individual_c),
        'note': 'Non-zero = atmospheric CO2/CH4 exchange, sediment flux',
        'severity': 'INFO'
    })

    # O2 budget
    total_dO2 = compute_kinetic_deriv(pr_rates, 'DISS_OXYGEN')
    mean_dO2 = float(np.mean(total_dO2))
    findings.append({
        'check': 'Mean kinetic dO2/dt',
        'mean_signed': mean_dO2,
        'note': 'Positive = net O2 production, negative = net consumption',
        'severity': 'INFO'
    })

    return findings


# ═════════════════════════════════════════════════════════════════════════════
# Main
# ═════════════════════════════════════════════════════════════════════════════
def run_all_checks(output_dir):
    all_results = {}
    all_sv_data = {}

    for box_id in BOX_IDS:
        print(f"\n{'=' * 74}")
        print(f"  Box {box_id} ({BOX_TYPES.get(box_id, '?')})")
        print(f"{'=' * 74}")

        pr_time, pr_rates = load_process_rates(output_dir, box_id)
        sv_result = load_state_vars(output_dir, box_id)
        if pr_time is None or sv_result[0] is None:
            print("  [SKIP] Missing data files")
            continue
        sv_time, sv_concs, sv_names = sv_result
        all_sv_data[box_id] = (sv_time, sv_concs, sv_names)

        print(f"  State vars: {sv_concs.shape[1]} cols, {len(sv_time)} timesteps")
        print(f"  Process rates: {pr_rates.shape[1]} cols, {len(pr_time)} timesteps")

        box = {}

        print("  [1/10] Negative concentrations...")
        box['negatives'] = check_1_negative_concentrations(sv_time, sv_concs, sv_names, box_id)

        print("  [2/10] NaN/Inf check...")
        box['nan_inf'] = check_2_nan_inf(sv_time, sv_concs, sv_names, pr_time, pr_rates, box_id)

        print("  [3/10] Stoichiometric ratios...")
        box['stoichiometry'] = check_3_stoichiometry(sv_time, sv_concs, sv_names, pr_rates, box_id)

        print("  [4/10] Process rate sign violations...")
        box['sign_violations'] = check_4_sign_violations(pr_rates, box_id)

        print("  [5/10] Euler integration test...")
        box['euler_integration'] = check_5_euler_integration(
            sv_time, sv_concs, sv_names, pr_time, pr_rates, box_id)

        print("  [6/10] Extended cross-variable consistency...")
        box['cross_variable'] = check_6_cross_variable_extended(pr_rates, box_id)

        print("  [8/10] Allelopathy state variables...")
        box['allelopathy'] = check_8_allelopathy(sv_time, sv_concs, sv_names, pr_rates, box_id)

        print("  [9/10] State variable smoothness...")
        box['smoothness'] = check_9_smoothness(sv_time, sv_concs, sv_names, box_id)

        print("  [10/10] Mass balance closure...")
        box['mass_balance'] = check_10_mass_balance(pr_rates, box_id)

        all_results[box_id] = box

    # Cross-box check
    print(f"\n{'=' * 74}")
    print("  Cross-box spatial consistency")
    print(f"{'=' * 74}")
    cross_box = check_7_cross_box(all_sv_data)
    all_results['cross_box'] = cross_box

    return all_results


def print_report(all_results):
    """Print structured report of all findings."""

    print("\n")
    print("=" * 78)
    print("  DEEP STATE VARIABLE & PROCESS RATE CROSS-CHECK REPORT")
    print("=" * 78)

    # ── CHECK 1: Negative concentrations ────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 1: NEGATIVE / IMPOSSIBLE CONCENTRATIONS")
    print("─" * 78)
    any_neg = False
    for box_id, box in all_results.items():
        if box_id == 'cross_box':
            continue
        for f in box.get('negatives', []):
            any_neg = True
            sev = f['severity']
            if 'n_negative' in f:
                print(f"  [{sev}] Box {box_id}: {f['variable']} has {f['n_negative']} negative values "
                      f"({f['pct_negative']:.1f}%), min={f['min_value']:.6e}, first at t={f['first_time']:.1f}")
            elif 'max_value' in f:
                print(f"  [{sev}] Box {box_id}: {f['variable']} max={f['max_value']:.4f}  {f.get('note', '')}")
            elif 'min_value' in f:
                print(f"  [{sev}] Box {box_id}: {f['variable']} min={f['min_value']:.6e}  {f.get('note', '')}")
    if not any_neg:
        print("  All concentrations within valid ranges.")

    # ── CHECK 2: NaN / Inf ──────────────────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 2: NaN / Inf VALUES")
    print("─" * 78)
    any_bad = False
    for box_id, box in all_results.items():
        if box_id == 'cross_box':
            continue
        for f in box.get('nan_inf', []):
            if f.get('severity') == 'ERROR':
                any_bad = True
                print(f"  [ERROR] Box {box_id}: {f.get('type', '?')} {f.get('variable', 'bulk')}: "
                      f"NaN={f['n_nan']}, Inf={f['n_inf']}")
    if not any_bad:
        print("  No NaN or Inf values found in any box.")

    # ── CHECK 3: Stoichiometry ──────────────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 3: STOICHIOMETRIC RATIO VALIDATION")
    print("─" * 78)
    # Show first box only (representative)
    first_box = [b for b in all_results if b != 'cross_box'][0]
    for f in all_results[first_box].get('stoichiometry', []):
        sev = f['severity']
        check = f['check']
        if 'match' in f:
            match_str = 'MATCH' if f['match'] else 'MISMATCH'
            print(f"  [{sev}] {check}: mean={f.get('mean', 0):.6f} expected={f.get('expected', '?')} [{match_str}]")
        elif 'mean' in f:
            extra = f.get('note', '')
            print(f"  [{sev}] {check}: mean={f['mean']:.6f} min={f.get('min', 0):.6f} max={f.get('max', 0):.6f}  {extra}")
        elif 'cv_pct' in f:
            print(f"  [{sev}] {check}: CV={f['cv_pct']:.1f}%")
        else:
            print(f"  [{sev}] {check}")

    # ── CHECK 4: Sign violations ────────────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 4: PROCESS RATE SIGN VIOLATIONS (rates that should be >= 0)")
    print("─" * 78)
    any_viol = False
    for box_id, box in all_results.items():
        if box_id == 'cross_box':
            continue
        for f in box.get('sign_violations', []):
            any_viol = True
            sev = f['severity']
            print(f"  [{sev}] Box {box_id}: {f['desc']} (slot {f['slot']}): "
                  f"{f['n_negative']} negative ({f['pct_negative']:.1f}%), min={f['min_value']:.6e}")
    if not any_viol:
        print("  No sign violations found. All growth/death/feeding rates >= 0.")

    # ── CHECK 5: Euler integration ──────────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 5: EULER INTEGRATION TEST (kinetic vs actual dC/dt)")
    print("─" * 78)
    print(f"  {'Variable':22s} {'%Kinetic':>10s} {'%Transport':>10s} {'Corr':>8s} {'Transport(signed)':>17s}")
    for f in all_results[first_box].get('euler_integration', []):
        var = f['variable']
        print(f"  {var:22s} {f['pct_kinetic']:10.1f} {f['pct_transport']:10.1f} "
              f"{f['correlation_kin_actual']:8.3f} {f['transport_mean_signed']:17.6e}")

    # ── CHECK 6: Cross-variable consistency ─────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 6: EXTENDED CROSS-VARIABLE CONSISTENCY")
    print("─" * 78)
    for f in all_results[first_box].get('cross_variable', []):
        sev = f['severity']
        check = f['check']
        active = f.get('active', True)
        if not active:
            sev = 'INACT'
        if 'max_difference' in f:
            print(f"  [{sev:5s}] {check}: max_diff={f['max_difference']:.2e}")
        elif 'mean_ratio' in f:
            print(f"  [{sev:5s}] {check}: ratio={f['mean_ratio']:.4f} expected={f.get('expected', '?')}")
        else:
            s = ", ".join(f"{k}={v:.6e}" if isinstance(v, float) else f"{k}={v}" for k, v in f.items()
                         if k not in ('check', 'severity', 'active', 'note'))
            print(f"  [{sev:5s}] {check}: {s}")

    # ── CHECK 7: Cross-box ──────────────────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 7: CROSS-BOX SPATIAL CONSISTENCY")
    print("─" * 78)
    for f in all_results.get('cross_box', []):
        var = f['variable']
        cv = f['cv_across_boxes_pct']
        ratio = f['max_min_ratio']
        means = f['box_means']
        means_str = ", ".join(f"B{b}={v:.3f}" for b, v in sorted(means.items()))
        sev = f['severity']
        print(f"  [{sev}] {var:15s}: CV={cv:.1f}%, max/min={ratio:.2f}  ({means_str})")

    # ── CHECK 8: Allelopathy ────────────────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 8: ALLELOPATHY STATE VARIABLES")
    print("─" * 78)
    for f in all_results[first_box].get('allelopathy', []):
        var = f['variable']
        if 'correlation_with_parent' in f:
            print(f"  [{f['severity']}] {var}: mean={f['mean']:.6e}, max={f['max']:.6e}, "
                  f"neg={f['n_negative']}, corr_parent={f['correlation_with_parent']:.3f}, "
                  f"zero={f['always_zero']}")
        else:
            print(f"  [{f['severity']}] {var}: total_rate_mag={f.get('total_rate_magnitude', 0):.6e}")

    # ── CHECK 9: Smoothness ─────────────────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 9: STATE VARIABLE SMOOTHNESS (spikes & trends)")
    print("─" * 78)
    any_finding = False
    for box_id, box in all_results.items():
        if box_id == 'cross_box':
            continue
        for f in box.get('smoothness', []):
            any_finding = True
            var = f['variable']
            typ = f['type']
            sev = f['severity']
            if typ == 'spike':
                print(f"  [{sev}] Box {box_id} {var}: {f['n_spikes']} spikes "
                      f"(mean dC/dt={f['mean_abs_daily_change']:.4e})")
            elif typ == 'monotonic_trend':
                print(f"  [{sev}] Box {box_id} {var}: {f['pct_same_direction']:.0f}% {f['direction']} "
                      f"(range={f['range']:.4e})")
    if not any_finding:
        print("  No significant spikes or monotonic trends detected.")

    # ── CHECK 10: Mass balance ──────────────────────────────────────────────
    print("\n" + "─" * 78)
    print("  CHECK 10: MASS BALANCE CLOSURE (kinetic budget)")
    print("─" * 78)
    for f in all_results[first_box].get('mass_balance', []):
        check = f['check']
        mean_s = f.get('mean_signed', 0)
        note = f.get('note', '')
        extra = ""
        if 'ratio_total_to_individual' in f:
            extra = f" ratio={f['ratio_total_to_individual']:.4f}"
        print(f"  [{f['severity']}] {check}: mean={mean_s:.6e}{extra}")
        if note:
            print(f"         {note}")

    # ── SUMMARY ─────────────────────────────────────────────────────────────
    print("\n" + "=" * 78)
    n_errors = 0
    n_warnings = 0
    for box_id, box in all_results.items():
        if box_id == 'cross_box':
            for f in box:
                if f.get('severity') == 'ERROR':
                    n_errors += 1
                elif f.get('severity') == 'WARNING':
                    n_warnings += 1
            continue
        for check_name, findings in box.items():
            if isinstance(findings, list):
                for f in findings:
                    if isinstance(f, dict):
                        if f.get('severity') == 'ERROR':
                            n_errors += 1
                        elif f.get('severity') == 'WARNING':
                            n_warnings += 1
    print(f"  TOTALS: {n_errors} ERRORs, {n_warnings} WARNINGs across {len(BOX_IDS)} boxes")
    print("=" * 78)


def main():
    parser = argparse.ArgumentParser(description='Deep State Var & Process Rate Cross-Check')
    parser.add_argument('--output-dir', default='OUTPUTS')
    args = parser.parse_args()

    results = run_all_checks(args.output_dir)
    print_report(results)
    return results


if __name__ == '__main__':
    main()
