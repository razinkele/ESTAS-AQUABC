#!/usr/bin/env python3
"""
Shared utilities for AQUABC analysis scripts.
=============================================
Constants, data loading, and helper functions used by:
  - deep_process_rate_analysis.py
  - deep_state_vs_process_crosscheck.py
  - generate_deep_pr_pdf.py
  - generate_crosscheck_pdf.py
"""

import os
import numpy as np

# ─────────────────────────────────────────────────────────────────────────────
# Model structure constants
# ─────────────────────────────────────────────────────────────────────────────
NDIAGVAR = 30
NSTATE = 32
NUM_ALLELOPATHY = 4
NSTATE_TOTAL = NSTATE + NUM_ALLELOPATHY  # 36
NUM_PROCESS_RATES = NSTATE_TOTAL * NDIAGVAR  # 1080

# ─────────────────────────────────────────────────────────────────────────────
# Box configuration
# ─────────────────────────────────────────────────────────────────────────────
BOX_IDS = [5, 6, 8, 9, 14, 17, 25]
BOX_TYPES = {
    5: 'sand', 6: 'sand', 8: 'sand', 9: 'sand',
    14: 'mud', 17: 'mud', 25: 'mud',
}

# ─────────────────────────────────────────────────────────────────────────────
# State variable names (1-based index 1..36)
# ─────────────────────────────────────────────────────────────────────────────
STATE_VAR_NAMES = [
    'NH4_N', 'NO3_N', 'PO4_P', 'DISS_OXYGEN', 'DIA_C', 'ZOO_C',
    'ZOO_N', 'ZOO_P', 'DET_PART_ORG_C', 'DET_PART_ORG_N', 'DET_PART_ORG_P',
    'DISS_ORG_C', 'DISS_ORG_N', 'DISS_ORG_P', 'CYN_C', 'OPA_C',
    'DISS_Si', 'PART_Si', 'FIX_CYN_C', 'INORG_C', 'TOT_ALK',
    'FE_II', 'FE_III', 'MN_II', 'MN_IV', 'CA', 'MG',
    'S_PLUS_6', 'S_MINUS_2', 'CH4_C', 'NOST_VEG_HET_C', 'AKI_C',
    'SEC_METAB_DIA', 'SEC_METAB_NOFIX_CYN', 'SEC_METAB_FIX_CYN', 'SEC_METAB_NOST',
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

# ─────────────────────────────────────────────────────────────────────────────
# Stoichiometric constants (from aquabc_II_pelagic_model_constants.f90)
# ─────────────────────────────────────────────────────────────────────────────
DEFAULT_N_TO_C = 0.220       # mg N / mg C  (all organisms share this default)
DEFAULT_P_TO_C = 0.024       # mg P / mg C
DEFAULT_O2_TO_C = 2.66       # mg O2 / mg C
DEFAULT_Si_TO_C = 0.25       # mg Si / mg C  (diatoms)

# ─────────────────────────────────────────────────────────────────────────────
# Variables that MUST remain non-negative (concentrations)
# ─────────────────────────────────────────────────────────────────────────────
NON_NEGATIVE_VARS = [
    'NH4_N', 'NO3_N', 'PO4_P', 'DISS_OXYGEN',
    'DIA_C', 'ZOO_C', 'ZOO_N', 'ZOO_P',
    'DET_PART_ORG_C', 'DET_PART_ORG_N', 'DET_PART_ORG_P',
    'DISS_ORG_C', 'DISS_ORG_N', 'DISS_ORG_P',
    'CYN_C', 'OPA_C', 'DISS_Si', 'PART_Si',
    'FIX_CYN_C', 'NOST_VEG_HET_C', 'AKI_C',
    'FE_II', 'FE_III', 'MN_II', 'MN_IV',
    'S_PLUS_6', 'CH4_C',
]

# ─────────────────────────────────────────────────────────────────────────────
# Process rate slots that must be non-negative (growth/death/feeding rates)
# Format: (var_name, slot_number, description)
# ─────────────────────────────────────────────────────────────────────────────
NONNEG_RATE_SLOTS = [
    # Phyto growth
    ('DIA_C', 1, 'DIA_C growth'),
    ('CYN_C', 1, 'CYN_C growth'),
    ('OPA_C', 1, 'OPA_C growth'),
    ('FIX_CYN_C', 1, 'FIX_CYN_C growth (total)'),
    ('NOST_VEG_HET_C', 1, 'NOST growth'),
    # Respiration
    ('DIA_C', 2, 'DIA_C respiration'),
    ('CYN_C', 2, 'CYN_C respiration'),
    ('OPA_C', 2, 'OPA_C respiration'),
    ('FIX_CYN_C', 2, 'FIX_CYN_C respiration'),
    ('NOST_VEG_HET_C', 2, 'NOST respiration'),
    # Excretion
    ('DIA_C', 3, 'DIA_C excretion'),
    ('CYN_C', 3, 'CYN_C excretion'),
    ('OPA_C', 3, 'OPA_C excretion'),
    ('FIX_CYN_C', 3, 'FIX_CYN_C excretion'),
    ('NOST_VEG_HET_C', 3, 'NOST excretion'),
    # Death
    ('DIA_C', 4, 'DIA_C death'),
    ('CYN_C', 4, 'CYN_C death'),
    ('OPA_C', 4, 'OPA_C death'),
    ('FIX_CYN_C', 4, 'FIX_CYN_C death'),
    ('NOST_VEG_HET_C', 4, 'NOST death'),
    # Zoo feeding
    ('DIA_C', 5, 'DIA_C zoo grazing'),
    ('CYN_C', 5, 'CYN_C zoo grazing'),
    ('OPA_C', 5, 'OPA_C zoo grazing'),
    ('FIX_CYN_C', 5, 'FIX_CYN_C zoo grazing'),
    # Zoo
    ('ZOO_C', 1, 'ZOO_C growth/feeding'),
    ('ZOO_C', 2, 'ZOO_C DOC excretion'),
    ('ZOO_C', 3, 'ZOO_C respiration'),
    ('ZOO_C', 4, 'ZOO_C death'),
    # Akinete
    ('AKI_C', 1, 'Akinete formation'),
    ('AKI_C', 2, 'Akinete germination'),
    ('AKI_C', 3, 'Akinete loss'),
    ('AKI_C', 4, 'Akinete mortality'),
    # Nitrification
    ('NH4_N', 10, 'Nitrification'),
    ('NO3_N', 1, 'Nitrification gain'),
]

# ─────────────────────────────────────────────────────────────────────────────
# Severity levels (standardized)
# ─────────────────────────────────────────────────────────────────────────────
SEV_ERROR = 'ERROR'
SEV_WARNING = 'WARNING'
SEV_INFO = 'INFO'
SEV_OK = 'OK'


# ─────────────────────────────────────────────────────────────────────────────
# Utility functions
# ─────────────────────────────────────────────────────────────────────────────

def get_slot_col(var_idx_1based, slot_1based):
    """Get 0-based column index in process rate data array (excluding TIME col)."""
    return (var_idx_1based - 1) * NDIAGVAR + (slot_1based - 1)


def load_process_rates(output_dir, box_id):
    """Load process rate file for a given box. Returns (time, rates_2d) or (None, None)."""
    fname = os.path.join(output_dir, f'PELAGIC_BOX_{box_id:05d}_PROCESS_RATES.out')
    if not os.path.exists(fname):
        return None, None
    data = np.loadtxt(fname)
    return data[:, 0], data[:, 1:]


def load_state_vars(output_dir, box_id):
    """Load state variable file for a given box. Returns (time, concs, header_names) or (None, None, None)."""
    fname = os.path.join(output_dir, f'PELAGIC_BOX_{box_id:05d}.out')
    if not os.path.exists(fname):
        return None, None, None
    with open(fname) as f:
        header = f.readline().split()
    names = header[1:]
    # Defense-in-depth: analysis maps variable -> column via names.index(name) on THIS
    # header, so wrong data is only possible if the Fortran output column order drifts.
    # Assert it still matches STATE_VAR_NAMES so any drift fails loudly here instead of
    # silently returning the wrong column downstream (e.g. reading DISS_ORG_P as CYN_C).
    if names != STATE_VAR_NAMES:
        import warnings
        expected = set(STATE_VAR_NAMES)
        got = set(names)
        warnings.warn(
            f"PELAGIC_BOX_{box_id:05d}.out header does not match STATE_VAR_NAMES -- "
            f"column-by-name lookups may be unsafe. Missing: {sorted(expected - got)}; "
            f"unexpected: {sorted(got - expected)}; order-changed: "
            f"{names != STATE_VAR_NAMES and expected == got}.",
            stacklevel=2,
        )
    data = np.loadtxt(fname, skiprows=1)
    return data[:, 0], data[:, 1:], names


def compute_kinetic_deriv(rates, var_name, slot_map, derivative_signs):
    """Sum of signed process rates = kinetic dC/dt for a state variable."""
    if var_name not in slot_map:
        return np.zeros(rates.shape[0])
    info = slot_map[var_name]
    var_idx = info['var_index']
    signs = derivative_signs.get(var_name, {})
    deriv = np.zeros(rates.shape[0])
    for slot, sign in signs.items():
        col = get_slot_col(var_idx, slot)
        if col < rates.shape[1]:
            deriv += sign * rates[:, col]
    return deriv


def compute_numerical_derivative(time, conc):
    """Compute numerical dC/dt from concentration data using forward differences."""
    dt = np.diff(time)
    dc = np.diff(conc)
    return dc / dt


def find_sv_column(var_name, sv_names):
    """Find the column index for a state variable in the output header."""
    for i, sn in enumerate(sv_names):
        if sn == var_name or sn.upper() == var_name.upper():
            return i
        if var_name == 'NOST_VEG_HET_C' and 'NOST' in sn.upper() and 'VEG' in sn.upper():
            return i
    return None
