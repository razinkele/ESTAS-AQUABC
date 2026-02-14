"""Utility functions for AQUABC Shiny application.

Extracted from app.py for independent testability.
Contains file I/O helpers, model output readers, and validation logic.
"""
import os
import re
import logging

import pandas as pd

logger = logging.getLogger("AQUABC")

# Fix ROOT path when running via symlink
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))
INPUTS_DIR = os.path.join(ROOT, 'INPUTS')

# Constants
REQUIRED_MODEL_CONSTANTS = 318  # Model requires exactly 318 constants

# Standard column names for PELAGIC_BOX output files (binary and text)
PELAGIC_BOX_COLUMNS = [
    "TIME_DAYS", "NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C",
    "ZOO_C", "ZOO_N", "ZOO_P", "DET_PART_ORG_C", "DET_PART_ORG_N",
    "DET_PART_ORG_P", "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P",
    "CYN_C", "OPA_C", "DISS_Si", "PART_Si", "FIX_CYN_C",
    "INORG_C", "TOT_ALK", "FE_II", "FE_III", "MN_II", "MN_IV",
    "CA", "MG", "S_PLUS_6", "S_MINUS_2", "CH4_C",
    "NOST_VEG_HET_C", "AKI_C", "SEC_METAB_DIA",
    "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST"
]


def count_file_lines_fast(filepath, sample_size=8192):
    """Efficiently count lines in a file using buffered reading.

    For large files, uses sampling to estimate. For small files, counts exactly.
    """
    try:
        file_size = os.path.getsize(filepath)
        if file_size < 1024 * 1024:  # < 1MB: count exactly
            with open(filepath, 'rb') as f:
                return sum(1 for _ in f)
        else:
            # Sample first chunk to estimate bytes per line
            with open(filepath, 'rb') as f:
                sample = f.read(sample_size)
                lines_in_sample = sample.count(b'\n')
                if lines_in_sample > 0:
                    bytes_per_line = sample_size / lines_in_sample
                    return int(file_size / bytes_per_line)
            return file_size // 100  # Fallback estimate
    except Exception:
        return 0


def read_pelagic_binary(bin_file, max_rows=None):
    """Read Fortran binary PELAGIC_BOX output file.

    Binary format (from Fortran stream I/O):
    - Each row: TIME (float64) + 36 state variables (float64)
    - Total 37 columns, all double precision (8 bytes)
    - No record markers (Fortran ACCESS='STREAM')

    Args:
        bin_file: Path to .bin file
        max_rows: Maximum rows to read (None for all)

    Returns:
        DataFrame with TIME_DAYS and state variable columns
    """
    import numpy as np

    ncols = len(PELAGIC_BOX_COLUMNS)  # 37

    with open(bin_file, 'rb') as f:
        data = np.fromfile(f, dtype=np.float64)

    nrows = len(data) // ncols
    remainder = len(data) % ncols

    if remainder != 0:
        logger.warning(f"Binary file has {remainder} extra bytes, truncating")
        data = data[:nrows * ncols]

    data = data.reshape(nrows, ncols)

    if max_rows is not None and nrows > max_rows:
        data = data[:max_rows]

    df = pd.DataFrame(data, columns=PELAGIC_BOX_COLUMNS)
    logger.info(f"Read binary file: {len(df)} rows x {ncols} cols")
    return df


def read_pelagic_text(text_file, max_rows=None):
    """Read PELAGIC_BOX text output file (whitespace-separated).

    Args:
        text_file: Path to .out file
        max_rows: Maximum rows to read (None for all)

    Returns:
        DataFrame with state variable columns
    """
    df = pd.read_csv(text_file, sep=r'\s+', nrows=max_rows)
    df.columns = [c.strip() for c in df.columns]
    logger.info(f"Read text file: {len(df)} rows x {len(df.columns)} cols")
    return df


def validate_constants_file(constants_filename):
    """Validate that a WCONST file has the required number of constants.

    Args:
        constants_filename: The filename (e.g., 'WCONST_01.txt') or full path

    Returns:
        tuple: (is_valid: bool, actual_count: int, error_message: str or None)
    """
    if not constants_filename:
        return True, 0, None  # No constants file specified, model uses defaults

    # Build full path if not already absolute
    if os.path.isabs(constants_filename):
        filepath = constants_filename
    else:
        filepath = os.path.join(INPUTS_DIR, constants_filename)

    if not os.path.exists(filepath):
        return False, 0, f"Constants file not found: {filepath}"

    try:
        # Count numbered constant lines (format: "   123   CONSTANT_NAME   value  !comment")
        # Lines start with whitespace followed by a number
        const_count = 0
        max_const_num = 0

        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                # Match lines starting with a number (the constant index)
                match = re.match(r'^(\d+)\s+\w+', line)
                if match:
                    const_num = int(match.group(1))
                    max_const_num = max(max_const_num, const_num)
                    const_count += 1

        if max_const_num < REQUIRED_MODEL_CONSTANTS:
            return False, max_const_num, (
                f"Constants file '{os.path.basename(filepath)}' has only {max_const_num} constants, "
                f"but the model requires {REQUIRED_MODEL_CONSTANTS}.\n"
                f"Missing constants: {max_const_num + 1} to {REQUIRED_MODEL_CONSTANTS}\n"
                f"Try using WCONST_04.txt which has all required constants."
            )

        return True, max_const_num, None

    except Exception as e:
        return False, 0, f"Error reading constants file: {e}"
