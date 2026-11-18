"""Utility functions for AQUABC Shiny application.

Extracted from app.py for independent testability.
Contains file I/O helpers, model output readers, and validation logic.
"""
import logging
import os
import re

import pandas as pd

logger = logging.getLogger("AQUABC")

# Fix ROOT path when running via symlink
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))
INPUTS_DIR = os.path.join(ROOT, 'INPUTS')

# Number of pelagic model constants the model requires: `nconst` in
# SOURCE_CODE/ESTAS/mod_GLOBAL.f90 (tests/python/test_audit_contracts.py pins the two).
REQUIRED_MODEL_CONSTANTS = 324


def format_full_precision(value) -> str:
    """Shortest text that reads back as exactly the same float (12.825253, 1e-06, 0.0).

    For writing edited values into model input files: a fixed number of significant
    digits (the old `%.6g`) silently rounds the value the user typed.
    """
    return repr(float(value))


def thin_rows(df, max_rows):
    """Evenly thin a time series to at most about `max_rows` rows for plotting.

    Returns (frame, step): every `step`-th row over the WHOLE record, with the last
    row kept so the plotted period ends where the data ends. step == 1 means the
    frame is returned unchanged. Reading only the first `max_rows` rows instead would
    silently cut a long record short.
    """
    n = len(df)
    if not max_rows or n <= max_rows:
        return df, 1
    step = -(-n // max_rows)                      # ceil(n / max_rows)
    idx = list(range(0, n, step))
    if idx[-1] != n - 1:
        idx.append(n - 1)
    return df.iloc[idx], step

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

    Handles both files with a proper header line and header-less files
    (e.g. PROCESS_RATES) where all columns are numeric.

    Args:
        text_file: Path to .out file
        max_rows: Maximum rows to read (None for all)

    Returns:
        DataFrame with state variable columns
    """
    df = pd.read_csv(text_file, sep=r'\s+', nrows=max_rows)
    df.columns = [c.strip() for c in df.columns]

    # Detect header-less file: first column name looks like a number
    # (proper headers start with strings like TIME_DAYS)
    def _first_col_numeric(cols):
        try:
            float(cols[0])
            return True
        except (ValueError, TypeError, IndexError):
            return False

    if _first_col_numeric(df.columns):
        logger.warning(f"Header-less text file detected: {os.path.basename(text_file)}")
        # Re-read without header so the first data row is not lost
        df = pd.read_csv(text_file, sep=r'\s+', header=None, nrows=max_rows)
        ncols = len(df.columns)
        if ncols == len(PELAGIC_BOX_COLUMNS):
            df.columns = PELAGIC_BOX_COLUMNS
        else:
            df.columns = [f"V{i}" for i in range(ncols)]

    logger.info(f"Read text file: {len(df)} rows x {len(df.columns)} cols")
    return df


def validate_constants_file(constants_filename, inputs_dir=None):
    """Validate that a WCONST file has exactly the constants the model reads.

    The Fortran reader stores constants by index and aborts on a missing index, a
    duplicate or an index above the required count, so all three are errors here.

    Args:
        constants_filename: The filename (e.g., 'WCONST_04.txt') or full path
        inputs_dir: folder a bare filename is looked up in -- the active setup's
            input folder (the model prepends PELAGIC_INPUT_FOLDER). Defaults to
            the Standard INPUTS/.

    Returns:
        tuple: (is_valid: bool, highest_index: int, error_message: str or None)
    """
    if not constants_filename:
        return True, 0, None  # No constants file specified, model uses defaults

    # Build full path if not already absolute
    if os.path.isabs(constants_filename):
        filepath = constants_filename
    else:
        filepath = os.path.join(inputs_dir or INPUTS_DIR, constants_filename)

    if not os.path.exists(filepath):
        return False, 0, f"Constants file not found: {filepath}"

    try:
        # Numbered constant lines: "   123   CONSTANT_NAME   value  !comment"
        indices = []
        with open(filepath, encoding='utf-8', errors='ignore') as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                match = re.match(r'^(\d+)\s+\w+', line)
                if match:
                    indices.append(int(match.group(1)))

        name = os.path.basename(filepath)
        max_const_num = max(indices, default=0)
        seen = set(indices)
        missing = [i for i in range(1, REQUIRED_MODEL_CONSTANTS + 1) if i not in seen]
        extra = sorted(i for i in seen if i > REQUIRED_MODEL_CONSTANTS)
        duplicates = sorted({i for i in indices if indices.count(i) > 1})

        def some(nums):
            return ", ".join(map(str, nums[:8])) + (" ..." if len(nums) > 8 else "")

        if max_const_num < REQUIRED_MODEL_CONSTANTS:
            return False, max_const_num, (
                f"Constants file '{name}' has only {max_const_num} constants, "
                f"but the model requires {REQUIRED_MODEL_CONSTANTS}.\n"
                f"Missing constants: {max_const_num + 1} to {REQUIRED_MODEL_CONSTANTS}\n"
                f"Try using WCONST_04.txt which has all required constants."
            )
        if missing:
            return False, max_const_num, (
                f"Constants file '{name}' is missing constant(s) {some(missing)}; "
                f"the model requires every index 1 to {REQUIRED_MODEL_CONSTANTS}.")
        if extra:
            return False, max_const_num, (
                f"Constants file '{name}' has constant(s) {some(extra)} above the "
                f"{REQUIRED_MODEL_CONSTANTS} the model reads; it stops on an index out of range.")
        if duplicates:
            return False, max_const_num, (
                f"Constants file '{name}' lists constant(s) {some(duplicates)} more than once.")

        return True, max_const_num, None

    except Exception as e:
        return False, 0, f"Error reading constants file: {e}"
