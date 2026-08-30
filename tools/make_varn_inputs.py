#!/usr/bin/env python3
"""Generate a 37-variable VARN CL29 setup from a standard 36-variable CL29 setup.

Adds CYN_N -- the nitrogen-quota companion to CYN_C required by the opt-in
Droop mechanism (docs/superpowers/specs/2026-08-30-cyn-droop-n-rescoped-design.md)
-- as state variable 33 to every per-state-variable structure in an ESTAS
pelagic-box setup, shifting the four allelopathy metabolite state variables
(SEC_METAB_DIA/NOFIX_CYN/FIX_CYN/NOST) from 33-36 to 34-37. CYN_C is state
variable 15 (aquabc_II_pelagic_svindex.f90::CYN_C_INDEX); CYN_N_INDEX = 33
there too -- these two constants below MUST match that Fortran source file,
they are not independently choosable.

Transforms, by file (discovered from `PELAGIC_INPUTS.txt`'s own reference
tables, not hard-coded filenames, except PELAGIC_INPUTS.txt and
PELAGIC_MODEL_OPTIONS.txt themselves):

  PELAGIC_INPUTS.txt
    - declared total (# NUM_PELAGIC_STATE_VARS): 36 -> 37
    - variable table: new row 33 CYN_N, transported like CYN_C (same
      MEASUREMENT ERROR / OUTPUT_TYPE / INITIAL_CONDITION_TYPE columns);
      metabolite rows renumbered 33-36 -> 34-37
    - SETTLING_VELOCITIES block (per box): new row 33 mirrors that box's
      CYN_C row (dissolved frac / settling vel no / deposited fraction /
      chla suppression) exactly -- a physical-consistency requirement, not
      an invented value; metabolite rows renumbered
    - OPEN BOUNDARIES block (per boundary): new row state-var-no=33,
      forcing-ts-no=<unchanged, same boundary>, forcing-ts-var-no=33 (the
      new column position in that boundary's FORC_TS_*.txt); metabolite
      rows' state-var-no AND forcing-ts-var-no both renumbered

  Each PELAGIC INITIAL CONDITION FILE (discovered from PELAGIC_INPUTS.txt's
  "INITIAL CONDITIONS" file-name table, e.g. INIT_CONC_1.txt/INIT_CONC_2.txt)
    - new row 33 CYN_N = Q_SEED * (that file's CYN_C row); metabolite rows
      renumbered

  Each FORC_TS_*.txt referenced as an open-boundary concentration series
  (discovered from the OPEN BOUNDARIES block's forcing-ts numbers, resolved
  through PELAGIC_INPUTS.txt's "FORCING TIME SERIES" file-name table --
  NOT every FORC_TS_*.txt: e.g. FORC_TS_9.txt is a per-box day-fraction
  series with NUMBER_OF_VARIABLES=29 and is correctly left untouched)
    - NUMBER_OF_VARIABLES header: 36 -> 37
    - SCALE FACTORS row: new value at position 33 mirrors position 15
      (CYN_C's own scale factor); UNIT CONVERSION FACTORS row: same
    - every TIME AND VALUES data row: new field at column 33 (after the
      leading time column) = Q_SEED * that row's CYN_C field; metabolite
      columns shifted right

  PELAGIC_MODEL_OPTIONS.txt
    - the five CYN_VARIABLE_N / CYN_N_QMIN / CYN_N_QMAX / CYN_N_VMAX /
      CYN_N_KHS_UPT comment+value pairs inserted immediately before the
      `# CYN_ALLELOPATHY_FILE_NAME` line -- the exact position
      READ_PELAGIC_MODEL_OPTIONS (mod_PELAGIC_ECOLOGY.f90) expects, NOT
      appended at EOF (the graceful reader would silently default an
      appended block away)

  Any OTHER file matching the "BOX NO / STATE VAR NO / FORCING TS NO /
  FORCING TS VAR NO" per-box-per-state-variable header signature
  (discovered by content, not by name -- in the real CL29 setup this is
  SED_FLUX_NO3_SINK.txt, the prescribed-sediment-flux assignment table;
  READ_BOTTOM_SEDS_FLUXES_INPUTS loops NUM_PELAGIC_BOXES x
  NUM_PELAGIC_STATE_VARS, so this file breaks under the VARN build if left
  at 36 rows/box -- not mentioned by name in the originating task brief,
  found by reading the real reader)
    - new row 33 per box mirrors that box's CYN_C row's (forcing-ts-no,
      forcing-ts-var-no) exactly, same rationale as the settling block;
      metabolite rows' state-var-no renumbered (their forcing-ts columns
      are untouched -- they reference an independent, unexpanded file)

Every other file in the source directory is copied verbatim.

Standard library only.

Usage:
    python tools/make_varn_inputs.py <src_dir> <dst_dir>
"""
from __future__ import annotations

import argparse
import re
import shutil
from pathlib import Path

CYN_C_INDEX = 15        # aquabc_II_pelagic_svindex.f90::CYN_C_INDEX
CYN_N_INDEX = 33         # aquabc_II_pelagic_svindex.f90::CYN_N_INDEX
N_META = 4               # SEC_METAB_DIA/NOFIX_CYN/FIX_CYN/NOST, trailing state vars
Q_SEED = 0.220            # CYN_N_TO_C -- CYN_N IC/boundary = Q_SEED * CYN_C

# --degenerate-cyn: state-var numbers (pre- and post-insertion identical -- all
# below CYN_N_INDEX=33, so unaffected by the CYN_N insertion) for the CYN-only
# conservation scenario, read from INPUTS_CL29/PELAGIC_INPUTS.txt's own
# "PELAGIC STATE VARIABLES" table.
#   phyto/zoo state vars to zero (IC + boundary), i.e. every phyto/zoo pool
#   EXCEPT CYN_C(15)/CYN_N(33): DIA_C, ZOO_C, ZOO_N, ZOO_P, OPA_C, FIX_CYN_C,
#   NOST_VEG_HET_C, AKI_C (the dormant akinete pool -- zeroed too so it cannot
#   later germinate into NOST_VEG_HET_C even though NOST staging is off).
DEGENERATE_ZERO_PHYTO_ZOO = [5, 6, 7, 8, 16, 19, 31, 32]
#   N pools in the conservation identity Delta(NH4+NO3+DON+DET_N+CYN_N): only
#   their BOUNDARY inflow is zeroed (not their IC -- they seed the N the CYN
#   population draws down and returns to).
DEGENERATE_ZERO_BOUNDARY_N = [1, 2, 10, 13]  # NH4_N, NO3_N, DET_PART_ORG_N, DISS_ORG_N
#   Internal N-destroying kinetic processes not attributable to any
#   MASS_BALANCES.out column (both are lumped into KINETICS along with
#   every N-conserving transformation, so they cannot be subtracted out
#   after the fact -- they must be disabled at the source):
#     - water-column denitrification (aquabc_II_pelagic_model.f90:1880,
#       R_DENITRIFICATION = 0.93 * R_ABIOTIC_DOC_MIN_NO3N, rate constant
#       K_MIN_DOC_NO3N_20, WCONST constant no.165): NO3_N -> N2 gas.
#     - ammonia volatilization (aquabc_II_pelagic_model.f90:1965,
#       AMMONIA_VOLATILIZATION() driven by K_A_CALC, which literally IS the
#       WCONST K_A constant no.1 whenever K_A >= 0 -- see
#       aquabc_II_pelagic_model.f90:1030-1037, K_A<0 means "compute from
#       wind" and K_A>=0 means "use this constant directly", so K_A=0.0 is
#       a real, exact "no gas exchange" setting, not a special case):
#       NH4_N -> NH3 gas (this doubles as disabling O2 reaeration, which is
#       fine -- DISS_OXYGEN is not part of the conservation identity).
#   Zeroed in the scenario's own WCONST file(s) (discovered from
#   PELAGIC_INPUTS.txt's "PELAGIC MODEL CONSTANTS FILE NAME" table, not
#   hard-coded to WCONST_04.txt) so the identity closes WITHOUT having to
#   also disable transport (ADVECTION/DIFFUSION/SETTLING/SEDIMENT FLUXES
#   stay real and are accounted for explicitly by check_varn_run.py
#   --mode conserve from MASS_BALANCES.out, since CL29's net advective
#   flushing means zero boundary concentration alone does not close a
#   whole-domain sum).
DEGENERATE_ZERO_WCONST_NAMES = ["K_MIN_DOC_NO3N_20", "K_A"]

BOXSTATE_HEADER_RE = re.compile(r'STATE VAR NO\s+FORCING TS NO\s+FORCING TS VAR NO')


# --------------------------------------------------------------------------
# generic whitespace-token helpers -- edit only the token(s) that must
# change, leave every other character (spacing, comments, alignment) as-is
# --------------------------------------------------------------------------

def _tokens(line):
    return list(re.finditer(r'\S+', line))


def _token(line, idx):
    return _tokens(line)[idx].group()


def _set_token(line, idx, value):
    tok = _tokens(line)[idx]
    return line[:tok.start()] + str(value) + line[tok.end():]


def _find(lines, pred, start=0):
    for i in range(start, len(lines)):
        if pred(lines[i]):
            return i
    raise ValueError(f"marker not found starting at line {start}")


def _consume_data_block(lines, start):
    """Return end index (exclusive) of the run of non-blank, non-'#' lines
    beginning at `start`."""
    end = start
    while end < len(lines) and lines[end].strip() and not lines[end].lstrip().startswith("#"):
        end += 1
    return end


def _split_groups(rows):
    """Split rows into groups by their leading (0th) whitespace token,
    preserving order and consecutive-run boundaries."""
    groups = []
    cur_key = None
    cur = []
    for ln in rows:
        key = _token(ln, 0)
        if key != cur_key and cur:
            groups.append(cur)
            cur = []
        cur.append(ln)
        cur_key = key
    if cur:
        groups.append(cur)
    return groups


# --------------------------------------------------------------------------
# per-box / per-boundary state-variable row-group transforms
# --------------------------------------------------------------------------

def _transform_box_group(group, has_name_comment):
    """SETTLING_VELOCITIES / SED_FLUX-style groups: token[1] = state var no.
    New row mirrors that group's CYN_C row (token[1] set to CYN_N_INDEX,
    every other token -- including forcing-ts columns where present --
    copied verbatim from the CYN_C row)."""
    cyn_c_row = next(r for r in group if int(_token(r, 1)) == CYN_C_INDEX)
    new_row = _set_token(cyn_c_row, 1, CYN_N_INDEX)
    if has_name_comment:
        new_row = _set_token(new_row, -1, "CYN_N")

    out = []
    inserted = False
    for row in group:
        no = int(_token(row, 1))
        if not inserted and no >= CYN_N_INDEX:
            out.append(new_row)
            inserted = True
        if no >= CYN_N_INDEX:
            row = _set_token(row, 1, no + 1)
        out.append(row)
    if not inserted:
        out.append(new_row)
    return out


def _transform_boundary_group(group):
    """OPEN BOUNDARIES-style groups: token[1] = state var no, token[3] =
    forcing ts var no, and (unlike the settling/SED_FLUX case) the NEW
    row's forcing-ts-var-no must equal CYN_N_INDEX (the new column
    position in the corresponding FORC_TS_*.txt), not mirror CYN_C's."""
    cyn_c_row = next(r for r in group if int(_token(r, 1)) == CYN_C_INDEX)
    new_row = _set_token(cyn_c_row, 1, CYN_N_INDEX)
    new_row = _set_token(new_row, 3, CYN_N_INDEX)

    out = []
    inserted = False
    for row in group:
        no = int(_token(row, 1))
        if not inserted and no >= CYN_N_INDEX:
            out.append(new_row)
            inserted = True
        if no >= CYN_N_INDEX:
            row = _set_token(row, 1, no + 1)
            row = _set_token(row, 3, no + 1)
        out.append(row)
    if not inserted:
        out.append(new_row)
    return out


# --------------------------------------------------------------------------
# PELAGIC_INPUTS.txt
# --------------------------------------------------------------------------

def transform_pelagic_inputs(lines):
    """Return (new_lines, init_conc_filenames, boundary_forc_ts_filenames)."""
    lines = list(lines)
    old_total = CYN_N_INDEX + N_META - 1  # 36

    # -- declared total --------------------------------------------------
    i_decl = _find(lines, lambda l: l.strip() == "# NUM_PELAGIC_STATE_VARS")
    declared = int(lines[i_decl + 1].strip())
    if declared != old_total:
        raise ValueError(f"declared NUM_PELAGIC_STATE_VARS={declared}, expected {old_total}")
    lines[i_decl + 1] = _set_token(lines[i_decl + 1], 0, old_total + 1)

    # -- variable table ----------------------------------------------------
    i_vt = _find(lines, lambda l: "PELAGIC STATE VARIABLES" in l)
    vt_start = i_vt + 2  # banner line, then column-header comment line
    vt_end = vt_start + old_total
    vt_rows = lines[vt_start:vt_end]
    if not (int(_token(vt_rows[CYN_C_INDEX - 1], 0)) == CYN_C_INDEX
            and _token(vt_rows[CYN_C_INDEX - 1], 1) == "CYN_C"):
        raise ValueError("variable-table row 15 is not CYN_C -- unexpected layout")
    new_vt_row = _set_token(vt_rows[CYN_C_INDEX - 1], 0, CYN_N_INDEX)
    new_vt_row = _set_token(new_vt_row, 1, "CYN_N")
    new_vt_rows = []
    inserted = False
    for row in vt_rows:
        no = int(_token(row, 0))
        if not inserted and no >= CYN_N_INDEX:
            new_vt_rows.append(new_vt_row)
            inserted = True
        if no >= CYN_N_INDEX:
            row = _set_token(row, 0, no + 1)
        new_vt_rows.append(row)
    lines[vt_start:vt_end] = new_vt_rows
    vt_end += len(new_vt_rows) - old_total

    # -- SETTLING_VELOCITIES block (per box) --------------------------------
    i_set = _find(lines, lambda l: "SETTLING_VELOCITIES" in l, start=vt_end)
    set_start = i_set + 2
    set_end = _consume_data_block(lines, set_start)
    set_groups = _split_groups(lines[set_start:set_end])
    new_set_rows = []
    for g in set_groups:
        new_set_rows.extend(_transform_box_group(g, has_name_comment=True))
    lines[set_start:set_end] = new_set_rows
    shift = len(new_set_rows) - (set_end - set_start)
    set_end += shift

    # -- OPEN BOUNDARIES block (per boundary) -------------------------------
    i_ob = _find(lines, lambda l: "OPEN BOUNDARIES" in l, start=set_end)
    ob_start = i_ob + 2
    ob_end = _consume_data_block(lines, ob_start)
    ob_groups = _split_groups(lines[ob_start:ob_end])
    boundary_ts_nos = {int(_token(g[0], 2)) for g in ob_groups}
    new_ob_rows = []
    for g in ob_groups:
        new_ob_rows.extend(_transform_boundary_group(g))
    lines[ob_start:ob_end] = new_ob_rows
    ob_end += len(new_ob_rows) - (ob_end - ob_start)

    # -- INITIAL CONDITIONS file-name table ---------------------------------
    i_icf = _find(lines, lambda l: "PELAGIC INITIAL CONDITION FILE NAME" in l, start=ob_end)
    icf_start = i_icf + 1
    icf_end = _consume_data_block(lines, icf_start)
    init_conc_names = [_token(l, 1) for l in lines[icf_start:icf_end]]

    # -- FORCING TIME SERIES file-name table ---------------------------------
    i_ftf = _find(lines, lambda l: "FORC TIME SERIE FILE NAME" in l, start=icf_end)
    ftf_start = i_ftf + 1
    ftf_end = _consume_data_block(lines, ftf_start)
    ts_name_by_no = {int(_token(l, 0)): _token(l, 1) for l in lines[ftf_start:ftf_end]}
    missing = sorted(n for n in boundary_ts_nos if n not in ts_name_by_no)
    if missing:
        raise ValueError(
            f"OPEN BOUNDARIES block references forcing-ts no(s) {missing} that are not "
            f"present in the FORCING TIME SERIES file-name table -- cannot resolve which "
            f"FORC_TS file(s) to transform for those boundaries")
    boundary_forc_ts_names = sorted({ts_name_by_no[n] for n in boundary_ts_nos})

    return lines, init_conc_names, boundary_forc_ts_names


def discover_wconst_files(lines):
    """Every distinct PELAGIC MODEL CONSTANTS FILE NAME referenced (usually
    one file shared by every box, but not assumed) -- independent of
    transform_pelagic_inputs()'s section-ordering-sensitive walk, since this
    table's position is never modified by it."""
    i = _find(lines, lambda l: "PELAGIC MODEL CONSTANTS FILE NAME" in l)
    start = i + 1
    end = _consume_data_block(lines, start)
    return sorted({_token(l, 1) for l in lines[start:end]})


# --------------------------------------------------------------------------
# WCONST_*.txt (per-box pelagic model constants)
# --------------------------------------------------------------------------

def zero_wconst_constant(lines, name):
    """Set the named constant's value (token[2]) to 0.0, leaving its comment
    and every other constant untouched. Raises if the name is not found."""
    lines = list(lines)
    for i, l in enumerate(lines):
        toks = _tokens(l)
        if len(toks) >= 3 and toks[1].group() == name:
            lines[i] = _set_token(l, 2, "0.0")
            return lines
    raise ValueError(f"WCONST constant {name!r} not found -- cannot disable it")


# --------------------------------------------------------------------------
# INIT_CONC_*.txt
# --------------------------------------------------------------------------

def _fmt_ic_row(no, value, name):
    return f"{no:>27}{value:>16.6f}     ! {name}"


def transform_init_conc(lines):
    header = lines[:2]
    data = [l for l in lines[2:] if l.strip()]

    cyn_c_row = next(r for r in data if int(_token(r, 0)) == CYN_C_INDEX)
    new_val = float(_token(cyn_c_row, 1)) * Q_SEED
    new_row = _fmt_ic_row(CYN_N_INDEX, new_val, "CYN_N")

    out = []
    inserted = False
    for row in data:
        no = int(_token(row, 0))
        if not inserted and no >= CYN_N_INDEX:
            out.append(new_row)
            inserted = True
        if no >= CYN_N_INDEX:
            row = _set_token(row, 0, no + 1)
        out.append(row)
    if not inserted:
        out.append(new_row)
    return header + out


# --------------------------------------------------------------------------
# FORC_TS_*.txt (boundary concentration series only)
# --------------------------------------------------------------------------

def transform_forc_ts(lines):
    lines = list(lines)

    i_nvars = _find(lines, lambda l: l.strip() == "# NUMBER_OF_VARIABLES")
    old_total = int(lines[i_nvars + 1].strip())
    lines[i_nvars + 1] = _set_token(lines[i_nvars + 1], 0, old_total + 1)

    i_scale = _find(lines, lambda l: l.strip() == "# SCALE FACTORS")
    scale_vals = lines[i_scale + 2].split()
    if len(scale_vals) != old_total:
        raise ValueError(f"SCALE FACTORS row has {len(scale_vals)} values, expected {old_total}")
    new_scale = scale_vals[CYN_C_INDEX - 1]
    scale_vals = scale_vals[:CYN_N_INDEX - 1] + [new_scale] + scale_vals[CYN_N_INDEX - 1:]
    lines[i_scale + 2] = " ".join(scale_vals)

    i_unit = _find(lines, lambda l: l.strip() == "# UNIT CONVERSION FACTORS")
    unit_vals = lines[i_unit + 2].split()
    if len(unit_vals) != old_total:
        raise ValueError(f"UNIT CONVERSION FACTORS row has {len(unit_vals)} values, expected {old_total}")
    new_unit = unit_vals[CYN_C_INDEX - 1]
    unit_vals = unit_vals[:CYN_N_INDEX - 1] + [new_unit] + unit_vals[CYN_N_INDEX - 1:]
    lines[i_unit + 2] = " ".join(unit_vals)

    i_tv = _find(lines, lambda l: l.strip() == "# TIME AND VALUES")
    for j in range(i_tv + 1, len(lines)):
        if not lines[j].strip():
            continue
        fields = lines[j].split()
        if len(fields) != old_total + 1:
            raise ValueError(
                f"line {j + 1}: {len(fields)} fields, expected {old_total + 1} (time + {old_total} vars)")
        new_field = f"{float(fields[CYN_C_INDEX]) * Q_SEED:.6f}"
        fields = fields[:CYN_N_INDEX] + [new_field] + fields[CYN_N_INDEX:]
        lines[j] = " ".join(fields)

    return lines


# --------------------------------------------------------------------------
# PELAGIC_MODEL_OPTIONS.txt
# --------------------------------------------------------------------------

OPTIONS_INSERT = [
    "# CYN_VARIABLE_N (0=legacy Monod CYN N-limitation, default; 1=variable-stoichiometry quota N storage/uptake -- VARN build only, nstate=33)",
    "            1",
    "# CYN_N_QMIN (quota floor, gN/gC)",
    "            0.10",
    "# CYN_N_QMAX (quota ceiling, gN/gC)",
    "            0.25",
    "# CYN_N_VMAX (max N-uptake rate, gN/gC/d)",
    "            0.44",
    "# CYN_N_KHS_UPT (uptake half-saturation, mg N/L)",
    "            0.003",
]

OPTIONS_ANCHOR = "# CYN_ALLELOPATHY_FILE_NAME"


def transform_options(lines):
    idx = _find(lines, lambda l: l.strip() == OPTIONS_ANCHOR)
    return lines[:idx] + list(OPTIONS_INSERT) + lines[idx:]


# --------------------------------------------------------------------------
# any other box x state-var forcing-assignment file (discovered by header
# signature, e.g. SED_FLUX_NO3_SINK.txt)
# --------------------------------------------------------------------------

def transform_boxstate_file(lines):
    header = lines[:2]
    data = [l for l in lines[2:] if l.strip()]
    groups = _split_groups(data)
    new_data = []
    for g in groups:
        new_data.extend(_transform_box_group(g, has_name_comment=False))
    return header + new_data


_OLD_TOTAL = CYN_N_INDEX + N_META - 1  # 36 -- declared state-var count pre-transform


def is_boxstate_file(lines):
    """True iff `lines` is a per-box x per-state-variable forcing-assignment
    file (e.g. SED_FLUX_NO3_SINK.txt): its header matches the box/state-var/
    forcing-ts column signature AND its data rows actually shape-check as
    that table (first data row's state-var no == 1, and every box's row
    count == the declared 36-state-var total).

    The header regex alone is not a safe signature -- it is textually
    identical to the OPEN BOUNDARIES block's column header inside
    PELAGIC_INPUTS.txt (that file is excluded via the caller's `skip` set,
    not via this check, so this function must not silently rely on that).
    A header match that FAILS the shape check is therefore NOT treated as
    "not a boxstate file" (a graceful `return False` here would be exactly
    the silent-mis-transform failure mode this check exists to prevent) --
    it raises instead, so a false-positive header match on some other file
    is caught loudly rather than silently mis-widened or silently skipped.
    """
    if not any(BOXSTATE_HEADER_RE.search(l) for l in lines[:5]):
        return False

    data = [l for l in lines[2:] if l.strip()]
    if not data:
        raise ValueError(
            "file matches the box-state-var header signature but has no data rows "
            "below the two header lines -- refusing to guess its shape")

    first_var_no = int(_token(data[0], 1))
    if first_var_no != 1:
        raise ValueError(
            f"file matches the box-state-var header signature but its first data "
            f"row's state-var no is {first_var_no}, not 1 -- this does not look like "
            f"a box x state-var table, refusing to silently treat it as one")

    groups = _split_groups(data)
    bad_boxes = {_token(g[0], 0): len(g) for g in groups if len(g) != _OLD_TOTAL}
    if bad_boxes:
        raise ValueError(
            f"file matches the box-state-var header signature but has rows-per-box "
            f"counts inconsistent with the declared {_OLD_TOTAL} state variables: "
            f"{bad_boxes} -- refusing to silently treat it as a box x state-var table")

    return True


# --------------------------------------------------------------------------
# --degenerate-cyn: the CYN-only conservation scenario
# --------------------------------------------------------------------------

def zero_init_conc_rows(lines, state_vars):
    """Zero the IC value of every row whose state-var no is in `state_vars`
    (token[1]), leaving every other row and all formatting untouched."""
    header = lines[:2]
    data = [l for l in lines[2:] if l.strip()]
    state_vars = set(state_vars)
    out = []
    for row in data:
        no = int(_token(row, 0))
        if no in state_vars:
            row = _set_token(row, 1, f"{0.0:.6f}")
        out.append(row)
    return header + out


def zero_forc_ts_columns(lines, state_vars):
    """Zero the data column(s) for `state_vars` in every '# TIME AND VALUES'
    row of a FORC_TS_*.txt (fields[0] is time, fields[k] is state var k)."""
    lines = list(lines)
    state_vars = set(state_vars)
    i_tv = _find(lines, lambda l: l.strip() == "# TIME AND VALUES")
    for j in range(i_tv + 1, len(lines)):
        if not lines[j].strip():
            continue
        fields = lines[j].split()
        for sv in state_vars:
            fields[sv] = "0.000000"
        lines[j] = " ".join(fields)
    return lines


def set_nost_staging_off(lines):
    """Set NOST_STAGE_MODEL's value to 0 (legacy akinete gates / staging off)
    in PELAGIC_MODEL_OPTIONS.txt -- this line predates the CYN_VARIABLE_N
    OPTIONS_INSERT block and is untouched by transform_options()."""
    lines = list(lines)
    idx = _find(lines, lambda l: l.strip().startswith("# NOST_STAGE_MODEL"))
    lines[idx + 1] = _set_token(lines[idx + 1], 0, 0)
    return lines


# --------------------------------------------------------------------------
# orchestration
# --------------------------------------------------------------------------

def _read(path):
    return path.read_text().splitlines()


def _write(path, lines):
    path.write_text("\n".join(lines) + "\n")


def generate(src, dst, degenerate_cyn=False):
    src = Path(src)
    dst = Path(dst)
    src_r = src.resolve()
    dst_r = dst.resolve()
    if dst_r == src_r or src_r in dst_r.parents:
        raise ValueError(
            f"refusing to shutil.rmtree(dst) -- dst ({dst} -> {dst_r}) is the same as "
            f"or nested inside src ({src} -> {src_r})")
    if dst.exists():
        shutil.rmtree(dst)
    shutil.copytree(src, dst)

    pel_path = dst / "PELAGIC_INPUTS.txt"
    pel_lines, init_conc_names, forc_ts_names = transform_pelagic_inputs(_read(pel_path))
    _write(pel_path, pel_lines)

    for name in init_conc_names:
        p = dst / name
        _write(p, transform_init_conc(_read(p)))

    for name in forc_ts_names:
        p = dst / name
        _write(p, transform_forc_ts(_read(p)))

    opts_path = dst / "PELAGIC_MODEL_OPTIONS.txt"
    _write(opts_path, transform_options(_read(opts_path)))

    skip = {"PELAGIC_INPUTS.txt", "PELAGIC_MODEL_OPTIONS.txt", *init_conc_names, *forc_ts_names}
    boxstate_names = []
    for p in sorted(dst.glob("*.txt")):
        if p.name in skip:
            continue
        lines = _read(p)
        if is_boxstate_file(lines):
            boxstate_names.append(p.name)
            _write(p, transform_boxstate_file(lines))

    wconst_names = []
    if degenerate_cyn:
        for name in init_conc_names:
            p = dst / name
            _write(p, zero_init_conc_rows(_read(p), DEGENERATE_ZERO_PHYTO_ZOO))

        for name in forc_ts_names:
            p = dst / name
            _write(p, zero_forc_ts_columns(
                _read(p), DEGENERATE_ZERO_PHYTO_ZOO + DEGENERATE_ZERO_BOUNDARY_N))

        _write(opts_path, set_nost_staging_off(_read(opts_path)))

        # discovered from the ORIGINAL (pre-transform) PELAGIC_INPUTS.txt --
        # this table's position/content is untouched by transform_pelagic_inputs()
        wconst_names = discover_wconst_files(_read(pel_path))
        for name in wconst_names:
            p = dst / name
            wlines = _read(p)
            for const_name in DEGENERATE_ZERO_WCONST_NAMES:
                wlines = zero_wconst_constant(wlines, const_name)
            _write(p, wlines)

    return {
        "init_conc": init_conc_names,
        "forc_ts": forc_ts_names,
        "boxstate": boxstate_names,
        "degenerate_cyn": degenerate_cyn,
        "wconst": wconst_names,
    }


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("src_dir", help="source setup folder (36-variable CL29, e.g. INPUTS_CL29/)")
    ap.add_argument("dst_dir", help="destination setup folder to (re)create (e.g. INPUTS_CL29_VARN/)")
    ap.add_argument("--degenerate-cyn", action="store_true",
                     help="additionally zero every other phyto/zoo pool's IC + boundary "
                          "(all except CYN_C/CYN_N), zero NH4/NO3/DON/DET_N boundary inflow, "
                          "and turn NOST staging off -- the CYN-only nitrogen-conservation "
                          "scenario used by 'check_varn_run.py --mode conserve'")
    args = ap.parse_args(argv)
    result = generate(args.src_dir, args.dst_dir, degenerate_cyn=args.degenerate_cyn)
    print(f"PELAGIC_INPUTS.txt: declared total, variable table, settling velocities, "
          f"open boundaries transformed")
    print(f"INIT_CONC files transformed: {result['init_conc']}")
    print(f"FORC_TS files transformed: {result['forc_ts']}")
    print(f"box-state forcing-assignment files transformed (discovered): {result['boxstate']}")
    print("PELAGIC_MODEL_OPTIONS.txt: five CYN_VARIABLE_N/CYN_N_* pairs inserted")
    if args.degenerate_cyn:
        print(f"--degenerate-cyn: zeroed IC+boundary for phyto/zoo state vars "
              f"{DEGENERATE_ZERO_PHYTO_ZOO}, zeroed boundary for N pools "
              f"{DEGENERATE_ZERO_BOUNDARY_N}, NOST_STAGE_MODEL set to 0, "
              f"zeroed {DEGENERATE_ZERO_WCONST_NAMES} in {result['wconst']}")


if __name__ == "__main__":
    main()
