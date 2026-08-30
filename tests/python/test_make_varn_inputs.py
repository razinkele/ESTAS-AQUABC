"""Fixture-based tests for tools/make_varn_inputs.py -- the VARN setup generator.

The fixture setup built by `_build_fixture` below reproduces the REAL header
structures found in the gitignored INPUTS_CL29/ (declared-total line,
variable table, per-box SETTLING_VELOCITIES block, per-boundary OPEN
BOUNDARIES block, INIT_CONC file-name table, FORCING TIME SERIES file-name
table, FORC_TS_*.txt's DATA_SIZE/NUMBER_OF_VARIABLES/SCALE FACTORS/UNIT
CONVERSION FACTORS/TIME AND VALUES header, PELAGIC_MODEL_OPTIONS.txt's
comment+value pair style, and a SED_FLUX_NO3_SINK.txt-style box x state-var
forcing-assignment file) at reduced scale (2 boxes, 2 open boundaries, 2
init-conc sets, DATA_SIZE=2) but with the FULL real 36-variable dimension
intact, since the transformation is position-critical: CYN_C is state
variable 15, CYN_N becomes 33 (aquabc_II_pelagic_svindex.f90::CYN_C_INDEX /
CYN_N_INDEX), and the four allelopathy metabolites shift 33-36 -> 34-37.
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
import make_varn_inputs as mvi  # noqa: E402

STATE_VAR_NAMES = [
    "NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C", "ZOO_C", "ZOO_N", "ZOO_P",
    "DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P", "DISS_ORG_C", "DISS_ORG_N",
    "DISS_ORG_P", "CYN_C", "OPA_C", "DISS_Si", "PART_Si", "FIX_CYN_C", "INORG_C",
    "TOT_ALK", "FE_II", "FE_III", "MN_II", "MN_IV", "CA", "MG", "S_PLUS_6",
    "S_MINUS_2", "CH4_C", "NOST_VEG_HET_C", "AKI_C",
    "SEC_METAB_DIA", "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST",
]
assert len(STATE_VAR_NAMES) == 36
CYN_C_IDX0 = STATE_VAR_NAMES.index("CYN_C")  # 14 (0-based) -> state var 15


def _var_table_block():
    lines = [
        "# ********************* PELAGIC STATE VARIABLES ********************* "
        "Units options: 1 - g/m^3. 2 - g/m^2",
        "#  STATE VARIABLE NO      STATE VARIABLE NAME  NUM_BOXES_WITH_MEASURED_VALS"
        "        MEASUREMENT ERROR    OUTPUT_TYPE    INITIAL_CONDITION_TYPE",
    ]
    for i, name in enumerate(STATE_VAR_NAMES, start=1):
        lines.append(f"{i:>21}{name:>25}{0:>30}{0.001:>17.9f}{1:>15}{1:>26}")
    return lines


def _settling_block(nboxes):
    lines = [
        "# ********************* SETTLING_VELOCITIES *********************",
        "#     PELAGIC BOX NO        STATE VAR NO      DISSOLVED FRAC     "
        "SETTLING VEL NO  DEPOSITED FRACTION   CHLA_SUPRESSION_OF_SETTLING",
    ]
    for box in range(1, nboxes + 1):
        for i, name in enumerate(STATE_VAR_NAMES, start=1):
            dissolved = 1.00 if name in ("NH4_N", "NO3_N", "PO4_P") else 0.00
            vel_no = 7 if name == "CYN_C" else 0  # distinctive: CYN_C's own vel no
            deposited = 0.770000 if name == "CYN_C" else 0.900000
            lines.append(
                f"{box:>20}{i:>20}{dissolved:>20.2f}{vel_no:>20}"
                f"{deposited:>20.6f}{1:>30}     ! BOX {box}: {name}"
            )
    return lines


def _open_boundaries_block(nboundaries):
    lines = [
        "# ********************* OPEN BOUNDARIES *********************",
        "#   OPEN BOUNDARY NO        STATE VAR NO       FORCING TS NO       FORCING TS VAR NO",
    ]
    for b in range(1, nboundaries + 1):
        for i in range(1, 37):
            lines.append(f"{b:>20}{i:>20}{b:>20}{i:>20}")
    return lines


def _sed_flux_file(nboxes):
    lines = [
        "# ************************* PRESCRIBED SEDIMENT FLUXES (fixture) *************************",
        "#         BOX NO        STATE VAR NO       FORCING TS NO   FORCING TS VAR NO",
    ]
    for box in range(1, nboxes + 1):
        for i, name in enumerate(STATE_VAR_NAMES, start=1):
            # CYN_C's row uses a distinctive (ts_no=9, ts_var=1) pair everything else
            # doesn't, so a test can prove the new row MIRRORS it, not the var-no formula.
            ts_no, ts_var = (9, 1) if name == "CYN_C" else (15, 1)
            lines.append(f"{box:>20}{i:>20}{ts_no:>20}{ts_var:>20}")
    return lines


def _init_conc_file(set_no, cyn_c_val, last_val):
    lines = [
        f"# PELAGIC INITIAL CONDITION SET {set_no} (fixture)",
        "#     PELAGIC STATE VAR. NO       PELAGIC CONCENTRATION",
    ]
    for i, name in enumerate(STATE_VAR_NAMES, start=1):
        val = cyn_c_val if name == "CYN_C" else (last_val if name == "SEC_METAB_NOST" else 0.0)
        lines.append(mvi._fmt_ic_row(i, val, name))
    return lines


def _forc_ts_file(boundary_no, cyn_c_vals, last_vals):
    scale = ["1.00000000"] * 36
    scale[CYN_C_IDX0] = "0.500000"  # distinctive, not 1.0 -- proves position not just "any value"
    lines = [
        f"# boundary {boundary_no} concentrations (fixture)",
        "# DATA_SIZE",
        "2",
        "# NUMBER_OF_VARIABLES",
        "36",
        "# SCALE FACTORS",
        "#",
        " ".join(scale),
        "# UNIT CONVERSION FACTORS",
        "#",
        " ".join(scale),
        "# INTERPOLATE (1=yes)",
        "1",
        "# TIME AND VALUES",
    ]
    for t, (cyn_c_val, last_val) in enumerate(zip(cyn_c_vals, last_vals)):
        fields = [str(float(t))] + ["1.000000"] * 36
        fields[CYN_C_IDX0 + 1] = f"{cyn_c_val:.6f}"      # +1: field0 is time
        fields[36] = f"{last_val:.6f}"                    # var36 = SEC_METAB_NOST, last field
        lines.append(" ".join(fields))
    return lines


def _pelagic_inputs_lines():
    header = [
        "# DESCRIPTION Fixture 2-box setup",
        "# NUM_PELAGIC_STATE_VARS",
        "                 36",
        "# NUM_MODEL_CONSTANTS",
        "                  5",
        "# NUM_PELAGIC_BASINS",
        "                  2",
        "# NUM_BATHYMETRIES",
        "                  2",
        "# NUM_PELAGIC_BOXES",
        "                  2",
        "# NUM_PELAGIC_INIT_CONC_SETS",
        "                  2",
        "# NUM_PELAGIC_ADVECTIVE_LINKS",
        "                  0",
        "# NUM_PELAGIC_DISPERSIVE_LINKS",
        "                  0",
        "# NUM_FLOW_TS",
        "                  0",
        "# NUM_MIXING_TS",
        "                  0",
        "# NUM_SETTLING_VELOCITIES",
        "                  0",
        "# NUM_OPEN_BOUNDARIES",
        "                  2",
        "# NUM_MASS_LOADS",
        "                  0",
        "# NUM_MASS_WITHDRAWALS",
        "                  0",
        "# NUM_FORCING_TS",
        "                  2",
        "# PELAGIC_MODEL_OPTIONS",
        "PELAGIC_MODEL_OPTIONS.txt",
        "# PELAGIC OUTPUT INFORMATION FILE",
        "PELAGIC_OUTPUT_INFORMATION_FILE.txt",
        "# PROCESS RATE OUTPUT TYPE, 1 Volume based 2 Area based",
        "1",
    ]
    body = header + _var_table_block() + _settling_block(2) + _open_boundaries_block(2) + [
        "# ********************* MASS LOADS *********************",
        "#   MASS LOAD NO   STATE VAR NO   FORCING TS NO   FORCING TS VAR NO",
        "# ********************* MASS WITHDRAWALS *********************",
        "#   MASS WITHDRAWAL NO   STATE VAR NO   FORCING TS NO   FORCING TS VAR NO",
        "# ********************* INITIAL CONDITIONS *********************",
        "#   PEL. INIT SET NO      PELAGIC INITIAL CONDITION FILE NAME",
        "                   1                         INIT_CONC_1.txt",
        "                   2                         INIT_CONC_2.txt",
        "# ********************* FORCING TIME SERIES *********************",
        "# FORC TIME SERIE NO                                   FORC TIME SERIE FILE NAME",
        "                   1                                     FORC_TS_1.txt",
        "                   2                                     FORC_TS_2.txt",
        "# ECOLOGICAL OUTPUTS",
        "# PRODUCE_ECOL_OUTPUT",
        "1",
    ]
    return body


def _options_lines():
    return [
        "# ZOOPLANKTON OPTION  if 0 unrealistic zooplankton CNP partitioning  ",
        "            1",
        "# NOST_STAGE_MODEL (0=legacy akinete gates, default; 1=bed akinete bank + radiation latch)",
        "            1",
        "# V_SETTLE_AKI (akinete settling velocity, m/d)",
        "            0.5",
        "# CYN_ALLELOPATHY_FILE_NAME",
        "ALLELOPATHIC_INFORMATION.txt",
    ]


@pytest.fixture
def fixture_src(tmp_path):
    src = tmp_path / "INPUTS_MINI"
    src.mkdir()
    (src / "PELAGIC_INPUTS.txt").write_text("\n".join(_pelagic_inputs_lines()) + "\n")
    (src / "PELAGIC_MODEL_OPTIONS.txt").write_text("\n".join(_options_lines()) + "\n")
    (src / "INIT_CONC_1.txt").write_text("\n".join(_init_conc_file(1, 0.680000, 0.111000)) + "\n")
    (src / "INIT_CONC_2.txt").write_text("\n".join(_init_conc_file(2, 0.500000, 0.222000)) + "\n")
    (src / "FORC_TS_1.txt").write_text(
        "\n".join(_forc_ts_file(1, [2.0, 4.0], [9.0, 10.0])) + "\n")
    (src / "FORC_TS_2.txt").write_text(
        "\n".join(_forc_ts_file(2, [3.0, 6.0], [11.0, 12.0])) + "\n")
    (src / "SED_FLUX_NO3_SINK.txt").write_text("\n".join(_sed_flux_file(2)) + "\n")
    (src / "PELAGIC_OUTPUT_INFORMATION_FILE.txt").write_text(
        "#  BOX_NO   STATE_VAR_OUT\n           1           1\n           2           1\n")
    (src / "BATHYMETRY_1.txt").write_text("# untouched fixture file\n1 2 3\n")
    return src


def _lines(path):
    return path.read_text().splitlines()


# --------------------------------------------------------------------------
# PELAGIC_INPUTS.txt
# --------------------------------------------------------------------------

def test_declared_total_37(fixture_src, tmp_path):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    lines = _lines(dst / "PELAGIC_INPUTS.txt")
    i = lines.index("# NUM_PELAGIC_STATE_VARS")
    assert lines[i + 1].strip() == "37"


def test_variable_table_insert_position_and_renumber(fixture_src, tmp_path):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    lines = _lines(dst / "PELAGIC_INPUTS.txt")
    i_vt = next(i for i, l in enumerate(lines) if "PELAGIC STATE VARIABLES" in l)
    rows = lines[i_vt + 2: i_vt + 2 + 37]
    parsed = [(int(mvi._token(r, 0)), mvi._token(r, 1)) for r in rows]
    assert parsed[14] == (15, "CYN_C")            # unchanged, 0-based index 14
    assert parsed[32] == (33, "CYN_N")             # the new row, exactly between 32 and 34
    assert parsed[31] == (32, "AKI_C")             # row before is unchanged
    assert parsed[33] == (34, "SEC_METAB_DIA")     # renumbered 33 -> 34
    assert parsed[34] == (35, "SEC_METAB_NOFIX_CYN")
    assert parsed[35] == (36, "SEC_METAB_FIX_CYN")
    assert parsed[36] == (37, "SEC_METAB_NOST")    # last row: 36 -> 37


def test_settling_velocities_mirror_cyn_c_per_box(fixture_src, tmp_path):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    lines = _lines(dst / "PELAGIC_INPUTS.txt")
    i_set = next(i for i, l in enumerate(lines) if l.startswith("#") and "*" in l and "SETTLING_VELOCITIES" in l)
    rows = lines[i_set + 2: i_set + 2 + 2 * 37]
    for box in (1, 2):
        box_rows = [r for r in rows if int(mvi._token(r, 0)) == box]
        assert len(box_rows) == 37
        cyn_c_row = next(r for r in box_rows if int(mvi._token(r, 1)) == 15)
        cyn_n_row = next(r for r in box_rows if int(mvi._token(r, 1)) == 33)
        # dissolved frac, settling vel no, deposited fraction, chla suppression all mirrored
        assert mvi._token(cyn_n_row, 2) == mvi._token(cyn_c_row, 2)
        assert mvi._token(cyn_n_row, 3) == mvi._token(cyn_c_row, 3) == "7"
        assert mvi._token(cyn_n_row, 4) == mvi._token(cyn_c_row, 4) == "0.770000"
        assert mvi._token(cyn_n_row, 5) == mvi._token(cyn_c_row, 5)
        assert cyn_n_row.rstrip().endswith(f"BOX {box}: CYN_N")
        # position: CYN_N sits between var32 and var34 (old var33)
        idx = box_rows.index(cyn_n_row)
        assert int(mvi._token(box_rows[idx - 1], 1)) == 32
        assert int(mvi._token(box_rows[idx + 1], 1)) == 34


def test_open_boundaries_new_row_and_shift(fixture_src, tmp_path):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    lines = _lines(dst / "PELAGIC_INPUTS.txt")
    i_ob = next(i for i, l in enumerate(lines) if "OPEN BOUNDARIES" in l)
    rows = lines[i_ob + 2: i_ob + 2 + 2 * 37]
    for boundary in (1, 2):
        b_rows = [r for r in rows if int(mvi._token(r, 0)) == boundary]
        assert len(b_rows) == 37
        cyn_n_row = next(r for r in b_rows if int(mvi._token(r, 1)) == 33)
        # forcing ts no stays this boundary's own; forcing ts var no becomes 33 (new column)
        assert int(mvi._token(cyn_n_row, 2)) == boundary
        assert int(mvi._token(cyn_n_row, 3)) == 33
        # old var 33 (SEC_METAB_DIA slot) is now numbered 34, forcing ts var no also 34
        shifted = next(r for r in b_rows if int(mvi._token(r, 1)) == 34)
        assert int(mvi._token(shifted, 3)) == 34
        last = next(r for r in b_rows if int(mvi._token(r, 1)) == 37)
        assert int(mvi._token(last, 3)) == 37


# --------------------------------------------------------------------------
# INIT_CONC_*.txt
# --------------------------------------------------------------------------

@pytest.mark.parametrize("set_no,cyn_c_val,last_val", [(1, 0.680000, 0.111000), (2, 0.500000, 0.222000)])
def test_init_conc_cyn_n_and_last_column(fixture_src, tmp_path, set_no, cyn_c_val, last_val):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    lines = _lines(dst / f"INIT_CONC_{set_no}.txt")
    data = [l for l in lines[2:] if l.strip()]
    assert len(data) == 37
    by_no = {int(mvi._token(r, 0)): r for r in data}
    assert mvi._token(by_no[15], 1) == f"{cyn_c_val:.6f}"          # CYN_C untouched
    assert mvi.Q_SEED == 0.220                                     # the spec's fixed Q_SEED
    assert float(mvi._token(by_no[33], 1)) == round(cyn_c_val * mvi.Q_SEED, 6)
    assert by_no[33].rstrip().endswith("CYN_N")
    assert int(mvi._token(by_no[34], 0)) == 34 and "SEC_METAB_DIA" in by_no[34]
    assert float(mvi._token(by_no[37], 1)) == round(last_val, 6)   # last column preserved
    assert "SEC_METAB_NOST" in by_no[37]


# --------------------------------------------------------------------------
# FORC_TS_*.txt
# --------------------------------------------------------------------------

@pytest.mark.parametrize(
    "boundary_no,cyn_c_vals,last_vals",
    [(1, [2.0, 4.0], [9.0, 10.0]), (2, [3.0, 6.0], [11.0, 12.0])],
)
def test_forc_ts_header_and_scale_factors(fixture_src, tmp_path, boundary_no, cyn_c_vals, last_vals):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    lines = _lines(dst / f"FORC_TS_{boundary_no}.txt")
    i_nv = next(i for i, l in enumerate(lines) if l.strip() == "# NUMBER_OF_VARIABLES")
    assert lines[i_nv + 1].strip() == "37"

    i_scale = next(i for i, l in enumerate(lines) if l.strip() == "# SCALE FACTORS")
    scale = lines[i_scale + 2].split()
    assert len(scale) == 37
    assert scale[14] == "0.500000"     # CYN_C's own scale factor, position 15 -> index 14
    assert scale[32] == "0.500000"     # CYN_N's new scale factor mirrors CYN_C's exactly
    assert scale[36] == "1.00000000"   # old var36 (last), shifted to index 36, unchanged value

    i_unit = next(i for i, l in enumerate(lines) if l.strip() == "# UNIT CONVERSION FACTORS")
    unit = lines[i_unit + 2].split()
    assert unit[32] == "0.500000"


@pytest.mark.parametrize(
    "boundary_no,cyn_c_vals,last_vals",
    [(1, [2.0, 4.0], [9.0, 10.0]), (2, [3.0, 6.0], [11.0, 12.0])],
)
def test_forc_ts_data_rows_column_33_and_last_column(fixture_src, tmp_path, boundary_no, cyn_c_vals, last_vals):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    lines = _lines(dst / f"FORC_TS_{boundary_no}.txt")
    i_tv = next(i for i, l in enumerate(lines) if l.strip() == "# TIME AND VALUES")
    data = [l for l in lines[i_tv + 1:] if l.strip()]
    assert len(data) == 2
    for row, cyn_c_val, last_val in zip(data, cyn_c_vals, last_vals):
        fields = row.split()
        assert len(fields) == 38                       # time + 37 vars
        assert float(fields[15]) == cyn_c_val            # CYN_C (field index = var no) untouched
        # the inserted CYN_N column, exactly 0.220 * CYN_C's column, computed the same way
        assert float(fields[33]) == round(cyn_c_val * mvi.Q_SEED, 6)
        assert float(fields[37]) == last_val             # last column == old last column, unmoved value


def test_forc_ts9_style_file_not_referenced_stays_untouched(fixture_src, tmp_path):
    # FORC_TS_9 isn't part of this fixture (only 2 boundaries -> FORC_TS_1/2), but prove
    # the discovery mechanism only transforms files the OPEN BOUNDARIES block references,
    # not every FORC_TS_*.txt by name, by adding an unreferenced one and checking it's
    # copied byte-identical.
    extra = fixture_src / "FORC_TS_9.txt"
    extra.write_text(
        "# day fraction\n# DATA_SIZE\n1\n# NUMBER_OF_VARIABLES\n2\n"
        "# SCALE FACTORS\n#\n1.0 1.0\n# UNIT CONVERSION FACTORS\n#\n1.0 1.0\n"
        "# INTERPOLATE (1=yes)\n1\n# TIME AND VALUES\n0.0 0.5 0.5\n"
    )
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    assert (dst / "FORC_TS_9.txt").read_text() == extra.read_text()


# --------------------------------------------------------------------------
# PELAGIC_MODEL_OPTIONS.txt -- position, not just presence
# --------------------------------------------------------------------------

def test_options_five_pairs_inserted_at_exact_position(fixture_src, tmp_path):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    lines = _lines(dst / "PELAGIC_MODEL_OPTIONS.txt")

    i_vsettle_val = next(i for i, l in enumerate(lines) if l.strip() == "0.5")
    i_anchor = next(i for i, l in enumerate(lines) if l.strip() == "# CYN_ALLELOPATHY_FILE_NAME")

    # exactly the 10 inserted lines sit between V_SETTLE_AKI's value and the anchor --
    # not appended at EOF, not scattered elsewhere.
    between = lines[i_vsettle_val + 1: i_anchor]
    assert between == mvi.OPTIONS_INSERT
    assert i_anchor == i_vsettle_val + 1 + len(mvi.OPTIONS_INSERT)

    # order and values, read as comment/value pairs the same way the Fortran reader does
    assert between[0].startswith("# CYN_VARIABLE_N")
    assert between[1].strip() == "1"
    assert between[2].startswith("# CYN_N_QMIN")
    assert between[3].strip() == "0.10"
    assert between[4].startswith("# CYN_N_QMAX")
    assert between[5].strip() == "0.25"
    assert between[6].startswith("# CYN_N_VMAX")
    assert between[7].strip() == "0.44"
    assert between[8].startswith("# CYN_N_KHS_UPT")
    assert between[9].strip() == "0.003"

    # everything after the anchor (the allelopathy filename line) is unchanged
    orig = _lines(fixture_src / "PELAGIC_MODEL_OPTIONS.txt")
    i_anchor_orig = next(i for i, l in enumerate(orig) if l.strip() == "# CYN_ALLELOPATHY_FILE_NAME")
    assert lines[i_anchor:] == orig[i_anchor_orig:]


# --------------------------------------------------------------------------
# discovered box x state-var forcing-assignment file (SED_FLUX_NO3_SINK.txt-style)
# --------------------------------------------------------------------------

def test_sed_flux_style_file_discovered_and_mirrors_cyn_c_forcing(fixture_src, tmp_path):
    dst = tmp_path / "out"
    result = mvi.generate(fixture_src, dst)
    assert "SED_FLUX_NO3_SINK.txt" in result["boxstate"]

    lines = _lines(dst / "SED_FLUX_NO3_SINK.txt")
    data = [l for l in lines[2:] if l.strip()]
    assert len(data) == 2 * 37
    for box in (1, 2):
        box_rows = [r for r in data if int(mvi._token(r, 0)) == box]
        assert len(box_rows) == 37
        cyn_n_row = next(r for r in box_rows if int(mvi._token(r, 1)) == 33)
        # mirrors CYN_C's (ts_no, ts_var) = (9, 1), NOT the open-boundaries "= var no" rule
        assert mvi._token(cyn_n_row, 2) == "9"
        assert mvi._token(cyn_n_row, 3) == "1"
        # old var33 (now 34) keeps ITS OWN forcing columns (15, 1), unshifted
        shifted = next(r for r in box_rows if int(mvi._token(r, 1)) == 34)
        assert mvi._token(shifted, 2) == "15"
        assert mvi._token(shifted, 3) == "1"


# --------------------------------------------------------------------------
# discipline: untouched files copied verbatim, no stray extra files
# --------------------------------------------------------------------------

def test_untouched_file_copied_verbatim(fixture_src, tmp_path):
    dst = tmp_path / "out"
    mvi.generate(fixture_src, dst)
    assert (dst / "BATHYMETRY_1.txt").read_text() == (fixture_src / "BATHYMETRY_1.txt").read_text()
    assert (dst / "PELAGIC_OUTPUT_INFORMATION_FILE.txt").read_text() == \
        (fixture_src / "PELAGIC_OUTPUT_INFORMATION_FILE.txt").read_text()


def test_generate_returns_discovered_filenames(fixture_src, tmp_path):
    dst = tmp_path / "out"
    result = mvi.generate(fixture_src, dst)
    assert sorted(result["init_conc"]) == ["INIT_CONC_1.txt", "INIT_CONC_2.txt"]
    assert sorted(result["forc_ts"]) == ["FORC_TS_1.txt", "FORC_TS_2.txt"]
