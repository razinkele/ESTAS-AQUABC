"""Tests for tools/check_varn_run.py.

Fixtures for the log-parsing functions (parse_options_echo, parse_transport_echo)
are REAL text captured from an actual `ESTAS_II_varN` run on `INPUTS_CL29_VARN`
(2026-08-30, Task 6 investigation -- see task-6-report.md), copied verbatim, not
constructed via this module's own regexes or formatters -- this caught a real bug
during development (the OFF-case echo text is 'OFF (legacy Monod CYN
N-limitation, default).', not 'OFF.' as first assumed from the ON-case pattern
alone; a hand-typed fixture built from that assumption would not have caught it).

The live degenerate-CYN conservation run and its measured residual are evidence
in task-6-report.md, not reproduced here as a unit test (stdlib+numpy only, no
model binary in CI) -- these tests cover the parsers and the pass/fail logic on
small synthetic MASS_BALANCES/PROCESS_RATES fixtures instead.
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))

import check_varn_run as chk  # noqa: E402


# ---------------------------------------------------------------------------
# real captured log fixtures (verbatim, from /tmp/varn_smoke/run.log,
# 2026-08-30, ESTAS_II_varN on INPUTS_CL29_VARN, 5-day run)
# ---------------------------------------------------------------------------

REAL_OPTIONS_ECHO_ON_LINE = (
    " CYN variable N (Droop): ON. CYN_N_QMIN=  0.10000000000000001       "
    "CYN_N_QMAX=  0.25000000000000000       CYN_N_VMAX=  0.44000000000000000  "
    "     CYN_N_KHS_UPT=   3.0000000000000001E-003"
)
# captured separately from a standard (non-VARN options) run, /tmp/varn_ab/run_std90.log
REAL_OPTIONS_ECHO_OFF_LINE = (
    " CYN variable N (Droop): OFF (legacy Monod CYN N-limitation, default)."
)
REAL_TRANSPORT_ANCHOR_LINE = " Process rates will be written in g/m^3/day"
REAL_TRANSPORT_ECHO_LINE = (
    "           1           1           1           1           1           1"
    "           1           1           1           1           1           1"
    "           1           1           1           1           1           1"
    "           1           1           1           1           1           1"
    "           1           1           1           1           1           1"
    "           1           0           1           1           1           1"
    "           1"
)
REAL_OPTIONS_FILE_TEXT = """\
# ZOOPLANKTON OPTION  if 0 unrealistic zooplankton CNP partitioning
            1
# NOST_STAGE_MODEL (0=legacy akinete gates, default; 1=bed akinete bank + radiation latch)
            1
# CYN_VARIABLE_N (0=legacy Monod CYN N-limitation, default; 1=variable-stoichiometry quota N storage/uptake -- VARN build only, nstate=33)
            1
# CYN_N_QMIN (quota floor, gN/gC)
            0.10
# CYN_N_QMAX (quota ceiling, gN/gC)
            0.25
# CYN_N_VMAX (max N-uptake rate, gN/gC/d)
            0.44
# CYN_N_KHS_UPT (uptake half-saturation, mg N/L)
            0.003
# CYN_ALLELOPATHY_FILE_NAME
ALLELOPATHIC_INFORMATION.txt
"""
# real MASS_BALANCES.out header + box-1 rows at t=1.0, from the fixed
# --degenerate-cyn run (denitrification + gas exchange disabled)
REAL_MB_HEADER = (
    "      TIME    BOX NO    VAR_NO        ADVECTION (g/m^3/days)        "
    "DIFFUSION (g/m^3/days)         SETTLING (g/m^3/days)       MASS LOADS "
    "(g/m^3/days) MASS WITHDRAWALS (g/m^3/days)         KINETICS (g/m^3/days)"
    "  SEDIMENT FLUXES (g/m^3/days)"
)
REAL_MB_ROWS = [
    "    1.0000         1         1                     -0.000745                      0.000000                     -0.000000                      0.000000                      0.000000                      0.062900                      0.000000",
    "    1.0000         1         2                     -0.029909                      0.000000                     -0.000000                      0.000000                      0.000000                      0.053585                      0.000000",
    "    1.0000         1        10                     -0.042210                      0.000000                     -0.008264                      0.000000                      0.000000                     -0.011584                      0.000000",
    "    1.0000         1        13                     -0.136041                      0.000000                     -0.000000                      0.000000                      0.000000                     -0.100014                      0.000000",
    "    1.0000         1        33                     -0.005026                      0.000000                     -0.000000                      0.000000                      0.000000                     -0.004887                      0.000000",
]


# ---------------------------------------------------------------------------
# parse_options_echo
# ---------------------------------------------------------------------------

def test_parse_options_echo_on_real_text():
    echo = chk.parse_options_echo(["...", REAL_OPTIONS_ECHO_ON_LINE, "..."])
    assert echo["CYN_VARIABLE_N"] == "ON"
    assert echo["CYN_N_QMIN"] == pytest.approx(0.10)
    assert echo["CYN_N_QMAX"] == pytest.approx(0.25)
    assert echo["CYN_N_VMAX"] == pytest.approx(0.44)
    assert echo["CYN_N_KHS_UPT"] == pytest.approx(0.003)


def test_parse_options_echo_off_real_text():
    # This is the case a hand-typed fixture ("OFF.") would have missed --
    # the real Fortran text is "OFF (legacy Monod ...)."
    echo = chk.parse_options_echo(["...", REAL_OPTIONS_ECHO_OFF_LINE, "..."])
    assert echo["CYN_VARIABLE_N"] == "OFF"
    assert "CYN_N_QMIN" not in echo


def test_parse_options_echo_missing_raises():
    with pytest.raises(ValueError, match="not found"):
        chk.parse_options_echo(["some", "unrelated", "log", "lines"])


# ---------------------------------------------------------------------------
# parse_transport_echo
# ---------------------------------------------------------------------------

def test_parse_transport_echo_real_single_line():
    log = ["...", REAL_TRANSPORT_ANCHOR_LINE, REAL_TRANSPORT_ECHO_LINE, "..."]
    flags = chk.parse_transport_echo(log)
    assert len(flags) == 37
    assert flags[chk.CYN_N_INDEX - 1] == 1              # slot 33 (CYN_N)
    assert flags[chk.CYN_N_INDEX:chk.CYN_N_INDEX + 4] == [1, 1, 1, 1]  # slots 34:37
    assert flags[31] == 0                                # slot 32 (AKI_C) -- not transported


def test_parse_transport_echo_wrapped_across_lines():
    # gfortran CAN wrap a long list-directed write across multiple lines
    # (the real capture above happened not to, for this array length) --
    # synthetic, not a real capture, exercising the join-until-total logic.
    toks = ["1"] * 31 + ["0"] + ["1"] * 5
    assert len(toks) == 37
    wrapped = [" ".join(toks[0:15]), " ".join(toks[15:30]), " ".join(toks[30:37])]
    log = [REAL_TRANSPORT_ANCHOR_LINE, *wrapped, "RESUSPENSION_OPTION :            0"]
    flags = chk.parse_transport_echo(log)
    assert flags == [int(t) for t in toks]


def test_parse_transport_echo_anchor_missing_raises():
    with pytest.raises(ValueError, match="anchor"):
        chk.parse_transport_echo(["no anchor here"])


def test_parse_transport_echo_wrong_count_raises():
    log = [REAL_TRANSPORT_ANCHOR_LINE, "1 1 1", "RESUSPENSION_OPTION :   0"]
    with pytest.raises(ValueError, match="expected 37"):
        chk.parse_transport_echo(log)


# ---------------------------------------------------------------------------
# read_options_file
# ---------------------------------------------------------------------------

def test_read_options_file_real_text(tmp_path):
    p = tmp_path / "PELAGIC_MODEL_OPTIONS.txt"
    p.write_text(REAL_OPTIONS_FILE_TEXT)
    opts = chk.read_options_file(str(p))
    assert opts == {
        "CYN_VARIABLE_N": "1", "CYN_N_QMIN": "0.10", "CYN_N_QMAX": "0.25",
        "CYN_N_VMAX": "0.44", "CYN_N_KHS_UPT": "0.003",
    }


def test_read_options_file_missing_key_raises(tmp_path):
    p = tmp_path / "opts.txt"
    p.write_text("# CYN_VARIABLE_N\n1\n")
    with pytest.raises(ValueError, match="CYN_N_QMIN"):
        chk.read_options_file(str(p))


# ---------------------------------------------------------------------------
# box_number / read_box_out
# ---------------------------------------------------------------------------

def test_box_number():
    assert chk.box_number("out/PELAGIC_BOX_00007.out") == 7
    assert chk.box_number("out/PELAGIC_BOX_00012_PROCESS_RATES.out") == 12
    assert chk.box_number("not_a_box.txt") is None


def test_box_out_paths_excludes_process_rates(tmp_path):
    (tmp_path / "PELAGIC_BOX_00001.out").write_text("TIME_DAYS X\n0.0 1.0\n")
    (tmp_path / "PELAGIC_BOX_00001_PROCESS_RATES.out").write_text("0.0 1.0\n")
    (tmp_path / "PELAGIC_BOX_00002.out").write_text("TIME_DAYS X\n0.0 1.0\n")
    paths = chk.box_out_paths(str(tmp_path))
    assert len(paths) == 2
    assert all(not p.endswith("_PROCESS_RATES.out") for p in paths)


def test_read_box_out(tmp_path):
    p = tmp_path / "PELAGIC_BOX_00001.out"
    p.write_text("TIME_DAYS CYN_C CYN_N\n0.0 0.68 0.1496\n1.0 0.70 0.1550\n")
    header, rows = chk.read_box_out(str(p))
    assert header == ["TIME_DAYS", "CYN_C", "CYN_N"]
    assert rows == [[0.0, 0.68, 0.1496], [1.0, 0.70, 0.1550]]


# ---------------------------------------------------------------------------
# smoke mode (end-to-end on small fixtures)
# ---------------------------------------------------------------------------

def _write_smoke_fixture(tmp_path, cyn_c_n_rows, high_flush_box=None):
    """cyn_c_n_rows: {box: [(t, cyn_c, cyn_n), ...]}."""
    outputs = tmp_path / "out"
    outputs.mkdir()
    for box, rows in cyn_c_n_rows.items():
        p = outputs / f"PELAGIC_BOX_{box:05d}.out"
        lines = ["TIME_DAYS CYN_C CYN_N"]
        lines += [f"{t:.6f} {c:.6f} {n:.6f}" for t, c, n in rows]
        p.write_text("\n".join(lines) + "\n")
    options = tmp_path / "PELAGIC_MODEL_OPTIONS.txt"
    options.write_text(REAL_OPTIONS_FILE_TEXT)
    log = tmp_path / "run.log"
    log.write_text("\n".join([
        REAL_OPTIONS_ECHO_ON_LINE, REAL_TRANSPORT_ANCHOR_LINE, REAL_TRANSPORT_ECHO_LINE,
    ]) + "\n")
    return str(log), str(outputs), str(options)


def test_run_smoke_passes_on_in_bounds_quota(tmp_path):
    log, outputs, options = _write_smoke_fixture(
        tmp_path, {1: [(0.0, 0.68, 0.68 * 0.20), (1.0, 0.70, 0.70 * 0.18)]})
    a = chk.argparse.Namespace(log=log, outputs=outputs, options=options, high_flush_boxes=[])
    assert chk.run_smoke(a) == 0


def test_run_smoke_fails_on_out_of_bounds_quota(tmp_path):
    # Q = 0.30 / 0.68 = 0.44, well outside [0.095, 0.255], CYN_C well above the floor
    log, outputs, options = _write_smoke_fixture(tmp_path, {1: [(0.0, 0.68, 0.30)]})
    a = chk.argparse.Namespace(log=log, outputs=outputs, options=options, high_flush_boxes=[])
    assert chk.run_smoke(a) == 1


def test_run_smoke_excludes_floor_artifact_samples(tmp_path):
    # CYN_C at 1x MIN_CONCENTRATION -> excluded, even though Q is nonsense (division near 0)
    floor = chk.MIN_CONCENTRATION
    log, outputs, options = _write_smoke_fixture(
        tmp_path, {1: [(0.0, floor, floor * 999.0)]})  # Q would be ~999, wildly out of bounds
    a = chk.argparse.Namespace(log=log, outputs=outputs, options=options, high_flush_boxes=[])
    assert chk.run_smoke(a) == 0  # excluded, not asserted -> still PASS


def test_run_smoke_high_flush_box_reported_not_asserted(tmp_path):
    # box 1 has an out-of-bounds sample but is declared high-flush -> reported only
    log, outputs, options = _write_smoke_fixture(tmp_path, {1: [(0.0, 0.68, 0.30)]})
    a = chk.argparse.Namespace(log=log, outputs=outputs, options=options, high_flush_boxes=[1])
    assert chk.run_smoke(a) == 0


def test_run_smoke_fails_when_options_echo_mismatches_file(tmp_path):
    log, outputs, options = _write_smoke_fixture(
        tmp_path, {1: [(0.0, 0.68, 0.68 * 0.20)]})
    # corrupt the options file so CYN_N_QMIN disagrees with the echoed 0.10
    bad_options = tmp_path / "PELAGIC_MODEL_OPTIONS.txt"
    bad_options.write_text(REAL_OPTIONS_FILE_TEXT.replace("0.10", "0.20"))
    a = chk.argparse.Namespace(log=log, outputs=outputs, options=str(bad_options), high_flush_boxes=[])
    assert chk.run_smoke(a) == 1


# ---------------------------------------------------------------------------
# parse_mass_balances (real fixture)
# ---------------------------------------------------------------------------

def test_parse_mass_balances_real_rows(tmp_path):
    p = tmp_path / "MASS_BALANCES.out"
    p.write_text(REAL_MB_HEADER + "\n" + "\n".join(REAL_MB_ROWS) + "\n")
    data = chk.parse_mass_balances(str(p), set(chk.N_POOL_VARS))
    key = (1, 1.0)
    assert key in data
    assert set(data[key]) == {1, 2, 10, 13, 33}
    assert data[key][33][5] == pytest.approx(-0.004887)  # KINETICS is index 5 of the 7-tuple
    assert data[key][1][0] == pytest.approx(-0.000745)   # ADVECTION is index 0


def test_parse_mass_balances_rejects_non_mass_balances_file(tmp_path):
    p = tmp_path / "not_mb.out"
    p.write_text("TIME_DAYS NH4_N\n0.0 0.1\n")
    with pytest.raises(ValueError, match="KINETICS"):
        chk.parse_mass_balances(str(p), {1})


# ---------------------------------------------------------------------------
# conserve mode
# ---------------------------------------------------------------------------

def _write_mb(tmp_path, samples):
    """samples: {(box,t): {var: kinetics_value}} -> MASS_BALANCES.out fixture
    (only the KINETICS column varies; the other six are 0.0)."""
    p = tmp_path / "MASS_BALANCES.out"
    lines = [REAL_MB_HEADER]
    for (box, t), per_var in samples.items():
        for var, kin in per_var.items():
            lines.append(f"{t:10.4f}{box:10d}{var:10d}"
                         f"{0.0:>30.6f}{0.0:>30.6f}{0.0:>30.6f}{0.0:>30.6f}{0.0:>30.6f}"
                         f"{kin:>30.6f}{0.0:>30.6f}")
    p.write_text("\n".join(lines) + "\n")
    return str(p)


def test_run_conserve_passes_when_kinetics_cancel(tmp_path):
    # NH4 loss exactly balanced by CYN_N gain (a clean uptake event), others 0
    mb = _write_mb(tmp_path, {(1, 1.0): {1: -0.05, 2: 0.0, 10: 0.0, 13: 0.0, 33: 0.05}})
    a = chk.argparse.Namespace(mass_balances=mb, rel_tol=1e-6)
    assert chk.run_conserve(a) == 0


def test_run_conserve_fails_when_kinetics_leak(tmp_path):
    # a systematic, uncancelled NO3 loss (simulating denitrification left ON)
    mb = _write_mb(tmp_path, {(1, 1.0): {1: 0.0, 2: -0.05, 10: 0.0, 13: 0.0, 33: 0.0}})
    a = chk.argparse.Namespace(mass_balances=mb, rel_tol=1e-4)
    assert chk.run_conserve(a) == 1


def test_run_conserve_ignores_incomplete_samples(tmp_path):
    # a (box,time) sample missing one of the five pools must not crash or count
    mb = _write_mb(tmp_path, {
        (1, 1.0): {1: -0.05, 2: 0.0, 10: 0.0, 13: 0.0, 33: 0.05},  # complete, cancels
        (2, 1.0): {1: -0.05, 2: 0.0},                              # incomplete -- skipped
    })
    a = chk.argparse.Namespace(mass_balances=mb, rel_tol=1e-6)
    assert chk.run_conserve(a) == 0


def test_run_conserve_raises_on_no_matching_vars(tmp_path):
    p = tmp_path / "MASS_BALANCES.out"
    p.write_text(REAL_MB_HEADER + "\n" + f"{1.0:10.4f}{1:10d}{99:10d}" + "0.0" * 7 + "\n")
    a = chk.argparse.Namespace(mass_balances=str(p), rel_tol=1e-4)
    with pytest.raises(ValueError, match="no rows"):
        chk.run_conserve(a)


# ---------------------------------------------------------------------------
# nbudget mode / cyn_n_process_terms
# ---------------------------------------------------------------------------

def test_cyn_n_process_terms_extracts_the_right_block():
    """Position-only fixture: place 5 distinct marker values at the exact
    flat index the source formula gives (mod_SOLVER.f90:1739-1744,
    PROCESS_RATE_BEGIN_NO=((j-1)*NDIAGVAR)+1 for j=CYN_N_INDEX=33,
    NDIAGVAR=30 -> 1-indexed 961..965, i.e. 0-indexed 960..964 in the
    post-time field list) and assert the function reads exactly those,
    in order -- markers chosen unlike any default/zero value so a passing
    assert can't be an accident."""
    total = chk.TOTAL_TRANSPORT_SLOTS * chk.NDIAGVAR  # 37*30 = 1110
    fields = ["0.0"] * (total + 1)  # +1 for the leading TIME field
    fields[0] = "5.0"  # time
    markers = ["1.111111", "2.222222", "3.333333", "4.444444", "5.555555"]
    begin = (chk.CYN_N_INDEX - 1) * chk.NDIAGVAR + 1  # 961
    for k, v in enumerate(markers):
        fields[begin + k] = v
    terms = chk.cyn_n_process_terms(fields)
    assert terms == pytest.approx([1.111111, 2.222222, 3.333333, 4.444444, 5.555555])


def _write_nbudget_fixture(tmp_path, box, with_process_rates):
    outputs = tmp_path / "out"
    outputs.mkdir()
    p = outputs / f"PELAGIC_BOX_{box:05d}.out"
    p.write_text("TIME_DAYS CYN_C CYN_N\n0.0 0.680000 0.149600\n1.0 0.700000 0.158184\n")
    if with_process_rates:
        pr = outputs / f"PELAGIC_BOX_{box:05d}_PROCESS_RATES.out"
        total = chk.TOTAL_TRANSPORT_SLOTS * chk.NDIAGVAR
        row0 = ["0.0"] * (total + 1)
        row0[0] = "0.0"
        row1 = list(row0)
        row1[0] = "1.0"
        begin = (chk.CYN_N_INDEX - 1) * chk.NDIAGVAR + 1
        for k, v in enumerate(["0.002987", "0.004916", "0.002956", "0.0", "0.0"]):
            row1[begin + k] = v
        pr.write_text(" ".join(row0) + "\n" + " ".join(row1) + "\n")
    return str(outputs)


def test_run_nbudget_uses_process_rates_when_available(tmp_path, capsys):
    outputs = _write_nbudget_fixture(tmp_path, box=1, with_process_rates=True)
    a = chk.argparse.Namespace(outputs=outputs, box=1)
    assert chk.run_nbudget(a) == 0
    out = capsys.readouterr().out
    assert "PROCESS_RATES output" in out
    assert "0.002987" in out  # uptake term made it into the printed table


def test_run_nbudget_falls_back_to_state_difference(tmp_path, capsys):
    outputs = _write_nbudget_fixture(tmp_path, box=1, with_process_rates=False)
    a = chk.argparse.Namespace(outputs=outputs, box=1)
    assert chk.run_nbudget(a) == 0
    out = capsys.readouterr().out
    assert "state-difference NET estimate" in out
    assert "NOT a kinetics-only decomposition" in out


def test_run_nbudget_prints_the_tn_limitation_note(tmp_path, capsys):
    outputs = _write_nbudget_fixture(tmp_path, box=1, with_process_rates=False)
    a = chk.argparse.Namespace(outputs=outputs, box=1)
    chk.run_nbudget(a)
    out = capsys.readouterr().out
    assert "mod_PELAGIC_ECOLOGY.f90:369" in out
    assert "validate_cl29_vs_epa.py" in out
