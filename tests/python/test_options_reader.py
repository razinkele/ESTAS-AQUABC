"""READ_PELAGIC_MODEL_OPTIONS: the optional block is positional, may stop early,
and must not accept an option in the wrong place.

These tests run the model binary on a one-day copy of the 0-D setup with the
options file rewritten. They are skipped where ESTAS_II has not been built.
"""
import os
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
BINARY = ROOT / "ESTAS_II"

pytestmark = pytest.mark.skipif(not BINARY.exists(), reason="ESTAS_II not built")

# The optional block in reader order, at values that leave every mechanism off.
OPTIONAL = [
    ("TEMPERATURE_MODEL", "0"),
    ("FEPO4_KSP_LOG10", "-26.4"),
    ("ZOO_FOOD_MODEL", "0"),
    ("KHS_FOOD_TOT_ZOO", "0.15"),
    ("ZOO_CLOSURE_REF", "0.05"),
    ("CYANO_POS_MODEL", "0"),
    ("H_SURF_POS", "0.5"),
    ("W_CRIT_POS_MIN", "3.0"),
    ("K_POS_UP", "3.0"),
    ("K_POS_DISP", "10.0"),
    ("W_DISP_POS", "4.0"),
    ("NOST_STAGE_MODEL", "0"),
    ("T_GERM_AKI_STAGE", "12.0"),
    ("I_FORM_AKI", "120.0"),
    ("KR_GERM_BED", "0.05"),
    ("K_MORT_BED_AKI", "0.001"),
    ("V_SETTLE_AKI", "0.5"),
    ("LIGHT_DAYLENGTH_OPTION", "0"),
    ("NOST_FIX_SWITCH", "0"),
    ("K_FIX_NOST", "0.008"),
    ("R_FIX_NOST", "1.0"),
    ("CYANO_POS_GUILDS", "0"),
    ("PHYTO_CLOSURE_MODEL", "0"),
    ("PHYTO_CLOSURE_REF", "1.0"),
    ("CYN_VARIABLE_N", "0"),
    ("CYN_N_QMIN", "0.10"),
    ("CYN_N_QMAX", "0.25"),
    ("CYN_N_VMAX", "0.44"),
    ("CYN_N_KHS_UPT", "0.003"),
]
TAIL = ["# CYN_ALLELOPATHY_FILE_NAME", "ALLELOPATHIC_INFORMATION.txt"]


def _pairs(options, comment="# {} (test)"):
    out = []
    for name, value in options:
        out += [comment.format(name), "            " + value]
    return out


@pytest.fixture
def setup(tmp_path):
    """A one-day copy of the 0-D setup; returns (run, head) where `head` is the
    mandatory part of the shipped options file and `run(lines)` runs the model
    with the optional block replaced by `lines`."""
    # the inputs are large: link them, and write only the options file
    (tmp_path / "INPUTS").mkdir()
    for f in (ROOT / "INPUTS").iterdir():
        if f.name != "PELAGIC_MODEL_OPTIONS.txt":
            (tmp_path / "INPUTS" / f.name).symlink_to(f)
    (tmp_path / "OUTPUTS").mkdir()
    master = (ROOT / "INPUT.txt").read_text().split("\n")
    i = master.index("# SIMULATION_START")
    start = float(master[i + 1])
    j = master.index("# SIMULATION_END")
    master[j + 1] = f"         {start + 1.0}"
    (tmp_path / "INPUT.txt").write_text("\n".join(master))

    opt = tmp_path / "INPUTS" / "PELAGIC_MODEL_OPTIONS.txt"
    shipped = (ROOT / "INPUTS" / "PELAGIC_MODEL_OPTIONS.txt").read_text().split("\n")
    k = next(n for n, line in enumerate(shipped) if line.startswith("# TEMPERATURE_MODEL"))
    head = shipped[:k]

    def run(lines, newline="\n", trailing=""):
        # the mandatory head keeps its own line endings; the optional block gets `newline`
        opt.write_bytes(("\n".join(head) + "\n" + newline.join(lines) + newline + trailing).encode())
        return subprocess.run([str(BINARY), "INPUT.txt"], cwd=tmp_path, capture_output=True, text=True,
                              timeout=600, env=dict(os.environ))

    return run


def _text(r):
    return r.stdout + r.stderr


def test_short_file_ending_at_the_allelopathy_line_runs(setup):
    r = setup(_pairs(OPTIONAL[:2]) + TAIL)
    assert r.returncode == 0, _text(r)[-2000:]
    assert "Day-length weighting: OFF" in r.stdout


def test_file_ending_without_the_allelopathy_line_runs(setup):
    r = setup(_pairs(OPTIONAL[:1]))
    assert r.returncode == 0, _text(r)[-2000:]


def test_trailing_blank_lines_after_an_early_end_run(setup):
    # the old reader ended the block cleanly here; a blank line names no option
    r = setup(_pairs(OPTIONAL[:3]), trailing="\n   \n\n")
    assert r.returncode == 0, _text(r)[-2000:]


def test_blank_lines_between_options_run(setup):
    lines = _pairs(OPTIONAL[:2]) + [""] + _pairs(OPTIONAL[2:5]) + ["", ""] + TAIL
    r = setup(lines)
    assert r.returncode == 0, _text(r)[-2000:]


def test_windows_line_endings_and_tabs_run(setup):
    r = setup(_pairs(OPTIONAL, comment="#\t{}\t(test)") + TAIL, newline="\r\n")
    assert r.returncode == 0, _text(r)[-2000:]
    assert "CYN variable N (Droop): OFF" in r.stdout


def test_full_block_runs_and_compact_comments_are_accepted(setup):
    r = setup(_pairs(OPTIONAL, comment="#{}") + TAIL)
    assert r.returncode == 0, _text(r)[-2000:]
    assert "CYN variable N (Droop): OFF" in r.stdout


def test_an_option_in_the_wrong_place_stops_the_run(setup):
    # the quota block where seven newer options are read first: the layout that
    # ran with the quota off and day-length weighting on before this check
    lines = _pairs(OPTIONAL[:17]) + _pairs([("CYN_VARIABLE_N", "1")]) + _pairs(OPTIONAL[25:]) + TAIL
    r = setup(lines)
    assert r.returncode != 0
    out = _text(r)
    assert "LIGHT_DAYLENGTH_OPTION" in out and "CYN_VARIABLE_N" in out
    assert "Day-length weighting" not in out  # stopped before the option echo


def test_a_skipped_option_stops_the_run(setup):
    lines = _pairs(OPTIONAL[:2]) + _pairs(OPTIONAL[3:5]) + TAIL  # ZOO_FOOD_MODEL missing
    r = setup(lines)
    assert r.returncode != 0
    assert "ZOO_FOOD_MODEL" in _text(r) and "KHS_FOOD_TOT_ZOO" in _text(r)


def test_a_value_of_the_wrong_type_stops_the_run(setup):
    lines = _pairs(OPTIONAL[:2]) + _pairs([("ZOO_FOOD_MODEL", "0.5x")]) + TAIL
    r = setup(lines)
    assert r.returncode != 0
    assert "ZOO_FOOD_MODEL" in _text(r) and "missing or not of its type" in _text(r)


def test_a_missing_value_line_stops_the_run(setup):
    lines = _pairs(OPTIONAL[:2]) + ["# ZOO_FOOD_MODEL (test)"] + TAIL
    r = setup(lines)
    assert r.returncode != 0
    assert "ZOO_FOOD_MODEL" in _text(r)
