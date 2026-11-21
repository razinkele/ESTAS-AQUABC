"""The validator must not add a per-area (g/m2) output to concentrations.

ESTAS writes a state variable with OUTPUT_TYPE 2 as concentration x the box's VOLUME/AREA
(sub_WRITE_PELAGIC_OUTPUT). In CL29 that is AKI_C, the water-column akinete pool, which TN and TP
include. Adding it as written put g/m2 into mg/L totals: on the adopted CL29 run pooled TP RMSE was
0.0424 instead of 0.032 (CL29 record §74). load_box_output now divides such columns by VOLUME/AREA
from the run's WATER_LEVELS.out; when the run's output types or depths cannot be found, TN/TP are
not scored.
"""
import csv
import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))

import validate_cl29_vs_epa as val  # noqa: E402

MASTER = """# DESCRIPTION test
# BASE_YEAR
           2012
# PELAGIC MODEL INPUT FOLDER write the folder always with / in the end
INPUTS_X/
# PELAGIC MODEL INPUT FILE
            PELAGIC_INPUTS.txt
# PELAGIC MODEL OUTPUT FOLDER write the folder always with / in the end
OUTPUTS_X/
# RESUSPENSION_OPTION
          0
"""
PELAGIC_INPUTS = """# ********************* PELAGIC STATE VARIABLES *********************  Units options: 1 - g/m^3. 2 - g/m^2
#  STATE VARIABLE NO      STATE VARIABLE NAME  NUM_BOXES_WITH_MEASURED_VALS        MEASUREMENT ERROR    OUTPUT_TYPE    INITIAL_CONDITION_TYPE
                   1                    NH4_N                             0              0.001000000              1                         1
                   2                    NO3_N                             0              0.001000000              1                         1
                  32                    AKI_C                             0              0.001000000              {aki}                         2
# ********************* PELAGIC INITIAL CONDITIONS *********************
                   1                  32                0.00                   0            0.900000                             1     ! BOX 1: AKI_C
"""
OBS_HEADER = ("station,box,region,date,variable,model_index,value,units,"
              "source_sheet,orig_column,orig_unit,orig_basis\n")
AKI_CONC = 0.5          # g C/m3 of akinetes in the water
OTHER_N = 1.0           # NH4_N + NO3_N, g N/m3


@pytest.fixture(autouse=True)
def _reset(monkeypatch):
    monkeypatch.setattr(val, "_ACTIVE_C_TO_CHLA", None)
    monkeypatch.setattr(val, "_ACTIVE_PELAGIC_INPUTS", None)
    monkeypatch.setattr(val, "_AREA_BASIS_CACHE", {})


def _run(root, depth, aki_type=2, master=True, water_levels=True):
    """One box (7) whose AKI_C is written as ESTAS writes it for this output type and depth."""
    out = root / "OUTPUTS_X"
    out.mkdir(parents=True)
    written = AKI_CONC * depth if aki_type == 2 else AKI_CONC
    (out / "PELAGIC_BOX_00007.out").write_text(
        f"TIME_DAYS NH4_N NO3_N AKI_C\n0.0 0.4 0.6 {written}\n400.0 0.4 0.6 {written}\n")
    if water_levels:
        (out / "WATER_LEVELS.out").write_text(
            "      TIME    BOX NO   SURFACE ELEVATION (m)   VOLUME (m^3)   DEPTH (m)   VOLUME / AREA (m)\n"
            f"    1.0000,         7,      0.0000,   1000000.0000,   {depth:.4f},   {depth:.4f}\n"
            f"  400.0000,         7,      0.0000,   1000000.0000,   {depth:.4f},   {depth:.4f}\n")
    inp = root / "INPUTS_X"
    inp.mkdir()
    (inp / "PELAGIC_INPUTS.txt").write_text(PELAGIC_INPUTS.format(aki=aki_type))
    if master:
        (root / "INPUT_X.txt").write_text(MASTER)
    return out


def test_state_output_types_reads_the_state_variable_block(tmp_path):
    (tmp_path / "P.txt").write_text(PELAGIC_INPUTS.format(aki=2))
    assert val.state_output_types(str(tmp_path / "P.txt")) == {"NH4_N": 1, "NO3_N": 1, "AKI_C": 2}


def test_finds_the_runs_own_pelagic_inputs(tmp_path):
    out = _run(tmp_path, 2.0)
    found = val.find_run_pelagic_inputs(str(out))
    assert os.path.realpath(found) == os.path.realpath(tmp_path / "INPUTS_X" / "PELAGIC_INPUTS.txt")


@pytest.mark.parametrize("depth", [1.0, 2.5, 10.8])
def test_type2_akinetes_give_the_same_tn_whatever_the_depth(tmp_path, depth):
    out = _run(tmp_path / f"d{depth}", depth)
    df = val.load_box_output(str(out / "PELAGIC_BOX_00007.out"), 2012)
    assert df["AKI_C"].tolist() == pytest.approx([AKI_CONC, AKI_CONC], rel=1e-9)
    assert df["TN"].tolist() == pytest.approx([OTHER_N + val.N_TO_C * AKI_CONC] * 2, rel=1e-9)
    assert df["TP"].tolist() == pytest.approx([val.P_TO_C * AKI_CONC] * 2, rel=1e-9)


def test_type1_akinetes_are_left_as_written(tmp_path):
    out = _run(tmp_path, 4.0, aki_type=1)
    df = val.load_box_output(str(out / "PELAGIC_BOX_00007.out"), 2012)
    assert df["AKI_C"].tolist() == pytest.approx([AKI_CONC, AKI_CONC])


@pytest.mark.parametrize("missing", ["master", "water_levels"])
def test_unknown_basis_leaves_tn_and_tp_unscored(tmp_path, capsys, missing):
    out = _run(tmp_path, 3.0, master=missing != "master", water_levels=missing != "water_levels")
    df = val.load_box_output(str(out / "PELAGIC_BOX_00007.out"), 2012)
    assert df["TN"].isna().all() and df["TP"].isna().all()
    obs = tmp_path / "obs.csv"
    obs.write_text(OBS_HEADER
                   + "LTK1,7,Strait,2012-03-01,TN,0,1.2,mg N/L,s,c,mg,element\n"
                   + "LTK1,7,Strait,2012-06-01,TN,0,1.3,mg N/L,s,c,mg,element\n"
                   + "LTK1,7,Strait,2012-03-01,NH4,1,0.3,mg N/L,s,c,mg,element\n"
                   + "LTK1,7,Strait,2012-06-01,NH4,1,0.5,mg N/L,s,c,mg,element\n")
    val.main(["--outputs", str(out), "--obs", str(obs), "--out", str(tmp_path / "v"), "--no-plots",
              "--template-c-to-chla"])
    assert "TN and TP not scored" in capsys.readouterr().out
    with open(tmp_path / "v" / "validation_metrics.csv") as fh:
        scored = {r["variable"] for r in csv.DictReader(fh)}
    assert scored == {"NH4"}


def test_water_levels_without_volume_over_area_count_as_absent(tmp_path):
    out = _run(tmp_path, 3.0)
    (out / "WATER_LEVELS.out").write_text(          # a five-column file, as written before 2026-01-18
        "      TIME    BOX NO   SURFACE ELEVATION (m)   VOLUME (m^3)   DEPTH (m)\n"
        "    1.0000,         7,      0.0000,   1000000.0000,   3.0000\n")
    assert val.box_depths(str(out)) is None
    df = val.load_box_output(str(out / "PELAGIC_BOX_00007.out"), 2012)
    assert df["TN"].isna().all()
    assert "WATER_LEVELS.out" in val.area_basis_problem(str(out))


def test_pelagic_inputs_flag_supplies_the_output_types(tmp_path, capsys):
    out = _run(tmp_path, 3.0, master=False)
    obs = tmp_path / "obs.csv"
    obs.write_text(OBS_HEADER
                   + "LTK1,7,Strait,2012-03-01,TN,0,1.2,mg N/L,s,c,mg,element\n"
                   + "LTK1,7,Strait,2012-06-01,TN,0,1.3,mg N/L,s,c,mg,element\n")
    val.main(["--outputs", str(out), "--obs", str(obs), "--out", str(tmp_path / "v"), "--no-plots",
              "--template-c-to-chla", "--pelagic-inputs", str(tmp_path / "INPUTS_X" / "PELAGIC_INPUTS.txt")])
    assert "converted to g/m3" in capsys.readouterr().out
    with open(tmp_path / "v" / "validation_metrics.csv") as fh:
        rows = {r["variable"]: r for r in csv.DictReader(fh)}
    assert float(rows["TN"]["model_mean"]) == pytest.approx(OTHER_N + val.N_TO_C * AKI_CONC)
