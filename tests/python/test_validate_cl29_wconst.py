"""The validator must not score Chl-a with C:Chl ratios the run did not use.

C:Chl ratios are model parameters (CL29 ships 53/78 against the module's template 30/40). Scoring
a run with the wrong ones mis-states its Chl-a by ~2x (the diagnosis' "§23 trap"; it struck
again on 2026-11-06). Without --wconst the validator now finds the run's own WCONST_04.txt via
the master input file that wrote the output folder, and if it cannot, it leaves Chl-a unscored
unless --template-c-to-chla says the template ratios are wanted.
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
{inp}
# PELAGIC MODEL INPUT FILE
            PELAGIC_INPUTS.txt
# PELAGIC MODEL OUTPUT FOLDER write the folder always with / in the end
{out}
# RESUSPENSION_OPTION
          0
"""
OBS_HEADER = ("station,box,region,date,variable,model_index,value,units,"
              "source_sheet,orig_column,orig_unit,orig_basis\n")


def _wconst(path, dia_ratio):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"    28                      DIA_C_TO_CHLA             {dia_ratio}  ! 28 Diatoms\n")


def _master(root, name, inp, out):
    (root / name).write_text(MASTER.format(inp=inp, out=out))


@pytest.fixture(autouse=True)
def _reset_active_ratios(monkeypatch):
    monkeypatch.setattr(val, "_ACTIVE_C_TO_CHLA", None)


def test_finds_the_wconst_of_the_master_that_wrote_the_outputs(tmp_path):
    (tmp_path / "OUTPUTS_X").mkdir()
    _wconst(tmp_path / "INPUTS_X" / "WCONST_04.txt", 53)
    _master(tmp_path, "INPUT_X.txt", "INPUTS_X/", "OUTPUTS_X/")
    _master(tmp_path, "INPUT_OTHER.txt", "INPUTS_Y/", "OUTPUTS_Y/")    # a different run
    found = val.find_run_wconst(str(tmp_path / "OUTPUTS_X"))
    assert os.path.realpath(found) == os.path.realpath(tmp_path / "INPUTS_X" / "WCONST_04.txt")


def test_no_master_means_unknown(tmp_path):
    (tmp_path / "OUTPUTS_X").mkdir()
    assert val.find_run_wconst(str(tmp_path / "OUTPUTS_X")) is None


def test_masters_disagreeing_on_the_inputs_mean_unknown(tmp_path):
    (tmp_path / "OUTPUTS_X").mkdir()
    _wconst(tmp_path / "INPUTS_A" / "WCONST_04.txt", 53)
    _wconst(tmp_path / "INPUTS_B" / "WCONST_04.txt", 30)
    _master(tmp_path, "INPUT_A.txt", "INPUTS_A/", "OUTPUTS_X/")
    _master(tmp_path, "INPUT_B.txt", "INPUTS_B/", "OUTPUTS_X/")
    assert val.find_run_wconst(str(tmp_path / "OUTPUTS_X")) is None


def test_masters_agreeing_on_the_inputs_are_not_ambiguous(tmp_path):
    (tmp_path / "OUTPUTS_X").mkdir()
    _wconst(tmp_path / "INPUTS_A" / "WCONST_04.txt", 53)
    _master(tmp_path, "INPUT_A.txt", "INPUTS_A/", "OUTPUTS_X/")
    _master(tmp_path, "INPUT_A_VERIFY.txt", "INPUTS_A/", "OUTPUTS_X/")
    assert val.find_run_wconst(str(tmp_path / "OUTPUTS_X")) is not None


def _scoring_case(tmp_path):
    """One box (7) with diatom carbon 1.0 mg C/L, observed Si and Chl-a on two dates."""
    out = tmp_path / "OUTPUTS_X"
    out.mkdir()
    (out / "PELAGIC_BOX_00007.out").write_text(
        "TIME_DAYS DISS_Si DIA_C\n0.0 1.0 1.0\n400.0 1.0 1.0\n")
    obs = tmp_path / "obs.csv"
    obs.write_text(OBS_HEADER
                   + "LTK1,7,Strait,2012-03-01,Si,17,0.8,mg Si/L,s,c,mg,element\n"
                   + "LTK1,7,Strait,2012-06-01,Si,17,0.9,mg Si/L,s,c,mg,element\n"
                   + "LTK1,7,Strait,2012-03-01,CHLA,0,20,ug/L,s,c,ug,element\n"
                   + "LTK1,7,Strait,2012-06-01,CHLA,0,22,ug/L,s,c,ug,element\n")
    return out, obs


def _metrics(path):
    with open(path) as fh:
        return {r["variable"]: r for r in csv.DictReader(fh)}


def test_chla_is_not_scored_when_the_ratios_are_unknown(tmp_path, capsys):
    out, obs = _scoring_case(tmp_path)
    rc = val.main(["--outputs", str(out), "--obs", str(obs), "--out", str(tmp_path / "v"), "--no-plots"])
    assert rc == 0
    m = _metrics(tmp_path / "v" / "validation_metrics.csv")
    assert "Si" in m and "CHLA" not in m
    assert "CHLA not scored" in capsys.readouterr().out


def test_ratios_are_taken_from_the_runs_own_wconst(tmp_path, capsys):
    out, obs = _scoring_case(tmp_path)
    _wconst(tmp_path / "INPUTS_X" / "WCONST_04.txt", 53)
    _master(tmp_path, "INPUT_X.txt", "INPUTS_X/", "OUTPUTS_X/")
    val.main(["--outputs", str(out), "--obs", str(obs), "--out", str(tmp_path / "v"), "--no-plots"])
    m = _metrics(tmp_path / "v" / "validation_metrics.csv")
    assert float(m["CHLA"]["model_mean"]) == pytest.approx(1000.0 / 53.0)
    assert "auto-detected" in capsys.readouterr().out


def test_template_ratios_only_on_request(tmp_path):
    out, obs = _scoring_case(tmp_path)
    val.main(["--outputs", str(out), "--obs", str(obs), "--out", str(tmp_path / "v"), "--no-plots",
              "--template-c-to-chla"])
    m = _metrics(tmp_path / "v" / "validation_metrics.csv")
    assert float(m["CHLA"]["model_mean"]) == pytest.approx(1000.0 / 30.0)


def test_explicit_wconst_still_wins(tmp_path):
    out, obs = _scoring_case(tmp_path)
    _wconst(tmp_path / "INPUTS_X" / "WCONST_04.txt", 53)
    _master(tmp_path, "INPUT_X.txt", "INPUTS_X/", "OUTPUTS_X/")
    _wconst(tmp_path / "elsewhere" / "WCONST_04.txt", 78)
    val.main(["--outputs", str(out), "--obs", str(obs), "--out", str(tmp_path / "v"), "--no-plots",
              "--wconst", str(tmp_path / "elsewhere" / "WCONST_04.txt")])
    m = _metrics(tmp_path / "v" / "validation_metrics.csv")
    assert float(m["CHLA"]["model_mean"]) == pytest.approx(1000.0 / 78.0)
