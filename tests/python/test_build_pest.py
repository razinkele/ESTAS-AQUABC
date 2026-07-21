"""Unit tests for the CL29 PEST build (pest/build_pest.py) — pure logic, stdlib only."""
import datetime as dt
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "pest"))
import build_pest as bp  # noqa: E402, I001


WCONST = """\
     1                                K_A              -1.0  !  1   Aeration coefficient
     5                    KG_DIA_OPT_TEMP               3.7  !  5   Diatoms Growth rate
   165                  K_MIN_DOC_NO3N_20             1.0  !165   Denitrification DOC-NO3
"""


def test_read_wconst(tmp_path):
    f = tmp_path / "w.txt"
    f.write_text(WCONST)
    _lines, idx = bp.read_wconst(str(f))
    assert idx["KG_DIA_OPT_TEMP"][2] == 3.7        # value parsed
    assert idx["K_MIN_DOC_NO3N_20"][2] == 1.0
    assert idx["KG_DIA_OPT_TEMP"][0] == 1          # 0-based line number


def test_template_line_replaces_value_and_keeps_comment():
    line = "     5                    KG_DIA_OPT_TEMP               3.7  !  5   Diatoms Growth rate"
    out = bp.template_line(line, "KG_DIA_OPT_TEMP")
    assert "@KG_DIA_OPT_TEMP" in out and out.count("@") == 2
    assert "3.7" not in out.split("!")[0]          # value token gone from the code part
    assert out.endswith("Diatoms Growth rate")     # comment preserved
    # the marker is whitespace-separated from the name and the '!'
    code = out.split("!")[0]
    assert " @KG_DIA_OPT_TEMP" in code and code.rstrip().endswith("@")


def _tidy(tmp_path, rows):
    cols = ["station", "box", "region", "date", "depth", "variable",
            "model_index", "value", "units", "source_file", "orig_param", "orig_unit"]
    f = tmp_path / "obs.csv"
    with open(f, "w") as fh:
        fh.write(",".join(cols) + "\n")
        for box, var, date, val in rows:
            fh.write(f"1,{box},Strait,{date},0.5,{var},,{val},,,,\n")
    return str(f)


def test_load_obs_filters_and_orders(tmp_path):
    csv_path = _tidy(tmp_path, [
        (7, "NH4", "2022-05-10", "0.06"),
        (7, "NH4", "2022-06-10", "0.08"),
        (11, "NO3", "2022-05-10", "0.5"),
        (7, "DIN", "2022-05-10", "0.3"),      # not a MODEL_COL var -> dropped
        (7, "NH4", "2024-01-01", "0.1"),      # outside window (>4016) -> dropped
    ])
    recs = bp.load_obs(csv_path, base_year=2012, end_day=4016)
    names = [r[0] for r in recs]
    assert names == ["nh4_7_001", "nh4_7_002", "no3_11_001"]   # order + per-(var,box) seq
    assert all(r[3].year == 2022 for r in recs)                # window enforced
    assert recs[0] == ("nh4_7_001", 7, "NH4", dt.date(2022, 5, 10), 0.06)


def test_obs_weights_are_inverse_group_mean():
    recs = [("a", 7, "NH4", None, 0.04), ("b", 7, "NH4", None, 0.06),
            ("c", 7, "PO4", None, 0.01)]
    w = bp.obs_weights(recs)
    assert w["NH4"] == 1.0 / 0.05          # 1/mean(0.04,0.06)
    assert w["PO4"] == 1.0 / 0.01


def test_write_ins_and_pst_counts(tmp_path):
    recs = [("nh4_7_001", 7, "NH4", dt.date(2022, 5, 10), 0.06),
            ("no3_11_001", 11, "NO3", dt.date(2022, 5, 10), 0.5)]
    ins = tmp_path / "m.ins"
    bp.write_ins(recs, str(ins))
    lines = ins.read_text().splitlines()
    assert lines[0] == "pif @"
    assert [ln for ln in lines if ln.startswith("l1")] == ["l1 !nh4_7_001!", "l1 !no3_11_001!"]

    pst = tmp_path / "c.pst"
    bp.write_pst(bp.PARAMS, {n: 1.0 for n, *_ in bp.PARAMS}, recs, str(pst),
                 "pest/wconst_04.tpl", "pest/model_obs.ins", "pest/model_obs.out",
                 "INPUTS_CL29/WCONST_04.txt", "python pest/forward_run.py",
                 noptmax=3, num_reals=50)
    body = pst.read_text().splitlines()
    # control-data line 4: NPAR NOBS NPARGP NPRIOR NOBSGP
    assert body[3] == f"{len(bp.PARAMS)} 2 3 0 2"
    assert "++ies_num_reals(50)" in body
    obs_block = body[body.index("* observation data") + 1: body.index("* model command line")]
    assert len(obs_block) == 2
