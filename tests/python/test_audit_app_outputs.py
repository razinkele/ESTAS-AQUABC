"""Audit items 22 and 30: features that needed a root OUTPUT.csv, and parameter categories."""
import os
import re

import pandas as pd

from shiny_app import output_data
from shiny_app.parameter_parser import PARAMETER_CATEGORIES

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))


def _setup(tmp_path, n_state=36):
    (tmp_path / "INPUT_x.txt").write_text(
        "# PELAGIC MODEL OUTPUT FOLDER write the folder ...\nOUT_X/\n")
    out = tmp_path / "OUT_X"
    out.mkdir()
    cols = ["TIME_DAYS"] + [f"V{i}" for i in range(1, n_state + 1)]
    rows = [[float(t)] + [i + 0.001 * t for i in range(1, n_state + 1)] for t in range(5)]
    text = " ".join(cols) + "\n" + "\n".join(" ".join(f"{v:.6f}" for v in r) for r in rows) + "\n"
    for box in (9, 5):
        (out / f"PELAGIC_BOX_{box:05d}.out").write_text(text)
    (out / "PELAGIC_BOX_00005_PROCESS_RATES.out").write_text("1 2 3\n")
    return str(tmp_path)


def test_first_box_output_is_the_lowest_numbered_state_file(tmp_path):
    root = _setup(tmp_path)
    assert output_data.first_box_output(root, "INPUT_x.txt").endswith("OUT_X/PELAGIC_BOX_00005.out")
    assert output_data.first_box_output(root, "INPUT_missing.txt") is None


def test_analysis_csv_has_the_output_csv_column_names(tmp_path):
    """ESTAS never writes the root OUTPUT.csv that Mass Balance and Observations read."""
    root = _setup(tmp_path)
    path, source = output_data.analysis_csv(root, "INPUT_x.txt", cache_dir=str(tmp_path / "cache"))
    assert source == os.path.join("OUT_X", "PELAGIC_BOX_00005.out")
    df = pd.read_csv(path)
    assert list(df.columns[:6]) == ["TIME", "NH4N", "NO3N", "PO4P", "DOXY", "DIAC"]
    assert {"DON", "DETN", "ZOON", "DIC", "DOC", "DETC", "NOFIX_CYNC", "FIX_CYNC", "OPA",
            "DISSOLVED_SILICA", "PARTICULATE_SILICA"} <= set(df.columns)
    assert len(df) == 5 and df["NH4N"].iloc[0] == 1.0 and df["DIAC"].iloc[4] == 5.004
    # cached until the box file changes
    mtime = os.path.getmtime(path)
    assert output_data.analysis_csv(root, "INPUT_x.txt", cache_dir=str(tmp_path / "cache"))[0] == path
    assert os.path.getmtime(path) == mtime


def test_analysis_csv_falls_back_to_a_root_output_csv_then_to_a_reason(tmp_path):
    (tmp_path / "INPUT.txt").write_text("# PELAGIC MODEL OUTPUT FOLDER\nOUTPUTS/\n")
    path, why = output_data.analysis_csv(str(tmp_path), "INPUT.txt")
    assert path is None and "Run the model" in why
    (tmp_path / "OUTPUT.csv").write_text("TIME, NH4N\n1, 2\n")
    assert output_data.analysis_csv(str(tmp_path), "INPUT.txt") == (str(tmp_path / "OUTPUT.csv"), "OUTPUT.csv")


def test_no_feature_requires_the_root_output_csv():
    mods = os.path.join(ROOT, "shiny_app", "modules")
    for name in ("mass_balance.py", "observations.py"):
        src = open(os.path.join(mods, name)).read()
        assert "OUTPUT.csv not found" not in src and "analysis_csv(" in src, name
    for name in ("plot.py", "dashboard.py"):
        assert "first_box_output(" in open(os.path.join(mods, name)).read(), name


def test_parameter_categories_cover_every_constant_once():
    ids = []
    for line in open(os.path.join(ROOT, "INPUTS", "WCONST_04.txt")):
        parts = line.split()
        if len(parts) >= 3 and parts[0].isdigit():
            ids.append(int(parts[0]))
    covered = sorted(i for lo, hi in PARAMETER_CATEGORIES.values() for i in range(lo, hi + 1))
    assert covered == sorted(ids) == list(range(1, 325))


def test_parameter_categories_match_the_constants_they_hold():
    names = {}
    for line in open(os.path.join(ROOT, "INPUTS", "WCONST_04.txt")):
        parts = line.split()
        if len(parts) >= 3 and parts[0].isdigit():
            names[int(parts[0])] = parts[1]
    rules = {
        "Diatoms": r"DIA", "Non-fixing Cyanobacteria": r"CYN", "Fixing Cyanobacteria": r"FIX",
        "Other Phytoplankton": r"OPA", "Zooplankton": r"ZOO", "Nitrification": r"NITR",
        "DOC Mineralisation": r"DOC|RED_LIM|RED_INHB", "DON Mineralisation": r"DON",
        "DOP Mineralisation": r"DOP", "Methane and Sulphide Oxidation": r"CH4|H2S",
        "Iron Dissolution": r"FE_II", "Nostocales": r"NOST|AKI|VEG_HET",
        "Nostocales Nitrogen": r"NOST|VEG_HET", "Light Curve and Ice": r"BETA|ICE",
    }
    for cat, pattern in rules.items():
        lo, hi = PARAMETER_CATEGORIES[cat]
        for i in range(lo, hi + 1):
            assert re.search(pattern, names[i]), (cat, i, names[i])


def test_analysis_tabs_can_use_any_output_box(tmp_path):
    """The analysis tabs used the first output box only."""
    root = _setup(tmp_path)
    assert output_data.box_outputs(root, "INPUT_x.txt") == {
        "PELAGIC_BOX_00005.out": "Box 5", "PELAGIC_BOX_00009.out": "Box 9"}
    assert output_data.box_outputs(root, "INPUT_missing.txt") == {}
    cache = str(tmp_path / "cache")
    first = output_data.analysis_csv(root, "INPUT_x.txt", cache_dir=cache)
    nine = output_data.analysis_csv(root, "INPUT_x.txt", cache_dir=cache, box_file="PELAGIC_BOX_00009.out")
    assert first[1].endswith("PELAGIC_BOX_00005.out") and nine[1].endswith("PELAGIC_BOX_00009.out")
    assert first[0] != nine[0]
    # a name that is not one of the run's box files falls back to the first box (no path tricks)
    odd = output_data.analysis_csv(root, "INPUT_x.txt", cache_dir=cache, box_file="../INPUT_x.txt")
    assert odd[1].endswith("PELAGIC_BOX_00005.out")
    mods = os.path.join(ROOT, "shiny_app", "modules")
    assert 'box_file=input.mb_box()' in open(os.path.join(mods, "mass_balance.py")).read()
    assert open(os.path.join(mods, "observations.py")).read().count('box_file=input.obs_box()') == 2
