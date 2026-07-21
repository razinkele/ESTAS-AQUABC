"""Unit tests for the KM (Curonian Lagoon) hydrochemistry ingester.

Exercise the pure logic on synthetic long-format frames — no real workbook and no
xlrd/openpyxl/lxml needed. Mirrors tests/python/test_ingest_epa.py.
"""
import os
import sys

import pandas as pd
import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
import ingest_km_observations as km  # noqa: E402


# --- parameter mapping (Lithuanian name -> canonical key) -------------------------
@pytest.mark.parametrize("name,expected", [
    ("amonio azotas", "NH4"),
    ("nitratų azotas", "NO3"),
    ("nitritų azotas", "NO2"),
    ("fosfatų fosforas", "PO4"),
    ("azotas bendras", "TN"),
    ("bendras fosforas", "TP"),
    ("azotas mineralinis", "DIN"),
    ("silicis", "Si"),
    ("Chlorofilas a", "CHLA"),
    ("chlorofilas a", "CHLA"),
    ("biocheminis deguonies suvartojimas per 7 par", "BOD7"),
    ("suspenduotos (skendinčios) medžiagos", "TSS"),
    ("  NITRATŲ  AZOTAS ", "NO3"),          # case / whitespace robust
    ("vandens temperatūra", None),          # unrecognised -> None
])
def test_map_param(name, expected):
    assert km.map_param(name) == expected


def test_map_param_disambiguates_totals_vs_species():
    # "azotas" appears in NH4/NO3 names too, and "fosfor" in PO4's name — the ordered
    # rules must not misclassify the totals or vice versa.
    assert km.map_param("fosfatų fosforas") == "PO4"   # not TP
    assert km.map_param("bendras fosforas") == "TP"    # not PO4
    assert km.map_param("azotas bendras") == "TN"      # not NH4/NO3


# --- date parsing ----------------------------------------------------------------
def test_parse_date_ok():
    assert km.parse_date("2018-03-26 00:00:00").isoformat() == "2018-03-26"


@pytest.mark.parametrize("raw", [None, "", "not-a-date", "n/a"])
def test_parse_date_bad(raw):
    assert km.parse_date(raw) is None


# --- column resolution -----------------------------------------------------------
def test_resolve_columns():
    df = pd.DataFrame(columns=[
        "MV kodas", "Parametro pavadinimas", "Parametro tyrimo rezultatas",
        "Matavimo vienetai", " Tyrimų data nuo", "Vandens gylis nuo"])
    lut = km._resolve_columns(df)
    assert lut["station"] == "MV kodas"
    assert lut["param"] == "Parametro pavadinimas"
    assert lut["value"] == "Parametro tyrimo rezultatas"
    assert lut["date"] == " Tyrimų data nuo"
    assert lut["depth"] == "Vandens gylis nuo"


def test_resolve_value_by_content_when_header_truncated():
    # MHTML export: headers truncated by display:none spans so the value header ("Parame")
    # can't be told from param by name — value must be resolved by content (numeric,
    # non-unit, first after param).
    df = pd.DataFrame(
        [["LTK1", "chlorofilas a", "18.67", "µg/l", "2023-01-04"]],
        columns=["MV kodas", "Parametro pa", "Parame", "Matav", "Tyrimų data"])
    lut = km._resolve_columns(df)
    assert lut["param"] == "Parametro pa"
    assert lut["value"] == "Parame"     # by content, not name
    assert lut["unit"] == "Matav"


# --- end-to-end ingest on a synthetic frame --------------------------------------
def _frame(rows):
    cols = ["MV kodas", "Parametro pavadinimas", "Parametro tyrimo rezultatas",
            "Matavimo vienetai", "Tyrimų data nuo", "Vandens gylis nuo"]
    return pd.DataFrame(rows, columns=cols)


STATION_BOX = {
    "1": {"label": "LTK1", "box": 7, "region": "Strait"},
    "5": {"label": "LTK5", "box": 17, "region": "Northern"},
}


def _fresh_stats():
    from collections import defaultdict
    return {"kept": 0, "blank_values": 0, "bad_dates": 0,
            "unmapped_stations": defaultdict(int), "unmapped_params": defaultdict(int),
            "skipped_files": []}


def test_ingest_file_maps_and_cleans():
    df = _frame([
        ["LTK1", "amonio azotas", "0,064", "mg/l N", "2022-05-10 00:00:00", "0.5"],
        ["LTK5", "fosfatų fosforas", "0.023", "mg/l P", "2022-05-11 00:00:00", "0.5"],
        ["LTK5", "silicis", "<0.02", "mg/l", "2022-05-11 00:00:00", "0.5"],   # censored
        ["LTB-1", "amonio azotas", "0.1", "mg/l N", "2022-05-10 00:00:00", "0.5"],  # sea -> drop
        ["LTK1", "vandens temperatūra", "12", "C", "2022-05-10 00:00:00", "0.5"],   # unknown param
        ["LTK1", "nitratų azotas", "", "mg/l N", "2022-05-10 00:00:00", "0.5"],     # blank -> drop
    ])
    stats = _fresh_stats()
    rows = km.ingest_file(df, "KM_2022.xls", STATION_BOX, stats)
    got = {(r["station"], r["variable"]): r for r in rows}
    assert set(got) == {("1", "NH4"), ("5", "PO4"), ("5", "Si")}
    assert got[("1", "NH4")]["box"] == 7
    assert got[("1", "NH4")]["value"] == pytest.approx(0.064)   # decimal comma parsed
    assert got[("5", "PO4")]["model_index"] == 3
    assert got[("5", "Si")]["value"] == pytest.approx(0.01)     # "<0.02" -> half
    assert stats["unmapped_stations"]["LTB-1"] == 1
    assert stats["unmapped_params"]["vandens temperatūra"] == 1
    assert stats["blank_values"] == 1


def test_write_dates_only_model_vars(tmp_path):
    df = _frame([
        ["LTK1", "amonio azotas", "0.06", "mg/l N", "2022-05-10 00:00:00", "0.5"],
        ["LTK1", "nitratų azotas", "0.20", "mg/l N", "2022-05-10 00:00:00", "0.5"],
        ["LTK1", "nitritų azotas", "0.01", "mg/l N", "2022-05-10 00:00:00", "0.5"],  # aux, no idx
    ])
    rows = km.ingest_file(df, "KM_2022.xls", STATION_BOX, _fresh_stats())
    written = km.write_dates(rows, str(tmp_path))
    assert written == [("KM_1_box7.dates", 1)]
    lines = (tmp_path / "KM_1_box7.dates").read_text().splitlines()
    assert lines[0].startswith("#date")
    vals = lines[1].split()
    assert vals[0] == "20220510"
    assert len(vals[2:]) == km.DATES_NCOL          # 62 value columns
    assert vals[2 + (1 - 1)] == "0.06"             # NH4 -> index 1
    assert vals[2 + (2 - 1)] == "0.2"              # NO3 -> index 2
    assert vals[2 + (17 - 1)] == "-1"              # Si absent here -> missing
    # NO2 is auxiliary (no model index) -> it never lands in any column
    assert "0.01" not in vals


def test_ingest_chla_to_index_59(tmp_path):
    df = _frame([
        ["LTK1", "Chlorofilas a", "18.67", "µg/l", "2022-02-23 00:00:00", "0.5"],
    ])
    rows = km.ingest_file(df, "chla.xls", STATION_BOX, _fresh_stats())
    assert len(rows) == 1
    r = rows[0]
    assert r["variable"] == "CHLA" and r["model_index"] == 59
    assert r["value"] == pytest.approx(18.67)
    km.write_dates(rows, str(tmp_path))
    vals = (tmp_path / "KM_1_box7.dates").read_text().splitlines()[1].split()
    assert vals[2 + (59 - 1)] == "18.67"     # CHLA -> model index 59


def test_write_dates_averages_depths(tmp_path):
    df = _frame([
        ["LTK1", "amonio azotas", "0.06", "mg/l N", "2022-05-10 00:00:00", "0.5"],
        ["LTK1", "amonio azotas", "0.10", "mg/l N", "2022-05-10 00:00:00", "3.0"],
    ])
    rows = km.ingest_file(df, "KM.xls", STATION_BOX, _fresh_stats())
    km.write_dates(rows, str(tmp_path))
    vals = (tmp_path / "KM_1_box7.dates").read_text().splitlines()[1].split()
    assert vals[2] == "0.08"   # (0.06 + 0.10) / 2
