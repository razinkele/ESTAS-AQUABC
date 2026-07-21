"""Tests for tools/ingest_epa_observations.py.

Exercises the pure ingestion logic — value cleaning, unit/basis conversion,
station normalisation, date plausibility, and the sheet -> tidy -> .dates path —
on synthetic frames, so the suite runs in CI without the external EPA .xlsx
archive (which lives outside the repo). The correctness core under test is the
per-era unit (µg/mg) and speciation (ion vs element) conversion.
"""
import math
import os
import sys
from collections import defaultdict

import pandas as pd
import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))

import ingest_epa_observations as ing  # noqa: E402


def new_stats():
    return {"dropped_stations": defaultdict(int), "unmatched": [],
            "bad_dates": 0, "skipped_sheets": []}


# --- clean_value ---------------------------------------------------------------
@pytest.mark.parametrize("raw,expected", [
    (0.045, 0.045),
    (5, 5.0),
    ("0,045", 0.045),      # decimal comma
    ("1.23", 1.23),
    ("<0.006", 0.003),     # left-censored -> half the detection limit
    ("<0,006", 0.003),     # censored + decimal comma
    (">100", 100.0),       # right-censored -> the limit
])
def test_clean_value_numbers(raw, expected):
    assert ing.clean_value(raw) == pytest.approx(expected)


@pytest.mark.parametrize("raw", [None, float("nan"), "", "-", "na", "N/A", "nd", "."])
def test_clean_value_missing(raw):
    assert math.isnan(ing.clean_value(raw))


# --- to_model_units: the unit/basis conversion core ----------------------------
def test_element_mg_passthrough():
    assert ing.to_model_units(1.4, "mg", "element") == pytest.approx(1.4)


def test_ug_scales_to_mg():
    assert ing.to_model_units(830.0, "ug", "element") == pytest.approx(0.830)


def test_ion_no3_ug_to_nitrogen():
    # 1984-1993 NO3- median 140 µg/L -> 140e-3 * 14.007/62.004 = 0.03163 mg N/L,
    # which is the continuity anchor against the modern NO3-N sheets.
    assert ing.to_model_units(140.0, "ug", "ion_no3") == pytest.approx(0.03163, rel=1e-3)


def test_ion_no3_mg_to_nitrogen():
    # 2006-2007 NO3 mg/L median 0.34 (ion) -> 0.0768 mg N/L == 2008-2010 NO3-N.
    assert ing.to_model_units(0.34, "mg", "ion_no3") == pytest.approx(0.0768, rel=1e-3)


def test_ion_nh4_to_nitrogen():
    assert ing.to_model_units(45.0, "ug", "ion_nh4") == pytest.approx(0.03494, rel=1e-3)


def test_silica_sio2_and_sio4():
    assert ing.to_model_units(552.0, "ug", "sio2") == pytest.approx(0.2580, rel=1e-3)
    assert ing.to_model_units(1.065, "mg", "sio4") == pytest.approx(0.3249, rel=1e-3)


def test_ph_and_do_unchanged():
    assert ing.to_model_units(8.2, "none", "element") == pytest.approx(8.2)
    assert ing.to_model_units(10.5, "mg", "element") == pytest.approx(10.5)


# --- norm_station --------------------------------------------------------------
@pytest.mark.parametrize("raw,expected", [
    ("LTK3B", "3B"), ("3B", "3B"), ("LTK1", "1"),
    (10, "10"), (10.0, "10"), ("3DT", "3DT"), ("7B", "7B"),
    ("Km Nida", None), ("KM VENTE", None), ("  ", None), (None, None),
])
def test_norm_station(raw, expected):
    assert ing.norm_station(raw) == expected


# --- parse_date + plausibility -------------------------------------------------
def test_parse_date_formats():
    assert ing.parse_date("2019-02-13").year == 2019
    assert ing.parse_date(pd.Timestamp("2010-06-01")).month == 6
    assert ing.parse_date("garbage") is None
    assert ing.parse_date(None) is None


def test_date_in_range():
    assert ing.date_in_range(ing.parse_date("2019-02-13"))
    assert not ing.date_in_range(ing.parse_date("2027-05-04"))  # source typo year
    assert not ing.date_in_range(ing.parse_date("1901-12-11"))  # source typo year
    assert not ing.date_in_range(None)


# --- station lookup ------------------------------------------------------------
def test_load_station_box_real_csv():
    csv_path = os.path.join(os.path.dirname(__file__), "..", "..", "tools",
                            "epa_station_to_box.csv")
    sb = ing.load_station_box(csv_path)
    assert sb["1"]["box"] == 7 and sb["1"]["label"] == "LTK1"
    assert sb["3B"]["box"] == 11
    assert sb["3DT"]["box"] == 11
    assert sb["3A"]["box"] == 11             # gpkg-confirmed (was inferred)
    assert sb["14"]["box"] == 9              # gpkg point-in-polygon fix (was mis-read as 25)
    assert "#" not in "".join(sb)  # comment lines skipped


# --- full sheet ingestion path -------------------------------------------------
STATION_BOX = {"1": {"label": "LTK1", "box": 7, "region": "Strait"}}


def test_ingest_modern_sheet_element_basis():
    """2013-2021: bare NO3 mg/l is element-basis and passes through unchanged."""
    df = pd.DataFrame({
        "Date of sampling": ["2019-02-13", "2027-01-01", "2019-03-01"],
        "MV code": ["LTK1", "LTK1", "3A"],  # mapped, mapped-but-bad-date, unmapped
        "NO3 mg/l": [1.4, 1.4, 0.5],
        "PO4 mg/l": ["<0.006", 0.02, 0.02],
        "pH": [8.1, 8.1, 8.1],
    })
    stats = new_stats()
    rows = list(ing.ingest_sheet(df, "2013-2021", ing.SHEET_SPECS["2013-2021"],
                                 STATION_BOX, stats))
    no3 = [r for r in rows if r["variable"] == "NO3"]
    assert len(no3) == 1                       # bad-date + unmapped rows excluded
    assert no3[0]["value"] == pytest.approx(1.4)
    assert no3[0]["model_index"] == 2 and no3[0]["box"] == 7
    po4 = [r for r in rows if r["variable"] == "PO4"][0]
    assert po4["value"] == pytest.approx(0.003)  # "<0.006" censored
    assert stats["dropped_stations"]["3A"] == 1
    assert stats["bad_dates"] == 1


def test_ingest_legacy_sheet_ion_conversion():
    """1984_1993: NH4+/NO3- in µg/L are ion-basis and convert to nitrogen."""
    df = pd.DataFrame({
        "Date": ["1990-06-15"],
        "Station (Id)": ["1"],
        "NH4+ (ug/l)": [45.0],
        "NO3- (ug/l)": [140.0],
        "pH": [8.3],
    })
    stats = new_stats()
    rows = list(ing.ingest_sheet(df, "1984_1993", ing.SHEET_SPECS["1984_1993"],
                                 STATION_BOX, stats))
    by = {r["variable"]: r for r in rows}
    assert by["NH4"]["value"] == pytest.approx(0.03494, rel=1e-3)
    assert by["NH4"]["orig_basis"] == "ion_nh4"
    assert by["NO3"]["value"] == pytest.approx(0.03163, rel=1e-3)
    assert by["pH"]["value"] == pytest.approx(8.3)


# --- .dates writer -------------------------------------------------------------
def test_write_dates_averages_and_formats(tmp_path):
    rows = [
        {"station": "LTK1", "box": 7, "model_index": 2, "date": "2019-02-13",
         "value": 1.0},
        {"station": "LTK1", "box": 7, "model_index": 2, "date": "2019-02-13",
         "value": 2.0},  # same station+date -> averaged with the row above
        {"station": "LTK1", "box": 7, "model_index": 1, "date": "2019-02-13",
         "value": 0.05},
        {"station": "LTK1", "box": 7, "model_index": "", "date": "2019-02-13",
         "value": 9.9},  # auxiliary var (no index) -> not written
    ]
    written = ing.write_dates(rows, str(tmp_path))
    assert written == [("EPA_LTK1_box7.dates", 1)]
    lines = (tmp_path / "EPA_LTK1_box7.dates").read_text().splitlines()
    assert lines[0].startswith("#date")
    header_idx = lines[0].split()[2:]
    assert header_idx[0] == "1" and header_idx[-1] == str(ing.DATES_NCOL)
    fields = lines[1].split()
    assert fields[0] == "20190213" and fields[1] == "000000"
    vals = fields[2:]
    assert len(vals) == ing.DATES_NCOL
    assert vals[0] == "0.05"        # index 1 (NH4)
    assert vals[1] == "1.5"         # index 2 (NO3) = mean(1.0, 2.0)
    assert vals[2] == "-1"          # index 3 unmeasured
