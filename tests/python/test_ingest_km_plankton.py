"""Unit tests for the pure logic of tools/ingest_km_plankton.py (no Excel deps needed)."""
import datetime as dt
import os
import sys

import pandas as pd

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
from ingest_km_plankton import (  # noqa: E402
    DEFAULT_RATIOS,
    ZOO_C_PER_WET,
    aggregate_group_carbon,
    blocks_from_2015,
    class_to_group,
    parse_2015_date,
    zoo_biomass_to_carbon,
)

# --- class/taxon -> AQUABC group ------------------------------------------------

def test_diatom_orders_map_to_dia():
    for cls in ("Bacillariales", "Fragilariales", "Eupodiscales"):
        assert class_to_group(cls, "Whatever species") == "DIA"


def test_nonfixing_cyano_orders():
    assert class_to_group("Chroococcales", "Microcystis aeruginosa") == "CYN"
    assert class_to_group("Oscillatoriales", "Planktothrix agardhii") == "CYN"


def test_nostocales_is_fixing():
    assert class_to_group("Nostocales", "Aphanizomenon flos-aquae") == "FIX"


def test_nostocophyceae_split_by_genus():
    # ambiguous class-level name: heterocystous genera -> FIX, others -> CYN
    assert class_to_group("Nostocophyceae", "Dolichospermum flos-aquae") == "FIX"
    assert class_to_group("Nostocophyceae", "Aphanizomenon gracile") == "FIX"
    assert class_to_group("Nostocophyceae", "Microcystis viridis") == "CYN"


def test_everything_else_is_opa():
    for cls in ("Cryptophyceae", "Dinophyceae", "Chlorellales", "KITI",
                "ĮVAIRIOS FITOPLANKTONO KLASĖS", "SomeNewOrder"):
        assert class_to_group(cls, "x") == "OPA"


# --- conversions -----------------------------------------------------------------

def test_zoo_biomass_to_carbon_units():
    # 1000 mg wet / m3 == 1 mg wet / L -> ZOO_C_PER_WET mg C / L
    assert abs(zoo_biomass_to_carbon(1000.0) - ZOO_C_PER_WET) < 1e-12


def test_default_ratios_have_all_groups():
    assert set(DEFAULT_RATIOS) == {"DIA", "CYN", "FIX", "OPA"}
    assert all(0.01 < v < 0.5 for v in DEFAULT_RATIOS.values())


# --- 2015 block parser -------------------------------------------------------------

def _frame_2015():
    """Synthetic mini-sheet in the real layout: 25 cols, 4-row blocks."""
    ncol = 25
    rows = []

    def row(vals):
        r = [float("nan")] * ncol
        for k, v in vals.items():
            r[k] = v
        rows.append(r)

    # header-ish rows (skipped by the parser)
    row({0: "Date", 1: "Station"})
    # block 1: complete — class cols: 4 CYN, 5 FIX, 7-10/12-14 OPA, 11 DIA
    row({0: "January 25", 1: "Rusnė", 2: "pav.", 3: "Abundance, thousand units l-1", 4: 100.0})
    row({0: "January 26", 1: "Rusnė", 3: "Biovolume, mg l-1", 4: 0.5})
    row({0: "January 27", 1: "Rusnė", 3: "Carbon, mg l-1",
         4: 0.02, 5: 0.0, 7: 0.004, 8: 0.006, 11: 0.03})
    row({0: "January 28", 1: "Rusnė", 3: "Ratio", 4: 0.18})
    # block 2: station blank on later rows, carbon row EMPTY -> must be skipped
    row({0: "January 28", 1: "Kiaulės nugara", 2: "sud.", 3: "Abundance, thousand units l-1", 4: 5.0})
    row({0: "January 29", 3: "Biovolume, mg l-1", 4: 0.1})
    row({0: "January 30", 3: "Carbon, mg l-1"})
    row({0: "January 31", 3: "Ratio", 4: 0.2})
    # block 3: same station+date as block 1, second sample type -> averaged later
    # ('Coeficient' row instead of 'Ratio' as on the per-station Nida sheet)
    row({0: "January 25", 1: "Rusnė", 2: "sud.", 3: "Abundance, thousand units l-1", 4: 50.0})
    row({0: "January 26", 3: "Biovolume, mg l-1", 4: 0.4})
    row({0: "January 27", 3: "Carbon, mg l-1",
         4: 0.04, 5: 0.02, 12: 0.01, 14: 0.02, 11: 0.05})
    row({0: "January 28", 3: "Coeficient", 4: 0.11})
    return pd.DataFrame(rows)


def test_blocks_from_2015_extracts_carbon_by_group():
    recs = blocks_from_2015(_frame_2015(), year=2015)
    # empty carbon row (block 2) skipped -> 2 blocks x 4 groups
    assert len(recs) == 8
    b1 = {r["group"]: round(r["carbon"], 6) for r in recs if r["sample"] == "pav."}
    assert b1 == {"DIA": 0.03, "CYN": 0.02, "FIX": 0.0, "OPA": 0.01}
    b3 = {r["group"]: round(r["carbon"], 6) for r in recs if r["sample"] == "sud."}
    assert b3 == {"DIA": 0.05, "CYN": 0.04, "FIX": 0.02, "OPA": 0.03}
    assert all(r["station"] == "Rusnė" for r in recs)
    assert all(r["date"] == dt.date(2015, 1, 25) for r in recs)


def test_parse_2015_date():
    assert parse_2015_date("January 25", 2015) == dt.date(2015, 1, 25)
    assert parse_2015_date("July 7", 2015) == dt.date(2015, 7, 7)
    assert parse_2015_date("not a date", 2015) is None


# --- species-file aggregation ------------------------------------------------------

def test_aggregate_group_carbon_sums_species_then_averages_samples():
    df = pd.DataFrame([
        # sample A: two diatom species + one fixer
        {"sample": "A", "station": "LTK2", "date": dt.date(2022, 6, 1),
         "cls": "Bacillariales", "taxon": "Asterionella formosa", "biomass": 1.0},
        {"sample": "A", "station": "LTK2", "date": dt.date(2022, 6, 1),
         "cls": "Bacillariales", "taxon": "Amphora ovalis", "biomass": 3.0},
        {"sample": "A", "station": "LTK2", "date": dt.date(2022, 6, 1),
         "cls": "Nostocales", "taxon": "Aphanizomenon flos-aquae", "biomass": 2.0},
        # sample B, same station+date: diatoms only
        {"sample": "B", "station": "LTK2", "date": dt.date(2022, 6, 1),
         "cls": "Fragilariales", "taxon": "Fragilaria", "biomass": 2.0},
    ])
    ratios = {"DIA": 0.1, "CYN": 0.2, "FIX": 0.2, "OPA": 0.15}
    out = aggregate_group_carbon(df, ratios)
    key = ("LTK2", dt.date(2022, 6, 1))
    # DIA: sample A = 4.0, sample B = 2.0 -> mean 3.0 wet -> x0.1 C
    assert abs(out[key]["DIA"] - 0.30) < 1e-12
    # FIX: sample A = 2.0, sample B = 0.0 -> mean 1.0 wet -> x0.2 C
    assert abs(out[key]["FIX"] - 0.20) < 1e-12
    assert out[key]["CYN"] == 0.0 and out[key]["OPA"] == 0.0
