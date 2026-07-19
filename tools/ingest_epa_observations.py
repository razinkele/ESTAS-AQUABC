#!/usr/bin/env python3
"""Ingest the Lithuanian EPA water-quality + chlorophyll archive into AQUABC obs.

Reads the two EPA workbooks in ``curonian/DATA/JTD`` — a multi-sheet water-quality
file (one sheet per era, 1984-2021) and a chlorophyll-a file — cleans the raw
values, converts every measurement to AQUABC's *element* convention, maps each
monitoring station to a CL29 model box, and emits:

  * ``epa_observations_tidy.csv`` — one row per (station, date, depth, variable):
    the canonical long table, carrying full provenance (source sheet, original
    column, original units/basis) so every converted number is auditable and
    directly usable by a PEST/PEST++ calibration.
  * ``EPA_<station>_box<box>.dates`` — one per mapped station, in the whitespace
    ``.dates`` format the Shiny observations module already reads (columns are the
    62 model state-variable indices, ``-1`` = missing, model units).

Units & speciation (the correctness core, verified empirically from cross-era
value continuity, see ``docs`` / the PR description):

  * The 1984-2005 sheets report micrograms per litre (µg/L); 2006-2021 report
    mg/L. The µg sheets are scaled by 1e-3.
  * Dissolved-N species (NH4, NO2, NO3) are reported as the *ion* in every sheet
    up to and including 2006-2007, then as nitrogen (``-N``) from 2008 on. The
    lab's switch is invisible in the headers but unmistakable in the data: e.g.
    the 2006-2007 ``NO3 mg/l`` median 0.34 × 14/62 = 0.077 lands exactly on the
    2008-2010 ``NO3-N mg/l`` median. Ion columns are converted to N by molar mass.
  * PO4 and the totals TN/TP are element-basis (P, N) throughout. Silica is taken
    literally by label: ``-Si``/``Si`` = element, ``SiO2`` × 28/60, ``SiO4`` × 28/92.

Stations are joined on the *bare* code (the "LTK" prefix stripped), which is what
the older sheets already use. Stations absent from ``epa_station_to_box.csv`` (the
~17 discontinued historical stations that have no coordinates anywhere, plus the
Curonian-spit "Km ..." coastal stations) are dropped and counted in the run log.

Standard-library + pandas only. Reading .xlsx needs ``openpyxl`` at runtime; the
unit tests exercise the pure logic on synthetic frames and need neither.
"""
from __future__ import annotations

import argparse
import csv
import math
import os
import re
import sys
from collections import defaultdict

import pandas as pd

# --- Model state-variable indices in the .dates format (see obs_loader.py) -----
# var_key -> (model_index or None, model_units, human name)
VARIABLES = {
    "NH4": (1, "mg N/L", "Ammonium N"),
    "NO3": (2, "mg N/L", "Nitrate N"),
    "PO4": (3, "mg P/L", "Orthophosphate P"),
    "DO": (4, "mg O2/L", "Dissolved oxygen"),
    "Si": (17, "mg Si/L", "Dissolved silica"),
    "pH": (37, "-", "pH"),
    "TP": (39, "mg P/L", "Total P"),
    "TN": (40, "mg N/L", "Total N"),
    "CHLA": (59, "ug/L", "Chlorophyll a"),
    # Auxiliary variables — cleaned and carried in the tidy table for QC / totals
    # closure, but have no direct model state (index None -> not written to .dates).
    "NO2": (None, "mg N/L", "Nitrite N"),
    "DIN": (None, "mg N/L", "Dissolved inorganic N"),
}
DATES_NCOL = 62  # number of value columns in a .dates row
# Plausibility bound for a sample date. The archive spans 1984-2021; a handful of
# source rows carry typo years (1901, 1903, 2026, 2027) that must not reach the
# calibration set. Slack on both ends keeps genuine edge samples.
MIN_YEAR, MAX_YEAR = 1980, 2023

# --- Ion -> element molar-mass conversion factors --------------------------------
MOLAR = {
    "element": 1.0,
    "ion_nh4": 14.007 / 18.039,  # NH4+  -> N
    "ion_no3": 14.007 / 62.004,  # NO3-  -> N
    "ion_no2": 14.007 / 46.005,  # NO2-  -> N
    "sio2": 28.085 / 60.083,     # SiO2  -> Si
    "sio4": 28.085 / 92.086,     # SiO4  -> Si
}

# --- Per-sheet column resolution -------------------------------------------------
# Columns are matched by regex against the lowercased/stripped header (robust to the
# µ-sign encoding and stray whitespace). Each row: (pattern, var, unit, basis) where
# unit in {"ug","mg","none"} ("ug" scales by 1e-3; "none"/"mg" leave the number as
# read) and basis keys into MOLAR.
SHEET_SPECS = {
    "1984_1993": {
        "date": [r"^date"], "station": [r"^station"],
        "cols": [
            (r"o2 conc", "DO", "mg", "element"),
            (r"^ph$", "pH", "none", "element"),
            (r"nh4", "NH4", "ug", "ion_nh4"),
            (r"no2", "NO2", "ug", "ion_no2"),
            (r"no3", "NO3", "ug", "ion_no3"),
            (r"^n \(", "TN", "ug", "element"),
            (r"po4", "PO4", "ug", "element"),
            (r"^p \(", "TP", "ug", "element"),
            (r"sio2", "Si", "ug", "element"),
        ],
    },
    "1992-2003": {
        "date": [r"^date"], "station": [r"^station"],
        "cols": [
            (r"^o2_mg$", "DO", "mg", "element"),
            (r"^ph$", "pH", "none", "element"),
            (r"po4_mkg", "PO4", "ug", "element"),
            (r"^p_mkg$", "TP", "ug", "element"),
            (r"si_mkg", "Si", "ug", "element"),
            (r"no2_mkg", "NO2", "ug", "ion_no2"),
            (r"no3_mkg", "NO3", "ug", "ion_no3"),
            (r"nh4_mkg", "NH4", "ug", "ion_nh4"),
            (r"^n_mkg$", "TN", "ug", "element"),
            (r"nmin_mkg", "DIN", "ug", "element"),
        ],
    },
    "2005": {
        "date": [r"^date"], "station": [r"^station"],
        "cols": [
            (r"no3", "NO3", "ug", "ion_no3"),
            (r"no2", "NO2", "ug", "ion_no2"),
            (r"nh4", "NH4", "ug", "ion_nh4"),
            (r"^tn", "TN", "ug", "element"),
            (r"^o2 mg", "DO", "mg", "element"),
            (r"^ph$", "pH", "none", "element"),
            (r"^po4", "PO4", "ug", "element"),
            (r"^tp", "TP", "ug", "element"),
            (r"sio2", "Si", "ug", "sio2"),
        ],
    },
    "2006-2007": {
        "date": [r"^date"], "station": [r"^station"],
        "cols": [
            (r"no3", "NO3", "mg", "ion_no3"),
            (r"no2", "NO2", "mg", "ion_no2"),
            (r"nh4", "NH4", "mg", "ion_nh4"),
            (r"^tn", "TN", "mg", "element"),
            (r"^o2 mg", "DO", "mg", "element"),
            (r"^ph$", "pH", "none", "element"),
            (r"^po4", "PO4", "mg", "element"),
            (r"^tp", "TP", "mg", "element"),
            (r"sio4", "Si", "mg", "sio4"),
        ],
    },
    "2008-2010": {
        "date": [r"^date"], "station": [r"^station"],
        "cols": [
            (r"^do ", "DO", "mg", "element"),
            (r"^ph$", "pH", "none", "element"),
            (r"^tn ", "TN", "mg", "element"),
            (r"no3-n", "NO3", "mg", "element"),
            (r"no2-n", "NO2", "mg", "element"),
            (r"nh4-n", "NH4", "mg", "element"),
            (r"^din", "DIN", "mg", "element"),
            (r"^tp ", "TP", "mg", "element"),
            (r"po4-p", "PO4", "mg", "element"),
            (r"sio2-si", "Si", "mg", "element"),
        ],
    },
    "2011-2012": {
        "date": [r"^date"], "station": [r"^station"],
        "cols": [
            (r"^do ", "DO", "mg", "element"),
            (r"^ph$", "pH", "none", "element"),
            (r"^tn ", "TN", "mg", "element"),
            (r"no3-n", "NO3", "mg", "element"),
            (r"no2-n", "NO2", "mg", "element"),
            (r"nh4-n", "NH4", "mg", "element"),
            (r"^din", "DIN", "mg", "element"),
            (r"^tp ", "TP", "mg", "element"),
            (r"po4-p", "PO4", "mg", "element"),
            (r"^si ", "Si", "mg", "element"),
        ],
    },
    "2013-2021": {
        "date": [r"^date"], "station": [r"mv code", r"^station"],
        "cols": [
            (r"^tn ", "TN", "mg", "element"),
            (r"azotas mineralinis", "DIN", "mg", "element"),
            (r"^nh4 ", "NH4", "mg", "element"),
            (r"^no2 ", "NO2", "mg", "element"),
            (r"^no3 ", "NO3", "mg", "element"),
            (r"^tp ", "TP", "mg", "element"),
            (r"^po4 ", "PO4", "mg", "element"),
            (r"^si ", "Si", "mg", "element"),
            (r"^do mg", "DO", "mg", "element"),
            (r"^ph$", "pH", "none", "element"),
        ],
    },
}

# Chlorophyll-a workbook: (station-col patterns, chla-col pattern) per sheet.
CHLA_SPECS = {
    "1984-2010": {"date": [r"^date"], "station": [r"^station"], "col": r"chlorophyll"},
    "2014-2021": {"date": [r"date"], "station": [r"mv kodas", r"mv code", r"^station"],
                  "col": r"chl-a"},
}


# --- Pure helpers (unit-tested) --------------------------------------------------
def clean_value(raw):
    """Parse one raw cell to a float, or NaN if it is not a usable measurement.

    Handles: numbers as-is; decimal commas ("0,045"); left-censored detection
    limits ("<0.006" -> half the limit, 0.003); right-censored (">x" -> x); and
    blanks / dashes / "na" -> NaN.
    """
    if raw is None or (isinstance(raw, float) and math.isnan(raw)):
        return float("nan")
    if isinstance(raw, (int, float)):
        return float(raw)
    s = str(raw).strip().replace(",", ".")
    if s == "" or s.lower() in {"-", "na", "n/a", "nd", "nan", "."}:
        return float("nan")
    m = re.match(r"^<\s*([\d.]+)$", s)
    if m:
        return float(m.group(1)) / 2.0
    m = re.match(r"^>\s*([\d.]+)$", s)
    if m:
        return float(m.group(1))
    try:
        return float(s)
    except ValueError:
        return float("nan")


def to_model_units(value, unit, basis):
    """Convert a cleaned value to AQUABC model units (element basis, mg/L)."""
    factor = 1.0
    if unit == "ug":
        factor *= 1e-3
    factor *= MOLAR[basis]
    return value * factor


def norm_station(raw):
    """Normalise a raw station cell to its bare code (join key), or None.

    "LTK3B"/"3B" -> "3B"; 10/"10.0" -> "10"; Curonian-spit "Km ..." -> None.
    """
    if raw is None or (isinstance(raw, float) and math.isnan(raw)):
        return None
    if isinstance(raw, float) and raw.is_integer():
        raw = int(raw)
    s = str(raw).strip().upper().replace(" ", "")
    if s == "" or s.startswith("KM"):  # spit / Baltic-shore stations: no lagoon box
        return None
    if s.startswith("LTK"):
        s = s[3:]
    return s or None


def parse_date(raw):
    """Return a ``datetime.date`` from a heterogeneous date cell, or None."""
    if raw is None or (isinstance(raw, float) and math.isnan(raw)):
        return None
    ts = pd.to_datetime(raw, errors="coerce")
    if pd.isna(ts) and isinstance(raw, str) and "." in raw:
        ts = pd.to_datetime(raw, errors="coerce", dayfirst=True)
    return None if pd.isna(ts) else ts.date()


def date_in_range(date):
    """True if a parsed date falls in the plausible [MIN_YEAR, MAX_YEAR] window."""
    return date is not None and MIN_YEAR <= date.year <= MAX_YEAR


def _match_col(patterns, columns, consumed):
    """First unconsumed column whose lowercased name matches any pattern."""
    for pat in patterns:
        rx = re.compile(pat)
        for c in columns:
            if c in consumed:
                continue
            if rx.search(str(c).strip().lower()):
                return c
    return None


def load_station_box(path):
    """Load epa_station_to_box.csv -> {bare_code: {'label','box','region'}}."""
    out = {}
    with open(path, newline="") as fh:
        for row in csv.DictReader(r for r in fh if not r.lstrip().startswith("#")):
            key = norm_station(row["station"])
            out[key] = {"label": row["station"].strip(),
                        "box": int(row["box"]), "region": row["region"].strip()}
    return out


# --- Ingestion ------------------------------------------------------------------
def ingest_sheet(df, sheet, spec, station_box, stats):
    """Yield tidy observation dicts from one water-quality sheet DataFrame."""
    cols = list(df.columns)
    date_col = _match_col(spec["date"], cols, set())
    sta_col = _match_col(spec["station"], cols, set())
    if date_col is None or sta_col is None:
        stats["skipped_sheets"].append(sheet)
        return
    consumed = {date_col, sta_col}
    resolved = []
    for pat, var, unit, basis in spec["cols"]:
        col = _match_col([pat], cols, consumed)
        if col is None:
            stats["unmatched"].append(f"{sheet}:{var}")
            continue
        consumed.add(col)
        resolved.append((col, var, unit, basis))

    for _, rec in df.iterrows():
        bare = norm_station(rec[sta_col])
        if bare is None:
            continue
        info = station_box.get(bare)
        if info is None:
            stats["dropped_stations"][bare] += 1
            continue
        date = parse_date(rec[date_col])
        if not date_in_range(date):
            stats["bad_dates"] += 1
            continue
        for col, var, unit, basis in resolved:
            val = clean_value(rec[col])
            if math.isnan(val):
                continue
            model = to_model_units(val, unit, basis)
            idx = VARIABLES[var][0]
            yield {
                "station": info["label"], "box": info["box"],
                "region": info["region"], "date": date.isoformat(),
                "variable": var, "model_index": idx if idx else "",
                "value": model, "units": VARIABLES[var][1],
                "source_sheet": sheet, "orig_column": str(col),
                "orig_unit": unit, "orig_basis": basis,
            }


def ingest_chla(df, sheet, spec, station_box, stats):
    """Yield tidy chlorophyll-a observation dicts from one chla sheet."""
    cols = list(df.columns)
    date_col = _match_col(spec["date"], cols, set())
    sta_col = _match_col(spec["station"], cols, set())
    chl_col = _match_col([spec["col"]], cols, set())
    if not (date_col and sta_col and chl_col):
        stats["skipped_sheets"].append(f"chla:{sheet}")
        return
    for _, rec in df.iterrows():
        bare = norm_station(rec[sta_col])
        info = station_box.get(bare) if bare else None
        if info is None:
            if bare is not None:
                stats["dropped_stations"][bare] += 1
            continue
        date = parse_date(rec[date_col])
        if not date_in_range(date):
            stats["bad_dates"] += 1
            continue
        val = clean_value(rec[chl_col])
        if math.isnan(val):
            continue
        yield {
            "station": info["label"], "box": info["box"], "region": info["region"],
            "date": date.isoformat(), "variable": "CHLA", "model_index": 59,
            "value": val, "units": "ug/L", "source_sheet": f"chla:{sheet}",
            "orig_column": str(chl_col), "orig_unit": "none", "orig_basis": "element",
        }


def ingest(wq_path, chla_path, station_box, stats):
    """Read both workbooks and return the full list of tidy observation dicts."""
    rows = []
    xl = pd.ExcelFile(wq_path)
    for sheet, spec in SHEET_SPECS.items():
        if sheet not in xl.sheet_names:
            stats["skipped_sheets"].append(sheet)
            continue
        df = xl.parse(sheet)
        rows.extend(ingest_sheet(df, sheet, spec, station_box, stats))
    if chla_path and os.path.exists(chla_path):
        cxl = pd.ExcelFile(chla_path)
        for sheet, spec in CHLA_SPECS.items():
            if sheet not in cxl.sheet_names:
                stats["skipped_sheets"].append(f"chla:{sheet}")
                continue
            rows.extend(ingest_chla(cxl.parse(sheet), sheet, spec, station_box, stats))
    return rows


# --- Output ---------------------------------------------------------------------
TIDY_FIELDS = ["station", "box", "region", "date", "variable", "model_index",
               "value", "units", "source_sheet", "orig_column", "orig_unit",
               "orig_basis"]


def write_tidy(rows, path):
    with open(path, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=TIDY_FIELDS)
        w.writeheader()
        for r in sorted(rows, key=lambda r: (r["station"], r["date"], r["variable"])):
            w.writerow({k: r[k] for k in TIDY_FIELDS})


def write_dates(rows, out_dir):
    """Write one .dates file per mapped station (depths averaged per date)."""
    # (station,label,box) -> {date -> {index -> [values]}}
    by_sta = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    meta = {}
    for r in rows:
        idx = r["model_index"]
        if idx == "" or idx is None:
            continue
        by_sta[r["station"]][r["date"]][int(idx)].append(r["value"])
        meta[r["station"]] = r["box"]
    written = []
    header = "#date      time      " + " ".join(str(i) for i in range(1, DATES_NCOL + 1))
    for station, dates in sorted(by_sta.items()):
        path = os.path.join(out_dir, f"EPA_{station}_box{meta[station]}.dates")
        with open(path, "w") as fh:
            fh.write(header + "\n")
            for date in sorted(dates):
                vals = ["-1"] * DATES_NCOL
                for idx, xs in dates[date].items():
                    vals[idx - 1] = f"{sum(xs) / len(xs):.6g}"
                fh.write(f"{date.replace('-', '')} 000000 " + " ".join(vals) + "\n")
        written.append((os.path.basename(path), len(dates)))
    return written


def _median(xs):
    xs = sorted(xs)
    n = len(xs)
    return xs[n // 2] if n % 2 else (xs[n // 2 - 1] + xs[n // 2]) / 2


def print_summary(rows, stats, dates_written):
    """Print a per-variable × per-era continuity table + a drop log to stdout."""
    per = defaultdict(list)  # (var, sheet) -> values
    for r in rows:
        per[(r["variable"], r["source_sheet"])].append(r["value"])
    sheets = sorted({s for _, s in per})
    print(f"\nIngested {len(rows)} observations "
          f"across {len({r['station'] for r in rows})} stations.\n")
    print("Median value per variable per source (model units) — eyeball continuity:")
    print(f"  {'var':5s} " + " ".join(f"{s[-7:]:>9s}" for s in sheets))
    for var in [v for v in VARIABLES if any(k[0] == v for k in per)]:
        cells = []
        for s in sheets:
            xs = per.get((var, s))
            cells.append(f"{_median(xs):9.4g}" if xs else f"{'·':>9s}")
        print(f"  {var:5s} " + " ".join(cells))
    print(f"\n.dates files written: {len(dates_written)}")
    for name, n in dates_written:
        print(f"  {name}  ({n} dates)")
    if stats["dropped_stations"]:
        drops = sorted(stats["dropped_stations"].items(),
                       key=lambda kv: -kv[1])
        total = sum(n for _, n in drops)
        print(f"\nDropped {total} samples at {len(drops)} unmapped stations "
              f"(no box / no coordinates):")
        print("  " + ", ".join(f"{k}={n}" for k, n in drops))
    if stats["unmatched"]:
        print(f"\nColumns not found (variable absent in that era): "
              f"{', '.join(stats['unmatched'])}")
    if stats["bad_dates"]:
        print(f"\nDates skipped (unparseable or outside "
              f"{MIN_YEAR}-{MAX_YEAR}): {stats['bad_dates']}")
    if stats["skipped_sheets"]:
        print(f"\nSkipped sheets: {', '.join(stats['skipped_sheets'])}")


def main(argv=None):
    here = os.path.dirname(os.path.abspath(__file__))
    default_dir = os.path.expanduser("~/curonian/DATA/JTD")
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--wq", default=os.path.join(default_dir,
                   "timeSeries_waterQuality_data_EPA.xlsx"),
                   help="EPA water-quality workbook (.xlsx)")
    p.add_argument("--chla", default=os.path.join(default_dir,
                   "timeSeries_Chla_EPA.xlsx"),
                   help="EPA chlorophyll-a workbook (.xlsx)")
    p.add_argument("--stations", default=os.path.join(here, "epa_station_to_box.csv"),
                   help="station -> box lookup CSV")
    p.add_argument("--out", default="./epa_observations_out",
                   help="output directory for the tidy CSV + .dates files")
    a = p.parse_args(argv)

    if not os.path.exists(a.wq):
        p.error(f"water-quality workbook not found: {a.wq}")
    os.makedirs(a.out, exist_ok=True)
    station_box = load_station_box(a.stations)
    stats = {"dropped_stations": defaultdict(int), "unmatched": [],
             "bad_dates": 0, "skipped_sheets": []}

    rows = ingest(a.wq, a.chla, station_box, stats)
    if not rows:
        print("No observations ingested — check inputs.", file=sys.stderr)
        return 1
    tidy = os.path.join(a.out, "epa_observations_tidy.csv")
    write_tidy(rows, tidy)
    dates_written = write_dates(rows, a.out)
    print_summary(rows, stats, dates_written)
    print(f"\nWrote {tidy}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
