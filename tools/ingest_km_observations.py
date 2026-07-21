#!/usr/bin/env python3
"""Ingest the Curonian Lagoon (KM) hydrochemistry extract into box-aligned AQUABC obs.

Reads the *Kuršių marios* (KM) hydrochemistry workbooks in
``curonian/DATA/2014-2023_BJ duomenys extrahuoti/Hidrocheminiai tyrimai`` and emits the
same two products as ``ingest_epa_observations.py``, so the result plugs directly into
``validate_cl29_vs_epa.py`` and the Shiny observations module:

  * ``km_observations_tidy.csv`` — one row per (station, date, depth, variable), carrying
    provenance (source file, original Lithuanian parameter name, original units).
  * ``KM_<station>_box<box>.dates`` — the whitespace ``.dates`` format (62 model
    state-variable columns, ``-1`` = missing, model units).

Why this is much simpler than the EPA ingester: the KM extract is already a *long* table
(one row per station/date/parameter) and every value is reported in the element basis in
mg/L (``mg/l N`` for the dissolved-N species, ``mg/l P`` for phosphate), so there is no
per-era µg scaling and no ion→element molar conversion — the number is taken as read.

Station→box mapping and the value-cleaning / station-normalisation logic are reused from
``ingest_epa_observations.py``. Lagoon stations use the same ``LTK`` codes as the EPA
archive (northings verified to match ``epa_station_to_box.csv``); Baltic-Sea ``LT`` /
coastal stations have no lagoon box and are dropped.

File formats in the extract are mixed and detected by magic bytes: real OLE2 ``.xls``
(``xlrd``), zip-based ``.xlsx`` (``openpyxl``), and — for the 2023 combined files — MIME /
MHTML "web page" exports that merely carry a ``.xls`` extension (parsed via ``read_html``).

Standard-library + pandas only. Reading the workbooks needs ``xlrd`` / ``openpyxl`` /
``lxml`` at runtime; the unit tests exercise the pure logic on synthetic frames and need
none of them.
"""
from __future__ import annotations

import argparse
import csv
import glob
import os
import sys

import pandas as pd

# Reuse the EPA ingester's verified helpers (sibling module in tools/).
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ingest_epa_observations import (  # noqa: E402
    DATES_NCOL,
    VARIABLES,
    clean_value,
    date_in_range,
    load_station_box,
    norm_station,
)

# --- Lithuanian parameter name -> canonical variable key -------------------------
# Ordered rules: the first rule whose substrings are ALL present in the lowercased
# parameter name wins. Substrings are chosen ASCII-safe (before any diacritic) so they
# match regardless of ų/č/ė encoding. Values are already element-basis mg/L.
PARAM_RULES = [
    (("amonio",), "NH4"),          # amonio azotas          -> NH4 (mg/l N)
    (("nitrat",), "NO3"),          # nitratų azotas         -> NO3 (mg/l N)
    (("nitrit",), "NO2"),          # nitritų azotas         -> NO2 (mg/l N)  [aux]
    (("fosfat",), "PO4"),          # fosfatų fosforas       -> PO4 (mg/l P)
    (("mineralin",), "DIN"),       # azotas mineralinis     -> DIN           [aux]
    (("bendras", "azot"), "TN"),   # azotas bendras         -> TN  (mg/l)
    (("bendras", "fosfor"), "TP"), # bendras fosforas       -> TP  (mg/l)
    (("silic",), "Si"),            # silicis                -> Si  (element, assumed)
    (("biochemin",), "BOD7"),      # BOD7                    -> aux
    (("suspend",), "TSS"),         # suspended solids        -> aux
    (("skendin",), "TSS"),         # skendinčios medžiagos   -> aux
]

# Auxiliary variables cleaned + carried in the tidy table but with no model state
# (never written to a .dates file). NH4/NO3/PO4/Si/TN/TP come from VARIABLES (EPA module).
AUX_UNITS = {"NO2": "mg N/L", "DIN": "mg N/L", "BOD7": "mg O2/L", "TSS": "mg/L"}

TIDY_FIELDS = ["station", "box", "region", "date", "depth", "variable", "model_index",
               "value", "units", "source_file", "orig_param", "orig_unit"]

# Column resolution: logical name -> header substrings (any match wins). Substrings are
# kept short so they also match the MHTML exports, whose headers are truncated by
# `display:none` spans ("Parametro pavadinimas" -> "Parametro pavadin").
COLUMN_KEYS = {
    "station": ["mv kodas"],
    "param":   ["parametro pavadin"],
    "value":   ["parametro tyri", "rezultat"],
    "unit":    ["matavimo vien"],
    "date":    ["data nuo", "tyrimų dat", "tyrimu dat"],
    "depth":   ["gylis", "horizontas"],
}


def map_param(name):
    """Lithuanian parameter name -> canonical var key, or None if unrecognised."""
    if name is None:
        return None
    s = str(name).strip().lower()
    for subs, var in PARAM_RULES:
        if all(x in s for x in subs):
            return var
    return None


def parse_date(raw):
    """Parse a KM date cell ('2018-03-26 00:00:00') to a datetime.date, or None."""
    if raw is None:
        return None
    ts = pd.to_datetime(str(raw), errors="coerce", dayfirst=False)
    if pd.isna(ts):
        return None
    return ts.date()


def _resolve_columns(df):
    """Map logical column names to the actual (Lithuanian) DataFrame columns."""
    lut = {}
    lowered = {str(c).strip().lower(): c for c in df.columns}
    for logical, keys in COLUMN_KEYS.items():
        for want in keys:
            hit = next((orig for low, orig in lowered.items() if want in low), None)
            if hit is not None:
                lut[logical] = hit
                break
    return lut


def _promote_header(df):
    """If the header sits in row 0 (integer columns from read_html), promote it."""
    if any("mv kodas" in str(c).lower() for c in df.columns):
        return df
    hdr = [str(v).strip() for v in df.iloc[0].tolist()]
    if not any("mv kodas" in h.lower() for h in hdr):
        return df
    df = df.iloc[1:].copy()
    df.columns = hdr
    return df.reset_index(drop=True)


def read_tables(path):
    """Load a KM workbook (any real format, detected by magic bytes) as a list of frames.

    xls/xlsx yield one frame; the MHTML "web page" exports yield several (the KM and BJ
    sheets are separate tables). read_html is given thousands=None so the Lithuanian
    decimal comma ("0,15") is NOT stripped to "015" as a thousands separator.
    """
    with open(path, "rb") as fh:
        magic = fh.read(8)
    if magic[:2] == b"PK":                       # zip -> .xlsx
        return [pd.read_excel(path, dtype=str, engine="openpyxl")]
    if magic[:4] == b"\xd0\xcf\x11\xe0":         # OLE2 -> real .xls
        return [pd.read_excel(path, dtype=str, engine="xlrd")]
    # MIME/MHTML export mislabelled .xls: decode every text/html part and keep every
    # table that has an "MV kodas" column (after promoting a first-row header).
    import email
    import io
    with open(path, "rb") as fh:
        msg = email.message_from_binary_file(fh)
    frames = []
    for part in msg.walk():
        if "html" not in part.get_content_type():
            continue
        payload = part.get_payload(decode=True)
        if not payload:
            continue
        try:
            tables = pd.read_html(io.StringIO(payload.decode("utf-8", "replace")),
                                  thousands=None)
        except ValueError:
            continue
        for t in tables:
            t = _promote_header(t)
            if any("mv kodas" in str(c).lower() for c in t.columns):
                frames.append(t)
    return frames


def ingest_file(df, source_file, station_box, stats):
    """Turn one loaded KM DataFrame into tidy rows; mutate stats counters."""
    col = _resolve_columns(df)
    missing = [k for k in ("station", "param", "value", "date") if k not in col]
    if missing:
        stats["skipped_files"].append((source_file, f"missing columns: {missing}"))
        return []
    rows = []
    for _, rec in df.iterrows():
        bare = norm_station(rec[col["station"]])
        if bare is None or bare not in station_box:
            stats["unmapped_stations"][str(rec[col["station"]]).strip()] += 1
            continue
        var = map_param(rec[col["param"]])
        if var is None:
            stats["unmapped_params"][str(rec[col["param"]]).strip()] += 1
            continue
        date = parse_date(rec[col["date"]])
        if date is None or not date_in_range(date):
            stats["bad_dates"] += 1
            continue
        value = clean_value(rec[col["value"]])
        if value != value:  # NaN
            stats["blank_values"] += 1
            continue
        idx, units, _ = VARIABLES.get(var, (None, AUX_UNITS.get(var, ""), ""))
        sb = station_box[bare]
        rows.append({
            "station": bare, "box": sb["box"], "region": sb["region"],
            "date": date.isoformat(),
            "depth": (str(rec[col["depth"]]).strip().replace(",", ".").strip('"')
                      if "depth" in col else ""),
            "variable": var, "model_index": ("" if idx is None else idx),
            "value": value, "units": units,
            "source_file": os.path.basename(source_file),
            "orig_param": str(rec[col["param"]]).strip(),
            "orig_unit": (str(rec[col["unit"]]).strip() if "unit" in col else ""),
        })
        stats["kept"] += 1
    return rows


def write_tidy(rows, path):
    with open(path, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=TIDY_FIELDS)
        w.writeheader()
        for r in sorted(rows, key=lambda r: (r["station"], r["date"], r["variable"])):
            w.writerow({k: r[k] for k in TIDY_FIELDS})


def write_dates(rows, out_dir):
    """One KM_<station>_box<box>.dates per station; depths averaged per date."""
    from collections import defaultdict
    by_sta = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    box_of = {}
    for r in rows:
        idx = r["model_index"]
        if idx == "" or idx is None:
            continue
        by_sta[r["station"]][r["date"]][int(idx)].append(r["value"])
        box_of[r["station"]] = r["box"]
    written = []
    header = "#date      time      " + " ".join(str(i) for i in range(1, DATES_NCOL + 1))
    for station, dates in sorted(by_sta.items()):
        path = os.path.join(out_dir, f"KM_{station}_box{box_of[station]}.dates")
        with open(path, "w") as fh:
            fh.write(header + "\n")
            for date in sorted(dates):
                vals = ["-1"] * DATES_NCOL
                for idx, xs in dates[date].items():
                    vals[idx - 1] = f"{sum(xs) / len(xs):.6g}"
                fh.write(f"{date.replace('-', '')} 000000 " + " ".join(vals) + "\n")
        written.append((os.path.basename(path), len(dates)))
    return written


def print_summary(rows, stats, dates_written):
    print(f"\nKept {stats['kept']} measurements -> {len(rows)} tidy rows")
    by_var = {}
    for r in rows:
        by_var[r["variable"]] = by_var.get(r["variable"], 0) + 1
    print("  by variable:", ", ".join(f"{k}={v}" for k, v in sorted(by_var.items())))
    boxes = sorted({r["box"] for r in rows})
    print(f"  boxes covered ({len(boxes)}): {boxes}")
    if stats["unmapped_stations"]:
        top = sorted(stats["unmapped_stations"].items(), key=lambda x: -x[1])[:8]
        print("  dropped (no lagoon box):",
              ", ".join(f"{s}×{n}" for s, n in top))
    if stats["unmapped_params"]:
        print("  unrecognised parameters:", sorted(stats["unmapped_params"]))
    if stats["skipped_files"]:
        print("  skipped files:", stats["skipped_files"])
    print(f"  blank values: {stats['blank_values']}  out-of-range dates: {stats['bad_dates']}")
    print(f".dates files written: {len(dates_written)}")


def main(argv=None):
    here = os.path.dirname(os.path.abspath(__file__))
    default_dir = os.path.join(
        here, "..", "..", "curonian", "DATA", "2014-2023_BJ duomenys extrahuoti",
        "Hidrocheminiai tyrimai")
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data-dir", default=default_dir,
                   help="folder holding the KM hydrochemistry workbooks")
    p.add_argument("--glob", default="*KM*.xls*",
                   help="filename glob for KM lagoon files (default: '*KM*.xls*')")
    p.add_argument("--stations", default=os.path.join(here, "epa_station_to_box.csv"),
                   help="station->box map (shared with the EPA ingester)")
    p.add_argument("--out-dir", default=here,
                   help="output directory for the tidy CSV + .dates files")
    p.add_argument("--tidy-name", default="km_observations_tidy.csv")
    a = p.parse_args(argv)

    station_box = load_station_box(a.stations)
    from collections import defaultdict
    stats = {"kept": 0, "blank_values": 0, "bad_dates": 0,
             "unmapped_stations": defaultdict(int), "unmapped_params": defaultdict(int),
             "skipped_files": []}
    files = sorted(glob.glob(os.path.join(a.data_dir, a.glob)))
    if not files:
        print(f"No files matched {a.glob!r} in {a.data_dir}", file=sys.stderr)
        return 1
    rows = []
    for f in files:
        try:
            frames = read_tables(f)
        except Exception as e:  # noqa: BLE001 — report and continue over one bad file
            stats["skipped_files"].append((os.path.basename(f), f"read error: {e}"))
            continue
        if not frames:
            stats["skipped_files"].append((os.path.basename(f), "no data tables found"))
            continue
        for df in frames:
            rows.extend(ingest_file(df, f, station_box, stats))

    os.makedirs(a.out_dir, exist_ok=True)
    tidy_path = os.path.join(a.out_dir, a.tidy_name)
    write_tidy(rows, tidy_path)
    dates_written = write_dates(rows, a.out_dir)
    print(f"Wrote {tidy_path}")
    print_summary(rows, stats, dates_written)
    return 0


if __name__ == "__main__":
    sys.exit(main())
