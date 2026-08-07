#!/usr/bin/env python3
"""Ingest Curonian Lagoon (KM) phyto- and zooplankton biomass into box-aligned AQUABC obs.

Sources (under ``curonian/DATA``):

  * ``Duomenys Mindaugo/2015 KM Fitoplanktonas_v160203.xlsx`` — the 2015 campaign:
    class-level abundance/biovolume/**carbon** blocks at 7 lagoon stations. Carbon is
    reported directly (mg C/L), already grouped into Diatoms / Non-fixing / Fixing /
    Others columns, so no conversion assumptions are needed. Its ``Ratio`` rows also
    provide the *empirical* carbon:wet-biovolume ratios used to convert the monitoring
    files below.
  * ``2014-2023_BJ duomenys extrahuoti/Biologiniai tyrimai/Fitoplanktonas_KM_2022*.xlsx``
    and ``Fitoplanktonas_KM_BJ_2023*.xls`` — official state monitoring, species-level wet
    biomass (mg/L) at LTK stations (same codes as the EPA archive -> same station->box
    map). Species rows are summed to AQUABC groups per sample, converted to carbon with
    the 2015 empirical ratios, and depth/replicate samples averaged per (station, date).
  * ``.../Zooplanktonas_KM_BJ_2023*.xls`` — species-level zooplankton abundance + wet
    biomass (mg/m3) -> total ZOO_C (mg C/L) with a documented wet->carbon factor.

Only lagoon (KM / LTK) files are ingested; the ``*_BJ_*``-station files cover the Baltic
Sea proper and are out of the CL29 domain (the KM files are picked by their station
codes, not by filename).

Group mapping (obs -> AQUABC state variables):

  DIA  -> DIA_C   (diatom orders)
  CYN  -> CYN_C   (non-fixing cyanobacteria: Chroococcales, Oscillatoriales, ...)
  FIX  -> FIX_CYN_C (heterocystous/N-fixing: Nostocales; NOTE: observations cannot
                     separate the model's FIX_CYN_C from NOST_VEG_HET_C — score this
                     against their SUM, see validate_cl29_vs_epa.py FIX_TOT_C)
  OPA  -> OPA_C   (everything else)
  ZOO  -> ZOO_C

Outputs (mirroring ingest_epa_observations.py / ingest_km_observations.py):

  * ``km_plankton_tidy.csv`` — one row per (station, date, variable) with provenance
    (source file, n samples averaged, wet biomass, C ratio used, station confidence).
  * ``KMP_<station>_box<box>.dates`` — 62-column .dates rows (model units, -1 missing).

Standard library + pandas; xlrd/openpyxl needed at runtime for the workbooks. The unit
tests (tests/python/test_ingest_km_plankton.py) exercise the pure logic only.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import glob
import math
import os
import re
import sys
from collections import defaultdict

import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ingest_epa_observations import DATES_NCOL, load_station_box, norm_station  # noqa: E402

# --- variables emitted ------------------------------------------------------------
# key -> (.dates model index (1-based state var), units, description)
PLANKTON_VARIABLES = {
    "DIA_C":     (5,  "mg C/L", "Diatom carbon"),
    "ZOO_C":     (6,  "mg C/L", "Zooplankton carbon"),
    "CYN_C":     (15, "mg C/L", "Non-fixing cyanobacteria carbon"),
    "OPA_C":     (16, "mg C/L", "Other planktonic algae carbon"),
    "FIX_CYN_C": (19, "mg C/L", "N-fixing (heterocystous) cyanobacteria carbon"),
    # QC / totals only — no single model state variable:
    "PHYTO_TOT_C": (None, "mg C/L", "Total phytoplankton carbon"),
}
GROUP_TO_VAR = {"DIA": "DIA_C", "CYN": "CYN_C", "FIX": "FIX_CYN_C", "OPA": "OPA_C"}

# --- class/taxon -> group ----------------------------------------------------------
DIATOM_ORDERS = {"bacillariales", "fragilariales", "eupodiscales", "bacillariophyceae",
                 "diatomophyceae", "thalassiosirales", "aulacoseirales", "melosirales",
                 "naviculales"}
NONFIX_CYANO = {"chroococcales", "oscillatoriales", "synechococcales"}
FIXING_CYANO = {"nostocales"}
# class names that mean "cyanobacteria" without an order -> split by genus
AMBIGUOUS_CYANO = {"nostocophyceae", "cyanophyceae", "cyanobacteria"}
# heterocystous (potentially N-fixing) genera for the ambiguous-class split
FIXER_GENERA = {"aphanizomenon", "anabaena", "dolichospermum", "anabaenopsis",
                "nodularia", "gloeotrichia", "cylindrospermopsis", "raphidiopsis",
                "cuspidothrix", "chrysosporum", "sphaerospermopsis", "calothrix",
                "rivularia"}


def class_to_group(cls, taxon):
    """Map a (class/order, species) pair from the monitoring files to an AQUABC group."""
    c = str(cls).strip().lower()
    if c in DIATOM_ORDERS:
        return "DIA"
    if c in FIXING_CYANO:
        return "FIX"
    if c in NONFIX_CYANO:
        return "CYN"
    if c in AMBIGUOUS_CYANO:
        genus = str(taxon).strip().split()[0].lower() if str(taxon).strip() else ""
        return "FIX" if genus in FIXER_GENERA else "CYN"
    return "OPA"


# --- carbon conversions --------------------------------------------------------------
# Fallback carbon : wet-biovolume ratios (mg C per mg wet). The real values are computed
# from the 2015 workbook's Ratio rows at ingest time; these fallbacks are that file's
# rounded means (diatoms are low: vacuole + frustule).
DEFAULT_RATIOS = {"DIA": 0.065, "CYN": 0.18, "FIX": 0.16, "OPA": 0.15}

# Zooplankton wet mass -> carbon (mg C per mg wet). Crustacean/rotifer plankton:
# dry ~10 % of wet, C ~50 % of dry  =>  C ~5 % of wet. Overridable via --zoo-c-per-wet.
ZOO_C_PER_WET = 0.05


def zoo_biomass_to_carbon(biomass_mg_m3, c_per_wet=ZOO_C_PER_WET):
    """mg wet / m3  ->  mg C / L."""
    return biomass_mg_m3 * c_per_wet / 1000.0


# --- 2015 station -> box -------------------------------------------------------------
# Georeferenced against curonian/b29polys.gpkg (EPSG:3346 point-in-polygon, 2026-08-07).
# Kiaulės nugara / Nida / Juodkrantė are well-known fixed sites (high confidence, and
# consistent with the EPA LTK mapping: strait box 7, LTK10-box 23, LTK5-box 17).
# Vidmarės / Dreisenynas / Litoralė are campaign site names located from approximate
# coordinates (low confidence). Rusnė is in the delta, outside the lagoon polygons.
STATIONS_2015 = {
    # name -> (box, confidence)
    "Kiaulės nugara": (7, "high"),
    "Nida": (23, "high"),
    "Juodkrantė": (17, "high"),
    "Vidmarės": (14, "low"),
    "Dreisenynas": (19, "low"),
    "Litoralė": (23, "low"),
    "Rusnė": (None, "outside"),   # Nemunas delta — no lagoon box, dropped (logged)
}

MONTHS = {m.lower(): i for i, m in enumerate(
    ["January", "February", "March", "April", "May", "June", "July",
     "August", "September", "October", "November", "December"], start=1)}


def parse_2015_date(text, year):
    """'January 25' -> date(year, 1, 25); None if not parseable."""
    m = re.match(r"^([A-Za-z]+)\s+(\d{1,2})$", str(text).strip())
    if not m or m.group(1).lower() not in MONTHS:
        return None
    try:
        return dt.date(year, MONTHS[m.group(1).lower()], int(m.group(2)))
    except ValueError:
        return None


# 2015 sheet layout (0-based frame indices), shared by the main carbon sheet and the
# per-station 'Nida' sheet: class columns 4..14; col 6 (Cyanobacteria TOTAL) and col 15
# (Total) are aggregates and excluded. Verified against the main sheet's precomputed
# group columns (21-24).
CLASS_COLS_2015 = {"CYN": [4], "FIX": [5], "OPA": [7, 8, 9, 10, 12, 13, 14], "DIA": [11]}
KIND_COL, STATION_COL, DATE_COL, SAMPLE_COL = 3, 1, 0, 2


def blocks_from_2015(frame, year=2015):
    """Extract per-(station, date, sample) group carbon from a 2015-layout block sheet.

    Blocks start at an 'Abundance…' row (which carries the sampling date, station and
    sample type); the block's 'Carbon, mg l-1' row carries per-class carbon in the class
    columns, summed here into AQUABC groups. Blocks whose carbon row is empty are
    skipped (carbon was not computed for every station in the campaign workbook).
    """
    recs = []
    station = date = sample = None
    for _, row in frame.iterrows():
        kind = str(row[KIND_COL]) if not pd.isna(row[KIND_COL]) else ""
        if kind.startswith("Abundance"):
            d = parse_2015_date(row[DATE_COL], year)
            st = None if pd.isna(row[STATION_COL]) else str(row[STATION_COL]).strip()
            station, date = st or station, d
            sample = None if pd.isna(row[SAMPLE_COL]) else str(row[SAMPLE_COL]).strip()
        elif kind.startswith("Carbon") and station and date:
            cells = {g: [row[c] for c in cc] for g, cc in CLASS_COLS_2015.items()}
            if all(pd.isna(v) for vv in cells.values() for v in vv):
                continue   # carbon not measured for this block
            for g, vv in cells.items():
                recs.append({"station": station, "date": date, "sample": sample or "",
                             "group": g,
                             "carbon": sum(0.0 if pd.isna(v) else float(v) for v in vv)})
    return recs


def ratios_from_2015(frame):
    """Mean empirical C:biovolume ratio per group from the sheet's 'Ratio' rows."""
    # class columns: 4 Non-fixing, 5 Fixing, 7..14 others incl. 11 Diatomophyceae
    cols = {"CYN": [4], "FIX": [5], "DIA": [11], "OPA": [7, 8, 9, 10, 12, 13, 14]}
    acc = {g: [] for g in cols}
    for _, row in frame.iterrows():
        if str(row[KIND_COL]).startswith("Ratio"):
            for g, cc in cols.items():
                for c in cc:
                    v = row[c]
                    if not pd.isna(v) and isinstance(v, (int, float)) and 0 < v < 1:
                        acc[g].append(float(v))
    out = dict(DEFAULT_RATIOS)
    for g, xs in acc.items():
        if xs:
            out[g] = sum(xs) / len(xs)
    return out


# --- species-level monitoring files ---------------------------------------------------

def _col(df, *subs):
    """First column whose header contains all substrings (case/NBSP-insensitive)."""
    for c in df.columns:
        h = str(c).replace("\xa0", " ").lower()
        if all(s in h for s in subs):
            return c
    return None


def aggregate_group_carbon(df, ratios):
    """(sample, station, date, cls, taxon, biomass) rows -> {(station, date): {group: mgC/L}}.

    Species wet biomass is summed to groups per sample; a counted sample without a group
    means that group was absent (0.0), so samples are averaged with zero-fill; the mean
    wet biomass is then converted to carbon with the per-group ratio.
    """
    per_sample = defaultdict(lambda: defaultdict(float))   # (st,date,sample) -> group -> wet
    for _, r in df.iterrows():
        b = r["biomass"]
        if pd.isna(b):
            continue
        g = class_to_group(r["cls"], r["taxon"])
        per_sample[(r["station"], r["date"], r["sample"])][g] += float(b)
    by_sd = defaultdict(list)
    for (st, d, _), groups in per_sample.items():
        by_sd[(st, d)].append(groups)
    out = {}
    for key, samples in by_sd.items():
        out[key] = {g: sum(s.get(g, 0.0) for s in samples) / len(samples) * ratios[g]
                    for g in ("DIA", "CYN", "FIX", "OPA")}
    return out


def load_species_phyto(path):
    """Monitoring phyto workbook -> normalized frame (sample, station, date, cls, taxon, biomass)."""
    eng = "xlrd" if path.lower().endswith(".xls") else None
    df = pd.ExcelFile(path, engine=eng).parse(0)
    cols = {"sample": _col(df, "mėginio numeris"), "date": _col(df, "paėmimo"),
            "station": _col(df, "mv kodas"), "cls": _col(df, "individų klas"),
            "taxon": _col(df, "taksonas"), "biomass": _col(df, "biomas")}
    missing = [k for k, v in cols.items() if v is None]
    if missing:
        raise ValueError(f"{os.path.basename(path)}: missing columns {missing}")
    out = pd.DataFrame({k: df[v] for k, v in cols.items()})
    out["date"] = pd.to_datetime(out["date"], errors="coerce").dt.date
    out["biomass"] = pd.to_numeric(out["biomass"], errors="coerce")
    out["station"] = out["station"].astype(str).str.strip()
    return out.dropna(subset=["date"])


def load_species_zoo(path):
    eng = "xlrd" if path.lower().endswith(".xls") else None
    df = pd.ExcelFile(path, engine=eng).parse(0)
    cols = {"sample": _col(df, "mėginio numeris"), "date": _col(df, "paėmimo"),
            "station": _col(df, "mv kodas"), "biomass": _col(df, "biomas")}
    missing = [k for k, v in cols.items() if v is None]
    if missing:
        raise ValueError(f"{os.path.basename(path)}: missing columns {missing}")
    out = pd.DataFrame({k: df[v] for k, v in cols.items()})
    out["date"] = pd.to_datetime(out["date"], errors="coerce").dt.date
    out["biomass"] = pd.to_numeric(out["biomass"], errors="coerce")
    out["station"] = out["station"].astype(str).str.strip()
    return out.dropna(subset=["date"])


def pick_latest(paths):
    """Among duplicate 'patikrinta_<date>' exports, keep the latest verification."""
    def key(p):
        m = re.search(r"patikrint\w*_(\d{4}-\d{2}-\d{2})", os.path.basename(p))
        return m.group(1) if m else "0000-00-00"
    return max(paths, key=key) if paths else None


# --- outputs -------------------------------------------------------------------------

TIDY_FIELDS = ["station", "box", "region", "date", "variable", "model_index", "value",
               "units", "n_samples", "wet_biomass", "c_ratio", "confidence", "source_file"]


def write_tidy(rows, out_dir):
    path = os.path.join(out_dir, "km_plankton_tidy.csv")
    with open(path, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=TIDY_FIELDS)
        w.writeheader()
        w.writerows(sorted(rows, key=lambda r: (r["station"], r["date"], r["variable"])))
    return path


def write_dates(rows, out_dir):
    """One KMP_<station>_box<box>.dates per station (same format as the EPA ingester)."""
    by_sta = defaultdict(lambda: defaultdict(dict))
    meta = {}
    for r in rows:
        if not r["model_index"]:
            continue
        by_sta[r["station"]][r["date"]][int(r["model_index"])] = r["value"]
        meta[r["station"]] = r["box"]
    header = "#date      time      " + " ".join(str(i) for i in range(1, DATES_NCOL + 1))
    written = []
    for station, dates in sorted(by_sta.items()):
        safe = re.sub(r"\W+", "_", station).strip("_")
        path = os.path.join(out_dir, f"KMP_{safe}_box{meta[station]}.dates")
        with open(path, "w") as fh:
            fh.write(header + "\n")
            for date in sorted(dates):
                vals = ["-1"] * DATES_NCOL
                for idx, v in dates[date].items():
                    vals[idx - 1] = f"{v:.6g}"
                fh.write(f"{str(date).replace('-', '')} 000000 " + " ".join(vals) + "\n")
        written.append((os.path.basename(path), len(dates)))
    return written


def emit_rows(carbon_by_sd, sta_meta, ratios, wet_by_sd, nsamp_by_sd, source, rows):
    """Append tidy rows for one source's {(station, date): {group: carbon}} result."""
    for (st, d), groups in carbon_by_sd.items():
        box, region, conf = sta_meta(st)
        if box is None:
            continue
        tot = 0.0
        for g, c in groups.items():
            var = GROUP_TO_VAR[g]
            tot += c
            rows.append({
                "station": st, "box": box, "region": region, "date": str(d),
                "variable": var, "model_index": PLANKTON_VARIABLES[var][0] or "",
                "value": round(c, 6), "units": "mg C/L",
                "n_samples": nsamp_by_sd.get((st, d), ""),
                "wet_biomass": wet_by_sd.get((st, d, g), ""),
                "c_ratio": round(ratios[g], 4) if ratios else "",
                "confidence": conf, "source_file": source,
            })
        rows.append({
            "station": st, "box": box, "region": region, "date": str(d),
            "variable": "PHYTO_TOT_C", "model_index": "",
            "value": round(tot, 6), "units": "mg C/L",
            "n_samples": nsamp_by_sd.get((st, d), ""), "wet_biomass": "",
            "c_ratio": "", "confidence": conf, "source_file": source,
        })


def main(argv=None):
    here = os.path.dirname(os.path.abspath(__file__))
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data-root", default=os.path.expanduser("~/curonian/DATA"),
                   help="curonian DATA folder")
    p.add_argument("--out", default=os.path.join(here, "..", "km_plankton_out"),
                   help="output folder")
    p.add_argument("--epa-map", default=os.path.join(here, "epa_station_to_box.csv"),
                   help="LTK station->box map (EPA ingester CSV)")
    p.add_argument("--zoo-c-per-wet", type=float, default=ZOO_C_PER_WET,
                   help="zooplankton carbon per wet mass (default %(default)s)")
    p.add_argument("--strict", action="store_true",
                   help="drop low-confidence 2015 campaign stations")
    a = p.parse_args(argv)

    bio = os.path.join(a.data_root, "2014-2023_BJ duomenys extrahuoti", "Biologiniai tyrimai")
    f2015 = os.path.join(a.data_root, "Duomenys Mindaugo", "2015 KM Fitoplanktonas_v160203.xlsx")
    f2022 = pick_latest(glob.glob(os.path.join(bio, "Fitoplanktonas_KM_2022*")))
    f2023 = pick_latest(glob.glob(os.path.join(bio, "Fitoplanktonas_KM_BJ_2023*")))
    fzoo = pick_latest(glob.glob(os.path.join(bio, "Zooplanktonas_KM*")))

    ltk_map = load_station_box(a.epa_map)   # station -> (box, region)

    def ltk_meta(st):
        info = ltk_map.get(norm_station(st))
        if not info:
            print(f"  ! station {st} not in the LTK->box map, dropped")
            return None, "", ""
        return info["box"], info["region"], "high"

    def sta2015_meta(st):
        box, conf = STATIONS_2015.get(st, (None, "unknown"))
        if box is None or (a.strict and conf == "low"):
            return None, "", conf
        return box, "campaign-2015", conf

    rows = []

    # 1) 2015 campaign (carbon measured directly)
    ratios = dict(DEFAULT_RATIOS)
    if os.path.exists(f2015):
        xl2015 = pd.ExcelFile(f2015)
        frame = xl2015.parse("2015 Fito_Gaus_Biom_Carbon", header=None)
        ratios = ratios_from_2015(frame)
        recs = blocks_from_2015(frame)
        # the 'Nida' per-station sheet carries carbon absent from the main sheet
        seen = {(r["station"], r["date"], r["sample"], r["group"]) for r in recs}
        if "Nida" in xl2015.sheet_names:
            for r in blocks_from_2015(xl2015.parse("Nida", header=None)):
                if (r["station"], r["date"], r["sample"], r["group"]) not in seen:
                    recs.append(r)
        per_sd = defaultdict(lambda: defaultdict(list))
        for r in recs:
            per_sd[(r["station"], r["date"])][r["group"]].append(r["carbon"])
        carbon = {k: {g: sum(v) / len(v) for g, v in gg.items()} for k, gg in per_sd.items()}
        nsamp = {k: max(len(v) for v in gg.values()) for k, gg in per_sd.items()}
        dropped = sorted({st for (st, _) in carbon if STATIONS_2015.get(st, (None,))[0] is None})
        emit_rows(carbon, sta2015_meta, None, {}, nsamp, os.path.basename(f2015), rows)
        print(f"2015 campaign: {len(carbon)} station-dates, ratios {ratios}"
              + (f", dropped (no box): {dropped}" if dropped else ""))
    else:
        print(f"! 2015 workbook not found ({f2015}), using fallback ratios")

    # 2) monitoring phytoplankton (wet biomass -> carbon via the 2015 ratios)
    for f in (f2022, f2023):
        if not f:
            continue
        df = load_species_phyto(f)
        carbon = aggregate_group_carbon(df, ratios)
        nsamp = df.groupby(["station", "date"])["sample"].nunique().to_dict()
        wet = {}
        emit_rows(carbon, ltk_meta, ratios, wet, nsamp, os.path.basename(f), rows)
        d = sorted({x for (_, x) in carbon})
        print(f"{os.path.basename(f)}: {len(carbon)} station-dates ({d[0]}..{d[-1]})")

    # 3) monitoring zooplankton -> ZOO_C
    if fzoo:
        df = load_species_zoo(fzoo)
        per_sample = df.groupby(["station", "date", "sample"])["biomass"].sum()
        per_sd = per_sample.groupby(level=[0, 1]).mean()
        n_sd = per_sample.groupby(level=[0, 1]).size()
        for (st, d), wet in per_sd.items():
            if math.isnan(wet):
                continue
            box, region, conf = ltk_meta(st)
            if box is None:
                continue
            rows.append({
                "station": st, "box": box, "region": region, "date": str(d),
                "variable": "ZOO_C", "model_index": PLANKTON_VARIABLES["ZOO_C"][0],
                "value": round(zoo_biomass_to_carbon(wet, a.zoo_c_per_wet), 6),
                "units": "mg C/L", "n_samples": int(n_sd[(st, d)]),
                "wet_biomass": round(wet, 3), "c_ratio": a.zoo_c_per_wet,
                "confidence": conf, "source_file": os.path.basename(fzoo),
            })
        print(f"{os.path.basename(fzoo)}: {len(per_sd)} station-dates -> ZOO_C")

    os.makedirs(a.out, exist_ok=True)
    tidy = write_tidy(rows, a.out)
    dates = write_dates(rows, a.out)
    nvar = defaultdict(int)
    for r in rows:
        nvar[r["variable"]] += 1
    print(f"\nWrote {tidy} ({len(rows)} rows): "
          + ", ".join(f"{k}={v}" for k, v in sorted(nvar.items())))
    for name, n in dates:
        print(f"  {name}: {n} dates")
    return 0


if __name__ == "__main__":
    sys.exit(main())
