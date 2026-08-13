#!/usr/bin/env python3
"""Build the CL29 daily wind forcing (WIND_SPEED_TS.txt) from ERA5, validated
against the KM/BJ hydrometeorology spot observations.

Why this exists: the shipped WIND_SPEED_TS.txt is a 2-point constant 4.0 m/s
placeholder. The cyanobacteria surface-positioning light mechanism (all three
_BOUYANT libraries) gates on MIX_DEPTH = 0.8121*W + 0.7006 vs the euphotic
depth, so under constant 4 m/s (MIX = 3.95 m) it has never engaged. Wind also
drives KAWIND reaeration and the wind-settling suppression, so replacing the
placeholder is a model-wide forcing change (see the 2026-08-13 plan).

Sources
  * ERA5 hourly 10 m wind, one 0.25-degree cell over Nida: era5_wind_nida_YYYY.nc
    (u10, v10), fetched by eutropy/scripts/fetch_era5_wind.py. Daily mean of the
    hourly speed (NOT the speed of the daily-mean vector, which under-counts).
  * Validation: the state-monitoring hydrometeorology workbooks carry per-visit
    'vejo greitis' (wind speed, m/s) readings with a timestamp at Curonian
    stations -- matched against the ERA5 hour, they are an independent in-lagoon
    spot check (visit readings, ~monthly; a check, never a forcing source).

Output format is byte-compatible with the ESTAS time-series reader (header as in
the shipped file; INTERPOLATE 1; "t value" rows, t in days from BASE_DATE).
"""
from __future__ import annotations

import argparse
import glob
import math
import os
import re
import unicodedata

BASE_DATE = "2012-01-01"
N_DAYS = 4017          # 2012-01-01 .. 2022-12-31 inclusive


def daily_speed_from_era5(raw_dir, years):
    """ERA5 yearly NetCDFs -> {date -> daily mean of hourly wind speed}."""
    import numpy as np  # lazy: keep module importable without the stack
    import pandas as pd
    import xarray as xr

    out = {}
    for year in years:
        path = os.path.join(raw_dir, f"era5_wind_nida_{year}.nc")
        if not os.path.exists(path):
            raise FileNotFoundError(f"missing {path} - fetch it first "
                                    f"(eutropy/scripts/fetch_era5_wind.py)")
        ds = xr.open_dataset(path)
        tdim = "valid_time" if "valid_time" in ds.dims else "time"
        speed = np.sqrt(ds["u10"] ** 2 + ds["v10"] ** 2)
        # average away any residual lat/lon dims (single cell -> no-op)
        for d in [d for d in speed.dims if d != tdim]:
            speed = speed.mean(dim=d)
        s = speed.to_pandas()
        daily = s.groupby(pd.to_datetime(s.index).date).mean()
        out.update({d: float(v) for d, v in daily.items()})
        ds.close()
    return out


def _norm(s):
    """Lowercase, accent-stripped, whitespace-collapsed (Lithuanian headers)."""
    s = unicodedata.normalize("NFKD", str(s))
    s = "".join(c for c in s if not unicodedata.combining(c))
    return re.sub(r"\s+", " ", s).lower().strip()


def spot_winds_from_workbook(path):
    """One hydrometeo workbook -> list of (timestamp, station, speed m/s).

    Layouts vary by year (sheet names, column titles); rows are parameter-per-row
    with a 'vejo greitis' name, a decimal-comma result and an m/s unit. Only
    Curonian rows are kept ('Kursiu' waterbody or LTK station codes).
    """
    import pandas as pd

    rows = []
    xl = pd.ExcelFile(path)
    for sheet in xl.sheet_names:
        if not re.search(r"meteo", _norm(sheet)):
            continue
        df = xl.parse(sheet)
        cols = {_norm(c): c for c in df.columns}
        pcol = next((v for k, v in cols.items() if "parametro pavadinim" in k), None)
        rcol = next((v for k, v in cols.items() if "rezultat" in k), None)
        dcol = next((v for k, v in cols.items() if "data" in k), None)
        scol = next((v for k, v in cols.items() if "mv kodas" in k), None)
        wcol = next((v for k, v in cols.items() if "telkinio pavadinim" in k), None)
        if not (pcol and rcol and dcol):
            continue
        m = df[pcol].map(_norm).str.contains("vejo greitis", na=False)
        if wcol is not None or scol is not None:
            cur = False
            if wcol is not None:
                cur = df[wcol].map(_norm).str.contains("kursiu", na=False)
            if scol is not None:
                cur = cur | df[scol].astype(str).str.startswith(("LTK", "LT"))
            m = m & cur
        for _, r in df[m].iterrows():
            t = pd.to_datetime(r[dcol], errors="coerce")
            v = str(r[rcol]).replace(",", ".")
            try:
                v = float(v)
            except ValueError:
                continue
            if pd.notna(t) and 0.0 <= v <= 40.0:
                rows.append((t, str(r[scol]) if scol is not None else "", v))
    return rows


def validate_against_spots(daily, hydro_dir):
    """Compare the ERA5 daily series with the in-lagoon spot readings.

    Spot readings are instantaneous; the forcing is a daily mean -- expect
    correlation well below 1 even for a perfect reanalysis. This is a sanity
    check on level and variability, not a fit target.
    """
    import numpy as np

    pairs = []
    for path in sorted(glob.glob(os.path.join(hydro_dir, "*.xls*"))):
        try:
            for t, _st, v in spot_winds_from_workbook(path):
                d = t.date()
                if d in daily:
                    pairs.append((daily[d], v))
        except Exception as exc:                      # noqa: BLE001 - report and move on
            print(f"  ! {os.path.basename(path)}: {exc}")
    if len(pairs) < 10:
        print(f"  validation: only {len(pairs)} matched spot readings - skipped")
        return None
    a = np.array(pairs)
    r = float(np.corrcoef(a[:, 0], a[:, 1])[0, 1])
    bias = float((a[:, 0] - a[:, 1]).mean())
    print(f"  validation vs {len(pairs)} in-lagoon spot readings: "
          f"r = {r:.2f}, ERA5-minus-spot bias = {bias:+.2f} m/s "
          f"(spot = instantaneous, series = daily mean)")
    return r


def write_ts(daily, out_path, base_date=BASE_DATE, n_days=N_DAYS):
    """Write the ESTAS WIND_SPEED_TS.txt (gap-fill by linear interpolation)."""
    import datetime as dt

    base = dt.date.fromisoformat(base_date)
    days = [base + dt.timedelta(days=i) for i in range(n_days)]
    vals, gaps = [], 0
    for d in days:
        vals.append(daily.get(d, math.nan))
    # linear gap fill (short gaps only are expected; count them honestly)
    for i, v in enumerate(vals):
        if math.isnan(v):
            gaps += 1
            lo = next((j for j in range(i - 1, -1, -1) if not math.isnan(vals[j])), None)
            hi = next((j for j in range(i + 1, len(vals)) if not math.isnan(vals[j])), None)
            if lo is None or hi is None:
                raise SystemExit(f"unfillable gap at {days[i]} - refuse to extrapolate")
            w = (i - lo) / (hi - lo)
            vals[i] = vals[lo] * (1 - w) + vals[hi] * w
    with open(out_path, "w") as f:
        f.write("# WIND_SPEED_TS\n# DATA_SIZE\n")
        f.write(f"{len(vals)}\n")
        f.write("# NUMBER_OF_VARIABLES\n1\n")
        f.write("# SCALE FACTORS\n#\n          1.00000000\n")
        f.write("# UNIT CONVERSION FACTORS\n#\n          1.00000000\n")
        f.write("# INTERPOLATE (1=yes)\n1\n")
        f.write("# TIME AND VALUES\n")
        for i, v in enumerate(vals):
            f.write(f"{float(i):.6f} {v:.6f}\n")
    return gaps


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--era5-dir", default=os.path.expanduser("~/eutropy/era5_raw"))
    p.add_argument("--years", default="2012-2022",
                   help="inclusive range, e.g. 2012-2022")
    p.add_argument("--hydro-dir", default=os.path.expanduser(
        "~/curonian/DATA/2014-2023_BJ duomenys extrahuoti/Hidrometeorologiniai matavimai"))
    p.add_argument("--out", default="WIND_SPEED_TS.txt")
    a = p.parse_args(argv)

    y0, y1 = (int(x) for x in a.years.split("-"))
    daily = daily_speed_from_era5(a.era5_dir, range(y0, y1 + 1))
    print(f"ERA5 daily series: {len(daily)} days ({min(daily)}..{max(daily)})")


    import numpy as np
    jun_sep = [v for d, v in daily.items() if d.month in (6, 7, 8, 9)]
    q = np.percentile(jun_sep, [10, 25, 50, 75, 90])
    print(f"Jun-Sep wind (m/s): p10 {q[0]:.1f}  p25 {q[1]:.1f}  p50 {q[2]:.1f}  "
          f"p75 {q[3]:.1f}  p90 {q[4]:.1f}   mean {np.mean(jun_sep):.1f}")
    calm = np.mean(np.array(jun_sep) <= 2.5)
    print(f"Jun-Sep calm days (<= 2.5 m/s, positioning-eligible under "
          f"transparent optics): {100 * calm:.0f} %")

    if os.path.isdir(a.hydro_dir):
        validate_against_spots(daily, a.hydro_dir)

    gaps = write_ts(daily, a.out)
    print(f"wrote {a.out} ({N_DAYS} rows, {gaps} gap-filled days)")


if __name__ == "__main__":
    main()
