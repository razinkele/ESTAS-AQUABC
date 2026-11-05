#!/usr/bin/env python3
"""Regenerate or extend net/wind_daily.csv (daily-mean 10 m wind at Nida, ERA5).

Two modes:

    python3 tools/eutropy_poc/make_wind_daily.py [path-to-hourly-era5.csv]
        Rebuild from the hourly point CSV (2012-2016, the original source).

    python3 tools/eutropy_poc/make_wind_daily.py --extend-era5 DIR --until YYYY-MM-DD
        Append the days after the file's last day, computed from monthly ERA5
        single-level files DIR/era5_curonian_YYYY-MM_instant.nc (u10, v10) at the
        same grid point as the point CSV, 55.25 N 21.00 E (an exact match of the
        original hourly series). Existing rows are kept byte for byte.

The committed net/wind_daily.csv is authoritative for conversion. The converter
needs it to cover the whole run: a shorter record raises instead of freezing
DIA_C settling at the last wind day.
"""
import argparse
import csv
import datetime as dt
import os
from collections import OrderedDict

DEFAULT_SRC = os.path.expanduser("~/eutropy/input/era5_wind_nida_2012_2016.csv")
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "net", "wind_daily.csv")
BASE = dt.date(2012, 1, 1)      # day 0
LAT, LON = 55.25, 21.0          # ERA5 grid point of the Nida series


def rebuild(src):
    daily = OrderedDict()  # date-string -> list of hourly wind_ms
    with open(src) as fh:
        for row in csv.DictReader(fh):
            day = row["time"][:10]          # 'YYYY-MM-DD'
            daily.setdefault(day, []).append(float(row["wind_ms"]))
    with open(OUT, "w") as fh:
        fh.write("# Daily-mean 10 m wind speed at Nida, 2012-2016 (m/s).\n")
        fh.write("# Source: ERA5 (Copernicus Climate Change Service / ECMWF).\n")
        fh.write("# Contains modified Copernicus Climate Change Service information.\n")
        fh.write("day,wind_ms\n")
        # day index = first-appearance order; assumes the ERA5 source is gapless & chronological (2012-2016 is)
        for i, (day, vals) in enumerate(daily.items()):
            fh.write(f"{i},{sum(vals) / len(vals):.4f}\n")
    print(f"wrote {OUT}: {len(daily)} days (day 0 = {next(iter(daily))})")


def last_day(path):
    """Day index of the last data row of a wind_daily.csv."""
    last = None
    with open(path) as fh:
        for ln in fh:
            ln = ln.strip()
            if ln and not ln.startswith("#") and not ln.lower().startswith("day"):
                last = int(ln.split(",")[0])
    return last


def era5_daily(era5_dir, first, until):
    """Daily-mean wind speed at (LAT, LON) for first..until (dates, inclusive)."""
    import numpy as np
    import xarray as xr
    out = OrderedDict()
    y, m = first.year, first.month
    while (y, m) <= (until.year, until.month):
        path = os.path.join(era5_dir, f"era5_curonian_{y}-{m:02d}_instant.nc")
        if not os.path.exists(path):
            raise SystemExit(f"missing ERA5 month: {path}")
        ds = xr.open_dataset(path).sel(latitude=LAT, longitude=LON)
        speed = np.hypot(ds["u10"].values, ds["v10"].values)
        for t, s in zip(ds["valid_time"].values, speed):
            d = dt.date.fromisoformat(str(t)[:10])
            if first <= d <= until:
                out.setdefault(d, []).append(float(s))
        y, m = (y + 1, 1) if m == 12 else (y, m + 1)
    days = sorted(out)
    expected = (until - first).days + 1
    if len(days) != expected:
        raise SystemExit(f"ERA5 gives {len(days)} days in {first}..{until}, expected {expected}")
    short = [d for d in days if len(out[d]) != 24]
    if short:
        raise SystemExit(f"incomplete hourly data on {len(short)} days, first {short[0]}")
    return [(d, sum(out[d]) / 24.0) for d in days]


def extend(era5_dir, until):
    n = last_day(OUT)
    first = BASE + dt.timedelta(days=n + 1)
    if first > until:
        print(f"{OUT} already reaches {BASE + dt.timedelta(days=n)}; nothing to add")
        return
    rows = era5_daily(era5_dir, first, until)
    with open(OUT, "a") as fh:
        for d, v in rows:
            fh.write(f"{(d - BASE).days},{v:.4f}\n")
    # the first comment line names the period; keep it true
    lines = open(OUT).read().split("\n")
    lines[0] = (f"# Daily-mean 10 m wind speed at Nida, {BASE}..{until} (m/s); days from "
                f"{first} appended from ERA5 monthly fields at {LAT}N {LON}E.")
    open(OUT, "w").write("\n".join(lines))
    print(f"appended {len(rows)} days {first}..{until} to {OUT} (from {era5_dir} at {LAT}N {LON}E)")


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("src", nargs="?", default=DEFAULT_SRC,
                    help="hourly ERA5 point CSV for a full rebuild")
    ap.add_argument("--extend-era5", metavar="DIR",
                    help="append days from monthly ERA5 files instead of rebuilding")
    ap.add_argument("--until", metavar="YYYY-MM-DD", help="last day to append")
    a = ap.parse_args()
    if a.extend_era5:
        if not a.until:
            ap.error("--extend-era5 needs --until")
        extend(a.extend_era5, dt.date.fromisoformat(a.until))
    else:
        rebuild(a.src)


if __name__ == "__main__":
    main()
