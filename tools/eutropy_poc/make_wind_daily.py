#!/usr/bin/env python3
"""Regenerate net/wind_daily.csv (daily-mean 10 m wind) from hourly ERA5.

Source: ERA5 (Copernicus Climate Change Service / ECMWF), hourly wind_ms at Nida,
2012-2016. The committed net/wind_daily.csv is authoritative for conversion; this
script only rebuilds it. Usage:
    python3 tools/eutropy_poc/make_wind_daily.py [path-to-hourly-era5.csv]
"""
import csv
import os
import sys
from collections import OrderedDict

DEFAULT_SRC = os.path.expanduser("~/eutropy/input/era5_wind_nida_2012_2016.csv")
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "net", "wind_daily.csv")


def main():
    src = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_SRC
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


if __name__ == "__main__":
    main()
