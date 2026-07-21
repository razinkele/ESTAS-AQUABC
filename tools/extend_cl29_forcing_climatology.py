#!/usr/bin/env python3
"""Extend the CL29 forcing one more year with a day-of-year climatology.

The EUTROPY-derived CL29 forcing (flows + boundary concentrations + physical drivers)
ends at day 4017 = 2022-12-31, so the model cannot be run into 2023 with real forcing.
This tool appends a **synthetic 2023** to every time-series input by, for each daily series
and each variable, filling each 2023 day with the 2012-2022 **mean seasonal cycle** for that
calendar day-of-year.

IMPORTANT: the result is NOT a hindcast. A climatological year carries the mean seasonal
cycle but none of 2023's inter-annual anomalies (river discharge, boundary loads, weather).
It lets CL29 reach 2023 so the 2023 monitoring can be scored against the model's typical-year
response — useful for seasonal structure, not for anomaly attribution.

Handling per input file (detected, not hard-coded):
  * a daily series (many rows, e.g. FLOW/FORC/TEMP/SALT/SOLAR_RAD) -> climatology-extended;
  * a short constant series (<= CONST_MAX rows, e.g. AIR_TEMP/WIND/ICE) -> one row appended
    at the end day holding the last value;
  * a series already covering the end day (e.g. SETTLING_VELOCITY at t=9999) -> copied;
  * a non-time-series file (BATHYMETRY, WCONST, INIT_CONC, options) -> copied verbatim.

Standard library only.
"""
from __future__ import annotations

import argparse
import os
import shutil
from collections import defaultdict
from datetime import date, timedelta

CONST_MAX = 8   # <= this many data rows => treat as a constant/sparse series (hold last)


def _doy(base, d):
    return (base + timedelta(days=int(round(d)))).timetuple().tm_yday


def parse_ts(path):
    """Return (header_lines, data_rows) if `path` is an ESTAS time series, else None."""
    lines = open(path).read().splitlines()
    tv = next((i for i, ln in enumerate(lines) if "TIME AND VALUES" in ln), None)
    if tv is None or not any("DATA_SIZE" in ln for ln in lines):
        return None
    header = lines[:tv + 1]
    rows = []
    for ln in lines[tv + 1:]:
        s = ln.strip()
        if not s or s.startswith("#"):
            continue
        rows.append([float(x) for x in s.split()])
    return header, rows


def set_data_size(header, n):
    """Return header with the value line after '# DATA_SIZE' replaced by n."""
    fixed = []
    replace_next = False
    for ln in header:
        if replace_next:
            fixed.append(str(n))
            replace_next = False
            continue
        fixed.append(ln)
        if ln.strip().startswith("# DATA_SIZE"):
            replace_next = True
    return fixed


def fmt_row(row):
    return " ".join(f"{v:.6f}" for v in row)


def extend_series(header, rows, base, end_day):
    """Return (new_header, all_rows) extended to end_day; None if already covered."""
    last_t = max(r[0] for r in rows)
    if last_t >= end_day:
        return None                          # already covers the target
    nvar = len(rows[0]) - 1
    last_day = int(round(last_t))
    if len(rows) <= CONST_MAX:               # constant/sparse -> hold last value
        new = [[float(end_day)] + rows[-1][1:]]
    else:                                    # daily series -> day-of-year climatology
        clim = [defaultdict(list) for _ in range(nvar)]
        for r in rows:
            k = _doy(base, r[0])
            for v in range(nvar):
                clim[v][k].append(r[1 + v])
        mean = [{k: sum(xs) / len(xs) for k, xs in clim[v].items()} for v in range(nvar)]
        gmean = [sum(sum(xs) for xs in clim[v].values())
                 / sum(len(xs) for xs in clim[v].values()) for v in range(nvar)]
        new = []
        for d in range(last_day + 1, end_day + 1):
            k = _doy(base, d)
            new.append([float(d)] + [mean[v].get(k, gmean[v]) for v in range(nvar)])
    return set_data_size(header, len(rows) + len(new)), rows + new


def main(argv=None):
    here = os.path.dirname(os.path.abspath(__file__))
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--in-dir", default=os.path.join(here, "..", "INPUTS_CL29"))
    p.add_argument("--out-dir", default=os.path.join(here, "..", "INPUTS_CL29_2023clim"))
    p.add_argument("--end-day", type=int, default=4382,   # 2023-12-31
                   help="last forcing day to generate (default 4382 = 2023-12-31)")
    p.add_argument("--base-date", default="2012-01-01")
    a = p.parse_args(argv)
    base = date.fromisoformat(a.base_date)
    os.makedirs(a.out_dir, exist_ok=True)

    extended, held, copied = [], [], 0
    for name in sorted(os.listdir(a.in_dir)):
        src = os.path.join(a.in_dir, name)
        dst = os.path.join(a.out_dir, name)
        if not os.path.isfile(src):
            continue
        parsed = parse_ts(src)
        result = extend_series(*parsed, base, a.end_day) if parsed else None
        if result is None:
            shutil.copy2(src, dst)
            copied += 1
            continue
        new_header, all_rows = result
        with open(dst, "w") as fh:
            fh.write("\n".join(new_header) + "\n")
            for r in all_rows:
                fh.write(fmt_row(r) + "\n")
        (held if len(parse_ts(src)[1]) <= CONST_MAX else extended).append(name)

    print(f"climatology-extended ({len(extended)}): {extended}")
    print(f"held-constant ({len(held)}): {held}")
    print(f"copied verbatim: {copied} files")
    print(f"-> {a.out_dir} (forcing now runs to day {a.end_day})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
