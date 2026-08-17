#!/usr/bin/env python3
"""Build symmetric open-boundary phytoplankton climatologies for CL29.

Why this exists (docs/CL29_phenology_diagnosis.md par. 24): the shipped boundary
forcing gives CYN a seasonal series while DIA, OPA and FIX_CYN each receive a
flat 0.020 mg C/L placeholder in every month of every year. In a
transport-dominated lagoon that asymmetry is decisive -- it is the measured
cause of OPA's permanent extinction, it makes part of the model's CYN skill
circular, and (par. 25) it is the leading explanation for growth constants that
will not retreat from their bounds.

The fix is data, applied symmetrically: all four groups get a monthly
climatology derived by ONE method from ONE source -- the state-monitoring
Baltic phytoplankton archive (`Fitoplanktonas_BJ_*`), restricted to the
Curonian plume zone (LT3/LT4/LT5, waterbody "Kursiu mariu vandenu isplitimo
Baltijos juroje zona"), which is the water mass exchanged through the Klaipeda
strait. Species-level wet biomass is mapped to model groups with the same
class/genus rules and the same C:biovolume ratios as the in-lagoon ingester
(tools/ingest_km_plankton.py), so boundary and interior observations are
commensurable.

Caveats, deliberately explicit:
  * the plume zone carries outflowing lagoon water as well as inflowing Baltic
    water, so these values are an upper bound on the "pure Baltic" signal;
    --stations lets the territorial-sea set (LT1B/LT2/LT20) be used instead as
    a sensitivity test.
  * the archive covers 2018-2023 while the model runs 2012-2022, so the product
    is a climatology repeated annually -- exactly the form the shipped CYN
    series already has (verified: its years are identical).
  * months with no samples are filled by circular interpolation between the
    neighbouring sampled months.
"""
from __future__ import annotations

import argparse
import glob
import os
import sys
import warnings

warnings.filterwarnings("ignore")
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from build_wind_forcing import _norm  # noqa: E402  accent-stripping header matcher
from ingest_km_plankton import DEFAULT_RATIOS, class_to_group  # noqa: E402

PLUME = ["LT3", "LT4", "LT5"]              # Curonian plume zone in the Baltic
TERRITORIAL = ["LT1B", "LT2", "LT20"]      # open territorial sea (sensitivity)
# model state-variable numbers of the boundary columns we rewrite
STATE_VAR = {"DIA": 5, "CYN": 15, "OPA": 16, "FIX": 19}
FLOOR = 0.001                              # mg C/L, keeps a seed everywhere


def load_bj_phyto(data_root):
    """All Fitoplanktonas_BJ_* workbooks -> tidy rows with model groups + carbon."""
    import numpy as np
    import pandas as pd

    frames = []
    pattern = os.path.join(data_root, "**", "Fitoplanktonas_BJ*")
    for f in glob.glob(os.path.expanduser(pattern), recursive=True):
        try:
            df = pd.ExcelFile(f).parse(0)
        except Exception as exc:                       # noqa: BLE001 - report, continue
            print(f"  ! {os.path.basename(f)}: {type(exc).__name__}")
            continue
        cols = {_norm(c): c for c in df.columns}

        def pick(*keys, _cols=cols):
            return next((v for k, v in _cols.items() if any(x in k for x in keys)), None)

        st, dt, cl, bm = pick("mv kodas"), pick("emimo"), pick("individu klas"), pick("biomas")
        tx = pick("taksonas")
        if not all([st, dt, cl, bm]):
            print(f"  ! {os.path.basename(f)}: missing columns, skipped")
            continue
        frames.append(pd.DataFrame({
            "st": df[st].astype(str),
            "date": pd.to_datetime(df[dt], errors="coerce"),
            "cls": df[cl].astype(str),
            "tx": df[tx].astype(str) if tx is not None else "",
            "bm": pd.to_numeric(df[bm].astype(str).str.replace(",", "."), errors="coerce"),
        }).dropna(subset=["bm"]))

    a = pd.concat(frames, ignore_index=True)
    # dtypes differ across workbooks -> object column after concat; re-coerce
    a["date"] = pd.to_datetime(a["date"], errors="coerce", utc=True).dt.tz_localize(None)
    a = a.dropna(subset=["date"])
    a["grp"] = [class_to_group(c, t) for c, t in zip(a.cls, a.tx)]
    a["C"] = [b * DEFAULT_RATIOS[g] if g in DEFAULT_RATIOS else np.nan
              for g, b in zip(a.grp, a.bm)]
    return a


def monthly_climatology(rows, stations):
    """Tidy rows -> {group: [12 monthly means]}, circularly gap-filled."""
    import pandas as pd

    sub = rows[rows.st.isin(stations)].dropna(subset=["C"])
    per = sub.groupby(["st", "date", "grp"])["C"].sum().reset_index()
    per["m"] = per.date.dt.month
    table = per.groupby(["m", "grp"])["C"].mean().unstack()
    out = {}
    for grp in STATE_VAR:
        col = table[grp] if grp in table else pd.Series(dtype=float)
        vals = [col.get(m, float("nan")) for m in range(1, 13)]
        # circular fill: nearest sampled months either way
        for i in range(12):
            if pd.isna(vals[i]):
                lo = next((j for j in range(1, 12)
                           if not pd.isna(vals[(i - j) % 12])), None)
                hi = next((j for j in range(1, 12)
                           if not pd.isna(vals[(i + j) % 12])), None)
                if lo is None or hi is None:
                    vals[i] = FLOOR
                else:
                    a_, b_ = vals[(i - lo) % 12], vals[(i + hi) % 12]
                    vals[i] = (a_ * hi + b_ * lo) / (lo + hi)
        out[grp] = [max(float(v), FLOOR) for v in vals]
    return out, len(per)


def rewrite_forcing(src, dst, clim, base_year=2012):
    """Rewrite FORC_TS_1: replace the four phyto columns with the climatology."""
    import datetime as dt

    header, data = [], []
    for line in open(src):
        (header if line.startswith("#") or len(line.split()) < 5 else data).append(line)
    base = dt.date(base_year, 1, 1)
    out, changed = [], 0
    for line in data:
        parts = line.split()
        t = float(parts[0])
        month = (base + dt.timedelta(days=t)).month
        for grp, sv in STATE_VAR.items():
            parts[sv] = f"{clim[grp][month - 1]:.6f}"
        changed += 1
        out.append(" ".join(parts) + "\n")
    # interleave preserved header lines in their original positions
    written, di = [], 0
    for line in open(src):
        if line.startswith("#") or len(line.split()) < 5:
            written.append(line)
        else:
            written.append(out[di])
            di += 1
    open(dst, "w").writelines(written)
    return changed


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data-root", default=os.path.expanduser("~/curonian/DATA"))
    p.add_argument("--stations", default="plume", choices=["plume", "territorial"])
    p.add_argument("--src", default="INPUTS_CL29/FORC_TS_1.txt")
    p.add_argument("--out", default="FORC_TS_1_symmetric.txt")
    a = p.parse_args(argv)

    rows = load_bj_phyto(a.data_root)
    print(f"BJ phytoplankton: {len(rows)} rows, "
          f"{rows.date.dt.year.min()}-{rows.date.dt.year.max()}")
    stations = PLUME if a.stations == "plume" else TERRITORIAL
    clim, n = monthly_climatology(rows, stations)
    print(f"{a.stations} stations {stations}: {n} station-date-group samples\n")
    print(f"{'month':>6}" + "".join(f"{g:>10}" for g in STATE_VAR))
    for m in range(12):
        print(f"{m + 1:>6}" + "".join(f"{clim[g][m]:>10.4f}" for g in STATE_VAR))
    n_rows = rewrite_forcing(a.src, a.out, clim)
    print(f"\nwrote {a.out} ({n_rows} rows; state vars "
          f"{sorted(STATE_VAR.values())} replaced)")


if __name__ == "__main__":
    main()
