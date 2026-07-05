#!/usr/bin/env python3
"""Compare the AQUABC box-19 PoC run against EUTROPY and the observations.

All three series are put on a common axis: chlorophyll-a (ug/L).
  * AQUABC   : DIA_C (mg C/L) / DIA_C_TO_CHLA * 1000
  * EUTROPY  : Cpy   (mg C/L) / a_C_chl       * 1000
  * observed : chlorophyll-a directly (ug/L)

Produces tools/eutropy_poc/box19/comparison_chla.png and prints skill stats
(Pearson r, RMSE, bias) of each model against the box-19 observations.

Run from the repository root.
"""

from __future__ import annotations

import argparse
import csv
import datetime as _dt
import math
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

# Per phyto group: output-CSV column index (0-based) and C:Chl ratio from
# const_CL.txt. The 0-D output writes TIME + state vars 1..21, so the column
# index equals the AQUABC state-variable number.
PHYTO_COL = {"dia": 5, "cyn": 15, "opa": 16}
PHYTO_C_TO_CHLA = {"dia": 30.0, "cyn": 40.0, "opa": 30.0}
EUTROPY_C_TO_CHL = 50.0    # a_C_chl in constants_pelagic_1.txt

BASE_DATE = _dt.date(2012, 1, 1)
HERE = os.path.dirname(__file__)
EUTROPY_OUT = os.path.expanduser("~/eutropy/outputs/boxOut_19.csv")
OBS = os.path.expanduser("~/eutropy/observations/19_observation.csv")

_p = argparse.ArgumentParser(description=__doc__)
_p.add_argument("--aquabc-out",
                default=os.path.join(HERE, "box19", "OUTPUT_aquabc.csv"))
_p.add_argument("--png", default=os.path.join(HERE, "box19", "comparison_chla.png"))
_p.add_argument("--label", default="AQUABC PoC (DIA_C/30)")
_p.add_argument("--phyto-target", choices=sorted(PHYTO_COL), default="dia")
_ARGS = _p.parse_args()
AQUABC_OUT = _ARGS.aquabc_out
PNG = _ARGS.png
AQUABC_COL = PHYTO_COL[_ARGS.phyto_target]
AQUABC_C_TO_CHLA = PHYTO_C_TO_CHLA[_ARGS.phyto_target]


def _parse_date(s: str) -> _dt.date:
    s = s.strip()
    for fmt in ("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y"):
        try:
            return _dt.datetime.strptime(s, fmt).date()
        except ValueError:
            continue
    raise ValueError(f"unparseable date {s!r}")


def load_aquabc() -> tuple[list[_dt.date], list[float]]:
    dates, chla = [], []
    with open(AQUABC_OUT) as fh:
        next(fh)  # header
        for row in csv.reader(fh):
            if len(row) <= AQUABC_COL:
                continue
            t = float(row[0])
            phyto_c = float(row[AQUABC_COL])           # selected phyto group
            dates.append(BASE_DATE + _dt.timedelta(days=t - 1))
            chla.append(phyto_c / AQUABC_C_TO_CHLA * 1000.0)
    return dates, chla


def load_eutropy() -> tuple[list[_dt.date], list[float]]:
    dates, chla = [], []
    with open(EUTROPY_OUT) as fh:
        for row in csv.DictReader(fh):
            dates.append(_parse_date(row["Date"]))
            chla.append(float(row["Cpy"]) / EUTROPY_C_TO_CHL * 1000.0)
    return dates, chla


def load_obs() -> tuple[list[_dt.date], list[float]]:
    dates, chla = [], []
    with open(OBS) as fh:
        for row in csv.DictReader(fh):
            dates.append(_parse_date(row["Date"]))
            chla.append(float(row["Cpy"]))
    return dates, chla


def sample_on(dates_model, vals_model, dates_obs, tol_days=4):
    """Nearest model value within tol_days of each observation date."""
    out = []
    for od in dates_obs:
        best, bestd = None, tol_days + 1
        for md, mv in zip(dates_model, vals_model):
            dd = abs((md - od).days)
            if dd < bestd:
                best, bestd = mv, dd
        out.append(best)
    return out


def skill(model, obs):
    pairs = [(m, o) for m, o in zip(model, obs) if m is not None]
    if len(pairs) < 2:
        return None
    m = [p[0] for p in pairs]
    o = [p[1] for p in pairs]
    n = len(pairs)
    mm, mo = sum(m) / n, sum(o) / n
    cov = sum((a - mm) * (b - mo) for a, b in pairs)
    vm = sum((a - mm) ** 2 for a in m)
    vo = sum((b - mo) ** 2 for b in o)
    r = cov / math.sqrt(vm * vo) if vm > 0 and vo > 0 else float("nan")
    rmse = math.sqrt(sum((a - b) ** 2 for a, b in pairs) / n)
    bias = mm - mo
    return {"n": n, "r": r, "rmse": rmse, "bias": bias}


def main() -> int:
    da, ca = load_aquabc()
    de, ce = load_eutropy()
    do, co = load_obs()

    # Restrict all series to the observation window's calendar span for the plot
    fig, ax = plt.subplots(figsize=(11, 5))
    ax.plot(de, ce, color="#1f77b4", lw=1.2, label="EUTROPY (Cpy/50)")
    ax.plot(da, ca, color="#d62728", lw=1.0, alpha=0.8, label=_ARGS.label)
    ax.scatter(do, co, color="black", zorder=5, s=28, label="Observed chl-a")
    ax.set_ylabel("Chlorophyll-a (ug/L)")
    ax.set_title("Curonian Lagoon box 19 — AQUABC PoC vs EUTROPY vs observations")
    ax.set_xlim(_dt.date(2012, 1, 1), _dt.date(2017, 1, 1))
    ax.legend(loc="upper left", fontsize=9)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(PNG, dpi=110)
    print(f"[compare] wrote {PNG}")

    # Skill vs observations
    sa = skill(sample_on(da, ca, do), co)
    se = skill(sample_on(de, ce, do), co)
    print("\n[compare] skill vs box-19 observations (chl-a ug/L):")
    print(f"  {'model':<16}{'n':>4}{'r':>8}{'RMSE':>9}{'bias':>9}")
    for name, s in (("EUTROPY", se), ("AQUABC PoC", sa)):
        if s:
            print(f"  {name:<16}{s['n']:>4}{s['r']:>8.2f}"
                  f"{s['rmse']:>9.2f}{s['bias']:>9.2f}")

    def rng(v):
        vv = [x for x in v if x is not None]
        return (min(vv), max(vv)) if vv else (float("nan"), float("nan"))

    print("\n[compare] chl-a range (ug/L):")
    print(f"  observed   {rng(co)[0]:6.1f} .. {rng(co)[1]:6.1f}")
    print(f"  EUTROPY    {rng(ce)[0]:6.1f} .. {rng(ce)[1]:6.1f}")
    print(f"  AQUABC PoC {rng(ca)[0]:6.1f} .. {rng(ca)[1]:6.1f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
