#!/usr/bin/env python3
"""Compare the 29-box AQUABC network (box 19) vs EUTROPY and observations.

Box 19 is now driven by the full box-to-box advective network rather than the
single-box flushing approximation. Reads net/OUTPUT_cyn_C.csv (CYN_C mg C/L
per box), converts box 19 to chlorophyll, and reports the fit.

Run from the repository root.
"""

from __future__ import annotations

import csv
import datetime as _dt
import math
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

HERE = os.path.dirname(__file__)
NET_OUT = os.path.join(HERE, "net", "OUTPUT_cyn_C.csv")
EUTROPY_OUT = os.path.expanduser("~/eutropy/outputs/boxOut_19.csv")
OBS = os.path.expanduser("~/eutropy/observations/19_observation.csv")
PNG = os.path.join(HERE, "net", "comparison_network_box19.png")
BASE_DATE = _dt.date(2012, 1, 1)
CYN_C_TO_CHLA = 40.0
EUTROPY_C_TO_CHL = 50.0
BOX = 19


def _fdate(s):
    for fmt in ("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y"):
        try:
            return _dt.datetime.strptime(s.strip(), fmt).date()
        except ValueError:
            continue
    raise ValueError(s)


def load_net_box(col_box):
    dates, chl = [], []
    with open(NET_OUT) as fh:
        header = next(csv.reader(fh))
        idx = header.index(f"box{col_box}")
        for row in csv.reader(fh):
            if len(row) <= idx:
                continue
            t = float(row[0])
            dates.append(BASE_DATE + _dt.timedelta(days=t - 1))
            chl.append(float(row[idx]) / CYN_C_TO_CHLA * 1000.0)
    return dates, chl


def load_two_col(path, col, conv):
    dates, val = [], []
    with open(path) as fh:
        for row in csv.DictReader(fh):
            dates.append(_fdate(row["Date"]))
            val.append(conv(float(row[col])))
    return dates, val


def sample_on(md, mv, od, tol=4):
    out = []
    for o in od:
        best, bd = None, tol + 1
        for d, v in zip(md, mv):
            dd = abs((d - o).days)
            if dd < bd:
                best, bd = v, dd
        out.append(best)
    return out


def skill(model_at_obs, obs):
    pairs = [(m, o) for m, o in zip(model_at_obs, obs) if m is not None]
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
    return {"n": n, "r": r, "rmse": rmse, "bias": mm - mo}


def main():
    da, ca = load_net_box(BOX)
    de, ce = load_two_col(EUTROPY_OUT, "Cpy", lambda c: c / EUTROPY_C_TO_CHL * 1000.0)
    do, co = load_two_col(OBS, "Cpy", lambda c: c)

    fig, ax = plt.subplots(figsize=(11, 5))
    ax.plot(de, ce, color="#1f77b4", lw=1.2, label="EUTROPY (Cpy/50)")
    ax.plot(da, ca, color="#2ca02c", lw=1.0, alpha=0.85,
            label="AQUABC 29-box network (CYN_C/40)")
    ax.scatter(do, co, color="black", zorder=5, s=28, label="Observed chl-a")
    ax.set_ylabel("Chlorophyll-a (ug/L)")
    ax.set_title("Curonian Lagoon box 19 — AQUABC 29-box network vs EUTROPY vs obs")
    ax.set_xlim(_dt.date(2012, 1, 1), _dt.date(2017, 1, 1))
    ax.legend(loc="upper left", fontsize=9)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(PNG, dpi=110)
    print(f"[compare-net] wrote {PNG}")

    sa = skill(sample_on(da, ca, do), co)
    se = skill(sample_on(de, ce, do), co)
    print("\n[compare-net] skill vs box-19 observations (chl-a ug/L):")
    print(f"  {'model':<24}{'n':>4}{'r':>8}{'RMSE':>9}{'bias':>9}")
    for name, s in (("EUTROPY", se), ("AQUABC network", sa)):
        if s:
            print(f"  {name:<24}{s['n']:>4}{s['r']:>8.2f}{s['rmse']:>9.2f}{s['bias']:>9.2f}")

    vv = [x for x in ca if x is not None]
    print(f"\n[compare-net] box-19 network chl range: {min(vv):.1f} .. {max(vv):.1f} ug/L")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
