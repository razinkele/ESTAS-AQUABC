#!/usr/bin/env python3
"""Recalibrate AQUABC cyanobacteria constants against the 29-box NETWORK.

Same three CYN growth/loss constants as the single-box calibration, but each
evaluation runs the full 29-box coupled network (real box-to-box transport)
and scores box-19 chlorophyll RMSE vs the observations. Warm-started from the
single-box-calibrated constants. Constants are global (applied to every box).

Run from the repository root.
"""

from __future__ import annotations

import csv
import datetime as _dt
import math
import os
import subprocess

import numpy as np
from scipy.optimize import minimize

BASE_CONST = "tools/eutropy_poc/box19_cyn/const_cyn_calibrated.txt"
WORK_CONST = "tools/eutropy_poc/net/const_net_calibrated.txt"
RUN = "./tools/eutropy_poc/run_network"
OUT = "tools/eutropy_poc/net/OUTPUT_cyn_C.csv"
OBS = os.path.expanduser("~/eutropy/observations/19_observation.csv")

BOX = 19
CYN_C_TO_CHLA = 40.0
BASE_DATE = _dt.date(2012, 1, 1)
CAL_STEPS = "24"       # sub-daily steps during calibration; validate at 48

# warm start from the single-box-calibrated values
PARAMS = [
    ("KG_CYN_OPT_TEMP", 2.6338, 1.0, 6.0),
    ("KR_CYN_20",       0.0588, 0.01, 0.15),
    ("KD_CYN_20",       0.1129, 0.02, 0.30),
]
NAMES = [p[0] for p in PARAMS]

with open(BASE_CONST) as fh:
    BASE_LINES = fh.readlines()


def _fdate(s):
    for fmt in ("%m/%d/%Y", "%Y-%m-%d", "%d/%m/%Y"):
        try:
            return _dt.datetime.strptime(s.strip(), fmt).date()
        except ValueError:
            continue
    raise ValueError(s)


def load_obs():
    d, v = [], []
    with open(OBS) as fh:
        for row in csv.DictReader(fh):
            d.append(_fdate(row["Date"]))
            v.append(float(row["Cpy"]))
    return d, v


def write_const(values):
    out = []
    for line in BASE_LINES:
        toks = line.split()
        if len(toks) >= 3 and toks[1] in values:
            valstr = f"{values[toks[1]]:.7E}".replace("E", "D")
            out.append(f"{toks[0]:>6} {toks[1]:>30}    {valstr}\n")
        else:
            out.append(line)
    with open(WORK_CONST, "w") as fh:
        fh.writelines(out)


def model_chl():
    subprocess.run([RUN, WORK_CONST, CAL_STEPS],
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
    dates, chl = [], []
    with open(OUT) as fh:
        header = next(csv.reader(fh))
        idx = header.index(f"box{BOX}")
        for row in csv.reader(fh):
            if len(row) <= idx:
                continue
            t = float(row[0])
            dates.append(BASE_DATE + _dt.timedelta(days=t - 1))
            chl.append(float(row[idx]) / CYN_C_TO_CHLA * 1000.0)
    return dates, chl


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


OBS_D, OBS_V = load_obs()
_ev = {"n": 0, "best": (float("inf"), None)}


def objective(x):
    x = [min(max(xi, lo), hi) for xi, (_, _, lo, hi) in zip(x, PARAMS)]
    write_const(dict(zip(NAMES, x)))
    try:
        md, mv = model_chl()
    except subprocess.CalledProcessError:
        return 1e6
    if not mv or any(math.isnan(v) or math.isinf(v) for v in mv):
        return 1e6
    at = sample_on(md, mv, OBS_D)
    pairs = [(m, o) for m, o in zip(at, OBS_V) if m is not None]
    if not pairs:
        return 1e6
    r = math.sqrt(sum((m - o) ** 2 for m, o in pairs) / len(pairs))
    _ev["n"] += 1
    if r < _ev["best"][0]:
        _ev["best"] = (r, list(x))
    print(f"  eval {_ev['n']:>3}: "
          + ", ".join(f"{n}={xi:.4f}" for n, xi in zip(NAMES, x)) + f"  RMSE={r:.2f}")
    return r


def main():
    x0 = np.array([p[1] for p in PARAMS])
    print("[net-cal] network baseline (single-box-calibrated constants):")
    r0 = objective(x0)
    print(f"[net-cal] baseline RMSE = {r0:.2f}\n[net-cal] optimising ...")
    res = minimize(objective, x0, method="Nelder-Mead",
                   options={"maxfev": 40, "xatol": 0.02, "fatol": 0.3})
    best_r, best_x = _ev["best"]
    write_const(dict(zip(NAMES, best_x)))
    print("\n[net-cal] ==== result ====")
    print(f"  baseline RMSE  : {r0:.2f}")
    print(f"  recalibrated   : {best_r:.2f}  (converged={res.success})")
    for (n, x0i, lo, hi), xi in zip(PARAMS, best_x):
        print(f"  {n:<18} {x0i:>8.4f} -> {xi:>8.4f}")
    print(f"[net-cal] wrote -> {WORK_CONST}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
