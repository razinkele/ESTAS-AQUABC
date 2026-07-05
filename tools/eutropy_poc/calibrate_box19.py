#!/usr/bin/env python3
"""Calibrate AQUABC cyanobacteria constants against box-19 chl-a observations.

Perturbs a few CYN growth/loss constants in const_CL.txt, runs the box-19 PoC
driver (Cpy->CYN_C mapping), and minimises RMSE of modelled vs observed
chlorophyll-a with Nelder-Mead. Writes the calibrated constants file and
prints the fit before/after.

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

BASE_CONST = "SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/data/const_CL.txt"
WORK_CONST = "tools/eutropy_poc/box19_cyn/const_cyn_calibrated.txt"
RUN = "./tools/eutropy_poc/run_box19"
INDIR = "tools/eutropy_poc/box19_cyn"
OUT = os.path.join(INDIR, "OUTPUT_aquabc.csv")
OBS = os.path.expanduser("~/eutropy/observations/19_observation.csv")

CYN_COL = 15          # NOFIX_CYNC column in the 0-D output CSV
CYN_C_TO_CHLA = 40.0  # CYN_C_TO_CHLA in const_CL.txt
BASE_DATE = _dt.date(2012, 1, 1)
CAL_STEPS = "48"      # sub-daily steps during calibration (speed); validate at 240

# CYN constants to calibrate: (name, x0, low, high)
PARAMS = [
    ("KG_CYN_OPT_TEMP", 2.40, 1.0, 6.0),    # growth rate at optimal temp
    ("KR_CYN_20",       0.06, 0.01, 0.15),  # respiration at 20 C
    ("KD_CYN_20",       0.125, 0.02, 0.30),  # mortality at 20 C
]
NAMES = [p[0] for p in PARAMS]

with open(BASE_CONST) as fh:
    BASE_LINES = fh.readlines()


def _fdate(s: str) -> _dt.date:
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


def write_const(values: dict[str, float]) -> None:
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
    subprocess.run([RUN, INDIR, WORK_CONST, CAL_STEPS],
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                   check=True)
    dates, chl = [], []
    with open(OUT) as fh:
        next(fh)
        for row in csv.reader(fh):
            if len(row) <= CYN_COL:
                continue
            t = float(row[0])
            dates.append(BASE_DATE + _dt.timedelta(days=t - 1))
            chl.append(float(row[CYN_COL]) / CYN_C_TO_CHLA * 1000.0)
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
_eval = {"n": 0, "best": (float("inf"), None)}


def rmse_of(model_at_obs) -> float:
    pairs = [(m, o) for m, o in zip(model_at_obs, OBS_V) if m is not None]
    if not pairs:
        return 1e6
    return math.sqrt(sum((m - o) ** 2 for m, o in pairs) / len(pairs))


def objective(x) -> float:
    x = [min(max(xi, lo), hi) for xi, (_, _, lo, hi) in zip(x, PARAMS)]
    write_const(dict(zip(NAMES, x)))
    try:
        md, mv = model_chl()
    except subprocess.CalledProcessError:
        return 1e6
    if not mv or any(math.isnan(v) or math.isinf(v) for v in mv):
        return 1e6
    r = rmse_of(sample_on(md, mv, OBS_D))
    _eval["n"] += 1
    if r < _eval["best"][0]:
        _eval["best"] = (r, list(x))
    print(f"  eval {_eval['n']:>3}: "
          + ", ".join(f"{n}={xi:.4f}" for n, xi in zip(NAMES, x))
          + f"  RMSE={r:.2f}")
    return r


def main() -> int:
    x0 = np.array([p[1] for p in PARAMS])
    print("[calibrate] baseline (default constants):")
    r0 = objective(x0)
    print(f"[calibrate] baseline RMSE = {r0:.2f}\n[calibrate] optimising ...")

    res = minimize(objective, x0, method="Nelder-Mead",
                   options={"maxfev": 45, "xatol": 0.02, "fatol": 0.3})

    best_r, best_x = _eval["best"]
    write_const(dict(zip(NAMES, best_x)))
    print("\n[calibrate] ==== result ====")
    print(f"  baseline RMSE : {r0:.2f}")
    print(f"  calibrated RMSE: {best_r:.2f}  (converged={res.success})")
    for (n, x0i, lo, hi), xi in zip(PARAMS, best_x):
        print(f"  {n:<18} {x0i:>8.4f} -> {xi:>8.4f}")
    print(f"[calibrate] wrote calibrated constants -> {WORK_CONST}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
