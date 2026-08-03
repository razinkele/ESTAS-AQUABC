#!/usr/bin/env python3
"""Controlled full-record (11-yr) evaluation of a handful of nutrient-subset parameter points, one wave.

A full-record *differential-evolution* calibration (`calibrate_cl29.py --days 4016`) needs hundreds of
~11-min runs and exceeds a typical background time-cap. When you only need to compare a few informed
candidate points on the full record — e.g. to check whether a lever transfers out of a short training
window, or to pick an adoptable value — this evaluates them all in a single parallel wave (~15 min) and
reports full-record Φ + per-variable bias vs the current defaults. See docs/CL29_Calibration_Results.md.

    python3 tools/eval_fullrecord_points.py            # the documented nutrient-subset grid

Edit POINTS to evaluate a different set (each is a name + a dict of WCONST overrides; None = defaults).
"""
import os
import shutil
import sys
from concurrent.futures import ProcessPoolExecutor

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import calibrate_cl29 as C  # noqa: E402  (reuses the calibrator forward model + Φ)

# Nutrient-subset points (phyto-biomass knobs stay at defaults; only listed keys are perturbed).
# Defaults: denit=1.0, nitrif=0.6, PON->NH4=0.25, KHS_DIP=0.005.
POINTS = [
    ("defaults",         None),
    ("denit1.5",         {"K_MIN_DOC_NO3N_20": 1.5}),
    ("denit2.0",         {"K_MIN_DOC_NO3N_20": 2.0}),
    ("Paff0.003",        {"KHS_DIP_DIA": 0.003}),
    ("denit1.5+Paff",    {"K_MIN_DOC_NO3N_20": 1.5, "KHS_DIP_DIA": 0.003}),
    ("Ncyc+Paff",        {"K_MIN_DOC_NO3N_20": 1.5, "K_NITR_20": 1.0,
                          "KDISS_DET_PART_ORG_N_20": 0.4, "KHS_DIP_DIA": 0.003}),
    ("denit2+Ncyc+Paff", {"K_MIN_DOC_NO3N_20": 2.0, "K_NITR_20": 1.2,
                          "KDISS_DET_PART_ORG_N_20": 0.5, "KHS_DIP_DIA": 0.003}),
    ("Nregen_only",      {"K_NITR_20": 1.0, "KDISS_DET_PART_ORG_N_20": 0.4}),
]
REPORT_VARS = ["NH4", "NO3", "PO4", "Si", "CHLA", "TN"]


def eval_point(item):
    name, vals = item
    tag = name.replace("+", "_").replace(".", "p")
    try:
        wd, ok = C._run(vals, tag)
        if not ok:
            return (name, None, {})
        rows = C._metrics_rows(wd, tag)
        phi = C._phi_from_rows(rows, C.CAL_PHI_VARS)
        shutil.rmtree(wd, ignore_errors=True)
        return (name, phi, rows)
    except Exception as e:
        return (name, None, {"err": str(e)})


def main():
    C.DAYS = 4016
    C.WORKDIR = "/tmp/cal_grid"
    os.makedirs(C.WORKDIR, exist_ok=True)
    if not os.path.exists(C.M.BIN):
        raise SystemExit("ESTAS_II not built (make build-estas)")
    print(f"Full-record ({C.DAYS}d) evaluation of {len(POINTS)} points, 8-way parallel")
    with ProcessPoolExecutor(max_workers=8) as ex:
        results = list(ex.map(eval_point, POINTS))
    base = {n: (p, r) for n, p, r in results}.get("defaults", (None, {}))
    print(f"\n{'point':<20}{'Φ':>9}{'ΔΦ%':>7}  " + "".join(f"{v + '_bias':>12}" for v in REPORT_VARS))
    for name, phi, rows in results:
        if phi is None:
            print(f"{name:<20}  FAILED {rows.get('err', '')}")
            continue
        dphi = 100 * (base[0] - phi) / base[0] if base[0] else 0.0
        biases = "".join(f"{rows[v][3]:>+12.4g}" if v in rows else f"{'-':>12}" for v in REPORT_VARS)
        print(f"{name:<20}{phi:>9.3f}{dphi:>+7.1f}  {biases}")
    print("\n(ΔΦ% > 0 = better than defaults; bias vs obs, want near 0; CHLA bias watched for regression)")


if __name__ == "__main__":
    main()
