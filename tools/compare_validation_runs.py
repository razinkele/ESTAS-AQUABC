#!/usr/bin/env python3
"""Compare two CL29 validation_metrics.csv runs (baseline vs promoted).

Aggregates per-variable obs-weighted RMSE + bias exactly as validate_cl29_vs_epa.py's
own summary does, prints a before/after/delta table, and applies a one-sided regression
guard: exit non-zero if any --no-regress variable regresses > --max-rise % on RMSE OR on
|bias| (or its bias sign-flips into non-trivial error).
"""
from __future__ import annotations

import argparse
import csv
import math
import sys


def read_metrics(path):
    with open(path) as fh:
        return list(csv.DictReader(fh))


def aggregate(rows):
    by = {}
    for r in rows:
        v = r["variable"]          # validate_cl29_vs_epa.py CSV column is "variable"
        by.setdefault(v, []).append((int(r["n"]), float(r["rmse"]), float(r["bias"])))
    out = {}
    for v, recs in by.items():
        n = sum(k for k, _, _ in recs)
        rmse = math.sqrt(sum(rm ** 2 * k for k, rm, _ in recs) / n)
        bias = sum(b * k for k, _, b in recs) / n
        out[v] = {"n": n, "rmse": rmse, "bias": bias}
    return out


def rmse_rise_pct(base, prom):
    """% RMSE rise; a rise from a perfect-fit (0) baseline to >0 is a regression (inf)."""
    if base > 0:
        return 100.0 * (prom - base) / base
    return float("inf") if prom > 0 else 0.0


def bias_regressed(b_bias, q_bias, tol_pct):
    """True if bias sign-flips into non-trivial error, |bias| appears from ~0, or grows beyond tol_pct."""
    if abs(q_bias) < 1e-9:
        return False
    if b_bias * q_bias < 0:                        # sign flip into real bias
        return True
    if abs(b_bias) <= 1e-6:                         # bias appeared where there was ~none
        return abs(q_bias) > 1e-6
    return abs(q_bias) > abs(b_bias) * (1 + tol_pct / 100.0)


def main(argv=None):
    p = argparse.ArgumentParser()
    p.add_argument("baseline")
    p.add_argument("promoted")
    p.add_argument("--no-regress", default="", help="comma list of vars held to the guard")
    p.add_argument("--max-rise", type=float, default=5.0,
                   help="max allowed RMSE/|bias| rise (%%)")
    a = p.parse_args(argv)

    base = aggregate(read_metrics(a.baseline))
    prom = aggregate(read_metrics(a.promoted))
    guard = {v.strip().upper() for v in a.no_regress.split(",") if v.strip()}

    print(f"{'var':6} {'n':>5} {'RMSE base':>10} {'RMSE prom':>10} {'dRMSE%':>8} "
          f"{'bias base':>10} {'bias prom':>10}")
    failures = []
    for v in sorted(set(base) | set(prom)):
        b = base.get(v)
        q = prom.get(v)
        if not b or not q:
            print(f"{v:6}  (only in one run — cannot compare)")
            if v.upper() in guard:
                failures.append((v, "missing in one run"))
            continue
        d = rmse_rise_pct(b["rmse"], q["rmse"])
        flag = ""
        if v.upper() in guard and (d > a.max_rise or bias_regressed(b["bias"], q["bias"], a.max_rise)):
            flag = "  <-- REGRESSION"
            failures.append((v, f"dRMSE {d:+.1f}%, bias {b['bias']:+.3g}->{q['bias']:+.3g}"))
        print(f"{v:6} {b['n']:>5} {b['rmse']:>10.4g} {q['rmse']:>10.4g} {d:>+8.1f} "
              f"{b['bias']:>+10.3g} {q['bias']:>+10.3g}{flag}")

    if failures:
        print("\nGUARD FAILED: " + "; ".join(f"{v} ({why})" for v, why in failures))
        return 1
    print(f"\nGuard passed (no --no-regress variable regressed > {a.max_rise:.0f}%).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
