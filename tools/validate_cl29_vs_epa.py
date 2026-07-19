#!/usr/bin/env python3
"""Validate a CL29 model run against the ingested EPA observations.

Joins each mapped model box's ``PELAGIC_BOX_NNNNN.out`` time series to the EPA
observations produced by ``ingest_epa_observations.py`` (the tidy CSV), over the
model's simulated window, and reports per (box, variable) fit metrics — count,
observed/model means, bias, RMSE, correlation — plus optional comparison plots.

Only the five EPA variables that map to a *direct* model state variable are
compared: NH4->NH4_N, NO3->NO3_N, PO4->PO4_P, DO->DISS_OXYGEN, Si->DISS_Si. The
derived diagnostics (pH, Tot_N, Tot_P, Chl_a) are not in the per-box .out — they
require pool summation with the model's stoichiometry and are left as follow-up.

Model time: ``PELAGIC_BOX_*.out`` column TIME_DAYS is days since Jan 1 of the run
BASE_YEAR (INPUT_CL29.txt); e.g. TIME_DAYS 0.0 == 2012-01-01 for the CL29 config.

Standard library + pandas + numpy; matplotlib only if plots are requested.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import glob
import math
import os
import re
import sys

import numpy as np
import pandas as pd

# EPA tidy variable key -> model .out column (the direct-comparison set).
MODEL_COL = {
    "NH4": "NH4_N", "NO3": "NO3_N", "PO4": "PO4_P",
    "DO": "DISS_OXYGEN", "Si": "DISS_Si",
}


def load_box_output(path, base_year):
    """Load a PELAGIC_BOX_*.out into a DataFrame with a real ``date`` column."""
    df = pd.read_csv(path, sep=r"\s+")
    base = dt.date(base_year, 1, 1)
    df["date"] = df["TIME_DAYS"].map(
        lambda d: base + dt.timedelta(days=float(d)))
    return df


def box_number(path):
    m = re.search(r"PELAGIC_BOX_0*(\d+)\.out$", path)
    return int(m.group(1)) if m else None


def load_obs(tidy_csv):
    """Load the EPA tidy CSV -> {(box, var): DataFrame[date(datetime.date), value]}."""
    out = {}
    with open(tidy_csv, newline="") as fh:
        for r in csv.DictReader(fh):
            var = r["variable"]
            if var not in MODEL_COL:
                continue
            key = (int(r["box"]), var)
            out.setdefault(key, []).append(
                (dt.date.fromisoformat(r["date"]), float(r["value"])))
    return {k: pd.DataFrame(v, columns=["date", "value"]).sort_values("date")
            for k, v in out.items()}


def metrics(model_df, obs_df, col):
    """Interpolate the model series to the obs dates; return fit metrics (or None).

    Observations outside the model's simulated window are dropped (no model value
    to compare). Returns None if nothing overlaps.
    """
    base = model_df["date"].iloc[0]
    m_days = model_df["TIME_DAYS"].to_numpy(float)
    m_vals = model_df[col].to_numpy(float)
    hi = m_days[-1]
    o_days, o_vals = [], []
    for d, v in zip(obs_df["date"], obs_df["value"]):
        off = (d - base).days
        if 0 <= off <= hi:
            o_days.append(off)
            o_vals.append(v)
    if not o_days:
        return None
    o_vals = np.array(o_vals)
    pred = np.interp(o_days, m_days, m_vals)
    resid = pred - o_vals
    r = (float(np.corrcoef(pred, o_vals)[0, 1])
         if len(o_vals) > 2 and o_vals.std() > 0 and pred.std() > 0 else float("nan"))
    return {
        "n": len(o_vals), "obs_mean": o_vals.mean(), "model_mean": pred.mean(),
        "bias": resid.mean(), "rmse": math.sqrt((resid ** 2).mean()), "r": r,
    }


def build_table(out_dir, base_year, obs):
    """Compute metrics for every mapped box/variable that has overlapping obs."""
    rows = []
    for path in sorted(glob.glob(os.path.join(out_dir, "PELAGIC_BOX_*.out"))):
        if path.endswith("_PROCESS_RATES.out"):
            continue
        box = box_number(path)
        keys = [(box, var) for var in MODEL_COL if (box, var) in obs]
        if not keys:
            continue
        mdf = load_box_output(path, base_year)
        for box_, var in keys:
            m = metrics(mdf, obs[(box_, var)], MODEL_COL[var])
            if m:
                rows.append({"box": box_, "variable": var, **m})
    return rows


def print_table(rows):
    if not rows:
        print("No overlapping observations in the model window.")
        return
    hdr = ["box", "var", "n", "obs_mean", "mod_mean", "bias", "rmse", "r"]
    print("  ".join(f"{h:>8s}" for h in hdr))
    for r in sorted(rows, key=lambda r: (r["variable"], r["box"])):
        print("  ".join([
            f"{r['box']:>8d}", f"{r['variable']:>8s}", f"{r['n']:>8d}",
            f"{r['obs_mean']:>8.4g}", f"{r['model_mean']:>8.4g}",
            f"{r['bias']:>8.3g}", f"{r['rmse']:>8.4g}",
            f"{r['r']:>8.2f}" if not math.isnan(r["r"]) else f"{'·':>8s}"]))
    # per-variable roll-up
    print("\nPer-variable summary (obs-weighted):")
    for var in MODEL_COL:
        vr = [r for r in rows if r["variable"] == var]
        if not vr:
            continue
        n = sum(r["n"] for r in vr)
        rmse = math.sqrt(sum(r["rmse"] ** 2 * r["n"] for r in vr) / n)
        bias = sum(r["bias"] * r["n"] for r in vr) / n
        print(f"  {var:4s}  boxes={len(vr):2d}  n={n:5d}  "
              f"RMSE={rmse:.4g}  bias={bias:+.3g}")


def write_metrics_csv(rows, path):
    with open(path, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=["box", "variable", "n", "obs_mean",
                                           "model_mean", "bias", "rmse", "r"])
        w.writeheader()
        w.writerows(rows)


def make_plots(out_dir, base_year, obs, rows, pdf_path):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.backends.backend_pdf import PdfPages

    keys = sorted({(r["box"], r["variable"]) for r in rows},
                  key=lambda k: (k[1], k[0]))
    cache = {}
    with PdfPages(pdf_path) as pdf:
        per_page = 6
        for start in range(0, len(keys), per_page):
            fig, axes = plt.subplots(3, 2, figsize=(11, 8.5))
            for ax, (box, var) in zip(axes.flat, keys[start:start + per_page]):
                path = os.path.join(out_dir, f"PELAGIC_BOX_{box:05d}.out")
                if box not in cache:
                    cache[box] = load_box_output(path, base_year)
                mdf = cache[box]
                ax.plot(mdf["date"], mdf[MODEL_COL[var]], lw=0.6, color="#0072B2",
                        label="model")
                odf = obs[(box, var)]
                ax.scatter(odf["date"], odf["value"], s=12, color="#D55E00",
                           zorder=3, label="EPA obs")
                ax.set_title(f"box {box} — {var} ({MODEL_COL[var]})", fontsize=9)
                ax.tick_params(labelsize=7)
                ax.legend(fontsize=7)
            for ax in axes.flat[len(keys[start:start + per_page]):]:
                ax.axis("off")
            fig.tight_layout()
            pdf.savefig(fig)
            plt.close(fig)


def main(argv=None):
    here = os.path.dirname(os.path.abspath(__file__))
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--outputs", default=os.path.join(here, "..", "OUTPUTS_CL29"),
                   help="CL29 model output folder (PELAGIC_BOX_*.out)")
    p.add_argument("--obs", required=True,
                   help="EPA tidy CSV from ingest_epa_observations.py")
    p.add_argument("--base-year", type=int, default=2012,
                   help="model BASE_YEAR (TIME_DAYS 0 == Jan 1 of this year)")
    p.add_argument("--out", default="./cl29_epa_validation",
                   help="output folder for the metrics CSV and plots")
    p.add_argument("--no-plots", action="store_true", help="skip the PDF plots")
    a = p.parse_args(argv)

    if not os.path.isdir(a.outputs):
        p.error(f"model output folder not found: {a.outputs}")
    obs = load_obs(a.obs)
    rows = build_table(a.outputs, a.base_year, obs)
    print_table(rows)
    if not rows:
        return 1
    os.makedirs(a.out, exist_ok=True)
    csv_path = os.path.join(a.out, "validation_metrics.csv")
    write_metrics_csv(rows, csv_path)
    print(f"\nWrote {csv_path}")
    if not a.no_plots:
        pdf_path = os.path.join(a.out, "validation_timeseries.pdf")
        make_plots(a.outputs, a.base_year, obs, rows, pdf_path)
        print(f"Wrote {pdf_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
