#!/usr/bin/env python3
"""Validate a CL29 model run against the ingested EPA observations.

Joins each mapped model box's ``PELAGIC_BOX_NNNNN.out`` time series to the EPA
observations produced by ``ingest_epa_observations.py`` (the tidy CSV), over the
model's simulated window, and reports per (box, variable) fit metrics — count,
observed/model means, bias, RMSE, correlation — plus optional comparison plots.

Five EPA variables map to a *direct* model state variable (NH4->NH4_N, NO3->NO3_N,
PO4->PO4_P, DO->DISS_OXYGEN, Si->DISS_Si). Three more (Tot_N, Tot_P, Chl_a) are not
state variables but are reconstructed from the pool variables using the CL29
WCONST_04 stoichiometry -- the same pool definition the Shiny mass_balance module
uses, extended to the Nostocales pools present in CL29 (see add_derived). pH is
still deferred (it needs a CO2SYS solve from INORG_C + TOT_ALK).

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

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from seasonal_phase import format_report, phase_metrics  # noqa: E402

# EPA tidy variable key -> model .out state-variable column (direct comparison).
DIRECT_COL = {
    "NH4": "NH4_N", "NO3": "NO3_N", "PO4": "PO4_P",
    "DO": "DISS_OXYGEN", "Si": "DISS_Si",
}
# Stoichiometry for the derived totals, from INPUTS_CL29/WCONST_04.txt: every
# group carries N:C 0.22, P:C 0.024; C:Chl-a is 30 (diatoms, OPA) or 40 (cyano,
# fixing cyano, Nostocales). Chl-a excludes dormant akinetes (AKI_C).
N_TO_C, P_TO_C = 0.22, 0.024
PHYTO_C = ["DIA_C", "CYN_C", "FIX_CYN_C", "OPA_C", "NOST_VEG_HET_C", "AKI_C"]
C_TO_CHLA = {"DIA_C": 30.0, "CYN_C": 40.0, "FIX_CYN_C": 40.0,
             "OPA_C": 30.0, "NOST_VEG_HET_C": 40.0}
# WCONST constant name -> the pool whose C:Chl it sets. The C:Chl ratios are *model*
# parameters (they drive CHLA -> light extinction -> self-shading in LIM_LIGHT/CUR_SMITH,
# see aquabc_II_pelagic_model.f90:1005), so a run with perturbed ratios must be scored with
# the same ratios — use --wconst to read them from that run's constants file.
WCONST_CHLA_KEYS = {"DIA_C_TO_CHLA": "DIA_C", "CYN_C_TO_CHLA": "CYN_C",
                    "FIX_CYN_C_TO_CHLA": "FIX_CYN_C", "OPA_C_TO_CHLA": "OPA_C",
                    "NOST_C_TO_CHLA": "NOST_VEG_HET_C"}


def load_c_to_chla(wconst_path):
    """Read the C:Chl ratios from a WCONST_04.txt -> {pool: ratio} (missing keys keep defaults)."""
    out = dict(C_TO_CHLA)
    with open(wconst_path) as fh:
        for ln in fh:
            parts = ln.split()
            if len(parts) >= 3 and parts[1] in WCONST_CHLA_KEYS:
                try:
                    out[WCONST_CHLA_KEYS[parts[1]]] = float(parts[2])
                except ValueError:
                    pass
    return out
# EPA key -> comparison column: a direct .out column, or a derived column of the
# same name added by add_derived().
MODEL_COL = {**DIRECT_COL, "TN": "TN", "TP": "TP", "CHLA": "CHLA"}
# Plankton-carbon observations (tools/ingest_km_plankton.py, mg C/L). Group biomasses
# compare directly to state variables; observed heterocystous/N-fixing carbon cannot
# separate FIX_CYN from Nostocales, so it is scored against their sum (FIX_TOT_C);
# the observed total is scored against all live phytoplankton carbon (PHYTO_TOT_C,
# dormant akinetes excluded — they are not counted as live biomass in the surveys).
MODEL_COL.update({
    "DIA_C": "DIA_C", "CYN_C": "CYN_C", "OPA_C": "OPA_C", "ZOO_C": "ZOO_C",
    "FIX_CYN_C": "FIX_TOT_C", "PHYTO_TOT_C": "PHYTO_TOT_C",
})


_ACTIVE_C_TO_CHLA = None      # set by main() from --wconst; None = module defaults


def _c_to_chla():
    return _ACTIVE_C_TO_CHLA or C_TO_CHLA


def _col(df, name):
    """Column as a float Series, or zeros if the model output lacks it."""
    return (df[name].astype(float) if name in df.columns
            else pd.Series(0.0, index=df.index))


def add_derived(df):
    """Add Tot_N (mg N/L), Tot_P (mg P/L), Chl_a (ug/L) columns from the pools.

    Tot_N/Tot_P sum the inorganic, dissolved-organic, detrital, zooplankton and
    phytoplankton (carbon x N:C or P:C) pools; Chl_a sums each phytoplankton
    carbon pool divided by its C:Chl ratio (x1000 for mg/L -> ug/L).

    TN's CYN contribution: if this output carries a CYN_N column (a VARN/Droop-
    mechanism run, CYN_VARIABLE_N=1), use it directly -- it is the model's own
    tracked N-quota state variable, mg N/L already. Otherwise fall back to the
    legacy fixed N_TO_C*CYN_C ratio, UNCHANGED from before this branch existed,
    so a standard (non-VARN) run scores byte-identically. The model's own
    Fortran-side derived TN (GENERATE_PELAGIC_DERIVED_VARS,
    mod_PELAGIC_ECOLOGY.f90:369) always uses the legacy ratio, even under
    CYN_VARIABLE_N=1 -- it does not read CYN_N -- so it is wrong for VARN runs;
    this Python-side reconstruction is the source of truth for TN in that case.
    """
    phyto_c = sum((_col(df, c) for c in PHYTO_C), pd.Series(0.0, index=df.index))
    if "CYN_N" in df.columns:
        non_cyn_phyto_c = phyto_c - _col(df, "CYN_C")
        df["TN"] = (_col(df, "NH4_N") + _col(df, "NO3_N") + _col(df, "DISS_ORG_N")
                    + _col(df, "DET_PART_ORG_N") + _col(df, "ZOO_N")
                    + N_TO_C * non_cyn_phyto_c + df["CYN_N"].astype(float))
    else:
        df["TN"] = (_col(df, "NH4_N") + _col(df, "NO3_N") + _col(df, "DISS_ORG_N")
                    + _col(df, "DET_PART_ORG_N") + _col(df, "ZOO_N") + N_TO_C * phyto_c)
    df["TP"] = (_col(df, "PO4_P") + _col(df, "DISS_ORG_P")
                + _col(df, "DET_PART_ORG_P") + _col(df, "ZOO_P") + P_TO_C * phyto_c)
    df["CHLA"] = 1000.0 * sum((_col(df, c) / r for c, r in _c_to_chla().items()),
                              pd.Series(0.0, index=df.index))
    df["FIX_TOT_C"] = _col(df, "FIX_CYN_C") + _col(df, "NOST_VEG_HET_C")
    df["PHYTO_TOT_C"] = (_col(df, "DIA_C") + _col(df, "CYN_C") + _col(df, "OPA_C")
                         + df["FIX_TOT_C"])
    return df


def load_box_output(path, base_year):
    """Load a PELAGIC_BOX_*.out into a DataFrame with ``date`` + derived columns."""
    df = pd.read_csv(path, sep=r"\s+")
    base = dt.date(base_year, 1, 1)
    df["date"] = df["TIME_DAYS"].map(
        lambda d: base + dt.timedelta(days=float(d)))
    return add_derived(df)


def box_number(path):
    m = re.search(r"PELAGIC_BOX_0*(\d+)\.out$", path)
    return int(m.group(1)) if m else None


def load_obs(tidy_csv, since=None, until=None):
    """Load the EPA tidy CSV -> {(box, var): DataFrame[date(datetime.date), value]}.

    ``since``/``until`` (ISO date strings, inclusive) restrict the observations — used to
    score a calibration holdout period in isolation.
    """
    out = {}
    with open(tidy_csv, newline="") as fh:
        for r in csv.DictReader(fh):
            var = r["variable"]
            if var not in MODEL_COL:
                continue
            if (since and r["date"] < since) or (until and r["date"] > until):
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


def _season_of(month):
    """Meteorological season bucket, used to expose obs sampling bias (e.g. summer-heavy Si)."""
    if month in (12, 1, 2, 3):
        return "winter"
    if month in (6, 7, 8, 9):
        return "summer"
    return "shoulder"


def season_summary(out_dir, base_year, obs):
    """Per-(variable, season) aggregate metrics, pooled over boxes.

    The aggregate obs mean can be dominated by a season the obs over-sample — EPA Si, for example,
    is ~3x summer-heavy, which pulls the annual obs mean down while the model is aseasonal, so the
    aggregate RMSE over-states the mismatch. Splitting by season separates where the model already
    matches (e.g. winter Si) from the real gap (e.g. the missing summer Si drawdown).
    """
    buckets = {}   # (var, season) -> [pred[], obs[]]
    for path in sorted(glob.glob(os.path.join(out_dir, "PELAGIC_BOX_*.out"))):
        if path.endswith("_PROCESS_RATES.out"):
            continue
        box = box_number(path)
        keys = [(box, var) for var in MODEL_COL if (box, var) in obs]
        if not keys:
            continue
        mdf = load_box_output(path, base_year)
        base = mdf["date"].iloc[0]
        m_days = mdf["TIME_DAYS"].to_numpy(float)
        hi = m_days[-1]
        for box_, var in keys:
            m_vals = mdf[MODEL_COL[var]].to_numpy(float)
            for d, v in zip(obs[(box_, var)]["date"], obs[(box_, var)]["value"]):
                off = (d - base).days
                if not (0 <= off <= hi):
                    continue
                pred = float(np.interp(off, m_days, m_vals))
                b = buckets.setdefault((var, _season_of(d.month)), [[], []])
                b[0].append(pred)
                b[1].append(v)
    rows = []
    for (var, s), (pred, ov) in buckets.items():
        p, o = np.array(pred), np.array(ov)
        resid = p - o
        rows.append({"variable": var, "season": s, "n": len(o),
                     "obs_mean": float(o.mean()), "model_mean": float(p.mean()),
                     "bias": float(resid.mean()), "rmse": math.sqrt(float((resid ** 2).mean()))})
    return rows


def print_season_table(rows):
    order = {"winter": 0, "shoulder": 1, "summer": 2}
    print("\nSeasonal breakdown (pooled over boxes) — exposes obs sampling bias:")
    print(f"  {'var':<5}{'season':<9}{'n':>6}{'obs_mean':>10}{'mod_mean':>10}{'bias':>9}{'rmse':>9}")
    for r in sorted(rows, key=lambda r: (r["variable"], order.get(r["season"], 9))):
        print(f"  {r['variable']:<5}{r['season']:<9}{r['n']:>6}{r['obs_mean']:>10.4g}"
              f"{r['model_mean']:>10.4g}{r['bias']:>+9.3g}{r['rmse']:>9.4g}")


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


def phase_summary(out_dir, base_year, obs, variable="CHLA"):
    """Monthly climatology of model vs observations, pooled over mapped boxes.

    Observations are restricted to the simulated window exactly as metrics() and
    season_summary() do; otherwise observations from years the model never ran
    dominate the observed climatology (69% of the CHLA rows predate 2016).
    """
    model_sum, model_n, obs_sum, obs_n = {}, {}, {}, {}
    for path in sorted(glob.glob(os.path.join(out_dir, "PELAGIC_BOX_*.out"))):
        if path.endswith("_PROCESS_RATES.out"):
            continue
        box = box_number(path)
        if (box, variable) not in obs:
            continue
        mdf = load_box_output(path, base_year)
        col = MODEL_COL[variable]
        base = mdf["date"].iloc[0]
        hi = mdf["TIME_DAYS"].to_numpy(float)[-1]
        for d, v in zip(mdf["date"], mdf[col]):
            model_sum[d.month] = model_sum.get(d.month, 0.0) + float(v)
            model_n[d.month] = model_n.get(d.month, 0) + 1
        for d, v in zip(obs[(box, variable)]["date"], obs[(box, variable)]["value"]):
            off = (d - base).days
            if not (0 <= off <= hi):
                continue
            obs_sum[d.month] = obs_sum.get(d.month, 0.0) + float(v)
            obs_n[d.month] = obs_n.get(d.month, 0) + 1
    model_by_month = {m: model_sum[m] / model_n[m] for m in model_sum}
    obs_by_month = {m: obs_sum[m] / obs_n[m] for m in obs_sum}
    return phase_metrics(model_by_month, obs_by_month), model_by_month, obs_by_month


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
    p.add_argument("--by-season", action="store_true",
                   help="also print a per-(variable, season) breakdown (exposes obs sampling bias)")
    p.add_argument("--phase", action="store_true",
                   help="report seasonal-phase metrics (peak month, autumn/spring ratio, "
                        "seasonal correlation) for chlorophyll-a")
    p.add_argument("--since", default=None, metavar="YYYY-MM-DD",
                   help="only score observations on/after this date (holdout scoring)")
    p.add_argument("--until", default=None, metavar="YYYY-MM-DD",
                   help="only score observations on/before this date")
    p.add_argument("--wconst", default=None,
                   help="read the C:Chl ratios from this run's WCONST_04.txt instead of the "
                        "shipped defaults (required when scoring a run with perturbed C:Chl — "
                        "they are model parameters, not just a reporting convention)")
    a = p.parse_args(argv)

    if a.wconst:
        global _ACTIVE_C_TO_CHLA
        _ACTIVE_C_TO_CHLA = load_c_to_chla(a.wconst)
        print(f"C:Chl from {a.wconst}: "
              + ", ".join(f"{k}={v:g}" for k, v in _ACTIVE_C_TO_CHLA.items()))

    if not os.path.isdir(a.outputs):
        p.error(f"model output folder not found: {a.outputs}")
    obs = load_obs(a.obs, since=a.since, until=a.until)
    rows = build_table(a.outputs, a.base_year, obs)
    print_table(rows)
    if not rows:
        return 1
    if a.by_season:
        print_season_table(season_summary(a.outputs, a.base_year, obs))
    if a.phase:
        m, model_by_month, obs_by_month = phase_summary(a.outputs, a.base_year, obs)
        print("\nSeasonal phase (chlorophyll-a, monthly climatology):")
        print(format_report(m))
        print(f"  {'month':>6}{'model':>9}{'obs':>9}")
        for mo in sorted(set(model_by_month) & set(obs_by_month)):
            print(f"  {mo:>6}{model_by_month[mo]:>9.1f}{obs_by_month[mo]:>9.1f}")
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
