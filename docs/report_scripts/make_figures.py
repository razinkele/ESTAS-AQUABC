#!/usr/bin/env python3
"""Measured-vs-modelled time-series figures + metrics JSON for the CL29 calibration report.

One PNG per compared variable (NH4, NO3, PO4, DO, Si, TN, TP, CHLA): 3 stacked
panels = the 3 mapped boxes with the most observations in the simulated window,
model daily line vs EPA observation points, per-panel fit metrics.
"""
import datetime as dt
import json
import math
import os
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np

REPO = "/home/razinka/AQUABCv0.2"
sys.path.insert(0, os.path.join(REPO, "tools"))
import validate_cl29_vs_epa as V  # noqa: E402

# Score chlorophyll with the RUN'S OWN C:Chl ratios, not the validator's module
# defaults. C:Chl is a model parameter (it sets I_s and drives self-shading), and
# the defaults are NOT CL29's values -- without this the CHLA figure and metrics
# read RMSE 37.97 instead of the correct 23.83, a 59 % overstatement of the bias.
# Same trap as the validator's --wconst flag.
V._ACTIVE_C_TO_CHLA = V.load_c_to_chla(
    os.environ.get("CL29_FIG_WCONST",
                   os.path.join(REPO, "INPUTS_CL29", "WCONST_04.txt")))

HERE = os.path.dirname(os.path.abspath(__file__))
# Model output directory. Defaults to the repo's canonical CL29 run so the
# figures always track the adopted configuration; override with CL29_FIG_OUT
# to plot a probe or an older verification run.
OUT_DIR = os.environ.get("CL29_FIG_OUT", os.path.join(REPO, "OUTPUTS_CL29"))
FIG_DIR = os.path.join(HERE, "figures")
OBS_CSV = os.path.join(REPO, "epa_observations_out", "epa_observations_tidy.csv")
BASE_YEAR = 2012

# palette (validated, dataviz reference instance, light mode)
C_MODEL = "#2a78d6"   # slot 1 blue  — model line
C_OBS = "#eb6834"     # slot 2 orange — observation points
C_TEXT = "#0b0b0b"
C_TEXT2 = "#52514e"
C_GRID = "#d9d8d4"

BOX_LABEL = {
    7: "Box 7 — Klaipėda Strait (LTK1, LTK2)",
    9: "Box 9 — Central lagoon (LTK14)",
    11: "Box 11 — Strait / river plume (LTK3, LTK3A, LTK3B)",
    14: "Box 14 — Central lagoon (LTK12)",
    15: "Box 15 — Northern lagoon (LTK6, LTK8)",
    17: "Box 17 — Northern lagoon (LTK5)",
    19: "Box 19 — Northern lagoon (LTK7B)",
    20: "Box 20 — Northern lagoon (LTK4)",
    23: "Box 23 — Central lagoon (LTK10)",
}
VAR_META = {  # var -> (long name, y-axis label)
    "NH4": ("Ammonium nitrogen", "NH$_4$–N (mg N/L)"),
    "NO3": ("Nitrate nitrogen", "NO$_3$–N (mg N/L)"),
    "PO4": ("Phosphate phosphorus", "PO$_4$–P (mg P/L)"),
    "DO": ("Dissolved oxygen", "DO (mg O$_2$/L)"),
    "Si": ("Dissolved silica", "DSi (mg Si/L)"),
    "TN": ("Total nitrogen", "TN (mg N/L)"),
    "TP": ("Total phosphorus", "TP (mg P/L)"),
    "CHLA": ("Chlorophyll-a", "Chl-a (µg/L)"),
}
VAR_ORDER = ["NH4", "NO3", "PO4", "Si", "DO", "TN", "TP", "CHLA"]


def main():
    os.makedirs(FIG_DIR, exist_ok=True)
    obs = V.load_obs(OBS_CSV)
    rows = V.build_table(OUT_DIR, BASE_YEAR, obs)
    season = V.season_summary(OUT_DIR, BASE_YEAR, obs)

    # model window
    sample = V.load_box_output(os.path.join(OUT_DIR, "PELAGIC_BOX_00007.out"), BASE_YEAR)
    t0, t1 = sample["date"].iloc[0], sample["date"].iloc[-1]
    print(f"model window: {t0} .. {t1}", flush=True)

    per_panel = {(r["box"], r["variable"]): r for r in rows}
    cache = {}
    plt.rcParams.update({
        "font.size": 9, "text.color": C_TEXT, "axes.labelcolor": C_TEXT,
        "xtick.color": C_TEXT2, "ytick.color": C_TEXT2,
        "axes.edgecolor": C_GRID, "font.family": "DejaVu Sans",
    })

    for var in VAR_ORDER:
        boxes = sorted([b for (b, v) in per_panel if v == var],
                       key=lambda b: -per_panel[(b, var)]["n"])[:3]
        if not boxes:
            print(f"{var}: no overlapping obs, skipped")
            continue
        boxes = sorted(boxes)  # north-to-south stable ordering by box id
        fig, axes = plt.subplots(len(boxes), 1, figsize=(7.0, 2.15 * len(boxes)),
                                 sharex=True)
        axes = np.atleast_1d(axes)
        long_name, ylab = VAR_META[var]
        for ax, box in zip(axes, boxes):
            if box not in cache:
                cache[box] = V.load_box_output(
                    os.path.join(OUT_DIR, f"PELAGIC_BOX_{box:05d}.out"), BASE_YEAR)
            mdf = cache[box]
            col = V.MODEL_COL[var]
            odf = obs[(box, var)]
            odf = odf[(odf["date"] >= t0) & (odf["date"] <= t1)]
            ax.plot(mdf["date"], mdf[col], lw=1.0, color=C_MODEL, zorder=2,
                    label="Model (daily)")
            ax.scatter(odf["date"], odf["value"], s=16, color=C_OBS, zorder=3,
                       edgecolors="white", linewidths=0.6, label="EPA observations")
            # robust y-limit: don't let one spike crush the panel
            vals = np.concatenate([mdf[col].to_numpy(float), odf["value"].to_numpy(float)])
            hi = max(np.percentile(mdf[col], 99.8), odf["value"].max() * 1.05)
            ax.set_ylim(0 if var != "DO" else max(0, vals.min() * 0.9), hi * 1.04)
            ax.grid(axis="y", color=C_GRID, lw=0.6)
            ax.set_axisbelow(True)
            for s in ("top", "right"):
                ax.spines[s].set_visible(False)
            ax.set_ylabel(ylab, fontsize=8.5)
            m = per_panel[(box, var)]
            r_txt = f", r = {m['r']:.2f}" if not math.isnan(m["r"]) else ""
            ax.set_title(BOX_LABEL.get(box, f"Box {box}"), fontsize=9,
                         loc="left", color=C_TEXT, pad=3)
            ax.text(0.995, 0.96,
                    f"n = {m['n']}   RMSE = {m['rmse']:.3g}   bias = {m['bias']:+.3g}{r_txt}",
                    transform=ax.transAxes, ha="right", va="top",
                    fontsize=7.5, color=C_TEXT2)
        axes[-1].xaxis.set_major_locator(mdates.YearLocator())
        axes[-1].xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
        axes[0].legend(loc="upper left", fontsize=8, frameon=False, ncols=2,
                       borderaxespad=0.2)
        fig.suptitle(f"{long_name} — measured vs modelled, 2012–2022", x=0.02,
                     ha="left", fontsize=11, fontweight="bold", color=C_TEXT)
        fig.tight_layout(rect=(0, 0, 1, 0.965))
        p = os.path.join(FIG_DIR, f"ts_{var}.png")
        fig.savefig(p, dpi=200, facecolor="white")
        plt.close(fig)
        print(f"wrote {p}")

    # metrics bundle for the docx builder
    agg = {}
    for var in VAR_ORDER:
        vr = [r for r in rows if r["variable"] == var]
        if not vr:
            continue
        n = sum(r["n"] for r in vr)
        agg[var] = {
            "boxes": len(vr), "n": n,
            "rmse": math.sqrt(sum(r["rmse"] ** 2 * r["n"] for r in vr) / n),
            "bias": sum(r["bias"] * r["n"] for r in vr) / n,
            "obs_mean": sum(r["obs_mean"] * r["n"] for r in vr) / n,
            "model_mean": sum(r["model_mean"] * r["n"] for r in vr) / n,
        }
    with open(os.path.join(FIG_DIR, "metrics.json"), "w") as f:
        json.dump({"per_box": rows, "aggregate": agg, "season": season,
                   "window": [str(t0), str(t1)]}, f, indent=1, default=str)
    print("wrote metrics.json")


if __name__ == "__main__":
    main()
