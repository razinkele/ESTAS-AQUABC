#!/usr/bin/env python3
"""Two figures the 2026-08 set predates, both from the ADOPTED CL29 configuration.

(a) seasonal_phase.png -- the chlorophyll seasonal cycle, model vs observed.
    REPLACES figures/seasonal_inversion.png, which shows the pre-adoption inverted
    cycle (model peaking in February). The inversion was closed by the FIX_CYN
    T_min correction (doc s.23), the CYN T_min 5->2 change (s.40) and the ice
    adoption (s.45); the current model peaks in September against an observed August.

(b) autumn_composition.png -- the doc s.50 result: Aug-Oct TOTAL cyanobacterial
    carbon is essentially exact while the split between the two guilds is wrong.
    This is the paper's composition argument in one panel.
"""
import collections
import datetime as dt
import os
import statistics as st
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

REPO = "/home/razinka/AQUABCv0.2"
sys.path.insert(0, os.path.join(REPO, "tools"))
import validate_cl29_vs_epa as V  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
FIG_DIR = os.path.join(HERE, "figures")
OUT_DIR = os.environ.get("CL29_FIG_OUT", os.path.join(REPO, "OUTPUTS_CL29"))
V._ACTIVE_C_TO_CHLA = V.load_c_to_chla(os.path.join(REPO, "INPUTS_CL29", "WCONST_04.txt"))

C_MODEL, C_OBS = "#2a78d6", "#eb6834"
C_TEXT, C_TEXT2, C_GRID = "#0b0b0b", "#52514e", "#d9d8d4"
MON = ["J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"]

# doc s.1 / s.50, obs-matched monthly climatologies (EPA chlorophyll; KM group carbon)
CHLA_OBS = {1: 17.6, 2: 10.2, 3: 25.1, 4: 30.8, 5: 29.5, 6: 20.9,
            7: 26.2, 8: 50.8, 9: 50.2, 10: 46.4, 11: 24.0}
CHLA_MOD = {1: 1.7, 2: 5.9, 3: 21.7, 4: 21.7, 5: 24.8, 6: 32.0,
            7: 42.3, 8: 49.4, 9: 49.7, 10: 25.7, 11: 3.8}
CYN_OBS = {8: 2.304, 9: 1.875, 10: 1.056, 11: 0.343}
FIX_OBS = {8: 1.606, 9: 1.980, 10: 0.812, 11: 0.457}
CYN_MOD = {8: 0.210, 9: 0.298, 10: 0.312, 11: 0.148}
FIX_MOD = {8: 3.729, 9: 3.563, 10: 1.577, 11: 0.034}


def style(ax):
    ax.set_facecolor("white")
    for sp in ("top", "right"):
        ax.spines[sp].set_visible(False)
    for sp in ("left", "bottom"):
        ax.spines[sp].set_color(C_GRID)
    ax.grid(True, color=C_GRID, lw=0.6, alpha=0.7)
    ax.set_axisbelow(True)
    ax.tick_params(colors=C_TEXT2, labelsize=9)


def fig_phase():
    m = sorted(CHLA_OBS)
    fig, ax = plt.subplots(figsize=(7.2, 3.6))
    ax.plot(m, [CHLA_OBS[k] for k in m], "o-", color=C_OBS, lw=2, ms=6, label="observed (EPA)")
    ax.plot(m, [CHLA_MOD[k] for k in m], "s-", color=C_MODEL, lw=2, ms=5, label="model (adopted)")
    ax.set_xticks(m); ax.set_xticklabels([MON[k - 1] for k in m])
    ax.set_ylabel("chlorophyll-a (µg L$^{-1}$)", color=C_TEXT2, fontsize=9)
    style(ax)
    ax.legend(frameon=False, fontsize=9, loc="upper left")
    ax.annotate("model peak\nSep", xy=(9, 49.7), xytext=(9.5, 60), fontsize=8, color=C_MODEL,
                arrowprops=dict(arrowstyle="->", color=C_MODEL, lw=1))
    ax.annotate("observed peak\nAug", xy=(8, 50.8), xytext=(5.6, 61), fontsize=8, color=C_OBS,
                arrowprops=dict(arrowstyle="->", color=C_OBS, lw=1))
    ax.set_ylim(0, 72)
    fig.suptitle("Chlorophyll-a seasonal cycle — adopted configuration (seasonal r = +0.74)",
                 x=0.02, ha="left", fontsize=11, fontweight="bold", color=C_TEXT)
    fig.tight_layout(rect=(0, 0, 1, 0.94))
    p = os.path.join(FIG_DIR, "seasonal_phase.png")
    fig.savefig(p, dpi=200, facecolor="white"); plt.close(fig)
    print("wrote", p)


def fig_composition():
    m = [8, 9, 10, 11]
    x = range(len(m)); w = 0.38
    fig, (a1, a2) = plt.subplots(1, 2, figsize=(9.2, 3.8))
    # left: total cyanobacterial carbon -- essentially exact
    a1.bar([i - w / 2 for i in x], [CYN_OBS[k] + FIX_OBS[k] for k in m], w,
           color=C_OBS, label="observed")
    a1.bar([i + w / 2 for i in x], [CYN_MOD[k] + FIX_MOD[k] for k in m], w,
           color=C_MODEL, label="model")
    a1.set_xticks(list(x)); a1.set_xticklabels([MON[k - 1] for k in m])
    a1.set_ylabel("mg C L$^{-1}$", color=C_TEXT2, fontsize=9)
    a1.set_title("TOTAL cyanobacterial carbon\n(Aug–Oct within 1–5 %)",
                 fontsize=9.5, color=C_TEXT, loc="left")
    style(a1); a1.legend(frameon=False, fontsize=9)
    # right: the split -- entirely wrong
    a2.bar([i - w / 2 for i in x], [CYN_OBS[k] / (CYN_OBS[k] + FIX_OBS[k]) for k in m], w,
           color=C_OBS, label="observed")
    a2.bar([i + w / 2 for i in x], [CYN_MOD[k] / (CYN_MOD[k] + FIX_MOD[k]) for k in m], w,
           color=C_MODEL, label="model")
    a2.axhline(0.5, color=C_TEXT2, lw=0.8, ls="--")
    a2.set_xticks(list(x)); a2.set_xticklabels([MON[k - 1] for k in m])
    a2.set_ylabel("CYN share of cyanobacterial carbon", color=C_TEXT2, fontsize=9)
    a2.set_ylim(0, 1)
    a2.set_title("The SPLIT between the two guilds\n(model 0.07–0.42 vs observed 0.49–0.59)",
                 fontsize=9.5, color=C_TEXT, loc="left")
    style(a2)
    fig.suptitle("Right total, wrong guild — the autumn partitioning error (doc §50)",
                 x=0.02, ha="left", fontsize=11, fontweight="bold", color=C_TEXT)
    fig.tight_layout(rect=(0, 0, 1, 0.92))
    p = os.path.join(FIG_DIR, "autumn_composition.png")
    fig.savefig(p, dpi=200, facecolor="white"); plt.close(fig)
    print("wrote", p)


if __name__ == "__main__":
    fig_phase()
    fig_composition()
