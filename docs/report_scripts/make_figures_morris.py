#!/usr/bin/env python3
"""Morris screening figure: mu*-sigma scatter plus the ranked mu* bar chart.

Values are the recorded screen from docs/CL29_Sensitivity_Analysis.md
(r = 6 trajectories, 2-year window 2012-2013, delta = 0.4, seed 12345,
96 runs, 0 failed). Phi is the PEST-style objective:
Phi = sum_var (1/obs_mean_var) * RMSE_var over NH4, NO3, PO4, DO and Si,
each RMSE n-weighted across the nine observed boxes.

Writes figures/morris_screening.png.
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
FIG_DIR = os.path.join(HERE, "figures")
os.makedirs(FIG_DIR, exist_ok=True)   # figures/ is git-ignored: absent in a fresh checkout
OUT = os.path.join(FIG_DIR, "morris_screening.png")

# (short label, mu*, sigma, tier) -- transcribed from the doc's result table.
DATA = [
    ("KD_CYN_20",               "cyano mortality",        2.447, 0.870, "high"),
    ("K_MIN_DOC_NO3N_20",       "denitrification",        1.744, 0.919, "high"),
    ("K_NITR_20",               "nitrification",          1.070, 0.355, "high"),
    ("KG_DIA_OPT_TEMP",         "diatom growth",          0.965, 0.450, "moderate"),
    ("KG_CYN_OPT_TEMP",         "cyano growth",           0.868, 0.644, "moderate"),
    ("KDISS_DET_PART_ORG_N_20", "PON to NH4",             0.809, 0.663, "moderate"),
    ("KD_DIA_20",               "diatom mortality",       0.790, 0.524, "moderate"),
    ("KHS_DIP_DIA",             "diatom DIP half-sat.",   0.742, 0.335, "moderate"),
    ("K_MIN_DOC_DOXY_20",       "aerobic DOC min.",       0.608, 0.647, "moderate"),
    ("KHS_DIN_CYN",             "cyano DIN half-sat.",    0.576, 0.331, "moderate"),
    ("KHS_DIN_DIA",             "diatom DIN half-sat.",   0.294, 0.128, "low"),
    ("KDISS_DET_PART_ORG_P_20", "POP to PO4",             0.258, 0.139, "low"),
    ("KHS_DIP_CYN",             "cyano DIP half-sat.",    0.215, 0.121, "low"),
    ("KDISS_PART_Si_20",        "biogenic Si dissol.",    0.061, 0.018, "negligible"),
    ("KHS_DSi_DIA",             "diatom Si half-sat.",    0.028, 0.018, "negligible"),
]

COLOUR = {"high": "#b2182b", "moderate": "#2166ac",
          "low": "#7fa8c9", "negligible": "#9e9e9e"}

# Scatter label offsets in points, set so the crowded mid-field stays readable.
OFFSET = {
    "KD_CYN_20": (-9, 7), "K_MIN_DOC_NO3N_20": (-9, 8),
    "K_NITR_20": (9, -2), "KG_DIA_OPT_TEMP": (8, -8),
    "KG_CYN_OPT_TEMP": (8, 3), "KDISS_DET_PART_ORG_N_20": (-8, 7),
    "KD_DIA_20": (8, -9), "KHS_DIP_DIA": (-8, 5),
    "K_MIN_DOC_DOXY_20": (-8, -11), "KHS_DIN_CYN": (-8, -9),
    "KHS_DIN_DIA": (8, 2), "KDISS_DET_PART_ORG_P_20": (8, -8),
    "KHS_DIP_CYN": (-8, 4), "KDISS_PART_Si_20": (9, 3),
    "KHS_DSi_DIA": (9, -6),
}

fig, (axa, axb) = plt.subplots(1, 2, figsize=(11.4, 5.6),
                               gridspec_kw={"width_ratios": [1.15, 1.0]})

# ---- (a) mu*-sigma scatter --------------------------------------------------
mumax = max(d[2] for d in DATA) * 1.14
axa.plot([0, mumax], [0, mumax], linestyle="--", linewidth=0.9,
         color="#b0b0b0", zorder=1)
axa.text(1.16, 1.19, r"$\sigma=\mu^*$", fontsize=7.8,
         color="#8a8a8a", ha="left", va="bottom", rotation=52)
axa.plot([0, mumax], [0, 0.5 * mumax], linestyle=":", linewidth=0.9,
         color="#c8c8c8", zorder=1)
axa.text(2.44, 1.235, r"$\sigma=0.5\,\mu^*$", fontsize=7.8,
         color="#a8a8a8", ha="right", va="bottom", rotation=27)

for key, desc, mu, sig, tier in DATA:
    axa.scatter(mu, sig, s=58, color=COLOUR[tier], edgecolor="white",
                linewidth=0.8, zorder=3)
    dx, dy = OFFSET[key]
    axa.annotate(key, (mu, sig), textcoords="offset points", xytext=(dx, dy),
                 fontsize=6.6, color="#333333", zorder=4,
                 ha="left" if dx >= 0 else "right")

# The screen's headline: the non-identifiable corner.
axa.annotate(
    "non-identifiable\nat this record length",
    xy=(0.075, 0.03), xytext=(1.42, 0.10), fontsize=7.6, color="#616161",
    ha="left", arrowprops=dict(arrowstyle="->", color="#9e9e9e",
                               linewidth=0.9, connectionstyle="arc3,rad=-0.25"))

axa.set_xlabel(r"$\mu^*$  — influence on the objective $\Phi$", fontsize=9.5)
axa.set_ylabel(r"$\sigma$  — non-linearity / interaction", fontsize=9.5)
axa.set_title("(a) Morris screening: influence versus interaction",
              fontsize=10, pad=8)
axa.set_xlim(-0.06, mumax)
axa.set_ylim(-0.04, mumax * 0.46)
axa.grid(alpha=0.22, linewidth=0.6)
axa.set_axisbelow(True)

handles = [plt.Line2D([], [], marker="o", linestyle="none", markersize=7,
                      markerfacecolor=COLOUR[t], markeredgecolor="white",
                      label=t) for t in ["high", "moderate", "low", "negligible"]]
axa.legend(handles=handles, title="tier", fontsize=8, title_fontsize=8.5,
           loc="upper left", frameon=True, framealpha=0.95)

# ---- (b) ranked mu* ---------------------------------------------------------
rows = sorted(DATA, key=lambda d: d[2])
ypos = range(len(rows))
axb.barh(list(ypos), [d[2] for d in rows],
         color=[COLOUR[d[4]] for d in rows], edgecolor="white", height=0.72)
axb.set_yticks(list(ypos))
axb.set_yticklabels([f"{d[0]}\n({d[1]})" for d in rows], fontsize=6.3)
for y, d in zip(ypos, rows):
    axb.text(d[2] + 0.045, y, f"{d[2]:.3f}", va="center", fontsize=6.8,
             color="#444444")
axb.set_xlabel(r"$\mu^*$", fontsize=9.5)
axb.set_title(r"(b) Parameters ranked by $\mu^*$", fontsize=10, pad=8)
axb.set_xlim(0, max(d[2] for d in DATA) * 1.16)
axb.grid(axis="x", alpha=0.22, linewidth=0.6)
axb.set_axisbelow(True)

fig.subplots_adjust(left=0.075, right=0.985, top=0.915, bottom=0.115,
                    wspace=0.32)
fig.savefig(OUT, dpi=200)
print("wrote", OUT)
