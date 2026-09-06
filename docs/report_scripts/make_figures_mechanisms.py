#!/usr/bin/env python3
"""Mechanism figures for the model-enhancements paper (Paper I).

Three figures, one per adopted mechanism where the evidence is a measurement
rather than a score:

  zoo_food_model.png      the zooplankton cycle the saturating total-food
                          response recovers, against observations
  akinete_viability.png   the five-probe viability ladder -- four geometric
                          collapses and one self-sustaining plateau -- plus the
                          benthic bank trajectory of the adopted configuration
  ice_sensitivity.png     the ice result does not rest on the one constant
                          nobody has measured in this lagoon

The zooplankton and akinete panels read the canonical full-record run in
OUTPUTS_CL29 (override with CL29_FIG_OUT). The ladder and sensitivity values
are transcribed from the recorded experiments in
docs/CL29_phenology_diagnosis.md (sections 30 and 45), which are not
reproducible from a single run.
"""
import collections
import csv
import datetime as dt
import os
import statistics as st

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))   # docs/report_scripts/ -> repo root
FIG_DIR = os.path.join(HERE, "figures")
os.makedirs(FIG_DIR, exist_ok=True)   # figures/ is git-ignored: absent in a fresh checkout
OUT_DIR = os.environ.get("CL29_FIG_OUT", os.path.join(REPO, "OUTPUTS_CL29"))
OBS = os.path.join(REPO, "km_plankton_out", "km_plankton_tidy.csv")

BASE = dt.date(2012, 1, 1)
MONTHS = ["J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"]
MONTH_NAME = ["January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December"]
INK, ACCENT, OBSC = "#1b3346", "#b2182b", "#2c2c2c"


# --------------------------------------------------------------------------
# Figure 1 -- zooplankton
# --------------------------------------------------------------------------
def monthly_model(var, boxes, out_dir):
    """Monthly mean of `var` over `boxes` from the run in `out_dir`."""
    acc = collections.defaultdict(list)
    for b in boxes:
        path = os.path.join(out_dir, "PELAGIC_BOX_%05d.out" % b)
        if not os.path.exists(path):
            continue
        with open(path) as fh:
            idx = {n: i for i, n in enumerate(fh.readline().split())}
            if var not in idx:
                continue
            for line in fh:
                p = line.split()
                if len(p) <= idx[var]:
                    continue
                t = float(p[0])
                acc[(BASE + dt.timedelta(days=t)).month].append(float(p[idx[var]]))
    return {m: st.mean(v) for m, v in acc.items() if v}


def monthly_obs(var):
    acc = collections.defaultdict(list)
    with open(OBS) as fh:
        for r in csv.DictReader(fh):
            if r["variable"] != var:
                continue
            m = dt.date.fromisoformat(r["date"]).month
            acc[m].append(float(r["value"]))
    return {m: st.mean(v) for m, v in acc.items() if v}, \
           {m: len(v) for m, v in acc.items()}


zoo_boxes = [7, 9, 11, 14, 17, 23]
obs, nobs = monthly_obs("ZOO_C")
mod = monthly_model("ZOO_C", zoo_boxes, OUT_DIR)

# The legacy comparison is a SEPARATE full-record run with ZOO_FOOD_MODEL = 0
# (which also disables the quadratic closure -- the two are coupled). It is not
# derivable from the adopted run, so the panel simply omits the series if the
# run is absent rather than inferring it.
# Prefer the raw run if it is present; otherwise fall back to the committed
# monthly summary, so the figure stays reproducible without carrying 124 MB of
# output for twelve numbers. To regenerate the run itself:
#     cp -r INPUTS_CL29 INPUTS_ZOOLEGACY
#     # set ZOO_FOOD_MODEL (entry 10) to 0 in the copy's PELAGIC_MODEL_OPTIONS.txt
#     sed -e 's|^INPUTS_CL29/$|INPUTS_ZOOLEGACY/|' -e 's|^OUTPUTS_CL29/$|OUT_ZOOLEGACY/|' \
#         INPUT_CL29.txt > INPUT_ZOOLEGACY.txt
#     ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_ZOOLEGACY.txt
LEG_DIR = os.environ.get("CL29_FIG_ZOOLEGACY", os.path.join(REPO, "OUT_ZOOLEGACY"))
LEG_CSV = os.path.join(HERE, "zoo_legacy_monthly.csv")   # tracked: the script needs it
if os.path.isdir(LEG_DIR):
    leg = monthly_model("ZOO_C", zoo_boxes, LEG_DIR)
elif os.path.exists(LEG_CSV):
    with open(LEG_CSV) as fh:
        leg = {int(r["month"]): float(r["ZOO_C_mean_mgC_per_L"])
               for r in csv.DictReader(fh)}
else:
    leg = {}

fig, ax = plt.subplots(figsize=(7.6, 4.4))
mm = list(range(1, 13))
ax.plot(mm, [obs.get(m, float("nan")) for m in mm], "o-", color=OBSC,
        linewidth=1.9, markersize=6,
        label=f"observed ({sum(nobs.values())} records, {len(zoo_boxes)} boxes)", zorder=4)
if leg:
    ax.plot(mm, [leg.get(m, float("nan")) for m in mm], "^--", color="#7a8b99",
            linewidth=1.6, markersize=5.5,
            label="legacy: summed preference-diluted Monod terms", zorder=3)
ax.plot(mm, [mod.get(m, float("nan")) for m in mm], "s-", color=ACCENT,
        linewidth=1.9, markersize=5.5,
        label="adopted: saturating total food + quadratic closure", zorder=3)

if leg:
    pk = max(obs, key=obs.get)
    ratio = obs[pk] / leg[pk] if leg.get(pk) else float("nan")
    leg_pk = max(leg, key=leg.get)
    ax.annotate(f"the legacy formulation has the wrong SHAPE, not merely the wrong\n"
                f"magnitude: it peaks in {MONTH_NAME[leg_pk - 1]} and declines through summer,\n"
                f"running {ratio:.1f}× low at the observed {MONTH_NAME[pk - 1]} peak. Its food factor\n"
                f"saturates at 0.18–0.25 because each per-prey Monod term is\n"
                f"diluted by its preference, holding ingestion below the fixed\n"
                f"losses at ANY food level — a ceiling in the functional form",
                xy=(pk, leg[pk]), xytext=(0.015, 0.60), textcoords="axes fraction",
                fontsize=7.2, color="#555555",
                arrowprops=dict(arrowstyle="->", color="#9e9e9e", linewidth=0.9,
                                connectionstyle="arc3,rad=-0.25"))

ax.set_xticks(mm)
ax.set_xticklabels(MONTHS)
ax.set_ylabel("zooplankton carbon (mg C L⁻¹)")
ax.set_title("Zooplankton: a formulation ceiling no coefficient could lift",
             fontsize=10.5, pad=9)
ax.legend(fontsize=7.8, frameon=True, framealpha=0.95, loc="upper right")
ax.grid(alpha=0.22, linewidth=0.6)
ax.set_axisbelow(True)
ax.margins(y=0.16)
fig.tight_layout()
p = os.path.join(FIG_DIR, "zoo_food_model.png")
fig.savefig(p, dpi=200)
plt.close(fig)
print("wrote", p, "" if leg else "(WITHOUT the legacy series -- run not found)")


# --------------------------------------------------------------------------
# Figure 2 -- akinete viability ladder + the adopted bank trajectory
# --------------------------------------------------------------------------
# Per-cycle return ratio, sum(dCUM_FORM)/sum(dCUM_GERM), from doc section 30.
# < 1 is geometric decay of the bank; > 1 is self-sustaining.
PROBES = [
    ("P1", "T_min 16→8 °C\n(literature cardinals)", 0.010, 0.020),
    ("P2", "KG_NOST 1.29→7.6\n(engine parity)",      0.030, 0.070),
    ("P3", "P1 + P2",                                0.050, 0.080),
    ("P5", "P3 + fast encystment\n(KR_FORM 0.1→0.5)", 0.110, 0.180),
    ("P4", "P3 + role swap\n(KG_FIX_CYN 7.62→1.29)", 1.100, 1.700),
]

fig, (axa, axb) = plt.subplots(1, 2, figsize=(11.0, 4.4),
                               gridspec_kw={"width_ratios": [1.0, 1.15]})

ys = range(len(PROBES))
for y, (tag, desc, lo, hi) in zip(ys, PROBES):
    ok = lo >= 1.0
    axa.barh(y, hi - lo, left=lo, height=0.55,
             color=(ACCENT if ok else "#9fb6c9"), edgecolor="white")
    axa.text(hi * 1.08, y, f"{lo:.2f}–{hi:.2f}", va="center", fontsize=7.6,
             color=("#7f1d1d" if ok else "#4a5b68"))
axa.axvline(1.0, color=INK, linestyle="--", linewidth=1.4, zorder=4)
axa.text(0.88, len(PROBES) - 1.9, "replacement (1.0)", fontsize=7.6, color=INK,
         rotation=90, va="center", ha="right")
axa.set_yticks(list(ys))
axa.set_yticklabels([f"{t}  {d}" for t, d, *_ in PROBES], fontsize=7.2)
axa.set_xscale("log")
axa.set_xlim(0.006, 4.0)
axa.set_xlabel("per-cycle return  ΣΔCUM_FORM / ΣΔCUM_GERM  (log scale)")
axa.set_title("(a) Five viability probes: only the role swap replaces its bank",
              fontsize=10, pad=8)
axa.grid(axis="x", alpha=0.22, linewidth=0.6)
axa.set_axisbelow(True)

# Bank trajectory of the adopted configuration.
stg = os.path.join(OUT_DIR, "NOST_STAGING.out")
if os.path.exists(stg):
    per_year = collections.defaultdict(float)
    with open(stg) as fh:
        idx = {n: i for i, n in enumerate(fh.readline().split())}
        for line in fh:
            p = line.split()
            if len(p) <= idx["BED_AKI"]:
                continue
            yr = (BASE + dt.timedelta(days=float(p[idx["WTIME"]]))).year
            per_year[yr] = max(per_year[yr], float(p[idx["BED_AKI"]]))
    yrs = sorted(per_year)
    axb.plot(yrs, [per_year[y] for y in yrs], "o-", color=ACCENT,
             linewidth=1.9, markersize=5.5, zorder=3)
    axb.set_ylabel("annual maximum benthic akinete bank (mg C L⁻¹)")
    axb.set_xlabel("year")
    axb.set_title("(b) The adopted configuration's bank, self-sustaining in production",
                  fontsize=10, pad=8)
    axb.grid(alpha=0.22, linewidth=0.6)
    axb.set_axisbelow(True)
    first, last = per_year[yrs[0]], per_year[yrs[-1]]
    axb.annotate(f"varies between winters but shows no drawdown trend:\n"
                 f"the bank ends HIGHER than it started "
                 f"({first:.1f} → {last:.1f} mg C L⁻¹),\n"
                 f"so it is reseeded each cycle rather than depleted\n"
                 f"from the initial condition",
                 xy=(0.02, 0.03), xycoords="axes fraction", fontsize=7.4,
                 color="#555555", ha="left", va="bottom")
    axb.margins(y=0.28)
else:
    axb.text(0.5, 0.5, "NOST_STAGING.out not found in\n" + OUT_DIR,
             ha="center", va="center", fontsize=9, color="#888888")
    axb.set_axis_off()

fig.tight_layout()
p = os.path.join(FIG_DIR, "akinete_viability.png")
fig.savefig(p, dpi=200)
plt.close(fig)
print("wrote", p)


# --------------------------------------------------------------------------
# Figure 3 -- ice transmittance sensitivity (doc section 45.3)
# --------------------------------------------------------------------------
T = [0.02, 0.05, 0.15]
CHLA = [23.8263, 23.8300, 23.8396]
FEB = [0.270, 0.278, 0.309]
SEAS_R = [0.74, 0.74, 0.73]
FEB_OBS = 0.280

fig, (axa, axb) = plt.subplots(1, 2, figsize=(10.2, 3.9))

axa.plot(T, CHLA, "o-", color=ACCENT, linewidth=1.9, markersize=7, zorder=3)
axa.set_xscale("log")
axa.set_xticks(T)
axa.set_xticklabels([str(t) for t in T])
axa.xaxis.set_minor_locator(matplotlib.ticker.NullLocator())   # minor log ticks collide
axa.set_xlim(0.0145, 0.21)   # room for the edge annotations
axa.set_xlabel("under-ice PAR transmittance  $T$  (log scale)")
axa.set_ylabel("chlorophyll-$a$ RMSE (µg L⁻¹)")
# Plot the flatness honestly: a +/-2 % window around the value, so a 0.013
# spread reads as the null it is rather than as a trend manufactured by zoom.
mid = sum(CHLA) / len(CHLA)
axa.set_ylim(mid * 0.98, mid * 1.02)
axa.set_title("(a) A 7.5-fold range in $T$ leaves the score unchanged",
              fontsize=10, pad=8)
for t, c, r in zip(T, CHLA, SEAS_R):
    axa.annotate(f"r = +{r:.2f}", (t, c), textcoords="offset points",
                 xytext=(0, 11), ha="center", fontsize=7.6, color="#555555")
axa.annotate(f"full spread {max(CHLA) - min(CHLA):.3f} µg L⁻¹ "
             f"({100 * (max(CHLA) - min(CHLA)) / mid:.2f} % of the RMSE);\n"
             f"axis spans ±2 % so the null is not exaggerated by zoom",
             xy=(0.03, 0.06), xycoords="axes fraction", fontsize=7.4,
             color="#555555", ha="left")
axa.grid(alpha=0.22, linewidth=0.6)
axa.set_axisbelow(True)

axb.plot(T, FEB, "o-", color=ACCENT, linewidth=1.9, markersize=7, zorder=3,
         label="modelled February diatom carbon")
axb.axhline(FEB_OBS, color=OBSC, linestyle="--", linewidth=1.4, zorder=2,
            label=f"observed ({FEB_OBS:.3f})")
axb.set_xscale("log")
axb.set_xticks(T)
axb.set_xticklabels([str(t) for t in T])
axb.xaxis.set_minor_locator(matplotlib.ticker.NullLocator())
axb.set_xlim(0.0145, 0.21)
axb.set_xlabel("under-ice PAR transmittance  $T$  (log scale)")
axb.set_ylabel("February diatom carbon (mg C L⁻¹)")
axb.set_title("(b) The adopted value is not load-bearing", fontsize=10, pad=8)
axb.legend(fontsize=8, frameon=True, framealpha=0.95, loc="lower right")
axb.grid(alpha=0.22, linewidth=0.6)
axb.set_axisbelow(True)
axb.annotate("at ~40 % mean February ice cover the multiplier\n"
             "$1-f(1-T)$ is dominated by the FRACTION iced,\n"
             "not by what the ice transmits — the physics is\n"
             "carried by the measured CMEMS series",
             xy=(0.05, FEB[1]), xytext=(0.0235, 0.2985), fontsize=7.3,
             color="#555555",
             arrowprops=dict(arrowstyle="->", color="#9e9e9e", linewidth=0.9))

fig.tight_layout()
p = os.path.join(FIG_DIR, "ice_sensitivity.png")
fig.savefig(p, dpi=200)
plt.close(fig)
print("wrote", p)
