#!/usr/bin/env python3
"""Surface-positioning ratchet: the state the wind statistics cannot represent.

S_POS is state-internal to the model and is never written to output, so this
figure integrates the model's OWN equation offline, from the same wind forcing
and the same adopted constants:

    X_POS  = max((EUPHOTIC_DEPTH - 0.7006) / 0.8121, W_CRIT_POS_MIN, 0)
    F_calm = CALM_FRACTION(W, X_POS)
    storm  = 1 - CALM_FRACTION(W, W_DISP_POS)
    S     += dt * (K_POS_UP * F_calm * (1 - S) - K_POS_DISP * storm * S),  S in [0,1]

with CALM_FRACTION the within-day wind CDF fitted on 96,432 ERA5 hours
(aquabc_positioning_state.f90).

⚠ ONE ASSUMPTION, verified rather than assumed. X_POS takes the
W_CRIT_POS_MIN floor throughout: EUPHOTIC_DEPTH = 4.61/K_E, and the model's
K_E runs 2.60-2.93 m-1 over the record, so the euphotic term reaches only
(4.61/2.60 - 0.7006)/0.8121 = 1.32 at its largest -- always below the 3.0 m/s
floor. The offline integration is therefore exact for this configuration, not
an approximation of it.

Writes figures/positioning_ratchet.png.
"""
import datetime as dt
import math
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))
FIG_DIR = os.path.join(HERE, "figures")
os.makedirs(FIG_DIR, exist_ok=True)
WIND = os.path.join(REPO, "INPUTS_CL29", "WIND_SPEED_TS.txt")
OUT = os.path.join(FIG_DIR, "positioning_ratchet.png")

# Adopted configuration (INPUTS_CL29/PELAGIC_MODEL_OPTIONS.txt).
K_POS_UP, K_POS_DISP = 3.0, 10.0
W_DISP_POS, W_CRIT_POS_MIN = 8.0, 3.0
STEPS_PER_DAY = 240                     # INPUT_CL29.txt
DT = 1.0 / STEPS_PER_DAY
BASE = dt.date(2012, 1, 1)


def calm_fraction(w_day, w_thresh):
    """Fraction of the day the hourly wind sits below w_thresh (fitted CDF)."""
    x = w_thresh / max(w_day, 1.0e-1)
    if x <= 1.0e-3:
        return 0.0
    l = math.log(x)
    return math.exp(min(0.0, 0.6218 * l * l + 3.8137 * l - 0.7987))


# --- wind forcing -----------------------------------------------------------
wind = []
with open(WIND) as fh:
    started = False
    for line in fh:
        if line.startswith("# TIME AND VALUES"):
            started = True
            continue
        if not started or line.startswith("#"):
            continue
        p = line.split()
        if len(p) >= 2:
            wind.append((float(p[0]), float(p[1])))
print(f"{len(wind)} daily wind records, "
      f"{wind[0][1]:.2f}-{max(w for _, w in wind):.2f} m/s")

# --- integrate the ratchet on the model's own time step ---------------------
S, days, S_daily = 0.0, [], []
for i, (t, w) in enumerate(wind):
    f_calm = calm_fraction(w, W_CRIT_POS_MIN)
    storm = 1.0 - calm_fraction(w, W_DISP_POS)
    for _ in range(STEPS_PER_DAY):
        S += DT * (K_POS_UP * f_calm * (1.0 - S) - K_POS_DISP * storm * S)
        S = max(0.0, min(1.0, S))
    days.append(t)
    S_daily.append(S)

frac_high = 100.0 * sum(1 for v in S_daily if v > 0.5) / len(S_daily)

# --- a representative summer, where the ratchet does its work ---------------
YEAR = 2015
lo = (dt.date(YEAR, 6, 1) - BASE).days
hi = (dt.date(YEAR, 9, 1) - BASE).days
sel = [i for i, d in enumerate(days) if lo <= d <= hi]
sd = [BASE + dt.timedelta(days=days[i]) for i in sel]
sw = [wind[i][1] for i in sel]
ss = [S_daily[i] for i in sel]

fig, (axa, axb) = plt.subplots(2, 1, figsize=(9.4, 5.6), sharex=True,
                               gridspec_kw={"height_ratios": [1.0, 1.25]})

axa.fill_between(sd, 0, sw, color="#c9d6e0", zorder=2)
axa.axhline(W_CRIT_POS_MIN, color="#2e7d32", linestyle=":", linewidth=1.3,
            zorder=3, label=f"formation floor {W_CRIT_POS_MIN:g} m s⁻¹")
axa.axhline(W_DISP_POS, color="#b2182b", linestyle="--", linewidth=1.3,
            zorder=3, label=f"dispersal threshold {W_DISP_POS:g} m s⁻¹")
axa.set_ylabel("wind speed\n(m s⁻¹)")
axa.legend(fontsize=7.6, frameon=True, framealpha=0.95, loc="upper right", ncol=2)
axa.grid(alpha=0.22, linewidth=0.6)
axa.set_axisbelow(True)
axa.set_title(f"The positional ratchet under observed wind, {YEAR}",
              fontsize=10.5, pad=9)

axb.fill_between(sd, 0, ss, color="#e8b4b8", zorder=2)
axb.plot(sd, ss, color="#b2182b", linewidth=1.7, zorder=3)
axb.set_ylabel("surface-positioned\nfraction  $S$")
axb.set_ylim(0, 1.02)
axb.grid(alpha=0.22, linewidth=0.6)
axb.set_axisbelow(True)
axb.axhline(0.5, color="#8a8a8a", linestyle=":", linewidth=1.0, zorder=2)
axb.text(sd[2], 0.53, f"$S$ > 0.5 on {frac_high:.1f} % of days over the full record",
         fontsize=7.4, color="#555555", va="bottom")

fig.autofmt_xdate(rotation=0, ha="center")
fig.tight_layout()
fig.savefig(OUT, dpi=200)
print("wrote", OUT)

print(f"days with S > 0.5 over the record: {frac_high:.1f} %")
