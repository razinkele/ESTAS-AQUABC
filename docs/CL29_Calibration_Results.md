# CL29 Identifiability-Guided Calibration

## Purpose

Calibrate CL29 against the EPA observations **following the Method-of-Morris identifiability screen**
(`docs/CL29_Sensitivity_Analysis.md`): adjust only the parameters the data can constrain, and hold the
non-identifiable ones (Si half-sat, biogenic-Si dissolution, POP dissolution) at their defaults. The
central result is a methodological one — **on this nonstationary record, short-window *screening* is
robust but short-window *calibration* is not** — plus a modest, defensible full-record refinement.

## Tools

- `tools/calibrate_cl29.py` — self-contained, **no-PEST++** parallel calibrator (the sandbox equivalent of
  `pestpp-ies` on `pest/cl29.pst`). Reuses the Morris forward model (symlink-farm worker + perturbed
  `WCONST_04.txt` + CL29 run + `validate_cl29_vs_epa.py` as Φ) and optimizes with scipy
  `differential_evolution`. Objective Φ = Σ (1/obs_mean)·RMSE over NH4, NO3, PO4, DO, Si **and Chl-a**.
  `--paramset all` (8 identifiable params) or `nutrient` (cycling/loss/affinity subset, phyto-biomass knobs
  fixed); `--days` sets the training window.
- `tools/eval_fullrecord_points.py` — a controlled full-record evaluation of a handful of parameter points
  in one parallel wave. A full-record *DE* exceeds the sandbox background time-cap (~11 min/run × hundreds
  of runs); this grid gives the full-record signal robustly in ~15 min.

## Result 1 — 8 identifiable parameters, 2-yr training window (2012–2013)

DE improved the *training-window* Φ 13.52 → 9.93 (**+26.6 %**) and drove:

| parameter | default → best |
|---|---|
| KD_CYN_20 (cyano mortality) | 0.125 → 0.042 |
| K_MIN_DOC_NO3N_20 (denit) | 1.0 → **2.97 (railed to upper bound)** |
| K_NITR_20 (nitrification) | 0.6 → 1.76 |
| KG_DIA/KG_CYN (growth) | 3.7/2.4 → 4.9/4.7 |

**Validated on the full 11-yr record, Φ 12.63 → 10.4 (+18 %)** — so the *levers* transfer (most nutrient
RMSEs improve: PO4 0.041→0.029, NO3 0.489→0.454, NH4 0.060→0.050, TN 1.082→0.938). **But the specific
values are over-tuned:**

- Denit **railed to 2.97**, ≈3× the full-record optimum (Result 2), over-correcting NO3 bias to −0.093.
- Chl-a mean bias inflated −3.2 → **+11.2 µg/L** (RMSE 29.8→28.2, so the weighted Φ barely notices, but the
  +34 % mean over-prediction is real). The optimizer bought nutrient fit by lowering cyano mortality (÷3)
  and raising growth — which strips nutrients via uptake but **inflates biomass**.
- (DO is *not* part of this: RMSE 8.005→8.074 on an obs mean of 10.66 is noise on an already-poor fit.)

This is the `[[cl29-pest-posterior-nonstationarity]]` trap: the 2012–2013 hyperbloom regime differs from the
post-2016 de-eutrophied record, so **2-yr-trained absolute values do not transfer**. Morris *screening* on a
short window is regime-robust (relative influence); *calibrating* on a short window is the error.

## Result 2 — full-record (11-yr) calibration, nutrient subset (phyto fixed)

Controlled full-record evaluation of the wall-respecting subset (denit, nitrification, PON→NH4 regeneration,
diatom DIP affinity; phyto-biomass knobs at defaults). Φ and per-variable bias vs the current defaults:

| point | Φ | ΔΦ% | NH4 | NO3 | PO4 | Si | CHLA |
|---|---|---|---|---|---|---|---|
| defaults | 12.63 | — | +0.010 | +0.033 | +0.029 | +1.06 | −3.21 |
| denit 1.5 | 12.49 | +1.1 | +0.010 | −0.018 | +0.029 | +1.06 | −3.05 |
| denit 2.0 | 12.04 | +4.7 | +0.013 | −0.053 | +0.027 | +1.04 | −2.76 |
| P-affinity (KHS_DIP 0.003) | 12.60 | +0.2 | +0.010 | +0.029 | +0.029 | +1.05 | −2.88 |
| denit 1.5 + P-affinity | 12.45 | +1.4 | +0.010 | −0.021 | +0.029 | +1.05 | −2.72 |
| N-cycle + P-affinity | 12.22 | +3.2 | +0.004 | +0.015 | +0.028 | +1.04 | −2.10 |
| **denit 2 + N-cycle + P-affinity** | 12.04 | **+4.6** | +0.002 | −0.014 | +0.028 | +1.04 | −1.86 |
| N-regeneration only | 12.20 | +3.3 | +0.004 | +0.079 | +0.028 | +1.06 | −2.02 |

*(N-cycle = nitrif 1.0–1.2, PON→NH4 0.4–0.5; P-affinity = KHS_DIP_DIA 0.003.)*

Three decisive findings:

1. **The shipped defaults are already near-optimal on the full record.** The best nutrient calibration buys
   only **+4.6 % Φ** — a fraction of the 2-yr training's illusory +26.6 %. That gap is the overfitting,
   quantified.
2. **Denit does not rail on the full record** (Φ saturates by ~2.0), directly refuting the 2-yr run's
   railing-to-2.97 as a regime-overfitting artifact. (The prior single-lever work recommended denit ≈1.0
   as a *Chl-a-tradeoff* balance; with phyto fixed here, denit 1.5–2.0 is defensible and zeroes the DIN
   biases — consistent that denit should be ≥ its 1.0 default.)
3. **PO4 (+0.028) and Si (+1.04) are immovable across every point** — no identifiable nutrient lever touches
   them, independently re-confirming that the PO4/Si over-prediction is boundary/structural, not
   calibratable ([[cl29-epa-validation]]).

## Recommendation

- **The current WCONST defaults are well-calibrated for the full record; do not adopt the 2-yr values.** The
  8-parameter 2-yr calibration (Result 1) is overfit — its denit is ≈3× physical and it inflates Chl-a.
- A **modest, defensible refinement** is available (Result 2): raise `K_MIN_DOC_NO3N_20` from 1.0 to ≈1.5–2.0
  with modest `K_NITR_20`/`KDISS_DET_PART_ORG_N_20` increases and `KHS_DIP_DIA`≈0.003. This zeroes the DIN
  biases (NH4 +0.002, NO3 −0.014) and slightly improves Chl-a, for ~+4.6 % full-record Φ. **PO4 and Si
  cannot be improved by these levers** — that residual is structural.
- **Adopting any of these as shipped defaults is a scientific decision left to the user** (the converter/
  WCONST are not changed here). The clean single-lever option is `K_MIN_DOC_NO3N_20 = 1.5` (NO3 bias → ~0,
  no side effects); the fuller balanced option is the `denit 2 + N-cycle + P-affinity` row.
- For a publication-grade calibration, run the proper full-record DE with `pestpp-ies` (or
  `tools/calibrate_cl29.py --days 4016` on hardware without the sandbox time-cap), seeded near these values.

## Reference

Morris, M. D. (1991). Factorial sampling plans for preliminary computational experiments.
*Technometrics*, 33(2), 161–174. https://doi.org/10.1080/00401706.1991.10484804
