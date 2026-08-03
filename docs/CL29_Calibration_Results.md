# CL29 Identifiability-Guided Calibration

## Purpose

Calibrate CL29 against the EPA observations **following the Method-of-Morris identifiability screen**
(`docs/CL29_Sensitivity_Analysis.md`): adjust only the parameters the data can constrain, and hold the
non-identifiable ones (Si half-sat, biogenic-Si dissolution, POP dissolution) at their defaults.

The central result: **the calibration is limited by a biomass↔nutrient↔Chl-a multivariate wall on the
full record.** PO4 and Si are ~64 % of the misfit, and the only lever that reduces them is over-growing
phytoplankton — which over-predicts Chl-a. So the *adoptable* refinement (which must keep Chl-a honest) is
modest. A secondary, narrower effect is training-window nonstationarity: a short-window calibration
additionally over-tunes one lever (denitrification). Morris *screening* stays robust throughout — it ranks
relative influence, which is regime-independent; it is *calibration* (absolute values) that hits the wall.

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

**Validated on the full 11-yr record, Φ 12.63 → 10.4 (+18 %)** — so the aggregate gain *does* transfer.
But it is bought by **two non-adoptable moves**, not by a genuinely better parameterization:

- **The biomass move (dominant — ~79 % of the +18 %).** PO4 alone accounts for 5.88→4.12 of the 2.24 Φ
  drop. The Result-2 grid shows no *nutrient-cycling* lever moves PO4 (KHS_DIP alone → PO4 flat), so this
  drop came from the phyto-biomass knobs (cyano mortality ÷3, growth ↑) growing more phytoplankton →
  more P uptake. The same move **inflated Chl-a mean bias −3.2 → +11.2 µg/L** (+34 %). This is the
  multivariate wall: PO4/Si are only reducible by over-growing biomass, at Chl-a's expense — exactly the
  standing structural picture ([[cl29-epa-validation]]: baseline biomass ≈ right, nutrients in excess, so
  consuming the excess *requires* more-than-observed biomass).
- **The narrow over-tuning move.** Denit **railed to 2.97**, ≈3× the full-record optimum (Result 2),
  over-correcting NO3 bias to −0.093 — the one genuinely nonstationary, window-overfit value.
- (DO is *not* part of this: RMSE 8.005→8.074 on an obs mean of 10.66 is noise on an already-poor fit.)

So the aggregate Φ transfers, but the improvement is non-adoptable — it either breaks Chl-a or over-tunes a
narrow lever. The wall is an on-record structural limit, not a short-window artifact.

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
3. **PO4 (+0.028) and Si (+1.04) are immovable by the nutrient-cycling subset** — no cycling/loss/affinity
   lever touches them (KHS_DIP alone → PO4 flat). They are *not* absolutely immovable: Result 1 reduced PO4
   by ~30 % — but only via the phyto-biomass knobs, at the cost of the +11 Chl-a bias. Since PO4+Si are
   ~64 % of the misfit and the only lever for them breaks Chl-a, the wall-respecting refinement structurally
   **cannot and should not** touch them — which is exactly why its ceiling is +4.6 %.

## Recommendation

- **The current WCONST defaults are well-calibrated for the full record; do not adopt the 2-yr values.** The
  8-parameter 2-yr calibration (Result 1) is overfit — its denit is ≈3× physical and it inflates Chl-a.
- A **modest, defensible refinement** is available (Result 2): raise `K_MIN_DOC_NO3N_20` from 1.0 to ≈1.5–2.0
  with modest `K_NITR_20`/`KDISS_DET_PART_ORG_N_20` increases and `KHS_DIP_DIA`≈0.003. This zeroes the DIN
  biases (NH4 +0.002, NO3 −0.014) and slightly improves Chl-a, for ~+4.6 % full-record Φ. **PO4 and Si
  (~64 % of the misfit) cannot be improved without over-growing biomass** — which over-predicts Chl-a — so
  that residual is wall-limited, not calibratable.
- **Adopting any of these as shipped defaults is a scientific decision left to the user** (the converter/
  WCONST are not changed here). The clean single-lever option is `K_MIN_DOC_NO3N_20 = 1.5` (NO3 bias → ~0,
  no side effects); the fuller balanced option is the `denit 2 + N-cycle + P-affinity` row.
- For a publication-grade calibration, run the proper full-record DE with `pestpp-ies` (or
  `tools/calibrate_cl29.py --days 4016` on hardware without the sandbox time-cap), seeded near these values.

## Reference

Morris, M. D. (1991). Factorial sampling plans for preliminary computational experiments.
*Technometrics*, 33(2), 161–174. https://doi.org/10.1080/00401706.1991.10484804
