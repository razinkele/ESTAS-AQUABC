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

## Adopted refinement (2026-08-03)

The defensible, wall-respecting refinement was **adopted** into `CL29_WCONST_OVERRIDE`
(`tools/eutropy_poc/eutropy_to_estas.py`) after a full-record verification:

| parameter | default → adopted |
|---|---|
| `K_MIN_DOC_NO3N_20` (denit) | 1.0 → **1.5** |
| `K_NITR_20` (nitrification) | 0.6 → **1.0** |
| `KDISS_DET_PART_ORG_N_20` (PON→NH4) | 0.25 → **0.4** |
| `KHS_DIP_DIA` (diatom P-affinity) | 0.005 → **0.003** |

**Verified on the full 11-yr record:** Φ 12.63 → **12.22 (+3.2 %)**; DIN biases zeroed (NH4 +0.010→+0.004,
NO3 +0.033→+0.015); **Chl-a bias improves** −3.2→−2.1 (no regression); PO4/Si unchanged (structural). The
`KHS_DIP_DIA` change was checked against the deliberately-tuned diatom/OPA phosphate competition: OPA
annual-mean carbon drops only ~4–6 % across boxes (DIA rises ~2–3 %) — the OPA shoulder-bloom **survives,
not starved**.

**Not adopted / out of scope:** the 8-parameter 2-yr calibration (Result 1) — overfit (denit ≈3× physical,
Chl-a +11). **PO4 and Si (~64 % of the misfit) are not calibratable** by any adoptable lever — reducing them
requires over-growing biomass, which over-predicts Chl-a (the wall). The remaining PO4/Si lever is the
open-boundary forcing / removal balance ([[cl29-epa-validation]]), not phytoplankton or nutrient kinetics.

For a publication-grade calibration, run the proper full-record DE with `pestpp-ies` (or
`tools/calibrate_cl29.py --days 4016` on hardware without the sandbox time-cap), seeded near these values.

## Summer PO4 — a documented structural residual (2026-08-04)

The `--by-season` breakdown (`validate_cl29_vs_epa.py --by-season`, PR #105) localizes the post-v0.10.0
residual: NO3/NH4 are well-fit in **every** season, Si and PO4 match in **winter**, and the entire
remaining misfit is a **summer failure** — most sharply, summer PO4 is ~10× over-predicted (obs 0.005 →
model 0.047: the model does not draw phosphate down during the bloom). The aggregate Φ is *pessimistic*
relative to this seasonal view.

Summer PO4 is a **structural residual** — it has now resisted three independent levers, all failing on the
same biomass↔nutrient↔Chl-a wall:

1. **Over-grow biomass** (the 8-parameter calibration, Result 1) — reduces PO4 but overshoots Chl-a (+11).
2. **Remove P via a benthic sink** (config-only summer-peaked PO4 sink, prototype Approach 1; the same
   prescribed-flux hook as `CL29_BENTHIC_DENIT`, retargeted to state var 3 PO4_P) — closes the PO4 gap
   (0.047→0.007) but only by starving the bloom: summer Chl-a crashes 25→8 (obs 33) and NO3/Si rise at
   **every** magnitude tested (0.1–0.5 mmol P/m²/d; no clean setting), with the fixed flux over-drawing PO4
   to the concentration floor. **Not adopted.**
3. **Variable P:C / luxury-P uptake (Droop-P)** — not viable and not pursued: the model's baseline summer
   bloom is nutrient-**replete** (LIM_P = 0.85 cyano / 0.94 diatom, LIM_N = 0.90; Monod C/(C+KHS)), so it is
   not P-limited. Luxury-P storage would draw P into storage, not biomass, leaving the (temperature/
   phenology-limited) Chl-a deficit untouched; and forcing P-limitation is exactly what crashes the bloom in
   (2). It fails the standing precondition — establish a genuinely uptake-limited target (`LIM≪1`) before any
   var-stoich work — and would be a large new-state-variable build.

Root cause: the real lagoon sustains **low PO4 *and* high Chl-a simultaneously**, which the model's
nutrient-replete, temperature/phenology-limited summer bloom cannot reproduce — no nutrient-side or
stoichiometry-side lever reaches obs PO4 without breaking Chl-a. The only remaining candidate is the one
that stands for the whole nutrient budget: **the open-boundary forcing / removal balance** (the summer P
*supply*), not a phytoplankton, sediment, or stoichiometry mechanism. See `CL29_EPA_Calibration_Summary.md`.

**Open-boundary P supply — checked; small clean gain adopted (2026-08-06).** The converter's summer PO4
boost (`CL29_BOUNDARY_PO4_SUMMER_PEAK`) was an eutrophic-era tuning that *doubled* the realistic summer
river low (0.0095→0.019 mg P/L). A full-record `--by-season` sweep of the boost showed reducing *supply* is
**clean** where the benthic sink was not — Monod uptake self-regulates, so LIM_P stays replete (0.86→0.81
across PEAK 2.0→0.5), Chl-a is essentially untouched (25.45→24.87), and there is no over-draw or NO3/Si side
effect. **Removing the boost (PEAK 2.0→1.0) was adopted** — summer PO4 0.047→0.038 (~20 %) at ~zero cost, a
defensible realism correction for the de-eutrophied record. It is **not** a gap-closer, and that is itself
the diagnostic: the model's interior summer PO4 (0.038) *exceeds* the boundary PO4 (~0.02), so the residual
is **internal-regeneration-dominated, not boundary-supplied** — confirming the structural conclusion from
the supply side. Closing it would require a P *removal* process (burial/retention) the water-column model
lacks and that no config-only lever can add without breaking Chl-a.

## Reference

Morris, M. D. (1991). Factorial sampling plans for preliminary computational experiments.
*Technometrics*, 33(2), 161–174. https://doi.org/10.1080/00401706.1991.10484804
