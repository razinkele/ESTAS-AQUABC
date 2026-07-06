# CL29 Model Setup — Scientific Validation

**Date:** 2026-07-06
**Scope:** Validation of the CL29 (Curonian Lagoon, 29-box, EUTROPY-derived) phytoplankton
parameterization against published literature, with emphasis on the temperature (CTMI)
cardinal points and nutrient half-saturation constants applied by
`tools/eutropy_poc/eutropy_to_estas.py` on top of `INPUTS/WCONST_04.txt`.
**Method:** Literature identified via the scite MCP; the Curonian Lagoon study
(Bartoli et al. 2018, open access) was fully retrieved and grounds the structural
validation. Most other publisher full texts were paywalled in this session, so the
cardinal-temperature and half-saturation judgments cross-check that one local study
against established phytoplankton ecology.

---

## Effective CL29 parameters (base `WCONST_04` + CL29 overrides)

| Group | T_min (°C) | **T_opt (°C)** | T_max (°C) | KG (/d) | KHS_DIN (mg/L) | KHS_DIP (mg/L) | KHS_DSi (mg/L) |
|---|---|---|---|---|---|---|---|
| Diatoms (DIA) | −2.0 | **10.0** ⟵(base 24) | 21.0 ⟵(base 35) | 3.7 | 0.010 | 0.005 | 0.013 |
| Non-fix cyano (CYN) | 15.0 | 26.0 | 34.0 ⟵(base 38) | 2.4 | 0.009 | 0.008 | — |
| N-fix cyano (FIX_CYN) | 18.0 | 26.0 | 32.0 ⟵(base 38) | 3.5 | 0.010 | 0.005 | — |
| Other algae (OPA) | 10.0 ⟵(base 9) | 17.0 ⟵(base 20) | 23.0 ⟵(base 33) | 2.9 | 0.015 | 0.006 ⟵(base 0.013) | — |
| Nostocales (NOST) | 16.0 | 26.0 | 33.0 ⟵(base 38) | 1.29 | — | — | — |

Overrides live in `CL29_WCONST_OVERRIDE` and `CL29_PHYTO_REFUGE`
(`tools/eutropy_poc/eutropy_to_estas.py`).

---

## Bottom line

The **structure** of the setup — a three-phase seasonal succession (spring diatoms →
short clear-water "other algae" phase → summer cyanobacteria) — is **strongly
supported** by direct observations of the Curonian Lagoon. But several
**cardinal-temperature values are phenomenological calibration devices, not
physiology**, and one (diatom `T_opt = 10 °C`) is defensible only because of the
specific cold-adapted spring assemblage in this lagoon. Two `T_max` values are set by
the model's *math* (avoiding a CTMI denominator singularity), not by biology.

---

## What the literature confirms

**Succession structure — SUPPORTED.** Bartoli et al. (2018), studying this exact
system, report: *"Diatoms dominate the spring phytoplankton community, after which,
following a short clear-water phase, cyanobacteria biomass increases."* This is
precisely the diatom → OPA → cyano hand-off the setup engineers. They also give
magnitudes to validate against: **Chl-a 47 ± 14 mg m⁻³ (spring diatom bloom)** vs
**96 ± 56 mg m⁻³ (summer bloom)** — summer ≈ 2× spring.

**Species identities — inform the T_opt debate.** The spring diatoms named are
*Stephanodiscus hantzschii, Aulacoseira islandica, Asterionella formosa, Diatoma
tenuis* — genuinely **cold / early-spring genera**. The summer bloom is
*Aphanizomenon flosaquae, Dolichospermum (Anabaena)* spp. (N-fixers), *Microcystis,
Planktothrix agardhii*. This means a *low realized* diatom optimum is not indefensible
for **this** assemblage, even though it would be wrong for diatoms in general.

---

## Parameter-by-parameter verdict

| Parameter | Model value | Verdict | Basis |
|---|---|---|---|
| Succession structure | diatom → OPA → cyano | ✅ **Supported** | Bartoli et al. 2018 (observed) |
| Cyano `T_opt` / `T_min` | 26 / 15 °C | ✅ **Supported** | Canonical cyano warm-preference (Paerl & Huisman; Jöhnk et al. 2008) |
| N-fix cyano `T_min` | 18 °C | 🟡 **Defensible** | Diazotrophy is warm-gated; 18 is slightly high but reasonable |
| Diatom `T_opt` | **10 °C** | 🟠 **Phenomenological** | Low for diatom *physiology* (culture optima usually 15–25 °C); defensible only via the cold spring assemblage above |
| Diatom `T_max` | **21 °C** | 🟠 **Fudge** | Forces summer die-off thermally; real decline is Si-depletion + grazing + competitive exclusion, not a thermal cutoff |
| Cyano / Nostocales `T_max` | 32–34 °C | ⚠️ **Set by math** | Lowered *to keep T_opt above the range midpoint and avoid the CTMI singularity* — a numerical constraint, not biology |
| Half-sat `KHS_DIN/DIP/DSi` | 0.005–0.015 mg/L | 🟡 **Reasonable** | High-affinity end of published Monod ranges (Si ≈ 0.46 µM, P ≈ 0.16 µM) |
| OPA (T_opt 17 + KHS_DIP 0.006 + low settling) | 3 tuned levers | 🟠 **Heavily engineered** | OPA is a catch-all group; three simultaneous knobs to manufacture a shoulder bloom — calibration, not evidence |

### The core issue: mechanism substitution

Spring-bloom *timing* in temperate systems is governed by light, mixing depth, grazing,
and Si supply — not by a cold thermal growth optimum. Encoding it as `T_opt = 10 °C` /
`T_max = 21 °C` makes the temperature-limitation term a proxy for controls the model
already represents elsewhere (it *has* `KHS_DSi` and zooplankton grazing). The result is
right-phenology-for-the-wrong-reason: robust for 2012–2017 hindcasting, but likely to
mispredict under warming, because a real +2 °C summer (which Bartoli et al. project for
this lagoon) would spuriously *kill* diatoms via the 21 °C wall rather than shifting
competition.

---

## Recommendations

1. **Relabel, don't necessarily re-fit.** Document diatom `T_opt`/`T_max` in the
   converter as *effective-niche calibration parameters*, not cardinal physiology. The
   existing code comment already frames the `T_max` lowering as a singularity workaround
   (honest); extend that framing to the diatom optimum.
2. **Shift the summer diatom decline onto mechanism.** The model already resolves Si
   (`KHS_DSi_DIA = 0.013`) and grazing. Letting Si-depletion + zooplankton end the spring
   bloom would let diatom `T_opt` rise toward literature (~15–18 °C) and make the model
   warming-robust.
3. **Sanity-check cyano µmax.** N-fix cyano `KG = 3.5/d` and non-fix `2.4/d` approach the
   diatom `3.7/d`. Real bloom-forming cyanobacteria grow *slower* than diatoms — they win
   on warmth, buoyancy, and N-fixation, not kinetics. Verify the model isn't producing the
   right biomass for the wrong reason.
4. **Validate against the observed 47 vs 96 mg m⁻³** spring/summer Chl-a ratio from
   Bartoli et al. (2018) — a concrete, local, quantitative target.

---

## Empirical follow-up (model runs, 2026-07-06)

The recommendations above were tested against the running model (box 19, full 5-year
2012–2017 hindcast, `ESTAS_HOLD_VOLUME=1`). Chl-a computed with the model's own C:Chl
ratios (DIA/OPA 30, CYN/FIX_CYN/NOST 40).

**Rec 4 — magnitude validation (baseline `T_opt=10 / T_max=21`):**

| Metric (box 19, 5-yr mean) | Model | Observed (Bartoli et al. 2018) |
|---|---|---|
| Spring diatom Chl-a | 19.6 mg/m³ | 47 ± 14 → **undershoots ~2.4×** |
| Summer cyano Chl-a | 64.6 mg/m³ | 96 ± 56 → within range, low side |
| Annual total Chl-a | 74.1 mg/m³ | — |

**Rec 3 — realized cyano growth:** despite inflated `KG` parameters (non-fix 2.4/d,
N-fix 3.5/d), the model's *realized* peak net cyano growth is **0.77/d** (0.9-day
doubling) — at the upper edge of the realistic 0.3–0.8/d range for bloom-forming
cyanobacteria. The temperature/nutrient/light limitation terms largely mask the
inflated maxima, so the biomass is not obtained by grossly unrealistic kinetics.
The `KG` values remain physically ungrounded but are not, in practice, driving the
result. *Concern mostly allayed.*

**Rec 2 — mechanism-substitution test (`T_opt=16 / T_max=28`, near-literature):**

| Metric (box 19, 5-yr mean) | Baseline (10/21) | Experiment (16/28) |
|---|---|---|
| Spring diatom Chl-a | 19.6 | **26.3** (closer to obs) |
| Summer cyano Chl-a | 64.6 | 55.5 (worse) |
| Year-3 summer cyano | 44.5 | 19.2 (near-collapse) |

Two findings, both important:

1. **`DISS_Si` never draws below ~1.4–1.7 mg/L** (≈ 100× the diatom `KHS_DSi` of
   0.013) in *either* configuration. The lagoon is Si-replete (Nemunas is Si-rich),
   so **Si-limitation cannot end the spring bloom** — the Rec 2 premise ("shift the
   decline onto Si") is invalid for this system. The thermal cutoff is load-bearing.
2. With the thermal cutoff removed (`T_max=28`), **diatoms did not run away in
   summer** — grazing + P-competition kept the summer cyano-dominated. So the
   `T_opt=10/T_max=21` fudge is *not strictly required* to preserve succession. But
   raising `T_opt` toward literature trades a better spring fit for a **weaker, less
   stable summer cyano bloom** (year 3 nearly collapses). Not a free win.

**Revised conclusion.** The empirical test says **keep the calibrated parameters** and
document the tradeoff rather than "fix" it: the low diatom `T_opt` buys a better
summer-cyano fit and interannual stability at the cost of physiological honesty; a
literature-realistic `T_opt` is feasible (grazing maintains succession) but fits worse.
The parameters are now labelled in the converter (`CL29_WCONST_OVERRIDE` comment) as
*effective-niche calibration*, not physiology.

**Open calibration gap (not addressed here):** both configurations undershoot the
observed spring diatom magnitude by roughly 2× (20–26 vs 47 mg/m³). Si is ruled out;
the likely controls are light (turbid lagoon), spring PO4 supply, grazing pressure, or
the seed/refuge level — a target for a future calibration pass, not a single-parameter
fix.

---

## References

- Bartoli, M., Zilius, M., Bresciani, M., et al. (2018). Drivers of cyanobacterial blooms
  in a hypertrophic lagoon. *Frontiers in Marine Science, 5*, 434.
  https://doi.org/10.3389/fmars.2018.00434
  *(full text retrieved; source of all Curonian succession, Chl-a, and species claims)*

**Related literature surfaced via scite** (metadata retrieved; full text access-limited
in this session, so treated as pointers rather than cited for specific numbers):

- Grimaud, G. M., Mairet, F., Sciandra, A., & Bernard, O. (2017). Modeling the temperature
  effect on the specific growth rate of phytoplankton: a review. *Reviews in Environmental
  Science and Bio/Technology, 16*, 625–645. https://doi.org/10.1007/s11157-017-9443-0
- Jöhnk, K. D., Huisman, J., Sharples, J., et al. (2008). Summer heatwaves promote blooms
  of harmful cyanobacteria. *Global Change Biology, 14*, 495–512.
  https://doi.org/10.1111/j.1365-2486.2007.01510.x
- Nausch, M., Nausch, G., & Wasmund, N. (2006). Phosphorus-limited growth dynamics in two
  Baltic Sea cyanobacteria, *Nodularia* sp. and *Aphanizomenon* sp. *FEMS Microbiology
  Ecology, 56*, 208–219. https://doi.org/10.1111/j.1574-6941.2006.00180.x

**Method caveat:** the scite MCP returned relevance-ranked results, but payloads were
trimmed to titles/DOIs (no abstracts/citation snippets) and most publisher full texts were
paywalled; only Bartoli et al. (2018, OA) was fully readable. The *structural* validation
is therefore solidly evidence-backed, while the cardinal-temperature and half-saturation
judgments lean on established phytoplankton ecology cross-checked against that one local
study. Author/year attributions for the paywalled references are best-effort matches to the
DOIs and should be verified against the source before formal citation.
