# CL29 Global Sensitivity / Identifiability Analysis (Method of Morris)

## Purpose

Before (or alongside) calibration, answer the identifiability question the EUTROPY↔AQUABC comparison
paper (§11.2) raises: **which model constants can the observational data actually constrain?** A
parameter the data has no leverage over cannot be calibrated no matter how good the optimizer is —
tuning it is wasted effort (and, worse, invites over-fitting). The Method of Morris is the standard,
cheap *screening* answer: it ranks parameters by influence and flags nonlinearity/interaction.

This complements — does not replace — the PEST++ calibration workflow
(`docs/CL29_Calibration_PEST_Workflow.md`). Sensitivity screening tells you *what is worth calibrating*;
PEST++ then calibrates it.

## Method: elementary effects (Morris 1991)

For `k` parameters, Morris builds `r` "trajectories" through the normalized parameter hypercube. Each
trajectory perturbs one parameter at a time by a fixed step Δ and records the change in the model's
scalar misfit Φ. The ratio ΔΦ/Δ for parameter *i* is one **elementary effect**. Over `r` trajectories
each parameter gets `r` elementary effects, summarized by:

- **μ\*** — mean of the *absolute* elementary effects. The **influence** measure: high μ\* ⇒ the data
  responds strongly to this parameter; μ\* ≈ 0 ⇒ the data cannot constrain it (**non-identifiable**).
- **σ** — standard deviation of the (signed) elementary effects. High σ ⇒ the parameter's effect depends
  on where you are in parameter space ⇒ **nonlinearity or interaction** with other parameters.

Cost is `(k+1)·r` model runs — cheap enough to run many parameters, unlike Sobol (`(2k+2)·N`, N≈500+),
which quantifies interaction variances precisely but is overkill for a screening question.

**What μ\* is and is not.** μ\* measures influence on the *aggregate scalar* misfit. It is **necessary
but not sufficient** for identifiability: a low μ\* is a reliable "cannot be constrained" verdict, but a
high μ\* does not prove a parameter is *separately* identifiable — Morris cannot detect two influential
parameters that are confounded with each other, and it cannot see multivariate trade-offs across the
individual observed variables (see the reconciliation note below).

## Objective function Φ

Same construction as the PEST objective (`tools/validate_cl29_vs_epa.py`): for the scored EPA state
variables (NH4, NO3, PO4, DO, Si), Φ = Σ_var (1/obs_mean_var)·RMSE_var, with each variable's RMSE
n-weighted across the 9 observed boxes. The `1/obs_mean` weighting puts the variables on a comparable
scale so no single high-magnitude variable (DO, Si) dominates the screen. Φ is **bit-reproducible** for a
given parameter vector (the CL29 binary is a serial, deterministic build), so even the small bottom-tail
elementary effects are real signal rather than run-to-run noise.

## The parameter set (curated, k=15)

Screening all ~318 WCONST constants is both infeasible (thousands of runs) and uninformative — most
(Fe/Mn/S redox speciation, allelopathy, akinete recruitment) are structurally irrelevant to the scored
variables. The curated set targets the constants where the EPA data plausibly has leverage:

| group | parameters |
|---|---|
| N regeneration / loss | K_MIN_DOC_NO3N_20 (denit), KDISS_DET_PART_ORG_N_20 (PON→NH4), K_NITR_20 (nitrification), K_MIN_DOC_DOXY_20 (aerobic DOC min → DO) |
| P regeneration | KDISS_DET_PART_ORG_P_20 (POP→PO4) |
| Si | KHS_DSi_DIA (diatom Si half-sat), KDISS_PART_Si_20 (biogenic Si diss) |
| diatom kinetics | KG_DIA_OPT_TEMP (growth), KD_DIA_20 (mortality), KHS_DIN_DIA, KHS_DIP_DIA |
| cyano kinetics | KG_CYN_OPT_TEMP (growth), KD_CYN_20 (mortality), KHS_DIN_CYN, KHS_DIP_CYN |

Ranges: multiplicative (log) ≈ ×[0.3, 3] of the default for rate/half-sat constants; linear for the
temperature-optimum growth rates. Exact bounds are in `tools/sensitivity_morris.py` (`PARAMS`).

## Running it (sandbox-runnable, no external dependency)

```bash
# ~96 short-window CL29 runs, parallel on the local cores; ~12-15 min
python3 tools/sensitivity_morris.py --trajectories 6 --workers 24 --days 730
```

The tool reuses the forward model as-is: each run is a symlink-farm worker directory (symlinks to
`INPUTS_CL29/` + a perturbed `WCONST_04.txt` + a short-window driver), a `ESTAS_HOLD_VOLUME=1` CL29 run,
and the validator as Φ. It requires only Python 3 stdlib + the validator — **no PEST++ install**. A short
(2-year) window keeps each run to ~75 s; extend `--days` for a full-record screen if compute allows.

## Full-record, industry-standard equivalent (`pestpp-sen`)

The PEST++ suite includes `pestpp-sen`, which runs the Method of Morris (and Sobol) directly on the same
control file the calibration harness produces (`pest/cl29.pst`). On a machine with PEST++ installed and a
full-record run budget:

```bash
# reuse the pest/ harness; swap the ++ies_* options for ++gsa_* (Morris)
pestpp-sen cl29.pst   # ++gsa_method(morris)  ++gsa_morris_r(...)  ++gsa_morris_p(...)
```

Use this for a publication-grade, full 11-year screen with worker parallelism across nodes. The
`sensitivity_morris.py` tool is the self-contained, short-window equivalent for quick in-repo screening.

## Result (r=6, 2-year window [2012–2013], Δ=0.4, seed 12345, 96 runs, 0 failed)

| rank | parameter | μ\* (influence) | σ (interaction) | tier |
|---|---|---|---|---|
| 1 | KD_CYN_20 (cyano mortality)         | 2.447  | 0.870 | **high** |
| 2 | K_MIN_DOC_NO3N_20 (denitrification) | 1.744  | 0.919 | **high** |
| 3 | K_NITR_20 (nitrification)           | 1.070  | 0.355 | **high** |
| 4 | KG_DIA_OPT_TEMP (diatom growth)     | 0.965  | 0.450 | moderate |
| 5 | KG_CYN_OPT_TEMP (cyano growth)      | 0.868  | 0.644 | moderate |
| 6 | KDISS_DET_PART_ORG_N_20 (PON→NH4)   | 0.809  | 0.663 | moderate |
| 7 | KD_DIA_20 (diatom mortality)        | 0.790  | 0.524 | moderate |
| 8 | KHS_DIP_DIA (diatom DIP half-sat)   | 0.742  | 0.335 | moderate |
| 9 | K_MIN_DOC_DOXY_20 (aerobic DOC min) | 0.608  | 0.647 | moderate |
| 10 | KHS_DIN_CYN (cyano DIN half-sat)   | 0.576  | 0.331 | moderate |
| 11 | KHS_DIN_DIA (diatom DIN half-sat)  | 0.294  | 0.128 | low |
| 12 | KDISS_DET_PART_ORG_P_20 (POP→PO4)  | 0.258  | 0.139 | low |
| 13 | KHS_DIP_CYN (cyano DIP half-sat)   | 0.215  | 0.121 | low |
| 14 | KDISS_PART_Si_20 (biogenic Si diss)| 0.061  | 0.018 | **negligible** |
| 15 | KHS_DSi_DIA (diatom Si half-sat)   | 0.028  | 0.018 | **negligible** |

(Tiers, not exact ranks, are the robust output of an r=6 screen.)

## Interpretation

**1. The data does constrain phytoplankton kinetics.** The single most influential parameter is
cyanobacteria mortality (KD_CYN_20), and diatom/cyano *growth* rates rank 4th/5th. Phytoplankton growth
and mortality are **not** inert — the summer bloom's turnover controls the whole N/P/DO balance, so the
EPA data has strong leverage on it. Any facile "only regeneration matters, phytoplankton is irrelevant"
reading is wrong.

**2. Nitrogen turnover and loss terms are the other high-leverage group.** Denitrification (#2),
nitrification (#3), and PON→NH4 dissolution (#6) all rank high — consistent with the DIN budget being
governed by regeneration and loss rather than by uptake half-saturation.

**3. Half-saturation constants mostly rank low.** Except diatom DIP (KHS_DIP_DIA, #8), the nutrient
half-saturations (KHS_DIN_DIA #11, KHS_DIP_CYN #13, KHS_DIN_CYN #10, KHS_DSi_DIA #15) carry little
influence. This is the fingerprint of a system where DIN is regeneration-floored so its half-saturation
rarely bites, while phosphorus can still limit diatoms (KHS_DIP_DIA does matter). It also means these
constants are poorly identifiable from this data and should be fixed to literature values, not calibrated.

**4. Silicon kinetics are non-identifiable in this screen.** KHS_DSi_DIA and KDISS_PART_Si_20 sit ~40–85×
below the top influencer. **Caveat:** the 2-year window under-weights slow processes (Si dissolution,
P burial/turnover), so part of the low Si/POP-dissolution influence is a window artifact rather than
universal non-identifiability — this is exactly what a full-record `pestpp-sen` run should resolve before
any parameter is dropped from calibration on this basis.

**5. Reconciliation with the structural over-prediction conclusion.** The project's structural finding is
that the summer nutrient over-prediction is boundary/regeneration-structural and *not* phyto-fixable
(three phyto-side levers died on the regeneration-floored-DIN wall — see
`docs/superpowers/` FIX_CYN and variable-stoichiometry write-ups, and the CL29↔EPA validation notes).
This screen is fully consistent with that: **high μ\* means Φ is sensitive to a parameter, not that a
setting of it exists that reduces the misfit without regressing another variable.** Morris collapses all
variables into one scalar Φ and cannot see the multivariate trade-off that is the actual wall. Cyano
mortality moving Φ the most simply confirms the bloom is the dominant dynamical knob — it does not
reopen the phyto-fixability question the multivariate experiments closed.

## Actionable implications for calibration

- **Prioritize the high/moderate tier** (KD_CYN_20, K_MIN_DOC_NO3N_20, K_NITR_20, KG_DIA_OPT_TEMP,
  KG_CYN_OPT_TEMP, KDISS_DET_PART_ORG_N_20, KD_DIA_20, KHS_DIP_DIA) as PEST adjustable parameters.
- **The current 5-parameter PEST set** (`pest/build_pest.py`) includes KHS_DSi_DIA (dead last here) and
  KDISS_DET_PART_ORG_P_20 (#12) — both low-leverage in this window. Cyano mortality (KD_CYN_20), the top
  influencer, is absent. Consider adding KD_CYN_20/K_NITR_20/KG_CYN_OPT_TEMP and — pending a full-record
  `pestpp-sen` confirmation of the Si/P window caveat — de-emphasizing KHS_DSi_DIA.
- **Fix, don't calibrate, the negligible tier** (Si half-sat, biogenic-Si dissolution) unless the
  full-record screen shows real leverage.

## Reference

Morris, M. D. (1991). Factorial sampling plans for preliminary computational experiments.
*Technometrics*, 33(2), 161–174. https://doi.org/10.1080/00401706.1991.10484804
