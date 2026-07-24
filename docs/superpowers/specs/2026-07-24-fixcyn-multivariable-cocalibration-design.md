# FIX_CYN Multivariable Co-Calibration — Design

**Date:** 2026-07-24
**Status:** ⛔ **BLOCKED by in-loop review (2026-07-25) — do NOT execute as specified.** Three
adversarial reviews (methodology, model-mechanism, feasibility) converged: the plumbing is fixable
but the calibration is **ill-posed at the premise level**. Reformulation required. See "In-loop
review outcome" below.

## In-loop review outcome (2026-07-25)

**Verdict: the co-calibration as designed cannot succeed and must not be run.** Three blocking
structural findings (each independently sufficient), plus fixable hardening, plus what to preserve.

### Blocking (structural — no parameter search resolves these)

1. **⛔ The FIX_CYN biomass targets and EPA Chl-a targets are numerically CONTRADICTORY at the
   overlapping boxes.** FIX_CYN enters modeled Chl-a additively as `biomass/40 × 1000` µg/L
   (`validate_cl29_vs_epa.py:45,70`). So the biomass target alone forces a minimum Chl-a:
   box 14 → 6.06 gC/m³ ⇒ **151 µg/L** vs total EPA Chl-a **65.9** (230 %, impossible);
   box 23 → 4.23 ⇒ **106** vs **90.6** (117 %, impossible). At the two boxes carrying ~84 % of the
   biomass-group weight, FIX_CYN alone demands more chlorophyll than the *entire* measured Chl-a.
   Root cause: co-calibrating a **2009/2015 peak-campaign biomass** (biovolume→C) against a
   **2012–21 decadal-mean extracted Chl-a** through a single fixed C:Chl=40 — non-commensurable
   bases/eras. Corroborated by the N budget (6.06 gC/m³ × N:C 0.22 = 1.33 gN/m³, matches the
   observed TN +56 %). Survives a 2× biomass-error revision. **No 6-param set fits both groups.**

2. **⛔ High-DIN fixation wall — "FIX_CYN present" and "realistic fixation" are mutually exclusive
   in CL29 summer.** The fixing fraction is gated by `K_FIX/(K_FIX+DIN)`
   (`aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90:203`). CL29 summer DIN is high (boundary-dominated
   over-prediction), so the switch ≈ 0.09 (mostly OFF): FIX_CYN can only reach observed biomass by
   growing as a **non-fixing** cyano (`:216`), i.e. redundant with CYN and fixing ~nothing. The
   6 params cannot create the low-DIN/replete-P niche (set by the open boundary). Same
   competitive-exclusion-under-nutrient-replete wall already hit for Nostocales.

3. **⛔ Composition degeneracy + Chl-neutrality is double-edged → silent drive-to-FIX-zero.** The
   obs use *total* Chl-a (sums DIA/CYN/OPA/FIX_CYN/NOST, `aquabc_II_pelagic_model.f90:1005-1007`)
   with **no CYN and no OPA constraint**. The "chl-neutral CYN↔FIX swap" enabler also makes Chl-a
   *uninformative* about the FIX/CYN partition, and the freed resource from lowering KG_CYN flows
   naturally to **OPA** (T_opt=17 °C, near-ideal for the cool summer; C:Chl=30, which *inflates*
   Chl), not FIX_CYN — there is no OPA lever. With obs ~2:1 against FIX presence and the presence
   check only post-hoc (not in-loop), the phi-minimizer will re-exclude FIX_CYN, report low
   aggregate phi, and silently reproduce the original problem while looking "converged."

### Fixable hardening (were the premise sound)

- Per-group weights (reused `obs_weight()` returns one weight → silently fits CHLA only); use
  `++ies_no_noise(true)` (weight=1/mean implies ~100 % obs noise); align the 2-yr proxy objective
  with the 11-yr acceptance metric (report per-group phi both ways); harden soft thresholds to
  a-priori hard numbers.
- Build each group's box list from its OWN obs availability (fixcyn 8 vs chla/si 9; boxes 18/24/25
  have no Chl-a/Si → would fabricate 0-targets); build writes the authoritative ordered manifest,
  forward_run consumes it.
- Copy `tools/validate_cl29_vs_epa.py` + `pest_fixcyn_multi/` into each PANTHER worker (workers have
  no `tools/`); `.pst` command `python3 …`.
- Move R_FIX/K_FIX to post-hoc (near-non-identifiable in-loop); calibrate the 4 identifiable params.
- Post-hoc N-fix: map via `station.box_id` (Nida→23, Vidmares→18), NOT `station_pts`; filter
  `statistic='Average'`; reconstruct with the FULL `LIM_KG_FIX_FIX_CYN` (DIN switch + LIM_P +
  LIM_LIGHT), not temperature alone (else ~10× overestimate); pin the µmol N2 vs N ×2 unit basis.

### Preserve (validated correct)

Reuse of the validator's `add_derived`/`load_box_output` (proxy CHLA definitionally identical to
the main-setup validation); R_FIX low bound as the N-fix guard; per-box summer-mean aggregation
(balances group counts 8/9/9 — load-bearing); the honest "forced regression = valid finding"
framing; the init-within-bounds build guard.

### Stale note

Spec §Parameters says "WCONST ships R_FIX=1.0, outside the bound" — this file already ships 0.1;
`INIT_OVERRIDE` is a no-op here. All 6 inits pass the bounds guard.

---
*Original design below — retained for the record; superseded by the reformulation decision.*

**Status (original):** design (approved in brainstorming)

## Problem

The biomass-only FIX_CYN calibration (`pest_fixcyn/`) correctly diagnosed that `FIX_CYN_OPT_TEMP`
T_opt=26 °C excludes the N2-fixing cyanobacteria from the cool Curonian summer, and lowering it to
~22 °C grows FIX_CYN to observed biomass (2.6–3.7 gC/m³ summer-mean). **But** an EPA/KM A/B showed
that activating FIX_CYN at those params **regresses the multivariable fit**: Chl-a +52 %, Si +64 %,
TN +56 % (EPA). Root cause: the model grows FIX_CYN biomass *on top of* the existing phytoplankton
rather than *competitively replacing* it, so total Chl-a ~doubles and diatom Si-drawdown drops.

## Goal

Calibrate FIX_CYN so it reaches observed biomass **without** breaking the Chl-a / Si fit, by giving
the calibration levers to shrink non-fixing cyanobacteria (and adjust diatoms) as FIX_CYN grows —
a partial phytoplankton-community recalibration.

**Key enabler:** `CYN_C_TO_CHLA` = `FIX_CYN_C_TO_CHLA` = **40** (identical). A 1:1 CYN→FIX_CYN
biomass swap is chlorophyll-neutral, so reducing `KG_CYN_OPT_TEMP` lets FIX_CYN fill the cyano niche
with **no Chl-a inflation** — the physical path that makes this resolvable, not just a biomass tradeoff.

## Architecture

pestpp-ies (iterative ensemble smoother) over 6 WCONST parameters against 3 per-box summer-mean
observation groups, on the fast CL29 2023-climatology setup, run via PANTHER parallel workers.
Reuses the `pest_fixcyn/` harness pattern (stdlib + numpy; no pyemu). New self-contained directory
`pest_fixcyn_multi/` leaves the biomass-only result intact.

### Parameters (6) — templated into `INPUTS_CL29_2023clim/WCONST_04.txt`

| WCONST param | init | bounds | transform | role |
|---|---|---|---|---|
| `FIX_CYN_OPT_TEMP_UR` (T_opt) | 22.0 | [20.0, 27.0] | none | FIX_CYN thermal optimum |
| `FIX_CYN_OPT_TEMP_LR` (T_min) | 15.0 | [10.0, 18.0] | none | FIX_CYN thermal minimum |
| `R_FIX` | 0.10 | [0.05, 0.20] | log | fixation rate ratio; low bound = **N-fix rate guard** |
| `K_FIX` | 0.05 | [0.005, 0.30] | log | DIN-inhibition switch effectivity |
| `KG_CYN_OPT_TEMP` | 2.4 | [1.0, 2.8] | none | **non-fixing cyano growth — the replacement lever** |
| `KG_DIA_OPT_TEMP` | 3.7 | [2.5, 5.0] | none | diatom growth — tunes Si drawdown |

Initial values via `INIT_OVERRIDE` (WCONST ships R_FIX=1.0, outside the bound). `T_min < T_opt`
always holds (bound maxima 18 < 20). Guard: build fails if any init falls outside its bounds.

### Observation groups (3, per-box summer-mean; weight = 1/group-mean for scale balance)

| group | model quantity | obs source | notes |
|---|---|---|---|
| `fixcyn` | FIX_CYN_C summer-mean | `pest_fixcyn/fixcyn_obs.csv` (8 boxes; curonian_db) | reused verbatim |
| `chla` | derived CHLA summer-mean | EPA tidy CSV, var=CHLA, Jun–Sep, per-box mean | derived via validator |
| `si` | DISS_Si summer-mean | EPA tidy CSV, var=Si, Jun–Sep, per-box mean | direct output col 18 |

- **CHLA is derived identically to the validator** (`load_box_output`/`add_derived`: Σ phyto-C/C:Chl),
  so the fast summer-mean fit is consistent with the main-setup validation.
- **Summer window** = year-2 days 517–638 (Jun 1–Sep 30, day 0 = Jan 1; validated: FIX_CYN peaks
  day ~612).
- **Weighting:** per-obs weight = 1/mean(|value|) within each group, so the three groups (scales
  ~1–6 gC/m³ vs ~30–100 µg/L vs ~1–3 mg/L) contribute comparably. If group obs-counts are very
  unequal after building, the plan may additionally normalise by √n (noted, decided at build).

### Forward model (`forward_run_multi.py`)

1. `OMP_NUM_THREADS=4 ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_grid.txt` → `OUTPUTS_grid/` (~76 s).
2. For each obs box, load with the validator's `load_box_output` and take the year-2 summer-mean of
   `FIX_CYN_C` / `CHLA` / `DISS_Si`.
3. Write `model_obs.out`, one value per obs in `.pst`/`.ins` order.

### pestpp-ies config

PANTHER master + 6 workers (28-core box; worker copies in scratchpad, `python3` command).
`num_reals=20`, `noptmax=3`, single-lambda cost control (`ies_lambda_mults(1.0)`,
`lambda_scale_fac(1.0)`, `ies_subset_size(-1)`) → ~80 runs / 6 workers ≈ 25–30 min.

### N-fixation rate — R_FIX guard + post-hoc check (`posthoc_nfix.py`)

The model emits state variables, not process rates, so no clean in-loop `nfix` group. Instead:
- **In-loop:** `R_FIX ∈ [0.05, 0.20]` (the measured specific rate ~0.02 µmol N2 l⁻¹ h⁻¹ implies a
  low R_FIX; the bound prevents biomass/Chl-a pressure from inflating the fixation rate).
- **Post-hoc:** after calibration, reconstruct the model summer specific fixation rate at the
  Nida/Vidmarès boxes — `R_FIX·KG_FIX_CYN·LIM(T_summer; calibrated T_opt/T_min)·FIX_CYN_N_TO_C` using
  the calibrated params + model summer temperature — convert to µmol N2 l⁻¹ h⁻¹, compare to the 28
  `process_rate` obs (Nida/Vidmarès → CL29 boxes via `station_pts`∩`box_poly`). Reported as a
  sanity check, not fit in-loop.

## Final confirmation (main 11-yr setup)

Apply the calibrated 6 params to `INPUTS_CL29/WCONST_04.txt`, run `INPUT_CL29.txt` (11-yr), score vs
EPA + KM with the native validator, and run `compare_validation_runs.py` OFF vs ON. Restore
`INPUTS_CL29/` to pristine afterward (it stays the untouched EPA baseline unless separately promoted).

## Success criteria

1. **FIX_CYN present:** summer-mean FIX_CYN_C in the bloom boxes ≥ ~1 gC/m³ (not re-excluded to 0).
2. **Chl-a not regressed:** on the final main-setup validation, CHLA RMSE rise vs the FIX_CYN-off
   baseline is ≤ ~10 % (vs +52 % at the biomass-only params) — the core objective.
3. **Si not regressed:** DISS_Si RMSE rise ≤ ~10 % (vs +64 %).
4. **Fixation realistic:** post-hoc specific fixation rate within ~1 order of the measured median.
5. Model runs stably to completion (2-yr calibration + 11-yr confirmation, HOLD_VOLUME=1).

A result that FIX_CYN can only stay present by regressing Chl-a/Si past these thresholds is itself a
valid finding (a real model-structure limit), to be reported rather than forced.

## File structure (`pest_fixcyn_multi/`)

- `build_pest_multi.py` — read WCONST, template 6 params, build obs (biomass from `fixcyn_obs.csv`;
  chla/si aggregated from the EPA tidy CSV), write `wconst.tpl` / `model_obs.ins` / `cl29_multi.pst`.
- `forward_run_multi.py` — run + extract 3 groups' per-box summer-means via `load_box_output`.
- `fixcyn_obs.csv` — biomass obs (copy of `pest_fixcyn/fixcyn_obs.csv`).
- `chla_si_obs.csv` — EPA-aggregated per-box summer-mean CHLA + Si targets (written by build for
  transparency/audit).
- `posthoc_nfix.py` — post-calibration fixation-rate sanity check vs `process_rate`.
- `README.md` — how to build/run/confirm; notes the experimental status.
- scratchpad: worker dirs + launch script (as in the biomass-only run).

## Risks / open points

- **Community recalibration may shift other fields.** Lowering `KG_CYN`/moving `KG_DIA` could affect
  nutrients beyond Si. The final main-setup validation checks NH4/NO3/PO4/TN too; a broad regression
  is a reportable negative result.
- **`KG_DIA` direction is coupled** (more diatoms → more Si drawdown but more Chl-a). The 6-param
  ensemble resolves the combination; no manual pre-judgement.
- **Post-hoc N-fix reconstruction is approximate** (LIM proxy from calibrated CTMI + summer T). It is
  a sanity check; a precise in-loop rate would need a model fixation-rate diagnostic (future work).
- **EPA obs are actual-year (2012–21); the 2023-clim run is climatological.** Chl-a/Si targets are
  therefore climatological per-box summer-means — appropriate for the fast proxy; the main-setup
  confirmation uses date-specific EPA/KM matching.
