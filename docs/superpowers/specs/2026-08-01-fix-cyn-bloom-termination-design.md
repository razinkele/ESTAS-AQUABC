# FIX_CYN density-dependent bloom termination — design

**Issue:** [#76 — Phytoplankton phenology: N-fixers form a persistent bloom, not the observed late-summer spike-and-crash](https://github.com/razinkele/ESTAS-AQUABC/issues/76)
**Date:** 2026-08-01
**Status:** ⚠️ **Revised after in-loop review (2026-08-01) — a necessity gate is now REQUIRED before
any implementation plan. See §12.** The code indicates the winter clear is temperature-forced, so
the quadratic term's necessity is unproven; one experiment settles it.
**Scope tier chosen:** Capability + first-cut (config-gated, default-off, byte-identical); full calibration is a documented follow-on.

---

## 1. Problem

In the CL29 Curonian Lagoon setup, AQUABC reproduces the **wrong phytoplankton phenology**,
and this is the root cause of the two largest remaining skill gaps: a poor chlorophyll fit and
a 2–9× phosphate over-prediction. The model's total chlorophyll peaks in **January** (a
spurious cold-water diatom bloom); the lagoon peaks in **September** (~150 µg L⁻¹, the
documented *Aphanizomenon/Nodularia* diazotrophic bloom).

The controlling defect is in the fixing-cyanobacteria (`FIX_CYN`) formulation. Once the group's
temperature window is opened so it can grow in the late-summer P-replete / N-poor niche where it
should dominate, it forms a **persistent, year-round plateau** (chl 44–73 µg L⁻¹ every month,
including January) instead of a sharp **spike-and-crash**. A controlled test confirmed the
mechanistic link: enabling the fixers shifts the chlorophyll peak to September **and** draws the
P-replete summer water down — one change fixing both residuals.

### 1.1 Why constants are not enough (already established)

The external calibration campaign (`~/curonian/harness/results/`) proved this is a
**formulation** problem, not a tuning problem:

- **Temperature window × growth rate** (`FIX_CYN_OPT_TEMP_LR` ∈ {16,17,18} × `KG_FIX_CYN` ∈
  {2.5,3.5}): byte-identical results — the bloom self-sustains (fix N → grow → fix N),
  insensitive to temperature and growth rate.
- **Mortality × grazing** (`KD_FIX_CYN` × `PREF_ZOO_FIX_CYN`): grazing had no effect; linear
  mortality only *trades* problems — `KD=0.25` (best chl-a RMSE 34.7) still leaves winter chl ~3×
  obs, `KD≥0.5` collapses the bloom or lets the winter diatoms return. **No mortality value gives
  both a September spike and a winter clear.**

Sources: `~/curonian/harness/results/aquabc_phyto_phenology.md`, `phyto_phenology_test.md`,
`phyto_fixer_tune.md`, `aquabc_po4_diagnosis.md`.

### 1.2 Relationship to the earlier "FIX_CYN won't-fix" decision

A 2026-07-25 project note recorded FIX_CYN as "won't-fix / decided." That decision was scoped to
*tuning constants to grow the fixer to observed biomass under the default temperature window*
(`T_opt`=26 °C, above the achievable ~21 °C → fixers competitively excluded, stay near zero). The
present work does **not** contest that; it opens a **different** lever the tuning campaign never
tried — a formulation-level bloom-termination term — together with the accompanying temperature
window changes. It is an evolution of the decision with new evidence, not a reversal.

*Open reconciliation to resolve when parameterizing (not blocking this design):* the earlier note
observed "NH4-floor competitive exclusion → fixers near zero," while the curonian controlled test
did bloom the fixers. The reconciliation is that the test **opened the temperature window** first
(`T_opt` 26→21); under the default window the fixers stay excluded. Confirm this holds in the
demonstration run.

## 2. Root-cause analysis (why a plateau, not a crash)

In `FIX_CYANOBACTERIA_BOUYANT` (the variant CL29 calls,
`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90:1139`):

- **Growth** is first-order in biomass: `R_FIX_CYN_GROWTH = KG_FIX_CYN · LIM · FIX_CYN_C`.
- **All loss terms are linear** in biomass: death `= KD_FIX_CYN · FAC · FIX_CYN_C`, plus
  respiration/excretion, each `∝ FIX_CYN_C`.
- **Self-shading already exists** as a *growth-side* feedback — light extinction includes
  chlorophyll terms (`aquabc_II_pelagic_model.f90:1032`):
  `K_E = K_B_E + 8.8e-3·CHLA + 5.4e-2·CHLA^(2/3)`.

With growth `μ·B` and total loss `d·B`, the net rate is `(μ−d)·B` — its sign is **independent of
biomass**, so the bloom either grows or clears *uniformly at all sizes*; it cannot crash *only
when large*. A growth-side negative feedback (self-shading) merely lowers the **plateau height**;
it does not create a crash. That is exactly the observed failure and exactly why linear-mortality
tuning could only trade over-bloom for collapse.

**The missing ingredient is a super-linear (density-dependent) loss.** Adding `−k·B²` makes the
net rate `(μ − d − k·B)·B`, which is **positive for small B and negative for large B** — a
carrying capacity the bloom overshoots and then collapses through. That biomass-dependent sign
flip is the entire difference between a plateau and a spike-and-crash.

### 2.1 Scientific basis

Quadratic (density-dependent) mortality is the standard "closure" device in
nutrient–phytoplankton–zooplankton models, representing the aggregate of processes not resolved
explicitly — aggregation and sinking, viral lysis, and programmed cell death. Its use to
stabilize and shape plankton dynamics is well established: quadratic mortality terms "have allowed
better representation of phytoplankton dynamics in complex models... and have been demonstrated to
prevent competitive exclusion in simple models" (Cropp & Norbury, 2009), and the contrast between
linear (density-independent) and quadratic (density-dependent) closure is a recognized control on
plankton system structure (Talmy et al., 2024). Applying a density-dependent loss to the
diazotrophic group to terminate its bloom is a conventional, defensible modelling choice, not a
novel mechanism.

## 3. Goal & scope

**Goal:** give `FIX_CYN` the loss nonlinearity it lacks, so an opened-window fixer bloom
self-limits into a seasonal spike-and-crash, and demonstrate that this shifts CL29 phenology in
the observed direction.

**In scope**
- One new gated model constant, `KD_FIX_CYN_DENS` (density-dependent mortality coefficient),
  **default 0.0** → every existing setup is byte-identical.
- The quadratic death term wired into both `FIX_CYANOBACTERIA` variants.
- A CL29 **phenology demonstration** input set (opt-in) that activates the term plus the four
  accompanying temperature-window changes, with a run showing the right direction.
- Unit test + a 3-config byte-identity gate.

**Out of scope (explicit)**
- Full joint calibration to chl-a / PO4 RMSE targets (the multi-process recalibration the
  analyses describe) — the documented follow-on.
- Applying the term to `CYN` or other phytoplankton groups.
- Final/calibrated temperature-window values (the demo's four values are a first cut to show
  direction).
- The `KDISS_DET_PART_ORG_P` P-remineralization patch (separate, already noted in #76).

## 4. Mechanism

One term folded into the FIX_CYN death rate, immediately after the existing death-rate assignment
and **before** the loss safeguards, in both variants of
`SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90`
(`FIX_CYANOBACTERIA_BOUYANT` after line 551; `FIX_CYANOBACTERIA` after line 258):

```fortran
! existing:  R_FIX_CYN_DEATH = KD_FIX_CYN * FAC_HYPOX_FIX_CYN_D * FIX_CYN_C
R_FIX_CYN_DEATH = R_FIX_CYN_DEATH + KD_FIX_CYN_DENS * FIX_CYN_C * FIX_CYN_C
```

- `KD_FIX_CYN_DENS` units: `day⁻¹ (mgC L⁻¹)⁻¹`. `FIX_CYN_C` is `mgC L⁻¹`, so the term is
  `day⁻¹ mgC L⁻¹` — the same units as `R_FIX_CYN_DEATH`.
- `FIX_CYN_C` is guaranteed ≥ 0 by the model's positivity clamp, so `FIX_CYN_C * FIX_CYN_C` is a
  well-defined non-negative loss; plain `C²` (not a normalized `C·(C/K_crowd)`) — one constant,
  YAGNI.
- Placed **before** the in-subroutine 50%-of-biomass loss safeguard (BOUYANT lines 553–565), so
  the term is clamped against driving the state negative, and before the caller-side death+grazing
  cap (`aquabc_II_pelagic_model.f90:2430–2459`).

### 4.1 Mass conservation (automatic)

`R_FIX_CYN_DEATH` already routes dead biomass to particulate detritus at the group's
stoichiometry (`aquabc_II_pelagic_model.f90`):
- `DET_PART_ORG_C  += R_FIX_CYN_DEATH`               (lines 2466, 2766)
- `DET_PART_ORG_N  += R_FIX_CYN_DEATH · FIX_CYN_N_TO_C` (lines 2467, 2816)
- `DET_PART_ORG_P  += R_FIX_CYN_DEATH · FIX_CYN_P_TO_C` (lines 2468, 2859)

Because the crowding loss is folded **into** `R_FIX_CYN_DEATH`, it inherits this routing
unchanged: the crashed biomass becomes a detritus pulse that then remineralizes — which is
physically the real autumn nutrient return, and conserves C/N/P by construction.

## 5. Architecture — files to change

### 5.1 Code (all default-0 ⟹ byte-identical when the constant is absent/zero)

1. **`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_types.f90`**
   - Add `real(kind = DBL_PREC) :: KD_FIX_CYN_DENS` to `type :: t_fix_cyn_params` (after line 96).
   - Add `p%KD_FIX_CYN_DENS = KD_FIX_CYN_DENS` to `populate_fix_cyn_params` (after line 408).

2. **`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model_constants.f90`** — constants are read
   **by name** (`para_get_value('NAME', var)`), so this is append-only; no renumbering of any
   existing constant.
   - Declare module var `KD_FIX_CYN_DENS` (near the FIX_CYN block, "Model constant no 324 …").
   - `call para_get_value('KD_FIX_CYN_DENS', KD_FIX_CYN_DENS)` in `READ_PELAGIC_MODEL_CONSTANTS`.
   - `call para_insert_value('KD_FIX_CYN_DENS', KD_FIX_CYN_DENS)` in `WRITE_PELAGIC_MODEL_CONSTANTS`.
   - Hardcoded default `KD_FIX_CYN_DENS = 0.0` in the defaults block (near line 1648).
   - Validation guard in `VALIDATE_PELAGIC_MODEL_CONSTANTS`, mirroring `BETA_FIX_CYN` (lines
     930–932): `if (KD_FIX_CYN_DENS < 0.0D0) then … = 0.0D0` with a WARNING.

3. **`SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90`**
   - Add `KD_FIX_CYN_DENS => params%KD_FIX_CYN_DENS` to the params `associate(` block in **both**
     `FIX_CYANOBACTERIA` (block at line 107, whose last alias `frac_avail_DON => params%frac_avail_DON`
     is at line 133) and `FIX_CYANOBACTERIA_BOUYANT` (block at line 391, last alias at line 417).
     Both last aliases currently have **no trailing comma** — add the new alias so exactly one field
     ends the list without a comma (e.g. append `KD_FIX_CYN_DENS` before `frac_avail_DON`, giving
     `frac_avail_DON` the comma).
   - Insert the `R_FIX_CYN_DEATH += KD_FIX_CYN_DENS·C²` term in both variants at the sites in §4.

### 5.2 Demonstration input (opt-in; does NOT touch any existing setup)

Follow the existing parallel-setup precedent (`INPUTS_CL29_2023clim/`):

4. **`INPUTS_CL29_phenology/`** — a copy of `INPUTS_CL29/` with only `WCONST_04.txt` edited: the
   four temperature-window values changed and the new constant appended. All five live in that one
   file (name-keyed; the leading integer is documentation only):

   | constant | # | current | demo first-cut | purpose |
   |---|---|---|---|---|
   | `KD_FIX_CYN_DENS` | 324 (new, appended) | — (0) | ~0.02–0.05 | the termination term |
   | `DIA_OPT_TEMP_LR` | 6 | −2.0 | +3.0 | remove the spurious cold-water winter diatom bloom |
   | `CYN_OPT_TEMP_UR` | 31 | 26.0 | 21.0 | let non-fixing cyano reach optimum at realistic temps |
   | `FIX_CYN_OPT_TEMP_LR` | 52 | 18.0 | 14.0 | open the fixer window into late summer |
   | `FIX_CYN_OPT_TEMP_UR` | 53 | 26.0 | 21.0 | fixer optimum at achievable summer temperature |

   *(The four temperature values and the `KD_FIX_CYN_DENS` magnitude are a first cut to show
   direction; they are not calibrated.)*

5. **`INPUT_CL29_phenology.txt`** — a driver copied from `INPUT_CL29.txt`, pointing at
   `INPUTS_CL29_phenology/`, output to `OUTPUTS_CL29_phenology/`, window 2012–2016 (the demo
   period used in the analyses). CL29 requires `ESTAS_HOLD_VOLUME=1`.

## 6. Data flow & byte-identity guarantee

```
WCONST_04.txt ──para_get_value('KD_FIX_CYN_DENS')──▶ module var (default 0.0 if absent)
   └▶ populate_fix_cyn_params ─▶ t_fix_cyn_params%KD_FIX_CYN_DENS
        └▶ FIX_CYANOBACTERIA[_BOUYANT] associate ─▶ R_FIX_CYN_DEATH += KD_FIX_CYN_DENS·C²
             └▶ loss safeguards ─▶ DET_PART_ORG_{C,N,P} at FIX_CYN_{N,P}_TO_C  (mass-conserving)
```

Byte-identity holds for every setup whose `WCONST_04.txt` does not set `KD_FIX_CYN_DENS`: the
name-keyed reader leaves it at the `0.0` default, the added term is `+0.0`, and no existing
constant is renumbered. The `WRITE`/`para_insert_value` round-trip adds one line to any
regenerated constants file but does not alter existing inputs.

## 7. Validation / demonstration (direction, not magnitude)

Run `INPUT_CL29_phenology.txt` vs baseline `INPUT_CL29.txt`, box 23 (central lagoon), 2012–2016,
and show three qualitative shifts against the curonian obs climatology:

1. **FIX_CYN** develops an Aug–Sep peak that **clears in winter** (spike-and-crash), replacing the
   44–73 µg L⁻¹ year-round plateau.
2. **Total chlorophyll** peak month moves from January to **September**.
3. **Summer/autumn PO4** draws down toward observations (the mechanistic PO4 link closes).

Plus a `KD_FIX_CYN_DENS` mini-sweep (0 / low / high) demonstrating the predicted regimes: plateau
/ seasonal spike-crash / over-suppressed. **Success = the directions match.** RMSE closure is the
follow-on, out of scope here.

## 8. Testing

- **Byte-identity gate (primary regression net):** old-code vs new-code across the three standing
  configs — Standard (`MODEL_SEDIMENTS=0`), CL29 (`=1`, `ESTAS_HOLD_VOLUME=1`), sediment test
  (`=2`) — outputs must be identical (default constant 0). A pure-add of a default-zero term makes
  this hold at any run length, so short gate windows are valid.
- **Fortran unit test** (`make test-fortran`): call `FIX_CYANOBACTERIA_BOUYANT` with a fixed
  `FIX_CYN_C` and (a) `KD_FIX_CYN_DENS = 0` → `R_FIX_CYN_DEATH` equals the baseline death rate
  exactly; (b) `KD_FIX_CYN_DENS = k` → `R_FIX_CYN_DEATH` increases by exactly `k·C²` relative to
  (a), evaluated on a case below the 50% loss cap so the safeguard does not rescale.
- **Demonstration run** — an artifact (plot/table), not a pass/fail gate.

## 9. Risks

- **First-cut `KD_FIX_CYN_DENS` magnitude:** too high over-suppresses (a KD=1.0-style collapse),
  too low leaves the plateau. The demo sweeps a few values; acceptable because calibration is out
  of scope.
- **Self-shading interaction:** the existing `K_E` chlorophyll feedback already damps growth into
  a plateau, so the crowding term must be strong enough to push the net rate negative at the peak.
  This is why §7 is a run, not only a unit test.
- **Loss-cap interaction:** the term must sit before both safeguards (it does) so a large peak
  loss is clamped rather than driving the state negative. The unit test deliberately stays below
  the cap to check the raw term; the demonstration exercises the capped regime.
- **OpenMP:** the term is elementwise per node with no cross-node coupling → thread-safe by
  construction (consistent with the per-chunk kinetics design).

## 10. Decisions log

- **Scope:** Capability + first-cut (default-off, byte-identical) — not the full end-to-end
  calibration, not minimal-experimental.
- **Groups:** `FIX_CYN` only (the group with the persistence pathology and the diazotrophic
  September bloom; non-fixing `CYN` is already seasonal via N-limitation).
- **Mechanism:** Approach 1 — density-dependent quadratic mortality closure. (Approach 2,
  temperature/nutrient senescence trigger, is the follow-on knob if the crash is mistimed;
  Approach 3, strengthened self-shading, was rejected — a growth-side feedback plateaus, it does
  not crash.)
- **Both `FIX_CYN` variants** get the term (the plain variant is currently unused but kept in
  lockstep to avoid silent divergence).
- **Demonstration** as a new `INPUTS_CL29_phenology/` directory (matches `INPUTS_CL29_2023clim/`).
- **Form** plain `C²` (one constant) over normalized `C·(C/K_crowd)` (two).

## 11. References

- Cropp, R., & Norbury, J. (2009). Parameterizing plankton functional type models: insights from a
  dynamical systems perspective. *Journal of Plankton Research*, 31(9), 939–963.
  <https://doi.org/10.1093/plankt/fbp042>
- Talmy, D., Carr, E., & Rajakaruna, H. (2024). Killing the predator: impacts of highest-predator
  mortality on the global-ocean ecosystem structure. *Biogeosciences*, 21(10), 2493–2507.
  <https://doi.org/10.5194/bg-21-2493-2024>
- Issue #76 and the curonian analyses: `~/curonian/harness/results/aquabc_phyto_phenology.md`,
  `phyto_phenology_test.md`, `phyto_fixer_tune.md`, `aquabc_po4_diagnosis.md`.

---

## 12. In-loop review outcome (2026-08-01) — necessity gate

A four-reviewer adversarial in-loop review (Fortran/byte-identity, scientific soundness,
scope/testing, adversarial premise) found the design **not implementable as written** and, more
importantly, put the **necessity of the quadratic term itself in question**. The clean capability
(a gated, default-0 closure) remains low-risk, but the demonstration and the premise it rests on
are defective. Findings below are verified against the source, not just asserted.

### 12.1 Blocking findings

- **BL-1 — Demonstration temperature windows are CTMI-invalid (scientific).** CL29 runs
  `TEMPERATURE_MODEL = 1` (CTMI) — `INPUTS_CL29/PELAGIC_MODEL_OPTIONS.txt:15`. In CTMI mode the
  constants are cardinal temps with **`T_max = KAPPA_*_OVER_OPT_TEMP`**
  (`aquabc_II_pelagic_auxillary.f90:80-85`), and CTMI is used only when
  `2·T_opt > T_min + T_max`; otherwise it **falls back to the plateau model** where
  `KAPPA_*_UNDER_OPT_TEMP = 0` ⟹ `exp(0)=1` ⟹ *full growth at all sub-optimal temperatures,
  including freezing winter*. The baseline windows are valid; **all §5.2 demo changes flip them
  invalid** (DIA 3/10/21 → 20<24; CYN 15/21/34 → 42<49; FIX 14/21/32 → 42<46), forcing the
  plateau-fallback branch that *manufactures* the year-round plateau the demo exists to remove.
  **Fix:** co-lower each `T_max` (= `KAPPA_*_OVER_OPT_TEMP`) to keep `T_max < 2·T_opt − T_min`:

  | group | demo T_min | demo T_opt | required `KAPPA_OVER` (T_max) | current |
  |---|---|---|---|---|
  | DIA | 3 | 10 | **< 17** (e.g. 16) | 21 |
  | FIX_CYN | 14 | 21 | **< 28** (e.g. 27) | 32 |
  | CYN | 15 | 21 | **< 27** (e.g. 26) — *or drop the CYN change, see BL-3/M7* | 34 |

  And assert the run log does **not** contain `GROWTH_AT_TEMP: CTMI params invalid … falling back
  to plateau`.

- **BL-2 — The term's necessity is unproven; the motivating "structural persistence" is likely a
  CTMI-invalidity artifact (scientific/adversarial).** The curonian runs that established the
  self-sustaining plateau (`phyto_phenology_test.md` PH_FIX/PH_ALL, `phyto_fixer_tune.md` Round 2)
  used `FIX_CYN_OPT_TEMP_UR = 21` — CTMI-invalid → the same plateau-fallback branch with the
  cold-side growth switch disabled. Under any **CTMI-valid** window, winter `T < T_min ⟹
  LIM_TEMP = 0 ⟹ zero winter growth by construction`, so the *existing* linear death (0.04–0.10
  d⁻¹) already clears the standing stock. **The plausible real fix is corrected, CTMI-valid
  temperature windows — with the quadratic term unnecessary.** This must be tested before the term
  is built (see §12.3).

- **BL-3 — Constant plumbing targets the wrong routines; as written it crashes every setup
  (Fortran + scope, independently).** `para_get_value` hard-`STOP`s on an unregistered name
  (`STRING_UTILS.f90:71`); it does **not** default-fill. The real sites are
  `INIT_PELAGIC_MODEL_CONSTANTS` (the `para_get_value` read-back, ~:681) and
  `INSERT_PELAGIC_MODEL_CONSTANTS` (the `para_insert_value` registrar, ~:1315) — **not**
  `READ_`/`WRITE_` as §5.1(2)/§6 say. **Fix:** mirror the last-added constant `BETA_FIX_CYN` (#321)
  exactly across `DEFAULT`(:1648) / `INSERT`(:1315) / `INIT`(:681) / `VALIDATE`(:930), and rewrite
  §6's mechanism to: "DEFAULT sets 0.0 → INSERT registers it in the para table → INIT's
  name-keyed read returns 0.0 → term = +0.0 → byte-identical." (The byte-identity *outcome* is
  still achievable; the recipe was wrong.)

- **BL-4 — §2's dynamical justification is wrong; `−kB²` alone gives a lower stable plateau, not a
  crash (scientific + adversarial).** For a continuous scalar system `dB/dt=(μ−d−kB)B` the approach
  to `B* = (μ−d)/k` is **monotone — no overshoot, no crash**; and `μ` is already `μ(B)` via
  self-shading, so `B*` is not even constant. The seasonal crash is delivered by autumn/winter
  `μ(t)` collapse (⟹ BL-1 must be fixed first). **Fix:** rewrite §2 to claim the term **caps the
  summer peak height / breaks the self-sustaining plateau** (a magnitude effect); the winter clear
  is temperature-forced, not term-driven.

### 12.2 Important findings (apply only if the gate in §12.3 passes)

- **IM-1 — Existing Fortran unit tests read uninitialized memory.** `tests/fortran/test_fix_cyn.f90`
  seeds params via `set_default_fix_cyn_params` (`tests/fortran/test_defaults.f90:159-187`), which
  will not set the new field → `+KD·C²` multiplies garbage in all 9 cases. **Fix:** set
  `KD_FIX_CYN_DENS = 0.0D0` there **and** default-initialize the type component itself
  (`:: KD_FIX_CYN_DENS = 0.0_DBL_PREC` in `t_fix_cyn_params`) for robustness.
- **IM-2 — The 3-config byte-identity gate is vacuous for the feature** (KD=0 ⟹ `+0.0` regardless
  of wiring/sign). Add a **byte-difference** test: CL29 `KD=0` vs `KD=k` must differ and FIX_CYN
  biomass must drop. Reword §8: the gate proves "no existing setup changes," not feature validity.
- **IM-3 — Detritus routing is mis-partitioned and may worsen the §7 autumn-PO4 target.** Routing
  100% of the crowding loss to `DET_PART_ORG_{C,N,P}` (fast-remineralizing; `KDISS_P` already 14×
  the C/N rate) can *increase* the autumn P overshoot the demo claims to fix. Lysis is dissolved
  (DOC/DON/DOP); aggregation/sinking is export. Add a caveat + an autumn-PO4 sign check; consider
  splitting the loss. (§4.1's "mass-conservation is automatic" holds for *total* mass, verified;
  the *partitioning* is a modelling choice, not automatic.)
- **IM-4 — Success criteria are cherry-picked.** Activating FIX_CYN is already known to regress the
  multivariate fit (EPA CHLA +52%, Si +64%, TN +56%, NH4 +16% worse; only PO4 −35% better —
  `fix-cyn-n2fixation-overprediction.md`). §7 reports only the improving subset. State the Si/TN/
  CHLA regression as an expected cost in §9 and report those variables in the demo.
- **IM-5 — Framing over-claims diazotrophy.** The project's verdict is that FIX_CYN-*as-a-fixer* is
  not reproducible in CL29; the opened-window biomass wins the niche as a *non-fixing* cyano. Drop
  the "*Aphanizomenon/Nodularia* diazotrophic bloom" and "real autumn nutrient return" language
  until §1.2's reconciliation is closed with fixation diagnostics.

### 12.3 The necessity gate (do this before writing any implementation plan)

Adjudicated reviewer conflict, for the record: the in-subroutine 50%-loss safeguard is **inert** at
realistic biomass (cap `= 0.5·C/TIME_STEP = 120·C day⁻¹` at `dt=1/240`; peak `kC² ≈ 7 day⁻¹` for
`k=0.2, C=6` — three orders below), so the crash is **not** a timestep artifact. §9's "exercises
the capped regime" text is wrong and must be removed.

**One decisive experiment (it is the corrected primary demo, not a new phase):** run CL29 with
**CTMI-valid** windows (BL-1 table; DIA corrected, FIX opened; leave CYN unchanged per M7) and
`KD_FIX_CYN_DENS ∈ {0, ~0.05, ~0.2}` with the **temperature values held identical across all three
arms**.

- If **k = 0 already** yields the September-peak / winter-clear phenology → **the term is
  unnecessary**; the deliverable pivots to shipping the corrected temperature windows (a much
  smaller change), and the quadratic-term code is dropped.
- If **k = 0 plateaus and k > 0 crashes it** → the term is justified; proceed with the term and
  apply the BL-3 wiring fix and the §12.2 fixes.

Report the outcome honestly either way (a k=0-already-seasonal result is a valid, publishable
finding, not a failure). Until this runs, **do not hand writing-plans a term-based plan** — it would
be built on an assumption the code contradicts.
