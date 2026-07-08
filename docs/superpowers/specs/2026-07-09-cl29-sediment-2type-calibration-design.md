# CL29 Sediment Phase 2 — Two-Type (Sandy/Muddy) Infrastructure + Calibration

**Date:** 2026-07-09
**Status:** Design (under review), pending implementation plan
**Goal:** Calibrate the CL29 sediment diagenesis so its benthic N/P/Si fluxes match measured
Curonian sandy/muddy data **and** the box-19 spring diatom bloom reaches the observed
~47 mg/m³ — replacing the Phase-1 stability values with data-anchored ones, and giving
sandy vs muddy sediments distinct behaviour.

> **Builds on Phase 1** (`2026-07-08-cl29-sediment-diagenesis-phase1-design.md`, merged):
> `MODEL_SEDIMENTS=2` stands up and runs stably (serial), off by default, with a single
> uniform sediment profile of *stability* values.

## 1. Objective

**Both must hold** (user decision): the modeled benthic NH4/NO3/PO4/DSi fluxes match the
measured sandy/muddy seasonal values **and** the box-19 spring diatom Chl-a reaches
47 ± 14 mg/m³ under the validation-doc guardrails. If matching the fluxes and closing the
diatom gap genuinely conflict, **stop and surface it as a finding** rather than force either.

## 2. Decomposition

Phase 2 splits by data dependency:

- **Phase 2a — two-type sediment infrastructure (buildable NOW, data-independent).** The
  Fortran reader extension for sandy/muddy profiles, the per-box flux-output fix, and
  converter support for two profiles + a box→type map. Fully testable with placeholder
  profiles (seeded from the Phase-1 stability values).
- **Phase 2b — calibration + validation (when the measured data arrives).** Derive the
  sandy/muddy geometry/IC + global oxic/anoxic rate constants analytically from the measured
  fluxes, verify with a few full runs, and check *both-must-hold*.

**This spec's implementation plan covers 2a.** 2b is scoped here but gets its own
spec→plan cycle once the data lands.

## 3. Phase 2a architecture

Three coordinated pieces. **Refinement from brainstorming:** the sandy vs muddy *rate*
difference need NOT be encoded as per-box rate constants. Dissolution/mineralization select
oxic vs anoxic rates by a per-cell `where (SED_DOXY ≥/< DOXY_AT_ANOXIA)` mask
(`aquabc_II_sediment_model_1_fast.f90:2455-2478`), and sediment O2 is driven per-box by
deposition + porosity. So sandy (oxic, low-deposition) cells automatically use `K_OXIC_*`
and muddy (anoxic, depositional) cells use `K_ANOXIC_*`, from the **global** `W_SED_CONST`.
The two-type profiles therefore vary **geometry (depths, porosities, densities, burial,
mixing) + the 24×layer IC block** — which the model already indexes per box — leaving the
kinetics and the single `W_SED_CONST` untouched.

### 3.1 Fortran reader extension (`mod_BOTTOM_SEDIMENTS.f90`, `READ_BOTTOM_SEDIMENTS_MODEL_INPUTS`)
- Add an optional `# NUM_SED_TYPES` (default 1) and, when > 1, a `# SED_TYPE_PER_BOX`
  block of `nkn` integer indices (1 = sandy, 2 = muddy) plus one geometry+IC profile block
  per type.
- Assign per box: `SED_DEPTHS(box,:)`, `SED_POROSITIES(box,:)`, `SED_DENSITIES(box,:)`,
  `SED_BURRIALS(box,:)`, `PART_MIXING_COEFFS(box,:,:)`, and `INIT_SED_STATE_VARS(box,:,:)`
  take the values of that box's type (these arrays are already `(nkn, …)`-dimensioned and
  used per box, so no kinetics/transport change is required).
- **Backward compatible:** a file with no `# NUM_SED_TYPES` (or `=1`) loads exactly as today
  — one profile broadcast to all boxes — so the 25-box example and `INPUT_sediment_test`
  are byte-for-byte unaffected. Guard the new reads behind the type count.

### 3.2 Per-box flux-output fix (`mod_SIMULATE.f90:719`)
`write(... FLUXES_OUTPUT_TO_WATER_COLUMN(nkn,:))` → `(i,:)`. The current code writes box
`nkn`'s fluxes for every box `i`, so per-box benthic fluxes cannot be read. Required for the
2b modeled-vs-measured comparison. (Benthic flux indices: PO4 = `FROM_SEDIMENT(5)`,
NH4 = `(1)`, NO3 = `(2)`, DSi = `(11)`, per `aquabc_II_sediment_auxillary.f90`.)

### 3.3 Converter support (`tools/eutropy_poc/eutropy_to_estas.py`)
- `CL29_SEDIMENT_TYPE = {box: 'sandy'|'muddy'}` (user supplies the 29-box map; a box absent
  ⇒ default type).
- Two profile dicts `CL29_SED_SANDY` / `CL29_SED_MUDDY`, each `{depths, porosities, densities,
  burial, mixing, ic_overrides}`. Seeded with the Phase-1 stability values as placeholders
  until 2b; the existing `CL29_SED_DEPTHS`/`CL29_SED_BURIAL`/`CL29_SED_CONST_OVERRIDE` become
  the shared/global baseline (`W_SED_CONST` stays single).
- `_write_sediment_inputs` authors the extended `BOTTOM_SEDIMENT_MODEL_INPUT.txt` (`NUM_SED_
  TYPES`, `SED_TYPE_PER_BOX`, two profile blocks) matching §3.1. When `CL29_SEDIMENT_TYPE` is
  empty it emits the single-profile format (Phase-1 behaviour), preserving the opt-in
  off-by-default byte-identical baseline.

**Data flow:** converter (2 profiles + box→type map) → extended reader assigns per-box
sediment → run (serial) → per-box flux output → (2b) compare to measured → refine.

## 4. Phase 2b calibration + validation (scoped; own cycle when data lands)

**Method — analytical-first + few runs (user decision):**
1. **Analytical derivation.** At quasi-steady state the benthic dissolved flux is set by the
   deposited particulate (mass conservation) modulated by dissolution/mineralization rates,
   burial, and the solid-partition coefficients. Deposition per box is known from a Phase-1
   run and differs sandy vs muddy. For each type × solute (N, P, Si), derive the geometry
   (porosity → O2 penetration → oxic/anoxic split) and the global `K_OXIC`/`K_ANOXIC` rates
   that reproduce the measured seasonal flux given that deposition — yielding initial sandy
   and muddy profiles without a run.
2. **Verify + refine (~2–4 full runs).** Run, extract per-box benthic N/P/Si fluxes (fixed
   output), compare to measured by type and season, adjust, re-run until matched.

**Validation (both-must-hold):**
- *Fidelity:* modeled benthic NH4/NO3/PO4/DSi fluxes match measured sandy/muddy seasonal
  values within a stated tolerance (set from measurement uncertainty in 2b).
- *Gap-closure:* box-19 spring diatom Chl-a → 47 ± 14; summer cyano within 96 ± 56;
  spring:summer ratio ≈ 0.5; diatom→OPA→cyano succession (order + timing) intact.
- *Guardrails (from `docs/CL29_Parameter_Validation.md`):* water-column DIP realism; summer
  DIN:DIP (N-fixer artifact); DISS_Si floor ≫ KHS_DSi; no interannual PO4 / SED_PSi drift;
  sediment pools not clamp-pinned; all 5 years reported (not just the mean); no NaN /
  CO2SYS non-convergence.
- *Conflict → finding:* if fidelity and gap-closure cannot both be met, stop and surface it.

## 5. Testing

**Phase 2a (now, data-independent):**
- **Fortran** (`tests/fortran`): the extended reader assigns per-box types correctly (a
  2-type file → boxes get their type's geometry/IC) **and** backward-compat — a
  single-profile (no `NUM_SED_TYPES`) file loads identically to today (all boxes → profile 1);
  `INPUT_sediment_test` still runs.
- **Flux-output fix:** a short 2-type run → assert `SEDIMENT_FLUX_OUTPUTS.out` rows are
  per-box distinct (not all identical to box 29).
- **Converter** (`tests/python`): a box→type map + two profiles produce an extended
  `BOTTOM_SEDIMENT_MODEL_INPUT.txt` with `NUM_SED_TYPES=2`, a correct `SED_TYPE_PER_BOX`
  block, and both profile blocks; an empty `CL29_SEDIMENT_TYPE` reproduces the Phase-1
  single-profile output (byte-identical); off-by-default baseline unchanged.
- **Clean-checkout:** fresh clone builds serial and runs a 2-type short simulation.

**Phase 2b (when data lands):** the modeled-vs-measured flux-comparison harness (per box /
type / season) and the both-must-hold validation, reported across all 5 years.

## 6. Out of scope / future

- Per-box *rate constants* (a kinetics change) — not needed; the oxic/anoxic split + per-box
  geometry cover sandy/muddy. Only revisit if 2b calibration proves it can't match both regimes.
- More than two sediment types (the reader design generalizes to `NUM_SED_TYPES > 2`, but
  only 2 are authored).
- OpenMP for the sediment path (deadlocks — serial only, per Phase 1).

## 7. References

- `docs/superpowers/specs/2026-07-08-cl29-sediment-diagenesis-phase1-design.md` — Phase 1.
- `docs/CL29_Parameter_Validation.md` — P-supply root cause + the guardrails reused here.
- Source: `mod_BOTTOM_SEDIMENTS.f90:317-486` (sediment reader, single-profile broadcast to
  extend), `aquabc_II_sediment_model_1_fast.f90:2455-2478` (per-cell oxic/anoxic rate mask),
  `mod_SIMULATE.f90:716-724` (per-box flux output bug), `aquabc_II_sediment_auxillary.f90`
  (benthic flux → pelagic-var mapping), `tools/eutropy_poc/eutropy_to_estas.py`
  (`_write_sediment_inputs`, `CL29_SED_*`).
