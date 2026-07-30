# Resuspension × Sediment-Diagenesis Coupling — Design / Scope

**Date:** 2026-07-30
**Status:** scope / design (for review — not yet a plan)

## Goal

Make bed **resuspension** mass-conserving against the **full diagenesis bed** (`MODEL_SEDIMENTS==2`)
so the two can run together, and lift the guard at `mod_AQUATIC_MODEL.f90:554` that currently halts the
program when both are enabled. See the findings note `docs/resuspension_diagenesis_coupling.md`.

## Current state (verified against the tree)

- **Resuspension has no link to any bed inventory.**
  - *Option 1* (`mod_SOLVER.f90:1475-1508`): injects a prescribed `velocity × concentration` mass into
    **all 32** water-column derivatives with **no matching sink** — "source from nowhere". It also has a
    **dimensional quirk**: the term is g/day (`FRAC·SURFACE_AREA·VEL·CONC`) added into the *per-volume*
    `DERIVATIVES` array that is later `×VOLUME` (`:1687-1689`) without the `÷depth` the sediment-flux path
    gets (`:1659-1668`) → an extra m³ factor vs every other term.
  - *Option 2* (`mod_SOLVER.f90:1243-1257`): only a shear-stress **gate that suppresses settling**; adds
    no mass.
- **The diagenesis bed** (`sediment_state_t`/`bsed`, `mod_BOTTOM_SEDIMENTS.f90:22-49`): a mass-conserving
  compartment, state `(box, layer, var)`, `NUM_SED_VARS=24`, units **g/m³ bulk sediment volume**. Surface
  = layer 1; deposition enters layer 1 via `FLX_ALUKAS_II_TO_SED_MOD_1_VEC` (g/m²/day). Its own erosion is
  **dormant and incomplete**: `H_ERODEP=0` hard-set (`mod_SOLVER.f90:1567`), `isedi=0` disables the erosion
  branch (`mod_BOTTOM_SEDIMENTS.f90:239`), and even that branch zeroes particulate erosion
  (`SOLUTE_FRACTIONS=0`, "solid phase still goes nowhere. fixme", `aquabc_II_sediment_model_1_fast.f90:2027`).
  The water-return map zeroes particulate detritus (`FLUXES_TO_ALUKAS(:,5:11)=0`,
  `aquabc_II_sediment_auxillary.f90:396`, "does not take into account particulate material resuspension").
- **The guard**: `mod_AQUATIC_MODEL.f90:554-560` halts if `MODEL_BOTTOM_SEDIMENTS > 1 .and.
  resusp%CONSIDER_RESUSPENSION > 0` (set for both resuspension options).

## Design

When diagenesis is on, resuspension **erodes a thickness `ΔH` of surface bed layer 1**, transferring the
eroded particulate C/N/P/Si to the water-column **detrital** pools and decrementing the bed inventory by
the same mass. All new behaviour is gated on `MODEL_BOTTOM_SEDIMENTS > 1 .and. CONSIDER_RESUSPENSION > 0`
(the currently-guarded, never-run combination) so nothing else changes.

### Variable mapping (the four clean particulate pairs)

| Bed var (layer 1) | slot | → Water-column var | index |
|---|---|---|---|
| `SED_POC` | 10 | `DET_PART_ORG_C` | 9 |
| `SED_PON` | 4 | `DET_PART_ORG_N` | 10 |
| `SED_POP` | 7 | `DET_PART_ORG_P` | 11 |
| `SED_PSi` | 12 | `PART_Si` | 18 |

All four eroded by a **single shared thickness `ΔH`** so bed C:N:P:Si stoichiometry is preserved
(per-var erosion would drift the ratios).

### Conservation invariant (the acceptance test)

For each eroded var `v`, bed surface conc `C_bed,v` (g/m³ bulk), erosion thickness `ΔH` (m), box depth
`h` (m), timestep `dt` (day), box volume `V` (m³):

- eroded areal mass `= C_bed,v · ΔH`  [g/m²]
- **bed decrement:** `bsed%FINAL_SED_STATE_VARS(i,1,v) -= (C_bed,v · ΔH) / SED_DEPTHS(i,1)`  [g/m³ bulk]
- **water increment:** add `(C_bed,v · ΔH) / dt / h` to `wsc%FLUXES_TO_WATER_COLUMN(i, water_v)`
  [g/m³/day], which the existing `×V` at `:1687-1689` turns into g/day.
- **invariant:** Σ(bed areal loss) = Σ(water areal gain), per var, every step.

Eroding *by thickness against bulk concentration* means porosity need not appear explicitly — the areal
bridge `g/m³ bulk × m = g/m²` matches the model's existing `UNIT_AREA_MASSES` convention.

### Erosion driver — the one real design fork

There is **no shear-driven erosion rate in the code** (Option 1 is prescribed vel×conc; Option 2 is only a
gate). So the fix must supply the rate. Two options:

- **A (recommended, minimal): reuse the prescribed `RESUSPENSION_VELOCITY`** as the erosion velocity, but
  draw the concentration from the **bed** surface layer instead of a prescribed series →
  `erosion_flux_v = FRAC · vel · C_bed,v`, matched by the bed decrement above. Lifts the guard and achieves
  conservation with minimal new machinery; the prescribed-concentration series is simply unused in
  diagenesis mode.
- **B (physical, more work): shear-stress erosion** `E = E₀(τ_b/τ_c − 1)` (the reference-manual formula)
  using Option-2's shear time series + a new `E₀` erosion-coefficient input → an erosion thickness `ΔH`.
  More realistic; needs new config + machinery.

**Recommendation:** Phase 1 = **A** (mass-conserving, lifts the guard); Phase 2 (optional) = **B**
(shear-driven realism) once the conservative plumbing exists.

### Insertion point

The ESTAS bed block, `mod_SOLVER.f90:1510-1669` — a localized change that avoids the SHYFEM-shared AQUABC
sediment lib:
1. Replace `H_ERODEP=0` (`:1567`) with the computed erosion (`ΔH` or per-var flux) for the coupled path.
2. After the sediment solve, **decrement** `bsed%FINAL_SED_STATE_VARS(:,1,{4,7,10,12})` and **add** the
   erosion return to `wsc%FLUXES_TO_WATER_COLUMN(:,{9,10,11,18})` — **before** the bed-state copy-back at
   `mod_SIMULATE.f90:342` (else the decrement is overwritten).
3. **Do not** revive the dormant `isedi>0` machinery in the shared lib (hard-set to 0, particulate erosion
   stubbed) — write the coupling in the ESTAS bed block instead.

## Scope boundaries

**In:** particulate C/N/P/Si detrital transfer, bed-mass-conserving; Option-A (prescribed-velocity)
erosion driver; guard removal (gated to the coupled path only).

**Out (documented non-goals):**
- **Disaggregation to plankton pools.** Deposition folds living algal/zoo C into bed *detritus*
  (`aquabc_II_pelagic_auxillary.f90:1247-1329`); the bed has no phyto/zoo pools, so erosion can only
  reconstitute water-column **detritus** (9/10/11/18). Defensible, but not a true inverse of deposition.
- **Adsorbed NH4/PO4 particulate fraction** resuspension (bed vars 1/5, `IN_WHICH_PHASE=2`).
- **Shear-driven erosion rate** (Phase 2).
- **Reviving the `isedi` erosion machinery.**

## Byte-identity / regression constraints

- Modes 0/1 (no diagenesis) and **diagenesis-without-resuspension** MUST stay **byte-identical** — the new
  code activates only under `MODEL_SEDIMENTS>1 .and. resuspension` (no existing runs hit that path).
- Verify with golden runs: Standard (25-box) and CL29 (Mode 0/1) byte-identical to pre-change; a
  diagenesis-only CL29 run (resuspension off) byte-identical.

## Risks & pre-existing issues to handle

1. **Option-1 dimensional quirk** (`mod_SOLVER.f90:1500-1502`, extra m³ factor + no `÷depth`). If Option A
   reuses that plumbing, the new coupled term must be dimensionally correct (`÷depth` into per-volume).
   **Decision:** fix only in the new coupled path (preserves Option-1-alone byte-identity), or fix Option 1
   globally (corrects a real latent bug but changes existing Option-1 behaviour → not byte-identical).
2. **Negative-inventory clamp.** Cap erosion so `ΔH · C_bed,v` never exceeds the layer-1 inventory; clamp
   `ΔH` to available bed mass to avoid negative bed concentrations.
3. **Bed-advance ordering.** Decrement `FINAL_SED_STATE_VARS` before the copy-back at `mod_SIMULATE.f90:342`.
4. **`SURF_WATER_CONCS` slot mismatch** (`mod_SOLVER.f90:1542-1565`) is latent/harmless (zero diffusivity)
   but lives in the same block — don't disturb it.
5. **Solver stages.** The bed block runs inside `CALC_DERIV`, called for both Euler and each RK2/Heun
   stage — ensure erosion is computed consistently per stage (and interacts correctly with `ESTAS_HOLD_VOLUME`).

## Task breakdown & effort (Phase 1 = Option A)

1. Erosion-rate helper: `erosion_flux_v = FRAC · RESUSPENSION_VELOCITY · C_bed,v` per box, with the
   negative-inventory clamp. (~1 d)
2. Bed decrement + water return wired into `:1510-1669` for the four particulate pairs, mass-conserving. (~1–2 d)
3. Resolve the Option-1 dimensional quirk (coupled-path-only vs global). (~0.5 d)
4. Lift the guard (`:554`), gated to allow only the coupled path. (~0.5 d)
5. **Mass-conservation test** (Σ bed loss = Σ water gain, per var, per step) + byte-identity goldens for the
   untouched modes. (~1–2 d)
6. Docs: reference manual + update the findings note. (~0.5 d)

**Estimate: ~5–7 days for Phase 1.** Phase 2 (shear-driven erosion) adds ~2–3 days.

## Open decisions for review

1. **Erosion driver:** A (prescribed velocity, minimal — recommended) vs B (shear-driven, physical).
2. **Option-1 dimensional-quirk fix:** coupled-path-only (byte-identity-safe) vs global (fixes a real bug,
   changes Option-1-alone behaviour).
3. **Depth of ambition:** Phase 1 only (lift the guard, conservative) vs Phase 1 + 2.
4. **Is this worth doing now?** No current setup needs both (CL29 diagenesis runs with resuspension off).
   The value is unblocking a physically-complete shallow-lagoon configuration; weigh against the ~1-week cost.
