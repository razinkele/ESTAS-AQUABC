# Design: Nostocales multi-year persistence — akinete formation-timing recalibration

> ## ⚠️ SUPERSEDED — DE-RISKED, INSUFFICIENT (2026-07-12)
> This approach was **empirically de-risked and does not work.** Two 5-yr calibration sweeps:
> - **Formation timing** (`T_FORM_AKI` 16→24): only partial — NOST yr3–5 rose from ~0 to ~0.2–0.7
>   (vs the yr1–2 ~1.9 target), and never recurs at bloom level.
> - **NOST growth** (`KG_NOST` 1.29→5): needs *physically indefensible* growth (bloom cyanobacteria
>   grow slower than diatoms, not faster), and even then only **redistributes a P-capped cyano
>   pool** (CYN↔NOST, total summer cyano unchanged).
>
> **Real root cause:** the multi-year Nostocales collapse is **competitive exclusion under
> system-wide P-limitation**, not an akinete-supply / timing / growth-kinetics defect. There is no
> defensible converter-only fix, and restoring NOST changes *none* of the validated metrics (NOST
> is not needed for the Chl-a match, which is already within 1 SD of Bartoli 96±56). **Do not
> implement this.** Kept as a record of the corrected root cause and the dead-end levers.

- **Date:** 2026-07-12 (rewritten after in-loop review overturned the first draft)
- **Status:** **SUPERSEDED / negative result** (see banner above) — not for implementation
- **Author:** Arturas Razinkovas-Baziukas (with Claude)
- **Scope:** CL29 converter parameter recalibration (`CL29_WCONST_OVERRIDE`). **Converter-only,
  no Fortran, no rebuild** — with a Fortran fallback documented if calibration fails (§6).

## 0. What changed from the first draft (important)

The first draft proposed a Fortran "transport exemption" to make akinetes a non-advecting local
pool. **An in-loop review found that is already implemented:** the model sets and applies
`ADVECTION_ON(32)=0`, `DIFFUSION_ON(32)=0`, `SETTLING_ON(32)=0` (init at
`mod_INITIALIZE_PELAGIC_BOX_MODEL.f90:161`; consumed at `mod_SOLVER.f90:993, 1136`). Akinetes
already do not advect, diffuse, or settle. That draft's premise was false; this rewrite is built
on the verified mechanism below.

## 1. Corrected root cause (verified)

Nostocales (`NOST_VEG_HET_C`, var 31) blooms yr1–2 then goes **extinct from yr3**, because its
akinete seed pool (`AKI_C`, var 32) drains one-way and is never replenished:

- Akinetes are **already a closed local pool** — no transport (§0), and `K_LOSS_AKI = 0`,
  `K_MORT_AKI_20 = 0`, so no chemical decay. Their **only** fluxes are germination (out) and
  formation (in).
- **Germination** (`aquabc_II_pelagic_lib_NOSTACALES.f90:339`): `where (DIN < KN_GERM_AKI .and.
  TEMP > T_GERM_AKI=21)` → `R_GERM = KR_GERM_AKI(0.3) × AKI`. Fires in the warm, N-depleted
  bloom season and **consumes ~5 units of AKI per summer** (box-19 yr1: AKI 8.0 → 3.0).
- **Formation** (`:356`): `where (TEMP < T_FORM_AKI=16 .and. day 200–365)` → `R_FORM =
  KR_FORM_AKI(0.1) × NOST_veg`. Requires **T < 16 °C**, i.e. autumn.
- **The timing mismatch is the defect.** The NOST bloom occurs at **~21 °C (box-19 doy 180–240,
  range 16.7–26.5)** and **crashes by doy ~240**; the formation window (T<16) only opens at
  doy ~240+, when NOST biomass is already **~0.001**. So `R_FORM = 0.1 × ~0 ≈ 0` — the pool
  cannot replenish **regardless of the formation rate**. This is why the earlier
  `P_FORM_AKI`→0.3 experiment failed (0.3 × 0 ≈ 0).

Compounding it: NOST has **net-negative vegetative growth** during its own bloom (yr1: ~5 units
of akinetes germinated but NOST only reached 1.88 mgC/L — losses > growth), so the bloom is small
and short-lived, leaving little biomass to form akinetes from even if timing were fixed.

**Conclusion:** the fix is a **kinetic recalibration** aligning akinete formation with the NOST
bloom (and, if needed, lengthening the bloom) — *not* a transport mechanism.

## 2. Goal / non-goals

- **Goal:** species/functional realism — the model should sustain **recurring annual Nostocales
  blooms** via a self-replenishing akinete cycle.
- **Non-goal:** closing the summer Chl-a gap. NOST is not needed for that — box-19 bloom-month
  total Chl-a (60 mg/m³) is already within 1 SD of Bartoli 96±56 without NOST. The fix must
  **not degrade** the validated succession or the Chl-a match.
- **Explicit uncertainty:** two prior akinete hypotheses failed (boundary refuge; formation
  *rate*). This design leads with an **empirical calibration step**; the levers below are
  hypotheses to be confirmed against a 5-yr run, not asserted fixes.

## 3. Approach — align formation with the bloom (converter-only, empirical)

Two coupled levers, both set via `CL29_WCONST_OVERRIDE` (name-matched into the copied
`WCONST_04.txt`; CL29-scoped, reversible, no shared-file or Fortran edit). Calibrate empirically:

### 3.1 Lever A — formation timing (primary)
Make akinete formation coincide with the (warm) bloom instead of calendar-autumn:
- **`T_FORM_AKI`** (16 → **~21**, at/just below the germination threshold `T_GERM_AKI=21`):
  formation then fires at `T < 21` — the bloom's **cooling tail** (doy ~225–260, T falling
  21→14) while NOST biomass still exists — and germination stays at `T > 21`, so the two windows
  are **cleanly separated** (no overlap). This directly fixes the timing mismatch (was T<16,
  reached only after the bloom had crashed) while avoiding the futile cycle.
- **`DAY_FORM_AKI`** (200): keep or lower slightly so the day-gate doesn't exclude the bloom
  (bloom is doy ~180–240; day>200 already overlaps its second half).

**Known risk — futile cycle:** if `T_FORM_AKI` is pushed **above** 21 to catch more of the warm
bloom peak, formation and germination overlap in the 21–`T_FORM_AKI` band, creating a
simultaneous AKI→NOST→AKI cycle that could damp the bloom or oscillate. Keep `T_FORM_AKI ≤ ~21`;
if the cooling-tail biomass proves too small (likely, given the fast crash), escalate to Lever B
(§3.2) rather than widening the temperature window.

### 3.2 Lever B — NOST bloom persistence (secondary, if A insufficient)
If formation-timing alone leaves too little biomass (net-negative growth crashes the bloom before
formation captures it), lengthen/strengthen the bloom via NOST kinetic constants (growth rate,
mortality, grazing susceptibility — exact WCONST names to be identified during implementation).
Higher risk: changes bloom magnitude/shape, so it is gated harder on the succession/Chl-a checks.

### 3.3 Small bounding loss (if needed)
With formation and germination balanced, add a small `K_LOSS_AKI`/`K_MORT_AKI_20` (>0) only if the
now-replenished local pool grows unbounded — a ceiling, determined during calibration.

## 4. Data flow (target)

```
summer bloom (T>21, DIN low):  AKI --KR_GERM(0.3)--> NOST_veg  (inoculation)
bloom + senescence (T<T_FORM_AKI, day>DAY_FORM): NOST_veg --KR_FORM--> AKI  (replenish, NOW overlaps biomass)
overwinter: AKI persists locally (no transport, ~no loss)
next summer: repeat -> bounded self-sustaining cycle
```

## 5. Validation criteria (promotion gate — empirical)

1. **NOST recurs yr1–5**: box-19 and other freshwater boxes bloom every summer (peak ~1–2
   mgC/L); no permanent yr3 extinction.
2. **Bounded akinete cycle**: per-box `AKI_C` reaches a stable multi-year oscillation (not → 0,
   not unbounded); no within-bloom futile-cycle oscillation (§3.1).
3. **Succession intact**: spring diatom peak 1.1–1.9 mgC/L; summer CYN unchanged; diatom→cyano
   hand-off preserved.
4. **Chl-a still within obs**: box-19 bloom-month total Chl-a within Bartoli 96±56 (1 SD 40–152);
   no summer overshoot.
5. **0 NaN**; no material regression in other state vars.

## 6. Fallback if converter calibration fails

If no `CL29_WCONST_OVERRIDE` combination restores a bounded recurring cycle without the futile
cycle or succession damage, the minimal Fortran alternative is a **senescence-triggered
formation** in `aquabc_II_pelagic_lib_NOSTACALES.f90:356`: trigger `R_FORM` on NOST *decline*
(e.g. when growth < loss, or biomass past peak) rather than a fixed temperature/day window, so
akinetes form from the actual bloom regardless of calendar. This is a small, local kinetics
change (one `where` condition) but a Fortran edit + rebuild, and is out of scope for the
converter-only first pass.

## 7. Files and effort

- **Converter:** akinete params in `CL29_WCONST_OVERRIDE` (`tools/eutropy_poc/eutropy_to_estas.py`):
  `T_FORM_AKI`, optionally `DAY_FORM_AKI`, `K_LOSS_AKI`/`K_MORT_AKI_20`, and (Lever B) NOST
  kinetic constants.
- **No Fortran, no rebuild** for the primary path (the existing binary is reused).
- **Effort:** the code change is trivial (a few converter override entries); the **cost is the
  empirical calibration** — an iterative 5-yr run loop to find a self-sustaining, non-degrading
  parameter set, which given the failure history may or may not converge.

## 8. Open questions

1. **`T_FORM_AKI` value** — the futile-cycle risk (§3.1) bounds it from above (must stay ≤ or
   near `T_GERM_AKI=21`); calibration finds the value that catches the bloom's cooling tail with
   enough biomass.
2. **Whether Lever A alone suffices**, or Lever B (NOST persistence) is required — an empirical
   question resolved in the first calibration experiment.
3. **Recalibration home** — `CL29_WCONST_OVERRIDE` (CL29-scoped, recommended) vs shared
   `const_net_calibrated.txt` (all apps). Default: converter-scoped until validated broadly.
