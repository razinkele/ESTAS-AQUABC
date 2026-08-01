# AQUABC / ESTAS — Consolidated Backlog

Single internal tracker. It consolidates the open items that were previously scattered across
`FORTRAN_IMPLEMENTATION_PLAN.md` (§8 Phase 5, §2.2 issue table), the EUTROPY↔AQUABC comparison
paper (`~/curonian/docs/EUTROPY_AQUABC_comparison*` §10.3 / §11.2), and the science/calibration
campaigns tracked in project notes. **Detailed task specs live in the referenced sources — this
file is the prioritized index, not a duplicate**, so it can't drift from the plan.

**Last updated:** 2026-07-31 · **Latest release:** v0.9.0 · Fortran-plan Phases 1–4 and every item
in `FIXES_AND_IMPROVEMENTS.md` are complete; there are **no open GitHub issues**.

---

## Recently completed (anchor points)

- **Resuspension global state → `resuspension_t`** — first slice of Phase 5.1 global-state
  reduction; `GLOBAL` allocatable count 55→44; byte-identical; merged **`2014265`**. Established a
  reusable byte-identical-refactor method (determinism pre-check → string/comment-aware rename →
  strip-and-compare pure-prefix proof → CI matrix incl. ifx).
- **v0.6.0 loadable-setup registry** — Standard (25-box) / CL29 (29-box) / CL29-clim as a one-click
  selection, with a Dashboard ↔ Run-Model selector sync and an OpenMP thread cap. Addresses much of
  the paper's §10.3 "lower the barrier to entry".
- **External calibration + validation harness** — EPA (~70k samples) + KM hydrochem ingesters,
  `tools/validate_cl29_vs_epa.py`, `tools/compare_validation_runs.py`, and PEST++ ensemble
  calibration. Realizes the paper's §10.3 alternative ("document the external PEST-style workflow");
  see the caveat under **Calibration** below — the in-Fortran objective is still a stub.
- Fortran-plan Phases 1–4 (safety, modernization, testing, OpenMP) and the whole
  `FIXES_AND_IMPROVEMENTS.md` list — done.
- **v0.8.0 benthic denitrification** (`CL29_BENTHIC_DENIT`, `78ce4ad`) — config-only NO3 sink; closes
  ~20% of the CL29 summer-NO3 over-prediction. See §4.
- **Configurable pelagic solver (RK2/Heun)** — shipped as an experimental opt-in via `ESTAS_PELAGIC_SOLVER`
  env (default Euler, byte-identical). Investigation concluded RK2 is correct but ~1st-order and not better
  than Euler for this model (MIN_CONCENTRATION clamping dominates). *Retires the old "expose RK2" backlog item.*
- **Phase 5.1 de-globalization — two more slices merged:** bottom-sediment `SED_*` → `sediment_state_t`/`bsed`
  (`1a413e9`) and settling/water-coupling → `wsc_state_t` (`54a2de2`), both byte-identical. `mod_GLOBAL`
  allocatables now **12** (the pelagic water-column core). *Retires the "sediment `SED_*` cluster" as the next slice.*
- **Sediment-redox Fe(II) salt-selection bugfix** (`maxloc … dim=2→dim=3`) — merged to `main` (PR #80).

---

## 0. In-flight — resuspension × sediment-diagenesis coupling

*Source: `docs/superpowers/specs/2026-07-30-resuspension-diagenesis-coupling-design.md`. Branch:
`feature/resuspension-diagenesis-coupling`.*

Lets bed resuspension (prescribed-velocity "Option 3") run **together with full diagenesis**
(`MODEL_SEDIMENTS=2`) with a mass-conserving bed↔water particulate transfer — the previously-guarded,
never-run combination. Built on the collaborator's `ali_version` Option-3 scaffolding.

| Item | Status |
|------|--------|
| Areal-vs-volumetric bed-side erosion conservation fix (missing `÷SED_DEPTHS`) | **Done** — flux ratio 1.0; `SED_PSi` delta −822. |
| Particulate **C/N/P** water handoff (`FLX_SED_MOD_1_TO_ALUKAS_II_VEC`, water 9/10/11 ← bed slots 10/4/7) | **Done** — mirrors the proven PART_Si channel. Verified: noero (diagenesis-without-resuspension) **byte-identical** (0 diffs), ero now delivers C/N/P detritus to the water column (+7.4/+2.4/+0.008) matching bed decrements (−9355/−5321/−84). Coupling is mass-conserving for all four particulates. |
| Shear-driven erosion rate (Phase 2, `E=E₀(τ_b/τ_c−1)`) | Deferred (spec Option B). |

**Phase 1 of the coupling is complete and verified** — the branch is now merge-ready (pending review). Only the
optional Phase 2 (shear-driven erosion realism) remains.

Two mergeable side-branches already split out: `feature/pelagic-negmass-diagnostics` (Ali's negative-mass
diagnostics refactor) and `docs/coupling-reconciliation` (the design reconciliation + this backlog refresh).

---

## 1. Engineering — Phase 5 "Advanced Refactoring"
*Source: `FORTRAN_IMPLEMENTATION_PLAN.md` §8 — DEFERRED / LOW priority.*

| ID | Item | Status | Notes |
|----|------|--------|-------|
| 5.1 | Reduce global state (`mod_GLOBAL` 50+ vars, ~416 `pelagic_internal` arrays → derived types) | **Substantially complete** | 4 byte-identical slices merged: resuspension (`2014265`), bottom-sediment (`1a413e9`), water-coupling (`54a2de2`), and the **pelagic-core `pcore` Tiers 1–2** (PRs #88 `1c2c9ef` / #89 `7c5d85e`) — `mod_GLOBAL` loose pelagic allocatables **12→4**. **Tier 3 (`pH`/`STATE_VARIABLES`/`MODEL_CONSTANTS`/`PROCESS_RATES`): NO-GO (2026-08-01)** — cosmetic count only (`pcore` stays in GLOBAL → zero coupling change) vs highest-risk/~1000 sites. See `docs/superpowers/specs/2026-08-01-pelagic-core-deglobalization-design.md`. |
| 5.2 | Separate CO2SYS into a standalone library (`CO2SYS_LIB/` + CMake) | Open | |
| 5.3 | Higher-order ODE solvers (beyond the existing RK2/Heun) | Open | |
| 5.4 | Runtime configuration (config-driven instead of recompile) | Open | RK2-via-config already shipped (see "Recently completed"). |
| — | Expose the existing RK2/Heun solver via config | **Done** | Shipped experimental via `ESTAS_PELAGIC_SOLVER` env (default Euler, byte-identical). |
| — | Inconsistent naming (throughout) | Open (Low) | §2.2 issue table. |

## 2. Calibration & reproducibility
*Source: paper §10.3 / §11.2.*

| Item | Priority | Status / notes |
|------|----------|----------------|
| Calibration objective in `mod_COST_FUNCTION` | P1 | **Still a 40-line stub** (alloc/dealloc only). Two paths: (a) implement the in-Fortran objective, or (b) formally document the external PEST++ workflow that now exists. Tooling for (b) is built; the stub itself is unchanged. |
| Auto-generate PEST template/instruction/control files | P1 | Partially covered by the calibration harness; not yet a first-class AQUABC feature. |
| Reproducibility: complete the constants file (`const_CL` reported 306 vs 318 expected, silent default-fill), fail-loud on missing constants, containerized build | P1 | Re-verify the exact counts before acting. |
| Global sensitivity / identifiability (Morris/Sobol over ~300 constants) | P2 | Shows which parameters the data can actually constrain. |

## 3. Science / model richness
*Source: paper §11.2 — larger lifts.*

| Item | Priority | Notes |
|------|----------|-------|
| Variable-stoichiometry option for key groups (Si:C, N:C plasticity) | P2 | Selective ERSEM-style realism, no full rewrite. **Now concretely motivated (2026-07-25):** the CL29 summer-NO3 over-prediction is unfixable by uptake precisely because N:C is fixed at 0.22 — phyto can't luxury-uptake N without over-growing biomass (breaking Chl-a). Variable N:C would let the model strip summer DIN to observed near-depletion. See §4 summer-NO3 + `cl29-epa-validation`. |
| Re-introduce explicit bacteria as a library (nitrifiers / heterotrophs / denitrifiers + electron acceptors) | P2 | Restores dynamic remineralization; corrects the organic-carbon underestimate. |
| Function-oriented zoobenthos library (filters / shredders / predators; e.g. *Dreissena*) | P3 | General, not Curonian-specific. |
| Unify ESTAS box ↔ SHYFEM 3-D (one kinetic core, two deployments) | P3 | Removes drift between standalone and SHYFEM-bundled versions. |
| Uncertainty quantification (ensembles) | P3 | Needs high-resolution boundary + calibration data. |

## 4. Science — Curonian Lagoon (data-blocked / decided)
*Status as of 2026-07-23; from project notes.*

| Item | Status |
|------|--------|
| CL29 facies map + silica calibration | **Data-blocked** — needs the sediment facies map and Si observations. |
| CL29 sediment Phase 2 (muddy-anoxic-P flux) | **NO-GO** at the §4.1a gate under advanced-redox = 0. |
| Nostocales multi-year bloom | **Won't-fix** — de-risked as not-defensibly-fixable (competitive exclusion under P-limitation). |
| CL29 PEST-posterior promotion | **Abandoned** — non-stationarity; PR #61 landed net-zero, kept only the tooling. |
| FIX_CYN (N2-fixing cyano) to observed biomass | **Won't-fix / decided (2026-07-25)** — temperature diagnosis correct (T_opt=26 too warm) but FIX_CYN-as-a-*fixer* is not reproducible: NH4-floor competitive exclusion (summer DIN floored ~0.06 by regeneration-driven NH4 that matches EPA → non-fixers stay 87% N-replete; fixers never competitive). Growing it to observed biomass also regresses Chl-a/Si/TN (single-var vs multivar tension). Same class as Nostocales. See `fix-cyn-n2fixation-overprediction`. |
| CL29 summer-NO3 over-prediction | **Partial fix ADOPTED (2026-07-25, `78ce4ad`)** — benthic denitrification `CL29_BENTHIC_DENIT` (config-only, converter option) closes ~20% of the summer gap: NO3 bias +0.065→+0.033, TN/DO better, small structural PO4 regression (+5.7% RMSE, unavoidable). Residual ~80% is spring-pool-drainage-dominated + the aseasonal-diatom-metabolism limit (uptake blocked by **fixed N:C** — see §3 variable-stoichiometry). A documented multi-factor residual alongside PO4/Si. |

---

## Phase 5.1 status: substantially complete — no shovel-ready slice remains

`mod_GLOBAL`'s loose pelagic allocatables are down **12→4** (pelagic-core `pcore` Tiers 1–2 merged); the
resuspension, bottom-sediment and water-coupling subsystems were done earlier. **Tier 3 (the last 4 core
arrays) is a decided NO-GO** — cosmetic count with zero coupling change vs the highest risk/effort (see 5.1
above). So there is no low-risk mechanical de-globalization slice left to pick up. What remains is either
deferred-by-design (5.2 CO2SYS-lib, 5.3 higher-order solvers, 5.4 config-driven runtime — all low priority),
science (§3, esp. variable N:C stoichiometry) or waiting on data/decisions (§4). The next *substantive*
engineering lift would be an actual de-coupling (lift `pcore`/`bsed`/`wsc`/`resusp` out of `GLOBAL` into
their own modules) — a design effort, not a mechanical slice.

## Sources (authoritative detail — do not duplicate here)

- `FORTRAN_IMPLEMENTATION_PLAN.md` — §8 Phase 5 task detail, §2.2 open-issue table, §11 verification.
- `~/curonian/docs/EUTROPY_AQUABC_comparison*` — §10.3 (AQUABC improvements), §11.2 (AQUABC roadmap).
- `FIXES_AND_IMPROVEMENTS.md` — all items resolved (historical snapshot).
- `CHANGELOG.md` — release history and per-release verification records.
