# AQUABC / ESTAS — Consolidated Backlog

Single internal tracker. It consolidates the open items that were previously scattered across
`FORTRAN_IMPLEMENTATION_PLAN.md` (§8 Phase 5, §2.2 issue table), the EUTROPY↔AQUABC comparison
paper (`~/curonian/docs/EUTROPY_AQUABC_comparison*` §10.3 / §11.2), and the science/calibration
campaigns tracked in project notes. **Detailed task specs live in the referenced sources — this
file is the prioritized index, not a duplicate**, so it can't drift from the plan.

**Last updated:** 2026-07-23 · **Latest release:** v0.7.0 · Fortran-plan Phases 1–4 and every item
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

---

## 1. Engineering — Phase 5 "Advanced Refactoring"
*Source: `FORTRAN_IMPLEMENTATION_PLAN.md` §8 — DEFERRED / LOW priority.*

| ID | Item | Status | Notes |
|----|------|--------|-------|
| 5.1 | Reduce global state (`mod_GLOBAL` 50+ vars, ~416 `pelagic_internal` arrays → derived types) | **In progress** | Resuspension slice done (`2014265`). Next natural slice: the sediment `SED_*` clusters. Reuse the resuspension method. |
| 5.2 | Separate CO2SYS into a standalone library (`CO2SYS_LIB/` + CMake) | Open | |
| 5.3 | Higher-order ODE solvers (beyond the existing RK2/Heun) | Open | |
| 5.4 | Runtime configuration (config-driven instead of recompile) | Open | Overlaps the RK2-via-config item below. |
| — | Expose the existing RK2/Heun solver via config | Open (P2) | Code present (`mod_SOLVER.f90`, `PELAGIC_SOLVER_NO == 2`) but hard-coded to Euler at `mod_SIMULATE.f90:91` (`PELAGIC_SOLVER_NO = 1`). A one-line switch + input plumbing unlocks it. |
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
| Variable-stoichiometry option for key groups (Si:C, N:C plasticity) | P2 | Selective ERSEM-style realism, no full rewrite. |
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

---

## The one shovel-ready engineering item

Continue **Phase 5.1** with the next `GLOBAL` / `pelagic_internal` slice (sediment `SED_*` cluster
is the obvious next bite), using the byte-identical method proven on resuspension. Everything else
is either deferred-by-design (5.2–5.4, low priority) or waiting on data/decisions.

## Sources (authoritative detail — do not duplicate here)

- `FORTRAN_IMPLEMENTATION_PLAN.md` — §8 Phase 5 task detail, §2.2 open-issue table, §11 verification.
- `~/curonian/docs/EUTROPY_AQUABC_comparison*` — §10.3 (AQUABC improvements), §11.2 (AQUABC roadmap).
- `FIXES_AND_IMPROVEMENTS.md` — all items resolved (historical snapshot).
- `CHANGELOG.md` — release history and per-release verification records.
