# Resuspension Global-State → Derived Type — Design

**Status:** Approved (2026-07-22)
**Scope:** Fortran only (ESTAS). A behavior-preserving refactor. No numerics change.
**Backlog item:** `FORTRAN_IMPLEMENTATION_PLAN.md` §2.2 "Global mutable state (50+ variables) — Open" / §8.1 Task 5.1 "Reduce Global State" — executed as one bounded slice.

## Problem

`mod_GLOBAL.f90` (module `GLOBAL`) is a god-module: 55 allocatable/global declarations
reached by host association (`use GLOBAL`) across ~11 files. Reducing it wholesale is the
deferred, multi-sprint Phase 5. This design takes **one cohesive, self-contained slice** and
encapsulates it into a derived type owned by its natural module, as a bounded, byte-identical
first step.

## The slice: the resuspension / shear-stress subsystem

An explicitly-delimited block in `mod_GLOBAL.f90` lines **251–274**
(`! Variables related to sediment resuspension` … `! End of variables related to sediment
resuspension`) — **19 variables**. Why this slice:

- **Blast radius = 4 files, all in `SOURCE_CODE/ESTAS/`**: `mod_RESUSPENSION.f90` (owns *all*
  allocation + file reading), `mod_AQUATIC_MODEL.f90` (config parsing), `mod_SOLVER.f90` (the
  per-timestep physics at :1208 and :1440), `ESTAS_II.f90` (one command-line arg at :21/53/58).
- **Zero references in `AQUABC/`** → entirely off the pelagic OpenMP hot path (no thread-safety
  surface).
- **A dedicated owning module already exists** (`mod_RESUSPENSION.f90`, `use GLOBAL` only) — it
  already allocates and reads every one of these arrays; only their *declarations* live in GLOBAL.
- **No deallocation sites** — allocated once, live for the program; nothing to reroute on teardown.
- **Byte-identical is directly verifiable**: the Standard setup runs `Resuspension:
  Semi-Prescribed`, which exercises this exact code path, so a Standard run diff is a real gate.

The 19 members (verbatim from `mod_GLOBAL.f90:253–271`):

| Member | Type |
|---|---|
| `NUM_RESUSPENSION_TS` | `integer` |
| `RESUSPENSION_OPTION` | `integer` |
| `CONSIDER_RESUSPENSION` | `integer` |
| `ACTIVATE_RESUSPENSIONS` | `integer, allocatable (:)` |
| `FRAC_RESUSPENSION_AREAS` | `real(DBL), allocatable (:)` |
| `RESUSPENSION_CONC_TS_NOS` | `integer, allocatable (:,:)` |
| `RESUSPENSION_CONC_TS_VAR_NOS` | `integer, allocatable (:,:)` |
| `RESUSPENSION_VEL_TS_NOS` | `integer, allocatable (:)` |
| `RESUSPENSION_VEL_TS_VAR_NOS` | `integer, allocatable (:)` |
| `RESUSPENSION_INPUT_FILE_NAME` | `character(2048)` *(dead — 0 refs; moved intact, see Non-goals)* |
| `RESUSPENSION_TS_FILE_NAMES` | `character(2048), allocatable (:)` |
| `RESUSPENSION_TS` | `type(TIME_SERIE), allocatable (:)` |
| `RESUSPENSION_INPUT_FOLDER` | `character(2048)` |
| `RESUSPENSION_OUTPUT_FOLDER` | `character(2048)` |
| `BOX_CRITICAL_SHEAR_STRESSES` | `real(DBL), allocatable (:)` |
| `CRITICAL_SHEAR_STRESS_FILENAME` | `character(2048)` |
| `CRIT_SHEAR_FNAME_FROM_OUTSIDE` | `integer` |
| `SHEAR_STRESS_TS_NOS` | `integer, allocatable (:)` |
| `SHEAR_STRESS_TS_VAR_NOS` | `integer, allocatable (:)` |

## Approach (chosen): encapsulate into `resuspension_t`, owned by `mod_RESUSPENSION`

Define a derived type holding the 19 members and a single module-scoped instance in the module
that already manages them. Arg-threading through the constrained RK derivative interface (§8.1's
literal "pass through arguments") was rejected: the solver's
consumption is inside the generic integrator's derivative evaluator, already host-associated to
dozens of *other* GLOBAL arrays; threading one subsystem there is higher-risk and only sensible as
part of de-globalizing the whole solver (the deferred Phase 5). Encapsulation delivers the P2 goal
(shrink `mod_GLOBAL` by a whole subsystem) at bounded risk with a clean byte-identical gate.

### Type + instance (in `mod_RESUSPENSION.f90`, before `contains`)

```fortran
type, public :: resuspension_t
    integer :: NUM_RESUSPENSION_TS
    integer :: RESUSPENSION_OPTION
    integer :: CONSIDER_RESUSPENSION
    integer,               allocatable, dimension(:)    :: ACTIVATE_RESUSPENSIONS
    real(kind = DBL),      allocatable, dimension(:)    :: FRAC_RESUSPENSION_AREAS
    integer,               allocatable, dimension(:, :) :: RESUSPENSION_CONC_TS_NOS
    integer,               allocatable, dimension(:, :) :: RESUSPENSION_CONC_TS_VAR_NOS
    integer,               allocatable, dimension(:)    :: RESUSPENSION_VEL_TS_NOS
    integer,               allocatable, dimension(:)    :: RESUSPENSION_VEL_TS_VAR_NOS
    character(len = 2048)                               :: RESUSPENSION_INPUT_FILE_NAME
    character(len = 2048), allocatable, dimension(:)    :: RESUSPENSION_TS_FILE_NAMES
    type(TIME_SERIE),      allocatable, dimension(:)    :: RESUSPENSION_TS
    character(len = 2048)                               :: RESUSPENSION_INPUT_FOLDER
    character(len = 2048)                               :: RESUSPENSION_OUTPUT_FOLDER
    real(kind = DBL),      allocatable, dimension(:)    :: BOX_CRITICAL_SHEAR_STRESSES
    character(len = 2048)                               :: CRITICAL_SHEAR_STRESS_FILENAME
    integer                                             :: CRIT_SHEAR_FNAME_FROM_OUTSIDE
    integer,               allocatable, dimension(:)    :: SHEAR_STRESS_TS_NOS
    integer,               allocatable, dimension(:)    :: SHEAR_STRESS_TS_VAR_NOS
end type resuspension_t

type(resuspension_t), public :: resusp
```

`TIME_SERIE` and `DBL` are already in scope in `mod_RESUSPENSION` via `use GLOBAL` (which
`use TIME_SERIES` and `use precision_kinds, only: DBL`). **No default initializers** on the scalar
components — the originals are uninitialized module vars set by code (`ESTAS_II.f90:21` sets
`CRIT_SHEAR_FNAME_FROM_OUTSIDE = 0`; `mod_AQUATIC_MODEL` sets `CONSIDER_RESUSPENSION`), and the
allocatable components start unallocated exactly like the current module arrays — preserving
byte-identical semantics.

### Edits per file

1. **`mod_GLOBAL.f90`** — delete the 19 declarations (lines 253–271); keep or remove the two
   comment markers (251/274). Net: GLOBAL drops from 55 → 36 allocatable/global decls.
2. **`mod_RESUSPENSION.f90`** — add the type + `resusp` instance before `contains`; rewrite every
   bare member reference in its own procedures to `resusp%MEMBER` (allocations become
   `allocate(resusp%…)`, TS reads `resusp%RESUSPENSION_TS(…)`, etc.).
3. **`mod_AQUATIC_MODEL.f90`** — already `use RESUSPENSION`; rewrite bare refs to `resusp%MEMBER`.
4. **`mod_SOLVER.f90`** — add `use RESUSPENSION`; rewrite bare refs to `resusp%MEMBER`.
5. **`ESTAS_II.f90`** — add `use RESUSPENSION`; rewrite the 3 refs
   (`CRITICAL_SHEAR_STRESS_FILENAME`, `CRIT_SHEAR_FNAME_FROM_OUTSIDE`) to `resusp%…`.

### No circular `use`

`mod_RESUSPENSION` uses only `GLOBAL`; `GLOBAL` uses `TIME_SERIES` / `aquabc_pel_state_var_indexes`
/ `precision_kinds` — none of which use `RESUSPENSION` or the consumer modules. Adding
`use RESUSPENSION` to `mod_SOLVER` and `ESTAS_II` introduces no cycle. (`mod_AQUATIC_MODEL` already
uses it.)

## Non-goals (YAGNI)

- **No arg-threading** (rejected above).
- **No numerics / logic change** — a pure move + rename. The byte-identical gate enforces this.
- **No dead-code removal** — `RESUSPENSION_INPUT_FILE_NAME` (0 refs) is moved into the type intact
  rather than deleted; removing dead globals is a separate concern kept out of this slice.
- **Do not touch the sediment (`SED_*`) or sediment↔water coupling clusters** — separate future
  slices.

## Testing / verification

The gate is **byte-identical model output**, since this is a behavior-preserving refactor:

1. **Build** the library + `ESTAS_II` cleanly (`make clean-all && make build-estas`) — the compiler
   catches any missed reference or name clash (every relocated symbol must resolve through
   `resusp%…`).
2. **Fortran unit tests**: `make test` — must stay green.
3. **Byte-identical run gate (primary):**
   - Baseline: with the **pre-change** binary, run the Standard setup to completion; snapshot the
     full `OUTPUTS/` tree.
   - Post-change: rebuild, run the identical Standard input; diff `OUTPUTS/` **bit-for-bit** (max
     |Δ| = 0 across all box files). Standard exercises `Resuspension: Semi-Prescribed`, so this
     directly validates the refactored path.
   - Secondary: a CL29 run (`Resuspension: Off`) must also stay bit-identical (trivially, the code
     is skipped) — a cheap regression check that the move didn't perturb allocation/order.
4. **CI**: the existing matrix (gfortran ubuntu/macOS + ifx oneAPI) must go green — this also
   guards the `use`-ordering / module-dependency change across compilers.

## Rollout

Single PR on `refactor/resuspension-state-derived-type`. Green CI + byte-identical Standard/CL29
gate, then merge on the user's go-ahead. Record the byte-identical result in the PR body.
