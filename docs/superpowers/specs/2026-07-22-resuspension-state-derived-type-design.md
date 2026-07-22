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

1. **`mod_GLOBAL.f90`** — delete **only** the 19 resuspension declarations at lines **253–271**.
   ⚠️ **Line 272 `integer :: SHUT_DOWN_SETTLING` MUST STAY in `GLOBAL`.** It sits *inside* the
   `! Variables related to sediment resuspension` comment block (251–275) but is a *settling*
   control — **not** a resuspension variable — referenced bare in `mod_AQUATIC_MODEL.f90:288` and
   `mod_SOLVER.f90:1210/1219/1224`, which stay bare. Do **not** sweep it into `resuspension_t`
   (an implementer skimming the comment markers instead of the 19-member table above is the exact
   failure mode this warns against). Leave `SHUT_DOWN_SETTLING` with an appropriate comment. Net:
   `GLOBAL` loses 19 declarations (11 of them allocatable → its `allocatable` count drops 55 → 44).
2. **`mod_RESUSPENSION.f90`** (module `RESUSPENSION`) — add the type + `resusp` instance before
   `contains`; rewrite every bare member reference in its own procedures to `resusp%MEMBER`
   (allocations become `allocate(resusp%…)`, TS reads `resusp%RESUSPENSION_TS(…)`, etc.).
   ⚠️ **Do not touch the local `RESUSPENSION_TS_NO`** (an `integer` loop index at
   `mod_RESUSPENSION.f90:12,44,52,53,104,133,141,142`) — it merely resembles `RESUSPENSION_TS`.
3. **`mod_AQUATIC_MODEL.f90`** — already `use RESUSPENSION`; rewrite bare refs to `resusp%MEMBER`.
4. **`mod_SOLVER.f90`** (module `PELAGIC_SOLVER`) — add `use RESUSPENSION`; rewrite bare refs to
   `resusp%MEMBER`. ⚠️ **Leave these same-prefixed locals alone** (not GLOBAL members): the scalar
   `RESUSPENSION_VELOCITY` (`:813,1452,1467`), the array `RESUSPENSION_CONCENTRATIONS`
   (`:815-816,1459,1467`), and the scalar `SHEAR_STRESS` (`:818,1213,1218`).
5. **`ESTAS_II.f90`** — add `use RESUSPENSION`; rewrite the 3 refs
   (`CRITICAL_SHEAR_STRESS_FILENAME`, `CRIT_SHEAR_FNAME_FROM_OUTSIDE`) to `resusp%…`.

**Rename discipline:** the edit is a symbol-scoped rename of exactly the 19 declared members, not a
regex substring replace — the look-alikes flagged above (`RESUSPENSION_TS_NO`,
`RESUSPENSION_VELOCITY`, `RESUSPENSION_CONCENTRATIONS`, `SHEAR_STRESS`, `SHUT_DOWN_SETTLING`) are
false-positive matches that a non-word-boundary find/replace would corrupt. Total: 122 whole-word
reference sites across the 5 files (GLOBAL 19 decls, `mod_RESUSPENSION` 48, `mod_AQUATIC_MODEL` 36,
`mod_SOLVER` 16, `ESTAS_II` 3).

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
- **Do not "fix" the pre-existing `CONSIDER_RESUSPENSION` read-before-write.** The in-loop review
  found a latent bug: on the `MODEL_BOTTOM_SED_PRESET` path in `READ_AQUATIC_MODEL_INPUTS`
  (`mod_AQUATIC_MODEL.f90:318-338`, sets `RESUSPENSION_OPTION = 0`), the `select case` at
  `mod_AQUATIC_MODEL.f90:402` has no `case(0)`, so `CONSIDER_RESUSPENSION` is never assigned before
  it is read at `mod_AQUATIC_MODEL.f90:493` / `mod_SOLVER.f90:1440`. It is harmless *only* because
  an uninitialized module scalar lands in zeroed BSS and 0 means "no resuspension." This bug is
  **unaffected by the move** (a scalar component of a module-scope derived type zero-inits
  identically — verified empirically with gfortran at `-O0/-O2/-O2 -march=native`), and adding
  `= 0` to the type would silently *change behavior* (fix a latent bug), which is out of scope for
  a byte-identical refactor. Note it; leave it. (Filing it as its own follow-up is fine.)

## In-loop review hardening (2026-07-22)

Three adversarial reviewers verified this design against the actual source:
- **Blast radius** — independently re-derived: exactly the 5 files named, 122 whole-word reference
  sites, zero stragglers, zero `use GLOBAL, only:` clauses naming any of the 19, `use RESUSPENSION`
  confirmed already present in `mod_AQUATIC_MODEL`. → produced the do-not-touch look-alike list.
- **Byte-identical semantics** — no SAVE/persistence, EQUIVALENCE/COMMON/NAMELIST/DATA, pointer/
  target/associate, or finalization hazard (`TIME_SERIE` has only pointer components, no `FINAL`);
  kind/len/dimension fidelity of the type checked term-by-term; scalar-element actual args
  (`resusp%RESUSPENSION_TS(idx)`) pass by reference identically. → surfaced the `SHUT_DOWN_SETTLING`
  carve-out and the pre-existing bug above.
- **Build order / compiler matrix** — the multi-pass `make_lib.sh` resolves forward references by
  construction; the new `PELAGIC_SOLVER→RESUSPENSION` and `ESTAS_II→RESUSPENSION` edges are the same
  shape as the already ifx-CI-green `mod_AQUATIC_MODEL→RESUSPENSION` edge; no module cycle; no
  existing `resusp`/`resuspension_t` symbol to collide with; `mod_RESUSPENSION` already linked.

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
