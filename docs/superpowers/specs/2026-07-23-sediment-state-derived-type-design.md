# Bottom-Sediment Global-State → Derived Type — Design

**Status:** Approved (2026-07-23)
**Scope:** Fortran only (ESTAS). A behavior-preserving refactor. No numerics change.
**Backlog item:** `FORTRAN_IMPLEMENTATION_PLAN.md` §8.1 Task 5.1 "Reduce Global State" — the
second bounded slice, following the resuspension slice
(`docs/superpowers/specs/2026-07-22-resuspension-state-derived-type-design.md`, merged `2014265`).

## Problem

`mod_GLOBAL.f90` (module `GLOBAL`) is a god-module reached by host association (`use GLOBAL`)
across ~11 files. After the resuspension slice it holds **44 allocatable** declarations. This design
takes the next **cohesive, self-contained slice** — the bottom-sediment submodel state — and
encapsulates it into a derived type owned by its natural module, as a bounded, byte-identical step.

## The slice: the bottom-sediment submodel

An explicitly-delimited block in `mod_GLOBAL.f90` lines **133–207**
(`! Variables for bottom sediment submodel` … `! End of variables for bottom sediment submodel`) —
**24 members** (21 allocatable + 3 scalars). Why this slice:

- **Blast radius = 5 files, all in `SOURCE_CODE/ESTAS/`**: `mod_BOTTOM_SEDIMENTS.f90` (owns *all*
  allocation + input reading; ~214 refs), `mod_SOLVER.f90` (per-timestep physics; ~59 refs),
  `ESTAS_II.f90` (deallocation on teardown; ~20 refs), `mod_SIMULATE.f90` (the sediment call site;
  ~15 refs), `mod_AQUATIC_MODEL.f90` (config; 2 refs).
- **Zero references in `AQUABC/`.** The AQUABC library sediment files (`aquabc_II_sediment_*.f90`,
  incl. `AQUABC_SEDIMENT_LIBRARY/*_DOC_MINER.f90` / `*_POC_DISSOLUTION.f90`) only `use GLOBAL, only:`
  the *dimension constants* (`nstate`→`NSTATE_CHECK`, `NUM_SED_VARS`→`SED_VARS_CHECK`) — **not** any
  block member. They receive the state as dummy arguments, so they are entirely outside the rename
  surface and off the OpenMP pelagic hot path.
- **`mod_SED_TYPEMAP.f90` is NOT in the rename surface** despite matching the member names: it
  `use precision_kinds, only: DBL` (never `GLOBAL`), and its `sed_depths`/`init_sed_state_vars`/…
  identifiers are `intent(out)` **dummy arguments** (lowercase, a separate scope). Full 24-name,
  case-insensitive discovery confirms the rename surface is exactly the **5 files** above (+ the
  GLOBAL declaration deletion); no sixth ESTAS file references any member as a module variable.
- **A dedicated owning module already exists** (`mod_BOTTOM_SEDIMENTS.f90`, module
  `BOTTOM_SEDIMENTS`) — it already allocates (`:375–398`) and reads every one of these; only their
  *declarations* live in GLOBAL. Exact analog of `mod_RESUSPENSION`.
- **Byte-identical is directly, fully gate-able.** Unlike the resuspension slice (whose option-1
  members no committed input exercised), committed setups turn the sediment model **on**:
  `INPUT_gf_release.txt` / `INPUT_3560day.txt` (`MODEL_SEDIMENTS 1`) and `INPUT_sediment_test.txt`
  (`MODEL_SEDIMENTS 2`). The Standard `INPUT.txt` has `MODEL_SEDIMENTS 0` (code skipped). So the run
  gate genuinely exercises the moved path — a materially stronger guarantee than the resuspension
  slice had.

The 24 members (verbatim from `mod_GLOBAL.f90:136–206`):

| Member | Type |
|---|---|
| `INIT_SED_STATE_VARS` | `real(DBL), allocatable (:,:,:)` |
| `SED_DEPTHS` | `real(DBL), allocatable (:,:)` |
| `SED_POROSITIES` | `real(DBL), allocatable (:,:)` |
| `SED_DENSITIES` | `real(DBL), allocatable (:,:)` |
| `PART_MIXING_COEFFS` | `real(DBL), allocatable (:,:,:)` |
| `SED_DIFFUSIONS` | `real(DBL), allocatable (:,:,:)` |
| `SURF_MIXLEN` | `real(DBL)` *(scalar)* |
| `SED_BURRIALS` | `real(DBL), allocatable (:,:)` |
| `SURF_WATER_CONCS` | `real(DBL), allocatable (:,:)` |
| `SED_TEMPS` | `real(DBL), allocatable (:,:)` |
| `SED_MODEL_CONSTANTS` | `real(DBL), allocatable (:)` |
| `SED_TYPE_PER_BOX` | `integer, allocatable (:)` |
| `PROCESSES_sed` | `real(DBL), allocatable (:,:,:,:)` |
| `SED_DRIVING_FUNCTIONS` | `real(DBL), allocatable (:,:)` |
| `FLUXES_TO_SEDIMENTS` | `real(DBL), allocatable (:,:)` |
| `ADVECTIVE_VELOCITY` | `real(DBL)` *(scalar)* |
| `H_ERODEP` | `real(DBL), allocatable (:)` |
| `SED_FLAGS` | `integer, allocatable (:)` |
| `NUM_FLUX_RECEIVING_SED_LAYERS` | `integer` *(scalar)* |
| `FINAL_SED_STATE_VARS` | `real(DBL), allocatable (:,:,:)` |
| `FLUXES_FROM_SEDIMENTS` | `real(DBL), allocatable (:,:)` |
| `SED_OUTPUTS` | `real(DBL), allocatable (:,:,:)` |
| `SED_SAVED_OUTPUTS` | `real(DBL), allocatable (:,:,:)` |
| `SED_BURRIAL_RATE_OUTPUTS` | `real(DBL), allocatable (:,:,:)` |

## Approach (chosen): encapsulate into `sediment_state_t`, owned by `mod_BOTTOM_SEDIMENTS`

Define a derived type holding the 24 members and a single module-scoped instance **`bsed`** in the
module that already manages them. Instance name `bsed` (not `sed`) deliberately: `sed` collides
visually with the `sed`-based strip-and-compare command and is a substring of the block names.
Type name `sediment_state_t` follows the `FORTRAN_IMPLEMENTATION_PLAN.md` §8.1 sketch. Arg-threading
through the RK derivative interface was rejected for the same reason as resuspension (only sensible
as part of de-globalizing the whole solver — the deferred full Phase 5). No existing `bsed` or
`sediment_state_t` symbol exists anywhere in the tree (verified) — no collision.

### Type + instance (in `mod_BOTTOM_SEDIMENTS.f90`, before `contains`)

```fortran
type, public :: sediment_state_t
    real(kind = DBL), allocatable, dimension(:, :, :)    :: INIT_SED_STATE_VARS
    real(kind = DBL), allocatable, dimension(:, :)       :: SED_DEPTHS
    real(kind = DBL), allocatable, dimension(:, :)       :: SED_POROSITIES
    real(kind = DBL), allocatable, dimension(:, :)       :: SED_DENSITIES
    real(kind = DBL), allocatable, dimension(:, :, :)    :: PART_MIXING_COEFFS
    real(kind = DBL), allocatable, dimension(:, :, :)    :: SED_DIFFUSIONS
    real(kind = DBL)                                     :: SURF_MIXLEN
    real(kind = DBL), allocatable, dimension(:, :)       :: SED_BURRIALS
    real(kind = DBL), allocatable, dimension(:, :)       :: SURF_WATER_CONCS
    real(kind = DBL), allocatable, dimension(:, :)       :: SED_TEMPS
    real(kind = DBL), allocatable, dimension(:)          :: SED_MODEL_CONSTANTS
    integer,          allocatable, dimension(:)          :: SED_TYPE_PER_BOX
    real(kind = DBL), allocatable, dimension(:, :, :, :) :: PROCESSES_sed
    real(kind = DBL), allocatable, dimension(:, :)       :: SED_DRIVING_FUNCTIONS
    real(kind = DBL), allocatable, dimension(:, :)       :: FLUXES_TO_SEDIMENTS
    real(kind = DBL)                                     :: ADVECTIVE_VELOCITY
    real(kind = DBL), allocatable, dimension(:)          :: H_ERODEP
    integer,          allocatable, dimension(:)          :: SED_FLAGS
    integer                                              :: NUM_FLUX_RECEIVING_SED_LAYERS
    real(kind = DBL), allocatable, dimension(:, :, :)    :: FINAL_SED_STATE_VARS
    real(kind = DBL), allocatable, dimension(:, :)       :: FLUXES_FROM_SEDIMENTS
    real(kind = DBL), allocatable, dimension(:, :, :)    :: SED_OUTPUTS
    real(kind = DBL), allocatable, dimension(:, :, :)    :: SED_SAVED_OUTPUTS
    real(kind = DBL), allocatable, dimension(:, :, :)    :: SED_BURRIAL_RATE_OUTPUTS
end type sediment_state_t

type(sediment_state_t), public :: bsed
```

`DBL` is already in scope in `mod_BOTTOM_SEDIMENTS` via `use GLOBAL` (which re-exports
`precision_kinds`, and `mod_BOTTOM_SEDIMENTS` also `use UTILS_1`). **No default initializers** on
the scalar components — the originals are uninitialized module vars set by code, and the allocatable
components start unallocated exactly like the current module arrays — preserving byte-identical
semantics. Component **declaration order preserved** verbatim from GLOBAL (irrelevant to semantics,
but keeps the strip-and-compare diff minimal).

### Edits per file

1. **`mod_GLOBAL.f90`** — delete the 24 declarations at lines **136–206** (the whole block between
   the `! Variables for bottom sediment submodel` header at 133 and the
   `! End of variables for bottom sediment submodel` footer at 207). Leave a breadcrumb comment in
   their place pointing to `sediment_state_t`/`bsed` in `mod_BOTTOM_SEDIMENTS`, mirroring the
   resuspension breadcrumb at `mod_GLOBAL.f90:250–260`. Net: `GLOBAL` loses 24 declarations
   (21 allocatable → its `allocatable` count drops **44 → 23**).
   ⚠️ Do **not** touch the adjacent blocks: `! water column – bottom sediment interaction`
   (`FLUXES_TO_WATER_COLUMN`, `FLUXES_OUTPUT_TO_WATER_COLUMN`, 209–212), the settling/deposition
   arrays (`DISSOLVED_FRACTIONS` … `DEPOSITION_AREA_RATIOS`, 214–223), the `*_FILENAME` scalars
   (225–232), and `BOTTOM_SED_ADVANCED_REDOX_SIMULATION` (246) — all stay in GLOBAL (separate future
   slices / different subsystem).
2. **`mod_BOTTOM_SEDIMENTS.f90`** (module `BOTTOM_SEDIMENTS`) — add the type + `bsed` instance before
   `contains`; rewrite every bare member reference in its own procedures to `bsed%MEMBER`
   (allocations become `allocate(bsed%INIT_SED_STATE_VARS(...))`, etc.). It still `use GLOBAL` for
   the dimension constants (`NUM_SED_LAYERS`, `NUM_SED_VARS`, `NUM_SED_CONSTS`, `nkn`, …), which are
   **not** moving.
3. **`ESTAS_II.f90`** — already `use BOTTOM_SEDIMENTS`; rewrite the ~20 refs (chiefly the
   `deallocate(...)` teardown at `:103–122`) to `bsed%…` (`deallocate(bsed%INIT_SED_STATE_VARS)`).
4. **`mod_AQUATIC_MODEL.f90`** — already `use BOTTOM_SEDIMENTS`; rewrite the 2 refs to `bsed%…`.
5. **`mod_SOLVER.f90`** (module `PELAGIC_SOLVER`) — already `use BOTTOM_SEDIMENTS`; rewrite the
   ~59 refs to `bsed%…`.
6. **`mod_SIMULATE.f90`** — **add `use BOTTOM_SEDIMENTS, only: bsed`** (it currently reaches the
   members via `use GLOBAL`; `use` is not transitive through its `use AQUATIC_MODEL` /
   `use PELAGIC_SOLVER`, so an explicit import is required). Rewrite the ~15 refs to `bsed%…`.

### Rename discipline — word-boundary is mandatory here

Unlike the contained resuspension slice, this block has a **pervasive substring-collision hazard**:
a systematic `*_LOC` local-copy convention plus `NUM_*` counters and `COCOA_*_FILENAME` scalars embed
member names as substrings. A naive (non-word-boundary) replace would corrupt **all** of these; a
word-boundary exact-token replace (`\bMEMBER\b → bsed%MEMBER`, applied only in code segments, leaving
string literals and comments verbatim) leaves **every one** of them untouched, because none has a
word boundary at the member-name edge (the adjoining char is always `_` or an alphanumeric). The
**do-not-touch** look-alikes, enumerated from the 5 files:

- **`*_LOC` local copies:** `SED_DEPTHS_LOC`, `SED_POROSITIES_LOC`, `SED_DENSITIES_LOC`,
  `PART_MIXING_COEFFS_LOC`, `SED_DIFFUSIONS_LOC`, `SURF_MIXLEN_LOC`, `SED_BURRIALS_LOC`,
  `SED_TEMPS_LOC`, `ADVECTIVE_VELOCITY_LOC`, `SED_FLAGS_LOC`, `NUM_SED_FLAGS_LOC`.
- **`NUM_*` counters:** `NUM_SED_FLAGS`, `NUM_SED_OUTPUTS`, `NUM_SED_SAVED_OUTPUTS`,
  `NUM_FLUXES_TO_SEDIMENTS`, `NUM_FLUXES_FROM_SEDIMENTS`.
- **`COCOA_*_FILENAME` scalars:** `COCOA_FLUXES_TO_SEDIMENTS_FILENAME`,
  `COCOA_FLUXES_FROM_SEDIMENTS_FILENAME`.
- **module / subroutine names:** `AQUABC_BSED_MODEL_CONSTANTS`, `INIT_BSED_MODEL_CONSTANTS`
  (embed `SED_MODEL_CONSTANTS`).

Also note these are **separate variables**, not shadows: the exact-name shadowing pre-check
(§Verification) is clean — no subroutine declares a local/dummy whose name *equals* a block member —
so once the forward replace is word-boundary-anchored, the strip-and-compare proof is airtight.

**String-literal blind spot:** a preliminary scan found **no** member name inside a string literal
(the two grep hits — `mod_SOLVER.f90:300`, `mod_BOTTOM_SEDIMENTS.f90:442` — are real code refs that
merely follow a `'...'` label on the same `write` line, and *are* rewritten). The implementation must
re-confirm this exhaustively, since the byte-identical `OUTPUTS/` gate cannot see stdout-label
corruption. This blind spot is expected to be empty here (contrast resuspension's
`'RESUSPENSION_OPTION : '`).

Reference-site count is **advisory only** — a clean compile under `implicit none` is the real
completeness gate, since a stale bare reference becomes an undeclared symbol → hard error.

### No circular `use`

`mod_BOTTOM_SEDIMENTS` uses `GLOBAL`, `UTILS_1`, `AQUABC_II_GLOBAL`, `TIME_SERIES`,
`AQUABC_BSED_MODEL_CONSTANTS`, `para_aqua`, `SED_TYPEMAP` — **none** of which is a consumer module,
and none uses `BOTTOM_SEDIMENTS`. Three consumers (`ESTAS_II`, `mod_AQUATIC_MODEL`, `mod_SOLVER`)
already `use BOTTOM_SEDIMENTS` today; adding `use BOTTOM_SEDIMENTS, only: bsed` to `mod_SIMULATE` is
the same shape and introduces no cycle. The multi-pass `make_lib.sh` resolves ordering by
construction.

## Non-goals (YAGNI)

- **No arg-threading** (rejected above).
- **No numerics / logic change** — a pure move + rename. The byte-identical gate enforces this.
- **No adjacent-block moves** — the settling/deposition arrays, water↔sediment coupling fluxes,
  `*_FILENAME` scalars, and `BOTTOM_SED_ADVANCED_REDOX_SIMULATION` all stay in GLOBAL; they are
  separate future slices.
- **No latent-bug fixes / dead-code removal** — a byte-identical refactor does not change behavior.
  Note anything found; leave it (file a follow-up if warranted).

## Verification

The gate is **byte-identical model output**, since this is a behavior-preserving refactor:

1. **Shadowing pre-check — DONE, CLEAN.** No file declares a local/dummy whose name equals a block
   member (grepped `::`/`intent(` context across the 5 files). This makes strip-and-compare airtight.
1b. **OpenMP-clause pre-check — DONE, EMPTY.** No member appears in any `!$omp` data-sharing clause
   (`private`/`firstprivate`/`copyin`/`threadprivate`/`reduction`/`shared`) in the 5 files. This
   matters because the rename is *comment-aware* and `!$omp` sentinels look like comments to it but
   are active code under `-fopenmp` — a member in a clause would break only an OpenMP build, invisible
   to the serial byte-identical gate. Empty result confirms the serial gate is sufficient (the
   sediment model runs once per timestep, outside the parallelized pelagic-kinetics region).
1c. **Casing uniformity — DONE, UNIFORM.** Every member is written with a single casing across all 5
   files (`PROCESSES_sed` — the only mixed-case member name — appears identically in all 7 of its
   occurrences). Since Fortran is case-insensitive but `sed`/`grep` are not, a case-varying reference
   would be missed by a case-sensitive `\bNAME\b` replace; uniform casing means the case-sensitive
   rename and strip-proof are honest. (Belt-and-suspenders: run the replace case-insensitively.)
2. **Determinism pre-check.** With the **pre-change** binary, run the sediment-ON gate setup twice
   into separate output dirs and diff — self-diff must be 0 before a 0-diff post-change is meaningful
   (this tree has real uninit-memory non-determinism history — see `[[fortran-uninit-debugging]]`).
3. **Build** the library + `ESTAS_II` cleanly (`make clean-all && make build-estas`) — the compiler
   catches any *missed* reference or name clash (stale bare ref → undeclared under `implicit none`).
   It does **not** catch a *wrong same-type* swap between two identically-typed members — that is
   what steps 4–5 cover.
4. **Strip-and-compare pure-prefix proof.** For each edited file,
   `diff <(git show HEAD~1:FILE) <(sed 's/bsed%//g' FILE)` must show **only** structural changes
   (the type block, the new `use` line in `mod_SIMULATE`, the GLOBAL deletion, any 132-col wraps) —
   proving no member was swapped/dropped/misspelled and, critically, **zero `*_LOC` corruption**.
   Separately normalize-diff the moved declarations against the original GLOBAL block to confirm
   kind/dimension fidelity (the strip proof cannot check *moved* decls).
5. **Byte-identical run gate (primary).** Build serial (default `make build-estas`; **not**
   `OPENMP=1`), identical compiler/flags both sides. Run a **`MODEL_SEDIMENTS`-on** setup
   (`INPUT_gf_release.txt`, `MODEL_SEDIMENTS 1`) to completion with the pre-change binary; snapshot
   `OUTPUTS/`. Rebuild post-change, rerun identical input, diff `OUTPUTS/` **bit-for-bit** (max |Δ| =
   0). This *does* exercise the moved sediment path (the resuspension slice could not). Secondary:
   the Standard `INPUT.txt` (`MODEL_SEDIMENTS 0`, code skipped) must also stay bit-identical — a cheap
   regression that allocation/order wasn't perturbed. Optionally also gate `INPUT_sediment_test.txt`
   (`MODEL_SEDIMENTS 2`) to cover mode 2.
6. **132-column free-form wrap.** The `bsed%` prefix (+5 chars) can push long continuation lines past
   gfortran's 132-col limit → truncated `&` → "Syntax error in argument list". Wrap at operators /
   after commas; pure formatting, no semantic change.
7. **Fortran unit tests** (`make test`) — build-health only; the units link none of the 5 ESTAS
   files, so they give zero coverage of the moved subsystem. Keep green regardless.
8. **CI matrix** (gfortran ubuntu/macOS + ifx oneAPI) green — guards the `use`-ordering change across
   compilers.

## Rollout

Single PR on `refactor/bottom-sediment-state-derived-type`. Green CI + byte-identical
sediment-ON + Standard gate + the strip-and-compare proof, then merge on the user's go-ahead. Record
the byte-identical result and the strip-proof output in the PR body.
