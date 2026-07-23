# Water↔Sediment Coupling + Settling Global-State → Derived Type — Design

**Status:** Approved (2026-07-23)
**Scope:** Fortran only (ESTAS). A behavior-preserving refactor. No numerics change.
**Backlog item:** `FORTRAN_IMPLEMENTATION_PLAN.md` §8.1 Task 5.1 "Reduce Global State" — the third
bounded slice, after resuspension (`2014265`) and bottom-sediment (`sediment_state_t`, merged
`d76b148`). See `docs/superpowers/specs/2026-07-23-sediment-state-derived-type-design.md`.

## Problem

`mod_GLOBAL.f90` (module `GLOBAL`) is a god-module. After the resuspension and bottom-sediment slices
it holds **23 allocatable** declarations. This design takes the next cohesive slice — the particle
**settling/deposition** machinery plus the **water↔sediment flux coupling** — and encapsulates it into
a derived type. Unlike the two prior slices, this subsystem has **no dedicated owner module**, so the
type lives in a **new leaf module**.

## The slice: settling/deposition + water↔sediment coupling

Two adjacent, comment-delimited blocks in `mod_GLOBAL.f90` (currently lines **142–156**) — **11
members**, all `real(DBL), allocatable, dimension(:,:)`:

| Member | Sub-block | Exercising code-path (run-gate coverage TBD by §5b) |
|---|---|---|
| `FLUXES_TO_WATER_COLUMN` | water↔sediment coupling (142–145) | `MODEL_BOTTOM_SEDIMENTS > 1` (mode-2) |
| `FLUXES_OUTPUT_TO_WATER_COLUMN` | water↔sediment coupling | `MODEL_BOTTOM_SEDIMENTS > 1` (mode-2) |
| `DISSOLVED_FRACTIONS` | settling/deposition (147–156) | `SHUT_DOWN_SETTLING == 0` (pelagic) |
| `FRACTION_OF_DEPOSITION` | settling/deposition | `SHUT_DOWN_SETTLING == 0` |
| `SETTLING_RATES` | settling/deposition | `SHUT_DOWN_SETTLING == 0` |
| `NOT_DEPOSITED_FLUXES` | settling/deposition | `SHUT_DOWN_SETTLING == 0` |
| `FLUXES` | settling/deposition | `SHUT_DOWN_SETTLING == 0` |
| `SETTLING_VELOCITIES_OUTPUT` | settling/deposition | `SHUT_DOWN_SETTLING == 0` |
| `EFFECTIVE_DISSLOVED_FRACTIONS` | settling/deposition | `SHUT_DOWN_SETTLING == 0` |
| `EFFECTIVE_DEPOSITION_FRACTIONS` | settling/deposition | `SHUT_DOWN_SETTLING == 0` |
| `DEPOSITION_AREA_RATIOS` | settling/deposition | `SHUT_DOWN_SETTLING == 0` |

Why this slice, and how it differs from the prior two:

- **No owner module.** These are allocated in `mod_AQUATIC_MODEL.f90` (`:220–238`) and deallocated in
  `ESTAS_II.f90` (`:83–97`); the physics is in `mod_SOLVER.f90` (module `PELAGIC_SOLVER`, ~68 refs).
  There is no `mod_RESUSPENSION`/`mod_BOTTOM_SEDIMENTS` analog → the type gets a **new home**.
- **Run-gate coverage is UNCERTAIN — the #1 risk this slice's in-loop review must resolve.** The 2
  water-coupling fluxes are used only inside `if (MODEL_BOTTOM_SEDIMENTS > 1)` (`mod_SOLVER.f90:1476–…`,
  `FLUXES_TO_WATER_COLUMN` at `:1587/1623/1654`) → mode-2 path. The 9 settling/deposition arrays run
  behind `if (SHUT_DOWN_SETTLING == 0)` (`mod_SOLVER.f90:1225`), and `SHUT_DOWN_SETTLING` is forced to
  0/1 **only inside `RESUSPENSION_OPTION == 2` + active resuspension + shear** (`:1210–1224`),
  otherwise it retains its (BSS-zero-initialized) value. ⚠️ **Empirically, the `MASS_BALANCES.out`
  SETTLING and SEDIMENT-FLUXES columns are ZERO in BOTH the mode-0 and mode-2 goldens** — so the
  mass-balance diagnostic does not demonstrate any committed setup driving non-zero settling/flux
  output. The arrays *do* feed the per-box state-variable derivatives
  (`EFFECTIVE_DEPOSITION_FRACTIONS → MASS_SETTLING`, `mod_SOLVER.f90:1300`) and settling velocities
  *are* configured in `INPUTS/PELAGIC_INPUTS.txt`, so the members may still influence the per-box
  concentration outputs even with a zero mass-balance term — but this is **not confirmed**. See
  Verification: the definitive resolution is a **perturbation test** (zero a member post-computation,
  rerun, diff), and the strip-proof + compile are the guaranteed backstop regardless.
- **Off-surface consumers.** The AQUABC library (`aquabc_II_pelagic_auxillary.f90`,
  `aquabc_II_sediment_model_1_fast.f90`) only `use GLOBAL, only:` dimension constants
  (`nstate`, `NUM_FLUXES_TO_SEDIMENTS`, `NUM_SED_VARS`) — none of the 11. `mod_PELAGIC_BOX.f90` is
  **off-surface too** despite `use GLOBAL` (see the name-collision below).

## Approach (chosen): new leaf module `WATER_SEDIMENT_COUPLING`, instance `wsc`

Create a new module `mod_WATER_SEDIMENT_COUPLING.f90` (module `WATER_SEDIMENT_COUPLING`) holding a
`wsc_state_t` derived type and one module-scoped instance **`wsc`**. Hosting in the allocator
(`AQUATIC_MODEL`) or the physics module (`PELAGIC_SOLVER`) was rejected: either forces a new edge
between modules already high in the dependency graph (`PELAGIC_SOLVER → AQUATIC_MODEL`, or the
reverse), risking a cycle. A new module that `use precision_kinds, only: DBL` and nothing else is a
pure **leaf** — every consumer `use WATER_SEDIMENT_COUPLING, only: wsc`, zero cycle surface.

Instance name **`wsc`** (not `settling`): "settling" is a case-variant substring of `SETTLING_RATES`
/ `SETTLING_VELOCITIES_OUTPUT` / `SHUT_DOWN_SETTLING`, which would muddy the `sed 's/wsc%//g'`
strip-proof (the `bsed`-not-`sed` lesson). No existing `wsc`/`wsc_state_t` symbol exists (to verify).

### New module (`SOURCE_CODE/ESTAS/mod_WATER_SEDIMENT_COUPLING.f90`)

```fortran
module WATER_SEDIMENT_COUPLING
    use precision_kinds, only: DBL
    implicit none

    ! Particle settling/deposition + water<->sediment flux coupling state,
    ! moved out of module GLOBAL (Phase 5.1). See
    ! docs/superpowers/specs/2026-07-23-water-sediment-coupling-derived-type-design.md
    type, public :: wsc_state_t
        real(kind = DBL), allocatable, dimension(:, :) :: FLUXES_TO_WATER_COLUMN
        real(kind = DBL), allocatable, dimension(:, :) :: FLUXES_OUTPUT_TO_WATER_COLUMN
        real(kind = DBL), allocatable, dimension(:, :) :: DISSOLVED_FRACTIONS
        real(kind = DBL), allocatable, dimension(:, :) :: FRACTION_OF_DEPOSITION
        real(kind = DBL), allocatable, dimension(:, :) :: SETTLING_RATES
        real(kind = DBL), allocatable, dimension(:, :) :: NOT_DEPOSITED_FLUXES
        real(kind = DBL), allocatable, dimension(:, :) :: FLUXES
        real(kind = DBL), allocatable, dimension(:, :) :: SETTLING_VELOCITIES_OUTPUT
        real(kind = DBL), allocatable, dimension(:, :) :: EFFECTIVE_DISSLOVED_FRACTIONS
        real(kind = DBL), allocatable, dimension(:, :) :: EFFECTIVE_DEPOSITION_FRACTIONS
        real(kind = DBL), allocatable, dimension(:, :) :: DEPOSITION_AREA_RATIOS
    end type wsc_state_t

    type(wsc_state_t), public :: wsc
end module WATER_SEDIMENT_COUPLING
```

No `contains`, no procedures — a pure data module. `make_lib.sh` globs source files and multi-pass
compiles module-defining files first, so the new leaf is picked up with no build-script edit.

### Edits per file

1. **`mod_GLOBAL.f90`** — delete the 11 declarations (lines **142–156**, both comment sub-blocks) and
   leave a breadcrumb pointing to `wsc_state_t`/`wsc`. GLOBAL allocatable count drops **23 → 12**.
   ⚠️ Do not touch the neighboring `*_FILENAME` scalars (158–165) or the resuspension breadcrumb
   below them.
2. **`mod_WATER_SEDIMENT_COUPLING.f90`** — new file, as above.
3. **`mod_AQUATIC_MODEL.f90`** (module `AQUATIC_MODEL`) — add `use WATER_SEDIMENT_COUPLING, only: wsc`;
   the allocation block `:220–238` becomes `allocate(wsc%…)`; rewrite the 14 bare refs to `wsc%…`.
4. **`mod_SOLVER.f90`** (module `PELAGIC_SOLVER`) — add `use WATER_SEDIMENT_COUPLING, only: wsc`;
   rewrite ~68 bare refs to `wsc%…`. ⚠️ **Skip the one component access**
   `PELAGIC_BOXES(i) % DISSOLVED_FRACTIONS` (see name-collision) — it is the `PELAGIC_BOX` type
   component, not the GLOBAL var.
5. **`ESTAS_II.f90`** — add `use WATER_SEDIMENT_COUPLING, only: wsc`; the dealloc block `:83–97`
   becomes `deallocate(wsc%…)`; rewrite the 11 refs.
6. **`mod_SIMULATE.f90`** (module `SIMULATE`) — add `use WATER_SEDIMENT_COUPLING, only: wsc`; rewrite
   the 9 refs.
7. **`mod_BOTTOM_SEDIMENTS.f90`** (module `BOTTOM_SEDIMENTS`) — add
   `use WATER_SEDIMENT_COUPLING, only: wsc`; rewrite the 9 refs.
8. **`sub_READ_PELAGIC_INPUTS.f90`** (subroutine `READ_PELAGIC_BOX_MODEL_INPUTS`) — add
   `use WATER_SEDIMENT_COUPLING, only: wsc` at the subroutine's `use` block; rewrite its 1 ref
   (`:500`, `DISSOLVED_FRACTIONS(PELAGIC_STATE_VAR_NO)` — it reaches the GLOBAL var today via the
   `PELAGIC_BOX_MODEL` re-export chain).

Rename surface = **6 files** (items 3–8). `mod_PELAGIC_BOX.f90` is **not** edited (off-surface).

### Rename discipline — THREE guards (this slice needs more than word-boundary)

1. **Word-boundary `\bMEMBER\b`** — the generic token **`FLUXES`** is a substring of a dozen
   identifiers (`FLUXES_TO_SEDIMENTS`, `NOT_DEPOSITED_FLUXES`, `NUM_FLUXES_*`, `COCOA_*_FILENAME`,
   `SEDIMENT_FLUXES`, `FLUXES_TO_WATER_COLUMN`, …). All have `_` at the token edge, so `\bFLUXES\b`
   matches only the standalone array. **No bare-`FLUXES` local/dummy shadow exists** in the rename
   surface (verified), so word-boundary is airtight for it.
2. **String-literal-aware** — `FLUXES` appears **inside a string literal**:
   `'  SEDIMENT FLUXES (g/m^3/days)'` at `mod_AQUATIC_MODEL.f90:~263` (a `write(unit=1001,…)` header).
   A code-part word-boundary replace WOULD hit it → the rename tool must skip quoted content. (Here
   the site is a file-output header so the byte gate would catch corruption, but preserve it anyway;
   re-scan exhaustively during implementation.)
3. **Component-selector-aware** — `DISSOLVED_FRACTIONS` is **both** the GLOBAL array we move **and** a
   component of the `PELAGIC_BOX` derived type (`mod_PELAGIC_BOX.f90:34`
   `real(DBL), pointer :: DISSOLVED_FRACTIONS`). Component accesses `… % DISSOLVED_FRACTIONS`
   (`mod_SOLVER.f90` has 1; `mod_PELAGIC_BOX` has its own, off-surface) must **not** be prefixed. The
   tool must skip a member occurrence immediately preceded by `%` (optional whitespace). A wrong
   prefix `PELAGIC_BOXES(i) % wsc%DISSOLVED_FRACTIONS` is invalid Fortran → the compile gate backstops
   it, but strip-and-compare would give a **false clean** (stripping `wsc%` restores the original), so
   the guard is load-bearing, not belt-and-suspenders.

Shadowing pre-check (exact-name locals/dummies in the 6 files) must be run and confirmed clean, as in
prior slices.

### No circular `use`

`WATER_SEDIMENT_COUPLING` uses only `precision_kinds` — a pure leaf. Adding
`use WATER_SEDIMENT_COUPLING, only: wsc` to the 6 consumers introduces no cycle (nothing uses them
back). `make_lib.sh` multi-pass resolves the new module-first ordering by construction.

## Non-goals (YAGNI)

- **No arg-threading**, no numerics/logic change — a pure move + rename.
- **Allocation stays in `AQUATIC_MODEL`, dealloc in `ESTAS_II`** (as `wsc%…`) — moving allocation into
  the new module is extra byte-identical risk for no benefit.
- **No latent-bug fixes / dead-code removal.**
- **Do not edit `mod_PELAGIC_BOX.f90`** (its `DISSOLVED_FRACTIONS` is the type component, not the
  moved GLOBAL var).

## Verification

Gate = byte-identical model output. **Split coverage → two run gates required.**

1. **Shadowing pre-check** — grep the 6 files for member names in `::`/`intent(` context; confirm no
   local/dummy equals a member. Airtight strip-proof depends on it.
2. **Determinism pre-check** — run each gate setup twice with the pre-change binary; self-diff = 0.
3. **Build clean** (`make clean-all && make build-estas`) — new module compiles; stale bare ref →
   undeclared under `implicit none`; wrong `% wsc%` component prefix → hard error.
4. **Strip-and-compare pure-prefix proof** — per edited file,
   `diff <(git show PRE:FILE) <(sed 's/wsc%//g' FILE)` shows only structural changes (the new `use`
   line, GLOBAL deletion, wraps). ⚠️ This proof is **blind to component over-prefixing** (strip
   restores `% DISSOLVED_FRACTIONS`) — guard #3 + compile are what catch that. Also normalize-diff the
   moved decls vs the new type for kind/rank fidelity.
5. **Byte-identical run gate — BOTH setups (mandatory, regardless of coverage):**
   - **mode-0 Standard `INPUT.txt`** (`OUTPUTS/`) and **mode-2 `INPUT_sediment_test.txt`**
     (`OUTPUTS_gf_debug/`) must BOTH be bit-identical after the move. A pure rename cannot change any
     output, so this is non-negotiable acceptance regardless of which members are "active."
   - Binary is `./ESTAS_II` (input = arg 1; no `ESTAS_HOLD_VOLUME` — Standard topology).
5b. **Coverage confirmation via PERTURBATION TEST (resolves the §slice uncertainty).** Because the
   `MASS_BALANCES` settling/flux columns are zero, a passing run-gate diff might reflect members that
   never influence output. To know which of the 11 the run gate actually exercises: with the
   pre-change binary, insert a temporary `MEMBER = MEMBER * 2.0D0` (or `= 0.0D0`) immediately after the
   member is populated and before it is consumed, rebuild, rerun both setups, and diff vs the golden.
   A member whose perturbation **changes** an output file is run-gate-covered; one whose perturbation
   leaves all outputs identical is **strip-proof + compile only** (the sediment-slice COCOA treatment —
   fully defensible, since strip+compile catch swaps/drops/misspellings). Do this at least for
   `SETTLING_RATES` (representative of the 9) and `FLUXES_TO_WATER_COLUMN` (the mode-2 pair); record
   the per-member coverage verdict. Revert the perturbation before proceeding.
6. **132-col wrap** — the `wsc%` prefix (+4) can overflow; wrap at commas/operators.
7. **Build-health** — `make test-fortran` green (links none of the 6 files → no moved-subsystem
   coverage; the run gates are the real test).
8. **CI matrix** (gfortran ubuntu/macOS + ifx oneAPI) green — guards the new-module dependency edge.

## Rollout

Single PR on `refactor/water-sediment-coupling-derived-type`. Green CI + **both** byte-identical gates
+ the strip-and-compare proof, then merge on the user's go-ahead. Record both gate results and the
strip-proof in the PR body. GLOBAL allocatable count 23 → 12.
