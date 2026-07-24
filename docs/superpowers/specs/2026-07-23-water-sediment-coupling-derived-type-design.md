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

| Member | Sub-block | Exercising code-path (run-gate coverage resolved in §Run-gate coverage) |
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
  *are* configured in `INPUTS/PELAGIC_INPUTS.txt`. **The in-loop review's perturbation test resolved
  this definitively — see §Run-gate coverage: 6 of 11 are run-gate-covered, 5 are strip+compile only,
  and Standard-run settling is confirmed active.** The bigger discovery was that 6 members are
  dummy-argument-shadowed in the solver, changing the rename method (§Method).
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

### Method — enumerated genuine touch points, NOT a blanket regex rename

⚠️ **The in-loop review (below) changed the method.** 6 of the 11 members are already **arg-threaded**:
they are redeclared as `intent(inout)` dummy arguments in live solver routines, so inside those
routines they are locals, not the GLOBAL var. A blanket word-boundary `sed` with scope carve-outs is a
scope-aware Fortran parser under a byte-identical constraint — fragile, and its worst failure (a stray
`wsc%` on a dummy *body* use) is **invisible to both the run gate and the strip-proof** (the dummy is
argument-associated to the very GLOBAL being moved → same memory → byte-identical). **So this slice is
done by an explicit enumerated `file:line old→new` list applied one edit at a time.** That list — plus
a scope-aware per-site review — **is** the verification here; the run gate and strip-proof cannot be.
The plan derives the list from value-flow, not grep counts, per member as:
`genuine = all occurrences − shadow-scope bodies − component accesses − strings/comments`.

**The 4 hazard classes to exclude from every member's genuine set:**

1. **Dummy-argument shadow (scope-aware) — the load-bearing one.** These occurrences stay bare:
   - `mod_SOLVER.f90` **`SOLVE`** (~50–456) redeclares `SETTLING_VELOCITIES_OUTPUT`,
     `EFFECTIVE_DISSLOVED_FRACTIONS`, `EFFECTIVE_DEPOSITION_FRACTIONS`, `DEPOSITION_AREA_RATIOS` as
     dummies; `mod_SOLVER.f90` **`CALC_DERIV`** (~720–1658, called 3×/timestep from `SOLVE`) redeclares
     the latter three. **Every** use of these 4 members inside `mod_SOLVER` is a dummy → **skip the
     whole file for those 4**; the true GLOBAL actuals are passed at the `SOLVE` call site in
     `mod_SIMULATE` (rename there).
   - `mod_BOTTOM_SEDIMENTS.f90` **`SEDIMENT_TRANSPORT`** (~332–358, **dead code, no callers**)
     redeclares `DISSOLVED_FRACTIONS`, `FRACTION_OF_DEPOSITION` as dummies → skip those in that scope.
2. **Component-selector (continuation-aware).** `DISSOLVED_FRACTIONS` is also a `PELAGIC_BOX` component
   (`mod_PELAGIC_BOX.f90:34`). The **3** accesses `… % DISSOLVED_FRACTIONS` in `mod_SOLVER` (1229,
   1233, 1241) must stay bare — and **2 of the 3 have the `%` on the *previous* continuation line**
   (`… PELAGIC_BOXES(i) % &`), so a same-line `%` test misses them. No other of the 11 is a component.
3. **Word-boundary (`FLUXES`).** The generic token `FLUXES` is a substring of a dozen identifiers
   (`FLUXES_TO_SEDIMENTS`, `NOT_DEPOSITED_FLUXES`, `NUM_FLUXES_*`, `COCOA_*_FILENAME`,
   `SEDIMENT_FLUXES`, …) — all `_`-bounded, so match only the standalone token.
4. **String + comment.** `FLUXES` is inside the literal `'  SEDIMENT FLUXES (g/m^3/days)'`
   (`mod_AQUATIC_MODEL.f90:263`, an output-file header) — skip it. Bare member names also appear in
   comments (`mod_SOLVER.f90:1593/1605/1614`, 692/1315) — skip those too (harmless but keeps the diff
   honest).

### Edits per file — **rename surface = 5 files**

1. **`mod_GLOBAL.f90`** — delete the 11 declarations (lines **142–156**, both comment sub-blocks); add
   a breadcrumb pointing to `wsc_state_t`/`wsc`. GLOBAL allocatable count drops **23 → 12**. ⚠️ Do not
   touch the neighboring `*_FILENAME` scalars (158–165) or the resuspension breadcrumb below.
2. **`mod_WATER_SEDIMENT_COUPLING.f90`** — new leaf module (as above).
3. **`mod_AQUATIC_MODEL.f90`** (`AQUATIC_MODEL`) — `use WATER_SEDIMENT_COUPLING, only: wsc`; the
   allocation block `:220–238` → `allocate(wsc%…)`; the zero-init at ~`:660–661` → `wsc%…`. No dummy
   shadows here (allocation is in `READ_AQUATIC_MODEL_INPUTS`). **Skip** the string literal at `:263`.
4. **`ESTAS_II.f90`** — `use WATER_SEDIMENT_COUPLING, only: wsc`; the dealloc block `:83–97` →
   `deallocate(wsc%…)` (all 11). No shadows.
5. **`mod_SIMULATE.f90`** (`SIMULATE`) — `use WATER_SEDIMENT_COUPLING, only: wsc`; rename the ~9 genuine
   refs (the `SOLVE` call-site actuals ~`:323–326` — this is where the 4 shadowed members' GLOBAL
   values flow in — plus reads ~587/588/612/613/737). No shadows here.
6. **`mod_SOLVER.f90`** (`PELAGIC_SOLVER`) — `use WATER_SEDIMENT_COUPLING, only: wsc`; rename **only
   the ~17 genuine GLOBAL refs of the 7 NON-shadowed members** inside `CALC_DERIV` (the coupling/
   settling arrays used but not in the dummy list — e.g. `FLUXES_TO_WATER_COLUMN`, `DISSOLVED_FRACTIONS`
   at 1537/1544, `SETTLING_RATES`, `NOT_DEPOSITED_FLUXES`, `FRACTION_OF_DEPOSITION`). **Skip** all uses
   of the 4 dummy-shadowed members (class 1) and the 3 component accesses (class 2).
7. **`mod_BOTTOM_SEDIMENTS.f90`** (`BOTTOM_SEDIMENTS`) — `use WATER_SEDIMENT_COUPLING, only: wsc`;
   rename the **3** genuine refs (`:351`, and the RHS at `:354`/`:357`). **Skip** the
   `SEDIMENT_TRANSPORT` dummy args and their LHS uses (class 1, dead code).

`mod_PELAGIC_BOX.f90` and **`sub_READ_PELAGIC_INPUTS.f90` are NOT edited** — both reference only the
`PELAGIC_BOX` component `DISSOLVED_FRACTIONS` (the latter's `:500` has `%` on the `:499` continuation),
never the GLOBAL array.

### No circular `use`

`WATER_SEDIMENT_COUPLING` uses only `precision_kinds` — a pure leaf. Adding
`use WATER_SEDIMENT_COUPLING, only: wsc` to the 5 consumers introduces no cycle. `make_lib.sh` globs
sources and multi-pass compiles module-defining files first, so the new leaf needs no build-script edit.

## Run-gate coverage (RESOLVED by perturbation test — see In-loop review)

**6 of 11 run-gate-covered, 5 strip-proof + compile only.** Settling *is* active in the Standard run
(the `INPUT.txt` RESUSPENSION block is absent → `RESUSPENSION_OPTION` coerced to 0 at runtime →
`SHUT_DOWN_SETTLING` stays 0 → the pelagic settling path runs, velocities 0.15 m/day).

| Member | Coverage |
|---|---|
| `EFFECTIVE_DISSLOVED_FRACTIONS` | run-gate (mode-0, **measured**) |
| `FLUXES_TO_WATER_COLUMN` | run-gate (mode-2, **measured**) |
| `FLUXES_OUTPUT_TO_WATER_COLUMN` | run-gate (mode-2, unit 1023) |
| `DISSOLVED_FRACTIONS` (GLOBAL) | run-gate (mode-2, **measured**) |
| `EFFECTIVE_DEPOSITION_FRACTIONS` | run-gate (**inferred**) |
| `FRACTION_OF_DEPOSITION` (GLOBAL) | run-gate (**inferred**) |
| `SETTLING_RATES` | strip + compile only (**measured** null) |
| `NOT_DEPOSITED_FLUXES` | strip + compile only (output-arg, no consumer) |
| `SETTLING_VELOCITIES_OUTPUT` | strip + compile only (only in dead `SEDIMENT_TRANSPORT`) |
| `FLUXES` | strip + compile only — **zero code refs (alloc/dealloc only): dead array** |
| `DEPOSITION_AREA_RATIOS` | strip + compile only (only in never-reached sub-branches) |

## Non-goals (YAGNI)

- **No numerics/logic change** — a pure move + rename. Preserve the existing arg-threading exactly
  (the 6 shadowed members keep flowing GLOBAL→`SOLVE`→`CALC_DERIV`; we only rename the GLOBAL end).
- **Allocation stays in `AQUATIC_MODEL`, dealloc in `ESTAS_II`** (as `wsc%…`).
- **No latent-bug/dead-code removal** — note `FLUXES` is dead and `SEDIMENT_TRANSPORT` is uncalled;
  leave both (latent cleanup candidates for a separate change).
- **Do not edit `mod_PELAGIC_BOX.f90` or `sub_READ_PELAGIC_INPUTS.f90`** (component accesses only).

## Verification

Gate = byte-identical model output on **both** setups — but for this slice the enumerated-site list +
per-site review are the primary correctness mechanism (the gates are blind to dummy-body mis-renames).

1. **Shadowing pre-check — EXPECTED TO FIRE on 6 members** (`SETTLING_VELOCITIES_OUTPUT`,
   `EFFECTIVE_DISSLOVED_FRACTIONS`, `EFFECTIVE_DEPOSITION_FRACTIONS`, `DEPOSITION_AREA_RATIOS`,
   `DISSOLVED_FRACTIONS`, `FRACTION_OF_DEPOSITION`). This is not a blocker — it identifies exactly which
   scopes class-1 must exclude. Grep `::`/`intent(` AND continuation lines (a `::` decl can carry the
   name on the next line — a single-line grep gives a false clean).
2. **Determinism pre-check** — run each gate setup twice with the pre-change binary; self-diff = 0.
3. **Build clean** (`make clean-all && make build-estas`) — catches a stale bare ref (undeclared under
   `implicit none`) and a prefixed *declaration* (`intent(inout) … :: wsc%X` is illegal). It does
   **NOT** catch a prefixed dummy *body* use (compiles + aliases correctly) — only per-site review does.
4. **Per-site review (load-bearing).** Every edit in the enumerated list is reviewed against the 4
   hazard classes: is this occurrence a genuine GLOBAL ref, or a dummy/component/string/comment? This
   replaces the strip-proof as the primary no-mis-rename check, because a dummy-body over-prefix strips
   clean AND runs byte-identical.
5. **Strip-and-compare (secondary)** — per edited file,
   `diff <(git show PRE:FILE) <(sed 's/wsc%//g' FILE)` shows only structural changes. Useful for
   catching swaps among the 5 *non-shadowed* files' members, but **blind** to class-1 and class-2
   over-prefixes (both strip clean) — hence step 4.
6. **Byte-identical run gate — BOTH setups (mandatory acceptance):** mode-0 `INPUT.txt` (`OUTPUTS/`,
   covers the settling group) and mode-2 `INPUT_sediment_test.txt` (`OUTPUTS_gf_debug/`, covers the
   coupling fluxes) must BOTH be bit-identical. Binary `./ESTAS_II` (input = arg 1; no
   `ESTAS_HOLD_VOLUME`). A pure rename cannot change output — non-negotiable regardless of coverage.
7. **132-col wrap** — the `wsc%` prefix (+4) can overflow; wrap at commas/operators.
8. **Build-health** — `make test-fortran` green (links none of the 5 files → no moved-subsystem
   coverage; not "the real test" for this slice — steps 4 & 6 are).
9. **CI matrix** (gfortran ubuntu/macOS + ifx oneAPI) green — guards the new-module dependency edge.

## In-loop review hardening (2026-07-24)

Four independent adversarial reviewers re-derived every claim against source; one ran a live
perturbation test. All four returned **NEEDS-CHANGES**, converging on one defect the original spec
missed entirely, now fixed above:

- **Dummy-argument shadows (all 4 reviewers).** 6 members are `intent(inout)` dummies in `SOLVE`/
  `CALC_DERIV` (live) and `SEDIMENT_TRANSPORT` (dead). A blanket rename over-prefixes their body uses;
  because the dummies are argument-associated to the moved GLOBALs, the result compiles AND is
  byte-identical → invisible to run gate and strip-proof. → method changed to enumerated touch points
  + per-site review (class-1 guard); `mod_SOLVER` genuine renames corrected ~68 → ~17,
  `mod_BOTTOM_SEDIMENTS` 9 → 3.
- **`sub_READ_PELAGIC_INPUTS` is off-surface** (its `DISSOLVED_FRACTIONS` is a component access, `%` on
  the prior continuation line) → surface corrected 6 → **5 files**; original item-8 rationale was a
  misread.
- **Component accesses = 3, continuation-split** (not 1 same-line) → guard #2 made continuation-aware.
- **Coverage resolved by perturbation test** (P1 EFFECTIVE_DISSLOVED_FRACTIONS mode-0 → differs; P2
  FLUXES_TO_WATER_COLUMN mode-2 → 23 files differ; P3 SETTLING_RATES → identical; P4 DISSOLVED_FRACTIONS
  mode-2 → 10 files differ): **6 covered / 5 strip-only**; `SETTLING_RATES` empirically null; `FLUXES`
  has zero code refs. Standard-run settling confirmed active (RESUSPENSION_OPTION→0 at runtime).
- Guard #4/string extended to comments. Confirmed sound: leaf-module cycle-safety, `wsc`/`wsc_state_t`
  no collision, build-script pickup, type/instance visibility, `mod_PELAGIC_BOX`/AQUABC off-surface,
  member-list & kind/rank fidelity.

## Rollout

Single PR on `refactor/water-sediment-coupling-derived-type`. Green CI + **both** byte-identical gates
+ the per-site review + strip-and-compare, then merge on the user's go-ahead. Record both gate results
and the enumerated edit list in the PR body. GLOBAL allocatable count 23 → 12.
