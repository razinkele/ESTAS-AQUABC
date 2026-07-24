# Water↔Sediment Coupling State → Derived Type Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the 11-member settling/deposition + water↔sediment coupling block out of module `GLOBAL` into a new leaf module `WATER_SEDIMENT_COUPLING` (`wsc_state_t` / instance `wsc`), byte-identically.

**Architecture:** A pure move + rename, but **NOT a mechanical one.** 6 of the 11 members are `intent(inout)` dummy-argument shadows in the solver (`SOLVE`/`CALC_DERIV`) and dead `SEDIMENT_TRANSPORT`; a blanket regex would over-prefix their body uses in a way that both compiles and stays byte-identical (invisible to the run gate AND strip-proof). So the change is applied as an **explicit, pre-enumerated list of exactly 53 renames** (`docs/superpowers/plans/2026-07-24-wsc-edit-list.md`), one edit at a time, with **per-site review as the load-bearing verification**.

**Tech Stack:** Fortran (free-form, gfortran/ifx), `make_lib.sh` multi-pass build, `make build-estas`.

**Design spec:** `docs/superpowers/specs/2026-07-23-water-sediment-coupling-derived-type-design.md`.
**Enumerated edit list (authoritative):** `docs/superpowers/plans/2026-07-24-wsc-edit-list.md` — 53 RENAME / 56 SKIP, per-file per-line.

## Global Constraints

- **Byte-identical output is acceptance** on BOTH gate setups: max |Δ| = 0. No numerics change.
- **Rename ONLY the 53 enumerated genuine GLOBAL refs.** The 56 SKIP occurrences (42 dummy-body, 3 component-access, 4 comment, 1 string, + dead-code dummies) stay bare. **Apply by exact-string match** on the edit list's "current code" column, not by line number (inserting the `use` line shifts all lines below it).
- **The load-bearing check is per-site review, not the automated gates.** A wrong `wsc%` on a dummy body use compiles AND runs byte-identical (the dummy is arg-associated to the moved GLOBAL) → both the run gate and strip-proof are blind. Only reviewing each edit against the 4 hazard classes catches it.
- **The 11 members** (all `real(DBL)(:,:)`): FLUXES_TO_WATER_COLUMN, FLUXES_OUTPUT_TO_WATER_COLUMN, DISSOLVED_FRACTIONS, FRACTION_OF_DEPOSITION, SETTLING_RATES, NOT_DEPOSITED_FLUXES, FLUXES, SETTLING_VELOCITIES_OUTPUT, EFFECTIVE_DISSLOVED_FRACTIONS, EFFECTIVE_DEPOSITION_FRACTIONS, DEPOSITION_AREA_RATIOS.
- **Rename surface = 5 files:** mod_AQUATIC_MODEL (13), mod_SOLVER (17), ESTAS_II (11), mod_SIMULATE (9), mod_BOTTOM_SEDIMENTS (3). `mod_PELAGIC_BOX.f90` and `sub_READ_PELAGIC_INPUTS.f90` are OFF-surface — do not touch.
- **Scope-mixed lines** (LHS and RHS treated oppositely): mod_SOLVER 1537/1538 (LHS rename, RHS dummy-skip); mod_BOTTOM_SEDIMENTS 354/357 (LHS dummy-skip, RHS rename); mod_SOLVER 1623 (both rename).
- **Two gate setups (split coverage):** mode-0 Standard `INPUT.txt` (`OUTPUTS/`) covers the settling group; mode-2 `INPUT_sediment_test.txt` (`OUTPUTS_gf_debug/`) covers the coupling fluxes. Binary `./ESTAS_II` (input = arg 1; NO `ESTAS_HOLD_VOLUME`).
- **Preserve, don't fix:** `FLUXES` is a dead array (alloc/dealloc only) and `SEDIMENT_TRANSPORT` is uncalled — leave both.
- **No component-`private`** line in the new type. Build serial (not `OPENMP=1`) for the gate.
- Source line numbers are as-of-current-`main` (`d76b148`); the edit list keys off pristine files.

---

### Task 1: Capture both deterministic golden baselines

Two goldens are needed (split coverage). Both must be deterministic before a 0-diff is meaningful.

**Files:** Modify: none. Artifacts (scratchpad): `/tmp/wsc_golden_std`, `/tmp/wsc_golden_sed2`, `/tmp/WSC_PRE_SHA`.

**Interfaces:** Produces `PRE` (git SHA of pre-code-change tip), and the two golden trees consumed by Tasks 3–4.

- [ ] **Step 1: Record the pre-change reference SHA** (all 5 source files pristine on this branch — only design/plan docs committed so far)

```bash
cd /home/razinka/AQUABCv0.2
git rev-parse HEAD | tee /tmp/WSC_PRE_SHA
```

- [ ] **Step 2: Build the pre-change binary (serial)**

```bash
make clean-all && make build-estas
```
Expected: clean build, `ESTAS_II` produced.

- [ ] **Step 3: Determinism self-diff, both setups**

```bash
# mode-0
./ESTAS_II INPUT.txt          > /tmp/wsc_std1.log 2>&1; cp -r OUTPUTS /tmp/wsc_golden_std
./ESTAS_II INPUT.txt          > /tmp/wsc_std2.log 2>&1; diff -r OUTPUTS /tmp/wsc_golden_std && echo "STD DETERMINISTIC" || echo "STD NON-DET — STOP"
# mode-2  (confirm its output folder first)
grep -in output INPUT_sediment_test.txt
./ESTAS_II INPUT_sediment_test.txt > /tmp/wsc_sed1.log 2>&1; cp -r OUTPUTS_gf_debug /tmp/wsc_golden_sed2
./ESTAS_II INPUT_sediment_test.txt > /tmp/wsc_sed2.log 2>&1; diff -r OUTPUTS_gf_debug /tmp/wsc_golden_sed2 && echo "SED2 DETERMINISTIC" || echo "SED2 NON-DET — STOP"
```
Expected: `STD DETERMINISTIC` and `SED2 DETERMINISTIC`. If either is NON-DET, STOP (BLOCKED). (Mode-0 prints ~121k pre-existing "NEGATIVE MASS" lines to stdout — benign, does not affect the `OUTPUTS/` diff.)

- [ ] **Step 4: No commit** (baselines are throwaway artifacts). Confirm `/tmp/wsc_golden_std`, `/tmp/wsc_golden_sed2`, `/tmp/WSC_PRE_SHA` exist.

---

### Task 2: Create the new leaf module (unused checkpoint)

Isolates "new module compiles + no symbol collision + byte-identical" before the surgical edit. At the end `wsc` exists but is referenced by nobody → build clean, output unchanged.

**Files:** Create: `SOURCE_CODE/ESTAS/mod_WATER_SEDIMENT_COUPLING.f90`.

**Interfaces:** Produces module-public `type(wsc_state_t) :: wsc` in module `WATER_SEDIMENT_COUPLING`.

- [ ] **Step 1: Create the file** with exactly:

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

- [ ] **Step 2: Build**

```bash
make build-estas
```
Expected: clean build (`wsc` unused is fine).

- [ ] **Step 3: Byte-identity sanity (nothing references wsc)**

```bash
./ESTAS_II INPUT_sediment_test.txt && diff -r OUTPUTS_gf_debug /tmp/wsc_golden_sed2 && echo "STILL IDENTICAL"
```
Expected: `STILL IDENTICAL`.

- [ ] **Step 4: Commit**

```bash
git add SOURCE_CODE/ESTAS/mod_WATER_SEDIMENT_COUPLING.f90
git commit -m "refactor(wsc): add WATER_SEDIMENT_COUPLING leaf module + wsc instance (unused)"
```

---

### Task 3: Apply the enumerated edits (GLOBAL deletion + 53 renames + 5 `use` inserts)

The atomic surgical change. Cannot be split (partial = won't compile). **Apply every edit in `docs/superpowers/plans/2026-07-24-wsc-edit-list.md` by exact-string match, one at a time**, then build + per-site review.

**Files:** Modify: `mod_GLOBAL.f90`, `mod_AQUATIC_MODEL.f90`, `mod_SOLVER.f90`, `ESTAS_II.f90`, `mod_SIMULATE.f90`, `mod_BOTTOM_SEDIMENTS.f90`.

**Interfaces:** Consumes the `wsc` instance (Task 2) and `WSC_PRE_SHA` (Task 1). Produces a byte-identical binary with the 11 members encapsulated in `wsc`.

- [ ] **Step 1: Add the 5 `use WATER_SEDIMENT_COUPLING, only: wsc` imports** (each after that file's `use GLOBAL` line): mod_AQUATIC_MODEL (after :17), mod_SOLVER (after :3), ESTAS_II (after :3), mod_SIMULATE (after :3), mod_BOTTOM_SEDIMENTS (after :15).

- [ ] **Step 2: Delete the GLOBAL block + breadcrumb.** In `mod_GLOBAL.f90` delete lines 142–156 (the 2 comment sub-blocks + 11 decls + blanks) and insert the breadcrumb from the edit list §1. ⚠️ Leave 157 (blank) and 158–165 (`*_FILENAME`) and the `bsed` breadcrumb (135–140) untouched.

- [ ] **Step 3: Apply the 53 renames by exact-string match**, per file, from the edit list:
  - `mod_AQUATIC_MODEL.f90` — 13 (allocs 220/221/230–238, zero-inits 660/661). Skip the `'  SEDIMENT FLUXES …'` string (263).
  - `mod_SOLVER.f90` — 17, ALL in `CALC_DERIV` (edit list §4a). ⚠️ 1537/1538 rename **LHS only**; 1623 renames **both**; skip every use of the 4 SOLVE/CALC_DERIV dummies, the 3 `% DISSOLVED_FRACTIONS` component accesses (1229/1233/1241, `%` on prev line for 2), and the 4 comments.
  - `ESTAS_II.f90` — 11 (dealloc 83/84/89–97).
  - `mod_SIMULATE.f90` — 9 (SOLVE-call actuals 323–326; reads 587/588/612/613/737).
  - `mod_BOTTOM_SEDIMENTS.f90` — 3 (dead `SEDIMENT_TRANSPORT`): 351 rename **RHS**; 354/357 rename **RHS only** (LHS is the dummy — skip). Skip the arg list (333) and decls (340/341).

- [ ] **Step 4: Build clean**

```bash
make clean-all && make build-estas
```
Expected: clean build. A stale bare ref (a missed genuine rename) → undeclared under `implicit none`. A prefixed declaration (`intent(inout) … :: wsc%X`) → illegal. ⚠️ It does NOT catch a prefixed dummy *body* use — Step 6 does. Fix any 132-col overflow by wrapping at a comma/operator.

- [ ] **Step 5: Strip-and-compare (secondary — catches swaps in the non-shadowed files)**

```bash
PRE=$(cat /tmp/WSC_PRE_SHA)
for f in mod_AQUATIC_MODEL mod_SOLVER ESTAS_II mod_SIMULATE mod_BOTTOM_SEDIMENTS; do
  echo "== $f =="; diff <(git show $PRE:SOURCE_CODE/ESTAS/$f.f90) <(sed 's/wsc%//g' SOURCE_CODE/ESTAS/$f.f90)
done
diff <(git show $PRE:SOURCE_CODE/ESTAS/mod_GLOBAL.f90) SOURCE_CODE/ESTAS/mod_GLOBAL.f90
```
Expected: each file's strip-diff shows ONLY the added `use` line (+ GLOBAL: only the 11 deletions + breadcrumb). ⚠️ This is **blind** to dummy-body and component over-prefixes (they strip clean) — that is what Step 6 is for.

- [ ] **Step 6: Per-site review (LOAD-BEARING).** For each of the 53 applied renames, confirm against the edit list that it is a genuine GLOBAL ref — NOT a dummy body use, component access, string, or comment. Specifically re-verify: no `wsc%` appears anywhere inside `SOLVE`/`CALC_DERIV` for the 4 shadowed members, none inside `SEDIMENT_TRANSPORT` on a dummy (LHS of 354/357), and none on a `% DISSOLVED_FRACTIONS`:

```bash
# must return NOTHING (no dummy/component over-prefix):
grep -nE "wsc%(SETTLING_VELOCITIES_OUTPUT|EFFECTIVE_DISSLOVED_FRACTIONS|EFFECTIVE_DEPOSITION_FRACTIONS|DEPOSITION_AREA_RATIOS)" SOURCE_CODE/ESTAS/mod_SOLVER.f90
grep -nE "% *wsc%|wsc%DISSOLVED_FRACTIONS *=" SOURCE_CODE/ESTAS/mod_SOLVER.f90 SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90
# GLOBAL allocatable count now 12:
grep -c allocatable SOURCE_CODE/ESTAS/mod_GLOBAL.f90
```
Expected: first two greps empty; count = 12.

- [ ] **Step 7: Commit**

```bash
git add SOURCE_CODE/ESTAS/mod_GLOBAL.f90 SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90 \
        SOURCE_CODE/ESTAS/mod_SOLVER.f90 SOURCE_CODE/ESTAS/ESTAS_II.f90 \
        SOURCE_CODE/ESTAS/mod_SIMULATE.f90 SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90
git commit -m "refactor(wsc): move settling/water-coupling GLOBAL state into wsc (wsc_state_t)"
```

---

### Task 4: Byte-identical run gate (both setups)

**Files:** none (verification).

**Interfaces:** Consumes `/tmp/wsc_golden_std`, `/tmp/wsc_golden_sed2`.

- [ ] **Step 1: Rebuild (serial, same flags)** `make clean-all && make build-estas`.

- [ ] **Step 2: Mode-0 gate**

```bash
./ESTAS_II INPUT.txt
diff -r OUTPUTS /tmp/wsc_golden_std && echo "MODE-0 BYTE-IDENTICAL" || echo "DIFF — INVESTIGATE"
```
Expected: `MODE-0 BYTE-IDENTICAL`.

- [ ] **Step 3: Mode-2 gate**

```bash
./ESTAS_II INPUT_sediment_test.txt
diff -r OUTPUTS_gf_debug /tmp/wsc_golden_sed2 && echo "MODE-2 BYTE-IDENTICAL" || echo "DIFF — INVESTIGATE"
```
Expected: `MODE-2 BYTE-IDENTICAL`. (If either differs, a genuine rename perturbed numerics — investigate before proceeding.)

- [ ] **Step 4: No commit. Record both results for the PR.**

---

### Task 5: Build-health, CI, PR

**Files:** none.

- [ ] **Step 1: `make test-fortran`** — expected all pass (build-health; links none of the 5 files, so zero moved-subsystem coverage — the run gates + per-site review are the real test).

- [ ] **Step 2: Push** `git push -u origin refactor/water-sediment-coupling-derived-type`.

- [ ] **Step 3: Open PR** to `main`. Body: both byte-identical gate results, the strip-and-compare output, the 53-edit enumerated list (link the committed `2026-07-24-wsc-edit-list.md`), GLOBAL allocatable 23→12, and a note that per-site review is the primary correctness mechanism (gates blind to dummy over-prefix). Link the design spec.

- [ ] **Step 4: Confirm CI matrix green** (gfortran ubuntu/macOS + ifx oneAPI + integration + lint).

- [ ] **Step 5: Merge on the user's go-ahead.**

---

## Self-Review

**Spec coverage:** New leaf module → Task 2. GLOBAL deletion + 53 renames + 5 imports → Task 3 (+ edit list). The 4 hazard-class skips + scope-mixed lines → Task 3 Steps 3/6 + the edit list. Split-coverage two-gate byte-identical → Tasks 1, 4. Per-site review as load-bearing → Task 3 Step 6. Preserve FLUXES-dead / SEDIMENT_TRANSPORT → Global Constraints. `make test-fortran`/CI/PR → Task 5. All spec sections map to a task.

**Placeholder scan:** No TBD/TODO; the edit list carries every exact old→new; the new module is given verbatim.

**Type consistency:** `wsc_state_t` / `wsc` / the 11 member tokens are identical across the module def, the edit list, and the constraints. `./ESTAS_II`, `make test-fortran`, and both gate setups match the sediment slice's verified invocation.
