# Pelagic-Core De-globalization — Tier 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the 5 peripheral pelagic-core allocatables (`node_active`, `CHLA`, `SAVED_OUTPUTS`, `SURFACE_BOXES`, dead `WATER_COLUMN_OUTPUT`) out of loose `GLOBAL` scope into a new `pelagic_core_t` instance `pcore` (defined in `mod_GLOBAL`), byte-identical. Reduces `GLOBAL`'s loose-allocatable count 12 → 7.

**Architecture:** A pure `X` → `pcore%X` rename of the 17 GLOBAL-resolving usage sites + 5 declaration moves, leaving every same-named shadow (dummy args, locals, `PELAGIC_BOX` components, comments) untouched. Correctness is enforced primarily by the **compiler backstop** (`implicit none` + the loose symbol deleted → any under-rename fails to compile), then a `-Wunused` diff (over-rename net), then a full byte-identity gate. This is one atomic commit (intermediate states don't build).

**Tech Stack:** Fortran (gfortran release + OpenMP), `make build-estas`, the `ESTAS_II` executable.

## Global Constraints

- **Byte-identical:** every output file of Standard + CL29 + the `MODEL_SEDIMENTS=2` gate must be identical before/after.
- Case-insensitive reasoning (Fortran): `pcore%CHLA` etc. — but the source keeps the existing casing at each site.
- `INPUTS/FLOW_TS.txt` stays out of every commit (explicit pathspec).
- Design authority: `docs/superpowers/specs/2026-08-01-pelagic-core-deglobalization-design.md` (hardened by 4-way review). All file:line refs below are under `SOURCE_CODE/` (ignore the `ali_version/` twin).

---

## File Structure

- Modify `SOURCE_CODE/ESTAS/mod_GLOBAL.f90` — define `pelagic_core_t` + `pcore`; move 5 declarations into the type.
- Modify `SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90` — retarget 4 `allocate` (`209,216,218,219`).
- Modify `SOURCE_CODE/ESTAS/ESTAS_II.f90` — retarget 3 `deallocate` (`74,81,83`).
- Modify `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` — 8 usage renames; skip all shadows.
- Modify `SOURCE_CODE/ESTAS/mod_SOLVER.f90` — 3 usage renames (incl. 1 mixed line); skip components.

**NOT touched** (all-skip or no Tier-1 GLOBAL ref): the 4 only-import AQUABC libs, `mod_BOTTOM_SEDIMENTS.f90` (B1 dummy — skip), `mod_PELAGIC_BOX.f90`, `mod_SIMULATE.f90`, `mod_INITIAL_CONDITIONS`, `mod_INITIALIZE_*`, `mod_PELAGIC_BOX_MODEL`, `mod_RESUSPENSION`, `mod_UTILS_01`, `mod_BASIN`, `DBL_ARRAY_2D_TO_1D`.

### The enumerated RENAME / SKIP list (load-bearing — apply exactly)

**RENAME (17 usage sites → `pcore%<name>`):**

| # | File:line | Site |
|---|---|---|
| 1 | `mod_AQUATIC_MODEL.f90:209` | `allocate(node_active(nkn))` |
| 2 | `mod_AQUATIC_MODEL.f90:216` | `allocate(SAVED_OUTPUTS(nkn,n_saved_outputs))` |
| 3 | `mod_AQUATIC_MODEL.f90:218` | `allocate(CHLA(nkn))` |
| 4 | `mod_AQUATIC_MODEL.f90:219` | `allocate(SURFACE_BOXES(nkn))` |
| 5 | `mod_AQUATIC_MODEL.f90:225` | `SURFACE_BOXES(i) = …` |
| 6 | `ESTAS_II.f90:74` | `deallocate(node_active)` |
| 7 | `ESTAS_II.f90:81` | `deallocate(SAVED_OUTPUTS)` |
| 8 | `ESTAS_II.f90:83` | `deallocate(CHLA)` |
| 9 | `mod_PELAGIC_ECOLOGY.f90:1320` | `CHLA, &` (arg, `CALCULATE_SETTLING_SUPRESSION`) |
| 10 | `mod_PELAGIC_ECOLOGY.f90:1328` | `CHLA(nd)` (neg-dump; gate-invisible but real GLOBAL) |
| 11 | `mod_PELAGIC_ECOLOGY.f90:1347` | `settling_suppres_factor_vec(CHLA, …)` |
| 12 | `mod_PELAGIC_ECOLOGY.f90:1467` | `node_active, &` (PELAGIC_KINETICS body → lib call arg) |
| 13 | `mod_PELAGIC_ECOLOGY.f90:1480` | `SAVED_OUTPUTS, &` (same call) |
| 14 | `mod_PELAGIC_ECOLOGY.f90:1488` | `SURFACE_BOXES, &` (same call) |
| 15 | `mod_SOLVER.f90:866` | `SAVED_OUTPUTS = 0.0D0` (reset block) |
| 16 | `mod_SOLVER.f90:1409` | `SAVED_OUTPUTS(i, :) = &` (LHS only) |
| 17 | `mod_SOLVER.f90:1690` | **RHS only** `= SAVED_OUTPUTS(i,:)` (LHS is a component — SKIP) |

**Declaration moves (into `pelagic_core_t`):** `mod_GLOBAL.f90` `node_active(:96)`, `SAVED_OUTPUTS(:118)`, `CHLA(:126)`, `WATER_COLUMN_OUTPUT(:128, dead — move, do not delete)`, `SURFACE_BOXES(:130)`.

**SKIP (shadows — never rename):** `CHLA` scalar local `mod_PELAGIC_ECOLOGY:318/363/380`; comments `mod_PELAGIC_ECOLOGY:1324`, `mod_SOLVER:22/391`; `SAVED_OUTPUTS` `PELAGIC_BOX` component `mod_PELAGIC_BOX:79/124/173/184`, `mod_SIMULATE:545`, `mod_PELAGIC_ECOLOGY:50/55/76/160/164/271/280`, `mod_SOLVER:1255/1410/1690-LHS`; **B1** `mod_BOTTOM_SEDIMENTS:335/343`; component reads via transitive re-export (not in the touch set) `sub_WRITE_PELAGIC_OUTPUT.f90:222`, `sub_WRITE_PELAGIC_BINARY_OUTPUT.f90:195`.

---

### Task 1: Capture the golden baseline

- [ ] **Step 1: Build the pre-change binary.**

Run: `make clean-all && make build-estas`
Expected: `ESTAS_II` built, exit 0.

- [ ] **Step 2: Capture the `-Wunused` warning baseline (over-rename net).** The release build already compiles with `-Wall -Wextra -fimplicit-none` (`Makefile:108`) — `-Wunused-variable` (from `-Wall`) and `-Wunused-dummy-argument` (from `-Wextra`) are already emitted, so **no extra flags are needed**. The only requirement is to capture the **merged** stream (warnings are on stderr and `make_lib.sh` interleaves them — a bare `2>file` came back EMPTY in review, making the check vacuous).

Run: `make clean-all >/dev/null 2>&1 && make build-estas >/tmp/build_before.txt 2>&1; grep -E 'Wunused' /tmp/build_before.txt | sort > /tmp/unused_before.txt; wc -l /tmp/unused_before.txt`
Expected: a **non-empty** sorted baseline. If it is empty the capture is wrong — fix it before proceeding, or the over-rename net tests nothing.

- [ ] **Step 3: Run the three gate configs and snapshot outputs.**

```bash
rm -rf OUTPUTS && mkdir OUTPUTS && ./ESTAS_II INPUT.txt >/dev/null 2>&1
cp -r OUTPUTS /tmp/gold_standard
rm -rf OUTPUTS_CL29 && mkdir OUTPUTS_CL29 && ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt >/dev/null 2>&1
cp -r OUTPUTS_CL29 /tmp/gold_cl29
# MODEL_SEDIMENTS=2 gate (covers B1's SEDIMENT_TRANSPORT); uses INPUT_sediment_test.txt
rm -rf OUTPUTS_gf_debug && mkdir -p OUTPUTS_gf_debug && ./ESTAS_II INPUT_sediment_test.txt >/dev/null 2>&1
cp -r OUTPUTS_gf_debug /tmp/gold_sed2
```
Expected: three golden output dirs saved. (If `INPUT_sediment_test.txt` / its output dir differ on this tree, resolve the exact `MODEL_SEDIMENTS=2` config before proceeding — the B1 gate is mandatory.)

- [ ] **Step 4: Confirm determinism.** Re-run Standard once more and diff vs `/tmp/gold_standard`.

Run: `rm -rf OUTPUTS && mkdir OUTPUTS && ./ESTAS_II INPUT.txt >/dev/null 2>&1 && diff -rq OUTPUTS /tmp/gold_standard`
Expected: no differences (runs are deterministic).

### Task 2: Define `pelagic_core_t` + move the 5 declarations

- [ ] **Step 1: In `mod_GLOBAL.f90`, add the type + instance** (place near the existing allocatable block, ~line 96). Move the 5 declarations verbatim (same kind/dimension) as components; delete the 5 loose declarations.

```fortran
type :: pelagic_core_t
    integer,        allocatable, dimension(:)   :: node_active
    real(kind=DBL), allocatable, dimension(:,:) :: SAVED_OUTPUTS
    real(kind=DBL), allocatable, dimension(:)   :: CHLA
    real(kind=DBL), allocatable, dimension(:,:) :: WATER_COLUMN_OUTPUT   ! dead; preserved
    integer,        allocatable, dimension(:)   :: SURFACE_BOXES
end type pelagic_core_t
type(pelagic_core_t) :: pcore
```

- [ ] **Step 2: Build — expect it to FAIL** with "no IMPLICIT type" / "not declared" at the RENAME usage sites. This is a **smoke check, not a completeness cross-check**: the multi-pass `make_lib.sh` halts at the first failing file, so it surfaces only *that* file's errors — do NOT expect all 17 sites at once. The completeness guarantee is Task 5's exit-0 clean build. What to check here: any error pointing at a **SKIP** site means that name genuinely resolved to GLOBAL there → re-classify.

Run: `make build-estas 2>&1 | grep -iE "Error|no IMPLICIT|not been declared" | head -40`
Expected: errors only at RENAME (not SKIP) sites among the files compiled before the build halts.

### Task 3: Retarget alloc/dealloc (preserve asymmetry)

- [ ] **Step 1:** In `mod_AQUATIC_MODEL.f90`, rename the 4 allocates (`209,216,218,219`) to `allocate(pcore%…)`.
- [ ] **Step 2:** In `ESTAS_II.f90`, rename the 3 deallocates (`74,81,83`) to `deallocate(pcore%…)`. **Do NOT add** `deallocate(pcore%SURFACE_BOXES)` or any `WATER_COLUMN_OUTPUT` alloc/dealloc — the asymmetry is intentional.

### Task 4: Apply the 17 usage renames

- [ ] **Step 1:** Edit each RENAME site in the table to `pcore%<name>`, **per occurrence**. For the two mixed statements:
  - `mod_SOLVER.f90:1409-1410`: rename the LHS `SAVED_OUTPUTS(i, :)` (line 1409) → `pcore%SAVED_OUTPUTS(i, :)`; leave the RHS component (`… % SAVED_OUTPUTS`, line 1410) unchanged.
  - `mod_SOLVER.f90:1689-1690`: leave the LHS component (`…PELAGIC_BOXES(i) % SAVED_OUTPUTS`, spanning 1689-1690) unchanged; rename **only** the RHS `SAVED_OUTPUTS(i,:)` → `pcore%SAVED_OUTPUTS(i,:)`.
- [ ] **Step 2:** Re-read each edited full statement (including `&` continuations) to confirm no shadow was touched and no continuation was mis-split.

### Task 5: Build clean (the compiler gate)

- [ ] **Step 1:** `make clean-all && make build-estas` → **exit 0**. Any remaining "no IMPLICIT type" = a missed RENAME; any new "not a component of pcore" = an over-rename of a shadow. Fix and rebuild until clean.
- [ ] **Step 2:** `make clean-all && make OPENMP=1 build-estas` → **exit 0** (OpenMP build must also compile).

### Task 6: Over-rename net (`-Wunused` diff)

- [ ] **Step 1:** Rebuild and capture the same merged stream: `make clean-all >/dev/null 2>&1 && make build-estas >/tmp/build_after.txt 2>&1; grep -E 'Wunused' /tmp/build_after.txt | sort > /tmp/unused_after.txt`.
- [ ] **Step 2:** `diff /tmp/unused_before.txt /tmp/unused_after.txt`.
Expected: **empty**. Any newly-unused local/dummy means a shadow was over-renamed (its local is now dead) — investigate and fix before proceeding.

### Task 7: Byte-identity gate + strip-and-compare

- [ ] **Step 1:** Re-run all three configs into fresh dirs and diff against the goldens:
```bash
rm -rf OUTPUTS && mkdir OUTPUTS && ./ESTAS_II INPUT.txt >/dev/null 2>&1 && diff -rq OUTPUTS /tmp/gold_standard
rm -rf OUTPUTS_CL29 && mkdir OUTPUTS_CL29 && ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt >/dev/null 2>&1 && diff -rq OUTPUTS_CL29 /tmp/gold_cl29
rm -rf OUTPUTS_gf_debug && mkdir -p OUTPUTS_gf_debug && ./ESTAS_II INPUT_sediment_test.txt >/dev/null 2>&1 && diff -rq OUTPUTS_gf_debug /tmp/gold_sed2
```
Expected: **no differences** in any of the three.

- [ ] **Step 2: Strip-and-compare** (no-stray-edits proof): `git diff` the source, strip `pcore%` back to bare, and confirm the result is empty (only `pcore%` prefixes + the declaration move were added). Note: this proves no stray edits, NOT correctness (correctness = Steps 5/6/7-Step-1).

Run: `git diff -- SOURCE_CODE/ | grep '^[+-]' | grep -v '^[+-][+-]' | sed 's/pcore%//g' | ...` (confirm added/removed lines match modulo the `pcore%` prefix + the mod_GLOBAL type block).

### Task 8: Commit, push, PR

- [ ] **Step 1: Commit** (explicit pathspec — NOT `-am`, keep `FLOW_TS` out):
```bash
git add -- SOURCE_CODE/ESTAS/mod_GLOBAL.f90 SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90 \
  SOURCE_CODE/ESTAS/ESTAS_II.f90 SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90 SOURCE_CODE/ESTAS/mod_SOLVER.f90
git commit -m "refactor(pcore): bundle 5 peripheral pelagic-core GLOBAL arrays into pelagic_core_t (Tier 1)"
```
- [ ] **Step 2:** Push `refactor/pelagic-core-tier1`, open a PR to `main`, watch the CI matrix (gfortran macOS/ubuntu, ifx, integration-tests, python-lint) green, merge.

---

## Self-Review

- **Spec coverage:** every spec method step (case-insensitive enumeration, per-occurrence classification, compiler backstop, `-Wunused` net, 3-config gate incl. `MODEL_SEDIMENTS=2` for B1, strip-and-compare, OpenMP build, alloc/dealloc asymmetry) maps to a task. ✓
- **Placeholder scan:** the list enumerates every GLOBAL-resolving RENAME site + every in-touch-set shadow with file:line, plus the two out-of-touch-set `sub_WRITE_PELAGIC_*` component reads (SKIP, for audit completeness). Commands are concrete. The one conditional is `INPUT_sediment_test.txt` — flagged as "resolve exact MODEL_SEDIMENTS=2 config if it differs" rather than left vague. ✓
- **Workflow-review fixes applied (2026-08-01):** the `-Wunused` capture stream bug (was vacuous) fixed to merged-stream `>file 2>&1` with the release build's existing `-Wall -Wextra`; `FFLAGS_EXTRA` (not a real hook) removed; Task-2-Step-2 reworded as a smoke check (make halts at first failing file); the two `sub_WRITE_PELAGIC_*` component sites added to SKIP. The load-bearing rename/skip classification was independently re-derived by 3 array finders and confirmed correct. ✓
- **Type consistency:** the `pelagic_core_t` component kinds/dims match the `mod_GLOBAL.f90:96-130` originals (int(:), real(:,:), real(:), real(:,:), int(:)). ✓
- **Atomicity:** one commit (intermediate states don't build) — the plan builds the failing state deliberately (Task 2 Step 2) as the backstop, not as a shippable checkpoint. ✓
