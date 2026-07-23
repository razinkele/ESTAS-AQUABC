# Bottom-Sediment State → Derived Type Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the 24-member "bottom sediment submodel" block out of the `GLOBAL` god-module into a `sediment_state_t` derived type with a single module-scoped instance `bsed` in module `BOTTOM_SEDIMENTS`, byte-identically.

**Architecture:** A pure move + rename. Every bare reference to a block member becomes `bsed%MEMBER`. The 24 declarations are deleted from `mod_GLOBAL.f90` and re-homed as components of `sediment_state_t` in `mod_BOTTOM_SEDIMENTS.f90` (which already allocates/reads them). No numerics or logic change — verified by bit-identical model output plus a strip-and-compare pure-prefix proof and a clean `implicit none` compile.

**Tech Stack:** Fortran (free-form, gfortran/ifx), the project's multi-pass `make_lib.sh` build, `make build-estas`.

**Design spec:** `docs/superpowers/specs/2026-07-23-sediment-state-derived-type-design.md` (read it first — it carries the full rationale, the do-not-touch look-alike list, and the in-loop review findings this plan implements).

## Global Constraints

- **Byte-identical output is the acceptance gate.** No task may change model numerics. Max |Δ| = 0 on the run gate.
- **Rename discipline: word-boundary exact-token only.** Replace `\bMEMBER\b → bsed%MEMBER` in code segments **only** — never inside string literals, and (to keep diffs clean) not inside comments. A naive substring replace corrupts the pervasive `*_LOC` locals (`SED_DEPTHS_LOC`, `PART_MIXING_COEFFS_LOC`, …), `NUM_SED_*`, `COCOA_*_FILENAME`, and `INIT_BSED_MODEL_CONSTANTS`. Word boundaries leave every one untouched.
- **The 24 members (exact tokens):** `INIT_SED_STATE_VARS`, `SED_DEPTHS`, `SED_POROSITIES`, `SED_DENSITIES`, `PART_MIXING_COEFFS`, `SED_DIFFUSIONS`, `SURF_MIXLEN`, `SED_BURRIALS`, `SURF_WATER_CONCS`, `SED_TEMPS`, `SED_MODEL_CONSTANTS`, `SED_TYPE_PER_BOX`, `PROCESSES_sed`, `SED_DRIVING_FUNCTIONS`, `FLUXES_TO_SEDIMENTS`, `ADVECTIVE_VELOCITY`, `H_ERODEP`, `SED_FLAGS`, `NUM_FLUX_RECEIVING_SED_LAYERS`, `FINAL_SED_STATE_VARS`, `FLUXES_FROM_SEDIMENTS`, `SED_OUTPUTS`, `SED_SAVED_OUTPUTS`, `SED_BURRIAL_RATE_OUTPUTS`.
- **Rename surface = exactly 5 ESTAS files:** `mod_BOTTOM_SEDIMENTS.f90`, `mod_SOLVER.f90`, `ESTAS_II.f90`, `mod_SIMULATE.f90`, `mod_AQUATIC_MODEL.f90`. The AQUABC library and `mod_SED_TYPEMAP.f90` are NOT touched (they see these names as dummy args, not GLOBAL vars).
- **Primary byte-identical gate = `INPUT_sediment_test.txt` (`MODEL_SEDIMENTS 2`)** — the only committed setup that exercises the 24 members (they live behind `MODEL_BOTTOM_SEDIMENTS > 1`). Its output folder is `OUTPUTS_gf_debug/`. Secondary regression = Standard `INPUT.txt` (`MODEL_SEDIMENTS 0`).
- **Preserve, do not "fix":** `SED_TYPE_PER_BOX` is allocated but never deallocated — do NOT add a `deallocate`. No latent-bug fixes in this slice.
- **No component-`private`** line in the type (components must be public or `bsed%MEMBER` fails to resolve in the four consumers).
- **Build serial for the gate** (`make build-estas`, NOT `OPENMP=1`), identical compiler/flags on both sides.
- All source line numbers below are as-of-current-`main`; grep to reconfirm before editing (comment-only collaborator commits can shift them by a line or two).

---

### Task 1: Capture the deterministic golden baseline

Establishes the test oracle. The whole refactor is judged bit-for-bit against these outputs. Because the mode-2 setup is a self-described "negative-mass debug" run that may abort, its determinism must be proven first, or a 0-diff later is meaningless.

**Files:**
- Modify: none (pre-change binary only)
- Artifacts (scratchpad, not committed): `golden_sed2/`, `check_sed2/`, `golden_std/`

**Interfaces:**
- Produces: `PRE` (git SHA of the pre-code-change tip), `golden_sed2/` (mode-2 golden `OUTPUTS_gf_debug/`), `golden_std/` (mode-0 golden `OUTPUTS/`). Tasks 3–4 consume these.

- [ ] **Step 1: Record the pre-change reference SHA**

All code files are still pristine on this branch (only doc commits so far). Capture the reference the strip-proof (Task 3) and run gate (Task 4) compare against:

```bash
cd /home/razinka/AQUABCv0.2
git rev-parse HEAD | tee /tmp/PRE_SHA   # e.g. de00961...
```

- [ ] **Step 2: Build the pre-change binary (serial)**

```bash
make clean-all && make build-estas
```
Expected: clean build, `estas` binary produced.

- [ ] **Step 3: Determinism self-diff on the mode-2 setup**

Run `INPUT_sediment_test.txt` twice into separate output dirs and diff. (Confirm the input's output folder first — the design says `OUTPUTS_gf_debug/`; verify with `grep -i output INPUT_sediment_test.txt` and adjust the copy target if it differs.)

```bash
grep -n "MODEL_SEDIMENTS" -A1 INPUT_sediment_test.txt   # confirm value is 2
grep -in "output" INPUT_sediment_test.txt               # confirm the output folder (expected: OUTPUTS_gf_debug)
# ESTAS_II takes the input file as arg 1 (defaults to INPUT.txt if omitted).
# Do NOT set ESTAS_HOLD_VOLUME=1 — that is a CL29-only workaround; this is a Standard-topology run.
# Run 1
./ESTAS_II INPUT_sediment_test.txt
cp -r OUTPUTS_gf_debug /tmp/golden_sed2
# Run 2
./ESTAS_II INPUT_sediment_test.txt
cp -r OUTPUTS_gf_debug /tmp/check_sed2
# Self-diff MUST be empty
diff -r /tmp/golden_sed2 /tmp/check_sed2 && echo "DETERMINISTIC" || echo "NON-DETERMINISTIC — STOP"
```
Expected: `DETERMINISTIC` (self-diff empty). If NON-DETERMINISTIC, stop — the byte-identical gate is invalid on this setup; escalate (stabilize the run or select another mode-2 fixture) before proceeding. (If the run aborts on a volume error, you *may* add `ESTAS_HOLD_VOLUME=1` — but then apply it identically to every run in this plan, baseline and post-change.)

- [ ] **Step 4: Snapshot the mode-0 regression baseline**

```bash
./ESTAS_II INPUT.txt     # MODEL_SEDIMENTS 0 (Standard setup; no ESTAS_HOLD_VOLUME)
cp -r OUTPUTS /tmp/golden_std
```
Expected: run completes; `OUTPUTS/` snapshotted.

- [ ] **Step 5: No commit (baselines are throwaway artifacts)**

Confirm `/tmp/golden_sed2`, `/tmp/golden_std`, `/tmp/PRE_SHA` exist. Nothing to commit.

---

### Task 2: Add the `sediment_state_t` type + `bsed` instance to `mod_BOTTOM_SEDIMENTS`

Isolates the type definition so a typo in it is caught by a clean compile **before** the 300-reference rename. At the end of this task `bsed` exists but is unused — the build still succeeds and output is unchanged (nothing references `bsed` yet).

**Files:**
- Modify: `SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90` (insert between `implicit none` ~:17 and `contains` ~:18)

**Interfaces:**
- Produces: module-public `type(sediment_state_t) :: bsed` in module `BOTTOM_SEDIMENTS`. Tasks 3–4 rewrite all member refs to `bsed%…`.

- [ ] **Step 1: Confirm the insertion point**

```bash
sed -n '13,20p' SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90
```
Expected: `module BOTTOM_SEDIMENTS`, `use GLOBAL`, `use UTILS_1`, `implicit none`, then `contains`. Insert the type block between `implicit none` and `contains`.

- [ ] **Step 2: Insert the type + instance**

Add exactly this block after `implicit none` (before `contains`). `DBL` is in scope via `use GLOBAL`. Component order matches the GLOBAL declaration order (keeps later strip-diffs minimal). **No `private` line.**

```fortran
    ! Bottom-sediment submodel state, moved out of module GLOBAL (Phase 5.1).
    ! See docs/superpowers/specs/2026-07-23-sediment-state-derived-type-design.md
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

- [ ] **Step 3: Build to verify the type compiles**

```bash
make build-estas
```
Expected: clean build. (`bsed` is declared-but-unused — allowed; not an error.) If it fails, fix the type block only.

- [ ] **Step 4: Confirm byte-identity is untouched (bsed unused)**

Optional fast check — the run must still match golden since nothing references `bsed`:
```bash
./ESTAS_II INPUT_sediment_test.txt && diff -r OUTPUTS_gf_debug /tmp/golden_sed2 && echo "STILL IDENTICAL"
```
Expected: `STILL IDENTICAL`.

- [ ] **Step 5: Commit**

```bash
git add SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90
git commit -m "refactor(sediment): add sediment_state_t type + bsed instance (unused)"
```

---

### Task 3: Execute the move — rename all references, delete the GLOBAL block, add the `mod_SIMULATE` import

The atomic change. After this task the code compiles, is byte-identical, and the strip-and-compare proof is clean. This cannot be split further and still compile (a half-renamed tree links a live GLOBAL var against an unallocated `bsed` component).

**Files:**
- Modify: `SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90` (rewrite ~214 member refs to `bsed%…`, incl. the `allocate` block ~:375-401 and the executable init `SED_MODEL_CONSTANTS = 0.0D0` ~:389)
- Modify: `SOURCE_CODE/ESTAS/mod_SOLVER.f90` (~59 refs)
- Modify: `SOURCE_CODE/ESTAS/ESTAS_II.f90` (~20 refs, incl. the `deallocate` teardown ~:103-125)
- Modify: `SOURCE_CODE/ESTAS/mod_SIMULATE.f90` (~15 refs + add `use BOTTOM_SEDIMENTS, only: bsed`)
- Modify: `SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90` (2 refs)
- Modify: `SOURCE_CODE/ESTAS/mod_GLOBAL.f90` (delete the 24 declarations ~:136-206; add breadcrumb)
- Tool (scratchpad, not committed): `rename_bsed.py`

**Interfaces:**
- Consumes: `type(sediment_state_t) :: bsed` from Task 2; `PRE` SHA from Task 1.
- Produces: a byte-identical binary with the bottom-sediment state fully encapsulated in `bsed`.

- [ ] **Step 1: Create the word-boundary, comment-aware rename tool**

Save to your scratchpad (or `/tmp/rename_bsed.py`). It replaces `\bMEMBER\b → bsed%MEMBER` in the **code portion of each line only** (text before the first `!` that is not inside a quote), leaving comments and string literals verbatim, and **skips the `sediment_state_t` type-definition block** (so the component declarations added in Task 2 are not prefixed).

```python
#!/usr/bin/env python3
import re, sys

MEMBERS = [
    "INIT_SED_STATE_VARS","SED_DEPTHS","SED_POROSITIES","SED_DENSITIES",
    "PART_MIXING_COEFFS","SED_DIFFUSIONS","SURF_MIXLEN","SED_BURRIALS",
    "SURF_WATER_CONCS","SED_TEMPS","SED_MODEL_CONSTANTS","SED_TYPE_PER_BOX",
    "PROCESSES_sed","SED_DRIVING_FUNCTIONS","FLUXES_TO_SEDIMENTS","ADVECTIVE_VELOCITY",
    "H_ERODEP","SED_FLAGS","NUM_FLUX_RECEIVING_SED_LAYERS","FINAL_SED_STATE_VARS",
    "FLUXES_FROM_SEDIMENTS","SED_OUTPUTS","SED_SAVED_OUTPUTS","SED_BURRIAL_RATE_OUTPUTS",
]
# Case-insensitive whole-word match; longest-first is irrelevant with \b, but harmless.
PAT = re.compile(r'\b(' + '|'.join(sorted(MEMBERS, key=len, reverse=True)) + r')\b', re.IGNORECASE)

def code_and_comment(line):
    """Split off the first '!' not inside a quote -> (code, comment_incl_bang)."""
    q = None
    for i, ch in enumerate(line):
        if q:
            if ch == q: q = None
        elif ch in "'\"":
            q = ch
        elif ch == '!':
            return line[:i], line[i:]
    return line, ''

def main(path):
    out, skip = [], False
    for line in open(path):
        low = line.lower()
        if 'type, public :: sediment_state_t' in low:
            skip = True
        code, comment = code_and_comment(line)
        if skip:
            out.append(line)          # leave the whole type-def block verbatim
        else:
            out.append(PAT.sub(lambda m: 'bsed%' + m.group(0), code) + comment)
        if 'end type sediment_state_t' in low:
            skip = False
    open(path, 'w').write(''.join(out))

if __name__ == '__main__':
    main(sys.argv[1])
```

Note: the `type(sediment_state_t), public :: bsed` instance line contains no member token, so it is unaffected whether inside or outside the skip window.

- [ ] **Step 2: Run the rename on the 4 consumer files + the owner**

```bash
cd /home/razinka/AQUABCv0.2
for f in mod_BOTTOM_SEDIMENTS.f90 mod_SOLVER.f90 ESTAS_II.f90 mod_SIMULATE.f90 mod_AQUATIC_MODEL.f90; do
  python3 /tmp/rename_bsed.py SOURCE_CODE/ESTAS/$f
done
```

- [ ] **Step 3: Add the `use BOTTOM_SEDIMENTS, only: bsed` import to `mod_SIMULATE`**

`mod_SIMULATE` reaches the members via `use GLOBAL` today and needs an explicit handle on `bsed`. Add after its existing `use` lines (~:3-8):

```fortran
    use BOTTOM_SEDIMENTS, only: bsed
```
(Defensive import: `bsed` also re-exports transitively through `use AQUATIC_MODEL`/`use PELAGIC_SOLVER`, but the explicit form is robust to future `only:` narrowing.)

- [ ] **Step 4: Delete the 24 declarations from `mod_GLOBAL.f90` and add a breadcrumb**

Confirm the block first, then replace lines `136-206` (the declarations between the header at 133 and footer at 207) with a breadcrumb. Keep the `! Variables for bottom sediment submodel` / `! End of ...` framing comments if you like, but the 24 real declarations go away.

```bash
sed -n '133,207p' SOURCE_CODE/ESTAS/mod_GLOBAL.f90   # eyeball the exact block
```
Replace the 24 declaration lines with:

```fortran
    ! -----------------------------------------------------------------------------------
    ! Bottom-sediment submodel state (24 members) moved to the derived type `bsed`
    ! (type sediment_state_t) in module BOTTOM_SEDIMENTS — see
    ! docs/superpowers/specs/2026-07-23-sediment-state-derived-type-design.md.
    ! -----------------------------------------------------------------------------------
```
⚠️ Do NOT delete the adjacent blocks below the footer: `FLUXES_TO_WATER_COLUMN`/`FLUXES_OUTPUT_TO_WATER_COLUMN` (209-212), `DISSOLVED_FRACTIONS`…`DEPOSITION_AREA_RATIOS` (214-223), the `*_FILENAME` scalars, and `BOTTOM_SED_ADVANCED_REDOX_SIMULATION` — all stay.

- [ ] **Step 5: Build clean (the real completeness gate)**

```bash
make clean-all && make build-estas
```
Expected: clean build. A stale bare reference is now an undeclared symbol under `implicit none` → hard error naming the exact file/line. A wrongly-prefixed look-alike (e.g. `bsed%SED_DEPTHS_LOC`) is a non-existent component → hard error. Fix any until the build is clean. **If a continuation line overflows 132 columns** ("Syntax error in argument list" / truncated `&`), wrap it after a comma/operator — pure formatting.

- [ ] **Step 6: Strip-and-compare pure-prefix proof (per file)**

Prove the rename introduced ONLY the `bsed%` prefix + the known structural changes. `PRE` is the SHA from Task 1 Step 1.

```bash
PRE=$(cat /tmp/PRE_SHA)
for f in mod_BOTTOM_SEDIMENTS.f90 mod_SOLVER.f90 ESTAS_II.f90 mod_SIMULATE.f90 mod_AQUATIC_MODEL.f90; do
  echo "===== $f ====="
  diff <(git show $PRE:SOURCE_CODE/ESTAS/$f) <(sed 's/bsed%//g' SOURCE_CODE/ESTAS/$f)
done
```
Expected, per file, ONLY these structural diffs (everything else must vanish under the strip):
- `mod_BOTTOM_SEDIMENTS.f90`: the added `sediment_state_t` type block + `bsed` instance; any 132-col wraps.
- `mod_SIMULATE.f90`: the added `use BOTTOM_SEDIMENTS, only: bsed`; any wraps.
- `mod_SOLVER.f90` / `ESTAS_II.f90` / `mod_AQUATIC_MODEL.f90`: nothing but wraps (ideally empty).

If any member name appears **swapped** (e.g. a line that read `SED_DEPTHS` now strips back to `SED_POROSITIES`), that is a same-type mis-rename — fix it. (The build cannot catch a swap between two identically-typed members; this proof is what does.)

- [ ] **Step 7: GLOBAL deletion proof + moved-declaration fidelity**

```bash
PRE=$(cat /tmp/PRE_SHA)
# GLOBAL: only deletions + the breadcrumb, nothing else changed
diff <(git show $PRE:SOURCE_CODE/ESTAS/mod_GLOBAL.f90) SOURCE_CODE/ESTAS/mod_GLOBAL.f90
# Moved-decl fidelity — print both side by side and eyeball kind/rank per member:
echo "--- deleted GLOBAL declarations (pre-change) ---"
git show $PRE:SOURCE_CODE/ESTAS/mod_GLOBAL.f90 | sed -n '136,206p'
echo "--- sediment_state_t components (post-change) ---"
sed -n '/type, public :: sediment_state_t/,/end type sediment_state_t/p' SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90
```
Expected: the GLOBAL `diff` shows only the 24 deletions + breadcrumb. In the two printed listings, each member's kind/dimension must match (kinds: `real(kind=DBL)` vs `integer`; ranks `(:)`,`(:,:)`,`(:,:,:)`,`(:,:,:,:)`; the 3 scalars `SURF_MIXLEN`/`ADVECTIVE_VELOCITY`/`NUM_FLUX_RECEIVING_SED_LAYERS`).

- [ ] **Step 8: Spot-check the known implementation gotchas**

```bash
# The executable init must now target the component:
grep -n "SED_MODEL_CONSTANTS = 0.0D0\|bsed%SED_MODEL_CONSTANTS = 0.0D0" SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90
# SED_TYPE_PER_BOX must be allocated but NOT deallocated (preserve asymmetry):
grep -n "deallocate(bsed%SED_TYPE_PER_BOX" SOURCE_CODE/ESTAS/ESTAS_II.f90 && echo "ERROR: must NOT exist" || echo "OK: asymmetry preserved"
# Look-alikes must be intact (no bsed% prefix anywhere):
grep -rn "bsed%SED_DEPTHS_LOC\|bsed%NUM_SED\|bsed%COCOA_\|bsed%INIT_BSED" SOURCE_CODE/ESTAS/ && echo "ERROR: look-alike corrupted" || echo "OK: no look-alike corruption"
```
Expected: init shows `bsed%SED_MODEL_CONSTANTS = 0.0D0`; `OK: asymmetry preserved`; `OK: no look-alike corruption`.

- [ ] **Step 9: Commit**

```bash
git add SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90 SOURCE_CODE/ESTAS/mod_SOLVER.f90 \
        SOURCE_CODE/ESTAS/ESTAS_II.f90 SOURCE_CODE/ESTAS/mod_SIMULATE.f90 \
        SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90 SOURCE_CODE/ESTAS/mod_GLOBAL.f90
git commit -m "refactor(sediment): move bottom-sediment GLOBAL state into bsed (sediment_state_t)"
```

---

### Task 4: Byte-identical run gate

Proves numerics are unchanged. This is the acceptance gate.

**Files:** none (verification only)

**Interfaces:**
- Consumes: `/tmp/golden_sed2`, `/tmp/golden_std` from Task 1; the post-change binary from Task 3.

- [ ] **Step 1: Rebuild the post-change binary (serial, same flags as Task 1)**

```bash
make clean-all && make build-estas
```
Expected: clean build.

- [ ] **Step 2: Mode-2 gate (primary) — bit-for-bit vs golden**

```bash
./ESTAS_II INPUT_sediment_test.txt
diff -r OUTPUTS_gf_debug /tmp/golden_sed2 && echo "MODE-2 BYTE-IDENTICAL" || echo "DIFF — INVESTIGATE"
```
Expected: `MODE-2 BYTE-IDENTICAL` (empty diff). This covers 20 of 24 members (the diagenesis feedback chain + all sediment-model inputs). If it differs, the move perturbed numerics — do NOT proceed; bisect the rename (the strip-proof should have been clean, so suspect a swap the proof's structural noise hid, or a wrap that changed a literal).

- [ ] **Step 3: Mode-0 regression — bit-for-bit vs golden**

```bash
./ESTAS_II INPUT.txt
diff -r OUTPUTS /tmp/golden_std && echo "MODE-0 BYTE-IDENTICAL" || echo "DIFF — INVESTIGATE"
```
Expected: `MODE-0 BYTE-IDENTICAL` (confirms allocation/order wasn't perturbed on the sediment-off path).

- [ ] **Step 4: (Optional) COCOA coverage for the 4 strip-only members**

`PROCESSES_sed`, `SED_BURRIAL_RATE_OUTPUTS`, `SED_OUTPUTS`, `SED_DRIVING_FUNCTIONS` are not written unless COCOA output is on. To bring them into run-gate coverage, temporarily set `PRODUCE_COCOA_OUTPUTS = 1` in `INPUTS/PELAGIC_INPUTS.txt`, re-baseline the pre-change binary and re-diff (units 2021/2022). Skip if the strip-proof + compile coverage is accepted (it is defensible — those catch swaps/drops/misspellings).

- [ ] **Step 5: No commit (verification only). Record both results for the PR body.**

---

### Task 5: Fortran unit tests, CI, and PR

**Files:** none (build/test/PR only)

**Interfaces:**
- Consumes: the verified branch from Tasks 3–4.

- [ ] **Step 1: Fortran unit tests (build-health)**

```bash
make test
```
Expected: green. (These link none of the 5 refactored files, so this is build-health only — zero coverage of the moved subsystem; the byte-identical gate is the real test.)

- [ ] **Step 2: Push the branch**

```bash
git push -u origin refactor/bottom-sediment-state-derived-type
```

- [ ] **Step 3: Open the PR with evidence**

Open a PR to `main`. Body must include: the mode-2 + mode-0 byte-identical results (Task 4), the strip-and-compare output (Task 3 Step 6-7), the GLOBAL allocatable count drop (44 → 23: `grep -c allocatable SOURCE_CODE/ESTAS/mod_GLOBAL.f90`), and a link to the design spec.

- [ ] **Step 4: Confirm CI matrix green**

Expected: gfortran (ubuntu/macOS) + ifx (oneAPI) + integration + lint all green. The multi-pass `make_lib.sh` absorbs the new `SIMULATE → BOTTOM_SEDIMENTS` module edge without a build-order edit; if ifx flags anything, it is a real conformance issue, not a spurious one.

- [ ] **Step 5: Merge on the user's go-ahead.**

---

## Self-Review

**Spec coverage:** The move (24 members, 5 files, GLOBAL deletion) → Tasks 2-3. Type fidelity → Task 2 Step 2 + Task 3 Step 7. Word-boundary/`*_LOC` discipline → Task 3 Steps 1-2, 8. `SED_MODEL_CONSTANTS=0.0D0` init, `SED_TYPE_PER_BOX` preserve-asymmetry, no-component-private → Task 3 Steps 8 / Task 2 Step 2. `mod_SIMULATE` import → Task 3 Step 3. Gate on mode-2 `INPUT_sediment_test.txt` + mode-0 regression + determinism pre-check → Tasks 1, 4. Strip-and-compare proof → Task 3 Steps 6-7. COCOA caveat → Task 4 Step 4. Compile/CI/unit-test framing → Tasks 3, 5. All spec sections map to a task.

**Placeholder scan:** No TBD/TODO; every code and command step shows exact content.

**Type consistency:** `sediment_state_t` / `bsed` / the 24 member tokens are used identically across Tasks 2-4. The rename tool's member list matches the type block and the Global Constraints list.
