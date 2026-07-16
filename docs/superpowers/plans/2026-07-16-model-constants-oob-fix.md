# Model-Constants OOB Write Fix (TODO 1.10) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans (inline). Steps use checkbox (`- [ ]`) syntax.

**Goal:** Eliminate the out-of-bounds write in the model-constants reader (`nconst=318` vs 323 constants in `WCONST_04.txt`) by bumping `nconst`/`NUM_MODEL_CONSTANTS` to 323, with **byte-identical production output** (a pure memory-safety fix — the ESTAS path never read `BETA_*` from the OOB slots, so nothing about the science changes).

**Architecture:** One-value change (`318→323`) in two Fortran declarations, the input `NUM_MODEL_CONSTANTS` in every config in play, and the config generator. `WCONST_04.txt` unchanged. Acceptance = byte-identical output vs the pre-fix baseline.

**Tech Stack:** Fortran 90 (gfortran), GNU Make, Python (the config generator), the 1.6 verification harness.

## Global Constraints

- **Byte-identical production output** is the acceptance criterion — the standard (advanced-redox-off) default config must be unchanged (serial + OMP=8) vs the pre-fix `verify_baseline/default_serial` (`nconst=318`). Verified by `tools/refactor_verify.sh`. **Do NOT re-capture the baseline** (it must stay the pre-fix output, or the test proves nothing).
- **No model-logic change**; **`WCONST_04.txt` unchanged.**
- Bump `NUM_MODEL_CONSTANTS`/`nconst` to 323 in every config **actually in play** (production + the two committed verify variants + the generator + CL29 local). Do NOT touch the orphaned legacy `PELAGIC_INPUTS_{per_square,before_dissolved_fractions_fixing,WCONST_02,zero_nost_boundary}.txt` files (they sit at an older 307 and are unreferenced).
- **Branch:** `fix/model-constants-oob` (already created; spec committed).
- Use `.venv/bin/python` for the 0D comparator.

---

## Task 1: Apply the fix

**Files:** `SOURCE_CODE/ESTAS/mod_GLOBAL.f90:20`, `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90:75`, `INPUTS/PELAGIC_INPUTS.txt:9`, `INPUTS/PELAGIC_INPUTS_verify.txt:9`, `INPUTS/PELAGIC_INPUTS_verify_ar.txt:9`, `tools/eutropy_poc/eutropy_to_estas.py:595`, `INPUTS_CL29/PELAGIC_INPUTS.txt:9` (local).

- [ ] **Step 1: Bump `nconst`, the input configs, and the generator**

```bash
sed -i '20s/nconst *= *318/nconst                        = 323/' SOURCE_CODE/ESTAS/mod_GLOBAL.f90
sed -i '75s/nconst *= *318/nconst              = 323/' SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90
for f in INPUTS/PELAGIC_INPUTS.txt INPUTS/PELAGIC_INPUTS_verify.txt INPUTS/PELAGIC_INPUTS_verify_ar.txt INPUTS_CL29/PELAGIC_INPUTS.txt; do
  sed -i '9s/318/323/' "$f"
done
sed -i '595s/NUM_MODEL_CONSTANTS", 318/NUM_MODEL_CONSTANTS", 323/' tools/eutropy_poc/eutropy_to_estas.py
```

- [ ] **Step 2: Verify each targeted site is now 323 (named files only — not a broad glob)**

```bash
grep -m1 -oE 'nconst.*= *[0-9]+' SOURCE_CODE/ESTAS/mod_GLOBAL.f90
grep -m1 -oE 'nconst.*= *[0-9]+' SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90
for f in INPUTS/PELAGIC_INPUTS.txt INPUTS/PELAGIC_INPUTS_verify.txt INPUTS/PELAGIC_INPUTS_verify_ar.txt INPUTS_CL29/PELAGIC_INPUTS.txt; do echo "$f: $(sed -n '9p' "$f" | tr -d ' ')"; done
grep -n 'NUM_MODEL_CONSTANTS", 32' tools/eutropy_poc/eutropy_to_estas.py
```
Expected: both `nconst … = 323`; every named config line 9 = `323`; the generator line shows `323`. (Ignore the orphaned `PELAGIC_INPUTS_*` legacy files — they stay at 307.)

- [ ] **Step 3: Build (release) and confirm the model runs (no `NUM≠nconst` error-stop)**

```bash
make build-estas 2>&1 | tail -3
rm -f OUTPUTS_verify/*.out; ./ESTAS_II INPUT_verify.txt 2>&1 | grep -ciE 'error stop|upper bound'
echo "out files: $(ls OUTPUTS_verify/*.out 2>/dev/null | wc -l)"
```
Expected: `Executable 'ESTAS_II' created successfully`; `0` error-stop/bounds messages; `52` output files.

- [ ] **Step 4: Confirm the OOB is gone under `-fcheck=all`**

```bash
make BUILD_TYPE=debug build-estas >/dev/null 2>&1
rm -f OUTPUTS_verify/*.out; ./ESTAS_II INPUT_verify.txt 2>&1 | grep -ciE 'above upper bound|Index .* of dimension'
make build-estas >/dev/null 2>&1   # back to release
```
Expected: `0` (the "Index 319 … above upper bound of 318" no longer fires).

- [ ] **Step 5: Commit the fix**

```bash
git add SOURCE_CODE/ESTAS/mod_GLOBAL.f90 SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90 \
        INPUTS/PELAGIC_INPUTS.txt INPUTS/PELAGIC_INPUTS_verify.txt INPUTS/PELAGIC_INPUTS_verify_ar.txt \
        tools/eutropy_poc/eutropy_to_estas.py
git commit -m "$(cat <<'MSG'
fix(model): eliminate out-of-bounds write in model-constants reader (TODO 1.10)

NUM_MODEL_CONSTANTS was declared 318 but WCONST_04.txt has 323 constants, so
READ_MODEL_CONSTANTS (mod_UTILS_01.f90) wrote indices 319-323 PAST the end of the
318-element MODEL_CONSTANTS array -- an out-of-bounds write (undefined behavior,
flagged by -fcheck=all). Bump nconst (mod_GLOBAL, aquabc_II_pelagic_interface) and
NUM_MODEL_CONSTANTS (input configs + the eutropy_to_estas.py generator) to 323 so
the array is sized correctly.

Memory-safety fix only: PRODUCTION OUTPUT IS UNCHANGED (byte-identical, verified).
The ESTAS constant-unpacking (mod_PELAGIC_ECOLOGY.f90 INIT_PELAGIC_MODEL_CONSTANTS)
stops at index 318 and never reads BETA_* (319-323), so those were never consumed
from the OOB slots on the production path. WCONST_04.txt unchanged; 0D golden
unaffected. (Separate/out-of-scope: BETA_* photoinhibition is not wired into the
ESTAS path at all -- harmless today since BETA=0 is the intended default.)

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>
MSG
)"
```
(`INPUTS_CL29/PELAGIC_INPUTS.txt` is git-ignored — bumped locally, not staged.)

---

## Task 2: Verify byte-identical + stability

**Files:** none modified (verification only).

- [ ] **Step 1: Byte-identical gate vs the pre-fix (318) baseline**

The pre-fix baseline `verify_baseline/default_serial` (and `default_omp8`) were captured at `nconst=318` during 1.6. Run the gate WITHOUT re-capturing:

```bash
tools/refactor_verify.sh; echo "exit=$?"
```
Expected: `[default/serial] BIT-IDENTICAL (52 files)`, `[default/omp8] BIT-IDENTICAL (52 files)`, `[0D golden] PASS`, `GATE: PASS`, `exit=0`. This proves the fix changed no production output. (If it is NOT bit-identical, STOP — the fix unexpectedly changed output; investigate before proceeding.)

- [ ] **Step 2: Full-year stability + determinism**

```bash
rm -f OUTPUTS/*.out; ./ESTAS_II INPUT.txt 2>&1 | tail -1                      # expect "simulation finished"
mkdir -p /tmp/cy1; cp OUTPUTS/PELAGIC_BOX_00005.out /tmp/cy1/
rm -f OUTPUTS/*.out; ./ESTAS_II INPUT.txt >/dev/null 2>&1
cmp -s /tmp/cy1/PELAGIC_BOX_00005.out OUTPUTS/PELAGIC_BOX_00005.out && echo "FULL-YEAR DETERMINISTIC" || echo "NON-DETERMINISTIC (investigate)"
```
Expected: `simulation finished`, `FULL-YEAR DETERMINISTIC`.

---

## Task 3: Land

**Files:** Modify `TODO_IMPLEMENTATION_PLAN.md` (§1.10 → done + roadmap checkbox).

- [ ] **Step 1: Mark backlog §1.10 complete**

Change §1.10 status to `✅ COMPLETE 2026-07-16` — nconst 318→323 eliminates the OOB write; production output byte-identical (memory-safety fix, no science change; adversarial review corrected the original garbage-BETA framing). Check the `- [ ] 1.10` roadmap box.

- [ ] **Step 2: Commit the backlog update**

```bash
git add TODO_IMPLEMENTATION_PLAN.md
git commit -m "docs: mark TODO 1.10 (model-constants OOB write) complete"
```

- [ ] **Step 3: Push + CI**

```bash
git push -u origin fix/model-constants-oob
```
Watch CI (`build-and-run`, `integration-tests`, ftnchek, 0D E2E) to green.

- [ ] **Step 4: Finish the branch**

Use superpowers:finishing-a-development-branch to present merge options; merge to `main` on green. No scientific sign-off gate — output is byte-identical.

---

## Notes for the executor

- **Acceptance is byte-identical, not "characterize the change."** The original plan's per-variable characterization + sign-off gate was removed after the adversarial review + empirical verification showed the fix does not change production output (the ESTAS path never reads `BETA_*` from the OOB slots).
- **Do NOT run `tools/refactor_baseline.sh`** — that re-captures the baseline at the fixed code (323), making Task 2 Step 1 a trivial self-comparison. The baseline must stay the pre-fix (318) output.
- If a config file is left at 318, the model prints `The number of pelagic model constants are not compitable …` + `stop "error stop"` (exit 0, partial output) — the `NUM_MODEL_CONSTANTS ≠ nconst` guard.
- The orphaned `INPUTS/PELAGIC_INPUTS_{per_square,before_dissolved_fractions_fixing.txt,WCONST_02,zero_nost_boundary}.txt` are unreferenced legacy configs at 307 — intentionally NOT touched.
