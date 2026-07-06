# Runtime per-application temperature model Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the phytoplankton temperature model (plateau vs CTMI) a runtime per-application option, defaulting to plateau, with CL29 opting into CTMI.

**Architecture:** A single flag `USE_CTMI_TEMP` lives in the shared AQUABC global module (`mod_AQUABC_II_GLOBAL`), defaults `.false.` (plateau). `GROWTH_AT_TEMP` reads it. ESTAS's options reader sets it from a new `TEMPERATURE_MODEL` line in `PELAGIC_MODEL_OPTIONS.txt`; the CL29 converter writes that line = 1. The 0-D example never reads the options file, so it keeps the plateau default.

**Tech Stack:** Fortran (gfortran), built via the root `Makefile` (`make build-lib build-example build-estas`); Python 3 stdlib converter (`tools/eutropy_poc/eutropy_to_estas.py`).

## Global Constraints

- Default temperature model is **plateau** (`USE_CTMI_TEMP = .false.`); CTMI is explicit opt-in.
- Option files **without** the `TEMPERATURE_MODEL` line must still work → read it with `end=`/`err=` handling that leaves the flag at plateau.
- The AQUABC library must **not** depend on ESTAS. The flag lives in an AQUABC module (`mod_AQUABC_II_GLOBAL`); ESTAS sets it, the library reads it.
- Keep the existing invalid-CTMI graceful fallback in `GROWTH_AT_TEMP` (warn once + plateau) as a safety net.
- This codebase is verified by **build + run + observe** (integration), matching CI's `build-and-run`. There is no fine-grained Fortran unit-test harness for config flags, so each task's test is a build/run/grep cycle. The debug-heavy run log is large (~hundreds of MB); always redirect it to a file, grep, then delete it.

**Option-file placement (applies to Tasks 2, 3):** `READ_PELAGIC_MODEL_OPTIONS` reads sequentially up to `CONSIDER_ALLELOPATHY`, then the file is closed; the trailing `CYN_ALLELOPATHY_FILE_NAME` lines are never read. The new `TEMPERATURE_MODEL` line goes **immediately after the `CONSIDER_ALLELOPATHY` value**, and is read immediately after `CONSIDER_ALLELOPATHY` in the reader.

---

### Task 1: Runtime flag + GROWTH_AT_TEMP reads it

**Files:**
- Modify: `SOURCE_CODE/AQUABC/mod_AQUABC_II_GLOBAL.f90`
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90` (subroutine `GROWTH_AT_TEMP`, ~lines 45-107)

**Interfaces:**
- Produces: module variable `AQUABC_II_GLOBAL :: USE_CTMI_TEMP` — `logical`, default `.false.`. Read by `GROWTH_AT_TEMP`; set by Task 2.

- [ ] **Step 1: Add the flag to the AQUABC global module**

Current `SOURCE_CODE/AQUABC/mod_AQUABC_II_GLOBAL.f90` is:
```fortran
module AQUABC_II_GLOBAL
    use precision_kinds, only: DBL_PREC
    implicit none
end module AQUABC_II_GLOBAL
```
Change it to:
```fortran
module AQUABC_II_GLOBAL
    use precision_kinds, only: DBL_PREC
    implicit none

    ! Temperature-response model selected at runtime by the driver.
    ! .false. = piecewise plateau (default); .true. = CTMI (Rosso et al. 1993).
    ! GROWTH_AT_TEMP reads this; ESTAS sets it from PELAGIC_MODEL_OPTIONS.txt.
    logical :: USE_CTMI_TEMP = .false.
end module AQUABC_II_GLOBAL
```

- [ ] **Step 2: Point GROWTH_AT_TEMP at the runtime flag**

In `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90`, in `GROWTH_AT_TEMP`:

(a) After the existing `use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp` line, add:
```fortran
    use AQUABC_II_GLOBAL, only: USE_CTMI_TEMP
```

(b) Delete this declaration line:
```fortran
    logical, parameter :: USE_CTMI = .true.
```

(c) Replace the two `USE_CTMI` references with `USE_CTMI_TEMP`. They are:
```fortran
    ctmi_ok = USE_CTMI .and. (T_opt > T_min) .and. (T_opt < T_max) .and. &
              (2.0D0 * T_opt > T_min + T_max)
    if (USE_CTMI .and. (.not. ctmi_ok) .and. (.not. ctmi_warned)) then
```
become:
```fortran
    ctmi_ok = USE_CTMI_TEMP .and. (T_opt > T_min) .and. (T_opt < T_max) .and. &
              (2.0D0 * T_opt > T_min + T_max)
    if (USE_CTMI_TEMP .and. (.not. ctmi_ok) .and. (.not. ctmi_warned)) then
```

- [ ] **Step 3: Build**

Run: `make build-lib build-example build-estas 2>&1 | grep -iE "error|created successfully" | grep -viE "unused|dummy"`
Expected: no `error`; ends with `Executable 'ESTAS_II' created successfully` (exit 0).

- [ ] **Step 4: Run the 25-box model and verify it now runs plateau directly (no CTMI attempt/warning)**

Run:
```bash
./ESTAS_II INPUT.txt > /tmp/t1.log 2>&1
echo "finished=$(grep -c 'simulation finished' /tmp/t1.log)  fallback=$(grep -c 'falling back to plateau' /tmp/t1.log)"
rm -f /tmp/t1.log
```
Expected: `finished=1  fallback=0`. (Before this task the flag was compile-time CTMI, so the 25-box attempted CTMI and printed the fallback warning; now the runtime default is plateau, so no attempt, no warning.)

- [ ] **Step 5: Commit**

```bash
git add SOURCE_CODE/AQUABC/mod_AQUABC_II_GLOBAL.f90 SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90
git commit -m "feat(aquabc): read temperature model from runtime USE_CTMI_TEMP flag (default plateau)"
```

---

### Task 2: ESTAS reads TEMPERATURE_MODEL and sets the flag

**Files:**
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` (subroutine `READ_PELAGIC_MODEL_OPTIONS`, ~lines 1071-1140)
- Modify: `INPUTS/PELAGIC_MODEL_OPTIONS.txt`

**Interfaces:**
- Consumes: `AQUABC_II_GLOBAL :: USE_CTMI_TEMP` (Task 1).
- Produces: after this task, an ESTAS run sets `USE_CTMI_TEMP` from `PELAGIC_MODEL_OPTIONS.txt`'s `TEMPERATURE_MODEL` value (0/1); absent line → plateau.

- [ ] **Step 1: Add the reader logic**

In `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90`, `subroutine READ_PELAGIC_MODEL_OPTIONS(IN_FILE)`:

(a) Under the existing `use GLOBAL` / `use ALLELOPATHY`, add:
```fortran
    use AQUABC_II_GLOBAL, only: USE_CTMI_TEMP
```

(b) After `integer, intent(in) :: IN_FILE`, add a local:
```fortran
    integer :: TEMP_MODEL_OPT
```

(c) Immediately before `end subroutine READ_PELAGIC_MODEL_OPTIONS` (after the `CONSIDER_ALLELOPATHY` `if` block), insert:
```fortran
    ! Temperature-response model (0 = plateau, 1 = CTMI). Read gracefully so option
    ! files without this line default to plateau. This sets the AQUABC-side flag that
    ! GROWTH_AT_TEMP reads. The trailing CYN_ALLELOPATHY_FILE_NAME lines (if present)
    ! are consumed harmlessly here; the file is closed right after this routine.
    USE_CTMI_TEMP = .false.
    read(IN_FILE + 1, *, end = 900, err = 900)
    read(IN_FILE + 1, *, end = 900, err = 900) TEMP_MODEL_OPT
    USE_CTMI_TEMP = (TEMP_MODEL_OPT == 1)
900 continue
    if (USE_CTMI_TEMP) then
        write(*,*) 'Temperature model: CTMI (Cardinal Temperature Model).'
    else
        write(*,*) 'Temperature model: plateau (piecewise, default).'
    end if
```

- [ ] **Step 2: Add the option line to the 25-box template (= plateau)**

In `INPUTS/PELAGIC_MODEL_OPTIONS.txt`, immediately after the `CONSIDER_ALLELOPATHY` value line (the `1` under `# CONSIDER_ALLELOPATHY`), insert these two lines (before `# CYN_ALLELOPATHY_FILE_NAME`):
```
# TEMPERATURE_MODEL (0=plateau, 1=CTMI)
            0
```

- [ ] **Step 3: Build**

Run: `make build-estas 2>&1 | grep -iE "error|created successfully" | grep -viE "unused|dummy"`
Expected: no `error`; `Executable 'ESTAS_II' created successfully`.

- [ ] **Step 4: Verify the plateau path (25-box reads = 0)**

Run:
```bash
./ESTAS_II INPUT.txt > /tmp/t2a.log 2>&1
grep -m1 "Temperature model" /tmp/t2a.log
echo "finished=$(grep -c 'simulation finished' /tmp/t2a.log)  fallback=$(grep -c 'falling back' /tmp/t2a.log)"
rm -f /tmp/t2a.log
```
Expected: prints `Temperature model: plateau (piecewise, default).`; `finished=1  fallback=0`.

- [ ] **Step 5: Verify the CTMI path end-to-end (temporary manual opt-in for CL29)**

The converter change is Task 3; for now, set CL29's option by hand to prove the reader → flag → GROWTH chain:
```bash
# insert TEMPERATURE_MODEL=1 into the CL29 options file, after CONSIDER_ALLELOPATHY
python3 - <<'PY'
p="INPUTS_CL29/PELAGIC_MODEL_OPTIONS.txt"
L=open(p).read().splitlines(keepends=True)
for i,ln in enumerate(L):
    if ln.strip()=="# CONSIDER_ALLELOPATHY":
        L[i+2:i+2]=["# TEMPERATURE_MODEL (0=plateau, 1=CTMI)\n","            1\n"]; break
open(p,"w").writelines(L)
PY
ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt > /tmp/t2b.log 2>&1
grep -m1 "Temperature model" /tmp/t2b.log
# diatoms should bloom under CTMI: per-box DIA_C (col 6) max well above the 0.002 seed
for b in 00002 00003 00029; do awk 'NR>1{if($6>m)m=$6}END{printf "box %s DIA_max=%.3f\n","'$b'",m}' OUTPUTS_CL29/PELAGIC_BOX_$b.out; done
rm -f /tmp/t2b.log
```
Expected: prints `Temperature model: CTMI ...`; DIA_max values ~1.7-1.9 (blooming), not ~0.002.

- [ ] **Step 6: Commit**

```bash
git add SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90 INPUTS/PELAGIC_MODEL_OPTIONS.txt
git commit -m "feat(estas): read TEMPERATURE_MODEL option, set USE_CTMI_TEMP (default plateau)"
```

---

### Task 3: CL29 converter writes TEMPERATURE_MODEL = 1

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py` (near the `PELAGIC_MODEL_OPTIONS.txt` copy, ~lines 163-165)

**Interfaces:**
- Consumes: the `TEMPERATURE_MODEL` option read by ESTAS (Task 2).
- Produces: regenerating `INPUTS_CL29/` yields `PELAGIC_MODEL_OPTIONS.txt` with `TEMPERATURE_MODEL = 1`.

- [ ] **Step 1: Add a helper and call it after the options-file copy**

In `tools/eutropy_poc/eutropy_to_estas.py`, add this function near the other `_write_*`/helper functions:
```python
def _set_temperature_model_ctmi(path):
    """CL29 opts into the CTMI temperature model: set TEMPERATURE_MODEL = 1 in the
    copied PELAGIC_MODEL_OPTIONS.txt (append the option if the template lacks it)."""
    with open(path) as fh:
        lines = fh.readlines()
    for i, ln in enumerate(lines):
        if ln.strip().startswith("# TEMPERATURE_MODEL"):
            lines[i + 1] = "            1\n"
            break
    else:
        lines.append("# TEMPERATURE_MODEL (0=plateau, 1=CTMI)\n")
        lines.append("            1\n")
    with open(path, "w") as fh:
        fh.writelines(lines)
```
Then, right after the existing copy of the options file:
```python
    shutil.copy(os.path.join(REPO, "INPUTS", "PELAGIC_MODEL_OPTIONS.txt"),
                os.path.join(OUT, "PELAGIC_MODEL_OPTIONS.txt"))
```
add:
```python
    _set_temperature_model_ctmi(os.path.join(OUT, "PELAGIC_MODEL_OPTIONS.txt"))
```

- [ ] **Step 2: Regenerate CL29 inputs and verify the option is set**

Run:
```bash
python3 tools/eutropy_poc/eutropy_to_estas.py 2>&1 | grep -iE "wrote|error" | head -1
grep -A1 "# TEMPERATURE_MODEL" INPUTS_CL29/PELAGIC_MODEL_OPTIONS.txt
```
Expected: `[estas] wrote 29-box INPUTS ...`; the grep shows `# TEMPERATURE_MODEL (0=plateau, 1=CTMI)` followed by a line whose value is `1`.

- [ ] **Step 3: Run CL29 and verify CTMI + diatom bloom (now automatic, no manual edit)**

Run:
```bash
ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt > /tmp/t3.log 2>&1
grep -m1 "Temperature model" /tmp/t3.log
for b in 00002 00003 00029; do awk 'NR>1{if($6>m)m=$6}END{printf "box %s DIA_max=%.3f\n","'$b'",m}' OUTPUTS_CL29/PELAGIC_BOX_$b.out; done
rm -f /tmp/t3.log
```
Expected: `Temperature model: CTMI ...`; DIA_max ~1.7-1.9 (blooming).

- [ ] **Step 4: Commit**

```bash
git add tools/eutropy_poc/eutropy_to_estas.py
git commit -m "feat(tools): converter sets CL29 TEMPERATURE_MODEL=1 (opt into CTMI)"
```

---

### Task 4: Full CI-equivalent verification

**Files:** none (verification only).

**Interfaces:** Consumes the complete feature (Tasks 1-3).

- [ ] **Step 1: Run the checks CI's `build-and-run` runs**

Run:
```bash
make build-lib build-example build-estas 2>&1 | grep -iE "error|created successfully" | grep -viE "unused|dummy" | tail -1
# 25-box (run-example) -> plateau, completes
make run-example > /tmp/re.log 2>&1; echo "run-example finished=$(grep -c 'simulation finished' /tmp/re.log)"; rm -f /tmp/re.log
# 0-D example regression (NaN/negative CHLA) -> defaults plateau
make -C SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D test 2>&1 | tail -2
# Fortran unit tests (own plateau copy) -> unaffected
make -C tests/fortran clean test 2>&1 | tail -2
```
Expected: build ends `created successfully`; `run-example finished=1`; the 0-D `test` target reports success (no NaN/negative CHLA); the Fortran unit tests pass.

- [ ] **Step 2: Confirm CL29 CTMI succession is intact**

Run:
```bash
ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt > /tmp/cl.log 2>&1
grep -m1 "Temperature model" /tmp/cl.log
awk 'NR>1{if($6>d)d=$6; if($16>c)c=$16; if($17>o)o=$17}END{printf "box3 DIA=%.3f CYN=%.3f OPA=%.3f\n",d,c,o}' OUTPUTS_CL29/PELAGIC_BOX_00003.out
rm -f /tmp/cl.log
```
Expected: `Temperature model: CTMI ...`; `box3 DIA≈1.70 CYN≈2.71 OPA≈0.67` (the merged CTMI succession, unchanged).

- [ ] **Step 3: Final commit (if any verification tweak was needed) and hand off**

No code change expected here. If everything passed, the branch is ready to push and open a PR (handled outside this plan).

---

## Notes for the implementer

- **Do not** re-enable a compile-time default; the whole point is the runtime flag. `USE_CTMI_TEMP` defaults `.false.` in the module and is only flipped by the ESTAS reader.
- The graceful `end=900, err=900` reads are load-bearing for backward compatibility — never remove them.
- Between Tasks 1 and 3 the CL29 run temporarily uses plateau (until the converter sets the option in Task 3); that is expected on this feature branch and resolved by Task 3.
