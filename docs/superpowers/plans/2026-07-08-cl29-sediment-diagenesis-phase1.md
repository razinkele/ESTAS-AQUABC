# CL29 Sediment Diagenesis Phase 1 — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stand up the full sediment diagenesis model (`MODEL_SEDIMENTS = 2`) for the CL29 29-box application as an opt-in converter toggle, and prove it runs stably for a clean-checkout 5-year simulation.

**Architecture:** All changes are in the converter `tools/eutropy_poc/eutropy_to_estas.py`, which generates `INPUTS_CL29/`. A new `_write_sediment_inputs()` copies the 170-constant `W_SED_CONST.txt` and authors `BOTTOM_SEDIMENT_MODEL_INPUT.txt` (advanced-redox forced to 0, optional carbonate-IC override); `_write_input_txt()` gains an `enable_sediments` flag that switches `INPUT_CL29.txt` between the `MODEL_SEDIMENTS=0` layout (today's baseline) and the `=2` layout, and sets `PRINT_INTERVAL`. The feature is off by default so the baseline stays byte-identical. Stability is validated by running (not by unit tests): a staged short run resolves the carbonate ICs empirically, then a full 5-year run is gated on output.

**Tech Stack:** Python 3 (converter + pytest), Fortran (ESTAS/AQUABC, built via `make`), gfortran with optional OpenMP.

## Global Constraints

- Opt-in, off by default: `CL29_ENABLE_SEDIMENTS = False`. With it off, generated inputs are **byte-identical** to today's baseline.
- No Fortran source changes required for the core; runtime uses `make OPENMP=1`. (`debug_stranger=.false.` is an optional runtime lever, not part of this plan.)
- Uniform sediment profile broadcast to all 29 boxes (per-box = Phase 2).
- Sediment `# ADVANCED REDOX SIMULATION` flag = **0** (match CL29 pelagic `PELAGIC_MODEL_OPTIONS.txt` line 4 = 0).
- `INPUT_CL29.txt` `==2` layout: `MODEL_SEDIMENTS` = 2, then `# BOTTOM SEDIMENT MODEL INPUT FILE` + `BOTTOM_SEDIMENT_MODEL_INPUT.txt`. The `# NUM_PRESCRIBED_SEDIMENT_FLUX_SETS` line MUST be absent (else the parser reads `0` as the filename → `open("INPUTS_CL29/0")` crash).
- Sediment output filenames stay **bare** (they open as `OUTPUTS_CL29/ // <name>`; a path there crashes — ESTAS never creates dirs). Only `BOTTOM_SEDIMENTS_OUTPUTS.out` and `SEDIMENT_FLUX_OUTPUTS.out` are produced (COCOA gated off).
- `PRINT_INTERVAL` = 240 (daily) when sediments on, 10 when off. On coarsens all pelagic output to daily.
- Preserve CRLF line endings when authoring `BOTTOM_SEDIMENT_MODEL_INPUT.txt` (the template is CRLF).
- Run with `ESTAS_HOLD_VOLUME=1`.
- Stability gates: no NaN; no CO2SYS `'pH does not converge'` stop; sediment concentrations not pinned at the 0 floor; benthic N+P return finite, correctly signed, order-of-magnitude plausible (post-year-1).
- Sediment state-var indices (IC block row order): INORG_C = var 13, TOT_ALK = var 14; 7 layers.

## File Structure

- **Modify** `tools/eutropy_poc/eutropy_to_estas.py`:
  - add module constants `CL29_ENABLE_SEDIMENTS`, `CL29_SED_CARBONATE_IC` (near `CL29_WCONST_OVERRIDE`, ~line 77);
  - add helpers `_replace_leading_number`, `_sed_ic_block_bounds`, `_override_sed_carbonate`, and `_write_sediment_inputs` (before `_write_input_txt`, ~line 483);
  - change `_write_input_txt(repo, tdays)` → `_write_input_txt(repo, tdays, enable_sediments=False)` (~line 484), edit its `PRINT_INTERVAL` line (494) and sediment block (500-503);
  - wire both into `main()` (~line 268-269).
- **Create** `tests/python/test_sediment_inputs.py` — converter unit tests.
- **Read-only templates:** `INPUTS/W_SED_CONST.txt`, `INPUTS/BOTTOM_SEDIMENT_MODEL_INPUT.txt`.
- **Generated (gitignored):** `INPUTS_CL29/W_SED_CONST.txt`, `INPUTS_CL29/BOTTOM_SEDIMENT_MODEL_INPUT.txt`.

---

### Task 1: Converter — sediment input generator (`_write_sediment_inputs`)

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py` (constants ~77; helpers + writer before `_write_input_txt` ~483)
- Test: `tests/python/test_sediment_inputs.py`

**Interfaces:**
- Produces: module constants `CL29_ENABLE_SEDIMENTS: bool` (default `False`), `CL29_SED_CARBONATE_IC: tuple[float,float] | None` (default `None`); function `_write_sediment_inputs(out: str, enable_sediments: bool) -> None` (no-op when `False`; else writes `out/W_SED_CONST.txt` and `out/BOTTOM_SEDIMENT_MODEL_INPUT.txt`).
- Consumes: module globals `REPO` (repo root) and `INPUTS/` templates.

- [ ] **Step 1: Write the failing test**

Create `tests/python/test_sediment_inputs.py`:

```python
"""Tests for the CL29 sediment-diagenesis converter additions (Phase 1)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "tools", "eutropy_poc", "eutropy_to_estas.py")
_SPEC = importlib.util.spec_from_file_location("eutropy_to_estas", _PATH)
conv = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(conv)   # executes module; REPO = os.getcwd() = repo root


def _redox_flag(path):
    """Return the integer on the line after '# ADVANCED REDOX SIMULATION'."""
    with open(path, newline="") as fh:
        lines = fh.readlines()
    for i, ln in enumerate(lines):
        if ln.lstrip().startswith("# ADVANCED REDOX SIMULATION"):
            return int(lines[i + 1].split()[0])
    raise AssertionError("redox header not found")


class TestWriteSedimentInputs:
    def test_disabled_writes_nothing(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), False)
        assert not (tmp_path / "W_SED_CONST.txt").exists()
        assert not (tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt").exists()

    def test_enabled_copies_constants_verbatim(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), True)
        out = (tmp_path / "W_SED_CONST.txt").read_bytes()
        src = open(os.path.join(os.getcwd(), "INPUTS", "W_SED_CONST.txt"), "rb").read()
        assert out == src

    def test_enabled_forces_redox_zero(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), True)
        assert _redox_flag(str(tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt")) == 0

    def test_output_names_stay_bare(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), True)
        text = (tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt").read_text()
        assert "BOTTOM_SEDIMENTS_OUTPUTS.out" in text
        assert "/BOTTOM_SEDIMENTS_OUTPUTS.out" not in text  # no path prefix

    def test_carbonate_override_applied(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_SED_CARBONATE_IC", (3.0, 3.1))
        conv._write_sediment_inputs(str(tmp_path), True)
        with open(tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt", newline="") as fh:
            lines = fh.readlines()
        start, _ = conv._sed_ic_block_bounds(lines)
        inorg = [float(x) for x in lines[start + 12].split()]
        alk = [float(x) for x in lines[start + 13].split()]
        assert all(v == 3.0 for v in inorg) and len(inorg) == 7
        assert all(v == 3.1 for v in alk) and len(alk) == 7
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/razinka/AQUABCv0.2 && python -m pytest tests/python/test_sediment_inputs.py -v`
Expected: FAIL — `AttributeError: module 'eutropy_to_estas' has no attribute '_write_sediment_inputs'`.

- [ ] **Step 3: Add the constants**

In `tools/eutropy_poc/eutropy_to_estas.py`, after the `CL29_WCONST_OVERRIDE = {...}` block (~line 77), add:

```python
# Phase-1 sediment diagenesis (MODEL_SEDIMENTS=2), opt-in and off by default: when
# False the converter emits no sediment files and INPUT_CL29 keeps MODEL_SEDIMENTS=0,
# so the baseline stays byte-identical. See
# docs/superpowers/specs/2026-07-08-cl29-sediment-diagenesis-phase1-design.md.
CL29_ENABLE_SEDIMENTS = False
# Sediment carbonate ICs. None = use the template's values (INORG_C/TOT_ALK ~0.003,
# a physically realistic pore-water DIC). If the staged run's CO2SYS hard-stops with
# 'pH does not converge', set to (INORG_C, TOT_ALK) ~= (3.0, 3.1): the inflated
# magnitude this codebase's CO2SYS empirically needs (the pelagic uses 3.0/3.1). The
# two readings conflict; the run decides. See spec section 5.
CL29_SED_CARBONATE_IC = None
```

- [ ] **Step 4: Add the helpers and writer**

Immediately before `def _write_input_txt(` (~line 483), add:

```python
def _replace_leading_number(line, new_val):
    """Replace a value line's numeric content with new_val, preserving indent + EOL."""
    eol = "\r\n" if line.endswith("\r\n") else "\n"
    stripped = line.rstrip("\r\n")
    indent = stripped[:len(stripped) - len(stripped.lstrip())]
    return f"{indent}{new_val}{eol}"


def _sed_ic_block_bounds(lines):
    """(start, end) line indices of the 24-row sediment INITIAL CONDITIONS data block."""
    for i, ln in enumerate(lines):
        if ln.lstrip().startswith("# INITIAL CONDITIONS"):
            start = i + 1
            while start < len(lines) and (
                    not lines[start].strip() or lines[start].lstrip().startswith("#")):
                start += 1
            return start, start + 24
    raise SystemExit("sediment template missing '# INITIAL CONDITIONS'")


def _override_sed_carbonate(lines, inorg_c, tot_alk, nlayers=7):
    """Overwrite INORG_C (var 13) and TOT_ALK (var 14) IC rows with nlayers copies."""
    start, _ = _sed_ic_block_bounds(lines)
    eol = "\r\n" if lines[start].endswith("\r\n") else "\n"
    def row(val):
        return " ".join(f"{val:.6f}" for _ in range(nlayers)) + eol
    lines[start + 12] = row(inorg_c)   # sediment state var 13 = INORG_C
    lines[start + 13] = row(tot_alk)   # sediment state var 14 = TOT_ALK
    return lines


def _write_sediment_inputs(out, enable_sediments):
    """Phase-1 sediment stand-up. When enabled, copy the 170-constant W_SED_CONST.txt
    verbatim and author BOTTOM_SEDIMENT_MODEL_INPUT.txt from the template with
    advanced-redox forced to 0 and an optional carbonate-IC override. No-op otherwise."""
    if not enable_sediments:
        return
    shutil.copy(os.path.join(REPO, "INPUTS", "W_SED_CONST.txt"),
                os.path.join(out, "W_SED_CONST.txt"))
    with open(os.path.join(REPO, "INPUTS", "BOTTOM_SEDIMENT_MODEL_INPUT.txt"),
              newline="") as fh:
        lines = fh.readlines()                       # newline="" preserves CRLF
    for i, ln in enumerate(lines):                   # force ADVANCED REDOX -> 0
        if ln.lstrip().startswith("# ADVANCED REDOX SIMULATION"):
            lines[i + 1] = _replace_leading_number(lines[i + 1], 0)
            break
    else:
        raise SystemExit("sediment template missing '# ADVANCED REDOX SIMULATION'")
    if CL29_SED_CARBONATE_IC is not None:
        lines = _override_sed_carbonate(lines, *CL29_SED_CARBONATE_IC)
    with open(os.path.join(out, "BOTTOM_SEDIMENT_MODEL_INPUT.txt"), "w",
              newline="") as fh:
        fh.writelines(lines)
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cd /home/razinka/AQUABCv0.2 && python -m pytest tests/python/test_sediment_inputs.py -v`
Expected: PASS (all 5 tests in `TestWriteSedimentInputs`).

- [ ] **Step 6: Commit**

```bash
cd /home/razinka/AQUABCv0.2
git add tools/eutropy_poc/eutropy_to_estas.py tests/python/test_sediment_inputs.py
git commit -m "feat(tools): CL29 sediment input generator (Phase 1, opt-in)"
```

---

### Task 2: Converter — wire the toggle into INPUT_CL29.txt + main()

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py` (`_write_input_txt` ~484-503; `main()` ~268-269)
- Test: `tests/python/test_sediment_inputs.py`

**Interfaces:**
- Consumes: `CL29_ENABLE_SEDIMENTS`, `_write_sediment_inputs` (Task 1).
- Produces: `_write_input_txt(repo: str, tdays: list, enable_sediments: bool = False) -> None` writing `repo/INPUT_CL29.txt`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/python/test_sediment_inputs.py`:

```python
class TestWriteInputTxt:
    def _write(self, tmp_path, enable):
        conv._write_input_txt(str(tmp_path), [0, 1826], enable_sediments=enable)
        return (tmp_path / "INPUT_CL29.txt").read_text()

    def test_enabled_uses_layout_2(self, tmp_path):
        t = self._write(tmp_path, True)
        assert "# MODEL_SEDIMENTS\n          2\n" in t
        assert "# BOTTOM SEDIMENT MODEL INPUT FILE\nBOTTOM_SEDIMENT_MODEL_INPUT.txt\n" in t
        assert "NUM_PRESCRIBED_SEDIMENT_FLUX_SETS" not in t   # must be absent under ==2
        assert f"{240:15d}\n" in t                            # PRINT_INTERVAL 240

    def test_disabled_matches_baseline(self, tmp_path):
        t = self._write(tmp_path, False)
        assert "# MODEL_SEDIMENTS\n          0\n" in t
        assert "# NUM_PRESCRIBED_SEDIMENT_FLUX_SETS\n          0\n" in t
        assert "# SEDIMENT MODEL INPUT FILE\n" in t
        assert f"{10:15d}\n" in t                             # PRINT_INTERVAL 10
        assert "MODEL_SEDIMENTS\n          2" not in t
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd /home/razinka/AQUABCv0.2 && python -m pytest tests/python/test_sediment_inputs.py::TestWriteInputTxt -v`
Expected: FAIL — `TypeError: _write_input_txt() got an unexpected keyword argument 'enable_sediments'`.

- [ ] **Step 3: Edit `_write_input_txt` signature + PRINT_INTERVAL + sediment block**

Change the signature (line 484):

```python
def _write_input_txt(repo, tdays, enable_sediments=False):
```

Replace the PRINT_INTERVAL line (currently line 494 `fh.write("# PRINT_INTERVAL IN TIME STEPS\n             10\n")`) with:

```python
        fh.write(f"# PRINT_INTERVAL IN TIME STEPS\n{240 if enable_sediments else 10:15d}\n")
```

Replace the sediment block (currently lines 500-503) with:

```python
        fh.write("# RESUSPENSION_OPTION\n          0\n")
        if enable_sediments:
            fh.write("# MODEL_SEDIMENTS\n          2\n")
            fh.write("# BOTTOM SEDIMENT MODEL INPUT FILE\n")
            fh.write("BOTTOM_SEDIMENT_MODEL_INPUT.txt\n")
        else:
            fh.write("# MODEL_SEDIMENTS\n          0\n")
            fh.write("# NUM_PRESCRIBED_SEDIMENT_FLUX_SETS\n          0\n")
            fh.write("# SEDIMENT MODEL INPUT FILE\n")
```

(Note: `f"{10:15d}"` == the original `"             10"`, so the disabled path is byte-identical.)

- [ ] **Step 4: Wire into `main()`**

At `main()` (~line 268-269), change:

```python
    _write_master(OUT, state_block, links, depth, area)
    _write_input_txt(REPO, tdays)
```

to:

```python
    _write_master(OUT, state_block, links, depth, area)
    _write_sediment_inputs(OUT, CL29_ENABLE_SEDIMENTS)
    _write_input_txt(REPO, tdays, CL29_ENABLE_SEDIMENTS)
```

- [ ] **Step 5: Run the full converter test file**

Run: `cd /home/razinka/AQUABCv0.2 && python -m pytest tests/python/test_sediment_inputs.py -v`
Expected: PASS (all tests, both classes).

- [ ] **Step 6: Regression — sediments-off baseline byte-identical**

```bash
cd /home/razinka/AQUABCv0.2
cp INPUT_CL29.txt /tmp/INPUT_CL29.before
python3 tools/eutropy_poc/eutropy_to_estas.py >/dev/null
diff /tmp/INPUT_CL29.before INPUT_CL29.txt && echo "BASELINE BYTE-IDENTICAL"
```
Expected: `BASELINE BYTE-IDENTICAL` (no diff — `CL29_ENABLE_SEDIMENTS` is `False`).

- [ ] **Step 7: Commit**

```bash
cd /home/razinka/AQUABCv0.2
git add tools/eutropy_poc/eutropy_to_estas.py tests/python/test_sediment_inputs.py
git commit -m "feat(tools): wire MODEL_SEDIMENTS=2 toggle into INPUT_CL29 (Phase 1)"
```

---

### Task 3: Staged stability bring-up (short run + carbonate resolution)

Operational task — validated by running, not unit tests. Uses a temporary local edit to enable the toggle; **do not commit the toggle as True**.

**Files:** temporary local edits to `tools/eutropy_poc/eutropy_to_estas.py` (toggle, carbonate IC) and `INPUT_CL29.txt` (short window). None committed here.

- [ ] **Step 1: Build with OpenMP**

```bash
cd /home/razinka/AQUABCv0.2
make OPENMP=1 build-lib && make OPENMP=1 build-estas
ls -la ESTAS_II
```
Expected: `Executable 'ESTAS_II' created successfully`, exit 0.

- [ ] **Step 2: Enable sediments + regenerate inputs**

Temporarily set `CL29_ENABLE_SEDIMENTS = True` in the converter, then:

```bash
cd /home/razinka/AQUABCv0.2
python3 tools/eutropy_poc/eutropy_to_estas.py
ls INPUTS_CL29/W_SED_CONST.txt INPUTS_CL29/BOTTOM_SEDIMENT_MODEL_INPUT.txt
grep -A1 "MODEL_SEDIMENTS" INPUT_CL29.txt | head -2
```
Expected: both sediment files present; `MODEL_SEDIMENTS` = `2`.

- [ ] **Step 3: Shorten the run window to ~60 days**

Edit `INPUT_CL29.txt`: set `# SIMULATION_END` value to `60.0` (from `1826.0`). Create the output dir and clear old output:

```bash
cd /home/razinka/AQUABCv0.2
mkdir -p OUTPUTS_CL29 && rm -f OUTPUTS_CL29/*.out
```

- [ ] **Step 4: Run the short simulation (timed)**

```bash
cd /home/razinka/AQUABCv0.2
/usr/bin/time -v env ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt \
  > /tmp/run_sed_short.log 2>&1; echo "rc=$?"
tail -3 /tmp/run_sed_short.log
```
Expected: either "simulation finished" (rc=0), or a CO2SYS stop (next step).

- [ ] **Step 5: Resolve the carbonate ICs (empirical fork)**

```bash
grep -ic "pH does not converge" /tmp/run_sed_short.log
```
- If `0` and the run finished: carbonate ICs are fine — leave `CL29_SED_CARBONATE_IC = None`.
- If `≥1`: set `CL29_SED_CARBONATE_IC = (3.0, 3.1)` in the converter, then repeat Steps 2–4 (regenerate + rerun). Expected after the fix: no `'pH does not converge'`, run finishes. Record which branch was taken (this decides the committed default in Task 5).

- [ ] **Step 6: Check no silent negative-mass collapse**

```bash
cd /home/razinka/AQUABCv0.2
python3 - <<'PY'
vals = []
with open("OUTPUTS_CL29/BOTTOM_SEDIMENTS_OUTPUTS.out") as fh:
    for line in fh:
        p = line.split()
        vals += [float(x) for x in p if x not in ("", "\n")]
nonneg = [v for v in vals if v >= 0]
zeros = sum(1 for v in nonneg if v == 0.0)
print(f"cells={len(nonneg)} zero-pinned={zeros} frac={zeros/max(1,len(nonneg)):.3f}")
print("PASS" if zeros/max(1,len(nonneg)) < 0.5 else "FAIL: pools collapsing to floor")
PY
```
Expected: `PASS` (most cells non-zero; a broad collapse to 0 means the clamp is masking instability — investigate before extending).

- [ ] **Step 7: Extrapolate runtime and gate the full run**

From Step 4's `/usr/bin/time` wall-clock for 60 days, project the 1826-day run: `full ≈ short × (1826 / 60)`. Record it. Proceed to Task 4 only if the projection fits the agreed budget (else revisit `OPENMP` thread count / `PRINT_INTERVAL` / a decision). No commit in this task.

---

### Task 4: Full 5-year run + output-based stability & flux gates

Operational task. Assumes the toggle (and any carbonate override) from Task 3 are still set locally.

**Files:** temporary local `INPUT_CL29.txt` (full window). None committed.

- [ ] **Step 1: Restore the full window and regenerate**

Set `# SIMULATION_END` back to `1826.0` in `INPUT_CL29.txt` (or re-run the converter, which writes `1826.0`):

```bash
cd /home/razinka/AQUABCv0.2
python3 tools/eutropy_poc/eutropy_to_estas.py   # toggle still True locally
rm -f OUTPUTS_CL29/*.out
```

- [ ] **Step 2: Launch the full run (background — hours)**

```bash
cd /home/razinka/AQUABCv0.2
env ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt > /tmp/run_sed_full.log 2>&1 &
echo "pid=$!"
```
Poll `tail -2 /tmp/run_sed_full.log` and `wc -l OUTPUTS_CL29/PELAGIC_BOX_00019.out` until it prints "simulation finished".

- [ ] **Step 3: Gate — completion, no NaN, no CO2SYS stop**

```bash
cd /home/razinka/AQUABCv0.2
grep -c "simulation finished" /tmp/run_sed_full.log
grep -ic "pH does not converge" /tmp/run_sed_full.log
grep -icE "NaN|Infinity" /tmp/run_sed_full.log
```
Expected: `1`, `0`, `0`.

- [ ] **Step 4: Gate — sediment pools not collapsed (full run)**

Re-run the Step-6 script from Task 3 against the full `OUTPUTS_CL29/BOTTOM_SEDIMENTS_OUTPUTS.out`. Expected: `PASS`.

- [ ] **Step 5: Gate — benthic P/N return is a finite, plausible source (post-year-1)**

The sediment source manifests as a water-column PO4/NH4 increase versus the sediments-off baseline. Compare box-19 annual-mean PO4/NH4 in the last year (days 1461–1826) with and without sediments (the sediments-off `OUTPUTS_CL29` from Task 2 Step 6, or a fresh off run):

```bash
cd /home/razinka/AQUABCv0.2
python3 - <<'PY'
def mean_last_year(path, col):
    xs = []
    for line in open(path):
        p = line.split()
        if len(p) < 4:
            continue
        try:
            t = float(p[0]); v = float(p[col-1])
        except ValueError:
            continue
        if 1461 <= t <= 1826:
            xs.append(v)
    return sum(xs)/len(xs) if xs else float("nan")
# col 4 = PO4_P, col 2 = NH4_N in PELAGIC_BOX output
print("box19 last-yr mean PO4:", mean_last_year("OUTPUTS_CL29/PELAGIC_BOX_00019.out", 4))
print("box19 last-yr mean NH4:", mean_last_year("OUTPUTS_CL29/PELAGIC_BOX_00019.out", 2))
PY
```
Expected: finite, non-NaN, and **≥** the sediments-off values (sediment is a source, not a sink) — an order-of-magnitude-plausible increase, not a blow-up. (Do **not** read `SEDIMENT_FLUX_OUTPUTS.out` — it has a last-box write bug.) Record the numbers for the Phase-2 calibration baseline. No commit.

---

### Task 5: Finalize — restore off-by-default, verify baseline & clean checkout, document

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py` (toggle back to `False`; keep any `CL29_SED_CARBONATE_IC` decision recorded in the comment)
- Modify: `docs/superpowers/specs/2026-07-08-cl29-sediment-diagenesis-phase1-design.md` (record measured runtime + which carbonate branch was needed)

- [ ] **Step 1: Restore off-by-default**

Set `CL29_ENABLE_SEDIMENTS = False`. If Task 3 needed the carbonate override, leave `CL29_SED_CARBONATE_IC = (3.0, 3.1)` **and** update its comment to state it was empirically required; otherwise leave `None`.

- [ ] **Step 2: Regenerate + confirm byte-identical baseline**

```bash
cd /home/razinka/AQUABCv0.2
python3 tools/eutropy_poc/eutropy_to_estas.py >/dev/null
diff /tmp/INPUT_CL29.before INPUT_CL29.txt && echo "BASELINE BYTE-IDENTICAL"
test ! -e INPUTS_CL29/BOTTOM_SEDIMENT_MODEL_INPUT.txt && echo "no sediment files when off"
```
Expected: `BASELINE BYTE-IDENTICAL` and `no sediment files when off`.

- [ ] **Step 3: Run the unit tests + full Python suite**

```bash
cd /home/razinka/AQUABCv0.2
python -m pytest tests/python/test_sediment_inputs.py -v
python -m pytest tests/python/ -q
```
Expected: all pass.

- [ ] **Step 4: Clean-checkout end-to-end (fresh clone, sediments on)**

```bash
SCRATCH=/tmp/cl29-sed-clone; rm -rf "$SCRATCH"
git clone --quiet /home/razinka/AQUABCv0.2 "$SCRATCH"
cd "$SCRATCH"
sed -i 's/^CL29_ENABLE_SEDIMENTS = False/CL29_ENABLE_SEDIMENTS = True/' tools/eutropy_poc/eutropy_to_estas.py
make OPENMP=1 build-lib && make OPENMP=1 build-estas
python3 tools/eutropy_poc/eutropy_to_estas.py
mkdir -p OUTPUTS_CL29
# short smoke: set SIMULATION_END to 60.0 then run
python3 - <<'PY'
import re; p="INPUT_CL29.txt"; s=open(p).read()
open(p,"w").write(re.sub(r"(# SIMULATION_END\n)\s*1826\.0", r"\g<1>           60.0", s))
PY
env ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt > /tmp/run_sed_clone.log 2>&1; echo "rc=$?"
grep -ic "pH does not converge" /tmp/run_sed_clone.log
```
Expected: `rc=0` (or "simulation finished"), CO2SYS-stop count `0`. Confirms the pipeline stands up from a clean clone.

- [ ] **Step 5: Record runtime + carbonate outcome in the spec**

Add a short "Phase-1 results" note to `docs/superpowers/specs/2026-07-08-cl29-sediment-diagenesis-phase1-design.md`: measured short-run wall-time + full-run projection/actual, whether the carbonate override was needed, and the box-19 last-year PO4/NH4 with-vs-without numbers (the Phase-2 baseline).

- [ ] **Step 6: Commit**

```bash
cd /home/razinka/AQUABCv0.2
git add tools/eutropy_poc/eutropy_to_estas.py docs/superpowers/specs/2026-07-08-cl29-sediment-diagenesis-phase1-design.md
git commit -m "feat(tools): CL29 sediment diagenesis Phase 1 stands up + stabilizes (off by default)"
```

---

## Self-Review

**Spec coverage:** §4 converter changes → Tasks 1–2. §5 carbonate empirical fork → Task 3 Step 5; negative-mass output gate → Task 3 Step 6 / Task 4 Step 4; depth-division check is implicitly covered by the no-NaN gate (Task 4 Step 3) and HOLD_VOLUME. §6 output/runtime → Task 3 Steps 4/7 (PRINT_INTERVAL 240 set by Task 2; OpenMP build Task 3 Step 1). §7 tests → Tasks 1–2 (unit), Tasks 3–4 (stability), Task 5 Step 4 (clean-checkout); sane-flux exit → Task 4 Step 5. §8 success criteria → Task 4 gates + Task 5. §10 realism caveats are documented in the spec (no code). All covered.

**Placeholder scan:** No TBD/vague steps; every code step has complete code; every run step has exact commands + expected output. The literature flux band is a spec-level Phase-2 concern, not a plan step. OK.

**Type consistency:** `_write_sediment_inputs(out, enable_sediments)`, `_write_input_txt(repo, tdays, enable_sediments=False)`, `_sed_ic_block_bounds(lines)->(start,end)`, `_override_sed_carbonate(lines, inorg_c, tot_alk, nlayers=7)`, `_replace_leading_number(line, new_val)` — names/signatures consistent across tasks and tests. `CL29_ENABLE_SEDIMENTS` / `CL29_SED_CARBONATE_IC` used consistently.
