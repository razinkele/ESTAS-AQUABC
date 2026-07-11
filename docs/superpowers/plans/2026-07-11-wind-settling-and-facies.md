# Wind-modulated diatom settling (#3) + provisional facies aid (#5) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the hand-tuned `CL29_DIATOM_SETTLING=0.1` constant with a wind-driven, time-varying diatom settling velocity `w_eff(t) = w0/(1+(U/U_c)²)` written from ERA5 daily wind, and add a provisional (inert) sediment-facies strawman — all converter-only, with a byte-identical fallback.

**Architecture:** ESTAS already reads settling velocities as time series (`TIME_SERIE`), so the entire change lives in the Python converter `tools/eutropy_poc/eutropy_to_estas.py`: new pure helpers compute per-day settling from a committed daily-wind CSV, and the settling-TS writer emits a 1827-day series for slot 1 (diatoms) when enabled, else the current 2-point constant (byte-identical). No Fortran changes.

**Tech Stack:** Python 3 (stdlib only: `csv`, `os`, `collections`), pytest. Fortran ESTAS binary for the final integration run.

## Global Constraints

- **Converter-only.** No changes to any `.f90` file. (Spec §Scope)
- **Byte-identical fallback.** When `CL29_WIND_RESUSPENSION=False` OR `net/wind_daily.csv` is absent, `SETTLING_VELOCITY_TS_1.txt` must be identical to today's output (2-point constant at `CL29_DIATOM_SETTLING`). (Spec §2.7)
- **Diatoms only.** Only settling slot 1 (`DIA_C`) is modulated; slots 2–6 are unchanged. (Spec §2.4)
- **Pinned parameters (do not re-tune in code):** `w0 = 0.3` m/day, `U_c = 4.21` m/s. (Spec §2.3)
- **Formula:** `w_eff = w0 / (1 + (U/U_c)²)`, `U` = daily-mean wind (m/s). No floor. (Spec §2.2)
- **Timeline:** model day 0..1826 = 2012-01-01..2016-12-31; the daily wind file is index-aligned (row d = day d). (Spec §2.6)
- **ERA5 attribution:** the committed `wind_daily.csv` must carry a "Contains modified Copernicus Climate Change Service information" header. (Spec §2.5)
- **#5 stays inert:** `CL29_SEDIMENT_TYPE` remains `{}` (CL29 byte-identical); the strawman is a separate, unused constant. (Spec §3.3)
- **Tests** live in `tests/python/`, load the converter via `importlib` as `conv` (see existing `tests/python/test_sediment_inputs.py`), and run with `pytest` from the repo root (`REPO = os.getcwd()`).

---

### Task 1: Pure wind→settling formula helper

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py` (add helper immediately after `write_ts`, which ends at ~line 335)
- Test: `tests/python/test_wind_settling.py` (create)

**Interfaces:**
- Produces: `wind_modulated_settling(wind, w0, uhalf) -> list[float]` — maps a list of daily wind speeds to per-day `w_eff = w0/(1+(U/uhalf)²)`.

- [ ] **Step 1: Write the failing test**

Create `tests/python/test_wind_settling.py`:

```python
"""Tests for CL29 wind-modulated diatom settling (#3)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "tools", "eutropy_poc", "eutropy_to_estas.py")
_SPEC = importlib.util.spec_from_file_location("eutropy_to_estas", _PATH)
conv = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(conv)   # REPO = os.getcwd() = repo root


class TestWindModulatedSettling:
    def test_calm_returns_w0(self):
        # U=0 -> w_eff = w0/(1+0) = w0
        assert conv.wind_modulated_settling([0.0], 0.3, 4.21) == [0.3]

    def test_half_at_uhalf(self):
        # U=U_c -> w_eff = w0/(1+1) = w0/2
        w = conv.wind_modulated_settling([4.21], 0.3, 4.21)[0]
        assert abs(w - 0.15) < 1e-12

    def test_strictly_decreasing(self):
        w = conv.wind_modulated_settling([0, 2, 4, 6, 8, 10], 0.3, 4.21)
        assert all(w[i] > w[i + 1] for i in range(len(w) - 1))

    def test_always_positive_and_bounded(self):
        w = conv.wind_modulated_settling([0, 5, 10, 20, 50], 0.3, 4.21)
        assert all(0.0 < x <= 0.3 for x in w)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `pytest tests/python/test_wind_settling.py -v`
Expected: FAIL — `AttributeError: module 'eutropy_to_estas' has no attribute 'wind_modulated_settling'`

- [ ] **Step 3: Write minimal implementation**

In `tools/eutropy_poc/eutropy_to_estas.py`, immediately after the `write_ts` function (after its last line, ~line 335), add:

```python
def wind_modulated_settling(wind, w0, uhalf):
    """Per-day net diatom settling velocity from daily wind (m/day).

    w_eff = w0 / (1 + (U/uhalf)**2)  -- inverse-quadratic in wind (bottom shear
    ~ U**2). Smooth, always positive, no floor. w0 is the calm-water (U->0) limit;
    settling is halved at U = uhalf. See docs/superpowers/specs/2026-07-11-*.
    """
    return [w0 / (1.0 + (u / uhalf) ** 2) for u in wind]
```

- [ ] **Step 4: Run test to verify it passes**

Run: `pytest tests/python/test_wind_settling.py -v`
Expected: PASS (4 passed)

- [ ] **Step 5: Commit**

```bash
git add tools/eutropy_poc/eutropy_to_estas.py tests/python/test_wind_settling.py
git commit -m "feat(cl29): pure wind-modulated settling formula (#3)"
```

---

### Task 2: Wind regeneration script + committed daily-wind artifact

**Files:**
- Create: `tools/eutropy_poc/make_wind_daily.py`
- Create: `tools/eutropy_poc/net/wind_daily.csv` (generated by the script, then committed)

**Interfaces:**
- Produces: `tools/eutropy_poc/net/wind_daily.csv` — comment header + `day,wind_ms`, 1827 data rows (day 0 = 2012-01-01), each `wind_ms` the daily mean of the hourly ERA5 `wind_ms` column.

- [ ] **Step 1: Write the regeneration script**

Create `tools/eutropy_poc/make_wind_daily.py`:

```python
#!/usr/bin/env python3
"""Regenerate net/wind_daily.csv (daily-mean 10 m wind) from hourly ERA5.

Source: ERA5 (Copernicus Climate Change Service / ECMWF), hourly wind_ms at Nida,
2012-2016. The committed net/wind_daily.csv is authoritative for conversion; this
script only rebuilds it. Usage:
    python3 tools/eutropy_poc/make_wind_daily.py [path-to-hourly-era5.csv]
"""
import csv
import os
import sys
from collections import OrderedDict

DEFAULT_SRC = os.path.expanduser("~/eutropy/input/era5_wind_nida_2012_2016.csv")
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "net", "wind_daily.csv")


def main():
    src = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_SRC
    daily = OrderedDict()  # date-string -> list of hourly wind_ms
    with open(src) as fh:
        for row in csv.DictReader(fh):
            day = row["time"][:10]          # 'YYYY-MM-DD'
            daily.setdefault(day, []).append(float(row["wind_ms"]))
    with open(OUT, "w") as fh:
        fh.write("# Daily-mean 10 m wind speed at Nida, 2012-2016 (m/s).\n")
        fh.write("# Source: ERA5 (Copernicus Climate Change Service / ECMWF).\n")
        fh.write("# Contains modified Copernicus Climate Change Service information.\n")
        fh.write("day,wind_ms\n")
        for i, (day, vals) in enumerate(daily.items()):
            fh.write(f"{i},{sum(vals) / len(vals):.4f}\n")
    print(f"wrote {OUT}: {len(daily)} days (day 0 = {next(iter(daily))})")


if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Generate the committed artifact**

Run: `python3 tools/eutropy_poc/make_wind_daily.py`
Expected: `wrote .../net/wind_daily.csv: 1827 days (day 0 = 2012-01-01)`

(If the ERA5 source is at a different path, pass it as the first argument.)

- [ ] **Step 3: Verify the artifact**

Run: `python3 -c "n=sum(1 for l in open('tools/eutropy_poc/net/wind_daily.csv') if l[:1].isdigit()); print('data rows:', n)"`
Expected: `data rows: 1827`

Run: `head -4 tools/eutropy_poc/net/wind_daily.csv`
Expected: three `#` comment lines (including "Contains modified Copernicus…") then `day,wind_ms`.

- [ ] **Step 4: Commit**

```bash
git add tools/eutropy_poc/make_wind_daily.py tools/eutropy_poc/net/wind_daily.csv
git commit -m "feat(cl29): committed daily-mean ERA5 wind artifact + regen script (#3)"
```

---

### Task 3: Wind-file reader helper

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py` (add `_read_wind_daily` after `wind_modulated_settling`)
- Test: `tests/python/test_wind_settling.py` (add a class)

**Interfaces:**
- Consumes: `net/wind_daily.csv` (Task 2)
- Produces: `_read_wind_daily(path=None) -> list[float] | None` — returns per-day wind (index = day), skipping `#` comments and the `day,wind_ms` header; returns `None` if the file is absent. Defaults to `NET/wind_daily.csv`.

- [ ] **Step 1: Write the failing test**

Append to `tests/python/test_wind_settling.py`:

```python
class TestReadWindDaily:
    def test_committed_file_present_and_sized(self):
        wind = conv._read_wind_daily()
        assert wind is not None
        assert len(wind) == 1827          # 2012-01-01 .. 2016-12-31
        assert all(w > 0 for w in wind)

    def test_absent_file_returns_none(self, tmp_path):
        missing = str(tmp_path / "nope.csv")
        assert conv._read_wind_daily(missing) is None

    def test_skips_comments_and_header(self, tmp_path):
        p = tmp_path / "wind_daily.csv"
        p.write_text(
            "# comment\n# Contains modified Copernicus\nday,wind_ms\n0,5.0\n1,6.5\n"
        )
        assert conv._read_wind_daily(str(p)) == [5.0, 6.5]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `pytest tests/python/test_wind_settling.py::TestReadWindDaily -v`
Expected: FAIL — `AttributeError: ... has no attribute '_read_wind_daily'`

- [ ] **Step 3: Write minimal implementation**

In `tools/eutropy_poc/eutropy_to_estas.py`, after `wind_modulated_settling`, add:

```python
def _read_wind_daily(path=None):
    """Read committed daily-mean wind (day,wind_ms). Returns per-day wind list
    (index = day), or None if the file is absent. Skips '#' comments and header."""
    if path is None:
        path = os.path.join(NET, "wind_daily.csv")
    if not os.path.exists(path):
        return None
    wind = []
    with open(path) as fh:
        for ln in fh:
            ln = ln.strip()
            if not ln or ln.startswith("#") or ln.lower().startswith("day"):
                continue
            wind.append(float(ln.split(",")[1]))
    return wind
```

- [ ] **Step 4: Run test to verify it passes**

Run: `pytest tests/python/test_wind_settling.py::TestReadWindDaily -v`
Expected: PASS (3 passed)

- [ ] **Step 5: Commit**

```bash
git add tools/eutropy_poc/eutropy_to_estas.py tests/python/test_wind_settling.py
git commit -m "feat(cl29): daily-wind reader helper (#3)"
```

---

### Task 4: Config constants + settling-TS writer (wind mode + byte-identical fallback)

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py`
  - Add 3 config constants after `CL29_DIATOM_SETTLING` (~line 94)
  - Add `_write_settling_velocity_files(out)` (near the other writers, e.g. after `_read_wind_daily`)
  - Replace the inline settling block in `_write_master` (lines 674–680) with a call
- Test: `tests/python/test_wind_settling.py` (add a class)

**Interfaces:**
- Consumes: `wind_modulated_settling` (Task 1), `_read_wind_daily` (Task 3), `CL29_DIATOM_SETTLING`
- Produces:
  - constants `CL29_WIND_RESUSPENSION: bool`, `CL29_SETTLING_W0: float`, `CL29_WIND_UHALF: float`
  - `_write_settling_velocity_files(out)` — writes `SETTLING_VELOCITY_TS_1..6.txt` into `out`. Slot 1 is a wind-modulated daily series (`1827` daily rows + one sentinel row at day `9999` holding the last value → `1828` rows) when resuspension is enabled and wind is present; otherwise the 2-point constant. Slots 2–6 are always the 2-point constants `[0.1, 0.05, 1.0, 0.5, 0.3]`.

- [ ] **Step 1: Add the config constants**

In `tools/eutropy_poc/eutropy_to_estas.py`, immediately after the line `CL29_DIATOM_SETTLING = 0.1` (~line 94), add:

```python
# Wind-modulated diatom settling (#3). When enabled, slot-1 (DIA_C) settling is
# written as a daily series w_eff(t) = CL29_SETTLING_W0 / (1 + (U(t)/CL29_WIND_UHALF)**2)
# from net/wind_daily.csv (ERA5). Disabled -> constant CL29_DIATOM_SETTLING (byte-identical).
# Params pinned per spec 2026-07-11 §2.3 (do NOT re-tune here): w0=0.3 (physical), U_c=4.21
# (half-suppression wind; fitted so annual-mean w_eff ~= 0.1, the validated level). Aseasonal
# wind here means this preserves rather than changes the bloom -- it is a defensibility change.
CL29_WIND_RESUSPENSION = True
CL29_SETTLING_W0       = 0.3
CL29_WIND_UHALF        = 4.21
```

- [ ] **Step 2: Write the failing test**

Append to `tests/python/test_wind_settling.py`:

```python
def _read_ts_values(path):
    """Return the list of first-column values from an ESTAS TS file
    (rows after the '# TIME AND VALUES' marker)."""
    vals = []
    started = False
    with open(path) as fh:
        for ln in fh:
            if started:
                parts = ln.split()
                if len(parts) >= 2:
                    vals.append(float(parts[1]))
            elif ln.startswith("# TIME AND VALUES"):
                started = True
    return vals


class TestWriteSettlingVelocityFiles:
    def test_wind_mode_writes_daily_series(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_WIND_RESUSPENSION", True)
        conv._write_settling_velocity_files(str(tmp_path))
        v1 = _read_ts_values(str(tmp_path / "SETTLING_VELOCITY_TS_1.txt"))
        assert len(v1) == 1828                      # 1827 daily + 1 sentinel
        wind = conv._read_wind_daily()
        expected0 = conv.wind_modulated_settling(
            wind[:1], conv.CL29_SETTLING_W0, conv.CL29_WIND_UHALF)[0]
        assert abs(v1[0] - expected0) < 1e-6        # day 0 matches the formula
        assert v1[-1] == v1[-2]                      # sentinel repeats last value
        # slots 2-6 stay 2-point constants
        v2 = _read_ts_values(str(tmp_path / "SETTLING_VELOCITY_TS_2.txt"))
        assert v2 == [0.1, 0.1]

    def test_fallback_is_constant(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_WIND_RESUSPENSION", False)
        conv._write_settling_velocity_files(str(tmp_path))
        v1 = _read_ts_values(str(tmp_path / "SETTLING_VELOCITY_TS_1.txt"))
        assert v1 == [conv.CL29_DIATOM_SETTLING, conv.CL29_DIATOM_SETTLING]  # 2 rows

    def test_fallback_byte_identical_to_legacy(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_WIND_RESUSPENSION", False)
        conv._write_settling_velocity_files(str(tmp_path))
        got = (tmp_path / "SETTLING_VELOCITY_TS_1.txt").read_text()
        ref = tmp_path / "ref.txt"
        conv.write_ts(str(ref), "settling velocity 1 m/day", [0, 9999], [[0.1], [0.1]])
        assert got == ref.read_text()
```

- [ ] **Step 3: Run test to verify it fails**

Run: `pytest tests/python/test_wind_settling.py::TestWriteSettlingVelocityFiles -v`
Expected: FAIL — `AttributeError: ... has no attribute '_write_settling_velocity_files'`

- [ ] **Step 4: Write the implementation**

In `tools/eutropy_poc/eutropy_to_estas.py`, add the writer after `_read_wind_daily`:

```python
def _write_settling_velocity_files(out):
    """Write SETTLING_VELOCITY_TS_1..6.txt. Slot 1 (DIA_C) is a wind-modulated daily
    series when CL29_WIND_RESUSPENSION and wind data are present; otherwise the 2-point
    constant CL29_DIATOM_SETTLING (byte-identical to legacy). Slots 2-6 are constants."""
    vels = [CL29_DIATOM_SETTLING, 0.1, 0.05, 1.0, 0.5, 0.3]  # slot1=DIA_C
    wind = _read_wind_daily() if CL29_WIND_RESUSPENSION else None
    for i, v in enumerate(vels, start=1):
        if i == 1 and wind:
            w = wind_modulated_settling(wind, CL29_SETTLING_W0, CL29_WIND_UHALF)
            days = list(range(len(w))) + [9999]      # sentinel holds last value past sim end
            cols = [[x] for x in w] + [[w[-1]]]
            write_ts(os.path.join(out, "SETTLING_VELOCITY_TS_1.txt"),
                     "settling velocity 1 m/day (wind-modulated DIA_C)", days, cols)
        else:
            write_ts(os.path.join(out, f"SETTLING_VELOCITY_TS_{i}.txt"),
                     f"settling velocity {i} m/day", [0, 9999], [[v], [v]])
```

Then in `_write_master` replace the inline block (lines 674–680):

```python
    # settling velocity TS files (constant velocities, m/day)
    # #3 is OPA_C only: reduced 0.2 -> 0.05 (motile green algae) so OPA is not sunk
    # out of its narrow clear-water-phase window before it can accumulate.
    vels = [CL29_DIATOM_SETTLING, 0.1, 0.05, 1.0, 0.5, 0.3]  # slot1=DIA_C (see CL29_DIATOM_SETTLING)
    for i, v in enumerate(vels, start=1):
        write_ts(os.path.join(out, f"SETTLING_VELOCITY_TS_{i}.txt"),
                 f"settling velocity {i} m/day", [0, 9999], [[v], [v]])
```

with:

```python
    # settling velocity TS files (m/day); slot 1 (DIA_C) is wind-modulated (#3),
    # slots 2-6 constant. See _write_settling_velocity_files / CL29_WIND_RESUSPENSION.
    _write_settling_velocity_files(out)
```

- [ ] **Step 5: Run test to verify it passes**

Run: `pytest tests/python/test_wind_settling.py::TestWriteSettlingVelocityFiles -v`
Expected: PASS (3 passed)

- [ ] **Step 6: Commit**

```bash
git add tools/eutropy_poc/eutropy_to_estas.py tests/python/test_wind_settling.py
git commit -m "feat(cl29): wind-modulated slot-1 settling writer with constant fallback (#3)"
```

---

### Task 5: Provisional sediment-facies strawman + decision aid (#5)

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py` (at the `CL29_SEDIMENT_TYPE = {}` line, ~line 270: add an inert `CL29_SEDIMENT_TYPE_PROVISIONAL` constant + decision-aid comment; keep `CL29_SEDIMENT_TYPE = {}`)
- Test: `tests/python/test_wind_settling.py` (add a class)

**Interfaces:**
- Produces: `CL29_SEDIMENT_TYPE_PROVISIONAL: dict[int, str]` — 29 entries, values in `{"sandy","muddy"}`, box 19 = `"muddy"`. **Unused** by the converter (inert); the expert activates it later by assigning it to `CL29_SEDIMENT_TYPE`.

- [ ] **Step 1: Write the failing test**

Append to `tests/python/test_wind_settling.py`:

```python
class TestFaciesStrawman:
    def test_active_map_stays_empty(self):
        # #5 stays inert -> CL29 byte-identical until the expert confirms a map.
        assert conv.CL29_SEDIMENT_TYPE == {}

    def test_provisional_covers_all_boxes(self):
        p = conv.CL29_SEDIMENT_TYPE_PROVISIONAL
        assert set(p) == set(range(1, 30))               # all 29 boxes
        assert set(p.values()) <= {"sandy", "muddy"}
        assert p[19] == "muddy"                          # interior muddy exemplar
        # marine-influenced boxes are sandy (spec §3.2)
        for b in (1, 4, 7, 10, 11, 12, 13, 16, 20, 22):
            assert p[b] == "sandy"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `pytest tests/python/test_wind_settling.py::TestFaciesStrawman -v`
Expected: FAIL — `AttributeError: ... has no attribute 'CL29_SEDIMENT_TYPE_PROVISIONAL'`

- [ ] **Step 3: Write the implementation**

In `tools/eutropy_poc/eutropy_to_estas.py`, immediately after the `CL29_SEDIMENT_TYPE = {}` line (~line 270), add:

```python
# --- #5 PROVISIONAL sediment-facies strawman (INERT; not wired in) ------------------
# The active map above stays EMPTY so CL29 is byte-identical until an authoritative
# per-box facies map is supplied. This strawman is a LOW-CONFIDENCE starting point for
# the domain expert to correct, derived from the salinity split (forcing_salt.csv):
# marine-influenced boxes (sal ~6-7, N/strait, higher energy) -> sandy; freshwater +
# transitional (sal ~0.1-3.7, central/southern low-energy basin) -> muddy. It is
# demonstrably imperfect (freshwater boxes 5,6,8,9 are labelled 'sand' in the analysis
# BOX_TYPES; freshwater != muddy) and conflicts with those (mutually inconsistent, unsourced)
# labels. To activate two-type authoring later: CL29_SEDIMENT_TYPE = CL29_SEDIMENT_TYPE_PROVISIONAL
# (after expert review). See docs/superpowers/specs/2026-07-11-*.md §3.
_SANDY = (1, 4, 7, 10, 11, 12, 13, 16, 20, 22)               # marine-influenced
CL29_SEDIMENT_TYPE_PROVISIONAL = {
    b: ("sandy" if b in _SANDY else "muddy") for b in range(1, 30)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `pytest tests/python/test_wind_settling.py::TestFaciesStrawman -v`
Expected: PASS (2 passed)

- [ ] **Step 5: Commit**

```bash
git add tools/eutropy_poc/eutropy_to_estas.py tests/python/test_wind_settling.py
git commit -m "docs(cl29): inert provisional sediment-facies strawman + decision aid (#5)"
```

---

### Task 6: Integration validation (5-yr run) + CHANGELOG

**Files:**
- Modify: `CHANGELOG.md`
- (Run) converter regeneration + ESTAS 5-yr simulation

**Interfaces:**
- Consumes: everything from Tasks 1–5.

- [ ] **Step 1: Run the full Python test suite (no regressions)**

Run: `pytest tests/python/ -q`
Expected: all pass (including the new `test_wind_settling.py`).

- [ ] **Step 2: Regenerate the CL29 inputs**

Run: `python3 tools/eutropy_poc/eutropy_to_estas.py`
Expected: writes `INPUTS_CL29/` and `INPUT_CL29.txt`; prints the `[estas] run with: ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt` hint.

- [ ] **Step 3: Verify the generated slot-1 series**

Run: `sed -n '2,3p' INPUTS_CL29/SETTLING_VELOCITY_TS_1.txt`
Expected: `# DATA_SIZE` then `1828` (1827 daily rows + 1 sentinel).

Run: `grep -c . INPUTS_CL29/SETTLING_VELOCITY_TS_2.txt`
Expected: the slot-2 constant file is unchanged (small — header lines + 2 data rows), confirming only slot 1 became a series.

- [ ] **Step 4: Build ESTAS if the binary is stale**

Run: `make build-estas`
Expected: builds `ESTAS_II` with no errors. (Sediment path is not OpenMP-safe — build serial, no `OPENMP=1`.)

- [ ] **Step 5: Run the 5-yr simulation (background — it takes minutes)**

Run (from repo root): `ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt`
Expected: runs to completion (`simulation finished`), writing `OUTPUTS_CL29/PELAGIC_BOX_*.out`. Run in background if it exceeds the foreground time limit.

- [ ] **Step 6: Check the guardrails**

Using the column indices (0-based, col 0 = TIME): `DIA_C`=5, `CYN_C`=15, `FIX_CYN_C`=19, `NOST_VEG_HET_C`=31 in `OUTPUTS_CL29/PELAGIC_BOX_00019.out`, verify per spec §2.10:
- box-19 spring `DIA_C` peak in **1.1–1.9 mgC/L** (each year),
- summer domain cyano (`CYN_C+FIX_CYN_C+NOST_VEG_HET_C`) ≈ **96 mg Chl/m³** (≈2–4 mgC/L; ×25 mgChl per mgC/L for cyano),
- seasonal succession intact (spring diatoms → summer cyano),
- **0 NaN** anywhere in the output.

Run (example check):
```bash
python3 - <<'PY'
import numpy as np, glob
f='OUTPUTS_CL29/PELAGIC_BOX_00019.out'
d=np.loadtxt(f, skiprows=1)
t=d[:,0]
print("NaN count:", int(np.isnan(d).sum()))
for yr in range(5):
    m=(t>=yr*365+60)&(t<yr*365+150)   # ~spring window
    if m.any(): print(f"yr{yr} spring DIA peak:", round(float(d[m,5].max()),2))
PY
```
Expected: `NaN count: 0`; spring DIA peaks land in ~1.1–1.9; the result is close to the constant-0.1 baseline (per spec §1.1 the aseasonal wind should not move the bloom materially). If a guardrail fails, the spec §4 contingency is to move `U_c` within ≈3.5–5.0 m/s (→ mean 0.080–0.121) and re-run — a single knob, no code-structure change.

- [ ] **Step 7: Update CHANGELOG**

Add to `CHANGELOG.md` under the `Unreleased` (or next) section:

```markdown
### Added
- CL29: wind-modulated diatom settling (#3) — `SETTLING_VELOCITY_TS_1.txt` is now a
  daily series `w_eff = w0/(1+(U/U_c)²)` from ERA5 wind (`w0=0.3`, `U_c=4.21`), replacing
  the constant `CL29_DIATOM_SETTLING`. Off-switch `CL29_WIND_RESUSPENSION` restores the
  byte-identical constant. Committed `tools/eutropy_poc/net/wind_daily.csv` (ERA5/Copernicus).
- CL29: inert provisional sediment-facies strawman `CL29_SEDIMENT_TYPE_PROVISIONAL` (#5);
  active map stays empty pending an authoritative facies map.
```

- [ ] **Step 8: Commit**

```bash
git add CHANGELOG.md
git commit -m "test(cl29): 5-yr guardrail validation of wind-modulated settling + changelog (#3)"
```

---

## Notes for the executor

- **`INPUTS_CL29/` and `OUTPUTS_CL29/` are gitignored build artifacts** — do not commit them. Only the converter, the committed `net/wind_daily.csv`, tests, and `CHANGELOG.md` are tracked.
- **ESTAS output-folder gotcha:** the run reads the output folder line list-directed, so absolute paths starting with `/` silently fail — use the relative `OUTPUTS_CL29/` the converter already wires up.
- **If the ERA5 hourly source is missing** at run time, Task 2's regen fails but the *committed* `wind_daily.csv` is what the converter uses, so Tasks 3–6 still work from a clean clone; and if even the committed file were removed, `_read_wind_daily` returns `None` and the converter falls back to the byte-identical constant.
