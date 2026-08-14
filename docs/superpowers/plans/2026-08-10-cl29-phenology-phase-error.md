# CL29 Seasonal Phase Error — Measurement and Diagnosis Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

> **Revision note (2026-08-10, after adversarial review).** The first draft of this plan proposed a
> "seed floor" for the diazotroph groups, on the premise that competitive exclusion drives them to
> the global `MIN_CONCENTRATION` clamp of 1×10⁻¹⁰ and they cannot recover. **That premise was false
> and the task has been deleted.** Measured in `OUTPUTS`/`cal_report` full-record output:
> `FIX_CYN_C` never falls below **5.0×10⁻⁴** (box 23; 1.0–1.2×10⁻³ in boxes 7 and 14) — five million
> times above the clamp. `NOST_VEG_HET_C` does sit near zero out of season, but it germinates from
> the akinete pool on schedule every year (`AKI_C` 0.742 → 0.002 mg C/L through August) and the
> combined fixer biomass peaks in **August**, which is the correct month. **The failure is
> amplitude (~0.17 against ~2.0 mg C/L observed in summer), not timing or extinction** — and a
> concentration floor is a bound, not a rate, so it could not have fixed it. No Fortran change is
> specified here: the review demonstrated that the fix cannot be written before the diagnosis, so
> this plan delivers the measurement and the diagnosis, and the model change follows in a second
> plan.

**Goal:** Build the seasonal-phase metric the toolchain lacks, then determine *why* CL29 carries too much diatom biomass in winter and too little diazotroph biomass in summer.

**Architecture:** Two strands, both pure tooling. First a *measurement* strand: nothing can currently score "the model peaks in the wrong month", so we add that metric and it becomes the acceptance test for all later work. Then a *diagnosis* strand that needs no code change to the model at all — the growth-limitation factors and rate terms for every phytoplankton group are already exported to `PROCESS_RATES` and are merely switched off in the CL29 output configuration.

**Tech Stack:** Python 3.12 (pandas, numpy, pytest), gfortran-built `ESTAS_II` (run only, not modified), ruff.

## Global Constraints

- **No model source changes in this plan.** `SOURCE_CODE/` is not touched. Runs use the existing binary.
- **CL29 runs require `ESTAS_HOLD_VOLUME=1`** or box 18 drains and the run crashes near day 449.
- **Use `PRINT_INTERVAL=240` (daily)** for multi-year runs; the default of 10 writes tens of GB.
- **Python:** ruff-clean (`line-length = 120`, config in `pyproject.toml`); tests in `tests/python/`; run `python3 -m pytest tests/python -q`.
- **`NDIAGVAR = 30`** (`SOURCE_CODE/ESTAS/mod_GLOBAL.f90:32`) — process-rate columns per state variable. Layout is state-major, so a full row is `1 + 36×30 = 1081` fields.
- **State-variable indices** (`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_svindex.f90`, module name `aquabc_pel_state_var_indexes`): `DIA_C = 5`, `CYN_C = 15`, `OPA_C = 16`, `FIX_CYN_C = 19`, `FIX_CYN_HET_C`/`NOST_VEG_HET_C = 31`, `AKI_C = 32`. In `PELAGIC_BOX_*.out` the column is index + 1 because column 1 is `TIME_DAYS`.
- **Reference window is 2016–2021** — the period for which the calibrated satellite product exists. Observation climatologies must be restricted to the model's simulated window; 1712 of 2472 chlorophyll rows (69%) predate it.
- **Target values** (2016–2021 lagoon climatology, `docs/Satellite_model_pattern_comparison.md`, autumn = Aug–Oct ÷ spring = Feb–May): observed peak month **August**, observed autumn/spring **1.90**; shipped model scores autumn/spring **0.58** and seasonal r **−0.58**.

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `tools/seasonal_phase.py` (new) | Pure functions: phase metrics from {month: value} maps. No I/O. | 1 |
| `tests/python/test_seasonal_phase.py` (new) | Unit tests for those functions. | 1 |
| `tools/validate_cl29_vs_epa.py` (modify) | `--phase` flag printing the phase report, using the *windowed* observation climatology. | 1 |
| `INPUTS_CL29/PELAGIC_OUTPUT_INFORMATION_FILE.txt` (modify) | Enable process-rate output for four diagnostic boxes. | 2 |
| `tools/diagnose_group_limitation.py` (new) | Reads `PROCESS_RATES`; reports which limitation term fails to bind, for any phytoplankton group. | 2 |
| `docs/CL29_phenology_diagnosis.md` (new) | Findings and the decision that selects the follow-up plan. | 3 |

---

### Task 1: Seasonal-phase metrics

`validate_cl29_vs_epa.py --by-season` reports per-season bias but cannot express a phase error: a model can carry the right annual mean and plausible per-season magnitudes while peaking six months late. This task builds that metric, because it is the acceptance test for everything after.

**Files:**
- Create: `tools/seasonal_phase.py`
- Create: `tests/python/test_seasonal_phase.py`
- Modify: `tools/validate_cl29_vs_epa.py`

**Interfaces:**
- Consumes: nothing.
- Produces: `phase_metrics(model_by_month: dict[int, float], obs_by_month: dict[int, float]) -> dict` with keys `peak_model`, `peak_obs`, `peak_offset_months`, `autumn_spring_model`, `autumn_spring_obs`, `seasonal_r`, `n_months`; and `format_report(metrics: dict) -> str`. Task 3 reads the printed output only.

- [ ] **Step 1: Write the failing test**

Create `tests/python/test_seasonal_phase.py`:

```python
"""Unit tests for tools/seasonal_phase.py (pure functions, no I/O)."""
import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
from seasonal_phase import circular_month_offset, phase_metrics  # noqa: E402


def test_circular_month_offset_takes_the_short_way_round():
    # positive means the first month falls AFTER the second
    assert circular_month_offset(2, 8) == 6      # Feb is 6 months either way from Aug
    assert circular_month_offset(1, 12) == 1     # January is one month after December
    assert circular_month_offset(12, 1) == -1    # December is one month before January
    assert circular_month_offset(9, 9) == 0


def test_phase_metrics_detects_an_inverted_cycle():
    obs = dict(zip(range(1, 13), [10, 10, 20, 30, 30, 25, 30, 50, 48, 45, 25, 12]))
    model = dict(zip(range(1, 13), [50, 47, 42, 40, 42, 29, 27, 30, 22, 24, 27, 32]))
    r = phase_metrics(model, obs)
    assert r["peak_obs"] == 8
    assert r["peak_model"] == 1
    assert abs(r["peak_offset_months"]) >= 5
    assert r["seasonal_r"] < 0
    assert r["autumn_spring_obs"] > 1.0
    assert r["autumn_spring_model"] < 1.0
    assert r["n_months"] == 12


def test_phase_metrics_detects_a_matching_cycle():
    obs = dict(zip(range(1, 13), [10, 10, 20, 30, 30, 25, 30, 50, 48, 45, 25, 12]))
    model = {m: v * 1.2 for m, v in obs.items()}
    r = phase_metrics(model, obs)
    assert r["peak_offset_months"] == 0
    assert r["seasonal_r"] > 0.99
    assert r["autumn_spring_model"] == pytest.approx(r["autumn_spring_obs"])


def test_phase_metrics_uses_only_shared_months():
    obs = {3: 20.0, 4: 30.0, 8: 50.0, 9: 48.0}
    model = {m: float(m) for m in range(1, 13)}
    r = phase_metrics(model, obs)
    assert r["n_months"] == 4


def test_season_ratio_ignores_months_absent_from_the_other_series():
    # only Feb and Aug are shared; the ratio must not silently use model-only months
    obs = {2: 10.0, 8: 40.0, 9: 30.0}
    model = {m: 100.0 for m in range(1, 13)}
    r = phase_metrics(model, obs)
    assert r["autumn_spring_model"] == pytest.approx(1.0)   # flat model over shared months
    assert r["autumn_spring_obs"] == pytest.approx(3.5)     # (40+30)/2 / 10


def test_phase_metrics_needs_at_least_three_shared_months():
    with pytest.raises(ValueError, match="at least 3"):
        phase_metrics({1: 1.0, 2: 2.0}, {1: 1.0, 2: 2.0})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/test_seasonal_phase.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'seasonal_phase'`

- [ ] **Step 3: Write the implementation**

Create `tools/seasonal_phase.py`:

```python
#!/usr/bin/env python3
"""Seasonal-phase metrics for model-observation comparison.

Per-season bias cannot express a phase error: a model can carry the right annual
mean and plausible per-season magnitudes while peaking in the wrong month. These
metrics score the shape and timing of the seasonal cycle instead.

Pure functions over {month: value} mappings; no file or model I/O, so they are
usable from the validator, from analysis scripts, and from tests.
"""
from __future__ import annotations

import math

SPRING = (2, 3, 4, 5)
AUTUMN = (8, 9, 10)


def circular_month_offset(month_a: int, month_b: int) -> int:
    """Signed months from ``month_b`` to ``month_a``, taking the shorter way round.

    Positive means ``month_a`` falls after ``month_b``: January minus December is +1,
    not -11. December minus January is -1. The result lies in [-6, 6].
    """
    d = (month_a - month_b) % 12
    if d > 6:
        d -= 12
    return d


def _mean(values):
    values = [v for v in values]
    return sum(values) / len(values) if values else float("nan")


def _pearson(xs, ys):
    mx, my = _mean(xs), _mean(ys)
    sx = math.sqrt(sum((x - mx) ** 2 for x in xs))
    sy = math.sqrt(sum((y - my) ** 2 for y in ys))
    if sx == 0 or sy == 0:
        return float("nan")
    return sum((x - mx) * (y - my) for x, y in zip(xs, ys)) / (sx * sy)


def _season_ratio(by_month: dict, months: set) -> float:
    """Autumn mean over spring mean, restricted to ``months`` (the shared set)."""
    num = _mean(by_month[m] for m in AUTUMN if m in months and m in by_month)
    den = _mean(by_month[m] for m in SPRING if m in months and m in by_month)
    if math.isnan(den) or den == 0:
        return float("nan")
    return num / den


def phase_metrics(model_by_month: dict, obs_by_month: dict) -> dict:
    """Compare the seasonal *shape* of a model series against observations.

    Both inputs map month number (1-12) to a climatological mean. Only months present
    in both are used, for every metric including the season ratios, so the two series
    are always compared over identical support.
    """
    shared = sorted(set(model_by_month) & set(obs_by_month))
    if len(shared) < 3:
        raise ValueError(f"need at least 3 shared months, got {len(shared)}")
    shared_set = set(shared)
    peak_model = max(shared, key=lambda m: model_by_month[m])
    peak_obs = max(shared, key=lambda m: obs_by_month[m])
    return {
        "peak_model": peak_model,
        "peak_obs": peak_obs,
        "peak_offset_months": circular_month_offset(peak_model, peak_obs),
        "autumn_spring_model": _season_ratio(model_by_month, shared_set),
        "autumn_spring_obs": _season_ratio(obs_by_month, shared_set),
        "seasonal_r": _pearson([model_by_month[m] for m in shared],
                               [obs_by_month[m] for m in shared]),
        "n_months": len(shared),
    }


def format_report(metrics: dict) -> str:
    """One-block human-readable summary."""
    return (
        f"  peak month        model {metrics['peak_model']:>2}   "
        f"obs {metrics['peak_obs']:>2}   offset {metrics['peak_offset_months']:+d} months\n"
        f"  autumn/spring     model {metrics['autumn_spring_model']:.2f}   "
        f"obs {metrics['autumn_spring_obs']:.2f}\n"
        f"  seasonal r        {metrics['seasonal_r']:+.2f}   "
        f"(n = {metrics['n_months']} months)"
    )
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/test_seasonal_phase.py -q`
Expected: PASS, 6 passed

- [ ] **Step 5: Wire the flag into the validator, with the observation window applied**

This is the step the review flagged as poisoning everything downstream if done naively: `metrics()` at `tools/validate_cl29_vs_epa.py:156-163` and `season_summary()` at `:223-232` both discard observations outside the simulated window with `if 0 <= off <= hi`. The phase summary must do the same, or 69 % of the chlorophyll rows — all predating the model window — enter the observed climatology and it reports an autumn/spring ratio of 2.53 instead of the 1.90 this plan targets.

Add the import after the existing `import pandas as pd` at the top of the file:

```python
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from seasonal_phase import format_report, phase_metrics  # noqa: E402
```

Add this function immediately before `def main(argv=None):`:

```python
def phase_summary(out_dir, base_year, obs, variable="CHLA"):
    """Monthly climatology of model vs observations, pooled over mapped boxes.

    Observations are restricted to the simulated window exactly as metrics() and
    season_summary() do; otherwise observations from years the model never ran
    dominate the observed climatology.
    """
    model_sum, model_n, obs_sum, obs_n = {}, {}, {}, {}
    for path in sorted(glob.glob(os.path.join(out_dir, "PELAGIC_BOX_*.out"))):
        if path.endswith("_PROCESS_RATES.out"):
            continue
        box = box_number(path)
        if (box, variable) not in obs:
            continue
        mdf = load_box_output(path, base_year)
        col = MODEL_COL[variable]
        base = mdf["date"].iloc[0]
        hi = mdf["TIME_DAYS"].to_numpy(float)[-1]
        for d, v in zip(mdf["date"], mdf[col]):
            model_sum[d.month] = model_sum.get(d.month, 0.0) + float(v)
            model_n[d.month] = model_n.get(d.month, 0) + 1
        for d, v in zip(obs[(box, variable)]["date"], obs[(box, variable)]["value"]):
            off = (d - base).days
            if not (0 <= off <= hi):
                continue
            obs_sum[d.month] = obs_sum.get(d.month, 0.0) + float(v)
            obs_n[d.month] = obs_n.get(d.month, 0) + 1
    model_by_month = {m: model_sum[m] / model_n[m] for m in model_sum}
    obs_by_month = {m: obs_sum[m] / obs_n[m] for m in obs_sum}
    return phase_metrics(model_by_month, obs_by_month), model_by_month, obs_by_month
```

Add the flag beside `--by-season` in `main()`:

```python
    p.add_argument("--phase", action="store_true",
                   help="report seasonal-phase metrics (peak month, autumn/spring ratio, "
                        "seasonal correlation) for chlorophyll-a")
```

And immediately after the existing `if a.by_season:` block:

```python
    if a.phase:
        m, model_by_month, obs_by_month = phase_summary(a.outputs, a.base_year, obs)
        print("\nSeasonal phase (chlorophyll-a, monthly climatology):")
        print(format_report(m))
        print(f"  {'month':>6}{'model':>9}{'obs':>9}")
        for mo in sorted(set(model_by_month) & set(obs_by_month)):
            print(f"  {mo:>6}{model_by_month[mo]:>9.1f}{obs_by_month[mo]:>9.1f}")
```

- [ ] **Step 6: Verify against the known reference values**

Pick any full-record CL29 output directory (`OUTPUTS_CL29/` if populated, otherwise produce one per Task 2 Step 2) and run:

```bash
cd /home/razinka/AQUABCv0.2
python3 tools/validate_cl29_vs_epa.py --outputs OUTPUTS_CL29 \
  --obs epa_observations_out/epa_observations_tidy.csv --no-plots --phase \
  --since 2016-01-01 --until 2021-12-31 --out /tmp/phase_check 2>/dev/null | tail -22
```
Expected, from `docs/Satellite_model_pattern_comparison.md`: observed peak month **8**, observed autumn/spring ≈ **1.90**, model autumn/spring ≈ **0.58**, seasonal r ≈ **−0.58**. If the observed ratio comes out near 2.5, the window filter is not being applied — fix before continuing, because every later judgement rests on this number.

- [ ] **Step 7: Lint and commit**

```bash
cd /home/razinka/AQUABCv0.2
ruff check tools/seasonal_phase.py tools/validate_cl29_vs_epa.py tests/python/test_seasonal_phase.py
python3 -m pytest tests/python -q
git add tools/seasonal_phase.py tools/validate_cl29_vs_epa.py tests/python/test_seasonal_phase.py
git commit -m "feat(validate): seasonal-phase metrics (peak month, autumn/spring ratio, seasonal r)

Per-season bias cannot express a phase error - a model can carry the right annual
mean while peaking six months late, which is the measured CL29 failure. Adds pure
metric functions plus a --phase flag whose observed climatology is restricted to
the simulated window, as metrics() and season_summary() already are."
```

---

### Task 2: Diagnose both halves of the phase error

Two questions, one tool, no model change — the limitation factors and rate terms are already written to `PROCESS_RATES` and are only switched off in the CL29 output configuration.

**Question A — why do diatoms not shut down in winter?** The model carries 1.2–1.3 mg C/L of diatoms through January–May while observed chlorophyll is at its annual minimum (9.6 µg/L in February). The light-climate correction does not fix it (`docs/Diazotroph_phenology_modelling_review.md` §4).

**Question B — why is the summer diazotroph bloom ~10× too small?** Contrary to the assumption in this plan's first draft, the fixers are *not* extinct and their *timing is correct*: `NOST_VEG_HET_C` germinates from akinetes on schedule (`AKI_C` 0.742 → 0.002 mg C/L through August) and the combined fixer peak falls in August. But that peak is 0.17 mg C/L against ~2.0 observed. A 9-parameter calibration that halved fixer mortality *and* raised fixer growth left them ~70× below observations (`docs/CL29_Calibration_Paper_Draft.md` §3.6), so the constraint is not those two rates.

Index map for any group (`aquabc_II_pelagic_model.f90:2263-2275` for DIA_C; the same 1–12 layout is used per group):

| index | quantity | | index | quantity |
|---|---|---|---|---|
| 1 | growth | | 7 | LIM DOXY |
| 2 | total respiration | | 8 | LIM N |
| 3 | excretion | | 9 | LIM P |
| 4 | death | | 10 | LIM Si |
| 5 | grazed by zooplankton | | 11 | LIM LIGHT |
| 6 | LIM TEMP | | 12 | light saturation |

**Files:**
- Modify: `INPUTS_CL29/PELAGIC_OUTPUT_INFORMATION_FILE.txt` (column 3 = `PROCESS_RATE_OUT`, currently `0` for all 29 boxes)
- Create: `tools/diagnose_group_limitation.py`

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces: printed tables plus `group_limitation.csv`; consumed by Task 3 as text.

- [ ] **Step 1: Enable process-rate output for four diagnostic boxes**

In `INPUTS_CL29/PELAGIC_OUTPUT_INFORMATION_FILE.txt`, set the third column to `1` for boxes 7, 14, 17 and 23 (strait, two central, one northern — all have in-situ observations), leaving the other 25 at `0`. Each edited line reads, for example:

```
           7           1           1           0
```

`INPUTS_CL29/` is gitignored and regenerated by `tools/eutropy_poc/eutropy_to_estas.py`; make the same change there if the run must be reproducible, otherwise record it as a manual diagnostic step in Task 3's write-up.

- [ ] **Step 2: Run the model with process rates on**

```bash
cd /home/razinka/AQUABCv0.2
mkdir -p /tmp/phen_diag/OUT
sed -e 's|OUTPUTS_CL29/|/tmp/phen_diag/OUT/|' -e 's|^             10$|            240|' \
    INPUT_CL29.txt > /tmp/phen_diag/INPUT.txt
ln -sfn "$PWD/INPUTS_CL29" /tmp/phen_diag/INPUTS_CL29
( cd /tmp/phen_diag && ESTAS_HOLD_VOLUME=1 /home/razinka/AQUABCv0.2/ESTAS_II INPUT.txt > run.log 2>&1 )
ls /tmp/phen_diag/OUT/ | grep PROCESS_RATES
```
Expected: four `PELAGIC_BOX_000NN_PROCESS_RATES.out` files. Runtime ~11 minutes. Note `ln -sfn`, not `ln -sf`: on a re-run the latter creates a self-referential symlink *inside* the real `INPUTS_CL29/`.

- [ ] **Step 3: Verify the column layout before reading any number**

```bash
cd /home/razinka/AQUABCv0.2
grep -n "NDIAGVAR " SOURCE_CODE/ESTAS/mod_GLOBAL.f90
head -1 /tmp/phen_diag/OUT/PELAGIC_BOX_00007_PROCESS_RATES.out | wc -w
```
Expected: `NDIAGVAR = 30`, and a field count of **1081** (= 1 + 36 × 30). If the field count differs, the stride is not 30 and every number below would be silently wrong — stop and re-derive.

- [ ] **Step 4: Write the diagnostic tool**

Create `tools/diagnose_group_limitation.py`:

```python
#!/usr/bin/env python3
"""Which term constrains a phytoplankton group, by month?

Answers two CL29 questions without touching the model: why diatoms do not shut down
in winter, and why the summer diazotroph bloom is ~10x too small. Both groups already
export their growth-limitation factors and rate terms to PROCESS_RATES; this reads them.

A limitation factor near 1.0 is NOT limiting. If every factor is high yet biomass is
low, growth is not the constraint and the loss terms are. If a factor is small, that
term is the constraint and is where a fix belongs.

PROCESS_RATES layout is state-major with NDIAGVAR (=30) columns per state variable,
and the file has no header row.
"""
from __future__ import annotations

import argparse
import glob
import os
import re

import pandas as pd

RATES = {1: "growth", 2: "respiration", 3: "excretion", 4: "death", 5: "grazed"}
LIMS = {6: "TEMP", 7: "DOXY", 8: "N", 9: "P", 10: "Si", 11: "LIGHT"}
GROUPS = {"DIA": 5, "CYN": 15, "OPA": 16, "FIX_CYN": 19, "NOST": 31}
WINTER = (1, 2, 3, 4, 5)
BLOOM = (7, 8, 9)


def load_group(path, base_year, state_index, stride):
    """Read one PROCESS_RATES file and return the named columns for one group."""
    df = pd.read_csv(path, sep=r"\s+", header=None)
    base = pd.Timestamp(f"{base_year}-01-01")
    out = {"date": [base + pd.Timedelta(days=float(d)) for d in df[0]]}
    first = 1 + (state_index - 1) * stride
    for idx, name in {**RATES, **LIMS}.items():
        col = first + (idx - 1)
        if col < df.shape[1]:
            out[name] = df[col].astype(float).values
    return pd.DataFrame(out)


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--outputs", required=True, help="folder containing *_PROCESS_RATES.out")
    p.add_argument("--group", default="DIA", choices=sorted(GROUPS))
    p.add_argument("--base-year", type=int, default=2012)
    p.add_argument("--stride", type=int, default=30, help="NDIAGVAR, columns per state variable")
    p.add_argument("--season", default="winter", choices=["winter", "bloom"],
                   help="which season to interrogate: winter (Jan-May) or bloom (Jul-Sep)")
    p.add_argument("--out", default="group_limitation.csv")
    a = p.parse_args()

    idx = GROUPS[a.group]
    frames = []
    for path in sorted(glob.glob(os.path.join(a.outputs, "*_PROCESS_RATES.out"))):
        box = int(re.search(r"PELAGIC_BOX_0*(\d+)_", os.path.basename(path)).group(1))
        df = load_group(path, a.base_year, idx, a.stride)
        df["box"] = box
        frames.append(df)
    if not frames:
        raise SystemExit(f"no *_PROCESS_RATES.out found in {a.outputs}")
    df = pd.concat(frames)
    df["month"] = df["date"].dt.month

    lim_cols = [c for c in LIMS.values() if c in df.columns]
    bad = [c for c in lim_cols if not df[c].between(0, 1.0001).all()]
    if bad:
        raise SystemExit(f"columns {bad} fall outside [0,1]: the stride is wrong, "
                         f"re-check NDIAGVAR (used {a.stride})")
    if all((df[c] == 0).all() for c in lim_cols):
        raise SystemExit("all limitation factors are identically zero: wrong column offset")

    focus = WINTER if a.season == "winter" else BLOOM
    other = BLOOM if a.season == "winter" else WINTER
    print(f"=== {a.group} (state variable {idx}), limitation factors by month "
          f"(1.0 = NOT limiting)")
    print(df.groupby("month")[lim_cols].mean().round(3).to_string())

    print(f"\n{a.season} {focus} vs {other}:")
    f_, o_ = df[df.month.isin(focus)][lim_cols].mean(), df[df.month.isin(other)][lim_cols].mean()
    print(f"{'factor':<8}{a.season:>10}{'other':>9}   verdict")
    for c in lim_cols:
        verdict = ("NOT limiting" if f_[c] > 0.66
                   else "weakly limiting" if f_[c] > 0.33 else "STRONGLY limiting")
        print(f"{c:<8}{f_[c]:>10.3f}{o_[c]:>9.3f}   {verdict}")

    rate_cols = [c for c in RATES.values() if c in df.columns]
    if rate_cols:
        print(f"\n{a.group} rate terms by month:")
        print(df.groupby("month")[rate_cols].mean().round(5).to_string())
        r = df[df.month.isin(focus)][rate_cols].mean()
        loss = sum(r.get(c, 0.0) for c in ("respiration", "excretion", "death", "grazed"))
        g = r.get("growth", float("nan"))
        print(f"\n{a.season}: growth {g:.5f} vs total losses {loss:.5f} -> "
              f"{'growth-dominated' if g > loss else 'loss-dominated'}")
        for c in ("respiration", "excretion", "death", "grazed"):
            if c in r and loss:
                print(f"    {c:<12}{r[c]:>10.5f}  ({100 * r[c] / loss:.0f}% of losses)")

    df.to_csv(a.out, index=False)
    print(f"\nwrote {a.out}")


if __name__ == "__main__":
    main()
```

- [ ] **Step 5: Run both diagnoses**

```bash
cd /home/razinka/AQUABCv0.2
python3 tools/diagnose_group_limitation.py --outputs /tmp/phen_diag/OUT \
    --group DIA --season winter --out /tmp/phen_diag/dia_winter.csv
python3 tools/diagnose_group_limitation.py --outputs /tmp/phen_diag/OUT \
    --group NOST --season bloom --out /tmp/phen_diag/nost_bloom.csv
python3 tools/diagnose_group_limitation.py --outputs /tmp/phen_diag/OUT \
    --group FIX_CYN --season bloom --out /tmp/phen_diag/fix_bloom.csv
```
Expected: for each group, a table naming the binding constraint and whether the season is growth- or loss-dominated. Record all three verdicts.

- [ ] **Step 6: Lint and commit**

```bash
cd /home/razinka/AQUABCv0.2
ruff check tools/diagnose_group_limitation.py
git add tools/diagnose_group_limitation.py
git commit -m "feat(diag): report which term constrains a phytoplankton group, by month

Reads the limitation factors and rate terms already exported to PROCESS_RATES.
Answers the two halves of the CL29 phase error: why diatoms do not shut down in
winter, and why the summer diazotroph bloom is ~10x too small despite correct
timing and a working akinete germination cycle."
```

---

### Task 3: Record the diagnosis and select the follow-up

**Files:**
- Create: `docs/CL29_phenology_diagnosis.md`

**Interfaces:**
- Consumes: the `--phase` output from Task 1 and the three verdicts from Task 2.
- Produces: the decision that scopes the second plan.

- [ ] **Step 1: Write up the measured baseline**

Record, from Task 1 Step 6 and Task 2 Step 5: the model's phase metrics against the 2016–2021 observed climatology; the monthly limitation factors for DIA (winter) and for NOST and FIX_CYN (bloom season); and for each, whether the season is growth- or loss-dominated with the loss breakdown.

- [ ] **Step 2: Apply the decision table**

| Diagnosis | Follow-up plan targets |
|---|---|
| DIA winter: all limitation factors high, growth-dominated | the diatom temperature and/or light response — verify CTMI validity (`2·T_opt > T_min + T_max`) before changing `KG_DIA_OPT_TEMP`, per the trap recorded in `fix-cyn-n2fixation-overprediction` |
| DIA winter: factors low but biomass persists, loss-dominated | diatom loss terms — mortality, settling velocity, grazing |
| Fixers in bloom season: a limitation factor is small | that resource pathway (light, P, or the N-fixation energetic penalty) |
| Fixers in bloom season: all factors high but biomass low, loss-dominated | loss terms — and note that halving mortality has already been shown insufficient, so look at grazing preference and settling |
| Fixers: growth-dominated yet biomass still low | the germination/transfer pathway — how much akinete carbon actually reaches the vegetative pool, since `AKI_C` falls 0.742 → 0.002 while the vegetative peak reaches only 0.17 |

- [ ] **Step 3: State explicitly what was ruled out**

Record that the seed-floor hypothesis is dead and why, so it is not revisited: `FIX_CYN_C` never approaches the `MIN_CONCENTRATION` clamp (measured minimum 5.0×10⁻⁴ against a clamp of 1×10⁻¹⁰), `NOST_VEG_HET_C` recovers from akinetes on schedule each year, and the combined fixer peak is in the correct month. The deficit is amplitude, and a concentration floor is a bound rather than a rate.

- [ ] **Step 4: Commit**

```bash
cd /home/razinka/AQUABCv0.2
git add docs/CL29_phenology_diagnosis.md
git commit -m "docs: CL29 seasonal-phase diagnosis and follow-up selection"
```

---

## Self-Review

**Spec coverage.** Of the six options in `docs/Diazotroph_phenology_modelling_review.md`, this plan delivers the prerequisite measurement (absent from all six) and the diagnosis for options 1 and 2. It deliberately specifies **no model change**: the adversarial review demonstrated that the first draft's Fortran task rested on a false premise, and that the fix cannot be written before the diagnosis. Options 3–6 remain separate plans.

**Placeholder scan.** No TBDs. The only conditional — which follow-up to write — is a decision table with five named branches and their targets, not a vague instruction.

**Type consistency.** `phase_metrics` and `format_report` are defined in Task 1 and used with those exact names and keys in Task 1 Step 5. `circular_month_offset`'s sign convention is stated identically in its docstring, its tests, and its one call site. `GROUPS`, `RATES` and `LIMS` in Task 2 use the state indices given in Global Constraints, which are quoted from `aquabc_II_pelagic_svindex.f90`.

**Corrections carried from the adversarial review** (30 findings raised, 28 survived verification): the observation window filter in `phase_summary`; the month-offset test assertions, which contradicted the implementation; `NDIAGVAR = 30`, not 20, with the field-count check and the correct grep target; `header=None` on the headerless process-rate file; `_season_ratio` restricted to the shared month set; the dead `n = len(xs)` that would have failed ruff; `ln -sfn` in place of `ln -sf`; the target values quoted as 0.58 / −0.58 over Feb–May with their actual source; and the deletion of the entire seed-floor task with its premise recorded as refuted.
