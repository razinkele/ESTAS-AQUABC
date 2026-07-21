# CL29 PEST-posterior promotion — Implementation Plan

> 🪦 **OUTCOME: ABANDONED (2026-07-21).** Executed through Task 4 (the gate run). Tasks 1–2 shipped the reusable `tools/compare_validation_runs.py`; Task 3's converter edit was validated then **reverted**. The gate failed on the EPA window: `KDISS`=0.118 closes PO4 but induces P-limitation that regresses NH4/NO3/Si/Chl-a, and the planned `K_MIN`→1.0 fallback (Tasks 6) does not help (the regression is `KDISS`-driven, not `K_MIN`-driven) — a nonstationarity mismatch between the 2022 low-P calibration and the 2012–16 eutrophic era. Tasks 5–8 were superseded by a revert + negative-result write-up. See `docs/CL29_KM_2022-2023_Validation.md` § "Promotion attempt (abandoned)".

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Promote the two defensible PEST posteriors (`KDISS_DET_PART_ORG_P_20`=0.118, `K_MIN_DOC_NO3N_20`=1.13) into the CL29 converter default, and prove with a measured before/after run that PO4 over-prediction closes without regressing the broader EPA fit.

**Architecture:** One-file converter edit (`CL29_WCONST_OVERRIDE` in `tools/eutropy_poc/eutropy_to_estas.py`), gated behind a two-run experiment on a single freshly-built binary: baseline (current defaults) vs promoted, each scored against KM-2022 and EPA-2012-2021 by the existing `validate_cl29_vs_epa.py`, with a new committed comparison tool applying a one-sided regression guard. Then the validation doc is reconciled with the measured numbers.

**Tech Stack:** Python 3 (converter + validators, stdlib + pandas/numpy already used), gfortran ESTAS build, `run_cl29.sh` wrapper.

## Global Constraints

- **Two-run protocol on ONE binary.** Build ESTAS once; use that same binary for both baseline and promoted runs. Never compare against numbers from a different binary (the doc's old tables used the v0.5.2 release binary — do not diff against them).
- **Same obs CSVs for before and after.** KM: the committed `pest/km_observations_tidy.csv` (already carries the post-LTK14 box map). EPA: one freshly-ingested `epa_observations_tidy.csv`, reused for both runs.
- **Never regenerate `pest/cl29.pst`.** It is the frozen historical record of the calibration run; its point-initials (3.48 / 1.0) document the pre-promotion state.
- **One-sided EPA regression guard:** for non-targeted variables (NH4, DO, TP) EPA RMSE must **not increase** by more than **5%**. PO4/NO3/TN are targets/diagnostics, not held to this.
- **K_MIN fallback:** `K_MIN`=1.13 raises denitrification → lowers NO3. If EPA NO3 or TN RMSE worsens >5%, revert `K_MIN` to 1.0 and re-score (Task 6).
- **CL29 default is `MODEL_SEDIMENTS=0`**, so an OpenMP build is safe (the sediment-path deadlock does not apply); a full 2012–2022 run is ~9 min.
- Scratch outputs (metrics CSVs, run logs) go under the scratchpad dir, not the repo: `/tmp/claude-1000/-home-razinka-AQUABCv0-2/a90c26f7-fa97-44bc-ac88-a9300f279f47/scratchpad/promotion/`.

---

### Task 1: Committed comparison tool with a one-sided regression guard

Makes the gate auditable and reproducible instead of eyeballed. Reads two `validation_metrics.csv` files (per-box rows from `validate_cl29_vs_epa.py`), aggregates per-variable obs-weighted RMSE + bias, prints a before/after/Δ table, and exits non-zero if any `--no-regress` variable's RMSE rises more than `--max-rise` %.

**Files:**
- Create: `tools/compare_validation_runs.py`
- Test: `tests/python/test_compare_validation_runs.py`

**Interfaces:**
- Consumes: two CSVs with header `box,variable,n,obs_mean,model_mean,bias,rmse,r` — the exact `fieldnames` `validate_cl29_vs_epa.py:write_metrics_csv` emits (verified at `tools/validate_cl29_vs_epa.py:178`; the column is `variable`, NOT `var` — `var` is only the console-print header at line 155).
- Produces: `aggregate(rows) -> dict[var] -> {"n","rmse","bias"}` (obs-weighted: `rmse=sqrt(Σ rmse²·n / Σn)`, `bias=Σ bias·n / Σn`), matching the validator's own per-variable summary (`validate_cl29_vs_epa.py:164-173`). CLI: `compare_validation_runs.py BASELINE.csv PROMOTED.csv --no-regress NH4,DO,TP --max-rise 5`.

- [ ] **Step 1: Write the failing test**

```python
# tests/python/test_compare_validation_runs.py
import csv
import subprocess
import sys
from pathlib import Path

TOOL = Path(__file__).resolve().parents[2] / "tools" / "compare_validation_runs.py"


def _write(path, rows):
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        # EXACT header validate_cl29_vs_epa.py:write_metrics_csv emits — column is "variable"
        w.writerow(["box", "variable", "n", "obs_mean", "model_mean", "bias", "rmse", "r"])
        for r in rows:
            w.writerow(r)


def rows_to_csv(tmp_path, rows):
    p = tmp_path / "m.csv"
    _write(p, rows)
    return str(p)


def _run(base, prom, guard):
    return subprocess.run([sys.executable, str(TOOL), str(base), str(prom),
                           "--no-regress", guard, "--max-rise", "5"],
                          capture_output=True, text=True)


def test_aggregate_obs_weighted(tmp_path):
    sys.path.insert(0, str(TOOL.parent))
    import compare_validation_runs as c
    # two boxes of PO4: n-weighted RMSE = sqrt((3²·10 + 1²·30)/40) = sqrt(120/40)=sqrt3
    rows = [["1", "PO4", "10", "0", "0", "0.5", "3", "0.9"],
            ["2", "PO4", "30", "0", "0", "0.1", "1", "0.9"]]
    agg = c.aggregate(c.read_metrics(rows_to_csv(tmp_path, rows)))
    assert abs(agg["PO4"]["rmse"] - (120 / 40) ** 0.5) < 1e-9
    assert abs(agg["PO4"]["bias"] - (0.5 * 10 + 0.1 * 30) / 40) < 1e-9


def test_guard_fails_on_rmse_regression(tmp_path):
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.0", "1.00", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.0", "1.20", "0.9"]])  # +20% RMSE
    r = _run(base, prom, "NH4")
    assert r.returncode != 0
    assert "NH4" in r.stdout


def test_guard_passes_within_tolerance(tmp_path):
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.0", "1.00", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.0", "1.03", "0.9"]])  # +3%
    assert _run(base, prom, "NH4").returncode == 0


def test_guard_fails_on_bias_growth(tmp_path):
    # RMSE flat, but |bias| grows 15x — a real directional error the RMSE guard misses
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.002", "1.00", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.031", "1.00", "0.9"]])
    assert _run(base, prom, "NH4").returncode != 0


def test_zero_baseline_rmse_is_regression(tmp_path):
    # perfect-fit baseline (RMSE 0) -> positive RMSE must be flagged, not treated as 0%
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.0", "0.0", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.5", "0.5", "0.9"]])
    assert _run(base, prom, "NH4").returncode != 0
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m pytest tests/python/test_compare_validation_runs.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'compare_validation_runs'`.

- [ ] **Step 3: Write minimal implementation**

```python
#!/usr/bin/env python3
"""Compare two CL29 validation_metrics.csv runs (baseline vs promoted).

Aggregates per-variable obs-weighted RMSE + bias exactly as validate_cl29_vs_epa.py's
own summary does, prints a before/after/delta table, and applies a one-sided regression
guard: exit non-zero if any --no-regress variable regresses > --max-rise % on RMSE OR on
|bias| (or its bias sign-flips into non-trivial error).
"""
from __future__ import annotations

import argparse
import csv
import math
import sys


def read_metrics(path):
    with open(path) as fh:
        return list(csv.DictReader(fh))


def aggregate(rows):
    by = {}
    for r in rows:
        v = r["variable"]          # validate_cl29_vs_epa.py CSV column is "variable"
        by.setdefault(v, []).append((int(r["n"]), float(r["rmse"]), float(r["bias"])))
    out = {}
    for v, recs in by.items():
        n = sum(k for k, _, _ in recs)
        rmse = math.sqrt(sum(rm ** 2 * k for k, rm, _ in recs) / n)
        bias = sum(b * k for k, _, b in recs) / n
        out[v] = {"n": n, "rmse": rmse, "bias": bias}
    return out


def rmse_rise_pct(base, prom):
    """% RMSE rise; a rise from a perfect-fit (0) baseline to >0 is a regression (inf)."""
    if base > 0:
        return 100.0 * (prom - base) / base
    return float("inf") if prom > 0 else 0.0


def bias_regressed(b_bias, q_bias, tol_pct):
    """True if bias sign-flips into non-trivial error, or |bias| grows beyond tol_pct."""
    if abs(q_bias) < 1e-9:
        return False
    if b_bias * q_bias < 0:                                    # sign flip into real bias
        return True
    return abs(b_bias) > 1e-6 and abs(q_bias) > abs(b_bias) * (1 + tol_pct / 100.0)


def main(argv=None):
    p = argparse.ArgumentParser()
    p.add_argument("baseline")
    p.add_argument("promoted")
    p.add_argument("--no-regress", default="", help="comma list of vars held to the guard")
    p.add_argument("--max-rise", type=float, default=5.0,
                   help="max allowed RMSE/|bias| rise (%)")
    a = p.parse_args(argv)

    base = aggregate(read_metrics(a.baseline))
    prom = aggregate(read_metrics(a.promoted))
    guard = {v.strip().upper() for v in a.no_regress.split(",") if v.strip()}

    print(f"{'var':6} {'n':>5} {'RMSE base':>10} {'RMSE prom':>10} {'dRMSE%':>8} "
          f"{'bias base':>10} {'bias prom':>10}")
    failures = []
    for v in sorted(set(base) | set(prom)):
        b = base.get(v);  q = prom.get(v)
        if not b or not q:
            print(f"{v:6}  (only in one run — cannot compare)")
            if v in guard:
                failures.append((v, "missing in one run"))
            continue
        d = rmse_rise_pct(b["rmse"], q["rmse"])
        flag = ""
        if v in guard and (d > a.max_rise or bias_regressed(b["bias"], q["bias"], a.max_rise)):
            flag = "  <-- REGRESSION"
            failures.append((v, f"dRMSE {d:+.1f}%, bias {b['bias']:+.3g}->{q['bias']:+.3g}"))
        print(f"{v:6} {b['n']:>5} {b['rmse']:>10.4g} {q['rmse']:>10.4g} {d:>+8.1f} "
              f"{b['bias']:>+10.3g} {q['bias']:>+10.3g}{flag}")

    if failures:
        print("\nGUARD FAILED: " + "; ".join(f"{v} ({why})" for v, why in failures))
        return 1
    print(f"\nGuard passed (no --no-regress variable regressed > {a.max_rise:.0f}%).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python -m pytest tests/python/test_compare_validation_runs.py -q`
Expected: PASS (5 passed).

- [ ] **Step 5: Lint + commit**

```bash
ruff check tools/compare_validation_runs.py tests/python/test_compare_validation_runs.py
git add tools/compare_validation_runs.py tests/python/test_compare_validation_runs.py
git commit -m "feat(tools): compare_validation_runs — per-variable before/after guard"
```

---

### Task 2: Build the binary + capture the BASELINE metrics

The "before" half of the protocol. Uses the current (un-promoted) converter defaults.

**Files:**
- Modify: none (read-only baseline capture)
- Uses: `tools/eutropy_poc/eutropy_to_estas.py`, `run_cl29.sh`, `tools/ingest_epa_observations.py`, `tools/validate_cl29_vs_epa.py`, `tools/compare_validation_runs.py`

- [ ] **Step 1: Build ESTAS once (OpenMP, reused for every run)**

Run: `make FC=gfortran OPENMP=1 build-estas`
Expected: builds `./ESTAS_II` (exit 0).

- [ ] **Step 2: Generate the EPA obs CSV (one-time; reused for both runs)**

Run: `python tools/ingest_epa_observations.py`
Expected: prints `Wrote ./epa_observations_out/epa_observations_tidy.csv`. (Defaults read the workbooks at `~/curonian/DATA/JTD/timeSeries_{waterQuality_data,Chla}_EPA.xlsx` and `tools/epa_station_to_box.csv`.)
Sanity: `wc -l epa_observations_out/epa_observations_tidy.csv` (thousands of rows).

- [ ] **Step 3: Regenerate INPUTS_CL29 at current defaults + run the baseline**

```bash
mkdir -p /tmp/claude-1000/-home-razinka-AQUABCv0-2/a90c26f7-fa97-44bc-ac88-a9300f279f47/scratchpad/promotion
python tools/eutropy_poc/eutropy_to_estas.py          # regen INPUTS_CL29 (KDISS=3.48, K_MIN=1.0)
grep -E 'KDISS_DET_PART_ORG_P_20|K_MIN_DOC_NO3N_20' INPUTS_CL29/WCONST_04.txt   # confirm 3.48 / 1.0
./run_cl29.sh                                          # 2012-2022, ESTAS_HOLD_VOLUME=1, ~9 min
```
Expected: `run_cl29.sh` exits 0; `OUTPUTS_CL29/PELAGIC_BOX_00007.out` etc. exist.

- [ ] **Step 4: Score the baseline on both windows**

```bash
S=/tmp/claude-1000/-home-razinka-AQUABCv0-2/a90c26f7-fa97-44bc-ac88-a9300f279f47/scratchpad/promotion
python tools/validate_cl29_vs_epa.py --obs pest/km_observations_tidy.csv \
    --outputs OUTPUTS_CL29 --base-year 2012 --no-plots --out $S/base_km
python tools/validate_cl29_vs_epa.py --obs epa_observations_out/epa_observations_tidy.csv \
    --outputs OUTPUTS_CL29 --base-year 2012 --no-plots --out $S/base_epa
```
Expected: each prints a "Per-variable summary" and `Wrote $S/base_*/validation_metrics.csv`.
Record the KM PO4 baseline bias as the sanity check: expect a **small positive** PO4 bias (the doc's pre-fix table showed ~+0.025; the exact value will differ with the corrected box map and the fresh binary — the sanity check is the **direction/order of magnitude**, not an exact match).

- [ ] **Step 5: No commit** (baseline artifacts are scratch, gitignored). Note the KM PO4 baseline bias in the task log for Task 5's comparison.

---

### Task 3: Apply the converter edit (KDISS + K_MIN)

The core change. Baseline is already captured, so it is now safe to edit.

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py:74-88` (the `CL29_WCONST_OVERRIDE` dict + comment)

**Interfaces:**
- Produces: a converter that writes `KDISS_DET_PART_ORG_P_20=0.118` and `K_MIN_DOC_NO3N_20=1.13` into `INPUTS_CL29/WCONST_04.txt` via `_apply_wconst_overrides` (raises `SystemExit` if a name isn't found exactly once — verified `eutropy_to_estas.py:326-328`).

- [ ] **Step 1: Edit the override dict**

In `tools/eutropy_poc/eutropy_to_estas.py`, change the existing `K_MIN_DOC_NO3N_20` line and add `KDISS_DET_PART_ORG_P_20`. The existing block ends:

```python
    # ... 1.0 brings NO3 bias +0.31->+0.06 (RMSE -31%) and
    # TN +0.80->+0.39 across all 9 boxes, with DO slightly improved and Chl-a ~5% low.
    # (PO4/Si stay high -- their sinks are sediment burial/Fe-binding, MODEL_SEDIMENTS=0.)
    "K_MIN_DOC_NO3N_20": 1.0,
}
```

Replace the `"K_MIN_DOC_NO3N_20": 1.0,` line and closing brace with:

```python
    # 1.0 brings NO3 bias +0.31->+0.06 (RMSE -31%) and TN +0.80->+0.39 across all 9 boxes
    # (these figures are the EPA-at-1.0 evaluation). The pestpp-ies KM-2022 calibration
    # (PR #54/#56, Phi 4058->1287) refined it to 1.13 +/-0.07 -- consistent to ~1.9 sigma
    # across two independent windows, so 1.13 is the cross-validated value.
    "K_MIN_DOC_NO3N_20": 1.13,
    # POP -> PO4 dissolution rate. Template default 3.48/day (e-fold ~7 h) is implausibly
    # fast and drives the CL29 PO4 over-prediction. pestpp-ies (PR #54/#56) puts the
    # posterior at 0.118 +/-0.004 -- the tightest-constrained parameter (99.9% variance
    # reduction), e-fold ~8.5 d, closing PO4 in BOTH the KM-2022 and EPA windows. It also
    # partly compensates the disabled sediment-P burial sink (MODEL_SEDIMENTS=0).
    "KDISS_DET_PART_ORG_P_20": 0.118,
    # NOT promoted (evaluated, rejected): KG_DIA_OPT_TEMP (posterior 5.45) WORSENS Si
    # (+0.56->+1.20), flips Chl-a to under-prediction, and degrades NH4 -- with no
    # biogenic-Si burial sink (MODEL_SEDIMENTS=0), raising diatom growth recycles more Si,
    # not less. KD_DIA_20 is identified but r=0.84-correlated with KG, so it can't be split
    # off. KHS_DSi_DIA is non-identifiable (posterior wider than prior). See
    # docs/superpowers/specs/2026-07-21-cl29-pest-posterior-promotion-design.md.
}
```

- [ ] **Step 2: Verify the converter writes the new values**

```bash
python tools/eutropy_poc/eutropy_to_estas.py
grep -E 'KDISS_DET_PART_ORG_P_20|K_MIN_DOC_NO3N_20' INPUTS_CL29/WCONST_04.txt
```
Expected: `KDISS_DET_PART_ORG_P_20 ... 0.118` and `K_MIN_DOC_NO3N_20 ... 1.13`.

- [ ] **Step 3: Confirm no test broke + lint**

```bash
python -m pytest tests/python/test_build_pest.py -q
ruff check tools/eutropy_poc/eutropy_to_estas.py
```
Expected: PASS (the test uses a `tmp_path` fixture, independent of the override dict).

- [ ] **Step 4: Commit the edit**

```bash
git add tools/eutropy_poc/eutropy_to_estas.py
git commit -m "feat(converter): promote calibrated KDISS=0.118 + K_MIN=1.13 into CL29 default"
```

---

### Task 4: Capture the PROMOTED metrics + run the gate

The "after" half + the pass/fail decision.

**Files:**
- Modify: none (measurement)
- Uses: `run_cl29.sh`, `validate_cl29_vs_epa.py`, `compare_validation_runs.py` (Task 1)

- [ ] **Step 1: Run the promoted model (same binary as baseline)**

```bash
./run_cl29.sh          # INPUTS_CL29 now carries 0.118 / 1.13; ~9 min
```
Expected: exit 0.

- [ ] **Step 2: Score the promoted run on both windows**

```bash
S=/tmp/claude-1000/-home-razinka-AQUABCv0-2/a90c26f7-fa97-44bc-ac88-a9300f279f47/scratchpad/promotion
python tools/validate_cl29_vs_epa.py --obs pest/km_observations_tidy.csv \
    --outputs OUTPUTS_CL29 --base-year 2012 --no-plots --out $S/prom_km
python tools/validate_cl29_vs_epa.py --obs epa_observations_out/epa_observations_tidy.csv \
    --outputs OUTPUTS_CL29 --base-year 2012 --no-plots --out $S/prom_epa
```

- [ ] **Step 3: Apply the guard (KM target + EPA regression)**

```bash
S=/tmp/claude-1000/-home-razinka-AQUABCv0-2/a90c26f7-fa97-44bc-ac88-a9300f279f47/scratchpad/promotion
echo "== KM 2022 (target: PO4 should close; before/after table only, no gate here) =="
python tools/compare_validation_runs.py $S/base_km/validation_metrics.csv \
    $S/prom_km/validation_metrics.csv --max-rise 5
echo "== EPA 2012-2021 (GATE: NH4/DO/TP must not regress >5% on RMSE OR |bias|) =="
python tools/compare_validation_runs.py $S/base_epa/validation_metrics.csv \
    $S/prom_epa/validation_metrics.csv --no-regress NH4,DO,TP --max-rise 5
```
(The KM call omits `--no-regress` — per the spec the regression guard is EPA-only; on KM the tool just prints the before/after table.)

- [ ] **Step 4: Evaluate against the success criteria (record the numbers)**

Pass requires ALL of:
- **KM PO4 closes:** pooled RMSE drops AND the per-box bias distribution tightens toward 0. Judge on pooled RMSE + the validator's per-box table, NOT pooled |bias| alone — opposite-sign per-box biases can cancel to a near-zero pooled bias without any real improvement (a near-zero pooled bias is necessary, not sufficient).
- **EPA PO4** RMSE and |bias| drop (PO4 over-predicted on EPA too).
- **EPA NH4/DO/TP guard PASSES** — compare tool exit 0 on the EPA call. The tool now flags a regression on RMSE **or** on |bias|/sign, so a bias-only degradation in a high-scatter variable is caught.
- **NO3/TN diagnostic:** read EPA NO3 and TN from the EPA compare table (ΔRMSE% and the bias columns). If either regressed > 5% → go to Task 6 (K_MIN fallback). Otherwise keep 1.13.
- **Early-era check:** scan the EPA per-box table for 2012–2016 PO4 — a modest under-prediction is acceptable, a collapse (near-zero modeled PO4) is not; note it.

Write the KM and EPA before/after per-variable tables into the scratchpad log — Task 5 pastes them into the doc.

- [ ] **Step 5: No commit** (measurement only; artifacts are scratch).

---

### Task 5: Reconcile the validation doc with the measured numbers

Make `docs/CL29_KM_2022-2023_Validation.md` internally consistent with the promoted state. Fill every number from the Task 4 metrics files — no invented values.

**Files:**
- Modify: `docs/CL29_KM_2022-2023_Validation.md`

- [ ] **Step 1: Add a "Post-promotion validation" subsection** at the end of `## Calibration (pestpp-ies)`, with the two measured tables (fill cells from `$S/base_*` and `$S/prom_*` per-variable summaries):

```markdown
### Post-promotion validation (KDISS=0.118, K_MIN=1.13)

Two-run before/after on one gfortran+OpenMP binary (baseline vs promoted converter),
scored on the committed KM CSV and freshly-ingested EPA obs.

**KM 2022** (obs-weighted per variable):

| var | RMSE before | RMSE after | bias before | bias after |
|---|---:|---:|---:|---:|
| PO4 | … | … | … | … |
| NO3 | … | … | … | … |
| Si  | … | … | … | … |
| CHLA| … | … | … | … |
| NH4 | … | … | … | … |

**EPA 2012–2021** (regression guard, one-sided on NH4/DO/TP):

| var | RMSE before | RMSE after | ΔRMSE % | verdict |
|---|---:|---:|---:|---|
| PO4 | … | … | … | target |
| NO3 | … | … | … | diagnostic |
| NH4 | … | … | … | guard |
| DO  | … | … | … | guard |
| TP  | … | … | … | guard |

Outcome: <one line — PO4 closed; guard passed/failed; K_MIN kept at 1.13 / reverted to 1.0>.
```

- [ ] **Step 2: Reconcile all four spec anchors so the doc isn't self-contradictory.**
  - (a) **2022 Results bias table:** add a header note directly above it — `_(pre-promotion default config; see "Post-promotion validation" for the calibrated result)_`.
  - (b) **2023 Results table:** no numeric edit — the KM-2023 typical-year run is deferred (see plan Notes), so it stays a pre-promotion record; add a one-line note saying so.
  - (c) **Interpretation prose** (the "PO4, Si, TN and Chl-a are over-predicted … NO3 under-predicted" paragraph): append `— these describe the pre-promotion default; see "Post-promotion validation" for the calibrated result.`
  - (d) **Next-step line** at the end of `## Calibration (pestpp-ies)` ("re-running the validation … is the next step"): change to past tense noting it is now done, pointing at the new subsection.

- [ ] **Step 3: Add the point-initial reconciliation note** next to the Calibration "initial" column (values 4.10/3.58/1.55/…): `> "initial" = iteration-0 ensemble mean (stochastic draw); the point-initials the control file used are 3.48 / 3.70 / 1.0 / 0.013 / 0.12 (pest/cl29.pst).`

- [ ] **Step 4: Commit**

```bash
git add docs/CL29_KM_2022-2023_Validation.md
git commit -m "docs: post-promotion CL29 validation (both windows) + reconcile prose"
```

---

### Task 6: (CONDITIONAL) K_MIN fallback to 1.0

**Only if** Task 4 Step 4 found EPA NO3 or TN RMSE rose > 5%. Skip otherwise.

**Files:**
- Modify: `tools/eutropy_poc/eutropy_to_estas.py` (the `K_MIN_DOC_NO3N_20` value)

- [ ] **Step 1: Revert K_MIN to 1.0**, keeping KDISS=0.118. Set `"K_MIN_DOC_NO3N_20": 1.0,` and update its comment to note "PEST 1.13 tried but reverted: it regressed EPA NO3/TN by X% — KDISS is the only promoted lever."

- [ ] **Step 2: Re-run + re-score BOTH windows** (K_MIN affects the whole run, so the KM table must be refreshed too — KM PO4 is unchanged since KDISS is untouched, but KM/EPA NO3 and TN change):

```bash
S=/tmp/claude-1000/-home-razinka-AQUABCv0-2/a90c26f7-fa97-44bc-ac88-a9300f279f47/scratchpad/promotion
python tools/eutropy_poc/eutropy_to_estas.py && ./run_cl29.sh
python tools/validate_cl29_vs_epa.py --obs epa_observations_out/epa_observations_tidy.csv \
    --outputs OUTPUTS_CL29 --base-year 2012 --no-plots --out $S/prom_epa_kmin10
python tools/validate_cl29_vs_epa.py --obs pest/km_observations_tidy.csv \
    --outputs OUTPUTS_CL29 --base-year 2012 --no-plots --out $S/prom_km_kmin10
python tools/compare_validation_runs.py $S/base_epa/validation_metrics.csv \
    $S/prom_epa_kmin10/validation_metrics.csv --no-regress NH4,DO,TP,NO3,TN --max-rise 5
```
Expected: guard now passes. Refresh BOTH doc tables (KM + EPA) from the `*_kmin10` metrics — re-scoring EPA alone would leave the KM table describing a K_MIN=1.13 config that no longer ships.

- [ ] **Step 3: Update the doc's post-promotion outcome line** to reflect K_MIN=1.0, and amend the Task 3 commit's values in the doc table.

- [ ] **Step 4: Commit**

```bash
git add tools/eutropy_poc/eutropy_to_estas.py docs/CL29_KM_2022-2023_Validation.md
git commit -m "fix(converter): revert K_MIN to 1.0 — 1.13 regressed EPA NO3/TN"
```

---

### Task 7: Close loose ends

**Files:**
- Modify: `docs/superpowers/specs/2026-07-21-cl29-pest-calibration-design.md` (header), `CHANGELOG.md`

- [ ] **Step 1: Close the open-follow-up in the calibration spec header.** In `2026-07-21-cl29-pest-calibration-design.md`, change the `**Open follow-up:**` sentence to note the promotion landed (KDISS=0.118 + K_MIN=<final value>) with a pointer to this plan. Be precise about the un-promoted params — do NOT lump them as "non-identifiable": `KG_DIA_OPT_TEMP` / `KD_DIA_20` were **evaluated and rejected** (KG worsens Si/Chl-a/NH4; KD is r=0.84-tied to KG), while only `KHS_DSi_DIA` is **genuinely non-identifiable**.

- [ ] **Step 2: Add a CHANGELOG line** under `## [Unreleased]`:

```markdown
- CL29 converter defaults now carry the calibrated POP→PO4 dissolution rate
  (`KDISS_DET_PART_ORG_P_20`=0.118) and denitrification rate
  (`K_MIN_DOC_NO3N_20`=<final>), promoted from the pestpp-ies calibration (PR #54/#56)
  and validated before/after against the KM and EPA windows.
```

- [ ] **Step 3: Confirm scope boundaries held** (no edits expected):

```bash
git status --porcelain                     # tracked-modified: only converter, both docs, CHANGELOG, Task-1 tool/test
git status --porcelain pest/cl29.pst       # MUST be empty (frozen — catches BOTH staged and unstaged)
grep -c KG_DIA_OPT_TEMP docs/PAPER_VS_CODE_ANALYSIS.md  # the rejected param appears by NAME only there; do NOT edit its value
```
Expected: `pest/cl29.pst` line is empty (unchanged). The `grep -c` returns a nonzero count (the name is present as documentation) — confirming there is nothing to edit there, since the *promoted* params (KDISS/K_MIN) don't appear in that file at all. Untracked run artifacts (`INPUTS_CL29/`, `OUTPUTS_CL29/`, `epa_observations_out/`, scratchpad metrics) are gitignored/expected — they must NOT be committed; only the tracked files listed above are staged.

- [ ] **Step 4: Full converter/pest test sweep**

Run: `python -m pytest tests/python/ -q`
Expected: all pass (no test asserts `CL29_WCONST_OVERRIDE` contents).

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/specs/2026-07-21-cl29-pest-calibration-design.md CHANGELOG.md
git commit -m "docs: close pest-calibration open-follow-up + CHANGELOG for promotion"
```

---

### Task 8: Open the PR

- [ ] **Step 1: Push + open PR**

```bash
git push -u origin feat/cl29-pest-posterior-promotion
```
Then `gh pr create --base main --head feat/cl29-pest-posterior-promotion` with a body covering: what promoted (KDISS + K_MIN<final>), why KG was rejected (verified Si/Chl-a/NH4 regression), the before/after validation tables for both windows, and the guard result. Use `--body-file` (backtick-safe).

- [ ] **Step 2: Poll CI, merge on green** per the established pattern (docs+python only; python-lint-test + the checks rollup; CodeRabbit is non-blocking).

---

## Notes for the executor

- The two runs are the long pole (~9 min each, +~9 if the fallback fires). Everything else is seconds.
- If `run_cl29.sh` ever crashes near day ~449, the `ESTAS_HOLD_VOLUME=1` guard (baked into the wrapper) was bypassed — always go through `run_cl29.sh`, never `./ESTAS_II` directly.
- Determinism: advanced-redox=1 is deterministic since PR #24, so baseline and promoted differ only by the two constants.
- The optional **KM-2023** typical-year check (spec) is deferred — it needs the separate climatology forcing (`INPUT_CL29_2023clim.txt`, sim-end 4382) and is not a gate. Add it only if the KM-2022 + EPA result is ambiguous and a second window would help adjudicate.
