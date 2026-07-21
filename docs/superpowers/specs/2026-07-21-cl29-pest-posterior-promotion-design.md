# CL29 PEST-posterior promotion — design

**Date:** 2026-07-21
**Status:** 🪦 ABANDONED after validation (2026-07-21). The promotion was implemented and validated with a measured two-run before/after (KM-2022 + EPA-2012-2021), then **reverted**: `KDISS`=0.118 closes PO4 in both windows but induces P-limitation that regresses EPA NH4/NO3/Si/Chl-a — the 2022 posteriors do not transfer to the 11-year record (nonstationarity). No default change shipped. The reusable `tools/compare_validation_runs.py` and the negative-result write-up (`docs/CL29_KM_2022-2023_Validation.md`, "Promotion attempt (abandoned)") were retained. See that section for the numbers.
**Author:** Arturas Razinkovas-Baziukas (with Claude)
**Scope:** `tools/eutropy_poc/eutropy_to_estas.py` (converter default) + `docs/CL29_KM_2022-2023_Validation.md`. No Fortran changes.

## Goal

Promote the **robust, physically-defensible** parameters from the converged `pestpp-ies`
calibration (PR #54/#56; Φ 4058→1287) into the CL29 converter default, and **validate** the
promotion with an explicit before/after run against both the KM calibration window and the
broader EPA record. This resolves the open follow-up in the header of
`2026-07-21-cl29-pest-calibration-design.md`.

## What changed after review (rev. 2)

The first draft proposed promoting "the 3 identified levers" (KDISS + KG_DIA_OPT_TEMP + K_MIN).
An adversarial in-loop review — cross-checked by recomputing the posterior fit directly from the
committed ensemble outputs (`cl29.3.obs.csv`) — **falsified that plan**:

**Verified posterior fit vs the default-config baseline (KM 2022, ensemble mean over 50 reals):**

| var | default bias | posterior bias | verdict |
|---|---:|---:|---|
| PO4 | +0.025 | **−0.002** | ✅ closes |
| NO3 | −0.344 | −0.154 | ✅ improves (joint effect) |
| Si | +0.56 | **+1.20** | ❌ **doubled — worse** |
| Chl-a | +9.48 | **−6.09** | ❌ **flips to under-prediction** |
| NH4 | +0.002 | **+0.031** | ❌ ~15× worse |
| TN | +0.445 | +0.450 | ~unchanged |

Consequences that reshaped the scope:

- **`KG_DIA_OPT_TEMP` (3.70→5.45) is dropped.** The validation doc claims "the Si correction rides
  on diatom growth" — the data say the opposite. With `MODEL_SEDIMENTS=0` there is **no biogenic-Si
  burial sink**, so raising diatom growth recycles *more* Si to dissolved (Si +0.56→+1.20),
  overshoots Chl-a into under-prediction, and degrades NH4 (the model's best-fit variable). It is
  the lever *causing* the regressions, not fixing them.
- **`KD_DIA_20` is not "non-identifiable."** It is identified (89% posterior-variance reduction) and
  correlated with `KG_DIA_OPT_TEMP` at r=0.84 — growth and mortality co-vary because the data
  constrain *net* accumulation. It cannot be split from `KG`; since `KG` is dropped, `KD` is too.
- **`KHS_DSi_DIA`** is the only genuinely unconstrained parameter (posterior *wider* than prior);
  correctly left at default.
- **`K_MIN` 1.0→1.13 is ~1.9σ**, not "agreement within noise." Retained per user decision, but
  gated on the validation (below), because its standalone effect on NO3 is *not* the joint
  posterior's NO3 gain.

## Background — what PEST actually calibrated (reconciliation)

`pest/forward_run.py` runs ESTAS directly against the committed `INPUTS_CL29/`; it does **not**
re-run the converter. So the calibration started from the converter-output values (post-override),
recorded as point-initials in `pest/cl29.pst`:

| param | converter default (start) | PEST posterior mean | ±std | promote? |
|---|---:|---:|---:|---|
| `KDISS_DET_PART_ORG_P_20` | 3.48 | **0.118** | ±0.004 | ✅ yes — dominant PO4 fix |
| `K_MIN_DOC_NO3N_20` | 1.0 (EPA override) | **1.13** | ±0.07 | ✅ yes (gated on NO3 check) |
| `KG_DIA_OPT_TEMP` | 3.70 | 5.45 | ±0.25 | ❌ no — worsens Si/Chl-a/NH4 |
| `KD_DIA_20` | 0.12 | 0.174 | ±0.016 | ❌ no — identified but tied to KG |
| `KHS_DSi_DIA` | 0.013 | 0.036 | ±0.023 | ❌ no — non-identifiable |

> The "initial" column in `CL29_KM_2022-2023_Validation.md` (4.10 / 3.58 / 1.55 / …) is the
> iteration-0 *ensemble* mean (stochastic log-normal draw), not the point-initial the control file
> used. The **posterior means** above are what get promoted.

## The change

Two entries in `CL29_WCONST_OVERRIDE` (`tools/eutropy_poc/eutropy_to_estas.py`):

```python
"KDISS_DET_PART_ORG_P_20": 0.118,   # PEST posterior (±0.004); ~30x cut vs default 3.48.
                                    # Tightest-constrained param (99.9% var-reduction); closes PO4
                                    # over-prediction in BOTH KM-2022 and EPA windows. Physically
                                    # sensible: dissolution e-fold ~8.5 d vs implausible ~7 h at 3.48.
                                    # Also partly compensates the disabled sediment-P sink (MODEL_SEDIMENTS=0).
"K_MIN_DOC_NO3N_20": 1.13,          # was 1.0 (EPA 2012-2022). PEST posterior (±0.07) refines it;
                                    # consistent to ~1.9σ across two independent windows. Gated on the
                                    # NO3 regression check (fallback to 1.0 if NO3/TN regress on EPA).
```

- `KDISS_DET_PART_ORG_P_20`: **add** (currently at template default 3.48, not overridden).
- `K_MIN_DOC_NO3N_20`: **update in place** 1.0 → 1.13. **Keep** the existing comment's EPA-at-1.0
  figures ("1.0 brings NO3 bias +0.31→+0.06 (RMSE −31%)…"), clearly attributed to the EPA-1.0
  evaluation — do not relabel them as 1.13's effect; append the PEST cross-validation note.
- Add a short comment recording that `KG_DIA_OPT_TEMP` / `KD_DIA_20` / `KHS_DSi_DIA` were
  **evaluated and deliberately not promoted** (KG worsens Si/Chl-a/NH4 with no biogenic-Si sink;
  KHS non-identifiable), so the next reader sees the rationale.

Mechanism is safe: `_apply_wconst_overrides` raises `SystemExit` if a name isn't found exactly once
(not a silent no-op); both names exist once in `INPUTS/WCONST_04.txt`. No diatom-settling or other
override changes.

## Validation gate — explicit two-run before/after protocol

There is **no committed baseline metrics table** to diff against, so the promotion must be measured
with two runs on **one freshly-built binary** and the **same obs CSVs**:

1. **Build once.** `make FC=gfortran OPENMP=1 build-estas` (CL29 default is `MODEL_SEDIMENTS=0`, so
   OpenMP is safe here — the sediment-path deadlock does not apply). Use this one binary for all runs.
2. **Baseline run.** Current converter (KDISS=3.48, K_MIN=1.0) → regen `INPUTS_CL29/` → `run_cl29.sh`
   (2012–2022, `ESTAS_HOLD_VOLUME=1`) → score. This is the "before".
3. **Promoted run.** Apply the edit → regen `INPUTS_CL29/` → `run_cl29.sh` → score. This is the "after".
4. **Score each run on two windows:**
   - **KM 2022** (calibration target): `python tools/validate_cl29_vs_epa.py --obs pest/km_observations_tidy.csv --outputs OUTPUTS_CL29 --base-year 2012` (its `load_obs` is generic — verified). Use the **committed** KM CSV for both runs (it already carries the post-LTK14-fix box map; the doc's old table used box 25 — do not diff against that).
   - **EPA 2012–2021** (regression guard): first generate the EPA obs — `python tools/ingest_epa_observations.py` (source workbooks at `~/curonian/DATA/JTD/timeSeries_*_EPA.xlsx`) → `epa_observations_out/epa_observations_tidy.csv`; then `validate_cl29_vs_epa.py --obs <that> --outputs OUTPUTS_CL29 --base-year 2012`.
   - (Optional) **KM 2023**: a typical-year check only, via the separate climatology forcing
     (`INPUT_CL29_2023clim.txt`, `SIMULATION_END=4382`) → `OUTPUTS_CL29_2023clim/`, scored against
     2023-filtered obs. Not a gate — the 2012–2022 run cannot reach 2023.

### Success criteria (quantified, per variable)

- **KM PO4** (calibration target): |bias| drops from ~0.025 toward ≈0 (expect near-closure).
- **EPA PO4**: |bias|/RMSE drops (PO4 is over-predicted on EPA too).
- **Regression guard (one-sided):** for variables *not* targeted — **NH4, DO, TP** — EPA RMSE must
  **not increase** by more than **5%**. (A symmetric band is wrong: PO4/NO3 are *meant* to move a
  lot; only unintended degradation is a failure.)
- **NO3/TN diagnostic:** report the change on both windows. K_MIN=1.13 = *more* denitrification =
  *lower* NO3; NO3 is already under-predicted, so if EPA NO3 or TN RMSE **worsens > 5%**, that is the
  signal to **revert K_MIN to 1.0** (fallback below) and re-score.
- **Nonstationarity watch:** KDISS was fit to the de-eutrophicated 2022 regime but becomes the
  default for the 2012–2016 hyperbloom era (summer P-PO4 ~25 µg/L then vs 2–3 now). Inspect EPA PO4
  in the early years specifically for large *under*-prediction; a 30× dissolution cut could starve
  the early blooms of P. Report it; a modest early-year PO4 under-shoot is acceptable, a collapse is not.

## Deliverables (single PR)

1. Converter edit (2 override entries + not-promoted rationale comment).
2. `docs/CL29_KM_2022-2023_Validation.md` reconciled — edit **all four** anchors so the doc is
   self-consistent: (a) the 2022 Results bias table, (b) the 2023 Results table if the optional run
   is done, (c) the **Interpretation** prose ("PO4/Si/TN/Chl-a over-predicted…"), (d) the
   next-step line at the end of `## Calibration (pestpp-ies)` (mark it done). Carry the point-initial
   vs ensemble-0-mean reconciliation note into the doc so its "initial" column isn't doubly-defined.
   Add the before/after metrics tables for both windows.
3. Update the header of `2026-07-21-cl29-pest-calibration-design.md` — its "**Open follow-up:** not
   yet promoted" note is resolved by this change.
4. `CHANGELOG.md` `[Unreleased]`: one line noting the calibrated PO4/denitrification defaults.
5. **Freeze `pest/cl29.pst`** — do NOT regenerate it. It is the historical record of the run that
   produced the posteriors (its point-initials 3.48/1.0 document the pre-promotion state);
   regenerating would erase that provenance. `pest/wconst_04.tpl` and `pest/model_obs.ins` are
   value-invariant (placeholders only), so no action there.
6. Existing tests stay green — verified: `test_build_pest.py` uses a `tmp_path` fixture (never reads
   the real converter output), and `test_defaults.f90` asserts Fortran-source defaults, which the
   converter's post-hoc override does not touch. No test asserts `CL29_WCONST_OVERRIDE` contents.

Out of scope (names only, no values): `docs/PAPER_VS_CODE_ANALYSIS.md` references the parameter names
but not the 3.48/3.7 values — confirm, don't edit.

## Risk / rollback

- **NO3/TN regresses on EPA** (K_MIN=1.13 pushing denitrification): revert `K_MIN` to **1.0**, keep
  KDISS. Decided from the validation numbers, not upfront.
- **KDISS starves early-era PO4**: if the 2012–2016 EPA PO4 collapses, KDISS is over-cut for that
  regime; document and consider a milder value — but PO4 is over-predicted there too, so a cut is
  directionally right.
- Everything is dict entries — trivially reversible; the ultimate floor is "promote nothing."

## Non-goals

- `KG_DIA_OPT_TEMP`, `KD_DIA_20`, `KHS_DSi_DIA` promotion — excluded above.
- The real Si fix (a biogenic-Si burial sink; enabling `MODEL_SEDIMENTS`, or adding Si/Chl obs to the
  PEST set and re-calibrating diatoms) — a separate, larger task. This spec only promotes the
  P/denitrification levers that are defensible today.
- No re-run of `pestpp-ies`; no Fortran / model-structure changes.
