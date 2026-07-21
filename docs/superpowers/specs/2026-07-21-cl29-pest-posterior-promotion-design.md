# CL29 PEST-posterior promotion — design

**Date:** 2026-07-21
**Status:** Design — awaiting user review
**Author:** Arturas Razinkovas-Baziukas (with Claude)
**Scope:** `tools/eutropy_poc/eutropy_to_estas.py` (converter default) + `docs/CL29_KM_2022-2023_Validation.md`. No Fortran changes.

## Goal

Promote the **data-identified** parameters from the converged `pestpp-ies` calibration
(PR #54/#56; Φ 4058→1287, ensemble std 1344→2.6) into the CL29 converter default so that a
fresh `INPUTS_CL29/` carries the calibrated biogeochemistry instead of the pre-calibration
values, and **validate** that the promotion actually closes the documented biases without
regressing the broader 11-year fit.

This is the open follow-up flagged in the header of
`2026-07-21-cl29-pest-calibration-design.md`.

## Background — what PEST actually calibrated (reconciliation)

The forward model (`pest/forward_run.py`) runs ESTAS directly against the committed
`INPUTS_CL29/`; it does **not** re-run the converter. So the calibration started from the
converter-output values (post-`CL29_WCONST_OVERRIDE`), recorded as point-initials in
`pest/cl29.pst`:

| param | converter default (start) | PEST posterior mean | ±std | identified? |
|---|---:|---:|---:|---|
| `KDISS_DET_PART_ORG_P_20` (POP→PO4 dissolution) | 3.48 | **0.118** | ±0.004 | ✅ tight — dominant PO4 fix (~30× cut) |
| `KG_DIA_OPT_TEMP` (diatom growth) | 3.70 | **5.45** | ±0.25 | ✅ well constrained — ↑ Si/P uptake |
| `K_MIN_DOC_NO3N_20` (denitrification) | 1.0 (EPA-derived override) | **1.13** | ±0.07 | ✅ constrained — barely moved from EPA 1.0 |
| `KHS_DSi_DIA` (diatom Si half-sat) | 0.013 | 0.036 | ±0.023 | ❌ not identified (σ ≈ value) |
| `KD_DIA_20` (diatom mortality) | 0.12 | 0.174 | ±0.016 | ❌ essentially unchanged |

> The "initial" column in `CL29_KM_2022-2023_Validation.md` (4.10 / 3.58 / 1.55 / …) is the
> iteration-0 *ensemble* mean (stochastic log-normal draw), not the point-initial the control
> file used. The **posterior means** above are what get promoted.

## The change

Add/update three entries in `CL29_WCONST_OVERRIDE`
(`tools/eutropy_poc/eutropy_to_estas.py`), each with a comment block citing the PEST run
(PR #54/#56, Φ 4058→1287, ±posterior std) in the same documented-override style already used
for `K_MIN_DOC_NO3N_20`:

```python
"KDISS_DET_PART_ORG_P_20": 0.118,   # PEST posterior (±0.004); 30x cut, tightest-constrained, dominant PO4 fix
"KG_DIA_OPT_TEMP":         5.45,    # PEST posterior (±0.25); raises diatom growth -> Si/P uptake
"K_MIN_DOC_NO3N_20":       1.13,    # PEST posterior (±0.07); refines the EPA-derived 1.0 (they agree within noise -> cross-validated)
```

- `K_MIN_DOC_NO3N_20`: **update in place** (1.0 → 1.13); rewrite the existing comment to note the
  PEST posterior confirms the EPA-2012-2022 value to within 13% (a cross-validation, not a
  contradiction).
- `KHS_DSi_DIA`, `KD_DIA_20`: **deliberately NOT promoted.** Add a short comment recording why
  (posteriors non-identifiable — as wide as / equal to the prior; the Si correction rides on
  `KG_DIA_OPT_TEMP`, not the half-sat). This keeps the honest-calibration rationale in the code
  for the next reader.

No change to the diatom settling override (0.1 m/day) or any other existing override.

## Validation gate (both windows)

The promotion is not "done" until a run confirms it. Procedure:

1. Regenerate `INPUTS_CL29/` via the converter (picks up the new overrides).
2. Build ESTAS (`make FC=gfortran build-estas`), run the full 2012–2022 sim via `run_cl29.sh`
   (`ESTAS_HOLD_VOLUME=1`).
3. **KM 2022–2023** (calibration target): score `pest/km_observations_tidy.csv` by
   interpolating the modeled series to each obs date — reusing the modeled-series loader
   (`load_box_output` / `MODEL_COL`) from `validate_cl29_vs_epa.py` exactly as
   `forward_run.py` does. Confirm the PO4 / Si / Chl-a over-prediction **closes** and the NO3
   under-prediction **improves**, consistent with the ensemble's Φ drop.
4. **EPA 2012–2021** (regression guard, via `tools/validate_cl29_vs_epa.py` against the EPA
   obs): confirm the broader 11-year fit does **not** regress — watch NO3/TN especially (the
   EPA-anchored `K_MIN` moved 1.0→1.13) and DO.

**Success criteria.** KM PO4/Si/Chl-a bias magnitudes drop meaningfully and NO3 improves;
EPA aggregate RMSE for every scored variable stays within ~±5% of its pre-promotion value
(no variable regresses by more than noise). Record the before/after numbers for both windows.

## Deliverables (single PR)

1. The converter edit (3 override entries + non-promotion comment).
2. `docs/CL29_KM_2022-2023_Validation.md` updated with the **post-promotion** bias table for
   both windows (the doc's "Validation" section already names this as the next step).
3. Existing converter tests still green (`tests/python/test_build_pest.py`,
   `tests/fortran/test_defaults.f90`, and the converter's own tests) — the override change must
   not break the WCONST writer or the parameter-count/defaults checks.

## Risk / rollback

- **KM improves but EPA regresses.** The change is three dict entries — trivially reversible.
  Documented fallback: promote **only** `KDISS_DET_PART_ORG_P_20` (the PO4 lever both windows
  should agree on, since PO4 is over-predicted in both), and leave `KG_DIA_OPT_TEMP` /
  `K_MIN` at their pre-promotion values. Decide this from the validation numbers, not upfront.
- **Diatom-growth interaction.** `KG_DIA_OPT_TEMP`=5.45 lifts diatom uptake and interacts with
  the existing 0.1 m/day settling override and the box-19 P-retention story; the KM Chl-a /
  box-19 diatom check in step 3 covers this — flag if the bloom over-shoots.
- **Determinism.** CL29 ships advanced-redox=1 (deterministic since the FLAGS-shadowing fix,
  PR #24), so the validation run is reproducible.

## Non-goals

- The two non-identifiable parameters (`KHS_DSi_DIA`, `KD_DIA_20`): out of scope. Constraining
  them would need Si/Chl observations added to the PEST obs set — a separate calibration task.
- No re-run of `pestpp-ies` itself; this promotes the already-converged posteriors.
- No Fortran / model-structure changes.
