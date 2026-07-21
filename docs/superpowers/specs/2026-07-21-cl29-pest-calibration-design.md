# CL29 PEST++ calibration setup — design

**Status:** ✅ Implemented + calibration converged. PR #54 (`932ed32`) built the files; the full `pestpp-ies` run (50 reals × 3 iters) converged Φ 4058→1287 (std 1344→2.6), recorded in `docs/CL29_KM_2022-2023_Validation.md` (PR #56). The "actual calibration is a compute job the user runs" framing below was fulfilled. **Follow-up — promotion attempted 2026-07-21, ABANDONED:** promoting `KDISS_DET_PART_ORG_P_20`=0.118 + `K_MIN_DOC_NO3N_20`=1.13 into the converter default was validated with a measured two-run before/after and reverted — the 2022 posteriors do **not** transfer to the 11-year record (`KDISS` closes PO4 but induces P-limitation regressing EPA NH4/NO3/Si/Chl-a; nonstationarity between the 2012–16 hyperbloom and 2022 low-P regimes). `KG_DIA_OPT_TEMP` was rejected earlier (worsens Si/Chl-a/NH4). See `2026-07-21-cl29-pest-posterior-promotion-design.md` and the "Promotion attempt (abandoned)" section of `docs/CL29_KM_2022-2023_Validation.md`. The reusable `tools/compare_validation_runs.py` came out of this.

**Goal.** Feed the ingested KM (Curonian Lagoon) observations into a runnable PEST++
calibration of the 29-box CL29 model, targeting the biases documented in
`docs/CL29_KM_2022-2023_Validation.md` (PO4/Si/Chl-a over-prediction, NO3 under-prediction).

**Constraint.** A full 29-box, 11-year forward run is ~9 min. Classic PEST (GLM) is
impractical single-threaded; the setup targets **`pestpp-ies`** (iterative ensemble
smoother — parallelizable, no Jacobian). We build and self-verify the files here; the actual
calibration is a compute job the user runs on multi-core hardware.

## Components (`pest/`)

1. **`build_pest.py`** — generates, from a parameter spec + `tools/ingest_km_observations.py`
   output:
   - `wconst_04.tpl` — `INPUTS_CL29/WCONST_04.txt` with each calibrated parameter's *value*
     token replaced by a `@NAME@` field. `READ_MODEL_CONSTANTS` reads `index name value`
     free-format and stores by index, so the templated line stays parse-compatible.
   - PEST **observations** from `km_observations_tidy.csv`, restricted to the model window
     (day ≤ `SIMULATION_END`; the KM 2022 obs). One obs per (box, variable, date), obs group
     = variable, **weight = 1 / group-mean-|value|** so the seven variables (NO3~0.5,
     PO4~0.01, Chl~30 …) contribute comparably to Φ. Obs names `‹var›_‹box›_‹seq›` (≤ 20 ch).
   - `model_obs.ins` — `pif @` + one `l1 !name!` per obs (so the forward output is one value
     per obs, in `.pst` order).
   - `cl29.pst` — control file: parameter groups/data (transform + bounds from the spec,
     initial value read from `WCONST_04.txt`), observation groups/data, model command
     (`python pest/forward_run.py`), the tpl→`WCONST_04.txt` and `model_obs.ins`→`model_obs.out`
     I/O pairs, and `++` options for `pestpp-ies` (num_reals, parameter/obs ensembles).

2. **`forward_run.py`** — the PEST forward model. PEST first writes the filled
   `INPUTS_CL29/WCONST_04.txt` from the template; this script then: runs
   `ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt`, loads each box's output and interpolates
   the modelled series to every obs date **reusing `tools/validate_cl29_vs_epa.py`
   (`load_box_output`, `add_derived`, `MODEL_COL`)**, and writes `model_obs.out` — one value
   per obs, in the order `build_pest.py` emitted.

3. **`pest/README.md`** — run instructions (`pestpp-ies pest/cl29.pst` + parallel
   `pestpp-ies ... /h :4000` workers), the forward-run-cost note, and how to re-target
   parameters/obs.

## Parameter spec (initial; edit in `build_pest.py`)

| WCONST name | bias addressed | transform | lower | upper |
|---|---|---|---|---|
| `K_MIN_DOC_NO3N_20` | NO3 under-pred (denitrification) | log | 0.1 | 5 |
| `KDISS_DET_PART_ORG_P_20` | PO4 over-pred (POP dissolution) | log | 0.1 | 10 |
| `KHS_DSi_DIA` | Si over-pred (diatom Si half-sat) | log | 0.005 | 0.15 |
| `KG_DIA_OPT_TEMP` | Chl/Si (diatom growth) | none | 1 | 6 |
| `KD_DIA_20` | Chl over-pred (diatom mortality) | log | 0.02 | 0.5 |

## Verification (here)

- `build_pest.py` runs and emits `wconst_04.tpl`, `model_obs.ins`, `cl29.pst`; the tpl fills
  with a `real` value the Fortran reads; obs/instruction counts agree with the `.pst`.
- One `forward_run.py` executes end-to-end (run CL29 → `model_obs.out` with the right count).
- Unit tests: value read, template fill, KM-obs→PEST conversion + weighting, `.ins`/`.pst`
  counts.

The full ensemble calibration is **not** run here (compute).
