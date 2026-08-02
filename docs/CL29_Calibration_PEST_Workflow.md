# CL29 calibration workflow (external PEST++)

How AQUABC's reference application (the 29-box Curonian Lagoon, **CL29**) is calibrated: an
**external** PEST++ workflow that treats the compiled model as a black box, perturbs its input
constants, and minimises a weighted misfit against observations.

## 1. The calibration objective lives *outside* the Fortran

AQUABC's calibration objective (Φ) is realised by the **external PEST++ harness in [`pest/`](../pest/)**,
**not** by the in-model `mod_COST_FUNCTION`. `SOURCE_CODE/ESTAS/mod_COST_FUNCTION.f90` is a deliberate
**stub** — it defines a `MEASURED_VALUE_DS` type (a box → forcing-time-series mapping for measured
values) plus its allocate/deallocate, and computes no objective. This is by design, not an unfinished
gap:

- A full 29-box, 11-year CL29 forward run is a **~9-minute compute job**, so classic derivative-based
  calibration (a Jacobian = one run per parameter per iteration) is impractical. The workflow instead
  uses an **iterative ensemble smoother** (`pestpp-ies`) — no Jacobian, parallelises over an ensemble.
- The misfit is scored by **reusing the validation tooling** (`tools/validate_cl29_vs_epa.py`) rather
  than re-implementing observed-vs-modelled interpolation in Fortran, so Φ and the validation report are
  guaranteed to agree.

If an in-model objective is ever wanted (e.g. to drive an internal optimiser), `MEASURED_VALUE_DS` is
the scaffold to build on — but the external route is the supported, reproducible one.

## 2. Architecture & data flow

```
 PARAMS spec (pest/build_pest.py)
        │  generates
        ▼
 wconst_04.tpl   +   model_obs.ins   +   cl29.pst          (+ km_observations_tidy.csv obs)
        │
        ▼   pestpp-ies loop, per ensemble member per iteration:
   1. fill template  →  INPUTS_CL29/WCONST_04.txt   (read by index via READ_MODEL_CONSTANTS)
   2. pest/forward_run.py:  ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt
   3. score:  interpolate each box's output to the obs dates via validate_cl29_vs_epa.py
              →  write model_obs.out  (one value per obs, in .pst order)
   4. PEST reads model_obs.out via model_obs.ins  →  updates Φ  →  proposes the next ensemble
```

The parameters are ordinary `WCONST_04.txt` model constants. The template replaces each calibrated
constant's *value* token with a `@NAME@` field; because the ESTAS reader **`READ_MODEL_CONSTANTS`**
(`SOURCE_CODE/ESTAS/mod_UTILS_01.f90`) parses each line free-format as `index name value` and stores **by
index**, the filled template stays parse-compatible (the name column is ignored at read time).

## 3. The objective function (Φ)

Φ is a **weighted sum of squared residuals** between the modelled series (interpolated to each
observation's date) and the observations. Weighting is `weight = 1 / mean(|value|)` per variable group,
so variables on very different scales (e.g. NO3 ≈ 0.5, PO4 ≈ 0.01, Chl-a ≈ 30) contribute comparably to
Φ. Observations are one per `(box, variable, date)`, grouped by variable; the mapped variables are the
direct state variables (NH4, NO3, PO4, DO, Si) plus the reconstructed pools (TN, TP, Chl-a) — the same
set `tools/validate_cl29_vs_epa.py` scores. Because `forward_run.py` scores through that exact module
(`load_box_output`, `add_derived`, `MODEL_COL`), **Φ and the EPA/KM validation report use identical
observed-vs-modelled interpolation.**

## 4. Parameters — and re-targeting the calibration

The calibrated constants and their intent (initial spec; edit `PARAMS` in `pest/build_pest.py` to
change the set, bounds, or transform):

| WCONST constant | bias addressed | transform | lower | upper |
|---|---|---|---|---|
| `K_MIN_DOC_NO3N_20` | NO3 under-prediction (denitrification) | log | 0.1 | 5 |
| `KDISS_DET_PART_ORG_P_20` | PO4 over-prediction (POP dissolution) | log | 0.1 | 10 |
| `KHS_DSi_DIA` | Si over-prediction (diatom Si half-saturation) | log | 0.005 | 0.15 |
| `KG_DIA_OPT_TEMP` | Chl-a / Si (diatom growth) | none | 1 | 6 |
| `KD_DIA_20` | Chl-a over-prediction (diatom mortality) | log | 0.02 | 0.5 |

Re-target by editing `PARAMS` (parameters) and `obs_weights` (per-group weighting) in
`pest/build_pest.py`, then regenerating the PEST files (§5).

## 5. Running it

The build + run mechanics — regenerating `wconst_04.tpl` / `model_obs.ins` / `cl29.pst`, and running
`pestpp-ies` serially or as a master + N parallel workers — are documented in **[`pest/README.md`](../pest/README.md)**
(the operational how-to). In short:

```sh
python pest/build_pest.py                 # regenerate the PEST files from PARAMS + obs
pestpp-ies pest/cl29.pst                   # serial (one ~9-min run per member per iteration)
# parallel: pestpp-ies pest/cl29.pst /h :4000  + worker copies pointing at localhost:4000
```

Outputs: `cl29.N.par.csv` / `cl29.N.obs.csv` (per-iteration ensembles) and `cl29.N.phi.*` (Φ history).

## 6. Instances & the key lesson

Two harness instances follow this pattern:

- **[`pest/`](../pest/)** — the CL29-vs-KM (Kuršių marios 2022) calibration. **Converged:** a
  `pestpp-ies` run (50 realisations × 3 iterations) drove Φ 4058 → 1287 (ensemble std 1344 → 2.6).
- **`pest_fixcyn/`** — an exploratory self-contained copy targeting the FIX_CYN (N-fixer) group.
  **Abandoned / won't-fix** (the FIX_CYN biomass target is not reproducible without breaking the
  multivariate fit; see `docs/CL29_EPA_Calibration_Summary.md`). Kept locally as a worked example; not
  tracked in git.

**⚠️ The load-bearing lesson: posteriors do not necessarily transfer across regimes.** Promoting the
2022 KM posterior (`KDISS_DET_PART_ORG_P_20 = 0.118`, `K_MIN_DOC_NO3N_20 = 1.13`) into the model default
was validated with a measured before/after and **reverted** — it closes 2022 PO4 but induces
P-limitation that regresses the EPA 2012–2016 NH4/NO3/Si/Chl-a, because the 2012–16 hyperbloom and the
2022 low-P regime are non-stationary. Calibrate against the era you intend to run, and validate any
promotion across the full record with `tools/compare_validation_runs.py`.

## 7. Results & validation

- **[`docs/CL29_KM_2022-2023_Validation.md`](CL29_KM_2022-2023_Validation.md)** — the KM 2022–2023
  validation and the (abandoned) promotion attempt.
- **[`docs/CL29_EPA_Calibration_Summary.md`](CL29_EPA_Calibration_Summary.md)** — the full calibration
  arc against the EPA 2012–2022 record: what was tuned (denitrification, benthic denit), the documented
  PO4/Si residuals, and what was ruled out with evidence.

## 8. Reproducing the observations

The calibration observations come from the ingestion tools, not committed by hand:

```sh
python tools/ingest_km_observations.py --out-dir /tmp/km
cp /tmp/km/km_observations_tidy.csv pest/
```

(`tools/ingest_epa_observations.py` produces the EPA equivalent used by
`tools/validate_cl29_vs_epa.py`.)

## Design record

Setup design: [`docs/superpowers/specs/2026-07-21-cl29-pest-calibration-design.md`](superpowers/specs/2026-07-21-cl29-pest-calibration-design.md).
Promotion analysis: [`docs/superpowers/specs/2026-07-21-cl29-pest-posterior-promotion-design.md`](superpowers/specs/2026-07-21-cl29-pest-posterior-promotion-design.md).
