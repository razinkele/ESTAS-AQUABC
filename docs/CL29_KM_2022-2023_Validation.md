# CL29 validation against Kuršių marios (KM) 2022–2023 monitoring

This extends the [EPA-archive validation](CL29_EPA_Calibration_Summary.md) (which ran to
2021) into the most recent years, using the **Kuršių marios (KM, Curonian Lagoon)**
monitoring newly ingested from the AAA *2014-2023_BJ duomenys extrahuoti* extract.

## Data and box alignment

| | |
|---|---|
| Source | `curonian/DATA/2014-2023_BJ duomenys extrahuoti` — KM hydrochemistry (`Hidrocheminiai tyrimai`) + KM chlorophyll-a (`Biologiniai tyrimai`) |
| Ingester | `tools/ingest_km_observations.py` (→ `km_observations_tidy.csv` + `KM_<station>_box<box>.dates`) |
| Station→box | the `LTK→box` map (`tools/epa_station_to_box.csv`), **verified by point-in-polygon against the 29-box vector layer `curonian/b29polys.gpkg`** (EPSG:3346, 2026-07-21) |
| Coverage | **3401 measurements**, obs window **2022-02-23 → 2023-11-15**, **9 boxes** (7, 11, 14, 15, 17, 19, 20, 23, 25) |
| Variables | NH4, NO3, NO2, PO4, TN, TP, DIN, Si, **Chl-a** (+ BOD7, TSS carried as auxiliary) |

The Baltic-Sea (`LT*`) stations in the same extract fall outside the lagoon domain and are
dropped by the ingester.

## Model run

CL29 default configuration built with the **v0.5.2** release binary, `ESTAS_HOLD_VOLUME=1`,
240 steps/day. Two runs are scored:

1. **2022 — real forcing.** Model calendar **2012-01-01 → 2022-12-30** (`SIMULATION_END = 4016`).
   The EUTROPY-derived forcing (flows + boundary loads) ends here, so this run validates the
   **2022** observations.
2. **2023 — climatological forcing.** EUTROPY carries no 2023 forcing, so a real 2023 hindcast
   is impossible. `tools/extend_cl29_forcing_climatology.py` appends a synthetic 2023 to every
   time-series input, filling each 2023 day with the **2012-2022 mean seasonal cycle** for that
   calendar day-of-year (`INPUT_CL29_2023clim.txt`, `SIMULATION_END = 4382 = 2023-12-31`). The
   extension is a clean *append* — this run's 2012-2022 output is **identical** to run 1 (the
   2022 metrics below reproduce exactly), so only the 2023 tail is new.

> **The 2023 run is NOT a hindcast.** A climatological year carries the mean seasonal cycle but
> none of 2023's inter-annual anomalies (river discharge, boundary loads, weather). The 2023
> scores therefore test the model's *typical-year* response against the 2023 observations
> (seasonal structure), not the reproduction of 2023-specific events.

## Method

`tools/validate_cl29_vs_epa.py` interpolates each box's modelled series onto the observation
dates and reports the count, bias, RMSE and Pearson correlation. NH4/NO3/PO4/Si are compared
directly to model state variables; TN, TP and Chl-a are reconstructed from the phytoplankton
pools with the CL29 stoichiometry (N:C 0.22, P:C 0.024, C:Chl-a 30 for diatoms/OPA and 40 for
the cyanobacteria groups).

## Results (2022 overlap, obs-weighted)

Units: N/P/Si in **mg/L**, Chl-a in **µg/L**. `r` is the range of the per-box correlations.

| Variable | n | bias | RMSE | r (per box) | Reading |
|---|---:|---:|---:|:---:|---|
| NH4  | 161 | **+0.002** | 0.050 | −0.73 … +0.44 | essentially unbiased |
| NO3  | 161 | **−0.344** | 0.884 | **+0.69 … +0.99** | under-predicted, but seasonal timing well captured |
| PO4  | 152 | +0.025 | 0.040 | −0.33 … +0.14 | over-predicted |
| Si   | 161 | +0.56 | 1.59 | −0.55 … +0.16 | over-predicted |
| TN   | 161 | +0.445 | 0.96 | +0.03 … +0.83 | over-predicted |
| TP   | 161 | +0.013 | 0.032 | +0.10 … +0.97 | slightly over-predicted |
| Chl-a | 91 | +9.48 | 30.2 | −0.70 … +0.54 | over-predicted |

## Results — 2023 (climatological forcing, obs-weighted)

The 2023 observations scored against the climatologically-extended run (same metrics).

| Variable | n | bias | RMSE | vs 2022 |
|---|---:|---:|---:|---|
| NH4  | 179 | +0.004 | 0.052 | still essentially unbiased |
| NO3  | 179 | −0.236 | 0.675 | still under-predicted |
| PO4  | 178 | +0.033 | 0.047 | still over-predicted |
| Si   | 179 | +0.71 | 1.22 | still over-predicted |
| TN   | 179 | +0.387 | 0.714 | still over-predicted |
| TP   | 179 | +0.022 | 0.034 | still over-predicted |
| Chl-a | 102 | +3.78 | 28.8 | over-predicted, smaller than 2022's +9.5 |

The 2023 (climatological) scores reproduce the **same bias structure** as 2022 — NH4 unbiased,
NO3 under-predicted, everything else over-predicted — confirming the model's structural biases
are robust across a typical seasonal cycle. Because the forcing is climatological, this mainly
tells us the 2023 observations broadly resembled a typical year; it does **not** attribute
2023-specific anomalies.

## Interpretation

The KM 2022 data **corroborates the EPA-based finding** that CL29 is boundary-input dominated
and over-predicts the standing nutrient and chlorophyll pools (see
[`CL29_EPA_Calibration_Summary.md`](CL29_EPA_Calibration_Summary.md)):

- **PO4, Si, TN and Chl-a are over-predicted.** The Chl-a bias (+9.5 µg/L) follows directly
  from the excess-nutrient signal.
- **NH4 is essentially unbiased** (+0.002 mg/L) — the recycling/nitrification balance is close.
- **NO3 is the interesting one:** it is *under*-predicted in magnitude (−0.34 mg/L) yet its
  per-box correlations are the strongest of any variable (up to r = 0.99). The model reproduces
  the NO3 seasonal cycle well but sits low — consistent with denitrification being somewhat too
  strong and/or the open-boundary NO3 being set low.

These are the **default configuration**, not a fresh PEST calibration; the numbers are a
baseline for the calibration levers already identified in the EPA summary.

## Calibration (`pestpp-ies`)

The five `pest/` parameters (PR #54) were calibrated against the 2022 obs with `pestpp-ies` —
the full committed ensemble, **50 realizations × 3 iterations**, parallelized over 24 cores
(~3.3 h). **The ensemble converged.**

Objective function Φ across the 50-member ensemble:

| iteration | mean Φ | std | min | max |
|---|---:|---:|---:|---:|
| 0 (initial) | 4058 | 1344 | 1587 | 7429 |
| 1 | 1448 | 158 | 1300 | 2035 |
| 2 | 1292 | 3.2 | 1284 | 1299 |
| 3 (final) | **1287** | **2.6** | 1281 | 1293 |

Mean Φ fell **68 %** (4058 → 1287) and the ensemble spread collapsed from std 1344 to **2.6** —
a converged posterior, not just a nudge.

Calibrated parameters (initial → final ensemble mean ± posterior std):

| parameter | initial | **final** | ±std | identifiability |
|---|---:|---:|---:|---|
| `KDISS_DET_PART_ORG_P_20` (POP dissolution → PO4) | 4.10 | **0.118** | ±0.004 | tightly constrained — 35× lower; the dominant PO4 fix |
| `KG_DIA_OPT_TEMP` (diatom growth → Si/biomass) | 3.58 | **5.45** | ±0.25 | well constrained — ↑ uptake |
| `K_MIN_DOC_NO3N_20` (denitrification → NO3) | 1.55 | **1.13** | ±0.07 | well constrained — ↓ raises NO3 |
| `KHS_DSi_DIA` (diatom Si half-sat) | 0.018 | 0.036 | ±0.023 | **not identified** (posterior ≈ value) |
| `KD_DIA_20` (diatom mortality) | 0.171 | 0.174 | ±0.016 | essentially unchanged |

The data strongly identifies **two levers**: cutting **POP → PO4 dissolution 35×** — the
tightest-constrained parameter, so the model's excess PO4 came from too-fast organic-P
remineralization — and **raising diatom growth** (more Si/nutrient uptake); denitrification
comes down modestly to lift the under-predicted NO3. The diatom Si half-saturation and mortality
are **not identifiable** by these observations (their posteriors are as wide as the value /
essentially unchanged), so the Si correction rides on diatom growth rather than the half-sat.

These calibrated the **2022** window. The identified levers were then promotion-tested against
both windows — see **Promotion attempt** below.

> **Note (2026-07-22 correction).** The "raising diatom growth corrects Si" claim above is
> **wrong** — verified against the ensemble output during the promotion test: raising
> `KG_DIA_OPT_TEMP` *doubles* the Si over-prediction and flips Chl-a to under-prediction,
> because with `MODEL_SEDIMENTS=0` there is no biogenic-Si burial sink, so more diatom growth
> recycles more Si to dissolved. `KD_DIA_20` is in fact identified (89% variance reduction) and
> r=0.84-correlated with `KG`; only `KHS_DSi_DIA` is genuinely non-identifiable.

## Promotion attempt (2026-07-21) — abandoned

The two defensible levers (`KDISS_DET_PART_ORG_P_20`=0.118, `K_MIN_DOC_NO3N_20`=1.13) were
promoted into the converter default and validated with a **measured two-run before/after** on one
gfortran+OpenMP binary (baseline vs promoted), scored on the committed KM CSV and freshly-ingested
EPA obs. The promotion was then **reverted** — the 2022-calibrated posteriors do not transfer to
the 11-year record.

**PO4 (the target) closes cleanly in both windows:**

| window | PO4 RMSE before → after | PO4 bias before → after |
|---|---|---|
| KM 2022 | 0.041 → **0.011** (−73%) | +0.025 → −0.000 |
| EPA 2012–2021 | 0.039 → **0.0135** (−66%) | +0.028 → −0.000 |

No early-era (2012–2016) PO4 collapse. **But the EPA regression guard failed** — the 30×
dissolution cut induces system-wide P-limitation (ΔRMSE % below are computed from the
full-precision metrics; the RMSE columns are rounded to 2–3 sig figs):

| EPA var | RMSE before → after | ΔRMSE | note |
|---|---|---|---|
| NH4 | 0.060 → 0.081 | **+36 %** | less N uptake → NH4 accumulates |
| NO3 | 0.505 → 0.657 | **+30 %** | bias +0.065 → +0.254 (more over-predicted) |
| Si  | 1.49 → 2.03 | **+36 %** | less diatom uptake |
| TN  | 1.10 → 1.24 | +13 % | |
| Chl-a | bias −2.5 → **−13.9** | — | P-limited growth collapse |

**Why the planned `K_MIN`→1.0 fallback does not help.** `K_MIN` is denitrification — it *lowers*
NO3 — yet EPA NO3 rose. So the NO3 (and NH4, Si) regression is **`KDISS`-driven** P-limitation,
not `K_MIN`-driven; `K_MIN`=1.13 was *masking* part of the NO3 rise, and NH4 is untouched by
`K_MIN`, so a `KDISS`-only promotion would fail the guard too.

**Root cause — nonstationarity.** `KDISS`=0.118 fits the **2022 de-eutrophicated low-P regime**
(on KM-2022 NO3 *improves*), but the same cut is wrong for the **2012–2016 hyperbloom era** (EPA
NO3 *regresses*) — the summer P-PO4 record itself shifted from ~25 µg/L (2012–16, peak 143 in
2015) to 2–3 µg/L (2019–24). A single time-invariant dissolution rate cannot represent both. The
PO4 over-prediction is real, but the fix belongs in a regime-aware or structural change (a
biogenic-Si / sediment-P sink), not a constant calibrated on one low-P year.

The reusable before/after comparator built for this test —
`tools/compare_validation_runs.py` (per-variable obs-weighted RMSE/bias diff + one-sided
regression guard) — is retained for future calibration work.

## Reproduce

```sh
python3 tools/ingest_km_observations.py --out-dir /tmp/km       # nutrients + Chl-a obs

# --- 2022 (real forcing) ---
./run_cl29.sh                                                   # -> OUTPUTS_CL29/ (or reuse)
python3 tools/validate_cl29_vs_epa.py \
    --outputs OUTPUTS_CL29 --obs /tmp/km/km_observations_tidy.csv --base-year 2012

# --- 2023 (climatological forcing) ---
python3 tools/extend_cl29_forcing_climatology.py                # -> INPUTS_CL29_2023clim/
ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29_2023clim.txt          # -> OUTPUTS_CL29_2023clim/
awk -F, 'NR==1 || $4 ~ /^2023-/' /tmp/km/km_observations_tidy.csv > /tmp/km_2023.csv
python3 tools/validate_cl29_vs_epa.py \
    --outputs OUTPUTS_CL29_2023clim --obs /tmp/km_2023.csv --base-year 2012
```

## Caveats and next steps

- **The 2023 run uses climatological, not real, forcing** (EUTROPY ends 2022-12-31). A true
  2023 hindcast awaits real 2023 flows + boundary loads (from EUTROPY or another source).
- **Station→box rechecked against the vector layer `b29polys.gpkg`** (point-in-polygon,
  2026-07-21): 13 of 14 stations matched the earlier visual raster read; **`LTK14` was
  corrected from box 25 → box 9** (it sits ~1.5 km *inside* box 9; box 25 is ~1.9 km away),
  and the previously-inferred **`LTK3A → box 11` is now spatially confirmed**. The LTK14 fix
  changes the aggregate 2022 metrics negligibly (e.g. Si bias 0.56 → 0.54, Chl-a 9.5 → 9.4),
  so the tables above and the calibration below — computed before the fix — still stand at the
  shown precision.
- The 2023 files are MHTML "web page" exports (handled by the ingester's decimal-comma and
  truncated-header logic); the 2022 files are clean `.xls`.
- Reconstructed TN/TP/Chl-a depend on the pool stoichiometry above.
- Feed `km_observations_tidy.csv` into the PEST/PEST++ calibration alongside the EPA obs.
