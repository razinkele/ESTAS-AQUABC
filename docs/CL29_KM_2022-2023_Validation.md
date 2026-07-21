# CL29 validation against Kuršių marios (KM) 2022–2023 monitoring

This extends the [EPA-archive validation](CL29_EPA_Calibration_Summary.md) (which ran to
2021) into the most recent years, using the **Kuršių marios (KM, Curonian Lagoon)**
monitoring newly ingested from the AAA *2014-2023_BJ duomenys extrahuoti* extract.

## Data and box alignment

| | |
|---|---|
| Source | `curonian/DATA/2014-2023_BJ duomenys extrahuoti` — KM hydrochemistry (`Hidrocheminiai tyrimai`) + KM chlorophyll-a (`Biologiniai tyrimai`) |
| Ingester | `tools/ingest_km_observations.py` (→ `km_observations_tidy.csv` + `KM_<station>_box<box>.dates`) |
| Station→box | the EPA `LTK→box` map (`tools/epa_station_to_box.csv`; northings verified to match), plus the new `LTK3A→11` (inferred, pending TIFF confirmation) |
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

## Demonstration calibration (`pestpp-ies`)

A first pass at calibrating the five `pest/` parameters (PR #54) against the 2022 obs, run
with `pestpp-ies` as a **reduced** ensemble — **12 realizations × 1 iteration**, parallel over
12 cores (~49 min) — to exercise the harness end-to-end. This is a demonstration, **not a
converged posterior**; the committed `pest/cl29.pst` (50 reals × 3 iterations) on dedicated
hardware would refine it.

Objective function Φ across the ensemble:

| iteration | mean Φ | std | min | max |
|---|---:|---:|---:|---:|
| 0 (initial ensemble) | 4434 | 1142 | 2935 | 6806 |
| 1 (after the iES update) | **1711** | 333 | **1382** | 2405 |

Mean Φ fell **61 %** in one iteration and the ensemble spread tightened 3.4×. The parameter
ensemble means moved to correct the documented biases:

| parameter | initial | calibrated | direction & effect |
|---|---:|---:|---|
| `K_MIN_DOC_NO3N_20` (denitrification) | 1.79 | 0.41 | ↓ less N removal → raises NO3 (was under-predicted) |
| `KDISS_DET_PART_ORG_P_20` (POP dissolution) | 3.95 | 0.33 | ↓ less P release → lowers PO4 (over-predicted) |
| `KHS_DSi_DIA` (diatom Si half-sat) | 0.018 | 0.0069 | ↓ easier Si uptake → lowers Si (over-predicted) |
| `KG_DIA_OPT_TEMP` (diatom growth) | 3.63 | 4.79 | ↑ more uptake |
| `KD_DIA_20` (diatom mortality) | 0.19 | 0.068 | ↓ |

Every shift moves in the direction that corrects a documented bias — a reassuring sign that
the parameters and observations are wired correctly. The calibrated values are a
demonstration, not a recommended parameter set; run the full ensemble to trust them.

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
- **LTK3A → box 11** confirmed against `29boxesNew_modified.tif`: LTK3/LTK3B sit on the box-11
  outline and LTK3A is a boundary point at box 11's south-east edge (just north of box 20). Its
  "Klaipėdos sąsiauris" (strait) water-body class matches box 11, not box 20's northern-lagoon
  class, so box 11 is the water-body-consistent assignment.
- The 2023 files are MHTML "web page" exports (handled by the ingester's decimal-comma and
  truncated-header logic); the 2022 files are clean `.xls`.
- Reconstructed TN/TP/Chl-a depend on the pool stoichiometry above.
- Feed `km_observations_tidy.csv` into the PEST/PEST++ calibration alongside the EPA obs.
