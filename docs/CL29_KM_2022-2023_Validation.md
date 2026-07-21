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

- CL29 default configuration built with the **v0.5.2** release binary, `ESTAS_HOLD_VOLUME=1`
  (`run_cl29.sh`), 240 steps/day.
- Model calendar: **2012-01-01 → 2022-12-30** (`SIMULATION_END = 4016` days).
- **The model window overlaps only the 2022 observations.** The results below are therefore
  the 2022 subset (scored `n ≈ 161` per nutrient); the **2023 observations are ingested and
  ready** and will validate once the run is extended past day 4016.

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

## Reproduce

```sh
./run_cl29.sh                                   # produces OUTPUTS_CL29/ (or reuse an existing run)
python3 tools/ingest_km_observations.py --out-dir /tmp/km
python3 tools/validate_cl29_vs_epa.py \
    --outputs OUTPUTS_CL29 --obs /tmp/km/km_observations_tidy.csv --base-year 2012
```

## Caveats and next steps

- **Model window** ends 2022-12-30, so only 2022 is scored; extend `SIMULATION_END` past 2023
  to validate the ingested 2023 observations.
- **LTK3A → box 11** is inferred from its "Klaipėdos sąsiauris" water-body and proximity to
  LTK3/LTK3B; confirm against `29boxesNew_modified.tif`.
- The 2023 files are MHTML "web page" exports (handled by the ingester's decimal-comma and
  truncated-header logic); the 2022 files are clean `.xls`.
- Reconstructed TN/TP/Chl-a depend on the pool stoichiometry above.
- Feed `km_observations_tidy.csv` into the PEST/PEST++ calibration alongside the EPA obs.
