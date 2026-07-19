# CL29 Calibration & Validation vs. EPA and Experimental Data

*Summary of the July 2026 calibration/validation arc for the 29-box EUTROPY-derived
Curonian Lagoon configuration (CL29).*

## Data and tooling

| artifact | purpose |
|---|---|
| `tools/ingest_epa_observations.py` (+ `epa_station_to_box.csv`) | Turns the Lithuanian EPA monitoring archive (~70k samples, 1984–2021) into tidy observations + per-station `.dates`. Resolves per-era units/speciation (µg vs mg; dissolved-N ion-basis pre-2008 → nitrogen) from cross-era value continuity. |
| `tools/validate_cl29_vs_epa.py` | Scores a CL29 run against the EPA observations per (box, variable): count, obs/model means, bias, RMSE, correlation, plus time-series plots. Compares 8 variables — 5 direct state variables (NH4, NO3, PO4, DISS_OXYGEN, DISS_Si) and 3 reconstructed from the pools (Tot_N, Tot_P, Chl-a). |
| CL29 run extent | Extended from 2012–2016 (1826 d) to the full EUTROPY forcing record **2012–2022** (`eutropy_to_aquabc_network.py` `MAX_DAY`), ~6×-ing the EPA overlap (n ≈ 2,800–2,960 per variable). |
| `~/curonian/DATA/Experimental/` | Process-rate + carbon-chemistry data (2015): N₂-fixation rates, seasonal remineralization / production / respiration, water-column DIC/alkalinity/DOC/POC, light/mixing, and a complete initial-condition set. |

## Calibrations

| change | effect | status |
|---|---|---|
| **Denitrification** `K_MIN_DOC_NO3N_20` 0.025 → 1.0 | NO3 bias +0.31 → +0.06 (RMSE −31%), TN +0.80 → +0.39; DO improved | merged (PR #42) |
| **Summer-P boost** `CL29_BOUNDARY_PO4_SUMMER_PEAK` 3.0 → 2.0 | TP +0.019 → +0.013; de-eutrophication-aware for the fuller record | open (PR #43) |

The denitrification calibration is mechanistically grounded: the Curonian Lagoon is a
documented strong-denitrification N sink (Bartoli et al. 2021 measured spring
denitrification removing up to ~35% of the Nemunas nitrogen load), and the template
default (0.025) badly under-represented it. Boundary concentrations were verified
against independent Nemunas/Matrosovka river water quality in `~/curonian/DATA` and
found realistic — so the pre-calibration ~2× high in-lagoon N was a weak-removal
problem, not over-loading.

## Residuals — characterized, not "fixed"

Every remaining misfit traces to **one root: the model's diatoms do not concentrate
their growing-season nutrient uptake enough** (near-aseasonal metabolism), which is
**entangled with the box-19/cyanobacteria succession** the model was deliberately tuned
for and therefore cannot be pushed without weakening the defining summer cyano bloom.

- **PO4** — mild residual over-supply + weak removal; no clean lever (a detrital-P
  burial loss shaves only ~0.008 and costs chlorophyll).
- **Si** — *not* broadly over-predicted. The model's annual-mean Si (~1.67) matches the
  careful 2015 experimental in-lagoon value (1.62); the apparent EPA "gap" is a
  seasonal-sampling artifact (EPA obs are summer-heavy, and Si is strongly drawn down in
  summer: EPA winter 2.40 → summer 0.55). The genuine residual is a missing *seasonal
  drawdown*, i.e. the same diatom-uptake issue.
- **DO** — seasonally realistic (winter-high/summer-low), with a modest winter
  over-estimate (model 15.3 vs obs 12.1) from excess winter production.

Process-rate comparison against the 2015 measurements confirms the mechanism: model
winter GPP is ~9× the measured, and summer remineralization/N-uptake are ~5–8× too low —
the seasonal amplitude of metabolism is collapsed relative to reality.

## Approaches ruled out (with evidence)

- **Sediment / burial P-sink.** Prototyped both a full `MODEL_SEDIMENTS=2` run and a
  first-order detrital/particulate burial-loss term. The full sediment model is a net
  P/Si *source* (Si rises), confirming prior work; the burial-loss term helps PO4
  marginally but *backfires on Si* (faster particulate-Si removal starves diatom Si
  recycling). Not viable.
- **Lowering boundary Si.** The EUTROPY boundary file has no Si column, so the model's
  boundary Si (1.5) is an AQUABC default; observed winter Si (2.40) implies the real
  river Si is if anything *higher*. Refuted.
- **Diatom thermal-niche shift** (to suppress winter over-production / winter DO).
  Directionally works but degrades the summer cyano bloom ~40–58% — the succession
  tension again. Not adopted.

## Key references

- Bartoli, M., Nizzoli, D., Zilius, M., et al. (2021). Denitrification, nitrogen uptake,
  and organic matter quality … sandy and muddy sediments of a turbid estuary.
  *Frontiers in Microbiology, 11*, 612700. https://doi.org/10.3389/fmicb.2020.612700
- Vybernaitė-Lubienė, I., Zilius, M., Bartoli, M., et al. (2022). Biogeochemical budgets
  of nutrients and metabolism in the Curonian Lagoon. *Water, 14*(2), 164.
  https://doi.org/10.3390/w14020164
- Stakėnienė, R., Jokšas, K., et al. (2023). Nutrient loadings and exchange between the
  Curonian Lagoon and the Baltic Sea (2001–2020). *Water, 15*(23), 4096.
  https://doi.org/10.3390/w15234096
- Mėžinė, J., Ferrarin, C., et al. (2019). Sediment transport mechanisms in a lagoon with
  high river discharge and sediment loading. *Water, 11*(10), 1970.
  https://doi.org/10.3390/w11101970

## Open threads

- **FIX_CYN calibration** against the measured 2015 N₂-fixation rates (0.18–0.27, up to
  1.2 mg N m⁻³ h⁻¹) — closes the nitrogen budget alongside the denitrification sink.
- **pH validation** using the 2015 DIC/alkalinity chemistry (also reconciles the
  `INORG_C` CO₂SYS units convention).
- **Sediment facies map** — a real Curonian sandy/muddy map would enable spatially
  variable P/Si burial; currently data-blocked.
