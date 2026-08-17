# Attempting to calibrate CMEMS chlorophyll to the EPA in-situ record (Curonian Lagoon)

2026-08-10. **Outcome: not possible with the standard products — and this is a defensible,
literature-consistent negative result, not a processing failure at our end.** Scripts and figures:
`docs/report_scripts/` (`fetch_cmems_daily.py`, `fetch_olci_daily.py`, `cmems_calibrate.py`,
`figures/cmems_calibration_failure.png`).

## What the prior literature predicted

Vaičiūtė, Bresciani, Bučas et al. (2012) validated five MERIS processors against in-situ data in
exactly these optically complex (Case-2) Lithuanian waters. Their findings anticipate ours:

- "The standard algorithms trained on open ocean Case 1 waters **often fail due to the interference
  of CDOM** for the estimation of chl a concentration."
- "the strong **underestimation of chl a, particularly during cyanobacteria bloom**, could be due to
  uncorrected … remote-sensing reflectance (Rrs) at red/near-infrared wavelengths … the processor
  **is unable to capture the typical peak of Rrs around 700 nm**."
- "the C2R, Boreal and Eutrophic processors **strongly underestimated in situ chl a concentrations
  over the sampling locations with reduced Secchi depth**."
- Best performer was a *dedicated* Case-2 processor (FUB, R² = 0.69, MAE 7.8 mg m⁻³) — not a
  standard product.

The project context the user asked about: **EOMORES** (H2020, "Earth Observation-Based Services for
Monitoring and Reporting of Ecological Status"; Hommersom, Poser et al. 2017; Ligi, Randla et al.
2017) and **INFORM** — both developed *locally tuned* processors/services for optically complex
inland and coastal waters rather than relying on standard ocean-colour products. The lagoon
remote-sensing results used by Žilius et al. (2021) likewise came from locally calibrated
Sentinel-2/3 retrievals (reported there as R² = 0.91 for Sentinel-2 and 0.95 for Sentinel-3), not
from CMEMS.

## What we did

- **Products:** `cmems_obs-oc_bal_bgc-plankton_my_l3-multi-1km_P1D` (daily, 1 km, multi-sensor,
  1997-09 → 2021-12 — the longest available record) and, as the best-case test,
  `cmems_obs-oc_bal_bgc-plankton_my_l3-olci-300m_P1D-m` (daily, 300 m, OLCI, 2016 →).
- **In-situ:** the EPA chlorophyll record, 2,224 determinations from 1997 onward at 13 stations.
- **Match-up protocol** (after Vaičiūtė et al. 2012 / Bailey & Werdell): 3 × 3 pixel window centred
  on the station, discarded unless ≥ 5 of 9 pixels are valid, matched to same-day sampling; median
  of the valid window.
- **Model fitted:** a power law (linear in log–log), *not* a constant factor — because the
  literature predicts a concentration- and turbidity-dependent bias. Fitted on the earlier
  two-thirds of match-ups, tested on the most recent third.

## Results

**1 km multi-sensor, 118 match-ups (2002–2021), 7 stations.** The predicted
concentration-dependent bias is confirmed exactly:

| in-situ quartile (µg L⁻¹) | n | in-situ mean | CMEMS mean | ratio |
|---|---|---|---|---|
| 0.9 – 23.8 | 30 | 13.5 | 15.5 | **0.9×** |
| 23.8 – 35.6 | 29 | 30.0 | 18.0 | 1.7× |
| 35.6 – 57.8 | 30 | 46.1 | 16.2 | 2.9× |
| 57.8 – 138.3 | 29 | 76.0 | 19.6 | **3.9×** |

The satellite reports an almost constant ~16–20 µg L⁻¹ regardless of the true value: it is
unbiased at low concentrations and progressively blind as blooms develop.

Fitted calibration: log₁₀(in-situ) = 0.837 + 0.531 · log₁₀(CMEMS), i.e. in-situ ≈ 6.9 × CMEMS⁰·⁵³.
But its skill is inadequate, and the seasonal split shows why:

| subset | n | r (log) | R² | slope | MAPE |
|---|---|---|---|---|---|
| all match-ups | 118 | +0.30 | 0.09 | 0.48 | 94 % |
| station-month means | 116 | +0.30 | 0.09 | 0.47 | 95 % |
| **summer (Jun–Sep)** | 68 | **−0.06** | **0.00** | −0.08 | 65 % |
| non-summer | 50 | +0.56 | 0.32 | 0.94 | 96 % |
| 2014 onward | 68 | +0.28 | 0.08 | 0.42 | 96 % |
| pre-2014 | 50 | +0.27 | 0.07 | 0.50 | 92 % |

**In the bloom season the satellite carries no information about in-situ chlorophyll at all**
(r = −0.06). Outside it there is modest skill with a slope near 1. Monthly aggregation does not
help, and neither does restricting to the modern sensor era.

**300 m OLCI, 155 match-ups (2016–2021) — the best-case test — is worse, not better:**

| subset | n | r (log) | R² | in-situ/satellite |
|---|---|---|---|---|
| all | 155 | +0.05 | 0.00 | 6.5× |
| summer | 97 | +0.01 | 0.00 | 8.2× |
| non-summer | 58 | +0.15 | 0.02 | 4.4× |

## Conclusion and consequences

**The standard CMEMS chlorophyll products cannot be calibrated to the in-situ record for this
lagoon.** This is not a scaling problem that a regression can absorb: in the season of interest
there is no signal to scale. Any calibration fitted across all seasons would be driven by
non-summer skill and would fabricate summer values.

Two consequences follow:

1. **Do not use CMEMS chlorophyll — raw or calibrated — for bloom-season work in the Curonian
   Lagoon.** The viable route is the one the regional literature already took: locally
   re-processed Sentinel-2/3 with Case-2 algorithms (the EOMORES/INFORM line of work; Bresciani /
   Riddick retrievals as used by Žilius et al. 2021), or a red/NIR band-ratio index designed to
   capture the ~700 nm peak.
2. **⚠️ This qualifies our earlier spatial comparison.** The summer model-vs-satellite spatial
   agreement reported on 2026-08-09 (r = 0.84 across 21 lagoon boxes) was computed from summer
   climatological means — precisely the season in which the satellite has *no* point-scale skill.
   Spatial-mean aggregation over ~28 monthly composites may still recover a real pattern, but the
   agreement can no longer be presented as independent validation of the model's spatial
   structure: it may equally reflect two fields responding similarly to depth and optical
   gradients. **Do not claim satellite validation of the model's spatial pattern on this basis.**

## References (retrieved via scite; no editorial notices)

- Vaičiūtė, D., Bresciani, M., & Bučas, M. (2012). Validation of MERIS bio-optical products with
  in situ data in the turbid Lithuanian Baltic Sea coastal waters. *Journal of Applied Remote
  Sensing*, 6(1), 063568. https://doi.org/10.1117/1.JRS.6.063568
- Žilius, M., Vybernaite-Lubiene, I., Vaičiūtė, D., et al. (2021). Spatiotemporal patterns of N₂
  fixation in coastal waters derived from rate measurements and remote sensing. *Biogeosciences*,
  18(5), 1857–1871. https://doi.org/10.5194/bg-18-1857-2021
- Hommersom, A., Poser, K., et al. (2017). *Earth Observation-Based Services for Monitoring and
  Reporting of Ecological Status* (EOMORES). https://doi.org/10.5281/zenodo.1037083
- Ligi, M., Randla, M., et al. (2017). *Earth Observation-Based Services for Monitoring and
  Reporting of Ecological Status* (EOMORES). https://doi.org/10.5281/zenodo.1065691

⟨Not retrieved — cited above only as reported by Žilius et al. (2021); pull before citing directly:
Bresciani et al. (2014) and Riddick et al. (2019) on the locally calibrated lagoon retrievals, and
the INFORM project deliverables.⟩
