# A working satellite chlorophyll calibration for the Curonian Lagoon (red/NIR index route)

2026-08-10. Follows the failure of the standard products
(`docs/CMEMS_chlorophyll_calibration_attempt.md`) and the algorithm survey
(`docs/EO_chlorophyll_public_algorithms.md`). **Outcome: the published red/NIR index approach
works — a Maximum-Peak-Height index computed from CMEMS OLCI reflectance and calibrated to the EPA
record recovers a real bloom-season signal (summer R² = 0.41, hold-out R² = 0.36) where the standard
chlorophyll product has none (R² = 0.00).** Scripts, coefficients and figure in
`docs/report_scripts/` (`fetch_rrs_stations.py`, `rrs_indices.py`, `mph_calibration.json`,
`figures/mph_calibration.png`).

## Why this works when the chlorophyll product does not

The regional literature diagnoses the failure precisely: as biomass rises, the chlorophyll
reflectance peak shifts into the red/NIR, and standard processors do not capture it. Soomets et al.
(2020) on Baltic lakes: C2RCC reflectance "often **missed the Chl-a absorption peak on or around
665 nm; the Chl-a peak in reflectance spectra of eutrophic waters can shift towards longer
wavelengths with increasing biomass**"; Vaičiūtė et al. (2012) for these exact waters: the
underestimation is worst "particularly during cyanobacteria bloom" because the processors are
"unable to capture the typical peak of Rrs around 700 nm".

The remedy is to stop using the chlorophyll product and build an index on the reflectance bands
themselves. Critically, **the CMEMS OLCI 300 m *reflectance* product does carry the required band**
(`RRS708_75`), even though its chlorophyll product ignores what that band is telling it. (The 1 km
multi-sensor reflectance product stops at 665 nm — which is exactly why the long multi-sensor record
is blind to blooms and cannot be rescued.)

## Method

- **Data:** `cmems_obs-oc_bal_bgc-reflectance_my_l3-olci-300m_P1D` (300 m, daily, 2016-04 →),
  bands RRS560/620/665/673.75/681.25/708.75/778.75, fetched in small windows around each EPA station.
- **In-situ:** the EPA chlorophyll record; **161 valid match-ups**, 11 stations, 2016-05 → 2021-11
  (77 in summer). Same-day, station window, ≥4 valid pixels, median.
- **Indices tested (all published families):** two-band 709/665 and 709/681 ratios; the three-band
  Gitelson/Dall'Olmo form; **MCI** (709 peak height over a 665→779 baseline); **MPH** (largest of the
  681 and 709 peak heights over that baseline); **FLH** (681 over a 665→709 baseline); plus green/red
  ratios for reference.
- **Calibration:** log₁₀(chlorophyll) regressed on the index, with a temporal hold-out (fit on the
  earlier two-thirds, test on the most recent third) and a leave-one-station-out test.

## Results

Summer (Jun–Sep) skill, the acceptance test the standard product failed:

| index | n | Pearson r | R² | Spearman | MAPE |
|---|---|---|---|---|---|
| **MPH, log form** | 77 | **+0.64** | **0.41** | +0.67 | **49 %** |
| MCI | 77 | +0.52 | 0.27 | +0.69 | 57 % |
| FLH | 77 | −0.41 | 0.17 | −0.65 | 63 % |
| 709/681 ratio | 77 | +0.41 | 0.17 | +0.61 | 64 % |
| 709/665 ratio | 77 | +0.30 | 0.09 | +0.58 | 68 % |
| three-band | 77 | +0.08 | 0.01 | +0.59 | 73 % |
| *standard CMEMS chlorophyll* | 97 | *+0.01* | *0.00* | — | *84 %* |

Note the ratio indices have strong **rank** correlation (ρ ≈ 0.6) but weak linear fit — they track
chlorophyll monotonically but non-linearly; MPH in log form is both monotonic and well-behaved.

**Adopted calibration** (all-season fit; MPH shift constant 2.708 × 10⁻⁴):

```
log10(chl-a [µg/L]) = 2.668 + 0.452 · log10(MPH + 0.00027079)
MPH = max( R681.25 , R708.75 ) − baseline(R665 → R778.75) evaluated at that band
```

| fit | subset | n | r | R² | RMSE(log₁₀) | MAPE |
|---|---|---|---|---|---|---|
| all-season | train | 107 | +0.50 | 0.25 | 0.344 | 107 % |
| all-season | **hold-out** | 54 | **+0.63** | **0.40** | 0.282 | 48 % |
| summer-only | train | 51 | +0.67 | 0.46 | 0.210 | 43 % |
| summer-only | **hold-out** | 26 | **+0.60** | **0.36** | 0.284 | 58 % |

**Leave-one-station-out (summer):** mean r = +0.41, mean MAPE 53 % across 7 stations, ranging from
r = +0.15 (LTK6) to +0.73 (LTK5) — the relationship transfers spatially but unevenly.

## Honest assessment

**What this buys:** a satellite chlorophyll estimate for the lagoon that carries real information in
the bloom season, from free data and published algorithms, validated out-of-sample. That is a
qualitative change from the standard product, which carries none.

**What it does not buy:** accuracy comparable to the published best-in-class. Soomets et al. reach
R² = 0.84–0.97 — but they use **top-of-atmosphere** reflectance with optical-water-type guidance,
having found the atmospheric correction to be the limiting step in exactly these waters. We used
the atmospherically-corrected Rrs because it is what CMEMS distributes. So this result is best read
as: *holding the atmospheric correction fixed at the standard chain, switching to the right
algorithm recovers most of what is recoverable* — and the residual gap (R² 0.41 vs 0.84+) is the
measure of what the atmospheric correction is still costing. That is a quantitative argument for the
L1b/TOA route (SNAP + FUB-CSIRO, or TOA band ratios), not against it.

**Other limits.** MAPE ≈ 50 % means individual retrievals are indicative, not precise; the index is
calibrated on 2016–2021 and 11 stations; MPH saturates at very high biomass and cannot see surface
scum; and there is no coverage before 2016 by this route (the pre-2016 record needs MERIS, which
also carries the 709 band and could extend this method back to 2002 — a natural next step).

**Recommended use:** spatial and seasonal *patterns* and relative bloom intensity — for which it is
now defensible — rather than absolute concentrations. It is suitable for comparing the model's
bloom timing and spatial structure, which was the original motivation.

## References (retrieved via scite; no editorial notices)

- Soomets, T., Uudeberg, K., Jakovels, D., et al. (2020). Validation and comparison of water quality
  products in Baltic lakes using Sentinel-2 MSI and Sentinel-3 OLCI data. *Sensors*, 20(3), 742.
  https://doi.org/10.3390/s20030742
- Vaičiūtė, D., Bresciani, M., & Bučas, M. (2012). Validation of MERIS bio-optical products with in
  situ data in the turbid Lithuanian Baltic Sea coastal waters. *Journal of Applied Remote Sensing*,
  6(1), 063568. https://doi.org/10.1117/1.JRS.6.063568
- Vaičiūtė, D., Sokolov, Y., Bučas, M., et al. (2024). Earth observation-based cyanobacterial bloom
  index testing … Baltic and Black Seas. *Remote Sensing*, 16(4), 696.
  https://doi.org/10.3390/rs16040696

⟨The MCI and MPH index definitions used here follow the standard published forms; pull the original
Gower (MCI) and Matthews (MPH) references before citing the algorithms by name in the manuscript.⟩
