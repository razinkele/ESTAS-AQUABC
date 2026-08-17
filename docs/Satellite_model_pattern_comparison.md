# Model vs satellite: seasonal, spatial and interannual chlorophyll patterns

2026-08-10. Uses the MPH index calibrated to the EPA record
(`docs/OLCI_MPH_chlorophyll_calibration.md`) — the only satellite product tested here with real
bloom-season skill — applied lagoon-wide, 2016–2021, aggregated to CL29 boxes and months. Model =
shipped-defaults full-record run. Scripts/figure: `docs/report_scripts/`
(`fetch_mph_lagoon.py`, `mph_vs_model.py`, `figures/seasonal_inversion.png`).

**Headline: the model's chlorophyll seasonality is inverted, and this is invisible in the
annual mean.** Two independent observing systems agree with each other and disagree with the model.

## 1. The calibration validates itself

Before judging the model, note that the calibrated satellite product reproduces the *observed*
seasonal cycle almost exactly — **satellite vs in-situ r = +0.90 (p < 0.001), Spearman +0.87** — even
though the calibration was fitted on individual same-day match-ups, never on the climatology. Both
put the minimum in February and the maximum in August–September. This is independent evidence that
the MPH route recovers real seasonal signal.

## 2. Seasonal cycle — the model is anti-correlated with reality

Monthly climatology, lagoon boxes, 2016–2021 (µg L⁻¹):

| month | in-situ | satellite | model |
|---|---|---|---|
| Feb | 9.6 | 27.5 | **47.3** |
| Mar | 27.8 | 31.1 | 42.0 |
| Apr | 36.5 | 32.3 | 40.0 |
| May | 30.1 | 28.3 | 42.4 |
| Jun | 22.1 | 28.3 | 29.1 |
| Jul | 26.3 | 30.0 | 27.4 |
| **Aug** | **52.4** | 36.3 | 29.7 |
| **Sep** | 47.9 | **40.1** | **21.6** |
| **Oct** | 48.3 | 38.7 | 23.6 |
| Nov | 23.4 | 30.7 | 28.5 |

| pair | Pearson | p | Spearman |
|---|---|---|---|
| satellite vs in-situ | **+0.90** | 0.000 | +0.87 |
| model vs in-situ | −0.58 | 0.079 | −0.36 |
| model vs satellite | **−0.68** | 0.032 | −0.61 |

- **Peak month:** in-situ **August**, satellite **September**, model **February**.
- **Minimum month:** in-situ February, satellite February, model **September**.
- **Autumn/spring ratio (Aug–Oct ÷ Feb–May):** in-situ **1.90**, satellite **1.29**, model **0.58**.

The model produces its largest phytoplankton biomass in winter and its smallest during the observed
bloom. Because the two errors offset, the annual-mean chlorophyll bias is only about −2 µg L⁻¹ —
a metric that would pass a conventional evaluation while concealing a complete phase error.

This corroborates, with continuous coverage, what the sparse in-situ seasonal breakdown already
indicated (winter bias +20 µg L⁻¹, summer −12 µg L⁻¹) and connects directly to the composition
result: the observed **August–October** maximum is the heterocystous-cyanobacteria bloom that the
model drives to extinction (Žilius et al. 2021 report diazotrophs dominating the community in
"summer and fall"). The missing autumn peak and the missing fixers are the same failure.

## 3. Spatial pattern — no within-lagoon skill

Summer (Jun–Sep) climatology per box:

- **Lagoon boxes only (n = 22): Pearson r = −0.20 (p = 0.37), Spearman −0.29 (p = 0.20)** — no skill.
- Including the marine/coastal boxes (n = 27): r = +0.50 (p = 0.008) — i.e. the model does capture
  the lagoon-versus-open-sea contrast, but not structure *within* the lagoon.

The satellite places the strongest summer blooms in the **southern boxes** (5, 28, 2, 3, 29, 6 at
41–43 µg L⁻¹) — the Russian part of the lagoon, where there are no in-situ data at all and where the
longest residence times are. The model places its maxima elsewhere and gives those southern boxes
some of its *lowest* values (20–25 µg L⁻¹).

**This supersedes the provisional result of 2026-08-09** (r = 0.84 model-vs-satellite), which was
computed with the *uncalibrated* standard product and was inflated by including marine boxes. With
a product that has demonstrated bloom-season skill, the honest conclusion is the opposite: the
model does not reproduce the within-lagoon spatial pattern.

## 4. Relative bloom intensity — no interannual skill detected

Summer means by year (lagoon):

| year | satellite | model | satellite (relative) | model (relative) |
|---|---|---|---|---|
| 2016 | 38.0 | 26.1 | 1.13 | 0.97 |
| 2017 | 37.6 | 27.7 | 1.12 | 1.03 |
| 2018 | 35.2 | 29.2 | 1.05 | 1.08 |
| 2019 | 32.8 | 24.1 | 0.98 | 0.89 |
| 2020 | 29.0 | 25.5 | 0.86 | 0.95 |
| 2021 | 29.3 | 29.2 | 0.87 | 1.08 |

Pearson r = +0.04 (p = 0.94), Spearman = +0.20 (p = 0.70), n = 6. Bloom-year ranking: satellite
2016 > 2017 > 2018 > 2019 > 2021 > 2020; model 2018 > 2021 > 2017 > 2016 > 2020 > 2019. **No
evidence of skill**, though with six years the statistical power is low — this is "not
demonstrated", not "demonstrably absent". The satellite does show a coherent decline across the
period (1.13 → 0.87) that the model does not follow.

## 5. What this means

The model reproduces the *magnitude* of lagoon chlorophyll but not its *organisation* in time or
space. Ranked by severity:

1. **Phase error in the seasonal cycle** (anti-correlated) — the most serious, and mechanistically
   tied to the absent diazotroph bloom.
2. **No within-lagoon spatial skill** — the north–south bloom gradient the satellite resolves,
   including in the unsampled south, is absent from the model.
3. **No demonstrated interannual skill** over six years.

All three are hidden by annual-mean, lagoon-mean metrics — which is the methodological point the
paper already makes about compensating errors, now extended from the light climate to phenology.

## References

- Žilius, M., Vybernaite-Lubiene, I., Vaičiūtė, D., et al. (2021). Spatiotemporal patterns of N₂
  fixation in coastal waters derived from rate measurements and remote sensing. *Biogeosciences*,
  18(5), 1857–1871. https://doi.org/10.5194/bg-18-1857-2021
