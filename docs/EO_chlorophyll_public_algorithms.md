# Publicly available EO chlorophyll algorithms for the Curonian Lagoon

2026-08-10. Investigation prompted by the failure to calibrate standard CMEMS products
(`docs/CMEMS_chlorophyll_calibration_attempt.md`). **Conclusion: yes — the algorithms, the software
and the input imagery are all public and free, and the best-performing chain for exactly these
waters has been published by Klaipėda University's own group.** All references retrieved via scite;
no editorial notices on any.

## 1. The short answer

| Layer | What to use | Availability |
|---|---|---|
| Imagery | Sentinel-3 OLCI **Level-1b** (and Sentinel-2 MSI L1C) | Free, Copernicus Data Space Ecosystem |
| Software | **ESA SNAP** + Sentinel-3 Toolbox (S3TBX) — carries FUB-CSIRO, C2RCC, C2X, IdePix, MPH, MCI | Free, ESA |
| Algorithm | **FUB-CSIRO** on OLCI L1b — validated best for Lithuanian coastal/transitional water | Published, in S3TBX |
| Alternative | OWT-guided red/NIR **band ratios** on TOA reflectance | Coefficients published in open-access papers |
| Calibration data | our EPA in-situ record (2,224 chlorophyll determinations, 1997–2021) | already in hand |

The one thing that is *not* usable is what we tried: standard, globally-tuned ocean-colour products
(CMEMS/Level-2), which fail in this optically complex water — quantified in the companion document.

## 2. The decisive reference — and it is in-house

**Vaičiūtė, D., Sokolov, Y., Bučas, M., et al. (2024).** *Earth Observation-Based Cyanobacterial
Bloom Index Testing for Ecological Status Assessment in the Open, Coastal and Transitional Waters
of the Baltic and Black Seas.* Remote Sensing, 16(4), 696. https://doi.org/10.3390/rs16040696
— **gold open access**, and first-authored from the Marine Research Institute, Klaipėda University.

Directly relevant findings:

> "Among three tested neural network-based processors (FUB-CSIRO, C2RCC, standard Level-2 data),
> **the FUB-CSIRO applied to Sentinel-3 OLCI images was the most appropriate for the retrieval of
> chlorophyll-a in both seas (R² = 0.81)**."

> "Sentinel-3 Toolbox Kit Module (**S3TBX; version 6.0.4**) in Sentinel Application Platform
> (**SNAP; version 9.0**) was used to process the Level-1b OLCI images. Originally, the FUB
> processor was designed for European coastal waters and uses **Level-1b top-of-atmosphere
> radiances** to retrieve Rrs and the concentration of the optical water constituents from the
> MERIS images, and lately, it was **adapted for OLCI data and named FUB-CSIRO**."

They processed 147 combined MERIS + OLCI scenes for the Baltic covering **2006–2019**, and report
that Baltic cyanobacteria blooms "mostly originated from the central part and **the outflow of the
Curonian Lagoon**."

**Practical consequence: the processing chain, the expertise and quite possibly an already-processed
archive exist inside your own institute.** Before rebuilding anything, ask Diana Vaičiūtė's group
for (a) the FUB-CSIRO-processed OLCI/MERIS chlorophyll for the lagoon, and (b) their match-up
database. That is very likely faster than re-deriving it, and it makes the satellite comparison a
collaboration rather than a side-project.

The same group's earlier validation for these waters — Vaičiūtė, Bresciani & Bučas (2012),
https://doi.org/10.1117/1.JRS.6.063568 — also found **FUB** the best of five MERIS processors
(R² = 0.69, MAE 7.8 mg m⁻³), i.e. the recommendation is consistent across a decade.

## 3. The Estonian/EOMORES line: optical-water-type-guided band ratios

**Soomets, T., Uudeberg, K., Jakovels, D., et al. (2020).** *Validation and Comparison of Water
Quality Products in Baltic Lakes Using Sentinel-2 MSI and Sentinel-3 OLCI Data.* Sensors, 20(3),
742. https://doi.org/10.3390/s20030742 — gold open access, Tartu Observatory (an EOMORES partner).

Why it matters for us:

- They tested **21 chlorophyll options** per sensor — C2RCC/C2X `conc_chl`, MCI, MPH/CHL, Level-2
  neural net, and band ratios from TOA/C2RCC/C2X reflectance — all in **SNAP** ("scientific image
  processing toolbox called the Sentinel Application Platform (SNAP v 6.0)").
- **The winning approach is optical-water-type (OWT) guidance plus red/NIR band ratios on TOA
  reflectance**, reaching R² = 0.84–0.97 for MSI: "the red and near-infrared (NIR) band ratios from
  TOA reflectance spectra were the most successful."
- Their diagnosis of the atmospheric-correction products is **exactly the failure mode we measured
  in CMEMS**: "The OLCI and MSI C2RCC processed reflectance spectra showed **large underestimation**
  of the reflectance spectra. This also often **missed the Chl-a absorption peak on or around
  665 nm; the Chl-a peak in reflectance spectra of eutrophic waters can shift towards longer
  wavelengths with increasing biomass**."
- The OWT scheme (Uudeberg et al.) explicitly includes a class for our situation: "**Very Turbid
  OWT is water where the Chl-a dominates and this type is associated with blooms**."

So the second viable route needs no neural-net processor at all: classify OWT, then apply the
published band ratio for that type — coefficients are in the paper's Tables 3–6 (CC-BY).

## 4. Recommended plan

1. **Ask in-house first** (Vaičiūtė group, MRI KU): FUB-CSIRO OLCI/MERIS chlorophyll for the lagoon,
   2006–2019+, and their in-situ match-up set. Highest value per unit effort, and a natural
   co-authorship.
2. **If self-processing:** Sentinel-3 OLCI L1b from the Copernicus Data Space → SNAP/S3TBX → IdePix
   masking → FUB-CSIRO (primary) with C2RCC as comparison → optional MPH/MCI for bloom peaks.
3. **Calibrate and validate against our EPA record** using the match-up machinery already built
   (`docs/report_scripts/cmems_calibrate.py`: 3×3 window, ≥5/9 valid pixels, same-day, log–log fit
   with a temporal hold-out). It is product-agnostic — only the input NetCDF changes.
4. **Judge it by the summer test that killed CMEMS**: a usable product must show real skill in
   Jun–Sep, not merely in the annual pool. Our CMEMS summer scores (r = −0.06 at 1 km, +0.01 at
   300 m) are the benchmark to beat.

## 5. Caveats

- The 2024 validation (R² = 0.81) is for **coastal** Lithuanian water with in-situ chlorophyll
  1.4–22.5 mg m⁻³. The lagoon interior reaches ~140 mg m⁻³ in our EPA record, well outside that
  calibration range — so FUB-CSIRO must be re-validated on lagoon match-ups before use, not assumed.
- OLCI is 300 m and MERIS was 300 m: adequate for the lagoon, marginal near its narrow northern
  channel. Sentinel-2 MSI (10–60 m) is better spatially but has fewer bands for water and a shorter
  record.
- Anything derived from Sentinel starts in **2016** (OLCI) or **2002–2012** (MERIS): there is a
  **2012–2016 gap** in high-quality EO coverage that no processing can fill.

## References (retrieved via scite; no editorial notices)

- Soomets, T., Uudeberg, K., Jakovels, D., et al. (2020). Validation and comparison of water quality
  products in Baltic lakes using Sentinel-2 MSI and Sentinel-3 OLCI data. *Sensors*, 20(3), 742.
  https://doi.org/10.3390/s20030742
- Vaičiūtė, D., Bresciani, M., & Bučas, M. (2012). Validation of MERIS bio-optical products with in
  situ data in the turbid Lithuanian Baltic Sea coastal waters. *Journal of Applied Remote Sensing*,
  6(1), 063568. https://doi.org/10.1117/1.JRS.6.063568
- Vaičiūtė, D., Sokolov, Y., Bučas, M., et al. (2024). Earth observation-based cyanobacterial bloom
  index testing for ecological status assessment in the open, coastal and transitional waters of the
  Baltic and Black Seas. *Remote Sensing*, 16(4), 696. https://doi.org/10.3390/rs16040696
- Ansper, A., & Alikas, K. (2018). Retrieval of chlorophyll a from Sentinel-2 MSI data for the
  European Union Water Framework Directive reporting purposes. *Remote Sensing*, 11(1), 64.
  https://doi.org/10.3390/rs11010064
- Hommersom, A., Poser, K., et al. (2017). *Earth Observation-Based Services for Monitoring and
  Reporting of Ecological Status* (EOMORES). https://doi.org/10.5281/zenodo.1037083

⟨Not retrieved — verify before citing directly: Uudeberg et al. (2019) OWT classification
(Remote Sensing 11:2297, referenced throughout Soomets 2020); Matthews et al. on MPH; Bresciani
et al. (2014) and Riddick et al. (2019) lagoon retrievals; the FUB-CSIRO adaptation paper.⟩
