# AQUABC Model Documentation

<div class="help-nav" style="background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 8px; padding: 1rem; margin-bottom: 1.5rem; border: 1px solid #dee2e6;">
<h4 style="margin: 0 0 0.75rem 0; color: #495057;">📚 Quick Navigation</h4>
<div style="display: flex; flex-wrap: wrap; gap: 0.5rem;">
<a href="#part1" style="background: #0d6efd; color: white; padding: 0.4rem 0.8rem; border-radius: 5px; text-decoration: none; font-weight: 500;">📖 Part 1: Model Overview</a>
<a href="#part2" style="background: #198754; color: white; padding: 0.4rem 0.8rem; border-radius: 5px; text-decoration: none; font-weight: 500;">📊 Part 2: State Variables</a>
<a href="#part3" style="background: #6f42c1; color: white; padding: 0.4rem 0.8rem; border-radius: 5px; text-decoration: none; font-weight: 500;">🧪 Part 3: Allelopathy</a>
<a href="#part4" style="background: #dc3545; color: white; padding: 0.4rem 0.8rem; border-radius: 5px; text-decoration: none; font-weight: 500;">🔬 Part 4: Scientific Review</a>
</div>
</div>

---

<div id="part1"></div>

# Part 1: Model Overview

<details open>
<summary style="font-size: 1.1rem; font-weight: bold; cursor: pointer; color: #0d6efd; padding: 0.5rem 0;">📋 Table of Contents</summary>
<div style="padding-left: 1rem; margin-bottom: 1rem;">
<a href="#intro">Introduction</a> · 
<a href="#structure">Model Structure</a> · 
<a href="#phyto-groups">Phytoplankton Groups</a> · 
<a href="#options">Model Options</a> · 
<a href="#color-legend">Color Legend</a> · 
<a href="#input-files">Input Files</a> · 
<a href="#output-files">Output Files</a>
</div>
</details>

<div id="intro"></div>

## Introduction

AQUABC (Aquatic Biogeochemical Model) is a comprehensive water quality simulation model designed for lakes, reservoirs, and coastal waters. The model simulates the coupled cycles of carbon, nitrogen, phosphorus, silica, oxygen, and other elements in aquatic ecosystems.

<div id="structure"></div>

## Model Structure

AQUABC integrates multiple biogeochemical processes:

- **Primary Production**: Growth of multiple phytoplankton functional groups
- **Secondary Production**: Zooplankton grazing and metabolism
- **Nutrient Cycling**: Mineralization, nitrification, denitrification
- **Organic Matter Dynamics**: Production, transformation, and decomposition
- **Carbonate Chemistry**: pH, alkalinity, and CO₂ equilibria
- **Redox Chemistry**: Iron, manganese, and sulphur cycling (optional)
- **Allelopathy**: Competitive interactions via secondary metabolites (optional)

<div id="phyto-groups"></div>

## Phytoplankton Functional Groups

The model distinguishes **five phytoplankton functional groups**:

| Group | Abbreviation | Key Characteristics |
|:------|:-------------|:--------------------|
| Diatoms | DIA | Silica requirement, spring blooms, high sinking rate |
| Non-fixing Cyanobacteria | CYN / NOFIX_CYN | No N₂ fixation, includes *Microcystis*, *Oscillatoria* |
| Nitrogen-fixing Cyanobacteria | FIX_CYN | N₂ fixation capability, includes *Aphanizomenon*, *Anabaena* |
| Nostocales | NOST | Heterocyst-forming, specialized N₂ fixers |
| Other Phytoplankton | OPA | Green algae, cryptophytes, other groups |

<div id="options"></div>

## Model Options

Several advanced features can be enabled via `PELAGIC_MODEL_OPTIONS.txt`:

| Option | Default | Description |
|:-------|:-------:|:------------|
| `ZOOPLANKTON_OPTION` | 1 | Realistic zooplankton CNP partitioning |
| `ADVANCED_REDOX_SIMULATION` | 0 | Full redox cycle (Fe, Mn, S, CH₄) |
| `LIGHT_EXTINCTION_OPTION` | 0 | Light extinction calculation method |
| `CYANO_BOUYANT_STATE_SIMULATION` | 1 | Cyanobacteria buoyancy dynamics |
| `CONSIDER_HETEROCYST_WITH_AKINETES` | 1 | Heterocyst and akinete formation |
| `CONSIDER_ALLELOPATHY` | 1 | Allelopathic interactions |

<div id="color-legend"></div>

## Color Legend for Optional Features

<div style="display: flex; flex-wrap: wrap; gap: 1rem; margin: 1rem 0;">
<span style="background-color: #28a745; color: white; padding: 0.3rem 0.8rem; border-radius: 4px; font-weight: bold;">🟢 Always Active</span>
<span style="background-color: #dc3545; color: white; padding: 0.3rem 0.8rem; border-radius: 4px; font-weight: bold;">🔴 ADVANCED_REDOX_SIMULATION</span>
<span style="background-color: #6f42c1; color: white; padding: 0.3rem 0.8rem; border-radius: 4px; font-weight: bold;">🟣 CONSIDER_HETEROCYST_WITH_AKINETES</span>
<span style="background-color: #fd7e14; color: white; padding: 0.3rem 0.8rem; border-radius: 4px; font-weight: bold;">🟠 CONSIDER_ALLELOPATHY</span>
</div>

| Color | Option | Description |
|:-----:|:-------|:------------|
| 🟢 | *Always active* | Core variables, always simulated |
| 🔴 | `ADVANCED_REDOX_SIMULATION` | Enables Fe, Mn, SO₄, methane cycling |
| 🟣 | `CONSIDER_HETEROCYST_WITH_AKINETES` | Enables heterocyst and akinete dynamics |
| 🟠 | `CONSIDER_ALLELOPATHY` | Enables allelopathic growth inhibition |

<div id="input-files"></div>

## Input Files

| File | Purpose |
|:-----|:--------|
| `PELAGIC_INPUTS.txt` | Main configuration file |
| `INIT_CONC_*.txt` | Initial concentrations for state variables |
| `PELAGIC_MODEL_CONSTANTS.txt` | Kinetic parameters and stoichiometry |
| `PELAGIC_MODEL_OPTIONS.txt` | Model option switches |
| `EXTRA_WCONST.txt` | Additional constants including allelopathy |
| `ALLELOPATHIC_INFORMATION.txt` | Box-specific allelopathy parameters |

<div id="output-files"></div>

## Output Files

| File | Content |
|:-----|:--------|
| `OUTPUT.csv` | Time series of all state variables |
| `PROCESS_RATES_*.csv` | Kinetic process rates |
| `MASS_BALANCE_*.csv` | Mass balance diagnostics |

---

<div id="part2"></div>

# Part 2: State Variables

<details open>
<summary style="font-size: 1.1rem; font-weight: bold; cursor: pointer; color: #198754; padding: 0.5rem 0;">📋 Table of Contents</summary>
<div style="padding-left: 1rem; margin-bottom: 1rem;">
<a href="#sv-overview">Overview</a> · 
<a href="#nutrients">2.1 Nutrients</a> · 
<a href="#gases">2.2 Dissolved Gases</a> · 
<a href="#phytoplankton">2.3 Phytoplankton</a> · 
<a href="#zooplankton">2.4 Zooplankton</a> · 
<a href="#pom">2.5 Particulate Organic Matter</a> · 
<a href="#dom">2.6 Dissolved Organic Matter</a> · 
<a href="#carbonate">2.7 Carbonate System</a> · 
<a href="#metals">2.8 Metals</a> · 
<a href="#sulphur">2.9 Sulphur Cycle</a> · 
<a href="#allelopathy-vars">2.10 Allelopathy Variables</a> · 
<a href="#quick-ref">Quick Reference</a> · 
<a href="#naming">Naming Convention</a>
</div>
</details>

<div id="sv-overview"></div>

## Overview

The AQUABC model tracks **36 state variables** organized into the following categories:

| Category | Count | Description |
|:---------|:-----:|:------------|
| Nutrients | 4 | Nitrogen, phosphorus, silica |
| Dissolved Gases | 2 | Oxygen and methane |
| Phytoplankton | 7 | Carbon biomass of algae groups |
| Zooplankton | 3 | Carbon, nitrogen, phosphorus |
| Particulate Organics | 4 | Detritus C, N, P, Si |
| Dissolved Organics | 3 | DOM C, N, P |
| Carbonate System | 2 | DIC and alkalinity |
| Metals | 6 | Fe, Mn, Ca, Mg |
| Sulphur | 2 | Sulphate and sulphide |
| Allelopathy | 4 | Secondary metabolites |

---

<div id="nutrients"></div>

## 2.1 Nutrients

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 1 | `NH4_N` | Ammonium Nitrogen | mg N/L | 🟢 | Dissolved ammonium (NH₄⁺-N). Primary nutrient for phytoplankton uptake. |
| 2 | `NO3_N` | Nitrate Nitrogen | mg N/L | 🟢 | Dissolved nitrate (NO₃⁻-N). Oxidized nitrogen form. |
| 3 | `PO4_P` | Orthophosphate Phosphorus | mg P/L | 🟢 | Dissolved reactive phosphorus (PO₄³⁻-P). Often limiting nutrient. |
| 17 | `DISS_Si` | Dissolved Silica | mg Si/L | 🟢 | Dissolved silicic acid. Essential for diatom growth. |

---

<div id="gases"></div>

## 2.2 Dissolved Gases

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 4 | `DISS_OXYGEN` | Dissolved Oxygen | mg O₂/L | 🟢 | Critical for aquatic life. From reaeration and photosynthesis. |
| 30 | `CH4_C` | Methane Carbon | mg C/L | 🔴 | Dissolved methane. Produced under anaerobic conditions. |

---

<div id="phytoplankton"></div>

## 2.3 Phytoplankton

All phytoplankton variables represent **carbon biomass**. Nitrogen and phosphorus content is calculated using internal stoichiometry.

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 5 | `DIA_C` | Diatom Carbon | mg C/L | 🟢 | Siliceous algae. Spring bloom formers. |
| 15 | `CYN_C` | Non-fixing Cyanobacteria Carbon | mg C/L | 🟢 | *Microcystis*, *Oscillatoria*. Cannot fix N₂. |
| 16 | `OPA_C` | Other Phytoplankton Carbon | mg C/L | 🟢 | Green algae, cryptophytes, others. |
| 19 | `FIX_CYN_C` | Nitrogen-fixing Cyanobacteria Carbon | mg C/L | 🟢 | *Aphanizomenon*, *Anabaena*. Can fix atmospheric N₂. |
| 31 | `NOST_VEG_HET_C` | Nostocales Heterocyst Carbon | mg C/L | 🟣 | Specialized N₂-fixing cells (heterocysts). |
| 32 | `AKI_C` | Akinetes Carbon | g/m² | 🟣 | Resting cells (akinetes). Benthic seed bank for blooms. |

---

<div id="zooplankton"></div>

## 2.4 Zooplankton

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 6 | `ZOO_C` | Zooplankton Carbon | mg C/L | 🟢 | Herbivorous grazers. Copepods, cladocerans, rotifers. |
| 7 | `ZOO_N` | Zooplankton Nitrogen | mg N/L | 🟢 | Nitrogen content of zooplankton biomass. |
| 8 | `ZOO_P` | Zooplankton Phosphorus | mg P/L | 🟢 | Phosphorus content of zooplankton biomass. |

---

<div id="pom"></div>

## 2.5 Particulate Organic Matter

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 9 | `DET_PART_ORG_C` | Detritus Particulate Organic Carbon | mg C/L | 🟢 | POC from dead organisms and feces. Subject to settling. |
| 10 | `DET_PART_ORG_N` | Detritus Particulate Organic Nitrogen | mg N/L | 🟢 | PON in detritus. Mineralizes to ammonium. |
| 11 | `DET_PART_ORG_P` | Detritus Particulate Organic Phosphorus | mg P/L | 🟢 | POP in detritus. Mineralizes to phosphate. |
| 18 | `PART_Si` | Particulate Silica | mg Si/L | 🟢 | Biogenic silica in diatom frustules. Dissolves to DSi. |

---

<div id="dom"></div>

## 2.6 Dissolved Organic Matter

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 12 | `DISS_ORG_C` | Dissolved Organic Carbon | mg C/L | 🟢 | DOC from exudation and cell lysis. Bacterial substrate. |
| 13 | `DISS_ORG_N` | Dissolved Organic Nitrogen | mg N/L | 🟢 | DON. Refractory and labile pools. Mineralizes to NH₄. |
| 14 | `DISS_ORG_P` | Dissolved Organic Phosphorus | mg P/L | 🟢 | DOP. Some is bioavailable. Mineralizes to PO₄. |

---

<div id="carbonate"></div>

## 2.7 Carbonate System

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 20 | `INORG_C` | Inorganic Carbon | mol C/L | 🟢 | Total DIC (CO₂ + HCO₃⁻ + CO₃²⁻). Controls pH. |
| 21 | `TOT_ALK` | Total Alkalinity | eq/L | 🟢 | Acid neutralizing capacity. Controls buffering. |

---

<div id="metals"></div>

## 2.8 Metals (Redox-sensitive)

Variables 22-25 require 🔴 `ADVANCED_REDOX_SIMULATION`.

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 22 | `FE_II` | Ferrous Iron | mg Fe/L | 🔴 | Fe²⁺. Reduced form, soluble under anoxic conditions. |
| 23 | `FE_III` | Ferric Iron | mg Fe/L | 🔴 | Fe³⁺. Oxidized form, precipitates as hydroxides. |
| 24 | `MN_II` | Manganous Manganese | mg Mn/L | 🔴 | Mn²⁺. Reduced form under anoxic conditions. |
| 25 | `MN_IV` | Manganic Manganese | mg Mn/L | 🔴 | Mn⁴⁺. Oxidized form, scavenges trace metals. |
| 26 | `CA` | Calcium | mg Ca/L | 🟢 | Major cation. Carbonate equilibria. |
| 27 | `MG` | Magnesium | mg Mg/L | 🟢 | Major cation. Conservative tracer. |

---

<div id="sulphur"></div>

## 2.9 Sulphur Cycle

Requires 🔴 `ADVANCED_REDOX_SIMULATION`.

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 28 | `S_PLUS_6` | Sulphate Sulphur | mg S/L | 🔴 | SO₄²⁻ as S. Electron acceptor under anoxia. |
| 29 | `S_MINUS_2` | Sulphide Sulphur | mg S/L | 🔴 | H₂S/HS⁻/S²⁻ as S. Toxic. From sulphate reduction. |

---

<div id="allelopathy-vars"></div>

## 2.10 Allelopathy State Variables

Requires 🟠 `CONSIDER_ALLELOPATHY`. See **<a href="#part3">Part 3</a>** for detailed description.

| Index | Variable | Full Name | Units | Option | Description |
|:-----:|:---------|:----------|:------|:------:|:------------|
| 33 | `SEC_METAB_DIA` | Diatom Secondary Metabolites | relative | 🟠 | Allelopathic compounds from diatoms. |
| 34 | `SEC_METAB_NOFIX_CYN` | Non-fixing Cyanobacteria Metabolites | relative | 🟠 | Toxins from *Microcystis*, etc. |
| 35 | `SEC_METAB_FIX_CYN` | Fixing Cyanobacteria Metabolites | relative | 🟠 | Compounds from *Aphanizomenon*, etc. |
| 36 | `SEC_METAB_NOST` | Nostocales Metabolites | relative | 🟠 | Compounds from heterocyst-forming cyanobacteria. |

---

<div id="quick-ref"></div>

## Quick Reference by Option

### 🟢 Always Active (23 variables)
`NH4_N`, `NO3_N`, `PO4_P`, `DISS_OXYGEN`, `DIA_C`, `ZOO_C`, `ZOO_N`, `ZOO_P`, `DET_PART_ORG_C`, `DET_PART_ORG_N`, `DET_PART_ORG_P`, `DISS_ORG_C`, `DISS_ORG_N`, `DISS_ORG_P`, `CYN_C`, `OPA_C`, `DISS_Si`, `PART_Si`, `FIX_CYN_C`, `INORG_C`, `TOT_ALK`, `CA`, `MG`

### 🔴 ADVANCED_REDOX_SIMULATION (7 variables)
`FE_II`, `FE_III`, `MN_II`, `MN_IV`, `S_PLUS_6`, `S_MINUS_2`, `CH4_C`

### 🟣 CONSIDER_HETEROCYST_WITH_AKINETES (2 variables)
`NOST_VEG_HET_C`, `AKI_C`

### 🟠 CONSIDER_ALLELOPATHY (4 variables)
`SEC_METAB_DIA`, `SEC_METAB_NOFIX_CYN`, `SEC_METAB_FIX_CYN`, `SEC_METAB_NOST`

---

<div id="naming"></div>

## Variable Naming Convention

| Pattern | Meaning | Examples |
|:--------|:--------|:---------|
| `*_N` | Nitrogen | `NH4_N`, `ZOO_N` |
| `*_P` | Phosphorus | `PO4_P`, `ZOO_P` |
| `*_C` | Carbon | `DIA_C`, `CH4_C` |
| `*_Si` | Silica | `DISS_Si`, `PART_Si` |
| `DISS_*` | Dissolved | `DISS_OXYGEN`, `DISS_ORG_C` |
| `PART_*` | Particulate | `PART_Si` |
| `DET_*` | Detrital | `DET_PART_ORG_C` |

---

## Output File Column Mapping

| Columns | Content |
|:--------|:--------|
| 1 | Time (days or Julian day) |
| 2-33 | State variables 1-32 (core AQUABC) |
| 34-37 | State variables 33-36 (allelopathy metabolites) |

---

<div id="part3"></div>

# Part 3: Allelopathy Model

<details open>
<summary style="font-size: 1.1rem; font-weight: bold; cursor: pointer; color: #6f42c1; padding: 0.5rem 0;">📋 Table of Contents</summary>
<div style="padding-left: 1rem; margin-bottom: 1rem;">
<a href="#allel-intro">Introduction</a> · 
<a href="#sec-metab-pools">Metabolite Pools</a> · 
<a href="#mass-balance">Mass Balance</a> · 
<a href="#formation">Formation Rate</a> · 
<a href="#degradation">Degradation Rate</a> · 
<a href="#inhibition">Inhibition Function</a> · 
<a href="#net-inhibition">Net Inhibition</a> · 
<a href="#interaction-matrix">Interaction Matrix</a> · 
<a href="#parameters">Parameters</a> · 
<a href="#config">Configuration</a> · 
<a href="#applications">Applications</a> · 
<a href="#references">References</a>
</div>
</details>

<div id="allel-intro"></div>

## 3.1 Introduction

**Allelopathy** refers to biochemical interactions between organisms through the release of secondary metabolites. In aquatic ecosystems, phytoplankton species produce allelochemicals that can inhibit the growth of competing species.

When `CONSIDER_ALLELOPATHY = 1` is set in model options, AQUABC simulates:
- Production of secondary metabolites during phytoplankton cell death
- Temperature-dependent degradation of metabolites
- Growth inhibition of competing phytoplankton groups

<div id="sec-metab-pools"></div>

## 3.2 Secondary Metabolite Pools

The model tracks **four secondary metabolite pools**, one for each major phytoplankton group:

| Pool | Source Organism | Example Compounds |
|:-----|:----------------|:------------------|
| `SEC_METAB_DIA` | Diatoms | Polyunsaturated aldehydes, oxylipins |
| `SEC_METAB_NOFIX_CYN` | Non-fixing Cyanobacteria | Microcystins, cylindrospermopsin |
| `SEC_METAB_FIX_CYN` | N-fixing Cyanobacteria | Anatoxins, saxitoxins |
| `SEC_METAB_NOST` | Nostocales | Nodularins, aplysiatoxins |

<div id="mass-balance"></div>

## 3.3 Mass Balance Equation

The concentration of secondary metabolites changes according to:

<div class="formula-box" style="background: #f8f9fa; border-left: 4px solid #6f42c1; padding: 1rem; margin: 1rem 0; font-family: 'Courier New', monospace;">
<strong>d[SEC_METAB<sub>i</sub>]/dt = R<sub>FORM,i</sub> − R<sub>DEG,i</sub></strong>
</div>

where:
- **i** = phytoplankton group (DIA, NOFIX_CYN, FIX_CYN, NOST)
- **R<sub>FORM,i</sub>** = formation rate from dying cells
- **R<sub>DEG,i</sub>** = degradation rate

<div id="formation"></div>

## 3.4 Formation Rate

Secondary metabolites are produced proportionally to phytoplankton death:

<div class="formula-box" style="background: #f8f9fa; border-left: 4px solid #6f42c1; padding: 1rem; margin: 1rem 0; font-family: 'Courier New', monospace;">
<strong>R<sub>FORM,i</sub> = S<sub>i</sub> × R<sub>DEATH,i</sub></strong>
</div>

| Symbol | Parameter | Description | Units |
|:------:|:----------|:------------|:------|
| S<sub>i</sub> | `S_SEC_METAB_TO_*` | Stoichiometric yield coefficient | dimensionless |
| R<sub>DEATH,i</sub> | `ALLEL_R_DEATH_*` | Death rate of phytoplankton group | mg C/L/day |

<div id="degradation"></div>

## 3.5 Degradation Rate

Metabolites degrade following **first-order kinetics** with temperature dependence:

<div class="formula-box" style="background: #f8f9fa; border-left: 4px solid #6f42c1; padding: 1rem; margin: 1rem 0; font-family: 'Courier New', monospace;">
<strong>R<sub>DEG,i</sub> = k<sub>i</sub><sup>20</sup> × θ<sub>i</sub><sup>(T−20)</sup> × [SEC_METAB<sub>i</sub>]</strong>
</div>

| Symbol | Parameter | Description | Units |
|:------:|:----------|:------------|:------|
| k<sub>i</sub><sup>20</sup> | `k_DEG_SEC_METAB_*_20` | Degradation rate at 20°C | 1/day |
| θ<sub>i</sub> | `THETA_k_DEG_SEC_METAB_*` | Temperature coefficient | dimensionless |
| T | `WATER_TEMP` | Water temperature | °C |

<div id="inhibition"></div>

## 3.6 Growth Inhibition Function

The inhibition of species **j** by metabolites from species **i** follows a **reverse Monod function**:

<div class="formula-box" style="background: #f8f9fa; border-left: 4px solid #6f42c1; padding: 1rem; margin: 1rem 0; font-family: 'Courier New', monospace;">
<strong>IHBF<sub>i→j</sub> = K<sub>HS,i→j</sub> / (K<sub>HS,i→j</sub> + [SEC_METAB<sub>i</sub>])</strong>
</div>

where:
- **IHBF<sub>i→j</sub>** = inhibition factor (0 to 1)
- **K<sub>HS,i→j</sub>** = half-saturation constant

**Interpretation:**
| Condition | IHBF Value | Effect |
|:----------|:-----------|:-------|
| [SEC_METAB<sub>i</sub>] = 0 | 1 | No inhibition |
| [SEC_METAB<sub>i</sub>] = K<sub>HS</sub> | 0.5 | 50% inhibition |
| [SEC_METAB<sub>i</sub>] >> K<sub>HS</sub> | → 0 | Full inhibition |

<div id="net-inhibition"></div>

## 3.7 Net Inhibition Factor

The **net inhibition** for each phytoplankton group is the **minimum** of all individual inhibition factors:

<div class="formula-box" style="background: #f8f9fa; border-left: 4px solid #6f42c1; padding: 1rem; margin: 1rem 0; font-family: 'Courier New', monospace;">
<strong>IHBF<sub>target</sub> = min(IHBF<sub>DIA→target</sub>, IHBF<sub>NOFIX→target</sub>, IHBF<sub>FIX→target</sub>, IHBF<sub>NOST→target</sub>)</strong>
</div>

This factor (0-1) multiplies the growth rate, representing the most severe inhibition from any competitor.

<div id="interaction-matrix"></div>

## 3.8 Interaction Matrix

The allelopathy module uses a matrix of half-saturation constants:

| Source ↓ / Target → | DIA | NOFIX_CYN | FIX_CYN | NOST | OPA | ZOO |
|:--------------------|:---:|:---------:|:-------:|:----:|:---:|:---:|
| **DIA** | — | ✓ | ✓ | ✓ | ✓ | ✓ |
| **NOFIX_CYN** | ✓ | — | ✓ | ✓ | ✓ | ✓ |
| **FIX_CYN** | ✓ | ✓ | — | ✓ | ✓ | ✓ |
| **NOST** | ✓ | ✓ | ✓ | — | ✓ | ✓ |

**Note**: Lower K<sub>HS</sub> values = stronger inhibition at lower metabolite concentrations.

<div id="parameters"></div>

## 3.9 Model Parameters

### Stoichiometric Coefficients

| Parameter | Description | Typical Value |
|:----------|:------------|:--------------|
| `S_SEC_METAB_TO_DIA` | Metabolite yield from diatom death | 0.01-0.1 |
| `S_SEC_METAB_TO_NOFIX_CYN` | Yield from non-fixing cyanobacteria | 0.01-0.1 |
| `S_SEC_METAB_TO_FIX_CYN` | Yield from N-fixing cyanobacteria | 0.01-0.1 |
| `S_SEC_METAB_TO_NOST` | Yield from Nostocales | 0.01-0.1 |

### Degradation Parameters

| Parameter | Description | Typical Value |
|:----------|:------------|:--------------|
| `k_DEG_SEC_METAB_*_20` | Degradation rate at 20°C | 0.05-0.5 /day |
| `THETA_k_DEG_SEC_METAB_*` | Temperature coefficient | 1.04-1.08 |

### Half-Saturation Constants

| Parameter Pattern | Description | Typical Value |
|:------------------|:------------|:--------------|
| `K_HS_SEC_METAB_*_*` | Inhibition half-saturation | 0.1-10 |

<div id="config"></div>

## 3.10 Configuration

### Enabling Allelopathy

In `PELAGIC_MODEL_OPTIONS.txt`:
```
# CONSIDER_ALLELOPATHY
1
# CYN_ALLELOPATHY_FILE_NAME
ALLELOPATHIC_INFORMATION.txt
```

### Parameter Files

| File | Content |
|:-----|:--------|
| `EXTRA_WCONST.txt` | Stoichiometry, degradation rates, K<sub>HS</sub> values |
| `ALLELOPATHIC_INFORMATION.txt` | Box-specific allelopathy parameters |

<div id="applications"></div>

## 3.11 Ecological Applications

Allelopathy is important for understanding:

| Application | Relevance |
|:------------|:----------|
| **Harmful Algal Blooms** | Cyanobacteria suppress competitors to dominate |
| **Seasonal Succession** | Changes in competitive interactions drive community shifts |
| **Species Dominance** | Allelopathy explains persistence of toxic species |
| **Water Quality** | Cyanotoxin dynamics affect drinking water safety |

<div id="references"></div>

## 3.12 Scientific References

1. **Granéli, E. & Johansson, N. (2003)**. Increase in the production of allelopathic substances by *Prymnesium parvum* cells grown under N- or P-deficient conditions. *Harmful Algae*, 2(2), 135-145.

2. **Legrand, C., Rengefors, K., Fistarol, G.O., & Granéli, E. (2003)**. Allelopathy in phytoplankton - biochemical, ecological and evolutionary aspects. *Phycologia*, 42(4), 406-419.

3. **Leflaive, J. & Ten-Hage, L. (2007)**. Algal and cyanobacterial secondary metabolites in freshwaters: a comparison of allelopathic compounds and toxins. *Freshwater Biology*, 52(2), 199-214.

4. **Suikkanen, S., Fistarol, G.O., & Granéli, E. (2004)**. Allelopathic effects of the Baltic cyanobacteria *Nodularia spumigena*, *Aphanizomenon flos-aquae* and *Anabaena lemmermannii* on algal monocultures. *Journal of Experimental Marine Biology and Ecology*, 308(1), 85-101.

5. **Rzymski, P., et al. (2014)**. Interspecific allelopathy in cyanobacteria: Cylindrospermopsin and *Cylindrospermopsis raciborskii* effect on the growth and metabolism of *Microcystis aeruginosa*. *Harmful Algae*, 35, 1-8.

---

<div id="part4"></div>

# Part 4: Scientific Review & Development Roadmap

<details open>
<summary style="font-size: 1.1rem; font-weight: bold; cursor: pointer; color: #dc3545; padding: 0.5rem 0;">📋 Table of Contents</summary>
<div style="padding-left: 1rem; margin-bottom: 1rem;">
<a href="#sci-summary">Executive Summary</a> · 
<a href="#sci-strengths">Strengths</a> · 
<a href="#sci-limitations">Limitations</a> · 
<a href="#sci-improvements">Improvement Avenues</a> · 
<a href="#sci-priority">Priority Recommendations</a>
</div>
</details>

<div id="sci-summary"></div>

## 4.1 Executive Summary

The AQUABC allelopathy module implements competitive interference through secondary metabolite dynamics. This section provides a scientific review of the implementation and identifies opportunities for enhancement.

**Current capabilities:**
- ✓ Four secondary metabolite pools (DIA, NOFIX_CYN, FIX_CYN, NOST)
- ✓ Death-mediated release with stoichiometric coefficients
- ✓ Temperature-dependent first-order degradation
- ✓ Reverse Monod inhibition function
- ✓ Minimum inhibition constraint (most severe effect wins)
- ✓ Integration with growth and grazing kinetics

<div id="sci-strengths"></div>

## 4.2 Scientific Strengths

### Mechanistic Foundation

1. **Death-mediated release**: Correctly implements intracellular toxin release upon cell lysis
2. **Temperature-dependent degradation**: Arrhenius kinetics (θ = 1.04-1.06) appropriate for biochemical degradation
3. **Functional group specificity**: Different groups produce allelochemicals with different potencies
4. **Asymmetric interaction matrix**: Species A can inhibit B more than B inhibits A

### Integration with Core Model

The `GROWTH_INHIB_FACTOR` is multiplicatively applied to:
- All phytoplankton growth rates
- Zooplankton feeding rates

<div id="sci-limitations"></div>

## 4.3 Scientific Limitations

### 4.3.1 Release Mechanism

**Current**: Metabolites released only upon death

**Reality**: Many allelochemicals are actively exuded by living cells under nutrient stress (Granéli & Johansson, 2003)

### 4.3.2 Uniform Toxin Properties

**Current**: Single generic metabolite per group

**Reality**: Cyanobacteria produce diverse toxins:

| Class | Examples | Stability |
|:------|:---------|:----------|
| Hepatotoxins | Microcystins, Nodularin | Very stable |
| Neurotoxins | Anatoxin-a, Saxitoxin | Less stable |
| Cytotoxins | Cylindrospermopsin | Moderate |

### 4.3.3 Missing Photodegradation

**Current**: Temperature-only degradation

**Reality**: Microcystin half-lives 1-10 days under sunlight vs >30 days in dark (Welker & Steinberg, 2000)

### 4.3.4 No Bioaccumulation

**Current**: Toxins only in water column

**Reality**: Zooplankton and fish bioaccumulate cyanotoxins → food web effects

### 4.3.5 Missing Bacterial Degradation

Some bacteria (*Sphingomonas* spp.) rapidly degrade microcystins via mlr genes (Jones et al., 1994)

<div id="sci-improvements"></div>

## 4.4 Improvement Avenues

### Short-term (v0.3)

| Enhancement | Description | Effort |
|:------------|:------------|:------:|
| **Active exudation** | Add stress-dependent exudation pathway | Low |
| **Photodegradation** | Light-dependent degradation rate | Low |
| **Variable K<sub>HS</sub>** | Temperature-dependent sensitivity | Low |

### Medium-term (v0.4)

| Enhancement | Description | Effort |
|:------------|:------------|:------:|
| **Multiple toxin classes** | 2-3 toxin types per group | Medium |
| **Zooplankton accumulation** | Internal toxin burden | Medium |
| **Bacterial degradation** | Microbial toxin removal | Medium |

### Long-term (v1.0+)

| Enhancement | Description | Effort |
|:------------|:------------|:------:|
| **Intracellular quota** | Cell quota-based production | High |
| **Gene expression** | Molecular-based toxin synthesis | High |
| **Food web transfer** | Fish bioaccumulation | High |

<div id="sci-priority"></div>

## 4.5 Priority Recommendations

### High Priority for v0.3

1. **Add nutrient-stress exudation** — improves realism with minimal changes
2. **Implement light-dependent degradation** — important for surface dynamics
3. **Enhanced diagnostics** — output inhibition factors for analysis

### Medium Priority for v0.4

4. **Variable temperature sensitivity**
5. **Zooplankton toxin accumulation**
6. **Multiple toxin classes**

### Validation Data Needs

| Data Type | Priority |
|:----------|:--------:|
| Cyanotoxin concentrations | High |
| Phytoplankton biovolumes by group | High |
| Toxin-producer fractions | Medium |
| Degradation rates (field) | Medium |

---

*For the complete scientific review with implementation details, see `ALLELOPATHY_SCIENTIFIC_REVIEW.md`*

---

<div style="text-align: center; color: #6c757d; margin-top: 2rem; padding-top: 1rem; border-top: 1px solid #dee2e6;">
<em>AQUABC v0.2 Documentation</em><br>
<a href="#" onclick="document.querySelector('.offcanvas-body').scrollTop=0; return false;">↑ Back to Top</a>
</div>
