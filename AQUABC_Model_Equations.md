# AQUABC v0.2 Ecological Model: Mathematical Documentation

## Table of Contents

1. [Model Overview](#1-model-overview)
2. [State Variables](#2-state-variables)
3. [Driving Functions](#3-driving-functions)
4. [Phytoplankton Submodels](#4-phytoplankton-submodels)
5. [Zooplankton Dynamics](#5-zooplankton-dynamics)
6. [Organic Matter Cycling](#6-organic-matter-cycling)
7. [Dissolved Oxygen Dynamics](#7-dissolved-oxygen-dynamics)
8. [Nutrient Cycling](#8-nutrient-cycling)
9. [Redox Chemistry and Metal Speciation](#9-redox-chemistry-and-metal-speciation)
10. [Iron Geochemistry](#10-iron-geochemistry)
11. [Inorganic Phosphorus Solubility](#11-inorganic-phosphorus-solubility)
12. [CO2 System (Carbonate Chemistry)](#12-co2-system-carbonate-chemistry)
13. [Sediment Diagenesis Model](#13-sediment-diagenesis-model)
14. [Macroalgae Model](#14-macroalgae-model)
15. [Allelopathy Module](#15-allelopathy-module)
16. [Auxiliary Functions](#16-auxiliary-functions)
17. [Numerical Solver](#17-numerical-solver)
18. [Physical Constants and Safety Functions](#18-physical-constants-and-safety-functions)
19. [Model Constants Summary](#19-model-constants-summary)

---

## 1. Model Overview

AQUABC v0.2 is a comprehensive aquatic biogeochemical model that simulates coupled pelagic-sediment dynamics. The model is organized as:

- **Pelagic model**: 32 state variables covering phytoplankton (5 functional groups), zooplankton, dissolved/particulate organic matter, dissolved oxygen, nutrients (N, P, Si), inorganic carbon, alkalinity, redox-sensitive metals (Fe, Mn), sulphur species, and methane.
- **Sediment model**: 24 state variables for early diagenesis with advanced redox sequencing.
- **Macroalgae model**: 6 state variables using Droop quota kinetics.
- **Allelopathy module**: 4 secondary metabolite pools for inter-species chemical inhibition.
- **CO2SYS**: Full carbonate chemistry solver (CDIAC implementation).

**Main source files:**

| Component | File |
|-----------|------|
| Pelagic kinetics | `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` |
| Phytoplankton libraries | `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_*.f90` |
| Sediment model | `SOURCE_CODE/AQUABC/SEDIMENTS/aquabc_II_sediment_model.f90` |
| Macroalgae | `SOURCE_CODE/MACROALGAE/mod_MACROALGAE.f90` |
| CO2SYS | `SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90` |
| Allelopathy | `SOURCE_CODE/ALLELOPATHY/mod_ALLELOPATHY.f90` |
| Solver | `SOURCE_CODE/ESTAS/mod_SOLVER.f90` |

---

## 2. State Variables

### 2.1 Pelagic State Variables (32)

Defined in `aquabc_II_pelagic_svindex.f90`:

| Index | Variable | Units | Description |
|-------|----------|-------|-------------|
| 1 | NH4_N | mg-N/L | Ammonium nitrogen |
| 2 | NO3_N | mg-N/L | Nitrate nitrogen |
| 3 | PO4_P | mg-P/L | Orthophosphate phosphorus |
| 4 | DISS_OXYGEN | mg-O2/L | Dissolved oxygen |
| 5 | DIA_C | mg-C/L | Diatom carbon |
| 6 | ZOO_C | mg-C/L | Zooplankton carbon |
| 7 | ZOO_N | mg-N/L | Zooplankton nitrogen |
| 8 | ZOO_P | mg-P/L | Zooplankton phosphorus |
| 9 | DET_PART_ORG_C | mg-C/L | Detrital particulate organic carbon |
| 10 | DET_PART_ORG_N | mg-N/L | Detrital particulate organic nitrogen |
| 11 | DET_PART_ORG_P | mg-P/L | Detrital particulate organic phosphorus |
| 12 | DISS_ORG_C | mg-C/L | Dissolved organic carbon |
| 13 | DISS_ORG_N | mg-N/L | Dissolved organic nitrogen |
| 14 | DISS_ORG_P | mg-P/L | Dissolved organic phosphorus |
| 15 | CYN_C | mg-C/L | Non-fixing cyanobacteria carbon |
| 16 | OPA_C | mg-C/L | Other planktonic algae carbon |
| 17 | DISS_Si | mg-Si/L | Dissolved silica |
| 18 | PART_Si | mg-Si/L | Particulate (biogenic) silica |
| 19 | FIX_CYN_C | mg-C/L | Nitrogen-fixing cyanobacteria carbon |
| 20 | INORG_C | mol-C/L | Dissolved inorganic carbon (DIC) |
| 21 | TOT_ALK | meq/L | Total alkalinity |
| 22 | FE_II | mg-Fe/L | Ferrous iron (Fe2+) |
| 23 | FE_III | mg-Fe/L | Ferric iron (Fe3+) |
| 24 | MN_II | mg-Mn/L | Manganous manganese (Mn2+) |
| 25 | MN_IV | mg-Mn/L | Manganic manganese (Mn4+) |
| 26 | CA | mmol/L | Calcium |
| 27 | MG | mmol/L | Magnesium |
| 28 | S_PLUS_6 | mg-S/L | Sulphate sulphur (SO4-S) |
| 29 | S_MINUS_2 | mg-S/L | Sulphide sulphur (H2S-S) |
| 30 | CH4_C | mg-C/L | Methane carbon |
| 31 | NOST_VEG_HET_C | mg-C/L | Nostocales vegetative+heterocyst carbon |
| 32 | NOST_AKI_C | mg-C/L | Nostocales akinete (spore) carbon |

### 2.2 Sediment State Variables (24)

Mirrors the pelagic model with sediment-specific organic matter, nutrients, and redox species per sediment layer.

### 2.3 Macroalgae State Variables (6)

| Variable | Units | Description |
|----------|-------|-------------|
| MAC_ALGAE_C | g-C/m2 | Live macroalgae carbon |
| MAC_ALGAE_N | g-N/m2 | Live macroalgae nitrogen |
| MAC_ALGAE_P | g-P/m2 | Live macroalgae phosphorus |
| MAC_ALGAE_ATT_DETPOC | g-C/m2 | Attached dead particulate organic carbon |
| MAC_ALGAE_ATT_DETPON | g-N/m2 | Attached dead particulate organic nitrogen |
| MAC_ALGAE_ATT_DETPOP | g-P/m2 | Attached dead particulate organic phosphorus |

---

## 3. Driving Functions

The model requires 10 time-dependent forcing inputs:

| Index | Variable | Units | Description |
|-------|----------|-------|-------------|
| 1 | TEMP | deg C | Water temperature (clamped to [0, 45]) |
| 2 | SALT | PSU | Salinity (clamped to >= 0) |
| 3 | I_A | langleys/day | Photosynthetically active radiation |
| 4 | FDAY | fraction | Fraction of day (photoperiod) |
| 5 | AIRTEMP | deg C | Air temperature |
| 6 | WINDS | m/s | Wind speed |
| 7 | ELEVATION | m | Elevation above sea level |
| 8 | DEPTH | m | Water column depth |
| 9 | K_B_E | 1/m | Background light extinction coefficient |
| 10 | ice_cover | fraction | Ice cover fraction (0-1) |

---

## 4. Phytoplankton Submodels

Five phytoplankton functional groups share a common equation framework with group-specific extensions.

### 4.1 Common Equation Framework

All groups follow the general mass balance:

```
d[PHY_C]/dt = R_GROWTH - R_TOT_RESP - R_EXCR - R_DEATH - R_ZOO_FEEDING
```

where each term is described below.

#### 4.1.1 Temperature Limitation

File: `aquabc_II_pelagic_auxillary.f90`, subroutine `GROWTH_AT_TEMP`

```
if T <= T_lower:
    LIM_TEMP = exp(-kappa_under * |T_lower - T|)
if T_lower < T < T_upper:
    LIM_TEMP = 1.0
if T >= T_upper:
    LIM_TEMP = exp(-kappa_over * |T_upper - T|)

KG = KG_OPT_TEMP * LIM_TEMP
```

Organisms have an optimal temperature range [T_lower, T_upper] with exponential decay outside it.

#### 4.1.2 Light Limitation

Two selectable formulations via the SMITH_FORMULATION flag:

**Steele integral (SMITH_FORMULATION = 0):**

```
alpha_0 = I_A / I_S                               (surface)
alpha_1 = (I_A / I_S) * exp(-K_E * DEPTH)         (bottom)

LIM_LIGHT = (e * FDAY) / (K_E * DEPTH) * [exp(-alpha_1) - exp(-alpha_0)]
```

where I_S is the light saturation intensity, K_E is the light extinction coefficient, and e = 2.718...

**Smith formulation (SMITH_FORMULATION = 1):**

```
call LIM_LIGHT(I_A, CHLA, KG, DEPTH, K_E, LIM_LIGHT_OUT,
               C_TO_CHLA, I_S, LIGHT_SAT, nkn)
```

Hyperbolic tangent-like formulation with depth integration.

#### 4.1.3 Nutrient Limitation (Monod Kinetics)

```
LIM_N = [DIN] / (K_HS_DIN + [DIN])
LIM_P = [PO4] / (K_HS_DIP + [PO4])
LIM_NUTR = min(LIM_N, LIM_P)                      (Liebig's Law of the Minimum)
```

#### 4.1.4 Dissolved Oxygen Limitation

```
LIM_DOXY = [O2] / (K_HS_O2 + [O2])
```

#### 4.1.5 Overall Growth Limitation

```
LIM_TOTAL = LIM_LIGHT * min(LIM_DOXY, LIM_NUTR)
```

#### 4.1.6 Growth Rate

```
R_GROWTH = KG * LIM_TOTAL * [PHY_C]
```

#### 4.1.7 Metabolic Partitioning

```
R_MET      = R_GROWTH * (1 - EFF_GROWTH)          (metabolic loss)
R_RESP     = (1 - FRAC_EXCR) * R_MET              (respiration fraction)
R_EXCR     = FRAC_EXCR * R_MET                    (excretion fraction)
R_INT_RESP = KR_20 * theta_R^(T-20) * LIM_DOXY * [PHY_C]   (basal respiration)
R_TOT_RESP = R_INT_RESP + R_RESP
```

#### 4.1.8 Mortality with Hypoxia Response

```
KD = KD_20 * theta_D^(T-20)

if [O2] > DO_threshold:
    FAC_HYPOX = 1.0
elif [O2]/DO_threshold > 0.1:
    FAC_HYPOX = theta_hypox^(exp_hypox * (DO_threshold - [O2]))
else (extreme hypoxia):
    FAC_HYPOX = min(dt/(0.5*KD), 0.9/(KD*dt))
    R_GROWTH = 0, R_RESP = 0, R_INT_RESP = 0

R_DEATH = KD * FAC_HYPOX * [PHY_C]
```

#### 4.1.9 Ammonia Preference (WASP formulation)

File: `aquabc_II_pelagic_auxillary.f90`

```
PREF_NH4 = ([NH4]*[NO3]) / ((K+[NH4])*(K+[NO3])) + (K*[NH4]) / (([NH4]+[NO3])*(K+[NO3]))
```

### 4.2 Diatoms

File: `aquabc_II_pelagic_lib_DIATOMS.f90`

**Unique features:**
- Silicon limitation: `LIM_Si = [Si] / (K_HS_Si + [Si])`
- Combined: `LIM_NUTR = min(LIM_N, LIM_P, LIM_Si)`
- No DON utilization
- No buoyancy regulation

**Key parameters:** KG_DIA_OPT_TEMP = 3.7/day, KR_DIA_20 = 0.05/day, KD_DIA_20 = 0.12/day

### 4.3 Non-Fixing Cyanobacteria

File: `aquabc_II_pelagic_lib_CYANOBACTERIA.f90`

**Unique features:**
- DON utilization: `LIM_N = ([NH4] + f_avail*[DON] + [NO3]) / (K_N + [NH4] + f_avail*[DON] + [NO3])`
- No silicon limitation
- Optional buoyancy model (subroutine `CYANOBACTERIA_BOUYANT`)

**Buoyancy model:**
```
EUPHOTIC_DEPTH = 4.61 / K_E
MIX_DEPTH = 0.8121 * WINDS + 0.7006        (Nagy et al. 2006)
CYANO_DEPTH = f(MIX_DEPTH, EUPHOTIC_DEPTH, DEPTH)
```

### 4.4 Nitrogen-Fixing Cyanobacteria

File: `aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90`

**Unique features:**
- Two metabolic fractions (fixing and non-fixing)
- Non-fixing fraction uses DIN: `LIM_N_nonfix = ([NH4]+f*[DON]+[NO3]) / (K+[NH4]+f*[DON]+[NO3])`
- Fixing fraction has inverse Monod: `LIM_N_fix = K_FIX / (K_FIX + [NH4]+f*[DON]+[NO3])`
- Total growth: `R_GROWTH = R_NON_FIX_GROWTH + R_FIX * KG * LIM_FIX * [FIX_CYN_C]`
- Optional buoyancy model

### 4.5 Other Planktonic Algae (OPA)

File: `aquabc_II_pelagic_lib_OTHER_PLANKTONIC_ALGAE.f90`

**Represents:** Flagellates, cryptophytes, chrysophytes

**Features:**
- Standard N and P limitation (no Si, no DON)
- No buoyancy regulation
- Ammonia preference (WASP formulation)

### 4.6 Nostocales

File: `aquabc_II_pelagic_lib_NOSTACALES.f90`

**Unique features:**
- Obligate N2 fixer with heterocyst differentiation
- Complex lifecycle with akinete (spore) stage
- Buoyancy regulation
- Density-dependent mortality
- DON + DIN combined nitrogen source

**Lifecycle equations:**

```
Fixing fraction:
    LIM_FIX = LIM_LIGHT * min(LIM_O2, LIM_P)              (only P-limited)
    R_FIX_GROWTH = f_fix * KG * LIM_FIX * [NOST_C]

Non-fixing fraction:
    LIM_NONFIX = LIM_LIGHT * min(LIM_O2, LIM_P, LIM_N)    (N+P limited)
    R_NONFIX_GROWTH = (1-f_fix) * KG * LIM_NONFIX * [NOST_C]

Density-dependent mortality:
    R_DENS_MORT = M_density * [NOST_C]^2

Akinete germination (low DIN + warm temperature trigger):
    if [DIN] < K_N_germ AND T > T_germ:
        R_GERM = K_germ * [AKI_C]

Akinete formation (cold + late season trigger):
    if T < T_form AND day_of_year > day_form:
        R_FORM = K_form * [NOST_C]

Akinete loss/mortality:
    R_LOSS_AKI = K_loss * [AKI_C]
    R_MORT_AKI = K_mort_20 * theta^(T-20) * [AKI_C]
```

### 4.7 Comparative Feature Table

| Feature | Diatoms | Non-Fix Cyan | Fix Cyan | OPA | Nostocales |
|---------|---------|-------------|---------|-----|-----------|
| Si limitation | Yes | No | No | No | No |
| DON utilization | No | Yes | Yes | No | Yes |
| N2 fixation | No | No | Yes | No | Yes (obligate) |
| Buoyancy model | No | Yes | Yes | No | Yes |
| Akinete lifecycle | No | No | No | No | Yes |
| Density mortality | No | No | No | No | Yes |

---

## 5. Zooplankton Dynamics

File: `aquabc_II_pelagic_lib_ZOOPLANKTON.f90`

### 5.1 Feeding (Multi-Prey Monod)

```
For each prey type i in {DIA, CYN, OPA, FIX_CYN, NOST, DET_PART_ORG_C}:

    FOOD_FACTOR_i = PREF_i * [PREY_i] / (KHS_i + TOTAL_AVAILABLE_FOOD)

    R_FEEDING_i = KG_ZOO * theta^(T-20) * FOOD_FACTOR_i * [ZOO_C]
```

**Predation limiter** (mass-balance safeguard):
```
if R_FEEDING_i > 0.5 * [PREY_i] / dt:
    R_FEEDING_i = 0.5 * [PREY_i] / dt
```

### 5.2 Growth and Respiration

```
R_ZOO_GROWTH    = sum(R_FEEDING_i)
R_ZOO_RESP      = R_ZOO_GROWTH * (1 - EFF_ZOO_GROWTH)
R_ZOO_INT_RESP  = KR_ZOO_20 * theta^(T-20) * [ZOO_C]
R_ZOO_TOT_RESP  = R_ZOO_INT_RESP + R_ZOO_RESP
```

### 5.3 Excretion

```
R_ZOO_EX_DOC = R_ZOO_GROWTH * FRAC_ZOO_EX_ORG
R_ZOO_EX_DON = R_ZOO_GROWTH * ZOO_N_TO_C
R_ZOO_EX_DOP = R_ZOO_GROWTH * ZOO_P_TO_C
```

### 5.4 Mortality with Hypoxia

```
KD_ZOO = KD_ZOO_20 * theta^(T-20)

if [O2] <= DO_STR_HYPOX_ZOO_D:
    if [O2]/DO_threshold > 0.1:
        FAC_HYPOX = theta_hypox^(exp_hypox * (DO_threshold - [O2]))
    else:
        FAC_HYPOX = min(dt/(0.5*KD), 0.9/(KD*dt))
        Set all feeding and respiration to 0
else:
    FAC_HYPOX = 1.0

R_ZOO_DEATH = KD_ZOO * FAC_HYPOX * [ZOO_C]
```

### 5.5 Loss Limiter

```
total_loss = R_DEATH + R_INT_RESP + R_RESP
if total_loss > 0.5 * [ZOO_C] / dt:
    scale = (0.5 * [ZOO_C] / dt) / total_loss
    All loss terms *= scale
```

### 5.6 N:P Stoichiometry

When ZOOP_OPTION_1 > 0:
```
ACTUAL_ZOO_N_TO_C = [ZOO_N] / max([ZOO_C], MIN_CONCENTRATION)
ACTUAL_ZOO_P_TO_C = [ZOO_P] / max([ZOO_C], MIN_CONCENTRATION)
```

**Default parameters:** KG_ZOO_OPT_TEMP = 0.45/day, PREF_ZOO_DIA = 0.26, PREF_ZOO_OPA = 0.37, FOOD_MIN_ZOO = 0.02 mg/L

---

## 6. Organic Matter Cycling

### 6.1 Particulate Organic Matter Dissolution

File: `aquabc_II_pelagic_lib_ORGANIC_CARBON.f90`

```
R_DISS_POC = KDISS_POC_20 * theta^(T-20) * [POC]
R_DISS_PON = KDISS_PON_20 * theta^(T-20) * [PON]
R_DISS_POP = KDISS_POP_20 * theta^(T-20) * [POP]
R_DISS_PSi = KDISS_PSi_20 * theta^(T-20) * [PSi]
```

Phytoplankton-accelerated dissolution when nutrients are scarce:
```
LIM_PHYT_DISS = PHYT_TOT_C / (PHYT_TOT_C + KHS_POC_DISS_SAT)
```

### 6.2 Dissolved Organic Matter Mineralization

Six electron acceptor pathways in strict thermodynamic sequence:

```
R_DOC_MIN_DOXY    = K_20 * theta^(T-20) * LIM_DOXY_RED    * PH_CORR * [DOC]/(K_HS+[DOC]) * [DOC]
R_DOC_MIN_NO3N    = K_20 * theta^(T-20) * LIM_NO3N_RED    * PH_CORR * [DOC]/(K_HS+[DOC]) * [DOC]
R_DOC_MIN_MN_IV   = K_20 * theta^(T-20) * LIM_MN_IV_RED   * PH_CORR * [DOC]/(K_HS+[DOC]) * [DOC]
R_DOC_MIN_FE_III  = K_20 * theta^(T-20) * LIM_FE_III_RED  * PH_CORR * [DOC]/(K_HS+[DOC]) * [DOC]
R_DOC_MIN_S_PLUS_6= K_20 * theta^(T-20) * LIM_S_PLUS_6_RED* PH_CORR * [DOC]/(K_HS+[DOC]) * [DOC]
R_DOC_MIN_DOC     = K_20 * theta^(T-20) * LIM_DOC_RED     * PH_CORR * [DOC]/(K_HS+[DOC]) * [DOC]
```

Same structure for DON and DOP mineralization.

### 6.3 Denitrification

```
R_DENITRIFICATION = 0.93 * R_DOC_MIN_NO3N
```

**Defaults:** KDISS_POC_20 = 0.1/day, KDISS_PON_20 = 0.25/day, KDISS_POP_20 = 0.48/day

---

## 7. Dissolved Oxygen Dynamics

### 7.1 Sources (Photosynthesis)

```
DO_from_DIA = R_DIA_GROWTH * (1.3 - 0.3*PREF_NH4N_DIA) * DIA_O2_TO_C
DO_from_CYN = R_CYN_GROWTH * (1.3 - 0.3*PREF_NH4N_CYN) * CYN_O2_TO_C
DO_from_OPA = R_OPA_GROWTH * (1.3 - 0.3*PREF_NH4N_OPA) * OPA_O2_TO_C
```

The factor (1.3 - 0.3*PREF_NH4) accounts for higher O2 production when using NO3 vs NH4 (EFDC formulation).

### 7.2 Sinks (Respiration and Oxidation)

```
DO consumed by phytoplankton resp  = R_*_TOT_RESP * *_O2_TO_C
DO consumed by zooplankton resp    = R_ZOO_TOT_RESP * ZOO_O2_TO_C
DO consumed by nitrification       = R_NITR * 4.57     (mg-O2 per mg-N)
DO consumed by aerobic DOC mineral = 2.66 * R_DOC_MIN_DOXY
DO consumed by Fe(II) oxidation    = 0.43 * R_FE_II_OXIDATION
DO consumed by Mn(II) oxidation    = 0.88 * R_MN_II_OXIDATION
DO consumed by sulphide oxidation  = 2.00 * R_SULPHIDE_OXIDATION
DO consumed by methane oxidation   = 5.33 * R_METHANE_OXIDATION
```

### 7.3 Aeration

```
R_AERATION = K_A * (DO_SAT - [O2]) * (1 - ice_cover)    (surface box only)
```

K_A is calculated from wind speed and water depth via the KAWIND function.

### 7.4 DO Saturation

File: `aquabc_II_pelagic_lib_DO_SATURATION.f90`

**Temperature effect:**
```
ln(CS_fresh) = -139.344 + 157570.1/T_K - 66423080/T_K^2 + 12438000000/T_K^3 - 862194900000/T_K^4
```

**Salinity correction (salting-out):**
```
ln(CS_saline) = ln(CS_fresh) - S * [0.017674 - 10.754/T_K + 2140.7/T_K^2]
```

**Altitude/pressure correction:**
```
THETA = 0.000975 - 0.00001426*T + 0.00000006436*T^2
P = (760 - 0.02667*H_feet) / 760
ln(PWV) = 11.8571 - 3840.7/T_K - 216961/T_K^2
CS = CS_saline * P * [(1-PWV/P)*(1-THETA*P)] / [(1-PWV)*(1-THETA)]
```

---

## 8. Nutrient Cycling

### 8.1 Nitrogen Cycle

**NH4_N derivative:**
```
d[NH4_N]/dt = + R_DON_MIN (mineralization from all 6 pathways)
              + R_ZOO_EX_DON (zooplankton excretion)
              + R_PHYT_RESP * N_TO_C (phytoplankton respiration)
              - R_PHYT_UPTAKE_NH4 (phytoplankton uptake)
              - R_NITR (nitrification)
              +/- sediment fluxes
```

**NO3_N derivative:**
```
d[NO3_N]/dt = + R_NITR (nitrification)
              - R_DENITRIFICATION
              - R_PHYT_UPTAKE_NO3 (phytoplankton uptake)
```

**Nitrification:**
```
R_NITR = K_NITR_20 * theta^(T-20) * ([NH4]/([NH4]+KHS)) * ([O2]/([O2]+KHS_O2)) * [NH4]
```

### 8.2 Phosphorus Cycle

**PO4_P derivative:**
```
d[PO4_P]/dt = + R_DOP_MIN (mineralization)
              + R_PHYT_RESP * P_TO_C (respiration)
              - R_PHYT_UPTAKE_PO4 (phytoplankton uptake)
              +/- sediment fluxes
```

PO4 availability is also modulated by the IP_SOLUBLE_FRACTION (see Section 11).

### 8.3 Silicon Cycle

```
d[DISS_Si]/dt = + R_PART_Si_DISS (biogenic Si dissolution)
                + R_DIA_RESP * DIA_SI_TO_C
                + R_DIA_EXCR * DIA_SI_TO_C
                - R_DIA_GROWTH * DIA_SI_TO_C (diatom uptake)
```

### 8.4 Carbon Cycle

**DIC dynamics** are handled through CO2SYS (Section 12). Organic carbon flows through:
```
Photosynthesis (DIC -> PHY_C) -> Death (PHY_C -> DET_POC) -> Dissolution (POC -> DOC)
-> Mineralization (DOC -> DIC)
```

---

## 9. Redox Chemistry and Metal Speciation

File: `aquabc_II_pelagic_lib_REDOX_AND_SPECIATION.f90`

### 9.1 Electron Acceptor Hierarchy

Limitation factors follow strict thermodynamic ordering with mutual inhibition:

```
LIM_DOXY    = [O2] / ([O2] + K_HS_O2)

LIM_NO3N    = [NO3] / ([NO3] + K_HS_NO3)
            * K_HS_O2_INHB / ([O2] + K_HS_O2_INHB)

LIM_MN_IV   = [Mn4+] / ([Mn4+] + K_HS_MN)
            * K_HS_O2_INHB / ([O2] + K_HS_O2_INHB)
            * K_HS_NO3_INHB / ([NO3] + K_HS_NO3_INHB)

LIM_FE_III  = [Fe3+] / ([Fe3+] + K_HS_FE)
            * K_HS_O2_INHB / ([O2] + K_HS_O2_INHB)
            * K_HS_NO3_INHB / ([NO3] + K_HS_NO3_INHB)
            * K_HS_MN_INHB / ([Mn4+] + K_HS_MN_INHB)

LIM_S_PLUS_6 = [SO4] / ([SO4] + K_HS_SO4)
             * K_HS_O2_INHB / ([O2] + K_HS_O2_INHB)
             * K_HS_NO3_INHB / ([NO3] + K_HS_NO3_INHB)
             * K_HS_MN_INHB / ([Mn4+] + K_HS_MN_INHB)
             * K_HS_FE_INHB / ([Fe3+] + K_HS_FE_INHB)

LIM_DOC     = max(0, 1 - sum(all above LIM factors))    (residual for methanogenesis)
```

### 9.2 Redox Potential (pE) Calculation

Based on the dominant electron acceptor:

```
O2 dominant:       pE = 20.75 - log10(1 / (sqrt(0.21*(O2/CS)) * H+))
NO3 dominant:      pE = 21.05 - log10(1 / ((NO3/14000) * H+^1.2))
Mn(IV) dominant:   pE = 20.8  - log10((Mn2+^0.5) / (Mn4+^0.5 * H+^2))
Fe(III) dominant:  pE = 13.0  - log10(Fe2+ / Fe3+)
SO4 dominant:      pE = 4.25  - log10((HS-^0.125) / (SO4^0.125 * H+^1.125))
DOC dominant:      pE = -0.2  - log10(1 / ((DOC/12000)^0.25 * H+))
```

### 9.3 Fe(II) Speciation

Four competing solid phases determine dissolved Fe(II):

```
FeCO3:   SI = 10^(-0.2 + pH + log10([HCO3-]))
Fe(OH)2: SI = 10^(-11.7 + 2*pH)
FeS:     SI = 10^(38 - 8*pH + log10([SO4]/S_molar) - 8*pE)
FeS2:    SI = 10^(86.8 - 16*pH + 2*log10([SO4]/S_molar) - 14*pE)
```

Most stable phase determined by maxloc; solubility products give free Fe2+:
```
FeCO3:   [Fe2+] = 10^(-0.3 - pH + log10([HCO3-]))
Fe(OH)2: [Fe2+] = 10^(13.3 - 2*pH)
FeS:     [Fe2+] = 10^(-18.64) / [S2-]      (if [S2-] > 1e-12)
FeS2:    [Fe2+] = 10^(-26.89) / (4*[S2-]^2) (if [S2-] > 1e-12)
```

### 9.4 Fe(III) Speciation

Four stepwise hydrolysis constants (Stumm and Morgan, 1996):

```
K1 = 10^(-3.05):  Fe3+ + H2O   -> FeOH2+   + H+
K2 = 10^(-6.31):  Fe3+ + 2H2O  -> Fe(OH)2+ + 2H+
K3 = 10^(-13.8):  Fe3+ + 3H2O  -> Fe(OH)3  + 3H+
K4 = 10^(-22.7):  Fe3+ + 4H2O  -> Fe(OH)4- + 4H+
K22 = 10^(-2.91): 2Fe3+ + 2OH- -> Fe2(OH)2^4+
K43 = 10^(-5.77): 4Fe3+ + 3OH- -> Fe4(OH)3^7+

Fe(III)_free = 10^(3.96 - 3*pH)
Fe(III)_diss = Fe(III)_free * [1 + K1/H+ + K2/H+^2 + K3/H+^3 + K4/H+^4
               + 2*K22*Fe3+_free/H+^2 + 3*K43*Fe3+_free^2/H+^4]
```

### 9.5 Mn(II) Speciation

Three competing solid phases (analogous to Fe(II)):
```
MnCO3:   SI = 10^(-0.2 + pH + log10([HCO3-]))
Mn(OH)2: SI = 10^(-15.0 + 2*pH)
MnS:     SI = 10^(34 - 8*pH + log10([SO4]/S_molar) - 8*pE)
```

---

## 10. Iron Geochemistry

File: `aquabc_II_pelagic_lib_IRON_II.f90`

### 10.1 Fe(II) Dissolution (Stumm and Lee, 1960)

Calculates total dissolved Fe(II) at equilibrium with three solid phases:

**From Fe(OH)2:**
```
Fe_II_1 = (K1/Kw^2)*H+^2 + (K2/Kw)*H+ + (K3*Kw)/H+
```

**From FeCO3:**
```
Fe_II_2 = [(H+ + 2*K6) / (TOT_ALK * K6)] * (K4 + (K5*Kw)/H+)
```

**From FeS (when total sulphide present):**
```
Fe_II_3 = [K7/HS2_TOT] * [1 + H+/K10b + H+^2/(K10b*K10a)]
        + K7 * [OH-/K8 + 27*OH-^3/K9]
```

**Result:** `FE_II_TOT = min(Fe_II_1, Fe_II_2, Fe_II_3)`

Equilibrium constants (25 deg C):
```
K1  = 8.0e-16   K2  = 4.0e-10   K3  = 8.3e-6
K4  = 2.1e-11   K5  = 1.0e-5    K6  = 4.8e-11
K7  = 6.0e-18   K8  = 3.0e-12   K9  = 6.2e-8
K10a = 1.0e-7   K10b = 1.3e-13  Kw = 1.0e-14
```

### 10.2 Fe(II) Oxidation (Morgan and Lahav, 2007)

Accounts for three Fe(II) species with different oxidation rates:

```
Rate constants:
    k_Fe2+      = 6.0e-5   (1/day)
    k_FeOH+     = 1.7      (1/day)
    k_Fe(OH)2   = 4.3e5    (1/day)

Equilibrium constants:
    K1 = 10^4.50 = 31623
    K2 = 10^2.93 = 851
    K3 = 10^3.57 = 3715

R_FE_II_OX = [k_Fe2+ / (1 + K1*Kw/H+ + K1*K2*Kw^2/H+^2 + K1*K2*Kw^4/H+^3)
             + k_FeOH+ / (1 + H+/(K1*Kw) + K2*Kw/H+ + K2*K3*Kw^2/H+^2)
             + k_Fe(OH)2 / (1 + K3*Kw/H+ + H+/(K2*Kw) + H+^2/(K1*K2*Kw^2))]
             * Fe2+_tot * (O2/CS) * FE_MOLAR_MASS
```

---

## 11. Inorganic Phosphorus Solubility

File: `aquabc_II_pelagic_lib_IP_SOLUBLE_FRACTION.f90`

Based on Snoeyink and Jenkins (1980). Fe(III) co-precipitates phosphate as FePO4(s).

```
Ksp = 10^(-26.4)  for FePO4(s) <-> Fe3+ + PO4^3-

Fe(III) side-reaction coefficient:
    alpha_Fe = 1 + K1/H+ + K2/H+^2 + K3/H+^3 + K4/H+^4

Phosphate side-reaction coefficient:
    alpha_PO4 = 1 + H+^3/(Ka1*Ka2*Ka3) + H+^2/(Ka2*Ka3) + H+/Ka3

Dissolved PO4 at equilibrium:
    C_PO4_dissolved = (Ksp * alpha_Fe * alpha_PO4) / C_T_FE_III

DIP_over_IP = C_PO4_dissolved / C_T_PO4_total    (clamped to [0, 1])
```

---

## 12. CO2 System (Carbonate Chemistry)

File: `aquabc_II_co2sys.f90`

Fortran implementation of the CDIAC CO2SYS program.

### 12.1 Input/Output

Any two of the following can be specified as input:
1. Total Alkalinity (umol/kg)
2. DIC / Total CO2 (umol/kg)
3. pH
4. pCO2 (uatm)
5. fCO2 (uatm)
6. HCO3- (umol/kg)
7. CO3^2- (umol/kg)
8. CO2(aq) (umol/kg)

### 12.2 Equilibrium Constants

Supports 13 sets of K1/K2 constants (Roy 1993, Goyet & Poisson 1989, Lueker 2000, etc.) and 2 KSO4 constant sets (Dickson 1990, Khoo 1977).

### 12.3 Outputs

- Complete carbonate speciation: CO2(aq), HCO3-, CO3^2-
- Boric acid speciation: B(OH)4-
- Phosphate speciation: H3PO4, H2PO4-, HPO4^2-, PO4^3-
- Saturation indices: Omega_Calcite, Omega_Aragonite
- Revelle buffer factor
- All values at both input and output T/P conditions

---

## 13. Sediment Diagenesis Model

### 13.1 Structure

The sediment model has the same redox sequence as the pelagic model, applied to sediment porewater. It tracks 24 state variables per sediment layer.

### 13.2 Particulate Organic Matter Dissolution

```
Oxic conditions:
    R_DISS_POC = K_OXIC * theta^(T-20) * [POC]/([POC]+KHS) * [POC]

Anoxic conditions:
    R_DISS_POC = K_ANOXIC * theta^(T-20) * [POC]/([POC]+KHS) * [POC]
```

### 13.3 DOC Mineralization

Identical 6-pathway redox sequence as pelagic model with sediment-specific rate constants (SED_K_MIN_DOC_*_20).

### 13.4 Sediment-Specific pH Correction

Gaussian formulation (different from pelagic exponential):
```
PH_CORR = exp(-((pH - PH_OPT_MID)^2) / (2 * PH_SIGMA^2))

where PH_OPT_MID = (PH_MIN + PH_MAX) / 2
      PH_SIGMA   = (PH_MAX - PH_MIN) / 4
```

### 13.5 Sediment-Water Fluxes

24 fluxes exchanged at the sediment-water interface:
```
FLUXES(1) = NH4N,  FLUXES(2) = NO3N,  FLUXES(3) = PO4P
FLUXES(4) = DOXY,  FLUXES(5-8) = 0 (living organisms)
FLUXES(9) = DOC,   FLUXES(10) = DON,  FLUXES(11) = DOP
FLUXES(12-14) = DSi, PSi, DIC
FLUXES(15) = ALK,  FLUXES(16-24) = metals/S/CH4
```

Positive flux = release from sediment to water.

---

## 14. Macroalgae Model

File: `SOURCE_CODE/MACROALGAE/mod_MACROALGAE.f90`

### 14.1 Droop Quota Nutrient Limitation

```
Q_N = [MAC_N] / [MAC_C]                    (internal N quota, g-N/g-C)
Q_P = [MAC_P] / [MAC_C]                    (internal P quota, g-P/g-C)

PHI_N = max(0, 1 - Q0_N/Q_N)              (N limitation, Droop model)
PHI_P = max(0, 1 - Q0_P/Q_P)              (P limitation, Droop model)
PHI_NB = min(PHI_N, PHI_P)                 (combined nutrient limitation)
```

### 14.2 Light Limitation (3 Options)

**Baly (1935):**
```
PHI_LB = I_bottom / (K_LB + I_bottom)
where I_bottom = I_A * exp(-K_E * H)
```

**Smith (1936):**
```
PHI_LB = I_bottom / sqrt(K_LB^2 + I_bottom^2)
```

**Steele (1962):**
```
PHI_LB = (I_bottom/K_LB) * exp(1 + I_bottom/K_LB)
```

### 14.3 Space Limitation (Logistic)

```
PHI_SB = 1 - [MAC_C] / MAC_C_MAX
```

### 14.4 Growth, Respiration, Excretion, Death

```
R_GROWTH = k_G_20 * theta^(T-20) * PHI_NB * PHI_LB * PHI_SB * [MAC_C]
R_RESP   = k_R_20 * theta^(T-20) * [MAC_C]
R_EXCR   = k_E_20 * theta^(T-20) * [MAC_C]
R_DEATH  = k_D_20 * theta^(T-20) * [MAC_C]

d[MAC_C]/dt = R_GROWTH - R_RESP - R_EXCR - R_DEATH
```

### 14.5 Nutrient Uptake (Monod)

```
N_UPTAKE = sigma_N * [DIN] / (KHS_DIN + [DIN])
P_UPTAKE = sigma_P * [SRP] / (KHS_SRP + [SRP])
```

### 14.6 Ammonium Preference

```
PREF_NH4 = ([NH4]*[NO3]) / ((K+[NH4])*(K+[NO3])) + (K*[NH4]) / ([DIN]*(K+[NO3]))
```

### 14.7 Deattachment

```
POC_to_WC = k_DEATT * [MAC_C] / H
PON_to_WC = k_DEATT * Q_N * [MAC_C] / H
POP_to_WC = k_DEATT * Q_P * [MAC_C] / H
```

### 14.8 Attached Detritus

```
d[ATT_DETPOC]/dt = R_DEATH - k_DISS * theta^(T-20) * [ATT_DETPOC]
```

---

## 15. Allelopathy Module

Files: `SOURCE_CODE/ALLELOPATHY/`

### 15.1 Overview

Four organism groups (diatoms, non-fixing cyanobacteria, fixing cyanobacteria, Nostocales) produce secondary metabolites upon death that inhibit other organisms.

### 15.2 Secondary Metabolite Formation

```
R_FORM_SEC_METAB_i = S_i * R_DEATH_i

where S_i = stoichiometric yield (metabolite per unit death)
```

### 15.3 Secondary Metabolite Degradation

```
R_DEG_SEC_METAB_i = k_DEG_20_i * theta_i^(T-20) * [SEC_METAB_i]
```

### 15.4 Net Metabolite Dynamics

```
d[SEC_METAB_i]/dt = R_FORM_i - R_DEG_i
```

### 15.5 Inhibition (Monod-type)

For each inhibitor-target pair:
```
IHBF_i_j = K_HS_i_j / (K_HS_i_j + [SEC_METAB_i])
```

Where IHBF = 1 means no inhibition, IHBF -> 0 means complete inhibition.

### 15.6 Aggregated Inhibition

For each target organism, worst-case (minimum) across all inhibitors:
```
IHBF_DIA = min(IHBF_CYN_DIA, IHBF_FIX_CYN_DIA, IHBF_NOST_DIA)
```

Applied multiplicatively to mortality/growth rates.

---

## 16. Auxiliary Functions

### 16.1 pH Correction (Pelagic)

File: `aquabc_II_pelagic_lib_PH_CORR.f90`

AQUATOX formulation (EPA-829-R-14-007):
```
if pH < PH_MIN:  PH_CORR = exp(pH - PH_MIN)
if PH_MIN <= pH <= PH_MAX:  PH_CORR = 1.0
if pH > PH_MAX:  PH_CORR = exp(PH_MAX - pH)
```

### 16.2 Chlorophyll-a Diagnostic

File: `aquabc_II_pelagic_lib_CHLA.f90`

```
CHLA = ([DIA_C]/DIA_C_TO_CHLA + [CYN_C]/CYN_C_TO_CHLA + [OPA_C]/OPA_C_TO_CHLA) * 1000

if FIX_CYN enabled:  CHLA += ([FIX_CYN_C]/FIX_CYN_C_TO_CHLA) * 1000
if NOST enabled:      CHLA += ([NOST_C]/NOST_C_TO_CHLA) * 1000
```

Units: ug/L

### 16.3 Settling Velocity Suppression

File: `aquabc_II_pelagic_lib_SETTLING.f90`

```
v_settle = -0.0061 * Chla + 1.0383    (m/day)

if Chla < 44 ug/L:         factor = 1.0
if 44 <= Chla <= 140 ug/L: factor = v_settle(Chla) / v_settle(44)
if Chla > 140 ug/L:        factor = v_settle(140) / v_settle(44)
```

### 16.4 Light Extinction Coefficient

```
K_E = K_B_E + sum(CHLA_contributions)
```

where K_B_E is the background extinction coefficient.

---

## 17. Numerical Solver

File: `SOURCE_CODE/ESTAS/mod_SOLVER.f90`

### 17.1 Available Solvers

**Solver 1: Forward Euler** (first-order explicit)
```
STATE(t+dt) = STATE(t) + DERIV_TOTAL * dt
```

**Solver 2: Heun's Method / RK2** (second-order explicit)
```
k1 = f(t, STATE(t))
k2 = f(t+dt, STATE(t) + k1*dt)
STATE(t+dt) = STATE(t) + (k1 + k2)/2 * dt
```

### 17.2 Derivative Components (7 sources)

```
DERIV_TOTAL = ADVECTION + DISPERSION + SETTLING + MASS_LOADS
            + MASS_WITHDRAWALS + KINETICS + SEDIMENT_FLUXES
```

### 17.3 Mass Conservation Safeguards

- Concentrations floored to MIN_CONCENTRATION (1e-10) if negative
- Error stop if concentration > 1e10 (with full diagnostic dump)
- OpenMP parallelization across spatial nodes

---

## 18. Physical Constants and Safety Functions

File: `aquabc_physical_constants.f90`

### 18.1 Fundamental Constants

```
CELSIUS_TO_KELVIN = 273.15
FE_MOLAR_MASS_MG = 56000.0    (mg/mol)
MN_MOLAR_MASS_MG = 54938.0    (mg/mol)
S_MOLAR_MASS_MG  = 32000.0    (mg/mol)
K_W_25C = 1.0e-14
```

### 18.2 Safety Parameters

```
MIN_CONCENTRATION = 1.0e-10    (minimum allowable concentration)
SAFE_EXP_MIN = -700.0          (minimum safe exponent)
SAFE_EXP_MAX = 700.0           (maximum safe exponent)
STRANGER_THRESHOLD = 1.0e30    (NaN/Inf detection threshold)
```

### 18.3 Safe Exponential Function

```fortran
elemental function safe_exp(x)
    x_clamped = max(SAFE_EXP_MIN, min(SAFE_EXP_MAX, x))
    safe_exp = exp(x_clamped)
end function
```

### 18.4 pH Clamping

All pH-dependent calculations clamp pH to [4, 11]:
```
H_PLUS = 10^(-max(4.0, min(11.0, pH)))
```

---

## 19. Model Constants Summary

Total: 318 pelagic model constants defined in `aquabc_II_pelagic_model_constants.f90`

### 19.1 Aeration
- K_A = -1.0 (internal calculation), THETA_K_A = 1.04

### 19.2 Phytoplankton (per group)

| Parameter | Diatoms | Cyanobacteria | Fix. Cyan. | OPA | Nostocales |
|-----------|---------|---------------|-----------|-----|-----------|
| KG_OPT_TEMP (1/day) | 3.7 | 2.4 | 3.5 | variable | variable |
| KR_20 (1/day) | 0.05 | variable | variable | variable | variable |
| KD_20 (1/day) | 0.12 | variable | variable | variable | variable |
| N_TO_C | 0.22 | variable | variable | variable | variable |
| P_TO_C | 0.024 | variable | variable | variable | variable |

### 19.3 Zooplankton

| Parameter | Value |
|-----------|-------|
| KG_ZOO_OPT_TEMP | 0.45 1/day |
| PREF_ZOO_DIA | 0.26 |
| PREF_ZOO_CYN | 0.10 |
| PREF_ZOO_FIX_CYN | 0.07 |
| PREF_ZOO_OPA | 0.37 |
| PREF_ZOO_DET | 0.20 |
| FOOD_MIN_ZOO | 0.02 mg/L |
| KR_ZOO_20 | 0.03 1/day |
| KD_ZOO_20 | 0.15 1/day |
| DO_STR_HYPOX_ZOO_D | 2.0 mg/L |

### 19.4 Organic Matter Dissolution

| Parameter | Value |
|-----------|-------|
| KDISS_DET_PART_ORG_C_20 | 0.1 1/day |
| KDISS_DET_PART_ORG_N_20 | 0.25 1/day |
| KDISS_DET_PART_ORG_P_20 | 0.48 1/day |
| KDISS_PART_Si_20 | 0.001 1/day |
| THETA_KDISS | 1.06 |

### 19.5 DOC Mineralization

For each of 6 electron acceptor pathways: K_MIN_20, THETA, KHS (substrate half-saturation), KHS_RED_LIM (acceptor half-saturation), KHS_RED_INHB (inhibition half-saturation), PH_MIN, PH_MAX.

---

## Key Assumptions and Limitations

1. **Temperature response**: Q10/Arrhenius-type exponential (theta^(T-20))
2. **Nutrient limitation**: Monod half-saturation kinetics with Liebig's Law of the Minimum
3. **Perfect mixing**: Complete mixing within each computational box
4. **Quasi-equilibrium speciation**: Fe/Mn solubility at instantaneous chemical equilibrium
5. **Hierarchical redox**: Strict thermodynamic priority for electron acceptors
6. **pH range**: Clamped to [4, 11] to prevent numerical instability
7. **Hardcoded equilibrium constants**: Most at 25 deg C (temperature-dependent Kw for Fe(III) only)
8. **Linear settling**: Settling velocity suppression linear with Chla in 44-140 ug/L range
9. **Mass balance safeguards**: 50% maximum loss per timestep for zooplankton and phytoplankton

---

## References

- Stumm, W. and Morgan, J.J. (1996). Aquatic Chemistry. Wiley.
- Stumm, W. and Lee, G.F. (1960). Oxygenation of ferrous iron. Industrial & Engineering Chemistry.
- Morgan, B. and Lahav, O. (2007). The effect of pH on the kinetics of spontaneous Fe(II) oxidation. Chemosphere.
- Snoeyink, V.L. and Jenkins, D. (1980). Water Chemistry. Wiley.
- Steele, J.H. (1962). Environmental control of photosynthesis in the sea. Limnology and Oceanography.
- Smith, E.L. (1936). Photosynthesis in relation to light and carbon dioxide. PNAS.
- Baly, E.C.C. (1935). The kinetics of photosynthesis. Proceedings of the Royal Society B.
- Nagy, S.A. et al. (2006). Mixing model for buoyant cyanobacteria.
- Droop, M.R. (1968). Vitamin B12 and marine ecology. Journal of the Marine Biological Association.
- EPA-829-R-14-007. AQUATOX Technical Documentation.
- CDIAC CO2SYS program documentation.
