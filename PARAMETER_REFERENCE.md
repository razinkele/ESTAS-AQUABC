# AQUABC Model Parameters Quick Reference

## Parameter File Structure

**Main Parameter File:** `INPUTS/WCONST_04.txt`
**Format:** `ID  NAME  VALUE  !COMMENT`
**Total Parameters:** 324
**This file's tables are generated:** `python tools/build_parameter_reference.py`

---

## Parameter Categories

<!-- BEGIN GENERATED CONSTANTS (tools/build_parameter_reference.py) -->

All 324 constants of `INPUTS/WCONST_04.txt` (the Standard setup). Values are the Standard ones; other setups (e.g. CL29) carry their own file. Descriptions are the file's own comments.

### 1. General (1-4) - 4 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 1 | `K_A` | -1.0 | Aeration coefficient (if negative calculates internally) |
| 2 | `THETA_K_A` | 1.04 | Temperature correction factor for aeration |
| 3 | `XKC` | 0.08 | fixme. Light extinction per chlorophyl unit,( mcg Chla/l/m) for calculation of saturation for Smith. Should be different for different phyto? |
| 4 | `PHIMX` | 720.00 | Quantum yield const. mg C/mole photon |

### 2. Diatoms (5-28) - 24 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 5 | `KG_DIA_OPT_TEMP` | 3.7 | 3.3 3.1  Diatoms Growth rate |
| 6 | `DIA_OPT_TEMP_LR` | 1.0 | Diatoms optimal temperature lower range |
| 7 | `DIA_OPT_TEMP_UR` | 24.0 | Diatoms optimal temperature upper range |
| 8 | `EFF_DIA_GROWTH` | 0.95 | Diatoms Effective growth. (1-EG)*growth - losses for respiration and excretion |
| 9 | `KAPPA_DIA_UNDER_OPT_TEMP` | 0.04 | Diatoms Temperature correction for growth lower temperature |
| 10 | `KAPPA_DIA_OVER_OPT_TEMP` | 0.07 | Diatoms Temperature correction for growth upper temperature |
| 11 | `KR_DIA_20` | 0.05 | Diatoms Respiration rate |
| 12 | `THETA_KR_DIA` | 1.04 | Diatoms Temperature correction for basal respiration rate |
| 13 | `KD_DIA_20` | 0.12 | Diatoms Mortality rate |
| 14 | `THETA_KD_DIA` | 1.02 | Diatoms Temperature correction for Mortality rate |
| 15 | `KHS_DIN_DIA` | 0.010 | 0.03  Diatoms Half saturation growth for DIN |
| 16 | `KHS_DIP_DIA` | 0.005 | Diatoms Half saturation growth for DIP |
| 17 | `KHS_DSi_DIA` | 0.013 | 0.15  Diatoms Half saturation growth for DSi |
| 18 | `KHS_O2_DIA` | 0.60 | Diatoms Half saturation growth for O2 |
| 19 | `FRAC_DIA_EXCR` | 0.30 | Diatoms Fraction of excretion in metabolism rate |
| 20 | `I_S_DIA` | 100.00 | Diatoms Light saturation (langleys) |
| 21 | `DO_STR_HYPOX_DIA_D` | 0.70 | Diatoms Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy |
| 22 | `THETA_HYPOX_DIA_D` | 1.20 | Diatoms Multiplier of the exponent for Dissolved oxygen stress |
| 23 | `EXPON_HYPOX_DIA_D` | 1.04 | Diatoms Exponent constant for Dissolved oxygen stress |
| 24 | `DIA_N_TO_C` | 0.22 | Diatoms Nitrogen to Carbon ratio |
| 25 | `DIA_P_TO_C` | 0.024 | Diatoms Phosphorus to Carbon ratio |
| 26 | `DIA_Si_TO_C` | 0.25 | Diatoms Silica to Carbon ratio |
| 27 | `DIA_O2_TO_C` | 2.66 | Diatoms Oxygen to Carbon ratio for respiration |
| 28 | `DIA_C_TO_CHLA` | 30.00 | Diatoms Carbon to Chlorophil a ratio |

### 3. Non-fixing Cyanobacteria (29-50) - 22 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 29 | `KG_CYN_OPT_TEMP` | 2.4 | 2.1 Non-fixing cyanobacteria Growth rate |
| 30 | `CYN_OPT_TEMP_LR` | 15.0 | Non-fixing cyanobacteria optimal temperature lower range |
| 31 | `CYN_OPT_TEMP_UR` | 26.0 | Non-fixing cyanobacteria optimal temperature upper range |
| 32 | `EFF_CYN_GROWTH` | 0.95 | Non-fixing cyanobacteria Effective growth. (1-EG)*growth - losses for respiration and excretion |
| 33 | `KAPPA_CYN_UNDER_OPT_TEMP` | 0.08 | Non-fixing cyanobacteria Temperature correction for growth lower temperature |
| 34 | `KAPPA_CYN_OVER_OPT_TEMP` | 0.05 | Non-fixing cyanobacteria Temperature correction for growth upper temperature |
| 35 | `KR_CYN_20` | 0.06 | Non-fixing cyanobacteria Respiration rate |
| 36 | `THETA_KR_CYN` | 1.04 | Non-fixing cyanobacteria Temperature correction for respiration rate |
| 37 | `KD_CYN_20` | 0.125 | Non-fixing cyanobacteria Mortality rate |
| 38 | `THETA_KD_CYN` | 1.05 | Non-fixing cyanobacteria Temperature correction for Mortality rate |
| 39 | `KHS_DIN_CYN` | 0.009 | Non-fixing cyanobacteria Half saturation growth for DIN |
| 40 | `KHS_DIP_CYN` | 0.008 | 0.007  Non-fixing cyanobacteria Half saturation growth for DIP |
| 41 | `KHS_O2_CYN` | 0.60 | Non-fixing cyanobacteria Half saturation growth for O2 |
| 42 | `FRAC_CYN_EXCR` | 0.30 | Non-fixing cyanobacteria Fraction of excretion in metabolism rate |
| 43 | `I_S_CYN` | 100.00 | Non-fixing cyanobacteria Light saturation (langleys).Not used, is calculated! |
| 44 | `DO_STR_HYPOX_CYN_D` | 0.70 | Non-fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy |
| 45 | `THETA_HYPOX_CYN_D` | 1.50 | Non-fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress |
| 46 | `EXPON_HYPOX_CYN_D` | 1.10 | Non-fixing cyanobacteria Exponent constant for Dissolved oxygen stress |
| 47 | `CYN_N_TO_C` | 0.220 | Non-fixing cyanobacteria Nitrogen to Carbon ratio ,was 0.1 |
| 48 | `CYN_P_TO_C` | 0.024 | Non-fixing cyanobacteria Phosphorus to Carbon ratio |
| 49 | `CYN_O2_TO_C` | 2.66 | Non-fixing cyanobacteria Oxygen to Carbon ratio for respiration |
| 50 | `CYN_C_TO_CHLA` | 40.00 | Non-fixing cyanobacteria Carbon to Chlorophyl a ratio |

### 4. Fixing Cyanobacteria (51-74) - 24 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 51 | `KG_FIX_CYN_OPT_TEMP` | 3.5 | Fixing cyanobacteria Growth rate constant |
| 52 | `FIX_CYN_OPT_TEMP_LR` | 18.0 | Fixing Cyanobacteria optimal temperature lower range |
| 53 | `FIX_CYN_OPT_TEMP_UR` | 26.0 | Fixing Cyanobacteria optimal temperature upper range |
| 54 | `EFF_FIX_CYN_GROWTH` | 0.95 | Fixing cyanobacteria Effective growth. (1-EG)*growth - losses for RESP and excretion |
| 55 | `KAPPA_FIX_CYN_UNDER_OPT_TEMP` | 0.075 | Fixing cyanobacteria Temperature correction for growth lower temperature |
| 56 | `KAPPA_FIX_CYN_OVER_OPT_TEMP` | 0.05 | Fixing cyanobacteria Temperature correction for growth upper temperature |
| 57 | `KR_FIX_CYN_20` | 0.06 | Fixing cyanobacteria RESP rate constant |
| 58 | `THETA_KR_FIX_CYN` | 1.04 | Fixing cyanobacteria Temperature correction for RESP rate |
| 59 | `KD_FIX_CYN_20` | 0.10 | Fixing cyanobacteria Mortality rate constant |
| 60 | `THETA_KD_FIX_CYN` | 1.05 | Fixing cyanobacteria Temperature correction for Mortality rate |
| 61 | `KHS_DIN_FIX_CYN` | 0.01 | Fixing cyanobacteria Half saturation growth for DIN |
| 62 | `KHS_DIP_FIX_CYN` | 0.005 | Fixing cyanobacteria Half saturation growth for DIP |
| 63 | `KHS_O2_FIX_CYN` | 0.60 | Fixing cyanobacteria Half saturation growth for O2 |
| 64 | `FRAC_FIX_CYN_EXCR` | 0.30 | Fixing cyanobacteria Fraction of excretion in metabolism rate |
| 65 | `I_S_FIX_CYN` | 100.00 | Fixing cyanobacteria Light saturation (langleys). Not used, is calculated! |
| 66 | `DO_STR_HYPOX_FIX_CYN_D` | 0.70 | Fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy |
| 67 | `THETA_HYPOX_FIX_CYN_D` | 1.50 | Fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress |
| 68 | `EXPON_HYPOX_FIX_CYN_D` | 1.10 | Fixing cyanobacteria Exponent constant for Dissolved oxygen stress |
| 69 | `FIX_CYN_N_TO_C` | 0.220 | Fixing cyanobacteria Nitrogen to Carbon ratio |
| 70 | `FIX_CYN_P_TO_C` | 0.024 | Fixing cyanobacteria Phosphorus to Carbon ratio |
| 71 | `FIX_CYN_O2_TO_C` | 2.66 | Fixing cyanobacteria Oxygen to Carbon ratio for respiration |
| 72 | `FIX_CYN_C_TO_CHLA` | 40.00 | Fixing cyanobacteria Carbon to Chlorophyl a ratio |
| 73 | `R_FIX` | 1.0 | Fixing cyanobacteria Ratio between non-fixing and fixing fractions growth rate |
| 74 | `K_FIX` | 0.008 | Fixing cyanobacteria Effectivity parameter of switching to nitrogen fixation |

### 5. Other Phytoplankton (75-96) - 22 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 75 | `KG_OPA_OPT_TEMP` | 2.9 | 2.7  OtherPhyto Growth rate constant |
| 76 | `OPA_OPT_TEMP_LR` | 9.0 | OtherPhyto optimal temperature lower range |
| 77 | `OPA_OPT_TEMP_UR` | 20.0 | OtherPhyto optimal temperature upper range |
| 78 | `EFF_OPA_GROWTH` | 0.95 | OtherPhyto Effective growth. (1-EG)*growth - losses for respiration and excretion |
| 79 | `KAPPA_OPA_UNDER_OPT_TEMP` | 0.05 | OtherPhyto Temperature correction for growth lower temperature |
| 80 | `KAPPA_OPA_OVER_OPT_TEMP` | 0.10 | OtherPhyto Temperature correction for growth upper temperature |
| 81 | `KR_OPA_20` | 0.06 | OtherPhyto Respiration rate constant |
| 82 | `THETA_KR_OPA` | 1.02 | OtherPhyto Temperature correction for respiration rate |
| 83 | `KD_OPA_20` | 0.11 | OtherPhyto Mortality rate constant |
| 84 | `THETA_KD_OPA` | 1.02 | OtherPhyto Temperature correction for Mortality rate |
| 85 | `KHS_DIN_OPA` | 0.015 | 0.025  OtherPhyto Half saturation growth for DIN |
| 86 | `KHS_DIP_OPA` | 0.013 | 0.005  OtherPhyto Half saturation growth for DIP |
| 87 | `KHS_O2_OPA` | 0.60 | OtherPhyto Half saturation growth for O2 |
| 88 | `FRAC_OPA_EXCR` | 0.30 | OtherPhyto Fraction of excretion in metabolism rate |
| 89 | `I_S_OPA` | 100.00 | OtherPhyto Light saturation (langleys). Not used, is calculated! |
| 90 | `DO_STR_HYPOX_OPA_D` | 0.70 | OtherPhyto Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy |
| 91 | `THETA_HYPOX_OPA_D` | 1.20 | OtherPhyto Multiplier of the exponent for Dissolved oxygen stress |
| 92 | `EXPON_HYPOX_OPA_D` | 1.04 | OtherPhyto Exponent constant for Dissolved oxygen stress |
| 93 | `OPA_N_TO_C` | 0.220 | OtherPhyto Nitrogen to Carbon ratio |
| 94 | `OPA_P_TO_C` | 0.024 | OtherPhyto Phosphorus to Carbon ratio |
| 95 | `OPA_O2_TO_C` | 2.66 | OtherPhyto Oxygen to Carbon ratio for respiration |
| 96 | `OPA_C_TO_CHLA` | 30.00 | OtherPhyto Carbon to Chlorophyl a ratio |

### 6. Zooplankton (97-133) - 37 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 97 | `KG_ZOO_OPT_TEMP` | 0.45 | Zooplankton Growth rate |
| 98 | `ZOO_OPT_TEMP_LR` | 10.0 | Zooplankton optimal temperature lower range |
| 99 | `ZOO_OPT_TEMP_UR` | 25.0 | Zooplankton optimal temperature upper range |
| 100 | `EFF_ZOO_GROWTH` | 0.80 | Zooplankton Effective growth. (1-EG)*growth - losses for respiration and excretion |
| 101 | `KAPPA_ZOO_UNDER_OPT_TEMP` | 0.04 | Zooplankton Temperature correction for growth lower temperature |
| 102 | `KAPPA_ZOO_OVER_OPT_TEMP` | 0.01 | Zooplankton Temperature correction for growth upper temperature |
| 103 | `GRAT_ZOO_DIA` | 1.00 | Zooplankton Grazing rate (growhth rate multiplier) on diatoms |
| 104 | `GRAT_ZOO_CYN` | 1.00 | Zooplankton Grazing rate (growhth rate multiplier) on Cyanobacteria |
| 105 | `GRAT_ZOO_OPA` | 1.00 | Zooplankton Grazing rate (growhth rate multiplier) on OtherPhyto |
| 106 | `GRAT_ZOO_FIX_CYN` | 1.00 | Zooplankton Grazing rate (growhth rate multiplier) on fixing Cyanobacteria |
| 107 | `GRAT_ZOO_NOST_VEG_HET` | 1.00 | Zooplankton Grazing rate (growhth rate multiplier) on Nostocles (veg + het) |
| 108 | `GRAT_ZOO_DET_PART_ORG_C` | 0.50 | Zooplankton Grazing rate (growhth rate multiplier) on part. ORG_C |
| 109 | `PREF_ZOO_DIA` | 0.26 | Zooplankton Preference for Diatoms |
| 110 | `PREF_ZOO_CYN` | 0.10 | Zooplankton Preference for Cyanobacteria |
| 111 | `PREF_ZOO_FIX_CYN` | 0.07 | Zooplankton Preference for fixing Cyanobacteria |
| 112 | `PREF_ZOO_NOST_VEG_HET` | 0.00 | Zooplankton Preference for nostocales (veg + het) |
| 113 | `PREF_ZOO_OPA` | 0.37 | Zooplankton Preference for OtherPhyto |
| 114 | `PREF_ZOO_DET_PART_ORG_C` | 0.20 | Zooplankton Preference for Part. ORG_C |
| 115 | `KHS_DIA_C_ZOO` | 0.10 | Zooplankton Half saturation growth for diatoms |
| 116 | `KHS_CYN_C_ZOO` | 0.07 | Zooplankton Half saturation growth for Cyanobacteria |
| 117 | `KHS_FIX_CYN_C_ZOO` | 0.07 | Zooplankton Half saturation growth for fixing Cyanobacteria |
| 118 | `KHS_NOST_VEG_HET_C_ZOO` | 0.07 | Zooplankton Half saturation growth for Nostocales (veg + het) |
| 119 | `KHS_OPA_C_ZOO` | 0.15 | Zooplankton Half saturation growth for OtherPhyto |
| 120 | `KHS_DET_PART_ORG_C_ZOO` | 0.50 | Zooplankton Half saturation growth for part. ORG_C |
| 121 | `FOOD_MIN_ZOO` | 0.02 | Zooplankton Minimum food conc. for feeding |
| 122 | `KE_ZOO` | 0.05 | not used Zooplankton Excretion rate as growth fraction |
| 123 | `FRAC_ZOO_EX_ORG` | 0.30 | not used Zooplankton Excretion rate organic fraction |
| 124 | `KR_ZOO_20` | 0.03 | Zooplankton Respiration rate |
| 125 | `THETA_KR_ZOO` | 1.04 | Zooplankton Respiration rate Temperature correction |
| 126 | `KD_ZOO_20` | 0.15 | Zooplankton Mortality rate |
| 127 | `THETA_KD_ZOO` | 1.04 | Zooplankton Mortality rate Temperature correction |
| 128 | `DO_STR_HYPOX_ZOO_D` | 2.00 | Zooplankton Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy |
| 129 | `THETA_HYPOX_ZOO_D` | 1.20 | Zooplankton Multiplier of the exponent for Dissolved oxygen stress |
| 130 | `EXPON_HYPOX_ZOO_D` | 1.06 | Zooplankton Exponent constant for Dissolved oxygen stress |
| 131 | `ZOO_N_TO_C` | 0.220 | Zooplankton Nitrogen to Carbon ratio |
| 132 | `ZOO_P_TO_C` | 0.024 | Zooplankton Phosphorus to Carbon ratio |
| 133 | `ZOO_O2_TO_C` | 2.66 | Zooplankton Oxygen to Carbon ratio for respiration |

### 7. Detritus (134-146) - 13 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 134 | `KDISS_DET_PART_ORG_C_20` | 0.25 | POC Dissolution rate not dependent on phytoplankton |
| 135 | `THETA_KDISS_DET_PART_ORG_C` | 1.06 | POC Carbon Dissolution rate Temperature correction |
| 136 | `FAC_PHYT_DET_PART_ORG_C` | 0. | !0.001  POC Carbon Phytoplankton linear factor for dissolution rate |
| 137 | `KDISS_DET_PART_ORG_N_20` | 0.25 | 0.12   PON Dissolution rate not dependent on phytoplankton |
| 138 | `THETA_KDISS_DET_PART_ORG_N` | 1.06 | PON Dissolution rate Temperature correction |
| 139 | `KHS_DISS_N` | 0.025 | 0.025  PON dissolution reverse half saturation for DIN |
| 140 | `FAC_PHYT_DET_PART_ORG_N` | 0. | !0.007 PON Phytoplankton linear factor for dissolution rate |
| 141 | `KDISS_DET_PART_ORG_P_20` | 3.48 | POP Phosphorus Dissolution rate not dependent on phytoplankton |
| 142 | `THETA_KDISS_DET_PART_ORG_P` | 1.06 | POP Dissolution rate Temperature correction |
| 143 | `KHS_DISS_P` | 0.03 | 0.03 POP Dissolution reverse half saturation for DIP |
| 144 | `FAC_PHYT_DET_PART_ORG_P` | 0. | !0.005  POP Phytoplankton linear factor for dissolution rate |
| 145 | `KDISS_PART_Si_20` | 0.001 | Particulate Silica Dissolution rate |
| 146 | `THETA_KDISS_PART_Si` | 1.04 | Particulate Silica Dissolution rate Temperature correction |

### 8. Dissolved Organics (147-151) - 5 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 147 | `FAC_PHYT_AMIN_DOC` | 0.0045 | 0.0075 0.045   DOC Phytoplankton linear factor for mineralisation rate |
| 148 | `KHS_AMIN_N` | 100. | 0.05  DON mineralisation reverse half saturation for DIN |
| 149 | `FAC_PHYT_AMIN_DON` | 0.008 | 0.01  DON Phytoplankton linear factor for mineralisation rate |
| 150 | `KHS_AMIN_P` | 0.025 | 0.025   DOP reverse half saturation for DIP |
| 151 | `FAC_PHYT_AMIN_DOP` | 0.90 | DOP Phytoplankton linear factor for mineralisation rate |

### 9. Nitrification (152-157) - 6 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 152 | `K_NITR_20` | 0.600 | Amonia nitrification rate |
| 153 | `THETA_K_NITR` | 1.045 | Amonia nitrification rate Temperature constant |
| 154 | `KHS_NITR_OXY` | 2.000 | Amonia nitrification half saturation for Oxygen |
| 155 | `KHS_NITR_NH4_N` | 0.030 | Amonia nitrification half saturation for Amonia |
| 156 | `PH_NITR_NH4_MIN` | 6.90 | optimum lower range for pH correction factor for nitrification |
| 157 | `PH_NITR_NH4_MAX` | 8.20 | optimum upper range for pH correction factor for nitrification |

### 10. Iron and Manganese Redox (158-163) - 6 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 158 | `k_OX_FE_II` | 0.00125 | Oxidation rate for iron 2+ |
| 159 | `k_RED_FE_III` | 2.00 | reduction rate for iron 3+ |
| 160 | `k_OX_MN_II` | 0.01 | oxidation rate for manganese 2+ |
| 161 | `k_RED_MN_IV` | 2.00 | reduction rate for manganese 4+ |
| 162 | `KHS_DOXY_FE_III_RED` | 0.20 | reversed Monod half saturation of DOXY for iron 3+ reduction |
| 163 | `KHS_DOXY_MN_IV_RED` | 0.20 | reversed Monod half saturation of DOXY for manganese 4+ reduction |

### 11. DOC Mineralisation (164-203) - 40 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 164 | `K_MIN_DOC_DOXY_20` | 0.010 | 0.025  Mineralization rate constant of DOC at 20 C for dissolved oxygen as final electron acceptor |
| 165 | `K_MIN_DOC_NO3N_20` | 0.025 | Mineralization rate constant of DOC at 20 C for nitrate as final electron acceptor |
| 166 | `K_MIN_DOC_MN_IV_20` | 0.025 | Mineralization rate constant of DOC at 20 C for Mn IV as final electron acceptor |
| 167 | `K_MIN_DOC_FE_III_20` | 0.025 | Mineralization rate constant of DOC at 20 C for Fe III as final electron acceptor |
| 168 | `K_MIN_DOC_S_PLUS_6_20` | 0.025 | Mineralization rate constant of DOC at 20 C for sulphate as final electron acceptor |
| 169 | `K_MIN_DOC_DOC_20` | 0.025 | Mineralization rate constant of DOC at 20 C for DOC itself as final electron acceptor |
| 170 | `THETA_K_MIN_DOC_DOXY` | 1.04 | Temperature correction factor for mineralization process rate constant of DOC for dissolved oxygen as final electron acceptor |
| 171 | `THETA_K_MIN_DOC_NO3N` | 1.04 | Temperature correction factor for mineralization process rate constant of DOC for nitrate as final electron acceptor |
| 172 | `THETA_K_MIN_DOC_MN_IV` | 1.04 | Temperature correction factor for mineralization process rate constant of DOC for MN IV as final electron acceptor |
| 173 | `THETA_K_MIN_DOC_FE_III` | 1.04 | Temperature correction factor for mineralization process rate constant of DOC for FE III as final electron acceptor |
| 174 | `THETA_K_MIN_DOC_S_PLUS_6` | 1.04 | Temperature correction factor for mineralization process rate constant of DOC for Suphate as final electron acceptor |
| 175 | `THETA_K_MIN_DOC_DOC` | 1.04 | Temperature correction factor for mineralization process rate constant of DOC for DOC |
| 176 | `K_HS_DOC_MIN_DOXY` | 0.00 | Monod type half-saturation concentration of DOC for DOC mineralization for dissolved oxygen as the final electron acceptor |
| 177 | `K_HS_DOC_MIN_NO3N` | 1.00 | Monod type half-saturation concentration of DOC for DOC mineralization for nitrate as the final electron acceptor |
| 178 | `K_HS_DOC_MIN_MN_IV` | 1.00 | Monod type half-saturation concentration of DOC for DOC mineralization for Mn IV as the final electron acceptor |
| 179 | `K_HS_DOC_MIN_FE_III` | 1.00 | Monod type half-saturation concentration of DOC for DOC mineralization for Fe III as the final electron acceptor |
| 180 | `K_HS_DOC_MIN_S_PLUS_6` | 1.00 | Monod type half-saturation concentration of DOC for DOC mineralization for sulphate as the final electron acceptor |
| 181 | `K_HS_DOC_MIN_DOC` | 1.00 | Monod type half-saturation concentration of DOC for DOC mineralization for DOC itself as the final electron acceptor |
| 182 | `K_HS_DOXY_RED_LIM` | 1.00 | Monod type half-saturation concentration of dissolved oxygen to limit the consumption of dissolved oxygen as the final electron acce |
| 183 | `K_HS_NO3N_RED_LIM` | 1.00 | Monod type half-saturation concentration of nitrate nitrogen to limit the consumption of nitrate as the final electron acceptor for |
| 184 | `K_HS_MN_IV_RED_LIM` | 1.00 | Monod type half-saturation concentration of MN_IV to limit the consumption of Mn IV as the final electron acceptor for DOC mineraliz |
| 185 | `K_HS_FE_III_RED_LIM` | 1.00 | Monod type half-saturation concentration of FE_III to limit the consumption of Fe III as the final electron acceptor for DOC mineral |
| 186 | `K_HS_S_PLUS_6_RED_LIM` | 1.00 | Monod type half-saturation concentration of sulphate to limit the consumption of sulphate as the final electron acceptor for DOC min |
| 187 | `K_HS_DOXY_RED_INHB` | 0.10 | Reversed Monod type half saturation concentration that simulate the inhibition effect of dissolved oxygen on consumption of NO3, Mn |
| 188 | `K_HS_NO3N_RED_INHB` | 0.10 | Reversed Monod type half saturation concentration that simulate the inhibition effect of nitrate on consumption of Mn IV, Fe III, S |
| 189 | `K_HS_MN_IV_RED_INHB` | 0.10 | Reversed Monod type half saturation concentration that simulate the inhibition effect of Mn IV on consumption of Fe III, S VI and DO |
| 190 | `K_HS_FE_III_RED_INHB` | 0.10 | Reversed Monod type half saturation concentration that simulate the inhibition effect of Fe III on consumption of S VI and DOC itsel |
| 191 | `K_HS_S_PLUS_6_RED_INHB` | 0.10 | Reversed Monod type half saturation concentration that simulate the inhibition effect of S VI on consumption of DOC itself for DOC m |
| 192 | `PH_MIN_DOC_MIN_DOXY` | 6.00 | Min. pH for the optimum pH range for DOC mineralization with DOXY as final electron acceptor |
| 193 | `PH_MIN_DOC_MIN_NO3N` | 6.00 | Min. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor |
| 194 | `PH_MIN_DOC_MIN_MN_IV` | 6.00 | Min. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor |
| 195 | `PH_MIN_DOC_MIN_FE_III` | 6.00 | Min. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor |
| 196 | `PH_MIN_DOC_MIN_S_PLUS_6` | 6.00 | Min. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor |
| 197 | `PH_MIN_DOC_MIN_DOC` | 6.00 | Min. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor |
| 198 | `PH_MAX_DOC_MIN_DOXY` | 9.00 | Max. pH for the optimum pH range for DOC mineralization with dissolved oxygen as final electron acceptor |
| 199 | `PH_MAX_DOC_MIN_NO3N` | 9.00 | Max. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor |
| 200 | `PH_MAX_DOC_MIN_MN_IV` | 9.00 | Max. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor |
| 201 | `PH_MAX_DOC_MIN_FE_III` | 9.00 | Max. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor |
| 202 | `PH_MAX_DOC_MIN_S_PLUS_6` | 9.00 | Max. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor |
| 203 | `PH_MAX_DOC_MIN_DOC` | 9.00 | Max. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor |

### 12. DON Mineralisation (204-233) - 30 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 204 | `K_MIN_DON_DOXY_20` | 0.100 | 0.060   Mineralization rate constant of DON at 20 C for dissolved oxygen as final electron acceptor |
| 205 | `K_MIN_DON_NO3N_20` | 0.0012 | Mineralization rate constant of DON at 20 C for NO3_N as final electron acceptor |
| 206 | `K_MIN_DON_MN_IV_20` | 0.0012 | Mineralization rate constant of DON at 20 C for Mn IV as final electron acceptor |
| 207 | `K_MIN_DON_FE_III_20` | 0.0012 | Mineralization rate constant of DON at 20 C for FE III as final electron acceptor |
| 208 | `K_MIN_DON_S_PLUS_6_20` | 0.0012 | Mineralization rate constant of DON at 20 C for S_PLUS_6 as final electron acceptor |
| 209 | `K_MIN_DON_DOC_20` | 0.0012 | Mineralization rate constant of DON at 20 C for DOC as final electron acceptor |
| 210 | `THETA_K_MIN_DON_DOXY` | 1.08 | Temperature correction factor for mineralization process rate constant of DON for dissolved oxygen as final electron acceptor |
| 211 | `THETA_K_MIN_DON_NO3N` | 1.08 | Temperature correction factor for mineralization process rate constant of DON for nitrate as final electron acceptor |
| 212 | `THETA_K_MIN_DON_MN_IV` | 1.08 | Temperature correction factor for mineralization process rate constant of DON for MN_IV as final electron acceptor |
| 213 | `THETA_K_MIN_DON_FE_III` | 1.08 | Temperature correction factor for mineralization process rate constant of DON for FE_III as final electron acceptor |
| 214 | `THETA_K_MIN_DON_S_PLUS_6` | 1.08 | Temperature correction factor for mineralization process rate constant of DON for sulphate as final electron acceptor |
| 215 | `THETA_K_MIN_DON_DOC` | 1.08 | Temperature correction factor for mineralization process rate constant of DON for DOC |
| 216 | `K_HS_DON_MIN_DOXY` | 0.00 | Monod type half-saturation concentration of DON for DON mineralization for dissolved oxygen as the final electron acceptor |
| 217 | `K_HS_DON_MIN_NO3N` | 0.05 | Monod type half-saturation concentration of DON for DON mineralization for nitrate nitrogen as the final electron acceptor |
| 218 | `K_HS_DON_MIN_MN_IV` | 0.05 | Monod type half-saturation concentration of DON for DON mineralization for MN_IV as the final electron acceptor |
| 219 | `K_HS_DON_MIN_FE_III` | 0.05 | Monod type half-saturation concentration of DON for DON mineralization for FE_III as the final electron acceptor |
| 220 | `K_HS_DON_MIN_S_PLUS_6` | 0.05 | Monod type half-saturation concentration of DON for DON mineralization for sulphate sulphur as the final electron acceptor |
| 221 | `K_HS_DON_MIN_DOC` | 0.05 | Monod type half-saturation concentration of DON for DON mineralization for DOC as the final electron acceptor |
| 222 | `PH_MIN_DON_MIN_DOXY` | 6.00 | Min. pH for the optimum pH range for DON mineralization with DOXY as final electron acceptor |
| 223 | `PH_MIN_DON_MIN_NO3N` | 6.00 | Min. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor |
| 224 | `PH_MIN_DON_MIN_MN_IV` | 6.00 | Min. pH for the optimum pH range for DON mineralization with MN IV as final electron acceptor |
| 225 | `PH_MIN_DON_MIN_FE_III` | 6.00 | Min. pH for the optimum pH range for DON mineralization with FE III as final electron acceptor |
| 226 | `PH_MIN_DON_MIN_S_PLUS_6` | 6.00 | Min. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor |
| 227 | `PH_MIN_DON_MIN_DOC` | 6.00 | Min. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor |
| 228 | `PH_MAX_DON_MIN_DOXY` | 9.00 | Max. pH for the optimum pH range for DON mineralization with dissolved oxygen as final electron acceptor |
| 229 | `PH_MAX_DON_MIN_NO3N` | 9.00 | Max. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor |
| 230 | `PH_MAX_DON_MIN_MN_IV` | 9.00 | Max. pH for the optimum pH range for DON mineralization with Mn IV as final electron acceptor |
| 231 | `PH_MAX_DON_MIN_FE_III` | 9.00 | Max. pH for the optimum pH range for DON mineralization with FE_III as final electron acceptor |
| 232 | `PH_MAX_DON_MIN_S_PLUS_6` | 9.00 | Max. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor |
| 233 | `PH_MAX_DON_MIN_DOC` | 9.00 | Max. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor |

### 13. DOP Mineralisation (234-263) - 30 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 234 | `K_MIN_DOP_DOXY_20` | 0.70 | Mineralization rate constant of DOP at 20 C for dissolved oxygen as final electron acceptor |
| 235 | `K_MIN_DOP_NO3N_20` | 0.03 | Mineralization rate constant of DOP at 20 C for NO3_N as final electron acceptor |
| 236 | `K_MIN_DOP_MN_IV_20` | 0.03 | Mineralization rate constant of DOP at 20 C for Mn IV as final electron acceptor |
| 237 | `K_MIN_DOP_FE_III_20` | 0.03 | Mineralization rate constant of DOP at 20 C for FE_III as final electron acceptor |
| 238 | `K_MIN_DOP_S_PLUS_6_20` | 0.03 | Mineralization rate constant of DOP at 20 C for S_PLUS_6 as final electron acceptor |
| 239 | `K_MIN_DOP_DOC_20` | 0.03 | Mineralization rate constant of DOP at 20 C for DOC as final electron acceptor |
| 240 | `THETA_K_MIN_DOP_DOXY` | 1.06 | Temperature correction factor for mineralization process rate constant of DOP for dissolved oxygen as final electron acceptor |
| 241 | `THETA_K_MIN_DOP_NO3N` | 1.04 | Temperature correction factor for mineralization process rate constant of DOP for nitrate as final electron acceptor |
| 242 | `THETA_K_MIN_DOP_MN_IV` | 1.04 | Temperature correction factor for mineralization process rate constant of DOP for Mn IV as final electron acceptor |
| 243 | `THETA_K_MIN_DOP_FE_III` | 1.04 | Temperature correction factor for mineralization process rate constant of DOP for FE_III as final electron acceptor |
| 244 | `THETA_K_MIN_DOP_S_PLUS_6` | 1.04 | Temperature correction factor for mineralization process rate constant of DOP for sulphate as final electron acceptor |
| 245 | `THETA_K_MIN_DOP_DOC` | 1.04 | Temperature correction factor for mineralization process rate constant of DOP for DOC |
| 246 | `K_HS_DOP_MIN_DOXY` | 0.00 | Monod type half-saturation concentration of DOP for DOP mineralization for dissolved oxygen as the final electron acceptor |
| 247 | `K_HS_DOP_MIN_NO3N` | 0.052 | Monod type half-saturation concentration of DOP for DOP mineralization for nitrate nitrogen as the final electron acceptor |
| 248 | `K_HS_DOP_MIN_MN_IV` | 0.052 | Monod type half-saturation concentration of DOP for DOP mineralization for MN_IV as the final electron acceptor |
| 249 | `K_HS_DOP_MIN_FE_III` | 0.052 | Monod type half-saturation concentration of DOP for DOP mineralization for FE_III as the final electron acceptor |
| 250 | `K_HS_DOP_MIN_S_PLUS_6` | 0.052 | Monod type half-saturation concentration of DOP for DOP mineralization for sulphate sulphur as the final electron acceptor |
| 251 | `K_HS_DOP_MIN_DOC` | 0.052 | Monod type half-saturation concentration of DOP for DOP mineralization for DOC as the final electron acceptor |
| 252 | `PH_MIN_DOP_MIN_DOXY` | 6.00 | Min. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor |
| 253 | `PH_MIN_DOP_MIN_NO3N` | 6.00 | Min. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor |
| 254 | `PH_MIN_DOP_MIN_MN_IV` | 6.00 | Min. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor |
| 255 | `PH_MIN_DOP_MIN_FE_III` | 6.00 | Min. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor |
| 256 | `PH_MIN_DOP_MIN_S_PLUS_6` | 6.00 | Min. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor |
| 257 | `PH_MIN_DOP_MIN_DOC` | 6.00 | Min. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor |
| 258 | `PH_MAX_DOP_MIN_DOXY` | 9.00 | Max. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor |
| 259 | `PH_MAX_DOP_MIN_NO3N` | 9.00 | Max. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor |
| 260 | `PH_MAX_DOP_MIN_MN_IV` | 9.00 | Max. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor |
| 261 | `PH_MAX_DOP_MIN_FE_III` | 9.00 | Max. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor |
| 262 | `PH_MAX_DOP_MIN_S_PLUS_6` | 9.00 | Max. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor |
| 263 | `PH_MAX_DOP_MIN_DOC` | 9.00 | Max. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor |

### 14. Methane and Sulphide Oxidation (264-269) - 6 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 264 | `k_OX_CH4` | 1.0 | Methane oxidation rate constant at 20 C |
| 265 | `THETA_k_OX_CH4` | 1.04 | Arhenius type temperature correction for methane oxidation rate constant |
| 266 | `k_HS_OX_CH4_DOXY` | 1.5 | Monod type Half saturation of methane oxidation for dissolved oxygen |
| 267 | `k_OX_H2S` | 1.0 | Hydrogen sulphide oxidation rate constant at 20 C |
| 268 | `THETA_k_OX_H2S` | 1.04 | Arhenius type temperature correction for sulphide oxidation rate constant |
| 269 | `k_HS_OX_H2S_DOXY` | 1.5 | Monod type Half saturation of hydrogen sulphide oxidation for dissolved oxygen |

### 15. Iron Dissolution (270-275) - 6 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 270 | `k_DISS_FE_II_20` | 0.1 | Dissolution rate constant for Fe II at 20 C |
| 271 | `THETA_k_DISS_FE_II` | 1.04 | Dissolution rate constant for Fe II |
| 272 | `INIT_MULT_FE_II_DISS` | 0.01 | Initial fraction of dissolved Fe II |
| 273 | `k_DISS_FE_III_20` | 0.10 | Dissolution rate constant for Fe III at 20 C |
| 274 | `THETA_k_DISS_FE_III` | 1.04 | Dissolution rate constant for Fe III |
| 275 | `INIT_MULT_FE_III_DISS` | 0.50 | Initial fraction of dissolved Fe III |

### 16. Nostocales (276-306) - 31 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 276 | `KG_NOST_VEG_HET_OPT_TEMP` | 1.29 | 1.3   Nostocales (veg + het) Growth rate constant |
| 277 | `NOST_VEG_HET_OPT_TEMP_LR` | 16.0 | Nostocales (veg + het) optimal temperature lower range |
| 278 | `NOST_VEG_HET_OPT_TEMP_UR` | 26.0 | Nostocales (veg + het) optimal temperature upper range |
| 279 | `EFF_NOST_VEG_HET_GROWTH` | 0.95 | Nostocales (veg + het) Effective growth. (1-EG)*growth - losses for RESP and excretion |
| 280 | `KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP` | 0.080 | Nostocales (veg + het) Temperature correction for growth lower temperature |
| 281 | `KAPPA_NOST_VEG_HET_OVER_OPT_TEMP` | 0.05 | Nostocales (veg + het) Temperature correction for growth upper temperature |
| 282 | `KR_NOST_VEG_HET_20` | 0.06 | Nostocales (veg + het) RESP rate constant |
| 283 | `THETA_KR_NOST_VEG_HET` | 1.04 | Nostocales (veg + het) Temperature correction for RESP rate |
| 284 | `KD_NOST_VEG_HET_20` | 0.040 | Nostocales (veg + het) Mortality rate constant |
| 285 | `THETA_KD_NOST_VEG_HET` | 1.05 | Nostocales (veg + het) Temperature correction for Mortality rate |
| 286 | `M_DENS_VEG_HET` | 0.001 | Nostocales (veg + het) mortality rate constant because of too dense population |
| 287 | `KHS_DP_NOST_VEG_HET` | 0.005 | Nostocales (veg + het) Half saturation growth for DP |
| 288 | `KHS_O2_NOST_VEG_HET` | 0.60 | Nostocales (veg + het) Half saturation growth for O2 |
| 289 | `FRAC_NOST_VEG_HET_EXCR` | 0.30 | Nostocales (veg + het) Fraction of excretion in metabolism rate |
| 290 | `I_S_NOST_VEG_HET` | 100. | 1 Nostocales (veg + het) Light saturation (langleys). Not used, is calculated! |
| 291 | `DO_STR_HYPOX_NOST_VEG_HET_D` | 0.70 | Nostocales (veg + het) Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy |
| 292 | `THETA_HYPOX_NOST_VEG_HET_D` | 1.50 | Nostocales (veg + het) Multiplier of the exponent for Dissolved oxygen stress |
| 293 | `EXPON_HYPOX_NOST_VEG_HET_D` | 1.10 | Nostocales (veg + het) Exponent constant for Dissolved oxygen stress |
| 294 | `NOST_N_TO_C` | 0.220 | Nostocales (veg + het) Nitrogen to Carbon ratio |
| 295 | `NOST_P_TO_C` | 0.024 | Nostocales (veg + het) Phosphorus to Carbon ratio |
| 296 | `NOST_O2_TO_C` | 2.66 | Nostocales (veg + het) Oxygen to Carbon ratio for respiration |
| 297 | `NOST_C_TO_CHLA` | 40.00 | Nostocales (veg + het) Carbon to Chlorophyl a ratio |
| 298 | `P_GERM_AKI` | 0.3 | Germination rate constant for akinets, 1/day |
| 299 | `N_GERM_AKI` | 0.1 | DIN concentration to trigger germination (below that germination starts) |
| 300 | `P_FORM_AKI` | 0.1 | 0.3  Formation rate constant for akinets |
| 301 | `DAY_FORM_AKI` | 200 | Day of the year from which formation of akinetes is allowed |
| 302 | `T_FORM_AKI` | 16.0 | Temperature to trigger formation (below that formation starts) |
| 303 | `K_LOSS_AKI` | 0.000 | Loss rate constant for akinetes |
| 304 | `K_MORT_AKI_20` | 0.000 | Mortality rate constant of akinetes at 20 degrees celsius |
| 305 | `THETA_K_MORT_AKI` | 1.020 | Arhenius temperature correction factor for akinete mortality rate constant |
| 306 | `T_GERM_AKI` | 21.0 | Temperature to trigger germination when DIN is also low |

### 17. Organic Matter Availability (307-312) - 6 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 307 | `KHS_POC_DISS_SAT` | 1.250 |  |
| 308 | `KHS_PON_DISS_SAT` | 0.250 |  |
| 309 | `KHS_POP_DISS_SAT` | 0.0250 |  |
| 310 | `frac_avail_DON` | 0.00 |  |
| 311 | `frac_avail_DOP` | 0.00 |  |
| 312 | `frac_avail_DON_NOST` | 0.0 |  |

### 18. Nostocales Nitrogen (313-315) - 3 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 313 | `KHS_DN_NOST_VEG_HET` | 0.00720 | half saturation constant for limitation of nost. growth on nitrogen |
| 314 | `FRAC_FIX_N_FOR_GR_VEG_HET` | 0.650 | fraction of fixed ntrogen used for nostacles for growth. 1-frac is released to environment |
| 315 | `FRAC_NOST_GROWTH` | 0.10 | fraction of maximum growth rate supported by fixed nitrogen 1-frac - supported by N forms in WC |

### 19. Mineralisation Thresholds (316-318) - 3 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 316 | `K_MIN_PHYT_AMIN_DOC` | 4.00 |  |
| 317 | `K_MIN_PHYT_AMIN_DON` | 4.00 |  |
| 318 | `K_MIN_PHYT_AMIN_DOP` | 4.00 |  |

### 20. Light Curve and Ice (319-324) - 6 parameters

| ID | Name | Standard value | Description |
|---:|---|---:|---|
| 319 | `BETA_DIA` | 0.0 | Diatoms photoinhibition (0=Steele default, >0=stronger) |
| 320 | `BETA_CYN` | 0.0 | Non-fixing cyanobacteria photoinhibition |
| 321 | `BETA_FIX_CYN` | 0.0 | Fixing cyanobacteria photoinhibition |
| 322 | `BETA_OPA` | 0.0 | OtherPhyto photoinhibition |
| 323 | `BETA_NOST_VEG_HET` | 0.0 | Nostocales photoinhibition |
| 324 | `ICE_LIGHT_TRANS` | 1.00 | Under-ice PAR transmittance (1.0 = no attenuation; set <1 only with real ICE_COVER data) |

<!-- END GENERATED CONSTANTS -->

---

## State Variables Reference (36 total)

### Dissolved Nutrients (7)
1. NH4_N - Ammonium nitrogen
2. NO3_N - Nitrate nitrogen
3. PO4_P - Phosphate phosphorus
4. DISS_Si - Dissolved silica
20. INORG_C - Dissolved inorganic carbon
21. TOT_ALK - Total alkalinity
28. S_PLUS_6 - Sulfate

### Phytoplankton Biomass (6)
5. DIA_C - Diatoms carbon
15. CYN_C - Non-fixing cyanobacteria
16. OPA_C - Other phytoplankton
19. FIX_CYN_C - N-fixing cyanobacteria
31. NOST_VEG_HET_C - Nostocales
32. AKI_C - Akinetes

### Zooplankton (3)
6. ZOO_C - Carbon
7. ZOO_N - Nitrogen
8. ZOO_P - Phosphorus

### Organic Matter (6)
9. DET_PART_ORG_C - Particulate C
10. DET_PART_ORG_N - Particulate N
11. DET_PART_ORG_P - Particulate P
12. DISS_ORG_C - Dissolved C
13. DISS_ORG_N - Dissolved N
14. DISS_ORG_P - Dissolved P

### Dissolved Gases (2)
4. DISS_OXYGEN - Dissolved oxygen
30. CH4_C - Methane

### Particulate Minerals (1)
18. PART_Si - Particulate silica

### Redox Species (7)
22. FE_II - Ferrous iron
23. FE_III - Ferric iron
24. MN_II - Manganous Mn
25. MN_IV - Manganic Mn
26. CA - Calcium
27. MG - Magnesium
29. S_MINUS_2 - Sulfide

### Allelopathy (4)
33-36. SEC_METAB_* - Secondary metabolites

---

## Common Parameter Ranges

### Growth Rates (/day at optimal temp)
- Diatoms: 2.5-4.0
- Cyanobacteria: 2.0-3.5
- Other phytoplankton: 2.5-3.5
- Zooplankton: 0.3-0.6

### Mortality Rates (/day at 20°C)
- Phytoplankton: 0.05-0.15
- Zooplankton: 0.10-0.20

### Half-Saturation Constants (mg/L)
- Nitrogen: 0.01-0.03
- Phosphorus: 0.005-0.015
- Oxygen: 0.5-2.0
- Silica: 0.01-0.15

### Temperature Coefficients
- Respiration: 1.04-1.06
- Mortality: 1.02-1.05
- Mineralization: 1.04-1.08

### Stoichiometric Ratios
- N:C: 0.15-0.25 (Redfield ≈ 0.17)
- P:C: 0.015-0.03 (Redfield ≈ 0.024)
- C:Chl-a: 30-50 (varies by species)

---

## Calibration Tips

### Key Parameters for Calibration (Priority Order):

1. **Phytoplankton Growth Rates** (KG_*_OPT_TEMP)
   - Controls bloom timing and magnitude
   - Most sensitive parameters

2. **Mortality Rates** (KD_*_20)
   - Affects peak concentrations
   - Balances growth

3. **Half-Saturation Constants** (KHS_*)
   - Controls nutrient limitation
   - Affects species competition

4. **Stoichiometric Ratios** (*_N_TO_C, *_P_TO_C)
   - Should match literature values
   - Less variable, calibrate last

5. **Settling Velocities** (v_s_*)
   - Controls vertical distribution
   - Important for stratified systems

### Parameter Estimation Strategy:

1. **Start with literature values** (provided in WCONST_04.txt)
2. **Adjust growth rates** to match bloom timing
3. **Tune mortality** to match peak magnitudes
4. **Refine half-saturations** for nutrient dynamics
5. **Check mass balance** (C, N, P conservation)
6. **Validate against multiple years** of data

### Common Issues:

- **Unrealistic blooms:** Decrease growth or increase mortality
- **Low oxygen:** Check mineralization rates, reduce organic loading
- **Nutrient accumulation:** Increase uptake rates or phytoplankton growth
- **Model instability:** Reduce time step, check extreme parameter values

---

## Additional Configuration Files

### PELAGIC_MODEL_OPTIONS.txt
- Zooplankton option (0/1)
- Advanced redox (0/1)
- Light extinction method (0/1)
- Cyanobacteria buoyancy (0/1)
- Allelopathy consideration (0/1)

### INPUT.txt
- Simulation start/end dates
- Time step (steps per day)
- Output interval
- Sediment model toggle

### INIT_CONC_*.txt
- Initial concentrations for all 36 state variables
- Separate files for different scenarios

---

## References

For detailed parameter descriptions and model equations, refer to:
- AQUABC model documentation
- Original publications
- WCONST_04.txt inline comments
