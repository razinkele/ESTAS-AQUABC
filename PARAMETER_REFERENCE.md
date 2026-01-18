# AQUABC Model Parameters Quick Reference

## Parameter File Structure

**Main Parameter File:** `INPUTS/WCONST_04.txt`
**Format:** `ID  NAME  VALUE  !COMMENT`
**Total Parameters:** 318

---

## Parameter Categories

### 1. General Parameters (1-5)
- Aeration coefficient
- Light extinction
- Quantum yield

### 2. Diatoms (5-31) - 27 parameters
**Growth:**
- KG_DIA_OPT_TEMP: Growth rate (default: 3.7 /day)
- DIA_OPT_TEMP_LR/UR: Optimal temperature range (1-24°C)
- EFF_DIA_GROWTH: Growth efficiency (0.95)

**Limitation:**
- KHS_DIN_DIA: Half-saturation for DIN (0.010 mg/L)
- KHS_DIP_DIA: Half-saturation for DIP (0.005 mg/L)
- KHS_DSi_DIA: Half-saturation for DSi (0.013 mg/L)
- KHS_O2_DIA: Half-saturation for O2 (0.60 mg/L)

**Loss Processes:**
- KR_DIA_20: Respiration rate (0.05 /day)
- KD_DIA_20: Mortality rate (0.12 /day)
- THETA_KR/KD_DIA: Temperature coefficients

**Stoichiometry:**
- DIA_N_TO_C: N:C ratio (0.22)
- DIA_P_TO_C: P:C ratio (0.024)
- DIA_Si_TO_C: Si:C ratio (0.25)
- DIA_C_TO_CHLA: C:Chl-a ratio (30.0)

### 3. Non-Fixing Cyanobacteria (29-50) - 22 parameters
Similar structure to diatoms:
- Growth: KG_CYN_OPT_TEMP (2.4 /day)
- Optimal temp: 15-26°C
- No silica requirement
- Higher C:Chl-a ratio (40.0)

### 4. Nitrogen-Fixing Cyanobacteria (51-74) - 24 parameters
**Unique Parameters:**
- R_FIX: Fixing/non-fixing growth ratio (1.0)
- K_FIX: N-fixation efficiency (0.008)
- Can grow without DIN limitation

### 5. Other Phytoplankton (75-96) - 22 parameters
- Growth: KG_OPA_OPT_TEMP (2.9 /day)
- Optimal temp: 9-20°C (cooler preference)
- General freshwater algae

### 6. Zooplankton (97-133) - 37 parameters
**Growth & Grazing:**
- KG_ZOO_OPT_TEMP: Growth rate (0.45 /day)
- GRAT_ZOO_*: Grazing rates on different prey
- PREF_ZOO_*: Feeding preferences (must sum to 1.0)

**Preferences (default):**
- Diatoms: 0.26
- Non-fixing cyanobacteria: 0.10
- Fixing cyanobacteria: 0.07
- Other phytoplankton: 0.37
- Detritus: 0.20

**Half-Saturations:**
- KHS_*_C_ZOO: For each food type
- FOOD_MIN_ZOO: Minimum food for feeding (0.02 mg C/L)

### 7. Particulate Organic Matter (134-145) - 12 parameters
**Dissolution Rates:**
- KDISS_DET_PART_ORG_C_20: POC dissolution (10.0 /day)
- KDISS_DET_PART_ORG_N_20: PON dissolution (0.25 /day)
- KDISS_DET_PART_ORG_P_20: POP dissolution (3.48 /day)
- KDISS_PART_Si_20: Biogenic silica dissolution (0.001 /day)

**Phytoplankton Enhancement:**
- FAC_PHYT_DET_PART_ORG_*: Bacterial colonization effect

### 8. Dissolved Organic Matter (146-152) - 7 parameters
**Mineralization:**
- FAC_PHYT_AMIN_DOC: DOC mineralization factor (0.0045)
- FAC_PHYT_AMIN_DON: DON mineralization factor (0.008)
- FAC_PHYT_AMIN_DOP: DOP mineralization factor (0.90)

**Reverse Half-Saturations:**
- KHS_AMIN_N: Inhibition by DIN (100.0 mg/L)
- KHS_AMIN_P: Inhibition by DIP (0.025 mg/L)

### 9. Nitrification (152-157) - 6 parameters
**Ammonia Oxidation:**
- K_NITR_20: Nitrification rate (0.600 /day)
- THETA_K_NITR: Temperature coefficient (1.045)
- KHS_NITR_OXY: O2 half-saturation (2.0 mg/L)
- KHS_NITR_NH4_N: NH4 half-saturation (0.030 mg/L)
- PH_NITR_NH4_MIN/MAX: Optimal pH range (6.9-8.2)

### 10. Redox Chemistry (158-209) - 52 parameters
**Metal Oxidation/Reduction:**
- k_OX_FE_II: Fe(II) oxidation (0.00125 /day)
- k_RED_FE_III: Fe(III) reduction (2.00 /day)
- k_OX_MN_II: Mn(II) oxidation (0.01 /day)
- k_RED_MN_IV: Mn(IV) reduction (2.00 /day)

**DOC Mineralization with Different Electron Acceptors:**
- K_MIN_DOC_DOXY_20: With O2 (0.010 /day)
- K_MIN_DOC_NO3N_20: With NO3 (0.025 /day)
- K_MIN_DOC_MN_IV_20: With Mn(IV) (0.025 /day)
- K_MIN_DOC_FE_III_20: With Fe(III) (0.025 /day)
- K_MIN_DOC_S_PLUS_6_20: With SO4 (0.025 /day)
- K_MIN_DOC_DOC_20: Methanogenesis (0.025 /day)

**Inhibition Constants:**
- K_HS_DOXY_RED_INHB: O2 inhibits NO3 reduction (0.10 mg/L)
- K_HS_NO3N_RED_INHB: NO3 inhibits Mn reduction (0.10 mg/L)
- Follows thermodynamic sequence

**pH Effects:**
- PH_MIN/MAX_DOC_MIN_*: Optimal pH ranges for each process

### 11. Methane (210-234) - 25 parameters
**Production:**
- K_MIN_DOC_DOC_20: Methanogenesis rate
- Stoichiometry of CH4 production

**Oxidation:**
- K_OX_CH4: Methane oxidation rate
- KHS_O2_CH4_OX: O2 half-saturation for oxidation
- Temperature dependencies

### 12. Settling (235-250) - 16 parameters
**Settling Velocities:**
- v_s_DIA: Diatom settling (m/day)
- v_s_CYN: Cyanobacteria settling
- v_s_OPA: Other phytoplankton
- v_s_DET_PART_ORG_C: Detritus settling
- v_s_PART_Si: Biogenic silica settling

**Temperature Effects:**
- THETA_v_s_*: Temperature corrections

### 13. pH Correction Factors (251-266) - 16 parameters
**Process pH Dependencies:**
- PH_MIN/MAX_*: Optimal pH ranges for:
  - Phytoplankton growth
  - Zooplankton grazing
  - Mineralization
  - Nitrification

### 14. Dissolved Metal Fractions (267-276) - 10 parameters
**Fe Dissolution:**
- k_DISS_FE_II_20: Fe(II) dissolution rate (0.1 /day)
- INIT_MULT_FE_II_DISS: Initial dissolved fraction (0.01)
- k_DISS_FE_III_20: Fe(III) dissolution rate (0.10 /day)
- INIT_MULT_FE_III_DISS: Initial dissolved fraction (0.50)

### 15. Nostocales (Heterocystous Cyanobacteria) (276-298) - 23 parameters
**Growth:**
- KG_NOST_VEG_HET_OPT_TEMP: Growth rate (1.29 /day)
- Optimal temp: 16-26°C

**Mortality:**
- KD_NOST_VEG_HET_20: Base mortality (0.040 /day)
- M_DENS_VEG_HET: Density-dependent mortality (0.001)

**Akinete Formation:**
- P_GERM_AKI: Germination rate (0.3 /day)

**Unique Features:**
- Nitrogen fixation capability
- Heterocyst formation
- Akinete (resting stage) dynamics

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
