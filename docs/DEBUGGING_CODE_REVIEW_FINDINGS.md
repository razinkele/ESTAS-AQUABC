# AQUABC Model: Code Review for Debugging Model Instability

## Executive Summary

This document presents findings from a comprehensive code review of the AQUABC pelagic water quality model, focusing on identifying potential sources of numerical instability and mathematical implementation inconsistencies.

**Status: HIGH-PRIORITY FIXES IMPLEMENTED** ✅

---

## 1. Potential Division by Zero Vulnerabilities

### 1.1 Monod Kinetics (Half-Saturation Expressions)

**Location:** Multiple files in `SOURCE_CODE/AQUABC/PELAGIC/`

The model uses Monod-type kinetics extensively:
```fortran
LIM_DOXY_RED = DISS_OXYGEN / (DISS_OXYGEN + K_HS_DOXY_RED_LIM)
```

**Risk Assessment:** LOW to MODERATE
- When `DISS_OXYGEN = 0` and `K_HS_DOXY_RED_LIM = 0`, this produces `0/0 = NaN`
- Half-saturation constants are typically > 0 from calibration, but no runtime check exists

**Similar patterns in:**
- `aquabc_II_pelagic_model.f90` lines ~1760-1950: DON, DOP, DOC mineralization
- `aquabc_II_pelagic_lib_DIATOMS.f90` line ~200: `LIM_KG_DIA_N`
- `aquabc_II_sediment_lib_DOC_MINER.f90` lines 184-238

**Recommendation:** Add parameter validation at initialization:
```fortran
if (K_HS_DOXY_RED_LIM <= 0.0D0) then
    write(*,*) 'ERROR: K_HS_DOXY_RED_LIM must be positive'
    stop
end if
```

---

### 1.2 Light Limitation Function (LIM_LIGHT / CUR_SMITH) ✅ FIXED

**Location:** `aquabc_II_pelagic_auxillary.f90` lines 640-760

**Original Issue:**
```fortran
TEMP1 = SKE * H
RLIGHT = 2.7183D0 / TEMP1 * (EXP(-TEMP2 * ITOT * TEMP3) - EXP(-TEMP2 * ITOT))
```

**Risk Assessment:** HIGH → **RESOLVED**
- If `SKE = 0` (no extinction) or `H = 0` (zero depth), `TEMP1 = 0` causes division by zero

**Fix Applied:**
```fortran
! Guard against division by zero when TEMP1 (extinction * depth) is very small
if (TEMP1 .lt. 1.0D-10) then
    LLIGHT = 1.0D0  ! Full light at surface
    CCHLX  = 15.0D0 ! Minimum C:Chla ratio
    return
end if
```

---

### 1.3 Ammonia Preference Calculations (AMMONIA_PREFS) ✅ FIXED

**Location:** `aquabc_II_pelagic_auxillary.f90` lines 78-100

**Original Issue:**
```fortran
where (NH3 .lt. 1.0D-6 .and. NOx .lt. 1.0D-6)
    PN = 0.0D0
elsewhere
    PN = ... / ((NH3 + NOx) * ...)
end where
```

**Risk Assessment:** MODERATE → **RESOLVED**
- The guard checked `NH3 < 1e-6 AND NOx < 1e-6`, but division by `(NH3 + NOx)` can still fail

**Fix Applied:**
```fortran
! Guard condition: check sum of NH3+NOx to prevent division by near-zero
where ((NH3 + NOx) .lt. 1.0D-6)
    PN = 0.0D0
elsewhere
    PN = ...
end where
```

**Same fix applied to:** `DIN_DON_PREFS`, `DIP_DOP_PREFS`, `DOP_DIP_PREFS`, `AMMONIA_DON_PREFS`

---

### 1.4 NP Ratio Calculation ✅ FIXED

**Location:** `aquabc_II_pelagic_model.f90` line ~2514

**Original Issue:**
```fortran
PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 16) = &
    ((NH4_N + NO3_N)/14.D0)/(DIP_OVER_IP*PO4_P/31.D0)
```

**Risk Assessment:** HIGH → **RESOLVED**
- If `PO4_P = 0` or `DIP_OVER_IP = 0`, this produces division by zero

**Fix Applied:**
```fortran
! NP molar ratio calculation with guard against division by zero
where (DIP_OVER_IP * PO4_P .lt. 1.0D-10)
    PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 16) = 999.0D0  ! Extreme N:P indicates P limitation
elsewhere
    PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 16) = &
        ((NH4_N + NO3_N)/14.D0)/(DIP_OVER_IP*PO4_P/31.D0)
end where
```

---

### 1.5 Iron/Manganese Dissolved Fraction Calculations ✅ FIXED

**Location:** `aquabc_II_pelagic_model.f90` lines 550-770

**Original Issue:**
```fortran
elsewhere
    MULT_FE_II_DISS = DISS_FE_II_CONC_TS_AVG / FE_II  ! FE_II could be 0 here
end where
```

**Risk Assessment:** MODERATE → **RESOLVED**

**Fix Applied:**
```fortran
where(DISS_FE_II_CONC_TS_AVG >= FE_II)
    ...
elsewhere(FE_II .lt. 1.0D-20)
    ! Guard against division by zero when total Fe2+ is depleted
    FE_II_DISS      = 0.0D0
    MULT_FE_II_DISS = 1.0D0
    MULT_FE_II_PART = 0.0D0
elsewhere
    MULT_FE_II_DISS = DISS_FE_II_CONC_TS_AVG / FE_II
    ...
end where
```

**Same fix applied to:** `FE_III`, `MN_II`, and `SAVED_OUTPUTS` calculations

---

## 2. Fractional Exponents on Potentially Negative Values

### 2.1 Active Switching in Zooplankton Grazing

**Location:** `aquabc_II_pelagic_lib_ZOOPLANKTON.f90`

```fortran
SWITCHING_POWER = 1.5D0
! Later: (relative_abundance) ** (SWITCHING_POWER - 1) = x^0.5
```

**Risk Assessment:** MODERATE
- `x^0.5` produces NaN for negative x
- Protected by `TOTAL_FOOD > 1.0D-10` guard, but intermediate calculations might have numerical noise

**Recommendation:** Add explicit floor:
```fortran
relative_abundance = max(relative_abundance, 0.0D0)
```

---

### 2.2 Temperature Correction Factors (Arrhenius-type)

**Location:** Multiple files

```fortran
K_RATE = K_RATE_20 * (THETA ** (TEMP - 20.0D0))
```

**Risk Assessment:** LOW
- If THETA < 0 (erroneous input), this produces complex numbers
- Temperature typically constrained to realistic ranges

**Recommendation:** Parameter validation at initialization.

---

## 3. Mathematical Consistency Issues

### 3.1 Stoichiometric Ratio Inconsistencies

**Location:** `aquabc_II_pelagic_model.f90` lines 2150-2200

**Observation:** Nutrient uptake for cyanobacteria uses different formulations depending on `DO_CYANO_BOUYANT_STATE_SIMULATION`:

```fortran
if (DO_CYANO_BOUYANT_STATE_SIMULATION > 0) then
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 7) = R_CYN_GROWTH * CYN_N_TO_C * PREF_DIN_DON_CYN * PREF_NH4N_CYN
else
    ! Old formulation with additional NH4/(NH4+DON) term
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 7) = R_CYN_GROWTH * PREF_NH4N_DON_CYN * CYN_N_TO_C * (NH4_N / ...)
end if
```

**Issue:** The two branches use fundamentally different preference formulations, which could lead to mass balance inconsistencies when switching modes.

---

### 3.2 Oxygen Photosynthesis Coefficients

**Location:** `aquabc_II_pelagic_model.f90` lines 2370-2400

```fortran
PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 2)  = &
    R_DIA_GROWTH * (1.3D0 - 0.3D0*PREF_NH4N_DIA) * DIA_O2_TO_C
```

**Note:** The factor `(1.3 - 0.3*PREF)` accounts for reduced O2 production when using NH4 vs NO3. This is theoretically sound but the coefficient 1.3 implies baseline assumes 100% NO3 uptake.

---

### 3.3 Zooplankton C:N:P Ratios

**Location:** `aquabc_II_pelagic_model.f90` lines 2830-2840

```fortran
DERIVATIVES(1:nkn,ZOO_N_INDEX) = DERIVATIVES(1:nkn,ZOO_C_INDEX) * ACTUAL_ZOO_N_TO_C
DERIVATIVES(1:nkn,ZOO_P_INDEX) = DERIVATIVES(1:nkn,ZOO_C_INDEX) * ACTUAL_ZOO_P_TO_C
```

**Observation:** This assumes fixed stoichiometry for zooplankton, which may not hold during variable food quality scenarios.

---

## 4. Existing Safeguards (Positive Findings)

### 4.1 STRANGERSD Function
- Checks for NaN and Inf in arrays
- Used extensively in debug mode

### 4.2 Removal Limiters
- Clamping logic exists for `FIX_CYN_C` to prevent excessive process rates
- Pattern: `allowed_rate = STATE / max(TIME_STEP, 1.0D-12)`

### 4.3 Warning Output
- Negative state variable warnings exist:
  ```fortran
  if(any(DISS_ORG_N .le. 0.D0)) then
      print *, 'PELAGIC MODEL: Warning, some DISS_ORG_N <= 0'
  end if
  ```

### 4.4 Debug Subroutines
- Comprehensive `DBGSTR_PEL_*` routines for each major state variable

---

## 5. Recommended Priority Fixes

### High Priority (Immediate)
1. **Light limitation zero-depth guard** - Add explicit check for `TEMP1 > 0`
2. **NP ratio guard** - Prevent division by zero in phosphorus-limited scenarios
3. **AMMONIA_PREFS sum guard** - Change condition to check `NH3 + NOx`

### Medium Priority (Short-term)
4. **Parameter validation at initialization** - Ensure all half-saturation constants > 0
5. **Consistent preference formulations** - Reconcile old vs new cyanobacteria uptake code
6. **Floor negative abundances** in zooplankton switching calculations

### Low Priority (Long-term)
7. **Variable stoichiometry** for zooplankton C:N:P
8. **Unified redox simulation pathway** - Remove code duplication between `if (DO_ADVANCED_REDOX_SIMULATION > 0)` branches

---

## 6. Testing Recommendations

1. **Boundary condition tests**: Run with extreme values (DO=0, nutrients=0, depth=0.01m)
2. **Long simulation stability tests**: 365-day runs with varying forcing
3. **Mass balance verification**: Sum all sources/sinks for each element (C, N, P, Si, O2)
4. **NaN propagation tests**: Inject small numerical noise to test robustness

---

## Document Information

- **Review Date:** Generated during code review session
- **Files Reviewed:** 
  - `aquabc_II_pelagic_model.f90` (3910 lines)
  - `aquabc_II_pelagic_auxillary.f90` (1468 lines)
  - `aquabc_II_pelagic_lib_DIATOMS.f90`
  - `aquabc_II_pelagic_lib_ZOOPLANKTON.f90`
  - Sediment library files
- **Model Version:** AQUABCv0.2
