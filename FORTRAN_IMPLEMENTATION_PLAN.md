# AQUABC/ESTAS_II Fortran Implementation Plan

**Document Version:** 1.0
**Date:** 2026-01-18
**Author:** Claude Code Review
**Target Codebase:** AQUABCv0.2/SOURCE_CODE

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Current State Assessment](#2-current-state-assessment)
3. [Implementation Phases](#3-implementation-phases)
4. [Phase 1: Critical Safety Fixes](#4-phase-1-critical-safety-fixes)
5. [Phase 2: Code Modernization](#5-phase-2-code-modernization)
6. [Phase 3: Testing Infrastructure](#6-phase-3-testing-infrastructure)
7. [Phase 4: Performance Optimization](#7-phase-4-performance-optimization)
8. [Phase 5: Advanced Refactoring](#8-phase-5-advanced-refactoring)
9. [File-by-File Implementation Details](#9-file-by-file-implementation-details)
10. [Risk Assessment](#10-risk-assessment)
11. [Verification Strategy](#11-verification-strategy)
12. [Appendices](#12-appendices)

---

## 1. Executive Summary

This document provides a detailed implementation plan for improving the AQUABC/ESTAS_II Fortran codebase. The plan is organized into 5 phases, progressing from critical safety fixes to advanced refactoring. Each phase is designed to be independently valuable while building toward a more maintainable, performant, and reliable codebase.

### Goals

1. **Eliminate type safety issues** - Add `implicit none` to all subroutines
2. **Unify code standards** - Consistent precision types and naming conventions
3. **Improve testability** - Comprehensive unit and integration tests
4. **Enable parallelization** - OpenMP support for spatial loops
5. **Modernize architecture** - Derived types, reduced global state

### Success Metrics

| Metric | Current | Target |
|--------|---------|--------|
| Subroutines with `implicit none` | ~60% | 100% |
| Unit test coverage | ~5% | >50% |
| Regression test suite | None | Full suite |
| Parallel efficiency (8 cores) | N/A | >70% |
| Compile-time warnings | Unknown | Zero |

---

## 2. Current State Assessment

### 2.1 Codebase Statistics

```
Total Fortran Files: 56 (excluding build/)
Total Lines of Code: ~35,000
Main Components:
  - ESTAS Framework:     ~10,000 lines (25 files)
  - AQUABC Pelagic:      ~15,000 lines (15 files)
  - AQUABC Sediments:    ~5,000 lines (8 files)
  - CO2SYS:              ~4,750 lines (1 file)
  - Supporting modules:  ~500 lines (7 files)
```

### 2.2 Identified Issues Summary

| Priority | Issue | Count | Risk |
|----------|-------|-------|------|
| Critical | Missing `implicit none` | 35 subroutines | Silent bugs |
| High | Inconsistent precision types | 2 definitions | Precision loss |
| High | Hardcoded array dimensions | 10+ locations | Runtime failures |
| Medium | Long argument lists | 15+ subroutines | Maintenance burden |
| Medium | Global mutable state | 50+ variables | Debug difficulty |
| Low | Inconsistent naming | Throughout | Readability |
| Low | Commented-out code | 20+ blocks | Confusion |

### 2.3 Files Requiring Immediate Attention

```
SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90
SOURCE_CODE/ESTAS/mod_BASIN.f90
SOURCE_CODE/ESTAS/mod_MASS_LOAD.f90
SOURCE_CODE/ESTAS/sub_WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT.f90
SOURCE_CODE/ESTAS/sub_READ_PELAGIC_INPUTS.f90
SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90
SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_internal.f90
SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_DO_SATURATION.f90
SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_SETTLING.f90
SOURCE_CODE/AQUABC/SEDIMENTS/aquabc_II_sediment_auxillary.f90
SOURCE_CODE/AQUABC/SEDIMENTS/AQUABC_SEDIMENT_LIBRARY/aquabc_II_sediment_lib_DO_SATURATION.f90
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90
SOURCE_CODE/MACROALGAE/mod_MACROALGAE.f90
```

---

## 3. Implementation Phases

### Overview Timeline

```
Phase 1: Critical Safety Fixes          [Sprint 1-2]
    |
    v
Phase 2: Code Modernization             [Sprint 3-4]
    |
    v
Phase 3: Testing Infrastructure         [Sprint 5-7]
    |
    v
Phase 4: Performance Optimization       [Sprint 8-9]
    |
    v
Phase 5: Advanced Refactoring           [Sprint 10-12]
```

### Phase Dependencies

```
Phase 1 ──> Phase 2 ──> Phase 3
                │
                └──> Phase 4 ──> Phase 5
```

- Phase 3 (Testing) can proceed in parallel with Phase 4 (Performance)
- Phase 5 (Refactoring) requires Phases 2-4 to be complete

---

## 4. Phase 1: Critical Safety Fixes

**Duration:** 2 Sprints
**Priority:** CRITICAL
**Risk if Skipped:** Silent runtime bugs, incorrect results

### 4.1 Task 1.1: Add `implicit none` to All Subroutines

**Objective:** Ensure all variables are explicitly declared.

**Files to Modify:**

#### ESTAS Module Files

| File | Subroutine | Line |
|------|------------|------|
| `mod_BOTTOM_SEDIMENTS.f90` | `INIT_BSED_MODEL_CONSTANTS` | 50 |
| `mod_BASIN.f90` | `READ_BATHYMETRY_DATA_FROM_FILE` | 108 |
| `mod_MASS_LOAD.f90` | `ALLOC_MASS_LOAD_DATA` | 34 |
| `mod_MASS_LOAD.f90` | `DEALLOC_MASS_LOAD_DATA` | 45 |
| `mod_MASS_LOAD.f90` | `ALLOC_MASS_WITHDRAWAL_DATA` | 55 |
| `mod_MASS_LOAD.f90` | `DEALLOC_MASS_WITHDRAWAL_DATA` | 66 |
| `mod_MASS_LOAD.f90` | `ALLOC_OPEN_BOUNDARY_DATA` | 76 |
| `mod_MASS_LOAD.f90` | `DEALLOC_OPEN_BOUNDARY_DATA` | 87 |
| `sub_WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT.f90` | `WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT` | 1 |
| `sub_READ_PELAGIC_INPUTS.f90` | `READ_PELAGIC_BOX_MODEL_INPUTS` | 1 |

#### AQUABC Pelagic Files

| File | Subroutine | Line |
|------|------------|------|
| `aquabc_II_pelagic_auxillary.f90` | `AMMONIA_DON_PREFS` | 190 |
| `aquabc_II_pelagic_auxillary.f90` | `light_kd` | 644 |
| `aquabc_II_pelagic_auxillary.f90` | `CUR_SMITH` | 663 |
| `aquabc_II_pelagic_internal.f90` | `ALLOC_AQUABC_PELAGIC_INTERNAL` | 687 |
| `aquabc_II_pelagic_internal.f90` | `DEALLOC_AQUABC_PELAGIC_INTERNAL` | 1157 |
| `aquabc_II_pelagic_lib_DO_SATURATION.f90` | `DO_SATURATION_VEC` | 6 |
| `aquabc_II_pelagic_lib_SETTLING.f90` | `settl_vel_vec` | 50 |

#### AQUABC Sediment Files

| File | Subroutine | Line |
|------|------------|------|
| `aquabc_II_sediment_auxillary.f90` | `SED_MOD_1_CVISC` | 233 |
| `aquabc_II_sediment_lib_DO_SATURATION.f90` | `DO_SATURATION_MAT` | 4 |

#### CO2SYS File (14 subroutines)

| Subroutine | Line |
|------------|------|
| `CO2SYS` | 14 |
| `Constants` | 1303 |
| `CalculatepHfCO2fromTATC` | 2554 |
| `CalculatepHfromTATC` | 2655 |
| `CalculatefCO2fromTCpH` | 3163 |
| `CalculateTCfromTApH` | 3245 |
| `CalculatepHfromTAfCO2` | 3446 |
| `CalculateTAfromTCpH` | 3721 |
| `CalculatepHfromTCfCO2` | 3922 |
| `CalculateTCfrompHfCO2` | 4011 |
| `RevelleFactor` | 4090 |
| `CalculateAlkParts` | 4245 |
| `CaSolubility` | 4380 |
| `FindpHOnAllScales` | 4607 |

#### Macroalgae File

| File | Subroutine | Line |
|------|------------|------|
| `mod_MACROALGAE.f90` | `MACRO_ALGAE_KINETICS` | 62 |

**Implementation Steps:**

```fortran
! BEFORE:
subroutine EXAMPLE_SUBROUTINE(arg1, arg2)
    ! Missing implicit none
    real :: arg1, arg2

! AFTER:
subroutine EXAMPLE_SUBROUTINE(arg1, arg2)
    implicit none
    real, intent(in) :: arg1
    real, intent(out) :: arg2
```

**Verification:**
```bash
# Compile with strict checking
gfortran -fimplicit-none -Wall -Wextra -c <file.f90>
```

### 4.2 Task 1.2: Unify Precision Type Definitions

**Objective:** Create single source of truth for numeric precision.

**Current State:**
```fortran
! In mod_GLOBAL.f90:
integer, parameter :: DBL = selected_real_kind(15, 307)

! In mod_AQUABC_II_GLOBAL.f90:
integer :: DBL_PREC
parameter(DBL_PREC = selected_real_kind(15, 307))
```

**Target State:**

Create new file `SOURCE_CODE/CORE_UTILS/precision_kinds.f90`:

```fortran
!===============================================================================
! Module: precision_kinds
! Purpose: Unified precision definitions for AQUABC/ESTAS
!===============================================================================
module precision_kinds
    implicit none

    ! Working precision (double)
    integer, parameter :: wp = selected_real_kind(15, 307)

    ! Single precision (for I/O if needed)
    integer, parameter :: sp = selected_real_kind(6, 37)

    ! Quad precision (if available)
    integer, parameter :: qp = selected_real_kind(30, 300)

    ! Legacy aliases for backwards compatibility
    integer, parameter :: DBL = wp
    integer, parameter :: DBL_PREC = wp

end module precision_kinds
```

**Migration Steps:**

1. Create `precision_kinds.f90`
2. Update `mod_GLOBAL.f90`:
   ```fortran
   module GLOBAL
       use precision_kinds, only: wp, DBL
       ! Remove: integer, parameter :: DBL = selected_real_kind(15, 307)
   ```
3. Update `mod_AQUABC_II_GLOBAL.f90`:
   ```fortran
   module AQUABC_II_GLOBAL
       use precision_kinds, only: wp, DBL_PREC
       implicit none
       ! Remove old definition
   end module
   ```
4. Update Makefile to include new file in build order

### 4.3 Task 1.3: Add Compiler Warning Flags

**Objective:** Catch issues at compile time.

**Update Makefile:**

```makefile
# Add to FFLAGS for all build types:
COMMON_WARNINGS = -Wall -Wextra -Wconversion -Wunused-parameter

ifeq ($(FC),gfortran)
    ifeq ($(BUILD_TYPE),debug)
        FFLAGS = -g -Og -fcheck=all -fbacktrace -Wall -Wextra -pedantic \
                 -ffpe-trap=invalid,zero,overflow -fimplicit-none \
                 -Wconversion -Wunused-parameter -Wuninitialized
```

### 4.4 Task 1.4: Fix Hardcoded Dimensions

**Objective:** Centralize dimension parameters.

**Current Issues:**
```fortran
! Scattered across files:
integer, parameter :: nstate = 32    ! Must match!
integer, parameter :: nconst = 318   ! Must match!
```

**Solution:**

Create `SOURCE_CODE/CORE_UTILS/model_dimensions.f90`:

```fortran
module model_dimensions
    implicit none

    !---------------------------------------------------------------------------
    ! PELAGIC MODEL DIMENSIONS
    !---------------------------------------------------------------------------
    integer, parameter :: NSTATE_PELAGIC = 32          ! Number of pelagic state variables
    integer, parameter :: NCONST_PELAGIC = 318         ! Number of pelagic model constants
    integer, parameter :: NDRIVING_FUNCS = 10          ! Number of driving functions
    integer, parameter :: NFLAGS_PELAGIC = 5           ! Number of pelagic flags
    integer, parameter :: NSAVED_OUTPUTS = 5           ! Number of saved outputs
    integer, parameter :: NDIAGVAR_PELAGIC = 30        ! Diagnostic variables per state

    !---------------------------------------------------------------------------
    ! SEDIMENT MODEL DIMENSIONS
    !---------------------------------------------------------------------------
    integer, parameter :: NSTATE_SEDIMENT = 24         ! Sediment state variables
    integer, parameter :: NCONST_SEDIMENT = 171        ! Sediment model constants
    integer, parameter :: NFLUX_TO_SED = 24            ! Fluxes to sediments
    integer, parameter :: NFLUX_FROM_SED = 30          ! Fluxes from sediments
    integer, parameter :: NDIAGVAR_SEDIMENT = 25       ! Sediment diagnostics
    integer, parameter :: NOUTPUTS_SEDIMENT = 26       ! Sediment outputs
    integer, parameter :: NFLAGS_SEDIMENT = 3          ! Sediment flags

    !---------------------------------------------------------------------------
    ! ALLELOPATHY DIMENSIONS
    !---------------------------------------------------------------------------
    integer, parameter :: NSTATE_ALLELOPATHY = 4       ! Allelopathy state variables

end module model_dimensions
```

**Migration:**
1. Replace all `nstate = 32` with `use model_dimensions, only: NSTATE_PELAGIC`
2. Update array declarations to use named constants

---

## 5. Phase 2: Code Modernization

**Duration:** 2 Sprints
**Priority:** HIGH
**Prerequisite:** Phase 1 Complete

### 5.1 Task 2.1: Create Derived Types for Process Parameters

**Objective:** Reduce argument list lengths from 65+ to <10.

**Example: Diatom Parameters**

Create `SOURCE_CODE/AQUABC/PELAGIC/types_pelagic_params.f90`:

```fortran
module types_pelagic_params
    use precision_kinds, only: wp
    implicit none

    !---------------------------------------------------------------------------
    ! Type: diatom_params_t
    ! Contains all parameters for diatom kinetics
    !---------------------------------------------------------------------------
    type :: diatom_params_t
        ! Growth parameters
        real(wp) :: KG_OPT_TEMP          ! Growth rate at optimal temp
        real(wp) :: OPT_TEMP_LR          ! Optimal temp lower range
        real(wp) :: OPT_TEMP_UR          ! Optimal temp upper range
        real(wp) :: EFF_GROWTH           ! Effective growth factor
        real(wp) :: KAPPA_UNDER_OPT      ! Temp correction below optimal
        real(wp) :: KAPPA_OVER_OPT       ! Temp correction above optimal

        ! Respiration parameters
        real(wp) :: KR_20                ! Respiration rate at 20C
        real(wp) :: THETA_KR             ! Temp correction for respiration

        ! Mortality parameters
        real(wp) :: KD_20                ! Mortality rate at 20C
        real(wp) :: THETA_KD             ! Temp correction for mortality

        ! Half-saturation constants
        real(wp) :: KHS_DIN              ! Half-sat for DIN
        real(wp) :: KHS_DIP              ! Half-sat for DIP
        real(wp) :: KHS_DSi              ! Half-sat for silica
        real(wp) :: KHS_O2               ! Half-sat for oxygen

        ! Other parameters
        real(wp) :: FRAC_EXCR            ! Excretion fraction
        real(wp) :: I_S                  ! Light saturation
        real(wp) :: DO_STR_HYPOX_D       ! DO stress threshold
        real(wp) :: THETA_HYPOX_D        ! Hypoxia multiplier
        real(wp) :: EXPON_HYPOX_D        ! Hypoxia exponent

        ! Stoichiometry
        real(wp) :: N_TO_C               ! Nitrogen to carbon ratio
        real(wp) :: P_TO_C               ! Phosphorus to carbon ratio
        real(wp) :: Si_TO_C              ! Silica to carbon ratio
        real(wp) :: O2_TO_C              ! Oxygen to carbon ratio
        real(wp) :: C_TO_CHLA            ! Carbon to chlorophyll ratio
    contains
        procedure :: init => diatom_params_init
        procedure :: load_from_array => diatom_params_load
    end type diatom_params_t

    !---------------------------------------------------------------------------
    ! Similar types for:
    ! - cyanobacteria_params_t
    ! - fix_cyanobacteria_params_t
    ! - opa_params_t (other planktonic algae)
    ! - zooplankton_params_t
    ! - organic_matter_params_t
    ! - redox_params_t
    !---------------------------------------------------------------------------

contains

    subroutine diatom_params_init(this)
        class(diatom_params_t), intent(out) :: this
        ! Set default values
        this%KG_OPT_TEMP = 2.0_wp
        this%OPT_TEMP_LR = 12.0_wp
        this%OPT_TEMP_UR = 22.0_wp
        ! ... etc
    end subroutine

    subroutine diatom_params_load(this, MODEL_CONSTANTS)
        class(diatom_params_t), intent(out) :: this
        real(wp), intent(in) :: MODEL_CONSTANTS(:)

        this%KG_OPT_TEMP = MODEL_CONSTANTS(5)
        this%OPT_TEMP_LR = MODEL_CONSTANTS(6)
        this%OPT_TEMP_UR = MODEL_CONSTANTS(7)
        ! ... map all 318 constants to structured types
    end subroutine

end module types_pelagic_params
```

**Refactored Subroutine Signature:**

```fortran
! BEFORE (65 arguments):
subroutine DIATOMS(KG_DIA_OPT_TEMP, DIA_OPT_TEMP_LR, DIA_OPT_TEMP_UR, ...)

! AFTER (7 arguments):
subroutine DIATOMS(params, state, env, rates)
    type(diatom_params_t), intent(in) :: params
    type(pelagic_state_t), intent(in) :: state
    type(environment_t), intent(in) :: env
    type(diatom_rates_t), intent(out) :: rates
```

### 5.2 Task 2.2: Create State Variable Container Types

```fortran
module types_pelagic_state
    use precision_kinds, only: wp
    use model_dimensions, only: NSTATE_PELAGIC
    implicit none

    type :: pelagic_state_t
        integer :: nkn  ! Number of spatial nodes

        ! Nutrients
        real(wp), allocatable :: NH4_N(:)
        real(wp), allocatable :: NO3_N(:)
        real(wp), allocatable :: PO4_P(:)
        real(wp), allocatable :: DISS_Si(:)

        ! Dissolved gases
        real(wp), allocatable :: DISS_OXYGEN(:)
        real(wp), allocatable :: INORG_C(:)

        ! Phytoplankton
        real(wp), allocatable :: DIA_C(:)
        real(wp), allocatable :: CYN_C(:)
        real(wp), allocatable :: FIX_CYN_C(:)
        real(wp), allocatable :: OPA_C(:)

        ! Zooplankton
        real(wp), allocatable :: ZOO_C(:)
        real(wp), allocatable :: ZOO_N(:)
        real(wp), allocatable :: ZOO_P(:)

        ! Organic matter
        real(wp), allocatable :: DET_PART_ORG_C(:)
        real(wp), allocatable :: DET_PART_ORG_N(:)
        real(wp), allocatable :: DET_PART_ORG_P(:)
        real(wp), allocatable :: DISS_ORG_C(:)
        real(wp), allocatable :: DISS_ORG_N(:)
        real(wp), allocatable :: DISS_ORG_P(:)

        ! Metals and redox
        real(wp), allocatable :: FE_II(:)
        real(wp), allocatable :: FE_III(:)
        real(wp), allocatable :: MN_II(:)
        real(wp), allocatable :: MN_IV(:)

        ! Additional
        real(wp), allocatable :: TOT_ALK(:)
        real(wp), allocatable :: PART_Si(:)

    contains
        procedure :: allocate => pelagic_state_allocate
        procedure :: deallocate => pelagic_state_deallocate
        procedure :: from_array => pelagic_state_from_array
        procedure :: to_array => pelagic_state_to_array
    end type

end module types_pelagic_state
```

### 5.3 Task 2.3: Standardize Naming Conventions

**Convention to Adopt:**

| Element | Convention | Example |
|---------|------------|---------|
| Modules | `snake_case` | `pelagic_ecology` |
| Types | `snake_case_t` | `diatom_params_t` |
| Subroutines | `snake_case` | `calculate_diatom_growth` |
| Functions | `snake_case` | `do_saturation` |
| Constants | `UPPER_SNAKE_CASE` | `NSTATE_PELAGIC` |
| Variables | `snake_case` | `growth_rate` |
| Loop indices | `i`, `j`, `k`, `n` | Standard |

### 5.4 Task 2.4: Remove Dead Code

**Locations with Commented-Out Code:**

```
aquabc_II_pelagic_model.f90: Lines 134-141 (optional arguments)
mod_SIMULATE.f90: Various debugging blocks
aquabc_II_co2sys.f90: Legacy algorithm options
```

**Process:**
1. Review each commented block
2. If truly obsolete, remove entirely
3. If potentially useful, move to separate `deprecated/` directory
4. Document removal in git commit message

---

## 6. Phase 3: Testing Infrastructure

**Duration:** 3 Sprints
**Priority:** HIGH
**Prerequisite:** Phase 1 Complete (can run parallel to Phase 2)

### 6.1 Task 3.1: Create Test Framework

**Directory Structure:**

```
SOURCE_CODE/
├── tests/
│   ├── test_framework.f90      # Test utilities
│   ├── unit/
│   │   ├── test_diatoms.f90
│   │   ├── test_cyanobacteria.f90
│   │   ├── test_zooplankton.f90
│   │   ├── test_co2sys.f90
│   │   ├── test_do_saturation.f90
│   │   └── test_settling.f90
│   ├── integration/
│   │   ├── test_pelagic_kinetics.f90
│   │   ├── test_sediment_model.f90
│   │   └── test_full_simulation.f90
│   └── regression/
│       ├── baseline_outputs/
│       └── test_regression.f90
```

**Test Framework Module:**

```fortran
module test_framework
    use precision_kinds, only: wp
    implicit none

    integer :: tests_run = 0
    integer :: tests_passed = 0
    integer :: tests_failed = 0

contains

    subroutine assert_equal_real(actual, expected, tol, test_name)
        real(wp), intent(in) :: actual, expected, tol
        character(*), intent(in) :: test_name

        tests_run = tests_run + 1

        if (abs(actual - expected) <= tol) then
            tests_passed = tests_passed + 1
            write(*,'(A,A,A)') '  [PASS] ', test_name
        else
            tests_failed = tests_failed + 1
            write(*,'(A,A)') '  [FAIL] ', test_name
            write(*,'(A,E15.7,A,E15.7)') '    Expected: ', expected, ' Got: ', actual
        end if
    end subroutine

    subroutine assert_array_equal(actual, expected, tol, test_name)
        real(wp), intent(in) :: actual(:), expected(:)
        real(wp), intent(in) :: tol
        character(*), intent(in) :: test_name

        integer :: i
        logical :: passed

        tests_run = tests_run + 1
        passed = .true.

        if (size(actual) /= size(expected)) then
            passed = .false.
        else
            do i = 1, size(actual)
                if (abs(actual(i) - expected(i)) > tol) then
                    passed = .false.
                    exit
                end if
            end do
        end if

        if (passed) then
            tests_passed = tests_passed + 1
            write(*,'(A,A)') '  [PASS] ', test_name
        else
            tests_failed = tests_failed + 1
            write(*,'(A,A)') '  [FAIL] ', test_name
        end if
    end subroutine

    subroutine assert_no_nan(arr, test_name)
        real(wp), intent(in) :: arr(:)
        character(*), intent(in) :: test_name

        integer :: i
        logical :: has_nan

        tests_run = tests_run + 1
        has_nan = .false.

        do i = 1, size(arr)
            if (arr(i) /= arr(i)) then  ! NaN check
                has_nan = .true.
                exit
            end if
        end do

        if (.not. has_nan) then
            tests_passed = tests_passed + 1
            write(*,'(A,A)') '  [PASS] ', test_name
        else
            tests_failed = tests_failed + 1
            write(*,'(A,A)') '  [FAIL] ', test_name
            write(*,'(A)') '    Array contains NaN values'
        end if
    end subroutine

    subroutine assert_positive(arr, test_name)
        real(wp), intent(in) :: arr(:)
        character(*), intent(in) :: test_name

        tests_run = tests_run + 1

        if (all(arr >= 0.0_wp)) then
            tests_passed = tests_passed + 1
            write(*,'(A,A)') '  [PASS] ', test_name
        else
            tests_failed = tests_failed + 1
            write(*,'(A,A)') '  [FAIL] ', test_name
            write(*,'(A,E15.7)') '    Min value: ', minval(arr)
        end if
    end subroutine

    subroutine print_test_summary()
        write(*,'(A)') ''
        write(*,'(A)') '=========================================='
        write(*,'(A,I5)') 'Tests run:    ', tests_run
        write(*,'(A,I5)') 'Tests passed: ', tests_passed
        write(*,'(A,I5)') 'Tests failed: ', tests_failed
        write(*,'(A)') '=========================================='

        if (tests_failed > 0) then
            error stop 'Some tests failed!'
        end if
    end subroutine

end module test_framework
```

### 6.2 Task 3.2: Unit Tests for Process Subroutines

**Example: test_diatoms.f90**

```fortran
program test_diatoms
    use test_framework
    use precision_kinds, only: wp
    use types_pelagic_params
    use types_pelagic_state
    implicit none

    write(*,'(A)') 'Running Diatom Unit Tests'
    write(*,'(A)') '=========================================='

    call test_diatom_growth_rate()
    call test_diatom_nutrient_limitation()
    call test_diatom_light_limitation()
    call test_diatom_temperature_limitation()
    call test_diatom_mortality()
    call test_diatom_respiration()

    call print_test_summary()

contains

    subroutine test_diatom_growth_rate()
        type(diatom_params_t) :: params
        real(wp) :: growth_rate
        real(wp) :: temp, light, din, dip, dsi, do_conc

        write(*,'(A)') ''
        write(*,'(A)') 'Test: Diatom Growth Rate'

        call params%init()

        ! Test 1: Optimal conditions
        temp = 18.0_wp
        light = 300.0_wp
        din = 1.0_wp
        dip = 0.1_wp
        dsi = 2.0_wp
        do_conc = 8.0_wp

        ! Calculate growth rate (simplified)
        growth_rate = params%KG_OPT_TEMP  ! At optimal conditions

        call assert_equal_real(growth_rate, 2.0_wp, 0.01_wp, &
            'Growth rate at optimal conditions')

        ! Test 2: Growth should be positive
        call assert_positive([growth_rate], 'Growth rate is positive')

    end subroutine

    subroutine test_diatom_nutrient_limitation()
        real(wp) :: din, khs_din, limitation

        write(*,'(A)') ''
        write(*,'(A)') 'Test: Diatom Nutrient Limitation'

        khs_din = 0.02_wp

        ! Test Michaelis-Menten limitation
        din = 0.02_wp  ! At half-saturation
        limitation = din / (din + khs_din)
        call assert_equal_real(limitation, 0.5_wp, 0.001_wp, &
            'Limitation = 0.5 at half-saturation')

        din = 1.0_wp  ! High nutrient
        limitation = din / (din + khs_din)
        call assert_equal_real(limitation, 0.98_wp, 0.01_wp, &
            'Limitation near 1.0 at high nutrient')

        din = 0.001_wp  ! Low nutrient
        limitation = din / (din + khs_din)
        call assert_equal_real(limitation, 0.048_wp, 0.01_wp, &
            'Limitation low at low nutrient')

    end subroutine

    ! ... additional test subroutines

end program test_diatoms
```

### 6.3 Task 3.3: CO2SYS Validation Tests

```fortran
program test_co2sys
    use test_framework
    use CO2SYS_CDIAC
    use precision_kinds, only: wp
    implicit none

    write(*,'(A)') 'Running CO2SYS Unit Tests'
    write(*,'(A)') '=========================================='

    call test_ph_calculation()
    call test_carbonate_equilibrium()
    call test_temperature_dependence()
    call test_salinity_dependence()

    call print_test_summary()

contains

    subroutine test_ph_calculation()
        ! Test against known carbonate system values
        ! Reference: Dickson et al. (2007) Guide to best practices

        real(wp) :: TA, DIC, pH_calc, pH_expected
        real(wp) :: temp, sal

        write(*,'(A)') ''
        write(*,'(A)') 'Test: pH Calculation'

        ! Standard seawater conditions
        temp = 25.0_wp
        sal = 35.0_wp
        TA = 2300.0_wp   ! umol/kg
        DIC = 2000.0_wp  ! umol/kg
        pH_expected = 8.1_wp

        ! Call CO2SYS and extract pH
        ! ... implementation

        call assert_equal_real(pH_calc, pH_expected, 0.05_wp, &
            'pH at standard seawater conditions')

    end subroutine

end program test_co2sys
```

### 6.4 Task 3.4: Regression Test Suite

```fortran
program test_regression
    use test_framework
    implicit none

    character(len=256) :: baseline_dir, output_dir

    write(*,'(A)') 'Running Regression Tests'
    write(*,'(A)') '=========================================='

    baseline_dir = 'tests/regression/baseline_outputs/'
    output_dir = 'tests/regression/current_outputs/'

    call run_reference_simulation(output_dir)
    call compare_outputs(baseline_dir, output_dir)

    call print_test_summary()

contains

    subroutine compare_outputs(baseline, current)
        character(*), intent(in) :: baseline, current

        real(wp), allocatable :: baseline_data(:,:), current_data(:,:)
        real(wp) :: max_diff, rel_diff
        integer :: i, j

        ! Load baseline OUTPUT.csv
        ! Load current OUTPUT.csv
        ! Compare column by column

        do j = 1, size(baseline_data, 2)
            max_diff = maxval(abs(baseline_data(:,j) - current_data(:,j)))
            rel_diff = max_diff / (maxval(abs(baseline_data(:,j))) + 1.0e-10_wp)

            if (rel_diff < 1.0e-6_wp) then
                tests_passed = tests_passed + 1
            else
                tests_failed = tests_failed + 1
                write(*,'(A,I3,A,E12.4)') 'Column ', j, ' max relative diff: ', rel_diff
            end if
        end do

    end subroutine

end program test_regression
```

### 6.5 Task 3.5: Mass Balance Verification Tests

```fortran
program test_mass_balance
    use test_framework
    use precision_kinds, only: wp
    implicit none

    write(*,'(A)') 'Running Mass Balance Tests'
    write(*,'(A)') '=========================================='

    call test_nitrogen_balance()
    call test_carbon_balance()
    call test_phosphorus_balance()
    call test_silica_balance()

    call print_test_summary()

contains

    subroutine test_nitrogen_balance()
        real(wp) :: N_initial, N_final, N_inputs, N_outputs
        real(wp) :: balance_error, tolerance

        write(*,'(A)') ''
        write(*,'(A)') 'Test: Nitrogen Mass Balance'

        tolerance = 1.0e-6_wp  ! 0.0001% error tolerance

        ! Run short simulation
        ! Sum all N pools at start and end
        ! Sum all N fluxes

        ! N_initial + N_inputs = N_final + N_outputs
        balance_error = abs((N_initial + N_inputs) - (N_final + N_outputs))
        balance_error = balance_error / (N_initial + 1.0e-10_wp)

        call assert_equal_real(balance_error, 0.0_wp, tolerance, &
            'Nitrogen mass balance closes')

    end subroutine

end program test_mass_balance
```

### 6.6 Task 3.6: Add Test Targets to Makefile

```makefile
#===============================================================================
# Test Targets
#===============================================================================

TESTDIR = SOURCE_CODE/tests
TEST_FRAMEWORK = $(TESTDIR)/test_framework.f90

# Unit tests
test-diatoms: $(LIBAQUABC) $(TEST_FRAMEWORK)
	$(FC) $(FFLAGS) -o test_diatoms \
		$(TEST_FRAMEWORK) \
		$(TESTDIR)/unit/test_diatoms.f90 \
		-L$(BUILDDIR) -laquabc
	./test_diatoms

test-co2sys: $(LIBAQUABC) $(TEST_FRAMEWORK)
	$(FC) $(FFLAGS) -o test_co2sys \
		$(TEST_FRAMEWORK) \
		$(TESTDIR)/unit/test_co2sys.f90 \
		-L$(BUILDDIR) -laquabc
	./test_co2sys

test-all-unit: test-diatoms test-co2sys test-zooplankton test-settling
	@echo "All unit tests passed"

# Integration tests
test-pelagic: $(LIBAQUABC)
	$(FC) $(FFLAGS) -o test_pelagic \
		$(TEST_FRAMEWORK) \
		$(TESTDIR)/integration/test_pelagic_kinetics.f90 \
		-L$(BUILDDIR) -laquabc
	./test_pelagic

# Regression tests
test-regression: build-estas
	./ESTAS_II INPUTS/INPUT.txt
	$(FC) $(FFLAGS) -o test_regression \
		$(TEST_FRAMEWORK) \
		$(TESTDIR)/regression/test_regression.f90
	./test_regression

# Run all tests
test: test-all-unit test-pelagic test-regression
	@echo ""
	@echo "========================================"
	@echo "All tests completed successfully!"
	@echo "========================================"

.PHONY: test test-all-unit test-regression
```

---

## 7. Phase 4: Performance Optimization

**Duration:** 2 Sprints
**Priority:** MEDIUM
**Prerequisite:** Phase 1 and 3 Complete

### 7.1 Task 4.1: Add OpenMP Parallelization

**Target Loops:**

The main spatial loop in `aquabc_II_pelagic_model.f90`:

```fortran
! Current:
do i = 1, nkn
    ! Process calculations for node i
end do

! With OpenMP:
!$OMP PARALLEL DO PRIVATE(i, local_vars) SHARED(STATE_VARIABLES, DERIVATIVES)
do i = 1, nkn
    ! Process calculations for node i
end do
!$OMP END PARALLEL DO
```

**Implementation Steps:**

1. **Identify thread-safe variables:**
   ```fortran
   ! Shared (read-only): MODEL_CONSTANTS, DRIVING_FUNCTIONS, STATE_VARIABLES
   ! Private (per-thread): all intermediate calculation variables
   ! Reduction: any accumulator variables
   ```

2. **Add OpenMP directives to key loops:**

   In `mod_SOLVER.f90`:
   ```fortran
   !$OMP PARALLEL DO PRIVATE(i,j) SCHEDULE(DYNAMIC)
   do i = 1, NUM_PELAGIC_BOXES
       do j = 1, NUM_PELAGIC_STATE_VARS
           ! Mass update calculations
       end do
   end do
   !$OMP END PARALLEL DO
   ```

   In `aquabc_II_pelagic_model.f90`:
   ```fortran
   !$OMP PARALLEL DO DEFAULT(NONE) &
   !$OMP SHARED(nkn, STATE_VARIABLES, MODEL_CONSTANTS, DRIVING_FUNCTIONS) &
   !$OMP SHARED(DERIVATIVES, PROCESS_RATES) &
   !$OMP PRIVATE(i, temp, salt, light, ...)
   do i = 1, nkn
       call PELAGIC_KINETICS_NODE(i, ...)
   end do
   !$OMP END PARALLEL DO
   ```

3. **Update Makefile:**
   ```makefile
   # OpenMP flags
   ifeq ($(FC),gfortran)
       OPENMP_FLAGS = -fopenmp
   else ifeq ($(FC),ifort)
       OPENMP_FLAGS = -qopenmp
   else ifeq ($(FC),ifx)
       OPENMP_FLAGS = -qopenmp
   endif

   # Add to build
   ifdef ENABLE_OPENMP
       FFLAGS += $(OPENMP_FLAGS)
   endif
   ```

4. **Runtime configuration:**
   ```bash
   # Set number of threads
   export OMP_NUM_THREADS=8

   # Run with OpenMP
   make ENABLE_OPENMP=1 build-estas
   ./ESTAS_II INPUT.txt
   ```

### 7.2 Task 4.2: Memory Access Optimization

**Current Issue:** Array layout may not be cache-optimal.

**Solution:** Ensure contiguous memory access in inner loops.

```fortran
! BEFORE (column-major but accessed row-wise):
do i = 1, nkn
    do j = 1, nstate
        DERIVATIVES(i, j) = ...
    end do
end do

! AFTER (access in column-major order):
do j = 1, nstate
    do i = 1, nkn
        DERIVATIVES(i, j) = ...
    end do
end do
```

### 7.3 Task 4.3: Vectorization Hints

Add compiler directives for SIMD vectorization:

```fortran
!DIR$ SIMD
do i = 1, nkn
    ! Simple arithmetic operations
    LIM_KG_DIA_N(i) = (NH4_N(i) + NO3_N(i)) / &
                      ((NH4_N(i) + NO3_N(i)) + KHS_DIN_DIA)
end do
```

### 7.4 Task 4.4: Profile-Guided Optimization

**Profiling workflow:**

```bash
# 1. Build with profiling
make FC=gfortran BUILD_TYPE=debug FFLAGS="-pg" build-estas

# 2. Run simulation
./ESTAS_II INPUT.txt

# 3. Analyze profile
gprof ESTAS_II gmon.out > profile.txt

# 4. Identify hotspots and optimize
```

**Expected hotspots:**
- `AQUABC_PELAGIC_KINETICS` (~60% of runtime)
- `SOLVE` in mod_SOLVER (~20%)
- `CO2SYS` (~10%)
- I/O operations (~5%)

---

## 8. Phase 5: Advanced Refactoring

**Duration:** 3 Sprints
**Priority:** LOW
**Prerequisite:** Phases 1-4 Complete

### 8.1 Task 5.1: Reduce Global State

**Current:** 50+ global allocatable arrays in `mod_GLOBAL.f90`

**Target:** Pass data through function arguments using derived types.

**New Module Structure:**

```fortran
module aquatic_model_data
    use precision_kinds, only: wp
    use types_pelagic_state
    use types_pelagic_params
    implicit none

    type :: aquatic_model_t
        ! Simulation parameters
        real(wp) :: time_start
        real(wp) :: time_end
        real(wp) :: time_step
        integer :: num_boxes

        ! State containers
        type(pelagic_state_t) :: pelagic_state
        type(sediment_state_t) :: sediment_state

        ! Parameter containers
        type(pelagic_params_t) :: pelagic_params
        type(sediment_params_t) :: sediment_params

        ! Output arrays
        real(wp), allocatable :: derivatives(:,:)
        real(wp), allocatable :: process_rates(:,:,:)

    contains
        procedure :: initialize
        procedure :: run_timestep
        procedure :: finalize
    end type

end module aquatic_model_data
```

### 8.2 Task 5.2: Separate CO2SYS into Standalone Library

**Directory structure:**
```
SOURCE_CODE/
├── CO2SYS_LIB/
│   ├── co2sys_module.f90
│   ├── co2sys_constants.f90
│   ├── co2sys_equilibria.f90
│   ├── co2sys_solvers.f90
│   └── CMakeLists.txt  # For standalone build
```

**Interface:**
```fortran
module co2sys_interface
    implicit none

    type :: carbonate_system_t
        real(8) :: temperature
        real(8) :: salinity
        real(8) :: pressure
        real(8) :: total_alkalinity
        real(8) :: dissolved_inorganic_carbon
        real(8) :: pH
        real(8) :: pCO2
        real(8) :: CO2_aq
        real(8) :: HCO3
        real(8) :: CO3
    contains
        procedure :: solve_from_TA_DIC
        procedure :: solve_from_pH_DIC
        procedure :: solve_from_TA_pH
    end type

end module co2sys_interface
```

### 8.3 Task 5.3: Implement Higher-Order ODE Solvers

**Add to mod_SOLVER.f90:**

```fortran
module ode_solvers
    use precision_kinds, only: wp
    implicit none

    integer, parameter :: SOLVER_EULER = 1
    integer, parameter :: SOLVER_RK4 = 2
    integer, parameter :: SOLVER_RK45_ADAPTIVE = 3

contains

    subroutine rk4_step(state, derivs, dt, state_new)
        real(wp), intent(in) :: state(:), dt
        real(wp), intent(out) :: state_new(:)

        interface
            subroutine derivs(s, dsdt)
                import wp
                real(wp), intent(in) :: s(:)
                real(wp), intent(out) :: dsdt(:)
            end subroutine
        end interface

        real(wp), allocatable :: k1(:), k2(:), k3(:), k4(:), temp(:)
        integer :: n

        n = size(state)
        allocate(k1(n), k2(n), k3(n), k4(n), temp(n))

        call derivs(state, k1)

        temp = state + 0.5_wp * dt * k1
        call derivs(temp, k2)

        temp = state + 0.5_wp * dt * k2
        call derivs(temp, k3)

        temp = state + dt * k3
        call derivs(temp, k4)

        state_new = state + (dt / 6.0_wp) * (k1 + 2.0_wp*k2 + 2.0_wp*k3 + k4)

        deallocate(k1, k2, k3, k4, temp)

    end subroutine rk4_step

end module ode_solvers
```

### 8.4 Task 5.4: Add Runtime Configuration

**Create configuration file parser:**

```fortran
module runtime_config
    use precision_kinds, only: wp
    implicit none

    type :: config_t
        ! Solver options
        integer :: solver_type = 1  ! 1=Euler, 2=RK4
        real(wp) :: max_timestep = 0.1_wp
        real(wp) :: min_timestep = 1.0e-6_wp

        ! Output options
        integer :: output_interval = 1
        logical :: write_process_rates = .false.
        logical :: write_mass_balance = .true.

        ! Parallel options
        integer :: num_threads = 1

        ! Model options
        logical :: enable_sediments = .true.
        logical :: enable_allelopathy = .false.

    contains
        procedure :: read_from_file
        procedure :: validate
    end type

end module runtime_config
```

---

## 9. File-by-File Implementation Details

### 9.1 Priority 1 Files (Phase 1)

| File | Tasks | Estimated Changes |
|------|-------|-------------------|
| `mod_AQUABC_II_GLOBAL.f90` | Use precision_kinds | 3 lines |
| `mod_GLOBAL.f90` | Use precision_kinds, model_dimensions | 20 lines |
| `mod_BOTTOM_SEDIMENTS.f90` | Add implicit none (1 sub) | 5 lines |
| `mod_BASIN.f90` | Add implicit none (1 sub) | 5 lines |
| `mod_MASS_LOAD.f90` | Add implicit none (6 subs) | 30 lines |
| `aquabc_II_pelagic_auxillary.f90` | Add implicit none (3 subs) | 15 lines |
| `aquabc_II_pelagic_internal.f90` | Add implicit none (2 subs) | 10 lines |
| `aquabc_II_co2sys.f90` | Add implicit none (14 subs) | 70 lines |

### 9.2 Priority 2 Files (Phase 2)

| File | Tasks | Estimated Changes |
|------|-------|-------------------|
| `aquabc_II_pelagic_lib_DIATOMS.f90` | Convert to derived types | 200 lines |
| `aquabc_II_pelagic_lib_CYANOBACTERIA.f90` | Convert to derived types | 200 lines |
| `aquabc_II_pelagic_lib_ZOOPLANKTON.f90` | Convert to derived types | 250 lines |
| `aquabc_II_pelagic_model.f90` | Use new types | 500 lines |

### 9.3 New Files to Create

| File | Purpose | Phase |
|------|---------|-------|
| `precision_kinds.f90` | Unified precision definitions | 1 |
| `model_dimensions.f90` | Centralized dimensions | 1 |
| `test_framework.f90` | Test utilities | 3 |
| `types_pelagic_params.f90` | Parameter derived types | 2 |
| `types_pelagic_state.f90` | State variable types | 2 |
| `ode_solvers.f90` | RK4/adaptive solvers | 5 |

---

## 10. Risk Assessment

### 10.1 Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Breaking existing functionality | Medium | High | Comprehensive regression tests before changes |
| Performance regression | Low | Medium | Benchmark before/after each phase |
| Compiler compatibility issues | Low | Medium | Test with gfortran, ifort, ifx |
| Memory issues with new types | Low | Medium | Memory profiling, careful allocation |

### 10.2 Schedule Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Underestimated effort for CO2SYS | Medium | Medium | Allow buffer time, prioritize safety fixes |
| Parallel implementation complexity | Medium | Medium | Start with simple loops, incremental |
| Testing infrastructure delays | Low | High | Begin test framework in Phase 1 |

### 10.3 Rollback Strategy

1. **Version Control:** Tag releases before each phase
   ```bash
   git tag -a v0.2.0-pre-phase1 -m "Before Phase 1 changes"
   ```

2. **Branching:** Use feature branches
   ```bash
   git checkout -b feature/phase1-implicit-none
   ```

3. **Incremental Commits:** Small, atomic changes with clear messages

4. **Baseline Outputs:** Save reference outputs for regression testing

---

## 11. Verification Strategy

### 11.1 Phase 1 Verification

```bash
# 1. Compile with strict flags
make FC=gfortran BUILD_TYPE=debug clean build-estas

# 2. Run existing tests
cd SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/test
./test_0D_no_nan.sh
./test_0D_clamp_summary.sh

# 3. Run reference simulation
cd /home/razinka/AQUABCv0.2
./ESTAS_II INPUTS/INPUT.txt > output_phase1.log

# 4. Compare outputs
diff OUTPUT.csv baseline/OUTPUT.csv
```

### 11.2 Phase 2 Verification

```bash
# 1. Unit tests for new types
make test-diatoms
make test-cyanobacteria

# 2. Integration test
make test-pelagic

# 3. Numerical equivalence
python3 compare_outputs.py baseline/OUTPUT.csv OUTPUT.csv --tolerance 1e-10
```

### 11.3 Phase 3 Verification

```bash
# Full test suite
make test

# Coverage report (if using gcov)
make BUILD_TYPE=coverage test
gcov *.gcno
```

### 11.4 Phase 4 Verification

```bash
# 1. Performance benchmark
time ./ESTAS_II INPUT.txt  # Serial
time OMP_NUM_THREADS=4 ./ESTAS_II INPUT.txt  # Parallel

# 2. Verify numerical results unchanged
diff OUTPUT_serial.csv OUTPUT_parallel.csv

# 3. Scalability test
for threads in 1 2 4 8 16; do
    export OMP_NUM_THREADS=$threads
    time ./ESTAS_II INPUT.txt 2>&1 | grep real
done
```

---

## 12. Appendices

### Appendix A: Complete List of Files Missing `implicit none`

```
SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90:50: subroutine INIT_BSED_MODEL_CONSTANTS
SOURCE_CODE/ESTAS/mod_BASIN.f90:108: subroutine READ_BATHYMETRY_DATA_FROM_FILE
SOURCE_CODE/ESTAS/mod_MASS_LOAD.f90:34: subroutine ALLOC_MASS_LOAD_DATA
SOURCE_CODE/ESTAS/mod_MASS_LOAD.f90:45: subroutine DEALLOC_MASS_LOAD_DATA
SOURCE_CODE/ESTAS/mod_MASS_LOAD.f90:55: subroutine ALLOC_MASS_WITHDRAWAL_DATA
SOURCE_CODE/ESTAS/mod_MASS_LOAD.f90:66: subroutine DEALLOC_MASS_WITHDRAWAL_DATA
SOURCE_CODE/ESTAS/mod_MASS_LOAD.f90:76: subroutine ALLOC_OPEN_BOUNDARY_DATA
SOURCE_CODE/ESTAS/mod_MASS_LOAD.f90:87: subroutine DEALLOC_OPEN_BOUNDARY_DATA
SOURCE_CODE/ESTAS/sub_WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT.f90:1: subroutine WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT
SOURCE_CODE/ESTAS/sub_READ_PELAGIC_INPUTS.f90:1: subroutine READ_PELAGIC_BOX_MODEL_INPUTS
SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90:190: subroutine AMMONIA_DON_PREFS
SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90:644: subroutine light_kd
SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90:663: subroutine CUR_SMITH
SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_internal.f90:687: subroutine ALLOC_AQUABC_PELAGIC_INTERNAL
SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_internal.f90:1157: subroutine DEALLOC_AQUABC_PELAGIC_INTERNAL
SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_DO_SATURATION.f90:6: subroutine DO_SATURATION_VEC
SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_SETTLING.f90:50: subroutine settl_vel_vec
SOURCE_CODE/AQUABC/SEDIMENTS/aquabc_II_sediment_auxillary.f90:233: subroutine SED_MOD_1_CVISC
SOURCE_CODE/AQUABC/SEDIMENTS/AQUABC_SEDIMENT_LIBRARY/aquabc_II_sediment_lib_DO_SATURATION.f90:4: subroutine DO_SATURATION_MAT
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:14: subroutine CO2SYS
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:1303: subroutine Constants
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:2554: subroutine CalculatepHfCO2fromTATC
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:2655: subroutine CalculatepHfromTATC
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:3163: subroutine CalculatefCO2fromTCpH
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:3245: subroutine CalculateTCfromTApH
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:3446: subroutine CalculatepHfromTAfCO2
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:3721: subroutine CalculateTAfromTCpH
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:3922: subroutine CalculatepHfromTCfCO2
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:4011: subroutine CalculateTCfrompHfCO2
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:4090: subroutine RevelleFactor
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:4245: subroutine CalculateAlkParts
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:4380: subroutine CaSolubility
SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90:4607: subroutine FindpHOnAllScales
SOURCE_CODE/MACROALGAE/mod_MACROALGAE.f90:62: subroutine MACRO_ALGAE_KINETICS
```

### Appendix B: Model Constants Mapping (First 100)

| Index | Name | Description |
|-------|------|-------------|
| 1 | K_A | Aeration coefficient |
| 2 | THETA_K_A | Temperature correction for aeration |
| 3 | XKC | Light extinction per chlorophyll |
| 4 | PHIMX | Quantum yield constant |
| 5 | KG_DIA_OPT_TEMP | Diatom growth rate at optimal temp |
| 6 | DIA_OPT_TEMP_LR | Diatom optimal temp lower range |
| 7 | DIA_OPT_TEMP_UR | Diatom optimal temp upper range |
| ... | ... | ... |
| 318 | (last constant) | ... |

*Full mapping available in `PARAMETER_REFERENCE.md`*

### Appendix C: State Variable Indices

| Index | Name | Units | Description |
|-------|------|-------|-------------|
| 1 | NH4_N | mg N/L | Ammonium nitrogen |
| 2 | NO3_N | mg N/L | Nitrate nitrogen |
| 3 | PO4_P | mg P/L | Phosphate phosphorus |
| 4 | DISS_OXYGEN | mg O2/L | Dissolved oxygen |
| 5 | DIA_C | mg C/L | Diatom carbon |
| 6 | ZOO_C | mg C/L | Zooplankton carbon |
| 7 | ZOO_N | mg N/L | Zooplankton nitrogen |
| 8 | ZOO_P | mg P/L | Zooplankton phosphorus |
| 9 | DET_PART_ORG_C | mg C/L | Detrital particulate organic carbon |
| 10 | DET_PART_ORG_N | mg N/L | Detrital particulate organic nitrogen |
| 11 | DET_PART_ORG_P | mg P/L | Detrital particulate organic phosphorus |
| 12 | DISS_ORG_C | mg C/L | Dissolved organic carbon |
| 13 | DISS_ORG_N | mg N/L | Dissolved organic nitrogen |
| 14 | DISS_ORG_P | mg P/L | Dissolved organic phosphorus |
| 15 | CYN_C | mg C/L | Non-fixing cyanobacteria carbon |
| 16 | OPA_C | mg C/L | Other planktonic algae carbon |
| 17 | DISS_Si | mg Si/L | Dissolved silica |
| 18 | PART_Si | mg Si/L | Particulate silica |
| 19 | FIX_CYN_C | mg C/L | Nitrogen-fixing cyanobacteria carbon |
| 20 | INORG_C | mg C/L | Inorganic carbon |
| 21 | TOT_ALK | meq/L | Total alkalinity |
| 22 | FE_II | mg Fe/L | Ferrous iron |
| 23 | FE_III | mg Fe/L | Ferric iron |
| 24 | MN_II | mg Mn/L | Manganous manganese |
| 25 | MN_IV | mg Mn/L | Manganic manganese |
| 26 | CA | mg Ca/L | Calcium |
| 27 | MG | mg Mg/L | Magnesium |
| 28 | S_PLUS_6 | mg S/L | Sulfate sulfur |
| 29 | S_MINUS_2 | mg S/L | Sulfide sulfur |
| 30 | CH4_C | mg C/L | Methane carbon |
| 31 | NOST_VEG_HET_C | mg C/L | Nostocales vegetative+heterocyst carbon |
| 32 | NOST_AKI_C | mg C/L | Nostocales akinete carbon |

---

## Document Control

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-18 | Claude Code Review | Initial document |

---

*End of Implementation Plan*
