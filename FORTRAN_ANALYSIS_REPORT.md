# Fortran Code Analysis & Recommendations

**Last updated:** 2026-02-12

## Overview
The AQUABC codebase is a Fortran 90/95 based aquatic chemistry and biology model. It uses a procedural approach, processing multiple spatial nodes (`nkn`) in vector operations. The core logic resides in `AQUABC_PELAGIC_KINETICS` which orchestrates various biological and chemical processes.

## Current Status Summary

| Area | Status |
|------|--------|
| `implicit none` coverage | 100% (all subroutines) |
| Precision unification | Complete (`precision_kinds` module) |
| Dead code cleanup | ~250 lines removed |
| Unused variables | 138 declarations removed |
| Compiler warnings | Zero on release builds |
| Division-by-zero guards | Comprehensive (pelagic + sediment) |
| Derived types refactoring | In progress (6 kinetics + 7 phyto subroutines done) |
| Named constants | Dimension magic numbers replaced |

## Key Findings

### 1. Architecture & State Management
*   **Current State**: The code relies heavily on a module `AQUABC_PELAGIC_INTERNAL` which acts as a global singleton state container. It holds allocatable arrays for state variables, driving functions, and intermediate rates.
*   **Issue**: This design limits the model to a single instance. It is not thread-safe and makes it difficult to run multiple independent simulations (e.g., in an ensemble) within the same executable without complex management.
*   **Recommendation**: Refactor `AQUABC_PELAGIC_INTERNAL` into a **Derived Type** (Class). This context object should be instantiated once and passed to the kinetics routines. This enables object-oriented patterns, thread safety, and better memory lifecycle management.
*   **Progress**: Derived types have been introduced for phytoplankton parameters (`t_diatom_params`, `t_cyn_params`, `t_opa_params`, etc.) and environmental inputs (`t_phyto_env`). The pattern has been applied to 6 kinetics subroutines and 7 phytoplankton subroutines, using `associate` blocks for zero-change variable mapping at call sites.

### 2. Performance Constraints
*   **Allocation Overhead**:
    *   In `AQUABC_PELAGIC_KINETICS`, there is a block of code related to definitions for `CO2SYS` where arrays (e.g., `CO2SYS_PAR1`, `CO2SYS_OUT_DATA`) are seemingly allocated and deallocated on *every subroutine call*.
    *   Dynamic memory allocation is expensive. Doing this inside the main time-stepping loop significantly degrades performance.
*   **Recommendation**:
    *   Pre-allocate working arrays (like those for `CO2SYS`) during initialization (in the proposed Derived Type) and reuse them.

### 3. Code Maintainability & Quality
*   **Dead Code**: ~~The files contain significant amounts of commented-out code, legacy optional arguments, and unused variables.~~ **RESOLVED** - ~250 lines of dead code and 138 unused variables removed.
*   **Hardcoded Constants**: ~~Several constants are defined inside the execution loop.~~ **PARTIALLY RESOLVED** - Dimension magic numbers replaced with named constants. Physical constants consolidated in `aquabc_physical_constants.f90`.
*   **Implicit Handling**: ~~While `implicit none` is generally used, some subroutines lack it.~~ **RESOLVED** - 100% coverage.
*   **Remaining work**:
    *   Continue derived type migration for remaining subroutines
    *   Use explicit `intent(in/out/inout)` for all dummy arguments

### 4. Modularization
*   **Library Structure**: The `AQUABC_PELAGIC_LIBRARY` folder shows good modularization (e.g., `REDOX_AND_SPECIATION`, `ZOOPLANKTON`).
*   **Integration**: The integration in the main loop is still monolithic.
*   **Recommendation**: Continue the derived types pattern. Each biological process should ideally accept structured parameter types and return structured rate types.

### 5. Numerical Safety (NEW)
*   **Comprehensive guards**: Division-by-zero protection applied across all model compartments using `max(divisor, 1.0D-20)` convention.
*   **pH clamping**: All pH-to-H+ conversions clamped to [4, 11] in both pelagic and sediment models.
*   **Safe exponential**: `safe_exp()` function clamps input to [-700, 700] before calling `exp()`.
*   **Input validation**: Temperature, salinity, and pH clamped at model entry points.

## Remaining Next Steps
1.  **Continue derived types**: Extend parameter bundling to remaining subroutines (zooplankton, organic carbon, sediment).
2.  **Pre-allocate CO2SYS buffers**: Move allocation out of the time-stepping loop.
3.  **Global state reduction**: Progressively replace module-level arrays with type-bound data.
