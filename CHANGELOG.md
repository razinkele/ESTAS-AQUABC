# AQUABC Changelog

All notable changes to the AQUABC model and Shiny frontend are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

---

## [Unreleased]

### Added
- **Bioturbation module** (`aquabc_II_sediment_bioturbation.f90`): depth-dependent biodiffusion, oxygen-dependent scaling, seasonal modulation, bioirrigation enhancement, and zero-flux lower boundary condition (Boudreau 1997; Soetaert et al. 1996)
- Dynamic particle mixing coefficients (`switch_partmixing = 1`): biodiffusion recomputed every sub-timestep from local depth, O₂, and day-of-year
- Bioirrigation enhancement of porewater diffusion for dissolved-phase species
- 23 unit tests for all bioturbation functions (depth attenuation, O₂ scaling, seasonality, effective Db, bioirrigation, array application, last-layer BC)
- Integration test input (`INPUT_sediment_test.txt`) for running the coupled model with full sediment diagenesis
- AQUABC and ESTAS PDF reference manuals (`docs/AQUABC_Reference_Manual.pdf`, `docs/ESTAS_Reference_Manual.pdf`)
- Makefile target `make build-docs` for PDF generation via pandoc

### Fixed
- **Missing `isedi` parameter**: registered `isedi = 0` in ESTAS `INIT_BSED_MODEL_CONSTANTS` — was expected from SHYFEM parameter system but never provided in standalone ESTAS
- **Sediment flux output format**: corrected format descriptor from `33F20.10` to `36F20.10` to match actual `FLUXES_OUTPUT_TO_WATER_COLUMN` array size (`nstate + NUM_ALLOLOPATHY_STATE_VARS = 36`)

### Changed
- Sediment model particle mixing now uses bioturbation physics (exponential depth decay × Monod O₂ × seasonal) instead of uniform constant
- Last-layer particle mixing boundary condition changed from hard-coded zero to proper zero-flux (Neumann) BC
- Updated `AQUABC_Model_Equations.md` with bioturbation/bioirrigation equations (§13.8) and references

- Unit tests for FIX_CYANOBACTERIA, OTHER_PLANKTONIC_ALGAE, and NOSTOCALES kinetics subroutines (25 test programs, 196 assertions total)
- Dependabot configuration (`.github/dependabot.yml`) for weekly pip and GitHub Actions dependency scanning
- `requirements-dev.txt` for development/test Python dependencies (ruff, pytest)
- Cardinal Temperature Model with Inflection (CTMI, Rosso et al. 1993) replacing piecewise-exponential temperature response for phytoplankton growth
- Synthesizing Unit nutrient colimitation (Saito et al. 2008) replacing Liebig's Law of the Minimum for all phytoplankton groups
- Tunable Platt-style photoinhibition (BETA parameter) for light limitation in all phytoplankton groups
- Unified precision type definitions via `precision_kinds` module
- Compiler warning flags (`-Wall -Wextra`) for release and fast builds
- Comprehensive AQUABC model equations reference document (`AQUABC_Model_Equations.md`)

### Changed
- OpenMP parallelization of pelagic kinetics nkn loop: single parallel region with chunked array slicing, ~750 line-level changes in `aquabc_II_pelagic_model.f90`, all library subroutine calls use per-thread derived type bundles, debug calls guarded with `!$omp master` barriers, serial fallback via Fortran sentinel comments
- Bundled scalar constant arguments into derived types (`t_diatom_params`, `t_cyn_params`, `t_opa_params`, etc.) for 6 kinetics subroutines
- Bundled environmental input arrays into `t_phyto_env` derived type in 7 phytoplankton subroutines
- Bundled ORGANIC_CARBON_MINERALIZATION I/O arrays into 3 new shared pointer types (`t_redox_state`, `t_redox_lim`, `t_docmin_outputs`), reducing arguments from 36 to 9
- Bundled REDOX_AND_SPECIATION I/O arrays into shared pointer types, reducing arguments from 33 to 12
- Removed 5 dead arguments (`K_NO3_RED`, `K_MN_IV_RED`, `K_FE_III_RED`, `K_S_PLUS_6_RED`, `K_DOC_RED`) from ORGANIC_CARBON_MINERALIZATION
- Replaced hardcoded dimension magic numbers with named constants throughout
- Replaced tabs with spaces in 15 source files for consistent formatting

### Security
- Added `safe_resolve()` path traversal protection to all Shiny app file operations (`load_file`, `save_file`, `file_info_panel`, `validate_constants_file`, observation handlers)
- Added subprocess timeouts (120s clean, 600s build) with `kill()` on timeout to prevent hung processes
- Added bounded output buffers to all subprocess stdout readers

### Added (Developer Tooling)
- `pyproject.toml` with ruff linter configuration (E, F, W, I, UP, B, S rules)
- 46 Python unit tests for parsers (`parameter_parser`, `ic_parser`, `options_parser`, `simulation_config`) and `safe_resolve`
- 19 Playwright integration tests covering app startup, navigation, and all major panels
- 9 Selenium integration tests (gracefully skip without chromedriver)
- CI `python-lint-test` job running ruff and pytest in parallel with Fortran build
- `CONTRIBUTING.md` with build/test/PR workflow documentation
- Makefile convenience targets: `make test-all`, `make test-python`, `make test-fortran`, `make lint`
- Fixed all 653 ruff lint warnings across `shiny_app/*.py` (whitespace, imports, type annotations, bare excepts)

### Fixed
- Fixed 3 zero-valued Monod half-saturation defaults (`K_HS_DOC_MIN_DOXY=0→1.0`, `K_HS_DON_MIN_DOXY=0→0.05`, `K_HS_DOP_MIN_DOXY=0→0.052`) that caused NaN when substrate concentration reached zero
- 25 Fortran unit tests (196 assertions) covering kinetics subroutines (DIATOMS, CYANOBACTERIA, FIX_CYANOBACTERIA, OTHER_PLANKTONIC_ALGAE, NOSTOCALES, ZOOPLANKTON, REDOX_AND_SPECIATION, ORGANIC_CARBON_MINERALIZATION) plus utility subroutines
- Shared test defaults module (`test_defaults.f90`) with realistic parameter populators for all derived types
- Guarded ~25 division-by-zero risks in sediment model (porosity, depth, mixing length, pH-to-H+ conversions)
- Guarded REDOX speciation divisions against zero/negative values in pelagic model
- Guarded CO2SYS critical divisions and discriminant
- Guarded macroalgae quota divisions against zero biomass
- Guarded state variable ratio divisions in pelagic model (Fe/Mn dissolved fractions)
- Added pH, temperature, and salinity clamping at model entry points
- Replaced raw `exp()` with `safe_exp()` in light limitation calculations to prevent overflow
- Numerical hardening: zooplankton switching, parameter validation, cyanobacteria cleanup
- Fixed 13 critical ecological model bugs across pelagic, sediment, and macroalgae
- Fixed Mn speciation, leap year, bathymetry initialization, and NaN guards
- Fixed settling suppression bug, allocation error checks, and getpar precision
- Added `implicit none` to all remaining subroutines and functions
- Removed 138 unused variable declarations across 25 source files
- Removed 5 unused parameter imports and declarations
- Resolved compare-reals and real-to-integer conversion warnings
- Initialized variables that may be used uninitialized
- Resolved character truncation warning in WRITE_PELAGIC_MODEL_CONSTANTS
- Fixed missing `precision_kinds` dependency in 6 test targets
- Removed ~250 lines of dead/commented-out code across 9 source files
- Fixed `shinywidgets` version pin (0.7.0 → 0.7.1) in `requirements.txt`

---

## [0.2.1] - 2026-01-22

### Added
- Pop-up tooltips throughout the entire interface for all major controls
- Changelog widget in the app header bar
- Output directory now saved/loaded with simulation configuration
- Two-column layout for output boxes selection
- CI integration with ftnchek static analysis for Fortran code quality
- Fortran unit tests for VECTOR_MATRIX_UTILS module

### Changed
- Build Options button moved inside Run Parameters card (cleaner layout)
- Output Directory moved from Simulation Config to Output Config tab
- Reduced vertical spacing in Run Parameters card for compactness
- Reduced text size in run log display
- Increased run log buffer from 50KB to 200KB
- Sediment Model disabled by default
- WCONST_04.txt set as visible default for Pelagic Constants File

### Fixed
- Intel Fortran (ifx) compiler linking with runtime libraries (-lifcore -lifport)
- Allelopathy kinetics: added missing concentration term for first-order degradation
- Allelopathy kinetics: initialize SEC_METAB state variables (33-36) before rate calculations
- Removed redeclaration of S_SEC_METAB_* variables to avoid module conflicts

---

## [0.2.0] - 2026-01-21

### Added
- Active Switching Model for zooplankton food selection
  - Implements Gentleman et al. (2003) switching formulation
  - Configurable via MODEL_SWITCHES.txt (K_ZOO_SWITCH parameter)
  - Documentation in docs/ZOOPLANKTON_SWITCHING_MODEL.md
- Multi-compiler build support (gfortran, ifort, ifx)
- Shiny frontend for model configuration and execution
  - Dashboard with quick run controls
  - Model build panel with compiler selection
  - Simulation configuration with presets
  - Parameter editor with categories
  - Initial conditions editor
  - Model options and switches editor
  - Scenario presets system
  - Results visualization with dual-axis plots
  - Mass balance calculations
  - Model validation with observations

### Changed
- Upgraded from AQUABC 0.1 to 0.2 with improved kinetics
- Reorganized source code structure

### Fixed
- Various numerical stability improvements

---

## [0.1.0] - 2025-01-01

### Added
- Initial release of AQUABC biogeochemical model
- Basic pelagic kinetics for:
  - Phytoplankton (Diatoms, Cyanobacteria, Other Phytoplankton)
  - Zooplankton
  - Dissolved oxygen
  - Nutrients (N, P, Si, Fe)
  - Organic matter (DOC, POC, DON, PON, DOP, POP)
- Bottom sediment diagenesis model (optional)
- Fortran 90 implementation

---

*This changelog is automatically displayed in the AQUABC Shiny app header.*
