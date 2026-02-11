# AQUABC Changelog

All notable changes to the AQUABC model and Shiny frontend are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

---

## [Unreleased]

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
