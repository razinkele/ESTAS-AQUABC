---
title: "ESTAS-AQUABC Integration Guide"
author: "ESTAS-AQUABC Development Team"
date: "February 2026"
geometry: "margin=2.5cm"
fontsize: 11pt
toc: true
toc-depth: 3
numbersections: true
header-includes:
  - \usepackage{booktabs}
  - \usepackage{longtable}
  - \usepackage{float}
  - \usepackage{fancyhdr}
  - \pagestyle{fancy}
  - \fancyhead[L]{ESTAS-AQUABC Integration Guide}
  - \fancyhead[R]{\thepage}
  - \fancyfoot[C]{}
  - \usepackage{amsmath}
  - \usepackage{amssymb}
---

\newpage

# Introduction

This document describes how the ESTAS-II transport framework and the AQUABC ecological model are coupled into a single executable.  It covers the data flow at every interface boundary — from environmental forcing through pelagic kinetics to sediment diagenesis and back — and is intended for developers who need to modify, extend, or embed the coupled system.

## Scope

| Layer | Responsibility |
|:---|:---|
| **ESTAS-II** | Box-model topology, advection, dispersion, settling, mass loads, withdrawals, time integration, I/O |
| **AQUABC Pelagic** | Biogeochemical kinetics for 32 state variables (phytoplankton, zooplankton, nutrients, metals, redox, organic matter, dissolved oxygen, carbonate) |
| **AQUABC Sediment** | Multi-layer early diagenesis (24 state variables), bioturbation, bioirrigation, sediment--water fluxes |
| **Auxiliary modules** | Allelopathy (4 extra state variables), macroalgae (Droop quota), CO2SYS (carbonate equilibrium), exergy diagnostics |

The integration follows a **caller--callee** pattern: ESTAS owns the time loop and calls into AQUABC subroutines at well-defined interface points.

\newpage

# Call Graph Overview

The top-level execution flow is:

```
ESTAS_II (main program)
  |
  +-- READ_AQUATIC_MODEL_INPUTS          [ESTAS]
  |     +-- read INPUT.txt
  |     +-- ALLOCATE_PELAGIC_ECOLOGY     [ESTAS/AQUABC bridge]
  |     +-- INIT_WC_MODEL_CONSTANTS      [AQUABC]
  |     +-- INIT_BSED_MODEL_CONSTANTS    [AQUABC/ESTAS bridge]
  |     +-- read forcing time series
  |
  +-- RUN_SIMULATION                     [ESTAS]
  |     |
  |     +-- time loop (t = t_start ... t_end)
  |           |
  |           +-- SOLVE                  [ESTAS]
  |                 |
  |                 +-- UPDATE_TIME_FUNCS
  |                 |     +-- UPDATE_PELAGIC_DRIVING_FUNCS  [bridge]
  |                 |     +-- interpolate flows, boundary concs
  |                 |     +-- interpolate settling velocities
  |                 |     +-- interpolate prescribed sediment fluxes
  |                 |
  |                 +-- CALC_DERIV
  |                 |     +-- compute transport derivatives
  |                 |     +-- PELAGIC_KINETICS              [AQUABC]
  |                 |     +-- (if Mode 2) coupling block:
  |                 |           +-- FLX_ALUKAS_II_TO_SED_MOD_1_VEC [AQUABC]
  |                 |           +-- AQUABC_SEDIMENT_MODEL_1        [AQUABC]
  |                 |           +-- FLX_SED_MOD_1_TO_ALUKAS_II_VEC [AQUABC]
  |                 |
  |                 +-- Euler mass update
  |                 +-- write output
  |
  +-- cleanup / deallocate
```

\newpage

# Interface 1 — Driving Functions

ESTAS collects 10 environmental forcing values per box per time step and places them into an array that AQUABC reads as `DRIVING_FUNCTIONS(nkn, 10)`.

## Driving Function Mapping

| Index | Variable | Source | Units |
|:---:|:---|:---|:---|
| 1 | Water temperature | Temperature time series | $^\circ$C |
| 2 | Salinity | Salinity time series | PSU |
| 3 | Solar radiation (at box depth) | Solar radiation TS $\times$ Beer--Lambert attenuation through overlying boxes | W m$^{-2}$ |
| 4 | Fraction of day (photoperiod) | Fraction-of-day time series | dimensionless |
| 5 | Air temperature | Air temperature time series | $^\circ$C |
| 6 | Wind speed | Wind speed time series | m s$^{-1}$ |
| 7 | Surface elevation | Computed from box volume | m |
| 8 | Water-column depth | $V_i / A_i^s$ | m |
| 9 | Background light extinction | User-entered constant `K_B_E` | m$^{-1}$ |
| 10 | Ice cover fraction | Ice fraction time series | 0--1 |

### Light Attenuation Through Stacked Boxes

For vertically stacked boxes within a basin (box overlying another), solar radiation is attenuated using the saved light extinction coefficient from the box above:

$$I_{\text{box}} = I_{\text{surface}} \cdot \exp\!\left(-\sum_{k=1}^{n-1} K_{e,k} \cdot \Delta z_k\right)$$

where $K_{e,k}$ is the extinction coefficient stored in `SAVED_OUTPUTS(1)` of box $k$, and $\Delta z_k$ is the depth of box $k$.

## Flags

Five integer flags are passed per box:

| Index | Meaning |
|:---:|:---|
| 1 | Safe mode (always 0) |
| 2 | Surface box indicator (1 = surface, 0 = subsurface) |
| 3 | First time-step indicator (1 on first step, 0 thereafter) |
| 4 | Zooplankton option |
| 5 | Advanced redox simulation flag |

\newpage

# Interface 2 — Pelagic Kinetics

## The PELAGIC\_KINETICS Call

At each time step, after computing all transport derivatives, ESTAS calls:

```fortran
call PELAGIC_KINETICS(PELAGIC_BOX_MODEL_DATA, SEDIMENT_FLUXES, &
                      TIME, TIME_STEP, CALLED_BEFORE)
```

### Inputs Passed to AQUABC

| Array | Dimension | Description |
|:---|:---|:---|
| `STATE_VARIABLES(nkn, nstate)` | $n_{kn} \times 32$ | Current concentrations (g m$^{-3}$) |
| `DRIVING_FUNCTIONS(nkn, 10)` | $n_{kn} \times 10$ | Environmental forcing (see §3) |
| `MODEL_CONSTANTS(nconst)` | 318 | Pelagic kinetic and stoichiometric constants |
| `FLAGS(nflags)` | 5 | Control flags |
| `SAVED_OUTPUTS(nkn, 5)` | $n_{kn} \times 5$ | Outputs carried forward from previous step |
| `SEDIMENT_FLUXES(nkn, nstate)` | $n_{kn} \times 32$ | Prescribed sediment fluxes (Mode 1) or zero |
| `TIME` | scalar | Current simulation time (days) |
| `TIME_STEP` | scalar | Integration time step (days) |
| `CALLED_BEFORE` | integer | 0 on first call, 1 thereafter |

### Outputs Returned to ESTAS

| Array | Dimension | Description |
|:---|:---|:---|
| `DERIVATIVES(nkn, nstate)` | $n_{kn} \times 32$ | Kinetic reaction rates $R_{i,j}$ (g m$^{-3}$ day$^{-1}$) |
| `PROCESS_RATES(nkn, nstate, NDIAGVAR)` | $n_{kn} \times 32 \times 30$ | Diagnostic process rates |
| `SAVED_OUTPUTS(nkn, 5)` | $n_{kn} \times 5$ | Updated saved outputs (e.g.\ light extinction) |
| `pH(nkn)` | $n_{kn}$ | Computed pH per box |
| `CHLA(nkn)` | $n_{kn}$ | Chlorophyll-a per box (derived) |

## Integration of Kinetic Derivatives

The kinetic derivative is combined with sediment return fluxes and converted to a mass rate:

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{kin}} = \left(R_{i,j} + F_{i,j}^{\text{return}}\right) \cdot V_i$$

where $F_{i,j}^{\text{return}}$ is the sediment-to-water-column flux divided by depth (Mode 2) or zero.

\newpage

# Interface 3 — Settling and Deposition Fluxes

## Settling Velocity and Dissolved Fractions

ESTAS interpolates per-box, per-variable settling velocities and dissolved fractions from time series and passes them as:

- `SETTLING_VELOCITIES(nkn, nstate)` — m day$^{-1}$
- `DISSOLVED_FRACTIONS(nkn, nstate)` — dimensionless, 0--1
- `FRACTION_OF_DEPOSITION(nkn, nstate)` — fraction deposited vs.\ reflected
- `DEPOSITION_AREA_RATIOS(nkn, nstate)` — bottom area correction factor

## Water-Column to Sediment Flux Translation

The subroutine `FLX_ALUKAS_II_TO_SED_MOD_1_VEC` converts the 32 pelagic settling rates into 24 sediment deposition fluxes. This translation is necessary because the pelagic and sediment models use different state variable definitions.

### Variable Mapping (WC $\to$ Sediment)

| Sediment Variable | Index | Source WC Variables | Notes |
|:---|:---:|:---|:---|
| NH$_4$-N | 1 | NH$_4$-N | Direct |
| NO$_3$-N | 2 | NO$_3$-N | Direct |
| PO$_4$-P (particulate organic N) | 3 | Det. Part. Org. N | Direct |
| Dissolved Org. N | 4 | Diss. Org. N | Direct (small) |
| PO$_4$-P | 5 | PO$_4$-P | Direct |
| Det. Part. Org. P | 6 | Det. Part. Org. P | Direct |
| Dissolved Org. P | 7 | Diss. Org. P | Direct (small) |
| Dissolved O$_2$ | 8 | Diss. O$_2$ | Direct |
| Det. Part. Org. C | 9 | Det. Part. Org. C + dead phytoplankton C | Aggregated |
| Dissolved Org. C | 10 | Diss. Org. C | Direct (small) |
| Dissolved Si | 11 | Diss. Si | Direct |
| Particulate Si | 12 | Part. Si + diatom Si | Aggregated |
| Inorganic C (DIC) | 13 | Inorg. C | Direct |
| Total Alkalinity | 14 | Tot. Alk. | Direct |
| Salinity | 15 | Driving function \#2 | From forcing |
| Fe(II), Fe(III), Mn(II), Mn(IV) | 16--19 | Metals | Direct |
| Ca, Mg, S(+6), S(-2), CH$_4$ | 20--24 | Minerals, sulphur, methane | Direct |

The conversion handles the fundamental asymmetry: dead phytoplankton biomass (diatoms, cyanobacteria, other phytoplankton) in the water column is aggregated into detrital organic C, N, P in the sediment.

### Settling Suppression

A chlorophyll-a--dependent factor suppresses settling velocities during intense blooms:

$$f_{\text{supp}} = f(\text{Chl-a})$$

This is computed inside `FLX_ALUKAS_II_TO_SED_MOD_1` before applying the settling flux calculation.

### Not-Deposited Fluxes

Material that settles but is not deposited is "reflected" back to the water column:

$$F_{j}^{\text{not\_dep}} = J_{j}^{\text{settl}} \cdot (1 - f_{\text{dep},j})$$

These fluxes are divided by water column depth and added to the kinetic derivative.

\newpage

# Interface 4 — Sediment Diagenesis (Mode 2)

When `MODEL_BOTTOM_SEDIMENTS = 2`, the full AQUABC sediment diagenesis model is called inside `CALC_DERIV`.

## Pre-Call Setup

Before calling the sediment model, ESTAS performs:

1. **Temperature transfer**: sediment layer temperatures are set equal to water-column temperature (clamped to 0--45$^\circ$C):

$$T_{\text{sed},\ell} = \max(0, \min(45, T_{\text{WC}}))$$

2. **Surface water concentration mapping**: 24 dissolved concentrations at the sediment--water interface are extracted from the current pelagic state:

| Sediment Index | WC Source | Description |
|:---:|:---|:---|
| 1 | `NH4_N` | Ammonium nitrogen |
| 2 | `NO3_N` | Nitrate nitrogen |
| 3 | `DET_PART_ORG_N` | Particulate organic nitrogen |
| 4 | $10^{-10}$ | Placeholder (dissolved org. N in sediment) |
| 5 | `PO4_P` | Phosphate phosphorus |
| 6 | `DET_PART_ORG_P` | Particulate organic phosphorus |
| 7 | $10^{-10}$ | Placeholder (dissolved org. P in sediment) |
| 8 | `DISS_OXYGEN` | Dissolved oxygen |
| 9 | `DET_PART_ORG_C` | Particulate organic carbon |
| 10 | $10^{-10}$ | Placeholder (dissolved org. C in sediment) |
| 11 | `DISS_Si` | Dissolved silica |
| 12 | $10^{-10}$ | Placeholder (particulate Si in sediment) |
| 13 | `INORG_C` | Dissolved inorganic carbon |
| 14 | `TOT_ALK` | Total alkalinity |
| 15 | Driving function \#2 | Salinity |
| 16--17 | `FE_II`, `FE_III` | Iron species |
| 18--19 | `MN_II`, `MN_IV` | Manganese species |
| 20--21 | `CA`, `MG` | Calcium, magnesium |
| 22--23 | `S_PLUS_6`, `S_MINUS_2` | Sulphur species |
| 24 | `CH4_C` | Methane |

Indices 4, 7, 10, 12 correspond to sediment dissolved/particulate organic pools that have no direct pelagic equivalent. They are initialised to a very small value ($10^{-10}$) to avoid division-by-zero errors inside the sediment model.

3. **Erosion/deposition thickness**: currently set to zero (`H_ERODEP = 0.0`), assuming negligible net erosion at the sediment surface.

4. **Settling flux conversion**: `FLX_ALUKAS_II_TO_SED_MOD_1_VEC` translates pelagic settling rates to 24-variable sediment deposition fluxes.

## Sediment Model Call

```fortran
call AQUABC_SEDIMENT_MODEL_1 &
    (nkn, INIT_SED_STATE_VARS, SED_DEPTHS, SED_POROSITIES,    &
     SED_DENSITIES, PART_MIXING_COEFFS, SED_DIFFUSIONS,       &
     SURF_MIXLEN, SED_BURRIALS, SURF_WATER_CONCS, SED_TEMPS,  &
     NUM_SED_VARS, NUM_SED_LAYERS, SED_MODEL_CONSTANTS,       &
     NUM_SED_CONSTS, SED_DRIVING_FUNCTIONS, NUM_SED_DRIV,     &
     SED_FLAGS, NUM_SED_FLAGS, FLUXES_TO_SEDIMENTS,           &
     NUM_FLUXES_TO_SEDIMENTS, NUM_FLUX_RECEIVING_SED_LAYERS,  &
     ADVECTIVE_VELOCITY, TIME, TIME_STEP, H_ERODEP,           &
     FINAL_SED_STATE_VARS, FLUXES_FROM_SEDIMENTS,             &
     NUM_FLUXES_FROM_SEDIMENTS, PROCESSES_sed, NDIAGVAR_sed,  &
     SED_OUTPUTS, NUM_SED_OUTPUTS, SED_SAVED_OUTPUTS,         &
     NUM_SED_SAVED_OUTPUTS, SED_BURRIAL_RATE_OUTPUTS,         &
     BOTTOM_SED_ADVANCED_REDOX_SIMULATION)
```

### Key Input Arrays

| Array | Shape | Description |
|:---|:---|:---|
| `INIT_SED_STATE_VARS` | (nkn, layers, 24) | Initial sediment concentrations |
| `SED_DEPTHS` | (nkn, layers) | Layer thicknesses (m) |
| `SED_POROSITIES` | (nkn, layers) | Volume fraction of pore water |
| `SED_DENSITIES` | (nkn, layers) | Bulk wet density (kg m$^{-3}$) |
| `PART_MIXING_COEFFS` | (nkn, layers, 24) | Particle mixing / bioturbation (m$^2$ day$^{-1}$) |
| `SED_DIFFUSIONS` | (nkn, layers, 24) | Molecular + eddy diffusion (m$^2$ day$^{-1}$) |
| `SED_BURRIALS` | (nkn, layers) | Burial rates (m day$^{-1}$) |
| `FLUXES_TO_SEDIMENTS` | (nkn, 24) | Deposition fluxes from settling (g m$^{-2}$ day$^{-1}$) |
| `SED_MODEL_CONSTANTS` | (171) | Sediment reaction constants |

### Key Output Arrays

| Array | Shape | Description |
|:---|:---|:---|
| `FINAL_SED_STATE_VARS` | (nkn, layers, 24) | Updated concentrations |
| `FLUXES_FROM_SEDIMENTS` | (nkn, 30) | Return fluxes to water column |
| `PROCESSES_sed` | (nkn, 24, layers, 25) | Diagnostic process rates |
| `SED_OUTPUTS` | (nkn, layers, 26) | Additional outputs |
| `SED_BURRIAL_RATE_OUTPUTS` | (nkn, layers, 24) | Burial rate diagnostics |

## Post-Call Processing

After the sediment model returns:

1. **Negative clamping**: any negative sediment concentrations are set to zero.

2. **Flux back-translation**: `FLX_SED_MOD_1_TO_ALUKAS_II_VEC` maps the 30 sediment return fluxes back to the 32 pelagic state variables (30 mapped, 2 set to zero for living organisms that do not diffuse from sediment).

3. **Unit conversion**: return fluxes are divided by water-column depth to convert from g m$^{-2}$ day$^{-1}$ to g m$^{-3}$ day$^{-1}$:

$$F_{i,j}^{\text{return}} = \frac{F_j^{\text{sed}\to\text{WC}}}{z_i}$$

4. **State propagation**: `FINAL_SED_STATE_VARS` replaces `INIT_SED_STATE_VARS` for the next time step.

## Sediment-to-Water Flux Mapping

The return mapping (`FLX_SED_MOD_1_TO_ALUKAS_II`) translates 30 sediment flux variables back to pelagic indices:

| WC Flux Index | Sediment Flux Index | Variable |
|:---:|:---:|:---|
| 1 | 1 | NH$_4$-N |
| 2 | 2 | NO$_3$-N |
| 3 | 5 | PO$_4$-P |
| 4 | 8 | Dissolved O$_2$ |
| 5--8 | — | Diatom C, Zooplankton C/N/P (zero — no living organism flux) |
| 9--11 | — | Detrital Part. Org. C/N/P (zero — remains in sediment) |
| 12 | 9 | Dissolved Org. C |
| 13 | 3 | Dissolved Org. N |
| 14 | 6 | Dissolved Org. P |
| 15--16 | — | Cyanobacteria, Other Phyto (zero) |
| 17 | 11 | Dissolved Si |
| 18 | 12 | Particulate Si |
| 19 | — | N$_2$-fixing Cyan. (zero) |
| 20 | 13 | Inorganic C (DIC) |
| 21 | 14 | Total Alkalinity |
| 22--25 | 16--19 | Fe(II), Fe(III), Mn(II), Mn(IV) |
| 26--30 | 20--24 | Ca, Mg, S(+6), S(-2), CH$_4$ |

\newpage

# Interface 5 — Allelopathy

The allelopathy module adds 4 state variables (indices 33--36) appended after the standard 32.  These are:

- Produced by phytoplankton groups proportional to their biomass
- Transported by the same advection--dispersion--settling equations
- Fed into `PELAGIC_KINETICS` which applies inhibition to susceptible groups

ESTAS handles allelopathy transparently: all transport arrays are dimensioned `(nkn, nstate + NUM_ALLOLOPATHY_STATE_VARS)`, so the 4 extra variables ride the same transport infrastructure with no additional coupling code.

\newpage

# Interface 6 — Model Constants

## Pelagic Constants

318 pelagic constants are initialised by `INIT_WC_MODEL_CONSTANTS` (AQUABC module) and stored in `MODEL_CONSTANTS(318)`.  ESTAS can optionally override them from an external file specified as command-line argument 2 (`WCONST_*.txt`).

The override mechanism:

1. AQUABC fills defaults via `INIT_WC_MODEL_CONSTANTS`
2. If `USE_PELAGIC_CONSTANTS_FILE_NAME = 1`, ESTAS reads the override file and replaces matching entries

## Sediment Constants

171 sediment constants are initialised by `INIT_BSED_MODEL_CONSTANTS` (in `mod_BOTTOM_SEDIMENTS.f90`) and optionally supplemented by `EXTRA_WCONST.txt`.  The `isedi` parameter (required by the sediment model) is registered via SHYFEM's parameter system:

```fortran
call para_insert_value('isedi', 0.0D0)
```

This ensures the sediment model can query `isedi` even when running in standalone ESTAS mode without SHYFEM.

\newpage

# Data Flow Diagram

The complete data flow per time step is:

```
  Time Series Files          ESTAS Transport
  ================          ================
  Temperature   ------+
  Salinity      ------+---> DRIVING_FUNCTIONS(nkn, 10)
  Solar rad.    ------+           |
  Wind speed    ------+           |
  ...           ------+           v
                           +------------------+
  Flows     -------------> |  UPDATE_TIME_FUNCS|---> Transport derivatives
  Boundaries  ----------->  |  (advection,     |     (7 components)
  Mass loads  ----------->  |   dispersion,    |
                           |   settling, ...)  |
                           +------------------+
                                  |
                                  v
                           +------------------+
  MODEL_CONSTANTS(318) --> | PELAGIC_KINETICS | --> DERIVATIVES(nkn,32)
  STATE_VARIABLES -------> |    (AQUABC)      | --> PROCESS_RATES
  FLAGS -----------------> |                  | --> SAVED_OUTPUTS
  SAVED_OUTPUTS ---------> |                  | --> pH, CHLA
                           +------------------+
                                  |
                     (if MODE 2)  |
                                  v
                    +-----------------------------+
  Settling rates -> | FLX_ALUKAS_II_TO_SED_MOD_1 | --> FLUXES_TO_SEDIMENTS(nkn,24)
                    +-----------------------------+
                                  |
                                  v
                    +-----------------------------+
  SED_DEPTHS -----> | AQUABC_SEDIMENT_MODEL_1    | --> FINAL_SED_STATE_VARS
  SED_POROSITIES -> |   (24 vars x N layers)     | --> FLUXES_FROM_SEDIMENTS(nkn,30)
  SED_DENSITIES --> |                             | --> SED_OUTPUTS, PROCESSES_sed
  PART_MIXING ----> |   Bioturbation, diffusion,  |
  SED_TEMPS ------> |   burial, kinetics, pH     |
                    +-----------------------------+
                                  |
                                  v
                    +-----------------------------+
                    | FLX_SED_MOD_1_TO_ALUKAS_II | --> FLUXES_TO_WATER_COLUMN(nkn,32)
                    +-----------------------------+
                                  |
                                  v
                    +-----------------------------+
                    |      EULER UPDATE           |
                    |  M(n+1) = M(n) + dM/dt*dt  |
                    |  C(n+1) = M(n+1) / V(n+1)  |
                    +-----------------------------+
```

\newpage

# Bioturbation Integration

The bioturbation module (`aquabc_II_sediment_bioturbation.f90`) was added in v0.2 and integrates at the sediment model level.  Key integration points:

## Within the Sediment Model

The sediment model calls eight bioturbation functions to compute depth- and temperature-dependent mixing coefficients:

| Function | Purpose |
|:---|:---|
| `compute_bioturbation_Db` | Gaussian depth profile $D_b(z)$ scaled by temperature |
| `compute_Db_temperature_factor` | $Q_{10}$ temperature correction |
| `compute_bioirrigation_alpha` | Exponential depth profile $\alpha(z)$ for bioirrigation |
| `compute_bioirrigation_enhancement` | Non-local exchange enhancement factor |
| `compute_dynamic_bulk_Db` | Combine baseline $D_b$ with fauna-activity scaling |
| `compute_dynamic_dissolved_Db` | Pore-water component of bioturbation |
| `apply_lower_boundary_bio` | Ensure smooth decay at domain base |
| `compute_Db_at_depth` | Point evaluation for arbitrary depth |

## Particle Mixing Coefficients

The `PART_MIXING_COEFFS(nkn, layers, 24)` array, read from `BOTTOM_SEDIMENT_MODEL_INPUT.txt` and managed by ESTAS, provides baseline values.  When bioturbation is active (`switch_partmixing > 0`), the sediment model dynamically overrides these with depth- and temperature-dependent values computed by the bioturbation module.

## Bioirrigation

Bioirrigation enhances pore-water transport by multiplying the effective diffusion coefficient.  This is handled entirely within the sediment model and is transparent to ESTAS.

\newpage

# Saved Outputs and State Propagation

## Pelagic Saved Outputs

Five values are preserved across time steps per box:

| Index | Variable | Usage |
|:---:|:---|:---|
| 1 | `FE_II_DISS_OVER_FE_II` | Iron(II) dissolved fraction |
| 2 | `FE_III_DISS_OVER_FE_III` | Iron(III) dissolved fraction |
| 3 | `MN_II_DISS_OVER_MN_II` | Manganese(II) dissolved fraction |
| 4 | `MN_IV_DISS_OVER_MN_IV` | Manganese(IV) dissolved fraction |
| 5 | `DIP_OVER_IP` | Dissolved inorganic P fraction |

These are computed by `PELAGIC_KINETICS` and fed back on the next call.  The light extinction coefficient, used for Beer--Lambert attenuation through stacked boxes, is also derived internally.

## Sediment State Propagation

After each sediment model call:

$$\texttt{INIT\_SED\_STATE\_VARS}^{n+1} = \texttt{FINAL\_SED\_STATE\_VARS}^n$$

This ensures the sediment layers carry forward between time steps.

## Called-Before Flag

The integer `CALLED_BEFORE` is 0 on the first invocation and 1 thereafter.  AQUABC uses this to perform one-time initialisations (e.g.\ computing initial pH from alkalinity and DIC).

\newpage

# Output Integration

## Pelagic Output

ESTAS writes per-box concentrations at `PRINT_INTERVAL` intervals.  The output columns combine ESTAS transport information with AQUABC-computed values:

| Columns | Content | Source |
|:---|:---|:---|
| 1 | Time | ESTAS |
| 2 | Box number | ESTAS |
| 3--34 | 32 pelagic state variables | AQUABC (via ESTAS transport) |
| 35--38 | 4 allelopathic variables | AQUABC/Allelopathy |

## Sediment Output (Mode 2)

Three output files are written per time step:

1. **Sediment concentrations** — all 24 variables $\times$ all layers $\times$ all boxes
2. **Sediment fluxes to water column** — 30 flux variables per box (format: `36F20.10`)
3. **Sediment burial rates** — optional COCOA-format output

## COCOA Extended Output

When `PRODUCE_COCOA_OUTPUTS > 0`, additional files are produced:

- Pelagic process rates (all diagnostic variables)
- Sediment process rates per layer
- Bidirectional flux time series

\newpage

# Adding a New Coupled Variable

To add a new state variable that is transported by ESTAS and reacted by AQUABC:

## Pelagic Side

1. Increment `nstate` in `mod_GLOBAL.f90`.
2. Add the variable index to `aquabc_pel_state_var_indexes.f90`.
3. Add kinetic equations in `aquabc_II_pelagic_kinetics.f90`.
4. Add initial conditions and any new constants.

## ESTAS Side

5. Update settling velocity and dissolved fraction arrays (they auto-dimension from `nstate`).
6. Add the variable to output column mapping in `sub_WRITE_PELAGIC_OUTPUT.f90`.
7. If the variable interacts with sediments, update the flux translation routines:
   - `FLX_ALUKAS_II_TO_SED_MOD_1` (WC $\to$ sediment)
   - `FLX_SED_MOD_1_TO_ALUKAS_II` (sediment $\to$ WC)

## Sediment Side (if applicable)

8. Increment `NUM_SED_VARS` in `mod_GLOBAL.f90`.
9. Add the variable to `BOTTOM_SEDIMENT_MODEL_INPUT.txt`.
10. Add kinetic reactions in the appropriate sediment library file.

\newpage

# Embedding in External Frameworks

ESTAS-AQUABC can be embedded in larger hydrodynamic frameworks (e.g.\ SHYFEM).  The key interface contract:

| Responsibility | Standalone ESTAS | Embedded in SHYFEM |
|:---|:---|:---|
| Box topology | Read from files | Provided by FE mesh |
| Flows | Time series interpolation | Computed by hydrodynamics |
| Dispersion | Time series interpolation | Computed by turbulence model |
| Driving functions | Time series interpolation | From hydrodynamic solution |
| Time stepping | ESTAS Euler loop | SHYFEM time loop calls AQUABC |
| `isedi` parameter | Registered in `INIT_BSED_MODEL_CONSTANTS` | Registered by SHYFEM |

The ecological model (AQUABC) is agnostic to the transport provider — it only sees `STATE_VARIABLES`, `DRIVING_FUNCTIONS`, `MODEL_CONSTANTS`, and `FLAGS`.

\newpage

# References

- AQUABC v0.3 Reference Manual (companion document)
- ESTAS-II Reference Manual (companion document)
- Boudreau, B.P. (1997). *Diagenetic Models and Their Implementation*. Springer.
- Chapra, S.C. (1997). *Surface Water-Quality Modeling*. McGraw-Hill.
- Soetaert, K., Herman, P.M.J., and Middelburg, J.J. (1996). A model of early diagenetic processes. *Geochim. Cosmochim. Acta* 60, 1019--1040.
