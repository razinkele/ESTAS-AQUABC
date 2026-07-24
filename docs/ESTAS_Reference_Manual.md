---
title: "ESTAS-II — Transport Framework Reference Manual"
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
  - \fancyhead[L]{ESTAS-II Reference Manual}
  - \fancyhead[R]{\thepage}
  - \fancyfoot[C]{}
  - \usepackage{amsmath}
  - \usepackage{amssymb}
---

\newpage

# Introduction

ESTAS-II (Ecological-System Transport and Aggregated Simulation, version II) is a one-dimensional box-model transport framework written in Fortran 90/95.  It provides the hydrodynamic "scaffolding" — advection, dispersion, settling, mass loads, withdrawals, and sediment fluxes — around the AQUABC ecological model, enabling multi-box simulations of lakes, reservoirs, estuaries, and coastal waters.

ESTAS-II can operate standalone (reading all forcing from text files) or be embedded as a library inside larger hydrodynamic codes such as SHYFEM.

## Key Features

- Arbitrary number of well-mixed boxes connected by advective and dispersive links
- Forward-Euler mass-balance solver with sub-daily time steps
- Supports three sediment coupling modes: off, prescribed fluxes, or full diagenesis
- Allelopathic interactions among phytoplankton groups
- Macroalgae and pelagic exergy diagnostics
- Resuspension modelling with critical shear stress thresholds
- Text and binary output formats; optional COCOA project-style outputs
- Command-line configurability for input file, constant overrides, binary output, and shear stress table

\newpage

# Program Architecture

## Execution Flow

The main program `ESTAS_II` performs the following steps:

1. **Parse command-line arguments** — determine input file, optional constant file, binary output file, and shear-stress file.
2. **Read configuration** — `READ_AQUATIC_MODEL_INPUTS` loads `INPUT.txt` and all referenced sub-files (bathymetry, forcing time series, initial conditions, etc.).
3. **Run simulation** — `RUN_SIMULATION` executes the main time loop.
4. **Clean up** — deallocate pelagic and sediment arrays, close output units.

## Module Organisation

| Module / File | Purpose |
|:---|:---|
| `ESTAS_II.f90` | Main program: argument parsing, init, run, cleanup |
| `mod_GLOBAL.f90` | Global constants, dimensions (`nkn`, `nstate = 32`, sediment dims), allocatable arrays for state variables, derivatives, driving functions, flags, process rates, sediment variables |
| `mod_AQUATIC_MODEL.f90` | `AQUATIC_MODEL_DS` derived type aggregating all sub-model data; `READ_AQUATIC_MODEL_INPUTS` master reader |
| `mod_SIMULATE.f90` | `RUN_SIMULATION` — outer repeat loop, inner time-step loop, output writing, cost-function evaluation |
| `mod_SOLVER.f90` | `PELAGIC_SOLVER` — `SOLVE` routine: time-function update, `CALC_DERIV`, Euler mass update, sediment integration |
| `mod_PELAGIC_BOX_MODEL.f90` | `PELAGIC_BOX_MODEL_DS` derived type: boxes, links, boundary conditions, derivative arrays |
| `mod_PELAGIC_BOX.f90` | `PELAGIC_BOX_DS` — per-box data: volume, areas, concentrations, masses |
| `mod_PELAGIC_LINK.f90` | Advective and dispersive link data structures |
| `mod_PELAGIC_ECOLOGY.f90` | Interface to AQUABC pelagic kinetics (`PELAGIC_KINETICS` call) |
| `mod_BOTTOM_SEDIMENTS.f90` | Sediment model initialisation: constant loading, `isedi` parameter, allocation |
| `mod_INITIAL_CONDITIONS.f90` | Reading initial concentrations from files |
| `mod_INITIALIZE_PELAGIC_BOX_MODEL.f90` | Construction of box-model topology from input data |
| `mod_INITIALIZE_AQUATIC_MODEL.f90` | High-level initialisation orchestrator |
| `mod_TIME_SERIES.f90` | `TIME_SERIE` type and interpolation for forcing data |
| `mod_INTERPOLATE.f90` | General interpolation utilities |
| `mod_MASS_LOAD.f90` | Mass-load and withdrawal handling |
| `mod_RESUSPENSION.f90` | Resuspension algorithms: shear stress, critical thresholds |
| `mod_BASIN.f90` | Basin geometry helpers |
| `mod_PELAGIC_EXERGY.f90` | Eco-exergy computation |
| `mod_COST_FUNCTION.f90` | Obs-vs-model cost function |
| `mod_UTILS_01.f90` | General utility routines |
| `sub_READ_PELAGIC_INPUTS.f90` | Read pelagic forcing files |
| `sub_READ_BOTTOM_SEDS_FLUXES_INPUTS.f90` | Read prescribed sediment flux files |
| `sub_WRITE_PELAGIC_OUTPUT.f90` | ASCII pelagic output writer |
| `sub_WRITE_PELAGIC_BINARY_OUTPUT.f90` | Binary pelagic output |
| `sub_WRITE_PELAGIC_MEM_OUTPUT.f90` | In-memory pelagic output |
| `sub_WRITE_PELAGIC_MEM_BINARY_OUTPUT.f90` | In-memory binary pelagic output |
| `sub_WRITE_PELAGIC_MEM_SAVED_OUTPUT.f90` | Saved-state output |
| `sub_WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT.f90` | Saved-state binary output |
| `sub_WRITE_PELAGIC_EX_MEM_OUTPUT.f90` | Exergy output |
| `sub_WRITE_PELAGIC_EX_MEM_BINARY_OUTPUT.f90` | Binary exergy output |

\newpage

# Global Dimensions and Constants

The `mod_GLOBAL` module defines the fixed dimensions of the coupled system:

| Parameter | Value | Description |
|:---|:---:|:---|
| `nstate` | 32 | Pelagic state variables |
| `nconst` | 318 | Pelagic model constants |
| `n_driving_functions` | 10 | Environmental driving functions |
| `nflags` | 5 | Pelagic flag variables |
| `n_saved_outputs` | 5 | Saved pelagic outputs carried across time steps |
| `NDIAGVAR` | 30 | Pelagic diagnostic (process-rate) variables |
| `NUM_SED_VARS` | 24 | Sediment state variables |
| `NUM_SED_CONSTS` | 171 | Sediment model constants |
| `NUM_SED_DRIV` | 1 | Sediment driving functions |
| `NUM_FLUXES_TO_SEDIMENTS` | 24 | Water-column $\to$ sediment flux variables |
| `NUM_FLUXES_FROM_SEDIMENTS` | 30 | Sediment $\to$ water-column flux variables |
| `NDIAGVAR_sed` | 25 | Sediment diagnostic variables |
| `NUM_SED_OUTPUTS` | 26 | Sediment output variables |
| `NUM_SED_FLAGS` | 3 | Sediment model flags |
| `NUM_SED_SAVED_OUTPUTS` | 5 | Sediment saved outputs |
| `NUM_ALLOLOPATHY_STATE_VARS` | 4 | Allelopathic state variables (appended to pelagic) |

The number of boxes (`nkn`) is determined at runtime from the box-model input file.

\newpage

# Input Configuration

## Master Input File (INPUT.txt)

The simulation is driven by a sequential text file read on Fortran unit 10.  Lines beginning with `#` are treated as comments.  The file is read record-by-record in the following order:

| Record | Example | Description |
|:---|:---|:---|
| 1--5 | `# DESCRIPTION ...` | Header / description lines (5 lines) |
| 6 | `1998` | Base year |
| 7 | `6209.0` | Simulation start time (days since reference) |
| 8 | `6574.0` | Simulation end time (days) |
| 9 | `1` | Number of repeat cycles |
| 10 | `240` | Time steps per day |
| 11 | `10` | Print interval (in time steps) |
| 12 | `INPUTS/` | Pelagic model input folder |
| 13 | `PELAGIC_INPUTS.txt` | Pelagic model input file name |
| 14 | `OUTPUTS/` | Pelagic model output folder |
| 15 | `2` | Resuspension option (0 = off) |
| 16 | `0` | `MODEL_BOTTOM_SEDIMENTS` (0 = off, 1 = prescribed, 2 = diagenesis) |
| 17 | `2` | Number of prescribed sediment flux sets |
| 18+ | filenames | Prescribed sediment flux file names (one per set) |

Additional records follow for sediment model input when `MODEL_BOTTOM_SEDIMENTS = 2`, COCOA options, and other advanced settings.

## Pelagic Input Files

The pelagic input folder must contain the file named in record 13 (typically `PELAGIC_INPUTS.txt`), which in turn references:

- **Box-model topology** — number of boxes (`nkn`), volumes, surface and bottom areas
- **Advective links** — `ADVECTIVE_LINKS.txt`: upstream/downstream box pairs and flow time-series references
- **Dispersive links** — `DISPERSIVE_LINKS.txt`: box pairs, dispersion coefficients, interface areas
- **Flow time series** — `FLOW_TS.txt` and individual `FORC_TS_*.txt` files
- **Boundary conditions** — open-boundary concentrations per state variable
- **Initial conditions** — per-box starting concentrations (files or inline)
- **Forcing time series** — air temperature, cloud cover, evaporation, wind speed, solar radiation
- **Bathymetry** — `BATHYMETRY_*.txt` (one per box): depth--area--volume profiles
- **Mass loads** — tributary inflows with concentrations
- **Mass withdrawals** — outflow points

## Sediment Input Files

### Prescribed Fluxes (Mode 1)

When `MODEL_BOTTOM_SEDIMENTS = 1`, the files listed after record 17 provide time-varying fluxes $F_j(t)$ for each state variable, read by `sub_READ_BOTTOM_SEDS_FLUXES_INPUTS.f90`.

### Full Diagenesis (Mode 2)

When `MODEL_BOTTOM_SEDIMENTS = 2`, ESTAS reads additional input:

- `BOTTOM_SEDIMENT_MODEL_INPUT.txt` — layer geometry (depths, porosities, densities), particle mixing coefficients, diffusion coefficients, burial rates, initial sediment concentrations, sediment model constants
- `EXTRA_WCONST.txt` — extra water-column constants for sediment coupling

## Resuspension Input

When `RESUSPENSION_OPTION > 0`:

- `CRITICAL_SHEAR_STRESSES.txt` — per-box critical shear stress $\tau_c$ values
- Shear stress and resuspension concentration time series files

\newpage

# Transport Model

## Box-Model Formulation

ESTAS represents the water body as `nkn` well-mixed boxes.  Each box $i$ has a time-varying volume $V_i(t)$, surface area $A_i^s$, and bottom area $A_i^b$.  The mass of state variable $j$ in box $i$ is:

$$M_{i,j} = C_{i,j} \cdot V_i$$

where $C_{i,j}$ is the concentration (g m$^{-3}$).

## Mass-Balance Equation

The total time derivative of mass in box $i$ for state variable $j$ is the sum of seven components:

$$\frac{dM_{i,j}}{dt} = \left(\frac{dM}{dt}\right)_{\text{adv}} + \left(\frac{dM}{dt}\right)_{\text{disp}} + \left(\frac{dM}{dt}\right)_{\text{settl}} + \left(\frac{dM}{dt}\right)_{\text{load}} + \left(\frac{dM}{dt}\right)_{\text{withdr}} + \left(\frac{dM}{dt}\right)_{\text{kin}} + \left(\frac{dM}{dt}\right)_{\text{sed}}$$

### Advection

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{adv}} = \sum_{k \in \text{in}(i)} Q_k \, C_{k,j}^{\text{up}} - \sum_{k \in \text{out}(i)} Q_k \, C_{i,j}$$

where $Q_k$ is the flow rate through advective link $k$, and $C_{k,j}^{\text{up}}$ is the upstream concentration (or boundary concentration for open boundaries).

### Dispersion

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{disp}} = \sum_{k} \frac{D_k \, A_k^{\text{ifc}}}{L_k} \left(C_{\text{nbr},j} - C_{i,j}\right)$$

where $D_k$ is the dispersion coefficient (m$^2$ s$^{-1}$), $A_k^{\text{ifc}}$ is the interface area (m$^2$), and $L_k$ is a characteristic mixing length.

### Settling

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{settl}} = -w_{s,j} \, A_i^b \, f_{\text{part},j} \, C_{i,j}$$

where $w_{s,j}$ is the settling velocity (m s$^{-1}$), $A_i^b$ is the bottom area, and $f_{\text{part},j}$ is the particulate fraction $(1 - f_{\text{diss},j})$.

### Mass Loads

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{load}} = \sum_{\ell} Q_\ell \, C_{\ell,j}^{\text{load}}$$

where $Q_\ell$ and $C_{\ell,j}^{\text{load}}$ are the flow and concentration of load source $\ell$.

### Mass Withdrawals

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{withdr}} = -\sum_{w} Q_w \, C_{i,j}$$

### Kinetics (AQUABC)

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{kin}} = V_i \cdot R_{i,j}$$

where $R_{i,j}$ is the reaction rate from the AQUABC pelagic kinetics model (g m$^{-3}$ day$^{-1}$, converted to seconds internally).

### Prescribed Sediment Fluxes

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{sed}} = A_i^b \cdot F_j^{\text{sed}}(t)$$

where $F_j^{\text{sed}}(t)$ is the prescribed areal flux (g m$^{-2}$ s$^{-1}$), active only in Mode 1.

### Derivative Array Naming Convention

The seven mass-balance components above correspond to the following arrays in `mod_SOLVER.f90`, which are summed to form `tot_deriv`:

| Mass-Balance Term | Code Array | Paper Eq. 1 Equivalent |
|:---|:---|:---|
| Advection | `ECOL_ADVECTION_DERIVS` | Inflow from neighbours + outflow to neighbours |
| Dispersion | `ECOL_DISPERSION_DERIVS` | Diffusion term |
| Settling | `ECOL_SETTLING_DERIVS` | Settling from/to boxes |
| Mass loads | `ECOL_MASS_LOAD_DERIVS` | Part of boundary forcing |
| Withdrawals | `ECOL_MASS_WITHDRAWAL_DERIVS` | Part of boundary forcing |
| Kinetics | `ECOL_KINETIC_DERIVS` | Kinetics (AQUABC) |
| Sediment fluxes | `ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS` | Not explicitly separated in paper |

> **Note:** The paper (Ertürk et al., 2023, Eq. 1) combines mass loads and withdrawals into a single \"boundary forcing\" term and splits settling into receiving/losing components. The code treats them as separate arrays.

\newpage

# Time Integration

## Forward Euler Scheme

ESTAS uses a simple forward-Euler (explicit) scheme.  At each time step $\Delta t$:

$$M_{i,j}^{n+1} = M_{i,j}^{n} + \frac{dM_{i,j}}{dt}\bigg|^{n} \cdot \Delta t$$

$$V_i^{n+1} = V_i^{n} + \frac{dV_i}{dt}\bigg|^{n} \cdot \Delta t$$

$$C_{i,j}^{n+1} = \frac{M_{i,j}^{n+1}}{V_i^{n+1}}$$

The solver ID (`PELAGIC_SOLVER_NO`, internal to `mod_SIMULATE.f90`) selects the scheme; it is set at
run start from the `ESTAS_PELAGIC_SOLVER` environment variable (unset or `1` → Forward Euler, the
default; `2` → RK2/Heun below; any other value stops the run with an error). Setting
`PELAGIC_SOLVER_NO = 2` activates the **RK2 (Heun's method)** solver, which is fully implemented in
`mod_SOLVER.f90` (the `PELAGIC_SOLVER_NO == 2` branch of `SOLVE`).

### RK2 (Heun's Method) — experimental

When `PELAGIC_SOLVER_NO = 2`, ESTAS uses Heun's two-stage predictor--corrector method:

**Stage 1 (predictor):**
$$\tilde{M}_{i,j}^{n+1} = M_{i,j}^{n} + \frac{dM_{i,j}}{dt}\bigg|^{n} \cdot \Delta t$$

**Stage 2 (corrector):** Recalculate derivatives (and time-dependent forcing) at the predicted state
and time $t+\Delta t$, then average:
$$M_{i,j}^{n+1} = M_{i,j}^{n} + \frac{1}{2}\left(\frac{dM_{i,j}}{dt}\bigg|^{n} + \frac{dM_{i,j}}{dt}\bigg|^{\text{pred}}\right) \cdot \Delta t$$

The box volume is advanced with the matching 0.5·(v1+v2)·$\Delta t$ average, so that
$C_{i,j}^{n+1} = M_{i,j}^{n+1}/V_i^{n+1}$ stays consistent with the mass update. The RK2 solver
includes negative mass handling, concentration clamping, and diagnostic messages identical to the
Euler solver, and is stable at the step sizes ESTAS is normally run at.

**Status: experimental, not a faster or more accurate default.** RK2 is a correctly implemented,
stable Heun method, but for this model its *output* (concentration) converges at only ~1st order, not
the 2nd order the two-stage scheme would otherwise give — dominated by the `MIN_CONCENTRATION`
positivity clamp described below, a non-smooth operation that caps the achievable order regardless of
solver. At equal computational cost (2× the derivative evaluations per step) it does not outperform
Euler. It is exposed via `ESTAS_PELAGIC_SOLVER=2` for experimentation, not as a recommended
alternative to the default Euler scheme.

## Stability and Safety

- **Minimum concentration clamp**: if $C_{i,j} <$ `MIN_CONCENTRATION`, it is reset to `MIN_CONCENTRATION` and the mass recomputed.
- **Maximum concentration clamp**: if $C_{i,j} > 10^{10}$, the simulation is halted.
- **Negative mass warning**: diagnostic messages are printed whenever $M_{i,j}^{n+1} < 0$.
- **Time step**: the user sets `TIME_STEPS_PER_DAY` in `INPUT.txt`.  Typical stable values range from 48 to 480.

## Simulation Control

- **Repeat cycles**: the simulation can be repeated `NUM_REPEATS` times, useful for spin-up.
- **Print interval**: output is written every `PRINT_INTERVAL` time steps.

### Spin-Up (Repeat Cycle) Mechanism

When `NUM_REPEATS > 1`, each cycle after the first:

1. **Time is reset** to `SIMULATION_START` (forcing time series indices are also reset to the beginning).
2. **State variables are preserved** — the final concentrations from the previous cycle become the initial conditions for the next cycle.
3. **Output continues appending** to the same output files (the output time is offset by the cycle number).
4. **Sediment state** (if Mode 2) is also carried forward.

This allows the model to "spin up" by repeating the same forcing period until the state variables converge.

\newpage

# Sediment Coupling Modes

ESTAS supports three modes of sediment--pelagic interaction, controlled by `MODEL_BOTTOM_SEDIMENTS`:

## Mode 0 — No Sediments

No sediment fluxes are applied.  Only pelagic processes contribute to the kinetic derivative.

## Mode 1 — Prescribed Sediment Fluxes

Time-varying fluxes per state variable are read from external files.  Each flux set provides $F_j(t)$ values that are interpolated to the current time and applied as:

$$\left(\frac{dM_{i,j}}{dt}\right)_{\text{sed}} = A_i^b \cdot F_j^{\text{sed}}(t)$$

Multiple flux sets can be active simultaneously (e.g.\ sandy-sediment fluxes and muddy-sediment fluxes in different spatial regions).

## Mode 2 — Full Diagenesis (AQUABC Sediment Model)

The AQUABC sediment diagenesis model is called at each time step for each box.  This mode:

1. Passes settling fluxes from the water column as upper boundary conditions
2. Solves the multi-layer sediment equations (24 state variables $\times$ `NUM_SED_LAYERS` layers)
3. Returns dissolved fluxes back to the water column (30 flux variables)

The returned fluxes replace the prescribed sediment flux term in the mass balance.  See the *AQUABC Reference Manual* for full details of the diagenesis equations, bioturbation, and bioirrigation.

### Sediment Arrays

In Mode 2, the following 3-D arrays are allocated per box, layer, and variable:

- `INIT_SED_STATE_VARS(nkn, NUM_SED_LAYERS, NUM_SED_VARS)` — initial conditions
- `FINAL_SED_STATE_VARS(nkn, NUM_SED_LAYERS, NUM_SED_VARS)` — end-of-step values
- `FLUXES_FROM_SEDIMENTS(nkn, NUM_FLUXES_FROM_SEDIMENTS)` — 30 return fluxes
- `FLUXES_TO_SEDIMENTS(nkn, NUM_FLUXES_TO_SEDIMENTS)` — 24 settling deposit fluxes
- `SED_OUTPUTS(nkn, NUM_SED_LAYERS, NUM_SED_OUTPUTS)` — diagnostic outputs

\newpage

# Settling and Deposition

## Settling Velocity

Each state variable has a settling velocity $w_{s,j}$ and a dissolved fraction $f_{\text{diss},j}$.  The effective particulate settling flux from box $i$ is:

$$J_{i,j}^{\text{settl}} = w_{s,j} \, (1 - f_{\text{diss},j}) \, C_{i,j}$$

## Deposition to Sediments

The fraction of settled material that deposits on the bottom (as opposed to being resuspended or being laterally transported) is controlled by the *effective deposition fraction* $f_{\text{dep},j}$ and the *deposition area ratio* $r_{\text{dep},j}$:

$$F_{i,j}^{\text{dep}} = J_{i,j}^{\text{settl}} \cdot f_{\text{dep},j} \cdot r_{\text{dep},j} \cdot A_i^b$$

This deposited flux is what enters the sediment model (Mode 2) or is simply removed from the water column (Modes 0 and 1).

\newpage

# Resuspension

When `RESUSPENSION_OPTION > 0`, ESTAS applies bed-sediment resuspension to selected boxes.  The module `mod_RESUSPENSION.f90` implements:

## Shear-Stress Threshold

Resuspension occurs when the bed shear stress $\tau_b$ exceeds the critical shear stress $\tau_c$ for a box:

$$E = \begin{cases} E_0 \left(\dfrac{\tau_b}{\tau_c} - 1\right) & \text{if } \tau_b > \tau_c \\ 0 & \text{otherwise} \end{cases}$$

where $E_0$ is the erosion rate coefficient.

## Configuration

- `CRITICAL_SHEAR_STRESSES.txt` provides per-box $\tau_c$ values.
- Shear stress time series are read from forcing files.
- `SHUT_DOWN_SETTLING` flag optionally disables settling during resuspension events.

\newpage

# Allelopathy

ESTAS carries four additional state variables (`NUM_ALLOLOPATHY_STATE_VARS = 4`) appended after the 32 standard pelagic variables.  These represent allelopathic substance concentrations released by competing phytoplankton groups.

The allelopathic module (`mod_ALLELOPATHY.f90`) computes:

- Production rates as functions of phytoplankton biomass
- Inhibition effects on susceptible phytoplankton growth
- Decay of allelopathic substances

These variables are transported by the same advection--dispersion--settling equations as the standard state variables.

\newpage

# Macroalgae

The macroalgae module (`mod_MACROALGAE.f90`) models attached benthic macroalgae using a Droop cell-quota framework.  Macroalgae interact with the pelagic model through:

- Nutrient uptake from the water column (N, P)
- Oxygen production / consumption
- Light competition (shading)

Macroalgae biomass is not transported but is associated with the bottom of specific boxes.

\newpage

# Exergy Diagnostics

The module `mod_PELAGIC_EXERGY.f90` computes eco-exergy — a thermodynamic measure of ecosystem organisation.  For each box, exergy components are computed from biomass concentrations using weighting factors (beta values) that reflect the genetic information content of each organismal group:

$$\text{Ex}_i = \sum_j \beta_j \, C_{i,j} \, V_i$$

Exergy outputs can be written to separate output files when enabled.

### Enabling Exergy Output

Exergy computation and output are controlled by three settings in the `PELAGIC_INPUTS.txt` file, read sequentially after the COCOA configuration section:

1. `CALCULATE_PELAGIC_EXERGY` — set to `1` to enable exergy computation (default: `0`).
2. `CREATE_PELAGIC_EXERGY_OUTPUTS` — set to `1` to write exergy output files (only read if step 1 is > 0).
3. `START_REPEAT_NO_PEL_EX_OUTS` — the repeat-cycle number from which to start writing exergy output (only read if step 2 is > 0).

By default, all three are `0` (disabled). To enable, add the appropriate lines to your `PELAGIC_INPUTS.txt` configuration.

\newpage

# Output Files

## Pelagic Output

ESTAS produces time-series output for each box:

| Column | Content |
|:---|:---|
| 1 | Time (days) |
| 2 | Box number |
| 3--34 | Concentrations of 32 pelagic state variables |
| 35--38 | Allelopathic state variables (if active) |

Output can be in ASCII or binary format, controlled by command-line arguments.

## Sediment Output (Mode 2)

When full diagenesis is active:

- **Sediment concentrations** — per-box, per-layer, per-variable (file: box-specific or COCOA format)
- **Sediment fluxes to water column** — 30 flux variables per box per output step
- **Sediment burial rates** — per-box, per-variable (optional COCOA output)
- **Fluxes to sediments** — settling fluxes entering the sediment surface

## COCOA Outputs

Optional extended output format (enabled via `PRODUCE_COCOA_OUTPUTS`), producing additional files for:

- Pelagic process rates
- Sediment process rates
- Burial rates
- Bidirectional sediment fluxes

\newpage

# Building and Running

## Compilation

The project uses a top-level `Makefile`:

```bash
# Clean and build in release mode
make clean-lib
make build-estas BUILD_TYPE=release

# Debug build
make build-estas BUILD_TYPE=debug
```

The build discovers all `.f90` files in `SOURCE_CODE/` and compiles them via `make_lib.sh`, producing the `ESTAS_II` executable.

## Running a Simulation

```bash
# Default: reads INPUT.txt from current directory
./ESTAS_II

# Specify input file
./ESTAS_II INPUT_gf_release.txt

# With constant override file
./ESTAS_II INPUT.txt WCONST_02.txt

# With constant override + binary output
./ESTAS_II INPUT.txt WCONST_02.txt output.bin

# Full: input + constants + binary + shear stress
./ESTAS_II INPUT.txt WCONST_02.txt output.bin CRITICAL_SHEAR_STRESSES.txt
```

## Running Tests

```bash
# Fortran unit tests (bioturbation, etc.)
make test-fortran

# Python/pytest integration tests (if configured)
pytest tests/
```

\newpage

# Source File Reference

## ESTAS Framework Files

All files are located under `SOURCE_CODE/ESTAS/`:

| File | Lines | Description |
|:---|:---:|:---|
| `ESTAS_II.f90` | 151 | Main program |
| `mod_GLOBAL.f90` | 285 | Global dimensions, allocatable arrays, switches |
| `mod_AQUATIC_MODEL.f90` | — | Master data structure and input reader |
| `mod_SIMULATE.f90` | 832 | Time loop, output orchestration |
| `mod_SOLVER.f90` | 1590 | Derivative calculation and Euler update |
| `mod_PELAGIC_BOX_MODEL.f90` | — | Box-model derived type |
| `mod_PELAGIC_BOX.f90` | — | Per-box data structure |
| `mod_PELAGIC_LINK.f90` | — | Link data structures |
| `mod_PELAGIC_ECOLOGY.f90` | — | AQUABC kinetics interface |
| `mod_BOTTOM_SEDIMENTS.f90` | — | Sediment initialisation |
| `mod_INITIAL_CONDITIONS.f90` | — | Initial concentration reader |
| `mod_INITIALIZE_PELAGIC_BOX_MODEL.f90` | — | Topology builder |
| `mod_INITIALIZE_AQUATIC_MODEL.f90` | — | High-level init |
| `mod_TIME_SERIES.f90` | — | Time-series type and interpolation |
| `mod_INTERPOLATE.f90` | — | Interpolation utilities |
| `mod_MASS_LOAD.f90` | — | Mass loads and withdrawals |
| `mod_RESUSPENSION.f90` | — | Resuspension algorithms |
| `mod_BASIN.f90` | — | Basin geometry |
| `mod_PELAGIC_EXERGY.f90` | — | Eco-exergy computation |
| `mod_COST_FUNCTION.f90` | — | Cost function |
| `mod_UTILS_01.f90` | — | General utilities |

## AQUABC Ecological Model Files

Located under `SOURCE_CODE/AQUABC/`:

| Directory | Key Files | Description |
|:---|:---|:---|
| `.` | `mod_AQUABC_II_GLOBAL.f90` | AQUABC global constants |
| `PELAGIC/` | `aquabc_II_pelagic_kinetics.f90` | 32-variable pelagic kinetics |
| `PELAGIC/` | `aquabc_II_wc_outputs.f90` | Water-column output mapping |
| `SEDIMENTS/` | `aquabc_II_sediment_model_1_fast.f90` | Multi-layer diagenesis driver |
| `SEDIMENTS/` | `aquabc_II_sediment_bioturbation.f90` | Bioturbation and bioirrigation |
| `SEDIMENTS/` | `aquabc_II_sediment_model_constants.f90` | 171 sediment constants |
| `SEDIMENTS/AQUABC_SEDIMENT_LIBRARY/` | 10+ library files | DOC mineralisation, redox, pH correction, etc. |

## Auxiliary Modules

| Directory | Key Files | Description |
|:---|:---|:---|
| `SOURCE_CODE/ALLELOPATHY/` | `mod_ALLELOPATHY.f90` | Allelopathic interactions |
| `SOURCE_CODE/MACROALGAE/` | `mod_MACROALGAE.f90` | Macroalgae (Droop quota) |
| `SOURCE_CODE/CO2SYS/` | CO2SYS source files | Carbonate equilibrium |

\newpage

# References

- Chapra, S.C. (1997). *Surface Water-Quality Modeling*. McGraw-Hill.
- Fischer, H.B. et al. (1979). *Mixing in Inland and Coastal Waters*. Academic Press.
- Jørgensen, S.E. (2006). *Eco-Exergy as Sustainability*. WIT Press.
- Droop, M.R. (1973). Some thoughts on nutrient limitation in algae. *J. Phycol.* 9, 264–272.
- Boudreau, B.P. (1997). *Diagenetic Models and Their Implementation*. Springer.
- Soetaert, K., Herman, P.M.J., and Middelburg, J.J. (1996). A model of early diagenetic processes from the shelf to abyssal depths. *Geochim. Cosmochim. Acta* 60, 1019–1040.
