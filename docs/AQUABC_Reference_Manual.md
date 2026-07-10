---
title: "AQUABC v0.3 — Ecological Model Reference Manual"
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
  - \fancyhead[L]{AQUABC v0.3 Reference Manual}
  - \fancyhead[R]{\thepage}
  - \fancyfoot[C]{}
  - \usepackage{amsmath}
  - \usepackage{amssymb}
---

\newpage

# Introduction

AQUABC v0.3 is a comprehensive aquatic biogeochemical model that simulates coupled pelagic--sediment dynamics in lakes, reservoirs, estuaries, and coastal waters. The model is designed to be embedded in hydrodynamic transport frameworks (such as ESTAS or SHYFEM) that provide advection, dispersion, and external forcing.

## Model Architecture

AQUABC consists of five coupled sub-models:

1. **Pelagic model**: 32 state variables covering phytoplankton (4 groups), zooplankton, organic matter, nutrients, dissolved oxygen, carbonate chemistry, and redox-sensitive metals.
2. **Sediment diagenesis model**: 24 state variables per sediment layer for early diagenesis with full redox sequencing and bioturbation.
3. **Macroalgae model**: 6 state variables using Droop quota kinetics.
4. **Allelopathy module**: 4 secondary metabolite pools for inter-species chemical inhibition.
5. **CO$_2$SYS**: Full carbonate chemistry solver (CDIAC implementation).

## Source Code Organisation

| Component | File |
|-----------|------|
| Pelagic kinetics | `aquabc_II_pelagic_model.f90` |
| Phytoplankton libraries | `aquabc_II_pelagic_lib_*.f90` |
| Sediment model | `aquabc_II_sediment_model_1_fast.f90` |
| Bioturbation module | `aquabc_II_sediment_bioturbation.f90` |
| Sediment model constants | `aquabc_II_sediment_model_constants.f90` |
| CO$_2$SYS | `aquabc_II_co2sys.f90` |
| Macroalgae | `mod_MACROALGAE.f90` |
| Allelopathy | `mod_ALLELOPATHY.f90` |
| Precision definitions | `precision_kinds.f90` |
| Parameter dictionary | `STRING_UTILS.f90` (`para_aqua` module) |

\newpage

# Pelagic Model

## State Variables

The pelagic model tracks 32 state variables in each computational box. An additional 4 allelopathic metabolite variables (indices 33--36) are appended when the allelopathy module is active, giving 36 total transported variables.

> **Note:** The indices below correspond to the actual code assignments in `aquabc_II_pelagic_svindex.f90`. The paper (Ertürk et al., 2023) uses a different numbering (1--21) reflecting a simplified configuration.

| Index | Code Name | Unit | Description |
|:-----:|-----------|------|-------------|
| 1 | `NH4_N` | mg N/L | Ammonium nitrogen |
| 2 | `NO3_N` | mg N/L | Nitrate nitrogen |
| 3 | `PO4_P` | mg P/L | Orthophosphate phosphorus |
| 4 | `DOXY` | mg O$_2$/L | Dissolved oxygen |
| 5 | `DIA_C` | mg C/L | Diatom carbon |
| 6 | `ZOO_C` | mg C/L | Zooplankton carbon |
| 7 | `ZOO_N` | mg N/L | Zooplankton nitrogen |
| 8 | `ZOO_P` | mg P/L | Zooplankton phosphorus |
| 9 | `DET_PART_ORG_C` | mg C/L | Detrital particulate organic carbon |
| 10 | `DET_PART_ORG_N` | mg N/L | Detrital particulate organic nitrogen |
| 11 | `DET_PART_ORG_P` | mg P/L | Detrital particulate organic phosphorus |
| 12 | `DISS_ORG_C` | mg C/L | Dissolved organic carbon |
| 13 | `DISS_ORG_N` | mg N/L | Dissolved organic nitrogen |
| 14 | `DISS_ORG_P` | mg P/L | Dissolved organic phosphorus |
| 15 | `CYN_C` | mg C/L | Non-fixing cyanobacteria carbon |
| 16 | `OPA_C` | mg C/L | Other planktonic algae carbon |
| 17 | `DISS_Si` | mg Si/L | Dissolved silica |
| 18 | `PART_Si` | mg Si/L | Particulate (biogenic) silica |
| 19 | `FIX_CYN_C` | mg C/L | N-fixing cyanobacteria carbon |
| 20 | `INORG_C` | mg C/L | Dissolved inorganic carbon (DIC) |
| 21 | `TOT_ALK` | meq/L | Alkalinity |
| 22 | `FE_II` | mg Fe/L | Dissolved ferrous iron |
| 23 | `FE_III` | mg Fe/L | Particulate ferric iron |
| 24 | `MN_II` | mg Mn/L | Dissolved manganese(II) |
| 25 | `MN_IV` | mg Mn/L | Particulate manganese(IV) oxide |
| 26 | `CA` | mg Ca/L | Calcium |
| 27 | `MG` | mg Mg/L | Magnesium |
| 28 | `S_PLUS_6` | mg S/L | Dissolved sulphate |
| 29 | `S_MINUS_2` | mg S/L | Dissolved sulphide |
| 30 | `CH4_C` | mg C/L | Dissolved methane |
| 31 | `NOST_VEG_HET_C` | mg C/L | Nostocales vegetative cells + heterocysts |
| 32 | `NOST_AKI_C` | mg C/L | Nostocales akinetes |
| 33 | `SEC_METAB_1` | mg/L | Allelopathic secondary metabolite 1 |
| 34 | `SEC_METAB_2` | mg/L | Allelopathic secondary metabolite 2 |
| 35 | `SEC_METAB_3` | mg/L | Allelopathic secondary metabolite 3 |
| 36 | `SEC_METAB_4` | mg/L | Allelopathic secondary metabolite 4 |

> **Legacy naming note:** Variables 31--32 use two naming conventions in the code: the legacy `FIX_CYN_HET_C_INDEX` / `FIX_CYN_AK_C_INDEX` and the preferred `NOST_VEG_HET_C_INDEX` / `NOST_AKI_C_INDEX`. Both refer to the same state variables. The `NOST_*` convention is recommended.

## Phytoplankton Submodels

Four phytoplankton groups share a common kinetic framework:

### Temperature Limitation (CTMI)

The Cardinal Temperature Model with Inflection (Rosso et al., 1993):

$$\mu(T) = \begin{cases}
\mu_{\max} \cdot \dfrac{(T - T_{\max})(T - T_{\min})^2}{(T_{\text{opt}} - T_{\min})\left[(T_{\text{opt}} - T_{\min})(T - T_{\text{opt}}) - (T_{\text{opt}} - T_{\max})(T_{\text{opt}} + T_{\min} - 2T)\right]} & T_{\min} < T < T_{\max} \\
0 & \text{otherwise}
\end{cases}$$

#### CTMI Parameter Name Mapping

The parameter names in the constant file (`WCONST_04.txt`) have counter-intuitive mappings to the CTMI cardinal temperatures. The following table clarifies the relationship (see `aquabc_II_pelagic_auxillary.f90`):

| WCONST Parameter Name | CTMI Role | Description |
|:---|:---|:---|
| `OPT_TEMP_LR` (mapped to `Lower_TEMP`) | $T_{\min}$ | Minimum cardinal temperature (growth = 0 below) |
| `OPT_TEMP_UR` (mapped to `Upper_TEMP`) | $T_{\text{opt}}$ | Optimal temperature (growth peaks) |
| `KAPPA_OVER_OPT_TEMP` | $T_{\max}$ | Maximum cardinal temperature (growth = 0 above) |
| `KAPPA_UNDER_OPT_TEMP` | *unused* | Not used in the CTMI formulation |

### Light Limitation

Steele/Smith formulation with optional Platt-style photoinhibition:

$$f_I = \frac{I}{I_s} \exp\left(1 - \frac{I}{I_s}\right) \cdot \exp\left(-\beta \cdot I\right)$$

where $I$ is the depth-averaged PAR (Photosynthetically Active Radiation), $I_s$ is the light saturation parameter, and $\beta$ controls photoinhibition intensity.

### Nutrient Limitation (Synthesizing Unit)

The Synthesizing Unit colimitation model (Saito et al., 2008) is used for N--P colimitation in the **non-fixing** growth pathway of all phytoplankton groups:

$$f_N = \frac{1}{\frac{1}{f_{\text{N-lim}}} + \frac{1}{f_{\text{P-lim}}} + \frac{1}{f_{\text{Si-lim}}} - \frac{1}{f_{\text{N-lim}} \cdot f_{\text{P-lim}}} - \frac{1}{f_{\text{N-lim}} \cdot f_{\text{Si-lim}}} - \frac{1}{f_{\text{P-lim}} \cdot f_{\text{Si-lim}}} + \frac{1}{f_{\text{N-lim}} \cdot f_{\text{P-lim}} \cdot f_{\text{Si-lim}}}}$$

where each individual limitation follows Monod kinetics: $f_{\text{N-lim}} = [\text{DIN}]/([\text{DIN}] + K_{\text{HS,N}})$.

> **Important:** The Synthesizing Unit replaces Liebig's minimum **only** for N--P nutrient colimitation in the non-fixing pathway. Liebig's minimum is still used in: (1) the **fixing fraction** of N-fixing cyanobacteria, where the nitrogen term is a DIN-inhibition switch rather than colimitation; (2) combining nutrient limitation with oxygen limitation; (3) diatom Si colimitation cascaded with the N--P SU result. There is no user-configurable switch between SU and Liebig --- the choice is hardcoded per pathway.

### Growth, Respiration, Mortality

$$\frac{d[\text{PHYTO}]}{dt} = (\mu - R - D) \cdot [\text{PHYTO}]$$

where $\mu$ is the temperature-, light-, and nutrient-limited growth rate, $R$ is Arrhenius-corrected respiration $R = k_{R,20} \cdot \theta^{T-20}$, and $D$ is mortality (enhanced under hypoxia).

### Hypoxia Stress: Three-Regime System

All phytoplankton groups implement a three-regime dissolved oxygen stress system that modulates mortality, respiration, and growth. Given a group-specific DO threshold `DO_STR_HYPOX`:

**Regime 1 — Normal** ($[\text{O}_2] > \text{DO\_STR\_HYPOX}$):
$$\text{FAC\_HYPOX} = 1.0$$
No enhancement of mortality. Normal growth and respiration.

**Regime 2 — Hypoxia** ($0.1 \cdot \text{DO\_STR\_HYPOX} < [\text{O}_2] \leq \text{DO\_STR\_HYPOX}$):
$$\text{FAC\_HYPOX} = \theta_{\text{hypox}}^{\;\epsilon_{\text{hypox}} \cdot (\text{DO\_STR\_HYPOX} - [\text{O}_2])}$$
Mortality is exponentially enhanced as DO drops below the threshold.

**Regime 3 — Crash** ($[\text{O}_2] / \text{DO\_STR\_HYPOX} \leq 0.1$):
$$\text{FAC\_HYPOX} = \min\!\left(\frac{\Delta t}{0.5 \cdot K_D},\; \frac{0.9}{K_D \cdot \Delta t}\right)$$
$$R_{\text{growth}} = 0, \quad R_{\text{respiration}} = 0$$
This crash regime is a critical **numerical safeguard** that prevents mass-balance overshoot under near-anoxia. Growth and respiration are zeroed, and mortality is capped to ensure no more than 90% of biomass can be removed in a single time step.

The parameters `THETA_HYPOX` and `EXPON_HYPOX` are defined per phytoplankton group in the `WCONST` constants file.

## Zooplankton Dynamics

Zooplankton feeding uses the Active Switching Model (Gentleman et al., 2003):

$$I_i = I_{\max} \cdot \frac{\rho_i \cdot [F_i] / \sum_j (\rho_j \cdot [F_j])}{K_F + \sum_j [F_j]} \cdot \sum_j [F_j]$$

where $I_i$ is ingestion rate of food source $i$, $\rho_i$ are preference weights, $[F_i]$ are food concentrations, and $K_F$ is the half-saturation constant.

The dynamic preferences are computed using a switching power exponent:

$$\rho_i^{\text{dyn}} = \rho_i \cdot \left(\frac{\rho_i \cdot [F_i]}{\sum_j (\rho_j \cdot [F_j])}\right)^{n-1}$$

where $n = 1.5$ is the **switching power** (hardcoded in `aquabc_II_pelagic_lib_ZOOPLANKTON.f90`, line 192). Values: $n = 1.0$ gives linear (no switching), $n = 2.0$ gives strong switching. The default $n = 1.5$ provides moderate prey-switching behaviour.

## Organic Matter Cycling

### POM Dissolution

Particulate organic matter (C, N, P, Si) dissolves into dissolved forms:

$$R_{\text{diss}} = k_{\text{diss},20} \cdot \theta^{T-20} \cdot \frac{[\text{POM}]}{[\text{POM}] + K_{\text{HS}}} \cdot [\text{POM}]$$

### DOM Mineralisation

DOC mineralisation follows a six-pathway redox sequence, each with its own rate constant, temperature correction, and half-saturation kinetics:

1. Aerobic respiration (O$_2$)
2. Denitrification (NO$_3^-$)
3. Manganese reduction (Mn(IV))
4. Iron reduction (Fe(III))
5. Sulphate reduction (S(VI))
6. Methanogenesis (DOC as terminal acceptor)

Each pathway is regulated by:

- Substrate half-saturation: $f = [\text{DOC}]/([\text{DOC}] + K_{\text{HS,DOC}})$
- Electron acceptor availability: $f = [\text{EA}]/([\text{EA}] + K_{\text{HS,EA}})$
- Inhibition by higher-priority acceptors (reversed Monod)
- pH correction (Gaussian)

## Dissolved Oxygen

$$\frac{d[\text{O}_2]}{dt} = R_{\text{reaeration}} + R_{\text{photosynthesis}} - R_{\text{respiration}} - R_{\text{nitrification}} - R_{\text{oxidation}}$$

Reaeration follows the O'Connor--Dobbins or Wanninkhof formulation depending on wind speed input.

## Nutrient Cycling

- **Nitrification**: $\text{NH}_4^+ \to \text{NO}_3^-$ (Monod kinetics in O$_2$ and NH$_4$, pH-corrected)
- **Denitrification**: $\text{NO}_3^- \to \text{N}_2$ (only under low-O$_2$ conditions)
- **Phosphate speciation**: pH-dependent sorption with Fe(III)

## Redox Chemistry

Full thermodynamic hierarchy: O$_2$ > NO$_3^-$ > Mn(IV) > Fe(III) > SO$_4^{2-}$ > methanogenesis.

Metal speciation (Fe, Mn) includes oxidation--reduction kinetics and equilibrium solubility calculations.

\newpage

# Sediment Diagenesis Model

## Overview

The sediment model tracks 24 state variables per layer in a multi-layer vertical discretisation. It shares the same biogeochemical reaction framework as the pelagic model (POM dissolution, DOM mineralisation, nitrification, denitrification, full redox sequence) applied to sediment porewater and solid phases.

File: `aquabc_II_sediment_model_1_fast.f90`

## Sediment State Variables (per layer)

The 24 sediment state variables mirror the pelagic model with sediment-specific organic matter pools, dissolved nutrients, and redox species. Each variable exists in dissolved (porewater) and/or particulate (solid) phase.

## Transport Processes

Four transport mechanisms operate on each sediment layer:

### Advection

Porewater flow between layers:

$$R_{\text{adv,in}} = \frac{C_{\text{entering}}}{\max(\phi, 10^{-20})} \cdot |v_{\text{adv}}|$$

$$R_{\text{adv,out}} = \frac{C_{\text{current}} \cdot f_{\text{sol}}}{\max(\phi, 10^{-20})} \cdot |v_{\text{adv}}|$$

where $\phi$ is porosity and $f_{\text{sol}}$ is the dissolved fraction.

### Diffusion

Concentration gradient-driven with tortuosity correction:

$$R_{\text{diff}} = \frac{D_{\text{eff}}}{1 + 3(1-\phi)} \cdot \frac{\Delta C}{\max(\ell, 10^{-20})}$$

where $\ell = 0.5 \cdot (h_{i-1} + h_i)$ is the mixing length between adjacent layers.

### Particle Mixing (Bioturbation)

For solid-phase species:

$$R_{\text{pmix}} = \frac{D_b \cdot \Delta C_{\text{solid}}}{\max(\ell, 10^{-20})}$$

where $D_b$ is the biodiffusion coefficient (see §3.5).

### Burial

Permanent removal to deep sediment:

$$R_{\text{burial}} = \frac{C \cdot w_{\text{burial}}}{\max(h, 10^{-20})}$$

where $w_{\text{burial}}$ is the burial rate (m/day) and $h$ is the layer thickness.

### Combined Transport

$$\frac{dC}{dt}\bigg|_{\text{transport}} = R_{\text{diff}} \cdot \phi + R_{\text{burial}} + R_{\text{pmix}} + R_{\text{adv,in}} - R_{\text{adv,out}}$$

## Erosion and Deposition

When the sediment transport model is active (`isedi > 0`), layer concentrations are adjusted for sediment--water interface movement:

- **Deposition** ($H_{\text{erodep}} \leq 0$): Upper layer material mixed into lower layers
- **Erosion** ($H_{\text{erodep}} > 0$): Lower layer material exposed at the surface

In ESTAS standalone mode, `isedi = 0` (prescribed settling used instead).

## Sediment--Water Fluxes

24 dissolved fluxes are exchanged at the sediment--water interface based on concentration gradients between porewater and overlying water. Positive flux = release from sediment to water column.

| Flux Index | Species | Direction |
|-----------|---------|-----------|
| 1 | NH$_4$-N | Typically upward (nutrient release) |
| 2 | NO$_3$-N | Variable (nitrification/denitrification) |
| 3 | PO$_4$-P | Typically upward |
| 4 | DOXY | Typically downward (sediment O$_2$ demand) |
| 9 | DOC | Typically upward |
| 15 | ALK | Typically upward |

## Bioturbation and Bioirrigation

File: `aquabc_II_sediment_bioturbation.f90`

When enabled (`switch_partmixing = 1`, default), particle mixing coefficients are dynamically computed based on local conditions rather than using a uniform constant.

### Biodiffusion Coefficient

$$D_b(z, t) = D_{b0} \cdot \exp\!\left(-\frac{z}{z_{\text{mix}}}\right) \cdot \frac{[\text{O}_2]}{[\text{O}_2] + K_{\text{HS,O}_2}} \cdot \left[1 + A \cos\!\left(\frac{2\pi(d - d_{\text{peak}})}{365}\right)\right]$$

The three multiplicative factors represent:

1. **Depth attenuation**: Exponential decay reflecting the decrease of macrofauna density with depth in sediments (Boudreau, 1997).
2. **Oxygen dependence**: Monod kinetics reflecting that benthic fauna require oxygen; under anoxia, bioturbation ceases.
3. **Seasonal modulation**: Sinusoidal annual cycle with summer peak, reflecting temperature-driven activity patterns (Soetaert et al., 1996).

### Bioturbation Parameters

| Parameter | Symbol | Default | Unit |
|-----------|--------|---------|------|
| Surface biodiffusion | $D_{b0}$ | from input | m$^2$/day |
| Mixing depth | $z_{\text{mix}}$ | 0.05 | m |
| O$_2$ half-saturation | $K_{\text{HS,O}_2}$ | 2.0 | mg/L |
| Seasonal amplitude | $A$ | 0.3 | — |
| Peak day | $d_{\text{peak}}$ | 200 | day |
| Irrigation enhancement | $\alpha_0$ | 3.0 | — |
| Irrigation depth | $z_{\text{irr}}$ | 0.04 | m |

### Bioirrigation

Burrow-dwelling organisms pump overlying water through the sediment, enhancing porewater solute exchange beyond molecular diffusion:

$$\alpha(z, t) = 1 + \alpha_0 \cdot \exp\!\left(-\frac{z}{z_{\text{irr}}}\right) \cdot \frac{[\text{O}_2]}{[\text{O}_2] + K_{\text{HS,O}_2}} \cdot f_{\text{season}}(t)$$

The irrigation factor $\alpha$ multiplies the molecular diffusion coefficient for dissolved-phase species:

$$D_{\text{eff,enhanced}} = \alpha \cdot D_{\text{eff,molecular}}$$

Bioirrigation is only applied to dissolved and mixed-phase species (not purely particulate). It is controlled by `switch_bioirrigation` (default = 1, enabled).

### Lower Boundary Condition

A zero-flux (Neumann) boundary condition at the bottom of the deepest layer:

$$\left.\frac{\partial C}{\partial z}\right|_{z_{\text{bottom}}} = 0 \quad \Rightarrow \quad R_{\text{pmix}}(N) = -R_{\text{pmix,in}}(N)$$

This prevents artificial mass loss through the lower boundary.

## Sediment pH Correction

Gaussian formulation (different from pelagic exponential):

$$f_{\text{pH}} = \exp\!\left(-\frac{(\text{pH} - \text{pH}_{\text{opt}})^2}{2\sigma^2}\right)$$

where $\text{pH}_{\text{opt}} = (\text{pH}_{\min} + \text{pH}_{\max})/2$ and $\sigma = (\text{pH}_{\max} - \text{pH}_{\min})/4$.

\newpage

# CO$_2$ System

The full carbonate chemistry system follows the CDIAC CO2SYS implementation, computing equilibrium speciation of dissolved inorganic carbon:

$$\text{CO}_2 \rightleftharpoons \text{HCO}_3^- \rightleftharpoons \text{CO}_3^{2-}$$

Given any two of (pH, pCO$_2$, DIC, ALK, CO$_3^{2-}$), the system solves for all others using iterative Newton--Raphson methods with temperature- and salinity-dependent equilibrium constants.

### Equilibrium Constant Selection

The code uses `K1K2CONSTANTS = 4`, which selects the **Mehrbach (1973) constants refit by Dickson \& Millero (1987)**. This is the standard choice for estuarine and coastal applications. The selection is hardcoded in `aquabc_II_pelagic_model.f90` (line 390).

\newpage

# Macroalgae Model

## Droop Quota Nutrient Limitation

$$Q_N = \frac{[\text{MAC-N}]}{[\text{MAC-C}]}, \qquad \Phi_N = \max\!\left(0,\; 1 - \frac{Q_{0,N}}{Q_N}\right)$$

## Growth

$$R_{\text{growth}} = k_{G,20} \cdot \theta^{T-20} \cdot \Phi_N \cdot \Phi_P \cdot \Phi_L \cdot \Phi_S \cdot [\text{MAC-C}]$$

where $\Phi_S = 1 - [\text{MAC-C}]/C_{\max}$ is the logistic space limitation.

\newpage

# Allelopathy Module

Four secondary metabolite pools (state variable indices **33--36**, appended after the 32 standard pelagic variables) model inter-species chemical inhibition. Each phytoplankton group produces a specific metabolite that inhibits growth of competing groups via Monod-type dose--response:

$$f_{\text{inhib}} = 1 - \frac{[\text{SEC-METAB}]}{[\text{SEC-METAB}] + K_{\text{HS,inhib}}}$$

\newpage

# Model Constants

## Pelagic Model Constants (323 parameters)

The pelagic model uses 323 parameters: 318 stored in the `MODEL_CONSTANTS(1:318)` array and 5 additional photoinhibition parameters (`BETA_*`) accessed via the named-parameter dictionary (`para_get_value`). Organised in 20 categories:

1. **General** (1--4): Temperature ranges, O$_2$ tolerances
2. **Diatoms** (5--31): Growth, respiration, mortality, nutrient kinetics, Si:C ratio
3. **Non-fixing cyanobacteria** (29--50): Buoyancy regulation, light adaptation
4. **N-fixing cyanobacteria** (51--74): Nitrogen fixation rates, heterocyst parameters
5. **Other planktonic algae** (75--96): Growth and loss parameters
6. **Zooplankton** (97--133): Feeding preferences, active switching parameters
7. **POM dissolution** (134--146): Rate constants per organic matter type
8. **DOM mineralisation** (147--152): Aerobic pathway parameters
9. **Nitrification** (152--157): Rate and half-saturation constants
10. **Redox chemistry** (158--209): All six electron acceptor pathways
11. **Methane** (210--234): CH$_4$ oxidation and production
12. **Settling** (235--250): Settling velocities per state variable
13. **pH correction** (251--266): Optimal pH ranges per process
14. **Dissolved metals** (267--276): Fe/Mn oxidation and dissolution rates
15. **Nostocales** (276--298): Heterocyst-forming cyanobacteria parameters
16. **Nostocales extended** (299--306): Akinete temperature/nitrogen thresholds
17. **POM dissolution saturation** (307--309): POM dissolution saturation kinetics
18. **DOM availability fractions** (310--315): DON/DOP availability fractions per process
19. **Phytoplankton mineralisation caps** (316--318): Max phytoplankton-mediated mineralisation rates
20. **Photoinhibition BETA** (319--323): Five $\beta$ photoinhibition constants for each phytoplankton group, accessed via `para_get_value` using names `BETA_*`

> **Note on paper vs. code:** Ert\u00fcrk et al. (2023) report 183 calibrated constants — this refers to the subset actively calibrated for the Curonian Lagoon case study, not the full model constant set.

All constants are read from an external file and stored in the `para_aqua` name--value dictionary for runtime access.

## Sediment Model Constants (171 parameters)

Sediment-specific rate constants mirror the pelagic model with separate values optimised for sediment conditions (lower temperatures, higher organic matter concentrations, different pH ranges). Read from `BOTTOM_SEDIMENT_MODEL_INPUT.txt` via the `WCONST` constants file.

\newpage

# Numerical Methods

## Safety Functions

All division operations use `max(divisor, 10^{-20})` floors to prevent NaN/Inf. Temperature-dependent exponentials use `safe_exp()` with overflow protection. pH is clamped to [4, 11].

## Time Integration

The host transport framework (ESTAS or SHYFEM) provides the numerical time integration. AQUABC returns instantaneous rate derivatives which are integrated by the host using Euler or second-order Runge--Kutta methods.

## Mass Balance Safeguards

- Maximum 50% loss per timestep for zooplankton and phytoplankton
- Concentration floors at 0 (non-negative constraint)
- Derivative clamping to prevent numerical overshoot

\newpage

# References

- Boudreau, B.P. (1997). *Diagenetic Models and Their Implementation*. Springer.
- Gentleman, W., Leising, A., Frost, B., Strom, S., and Murray, J. (2003). Functional responses for zooplankton feeding on multiple resources. *J. Plankton Res.* 25:1215--1234.
- Middelburg, J.J., Soetaert, K., and Herman, P.M.J. (1997). Empirical relationships for use in global diagenetic models. *Deep-Sea Res. I* 44:327--344.
- Morgan, B. and Lahav, O. (2007). The effect of pH on the kinetics of spontaneous Fe(II) oxidation. *Chemosphere*.
- Platt, T., Gallegos, C.L., and Harrison, W.G. (1980). Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton. *J. Mar. Res.* 38:687--701.
- Rosso, L., Lobry, J.R., and Flandrois, J.P. (1993). An unexpected correlation between cardinal temperatures of microbial growth highlighted by a new model. *J. Theor. Biol.* 162:447--463.
- Saito, M.A., Goepfert, T.J., and Ritt, J.T. (2008). Some thoughts on the concept of colimitation. *Limnol. Oceanogr.* 53:276--290.
- Soetaert, K., Herman, P.M.J., and Middelburg, J.J. (1996). A model of early diagenetic processes from the shelf to abyssal depths. *Geochim. Cosmochim. Acta* 60:1019--1040.
- Steele, J.H. (1962). Environmental control of photosynthesis in the sea. *Limnol. Oceanogr.*
- Stumm, W. and Morgan, J.J. (1996). *Aquatic Chemistry*. Wiley.
- CDIAC CO2SYS program documentation.
- EPA-829-R-14-007. AQUATOX Technical Documentation.
