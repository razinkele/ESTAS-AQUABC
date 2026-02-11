# AQUABC Allelopathy Module: Scientific Review and Development Roadmap

## Executive Summary

This document provides an in-depth scientific review of the allelopathy modeling component in AQUABC, evaluates its current implementation against the state-of-the-art in aquatic ecology, and proposes avenues for improvement and further development.

---

## 1. Current Implementation Analysis

### 1.1 Model Structure Overview

The AQUABC allelopathy module implements a **competitive interference mechanism** through secondary metabolite dynamics. The model tracks four secondary metabolite pools corresponding to the major phytoplankton functional groups:

| Pool | Source Organism | Implemented |
|:-----|:----------------|:-----------:|
| `SEC_METAB_DIA` | Diatoms | ✓ |
| `SEC_METAB_NOFIX_CYN` | Non-fixing Cyanobacteria (*Microcystis*, *Oscillatoria*) | ✓ |
| `SEC_METAB_FIX_CYN` | N-fixing Cyanobacteria (*Aphanizomenon*, *Anabaena*) | ✓ |
| `SEC_METAB_NOST` | Nostocales (*Nodularia*, heterocyst-formers) | ✓ |

### 1.2 Mathematical Framework

#### Mass Balance

The secondary metabolite dynamics follow first-order kinetics:

```
d[SEC_METAB_i]/dt = R_FORM,i - R_DEG,i
```

Where:
- **Formation**: R<sub>FORM,i</sub> = S<sub>i</sub> × R<sub>DEATH,i</sub>
- **Degradation**: R<sub>DEG,i</sub> = k<sub>i</sub><sup>20</sup> × θ<sub>i</sub><sup>(T-20)</sup> × [SEC_METAB<sub>i</sub>]

#### Inhibition Function

A **reverse Monod (inhibitory)** formulation:

```
IHBF_i→j = K_HS / (K_HS + [SEC_METAB_i])
```

#### Net Inhibition

The minimum inhibition factor is applied (most severe constraint):

```
IHBF_target = min(IHBF_DIA→target, IHBF_NOFIX→target, IHBF_FIX→target, IHBF_NOST→target)
```

### 1.3 Parameter Space

Current default parameters (from `EXTRA_WCONST.txt`):

| Parameter | DIA | NOFIX_CYN | FIX_CYN | NOST |
|:----------|:---:|:---------:|:-------:|:----:|
| K<sub>HS</sub> (half-saturation) | 0.001 | 0.002 | 0.003 | 0.004 |
| k<sub>DEG</sub><sup>20</sup> (1/day) | 0.10 | 0.15 | 0.20 | 0.25 |
| θ (temp. coeff.) | 1.045 | 1.050 | 1.055 | 1.060 |
| S (yield coeff.) | 0.005 | 0.010 | 0.015 | 0.020 |

---

## 2. Scientific Strengths

### 2.1 Mechanistic Foundation ✓

The model correctly implements:

1. **Death-mediated release**: Secondary metabolites are released upon cell death (lysis), consistent with observations that many cyanotoxins are intracellular and released during bloom senescence (Chorus & Welker, 2021).

2. **Temperature-dependent degradation**: Uses Arrhenius-type kinetics appropriate for biochemical processes. θ values (1.04-1.06) are reasonable for enzymatic/photolytic degradation.

3. **Functional group specificity**: Recognizes that different phytoplankton groups produce different allelochemicals with varying potencies.

4. **Competitive interaction matrix**: Allows asymmetric interactions (species A may inhibit B more than B inhibits A).

### 2.2 Integration with Growth Kinetics ✓

The `GROWTH_INHIB_FACTOR` is multiplicatively applied to:
- Phytoplankton growth rates
- Zooplankton feeding rates

This is ecologically reasonable—toxins can reduce both autotrophic production and heterotrophic grazing.

### 2.3 Spatial Flexibility ✓

The module operates on a per-node (nkn) basis, allowing spatial heterogeneity in allelopathic interactions across the model domain.

---

## 3. Scientific Limitations and Gaps

### 3.1 Release Mechanism Oversimplification

**Current**: Metabolites released only upon death (R<sub>DEATH</sub>)

**Reality**: Many allelochemicals are actively exuded by living cells, especially under nutrient stress:

> "Increase in the production of allelopathic substances by *Prymnesium parvum* cells grown under N- or P-deficient conditions" — Granéli & Johansson (2003)

**Impact**: Current model may underestimate allelopathic effects during nutrient-limited but actively growing conditions.

### 3.2 Uniform Toxin Properties

**Current**: Each phytoplankton group produces a single "generic" secondary metabolite

**Reality**: Cyanobacteria alone produce diverse toxins with different properties:

| Toxin Class | Examples | Mode of Action | Stability |
|:------------|:---------|:---------------|:----------|
| Hepatotoxins | Microcystins, Nodularin | Phosphatase inhibition | Very stable |
| Neurotoxins | Anatoxin-a, Saxitoxin | Nerve impulse blocking | Less stable |
| Cytotoxins | Cylindrospermopsin | Protein synthesis | Moderate |
| Dermatotoxins | Aplysiatoxins | Skin irritation | Variable |

### 3.3 Missing Photodegradation

**Current**: Temperature-only degradation kinetics

**Reality**: Many cyanotoxins undergo significant photodegradation. Microcystin half-lives under sunlight are 1-10 days, but can be >30 days in the dark (Chorus & Welker, 2021).

### 3.4 No Self-Inhibition (Autoinhibition)

**Current**: Species cannot inhibit themselves (diagonal of interaction matrix is excluded)

**Reality**: Some evidence suggests autoinhibition at high cell densities (quorum-sensing related)

### 3.5 Static K<sub>HS</sub> Parameters

**Current**: Fixed half-saturation constants

**Reality**: Target species sensitivity varies with:
- Physiological state
- Previous exposure (acclimation)
- Life stage
- Environmental conditions

### 3.6 No Toxin Accumulation in Biomass

**Current**: Toxins are free in water only

**Reality**: Zooplankton and fish can bioaccumulate cyanotoxins, leading to:
- Chronic effects
- Biomagnification
- Food web transfer

### 3.7 Missing Bacterial Degradation

**Current**: Abiotic degradation only

**Reality**: Microbial communities (especially *Sphingomonas* spp.) can rapidly degrade microcystins via the mlr gene cluster (Jones et al., 1994).

---

## 4. Improvement Avenues

### 4.1 Short-term Improvements (Moderate Effort)

#### 4.1.1 Active Exudation Pathway

Add living-cell exudation term to metabolite formation:

```fortran
R_FORM = S_DEATH * R_DEATH + S_EXUDE * BIOMASS * STRESS_FACTOR
```

Where `STRESS_FACTOR` could be based on nutrient limitation:

```fortran
STRESS_FACTOR = 1.0 - min(LIM_N, LIM_P)
```

**Effort**: ~20 lines of code, 4 new parameters per group

#### 4.1.2 Light-dependent Degradation

Extend degradation kinetics:

```fortran
R_DEG = (k_DARK + k_LIGHT * I/I_ref) * THETA^(T-20) * [SEC_METAB]
```

Where:
- `k_DARK`: Dark degradation rate
- `k_LIGHT`: Additional photolysis rate
- `I`: Light intensity at depth
- `I_ref`: Reference light intensity

**Effort**: ~30 lines, 2 new parameters per group, use existing light field

#### 4.1.3 Variable K<sub>HS</sub> with Temperature

Implement temperature-dependent sensitivity:

```fortran
K_HS_effective = K_HS_20 * THETA_KHS^(T-20)
```

This reflects that organisms under thermal stress may be more susceptible.

**Effort**: ~10 lines, 1 new parameter per interaction

### 4.2 Medium-term Enhancements (Significant Effort)

#### 4.2.1 Multiple Toxin Classes

Distinguish 2-3 toxin categories per cyanobacteria group:

| Group | Toxin 1 | Toxin 2 |
|:------|:--------|:--------|
| NOFIX_CYN | Microcystin (stable) | Cylindrospermopsin |
| FIX_CYN | Anatoxin-a (labile) | Saxitoxin (stable) |
| NOST | Nodularin | Aplysiatoxin |

**Effort**: Double state variables (8 instead of 4), restructure interaction matrix

#### 4.2.2 Zooplankton Toxin Accumulation

Add internal toxin pool in zooplankton:

```fortran
d[ZOO_TOX]/dt = INGESTION_TOX - DEPURATION - ZOO_DEATH * [ZOO_TOX]
```

With grazing reduction based on internal burden:

```fortran
GRAZING_FACTOR = exp(-lambda * [ZOO_TOX])
```

**Effort**: ~100 lines, new state variable, ~6 new parameters

#### 4.2.3 Bacterial Degradation Module

Add microbial degradation pathway:

```fortran
R_DEG_BACT = V_max * [SEC_METAB] / (K_M + [SEC_METAB]) * BACT_BIOMASS
```

This requires coupling with bacterial dynamics (if available) or a simple bacterial biomass proxy.

**Effort**: ~50 lines, 2-3 new parameters

### 4.3 Long-term Research Directions (Major Effort)

#### 4.3.1 Cell Quota-based Toxin Dynamics

Replace biomass-proportional production with intracellular quota model:

```fortran
d[Q_TOX]/dt = PRODUCTION(nutrient_status) - DILUTION(growth)
[SEC_METAB]_release = Q_TOX * (R_DEATH + R_EXUDE)
```

This captures the observation that toxin content per cell varies with growth conditions.

**Effort**: Major restructuring, ~200+ lines, integration with internal quota models

#### 4.3.2 Gene-based Toxin Production

Link toxin production to "toxin gene expression" proxy:

```fortran
GENE_EXPR = f(LIGHT, TEMP, NUTRIENTS, DENSITY)
PRODUCTION = GENE_EXPR * MAX_PRODUCTION
```

Calibrate against molecular studies of mcy gene expression.

**Effort**: Novel module, ~150 lines, requires literature-derived functions

#### 4.3.3 Toxin-specific Pharmacokinetics

Implement ADME (Absorption, Distribution, Metabolism, Excretion) for key toxins:

```fortran
! Fish liver microcystin accumulation
d[FISH_MC]/dt = k_abs * MC_WATER - k_elim * [FISH_MC]
```

**Effort**: Requires food web model extension, ~300+ lines

#### 4.3.4 Climate Change Scenarios

Develop temperature-toxin production relationships based on literature:

> "Warming may enhance toxin production in *Microcystis* and *Cylindrospermopsis*" — Paerl & Paul (2012)

```fortran
TOXIN_PRODUCTION_MULTIPLIER = 1.0 + BETA_WARMING * (T - T_REF)
```

**Effort**: Parameterization study, ~20 lines code

---

## 5. Validation and Calibration Needs

### 5.1 Required Observational Data

For proper calibration, the allelopathy module needs:

| Data Type | Priority | Availability |
|:----------|:--------:|:-------------|
| Cyanotoxin concentrations | High | Often monitored |
| Phytoplankton biovolumes by group | High | Microscopy/FlowCAM |
| Toxin-producer fractions | Medium | PCR-based |
| Degradation rates (field) | Medium | Mesocosm studies |
| Zooplankton sensitivity tests | Low | Bioassays |

### 5.2 Recommended Validation Approach

1. **Hindcast known bloom events** with toxin measurements
2. **Sensitivity analysis** on K<sub>HS</sub> and S parameters
3. **Compare competitive exclusion patterns** with observations
4. **Validate degradation rates** against mesocosm data

---

## 6. Priority Recommendations

### High Priority (Recommended for v0.3)

1. **Add nutrient-stress exudation** — improves realism with minimal code changes
2. **Implement light-dependent degradation** — important for surface layer dynamics
3. **Add output diagnostics** — track inhibition factors in output for model analysis

### Medium Priority (v0.4)

4. **Variable temperature sensitivity** — easy to implement
5. **Zooplankton toxin accumulation** — ecologically important
6. **Multiple toxin classes** — better representation of cyanobacteria diversity

### Lower Priority (v1.0+)

7. **Bacterial degradation**
8. **Intracellular quota model**
9. **Food web transfer/bioaccumulation**

---

## 7. References

1. **Chorus, I. & Welker, M. (2021)**. *Toxic Cyanobacteria in Water*, 2nd ed. WHO/CRC Press.

2. **Fistarol, G.O., Legrand, C., & Granéli, E. (2003)**. Allelopathic effect of *Prymnesium parvum* on a natural plankton community. *Marine Ecology Progress Series*, 255, 115-125.

3. **Granéli, E. & Johansson, N. (2003)**. Increase in the production of allelopathic substances by *Prymnesium parvum* cells grown under N- or P-deficient conditions. *Harmful Algae*, 2(2), 135-145.

4. **Jones, G.J., Bourne, D.G., Blakeley, R.L., & Doelle, H. (1994)**. Degradation of the cyanobacterial hepatotoxin microcystin by aquatic bacteria. *Natural Toxins*, 2(4), 228-235.

5. **Leflaive, J. & Ten-Hage, L. (2007)**. Algal and cyanobacterial secondary metabolites in freshwaters: a comparison of allelopathic compounds and toxins. *Freshwater Biology*, 52(2), 199-214.

6. **Legrand, C., Rengefors, K., Fistarol, G.O., & Granéli, E. (2003)**. Allelopathy in phytoplankton - biochemical, ecological and evolutionary aspects. *Phycologia*, 42(4), 406-419.

7. **Paerl, H.W. & Paul, V.J. (2012)**. Climate change: Links to global expansion of harmful cyanobacteria. *Water Research*, 46(5), 1349-1363.

8. **Rzymski, P., et al. (2014)**. Interspecific allelopathy in cyanobacteria: Cylindrospermopsin and *Cylindrospermopsis raciborskii* effect on growth and metabolism of *Microcystis aeruginosa*. *Harmful Algae*, 35, 1-8.

9. **Suikkanen, S., Fistarol, G.O., & Granéli, E. (2004)**. Allelopathic effects of the Baltic cyanobacteria *Nodularia spumigena*, *Aphanizomenon flos-aquae* and *Anabaena lemmermannii* on algal monocultures. *Journal of Experimental Marine Biology and Ecology*, 308(1), 85-101.

10. **Welker, M. & Steinberg, C. (2000)**. Rates of humic substance photosensitized degradation of microcystin-LR in natural waters. *Environmental Science & Technology*, 34(16), 3415-3419.

---

## 8. Implementation Checklist for Developers

### Phase 1: Diagnostics Enhancement

- [ ] Add IHBF factors to output file (process rates)
- [ ] Add metabolite formation/degradation rates to diagnostics
- [ ] Create validation plotting routine for allelopathy dynamics

### Phase 2: Active Exudation

- [ ] Add `S_EXUDE_*` parameters to EXTRA_WCONST.txt
- [ ] Implement stress-dependent exudation in `allelopathy_SEC_METABOLITES.f90`
- [ ] Update documentation

### Phase 3: Photodegradation

- [ ] Add `k_LIGHT_*` parameters
- [ ] Pass light field to allelopathy module
- [ ] Implement depth-dependent photolysis

### Phase 4: Zooplankton Accumulation

- [ ] Add `ZOO_TOXIN` state variable
- [ ] Implement ingestion/depuration kinetics
- [ ] Modify grazing functions

---

*Document prepared for AQUABC v0.2*  
*Last updated: January 2026*
