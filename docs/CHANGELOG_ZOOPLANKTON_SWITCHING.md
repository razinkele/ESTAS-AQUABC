# Zooplankton Active Switching Model

## Summary

Implemented an **Active Switching Model** for zooplankton grazing that dynamically adjusts prey preferences based on relative prey abundance. This creates adaptive foraging behavior where zooplankton shift grazing effort toward more abundant prey types.

## Changes

**File Modified:** `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_ZOOPLANKTON.f90`

### New Variables Added

| Variable | Type | Description |
|:---------|:-----|:------------|
| `TOTAL_FOOD` | array(nkn) | Total preference-weighted food availability |
| `DYN_PREF_DIA` | array(nkn) | Dynamic preference for diatoms |
| `DYN_PREF_CYN` | array(nkn) | Dynamic preference for non-fixing cyanobacteria |
| `DYN_PREF_OPA` | array(nkn) | Dynamic preference for other phytoplankton |
| `DYN_PREF_FIX_CYN` | array(nkn) | Dynamic preference for N-fixing cyanobacteria |
| `DYN_PREF_NOST` | array(nkn) | Dynamic preference for Nostocales |
| `DYN_PREF_DET` | array(nkn) | Dynamic preference for detritus |
| `SWITCHING_POWER` | scalar | Controls switching intensity (default: 1.5) |

## Mathematical Formulation

### Dynamic Preference Calculation

$$\text{DYN\_PREF}_i = \text{BASE\_PREF}_i \times \left(\frac{\text{BASE\_PREF}_i \times \text{PREY}_i}{\text{TOTAL\_FOOD}}\right)^{n-1}$$

Where:
- $n$ = `SWITCHING_POWER` (currently set to 1.5)
- `TOTAL_FOOD` = $\sum_j (\text{PREF}_j \times \max(\text{PREY}_j - \text{FOOD\_MIN}, 0))$

### Switching Power Effects

| Value | Behavior |
|:------|:---------|
| n = 1.0 | No switching (original model behavior) |
| n = 1.5 | Moderate switching (current default) |
| n = 2.0 | Strong switching - heavily favors abundant prey |

## Ecological Implications

1. **Adaptive Foraging**: Zooplankton preferentially graze on abundant prey
2. **Competitive Release**: Rare prey gain refuge from grazing pressure
3. **Prey Switching**: Grazing pressure shifts as community composition changes
4. **Stabilizing Effect**: Can prevent competitive exclusion by reducing pressure on declining species

## References

- Gentleman, W., Leising, A., Frost, B., Strom, S., & Murray, J. (2003). Functional responses for zooplankton feeding on multiple resources: a review of assumptions and biological dynamics. *Deep Sea Research Part II*, 50(22-26), 2847-2875.

- Kiørboe, T. (2008). Optimal swimming strategies in mate-searching pelagic copepods. *Oecologia*, 155(1), 179-192.

## Future Work

- Consider exposing `SWITCHING_POWER` as a model parameter in `PELAGIC_MODEL_CONSTANTS.txt`
- Add option to toggle between fixed and dynamic preferences
- Implement prey-specific switching powers for different prey types

---

*Implemented: January 2026*
