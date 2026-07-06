# Runtime per-application temperature-model selection (plateau vs CTMI)

**Date:** 2026-07-06
**Status:** Approved design

## Problem

`GROWTH_AT_TEMP` (the phytoplankton temperature-limitation function) currently
hard-codes its model at compile time: `logical, parameter :: USE_CTMI = .true.`.
Because the function is compiled once into the shared `libaquabc.a`, every
application that links it — the 29-box CL29 run, the base 25-box model, and the
0-D example — is forced onto the same model.

Only CL29's temperature constants are recalibrated for CTMI (Rosso et al. 1993).
The 25-box model and the 0-D example use plateau-era constants (e.g.
`KAPPA_DIA_OVER_OPT_TEMP = 0.07`, a decay rate, not a `T_max`), which are
CTMI-invalid. Today they only run because `GROWTH_AT_TEMP` detects the invalid
CTMI parameters and falls back to the plateau model (merged in PR #14). That
fallback is a safety net, not a configuration mechanism: the temperature model
should be an explicit per-application choice.

## Goal

Make the temperature model a **runtime per-application option**, defaulting to
the plateau model, with CTMI as explicit opt-in. CL29 opts into CTMI; everything
else stays plateau (its calibrated regime) without relying on the fallback.

Non-goals (YAGNI): per-species model selection; a third temperature model;
exposing the option to the 0-D example (it keeps the default).

## Architecture

A single runtime flag lives in the shared AQUABC global module and is read by
`GROWTH_AT_TEMP`. Whatever drives the model sets it; if nothing sets it, it
defaults to plateau.

```
PELAGIC_MODEL_OPTIONS.txt ──read──> mod_PELAGIC_ECOLOGY (ESTAS)
                                        │ sets
                                        ▼
              mod_AQUABC_II_GLOBAL :: USE_CTMI_TEMP  (default .false.)
                                        │ read by
                                        ▼
                                   GROWTH_AT_TEMP
```

`mod_AQUABC_II_GLOBAL` is chosen because it is an AQUABC-side module already used
by 25 library files **and** by the ESTAS driver — so ESTAS can set the flag and
the library can read it without the AQUABC library depending on ESTAS.

## Components (5 changes)

### 1. `SOURCE_CODE/AQUABC/mod_AQUABC_II_GLOBAL.f90`
Add a module variable:
```fortran
logical :: USE_CTMI_TEMP = .false.   ! temperature model: .false.=plateau, .true.=CTMI
```
This is the single source of truth. Default `.false.` (plateau) preserves the
behavior every un-recalibrated setup needs.

### 2. `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90` — `GROWTH_AT_TEMP`
- Remove the compile-time `logical, parameter :: USE_CTMI = .true.`.
- Add `use AQUABC_II_GLOBAL, only: USE_CTMI_TEMP` and use it in place of the
  parameter.
- Keep the existing graceful validity check: when `USE_CTMI_TEMP` is true but the
  per-species constants are CTMI-invalid (`T_min < T_opt < T_max` and
  `2*T_opt > T_min+T_max`), warn once and fall back to plateau. This remains as
  defense-in-depth for a genuine misconfiguration.

### 3. `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` — options reader
After the existing option reads (…, `CONSIDER_ALLELOPATHY`, allelopathy file
name), read the new option **gracefully** so option files without the line keep
working:
```fortran
        read(IN_FILE + 1, *, end = 900, err = 900)                 ! comment line
        read(IN_FILE + 1, *, end = 900, err = 900) TEMP_MODEL_OPT
        USE_CTMI_TEMP = (TEMP_MODEL_OPT == 1)
900     continue   ! absent/malformed line -> USE_CTMI_TEMP stays .false. (plateau)
```
Requires `use AQUABC_II_GLOBAL, only: USE_CTMI_TEMP` in this module (add if not
already present) and a local `integer :: TEMP_MODEL_OPT`. (The label number `900`
is illustrative; the implementation picks one not already used in the routine.)

### 4. `INPUTS/PELAGIC_MODEL_OPTIONS.txt` (25-box template)
Append:
```
# TEMPERATURE_MODEL (0=plateau, 1=CTMI)
           0
```
The base 25-box model stays on plateau (its constants' calibrated regime).

### 5. `tools/eutropy_poc/eutropy_to_estas.py` — CL29 converter
The converter copies `PELAGIC_MODEL_OPTIONS.txt` from `INPUTS/`. After copying,
set CL29's option to CTMI: ensure the copied file's `TEMPERATURE_MODEL` value is
`1` (append the option if the template lacks it, or rewrite the value if present).
CL29 then explicitly runs CTMI with its recalibrated constants.

## Data flow

1. ESTAS run: `sub_READ_PELAGIC_INPUTS` → `mod_PELAGIC_ECOLOGY` reads
   `PELAGIC_MODEL_OPTIONS.txt`, sets `USE_CTMI_TEMP`.
2. Kinetics call `GROWTH_AT_TEMP`, which reads `USE_CTMI_TEMP` and selects CTMI
   (if true and params valid) or plateau.
3. 0-D example: never reads the options file → `USE_CTMI_TEMP` keeps its `.false.`
   default → plateau.

## Backward compatibility

- Option files without the `TEMPERATURE_MODEL` line → plateau (graceful read).
- The 25-box model and 0-D example now run plateau **directly** — no CTMI attempt,
  no fallback warning (cleaner than today's attempt-then-fall-back).
- CL29 (`TEMPERATURE_MODEL = 1`) runs CTMI with valid constants → identical results
  to the current merged behavior (diatom/OPA blooms, seasonal succession).
- The invalid-CTMI fallback remains only as a safety net.

## Testing / verification

| Case | Expectation |
|---|---|
| 25-box `./ESTAS_II INPUT.txt` (no line / `=0`) | plateau; "simulation finished"; **no** CTMI warning |
| CL29 `./ESTAS_II INPUT_CL29.txt` (`=1`) | CTMI; diatoms bloom (per-box max ~1.9), Si drawdown |
| 0-D example (`make run-0d` / `test`) | plateau default; no NaN/negative CHLA |
| Fortran unit tests (`tests/fortran`) | unaffected (own embedded plateau copy) |
| CI `build-and-run` | green |
| Regenerate `INPUTS_CL29/` | options file shows `TEMPERATURE_MODEL = 1` |

## Rollout

Single PR. Rebuild the library so both ESTAS and the example pick up the module
change. No data migration; existing input sets remain valid (default plateau).
