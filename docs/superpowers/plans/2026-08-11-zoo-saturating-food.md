# Zooplankton Saturating Total-Food Response (Route B) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement
> this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** An opt-in Fasham-type food limitation for zooplankton — total ingestion saturates on
preference-weighted total food instead of summing preference-diluted per-prey Monods — so
that feeding can, at realistic food levels, exceed mortality (doc §12: measured per-capita
ingestion 0.058/d vs losses 0.185/d under the current formulation; ceiling 0.18–0.25).

**Architecture:** One new integer option `ZOO_FOOD_MODEL` (0 = legacy, default; 1 =
saturating) plus one constant `KHS_FOOD_TOT_ZOO` (half-saturation on total preferred food,
default 0.5 mg C/L), read gracefully from `PELAGIC_MODEL_OPTIONS.txt` exactly like
`TEMPERATURE_MODEL`/`FEPO4_KSP_LOG10`, threaded as trailing dummy arguments
ESTAS → `AQUABC_PELAGIC_KINETICS` → `ZOOPLANKTON`, overriding only the six
`FOOD_FACTOR_ZOO_*` values when the option is on. Legacy path arithmetic untouched →
byte-identical default.

**Verified ground truth** (this session): CL29 runs the ESTAS path — options come from
`READ_PELAGIC_MODEL_OPTIONS` (`mod_PELAGIC_ECOLOGY.f90:1071`), NOT the 0D interface's
hardcoded block (`aquabc_II_pelagic_interface.f90:150`), which is the parallel-code-path trap.
`ZOOPLANKTON` has exactly one caller (`aquabc_II_pelagic_model.f90:1319`);
`AQUABC_PELAGIC_KINETICS` has two call sites (ESTAS `mod_PELAGIC_ECOLOGY.f90:1465`, 0D
interface). The routine runs inside the OpenMP parallel region → no lazy env reads; options
must arrive as arguments. The severe-hypoxia guard zeroes `R_ZOO_FEEDING_*` after computation
and must stay downstream of the override.

## The formulation (only under `ZOO_FOOD_MODEL > 0`)

```
F_TOT   = TOTAL_FOOD                          (already computed: Σ pref_i · max(C_i − FOOD_MIN, 0))
FF_TOT  = F_TOT / (F_TOT + KHS_FOOD_TOT_ZOO)  (saturates to 1)
W_i     = DYN_PREF_i · max(C_i − FOOD_MIN, 0) (keeps the active-switching diet split)
FOOD_FACTOR_ZOO_i = FF_TOT · W_i / Σ W        (Σ FOOD_FACTOR_i = FF_TOT exactly)
```

`R_ZOO_FEEDING_i = KG_ZOO·GRAT_i · FOOD_FACTOR_i · ZOO_C` stays as is; GRAT_DET = 0.5 still
discounts detritus at intake. Where `Σ W ≤ 1e-10` or `TOTAL_FOOD ≤ 1e-10`: factors = 0.

### Task 1: plumbing + formulation (single compile unit of work)

**Files:**
- Modify: `SOURCE_CODE/ESTAS/mod_GLOBAL.f90` (~:183) — `ZOO_FOOD_MODEL` (int, init 0),
  `KHS_FOOD_TOT_ZOO` (dbl, init 0.5D0) next to `ZOOPLANKTON_OPTION`.
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90:1131ff` — two graceful read pairs after
  the FEPO4 pair (defaults pre-set; `end=900, err=900`; must precede the trailing
  `CYN_ALLELOPATHY_FILE_NAME` lines, which err harmlessly into 900 for old files).
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` — two trailing dummy args
  on `AQUABC_PELAGIC_KINETICS`; pass into `ZOOPLANKTON` at :1319.
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90:1465` — pass the two globals.
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90` — pass `0, 0.5D0`
  (0D path keeps legacy behaviour).
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_ZOOPLANKTON.f90`
  — two dummy args; override block after the six legacy `FOOD_FACTOR` where-blocks, before
  the `R_ZOO_FEEDING_*` lines; local arrays `FF_TOT(nkn)`, `W_*(nkn)`, `W_SUM(nkn)` declared
  in the file's existing style.

**Steps:**
- [ ] 1. Implement all edits above.
- [ ] 2. `make clean-all && make build-estas` — clean compile, no warnings on touched files.
- [ ] 3. **Byte gate:** 30-day CL29 run with the UNMODIFIED options file (option absent →
  defaults) vs the pre-change binary's outputs: `diff -r` must be empty.
- [ ] 4. Commit.

### Task 2: opt-in verification

- [ ] 1. Scratch config: copy `PELAGIC_MODEL_OPTIONS.txt` + insert `ZOO_FOOD_MODEL 1` and
  `KHS_FOOD_TOT_ZOO 0.5` between the FEPO4 entry and `CYN_ALLELOPATHY_FILE_NAME`.
- [ ] 2. 30-day smoke run: completes; zoo feeding rates nonzero; `ZOO_C` trajectory sane.
- [ ] 3. Full-record run (proven harness); score: validator (EPA + groups + `--phase`) and
  the per-capita zoo budget (`ZOO_C_INDEX=6`; growth==Σfeedings identity, hypoxia rows exempt).
  Expected: June per-capita ingestion rises toward ~KG_ZOO·0.7·FF_TOT ≈ 0.2–0.3/d; ZOO_C
  toward obs 0.046; watch grazing feedback on DIA (winter) and CYN/FIX (summer).
- [ ] 4. Record results in `docs/CL29_phenology_diagnosis.md` §13; commit.

### Task 3: decision + finish

- [ ] 1. Adoption is a user decision (needs a KG/KD_ZOO recalibration pass under the new
  response — doc §12). Present A/B table; do not flip the CL29 default unilaterally.
- [ ] 2. finishing-a-development-branch.

## Self-review notes
- Old option files: both new reads err into 900 → defaults → legacy. Covered.
- File WITH `ZOO_FOOD_MODEL` but WITHOUT `KHS_FOOD_TOT_ZOO`: second pair errs into 900 →
  default 0.5. Covered.
- 0D/benchmark path: literals `0, 0.5D0` → behaviour unchanged. Covered.
- OpenMP: option arrives as a dummy arg; no save-state in the library routine. Covered.
- genmod files under `SOURCE_CODE/build/` are compiler-generated; do not hand-edit.
