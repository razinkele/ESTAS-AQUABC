# Activate Effective-Depth Light via Real Wind Forcing — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement
> this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the cyanobacteria surface-positioning light mechanism — which already exists in
the code — actually engage, by replacing the constant-4 m/s placeholder wind forcing with the
real 2012–2022 daily wind record, then measure whether a physically honest configuration
(§15/§17 optics) can finally hold the observed summer bloom.

**Architecture:** No Fortran change in the core path. The mechanism (Petras 2019, all three
cyano `_BOUYANT` libs): `EUPHOTIC_DEPTH = 4.61/kd`, `MIX_DEPTH = 0.8121·W + 0.7006`
(Nagy et al. 2006), effective light depth = euphotic / mixed / full cascade. It never engages
because `INPUTS_CL29/WIND_SPEED_TS.txt` is a 2-point constant 4.0 m/s → MIX_DEPTH ≡ 3.95 m >
euphotic always. This plan is data + experiments; Fortran only if Task 5's gate refinement is
demanded by the numbers.

**Ground truth (verified 2026-08-13, this session):**
- CL29 calls `CYANOBACTERIA_BOUYANT` / `FIX_CYANOBACTERIA_BOUYANT` / (NOST lib) — the
  positioning variants. The non-buoyant `FIX_CYANOBACTERIA` (with the vestigial ×1.0
  multiplier) is NOT the CL29 path. `CYANO_BOUYANT_STATE_SIMULATION` is a dead flag —
  positioning is unconditional in the `_BOUYANT` routines, gated only by wind/kd/depth.
- Wind consumers beyond the gates: KAWIND reaeration, the wind-settling arc (v0.3.x), and
  possibly resuspension — Task 3's A/B must attribute effects across these channels.
- ERA5 hourly Nida wind: `~/eutropy/era5_raw/era5_wind_nida_{2012..2016}.nc`; builder
  `~/curonian/resusp_grids/build_wind.py`. Measured wind: KM hydrometeo workbooks 2014–2023
  (`~/curonian/DATA/2014-2023_BJ duomenys extrahuoti/Hidrometeorologiniai matavimai/`).

## Global constraints
- `INPUTS_CL29` is gitignored → every adopted input change is versioned in ESTAS-AQUABC-DATA.
- Runs: proven symlink-farm harness (scratchpad `run_*.py` family), `ESTAS_HOLD_VOLUME=1`,
  full record 4016 d, boxes 7/14/17/23 for PROCESS_RATES.
- Scoring: `validate_cl29_vs_epa.py` EPA + plankton obs + `--phase`; `--wconst` whenever the
  run's C:Chl differ from defaults. Never regenerate obs CSVs while a DE runs.
- Adoption discipline: A/B vs the adopted baseline; subsets before full optima; user decides.

### Task 1: build the 2012–2022 daily wind series

**Files:**
- Create: `tools/build_wind_forcing.py` (repo, committed)
- Create: `INPUTS_CL29/WIND_SPEED_TS.txt` replacement (adopted only in Task 6; until then a
  scratch copy) — format: the existing header (`DATA_SIZE n`, `NUMBER_OF_VARIABLES 1`,
  scale/unit 1.0, `INTERPOLATE 1`) + `t  speed` rows, t in days from 2012-01-01.

**Steps:**
- [ ] 1. Sources: ERA5 u10/v10 → daily mean speed for 2012–2016 (reuse `build_wind.py` logic);
  KM hydrometeo workbooks → daily station wind 2014–2023 (new parser; expect the usual
  Lithuanian-workbook traps — verify header rows per file year like the zoo archive).
- [ ] 2. Cross-validate on the overlap (2014–2016): daily r and bias ERA5 vs KM. Accept if
  r ≥ 0.8; document the offset. Choose the primary source per era (ERA5 2012–2013, KM or
  ERA5 2014–2022 by whichever validates better); no gap > 3 days uninterpolated.
- [ ] 3. Write the TS file; unit test the writer (header counts, monotonic t, 4017 coverage).
- [ ] 4. Sanity: Jun–Sep daily distribution (mean ~4–6 m/s, calm tail p10 ≤ 2.5 m/s expected);
  plot-free percentile table into the task log.
- [ ] 5. Commit tool + tests (`feat(obs): daily wind forcing builder 2012-2022`).

### Task 2: engagement statistics (no model run)

- [ ] 1. Script (scratch): daily `MIX = 0.8121·W + 0.7006` vs `EUPHOTIC = 4.61/kd` for
  kd ∈ {transparent ≈ 1.7 + 0.02·Chl, honest ≈ 2.9} and box depths {1.5, 2.5, 3.8} m.
  Output: % days per month in each gate branch (positioned-at-euphotic / mixed-layer / full).
- [ ] 2. Record in the plan log. Decision point: if positioning < 5 % of Jun–Sep days even
  under honest kd, pre-register Task 5 (gate refinement) as likely needed.

### Task 3: A/B — real wind on the ADOPTED config (attribution run)

- [ ] 1. Scratch run: adopted config + new WIND_SPEED_TS only. Full record + PROCESS_RATES.
- [ ] 2. Score the standard grid. Attribute changes: light factors (CYN/FIX/NOST slots) on
  calm days = positioning; DO shifts = KAWIND; settling/resuspension side-effects via
  Si/TP. Expected: modest — under transparent optics euphotic ≈ 2.7 m, so positioning
  engages only on calm days.
- [ ] 3. Verdict: is real wind adoptable on its own merits (forcing realism + no regressions)?
  Present to user either way.

### Task 4: the decisive run — Scheffer package + real wind

- [ ] 1. Scratch run: honest optics (K_B_E 2.18, C:Chl 53/78) + CYN T_min 5 + BETA_CYN 2 +
  real wind. This is §17 with the positioning gate able to open.
- [ ] 2. Score vs the §17 table: does Aug move from 14.2 toward 50.8 while Feb/Mar stay exact?
  Groups, phase, nutrients, zoo — full grid, `--wconst`.
- [ ] 3. Honest-evaluation caveat to carry in the write-up: EPA chlorophyll is surface-sampled;
  the model value is a box average. When positioning engages, the true comparison quantity
  diverges — if Task 4 lands short of observations, quantify how much a surface-layer
  concentration estimate (biomass concentrated into the effective depth) would close, as a
  diagnostic only.
- [ ] 4. Decision gate with user: adopt real wind alone (Task 3), the full package, or neither;
  any adoption → recalibration pass (N-cycle four + KG under the final optics) per the
  established subset discipline.

### Task 5 (conditional): gate refinement

Only if Tasks 2/4 show the Nagy mixing-depth gate under-engages against observed scum
frequency: replace/augment with a flotation-velocity criterion (turbulent friction velocity
vs gas-vesicle flotation speed, Visser et al.; Wallace & Hamilton) or a sub-daily
calm-fraction weight. This IS a Fortran change → its own mini-plan, byte-gate, opt-in flag
per the `ZOO_FOOD_MODEL` pattern. Do not start it inside this plan.

### Task 6: adoption, docs, paper hook

- [ ] 1. Whatever the user adopts: sync `INPUTS_CL29` → ESTAS-AQUABC-DATA with the evidence
  trail in the commit message.
- [ ] 2. `docs/CL29_phenology_diagnosis.md` §18: the disabled-mechanism finding (constant wind
  since inception), engagement statistics, both A/B tables, decision.
- [ ] 3. Paper draft v0.5 hook: this is the fourth invisible-error class instance — a correct
  mechanism disabled by placeholder *forcing* (neither parameter nor formulation). Slots
  into §3.11/discussion taxonomy.
- [ ] 4. BACKLOG: cleanup note for the dead `CYANO_BOUYANT_STATE_SIMULATION` flag and the
  vestigial non-buoyant `FIX_CYANOBACTERIA` ×1.0 multiplier (housekeeping, not this plan).

## Self-review notes
- The 2019 mechanism comment says "Nothing is done to increase selfshading. Concentration is
  still evenly distributed" — positioning raises growth without concentrating self-shading;
  conservative in the right direction for a first activation, note in §18.
- KAWIND/settling/resuspension all see the new wind — Task 3 exists precisely to catch
  regressions there before any package conclusion; if resuspension responds strongly, Si/TP
  shifts must not be attributed to positioning.
- ERA5 years 2017–2022 may need a fresh CDS fetch (eutropy fetch scripts; network/credentials
  assumed present since era5_raw exists). If unavailable, KM measured wind covers 2014–2023
  and ERA5 2012–2013 the remainder — the cross-validation step decides trust.
- The wind file is read with `INTERPOLATE 1`; daily means are the right cadence (model is
  daily-forced elsewhere); no sub-daily ambition in this plan.
