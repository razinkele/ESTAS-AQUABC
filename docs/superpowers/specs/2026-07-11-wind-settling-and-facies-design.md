# Design: Wind-modulated diatom settling (#3) + provisional sediment-facies aid (#5)

- **Date:** 2026-07-11
- **Status:** Draft (awaiting user review)
- **Author:** Arturas Razinkovas-Baziukas (with Claude)
- **Scope:** CL29 Curonian Lagoon application; converter-only (`tools/eutropy_poc/eutropy_to_estas.py`). No Fortran changes.

## 1. Context and motivation

The box-19 spring-diatom gap was diagnosed and closed earlier by lowering the diatom
settling velocity from 0.5 to 0.1 m/day (`CL29_DIATOM_SETTLING`, commit 7cd934f). The 0.1
value is a **hand-tuned constant**: a lumped stand-in for "this shallow, wind-mixed lagoon
retains diatoms in the water column." It works (5-yr validation holds), but "settling = 0.1
because it worked" is a weak spot for a model heading toward publication/application.

Two backlog recommendations remain:

- **#3** — recast the tuned constant as a physically-structured, wind-driven settling velocity
  (adds temporal/interannual realism; the annual *level* stays calibration-set — see §1.1/§2.3).
- **#5** — a per-box sandy/muddy sediment-facies map (+ the §4.2 biogenic-Si mechanism it enables).

### 1.1 Load-bearing finding: the wind is nearly aseasonal in spring–summer

Monthly-mean wind speed at Nida (ERA5, 2012–2016):

| Season | Wind (m/s) |
|---|---|
| Winter (DJF) | 7.1–8.8 (windiest) |
| Spring (MAM) | 6.12 |
| Summer (JJA) | 5.86 (calmest) |
| Autumn (SON) | 6.2–7.7 |

**Spring/summer ratio = 1.04.** Spring is *not* meaningfully windier than summer here, so
wind-modulated settling will **not change the spring diatom bloom**. This is an explicit,
data-grounded expectation, not a hoped-for improvement.

Calibration of the chosen formulation (§2.2, inverse-quadratic `w_eff = w0/(1+(U/U_c)²)`) against
the real daily wind, with `w0`=0.3 pinned:

```
U_c=3.5 -> ann-mean 0.080     U_c=4.2 -> 0.100     U_c=5.0 -> 0.121
```

At the calibrated `U_c`=4.21 m/s the annual mean is 0.100 (matches the validated constant);
spring 0.110 ≈ summer 0.116 (ratio 0.95 — if anything summer settles slightly *more*, being
slightly calmer). So #3 is **designed to preserve** the validated bloom — confirmed by the 5-yr
integration gate (§2.10), not asserted by construction — while adding genuine day-to-day and
interannual structure. Its value is **mechanistic defensibility and robustness, not a bloom fix.**

**Well-conditioned across the wind distribution.** Unlike a subtract-and-floor form (which pins
~60% of days at its floor in this windy climate, median 6.5 m/s), the divisive form gives nearly
every day a distinct settling value: realized daily `w_eff` spans [0.015, 0.270] with median
0.089 and only ~2% of days near an extreme. The temporal structure is real, not concentrated in
the calm days. Interannual spread is modest but present (per-year means 0.095–0.103 over
2012–2016).

**Honest accounting.** The annual *level* is still calibration-set: `w0`=0.3 is pinned to the
slow-sinking end of the diatom range (apt for actively-growing spring cells) and `U_c`=4.21 m/s
(a "half-suppression wind" marginally below the ~5–6 m/s fine-sediment resuspension threshold) is
fitted so the mean reproduces the validated 0.1. #3 does not eliminate calibration — it recasts a
constant velocity as a physically-structured, always-positive response whose level is set by a
near-physical shear scale rather than an arbitrary constant or floor (see §2.3).

## 2. Part A — Wind-modulated diatom settling (#3)

### 2.1 Enabling structural facts (why this is converter-only)

- ESTAS stores settling velocities as `TIME_SERIE` objects (`mod_PELAGIC_BOX_MODEL.f90:60`)
  and advances each series' `TIME_INDEX` every timestep (`mod_SIMULATE.f90:274`). The solver
  is agnostic to whether a series is constant or time-varying — the physics lives entirely in
  the numbers the converter writes to `SETTLING_VELOCITY_TS_1.txt`.
- The settling flux is computed as `MASS = SURFACE_AREA(i) · SETTLING_VELOCITIES(i,j) · C`
  (`mod_SOLVER.f90:1187–1241`); dividing that mass rate by box volume (`area · depth`, depth
  from `mod_SOLVER.f90:516`) gives a per-box concentration loss of `w·C/depth`. So a single
  basin-wide `w(t)` automatically hits shallow boxes hardest (box-19 at 1.5 m loses ~9× faster
  than the 13 m strait box). Depth-dependent behavior falls out for free.
- The wind record is a single ERA5 grid cell (Nida). A per-box wind *forcing* would fabricate
  spatial structure the data does not contain, so a basin-wide series is the honest resolution.
- **Settling is already modified in-solver.** The converter enables, for every box and state
  var, a `CHLA_SUPRESSION_OF_SETTLING` factor (`mod_SOLVER.f90:768,782` → `SETTLING_VELOCITIES
  = SETTLING_VELOCITIES · SETTLING_VELOCITY_FACTORS`) that reduces settling when chlorophyll is
  high, plus a `SHUT_DOWN_SETTLING`/`SETTLING_ON` gate. So the *effective* diatom settling the
  solver applies is `w_eff(t) · chla_suppression(i,t)`, not `w_eff(t)` alone. Wind modulation
  composes multiplicatively with these. Because the identical chla-suppression multiplies both
  the constant baseline and the wind-modulated run, the §1.1 calibration (matching mean `w_eff`
  to 0.1) stays the correct comparison basis — but the absolute in-model settling is lower than
  the nominal `w_eff`, and the two runs' chla trajectories can feed back, so the 5-yr gate
  (§2.10) is what confirms equivalence, not the nominal mean.

### 2.2 Physical formulation

Effective net diatom settling velocity, evaluated per model day *t*:

```
w_eff(t) = w0 / ( 1 + (U(t) / U_c)² )
```

- `U(t)` — daily-mean wind speed (m/s), aggregated from hourly ERA5.
- The `U²` term follows wind-wave **bottom shear stress ∝ U²**, the physical driver of
  resuspension in shallow water. As a *divisive* (saturation) form it reduces settling smoothly
  with shear and **never clips**: `w_eff → w0` as `U → 0`, `w_eff → 0` as `U → ∞`, always
  positive. At `U = U_c` settling is exactly halved — hence "half-suppression wind."
- **No floor parameter.** The response is well-conditioned across the whole wind range (§1.1;
  only ~2% of days near an extreme), so no hard floor is needed and none is used. This is the
  fix for the subtract-and-floor pathology, where `f_min` floored ~60% of days and became the
  de-facto calibration knob.

**Aggregation choice.** `U(t)` is the **daily-mean** wind speed. Because the model advances
daily, sub-daily storm peaks are smoothed — a day with a short storm and otherwise calm winds
is represented by its mean, which understates the storm's instantaneous shear (shear ∝ `U²`,
so `mean(U²) ≥ (mean U)²`). This is an accepted first-iteration simplification consistent with
the daily calibration in §1.1. A documented refinement is to aggregate the daily **mean of
`U²`** (shear-preserving, registers within-day storms) instead of `(mean U)²`; deferred to keep
the committed wind artifact parameter-free and the formulation transparent.

### 2.3 Parameters

| Param | Symbol | Value | Role & basis |
|---|---|---|---|
| Intrinsic calm-water diatom settling | `w0` | 0.3 m/day | **Physical, pinned.** Slow-sinking end of the diatom range (~0.1–2 m/day), apt for actively-growing spring cells. The `U→0` limit. |
| Half-suppression wind | `U_c` | 4.21 m/s | **Calibration lever (near-physical).** The wind at which settling halves — marginally below the ~5–6 m/s fine-sediment resuspension threshold. Fitted so annual-mean `w_eff` ≈ the validated 0.1 (`U_c`=3.5→0.080, 4.2→0.100, 5.0→0.121). A physical shear scale, smoothly varied — not a clip. |

**Honest accounting of degrees of freedom.** `w0` is pinned to literature. `U_c` is the one
calibrated parameter — it sets the annual *level*, playing the role the old `0.1` constant did,
but as a near-physical half-suppression wind (close to the resuspension threshold) rather than an
arbitrary value, and without the flooring pathology (the whole wind distribution shapes the
response — §1.1). This `w0`=0.3 / `U_c`=4.2 pairing is a deliberate compromise: a lower `w0`=0.2
would place `U_c` exactly at the ~6 m/s threshold, a higher `w0`=0.5 would push `U_c` down to an
unphysical 2.8 m/s — 0.3 keeps both numbers defensible. So #3 trades a constant velocity for a
calibrated shear scale plus genuine temporal structure. The 5-yr integration gate (§2.10) is the
authoritative confirmation that the bloom survives. Citation for `w0` to be finalized by the
domain expert (candidate: Reynolds on phytoplankton sinking). `U_c` is a fitted quantity, so it
is reported, not cited.

### 2.4 Scope: diatoms only (settling slot 1), this iteration

Wind resuspension physically acts on all particulates, but modulating detritus (slot 2,
`DET_PART_ORG_C/N/P`) and biogenic Si (slot 4, `PART_Si`) would perturb benthic **deposition
fluxes** and pull the sediment-diagenesis coupling into the validation surface. Diatoms
(slot 1, `DIA_C`) are the validated lever and the box-19 driver, so the change is isolated
there. The fixed-other-particulates simplification is a documented known limitation with a
clear extension path (apply the same `w_eff/w0` factor to slots 2 and 4 in a later iteration,
re-validating benthic fluxes).

### 2.5 Data and provenance

- **Source:** `~/eutropy/input/era5_wind_nida_2012_2016.csv` — hourly `time,u10,v10,wind_ms`,
  43 848 rows, 2012-01-01 00:00 … 2016-12-31 23:00.
- **Committed artifact:** aggregate hourly → **daily-mean** wind into
  `tools/eutropy_poc/net/wind_daily.csv` (columns `day,wind_ms`, 1827 rows, day 0 = 2012-01-01).
  Committing the small daily file keeps CL29 self-contained and CI-reproducible without the
  external `~/eutropy` tree.
- **Regeneration:** a documented one-shot helper (script or a `--regen-wind` path) rebuilds
  `wind_daily.csv` from the ERA5 hourly source, recording provenance. The committed daily file
  is authoritative for conversion.
- **Attribution/licence:** the source is ERA5 (Copernicus Climate Change Service / ECMWF), which
  is freely redistributable **with attribution**. The committed `wind_daily.csv` header must
  carry an ERA5/Copernicus attribution line and generated-Contains-modified-Copernicus-data
  notice, so the derived file is licence-clean to ship in-repo.

### 2.6 Timeline alignment (exact, no clamping)

The converter's time base is **day 0..1826 = 2012-01-01..2016-12-31** (stated in the converter
header). The daily wind file spans the identical 1827 days. Alignment is index-identity:
`w_eff` for model day *d* uses `wind_daily.csv` row *d*. Defensive handling: if lengths ever
differ, clamp to the shorter and cycle/repeat the last value, emitting a `log`/warning.

### 2.7 Graceful degradation (byte-identical fallback)

Wind modulation is purely additive. `SETTLING_VELOCITY_TS_1.txt` reverts to today's exact
2-point constant series when **either**:

- `CL29_WIND_RESUSPENSION` is `False`, **or**
- `wind_daily.csv` is absent (fresh clone / CI without the external tree).

In both cases the converter writes `times=[0,9999], values=[[v],[v]]` with
`v = CL29_DIATOM_SETTLING` exactly as now → the CL29 output is **byte-identical** to the
current baseline. This preserves the existing "runs from a fresh clone" property and keeps the
feature reviewable in isolation.

### 2.8 Config constants (beside `CL29_DIATOM_SETTLING`)

```python
CL29_WIND_RESUSPENSION = True    # master toggle; False -> constant settling (byte-identical)
CL29_SETTLING_W0       = 0.3     # intrinsic calm-water diatom settling, m/day (physical, pinned)
CL29_WIND_UHALF        = 4.21    # half-suppression wind, m/s (calibration lever: sets annual mean, §2.3)
```

`CL29_DIATOM_SETTLING` (0.1) is retained as the constant-mode value and the fallback.

### 2.9 Converter changes

- New helper `wind_daily_series()` (or similar): read `wind_daily.csv`, return per-day
  `wind_ms` list; return `None` if the file is absent.
- New pure helper `wind_modulated_settling(wind_list, w0, uhalf)` → per-day `w_eff` list,
  applying §2.2 (`w0/(1+(U/uhalf)²)`). Pure and unit-testable.
- In the settling-TS write block (currently ~lines 674–680): when resuspension is enabled and
  wind data is present, write slot-1 as a daily series `times=[0,1,…,1826]`,
  `values=[[w_eff(0)],…,[w_eff(1826)]]` (one row per model day); otherwise unchanged.
  Slots 2–6 unchanged.

### 2.10 Testing and validation

- **Unit — formula:** `w_eff(0)=w0`; `w_eff(U_c)=w0/2`; strictly decreasing in `U`; values in
  `(0, w0]` (always positive, no floor); `w_eff → 0` as `U` grows.
- **Unit — fallback:** toggle off → slot-1 TS byte-identical to constant mode; wind file absent
  → same.
- **Unit — series:** enabled path yields a 1827-row series, values in range, day-0 finite.
- **Integration — 5-yr CL29:** spring diatom peak 1.1–1.9 mgC/L; summer domain cyano
  ~96 mg Chl/m³ (≈2–4 mgC/L); seasonal succession intact; 0 NaN. Compared against the
  constant-0.1 baseline (expected: close, per §1.1).

## 3. Part B — Provisional facies strawman + decision aid (#5)

### 3.1 Why the map cannot be automated

There are **no box coordinates** anywhere in the repo or the `~/eutropy` tree (no shapefiles,
no lat/lon; boxes are defined only by connectivity, depth/area, and forcing). So even a
published Curonian sediment map cannot be rigorously overlaid onto box numbers by tooling. The
converter comment already established this and proved a depth heuristic wrong (box-19 is the
muddy interior exemplar yet shallow, 1.52 m). The authoritative map must come from the domain
expert or a published map keyed to box IDs.

Two illustrative `BOX_TYPES` sand/mud dicts do exist in the analysis tooling
(`aquabc_analysis_utils.py`: `{5,6,8,9:sand; 14,17,25:mud}`;
`deep_process_rate_analysis_v1.py`: `{5:mud,6:sand,8:mud,9:sand,…}`). They are **analysis-only
grouping labels for output plots, unsourced, and mutually inconsistent** (boxes 5 and 8 are
`sand` in one and `mud` in the other). They are therefore *not* an authoritative facies map —
if anything their disagreement reinforces that a supplied map is required.

### 3.2 Provisional strawman (clearly labeled provisional)

Salinity cleanly separates the boxes by marine influence, which tracks lagoon geography and
depositional energy (`forcing_salt.csv`, day 0):

- **Marine-influenced (sal ≈ 6–7; northern/strait, higher energy → likely sandy):**
  1, 4, 7, 10, 11, 12, 13, 16, 20, 22
- **Transitional (sal ≈ 3.7):** 17, 18
- **Freshwater (sal ≈ 0.1–0.6; central/southern low-energy basin → likely muddy):**
  2, 3, 5, 6, 8, 9, 14, 15, 19, 21, 23, 24, 25, 26, 27, 28, 29

Box-19 lands muddy (sal 0.52), matching the spec's interior-muddy exemplar.

**This is a low-confidence strawman for expert correction, not a classification.** It is
demonstrably imperfect: boxes 5, 6, 8, 9 are fully freshwater (sal ≈ 0.10) yet are labeled
`sand` in the analysis `BOX_TYPES` (§3.1) — freshwater ≠ muddy (riverine and high-energy
nearshore sands exist). Salinity is one weak axis only. The expert map should reconcile
salinity, depth, the (conflicting) `BOX_TYPES` labels, and a published Curonian facies map,
and will override this strawman wherever they disagree.

### 3.3 What ships for #5 this session

- The provisional strawman + a salinity/depth/topology decision-aid table, surfaced at the
  existing `CL29_SEDIMENT_TYPE = {}` fill-in point (kept empty → CL29 stays byte-identical).
- **Deferred (documented follow-up, not this session):** activating the two-type author and
  implementing the §4.2 biogenic-Si-deposition mechanism. That half can only be validated
  against the sandy/muddy contrast, which requires the confirmed map. Building it now would be
  dormant, unvalidatable sediment-input logic — exactly the silent-breakage class this codebase
  has repeatedly hit.

## 4. Risks and mitigations

| Risk | Mitigation |
|---|---|
| Wind modulation perturbs the validated bloom | `U_c` fitted so the annual mean lands at 0.100; the 5-yr integration gate (§2.10) must pass before promotion. If it fails, `U_c` may move within a plausible window (≈3.5–5.0 m/s → mean 0.080–0.121) to re-center the level — a single near-physical knob, no floor. |
| Overstating the "removes the magic constant" benefit | Disclosed honestly (§1.1, §2.3): `w0` is physical; `U_c` is a calibrated *level* analogous to the old constant. The real benefit is genuine temporal/interannual structure (well-conditioned across all days), not eliminating calibration. |
| External wind file breaks fresh-clone/CI runs | Committed small daily file + absent-file fallback → byte-identical baseline (§2.7). |
| Provisional facies map mistaken for authoritative | Labeled provisional in code + docs; `CL29_SEDIMENT_TYPE` left empty so nothing activates until the expert confirms. |

## 5. Files touched / out of scope

- **Touched:** `tools/eutropy_poc/eutropy_to_estas.py` (config + helpers + settling-TS write);
  `tools/eutropy_poc/net/wind_daily.csv` (new committed artifact); wind-regeneration helper;
  `tests/python/` (new unit tests). Docs: this spec; converter comments; CHANGELOG on promotion.
- **Out of scope:** any Fortran change; modulating detritus/PSi settling; per-box resuspension
  threshold; two-type author activation; §4.2 Si mechanism; facies map authoring.

## 6. Open questions

None blocking. Three items for the expert to finalize post-implementation: (a) canonical
citation for `w0` (the diatom calm-water sinking velocity; `U_c` is fitted, so it needs no
citation, only reporting); (b) confirm/correct the provisional facies map when #5's second half
is scheduled; (c) reconcile the two mutually-inconsistent analysis `BOX_TYPES` dicts (§3.1)
against the confirmed map — they should be de-duplicated into a single sourced classification
once one exists.
