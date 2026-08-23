# NOST akinete life-cycle staging (opt-in `NOST_STAGE_MODEL`)

**Date:** 2026-08-23 · **Status:** approved design, awaiting implementation plan ·
**Review:** adversarially workflow-reviewed (23 findings, 10 verified-confirmed, 0 refuted — §8)

## 1. Summary

Give the Nostocales guild a working life cycle: a benthic akinete bank (`BED_AKI`, per-box,
non-transported), growth-viability-gated germination, an energy-cue (filtered declining
radiation) akinete formation, and akinete settling — as an opt-in (`NOST_STAGE_MODEL=1`) with a
byte-identical default, following the `CYANO_POS_MODEL` precedent in every mechanism it can
reuse. The scope decision (2026-08-23, user-directed): **repair NOST staging only**; the
`FIX_CYN` surrogate is untouched, and the fixer-role question is deferred until this runs.

## 2. Motivation — measured, not inferred

Full-record run of the adopted operational configuration (post-`faaccf0`, scores reproduce doc
§26/§27 to the digit):

- **The whole NOST guild is a 2012 initial-condition transient.** Box 5: initial `AKI_C` 8.0
  → one 2012 bloom (VEG 1.7 mg C/L) → pool ~0 by 2013 → flat zero for ten years. Slow-drain
  boxes (1, 4, 10, 12, 13) hold their initial pool about a decade; by 2022 every box is empty.
  No box ever banks a new akinete.
- **Transport export, not germination, is the dominant drain** of the water-column akinete
  pool: box 5 (river-influenced — high DIN keeps the germination gate mostly closed there)
  drained fastest. A transported overwinter pool cannot hold an inoculum in a lagoon with
  months-scale residence time; real akinetes overwinter in the sediment.
- **The current transition logic is a one-way pipe** (`aquabc_II_pelagic_lib_NOSTACALES.f90`
  ~396–433): germination `DIN < 0.1 && T > 21 °C` (July-only); formation
  `T < 16 °C && doy > 200` — but 16 °C is also the NOST growth CTMI T_min, so formation begins
  exactly when growth stops and the population crashes before banking. `K_LOSS_AKI` and
  `K_MORT_AKI_20` are 0; the akinete boundary (var 32 in `FORC_TS_1.txt`) is 0.
- **Context for expectations** (doc §28): the model's fixer complex (`FIX_CYN` surrogate) is
  already OVER vs obs `FIX_TOT` in Jun–Oct (Jun 7.4×, an early start) and UNDER in Nov. The
  October chlorophyll gap belongs to CYN/DIA/OPA, **not** to fixers. This design's honest
  targets are: a self-sustaining staged guild (currently extinct), the June early-start
  (inoculum constraint), November persistence, and species realism (*Aphanizomenon* IS the
  akinete-forming type). It is NOT expected to close the October chlorophyll gap.

## 3. Goals and non-goals

**Goals**
1. A self-sustaining multi-year akinete cycle: bank → germinate into growth-viable water →
   bloom → re-bank, with inter-year memory, in the CL29 transport regime.
2. Opt-in, flag=0 byte-identical, both temperature models and both solvers correct.
3. A measured ladder whose criteria can distinguish "self-sustaining" from the two known
   failure modes (sterile recycle; inoculum preserved but growth still fails).

**Non-goals**
- Changing `FIX_CYN`, its parameters, or the fixer competition outcome by design (it is a
  *measured output* of the ladder, not a target).
- Stage-specific in-water buoyancy (approach C) — the positioning ratchet already covers the
  vegetative stage; rejected as YAGNI.
- Closing the October CHLA/PO4/DO residuals (they belong to the autumn CYN/diatom guilds).
- Resuspension of the akinete bank (guarded + deferred, §6.3).

## 4. Design

### 4.1 States and module

- `NOST_VEG_HET_C` (state 31) and `AKI_C` (state 32) remain transported state variables,
  unchanged in the transport framework.
- New: `BED_AKI(nkn)` in **g C/m²** (note: NOT mg — `AKI_C` is g C/m³ ≡ mg/L, so
  `V_SETTLE_AKI [m/d] × AKI_C [g C/m³] = g C/m²/d` with no scale factor), held in a new module
  `AQUABC_NOST_STAGING` (`SOURCE_CODE/AQUABC/PELAGIC/aquabc_nost_staging.f90`), modeled on
  `AQUABC_POSITIONING_STATE`: allocatable state, `INIT/RESET/SET_..._PARAMS` routines,
  per-chunk access under OpenMP.
- The module also holds the per-box **flux-export arrays** (`STG_SETTLE_FLUX`,
  `STG_GERM_FLUX`, `STG_FORM_FLUX`, g C/m²/d), the gate export `STG_GERM_COND` (logical),
  exact cumulative integrals (`CUM_SETTLE_AKI`, `CUM_GERM_AKI`, `CUM_FORM_AKI`, `BURIED_AKI`,
  g C/m²), and one per-box scalar `RAD_EMA` (W/m², the filtered radiation, §4.3). `BED_AKI` starts at 0 — the existing 2012 initial `AKI_C` banks itself via settling
  (§4.2); no new initial-condition machinery. Banking e-folding is `DEPTH/V_SETTLE_AKI`
  (~6–8 d in shallow boxes, weeks-to-months in the 12–21 m boxes) — this is the stated
  spin-up, handled in §7 by excluding 2012–2013 from scoring.

### 4.2 Process structure (all of it active only when `NOST_STAGE_MODEL = 1`)

Rates as seen by the water column (g C/m³/d):

| Process | Formula | Gate |
|---|---|---|
| Akinete settling (AKI_C sink) | `(V_SETTLE_AKI / DEPTH) · AKI_C` | always on |
| Bed germination (VEG source) | `R_GERM_BED_AKI = KR_GERM_BED · BED_AKI / DEPTH` | `DIN < KN_GERM_AKI` **and** `LIM_KG_NOST_VEG_HET_TEMP > 0.05` **and** `TEMP > T_GERM_AKI_STAGE` **and** formation latch OFF |
| Akinete formation (VEG → AKI_C) | `KR_FORM_AKI · NOST_VEG_HET_C` (rate unchanged) | formation latch ON (§4.3) |
| Water-pool germination | `R_GERM_NOST_AKI ≡ 0` | forced off under the flag |
| Legacy water losses | `K_LOSS_AKI`, `K_MORT_AKI_20` terms unchanged | (0 in CL29) |

Bed budget (g C/m²/d), integrated solver-side (§4.4):

```
dBED_AKI/dt = V_SETTLE_AKI·AKI_C  −  KR_GERM_BED·BED_AKI  −  K_MORT_BED_AKI·BED_AKI
```

Bed mortality is burial: carbon exits the model (same fate as today's `R_LOSS_AKI`).
Stoichiometry: akinetes carry the fixed NOST N:C / P:C; transitions move carbon with
nutrients implicit — unchanged from the current code.

**The germination gate is the design's load-bearing fix.** The review measured that a plain
`T > 12 °C` gate germinates a median **65 %** (30–88 %) of the overwintered bank into a
late-May–June window where DIN is already low but NOST growth is exactly zero (CTMI T_min 16),
an unrecoverable pure loss. `LIM_KG_NOST_VEG_HET_TEMP > 0.05` — the growth-window factor the
routine has already computed — closes both this and the autumn dead-water leak, tracks any
recalibration of the cardinals, and stays correct under `TEMPERATURE_MODEL=0` (where growth
below 16 °C is NOT zero and a hard-coded 16 would wrongly suppress germination). The 0.05
epsilon is a hardcoded parameter of the gate, documented in code. `T_GERM_AKI_STAGE` (12 °C)
remains as a pre-season guard; mutual exclusivity with the formation latch breaks the
autumn bed→VEG→AKI→bed futile cycle (only ~55 % recovered per pass).

**`KR_GERM_BED = 0.05/d` makes the bank an annual relay, not a multi-year buffer** — the
DIN window (60–100+ open days) drains ~95 % of the bank each season. This is accepted and
stated: the inter-year memory is the *post-formation* bank, and the fragility rung (§7 V7)
measures what one failed bloom year does to it. A genuine multi-year refuge would need
`KR_GERM_BED` small enough to drain < 1 e-fold per season, trading against seeding strength —
an explicit calibration trade-off, not a hidden one.

### 4.3 Formation cue: filtered declining radiation with a seasonal latch

The CLC termination cue is declining light (Hense: "energy limitation due to decreasing solar
radiation … initiates the formation of resting cells"). A raw daily threshold is wrong twice
over (fires on 6.3 % of Jun–Aug days in the measured CL29 forcing; stays on all winter), so:

- `RAD_EMA` = exponential moving average of the **raw daily surface solar radiation**
  (`DRIVING_FUNCTIONS(:,3)`, W/m² — NOT `I_A`, which is PAR in langley/day) with a 7-day
  e-folding time: `RAD_EMA += (dt/7d)·(DF3 − RAD_EMA)`. One scalar per box in the staging
  module; initialized to the first forcing value (spin-up ≪ the excluded 2012–2013).
  `RAD_EMA` and the latch are updated in the **solver-side once-per-step staging update**
  (§4.4) — never inside the kinetics, where the RK2 double evaluation would apply the
  increment twice and silently halve the filter's time constant. The kinetics only READ them.
- **Latch ON** when `RAD_EMA < I_FORM_AKI` (default 120 W/m²): measured on the actual forcing,
  first crossing Aug 31–Sep 25 across all 11 years, median **Sep 14**, zero Jun–Aug firings —
  a 7-day window specifically (5-day trips on 2017's dark Jun 30–Jul 2).
- **Latch OFF** (reset) when the germination gate's **other three conditions** (`DIN`,
  growth-viability, `T_GERM`) first co-occur in spring — defined on the non-latch conditions
  so the rule is not circular; germination then starts on the same day the latch releases.
  (In practice DIN > 0.1 all spring, so the latch holds until the June niche opening; winter
  formation is harmless because VEG ≈ 0.) The latch prevents flicker around the threshold and
  gives formation a defined season; while it is ON, germination is off (mutual exclusivity,
  §4.2).
- Recorded alternative (not chosen): photoperiod `FDAY < 0.53 ∧ declining` — zero new
  plumbing (`ENV_CHUNK%FDAY` exists), but it is a calendar envelope rather than the energy
  cue itself.

The ladder reports the Oct–Nov VEG trajectory explicitly (§7 V6): the 0.1/d formation drain
must hold, not terminate, the autumn population this design exists to create.

### 4.4 Numerical integration: solver-side, exactly conservative under both solvers

`BED_AKI` is NOT integrated inside the kinetics call. Under the `ESTAS_PELAGIC_SOLVER` RK2/Heun
opt-in, kinetics are evaluated twice per step — an in-kinetics update would double-bank mass.
(The review found `S_POS` carries this same latent issue today; filed separately in BACKLOG,
out of scope here.) Instead:

- The kinetics **compute and export** the settle and bed-germination fluxes into the module's
  export arrays, *overwriting* (never accumulating) on every evaluation.
- The ESTAS step loop integrates `BED_AKI` **once per completed step** after the state update:
  Euler uses the single exported flux; RK2 uses `0.5·(F_stage1 + F_stage2)·dt` — the same
  average the transported `AKI_C`/VEG sides receive through the solver, so bed + water carbon
  stay exactly complementary. The same once-per-step update advances `RAD_EMA` and the
  formation latch (§4.3). Verified by ladder rung V4.

### 4.5 Configuration: graceful options lines — no constants flag-day

**No WCONST/`nconst` change.** Appending model constants is a hard two-repo flag-day
(`nconst=323` is hardcoded in `mod_GLOBAL.f90` and `aquabc_II_pelagic_interface.f90`, echoed
in every setup's `PELAGIC_INPUTS.txt` `NUM_MODEL_CONSTANTS` with error-stops, and the
fail-loud reader requires every shipped constants file index-complete). Instead, all staging
scalars follow the positioning-constants precedent — appended to `PELAGIC_MODEL_OPTIONS.txt`
**after `W_DISP_POS`, before the filename lines** (position is load-bearing; the reader is
positional with a graceful `end=900/err=900` tail, so files without the new lines get code
defaults and old setups in both repos keep working unchanged):

| Option line | Default | Units / meaning |
|---|---|---|
| `NOST_STAGE_MODEL` | 0 | 0 = legacy (byte-identical), 1 = staging |
| `T_GERM_AKI_STAGE` | 12.0 | °C, pre-season germination guard |
| `I_FORM_AKI` | 120.0 | W/m², formation threshold on `RAD_EMA` |
| `KR_GERM_BED` | 0.05 | 1/d, bed germination rate |
| `K_MORT_BED_AKI` | 0.001 | 1/d, bed (burial) mortality — ≈ 31 %/year, ≈ 17 % over a 6-month winter |
| `V_SETTLE_AKI` | 0.5 | m/d, akinete settling (literature 0.1–1 m/d for *Aphanizomenon*/*Anabaena*-class resting cells) |

Defaults live as module initializers in `AQUABC_NOST_STAGING` with a `SET_NOST_STAGING_PARAMS`
setter called after label 900 alongside `SET_POSITIONING_PARAMS`. The parsed flag is echoed to
stdout ("NOST staging: ON/OFF") like other options. Existing WCONST constants reused
unchanged: `KN_GERM_AKI` (#299, 0.1 mg N/L). Made inert under the flag: `P_GERM_AKI` (#298,
water-pool germination), `T_GERM_AKI` (#306, legacy 21 °C), `DAY_FORM_AKI` (#301),
`T_FORM_AKI` (#302). `KR_FORM_AKI` (#300 `P_FORM_AKI`, 0.1/d) keeps its role with the new gate.
The five staging scalars (all lines except the flag) are calibration-visible (the closure-DE
already tunes options-file scalars such as the ratchet constants).

### 4.6 Guards and interaction contracts

1. **Resuspension**: `NOST_STAGE_MODEL=1 && RESUSPENSION_OPTION>0` → `error stop` at model
   init with a clear message. `BED_AKI` sits outside the bed mass budget; storm erosion that
   ignores the akinete bank would be silently wrong. The erosion-return term is a documented
   deferred limitation (§9). CL29 runs `RESUSPENSION_OPTION 0`.
2. **Transport settling table**: the ESTAS per-variable settling entry for var 32 stays 0
   under the flag (it already is 0 in every CL29 box); the kinetic sink is the sole settling
   pathway — the transport deposition route is deliberately NOT reused because it targets the
   ESTAS bottom-sediment state, not `BED_AKI`. No double-counting is possible.
3. **Zooplankton, allelopathy, positioning**: unchanged; VEG remains grazeable and
   positioning-aware exactly as now.
4. **Mass accounting**: the settle sink and bed pool are visible to the verification suite via
   the conservation rung V4 (water `AKI_C` + `BED_AKI`·area + burial = conserved);
   `CHECK_NEW_PELAGIC_MASS` semantics unchanged (the settle sink is an ordinary derivative
   term on `AKI_C`).

### 4.7 Derivative and PROCESS_RATES wiring (flag=1 invariants)

- New rate array `R_GERM_BED_AKI` is the sole VEG germination source; `R_GERM_NOST_AKI ≡ 0`
  (the water-pool sink in the AKI_C derivative therefore vanishes — no double debit).
- `AKI_C` derivative gains the settle sink as a **new, dedicated PROCESS_RATES slot** (never
  overloading an existing slot — the per-group PROCESS_RATES layout trap is documented in this
  project); the DEPTH diagnostic slot stays where it is.
- Unit-test assertions (V1): VEG germination credit == bed debit; AKI_C germination sink == 0
  under the flag; settle debit(water) == bank credit(bed)·(1/DEPTH); all under both solvers.

## 5. Files touched

| File | Change |
|---|---|
| `SOURCE_CODE/AQUABC/PELAGIC/aquabc_nost_staging.f90` | NEW module: `BED_AKI`, `RAD_EMA`, flux exports, params + setter, init/reset |
| `.../AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_NOSTACALES.f90` | gated germination/formation, flux computation + export, new args (flag, staging params via module) |
| `.../aquabc_II_pelagic_model.f90` | call-site args, `R_GERM_BED_AKI` wiring, new PROCESS_RATES slot, resuspension guard |
| `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` | options reader lines (graceful tail) + setter call + stdout echo |
| `SOURCE_CODE/ESTAS/mod_SOLVER.f90` / `mod_SIMULATE.f90` | once-per-step `BED_AKI` integration (stage-flux averaging under RK2); `NOST_STAGING.out` writer in the serial output path at `PRINT_INTERVAL` (never inside the parallel region) |
| `.../aquabc_II_pelagic_interface.f90` | flag passthrough for the 0D call path (literal 0) |
| Existing NOSTOCALES-signature test | update for the new interface |
| NEW unit-test file | staging module + §4.7 invariants |
| `INPUTS_CL29/PELAGIC_MODEL_OPTIONS.txt` (data repo) | six lines appended — only when enabling; graceful defaults mean no other setup file changes anywhere |

`NOST_STAGING.out` (flag-on only): per box per print interval — `BED_AKI`, `RAD_EMA`, latch
state, and the three stage fluxes (settle, germination, formation). The stage fluxes are the
evidence that separates self-sustainment from recycle (§7).

## 6. Not changed

The 36-variable transport framework; boundary files; initial conditions; all legacy behavior
at flag=0 (byte-identical); `FIX_CYN` in its entirety; WCONST files and `nconst`.

## 7. Verification ladder

| Rung | What | Pass condition |
|---|---|---|
| V1 | Unit tests: staging module + §4.7 invariants | mass balance to round-off, both solvers |
| V2 | Flag=0 A/B: 30-day standard + CL29 gate | **byte-identical** |
| V3 | Flag=1 positive-control smoke (short run) | options echo shows ON; `NOST_STAGING.out` non-empty; `BED_AKI > 0` by first autumn — guards the fail-silent positional reader |
| V4 | RK2 conservation A/B (`ESTAS_PELAGIC_SOLVER` 1 vs 2, short flag=1 run) | water `AKI_C` + bed + burial conserved to round-off under both |
| V5 | Formation-timing check on the 11-yr forcing | first formation day per year ∈ [Aug 31, Sep 25]; zero Jun–Aug firings |
| V6 | Full-record ladder on the adopted config (2014–2022 scored; 2012–2013 spin-up excluded) | see criteria below |
| V7 | Fragility: synthetic failed-bloom year (germination disabled for one year) | bank survives to the following season at a reported fraction |

**V6 criteria:**
- **[a] Self-sustainment (recycle-proof):** per-box **post-formation annual maximum** of
  `BED_AKI` non-declining after spin-up; **and** annual formation-to-bed flux / annual
  germination-from-bed flux > 1 in sustaining boxes (= 1 is a sterile recycle, < 1 is decay
  masked by low bed mortality); **and** the fraction of annual germination flux occurring at
  `LIM_KG_NOST_VEG_HET_TEMP ≤ 0.05` reported (dead-water leak monitor, expect ≈ 0).
- **[b] No headline regression** beyond noise vs the adopted scores (PO4 RMSE 0.0232,
  Si 0.8235, CHLA 25.52, NH4 0.0489, seasonal r +0.70, ZOO bias ≈ 0).
- **[c] Competition, measured not targeted:** monthly `FIX_CYN_C` and `NOST_VEG_HET_C` vs obs
  `FIX_TOT`; the June overshoot (currently 7.4×) must not worsen; November (currently 0.47×)
  reported; PO4/DIN monthly shifts reported; the Oct–Nov VEG trajectory reported (formation
  drain must not clear autumn early).
- Per-box banked-fraction diagnostic (annual settle flux vs formation flux; lagoon-total
  formation vs open-boundary `AKI_C` export).

**Adoption is a separate, user-decided step** after the ladder, per house rule.

## 8. Review record

Adversarial workflow review 2026-08-23 (4 finder dimensions → refute-oriented verification;
14 agents): **23 findings; 10 verified CONFIRMED (4 blocking), 0 refuted; 5 majors
capped-unverified; 8 minors.** All confirmed findings and all recovered capped/minor findings
are incorporated above. The blocking four: (1) dead-water germination leak (→ §4.2
growth-viability gate + mutual exclusivity); (2)+(4) `V_SETTLE_AKI` undeclared (→ §4.5 sixth
option line, default 0.5 m/d, settle-race spin-up stated); (3) constants flag-day (→ §4.5
graceful options lines, no WCONST change). Key majors: raw radiation cue fires mid-summer
(→ §4.3 7-day EMA + latch, measured Sep 14 median onset); RK2 double-banking (→ §4.4
solver-side integration); germination below growth T_min (→ merged into §4.2); resuspension
silent incompatibility (→ §6.1 guard); files-touched under-scoping (→ §5); fail-silent
options reader (→ V3); criterion [a] recycle-blindness (→ V6[a]). Minors: unit conventions
(g C/m², W/m² carrier, 30 %/year arithmetic), derivative double-debit (→ §4.7), writer
location (→ §5), option-line position (→ §4.5), spin-up/competition metrics (→ V6).
Adjacent finding filed to BACKLOG, out of scope: `S_POS` double-integration under the RK2
opt-in solver.

## 9. Deferred / known limitations

- Resuspension × staging: guarded off (§6.1); the erosion-return term (`BED_AKI` → `AKI_C`
  scaled by the bed erosion rate) is future work if a resuspension setup ever needs staging.
- The annual-relay character of the bank at default `KR_GERM_BED` (§4.2): a deliberate,
  documented trade-off, revisitable via the option scalar.
- Recruiting as a distinct buoyant stage (full CLC) — rejected as YAGNI while the positioning
  ratchet covers vegetative buoyancy.
- `FIX_CYN`/NOST role consolidation — a future scientific decision informed by V6[c].
