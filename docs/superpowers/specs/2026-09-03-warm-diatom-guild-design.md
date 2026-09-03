# Warm diatom guild (`DIA2_C`) — design **v2**

**Status:** design, **not approved for build**. §0 (revised) is a light-climate work item in
its own right and **no longer gates the guild**; the guild stands or falls on August–September
(§2.2), which is where its evidence actually lives.
**Date:** 2026-09-03, revised 2026-09-04 after adversarial review (38 findings, 10 verified,
**9 confirmed / 1 refuted**). Every §-reference below is `docs/CL29_phenology_diagnosis.md`.
**Cost model:** the Droop-N VARN arc (spec `2026-08-30-cyn-droop-n-rescoped-design.md`) — same
`nstate 32 → 33`-class shape; its *tooling pattern* is reusable, its *routing* is not.

---

## 0. Premise test — do this first, and possibly stop here

The review refuted the reasoning that made this build look necessary, so the build is gated on
re-establishing it.

**What was wrong.** §46.3 claimed "November wants C:Chl ≈34, February wants ≈53" as a third,
model-mechanics axis proving two guilds. On the adopted ice baseline §45.4 implies February
wants **≈27.5**, and §43.2's local measurement is 34.2 with a winter-subset median of 32.4 and
a **winter IQR of 19.9–44.2**. ⚠ 27.5 sits *outside* the mean's CI [29.5, 39.1] but inside that
IQR — and the IQR is the right comparison, since one seasonal constant must span the spread of
real values, not the uncertainty in their mean. **On that basis both are ordinary winter
values: the two months agree on pigment and diverge only on growth.**

**And the production error that was supposed to explain it has now been retired (§47).** The
two candidates were `FDAY` and the background extinction. Both were measured on 2026-09-04,
**before** spending a run:

- **`FDAY` done correctly is an offset, not a February lever.** `I_A` is a daily integral, so
  the physically correct (WASP) form `FDAY·f(I_A/FDAY)` gives **−22.1 % February / −19.0 % May
  — a 1.04× differential.** The 1.71× belongs to the *forcing*, not to the model's response to
  it; in the light-limited regime the FDAY cancels, which is correct — spreading a fixed daily
  dose over more hours cannot change a near-linear daily integral. §44.3's FDAY row (×0.452)
  used the **incorrect** form (`FDAY·f(I_A)`, which silently discards (1−FDAY) of each day's
  light — 71 % of it in December). §47.2 has the full 12-month table.
- **The extinction is near-uniform and has no winter measurement.** Model kd is 2.60–2.93
  (Feb 2.617 / May 2.648 = 0.988), and `light_mixing_Nida_2015.csv` covers **May–November only**
  (§44.3, corrected). The model is within 5–8 % of measurement in five of seven measured months
  and fails in exactly two — **September −36 % and October −21 %** — so fixing it darkens
  **autumn** most and *costs* October rather than buying it.

**So February's C:Chl conflict has no identified production error left to blame**, and the
§0 gate as originally drafted cannot fire the way it was designed to.

⭐ Still cheap and still worth running, for its own sake rather than as a guild gate: the
day-length series already exists and is correct (`INPUTS_CL29/FORC_TS_9.txt`, 4,017 daily
records, 0.2898 on 1 January = 6.96 h at 55 °N — **no data work**), and the repo contains its
own reference implementation on the `smith == 0` branch of all six library routines.

**The revised test — three independent arms, not one bundle** (bundling is how §44.3's total
became unsupportable):
1. **Form B `FDAY`** behind `LIGHT_DAYLENGTH_OPTION` (0 = current/byte-identical, 1 = Form A
   for the record, 2 = Form B). A correctness fix with a ~−20 % near-uniform cost.
2. **Background extinction** to the measured May–Nov level, as its own arm. ⚠ Registered
   prediction: costs May and October.
3. **C:Chl 34** re-run on whatever baseline (1) and (2) leave.

⚠ **The guild is no longer gated on this.** Arms 1–3 answer "is C:Chl 34 adoptable?"; the guild
answers "can anything grow Aug–Oct diatoms?" — and §41.2 plus the CTMI table in §2.2 speak to
that directly. Coupling the two is what produced the overstated axis 3 in the first place.
**And §47.4 names the likelier exit for the C:Chl question specifically: photoacclimative
C:Chl** (§22, BACKLOG P2), because the constant is simultaneously a pigment conversion and a
growth parameter (it sets `I_s`) and no single value can serve both across a 20× seasonal light
range. That is better-targeted at this residual than a warm guild is.

**Gate.** If February `DIA_C` stays ≤1.3× observed with C:Chl 34 applied, **the guild is not
needed for this residual** — adopt the measured C:Chl, record the negative, and stop. Only if
February still demands ≈53 does the two-guild premise survive on mechanics, and even then
axes 1–2 below are the load-bearing ones.

---

## 1. Premise: two observational axes (not three)

The model represents diatoms with **one** CTMI envelope — `DIA_OPT_TEMP_LR` −2,
`DIA_OPT_TEMP_UR` 10, `KAPPA_DIA_OVER_OPT_TEMP` 21 — which is the *cold* guild exactly. The
observations contain two, three months apart (§41.2):

| guild | genera | wtd. mean month | peak |
|---|---|---|---|
| cold / spring | *Asterionella* 4.4, *Stephanodiscus* 5.1, *Aulacoseira* 6.3 | 4.4–6.3 | May (102.7) |
| **warm / late** | ***Skeletonema subsalsum*** 8.0, ***Actinocyclus normanii*** 8.2 | 8.0–8.2 | **Aug–Sep (41.6/42.9), carries Oct (25.5)** |

*A. normanii* alone is **44.5 %** of autumn planktonic diatom biomass, and the autumn diatom
signal is ~90 % genuinely planktonic (benthic share 4.5 % Oct / 9.7 % Nov, §41.1) — biomass the
model must grow, not a mapping artifact.

⚠ **Axes 1 and 2 both derive from the same §41.2 table**, so this is two lines of evidence from
one dataset, not three independent ones. The former "axis 3" is demoted to: *one C:Chl cannot
serve both months **under the current light climate*** — a statement about §0's unfixed error.

**Rejected alternatives:** widening the single envelope (§11 measured it destroying June);
re-tuning its C:Chl (§46, cancels against ice — but see §0); repurposing the extinct `OPA` slot
(cheapest, loses nothing live, but forces a mid-study re-derivation of two observation mappings
and needs the same silica work, since `OPA` has no `KHS_DSi`/`Si_TO_C`).

## 2. What is built

A new pelagic state variable **`DIA2_C` — warm diatom guild — at index 34**, fixed
stoichiometry as for every existing group.

⚠ **Index 33 is NOT free.** `aquabc_II_pelagic_svindex.f90` already defines
`CYN_N_INDEX = 33` from the merged Droop-N work, and the VARN guard keys on `nstate /= 33`
(`mod_PELAGIC_ECOLOGY.f90:1267`), so a DIA2 build at 33 would collide **silently in both
directions**. Placing DIA2 at **34** leaves that guard firing correctly and loudly on any stale
`CYN_VARIABLE_N 1`; index 33 becomes an unused hole in the DIA2 build (zero IC, zero growth
constant). Add `integer, parameter :: DIA2_C_INDEX = 34` beside `CYN_N_INDEX`, commented as a
mutually-exclusive build variant. **Never reference `CYN_N_INDEX` in DIA2 routing.** Use a new
`build-estas-dia2` target and binary name — do not reuse `build-estas-varn`.

### 2.1 Constants: 28 entries at 325–352; `NUM_MODEL_CONSTANTS` 324 → 352

The growth bundle mirrors `DIA` (5–27) plus C:Chl and BETA; **plus the three zooplankton
constants every phytoplankton group in this model has**, which v1 omitted:

| # | name | value | source / mirrors |
|---|---|---|---|
| 325 | `KG_DIA2_OPT_TEMP` | ⟨fitted — the one calibration target⟩ | start from the cold guild's literature-scaled rate |
| 326 | `DIA2_OPT_TEMP_LR` | **4.0** | *A. normanii* absent below ~4 °C in the record |
| 327 | `DIA2_OPT_TEMP_UR` | **17.0** | see §2.2 — matches the observed Aug–Sep peak *and* keeps CTMI valid |
| 328 | `EFF_DIA2_GROWTH` | 0.95 | as DIA |
| 329 | `KAPPA_DIA2_UNDER_OPT_TEMP` | 0.0 | inert **only on the CTMI branch**; note it |
| 330 | `KAPPA_DIA2_OVER_OPT_TEMP` | **26.0** | forcing max 28.6 °C, Jul–Aug p95 25.2 / p99 26.9 — binding in the top ~1 % of days, by design |
| 331–334 | `KR_DIA2_20`, `THETA_KR_DIA2`, `KD_DIA2_20`, `THETA_KD_DIA2` | as DIA | no evidence to differ |
| 335–338 | `KHS_DIN_DIA2`, `KHS_DIP_DIA2`, `KHS_DSi_DIA2`, `KHS_O2_DIA2` | as DIA | ⚠ §5 |
| 339 | `FRAC_DIA2_EXCR` | 0.30 | as DIA |
| 340 | `I_S_DIA2` | 100.0 | ⚠ only used when the adaptive `I_s` < 10 langley/d; reachable at low C:Chl — state it |
| 341–343 | hypoxia trio | as DIA | |
| 344–347 | `DIA2_N_TO_C`, `_P_TO_C`, `_Si_TO_C`, `_O2_TO_C` | as DIA | |
| 348 | `DIA2_C_TO_CHLA` | ⟨fitted, bounded ≤40 by §42.2⟩ | ⚠ **§43.2's 34 is the COLD guild's** (n=34 winter samples); the warm guild's is unmeasured |
| 349 | `BETA_DIA2` | 0.0 | as DIA |
| **350** | **`GRAT_ZOO_DIA2`** | 1.00 | mirrors `GRAT_ZOO_DIA` (103) |
| **351** | **`PREF_ZOO_DIA2`** | see §5 | mirrors `PREF_ZOO_DIA` (109) = 0.26 |
| **352** | **`KHS_DIA2_C_ZOO`** | 0.10 | mirrors `KHS_DIA_C_ZOO` (115) |

Each constant needs **all five sites** in `aquabc_II_pelagic_model_constants.f90` (declaration,
`para_get_value`, `para_insert_value`, bounds check, storage default) **and** the positional
ESTAS reader `INIT_PELAGIC_MODEL_CONSTANTS` — the §34 trap that silently dropped an entire
block once. All eight constants files and every declared count updated with defaults; every
read guarded (§45: `para_get_value` hard-stops on a missing name).

### 2.2 Cardinals: the guild's value is summer persistence, not October rate

**v1's error:** it justified the October gate on the guild's October growth. **No warm-guild
cardinal set beats the incumbent in October** — cold guild CTMI 0.935 at 12.8 °C versus 0.819
(4,17,26), 0.773 (4,18,28), 0.575 (v1's 4,20,28). The guild's value is **August–September**,
where it runs 0.97–1.00 while the cold guild collapses to 0.40 (water 18.5 °C against T_max 21):
it holds a summer population the cold guild cannot, and *that* carries October — precisely
§41.2's "the summer collapse is the absence of the guild that owns the summer".

**(4, 17, 26) is the choice**: best across Aug/Sep/Oct of the valid sets, and CTMI-valid with
margin. ⚠ **The validity gate is `2·T_opt > T_min + T_max`**
(`aquabc_II_pelagic_auxillary.f90:84-85`); 2·17 = 34 > 30 passes. Sets like (4,16,28) fail it
and fall back **silently** to the plateau formulation — the documented CTMI-validity trap.

### 2.3 Routing sites

Enumerate by grep, assert hit counts, never line-anchor (§38). Beyond the obvious growth /
nutrient-debit / loss / settling terms, v1 **omitted** these and they are blocking:

- **Bed handoff composition** — `aquabc_II_pelagic_auxillary.f90`, in **both**
  `FLX_ALUKAS_II_TO_SED_MOD_1` and `..._VEC`: add `DIA2_C` to `FLUXES(4)` PON (× `DIA2_N_TO_C`,
  in *both* branches of `if (CYN_VARIABLE_N > 0)`), `FLUXES(7)` POP, `FLUXES(10)` POC,
  `FLUXES(12)` particulate Si. Without this, settled DIA2 mass **vanishes**.
- **Allelopathy** — live in operational CL29 (`CONSIDER_ALLELOPATHY 1`). v1 never mentioned it,
  so DIA2 would grow *un-inhibited* while its competitors are suppressed — a decisive unfair
  advantage. **Option A (chosen):** DIA2 shares DIA's allelopathic role on both sides, keeping
  the state count at 37 and `NUM_ALLOLOPATHY_STATE_VARS` at 4. Declare
  `GROWTH_INHIB_FACTOR_DIA2` in `aquabc_II_pelagic_internal.f90` and mirror DIA's alloc and
  application.
- **Excretion** to DOC and dissolved Si, and the **DIC / alkalinity** chain
  (`TOTAL_DIC_KINETIC_SINKS`, `model.f90:3291`) — omitted entirely from v1.
- **`TCHLA`** for self-shading (one array, not per-group) and the `CHLA` sum.
- Transport switches for the new index (§38's blocker: `INIT_TRANSPORT_FIELDS` uses literal
  ranges), the count assert, and both metabolite literals.

## 3. Tooling — and the validator lands *first*

- **Validator before the build, not after** (v1 dropped the one tooling item the prior arc had).
  Five edits to `tools/validate_cl29_vs_epa.py`: `:47` `PHYTO_C` gains `DIA2_C` (feeds TN/TP);
  `:48-49` `C_TO_CHLA` gains `DIA2_C`; `:54-56` `WCONST_CHLA_KEYS` gains
  `DIA2_C_TO_CHLA → DIA2_C`; and observed `DIA_C` must be compared against **`DIA_C + DIA2_C`**.
  ⚠ **Without this every post-build score silently omits the new guild and is incomparable to
  every number in §38–§46.**
- **Setup generator** — the `make_varn_inputs.py` pattern is a *pattern, not a drop-in*: it
  hardwires `float(fields[CYN_C_INDEX]) * Q_SEED` (`:424`, `Q_SEED` `:86`); DIA2 needs
  `fields[DIA_C_INDEX]` (= 5) × `F_DIA2`, and the mirrored SCALE / UNIT-CONVERSION inserts
  (`:393-403`) must copy field 5 too. **Boundary and IC series must be specified, not zero** — a
  zero column pre-determines extinction and would make the whole ladder unfalsifiable.
- **Run checker** — the `check_varn_run.py` pattern (smoke / conserve / budget).
- ⚠ **`nconst` is defined in three places** (`aquabc_II_pelagic_interface.f90`, `mod_GLOBAL.f90`,
  and the declared count) — §45's trap: it built clean and aborted at runtime.

## 4. Verification ladder

1. **Standard build byte-identical** at full record **and** on the 0D golden (§45: the 0D leg is
   the one that gets skipped).
2. **Conservation** of C/N/P **and Si** on a degenerate scenario — ⚠ Si is the mass path a
   diatom adds that the reused gate does not cover. A "turn group X off" scenario must zero X's
   *growth constant*, not just its IC (§38: `MIN_CONCENTRATION` reseeding regrew diatoms
   1e-10 → 1.89 mg C/L in 30 days).
3. **Solver cross-check** Euler vs RK2, no NaN.
4. **Science ladder, pre-registered** — and note the gates below are stated in terms the
   *corrected* validator computes:
   - **[a] October** `DIA_C + DIA2_C` ≥ 0.40 mg C/L (obs 0.670; current 0.023) = success;
     < 0.15 = NULL. Justified by §2.2's summer-persistence mechanism, **not** October rate.
   - **[b] February** `DIA_C` ≤ 1.3× observed — §45's ice fix must survive.
   - **[c] June–July** `DIA_C + DIA2_C` ≤ 1.5× observed. ⚠ v1 had no June gate — and June is
     exactly what killed the cheaper alternative it cites (§11).
   - **[d] Phase first:** seasonal r ≥ +0.74, autumn:spring in [1.8, 2.4].
     ⚠⚠ **§46's rule: a simultaneous CHLA + PO4 RMSE improvement is NOT evidence until the phase
     metrics are checked** — that pattern has misled three times (§22, §43.3, §46).
   - **[e] Composition:** report the decomposition; the guild must not simply take the fixers'
     nitrogen (§38: CYN gained 0.109 while fixers lost 0.155 and total phyto carbon *fell*).

## 5. Risks

- ⚠⚠ **The premise may not survive §0.** If FDAY + the extinction floor let C:Chl 34 stand, this
  build is unnecessary. That test is cheap and comes first.
- ⚠⚠ **The guild's `C:Chl` is unmeasured** — §43.2's 34.2 is the *cold* guild's (diatoms dominate
  only winter samples, n = 34; summer/autumn n = 3). It is the load-bearing constant for the
  guild's defining trait and the one declared fitted parameter.
- ⚠ **Competitive exclusion is this model's default outcome** — OPA has been extinct in every
  configuration ever run (§24–27); akinete staging failed self-sustainment until its niche was
  opened (§29/§30). Existing is not succeeding; gates [a] and [e] decide.
- ⚠ **Silica**: a second Si consumer perturbs the budget §26 solved via the boundary. Si RMSE
  (0.86972) is a first-class metric here.
- ⚠ **Nitrogen**: §39 showed the autumn niche is not N-limited *for the current guilds*; a new
  summer consumer competes with the fixers for the same pool in the guild's defining season.
- ⚠ **`PREF_ZOO_DIA2`**: grazing preference is what §36's W6 adoption used to rescue CYN. Setting
  DIA2's preference low would be the same lever, and would need the same scrutiny.

## 6. Success, and the honest alternative

**Success** = October diatom carbon ≥0.40 mg C/L with February and June intact, phase metrics
held or improved, and the guild self-sustaining without taking the fixers' nitrogen.

**If it fails**, the result still stands as evidence: the autumn deficit would not be a
missing-organism problem either, leaving §40.1's light wall as a genuine limit of pelagic-growth
formulations in kd ≈ 3 water — the same shape of answer as §29's akinete negative and §38's
Droop negative, and publishable as such.
