# Warm diatom guild (`DIA2_C`) — design **v2**

**Status:** design, **not approved for build**. §0 (revised) is a light-climate work item in
its own right and **no longer gates the guild**; the guild stands or falls on August–September
(§2.2), which is where its evidence actually lives.
**Date:** 2026-09-03, revised 2026-09-04 after adversarial review (38 findings, 10 verified,
**9 confirmed / 1 refuted**). Every §-reference below is `docs/CL29_phenology_diagnosis.md`.
**Cost model:** the Droop-N VARN arc (spec `2026-08-30-cyn-droop-n-rescoped-design.md`) — same
`nstate 32 → 33`-class shape; its *tooling pattern* is reusable, its *routing* is not.

---

## 0. The light-climate arms — do these first, but they no longer gate the guild

The review refuted the reasoning that made this build look necessary. Re-establishing it was
attempted and **failed**: §47 retired the production error the gate depended on. What remains
here is a set of light-climate correctness items worth landing on their own merits — and a
sequencing argument (§0.1) for doing them before any guild build.

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

- ⚠⚠ **RETRACTED by §48.1 (2026-09-04): `FDAY` IS a February lever.** The claim below is true of
  the *rate* and false of the *biomass* — run at full record, Form B's Feb/May biomass
  differential is **4.66×**, and it drives February `DIA_C` from 0.99× to **0.22×**, undoing
  §45's adopted ice fix. Form B is correct physics but is **not adopted** (§48.4). The original
  text is kept below for the record:
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

⚠ **Updated after §48:** February *does* have a production error — but it points the **wrong
way**. February needs *more* biomass to match observations, and Form B removes biomass. Applying
the measured C:Chl 34 on a Form B baseline would compound the under-prediction. Arm 3 is
therefore **not** worth running on a Form B baseline; the exit for C:Chl remains photoacclimative
structure (§22, BACKLOG P2).

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

### 0.1 Why these still come first — sequencing, not gating

⭐ **Arm 1 changes production by ≈−20 % in every month and arm 2 darkens September–October by
21–36 %.** A guild built and tuned against today's light climate would be tuned against a
climate that is about to move, and its one fitted constant (`KG_DIA2_OPT_TEMP`) would silently
absorb the difference — the compensation pattern this study has now documented five times
(§22's pigment channel most recently). **Land the light climate, re-baseline, then build.**

**Outcome for C:Chl specifically.** If February `DIA_C` stays ≤1.3× observed with C:Chl 34
applied on the corrected baseline, adopt the measured C:Chl and record it. If it does not,
§47.4's reading is confirmed rather than refuted — the constant is doing two jobs at once and
the exit is photoacclimative C:Chl, still not a second guild.

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
one dataset, not three independent ones. The former "axis 3" is **withdrawn entirely** (§46.3,
§47.3): the two months agree on pigment within the measured winter IQR and differ on growth,
and §47 then showed the growth error it was blamed on does not exist at the size required.

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

⚠⚠ **QUALIFIED BY §50 (2026-09-04) — read before building on this section.** The autumn
re-measurement on the adopted baseline says `DIA` −1.435 mg C/L **is** the largest single *net*
autumn deficit, so the guild's premise stands — but it is ~45 % of a −3.217 total, and the
Aug–Sep window this section argues for is **precisely where the model already produces the right
total cyanobacterial carbon (Aug 0.99×, Sep 0.96×) in the wrong guild**. A warm diatom guild
added there enters a niche the model over-fills, against a fixer running
`KG_NOST_VEG_HET_OPT_TEMP` 7.6 versus `KG_CYN_OPT_TEMP` 2.0 — the same competition that holds
CYN at 0.10–0.27×. **Settle the cyanobacterial partitioning before sizing the guild**, or its one
fitted constant will absorb that error too.

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

---

## 7. Addendum 2026-11-07: what the corrected boundary and the silica work change

Status unchanged: **design, not approved for build.** This section re-checks §1–§6 against the
2026-11-06/07 findings (diagnosis §57; ESTAS-AQUABC #127–#130; ESTAS-AQUABC-DATA #1–#3).

**7.1 The baseline the guild was sized on was corrupted.**
- §2.2's qualifier and the §50 decomposition were measured on the configuration adopted in §26.
  Its `FORC_TS_1` carried the `build_boundary_phyto.py` factor-row bug (#127): the Baltic
  boundary applied OPA_C ×0.0014, DISS_Si ×0.0005, ZOO_C ×0.0021 and INORG_C ×0.00008.
- The adopted configuration is now the corrected boundary with measured Baltic Si, Table-2
  seasonal river DSi and DIA_Si_TO_C 0.30.
- Re-measured on it (mg C/L, KM lagoon boxes, model FIX = FIX_CYN + NOST):

| Aug / Sep / Oct / Nov | obs | adopted 2026-11-07 |
|---|---|---|
| DIA_C | 0.56 / 0.56 / 0.67 / 0.31 | 0.003 / 0.006 / 0.024 / 0.062 |
| CYN_C | 2.30 / 1.88 / 1.06 / 0.34 | 0.26 / 0.34 / 0.31 / 0.14 |
| FIX | 1.61 / 1.98 / 0.81 / 0.46 | 3.63 / 3.67 / 1.79 / 0.07 |
| OPA_C | 0.88 / 0.52 / 0.38 / 0.24 | 0.008 / 0.012 / 0.015 / 0.010 |
| total, model/obs | | 0.73 / 0.82 / 0.73 / 0.21 |

- **§2.2's qualifier survives the correction.** August-September total cyanobacteria are right
  (CYN+FIX 3.89 / 4.01 against observed 3.91 / 3.86) but in the wrong guild: fixers ~2× over,
  CYN 11-18 %. "Settle the cyanobacterial partitioning before sizing the guild" still holds.
- §56's allelopathic reading of the partition was also made on the corrupted boundary. Its
  re-check belongs in that arc, not here.

**7.2 Silica is now evidence for the guild, not a risk (revises §5).**
- §5 called a second Si consumer a perturbation of "the budget §26 solved via the boundary".
  §26's Si gain was the bug zeroing Baltic silica (diagnosis §57: −1.0 % of the objective from
  the phytoplankton climatology, ~−14 % from Baltic Si).
- On the corrected, silica-adopted configuration the interior Si cycle matches May (0.47 vs
  0.33) and July (0.62 vs 0.51), but sits **+0.6 to +0.9 mg Si/L above observed in Aug-Oct**
  (1.05 / 1.28 / 1.60 vs 0.47 / 0.56 / 0.67), the guild's window.
- The observed summer diatom stock (~0.5 mg C/L) at Si:C 0.30 could remove that several times
  over.
- **New gate [f]:** Aug-Oct interior Si bias at least halved, May-July not worse than ±0.15
  mg Si/L.

**7.3 Constants and tooling.**
- `DIA2_Si_TO_C`: start at the adopted cold-guild 0.30 (Brzezinski 1985 mean, 0.13 molar). No
  guild-specific value was found.
- **Setup generator (§3), the #127 lesson.** ESTAS SCALE / UNIT-CONVERSION rows have no time
  column: in a data row, field *j* (0-based, time at 0) is variable *j*; in a factor row it is
  field *j−1*. "Mirror field 5 into the inserted column" therefore means different indices in
  the two kinds of row. Test it as #127 did: a byte-level check that the factor rows hold the
  intended values and that nothing else moved.
- **Validator (§3).** #129 makes the validator read C:Chl from the run's own WCONST (else Chl-a
  is not scored). DIA2 still needs the `C_TO_CHLA` / `WCONST_CHLA_KEYS` / `PHYTO_C` entries of §3.
- **Runs.** #130 limits ESTAS alert printing (an unfiltered CL29 run writes ~9 GB). Until it
  merges, use curonian `shyfem_box/run_estas_filtered.sh`.
- (4, 17, 26) still passes the CTMI validity gate (2·17 > 30).

---

## 8. Addendum 2026-11-07 (2): falsified as specified — the build/no-build review

**Proposed status (owner to accept or edit):** not approved for build; **falsified as specified
on 2026-11-07** (gate [a] October 0.053, below the 0.15 NULL). A re-specified guild (warm
envelope + ~4× nutrient affinity) passes the §8.5 screen marginally: October 0.38 alone, 0.47 with
the cold guild. Revisit only with a literature-backed nutrient affinity for the warm genera, or
after the summer-N deficit (§8.4) is fixed, re-screened by §8.5 either way. Diagnosis references:
§57, §58.

**8.1 The test: the spec's own cardinals on a diatom.** One run of the adopted E4 configuration
(allelopathy off, corrected boundary, silica adopted), with the diatom CTMI envelope swapped from
the cold guild (−2, 10, 21) to the warm guild's (4, 17, 26). The inputs differ from E4 in those
three constants only, plus process-rate output for boxes 7, 14, 17 and 23. The process-rate copy
of E4 reproduces E4's 29 state outputs byte for byte. This is a **probe, not a candidate**.

- **Proxy validity.** In E4 the cold guild is already absent in Jul-Sep (0.003-0.008 mg C/L), so
  the swapped diatom met exactly the competitors a DIA2 would meet there: FIX 3.3-3.5 and CYN
  0.45-0.70 mg C/L. Its winter and Jan-Mar collapse is expected and irrelevant to the question.
- Diatom carbon, KM lagoon boxes (mg C/L):

| | Jul | Aug | Sep | Oct |
|---|---|---|---|---|
| observed | 0.41 | 0.56 | 0.56 | 0.67 |
| E4, cold envelope | 0.008 | 0.003 | 0.008 | 0.088 |
| warm envelope (4, 17, 26) | 0.012 | 0.005 | 0.009 | 0.053 |
| warm envelope + ~4× nutrient affinity (KHS_DIN_DIA 0.0025, KHS_DIP_DIA 0.0021) | 0.130 | 0.057 | 0.072 | **0.381** |

**8.2 Gates.**
- **[a] October ≥ 0.40 fails as specified, below the NULL line.** The gate is DIA_C + DIA2_C:
  E4's cold guild (0.088) plus the warm envelope (0.053) is 0.14, under the 0.15 NULL.
- **[f] Silica fails as specified.** The Aug-Oct interior Si bias is +0.80 in E4 and +0.85 with
  the warm envelope; it was to be at least halved.
- **[d] already fails on E4, before any guild.** Autumn/spring is 2.53 against the [1.8, 2.4]
  band (seasonal r +0.78 passes). A guild adds autumn Chl-a, so it pushes the wrong way unless the
  fixers fall. §2.2's "settle the partition first" is therefore a gate consequence, not only a
  preference.
- [b] and [c] pass on E4 (February 1.14× observed; June-July under 1.5×).

**8.3 Why: once temperature is relieved, nutrients and light bind.** Growth is
`KG · LIM_TEMP · LIM_LIGHT · min(LIM_DOXY, SU(N, P, Si))`. Jul-Sep factors in the four boxes:

| | TEMP | N | P | LIGHT | growth : losses |
|---|---|---|---|---|---|
| E4, cold envelope | 0.32 | 0.38 | 0.16 | 0.21 | 0.73 |
| warm envelope | **0.82** | 0.38 | 0.16 | 0.16 | 0.82 |
| warm + ~4× affinity | 0.82 | 0.64 | 0.35 | **0.16** | **1.27** |

- Two single-axis reliefs have now failed. §58.6 relieved the growth rate and the P affinity on the
  cold envelope (KG_DIA 9.0, KHS_DIP_DIA at its floor): DIA Aug-Oct 0.042. The swap relieved
  temperature: 0.005-0.012.
- §39 precedent: autumn DIN was already 4-5× under and a ×10 KDISS_PON probe gave ×1.02, because
  October light is 0.07. Summer light is 0.16-0.24, so nutrient relief has room to act in summer
  that it never had in autumn.

**8.4 The summer nutrients, model against measured** (same-day pairs, nine EPA lagoon boxes,
Jul-Sep 2012-2022, n = 1034, mg/L):

| | observed median / mean | model median / mean |
|---|---|---|
| NH4 | 0.028 / 0.038 | 0.007 / 0.009 |
| NO3 | 0.011 / 0.015 | 0.000 / 0.001 |
| PO4 | 0.0025 / 0.0048 | 0.0007 / 0.0017 |

- **The DIN deficit is established.** NH4 runs at about a quarter of observed (floor 0.004, so
  the values are mostly real). NO3 is stripped to zero against a real median of 0.011, though 22 %
  of the observations sit at a 0.0015 substitute. In April-May the model's NO3 median is instead too
  high (0.49 vs 0.20), though the means agree (0.55 vs 0.58).
- **PO4 is untestable.** 63 % of summer observations are the 0.0025 below-detection substitute,
  and the model's 0.0007 is consistent with "below detection". LIM_P is 0.08 at the model's PO4;
  if every censored value were at the 0.005 limit it would still be at most 0.37.
- **Hypothesis, not finding:** the over-predicted fixer bloom (2× observed in Aug-Sep) holds the
  summer nutrients. The positive TP bias (phosphorus held as particles) fits it. So does the
  K_NITR / K_MIN_DOC_NO3N double ceiling in both calibrations: the model fits the spring NO3
  excess by removing nitrogen, then starves the summer. This is what a fixer-dominance /
  summer-N diagnosis would test.

**8.5 The swap screen.** Eight minutes on the adopted configuration, the spec's exact cardinals,
gate [a] read straight off the output. Recipe: copy the adopted `INPUTS_CL29`; set
`DIA_OPT_TEMP_LR` / `DIA_OPT_TEMP_UR` / `KAPPA_DIA_OVER_OPT_TEMP` to 4 / 17 / 26 in
`WCONST_04.txt`; switch on process-rate output for boxes 7/14/17/23 in
`PELAGIC_OUTPUT_INFORMATION_FILE.txt`; run E4's master with the folders renamed. Any proposed
enabling fix should pass it before anyone commits the 32 → 33 state-variable build.

**8.6 The second probe: the guild was mis-specified, not impossible.** The same swap with ~4×
higher nutrient affinity (KHS_DIN_DIA 0.010 → 0.0025, KHS_DIP_DIA 0.0084 → 0.0021) establishes a
summer-autumn population. Jul-Sep turns growth-dominated (growth : losses 1.27), and with N and P
relieved (0.64 / 0.35), light becomes the main summer limit (0.16).
- **Gate [a] passes as defined:** October 0.381 alone, and 0.47 with E4's cold guild (0.088)
  added. A real two-guild model would have the guilds competing in October, so treat 0.47 as
  an upper estimate.
- **Jul-Sep:** up 10× (0.06-0.13), but still 4-8× under observed, capped by light.
- **Composition:** fixers fall 12-22 % toward observed (Aug 3.38 → 2.97, Oct 1.57 → 1.22), and
  OPA is unchanged. CYN falls in Oct-Nov (Oct 0.83 → 0.64, against 1.06 observed).
- **Gate [f]:** the Aug-Oct Si bias falls 41 % (+0.80 → +0.47), not quite halved. May-Jul goes
  to −0.17, just outside ±0.15.
- **Gate [c]:** June is borderline (0.30 against 0.196 observed, 1.5×). The probe's January-March
  collapse is the swap, not the guild.
- So **neither trait alone opens the summer niche**: the warm envelope alone gives October 0.053;
  high P affinity on the cold envelope (§58.6, KHS_DIP_DIA at its floor) gives Aug-Oct 0.042.
  Only both together do. §2.1's "`KHS_*_DIA2` as DIA" row is the flaw.

**8.7 What would have to be true to build.**
1. **Literature for the warm genera's nutrient half-saturations.** *Skeletonema subsalsum* and
   *Actinocyclus normanii* would need N and P affinities about 4× higher than the cold guild's.
   The ×0.25 used here is a probe value, not a measurement. Without a source, the guild's one
   decisive trait would be a fitted constant: the compensation pattern this study has documented
   repeatedly.
2. **Or fix the summer N deficit first** (§8.4). If the model's summer DIN came up toward the
   observed values, a guild with the cold guild's affinities might pass. That is a
   fixer-dominance / summer-N diagnosis, and it also bears on the partition (§2.2, gate [d]).
3. **Either route is re-screened by §8.5 before any build**, and the light cap (summer LIGHT
   0.16) bounds what any summer diatom can reach.

**8.8 Literature check (2026-11-07): the ×4 affinity of route 1 is not supported.** Searched with
scite (abstracts, open full text, citation statements); no editorial notices on any source used.
- **No constants exist for the two species.** No N or P half-saturation constant, for uptake or
  growth, was found for *Skeletonema subsalsum* or *Actinocyclus normanii*. The kinetic-term search
  on the two names (plus *S. potamos*) returns 89 records, none with a constant.
- **Size points the other way for the dominant species.** Half-saturation constants for nutrient
  uptake rise with cell volume, and between-taxon differences are "nearly always due to
  between-taxon variation in volume" (Edwards et al. 2012, doi:10.4319/lo.2012.57.2.0554; also
  Hillebrand et al. 2021, doi:10.1111/1365-2435.13986). *A. normanii* is a large centric: a 40 µm
  valve diameter separates its small and large cells (Vidal et al. 2018,
  doi:10.18257/raccefyn.713), against ~10 µm for the cold guild's *Stephanodiscus*. By
  allometry its constants would be higher, not 4× lower, and it carries 44.5 % of the observed
  autumn diatom biomass (§1). Only the small *S. subsalsum* could plausibly sit below the cold
  guild.
- **The ecology does not separate the guilds.** *A. normanii* is "indicator of eutrophied,
  polluted waters" (Vidaković et al. 2016, doi:10.17110/studbot.2016.47.2.201) and of "high
  trophic conditions" (Pérez et al. 2017, doi:10.1080/00288330.2017.1417318). *S. subsalsum* is
  listed as "tolerant to nutrient pollution" (Bilous et al. 2021, doi:10.3897/phytokeys.178.64426),
  but in the same list as the cold guild's *Stephanodiscus hantzschii*.
- **The only congener number is 1.8×, not 4×.** *S. costatum*'s ammonium uptake Ks is 0.4 µM
  (Conway et al. 1976, doi:10.1007/BF00390940), against the model's 0.71 µM. *S. subsalsum* is a
  separate lineage from the *S. costatum* complex, whose literature "calls for reinterpretation"
  (Sarno et al. 2005, doi:10.1111/j.1529-8817.2005.04067.x).
- **Uptake and growth constants are not interchangeable for P.** For nitrate, Ks for uptake and
  growth are "similar if not identical" (Eppley & Thomas 1969,
  doi:10.1111/j.1529-8817.1969.tb02628.x). For phosphate, *Asterionella* and *Cyclotella*
  differed in growth K but not in uptake K (Tilman & Kilham 1976,
  doi:10.1111/j.1529-8817.1976.tb02860.x). The model's Monod `KHS_DIP_DIA` therefore cannot be
  checked against uptake compilations.
- **The temperature niche is supported.** *S. subsalsum* is a "heat-preferring species" with an
  optimum of 16-22 °C in the Volga reservoirs (Kulizin et al. 2023, doi:10.3390/life14010061,
  citing earlier Volga work). *A. normanii* "frequently occurs in summer and autumn" and formed
  blooms in the Szczecin Lagoon; it made up over 95 % of August heatwave biovolume there, though
  the authors flag that as possibly an artefact of few samples (Schernewski et al. 2025,
  doi:10.3390/environments12100370). The cardinals (4, 17, 26) are consistent with both.
- **So route 1 of §8.7 is closed.** A ×4 affinity would be a fitted constant, against the
  allometry for the guild's main species. Route 2 (the summer-N deficit, §8.4) remains the
  defensible way to open the summer niche.
