# Warm diatom guild (`DIA2_C`) — design

**Status:** design, not approved for build. **Date:** 2026-09-03.
**Motivating evidence:** doc `CL29_phenology_diagnosis.md` §41 (taxonomy + phenology), §42
(light trait), §43 (measured C:Chl), §45 (ice adopted), §46 (the two-guild case proven from
model mechanics). **Cost model:** the Droop-N VARN arc (§38, spec
`2026-08-30-cyn-droop-n-rescoped-design.md`) — same `nstate 32 → 33` shape, tooling reusable,
routing not.

---

## 1. Premise: why a second guild, and why not anything cheaper

The model represents diatoms with **one** CTMI envelope — `DIA_OPT_TEMP_LR` −2,
`DIA_OPT_TEMP_UR` 10, `KAPPA_DIA_OVER_OPT_TEMP` 21 — which is the *cold* guild exactly. The
observations contain two, three months apart:

| guild | genera | weighted mean month | peak |
|---|---|---|---|
| cold / spring | *Asterionella* 4.4, *Stephanodiscus* 5.1, *Aulacoseira* 6.3 | 4.4–6.3 | May (102.7) |
| **warm / late** | ***Skeletonema subsalsum*** 8.0, ***Actinocyclus normanii*** 8.2 | 8.0–8.2 | **Aug–Sep (41.6/42.9), carries Oct (25.5)** |

*Actinocyclus normanii* alone is **44.5 %** of autumn planktonic diatom biomass (three forms;
§41.2), and the autumn diatom observations are ~90 % genuinely planktonic (benthic share
4.5 % Oct / 9.7 % Nov, §41.1) — so this is biomass the model must grow, not a mapping
artifact.

**Three independent axes say one envelope cannot do it** (§46.3):
1. **Taxonomy** — different organisms, brackish warm-water centrics vs cold-water centrics.
2. **Phenology** — three months apart in weighted mean month; the model over-predicts
   Feb–Jun (the cold guild) and is absent Jul–Oct (the warm one).
3. **Model mechanics** — one `C:Chl` sets *both* guilds' light efficiency through
   `I_s = GITMAX·CCHL·e/(0.083·PHIMX·XKC)`. November needs ≈34, February ≈53; §46.1 measured
   the collision directly (C:Chl 34 fixed November to 1.02× and returned February to 2.85×,
   cancelling the adopted ice fix exactly).

**Cheaper routes, and why they were rejected:**
- **Widen the single envelope** — measured in §11: it fixed Si and winter but destroyed June
  (the wide envelope carried 0.71 mg C/L through Jun–Jul against an observed annual minimum
  of 0.17 and ate the fixers' phosphorus). Trades one error for another.
- **Re-tune the single envelope's C:Chl** — §46: cancels against ice; unusable by construction.
- **Repurpose the extinct `OPA` slot** — cheapest (no new state), and OPA contributes
  0.001–0.003 mg C/L in every month so nothing live would be lost. **Rejected because it
  re-defines what published scores mean**: observed `OPA_C` maps to the chlorococcalean greens
  (*Pediastrum* 25.8 %, *Desmodesmus* 11.5 %) and observed `DIA_C` to all diatoms; repurposing
  forces a re-derivation of both mappings mid-study. Also needs the same silica work as (A),
  since `OPA` has no `KHS_DSi`/`Si_TO_C` machinery.

## 2. What is built

A new pelagic state variable **`DIA2_C` — warm diatom guild — at index 33**, with fixed
stoichiometry exactly like the existing groups (one carbon state; N/P/Si/O2 follow via ratio
constants). This is the same structural shape as §38's `CYN_N`, so its tooling applies.

### 2.1 Constants: a full 25-entry block at 325–349

Mirroring the `DIA` block (5–27) plus C:Chl and BETA. Provisional values and their sourcing:

| # | name | value | source |
|---|---|---|---|
| 325 | `KG_DIA2_OPT_TEMP` | ⟨fit⟩ | calibrate; start at the cold guild's literature-scaled rate |
| 326 | `DIA2_OPT_TEMP_LR` | **4.0** | brackish warm centrics; *A. normanii* absent below ~4 °C in the record |
| 327 | `DIA2_OPT_TEMP_UR` | **20.0** | Great Lakes *A. normanii* peak abundance ≈20 °C (Kipp et al. via Vidaković 2016) |
| 328 | `EFF_DIA2_GROWTH` | 0.95 | as DIA |
| 329 | `KAPPA_DIA2_UNDER_OPT_TEMP` | 0.0 | unused, kept for layout parity |
| 330 | `KAPPA_DIA2_OVER_OPT_TEMP` | **28.0** | warm tolerance; lagoon max is 21.9 °C so this is non-binding by design |
| 331–334 | `KR_DIA2_20`, `THETA_KR_DIA2`, `KD_DIA2_20`, `THETA_KD_DIA2` | as DIA | no evidence to differ |
| 335–338 | `KHS_DIN_DIA2`, `KHS_DIP_DIA2`, `KHS_DSi_DIA2`, `KHS_O2_DIA2` | as DIA | ⚠ see §5 risk |
| 339 | `FRAC_DIA2_EXCR` | 0.30 | as DIA |
| 340 | `I_S_DIA2` | 100.0 | as DIA (adaptive `I_s` dominates; see §2.3) |
| 341–343 | hypoxia trio | as DIA | |
| 344–347 | `DIA2_N_TO_C`, `_P_TO_C`, `_Si_TO_C`, `_O2_TO_C` | as DIA | |
| 348 | `DIA2_C_TO_CHLA` | **⟨34, see §5⟩** | §43.2 measured 34.2 [29.5, 39.1] — but that estimate is the *cold* guild's (§43.2 caveat) |
| 349 | `BETA_DIA2` | 0.0 | as DIA |

`NUM_MODEL_CONSTANTS` 324 → 349; every setup updated with defaults, every read guarded
(§45's `para_get_value` hard-stop lesson).

### 2.2 Routing sites

Every site where `DIA_C` appears must gain its `DIA2_C` analogue. Enumerate by grep, assert
the hit count, never line-anchor (§38's lesson):

- growth and its limitation terms (CTMI, LIM_N, LIM_P, **LIM_Si**, LIM_LIGHT, hypoxia);
- nutrient debits: NH4 + NO3 split by preference, PO4, **DSi**;
- O2 production via `DIA2_O2_TO_C`;
- losses: respiration → NH4/PO4/DSi, death → DET_PART_ORG_C/N/P + particulate Si,
  excretion → DON/DOP, grazing → ZOO with its own `PREF_ZOO_DIA2`;
- settling → bed, with its own velocity/dissolved/deposited rows;
- chlorophyll: `CHLA` sum gains `DIA2_C / DIA2_C_TO_CHLA`, and **`TCHLA` for self-shading**;
- `PHYT_TOT_C` and every total that currently sums the five groups;
- transport switches for index 33 (§38's blocker: `INIT_TRANSPORT_FIELDS` uses literal
  ranges and would leave 33 uninitialised);
- both metabolite literals and the count assert (§38).

### 2.3 Light: the trait that motivates the guild

`I_s = GITMAX·CCHL·e/(0.083·PHIMX·XKC)` is adaptive, so the guild's light behaviour follows
from its **own** `KG` and `C:Chl` — which is the entire point: §46 showed a shared `C:Chl`
cannot serve both seasons. *A. normanii* is documented as adapted to "strongly changing light
situations… high vertical turbulent mixing and low Zeu/Zmix", with its distribution limit set
by light (Rehbehn et al. 1993, doi:10.1007/bf02334784) — the CL29 light climate exactly.

⚠ **The guild must clear §40.1's rebuild bar to be worth building:** October net growth
≥ **+0.056/d** (rebuild from the summer floor to the observed 0.670 mg C/L inside the ~60-day
window). §42.2's algebra says growth rate alone cannot deliver this — the ceiling is
KG-independent — so the guild's `C:Chl` is the load-bearing constant, and §5 records that its
value is *not* measured.

## 3. Architecture and build shape

Follow §38's VARN pattern exactly, because it is proven:
- **`nstate 32 → 33` via the trap-guarded transient patch** (`make build-estas-varn`), so the
  tracked source never carries 33 and the standard build stays byte-identical.
- **Setup generator** (`tools/make_varn_inputs.py` pattern) to emit the 37-variable setup:
  variable-table row, per-box ICs, per-state settling/dissolved/deposited rows, FORC_TS
  headers and columns, options position. Its lessons carry: insert at the reader's position,
  reproduce real header structures, mirror the settling plumbing of the analogous state.
- **Run checker** (`tools/check_varn_run.py` pattern): smoke (echo, column present, transport
  flags), conservation on a degenerate scenario, per-term budget.
- ⚠ **`nconst` is defined in three places** (`aquabc_II_pelagic_interface.f90`,
  `mod_GLOBAL.f90`, plus the declared count) — §45's trap, which built clean and aborted at
  runtime.

## 4. Verification ladder

1. **Standard build byte-identical** at full record and on the 0D golden (§38 V2; §45 showed
   the 0D leg is the one that gets skipped).
2. **Conservation** of C/N/P/Si on a degenerate single-guild scenario (§38 V4: a "turn group
   X off" scenario must zero X's *growth constant*, not just its IC —
   `MIN_CONCENTRATION` reseeding regrew diatoms 1e-10 → 1.89 mg C/L in 30 days).
3. **Solver cross-check** Euler vs RK2, no NaN.
4. **Science ladder, pre-registered** (§43.3's discipline — write the numbers down first):
   - **[a]** October `DIA_C + DIA2_C` ≥ 0.40 mg C/L (obs 0.670; current 0.023) = success;
     < 0.15 = NULL.
   - **[b]** February `DIA_C` stays ≤ 1.3× observed — i.e. **the cold guild is not
     re-inflated**; §45's ice fix must survive.
   - **[c]** Phase first: seasonal r ≥ +0.74 and autumn:spring within [1.8, 2.4].
     ⚠⚠ **§46's rule: a simultaneous CHLA + PO4 RMSE improvement is NOT evidence until the
     phase metrics are checked** — that pattern has now misled three times (§22, §43.3, §46).
   - **[d]** The guild must be *self-sustaining without displacing the fixers*: report the
     compositional decomposition (§38's lesson — CYN gained 0.109 while fixers lost 0.155 and
     total phytoplankton carbon fell).

## 5. Risks, stated before the build

- ⚠⚠ **The guild's `C:Chl` is unmeasured.** §43.2's 34.2 [29.5, 39.1] comes from
  diatom-dominated samples, and diatoms dominate **only in winter/spring** (n = 34); in
  summer/autumn they never reach 70 % of biomass (n = 3). So the load-bearing constant for the
  guild's defining trait has no local measurement. Mitigation: treat it as the one fitted
  constant, state it as such, and bound it by the §42.2 rebuild-bar arithmetic (≤ 40 clears).
- ⚠ **Competitive exclusion is the default outcome here.** OPA has been extinct in every
  configuration ever run (§24–27), and the akinete staging failed self-sustainment before the
  niche was opened (§29/§30). A new guild that merely *exists* is not a result; [a] and [d]
  are the gates.
- ⚠ **Silica.** A second Si consumer changes the Si budget that §26 already solved via the
  boundary. Watch Si RMSE (currently 0.86972) as a first-class metric, not an afterthought.
- ⚠ **Two open light-climate items remain unfixed** (§44.2): `FDAY` read-and-never-used, and
  the background extinction below the measured kd floor. Both are near-uniform across months,
  so they should *not* be bundled — but the guild's October arithmetic assumes the current
  light climate, and fixing either later will move the guild's calibration.
- **Scope honesty:** this is a 7-task-plan build. The prior arc's tooling reduces the cost but
  the routing is new work, and §38's per-task adversarial review found two silent-corruption
  blockers (uninitialised transport flags; a duplicated DON sink in the branch CL29 runs) that
  no test would have caught.

## 6. Success, and the honest alternative

**Success** = October diatom carbon recovered to ≥0.40 mg C/L with February intact, phase
metrics held or improved, and the guild self-sustaining without taking the fixers' nitrogen.

**If it fails** the result is still worth having: it would show the autumn diatom deficit is
not a missing-organism problem either, leaving §40.1's light wall as a genuine limit of the
pelagic-growth formulation in kd ≈ 3 water — which is a publishable structural statement, and
the same shape of answer as §29's akinete negative and §38's Droop negative.
