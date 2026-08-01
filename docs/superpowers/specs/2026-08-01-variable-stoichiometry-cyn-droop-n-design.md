# Variable stoichiometry — Droop N-quota pilot for CYN — design

**Motivation:** `BACKLOG.md` §3 (variable Si:C/N:C stoichiometry); the multivariate wall diagnosed
across the Nostocales / FIX_CYN / issue-#76 negative results.
**Date:** 2026-08-01
**Status:** ⛔ **Do-not-implement-as-written — a Workflow review (2026-08-01) found the premise refuted
and the architecture broken. See §12.** The motivating hypothesis (CYN luxury N-uptake draws summer DIN
down) is contradicted by the model's own conservation + regeneration-floored-DIN verdict; separately the
gating architecture is unimplementable on four counts and mass-conservation is false as written. Design
§1–§11 retained for the record.
**Scope tier chosen:** Scoped Droop-N **pilot** on non-fixing cyanobacteria (CYN) only; config-gated
via a new setup variant; goal is to test whether variable N:C breaks the wall before any full feature.

---

## 1. Problem & motivation

AQUABC is a **fixed-stoichiometry (Monod)** model at its core. All five phytoplankton groups are
**carbon-only** state variables (part of `nstate = 32`). Growth is Monod on *ambient* nutrient
(`LIM_KG_CYN_N = DIN/(KHS_DIN_CYN + DIN)`, `aquabc_II_pelagic_lib_CYANOBACTERIA.f90:165`), and **every**
N/P/Si flux is `carbon-flux × a fixed ratio` (`*_N_TO_C`, `*_P_TO_C`, `*_Si_TO_C`): uptake
`= growth × N_TO_C` (`aquabc_II_pelagic_model.f90:1988`), remineralization `= respiration × N_TO_C`
(`:1976`), detritus `= death × N_TO_C` (`:2812`). **Nutrient uptake is rigidly welded to carbon growth.**

That weld is the *multivariate wall* that closed the last three science lines (Nostocales, FIX_CYN,
issue #76): fixing one variable regresses others (the #76 EPA A/B: PO4 −19% but NH4 +30%, DO/Si/TN/Chl-a
worse). A group physically cannot strip summer DIN below its growth-limited level, because taking up N
*requires* growing C proportionally — over-producing biomass (breaking Chl-a) and, for the diazotroph,
over-producing N. **Decoupling uptake from growth** — "luxury" uptake into an internal store — is the
mechanism the wall needs, and it requires an internal nutrient **quota**: the Droop cell-quota model.

### 1.1 Scientific basis

The cell-quota (Droop) model makes phytoplankton growth a function of the **internal** nutrient quota
Q rather than the ambient concentration, with Michaelis–Menten uptake filling the quota and Liebig
co-limitation across elements; it reproduces luxury uptake and variable C:N:P that fixed-Redfield Monod
models cannot (Kwiatkowski et al., 2018). Empirically, cellular N and P quotas track the Droop function
and phytoplankton show excess ("luxury") uptake of both N and P (Bi et al., 2012). Adopting a Droop
N-quota for one group is a conventional, well-grounded step from Monod toward variable stoichiometry.

## 2. Goal & scope

**Goal:** give the non-fixing cyanobacteria (CYN) a Droop nitrogen quota so CYN can luxury-uptake N —
drawing summer DIN down toward observations **without** the proportional carbon/Chl-a over-growth the
fixed ratio forces — and measure whether that breaks the multivariate wall against EPA.

**In scope**
- One new state variable `CYN_N` (gN m⁻³), the nitrogen in the CYN pool.
- Droop growth N-limitation (normalized quota) + Michaelis–Menten uptake down-regulated by quota
  fullness.
- N-flux routing at the *current* quota for CYN (uptake, remineralization, detritus, grazing).
- A runtime flag `CYN_VARIABLE_N` (default 0) that makes `nstate` 32/33 and branches the CYN kinetics.
- A new `INPUTS_CL29_varstoich/` setup (flag on, `nstate = 33`) and its driver.
- EPA multivariate A/B validation + a byte-identity gate + unit/mass-balance tests.

**Out of scope (explicit)**
- P:C and Si:C quotas; any group other than CYN; the full multi-group / multi-element feature.
- DON as a quota N source (the pilot takes up DIN only — see §9); calibrating the four new constants to
  Curonian data (pilot uses literature-typical values; the test is *direction*, not a tuned fit).

## 3. The Droop-N formulation

Let `Q = CYN_N / CYN_C` (gN gC⁻¹) be the cell quota, bounded `[Q_min, Q_max]`. Four new constants
(CYN block): `CYN_N_QMIN`, `CYN_N_QMAX` (gN gC⁻¹), `CYN_N_VMAX` (gN gC⁻¹ d⁻¹, max specific uptake),
`CYN_N_KHS_UPT` (gN m⁻³, uptake half-saturation on ambient DIN).

**Uptake** (new flux, gN m⁻³ d⁻¹) — MM on ambient DIN, down-regulated as the quota fills:
```
f_down          = max(0, (Q_max − Q) / (Q_max − Q_min))          ! → 0 at Q_max, 1 at Q_min
R_CYN_N_UPTAKE  = CYN_N_VMAX · DIN/(CYN_N_KHS_UPT + DIN) · f_down · CYN_C
```
where `DIN = NH4_N + NO3_N` (+ `DON·frac_avail_DON` is deferred, §9). The uptake is split between NH4
and NO3 by the existing preference `PREF_NH4N_CYN` (so the NH4/NO3 sink columns keep their current
partitioning). Uptake is additionally capped by available DIN per step (the existing loss-safeguard
pattern) to prevent negative DIN.

**Growth N-limitation** — replaces the ambient Monod `LIM_KG_CYN_N` (`…CYANOBACTERIA.f90:165`) with the
normalized (Caperon–Meyer) quota:
```
LIM_KG_CYN_N = min(1, max(0, (Q − Q_min) / (Q_max − Q_min)))
```
Everything else in growth is unchanged: `LIM_KG_CYN_NUTR` still Liebig/SU-co-limits `LIM_KG_CYN_N` with
`LIM_KG_CYN_P`, `LIM_KG_CYN = LIM_LIGHT · min(LIM_DOXY, LIM_NUTR)`, and
`R_CYN_GROWTH = KG_CYN · LIM_KG_CYN · CYN_C`. Growth adds carbon **without** consuming DIN, which
*dilutes* Q (more C, same N) — the quota is drawn down by growth and refilled by uptake.

**N mass balance** (the routing change; `Q` = current quota):
| pool | fixed-ratio path (today) | Droop path (piloted) |
|---|---|---|
| `CYN_C` deriv | growth − resp − excr − death − graze | *unchanged* |
| **`CYN_N` deriv** | — (no state var) | `+ R_CYN_N_UPTAKE − (R_CYN_TOT_RESP + R_CYN_DEATH + R_CYN_EXCR + R_ZOO_FEEDING_CYN)·Q` |
| NH4 / NO3 (uptake) | `− R_CYN_GROWTH · CYN_N_TO_C` (`:1988/:2067`) | `− R_CYN_N_UPTAKE` (split by `PREF_NH4N_CYN`) |
| NH4 (remin) | `+ R_CYN_TOT_RESP · CYN_N_TO_C` (`:1976`) | `+ R_CYN_TOT_RESP · Q` |
| DET_PART_ORG_N | `+ R_CYN_DEATH · CYN_N_TO_C` (`:2812`) | `+ R_CYN_DEATH · Q` |
| DISS_ORG_N | `+ R_CYN_EXCR · CYN_N_TO_C` (`:2944`) | `+ R_CYN_EXCR · Q` |
| ZOO_N | `+ R_ZOO_FEEDING_CYN · CYN_N_TO_C` (`:2700`) | `+ R_ZOO_FEEDING_CYN · Q` |

**Conservation:** every gN leaving a pool enters another (`ΔDIN + ΔCYN_N + ΔDON + ΔDET_N + ΔZOO_N = 0`
for the CYN terms), by construction — `CYN_N` explicitly tracks the nitrogen that the fixed ratio only
imputed. This is asserted in a mass-balance test (§8).

## 4. Architecture

- **State index:** add `integer, parameter :: CYN_N_INDEX = 33` in
  `aquabc_II_pelagic_svindex.f90` (highest existing index = 32; `CYN_C_INDEX = 15`).
- **`nstate` is flag-driven:** `nstate = 32` when `CYN_VARIABLE_N = 0`, `33` when `= 1`
  (`aquabc_II_pelagic_interface.f90:74`, currently the literal `32`). The ESTAS driver already checks
  `NUM_PELAGIC_STATE_VARS == nstate` (`mod_AQUATIC_MODEL.f90:179`), so the setup's input must declare
  the matching count.
- **The flag** `CYN_VARIABLE_N` is read from `PELAGIC_MODEL_OPTIONS.txt` with a **graceful optional
  read** (`end=`/`err=` → default 0), mirroring the `TEMPERATURE_MODEL` option, so every existing
  options file (which lacks the line) defaults to off.
- **Kinetics branch:** `CYANOBACTERIA` takes the flag (+ `CYN_N`, `Q`); flag off → the current Monod +
  fixed-ratio path (unchanged); flag on → the Droop path (§3). Same branch in `aquabc_II_pelagic_model.f90`
  for the N-flux routing table above and the new `CYN_N` derivative.
- **Constants:** the four `CYN_N_*` constants added to `t_cyn_params` + `populate_cyn_params`
  (`aquabc_II_pelagic_types.f90`) and to the name-keyed constants machinery
  (`aquabc_II_pelagic_model_constants.f90`: declare / INIT `para_get_value` / INSERT
  `para_insert_value` / DEFAULT / VALIDATE `≥ 0` guard — the same six sites as any constant). They live
  only in the varstoich setup's `WCONST_04.txt`; absent elsewhere → name-keyed default, irrelevant
  because the flag is off there.
- **New setup:** `INPUTS_CL29_varstoich/` (copy of `INPUTS_CL29/`) with: `PELAGIC_MODEL_OPTIONS.txt`
  `CYN_VARIABLE_N = 1`; `PELAGIC_INPUTS.txt` + IC + boundary declaring 33 state vars (the 33rd, `CYN_N`,
  seeded at `Q ≈ CYN_N_TO_C · CYN_C`, i.e. the Redfield-equivalent quota); the four `CYN_N_*` constants
  in `WCONST_04.txt`. Driver `INPUT_CL29_varstoich.txt`.

## 5. Files to change (exact)

1. `aquabc_II_pelagic_svindex.f90` — add `CYN_N_INDEX = 33`.
2. `aquabc_II_pelagic_interface.f90` — read `CYN_VARIABLE_N`; set `nstate` 32/33 from it; thread the
   flag into the kinetics call.
3. `aquabc_II_pelagic_types.f90` — add the four `CYN_N_*` fields to `t_cyn_params` + `populate_cyn_params`.
4. `aquabc_II_pelagic_model_constants.f90` — the six-site name-keyed plumbing for the four constants
   (declare / INIT get / INSERT insert / DEFAULT 0 / VALIDATE `< 0 → 0`), mirroring `BETA_FIX_CYN`.
5. `aquabc_II_pelagic_lib_CYANOBACTERIA.f90` — the Droop branch: quota-based `LIM_KG_CYN_N`, the
   `R_CYN_N_UPTAKE` flux (+ DIN cap), guarded by the flag; Monod path unchanged when off.
6. `aquabc_II_pelagic_model.f90` — the `CYN_N` derivative assembly and the flag-branched N-flux routing
   (the §3 table). ESTAS transport advects `CYN_N` automatically once `nstate = 33`.
7. New `INPUTS_CL29_varstoich/` + `INPUT_CL29_varstoich.txt` (per §4). ESTAS handles `nstate = 33`
   via the count it reads; confirm the converter (`eutropy_to_estas.py`) can emit the 33rd var or author
   it manually for the pilot.

## 6. Gating & byte-identity

A new state variable **cannot** be byte-identical via a zero constant (adding it would change `nstate`,
the I/O structure, and transport for *every* setup). So the gate is **setup-level**: with
`CYN_VARIABLE_N = 0` (absent → default), `nstate = 32`, `CYN_N` never exists, and CYN runs the current
Monod + fixed-ratio path → existing setups (Standard, CL29, sediment-test) are **byte-identical**. Only
the opt-in `INPUTS_CL29_varstoich/` (flag = 1, `nstate = 33`) exercises the quota.

## 7. Validation — does it break the wall? (the pilot's whole point)

EPA multivariate A/B, mirroring the #76 gate: run `INPUTS_CL29_varstoich` (flag on) vs baseline
`INPUTS_CL29` (flag off), score both with `tools/validate_cl29_vs_epa.py` (9 boxes, 2012–2016).

- **Primary success:** summer DIN (NH4 + NO3) / NH4 RMSE **improves** (CYN strips DIN via luxury
  uptake) **without** regressing Chl-a, Si, TN — or with materially less regression than the fixed-ratio
  FIX_CYN A/B. That is the wall breaking.
- **Diagnostics:** total-N closes (§8); `Q` stays within `[Q_min, Q_max]`; summer `Q` **rises** (luxury
  uptake) then dilutes through the bloom — confirming the mechanism actually engaged.
- **Honest failure mode:** if `Q` simply pins near `CYN_N_TO_C` and reproduces the fixed-ratio behaviour
  (no luxury benefit), the pilot has shown variable N:C does not help here — a valid, cheap negative
  result that (unlike #76's window fix) settles the §3 backlog item with evidence.

## 8. Testing

- **Byte-identity gate (primary net):** old-code vs new-code, flag off, across Standard
  (`MODEL_SEDIMENTS=0`), CL29 (`=1`, `ESTAS_HOLD_VOLUME=1`), sediment test (`=2`) — outputs identical.
- **Unit test** (`make test-fortran`, extend `tests/fortran/test_cyn*`): with the flag on, assert (a)
  `R_CYN_N_UPTAKE → 0` as `Q → Q_max` and is MM in DIN; (b) `LIM_KG_CYN_N = (Q−Q_min)/(Q_max−Q_min)`
  clamped; (c) flag off reproduces the Monod `LIM_KG_CYN_N` and the fixed-ratio fluxes exactly.
- **Mass-conservation test:** over one step with the flag on, the CYN N terms satisfy
  `ΔDIN + ΔCYN_N + ΔDON + ΔDET_N + ΔZOO_N = 0` to roundoff.

## 9. Risks

- **New-state-var surface:** the flag + conditional `nstate` + IC/boundary/transport must line up
  exactly, or an existing setup breaks — the byte-identity gate (§8) is the backstop, and the ESTAS
  count-check fails loudly on a mismatch.
- **Quota bounds under Euler:** at large `dt`, uptake could overshoot `Q_max` or losses drive `Q < Q_min`;
  clamp `Q` and cap `R_CYN_N_UPTAKE` by available DIN (existing 50%-loss-safeguard pattern).
- **Uncalibrated constants:** the four `CYN_N_*` values use literature/ERSEM-typical numbers (e.g.
  `Q_min ≈ 0.5 · CYN_N_TO_C`, `Q_max ≈ 2–3 · CYN_N_TO_C`, `V_max ≈ a few · KG_CYN · CYN_N_TO_C`); the
  test is direction, not a tuned fit, and §7 reports that caveat.
- **DON simplification:** today CYN draws some N from DON during growth; the pilot's uptake is DIN-only,
  so the DON→CYN pathway is dropped while the flag is on (noted; a full feature would add DON to uptake).
- **Grazing/other consumers read `Q`:** the zooplankton N gain now uses the CYN quota rather than the
  fixed ratio — correct, but it couples ZOO_N to a varying source; covered by the mass-balance test.

## 10. Decisions log

- **Scope:** scoped Droop-N *pilot*, CYN only, gated — not the full multi-group/multi-element feature.
- **Element:** N only (the wall's motivated axis; summer DIN drawdown).
- **Group:** non-fixing cyanobacteria (CYN) — the summer-bloom DIN player, no fixation complications.
- **Growth form:** normalized (Caperon–Meyer) quota `(Q−Q_min)/(Q_max−Q_min)` over classic Droop
  `1−Q_min/Q` (interpretable `[Q_min,Q_max]` band, pairs with the uptake cap).
- **Uptake:** Michaelis–Menten on ambient DIN × quota-fullness down-regulation.
- **Gating:** runtime flag `CYN_VARIABLE_N` + conditional `nstate` + new setup variant (a new state
  variable can't be byte-identical via a zero constant).

## 11. References

- Kwiatkowski, L., Aumont, O., & Bopp, L. (2018). The impact of variable phytoplankton stoichiometry on
  projections of primary production, food quality, and carbon uptake in the global ocean. *Global
  Biogeochemical Cycles*, 32(4), 516–528. <https://doi.org/10.1002/2017GB005799>
- Bi, R., Arndt, C., & Sommer, U. (2012). Stoichiometric responses of phytoplankton species to the
  interactive effect of nutrient supply ratios and growth rates. *Journal of Phycology*, 48(3),
  539–549. <https://doi.org/10.1111/j.1529-8817.2012.01163.x>

---

## 12. Workflow review outcome (2026-08-01) — do not implement as written

A 4-dimension adversarial Workflow review (Fortran/architecture, formulation/mass-conservation,
adversarial premise, scope/testability; each finding independently verified against source) returned
**21 confirmed findings, 8 BLOCKING.** Two independent classes of failure; the first is decisive.

### 12.1 The premise is refuted (finding 11, BLOCKING — the headline)

The pilot's motivating claim — "CYN luxury N-uptake draws summer DIN **down** toward EPA" — is
contradicted by the model's own steady-state N cycling and the project's own verdict:
- CYN is **88–97% N-replete** across the whole summer DIN range (`KHS_DIN_CYN = 0.009`,
  `LIM_KG_CYN_N = DIN/(DIN+0.009) ≈ 0.9`), so growth is *not* N-limited and decoupling uptake from
  growth adds **no sustained N demand**. Biomass is capped by light/temp/grazing/self-shading, not N.
- A Droop quota therefore adds only a **bounded one-time storage transient** (`≈ (Q_max−Q_Redfield)·CYN_C`),
  not a season-long sink. Every quota-N loss path (resp→NH4, death→detritus→remin, excr→DON→remin,
  graze→ZOO→remin) **returns N to the water column, dominantly as NH4** — which *is* the
  regeneration-driven NH4 that the 2026-07-25 DEFINITIVE VERDICT already identified as the irreducible
  ~0.06 DIN floor that **already matches EPA**. **Luxury uptake feeds the floor; it cannot lower it**
  (storage ≠ removal — provable from the §8 conservation identity itself).
- Worse (finding 12, MAJOR): NH4-preferring uptake strips the pool that **already matches EPA** (NH4)
  and spares the over-predicted, advection-resupplied **NO3** — the likely signature is a NO3→NH4
  speciation shift that **raises** NH4 above its floor, recreating the multivariate wall (finding 13).

So the pilot as framed cannot test "lower the summer DIN budget" — at most a phenology/timing
redistribution — and is a near-certain negative already settled by conservation + the project record.

### 12.2 The architecture is broken (even setting the premise aside)

- **BLOCKING (finding 1):** the runtime flag cannot toggle `nstate` — the ESTAS path is governed by
  `GLOBAL::nstate`, a **compile-time `parameter = 32`** (`mod_GLOBAL.f90:16`), sizing automatic arrays
  `dimension(nkn,nstate)`. The `nstate` §5 edits (`interface.f90:74`) is a different symbol used only by
  the 0D driver. Fix would be a *separate compile-time build* (`nstate=33`), not a runtime flag.
- **BLOCKING (finding 3):** `CYN_N_INDEX = 33` **collides with the allelopathy block** — metabolites
  occupy `nstate+1..+4` = 33–36 (`NUM_ALLOLOPATHY_STATE_VARS = 4`), and CL29 declares **36** state vars.
  The spec never mentions allelopathy; the "33 state vars" arithmetic is wrong.
- **BLOCKING (finding 4):** the pilot **patches the wrong subroutine** — CL29 runs
  `CYANO_BOUYANT_STATE_SIMULATION=1` → `CYANOBACTERIA_BOUYANT` (`model.f90:1096`); the plain
  `CYANOBACTERIA` (`:165`, the spec's target) is **dead code**. As written the Droop path never runs.
- **BLOCKING (finding 2):** the §9 count-check backstop is **blind under allelopathy** (tautological for
  both gate setups) → silent state-vector misalignment instead of a loud stop.
- **BLOCKING (finding 7):** mass conservation is **false as written** — the DON-uptake-during-growth sink
  (`DISS_ORG_N` process #5, `model.f90:2947/2979`) is left in place, destroying N at
  `R_CYN_GROWTH·CYN_N_TO_C·(1−PREF_DIN_DON_CYN)` per step; the "by construction" proof does not hold.
- **BLOCKING (findings 16, 18):** no positive integration test for the flag-ON path (byte-identity gate
  is vacuous for the new code), and the four `CYN_N_*` constants are ranges/undefined (`CYN_N_KHS_UPT`
  has no value) — an implementer cannot author `WCONST_04.txt` deterministically.
- Plus IMPORTANT: converter hardcodes 32 in ~6 sites (finding 5); photosynthetic-O2 stoichiometry
  breaks and DO is scored (finding 9); the §7 A/B is confounded by the DON drop (finding 14) and is the
  same unattributable multi-effect bundle as #76 (finding 19); the mass-balance test isn't runnable in
  the named harness (finding 21).

### 12.3 Decision

**Do not implement this pilot as framed.** The `§3` variable-stoichiometry backlog item stands, but the
specific "CYN Droop-N draws summer DIN down" hypothesis is refuted (§12.1) — it joins the same
regeneration-floored-DIN / multivariate-wall class as FIX_CYN and #76. Any future variable-stoichiometry
work must (a) target a variable/mechanism where the model is genuinely *uptake*-limited (not
regeneration-floored), and/or reframe as a phenology/timing question with a sustained-drawdown
discriminator; and (b) if pursued, adopt the corrected architecture (separate compile-time `nstate=33`
build, allelopathy-aware indexing, patch `CYANOBACTERIA_BOUYANT`, zero the DON-uptake sink, an explicit
non-blind count assert, committed constants, a flag-ON integration test).
