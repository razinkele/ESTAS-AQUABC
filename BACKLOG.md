# AQUABC / ESTAS — Consolidated Backlog

Single internal tracker. It consolidates the open items that were previously scattered across
`FORTRAN_IMPLEMENTATION_PLAN.md` (§8 Phase 5, §2.2 issue table), the EUTROPY↔AQUABC comparison
paper (`~/curonian/docs/EUTROPY_AQUABC_comparison*` §10.3 / §11.2), and the science/calibration
campaigns tracked in project notes. **Detailed task specs live in the referenced sources — this
file is the prioritized index, not a duplicate**, so it can't drift from the plan.

**Last updated:** 2026-08-30 · **Latest release:** v0.11.0 (`main` is ahead of it — the doc §20–27
honest-configuration + boundary arc is merged unreleased) · Fortran-plan Phases 1–4 and every item
in `FIXES_AND_IMPROVEMENTS.md` are complete; the only open GitHub issue is the v0.11.0
release-announcement pointer (#112). (#76 FIX_CYN phenology was investigated and closed as
*not planned* — see §4.)

---

## Recently completed (anchor points)

- **The phenology/structure arc (2026-08-10 → 08-14)** — seasonal inversion diagnosed and fixed
  (`FIX_CYN_OPT_TEMP_LR` 18→8, the Nodularia-value error; seasonal r −0.70 → **+0.59** operational),
  zooplankton food-limitation formulation replaced (opt-in `ZOO_FOOD_MODEL` + quadratic closure —
  the legacy ceiling and numerical stability were the same artifact; zoo bias ≈ 0), real ERA5 wind
  forcing adopted (the constant-4 placeholder had disabled AND impersonated the 2019 positioning
  gates), sub-daily positioning machinery merged (`CYANO_POS_MODEL`, byte-identical default), and
  the honest-optics programme closed by experiment: winter/N/O₂/zoo essentially solved under
  measured optics; the summer surface bloom is a **persistent state** the daily-fraction mechanism
  class cannot reach. Full ledger: `docs/CL29_phenology_diagnosis.md` §1–19.

- **The honest-configuration adoption + boundary arc (2026-08-14 → 08-17)** — the surface-bloom
  persistence STATE built as the positional ratchet (`CYANO_POS_MODEL=2`, module
  `AQUABC_POSITIONING_STATE`, dS/dt = K_UP·F_calm·(1−S) − K_DISP·F_storm·S, formation/dispersal
  hysteresis 3/8 m/s; doc §20, merged `a92b33f`) — with it the UNFITTED honest configuration
  overtook the operational one (seasonal r +0.70 vs +0.59). The honest-base DE added project
  bests (§21); self-shading of the positioned fraction was built and the FIFTH compensation
  channel found — pigment inflation: C:Chl must never be a calibration knob (§22). **✅ ADOPTED
  as operational CL29 (§23, user decision): the transparent-water era is over** (r +0.72, CHLA
  RMSE 25.19, peak Sep). Then the boundary arc: OPA's flat-placeholder open boundary diagnosed
  (§24); the symmetric 4-group boundary built from Baltic plume data (§26) REFUTED the OPA
  attribution (competitive exclusion, confirmed from three directions) and **SOLVED the silica
  residual (RMSE −48 % — a missing boundary diatom supply, not kinetics/burial)**; adopted §27
  (merged `faaccf0`): baseline Φ −14.6 % from the data alone, the largest single-change gain of
  the study. Post-boundary recalibration found nothing to adopt and proved the KG inflation was
  compensation for the missing supply — now cosmetic (a literature-parameters variant is
  recorded in §27). Current operational scores: **r +0.73, ratio 1.66, CHLA RMSE ~25.5,
  Si 0.82, PO4 0.0232.** Full ledger: doc §20–27.

- **The staged-fixer + *Planktothrix* arc (2026-08-23 → 08-30, doc §28–36)** — residual
  re-measurement killed the benthic-P premise (§28); akinete staging BUILT (opt-in
  `NOST_STAGE_MODEL`, workflow-reviewed spec+plan, subagent-driven, byte-identical off) and its
  honest FAIL exposed the **seventh invisible-error class** (the niche held closed by the tuned
  surrogate) — the role swap closed the life cycle and was **ADOPTED (§31)**; the July/autumn
  probes closed fixer-side tuning (§32–33); a bit-identical null exposed the **eighth class**
  (the BETA block dead in the ESTAS-side parallel reader — fixed `ebab415`, §34); six parameter
  families measured null on the autumn community until the **obs audit** (§35) split the residual
  (⅓ of OPA = tychoplanktonic *Mougeotia*; summer CYN = 65–70 % *Planktothrix*) and the
  low-loss trait correction was **ADOPTED (§36)**. Current operational: **CHLA RMSE 24.06 +
  PO4 0.01695 (project bests), Sep exact, peak-month margin 2.3 µg, staged bank self-sustaining
  in production.** Named frontier: Aug *Planktothrix* exclusion, autumn chlorococcaleans,
  July knot.

- **Resuspension global state → `resuspension_t`** — first slice of Phase 5.1 global-state
  reduction; `GLOBAL` allocatable count 55→44; byte-identical; merged **`2014265`**. Established a
  reusable byte-identical-refactor method (determinism pre-check → string/comment-aware rename →
  strip-and-compare pure-prefix proof → CI matrix incl. ifx).
- **v0.6.0 loadable-setup registry** — Standard (25-box) / CL29 (29-box) / CL29-clim as a one-click
  selection, with a Dashboard ↔ Run-Model selector sync and an OpenMP thread cap. Addresses much of
  the paper's §10.3 "lower the barrier to entry".
- **External calibration + validation harness** — EPA (~70k samples) + KM hydrochem ingesters,
  `tools/validate_cl29_vs_epa.py`, `tools/compare_validation_runs.py`, and PEST++ ensemble
  calibration. Realizes the paper's §10.3 alternative ("document the external PEST-style workflow");
  see the caveat under **Calibration** below — the in-Fortran objective is still a stub.
- Fortran-plan Phases 1–4 (safety, modernization, testing, OpenMP) and the whole
  `FIXES_AND_IMPROVEMENTS.md` list — done.
- **v0.8.0 benthic denitrification** (`CL29_BENTHIC_DENIT`, `78ce4ad`) — config-only NO3 sink; closes
  ~20% of the CL29 summer-NO3 over-prediction. See §4.
- **Configurable pelagic solver (RK2/Heun)** — shipped as an experimental opt-in via `ESTAS_PELAGIC_SOLVER`
  env (default Euler, byte-identical). Investigation concluded RK2 is correct but ~1st-order and not better
  than Euler for this model (MIN_CONCENTRATION clamping dominates). *Retires the old "expose RK2" backlog item.*
- **Phase 5.1 de-globalization — two more slices merged:** bottom-sediment `SED_*` → `sediment_state_t`/`bsed`
  (`1a413e9`) and settling/water-coupling → `wsc_state_t` (`54a2de2`), both byte-identical. `mod_GLOBAL`
  allocatables now **12** (the pelagic water-column core). *Retires the "sediment `SED_*` cluster" as the next slice.*
- **Sediment-redox Fe(II) salt-selection bugfix** (`maxloc … dim=2→dim=3`) — merged to `main` (PR #80).

---

## 0. Completed — resuspension × sediment-diagenesis coupling (shipped v0.9.0)

*Source: `docs/superpowers/specs/2026-07-30-resuspension-diagenesis-coupling-design.md`. Branch:
`feature/resuspension-diagenesis-coupling`.*

Lets bed resuspension (prescribed-velocity "Option 3") run **together with full diagenesis**
(`MODEL_SEDIMENTS=2`) with a mass-conserving bed↔water particulate transfer — the previously-guarded,
never-run combination. Built on the collaborator's `ali_version` Option-3 scaffolding.

| Item | Status |
|------|--------|
| Areal-vs-volumetric bed-side erosion conservation fix (missing `÷SED_DEPTHS`) | **Done** — flux ratio 1.0; `SED_PSi` delta −822. |
| Particulate **C/N/P** water handoff (`FLX_SED_MOD_1_TO_ALUKAS_II_VEC`, water 9/10/11 ← bed slots 10/4/7) | **Done** — mirrors the proven PART_Si channel. Verified: noero (diagenesis-without-resuspension) **byte-identical** (0 diffs), ero now delivers C/N/P detritus to the water column (+7.4/+2.4/+0.008) matching bed decrements (−9355/−5321/−84). Coupling is mass-conserving for all four particulates. |
| Shear-driven erosion rate (Phase 2, `E=E₀(τ_b/τ_c−1)`) | Deferred (spec Option B). |

**Shipped in v0.9.0 (2026-07-31)** — Phase 1 merged to `main` together with the Fe(II) salt-selection
bugfix and the negative-mass diagnostics refactor (the two side-branches landed with the release);
Standard runs byte-identical. Only the optional Phase 2 (shear-driven erosion realism,
`E=E₀(τ_b/τ_c−1)`) remains, deferred by design.

---

## 1. Engineering — Phase 5 "Advanced Refactoring"
*Source: `FORTRAN_IMPLEMENTATION_PLAN.md` §8 — DEFERRED / LOW priority.*

| ID | Item | Status | Notes |
|----|------|--------|-------|
| 5.1 | Reduce global state (`mod_GLOBAL` 50+ vars, ~416 `pelagic_internal` arrays → derived types) | **Substantially complete** | 4 byte-identical slices merged: resuspension (`2014265`), bottom-sediment (`1a413e9`), water-coupling (`54a2de2`), and the **pelagic-core `pcore` Tiers 1–2** (PRs #88 `1c2c9ef` / #89 `7c5d85e`) — `mod_GLOBAL` loose pelagic allocatables **12→4**. **Tier 3 (`pH`/`STATE_VARIABLES`/`MODEL_CONSTANTS`/`PROCESS_RATES`): NO-GO (2026-08-01)** — cosmetic count only (`pcore` stays in GLOBAL → zero coupling change) vs highest-risk/~1000 sites. See `docs/superpowers/specs/2026-08-01-pelagic-core-deglobalization-design.md`. |
| 5.2 | Separate CO2SYS into a standalone library (`CO2SYS_LIB/` + CMake) | Open | |
| 5.3 | Higher-order ODE solvers (beyond the existing RK2/Heun) | Open | |
| 5.4 | Runtime configuration (config-driven instead of recompile) | Open | RK2-via-config already shipped (see "Recently completed"). |
| — | Expose the existing RK2/Heun solver via config | **Done** | Shipped experimental via `ESTAS_PELAGIC_SOLVER` env (default Euler, byte-identical). |
| — | Inconsistent naming (throughout) | Open (Low) | §2.2 issue table. |

## 2. Calibration & reproducibility
*Source: paper §10.3 / §11.2.*

| Item | Priority | Status / notes |
|------|----------|----------------|
| Calibration objective in `mod_COST_FUNCTION` | **Done (2026-08-02, path b)** | The external PEST++ workflow is now formally documented as **the** calibration objective — `docs/CL29_Calibration_PEST_Workflow.md` (architecture / Φ = weighted RMSE via `validate_cl29_vs_epa.py` / parameters / the `pest/` + `pest_fixcyn/` instances + the posterior-nonstationarity lesson), wired into the README docs index, with a pointer comment in `mod_COST_FUNCTION.f90` recording that the stub is intentional. The in-Fortran objective (path a) remains deliberately unimplemented. |
| Auto-generate PEST template/instruction/control files | P1 | Partially covered by the calibration harness; not yet a first-class AQUABC feature. |
| Reproducibility: complete the constants file / fail-loud on missing constants / containerized build | **P1 — ALL 3 DONE** | **"Complete the constants file" — DONE (2026-08-02, PR #95):** the "306 vs 318" was real (the 0D-example files, not `WCONST_04.txt`); `const_CL.txt` 318→323 and `const_default.txt` 306→323, appended at code defaults, **byte-identical** (0D golden unchanged). **"Fail-loud" — DONE (2026-08-02, PR #99):** correctly retargeted at the ESTAS *positional, name-blind* `READ_MODEL_CONSTANTS` (`mod_UTILS_01`, the shared pelagic+sediment reader) after the first spec mis-targeted the 0D reader (see `aquabc-parallel-code-paths`). `size()`-based bounds-check + seen-mask + zero-init → `error stop` naming the offending index on a dropped/out-of-range/duplicate/malformed constant; `AQUABC_LENIENT_CONSTANTS=1` escape. Byte-identical (shipped files index-complete); a CI `build-and-run` step proves the fail-loud fires. Design + 3-reviewer review: `docs/superpowers/specs/2026-08-02-fail-loud-positional-constants-reader-design.md`. (Misnamed-with-right-index deferred — numerically harmless.) **Containerized build — DONE (2026-08-02, PR #97):** a reproducible `Dockerfile` (`ubuntu:24.04` + `gfortran-13` + `python3-numpy`) that builds with a **pinned `-march=x86-64-v2`** (the release default is host-specific `-march=native`) and runs the committed 0D example vs its golden **at build time**; plus a CI `docker-build` job that verifies the image every push. Build + 0D reproduce out-of-box; CL29 + EPA-validation via mounted (external) data. See `docs/superpowers/specs/2026-08-02-reproducible-build-container-design.md` + the README "Reproducible container" section. |
| Global sensitivity / identifiability (Morris/Sobol over ~300 constants) | **Done (2026-08-02)** | Method-of-Morris identifiability screen shipped as `tools/sensitivity_morris.py` — a self-contained, **no-PEST++-dependency** tool that reuses the CL29 forward model (symlink-farm worker + perturbed `WCONST_04.txt` + short-window run + `validate_cl29_vs_epa.py` as Φ), parallelized locally. Scoped to a **curated 15-parameter** set (the calibration-relevant N/P/Si regeneration + diatom/cyano growth-mortality-halfsat constants) rather than all ~300 (most are structurally irrelevant to the scored EPA variables). Ran r=6 (96 runs, 2-yr window, Φ bit-reproducible). **Result:** the data has strong leverage on **cyano mortality (KD_CYN_20, top), denitrification, nitrification, and diatom/cyano growth** — phyto *kinetics are not inert*; the **Si half-sat / biogenic-Si dissolution constants are non-identifiable** (μ\* ~40–85× below the top; partly a short-window artifact for slow Si/P → run full-record `pestpp-sen` to confirm). Reconciles with the structural over-prediction finding: **high μ\* = Φ-sensitive, not fixable** (Morris is blind to the multivariate trade-off wall). Full write-up + tiered ranking + `pestpp-sen` full-record equivalent: `docs/CL29_Sensitivity_Analysis.md`. |
| Plankton-biomass observations (group-level phyto C + zoo C) | **Ingester + validator hook DONE (2026-08-07)** | `tools/ingest_km_plankton.py` ingests the KM plankton archives (`~/curonian/DATA`): the 2015 campaign workbook (class-level **carbon** at 7 lagoon stations, incl. the per-station `Nida` sheet; empirical C:biovolume ratios DIA 0.063 / CYN 0.172 / FIX 0.160 / OPA 0.142 extracted from its Ratio rows) + state-monitoring `Fitoplanktonas_KM_2022/2023` (species-level wet biomass at LTK stations → groups × ratios) + `Zooplanktonas_KM_BJ_2023` (wet → C at 5 %). 631 tidy rows (DIA_C/CYN_C/FIX_CYN_C/OPA_C/PHYTO_TOT_C ×119 station-dates + ZOO_C ×36) + 14 `KMP_*.dates`; 2015+2022 in-window. `validate_cl29_vs_epa.py` now scores them (obs FIX_CYN_C vs model `FIX_CYN_C+NOST_VEG_HET_C`; totals vs live-phyto sum). **First result (full-record v0.11.0 run): the summer biomass deficit is the N-FIXER deficit** — obs 2.0 vs model 0.11 mg C/L summer (r 0.4–0.8: phasing right, magnitude ~18× off); diatoms essentially unbiased (+0.08); implied obs summer C:Chl ≈ 115 vs the model's 30–40 (flag: C:Chl and the wet→C ratios are the key obs-side uncertainties). Unit tests `tests/python/test_ingest_km_plankton.py`. **Group-C terms added to Φ + recalibrated (2026-08-08, `calibrate_cl29.py --group-carbon`, sets `phyto`/`phyto_all`):** the 5-knob DE transfers to the full record (+8.9 % ext-Φ; CYN_C bias −0.66→−0.05 incl. held-out 2022; PO4 bias −39 % — through the wall via real uptake) **but wins by OPA/fixer extinction; the 9-knob rerun with OPA/FIX-specific knobs proves the niche collapse is structural** (fixers unrescuable at halved mortality — resource-driven exclusion; OPA killed by choice). **Neither adopted** (OPA-survival guard). ⇒ Composition coexistence needs model structure (niche separation / fixer-specific phenology-seeding-energetics, see §3) — now *data-demanded*, not speculative. **NDJSON extension (2026-08-08 afternoon):** the ingester now also consumes the AAA open-data dump (`JTD/monitoringasjsonl`) — systematic species-level phyto+zoo 2016–2024 at LTK stations; null dates recovered via the `reg_nr` sampling-event join (21,469 recovered / 67 lost); "patikrinta" workbooks take precedence on overlaps; units verified consistent across sources. **In-window obs: 80→317 station-dates/group; ZOO_C 0→329 in-window** (first-ever grazing constraint: obs 0.049 vs model 0.009 mg C/L — **model zooplankton is 5.5× LOW**). 7-yr baseline group biases: DIA +0.07 (robust), CYN −0.58, FIX −0.67, OPA −0.45. **7-yr recalibration DONE (2026-08-09, train 2012–2018 / holdout 2019–2022, ZOO_C in Φ):** the 7-yr optimum is *qualitatively opposite* to the 2-yr one (cyano growth DOWN 2.4→1.53, DIN affinity WEAKER 0.009→0.024) and **grew the fixers 8× for the first time** (0.038→0.302) — at CYN's expense, so still zero-sum. **Holdout verdict: nutrients transfer, composition does not** — PO4 bias HALVED on unseen data (+0.024→+0.012, RMSE −35 %; full-record +0.022→+0.011, EPA-Φ +13 %) but every group bias worsens because lagoon composition is strongly nonstationary between eras (obs FIX 1.13→0.23, CYN 0.74→1.31). Chl-a −2.8→+6.1 and OPA still extinct ⇒ **not adopted**. Totals show a **biomass LEVEL ceiling** (cyano total stays 3.9×→3.2× low) distinct from the partition problem; ZOO_C is **inert to phyto params** (5.5× low) → new `phyto_zoo` param set. **C:Chl probe (NEW, 2026-08-09):** C:Chl is a *model* parameter (drives self-shading via `light_kd`), observed ~53 median / ~78 cyano-like (312 paired same-sample determinations) vs model 40 — raising it to 78 lifts CYN carbon **+71 %** (largest single-lever gain seen) but total phyto only +2 % (stolen from FIX/OPA) and Chl-a *worsens* ⇒ **4th independent lever family to fail on the total-biomass ceiling**. Validator gained `--wconst` (score perturbed-C:Chl runs consistently) + `--since/--until` (holdout scoring). **⭐ LIGHT-CLIMATE FINDING (2026-08-09, the arc's most consequential): CL29's `K_B_E`=0.70 (in `EXTRA_WCONST.txt` — invisible to every calibration tool, like `C_TO_CHLA`) gives kd ≈ 1.7 /m against MEASURED Curonian kd 3.18 mean / 2.92 median (n=199, 2015) — the modelled water column is ~2× too transparent, and the empirical kd relation it feeds was itself derived from those measurements (inverting to K_B_E ≈ 2.18). Imposing the realistic value makes EVERYTHING worse: Chl-a 30.2→25.3 (below obs 32.9), phyto C −15 %, PO4 bias +0.022→+0.028, ext-Φ −5.0 %. ⇒ the model's apparent chlorophyll skill rests on COMPENSATING ERRORS — it needs ~2× excess light to reach observed chlorophyll, so its production engine is under-powered by about that factor, and the nutrient over-prediction is *worse* under an honest light climate.** ⇒ New §3 candidate ahead of the phyto-side items: **audit the light/production sub-model** (K_B_E realism + the growth engine) — a physically-grounded, measurement-backed target, unlike the exhausted kinetics levers. |
| Identifiability-guided calibration (following the Morris screen) | **Done (2026-08-03, PR #102)** | `tools/calibrate_cl29.py` (scipy DE, no-PEST++) + `tools/eval_fullrecord_points.py` (controlled full-record grid). **Finding:** the calibration is limited by the **biomass↔nutrient↔Chl-a multivariate wall on the full record** — PO4+Si are ~64% of the misfit and only reducible by over-growing biomass (→ Chl-a +11), so the *adoptable* (Chl-a-honest) refinement is modest. Window-nonstationarity is secondary/narrow (denit rails to 2.97 on a 2-yr window but **saturates ~2.0 and does not rail on the full record**). **The shipped WCONST defaults are already near-optimal** (best adoptable nutrient calibration = +4.6% full-record Φ vs the illusory 2-yr +26.6%). Defensible refinement: denit 1.0→~1.5–2.0 + modest nitrif/PON-regeneration + `KHS_DIP_DIA`≈0.003 (zeroes DIN biases); **WCONST left unchanged — adoption is the user's scientific call**. Full write-up: `docs/CL29_Calibration_Results.md`. See [[cl29-calibration-wall]]. |

## 3. Science / model richness
*Source: paper §11.2 — larger lifts.*

| Item | Priority | Notes |
|------|----------|-------|
| **⭐ Surface-bloom persistence state (positional ratchet + storm reset)** | **✅ DONE + ADOPTED (2026-08-14/15, doc §20/§23)** | Built as `CYANO_POS_MODEL=2` — module `AQUABC_POSITIONING_STATE`, per-box surface fraction `S_POS(nkn,3)`, dS/dt = K_UP·F_calm·(1−S) − K_DISP·F_storm·S, hysteresis formation 3 / dispersal 8 m/s (merged `a92b33f`; surface self-shading of the positioned fraction added §22, `299eccd`). Operational since §23 — live `INPUTS_CL29/PELAGIC_MODEL_OPTIONS.txt`: `CYANO_POS_MODEL 2`, `K_POS_UP 3`, `K_POS_DISP 10`, `W_DISP_POS 8`. Residual margins are the two rows below: Aug–Sep ≈ −16 µg/L (S-dynamics refinement + photoacclimative C:Chl) and October (akinete staging). Independent validation idea (unstarted): date real surface-accumulation episodes from the OLCI/MPH satellite record and compare against the modelled `S_POS` trajectory. |
| **⭐ Akinete staging (life-cycle transition logic for NOST/FIX)** | **✅ ADOPTED (doc §31, 2026-08-29): the staged fixer IS operational CL29** — role-swap closed the cycle (form/germ 1.1–1.7 every year, bank self-sustaining in production); DE (180 evals) could not beat the hand optimum. | Built on `feature/nost-akinete-staging` (opt-in `NOST_STAGE_MODEL`, bed akinete bank + radiation latch, flag=0 byte-identical to `main`). **Ladder result (2026-08-25, doc §29): the bed bank goes EXTINCT within 4 years (6-order-of-magnitude collapse, no recovery in the remaining 7 years) and never delivers the October CHLA/PO4/FIX_TOT gains it was built for — every biologically live headline score is unchanged from the pre-staging baseline — but TN RMSE −9.5% and TP RMSE −28% improve, traced to the model's initial `AKI_C` charge draining faster under staging during the 2012–2013 spin-up (zero difference 2014 onward at the scored boxes) — a bulk N/P mass-accounting gain confined to the spin-up years, not an ongoing one. (Separately, `AKI_C` is measured pinned at its 8.0 mg C/L initial condition for 7 years in 5 non-scored boxes under the legacy gate — a real defect, but not traced as the cause of the TN/TP score delta.) No-recruitment control confirms even the best case (germination fully off) decays with a 1.9-year half-life to 2% of peak in 11 years — the mechanism cannot self-sustain at any default setting.** October: model ~25 vs obs ~46 µg/L even post-boundary (§21/§27). NOST is inoculum-limited, not temperature-limited: `AKI_C` drains 0.73→0.002 by August, and T_min 16→8 was a NULL (doc §9). The CLC literature (Hense & Beckmann; `docs/Diazotroph_phenology_modelling_review.md`) gates transitions differently from growth — heterocyst formation by DIN limitation, akinete formation by an ENERGY/light cue (what holds the bloom into autumn), stage-specific buoyancy, between-year inoculum memory. AQUABC already carries the veg/het/akinete STATE VARIABLES; what is missing is the transition logic. Interacts with the ratchet's storm resets (both shape Oct). **✅ RESOLVED + ADOPTED (2026-08-29, doc §29–31): staging BUILT (opt-in `NOST_STAGE_MODEL`, merged), self-sustainment initially FAILED (the niche was held by the tuned FIX_CYN surrogate — every past DE had voted the real species' life cycle out), the role-swap + tuning ladder closed it, and the staged-fixer configuration IS operational CL29 — CHLA RMSE 24.22 (best ever), peak Aug exact, PO4 −21 %, self-sustaining bank verified in production. The §28 autumn PO4/DO payoff conjecture is superseded; Oct/Nov residuals belong to the autumn CYN/diatom guilds.** |
| **Second diatom guild (June collapse / August return)** | P2 — **re-measured 2026-08-23 (doc §28): STANDS, sharpened** | Post-boundary obs-matched monthly ratios: cool-season excess Feb 2.29× / Mar 1.90× / **Jun 1.94×** (the observed June clear-water collapse to 0.196 mg C/L is not reproduced), warm-season absence total (Jul 0.08×, **Aug 0.005×**, Sep 0.01×, Oct 0.03×). Si carries the matching signature — Aug–Oct still **2.0–2.2×** (the §26 boundary fix bought the RMSE via autumn–winter drawdown; the *summer* Si consumer is still missing). One CTMI envelope cannot do both sides — the wide-envelope experiment (doc §11) fixed Si/winter but destroyed June and ate the fixers' P. Cool guild lit T_opt≈14 (*Stephanodiscus*); the warm guild (*Actinocyclus*/*Skeletonema subsalsum*) has no clean published cardinals. |
| Photoacclimative C:Chl (structure — never a knob) | P2 | The fifth compensation channel (doc §22): C:Chl handed to the objective — even bounded by the measured IQR — fills the CHLA gap with pigment, not biomass (Feb 54 vs obs 10, r collapse to +0.47). Operationally FIXED at the measured 53/78; the honest closure is photoacclimative structure (Geider-class). Also the leading candidate for part of the Aug–Sep margin. |
| CYN summer-guild absence | **LARGELY RESOLVED (doc §35–36, 2026-08-30): the guild is 65–70 % *Planktothrix*; its low-loss traits (KD 0.04, pref 0.03, KG 2.0) ADOPTED** — CYN present 2–5× Jun–Nov, CHLA/PO4 project bests. Remainder: the **August exclusion at bloom peak (0.16 vs 2.30)** — structural (depth-regulating filament biology), alongside the July knot. |
| Housekeeping: unify the two parallel constant-INIT implementations | P3 — the class-(h) source (doc §34) | `INIT_PELAGIC_MODEL_CONSTANTS` exists twice: the ESTAS-side positional-assignment version (mod_PELAGIC_ECOLOGY, where the BETA block was dead until `ebab415`) and the AQUABC-side para_aqua registry pair. Any future constant append must touch both — or, better, the ESTAS side should delegate to the AQUABC INSERT/INIT pair so appends are single-sited. Same family as [[aquabc-parallel-code-paths]]. |
| Housekeeping: `S_POS` double-integration under the RK2 opt-in solver | P3 — latent, found by the 2026-08-23 staging-design review | `AQUABC_POSITIONING_STATE` updates `S_POS` inside the kinetics call, which the RK2/Heun opt-in (`ESTAS_PELAGIC_SOLVER=2`) evaluates twice per step — the ratchet state integrates at ~2× its nominal rates under RK2. Harmless operationally (CL29 runs Euler; RK2 is experimental opt-in) but should move to the solver-side once-per-step pattern specced in `docs/superpowers/specs/2026-08-23-nost-akinete-staging-design.md` §4.4 when next touched. |
| Housekeeping: dead/vestigial buoyancy code | P3 | `CYANO_BOUYANT_STATE_SIMULATION` is read, printed, passed and never used (a placebo switch since ≥2019); the non-buoyant `FIX_CYANOBACTERIA` first variant carries a vestigial `FIX_CYN_DEPTH = 1.0` scalar multiplier (2013 "×1.2 surface layer", neutralized 2014) — not the CL29 path. Remove or wire both when next touching the libs. |
| Variable-stoichiometry option for key groups (Si:C, N:C plasticity) | P2 — **CL29 Droop-N BUILT + LADDERED, storage REFUTED (doc §38, 2026-08-30); adoption is an open user decision** | **2026-08-30 result: the re-scoped CYN Droop-N pilot ships as opt-in `CYN_VARIABLE_N` on the `ESTAS_II_varN` build variant (standard build byte-identical to `main` at full record); August *Planktothrix* 0.218 → 0.503 mg C/L (obs 2.304, ×2.3), CHLA 24.05 → 24.02, CHLA peak month 9 → 8 (obs-exact), at PO4 +2.1 % and r −0.02 — but the pre-registered quota signature REFUTES the storage hypothesis (August Q 0.231 vs a 0.175 mid-band bar, only 1.9 % of samples below it): the gain is explicit high-affinity uptake of the regeneration flux (5.6 DIN-stock turnovers/day in August), not June→August storage, so a 32-state uptake reformulation may buy the same without the build variant. Also learned, reusable: a "turn group X off" scenario must zero X's GROWTH constant — `MIN_CONCENTRATION` (1e-10) reseeding regrew diatoms 1e-10 → 1.89 mg C/L in 30 days and broke the N-conservation gate.** Stands as a *general model-richness* goal (selective ERSEM-style, no full rewrite). **⚠️ The earlier (2026-07-25) "variable N:C would strip summer DIN to depletion" motivation is now CONTRAINDICATED for CL29 (2026-08-02).** A scoped Droop-N pilot on CYN was designed and adversarially **Workflow-reviewed (21 confirmed findings, 8 BLOCKING)**; the premise fails — CL29 summer DIN is **regeneration/boundary-floored** (CYN already ~90% N-replete, `KHS_DIN_CYN=0.009`), so a luxury quota is a small bounded store that remineralizes back into the EPA-matching NH4 floor (net export second-order). This is the **3rd phyto-side lever** (after Nostocales, FIX_CYN/#76) to die on the same wall → the CL29 nutrient over-prediction is likely **boundary-forcing / regeneration structural, not phyto-fixable**. Before any future var-stoich (or phyto-kinetics) work, first establish that a genuinely *uptake-limited* target exists; if the wall is boundary-driven, the lever is open-boundary forcing (`cl29-epa-validation`, boundary×0.5). Full write-up: `docs/superpowers/specs/2026-08-01-variable-stoichiometry-cyn-droop-n-design.md` §12. |
| **Benthic P-retention / burial process — ~~the summer-PO4 residual~~** | **P3 / blocked-on-akinete — the premise is DEAD (re-measured 2026-08-23, doc §28)** — summer PO4 is now UNDER-predicted (Jul/Aug/Sep model/obs 0.16/0.16/0.24); the residual MOVED to **autumn: Oct 6.1×, Nov 4.9× (bias +0.052)** + Jun 4.1×, coinciding month-for-month with the missing autumn bloom (CHLA Oct −25.9, Nov −20.4) — likely the SAME defect as the Oct gap (missing autumn biomass = missing P consumer), and a benthic sink would now WORSEN Jul–Sep. **Re-measure again after akinete staging.** Historical detail below kept for the record: | CL29 over-predicts SUMMER PO4 ~10× (obs 0.005 → model 0.047; winter matches). Confirmed a **structural residual** (2026-08, v0.11.0): it resisted over-growing biomass (Chl-a +11), a config-only benthic PO4 sink (crashes Chl-a 25→8), var-stoich-P (baseline nutrient-**replete**, LIM_P=0.85 → fails the `LIM≪1` precondition), and boundary-P supply (a small clean gain *was* adopted — `CL29_BOUNDARY_PO4_SUMMER_PEAK` 2.0→1.0, #108). The model interior summer PO4 **>** the boundary PO4 → **internal-regeneration-dominated**; closing it needs a genuine P *removal* process the water-column model lacks: **redox-dependent oxic (Fe-oxyhydroxide-bound) P burial**, or **benthic primary-producer P uptake** (macrophyte/periphyton). A **Fortran model-richness effort** (new process/state variable), NOT config/kinetics tuning. ⚠️ Hard prereq: any candidate must draw PO4 down **without** inducing P-limitation (the wall) — i.e. remove the P the replete bloom isn't using; a fixed sink over-draws and crashes Chl-a (tested). See `docs/CL29_Calibration_Results.md` §"Summer PO4" + [[cl29-calibration-wall]]. |
| Re-introduce explicit bacteria as a library (nitrifiers / heterotrophs / denitrifiers + electron acceptors) | P2 | Restores dynamic remineralization; corrects the organic-carbon underestimate. |
| Function-oriented zoobenthos library (filters / shredders / predators; e.g. *Dreissena*) | P3 | General, not Curonian-specific. |
| Unify ESTAS box ↔ SHYFEM 3-D (one kinetic core, two deployments) | P3 | Removes drift between standalone and SHYFEM-bundled versions. |
| Uncertainty quantification (ensembles) | P3 | Needs high-resolution boundary + calibration data. |

## 4. Science — Curonian Lagoon (data-blocked / decided)
*Status as of 2026-07-23; from project notes.*

| Item | Status |
|------|--------|
| CL29 facies map + silica calibration | **Si RESOLVED (2026-08-17, doc §26):** the oldest residual (3.4× summer Si over-prediction) was a MISSING BOUNDARY DIATOM SUPPLY — the symmetric 4-group boundary takes Si RMSE 1.59→0.82 (bias −84 %) — not kinetics or burial; no Si calibration needed. The sediment facies map remains **data-blocked** (sediment-side work only). |
| CL29 sediment Phase 2 (muddy-anoxic-P flux) | **NO-GO** at the §4.1a gate under advanced-redox = 0. |
| Nostocales multi-year bloom | **Won't-fix** — de-risked as not-defensibly-fixable (competitive exclusion under P-limitation). **Superseded framing (2026-08):** NOST is inoculum-limited (the akinete pool drains by August); the live remedy is §3 akinete staging, not growth kinetics. |
| OPA extinction (bias −0.47 in every config ever run) | **Decided (2026-08-17, doc §24–27): internal competitive exclusion, confirmed from three directions** — boundary supply raised 18× (modelled OPA *fell*), raised KG_OPA, and full recalibration all leave OPA at −0.476. §24's boundary-asymmetry attribution was refuted by the §26 experiment; the §3.6 composition-wall reading stands. Report as a documented structural limit, not a calibration deficiency. |
| CL29 PEST-posterior promotion | **Abandoned** — non-stationarity; PR #61 landed net-zero, kept only the tooling. |
| FIX_CYN (N2-fixing cyano) to observed biomass | **SUPERSEDED — FIXED (2026-08-11)**: the wrong number was `FIX_CYN_OPT_TEMP_LR`=18 (a Nodularia value; the lagoon is Aphanizomenon/Anabaena, lit T_min≈8). Adopted T_min=8 + N-cycle recalibration ⇒ fixer bias **+0.03** vs obs 0.70 mg C/L, Chl-a peak Feb→Sep, seasonal r −0.70→+0.53, best-ever CHLA RMSE 26.11. See `docs/CL29_phenology_diagnosis.md`. Original verdict below kept for the record: **Won't-fix / decided (2026-07-25)** — temperature diagnosis correct (T_opt=26 too warm) but FIX_CYN-as-a-*fixer* is not reproducible: NH4-floor competitive exclusion (summer DIN floored ~0.06 by regeneration-driven NH4 that matches EPA → non-fixers stay 87% N-replete; fixers never competitive). Growing it to observed biomass also regresses Chl-a/Si/TN (single-var vs multivar tension). Same class as Nostocales. See `fix-cyn-n2fixation-overprediction`. |
| FIX_CYN bloom phenology / late-summer spike-and-crash (**issue #76**) | **Won't-fix / documented (2026-08-01)** — closed as *not planned* (PR #92 landed the investigation doc). A four-reviewer in-loop review + two CL29 gates found: (a) the proposed density-dependent bloom-termination term is **unnecessary** — corrected CTMI-valid temperature windows alone give the Sep spike-and-crash (winter clear is temperature-forced, not loss-driven); (b) the prior "structural persistence" was a **CTMI-invalidity artifact** (invalid window → plateau-fallback branch disables the cold-season growth-off switch); (c) the phase-fix **regresses multivariate skill** vs EPA (PO4 −19% but NH4 +30%, DO/Si/TN/TP/Chl-a worse) — same wall as the row above — **now superseded (2026-08-11): with T_min=8 + the N-cycle recalibration the phase fix improves EVERY headline metric simultaneously** (no wall trade). Reusable lesson: verify the temperature-model branch is *valid* before trusting a "needs a formulation change" diagnosis. See `docs/superpowers/specs/2026-08-01-fix-cyn-bloom-termination-design.md` (§12.4/§12.5) and `fix-cyn-n2fixation-overprediction`. |
| CL29 summer-NO3 over-prediction | **Partial fix ADOPTED (2026-07-25, `78ce4ad`)** — benthic denitrification `CL29_BENTHIC_DENIT` (config-only, converter option) closes ~20% of the summer gap: NO3 bias +0.065→+0.033, TN/DO better, small structural PO4 regression (+5.7% RMSE, unavoidable). Residual ~80% is spring-pool-drainage-dominated + the aseasonal-diatom-metabolism limit (uptake blocked by **fixed N:C** — see §3 variable-stoichiometry). A documented multi-factor residual alongside PO4/Si. |

---

## Phase 5.1 status: substantially complete — no shovel-ready slice remains

`mod_GLOBAL`'s loose pelagic allocatables are down **12→4** (pelagic-core `pcore` Tiers 1–2 merged); the
resuspension, bottom-sediment and water-coupling subsystems were done earlier. **Tier 3 (the last 4 core
arrays) is a decided NO-GO** — cosmetic count with zero coupling change vs the highest risk/effort (see 5.1
above). So there is no low-risk mechanical de-globalization slice left to pick up. What remains is either
deferred-by-design (5.2 CO2SYS-lib, 5.3 higher-order solvers, 5.4 config-driven runtime — all low priority),
science (§3, esp. akinete staging and the second diatom guild — variable N:C is contraindicated for CL29) or
waiting on data/decisions (§4). The next *substantive*
engineering lift would be an actual de-coupling (lift `pcore`/`bsed`/`wsc`/`resusp` out of `GLOBAL` into
their own modules) — a design effort, not a mechanical slice.

## Sources (authoritative detail — do not duplicate here)

- `FORTRAN_IMPLEMENTATION_PLAN.md` — §8 Phase 5 task detail, §2.2 open-issue table, §11 verification.
- `~/curonian/docs/EUTROPY_AQUABC_comparison*` — §10.3 (AQUABC improvements), §11.2 (AQUABC roadmap).
- `FIXES_AND_IMPROVEMENTS.md` — all items resolved (historical snapshot).
- `CHANGELOG.md` — release history and per-release verification records.
