# AQUABC / ESTAS — Consolidated Backlog

Single internal tracker. It consolidates the open items that were previously scattered across
`FORTRAN_IMPLEMENTATION_PLAN.md` (§8 Phase 5, §2.2 issue table), the EUTROPY↔AQUABC comparison
paper (`~/curonian/docs/EUTROPY_AQUABC_comparison*` §10.3 / §11.2), and the science/calibration
campaigns tracked in project notes. **Detailed task specs live in the referenced sources — this
file is the prioritized index, not a duplicate**, so it can't drift from the plan.

**Last updated:** 2026-08-06 · **Latest release:** v0.11.0 · Fortran-plan Phases 1–4 and every item
in `FIXES_AND_IMPROVEMENTS.md` are complete; there are **no open GitHub issues** (the one that was open,
#76 FIX_CYN phenology, was investigated and closed as *not planned* — see §4).

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

## 0. In-flight — resuspension × sediment-diagenesis coupling

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

**Phase 1 of the coupling is complete and verified** — the branch is now merge-ready (pending review). Only the
optional Phase 2 (shear-driven erosion realism) remains.

Two mergeable side-branches already split out: `feature/pelagic-negmass-diagnostics` (Ali's negative-mass
diagnostics refactor) and `docs/coupling-reconciliation` (the design reconciliation + this backlog refresh).

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
| **⭐ Surface-bloom persistence state (positional ratchet + storm reset)** | **P1 — the single item between CL29 and an honest-optics configuration matching observations** | Doc §19 (2026-08-14): the measured ladder (no gate 14.9 → Nagy 15.1 → scum-threshold 18.0 µg/L Aug vs obs 50.8) extrapolates to the observed plateau exactly at full persistence (F→1, light factor 0.9 at 0.5 m in kd≈2.9). Needs a state (surface-accumulated biomass fraction, built up in calm spells, dispersed by storms), NOT further wind-statistics parameterization — that class is exhausted 3 rungs deep. Companion machinery already merged (`CYANO_POS_MODEL`/`H_SURF_POS`/`W_CRIT_POS_MIN`, byte-identical default). Same family: akinete staging (§9 — Oct inoculum), second diatom guild (§11 — June collapse/Aug return + the Si consumer). |
| Housekeeping: dead/vestigial buoyancy code | P3 | `CYANO_BOUYANT_STATE_SIMULATION` is read, printed, passed and never used (a placebo switch since ≥2019); the non-buoyant `FIX_CYANOBACTERIA` first variant carries a vestigial `FIX_CYN_DEPTH = 1.0` scalar multiplier (2013 "×1.2 surface layer", neutralized 2014) — not the CL29 path. Remove or wire both when next touching the libs. |
| Variable-stoichiometry option for key groups (Si:C, N:C plasticity) | P2 — **CL29: contraindicated** | Stands as a *general model-richness* goal (selective ERSEM-style, no full rewrite). **⚠️ The earlier (2026-07-25) "variable N:C would strip summer DIN to depletion" motivation is now CONTRAINDICATED for CL29 (2026-08-02).** A scoped Droop-N pilot on CYN was designed and adversarially **Workflow-reviewed (21 confirmed findings, 8 BLOCKING)**; the premise fails — CL29 summer DIN is **regeneration/boundary-floored** (CYN already ~90% N-replete, `KHS_DIN_CYN=0.009`), so a luxury quota is a small bounded store that remineralizes back into the EPA-matching NH4 floor (net export second-order). This is the **3rd phyto-side lever** (after Nostocales, FIX_CYN/#76) to die on the same wall → the CL29 nutrient over-prediction is likely **boundary-forcing / regeneration structural, not phyto-fixable**. Before any future var-stoich (or phyto-kinetics) work, first establish that a genuinely *uptake-limited* target exists; if the wall is boundary-driven, the lever is open-boundary forcing (`cl29-epa-validation`, boundary×0.5). Full write-up: `docs/superpowers/specs/2026-08-01-variable-stoichiometry-cyn-droop-n-design.md` §12. |
| **Benthic P-retention / burial process — the summer-PO4 residual** | P2 — **demoted (2026-08-11): no longer the only lever** — the T_min=8 fixer bloom consumed much of the excess (PO4 RMSE −26 %, bias +0.022→+0.011); the residual is halved but real | CL29 over-predicts SUMMER PO4 ~10× (obs 0.005 → model 0.047; winter matches). Confirmed a **structural residual** (2026-08, v0.11.0): it resisted over-growing biomass (Chl-a +11), a config-only benthic PO4 sink (crashes Chl-a 25→8), var-stoich-P (baseline nutrient-**replete**, LIM_P=0.85 → fails the `LIM≪1` precondition), and boundary-P supply (a small clean gain *was* adopted — `CL29_BOUNDARY_PO4_SUMMER_PEAK` 2.0→1.0, #108). The model interior summer PO4 **>** the boundary PO4 → **internal-regeneration-dominated**; closing it needs a genuine P *removal* process the water-column model lacks: **redox-dependent oxic (Fe-oxyhydroxide-bound) P burial**, or **benthic primary-producer P uptake** (macrophyte/periphyton). A **Fortran model-richness effort** (new process/state variable), NOT config/kinetics tuning. ⚠️ Hard prereq: any candidate must draw PO4 down **without** inducing P-limitation (the wall) — i.e. remove the P the replete bloom isn't using; a fixed sink over-draws and crashes Chl-a (tested). See `docs/CL29_Calibration_Results.md` §"Summer PO4" + [[cl29-calibration-wall]]. |
| Re-introduce explicit bacteria as a library (nitrifiers / heterotrophs / denitrifiers + electron acceptors) | P2 | Restores dynamic remineralization; corrects the organic-carbon underestimate. |
| Function-oriented zoobenthos library (filters / shredders / predators; e.g. *Dreissena*) | P3 | General, not Curonian-specific. |
| Unify ESTAS box ↔ SHYFEM 3-D (one kinetic core, two deployments) | P3 | Removes drift between standalone and SHYFEM-bundled versions. |
| Uncertainty quantification (ensembles) | P3 | Needs high-resolution boundary + calibration data. |

## 4. Science — Curonian Lagoon (data-blocked / decided)
*Status as of 2026-07-23; from project notes.*

| Item | Status |
|------|--------|
| CL29 facies map + silica calibration | **Data-blocked** — needs the sediment facies map and Si observations. |
| CL29 sediment Phase 2 (muddy-anoxic-P flux) | **NO-GO** at the §4.1a gate under advanced-redox = 0. |
| Nostocales multi-year bloom | **Won't-fix** — de-risked as not-defensibly-fixable (competitive exclusion under P-limitation). |
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
science (§3, esp. variable N:C stoichiometry) or waiting on data/decisions (§4). The next *substantive*
engineering lift would be an actual de-coupling (lift `pcore`/`bsed`/`wsc`/`resusp` out of `GLOBAL` into
their own modules) — a design effort, not a mechanical slice.

## Sources (authoritative detail — do not duplicate here)

- `FORTRAN_IMPLEMENTATION_PLAN.md` — §8 Phase 5 task detail, §2.2 open-issue table, §11 verification.
- `~/curonian/docs/EUTROPY_AQUABC_comparison*` — §10.3 (AQUABC improvements), §11.2 (AQUABC roadmap).
- `FIXES_AND_IMPROVEMENTS.md` — all items resolved (historical snapshot).
- `CHANGELOG.md` — release history and per-release verification records.
