# AQUABC Changelog

All notable changes to the AQUABC model and Shiny frontend are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

---

## [Unreleased]

## [0.10.0] - 2026-08-03

> 🎉 **Released — [AQUABC v0.10.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.10.0)**
> (Linux binary attached). **Calibration-rigor toolkit + identifiability-guided recalibration of CL29.**
> Adds a Method-of-Morris sensitivity/identifiability screen and a self-contained parallel calibrator,
> plus the PEST++ workflow guide, a reproducible-build container, and a fail-loud constants reader — and
> adopts a verified nutrient recalibration for CL29 (+3.2 % full-record fit, DIN biases zeroed).

### Added
- **Fail-loud constants reader.** `READ_MODEL_CONSTANTS` (the shared positional reader for the pelagic
  `WCONST_04.txt` and the sediment `W_SED_CONST.txt`) now rejects an incomplete/invalid constants file —
  a missing, out-of-range, or duplicate index, or a malformed line — with a nonzero `error stop` that
  names the offending index, instead of silently defaulting a dropped constant or corrupting memory on an
  out-of-range write. Set `AQUABC_LENIENT_CONSTANTS=1` to restore the old warn-and-continue behaviour.
  Byte-identical for the shipped (index-complete) setups. Also completed the 0D example constants files
  (`const_CL.txt`/`const_default.txt`) to their full set, and added a reproducible-build container.
- **PEST++ calibration workflow guide** (`docs/CL29_Calibration_PEST_Workflow.md`) — documents the external
  calibration harness (Φ objective, adjustable parameters, run instances) that stands in for the
  deliberately-stubbed in-Fortran cost function.
- **Global sensitivity / identifiability screen** — Method of Morris (`tools/sensitivity_morris.py`), a
  self-contained (no PEST++) parallel screen that reuses the CL29 forward model to rank which `WCONST`
  constants the EPA data can constrain. Result: phytoplankton kinetics (cyano mortality, diatom/cyano
  growth) are identifiable; the Si half-saturation and biogenic-Si dissolution constants are not.
  `docs/CL29_Sensitivity_Analysis.md`.
- **Identifiability-guided calibration** — `tools/calibrate_cl29.py` (scipy differential-evolution) and
  `tools/eval_fullrecord_points.py`, a no-PEST++ parallel calibrator. Established that the
  biomass↔nutrient↔Chl-a multivariate wall (PO4 + Si ≈ 64 % of the misfit, reducible only by over-growing
  biomass) is the primary full-record limit, and that the shipped defaults were already near-optimal.
  `docs/CL29_Calibration_Results.md`.

### Changed
- **CL29 nutrient recalibration (adopted).** `CL29_WCONST_OVERRIDE` in
  `tools/eutropy_poc/eutropy_to_estas.py`: denitrification `K_MIN_DOC_NO3N_20` 1.0→1.5, nitrification
  `K_NITR_20` 0.6→1.0, PON→NH4 regeneration `KDISS_DET_PART_ORG_N_20` 0.25→0.4, diatom P-affinity
  `KHS_DIP_DIA` 0.005→0.003. Verified on the full 11-yr EPA record: +3.2 % Φ, DIN biases zeroed
  (NH4 +0.010→+0.004, NO3 +0.033→+0.015), Chl-a bias improved (−3.2→−2.1), PO4/Si unchanged (structural).
  The deliberately-tuned diatom/OPA phosphate competition survives the `KHS_DIP_DIA` change (OPA −4…−6 %).
  CL29-only; the Standard setups are unaffected.

## [0.9.1] - 2026-08-01

> 🎉 **Released — [AQUABC v0.9.1](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.9.1)**
> (Linux binary attached). **Internal refactoring only — pelagic-core `GLOBAL` de-globalization (Phase 5.1),
> byte-identical.** Bundles 8 loose pelagic water-column allocatables in `mod_GLOBAL` into a single
> derived-type instance `pcore` (`pelagic_core_t`), dropping the loose-allocatable count 12 → 4. **No
> behavioural or output change — every model run is byte-identical to v0.9.0.**

### Changed
- **Pelagic-core `GLOBAL` de-globalization (Phase 5.1, byte-identical).** Moved 8 of the 12 loose pelagic
  water-column arrays out of `mod_GLOBAL`'s global scope into one `pcore` instance — Tier 1 (`node_active`,
  `SAVED_OUTPUTS`, `CHLA`, `WATER_COLUMN_OUTPUT`, `SURFACE_BOXES`) and Tier 2 (`DERIVATIVES`,
  `DRIVING_FUNCTIONS`, `FLAGS`). Pure `X` → `pcore%X` renames; verified byte-identical across Standard
  (`MODEL_SEDIMENTS=0`), CL29 (`=1`) and the `MODEL_SEDIMENTS=2` sediment test, with clean gfortran + OpenMP
  builds and an unchanged `-Wunused` set. `GLOBAL` loose-allocatable count 12 → 4. Tier 3 (the last 4 core
  arrays — `pH`/`STATE_VARIABLES`/`MODEL_CONSTANTS`/`PROCESS_RATES`) is a decided **no-go** (a cosmetic
  count reduction with zero coupling change, since `pcore` stays in `mod_GLOBAL`, against the highest-risk
  tier).

### Documentation
- Design spec + implementation plan for the pelagic-core de-globalization — hardened by a 4-way adversarial
  in-loop review + a 5-finder workflow review — and the Tier-3 no-go decision, under
  `docs/superpowers/{specs,plans}/2026-08-01-pelagic-core-*`. `BACKLOG.md` §1 5.1 updated to substantially
  complete.
- Added the model description paper to the README citation section: Ertürk, A., Šakurova, I., Žilius, M.,
  et al. (2023), *Ecological Modelling* 486, 110509, <https://doi.org/10.1016/j.ecolmodel.2023.110509>.

## [0.9.0] - 2026-07-31

> 🎉 **Released — [AQUABC v0.9.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.9.0)**
> (Linux binary attached). **Headline: resuspension now runs together with full sediment diagenesis via a
> mass-conserving bed↔water particulate coupling.** Prescribed-velocity "Option 3" erodes the surface bed
> layer and transfers the eroded particulate C/N/P/Si to the water-column detritus pools, decrementing the
> bed by the same mass — the previously-guarded, never-run combination. The release also lands a
> sediment-redox Fe(II) salt-selection bugfix and a negative-mass diagnostics refactor, consolidated from a
> collaborator's tree. **Standard (25-box, `MODEL_SEDIMENTS=0`) runs remain byte-identical to v0.8.0.**

### Added
- **Resuspension × sediment-diagenesis coupling (Phase 1).** Bed resuspension ("Option 3" — a per-box
  prescribed erosion-velocity time series) can now run together with full diagenesis (`MODEL_SEDIMENTS=2`):
  an erosion term removes particulate C/N/P/Si from bed layer 1 and delivers it to the water-column detritus
  pools (`DET_PART_ORG_C/N/P`, `PART_Si`) by the same areal mass — a mass-conserving two-way transfer — and
  the guard that previously halted diagenesis+resuspension is lifted for Option 3. All new behaviour is gated
  on `MODEL_SEDIMENTS>1 .and. resuspension`, a combination no existing setup runs. Verified over 365-day
  coupled runs: flux-level bed_out/water_in ratio = 1.0; diagenesis-without-resuspension **byte-identical**;
  eroded C/N/P/Si delivered to the water column matches the bed decrement. Phase 2 (shear-driven erosion) is
  deferred by design.

### Changed
- **Sediment-redox Fe(II) salt-selection bugfix.** `SED_REDOX_AND_SPECIATION` picked the precipitating
  Fe(II) salt by reducing the wrong axis of the `(box, layer, salt)` activity-ratio array
  (`maxloc(..., dim=2)`, the *layer* axis); corrected to `dim=3` (the *salt* axis). This changes sediment
  redox/speciation output for **advanced-redox diagenesis runs** (a correctness fix, not a no-op);
  no-diagenesis runs (e.g. Standard) are unaffected.
- **Negative-mass / negative-concentration diagnostics** extracted from the pelagic solver loop into
  dedicated `CHECK_NEW_PELAGIC_MASS` / `CHECK_NEW_PELAGIC_CONCENTRATION` routines in `mod_PELAGIC_ECOLOGY`
  (readability/locality; behaviour preserved).
- **Sediment `STRANGER` sanity band widened** ±1e4 → ±1e5 for headroom on coupled runs; the NaN/Inf guard is
  unchanged, so true blow-ups are still caught (changes only the finite out-of-range trip threshold).

### Documentation
- **Resuspension × sediment-diagenesis coupling design + `ali_version` reconciliation** recorded under
  `docs/superpowers/specs/2026-07-30-resuspension-diagenesis-coupling-design.md`.
- **BACKLOG.md refreshed** — coupling Phase-1 marked complete; the next shovel-ready engineering slice is the
  remaining 12-allocatable pelagic-core `GLOBAL` de-globalization; variable N:C stoichiometry remains the
  flagged science lever for the CL29 summer-NO3 residual.

## [0.8.0] - 2026-07-25

> 🎉 **Released — [AQUABC v0.8.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.8.0)**
> (Linux binary attached). **Headline: Phase-5 de-globalisation continues, the pelagic solver becomes
> selectable, and CL29 gains a benthic-denitrification nitrogen sink.** Two more byte-identical
> `GLOBAL`→derived-type slices (bottom-sediment `sediment_state_t`, settling / water-coupling
> `wsc_state_t`) drop `GLOBAL`'s allocatable count 44 → 12. The pelagic solver is now selectable via
> the `ESTAS_PELAGIC_SOLVER` env var (default Euler, byte-identical; RK2/Heun an experimental opt-in).
> A new config-only `CL29_BENTHIC_DENIT` sediment NO3 sink addresses the CL29 summer-NO3
> over-prediction (EPA-verified: NO3 bias more than halved). Standard model runs remain byte-identical
> to v0.7.0.

### Added
- **Configurable pelagic solver** (`ESTAS_PELAGIC_SOLVER` env var). Default (unset / `1`) is Euler and
  byte-identical to prior releases; `2` selects the RK2/Heun solver as a **documented experimental**
  opt-in — an investigation established it converges ~1st-order for this model (dominated by
  `MIN_CONCENTRATION` clamping) and is not better than Euler per unit cost. Ships with a Shiny
  run-control selector and the banked RK2 correctness fixes (volume RK2-averaging + stage-2 forcing
  re-evaluation).
- **Benthic denitrification NO3 sink for CL29** (`CL29_BENTHIC_DENIT` converter option). A summer-peaked
  prescribed sediment-interface NO3 sink (config-only, no Fortran) addressing the diagnosed summer-NO3
  over-prediction — the water-column-only model lacks the dominant shallow-lagoon N sink (its own
  water-column denitrification is O2-throttled). EPA-verified over 11 years: NO3 bias more than halved
  and TN/DO improved, with a small, structural PO4 trade-off; off by config for a byte-identical baseline.

### Changed
- **Phase-5.1 `GLOBAL` de-globalisation continues** (both byte-identical): the bottom-sediment state
  moves into `sediment_state_t` (`bsed`) and the settling / water-sediment-coupling state into
  `wsc_state_t` (`wsc`, a new leaf module), dropping `GLOBAL`'s allocatable count 44 → 23 → 12. Each
  slice was hardened via adversarial in-loop review and a strip-and-compare byte-identity proof.

### Documentation
- **FIX_CYN (N2-fixing cyanobacteria) investigation — decided negative result.** Documented as not
  reproducible in CL29: an NH4-floor competitive exclusion (summer DIN floored by regeneration-driven
  NH4 that already matches observations, keeping non-fixers N-replete), the same class as the
  Nostocales limit. The multivariable co-calibration spec was blocked by a three-lens in-loop review
  (contradictory observations + the fixation wall). The related summer-NO3 over-prediction is
  root-caused (missing benthic denitrification, now partially fixed), and **variable N:C stoichiometry**
  is flagged in the backlog as the clean future lever to close the remaining gap.

## [0.7.0] - 2026-07-23

> 🎉 **Released — [AQUABC v0.7.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.7.0)**
> (Linux binary attached). **Headline: the `GLOBAL` de-globalisation begins.** The 19-variable
> sediment-resuspension / shear-stress subsystem moves out of the `GLOBAL` god-module into a
> `resuspension_t` derived type — the first bounded, **byte-identical** slice of the long-deferred
> Phase-5 refactor (`GLOBAL`'s allocatable count drops 55 → 44). The Dashboard landing page also
> gains the loadable-setup selector (in two-way sync with the Run Model tab), and the dependency
> stack is refreshed. Fully backward-compatible: the Standard model run is byte-identical to v0.6.0.

### Added
- **Setup selector surfaced on the Dashboard landing page** (`shiny_app/modules/dashboard.py`).
  The loadable-setup selector — previously only on Model Config → Run Model — now also appears on
  the Dashboard, directly above **Quick Run**, so the Standard (25-box) vs CL29 (29-box) choice is
  discoverable where runs are actually launched. It stays in **two-way sync** with the Run Model tab
  selector (both drive the single `run.current_setup` source of truth) and shows the same
  availability notice when a setup's inputs are absent. UI-only; no model-behaviour change, and the
  Run Model tab selector is unchanged.

### Changed
- **Resuspension global state encapsulated into a derived type** (`SOURCE_CODE/ESTAS/`). The
  19-variable sediment-resuspension / shear-stress subsystem was lifted out of the `GLOBAL`
  god-module into a `resuspension_t` derived type with a single module-scoped instance `resusp`,
  owned by `mod_RESUSPENSION` (which already allocated and read every one of them); `GLOBAL`'s
  allocatable count drops 55 → 44. Behaviour-preserving: the Standard (option-2) run is
  **byte-identical** to pre-change output, the rename is a provable pure-prefix of the 19 members
  (verified by strip-and-compare on every consumer file), CL29 (option-0) runs to completion, and
  the Fortran unit tests stay green. First bounded slice of the deferred `GLOBAL` de-globalisation
  (`FORTRAN_IMPLEMENTATION_PLAN.md` §8.1 Task 5.1). Design + two-round adversarial review:
  `docs/superpowers/specs/2026-07-22-resuspension-state-derived-type-design.md`.

### Dependencies
- Refreshed the dependency stack (Dependabot, all CI-green): `ruff` 0.8.0 → 0.15.22, `ipywidgets`
  → ≥8.1.8, `ipyleaflet` → ≥0.20.0, `python-dotenv` → ≥1.2.2, `pre-commit` → ≥4.6.1,
  `actions/setup-python` 6.3.0 → 7.0.0, and `actions/checkout` 7.0.0 → 7.0.1. Dev/CI-only; no
  runtime behaviour change.

## [0.6.0] - 2026-07-22

> 🎉 **Released — [AQUABC v0.6.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.6.0)**
> (Linux binary attached). **Headline: loadable setups.** Load a complete model configuration in
> one click — the Shiny UI now ships a **setup registry** with **Standard (25-box)**, **CL29 —
> Curonian Lagoon (29-box)**, and **CL29 (29-box, 2023 climatology)**. Picking a setup wires up the
> input file, run environment (incl. `ESTAS_HOLD_VOLUME=1` for the CL29 setups), box geometry, and
> result/input directories, with guards that keep the Standard model's inputs safe. This release also
> lands a large UI-run speedup (OpenMP thread cap — ~180× faster on small box models) and reusable
> before/after validation tooling. Fully backward-compatible: the default Standard setup is
> byte-identical to prior behaviour.

### Added
- **Loadable setup registry in the Shiny UI (`shiny_app/setups.py`).** One selection loads a
  complete model configuration — input file + inputs/outputs directories + box count + run
  environment. Ships three setups: **Standard (25-box)**, **CL29 — Curonian Lagoon (29-box)**, and
  **CL29 — Curonian Lagoon (29-box, 2023 climatology)**. Selecting a setup drives the config
  dropdown (filtered to compatible input files), injects `ESTAS_HOLD_VOLUME=1` on both run paths for
  the CL29 setups, sets the box-count selectors (preserving the committed default), points the
  primary results/input views at the setup's directories, and prevents running a setup whose inputs
  are absent (with the exact generate command). Write-guards block overwriting the Standard model's
  `INPUTS/` (scenario apply + the parameter / initial-condition / model-option Save handlers) while a
  non-standard setup is loaded. Backward-compatible: the default Standard setup is byte-identical to
  prior behaviour. Future applications are added as data (one `Setup(...)` entry).
- **`tools/compare_validation_runs.py`** — per-variable obs-weighted RMSE/bias diff between two
  `validate_cl29_vs_epa.py` runs, with a one-sided regression guard (a named variable fails if
  RMSE or |bias| rises beyond a threshold, or its bias sign-flips). Reusable for before/after
  calibration testing.

### Fixed
- **Shiny UI model runs now cap `OMP_NUM_THREADS = min(4, cores)`** in the run environment
  (`shiny_app/compiler_env.py`). An OpenMP-built `ESTAS_II` with `OMP_NUM_THREADS` unset defaulted to
  every core; for the small box-model problems (25–29 nodes) that is **~180× slower** than a small
  thread count, because the per-timestep thread spawn/sync overhead swamps the compute (a CL29 UI run
  went from ~27 h projected back to ~7 min). A user-set `OMP_NUM_THREADS` is respected; serial
  (non-OpenMP) binaries are unaffected.

### Investigated (no model-behaviour change)
- **CL29 PEST-posterior promotion — validated and abandoned.** Promoting the 2022-calibrated
  `KDISS_DET_PART_ORG_P_20`=0.118 + `K_MIN_DOC_NO3N_20`=1.13 into the converter default was tested
  with a measured two-run before/after and **reverted**: `KDISS` closes PO4 over-prediction in both
  windows but induces P-limitation that regresses the EPA 2012–2021 fit (NH4/NO3/Si/Chl-a) — the
  posteriors do not transfer across the 2012–16 hyperbloom vs 2022 low-P regimes (nonstationarity).
  Documented in `docs/CL29_KM_2022-2023_Validation.md`; no default changed.

### Verified (release binary)
- The published `ESTAS_II-0.6.0-linux-x86_64` asset was verified end-to-end after release. It runs
  the **Standard (25-box)** model to full completion (365-day window, exit 0, no NaN) and the
  **CL29 (29-box)** application to **full completion — day 4016 (2012–2022), all 29 boxes, no NaN**,
  with `ESTAS_HOLD_VOLUME=1`. Its CL29 output is **byte-for-byte identical** to the canonical
  `run_cl29.sh` reference run: **29/29 box files identical, max |Δ| = 0** across every field and
  timestep — confirming the serial release build reproduces the reference path exactly (the local
  reference binary is a separate OpenMP build).

## [0.5.2] - 2026-07-21

### Fixed
- **Full Intel `ifx` build now works (release+OpenMP `build-estas` in ~66 s, debug `-O0`
  in ~20 s) and is exercised in CI.** The long-suspected "ifx hangs" was **not** the `-O2`
  optimizer — the whole library compiles at `-O2` in ~1 minute once its modules exist. The
  real cause was `SOURCE_CODE/build/make_lib.sh`'s *speculative multi-pass* build: it
  attempts every source and defers whatever fails because a `use`d module isn't built yet.
  gfortran fails such an attempt in milliseconds, so the passes converge; `ifx`'s front end
  instead **hangs** when a whole-module `use` target is missing, so the build stalled
  indefinitely. Fixed by compiling all module-defining files **before** the leaf files
  (external procedures that define no module, so nothing depends on them) — a leaf file can
  then never see a missing module — plus a per-file Intel compile timeout
  (`IFX_COMPILE_TIMEOUT`, default 300 s) as a safety net. gfortran output is unchanged
  (0D golden regression identical).
- **Three latent standard-conformance bugs gfortran tolerated but `ifx` rejects**, surfaced
  once the build no longer hung — all behaviour-preserving (0D golden unchanged):
  - `PELAGIC_KINETICS` dummy `AQUABC_CALLED_BEFORE` was `real(DBL)` while the actual argument
    is the integer `GLOBAL::CALLED_BEFORE` (forwarded to an integer dummy) — retyped to
    `integer` (ifx #6633; also removed a second latent real→int mismatch downstream).
  - `AQUABC_SEDIMENT_MODEL_1`'s `SED_BURRIAL_RATE_OUTPUTS` was `optional` — never
    `present()`-tested, always passed, and written unconditionally — so `optional` was both
    unnecessary and a latent crash risk; removed, which also dropped the explicit-interface
    requirement the external caller lacked (ifx #8055).
  - `DO_SATURATION`/`KAWIND` in `aquabc_II_pelagic_model` were declared as bare `real`
    scalars but are external functions — added the `external` attribute (ifx #6410).
- **Intel (`ifx`/`ifort`) release build now uses value-safe floating point.** The release
  flags gained **`-fp-model precise`** — Intel defaults to `-fp-model fast=1` at `-O2`,
  which reorders/contracts FP and would diverge from the gfortran release build and the
  bit-reproducible 0D golden (`fast` mode keeps `-fp-model fast=2` deliberately). Also
  added **`-heap-arrays`** to the Intel OpenMP flags so the large per-thread kinetics
  buffers live on the heap instead of overflowing the smaller OpenMP thread stacks
  (runtime alternative: a large `OMP_STACKSIZE`). gfortran is unaffected.

### Added
- **Intel oneAPI (`ifx`) full-build CI job** (`build-intel`) — installs the free Intel
  Fortran compiler from Intel's apt repo on a stock `ubuntu-latest` runner and builds the
  whole AQUABC library + `ESTAS_II` engine at release+OpenMP and the library at debug
  `-O0`, asserting the artifacts exist and that `show-config` carries `-fp-model precise`.
  Completes the Intel half of TODO 3.1.

## [0.5.1] - 2026-07-20

### Changed
- **README rewritten to a best-practice structure** — CI / release / license badges, a
  project overview and component map, a **development-status** section (per-component
  maturity plus the CL29 calibration state), a documentation index, and
  contributing / license / citation pointers. Corrects stale content (the `make test`
  target name → `make test-all`; the "306/318 constants" note) and preserves the
  auto-synced `LATEST_RELEASE` marker.

## [0.5.0] - 2026-07-20

Headline: a full **CL29 (29-box Curonian Lagoon) calibration & validation toolchain**
against the Lithuanian EPA monitoring archive and 2015 experimental process-rate data,
plus a **CO2SYS carbonate-chemistry correctness fix**, **OpenMP parallelization** of the
pelagic kinetics, a batch of **memory-safety fixes**, and the **`AQUABC_PELAGIC_KINETICS`
decomposition**.

### Added
- **CL29 EPA calibration & validation toolchain.** `tools/ingest_epa_observations.py`
  (+ `tools/epa_station_to_box.csv`) turns the EPA archive (~70k samples, 1984–2021) into
  tidy observations + per-station `.dates`, resolving per-era units/speciation (µg vs mg;
  dissolved-N ion-basis pre-2008 → nitrogen) from cross-era value continuity (#38).
  `tools/validate_cl29_vs_epa.py` scores a CL29 run against the observations per
  (box, variable) for 8 variables, three of them (Tot_N/Tot_P/Chl-a) reconstructed from
  the state-variable pools, with time-series plots (#39, #40). The CL29 run was **extended
  from 2012–2016 to the full 2012–2022 EUTROPY forcing record**, ~6×-ing the EPA overlap
  (#41). `docs/CL29_EPA_Calibration_Summary.md` documents the whole arc (#44).
- **OpenMP parallelization** of `AQUABC_PELAGIC_KINETICS` and the CO2SYS call (TODO 4.1–4.4):
  a micro-benchmark harness + `docs/OPENMP_PERFORMANCE.md`, CO2SYS chunked across threads
  (8-thread speed-up 2.84× → 6.55× at nkn=1000), an empty-chunk barrier-deadlock fix, and
  thread-affinity guidance. Build with `make OPENMP=1 build-estas`.
- **macOS/gfortran CI** entry in the `build-and-run` matrix, with a `--numeric-warn`
  cross-platform tolerance for the 0D golden (TODO 3.1, #35).
- **CO2SYS Fortran test coverage** — `tests/fortran/test_co2sys.f90` (13 round-trip /
  invariant / PyCO2SYS-validated checks) (#34).
- **`run_cl29.sh`** wrapper that bakes in `ESTAS_HOLD_VOLUME=1`, fixing the CL29 day-~449
  negative-mass volume crash (EUTROPY flows are not per-box volume-conserving) (#26).

### Changed
- **CL29 nitrogen calibration** — water-column denitrification `K_MIN_DOC_NO3N_20`
  0.025 → 1.0 (converter override), correcting a ~2× NO3/TN over-prediction versus EPA
  (NO3 bias +0.31 → +0.06). Mechanistically grounded — the lagoon is a documented
  strong-denitrification N sink (#42).
- **CL29 summer-P boost** `CL29_BOUNDARY_PO4_SUMMER_PEAK` 3.0 → 2.0, de-eutrophication-aware
  for the fuller 2012–2022 record (#43).
- **`AQUABC_PELAGIC_KINETICS` decomposition** (TODO 1.6) — a 3,642-line mega-subroutine
  became a 394-line orchestrator plus five `contains` procedures, byte-identical.
- **Shiny non-blocking I/O** (TODO 2.4) — the three heaviest `OUTPUT.csv` read+compute
  handlers moved off the event loop via `@reactive.extended_task` + `asyncio.to_thread` (#33).
- **Centralized configuration** — subprocess timeouts + default constants filename into
  `shiny_app/config.py` (TODO 2.6, #31); sediment write-only-variable cleanup (TODO 1.7, #32).
- **Serial solver performance** — box-geometry cache + hoisting the O(nkn²) per-box array
  zeroing to linear, byte-identical (#37).
- **Advanced-redox configurability** — runtime FePO4 solubility (`FEPO4_KSP_LOG10`) and the
  reductive Fe(III)-P coupling (`FE_P_REDOX_FRAC`, W_SED_CONST #171) (#27, #28).

### Fixed
- **RK2 solver double-applied the CHLA settling suppression (latent).** `CALC_DERIV`
  (`SOURCE_CODE/ESTAS/mod_SOLVER.f90`) applies the chlorophyll-based settling-suppression
  factor to `SETTLING_VELOCITIES` **in place**, but `UPDATE_TIME_FUNCS` (which recomputes
  the fresh velocities) runs only **once** per RK2 (Heun) step — so RK2's stage-2
  `CALC_DERIV` suppressed an already-suppressed velocity, compounding the factor. Fixed by
  preserving a pristine `SETTLING_VELOCITIES_FRESH` after `UPDATE_TIME_FUNCS` and restoring
  it before stage 2, so each stage suppresses once from the fresh base. **No live output
  changes:** the solver is hardcoded to forward-Euler (`PELAGIC_SOLVER_NO = 1`,
  `mod_SIMULATE.f90`), so RK2 is currently dead code; the Euler path is untouched and the
  0D golden is byte-identical. This is a latent-correctness fix for anyone enabling RK2.
  Found during a profiling-driven solver optimization review (see the deferred
  `PELAGIC_SOLVER` perf items in `TODO_IMPLEMENTATION_PLAN.md`).
- **CO2SYS carbonate chemistry — KB=0 bug (found by new test coverage, TODO 5.1).** The Dickson (1990) boron-constant formula in `SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90` had a **misplaced parenthesis** — `(-24.4344 - 25.085·√S - 0.2474·S)·(logTempK + 0.053105·√S·TempK)` instead of `(…)·logTempK + 0.053105·√S·TempK` per Dickson 1990. This drove `lnKB ≈ −1.7e4`, so the boric-acid dissociation constant **KB underflowed to 0**, dropping borate alkalinity (~91 µmol/kg at S=35) from the alkalinity budget and misassigning it to the carbonate system. Effect at the canonical case (S=35, T=25 °C, TA=2300, DIC=2000, K1K2=4, KSO4=1, total scale): **pH 8.21→8.045, pCO₂ 260→397 µatm, Ω_aragonite 4.72→3.39** — the corrected values now match **PyCO2SYS 1.8** to ~1e-4. This changes model output wherever CO2SYS runs (pelagic pH / CO₂ flux / calcite-aragonite saturation, and the sediment carbonate system); the **0D regression golden was regenerated** (`tests/regression/pelagic_0D_golden.csv`; worst 0D shift ~10 % in NO3N via the pH→nitrification coupling, output finite and physically sane). Surfaced by the new **`tests/fortran/test_co2sys.f90`** (13 checks: round-trip consistency, mass-balance closure, physical invariants, a borate-alkalinity regression guard, and a PyCO2SYS-validated anchor). Latent bugs in the CO2SYS input pairings the model does *not* use — `(TA,pH)`, `(pH,pCO₂)`, `(TA,pCO₂)` — are documented for a future fix (TODO 5.1).
- **Model-constants out-of-bounds write** (TODO 1.10) — a `NUM_MODEL_CONSTANTS` mismatch
  let the constants reader write past the end of the array; memory-safety fix, production
  output byte-identical (#23).
- **Advanced-redox non-determinism** (TODO 1.11) — a local `FLAGS` in `CALC_DERIV` shadowed
  the global, leaving `FIRST_TIME_STEP` / init-option flags reading stack garbage so runs
  diverged; one-line fix, now deterministic across runs (#24).
- **Nostocales `DAY_OF_YEAR` uninitialised read** — corrected the day-of-year seasonality
  input to the Nostocales module (#25).
- **Kelvin-offset single-precision loss** (TODO 1.8) — the only real magic number (273.15)
  became a named `CELSIUS_TO_KELVIN` constant, fixing a latent precision bug in
  DO-saturation / CO2SYS (~1e-6 intended output change, 0D golden regenerated) (#30).
- **Missing `IOSTAT` on input opens** (TODO 1.9) — an `OPEN_INPUT_FILE` helper guards the
  input-file opens so a missing/unreadable file gives a clean message + nonzero stop
  instead of a raw runtime crash; byte-identical when files exist (#29).

### Dependencies
- Dependabot bumps: numpy (≥2.2.6,<3.0), networkx (≥3.4.2,<4.0), pre-commit (≥4.6.0),
  pytest-cov (≥7.1.0) (#18–#21).

## [0.4.5] - 2026-07-15

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 5 (final cleanup): the rearchitecture is complete** (TODO 2.1). This closing phase removes the dead weight left after all fifteen tabs became true Shiny modules: (1) the **unread `run.build_config`** `RunController` value — a `reactive.calc` whose only consumer, the "Build & Run" handler, was removed as dead code in Phase 4 (the cross-tab contract is now the four live values `command_config`/`constants_config`/`run_executable_name`/`active_executable` plus `exe_list_version`); (2) the now-empty **`shiny_app/ui_panels.py`** stub (all three panels it once held are modules); and (3) **~70 dead imports** in `app.py` (leaf parsers, stdlib, and viz libraries orphaned as inline handlers moved into modules), after which the `F401` per-file-ignore is dropped (the structural `E402`/`F841`/`B023`/`S602`/`S605` ignores remain). Behavior is **unchanged** — pure removals, verified by `import shiny_app.app` + `create_ui().tagify()` (the test suite does not import `app.py`, so this is the real backstop), the full suite (**178** Python tests), and a boot smoke (all 15 tabs render namespaced with zero bare-id leaks).
- **End state (spec §9 success criteria met):** `server()` is a thin assembler — per-session `RunController`/`AppState` construction plus the two app-level chrome renders (`help_content`/`changelog_content`) and 15 `x_server("id", state)` calls, nothing else. **`shiny_app/app.py` is 756 lines, down from ~5,600** at the start of the rearchitecture. Fifteen cohesive `@module.ui`/`@module.server` modules (plus the converted `diagnostics`) live in `shiny_app/` behind the `RunController`/`AppState` contract; **no `input.X` crosses a module boundary** except via that contract or the documented `session.root_scope().make_scope("run_control")` bridge. The `v0.4.0`–`v0.4.5` series (Phases 0–5) was executed subagent-driven with per-task and whole-branch reviews, each release CI-verified including the Playwright/Selenium integration tests. Spec + phase plans under `docs/superpowers/{specs,plans}/2026-07-1[45]-*shiny-modules*`.

## [0.4.4] - 2026-07-15

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 4 (run/build/dashboard cluster)** (TODO 2.1): converted the **last three** inline tabs out of the `server()` closure into true namespaced `@module.ui`/`@module.server` Shiny modules — `model_build`, `run_control` (a fat tab: the Run Model + Output Config sub-tabs of `panel_model_control`), and `dashboard`. `server()` is now a thin assembler: per-session `RunController`/`AppState` construction, the two app-level chrome renders (`help_content`/`changelog_content`), and 15 `x_server("id", state)` calls — nothing else. **`app.py` drops from 2,393 to 871 lines** (from ~5,600 at the start of the rearchitecture). Behavior is **unchanged except the within-tab id namespace** — verified by a render-smoke unit test per module (`.tagify()` for the `run_control` fat tab; **177 → 178** Python tests), a `create_ui().tagify()` boot smoke (all cluster tabs render namespaced with zero bare-id leaks, default `nav_dashboard` view intact), and the Playwright/Selenium integration selectors migrated to `model_build-*`/`run_control-*`/`dashboard-*`.
- **Contract-first cross-module rewiring (the de-risk for the most-coupled cluster).** Before any id was namespaced, every value that crosses between these three modules was routed through the shared `RunController` (`run`): `run.command_config` (the assembled command **`List[str]`**), `run.build_config` (build-config dict), `run.constants_config` (the quick-run constants-validation triple), `run.run_executable_name` (the bare Run-Model executable-name string), and `run.active_executable` (the Model-Build selector) — the dashboard now reads these instead of another tab's `input.X`. This exposed and fixed a latent list-vs-name conflation (the command is a list; the executable name is a separate string), and it means **no `input.X` crosses a module boundary**.
- **`sim_output_dir`/`run_executable` cross-namespace bridge repointed via `session.root_scope().make_scope("run_control")`.** The `plot`, `sim_config`, and `model_build` modules reach the Run-Model/Output-Config widgets that `run_control` now owns; the bridge uses a module-scoped session plus a **bare** id (for both reads and writes). The naïve fully-qualified-literal form (`input["run_control-sim_output_dir"]`) is deliberately avoided because Shiny's `validate_id` rejects the hyphen and raises `ValueError` on read. A pre-execution multi-agent adversarial review of the plan (verified empirically against Shiny 1.5.1) caught this and four other issues before implementation. A preparatory sweep also dropped ~286 lines of dead run/build handlers (zero UI references) so the conversion diffs stayed clean. After this phase, only Phase 5 (final cleanup: an empty `ui_panels.py` stub, `app.py` dead leaf-imports, and one now-unread `run.build_config` registration) remains. Spec + Phase-4 plan under `docs/superpowers/{specs,plans}/2026-07-1[45]-*shiny-modules*`.

## [0.4.3] - 2026-07-15

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 3 (output cluster + dead-bus removal)** (TODO 2.1): converted the last four content tabs out of the `server()` closure into true namespaced `@module.ui`/`@module.server` Shiny modules — `mass_balance`, `observations`, and a single merged `plot` module (`shiny_app/modules/`), plus `diagnostics` upgraded in place (`shiny_app/diagnostics.py`). Each `<tab>_ui()` returns panel *content* (the `panel_conditional` stays in `create_ui()` so `input.navigation` keeps referencing the global nav input); each `<tab>_server(input, output, session, state)` is a verbatim port of the tab's handlers + reactive values; within-tab ids namespace to `<tab>-*`. Behavior is **unchanged except the id namespace** — verified by a render-smoke unit test per module (**173 → 176** Python tests), a live `create_ui()` boot smoke (all twelve converted tabs render namespaced with zero bare-id leaks, `server()` constructs clean), and the Playwright/Selenium integration selectors.
- **Plots tab merged into one `plot` module (spec §6 reversal):** rather than the originally-specced `plot` + `output_browser` split, the whole Plots tab — plotting, the former output-browser (file preview / column summary / output-dir + file selection), and the input-timeseries viewer — became a **single** `plot` module (its 18 handlers span two non-contiguous `app.py` ranges that straddled the still-inline run/dashboard block; ported byte-verbatim). The output-file selection is now consumed **entirely within** the module, so the cross-module output-selection signal was never needed.
- **Dead Phase-0 output-selection bus removed:** because no cross-module reader survives the merge, `AppState` drops from 7 fields to **4** (`run`, `navigate`, `output_config_version`, `sim_config_version`) — the `selected_output_dir`/`selected_output_file`/`selected_output_format` fields and the app-level `_publish_output_selection` effect that fed them are deleted (`test_appstate_holds_fields` updated). The `plot` module's `init_output_dirs` reaches the still-app-level sibling `sim_output_dir` input via `session.root_scope()` — the same documented, Phase-4-flagged bridge `sim_config` established.
- **`diagnostics` pseudo-module → true module:** the last panel using the old flat-id pattern (manually `diag_`-prefixed global ids, a plain `diagnostics_ui()` returning a `panel_conditional`, and `diagnostics_server(input, output, session, root_dir)`) is now a true `@module.ui`/`@module.server` module (ids namespace to `diagnostics-diag_*`); its `panel_conditional` moved to `create_ui`, the `root_dir` parameter was dropped for the module-computed project root, and its render-smoke test uses `.tagify()` (the UI wraps a `navset_card_tab`). All diagnostic logic (`_diag_state`, the polling effect, the threaded 16-check analysis, the PDF-export effects, and every render output) ports verbatim. After this phase, only the run/build/dashboard cluster remains inline in `server()` (Phase 4). Spec + Phase-3 plan under `docs/superpowers/{specs,plans}/2026-07-1[45]-*shiny-modules*`.

## [0.4.2] - 2026-07-15

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 2 (leaf modules)** (TODO 2.1): converted seven more tabs into true namespaced `@module.ui`/`@module.server` Shiny modules under `shiny_app/modules/`, applying the Phase-1 `parameters` pilot template — `model_structure`, `map`, `model_options`, `initial_conditions`, `input_files`, `scenarios`, and `sim_config`. Each `<tab>_ui()` returns the panel *content* (the `panel_conditional` stays in `create_ui()` so `input.navigation` keeps referencing the global nav input); each `<tab>_server(input, output, session, state)` is a verbatim port of the tab's handlers + reactive values; within-tab widget ids namespace to `<tab>-*` (nav ids stay global). Modules are self-contained (stdlib + the existing leaf parsers only, nothing from `app.py`). Observable behavior is **unchanged except the id namespace** — verified by a render-smoke unit test per module (**166 → 173** Python tests), a live websocket boot smoke (all seven tabs render namespaced with zero bare-id leaks, `server()` runs clean), and the Playwright/Selenium integration selectors migrated to `<tab>-*`. Two conversions were non-standard: **`sim_config`** is a partial fat-tab extraction — its `@module.ui` returns a `nav_panel` composed into `panel_model_control`'s `navset_card_tab` while the sibling sub-tabs (Run Model, Output Config) stay inline for Phase 4, and its handlers reach the still-app-level sibling `sim_output_dir` input via `session.root_scope()` (a documented, Phase-4-flagged bridge); **`map`** uses `ipyleaflet` and **`model_structure`** re-derives its diagram `www` path for the deeper `modules/` location. `app.py` drops to ~3,800 lines (from ~5,600 at the start of the rearchitecture). Spec + Phase-2 plan under `docs/superpowers/{specs,plans}/2026-07-14-*shiny-modules*`.
- **`chrome` decision (spec §6 refinement):** `help_content`/`changelog_content` stay app-level `@render.ui`s in `server()` rather than becoming a module, because their offcanvas container ids (`helpOffcanvas`/`changelogOffcanvas`) are referenced by JS (`getElementById`) and therefore cannot be namespaced. Total planned modules: 16 (not 17).

## [0.4.1] - 2026-07-14

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 1 (pilot: `parameters` Shiny module)** (TODO 2.1): the first tab converted to a true namespaced Shiny module. New `shiny_app/modules/parameters.py` provides `parameters_ui()` (`@module.ui`, returning the panel *content* only) and `parameters_server(input, output, session, state)` (`@module.server`, the 5 handlers + 3 reactive values ported verbatim from the old inline `server()` block). `create_ui()` wraps it at app level — `ui.panel_conditional("input.navigation === 'nav_parameters'", parameters_ui("parameters"))` — so the nav condition keeps referencing the **global** `navigation` input, and `server()` deletes the inline block and calls `parameters_server("parameters", state)` once. Within-tab widget ids now namespace to `parameters-*` (e.g. `parameters-param_category`); the `nav_parameters` nav id stays global. Observable behavior is **unchanged** except the id namespace (the tab's load/edit/save flow, including the dynamic `param_{id}` inputs, round-trips identically inside the module) — verified by a render-smoke unit test (`tests/python/test_parameters_module.py`, **165 → 166** tests), a live websocket boot smoke, and the Playwright/Selenium integration selectors migrated to `parameters-*`. The module is self-contained (imports only `parameter_parser` + stdlib, nothing from `app.py`) and establishes the `shiny_app/modules/` layout + `x_ui(id)`/`x_server(id, state)` pattern reused by the remaining tab conversions.
- **`state.navigate()` upgraded to a namespace-independent mechanism.** Phase 0's `ui.update_radio_buttons("navigation", …)` only works from the app-level (global) namespace; a converted module cannot reach the global `navigation` input that way. `state.navigate` is now an async callable that does `session.send_custom_message("aquabc_navigate", …)`, handled by a new `nav_script` client handler that sets the nav input and the sidebar active-link (matching a real nav click); the two dashboard goto handlers are now `async` and `await` it (`AppState.navigate` typed `Callable[[str], Awaitable[None]]`). Proven on the still-app-level goto buttons this phase; ready for the namespaced dashboard module in a later phase. Spec + Phase-1 plan under `docs/superpowers/{specs,plans}/2026-07-14-*shiny-modules*`.

## [0.4.0] - 2026-07-14

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 0 (shared-state contract)** (TODO 2.1): first phase of converting the ~5,000-line `server()` closure toward true namespaced Shiny modules. Extracts the per-session run/build state out of the `server()` closure into a new module `shiny_app/app_state.py` with two units: **`RunController`** — the run/build "session" holding the subprocess handle, the thread-appended `build_log_lines`/`run_log_lines` buffers, and the `execute_build`/`start_run`/`stop`/`is_running` methods (ported verbatim from the old `_execute_build_process`, the `on_run` worker body, and the stop handlers, modulo the documented `_box[0]`→`self.attr` renames), plus the `exe_list_version`/`active_executable`/`build_config`/`command_config` signals; and **`AppState`** — a 7-field dataclass bundling the cross-tab reactive signals (`run`, `navigate`, `selected_output_dir`/`file`/`format`, `output_config_version`, `sim_config_version`). `server()` now constructs `state = AppState(...)` once per session and every build/run/stop handler and log render delegates to `run.*`; the old closure boxes (`_execute_build_process`, `_log_lines`, `_build_log_lines`, `_model_process`/`_model_running`/`_last_run_time`/`_model_progress`, `_exe_list_version`) are removed; output-selection and config-version signals are published into `state`, and the goto handlers route through `state.navigate`. Observable behavior is **unchanged / DOM-identical** — no widget id or UI layout changed (no UI file touched), verified by the full suite plus a live websocket-session boot smoke (`server()` runs end-to-end with no traceback). The run/build engine is independently unit-testable for the first time: adds `tests/python/test_run_controller.py` (**155 → 165** Python tests). This phase publishes/registers the cross-tab bus, but its consumers switch in later phases (the `selected_*`/`command_config`/`build_config` values are written-but-not-read for now). Opens the `v0.4` series for the Shiny-modules rearchitecture. Executed via subagent-driven development (per-task TDD + review, clean whole-branch review). Spec + Phase-0 plan under `docs/superpowers/{specs,plans}/2026-07-14-*`.

## [0.3.8] - 2026-07-13

### Changed
- **Command-preview dedup:** the `status_info` panel's command-line preview re-implemented the model command-assembly logic inline (a second copy of what `shiny_app/build_commands.assemble_estas_command` already does). It now delegates to `assemble_estas_command`, removing the duplication (`shiny_app/app.py` −23 lines). Behavior is unchanged — proven identical across an exhaustive 216-combination input matrix (executable × input file × constants × binary-enabled × binary filename × shear), preserving the handler's exact input-read semantics and the `"(model defaults)"` placeholder handling.
- **Lint cleanup of the extracted modules:** the ten finalized `shiny_app/` modules extracted during the decomposition (`build_commands`, `box_network`, `output_data`, `ui_scripts`, `ui_panels`, `diagnostics`, `diagnostics_plots`, `file_locators`, `input_analysis`, `utils`) now pass a full `ruff check` — import sorting (`I001`), unused imports (`F401`), whitespace (`W29x`), and dead locals/loop vars (`F841`/`B007`) were cleared (behavior-neutral). `app.py` is intentionally left untouched (it is still mid-decomposition, and a large cosmetic diff there would churn history on a file under active refactoring). `shiny_app/ui_scripts.py` gets a scoped `W293` ignore because those blank-line whitespaces live inside the inline-JavaScript string literals, where stripping them would alter the rendered `<script>` content. No behavior change; the full test suite (155 tests) is green.

## [0.3.7] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 3, output-data cluster** (TODO 2.1): extracted seven pure output-file helpers from `server()` into a new module `shiny_app/output_data.py` — `looks_numeric`, `format_elapsed`, `get_output_folder_from_config`, `get_output_files_info`, `get_output_columns` (CSV/`.out`/`.bin` header reading via pandas), `get_output_directories`, and `get_output_files_from_dir`. The functions use a default-arg pattern (`root=ROOT`, `output_csv=OUTPUT_CSV`, `input_txt_path=INPUT_TXT_PATH` defaulting to self-computed module consts) so the 11 call sites stay argument-free while unit tests inject a `tmp_path`. `output_data.py` is stdlib + pandas only, importing `PELAGIC_BOX_COLUMNS`/`SimulationConfigFile` from the existing `utils`/`simulation_config` leaf modules and nothing from `app.py`. Observable behavior is **unchanged** (all seven bodies confirmed equal to the originals modulo the documented const→param edits; the reactive `reactive.Value`-backed CSV cache and the reactive neighbours `run_command`/`analyze_output_directory` were deliberately left in `server()`). Adds `tests/python/test_output_data.py` (7 tests pinning the parse/scan/format logic; **148 → 155** Python tests); `app.py` −161 lines. Third phase-3 cluster after the build pilot (0.3.5) and box-network (0.3.6). Spec + plan under `docs/superpowers/{specs,plans}/2026-07-13-app-py-decomposition-phase3-outputdata*.md`.

## [0.3.6] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 3, box-network cluster** (TODO 2.1): extracted the six non-reactive box-network functions from `server()` into a new module `shiny_app/box_network.py` — the three input parsers `parse_pelagic_inputs`/`parse_advective_links`/`parse_bathymetry` (now taking `inputs_dir` explicitly instead of a closed-over global) and the three Map-Display plotly figure builders `build_box_network_figure`/`build_bathymetry_figure`/`build_depths_overview` (moved verbatim). The two Map-Display render handlers now delegate to the module (7 call sites); observable behavior is **unchanged** (all six bodies confirmed character-identical to the originals; the handlers' control flow and `go.FigureWidget` wrap untouched). `box_network.py` is stdlib + plotly only and imports nothing from `app.py`. Adds `tests/python/test_box_network.py` (10 tests: parser fixtures pinning exact structures + malformed-row/missing-file paths, plus figure smoke tests; **138 → 148** Python tests); `app.py` −595 lines. Second phase-3 cluster after the build-cluster pilot (0.3.5). Spec + plan under `docs/superpowers/{specs,plans}/2026-07-13-app-py-decomposition-phase3-boxnetwork*.md`.

## [0.3.5] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 3 pilot (build cluster)** (TODO 2.1): extracted the non-reactive build/command logic out of the ~5,900-line `server()` into a new pure module `shiny_app/build_commands.py` — `assemble_estas_command` (command-line assembly + all value-defaulting), `get_available_executables`, `get_executable_info` (size/mtime + `file(1)` type), and `target_exe_name`. The four `server()` nested functions are now **thin wrappers** that resolve the reactive inputs and delegate; observable behavior is **unchanged** (reactive dependencies and defaulting preserved — the conditional `cmd_binary_filename` read is kept, and all 15 call sites are untouched). `build_commands.py` is stdlib-only and imports nothing from `app.py`. Adds `tests/python/test_build_commands.py` (15 unit tests pinning every command-assembly branch; **123 → 138** Python tests); `app.py` −76 lines. This pilot proves the "resolve-at-call-site, pure-helper" pattern for the remaining `server()` clusters (file-I/O, plot-prep, mass-balance, observations, scenarios), which stay deferred. Spec + plan under `docs/superpowers/{specs,plans}/2026-07-13-app-py-decomposition-phase3-build-pilot*.md`.

### Added
- **Release-doc auto-sync:** the release workflow now keeps the README `Latest release` marker in sync on every `vX.Y.Z` tag via `tools/sync_release_docs.sh` (idempotent; committed back to `main` with `[skip ci]`). README refreshed to the current state (123→138-test Python suite, the decomposed `shiny_app/` module layout). CHANGELOG remains manually curated and validated by the workflow.

## [0.3.4] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 2** (TODO 2.1): split the ~1,566-line `create_ui()` into three declarative UI-fragment modules — `shiny_app/ui_scripts.py` (6 inline-JS blocks), `shiny_app/ui_panels.py` (14 content panels), and `shiny_app/ui_chrome.py` (sidebar, header, css, 3 offcanvas). `create_ui()` is now a **53-line** assembler that composes the fragments; the rendered UI is **byte-identical** (verified by a full render diff — 148,580 chars unchanged). The moves are verbatim (each fragment independently confirmed byte/AST-identical); the four app-level consts a fragment needs (`COMPILERS`, `BUILD_TYPES`, `NAV_CHOICES`, `MIN_SMOOTH_WINDOW`) stay in `app.py` and are passed as arguments, so the fragment modules import nothing from `app.py` (no circular imports). Adds multi-marker render-smoke tests `tests/python/test_ui_{scripts,panels,chrome}.py` (**117 → 123** Python tests). `app.py`: **7,925 → ~6,450 lines** (−1,472 net in this change). Deferred to later phases: extracting the non-reactive logic the reactive handlers call, and the full Shiny-modules rearchitecture. Spec + plan under `docs/superpowers/{specs,plans}/2026-07-13-app-py-decomposition-phase2*.md`.

## [0.3.3] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 1** (TODO 2.1): extracted three clusters of non-reactive, module-level helpers out of the 8,616-line monolith into focused, unit-tested modules — `shiny_app/compiler_env.py` (Intel/compiler detection), `shiny_app/input_analysis.py` (input-file analysis + `INPUT_FILE_CATEGORIES`), and `shiny_app/file_locators.py` (output/box-file discovery). `app.py` re-imports them via the existing fallback pattern; `server()`/`create_ui()` and all runtime behavior are **unchanged** — the moves are verbatim (verified byte-identical) and the Playwright integration tests pass. `app.py`: **8,616 → 7,925 lines** (−691). Adds `tests/python/test_{compiler_env,input_analysis,file_locators}.py` (117 Python tests total). Deferred to later phases: splitting `create_ui()` into `ui/` fragments, extracting the non-reactive logic the reactive handlers call, and the full Shiny-modules rearchitecture. Spec + plan under `docs/superpowers/{specs,plans}/2026-07-12-app-py-decomposition*.md`.

## [0.3.2] - 2026-07-12

### Added
- **End-to-end 0D regression test** (TODO 5.2): a golden-file check on the `AQUABC_PELAGIC_0D` example. `tests/regression/compare_0D.py` diffs a fresh `OUTPUT.csv` against a committed golden (`tests/regression/pelagic_0D_golden.csv`, the current-code output downsampled across the full 1096-day run) with an **exact header check** (catches column reorder/rename — the "wrong-column" bug class) plus a per-cell `rtol/atol` numeric check. Wired into the `build-and-run` CI job (`--rtol 1e-6`); the comparison logic is unit-tested in `tests/python/test_e2e_regression.py`.

### Changed
- **CI compiler/platform matrix** (TODO 3.1): the `build-and-run` job now runs under a `strategy.matrix` (`fail-fast: false`, compiler selected via job-level `env: FC`, honored by the Makefile's `ifeq ($(origin FC),default)`). gfortran/ubuntu-latest is active, with documented, ready-to-enable rows for `ifx` (Intel oneAPI) and `macos-latest`.

### Fixed
- **`tests/python/test_safe_resolve.py` collection failure:** it imported `safe_resolve` from `shiny_app.app`, which imports pandas — and the installed pandas (compiled against NumPy 1.x) crashes the import under NumPy 2.x. `safe_resolve` is a pure `os.path` helper, so it was moved to a stdlib-only `shiny_app/safe_resolve.py` (`app.py` re-exports it, backward-compatible). The full Python test suite now runs with **no exclusion (99 → 107 tests)**.

## [0.3.1] - 2026-07-12

### Added
- **CL29 wind-modulated diatom settling (#3):** `SETTLING_VELOCITY_TS_1.txt` (DIA_C) is now a daily time series `w_eff = w0/(1+(U/U_c)²)` derived from ERA5 daily wind (2012–2016), replacing the constant `CL29_DIATOM_SETTLING`. Parameters pinned to `w0 = 0.3` m/day and `U_c = 4.21` m/s (half-suppression wind, near the fine-sediment resuspension threshold). Converter-only — ESTAS already reads settling as a time series. Off-switch `CL29_WIND_RESUSPENSION = False` (or an absent wind file) restores the byte-identical constant. Ships the committed daily-wind artifact `tools/eutropy_poc/net/wind_daily.csv` (ERA5/Copernicus, attributed) and its regenerator `tools/eutropy_poc/make_wind_daily.py`. Same-binary validation shows the change reproduces the constant-0.1 behavior almost exactly (5-yr domain spring-diatom peak +0 %, summer cyano +1 %, 0 NaN): because the Nida wind is aseasonal, this is a mechanistic-defensibility change, not a change to the bloom.
- **CL29 provisional sediment-facies strawman (#5):** inert `CL29_SEDIMENT_TYPE_PROVISIONAL` (low-confidence salinity/depth classification for the 29 boxes); the active `CL29_SEDIMENT_TYPE` stays empty (CL29 byte-identical) pending an authoritative facies map.

### Changed
- **CL29 summer boundary-P correction enabled (`CL29_BOUNDARY_PO4_SUMMER_PEAK = 3`, was `1.0`/off):** a day-of-year Gaussian multiplier lifts mid-summer boundary PO4 (~0.018 → 0.034 mg/L) into the observed 2012–2016 Curonian summer range (avg ~0.025 mg/L; Aleksandrov 2025), correcting a documented ~3–15× summer-P under-supply. A `{1, 3, 5}` 5-yr sweep confirmed raising summer P is beneficial — summer cyano (CYN 1.25 → 1.34 mgC/L) and the spring diatom peak (1.35 → 1.44 mgC/L) rise modestly, succession stays intact, 0 NaN, no diatom/OPA summer takeover; an earlier "summer-P crashes cyano" report was a wrong-column analysis artifact. This is a forcing-realism change, **not** the (settling-fixed) spring-diatom gap. Setting `= 1.0` restores the byte-identical prior baseline.

### Fixed
- Removed 15 unguarded per-timestep debug `write(6,*)` statements in the pelagic light-limitation path (`aquabc_II_pelagic_lib_{DIATOMS,CYANOBACTERIA,FIX_CYANOBACTERIA}.f90`, `smith == 1` branch) that emitted ~19 GB of stdout over a 5-yr CL29 run. The `LIM_LIGHT` computation is unchanged; no numerical effect.

## [0.3.0] - 2026-07-10

### Added
- **Curonian Lagoon 29-box application (CL29):** an EUTROPY-derived 29-basin production input set generated by `tools/eutropy_poc/eutropy_to_estas.py` (forcing, boundaries, and initial conditions mapped onto the Curonian Lagoon box topology; opt-in sediment diagenesis)
- **Two-type (sandy/muddy) sediment profiles:** `SED_TYPEMAP::ASSIGN_SED_PROFILES_TO_BOXES` (`SOURCE_CODE/ESTAS/mod_SED_TYPEMAP.f90`) maps per-type geometry/IC profiles onto per-box arrays, with unit test `tests/fortran/test_sed_typemap`
- **Reductive Fe(III)–P coupling** as sediment constant `FE_P_REDOX_FRAC` (`W_SED_CONST` #171): scales PO₄ solid sorption by `(1−f) + f·MULT_FE_III_PART`, releasing bound P as Fe(III) is reduced under anoxia; **off (0.0) by default**. Converter knob `CL29_SED_FE_P_REDOX_FRAC`
- **`CL29_SED_ADVANCED_REDOX` converter flag:** enables the sediment anoxic Mn/Fe/SO₄ DON/DOP mineralization pathways for the CL29 application
- **Per-box benthic flux output:** `SEDIMENT_FLUX_OUTPUTS.out` now writes each box's own sediment→water fluxes (previously emitted one box's fluxes for every box)
- **Diagnostics panel** in the Shiny app (`shiny_app/diagnostics.py`, `diagnostics_plots.py`): live process-rate and state-variable diagnostics driven by a central reactive poll, integrated into the main UI
- **"Scientific Observatory" UI redesign**: custom dark theme via external CSS (`shiny_app/www/aquabc.css`), shinyswatch removed, dashboard restructured with a horizontal status bar (5/7 layout), full-width Parameters panel, and a light/dark theme switcher
- **In-app getting-started tutorial** (step-by-step UI walkthrough opening in a separate window), an automated tutorial runner (`tools/run_tutorial.py`), and Playwright E2E tests that follow the tutorial steps
- **Process-rate analysis toolkit** (`tools/deep_process_rate_analysis.py`, `deep_state_vs_process_crosscheck.py`, `process_rate_slot_map.py`, `analyse_process_rates.py`, `aquabc_analysis_utils.py`): 16-check mass-balance and state-vs-process cross-check analysis over model output. Added 59 lines to `sub_WRITE_PELAGIC_OUTPUT.f90` to emit the required per-process rate output
- 365-day and 3,560-day (10-year) deep-process analysis runs and reports
- Analysis, cross-validation, and reference documentation as PDFs under `docs/`: process-rate and Fixes & Results reports, code-vs-paper cross-validation (Ertürk et al. 2023), AQUABC constant reference, and the ESTAS-AQUABC Integration Guide; plus the "Scientific Observatory" UI design/implementation plan under `docs/plans/`
- **Bioturbation module** (`aquabc_II_sediment_bioturbation.f90`): depth-dependent biodiffusion, oxygen-dependent scaling, seasonal modulation, bioirrigation enhancement, and zero-flux lower boundary condition (Boudreau 1997; Soetaert et al. 1996)
- Dynamic particle mixing coefficients (`switch_partmixing = 1`): biodiffusion recomputed every sub-timestep from local depth, O₂, and day-of-year
- Bioirrigation enhancement of porewater diffusion for dissolved-phase species
- 23 unit tests for all bioturbation functions (depth attenuation, O₂ scaling, seasonality, effective Db, bioirrigation, array application, last-layer BC)
- Integration test input (`INPUT_sediment_test.txt`) for running the coupled model with full sediment diagenesis
- AQUABC and ESTAS PDF reference manuals (`docs/AQUABC_Reference_Manual.pdf`, `docs/ESTAS_Reference_Manual.pdf`)
- Makefile target `make build-docs` for PDF generation via pandoc
- Unit tests for FIX_CYANOBACTERIA, OTHER_PLANKTONIC_ALGAE, and NOSTOCALES kinetics subroutines (25 test programs, 196 assertions total)
- Dependabot configuration (`.github/dependabot.yml`) for weekly pip and GitHub Actions dependency scanning
- `requirements-dev.txt` for development/test Python dependencies (ruff, pytest)
- Comprehensive AQUABC model equations reference document (`AQUABC_Model_Equations.md`)
- `pyproject.toml` with ruff linter configuration (E, F, W, I, UP, B, S rules)
- 46 Python unit tests for parsers (`parameter_parser`, `ic_parser`, `options_parser`, `simulation_config`) and `safe_resolve`
- 19 Playwright integration tests covering app startup, navigation, and all major panels
- 9 Selenium integration tests (gracefully skip without chromedriver)
- CI `python-lint-test` job running ruff and pytest in parallel with Fortran build
- `CONTRIBUTING.md` with build/test/PR workflow documentation
- Makefile convenience targets: `make test-all`, `make test-python`, `make test-fortran`, `make lint`

### Changed
- Zooplankton boundary conditions and initial-condition sets updated per the 3,560-day analysis recommendations
- Sediment model particle mixing now uses bioturbation physics (exponential depth decay × Monod O₂ × seasonal) instead of uniform constant
- Last-layer particle mixing boundary condition changed from hard-coded zero to proper zero-flux (Neumann) BC
- Updated `AQUABC_Model_Equations.md` with bioturbation/bioirrigation equations (§13.8) and references
- Cardinal Temperature Model with Inflection (CTMI, Rosso et al. 1993) replacing piecewise-exponential temperature response for phytoplankton growth
- Synthesizing Unit nutrient colimitation (Saito et al. 2008) replacing Liebig's Law of the Minimum for all phytoplankton groups
- Tunable Platt-style photoinhibition (BETA parameter) for light limitation in all phytoplankton groups
- Unified precision type definitions via `precision_kinds` module
- Compiler warning flags (`-Wall -Wextra`) for release and fast builds
- OpenMP parallelization of pelagic kinetics nkn loop: single parallel region with chunked array slicing, ~750 line-level changes in `aquabc_II_pelagic_model.f90`, all library subroutine calls use per-thread derived type bundles, debug calls guarded with `!$omp master` barriers, serial fallback via Fortran sentinel comments
- Bundled scalar constant arguments into derived types (`t_diatom_params`, `t_cyn_params`, `t_opa_params`, etc.) for 6 kinetics subroutines
- Bundled environmental input arrays into `t_phyto_env` derived type in 7 phytoplankton subroutines
- Bundled ORGANIC_CARBON_MINERALIZATION I/O arrays into 3 new shared pointer types (`t_redox_state`, `t_redox_lim`, `t_docmin_outputs`), reducing arguments from 36 to 9
- Bundled REDOX_AND_SPECIATION I/O arrays into shared pointer types, reducing arguments from 33 to 12
- Removed 5 dead arguments (`K_NO3_RED`, `K_MN_IV_RED`, `K_FE_III_RED`, `K_S_PLUS_6_RED`, `K_DOC_RED`) from ORGANIC_CARBON_MINERALIZATION
- Replaced hardcoded dimension magic numbers with named constants throughout
- Replaced tabs with spaces in 15 source files for consistent formatting

### Fixed
- **FIX_CYN photosynthetic O₂ production omitted (HIGH):** when both `DO_NOSTOCALES` and `DO_NON_OBLIGATORY_FIXERS` were enabled, nitrogen-fixing cyanobacteria O₂ production was dropped from the dissolved-oxygen derivative (slot 5 held Nostocales only). Repurposed rate slot 19 to carry FIX_CYN O₂ production. **Changes simulated dissolved-oxygen results.**
- **DON-uptake N:C ratio (MEDIUM):** FIX_CYN dissolved-organic-nitrogen uptake used `CYN_N_TO_C` instead of `FIX_CYN_N_TO_C` (copy-paste error), producing a nitrogen mass imbalance. **Changes simulated nitrogen pools.**
- **Light-limitation negativity:** the Steele/Platt formula produced tiny negatives (~−4e-4) at dusk when surface irradiance approaches zero; now clamped to [0,1] in all 8 code paths (central `LIM_LIGHT` plus inline Steele in DIA/OPA/CYN/FIX_CYN/NOST)
- Detritus C:N ratio correction
- Corrected model input values and initial-condition sets from long-run value analysis and code-vs-paper cross-validation; regenerated forcing time-series
- Shiny plot panel mis-parsing `PROCESS_RATES` output files
- Diagnostics panel reliability: thread-safe mutable dict instead of `reactive.Value`, and `suspend_when_hidden=False` + central poll so inactive-tab outputs refresh
- WCONST parser fix (added units column and value justifications to the constant-reference PDF)
- **Missing `isedi` parameter**: registered `isedi = 0` in ESTAS `INIT_BSED_MODEL_CONSTANTS` — was expected from SHYFEM parameter system but never provided in standalone ESTAS
- **Sediment flux output format**: corrected format descriptor from `33F20.10` to `36F20.10` to match actual `FLUXES_OUTPUT_TO_WATER_COLUMN` array size (`nstate + NUM_ALLOLOPATHY_STATE_VARS = 36`)
- **`FE_III_DISS` read uninitialized (correctness):** the unsaturated (`elsewhere`) branch of the Fe(III) dissolution `where` block in `AQUABC_SEDIMENT_MODEL_1` left `FE_III_DISS` unset before it was read in `SED_DOC_MINERALIZATION` (`LIM_FE_III_RED`), so advanced-redox sediment results depended on stack garbage (non-deterministic across builds). Now assigned in both branches; verified via `-finit-real=snan` (no trap) and `-O2 == -finit-local-zero` (bit-identical)
- **Sediment advanced-redox metal-dissolution NaN crash:** dissolved Fe/Mn fractions could blow up toward a near-zero metal pool and self-propagate via saved outputs; clamped to finite `[0,1]` with an `IEEE_IS_FINITE` guard in `CALC_DISS_ME_CONC`
- **Sediment→water flux buffer not zeroed:** stale allelopathy-tail values (cols 33–36) corrupted secondary-metabolite derivatives; now zero `FLUXES_TO_WATER_COLUMN` before the sediment-to-water mapping
- Fixed all 653 ruff lint warnings across `shiny_app/*.py` (whitespace, imports, type annotations, bare excepts)
- Fixed 3 zero-valued Monod half-saturation defaults (`K_HS_DOC_MIN_DOXY=0→1.0`, `K_HS_DON_MIN_DOXY=0→0.05`, `K_HS_DOP_MIN_DOXY=0→0.052`) that caused NaN when substrate concentration reached zero
- 25 Fortran unit tests (196 assertions) covering kinetics subroutines (DIATOMS, CYANOBACTERIA, FIX_CYANOBACTERIA, OTHER_PLANKTONIC_ALGAE, NOSTOCALES, ZOOPLANKTON, REDOX_AND_SPECIATION, ORGANIC_CARBON_MINERALIZATION) plus utility subroutines
- Shared test defaults module (`test_defaults.f90`) with realistic parameter populators for all derived types
- Guarded ~25 division-by-zero risks in sediment model (porosity, depth, mixing length, pH-to-H+ conversions)
- Guarded REDOX speciation divisions against zero/negative values in pelagic model
- Guarded CO2SYS critical divisions and discriminant
- Guarded macroalgae quota divisions against zero biomass
- Guarded state variable ratio divisions in pelagic model (Fe/Mn dissolved fractions)
- Added pH, temperature, and salinity clamping at model entry points
- Replaced raw `exp()` with `safe_exp()` in light limitation calculations to prevent overflow
- Numerical hardening: zooplankton switching, parameter validation, cyanobacteria cleanup
- Fixed 13 critical ecological model bugs across pelagic, sediment, and macroalgae
- Fixed Mn speciation, leap year, bathymetry initialization, and NaN guards
- Fixed settling suppression bug, allocation error checks, and getpar precision
- Added `implicit none` to all remaining subroutines and functions
- Removed 138 unused variable declarations across 25 source files
- Removed 5 unused parameter imports and declarations
- Resolved compare-reals and real-to-integer conversion warnings
- Initialized variables that may be used uninitialized
- Resolved character truncation warning in WRITE_PELAGIC_MODEL_CONSTANTS
- Fixed missing `precision_kinds` dependency in 6 test targets
- Removed ~250 lines of dead/commented-out code across 9 source files
- Fixed `shinywidgets` version pin (0.7.0 → 0.7.1) in `requirements.txt`

### Security
- Added `safe_resolve()` path traversal protection to all Shiny app file operations (`load_file`, `save_file`, `file_info_panel`, `validate_constants_file`, observation handlers)
- Added subprocess timeouts (120s clean, 600s build) with `kill()` on timeout to prevent hung processes
- Added bounded output buffers to all subprocess stdout readers

---

## [0.2.1] - 2026-01-22

### Added
- Pop-up tooltips throughout the entire interface for all major controls
- Changelog widget in the app header bar
- Output directory now saved/loaded with simulation configuration
- Two-column layout for output boxes selection
- CI integration with ftnchek static analysis for Fortran code quality
- Fortran unit tests for VECTOR_MATRIX_UTILS module

### Changed
- Build Options button moved inside Run Parameters card (cleaner layout)
- Output Directory moved from Simulation Config to Output Config tab
- Reduced vertical spacing in Run Parameters card for compactness
- Reduced text size in run log display
- Increased run log buffer from 50KB to 200KB
- Sediment Model disabled by default
- WCONST_04.txt set as visible default for Pelagic Constants File

### Fixed
- Intel Fortran (ifx) compiler linking with runtime libraries (-lifcore -lifport)
- Allelopathy kinetics: added missing concentration term for first-order degradation
- Allelopathy kinetics: initialize SEC_METAB state variables (33-36) before rate calculations
- Removed redeclaration of S_SEC_METAB_* variables to avoid module conflicts

---

## [0.2.0] - 2026-01-21

### Added
- Active Switching Model for zooplankton food selection
  - Implements Gentleman et al. (2003) switching formulation
  - Configurable via MODEL_SWITCHES.txt (K_ZOO_SWITCH parameter)
  - Documentation in docs/ZOOPLANKTON_SWITCHING_MODEL.md
- Multi-compiler build support (gfortran, ifort, ifx)
- Shiny frontend for model configuration and execution
  - Dashboard with quick run controls
  - Model build panel with compiler selection
  - Simulation configuration with presets
  - Parameter editor with categories
  - Initial conditions editor
  - Model options and switches editor
  - Scenario presets system
  - Results visualization with dual-axis plots
  - Mass balance calculations
  - Model validation with observations

### Changed
- Upgraded from AQUABC 0.1 to 0.2 with improved kinetics
- Reorganized source code structure

### Fixed
- Various numerical stability improvements

---

## [0.1.0] - 2025-01-01

### Added
- Initial release of AQUABC biogeochemical model
- Basic pelagic kinetics for:
  - Phytoplankton (Diatoms, Cyanobacteria, Other Phytoplankton)
  - Zooplankton
  - Dissolved oxygen
  - Nutrients (N, P, Si, Fe)
  - Organic matter (DOC, POC, DON, PON, DOP, POP)
- Bottom sediment diagenesis model (optional)
- Fortran 90 implementation

---

*This changelog is automatically displayed in the AQUABC Shiny app header.*
