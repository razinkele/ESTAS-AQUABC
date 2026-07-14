# AQUABC Changelog

All notable changes to the AQUABC model and Shiny frontend are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

---

## [Unreleased]

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
