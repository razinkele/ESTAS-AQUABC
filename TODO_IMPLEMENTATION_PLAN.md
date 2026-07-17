# TODO Implementation Plan — AQUABC v0.3

**Created:** 2026-02-13
**Based on:** Deep audit of Fortran code, Python/Shiny app, and CI/build/test infrastructure

---

## Priority Legend

| Priority | Meaning |
|----------|---------|
| P0 | Critical — correctness/safety bug, fix ASAP |
| P1 | High — significant quality/maintainability issue |
| P2 | Medium — improvement that reduces tech debt |
| P3 | Low — nice-to-have, do when convenient |

---

## 1. Fortran Code Quality

### 1.1 [P0] Memory Leaks in ALLELOPATHY Module

> **Status:** ✅ COMPLETED (Sprint 1, 2026-02-14) — No fix needed — no leak (`mod_ALLELOPATHY.f90` alloc/dealloc correct)

**File:** `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_ALLELOPATHY.f90`

**Problem:** 44 local allocatable arrays are allocated at subroutine entry but never deallocated. Every call leaks memory. Over a long simulation with thousands of timesteps × spatial nodes, this accumulates into significant memory bloat.

**Fix:**
- Add `deallocate(...)` for all 44 arrays before each `return` and at subroutine end
- Alternatively, convert to automatic (stack) arrays since `nkn` is known at entry: `real(dp) :: ALLEL_C(nkn)` instead of `allocatable`

**Effort:** ~1 hour

---

### 1.2 [P0] Unguarded K_E Division — EUPHOTIC_DEPTH

> **Status:** ✅ COMPLETED (Sprint 1, 2026-02-14) — Fixed — `K_E` guarded in CYANOBACTERIA / FIX_CYANOBACTERIA / NOSTOCALES

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Problem:** `EUPHOTIC_DEPTH = 4.61D0 / K_E` — if K_E is zero (e.g., no particles, no background extinction), this produces Inf/NaN that propagates through light limitation.

**Fix:**
```fortran
EUPHOTIC_DEPTH(ns:ne) = 4.61D0 / max(K_E(ns:ne), 1.0D-20)
```

**Effort:** 5 minutes

---

### 1.3 [P0] SAVE Variables — Thread Safety Risk

> **Status:** ✅ COMPLETED (Sprint 1, 2026-02-14) — Documented — SAVE vars audited; no active race under current OpenMP usage

**Files:**
- `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90` — 6 arrays with `SAVE` attribute (allocated on first call)
- `SOURCE_CODE/UTILS/string_utils.f90` — `SAVE` variable in string formatting

**Problem:** `SAVE` variables persist across calls and are shared across threads. If the pelagic interface is ever called from within the OpenMP region (currently it is not, but future changes could introduce this), data races would occur.

**Fix:**
- For pelagic_interface: Move SAVE arrays to module scope with explicit initialization, or allocate once at simulation start
- For string_utils: Make the SAVE buffer `threadprivate` or use a local buffer
- Add comments documenting thread-safety assumptions

**Effort:** ~2 hours

---

### 1.4 [P1] CO2SYS Exponential Overflow Risks

> **Status:** ✅ COMPLETED (Sprint 2, 2026-02-14) — Fixed — 8 `exp()` calls wrapped with `safe_exp` in `aquabc_II_co2sys.f90`

**File:** `SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90`

**Problem:** Several raw `exp()` calls with unbounded arguments (e.g., `exp(-pH * log(10))` for extreme pH values). While pH is now clamped at model entry, CO2SYS has its own internal calculations that could produce extreme arguments.

**Fix:**
- Add `safe_exp()` calls (already defined in `aquabc_II_pelagic_model_constants.f90`) to the ~5 vulnerable `exp()` calls in CO2SYS
- Or clamp arguments: `exp(max(min(arg, 700.0D0), -700.0D0))`

**Effort:** ~30 minutes

---

### 1.5 [P1] Remaining Division-by-Zero Risks in Pelagic Model --- COMPLETED 2026-02-14

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Audit complete.** Systematic review of all ~80 division operations in `AQUABC_PELAGIC_KINETICS`. One missing guard was found and fixed:

**Fix applied:** Added `elsewhere(FE_III .lt. 1.0D-20)` guard to the Fe3+ first-timestep initialization (case 1), matching the existing Fe2+ pattern. All other divisions confirmed safe:

| Category | Count | Protection |
|----------|-------|------------|
| Constant divisors (molar masses, unit conversions) | ~20 | `S_MOLAR_MASS_MG`, `FE_MOLAR_MASS_MG`, `12000.0D0`, `14007.0D0`, `30974.0D0`, `14.D0`, `31.D0`, `28.0855D0`, `30.9737D0`, `1.0D4`, `1.0D6` |
| C-to-CHLA ratios | 5 | `DIA_C_TO_CHLA`, `CYN_C_TO_CHLA`, etc. (model constants, always > 0) |
| Monod/half-saturation kinetics | ~30 | Form `X / (X + K_HS)` where K_HS > 0, so denominator > 0 |
| Fe2+ fractions (first timestep + subsequent) | 2 | `where(FE_II .lt. 1.0D-20)` guard |
| Fe3+ fractions (subsequent timestep) | 1 | `where(FE_III .lt. 1.0D-20)` guard |
| Fe3+ fractions (first timestep) | 1 | **Fixed:** added `elsewhere(FE_III .lt. 1.0D-20)` guard |
| Mn2+ fractions | 1 | `where(MN_II .lt. 1.0D-20)` guard |
| Saved outputs (FE_II, FE_III) | 2 | `where(FE_II/FE_III .lt. 1.0D-20)` guard |
| H2S speciation | 2 | `H2S_DIVISOR = H+ ^2 + H+*K1 + K1*K2` (sum of positive terms, always > 0 for valid pH) |
| Phosphate speciation | 3 | `FRACTION_DIVISOR_TIP = H+^3 + K1*H+^2 + K1*K2*H+ + K1*K2*K3` (sum of positive terms) |
| Temperature-dependent constants | ~6 | `T_A = TEMP + 273.15` (always > 0 for liquid water) |
| Zoo/detritus N:C and P:C | 4 | `max(divisor, MIN_CONCENTRATION)` guard |
| NH4 preference fractions | 2 | `max(denominator, 1.0D-10)` guard |
| N:P molar ratio | 1 | `where(PO4_P .lt. 1.0D-10)` conditional guard |
| FRAC_NH3 | 1 | `1.0 / (1.0 + 10^(...))` — denominator always >= 1.0 |
| FRAC_FIX_N_FOR_GR_VEG_HET | 2 | Model constant (default 0.65, user-provided, must be > 0) |
| Rate limiters (allowed_rate/total_removal) | 1 | Guarded by `if (total_removal > allowed_rate)` |
| OpenMP chunk_size | 1 | Integer division `(nkn + nthreads - 1) / nthreads`, always > 0 |

**Effort:** ~1 hour (as estimated)

---

### 1.6 [P2] Mega-Subroutine Decomposition

**Status:** ✅ COMPLETE 2026-07-16. `AQUABC_PELAGIC_KINETICS` decomposed into a thin
orchestrator + **five internal `contains` procedures** via pure code motion:
`pelagic_co2sys_preprocess`, `pelagic_speciation_preprocess`, `pelagic_biology`,
`pelagic_chemistry`, `pelagic_derivatives`. Shared `(nkn)` arrays reached by host
association; per-thread private data (bundles/scalars) passed as arguments (the
OpenMP correctness rule); the `if (nkn_local > 0)` guard + the two straddling `if`
constructs kept whole in the orchestrator. **Byte-for-byte identical output** —
gated after every extraction by `tools/refactor_verify.sh` (default all-box config,
serial + OMP=8 bit-identical over 52 files + 0D golden). Adversarially reviewed
plan (Workflow, 11 findings fixed) → subagent-driven execution (5 extraction tasks,
each independently reviewed byte-identical). Spec/plan:
`docs/superpowers/specs|plans/2026-07-15-pelagic-kinetics-decomposition*`.

**Note:** the advanced-redox verify config surfaced two pre-existing model bugs
(TODO 1.10 constants OOB, 1.11 advanced-redox uninitialised memory) — filed
separately; the decomposition proceeded on the deterministic default-only gate.

**Effort:** ~1–2 days (as estimated)

---

### 1.7 [P2] Sediment Model Variable Declarations

**File:** `SOURCE_CODE/AQUABC/SEDIMENTS/aquabc_II_sediment_model_1_fast.f90`

**Problem:** ~315 variable declarations at the top of the main sediment subroutine. Many may be unused after previous cleanups.

**Fix:**
- Run unused variable detection: `gfortran -Wunused-variable`
- Remove confirmed unused declarations
- Consider grouping related variables into derived types (similar to Phase 2 pelagic refactoring)

**Effort:** ~2–4 hours

---

### 1.8 [P3] Magic Numbers in Physics Constants

**Files:** Various, especially `aquabc_II_co2sys.f90`, sediment model

**Problem:** Scattered numeric literals (e.g., `273.15`, `1013.25`, `8.314`) without named constants.

**Fix:** Define named constants in `aquabc_II_pelagic_model_constants.f90`:
```fortran
real(dp), parameter :: KELVIN_OFFSET = 273.15D0
real(dp), parameter :: STD_ATM_MBAR = 1013.25D0
real(dp), parameter :: GAS_CONST_R = 8.314D0
```

**Effort:** ~1 hour

---

### 1.9 [P3] Missing IOSTAT on File READ Operations

**Files:** Various utility and I/O routines

**Problem:** Some `READ` statements lack `IOSTAT=` error handling, which can cause crashes on malformed input files.

**Fix:** Add `IOSTAT=ios` and error checking to all file I/O operations.

**Effort:** ~1–2 hours

---

### 1.10 [P1] Model-constants array out-of-bounds (NUM_MODEL_CONSTANTS mismatch)

**Files:** `SOURCE_CODE/ESTAS/mod_GLOBAL.f90:20` (`nconst = 318`),
`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90:75` (`nconst = 318`),
`INPUTS/PELAGIC_INPUTS.txt:9` + `INPUTS_CL29/PELAGIC_INPUTS.txt:9` (`NUM_MODEL_CONSTANTS = 318`),
data files `INPUTS/WCONST_04.txt` (323 constants).

**Status:** ✅ COMPLETE 2026-07-17 (found 2026-07-16 during TODO 1.6). A **pure
memory-safety fix** — production output is byte-identical.

**Problem:** `WCONST_04.txt` contains **323** model constants but `nconst` (and the
input `NUM_MODEL_CONSTANTS`) were declared **318**. `READ_MODEL_CONSTANTS`
(`mod_UTILS_01.f90`) does `MODEL_CONSTANTS(CONSTANT_NO) = value` for every file line,
so constants 319–323 (the `BETA_*` photoinhibition params) are written **out of
bounds** of the 318-element array — an OOB *write* (undefined behavior, flagged by
`-fcheck=all`: "Index 319 … above upper bound of 318").

**Corrected diagnosis (adversarial plan review + verification):** the initial
"garbage `BETA_*` distorts production output" framing was WRONG. The ESTAS/production
constant-unpacking (`mod_PELAGIC_ECOLOGY.f90` `INIT_PELAGIC_MODEL_CONSTANTS`) stops at
index 318 and **never reads `BETA_*` from 319–323** (only the 0D-path routine in
`aquabc_II_pelagic_model_constants.f90` does). So `BETA_*` were never consumed from
the OOB slots on production — they hold their static-zero `0.0`. Verified empirically:
the fix leaves the default run **byte-for-byte identical** (0/52 files).

**Fix:** `nconst 318→323` in `mod_GLOBAL.f90:20` + `aquabc_II_pelagic_interface.f90:75`,
and `NUM_MODEL_CONSTANTS 318→323` in the input configs **and the generator**
`tools/eutropy_poc/eutropy_to_estas.py:595` (else a regenerated CL29 config reverts).
`WCONST_04.txt` unchanged. Verified: byte-identical (default serial+omp8 gate + 0D
golden), `-fcheck` OOB gone, full-year run stable + deterministic. NO scientific
sign-off needed (no output change).

**Separate future observation (NOT fixed here):** `BETA_*` photoinhibition is not
wired into the ESTAS path at all — harmless today since `BETA=0` is the intended
default. Spec/plan: `docs/superpowers/*/2026-07-16-model-constants-oob-fix*`.

**Effort:** ~1 hour (as estimated).

---

### 1.11 [P1] Advanced-redox uninitialised-memory non-determinism

**Files:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_internal.f90` (working-array
allocation block ~866–1114), `SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90:197`
(`PROCESS_RATES` allocation), and — residual, unlocated — likely an uninitialised
local in the redox **library** routines (`ORGANIC_CARBON_MINERALIZATION` /
`REDOX_AND_SPECIATION`).

**Status:** 🔴 OPEN (partially root-caused) — found 2026-07-16.

**Problem:** With `ADVANCED_REDOX_OPTION=1`, the model is **non-deterministic
run-to-run** (same binary, same inputs, ~50–60% of launches diverge by ~1 ULP that
amplifies through the nonlinear integration). Root cause: uninitialised heap
allocatables read-before-write in the advanced-redox path (valgrind
`--track-origins`). The default (`ADVANCED_REDOX=0`) path is deterministic — it does
not exercise these arrays. Depends on TODO 1.10 being fixed first (the constants OOB
otherwise masks/confounds this).

**Progress (uncommitted, reverted this session):** zero-initialising the 246
`internal.f90` working arrays (`source=0.0d0`) + `PROCESS_RATES`/`SAVED_OUTPUTS`/
`DERIVATIVES`/`FLUXES_*` (`mod_AQUATIC_MODEL`) reduced divergence from 100% → ~60% but
did **not** eliminate it. A residual source manifests only at long runtime (30-day)
under release optimisation and is invisible to a 1-day valgrind pass — likely a
stack local inside the redox library routines.

**Fix:** re-do the zero-inits above (they are genuine fixes), then hunt the residual
with a long-run valgrind and/or a `-finit-real=snan` + `-ffpe-trap=invalid` build
(traps uninitialised local-real reads). Dedicated multi-pass debugging effort.

**Effort:** ~1–2 days (concurrency-style uninitialised-memory hunt).

---

## 2. Python / Shiny App

### 2.1 [P1] Monolithic app.py (8,012 lines)

**File:** `shiny_app/app.py`

**Problem:** Single file contains all UI definitions, server logic, parsers, file handlers, build logic, and plotting code. Extremely difficult to navigate, test, or maintain.

**Suggested modularization:**
1. `ui/` — UI component definitions (cards, panels, layouts)
2. `server/build.py` — Build and compilation logic
3. `server/simulation.py` — Model run management
4. `server/plotting.py` — Visualization and charting
5. `server/file_handlers.py` — File I/O, parsing, validation
6. `parsers/` — Already partially extracted, complete the separation
7. `app.py` — Thin entry point importing from modules

**Risk:** Large refactor. Shiny for Python module structure needs care with reactive contexts.

**Effort:** ~2–3 days

**Status:** ✅ COMPLETE 2026-07-15 — the app.py modularization is FULLY DONE. **End state:** `server()` is a thin assembler (per-session `RunController`/`AppState` construction + 2 app-level chrome renders + 15 `x_server("id", state)` calls) over **15 namespaced `@module.ui`/`@module.server` Shiny modules** (incl. the converted `diagnostics`) behind the `RunController`/`AppState` contract; no `input.X` crosses a module boundary except via that contract or the `session.root_scope().make_scope("run_control")` bridge; **app.py 8,012 → 786 lines**; the whole `shiny_app/` + `tests/` tree is ruff-clean and CI-linted. Two efforts got here: **(A)** the DECOMPOSITION (2026-07-12/13 — pull pure/non-reactive logic into leaf modules) then **(B)** the SHINY-MODULES REARCHITECTURE (v0.4.0–v0.4.5, 2026-07-14/15 — convert `server()` itself into true namespaced modules). Both detailed below.

**(A) DECOMPOSITION:** `create_ui()` split shipped (verbatim moves, byte-identical render). PHASE 1 (2026-07-12): non-reactive helper extraction — `shiny_app/compiler_env.py` (Intel/compiler detection), `input_analysis.py` (input-file analysis + `INPUT_FILE_CATEGORIES`), `file_locators.py` (output/box discovery); app.py 8616→7925 lines. PHASE 2 (2026-07-13, tasks 1–3 of `refactor/app-py-phase2`): extracted `shiny_app/ui_scripts.py` (inline JS blocks: `reload_script`, `nav_script`, `settings_script`, `help_script`, `changelog_script`, `theme_script`), `shiny_app/ui_panels.py` (14 content-panel fragments: `panel_dashboard`, `panel_model_structure`, `panel_model_build`, `panel_model_control`, `panel_input_files`, `panel_parameters`, `panel_initial_conditions`, `panel_model_options`, `panel_sim_config`, `panel_scenarios`, `panel_plot`, `panel_mass_balance`, `panel_observations`, `panel_map`), and `shiny_app/ui_chrome.py` (sidebar/header/css/offcanvas: `build_sidebar`, `app_header`, `external_css`, `settings_offcanvas`, `help_offcanvas`, `changelog_offcanvas`). `create_ui()` is now a ~53-line thin assembler (was ~270 lines of inline UI). All fragments verified verbatim (byte-identical minus documented param substitutions); 123 python tests green (121 baseline + 2 for `ui_chrome`); F821-clean. Deferred: extract `server()` non-reactive logic, full Shiny-modules rearchitecture. Spec/plan: docs/superpowers/{specs,plans}/2026-07-12-app-py-decomposition*.md. PHASE 3 build-cluster pilot COMPLETE 2026-07-13 (`refactor/app-py-phase3-build-pilot`, tasks 1–2): extracted `shiny_app/build_commands.py` — 4 pure/unit-testable functions (`assemble_estas_command`, `get_available_executables`, `get_executable_info`, `target_exe_name`) — and rewired the 4 corresponding `server()` nested functions (`build_estas_command`, `get_available_executables`, `get_executable_info`, `get_target_exe_name`) as thin wrappers that resolve reactive `input.*()` reads (preserving the original conditional `cmd_binary_filename` read) and delegate to the module via `build_commands.<fn>(...)` (module-import form, to avoid name-shadowing/self-recursion and the `target_exe_name` collision with the existing `@render.ui` of the same name). All 15 original call sites unchanged; 137 python tests green (unchanged from Task 1 baseline, which added `tests/python/test_build_commands.py` — 11 tests; task 2 adds no new tests); F821-clean. PHASE 3 box-network cluster COMPLETE 2026-07-13 (`refactor/app-py-phase3-boxnetwork`, tasks 1–2): extracted `shiny_app/box_network.py` — 6 functions (`parse_pelagic_inputs`, `parse_advective_links`, `parse_bathymetry` — each `INPUTS_DIR` global → `inputs_dir` param — plus verbatim-body figure builders `build_box_network_figure`, `build_bathymetry_figure`, `build_depths_overview`) — and rewired the two Map-Display render handlers (`map_display_plot`, `map_display_info`) to call `box_network.<fn>(...)` (module-import form) at all 7 original call sites; deleted the 6 now-duplicate nested defs (599 lines) from `server()`. 148 python tests green (unchanged from Task 1 baseline, which added `tests/python/test_box_network.py`; task 2 adds no new tests); F821-clean on both `app.py` and `box_network.py`. Deferred: 7 pre-existing non-F821 lint smells carried over verbatim in `box_network.py` (unsorted import block, 3× unnecessary `open(..., 'r')` mode arg, unused `depths`/`bnd_id` locals, one semicolon-joined statement) — cosmetic, left untouched to keep the move byte-identical; cleanup deferred to a future lint pass. PHASE 3 output-data cluster COMPLETE 2026-07-13 (`refactor/app-py-phase3-outputdata`, tasks 1–2): extracted `shiny_app/output_data.py` — 7 pure functions (`looks_numeric`, `format_elapsed`, `get_output_folder_from_config`, `get_output_files_info`, `get_output_columns` [renamed from `_get_output_columns`], `get_output_directories`, `get_output_files_from_dir`) — and rewired all 11 corresponding `server()` call sites to `output_data.<fn>(...)` (module-import form); deleted the 7 now-duplicate nested defs from `server()` (193-line net reduction in `app.py`). 155 python tests green (unchanged from Task 1 baseline, which added `tests/python/test_output_data.py`; task 2 adds no new tests); F821-clean on both `app.py` and `output_data.py`. **(B) SHINY-MODULES REARCHITECTURE — COMPLETE, released `v0.4.0`–`v0.4.5` (2026-07-14/15, all CI-verified incl. the Playwright/Selenium integration-tests).** Converted `server()`'s ~5,600-line closure into **15 true `@module.ui`/`@module.server` modules** — `dashboard`, `model_structure`, `model_build`, `input_files`, `parameters`, `initial_conditions`, `model_options`, `scenarios`, `mass_balance`, `observations`, `map`, `diagnostics`, `sim_config`, `run_control`, `plot` — behind a shared `RunController` (run/build session carrying `command_config` [a `List[str]` argv] / `constants_config` / `run_executable_name` / `active_executable` / `exe_list_version`) + 4-field `AppState` (`run`, `navigate`, `output_config_version`, `sim_config_version`). Phases: 0 shared contract (`v0.4.0`, zero-namespacing) → 1 pilot `parameters` (`v0.4.1`) → 2 seven leaf modules (`v0.4.2`) → 3 output cluster + dead-bus removal (`v0.4.3`) → 4 run/build/dashboard cluster (`v0.4.4`; contract-first rewiring routes cross-module values through `RunController` before any id namespaces, DOM-identical; `session.root_scope().make_scope("run_control")` bridge for the shared `sim_output_dir`/`run_executable` widgets) → 5 final cleanup (`v0.4.5`: dropped unread `build_config`, empty `ui_panels.py`, ~70 dead imports; then app.py made fully ruff-clean + CI extended to lint `shiny_app/` + `ruff` pinned to `0.15.21`). **This RESOLVES all previously-deferred phase-3 items** — `mass_balance`/`observations`/`scenarios` became modules, the reactive CSV cache is now internal to the `plot` module, and `_execute_build_process` lives in `RunController`. app.py final = **786 lines**. Spec + phase plans: `docs/superpowers/{specs,plans}/2026-07-1[45]-*shiny-modules*`; each phase executed subagent-driven with per-task + whole-branch reviews (Phase 4's plan also got a Workflow adversarial pre-review that caught 13 defects).

---

### 2.2 [P1] Bare Except Blocks (5 remaining)

> **Status:** ✅ COMPLETED (Sprint 1, 2026-02-14) — Fixed — 5 bare `except` blocks replaced with specific exception types

**File:** `shiny_app/app.py`

**Problem:** 5 bare `except:` blocks catch all exceptions including `SystemExit`, `KeyboardInterrupt`, making debugging difficult and hiding real errors.

**Fix:** Replace with specific exception types:
```python
# Before:
except:
    pass

# After:
except (ValueError, FileNotFoundError, OSError) as e:
    logger.warning(f"Failed to process: {e}")
```

**Effort:** ~30 minutes

---

### 2.3 [P1] Duplicated Build/Rebuild Logic

> **Status:** ✅ COMPLETED (Sprint 3, 2026-02-14) — Done — extracted `_execute_build_process` helper (−110 duplicated lines)

**File:** `shiny_app/app.py`

**Problem:** Build and rebuild handlers share ~190 lines of nearly identical logic (compiler setup, flag assembly, subprocess management, output parsing). Bugs fixed in one copy may be missed in the other.

**Fix:** Extract shared logic into a `_execute_build(compiler, mode, openmp, clean_first)` helper function. Both handlers call it with different `clean_first` flags.

**Effort:** ~1–2 hours

---

### 2.4 [P2] Blocking I/O in Reactive Handlers

**File:** `shiny_app/app.py`

**Problem:** File reads (parameter loading, IC loading, config parsing) are synchronous and block the event loop. For large files or slow filesystems, this freezes the UI.

**Fix:** Use `@reactive.extended_task` or async file I/O for long operations. Short file reads (<1KB) can stay synchronous.

**Effort:** ~4 hours

---

### 2.5 [P2] Missing Unit Tests for Business Logic

> **Status:** ✅ COMPLETED (Sprint 3, 2026-02-14) — Done — 4 functions extracted to `utils.py`, 28 tests added

**Files:** `shiny_app/app.py` (embedded functions)

**Problem:** Business logic functions (parameter validation, unit conversions, mass balance calculations, plot data preparation) are embedded in app.py and have no unit tests. The 46 existing pytest tests cover parsers and security, not core model logic.

**Fix:**
1. Extract testable functions from app.py into utility modules
2. Write pytest tests for: validation rules, unit conversions, mass balance math, output file parsing

**Effort:** ~1 day

---

### 2.6 [P3] Hardcoded Configuration Values

**File:** `shiny_app/app.py`

**Problem:** File paths, default values, timeout durations, and UI constants are scattered as string literals throughout the code.

**Fix:** Create `config.py` with centralized configuration:
```python
class AppConfig:
    BUILD_TIMEOUT = 600
    CLEAN_TIMEOUT = 120
    RUN_LOG_BUFFER_SIZE = 200 * 1024
    DEFAULT_CONSTANTS_FILE = "WCONST_04.txt"
    ...
```

**Effort:** ~1–2 hours

---

## 3. CI / Build / Test Infrastructure

### 3.1 [P1] No Compiler Matrix in CI

**File:** `.github/workflows/ci.yml`

**Problem:** CI only tests with gfortran on Ubuntu. The project supports ifort and ifx, plus macOS. Compiler-specific bugs (especially Intel Fortran quirks) won't be caught until users report them.

**Fix:** Add a build matrix:
```yaml
strategy:
  matrix:
    os: [ubuntu-latest]
    compiler: [gfortran]
    # Future: add ifort/ifx when Intel oneAPI action is available
```

Start with gfortran-only matrix (documenting the intent to add Intel later when CI runners support it).

**Effort:** ~1 hour

**Status:** ✅ COMPLETED 2026-07-12 — `build-and-run` job converted to a `strategy.matrix` (`fail-fast: false`, `runs-on: ${{ matrix.os }}`, compiler via job-level `env: FC`). Active entry gfortran/ubuntu-latest; commented, ready-to-enable entries for `ifx` (Intel oneAPI) and `macos-latest`. The Makefile's `ifeq ($(origin FC),default)` means the exported `FC` propagates, so a new matrix row switches compilers with a one-line change.

---

### 3.2 [P1] Integration Tests Excluded from CI

> **Status:** ✅ COMPLETED (Sprint 3, 2026-02-14) — Done — Playwright integration job added to CI (19 tests)

**File:** `.github/workflows/ci.yml`

**Problem:** 19 Playwright and 9 Selenium integration tests exist but don't run in CI. They require a running Shiny app instance and browser dependencies.

**Fix:**
1. Add a CI job that installs Playwright browsers
2. Start the Shiny app in background
3. Run Playwright tests against it
4. Tear down on completion

**Effort:** ~2–4 hours

---

### 3.3 [P1] No Code Coverage Tracking

> **Status:** ✅ COMPLETED (Sprint 2, 2026-02-14) — Done — `pytest-cov` with CI coverage reporting

**Problem:** No visibility into which code paths are tested. Can't measure improvement or identify untested critical paths.

**Fix:**
- Python: Add `pytest-cov` to dev dependencies, add `--cov=shiny_app --cov-report=xml` to CI
- Fortran: Consider `gcov` integration for unit test coverage (lower priority)
- Upload reports to Codecov or similar service

**Effort:** ~2 hours (Python), ~4 hours (Fortran)

---

### 3.4 [P2] GitHub Actions Not Pinned to SHA

> **Status:** ✅ COMPLETED (Sprint 2, 2026-02-14) — Done — GitHub Actions pinned to SHA

**File:** `.github/workflows/ci.yml`

**Problem:** Actions referenced by tag (e.g., `actions/checkout@v4`) instead of SHA. A compromised action could inject malicious code into the build.

**Fix:**
```yaml
# Before:
- uses: actions/checkout@v4

# After:
- uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4.1.1
```

**Effort:** ~30 minutes

---

### 3.5 [P2] No Dependency Caching in CI

> **Status:** ✅ COMPLETED (Sprint 2, 2026-02-14) — Done — pip caching enabled in CI

**File:** `.github/workflows/ci.yml`

**Problem:** Every CI run installs Python packages and potentially rebuilds Fortran from scratch. No caching of pip packages or compiled objects.

**Fix:**
```yaml
- uses: actions/setup-python@v5
  with:
    python-version: '3.11'
    cache: 'pip'
```

**Effort:** ~30 minutes

---

### 3.6 [P2] No Pre-commit Hooks

> **Status:** ✅ COMPLETED (Sprint 3, 2026-02-14) — Done — pre-commit `ruff` + file-hygiene hooks

**Problem:** Developers can commit code that fails linting or has formatting issues. These are only caught in CI after push.

**Fix:**
1. Add `.pre-commit-config.yaml` with ruff, trailing whitespace, end-of-file fixer
2. Document in CONTRIBUTING.md: `pre-commit install`

**Effort:** ~1 hour

---

### 3.7 [P3] No Release Workflow

> **Status:** ✅ COMPLETED (2026-07-10) — Done — `.github/workflows/release.yml` + `tools/extract_release_notes.sh`

**Problem:** No automated process for creating tagged releases with changelogs and build artifacts.

**Fix:** Add `.github/workflows/release.yml` triggered on version tags:
1. Build the Fortran library
2. Run full test suite
3. Create GitHub Release with changelog excerpt and binary artifact

**Effort:** ~2–4 hours

---

## 4. OpenMP Follow-up Items

### 4.1 [P2] Performance Benchmarking

**Status:** ✅ COMPLETE 2026-07-15 — benchmarked via a micro-benchmark harness (`tools/benchmark_openmp.sh` + `SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/aquabc_II_pelagic_benchmark.f90`) that reuses the node-agnostic 0D interface to time the real `AQUABC_PELAGIC_KINETICS` `!$omp parallel` region at `OMP_NUM_THREADS=1,2,4,8` × `nkn=100/500/1000` with `omp_get_wtime()`. Results + analysis in **`docs/OPENMP_PERFORMANCE.md`**; run via `make benchmark-openmp`. Headline: speedup grows with `nkn` (negligible at nkn=100, **2.84× at nkn=1000 on 8 threads**), plateaus ~8 threads; an Amdahl fit gives **~26 % serial fraction** at nkn=1000 (the serial CO2SYS call — see 4.2). Recommend enabling OpenMP for `nkn≳500` with 2–4 threads for best efficiency; leave off for the default/CL29 small networks (<1.3×). (Intel i9-10940X, 14C/28T, gfortran 13.3.0.)

**Task:**
1. Create a benchmark script that times `AQUABC_PELAGIC_KINETICS` with `OMP_NUM_THREADS=1,2,4,8`
2. Use a representative test case with realistic `nkn` (100–1000 nodes)
3. Report wall-clock time and compute speedup/efficiency
4. Document results in `docs/OPENMP_PERFORMANCE.md`

**Effort:** ~2–4 hours

---

### 4.2 [P2] CO2SYS Parallelization

**Status:** ✅ COMPLETE 2026-07-15 — profiled (gprof: CO2SYS ~15% of kinetics, >10% gate met), then parallelized the pelagic CO2SYS call by chunking its `ntps=nkn` arrays `[ns:ne]` across threads with private output buffers (`aquabc_II_pelagic_model.f90` `RUN_CO2SYS` block; `co2sys.f90` unchanged — CO2SYS is pure/stateless). **Result: 8-thread speedup at nkn=1000 jumped from 2.84× to 6.55×** (roughly doubled at large nkn; see `docs/OPENMP_PERFORMANCE.md`). Correctness: NOT bit-identical (whole-vector Newton pH converges to its chunk's slowest element) but drift is ~1000× below the solver's `pHTol=1e-4` (0D golden `nkn=1` bit-identical + passes; full model nkn=25 1-vs-2-thread max abs diff 1e-6 = output print precision, max rel 7.8e-9). Scope: pelagic call site only (the 4 sediment CO2SYS calls are a follow-up, only active with MODEL_SEDIMENTS=1).

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Problem:** CO2SYS computation is currently sequential (before the parallel region). For large `nkn`, this could become the serial bottleneck (Amdahl's law).

**Fix:** Profile first. If CO2SYS takes >10% of kinetics time, parallelize its loop similarly to the main computation block.

**Update 2026-07-15 (from 4.1 benchmark):** now quantitatively justified — the OpenMP benchmark measured a **~26 % serial fraction** at `nkn=1000` (Amdahl fit), which caps OpenMP scaling at ~3.9× regardless of thread count. CO2SYS (`aquabc_II_co2sys.f90`, no `!$omp`, O(`nkn`) serial before the parallel region) is the prime suspect. Parallelizing its per-node loop would lift the ceiling at large `nkn`. See `docs/OPENMP_PERFORMANCE.md`.

**Effort:** ~4 hours (including profiling)

---

### 4.4 [P2] Full-model OpenMP hang at high thread counts (NEW — found during 4.2)

**Status:** ✅ COMPLETE 2026-07-15 — **empty-chunk barrier deadlock in the kinetics `!$omp parallel` region** (NOT the solver/transport path as first guessed). Root cause (found via thread-id checkpoint tracing since gdb ptrace was sandbox-blocked): the `chunk = ceil(nkn/nthreads)` split left the last thread with an **empty chunk** (`nkn_local ≤ 0`) whenever `nthreads` didn't evenly divide `nkn` (e.g. nkn=25/8thr → thread 7 gets 0 nodes); that thread skipped the region's collective `!$omp barrier`s → the other threads waited for it forever (active-spin → high CPU, no output). Only bit when `nkn < nthreads` or non-divisible, which is why nkn=1000/8 worked but nkn=25/8 hung; reproduced in the micro-benchmark. Pre-existing (Phase-4 OpenMP work, independent of 4.2 — stock code hung identically). **Fix:** balanced chunk split (`base = nkn/nthreads`, first `mod(nkn,nthreads)` threads get +1 node) + `num_threads(min(nkn, omp_get_max_threads()))` cap → every thread always gets ≥1 node. Applied to both the kinetics and CO2SYS regions. Verified: `ESTAS_II` completes at 8 threads; 0D golden bit-identical; benchmark speedup unchanged (6.4× @ nkn=1000/8); @1-vs-@8 drift ≤1e-6 (output precision). See `docs/OPENMP_PERFORMANCE.md`.

**Effort:** ~4–8 hours (concurrency debugging)

---

### 4.3 [P3] OpenMP Thread Affinity Guidance

**Status:** ✅ COMPLETE 2026-07-15 — documented in `docs/OPENMP_PERFORMANCE.md` §"Thread affinity", now with *measured* effect: on the single-socket test machine `OMP_PROC_BIND=close`/`OMP_PLACES=cores` gave **no benefit** (within run-to-run noise, marginally worse — no NUMA to optimize). Settings kept as recommended practice for multi-socket/NUMA hardware, with a note to re-measure on the target deployment machine. (Done alongside 4.1.)

**Task:** Document recommended `OMP_PROC_BIND` and `OMP_PLACES` settings for optimal cache behavior:
```bash
export OMP_PROC_BIND=close
export OMP_PLACES=cores
```

**Effort:** ~30 minutes (documentation only)

---

## 5. Testing Improvements

### 5.1 [P2] Fortran Test Coverage Expansion

**Current:** 26 test programs (0 failures, verified 2026-07-05) covering phytoplankton, zooplankton, redox/speciation, organic-carbon mineralization, iron and dissolved-metal chemistry, pH correction, ammonia chemistry, light extinction, allelopathy, sediment bioturbation, and utilities.

**Missing coverage:**
- CO2SYS (complex equilibrium chemistry — high bug risk, still untested)
- Main sediment diagenesis model (`aquabc_II_sediment_model_1_fast.f90`) — bioturbation is tested, but the solute/kinetics core is not
- End-to-end integrated pelagic + sediment run (see 5.2)

Note: ALLELOPATHY, light extinction (`light_kd`), ammonia chemistry, iron chemistry, dissolved metals, and pH correction now have dedicated test programs (`test_allelopathy`, `test_light`, `test_ammonia_chem`, `test_iron_ii`, `test_diss_me`, `test_ph_corr`) — they are no longer coverage gaps.

**Effort:** ~1 day per subroutine

---

### 5.2 [P2] End-to-End Regression Test

**Problem:** No automated test that runs the full AQUABC model and compares output against a reference solution. Unit tests verify individual subroutines but not the integrated system.

**Fix:**
1. Create a small test case (10 nodes, 10 timesteps)
2. Generate reference output with the current code
3. Add a CI job that runs the model and diffs against reference
4. Allow small floating-point tolerance (1e-10 relative)

**Effort:** ~4–8 hours

**Status:** ✅ COMPLETED 2026-07-12 — golden-file regression on the 0D pelagic example. `tests/regression/pelagic_0D_golden.csv` (current-code output downsampled every 50th row across the full 1096-day run) + `tests/regression/compare_0D.py` (stdlib tolerance diff: exact header check catches column reorder/rename, per-cell `rtol/atol` numeric check). Wired into the `build-and-run` CI job (`--rtol 1e-6`). `tests/python/test_e2e_regression.py` unit-tests the comparison logic (runs in the Python-only job) and diffs a fresh 0D output when present. Full python suite 107 passed. Note: golden is gfortran-generated; loosen tolerance / add per-compiler goldens when the matrix (3.1) gains ifx/macOS.

---

## Implementation Roadmap

### Sprint 1 — Critical Fixes (1–2 days) --- COMPLETED 2026-02-14
- [x] 1.1 ALLELOPATHY memory leaks — **No fix needed** (file `aquabc_II_pelagic_lib_ALLELOPATHY.f90` does not exist; `mod_ALLELOPATHY.f90` has proper alloc/dealloc)
- [x] 1.2 K_E division guard — **Fixed** in CYANOBACTERIA, FIX_CYANOBACTERIA, NOSTOCALES library files (not pelagic_model.f90 as originally stated)
- [x] 1.3 SAVE variable thread safety audit — **Documented** (22 vars in pelagic_interface + 3 in STRING_UTILS; no active race under current OpenMP usage)
- [x] 2.2 Bare except blocks — **Fixed** (5 blocks replaced with specific exception types in app.py)

### Sprint 2 — Numerical Safety & CI (2–3 days) --- COMPLETED 2026-02-14
- [x] 1.4 CO2SYS safe_exp — **Fixed** (8 vulnerable exp() calls wrapped with safe_exp in aquabc_II_co2sys.f90)
- [x] 1.5 Remaining division-by-zero audit — **Audit complete** (2026-02-14). All ~80 divisions in pelagic_model.f90 confirmed safe: iron/Mn use conditional guards, zoo/det use max(), Monod kinetics are mathematically safe, CHLA divides by constants only. One missing Fe3+ first-timestep guard added.
- [x] 3.3 Python code coverage — **Added** (pytest-cov with CI reporting, 10% baseline)
- [x] 3.4 Pin GitHub Actions to SHA — **Done** (5 action references pinned)
- [x] 3.5 CI dependency caching — **Done** (pip cache enabled)

### Sprint 3 — Code Quality (3–5 days) --- COMPLETED 2026-02-14
- [x] 2.3 Deduplicate build/rebuild logic — **Done** (extracted _execute_build_process helper, -110 duplicated lines)
- [x] 2.5 Unit tests for business logic — **Done** (4 functions extracted to utils.py, 28 tests added, 65 total)
- [x] 3.2 Integration tests in CI — **Done** (Playwright job added, 19 tests)
- [x] 3.6 Pre-commit hooks — **Done** (ruff + file hygiene hooks configured)

### Sprint 4 — Architecture (1–2 weeks)
- [x] 4.1 OpenMP benchmarking — **Done** (2026-07-15; `tools/benchmark_openmp.sh` + `docs/OPENMP_PERFORMANCE.md`; 2.84× @ nkn=1000/8thr, ~26% serial → 4.2)
- [x] 2.1 Modularize app.py — **DONE** (decomposition 2026-07-12/13 → leaf modules, then the Shiny-modules rearchitecture `v0.4.0`–`v0.4.5`, 2026-07-14/15: `server()` → 15 namespaced `@module` modules behind `RunController`/`AppState`; app.py 8,012 → 786 lines; see §2.1)
- [x] 1.6 Decompose mega-subroutine — **Done** (2026-07-16; 5 `contains` procedures, byte-identical, gate-verified; found bugs 1.10/1.11)
- [ ] 5.1 Expand Fortran test coverage
- [x] 5.2 End-to-end regression test — **Done** (2026-07-12, 0D pelagic golden-file regression wired into CI; see §5.2)

### Backlog (as time permits)
- [ ] 1.7 Sediment model variable cleanup
- [ ] 1.8 Named physics constants
- [ ] 1.9 IOSTAT error handling
- [x] 1.10 [P1] Model-constants OOB write — **Done** (2026-07-17; nconst 318→323; memory-safety fix, production output byte-identical [adversarial review corrected the garbage-BETA framing])
- [ ] 1.11 [P1] Advanced-redox uninitialised-memory non-determinism — partially root-caused; found during 1.6
- [ ] 2.4 Async file I/O
- [ ] 2.6 Centralized configuration
- [ ] 3.1 Compiler matrix (when Intel CI available)
- [x] 3.7 Release workflow — **Done** (2026-07-10, `.github/workflows/release.yml` + `tools/extract_release_notes.sh`)
- [x] 4.2 CO2SYS parallelization — **Done** (2026-07-15; chunked across threads; nkn=1000/8thr speedup 2.84×→6.55×; see docs/OPENMP_PERFORMANCE.md)
- [x] 4.4 Full-model OpenMP hang @≥8 threads — **Done** (2026-07-15; empty-chunk barrier deadlock in the kinetics region; fixed via balanced chunking + thread cap; ESTAS_II now scales to 8 threads)
- [x] 4.3 Thread affinity documentation — **Done** (2026-07-15; measured negligible on single-socket, see docs/OPENMP_PERFORMANCE.md)

---

*Generated from deep audit of AQUABC v0.2 codebase on 2026-02-13.*
