# TODO Implementation Plan — AQUABC v0.2

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

**File:** `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_ALLELOPATHY.f90`

**Problem:** 44 local allocatable arrays are allocated at subroutine entry but never deallocated. Every call leaks memory. Over a long simulation with thousands of timesteps × spatial nodes, this accumulates into significant memory bloat.

**Fix:**
- Add `deallocate(...)` for all 44 arrays before each `return` and at subroutine end
- Alternatively, convert to automatic (stack) arrays since `nkn` is known at entry: `real(dp) :: ALLEL_C(nkn)` instead of `allocatable`

**Effort:** ~1 hour

---

### 1.2 [P0] Unguarded K_E Division — EUPHOTIC_DEPTH

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Problem:** `EUPHOTIC_DEPTH = 4.61D0 / K_E` — if K_E is zero (e.g., no particles, no background extinction), this produces Inf/NaN that propagates through light limitation.

**Fix:**
```fortran
EUPHOTIC_DEPTH(ns:ne) = 4.61D0 / max(K_E(ns:ne), 1.0D-20)
```

**Effort:** 5 minutes

---

### 1.3 [P0] SAVE Variables — Thread Safety Risk

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

**File:** `SOURCE_CODE/AQUABC/PELAGIC/co2sys.f90`

**Problem:** Several raw `exp()` calls with unbounded arguments (e.g., `exp(-pH * log(10))` for extreme pH values). While pH is now clamped at model entry, CO2SYS has its own internal calculations that could produce extreme arguments.

**Fix:**
- Add `safe_exp()` calls (already defined in `aquabc_II_pelagic_model_constants.f90`) to the ~5 vulnerable `exp()` calls in CO2SYS
- Or clamp arguments: `exp(max(min(arg, 700.0D0), -700.0D0))`

**Effort:** ~30 minutes

---

### 1.5 [P1] Remaining Division-by-Zero Risks in Pelagic Model

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Problem:** A handful of divisions were missed in the earlier hardening pass:
- `DIA_C / PHYT_TOT_C` and similar phytoplankton fraction calculations where `PHYT_TOT_C` could be zero
- `FE_II / (FE_II + FE_III)` dissolved iron fractions
- Any division by biomass quantities that could reach zero during die-off events

**Fix:** Audit all `/` operations in the kinetics subroutine, add `max(divisor, 1.0D-20)` guards where needed.

**Effort:** ~1 hour

---

### 1.6 [P2] Mega-Subroutine Decomposition

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` (~3600 lines)

**Problem:** `AQUABC_PELAGIC_KINETICS` is a single ~3400-line subroutine. This makes it hard to navigate, test, and maintain. The OpenMP parallelization added further complexity.

**Suggested decomposition:**
1. Extract sequential preprocessing (CO2SYS, clamping, K_E) into `pelagic_preprocess()`
2. Extract biology block (phytoplankton + zooplankton calls) into `pelagic_biology(ns, ne, ...)`
3. Extract chemistry block (mineralization, redox, nitrification, volatilization) into `pelagic_chemistry(ns, ne, ...)`
4. Extract derivative assembly into `pelagic_derivatives(ns, ne, ...)`
5. Keep OpenMP region in the main subroutine as orchestrator

**Risk:** This is a large refactor. Should be done carefully with before/after output comparison.

**Effort:** ~1–2 days

---

### 1.7 [P2] Sediment Model Variable Declarations

**File:** `SOURCE_CODE/AQUABC/SEDIMENT/aquabc_II_sed_solute_model.f90`

**Problem:** ~307 variable declarations at the top of the main sediment subroutine. Many may be unused after previous cleanups.

**Fix:**
- Run unused variable detection: `gfortran -Wunused-variable`
- Remove confirmed unused declarations
- Consider grouping related variables into derived types (similar to Phase 2 pelagic refactoring)

**Effort:** ~2–4 hours

---

### 1.8 [P3] Magic Numbers in Physics Constants

**Files:** Various, especially `co2sys.f90`, sediment model

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

---

### 2.2 [P1] Bare Except Blocks (5 remaining)

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

---

### 3.2 [P1] Integration Tests Excluded from CI

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

**Problem:** No visibility into which code paths are tested. Can't measure improvement or identify untested critical paths.

**Fix:**
- Python: Add `pytest-cov` to dev dependencies, add `--cov=shiny_app --cov-report=xml` to CI
- Fortran: Consider `gcov` integration for unit test coverage (lower priority)
- Upload reports to Codecov or similar service

**Effort:** ~2 hours (Python), ~4 hours (Fortran)

---

### 3.4 [P2] GitHub Actions Not Pinned to SHA

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

**Problem:** Developers can commit code that fails linting or has formatting issues. These are only caught in CI after push.

**Fix:**
1. Add `.pre-commit-config.yaml` with ruff, trailing whitespace, end-of-file fixer
2. Document in CONTRIBUTING.md: `pre-commit install`

**Effort:** ~1 hour

---

### 3.7 [P3] No Release Workflow

**Problem:** No automated process for creating tagged releases with changelogs and build artifacts.

**Fix:** Add `.github/workflows/release.yml` triggered on version tags:
1. Build the Fortran library
2. Run full test suite
3. Create GitHub Release with changelog excerpt and binary artifact

**Effort:** ~2–4 hours

---

## 4. OpenMP Follow-up Items

### 4.1 [P2] Performance Benchmarking

**Status:** OpenMP parallelization is complete (Phase 4) but not benchmarked.

**Task:**
1. Create a benchmark script that times `AQUABC_PELAGIC_KINETICS` with `OMP_NUM_THREADS=1,2,4,8`
2. Use a representative test case with realistic `nkn` (100–1000 nodes)
3. Report wall-clock time and compute speedup/efficiency
4. Document results in `docs/OPENMP_PERFORMANCE.md`

**Effort:** ~2–4 hours

---

### 4.2 [P2] CO2SYS Parallelization

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Problem:** CO2SYS computation is currently sequential (before the parallel region). For large `nkn`, this could become the serial bottleneck (Amdahl's law).

**Fix:** Profile first. If CO2SYS takes >10% of kinetics time, parallelize its loop similarly to the main computation block.

**Effort:** ~4 hours (including profiling)

---

### 4.3 [P3] OpenMP Thread Affinity Guidance

**Task:** Document recommended `OMP_PROC_BIND` and `OMP_PLACES` settings for optimal cache behavior:
```bash
export OMP_PROC_BIND=close
export OMP_PLACES=cores
```

**Effort:** ~30 minutes (documentation only)

---

## 5. Testing Improvements

### 5.1 [P2] Fortran Test Coverage Expansion

**Current:** 25 test programs covering 8 library subroutines + utilities

**Missing coverage:**
- CO2SYS (complex equilibrium chemistry — high bug risk)
- Sediment model subroutines
- ALLELOPATHY subroutine
- Light extinction (`light_kd`)
- Ammonia volatilization
- Iron oxidation
- pH correction functions

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

---

## Implementation Roadmap

### Sprint 1 — Critical Fixes (1–2 days) --- COMPLETED 2026-02-14
- [x] 1.1 ALLELOPATHY memory leaks — **No fix needed** (file `aquabc_II_pelagic_lib_ALLELOPATHY.f90` does not exist; `mod_ALLELOPATHY.f90` has proper alloc/dealloc)
- [x] 1.2 K_E division guard — **Fixed** in CYANOBACTERIA, FIX_CYANOBACTERIA, NOSTOCALES library files (not pelagic_model.f90 as originally stated)
- [x] 1.3 SAVE variable thread safety audit — **Documented** (22 vars in pelagic_interface + 3 in STRING_UTILS; no active race under current OpenMP usage)
- [x] 2.2 Bare except blocks — **Fixed** (5 blocks replaced with specific exception types in app.py)

### Sprint 2 — Numerical Safety & CI (2–3 days)
- [ ] 1.4 CO2SYS safe_exp
- [ ] 1.5 Remaining division-by-zero audit
- [ ] 3.3 Python code coverage
- [ ] 3.4 Pin GitHub Actions to SHA
- [ ] 3.5 CI dependency caching

### Sprint 3 — Code Quality (3–5 days)
- [ ] 2.3 Deduplicate build/rebuild logic
- [ ] 2.5 Unit tests for business logic
- [ ] 3.2 Integration tests in CI
- [ ] 3.6 Pre-commit hooks
- [ ] 4.1 OpenMP benchmarking

### Sprint 4 — Architecture (1–2 weeks)
- [ ] 2.1 Modularize app.py
- [ ] 1.6 Decompose mega-subroutine
- [ ] 5.1 Expand Fortran test coverage
- [ ] 5.2 End-to-end regression test

### Backlog (as time permits)
- [ ] 1.7 Sediment model variable cleanup
- [ ] 1.8 Named physics constants
- [ ] 1.9 IOSTAT error handling
- [ ] 2.4 Async file I/O
- [ ] 2.6 Centralized configuration
- [ ] 3.1 Compiler matrix (when Intel CI available)
- [ ] 3.7 Release workflow
- [ ] 4.2 CO2SYS parallelization
- [ ] 4.3 Thread affinity documentation

---

*Generated from deep audit of AQUABC v0.2 codebase on 2026-02-13.*
