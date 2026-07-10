# Fixes and Improvements — AQUABC v0.3

## Executive summary

This document summarizes fixes and improvements identified by scanning the repository. Items are grouped by priority with current status.

**Last updated:** 2026-02-13

---

## High priority (Critical / must fix)

1) ~~Fortran: Missing `implicit none` in many routines (silent bugs)~~ **RESOLVED**
   - **Status:** Fixed in commit `39bb8c3` — `implicit none` added to all remaining subroutines and functions.
   - **Verification:** Build with `-fimplicit-none` flag succeeds. All tests pass.

2) ~~Save/load file path handling in `shiny_app/app.py` (path traversal / lack of sanitization)~~ **RESOLVED**
   - **Status:** Fixed in commit `ece21d1` — `safe_resolve()` helper added to validate all user-supplied file paths against base directories. Applied to `load_file`, `save_file`, `file_info_panel`, `validate_constants_file`, and observation file handlers.
   - **Verification:** Traversal attempts (`../`, absolute paths) are rejected with `ValueError`.

3) ~~Add CI step to run `make test` and Fortran regression tests~~ **RESOLVED**
   - **Status:** CI now runs Fortran unit tests via `make test` in the test suite. See `.github/workflows/ci.yml`.

---

## Medium priority (Quality / maintainability) ⚙️

1) ~~Python linting and formatting~~ **RESOLVED**
   - **Status:** `pyproject.toml` updated with ruff configuration (E, F, W, I, UP, B, S rule sets). CI `python-lint-test` job runs `ruff check` on test files. Existing shiny_app code has ~653 lint warnings to address incrementally.

2) ~~Add Python unit tests and CI workflow for the Shiny app code~~ **RESOLVED**
   - **Status:** 46 pytest tests added in `tests/python/` covering `parameter_parser`, `ic_parser`, `options_parser`, `simulation_config`, and `safe_resolve`. CI job runs tests in parallel with Fortran build.

3) ~~Hardening subprocess usage and long-running build/run actions~~ **RESOLVED**
   - **Status:** Fixed — all `subprocess.Popen` calls now have bounded output buffers and `p.wait(timeout=...)` with `TimeoutExpired` handling that kills hung processes. Clean operations: 120s timeout. Build operations: 600s timeout. Helper `run_command`: 600s timeout with kill-on-timeout. Model execution intentionally has no timeout (user-controlled via stop button).
   - **Note:** `shell=True` is used only for Intel oneAPI wrapper (necessary to source `setvars.sh`) with `shlex.quote()` escaping. All other calls use list args.

---

## Low priority (Improvements / docs / ops) 📝

1) ~~Documentation and developer onboarding~~ **RESOLVED**
   - **Status:** `CONTRIBUTING.md` created with build/test/PR workflow, Shiny app instructions, and coding guidelines.

2) ~~Dependency security & maintenance~~ **RESOLVED**
   - **Status:** Dependabot configured (`.github/dependabot.yml`) with weekly scans for pip and GitHub Actions ecosystems. `requirements-dev.txt` created for test/dev tools (ruff, pytest). Fixed `shinywidgets==0.7.0` → `0.7.1` in `requirements.txt`.

3) ~~Add `Makefile` commands for developer checks~~ **RESOLVED**
   - **Status:** Added `make test-all`, `make test-python`, `make test-fortran`, `make lint` targets.

---

## Recently completed improvements (since v0.2.1)

The following major improvements have been implemented:

### Numerical Safety
- **Division-by-zero guards**: ~50+ guarded divisions across pelagic, sediment, macroalgae, CO2SYS, and REDOX models using `max(divisor, 1.0D-20)` convention
- **Zero K_HS defaults fixed**: 3 Monod half-saturation constants (`K_HS_DOC_MIN_DOXY`, `K_HS_DON_MIN_DOXY`, `K_HS_DOP_MIN_DOXY`) had default value 0.0, causing `0/(0+0)=NaN` when substrate reached zero. Fixed to match sibling values (1.0, 0.05, 0.052).
- **pH clamping**: All pH-to-H+ conversions clamped to [4, 11] across all model compartments
- **Safe exponential**: `safe_exp()` function prevents overflow/underflow in light limitation
- **Input clamping**: Temperature [0, 45], salinity >= 0, pH [4, 11] at model entry points

### Ecological Model Enhancements
- **CTMI temperature response** (Rosso et al. 1993) replacing piecewise-exponential
- **Synthesizing Unit colimitation** (Saito et al. 2008) replacing Liebig minimum
- **Platt-style photoinhibition** with tunable BETA parameter per phytoplankton group

### Code Quality
- `implicit none` in all subroutines/functions (commit `39bb8c3`)
- Unified precision via `precision_kinds` module (commit `83b8c04`)
- Named constants replacing magic numbers (commit `a72d39d`)
- 138 unused variables removed (commit `7e295ec`)
- ~250 lines of dead code removed (commit `742209c`)
- Compiler warnings `-Wall -Wextra` enforced on release builds (commit `17b185d`)

### Refactoring
- Derived types for phytoplankton parameters (`t_diatom_params`, `t_cyn_params`, etc.)
- Environmental input bundled into `t_phyto_env` derived type
- Consistent tab-to-space formatting across 15 source files

---

## Remaining action items (prioritized)

1. ~~**Shiny app path sanitization**~~ — **RESOLVED** (commit `ece21d1`)
2. ~~**Subprocess hardening**~~ — **RESOLVED** (timeouts + bounded buffers added)
3. ~~**Python linting**~~ — **RESOLVED** (ruff config in `pyproject.toml`, CI lints test files)
4. ~~**Python unit tests**~~ — **RESOLVED** (46 tests in `tests/python/`, CI runs pytest)
5. ~~**Shiny app lint cleanup**~~ — **RESOLVED** (all 653 ruff warnings fixed, 0 errors remain)
6. ~~**CONTRIBUTING.md**~~ — **RESOLVED** (developer onboarding guide created)
7. ~~**Integration tests**~~ — **RESOLVED** (19 Playwright + 9 Selenium tests)

---

## Notes and references
- `shiny_app/app.py` — main UI where load/save and run/build actions are implemented.
- `FORTRAN_IMPLEMENTATION_PLAN.md` — contains guidance on testing strategy and build matrix.
