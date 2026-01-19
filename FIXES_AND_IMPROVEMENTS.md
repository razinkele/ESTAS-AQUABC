# Fixes and Improvements — AQUABC v0.2 🔧

## Executive summary ✨

This document summarizes the highest-impact fixes and improvements I found by scanning the repository. It groups items by priority, gives short technical descriptions, points to affected files, and suggests concrete fixes, tests, and CI changes.

---

## High priority (Critical / must fix) ✅

1) Fortran: Missing `implicit none` in many routines (silent bugs)
   - Files / evidence: See `missing_implicit_none.txt` (e.g. `SOURCE_CODE/ESTAS/mod_BOTTOM_SEDIMENTS.f90`, `SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90`, etc.).
   - Impact: Undeclared variables can lead to silent miscomputed values and subtle runtime bugs.
   - Suggested fix:
     - Add `implicit none` to all modules/subroutines/functions listed in `missing_implicit_none.txt`.
     - Add a CI lint step that checks for missing `implicit none` (or integrates the existing patch in `fortran_implicit_none_patch/`).
     - Add unit/regression tests that fail when unexpected NaNs or silently incorrect values appear (see `FORTRAN_IMPLEMENTATION_PLAN.md` and existing 0D tests).
   - Estimated effort: 1–3 days (depending on regression test coverage).

2) Save/load file path handling in `shiny_app/app.py` (path traversal / lack of sanitization)
   - Files: `shiny_app/app.py` (`load_file`, `save_file` and other file operations using `input.file_select()`)
   - Impact: Potential for writing/reading unintended paths if filenames contain `../` or absolute paths. Should ensure all paths are restricted to `INPUTS/`.
   - Suggested fix:
     - Sanitize and normalize selected filenames before using (e.g. `norm = os.path.normpath(path); ensure norm.startswith(INPUTS_DIR)`).
     - Validate `file_select()` values and reject suspicious names; log and show safe error to users.
     - Add tests that simulate malicious filenames and assert rejection.
   - Estimated effort: 0.5–1 day.

3) Add CI step to run `make test` and Fortran regression tests
   - File: `.github/workflows/ci.yml`
   - Impact: CI currently builds and runs minimal example; adding regression tests increases confidence and prevents regressions.
   - Suggested fix:
     - Extend `ci.yml` to run `make test` (or explicit test scripts under `tests/`) and fail the workflow on regressions.
   - Estimated effort: 0.5 day.

---

## Medium priority (Quality / maintainability) ⚙️

1) Python linting and formatting
   - Files: `shiny_app/*.py` and other Python modules
   - Suggestions:
     - Add `pyproject.toml` with `ruff`/`black` configuration (or `pre-commit` hooks). Run `ruff`/`black` in CI.
     - Add type-checking with `mypy` incrementally (start with key modules: parsers, simulation config).
   - Estimated effort: 0.5–1 day (initial setup), more for fixing violations.

2) Add Python unit tests and CI workflow for the Shiny app code
   - Files: `shiny_app/` modules such as `parameter_parser.py`, `ic_parser.py`, `options_parser.py`, `simulation_config.py`.
   - Suggestions:
     - Add unit tests (pytest) for parser classes and file IO (use tmpdir fixtures).
     - Add a GitHub Actions job to run Python tests (in parallel with Fortran build job).
   - Estimated effort: 1–2 days.

3) Hardening subprocess usage and long-running build/run actions
   - Files: `shiny_app/app.py` (calls to `subprocess.run` / `Popen`) and Makefile commands invoked by the UI
   - Suggestions:
     - Ensure all `subprocess` calls use list args (no `shell=True`). Use timeouts and well-handled stdout/stderr logging.
     - Provide bounded buffer reads and timeouts for long-running processes started in threads.
   - Estimated effort: 0.5 day.

---

## Low priority (Improvements / docs / ops) 📝

1) Documentation and developer onboarding
   - Files: `README.md`, `PARAMETER_REFERENCE.md`, `FORTRAN_IMPLEMENTATION_PLAN.md`
   - Suggestions:
     - Add a `CONTRIBUTING.md` outlining run/build/test steps, branch/PR policy, and how to add Fortran fixes safely.
     - Document how to run the Shiny app (venv, requirements), and how to run unit/regression tests locally.
   - Estimated effort: 0.5 day.

2) Dependency security & maintenance
   - Files: `requirements.txt`
   - Suggestions:
     - Consider adding Dependabot or GitHub Actions job to check package security (e.g., `safety`), and optionally a `requirements-dev.txt` for test/dev tools.
   - Estimated effort: 0.25 day.

3) Add `pre-commit` hooks and basic `Makefile` commands for developer checks
   - E.g. `make lint`, `make pytests`, `make fmt`.
   - Estimated effort: 0.25 day.

---

## Suggested immediate action plan (ordered)
1. Add `implicit none` consistently to the Fortran subroutines listed in `missing_implicit_none.txt`. Run Fortran test suite and CI.
2. Add sanitization for `save_file` and `load_file` in `shiny_app/app.py`. Add unit tests covering malicious filenames.
3. Add a CI step to run `make test` (Fortran regression tests) and add a Python test job.
4. Add linting/formatting (`ruff`/`black`) and add pre-commit to save developer time.
5. Document contributors' guide and add instructions to run tests locally.

---

## Example code notes & snippets (quick wins)
- Sanitize path example (Python):

```python
# safe join + check
requested = input.file_select()
path = os.path.normpath(os.path.join(INPUTS_DIR, requested))
if not path.startswith(os.path.abspath(INPUTS_DIR) + os.sep):
    raise ValueError("Invalid file selection")
```

- Fortran: always add at top of subroutine/function:

```fortran
subroutine FOO(...)
  implicit none
  ! declarations...
```

- Add to CI (`.github/workflows/ci.yml`): after `Run example and capture log` step add `make test` or `make -C SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D test`.

---

## Notes and references 📚
- `missing_implicit_none.txt` — exact Fortran routines missing `implicit none`.
- `fortran_implicit_none_patch/` — partially prepared fixes; consider merging with mainline after review and tests.
- `shiny_app/app.py` — main UI where load/save and run/build actions are implemented.
- `FORTRAN_IMPLEMENTATION_PLAN.md` — contains useful guidance on testing strategy and build matrix.

---

If you'd like, I can implement the high-priority fixes now (Fortran implicit-none patches, `shiny_app` path sanitization, and CI change) and open a PR with tests and CI updates. Which items should I start with first? 🚀
