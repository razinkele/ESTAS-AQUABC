# AQUABC v0.3 — Quick Start

<!-- LATEST_RELEASE -->**Latest release:** [v0.3.6](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.3.6)<!-- /LATEST_RELEASE --> · full history in [CHANGELOG.md](CHANGELOG.md)

Short README to build and run the example and reproduce local results.

> The `Latest release` line above is kept in sync automatically by the release workflow
> (`.github/workflows/release.yml` → `tools/sync_release_docs.sh`) on every `vX.Y.Z` tag.

## Prerequisites
- gfortran (GNU Fortran)
- make
- standard Unix tools (sh, ln, ar)
- Optional for plotting: `splitcol`, `gp`/gnuplot, `gpsmerge`, `ps2pdf` (not required for running the model)

## Quick build & run (recommended)
From repository root:

1. Link example data into repo root (makes running from repo root work):

   make link-data

2. Build library and example:

   make build-lib
   make build-example

3. Run the 0D example:

   make run-example

This runs the `aquabc_II_pelagic_0D` example using the repository `INPUT.txt` and example `data/` files.

## Makefile targets
- `make link-data` — create `data` symlink pointing to example `data/` (safe to run multiple times)
- `make build-lib` — compiles Fortran modules and creates `libaquabc.a` (uses `SOURCE_CODE/build/make_lib.sh`)
- `make build-example` — builds example executable (links to `libaquabc.a`)
- `make run-example` — runs the example executable from the example directory

## Notes
- The project is Fortran-based (F90). The example Makefiles expect to find `build/libaquabc.a` in `SOURCE_CODE/build` — the repository includes `make_lib.sh` to produce that archive.
- When running briefly, the example prints a message when it finishes ("simulation finished").
- A small discrepancy: `data/const_CL.txt` currently has 306 constants while 318 are expected; the model writes `const_out.txt` with default/fallback values and proceeds. Consider updating `const_CL.txt` if exact constants are required.

## CI
See `.github/workflows/ci.yml` — the workflow builds the library and example and runs the 0D example, checking for successful completion.

---

## Tests ✅

We added a few lightweight tests to help catch regressions:

- `make test` — runs the 0D regression checks:
  - `test/test_0D_no_nan.sh` — runs the example and fails if any NaN or CHLA negatives are detected.
  - `test/test_0D_clamp_summary.sh` — checks that clamp behaviour is summarized compactly (or that no clamps occurred).
  - The `test` target now also invokes the unit-level limiter test.

- `make test-unit-limiter` — a small Fortran unit-style test that directly exercises the FIX_CYN per-process clamp logic to ensure excessive process rates are limited and counters are recorded.

- `make -C tests/fortran clean test` — the Fortran unit tests (also run in the release workflow before publishing a tag).

- `python -m pytest tests/python -q` — the Python test suite (**123 tests**) covering the Shiny front end's parsers, config/IO helpers, and the extracted UI fragments. An end-to-end 0D golden-file regression lives in `tests/regression/compare_0D.py`. Browser (Playwright/Selenium) integration tests run in CI's dedicated `integration-tests` job.

All tests are runnable locally from the example directory. If you'd like, I can add these to CI so `make test` runs during CI as an extra verification step.

## Python Shiny front end 🔧
A minimal Python Shiny front end is included at `shiny_app/`. It provides:

- Build and run buttons (runs `make` targets in the repo root)
- A browser/editor for files in `INPUTS/` (first save creates a `.bak` backup)
- Quick plotting and preview of `OUTPUT.csv`

The front end is organized into focused, unit-tested modules rather than one monolith: `app.py`
holds the reactive `server()` and a thin `create_ui()` assembler; the non-reactive helpers live in
`compiler_env.py`, `input_analysis.py`, `file_locators.py`, and `safe_resolve.py`; and the UI is
composed from fragment modules `ui_scripts.py` (inline JS), `ui_panels.py` (content panels), and
`ui_chrome.py` (sidebar/header/offcanvas). See `CHANGELOG.md` (0.3.3, 0.3.4) for the decomposition history.

Quick start:

1. Create and activate a venv: `python -m venv .venv && source .venv/bin/activate`
2. Install deps: `pip install -r shiny_app/requirements.txt`
3. Run: `shiny run --port 5001 shiny_app.app:app` or `shiny_app/run_shiny.sh` (the script starts the dev server on port **5001** to avoid conflicts).
   - To enable autoreload: `SHINY_RELOAD=1 ./shiny_app/run_shiny.sh`.
   - To set a custom autoreload port (absolute or relative): `SHINY_RELOAD=1 SHINY_RELOAD_PORT=5124 ./shiny_app/run_shiny.sh` or `SHINY_RELOAD=1 SHINY_RELOAD_PORT=+200 ./shiny_app/run_shiny.sh`.

> **Warning:** the app saves edits directly to files in `INPUTS/`. Use backups and version control as needed.

If you'd like, I can expand this README with developer notes, coding guidelines, or a longer `Contributing.md`.