# Contributing to AQUABC v0.2

## Prerequisites

- **Fortran compiler**: gfortran (default), ifort, or ifx
- **Python 3.10+** with micromamba shiny environment at `/opt/micromamba/envs/shiny`
- **Make** for build orchestration

## Quick Start

```bash
# Clone and build
git clone <repo-url> && cd AQUABCv0.2
make build-estas

# Run the model
make run-estas

# Run all tests
make test-all
```

## Building

```bash
make build-estas                          # Default: gfortran, release
make FC=ifort BUILD_TYPE=debug build-estas  # Intel debug build
make OPENMP=1 build-estas                 # With OpenMP parallelization
make clean-all && make build-estas        # Clean rebuild
make show-config                          # Show current build settings
```

Build types: `debug` (bounds checking, backtraces), `release` (-O2), `fast` (aggressive -O3).

## Testing

### Run everything

```bash
make test-all    # Fortran unit tests + Python unit tests + lint
```

### Fortran unit tests (18 tests)

```bash
make -C tests/fortran test                  # All Fortran unit tests
make -C tests/fortran test_do_saturation    # Individual test
```

### Python unit tests (46+ tests)

```bash
/opt/micromamba/envs/shiny/bin/python -m pytest tests/python/ -v
```

### Integration tests (Playwright)

```bash
/opt/micromamba/envs/shiny/bin/python -m pytest tests/python/test_app_playwright.py -v
```

### Linting

```bash
make lint        # Ruff check on all Python code
```

### Pre-commit Hooks (Recommended)

Install pre-commit hooks to automatically lint before each commit:

```bash
pip install pre-commit
pre-commit install
```

This runs ruff and basic file checks on every `git commit`. To run manually on all files:

```bash
pre-commit run --all-files
```

## Project Structure

```
AQUABCv0.2/
  SOURCE_CODE/           # Fortran source (ESTAS framework + AQUABC model)
    ESTAS/               # Simulation framework
    AQUABC/              # Water quality model (pelagic, sediment, CO2SYS)
    CORE_UTILS/          # Shared modules (precision_kinds, model_dimensions)
    build/               # Library build script (make_lib.sh)
  shiny_app/             # Python Shiny web UI
    app.py               # Main application (~8100 lines)
    parameter_parser.py  # Model parameter file parser
    ic_parser.py         # Initial conditions parser
    options_parser.py    # Model options parser
    simulation_config.py # Simulation configuration parser
  tests/
    fortran/             # Fortran unit tests (18 tests)
    python/              # Python unit + integration tests
  INPUTS/                # Model input files
  Makefile               # Top-level build system
```

## Making Changes

### Fortran code

1. All files must have `implicit none` in every subroutine/function.
2. Use `precision_kinds` module for numeric types (`use precision_kinds, only: wp`).
3. Guard divisions: `result = numerator / max(denominator, 1.0D-20)`.
4. Build with debug mode to catch issues: `make FC=gfortran BUILD_TYPE=debug build-estas`.
5. Run `make -C tests/fortran test` before committing.
6. If adding a new source file, update `SOURCE_CODE/build/make_lib.sh` compilation order.

### Python / Shiny app code

1. All code must pass `ruff check` with zero errors.
2. Run `make lint` before committing.
3. Add unit tests for new parser/utility code in `tests/python/`.
4. The Shiny app uses custom sidebar navigation via `Shiny.setInputValue('navigation', navId)` — integration tests should use this pattern rather than clicking nav links by text.

### Commit messages

Follow conventional commits:

- `fix:` for bug fixes
- `feat:` for new features
- `refactor:` for code restructuring
- `docs:` for documentation
- `test:` for test additions

### Pull requests

1. Create a feature branch from `main`.
2. Keep changes focused — one logical change per PR.
3. Ensure all tests pass (`make test-all`).
4. Ensure lint passes (`make lint`).
5. Include a brief description of what changed and why.

## Running the Shiny App

```bash
# Using the micromamba environment
/opt/micromamba/envs/shiny/bin/python -m shiny run --port 5001 shiny_app/app.py

# Or with the convenience script
./shiny_app/run_shiny.sh
```

## CI

GitHub Actions runs on every push to `main` and on PRs:

- **python-lint-test**: ruff lint + pytest on `tests/python/`
- **build-and-run**: Fortran build + unit tests + 0D example regression test + ftnchek static analysis
