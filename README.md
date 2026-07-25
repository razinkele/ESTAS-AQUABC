# AQUABC — Aquatic Biogeochemical Cycling Model

[![CI](https://github.com/razinkele/ESTAS-AQUABC/actions/workflows/ci.yml/badge.svg)](https://github.com/razinkele/ESTAS-AQUABC/actions/workflows/ci.yml)
[![Release](https://img.shields.io/github/v/release/razinkele/ESTAS-AQUABC?sort=semver)](https://github.com/razinkele/ESTAS-AQUABC/releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

<!-- LATEST_RELEASE -->**Latest release:** [v0.8.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.8.0)<!-- /LATEST_RELEASE --> · full history in [CHANGELOG.md](CHANGELOG.md)

AQUABC is a process-based **aquatic biogeochemical model** — pelagic and sediment
cycling of nitrogen, phosphorus, silica, carbon, oxygen, and the Mn/Fe/S redox system,
driven by multiple phytoplankton groups (diatoms, cyanobacteria, N-fixers, Nostocales),
zooplankton, and a CO2SYS carbonate solver. The Fortran core is coupled to the **ESTAS**
box-transport driver and packaged with a **Python Shiny** front end. Its reference
application is the **29-box Curonian Lagoon** configuration (CL29), calibrated and
validated against long-term monitoring data.

> The `Latest release` line above is kept in sync automatically by the release workflow
> (`.github/workflows/release.yml` → `tools/sync_release_docs.sh`) on every `vX.Y.Z` tag.

## Contents

- [Overview](#overview)
- [Development status](#development-status)
- [Prerequisites](#prerequisites)
- [Quick start](#quick-start-build--run)
- [Python Shiny front end](#python-shiny-front-end)
- [Testing](#testing)
- [Documentation](#documentation)
- [Contributing](#contributing)
- [License & citation](#license--citation)

## Overview

| Layer | Location | What it is |
|---|---|---|
| Biogeochemistry | `SOURCE_CODE/AQUABC/` | The pelagic + sediment kinetics library (`libaquabc.a`), incl. the CO2SYS carbonate system |
| Transport driver | `SOURCE_CODE/ESTAS/` | The ESTAS box-network solver (`ESTAS_II`) that advects/mixes state variables between boxes |
| Examples | `SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/` | The 0D pelagic example used for the quick start and the CI regression golden |
| Application | `INPUTS_CL29/`, `INPUT_CL29.txt` | The 29-box Curonian Lagoon (EUTROPY-derived) configuration |
| Front end | `shiny_app/` | A Python Shiny app to build, run, edit inputs, and preview outputs |
| Tools | `tools/` | Converters, the EPA observation-ingestion + validation toolchain, benchmarks |

## Development status

**Current release:** v0.8.0. AQUABC is under active research development; maturity varies
by component:

| Component | Status |
|---|---|
| Pelagic biogeochemistry (Fortran core) | **Stable** — `implicit none` throughout, `-Wall -Wextra` release builds, division-by-zero / overflow guards, CO2SYS validated against PyCO2SYS |
| ESTAS transport driver | **Stable** |
| OpenMP parallelization | **Opt-in** (`make OPENMP=1 build-estas`) — ~6.5× at 8 threads for large networks; see `docs/OPENMP_PERFORMANCE.md` |
| Python Shiny front end | **Stable** — 15 namespaced modules; browser integration tests in CI |
| Continuous integration | **Stable** — gfortran on Ubuntu + macOS **and a full Intel `ifx` (oneAPI) build**, Python lint/unit/integration, and a 0D golden-file regression |
| Sediment diagenesis | **Experimental** — advanced redox runs but is off by default; a per-box facies map is data-blocked |
| CL29 Curonian Lagoon application | **Calibrated & validated** — see below |

**CL29 calibration (v0.5.0).** The nitrogen cycle (denitrification) and dissolved oxygen
are calibrated and literature-validated against the EPA **2012–2022** monitoring record
and 2015 experimental process-rate data. The remaining PO4/Si residuals are documented,
bounded structural limits (growing-season phytoplankton uptake and organic-matter
turnover), not open defects. The full arc — tooling, calibrations, and what was ruled out
with evidence — is in [`docs/CL29_EPA_Calibration_Summary.md`](docs/CL29_EPA_Calibration_Summary.md).

**Known limitations / future work:** spatially variable sediment P/Si burial (needs a
Curonian grain-size/facies map); realistic N₂-fixation (coupled to organic-N turnover).
Tracked in `TODO_IMPLEMENTATION_PLAN.md`.

## Prerequisites

- **gfortran** (GNU Fortran) and **make**
- Standard Unix tools (`sh`, `ln`, `ar`)
- **Python 3.11+** for the Shiny front end and the analysis tools (see `shiny_app/requirements.txt`)
- Optional plotting: `gnuplot`, `ps2pdf` (not required to run the model)

## Quick start (build & run)

From the repository root, build the library and run the 0D pelagic example:

```sh
make link-data       # symlink example data into the repo root (idempotent)
make build-lib       # compile the Fortran modules -> SOURCE_CODE/build/libaquabc.a
make build-example   # build the 0D example executable
make run-example     # run it (prints "simulation finished" on completion)
```

To build the **ESTAS engine** (needed for the multi-box Curonian Lagoon runs):

```sh
make build-estas                 # serial (gfortran, release)
make OPENMP=1 build-estas        # OpenMP-enabled
./run_cl29.sh                    # run the 29-box CL29 configuration
```

`run_cl29.sh` bakes in `ESTAS_HOLD_VOLUME=1`, required because the EUTROPY-derived flows
are not per-box volume-conserving.

The pelagic time-integration scheme is selectable via `ESTAS_PELAGIC_SOLVER`: unset or `1` uses the
default Forward Euler solver; `2` selects the Heun/RK2 solver. **RK2 is experimental** — for this
model it converges at only ~1st order (dominated by the `MIN_CONCENTRATION` positivity clamp, not a
solver defect) and is not faster or more accurate than the default Euler; see the "RK2 (Heun's
Method)" section of [`docs/ESTAS_Reference_Manual.md`](docs/ESTAS_Reference_Manual.md) for details.

## Python Shiny front end

`shiny_app/` provides build/run controls, an editor for `INPUTS/` files (first save writes
a `.bak`), and quick plotting of `OUTPUT.csv`. It is organized into fifteen namespaced
`@module` components behind a `RunController`/`AppState` contract rather than one monolith.

```sh
python -m venv .venv && source .venv/bin/activate
pip install -r shiny_app/requirements.txt
./shiny_app/run_shiny.sh          # dev server on port 5001
```

Set `SHINY_RELOAD=1` for autoreload.

> **Warning:** the app writes edits directly to files in `INPUTS/`. Keep backups / use
> version control.

## Testing

```sh
make test-all       # Fortran unit tests + Python suite + lint (the full local gate)
make test-fortran   # Fortran unit tests (also run in the release workflow before a tag)
make test-python    # Python suite: front-end parsers, config/IO helpers, UI fragments
make lint           # ruff over shiny_app/ and tests/
```

An end-to-end 0D golden-file regression lives in `tests/regression/compare_0D.py` (wired
into CI). Browser (Playwright/Selenium) integration tests run in CI's dedicated
`integration-tests` job.

## Documentation

| Document | Topic |
|---|---|
| [`docs/Tutorial_Getting_Started.md`](docs/Tutorial_Getting_Started.md) | Getting started |
| [`docs/AQUABC_Reference_Manual.md`](docs/AQUABC_Reference_Manual.md) | Biogeochemical model reference |
| [`docs/ESTAS_Reference_Manual.md`](docs/ESTAS_Reference_Manual.md) | ESTAS transport driver reference |
| [`docs/ESTAS_AQUABC_Integration_Guide.md`](docs/ESTAS_AQUABC_Integration_Guide.md) | How the two couple |
| [`docs/CL29_EPA_Calibration_Summary.md`](docs/CL29_EPA_Calibration_Summary.md) | Curonian Lagoon calibration & validation |
| [`docs/CL29_KM_2022-2023_Validation.md`](docs/CL29_KM_2022-2023_Validation.md) | CL29 vs the 2022–2023 KM monitoring (nutrients + Chl-a) |
| [`docs/OPENMP_PERFORMANCE.md`](docs/OPENMP_PERFORMANCE.md) | OpenMP scaling & benchmarks |
| [`CHANGELOG.md`](CHANGELOG.md) | Release history |

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) for the build/test/PR workflow and coding
guidelines. In short: branch from `main`, keep changes focused, ensure `make test` and the
Python suite pass, and open a pull request — CI must be green before merge.

## License & citation

Released under the [MIT License](LICENSE). If you use AQUABC in published work, please
cite the repository (`razinkele/ESTAS-AQUABC`) and the release version.
