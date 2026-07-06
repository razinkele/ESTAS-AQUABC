# Copilot Instructions for AQUABC v0.2

## Project Overview

AQUABC v0.2 is a water quality modelling system. The scientific core is written in Fortran 90: the **ESTAS** simulation framework drives the **AQUABC** pelagic/sediment kinetic model. A **Python Shiny** web front end provides a UI for editing inputs, running the model, and visualising `OUTPUT.csv`. Python is used only for the UI and test tooling — all numerical computation is Fortran.

---

## Build Commands

```bash
# Standard build (gfortran, release) — produces ./ESTAS_II
make build-estas

# Debug build: bounds checking, backtraces, FPE traps
make FC=gfortran BUILD_TYPE=debug build-estas

# Release build with Intel compilers
make FC=ifort BUILD_TYPE=release build-estas
make FC=ifx   BUILD_TYPE=release build-estas

# Build with OpenMP parallelisation
make OPENMP=1 build-estas

# Auto-named binary: ESTAS_II_gf_release, ESTAS_II_ifort_debug, etc.
make build-named

# Run model (reads INPUT.txt from repo root)
make run-estas                        # equivalent: ./ESTAS_II INPUT.txt

# Clean rebuild
make clean-all && make build-estas

# Show active build config
make show-config
```

Build artifact: `SOURCE_CODE/build/libaquabc.a` is compiled first by `make_lib.sh`, then linked into the executable. The `build/` directory also holds all `.o` and `.mod` files.

> **WARNING — BUILD_TYPE=fast**: `-ffast-math` (gfortran) and `-fp-model fast=2` (Intel) break IEEE 754 compliance. They can silently change results for `exp()`, `log()`, trig, Michaelis-Menten kinetics, and DO saturation. Always use `release` for validated science; use `fast` only for profiling.

---

## Test Commands

```bash
# Everything: Fortran unit tests + Python unit tests + ruff lint
make test-all

# Fortran unit tests (18 individual test programs)
make -C tests/fortran test

# Single Fortran unit test by name
make -C tests/fortran test_do_saturation
make -C tests/fortran test_fix_cyn
make -C tests/fortran test_growth_temp

# Python unit tests only (46+ tests; excludes slow browser tests)
/opt/micromamba/envs/shiny/bin/python -m pytest tests/python/ -v \
  --ignore=tests/python/test_app_playwright.py \
  --ignore=tests/python/test_app_selenium.py

# Single Python test file
/opt/micromamba/envs/shiny/bin/python -m pytest tests/python/test_parameter_parser.py -v

# Single Python test function
/opt/micromamba/envs/shiny/bin/python -m pytest tests/python/test_parameter_parser.py::test_parse_diatom_params -v

# Playwright integration tests (slow; requires the Shiny app to be running)
/opt/micromamba/envs/shiny/bin/python -m pytest tests/python/test_app_playwright.py -v

# Ruff linter only
make lint
```

The Python interpreter is `/opt/micromamba/envs/shiny/bin/python`. `PYTHON` in the Makefile always resolves to that path.

---

## Shiny App

```bash
# Dev server (port 5001)
./shiny_app/run_shiny.sh

# With hot-reload
SHINY_RELOAD=1 ./shiny_app/run_shiny.sh

# Custom reload port (absolute or relative offset)
SHINY_RELOAD=1 SHINY_RELOAD_PORT=5124 ./shiny_app/run_shiny.sh
SHINY_RELOAD=1 SHINY_RELOAD_PORT=+200 ./shiny_app/run_shiny.sh

# Production deploy (systemd + optional nginx)
sudo bash shiny_app/deploy.sh
sudo systemctl status shiny_aquabc
sudo journalctl -u shiny_aquabc -f
```

---

## Repository Layout

```
SOURCE_CODE/
  CORE_UTILS/          # Shared modules compiled first: precision_kinds, VECTOR_MATRIX_UTILS,
                       #   STRING_UTILS, FIND_FILE_UNIT, DBL_ARRAY_2D_TO_1D
  AQUABC/
    PELAGIC/           # Pelagic kinetics: phytoplankton, zooplankton, DO, nutrients, redox
      AQUABC_PELAGIC_LIBRARY/   # One .f90 per biological group (DIATOMS, CYANOBACTERIA, …)
      aquabc_II_pelagic_interface.f90   # Public API (aquabc_init / aquabc_run)
      aquabc_II_pelagic_model.f90       # Main kinetics dispatch (PELAGIC_KINETICS)
      aquabc_II_pelagic_auxillary.f90   # Shared helpers: GROWTH_AT_TEMP, LIM_LIGHT, DO_SATURATION
      aquabc_II_pelagic_svindex.f90     # State-variable index parameters (NH4=1 … FIX_CYN_AK=32)
      aquabc_II_pelagic_types.f90       # Derived types per organism group (t_diatom_params, …)
      aquabc_II_pelagic_model_constants.f90  # Module-level parameter variables (all 318)
    SEDIMENTS/         # Sediment diagenesis: dissolution, mineralisation, bioturbation
    CO2SYS/            # CO2 / carbonate chemistry (inorganic carbon, alkalinity, pH)
    AQUABC_EXAMPLES/   # Standalone 0D example used by CI regression test
  ESTAS/               # Simulation framework
    ESTAS_II.f90       # Program entry; reads INPUT.txt, dispatches to RUN_SIMULATION
    mod_GLOBAL.f90     # Global dimensions (nstate=32, nconst=318, NUM_SED_VARS=24, …)
    mod_SIMULATE.f90   # Time-loop driver (RUN_SIMULATION)
    mod_SOLVER.f90     # Euler/RK time integrator (SOLVE); calls PELAGIC_KINETICS each step
    mod_PELAGIC_BOX_MODEL.f90   # Grid data structure for multi-box setups
    mod_BASIN.f90      # Basin geometry
    mod_TIME_SERIES.f90         # Forcing time-series interpolation
    mod_MASS_LOAD.f90           # Point/diffuse mass loads
    mod_PELAGIC_ECOLOGY.f90     # Passes kinetics results back to solver
    mod_PELAGIC_EXERGY.f90      # Optional exergy diagnostics
    mod_COST_FUNCTION.f90       # Optional calibration cost function
    sub_READ_PELAGIC_INPUTS.f90 # Parses PELAGIC_INPUTS.txt (geometry, links, BCs, …)
    sub_WRITE_PELAGIC_OUTPUT.f90 / sub_WRITE_PELAGIC_*  # CSV and binary output writers
  build/
    make_lib.sh        # Single-pass compilation script; defines module dependency order
    libaquabc.a        # Compiled static library (generated; not in VCS)
shiny_app/
  app.py               # Main Shiny application (~8100 lines); all UI + server logic
  parameter_parser.py  # Parses WCONST_04.txt / EXTRA_WCONST.txt (318 params, 14 categories)
  ic_parser.py         # Parses INIT_CONC_*.txt (initial concentrations for all state vars)
  options_parser.py    # Parses PELAGIC_MODEL_OPTIONS.txt
  simulation_config.py # Parses INPUT.txt (dates, time steps, folder paths)
  diagnostics.py / diagnostics_plots.py  # Output post-processing and diagnostic plots
  observation_compare.py / obs_loader.py # Observation data loading and comparison
  mass_balance.py      # Mass balance diagnostics
  scenarios.py / scenarios/  # Scenario management
tests/
  fortran/             # One .f90 per unit test; each compiled independently against libaquabc.a
  python/              # pytest suite: parser unit tests + Playwright integration tests
INPUTS/                # Model input files (edit via Shiny UI; first save creates .bak backup)
INPUT.txt              # Default run configuration (points at INPUTS/ and OUTPUTS/)
PARAMETER_REFERENCE.md # Human-readable parameter quick reference (14 categories, typical ranges)
docs/                  # Reference manuals (Markdown; generate PDFs with make build-docs)
```

---

## Scientific Model Structure

### Pelagic State Variables (32 total — indices from `aquabc_II_pelagic_svindex.f90`)

| Index | Variable | Description |
|-------|----------|-------------|
| 1 | NH4_N | Ammonium nitrogen |
| 2 | NO3_N | Nitrate nitrogen |
| 3 | PO4_P | Dissolved inorganic phosphorus |
| 4 | DISS_OXYGEN | Dissolved oxygen |
| 5 | DIA_C | Diatoms (carbon) |
| 6–8 | ZOO_C/N/P | Zooplankton (C, N, P) |
| 9–11 | DET_PART_ORG_C/N/P | Particulate detritus (C, N, P) |
| 12–14 | DISS_ORG_C/N/P | Dissolved organics (C, N, P) |
| 15 | CYN_C | Non-fixing cyanobacteria |
| 16 | OPA_C | Other phytoplankton algae |
| 17 | DISS_Si | Dissolved silica |
| 18 | PART_Si | Particulate silica |
| 19 | FIX_CYN_C | Nitrogen-fixing cyanobacteria (Nostocales vegetative) |
| 20 | INORG_C | Dissolved inorganic carbon |
| 21 | TOT_ALK | Total alkalinity |
| 22–25 | FE_II, FE_III, MN_II, MN_IV | Redox metals |
| 26–27 | CA, MG | Major cations |
| 28–29 | S_PLUS_6, S_MINUS_2 | Sulphur species |
| 30 | CH4_C | Methane carbon |
| 31 | FIX_CYN_HET_C | Nostocales heterocysts |
| 32 | FIX_CYN_AK_C | Nostocales akinetes |

The `PELAGIC_INPUTS.txt` header declares `NUM_PELAGIC_STATE_VARS = 36` — the 4 extra slots are used internally for allelopathy state variables when that option is active.

### Sediment State Variables (24 total) and constants (171)

Tracked via `mod_BOTTOM_SEDIMENTS.f90`. Processes include: POC/PON/POP/PSi dissolution, DOC/DON/DOP/DOSi mineralisation, nitrification, denitrification, sulphate reduction, iron/manganese redox, bioturbation (`aquabc_II_sediment_bioturbation.f90`). Sediment model is toggled via `MODEL_SEDIMENTS` in `INPUT.txt` (0 = off, use prescribed fluxes instead).

### Phytoplankton Groups and Key Parameters

Each group follows the **CTMI temperature model** (Rosso 1993): growth is zero below `T_min` and above `T_max`, peaks at `T_opt`. Parameter naming in `WCONST_04.txt`:
- `KG_<GROUP>_OPT_TEMP` — maximum growth rate at `T_opt` (/day)
- `<GROUP>_OPT_TEMP_LR` — `T_min` (minimum cardinal temperature)
- `<GROUP>_OPT_TEMP_UR` — `T_opt` (optimal temperature)
- `KAPPA_<GROUP>_OVER_OPT_TEMP` — `T_max` (maximum cardinal temperature; growth → 0)
- `KAPPA_<GROUP>_UNDER_OPT_TEMP` — **unused legacy field**, kept for backward file compatibility

| Group | Index | Notes |
|-------|-------|-------|
| Diatoms (DIA) | 5 | Requires dissolved silica (DSi); `DIA_Si_TO_C` stoichiometry |
| Non-fixing cyanobacteria (CYN) | 15 | No silica; `frac_avail_DON` controls DON use |
| N-fixing cyanobacteria (FIX_CYN) | 19, 31, 32 | Three state vars (vegetative, heterocyst, akinete); `R_FIX`, `K_FIX` params |
| Other phytoplankton (OPA) | 16 | Generic freshwater algae; cooler optimum |

### Driving Functions (10 per box, per time step)

Passed as `DRIVING_FUNCTIONS(nkn, 10)` to `PELAGIC_KINETICS`:

| Slot | Variable | Units |
|------|----------|-------|
| 1 | Water temperature (TEMP) | °C, clamped [0, 45] |
| 2 | Salinity (SALT) | PSU |
| 3 | Solar radiation (I_A) | converted internally to langleys |
| 4 | Fraction of daylight (FDAY) | 0–1 |
| 5 | Air temperature (AIRTEMP) | °C |
| 6 | Wind speed (WINDS) | m/s |
| 7 | Elevation (ELEVATION) | m above sea level |
| 8 | Water depth (DEPTH) | m |
| 9 | Background light extinction (K_B_E) | 1/m |
| 10 | Ice cover fraction (ice_cover) | 0–1 |

### Model Options (`INPUTS/PELAGIC_MODEL_OPTIONS.txt`)

| Option | Default | Effect |
|--------|---------|--------|
| ZOOPLANKTON OPTION | 1 | 1 = realistic C:N:P grazing partitioning |
| ADVANCED REDOX SIMULATION | 0 | 1 = full Mn/Fe/SO4/CH4 redox cycle |
| LIGHT_EXTINCTION_OPTION | 0 | 1 = alternative extinction formula |
| CYANO_BOUYANT_STATE_SIMULATION | 1 | 1 = cyanobacteria buoyancy model active |
| CONSIDER NON-OBLIGATORY FIXERS | 1 | 1 = Nostocales vegetative growth enabled |
| CONSIDER HETEROCYST WITH AKINETES | 1 | 1 = heterocyst/akinete state vars active |
| CONSIDER_ALLELOPATHY | 1 | 1 = allelopathic inhibition enabled |

### AQUABC Pelagic API Calling Sequence

```fortran
call aquabc_init(nkn, nstate, n_driving_functions, SURFACE_BOXES)
call aquabc_init_flags(nflags, flags)          ! optional; sets 12 option flags
call aquabc_read_constants(constants_file)     ! optional; overrides defaults from WCONST_04.txt

do i = 1, ntime_steps
    call aquabc_run(time, time_step, STATE_VARIABLES, PH, DRIVING_FUNCTIONS, SEDIMENT_FLUXES)
end do
```

`aquabc_init` **must** run before any parallel region — module-level `SAVE` variables are written once during init and only read during the parallel `aquabc_run` calls (thread-safe by design).

---

## Fortran Conventions

### Mandatory rules (enforced by CI via `ftnchek`)
- **`implicit none`** in every program unit, subroutine, and function — no exceptions.
- **Division guard**: always protect against zero denominators: `result = numerator / max(denominator, 1.0D-20)`.

### Precision
- Use `use precision_kinds, only: wp` and declare reals as `real(wp)`.
- Legacy aliases that resolve to `wp`: `DBL`, `DBL_PREC`, `DBL_PRECISION`, `DBL_ALLEL`. New code should use `wp`; existing code that uses `DBL_PREC` is fine to leave unchanged.
- Double-precision literals in new code: use `1.0_wp` (preferred) or the legacy `1.0D0` form.

### Parameter naming conventions in `WCONST_04.txt` and Fortran
- `KG_<GROUP>_OPT_TEMP` — growth rate at optimal temperature
- `KR_<GROUP>_20` — respiration rate at 20 °C
- `KD_<GROUP>_20` — mortality rate at 20 °C
- `THETA_<RATE>_<GROUP>` — Arrhenius temperature coefficient (typical range 1.04–1.08)
- `KHS_<NUTRIENT>_<GROUP>` — Michaelis-Menten half-saturation constant
- `<GROUP>_N_TO_C`, `_P_TO_C`, `_Si_TO_C`, `_O2_TO_C` — stoichiometric ratios
- `<GROUP>_C_TO_CHLA` — carbon-to-chlorophyll-a conversion

### Adding a new Fortran source file
1. Place it in the appropriate subdirectory (`CORE_UTILS/`, `AQUABC/PELAGIC/`, etc.).
2. Insert it into `SOURCE_CODE/build/make_lib.sh` **after** all modules it `use`s and **before** any module that `use`s it. The script does a single sequential compilation pass — out-of-order placement causes undefined-module errors.
3. Rebuild with `make clean-lib && make build-estas` to verify.

### Debug builds
```bash
make FC=gfortran BUILD_TYPE=debug build-estas
# Flags: -g -Og -fcheck=all -fbacktrace -Wall -Wextra -pedantic -fimplicit-none
#        -ffpe-trap=invalid,zero,overflow
```
The FPE trap (`-ffpe-trap`) causes an immediate abort with a backtrace on any NaN/Inf/divide-by-zero, making root-cause analysis much easier than hunting through output.

---

## Input File Formats

### `INPUT.txt` — top-level run configuration

```
# BASE_YEAR          (integer, e.g. 1998)
# SIMULATION_START   (days since BASE_YEAR Jan 1, e.g. 6209.0)
# SIMULATION_END     (days since BASE_YEAR Jan 1)
# NUM_REPEATS        (usually 1)
# TIME_STEPS_PER_DAY (typical: 240 = 6-minute steps)
# PRINT_INTERVAL IN TIME STEPS
# PELAGIC MODEL INPUT FOLDER   (e.g. INPUTS/)
# PELAGIC MODEL INPUT FILE     (e.g. PELAGIC_INPUTS.txt)
# PELAGIC MODEL OUTPUT FOLDER  (e.g. OUTPUTS/)
# RESUSPENSION_OPTION
# MODEL_SEDIMENTS    (0 = off / prescribed fluxes, 1 = full sediment model)
```

### `INPUTS/PELAGIC_INPUTS.txt` — spatial/hydrodynamic setup

Declares grid dimensions: `NUM_PELAGIC_STATE_VARS` (36), `NUM_MODEL_CONSTANTS` (318), `NUM_PELAGIC_BASINS`, `NUM_PELAGIC_BOXES`, `NUM_PELAGIC_ADVECTIVE_LINKS`, `NUM_SETTLING_VELOCITIES`, `NUM_OPEN_BOUNDARIES`, `NUM_MASS_LOADS`, etc. Then provides time-series pointers, geometry, and boundary conditions.

### `INPUTS/WCONST_04.txt` — kinetic parameters

Format: `ID  NAME  VALUE  ! comment` — one parameter per line, 318 lines total. The 14 categories (with line ranges) are documented in `PARAMETER_REFERENCE.md`. The Python `parameter_parser.py` uses the line-range mapping to parse by category.

### `INPUTS/INIT_CONC_*.txt` — initial concentrations

One value per state variable (36 values), one per line. Multiple sets are supported (`NUM_PELAGIC_INIT_CONC_SETS` in `PELAGIC_INPUTS.txt`).

### `INPUTS/PELAGIC_MODEL_OPTIONS.txt` — feature flags

Plain `# LABEL` / `value` pairs. Parsed by `options_parser.py`.

> **Known issue**: `data/const_CL.txt` currently has 306 constants while the model expects 318. The model writes `const_out.txt` with fallback defaults and continues — this is non-fatal but means some constants use defaults rather than calibrated values.

---

## Python / Shiny App Conventions

### Linting and pre-commit
- **Linter**: `ruff` with 120-char line length. Run `make lint` before committing.
- **Pre-commit hooks**: `pre-commit install` sets up automatic ruff + file checks on every `git commit`. Run manually: `pre-commit run --all-files`.
- Active ruff rules: `E, F, W, I (isort), UP (pyupgrade), B (bugbear), S (bandit)`.

### Key suppressions in `pyproject.toml`
- `E501` (line length), `E741` (ambiguous names — common in scientific code), `S110`, `S603/607` are globally suppressed.
- `shiny_app/app.py` also suppresses `F401` (unused imports from try/except fallback), `F841` (reactive side-effects), `B023`, `E402`, `S602/605` (Intel oneAPI subprocess wrapping).

### Shiny app structure (`shiny_app/app.py`)
~8100 lines; all UI layout and server logic in one file. Navigation sections:
1. **Dashboard** — status overview, quick run
2. **Model Control** — build & run buttons, inline run log
3. **Input Files** — file browser/editor for `INPUTS/`; first save creates `.bak`
4. **Output Viewer** — plots `OUTPUT.csv` via Plotly (multi-series, dual-axis, rolling mean)
5. **Diagnostics** — mass balance, process rate diagnostics
6. **Scenarios** — save/load named scenario sets
7. **Observations** — load and overlay field measurements

Navigation is implemented with `Shiny.setInputValue('navigation', navId)` via JavaScript — **integration tests must use this pattern**, not text-based nav-link clicks.

### Parser modules
All parsers follow the same pattern:
- Module-level logger: `logger = logging.getLogger("AQUABC.<component>")`
- Data returned as `@dataclass` instances
- Parse errors logged at WARNING level; the app continues with defaults

| Module | Parses | Key class |
|--------|--------|-----------|
| `parameter_parser.py` | `WCONST_04.txt` / `EXTRA_WCONST.txt` | `ParameterSet` |
| `ic_parser.py` | `INIT_CONC_*.txt` | `InitialConditions` |
| `options_parser.py` | `PELAGIC_MODEL_OPTIONS.txt` | `ModelOptions` |
| `simulation_config.py` | `INPUT.txt` | `SimulationConfig` |

---

## Commit Conventions

Follow [Conventional Commits](https://www.conventionalcommits.org/):
- `fix:` bug fixes
- `feat:` new features
- `refactor:` code restructuring
- `docs:` documentation
- `test:` test additions

---

## CI

GitHub Actions (`.github/workflows/ci.yml`) runs on push to `main` and all PRs:
- **python-lint-test**: ruff lint + pytest on `tests/python/`
- **build-and-run**: Fortran build → Fortran unit tests → 0D regression test (checks for NaN/negative CHLA) → `ftnchek` static analysis (enforces `implicit none`)

---

## Allelopathy Module

### Overview

The allelopathy module models chemical inhibition between phytoplankton groups via dissolved secondary metabolites. It is an **optional add-on** controlled by `CONSIDER_ALLELOPATHY` in `INPUTS/PELAGIC_MODEL_OPTIONS.txt` (1 = on, 0 = off). When active, the module adds **4 extra state variables** (secondary metabolite concentrations, indices 33–36 in the state vector — the reason `NUM_PELAGIC_STATE_VARS` is 36, not 32).

### Source files

| File | Role |
|------|------|
| `SOURCE_CODE/ALLELOPATHY/mod_ALLELOPATHY.f90` | Module definition: all parameters, allocatable per-box arrays, `ALLOC_ALLEOPATHY(nkn)` / `DEALLOC_ALLEOPATHY()` |
| `SOURCE_CODE/ALLELOPATHY/ALLELOPATHY_LIBRARY/allelopathy_SEC_METABOLITES.f90` | `allelopathy_SEC_METABOLITES(nkn)` — time derivatives for the 4 metabolite pools |
| `SOURCE_CODE/ALLELOPATHY/ALLELOPATHY_LIBRARY/allelopathy_INHIBITION_RATES.f90` | `ALLELOPATHY_INHIBITION_RATES()` — computes per-pair and net `IHBF_*` inhibition factors |
| `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` | Integration point: reads `EXTRA_WCONST.txt`, calls both routines, maps `IHBF_*` → `GROWTH_INHIB_FACTOR_*` |
| `INPUTS/EXTRA_WCONST.txt` | Parameter file for all allelopathy constants |
| `INPUTS/ALLELOPATHIC_INFORMATION.txt` | Per-box legacy inhibition parameters (GPP thresholds and `K_HS_ALLEL` for growth inhibition) |
| `tests/fortran/test_allelopathy.f90` | Unit test covering allocation, inhibition maths, formation/degradation, temperature effect |

### Secondary metabolite pools (4 state variables)

| Pool | Source organism | Variable |
|------|----------------|----------|
| `SEC_METAB_DIA` | Diatoms | State var index 33 |
| `SEC_METAB_NOFIX_CYN` | Non-fixing cyanobacteria | State var index 34 |
| `SEC_METAB_FIX_CYN` | N-fixing cyanobacteria | State var index 35 |
| `SEC_METAB_NOST` | Nostocales | State var index 36 |

All four are allocated per-box (`dimension(:), allocatable`) in module `ALLELOPATHY`.

### Mathematical framework

**Metabolite dynamics** (first-order, temperature-corrected):
```
d[SEC_METAB_i]/dt = S_i × R_DEATH_i  −  k_DEG_i_20 × θ_i^(T−20) × [SEC_METAB_i]
```
- `S_SEC_METAB_TO_<GROUP>` — stoichiometric yield (metabolite released per unit death rate)
- `k_DEG_SEC_METAB_<GROUP>_20` — degradation rate at 20 °C (/day)
- `THETA_k_DEG_SEC_METAB_<GROUP>` — Arrhenius temperature coefficient (typical: 1.045–1.060)
- `ALLEL_R_DEATH_<GROUP>` — organism death process rate, pulled from `AQUABC_PROCESS_RATES(:, <INDEX>, 4)`

> **Bug note (fixed)**: Before the current version, `R_DEG` was computed without multiplying by `SEC_METAB` concentration — it was a zero-order rather than first-order loss. The fix is documented in `allelopathy_SEC_METABOLITES.f90`.

**Inhibition factor** (reverse Monod, per producer–target pair):
```
IHBF_<PRODUCER>_<TARGET> = K_HS / (K_HS + [SEC_METAB_PRODUCER])
```
- Value = 1.0 when metabolite concentration is 0 (no inhibition)
- Value = 0.5 when metabolite = K_HS (half-saturation)
- Value → 0 as metabolite → ∞ (full inhibition)

**Net inhibition** applied to each target group = `min()` over all producer-specific inhibition factors:
```fortran
IHBF_SEC_METAB_DIA = min(IHBF_SEC_METAB_NOFIX_CYN_DIA, IHBF_SEC_METAB_FIX_CYN_DIA, IHBF_SEC_METAB_NOST_DIA)
! OPA and ZOO take the min across all four producers
IHBF_SEC_METAB_OPA = min(IHBF_SEC_METAB_DIA_OPA, IHBF_SEC_METAB_NOFIX_CYN_OPA, IHBF_SEC_METAB_FIX_CYN_OPA, IHBF_SEC_METAB_NOST_OPA)
```

These net factors are then mapped to `GROWTH_INHIB_FACTOR_*` in `mod_PELAGIC_ECOLOGY.f90` and multiplied into each group's growth rate inside `PELAGIC_KINETICS`.

### Integration sequence (per time step)

```
mod_PELAGIC_ECOLOGY → STEP_ECOLOGY:
  1. Read SEC_METAB_* from STATE_VARIABLES(:, nstate+1 … nstate+4)
  2. call ALLELOPATHY_INHIBITION_RATES()          → sets IHBF_SEC_METAB_* arrays
  3. Map IHBF_* to GROWTH_INHIB_FACTOR_* for each group
  4. call PELAGIC_KINETICS (uses GROWTH_INHIB_FACTOR_* to scale growth)
  5. Extract ALLEL_R_DEATH_* from returned AQUABC_PROCESS_RATES
  6. call allelopathy_SEC_METABOLITES(nkn)        → updates DERIVATIVES_SEC_METAB
  7. Advance SEC_METAB state variables by DERIVATIVES_SEC_METAB × dt
```

### Parameter file: `INPUTS/EXTRA_WCONST.txt`

All allelopathy parameters follow a `# LABEL / value` format. They are read **only** when `CONSIDER_ALLELOPATHY = 1`; omitting or skipping this section when the feature is off is safe.

**Default values:**

| Group | `K_HS` targets (all targets share same K_HS per producer) | `k_DEG_20` (/day) | `θ` | `S` (yield) |
|-------|-----|-----|-----|-----|
| DIA | 0.001 | 0.10 | 1.045 | — |
| NOFIX_CYN | 0.002 | 0.15 | 1.050 | — |
| FIX_CYN | 0.003 | 0.20 | 1.055 | — |
| NOST | 0.004 | 0.25 | 1.060 | — |

`S_SEC_METAB_TO_*` (yield coefficients) follow the degradation block. The `K_HS` parameters are pairwise (20 pairs total: 4 producers × 5 targets).

### `INPUTS/ALLELOPATHIC_INFORMATION.txt`

One row per pelagic box. Columns:
- `MODEL_BOX_NO`
- `MIN_CYN_GPP_FOR_DIA_PP_INHIB` — minimum cyanobacteria GPP threshold before diatom PP inhibition activates
- `MIN_CYN_GPP_FOR_OPA_PP_INHIB` — same threshold for OPA inhibition
- `K_HS_ALLEL_OPA_GROWTH_INHIB` — half-saturation for OPA growth inhibition
- `K_HS_ALLEL_DIA_GROWTH_INHIB` — half-saturation for diatom growth inhibition

Default: all boxes use the same values (0.5, 0.6, 0.2, 0.3).

### Known issue: `DEALLOC_ALLEOPATHY`

The `DEALLOC_ALLEOPATHY` subroutine in `mod_ALLELOPATHY.f90` has a bug — it double-deallocates `DERIVATIVES_SEC_METAB` and omits some arrays. The unit test (`test_allelopathy.f90`) explicitly skips calling it and documents this. Do not call `DEALLOC_ALLEOPATHY` at runtime; it is not called in the normal model flow.

### Running the unit test

```bash
make -C tests/fortran test_allelopathy
```

Tests covered: allocation, no-metabolite IHBF = 1, high-metabolite IHBF ∈ (0,1), half-saturation identity (IHBF = 0.5 at [S] = K_HS), formation from death rates, first-order degradation kinetics, Arrhenius temperature effect on degradation, derivative balance (formation − degradation).

### Precision: `DBL_ALLEL`

The `ALLELOPATHY` module declares all its variables as `real(kind = DBL_ALLEL)`. `DBL_ALLEL` is defined in `precision_kinds` as an alias for `wp` (double precision). Use `DBL_ALLEL` (not `wp` or `DBL_PREC`) when writing new code inside the `ALLELOPATHY` module to maintain naming consistency.

### Known scientific limitations (from `shiny_app/ALLELOPATHY_SCIENTIFIC_REVIEW.md`)

- **Release mechanism**: metabolites are released only on cell death, not by active exudation from living cells under nutrient stress (known underestimation).
- **No photodegradation**: degradation is temperature-only; light-dependent photolysis is not implemented.
- **No self-inhibition**: diagonal of the 4×4 producer–target matrix is excluded.
- **Uniform toxin per group**: each group produces a single generic metabolite, not distinct toxin classes (microcystins vs. anatoxin-a, etc.).
- **No zooplankton bioaccumulation**: toxins are not tracked inside the zooplankton biomass.

---

## Debugging Tips

### NaN / unexpected values in output
1. Rebuild with `BUILD_TYPE=debug` — FPE traps abort immediately with a backtrace.
2. Check for missing division guards: any `a / b` where `b` is a biological concentration.
3. Run `make -C tests/fortran test_do_saturation` — DO saturation is a common source of NaN at extreme temperatures.

### Clamp / process-rate limiting
`aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90` implements per-process rate clamping. When a process rate exceeds a threshold, the rate is clamped and a counter is incremented. `make test-unit-limiter` runs a dedicated test for this logic.

### `ftnchek` failures in CI
`ftnchek` flags any subroutine or function missing `implicit none`. Add it to every program unit — there are no legitimate exceptions.

### Fortran compilation order errors
If you see `use` of an undefined module during `make_lib.sh`, the file was added in the wrong position. Open `SOURCE_CODE/build/make_lib.sh`, find the `find` command that collects `.f90` files, and note that compilation is alphabetical within directories — use explicit ordering or rename files to control order if needed.
