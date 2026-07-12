# Design: `shiny_app/app.py` decomposition — phase 1 (non-reactive helper extraction)

- **Date:** 2026-07-12
- **Status:** Draft (awaiting user review)
- **Author:** Arturas Razinkovas-Baziukas (with Claude)
- **Scope:** `shiny_app/`. Extract non-reactive, module-level helper functions out of the
  8,616-line `app.py` into focused, unit-testable modules. **No change to the reactive graph.**

## 1. Context & motivation

`shiny_app/app.py` is a 8,616-line monolith (TODO 2.1, [P1]). Its bulk is two blocks:
`create_ui()` (~1,565 lines) and `server()` (~5,914 lines, **154 reactive handlers** —
`@reactive.effect/.event`, `@render.ui/.text/.table` — all closing over `input/output/session`).
Around 950 lines are module-level, **non-reactive** helper functions. Eleven modules are already
extracted (`parameter_parser`, `ic_parser`, `options_parser`, `mass_balance`,
`simulation_config`, `observation_compare`, `obs_loader`, `scenarios`, `utils`, `diagnostics`,
`safe_resolve`).

## 2. Goal / non-goals

- **Goal:** move the non-reactive module-level helpers into focused modules so `app.py` shrinks
  and the helpers become independently unit-testable. `server()` remains the reactive
  orchestrator; it imports what it needs.
- **Non-goals (this phase):** touching the reactive graph; the full Shiny-modules
  (`@module.ui`/`@module.server`) rearchitecture; splitting `create_ui()` or extracting the
  logic the handlers call — those are the deferred roadmap (§7).
- **Invariant:** the app behaves identically. Guarded by the existing Playwright/Selenium
  integration tests plus new unit tests on the extracted functions.

## 3. Approach

Move each cluster of related, non-reactive functions (+ the constants they *exclusively* use)
into a new `shiny_app/` module. `app.py` re-imports the functions (and any still-shared consts)
via the established fallback pattern already used for `utils`/`safe_resolve`:

```python
try:
    from shiny_app.<mod> import <names>
except ImportError:      # running as a script from inside shiny_app/
    from <mod> import <names>
```

Each `shiny_app/` module **defines its own** module logger (`logger = logging.getLogger("AQUABC")`
— the same named logger `app.py` uses, so log output is unchanged) and self-computes any
`ROOT`/`INPUTS_DIR` it needs (`ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__),
'..'))`, `INPUTS_DIR = os.path.join(ROOT, 'INPUTS')`). So no new shared-config module is required
and each extraction is dependency-free of `app.py`. (A comprehensive AST scan confirmed the only
`app.py`-module-level names the moved functions reference are `logger` and `INPUTS_DIR` — both
reproduced locally — and that **none** reference `input`/`output`/`session`/`ui`/`render`.)

## 4. The three extractions (this phase)

Dependency analysis (verified against the code) shows the three clusters have **no
cross-cluster function calls** — each function only calls others within its own cluster — so the
splits are clean.

### 4.1 `shiny_app/compiler_env.py` — Intel/compiler detection
- **Functions:** `find_compiler_path`, `is_intel_executable`, `get_intel_library_paths`,
  `check_intel_libs_available`, `get_run_environment`, `get_intel_setvars_path`,
  `build_intel_wrapped_command`.
- **Const moved:** `INTEL_COMPILER_SEARCH_PATHS` — used **only** by `find_compiler_path`
  (2 refs total), so it moves fully and is **not** re-imported into `app.py`.
- **Stays in `app.py`:** `COMPILERS`, `BUILD_TYPES` (UI dropdown consts, 3 refs each, used by
  `create_ui()`/`server()`, *not* by these functions).
- **`app.py` re-imports:** all 7 functions (each has 3–6 refs, i.e. called by `server()`).

### 4.2 `shiny_app/input_analysis.py` — input-file analysis
- **Functions:** `analyze_input_file`, `get_input_file_categories`, `validate_required_inputs`.
- **Consts moved:** `INPUT_FILE_CATEGORIES` **including its single `.update({…})` block** (base
  def + the later `INPUT_FILE_CATEGORIES.update(...)` — the update mutates the base, so both must
  move together, update applied at module import; verified there is exactly one such mutation);
  `REQUIRED_INPUT_FILES`, `RECOMMENDED_INPUT_FILES` (used only by `validate_required_inputs`,
  2 refs each — not re-imported). Module also self-computes `INPUTS_DIR` (used by
  `validate_required_inputs`).
- **`app.py` re-imports:** `analyze_input_file`, `get_input_file_categories`,
  `validate_required_inputs`, **and `INPUT_FILE_CATEGORIES`** (11 refs — heavily used by
  `create_ui()`/`server()`).

### 4.3 `shiny_app/file_locators.py` — output/box file discovery
- **Functions:** `get_output_folder`, `find_pelagic_box_file`, `get_available_boxes`,
  `get_timeseries_variables`.
- **Consts:** none moved; module self-computes `ROOT`/`INPUTS_DIR`.
- **`app.py` re-imports:** all 4 (external refs 1–3 in `server()`).

## 5. Phasing & per-phase validation gate

One module per phase, one commit each, in order **compiler_env → input_analysis →
file_locators** (increasing coupling to shared consts). After **each** phase:

1. `python -m py_compile shiny_app/app.py` and `python -c "import shiny_app.<mod>"` succeed.
2. Full Python suite **107 passed** (no regressions).
3. New unit tests for the extracted pure functions pass (`tests/python/test_<mod>.py`).
4. Playwright + Selenium integration tests green (the reactive-behavior safety net).

Any red → stop and fix before the next phase. Each phase leaves `app.py` importing the module,
so the app is runnable at every commit.

## 6. Risks & mitigations

| Risk | Mitigation |
|---|---|
| A moved const is referenced somewhere unnoticed → `NameError` at import/render | Ref-counts verified per name (§4); `app.py` re-imports every function/const with external refs; `py_compile` + import check + Playwright catch a miss |
| `INPUT_FILE_CATEGORIES.update()` ordering (base must exist before update) | Both base and update live in `input_analysis.py`; the final (updated) dict is what `app.py` imports |
| Circular import (`app.py` ↔ new module) | New modules import only stdlib + already-extracted leaf modules; they do **not** import `app.py`. Verified none of the moved functions reference `app_ui`/`server`/`input`/`output`/`session` |
| Behavior drift in a "pure" move | Functions are moved verbatim (no logic edits); unit tests pin behavior; integration tests guard end-to-end |
| Running as a script (`from <mod>`) vs module (`from shiny_app.<mod>`) | Use the existing `try/except ImportError` fallback for every new import |

## 7. Deferred roadmap (not this phase)

- Split `create_ui()` into `shiny_app/ui/` fragment functions (declarative, moderate risk).
- Extract the non-reactive logic the 154 handlers *call* (build-command construction, plot-data
  prep, file I/O) into `server/` helpers; handlers stay as thin reactive wrappers.
- Full Shiny-modules rearchitecture (`@module.ui`/`@module.server`) — largest, highest-risk,
  explicitly out of scope until the above land.

## 8. Files touched (this phase)

- **New:** `shiny_app/compiler_env.py`, `shiny_app/input_analysis.py`,
  `shiny_app/file_locators.py`; `tests/python/test_compiler_env.py`,
  `test_input_analysis.py`, `test_file_locators.py`.
- **Modified:** `shiny_app/app.py` (remove the moved defs/consts, add the re-imports);
  `TODO_IMPLEMENTATION_PLAN.md` (mark 2.1 phase-1 progress).
- **Out of scope:** `create_ui()`, `server()` bodies, any `.f90`.
