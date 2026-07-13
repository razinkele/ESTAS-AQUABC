# Design: `shiny_app/app.py` decomposition — phase 2 (`create_ui()` fragment split)

- **Date:** 2026-07-13
- **Status:** Draft (awaiting user review)
- **Author:** Arturas Razinkovas-Baziukas (with Claude)
- **Scope:** `shiny_app/`. Split the ~1,566-line `create_ui()` function into declarative UI
  fragment functions in focused modules. **No change to the reactive graph, the UI structure,
  or the rendered HTML.**
- **Predecessor:** `docs/superpowers/specs/2026-07-12-app-py-decomposition-design.md` (phase 1 —
  non-reactive helper extraction, shipped v0.3.3). This is roadmap item §7 bullet 1 of that spec.

## 1. Context & motivation

After phase 1, `shiny_app/app.py` is ~7,925 lines. Its two remaining bulk blocks are
`create_ui()` (~1,566 lines, `426`–`1992`) and `server()` (~5,914 lines, the reactive graph —
out of scope this phase). `create_ui()` is **not** a deeply nested navset tree: it is a flat
sequence of ~30 local assignments (`panel_dashboard = ui.panel_conditional(...)`, six
`ui.tags.script(...)` blocks, sidebar/header/offcanvas chrome) composed at the end into a
`content = [...]` list and returned via `ui.page_fillable(*content, title="AQUABC")`. Each panel
local is **assigned once and referenced once** (inside `main_content`).

The target pattern **already exists** in the codebase: `panel_diagnostics = diagnostics_ui()`
(line 1720) calls an arg-free fragment function in `shiny_app/diagnostics.py`. This phase extends
that precedent to the remaining panels, scripts, and chrome.

## 2. Goal / non-goals

- **Goal:** move each self-contained UI fragment out of `create_ui()` into a focused module so
  `create_ui()` becomes a thin (~40-line) assembler and each fragment is independently readable
  and smoke-testable.
- **Non-goals (this phase):** the reactive graph / `server()`; changing the UI structure, panel
  order, IDs, classes, or any rendered markup; the full Shiny-modules (`@module.ui`/
  `@module.server`) rearchitecture; splitting `server()` helpers (deferred roadmap §7).
- **Invariant:** the rendered UI is **identical** — same elements, same IDs, same order, same
  attributes. Guarded by the existing Playwright/Selenium integration tests (which render the
  full page) plus new per-fragment render-smoke unit tests.

## 3. Approach

Each fragment becomes a module-level function that **returns** the `ui.*` object the local
currently holds. `create_ui()` calls the function where it used to reference the local. Fragment
modules import only stdlib + `shiny` + already-extracted leaf modules; they do **not** import
`app.py` (no circular dependency). `app.py` re-imports the fragment functions via the established
fallback pattern:

```python
try:
    from shiny_app.ui_panels import panel_dashboard, panel_model_build, ...
except ImportError:      # running as a script from inside shiny_app/
    from ui_panels import panel_dashboard, panel_model_build, ...
```

**Const handling (decided): pass-as-args for `app.py`-defined consts; direct leaf-module import
for everything else.** Fragments reference two kinds of external names, which are handled
differently:

- **`app.py`-defined consts → passed as args** (a fragment importing these from `app.py` would
  create a circular import). Exactly **three** fragments need them:
  - `panel_model_build(compilers, build_types)` ← `COMPILERS`, `BUILD_TYPES`
  - `build_sidebar(nav_choices)` ← `NAV_CHOICES`
  - `panel_plot(min_smooth_window)` ← `MIN_SMOOTH_WINDOW` (defined `app.py:186`, also logged at
    `app.py:291`, so it stays in `app.py`)

  These consts **stay defined in `app.py`** and are passed at the call site
  (`panel_model_build(COMPILERS, BUILD_TYPES)`, etc.). No new constants module.

- **Leaf-module names → imported directly** by the fragment module from the same already-extracted
  leaf module `app.py` imports them from (no `app.py` dependency, no circular import). Per §4.2's
  import table.

All other fragments are arg-free (matching `diagnostics_ui()`). Circular imports are structurally
impossible: no fragment module imports `app.py`.

**Reactive safety.** The `ui.panel_conditional(...)` bodies only *declare* UI slots by string ID
(`ui.output_text("file_header_text")`, `ui.input_text_area("file_contents", ...)`). They never
reference `input`/`output`/`session`/`render` or any reactive value. Verified: no fragment body
reads a reactive symbol. So the moves are verbatim relocations of declarative expressions.

## 4. Module layout & the fragments

Three new modules under `shiny_app/`, mirroring phase 1's three-module split. Each defines its
own module logger (`logger = logging.getLogger("AQUABC")`) if it logs (the fragments do not) and
imports `from shiny import ui`.

### 4.1 `shiny_app/ui_scripts.py` — inline JS blocks (~130 lines)
Six functions, each returning `ui.tags.script(...)` verbatim, **zero args, zero deps**:
`reload_script`, `nav_script`, `settings_script`, `help_script`, `changelog_script`,
`theme_script`. (Current locals: `reload_js`, `nav_js`, `settings_js`, `help_js`,
`changelog_js`, `theme_js`.) **Module imports: `from shiny import ui` only** — every block is a
plain static triple-quoted string (verified: no f-strings, no name interpolation).

### 4.2 `shiny_app/ui_panels.py` — content panels (~1,100 lines)
Fourteen fragment functions, each returning its `ui.panel_conditional(...)` verbatim:
`panel_dashboard`, `panel_model_build(compilers, build_types)`, `panel_model_control`,
`panel_input_files`, `panel_parameters`, `panel_initial_conditions`, `panel_model_options`,
`panel_sim_config`, `panel_scenarios`, `panel_plot(min_smooth_window)`, `panel_mass_balance`,
`panel_observations`, `panel_map`, `panel_model_structure`. Arg-taking:
`panel_model_build(compilers, build_types)` and `panel_plot(min_smooth_window)`; the rest are
arg-free. (`panel_diagnostics` already lives in `diagnostics.py` — untouched.)

**`ui_panels.py` module imports** (leaf-module names the fragments reference — the module imports
these at the top via the same `try/except ImportError` fallback `app.py` uses):

| Fragment | Imports it needs |
|---|---|
| `panel_model_control` | `simulation_config`: `TIME_STEP_PRESETS`, `OUTPUT_INTERVAL_PRESETS` |
| `panel_parameters` | `parameter_parser`: `PARAMETER_CATEGORIES` |
| `panel_initial_conditions` | `ic_parser`: `STATE_VARIABLE_CATEGORIES` |
| `panel_model_options` | `options_parser`: `OPTION_CATEGORIES` |
| `panel_input_files` | `input_analysis`: `get_input_file_categories`; `shinywidgets`: `output_widget` |
| `panel_plot` | `shinywidgets`: `output_widget` (+ arg `min_smooth_window`) |
| `panel_map` | `shinywidgets`: `output_widget` |
| `panel_model_build` | (none — consts via args `compilers`, `build_types`) |

All other panels reference only `shiny.ui` + literals. The implementer must confirm the final
import list by grepping each fragment body for free names (the `F821` gate in §5 catches a miss in
`app.py`, but the fragment module needs its own imports right — a missing one fails the module
import check and the render-smoke test).

### 4.3 `shiny_app/ui_chrome.py` — sidebar / header / css / offcanvas (~300 lines)
`build_sidebar(nav_choices)` (builds the `nav_links` loop + `sidebar_content` div; returns the
`sidebar_content` div), `app_header`, `external_css`, `settings_offcanvas`, `help_offcanvas`,
`changelog_offcanvas`. All arg-free except `build_sidebar(nav_choices)`. **Module imports:
`from shiny import ui` only** — verified none of these reference an `app.py`-defined or leaf-module
name: the `"v0.3"` version strings are literals, and `help_offcanvas`/`changelog_offcanvas` embed
no constant or file read — they declare `ui.output_ui("help_content")` / `ui.output_ui(
"changelog_content")` ID slots the server fills. `build_sidebar`'s two f-strings interpolate only
its own loop locals (`is_active`, `icon`).

### 4.4 What **stays** in `create_ui()` (the thin assembler)
The composition glue only: `nav_input`/`nav_input_hidden` (5 lines), `main_content = ui.div(...)`
(the panel composition div), `sidebar_container`, the `content = [...]` list, and
`return ui.page_fillable(*content, title="AQUABC")`. `create_ui()` calls each fragment function
in place of the former local.

## 5. Phasing & per-phase validation gate

One module per phase, one commit each, in order of increasing coupling: **2a `ui_scripts` →
2b `ui_panels` → 2c `ui_chrome`**. After **each** phase:

1. `python -m py_compile shiny_app/app.py` and `python -c "import shiny_app.<mod>"` succeed.
2. Full Python suite green (117 → grows with new smoke tests; no prior test regresses).
3. `ruff --select F821 shiny_app/app.py` clean (the missed-re-import guard — `app.py` cannot be
   imported in-process because it pulls in `pandas`, whose installed build has a NumPy-2 ABI
   crash; `F821` statically catches an undefined name from a forgotten re-import).
4. New per-fragment render-smoke unit tests pass (`tests/python/test_ui_<group>.py`): call each
   fragment and assert **multiple** expected marker strings appear in its rendered HTML (nav
   condition + ≥2 card-header titles for panels) — a single marker is too weak to catch a
   dropped sub-element.
5. Playwright + Selenium integration tests green **in CI's `integration-tests` job**. They are
   **not installed locally** and `tests/python/conftest.py:10-14` `collect_ignore`s them, so
   `pytest tests/python` does not run them here. Locally, the behavioral net is item 4 (multi-
   marker smoke tests) plus the **mandatory verbatim-diff review (§6)** — do not report step 5 as
   locally "passed" unless `import playwright` succeeds.

Any red → stop and fix before the next phase. Each phase leaves `create_ui()` calling the
extracted fragments, so the app renders at every commit.

## 6. Verbatim-move verification

The `panel_x = ui.panel_conditional(...)` → `def panel_x(): return ui.panel_conditional(...)`
transform changes only the assignment head; the `ui.*` expression body is moved **verbatim**. The
implementer confirms per fragment that the moved expression is character-identical to the original
(diff of the extracted expression vs. the removed lines). The render-smoke tests and Playwright
render are the behavioral proof.

## 7. Risks & mitigations

| Risk | Mitigation |
|---|---|
| A fragment silently drops/reorders an element → different UI | Verbatim expression move + **mandatory §6 diff review (the primary local guarantee** — Playwright/Selenium are not installed locally, only in CI); render-smoke tests assert **multiple** structural markers per fragment so a dropped sub-element fails the test |
| Missed re-import after moving a local out → `NameError` at render | `ruff --select F821` gate (phase-1 proven); `py_compile`; import check; Playwright render |
| Circular import (`app.py` ↔ fragment module) | Fragments import only stdlib + `shiny` + leaf modules; **never** `app.py`. The 4 `app.py`-defined consts (`COMPILERS`/`BUILD_TYPES`/`NAV_CHOICES`/`MIN_SMOOTH_WINDOW`) are passed as args, so no fragment needs an `app.py` symbol |
| Fragment module missing a leaf-module import (e.g. `TIME_STEP_PRESETS`, `output_widget`) → `NameError` at render | Per-fragment import table (§4.2) enumerates them; `python -c "import shiny_app.ui_panels"` + the render-smoke test (§5.1, §5.4) fail immediately if one is missing |
| A "pure" move accidentally captures a reactive symbol | Verified no fragment body references `input`/`output`/`session`/`render`; the `ui.output_*`/`ui.input_*` calls are ID-string declarations, not reactive closures |
| Panel order changes because `main_content`/`content` list is rebuilt | The composition lists stay in `create_ui()` **verbatim** — only the RHS of each entry changes from a local name to a same-named function call; order preserved exactly |
| Running as a script (`from ui_panels`) vs module (`from shiny_app.ui_panels`) | Existing `try/except ImportError` fallback for every new import |

## 8. Deferred roadmap (unchanged from phase 1 §7)

- Extract the non-reactive logic the 154 handlers *call* (build-command construction, plot-data
  prep, file I/O) into `server/` helpers; handlers stay thin reactive wrappers.
- Full Shiny-modules rearchitecture (`@module.ui`/`@module.server`) — largest, highest-risk,
  out of scope until the above lands.

## 9. Files touched (this phase)

- **New:** `shiny_app/ui_scripts.py`, `shiny_app/ui_panels.py`, `shiny_app/ui_chrome.py`;
  `tests/python/test_ui_scripts.py`, `test_ui_panels.py`, `test_ui_chrome.py`.
- **Modified:** `shiny_app/app.py` (replace the ~26 fragment locals with function calls, add the
  re-imports; keep `COMPILERS`/`BUILD_TYPES`/`NAV_CHOICES` defined, keep the composition glue);
  `TODO_IMPLEMENTATION_PLAN.md` (mark 2.1 phase-2 progress).
- **Out of scope:** `server()` body, any `.f90`.
