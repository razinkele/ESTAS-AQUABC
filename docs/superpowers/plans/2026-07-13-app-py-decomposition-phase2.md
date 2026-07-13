# app.py Decomposition Phase 2 — `create_ui()` Fragment Split — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split the ~1,566-line `create_ui()` in `shiny_app/app.py` into declarative UI fragment functions across three focused modules (`ui_scripts.py`, `ui_panels.py`, `ui_chrome.py`), leaving `create_ui()` a thin (~40-line) assembler, with the rendered UI byte-for-byte unchanged.

**Architecture:** Each `X = ui.<expr>(...)` local inside `create_ui()` becomes a module-level function `def X_fn(...): return ui.<expr>(...)` in a fragment module. `create_ui()` calls the function where it referenced the local. Fragment modules import only `shiny` + already-extracted leaf modules — never `app.py`. The four `app.py`-defined consts a fragment needs (`COMPILERS`, `BUILD_TYPES`, `NAV_CHOICES`, `MIN_SMOOTH_WINDOW`) are passed as arguments. Spec: `docs/superpowers/specs/2026-07-13-app-py-decomposition-phase2-design.md`.

**Tech Stack:** Python 3.10+, Shiny for Python (`shiny.ui`), shinywidgets, pytest, ruff, Playwright/Selenium.

## Global Constraints

- **Verbatim move.** The `ui.*` expression body inside each fragment MUST be character-identical to the lines it replaces — no logic edits, no reformatting, no reindentation of inner content beyond the one-level function-body indent. The behavioral proof is the render-smoke test + Playwright; the review confirms the moved expression matches the removed lines.
- **No `app.py` import from fragments.** Fragment modules import `from shiny import ui`, plus (for `ui_panels.py` only) the leaf-module names in the §4.2 import table. They MUST NOT `import app` or `from app import …`.
- **Consts stay in `app.py`.** `COMPILERS`, `BUILD_TYPES`, `NAV_CHOICES`, `MIN_SMOOTH_WINDOW` remain defined in `app.py`; the 3 fragments that use them receive them as args (`panel_model_build(compilers, build_types)`, `build_sidebar(nav_choices)`, `panel_plot(min_smooth_window)`).
- **Composition glue stays & order preserved.** `nav_input`, `nav_input_hidden`, `main_content`, `sidebar_container`, the `content = [...]` list, and `return ui.page_fillable(*content, title="AQUABC")` stay in `create_ui()`. In `main_content` and `content`, each moved entry changes ONLY from a local name to a same-named function call; the ORDER of entries is preserved exactly.
- **Import fallback.** Every new re-import in `app.py` uses the established `try: from shiny_app.<mod> import … / except ImportError: from <mod> import …` pattern, placed right after the `diagnostics` import block (`app.py:166–170`).
- **Per-phase gate (run ALL after each task, must be green before the next):**
  1. `python -m py_compile shiny_app/app.py`
  2. `python -c "import shiny_app.<new_module>"`
  3. `ruff check --select F821 shiny_app/app.py` → clean (missed-re-import guard; `app.py` can't be imported in-process due to a pandas NumPy-2 ABI crash)
  4. `python -m pytest tests/python -q` → all green (117 baseline + new smoke tests; no prior test regresses)
  5. Playwright/Selenium integration tests green (`tests/python/test_app_playwright.py`) — the full-page render safety net
- **Render-smoke tests run in-process.** Fragment modules and their leaf deps do NOT import pandas, so `from shiny_app.ui_<group> import …` + `str(fragment())` works in a plain pytest (verified). This is both the per-fragment coverage AND the module-import check.

---

### Task 1: Phase 2a — `ui_scripts.py` (6 inline-JS fragments)

**Files:**
- Create: `shiny_app/ui_scripts.py`
- Create: `tests/python/test_ui_scripts.py`
- Modify: `shiny_app/app.py` (remove 6 script locals; add re-import; update `content` list entries)

**Interfaces:**
- Produces (all zero-arg, each returns `ui.tags.script(...)`):
  `reload_script()`, `nav_script()`, `settings_script()`, `help_script()`, `changelog_script()`, `theme_script()`
- Consumes: nothing (`from shiny import ui` only)

**Fragment → current source local (extract the `ui.tags.script("""…""")` verbatim):**

| Function | Current local | Marker token (present in rendered output) |
|---|---|---|
| `reload_script` | `reload_js` | `reload_page` |
| `nav_script` | `nav_js` | `initSidebar` |
| `settings_script` | `settings_js` | `settingsOffcanvas` |
| `help_script` | `help_js` | `helpOffcanvas` |
| `changelog_script` | `changelog_js` | `changelogOffcanvas` |
| `theme_script` | `theme_js` | `classList` |

- [ ] **Step 1: Write the failing smoke test** — `tests/python/test_ui_scripts.py`:

```python
from shiny_app.ui_scripts import (
    reload_script, nav_script, settings_script,
    help_script, changelog_script, theme_script,
)

_MARKERS = [
    (reload_script, "reload_page"),
    (nav_script, "initSidebar"),
    (settings_script, "settingsOffcanvas"),
    (help_script, "helpOffcanvas"),
    (changelog_script, "changelogOffcanvas"),
    (theme_script, "classList"),
]


def test_each_script_renders_a_script_tag_with_its_marker():
    for fn, marker in _MARKERS:
        html = str(fn())
        assert html.lstrip().startswith("<script"), f"{fn.__name__} is not a <script> tag"
        assert marker in html, f"{fn.__name__} missing marker {marker!r}"
```

- [ ] **Step 2: Run the test — verify it fails**

Run: `python -m pytest tests/python/test_ui_scripts.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'shiny_app.ui_scripts'`

- [ ] **Step 3: Create `shiny_app/ui_scripts.py`** — header + 6 functions. Move each `ui.tags.script("""…""")` body VERBATIM from the current `app.py` local into the function:

```python
"""Inline JavaScript blocks for the AQUABC UI (extracted from create_ui())."""
from shiny import ui


def reload_script():
    return ui.tags.script("""<<< verbatim body of reload_js >>>""")


def nav_script():
    return ui.tags.script("""<<< verbatim body of nav_js >>>""")


# … settings_script, help_script, changelog_script, theme_script — same pattern …
```

- [ ] **Step 4: Run the smoke test — verify it passes**

Run: `python -m pytest tests/python/test_ui_scripts.py -q`
Expected: PASS (1 test)

- [ ] **Step 5: Wire `app.py`** — three edits:
  1. Delete the 6 assignments `reload_js = ui.tags.script(…)`, `nav_js = …`, `settings_js = …`, `help_js = …`, `changelog_js = …`, `theme_js = …` from `create_ui()`.
  2. Add the re-import block right after the `diagnostics` import block (after `app.py:170`):

```python
# Import UI script fragments (phase-2 create_ui() split)
try:
    from shiny_app.ui_scripts import (
        reload_script, nav_script, settings_script,
        help_script, changelog_script, theme_script,
    )
except ImportError:
    from ui_scripts import (
        reload_script, nav_script, settings_script,
        help_script, changelog_script, theme_script,
    )
```

  3. In the `content = [...]` list, change each moved entry to its function call (order unchanged): `external_css, nav_script(), reload_script(), theme_script(), app_header, settings_offcanvas, settings_script(), help_offcanvas, help_script(), changelog_offcanvas, changelog_script(), sidebar_container`. (`external_css`, `app_header`, `settings_offcanvas`, `help_offcanvas`, `changelog_offcanvas`, `sidebar_container` remain locals — they move in Task 3.)

- [ ] **Step 6: Run the full per-phase gate**

```bash
python -m py_compile shiny_app/app.py
python -c "import shiny_app.ui_scripts"
ruff check --select F821 shiny_app/app.py
python -m pytest tests/python -q
python -m pytest tests/python/test_app_playwright.py -q   # full-page render
```
Expected: all pass; F821 clean; suite green.

- [ ] **Step 7: Commit**

```bash
git add shiny_app/ui_scripts.py tests/python/test_ui_scripts.py shiny_app/app.py
git commit -m "refactor(shiny): extract create_ui() JS blocks to ui_scripts.py (phase 2a)"
```

---

### Task 2: Phase 2b — `ui_panels.py` (14 content-panel fragments)

**Files:**
- Create: `shiny_app/ui_panels.py`
- Create: `tests/python/test_ui_panels.py`
- Modify: `shiny_app/app.py` (remove 14 panel locals; add re-import; update `main_content` entries)

**Interfaces:**
- Produces (each returns `ui.panel_conditional(...)`; arg-free unless noted):
  `panel_dashboard()`, `panel_model_build(compilers, build_types)`, `panel_model_control()`, `panel_input_files()`, `panel_parameters()`, `panel_initial_conditions()`, `panel_model_options()`, `panel_sim_config()`, `panel_scenarios()`, `panel_plot(min_smooth_window)`, `panel_mass_balance()`, `panel_observations()`, `panel_map()`, `panel_model_structure()`
- Consumes (module-top imports, via `try/except ImportError` fallback mirroring `app.py`):
  - `from shiny import ui`
  - `from shinywidgets import output_widget`  (used by `panel_input_files`, `panel_plot`, `panel_map`)
  - `simulation_config`: `TIME_STEP_PRESETS`, `OUTPUT_INTERVAL_PRESETS`  (`panel_model_control`)
  - `parameter_parser`: `PARAMETER_CATEGORIES`  (`panel_parameters`)
  - `ic_parser`: `STATE_VARIABLE_CATEGORIES`  (`panel_initial_conditions`)
  - `options_parser`: `OPTION_CATEGORIES`  (`panel_model_options`)
  - `input_analysis`: `get_input_file_categories`  (`panel_input_files`)

**Marker per panel = its `panel_conditional` condition string `nav_<x>`** (guaranteed present in rendered HTML):

| Function | Marker | | Function | Marker |
|---|---|---|---|---|
| `panel_dashboard` | `nav_dashboard` | | `panel_scenarios` | `nav_scenarios` |
| `panel_model_build` | `nav_model_build` | | `panel_plot` | `nav_plot` |
| `panel_model_control` | `nav_model_control` | | `panel_mass_balance` | `nav_mass_balance` |
| `panel_input_files` | `nav_input_files` | | `panel_observations` | `nav_observations` |
| `panel_parameters` | `nav_parameters` | | `panel_map` | `nav_map` |
| `panel_initial_conditions` | `nav_initial_conditions` | | `panel_model_structure` | `nav_model_structure` |
| `panel_model_options` | `nav_model_options` | | `panel_sim_config` | `nav_sim_config_disabled` |

- [ ] **Step 1: Write the failing smoke test** — `tests/python/test_ui_panels.py`:

```python
from shiny_app import ui_panels

# arg-free panels: name -> nav marker
ARGFREE = {
    "panel_dashboard": "nav_dashboard",
    "panel_model_control": "nav_model_control",
    "panel_input_files": "nav_input_files",
    "panel_parameters": "nav_parameters",
    "panel_initial_conditions": "nav_initial_conditions",
    "panel_model_options": "nav_model_options",
    "panel_sim_config": "nav_sim_config_disabled",
    "panel_scenarios": "nav_scenarios",
    "panel_mass_balance": "nav_mass_balance",
    "panel_observations": "nav_observations",
    "panel_map": "nav_map",
    "panel_model_structure": "nav_model_structure",
}


def test_argfree_panels_render_with_their_nav_marker():
    for name, marker in ARGFREE.items():
        html = str(getattr(ui_panels, name)())
        assert marker in html, f"{name} missing {marker!r}"


def test_panel_model_build_takes_consts_and_renders():
    compilers = {"gfortran": {"name": "GNU Fortran"}}
    build_types = {"release": {"name": "Release"}}
    html = str(ui_panels.panel_model_build(compilers, build_types))
    assert "nav_model_build" in html
    assert "GNU Fortran" in html and "Release" in html


def test_panel_plot_takes_min_smooth_window_and_renders():
    html = str(ui_panels.panel_plot(2))
    assert "nav_plot" in html
```

- [ ] **Step 2: Run the test — verify it fails**

Run: `python -m pytest tests/python/test_ui_panels.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'shiny_app.ui_panels'`

- [ ] **Step 3: Create `shiny_app/ui_panels.py`** — module header with the leaf-module imports (use the `try/except ImportError` fallback for EACH leaf import, mirroring `app.py:56–75` etc.), then the 14 functions. Move each `ui.panel_conditional(...)` body VERBATIM. For `panel_model_build`, replace the free names `COMPILERS`/`BUILD_TYPES` with the params `compilers`/`build_types`; for `panel_plot`, replace `MIN_SMOOTH_WINDOW` with the param `min_smooth_window`. Skeleton:

```python
"""Content-panel fragments for the AQUABC UI (extracted from create_ui())."""
from shiny import ui
from shinywidgets import output_widget   # third-party — plain import, mirrors app.py:42
try:
    from shiny_app.simulation_config import TIME_STEP_PRESETS, OUTPUT_INTERVAL_PRESETS
    from shiny_app.parameter_parser import PARAMETER_CATEGORIES
    from shiny_app.ic_parser import STATE_VARIABLE_CATEGORIES
    from shiny_app.options_parser import OPTION_CATEGORIES
    from shiny_app.input_analysis import get_input_file_categories
except ImportError:
    from simulation_config import TIME_STEP_PRESETS, OUTPUT_INTERVAL_PRESETS
    from parameter_parser import PARAMETER_CATEGORIES
    from ic_parser import STATE_VARIABLE_CATEGORIES
    from options_parser import OPTION_CATEGORIES
    from input_analysis import get_input_file_categories


def panel_dashboard():
    return ui.panel_conditional("input.navigation === 'nav_dashboard'", <<< verbatim body >>>)


def panel_model_build(compilers, build_types):
    # body verbatim from app.py, with COMPILERS->compilers, BUILD_TYPES->build_types
    return ui.panel_conditional("input.navigation === 'nav_model_build'", ...)


def panel_plot(min_smooth_window):
    # body verbatim, with MIN_SMOOTH_WINDOW->min_smooth_window (single use)
    return ui.panel_conditional("input.navigation === 'nav_plot'", ...)


# … the other 11 arg-free panels — verbatim bodies …
```

- [ ] **Step 4: Run the smoke test — verify it passes**

Run: `python -m pytest tests/python/test_ui_panels.py -q`
Expected: PASS (3 tests)

- [ ] **Step 5: Wire `app.py`** — three edits:
  1. Delete the 14 `panel_* = ui.panel_conditional(…)` assignments from `create_ui()`.
  2. Add the re-import block after the `ui_scripts` block:

```python
try:
    from shiny_app.ui_panels import (
        panel_dashboard, panel_model_build, panel_model_control, panel_input_files,
        panel_parameters, panel_initial_conditions, panel_model_options, panel_sim_config,
        panel_scenarios, panel_plot, panel_mass_balance, panel_observations,
        panel_map, panel_model_structure,
    )
except ImportError:
    from ui_panels import (
        panel_dashboard, panel_model_build, panel_model_control, panel_input_files,
        panel_parameters, panel_initial_conditions, panel_model_options, panel_sim_config,
        panel_scenarios, panel_plot, panel_mass_balance, panel_observations,
        panel_map, panel_model_structure,
    )
```

  3. In `main_content = ui.div({"class": "main-content"}, nav_input_hidden, …)`, change each panel entry to its call, ORDER UNCHANGED. Arg-free panels become `panel_x()`; the two arg-takers become `panel_model_build(COMPILERS, BUILD_TYPES)` and `panel_plot(MIN_SMOOTH_WINDOW)`. `nav_input_hidden` and `panel_diagnostics` stay as-is (`panel_diagnostics` is already `diagnostics_ui()`'s result).

- [ ] **Step 6: Run the full per-phase gate**

```bash
python -m py_compile shiny_app/app.py
python -c "import shiny_app.ui_panels"
ruff check --select F821 shiny_app/app.py
python -m pytest tests/python -q
python -m pytest tests/python/test_app_playwright.py -q
```
Expected: all pass; F821 clean.

- [ ] **Step 7: Commit**

```bash
git add shiny_app/ui_panels.py tests/python/test_ui_panels.py shiny_app/app.py
git commit -m "refactor(shiny): extract create_ui() content panels to ui_panels.py (phase 2b)"
```

---

### Task 3: Phase 2c — `ui_chrome.py` (sidebar / header / css / offcanvas)

**Files:**
- Create: `shiny_app/ui_chrome.py`
- Create: `tests/python/test_ui_chrome.py`
- Modify: `shiny_app/app.py` (remove chrome locals; add re-import; update `content` list + `sidebar_container`)

**Interfaces:**
- Produces:
  `build_sidebar(nav_choices)` (returns the `sidebar_content` div — folds in the `nav_links` loop), `app_header()`, `external_css()`, `settings_offcanvas()`, `help_offcanvas()`, `changelog_offcanvas()`
- Consumes: `from shiny import ui` only

**Marker per fragment:**

| Function | Marker |
|---|---|
| `build_sidebar` | `sidebar-nav` |
| `app_header` | `app-header` |
| `external_css` | `bootstrap-icons` |
| `settings_offcanvas` | `settingsOffcanvas` |
| `help_offcanvas` | `help_content` |
| `changelog_offcanvas` | `changelog_content` |

- [ ] **Step 1: Write the failing smoke test** — `tests/python/test_ui_chrome.py`:

```python
from shiny_app import ui_chrome

NAV_CHOICES = {
    "nav_dashboard": ("bi-speedometer2", "Dashboard"),
    "nav_plot": ("bi-graph-up", "Plot"),
}


def test_build_sidebar_renders_nav_links_from_choices():
    html = str(ui_chrome.build_sidebar(NAV_CHOICES))
    assert "sidebar-nav" in html
    assert "Dashboard" in html and "Plot" in html


def test_argfree_chrome_fragments_render_with_markers():
    cases = [
        (ui_chrome.app_header, "app-header"),
        (ui_chrome.external_css, "bootstrap-icons"),
        (ui_chrome.settings_offcanvas, "settingsOffcanvas"),
        (ui_chrome.help_offcanvas, "help_content"),
        (ui_chrome.changelog_offcanvas, "changelog_content"),
    ]
    for fn, marker in cases:
        assert marker in str(fn()), f"{fn.__name__} missing {marker!r}"
```

- [ ] **Step 2: Run the test — verify it fails**

Run: `python -m pytest tests/python/test_ui_chrome.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'shiny_app.ui_chrome'`

- [ ] **Step 3: Create `shiny_app/ui_chrome.py`** — header + 6 functions. `build_sidebar(nav_choices)` folds in the current `nav_links = []` + `for nav_id, (icon, label) in NAV_CHOICES.items(): nav_links.append(…)` loop (replacing `NAV_CHOICES` with the `nav_choices` param) and returns the current `sidebar_content = ui.div(…)` VERBATIM. The other 5 return their current locals verbatim:

```python
"""Chrome fragments (sidebar, header, css, offcanvas) for the AQUABC UI."""
from shiny import ui


def build_sidebar(nav_choices):
    nav_links = []
    for nav_id, (icon, label) in nav_choices.items():
        nav_links.append(<<< verbatim append body >>>)
    return ui.div(<<< verbatim sidebar_content body, using *nav_links >>>)


def app_header():
    return ui.div(<<< verbatim app_header body >>>)


# … external_css, settings_offcanvas, help_offcanvas, changelog_offcanvas — verbatim …
```

- [ ] **Step 4: Run the smoke test — verify it passes**

Run: `python -m pytest tests/python/test_ui_chrome.py -q`
Expected: PASS (2 tests)

- [ ] **Step 5: Wire `app.py`** — edits:
  1. Delete `nav_links`, `sidebar_content`, `app_header`, `external_css`, `settings_offcanvas`, `help_offcanvas`, `changelog_offcanvas` assignments from `create_ui()`.
  2. Add the re-import block after the `ui_panels` block:

```python
try:
    from shiny_app.ui_chrome import (
        build_sidebar, app_header, external_css,
        settings_offcanvas, help_offcanvas, changelog_offcanvas,
    )
except ImportError:
    from ui_chrome import (
        build_sidebar, app_header, external_css,
        settings_offcanvas, help_offcanvas, changelog_offcanvas,
    )
```

  3. `sidebar_container = ui.div(sidebar_content, main_content, …)` → change `sidebar_content` to `build_sidebar(NAV_CHOICES)`. In the `content` list, change `external_css`→`external_css()`, `app_header`→`app_header()`, `settings_offcanvas`→`settings_offcanvas()`, `help_offcanvas`→`help_offcanvas()`, `changelog_offcanvas`→`changelog_offcanvas()` (order unchanged). After this task `create_ui()` is the thin assembler: `nav_input`/`nav_input_hidden`, `main_content` (calls panels), `sidebar_container` (calls `build_sidebar`), `content` (calls scripts + chrome), `return ui.page_fillable(*content, title="AQUABC")`.

- [ ] **Step 6: Run the full per-phase gate**

```bash
python -m py_compile shiny_app/app.py
python -c "import shiny_app.ui_chrome"
ruff check --select F821 shiny_app/app.py
python -m pytest tests/python -q
python -m pytest tests/python/test_app_playwright.py -q
```
Expected: all pass; F821 clean.

- [ ] **Step 7: Update `TODO_IMPLEMENTATION_PLAN.md`** — mark TODO 2.1 phase-2 complete (create_ui() split shipped). Note remaining deferred roadmap (server() helper extraction, Shiny-modules).

- [ ] **Step 8: Commit**

```bash
git add shiny_app/ui_chrome.py tests/python/test_ui_chrome.py shiny_app/app.py TODO_IMPLEMENTATION_PLAN.md
git commit -m "refactor(shiny): extract create_ui() chrome to ui_chrome.py; create_ui() now thin (phase 2c)"
```

---

## Final verification (after all 3 tasks)

- [ ] `create_ui()` is ≤ ~60 lines (assembler only): `nav_input`, `nav_input_hidden`, `main_content`, `sidebar_container`, `content`, `return`.
- [ ] `git diff --stat` on `app.py` shows a net reduction of ~1,400 lines; the three new modules total ~1,530 lines.
- [ ] Full `pytest tests/python -q` green; Playwright/Selenium green; `ruff check --select F821 shiny_app/app.py` clean.
- [ ] Broad whole-branch review (subagent-driven-development final step) before merge.

## Notes on the verbatim-move steps

The plan intentionally does NOT reproduce the ~1,530 lines of moved UI content inline — doing so would risk transcription drift and defeat the verbatim-move guarantee. The implementer moves the ACTUAL bytes from the current `app.py` into each function body (the only edits: the `def …:` / `return` wrapper, and the 4 const-name→param substitutions in `panel_model_build`/`panel_plot`/`build_sidebar`). The task reviewer verifies each moved expression is character-identical to the removed lines, and the render-smoke + Playwright tests prove behavior is unchanged.
