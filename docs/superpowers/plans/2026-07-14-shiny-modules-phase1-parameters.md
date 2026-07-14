# Shiny-Modules Rearchitecture — Phase 1 (Pilot: `parameters` module) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convert the `parameters` tab into the first real `@module.ui`/`@module.server` Shiny module — establishing the file layout, the namespaced-ID test convention (`nid()`), and the client-side `navigate()` mechanism that all later modules reuse.

**Architecture:** Create `shiny_app/modules/parameters.py` with `parameters_ui()` (`@module.ui`, returns the panel *content* — no `panel_conditional`) and `parameters_server()` (`@module.server`, the ported handlers + private reactive state). `create_ui()` keeps the `panel_conditional` at app level, wrapping `parameters_ui("parameters")`; `server()` deletes the old inline handlers and calls `parameters_server("parameters", state)` once. Within-tab IDs namespace to `parameters-*`; the `nav_parameters` nav id stays global. Also upgrades `state.navigate` from the Phase-0 `ui.update_radio_buttons` to `session.send_custom_message` + a `nav_script` handler (a namespaced module can't reach the global `navigation` input by id).

**Tech Stack:** Python 3.10+, Shiny for Python 1.5.x (`shiny.module`, `shiny.reactive`), pytest, Playwright + Selenium (CI-only) integration tests.

**Spec:** `docs/superpowers/specs/2026-07-14-app-py-shiny-modules-rearchitecture-design.md` (§4 module pattern, §7 Phase 1). Builds on Phase 0 (`v0.4.0`, `shiny_app/app_state.py`).

## Global Constraints

- **Behavior-identical:** the parameters tab's load/edit/save flow is unchanged; only within-tab widget IDs gain the `parameters-` namespace prefix. The `nav_parameters` nav id and all other tabs are untouched.
- **Module pattern (the convention this pilot sets):** `x_ui(id)` returns panel *content* (no `panel_conditional`, no nav knowledge); the `panel_conditional("input.navigation === 'nav_x'", x_ui("x"))` wrapper lives in `create_ui()`. `x_server(id, state)` takes the shared `state` even when it uses nothing from it (uniform call convention).
- **Self-contained modules:** `shiny_app/modules/*.py` import only stdlib + already-extracted leaf modules (`parameter_parser`, …) + `shiny_app/app_state.py`, never `app.py`. Use the import-fallback pattern (`try: from shiny_app.<mod> import … / except ImportError: from <mod> import …`) and `logging.getLogger("AQUABC")`.
- **Verbatim ports:** the 5 handler bodies and 3 reactive values move from `app.py:1932-2084` into the module unchanged (no logic edits).
- **Lint-clean:** `ruff check shiny_app/modules/ tests/python/` must pass (the modules subpackage and tests are lint-gated; `app.py` is not).
- **Integration tests are CI-only** (playwright/selenium absent locally) — the namespaced-selector updates are verified by CI on push; locally, namespacing is proved by a render-smoke unit test + boot smoke.
- **Commit per task.**

## File Structure

- **Create** `shiny_app/modules/__init__.py` — empty; makes `modules` a subpackage.
- **Create** `shiny_app/modules/parameters.py` — `parameters_ui()` + `parameters_server()`. One responsibility: the parameters tab. Self-contained (imports `parameter_parser`, self-computes `INPUTS_DIR`).
- **Create** `tests/python/nsutil.py` — `nid(module_id, input_id) -> str` namespaced-id helper (used by unit + integration tests).
- **Create** `tests/python/test_parameters_module.py` — render-smoke test asserting `parameters_ui("parameters")` emits namespaced IDs.
- **Modify** `shiny_app/app.py` — import the module; `create_ui()` swaps `panel_parameters()` → `panel_conditional(..., parameters_ui("parameters"))`; `server()` deletes the parameters handlers + reactive values and calls `parameters_server("parameters", state)`; `navigate()` upgraded to async `send_custom_message` and the two goto handlers made async.
- **Modify** `shiny_app/ui_panels.py` — delete `panel_parameters()`.
- **Modify** `shiny_app/ui_scripts.py` — add the `aquabc_navigate` custom-message handler to `nav_script`.
- **Modify** `shiny_app/app_state.py` — `AppState.navigate` type → `Callable[[str], Awaitable[None]]`.
- **Modify** `tests/python/test_app_playwright.py`, `tests/python/test_app_selenium.py` — migrate the ~6 parameters selectors to `parameters-*`.

---

### Task 1: The `parameters` module + `nid()` helper + render-smoke test

**Files:**
- Create: `shiny_app/modules/__init__.py`, `shiny_app/modules/parameters.py`
- Create: `tests/python/nsutil.py`, `tests/python/test_parameters_module.py`

**Interfaces:**
- Consumes: `parameter_parser` (`ParameterFile`, `PARAMETER_CATEGORIES`); `shiny_app/app_state.py` `AppState` (type only — the server takes `state` but uses nothing from it this tab).
- Produces: `parameters_ui()` (`@module.ui`; call as `parameters_ui("parameters")` → panel content with `parameters-`-namespaced ids) and `parameters_server()` (`@module.server`; call as `parameters_server("parameters", state)`); `nid(module_id, input_id) -> f"{module_id}-{input_id}"`.

- [ ] **Step 1: Write the `nid` helper + the failing render-smoke test**

```python
# tests/python/nsutil.py
def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"
```

```python
# tests/python/test_parameters_module.py
from nsutil import nid

try:
    from shiny_app.modules.parameters import parameters_ui, parameters_server
except ImportError:
    from modules.parameters import parameters_ui, parameters_server


def test_parameters_ui_namespaces_ids():
    html = str(parameters_ui("parameters"))
    # within-tab widgets get the "parameters-" prefix; the panel_conditional/nav is NOT here
    for raw in ("param_file", "param_category", "load_params", "save_params",
                "param_category_info", "param_table", "param_save_status"):
        assert nid("parameters", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html


def test_parameters_server_is_module_server():
    # the decorated server is callable with (id, state); we don't run a session here,
    # just assert it's the module-wrapped server (has the shiny module marker) and importable.
    assert callable(parameters_server)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `.venv/bin/python -m pytest tests/python/test_parameters_module.py -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'shiny_app.modules'`.

- [ ] **Step 3: Create the module package + `parameters.py`**

```python
# shiny_app/modules/__init__.py
```
(empty file)

```python
# shiny_app/modules/parameters.py
"""Parameters tab as a true Shiny module (Phase 1 pilot).

`parameters_ui(id)` returns the panel *content* (the app-level panel_conditional
stays in create_ui); `parameters_server(id, state)` registers the handlers. Both
ids namespace to `parameters-*`. Self-contained: imports parameter_parser and
self-computes INPUTS_DIR; imports nothing from app.py.
"""
import logging
import os
from datetime import datetime

from shiny import module, reactive, render, ui

try:
    from shiny_app.parameter_parser import ParameterFile, PARAMETER_CATEGORIES
except ImportError:  # running as a script from inside shiny_app/
    from parameter_parser import ParameterFile, PARAMETER_CATEGORIES

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
INPUTS_DIR = os.path.join(ROOT, "INPUTS")


@module.ui
def parameters_ui():
    return ui.card(
        ui.card_header("Parameters"),
        ui.layout_columns(
            ui.tooltip(
                ui.input_select("param_file", "Constants file:",
                                choices=["WCONST_04.txt"], selected="WCONST_04.txt"),
                "WCONST_04.txt contains calibrated model parameters",
            ),
            ui.tooltip(
                ui.input_select("param_category", "Category:",
                                choices=list(PARAMETER_CATEGORIES.keys()), selected="Diatoms"),
                "Select parameter category: Diatoms, Cyanobacteria, Zooplankton, etc.",
            ),
            ui.tooltip(
                ui.input_action_button("load_params", "Load", class_="btn-secondary mt-4"),
                "Load parameters from selected file and category",
            ),
            col_widths=[3, 7, 2],
        ),
        ui.tags.hr(),
        ui.div(
            ui.output_text("param_category_info"),
            style="font-size: 0.78rem; padding: 0.4rem 0.75rem; background: rgba(14, 165, 233, 0.04); border-radius: 4px; margin-bottom: 0.75rem; border: 1px solid rgba(14, 165, 233, 0.1);",
        ),
        ui.card(
            ui.card_header("Parameters"),
            ui.output_ui("param_table"),
            style="max-height: 550px; overflow-y: auto;",
        ),
        ui.layout_columns(
            ui.tooltip(
                ui.input_action_button("save_params", "Save All Changes", class_="btn-success"),
                "Save modified parameters to file (creates backup)",
            ),
            ui.output_text("param_save_status"),
            col_widths=[3, 9],
        ),
    )


@module.server
def parameters_server(input, output, session, state):
    # `state` is accepted for the uniform x_server(id, state) convention; the
    # parameters tab is self-contained and uses nothing from it.
    param_file_obj = reactive.Value(None)
    param_modified = reactive.Value({})  # param_id -> new_value
    param_save_msg = reactive.Value("")

    @reactive.effect
    @reactive.event(input.load_params, input.param_category, input.param_file)
    def load_param_file():
        """Load parameter file when category or file changes"""
        param_filename = input.param_file()
        if not param_filename:
            return
        filepath = os.path.join(INPUTS_DIR, param_filename)
        if not os.path.exists(filepath):
            logger.error(f"Parameter file not found: {filepath}")
            return
        logger.info(f"Loading parameter file: {param_filename}")
        pf = ParameterFile(filepath)
        if pf.parse():
            param_file_obj.set(pf)
            param_modified.set({})
            param_save_msg.set("")
            logger.info(f"Loaded {len(pf.parameters)} parameters")
        else:
            logger.error("Failed to parse parameter file")

    @render.text
    def param_category_info():
        """Display category information"""
        category = input.param_category()
        pf = param_file_obj.get()
        if not category:
            return "Select a category"
        if category in PARAMETER_CATEGORIES:
            start, end = PARAMETER_CATEGORIES[category]
            count = end - start + 1
            info = f"Category: {category}\n"
            info += f"Parameters: {count} ({start}-{end})\n"
            if pf:
                params = pf.get_parameters_by_category(category)
                info += f"Loaded: {len(params)} parameters"
            return info
        return "Unknown category"

    @render.ui
    def param_table():
        """Render parameter table for editing"""
        category = input.param_category()
        pf = param_file_obj.get()
        if not pf:
            return ui.tags.div(
                ui.tags.p("Click 'Load Parameters' to load the parameter file", class_="text-muted"),
                class_="mt-2",
            )
        params = pf.get_parameters_by_category(category)
        if not params:
            return ui.tags.p(f"No parameters found for category: {category}", class_="text-warning")
        param_inputs = []
        for p in params:
            param_row = ui.tags.div(
                ui.tags.div(
                    ui.tags.strong(p.name, class_="small"),
                    ui.tags.br(),
                    ui.tags.small(p.comment[:60] + "..." if len(p.comment) > 60 else p.comment, class_="text-muted"),
                    class_="col-7",
                ),
                ui.tags.div(
                    ui.input_numeric(f"param_{p.id}", "", value=p.value, width="100%"),
                    class_="col-5",
                ),
                class_="row mb-2 align-items-center border-bottom pb-2",
            )
            param_inputs.append(param_row)
        return ui.tags.div(
            ui.tags.div(
                ui.tags.small(f"Showing {len(params)} parameters", class_="text-muted"),
                class_="mb-2",
            ),
            *param_inputs,
            style="max-height: 400px; overflow-y: auto;",
        )

    @reactive.effect
    @reactive.event(input.save_params)
    def save_parameters():
        """Save modified parameters"""
        pf = param_file_obj.get()
        if not pf:
            param_save_msg.set("Error: No parameter file loaded")
            return
        category = input.param_category()
        params = pf.get_parameters_by_category(category)
        updates = {}
        for p in params:
            input_id = f"param_{p.id}"
            try:
                new_value = input[input_id]()
                if new_value is not None and new_value != p.value:
                    updates[p.id] = float(new_value)
            except Exception as e:
                logger.debug(f"Could not get value for {input_id}: {e}")
        if not updates:
            param_save_msg.set("No changes to save")
            return
        logger.info(f"Saving {len(updates)} parameter changes")
        success_count, fail_count, messages = pf.update_parameters(updates)
        save_ok, save_msg = pf.save(backup=True)
        if save_ok:
            param_save_msg.set(f"Saved {success_count} changes at {datetime.now().strftime('%H:%M:%S')}")
            ui.notification_show(f"Successfully saved {success_count} parameter changes", type="message", duration=3)
        else:
            param_save_msg.set(f"Save failed: {save_msg}")
            ui.notification_show(f"Failed to save parameters: {save_msg}", type="error", duration=5)

    @render.text
    def param_save_status():
        """Display save status"""
        return param_save_msg.get()
```

- [ ] **Step 4: Run test + ruff**

Run: `.venv/bin/python -m pytest tests/python/test_parameters_module.py -v && .venv/bin/python -m pytest tests/python -q`
Expected: the 2 new tests PASS; full suite still **165 passed** (module not yet wired in — app.py unchanged).
Run: `~/.local/bin/ruff check shiny_app/modules/ tests/python/nsutil.py tests/python/test_parameters_module.py`
Expected: All checks passed.

- [ ] **Step 5: Commit**

```bash
git add shiny_app/modules/ tests/python/nsutil.py tests/python/test_parameters_module.py
git commit -m "feat(shiny): parameters Shiny module (module.ui/server) + nid test helper"
```

---

### Task 2: Wire the module into the app + migrate the parameters test selectors

**Files:**
- Modify: `shiny_app/app.py` (import ~line 211; `create_ui()` @503; delete parameters handlers @1932-2084; add server call)
- Modify: `shiny_app/ui_panels.py` (delete `panel_parameters()` @573-626)
- Modify: `tests/python/test_app_playwright.py`, `tests/python/test_app_selenium.py` (parameters selectors)

**Interfaces:**
- Consumes: `parameters_ui`, `parameters_server` (Task 1); `state` (the Phase-0 `AppState`, already constructed in `server()`).

- [ ] **Step 1: Import the module** (`app.py`, near the other `shiny_app.*` imports ~line 211)

```python
try:
    from shiny_app.modules.parameters import parameters_ui, parameters_server
except ImportError:
    from modules.parameters import parameters_ui, parameters_server
```

- [ ] **Step 2: Swap the panel in `create_ui()`** — replace the `panel_parameters(),` entry (`app.py:503`) in the `main_content` list with:

```python
        ui.panel_conditional("input.navigation === 'nav_parameters'", parameters_ui("parameters")),
```

- [ ] **Step 3: Delete `panel_parameters()` from `ui_panels.py`** (@573-626, the whole function). Confirm nothing else in `ui_panels.py`/`app.py` still calls `panel_parameters` (grep).

- [ ] **Step 4: Replace the inline parameters server code with the module call** — delete the block `app.py:1930-2086` (the `# ===== PARAMETER EDITOR =====` comment through `# ===== END PARAMETER EDITOR =====`: the 3 reactive values + `load_param_file`/`param_category_info`/`param_table`/`save_parameters`/`param_save_status`) and put in its place:

```python
    # Parameters tab is a Shiny module (Phase 1)
    parameters_server("parameters", state)
```

- [ ] **Step 5: Migrate the parameters integration-test selectors** (namespaced ids; `nav_parameters` stays):

- `tests/python/test_app_selenium.py`: `(By.ID, "load_params")` → `(By.ID, "parameters-load_params")`; `driver.find_element(By.ID, "param_category")` → `(By.ID, "parameters-param_category")`.
- `tests/python/test_app_playwright.py`: `page.locator("#load_params")` → `page.locator("#parameters-load_params")` (both occurrences, ~lines 85 and 215); `page.locator("#param_category")` → `page.locator("#parameters-param_category")` (both, ~lines 86 and 220).
- Leave the `navigate_to(…, "nav_parameters")` calls and the `"nav_parameters"` nav-link checks unchanged (nav ids are global).

- [ ] **Step 6: Verify (local)**

Run: `.venv/bin/python -m py_compile shiny_app/app.py shiny_app/ui_panels.py`
Run: `grep -nE "panel_parameters|def load_param_file|def param_table|def save_parameters|param_file_obj" shiny_app/app.py shiny_app/ui_panels.py` → **empty** (old handlers/panel gone).
Run: `.venv/bin/python -m pytest tests/python -q` → all pass (165 + the 2 module tests from Task 1 = **167**).
Run: `.venv/bin/python -m py_compile tests/python/test_app_playwright.py tests/python/test_app_selenium.py` (selector edits compile).

- [ ] **Step 7: Boot smoke** — launch the app and confirm the parameters tab renders with namespaced ids and no server error:

```bash
.venv/bin/python -m shiny run --port 5099 shiny_app/app.py &   # (run in background)
# then, after it serves:
curl -s http://127.0.0.1:5099/ | grep -c "parameters-param_category"   # expect >= 1
```
Then drive a websocket session (as in Phase 0's smoke) to confirm `server()` + `parameters_server` register with no traceback in the log. Stop the server.

- [ ] **Step 8: Commit**

```bash
git add shiny_app/app.py shiny_app/ui_panels.py tests/python/test_app_playwright.py tests/python/test_app_selenium.py
git commit -m "refactor(shiny): wire parameters module into app; migrate its integration selectors to parameters-*"
```

---

### Task 3: Upgrade `navigate()` to `send_custom_message` + nav-JS handler

**Files:**
- Modify: `shiny_app/app.py` (`state.navigate` construction @556; `navigate_to_build` @995; `navigate_to_model_config` @1001)
- Modify: `shiny_app/ui_scripts.py` (`nav_script`)
- Modify: `shiny_app/app_state.py` (`AppState.navigate` type)

**Interfaces:**
- Consumes: `state.navigate` (now async). Produces: a namespace-independent nav mechanism reusable by future modules (Phase 4's dashboard goto buttons).

**Why now:** Phase 0's `navigate = ui.update_radio_buttons("navigation", …)` works only from the app-level (global) namespace. A converted module cannot reach the global `navigation` input that way; `send_custom_message` (a client-global message) can. Proving it here — on the still-app-level goto buttons — de-risks Phase 4.

- [ ] **Step 1: Define an async `navigate` and pass it to `AppState`** — in `server()`, before `state = AppState(...)`, add:

```python
    async def _navigate(nav_id):
        await session.send_custom_message("aquabc_navigate", {"navId": nav_id})
```
and change the `AppState(...)` `navigate=` argument (currently the lambda at `app.py:556`) to `navigate=_navigate`.

- [ ] **Step 2: Make the two goto handlers async** (`app.py:995`, `1001`):

```python
    @reactive.effect
    @reactive.event(input.goto_build)
    async def navigate_to_build():
        await state.navigate("nav_model_build")

    @reactive.effect
    @reactive.event(input.goto_model_config)
    async def navigate_to_model_config():
        await state.navigate("nav_model_control")
```

- [ ] **Step 3: Add the client handler to `nav_script`** — inside the existing `<script>` (after the click-handler wiring that already calls `Shiny.setInputValue('navigation', navId)`), add:

```javascript
Shiny.addCustomMessageHandler('aquabc_navigate', function(msg) {
    var navId = msg.navId;
    Shiny.setInputValue('navigation', navId);
    document.querySelectorAll('.custom-sidebar .nav-link').forEach(function(el) {
        el.classList.toggle('active', el.getAttribute('data-nav-id') === navId);
    });
});
```
(This sets the nav input and the active-link highlight — the same effect as a user click; the panel still switches via `panel_conditional` on `input.navigation`.)

- [ ] **Step 4: Update the `AppState.navigate` type** (`shiny_app/app_state.py`): change `navigate: Callable[[str], None]` → `navigate: Callable[[str], Awaitable[None]]` and add `Awaitable` to the `from collections.abc import Callable` import (→ `from collections.abc import Awaitable, Callable`).

- [ ] **Step 5: Verify**

Run: `.venv/bin/python -m py_compile shiny_app/app.py && .venv/bin/python -m pytest tests/python -q` → 167 pass.
Run: `~/.local/bin/ruff check shiny_app/app_state.py tests/python/` → All checks passed.
Boot smoke: launch the app, curl `/` and confirm `aquabc_navigate` appears in the served HTML (the handler registered); a websocket session runs `server()` with no traceback.

- [ ] **Step 6: Commit**

```bash
git add shiny_app/app.py shiny_app/ui_scripts.py shiny_app/app_state.py
git commit -m "feat(shiny): navigate() via send_custom_message + nav_script handler; goto handlers async"
```

---

### Task 4: Phase-1 regression gate

**Files:** none changed — verification gate.

- [ ] **Step 1: Static + unit**

Run: `.venv/bin/python -m py_compile shiny_app/app.py && .venv/bin/python -c "import shiny_app.app; import shiny_app.modules.parameters"` → clean.
Run: `.venv/bin/python -m pytest tests/python -q` → **167 passed**.
Run: `~/.local/bin/ruff check shiny_app/modules/ tests/python/` → All checks passed.

- [ ] **Step 2: Confirm the old parameters code is fully gone**

Run: `grep -nE "def load_param_file|def param_table|def save_parameters|param_file_obj|panel_parameters" shiny_app/app.py shiny_app/ui_panels.py` → **empty**.

- [ ] **Step 3: Boot smoke (behavior)**

Launch the app; confirm: it serves 200 with the full nav shell; the Parameters tab renders (`parameters-param_category` present in the HTML); a websocket session runs `server()` end-to-end with no traceback; the goto buttons drive navigation (observe, via the `aquabc_navigate` path). Stop the server.

- [ ] **Step 4: Integration tests (the DOM net — runs on push)**

Locally these are CI-only. On push, confirm the `integration-tests` CI job stays green (it drives the real `parameters-load_params`/`parameters-param_category` flow) — the authoritative behavior-identical proof for the namespaced tab.

- [ ] **Step 5: (Deferred) release** — `v0.4.1` (`chore(release)` + CHANGELOG + tag + push) is done at the finishing/release step, not here, per the Phase-0 pattern (user chooses when to release).

---

## Self-Review

**Spec coverage (§4/§7 Phase 1 → tasks):** `@module.ui`/`@module.server` conversion of `parameters` → Task 1; `panel_conditional` stays in `create_ui`, module UI is content-only, `x_server(id, state)` convention → Tasks 1-2; `nid()` helper + namespaced-selector migration → Tasks 1-2 Step 5; `navigate()` → `send_custom_message` + nav-JS + async goto → Task 3; file layout (`shiny_app/modules/`) → Task 1; regression + CI DOM net → Task 4.

**Placeholder scan:** the module UI/server and tests are full code; the app.py deletions name exact line ranges + the grep-clean gate; the selector edits enumerate each site. No TBD.

**Type consistency:** `parameters_ui("parameters")` (Task 2 Step 2) matches the `@module.ui` def (Task 1). `parameters_server("parameters", state)` (Task 2 Step 4) matches the `@module.server def parameters_server(input, output, session, state)` (Task 1). `state.navigate` is async (Task 3 Step 1) and every caller `await`s it (Task 3 Step 2); the `AppState.navigate` annotation is updated to match (Task 3 Step 4).

**Notes / carried risks:**
- **No standalone module-server unit test:** py-shiny has no clean per-module `server` test harness, and the 5 handler bodies are verbatim ports whose logic (`ParameterFile`, `PARAMETER_CATEGORIES`) is already unit-tested (`test_parameter_parser.py`). Coverage = render-smoke (namespacing) + boot smoke (server registers) + CI integration (the real flow). Called out so a reviewer doesn't read the absence as a gap.
- **Dynamic `param_{p.id}` inputs:** created in `param_table` and read via `input[f"param_{p.id}"]()` in `save_parameters` — both inside the module, so both namespace consistently to `parameters-param_{id}`. Verbatim; verified end-to-end by the CI integration save-flow.
- **`navigate()` active-link highlight:** the new `aquabc_navigate` handler sets the sidebar active-link in addition to the nav input (matching a real nav click). If the Phase-0 `update_radio_buttons` goto did *not* highlight the link, this is a minor visible improvement, not a regression; the integration tests assert panel *content*, not the highlight.
- **`param_modified` reactive value** is set but never read (pre-existing dead state) — ported verbatim; not cleaned up here (out of scope for a behavior-identical move).
