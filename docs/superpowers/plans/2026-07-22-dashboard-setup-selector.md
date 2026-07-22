# Dashboard Setup Selector Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Surface the loadable-setup selector on the Dashboard landing page, kept bidirectionally in sync with the existing Run-Model-tab selector.

**Architecture:** A new `dash_setup_select` input on the Dashboard mirrors run_control's authoritative `setup_select` through the existing `session.root_scope().make_scope("run_control")` bridge, using two guarded `reactive.effect`s (each updates the *other* widget only when the value differs, so no loop). `run.current_setup` — a `reactive.calc` over run_control's `input.setup_select()` — stays the single source of truth and is not modified.

**Tech Stack:** Shiny for Python (`shiny.module`, `reactive`, `render`, `ui`), pytest. UI-only change; no Fortran, no model behavior.

## Global Constraints

- Do **not** modify `run_control.py`, `setups.py`, or `app_state.py`. All code changes are confined to `shiny_app/modules/dashboard.py` and `tests/python/test_dashboard_module.py`.
- Do **not** change `run.current_setup`'s definition or source. It remains a `reactive.calc` over run_control's `input.setup_select()`.
- Both setup widgets are created with `selected="standard"` (matching `current_setup`'s default) so they start in sync.
- Both sync effects read the *other* widget's current value under `reactive.isolate()` and call `ui.update_select` only when the value actually differs — this is the loop-safety guarantee; do not remove either guard.
- Cross-scope writes use the established pattern: `rc = session.root_scope().make_scope("run_control")` then `ui.update_select("setup_select", selected=..., session=rc)` (see `sim_config.py:155/214`, `model_build.py:356-357`, `plot.py:244/467`).
- Availability logic is not duplicated: the Dashboard notice reuses `setups.is_available()` and `st.unavailable_hint`, identical to run_control's `setup_availability` (`run_control.py:341-347`).
- Design reference: `docs/superpowers/specs/2026-07-22-dashboard-setup-selector-design.md`.

---

## File Structure

- `shiny_app/modules/dashboard.py` — **modify.** `dashboard_ui()` gains a compact Setup row; `dashboard_server()` gains the bridge, two sync effects, and the availability render. `setups` and `ROOT` are already imported/defined here (`:44/55`, `:67`).
- `tests/python/test_dashboard_module.py` — **modify.** Add a render assertion for the new namespaced ids and label. `nid()` helper already exists in this file.

---

### Task 1: Dashboard setup-selector UI + availability notice

Add the `Setup:` selector row and its availability notice to the Dashboard. After this task the widget renders and its notice reflects `run.current_setup()`; the widget does not yet *drive* the setup (that is Task 2).

**Files:**
- Modify: `shiny_app/modules/dashboard.py` (`dashboard_ui()` around `:110-112`; add `dash_setup_availability` render in `dashboard_server()`)
- Test: `tests/python/test_dashboard_module.py`

**Interfaces:**
- Consumes: `setups.list_setups()`, `setups.is_available(st, ROOT)`, `st.name`, `st.unavailable_hint` (all existing); `run.current_setup()` (existing `reactive.calc`, assigned by run_control).
- Produces: DOM ids `dashboard-dash_setup_select` (input_select) and `dashboard-dash_setup_availability` (output_ui) rendered by `dashboard_ui`. Task 2's effects target the bare id `dash_setup_select`.

- [ ] **Step 1: Write the failing test**

Add to `tests/python/test_dashboard_module.py`:

```python
def test_dashboard_ui_includes_setup_selector():
    """The Dashboard surfaces the loadable-setup selector + its availability notice."""
    html = str(dashboard_ui("dashboard"))
    assert nid("dashboard", "dash_setup_select") in html, "missing dashboard setup selector"
    assert nid("dashboard", "dash_setup_availability") in html, "missing setup availability output"
    assert "Setup:" in html, "missing Setup: label"
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `python -m pytest tests/python/test_dashboard_module.py::test_dashboard_ui_includes_setup_selector -v`
Expected: FAIL — `missing dashboard setup selector` (the ids are not in the rendered HTML yet).

- [ ] **Step 3: Add the Setup row to `dashboard_ui()`**

In `shiny_app/modules/dashboard.py`, insert the new row between the status-bar `div` (which closes at `:110`) and the `# Two-column layout` comment / `ui.layout_columns(` at `:111-112`. Anchor on the comment line:

```python
        ),
        # Setup selection — surfaced on the landing page, synced to the Run Model tab
        ui.div(
            {"class": "dashboard-setup-row", "style": "max-width: 460px; margin-bottom: 0.75rem;"},
            ui.input_select("dash_setup_select", "Setup:",
                            choices={s.id: s.name for s in setups.list_setups()},
                            selected="standard"),
            ui.output_ui("dash_setup_availability"),
        ),
        # Two-column layout: actions + system | run log
        ui.layout_columns(
```

(The first `),` shown is the existing closer of the status-bar `div` at `:110` — do not duplicate it; insert the new `ui.div(...)` block and the trailing `# Two-column layout` comment stays as-is.)

- [ ] **Step 4: Add the availability render to `dashboard_server()`**

In `dashboard_server`, add the render (place it with the other `@render.ui` definitions; it reads the existing `run.current_setup()` and `ROOT`):

```python
    @render.ui
    def dash_setup_availability():
        st = run.current_setup()
        if setups.is_available(st, ROOT):
            return ui.TagList()
        return ui.div(
            ui.tags.small(f"⚠ Inputs for “{st.name}” not found. {st.unavailable_hint}"),
            class_="text-warning",
        )
```

- [ ] **Step 5: Run the new test + the render backstop to verify they pass**

Run: `python -m pytest tests/python/test_dashboard_module.py -v`
Expected: PASS (all tests, including the new one).

Run: `python -c "import shiny_app.app; shiny_app.app.create_ui().tagify()"`
Expected: no exception (full app UI renders with the new row).

- [ ] **Step 6: Lint**

Run: `ruff check shiny_app/modules/dashboard.py tests/python/test_dashboard_module.py`
Expected: no findings.

- [ ] **Step 7: Commit**

```bash
git add shiny_app/modules/dashboard.py tests/python/test_dashboard_module.py
git commit -m "feat(dashboard): surface the setup selector + availability notice on the landing page"
```

---

### Task 2: Dashboard↔run_control setup sync wiring

Wire the Dashboard selector to drive and mirror run_control's authoritative `setup_select` via the make_scope bridge and two guarded effects. After this task, picking a setup in either place keeps both — and `run.current_setup` — consistent.

**Files:**
- Modify: `shiny_app/modules/dashboard.py` (`dashboard_server()`, immediately after `run = state.run` at `:175`)

**Interfaces:**
- Consumes: `input.dash_setup_select()` (added in Task 1); `run.current_setup()` (existing); run_control's `setup_select` input, reached via `rc = session.root_scope().make_scope("run_control")` → `rc.input.setup_select()` (read) and `ui.update_select("setup_select", ..., session=rc)` (write).
- Produces: two-way sync — no new public interface.

- [ ] **Step 1: Add the bridge + two sync effects**

In `shiny_app/modules/dashboard.py`, insert immediately after `run = state.run` (`:175`), before the `# =================` / `# Log Copy Handlers` block (`:177`):

```python
    # ---------------------------------------------------------------
    # Setup selector sync: Dashboard <-> Run Model tab.
    # Single source of truth is run.current_setup (a reactive.calc over
    # run_control's input.setup_select). The Dashboard's dash_setup_select
    # is a mirror kept in sync through the run_control scope bridge.
    # Each effect updates the OTHER widget only when the value differs,
    # so a change converges in one hop and never loops.
    # ---------------------------------------------------------------
    rc = session.root_scope().make_scope("run_control")

    @reactive.effect
    def _dash_setup_to_run_control():
        """Dashboard selector drives run_control's authoritative setup_select."""
        chosen = input.dash_setup_select()
        if not chosen:
            return
        with reactive.isolate():
            current = rc.input.setup_select()
        if chosen != current:
            ui.update_select("setup_select", selected=chosen, session=rc)

    @reactive.effect
    def _run_control_to_dash_setup():
        """Mirror the authoritative current setup back into the Dashboard selector."""
        target = run.current_setup().id
        with reactive.isolate():
            shown = input.dash_setup_select()
        if target != shown:
            ui.update_select("dash_setup_select", selected=target)
```

- [ ] **Step 2: Verify the full app renders and the suite is green**

Run: `python -c "import shiny_app.app; shiny_app.app.create_ui().tagify()"`
Expected: no exception.

Run: `python -m pytest tests/python -q`
Expected: PASS — no regressions (the reactive effects have no static-render unit test; they are exercised by the manual smoke in Step 4 and guarded against import/render breakage by the backstop above).

- [ ] **Step 3: Lint**

Run: `ruff check shiny_app/modules/dashboard.py`
Expected: no findings.

- [ ] **Step 4: Manual Playwright smoke (record result in the commit/PR, not CI)**

Start the app, then verify the two-way sync and the notice:
1. Launch: `python -m shiny run shiny_app/app.py --port 8000` (or the project's usual launch).
2. On the **Dashboard**, set the Setup selector to **CL29 (29-box Curonian Lagoon)**. Navigate to **Model Config → Run Model** and confirm its **Setup:** dropdown now reads CL29.
3. On the **Run Model** tab, switch back to **Standard**. Return to the **Dashboard** and confirm its selector now reads Standard.
4. Select an unavailable setup (one whose `required_input` is absent) and confirm the Dashboard shows the ⚠ availability notice, matching the Run Model tab.

Expected: both selectors track each other in both directions with no flicker/oscillation; the availability notice matches.

- [ ] **Step 5: Commit**

```bash
git add shiny_app/modules/dashboard.py
git commit -m "feat(dashboard): sync the Dashboard setup selector with the Run Model tab

Two guarded reactive effects over the make_scope(\"run_control\") bridge keep
dash_setup_select and run_control's setup_select mirrored; run.current_setup
stays the single source of truth."
```

---

## Self-Review

**Spec coverage:**
- Two-effect mirror over make_scope bridge → Task 2, Step 1. ✅
- `run.current_setup` unchanged / run_control untouched → Global Constraints + no task modifies those files. ✅
- Placement between status bar and action row → Task 1, Step 3. ✅
- Availability notice reusing `setups` helpers → Task 1, Step 4. ✅
- Loop-safety (isolate + differ-guard) → Task 2, Step 1 + Global Constraints. ✅
- Testing: render assertion + tagify backstop + manual Playwright → Task 1 Steps 1/5, Task 2 Steps 2/4. ✅

**Placeholder scan:** No TBD/TODO; every code step shows the actual code. ✅

**Type consistency:** `dash_setup_select` / `dash_setup_availability` (bare ids) and `setup_select` (run_control bare id) are used identically across UI, test, and effects. `run.current_setup().id` and `.name` / `.unavailable_hint` match the `Setup` dataclass used in `run_control.py`. ✅
