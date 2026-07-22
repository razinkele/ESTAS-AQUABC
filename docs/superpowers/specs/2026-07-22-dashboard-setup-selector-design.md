# Dashboard Setup Selector — Design

**Status:** Approved (2026-07-22)
**Author:** AQUABC maintainers
**Scope:** Shiny app UI only. No Fortran, no model behavior.

## Problem

The loadable-setup registry (`shiny_app/setups.py`, shipped v0.6.0) lets a user
pick which application to run — **Standard** (25-box) vs **CL29** (29-box
Curonian Lagoon) vs **CL29 (2023 climatology)**. That choice is the single
control that drives the input file, the box count, the output directory, and the
`ESTAS_HOLD_VOLUME` environment injection.

But the selector lives only at **Model Config → Run Model tab → "Setup:"** —
behind a non-default tab. Users land on the **Dashboard**, see **Quick Run**, and
cannot find where to switch between the 25-box and 29-box models. The setup choice
is undiscoverable from the page where a run is actually launched.

## Goal

Surface the setup selector on the Dashboard landing page, kept bidirectionally in
sync with the existing Run-Model-tab selector, so both drive the one source of
truth and either can change the setup.

## Non-goals (YAGNI)

- Do **not** change the run_control selector or its behavior.
- Do **not** change `run.current_setup`'s source (it stays a `reactive.calc` over
  run_control's `input.setup_select()`).
- Do **not** duplicate availability *logic* — both selectors call the same
  `setups` helpers (`is_available`, `unavailable_hint`).
- No new setups, no registry changes.

## Background: the single source of truth

`run.current_setup` is a `@reactive.calc` defined in `run_control_server`
(`shiny_app/modules/run_control.py:336-339`):

```python
@reactive.calc
def _current_setup():
    return setups.get_setup(input.setup_select() or "standard")
run.current_setup = _current_setup
```

It reads **run_control's** namespaced `input.setup_select()`
(`run_control.py:88`). Everything downstream — the config-file filter
(`_sync_setup_to_config`), the box-count guard, the run environment
(`ESTAS_HOLD_VOLUME`), and the Dashboard's own Quick Run
(`dashboard.py:205`, `:587`, `:655`) — reads `run.current_setup()`. It is the one
authoritative value and must stay so.

Shiny inputs are per-module (namespaced), so a Dashboard selector is necessarily a
**second** input widget. The design keeps the two widgets in sync without
repainting the run or looping.

## Architecture: two-effect mirror over the make_scope bridge

The app already reaches run_control's namespaced inputs from other modules via
`session.root_scope().make_scope("run_control")` + the bare id — see
`sim_config.py:155/214`, `model_build.py:356-357`, `plot.py:244/467`. The
Dashboard uses the same bridge.

```
   Dashboard                          run_control (authoritative)
   ┌───────────────────┐             ┌──────────────────────────┐
   │ dash_setup_select │──Effect A──▶│ setup_select             │
   │  (new widget)     │◀─Effect B───│   → run.current_setup()  │
   └───────────────────┘             └──────────────────────────┘
        Effect A: dashboard change  → push to run_control's setup_select (via rc)
        Effect B: current_setup change → mirror back into dash_setup_select
   Each effect updates the OTHER widget only when the value actually differs.
```

- **Effect A (dashboard → authoritative):** reacts to `input.dash_setup_select()`.
  If it differs from run_control's current `setup_select` (read under
  `reactive.isolate()` so this effect depends only on the dashboard widget), push
  it with `ui.update_select("setup_select", selected=chosen, session=rc)`.
- **Effect B (authoritative → dashboard):** reacts to `run.current_setup()`. If
  its `.id` differs from the dashboard widget's current value (read under
  `reactive.isolate()`), mirror it with
  `ui.update_select("dash_setup_select", selected=target)`.

**Why it cannot loop:** each effect writes the *other* widget and only when the
value differs. A change made in either widget propagates once to the other, whose
value then already matches, so the paired effect's guard is a no-op. Both widgets
are created with `selected="standard"`, matching `current_setup`'s default, so they
start in sync.

**Availability notice on the Dashboard:** a `@render.ui` that reads
`run.current_setup()` (the source of truth, so it reflects whichever widget last
changed) and renders the same warning as run_control's `setup_availability`
(`run_control.py:341-347`) when the setup's inputs are missing. This matters
because **Quick Run** and **Stop** sit directly below — the hint explains why a run
would be blocked *before* the click, instead of failing at launch
(`dashboard.py:206`).

## Components / files

All changes are confined to two files plus one test file.

### `shiny_app/modules/dashboard.py`

**`dashboard_ui()`** — insert a compact row between the status-bar `div` (closes at
`:110`) and the `ui.layout_columns(...)` action/log row (`:112`):

```python
ui.div(
    {"class": "dashboard-setup-row", "style": "max-width: 460px; margin-bottom: 0.75rem;"},
    ui.input_select("dash_setup_select", "Setup:",
                    choices={s.id: s.name for s in setups.list_setups()},
                    selected="standard"),
    ui.output_ui("dash_setup_availability"),
),
```

`setups` is already imported at module level (`dashboard.py:44/55`); `dashboard_ui`
is a module-level function that can call `setups.list_setups()`.

**`dashboard_server(input, output, session, state)`** — add the bridge, the two
sync effects, and the availability render. `run = state.run` and `ROOT` already
exist (`:175`, `:67`):

```python
# Setup selector sync: Dashboard <-> Run Model tab. Single source of truth is
# run.current_setup (a reactive.calc over run_control's input.setup_select).
rc = session.root_scope().make_scope("run_control")

@reactive.effect
def _dash_setup_to_run_control():
    chosen = input.dash_setup_select()
    if not chosen:
        return
    with reactive.isolate():
        current = rc.input.setup_select()
    if chosen != current:
        ui.update_select("setup_select", selected=chosen, session=rc)

@reactive.effect
def _run_control_to_dash_setup():
    target = run.current_setup().id
    with reactive.isolate():
        shown = input.dash_setup_select()
    if target != shown:
        ui.update_select("dash_setup_select", selected=target)

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

### `tests/python/test_dashboard_module.py`

Extend the existing render test (or add a focused one) to assert the new
namespaced ids and the label marker appear in `dashboard_ui("dashboard")`:

```python
def test_dashboard_ui_includes_setup_selector():
    html = str(dashboard_ui("dashboard"))
    assert nid("dashboard", "dash_setup_select") in html
    assert nid("dashboard", "dash_setup_availability") in html
    assert "Setup:" in html
```

`nid()` already exists in this file (namespaced-id helper).

### No change: `run_control.py`, `setups.py`, `app_state.py`

The authoritative selector, the registry, and the contract are untouched.

## Data flow (worked examples)

**User picks CL29 on the Dashboard:**
1. `dash_setup_select` → `"cl29"`.
2. Effect A: `rc.input.setup_select()` is `"standard"` ≠ `"cl29"` → push to
   run_control's `setup_select`.
3. run_control `setup_select` → `"cl29"` → `run.current_setup()` recomputes to the
   CL29 setup; `_sync_setup_to_config` filters the config file; the box-count guard
   and env injection follow.
4. Effect B: `run.current_setup().id` is `"cl29"`; `dash_setup_select` is already
   `"cl29"` → no-op. Converged.
5. `dash_setup_availability` re-renders from `run.current_setup()`.

**User picks CL29 on the Run Model tab (reverse):**
1. run_control `setup_select` → `"cl29"` → `run.current_setup()` recomputes.
2. Effect B: `dash_setup_select` is `"standard"` ≠ `"cl29"` → mirror to `"cl29"`.
3. Effect A: `rc.input.setup_select()` is already `"cl29"` → no-op. Converged.

## Error handling / edge cases

- **Input not yet initialized at first flush:** reading `rc.input.setup_select()`
  or `run.current_setup()` before the value exists raises Shiny's silent
  exception, which halts that effect run gracefully; it re-runs on the next flush
  once defaults are set. Both widgets ship `selected="standard"`, so the steady
  state is in sync. This mirrors how existing dashboard effects already read
  `run.current_setup()`.
- **Unknown/empty selection:** Effect A returns early on falsy `chosen`.
  `setups.get_setup(... or "standard")` already defaults unknown ids.
- **Setup unavailable (e.g. CL29 not generated):** the Dashboard shows the same
  warning as the Run Model tab; the existing Quick Run availability guard
  (`dashboard.py:206`) still blocks the actual launch.

## Testing

1. **Unit (added):** `test_dashboard_module.py` asserts the new namespaced ids and
   the "Setup:" label render in `dashboard_ui`.
2. **Render backstop (existing):** `tests/python/test_ui_renders.py`
   (`create_ui().tagify()`) exercises the full app render — the suite does not
   otherwise import `app.py`, so this catches a broken `dashboard_ui`.
3. **Manual Playwright smoke (post-merge, not CI):** on the Dashboard, select CL29
   → confirm the Run Model tab's selector follows; change it back on the Run Model
   tab → confirm the Dashboard selector follows. Confirm the availability hint
   appears for an unavailable setup.

## Rollout

Single PR on `feat/dashboard-setup-selector`. Green CI (ruff + pytest, incl. the
render backstop) then merge on the user's go-ahead, per project convention.
