# Shiny-Modules Rearchitecture — Phase 3 (Output-Selection Cluster) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convert the output-cluster tabs into Shiny modules and finally CONSUME the Phase-0 output-selection bus: `output_browser` (sole publisher of `state.selected_output_dir/file/format`), `plot` (the sole consumer), plus the self-contained `mass_balance`, `observations`, and `diagnostics` (pseudo→true).

**Architecture:** Apply the proven Phase-1/2 module recipe (`shiny_app/modules/<tab>.py`, `x_ui` content-only, `x_server(input,output,session,state)`, selectors migrated). The novel part: `plot` switches its cross-module reads of `output_dir_select`/`plot_output_file`/`output_format` to `state.selected_*`; `output_browser` OWNS those inputs and takes over publishing (moving the Phase-0 `_publish_output_selection` effect into itself). The shared `get_selected_output_file_path` helper becomes a pure leaf. `diagnostics` converts from its plain-function pseudo-module to a true `@module.ui`/`@module.server`. `plot`+`output_browser` are the interleaved Plots fat-tab (card-level composition).

**Tech Stack:** Python 3.10+, Shiny for Python 1.5.x (`shiny.module`), pytest, Playwright/Selenium (CI-only). Builds on v0.4.2 (Phase 2).

**Spec:** `docs/superpowers/specs/2026-07-14-app-py-shiny-modules-rearchitecture-design.md` (§5 the bus, §6 Plots split, §7 Phase 3). Template: `shiny_app/modules/parameters.py` (+ `sim_config.py` for the `session.root_scope()` / fat-tab patterns).

## Global Constraints

- **Behavior-identical:** each tab's flow unchanged; ONLY within-tab ids gain the `<module>-` namespace. Nav ids + other tabs untouched.
- **Module recipe** (as Phases 1-2): `x_ui(id)` content-only (panel_conditional stays in create_ui); `x_server(input,output,session,state)`; self-contained (leaf imports only, nothing from app.py); import-fallback; `getLogger("AQUABC")`; self-computed ROOT/INPUTS_DIR (`.tagify()` render test only for nav-panel modules; plain `str()` otherwise).
- **The output-selection bus (the phase's crux — verified empirically):** the ONLY cross-module consumer of the output selection is `plot` (reads `input.output_format` + calls `get_selected_output_file_path` which reads `output_dir_select`/`plot_output_file`). `mass_balance`/`observations`/`diagnostics` do NOT read it (self-contained). `output_browser` OWNS `output_dir_select`/`plot_output_file`/`output_format`.
- **Bus-consumption invariant:** after Phase 3, `plot` reads `state.selected_output_dir/file/format` (NOT `input.*`); `output_browser` is the SOLE writer (its `@reactive.effect` publishes from its namespaced inputs); the Phase-0 app-level `_publish_output_selection` effect (@3164) is DELETED (moved into `output_browser`). No `input.output_dir_select` read survives outside `output_browser`.
- **Lint-clean:** `ruff check shiny_app/modules/ tests/python/`. Integration tests CI-only → local = render-smoke + boot smoke.
- **Commit per task.**

## Ordering (READER-FIRST — refines the spec's "output_browser first")

The spec §7 lists "output_browser first, then readers." **Implementation reality inverts this:** the moment `output_browser` namespaces `output_dir_select` (→ `output_browser-output_dir_select`), any still-inline reader of `input.output_dir_select` breaks. So the readers must already read `state.selected_*` before `output_browser` namespaces. The Phase-0 `_publish_output_selection` effect (still app-level) keeps feeding `state.selected_*` from the app-level inputs throughout, so switching a reader to `state` is behavior-identical while `output_browser` is still inline. Order: **leaf helper → mass_balance → observations → plot (switches to bus) → output_browser (takes over publishing, last) → diagnostics.**

## Per-Module Spec (source ranges current on `main` @ v0.4.2; grep to confirm)

| id | handlers (app.py) | reactive.Values | leaf deps | bus role | selectors |
|---|---|---|---|---|---|
| `mass_balance` | `calculate_mass_balance`@1805, `mass_balance_summary`@1843, `mass_balance_details`@1866, `mass_balance_plot_ui`@1927 | `mb_results`, `mb_calculator` | `mass_balance` | none (self-contained) | grep |
| `observations` | `scan_observations_dir`@1995 … `obs_scatter_info`@2339 (10 handlers) | `obs_data_obj`, `obs_comparison_obj`, `obs_metrics_results`, `obs_files_list`, `obs_loaded_file`, `obs_file_preview` | `obs_loader`, `observation_compare` | none (self-contained) | grep `#obs_*` |
| `plot` | `_get_cached_data`@2376, `_get_cached_csv`@2437, `_update_variable_choices`@2444, `update_input_ts_boxes`@3449, `input_ts_info`@3466, `input_ts_date_range`@3491, `input_ts_plot`@3513, `main_plot`@3623 | `csv_cache`, `csv_cache_mtime`, `csv_cache_path` (private) | `output_data`, `diagnostics_plots`(?) | **CONSUMER**: reads `state.selected_output_dir/file/format` + the leaf path resolver | `#plot_*`, `#refresh_plot`, `#output_format` |
| `output_browser` | `out_preview`@3027, `analyze_output_directory`@3042, `init_output_dirs`@3117, `refresh_output_dirs`@3147, `output_file_preview`@3197, `output_files_summary`@3295, `update_plot_output_files`@3393, `refresh_plot_output_files`@3410, `plot_output_file_info`@3420 + the Phase-0 `_publish_output_selection`@3164 | (its own) | `output_data`, `file_locators` | **PUBLISHER**: owns `output_dir_select`/`plot_output_file`/`output_format`; publishes to `state.selected_*` | `#output_dir_select`, `#plot_output_file`, `#output_format`, `#analyze_output_dir`, `#refresh_*` |
| `diagnostics` | `shiny_app/diagnostics.py` (existing pseudo-module: `diagnostics_ui()` no-id, `diagnostics_server(input,output,session,root_dir)`) | internal | (its own) | none (own `diag_output_dir`) | `#diag_*` |

`plot` + `output_browser` are the interleaved **Plots** fat-tab (`panel_plot`): each `_ui` returns CARDS that `create_ui`/`panel_plot` composes (card-level, per spec §6 — like the Phase-1 decision, but here executed).

---

### Task 1: Extract `get_selected_output_file_path` → a pure leaf resolver

**Files:** Modify `shiny_app/output_data.py` (add a pure fn); Modify `shiny_app/app.py` (repoint the 3 call sites). Test: `tests/python/test_output_data.py`.

`get_selected_output_file_path()` (app.py:3379) reads `input.output_dir_select()`+`input.plot_output_file()` and is called cross-module by `plot` (2448, 3634) and `output_browser` (3422). A leaf can't read inputs, so make it a **pure function**:

- [ ] Add `resolve_output_file_path(dir_name, file_name, root=ROOT) -> str | None` to `output_data.py` (the body of `get_selected_output_file_path` with `input.output_dir_select()`/`input.plot_output_file()` replaced by the `dir_name`/`file_name` params). Add a unit test.
- [ ] In app.py, repoint the 3 call sites to pass the values: `output_browser` calls `resolve_output_file_path(input.output_dir_select(), input.plot_output_file())`; `plot`'s 2 call sites will pass `state.selected_output_dir()`/`state.selected_output_file()` (done when plot converts, Task 4). For now (still inline), pass `input.output_dir_select()`/`input.plot_output_file()` at all 3 — behavior-identical. Delete the old nested `get_selected_output_file_path` def.
- [ ] Gate: ruff clean; suite +1 (the new unit test); grep `def get_selected_output_file_path` → empty.
- [ ] **Commit** — `refactor(shiny): extract resolve_output_file_path to output_data leaf`

---

### Task 2: `mass_balance` module (self-contained)

Standard recipe (id `mass_balance`). Handlers @1805-1927 + reactive values `mb_results`/`mb_calculator`; import the `mass_balance` leaf. **Verify no output-selection bus read** (grep the block for `input.output_dir_select`/`state.selected`/`resolve_output_file_path` — expected none; if any, switch to `state.selected_*`). Render-smoke test; migrate any `#mass_balance*`/`#calc_mass_balance` selectors. Gate as recipe (suite +1). **Commit** — `refactor(shiny): mass_balance Shiny module`

---

### Task 3: `observations` module (self-contained)

Standard recipe (id `observations`). The 10 handlers @1995-2339 + 6 reactive values (`obs_data_obj`, `obs_comparison_obj`, `obs_metrics_results`, `obs_files_list`, `obs_loaded_file`, `obs_file_preview`); import `obs_loader`+`observation_compare` (grep exact names). Verify no bus read (expected none). Migrate `#obs_*` selectors (grep all 3 test files). Gate as recipe (suite +1). **Commit** — `refactor(shiny): observations Shiny module`

---

### Task 4: `plot` module (THE bus consumer)

Standard recipe (id `plot`) with the bus switch. Handlers @2376-2444, @3449-3623 (8) + private `csv_cache`/`csv_cache_mtime`/`csv_cache_path`; import `output_data` (+ whatever `main_plot`/`_get_cached_data` call — grep, likely `diagnostics_plots`).

- [ ] **Bus switch (the point of this task):** every cross-module read of the output selection → `state.*`:
  - `input.output_format()` (@2447, @3639) → `state.selected_output_format()`.
  - `get_selected_output_file_path()` calls (@2448, @3634) → `resolve_output_file_path(state.selected_output_dir(), state.selected_output_file())` (the Task-1 leaf).
  - Grep the plot block for ANY other `input.output_dir_select`/`input.plot_output_file`/`input.output_format` and switch each to `state.selected_*`.
- [ ] `plot_ui("plot")` returns the plot CARDS (main plot, variable choices, input-timeseries) — the Plots-panel content that is `plot`'s (NOT the output-browser cards). `create_ui`/`panel_plot` composes `plot_ui("plot")` + (Task 5) `output_browser_ui("output_browser")`. **Coordinate the `panel_plot` split with Task 5** — Task 4 puts plot's cards behind `plot_ui`, Task 5 puts the browser cards behind `output_browser_ui`; `panel_plot` composes both. (If `panel_plot`'s current structure makes a clean card split hard, STOP and report — this is the fat-tab risk.)
- [ ] Render-smoke test (`plot-main_plot` etc. namespaced). Migrate `#plot_*`/`#refresh_plot` selectors. Gate: ruff; grep-clean; suite +1; boot smoke (`plot-*` present, and — crucially — the plot still renders because `state.selected_*` is fed by the still-app-level Phase-0 publisher).
- [ ] **Commit** — `refactor(shiny): plot Shiny module (consume the output-selection bus)`

---

### Task 5: `output_browser` module (THE publisher — last)

Standard recipe (id `output_browser`), converting last so no consumer reads its inputs by the time they namespace. Handlers @3027-3449 (9) + import `output_data`+`file_locators`. It OWNS `output_dir_select`/`plot_output_file`/`output_format`.

- [ ] **Take over publishing:** MOVE the Phase-0 `_publish_output_selection` `@reactive.effect` (@3164) INTO `output_browser_server` (it reads the module's now-namespaced `input.output_dir_select`/`plot_output_file`/`output_format` and sets `state.selected_output_dir/file/format`). DELETE the app-level `_publish_output_selection` from `server()`.
- [ ] `output_browser`'s own `get_selected_output_file_path` calls (@3422) → `resolve_output_file_path(input.output_dir_select(), input.plot_output_file())` (its own namespaced inputs — fine).
- [ ] `output_browser_ui("output_browser")` returns the output-browser CARDS; `panel_plot` composes them alongside `plot_ui("plot")` (Task 4).
- [ ] Render-smoke; migrate `#output_dir_select`/`#plot_output_file`/`#output_format`/`#analyze_output_dir`/`#refresh_*` selectors. Gate: ruff; grep-clean (`_publish_output_selection` gone from app.py `server()`; NO bare `input.output_dir_select` anywhere outside the module); suite +1; **BOOT SMOKE (critical): the bus round-trips end-to-end** — select an output dir in the (namespaced) output_browser, and the plot (reading `state.selected_*`) sees it. Verify `output_browser-output_dir_select` present, 0 bare leaks, ws session no traceback. (The actual round-trip is CI-integration-verified.)
- [ ] **Commit** — `refactor(shiny): output_browser Shiny module (sole output-selection publisher)`

---

### Task 6: `diagnostics` — pseudo-module → true module

**Files:** `shiny_app/diagnostics.py`, `shiny_app/app.py` (create_ui + the `diagnostics_server(...)` call @3770).

The existing `diagnostics_ui()` takes no id and `diagnostics_server(input,output,session,root_dir)` receives the GLOBAL trio with manually-`diag_`-prefixed ids. Convert to a TRUE module:
- [ ] `diagnostics_ui()` → `@module.ui def diagnostics_ui():` (called `diagnostics_ui("diagnostics")`); it returns a `panel_conditional`? — NO: strip it to content-only (the `panel_conditional("input.navigation === 'nav_diagnostics'", …)` moves to `create_ui`, matching every other module). Ids stay bare (`diag_output_dir` etc.) and now namespace to `diagnostics-diag_output_dir`.
- [ ] `diagnostics_server(input,output,session,root_dir)` → `@module.server def diagnostics_server(input, output, session, state):` — self-contained (own `diag_output_dir`, uses nothing from `state` — accept it for convention; keep `root_dir` behavior by self-computing ROOT). Called `diagnostics_server("diagnostics", state)`.
- [ ] `create_ui`: `panel_diagnostics = diagnostics_ui("diagnostics")` (already assembled there — now wrapped in the app-level `panel_conditional`). Server call `diagnostics_server(input, output, session, root_dir=ROOT)` (@3770) → `diagnostics_server("diagnostics", state)`.
- [ ] Migrate any `#diag_*` selectors (grep). Render-smoke (`diagnostics-diag_output_dir` etc.). **Named risk:** `diagnostics.py` already has helper functions + `_diag_state`; ensure only the ui/server decorators + the panel_conditional move changes — the diagnostic logic ports verbatim.
- [ ] Gate: ruff; grep-clean; suite +1; boot smoke (`diagnostics-diag_*` namespaced; the Diagnostics tab runs its analysis). **Commit** — `refactor(shiny): diagnostics true Shiny module (pseudo→@module)`

---

### Task 7: Phase-3 regression gate

- [ ] **Static + unit:** py_compile; import app + each new module (`mass_balance`, `observations`, `plot`, `output_browser`, `diagnostics`); ruff modules+tests clean; full suite (**173 + one render test per module + the leaf test** ≈ 179-180).
- [ ] **Bus invariant:** `grep -nE "input.output_dir_select|input.plot_output_file|input.output_format" shiny_app/app.py` → hits ONLY inside `output_browser` (none in `server()`-level or other modules); `grep "_publish_output_selection" shiny_app/app.py` → empty (moved into output_browser); `plot` reads `state.selected_*`.
- [ ] **Grep-clean:** all 5 tabs' old handlers + `panel_*`/pseudo-`diagnostics_server(input,...)` gone.
- [ ] **Boot smoke:** every converted tab renders namespaced, 0 bare-id leaks; ws session runs `server()` (all modules) no traceback; the Plots tab shows both plot + output-browser cards.
- [ ] **CI (on push):** `integration-tests` drives the `plot-*`/`output_browser-*` flow AND the bus round-trip — the authoritative proof the output selection still drives the plot after namespacing.
- [ ] **(Deferred) release** — `v0.4.3` at finishing.

---

## Self-Review

**Spec coverage (§7 Phase 3 → tasks):** output_browser (publisher) + plot (consumer) + mass_balance/observations/diagnostics → Tasks 2-6; the bus consumption (§5) → Tasks 4-5 (plot reads state, output_browser publishes, Phase-0 effect moves in); `get_selected_output_file_path` → leaf (§11) → Task 1; Plots fat-tab card split (§6) → Tasks 4+5; diagnostics pseudo→true → Task 6.

**Placeholder scan:** recipe + per-module spec table give ranges/deps/selectors; the two intricate cases (the bus publisher/consumer handoff; the Plots card split) get explicit steps + named risks. Full per-module code NOT pasted — mechanical application of the proven `parameters.py`/`sim_config.py` templates.

**Key refinements from empirical verification (before this plan executes, RE-VERIFY like Phase 1/2):**
1. **Reader-first ordering** (not spec's "output_browser first") — because namespacing output_browser's inputs breaks still-inline readers; the Phase-0 publisher covers the interim. This is the load-bearing decision.
2. **Only `plot` consumes the bus** (grep-verified); mass_balance/observations/diagnostics are self-contained — simpler than the §5.1 audit implied. Task 2/3 each grep-verify "no bus read" before assuming self-contained.
3. **`get_selected_output_file_path` → pure leaf** `resolve_output_file_path(dir, file)` (can't read inputs as a leaf).
4. **Plots fat-tab** = card-level composition of `plot_ui` + `output_browser_ui` in `panel_plot` — the one structural risk; Tasks 4/5 STOP-and-report if `panel_plot` doesn't split cleanly.
5. **diagnostics** strips its own `panel_conditional` into create_ui (like every module) and accepts `state` for convention though self-contained.
