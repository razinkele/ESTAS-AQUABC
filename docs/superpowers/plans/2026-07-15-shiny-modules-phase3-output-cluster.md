# Shiny-Modules Rearchitecture — Phase 3 (Output/Plot Cluster) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convert the remaining data/output tabs into Shiny modules — `mass_balance`, `observations`, `plot` (the whole Plots tab, merging what the spec called plot+output_browser), and `diagnostics` (pseudo→true) — and remove the confirmed-dead Phase-0 output-selection bus.

**Architecture:** Apply the proven Phase-1/2 module recipe (`shiny_app/modules/<tab>.py`, `x_ui` content-only, `x_server(input,output,session,state)`, selectors migrated). **Decision (user-confirmed, reverses spec §6): `plot` and `output_browser` MERGE into one `plot` module** — grep proved the output selection (`output_dir_select`/`plot_output_file`/`output_format`) is consumed only within those two, both live in `panel_plot`, and `state.selected_output_dir/file/format` is read by nobody. Merging keeps the selection internal (no cross-module bus, no `panel_plot` card-split) and lets us delete the dead Phase-0 bus. `mass_balance`/`observations` are self-contained leaf conversions. `diagnostics` converts from its plain-function pseudo-module to a true `@module`.

**Tech Stack:** Python 3.10+, Shiny for Python 1.5.x (`shiny.module`), pytest, Playwright/Selenium (CI-only). Builds on v0.4.2 (Phase 2).

**Spec:** `docs/superpowers/specs/2026-07-14-app-py-shiny-modules-rearchitecture-design.md` (§4 pattern; §6 Plots split — **superseded by the merge**; §7 Phase 3). Template: `shiny_app/modules/parameters.py` (+ `input_files.py` for a large multi-handler module).

## Global Constraints

- **Behavior-identical:** each tab's flow unchanged; ONLY within-tab ids gain the `<module>-` namespace. Nav ids + other tabs untouched.
- **Module recipe** (as Phases 1-2): `x_ui(id)` content-only (panel_conditional stays in create_ui); `x_server(input,output,session,state)`; self-contained (leaf imports only, nothing from app.py); import-fallback; `getLogger("AQUABC")`; self-computed ROOT/INPUTS_DIR; render-smoke via `str(x_ui("x"))`.
- **Merged `plot`:** the whole `panel_plot` (Plots tab) becomes ONE module. The output-selection widgets and their readers are ALL inside it (namespace consistently to `plot-*`), so no `state.selected_*` is used. Any `__file__`-relative or dynamic-input patterns get the usual module-depth/round-trip care (Phase-2 lessons).
- **Dead-bus removal (Phase-0 cleanup, in the plot task):** delete the app-level `_publish_output_selection` `@reactive.effect` (app.py ~3164) and the three `AppState.selected_output_dir/file/format` fields (from `shiny_app/app_state.py` dataclass + the `AppState(...)` construction in `server()`), and update `tests/python/test_run_controller.py::test_appstate_holds_fields`. These were Phase-0 speculation with zero consumers (grep-verified). Keep the other 4 AppState fields (`run`, `navigate`, `output_config_version`, `sim_config_version`).
- **Lint-clean:** `ruff check shiny_app/modules/ tests/python/`. Integration tests CI-only → local = render-smoke + boot smoke.
- **Commit per task.**

## Per-Module Spec (source ranges current on `main` @ v0.4.2; grep to confirm)

| id | handlers (app.py) | reactive.Values | leaf deps | selectors |
|---|---|---|---|---|
| `mass_balance` | `calculate_mass_balance`@1805, `mass_balance_summary`@1843, `mass_balance_details`@1866, `mass_balance_plot_ui`@1927 | `mb_results`, `mb_calculator` | `mass_balance` | grep `#calc_mass_balance`/`#mb_*` |
| `observations` | 10 handlers `scan_observations_dir`@1995 … `obs_scatter_info`@2339 | `obs_data_obj`, `obs_comparison_obj`, `obs_metrics_results`, `obs_files_list`, `obs_loaded_file`, `obs_file_preview` | `obs_loader`, `observation_compare` | grep `#obs_*`/`#generate_sample_obs` |
| `plot` (whole `panel_plot`, incl. former output_browser) | `_get_cached_data`@2376, `_get_cached_csv`@2437, `_update_variable_choices`@2444, `out_preview`@3027, `analyze_output_directory`@3042, `init_output_dirs`@3117, `refresh_output_dirs`@3147, `output_file_preview`@3197, `output_files_summary`@3295, `get_selected_output_file_path`@3379, `update_plot_output_files`@3393, `refresh_plot_output_files`@3410, `plot_output_file_info`@3420, `update_input_ts_boxes`@3449, `input_ts_info`@3466, `input_ts_date_range`@3491, `input_ts_plot`@3513, `main_plot`@3623 | `csv_cache`, `csv_cache_mtime`, `csv_cache_path` | `output_data`, `file_locators` (+ grep `main_plot`/`_get_cached_data` for others, e.g. `diagnostics_plots`) | `#output_dir_select`, `#plot_output_file`, `#output_format`, `#analyze_output_dir`, `#refresh_plot*`, `#plot_*`, `#input_ts_*` |
| `diagnostics` | `shiny_app/diagnostics.py` pseudo-module (`diagnostics_ui()` no-id + `diagnostics_server(input,output,session,root_dir)`) | internal `_diag_state` | its own | `#diag_*` |

---

### Task 1: `mass_balance` module (self-contained)

Standard recipe (id `mass_balance`). Handlers @1805-1927 + reactive values `mb_results`/`mb_calculator`; import the `mass_balance` leaf (grep exact names). **Grep-verify no output-selection read** (`input.output_dir_select`/`state.selected`/`get_selected_output_file_path` — expected none). Render-smoke test; migrate any `#calc_mass_balance`/`#mb_*` selectors (grep all 3 test files). Gate: ruff; grep-clean; suite +1; boot smoke (`mass_balance-*` namespaced). **Commit** — `refactor(shiny): mass_balance Shiny module`

---

### Task 2: `observations` module (self-contained)

Standard recipe (id `observations`). 10 handlers @1995-2339 + 6 reactive values (`obs_data_obj`, `obs_comparison_obj`, `obs_metrics_results`, `obs_files_list`, `obs_loaded_file`, `obs_file_preview`); import `obs_loader`+`observation_compare` (grep exact names). Grep-verify no bus read. Migrate `#obs_*`/`#generate_sample_obs` selectors. Gate as recipe (suite +1). **Commit** — `refactor(shiny): observations Shiny module`

---

### Task 3: `plot` module (whole Plots tab — merged; remove the dead Phase-0 bus)

The largest module — the entire `panel_plot` (plotting + output-directory browsing + file preview + input-timeseries). Standard recipe (id `plot`) but big; work like `input_files` (map the block first).

- [ ] **Map the handlers:** grep `panel_plot`'s output ids and the app.py block for all handlers — the plotting set (`_get_cached_data`, `_get_cached_csv`, `_update_variable_choices`, `main_plot`, `update_input_ts_boxes`, `input_ts_info`, `input_ts_date_range`, `input_ts_plot`) AND the former-output_browser set (`out_preview`, `analyze_output_directory`, `init_output_dirs`, `refresh_output_dirs`, `output_file_preview`, `output_files_summary`, `get_selected_output_file_path`, `update_plot_output_files`, `refresh_plot_output_files`, `plot_output_file_info`) + reactive values `csv_cache`/`csv_cache_mtime`/`csv_cache_path`. (Note: these are NOT contiguous in app.py — the plotting handlers straddle the obs block; grep each.)
- [ ] `plot_ui()` = the content of `panel_plot(min_smooth_window)` (ui_panels.py @400-582) with the `panel_conditional` stripped. Since everything is one module now, all its ids namespace to `plot-*` uniformly — **no card-split, no cross-module composition.** Pass `min_smooth_window` into `plot_ui` (a `@module.ui` arg after the id) or import the const.
- [ ] `plot_server(input, output, session, state)`: all ~18 handlers + the 3 csv-cache reactive values, ported verbatim. **`get_selected_output_file_path` stays a nested helper** (reads the module's own `input.output_dir_select`/`input.plot_output_file` — namespaced consistently). All `input.output_format`/`input.output_dir_select`/`input.plot_output_file` reads stay as `input.*` (they're the module's own inputs now — do NOT switch to `state`). Import `output_data`+`file_locators` (grep exact deps). `state` accepted, unused.
- [ ] **Remove the dead Phase-0 bus:** delete the app-level `_publish_output_selection` `@reactive.effect` (app.py ~3164 — it published the now-internal inputs to a bus nobody reads); delete `selected_output_dir`/`selected_output_file`/`selected_output_format` from `AppState` (`shiny_app/app_state.py` dataclass) and from the `AppState(...)` construction in `server()`; update `tests/python/test_run_controller.py::test_appstate_holds_fields` to the 4-field `AppState`. Grep-confirm zero `state.selected_output` references remain anywhere.
- [ ] Render-smoke test (`plot-main_plot`, `plot-output_dir_select`, `plot-plot_output_file`, `plot-output_files_summary`, `plot-input_ts_plot` etc. namespaced). Wire into app.py (create_ui swap; `plot_server("plot", state)`; delete inline handlers; delete `panel_plot` from ui_panels + app.py import lists + test_ui_panels ARGFREE). Migrate `#output_dir_select`/`#plot_output_file`/`#output_format`/`#analyze_output_dir`/`#refresh_plot*`/`#plot_*`/`#input_ts_*` selectors (grep all 3 test files).
- [ ] Gate: py_compile; ruff; grep-clean (all ~18 handlers + `panel_plot` + `_publish_output_selection` gone; NO `state.selected_output` anywhere); import `shiny_app.app`; suite (+1 render −? for the appstate-test edit → net +1); BOOT SMOKE (Plots tab renders fully namespaced `plot-*`, 0 bare leaks; select an output dir → the plot updates [internal now]; ws session no traceback).
- [ ] **Named risk:** the plotting handlers and former-output_browser handlers are non-contiguous in app.py (the observations block sits between them) — map carefully; if the block boundary is ambiguous, STOP and report. Also `main_plot`/`input_ts_plot` are `@render_widget` (shinywidgets) — confirm the import.
- [ ] **Commit** — `refactor(shiny): plot Shiny module (whole Plots tab; drop dead Phase-0 output-selection bus)`

---

### Task 4: `diagnostics` — pseudo-module → true module

**Files:** `shiny_app/diagnostics.py`, `shiny_app/app.py` (create_ui `diagnostics_ui()` @~487 + the `diagnostics_server(input,output,session,root_dir=ROOT)` call @3770).

Convert the existing plain-function pseudo-module (manually `diag_`-prefixed ids, global trio) to a TRUE module:
- [ ] `diagnostics_ui()` → `@module.ui def diagnostics_ui():`, called `diagnostics_ui("diagnostics")`. Strip its outer `panel_conditional` (it currently returns one — move the `panel_conditional("input.navigation === 'nav_diagnostics'", …)` to `create_ui`, matching every other module). Ids stay bare (`diag_output_dir`, `diag_run_btn`, …) → now namespace to `diagnostics-diag_*`.
- [ ] `diagnostics_server(input,output,session,root_dir)` → `@module.server def diagnostics_server(input, output, session, state):` — self-contained (its own `diag_output_dir`); replace the `root_dir` param by self-computing ROOT in the module (the pseudo-module already self-computes helpers). Accept `state` for convention (unused). Called `diagnostics_server("diagnostics", state)`. The diagnostic logic (`_diag_state`, the 16-check analysis, PDF export) ports VERBATIM.
- [ ] `create_ui`: wrap `diagnostics_ui("diagnostics")` in the app-level `panel_conditional`; update the `diagnostics_server(...)` call.
- [ ] Migrate any `#diag_*` selectors (grep). **Render-smoke — MUST `.tagify()`:** after stripping the `panel_conditional`, `diagnostics_ui()` returns a `ui.navset_card_tab(...)` (verified: diagnostics.py:226-246 is `panel_conditional(navset_card_tab(nav_panels))`), and navset objects return a repr from bare `str()` — so assert against `str(diagnostics_ui("diagnostics").tagify())` containing `diagnostics-diag_output_dir`/`diagnostics-diag_run_btn` (same gotcha as `sim_config`). **Named risk:** `diagnostics.py` has module-level helper functions + `_diag_state` — ensure ONLY the ui/server decorators + the panel_conditional move; the helpers stay as-is (they're called by the server). Confirm `import shiny_app.diagnostics` still works.
- [ ] Gate: ruff; grep-clean; suite +1; boot smoke (`diagnostics-diag_*` namespaced; the Diagnostics tab runs its analysis). **Commit** — `refactor(shiny): diagnostics true Shiny module (pseudo→@module)`

---

### Task 5: Phase-3 regression gate

- [ ] **Static + unit:** py_compile; import app + each new module (`mass_balance`, `observations`, `plot`, `diagnostics`); ruff modules+tests clean; full suite (**173 + one render test per module ≈ 177**, minus any appstate-test delta = re-count at gate).
- [ ] **Dead-bus gone:** `grep -nE "state.selected_output|_publish_output_selection|selected_output_dir|selected_output_file|selected_output_format" shiny_app/app.py shiny_app/app_state.py` → empty; `AppState` is 4 fields; test_appstate_holds_fields updated.
- [ ] **Grep-clean:** all 4 tabs' old handlers + `panel_mass_balance`/`panel_observations`/`panel_plot` + the pseudo `diagnostics_server(input,...)` gone.
- [ ] **Boot smoke:** every converted tab renders namespaced, 0 bare-id leaks; ws session runs `server()` (all modules) no traceback; the Plots tab still shows all its sub-tabs (plotting + output browsing + input-timeseries) with `plot-*` ids; output-dir selection still drives the plot (now internal).
- [ ] **CI (on push):** `integration-tests` drives the `plot-*` flow (incl. the output-selection→plot round-trip, now internal) — the authoritative DOM proof.
- [ ] **(Deferred) release** — `v0.4.3` at finishing.

---

## Self-Review

**Spec coverage (§7 Phase 3 → tasks):** mass_balance/observations → Tasks 1-2; the plot+output_browser cluster → Task 3 (**merged** per the confirmed §6 reversal); diagnostics pseudo→true → Task 4; regression + CI → Task 5. The Phase-0 output-selection bus (§5) is REMOVED as confirmed-dead code (Task 3), not consumed.

**Placeholder scan:** recipe + per-module spec table give ranges/deps/selectors; the two non-trivial cases (the large merged `plot` with non-contiguous handlers + dead-bus removal; the `diagnostics` pseudo→true) get explicit steps + named risks. Full per-module code NOT pasted — mechanical application of the proven `parameters.py`/`input_files.py` templates.

**Key facts from empirical review (RE-VERIFY at execution like Phases 1-2):**
1. **plot+output_browser MERGE** — only they consume the output selection, both in `panel_plot`, and `state.selected_*` has zero readers → merging removes the bus + the `panel_plot` card-split. Confirmed by grep + user decision.
2. **mass_balance/observations self-contained** (grep-verified no bus read) — standard leaf conversions; Task 1/2 re-grep to confirm before assuming.
3. **Dead-bus removal** drops `AppState` 7→4 fields (remove `selected_output_dir/file/format` + the `_publish_output_selection` effect); update `test_appstate_holds_fields`.
4. **plot handlers are non-contiguous** in app.py (obs block interleaves) — map carefully (Task 3 named risk).
5. **diagnostics** strips its own `panel_conditional` into create_ui and drops `root_dir` for self-computed ROOT; helpers/`_diag_state` port verbatim.
