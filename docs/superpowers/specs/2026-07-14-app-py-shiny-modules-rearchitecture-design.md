# Design: `shiny_app/app.py` — true Shiny-modules rearchitecture

- **Date:** 2026-07-14
- **Status:** Draft (awaiting user review)
- **Author:** Arturas Razinkovas-Baziukas (with Claude)
- **Scope:** `shiny_app/`. Convert the ~5,000-line `server()` closure into 17 cohesive
  `@module.ui`/`@module.server` Shiny modules behind an explicit shared-state contract, leaving
  `app.py` a thin assembler. This is the "full Shiny-modules rearchitecture" deferred in §7 of the
  2026-07-12 phase-1 decomposition design — the largest, highest-risk remaining item.

## 1. Context & motivation

The six prior decomposition phases (v0.3.3–v0.3.8) pulled **non-reactive** helpers out of `app.py`
(8,616 → ~5,600 lines) into leaf modules. What remains is the reactive core: `server()` is a single
~5,000-line closure (lines 538–5584) holding **~60 reactive handlers** and **~25 `reactive.Value`
cells plus 4 thread-shared buffers**, all in one namespace where anything can touch anything. The
UI is already split (`ui_panels`/`ui_chrome`/`ui_scripts`) but every widget lives in one global id
space.

The repo already contains **one** "module" — `diagnostics` — but it is a *plain-function
pseudo-module*: `diagnostics_ui()` takes no `id` and uses manually-prefixed ids (`diag_*`), and
`diagnostics_server(input, output, session, root_dir)` receives the **global** trio. It is
effectively the server-only (flat-id) pattern.

## 2. Goal / non-goals

- **Goal:** every feature area leaves `app.py` as a true, namespaced Shiny module
  (`@module.ui`/`@module.server`); `app.py` becomes a thin assembler; cross-tab coupling becomes an
  explicit, minimal shared contract instead of ambient closure state.
- **Non-goals:** changing any user-facing behavior, layout, or flow; rewriting the custom nav;
  touching any `.f90`; reworking the leaf modules already extracted.
- **Invariant:** at every commit the app boots, every tab renders, and every flow behaves
  identically — guarded by the full unit suite + Playwright/Selenium + a per-tab boot smoke.

## 3. Chosen approach and rejected alternatives

**Chosen — Path A, true Shiny modules** (`@module.ui`/`@module.server`, automatic id namespacing),
including converting `diagnostics` to match. Gives enforced encapsulation (a module *cannot* read
another module's `input`), textbook-idiomatic Shiny, and collision-proof ids. Cost, accepted:
within-tab ids namespace (`#param_category` → `#parameters-param_category`), so a handful of
integration-test selectors and the fat-tab UI wrappers change.

**Rejected — Path B, server-side logical modules** (keep flat ids, extend the `diagnostics`
pattern). Lower risk and DOM-identical, but leaves encapsulation by-convention only; the user chose
the idiomatic, enforced-namespacing end state after seeing this trade-off twice.

**Rejected — Path C, hybrid** (B now, module.ui later). Defers the goal without reducing total work.

## 4. Architecture

**`app.py` end state — a thin assembler.** Keeps: imports, the startup-diagnostics block,
module-level constants (`COMPILERS`, `BUILD_TYPES`, `NAV_CHOICES`, `ROOT`, …), `create_ui()`, and a
`server()` that does exactly three things — construct the shared state, call each
`x_server("<id>", state)` once, nothing else. Target: a few hundred lines.

**True modules coexist with the custom nav (load-bearing rule).** The nav is a hidden global
`input.navigation` text input + custom sidebar links (`data-nav-id`) + `panel_conditional`
wrappers + `nav_script` JS. **This mechanism stays app-level and un-namespaced.** Each tab module
supplies only the *content* of its panel via `@module.ui`; the `panel_conditional` wrapper stays in
`create_ui()`:

```python
# create_ui() — app level, nav stays global:
ui.panel_conditional("input.navigation === 'nav_parameters'",
                     parameters_ui("parameters"))    # module UI, namespaced inside
```

The JS condition still references the un-namespaced `input.navigation` (works); `data-nav-id` links
and their tests keep working; only widgets *inside* each panel get the `parameters-` prefix. All
hard-coded DOM ids in the JS (`custom-sidebar`, `.nav-link`, `navigation`, the offcanvas ids,
`theme-icon`) are app-level chrome — **namespacing touches no JS** (audited).

**Per-module pattern** (generalizes the converted `diagnostics`):
- `x_ui(id)` — `@module.ui`, returns panel *content* only (no `panel_conditional`, no nav knowledge).
- `x_server(id, state)` — `@module.server`, receives its *namespaced* `input/output/session` plus
  the shared `state`; registers that tab's handlers; owns its tab-local `reactive.Value`s privately.

## 5. Shared-state contract

Namespaced modules cannot reach across tabs, which forces the coupling to be explicit. An
**exhaustive audit** of every `input.X` read (§5.1) found the run/build/command/output *configuration*
is genuinely cross-cutting — the dashboard mirrors build+run+command state, and the run action needs
config owned by several tabs. So the shared surface is larger than a naïve tab-by-tab split assumes,
but it is bounded and named: one fat `RunController` (the whole run/build session) plus a small
`AppState`.

**`RunController` — the run/build *session* (plain per-session class, instantiated once in `server()`).**
Beyond the subprocess machinery it carries the build/command configuration that dashboard,
model_build, and run_control all touch, so no module reaches into another's inputs for it:

```python
class RunController:
    def __init__(self):
        self.process = None                        # was _model_process[0]
        self.last_run_time = None                  # was _last_run_time[0]
        self.build_log_lines = []                  # was _build_log_lines (thread-appended)
        self.run_log_lines = ["Ready.\n"]          # was _log_lines (thread-appended)
        self.exe_list_version = reactive.Value(0)  # bumped after a successful build
        self.active_executable = reactive.Value(None)  # built/selected exe — model_build ↔ dashboard
        self.build_config = None                   # reactive.Calc registered by model_build
        self.command_config = None                 # reactive.Calc registered by run_control
    def execute_build(self, build_config, ...): ...  # was _execute_build_process
    def start_run(self, command_config, ...): ...    # the on_run subprocess launch
    def stop(self): ...                              # on_stop_run / on_dashboard_stop body
    def is_running(self):
        return self.process is not None and self.process.poll() is None
```

`build_config` (compiler, build_type, exe_name, skip/clean flags — from model_build's inputs) and
`command_config` (`cmd_*`, run_executable — from run_control's inputs) are `reactive.Calc`s each
**registered by its owning module** and read by the others (dashboard's status/quick-run preview,
run_control's build-run). Threads still append to the plain-list buffers; renders still poll via
`invalidate_later(0.5)` — behavior verbatim. Side benefit: build-command assembly and run-state
transitions become unit-testable for the first time.

**`AppState` — the shared reactive bundle (dataclass, created in `server()`, passed to every module):**

```python
@dataclass
class AppState:
    run: RunController                      # the run/build session above
    navigate: Callable[[str], None]         # switch the global nav from inside a module
    selected_output_dir: reactive.Value     # ┐ output selection, published by output_browser,
    selected_output_file: reactive.Value    # │ read by plot / mass_balance / observations
    selected_output_format: reactive.Value  # ┘ (format was missing from the first draft — audit caught it)
    output_config_version: reactive.Value   # output-config save → dashboard refresh
    sim_config_version: reactive.Value      # sim-config save → dashboard input_txt_variables refresh
```

The cross-tab contract is the `RunController` (run/build session) plus this 7-field `AppState`:
`navigate`, the 3-field output selection, and 2 change-signal counters. A module needing nothing
shared still takes `state` for uniformity and ignores it.

`sim_config_version` exists because the dashboard's `input_txt_variables` re-fires when the sim
config is saved (the one *private* value read cross-tab); rather than expose `sim_config_save_msg`,
`sim_config` bumps a counter the dashboard observes (like `output_config_version`).

### 5.1 Cross-module input audit (the reason the surface is this size)

Every `input.X` read was scanned for reads spanning more than one module's region. **10 crossed a
boundary**; each is now served by a published shared value instead of a cross-module `input` read:

| Cross-read input(s) | Owner module | Read by | Served via |
|---|---|---|---|
| `cmd_constants_file`, `cmd_binary_enabled`, `cmd_shear_stress_file`, `cmd_input_file`, `cmd_binary_filename`, `run_executable` | run_control (defined in `panel_model_control`) | dashboard, run_control | `run.command_config` |
| `build_type` (+ compiler/exe_name/skip/clean) | model_build | run_control (build-run) | `run.build_config` |
| `active_executable` | model_build | dashboard | `run.active_executable` |
| `output_dir_select`, `plot_output_file`, `output_format` | output_browser | plot, mass_balance, obs (diagnostics is self-contained — own `diag_output_dir`) | `selected_output_dir/file/format` |
| `sim_output_dir` | sim_config | output_browser | `sim_config` publishes it into the output-selection bus |

This makes the headline invariant — *no `input.X` crosses a module boundary* — provably complete,
not aspirational.

**Two patterns the contract relies on:**
- **Publish-to-bus for cross-module input values.** The owning module publishes:
  `@reactive.effect: state.selected_output_dir.set(input.output_dir_select())`; consumers read
  `state.selected_output_dir()`. One writer, many readers. (Config bundles use the `reactive.Calc`
  variant registered onto `RunController`.)
- **`navigate()` for cross-tab goto buttons.** `state.navigate(nav_id)` calls
  `session.send_custom_message("aquabc_navigate", {navId})`; ~3 lines added to `nav_script` handle
  it (set the nav input + active link). The custom nav is otherwise untouched.

**Everything else stays private** to its module: `param_*`, `ic_*`, `options_*`, `sim_config_obj`,
`scenario_*`, `mb_*`, `obs_*`, `csv_cache*`, `file_list_version`, `save_status_msg`. (A cross-region
read audit confirmed these stay within their cluster; the sole exception, `sim_config_save_msg`, is
handled by `sim_config_version` above.)

## 6. Module inventory (17 modules)

A module = one cohesive feature. `chrome` (17) is the one non-tab module — the app-level help and
changelog offcanvas renders that belong to no nav panel. The last column is the only cross-tab
coupling; everything else is private. A handler-by-handler pass confirmed all ~60 handlers map to
exactly one module (the anonymous file-list effect → `input_files`; `input_txt_variables` →
`dashboard`; `help_content`/`changelog_content` → `chrome`).

**Fat-tab UI splitting differs by tab (verified against `ui_panels.py`):** `model_control` splits
cleanly on `navset_card_tab` boundaries — sub-tab 1 → `sim_config_ui`, sub-tabs 2–3 → `run_control_ui`
— so the outer navset stays in `create_ui()` and each module returns whole `nav_panel`s. `plot` and
`output_browser`, however, are **interleaved within one sub-tab** (a `layout_columns` mixing
`output_browser`'s file-preview card with `plot`'s variable/options cards), so those two module UIs
return **individual cards** that `create_ui()` composes into the shared layout — finer-grained than
sub-tab stacking. Module UIs that need app-level consts (`panel_model_build`→compilers/build_types,
`panel_plot`→min_smooth_window) receive them as `@module.ui` args or import them.

> **Decided (plot / output_browser split): keep the split.** The card-level composition cost in
> `create_ui()` is accepted in exchange for the single-writer isolation of the output-selection bus.

| # | Module `id` | Nav tab | Absorbs (theme) | Shared-state touch |
|---|---|---|---|---|
| 1 | `dashboard` | Dashboard | status_info, run_log_mini, dashboard_* mirrors, system_status_compact, run_timer_display, quick_run, input_txt_variables, copy/goto buttons | reads `run` (logs/is_running/last_run/`active_executable`/`command_config`), `exe_list_version`, `output_config_version`, `sim_config_version`; calls `run.stop()`, `navigate()` |
| 2 | `model_structure` | Model Structure | model_structure_iframe | — |
| 3 | `model_build` | Model Build | compiler_status, build_flags, target_exe_name, executable list/info, on_build, on_rebuild, build_log, refresh/init executables | **registers** `run.build_config`; calls `run.execute_build()`; writes `run.exe_list_version`, `run.active_executable`; `navigate()` |
| 4 | `input_files` | Input Files | refresh/load/save file, file_info_panel, save_status, map_display | private (`file_list_version`, `save_status_msg`) |
| 5 | `parameters` | Parameters | load/save params, param_table, category/save info | private (`param_*`) |
| 6 | `initial_conditions` | Initial Cond. | load/save ICs, ic_table, category/save info | private (`ic_*`) |
| 7 | `model_options` | Model Options | load/save options, switches, constants | private (`options_*`) |
| 8 | `scenarios` | Scenarios | manager init, refresh/load/save/delete, status | private (`scenario_*`) |
| 9 | `mass_balance` | Mass Balance | calculate, summary/details/plot | reads `selected_output_dir` |
| 10 | `observations` | Observations | scan/preview/load obs, comparison, metrics, scatter | reads `selected_output_dir/file` |
| 11 | `map` | Map | pydeck_map, map_info | — |
| 12 | `diagnostics` | Diagnostics | *(convert existing pseudo-module → true module)* | — (own `diag_output_dir` selector) |
| 13 | `sim_config` | Model Config ▸ sub-tab | load/save sim config, timestep/output presets, duration/timestep/output info | **writes** `sim_config_version`; publishes `sim_output_dir` (private `sim_config_obj`) |
| 14 | `run_control` | Model Config ▸ sub-tab | cmd dropdowns/command builder/preview (owns `cmd_*`), on_run, on_build_run, run_log, progress, stop, constants_validation, output-config load/save, output-dir select | **registers** `run.command_config`; reads `run.build_config`; calls `run.start_run()`/`run.stop()`, `run.execute_build()` (build-run); reads `exe_list_version`; writes `output_config_version`, `selected_output_dir`; `navigate()` |
| 15 | `plot` | Plots ▸ sub-area | main_plot, variable-choice updates, input-timeseries, CSV cache | reads `selected_output_dir/file/format` (private `csv_cache*`) |
| 16 | `output_browser` | Plots ▸ sub-area | output-dir discovery, file preview/summary, out_preview, plot-file selection | **writes** `selected_output_dir/file/format` (sole publisher) |
| 17 | `chrome` | *(none — offcanvas)* | help_content, changelog_content | — |

`output_browser` (16) is the single writer of the selected-output state read by 9, 10, 15 —
a one-writer/many-reader bus. **9 of 17 modules touch nothing shared** (2, 4, 5, 6, 7, 8, 11, 12, 17),
converting cleanly.

Two **pure** server helpers are called cross-module — `get_executable_info` (model_build +
run_control) and `get_selected_output_file_path` (plot + output_browser). They move to leaf modules
(`compiler_env`/`file_locators` and `output_data` respectively), not into one tab, so both callers
share one copy. (`build_estas_command` similarly becomes the `run.command_config` reactive; the CSV
cache is genuinely plot-private.)

> **Decided (chrome, 17): keep it a module.** `help_content`/`changelog_content` become a `chrome`
> module so `app.py`'s `server()` stays a *pure* assembler (its stated success criterion).

## 7. Phasing & per-module validation gate

**Contract-first is the key de-risk:** a tab conversion is atomic (all its ids namespace at once),
but the shared plumbing can be introduced with zero behavior change first, proving it before any id
moves. ~19 commits total; the app is runnable and shippable at every one.

- **Phase 0 — shared contract, zero namespacing (1 commit).** Add `RunController`, `AppState`,
  `navigate()` + the ~3-line nav-JS message handler; refactor the *still-monolithic* `server()` to
  use them: `_model_process`/`_log_lines`/`_execute_build_process` → `RunController`; the `build_config`
  and `command_config` `reactive.Calc`s + `active_executable` registered onto `run`;
  `input.output_dir_select`/`plot_output_file`/`output_format` (and `sim_output_dir`) → published into
  `AppState`; the `output_config_version`/`sim_config_version` counters wired so `input_txt_variables`
  reads them from `AppState`; goto → `navigate()`. No id changes, DOM byte-identical. This phase
  proves the *entire* §5.1 audit resolution before any namespacing. Add `test_run_controller.py`.
- **Phase 1 — pilot module `parameters` (1 commit).** Self-contained but *has* breaking selectors
  (`#param_category`, `#load_params`), so it exercises the full loop: `@module.ui`/`@module.server`,
  the namespaced-selector test update, and a load/edit/save flow. Establishes the file layout and
  the `nid(module_id, input_id)` test helper.
- **Phase 2 — remaining leaf modules (8 commits):** `model_structure`, `map`, `model_options`,
  `initial_conditions`, `input_files`, `scenarios`, `sim_config`, `chrome`. Each touches nothing
  shared except `sim_config` (which only *writes* `sim_config_version`).
- **Phase 3 — output-selection cluster (5 commits):** `output_browser` first (sole publisher), then
  readers `plot`, `mass_balance`, `observations`, `diagnostics` (which also converts to true-module).
- **Phase 4 — run/build cluster, last because most coupled (3 commits):** `model_build`,
  `run_control`, `dashboard`.
- **Phase 5 — cleanup (1 commit):** delete the dead `panel_sim_config` placeholder, collapse
  `server()` to the thin assembler, final full suite + E2E + visual smoke, update docs/memory/CHANGELOG.

**Per-module gate (every conversion commit):** (1) `py_compile` + import succeed; (2) full Python
suite green (155 + new per-module tests); (3) that tab's selectors updated to namespaced ids,
Playwright + Selenium green; (4) boot smoke via the `run`/`verify` skill. Any red → stop and fix.

## 8. Risks & mitigations

| Risk | Mitigation |
|---|---|
| A handler silently reads another tab's `input.X` → `None`/error once namespaced | The **§5.1 audit already enumerated all 10 cross-module reads** and assigned each a published shared value (lifted in Phase 0); re-run the per-module grep at each conversion as a backstop. |
| A `panel_conditional` on `input.navigation` pulled inside a module → `mod-navigation`, never matches | Wrappers stay in `create_ui()`; module UIs return inner content only (§4). |
| Output/handler id mismatch on a partial move | Conversion is atomic — UI fragment + all its handlers move together in one commit. |
| Behavior drift during a "move" | Move verbatim, no logic edits in a conversion commit; E2E + per-tab visual smoke. |
| Fat-tab split across two module UIs | `model_control` splits on `nav_panel` boundaries (outer navset in `create_ui()`); `plot`/`output_browser` interleave, so their module UIs return **cards** composed by `create_ui()` (§6). DOM diffed either way. |
| `AppState`/`RunController` constructed at import → shared across sessions | Constructed inside `server()` only; never at module level. |
| New `modules/` import cycle or script-mode failure | Modules import only stdlib + leaf modules + the shared `app_state` module, never `app.py`; established `try/except ImportError` fallback. |
| Selenium/Playwright selector drift | Update the ~10 within-tab selectors per-commit with the renaming module; `nid()` helper centralizes; nav-level selectors unaffected. |
| Background-thread log append vs reactive read | Buffers stay plain lists (atomic append), polled via `invalidate_later(0.5)` — ported verbatim. The one `reactive.Value.set()` is already main-context. |

## 9. Success criteria (end state)

- `app.py` is a thin assembler: `server()` = construct `state` + 17 `x_server(...)` calls; file
  drops from ~5,600 to a few hundred lines.
- 17 cohesive `@module.ui`/`@module.server` modules; `diagnostics` converted.
- Cross-tab shared surface is exactly the `RunController` (run/build session, carrying
  `build_config`/`command_config`/`active_executable`) plus the 7-field `AppState`; **no `input.X`
  (nor private `reactive.Value`) crosses a module boundary** — verified by the §5.1 audit — except
  via `run`/`AppState`.
- `RunController` unit-tested; per-module tests where logic warrants; all integration tests green
  with namespaced selectors; full suite green; app visually verified.

## 10. Test-migration strategy

Namespaced ids are deterministic (`moduleid-inputid`) and stable, so **rewrite the ~10 broken
selectors to their namespaced form** (option chosen over `data-testid`, whose stability payoff is
moot here, and over a module test-harness rethink, which py-shiny does not support well and which
would trade away E2E coverage). Each selector rewrite lands in the same commit as the module that
renames it. Optional `nid(module_id, input_id)` helper centralizes the convention. `data-nav-id`
nav-level tests are unaffected (nav stays global).

## 11. Conventions

- **File layout:** new modules live in a `shiny_app/modules/` subpackage (17 files would clutter
  the ~24-file flat dir); import fallback becomes `from shiny_app.modules.parameters import …` /
  `from modules.parameters import …`. `RunController` and `AppState` both live in
  `shiny_app/app_state.py`.
- **Release cadence:** map phases to a new `v0.4.x` line — Phase 0+pilot → `v0.4.0`, then a release
  per phase — matching the frequent-small-release rhythm.

## 12. Files touched

- **New:** `shiny_app/app_state.py` (`RunController` + `AppState`); `shiny_app/modules/*.py`
  (17 module files); `tests/python/test_run_controller.py` + per-module tests as warranted.
- **Modified (incrementally):** `shiny_app/app.py` (shrinks to assembler); `ui_scripts.py`
  (nav-JS `aquabc_navigate` handler); `shiny_app/diagnostics.py` (pseudo → true module);
  the ~10 broken integration-test selectors; `CHANGELOG.md`, `TODO_IMPLEMENTATION_PLAN.md`.
- **Out of scope:** `create_ui()` layout semantics (only the `panel_conditional` wrappers stay, now
  wrapping module UIs), any `.f90`, the already-extracted leaf modules.
