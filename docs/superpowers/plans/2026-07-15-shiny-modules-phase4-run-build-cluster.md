# Shiny-modules Rearchitecture — Phase 4 (run/build/dashboard cluster) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the last three inline tabs of `shiny_app/app.py`'s `server()` — `model_build`, `run_control`, `dashboard` — into true namespaced `@module.ui`/`@module.server` modules, leaving `server()` a thin assembler (state construction + module calls + the two app-level chrome renders).

**Architecture:** This is the **most coupled** cluster: dashboard reads run/build state that model_build and run_control own. Rather than move-and-namespace in one shot (which would break cross-module `input.X` reads the moment ids namespace), the phase is **contract-first** (the Phase-0 playbook): first make every cross-module value flow through the already-existing `RunController` (`run.command_config`/`run.build_config`/`run.active_executable`) and `AppState`, with **zero id namespacing** and a DOM-identical result — proving the wiring — and only then namespace each module atomically. A dead-code sweep precedes the conversions so ~260 lines of confirmed-dead handlers are dropped, not migrated.

**Tech Stack:** Python 3.10, Shiny for Python 1.5.1 (`@module.ui`/`@module.server`, `session.root_scope()` cross-namespace bridge), pytest, ruff. Modules live in `shiny_app/modules/`.

## Global Constraints

*(Every task's requirements implicitly include this section. Values copied verbatim from the codebase state at plan time — RE-GREP current line numbers at execution: they shift as each task removes code. Names/ids are stable; line numbers are the v0.4.3 baseline.)*

- **Behavior/DOM-identical except the within-tab id namespace.** Only ids inside a converted tab gain the `<module>-` prefix; nav ids (`nav_dashboard`, `nav_model_build`, `nav_model_control`) and the global `navigation` input stay global.
- **Module recipe** (12 modules already follow it — templates: `shiny_app/modules/sim_config.py` for a fat-tab/nav UI, `shiny_app/modules/plot.py` for a large module, `shiny_app/modules/parameters.py` for the base shape): `x_ui(...)` returns panel **content** only — the `panel_conditional("input.navigation === 'nav_X'", …)` moves to `create_ui()`; `x_server(input, output, session, state)`; self-contained (stdlib + existing leaf modules only, nothing from `app.py`); `logger = logging.getLogger("AQUABC")`; self-compute `ROOT` for the `modules/` depth (`os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))`); `try/except ImportError` import fallback (`from shiny_app.modules.X import …` / `from modules.X import …`).
- **No `input.X` crosses a module boundary.** Cross-module values flow via the shared contract only: `run.command_config` (run_control→dashboard), `run.build_config` (model_build→run_control), `run.active_executable` (model_build→dashboard), `state.output_config_version` / `state.sim_config_version` counters, `state.navigate()`. The one exception is the sibling **`sim_output_dir`** SELECT widget (a bidirectionally-written input, not a one-way value) reached via `session.root_scope()` — see the sim_output_dir constraint below.
- **`sim_output_dir` bridge (the top Phase-4 risk).** `sim_output_dir` (widget defined `ui_panels.py:350`, in `panel_model_control`'s Output Config sub-tab) is written by three modules: `run_control` (owner: `refresh_sim_output_dirs`, `sim_output_dir_info`), `plot` (`init_output_dirs` updates its choices, `plot.py:463`), and `sim_config` (`load_simulation_config_file` sets it `sim_config.py:210`; the save handler reads it `sim_config.py:351`). Today plot/sim_config reach it via `session.root_scope()` + the literal id `"sim_output_dir"` because it lives at root. Once `run_control` owns it (Task 4), it becomes `run_control-sim_output_dir` and is **no longer at root** — the literal-id bridge silently breaks (reads return `None`, updates target a non-existent id). **Resolution (chosen): repoint the bridge id, keep the pattern.** `session.root_scope()` reaches the root (namespace `""`); from root, the fully-qualified id `"run_control-sim_output_dir"` is addressable. So the three external bridge sites change the literal id `"sim_output_dir"` → `"run_control-sim_output_dir"` (root_scope call itself is unchanged). Task 4 makes this change atomically with the namespacing and boot-smokes the cross-tab round-trip. *(Alternative considered and rejected for now: promoting the value to a shared `reactive.Value` on `AppState` — rejected because the widget is bidirectionally written by three modules, so a one-writer bus does not fit and you would still need `ui.update_select` on the real widget.)*
- **Conversion commits move handlers verbatim** (no logic edits) except the explicitly-specified cross-module rewirings (input read → `run.*`) and id namespacing. Dead code is dropped in Task 1, not carried into a module.
- **`shiny_app/modules/` + `shiny_app/diagnostics.py` are fully ruff-gated; `tests/python/` is ruff-gated.** `app.py` has an F401/F841 per-file-ignore (mid-decomposition). Keep new module files lint-clean.
- **`tests/python/` is a package** (has `__init__.py`) — no bare `from nsutil import`; inline the `nid(module_id, input_id)` helper in each test (`return f"{module_id}-{input_id}"`).
- **Use `.venv/bin/python`** for anything importing `app.py` — the system `python3` lacks `networkx` (masks a false "import fails"; `.venv` is the real env, `pytest` runs there).
- **Per-task gate:** `py_compile` + import; full suite green; that tab's selectors migrated to namespaced ids (same commit); ruff clean; boot smoke (`create_ui().tagify()` shows the tab's namespaced ids, zero bare-id leaks; `.venv` import constructs the `App`). Commit per task.

---

## File Structure

**New module files (`shiny_app/modules/`):**
- `model_build.py` — `model_build_ui(compilers, build_types)` + `model_build_server(input, output, session, state)`. Owns the Model Build tab; **registers** `run.build_config` (as `reactive.Calc`); sets `run.active_executable`; updates its own `active_executable` + the sibling `run_control-run_executable` (via root_scope).
- `run_control.py` — `run_control_ui()` (returns a `ui.TagList` of the **Run Model** + **Output Config** `nav_panel`s, composed into `panel_model_control`'s navset in `create_ui`) + `run_control_server(input, output, session, state)`. Owns `cmd_*`, `run_executable`, `run`, `stop_run`, `goto_build`, `copy_mini_log`/`run_log_mini`/`run_status_indicator` (ids physically in Run Model tab), and the whole Output Config sub-tab (`output_boxes`/`output_types`/`load_output_config`/`save_output_config`/`sim_output_dir`/`refresh_sim_output_dirs`/`sim_output_dir_info`). **Registers** `run.command_config` (`reactive.Calc`); reads `run.build_config`; writes `state.output_config_version`.
- `dashboard.py` — `dashboard_ui()` + `dashboard_server(input, output, session, state)`. Owns the Dashboard tab; a pure consumer of `run.*` (logs, `is_running`, `last_run_time`, `run.command_config()`, `run.active_executable()`) + `state.output_config_version`/`sim_config_version`; calls `run.stop()`, `state.navigate()`.

**New test files (`tests/python/`):** `test_model_build_module.py`, `test_run_control_module.py` (`.tagify()` — fat-tab nav UI), `test_dashboard_module.py`. Update `test_run_controller.py` if `RunController` gains typed `command_config`/`build_config` fields.

**Modified:** `shiny_app/app.py` (shrinks — server() → assembler); `shiny_app/ui_panels.py` (`panel_dashboard`/`panel_model_build`/`panel_model_control` → module UI content; drop the internal `panel_conditional`s / the whole `panel_model_control` navset moves to `create_ui`); `shiny_app/app_state.py` (only if `RunController.command_config`/`build_config` are typed as `reactive.Calc` fields — likely no change, they are assigned in `server()`); `shiny_app/modules/plot.py` + `shiny_app/modules/sim_config.py` (repoint the `sim_output_dir` bridge id, Task 4); the integration-test selectors for these three tabs.

**Out of scope:** any `.f90`; the already-converted 12 modules (except the plot/sim_config bridge repoint); `create_ui()` layout semantics beyond moving the `panel_conditional` wrappers and the `model_control` navset.

---

## Task 1: Dead-code removal (verify-then-drop)

**Files:** Modify `shiny_app/app.py` (drop dead handlers/vestiges).

**Rationale:** The inventory found ~260 lines of inline handlers with **zero UI references** (verified at plan time: 0 `output_*` refs / 0 trigger widgets). Dropping them now keeps the later conversion diffs clean (no dead code migrated into modules). This is behavior-neutral on all **live** paths.

**Interfaces:** Produces a smaller `server()`; no signatures change. Later tasks assume these names are gone.

- [ ] **Step 1: Re-verify each item is dead (guard against stale line numbers).** Run and confirm each returns zero:
```bash
cd /home/razinka/AQUABCv0.2
# Dead render outputs — expect 0 each:
for id in status_info run_log run_progress_bar; do
  echo -n "$id output refs: "; grep -rc "\"$id\"\|'$id'" shiny_app/ui_panels.py shiny_app/ui_chrome.py | grep -v ':0' | grep -iE 'output' || echo 0
done
# Dead event triggers (no UI widget) — expect 0 each:
for id in build_run cmd_skip_build cmd_clean_before_build; do
  echo -n "$id widget: "; grep -rc "ui.input.*\"$id\"\|ui.input.*'$id'" shiny_app/ui_panels.py shiny_app/ui_chrome.py | grep -v ':0' || echo 0
done
```
Expected: every count `0`. If ANY is non-zero, STOP — that item is live; do not drop it, report to controller.

- [ ] **Step 2: Drop the dead handlers.** Remove from `shiny_app/app.py` (re-grep current line ranges — v0.4.3 baseline shown):
  - `status_info` (`@render.text`, ~L608-696) — no `output_text("status_info")` anywhere.
  - `run_log` (`@render.text`, ~L1991-1996) — no output.
  - `run_progress_bar` (`@render.ui`, ~L2000-2054) — no output.
  - `run_command` helper (~L1791-1824) + `on_build_run` (`@reactive.event(input.build_run)`, ~L1826-1914) — triggered only by non-existent `build_run`/`cmd_skip_build`/`cmd_clean_before_build`.
  - `INPUT_TXT_PATH = os.path.join(ROOT, "INPUT.txt")` (~L1667) — its only documented consumer (`init_output_dirs`) now lives in `plot.py` with its own `INPUT_TXT_PATH`; grep-confirm zero other references in `app.py` before dropping.
  - **KEEP** `state.run.active_executable = reactive.Value(None)` (~L581) — currently never `.set()`, but Task 2 activates it as the model_build→dashboard bridge.

- [ ] **Step 3: Verify nothing live referenced them.**
```bash
cd /home/razinka/AQUABCv0.2
grep -nE "\bstatus_info\b|\brun_command\b|\bon_build_run\b|\brun_progress_bar\b|\bINPUT_TXT_PATH\b" shiny_app/app.py || echo "clean — all dropped names gone"
.venv/bin/python -c "import shiny_app.app; print('import OK')"
```
Expected: "clean", "import OK".

- [ ] **Step 4: Full suite + ruff.**
```bash
.venv/bin/python -m pytest tests/python/ -q
ruff check shiny_app/ tests/python/
```
Expected: 176 passed (no test targeted the dead code); ruff clean.

- [ ] **Step 5: Commit.**
```bash
git add shiny_app/app.py
git commit -m "refactor(shiny): drop dead run/build handlers before Phase-4 conversion

status_info, run_log, run_progress_bar, run_command+on_build_run, and the
INPUT_TXT_PATH vestige had zero UI references (verified). Behavior-neutral."
```

---

## Task 2: Contract-first cross-module rewiring (zero namespacing, DOM-identical)

**Files:** Modify `shiny_app/app.py` (still-inline handlers).

**Rationale:** Before any id namespaces, make every cross-module value flow through `RunController`, so the later per-module namespacing cannot break a cross-tab read. **No id changes — DOM byte-identical.** This mirrors Phase 0's contract-first de-risk.

**Interfaces produced (later tasks rely on these):**
- `run.command_config` — a `reactive.Calc` returning the assembled ESTAS command string (the current `build_estas_command()` value). Read by dashboard.
- `run.build_config` — a `reactive.Calc` returning the current build config dict (the current `_current_build_config()` value). Read by run_control.
- `run.active_executable` — the existing `reactive.Value`, now `.set()` by a model_build effect mirroring `input.active_executable()`. Read by dashboard.

- [ ] **Step 1: Convert `run.command_config` to a `reactive.Calc`.** The current registration (~L778) is `run.command_config = build_estas_command` (a plain function reading `input.run_executable()`/`input.cmd_*`). Wrap it so it is reactive and memoized:
```python
    @reactive.calc
    def _command_config():
        return build_estas_command()   # unchanged body at ~L740-776
    run.command_config = _command_config
```
Keep `build_estas_command` as the inner helper (still called by `cmd_preview`, `on_run`). The **value** is identical; only the registration becomes reactive.

- [ ] **Step 2: Convert `run.build_config` to a `reactive.Calc`.** Current (~L1022-1029) `_current_build_config` is a plain closure assigned to `run.build_config`. Make it:
```python
    @reactive.calc
    def _build_config():
        return {                     # unchanged body — reads build_compiler/build_type/build_clean_first
            ...
        }
    run.build_config = _build_config
```

- [ ] **Step 3: Activate the `run.active_executable` bridge.** Add an effect (near model_build's handlers) mirroring the input into the shared value:
```python
    @reactive.effect
    def _publish_active_executable():
        run.active_executable.set(input.active_executable())
```

- [ ] **Step 4: Route dashboard's cross-bucket reads through `run.*`.** These three handlers currently read run_control/model_build inputs directly; rewire the cross-reads only (leave all other logic verbatim):
  - `dashboard_exe_text` (~L2182-2187): `input.active_executable()` → `run.active_executable()`.
  - `system_status_compact` (~L2113-2176): the `input.run_executable()` read + the direct `build_estas_command()` call (~L2154/2166) → `run.command_config()`.
  - `handle_quick_run` (~L1172-1411): replace the command-assembly reads (`input.run_executable()` L1205, the `build_estas_command()` call L1201, and the `input.cmd_constants_file()`/`cmd_binary_enabled()`/`cmd_shear_stress_file()` reads used to build the command) with `run.command_config()`. Leave the subprocess-launch body (which writes `run.process`/`run.running`/`run.last_run_time`/`run.progress`/`run.run_log_lines`) verbatim — those are `run.*` attrs, namespace-safe. *(Deduping this ~140-line block against `RunController.start_run` is out of scope — a separate refactor; do not attempt it here.)*

- [ ] **Step 5: Write a behavior-pin test for the reactive configs.** In `tests/python/test_run_controller.py`, add a test that constructs a `RunController` and asserts `command_config`/`build_config`/`active_executable` are assignable and readable as the contract expects (the attrs exist and default sensibly). Keep it to what `RunController` exposes without a live session; the full cross-module round-trip is covered by the boot smoke + integration-tests.
```bash
.venv/bin/python -m pytest tests/python/test_run_controller.py -v
```
Expected: PASS (existing 155→ +your assertions).

- [ ] **Step 6: DOM-identical boot smoke (no id changed).**
```bash
.venv/bin/python -c "
from shiny_app.app import create_ui
html = str(create_ui().tagify())
# no namespacing yet — the cluster ids stay bare:
for bare in ('quick_run','build_compiler','run_executable','sim_output_dir','dashboard_status_text'):
    assert 'id=\"%s\"' % bare in html or '\"%s\"' % bare in html, bare
print('DOM still bare-id (contract-first, no namespacing) — OK')
"
.venv/bin/python -m pytest tests/python/ -q
ruff check shiny_app/ tests/python/
```
Expected: bare ids still present (nothing namespaced), full suite green, ruff clean.

- [ ] **Step 7: Commit.**
```bash
git add shiny_app/app.py tests/python/test_run_controller.py
git commit -m "refactor(shiny): route run/build cross-module reads through RunController (contract-first)

command_config/build_config become reactive.Calc; active_executable published
from its input; dashboard reads run.command_config()/run.active_executable()
instead of sibling inputs. Zero id namespacing — DOM byte-identical."
```

---

## Task 3: `model_build` module

**Files:** Create `shiny_app/modules/model_build.py`; create `tests/python/test_model_build_module.py`; modify `shiny_app/app.py` (remove the inline block; add `model_build_server` call), `shiny_app/ui_panels.py` (`panel_model_build` → module UI content), and the model_build integration selectors.

**Interfaces:**
- Consumes: `state` (AppState). Reads its own `build_compiler`/`build_type`/`build_clean_first`/`active_executable`/`btn_*` inputs.
- Produces: registers `run.build_config` (`reactive.Calc`, moved here from server); `.set()`s `run.active_executable`; keeps the `run.execute_build()` calls. Updates its own `active_executable` select and the **sibling `run_executable`** select (run_control not yet converted → still at root; bridge via `root_scope()` + literal `"run_executable"`, repointed in Task 4).

- [ ] **Step 1: Write the failing render-smoke test.** `tests/python/test_model_build_module.py` (template: `tests/python/test_parameters_module.py`). `panel_model_build` is a plain `layout_columns` (no nav) → render via `str(model_build_ui("model_build", COMPILERS, BUILD_TYPES))` (import `COMPILERS`/`BUILD_TYPES` from `shiny_app.app` or pass literal lists). Assert namespaced ids present via `nid("model_build", raw)` for: `build_compiler`, `build_type`, `build_clean_first`, `btn_build`, `btn_rebuild`, `btn_refresh_executables`, `active_executable`, `btn_clear_build_log`, and outputs `compiler_status`, `build_flags_info`, `target_exe_name`, `executable_list`, `executable_info`, `build_log`. Assert `"input.navigation" not in html`.
```bash
.venv/bin/python -m pytest tests/python/test_model_build_module.py -v
```
Expected: FAIL (module doesn't exist).

- [ ] **Step 2: Create `shiny_app/modules/model_build.py`.** Follow the recipe. `@module.ui def model_build_ui(compilers, build_types):` returns the **content** of `panel_model_build` (strip its internal `panel_conditional`; the compilers/build_types come in as args, matching the current `panel_model_build(COMPILERS, BUILD_TYPES)` signature). `@module.server def model_build_server(input, output, session, state): run = state.run` then the verbatim handlers moved from `app.py`: `compiler_status`, `build_flags_info`, `get_target_exe_name`, `target_exe_name`, `executable_list`, `executable_info`, `build_log`, `clear_build_log`, `refresh_executables`, `init_executable_list` (+ its `_exe_list_initialized` guard), `on_build`, `on_rebuild`; the `run.build_config` `reactive.Calc` registration (from Task 2); the `_publish_active_executable` effect (from Task 2); and the shared helpers `get_available_executables`/`get_executable_info` (import from `shiny_app.build_commands` — they are thin wrappers `build_commands.get_available_executables(ROOT)` etc.; do not duplicate). For the **sibling `run_executable`** updates in `refresh_executables` and `init_executable_list`, bridge to root (run_control still inline):
```python
    root = session.root_scope()
    ui.update_select("run_executable", choices=..., selected=..., session=root)   # repointed to "run_control-run_executable" in Task 4
```
Its own `active_executable` updates stay plain (`ui.update_select("active_executable", …)`).

- [ ] **Step 3: Wire into `app.py` + `ui_panels.py`.** In `create_ui()` (~L518) replace `panel_model_build(COMPILERS, BUILD_TYPES)` with `ui.panel_conditional("input.navigation === 'nav_model_build'", model_build_ui("model_build", COMPILERS, BUILD_TYPES))`. In `server()` add `model_build_server("model_build", state)` (near the other module calls) and delete the inline model_build handlers + the Task-2 `run.build_config` Calc + `_publish_active_executable` (they moved into the module). Remove/repoint the now-unused `panel_model_build` in `ui_panels.py` (delete it, or keep as dead — prefer delete + drop its import in `app.py`). Add the `model_build` import (fallback pattern).

- [ ] **Step 4: Migrate integration selectors.** Grep tests for the model_build ids and rewrite to `model_build-*`:
```bash
grep -rn "build_compiler\|build_type\|btn_build\|btn_rebuild\|active_executable\|btn_refresh_executables\|compiler_status\|executable_list" tests/python/*.py
```
Rewrite each bare `#build_compiler` → `#model_build-build_compiler`, etc. (only for ids owned by this tab).

- [ ] **Step 5: Verify.**
```bash
.venv/bin/python -m pytest tests/python/test_model_build_module.py -v   # PASS
.venv/bin/python -c "import shiny_app.app; print('import OK')"
.venv/bin/python -c "
from shiny_app.app import create_ui
html=str(create_ui().tagify())
assert 'model_build-build_compiler' in html and 'id=\"build_compiler\"' not in html
print('namespaced, no bare leak')"
.venv/bin/python -m pytest tests/python/ -q      # full suite, +1
ruff check shiny_app/modules/model_build.py tests/python/test_model_build_module.py
```

- [ ] **Step 6: Commit** — `refactor(shiny): model_build Shiny module`.

---

## Task 4: `run_control` module (fat-tab; repoints the sim_output_dir + run_executable bridges)

**Files:** Create `shiny_app/modules/run_control.py`; create `tests/python/test_run_control_module.py`; modify `shiny_app/app.py`, `shiny_app/ui_panels.py` (Run Model + Output Config sub-tabs → `run_control_ui`; the `panel_model_control` navset moves to `create_ui`), `shiny_app/modules/plot.py` + `shiny_app/modules/sim_config.py` (repoint the `sim_output_dir` bridge id), and the run_control integration selectors.

**Interfaces:**
- Consumes: `state`; reads its own `cmd_*`/`run_executable`/`run`/`stop_run`/`output_boxes`/`output_types`/`sim_output_dir`/`goto_build` inputs; reads `run.build_config()`.
- Produces: registers `run.command_config` (`reactive.Calc`, moved from server); writes `state.output_config_version`; calls `run.start_run()`/`run.stop()`/`run.execute_build()`; owns `run_control-run_executable` and `run_control-sim_output_dir` (the repoint targets).

**⚠️ This is the highest-risk task.** The fat-tab UI returns two nav_panels; the `sim_output_dir` bridge repoints across three files; several ids physically in the Run Model tab (`copy_mini_log`, `run_log_mini`, `run_status_indicator`, `goto_build`) belong here despite thematic names.

- [ ] **Step 1: De-risk the fat-tab UI composition FIRST.** Confirm a `@module.ui` returning a `ui.TagList` of two `nav_panel`s composes into a `navset_card_tab` alongside `sim_config_ui("sim_config")`. Write a throwaway check:
```bash
.venv/bin/python -c "
from shiny import ui, module
@module.ui
def rc(): return ui.TagList(ui.nav_panel('Run Model', ui.input_text('run_executable','x')), ui.nav_panel('Output Config', ui.input_text('sim_output_dir','y')))
html=str(ui.navset_card_tab(ui.nav_panel('Sim', ui.input_text('a','a')), rc('run_control'), id='model_control_tabs').tagify())
assert 'run_control-run_executable' in html and 'run_control-sim_output_dir' in html and 'Run Model' in html and 'Output Config' in html
print('fat-tab TagList-of-navpanels composes + namespaces OK')
"
```
If this fails, fall back: have `create_ui` unpack — `ui.navset_card_tab(sim_config_ui('sim_config'), *run_control_ui('run_control'), id=...)` requires `run_control_ui` to return a list; test that variant. Pick whichever renders both sub-tabs with namespaced ids. Record the working form; use it in Step 3.

- [ ] **Step 2: Write the failing render-smoke test.** `tests/python/test_run_control_module.py` (template: `tests/python/test_sim_config_module.py` — **`.tagify()`** since the UI is nav content). Compose like create_ui: `html = str(ui.navset_card_tab(run_control_ui("run_control")).tagify())` (or the Step-1 working form). Assert `nid("run_control", raw)` present for Run Model ids (`goto_build`, `run_executable`, `cmd_input_file`, `cmd_constants_file`, `cmd_binary_enabled`, `cmd_binary_filename`, `run`, `stop_run`, `btn_copy_mini_log`; outputs `run_executable_info`, `cmd_preview`, `constants_validation_status`, `run_status_indicator`, `run_log_mini`) and Output Config ids (`output_boxes`, `sim_output_dir`, `refresh_sim_output_dirs`, `output_types`, `load_output_config`, `save_output_config`; outputs `sim_output_dir_info`, `output_config_status`). Assert both sub-tab titles ("Run Model", "Output Config") and `"input.navigation" not in html`.
```bash
.venv/bin/python -m pytest tests/python/test_run_control_module.py -v   # FAIL
```

- [ ] **Step 3: Create `shiny_app/modules/run_control.py`.** `@module.ui def run_control_ui():` returns the Step-1 working form (TagList of the Run Model nav_panel `ui_panels.py:220-328` + Output Config nav_panel `ui_panels.py:330-393`, content moved verbatim). `@module.server def run_control_server(input, output, session, state): run = state.run` then verbatim-move from `app.py`: `init_cmd_dropdowns`, `build_estas_command` (+ the `run.command_config` `reactive.Calc` registration from Task 2), `cmd_preview`, `run_executable_info` (the `get_executable_info` wrapper — import from `build_commands`), `constants_validation_status`, `navigate_to_build` (the `goto_build` handler — `state.navigate("nav_model_build")`), `on_run` (calls `run.start_run` via thread), `on_stop_run`, `copy_mini_log`, `run_log_mini`, `run_status_indicator`, `DEFAULT_CONSTANTS_FILE` (module-level const — also used by dashboard's quick_run; keep a copy or import from a shared leaf; **note**: dashboard's quick_run reads `run.command_config()` post-Task-2 so it no longer needs `DEFAULT_CONSTANTS_FILE` for the command — verify), and the Output Config cluster (`output_config_msg` reactive.Value, `OUTPUT_INFO_FILE`, `load_output_config`, `save_output_config` [writes `state.output_config_version`], `output_config_status`, `refresh_sim_output_dirs`, `sim_output_dir_info`).

- [ ] **Step 4: Repoint the `sim_output_dir` + `run_executable` cross-namespace bridges.** Now that `run_control` owns these ids:
  - In `shiny_app/modules/model_build.py` (from Task 3): change the sibling `run_executable` update id `"run_executable"` → `"run_control-run_executable"` (root_scope call unchanged).
  - In `shiny_app/modules/plot.py` (~L463, inside `init_output_dirs`): `ui.update_select("sim_output_dir", …, session=root)` → `ui.update_select("run_control-sim_output_dir", …, session=root)`.
  - In `shiny_app/modules/sim_config.py`: `ui.update_select("sim_output_dir", …, session=root)` (~L210) → `"run_control-sim_output_dir"`; and the read `root.input.sim_output_dir()` (~L351) → `root.input["run_control-sim_output_dir"]()`.
  - Update the bridge docstrings in plot.py (~L11-15) + sim_config.py to name the new fully-qualified id.

- [ ] **Step 5: Wire into `app.py` + `ui_panels.py`.** In `create_ui()` (~L519) replace `panel_model_control()` with the composed navset (moved out of `ui_panels.py`):
```python
        ui.panel_conditional("input.navigation === 'nav_model_control'",
            ui.navset_card_tab(sim_config_ui("sim_config"), run_control_ui("run_control"), id="model_control_tabs")),
```
Delete `panel_model_control` from `ui_panels.py` (and its `app.py` import). In `server()` add `run_control_server("run_control", state)` and delete the inline run_control handlers + the Task-2 `run.command_config` Calc. Add the `run_control` import (fallback pattern).

- [ ] **Step 6: Migrate integration selectors.** Grep + rewrite run_control ids → `run_control-*`:
```bash
grep -rn "cmd_input_file\|cmd_constants_file\|cmd_binary_enabled\|run_executable\|sim_output_dir\|load_output_config\|save_output_config\|output_boxes\|run_log_mini\|constants_validation" tests/python/*.py
```

- [ ] **Step 7: Verify — including the cross-tab bridge round-trip.**
```bash
.venv/bin/python -m pytest tests/python/test_run_control_module.py -v   # PASS
.venv/bin/python -c "import shiny_app.app; print('import OK')"
.venv/bin/python -c "
from shiny_app.app import create_ui
html=str(create_ui().tagify())
assert 'run_control-sim_output_dir' in html and 'run_control-run_executable' in html
assert 'id=\"sim_output_dir\"' not in html and 'id=\"run_executable\"' not in html
# both sub-tabs render:
assert 'Run Model' in html and 'Output Config' in html
# model_build's bridge + plot/sim_config bridges now target the namespaced id:
print('run_control namespaced; sim_output_dir/run_executable bridges repointed')
"
grep -n 'run_control-sim_output_dir' shiny_app/modules/plot.py shiny_app/modules/sim_config.py shiny_app/modules/model_build.py
.venv/bin/python -m pytest tests/python/ -q
ruff check shiny_app/modules/ tests/python/
```
Expected: module test PASS; namespaced ids present, zero bare leaks; the three bridge files reference `run_control-sim_output_dir`/`run_control-run_executable`; full suite green; ruff clean. **The integration-tests CI job (on push) is the authoritative proof that load-sim-config → sim_output_dir updates across the tab boundary and that build→run_executable population works** — flag in the commit that this cross-tab flow is CI-verified.

- [ ] **Step 8: Commit** — `refactor(shiny): run_control fat-tab Shiny module + repoint sim_output_dir/run_executable bridges`.

---

## Task 5: `dashboard` module

**Files:** Create `shiny_app/modules/dashboard.py`; create `tests/python/test_dashboard_module.py`; modify `shiny_app/app.py`, `shiny_app/ui_panels.py` (`panel_dashboard` → module UI content), and the dashboard integration selectors.

**Interfaces:**
- Consumes: `state`; reads `run.run_log_lines`/`run.running`/`run.last_run_time`/`run.progress`/`run.command_config()`/`run.active_executable()`, `state.output_config_version`/`sim_config_version`. Calls `run.stop()`, `state.navigate()`. **No sibling `input.X` reads** (all routed through `run.*` in Task 2).
- Produces: nothing shared (pure consumer).

- [ ] **Step 1: Write the failing render-smoke test.** `tests/python/test_dashboard_module.py` (template: `test_parameters_module.py`; `panel_dashboard` is one card, no nav → `str(dashboard_ui("dashboard"))`). Assert `nid("dashboard", raw)` for inputs `quick_run`, `dashboard_stop`, `goto_model_config`, `btn_copy_dashboard_log` and outputs `dashboard_status_text`, `dashboard_exe_text`, `dashboard_last_run_text`, `run_timer_display`, `system_status_compact`, `input_txt_variables`, `dashboard_run_log`. Assert `"input.navigation" not in html`.
```bash
.venv/bin/python -m pytest tests/python/test_dashboard_module.py -v   # FAIL
```

- [ ] **Step 2: Create `shiny_app/modules/dashboard.py`.** `@module.ui def dashboard_ui():` returns `panel_dashboard`'s content (strip its internal `panel_conditional`). `@module.server def dashboard_server(input, output, session, state): run = state.run` then verbatim-move: `copy_dashboard_log`, `navigate_to_model_config` (`goto_model_config` → `state.navigate("nav_model_control")`), `handle_quick_run` (already routed through `run.command_config()` in Task 2), `dashboard_run_log`, `run_timer_display`, `system_status_compact` (already `run.command_config()`), `dashboard_status_text`, `dashboard_exe_text` (already `run.active_executable()`), `dashboard_last_run_text`, `input_txt_variables` (reads `state.output_config_version`/`sim_config_version` — namespace-agnostic), `on_dashboard_stop`. Confirm zero `input.<sibling>` reads remain (grep the new file for `cmd_`, `build_`, `run_executable`, `active_executable` as `input.` reads → must be none; all via `run.*`).

- [ ] **Step 3: Wire into `app.py` + `ui_panels.py`.** In `create_ui()` (~L516) replace `panel_dashboard()` with `ui.panel_conditional("input.navigation === 'nav_dashboard'", dashboard_ui("dashboard"))`. In `server()` add `dashboard_server("dashboard", state)` and delete the inline dashboard handlers. Delete `panel_dashboard` from `ui_panels.py` (+ its `app.py` import). Add the `dashboard` import (fallback pattern).

- [ ] **Step 4: Migrate integration selectors.** Grep + rewrite dashboard ids → `dashboard-*`:
```bash
grep -rn "quick_run\|dashboard_stop\|goto_model_config\|dashboard_status_text\|dashboard_exe_text\|input_txt_variables\|system_status_compact\|dashboard_run_log" tests/python/*.py
```
**Note the nav landing:** `navigation` defaults to `nav_dashboard`; confirm the default view still renders (the `panel_conditional` on `nav_dashboard` is truthy at load).

- [ ] **Step 5: Verify.**
```bash
.venv/bin/python -m pytest tests/python/test_dashboard_module.py -v   # PASS
.venv/bin/python -c "import shiny_app.app; print('import OK')"
.venv/bin/python -c "
from shiny_app.app import create_ui
html=str(create_ui().tagify())
assert 'dashboard-quick_run' in html and 'id=\"quick_run\"' not in html
print('namespaced, no bare leak')"
grep -n "input\.\(cmd_\|build_\|run_executable\|active_executable\)" shiny_app/modules/dashboard.py || echo "clean — no sibling input reads in dashboard"
.venv/bin/python -m pytest tests/python/ -q
ruff check shiny_app/modules/dashboard.py tests/python/test_dashboard_module.py
```

- [ ] **Step 6: Commit** — `refactor(shiny): dashboard Shiny module`.

---

## Task 6: Phase-4 regression gate (controller-run)

- [ ] **Static + unit:** `py_compile` app + the 3 new modules; `.venv/bin/python` import app + `model_build`/`run_control`/`dashboard`; App object constructs; ruff `shiny_app/modules/ shiny_app/diagnostics.py tests/python/` clean; full suite (176 + 3 module render tests + Task-2 assertions ≈ **179+** — re-count at gate).
- [ ] **`server()` is now a thin assembler:** grep `server()` — it should contain only `state`/`run` construction + the `_navigate` closure + 15 `x_server("id", state)` calls + the two chrome `@render.ui`s (`help_content`, `changelog_content`). No `@reactive.event(input.build_*|input.run|input.quick_run|input.cmd_*|input.load_output_config|input.save_output_config)` handlers remain inline.
- [ ] **No cross-boundary `input.X`:** `grep -nE "input\.(cmd_|build_|run_executable|active_executable|quick_run|output_boxes|sim_output_dir)" shiny_app/app.py` → empty (all moved into modules).
- [ ] **Bridges intact:** `grep -rn "run_control-sim_output_dir\|run_control-run_executable" shiny_app/modules/{plot,sim_config,model_build}.py` → all three files reference the fully-qualified ids; `grep -rn 'update_select("sim_output_dir"\|update_select("run_executable"' shiny_app/modules/` → empty (no stale bare-id bridge).
- [ ] **`RunController` contract:** `run.command_config`/`run.build_config` are `reactive.Calc`; `run.active_executable` is `.set()` by model_build.
- [ ] **Boot smoke:** every one of the 15 tabs renders namespaced in `create_ui().tagify()`, zero bare-id leaks across the cluster (`build_*`, `cmd_*`, `run_executable`, `sim_output_dir`, `quick_run`, `dashboard_*`); default `nav_dashboard` view renders; ws session import constructs `App` with no traceback.
- [ ] **CI (on push):** `integration-tests` drives the namespaced `model_build-*`/`run_control-*`/`dashboard-*` selectors AND the two cross-tab flows (load-sim-config → `run_control-sim_output_dir`; build → `run_control-run_executable` population; quick-run from dashboard using `run.command_config()`) — the authoritative DOM/behavior proof.
- [ ] **(Deferred) release** — `v0.4.4` at finishing (Phase 5 = final cleanup + docs is the last release).

---

## Self-Review

**Spec coverage (§7 Phase 4 → tasks):** model_build → Task 3; run_control (fat-tab) → Task 4; dashboard → Task 5. The spec's "3 commits" expands to 6 tasks because (a) ~260 lines of dead code are dropped first (Task 1 — the spec predates this finding), and (b) the cross-module contract is lifted before namespacing (Task 2 — the Phase-0 de-risk applied to the most-coupled cluster). The spec's stale row-14 "writes selected_output_dir" is void (Phase 3 removed the bus); the only output-dir handling left inline is the Output Config `sim_output_dir`, now owned by run_control.

**Placeholder scan:** the two non-mechanical cases carry explicit code — the `reactive.Calc` conversions + cross-read rewiring (Task 2) and the three-file `sim_output_dir`/`run_executable` bridge repoint (Task 4). Verbatim handler moves are specified by name + line range (v0.4.3 baseline; re-grep at execution), matching the proven Phase 1-3 plan style. The fat-tab composition is de-risked with an executable check (Task 4 Step 1) before the real UI is written.

**Type/name consistency:** `run.command_config`/`run.build_config` are `reactive.Calc` (Task 2) and read as `run.command_config()`/`run.build_config()` everywhere (Tasks 4, 5). `run.active_executable` is a `reactive.Value` set by model_build (Tasks 2/3) and read as `run.active_executable()` (Task 5). The `sim_output_dir` bridge id is `"run_control-sim_output_dir"` consistently across plot.py, sim_config.py (Task 4), and the gate (Task 6). `run_executable` bridge id is `"run_control-run_executable"` in model_build.py (Task 4) and the gate.

**Ordering rationale:** model_build → run_control → dashboard keeps every commit shippable — model_build's sibling `run_executable` update targets the still-root id during Task 3 (correct then), and is repointed to the namespaced id atomically in Task 4 when run_control claims it; dashboard is last because Task 2 already routed all its cross-reads through `run.*`, so it converts as a pure consumer with no sibling-input landmines.

**Key risks flagged for review-in-loop (RE-VERIFY at execution):** (1) the `sim_output_dir` bridge break + repoint (top risk — Task 4 Step 4 + boot-smoke round-trip); (2) the fat-tab `TagList`-of-two-`nav_panel`s composition (Task 4 Step 1 de-risk); (3) the dead-code deletions (Task 1 Step 1 re-verify-zero-refs before dropping); (4) the bucket/id mismatches — `copy_mini_log`/`run_log_mini`/`run_status_indicator`/`goto_build` register in `run_control` despite thematic names (their ids live in the Run Model tab).
