# Shiny-modules Rearchitecture — Phase 4 (run/build/dashboard cluster) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the last three inline tabs of `shiny_app/app.py`'s `server()` — `model_build`, `run_control`, `dashboard` — into true namespaced `@module.ui`/`@module.server` modules, leaving `server()` a thin assembler (state construction + module calls + the two app-level chrome renders).

**Architecture:** This is the **most coupled** cluster: dashboard reads run/build state that model_build and run_control own. Rather than move-and-namespace in one shot (which would break cross-module `input.X` reads the moment ids namespace), the phase is **contract-first** (the Phase-0 playbook): first make every cross-module value flow through the already-existing `RunController` (`run.command_config`/`run.build_config`/`run.active_executable`) and `AppState`, with **zero id namespacing** and a DOM-identical result — proving the wiring — and only then namespace each module atomically. A dead-code sweep precedes the conversions so ~260 lines of confirmed-dead handlers are dropped, not migrated.

**Tech Stack:** Python 3.10, Shiny for Python 1.5.1 (`@module.ui`/`@module.server`, `session.root_scope()` cross-namespace bridge), pytest, ruff. Modules live in `shiny_app/modules/`.

## Global Constraints

*(Every task's requirements implicitly include this section. Values copied verbatim from the codebase state at plan time — RE-GREP current line numbers at execution: they shift as each task removes code. Names/ids are stable; line numbers are the v0.4.3 baseline.)*

- **Behavior/DOM-identical except the within-tab id namespace.** Only ids inside a converted tab gain the `<module>-` prefix; nav ids (`nav_dashboard`, `nav_model_build`, `nav_model_control`) and the global `navigation` input stay global.
- **Module recipe** (12 modules already follow it — templates: `shiny_app/modules/sim_config.py` for a fat-tab/nav UI, `shiny_app/modules/plot.py` for a large module, `shiny_app/modules/parameters.py` for the base shape): `x_ui(...)` returns panel **content** only — the `panel_conditional("input.navigation === 'nav_X'", …)` moves to `create_ui()`; `x_server(input, output, session, state)`; self-contained (stdlib + existing leaf modules only, nothing from `app.py`); `logger = logging.getLogger("AQUABC")`; self-compute `ROOT` for the `modules/` depth (`os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))`); `try/except ImportError` import fallback (`from shiny_app.modules.X import …` / `from modules.X import …`).
- **No `input.X` crosses a module boundary.** Cross-module values flow via the shared contract only: `run.command_config` (run_control→dashboard; **returns a `List[str]` argv, e.g. `["./ESTAS_II", "INPUT.txt", …]` — NOT a bare name**), `run.run_executable_name` (run_control→dashboard; the bare Run-Model executable name string), `run.constants_config` (run_control→dashboard; `(const_file, binary_enabled, shear_file)` for quick-run pre-flight validation), `run.build_config` (model_build→run_control), `run.active_executable` (model_build→dashboard; the *Model Build* tab's selector — a DIFFERENT widget from Run Model's `run_executable`), `state.output_config_version` / `state.sim_config_version` counters, `state.navigate()`. The one exception is the sibling **`sim_output_dir`** SELECT widget (a bidirectionally-written input, not a one-way value) reached via `session.root_scope().make_scope("run_control")` — see the sim_output_dir constraint below.
- **`sim_output_dir` bridge (the top Phase-4 risk — empirically re-verified).** `sim_output_dir` (widget defined `ui_panels.py:350`, in `panel_model_control`'s Output Config sub-tab) is written by three modules: `run_control` (owner: `refresh_sim_output_dirs`, `sim_output_dir_info`), `plot` (`init_output_dirs` updates its choices, `plot.py:463`), and `sim_config` (`load_simulation_config_file` sets it `sim_config.py:210`; the save handler **reads** it `sim_config.py:351`). Today plot/sim_config reach it via `session.root_scope()` + the literal id `"sim_output_dir"` because it lives at root. Once `run_control` owns it (Task 4), it becomes `run_control-sim_output_dir` and is **no longer at root**. **Do NOT use the literal fully-qualified id** — `root.input["run_control-sim_output_dir"]()` raises `ValueError: … not a valid id; only letters, numbers, and underscore are permitted` (verified: `Inputs.__getitem__` runs `validate_id` before namespace resolution, so a hyphenated id crashes even from root scope — a runtime error, not a silent `None`). **Resolution (chosen, verified against Shiny 1.5.1): use `make_scope`, symmetric for read and write.** `session.root_scope().make_scope("run_control")` returns a session/scope whose namespace prefixes a *bare* id (each segment passes `validate_id` individually):
  - **read:** `session.root_scope().make_scope("run_control").input.sim_output_dir()` (replaces `root.input.sim_output_dir()` / the broken literal-id subscript).
  - **write:** `ui.update_select("sim_output_dir", …, session=session.root_scope().make_scope("run_control"))` (replaces `ui.update_select("sim_output_dir", …, session=root)`).

  The `run_executable` bridge (model_build→run_control, Task 4) uses the same `make_scope` write form. Task 4 makes these changes atomically with the namespacing; because a static `create_ui().tagify()` never invokes reactive handlers, the read-side crash surfaces **only** under a click-through — the boot smoke must actually exercise (or the integration-tests CI must cover) load-sim-config → save-config. *(Alternative considered and rejected: promoting the value to a shared `reactive.Value` on `AppState` — rejected because the widget is bidirectionally written by three modules, so a one-writer bus does not fit and you would still need `ui.update_select` on the real widget.)*
- **Conversion commits move handlers verbatim** (no logic edits) except the explicitly-specified cross-module rewirings (input read → `run.*`) and id namespacing. Dead code is dropped in Task 1, not carried into a module.
- **`shiny_app/modules/` + `shiny_app/diagnostics.py` are fully ruff-gated; `tests/python/` is ruff-gated.** `app.py` has an F401/F841 per-file-ignore (mid-decomposition). Keep new module files lint-clean.
- **`tests/python/` is a package** (has `__init__.py`) — no bare `from nsutil import`; inline the `nid(module_id, input_id)` helper in each test (`return f"{module_id}-{input_id}"`).
- **Use `.venv/bin/python`** for anything importing `app.py` — the system `python3` lacks `networkx` (masks a false "import fails"; `.venv` is the real env, `pytest` runs there).
- **Per-task gate:** `py_compile` + import; full suite green; that tab's selectors migrated to namespaced ids (same commit); ruff clean; boot smoke (`create_ui().tagify()` shows the tab's namespaced ids, zero bare-id leaks; `.venv` import constructs the `App`). Commit per task.

---

## File Structure

**New module files (`shiny_app/modules/`):**
- `model_build.py` — `model_build_ui(compilers, build_types)` + `model_build_server(input, output, session, state)`. Owns the Model Build tab; **registers** `run.build_config` (as `reactive.Calc`); sets `run.active_executable`; updates its own `active_executable` + the sibling Run-Model `run_executable` select (via `root_scope()` + bare id while run_control is still inline in Task 3, repointed to `root_scope().make_scope("run_control")` in Task 4).
- `run_control.py` — `run_control_ui()` (returns a **plain `list` of two `nav_panel`s** — **Run Model** + **Output Config** — that `create_ui` composes into `panel_model_control`'s navset by **`*`-unpacking**; a `@module.ui` returning a `ui.TagList`/nav content as a *single* positional arg to `navset_card_tab` raises `AttributeError: 'TagList' object has no attribute 'resolve'`, verified) + `run_control_server(input, output, session, state)`. Owns `cmd_*`, `run_executable`, `run`, `stop_run`, `goto_build`, `copy_mini_log`/`run_log_mini`/`run_status_indicator` (ids physically in Run Model tab), and the whole Output Config sub-tab (`output_boxes`/`output_types`/`load_output_config`/`save_output_config`/`sim_output_dir`/`refresh_sim_output_dirs`/`sim_output_dir_info`). **Registers** `run.command_config` + `run.run_executable_name` + `run.constants_config`; reads `run.build_config`; writes `state.output_config_version`.
- `dashboard.py` — `dashboard_ui()` + `dashboard_server(input, output, session, state)`. Owns the Dashboard tab; a pure consumer of `run.*` (logs, `is_running`, `last_run_time`, `run.command_config()` [list], `run.run_executable_name()` [bare name], `run.constants_config()` [validation triple], `run.active_executable()`) + `state.output_config_version`/`sim_config_version`; calls `run.stop()`, `state.navigate()`.

**New test files (`tests/python/`):** `test_model_build_module.py`, `test_run_control_module.py` (`.tagify()` — fat-tab nav UI), `test_dashboard_module.py`. Update `test_run_controller.py` if `RunController` gains typed `command_config`/`build_config` fields.

**Modified:** `shiny_app/app.py` (shrinks — server() → assembler; add a `sim_config_ui` import — currently only `sim_config_server` is imported, but Task 4 calls `sim_config_ui("sim_config")` directly in `create_ui`); `shiny_app/ui_panels.py` (`panel_dashboard`/`panel_model_build`/`panel_model_control` → module UI content; drop the internal `panel_conditional`s / the whole `panel_model_control` navset moves to `create_ui`); **`tests/python/test_ui_panels.py`** (it calls `panel_dashboard()`/`panel_model_control()`/`panel_model_build(...)` **by name** — each panel deletion breaks it, so its `ARGFREE` entries + `test_panel_model_build_takes_consts_and_renders` are removed per-task, and the panels' content-marker coverage migrates into the new module render tests); `shiny_app/app_state.py` (only if `RunController` gains typed `command_config`/`build_config`/`run_executable_name`/`constants_config` fields — likely no change, they are assigned in `server()` like the existing `active_executable`); `shiny_app/modules/plot.py` + `shiny_app/modules/sim_config.py` + `shiny_app/modules/model_build.py` (repoint the `sim_output_dir`/`run_executable` bridges via `make_scope`, Task 4); the integration-test selectors for these three tabs.

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
ruff check tests/python/       # CI scope — shiny_app/app.py has ~83 pre-existing WIP-exempt errors; do NOT run `ruff check shiny_app/` here (it is deliberately un-gated, only tests/ is linted in CI)
```
Expected: 176 passed (no test targeted the dead code); `ruff check tests/python/` clean. (Do not attempt to fix app.py's pre-existing lint debt — out of scope.)

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
- `run.command_config` — a `reactive.Calc` returning the assembled ESTAS command as a **`List[str]` argv** (the current `build_estas_command()` value, e.g. `["./ESTAS_II", "INPUT.txt", …]`). Read by dashboard's *command-preview* paths only (NOT for a bare executable name).
- `run.run_executable_name` — a `reactive.Value(str)` mirroring the Run-Model-tab `input.run_executable()` (the bare selected exe name, default `"ESTAS_II"`). Read by dashboard's `system_status_compact` / `handle_quick_run` where a **name string** is needed (`os.path.join(ROOT, name)`, `is_intel_executable(name)`, display). This is the bridge the plan originally missed by conflating it with `command_config`.
- `run.constants_config` — a `reactive.Calc` returning `(const_file, binary_enabled, shear_file)` from the Run-Model `cmd_*` inputs. Read by `handle_quick_run`'s pre-flight `validate_constants_file` branch (which is **not** command assembly).
- `run.build_config` — a `reactive.Calc` returning the current build config dict (the current `_current_build_config()` value). Read by run_control.
- `run.active_executable` — the existing `reactive.Value`, now `.set()` by a model_build effect mirroring the **Model-Build-tab** `input.active_executable()` (a DIFFERENT widget from Run Model's `run_executable`). Read by dashboard's `dashboard_exe_text`.

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

- [ ] **Step 3: Activate the `run.active_executable` bridge + register the two new run_control bridges.** Add three registrations (in `server()` now; they move into their owning module later — `active_executable` publisher → model_build in Task 3; `run_executable_name` publisher + `constants_config` calc → run_control in Task 4):
```python
    # model_build → dashboard: the Model Build tab's active_executable selector
    @reactive.effect
    def _publish_active_executable():
        run.active_executable.set(input.active_executable())

    # run_control → dashboard: the Run Model tab's run_executable name (bare string)
    run.run_executable_name = reactive.Value("ESTAS_II")
    @reactive.effect
    def _publish_run_executable_name():
        run.run_executable_name.set(input.run_executable() or "ESTAS_II")

    # run_control → dashboard: the quick-run constants-validation inputs
    @reactive.calc
    def _constants_config():
        return (input.cmd_constants_file(), input.cmd_binary_enabled(), input.cmd_shear_stress_file())
    run.constants_config = _constants_config
```

- [ ] **Step 4: Route dashboard's cross-bucket reads through `run.*` — distinguishing the FOUR different values.** `run.command_config()` is a **list**, not a name; `run_executable` (Run Model) and `active_executable` (Model Build) are **different** widgets. Rewire only the cross-reads (leave all other logic — subprocess launch, validation flow, display formatting — verbatim):
  - `dashboard_exe_text` (~L2182-2187): `input.active_executable()` → `run.active_executable()`. *(Model-Build selector — correct as originally planned.)*
  - `system_status_compact` (~L2113-2176): TWO separate reads —
    - the **exe-name** read `exe_name = input.run_executable() or "ESTAS_II"` (~L2154, feeds `os.path.join(ROOT, exe_name)` + the "Exe:" display) → `exe_name = run.run_executable_name()`.
    - the **command** read `cmd = build_estas_command()` (~L2166, feeds `" ".join(cmd)` for the "Cmd:" display) → `cmd = run.command_config()`.
  - `handle_quick_run` (~L1172-1411): THREE distinct rewires —
    - the command-assembly call `estas_cmd = build_estas_command()` (~L1201) → `estas_cmd = run.command_config()`.
    - the **exe-name** read `exe_name = input.run_executable() or "ESTAS_II"` (~L1205, feeds `os.path.join`/`is_intel_executable(exe_name)` at ~L1209/1216/1309) → `exe_name = run.run_executable_name()`. **Do NOT route this through `command_config`** (a list → `TypeError` on `os.path.join`, `AttributeError` on `.lower()`).
    - the pre-flight validation reads `input.cmd_constants_file()`/`input.cmd_binary_enabled()`/`input.cmd_shear_stress_file()` (~L1234/1239/1244) → `const_file, binary_enabled, shear_file = run.constants_config()`. Leave the subsequent `if not const_file and (binary_enabled or shear_file): const_file = DEFAULT_CONSTANTS_FILE` fallback + `validate_constants_file(const_file)` verbatim (keep a module-local `DEFAULT_CONSTANTS_FILE = "WCONST_01.txt"`).
    - Leave the subprocess-launch body (writes `run.process`/`run.running`/`run.last_run_time`/`run.progress`/`run.run_log_lines`) verbatim. *(Deduping this ~140-line block against `RunController.start_run` is out of scope.)*
  - **These render/handler bodies are NOT exercised by any pytest** (only Playwright/manual QA runs them live), so the boot smoke in Step 6 cannot catch a type error here — read each edit carefully; the failure is a runtime `TypeError`/`AttributeError`, not a test failure.

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
ruff check tests/python/       # CI scope only — app.py lint is deliberately WIP-exempt
```
Expected: bare ids still present (nothing namespaced), full suite green (176 + Step-5 assertions), `ruff check tests/python/` clean.

- [ ] **Step 7: Commit.**
```bash
git add shiny_app/app.py tests/python/test_run_controller.py
git commit -m "refactor(shiny): route run/build cross-module reads through RunController (contract-first)

command_config/build_config/constants_config become reactive.Calc;
active_executable + run_executable_name published from their inputs; dashboard
reads run.command_config() (list) / run.run_executable_name() (name) /
run.constants_config() / run.active_executable() instead of sibling inputs.
Zero id namespacing — DOM byte-identical."
```

---

## Task 3: `model_build` module

**Files:** Create `shiny_app/modules/model_build.py`; create `tests/python/test_model_build_module.py`; modify `shiny_app/app.py` (remove the inline block; add `model_build_server` call), `shiny_app/ui_panels.py` (`panel_model_build` → module UI content), and the model_build integration selectors.

**Interfaces:**
- Consumes: `state` (AppState). Reads its own `build_compiler`/`build_type`/`build_clean_first`/`active_executable`/`btn_*` inputs.
- Produces: registers `run.build_config` (`reactive.Calc`, moved here from server); `.set()`s `run.active_executable`; keeps the `run.execute_build()` calls. Updates its own `active_executable` select and the **sibling `run_executable`** select (run_control not yet converted → still at root; bridge via `root_scope()` + literal `"run_executable"`, repointed in Task 4).

- [ ] **Step 1: Write the failing render-smoke test.** `tests/python/test_model_build_module.py` (template: `tests/python/test_parameters_module.py`). `panel_model_build` is a plain `layout_columns` (no nav) → render via `str(model_build_ui("model_build", COMPILERS, BUILD_TYPES))` (import `COMPILERS`/`BUILD_TYPES` from `shiny_app.app` or pass literal lists like `test_panel_model_build_takes_consts_and_renders` does: `{"gfortran": {"name": "GNU Fortran"}}` / `{"release": {"name": "Release"}}`). Assert namespaced ids present via `nid("model_build", raw)` for: `build_compiler`, `build_type`, `build_clean_first`, `btn_build`, `btn_rebuild`, `btn_refresh_executables`, `active_executable`, `btn_clear_build_log`, and outputs `compiler_status`, `build_flags_info`, `target_exe_name`, `executable_list`, `executable_info`, `build_log`. **Also assert the content markers migrated from `test_ui_panels.py::test_panel_model_build_takes_consts_and_renders`** (which Step 5 removes): `"Build Configuration"`, `"Available Executables"`, `"Build Log"`, `"GNU Fortran"`, `"Release"` all in `html` (guards a verbatim move that drops/reorders a sub-card). Assert `"input.navigation" not in html` (nav moved to create_ui; do NOT assert `"nav_model_build"`).
```bash
.venv/bin/python -m pytest tests/python/test_model_build_module.py -v
```
Expected: FAIL (module doesn't exist).

- [ ] **Step 2: Create `shiny_app/modules/model_build.py`.** Follow the recipe. `@module.ui def model_build_ui(compilers, build_types):` returns the **content** of `panel_model_build` (strip its internal `panel_conditional`; the compilers/build_types come in as args, matching the current `panel_model_build(COMPILERS, BUILD_TYPES)` signature). `@module.server def model_build_server(input, output, session, state): run = state.run` then the verbatim handlers moved from `app.py`: `compiler_status`, `build_flags_info`, `get_target_exe_name`, `target_exe_name`, `executable_list`, `executable_info`, `build_log`, `clear_build_log`, `refresh_executables`, `init_executable_list` (+ its `_exe_list_initialized` guard), `on_build`, `on_rebuild`; the `run.build_config` `reactive.Calc` registration (from Task 2); the `_publish_active_executable` effect (from Task 2); and the shared helpers `get_available_executables`/`get_executable_info` (import from `shiny_app.build_commands` — they are thin wrappers `build_commands.get_available_executables(ROOT)` etc.; do not duplicate). For the **sibling `run_executable`** updates in `refresh_executables` and `init_executable_list`, bridge to root (run_control still inline):
```python
    root = session.root_scope()
    ui.update_select("run_executable", choices=..., selected=..., session=root)   # bare id + root (run_control still inline); repointed to session=root.make_scope("run_control") in Task 4
```
Its own `active_executable` updates stay plain (`ui.update_select("active_executable", …)`).

- [ ] **Step 3: Wire into `app.py` + `ui_panels.py`.** In `create_ui()` (~L518) replace `panel_model_build(COMPILERS, BUILD_TYPES)` with `ui.panel_conditional("input.navigation === 'nav_model_build'", model_build_ui("model_build", COMPILERS, BUILD_TYPES))`. In `server()` add `model_build_server("model_build", state)` (near the other module calls) and delete the inline model_build handlers + the Task-2 `run.build_config` Calc + `_publish_active_executable` (they moved into the module). Delete `panel_model_build` from `ui_panels.py` + drop its import in `app.py`. Add the `model_build` import (fallback pattern).

- [ ] **Step 3b: Remove the now-broken `test_ui_panels.py` case (same commit).** Deleting `panel_model_build` breaks `tests/python/test_ui_panels.py::test_panel_model_build_takes_consts_and_renders` (`AttributeError: module 'shiny_app.ui_panels' has no attribute 'panel_model_build'`). Delete that test function (its content-marker coverage moved into Step 1's module test). Leave the `ARGFREE` dict + `test_argfree_panels_render_with_all_markers` untouched here (Tasks 4/5 handle `panel_model_control`/`panel_dashboard`).

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

- [ ] **Step 1: Confirm the fat-tab composition form (already verified — do NOT use the single-arg form).** Empirically established against Shiny 1.5.1: passing a `@module.ui`'s return **as one positional arg** to `navset_card_tab` raises `AttributeError: 'TagList' object has no attribute 'resolve'` at `.tagify()`. **The working form is: `run_control_ui` returns a plain `list` of the two `nav_panel`s, `*`-unpacked at the call site.** Re-confirm before building:
```bash
.venv/bin/python -c "
from shiny import ui, module
@module.ui
def rc():
    return [ui.nav_panel('Run Model', ui.input_text('run_executable','x')),
            ui.nav_panel('Output Config', ui.input_text('sim_output_dir','y'))]
html=str(ui.navset_card_tab(ui.nav_panel('Sim', ui.input_text('a','a')), *rc('run_control'), id='model_control_tabs').tagify())
assert 'run_control-run_executable' in html and 'run_control-sim_output_dir' in html and 'Run Model' in html and 'Output Config' in html
print('fat-tab list + splat composes + namespaces OK')
"
```
Expected: prints OK. Use this exact form (list return + `*`-unpack) in Steps 2, 3, 5 — the single-arg form is NOT an option.

- [ ] **Step 2: Write the failing render-smoke test.** `tests/python/test_run_control_module.py` (template: `tests/python/test_sim_config_module.py` — **`.tagify()`** since the UI is nav content). Compose with the Step-1 working form (`*`-unpack): `html = str(ui.navset_card_tab(*run_control_ui("run_control")).tagify())`. Assert `nid("run_control", raw)` present for Run Model ids (`goto_build`, `run_executable`, `cmd_input_file`, `cmd_constants_file`, `cmd_binary_enabled`, `cmd_binary_filename`, `run`, `stop_run`, `btn_copy_mini_log`; outputs `run_executable_info`, `cmd_preview`, `constants_validation_status`, `run_status_indicator`, `run_log_mini`) and Output Config ids (`output_boxes`, `sim_output_dir`, `refresh_sim_output_dirs`, `output_types`, `load_output_config`, `save_output_config`; outputs `sim_output_dir_info`, `output_config_status`). Assert both sub-tab titles ("Run Model", "Output Config") and `"input.navigation" not in html`.
```bash
.venv/bin/python -m pytest tests/python/test_run_control_module.py -v   # FAIL
```

- [ ] **Step 3: Create `shiny_app/modules/run_control.py`.** `@module.ui def run_control_ui():` returns a **plain `list`** `[run_model_nav_panel, output_config_nav_panel]` (Run Model nav_panel `ui_panels.py:220-328` + Output Config nav_panel `ui_panels.py:330-393`, content moved verbatim; NOT a `ui.TagList` passed as one arg). `@module.server def run_control_server(input, output, session, state): run = state.run` then verbatim-move from `app.py`: `init_cmd_dropdowns`, `build_estas_command`, `cmd_preview`, `run_executable_info` (the `get_executable_info` wrapper — import from `build_commands`), `constants_validation_status`, `navigate_to_build` (the `goto_build` handler — `state.navigate("nav_model_build")`), `on_run` (calls `run.start_run` via thread), `on_stop_run`, `copy_mini_log`, `run_log_mini`, `run_status_indicator`, and the Output Config cluster (`output_config_msg` reactive.Value, `OUTPUT_INFO_FILE`, `load_output_config`, `save_output_config` [writes `state.output_config_version`], `output_config_status`, `refresh_sim_output_dirs`, `sim_output_dir_info`). **Move the three run_control-owned contract registrations from server (Task 2) INTO this module** (they now read this module's namespaced inputs): the `run.command_config` `reactive.Calc` (wrapping `build_estas_command`), the `_publish_run_executable_name` effect (`run.run_executable_name.set(input.run_executable() or "ESTAS_II")`), and the `_constants_config` `reactive.Calc` (`run.constants_config`). Keep `DEFAULT_CONSTANTS_FILE = "WCONST_01.txt"` as a module-level const (used by `run_control`'s own logic; dashboard keeps its own copy for the quick-run fallback).

- [ ] **Step 4: Repoint the `sim_output_dir` + `run_executable` cross-namespace bridges via `make_scope` (verified — do NOT use a hyphenated literal id).** Now that `run_control` owns these ids, the existing `session.root_scope()` + bare-`"sim_output_dir"` bridge misses (id is no longer at root). **Use `session.root_scope().make_scope("run_control")`** (a module-scoped session that namespaces a *bare* id — verified working; the literal `"run_control-sim_output_dir"` form crashes `validate_id` on the READ side, so it is banned). Define once per handler, e.g. `rc = session.root_scope().make_scope("run_control")`:
  - In `shiny_app/modules/model_build.py` (from Task 3): the sibling `run_executable` update → `ui.update_select("run_executable", choices=…, selected=…, session=session.root_scope().make_scope("run_control"))` (bare id, module-scoped session — replaces the Task-3 root+bare form).
  - In `shiny_app/modules/plot.py` (~L463, inside `init_output_dirs`): `ui.update_select("sim_output_dir", …, session=root)` → `ui.update_select("sim_output_dir", …, session=session.root_scope().make_scope("run_control"))`.
  - In `shiny_app/modules/sim_config.py`: the **write** (~L210) → `ui.update_select("sim_output_dir", …, session=session.root_scope().make_scope("run_control"))`; the **read** (~L351) `root.input.sim_output_dir()` → `session.root_scope().make_scope("run_control").input.sim_output_dir()`. **The read is the one the review flagged as `ValueError` under the literal-id form — this `make_scope` read is the verified fix.**
  - Update the bridge docstrings in plot.py (~L11-15) + sim_config.py to describe the `make_scope("run_control")` bridge.

- [ ] **Step 5: Wire into `app.py` + `ui_panels.py`.** In `create_ui()` (~L519) replace `panel_model_control()` with the composed navset (moved out of `ui_panels.py`), using the **`*`-unpack** form from Step 1:
```python
        ui.panel_conditional("input.navigation === 'nav_model_control'",
            ui.navset_card_tab(sim_config_ui("sim_config"), *run_control_ui("run_control"), id="model_control_tabs")),
```
**Add a `sim_config_ui` import to `app.py`** (both the `from shiny_app.modules.sim_config import …` and the `from modules.sim_config import …` fallback branches) — today app.py imports only `sim_config_server`; `sim_config_ui` was previously used only inside `ui_panels.py` (now deleted), so `create_ui()` would hit `NameError` at import without it. Delete `panel_model_control` from `ui_panels.py` (and its `app.py` import). In `server()` add `run_control_server("run_control", state)` and delete the inline run_control handlers + the Task-2 `run.command_config`/`run_executable_name`/`constants_config` registrations (they moved into the module). Add the `run_control` import (fallback pattern).

- [ ] **Step 5b: Remove the now-broken `test_ui_panels.py` case (same commit).** Deleting `panel_model_control` breaks `test_argfree_panels_render_with_all_markers` (it calls `ui_panels.panel_model_control()`). Delete the `"panel_model_control"` entry from the `ARGFREE` dict in `tests/python/test_ui_panels.py` (its `"Time Period"`/`"Time Stepping"`/`"Output Interval"` markers are sim_config content already covered by `test_sim_config_module.py`). Leave the `"panel_dashboard"` entry for Task 5.

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
assert 'run_control-sim_output_dir' in html and 'run_control-run_executable' in html   # the widgets are namespaced in the rendered DOM
assert 'id=\"sim_output_dir\"' not in html and 'id=\"run_executable\"' not in html      # no bare (root) copies remain
# both sub-tabs render:
assert 'Run Model' in html and 'Output Config' in html
print('run_control namespaced; sim_output_dir/run_executable bridges repointed')
"
# the bridges use make_scope (bare id + module-scoped session), NOT a hyphenated literal id:
grep -n 'make_scope(\"run_control\")' shiny_app/modules/plot.py shiny_app/modules/sim_config.py shiny_app/modules/model_build.py
grep -rn 'input\[\"run_control-\|update_select(\"run_control-' shiny_app/modules/ && echo "!!! banned literal-namespaced-id form found" || echo "clean — no literal hyphenated-id bridge"
.venv/bin/python -m pytest tests/python/ -q
ruff check shiny_app/modules/ tests/python/
```
Expected: module test PASS; namespaced widgets present in DOM, zero bare (root) leaks; the three bridge files use `make_scope("run_control")` (and NONE use the banned `input["run_control-…"]` / `update_select("run_control-…")` literal form); full suite green; ruff clean. **The read-side `make_scope` bridge (sim_config save handler) is a reactive path NOT exercised by `create_ui().tagify()` or pytest — the `integration-tests` CI job (on push) is the authoritative proof that load-sim-config → save-config round-trips without the `ValueError`, and that build → `run_executable` population works across the tab boundary.** Flag in the commit that these cross-tab flows are CI-verified only.

- [ ] **Step 8: Commit** — `refactor(shiny): run_control fat-tab Shiny module + repoint sim_output_dir/run_executable bridges`.

---

## Task 5: `dashboard` module

**Files:** Create `shiny_app/modules/dashboard.py`; create `tests/python/test_dashboard_module.py`; modify `shiny_app/app.py`, `shiny_app/ui_panels.py` (`panel_dashboard` → module UI content), `tests/python/test_ui_panels.py` (drop the last `ARGFREE` entry — the file becomes vacuous, so delete it), and the dashboard integration selectors.

**Interfaces:**
- Consumes: `state`; reads `run.run_log_lines`/`run.running`/`run.last_run_time`/`run.progress`/`run.command_config()` [list] /`run.run_executable_name()` [name] /`run.constants_config()` [validation triple] /`run.active_executable()`, `state.output_config_version`/`sim_config_version`. Calls `run.stop()`, `state.navigate()`. **No sibling `input.X` reads** (all routed through `run.*` in Task 2). Keeps a module-local `DEFAULT_CONSTANTS_FILE = "WCONST_01.txt"` for the quick-run fallback.
- Produces: nothing shared (pure consumer).

- [ ] **Step 1: Write the failing render-smoke test.** `tests/python/test_dashboard_module.py` (template: `test_parameters_module.py`; `panel_dashboard` is one card, no nav → `str(dashboard_ui("dashboard"))`). Assert `nid("dashboard", raw)` for inputs `quick_run`, `dashboard_stop`, `goto_model_config`, `btn_copy_dashboard_log` and outputs `dashboard_status_text`, `dashboard_exe_text`, `dashboard_last_run_text`, `run_timer_display`, `system_status_compact`, `input_txt_variables`, `dashboard_run_log`. **Also assert the content markers migrated from `test_ui_panels.py`'s `panel_dashboard` ARGFREE entry** (which Step 3b removes): `"Dashboard"`, `"System Status"`, `"Simulation Config"` in `html`. Assert `"input.navigation" not in html`.
```bash
.venv/bin/python -m pytest tests/python/test_dashboard_module.py -v   # FAIL
```

- [ ] **Step 2: Create `shiny_app/modules/dashboard.py`.** `@module.ui def dashboard_ui():` returns `panel_dashboard`'s content (strip its internal `panel_conditional`). `@module.server def dashboard_server(input, output, session, state): run = state.run` then verbatim-move: `copy_dashboard_log`, `navigate_to_model_config` (`goto_model_config` → `state.navigate("nav_model_control")`), `handle_quick_run` (already routed in Task 2: command → `run.command_config()`, exe name → `run.run_executable_name()`, validation triple → `run.constants_config()`; keep the module-local `DEFAULT_CONSTANTS_FILE` fallback + `validate_constants_file`), `dashboard_run_log`, `run_timer_display`, `system_status_compact` (already Task 2: exe → `run.run_executable_name()`, cmd → `run.command_config()`), `dashboard_status_text`, `dashboard_exe_text` (already `run.active_executable()`), `dashboard_last_run_text`, `input_txt_variables` (reads `state.output_config_version`/`sim_config_version` — namespace-agnostic), `on_dashboard_stop`. **Confirm zero sibling `input.X` reads remain** — grep the new file: `grep -nE "input\.(cmd_|build_|run_executable|active_executable|output_boxes|output_types|sim_output_dir)" shiny_app/modules/dashboard.py` must be EMPTY (all sibling values come via `run.*`).

- [ ] **Step 3: Wire into `app.py` + `ui_panels.py`.** In `create_ui()` (~L516) replace `panel_dashboard()` with `ui.panel_conditional("input.navigation === 'nav_dashboard'", dashboard_ui("dashboard"))`. In `server()` add `dashboard_server("dashboard", state)` and delete the inline dashboard handlers. Delete `panel_dashboard` from `ui_panels.py` (+ its `app.py` import). Add the `dashboard` import (fallback pattern).

- [ ] **Step 3b: Remove the now-vacuous `test_ui_panels.py` (same commit).** Deleting `panel_dashboard` breaks the last `ARGFREE` entry. With `panel_model_build` (Task 3) and `panel_model_control` (Task 4) already dropped, `ARGFREE` is now empty and `test_argfree_panels_render_with_all_markers` would iterate nothing (vacuous). **Delete `tests/python/test_ui_panels.py` entirely** (all three panels are gone; their content-marker coverage now lives in the module render tests). Confirm no other test imports `shiny_app.ui_panels` panel functions: `grep -rn "ui_panels\.\(panel_dashboard\|panel_model_control\|panel_model_build\)" tests/`.

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
- [ ] **No cross-boundary `input.X`:** `grep -nE "input\.(cmd_|build_|run_executable|active_executable|quick_run|output_boxes|output_types|sim_output_dir)" shiny_app/app.py shiny_app/modules/dashboard.py` → empty (scan app.py AND the migrated dashboard module — a leftover/mis-typed cross-read inside dashboard.py is exactly the class of bug the boot-smoke can't catch).
- [ ] **Bridges via `make_scope` (not literal ids):** `grep -rn 'make_scope("run_control")' shiny_app/modules/{plot,sim_config,model_build}.py` → all three present; `grep -rn 'input\["run_control-\|update_select("run_control-\|update_select("sim_output_dir", *[^s]\|update_select("run_executable", *[^s]' shiny_app/modules/` → empty (no banned hyphenated-literal-id bridge and no stale root-scope bare-id bridge).
- [ ] **`RunController` contract:** `run.command_config`/`run.build_config`/`run.constants_config` are `reactive.Calc`; `run.active_executable`/`run.run_executable_name` are `reactive.Value` set by an effect (model_build / run_control respectively).
- [ ] **Boot smoke:** every one of the 15 tabs renders namespaced in `create_ui().tagify()`, zero bare-id leaks across the cluster (`build_*`, `cmd_*`, `run_executable`, `sim_output_dir`, `quick_run`, `dashboard_*`); default `nav_dashboard` view renders; ws session import constructs `App` with no traceback.
- [ ] **CI (on push):** `integration-tests` drives the namespaced `model_build-*`/`run_control-*`/`dashboard-*` selectors AND the cross-tab flows that pytest cannot cover — load-sim-config → **save-config** (exercises the `make_scope` read that would `ValueError` under a literal id); build → `run_executable` population; and quick-run from dashboard (exercises `run.command_config()` + `run.run_executable_name()` + `run.constants_config()`) — the authoritative DOM/behavior proof.
- [ ] **(Deferred) release** — `v0.4.4` at finishing (Phase 5 = final cleanup + docs is the last release).

---

## Self-Review

**Spec coverage (§7 Phase 4 → tasks):** model_build → Task 3; run_control (fat-tab) → Task 4; dashboard → Task 5. The spec's "3 commits" expands to 6 tasks because (a) ~260 lines of dead code are dropped first (Task 1 — the spec predates this finding), and (b) the cross-module contract is lifted before namespacing (Task 2 — the Phase-0 de-risk applied to the most-coupled cluster). The spec's stale row-14 "writes selected_output_dir" is void (Phase 3 removed the bus); the only output-dir handling left inline is the Output Config `sim_output_dir`, now owned by run_control.

**Placeholder scan:** the two non-mechanical cases carry explicit code — the `reactive.Calc`/`reactive.Value` bridge registrations + cross-read rewiring (Task 2) and the three-file `make_scope` bridge repoint (Task 4). Verbatim handler moves are specified by name + line range (v0.4.3 baseline; re-grep at execution), matching the proven Phase 1-3 plan style. The fat-tab composition form (list + `*`-unpack) is settled by an executable check (Task 4 Step 1) before the real UI is written.

**Type/name consistency:** `run.command_config`/`run.build_config`/`run.constants_config` are `reactive.Calc`, `run.active_executable`/`run.run_executable_name` are `reactive.Value` (Task 2), all read via `run.X()` everywhere (Tasks 4, 5). **`command_config` returns a `List[str]` argv, not a name** — the bare Run-Model exe name is `run.run_executable_name()`; the Model-Build exe name is `run.active_executable()` (two different widgets). The `sim_output_dir`/`run_executable` cross-namespace bridges use `session.root_scope().make_scope("run_control")` + a **bare** id (read + write) — the hyphenated literal `"run_control-…"` form is banned (crashes `validate_id` on read) and is grep-guarded in Tasks 4 & 6.

**Ordering rationale:** model_build → run_control → dashboard keeps every commit shippable — model_build's sibling `run_executable` update targets the still-root id during Task 3 (correct then), and is repointed to the `make_scope` form atomically in Task 4 when run_control claims it; dashboard is last because Task 2 already routed all its cross-reads through `run.*`, so it converts as a pure consumer with no sibling-input landmines.

**Adversarial-review corrections applied (workflow `wf_1d0587e4-5fd`, 13 confirmed findings):** (1) fat-tab must return a `list` + `*`-unpack — the single-arg `TagList` form crashes `.tagify()` (Task 4 Steps 1/2/3/5); (2) `run.command_config()` is a list, so exe-name reads route through the NEW `run.run_executable_name`, and quick-run's constants-validation reads through the NEW `run.constants_config` — not `command_config` (Task 2 Step 3/4, Interfaces); (3) the `sim_output_dir` read-side bridge uses `make_scope`, not a hyphenated literal id (`ValueError`) (Task 4 Step 4); (4) `test_ui_panels.py` calls the three `panel_*` by name → its cases are removed / the file deleted per-task, coverage migrated into the module render tests (Tasks 3/4/5); (5) the pre-namespacing ruff gate is scoped to `tests/python/` (app.py has ~83 WIP-exempt errors) (Tasks 1/2). Also: the Task-2/Task-5 render/handler bodies (system_status_compact, handle_quick_run) are **not** pytest-covered — their type-correctness rests on careful reading + the integration-tests CI, flagged at each site.

**Coverage caveat:** the `handle_quick_run` pre-flight validation branch and the live `@render`/`@reactive.event` bodies are exercised only by Playwright/manual QA, so a type error in the Task-2 rewiring ships silently past pytest/ruff/`tagify()` — the integration-tests job is the backstop; do not treat a green local suite as proof these paths work.

**Key risks flagged for review-in-loop (RE-VERIFY at execution):** (1) the `sim_output_dir` bridge break + repoint (top risk — Task 4 Step 4 + boot-smoke round-trip); (2) the fat-tab `TagList`-of-two-`nav_panel`s composition (Task 4 Step 1 de-risk); (3) the dead-code deletions (Task 1 Step 1 re-verify-zero-refs before dropping); (4) the bucket/id mismatches — `copy_mini_log`/`run_log_mini`/`run_status_indicator`/`goto_build` register in `run_control` despite thematic names (their ids live in the Run Model tab).
