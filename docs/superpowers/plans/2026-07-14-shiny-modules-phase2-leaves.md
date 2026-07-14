# Shiny-Modules Rearchitecture — Phase 2 (Leaf Modules) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convert the remaining self-contained tabs into true `@module.ui`/`@module.server` Shiny modules by applying the proven Phase-1 `parameters` template: `model_structure`, `map`, `model_options`, `initial_conditions`, `input_files`, `scenarios`, and `sim_config` — plus a decision on `chrome`.

**Architecture:** Each tab becomes `shiny_app/modules/<tab>.py` with `<tab>_ui()` (`@module.ui`, panel content only) + `<tab>_server(input, output, session, state)` (`@module.server`, verbatim handler port). `create_ui()` wraps each in the app-level `panel_conditional`; `server()` deletes the inline block and calls `<tab>_server("<id>", state)`. Within-tab ids namespace to `<id>-*`; nav ids stay global. This is a mechanical repetition of Phase 1 — the recipe below is the unit of work.

**Tech Stack:** Python 3.10+, Shiny for Python 1.5.x (`shiny.module`), pytest, Playwright/Selenium (CI-only).

**Spec:** `docs/superpowers/specs/2026-07-14-app-py-shiny-modules-rearchitecture-design.md` (§4, §7 Phase 2). Template: `shiny_app/modules/parameters.py` (Phase 1, `v0.4.1`).

## Global Constraints

- **Behavior-identical:** each tab's flow is unchanged; ONLY within-tab widget ids gain the `<module>-` namespace prefix. Nav ids (`nav_*`) and every other tab stay untouched.
- **Module pattern (from the pilot):** `x_ui(id)` returns panel *content* only (no `panel_conditional`, no nav knowledge); the `panel_conditional("input.navigation === 'nav_x'", x_ui("x"))` wrapper lives in `create_ui()`. `x_server(id, state)` takes `state` even if unused.
- **Self-contained modules:** import only stdlib + already-extracted leaf modules (`options_parser`, `ic_parser`, `input_analysis`, `file_locators`, `box_network`, `scenarios`, `simulation_config`) + `app_state` via the `try/except ImportError` fallback; never `app.py`. `logging.getLogger("AQUABC")`. Each module **self-computes** `ROOT`/`INPUTS_DIR` (2 lines, matching `parameters.py` and the repo's per-module convention — a shared paths helper was evaluated and rejected: it would not single-source path logic, since `app.py`/leaf modules keep self-computing, so it only adds a third pattern).
- **Verbatim ports:** handler bodies + reactive values move unchanged; only the `panel_conditional` is stripped from the UI and the reactive-value declarations move inside the module server.
- **Lint-clean:** `ruff check shiny_app/modules/ tests/python/` must pass (the modules subpackage is fully lint-gated). Note the Phase-1 lessons: order imported names for isort (uppercase-first); test helpers go **inline** (`tests/python/` is a package).
- **Integration tests are CI-only** — the namespaced-selector migrations are verified by CI on push; locally, use a render-smoke unit test + boot smoke.
- **Commit per task.**

## The Module-Conversion Recipe (the repeatable unit — mirrors `parameters.py`)

For a tab with module id `X`, panel `panel_X()` in `ui_panels.py`, and inline handlers in `app.py`:

1. **Create `shiny_app/modules/X.py`:**
   - Module docstring + `import logging, os`; `from shiny import module, reactive, render, ui`; `try/except` import of the tab's leaf module(s); `logger = logging.getLogger("AQUABC")`; self-computed `ROOT`/`INPUTS_DIR`.
   - `@module.ui def X_ui():` — return the **content** of `panel_X()` (the `ui.card(...)`/inner tree) with the `ui.panel_conditional("input.navigation === 'nav_X'", …)` wrapper **stripped**. Ids stay bare (they namespace automatically).
   - `@module.server def X_server(input, output, session, state):` — declare the tab's `reactive.Value`s, then the handlers ported **verbatim**.
2. **Create `tests/python/test_X_module.py`:** a render-smoke test — `str(X_ui("X"))` contains each namespaced id `X-<widget>`, and `panel_conditional`/`input.navigation` are absent. Define `nid(m, i)` **inline** (no separate module).
3. **Wire into `app.py`:** add the fallback import of `X_ui, X_server`; in `create_ui()` replace `panel_X(),` with `ui.panel_conditional("input.navigation === 'nav_X'", X_ui("X")),`; delete the inline handler block + reactive values and put `X_server("X", state)` in their place; remove `panel_X` from any `ui_panels` import list in `app.py`.
4. **`ui_panels.py`:** delete `panel_X()`; if a generic `ARGFREE`/panel-list test enumerates it (`test_ui_panels.py`), drop that entry.
5. **Migrate integration selectors** (same commit): the tab's `#<id>`/`By.ID, "<id>"` selectors → `<X>-<id>` in `test_app_playwright.py`, `test_app_selenium.py`, `test_tutorial_playwright.py`. Nav ids + `navigate_to(..., "nav_X")` stay.
6. **Gate:** `py_compile`; `ruff check shiny_app/modules/ tests/python/` clean; render test + full suite green (+1 per new render test); grep-clean (old handlers/panel gone); boot smoke (`curl` shows `X-<id>` namespaced, 0 bare leaks; ws session runs `server()` with no traceback).

## Per-Module Spec (source line ranges are current on `main` @ v0.4.1; grep to confirm before editing)

| id | `panel_` (ui_panels.py) | handlers (app.py) | reactive.Values | leaf deps | dynamic inputs | test selectors → namespaced |
|---|---|---|---|---|---|---|
| `model_structure` | `panel_model_structure` @1124 | `model_structure_iframe` @4954 | none | none (iframe) | none | none (only `nav_model_structure`, stays) |
| `map` | `panel_map` @1049 | `pydeck_map` @4856, `map_info` @4928 | none | (pydeck/box data — confirm imports) | none | none (only `nav_map`) |
| `model_options` | `panel_model_options` @627 | `load_options_files` @2105, `options_switches` @2128, `options_constants` @2187, `save_model_options` @2234, `options_save_status` @2328 | `options_file_obj`, `extra_const_file_obj`, `options_save_msg` | `options_parser` | `options_switches`/`options_constants` create dynamic ids — **verify they read back inside the module** | `#load_options` |
| `initial_conditions` | `panel_initial_conditions` @571 | `load_ic_file` @1948, `ic_category_info` @1969, `ic_table` @1992, `save_initial_conditions` @2042, `ic_save_status` @2091 | `ic_file_obj`, `ic_save_msg` | `ic_parser` | `ic_table` creates `ic_{id}` inputs read in `save_initial_conditions` (like `param_{id}` — round-trips inside module) | `#load_ics` |
| `input_files` | `panel_input_files` @497 | `refresh_files` @1681, `load_file` @1688, `file_header_text` @1707, `file_info_panel` @1715, `save_file` @1829, `save_status` @1867, `map_display_plot` @1873, `map_display_info` @1893, **+ the anonymous file-list `@reactive.effect`** (~1650, populates `file_select`) | `file_list_version`, `save_status_msg` | `input_analysis`, `file_locators`, `box_network` | — | `#file_select`, `#file_contents` (multiple) |
| `scenarios` | `panel_scenarios` @683 | `init_scenario_manager` @2689, `refresh_scenario_list` @2697, `scenario_info` @2706, `update_scenario_choices` @2740, `load_selected_scenario` @2752, `save_new_scenario` @2787, `delete_selected_scenario` @2850, `scenario_status` @2883 | `scenario_mgr`, `scenario_status_msg` | `scenarios` (leaf) | — | none (only `nav_scenarios`) |
| `sim_config` | **sub-tab of `panel_model_control`** (NOT `panel_sim_config`, which is dead) | `load_simulation_config_file` @2344, `update_timesteps_from_preset` @2410, `update_output_from_preset` @2418, `sim_duration_info` @2430, `sim_timestep_info` @2456, `sim_output_info` @2469, `save_simulation_config` @2486, `sim_config_save_status` @2571 | `sim_config_obj`, `sim_config_save_msg` | `simulation_config` | — | `#load_sim_config`, `#sim_base_year` |

---

### Task 1: `model_structure` + `map` modules (two trivial leaves)

**Files:** Create `shiny_app/modules/model_structure.py`, `shiny_app/modules/map.py`, `tests/python/test_model_structure_module.py`, `tests/python/test_map_module.py`. Modify `shiny_app/app.py`, `shiny_app/ui_panels.py`.

Both have **no reactive values and no within-tab test selectors**, so each is a pure UI+render lift. Apply the recipe to both in one commit (module id `model_structure` and `map`). First confirm `map`'s leaf imports (grep `panel_map`/`pydeck_map`/`map_info` bodies for what they call — likely `box_network`/pydeck; import those into `map.py`).

- [ ] **Step 1: Create both modules** (recipe step 1) — `model_structure_ui`/`_server` (server registers only `model_structure_iframe`); `map_ui`/`_server` (registers `pydeck_map`, `map_info`). `state` accepted, unused.
- [ ] **Step 2: Render-smoke tests** (recipe step 2) — assert the namespaced output ids appear (`model_structure-model_structure_iframe`; `map-pydeck_map`, `map-map_info`) and no `panel_conditional`.
- [ ] **Step 3: RED → confirm both tests fail (modules missing).**
- [ ] **Step 4: Implement modules → GREEN.**
- [ ] **Step 5: Wire both into app.py** (recipe steps 3-4): imports; `create_ui` swaps `panel_model_structure()`/`panel_map()` for the `panel_conditional(..., X_ui("X"))` forms; delete the two inline render blocks + the two `panel_` functions; `X_server("X", state)` calls.
- [ ] **Step 6: Gate** — `py_compile`; ruff clean; grep-clean (`model_structure_iframe`/`pydeck_map`/`map_info`/`panel_model_structure`/`panel_map` gone from app.py+ui_panels); full suite (**+2**); boot smoke (both tabs render, ws no traceback).
- [ ] **Step 7: Commit** — `feat(shiny): model_structure + map Shiny modules`

---

### Task 2: `model_options` module

**Files:** Create `shiny_app/modules/model_options.py`, `tests/python/test_model_options_module.py`. Modify `app.py`, `ui_panels.py`, integration tests (`#load_options`).

Apply the recipe (id `model_options`). Import `options_parser`. Port the 5 handlers + 3 reactive values (`options_file_obj`, `extra_const_file_obj`, `options_save_msg`). **Verify** any dynamic option-widget ids created in `options_switches`/`options_constants` are read back within the module (namespace symmetrically, like `param_{id}` did). Migrate `#load_options` → `model_options-load_options` (and any other `#options_*` the tests use — grep).

- [ ] Steps mirror Task 1 / the recipe (RED render test → module → wire → migrate `#load_options` → gate: py_compile, ruff, grep-clean, suite **+1**, boot smoke shows `model_options-*`).
- [ ] **Commit** — `refactor(shiny): model_options Shiny module`

---

### Task 3: `initial_conditions` module

**Files:** Create `shiny_app/modules/initial_conditions.py`, `tests/python/test_initial_conditions_module.py`. Modify `app.py`, `ui_panels.py`, integration tests (`#load_ics`).

Apply the recipe (id `initial_conditions`). Import `ic_parser`. Port the 5 handlers + 2 reactive values (`ic_file_obj`, `ic_save_msg`). Note `ic_table` creates dynamic `ic_{id}` numeric inputs read in `save_initial_conditions` — this is the **same dynamic-input pattern the pilot proved** (creates + reads inside the same module server → namespaces symmetrically). Migrate `#load_ics` → `initial_conditions-load_ics`.

- [ ] Steps per the recipe (RED → module → wire → migrate `#load_ics` → gate incl. boot smoke showing `initial_conditions-ic_table` etc.).
- [ ] **Commit** — `refactor(shiny): initial_conditions Shiny module`

---

### Task 4: `input_files` module (the largest leaf)

**Files:** Create `shiny_app/modules/input_files.py`, `tests/python/test_input_files_module.py`. Modify `app.py`, `ui_panels.py`, integration tests (`#file_select`, `#file_contents`).

Apply the recipe (id `input_files`). Import `input_analysis`, `file_locators`, `box_network`. Port the 8 handlers (`refresh_files`, `load_file`, `file_header_text`, `file_info_panel`, `save_file`, `save_status`, `map_display_plot`, `map_display_info`) **plus the anonymous `@reactive.effect` that populates `file_select`** (near the reactive-value declarations, ~1650) + 2 reactive values (`file_list_version`, `save_status_msg`). Migrate `#file_select` and `#file_contents` (multiple occurrences across selenium/playwright/tutorial) → `input_files-file_select`/`input_files-file_contents`.

- [ ] Steps per the recipe. **Extra care:** the anonymous file-list effect must move too (grep for `file_list_version.get()` + `ui.update_select("file_select"...)` to find it); `map_display_plot` uses `box_network.*` — confirm all box_network calls resolve via the module's import.
- [ ] Gate: py_compile, ruff, grep-clean (all 8 handlers + the anon effect + `panel_input_files` gone), suite **+1**, boot smoke (`input_files-file_select` present, 0 bare `#file_select` leaks).
- [ ] **Commit** — `refactor(shiny): input_files Shiny module`

---

### Task 5: `scenarios` module

**Files:** Create `shiny_app/modules/scenarios.py`, `tests/python/test_scenarios_module.py`. Modify `app.py`, `ui_panels.py`.

Apply the recipe (id `scenarios`). Import the `scenarios` leaf module (mind the name clash: the module file is `shiny_app/modules/scenarios.py` importing from `shiny_app/scenarios.py` — use the fully-qualified `try/except` import so there's no self-import/recursion, e.g. `from shiny_app.scenarios import …` / `from scenarios import …`; do NOT use a bare relative import). Port the 8 handlers + 2 reactive values (`scenario_mgr`, `scenario_status_msg`). No integration selectors to migrate.

- [ ] Steps per the recipe. **Named risk:** the module-name collision (`modules/scenarios.py` vs leaf `scenarios.py`) — verify the import resolves to the leaf and `import shiny_app.modules.scenarios` doesn't recurse (this exact hazard was called out in an earlier phase). Add an import-check to the gate: `.venv/bin/python -c "import shiny_app.modules.scenarios"`.
- [ ] **Commit** — `refactor(shiny): scenarios Shiny module`

---

### Task 6: `sim_config` module (partial fat-tab extraction)

**Files:** Create `shiny_app/modules/sim_config.py`, `tests/python/test_sim_config_module.py`. Modify `app.py`, `ui_panels.py` (`panel_model_control` + delete `panel_sim_config`), integration tests (`#load_sim_config`, `#sim_base_year`).

`sim_config` is **not** a standalone `panel_conditional` — it is the first `ui.nav_panel("Simulation Config", …)` inside `panel_model_control`'s `ui.navset_card_tab(...)` (ui_panels.py ~223). The sibling nav_panels (run parameters, output config) stay inline until Phase 4. Approach:

- `sim_config_ui()` (`@module.ui`) returns the **`ui.nav_panel("Simulation Config", <content>)`** (the sub-tab), content ported from `panel_model_control`.
- `panel_model_control()` composes it: `ui.navset_card_tab(sim_config_ui("sim_config"), ui.nav_panel("Run Parameters", <inline>), ui.nav_panel(<output config inline>), ...)` — i.e. the module supplies ONE nav_panel, the rest stay inline (still un-namespaced). The outer `panel_conditional`/`navset_card_tab` stay in `panel_model_control`.
- `sim_config_server("sim_config", state)` — port the 8 handlers + 2 reactive values (`sim_config_obj`, `sim_config_save_msg`). **`state` is used here:** `save_simulation_config` already bumps `state.sim_config_version` (wired in Phase 0) — keep that, now inside the module.
- **Delete** the dead `panel_sim_config()` (ui_panels.py @674) and its `create_ui()` call (app.py:511).
- Migrate `#load_sim_config`/`#sim_base_year` → `sim_config-load_sim_config`/`sim_config-sim_base_year`.

- [ ] **Structural approach VERIFIED (empirically, at plan time):** a `@module.ui` returning a `nav_panel` composed into an app-level `ui.navset_card_tab(...)` DOES namespace its inner ids (`sim_config-load_sim_config`/`sim_config-sim_base_year`) while a sibling inline `nav_panel` stays un-namespaced. **Gotcha for the render test:** `NavPanel`/`NavSetCard` do NOT render HTML via `str()` (they return a Python repr) — the render-smoke test MUST tagify: assert against `str(ui.navset_card_tab(sim_config_ui("sim_config")).tagify())` (or `str(sim_config_ui("sim_config").tagify())`), NOT bare `str(sim_config_ui(...))`.
- [ ] **Remaining named risks (verify):** (a) the sibling inline nav_panels' ids are UNCHANGED (they belong to Phase-4 `run_control`); (b) `save_simulation_config` still bumps `state.sim_config_version` (Phase-0 wiring), now inside the module.
- [ ] Gate: py_compile, ruff, grep-clean (8 handlers + `panel_sim_config` gone; the sibling model_control handlers still present), suite **+1**, boot smoke (`sim_config-load_sim_config` present; Model Config tab still shows all its sub-tabs).
- [ ] **Commit** — `refactor(shiny): sim_config Shiny module (extract Model Config sub-tab; drop dead panel_sim_config)`

---

### Task 7: `chrome` — decision + resolution

**Files:** `shiny_app/app.py` (or a small module), possibly `shiny_app/ui_chrome.py`; docs.

**The finding:** `help_content`/`changelog_content` (app.py @1405/@1555) render into the `helpOffcanvas`/`changelogOffcanvas` offcanvases, whose **container ids are referenced by JS** (`ui_scripts.py:108/118` `getElementById('helpOffcanvas')`). Those structural ids must stay un-namespaced. Making `chrome` a true module would namespace `help_content`→`chrome-help_content`, forcing `ui_chrome`'s app-level `output_ui("help_content")` to somehow learn the namespace (it builds the offcanvas at app level) — awkward for two static, cross-tab-uncoupled renders.

**Recommendation (default): keep `help_content`/`changelog_content` as app-level `@render.ui`s in the thin `server()` — do NOT make `chrome` a module.** This reverses the spec §6 "chrome module" resolution based on this implementation finding; the "pure assembler" success criterion takes a documented 2-render exception (total modules 17 → 16). No code change to the renders; just document the decision (update the §6 resolution + memory).

**Alternative (if a module is still wanted):** `chrome_ui("chrome")` returns only the two namespaced `output_ui` placeholders; `ui_chrome`'s offcanvas divs (keeping their un-namespaced structural ids + JS refs) embed `chrome_ui`'s outputs; `chrome_server("chrome", state)` registers the two renders. More wiring for zero behavioral benefit.

- [ ] **Present the decision to the user** (this reverses a prior §6 choice); default to keep-app-level.
- [ ] If keep-app-level: update spec §6 + `docs`/memory to record it; no code change. If module: apply the alternative.
- [ ] **Commit** — `docs: chrome stays app-level (offcanvas ids are JS-referenced) — spec §6 refinement` (or the module impl).

---

### Task 8: Phase-2 regression gate

**Files:** none — verification.

- [ ] **Static + unit:** `py_compile shiny_app/app.py`; `import shiny_app.app` + each `import shiny_app.modules.<X>`; `ruff check shiny_app/modules/ tests/python/` clean; full suite (**166 + one render test per module** ≈ 173-174).
- [ ] **Grep-clean:** every converted tab's old inline handlers + `panel_<X>` gone from `app.py`/`ui_panels.py`; the dead `panel_sim_config` gone.
- [ ] **Boot smoke:** app serves; each converted tab renders with `<X>-*` namespaced ids and **zero bare-id leaks** for the migrated selectors; a ws session runs `server()` (all module servers registered) with no traceback.
- [ ] **CI (on push):** the `integration-tests` job drives the migrated `<X>-*` selectors — the authoritative DOM proof.
- [ ] **(Deferred) release** — `v0.4.2` at the finishing step per the Phase-0/1 cadence.

---

## Self-Review

**Spec coverage (§7 Phase 2 → tasks):** the 8 named tabs → Tasks 1-6 (7 tab modules) + Task 7 (chrome decision). Recipe encodes the §4 module pattern (content-only UI, panel_conditional in create_ui, `x_server(id, state)`). Selector migration folded into each conversion commit (per §10 discipline). Regression + CI DOM net → Task 8.

**Placeholder scan:** the recipe + per-module spec table give exact source ranges, ids, deps, and selectors for each module; the two intricate cases (sim_config fat-tab, chrome offcanvas) get explicit structural treatment and named risks. Full per-module code is intentionally NOT pasted — each module is a mechanical application of the proven `parameters.py` template (Phase 1, v0.4.1); the implementer follows that file as the reference and ports the specified handlers/panel. This is a deliberate DRY choice for 7 near-identical conversions, not a placeholder.

**Type/consistency:** every module follows `X_ui()`/`X_server(input, output, session, state)` called as `X_ui("X")`/`X_server("X", state)` (verified in Phase 1). Self-computed `ROOT`/`INPUTS_DIR` per module (consistent with repo convention).

**Open decisions carried to review/user:**
1. **chrome** (Task 7): keep app-level (recommended) vs a chrome module — reverses spec §6; needs the user's call.
2. **sim_config** (Task 6): the fat-tab structural approach is **VERIFIED sound** (empirically at plan time — a module `nav_panel` composes into an app-level navset and namespaces correctly; sibling inline nav_panels stay un-namespaced). The only carried caveat is the render-test method (must `.tagify()`, since nav objects don't `str()`-render) — folded into Task 6.
3. Shared paths helper: evaluated and **rejected** (self-compute matches repo convention; a modules-only helper wouldn't single-source it). Noted in case the reviewer prefers otherwise.
