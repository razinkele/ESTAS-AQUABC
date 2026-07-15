# Shiny-modules Rearchitecture — Phase 5 (final cleanup) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close out the `shiny_app/app.py` Shiny-modules rearchitecture — remove the dead weight left after all 15 tabs became modules (an unread contract value, an empty stub file, ~70 dead imports), and mark the rearchitecture complete.

**Architecture:** Pure cleanup — no new modules, no behavior change. Three small removals (the unread `run.build_config` contract leg; the empty `ui_panels.py`; app.py's dead imports + trivial lint), then a docs/CHANGELOG capstone and a regression gate. Every task is behavior-neutral and verified by `import shiny_app.app` + the full suite.

**Tech Stack:** Python 3.10, Shiny for Python 1.5.1, pytest, ruff.

## Global Constraints

*(Every task's requirements implicitly include this section.)*

- **Behavior-neutral.** Nothing a user sees changes. No id renames, no handler edits, no logic changes. Only removal of provably-dead code + lint hygiene.
- **Use `.venv/bin/python`** for anything importing `app.py` (system `python3` lacks `networkx`).
- **Verify every removal** with `.venv/bin/python -c "import shiny_app.app; from shiny_app.app import create_ui; str(create_ui().tagify())"` (clean import + render) AND `.venv/bin/python -m pytest tests/python/ -q` (full suite green). **CRITICAL GOTCHA: the test suite does NOT import `shiny_app.app`** (the module tests import `shiny_app.modules.*` / `shiny_app.app_state`), so a green suite does **not** prove app.py imports — a removed-but-still-used import passes the suite and only fails at `import shiny_app.app` / `create_ui()`. The explicit import+render check is therefore MANDATORY for any app.py edit, and is the real backstop (empirically confirmed during review: a wrongly-removed `INPUT_FILE_CATEGORIES` gave `NameError` at import while the suite stayed 178-green).
- **ruff scope for tests:** `ruff check tests/python/` (CI scope). For app.py-specific checks, use `--isolated` to bypass the per-file-ignore when detecting/verifying (e.g. `ruff check shiny_app/app.py --select F401 --isolated`). `shiny_app/modules/` stays fully gated (`ruff check shiny_app/modules/`).
- **The rearchitecture end-state (spec §9 success criteria):** `server()` is a thin assembler (already true: state construction + 15 `x_server` calls + 2 chrome renders); 15 cohesive modules + `diagnostics`; no `input.X` crosses a module boundary except via `RunController`/`AppState`/the `make_scope` bridge. Phase 5 does not change this — it removes leftovers and records completion.
- **Line numbers are a `v0.4.4` baseline** and shift as removals land — grep current locations by name.
- **Commit per task.**

---

## File Structure

- **Modify:** `shiny_app/app_state.py` (drop the `build_config` field + docstring mention — Task 1); `shiny_app/modules/model_build.py` (drop the `_build_config` calc + registration + docstring mention — Task 1); `tests/python/test_run_controller.py` (drop the `build_config` assertions/pin — Task 1); `shiny_app/app.py` (remove ~70 dead imports + 6 F541 — Task 3); `pyproject.toml` (drop the now-unneeded `F401` per-file-ignore for app.py — Task 3); `CHANGELOG.md` (capstone — Task 4).
- **Delete:** `shiny_app/ui_panels.py` (empty docstring-only stub — Task 2).

---

## Task 1: Remove the unread `run.build_config` contract value

**Rationale:** `run.build_config` (a `reactive.calc` registered in `model_build.py:385`) has **zero functional readers** app-wide (verified: the only `.build_config()` call is the test pin). Its consumer was `on_build_run` (the "Build & Run" handler), removed as dead code in Phase-4 Task 1. Per the final whole-branch review, this write-only contract leg is debt to clean. **Decision (flagged for review-in-loop): remove it.** YAGNI — if a "Build & Run" button returns, re-adding a one-line `reactive.calc` is trivial. *(Alternative: keep it documented as "reserved" — rejected because, unlike Phase-0's `selected_*`/`active_executable` which had future consumers, this one's consumer is gone, not pending.)*

**Files:**
- Modify: `shiny_app/modules/model_build.py`, `shiny_app/app_state.py`, `tests/python/test_run_controller.py`

**Interfaces:** After this task, `RunController` has 4 registered contract calcs/values (`command_config`, `constants_config`, `run_executable_name`, `active_executable`) — `build_config` is gone. (`exe_list_version` remains — it is separate and live.)

- [ ] **Step 1: Confirm still zero readers (guard against a new consumer).**
```bash
cd /home/razinka/AQUABCv0.2
grep -rn "build_config" shiny_app/ tests/python/ --include=*.py | grep -v "test_run_controller"
```
Expected: only `app_state.py` (field + docstring) and `model_build.py` (the `_build_config` def + registration + a docstring mention) — NO `.build_config()` read in any `shiny_app/modules/*.py` or `app.py`. If a real reader appears, STOP and report (removal would break it).

- [ ] **Step 2: Remove the registration from `model_build.py`.** Delete the `_build_config` `reactive.calc` function and the `run.build_config = _build_config` line (grep `def _build_config` / `run.build_config`). Update the module docstring line that mentions "plus the `run.build_config`" to drop that clause.

- [ ] **Step 3: Remove the field from `app_state.py`.** Delete `self.build_config = None` (the `# Callable[[], dict]` line) from `RunController.__init__`, and remove `build_config` from the class docstring's attribute list.

- [ ] **Step 4: Update `test_run_controller.py`.** Remove the `build_config` assertions: the two `assert rc.build_config is None` lines, and the pin block that does `rc.build_config = reactive.calc(...)` + `assert rc.build_config()["compiler"] == "gfortran"`. Keep the rest of the contract pin (command_config/run_executable_name/constants_config/active_executable) intact. Update the test's docstring/comment that lists `build_config` among the pinned attrs.

- [ ] **Step 5: Verify.**
```bash
.venv/bin/python -c "import shiny_app.app; import shiny_app.app_state; print('import OK')"
grep -rn "build_config" shiny_app/ tests/python/ --include=*.py || echo "clean — build_config fully removed"
.venv/bin/python -m pytest tests/python/ -q
ruff check shiny_app/modules/model_build.py tests/python/test_run_controller.py
```
Expected: import OK; `build_config` fully gone; full suite green (one fewer assertion, same test count or −0); ruff clean.

- [ ] **Step 6: Commit.**
```bash
git add shiny_app/modules/model_build.py shiny_app/app_state.py tests/python/test_run_controller.py
git commit -m "refactor(shiny): drop unread run.build_config contract value

Its consumer (on_build_run) was removed as dead code in Phase 4; the
reactive.calc had zero readers. RunController's cross-module contract is now
command_config/constants_config/run_executable_name/active_executable."
```

---

## Task 2: Delete the empty `ui_panels.py` stub

**Rationale:** `shiny_app/ui_panels.py` is a 6-line docstring-only file — all three panels it once held (`panel_dashboard`, `panel_model_build`, `panel_model_control`) became modules in Phase 4. Nothing imports it (grep-confirmed: only docstring/comment mentions remain in other modules).

**Files:**
- Delete: `shiny_app/ui_panels.py`

- [ ] **Step 1: Confirm no code imports it.**
```bash
cd /home/razinka/AQUABCv0.2
grep -rn "import ui_panels\|from shiny_app.ui_panels\|from ui_panels\|ui_panels\." shiny_app/ tests/python/ --include=*.py | grep -v "^Binary"
```
Expected: only docstring/comment strings (e.g. "imports nothing from … ui_panels", "content moved verbatim from ui_panels.py:…") — NO actual `import` statement or `ui_panels.X` call. If a real import exists, STOP and report.

- [ ] **Step 2: Delete the file.**
```bash
git rm shiny_app/ui_panels.py
```

- [ ] **Step 3: Verify + tidy stale mentions (optional, same commit).** The docstring/comment references in `shiny_app/modules/{sim_config,dashboard,run_control}.py` (e.g. "imports nothing from app.py or ui_panels.py") remain accurate (they still import nothing from it) — leave them, OR trim the now-dangling "ui_panels.py:220-393" historical line-refs in `run_control.py` if trivial. Do NOT churn the modules for this.
```bash
.venv/bin/python -c "import shiny_app.app; print('import OK')"
.venv/bin/python -m pytest tests/python/ -q
```
Expected: import OK (nothing imported the file); full suite green (same count).

- [ ] **Step 4: Commit.**
```bash
git add -A
git commit -m "chore(shiny): delete empty ui_panels.py stub (all panels are modules)"
```

---

## Task 3: app.py dead-import + trivial-lint cleanup

**Rationale:** app.py accumulated **70 dead imports** (F401) as inline handlers moved into modules (the modules import their own leaf parsers). It also has 6 F541 (f-strings without placeholders). Removing these lets the `F401` per-file-ignore be dropped. **Keep** the E402 (7, sys.path-ordering) + F841 (1, reactive side-effect) + B023/S602/S605 ignores — those are structural and legitimate.

**⚠️ Two review-verified hazards (do NOT deviate):**
- **`ruff --fix` only removes the ~23 UNCONDITIONAL dead imports** (stdlib/viz/widgets). It **refuses** to touch the **53 dead names inside the `try/except ImportError` fallback blocks** (it emits "consider using `importlib.util.find_spec`" and leaves them). `--unsafe-fixes` does NOT help either (verified). Those 53 must be removed **by hand**.
- **`input_analysis` is PARTIALLY dead** — of its 4 imported names, 3 are dead but **`INPUT_FILE_CATEGORIES` is still USED** (it does not appear in the F401 list). Deleting its whole block breaks app.py with `NameError: INPUT_FILE_CATEGORIES` (verified). Every other leaf block is FULLY dead.

**Files:**
- Modify: `shiny_app/app.py`, `pyproject.toml`

- [ ] **Step 1: Snapshot the dead-name list (authoritative — the removal must match it exactly).**
```bash
cd /home/razinka/AQUABCv0.2
ruff check shiny_app/app.py --select F401,F541 --isolated 2>&1 | tee /tmp/app_lint_before.txt | tail -3
echo "F401 count:"; grep -c F401 /tmp/app_lint_before.txt
```
Expected: 70 F401 + 6 F541. Keep this file open — the manual removals in Step 3 must remove **exactly** these names, nothing more.

- [ ] **Step 2: Autofix the UNCONDITIONAL dead imports, then review.**
```bash
ruff check shiny_app/app.py --select F401,F541 --fix --isolated 2>&1 | tail -2
git diff --stat shiny_app/app.py
ruff check shiny_app/app.py --select F401,F541 --isolated 2>&1 | tail -1   # ~53 conditional F401 REMAIN — expected
```
This removes the top-level stdlib/viz/widget imports (`threading`, `shutil`, `time`, `select`, `signal`, `traceback`, `re`, `shlex`, `datetime.date`, `datetime.timedelta`, `numpy`, `plotly.express`, `plotly.graph_objects`, `networkx`, `shiny.req`, `shinywidgets.output_widget`, `shinywidgets.render_widget`, + the 6 F541). **Expect ~53 F401 to REMAIN** — those are the conditional blocks Step 3 handles. Confirm the autofix left the file parseable (`python3 -c "import ast; ast.parse(open('shiny_app/app.py').read())"`).

- [ ] **Step 3: Manually remove the 53 conditional dead imports.** For each leaf-module `try: from shiny_app.<mod> import … except ImportError: from <mod> import …` block:
  - **DELETE THE WHOLE `try/except` BLOCK** (both branches) for these **12 FULLY-DEAD** modules: `parameter_parser`, `ic_parser`, `options_parser`, `simulation_config`, `scenarios`, `utils`, `safe_resolve`, `compiler_env`, `file_locators`, and the three module-level imports `build_commands`, `box_network`, `output_data` (`from shiny_app import build_commands` / `import build_commands`, etc. — grep-confirmed 0 `build_commands.`/`box_network.`/`output_data.` references in app.py).
  - **For `input_analysis` (PARTIALLY dead): edit, do NOT delete.** Remove only `analyze_input_file`, `get_input_file_categories`, `validate_required_inputs` from **both** the `try` and `except` branches; **KEEP `INPUT_FILE_CATEGORIES`** (still used). The block stays as `try: from shiny_app.input_analysis import INPUT_FILE_CATEGORIES except ImportError: from input_analysis import INPUT_FILE_CATEGORIES`.
  - **KEEP entirely** (still used — NOT in the dead list): the `diagnostics`, `ui_scripts`, `ui_chrome`, `app_state`, and all `modules.*` import blocks.

- [ ] **Step 4: Drop the `F401` per-file-ignore for app.py.** In `pyproject.toml`, remove the `"F401",` line (and its comment) from the `"shiny_app/app.py"` per-file-ignores block. **Keep** `F841`, `B023`, `E402`, `S602`, `S605` (7 E402 + 1 F841 remain and are legitimate). The block header comment can stay.

- [ ] **Step 5: Verify — IMPORT+RENDER is the essential check (the suite does NOT import app.py).**
```bash
# THE backstop — a wrongly-removed used import fails HERE, not in the suite:
.venv/bin/python -c "import shiny_app.app; from shiny_app.app import create_ui; str(create_ui().tagify()); print('import + render OK')"
ruff check shiny_app/app.py --select F401,F541 --isolated 2>&1 | tail -1   # expect "All checks passed!"
ruff check shiny_app/app.py 2>&1 | tail -1                                  # config-aware (F401 now un-ignored): no F401/F541; E402/F841 stay suppressed
.venv/bin/python -m pytest tests/python/ -q
ruff check shiny_app/modules/ tests/python/
```
Expected: **import + render OK** (if this raises `NameError`, a used import was removed — restore it, esp. check `INPUT_FILE_CATEGORIES`); `--isolated` F401/F541 → "All checks passed!"; config-aware ruff clean of F401/F541; full suite green (same count); modules/tests clean. app.py should drop by ~110-120 lines (871 → ~755).

- [ ] **Step 6: Commit.**
```bash
git add shiny_app/app.py pyproject.toml
git commit -m "refactor(shiny): remove ~70 dead imports from app.py + drop F401 ignore

Imports orphaned as inline handlers moved into modules. Autofix cleared the
unconditional ones; the try/except-fallback leaf-import blocks removed by hand
(input_analysis keeps INPUT_FILE_CATEGORIES, still used). Also fixes 6 F541.
app.py is now F401/F541-clean; F401 per-file-ignore dropped (E402/F841/B023/
S602/S605 stay — structural)."
```

---

## Task 4: Docs/CHANGELOG capstone + regression gate (controller-run)

**Rationale:** Record that the rearchitecture is complete and run the final end-state verification.

**Files:**
- Modify: `CHANGELOG.md`

- [ ] **Step 1: Regression gate (controller-run).**
  - `.venv/bin/python -c "import shiny_app.app"` clean; `create_ui().tagify()` renders; App constructs.
  - Full suite green (`.venv/bin/python -m pytest tests/python/ -q`); `ruff check shiny_app/modules/ shiny_app/diagnostics.py tests/python/` clean.
  - **End-state assertions:** `build_config` gone (`grep -rn build_config shiny_app/ tests/python/` empty); `ui_panels.py` gone (`ls shiny_app/ui_panels.py` → absent); app.py F401-clean (`ruff check shiny_app/app.py --select F401 --isolated` → 0); `server()` still a thin assembler (only `help_content`/`changelog_content` inline + 15 `x_server` calls); no `input.X` cross-module leak (`grep -nE "input\.(cmd_|build_|run_executable|active_executable|quick_run|output_boxes|sim_output_dir)" shiny_app/app.py shiny_app/modules/dashboard.py` empty); boot smoke (all 15 tabs render namespaced, zero bare-id leaks).
  - **CI (on push):** `integration-tests` green — the DOM behavioral proof for the full modular app.

- [ ] **Step 2: CHANGELOG capstone.** Add a `## [0.4.5]` "Changed" entry: Phase 5 removes the unread `run.build_config` contract leg, the empty `ui_panels.py`, and app.py's 70 dead imports (F401 ignore dropped); **the `shiny_app/app.py` Shiny-modules rearchitecture is complete** — `server()` reduced from ~5,600 lines to a thin assembler over 15 namespaced `@module.ui`/`@module.server` modules behind the `RunController`/`AppState` contract, all `v0.4.x` releases CI-verified. (The version bump + tag happen at finishing, per the release convention.)

- [ ] **Step 3: Commit** (folded into the release commit at finishing, or standalone `docs:` commit).

---

## Self-Review

**Spec coverage (§7 Phase 5 / §9 success criteria):** the spec's Phase 5 = "delete the dead placeholder, collapse `server()` to the thin assembler, final full suite + E2E, update docs." `server()` is already the thin assembler (achieved in Phase 4); this plan removes the three concrete leftovers (`build_config`, `ui_panels.py`, dead imports) + records completion. The success criteria (thin assembler, 15 modules + diagnostics, no cross-boundary `input.X`) are asserted in Task 4's gate.

**Placeholder scan:** every removal has an exact detection command + a guard step (confirm-still-dead before deleting) + the import/suite backstop. The one judgment call (`build_config` remove vs keep-reserved) is explicit in Task 1 with rationale, flagged for review-in-loop.

**Type/name consistency:** after Task 1, references to `run.build_config` are removed from all three files together (registration, field, test) — no dangling reader. The `F401` ignore removal (Task 3) is consistent with the dead-import removal in the same commit (app.py becomes F401-clean, so the ignore is safely dropped; E402/F841 stay because 7+1 legitimate instances remain).

**Risk (review-in-loop corrected Task 3 twice):** (1) `ruff --fix` removes only the ~23 unconditional dead imports and REFUSES the 53 `try/except ImportError` conditional ones (even with `--unsafe-fixes`) — so Task 3 splits into an autofix (Step 2) + a precise manual removal (Step 3), not a single autofix. (2) `input_analysis` is partially dead — `INPUT_FILE_CATEGORIES` is still used, so its block is EDITED (3 names dropped, 1 kept), not deleted; the other 12 leaf blocks are fully dead and deleted wholesale. (3) The test suite does NOT import `shiny_app.app`, so a wrongly-removed used import passes the suite and only surfaces at `import shiny_app.app`/`create_ui()` — the import+render check (Step 5) is the mandatory backstop, empirically confirmed during review (a whole-block deletion of `input_analysis` gave `NameError` at import while the suite stayed 178-green). Tasks 1 (build_config) and 2 (ui_panels) are low-risk single-purpose removals, both grep-verified during review (build_config has zero readers; ui_panels has no code importers).
