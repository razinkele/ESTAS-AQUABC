# Shiny-modules Rearchitecture — Phase 5 (final cleanup) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close out the `shiny_app/app.py` Shiny-modules rearchitecture — remove the dead weight left after all 15 tabs became modules (an unread contract value, an empty stub file, ~70 dead imports), and mark the rearchitecture complete.

**Architecture:** Pure cleanup — no new modules, no behavior change. Three small removals (the unread `run.build_config` contract leg; the empty `ui_panels.py`; app.py's dead imports + trivial lint), then a docs/CHANGELOG capstone and a regression gate. Every task is behavior-neutral and verified by `import shiny_app.app` + the full suite.

**Tech Stack:** Python 3.10, Shiny for Python 1.5.1, pytest, ruff.

## Global Constraints

*(Every task's requirements implicitly include this section.)*

- **Behavior-neutral.** Nothing a user sees changes. No id renames, no handler edits, no logic changes. Only removal of provably-dead code + lint hygiene.
- **Use `.venv/bin/python`** for anything importing `app.py` (system `python3` lacks `networkx`).
- **Verify every removal** with `.venv/bin/python -c "import shiny_app.app"` (clean) + `.venv/bin/python -m pytest tests/python/ -q` (full suite green). A removal that breaks either is wrong — revert it.
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

**Files:**
- Modify: `shiny_app/app.py`, `pyproject.toml`

- [ ] **Step 1: Snapshot the dead-import list (for the diff review).**
```bash
cd /home/razinka/AQUABCv0.2
ruff check shiny_app/app.py --select F401,F541 --isolated 2>&1 | tee /tmp/app_lint_before.txt | tail -5
echo "F401 count:"; grep -c F401 /tmp/app_lint_before.txt
```
Expected: ~70 F401 + 6 F541. These are leaf-module names (`ParameterFile`, `ICFile`, `SimulationConfigFile`, …), stdlib (`threading`, `shutil`, `time`, `select`, `signal`, `traceback`, `re`, `shlex`, `date`, `timedelta`), and viz/widget imports (`numpy`, `plotly.express`, `plotly.graph_objects`, `networkx`, `shiny.req`, `output_widget`, `render_widget`) — all orphaned by moved handlers.

- [ ] **Step 2: Autofix F401 + F541, then MANUALLY review the diff.**
```bash
ruff check shiny_app/app.py --select F401,F541 --fix --isolated 2>&1 | tail -3
git diff --stat shiny_app/app.py
```
**Critical review of the diff:** the imports use a `try: from shiny_app.X import … except ImportError: from X import …` fallback pattern. Confirm the autofix removed the dead name from **both** branches (or the whole `try/except` block if all its names were dead) and did NOT leave a half-empty `import ()` or an orphaned `except ImportError:` with nothing in it. If a `try`/`except` block is now malformed (empty import parens, dangling `except`), fix it by hand. **Do NOT let it remove a name that is still used** — the suite + import checks in Step 4 are the backstop, but eyeball the removed names against the Step-1 list (all should be leaf-parser/stdlib/viz names no longer referenced).

- [ ] **Step 3: Drop the `F401` per-file-ignore for app.py.** In `pyproject.toml`, remove the `"F401",` line (and its comment) from the `"shiny_app/app.py"` per-file-ignores block. **Keep** `F841`, `B023`, `E402`, `S602`, `S605` (all still needed — verified: 7 E402 + 1 F841 remain and are legitimate). The block header comment can stay.

- [ ] **Step 4: Verify — import, suite, and that app.py is now F401-clean under the real config.**
```bash
.venv/bin/python -c "import shiny_app.app; from shiny_app.app import create_ui; str(create_ui().tagify()); print('import + render OK')"
.venv/bin/python -m pytest tests/python/ -q
ruff check shiny_app/app.py 2>&1 | grep -E "F401|F541" && echo "!!! F401/F541 remain" || echo "clean — app.py F401/F541-free under project config"
ruff check shiny_app/app.py 2>&1 | tail -3   # should show only the still-ignored E402/F841 are suppressed → expect "All checks passed" or only non-F401 findings
ruff check shiny_app/modules/ tests/python/
```
Expected: import + render OK; full suite green (same count — no test imported the dead names); `ruff check shiny_app/app.py` (config-aware, F401 no longer ignored) reports **no F401/F541** (E402/F841 stay suppressed by the remaining ignores); modules/tests clean.

- [ ] **Step 5: Commit.**
```bash
git add shiny_app/app.py pyproject.toml
git commit -m "refactor(shiny): remove 70 dead imports from app.py + drop F401 ignore

Imports orphaned as inline handlers moved into modules (modules import their
own leaf parsers). Also fixes 6 F541. app.py is now F401/F541-clean; the
F401 per-file-ignore is removed (E402/F841/B023/S602/S605 stay — structural)."
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

**Risk:** the app.py autofix (Task 3) is the only non-trivial step — the try/except ImportError fallback blocks must not be left malformed. Mitigated by the manual diff review (Step 2) + the import+render+suite verification (Step 4). Everything else is a single-file deletion or a small targeted edit.
