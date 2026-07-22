# Loadable setup registry (CL29 + Standard) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Add a general setup registry to the Shiny UI so one selection loads a complete model config; ship Standard (25-box) + CL29 (29-box) so CL29 runs correctly/safely and the primary views reflect it.

**Architecture:** New pure `shiny_app/setups.py` registry → published as a `@reactive.calc` `run.current_setup` on the existing `RunController` contract → consumers read `state.run.current_setup()` inside reactive bodies. Drives input file (filtered dropdown), run env (`ESTAS_HOLD_VOLUME=1`, merged before `Popen` on both run paths), box-count selectors (via `update_*` effects), and the primary output/inputs dirs. Secondary viewers + the map are guarded with a notice; scenarios are disabled under non-standard setups.

**Tech Stack:** Python 3, Shiny for Python (modules, `reactive.calc`/`reactive.effect`, `ui.update_*`), pytest, ruff.

## Global Constraints

- **Env merge is additive and late.** `run_env.update(env_extra or {})` must run **after** the `run_env = compiler_env.get_run_environment()` reassignments and **immediately before** `subprocess.Popen`. Never merge into `os.environ` or the initial copy; never *replace* `run_env` (that drops Intel `LD_LIBRARY_PATH`).
- **Both run paths get the env fix:** `RunController.start_run` (via `run_control.py`) AND the dashboard Quick Run (`dashboard.py handle_quick_run._work`).
- **Reactives can't be read in worker threads.** Capture `current_setup().env` and `current_setup().input_file` as **plain values at run-start**, pass as thread/function args.
- **Box selectors are static `@module.ui`** — never read a reactive there. Change choices via a `reactive.effect` that calls `ui.update_*` (the `init_cmd_dropdowns` pattern), keyed on `current_setup`.
- **`is_available` checks a required file** (`inputs_dir/PELAGIC_INPUTS.txt`), not non-emptiness.
- **`cmd_input_file` is filtered to the setup's compatible files** (those declaring `setup.inputs_dir`) — no manual desync possible.
- **Scenarios are disabled when a non-standard setup is active** (they read+write `INPUTS/`).
- **Backward-compatible:** `default_setup()` is Standard (`box_count=25`, `env={}`, `INPUT.txt`, `INPUTS/`, `OUTPUTS/`) → no-interaction behavior is byte-identical.
- `env_extra=None` default (no mutable default arg).

---

### Task 1: The registry — `shiny_app/setups.py` (pure) + tests

**Files:**
- Create: `shiny_app/setups.py`
- Test: `tests/python/test_setups.py`

**Interfaces (Produces):** `Setup` dataclass (fields below); `SETUPS: list[Setup]`; `list_setups() -> list[Setup]`; `get_setup(id) -> Setup` (unknown → `default_setup()`); `default_setup() -> Setup` (the `standard` entry); `is_available(setup, root) -> bool`; `input_files_for(setup, root) -> list[str]` (repo-root `INPUT*.txt` whose declared `PELAGIC_INPUT_FOLDER` == `setup.inputs_dir + "/"`).

- [ ] **Step 1: Write the failing test**

```python
# tests/python/test_setups.py
import os
from pathlib import Path
import sys

TOOLS = Path(__file__).resolve().parents[2] / "shiny_app"
sys.path.insert(0, str(TOOLS))
import setups as s  # noqa: E402


def test_two_entries_with_expected_fields():
    ids = [x.id for x in s.list_setups()]
    assert ids == ["standard", "cl29"]
    std, cl29 = s.get_setup("standard"), s.get_setup("cl29")
    assert (std.input_file, std.inputs_dir, std.output_dir, std.box_count, std.env) == \
           ("INPUT.txt", "INPUTS", "OUTPUTS", 25, {})
    assert (cl29.input_file, cl29.inputs_dir, cl29.output_dir, cl29.box_count) == \
           ("INPUT_CL29.txt", "INPUTS_CL29", "OUTPUTS_CL29", 29)
    assert cl29.env == {"ESTAS_HOLD_VOLUME": "1"}


def test_unknown_id_falls_back_to_default():
    assert s.get_setup("nope").id == "standard"
    assert s.default_setup().id == "standard"


def test_is_available_requires_the_sentinel_file(tmp_path):
    root = tmp_path
    (root / "INPUTS").mkdir()
    assert s.is_available(s.get_setup("standard"), str(root)) is False   # no PELAGIC_INPUTS.txt
    (root / "INPUTS" / "WCONST.txt").write_text("x")                     # decoy: non-empty, still unavailable
    assert s.is_available(s.get_setup("standard"), str(root)) is False   # kills a non-emptiness impl
    (root / "INPUTS" / "PELAGIC_INPUTS.txt").write_text("x")
    assert s.is_available(s.get_setup("standard"), str(root)) is True
    assert s.is_available(s.get_setup("cl29"), str(root)) is False        # no INPUTS_CL29/


def test_input_files_for_matches_real_comment_format(tmp_path):
    root = tmp_path
    hdr = '# PELAGIC MODEL INPUT FOLDER write the folder always "/" in the end\n'   # real trailing text
    (root / "INPUT.txt").write_text(hdr + "INPUTS/\n")
    (root / "INPUT_CL29.txt").write_text(hdr + "INPUTS_CL29/\n")
    (root / "INPUT_30day.txt").write_text(hdr + "INPUTS/\n")
    std = s.input_files_for(s.get_setup("standard"), str(root))
    assert set(std) == {"INPUT.txt", "INPUT_30day.txt"}
    assert s.input_files_for(s.get_setup("cl29"), str(root)) == ["INPUT_CL29.txt"]


def test_input_files_for_against_real_repo():
    repo = str(Path(__file__).resolve().parents[2])
    std = s.input_files_for(s.get_setup("standard"), repo)
    assert "INPUT.txt" in std and "INPUT_30day.txt" in std   # real Standard configs stay visible
    assert "INPUT_CL29.txt" not in std                        # CL29 config excluded from Standard
    cl29 = s.input_files_for(s.get_setup("cl29"), repo)
    assert "INPUT_CL29.txt" in cl29
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m pytest tests/python/test_setups.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'setups'`.

- [ ] **Step 3: Write `shiny_app/setups.py`**

```python
"""Model-application setup registry for the Shiny UI.

A Setup bundles a complete model configuration (input file + inputs/outputs dirs +
box count + run env). The registry is the single source of truth so box count,
directories, and env become data rather than hardcoded constants.
"""
from __future__ import annotations

import os
from dataclasses import dataclass, field


@dataclass(frozen=True)
class Setup:
    id: str
    name: str
    description: str
    input_file: str        # default repo-root config
    inputs_dir: str
    output_dir: str
    box_count: int
    env: dict = field(default_factory=dict)
    required_input: str = "PELAGIC_INPUTS.txt"   # availability sentinel (ESTAS reads it first)
    unavailable_hint: str = ""


SETUPS = [
    Setup("standard", "Standard (25-box)",
          "The default AQUABC pelagic configuration (committed INPUTS/).",
          "INPUT.txt", "INPUTS", "OUTPUTS", 25),
    Setup("cl29", "CL29 — Curonian Lagoon (29-box)",
          "EUTROPY-derived 29-box Curonian Lagoon; requires ESTAS_HOLD_VOLUME=1.",
          "INPUT_CL29.txt", "INPUTS_CL29", "OUTPUTS_CL29", 29,
          env={"ESTAS_HOLD_VOLUME": "1"},
          unavailable_hint="Generate inputs: python tools/eutropy_poc/eutropy_to_estas.py"),
]

_BY_ID = {s.id: s for s in SETUPS}


def list_setups():
    return list(SETUPS)


def default_setup():
    return SETUPS[0]


def get_setup(setup_id):
    return _BY_ID.get(setup_id, default_setup())


def is_available(setup, root):
    return os.path.isfile(os.path.join(root, setup.inputs_dir, setup.required_input))


def _declared_input_folder(path):
    """Return the PELAGIC_INPUT_FOLDER value (folder name, no trailing slash) or ''."""
    try:
        with open(path) as fh:
            lines = fh.read().splitlines()
        for i, line in enumerate(lines):
            if "PELAGIC MODEL INPUT FOLDER" in line:
                for nxt in lines[i + 1:]:
                    if nxt.strip():
                        return nxt.strip().rstrip("/")
    except OSError:
        pass
    return ""


def input_files_for(setup, root):
    """Repo-root INPUT*.txt files whose declared input folder matches setup.inputs_dir."""
    out = []
    for f in sorted(os.listdir(root)):
        if f.startswith("INPUT") and f.endswith(".txt"):
            if _declared_input_folder(os.path.join(root, f)) == setup.inputs_dir:
                out.append(f)
    return out
```

- [ ] **Step 4: Run tests + lint**

Run: `python -m pytest tests/python/test_setups.py -q` → 5 passed.
Run: `ruff check shiny_app/setups.py tests/python/test_setups.py` → clean.

- [ ] **Step 5: Commit**

```bash
git add shiny_app/setups.py tests/python/test_setups.py
git commit -m "feat(shiny): setup registry (Standard + CL29) — setups.py"
```

---

### Task 2: Setup selector + `run.current_setup` contract + availability + filtered config

**Files:**
- Modify: `shiny_app/app_state.py` (RunController placeholder)
- Modify: `shiny_app/modules/run_control.py` (selector UI + `run.current_setup` + availability + filter `cmd_input_file`)

**Interfaces:**
- Consumes: `setups.{list_setups,get_setup,default_setup,is_available,input_files_for}` (Task 1).
- Produces: `run.current_setup` — a `@reactive.calc` returning the selected `Setup`; every consumer reads `state.run.current_setup()`.

- [ ] **Step 1: RunController placeholder defaults to Standard**

Add a **module-level** dual import at the top of `app_state.py` (match its existing try/except import style — do not put a bare packaged import inside `__init__`, it breaks when the app runs as a script from inside `shiny_app/`):
```python
try:
    from shiny_app.setups import default_setup as _default_setup
except ImportError:
    from setups import default_setup as _default_setup
```
Then in `RunController.__init__`, next to `self.command_config = None`, add:
```python
        # current_setup: () -> Setup ; degrades to Standard until run_control assigns the reactive
        self.current_setup = lambda: _default_setup()
```

- [ ] **Step 2: Add the setup selector to the Run Control UI**

In `run_control.py run_control_ui()`, insert at the TOP of the tab's controls (above the executable/config-file controls) a select input:

```python
                ui.input_select("setup_select", "Setup:",
                                choices={s.id: s.name for s in setups.list_setups()},
                                selected="standard"),
                ui.output_ui("setup_availability"),
```
Add `from shiny_app import setups` (or `import setups`) at the top matching the module's import style.

- [ ] **Step 3: Publish `run.current_setup` + availability + filter the config dropdown**

In `run_control_server`, after `run.command_config = _command_config` (line ~331), add:

```python
    @reactive.calc
    def _current_setup():
        return setups.get_setup(input.setup_select() or "standard")
    run.current_setup = _current_setup

    @render.ui
    def setup_availability():
        st = _current_setup()
        if setups.is_available(st, ROOT):
            return ui.TagList()
        return ui.div(ui.tags.small(f"⚠ Inputs for “{st.name}” not found. {st.unavailable_hint}"),
                      class_="text-warning")

    @reactive.effect
    def _sync_setup_to_config():
        st = _current_setup()
        files = setups.input_files_for(st, ROOT) or [st.input_file]
        ui.update_select("cmd_input_file",
                         choices={f: (f + " (default)" if f == st.input_file else f) for f in files},
                         selected=st.input_file)
```
This replaces the unconditional `cmd_input_file` population in `init_cmd_dropdowns` (line ~268-272) for the input-file part — keep `init_cmd_dropdowns` for constants/shear, but its `cmd_input_file` block is now driven by `_sync_setup_to_config` (remove the duplicate `update_select("cmd_input_file", …)` at ~272 to avoid two writers).

- [ ] **Step 4: Verify UI still renders**

Run: `python -c "import sys; sys.path.insert(0,'shiny_app'); import app; app.create_ui().tagify()" ` (from repo root) → no exception.
Run: `ruff check shiny_app/modules/run_control.py shiny_app/app_state.py` → clean.

- [ ] **Step 5: Commit**

```bash
git add shiny_app/modules/run_control.py shiny_app/app_state.py
git commit -m "feat(shiny): setup selector + run.current_setup contract + availability + filtered config"
```

---

### Task 3: Env injection + progress tracker on BOTH run paths

Makes CL29 run without the ~day-449 crash. **This is the correctness core.**

**Files:**
- Modify: `shiny_app/app_state.py` (`start_run` signature + merge + tracker)
- Modify: `shiny_app/modules/run_control.py` (thread args)
- Modify: `shiny_app/modules/dashboard.py` (`handle_quick_run._work` merge + tracker)
- Test: `tests/python/test_start_run_env.py`

- [ ] **Step 1: Write the failing test (env merge survives the reassignment)**

```python
# tests/python/test_start_run_env.py
import sys, types
from pathlib import Path
from unittest import mock

APP = Path(__file__).resolve().parents[2] / "shiny_app"
sys.path.insert(0, str(APP))
import app_state  # noqa: E402


def test_env_extra_is_in_popen_env_after_get_run_environment(monkeypatch):
    captured = {}

    class FakePopen:
        def __init__(self, *a, **kw):
            captured["env"] = kw.get("env")
            self.stdout = None
            self.returncode = 0
        def poll(self): return 0
        def wait(self): return 0

    monkeypatch.setattr(app_state.subprocess, "Popen", FakePopen)
    monkeypatch.setattr(app_state.compiler_env, "is_intel_executable", lambda n: False)
    monkeypatch.setattr(app_state.compiler_env, "get_run_environment",
                        lambda: {"PATH": "/x"})  # reassigns run_env, no HOLD_VOLUME
    rc = app_state.RunController(root=str(APP))
    rc.start_run(["./ESTAS_II", "INPUT_CL29.txt"], "ESTAS_II",
                 env_extra={"ESTAS_HOLD_VOLUME": "1"})
    assert captured["env"].get("ESTAS_HOLD_VOLUME") == "1"   # survived the reassignment


def test_env_extra_none_is_noop(monkeypatch):
    captured = {}

    class FakePopen:
        def __init__(self, *a, **kw):
            captured["env"] = kw.get("env"); self.stdout = None; self.returncode = 0
        def poll(self): return 0
        def wait(self): return 0
    monkeypatch.setattr(app_state.subprocess, "Popen", FakePopen)
    monkeypatch.setattr(app_state.compiler_env, "is_intel_executable", lambda n: False)
    monkeypatch.setattr(app_state.compiler_env, "get_run_environment", lambda: {"PATH": "/x"})
    rc = app_state.RunController(root=str(APP))
    rc.start_run(["./ESTAS_II"], "ESTAS_II")
    assert "ESTAS_HOLD_VOLUME" not in captured["env"]
```
(Confirm `RunController.__init__` accepts `root=`; if the ctor differs, adapt construction — read `app_state.py:32-52`.)

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m pytest tests/python/test_start_run_env.py -q`
Expected: FAIL — `start_run() got an unexpected keyword argument 'env_extra'`.

- [ ] **Step 3: `start_run` — add params, merge before Popen, setup-aware tracker**

In `app_state.py`, change the signature (line 119):
```python
    def start_run(self, estas_cmd, exe_name, env_extra=None, input_file=None):
```
After the `if/else` that reassigns `run_env` (i.e. after line 137, before the `logger.info(f"Executing…")` at 138), insert:
```python
            if env_extra:
                run_env.update(env_extra)   # additive, after get_run_environment(), before Popen
```
Compute a tracker path once near the top of `start_run` (after line 122):
```python
        input_txt = os.path.join(self.root, input_file) if input_file else None
```
Change both `output_data.get_output_files_info()` calls (lines 164, 189) to:
```python
            output_info = output_data.get_output_files_info(input_txt_path=input_txt) if input_txt \
                else output_data.get_output_files_info()
```
(same for `final_output_info` at 189).

- [ ] **Step 4: `run_control` guards availability + passes the captured setup env/input to the thread**

In `run_control.py` `on_run` (the handler that starts the thread at ~530), before constructing the thread, capture the setup as a plain value, **guard availability** (spec §5: unavailable selection is prevented), then extend the thread `args`:
```python
        st = run.current_setup()
        if not setups.is_available(st, ROOT):
            run.run_log_lines.append(f"⚠ Inputs for “{st.name}” not found. {st.unavailable_hint}\n")
            return
        # ... existing command build ...
        threading.Thread(
            target=run.start_run, args=(estas_cmd, exe_name, dict(st.env), st.input_file),
            daemon=True, name="RunThread").start()
```
(Read lines 525-533 for the exact existing call; keep its `daemon=True` and the existing `name="RunThread"` — do not rename the thread.)

- [ ] **Step 5: Dashboard Quick Run — env merge only**

In `dashboard.py handle_quick_run`, capture `cur = run.current_setup()` as a plain value **before** `def _work` (so the closure captures the value, not a reactive read inside the thread). Inside `_work`, after the `run_env = get_run_environment()` reassignments (~324/330) and **before** `subprocess.Popen` (~333), insert — and this is the ONLY change here:
```python
                if cur.env:
                    run_env.update(cur.env)
```
Do **not** add output-folder/tracker plumbing: `_work` tracks progress via `get_csv_info()` on a fixed `OUTPUT_CSV`, not an output folder, so there is nothing to make setup-aware (and `OUTPUT.csv` is a Non-goal). Also guard availability at the top of `handle_quick_run` (before building the command): `cur = run.current_setup(); if not setups.is_available(cur, ROOT): run.run_log_lines.append(f"⚠ … {cur.unavailable_hint}\n"); return`.

- [ ] **Step 6: Run tests + lint**

Run: `python -m pytest tests/python/test_start_run_env.py tests/python/test_setups.py -q` → all pass.
Run: `ruff check shiny_app/app_state.py shiny_app/modules/run_control.py shiny_app/modules/dashboard.py` → clean.

- [ ] **Step 7: Commit**

```bash
git add shiny_app/app_state.py shiny_app/modules/run_control.py shiny_app/modules/dashboard.py tests/python/test_start_run_env.py
git commit -m "feat(shiny): inject setup env (ESTAS_HOLD_VOLUME) on both run paths + setup-aware progress"
```

---

### Task 4: Box-count geometry — selectors + output-config writer

**Files:**
- Modify: `shiny_app/modules/run_control.py` (box-selector effect + writer loop/path)
- Modify: `shiny_app/modules/input_files.py` (box-selector effect + copy string)
- Modify: `shiny_app/modules/plot.py` (box cap line 745)

- [ ] **Step 1: The output-box selector follows `current_setup` (preserving selection)**

The `output_boxes` checkbox group at `run_control.py:198` is built statically with a **committed default** `selected=["5","6","8","9","14","17","25"]` (`:199`). Add in `run_control_server` a `reactive.effect` keyed on `run.current_setup()`. **Critical:** an `update_checkbox_group` with no `selected` clears every box — on the first Standard flush that would wipe the committed 7-box default (a user who Saves Output Config without re-picking then writes 0 boxes), violating the byte-identical Global Constraint. Preserve the current selection, reading it under `reactive.isolate` so re-selecting is not a self-dependency:
```python
    @reactive.effect
    def _sync_output_boxes_to_setup():
        n = run.current_setup().box_count
        with reactive.isolate():
            keep = [b for b in (input.output_boxes() or []) if b.isdigit() and int(b) <= n]
        ui.update_checkbox_group("output_boxes",
                                 choices={str(i): f"Box {i}" for i in range(1, n + 1)},
                                 selected=keep)
```
**Do NOT touch `input_files.py:95` (`map_bathymetry_box`)** — it feeds the box-network map, a **deferred/guarded 25-box surface** (spec Non-goals). Driving it to 29 would offer boxes the 25-box map can't render; leave it at 25.

- [ ] **Step 2: Output-config writer uses box_count + the setup's inputs dir**

In `run_control.py save_output_config()` (~618): change `for box in range(1, 26):` (line 631) to `for box in range(1, run.current_setup().box_count + 1):`. Change the `OUTPUT_INFO_FILE` path (defined ~562 as `os.path.join(ROOT, "INPUTS", "PELAGIC_OUTPUT_INFORMATION_FILE.txt")`) so both the save (~643) and load (~570) resolve it under `run.current_setup().inputs_dir` instead of the literal `"INPUTS"` — compute it inside the handlers: `os.path.join(ROOT, run.current_setup().inputs_dir, "PELAGIC_OUTPUT_INFORMATION_FILE.txt")`.

- [ ] **Step 3: Plot box cap**

In `plot.py:745`, change `range(1, min(num_vars + 1, 26))` → `range(1, min(num_vars + 1, state.run.current_setup().box_count + 1))` (read `plot_server`'s state access; bind `run = state.run` if not already).

- [ ] **Step 4: Copy string**

`input_files.py:400` "all 25 boxes" is in the map-display surface (deferred/guarded) — make it generic ("all boxes") rather than box_count-derived.

- [ ] **Step 5: Verify + commit**

Run: `python -c "import sys; sys.path.insert(0,'shiny_app'); import app; app.create_ui().tagify()"` → no exception.
Run: `ruff check shiny_app/modules/run_control.py shiny_app/modules/input_files.py shiny_app/modules/plot.py` → clean.
```bash
git add shiny_app/modules/run_control.py shiny_app/modules/input_files.py shiny_app/modules/plot.py
git commit -m "feat(shiny): box selectors + output-config writer follow the setup's box_count"
```

---

### Task 5: Primary results setup-aware — output dir + input browser

**Files:**
- Modify: `shiny_app/modules/dashboard.py`, `shiny_app/modules/plot.py`, `shiny_app/diagnostics.py` (output-dir dropdowns seed from `current_setup.output_dir`)
- Modify: `shiny_app/modules/input_files.py` (input browser reads `current_setup.inputs_dir`)

- [ ] **Step 1: Output-dir dropdowns follow the setup**

Add a `reactive.effect` keyed **only on `current_setup`** (never reading the dropdown's own value — that self-triggers and fights the user) that `ui.update_select`s the dropdown's selected value to `current_setup().output_dir`, once per setup change:
- `plot.py` `init_output_dirs` (~440-466): select `state.run.current_setup().output_dir` (note **`sim_output_dir`** is the id written cross-namespace via `session=rc` at ~466 — preserve that; `output_dir_select` is plot's own id).
- `diagnostics.py` (`diag_output_dir` default `"OUTPUTS"`, ~108/392/408): add the effect selecting `current_setup().output_dir`. `diagnostics_server` already receives `state` (confirmed).
- `dashboard.py`: the system-status / input-variable cards read `INPUT.txt` (path ~:577) and the `INPUTS` literal (~:645) — point those at `current_setup().input_file` (ROOT-joined) and `current_setup().inputs_dir` so the cards reflect CL29. (Dashboard has **no** output-folder card — do not invent one.)

Each effect guards on availability (auto-select only if the dir exists) and seeds on setup change only — do NOT read `input.output_dir_select()`/`input.diag_output_dir()` inside it (self-dependency).

- [ ] **Step 2: Input browser reads the setup's inputs dir**

In `input_files.py`, the file browser reads `INPUTS_DIR` (a module constant `os.path.join(ROOT, 'INPUTS')`). Route the browse root through `state.run.current_setup().inputs_dir` (compute `os.path.join(ROOT, current_setup().inputs_dir)` inside the reactive that lists/loads files — lines ~137, ~202 and the category filter). Keep the box-network map read (which the guard task handles) separate.

- [ ] **Step 3: Verify + commit**

Run: `python -c "import sys; sys.path.insert(0,'shiny_app'); import app; app.create_ui().tagify()"` → no exception. Run ruff on the four files.
```bash
git add shiny_app/modules/dashboard.py shiny_app/modules/plot.py shiny_app/diagnostics.py shiny_app/modules/input_files.py
git commit -m "feat(shiny): primary results + input browser resolve dirs from the current setup"
```

---

### Task 6: Guards + notices for the deferred surface

**Files:**
- Modify: `shiny_app/modules/scenarios.py` (disable apply under non-standard)
- Modify: `shiny_app/modules/parameters.py`, `initial_conditions.py`, `model_options.py`, `input_files.py` (map-display notice)
- Modify: copy strings `box_network.py:475`, `diagnostics.py:240` (static → generic)

- [ ] **Step 1: Disable scenario *apply* under a non-standard setup**

`scenarios.py` overwrites `INPUTS/` only via **`load_selected_scenario`** (the Load button → `apply_scenario`, which rewrites WCONST/ICs/options in `INPUTS/`). `save_new_scenario`/`delete_scenario` write to `shiny_app/scenarios/` (setup-independent, safe). Guard: in `scenarios_server`, when `state.run.current_setup().id != "standard"`, render a notice ("Scenario editing is available for the Standard model only.") and **early-return in `load_selected_scenario`** (defensively also in `save_new_scenario`/`delete_scenario`) with a log line — so no scenario apply can overwrite `INPUTS/` while CL29 is active.

- [ ] **Step 2: Secondary-viewer notice**

In `parameters.py`, `initial_conditions.py`, `model_options.py`, and the **Map Display** render in `input_files.py` (`map_display_info` — the map lives here; `box_network.py`/`map.py` are pure helpers with no reactive context, so the notice cannot go there), when `current_setup().id != "standard"`, prepend a one-line notice: "Showing Standard-model reference data; the CL29-specific view is not yet wired." (These keep reading `INPUTS/` / the 25-box `BOX_GEOM` — the notice makes the limitation honest.) All four servers already receive `state`.

- [ ] **Step 3: Copy strings**

`box_network.py:475` title "(25 Pelagic Boxes)" and `diagnostics.py:240` "25 boxes" are in **static (non-reactive) contexts** — they cannot read `current_setup()`. Make them generic ("Pelagic Boxes" / drop the count) rather than box_count-derived. Low priority.

- [ ] **Step 4: Verify + commit**

Run: `python -c "import sys; sys.path.insert(0,'shiny_app'); import app; app.create_ui().tagify()"` → no exception. Run ruff on the changed files.
```bash
git add -A
git commit -m "feat(shiny): guard scenarios + notice secondary viewers/map under non-standard setups"
```

---

### Task 7: Backstop, full sweep, PR

- [ ] **Step 1: UI backstop test**

Add (or confirm) `tests/python/test_ui_renders.py` asserting `import shiny_app.app; shiny_app.app.create_ui().tagify()` succeeds (the suite does not import app.py otherwise). Run it.

- [ ] **Step 2: Full python suite + lint**

Run: `python -m pytest tests/python/ -q` → all pass.
Run: `ruff check shiny_app/ tests/python/` → clean (fix any new findings in changed lines; pre-existing debt out of scope).

- [ ] **Step 3: Manual smoke (documented, not automated)**

If `INPUTS_CL29/` is present: launch the app, select **CL29**, confirm the config dropdown shows `INPUT_CL29.txt`, box selectors show 1–29, scenarios shows the disabled notice; run a short CL29 sim and confirm the log shows `OUTPUTS_CL29/` and `ESTAS_HOLD_VOLUME` behavior (no day-449 crash for a >449-day run). Note results in the PR. If `INPUTS_CL29/` absent, confirm CL29 shows the unavailable hint.

- [ ] **Step 4: PR**

```bash
git push -u origin feat/loadable-setup-registry
gh pr create --base main --head feat/loadable-setup-registry  # --body-file (backtick-safe)
```
Body: the registry; Tier-1 (CL29 runs correctly, both paths, box-count, writer); Tier-2 (primary results/inputs); guards (scenarios disabled, notices); the documented deferrals; test + smoke results.

- [ ] **Step 5: Poll CI, merge on green** per the established pattern (python-lint-test + integration-tests; checks-rollup guard for CodeRabbit).

---

## Notes for the executor

- **Import style:** `shiny_app/` files use a try/except dual import (`from shiny_app.X import …` / `from X import …`). Match the file you edit; `setups` must import the same way (add to both branches if a file uses the pattern).
- **Reactive vs thread:** anything read inside a `threading.Thread` target or `_work` closure must be captured as a plain value first (setups, ids, dirs). The env/input-file capture in Task 3 is the template.
- **Don't touch `OUTPUT.csv` readers** (`observations.py`, `mass_balance.py`, plot CSV-preview) — a separate fixed artifact, out of scope (Non-goal).
- **Quick-Run pre-validation** (`validate_required_inputs`, `dashboard.py:205`) reads `INPUTS/BATHYMETRY_{1..25}` — under CL29 it validates the Standard `INPUTS/` (whose 1..25 exist), so it passes but isn't CL29-specific. Acceptable for this tier — the Task-3 unavailable-setup run-guard covers the genuinely-missing-inputs case; full CL29 pre-validation is deferred.
- Tiers are ordered so the branch is runnable-correct after Task 3 and visually-correct after Task 5; guards (Task 6) prevent wrong/destructive secondary behavior.
