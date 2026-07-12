# app.py Decomposition — Phase 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Extract three clusters of non-reactive, module-level helper functions out of the 8,616-line `shiny_app/app.py` into focused, unit-testable modules — without touching the reactive graph.

**Architecture:** Move each cluster (functions + the consts they *exclusively* use) into a new `shiny_app/` module; `app.py` re-imports the still-referenced names via the existing fallback pattern. `server()`/`create_ui()` bodies are untouched. Design + dependency analysis: `docs/superpowers/specs/2026-07-12-app-py-decomposition-design.md`.

**Tech Stack:** Python 3, Shiny for Python, pytest, ruff.

## Global Constraints

- **Move functions VERBATIM** — no logic edits during the move (behavior-preserving refactor).
- **Each new module** starts with `import logging` + `logger = logging.getLogger("AQUABC")` (same named logger as app.py) and, where needed, `ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))` and `INPUTS_DIR = os.path.join(ROOT, 'INPUTS')`.
- **`app.py` re-imports** every moved name that is still referenced outside the new module, via the established fallback:
  ```python
  try:
      from shiny_app.<mod> import <names>
  except ImportError:
      from <mod> import <names>
  ```
- **Remove** the moved defs/consts from `app.py` after adding the re-import.
- **Do NOT touch** `create_ui()` (L1117+) or `server()` (L2687+) bodies, nor any `.f90`.
- **Consts that STAY in app.py:** `COMPILERS`, `BUILD_TYPES`, `NAV_CHOICES` (UI consts, not used by moved fns).
- **Per-phase gate (all must pass before the phase is done):**
  1. `python -m py_compile shiny_app/app.py shiny_app/<mod>.py`
  2. `python -c "from shiny_app.<mod> import <all moved public names>"` succeeds
  3. `ruff check shiny_app/app.py --select F821` → **All checks passed** (catches a missed re-import as an undefined name; app.py is F821-clean at baseline)
  4. `pytest tests/python/ -q` → full suite green (was **107 passed**), including the new unit-test file
  5. New `tests/python/test_<mod>.py` unit tests pass
  (Playwright/Selenium integration tests are the end-to-end safety net; they run in CI on merge — Playwright is not installed locally.)

---

### Task 1: `shiny_app/compiler_env.py` — Intel/compiler detection

**Files:**
- Create: `shiny_app/compiler_env.py`
- Modify: `shiny_app/app.py` (remove moved defs + `INTEL_COMPILER_SEARCH_PATHS`; add re-import)
- Test: `tests/python/test_compiler_env.py`

**Move (verbatim) from app.py:** functions `find_compiler_path`, `is_intel_executable`, `get_intel_library_paths`, `check_intel_libs_available`, `get_run_environment`, `get_intel_setvars_path`, `build_intel_wrapped_command`; and the constant `INTEL_COMPILER_SEARCH_PATHS` (used only by `find_compiler_path`).

**`app.py` re-imports (all 7 functions; NOT the const):**
```python
try:
    from shiny_app.compiler_env import (
        find_compiler_path, is_intel_executable, get_intel_library_paths,
        check_intel_libs_available, get_run_environment, get_intel_setvars_path,
        build_intel_wrapped_command,
    )
except ImportError:
    from compiler_env import (
        find_compiler_path, is_intel_executable, get_intel_library_paths,
        check_intel_libs_available, get_run_environment, get_intel_setvars_path,
        build_intel_wrapped_command,
    )
```

- [ ] **Step 1: Write the unit test** (`tests/python/test_compiler_env.py`)

```python
"""Tests for shiny_app.compiler_env (extracted from app.py)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "shiny_app", "compiler_env.py")
_spec = importlib.util.spec_from_file_location("compiler_env", _PATH)
compiler_env = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(compiler_env)


def test_module_exposes_all_functions():
    for name in ("find_compiler_path", "is_intel_executable", "get_intel_library_paths",
                 "check_intel_libs_available", "get_run_environment",
                 "get_intel_setvars_path", "build_intel_wrapped_command"):
        assert callable(getattr(compiler_env, name)), name

def test_search_paths_present():
    assert isinstance(compiler_env.INTEL_COMPILER_SEARCH_PATHS, list)

def test_is_intel_executable_gfortran_false():
    # gfortran is not an Intel executable
    assert compiler_env.is_intel_executable("gfortran") is False

def test_build_intel_wrapped_command_returns_list():
    out = compiler_env.build_intel_wrapped_command(["echo", "hi"])
    assert isinstance(out, list)
```

- [ ] **Step 2: Run it — expect FAIL** (module doesn't exist yet)

Run: `pytest tests/python/test_compiler_env.py -q` → FAIL (file not found / import error).

- [ ] **Step 3: Create `shiny_app/compiler_env.py`**

Header + move the 7 functions and `INTEL_COMPILER_SEARCH_PATHS` verbatim from app.py:
```python
"""Intel/compiler detection and run-environment helpers, extracted from app.py."""
import logging
import os
import shutil
import subprocess

logger = logging.getLogger("AQUABC")

# INTEL_COMPILER_SEARCH_PATHS = [ ... ]   # moved verbatim from app.py
# def find_compiler_path(...): ...        # 7 functions moved verbatim
```
(Copy the exact bodies from app.py; keep imports the functions actually use — check each for `os`/`shutil`/`subprocess`/`glob` etc. and import accordingly.)

- [ ] **Step 4: Edit `app.py`** — delete the 7 moved function defs and the `INTEL_COMPILER_SEARCH_PATHS` assignment; add the re-import block (Task header) near the other `try/except` imports (after the `safe_resolve` import).

- [ ] **Step 5: Run the per-phase gate**

```bash
python -m py_compile shiny_app/app.py shiny_app/compiler_env.py
python -c "from shiny_app.compiler_env import find_compiler_path, is_intel_executable, get_intel_library_paths, check_intel_libs_available, get_run_environment, get_intel_setvars_path, build_intel_wrapped_command; print('imports ok')"
ruff check shiny_app/app.py --select F821
pytest tests/python/ -q
```
Expected: py_compile silent, "imports ok", ruff "All checks passed", pytest all green.

- [ ] **Step 6: Commit**

```bash
git add shiny_app/compiler_env.py shiny_app/app.py tests/python/test_compiler_env.py
git commit -m "refactor(shiny): extract compiler_env.py from app.py (TODO 2.1 phase 1)"
```

---

### Task 2: `shiny_app/input_analysis.py` — input-file analysis

**Files:**
- Create: `shiny_app/input_analysis.py`
- Modify: `shiny_app/app.py`
- Test: `tests/python/test_input_analysis.py`

**Move (verbatim) from app.py:** functions `analyze_input_file`, `get_input_file_categories`, `validate_required_inputs`; consts `INPUT_FILE_CATEGORIES` (the `INPUT_FILE_CATEGORIES = {...}` assignment **and** its separate `INPUT_FILE_CATEGORIES.update({...})` statement — they are non-contiguous; other unrelated code sits between them and must STAY in app.py), `REQUIRED_INPUT_FILES`, `RECOMMENDED_INPUT_FILES`. Module self-computes `INPUTS_DIR`.

**`app.py` re-imports (3 functions + `INPUT_FILE_CATEGORIES`; NOT `REQUIRED/RECOMMENDED_INPUT_FILES`):**
```python
try:
    from shiny_app.input_analysis import (
        analyze_input_file, get_input_file_categories, validate_required_inputs,
        INPUT_FILE_CATEGORIES,
    )
except ImportError:
    from input_analysis import (
        analyze_input_file, get_input_file_categories, validate_required_inputs,
        INPUT_FILE_CATEGORIES,
    )
```

- [ ] **Step 1: Write the unit test** (`tests/python/test_input_analysis.py`)

```python
"""Tests for shiny_app.input_analysis (extracted from app.py)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "shiny_app", "input_analysis.py")
_spec = importlib.util.spec_from_file_location("input_analysis", _PATH)
ia = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(ia)


def test_functions_present():
    for name in ("analyze_input_file", "get_input_file_categories", "validate_required_inputs"):
        assert callable(getattr(ia, name)), name

def test_categories_dict_includes_base_and_update():
    cats = ia.INPUT_FILE_CATEGORIES
    assert isinstance(cats, dict) and cats
    # the .update() adds EXTRA_WCONST.txt — proves both base + update moved
    assert "EXTRA_WCONST.txt" in cats

def test_get_input_file_categories_returns_the_dict():
    assert ia.get_input_file_categories() is ia.INPUT_FILE_CATEGORIES or \
           ia.get_input_file_categories() == ia.INPUT_FILE_CATEGORIES
```

- [ ] **Step 2: Run it — expect FAIL.** `pytest tests/python/test_input_analysis.py -q`

- [ ] **Step 3: Create `shiny_app/input_analysis.py`** — header (`logging` + `logger`, `os`, self-compute `ROOT`/`INPUTS_DIR`), then move verbatim: `INPUT_FILE_CATEGORIES` base assignment, its `.update({...})` block, `REQUIRED_INPUT_FILES`, `RECOMMENDED_INPUT_FILES`, and the 3 functions. Import whatever the functions use (`os`, `re`, etc.).

- [ ] **Step 4: Edit `app.py`** — delete the moved base def + `.update()` statement + the two `*_INPUT_FILES` consts + the 3 function defs (leave the unrelated code between the base def and update); add the re-import block.

- [ ] **Step 5: Run the per-phase gate** (as Task 1, with `input_analysis` and the imported names `analyze_input_file, get_input_file_categories, validate_required_inputs, INPUT_FILE_CATEGORIES`).

- [ ] **Step 6: Commit** — `refactor(shiny): extract input_analysis.py from app.py (TODO 2.1 phase 1)`

---

### Task 3: `shiny_app/file_locators.py` — output/box file discovery

**Files:**
- Create: `shiny_app/file_locators.py`
- Modify: `shiny_app/app.py`
- Test: `tests/python/test_file_locators.py`

**Move (verbatim) from app.py:** functions `get_output_folder`, `find_pelagic_box_file`, `get_available_boxes`, `get_timeseries_variables`. Module self-computes `ROOT`/`INPUTS_DIR` and defines `logger`. (`find_pelagic_box_file` calls `get_output_folder` — both in this module, so no cross-module call.)

**`app.py` re-imports (all 4 functions):**
```python
try:
    from shiny_app.file_locators import (
        get_output_folder, find_pelagic_box_file, get_available_boxes, get_timeseries_variables,
    )
except ImportError:
    from file_locators import (
        get_output_folder, find_pelagic_box_file, get_available_boxes, get_timeseries_variables,
    )
```

- [ ] **Step 1: Write the unit test** (`tests/python/test_file_locators.py`)

```python
"""Tests for shiny_app.file_locators (extracted from app.py)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "shiny_app", "file_locators.py")
_spec = importlib.util.spec_from_file_location("file_locators", _PATH)
fl = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(fl)


def test_functions_present():
    for name in ("get_output_folder", "find_pelagic_box_file",
                 "get_available_boxes", "get_timeseries_variables"):
        assert callable(getattr(fl, name)), name

def test_root_and_inputs_dir_resolve():
    assert fl.ROOT.endswith(os.sep + "AQUABCv0.2") or os.path.isdir(fl.ROOT)
    assert fl.INPUTS_DIR.endswith("INPUTS")

def test_get_output_folder_returns_str():
    assert isinstance(fl.get_output_folder(), str)
```

- [ ] **Step 2: Run it — expect FAIL.** `pytest tests/python/test_file_locators.py -q`

- [ ] **Step 3: Create `shiny_app/file_locators.py`** — header (`logging`+`logger`, `os`, `ROOT`, `INPUTS_DIR`) + move the 4 functions verbatim (import what they use, e.g. `safe_resolve` if referenced — check and import from `shiny_app.safe_resolve`).

- [ ] **Step 4: Edit `app.py`** — delete the 4 moved function defs; add the re-import block.

- [ ] **Step 5: Run the per-phase gate** (with `file_locators` and the 4 imported names).

- [ ] **Step 6: Commit** — `refactor(shiny): extract file_locators.py from app.py (TODO 2.1 phase 1)`

---

## Self-review (controller)

After all three tasks: confirm `app.py` line count dropped by ~500+, `ruff check shiny_app/app.py --select F821` is clean, the full suite is green with 3 new test files, and `TODO_IMPLEMENTATION_PLAN.md` 2.1 is annotated with the phase-1 progress (and the deferred UI/server roadmap). The Playwright integration tests are the merge-time safety net (CI).
