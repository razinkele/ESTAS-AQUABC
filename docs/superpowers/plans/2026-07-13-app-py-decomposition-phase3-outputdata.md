# app.py Decomposition Phase 3 — Output-Data Cluster — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the seven pure output-file helpers from `shiny_app/app.py`'s `server()` into a new module `shiny_app/output_data.py`; update their 11 external call sites; delete the nested defs. Observable behavior unchanged.

**Architecture:** `output_data.py` self-computes `ROOT`/`OUTPUT_CSV`/`INPUT_TXT_PATH`, imports `PELAGIC_BOX_COLUMNS` (from `utils`) + `SimulationConfigFile` (from `simulation_config`), and exposes 7 functions using a default-arg pattern (`root=ROOT`, etc.) so call sites stay arg-free while tests inject a `tmp_path`. Spec: `docs/superpowers/specs/2026-07-13-app-py-decomposition-phase3-outputdata-design.md`.

**Tech Stack:** Python 3.10+, Shiny, pandas, pytest, ruff.

## Global Constraints

- **Behavior-preserving, not byte-identical.** Verified by unit tests + CI Playwright.
- **The 7 functions' bodies move with these deliberate edits ONLY:** (a) drop the leading underscore from `_looks_numeric`/`_get_output_columns`; (b) add the default-arg const params to each signature; (c) inside each body, the closed-over consts become the params — `ROOT`→`root`, `OUTPUT_CSV`→`output_csv`, `INPUT_TXT_PATH`→`input_txt_path`; (d) intra-cluster calls: `_looks_numeric(...)`→`looks_numeric(...)`, and `get_output_folder_from_config()`→`get_output_folder_from_config(input_txt_path=input_txt_path)` inside `get_output_files_info`. No other logic change. (The dead local `extensions` in `get_output_files_from_dir` moves verbatim — do not "fix" it.)
- **`output_data.py` imports:** `os`, `logging`, `pandas as pd`; leaf imports `PELAGIC_BOX_COLUMNS`/`SimulationConfigFile` via the `try/except ImportError` fallback. No `shiny`, no `app`.
- **Module consts before functions** so `def f(root=ROOT)` binds at import.
- **Module-import form** in `app.py`: `try: from shiny_app import output_data / except ImportError: import output_data`; call `output_data.<fn>(...)`.
- **`INPUT_TXT_PATH` at app.py:2573 STAYS** (used by `analyze_output_directory` + config blocks — only the extracted function's use leaves).
- **Gate (after each task):** `py_compile app.py`; `python -c "import shiny_app.output_data"`; `ruff check --select F821 shiny_app/app.py shiny_app/output_data.py`; `pytest tests/python -q` green. Use `.venv/bin/python` + `/home/razinka/.local/bin/ruff`. Playwright CI-only.

---

### Task 1: Create `shiny_app/output_data.py` + unit tests

**Files:**
- Create: `shiny_app/output_data.py`
- Create: `tests/python/test_output_data.py`

**Interfaces:** produces `looks_numeric`, `format_elapsed`, `get_output_folder_from_config`, `get_output_files_info`, `get_output_columns`, `get_output_directories`, `get_output_files_from_dir`. `app.py` untouched this task.

- [ ] **Step 1: Write the failing tests** — `tests/python/test_output_data.py`:

```python
import os
from shiny_app.output_data import (
    looks_numeric, format_elapsed, get_output_folder_from_config,
    get_output_files_info, get_output_columns, get_output_directories,
    get_output_files_from_dir,
)
try:
    from shiny_app.utils import PELAGIC_BOX_COLUMNS
except ImportError:
    from utils import PELAGIC_BOX_COLUMNS


def test_looks_numeric():
    assert looks_numeric("3.5") and looks_numeric("10") and looks_numeric("-2e3")
    assert not looks_numeric("abc")
    assert not looks_numeric("")
    assert not looks_numeric(None)


def test_format_elapsed():
    assert format_elapsed(3661) == "1h 1m 1s"
    assert format_elapsed(61) == "1m 1s"
    assert format_elapsed(5) == "5s"


def test_get_output_folder_from_config_missing(tmp_path):
    assert get_output_folder_from_config(input_txt_path=str(tmp_path / "nope.txt")) == "OUTPUTS"


def test_get_output_files_info_missing(tmp_path):
    info = get_output_files_info(root=str(tmp_path), input_txt_path=str(tmp_path / "nope.txt"))
    assert info["exists"] is False and info["folder"] == "OUTPUTS"


def test_get_output_directories(tmp_path):
    (tmp_path / "OUTPUTS_a").mkdir()
    (tmp_path / "stray.txt").write_text("x")
    dirs = get_output_directories(root=str(tmp_path), output_csv=str(tmp_path / "none.csv"))
    assert dirs == {"OUTPUTS_a": "OUTPUTS_a"}          # OUTPUTS* dirs only; no ROOT key (csv absent)


def test_get_output_files_from_dir_text_and_binary(tmp_path):
    sub = tmp_path / "SUB"; sub.mkdir()
    (sub / "PELAGIC_BOX_01.out").write_text("x")       # matches text filter
    (sub / "PELAGIC_BOX_PROCESS_RATES.out").write_text("x")   # excluded (PROCESS_RATES)
    (sub / "PELAGIC_BOX_01.bin").write_text("x")       # matches binary filter
    assert get_output_files_from_dir("SUB", "text", root=str(tmp_path)) == {"PELAGIC_BOX_01.out": "PELAGIC_BOX_01.out"}
    assert get_output_files_from_dir("SUB", "binary", root=str(tmp_path)) == {"PELAGIC_BOX_01.bin": "PELAGIC_BOX_01.bin"}
    assert get_output_files_from_dir("NOPE", "text", root=str(tmp_path)) == {}


def test_get_output_columns_csv_and_binary(tmp_path):
    csv = tmp_path / "out.csv"
    csv.write_text("TIME_DAYS,DIA_C,CYN_C\n1,2,3\n")
    assert get_output_columns(file_path=str(csv), file_format="csv") == ["TIME_DAYS", "DIA_C", "CYN_C"]
    assert get_output_columns(file_path="x.bin", file_format="binary") == PELAGIC_BOX_COLUMNS
```

- [ ] **Step 2: Run tests — verify they fail**

Run: `.venv/bin/python -m pytest tests/python/test_output_data.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'shiny_app.output_data'`

- [ ] **Step 3: Create `shiny_app/output_data.py`** with exactly this content:

```python
"""Pure output-file helpers (extracted from server())."""
import os
import logging
import pandas as pd

try:
    from shiny_app.utils import PELAGIC_BOX_COLUMNS
    from shiny_app.simulation_config import SimulationConfigFile
except ImportError:
    from utils import PELAGIC_BOX_COLUMNS
    from simulation_config import SimulationConfigFile

logger = logging.getLogger("AQUABC")

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))
OUTPUT_CSV = os.path.join(ROOT, 'OUTPUT.csv')
INPUT_TXT_PATH = os.path.join(ROOT, 'INPUT.txt')


def looks_numeric(s: str) -> bool:
    """Return True if string looks like a number (int or float)."""
    try:
        float(s)
        return True
    except (ValueError, TypeError):
        return False


def format_elapsed(seconds):
    """Format elapsed time as HH:MM:SS"""
    hours = int(seconds // 3600)
    minutes = int((seconds % 3600) // 60)
    secs = int(seconds % 60)
    if hours > 0:
        return f"{hours}h {minutes}m {secs}s"
    elif minutes > 0:
        return f"{minutes}m {secs}s"
    else:
        return f"{secs}s"


def get_output_folder_from_config(input_txt_path=INPUT_TXT_PATH):
    """Get output folder from INPUT.txt configuration"""
    try:
        if os.path.exists(input_txt_path):
            scf = SimulationConfigFile(input_txt_path)
            if scf.parse():
                return scf.config.output_folder.rstrip('/')
    except Exception as e:
        logger.warning(f"Could not read output folder from INPUT.txt: {e}")
    return "OUTPUTS"  # fallback


def get_output_files_info(root=ROOT, input_txt_path=INPUT_TXT_PATH):
    """Get info about output files in the configured output folder for progress tracking"""
    try:
        output_folder = get_output_folder_from_config(input_txt_path=input_txt_path)
        output_dir = os.path.join(root, output_folder)

        if not os.path.isdir(output_dir):
            return {"exists": False, "size_kb": 0, "file_count": 0, "folder": output_folder}

        total_size = 0
        file_count = 0
        out_files = 0
        bin_files = 0

        for fname in os.listdir(output_dir):
            fpath = os.path.join(output_dir, fname)
            if os.path.isfile(fpath):
                try:
                    total_size += os.path.getsize(fpath)
                    file_count += 1
                    if fname.endswith('.out'):
                        out_files += 1
                    elif fname.endswith('.bin'):
                        bin_files += 1
                except OSError:
                    pass

        return {
            "exists": True,
            "size_kb": total_size / 1024,
            "file_count": file_count,
            "out_files": out_files,
            "bin_files": bin_files,
            "folder": output_folder
        }
    except Exception as e:
        logger.debug(f"Error getting output info: {e}")
    return {"exists": False, "size_kb": 0, "file_count": 0, "folder": "OUTPUTS"}


def get_output_columns(file_path=None, file_format=None, output_csv=OUTPUT_CSV):
    """Get column names from an output file."""
    target_path = file_path or output_csv

    # Auto-detect format
    if file_format is None:
        if target_path.endswith('.bin'):
            file_format = 'binary'
        elif target_path.endswith('.out'):
            file_format = 'text'
        else:
            file_format = 'csv'

    try:
        if file_format == 'binary':
            # Binary files use fixed column names
            return PELAGIC_BOX_COLUMNS
        elif file_format == 'text':
            # Read header from .out file
            df = pd.read_csv(target_path, sep=r'\s+', nrows=0)
            cols = [c.strip() for c in df.columns]
            # Sanity check: if the first column name looks numeric, the file
            # has no header (e.g. PROCESS_RATES).  A proper header always
            # starts with a string like TIME_DAYS.
            if cols and looks_numeric(cols[0]):
                logger.warning(f"File appears headerless (numeric column names): {os.path.basename(target_path)}")
                if len(cols) == len(PELAGIC_BOX_COLUMNS):
                    return list(PELAGIC_BOX_COLUMNS)
                return [f"V{i}" for i in range(len(cols))]
            return cols
        else:
            # Read header from CSV
            df = pd.read_csv(target_path, comment='#', skip_blank_lines=True, nrows=0)
            return [c.strip() for c in df.columns]
    except Exception as e:
        logger.error(f"Error reading output file header: {e}")
        return []


def get_output_directories(root=ROOT, output_csv=OUTPUT_CSV):
    """Get list of output directories in the workspace"""
    dirs = {}
    # Add root OUTPUT.csv as option
    if os.path.exists(output_csv):
        dirs["ROOT"] = "OUTPUT.csv (root directory)"

    # Find OUTPUTS_* directories
    for item in os.listdir(root):
        if item.startswith("OUTPUTS") and os.path.isdir(os.path.join(root, item)):
            dirs[item] = item
    return dirs


def get_output_files_from_dir(dir_name, file_format="text", root=ROOT):
    """Get list of output files from the selected directory based on format.

    Args:
        dir_name: Directory name (relative to ROOT) or "ROOT"
        file_format: 'text' for .out, 'binary' for .bin, 'csv' for .csv

    Returns:
        dict: {filename: display_name} for UI choices
    """
    files = {}

    if not dir_name:
        return files

    if dir_name == "ROOT":
        dir_path = root
    else:
        dir_path = os.path.join(root, dir_name)

    if not os.path.isdir(dir_path):
        return files

    # Determine file extensions based on format
    if file_format == "binary":
        extensions = [".bin"]
        # For binary, prefer PELAGIC_BOX files
        for f in sorted(os.listdir(dir_path)):
            if f.endswith(".bin") and "PELAGIC_BOX" in f and "PROCESS_RATES" not in f:
                files[f] = f
    elif file_format == "csv":
        extensions = [".csv"]
        for f in sorted(os.listdir(dir_path)):
            if f.endswith(".csv") and os.path.isfile(os.path.join(dir_path, f)):
                files[f] = f
    else:  # text (.out)
        extensions = [".out"]
        for f in sorted(os.listdir(dir_path)):
            if (f.endswith(".out") and "PELAGIC_BOX" in f
                    and "PROCESS_RATES" not in f
                    and os.path.isfile(os.path.join(dir_path, f))):
                files[f] = f

    return files
```

- [ ] **Step 4: Run tests — verify they pass**

Run: `.venv/bin/python -m pytest tests/python/test_output_data.py -q`
Expected: PASS (7 tests). If `get_output_columns` binary test fails on `PELAGIC_BOX_COLUMNS` identity, ensure the import in the test and module both resolve to `utils.PELAGIC_BOX_COLUMNS`.

- [ ] **Step 5: Gate + commit**

```bash
.venv/bin/python -c "import shiny_app.output_data"
/home/razinka/.local/bin/ruff check --select F821 shiny_app/output_data.py
.venv/bin/python -m pytest tests/python -q     # 148 baseline + new, no regressions
git add shiny_app/output_data.py tests/python/test_output_data.py
git commit -m "feat(shiny): add output_data module (pure output-file helpers) + tests (phase 3, task 1)"
```

---

### Task 2: Wire `server()` — delete nested defs, update call sites

**Files:**
- Modify: `shiny_app/app.py` (add re-import; update 11 call sites; delete 7 nested defs)
- Modify: `TODO_IMPLEMENTATION_PLAN.md`

- [ ] **Step 1: Add the re-import** after the `box_network` re-import block in `app.py`:

```python
try:
    from shiny_app import output_data
except ImportError:
    import output_data
```

- [ ] **Step 2: Update the 11 external call sites** (do this BEFORE deleting the defs; grep after to confirm zero remaining bare references). Each keeps its varying args; consts come from the defaults:

| Line | Was | Becomes |
|---|---|---|
| 4199, 4233, 4238 | `format_elapsed(<x>)` | `output_data.format_elapsed(<x>)` |
| 4183, 4224 | `get_output_files_info()` | `output_data.get_output_files_info()` |
| 3825 | `_get_output_columns(<args>)` | `output_data.get_output_columns(<args>)` |
| 4754, 4784, 4793 | `get_output_directories()` | `output_data.get_output_directories()` |
| 5059, 5076 | `get_output_files_from_dir(<args>)` | `output_data.get_output_files_from_dir(<args>)` |

(Note the rename `_get_output_columns` → `output_data.get_output_columns` at 3825. The two intra-cluster call sites — `_looks_numeric`@3790 and `get_output_folder_from_config`@3994 — are inside the defs being deleted, so they vanish with them.)

- [ ] **Step 3: Delete the 7 nested defs.** After Step 2, grep to confirm no bare references remain: `grep -nE "\b(_looks_numeric|format_elapsed|get_output_folder_from_config|get_output_files_info|_get_output_columns|get_output_directories|get_output_files_from_dir)\(" shiny_app/app.py` should show ONLY `output_data.<fn>` calls (zero bare `def`s or bare calls). Then delete each of the 7 `def` blocks:
  - `_looks_numeric` (3804–3810), `format_elapsed` (4030–4040), `get_output_folder_from_config` (3980–3989), `get_output_files_info` (3991–4028), `_get_output_columns` (3766–3802), `get_output_directories` (4664–4675), `get_output_files_from_dir` (4996–5039).
  These are NON-contiguous (scattered through server()), so delete each block individually and re-verify the surrounding decorators/functions stay intact. Do NOT delete the `server()`-local `INPUT_TXT_PATH = os.path.join(ROOT, "INPUT.txt")` at 2573 (other code uses it). After deleting, re-grep to confirm zero bare `def _looks_numeric`/etc. remain and the 7 names appear ONLY as `output_data.<fn>` calls.

- [ ] **Step 4: Gate**

```bash
.venv/bin/python -m py_compile shiny_app/app.py
.venv/bin/python -c "import shiny_app.output_data"
/home/razinka/.local/bin/ruff check --select F821 shiny_app/app.py shiny_app/output_data.py
.venv/bin/python -m pytest tests/python -q
```
Expected: all pass; F821 clean; suite green (no new tests here).

- [ ] **Step 5: Update `TODO_IMPLEMENTATION_PLAN.md`** — mark the output-data cluster done; note remaining phase-3 work (mass-balance/observations/scenarios likely-thin, the reactive CSV cache + run_command/analyze_output_directory, inline cmd-logic ~628–667, `_execute_build_process`, box_network lint PR) as deferred.

- [ ] **Step 6: Commit**

```bash
git add shiny_app/app.py TODO_IMPLEMENTATION_PLAN.md
git commit -m "refactor(shiny): output handlers delegate to output_data; drop 7 nested defs (phase 3, task 2)"
```

---

## Final verification (after both tasks)

- [ ] `pytest tests/python -q` green; `ruff check --select F821 shiny_app/app.py shiny_app/output_data.py` clean.
- [ ] Grep: the 7 names appear ONLY as `output_data.<fn>` calls (11 sites); zero nested defs remain; `INPUT_TXT_PATH` (2573) untouched.
- [ ] Manual equivalence: each body moved with only the documented edits (underscore-drop, default-arg params, const→param inside body, intra-call fixes); no logic change.
- [ ] Broad whole-branch review before merge.

## Notes

All 7 functions are reproduced above (not verbatim-copied) because each carries the const→param edit; the plan makes every edit explicit so the implementer doesn't have to infer which references change. The reviewer should diff each reproduced body against the pre-branch original (app.py) to confirm the ONLY differences are the documented edits.
