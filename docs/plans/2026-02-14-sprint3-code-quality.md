# Sprint 3: Code Quality Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Improve code quality via pre-commit hooks, business logic unit tests, build/rebuild deduplication, and Playwright integration tests in CI.

**Architecture:** Four independent tasks — pre-commit setup, extract+test business logic from app.py, refactor duplicated build handlers, and add a CI integration test job. Each commits separately.

**Tech Stack:** Python 3 (pytest, ruff, pre-commit, playwright), Shiny for Python, GitHub Actions

---

## Pre-flight: Verify Baseline

**Step 1:** `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
Expected: All tests PASSED

**Step 2:** `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py --ignore=tests/python/test_safe_resolve.py`
Expected: 37 passed

**Step 3:** `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
Expected: ESTAS_II created

---

## Task 1: Add Pre-commit Hooks (P2)

Add `.pre-commit-config.yaml` with ruff linting/formatting, trailing whitespace, and end-of-file fixers. Update dev dependencies and CONTRIBUTING.md.

**Files:**
- Create: `.pre-commit-config.yaml`
- Modify: `requirements-dev.txt`
- Modify: `CONTRIBUTING.md`

### Step 1: Create .pre-commit-config.yaml

```yaml
repos:
  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v5.0.0
    hooks:
      - id: trailing-whitespace
      - id: end-of-file-fixer
      - id: check-yaml
      - id: check-added-large-files
        args: ['--maxkb=500']

  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.9.7
    hooks:
      - id: ruff
        args: [--fix]
      - id: ruff-format
```

### Step 2: Add pre-commit to dev dependencies

In `requirements-dev.txt`, add:
```
pre-commit>=4.0
```

### Step 3: Update CONTRIBUTING.md

Find the section about linting (around line 62-66) and add pre-commit setup instructions after it:

```markdown
### Pre-commit Hooks (Recommended)

Install pre-commit hooks to automatically lint before each commit:

```bash
pip install pre-commit
pre-commit install
```

This runs ruff and basic file checks on every `git commit`. To run manually on all files:

```bash
pre-commit run --all-files
```
```

### Step 4: Verify pre-commit works locally

Run: `cd /home/razinka/AQUABCv0.2 && pip install pre-commit && pre-commit run --all-files`
Expected: All hooks pass (or show auto-fixes for trailing whitespace)

If trailing whitespace fixes are applied, stage and include them in the commit.

### Step 5: Commit

```bash
git add .pre-commit-config.yaml requirements-dev.txt CONTRIBUTING.md
# Also add any files auto-fixed by pre-commit
git commit -m "chore: add pre-commit hooks for ruff linting and file hygiene

Add .pre-commit-config.yaml with ruff linter/formatter, trailing
whitespace, end-of-file, YAML check, and large file detection.
Update dev dependencies and CONTRIBUTING.md with setup instructions."
```

---

## Task 2: Extract and Test Business Logic from app.py (P1)

Extract 3 high-value standalone functions from `shiny_app/app.py` into a new utility module and add comprehensive unit tests. These functions have zero test coverage today.

**Functions to extract:**
1. `count_file_lines_fast()` (lines 136-173) — Efficient line counting with sampling for large files
2. `validate_constants_file()` (lines 415-465) — Validates WCONST constant files have 318 entries
3. `read_pelagic_binary()` + `read_pelagic_text()` (lines 235-286) — Model output parsers

**Files:**
- Create: `shiny_app/utils.py`
- Modify: `shiny_app/app.py` (replace functions with imports)
- Create: `tests/python/test_utils.py`

### Step 1: Read the functions from app.py

Read `shiny_app/app.py` lines 136-173 (`count_file_lines_fast`), 235-286 (`read_pelagic_binary`, `read_pelagic_text`), and 415-465 (`validate_constants_file`) to get the exact code.

### Step 2: Create shiny_app/utils.py

Create the new module with the 4 functions copied from app.py. Keep the exact same signatures and logic. Add necessary imports at the top:

```python
"""Utility functions extracted from app.py for testability."""

import os
import re
import struct

import numpy as np
import pandas as pd


def count_file_lines_fast(filepath):
    """Count lines in a file. For files >1MB, uses sampling to estimate."""
    # ... exact code from app.py lines 136-173 ...


def read_pelagic_binary(filepath, ncols=37, max_rows=None):
    """Read Fortran stream binary output into a DataFrame."""
    # ... exact code from app.py lines 235-260 ...


def read_pelagic_text(filepath, max_rows=None):
    """Read whitespace-separated text output into a DataFrame."""
    # ... exact code from app.py lines 262-286 ...


def validate_constants_file(filepath):
    """Validate that a WCONST file has exactly 318 model constants.

    Returns (is_valid, message) tuple.
    """
    # ... exact code from app.py lines 415-465 ...
```

### Step 3: Update app.py to import from utils

Replace the function bodies in app.py with imports:

At the top of app.py (near other shiny_app imports), add:
```python
from shiny_app.utils import count_file_lines_fast, read_pelagic_binary, read_pelagic_text, validate_constants_file
```

Then remove or comment out the original function definitions (lines 136-173, 235-286, 415-465). Replace each with a comment like:
```python
# count_file_lines_fast — moved to shiny_app/utils.py
```

**IMPORTANT:** Do NOT remove any other code. Only the 4 function bodies. Verify the imports and usages still work.

### Step 4: Write unit tests

Create `tests/python/test_utils.py`:

```python
"""Tests for shiny_app/utils.py — extracted business logic functions."""

import os
import struct
import tempfile

import numpy as np
import pandas as pd
import pytest

from shiny_app.utils import (
    count_file_lines_fast,
    read_pelagic_binary,
    read_pelagic_text,
    validate_constants_file,
)


class TestCountFileLinesfast:
    """Tests for count_file_lines_fast()."""

    def test_small_file(self, tmp_path):
        f = tmp_path / "small.txt"
        f.write_text("line1\nline2\nline3\n")
        assert count_file_lines_fast(str(f)) == 3

    def test_empty_file(self, tmp_path):
        f = tmp_path / "empty.txt"
        f.write_text("")
        assert count_file_lines_fast(str(f)) == 0

    def test_single_line_no_newline(self, tmp_path):
        f = tmp_path / "single.txt"
        f.write_text("hello")
        result = count_file_lines_fast(str(f))
        assert result >= 1  # implementation may count as 0 or 1

    def test_nonexistent_file(self):
        result = count_file_lines_fast("/nonexistent/file.txt")
        assert result == 0  # should handle gracefully


class TestValidateConstantsFile:
    """Tests for validate_constants_file()."""

    def _make_constants_file(self, tmp_path, n_constants):
        """Helper: create a WCONST-style file with n constant entries."""
        f = tmp_path / "WCONST_test.txt"
        lines = []
        for i in range(1, n_constants + 1):
            lines.append(f"  {i}  1.0000D+00  ! constant_{i}")
        f.write_text("\n".join(lines) + "\n")
        return str(f)

    def test_valid_318_constants(self, tmp_path):
        path = self._make_constants_file(tmp_path, 318)
        is_valid, msg = validate_constants_file(path)
        assert is_valid is True

    def test_too_few_constants(self, tmp_path):
        path = self._make_constants_file(tmp_path, 100)
        is_valid, msg = validate_constants_file(path)
        assert is_valid is False
        assert "100" in msg or "318" in msg

    def test_nonexistent_file(self):
        is_valid, msg = validate_constants_file("/nonexistent/WCONST.txt")
        assert is_valid is False

    def test_empty_file(self, tmp_path):
        f = tmp_path / "empty.txt"
        f.write_text("")
        is_valid, msg = validate_constants_file(str(f))
        assert is_valid is False


class TestReadPelagicBinary:
    """Tests for read_pelagic_binary()."""

    def _make_binary_file(self, tmp_path, nrows, ncols=37):
        """Helper: create a binary file with float64 data."""
        f = tmp_path / "output.bin"
        data = np.arange(nrows * ncols, dtype=np.float64).reshape(nrows, ncols)
        with open(f, "wb") as fh:
            fh.write(data.tobytes())
        return str(f), data

    def test_read_valid_binary(self, tmp_path):
        path, expected = self._make_binary_file(tmp_path, 10)
        df = read_pelagic_binary(path, ncols=37)
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 10
        assert df.shape[1] == 37

    def test_read_with_max_rows(self, tmp_path):
        path, _ = self._make_binary_file(tmp_path, 100)
        df = read_pelagic_binary(path, ncols=37, max_rows=5)
        assert len(df) <= 5

    def test_empty_binary_file(self, tmp_path):
        f = tmp_path / "empty.bin"
        f.write_bytes(b"")
        df = read_pelagic_binary(str(f), ncols=37)
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 0

    def test_incomplete_record(self, tmp_path):
        """Binary file with partial last record should not crash."""
        f = tmp_path / "partial.bin"
        # Write 1.5 records (37 + 18 doubles)
        data = np.ones(55, dtype=np.float64)
        f.write_bytes(data.tobytes())
        df = read_pelagic_binary(str(f), ncols=37)
        assert len(df) == 1  # partial record dropped


class TestReadPelagicText:
    """Tests for read_pelagic_text()."""

    def test_read_valid_text(self, tmp_path):
        f = tmp_path / "output.csv"
        lines = ["1.0 2.0 3.0\n", "4.0 5.0 6.0\n", "7.0 8.0 9.0\n"]
        f.write_text("".join(lines))
        df = read_pelagic_text(str(f))
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 3
        assert df.shape[1] == 3

    def test_read_with_max_rows(self, tmp_path):
        f = tmp_path / "output.csv"
        lines = [f"{i}.0 {i+1}.0\n" for i in range(100)]
        f.write_text("".join(lines))
        df = read_pelagic_text(str(f), max_rows=10)
        assert len(df) <= 10

    def test_empty_text_file(self, tmp_path):
        f = tmp_path / "empty.csv"
        f.write_text("")
        df = read_pelagic_text(str(f))
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 0
```

### Step 5: Run tests

Run: `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/test_utils.py -v`
Expected: All tests pass (some may need adjustment based on actual function signatures/return values)

Also verify existing tests still pass:
Run: `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py --ignore=tests/python/test_safe_resolve.py`
Expected: 37 + new tests all pass

### Step 6: Commit

```bash
git add shiny_app/utils.py shiny_app/app.py tests/python/test_utils.py
git commit -m "refactor: extract business logic from app.py into utils.py with tests

Extract count_file_lines_fast, validate_constants_file, read_pelagic_binary,
and read_pelagic_text into shiny_app/utils.py. Add 16 unit tests covering
normal operation, edge cases, and error handling. App.py imports from utils."
```

---

## Task 3: Deduplicate Build/Rebuild Handlers (P2)

Extract shared build logic from `on_build()` (lines 3491-3587) and `on_rebuild()` (lines 3591-3684) in app.py into a shared helper function.

**Files:**
- Modify: `shiny_app/app.py`

### Step 1: Read both handlers

Read `shiny_app/app.py` lines 3491-3700 to get both `on_build()` and `on_rebuild()` handlers.

### Step 2: Create shared helper function

Add a new function `_execute_build_process()` immediately before `on_build()` (around line 3489). This function captures the ~60 lines of duplicated subprocess logic from the inner `_do_build()` and `_do_rebuild()` functions:

```python
def _execute_build_process(compiler, build_type, exe_name, clean_first,
                           action_name, log_output):
    """Shared build/rebuild subprocess logic.

    Args:
        compiler: Compiler path string
        build_type: Build type string (release/debug/fast)
        exe_name: Executable name
        clean_first: Whether to run make clean-all before building
        action_name: Display name for UI messages ("Build" or "Rebuild")
        log_output: Reactive value for log output
    """
    import subprocess
    import time

    log_lines = []

    def append_log(line):
        log_lines.append(line)
        log_output.set("\n".join(log_lines))

    append_log(f"=== {action_name} Started ===")
    append_log(f"Compiler: {compiler}")
    append_log(f"Build type: {build_type}")
    append_log("")

    try:
        # Clean step (conditional)
        if clean_first:
            append_log(f"Cleaning previous build artifacts...")
            proc = subprocess.Popen(
                ["make", "clean-all"],
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                cwd=app_dir,
            )
            for line in proc.stdout:
                append_log(line.rstrip())
            proc.wait()
            if proc.returncode != 0:
                append_log(f"Clean failed (exit code {proc.returncode})")
                ui.notification_show(f"{action_name} failed during clean", type="error")
                return
            append_log("Clean complete.\n")

        # Build step
        append_log(f"{action_name}ing library and executable...")
        build_cmd = ["make", f"FC={compiler}", f"BUILD_TYPE={build_type}",
                     f"EXE_NAME={exe_name}", "build-estas"]
        proc = subprocess.Popen(
            build_cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            cwd=app_dir,
        )
        for line in proc.stdout:
            append_log(line.rstrip())
        proc.wait()

        if proc.returncode == 0:
            append_log(f"\n=== {action_name} Completed Successfully ===")
            ui.notification_show(f"{action_name} completed successfully!", type="message")
        else:
            append_log(f"\n=== {action_name} Failed (exit code {proc.returncode}) ===")
            ui.notification_show(f"{action_name} failed (exit code {proc.returncode})", type="error")

    except Exception as e:
        append_log(f"\n{action_name} error: {e}")
        ui.notification_show(f"{action_name} error: {e}", type="error")
```

**NOTE:** The exact code will need to match the actual patterns in app.py. Read the handlers carefully and adapt. The key principle is: everything that differs between build/rebuild becomes a parameter.

### Step 3: Simplify on_build() and on_rebuild()

Replace the inner `_do_build()` function body with a call to `_execute_build_process()`:

```python
@reactive.effect
@reactive.event(input.btn_build)
def on_build():
    compiler = input.build_compiler()
    build_type = input.build_type()
    exe_name = input.exe_name()
    clean_first = input.build_clean_first()

    # Verify compiler exists
    # ... keep the compiler validation ...

    def _do_build():
        _execute_build_process(compiler, build_type, exe_name,
                               clean_first, "Build", build_log)

    threading.Thread(target=_do_build, name="BuildThread", daemon=True).start()
```

Similarly for `on_rebuild()`:

```python
@reactive.effect
@reactive.event(input.btn_rebuild)
def on_rebuild():
    compiler = input.build_compiler()
    build_type = input.build_type()
    exe_name = input.exe_name()

    # Verify compiler exists
    # ... keep the compiler validation ...

    def _do_rebuild():
        _execute_build_process(compiler, build_type, exe_name,
                               True, "Rebuild", build_log)  # always clean

    threading.Thread(target=_do_rebuild, name="RebuildThread", daemon=True).start()
```

### Step 4: Verify Python tests still pass

Run: `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py --ignore=tests/python/test_safe_resolve.py`
Expected: All tests pass (no functional change)

Also verify ruff passes:
Run: `cd /home/razinka/AQUABCv0.2 && ruff check shiny_app/app.py`
Expected: No new errors

### Step 5: Commit

```bash
git add shiny_app/app.py
git commit -m "refactor: deduplicate build/rebuild handlers into shared helper

Extract ~120 lines of duplicated subprocess logic from on_build() and
on_rebuild() into _execute_build_process(). Both handlers now delegate
to the shared function with action_name and clean_first parameters."
```

---

## Task 4: Add Playwright Integration Tests to CI (P1)

Add a new CI job that installs Playwright, starts the Shiny app, and runs the 19 Playwright integration tests.

**Files:**
- Modify: `.github/workflows/ci.yml`

### Step 1: Read current CI file

Read `.github/workflows/ci.yml` to understand current structure.

### Step 2: Add integration-tests job

Add a new job after the existing `python-lint-test` job:

```yaml
  integration-tests:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@34e114876b0b11c390a56381ad16ebd13914f8d5 # v4

      - name: Set up Python
        uses: actions/setup-python@a26af69be951a213d495a4c3e4e4022e16d87065 # v5
        with:
          python-version: "3.13"
          cache: 'pip'

      - name: Install Python dependencies
        run: |
          pip install pytest playwright
          pip install shiny pandas plotly shinywidgets python-dotenv shinyswatch shinychat

      - name: Install Playwright browsers
        run: playwright install --with-deps chromium

      - name: Run Playwright integration tests
        run: python -m pytest tests/python/test_app_playwright.py -v --tb=short
```

**IMPORTANT:**
- Use the same SHA-pinned action references as the other jobs
- The `playwright install --with-deps chromium` command installs both the browser and OS-level dependencies (libgtk, libgbm, etc.)
- Shiny's `create_app_fixture()` automatically starts/stops the app — no manual app management needed
- This job runs independently from the other two jobs (no `needs:` dependency)

### Step 3: Verify YAML syntax

Run: `cd /home/razinka/AQUABCv0.2 && python3 -c "import yaml; yaml.safe_load(open('.github/workflows/ci.yml'))"`
Expected: No errors (or install PyYAML first if needed)

### Step 4: Commit

```bash
git add .github/workflows/ci.yml
git commit -m "ci: add Playwright integration test job

Run 19 Playwright tests against the Shiny app in CI. Uses Shiny's
built-in test fixture for automatic app lifecycle management.
Installs Chromium via playwright install --with-deps."
```

---

## Task 5: Update Sprint 3 in TODO Plan

**Files:**
- Modify: `TODO_IMPLEMENTATION_PLAN.md`

### Step 1: Mark Sprint 3 complete

```markdown
### Sprint 3 — Code Quality (3–5 days) --- COMPLETED 2026-02-14
- [x] 2.3 Deduplicate build/rebuild logic — **Done** (extracted _execute_build_process helper)
- [x] 2.5 Unit tests for business logic — **Done** (4 functions extracted to utils.py, 16 tests added)
- [x] 3.2 Integration tests in CI — **Done** (Playwright job added, 19 tests)
- [x] 3.6 Pre-commit hooks — **Done** (ruff + file hygiene hooks configured)
```

### Step 2: Commit

```bash
git add TODO_IMPLEMENTATION_PLAN.md
git commit -m "docs: mark Sprint 3 items as complete in TODO plan"
```

---

## Post-flight: Full Verification

**Step 1:** `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
**Step 2:** `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py --ignore=tests/python/test_safe_resolve.py`
**Step 3:** `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
