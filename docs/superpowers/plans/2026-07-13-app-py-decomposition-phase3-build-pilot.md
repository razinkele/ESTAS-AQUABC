# app.py Decomposition Phase 3 Pilot — Build-Cluster Extraction — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the non-reactive build/command logic from `shiny_app/app.py`'s `server()` into a new pure module `shiny_app/build_commands.py`, leaving the four `server()` nested functions as thin wrappers that resolve reactive inputs and delegate — observable behavior unchanged.

**Architecture:** The wrapper does only the reactive reads (raw, with the input-not-ready try/except) and calls a module-level function that owns all value-defaulting and logic. `build_commands.py` imports stdlib only and takes `root`/defaults as arguments — it imports nothing from `app.py` (no circular import). Spec: `docs/superpowers/specs/2026-07-13-app-py-decomposition-phase3-build-pilot-design.md`.

**Tech Stack:** Python 3.10+, Shiny for Python, pytest, ruff.

## Global Constraints

- **Behavior-preserving, not byte-identical.** There is no rendered-HTML oracle here (this is behavioral). Correctness rests on the unit tests pinning each extracted function + the review confirming the moved logic and the wrappers' reactive reads are equivalent + CI Playwright.
- **Wrapper = raw reactive reads only.** Each wrapper resolves `input.X()` inside `try/except` (default to the falsy sentinel the original used), applies **no** `or "…"` defaulting, and passes the raw value. **Preserve the original read pattern exactly**, including that `cmd_binary_filename` is read **only when `cmd_binary_enabled` is truthy** (so the reactive dependency graph is unchanged).
- **Pure function owns all defaulting.** `assemble_estas_command` owns `exe_name or "ESTAS_II"`, `input_file or "INPUT.txt"`, `const_file or ""`, the switch-on-empty `PELAGIC_OUTPUT.bin` default, the `(binary or shear) and not const → default_constants_file` insertion, and the shear-without-binary placeholder.
- **Module-import form.** `app.py` re-imports via `try: from shiny_app import build_commands / except ImportError: import build_commands` and calls `build_commands.<fn>(...)`. Do NOT `from build_commands import <names>` — `get_available_executables`/`get_executable_info` wrappers share the callee's name and would shadow it (infinite recursion).
- **`build_commands.py` imports:** `os`, `glob`, `subprocess`, `from datetime import datetime`. No `shiny`, no `app`.
- **Gate (run after each task):** `py_compile shiny_app/app.py`; `python -c "import shiny_app.build_commands"`; `ruff check --select F821 shiny_app/app.py shiny_app/build_commands.py` (lint the new module too — its `subprocess`/`datetime` use means F821 on `app.py` alone would miss a forgotten import); `python -m pytest tests/python -q` green. Playwright is CI-only (not installed locally) — do not claim it locally.

---

### Task 1: Create `shiny_app/build_commands.py` + unit tests

**Files:**
- Create: `shiny_app/build_commands.py`
- Create: `tests/python/test_build_commands.py`

**Interfaces:**
- Produces: `assemble_estas_command(exe_name, input_file, const_file, binary_enabled, binary_filename, shear_file, default_constants_file) -> list[str]`; `get_available_executables(root) -> list[str]`; `get_executable_info(exe_name, root) -> dict`; `target_exe_name(compiler, build_type) -> str`.
- Consumes: stdlib only.

This task leaves `app.py` **untouched**, so the app still runs on the current nested functions; the new module is independently unit-tested.

- [ ] **Step 1: Write the failing tests** — `tests/python/test_build_commands.py`:

```python
import os
import stat
import pytest
from shiny_app.build_commands import (
    assemble_estas_command, get_available_executables,
    get_executable_info, target_exe_name,
)

DC = "WCONST_01.txt"  # default_constants_file


def test_assemble_all_not_ready_defaults():
    # wrapper passes raw None/False on input-not-ready
    assert assemble_estas_command(None, None, None, False, None, None, DC) == \
        ["./ESTAS_II", "INPUT.txt"]


def test_assemble_input_only():
    assert assemble_estas_command("ESTAS_II", "MYINPUT.txt", "", False, None, "", DC) == \
        ["./ESTAS_II", "MYINPUT.txt"]


def test_assemble_input_and_const():
    assert assemble_estas_command("ESTAS_II", "INPUT.txt", "WCONST_02.txt", False, None, "", DC) == \
        ["./ESTAS_II", "INPUT.txt", "WCONST_02.txt"]


def test_assemble_binary_enabled_empty_name_inserts_default_const_and_placeholder():
    # switch on, name empty -> PELAGIC_OUTPUT.bin; binary set, no const -> default const
    assert assemble_estas_command("ESTAS_II_gf_release", "MYINPUT.txt", "", True, "", "", DC) == \
        ["./ESTAS_II_gf_release", "MYINPUT.txt", "WCONST_01.txt", "PELAGIC_OUTPUT.bin"]


def test_assemble_binary_enabled_named():
    assert assemble_estas_command("ESTAS_II", "INPUT.txt", "C.txt", True, "OUT.bin", "", DC) == \
        ["./ESTAS_II", "INPUT.txt", "C.txt", "OUT.bin"]


def test_assemble_shear_without_binary_uses_placeholder_and_default_const():
    # falsy exe -> ESTAS_II; shear set, no const -> default const; shear no binary -> placeholder
    assert assemble_estas_command("", "INPUT.txt", "", False, None, "INPUTS/SHEAR.txt", DC) == \
        ["./ESTAS_II", "INPUT.txt", "WCONST_01.txt", "PELAGIC_OUTPUT.bin", "INPUTS/SHEAR.txt"]


def test_assemble_binary_disabled_ignores_filename():
    # binary_enabled False -> binary_filename ignored, no const -> stop after input
    assert assemble_estas_command("ESTAS_II", "INPUT.txt", "", False, "IGNORED.bin", "", DC) == \
        ["./ESTAS_II", "INPUT.txt"]


@pytest.mark.parametrize("compiler,bt,expected", [
    ("gfortran", "release", "ESTAS_II_gf_release"),
    ("ifort", "debug", "ESTAS_II_ifort_debug"),
    ("ifx", "release", "ESTAS_II_ifx_release"),
    ("weirdfc", "release", "ESTAS_II_weirdfc_release"),  # unknown -> identity
])
def test_target_exe_name(compiler, bt, expected):
    assert target_exe_name(compiler, bt) == expected


def test_get_available_executables(tmp_path):
    exe = tmp_path / "ESTAS_II"
    exe.write_text("x"); os.chmod(exe, os.stat(exe).st_mode | stat.S_IEXEC)
    (tmp_path / "notes.txt").write_text("x")            # not executable -> excluded
    (tmp_path / "ESTAS_II_dir").mkdir()                 # matches AQUABC*/ESTAS_II_* but is a dir
    result = get_available_executables(str(tmp_path))
    assert result == ["ESTAS_II"]                       # only the executable file, deduped/sorted


def test_get_executable_info_missing(tmp_path):
    assert get_executable_info("nope", str(tmp_path)) == {"exists": False}


def test_get_executable_info_existing(tmp_path):
    exe = tmp_path / "ESTAS_II"
    exe.write_text("x"); os.chmod(exe, os.stat(exe).st_mode | stat.S_IEXEC)
    info = get_executable_info("ESTAS_II", str(tmp_path))
    assert info["exists"] is True
    assert info["path"] == str(exe)
    assert info["size"] == 1
    assert "file_type" in info          # value comes from file(1) / "Unknown" — env-dependent, don't pin
    assert "modified" in info
```

- [ ] **Step 2: Run tests — verify they fail**

Run: `python -m pytest tests/python/test_build_commands.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'shiny_app.build_commands'`

- [ ] **Step 3: Create `shiny_app/build_commands.py`** with exactly this content:

```python
"""Non-reactive build/command helpers (extracted from server())."""
import os
import glob
import subprocess
from datetime import datetime


def assemble_estas_command(exe_name, input_file, const_file, binary_enabled,
                           binary_filename, shear_file, default_constants_file):
    """Assemble the model command-line args from resolved (raw) widget values.

    Owns all value-defaulting so it is fully unit-testable. Arg-count rules:
    0 args uses INPUT.txt; 1: INPUT_FILE; 2: +CONSTANTS; 3: +BINARY; 4: +SHEAR.
    """
    exe_name = exe_name or "ESTAS_II"
    cmd = [f"./{exe_name}"]

    # Arg 1: input file (required)
    cmd.append(input_file or "INPUT.txt")

    const_file = const_file or ""

    # Binary file only used if the switch is enabled
    binary_file = ""
    if binary_enabled:
        binary_file = binary_filename or ""
        if not binary_file:
            binary_file = "PELAGIC_OUTPUT.bin"  # default if switch on but name empty

    shear_file = shear_file or ""

    # If binary or shear file is set, we need a constants file
    if (binary_file or shear_file) and not const_file:
        const_file = default_constants_file

    # Arg 2: constants file
    if not const_file:
        return cmd
    cmd.append(const_file)

    # Shear set but no binary -> placeholder binary output
    if shear_file and not binary_file:
        binary_file = "PELAGIC_OUTPUT.bin"

    # Arg 3: binary output file
    if not binary_file:
        return cmd
    cmd.append(binary_file)

    # Arg 4: shear stress file (optional)
    if shear_file:
        cmd.append(shear_file)

    return cmd


def get_available_executables(root):
    """Scan for available executable files under root."""
    executables = []
    exe_patterns = ["ESTAS_II", "ESTAS_II_*", "AQUABC*"]
    for pattern in exe_patterns:
        for f in glob.glob(os.path.join(root, pattern)):
            if os.path.isfile(f) and os.access(f, os.X_OK):
                executables.append(os.path.basename(f))
    for f in ["AQUABC02GFREL", "AQUABC02INTL"]:
        path = os.path.join(root, f)
        if os.path.isfile(path) and os.access(path, os.X_OK):
            if f not in executables:
                executables.append(f)
    return sorted(set(executables))


def get_executable_info(exe_name, root):
    """Return metadata about an executable (size/mtime + file(1) type)."""
    exe_path = os.path.join(root, exe_name)
    if not os.path.exists(exe_path):
        return {"exists": False}

    info = {
        "exists": True,
        "path": exe_path,
        "size": os.path.getsize(exe_path),
        "modified": datetime.fromtimestamp(os.path.getmtime(exe_path)).strftime('%Y-%m-%d %H:%M:%S'),
    }

    # Check if stripped (no debug symbols)
    try:
        result = subprocess.run(["file", exe_path], capture_output=True, text=True, timeout=5)
        info["file_type"] = result.stdout.strip()
        info["stripped"] = "stripped" in result.stdout.lower()
        info["has_debug"] = "not stripped" in result.stdout.lower()
    except Exception:
        info["file_type"] = "Unknown"
        info["stripped"] = None

    return info


def target_exe_name(compiler, build_type):
    """Map (compiler, build_type) to the target executable name."""
    fc_short = {
        "gfortran": "gf",
        "ifort": "ifort",
        "ifx": "ifx",
    }.get(compiler, compiler)
    return f"ESTAS_II_{fc_short}_{build_type}"
```

- [ ] **Step 4: Run tests — verify they pass**

Run: `python -m pytest tests/python/test_build_commands.py -q`
Expected: PASS (all cases). If `test_get_executable_info_existing` fails because `file(1)` is absent, that is fine — the `except` sets `file_type="Unknown"` and `"file_type" in info` still holds; the test does not pin the value.

- [ ] **Step 5: Gate + commit**

```bash
python -c "import shiny_app.build_commands"
ruff check --select F821 shiny_app/build_commands.py
python -m pytest tests/python -q     # 123 baseline + new tests, no regressions
git add shiny_app/build_commands.py tests/python/test_build_commands.py
git commit -m "feat(shiny): add pure build_commands module + unit tests (phase 3 pilot, task 1)"
```

---

### Task 2: Wire `server()` nested functions as thin wrappers

**Files:**
- Modify: `shiny_app/app.py` (add re-import; replace 4 nested-function bodies with thin wrappers)
- Modify: `TODO_IMPLEMENTATION_PLAN.md` (mark phase-3 pilot progress)

**Interfaces:**
- Consumes: `build_commands.assemble_estas_command/get_available_executables/get_executable_info/target_exe_name` from Task 1.

- [ ] **Step 1: Add the re-import** after the `ui_chrome` re-import block in `app.py`:

```python
try:
    from shiny_app import build_commands
except ImportError:
    import build_commands
```

- [ ] **Step 2: Replace `build_estas_command()`** (currently app.py ~713–787) with this thin wrapper — RAW reads, preserving the conditional `cmd_binary_filename` read (reactive-dependency-preserving):

```python
    def build_estas_command():
        """Build the model command from current widget values (thin wrapper).

        Reads are raw (input-not-ready -> falsy sentinel); all defaulting lives in
        build_commands.assemble_estas_command. Command format: 0 args uses INPUT.txt;
        1: INPUT_FILE; 2: +CONSTANTS; 3: +BINARY; 4: +SHEAR.
        """
        try:
            exe_name = input.run_executable()
        except Exception:
            exe_name = None
        try:
            input_file = input.cmd_input_file()
        except Exception:
            input_file = None
        try:
            const_file = input.cmd_constants_file()
        except Exception:
            const_file = None
        try:
            binary_enabled = input.cmd_binary_enabled()
        except Exception:
            binary_enabled = False
        # Read the binary filename ONLY when the switch is on (preserve original reactive deps)
        binary_filename = None
        if binary_enabled:
            try:
                binary_filename = input.cmd_binary_filename()
            except Exception:
                binary_filename = None
        try:
            shear_file = input.cmd_shear_stress_file()
        except Exception:
            shear_file = None
        return build_commands.assemble_estas_command(
            exe_name, input_file, const_file, binary_enabled,
            binary_filename, shear_file, DEFAULT_CONSTANTS_FILE)
```

- [ ] **Step 3: Replace `get_available_executables()`** (app.py ~805–820) with:

```python
    def get_available_executables():
        """Scan for available executable files (thin wrapper)."""
        return build_commands.get_available_executables(ROOT)
```

- [ ] **Step 4: Replace `get_executable_info(exe_name)`** (app.py ~822–845) with:

```python
    def get_executable_info(exe_name):
        """Get information about an executable (thin wrapper)."""
        return build_commands.get_executable_info(exe_name, ROOT)
```

- [ ] **Step 5: Replace `get_target_exe_name()`** (app.py ~887–903) with:

```python
    def get_target_exe_name():
        """Generate executable name based on compiler and build type (thin wrapper)."""
        try:
            compiler = input.build_compiler()
            build_type = input.build_type()
        except Exception:
            return "ESTAS_II_gf_release"
        return build_commands.target_exe_name(compiler, build_type)
```

Leave `DEFAULT_CONSTANTS_FILE = "WCONST_01.txt"` (711) and the `@render.text def cmd_preview()` (789) as-is; all 15 call sites of the four wrappers are unchanged.

- [ ] **Step 6: Run the gate**

```bash
python -m py_compile shiny_app/app.py
python -c "import shiny_app.build_commands"
ruff check --select F821 shiny_app/app.py shiny_app/build_commands.py
python -m pytest tests/python -q
```
Expected: all pass; F821 clean; suite green. (Do not run Playwright locally — CI-only.)

- [ ] **Step 7: Update `TODO_IMPLEMENTATION_PLAN.md`** — mark the phase-3 build-cluster pilot done; note remaining phase-3 clusters (file-I/O, plot-prep, mass-balance, observations, scenarios) + `_execute_build_process` machinery as deferred.

- [ ] **Step 8: Commit**

```bash
git add shiny_app/app.py TODO_IMPLEMENTATION_PLAN.md
git commit -m "refactor(shiny): server() build helpers delegate to build_commands (phase 3 pilot, task 2)"
```

---

## Final verification (after both tasks)

- [ ] Full `pytest tests/python -q` green; `ruff check --select F821 shiny_app/app.py shiny_app/build_commands.py` clean.
- [ ] Manual equivalence check: the four wrappers resolve exactly the inputs the originals did (same reads, same try/except defaults, conditional `cmd_binary_filename` read preserved), and `assemble_estas_command` reproduces the original branching (the test table covers every branch).
- [ ] Broad whole-branch review before merge (subagent-driven-development final step).

## Notes

`assemble_estas_command` is a **restructure**, not a verbatim move — the original interleaved reactive reads with the branching, so the logic is re-expressed as a pure function of the 7 params (the reviewer's equivalence walk in the spec confirms the split matches the original). The other three functions are verbatim body moves with `ROOT`→`root` (and, for `target_exe_name`, the try/except default left behind in the wrapper).
