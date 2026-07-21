# Design: `shiny_app/app.py` decomposition — phase 3 pilot (build-cluster logic extraction)

- **Date:** 2026-07-13
- **Status:** ✅ Shipped — released v0.3.5 (`58f9476`); `shiny_app/build_commands.py` + `test_build_commands.py` present. §4.5-deferred `_execute_build_process` later delivered as `RunController.execute_build`.
- **Author:** Arturas Razinkovas-Baziukas (with Claude)
- **Scope:** `shiny_app/`. Extract the **non-reactive build/command logic** that `server()`'s
  handlers call into a new pure module `shiny_app/build_commands.py`, leaving the reactive
  handlers as thin wrappers. **No change to the reactive graph or observable behavior.**
- **Predecessors:** phase 1 (helper extraction, v0.3.3) + phase 2 (`create_ui()` split, v0.3.4).
  This is roadmap item §8 bullet 1 of the phase-1/2 specs — *piloted* on the build cluster only.

## 1. Context & motivation

After phase 2, `shiny_app/app.py` is ~6,450 lines; its bulk is now `server()` (lines 524–6438,
~5,914 lines) — **112 decorated reactive handlers + 23 plain nested `def`s**. The plain nested
defs include non-reactive logic (build-command construction, executable discovery, file
inspection) tangled inside the reactive closure. This pilot extracts the **build cluster** — the
cleanest, highest-value, most self-contained group — to prove the extraction pattern before later
increments (file-I/O, plot-prep, mass-balance, observations, scenarios).

## 2. Goal / non-goals

- **Goal:** move the non-reactive build/command *logic* into `shiny_app/build_commands.py` as
  module-level functions that take plain values and return plain values — independently
  unit-testable, no Shiny. (Three are pure; `get_executable_info` additionally shells out to
  `file(1)` via stdlib `subprocess` — side-effecting, but still no `app`/`shiny` dependency.)
  The `server()` nested functions become thin wrappers that resolve reactive inputs and delegate.
- **Non-goals (this pilot):** the reactive graph itself; the other 5 clusters; `_execute_build_process`
  (its subprocess execution, `_build_log_lines` mutation, and logging are interleaved — not a pure
  extraction; deferred); the full Shiny-modules rearchitecture.
- **Invariant:** observable behavior is unchanged. There is **no byte-identical oracle** here (this
  is behavioral, not declarative). Correctness rests on: (a) unit tests pinning each extracted pure
  function against the original behavior; (b) the moved logic being equivalent (verified in review);
  (c) the CI `integration-tests` Playwright job for the reactive wiring.

## 3. Approach — "resolve at the call site, pass plain values"

The extraction pattern for every phase-3 cluster:

```python
# build_commands.py — PURE, no Shiny, unit-testable
def assemble_estas_command(exe_name, input_file, const_file, binary_enabled,
                           binary_filename, shear_file, default_constants_file):
    ...pure branching logic...
    return cmd   # list[str]

# server() — thin wrapper: RAW reactive reads only (try/except → None/""/False), then delegate.
# The wrapper applies NO value-defaulting; the pure fn owns every default (see §4.1).
def build_estas_command():
    try: exe_name = input.run_executable()          # raw, no `or "ESTAS_II"` here
    except Exception: exe_name = None
    try: input_file = input.cmd_input_file()        # raw, no `or "INPUT.txt"` here
    except Exception: input_file = None
    ...  # const_file, binary_enabled, binary_filename, shear_file — same pattern, raw values
    return assemble_estas_command(exe_name, input_file, const_file, binary_enabled,
                                  binary_filename, shear_file, DEFAULT_CONSTANTS_FILE)
```

The thin wrapper keeps its **current name and 0-arg signature**, so its call sites are unchanged.
`build_commands.py` imports **stdlib only** (`os`, `glob`, `subprocess`, `datetime` — see §5) and
takes `root`/defaults as arguments — it imports nothing from `app.py`, so no circular import.

## 4. The four extractions

### 4.1 `assemble_estas_command(exe_name, input_file, const_file, binary_enabled, binary_filename, shear_file, default_constants_file) -> list[str]`
The branching logic of the current `build_estas_command()` (app.py 713–787): assemble
`["./<exe>", <input>, <const>, <binary>, <shear>]` with the documented arg-count rules.

**Boundary (critical — get this exact or behavior changes):** the wrapper does **only** the six
defensive reactive reads and passes the **raw** resolved values through; the pure function owns
**all** value-defaulting. Concretely:

- **Wrapper `build_estas_command()`** (stays in `server()`): six reads, each
  `try: v = input.X() except Exception: v = <falsy sentinel>` (`None`/`""`/`False`), then
  `return assemble_estas_command(exe_name, input_file, const_file, binary_enabled,
  binary_filename, shear_file, DEFAULT_CONSTANTS_FILE)`. The wrapper applies **no** `or "…"`
  defaulting itself — it hands over the raw `None`/`""`/`False`.
- **Pure `assemble_estas_command`** owns every default, so all of it is unit-testable:
  `exe_name or "ESTAS_II"`; `input_file or "INPUT.txt"`; `const_file or ""`;
  `if binary_enabled and not binary_filename → "PELAGIC_OUTPUT.bin"`;
  `if (binary or shear) and not const → default_constants_file`;
  shear-without-binary placeholder `"PELAGIC_OUTPUT.bin"`; then the arg-count assembly.

This is why the signature carries `binary_enabled` **and** the raw `binary_filename` separately (the
switch-on-but-empty default lives in the pure fn), and why §6.3's `binary_enabled`-but-empty-name
test targets the pure fn. Do **not** move any `or "…"` default into the wrapper — a dropped default
would turn the not-ready path from `["./ESTAS_II", "INPUT.txt"]` into `["./None", ""]` (a silent
behavior change `py_compile`/`F821` cannot catch).

**5 call sites unchanged** (app.py 792, 1341, 4565, 4724, 5099) — the wrapper keeps its name and
0-arg signature.

### 4.2 `get_available_executables(root) -> list[str]`
Verbatim body of the current `get_available_executables()` (app.py 805–820), with the closed-over
`ROOT` becoming the `root` parameter. Pure filesystem scan (glob + `os.access` X_OK). **Wrapper:**
`get_available_executables()` stays in `server()` → `return build_commands.get_available_executables(ROOT)`.
**3 call sites unchanged** (918, 1046, 1060).

### 4.3 `get_executable_info(exe_name, root) -> dict`
Verbatim body of the current `get_executable_info(exe_name)` (app.py **822–845**), with `ROOT` →
the `root` parameter. It inspects size/mtime (`os`, `datetime.fromtimestamp(...).strftime(...)`)
**and shells out to `file(1)`** via `subprocess.run(["file", exe_path], capture_output=True,
text=True, timeout=5)` for type/stripped info — so it is **stdlib-side-effecting, not strictly
pure**, but still stdlib-only with no `app`/`shiny` import, so the no-circular-import invariant
holds. Returns `{exists, path, size, modified, file_type, stripped, has_debug}` (or
`{exists: False}` when the path is missing). **Requires `import subprocess` and
`from datetime import datetime` in `build_commands.py`** (see §5). **Wrapper:**
`get_executable_info(exe)` in `server()` → `return build_commands.get_executable_info(exe, ROOT)`.
**4 call sites unchanged** (924, 968, 987, 4739).

### 4.4 `target_exe_name(compiler, build_type) -> str`
The pure logic of `get_target_exe_name()` (app.py 887–903): map compiler → short name
(`gfortran→gf`, `ifort→ifort`, `ifx→ifx`, else identity) and return
`f"ESTAS_II_{fc_short}_{build_type}"`. **Wrapper:** `get_target_exe_name()` in `server()` resolves
`input.build_compiler()`/`input.build_type()` (keeping the try/except that returns
`"ESTAS_II_gf_release"` when inputs aren't ready), calls this. **3 call sites unchanged** (907,
1180, 1231).

### 4.5 Deferred (this pilot): `_execute_build_process`
Its `make clean-lib` / `make` command lists are trivial, but the subprocess `Popen`, the
`_build_log_lines.append(...)` streaming (a `server()` closure), and logging are interleaved line by
line. Extracting a pure function would either leave the machinery behind (low value) or require
threading the log buffer + callbacks (high risk). Deferred to a later increment focused on the
build-execution machinery.

## 5. Module layout & imports

- **New:** `shiny_app/build_commands.py` — `"""Non-reactive build/command helpers (extracted from server())."""`
  then **`import os`, `import glob`, `import subprocess`, `from datetime import datetime`** (the last
  two are required by `get_executable_info` — §4.3; note the bare-name form `datetime.fromtimestamp`
  needs `from datetime import datetime`, not `import datetime`), and the 4 functions. No `shiny`, no
  `app` import.
- **`server()` keeps** the 4 same-named nested wrappers (thin adapters) plus the `DEFAULT_CONSTANTS_FILE`
  local (711) and `ROOT` (module const 234). Add the re-import after the `ui_chrome` block using the
  **module-import form** — `try: from shiny_app import build_commands / except ImportError: import
  build_commands` — and call `build_commands.<fn>(...)`. Do **not** use `from build_commands import
  <names>`: two wrappers (`get_available_executables`, `get_executable_info`) keep the **same name** as
  the function they call, so a name import would be shadowed by the wrapper (the wrapper would call
  itself → infinite recursion). The module-import form sidesteps the collision and is used uniformly
  by all 4 wrappers (`build_commands.assemble_estas_command(...)`, etc.).

## 6. Per-phase validation gate

1. `python -m py_compile shiny_app/app.py`; `python -c "import shiny_app.build_commands"`.
2. `ruff check --select F821 shiny_app/app.py shiny_app/build_commands.py` → clean. Lint the NEW
   module too, not just `app.py`: `get_executable_info`'s moved body uses `subprocess`/`datetime`, so
   F821 on `build_commands.py` is what catches a forgotten import there (F821 on `app.py` alone
   would not).
3. New unit tests `tests/python/test_build_commands.py` pin each extracted function against the
   ORIGINAL behavior — the valuable part:
   - `assemble_estas_command`: a table of input combinations → exact expected `list[str]`
     (0-arg default; input only; input+const; input+const+binary; +shear; binary-enabled but no
     const → default const inserted; shear but no binary → placeholder `PELAGIC_OUTPUT.bin`;
     binary_enabled but empty filename → `PELAGIC_OUTPUT.bin`).
   - `target_exe_name`: gfortran/ifort/ifx/unknown × a couple of build types.
   - `get_available_executables(root)`: a `tmp_path` with an executable file, a non-executable file,
     and a matching-name dir → only the executable basename returned, sorted/deduped.
   - `get_executable_info(exe, root)`: existing file → assert `exists is True`, correct `path`/`size`,
     and `"file_type" in info` — do **NOT** assert exact `file_type`/`stripped`/`has_debug`, which come
     from `file(1)` and are platform/environment-dependent (a strict equality would be flaky; guard
     the `file`-binary-absent case too, where the `except` sets `file_type="Unknown"`); missing →
     `{"exists": False}`.
4. Full Python suite green (123 baseline + new tests; no regression).
5. **Behavioral coverage caveat (as in phase 2):** Playwright/Selenium are not installed locally
   (`conftest.py` `collect_ignore`s them) — the reactive wiring runs only in CI's `integration-tests`
   job. Locally the net is the unit tests (step 3) + the mandatory equivalence review (§7).

## 7. Verbatim-logic verification

`assemble_estas_command`, `get_available_executables`, `get_executable_info` move their bodies
**verbatim** (only closed-over `ROOT`→`root` and the six `input.*()` reads → parameters).
`target_exe_name` moves the mapping dict + f-string verbatim. The reviewer confirms each moved body
is logically identical to the original (line-by-line), and that each thin wrapper resolves exactly
the same inputs (with the same try/except defaults) it did before. The unit tests pin the behavior.

## 8. Risks & mitigations

| Risk | Mitigation |
|---|---|
| A thin wrapper resolves an input differently (drops a try/except default) → behavior change | Wrappers keep the reactive reads + defaults **verbatim**; review diffs wrapper-vs-original; unit tests pin the pure logic |
| The pure logic diverges from the original branching (arg-count rules) | Body moved verbatim; the `assemble_estas_command` test table encodes every documented branch |
| Missed re-import → `NameError` at call | `ruff --select F821` + `import shiny_app.build_commands` + full suite |
| Circular import | `build_commands.py` imports stdlib only; takes `root`/defaults as args; never imports `app` |
| No local behavioral oracle for the reactive path | Unit tests on the pure functions + CI Playwright job; the wrappers are mechanical resolve-and-delegate |

## 9. Deferred roadmap (after this pilot)

- Remaining phase-3 clusters as later increments: file-I/O helpers, plot-data prep, mass-balance,
  observations, scenarios; then `_execute_build_process`'s build-execution machinery.
- Full Shiny-modules (`@module.ui`/`@module.server`) rearchitecture — still the terminal item.

## 10. Files touched (this pilot)

- **New:** `shiny_app/build_commands.py`; `tests/python/test_build_commands.py`.
- **Modified:** `shiny_app/app.py` (4 nested functions become thin wrappers; add the re-import);
  `TODO_IMPLEMENTATION_PLAN.md` (mark phase-3 pilot progress).
- **Out of scope:** the reactive handlers, `_execute_build_process`, any `.f90`.
