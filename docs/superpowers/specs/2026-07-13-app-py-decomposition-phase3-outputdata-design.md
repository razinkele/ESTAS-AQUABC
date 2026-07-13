# Design: `shiny_app/app.py` decomposition — phase 3, output-data cluster

- **Date:** 2026-07-13
- **Status:** Draft (awaiting user review)
- **Author:** Arturas Razinkovas-Baziukas (with Claude)
- **Scope:** `shiny_app/`. Extract seven pure output-file helpers from `server()` into a new module
  `shiny_app/output_data.py`. **No change to the reactive graph or observable behavior.**
- **Predecessors:** phase 1 (helpers), phase 2 (`create_ui()`), phase 3 pilot (build cluster,
  v0.3.5), phase 3 box-network (v0.3.6). Third phase-3 cluster.

## 1. Context & motivation

`server()` is now ~5,235 lines. Its output-handling code tangles **pure** helpers with genuinely
**reactive** ones. This cluster extracts only the seven unambiguously pure functions; the reactive
neighbours (`run_command` — reads 7 inputs; `analyze_output_directory` — 6 inputs + pandas;
`get_selected_output_file_path` — 4 inputs) and the `reactive.Value`-backed CSV cache
(`_get_cached_data`/`_get_cached_csv`, keyed on `csv_cache = reactive.Value(...)` at app.py:3696)
**stay** — their "purity" is one `pd.read_csv` wrapped in reactive state; prying it apart adds risk
for no real gain.

## 2. Goal / non-goals

- **Goal:** move the seven pure functions into `shiny_app/output_data.py`; update their 11 external
  call sites to `output_data.<fn>(...)`; delete the nested defs.
- **Non-goals:** the reactive functions/cache above; other clusters; Shiny-modules rearchitecture.
- **Invariant:** observable behavior unchanged. Behavioral (no byte oracle) — guarded by unit tests
  on the pure functions + CI Playwright.

## 3. The seven functions & their dependencies

All seven reference **no** `input.*`/`session`/`output`/reactive value (verified — the earlier
`input.*` hits were adjacent-decorator boundary artifacts). Their non-trivial dependencies:

| Function (current name) | Deps | Notes |
|---|---|---|
| `_looks_numeric(s)` | none | pure `float(s)` check |
| `format_elapsed(seconds)` | none | pure HH:MM:SS format |
| `get_output_folder_from_config()` | `INPUT_TXT_PATH`, `SimulationConfigFile`, `logger`, `os` | reads INPUT.txt |
| `get_output_files_info()` | `ROOT`, `logger`, `os`; **calls `get_output_folder_from_config`** | dir stats |
| `_get_output_columns(file_path, file_format)` | `OUTPUT_CSV`, `PELAGIC_BOX_COLUMNS`, `pd`, `logger`, `os`; **calls `_looks_numeric`** | CSV/`.out`/`.bin` header |
| `get_output_directories()` | `ROOT`, `OUTPUT_CSV`, `os` | scan `OUTPUTS*` dirs |
| `get_output_files_from_dir(dir_name, file_format)` | `ROOT`, `os` | scan files by format |

Two **intra-cluster calls** (`get_output_columns→looks_numeric`, `get_output_files_info→get_output_folder_from_config`)
move together into the module — no external coupling.

## 4. Module layout & const handling

`shiny_app/output_data.py`:
`"""Pure output-file helpers (extracted from server())."""`, then `import os`, `import logging`,
`import pandas as pd`, `logger = logging.getLogger("AQUABC")`, the leaf-module imports (via the
`try/except ImportError` fallback), the self-computed path consts, and the 7 functions.

- **Leaf-module imports** (these names live in already-extracted leaf modules — imported directly,
  no `app.py` dependency): `PELAGIC_BOX_COLUMNS` from `utils`; `SimulationConfigFile` from
  `simulation_config`.
- **Self-computed path consts** (identical to `app.py`'s — all derive from the same repo layout):
  `ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))`;
  `OUTPUT_CSV = os.path.join(ROOT, 'OUTPUT.csv')`; `INPUT_TXT_PATH = os.path.join(ROOT, 'INPUT.txt')`.
  (`ROOT`/`OUTPUT_CSV` are module consts in `app.py:243/245`; `INPUT_TXT_PATH` is a `server()` local
  at `app.py:2573` = `os.path.join(ROOT, "INPUT.txt")` — the module re-derives the same value.)
- **Default-arg pattern → arg-free call sites + testability.** The path-dependent functions take
  those consts as **parameters defaulting to the module consts**, so the 11 call sites stay
  argument-free while unit tests can inject a `tmp_path`:
  - `looks_numeric(s)` / `format_elapsed(seconds)` — no consts.
  - `get_output_folder_from_config(input_txt_path=INPUT_TXT_PATH)`
  - `get_output_files_info(root=ROOT, input_txt_path=INPUT_TXT_PATH)` (passes `input_txt_path`
    through to `get_output_folder_from_config`)
  - `get_output_columns(file_path=None, file_format=None, output_csv=OUTPUT_CSV)`
  - `get_output_directories(root=ROOT, output_csv=OUTPUT_CSV)`
  - `get_output_files_from_dir(dir_name, file_format="text", root=ROOT)`

Public names drop the leading underscore (`_looks_numeric`→`looks_numeric`,
`_get_output_columns`→`get_output_columns`). Bodies move **verbatim** except: the underscore-drop,
the const references now resolve to the module consts/params, and the two intra-cluster calls now
reference the module functions.

## 5. Call-site wiring (11 external sites)

Module-import form: `try: from shiny_app import output_data / except ImportError: import output_data`,
after the `box_network` re-import block. Then update (all pass **no** const args — defaults handle it):

| Function | Sites | Becomes |
|---|---|---|
| `format_elapsed` | 4199, 4233, 4238 | `output_data.format_elapsed(<arg>)` |
| `get_output_files_info` | 4183, 4224 | `output_data.get_output_files_info()` |
| `_get_output_columns` | 3825 | `output_data.get_output_columns(<args>)` |
| `get_output_directories` | 4754, 4784, 4793 | `output_data.get_output_directories()` |
| `get_output_files_from_dir` | 5059, 5076 | `output_data.get_output_files_from_dir(<args>)` |

(The 2 intra-cluster call sites — `_looks_numeric`@3790 inside `get_output_columns`, and
`get_output_folder_from_config`@3994 inside `get_output_files_info` — are deleted along with their
enclosing defs; inside the module they call the sibling functions.) Delete the 7 nested defs. The
`server()`-local `INPUT_TXT_PATH` at 2573 **stays** (other server code may use it — verify; the
module self-computes its own copy regardless).

## 6. Validation gate

1. `python -m py_compile shiny_app/app.py`; `python -c "import shiny_app.output_data"`.
2. `ruff check --select F821 shiny_app/app.py shiny_app/output_data.py` → clean (lint the new module
   too — it imports `os`/`logging`/`pandas` + the two leaf names; F821 catches a forgotten one).
3. Unit tests `tests/python/test_output_data.py` (in-process — pandas imports cleanly):
   - `looks_numeric`: `"3.5"`/`"10"`→True; `"abc"`/`""`/`None`→False.
   - `format_elapsed`: 3661→"1h 1m 1s"; 61→"1m 1s"; 5→"5s".
   - `get_output_directories(root=tmp)`: a `tmp_path` with `OUTPUTS_a/` dir + a stray file → only
     the `OUTPUTS*` dirs (+ `ROOT` key iff `output_csv` exists).
   - `get_output_files_from_dir("SUB", "text", root=tmp)`: `.out` files returned for text, `.bin`
     for binary; missing dir → `{}`.
   - `get_output_columns(file_path=<tmp csv>, file_format='csv')`: header row → stripped column
     list; `get_output_columns(file_format='binary')` → `PELAGIC_BOX_COLUMNS`.
   - `get_output_folder_from_config(input_txt_path=<missing>)` → `"OUTPUTS"` fallback.
   - `get_output_files_info(root=<empty tmp>, input_txt_path=<missing>)` → `{"exists": False, ...}`.
4. Full suite green (148 baseline + new tests; no regression).
5. Playwright is CI-only (not local).

## 7. Risks & mitigations

| Risk | Mitigation |
|---|---|
| A function references a name not captured (esp. a hidden const/helper) | Review does an exhaustive free-name sweep of all 7 bodies; `import shiny_app.output_data` + F821 on the new file + tests catch a miss |
| `INPUT_TXT_PATH` server-local differs from the module's self-computed value | Both are `os.path.join(ROOT, "INPUT.txt")` from the same repo root; review confirms the server-local's single definition (2573) matches |
| Default-arg const captured at wrong time | Consts defined at module top, above the functions, so `def f(root=ROOT)` binds the intended value at import |
| A call site passes args the new default-arg signature doesn't expect | §5 table lists each; the varying args (`file_path`, `dir_name`, `file_format`, the elapsed/seconds value) are unchanged positionally; F821/py_compile + tests catch a mismatch |
| Circular import | `output_data.py` imports stdlib + pandas + the `utils`/`simulation_config` leaf modules; never `app` |
| Name collision on the 7 public names | Verified each has exactly one def in `app.py`; module-import form (`output_data.<fn>`) sidesteps collisions regardless |

## 8. Deferred roadmap

- Remaining phase-3: mass-balance/observations/scenarios (largely delegate to phase-1 leaf modules
  already — likely little to extract), the reactive CSV cache + `run_command`/`analyze_output_directory`,
  the inline command-logic copy (~628–667), `_execute_build_process`, the `box_network.py` lint PR.
- Full Shiny-modules rearchitecture — terminal item.

## 9. Files touched

- **New:** `shiny_app/output_data.py`; `tests/python/test_output_data.py`.
- **Modified:** `shiny_app/app.py` (delete 7 nested defs; add re-import; update 11 call sites);
  `TODO_IMPLEMENTATION_PLAN.md`.
- **Out of scope:** the reactive handlers/cache, any `.f90`.
