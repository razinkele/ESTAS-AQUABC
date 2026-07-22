# Loadable setup registry (CL29 + Standard) — design

**Date:** 2026-07-22
**Status:** Design — awaiting user review
**Author:** Arturas Razinkovas-Baziukas (with Claude)
**Scope:** `shiny_app/` (new `setups.py` + wiring in `run_control`, `input_files`, `app_state`, output/geometry resolution). No Fortran changes.

## Goal

Add a **general setup registry** to the Shiny UI so a user can load a complete model
configuration in one selection. Ship two entries: the existing **Standard (25-box)** model and
**CL29 — Curonian Lagoon (29-box)**. Selecting a setup drives the input file, the run
environment, the box count, and which inputs/outputs directories the views read — so CL29 runs
*correctly* (today it is technically selectable but silently broken) and the whole UI reflects the
loaded setup. Designed so future applications are added as **data**, not new special-cases.

## Background — current state

- `INPUT_CL29.txt` already appears in the Run Control config dropdown (it's scanned from repo-root
  `INPUT*.txt`), so CL29 is *selectable* — but nothing wires up what makes it work:
  - **No `ESTAS_HOLD_VOLUME=1`.** Without it CL29 drains box-18 volume and crashes ~day 449. The
    run executor already builds a `run_env` (`app_state.py:127`, passed at `:140`) — the flag is
    simply never set.
  - **`INPUTS_CL29/` is gitignored + converter-generated** (present now, 57 files; absent on a
    fresh clone). The 25-box `INPUTS/` is committed (always present).
  - **Box count is hardcoded to 25** in the selector dropdowns (`run_control.py:198`,
    `input_files.py:95`; copy string `input_files.py:400`).
  - **Output/geometry resolution is not setup-aware:** `file_locators.get_output_folder()`
    hardcodes `INPUT.txt` → the results views would show `OUTPUTS/` even after a CL29 run wrote to
    `OUTPUTS_CL29/`; the input browser + box-network map read the hardcoded `INPUTS/` dir.
- The app already has the right seams: modules reach shared state via `run = state.run`
  (`dashboard.py:173`); the contract pattern is `run.X = <reactive>` (`run.command_config`,
  `run.run_executable_name`); and a config-driven output resolver already exists
  (`get_output_folder_from_config(input_txt_path=…)`, `output_data.py:58`).

## Design

### 1. The registry — `shiny_app/setups.py` (new; pure, stdlib-only)

The single source of truth. Box count, directories, and env become **data**:

```python
@dataclass(frozen=True)
class Setup:
    id: str            # "standard" | "cl29"
    name: str          # human label for the selector
    description: str
    input_file: str    # "INPUT.txt" | "INPUT_CL29.txt"   (repo-root config)
    inputs_dir: str    # "INPUTS" | "INPUTS_CL29"
    output_dir: str    # "OUTPUTS" | "OUTPUTS_CL29"
    box_count: int     # 25 | 29
    env: dict          # {} | {"ESTAS_HOLD_VOLUME": "1"}
    unavailable_hint: str = ""   # shown when inputs_dir is missing

SETUPS = [
    Setup("standard", "Standard (25-box)",
          "The default AQUABC pelagic configuration (committed INPUTS/).",
          "INPUT.txt", "INPUTS", "OUTPUTS", 25, {}),
    Setup("cl29", "CL29 — Curonian Lagoon (29-box)",
          "EUTROPY-derived 29-box Curonian Lagoon; requires ESTAS_HOLD_VOLUME=1.",
          "INPUT_CL29.txt", "INPUTS_CL29", "OUTPUTS_CL29", 29, {"ESTAS_HOLD_VOLUME": "1"},
          unavailable_hint="Generate inputs first: python tools/eutropy_poc/eutropy_to_estas.py"),
]
```

Helpers: `list_setups()`, `get_setup(id) -> Setup`, `default_setup() -> Setup` (returns
`standard`), `is_available(setup, root) -> bool` (does `root/inputs_dir/` exist and is non-empty).
No Shiny imports — unit-testable in isolation.

### 2. Propagation — one `reactive.Calc` on the existing contract

Run Control owns the setup selector `input.setup_select` and publishes the chosen `Setup` on the
RunController, exactly like `run.command_config`:

- `RunController.__init__` gains a placeholder `self.current_setup = None`.
- `run_control_server` assigns `run.current_setup = reactive.Calc(lambda: get_setup(input.setup_select() or "standard"))`.

Every consumer reads one place: `state.run.current_setup()`. The five driven concerns:

| concern | today | with a setup |
|---|---|---|
| **input file** | `input.cmd_input_file` dropdown | selecting a setup sets `cmd_input_file` to `setup.input_file` (dropdown remains, for advanced override) |
| **run env** | `run_env = os.environ.copy()` only | `start_run` merges `setup.env` → injects `ESTAS_HOLD_VOLUME=1` for CL29 |
| **box count** | `range(1, 26)` hardcoded | selectors use `range(1, setup.box_count + 1)`; the "25 boxes" copy string derives from `box_count` |
| **output dir** | `get_output_folder()` parses `INPUT.txt` | results views resolve from `setup.input_file` (via the existing `get_output_folder_from_config`) / `setup.output_dir` |
| **inputs dir** | `INPUTS/` hardcoded (browser, box-network map) | those views read `setup.inputs_dir` |

### 3. Availability UX

The selector lists both setups. An **unavailable** setup (CL29 before its inputs exist) is shown
disabled/greyed with its `unavailable_hint` (the exact converter command). Availability is checked
on render and re-checked when the setup panel is opened. The Standard setup's `INPUTS/` is
committed, so it is always available.

### 4. Error handling

- Selecting an unavailable setup is prevented (disabled option); the hint tells the user the one
  command to run. If inputs vanish between check and run, `start_run` surfaces the ESTAS failure in
  the run log as it does today (no new silent path).
- `get_setup(unknown_id)` falls back to `default_setup()` (defensive; the selector only offers
  known ids).
- Empty `env` (Standard) means the run path is byte-for-byte the current behavior.

### 5. Backward compatibility

`default_setup()` is **Standard** with `env={}`, `box_count=25`, `INPUTS/`, `OUTPUTS/`, `INPUT.txt`.
With no interaction the app behaves exactly as today; CL29 is purely additive. `start_run` gains one
optional `env_extra={}` parameter — existing callers unchanged.

## Components / files

**New:** `shiny_app/setups.py`; `tests/python/test_setups.py`.

**Modified:**
- `shiny_app/modules/run_control.py` — setup selector UI (top of the tab) + `run.current_setup`
  publish + box-selector range + `start_run` env pass-through + set `cmd_input_file` on selection.
- `shiny_app/app_state.py` — `RunController.current_setup` placeholder; `start_run(..., env_extra={})`
  merges into `run_env`.
- `shiny_app/modules/input_files.py` — box-selector range + copy string from `current_setup.box_count`;
  input browser + box-network map read `current_setup.inputs_dir`.
- **Output-reading views** (`dashboard`, `plot`, output browser, `observation_compare` as applicable)
  — resolve outputs from `current_setup` rather than hardcoded `OUTPUTS/`. **`setup.output_dir` is
  authoritative**; it equals the input file's declared OUTPUT folder by construction (INPUT_CL29.txt
  → `OUTPUTS_CL29/`), so wherever a call site already parses a config via
  `get_output_folder_from_config`, feeding it `current_setup.input_file` yields the same value — the
  plan picks the minimal-churn mechanism per call site. The exact call sites are enumerated there.

## Data flow

`setup_select` (Run Control) → `run.current_setup()` (contract `reactive.Calc`) → consumers:
run command (input file), `start_run` (env), selectors (box_count), input browser + map (inputs_dir),
results views (output_dir). One selection, one source of truth, many reactive readers.

## Testing

- `tests/python/test_setups.py` (pure): both entries' fields; `get_setup`/`default_setup`;
  `is_available` true (tmp `INPUTS/` dir) and false (missing dir); unknown-id fallback.
- The mandatory UI backstop — `import shiny_app.app; create_ui().tagify()` — since the suite does
  not import `app.py`; guards against a broken selector/render.
- A focused check that `start_run` merges `env_extra` into `run_env` (the `HOLD_VOLUME` path).

## Scope tiers (for the plan)

1. **Core (CL29 runs correctly):** registry + selector + `run.current_setup` + env injection +
   box-count selectors. After this, loading CL29 runs to completion with the right geometry.
2. **Views reflect the setup:** inputs-dir (browser/map) + output-dir (results) plumbing, enumerated
   per call site.

## Non-goals

- **No in-app converter/"Generate inputs" button** — availability is detect-and-guide (option A);
  the button can layer on later using the same `is_available` check.
- **No committing `INPUTS_CL29/`** — it stays gitignored/regenerable.
- **No new box-geometry logic** — data-driven views (map, plots reading outputs) already handle N
  boxes; only the hardcoded *selector ranges* and *copy strings* change.
- No Fortran / model-structure changes; no new setup beyond the two.
