# Loadable setup registry (CL29 + Standard) — design

**Date:** 2026-07-22
**Status:** Design (rev. 2, after adversarial in-loop review) — awaiting user review
**Author:** Arturas Razinkovas-Baziukas (with Claude)
**Scope:** `shiny_app/` (new `setups.py` + wiring). No Fortran changes.

## Goal

Add a **general setup registry** to the Shiny UI so one selection loads a complete model
configuration. Ship two entries: **Standard (25-box)** and **CL29 — Curonian Lagoon (29-box)**.
Selecting a setup drives the input file, the run environment, the box count, and which
inputs/outputs directories the primary views read — so CL29 **runs correctly and safely** (today it
is selectable but silently broken) and future applications are added as **data**.

**Delivery scope (chosen): Core + primary results + guards.** Make CL29 runnable/correct and the
primary results views setup-aware; *guard* (not fully port) the secondary parametric viewers and the
box-network map, with honest in-UI notes, so nothing shows wrong data or corrupts Standard inputs.
Full setup-awareness of every viewer + a 29-box map is explicitly deferred (see Non-goals).

## Background — current state (verified against the code)

CL29 is technically selectable (`INPUT_CL29.txt` is in the config dropdown) but nothing wires up
what makes it work, and the mechanism is subtler than a naive read suggests:

- **`ESTAS_HOLD_VOLUME=1` is never set** → CL29 drains box-18 volume and crashes ~day 449. The run
  executor builds a `run_env`, **but reassigns it** (`app_state.py:135/137`
  `run_env = compiler_env.get_run_environment()`) after the initial `os.environ.copy()` (`:127`) —
  on the exact gfortran/non-Intel path CL29 uses. A merge must land **after** those reassignments,
  right before `Popen` (`:139`). `get_run_environment()` returns a fresh dict, so the merge must be
  additive (`run_env.update(...)`), not a replacement (which would drop Intel `LD_LIBRARY_PATH`).
- **Two run paths.** `RunController.start_run` (`app_state.py:119`, called from `run_control.py:531`
  via `threading.Thread`) *and* the dashboard **Quick Run** (`dashboard.py:handle_quick_run` → its
  own inline `subprocess.Popen(env=run_env)` at `:333`) — env injection must cover **both** or Quick
  Run still crashes CL29. Reactive values can't be read inside the worker thread, so the setup's
  `env`/`output_dir` must be captured as **plain values at run-start** and passed as thread args.
- **`INPUTS_CL29/` is gitignored + converter-generated** (`.gitignore:123`; `eutropy_to_estas.py:31`
  `OUT=…/INPUTS_CL29`; ~57 files). Standard `INPUTS/` is committed (92 files, always present).
- **Box count 25 is hardcoded in ≥7 places** — and the box selectors are built in **static
  `@module.ui`** (no reactive context), so they can't just "use `range(1, box_count+1)`"; they need a
  `reactive.effect` that `ui.update_*()` their choices (the `init_cmd_dropdowns` pattern,
  `run_control.py:264`). Sites: selector `run_control.py:198`, selector `input_files.py:95`,
  **output-config writer `run_control.py:631`** (Tier-1: decides which boxes emit output), plot box
  cap `plot.py:745`, bathymetry categories `app.py:367`, existence check `input_analysis.py:491`,
  dead helper `file_locators.py:75`; copy strings `input_files.py:400`, `box_network.py:475`,
  `diagnostics.py:240`.
- **Inputs-dir `INPUTS/` is hardcoded in ~10 modules** — parameters (`parameters.py:80`),
  initial-conditions (`initial_conditions.py:93`), model-options (`model_options.py:82/91`),
  timeseries plots (`plot.py:739+`), **scenarios read *and write* (`scenarios.py:118+`) — applying a
  scenario under CL29 would MUTATE the Standard inputs**, mass-balance stoichiometry
  (`mass_balance.py:112`), dashboard/run-control output-info file (`dashboard.py:645`,
  `run_control.py:562`), and the constants/shear scans (`run_control.py:277/285`).
- **Output-dir resolution is not setup-aware** — `file_locators.get_output_folder()` hardcodes
  `INPUT.txt`; results readers include dashboard, `plot`, **diagnostics** (`diagnostics.py`, note it
  lives in `shiny_app/`, not `modules/`), and the **run-progress tracker**
  (`app_state.get_output_files_info()` defaults to `OUTPUTS`). Two user-facing output-dir dropdowns
  already exist and are seeded from `INPUT.txt`: `sim_output_dir` (`run_control.py:208`) and
  `output_dir_select` (`plot.py:317`), plus diagnostics' `diag_output_dir` (`diagnostics.py:108`).
- **`OUTPUT.csv` is a separate artifact, NOT `OUTPUTS/`** — observation-compare (`observations.py:30`),
  mass-balance, and the plot CSV-preview read a fixed `ROOT/OUTPUT.csv`. There is no setup seam here;
  it is not part of this feature (Non-goal).
- **The box-network map is NOT N-box-safe** — `box_network.BOX_GEOM` (`:145-179`) is a literal dict
  of exactly boxes 1–25, iterated to draw the map; CL29's 26–29 would vanish. (A 29-box layout exists
  as `~/curonian/b29polys.gpkg` but rebuilding the map is deferred.)
- **Good seams that hold:** modules reach shared state via `run = state.run` (`dashboard.py:173`);
  the contract pattern is a `@reactive.calc` assigned to `run.X` (`run_control.py:328-331`); no
  `make_scope` bridge is needed (the Calc closes over run_control's namespaced `input`);
  `setup.output_dir` == the input file's declared OUTPUT folder (INPUT_CL29.txt:23=`OUTPUTS_CL29/`,
  INPUT.txt:23=`OUTPUTS/`), so `get_output_folder_from_config(input_txt_path=ROOT/setup.input_file)`
  yields it.

## Design

### 1. The registry — `shiny_app/setups.py` (new; pure, stdlib-only)

```python
@dataclass(frozen=True)
class Setup:
    id: str            # "standard" | "cl29"
    name: str          # selector label
    description: str
    input_file: str    # default config: "INPUT.txt" | "INPUT_CL29.txt"
    inputs_dir: str    # "INPUTS" | "INPUTS_CL29"
    output_dir: str    # "OUTPUTS" | "OUTPUTS_CL29"
    box_count: int     # 25 | 29
    env: dict          # {} | {"ESTAS_HOLD_VOLUME": "1"}
    required_input: str = "PELAGIC_INPUTS.txt"   # availability sentinel (the file ESTAS reads first)
    unavailable_hint: str = ""

SETUPS = [Setup("standard", …, "INPUT.txt", "INPUTS", "OUTPUTS", 25, {}),
          Setup("cl29", …, "INPUT_CL29.txt", "INPUTS_CL29", "OUTPUTS_CL29", 29,
                {"ESTAS_HOLD_VOLUME": "1"},
                unavailable_hint="Generate inputs: python tools/eutropy_poc/eutropy_to_estas.py")]
```

Helpers: `list_setups()`, `get_setup(id)`, `default_setup()` (→ standard), `is_available(setup, root)`
= `os.path.isfile(root/inputs_dir/required_input)` (a **specific required file**, not mere
non-emptiness — the converter `rmtree`s then writes ~57 files sequentially, so a crash leaves a
non-empty but incomplete dir). `input_files_for(setup, root)` → the repo-root `INPUT*.txt` whose
declared `PELAGIC_INPUT_FOLDER` matches `setup.inputs_dir` (used to filter the config dropdown so it
can never desync from the setup). No Shiny imports.

### 2. Propagation — one `reactive.calc` on the contract

- `RunController.__init__`: `self.current_setup = None` **but** consumers must never rely on the
  None; the `run_control` assignment happens before any reactive *body* fires. To be safe against
  edge reads, the placeholder is a callable default: `self.current_setup = lambda: default_setup()`.
- `run_control_server`: a `@reactive.calc` reading `input.setup_select()` (default `"standard"`),
  assigned `run.current_setup`. Consumers bind `run = state.run` at the top of their server and call
  `state.run.current_setup()` **inside** reactive bodies only.

### 3. The driven concerns (Tier 1 unless noted)

| concern | wiring |
|---|---|
| **input file** | selecting a setup sets `cmd_input_file` to `setup.input_file` via `update_select`; the config dropdown is **filtered to `input_files_for(setup)`** so any choice stays coherent (no manual desync). `command_config` reads `cmd_input_file` as today. |
| **run env** | capture `setup.env` (plain) at run-start; pass as a thread/`Popen` arg to **both** `start_run` and Quick Run; `run_env.update(env_extra)` **immediately before `Popen`**, after all reassignments. `env_extra=None` default (no mutable default). |
| **box count** | a `reactive.effect` on `current_setup` calls `ui.update_*` on the box selectors (`run_control.py:198`, `input_files.py:95`) and drives the **output-config writer** loop (`run_control.py:631`) via `current_setup().box_count`; the writer's target file uses `setup.inputs_dir` not literal `INPUTS`. |
| **output dir** (primary) | dashboard, `plot`, diagnostics resolve/seed their output-dir from `current_setup.output_dir` (their existing dropdowns `update_select`-ed to it); the run-progress tracker gets `setup.output_dir` (or `setup.input_file`) passed into `start_run`/Quick Run → `get_output_files_info(input_txt_path=…)`. |
| **inputs dir** (primary) | the input browser reads `current_setup.inputs_dir`. |

### 4. Availability & guards

- The selector lists both; an **unavailable** setup is disabled with its `unavailable_hint`
  (checked via `is_available` on render + when the panel opens). Standard is always available.
- **Guards for the deferred surface (so nothing is wrong or destructive under CL29):**
  - **Scenarios: disabled when a non-standard setup is active** (they read *and write* `INPUTS/`;
    a scenario apply under CL29 would corrupt Standard inputs). Show "Scenario editing is available
    for the Standard model only."
  - **Secondary parametric viewers** (parameters, initial-conditions, model-options) and the
    **box-network map** keep reading `INPUTS/`/25-box geometry but show a one-line notice when a
    non-standard setup is loaded: "Showing Standard-model reference data; CL29-specific view is not
    yet wired." No wrong data presented as CL29's.
  - Constants/shear command scans + copy strings ("25 boxes") are cosmetic/secondary → left with the
    notice or a follow-up; they don't affect a default CL29 run (which uses only the input-file arg).

### 5. Error handling & backward-compat

- Unknown `get_setup(id)` → `default_setup()` (defensive). Unavailable selection is prevented at the
  UI; if inputs vanish before a run, ESTAS's failure surfaces in the run log as today.
- **Backward-compat:** `default_setup()` is Standard — `box_count=25` reproduces every current
  selector, `env={}` makes the additive `update({})` a no-op, `cmd_input_file` already defaults to
  `INPUT.txt`. No-interaction behavior is byte-identical **provided** the env merge is additive
  (never a replacement — that would drop Intel `LD_LIBRARY_PATH`).

## Components / files

**New:** `shiny_app/setups.py`; `tests/python/test_setups.py`.
**Modified (Tier 1):** `run_control.py` (selector UI + `run.current_setup` + box-selector update-effect
+ output-config writer range/path + filtered `cmd_input_file` + env pass-through), `app_state.py`
(`current_setup` placeholder; `start_run(..., env_extra=None, output_dir=None)` merge-before-Popen +
progress tracker), `dashboard.py` (Quick Run env + output-dir).
**Modified (Tier 2 / primary views + guards):** `input_files.py` (inputs-dir + box selector),
`plot.py` (output-dir seed + box cap), `diagnostics.py` (output-dir default), `scenarios.py` (disable
guard), and the secondary-viewer notices (`parameters.py`, `initial_conditions.py`,
`model_options.py`, `box_network.py`).

## Testing

- `tests/python/test_setups.py` (pure): both entries' fields; `get_setup`/`default_setup`/unknown-id
  fallback; `is_available` true (tmp dir with `PELAGIC_INPUTS.txt`) vs false (missing file / empty
  dir); `input_files_for` matching by declared folder.
- A focused `start_run` test: `env_extra={"ESTAS_HOLD_VOLUME":"1"}` ends up in the `Popen` env **after**
  the `get_run_environment()` reassignment (guards the clobber bug) and `env_extra=None`/`{}` is a no-op.
- UI backstop: `import shiny_app.app; create_ui().tagify()` (the suite doesn't import `app.py`).

## Non-goals (explicitly deferred, with guards in §4)

- **No 29-box box-network map** — deferred (would rebuild `BOX_GEOM` from `b29polys.gpkg`); guarded by
  a notice under CL29.
- **No full port of the secondary parametric viewers** (parameters/ICs/model-options) or the
  constants/shear scans to `INPUTS_CL29` — guarded by a notice; can be ported later using the same
  `current_setup.inputs_dir` seam.
- **Scenarios not made per-setup-safe** — disabled under CL29 (write-hazard) rather than ported.
- **`OUTPUT.csv` readers** (observation-compare, mass-balance, plot CSV-preview) are a separate fixed
  artifact, not `OUTPUTS/`, and are out of scope.
- **No in-app converter/"Generate inputs" button**, **no committing `INPUTS_CL29/`**, no Fortran
  changes, no setup beyond the two.
