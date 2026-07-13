# Design: `shiny_app/app.py` decomposition — phase 3, box-network cluster

- **Date:** 2026-07-13
- **Status:** Draft (awaiting user review)
- **Author:** Arturas Razinkovas-Baziukas (with Claude)
- **Scope:** `shiny_app/`. Extract the box-network **input parsers** and **figure builders** from
  `server()` into a new module `shiny_app/box_network.py`. **No change to the reactive graph or
  rendered output.**
- **Predecessors:** phase 1 (helpers, v0.3.3), phase 2 (`create_ui()`, v0.3.4), phase 3 **pilot**
  (build cluster → `build_commands.py`, v0.3.5). This is the second phase-3 cluster.

## 1. Context & motivation

`server()` (~5,900 lines) still holds several non-reactive clusters. The **box-network** cluster is
six nested functions (app.py 1945–2543, ~600 lines) that parse the box-model input files and build
the Map-Display plotly figures. Unlike the build cluster, **none of the six reads `input.*`** — the
three parsers read fixed files under `INPUTS_DIR`; the three figure builders are already fully
parameterized (they take parsed data). So this is a near-pure relocation with no reactive graph to
preserve.

## 2. Goal / non-goals

- **Goal:** move the six functions into `shiny_app/box_network.py` as public module-level functions;
  update their call sites in the two render handlers to call the module; delete the nested defs.
- **Non-goals:** the reactive handlers themselves (`map_display_plot`, `map_display_info` stay —
  only their calls change); other `server()` clusters; the full Shiny-modules rearchitecture.
- **Invariant:** the Map-Display views render identically. Behavioral (no byte oracle) — guarded by
  unit tests on the parsers + figure smoke tests + CI Playwright.

## 3. Approach — direct call-site update (no wrappers)

The pilot kept thin same-named wrappers because its helpers had 15 call sites across many handlers.
Here there are only **7 call sites in 2 handlers**, so the cleaner move is to **update the call
sites directly** to `box_network.<fn>(...)` and delete the six nested defs — no forwarding wrappers.
The three parsers gain an explicit `inputs_dir` parameter (replacing the closed-over module-level
`INPUTS_DIR`), which the call sites pass as `INPUTS_DIR`; the three figure builders keep their
current signatures (already parameterized). `box_network.py` imports stdlib + `plotly.graph_objects`
and nothing from `app.py` (no circular import). Import via the module form
`try: from shiny_app import box_network / except ImportError: import box_network`.

## 4. The six functions → `shiny_app/box_network.py`

Module header: `"""Box-network input parsing + Map-Display figures (extracted from server())."""`,
then `import os`, `import logging`, `import plotly.graph_objects as go`, and
`logger = logging.getLogger("AQUABC")` (the parsers log parse errors via this logger — same named
logger `app.py` uses, so log output is unchanged).

### 4.1 Parsers (pure; `INPUTS_DIR` → `inputs_dir` param)
- `parse_pelagic_inputs(inputs_dir) -> dict` — reads `<inputs_dir>/PELAGIC_INPUTS.txt`; returns
  `{box_no: {ic_set, sediment('Mud'/'Sand'), surface_elevation, bottom_elevation, depth}}`.
  Missing file → `{}`. (was `_parse_pelagic_inputs()`, 1945–1985.)
- `parse_advective_links(inputs_dir) -> list[tuple[int,int]]` — reads
  `<inputs_dir>/ADVECTIVE_LINKS.txt`; returns `[(upstream, downstream), …]`. Missing → `[]`.
  (was `_parse_advective_links()`, 1987–2009.)
- `parse_bathymetry(box_no, inputs_dir) -> list[dict]` — reads
  `<inputs_dir>/BATHYMETRY_{box_no}.txt`; returns layer dicts
  `{layer_no, upper_elevation, lower_elevation, upper_area, lower_area, upper_length, lower_length}`.
  Missing → `[]`. (was `_parse_bathymetry(box_no)`, 2011–2051.)

Each is a **verbatim body move** with the single edit `INPUTS_DIR` → the `inputs_dir` parameter; the
per-line `try/except (ValueError, IndexError): pass` and outer `except … logger.error(...)` are kept
exactly (they are parse-robustness, not reactive guards).

### 4.2 Figure builders (already parameterized; verbatim moves)
- `build_box_network_figure(boxes, links) -> go.Figure` (was `_build_box_network_figure`, 2053–2421).
- `build_bathymetry_figure(box_no, layers, boxes) -> go.Figure` (was `_build_bathymetry_figure`,
  2422–2481).
- `build_depths_overview(boxes) -> go.Figure` (was `_build_depths_overview`, 2482–2543; note line
  2530 only *opens* `fig.update_layout(...)` — the call runs to 2542 and `return fig` is at 2543).

These are **verbatim moves, no signature change**. Their hardcoded geometry/style constants
(`BOX_GEOM`, `BOUNDARY_EDGES`, `BND_CLR`, `BND_W`) are **function-locals** (defined inside
`_build_box_network_figure` at 2075/2115/2236/2237), so they travel with the body — no module-level
constant to relocate. Their only free names are `go` and their parameters (to be confirmed by the
review's exhaustive free-name sweep — the design assumes `go` + params + locals only, with no
`np`/`pd`/`math`/`logger`/`INPUTS_DIR` reference, consistent with the grep).

## 5. Call-site wiring (2 handlers, 7 sites)

`map_display_plot` (`@render_widget`, ~2545) and `map_display_info` (`@render.ui`, ~2586) change
only these calls (nothing else in the handlers):

| Was | Becomes |
|---|---|
| `_parse_pelagic_inputs()` (2551, 2586) | `box_network.parse_pelagic_inputs(INPUTS_DIR)` |
| `_parse_advective_links()` (2553) | `box_network.parse_advective_links(INPUTS_DIR)` |
| `_parse_bathymetry(box_no)` (2556) | `box_network.parse_bathymetry(box_no, INPUTS_DIR)` |
| `_build_box_network_figure(boxes, links)` (2554) | `box_network.build_box_network_figure(boxes, links)` |
| `_build_bathymetry_figure(box_no, layers, boxes)` (2557) | `box_network.build_bathymetry_figure(box_no, layers, boxes)` |
| `_build_depths_overview(boxes)` (2559) | `box_network.build_depths_overview(boxes)` |

Add the module-import after the `build_commands` re-import block. Delete the six nested defs
(**1945–2543** — the whole run of defs ending at `_build_depths_overview`'s `return fig` on 2543,
immediately before the blank line and `@render_widget` at 2545; deleting only to 2530 would orphan
the tail of `update_layout(...)` and break syntax). `INPUTS_DIR` (module const, app.py:239) and the
two handlers otherwise stay.

## 6. Validation gate

1. `python -m py_compile shiny_app/app.py`; `python -c "import shiny_app.box_network"`.
2. `ruff check --select F821 shiny_app/app.py shiny_app/box_network.py` → clean (lint the new module
   too — it must import `os`/`logging`/`plotly.graph_objects`; F821 catches a forgotten one).
3. Unit tests `tests/python/test_box_network.py` (in-process — plotly imports cleanly, verified):
   - **Parsers (the valuable part):** write fixture `PELAGIC_INPUTS.txt` / `ADVECTIVE_LINKS.txt` /
     `BATHYMETRY_5.txt` into a `tmp_path`, assert the exact parsed structures; assert missing-file →
     `{}`/`[]`; assert a malformed row is skipped (the `except: pass` path).
   - **Figures (smoke):** with small representative `boxes`/`links`/`layers`, assert each builder
     returns a `plotly.graph_objects.Figure` without raising and has ≥1 trace. Do not pin pixel/layout
     details.
4. Full Python suite green (138 baseline + new tests; no regression).
5. Playwright is CI-only (not local) — do not claim it locally.

## 7. Risks & mitigations

| Risk | Mitigation |
|---|---|
| A figure builder references a module-level name not captured (would `NameError`) | Review does an exhaustive free-name sweep of all 6 bodies; the constants are verified locals; `import shiny_app.box_network` + F821 on the new file + the figure smoke tests catch a miss |
| Parser behavior drift | Bodies moved verbatim (only `INPUTS_DIR`→param); unit tests pin exact parsed structures against fixtures |
| A call site missed or passed wrong args | Only 7 sites in 2 handlers (§5 table); `F821`/`py_compile`; the render handlers exercised by CI Playwright |
| Circular import | `box_network.py` imports stdlib + `plotly` only; never `app`; parsers take `inputs_dir` as arg |
| `logger` name mismatch changes log routing | Module defines `logging.getLogger("AQUABC")` — the same named logger `app.py` uses |

## 8. Deferred roadmap (after this cluster)

- Remaining phase-3 clusters: output-data/plot helpers, mass-balance, observations, scenarios, the
  second inline command-logic copy (~628–667), `_execute_build_process` machinery.
- Full Shiny-modules rearchitecture — terminal item.

## 9. Files touched

- **New:** `shiny_app/box_network.py`; `tests/python/test_box_network.py`.
- **Modified:** `shiny_app/app.py` (delete 6 nested defs; add re-import; update 7 call sites);
  `TODO_IMPLEMENTATION_PLAN.md`.
- **Out of scope:** the reactive handlers' bodies (beyond the 7 calls), any `.f90`.
