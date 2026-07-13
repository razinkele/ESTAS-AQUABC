# app.py Decomposition Phase 3 — Box-Network Cluster — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the six non-reactive box-network functions (3 input parsers + 3 plotly figure builders) from `shiny_app/app.py`'s `server()` into a new module `shiny_app/box_network.py`; update their 7 call sites in the two Map-Display render handlers; delete the nested defs. Rendered output unchanged.

**Architecture:** The parsers become pure functions taking `inputs_dir` (was the closed-over `INPUTS_DIR`); the figure builders move verbatim (already parameterized). `box_network.py` imports stdlib + `plotly.graph_objects` and nothing from `app.py`. Spec: `docs/superpowers/specs/2026-07-13-app-py-decomposition-phase3-boxnetwork-design.md`.

**Tech Stack:** Python 3.10+, Shiny for Python, plotly, pytest, ruff.

## Global Constraints

- **Behavior-preserving, not byte-identical.** Verified by unit tests on the parsers + figure smoke tests + CI Playwright. The moved bodies must be logically identical to the originals.
- **Parsers:** verbatim body move with the single change `INPUTS_DIR` → the new `inputs_dir` parameter; keep the per-line `try/except (ValueError, IndexError): pass` and outer `except … logger.error(...)` exactly.
- **Figures:** verbatim body move, unchanged signatures; their constants (`BOX_GEOM`, `BOUNDARY_EDGES`, `BND_CLR`, `BND_W`) are function-locals and travel with the body.
- **`box_network.py` imports:** `os`, `logging`, `plotly.graph_objects as go`; defines `logger = logging.getLogger("AQUABC")`. No `shiny`, no `app` import.
- **Module-import form** in `app.py`: `try: from shiny_app import box_network / except ImportError: import box_network`; call `box_network.<fn>(...)`.
- **Delete range is 1945–2543** (the whole run of six defs; `_build_depths_overview`'s `return fig` is at 2543 — deleting only to 2530 orphans the tail of `update_layout(...)` → SyntaxError).
- **Gate (after each task):** `py_compile shiny_app/app.py`; `python -c "import shiny_app.box_network"`; `ruff check --select F821 shiny_app/app.py shiny_app/box_network.py`; `python -m pytest tests/python -q` green. Playwright is CI-only.

---

### Task 1: Create `shiny_app/box_network.py` + unit tests

**Files:**
- Create: `shiny_app/box_network.py`
- Create: `tests/python/test_box_network.py`

**Interfaces:**
- Produces: `parse_pelagic_inputs(inputs_dir) -> dict`, `parse_advective_links(inputs_dir) -> list`, `parse_bathymetry(box_no, inputs_dir) -> list`, `build_box_network_figure(boxes, links) -> go.Figure`, `build_bathymetry_figure(box_no, layers, boxes) -> go.Figure`, `build_depths_overview(boxes) -> go.Figure`.
- Consumes: stdlib + plotly.

`app.py` is **untouched** this task; the app keeps running on its nested defs.

- [ ] **Step 1: Write the failing tests** — `tests/python/test_box_network.py`:

```python
import plotly.graph_objects as go
from shiny_app.box_network import (
    parse_pelagic_inputs, parse_advective_links, parse_bathymetry,
    build_box_network_figure, build_bathymetry_figure, build_depths_overview,
)


def _write(p, text):
    p.write_text(text)


def test_parse_pelagic_inputs(tmp_path):
    _write(tmp_path / "PELAGIC_INPUTS.txt",
           "# header\n# INITIAL CONDITIONS\n1 1 0.0 -5.0\n2 2 1.0 -2.0\n# MASS LOADS\n9 9 0 0\n")
    boxes = parse_pelagic_inputs(str(tmp_path))
    assert set(boxes) == {1, 2}
    assert boxes[1] == {"ic_set": 1, "sediment": "Mud", "surface_elevation": 0.0,
                        "bottom_elevation": -5.0, "depth": 5.0}
    assert boxes[2]["sediment"] == "Sand" and boxes[2]["depth"] == 3.0


def test_parse_pelagic_inputs_missing(tmp_path):
    assert parse_pelagic_inputs(str(tmp_path)) == {}


def test_parse_pelagic_inputs_skips_malformed(tmp_path):
    _write(tmp_path / "PELAGIC_INPUTS.txt",
           "# INITIAL CONDITIONS\nBAD ROW HERE X\n3 1 0.0 -4.0\n# MASS LOADS\n")
    boxes = parse_pelagic_inputs(str(tmp_path))
    assert set(boxes) == {3}          # 'BAD ROW HERE X' has 4 parts -> int('BAD') raises -> except path -> skipped


def test_parse_advective_links(tmp_path):
    _write(tmp_path / "ADVECTIVE_LINKS.txt", "# links\n0 1 2\n1 2 3\n\n")
    assert parse_advective_links(str(tmp_path)) == [(1, 2), (2, 3)]


def test_parse_advective_links_missing(tmp_path):
    assert parse_advective_links(str(tmp_path)) == []


def test_parse_bathymetry(tmp_path):
    _write(tmp_path / "BATHYMETRY_5.txt",
           "BATHYMETRY BOX 5\nNUM_LAYERS\n2\n"
           "layer up_el low_el up_a low_a up_l low_l\n"
           "1 0.0 -1.0 100.0 90.0 10.0 9.0\n2 -1.0 -2.0 90.0 80.0 9.0 8.0\n")
    layers = parse_bathymetry(5, str(tmp_path))
    assert len(layers) == 2
    assert layers[0] == {"layer_no": 1, "upper_elevation": 0.0, "lower_elevation": -1.0,
                         "upper_area": 100.0, "lower_area": 90.0, "upper_length": 10.0,
                         "lower_length": 9.0}


def test_parse_bathymetry_missing(tmp_path):
    assert parse_bathymetry(99, str(tmp_path)) == []


# --- figure smoke tests: build without raising, return a Figure with >=1 trace ---
_BOXES = {1: {"ic_set": 1, "sediment": "Mud", "surface_elevation": 0.0,
              "bottom_elevation": -5.0, "depth": 5.0},
          2: {"ic_set": 2, "sediment": "Sand", "surface_elevation": 1.0,
              "bottom_elevation": -2.0, "depth": 3.0}}
_LINKS = [(1, 2)]
_LAYERS = [{"layer_no": 1, "upper_elevation": 0.0, "lower_elevation": -1.0,
            "upper_area": 100.0, "lower_area": 90.0, "upper_length": 10.0, "lower_length": 9.0}]


def test_build_box_network_figure_smoke():
    fig = build_box_network_figure(_BOXES, _LINKS)
    assert isinstance(fig, go.Figure) and len(fig.data) >= 1


def test_build_bathymetry_figure_smoke():
    fig = build_bathymetry_figure(1, _LAYERS, _BOXES)
    assert isinstance(fig, go.Figure) and len(fig.data) >= 1


def test_build_depths_overview_smoke():
    fig = build_depths_overview(_BOXES)
    assert isinstance(fig, go.Figure) and len(fig.data) >= 1
```

- [ ] **Step 2: Run tests — verify they fail**

Run: `python -m pytest tests/python/test_box_network.py -q`
Expected: FAIL — `ModuleNotFoundError: No module named 'shiny_app.box_network'`

- [ ] **Step 3: Create `shiny_app/box_network.py`.** Header + logger, the 3 parsers (given below — verbatim from app.py 1945–2051 with `INPUTS_DIR` → the `inputs_dir` parameter and the underscore dropped from the name), and the 3 figure builders (MOVE VERBATIM from the current `app.py`: `_build_box_network_figure` body app.py 2053–2421, `_build_bathymetry_figure` 2422–2481, `_build_depths_overview` 2482–2543 — drop the leading underscore, keep signatures and every body line unchanged including the local `BOX_GEOM`/`BOUNDARY_EDGES`/`BND_CLR`/`BND_W`).

```python
"""Box-network input parsing + Map-Display figures (extracted from server())."""
import os
import logging
import plotly.graph_objects as go

logger = logging.getLogger("AQUABC")


def parse_pelagic_inputs(inputs_dir):
    """Parse PELAGIC_INPUTS.txt for box data: depths, sediment types, basin mapping."""
    path = os.path.join(inputs_dir, "PELAGIC_INPUTS.txt")
    boxes = {}
    if not os.path.isfile(path):
        return boxes
    try:
        with open(path, 'r') as fh:
            lines = fh.readlines()
        in_ic = False
        for line in lines:
            stripped = line.strip()
            if "INITIAL CONDITIONS" in stripped:
                in_ic = True
                continue
            if in_ic and stripped.startswith("#"):
                if "MASS LOADS" in stripped:
                    break
                continue
            if in_ic and stripped:
                parts = stripped.split()
                if len(parts) >= 4:
                    try:
                        box_no = int(parts[0])
                        ic_set = int(parts[1])    # 1=Mud, 2=Sand
                        surf_elev = float(parts[2])
                        bot_elev = float(parts[3])
                        boxes[box_no] = {
                            'ic_set': ic_set,
                            'sediment': 'Mud' if ic_set == 1 else 'Sand',
                            'surface_elevation': surf_elev,
                            'bottom_elevation': bot_elev,
                            'depth': abs(bot_elev - surf_elev),
                        }
                    except (ValueError, IndexError):
                        pass
    except Exception as e:
        logger.error(f"Error parsing PELAGIC_INPUTS.txt: {e}")
    return boxes


def parse_advective_links(inputs_dir):
    """Parse ADVECTIVE_LINKS.txt for box connectivity."""
    path = os.path.join(inputs_dir, "ADVECTIVE_LINKS.txt")
    links = []
    if not os.path.isfile(path):
        return links
    try:
        with open(path, 'r') as fh:
            for line in fh:
                stripped = line.strip()
                if stripped.startswith("#") or not stripped:
                    continue
                parts = stripped.split()
                if len(parts) >= 3:
                    try:
                        upstream = int(parts[1])
                        downstream = int(parts[2])
                        links.append((upstream, downstream))
                    except (ValueError, IndexError):
                        pass
    except Exception as e:
        logger.error(f"Error parsing ADVECTIVE_LINKS.txt: {e}")
    return links


def parse_bathymetry(box_no, inputs_dir):
    """Parse BATHYMETRY_{box_no}.txt for layer data."""
    path = os.path.join(inputs_dir, f"BATHYMETRY_{box_no}.txt")
    layers = []
    if not os.path.isfile(path):
        return layers
    try:
        with open(path, 'r') as fh:
            lines = fh.readlines()
        data_start = None
        for i, line in enumerate(lines):
            stripped = line.strip()
            parts = stripped.split()
            if len(parts) >= 7:
                try:
                    int(parts[0])
                    float(parts[1])
                    data_start = i
                    break
                except ValueError:
                    continue
        if data_start is not None:
            for line in lines[data_start:]:
                parts = line.strip().split()
                if len(parts) >= 7:
                    try:
                        layers.append({
                            'layer_no': int(parts[0]),
                            'upper_elevation': float(parts[1]),
                            'lower_elevation': float(parts[2]),
                            'upper_area': float(parts[3]),
                            'lower_area': float(parts[4]),
                            'upper_length': float(parts[5]),
                            'lower_length': float(parts[6]),
                        })
                    except (ValueError, IndexError):
                        pass
    except Exception as e:
        logger.error(f"Error parsing BATHYMETRY_{box_no}.txt: {e}")
    return layers


def build_box_network_figure(boxes, links):
    # MOVE VERBATIM from app.py `_build_box_network_figure` body (2053-2421). Keep every line,
    # including the local BOX_GEOM / BOUNDARY_EDGES / BND_CLR / BND_W. Signature unchanged.
    ...


def build_bathymetry_figure(box_no, layers, boxes):
    # MOVE VERBATIM from app.py `_build_bathymetry_figure` body (2422-2481).
    ...


def build_depths_overview(boxes):
    # MOVE VERBATIM from app.py `_build_depths_overview` body (2482-2543, through `return fig`).
    ...
```

- [ ] **Step 4: Run tests — verify they pass**

Run: `python -m pytest tests/python/test_box_network.py -q`
Expected: PASS (all). If a figure smoke test fails, the verbatim move dropped/changed a line — fix the move, do not weaken the test.

- [ ] **Step 5: Gate + commit**

```bash
python -c "import shiny_app.box_network"
ruff check --select F821 shiny_app/box_network.py
python -m pytest tests/python -q     # 138 baseline + new tests, no regressions
git add shiny_app/box_network.py tests/python/test_box_network.py
git commit -m "feat(shiny): add box_network module (parsers + figures) + tests (phase 3, task 1)"
```

---

### Task 2: Wire `server()` — delete nested defs, update call sites

**Files:**
- Modify: `shiny_app/app.py` (add re-import; delete 6 nested defs 1945–2543; update 7 call sites)
- Modify: `TODO_IMPLEMENTATION_PLAN.md`

- [ ] **Step 1: Add the re-import** after the `build_commands` re-import block in `app.py`:

```python
try:
    from shiny_app import box_network
except ImportError:
    import box_network
```

- [ ] **Step 2: Update the 7 call sites** in `map_display_plot` (~2545) and `map_display_info` (~2586) — change only these calls, nothing else in the handlers:

| Line | Was | Becomes |
|---|---|---|
| 2551 | `boxes = _parse_pelagic_inputs()` | `boxes = box_network.parse_pelagic_inputs(INPUTS_DIR)` |
| 2553 | `links = _parse_advective_links()` | `links = box_network.parse_advective_links(INPUTS_DIR)` |
| 2554 | `fig = _build_box_network_figure(boxes, links)` | `fig = box_network.build_box_network_figure(boxes, links)` |
| 2556 | `layers = _parse_bathymetry(box_no)` | `layers = box_network.parse_bathymetry(box_no, INPUTS_DIR)` |
| 2557 | `fig = _build_bathymetry_figure(box_no, layers, boxes)` | `fig = box_network.build_bathymetry_figure(box_no, layers, boxes)` |
| 2559 | `fig = _build_depths_overview(boxes)` | `fig = box_network.build_depths_overview(boxes)` |
| 2586 | `boxes = _parse_pelagic_inputs()` | `boxes = box_network.parse_pelagic_inputs(INPUTS_DIR)` |

(Line numbers are pre-deletion; do the call-site edits first, or account for the shift. Grep `_parse_pelagic_inputs\|_parse_advective_links\|_parse_bathymetry\|_build_box_network_figure\|_build_bathymetry_figure\|_build_depths_overview` afterward to confirm ZERO remaining references before deleting the defs.)

- [ ] **Step 3: Delete the six nested defs** — the contiguous block `def _parse_pelagic_inputs()` through `_build_depths_overview()`'s `return fig` (app.py **1945–2543**, ending just before the blank line and `@render_widget` at 2545). Verify with a grep that none of the six names remain anywhere in `app.py` except as the `box_network.<fn>` calls from Step 2.

- [ ] **Step 4: Gate**

```bash
python -m py_compile shiny_app/app.py
python -c "import shiny_app.box_network"
ruff check --select F821 shiny_app/app.py shiny_app/box_network.py
python -m pytest tests/python -q
```
Expected: all pass; F821 clean; suite green (no new tests here). Do not run Playwright locally.

- [ ] **Step 5: Update `TODO_IMPLEMENTATION_PLAN.md`** — mark the box-network cluster done; note remaining phase-3 clusters (output-data/plot helpers, mass-balance, observations, scenarios, the inline command-logic copy ~628–667, `_execute_build_process`) as deferred.

- [ ] **Step 6: Commit**

```bash
git add shiny_app/app.py TODO_IMPLEMENTATION_PLAN.md
git commit -m "refactor(shiny): Map-Display handlers delegate to box_network; drop 6 nested defs (phase 3, task 2)"
```

---

## Final verification (after both tasks)

- [ ] `pytest tests/python -q` green; `ruff check --select F821 shiny_app/app.py shiny_app/box_network.py` clean.
- [ ] Grep confirms the six old names appear ONLY as `box_network.<fn>` calls (7 sites) in app.py; the nested defs are gone.
- [ ] Manual equivalence check: parsers moved verbatim except `INPUTS_DIR`→param; figures byte-identical bodies; the two render handlers otherwise unchanged.
- [ ] Broad whole-branch review before merge.

## Notes

The three figure builders (~490 lines total, incl. a 368-line function) are **verbatim moves** — the implementer copies the actual bodies from the current `app.py`; the plan does not reproduce them (retyping risks drift). The three parsers are reproduced above because they carry the one intended edit (`INPUTS_DIR`→`inputs_dir`). The review verifies the figure bodies are byte-identical to the originals.
