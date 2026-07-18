"""Tests for the TODO 2.4 non-blocking I/O helpers.

The mass_balance and observations modules moved their heavy OUTPUT.csv
read+compute into module-level blocking helpers that run in a worker thread (via
``asyncio.to_thread`` inside a ``@reactive.extended_task``) so they no longer block
the Shiny event loop.

The extended_task wiring itself is exercised by the Playwright integration tests
(which load the app and construct the tasks). These unit tests lock the
*return-shape contracts* that the in-server collect effects depend on, and prove the
relocated read+compute still works end-to-end against a real (tiny) OUTPUT.csv.
"""
import ast
import inspect

import pandas as pd

try:
    from shiny_app.modules import mass_balance as mb_module
    from shiny_app.modules import observations as obs_module
    from shiny_app.modules.mass_balance import _compute_mass_balance_blocking
    from shiny_app.modules.observations import (
        _compare_blocking,
        _sample_and_compare_blocking,
    )
except ImportError:  # running from inside shiny_app/
    from modules import mass_balance as mb_module
    from modules import observations as obs_module
    from modules.mass_balance import _compute_mass_balance_blocking
    from modules.observations import _compare_blocking, _sample_and_compare_blocking

# Every state-variable column the MassBalanceCalculator reads, so each element
# (N/C/P/Si) has a nonzero pool total (avoids any divide-by-zero in % change).
_STATE_COLS = [
    "DETC", "DETN", "DETP", "DIAC", "DIC", "DISSOLVED_SILICA", "DOC", "DON", "DOP",
    "FIX_CYNC", "NH4N", "NO3N", "NOFIX_CYNC", "OPA", "PARTICULATE_SILICA", "PO4P",
    "ZOOC", "ZOON", "ZOOP",
]


def _write_min_output_csv(path):
    """A tiny but structurally valid OUTPUT.csv (3 timesteps, all element pools)."""
    data = {"TIME": [0.0, 1.0, 2.0]}
    for i, col in enumerate(_STATE_COLS, start=1):
        # distinct, nonzero, mildly time-varying values
        data[col] = [float(i), float(i) + 0.1, float(i) + 0.2]
    pd.DataFrame(data).to_csv(path, index=False)
    return str(path)


def test_compute_mass_balance_blocking_returns_calc_and_results(tmp_path):
    csv = _write_min_output_csv(tmp_path / "OUTPUT.csv")
    # bogus param path -> loader falls back to DEFAULT_STOICHIOMETRY
    out = _compute_mass_balance_blocking(csv, "/no/such/WCONST_04.txt")

    assert out is not None
    calc, results = out
    # `results` is the dict the collect effect + summary render iterate over
    assert isinstance(results, dict)
    assert set(results) == {"Nitrogen", "Carbon", "Phosphorus", "Silicon"}
    # `calc` exposes the time column used by mass_balance_plot_ui
    assert len(calc.get_time_column()) == 3


def test_compute_mass_balance_blocking_missing_output_returns_none(tmp_path):
    missing = str(tmp_path / "does_not_exist.csv")
    # load_data() fails -> helper returns None (collect effect shows an error)
    assert _compute_mass_balance_blocking(missing, "/no/such/WCONST_04.txt") is None


def test_compare_blocking_missing_output_returns_none_none():
    # Contract the observations collect effect relies on: an absent OUTPUT.csv
    # yields (None, None), and `obs` is never touched (guarded before use).
    assert _compare_blocking("/no/such/OUTPUT.csv", None) == (None, None)


def test_blocking_helpers_are_module_level_callables():
    # The extended_task wiring depends on these being importable module-level
    # functions (not nested closures), so a rename/relocation is caught here.
    for fn in (_compute_mass_balance_blocking, _compare_blocking, _sample_and_compare_blocking):
        assert callable(fn)


def _extended_task_defs(module):
    """Yield the def nodes in `module` decorated with `@reactive.extended_task`."""
    tree = ast.parse(inspect.getsource(module))
    for node in ast.walk(tree):
        if not isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            continue
        for dec in node.decorator_list:
            name = getattr(dec, "attr", None) or getattr(dec, "id", None)
            if name == "extended_task":
                yield node


def test_all_extended_tasks_are_async():
    # A sync function passed to @reactive.extended_task raises TypeError when the
    # module server constructs it (on session connect). This static check locks the
    # invariant deterministically, without needing a live Shiny session.
    found = 0
    for module in (mb_module, obs_module):
        for node in _extended_task_defs(module):
            found += 1
            assert isinstance(node, ast.AsyncFunctionDef), (
                f"extended_task target {node.name!r} in {module.__name__} must be "
                "'async def' (a sync function raises TypeError at construction)"
            )
    assert found == 3, f"expected 3 @reactive.extended_task definitions, found {found}"
