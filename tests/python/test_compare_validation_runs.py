# tests/python/test_compare_validation_runs.py
import csv
import subprocess
import sys
from pathlib import Path

TOOL = Path(__file__).resolve().parents[2] / "tools" / "compare_validation_runs.py"


def _write(path, rows):
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        # EXACT header validate_cl29_vs_epa.py:write_metrics_csv emits — column is "variable"
        w.writerow(["box", "variable", "n", "obs_mean", "model_mean", "bias", "rmse", "r"])
        for r in rows:
            w.writerow(r)


def rows_to_csv(tmp_path, rows):
    p = tmp_path / "m.csv"
    _write(p, rows)
    return str(p)


def _run(base, prom, guard):
    return subprocess.run([sys.executable, str(TOOL), str(base), str(prom),
                           "--no-regress", guard, "--max-rise", "5"],
                          capture_output=True, text=True)


def test_aggregate_obs_weighted(tmp_path):
    sys.path.insert(0, str(TOOL.parent))
    import compare_validation_runs as c
    # two boxes of PO4: n-weighted RMSE = sqrt((3²·10 + 1²·30)/40) = sqrt(120/40)=sqrt3
    rows = [["1", "PO4", "10", "0", "0", "0.5", "3", "0.9"],
            ["2", "PO4", "30", "0", "0", "0.1", "1", "0.9"]]
    agg = c.aggregate(c.read_metrics(rows_to_csv(tmp_path, rows)))
    assert abs(agg["PO4"]["rmse"] - (120 / 40) ** 0.5) < 1e-9
    assert abs(agg["PO4"]["bias"] - (0.5 * 10 + 0.1 * 30) / 40) < 1e-9


def test_guard_fails_on_rmse_regression(tmp_path):
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.0", "1.00", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.0", "1.20", "0.9"]])  # +20% RMSE
    r = _run(base, prom, "NH4")
    assert r.returncode != 0
    assert "NH4" in r.stdout


def test_guard_passes_within_tolerance(tmp_path):
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.0", "1.00", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.0", "1.03", "0.9"]])  # +3%
    assert _run(base, prom, "NH4").returncode == 0


def test_guard_fails_on_bias_growth(tmp_path):
    # RMSE flat, but |bias| grows 15x — a real directional error the RMSE guard misses
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.002", "1.00", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.031", "1.00", "0.9"]])
    assert _run(base, prom, "NH4").returncode != 0


def test_zero_baseline_rmse_is_regression(tmp_path):
    # perfect-fit baseline (RMSE 0) -> positive RMSE must be flagged, not treated as 0%
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.0", "0.0", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.5", "0.5", "0.9"]])
    assert _run(base, prom, "NH4").returncode != 0


def test_guard_case_insensitive(tmp_path):
    # guard is upper-cased internally; a mixed-case CSV var like "Si" must still match "Si" guard
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "Si", "10", "0", "0", "0.0", "1.0", "0.9"]])
    _write(prom, [["1", "Si", "10", "0", "0", "0.0", "5.0", "0.9"]])
    r = _run(base, prom, "Si")
    assert r.returncode != 0
    assert "Si" in r.stdout


def test_zero_baseline_bias_is_regression(tmp_path):
    # RMSE flat, but bias appears from ~0 (0.0 -> 0.5) with no sign flip — must still flag
    base, prom = tmp_path / "b.csv", tmp_path / "p.csv"
    _write(base, [["1", "NH4", "10", "0", "0", "0.0", "1.0", "0.9"]])
    _write(prom, [["1", "NH4", "10", "0", "0", "0.5", "1.0", "0.9"]])
    assert _run(base, prom, "NH4").returncode != 0


def test_help_does_not_crash(tmp_path):
    # the "(%)" help string previously crashed argparse's HelpFormatter on --help
    r = subprocess.run([sys.executable, str(TOOL), "--help"], capture_output=True, text=True)
    assert r.returncode == 0
    assert "usage" in r.stdout.lower()
