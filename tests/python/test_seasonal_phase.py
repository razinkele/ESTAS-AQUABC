"""Unit tests for tools/seasonal_phase.py (pure functions, no I/O)."""
import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
from seasonal_phase import circular_month_offset, phase_metrics  # noqa: E402


def test_circular_month_offset_takes_the_short_way_round():
    # positive means the first month falls AFTER the second
    assert circular_month_offset(2, 8) == 6      # Feb is 6 months either way from Aug
    assert circular_month_offset(1, 12) == 1     # January is one month after December
    assert circular_month_offset(12, 1) == -1    # December is one month before January
    assert circular_month_offset(9, 9) == 0


def test_phase_metrics_detects_an_inverted_cycle():
    obs = dict(zip(range(1, 13), [10, 10, 20, 30, 30, 25, 30, 50, 48, 45, 25, 12]))
    model = dict(zip(range(1, 13), [50, 47, 42, 40, 42, 29, 27, 30, 22, 24, 27, 32]))
    r = phase_metrics(model, obs)
    assert r["peak_obs"] == 8
    assert r["peak_model"] == 1
    assert abs(r["peak_offset_months"]) >= 5
    assert r["seasonal_r"] < 0
    assert r["autumn_spring_obs"] > 1.0
    assert r["autumn_spring_model"] < 1.0
    assert r["n_months"] == 12


def test_phase_metrics_detects_a_matching_cycle():
    obs = dict(zip(range(1, 13), [10, 10, 20, 30, 30, 25, 30, 50, 48, 45, 25, 12]))
    model = {m: v * 1.2 for m, v in obs.items()}
    r = phase_metrics(model, obs)
    assert r["peak_offset_months"] == 0
    assert r["seasonal_r"] > 0.99
    assert r["autumn_spring_model"] == pytest.approx(r["autumn_spring_obs"])


def test_phase_metrics_uses_only_shared_months():
    obs = {3: 20.0, 4: 30.0, 8: 50.0, 9: 48.0}
    model = {m: float(m) for m in range(1, 13)}
    r = phase_metrics(model, obs)
    assert r["n_months"] == 4


def test_season_ratio_ignores_months_absent_from_the_other_series():
    # only Feb, Aug and Sep are shared; the ratio must not silently use model-only months
    obs = {2: 10.0, 8: 40.0, 9: 30.0}
    model = {m: 100.0 for m in range(1, 13)}
    r = phase_metrics(model, obs)
    assert r["autumn_spring_model"] == pytest.approx(1.0)   # flat model over shared months
    assert r["autumn_spring_obs"] == pytest.approx(3.5)     # (40+30)/2 / 10


def test_phase_metrics_needs_at_least_three_shared_months():
    with pytest.raises(ValueError, match="at least 3"):
        phase_metrics({1: 1.0, 2: 2.0}, {1: 1.0, 2: 2.0})
