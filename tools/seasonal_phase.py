#!/usr/bin/env python3
"""Seasonal-phase metrics for model-observation comparison.

Per-season bias cannot express a phase error: a model can carry the right annual
mean and plausible per-season magnitudes while peaking in the wrong month. These
metrics score the shape and timing of the seasonal cycle instead.

Pure functions over {month: value} mappings; no file or model I/O, so they are
usable from the validator, from analysis scripts, and from tests.
"""
from __future__ import annotations

import math

SPRING = (2, 3, 4, 5)
AUTUMN = (8, 9, 10)


def circular_month_offset(month_a: int, month_b: int) -> int:
    """Signed months from ``month_b`` to ``month_a``, taking the shorter way round.

    Positive means ``month_a`` falls after ``month_b``: January minus December is +1,
    not -11. December minus January is -1. The result lies in [-6, 6].
    """
    d = (month_a - month_b) % 12
    if d > 6:
        d -= 12
    return d


def _mean(values):
    values = list(values)
    return sum(values) / len(values) if values else float("nan")


def _pearson(xs, ys):
    mx, my = _mean(xs), _mean(ys)
    sx = math.sqrt(sum((x - mx) ** 2 for x in xs))
    sy = math.sqrt(sum((y - my) ** 2 for y in ys))
    if sx == 0 or sy == 0:
        return float("nan")
    return sum((x - mx) * (y - my) for x, y in zip(xs, ys)) / (sx * sy)


def _season_ratio(by_month: dict, months: set) -> float:
    """Autumn mean over spring mean, restricted to ``months`` (the shared set)."""
    num = _mean(by_month[m] for m in AUTUMN if m in months and m in by_month)
    den = _mean(by_month[m] for m in SPRING if m in months and m in by_month)
    if math.isnan(den) or den == 0:
        return float("nan")
    return num / den


def phase_metrics(model_by_month: dict, obs_by_month: dict) -> dict:
    """Compare the seasonal *shape* of a model series against observations.

    Both inputs map month number (1-12) to a climatological mean. Only months present
    in both are used, for every metric including the season ratios, so the two series
    are always compared over identical support.
    """
    shared = sorted(set(model_by_month) & set(obs_by_month))

    if len(shared) < 3:
        raise ValueError(f"need at least 3 shared months, got {len(shared)}")

    shared_set = set(shared)
    peak_model = max(shared, key=lambda m: model_by_month[m])
    peak_obs = max(shared, key=lambda m: obs_by_month[m])

    return {
        "peak_model": peak_model,
        "peak_obs": peak_obs,
        "peak_offset_months": circular_month_offset(peak_model, peak_obs),
        "autumn_spring_model": _season_ratio(model_by_month, shared_set),
        "autumn_spring_obs": _season_ratio(obs_by_month, shared_set),
        "seasonal_r": _pearson([model_by_month[m] for m in shared],
                               [obs_by_month[m] for m in shared]),
        "n_months": len(shared),
    }


def format_report(metrics: dict) -> str:
    """One-block human-readable summary."""
    return (
        f"  peak month        model {metrics['peak_model']:>2}   "
        f"obs {metrics['peak_obs']:>2}   offset {metrics['peak_offset_months']:+d} months\n"
        f"  autumn/spring     model {metrics['autumn_spring_model']:.2f}   "
        f"obs {metrics['autumn_spring_obs']:.2f}\n"
        f"  seasonal r        {metrics['seasonal_r']:+.2f}   "
        f"(n = {metrics['n_months']} months)"
    )
