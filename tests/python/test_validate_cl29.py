"""Tests for tools/validate_cl29_vs_epa.py.

Covers the pure logic — box-number parsing, the TIME_DAYS -> calendar-date mapping,
tidy-CSV obs loading (filtered to the direct-comparison variables), and the
interpolate-to-obs-dates fit metrics (bias / RMSE, and the drop of obs outside the
model window) — on synthetic frames, so it runs in CI without a model run.
"""
import datetime as dt
import os
import sys

import pandas as pd
import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))

import validate_cl29_vs_epa as val  # noqa: E402


def test_box_number():
    assert val.box_number("OUTPUTS_CL29/PELAGIC_BOX_00007.out") == 7
    assert val.box_number("/a/b/PELAGIC_BOX_00025.out") == 25
    assert val.box_number("not_a_box.txt") is None


def test_load_box_output_maps_time_to_date(tmp_path):
    f = tmp_path / "PELAGIC_BOX_00007.out"
    f.write_text("TIME_DAYS NH4_N NO3_N\n0.0 0.1 0.5\n10.0 0.2 0.6\n")
    df = val.load_box_output(str(f), 2012)
    assert df["date"].iloc[0] == dt.date(2012, 1, 1)
    assert df["date"].iloc[1] == dt.date(2012, 1, 11)  # base + 10 days


def test_load_obs_filters_to_direct_vars(tmp_path):
    csv = tmp_path / "tidy.csv"
    csv.write_text(
        "station,box,region,date,variable,model_index,value,units,"
        "source_sheet,orig_column,orig_unit,orig_basis\n"
        "LTK1,7,Strait,2013-06-01,NO3,2,0.4,mg N/L,s,c,mg,element\n"
        "LTK1,7,Strait,2013-07-01,NO3,2,0.5,mg N/L,s,c,mg,element\n"
        "LTK1,7,Strait,2013-06-01,DIN,,0.5,mg N/L,s,c,mg,element\n"  # not comparable
    )
    obs = val.load_obs(str(csv))
    assert set(obs) == {(7, "NO3")}          # DIN dropped (no direct model var)
    assert list(obs[(7, "NO3")]["value"]) == [0.4, 0.5]


def _model_df(base_year=2012, days=100, value=1.0):
    times = list(range(days + 1))
    base = dt.date(base_year, 1, 1)
    return pd.DataFrame({
        "TIME_DAYS": [float(t) for t in times],
        "NH4_N": [value] * len(times),
        "date": [base + dt.timedelta(days=t) for t in times],
    })


def test_metrics_bias_and_rmse():
    model = _model_df(value=1.0)  # constant model = 1.0 over 2012 days 0..100
    obs = pd.DataFrame({"date": [dt.date(2012, 1, 21), dt.date(2012, 2, 10)],
                        "value": [0.5, 1.5]})  # residuals +0.5, -0.5
    m = val.metrics(model, obs, "NH4_N")
    assert m["n"] == 2
    assert m["bias"] == pytest.approx(0.0)          # (+0.5 - 0.5)/2
    assert m["rmse"] == pytest.approx(0.5)
    assert m["model_mean"] == pytest.approx(1.0)


def test_metrics_drops_obs_outside_window():
    model = _model_df(days=100, value=1.0)
    obs = pd.DataFrame({
        "date": [dt.date(2012, 2, 10), dt.date(2015, 1, 1)],  # 2nd is past day 100
        "value": [0.5, 99.0]})
    m = val.metrics(model, obs, "NH4_N")
    assert m["n"] == 1                              # out-of-window obs excluded
    assert m["bias"] == pytest.approx(0.5)


def test_metrics_interpolates_between_model_points():
    base = dt.date(2012, 1, 1)
    model = pd.DataFrame({
        "TIME_DAYS": [0.0, 10.0],
        "NH4_N": [0.0, 10.0],                        # linear 0 -> 10 over 10 days
        "date": [base, base + dt.timedelta(days=10)]})
    obs = pd.DataFrame({"date": [base + dt.timedelta(days=5)], "value": [5.0]})
    m = val.metrics(model, obs, "NH4_N")
    assert m["model_mean"] == pytest.approx(5.0)     # interpolated midpoint
    assert m["bias"] == pytest.approx(0.0)


def test_metrics_returns_none_when_no_overlap():
    model = _model_df(days=100)
    obs = pd.DataFrame({"date": [dt.date(2020, 1, 1)], "value": [1.0]})
    assert val.metrics(model, obs, "NH4_N") is None


def test_add_derived_totals_and_chla():
    df = pd.DataFrame({
        "NH4_N": [0.1], "NO3_N": [0.5], "DISS_ORG_N": [0.2],
        "DET_PART_ORG_N": [0.1], "ZOO_N": [0.05],
        "PO4_P": [0.02], "DISS_ORG_P": [0.01], "DET_PART_ORG_P": [0.005],
        "ZOO_P": [0.002], "DIA_C": [1.0],  # only diatom carbon present
    })
    out = val.add_derived(df)
    # TN = inorganic+organic+detr+zoo + N:C*phyto_C = 0.95 + 0.22*1.0
    assert out["TN"].iloc[0] == pytest.approx(1.17)
    # TP = 0.037 + 0.024*1.0
    assert out["TP"].iloc[0] == pytest.approx(0.061)
    # Chl-a = 1000 * DIA_C / 30  (ug/L)
    assert out["CHLA"].iloc[0] == pytest.approx(1000.0 / 30.0)


def test_add_derived_tolerates_missing_pools():
    # A minimal frame (as from a stripped .out) must not raise; absent pools -> 0.
    df = pd.DataFrame({"NO3_N": [0.5], "DIA_C": [0.0]})
    out = val.add_derived(df)
    assert out["TN"].iloc[0] == pytest.approx(0.5)
    assert out["TP"].iloc[0] == pytest.approx(0.0)
    assert out["CHLA"].iloc[0] == pytest.approx(0.0)


# --- CYN_N-aware TN (VARN/Droop-mechanism outputs, Task 6) -----------------

def _n_pool_frame(cyn_n=None):
    """DIA_C/FIX_CYN_C/OPA_C/NOST_VEG_HET_C/AKI_C are all present alongside
    CYN_C so a test can prove ONLY CYN_C's contribution is replaced by CYN_N
    -- the other four phyto pools must still use the legacy N_TO_C ratio."""
    cols = {
        "NH4_N": [0.10], "NO3_N": [0.20], "DISS_ORG_N": [0.30],
        "DET_PART_ORG_N": [0.40], "ZOO_N": [0.05],
        "DIA_C": [1.0], "CYN_C": [2.0], "FIX_CYN_C": [0.5],
        "OPA_C": [0.3], "NOST_VEG_HET_C": [0.1], "AKI_C": [0.0],
    }
    if cyn_n is not None:
        cols["CYN_N"] = [cyn_n]
    return pd.DataFrame(cols)


def test_add_derived_uses_cyn_n_column_when_present():
    # CYN_N deliberately NOT equal to N_TO_C*CYN_C (0.22*2.0=0.44) -- 0.30 --
    # so a passing assert can only mean the CYN_N column was actually read,
    # not that the legacy ratio happened to coincide.
    df = _n_pool_frame(cyn_n=0.30)
    out = val.add_derived(df)
    non_cyn_phyto_c = 1.0 + 0.5 + 0.3 + 0.1 + 0.0  # DIA+FIX_CYN+OPA+NOST+AKI
    expected = (0.10 + 0.20 + 0.30 + 0.40 + 0.05
                + val.N_TO_C * non_cyn_phyto_c + 0.30)
    assert out["TN"].iloc[0] == pytest.approx(expected)
    # sanity: NOT the legacy value (proves the branch actually fired)
    legacy = (0.10 + 0.20 + 0.30 + 0.40 + 0.05
              + val.N_TO_C * (non_cyn_phyto_c + 2.0))
    assert out["TN"].iloc[0] != pytest.approx(legacy)


def test_add_derived_without_cyn_n_matches_legacy_ratio_exactly():
    """Backward-compat proof (unit-test form): the no-CYN_N branch is the
    ORIGINAL expression, unmodified -- byte-for-byte, not just numerically
    equivalent to a decomposed reconstruction (floating-point associativity
    can differ even when two forms are mathematically equal)."""
    df = _n_pool_frame(cyn_n=None)
    out = val.add_derived(df)
    phyto_c = 1.0 + 2.0 + 0.5 + 0.3 + 0.1 + 0.0
    legacy_expr = (val._col(df, "NH4_N") + val._col(df, "NO3_N") + val._col(df, "DISS_ORG_N")
                   + val._col(df, "DET_PART_ORG_N") + val._col(df, "ZOO_N")
                   + val.N_TO_C * phyto_c)
    assert (out["TN"] == legacy_expr).all()  # exact, not approx: same expression
