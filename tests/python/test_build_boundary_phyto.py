"""Unit tests for tools/build_boundary_phyto.py: rewriting an ESTAS boundary file.

The ESTAS time-series format carries two numeric header rows -- SCALE FACTORS and
UNIT CONVERSION FACTORS -- that do not start with '#' and have one value per
variable but NO time column. ESTAS multiplies every value by both
(mod_TIME_SERIES.f90 VALUE_FROM_TIME_SERIE), so a rewrite that touches them
silently rescales the boundary.
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
from build_boundary_phyto import STATE_VAR, rewrite_forcing  # noqa: E402

NVAR = 20  # enough for the highest rewritten column (FIX_CYN_C = 19)
SCALE_ROW = "".join(f"{(2.0 if i == 3 else 1.0):20.8f}" for i in range(1, NVAR + 1)) + "\n"
UNIT_ROW = "".join(f"{1.0:20.8f}" for _ in range(NVAR)) + "\n"


def _data_row(t):
    # value of variable j is 1000 + j, so every untouched column is recognisable
    return f"{t:f} " + " ".join(f"{1000.0 + j:f}" for j in range(1, NVAR + 1)) + " \n"


def _estas_file(path, times, with_marker=True):
    lines = [
        "# boundary 1 concentrations\n", "# DATA_SIZE\n", f"{len(times)}\n",
        "# NUMBER_OF_VARIABLES\n", f"{NVAR}\n",
        "# SCALE FACTORS\n", "#\n", SCALE_ROW,
        "# UNIT CONVERSION FACTORS\n", "#\n", UNIT_ROW,
        "# INTERPOLATE (1=yes)\n", "1\n",
    ]
    if with_marker:
        lines.append("# TIME AND VALUES\n")
    lines += [_data_row(t) for t in times]
    path.write_text("".join(lines))
    return lines


# month m (1-12) of group g gets a distinct value, so the month mapping is checked too
CLIM = {g: [round(0.1 * (k + 1) + m / 1000.0, 6) for m in range(1, 13)]
        for k, g in enumerate(STATE_VAR)}


def test_factor_rows_are_left_untouched(tmp_path):
    src, dst = tmp_path / "FORC_TS_1.txt", tmp_path / "out.txt"
    before = _estas_file(src, [0.0, 40.0, 200.0])

    rewrite_forcing(str(src), str(dst), CLIM)

    after = dst.read_text().splitlines(keepends=True)
    assert after[7] == before[7] == SCALE_ROW
    assert after[10] == before[10] == UNIT_ROW


def test_only_data_rows_are_rewritten_with_their_month(tmp_path):
    src, dst = tmp_path / "FORC_TS_1.txt", tmp_path / "out.txt"
    before = _estas_file(src, [0.0, 40.0, 200.0])          # 2012: Jan 1, Feb 10, Jul 19

    n = rewrite_forcing(str(src), str(dst), CLIM)

    assert n == 3
    after = dst.read_text().splitlines(keepends=True)
    assert after[:14] == before[:14]                        # header incl. the marker
    for row, month in zip(after[14:], (1, 2, 7)):
        parts = row.split()
        for grp, col in STATE_VAR.items():
            assert float(parts[col]) == pytest.approx(CLIM[grp][month - 1])
        untouched = [j for j in range(1, NVAR + 1) if j not in STATE_VAR.values()]
        assert [float(parts[j]) for j in untouched] == [1000.0 + j for j in untouched]


def test_file_without_time_and_values_marker_is_refused(tmp_path):
    src, dst = tmp_path / "FORC_TS_1.txt", tmp_path / "out.txt"
    _estas_file(src, [0.0, 40.0], with_marker=False)

    with pytest.raises(ValueError, match="TIME AND VALUES"):
        rewrite_forcing(str(src), str(dst), CLIM)
    assert not dst.exists()
