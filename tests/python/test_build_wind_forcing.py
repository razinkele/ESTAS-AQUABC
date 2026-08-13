"""Unit tests for tools/build_wind_forcing.py (writer + header format; no ERA5 deps)."""
import datetime as dt
import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
from build_wind_forcing import _norm, write_ts  # noqa: E402


def _series(n, base=dt.date(2012, 1, 1), value=5.0):
    return {base + dt.timedelta(days=i): value + 0.1 * i for i in range(n)}


def test_write_ts_matches_the_estas_reader_format(tmp_path):
    out = tmp_path / "WIND_SPEED_TS.txt"
    gaps = write_ts(_series(5), str(out), n_days=5)
    lines = out.read_text().splitlines()
    assert gaps == 0
    assert lines[0] == "# WIND_SPEED_TS"
    assert lines[1] == "# DATA_SIZE" and lines[2] == "5"
    assert lines[3] == "# NUMBER_OF_VARIABLES" and lines[4] == "1"
    assert lines[11] == "# INTERPOLATE (1=yes)" and lines[12] == "1"
    assert lines[13] == "# TIME AND VALUES"
    # rows are "t value" with t in days from the base date
    t0, v0 = lines[14].split()
    t4, v4 = lines[18].split()
    assert float(t0) == 0.0 and float(v0) == pytest.approx(5.0)
    assert float(t4) == 4.0 and float(v4) == pytest.approx(5.4)
    assert len(lines) == 14 + 5


def test_write_ts_fills_short_gaps_linearly_and_counts_them(tmp_path):
    s = _series(5)
    del s[dt.date(2012, 1, 3)]                     # one-day hole -> midpoint
    out = tmp_path / "ts.txt"
    gaps = write_ts(s, str(out), n_days=5)
    assert gaps == 1
    v = float(out.read_text().splitlines()[16].split()[1])
    assert v == pytest.approx((5.1 + 5.3) / 2)


def test_write_ts_refuses_to_extrapolate_edge_gaps(tmp_path):
    s = _series(5)
    del s[dt.date(2012, 1, 5)]                     # trailing hole -> no right anchor
    with pytest.raises(SystemExit, match="refuse to extrapolate"):
        write_ts(s, str(tmp_path / "ts.txt"), n_days=5)


def test_norm_strips_lithuanian_diacritics_for_header_matching():
    assert _norm("Vėjo greitis,  m/s") == "vejo greitis, m/s"
    assert "kursiu" in _norm("Kuršių marių vandenys")
