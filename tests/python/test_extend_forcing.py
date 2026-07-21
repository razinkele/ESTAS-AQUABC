"""Unit tests for the CL29 climatological forcing extender."""
import os
import sys
from datetime import date

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
import extend_cl29_forcing_climatology as ext  # noqa: E402


def test_set_data_size():
    header = ["# TITLE", "# DATA_SIZE", "2", "# NUMBER_OF_VARIABLES", "1", "# TIME AND VALUES"]
    out = ext.set_data_size(header, 730)
    assert out[2] == "730"          # the value line after '# DATA_SIZE' is replaced
    assert out[4] == "1"            # other value lines untouched


def test_fmt_row():
    assert ext.fmt_row([730.0, 6.0, 1.5]) == "730.000000 6.000000 1.500000"


def _daily_series():
    """Two full non-leap years (2021-2022), one variable = day-of-year in yr1, +10 in yr2."""
    base = date(2021, 1, 1)
    header = ["# X", "# DATA_SIZE", "730", "# NUMBER_OF_VARIABLES", "1", "# TIME AND VALUES"]
    rows = []
    for d in range(0, 730):                       # 2021-01-01 .. 2022-12-31
        doy = (base.__class__.fromordinal(base.toordinal() + d)).timetuple().tm_yday
        offset = 0 if d < 365 else 10             # year 1 vs year 2
        rows.append([float(d), float(doy + offset)])
    return base, header, rows


def test_extend_daily_uses_doy_climatology():
    base, header, rows = _daily_series()
    new_header, all_rows = ext.extend_series(header, rows, base, end_day=1094)  # +2023
    # 2023-01-01 is day 730, doy 1 -> mean of the two prior years' doy-1 values (1 and 11) = 6
    day730 = next(r for r in all_rows if r[0] == 730.0)
    assert day730[1] == 6.0
    # 2023-07-01 (day 911, doy 182) -> mean of (182, 192) = 187
    day911 = next(r for r in all_rows if r[0] == 911.0)
    assert day911[1] == 187.0
    assert new_header[2] == str(len(all_rows))    # DATA_SIZE updated
    assert all_rows[-1][0] == 1094.0              # extended to the end day


def test_extend_constant_holds_last_value():
    base = date(2012, 1, 1)
    header = ["# WIND", "# DATA_SIZE", "2", "# NUMBER_OF_VARIABLES", "1", "# TIME AND VALUES"]
    rows = [[0.0, 5.0], [100.0, 5.0]]             # <= CONST_MAX rows => constant
    new_header, all_rows = ext.extend_series(header, rows, base, end_day=200)
    assert all_rows[-1] == [200.0, 5.0]           # one row appended at end_day, value held
    assert new_header[2] == "3"


def test_already_covered_returns_none():
    base = date(2012, 1, 1)
    header = ["# S", "# DATA_SIZE", "2", "# NUMBER_OF_VARIABLES", "1", "# TIME AND VALUES"]
    rows = [[0.0, 1.0], [9999.0, 1.0]]            # last_t 9999 >= end_day
    assert ext.extend_series(header, rows, base, end_day=4382) is None
