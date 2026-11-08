"""Tests for tools/set_per_box_forcing_columns.py on a 3-box fixture with the real section layout."""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))
import set_per_box_forcing_columns as spb  # noqa: E402

SECTIONS = {"AIR TEMPERATURE": 4, "WIND SPEED": 5, "PRECIPITATION": 6, "EVAPORATION": 7, "ICE FRACTION": 8}
FILES = {1: "TEMP_TS.txt", 4: "AIR_TEMP_TS.txt", 5: "WIND_SPEED_TS.txt", 6: "RAINFALL_TS.txt",
         7: "EVAPORATION_TS.txt", 8: "ICE_COVER.txt"}


def _ts(path, ncol):
    with open(path, "w") as fh:
        fh.write(f"# X\n# DATA_SIZE\n2\n# NUMBER_OF_VARIABLES\n{ncol}\n# TIME AND VALUES\n")


def _fixture(tmp_path, widths):
    L = ["# DESCRIPTION", "# ********************* WATER TEMPERATURE *********************",
         "#     PELAGIC BOX NO       FORCING TS NO   FORCING TS VAR NO"]
    L += [f"{b:20d}{1:20d}{b:20d}" for b in (1, 2, 3)]
    for name, ts in SECTIONS.items():
        L += [f"# ********************* {name} *********************",
              "#     PELAGIC BOX NO       FORCING TS NO   FORCING TS VAR NO"]
        L += [f"{b:20d}{ts:20d}{1:20d}" for b in (1, 2, 3)]
    L += ["# ********************* FORCING TIME SERIES *********************",
          "# FORC TIME SERIE NO                                   FORC TIME SERIE FILE NAME"]
    L += [f"{n:20d}{f:>34s}" for n, f in FILES.items()]
    L += ["# NUM_MASS_LOADS", "0"]
    (tmp_path / "PELAGIC_INPUTS.txt").write_text("\n".join(L))
    _ts(tmp_path / "TEMP_TS.txt", 3)
    for ts, f in FILES.items():
        if ts != 1:
            _ts(tmp_path / f, widths.get(f, 3))
    return tmp_path


def _vars(d, section):
    lines = (d / "PELAGIC_INPUTS.txt").read_text().split("\n")
    i = next(k for k, ln in enumerate(lines) if section in ln and ln.startswith("# ****"))
    return [int(ln.split()[2]) for ln in lines[i + 2:i + 5]]


def test_multi_column_files_get_per_box_columns(tmp_path):
    d = _fixture(tmp_path, {})
    assert spb.main([str(d)]) == 1                       # report mode flags the problem
    assert spb.main([str(d), "--write"]) == 0
    for s in SECTIONS:
        assert _vars(d, s) == [1, 2, 3], s
    assert _vars(d, "WATER TEMPERATURE") == [1, 2, 3]    # untouched section
    assert spb.main([str(d)]) == 0                       # now clean


def test_single_column_files_stay_on_var_1(tmp_path):
    d = _fixture(tmp_path, {f: 1 for f in FILES.values()})
    assert spb.main([str(d)]) == 0
    for s in SECTIONS:
        assert _vars(d, s) == [1, 1, 1]


def test_wrong_width_is_an_error(tmp_path):
    d = _fixture(tmp_path, {"WIND_SPEED_TS.txt": 2})
    try:
        spb.main([str(d), "--write"])
    except SystemExit as e:
        assert "2 columns for 3 boxes" in str(e)
    else:
        raise AssertionError("expected SystemExit")
