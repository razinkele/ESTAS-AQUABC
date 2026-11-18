"""Contract tests between the app, the tracked Standard inputs and the model.

Each test pins one mismatch found by the 2026-11-18 audit so it cannot return:

* the Standard constants must match the temperature model the Standard options select;
* the built-in scenario presets must address the state variables they name;
* plot dates must be counted from the producing run's BASE_YEAR.
"""
import json
import os
import re

try:
    from shiny_app.scenarios import create_builtin_presets
    from shiny_app.simulation_config import (
        base_year_for_input_dir,
        base_year_for_output_dir,
        read_base_year,
    )
except ImportError:
    from scenarios import create_builtin_presets
    from simulation_config import (
        base_year_for_input_dir,
        base_year_for_output_dir,
        read_base_year,
    )

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
INPUTS = os.path.join(ROOT, "INPUTS")


def _option(options_file: str, key: str) -> str:
    lines = open(os.path.join(INPUTS, options_file)).read().splitlines()
    for i, line in enumerate(lines[:-1]):
        if line.startswith("#") and key in line:
            return lines[i + 1].strip()
    raise AssertionError(f"{key} not found in {options_file}")


def _constants() -> dict[str, float]:
    out = {}
    for line in open(os.path.join(INPUTS, "WCONST_04.txt")):
        parts = line.split()
        if len(parts) >= 3 and parts[0].isdigit():
            out[parts[1]] = float(parts[2])
    return out


def _state_names() -> dict[int, str]:
    """State-variable number -> name, from the tracked initial-condition file."""
    names = {}
    for line in open(os.path.join(INPUTS, "INIT_CONC_1.txt")):
        m = re.match(r"\s*(\d+)\s+\S+\s+!\s*(.+)", line)
        if m:
            names[int(m.group(1))] = m.group(2).strip().upper()
    return names


# --- Standard constants vs the temperature model ---------------------------------

def test_standard_temperature_constants_match_the_selected_model():
    """Plateau (0) reads KAPPA_* as decay rates per degC; CTMI (1) reads
    KAPPA_*_OVER_OPT_TEMP as T_max in degC. A T_max under plateau removes the cold
    limit and collapses growth just above the plateau, with no warning."""
    model = int(_option("PELAGIC_MODEL_OPTIONS.txt", "TEMPERATURE_MODEL"))
    consts = _constants()
    over = {k: v for k, v in consts.items() if re.fullmatch(r"KAPPA_\w+_OVER_OPT_TEMP", k)}
    under = {k: v for k, v in consts.items() if re.fullmatch(r"KAPPA_\w+_UNDER_OPT_TEMP", k)}
    assert len(over) == 6 and len(under) == 6
    assert model in (0, 1), f"unsupported TEMPERATURE_MODEL {model}"
    for name in over:
        group = name[len("KAPPA_"):-len("_OVER_OPT_TEMP")]
        assert consts[f"{group}_OPT_TEMP_LR"] < consts[f"{group}_OPT_TEMP_UR"], group
    if model == 0:
        assert all(0.0 < v < 1.0 for v in over.values()), over
        assert all(0.0 < v < 1.0 for v in under.values()), under
    else:
        for name, t_max in over.items():
            group = name[len("KAPPA_"):-len("_OVER_OPT_TEMP")]
            t_min, t_opt = consts[f"{group}_OPT_TEMP_LR"], consts[f"{group}_OPT_TEMP_UR"]
            assert t_min < t_opt < t_max and 2 * t_opt > t_min + t_max, (group, t_min, t_opt, t_max)


def test_both_standard_option_files_select_the_same_temperature_model():
    assert (_option("PELAGIC_MODEL_OPTIONS.txt", "TEMPERATURE_MODEL")
            == _option("PELAGIC_MODEL_OPTIONS_advredox.txt", "TEMPERATURE_MODEL"))


# --- Scenario presets --------------------------------------------------------------

def test_hypoxic_preset_lowers_oxygen_not_zooplankton(tmp_path):
    create_builtin_presets(str(tmp_path))
    ics = json.load(open(tmp_path / "hypoxic.json"))["initial_conditions"]
    names = _state_names()
    lowered = [names[int(k)] for k, v in ics.items() if v == 2.0]
    assert lowered == ["DISSOLVED OXYGEN"]
    assert not any("ZOOPLANKTON" in names[int(k)] for k in ics)


def test_shipped_hypoxic_json_matches_the_builtin_preset(tmp_path):
    create_builtin_presets(str(tmp_path))
    built = json.load(open(tmp_path / "hypoxic.json"))["initial_conditions"]
    shipped = json.load(open(os.path.join(ROOT, "shiny_app", "scenarios", "hypoxic.json")))
    assert shipped["initial_conditions"] == built


# --- Plot time origin ----------------------------------------------------------------

def test_read_base_year_of_tracked_control_files():
    assert read_base_year(os.path.join(ROOT, "INPUT.txt")) == 1998
    assert read_base_year(os.path.join(ROOT, "INPUT_CL29.txt")) == 2012
    assert read_base_year(os.path.join(ROOT, "no_such_file.txt")) is None


def test_base_year_follows_the_output_folder(tmp_path):
    def control(name, year, folder):
        (tmp_path / name).write_text(
            f"# BASE_YEAR\n   {year}\n# PELAGIC MODEL OUTPUT FOLDER write ...\n{folder}\n")

    control("INPUT.txt", 1998, "OUTPUTS/")
    control("INPUT_CL29.txt", 2012, "OUTPUTS_CL29/")
    control("INPUT_zz.txt", 2005, "OUTPUTS_CL29/")
    root = str(tmp_path)
    assert base_year_for_output_dir(root, "OUTPUTS") == 1998
    assert base_year_for_output_dir(root, "OUTPUTS_CL29") == 2012          # name order
    assert base_year_for_output_dir(root, "OUTPUTS_CL29", ("INPUT_zz.txt",)) == 2005
    assert base_year_for_output_dir(root, "OUTPUTS_other") is None
    assert base_year_for_output_dir(root, None) is None


def test_base_year_lookup_tolerates_odd_control_files(tmp_path):
    (tmp_path / "INPUT_a.txt").write_bytes(b"# BASE_YEAR\n 2001\n\xff\xfe# junk\n")       # undecodable
    (tmp_path / "INPUT_b.txt").write_text(
        "  # BASE_YEAR\n 2012\n  # PELAGIC MODEL OUTPUT FOLDER\n./OUTPUTS_CL29/\n"
        "# PELAGIC MODEL INPUT FOLDER\nINPUTS_CL29/\n")                               # indented, ./
    (tmp_path / "INPUT_c.txt").write_text(
        "# BASE_YEAR\n 10000\n# PELAGIC MODEL OUTPUT FOLDER\nOUTPUTS_far/\n")          # not a date
    root = str(tmp_path)
    assert base_year_for_output_dir(root, "OUTPUTS_CL29") == 2012
    assert base_year_for_input_dir(root, "INPUTS_CL29") == 2012
    assert base_year_for_output_dir(root, "OUTPUTS_far") is None
    assert read_base_year(str(tmp_path / "INPUT_c.txt")) is None


def test_input_file_dates_use_the_folders_base_year(tmp_path):
    try:
        from shiny_app.input_analysis import analyze_input_file
    except ImportError:
        from input_analysis import analyze_input_file
    (tmp_path / "INPUT_CL29.txt").write_text(
        "# BASE_YEAR\n 2012\n# PELAGIC MODEL INPUT FOLDER\nINPUTS_CL29/\n")
    d = tmp_path / "INPUTS_CL29"
    d.mkdir()
    (d / "TEMP_TS.txt").write_text("# header\n1.0 1.0\n366.0 2.0\n")
    info = analyze_input_file(str(d / "TEMP_TS.txt"))
    assert (info["date_start"], info["date_end"]) == ("2012-01-02", "2013-01-01")


def test_app_has_no_fixed_time_origin():
    """No module may count dates from a literal year (plots once used 1 Jan 1997)."""
    app = os.path.join(ROOT, "shiny_app")
    for folder in (app, os.path.join(app, "modules")):
        for name in sorted(os.listdir(folder)):
            if name.endswith(".py") and name != "test_plotting.py":
                src = open(os.path.join(folder, name)).read()
                assert not re.search(r"\b(?:date|datetime|dt)\(\s*1997\s*,", src), name


# --- App constants contract (audit item 9) ---------------------------------------------

def test_app_constant_count_matches_the_model():
    try:
        from shiny_app.utils import REQUIRED_MODEL_CONSTANTS
    except ImportError:
        from utils import REQUIRED_MODEL_CONSTANTS
    src = open(os.path.join(ROOT, "SOURCE_CODE", "ESTAS", "mod_GLOBAL.f90")).read()
    assert REQUIRED_MODEL_CONSTANTS == int(re.search(r"nconst\s*=\s*(\d+)", src).group(1))


def test_default_constants_file_is_tracked_and_valid():
    try:
        from shiny_app.config import DEFAULT_CONSTANTS_FILE
        from shiny_app.utils import validate_constants_file
    except ImportError:
        from config import DEFAULT_CONSTANTS_FILE
        from utils import validate_constants_file
    assert os.path.isfile(os.path.join(INPUTS, DEFAULT_CONSTANTS_FILE))
    assert validate_constants_file(DEFAULT_CONSTANTS_FILE, INPUTS) == (True, 324, None)


def test_modules_do_not_define_their_own_default_constants_file():
    """run_control and dashboard each carried a stale copy naming an untracked file."""
    mods = os.path.join(ROOT, "shiny_app", "modules")
    for name in sorted(os.listdir(mods)):
        if name.endswith(".py"):
            src = open(os.path.join(mods, name)).read()
            assert not re.search(r"^DEFAULT_CONSTANTS_FILE\s*=", src, re.M), name


# --- Plot thinning (audit item 7) ---------------------------------------------------------

def test_thin_rows_covers_the_whole_record():
    import pandas as pd
    try:
        from shiny_app.utils import thin_rows
    except ImportError:
        from utils import thin_rows
    df = pd.DataFrame({"t": range(96385), "y": range(96385)})      # CL29: 4016 d x 24/d + 1
    out, step = thin_rows(df, 10000)
    assert step == 10 and len(out) <= 10001
    assert out["t"].iloc[0] == 0 and out["t"].iloc[-1] == 96384     # first AND last row kept
    assert out["t"].is_monotonic_increasing and out["t"].is_unique
    small, step1 = thin_rows(df.head(500), 10000)
    assert step1 == 1 and small is not None and len(small) == 500
    assert thin_rows(df, None)[1] == 1


def test_main_plot_reads_the_full_record():
    src = open(os.path.join(ROOT, "shiny_app", "modules", "plot.py")).read()
    assert "max_rows=DEFAULT_PLOT_ROWS" not in src
    assert "thin_rows(df, DEFAULT_PLOT_ROWS)" in src


# --- Full-precision writes (audit item 8) ---------------------------------------------------

def test_format_full_precision_round_trips():
    try:
        from shiny_app.utils import format_full_precision
    except ImportError:
        from utils import format_full_precision
    for v in (12.825253, 0.059870, 2.97874, 8.10035, 1e-6, 0.0, -1.0, 720.0, 0.1 + 0.2, 123456789.123):
        assert float(format_full_precision(v)) == v
    assert format_full_precision(12.825253) == "12.825253"


def test_edited_values_are_written_at_full_precision(tmp_path):
    try:
        from shiny_app.ic_parser import ICFile
        from shiny_app.parameter_parser import ParameterFile
    except ImportError:
        from ic_parser import ICFile
        from parameter_parser import ParameterFile
    import shutil
    w = tmp_path / "WCONST_04.txt"
    shutil.copy(os.path.join(INPUTS, "WCONST_04.txt"), w)
    pf = ParameterFile(str(w))
    assert pf.parse()
    ok, _msg = pf.update_parameter(5, 8.1003512)
    assert ok and pf.save(backup=False)[0]
    line = next(ln for ln in open(w) if ln.split()[:2] == ["5", "KG_DIA_OPT_TEMP"])
    assert line.split()[2] == "8.1003512"

    ic = tmp_path / "INIT_CONC_1.txt"
    shutil.copy(os.path.join(INPUTS, "INIT_CONC_1.txt"), ic)
    icf = ICFile(str(ic))
    assert icf.parse()
    ok, _msg = icf.update_condition(4, 12.825253)
    assert ok and icf.save(backup=False)[0]
    line = next(ln for ln in open(ic) if ln.split()[:1] == ["4"])
    assert line.split()[1] == "12.825253"


# --- Simulation Config (audit item 6) ---------------------------------------------------------

def test_sediment_mode_maps_to_the_models_meaning():
    try:
        from shiny_app.simulation_config import sediment_mode_choice, sediment_mode_value
    except ImportError:
        from simulation_config import sediment_mode_choice, sediment_mode_value
    assert [sediment_mode_choice(v) for v in (0, 1, 2, 3)] == ["0", "1", "2", "2"]
    assert sediment_mode_value("2", 0) == 2            # switching diagenesis on writes 2, not 1
    assert sediment_mode_value("1", 0) == 1
    assert sediment_mode_value("2", 3) == 3            # unchanged mode keeps the file's number
    assert sediment_mode_value("0", 2) == 0


def test_sim_config_does_not_hard_wire_input_txt_for_load_and_save():
    src = open(os.path.join(ROOT, "shiny_app", "modules", "sim_config.py")).read()
    assert "SimulationConfigFile(INPUT_TXT_PATH)" not in src
    assert "SimulationConfigFile(control_path)" in src


# --- Binary output layout ----------------------------------------------------------------

def _write_records(path, n_state, nrows=6):
    import numpy as np
    data = np.zeros((nrows, n_state + 1))
    data[:, 0] = 6209.0 + np.arange(nrows) / 24.0            # time, increasing
    data[:, 1:] = 100.0 * np.arange(1, n_state + 1) + np.arange(nrows)[:, None] * 0.001
    data.tofile(str(path))
    return data


def test_binary_columns_follow_the_state_count():
    """32 base states; CYN_N is state 33 (VARN), before the four metabolites."""
    from shiny_app.utils import PELAGIC_BOX_COLUMNS, pelagic_box_columns
    assert pelagic_box_columns(36) == PELAGIC_BOX_COLUMNS
    assert pelagic_box_columns(32) == PELAGIC_BOX_COLUMNS[:33]
    varn = pelagic_box_columns(37)
    assert len(varn) == 38 and varn[33] == "CYN_N" and varn[34:] == PELAGIC_BOX_COLUMNS[33:]
    assert pelagic_box_columns(33)[-1] == "CYN_N"
    try:
        pelagic_box_columns(35)
    except ValueError:
        pass
    else:
        raise AssertionError("35 states is not a layout the model writes")


def test_binary_reader_takes_the_layout_from_the_setup(tmp_path):
    """A VARN run writes 38 doubles per record; read as 37 every column shifts."""
    from shiny_app.utils import read_pelagic_binary, state_count_for_output_file
    (tmp_path / "INPUT_V.txt").write_text(
        "# PELAGIC MODEL INPUT FOLDER\nINPUTS_V/\n# PELAGIC MODEL INPUT FILE\n  PELAGIC_INPUTS.txt\n"
        "# PELAGIC MODEL OUTPUT FOLDER write ...\nOUTPUTS_V/\n")
    (tmp_path / "INPUTS_V").mkdir()
    (tmp_path / "INPUTS_V" / "PELAGIC_INPUTS.txt").write_text("# NUM_PELAGIC_STATE_VARS\n   37\n")
    out = tmp_path / "OUTPUTS_V"
    out.mkdir()
    f = out / "P.bin_PELAGIC_BOX_00005.bin"
    data = _write_records(f, 37)
    assert state_count_for_output_file(str(f)) == 37
    df = read_pelagic_binary(str(f))
    assert df.shape == (6, 38)
    assert df["CYN_N"].iloc[0] == data[0, 33] and df["SEC_METAB_NOST"].iloc[0] == data[0, 37]
    assert list(df["TIME_DAYS"]) == list(data[:, 0])


def test_binary_reader_does_not_trust_a_setup_the_file_contradicts(tmp_path):
    """The folder can hold a run made before the setup changed its state count."""
    from shiny_app.utils import read_pelagic_binary, state_count_for_output_file
    (tmp_path / "INPUT.txt").write_text(
        "# PELAGIC MODEL INPUT FOLDER\nINPUTS/\n# PELAGIC MODEL INPUT FILE\nPELAGIC_INPUTS.txt\n"
        "# PELAGIC MODEL OUTPUT FOLDER write ...\nOUTPUTS/\n")
    (tmp_path / "INPUTS").mkdir()
    (tmp_path / "INPUTS" / "PELAGIC_INPUTS.txt").write_text("# NUM_PELAGIC_STATE_VARS\n   36\n")
    (tmp_path / "OUTPUTS").mkdir()
    f = tmp_path / "OUTPUTS" / "P.bin_PELAGIC_BOX_00001.bin"
    data = _write_records(f, 37, nrows=37)             # 37 x 38 values: also divisible by 37
    assert state_count_for_output_file(str(f)) == 36
    df = read_pelagic_binary(str(f))
    assert df.shape == (37, 38) and list(df["TIME_DAYS"]) == list(data[:, 0])
    # ... and with the matching layout a record still being written is dropped, not an error
    g = tmp_path / "OUTPUTS" / "P.bin_PELAGIC_BOX_00002.bin"
    _write_records(g, 36, nrows=5)
    with open(g, "ab") as fh:
        fh.write(b"\0" * 8 * 3)
    assert read_pelagic_binary(str(g)).shape == (5, 37)


def test_binary_reader_infers_the_layout_without_a_setup(tmp_path):
    """Only one record length both divides the file and gives an increasing time column."""
    from shiny_app.utils import pelagic_binary_layout, read_pelagic_binary
    for n_state in (32, 33, 36, 37):
        f = tmp_path / f"box_{n_state}.bin"
        data = _write_records(f, n_state, nrows=7 * 11 * 17)   # divisible by none of the others by chance
        assert len(pelagic_binary_layout(str(f))) == n_state + 1, n_state
        assert read_pelagic_binary(str(f)).shape == data.shape
    # the tracked Standard setup is the 36-state layout
    assert int(open(os.path.join(INPUTS, "PELAGIC_INPUTS.txt")).read().splitlines()[6].split()[0]) == 36


def test_binary_reader_refuses_an_ambiguous_file(tmp_path):
    import numpy as np

    from shiny_app.utils import pelagic_binary_layout
    f = tmp_path / "one_record.bin"                    # 33*34*37*38 values: every layout divides it
    n = 33 * 34 * 37 * 38
    np.arange(n, dtype=np.float64).tofile(str(f))      # ... and every "time" column increases
    try:
        pelagic_binary_layout(str(f))
    except ValueError as e:
        assert "record layout" in str(e)
    else:
        raise AssertionError("ambiguous layout must not be guessed")


def test_no_module_hard_codes_the_binary_record_length():
    app = os.path.join(ROOT, "shiny_app")
    for folder in (app, os.path.join(app, "modules")):
        for name in sorted(os.listdir(folder)):
            if name.endswith(".py"):
                src = open(os.path.join(folder, name)).read()
                assert not re.search(r"num_cols\s*=\s*37\b|ncols\s*=\s*37\b", src), name


# --- Output Config ---------------------------------------------------------------------

def _oc():
    from shiny_app import output_config
    return output_config


def test_output_config_round_trip_reproduces_the_tracked_files():
    oc = _oc()
    for name in ("PELAGIC_OUTPUT_INFORMATION_FILE.txt", "PELAGIC_OUTPUT_INFORMATION_FILE_verify.txt"):
        text = open(os.path.join(INPUTS, name)).read()
        _header, flags = oc.parse(text)
        boxes, types, _mixed = oc.summarize(flags)
        assert oc.rewrite(text, oc.apply_selection(flags, boxes, types, len(flags), types)) == text, name


def test_output_config_keeps_other_layouts_and_per_box_flags():
    oc = _oc()
    text = ("#  BOX_NO   STATE_VAR_OUT   PROCESS_RATE_OUT   MASS_BALANCE_OUT\n"
            "           1           1           0           0\n"
            "           2           1           1           1\n"
            "           3           0           0           0\n")
    _header, flags = oc.parse(text)
    boxes, types, mixed = oc.summarize(flags)
    assert (boxes, types, mixed) == (["1", "2"], ["state_vars", "process_rates", "mass_balance"], True)
    # unchanged selection: byte-identical, per-box differences intact
    assert oc.rewrite(text, oc.apply_selection(flags, boxes, types, 3, types)) == text
    # adding box 3 with the types untouched: 1 and 2 keep their own flags, 3 gets the shown set
    new = oc.apply_selection(flags, ["1", "2", "3"], types, 3, types)
    assert new == {1: (1, 0, 0), 2: (1, 1, 1), 3: (1, 1, 1)}
    assert oc.rewrite(text, new).splitlines()[1:3] == text.splitlines()[1:3]
    # changing the types is an explicit choice for every selected box
    assert oc.apply_selection(flags, ["1", "2"], ["state_vars"], 3, types) == {
        1: (1, 0, 0), 2: (1, 0, 0), 3: (0, 0, 0)}
    # a setup with more boxes than the file has rows gets the missing rows appended
    grown = oc.rewrite(text, oc.apply_selection(flags, ["4"], ["state_vars"], 4, types))
    assert oc.parse(grown)[1] == {1: (0, 0, 0), 2: (0, 0, 0), 3: (0, 0, 0), 4: (1, 0, 0)}


def test_output_config_second_save_leaves_per_box_flags_alone():
    """After a save the form is re-filled from the file; a second, unchanged save
    must not read the narrower type set as a change and overwrite a box's flags."""
    oc = _oc()
    text = ("# header\n   1   1   0   0\n   2   1   1   1\n   3   0   1   0\n")
    _h, flags = oc.parse(text)
    _boxes, types, _m = oc.summarize(flags)                       # form after load
    saved = oc.rewrite(text, oc.apply_selection(flags, ["1", "3"], types, 3, types))   # drop box 2
    _h, flags2 = oc.parse(saved)
    assert flags2 == {1: (1, 0, 0), 2: (0, 0, 0), 3: (0, 1, 0)}
    boxes2, types2, _m = oc.summarize(flags2)                     # form re-filled after the save
    again = oc.rewrite(saved, oc.apply_selection(flags2, boxes2, types2, 3, types2))
    assert again == saved
    src = open(os.path.join(ROOT, "shiny_app", "modules", "run_control.py")).read()
    save = src[src.index("def save_output_config():"):]
    assert "_load_output_config(announce=False)" in save[:save.index("def output_config_status")]


def test_output_config_save_backs_up_and_never_creates_the_file(tmp_path):
    oc = _oc()
    path = tmp_path / "PELAGIC_OUTPUT_INFORMATION_FILE.txt"
    try:
        oc.save(str(path), "x\n")
    except FileNotFoundError:
        pass
    else:
        raise AssertionError("must not create a file the model does not read")
    assert not path.exists()
    path.write_text("old\n")
    backup = oc.save(str(path), "new\n")
    assert path.read_text() == "new\n" and open(backup).read() == "old\n"


def test_output_config_tab_cannot_write_widget_defaults():
    """Save is refused until the form was filled from the active setup's file."""
    src = open(os.path.join(ROOT, "shiny_app", "modules", "run_control.py")).read()
    save = src[src.index("def save_output_config():"):]
    save = save[:save.index("def output_config_status")]
    assert "output_config_loaded.get()" in save and "Load Current first" in save
    assert "output_config.save(" in save and "open(OUTPUT_INFO_FILE, 'w')" not in src
    assert 'selected=["5", "6", "8", "9", "14", "17", "25"]' not in src


# --- Review follow-ups to the four fixes above -----------------------------------------

def test_stale_setup_count_does_not_win_over_a_layout_that_fits_the_file(tmp_path):
    """Two 37-double records under a setup that now says 32 states: at 33 columns the
    'time' samples are [t0, a small state value] and can rise by chance."""
    import numpy as np

    from shiny_app.utils import read_pelagic_binary, state_count_for_output_file
    (tmp_path / "INPUT.txt").write_text(
        "# PELAGIC MODEL INPUT FOLDER\nINPUTS/\n# PELAGIC MODEL INPUT FILE\nPELAGIC_INPUTS.txt\n"
        "# PELAGIC MODEL OUTPUT FOLDER write ...\nOUTPUTS/\n")
    (tmp_path / "INPUTS").mkdir()
    (tmp_path / "INPUTS" / "PELAGIC_INPUTS.txt").write_text("# NUM_PELAGIC_STATE_VARS\n   32\n")
    (tmp_path / "OUTPUTS").mkdir()
    f = tmp_path / "OUTPUTS" / "P.bin_PELAGIC_BOX_00001.bin"
    rec = np.zeros((2, 37))
    rec[:, 0] = [0.0, 1.0]
    rec[0, 33] = 0.1                                   # SEC_METAB_DIA: the 34th value
    rec.tofile(str(f))
    assert state_count_for_output_file(str(f)) == 32
    df = read_pelagic_binary(str(f))
    assert df.shape == (2, 37) and list(df["TIME_DAYS"]) == [0.0, 1.0]


def test_output_config_keeps_rows_beyond_the_setups_box_count():
    oc = _oc()
    text = "# header\n 1 1 0 0\n 2 0 0 0\n 3 1 1 0\n"
    _h, flags = oc.parse(text)
    new = oc.apply_selection(flags, ["1"], ["state_vars", "process_rates"], 2,
                             ["state_vars", "process_rates"])
    assert new == {1: (1, 0, 0), 2: (0, 0, 0), 3: (1, 1, 0)}
    assert oc.rewrite(text, new) == text
    assert oc.apply_selection(flags, [], [], 2, None)[3] == (1, 1, 0)


def test_output_config_keeps_crlf_line_endings(tmp_path):
    oc = _oc()
    path = tmp_path / "PELAGIC_OUTPUT_INFORMATION_FILE.txt"
    original = b"# header\r\n 1 1 0 0\r\n 2 0 0 0\r\n"
    path.write_bytes(original)
    with open(path, newline="") as fh:
        text = fh.read()
    _h, flags = oc.parse(text)
    oc.save(str(path), oc.rewrite(text, oc.apply_selection(flags, ["1"], ["state_vars"], 2, ["state_vars"])))
    assert path.read_bytes() == original
    oc.save(str(path), oc.rewrite(text, oc.apply_selection(flags, ["1", "2"], ["state_vars"], 3, ["state_vars"])))
    data = path.read_bytes()
    assert data.count(b"\r\n") == 4 and data.count(b"\n") == 4      # changed + appended rows are CRLF too
    src = open(os.path.join(ROOT, "shiny_app", "modules", "run_control.py")).read()
    save = src[src.index("def save_output_config():"):]
    assert 'open(path, newline="")' in save[:save.index("def output_config_status")]


# --- Fortran real literals ---------------------------------------------------------------

def test_fortran_sources_have_no_single_precision_real_literals():
    """`0.1` is a default-real literal: in double-precision arithmetic it carries
    single-precision rounding (about 1e-8 relative, more after cancellation -- the
    DO-saturation polynomial moved by 5e-5). Every real literal needs a D exponent
    or a kind suffix. Lines that are I/O statements are skipped (format widths)."""
    lit = re.compile(r"(?<![\w.])(\d+\.\d*|\.\d+)([eE][+-]?\d+)?(?![\w.])")
    io = re.compile(r"^\s*(\d+\s+)?(write|print|read|format)\b", re.I)
    found = []
    for folder, _dirs, files in os.walk(os.path.join(ROOT, "SOURCE_CODE")):
        for name in sorted(files):
            if not name.endswith(".f90"):
                continue
            in_io = False
            for no, line in enumerate(open(os.path.join(folder, name), errors="replace"), 1):
                code = re.sub(r"'[^']*'|\"[^\"]*\"", "", line).split("!")[0]
                is_io = bool(io.search(code)) or in_io
                in_io = is_io and code.rstrip().endswith("&")
                if not is_io and lit.search(code):
                    found.append(f"{name}:{no}: {code.strip()[:80]}")
    assert not found, "single-precision literals:\n" + "\n".join(found[:20])


# --- Container and lint coverage ---------------------------------------------------------

def test_container_installs_what_the_documented_validation_command_imports():
    """The Dockerfile documents `python3 tools/validate_cl29_vs_epa.py ...`; the image
    must carry every third-party module that script imports (it had numpy only)."""
    import ast
    import sys
    tree = ast.parse(open(os.path.join(ROOT, "tools", "validate_cl29_vs_epa.py")).read())
    mods = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            mods.update(a.name.split(".")[0] for a in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module and node.level == 0:
            mods.add(node.module.split(".")[0])
    local = {os.path.splitext(f)[0] for f in os.listdir(os.path.join(ROOT, "tools"))}
    third_party = {m for m in mods if m not in sys.stdlib_module_names and m not in local}
    assert third_party == {"numpy", "pandas", "matplotlib"}, third_party
    docker = open(os.path.join(ROOT, "Dockerfile")).read()
    for mod in third_party:
        assert f"python3-{mod}" in docker, mod


def test_ruff_covers_tools_and_pest_everywhere():
    pyproject = open(os.path.join(ROOT, "pyproject.toml")).read()
    include = re.search(r"^include = \[(.*)\]", pyproject, re.M).group(1)
    ci = open(os.path.join(ROOT, ".github", "workflows", "ci.yml")).read()
    make = open(os.path.join(ROOT, "Makefile")).read()
    for folder in ("shiny_app", "tests/python", "tools", "pest"):
        assert f'"{folder}/**/*.py"' in include, folder
        assert re.search(rf"ruff check[^\n]*\b{folder}/", ci), folder
        assert re.search(rf"ruff check[^\n]*\b{folder}/", make), folder


def test_fortran_tests_link_production_routines_not_copies():
    """A trimmed copy of the auxiliary routines kept a CTMI-only GROWTH_AT_TEMP after
    production gained the plateau model, so the default path went untested."""
    tests = os.path.join(ROOT, "tests", "fortran")
    assert not os.path.exists(os.path.join(tests, "test_pelagic_aux_subset.f90"))
    makefile = open(os.path.join(tests, "Makefile")).read()
    assert "standalone" not in makefile and "aux_subset" not in makefile
    production = open(os.path.join(ROOT, "SOURCE_CODE", "AQUABC", "PELAGIC",
                                   "aquabc_II_pelagic_auxillary.f90")).read().lower()
    names = set(re.findall(r"^\s*subroutine\s+(\w+)", production, re.M))
    for name in sorted(os.listdir(tests)):
        if name.endswith(".f90"):
            src = open(os.path.join(tests, name)).read().lower()
            copied = names & set(re.findall(r"^\s*subroutine\s+(\w+)", src, re.M))
            assert not copied, (name, copied)
