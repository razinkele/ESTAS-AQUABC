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
