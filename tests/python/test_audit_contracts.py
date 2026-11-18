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
    from shiny_app.simulation_config import base_year_for_output_dir, read_base_year
except ImportError:
    from scenarios import create_builtin_presets
    from simulation_config import base_year_for_output_dir, read_base_year

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


def test_plot_module_has_no_fixed_time_origin():
    src = open(os.path.join(ROOT, "shiny_app", "modules", "plot.py")).read()
    assert not re.search(r"\b(?:date|datetime|dt)\(\s*1997\s*,", src)
