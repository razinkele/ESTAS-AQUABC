"""Tests for shiny_app.file_locators (extracted from app.py)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "shiny_app", "file_locators.py")
_spec = importlib.util.spec_from_file_location("file_locators", _PATH)
fl = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(fl)


def test_functions_present():
    for name in ("get_output_folder", "find_pelagic_box_file",
                 "get_available_boxes", "get_timeseries_variables"):
        assert callable(getattr(fl, name)), name

def test_root_and_inputs_dir_resolve():
    assert fl.ROOT.endswith(os.sep + "AQUABCv0.2") or os.path.isdir(fl.ROOT)
    assert fl.INPUTS_DIR.endswith("INPUTS")

def test_get_output_folder_returns_str():
    assert isinstance(fl.get_output_folder(), str)
