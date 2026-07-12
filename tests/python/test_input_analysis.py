"""Tests for shiny_app.input_analysis (extracted from app.py)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "shiny_app", "input_analysis.py")
_spec = importlib.util.spec_from_file_location("input_analysis", _PATH)
ia = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(ia)


def test_functions_present():
    for name in ("analyze_input_file", "get_input_file_categories", "validate_required_inputs"):
        assert callable(getattr(ia, name)), name

def test_categories_dict_includes_base_and_update():
    cats = ia.INPUT_FILE_CATEGORIES
    assert isinstance(cats, dict) and cats
    # the .update() adds EXTRA_WCONST.txt — proves both base + update moved
    assert "EXTRA_WCONST.txt" in cats

def test_get_input_file_categories_returns_sorted_unique_category_names():
    # Actual behavior (verified against the verbatim-moved function body):
    # collects info["category"] from every INPUT_FILE_CATEGORIES value into a
    # set, then returns sorted(set) — a sorted list of unique category name
    # strings, NOT the INPUT_FILE_CATEGORIES dict itself.
    cats = ia.get_input_file_categories()
    assert isinstance(cats, list)
    assert cats == sorted(cats)
    assert len(cats) == len(set(cats))
    expected_categories = {info.get("category", "Unknown") for info in ia.INPUT_FILE_CATEGORIES.values()}
    assert set(cats) == expected_categories
    assert "Model Constants" in cats
