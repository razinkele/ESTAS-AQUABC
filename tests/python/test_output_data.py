import os
from shiny_app.output_data import (
    looks_numeric, format_elapsed, get_output_folder_from_config,
    get_output_files_info, get_output_columns, get_output_directories,
    get_output_files_from_dir,
)
try:
    from shiny_app.utils import PELAGIC_BOX_COLUMNS
except ImportError:
    from utils import PELAGIC_BOX_COLUMNS


def test_looks_numeric():
    assert looks_numeric("3.5") and looks_numeric("10") and looks_numeric("-2e3")
    assert not looks_numeric("abc")
    assert not looks_numeric("")
    assert not looks_numeric(None)


def test_format_elapsed():
    assert format_elapsed(3661) == "1h 1m 1s"
    assert format_elapsed(61) == "1m 1s"
    assert format_elapsed(5) == "5s"


def test_get_output_folder_from_config_missing(tmp_path):
    assert get_output_folder_from_config(input_txt_path=str(tmp_path / "nope.txt")) == "OUTPUTS"


def test_get_output_files_info_missing(tmp_path):
    info = get_output_files_info(root=str(tmp_path), input_txt_path=str(tmp_path / "nope.txt"))
    assert info["exists"] is False and info["folder"] == "OUTPUTS"


def test_get_output_directories(tmp_path):
    (tmp_path / "OUTPUTS_a").mkdir()
    (tmp_path / "stray.txt").write_text("x")
    dirs = get_output_directories(root=str(tmp_path), output_csv=str(tmp_path / "none.csv"))
    assert dirs == {"OUTPUTS_a": "OUTPUTS_a"}          # OUTPUTS* dirs only; no ROOT key (csv absent)


def test_get_output_files_from_dir_text_and_binary(tmp_path):
    sub = tmp_path / "SUB"; sub.mkdir()
    (sub / "PELAGIC_BOX_01.out").write_text("x")       # matches text filter
    (sub / "PELAGIC_BOX_PROCESS_RATES.out").write_text("x")   # excluded (PROCESS_RATES)
    (sub / "PELAGIC_BOX_01.bin").write_text("x")       # matches binary filter
    assert get_output_files_from_dir("SUB", "text", root=str(tmp_path)) == {"PELAGIC_BOX_01.out": "PELAGIC_BOX_01.out"}
    assert get_output_files_from_dir("SUB", "binary", root=str(tmp_path)) == {"PELAGIC_BOX_01.bin": "PELAGIC_BOX_01.bin"}
    assert get_output_files_from_dir("NOPE", "text", root=str(tmp_path)) == {}


def test_get_output_columns_csv_and_binary(tmp_path):
    csv = tmp_path / "out.csv"
    csv.write_text("TIME_DAYS,DIA_C,CYN_C\n1,2,3\n")
    assert get_output_columns(file_path=str(csv), file_format="csv") == ["TIME_DAYS", "DIA_C", "CYN_C"]
    assert get_output_columns(file_path="x.bin", file_format="binary") == PELAGIC_BOX_COLUMNS
