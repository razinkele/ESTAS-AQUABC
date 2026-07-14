import os
import stat

import pytest

from shiny_app.build_commands import (
    assemble_estas_command,
    get_available_executables,
    get_executable_info,
    target_exe_name,
)

DC = "WCONST_01.txt"  # default_constants_file


def test_assemble_all_not_ready_defaults():
    # wrapper passes raw None/False on input-not-ready
    assert assemble_estas_command(None, None, None, False, None, None, DC) == \
        ["./ESTAS_II", "INPUT.txt"]


def test_assemble_input_only():
    assert assemble_estas_command("ESTAS_II", "MYINPUT.txt", "", False, None, "", DC) == \
        ["./ESTAS_II", "MYINPUT.txt"]


def test_assemble_input_and_const():
    assert assemble_estas_command("ESTAS_II", "INPUT.txt", "WCONST_02.txt", False, None, "", DC) == \
        ["./ESTAS_II", "INPUT.txt", "WCONST_02.txt"]


def test_assemble_binary_enabled_empty_name_inserts_default_const_and_placeholder():
    # switch on, name empty -> PELAGIC_OUTPUT.bin; binary set, no const -> default const
    assert assemble_estas_command("ESTAS_II_gf_release", "MYINPUT.txt", "", True, "", "", DC) == \
        ["./ESTAS_II_gf_release", "MYINPUT.txt", "WCONST_01.txt", "PELAGIC_OUTPUT.bin"]


def test_assemble_binary_enabled_named():
    assert assemble_estas_command("ESTAS_II", "INPUT.txt", "C.txt", True, "OUT.bin", "", DC) == \
        ["./ESTAS_II", "INPUT.txt", "C.txt", "OUT.bin"]


def test_assemble_shear_without_binary_uses_placeholder_and_default_const():
    # falsy exe -> ESTAS_II; shear set, no const -> default const; shear no binary -> placeholder
    assert assemble_estas_command("", "INPUT.txt", "", False, None, "INPUTS/SHEAR.txt", DC) == \
        ["./ESTAS_II", "INPUT.txt", "WCONST_01.txt", "PELAGIC_OUTPUT.bin", "INPUTS/SHEAR.txt"]


def test_assemble_binary_disabled_ignores_filename():
    # binary_enabled False -> binary_filename ignored, no const -> stop after input
    assert assemble_estas_command("ESTAS_II", "INPUT.txt", "", False, "IGNORED.bin", "", DC) == \
        ["./ESTAS_II", "INPUT.txt"]


@pytest.mark.parametrize("compiler,bt,expected", [
    ("gfortran", "release", "ESTAS_II_gf_release"),
    ("ifort", "debug", "ESTAS_II_ifort_debug"),
    ("ifx", "release", "ESTAS_II_ifx_release"),
    ("weirdfc", "release", "ESTAS_II_weirdfc_release"),  # unknown -> identity
])
def test_target_exe_name(compiler, bt, expected):
    assert target_exe_name(compiler, bt) == expected


def test_assemble_binary_named_and_shear():
    # binary_enabled with an explicit name AND shear -> 5-element cmd, shear appended last
    assert assemble_estas_command("ESTAS_II", "INPUT.txt", "C.txt", True, "OUT.bin",
                                  "INPUTS/SHEAR.txt", DC) == \
        ["./ESTAS_II", "INPUT.txt", "C.txt", "OUT.bin", "INPUTS/SHEAR.txt"]


def test_get_available_executables(tmp_path):
    exe = tmp_path / "ESTAS_II"
    exe.write_text("x")
    os.chmod(exe, os.stat(exe).st_mode | stat.S_IEXEC)
    (tmp_path / "notes.txt").write_text("x")            # matches no pattern -> excluded
    (tmp_path / "ESTAS_II_dir").mkdir()                 # matches ESTAS_II_* but is a dir -> excluded
    # matches ESTAS_II_* but the executable bit is OFF -> excluded by the os.access(X_OK) filter
    noexec = tmp_path / "ESTAS_II_debug"
    noexec.write_text("x")
    os.chmod(noexec, os.stat(noexec).st_mode & ~stat.S_IEXEC & ~stat.S_IXGRP & ~stat.S_IXOTH)
    result = get_available_executables(str(tmp_path))
    assert result == ["ESTAS_II"]                       # only the executable file, deduped/sorted


def test_get_executable_info_missing(tmp_path):
    assert get_executable_info("nope", str(tmp_path)) == {"exists": False}


def test_get_executable_info_existing(tmp_path):
    exe = tmp_path / "ESTAS_II"
    exe.write_text("x")
    os.chmod(exe, os.stat(exe).st_mode | stat.S_IEXEC)
    info = get_executable_info("ESTAS_II", str(tmp_path))
    assert info["exists"] is True
    assert info["path"] == str(exe)
    assert info["size"] == 1
    assert "file_type" in info          # value comes from file(1) / "Unknown" — env-dependent, don't pin
    assert "modified" in info
