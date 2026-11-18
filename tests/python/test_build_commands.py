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
    # the model prepends the resuspension input folder, so only the bare name is passed
    assert assemble_estas_command("", "INPUT.txt", "", False, None, "INPUTS/SHEAR.txt", DC) == \
        ["./ESTAS_II", "INPUT.txt", "WCONST_01.txt", "PELAGIC_OUTPUT.bin", "SHEAR.txt"]


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
                                  "SHEAR.txt", DC) == \
        ["./ESTAS_II", "INPUT.txt", "C.txt", "OUT.bin", "SHEAR.txt"]


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


def test_three_or_more_arguments_mean_binary_only_output():
    """ESTAS_II.f90 sets PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT for 3+ arguments."""
    from shiny_app.build_commands import is_binary_only
    assert not is_binary_only(["./ESTAS_II", "INPUT.txt"])
    assert not is_binary_only(["./ESTAS_II", "INPUT.txt", "C.txt"])
    assert is_binary_only(["./ESTAS_II", "INPUT.txt", "C.txt", "OUT.bin"])
    # a shear file alone forces the placeholder binary prefix, hence binary-only too
    assert is_binary_only(assemble_estas_command("", "INPUT.txt", "", False, None, "S.txt", DC))


def test_critical_shear_choices_list_threshold_tables_by_bare_name(tmp_path):
    """Argument 4 is the per-box critical-stress table; the time series aborts the run."""
    from shiny_app.build_commands import critical_shear_choices
    for name in ("CRITICAL_SHEAR_STRESSES.txt", "SHEAR_STRESSES_TS.txt", "SHEAR_STRESS.txt",
                 "critical_shear_muddy.txt", "CRITICAL_SHEAR.bak"):
        (tmp_path / name).write_text("x\n")
    assert critical_shear_choices(str(tmp_path)) == {
        "": "(not used)",
        "CRITICAL_SHEAR_STRESSES.txt": "CRITICAL_SHEAR_STRESSES.txt",
        "critical_shear_muddy.txt": "critical_shear_muddy.txt",
    }
    assert critical_shear_choices(str(tmp_path / "missing")) == {"": "(not used)"}
