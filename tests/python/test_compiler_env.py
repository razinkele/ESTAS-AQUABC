"""Tests for shiny_app.compiler_env (extracted from app.py)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "shiny_app", "compiler_env.py")
_spec = importlib.util.spec_from_file_location("compiler_env", _PATH)
compiler_env = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(compiler_env)


def test_module_exposes_all_functions():
    for name in ("find_compiler_path", "is_intel_executable", "get_intel_library_paths",
                 "check_intel_libs_available", "get_run_environment",
                 "get_intel_setvars_path", "build_intel_wrapped_command"):
        assert callable(getattr(compiler_env, name)), name

def test_search_paths_present():
    assert isinstance(compiler_env.INTEL_COMPILER_SEARCH_PATHS, list)

def test_is_intel_executable_gfortran_false():
    # gfortran is not an Intel executable
    assert compiler_env.is_intel_executable("gfortran") is False

def test_build_intel_wrapped_command_returns_tuple():
    # Function always returns (command, use_shell_bool) per its docstring.
    out = compiler_env.build_intel_wrapped_command(["echo", "hi"])
    assert isinstance(out, tuple)
    assert len(out) == 2
    assert isinstance(out[1], bool)
