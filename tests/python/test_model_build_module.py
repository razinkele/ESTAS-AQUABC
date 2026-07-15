try:
    from shiny_app.modules.model_build import model_build_ui
except ImportError:
    from modules.model_build import model_build_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_model_build_ui_namespaces_ids_and_renders_content():
    compilers = {"gfortran": {"name": "GNU Fortran"}}
    build_types = {"release": {"name": "Release"}}
    html = str(model_build_ui("model_build", compilers, build_types))

    for raw in (
        "build_compiler",
        "build_type",
        "build_clean_first",
        "btn_build",
        "btn_rebuild",
        "btn_refresh_executables",
        "active_executable",
        "btn_clear_build_log",
        "compiler_status",
        "build_flags_info",
        "target_exe_name",
        "executable_list",
        "executable_info",
        "build_log",
    ):
        assert nid("model_build", raw) in html, f"missing namespaced id for {raw}"

    # content markers migrated from the deleted test_ui_panels.py case
    for marker in ("Build Configuration", "Available Executables", "Build Log", "GNU Fortran", "Release"):
        assert marker in html, f"model_build_ui missing marker {marker!r}"

    # nav wrapper stays in create_ui, not in the module content
    assert "input.navigation" not in html
