"""Model Build tab as a Shiny module (Phase 4, Task 3).

`model_build_ui(id, compilers, build_types)` returns the panel *content* of
the former `panel_model_build` (the app-level panel_conditional stays in
create_ui); `model_build_server(id, state)` registers the handlers ported
verbatim from app.py: compiler_status, build_flags_info, get_target_exe_name,
target_exe_name, executable_list, executable_info, build_log,
clear_build_log, refresh_executables, init_executable_list (+ its
_exe_list_initialized guard), on_build, on_rebuild — plus the `run.build_config`
reactive.Calc registration and the `_publish_active_executable` effect (both
were placed in server() in Task 2, since they read this module's inputs).

`get_available_executables`/`get_executable_info` are thin wrappers imported
from `shiny_app.build_commands` (not duplicated); `run_control` also uses them.

Cross-namespace note: `refresh_executables` and `init_executable_list` also
update `run_executable`, a widget that now lives in the `run_control` module
(Run Model sub-tab). Those two touchpoints are routed through
`session.root_scope().make_scope("run_control")` with the bare id
`"run_executable"` — a module-scoped session that namespaces the bare id to
`run_control-run_executable` without using the banned hyphenated-literal form
(`"run_control-run_executable"`, which raises `ValueError` in `validate_id`
on read). This module's OWN `active_executable` updates stay plain.

Self-contained: imports compiler_env (find_compiler_path/is_intel_executable/
check_intel_libs_available) and build_commands; self-computes ROOT; owns the
COMPILERS/BUILD_TYPES data dicts (moved here from app.py, imported back for
the create_ui() call site). Imports nothing else from app.py.
"""
import logging
import os
import threading

from shiny import module, reactive, render, ui

try:
    from shiny_app import build_commands
    from shiny_app.compiler_env import (
        check_intel_libs_available,
        find_compiler_path,
        is_intel_executable,
    )
except ImportError:  # running as a script from inside shiny_app/
    import build_commands
    from compiler_env import check_intel_libs_available, find_compiler_path, is_intel_executable

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))

# Build configuration options
BUILD_TYPES = {
    "release": {
        "name": "Release",
        "description": "Standard optimizations (-O2). Good balance of speed and stability.",
        "flags_gfortran": "-O2 -march=native -mtune=native",
        "flags_intel": "-O2 -xHost"
    },
    "debug": {
        "name": "Debug",
        "description": "Full debugging with bounds checking, backtraces, and warnings. Catches errors but runs slower.",
        "flags_gfortran": "-g -Og -fcheck=all -fbacktrace -Wall -Wextra -pedantic -ffpe-trap=invalid,zero,overflow",
        "flags_intel": "-g -O0 -check all -traceback -warn all -fpe0"
    },
    "fast": {
        "name": "Fast",
        "description": "Aggressive optimizations (-O3, -ffast-math). Maximum speed but may hide numerical issues.",
        "flags_gfortran": "-O3 -march=native -mtune=native -funroll-loops -ffast-math -flto",
        "flags_intel": "-O3 -xHost -ipo -no-prec-div -fp-model fast=2"
    }
}

COMPILERS = {
    "gfortran": {
        "name": "GNU Fortran (gfortran)",
        "command": "gfortran",
        "description": "Free, widely available GNU Fortran compiler"
    },
    "ifort": {
        "name": "Intel Fortran (ifort)",
        "command": "ifort",
        "description": "Intel Fortran Compiler (Classic) - requires Intel oneAPI"
    },
    "ifx": {
        "name": "Intel Fortran (ifx)",
        "command": "ifx",
        "description": "Intel Fortran Compiler (LLVM-based) - requires Intel oneAPI"
    }
}


@module.ui
def model_build_ui(compilers, build_types):
    return ui.layout_columns(
        # Left column: Build Configuration
        ui.card(
            ui.card_header("Build Configuration"),

            # Compiler selection
            ui.h6("Compiler"),
            ui.input_radio_buttons(
                "build_compiler",
                None,
                choices={k: v["name"] for k, v in compilers.items()},
                selected="gfortran"
            ),
            ui.output_ui("compiler_status"),

            ui.tags.hr(),

            # Build type selection
            ui.h6("Build Type"),
            ui.input_radio_buttons(
                "build_type",
                None,
                choices={k: v["name"] for k, v in build_types.items()},
                selected="release"
            ),
            ui.output_text("build_flags_info"),

            ui.tags.hr(),

            # Build options
            ui.h6("Build Options"),
            ui.tooltip(
                ui.input_switch("build_clean_first", "Clean before build", value=False),
                "Remove all object files and rebuild from scratch. Enable when switching compilers or build types."
            ),

            ui.tags.hr(),

            # Target executable name
            ui.h6("Target Executable"),
            ui.output_ui("target_exe_name"),

            ui.tags.hr(),

            # Build actions
            ui.layout_columns(
                ui.tooltip(
                    ui.input_action_button("btn_build", "Build", class_="btn-primary w-100"),
                    "Compile changed source files and link the executable"
                ),
                ui.tooltip(
                    ui.input_action_button("btn_rebuild", "Rebuild All", class_="btn-warning w-100"),
                    "Clean and recompile all source files from scratch"
                ),
                col_widths=[6, 6]
            ),
            ui.tags.small("Creates named executable based on compiler and build type.", class_="text-muted mt-2"),

            fill=False
        ),

        # Middle column: Executable Selection
        ui.card(
            ui.card_header("Available Executables"),
            ui.output_ui("executable_list"),
            ui.input_action_button("btn_refresh_executables", "Refresh List", class_="btn-secondary w-100 mt-2"),

            ui.tags.hr(),

            ui.h6("Select for Run"),
            ui.input_select(
                "active_executable",
                None,
                choices=["ESTAS_II"],
                selected="ESTAS_II"
            ),
            ui.output_ui("executable_info"),

            fill=False
        ),

        # Right column: Build Log
        ui.card(
            ui.card_header(
                ui.div(
                    "Build Log",
                    ui.input_action_button("btn_clear_build_log", "Clear", class_="btn-sm btn-outline-secondary float-end"),
                    class_="d-flex justify-content-between align-items-center w-100"
                )
            ),
            ui.output_text_verbatim("build_log", placeholder=True),
            style="height: 500px; overflow-y: auto;",
            fill=True
        ),

        col_widths=[3, 3, 6]
    )


@module.server
def model_build_server(input, output, session, state):
    run = state.run

    def get_available_executables():
        """Scan for available executable files (thin wrapper)."""
        return build_commands.get_available_executables(ROOT)

    def get_executable_info(exe_name):
        """Get information about an executable (thin wrapper)."""
        return build_commands.get_executable_info(exe_name, ROOT)

    @render.ui
    def compiler_status():
        """Check if selected compiler is available"""
        compiler = input.build_compiler()
        compiler_info = COMPILERS.get(compiler, {})
        cmd = compiler_info.get("command", compiler)

        try:
            path, version = find_compiler_path(cmd)
            if path:
                # Show short path for display
                display_path = path if len(path) < 40 else "..." + path[-37:]
                return ui.div(
                    ui.tags.small(f"✓ {cmd} available", class_="text-success"),
                    ui.tags.br(),
                    ui.tags.small(version or "Unknown version", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.small(display_path, class_="text-muted", style="font-size: 9px;")
                )
            else:
                return ui.div(
                    ui.tags.small(f"✗ {cmd} not found", class_="text-danger"),
                    ui.tags.br(),
                    ui.tags.small(compiler_info.get("description", ""), class_="text-muted")
                )
        except Exception as e:
            return ui.div(ui.tags.small(f"Error checking compiler: {e}", class_="text-warning"))

    @render.text
    def build_flags_info():
        """Show compiler flags for selected build type and compiler"""
        build_type = input.build_type()
        compiler = input.build_compiler()

        type_info = BUILD_TYPES.get(build_type, {})
        flag_key = "flags_intel" if compiler in ["ifort", "ifx"] else "flags_gfortran"
        flags = type_info.get(flag_key, type_info.get("flags_gfortran", ""))

        return f"Flags: {flags}"

    def get_target_exe_name():
        """Generate executable name based on compiler and build type (thin wrapper)."""
        try:
            compiler = input.build_compiler()
            build_type = input.build_type()
        except Exception:
            return "ESTAS_II_gf_release"
        return build_commands.target_exe_name(compiler, build_type)

    @render.ui
    def target_exe_name():
        """Display the target executable name that will be built"""
        exe_name = get_target_exe_name()
        return ui.div(
            ui.tags.code(exe_name, class_="fs-5"),
            class_="p-2 bg-light border rounded"
        )

    @render.ui
    def executable_list():
        """Display list of available executables with info"""
        # Depend on reactive value to trigger refresh
        run.exe_list_version.get()
        executables = get_available_executables()
        if not executables:
            return ui.div(ui.tags.em("No executables found. Build the model first.", class_="text-muted"))

        items = []
        for exe in executables:
            info = get_executable_info(exe)
            if info["exists"]:
                # Determine build type from executable name
                if "_debug" in exe or "_gf_debug" in exe:
                    badge_class = "bg-warning"
                    badge_text = "debug"
                elif "_fast" in exe or "_gf_fast" in exe:
                    badge_class = "bg-info"
                    badge_text = "fast"
                elif "_release" in exe or "_gf_release" in exe:
                    badge_class = "bg-success"
                    badge_text = "release"
                else:
                    # Default executable (no suffix) - treat as release
                    badge_class = "bg-success"
                    badge_text = "release"

                # Add Intel indicator
                compiler_badge = None
                if is_intel_executable(exe):
                    intel_available, _ = check_intel_libs_available()
                    if intel_available:
                        compiler_badge = ui.tags.span("Intel", class_="badge bg-primary ms-1")
                    else:
                        compiler_badge = ui.tags.span("Intel ⚠", class_="badge bg-danger ms-1",
                                                      title="Intel runtime libraries not found")

                items.append(
                    ui.div(
                        ui.tags.span(exe, class_="fw-bold"),
                        ui.tags.span(badge_text, class_=f"badge {badge_class} ms-2"),
                        compiler_badge,
                        ui.tags.br(),
                        ui.tags.small(f"Modified: {info['modified']}", class_="text-muted"),
                        ui.tags.small(f" | Size: {info['size'] / 1024:.1f} KB", class_="text-muted"),
                        class_="mb-2 p-2 border rounded"
                    )
                )
        return ui.div(*items)

    @render.ui
    def executable_info():
        """Display info about the selected active executable"""
        exe_name = input.active_executable()
        info = get_executable_info(exe_name)

        if not info["exists"]:
            return ui.div(
                ui.tags.small(f"✗ {exe_name} not found", class_="text-danger"),
                ui.tags.br(),
                ui.tags.small("Build the model to create this executable.", class_="text-muted")
            )

        return ui.div(
            ui.tags.small("✓ Ready to run", class_="text-success"),
            ui.tags.br(),
            ui.tags.small(f"Last built: {info['modified']}", class_="text-muted")
        )

    @render.text
    def build_log():
        """Render the build log - polls every 0.5s for updates"""
        reactive.invalidate_later(0.5)
        if not run.build_log_lines:
            return "Build log will appear here when you start a build..."
        return "".join(run.build_log_lines[-200:])  # Last 200 lines

    @reactive.effect
    @reactive.event(input.btn_clear_build_log)
    def clear_build_log():
        """Clear the build log"""
        run.build_log_lines.clear()

    @reactive.effect
    @reactive.event(input.btn_refresh_executables)
    def refresh_executables():
        """Refresh the executable list"""
        # Increment to trigger re-render of executable_list UI
        run.exe_list_version.set(run.exe_list_version.get() + 1)
        executables = get_available_executables()
        choices = {e: e for e in executables} if executables else {"ESTAS_II": "ESTAS_II"}
        ui.update_select("active_executable", choices=choices)
        # sibling: run_executable lives in the run_control module (Run Model tab)
        rc = session.root_scope().make_scope("run_control")
        ui.update_select("run_executable", choices=choices, session=rc)

    # Initialize executable list on session start (runs once)
    _exe_list_initialized = [False]

    @reactive.effect
    def init_executable_list():
        """Populate executable list on startup (runs once)"""
        if _exe_list_initialized[0]:
            return
        _exe_list_initialized[0] = True
        executables = get_available_executables()
        if executables:
            choices = {e: e for e in executables}
            ui.update_select("active_executable", choices=choices)
            # sibling: run_executable lives in the run_control module (Run Model tab)
            rc = session.root_scope().make_scope("run_control")
            ui.update_select("run_executable", choices=choices, session=rc)
            logger.info(f"Initialized executable list with {len(executables)} executables: {executables}")

    @reactive.calc
    def _build_config():
        return {
            "compiler": input.build_compiler(),
            "build_type": input.build_type(),
            "exe_name": get_target_exe_name(),
            "clean_first": input.build_clean_first(),
        }
    run.build_config = _build_config

    # model_build -> dashboard: the Model Build tab's active_executable selector
    @reactive.effect
    def _publish_active_executable():
        run.active_executable.set(input.active_executable())

    @reactive.effect
    @reactive.event(input.btn_build)
    def on_build():
        """Handle Build button click - builds named executable"""
        logger.info("User clicked Build button")

        compiler = input.build_compiler()
        build_type = input.build_type()
        clean_first = input.build_clean_first()
        exe_name = get_target_exe_name()

        # Find the full path to the compiler
        compiler_path, compiler_version = find_compiler_path(compiler)
        if not compiler_path:
            run.build_log_lines.clear()
            run.build_log_lines.extend([
                "=" * 50 + "\n",
                f"ERROR: Compiler '{compiler}' not found!\n",
                "=" * 50 + "\n",
                "Please ensure the compiler is installed and accessible.\n",
                "For Intel compilers, check /opt/intel/oneapi/compiler/\n",
            ])
            return

        run.build_log_lines.clear()
        run.build_log_lines.extend([
            "=" * 50 + "\n",
            f"Building: {exe_name}\n",
            "=" * 50 + "\n",
            f"Compiler: {compiler_path}\n",
            f"Version: {compiler_version or 'Unknown'}\n",
            f"Build Type: {build_type}\n",
        ])

        # Capture variables for thread closure
        _compiler_path = compiler_path
        _build_type = build_type
        _clean_first = clean_first
        _exe_name = exe_name

        def _do_build():
            run.execute_build(
                compiler_path=_compiler_path,
                build_type=_build_type,
                exe_name=_exe_name,
                clean_first=_clean_first,
                action_name="Build",
            )

        logger.info("Starting build thread")
        threading.Thread(target=_do_build, daemon=True, name="BuildThread").start()

    @reactive.effect
    @reactive.event(input.btn_rebuild)
    def on_rebuild():
        """Handle Rebuild All button click - forces clean build"""
        logger.info("User clicked Rebuild All button")

        compiler = input.build_compiler()
        build_type = input.build_type()
        exe_name = get_target_exe_name()

        # Find the full path to the compiler
        compiler_path, compiler_version = find_compiler_path(compiler)
        if not compiler_path:
            run.build_log_lines.clear()
            run.build_log_lines.extend([
                "=" * 50 + "\n",
                f"ERROR: Compiler '{compiler}' not found!\n",
                "=" * 50 + "\n",
                "Please ensure the compiler is installed and accessible.\n",
                "For Intel compilers, check /opt/intel/oneapi/compiler/\n",
            ])
            return

        run.build_log_lines.clear()
        run.build_log_lines.extend([
            "=" * 50 + "\n",
            f"Full Rebuild: {exe_name}\n",
            "=" * 50 + "\n",
            f"Compiler: {compiler_path}\n",
            f"Version: {compiler_version or 'Unknown'}\n",
            f"Build Type: {build_type}\n",
        ])

        # Capture variables for thread closure
        _compiler_path = compiler_path
        _build_type = build_type
        _exe_name = exe_name

        def _do_rebuild():
            run.execute_build(
                compiler_path=_compiler_path,
                build_type=_build_type,
                exe_name=_exe_name,
                clean_first=True,
                action_name="Rebuild",
            )

        threading.Thread(target=_do_rebuild, daemon=True, name="RebuildThread").start()
