#!/usr/bin/env python3
import os
import subprocess
import threading
import logging
import sys
import shutil
import time
import select
import signal
import traceback
import re
import shlex
from datetime import datetime, date, timedelta

# Try to import markdown for help rendering
try:
    import markdown
    MARKDOWN_AVAILABLE = True
except ImportError:
    MARKDOWN_AVAILABLE = False

# Add the script's directory and parent to path for module imports
# This ensures imports work both locally and on Shiny server
_script_dir = os.path.dirname(os.path.realpath(__file__))
_parent_dir = os.path.dirname(_script_dir)
if _script_dir not in sys.path:
    sys.path.insert(0, _script_dir)
if _parent_dir not in sys.path:
    sys.path.insert(0, _parent_dir)

import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
import networkx as nx
import ipyleaflet as L
from ipywidgets import HTML

from shiny import App, ui, reactive, render, req
from shinywidgets import output_widget, render_widget

# Import parameter parser (try both absolute and relative imports)
try:
    from shiny_app.parameter_parser import ParameterFile, PARAMETER_CATEGORIES, load_parameters
    from shiny_app.ic_parser import (
        ICFile, STATE_VARIABLE_CATEGORIES, STATE_VARIABLES, get_available_ic_files,
        get_variable_display_name, get_variable_info, get_grouped_variable_choices, CSV_COLUMN_INFO
    )
    from shiny_app.options_parser import (
        ModelOptionsFile, ExtraConstantsFile, MODEL_OPTIONS, EXTRA_CONSTANTS, OPTION_CATEGORIES,
        load_model_options, load_extra_constants
    )
    from shiny_app.mass_balance import MassBalanceCalculator, load_stoichiometry_from_params
    from shiny_app.simulation_config import (
        SimulationConfigFile, SimulationConfig, load_simulation_config,
        TIME_STEP_PRESETS, OUTPUT_INTERVAL_PRESETS, days_to_date, date_to_days
    )
except ImportError:
    # Fallback for when running from within shiny_app directory
    from parameter_parser import ParameterFile, PARAMETER_CATEGORIES, load_parameters
    from ic_parser import (
        ICFile, STATE_VARIABLE_CATEGORIES, STATE_VARIABLES, get_available_ic_files,
        get_variable_display_name, get_variable_info, get_grouped_variable_choices, CSV_COLUMN_INFO
    )
    from options_parser import (
        ModelOptionsFile, ExtraConstantsFile, MODEL_OPTIONS, EXTRA_CONSTANTS, OPTION_CATEGORIES,
        load_model_options, load_extra_constants
    )
    from mass_balance import MassBalanceCalculator, load_stoichiometry_from_params
    from simulation_config import (
        SimulationConfigFile, SimulationConfig, load_simulation_config,
        TIME_STEP_PRESETS, OUTPUT_INTERVAL_PRESETS, days_to_date, date_to_days
    )

# Import observation comparison (try both paths)
try:
    from shiny_app.observation_compare import (
        ObservationData, ModelObservationComparison, create_sample_observations
    )
except ImportError:
    from observation_compare import (
        ObservationData, ModelObservationComparison, create_sample_observations
    )

# Import observation file loader (try both paths)
try:
    from shiny_app.obs_loader import (
        scan_observations_directory, load_observation_file as load_obs_file,
        get_file_preview, ObservationFile, LoadedObservations, 
        get_variable_description, STATE_VARIABLE_INDEX
    )
except ImportError:
    from obs_loader import (
        scan_observations_directory, load_observation_file as load_obs_file,
        get_file_preview, ObservationFile, LoadedObservations,
        get_variable_description, STATE_VARIABLE_INDEX
    )

# Import scenario manager (try both paths)
try:
    from shiny_app.scenarios import (
        Scenario, ScenarioManager, load_scenario_manager, get_scenarios_dir
    )
except ImportError:
    from scenarios import (
        Scenario, ScenarioManager, load_scenario_manager, get_scenarios_dir
    )

# Import utility functions (extracted for testability)
try:
    from shiny_app.utils import (
        count_file_lines_fast, read_pelagic_binary, read_pelagic_text,
        validate_constants_file, PELAGIC_BOX_COLUMNS, REQUIRED_MODEL_CONSTANTS
    )
except ImportError:
    from utils import (
        count_file_lines_fast, read_pelagic_binary, read_pelagic_text,
        validate_constants_file, PELAGIC_BOX_COLUMNS, REQUIRED_MODEL_CONSTANTS
    )

# Path-traversal-safe filename resolution (stdlib-only, extracted for testability)
try:
    from shiny_app.safe_resolve import safe_resolve
except ImportError:
    from safe_resolve import safe_resolve

# Intel/compiler detection and run-environment helpers (stdlib-only, extracted for testability)
try:
    from shiny_app.compiler_env import (
        find_compiler_path, is_intel_executable, get_intel_library_paths,
        check_intel_libs_available, get_run_environment, get_intel_setvars_path,
        build_intel_wrapped_command,
    )
except ImportError:
    from compiler_env import (
        find_compiler_path, is_intel_executable, get_intel_library_paths,
        check_intel_libs_available, get_run_environment, get_intel_setvars_path,
        build_intel_wrapped_command,
    )

# Input-file analysis and validation helpers (stdlib-only, extracted for testability)
try:
    from shiny_app.input_analysis import (
        analyze_input_file, get_input_file_categories, validate_required_inputs,
        INPUT_FILE_CATEGORIES,
    )
except ImportError:
    from input_analysis import (
        analyze_input_file, get_input_file_categories, validate_required_inputs,
        INPUT_FILE_CATEGORIES,
    )

# Output/box file discovery helpers (extracted for testability)
try:
    from shiny_app.file_locators import (
        get_output_folder, find_pelagic_box_file, get_available_boxes, get_timeseries_variables,
    )
except ImportError:
    from file_locators import (
        get_output_folder, find_pelagic_box_file, get_available_boxes, get_timeseries_variables,
    )

# Import diagnostics panel (process rate analysis UI)
try:
    from shiny_app.diagnostics import diagnostics_ui, diagnostics_server
except ImportError:
    from diagnostics import diagnostics_ui, diagnostics_server

# Import UI script fragments (phase-2 create_ui() split)
try:
    from shiny_app.ui_scripts import (
        reload_script, nav_script, settings_script,
        help_script, changelog_script, theme_script,
    )
except ImportError:
    from ui_scripts import (
        reload_script, nav_script, settings_script,
        help_script, changelog_script, theme_script,
    )

# Import UI content panel fragments (phase-2b create_ui() split)
try:
    from shiny_app.ui_panels import (
        panel_dashboard, panel_model_build, panel_model_control, panel_input_files,
        panel_parameters, panel_initial_conditions, panel_model_options, panel_sim_config,
        panel_scenarios, panel_plot, panel_mass_balance, panel_observations,
        panel_map, panel_model_structure,
    )
except ImportError:
    from ui_panels import (
        panel_dashboard, panel_model_build, panel_model_control, panel_input_files,
        panel_parameters, panel_initial_conditions, panel_model_options, panel_sim_config,
        panel_scenarios, panel_plot, panel_mass_balance, panel_observations,
        panel_map, panel_model_structure,
    )

# Import UI chrome fragments (phase-2c create_ui() split)
try:
    from shiny_app.ui_chrome import (
        build_sidebar, app_header, external_css,
        settings_offcanvas, help_offcanvas, changelog_offcanvas,
    )
except ImportError:
    from ui_chrome import (
        build_sidebar, app_header, external_css,
        settings_offcanvas, help_offcanvas, changelog_offcanvas,
    )

try:
    from shiny_app import build_commands
except ImportError:
    import build_commands

try:
    from shiny_app import box_network
except ImportError:
    import box_network

try:
    from shiny_app import output_data
except ImportError:
    import output_data

# Configure logging
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    stream=sys.stdout
)
logger = logging.getLogger("AQUABC")

# Theme is handled by custom CSS (shiny_app/www/aquabc.css)
logger.info("=" * 60)
logger.info("AQUABC Application starting...")
logger.info("=" * 60)

# Constants
MAX_LOG_LENGTH = 1000000  # 1MB buffer for run log
MIN_SMOOTH_WINDOW = 2
DEFAULT_PLOT_ROWS = 10000  # Max rows to read for plotting to avoid OOM
# REQUIRED_MODEL_CONSTANTS imported from shiny_app.utils


# count_file_lines_fast imported from shiny_app.utils

# Fix ROOT path when running via symlink
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))
INPUTS_DIR = os.path.join(ROOT, 'INPUTS')
OUTPUT_CSV = os.path.join(ROOT, 'OUTPUT.csv')


# safe_resolve() moved to shiny_app/safe_resolve.py (imported above) so the pure
# path-safety helper can be unit-tested without importing this whole module.

# PELAGIC_BOX_COLUMNS imported from shiny_app.utils

# get_output_folder() and find_pelagic_box_file() moved to
# shiny_app/file_locators.py (imported above) so they can be unit-tested
# without importing this whole module.

# read_pelagic_binary and read_pelagic_text imported from shiny_app.utils

# Startup diagnostics
logger.info("=== Path Configuration ===")
logger.info(f"Script location: {os.path.realpath(__file__)}")
logger.info(f"ROOT directory: {ROOT}")
logger.info(f"INPUTS_DIR: {INPUTS_DIR}")
logger.info(f"OUTPUT_CSV: {OUTPUT_CSV}")

logger.info("=== Directory Checks ===")
if os.path.exists(ROOT):
    logger.info(f"✓ ROOT directory exists")
    logger.info(f"  ROOT is readable: {os.access(ROOT, os.R_OK)}")
    logger.info(f"  ROOT is writable: {os.access(ROOT, os.W_OK)}")
else:
    logger.error(f"✗ ROOT directory does NOT exist: {ROOT}")

if os.path.exists(INPUTS_DIR):
    logger.info(f"✓ INPUTS directory exists")
    logger.info(f"  INPUTS is readable: {os.access(INPUTS_DIR, os.R_OK)}")
    logger.info(f"  INPUTS is writable: {os.access(INPUTS_DIR, os.W_OK)}")
    try:
        input_files = [f for f in os.listdir(INPUTS_DIR) if os.path.isfile(os.path.join(INPUTS_DIR, f))]
        logger.info(f"  Found {len(input_files)} input files: {', '.join(input_files[:5])}")
        if len(input_files) > 5:
            logger.info(f"    ... and {len(input_files) - 5} more")
    except Exception as e:
        logger.error(f"  Error listing INPUTS directory: {e}")
else:
    logger.error(f"✗ INPUTS directory does NOT exist: {INPUTS_DIR}")

logger.info("=== Output File Checks ===")
if os.path.exists(OUTPUT_CSV):
    logger.info(f"✓ OUTPUT.csv exists")
    file_size = os.path.getsize(OUTPUT_CSV)
    logger.info(f"  File size: {file_size:,} bytes ({file_size / 1024 / 1024:.2f} MB)")
    logger.info(f"  File is readable: {os.access(OUTPUT_CSV, os.R_OK)}")
    logger.info(f"  Last modified: {datetime.fromtimestamp(os.path.getmtime(OUTPUT_CSV)).strftime('%Y-%m-%d %H:%M:%S')}")
    try:
        # Try to read header
        with open(OUTPUT_CSV, 'r') as f:
            first_line = f.readline().strip()
            logger.info(f"  Header preview: {first_line[:100]}")
        # Count lines (quick estimate)
        try:
            result = subprocess.run(['wc', '-l', OUTPUT_CSV], capture_output=True, text=True, timeout=2)
            if result.returncode == 0:
                line_count = result.stdout.split()[0]
                logger.info(f"  Line count: {line_count}")
        except Exception:
            pass
    except Exception as e:
        logger.warning(f"  Could not read OUTPUT.csv header: {e}")
else:
    logger.warning(f"⚠ OUTPUT.csv does NOT exist yet: {OUTPUT_CSV}")
    logger.info(f"  This is normal if the model hasn't been run yet")

logger.info("=== Environment ===")
logger.info(f"Python version: {sys.version}")
logger.info(f"Working directory: {os.getcwd()}")
logger.info(f"User: {os.environ.get('USER', 'unknown')}")
logger.info(f"HOME: {os.environ.get('HOME', 'unknown')}")

logger.info("=== Module Versions ===")
try:
    logger.info(f"pandas: {pd.__version__}")
except Exception:
    logger.warning("Could not get pandas version")

try:
    import plotly
    logger.info(f"plotly: {plotly.__version__}")
except Exception:
    logger.warning("Could not get plotly version")

try:
    import shiny
    logger.info(f"shiny: {shiny.__version__}")
except Exception:
    logger.warning("Could not get shiny version")

logger.info("=== Configuration ===")
logger.info(f"MAX_LOG_LENGTH: {MAX_LOG_LENGTH}")
logger.info(f"MIN_SMOOTH_WINDOW: {MIN_SMOOTH_WINDOW}")
logger.info(f"DEFAULT_PLOT_ROWS: {DEFAULT_PLOT_ROWS}")

logger.info("=" * 60)
logger.info("Startup diagnostics complete. Building UI...")
logger.info("=" * 60)


# validate_constants_file imported from shiny_app.utils


# Navigation menu choices with icons
NAV_CHOICES = {
    "nav_dashboard": ("bi-speedometer2", "Dashboard"),
    "nav_model_structure": ("bi-diagram-3", "Model Structure"),
    "nav_model_build": ("bi-hammer", "Model Build"),
    "nav_model_control": ("bi-sliders", "Model Config"),
    "nav_input_files": ("bi-file-earmark-text", "Input Files"),
    "nav_parameters": ("bi-gear-wide-connected", "Parameters"),
    "nav_initial_conditions": ("bi-water", "Initial Cond."),
    "nav_model_options": ("bi-toggles", "Model Options"),
    "nav_scenarios": ("bi-collection", "Scenarios"),
    "nav_plot": ("bi-graph-up", "Plots"),
    "nav_mass_balance": ("bi-arrows-angle-expand", "Mass Balance"),
    "nav_observations": ("bi-binoculars", "Observations"),
    "nav_map": ("bi-globe", "Map"),
    "nav_diagnostics": ("bi-shield-check", "Diagnostics"),
}

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

# INTEL_COMPILER_SEARCH_PATHS and the Intel/compiler detection helpers
# (find_compiler_path, is_intel_executable, get_intel_library_paths,
# check_intel_libs_available, get_run_environment, get_intel_setvars_path,
# build_intel_wrapped_command) moved to shiny_app/compiler_env.py (imported above)
# so they can be unit-tested without importing this whole module.

# Add FORC_TS files (boundary forcing)
for i in range(1, 20):
    INPUT_FILE_CATEGORIES[f"FORC_TS_{i}.txt"] = {
        "category": "Boundary Forcing Timeseries",
        "description": f"Boundary forcing time series #{i} - water quality concentrations at open boundaries",
        "structure": "Standard timeseries format with 30 state variables (nutrients, phytoplankton, etc.)",
        "model_use": "Specifies concentrations entering at open boundaries (Baltic Sea, rivers)",
        "is_timeseries": True,
    }

# Add SETTLING_VELOCITY_TS files
settling_var_names = {
    1: "Diatoms carbon",
    2: "Detritus particulate organic carbon",
    3: "Biogenic silica",
    4: "Cyanobacteria carbon",
    5: "Other phytoplankton carbon",
    6: "Fixing cyanobacteria carbon",
}
for i in range(1, 7):
    INPUT_FILE_CATEGORIES[f"SETTLING_VELOCITY_TS_{i}.txt"] = {
        "category": "Settling Velocity Timeseries",
        "description": f"Settling velocity time series for {settling_var_names.get(i, f'variable {i}')}",
        "structure": "Standard timeseries format",
        "model_use": "Controls vertical transport of particulate matter to sediments",
        "is_timeseries": True,
    }

# Add BATHYMETRY files
for i in range(1, 26):
    INPUT_FILE_CATEGORIES[f"BATHYMETRY_{i}.txt"] = {
        "category": "Box Geometry",
        "description": f"Bathymetry profile for model box #{i}",
        "structure": "Layer table: LAYER_NO, UPPER/LOWER_ELEVATION, UPPER/LOWER_AREA, UPPER/LOWER_LENGTH",
        "model_use": "Defines vertical stratification, volumes, and surface areas for box calculations",
        "is_timeseries": False,
    }

# Constants files
for i in range(1, 5):
    INPUT_FILE_CATEGORIES[f"WCONST_0{i}.txt"] = {
        "category": "Model Constants",
        "description": f"Water column constants set #{i} - biogeochemical parameters",
        "structure": "Parameter index | name | value | comment format",
        "model_use": "Defines kinetic rates, stoichiometric ratios, and half-saturation constants",
        "is_timeseries": False,
    }

# INPUT_FILE_CATEGORIES.update({...}), REQUIRED_INPUT_FILES, RECOMMENDED_INPUT_FILES,
# analyze_input_file(), get_input_file_categories(), and validate_required_inputs()
# moved to shiny_app/input_analysis.py (imported above) so they can be
# unit-tested without importing this whole module. INPUT_FILE_CATEGORIES is a
# shared dict object, so the FORC_TS/SETTLING_VELOCITY_TS/BATHYMETRY/WCONST_0
# loops above still mutate it in place after the import runs.

# get_available_boxes() and get_timeseries_variables() moved to
# shiny_app/file_locators.py (imported above) so they can be unit-tested
# without importing this whole module.


def create_ui():
    """Create the application UI layout."""
    # Hidden input to track navigation state (for Shiny reactivity)
    nav_input = ui.input_text("navigation", None, value="nav_dashboard")
    nav_input_hidden = ui.tags.div(nav_input, style="display: none;")

    # === DIAGNOSTICS PANEL (built by diagnostics module) ===
    panel_diagnostics = diagnostics_ui()

    # === COMBINE ALL PANELS ===
    main_content = ui.div(
        {"class": "main-content"},
        nav_input_hidden,
        panel_dashboard(),
        panel_model_structure(),
        panel_model_build(COMPILERS, BUILD_TYPES),
        panel_model_control(),
        panel_input_files(),
        panel_parameters(),
        panel_initial_conditions(),
        panel_model_options(),
        panel_sim_config(),
        panel_scenarios(),
        panel_plot(MIN_SMOOTH_WINDOW),
        panel_mass_balance(),
        panel_observations(),
        panel_map(),
        panel_diagnostics,
    )

    # Sidebar container with navigation and main content
    sidebar_container = ui.div(
        {"class": "sidebar-container"},
        build_sidebar(NAV_CHOICES),
        main_content,
    )

    content = [
        external_css(),
        nav_script(),
        reload_script(),
        theme_script(),
        app_header(),
        settings_offcanvas(),
        settings_script(),
        help_offcanvas(),
        help_script(),
        changelog_offcanvas(),
        changelog_script(),
        sidebar_container,
    ]

    return ui.page_fillable(*content, title="AQUABC")

app_ui = create_ui()

# Server

def server(input, output, session):
    logger.info("=" * 60)
    logger.info("Server function initializing...")
    logger.info(f"Session ID: {session.id if hasattr(session, 'id') else 'N/A'}")
    logger.info("=" * 60)

    # =================
    # Log Copy Handlers
    # =================
    @reactive.effect
    @reactive.event(input.btn_copy_dashboard_log)
    async def copy_dashboard_log():
        """Copy dashboard run log to clipboard via client-side JS"""
        log_content = "".join(_log_lines)
        if not log_content:
            log_content = "Log is empty."
        await session.send_custom_message("copy_to_clipboard", log_content)
        ui.notification_show("Run log copied to clipboard!", type="message", duration=2)

    @reactive.effect
    @reactive.event(input.btn_copy_mini_log)
    async def copy_mini_log():
        """Copy mini run log to clipboard via client-side JS"""
        log_content = "".join(_log_lines)
        if not log_content:
            log_content = "Log is empty."
        await session.send_custom_message("copy_to_clipboard", log_content)
        ui.notification_show("Run log copied to clipboard!", type="message", duration=2)

    # Dashboard status info
    @render.text
    def status_info():
        """Display system status on dashboard"""
        status_lines = []
        status_lines.append(f"Working Directory: {os.path.basename(ROOT)}")

        # Check if OUTPUT.csv exists
        if os.path.exists(OUTPUT_CSV):
            mtime = datetime.fromtimestamp(os.path.getmtime(OUTPUT_CSV))
            status_lines.append(f"Last Run: {mtime.strftime('%Y-%m-%d %H:%M:%S')}")
            lines = count_file_lines_fast(OUTPUT_CSV)
            status_lines.append(f"Output Rows: {lines:,}")
        else:
            status_lines.append("Last Run: Never")

        # Count input files (cached for performance)
        try:
            input_files_count = len([f for f in os.listdir(INPUTS_DIR) if os.path.isfile(os.path.join(INPUTS_DIR, f))])
        except Exception:
            input_files_count = 0
        status_lines.append(f"Input Files: {input_files_count}")

        # Add separator
        status_lines.append("")
        status_lines.append("─" * 40)
        status_lines.append("MODEL CONFIGURATION")
        status_lines.append("─" * 40)

        # Get current model config parameters
        try:
            exe_name = input.run_executable() or "ESTAS_II"
        except Exception:
            exe_name = "ESTAS_II"
        status_lines.append(f"Executable: {exe_name}")

        try:
            input_file = input.cmd_input_file() or "INPUT.txt"
        except Exception:
            input_file = "INPUT.txt"
        status_lines.append(f"Input File: {input_file}")

        try:
            const_file = input.cmd_constants_file() or "(model defaults)"
        except Exception:
            const_file = "(model defaults)"
        status_lines.append(f"Constants: {const_file}")

        try:
            binary_enabled = input.cmd_binary_enabled()
            if binary_enabled:
                binary_file = input.cmd_binary_filename() or "PELAGIC_OUTPUT.bin"
                status_lines.append(f"Binary Output: {binary_file}")
            else:
                status_lines.append("Binary Output: Disabled")
        except Exception:
            status_lines.append("Binary Output: Disabled")

        try:
            shear_file = input.cmd_shear_stress_file()
            if shear_file:
                status_lines.append(f"Shear Stress: {shear_file}")
        except Exception:
            pass

        # Add command line preview - build it from the values we already have
        status_lines.append("")
        status_lines.append("─" * 40)
        status_lines.append("COMMAND LINE")
        status_lines.append("─" * 40)

        # Build command from already-retrieved values
        cmd_parts = [f"./{exe_name}", input_file]

        # Add constants file if set (not the default placeholder)
        actual_const = const_file if const_file != "(model defaults)" else ""

        # Check binary settings
        try:
            bin_enabled = input.cmd_binary_enabled()
            bin_file = input.cmd_binary_filename() if bin_enabled else ""
            if bin_enabled and not bin_file:
                bin_file = "PELAGIC_OUTPUT.bin"
        except Exception:
            bin_enabled = False
            bin_file = ""

        # Check shear file
        try:
            shear = input.cmd_shear_stress_file() or ""
        except Exception:
            shear = ""

        # If binary or shear is set, we need constants
        if (bin_enabled or shear) and not actual_const:
            actual_const = "WCONST_01.txt"

        if actual_const:
            cmd_parts.append(actual_const)

            # Binary file (needed if binary enabled or shear is set)
            if bin_enabled and bin_file:
                cmd_parts.append(bin_file)
            elif shear:
                cmd_parts.append("PELAGIC_OUTPUT.bin")

            # Shear file
            if shear:
                cmd_parts.append(shear)

        status_lines.append(" ".join(cmd_parts))

        return "\n".join(status_lines)

    # Mini run log for Model Control panel
    @render.text
    def run_log_mini():
        """Abbreviated run log for sidebar"""
        # Poll every 500ms to catch updates from background threads
        reactive.invalidate_later(0.5)
        # Return last 50 lines from shared list
        return ''.join(_log_lines[-50:])

    # Shared list for thread-safe log updates (lists are thread-safe for append)
    # Define early so it's available for quick_run
    _log_lines = ["Ready.\n"]

    # =========================================================================
    # ESTAS_II Command Line Parameter Controls
    # =========================================================================
    
    @reactive.effect
    def init_cmd_dropdowns():
        """Initialize command line parameter dropdown choices"""
        # Get available INPUT*.txt files
        input_files = {"INPUT.txt": "INPUT.txt (default)"}
        for f in sorted(os.listdir(ROOT)):
            if f.startswith("INPUT") and f.endswith(".txt") and f != "INPUT.txt":
                input_files[f] = f
        ui.update_select("cmd_input_file", choices=input_files)
        
        # Get available WCONST*.txt files for constants override (Arg 2)
        # Note: Fortran code prepends PELAGIC_INPUT_FOLDER, so just use filename
        const_files = {"": "(not used - use defaults)"}
        for f in sorted(os.listdir(INPUTS_DIR)):
            if f.startswith("WCONST") and f.endswith(".txt"):
                const_files[f] = f  # Just filename, not path
        ui.update_select("cmd_constants_file", choices=const_files)
        
        # Get available shear stress files (Arg 4)
        # Note: Fortran code uses this path directly
        shear_files = {"": "(not used)"}
        for f in sorted(os.listdir(INPUTS_DIR)):
            if "SHEAR" in f.upper() and f.endswith(".txt"):
                shear_files[os.path.join("INPUTS", f)] = f
        ui.update_select("cmd_shear_stress_file", choices=shear_files)

    # Default constants file to use when Arg 3 is set but Arg 2 is not
    DEFAULT_CONSTANTS_FILE = "WCONST_01.txt"

    def build_estas_command():
        """Build the model command from current widget values (thin wrapper).

        Reads are raw (input-not-ready -> falsy sentinel); all defaulting lives in
        build_commands.assemble_estas_command. Command format: 0 args uses INPUT.txt;
        1: INPUT_FILE; 2: +CONSTANTS; 3: +BINARY; 4: +SHEAR.
        """
        try:
            exe_name = input.run_executable()
        except Exception:
            exe_name = None
        try:
            input_file = input.cmd_input_file()
        except Exception:
            input_file = None
        try:
            const_file = input.cmd_constants_file()
        except Exception:
            const_file = None
        try:
            binary_enabled = input.cmd_binary_enabled()
        except Exception:
            binary_enabled = False
        # Read the binary filename ONLY when the switch is on (preserve original reactive deps)
        binary_filename = None
        if binary_enabled:
            try:
                binary_filename = input.cmd_binary_filename()
            except Exception:
                binary_filename = None
        try:
            shear_file = input.cmd_shear_stress_file()
        except Exception:
            shear_file = None
        return build_commands.assemble_estas_command(
            exe_name, input_file, const_file, binary_enabled,
            binary_filename, shear_file, DEFAULT_CONSTANTS_FILE)

    @render.text
    def cmd_preview():
        """Show preview of the command that will be executed"""
        cmd = build_estas_command()
        return " ".join(cmd)

    # =========================================================================
    # Model Build Panel - Server Logic
    # =========================================================================

    # Build log storage (separate from run log) - plain list for thread safety
    _build_log_lines = []

    # Reactive value to trigger executable list refresh
    _exe_list_version = reactive.Value(0)

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
        _exe_list_version.get()
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
            ui.tags.small(f"✓ Ready to run", class_="text-success"),
            ui.tags.br(),
            ui.tags.small(f"Last built: {info['modified']}", class_="text-muted")
        )

    @render.ui
    def run_executable_info():
        """Display info about the selected run executable"""
        exe_name = input.run_executable()
        info = get_executable_info(exe_name)

        if not info["exists"]:
            return ui.div(
                ui.tags.small(f"✗ {exe_name} not found. Go to Model Build to compile.", class_="text-danger")
            )

        # Determine build type from executable name
        if "_debug" in exe_name or "_gf_debug" in exe_name:
            build_info = "debug build"
        elif "_fast" in exe_name or "_gf_fast" in exe_name:
            build_info = "fast build (optimized)"
        elif "_release" in exe_name or "_gf_release" in exe_name:
            build_info = "release build"
        else:
            build_info = "release build"
        
        # Check if Intel executable needs runtime libraries
        if is_intel_executable(exe_name):
            intel_available, intel_path = check_intel_libs_available()
            if intel_available:
                return ui.div(
                    ui.tags.small(f"✓ {build_info} (Intel), {info['size'] / 1024:.1f} KB", class_="text-success"),
                    ui.tags.br(),
                    ui.tags.small(f"Intel libs: {intel_path[:50]}...", class_="text-muted", style="font-size: 9px;")
                )
            else:
                return ui.div(
                    ui.tags.small(f"⚠ {build_info} (Intel), {info['size'] / 1024:.1f} KB", class_="text-warning"),
                    ui.tags.br(),
                    ui.tags.small("⚠ Intel runtime libraries not found!", class_="text-warning"),
                    ui.tags.br(),
                    ui.tags.small("Run 'source /opt/intel/oneapi/setvars.sh' first, or use gfortran builds.", class_="text-muted", style="font-size: 9px;")
                )
        
        return ui.div(
            ui.tags.small(f"✓ {build_info}, {info['size'] / 1024:.1f} KB", class_="text-success")
        )

    @render.text
    def build_log():
        """Render the build log - polls every 0.5s for updates"""
        reactive.invalidate_later(0.5)
        if not _build_log_lines:
            return "Build log will appear here when you start a build..."
        return "".join(_build_log_lines[-200:])  # Last 200 lines

    @reactive.effect
    @reactive.event(input.btn_clear_build_log)
    def clear_build_log():
        """Clear the build log"""
        _build_log_lines.clear()

    @reactive.effect
    @reactive.event(input.btn_refresh_executables)
    def refresh_executables():
        """Refresh the executable list"""
        # Increment to trigger re-render of executable_list UI
        _exe_list_version.set(_exe_list_version.get() + 1)
        executables = get_available_executables()
        choices = {e: e for e in executables} if executables else {"ESTAS_II": "ESTAS_II"}
        ui.update_select("active_executable", choices=choices)
        ui.update_select("run_executable", choices=choices)

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
            ui.update_select("run_executable", choices=choices)
            logger.info(f"Initialized executable list with {len(executables)} executables: {executables}")

    @reactive.effect
    @reactive.event(input.goto_build)
    def navigate_to_build():
        """Navigate to the Model Build panel"""
        ui.update_radio_buttons("navigation", selected="nav_model_build")

    @reactive.effect
    @reactive.event(input.goto_model_config)
    def navigate_to_model_config():
        """Navigate to the Model Config panel from dashboard"""
        ui.update_radio_buttons("navigation", selected="nav_model_control")

    def _execute_build_process(compiler_path, build_type, exe_name,
                               clean_first, action_name):
        """Shared build/rebuild subprocess logic run inside a background thread.

        Parameters
        ----------
        compiler_path : str
            Full path to the Fortran compiler.
        build_type : str
            Build configuration (e.g. "release", "debug").
        exe_name : str
            Target executable name.
        clean_first : bool
            Whether to run ``make clean-lib`` before building.
        action_name : str
            Display label used in log messages ("Build" or "Rebuild").
        """
        start_time = time.time()

        try:
            logger.info(f"{action_name} thread started for {exe_name}")

            if clean_first:
                clean_msg = (
                    "Cleaning all build artifacts"
                    if action_name == "Rebuild"
                    else "Cleaning previous build"
                )
                _build_log_lines.append(f"\n=== {clean_msg} ===\n")
                p = subprocess.Popen(
                    ["make", "clean-lib"],
                    cwd=ROOT,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True,
                )
                for line in p.stdout:
                    _build_log_lines.append(line)
                p.wait()

            build_verb = "Rebuilding" if action_name == "Rebuild" else "Building"
            _build_log_lines.append(
                f"\n=== {build_verb} library and executable ===\n"
            )

            cmd = [
                "make",
                f"FC={compiler_path}",
                f"BUILD_TYPE={build_type}",
                "build-named",
            ]
            logger.info(f"Running: {' '.join(cmd)}")
            p = subprocess.Popen(
                cmd,
                cwd=ROOT,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
            )

            for line in p.stdout:
                _build_log_lines.append(line)
                # Trim if too long
                if len(_build_log_lines) > 500:
                    del _build_log_lines[:100]

            p.wait()
            elapsed = time.time() - start_time

            _build_log_lines.append("-" * 50 + "\n")
            if p.returncode == 0:
                _build_log_lines.append(
                    f"\u2713 {action_name} completed successfully!\n"
                )
                _build_log_lines.append(f"  Executable: {exe_name}\n")
                _build_log_lines.append(f"  Time: {elapsed:.1f}s\n")
            else:
                _build_log_lines.append(
                    f"\u2717 {action_name} failed with return code "
                    f"{p.returncode}\n"
                )
            _build_log_lines.append("=" * 50 + "\n")

            logger.info(f"{action_name} thread completed for {exe_name}")

        except Exception as e:
            logger.error(
                f"{action_name} thread error: {e}\n{traceback.format_exc()}"
            )
            _build_log_lines.append(f"\nError: {e}\n")
            _build_log_lines.append(traceback.format_exc())

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
            _build_log_lines.clear()
            _build_log_lines.extend([
                "=" * 50 + "\n",
                f"ERROR: Compiler '{compiler}' not found!\n",
                "=" * 50 + "\n",
                "Please ensure the compiler is installed and accessible.\n",
                "For Intel compilers, check /opt/intel/oneapi/compiler/\n",
            ])
            return

        _build_log_lines.clear()
        _build_log_lines.extend([
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
            _execute_build_process(
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
            _build_log_lines.clear()
            _build_log_lines.extend([
                "=" * 50 + "\n",
                f"ERROR: Compiler '{compiler}' not found!\n",
                "=" * 50 + "\n",
                "Please ensure the compiler is installed and accessible.\n",
                "For Intel compilers, check /opt/intel/oneapi/compiler/\n",
            ])
            return

        _build_log_lines.clear()
        _build_log_lines.extend([
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
            _execute_build_process(
                compiler_path=_compiler_path,
                build_type=_build_type,
                exe_name=_exe_name,
                clean_first=True,
                action_name="Rebuild",
            )

        threading.Thread(target=_do_rebuild, daemon=True, name="RebuildThread").start()

    # =========================================================================
    # End Model Build Panel - Server Logic
    # =========================================================================

    @render.ui
    def constants_validation_status():
        """Display real-time validation status of the selected constants file"""
        const_file = input.cmd_constants_file() or ""
        
        # Check if binary/shear file is set but not constants - use default
        try:
            binary_enabled = input.cmd_binary_enabled()
            shear_file = input.cmd_shear_stress_file()
            if (binary_enabled or shear_file) and not const_file:
                const_file = DEFAULT_CONSTANTS_FILE
        except Exception:
            pass
        
        if not const_file:
            return ui.div(
                ui.tags.small("ℹ️ No constants file selected (using model defaults)", 
                             class_="text-muted")
            )
        
        is_valid, actual_count, error_msg = validate_constants_file(const_file)
        
        if is_valid:
            return ui.div(
                ui.tags.small(
                    f"✓ {const_file}: {actual_count}/{REQUIRED_MODEL_CONSTANTS} constants", 
                    class_="text-success"
                )
            )
        else:
            return ui.div(
                ui.tags.small(f"❌ {error_msg}", class_="text-danger"),
                ui.tags.br(),
                ui.tags.small("💡 Recommended: Use WCONST_04.txt", class_="text-warning")
            )

    # Quick action handlers (defined early to access _log_lines)
    @reactive.effect
    @reactive.event(input.quick_run)
    def handle_quick_run():
        """Quick run action from dashboard"""
        logger.info("User clicked Quick Run from dashboard")
        _log_lines.clear()
        _log_lines.append("Starting quick run...\n")

        # Validate required input files first
        _log_lines.append("Validating input files...\n")
        is_valid, errors, warnings = validate_required_inputs()

        if warnings:
            for w in warnings:
                _log_lines.append(f"⚠ {w}\n")

        if not is_valid:
            _log_lines.append("❌ INPUT VALIDATION FAILED:\n")
            for e in errors:
                _log_lines.append(f"  • {e}\n")
            _log_lines.append("\nModel run aborted. Please ensure all required input files exist.\n")
            logger.error(f"Input validation failed: {errors}")
            return

        _log_lines.append("✓ Input files validated\n")

        try:
            # Capture current widget values (must be done in reactive context)
            estas_cmd = build_estas_command()

            # Check if executable exists
            try:
                exe_name = input.run_executable() or "ESTAS_II"
            except Exception:
                exe_name = "ESTAS_II"

            exe_path = os.path.join(ROOT, exe_name)
            if not os.path.exists(exe_path):
                _log_lines.append(f"❌ ERROR: Executable '{exe_name}' not found.\n")
                _log_lines.append("Please go to Model Build to compile the model first.\n")
                return

            # Check Intel library requirements for Intel-compiled executables
            if is_intel_executable(exe_name):
                setvars_path = get_intel_setvars_path()
                if setvars_path:
                    _log_lines.append(f"ℹ️  Intel executable detected. Will source Intel environment.\n")
                else:
                    intel_available, intel_path = check_intel_libs_available()
                    if intel_available:
                        _log_lines.append(f"ℹ️  Intel executable detected. Using runtime libs from:\n")
                        _log_lines.append(f"   {intel_path}\n")
                    else:
                        _log_lines.append("⚠️  WARNING: Intel-compiled executable selected but Intel runtime\n")
                        _log_lines.append("   libraries (libimf.so) and setvars.sh not found.\n")
                        _log_lines.append("   The model may fail to start. Consider:\n")
                        _log_lines.append("   • Installing Intel oneAPI or using a gfortran executable\n")
                        _log_lines.append("-" * 50 + "\n")

            # Validate constants file before running
            try:
                const_file = input.cmd_constants_file() or ""
            except Exception:
                const_file = ""

            try:
                binary_enabled = input.cmd_binary_enabled()
            except Exception:
                binary_enabled = False

            try:
                shear_file = input.cmd_shear_stress_file() or ""
            except Exception:
                shear_file = ""

            if not const_file and (binary_enabled or shear_file):
                const_file = DEFAULT_CONSTANTS_FILE

            if const_file:
                is_valid, actual_count, error_msg = validate_constants_file(const_file)
                if not is_valid:
                    _log_lines.append(f"❌ VALIDATION ERROR:\n{error_msg}\n")
                    _log_lines.append("Model run aborted. Please select a constants file with all required parameters.\n")
                    logger.error(f"Constants file validation failed: {error_msg}")
                    return
                else:
                    _log_lines.append(f"✓ Constants file validated: {const_file} ({actual_count} constants)\n")

            # Show command before starting
            cmd_display = " ".join([c if c else '""' for c in estas_cmd])
            _log_lines.append(f"\nCommand: {cmd_display}\n")
            _log_lines.append("-" * 40 + "\n")

        except Exception as e:
            _log_lines.append(f"\n❌ Error preparing quick run: {e}\n")
            _log_lines.append(f"Traceback:\n{traceback.format_exc()}\n")
            logger.error(f"Error in quick_run setup: {e}\n{traceback.format_exc()}")
            return

        def _work():
            start_time = time.time()
            logger.info("Quick Run thread started")
            _log_lines.append("Starting model execution...\n")

            # Filter out empty strings for actual execution
            exec_cmd = [c for c in estas_cmd if c]

            def format_time(seconds):
                """Format seconds into HH:MM:SS or MM:SS"""
                hours = int(seconds // 3600)
                minutes = int((seconds % 3600) // 60)
                secs = int(seconds % 60)
                if hours > 0:
                    return f"{hours:02d}:{minutes:02d}:{secs:02d}"
                else:
                    return f"{minutes:02d}:{secs:02d}"

            def get_csv_info():
                """Get info about OUTPUT.csv file for progress tracking"""
                try:
                    if os.path.exists(OUTPUT_CSV):
                        stat = os.stat(OUTPUT_CSV)
                        size_kb = stat.st_size / 1024
                        with open(OUTPUT_CSV, 'rb') as f:
                            lines = sum(1 for _ in f)
                        return {"exists": True, "size_kb": size_kb, "lines": lines}
                except Exception:
                    pass
                return {"exists": False, "size_kb": 0, "lines": 0}

            try:
                # For Intel executables, wrap command to source Intel environment
                use_shell = False
                final_cmd = exec_cmd
                run_env = os.environ.copy()  # Start with current environment
                
                if is_intel_executable(exe_name):
                    setvars_path = get_intel_setvars_path()
                    if setvars_path:
                        # Use shell command that sources Intel environment first
                        final_cmd, use_shell = build_intel_wrapped_command(exec_cmd)
                        _log_lines.append(f"ℹ️  Sourcing Intel environment: {setvars_path}\n")
                        logger.info(f"Using Intel wrapper with setvars: {setvars_path}")
                    else:
                        # Fall back to LD_LIBRARY_PATH approach
                        run_env = get_run_environment()
                        ld_path = run_env.get("LD_LIBRARY_PATH", "NOT SET")
                        _log_lines.append(f"LD_LIBRARY_PATH: {ld_path[:200]}...\n")
                        logger.info(f"Starting process with LD_LIBRARY_PATH: {ld_path[:100]}...")
                else:
                    # For non-Intel executables, use standard environment
                    run_env = get_run_environment()
                
                logger.info(f"Executing: {final_cmd if isinstance(final_cmd, str) else ' '.join(final_cmd)}")
                p = subprocess.Popen(
                    final_cmd,
                    cwd=ROOT,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True,
                    bufsize=1,
                    env=run_env,
                    shell=use_shell,
                    executable="/bin/bash" if use_shell else None
                )
                _model_process[0] = p
                _model_running[0] = True
                _last_run_time[0] = datetime.now()
                _model_progress[0] = ({"elapsed": "00:00", "rows": 0, "size_kb": 0, "status": "running"})

                last_progress_update = time.time()

                # Read output with progress updates
                while p.poll() is None:
                    if p.stdout:
                        try:
                            readable, _, _ = select.select([p.stdout], [], [], 0.5)
                            if readable:
                                line = p.stdout.readline()
                                if line:
                                    _log_lines.append(line)
                                    while len(_log_lines) > 1000:
                                        _log_lines.pop(0)
                        except Exception:
                            time.sleep(0.5)

                    # Update progress every second
                    now = time.time()
                    if now - last_progress_update >= 1.0:
                        elapsed = now - start_time
                        output_info = get_csv_info()
                        _model_progress[0] = ({
                            "elapsed": format_time(elapsed),
                            "rows": output_info.get("lines", 0),
                            "size_kb": output_info.get("size_kb", 0),
                            "status": "running"
                        })
                        last_progress_update = now

                # Read any remaining output
                if p.stdout:
                    remaining = p.stdout.read()
                    if remaining:
                        _log_lines.append(remaining)

                p.wait()
                rc = p.returncode

                elapsed = time.time() - start_time
                output_info = get_csv_info()
                _log_lines.append("-" * 40 + "\n")
                if rc == 0:
                    _log_lines.append(f"✓ Model run completed successfully!\n")
                    _model_progress[0] = ({
                        "elapsed": format_time(elapsed),
                        "rows": output_info.get("lines", 0),
                        "size_kb": output_info.get("size_kb", 0),
                        "status": "completed"
                    })
                else:
                    _log_lines.append(f"✗ Model run failed with return code {rc}\n")
                    _model_progress[0] = ({
                        "elapsed": format_time(elapsed),
                        "rows": output_info.get("lines", 0),
                        "size_kb": output_info.get("size_kb", 0),
                        "status": "failed"
                    })
                _log_lines.append(f"Total time: {format_time(elapsed)}\n")
                logger.info(f"Quick Run finished: rc={rc}, elapsed={elapsed:.1f}s")

            except Exception as e:
                _log_lines.append(f"\n❌ Error running model: {e}\n")
                logger.error(f"Quick Run error: {e}")
                _model_progress[0] = ({"elapsed": "", "rows": 0, "size_kb": 0, "status": "error"})
            finally:
                _model_process[0] = None
                _model_running[0] = False

        threading.Thread(target=_work, daemon=True, name="QuickRunThread").start()

    @render.ui
    def help_content():
        """Render the state variables help content from markdown file"""
        help_file = os.path.join(os.path.dirname(__file__), "STATE_VARIABLES_HELP.md")
        try:
            if not os.path.exists(help_file):
                return ui.div(
                    ui.tags.p("Help file not found.", class_="text-danger"),
                    ui.tags.p(f"Expected: {help_file}", class_="text-muted small")
                )
            
            with open(help_file, 'r', encoding='utf-8') as f:
                md_content = f.read()
            
            # Add custom CSS for better table and navigation styling
            table_css = """
            <style>
                .offcanvas-body table {
                    width: 100%;
                    border-collapse: collapse;
                    margin: 1rem 0;
                    font-size: 0.85rem;
                }
                .offcanvas-body th, .offcanvas-body td {
                    border: 1px solid #dee2e6;
                    padding: 0.5rem;
                    text-align: left;
                }
                .offcanvas-body th {
                    background-color: #f8f9fa;
                    font-weight: 600;
                }
                .offcanvas-body tr:nth-child(even) {
                    background-color: #f8f9fa;
                }
                .offcanvas-body code {
                    background-color: #e9ecef;
                    padding: 0.1rem 0.3rem;
                    border-radius: 0.2rem;
                    font-size: 0.85em;
                }
                .offcanvas-body h1 {
                    color: #212529;
                    font-size: 1.5rem;
                    margin-top: 2rem;
                    margin-bottom: 1rem;
                    padding-bottom: 0.5rem;
                    border-bottom: 3px solid #0d6efd;
                }
                .offcanvas-body h1:first-of-type {
                    margin-top: 0;
                }
                .offcanvas-body h2 {
                    color: #0d6efd;
                    border-bottom: 2px solid #0d6efd;
                    padding-bottom: 0.5rem;
                    margin-top: 1.5rem;
                    font-size: 1.2rem;
                }
                .offcanvas-body h3 {
                    color: #495057;
                    margin-top: 1.2rem;
                    font-size: 1rem;
                }
                .offcanvas-body hr {
                    margin: 1.5rem 0;
                    border-color: #dee2e6;
                }
                .offcanvas-body pre {
                    background-color: #f8f9fa;
                    padding: 1rem;
                    border-radius: 0.25rem;
                    overflow-x: auto;
                    white-space: pre-wrap;
                    font-size: 0.8rem;
                }
                /* Navigation styling */
                .offcanvas-body .help-nav a {
                    transition: transform 0.2s, box-shadow 0.2s;
                }
                .offcanvas-body .help-nav a:hover {
                    transform: translateY(-2px);
                    box-shadow: 0 4px 8px rgba(0,0,0,0.2);
                    text-decoration: none;
                }
                /* Formula box styling */
                .offcanvas-body .formula-box {
                    font-size: 1rem;
                }
                .offcanvas-body .formula-box strong {
                    font-size: 1.1rem;
                }
                /* Anchor link styling */
                .offcanvas-body a {
                    color: #0d6efd;
                    text-decoration: none;
                }
                .offcanvas-body a:hover {
                    text-decoration: underline;
                }
                /* Details/Summary styling */
                .offcanvas-body details {
                    background: #f8f9fa;
                    border-radius: 5px;
                    padding: 0.5rem 1rem;
                    margin: 0.5rem 0;
                }
                .offcanvas-body summary {
                    cursor: pointer;
                    user-select: none;
                }
                .offcanvas-body summary::-webkit-details-marker {
                    color: #0d6efd;
                }
                /* Subscript/superscript in formulas */
                .offcanvas-body sub, .offcanvas-body sup {
                    font-size: 0.75em;
                }
                /* Smooth scrolling for anchor links */
                .offcanvas-body {
                    scroll-behavior: smooth;
                }
            </style>
            """
            
            if MARKDOWN_AVAILABLE:
                # Convert markdown to HTML with table extension
                html_content = markdown.markdown(
                    md_content,
                    extensions=['tables', 'fenced_code', 'toc']
                )
                return ui.HTML(table_css + html_content)
            else:
                # Fallback: display raw markdown in a pre block
                return ui.div(
                    ui.tags.div(
                        ui.tags.i(class_="bi bi-exclamation-triangle me-2"),
                        "Markdown library not installed. Showing raw content.",
                        class_="alert alert-warning"
                    ),
                    ui.HTML(table_css),
                    ui.tags.pre(md_content, style="white-space: pre-wrap; font-size: 0.85rem;")
                )
        except Exception as e:
            logger.error(f"Error loading help content: {e}")
            return ui.div(
                ui.tags.p("Error loading help content.", class_="text-danger"),
                ui.tags.pre(str(e), class_="text-muted small")
            )

    @render.ui
    def changelog_content():
        """Render the changelog from CHANGELOG.md file"""
        changelog_file = os.path.join(ROOT, "CHANGELOG.md")
        try:
            if not os.path.exists(changelog_file):
                return ui.div(
                    ui.tags.p("Changelog file not found.", class_="text-warning"),
                    ui.tags.p(f"Expected: {changelog_file}", class_="text-muted small")
                )
            
            with open(changelog_file, 'r', encoding='utf-8') as f:
                md_content = f.read()
            
            # Custom CSS for changelog styling
            changelog_css = """
            <style>
                .offcanvas-body h1 {
                    color: #17a2b8;
                    font-size: 1.5rem;
                    margin-bottom: 1rem;
                    padding-bottom: 0.5rem;
                    border-bottom: 2px solid #17a2b8;
                }
                .offcanvas-body h2 {
                    color: #495057;
                    font-size: 1.2rem;
                    margin-top: 1.5rem;
                    margin-bottom: 0.75rem;
                    padding: 0.5rem;
                    background: #f8f9fa;
                    border-left: 4px solid #17a2b8;
                }
                .offcanvas-body h3 {
                    color: #28a745;
                    font-size: 1rem;
                    margin-top: 1rem;
                    margin-bottom: 0.5rem;
                }
                .offcanvas-body h3:contains('Added') { color: #28a745; }
                .offcanvas-body h3:contains('Changed') { color: #ffc107; }
                .offcanvas-body h3:contains('Fixed') { color: #dc3545; }
                .offcanvas-body h3:contains('Removed') { color: #6c757d; }
                .offcanvas-body ul {
                    margin: 0.5rem 0;
                    padding-left: 1.5rem;
                }
                .offcanvas-body li {
                    margin-bottom: 0.25rem;
                    line-height: 1.4;
                }
                .offcanvas-body hr {
                    margin: 1rem 0;
                    border-color: #dee2e6;
                }
                .offcanvas-body code {
                    background-color: #e9ecef;
                    padding: 0.1rem 0.3rem;
                    border-radius: 0.2rem;
                    font-size: 0.85em;
                }
                .offcanvas-body a {
                    color: #17a2b8;
                }
            </style>
            """
            
            if MARKDOWN_AVAILABLE:
                html_content = markdown.markdown(
                    md_content,
                    extensions=['tables', 'fenced_code']
                )
                return ui.HTML(changelog_css + html_content)
            else:
                return ui.div(
                    ui.tags.div(
                        ui.tags.i(class_="bi bi-exclamation-triangle me-2"),
                        "Markdown library not installed. Showing raw content.",
                        class_="alert alert-warning"
                    ),
                    ui.HTML(changelog_css),
                    ui.tags.pre(md_content, style="white-space: pre-wrap; font-size: 0.85rem;")
                )
        except Exception as e:
            logger.error(f"Error loading changelog: {e}")
            return ui.div(
                ui.tags.p("Error loading changelog.", class_="text-danger"),
                ui.tags.pre(str(e), class_="text-muted small")
            )

    # Reactive value to track file list refresh
    file_list_version = reactive.Value(0)

    # populate file list on start and when refresh button is clicked or category changes
    @reactive.Effect
    def _():
        # Depend on file_list_version and category filter to trigger refresh
        file_list_version.get()
        category_filter = input.file_category_filter()

        try:
            all_files = sorted([f for f in os.listdir(INPUTS_DIR) if os.path.isfile(os.path.join(INPUTS_DIR, f))])

            # Apply category filter
            if category_filter and category_filter != "All Categories":
                filtered_files = []
                for f in all_files:
                    if f in INPUT_FILE_CATEGORIES:
                        if INPUT_FILE_CATEGORIES[f].get("category") == category_filter:
                            filtered_files.append(f)
                    else:
                        # For uncatalogued files, use pattern matching
                        info = analyze_input_file(os.path.join(INPUTS_DIR, f))
                        if info.get("category") == category_filter:
                            filtered_files.append(f)
                files = filtered_files
            else:
                files = all_files

            logger.info(f"Populating file list with {len(files)} files (category: {category_filter})")
            ui.update_select("file_select", choices=files)
        except Exception as e:
            logger.error(f"Error populating file list: {e}")

    # Refresh file list button
    @reactive.effect
    @reactive.event(input.refresh_files)
    def refresh_files():
        logger.info("User clicked 'Refresh file list' button")
        file_list_version.set(file_list_version.get() + 1)

    # load selected file contents
    @reactive.effect
    @reactive.event(input.file_select)
    def load_file():
        f = input.file_select()
        if not f:
            logger.debug("load_file called but no file selected")
            return
        logger.info(f"Loading file: {f}")

        try:
            path = safe_resolve(INPUTS_DIR, f)
            with open(path, 'r') as fh:
                txt = fh.read()
            logger.info(f"Successfully loaded {f} ({len(txt)} characters)")
            ui.update_text_area("file_contents", value=txt)
        except Exception as e:
            logger.error(f"Error reading file {f}: {e}")
            ui.update_text_area("file_contents", value=f"Error reading file: {e}")

    # Render file header text
    @render.text
    def file_header_text():
        f = input.file_select()
        if not f:
            return "File Contents"
        return f"File Contents: {f}"

    # Render file info panel - directly depends on file_select for reactivity
    @render.ui
    def file_info_panel():
        f = input.file_select()
        if not f:
            return ui.tags.div(
                ui.tags.p("Select a file to view its information", class_="text-muted"),
                class_="p-2"
            )

        # Analyze the file
        try:
            path = safe_resolve(INPUTS_DIR, f)
        except ValueError as e:
            return ui.tags.div(
                ui.tags.p(f"Invalid file: {e}", class_="text-danger"),
                class_="p-2"
            )
        analysis = analyze_input_file(path)

        # Build info rows
        rows = []

        # Category badge
        category = analysis.get("category", "Unknown")
        category_colors = {
            "Forcing Timeseries": "primary",
            "Meteorological Timeseries": "info",
            "Boundary Forcing Timeseries": "success",
            "Sediment Forcing Timeseries": "warning",
            "Settling Velocity Timeseries": "secondary",
            "Box Geometry": "dark",
            "Model Constants": "danger",
            "Model Configuration": "primary",
            "Initial Conditions": "info",
            "Transport Configuration": "success",
            "Sediment Fluxes": "warning",
            "Sediment Parameters": "secondary",
            "Process Parameters": "dark",
            "Boundary Conditions": "success",
        }
        badge_color = category_colors.get(category, "secondary")
        rows.append(
            ui.tags.div(
                ui.tags.span(category, class_=f"badge bg-{badge_color}"),
                class_="mb-2"
            )
        )

        # Description
        desc = analysis.get("description", "")
        if desc:
            rows.append(ui.tags.p(ui.tags.strong("Description: "), desc))

        # Structure
        structure = analysis.get("structure", "")
        if structure:
            rows.append(ui.tags.p(ui.tags.strong("Structure: "), structure))

        # Model use
        model_use = analysis.get("model_use", "")
        if model_use:
            rows.append(ui.tags.p(ui.tags.strong("Model Use: "), model_use))

        # File statistics
        stats = []
        if analysis.get("num_lines"):
            stats.append(f"Lines: {analysis['num_lines']:,}")
        if analysis.get("num_variables"):
            stats.append(f"Variables: {analysis['num_variables']}")
        if analysis.get("data_size"):
            stats.append(f"Data rows: {analysis['data_size']:,}")

        if stats:
            rows.append(ui.tags.p(ui.tags.strong("Statistics: "), " | ".join(stats)))

        # Timespan (for timeseries files)
        if analysis.get("is_timeseries") and analysis.get("time_start") is not None:
            timespan_info = []
            if analysis.get("date_start") and analysis.get("date_end"):
                timespan_info.append(f"Date range: {analysis['date_start']} to {analysis['date_end']}")
            if analysis.get("time_start") is not None and analysis.get("time_end") is not None:
                duration = analysis['time_end'] - analysis['time_start']
                years = duration / 365.25
                timespan_info.append(f"Duration: {duration:.1f} days ({years:.1f} years)")
                timespan_info.append(f"Julian days: {analysis['time_start']:.1f} - {analysis['time_end']:.1f}")

            if timespan_info:
                rows.append(
                    ui.tags.div(
                        ui.tags.strong("Timespan:"),
                        ui.tags.ul(
                            *[ui.tags.li(info) for info in timespan_info],
                            class_="mb-0 ps-3"
                        ),
                        class_="mb-2"
                    )
                )

        # Error if any
        if analysis.get("error"):
            rows.append(
                ui.tags.div(
                    ui.tags.span("Error: ", class_="text-danger"),
                    analysis["error"],
                    class_="text-danger"
                )
            )

        return ui.tags.div(*rows, class_="p-2")

    # Reactive value for save status feedback
    save_status_msg = reactive.Value("")

    # save file
    @reactive.event(input.save_file)
    def save_file():
        f = input.file_select()
        logger.info(f"User clicked 'Save file' button")
        if not f:
            logger.warning("Save attempted but no file selected")
            save_status_msg.set("Error: No file selected")
            return

        logger.info(f"Saving file: {f}")
        try:
            path = safe_resolve(INPUTS_DIR, f)
        except ValueError as e:
            save_status_msg.set(f"Error: {e}")
            return
        content_length = len(input.file_contents())
        logger.debug(f"Content length: {content_length} characters")

        # Create timestamped backup
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        bak = f"{path}.{timestamp}.bak"
        try:
            if os.path.exists(path):
                original_size = os.path.getsize(path)
                shutil.copy(path, bak)
                logger.info(f"Backup created: {os.path.basename(bak)} (original size: {original_size} bytes)")

            with open(path, 'w') as fh:
                fh.write(input.file_contents())

            new_size = os.path.getsize(path)
            save_status_msg.set(f"✓ Saved successfully at {datetime.now().strftime('%H:%M:%S')}")
            logger.info(f"File saved successfully: {f} (new size: {new_size} bytes)")
        except Exception as e:
            error_msg = f"✗ Save failed: {e}"
            save_status_msg.set(error_msg)
            logger.error(f"Error saving file {f}: {e}", exc_info=True)

    @render.text
    def save_status():
        return save_status_msg.get()

    # ========== MAP DISPLAY TAB (Input Files) ==========

    @render_widget
    def map_display_plot():
        """Render the Map Display plotly figure based on selected view."""
        view = input.map_display_view()
        box_no = int(input.map_bathymetry_box())

        boxes = box_network.parse_pelagic_inputs(INPUTS_DIR)
        if view == "Box Network":
            links = box_network.parse_advective_links(INPUTS_DIR)
            fig = box_network.build_box_network_figure(boxes, links)
        elif view == "Bathymetry Profile":
            layers = box_network.parse_bathymetry(box_no, INPUTS_DIR)
            fig = box_network.build_bathymetry_figure(box_no, layers, boxes)
        elif view == "Box Depths Overview":
            fig = box_network.build_depths_overview(boxes)
        else:
            fig = go.Figure()
            fig.update_layout(height=700, template='plotly_dark')
        return go.FigureWidget(fig)

    @render.ui
    def map_display_info():
        """Contextual info for the current map view."""
        view = input.map_display_view()
        if view == "Box Network":
            return ui.tags.div(
                ui.tags.small(
                    ui.tags.strong("Mosaic box model: "),
                    "Touching boxes share an advective link (water exchange). "
                    "Dark gaps separate grid-neighbours with no connection. "
                    "Green dashed lines = non-adjacent links (8 of 42). "
                    "Border colour: blue = Sand, brown = Mud. "
                    "Box numbering is an ID, not geographic order "
                    "(e.g. Box 2 sits at the Nemunas inflow, not near Baltic). "
                    "Hover for details.",
                    class_="text-muted"
                ),
                class_="mt-2"
            )
        elif view == "Bathymetry Profile":
            box_no = int(input.map_bathymetry_box())
            boxes = box_network.parse_pelagic_inputs(INPUTS_DIR)
            info = boxes.get(box_no, {})
            return ui.tags.div(
                ui.tags.small(
                    ui.tags.strong(f"Box {box_no}: "),
                    f"Depth {info.get('depth', 0):.1f} m, "
                    f"{info.get('sediment', 'Unknown')} substrate. "
                    "Horizontal bars show layer area at each elevation.",
                    class_="text-muted"
                ),
                class_="mt-2"
            )
        elif view == "Box Depths Overview":
            return ui.tags.div(
                ui.tags.small(
                    ui.tags.strong("Overview: "),
                    "Bottom elevations for all 25 boxes. "
                    "Blue = Sand substrate, Brown = Mud. "
                    "Diamonds = surface water elevation.",
                    class_="text-muted"
                ),
                class_="mt-2"
            )
        return ui.tags.div()

    # ========== PARAMETER EDITOR ==========
    # Reactive values for parameter editing
    param_file_obj = reactive.Value(None)
    param_modified = reactive.Value({})  # param_id -> new_value
    param_save_msg = reactive.Value("")

    @reactive.effect
    @reactive.event(input.load_params, input.param_category, input.param_file)
    def load_param_file():
        """Load parameter file when category or file changes"""
        param_filename = input.param_file()
        if not param_filename:
            return

        filepath = os.path.join(INPUTS_DIR, param_filename)
        if not os.path.exists(filepath):
            logger.error(f"Parameter file not found: {filepath}")
            return

        logger.info(f"Loading parameter file: {param_filename}")
        pf = ParameterFile(filepath)
        if pf.parse():
            param_file_obj.set(pf)
            param_modified.set({})  # Clear modifications when loading
            param_save_msg.set("")
            logger.info(f"Loaded {len(pf.parameters)} parameters")
        else:
            logger.error("Failed to parse parameter file")

    @render.text
    def param_category_info():
        """Display category information"""
        category = input.param_category()
        pf = param_file_obj.get()

        if not category:
            return "Select a category"

        if category in PARAMETER_CATEGORIES:
            start, end = PARAMETER_CATEGORIES[category]
            count = end - start + 1

            info = f"Category: {category}\n"
            info += f"Parameters: {count} ({start}-{end})\n"

            if pf:
                params = pf.get_parameters_by_category(category)
                info += f"Loaded: {len(params)} parameters"

            return info
        return "Unknown category"

    @render.ui
    def param_table():
        """Render parameter table for editing"""
        category = input.param_category()
        pf = param_file_obj.get()

        if not pf:
            return ui.tags.div(
                ui.tags.p("Click 'Load Parameters' to load the parameter file", class_="text-muted"),
                class_="mt-2"
            )

        params = pf.get_parameters_by_category(category)

        if not params:
            return ui.tags.p(f"No parameters found for category: {category}", class_="text-warning")

        # Create input fields for each parameter
        param_inputs = []
        for p in params:
            param_row = ui.tags.div(
                ui.tags.div(
                    ui.tags.strong(p.name, class_="small"),
                    ui.tags.br(),
                    ui.tags.small(p.comment[:60] + "..." if len(p.comment) > 60 else p.comment, class_="text-muted"),
                    class_="col-7"
                ),
                ui.tags.div(
                    ui.input_numeric(
                        f"param_{p.id}",
                        "",
                        value=p.value,
                        width="100%"
                    ),
                    class_="col-5"
                ),
                class_="row mb-2 align-items-center border-bottom pb-2"
            )
            param_inputs.append(param_row)

        return ui.tags.div(
            ui.tags.div(
                ui.tags.small(f"Showing {len(params)} parameters", class_="text-muted"),
                class_="mb-2"
            ),
            *param_inputs,
            style="max-height: 400px; overflow-y: auto;"
        )

    @reactive.effect
    @reactive.event(input.save_params)
    def save_parameters():
        """Save modified parameters"""
        pf = param_file_obj.get()
        if not pf:
            param_save_msg.set("Error: No parameter file loaded")
            return

        # Collect all modified values
        category = input.param_category()
        params = pf.get_parameters_by_category(category)

        updates = {}
        for p in params:
            input_id = f"param_{p.id}"
            try:
                new_value = input[input_id]()
                if new_value is not None and new_value != p.value:
                    updates[p.id] = float(new_value)
            except Exception as e:
                logger.debug(f"Could not get value for {input_id}: {e}")

        if not updates:
            param_save_msg.set("No changes to save")
            return

        logger.info(f"Saving {len(updates)} parameter changes")

        # Apply updates
        success_count, fail_count, messages = pf.update_parameters(updates)

        # Save to file
        save_ok, save_msg = pf.save(backup=True)

        if save_ok:
            param_save_msg.set(f"Saved {success_count} changes at {datetime.now().strftime('%H:%M:%S')}")
            ui.notification_show(
                f"Successfully saved {success_count} parameter changes",
                type="message",
                duration=3
            )
        else:
            param_save_msg.set(f"Save failed: {save_msg}")
            ui.notification_show(
                f"Failed to save parameters: {save_msg}",
                type="error",
                duration=5
            )

    @render.text
    def param_save_status():
        """Display save status"""
        return param_save_msg.get()

    # ========== END PARAMETER EDITOR ==========

    # ========== INITIAL CONDITIONS EDITOR ==========
    # Reactive values for IC editing
    ic_file_obj = reactive.Value(None)
    ic_save_msg = reactive.Value("")

    @reactive.effect
    @reactive.event(input.load_ics, input.ic_category, input.ic_file)
    def load_ic_file():
        """Load IC file when category or file changes"""
        ic_filename = input.ic_file()
        if not ic_filename:
            return

        filepath = os.path.join(INPUTS_DIR, ic_filename)
        if not os.path.exists(filepath):
            logger.error(f"IC file not found: {filepath}")
            return

        logger.info(f"Loading IC file: {ic_filename}")
        ic = ICFile(filepath)
        if ic.parse():
            ic_file_obj.set(ic)
            ic_save_msg.set("")
            logger.info(f"Loaded {len(ic.conditions)} initial conditions")
        else:
            logger.error("Failed to parse IC file")

    @render.text
    def ic_category_info():
        """Display category information"""
        category = input.ic_category()
        ic = ic_file_obj.get()

        if not category:
            return "Select a category"

        if category in STATE_VARIABLE_CATEGORIES:
            var_ids = STATE_VARIABLE_CATEGORIES[category]
            count = len(var_ids)

            info = f"Category: {category}\n"
            info += f"Variables: {count}\n"

            if ic:
                conditions = ic.get_conditions_by_category(category)
                info += f"Loaded: {len(conditions)} variables"

            return info
        return "Unknown category"

    @render.ui
    def ic_table():
        """Render IC table for editing"""
        category = input.ic_category()
        ic = ic_file_obj.get()

        if not ic:
            return ui.tags.div(
                ui.tags.p("Click 'Load Initial Conditions' to load the IC file", class_="text-muted"),
                class_="mt-2"
            )

        conditions = ic.get_conditions_by_category(category)

        if not conditions:
            return ui.tags.p(f"No variables found for category: {category}", class_="text-warning")

        # Create input fields for each IC
        ic_inputs = []
        for cond in conditions:
            ic_row = ui.tags.div(
                ui.tags.div(
                    ui.tags.strong(cond.name, class_="small"),
                    ui.tags.br(),
                    ui.tags.small(f"{cond.description} ({cond.units})", class_="text-muted"),
                    class_="col-7"
                ),
                ui.tags.div(
                    ui.input_numeric(
                        f"ic_{cond.var_id}",
                        "",
                        value=cond.value,
                        width="100%"
                    ),
                    class_="col-5"
                ),
                class_="row mb-2 align-items-center border-bottom pb-2"
            )
            ic_inputs.append(ic_row)

        return ui.tags.div(
            ui.tags.div(
                ui.tags.small(f"Showing {len(conditions)} state variables", class_="text-muted"),
                class_="mb-2"
            ),
            *ic_inputs,
            style="max-height: 400px; overflow-y: auto;"
        )

    @reactive.effect
    @reactive.event(input.save_ics)
    def save_initial_conditions():
        """Save modified initial conditions"""
        ic = ic_file_obj.get()
        if not ic:
            ic_save_msg.set("Error: No IC file loaded")
            return

        # Collect all modified values
        category = input.ic_category()
        conditions = ic.get_conditions_by_category(category)

        updates = {}
        for cond in conditions:
            input_id = f"ic_{cond.var_id}"
            try:
                new_value = input[input_id]()
                if new_value is not None and new_value != cond.value:
                    updates[cond.var_id] = float(new_value)
            except Exception as e:
                logger.debug(f"Could not get value for {input_id}: {e}")

        if not updates:
            ic_save_msg.set("No changes to save")
            return

        logger.info(f"Saving {len(updates)} IC changes")

        # Apply updates
        success_count, fail_count, messages = ic.update_conditions(updates)

        # Save to file
        save_ok, save_msg = ic.save(backup=True)

        if save_ok:
            ic_save_msg.set(f"Saved {success_count} changes at {datetime.now().strftime('%H:%M:%S')}")
            ui.notification_show(
                f"Successfully saved {success_count} IC changes",
                type="message",
                duration=3
            )
        else:
            ic_save_msg.set(f"Save failed: {save_msg}")
            ui.notification_show(
                f"Failed to save ICs: {save_msg}",
                type="error",
                duration=5
            )

    @render.text
    def ic_save_status():
        """Display IC save status"""
        return ic_save_msg.get()

    # ========== END INITIAL CONDITIONS EDITOR ==========

    # ========== MODEL OPTIONS EDITOR ==========
    # Reactive values for options editing
    options_file_obj = reactive.Value(None)
    extra_const_file_obj = reactive.Value(None)
    options_save_msg = reactive.Value("")

    @reactive.effect
    @reactive.event(input.load_options, input.options_category)
    def load_options_files():
        """Load model options files"""
        # Load PELAGIC_MODEL_OPTIONS.txt
        options_path = os.path.join(INPUTS_DIR, "PELAGIC_MODEL_OPTIONS.txt")
        if os.path.exists(options_path):
            logger.info(f"Loading model options: {options_path}")
            mof = ModelOptionsFile(options_path)
            if mof.parse():
                options_file_obj.set(mof)
                logger.info(f"Loaded {len(mof.options)} model options")

        # Load EXTRA_WCONST.txt
        extra_path = os.path.join(INPUTS_DIR, "EXTRA_WCONST.txt")
        if os.path.exists(extra_path):
            logger.info(f"Loading extra constants: {extra_path}")
            ecf = ExtraConstantsFile(extra_path)
            if ecf.parse():
                extra_const_file_obj.set(ecf)
                logger.info(f"Loaded {len(ecf.constants)} extra constants")

        options_save_msg.set("")

    @render.ui
    def options_switches():
        """Render model option switches"""
        category = input.options_category()
        mof = options_file_obj.get()

        if not mof:
            return ui.tags.div(
                ui.tags.p("Click 'Load Options' to load model options", class_="text-muted"),
                class_="mt-2"
            )

        # Get options for this category
        category_option_names = OPTION_CATEGORIES.get(category, [])

        # Filter to boolean options only for switches
        switch_inputs = []
        for opt_name in category_option_names:
            opt = mof.get_option(opt_name)
            if opt and opt.is_boolean():
                switch_row = ui.tags.div(
                    ui.tags.div(
                        ui.input_switch(
                            f"opt_{opt_name}",
                            opt.description,
                            value=bool(opt.value)
                        ),
                        ui.tags.small(opt.help_text, class_="text-muted d-block"),
                        class_="mb-2"
                    ),
                    class_="border-bottom pb-2 mb-2"
                )
                switch_inputs.append(switch_row)
            elif opt and opt.option_type == "string":
                # String option (like filename)
                string_row = ui.tags.div(
                    ui.tags.div(
                        ui.tags.strong(opt.description, class_="small"),
                        ui.input_text(
                            f"opt_{opt_name}",
                            "",
                            value=str(opt.value),
                            width="100%"
                        ),
                        ui.tags.small(opt.help_text, class_="text-muted"),
                        class_="mb-2"
                    ),
                    class_="border-bottom pb-2 mb-2"
                )
                switch_inputs.append(string_row)

        if not switch_inputs:
            return ui.tags.p("No switches in this category", class_="text-muted")

        return ui.tags.div(
            ui.tags.small(f"Showing {len(switch_inputs)} options", class_="text-muted mb-2 d-block"),
            *switch_inputs
        )

    @render.ui
    def options_constants():
        """Render extra constants for editing"""
        category = input.options_category()
        ecf = extra_const_file_obj.get()

        if not ecf:
            return ui.tags.div()

        # Get constants for this category
        category_option_names = OPTION_CATEGORIES.get(category, [])

        # Filter to numeric constants
        const_inputs = []
        for const_name in category_option_names:
            const = ecf.get_constant(const_name)
            if const and const.option_type == "float":
                const_row = ui.tags.div(
                    ui.tags.div(
                        ui.tags.strong(const.description, class_="small"),
                        ui.tags.br(),
                        ui.tags.small(const.help_text, class_="text-muted"),
                        class_="col-7"
                    ),
                    ui.tags.div(
                        ui.input_numeric(
                            f"const_{const_name}",
                            "",
                            value=const.value,
                            width="100%"
                        ),
                        class_="col-5"
                    ),
                    class_="row mb-2 align-items-center border-bottom pb-2"
                )
                const_inputs.append(const_row)

        if not const_inputs:
            return ui.tags.div()

        return ui.tags.div(
            ui.tags.small(f"Showing {len(const_inputs)} constants", class_="text-muted mb-2 d-block"),
            *const_inputs,
            style="max-height: 300px; overflow-y: auto;"
        )

    @reactive.effect
    @reactive.event(input.save_options)
    def save_model_options():
        """Save modified model options"""
        mof = options_file_obj.get()
        ecf = extra_const_file_obj.get()
        category = input.options_category()

        if not mof and not ecf:
            options_save_msg.set("Error: No options loaded")
            return

        total_updates = 0
        errors = []

        # Save model options switches
        if mof:
            category_option_names = OPTION_CATEGORIES.get(category, [])
            for opt_name in category_option_names:
                opt = mof.get_option(opt_name)
                if opt and opt.is_boolean():
                    input_id = f"opt_{opt_name}"
                    try:
                        new_value = 1 if input[input_id]() else 0
                        if new_value != opt.value:
                            success, msg = mof.update_option(opt_name, new_value)
                            if success:
                                total_updates += 1
                            else:
                                errors.append(msg)
                    except Exception as e:
                        logger.debug(f"Could not get value for {input_id}: {e}")
                elif opt and opt.option_type == "string":
                    input_id = f"opt_{opt_name}"
                    try:
                        new_value = input[input_id]()
                        if new_value and new_value != opt.value:
                            success, msg = mof.update_option(opt_name, new_value)
                            if success:
                                total_updates += 1
                            else:
                                errors.append(msg)
                    except Exception as e:
                        logger.debug(f"Could not get value for {input_id}: {e}")

            # Save options file if there were updates
            if total_updates > 0:
                save_ok, save_msg = mof.save(backup=True)
                if not save_ok:
                    errors.append(save_msg)

        # Save extra constants
        if ecf:
            category_option_names = OPTION_CATEGORIES.get(category, [])
            const_updates = 0
            for const_name in category_option_names:
                const = ecf.get_constant(const_name)
                if const and const.option_type == "float":
                    input_id = f"const_{const_name}"
                    try:
                        new_value = input[input_id]()
                        if new_value is not None and new_value != const.value:
                            success, msg = ecf.update_constant(const_name, new_value)
                            if success:
                                const_updates += 1
                                total_updates += 1
                            else:
                                errors.append(msg)
                    except Exception as e:
                        logger.debug(f"Could not get value for {input_id}: {e}")

            # Save constants file if there were updates
            if const_updates > 0:
                save_ok, save_msg = ecf.save(backup=True)
                if not save_ok:
                    errors.append(save_msg)

        # Report results
        if errors:
            options_save_msg.set(f"Errors: {'; '.join(errors)}")
            ui.notification_show(
                f"Some errors occurred: {'; '.join(errors)}",
                type="error",
                duration=5
            )
        elif total_updates == 0:
            options_save_msg.set("No changes to save")
        else:
            options_save_msg.set(f"Saved {total_updates} changes at {datetime.now().strftime('%H:%M:%S')}")
            ui.notification_show(
                f"Successfully saved {total_updates} option changes",
                type="message",
                duration=3
            )

    @render.text
    def options_save_status():
        """Display options save status"""
        return options_save_msg.get()

    # ========== END MODEL OPTIONS EDITOR ==========

    # ========== SIMULATION CONFIGURATION ==========
    # Reactive values for simulation config
    sim_config_obj = reactive.Value(None)
    sim_config_save_msg = reactive.Value("")

    # Path to INPUT.txt (in ROOT, not INPUTS)
    INPUT_TXT_PATH = os.path.join(ROOT, "INPUT.txt")

    @reactive.effect
    @reactive.event(input.load_sim_config)
    def load_simulation_config_file():
        """Load INPUT.txt simulation configuration"""
        if not os.path.exists(INPUT_TXT_PATH):
            logger.error(f"INPUT.txt not found: {INPUT_TXT_PATH}")
            ui.notification_show("INPUT.txt not found", type="error")
            return

        logger.info(f"Loading simulation config: {INPUT_TXT_PATH}")
        scf = SimulationConfigFile(INPUT_TXT_PATH)
        if scf.parse():
            sim_config_obj.set(scf)
            sim_config_save_msg.set("")

            # Update UI with loaded values
            cfg = scf.config

            # Base year
            ui.update_numeric("sim_base_year", value=cfg.base_year)

            # Calculate dates from days
            try:
                start_date = cfg.get_start_date()
                end_date = cfg.get_end_date()
                ui.update_date("sim_start_date", value=start_date)
                ui.update_date("sim_end_date", value=end_date)
            except Exception as e:
                logger.warning(f"Could not calculate dates: {e}")

            # Time stepping
            ui.update_numeric("sim_timesteps_per_day", value=cfg.time_steps_per_day)

            # Find matching preset for time steps
            for preset_name, preset_value in TIME_STEP_PRESETS.items():
                if preset_value == cfg.time_steps_per_day:
                    ui.update_select("sim_timestep_preset", selected=preset_name)
                    break

            # Output interval
            ui.update_numeric("sim_print_interval", value=cfg.print_interval)

            # Find matching preset for output interval
            output_hours = cfg.get_output_interval_hours()
            for preset_name, preset_value in OUTPUT_INTERVAL_PRESETS.items():
                if abs(preset_value - output_hours) < 0.1:
                    ui.update_select("sim_output_preset", selected=preset_name)
                    break

            # Model options
            ui.update_switch("sim_model_sediments", value=bool(cfg.model_sediments))
            ui.update_select("sim_resuspension", selected=str(cfg.resuspension_option))

            # Output directory
            if cfg.output_folder:
                # Remove trailing slash for matching
                output_folder = cfg.output_folder.rstrip('/')
                ui.update_select("sim_output_dir", selected=output_folder)

            logger.info(f"Loaded simulation config: {cfg.base_year}, "
                       f"days {cfg.simulation_start}-{cfg.simulation_end}")
            ui.notification_show("Configuration loaded", type="message", duration=2)
        else:
            logger.error("Failed to parse INPUT.txt")
            ui.notification_show("Failed to parse INPUT.txt", type="error")

    @reactive.effect
    @reactive.event(input.sim_timestep_preset)
    def update_timesteps_from_preset():
        """Update time steps when preset is selected"""
        preset = input.sim_timestep_preset()
        if preset in TIME_STEP_PRESETS:
            ui.update_numeric("sim_timesteps_per_day", value=TIME_STEP_PRESETS[preset])

    @reactive.effect
    @reactive.event(input.sim_output_preset)
    def update_output_from_preset():
        """Update print interval when output preset is selected"""
        preset = input.sim_output_preset()
        steps_per_day = input.sim_timesteps_per_day()

        if preset in OUTPUT_INTERVAL_PRESETS and steps_per_day:
            hours = OUTPUT_INTERVAL_PRESETS[preset]
            steps_per_hour = steps_per_day / 24.0
            print_interval = int(round(hours * steps_per_hour))
            ui.update_numeric("sim_print_interval", value=print_interval)

    @render.text
    def sim_duration_info():
        """Display simulation duration info"""
        try:
            start = input.sim_start_date()
            end = input.sim_end_date()
            base_year = input.sim_base_year()

            if start and end and base_year:
                # Convert to date objects if needed
                if isinstance(start, str):
                    start = datetime.strptime(start, "%Y-%m-%d").date()
                if isinstance(end, str):
                    end = datetime.strptime(end, "%Y-%m-%d").date()

                duration = (end - start).days
                base_date = date(base_year, 1, 1)
                start_days = (start - base_date).days
                end_days = (end - base_date).days

                return (f"Duration: {duration} days\n"
                        f"Days from base: {start_days:.0f} - {end_days:.0f}")
        except Exception as e:
            logger.debug(f"Error calculating duration: {e}")
        return "Load configuration to see details"

    @render.text
    def sim_timestep_info():
        """Display time step info"""
        try:
            steps = input.sim_timesteps_per_day()
            if steps and steps > 0:
                dt_seconds = 86400 / steps
                dt_minutes = dt_seconds / 60
                return f"dt = {dt_minutes:.1f} min ({dt_seconds:.0f} sec)"
        except Exception as e:
            logger.debug(f"Error calculating timestep: {e}")
        return ""

    @render.text
    def sim_output_info():
        """Display output interval info"""
        try:
            steps = input.sim_timesteps_per_day()
            interval = input.sim_print_interval()
            if steps and interval and steps > 0:
                hours = interval * 24.0 / steps
                if hours >= 24:
                    return f"Output every {hours/24:.1f} days"
                else:
                    return f"Output every {hours:.1f} hours"
        except Exception as e:
            logger.debug(f"Error calculating output info: {e}")
        return ""

    @reactive.effect
    @reactive.event(input.save_sim_config)
    def save_simulation_config():
        """Save simulation configuration to INPUT.txt"""
        scf = sim_config_obj.get()

        # If not loaded yet, create new from file
        if scf is None:
            if os.path.exists(INPUT_TXT_PATH):
                scf = SimulationConfigFile(INPUT_TXT_PATH)
                if not scf.parse():
                    sim_config_save_msg.set("Error: Could not parse INPUT.txt")
                    return
            else:
                sim_config_save_msg.set("Error: INPUT.txt not found")
                return

        try:
            cfg = scf.config

            # Update base year
            base_year = input.sim_base_year()
            if base_year:
                cfg.base_year = int(base_year)

            # Update dates -> convert to days
            start_date = input.sim_start_date()
            end_date = input.sim_end_date()

            if start_date:
                if isinstance(start_date, str):
                    start_date = datetime.strptime(start_date, "%Y-%m-%d").date()
                cfg.set_start_date(start_date)

            if end_date:
                if isinstance(end_date, str):
                    end_date = datetime.strptime(end_date, "%Y-%m-%d").date()
                cfg.set_end_date(end_date)

            # Time stepping
            steps = input.sim_timesteps_per_day()
            if steps:
                cfg.time_steps_per_day = int(steps)

            # Print interval
            interval = input.sim_print_interval()
            if interval:
                cfg.print_interval = int(interval)

            # Model options
            cfg.model_sediments = 1 if input.sim_model_sediments() else 0
            resuspension = input.sim_resuspension()
            if resuspension:
                cfg.resuspension_option = int(resuspension)

            # Output directory
            output_dir = input.sim_output_dir()
            if output_dir:
                # Ensure trailing slash for folder path
                cfg.output_folder = output_dir if output_dir.endswith('/') else output_dir + '/'

            # Validate
            is_valid, errors = cfg.validate()
            if not is_valid:
                sim_config_save_msg.set(f"Validation errors: {'; '.join(errors)}")
                ui.notification_show(f"Validation errors: {'; '.join(errors)}", type="error")
                return

            # Save
            save_ok, save_msg = scf.save(backup=True)

            if save_ok:
                sim_config_save_msg.set(f"Saved at {datetime.now().strftime('%H:%M:%S')}")
                ui.notification_show("Configuration saved successfully", type="message", duration=3)
                logger.info(f"Saved simulation config: base={cfg.base_year}, "
                           f"start={cfg.simulation_start}, end={cfg.simulation_end}")
            else:
                sim_config_save_msg.set(f"Save failed: {save_msg}")
                ui.notification_show(f"Save failed: {save_msg}", type="error")

        except Exception as e:
            logger.error(f"Error saving simulation config: {e}", exc_info=True)
            sim_config_save_msg.set(f"Error: {e}")
            ui.notification_show(f"Error: {e}", type="error")

    @render.text
    def sim_config_save_status():
        """Display save status"""
        return sim_config_save_msg.get()

    # ========== END SIMULATION CONFIGURATION ==========

    # ========== OUTPUT CONFIGURATION ==========
    output_config_msg = reactive.Value("")
    output_config_version = reactive.Value(0)  # Incremented on save to trigger dashboard refresh
    OUTPUT_INFO_FILE = os.path.join(ROOT, "INPUTS", "PELAGIC_OUTPUT_INFORMATION_FILE.txt")

    @reactive.effect
    @reactive.event(input.load_output_config)
    def load_output_config():
        """Load current output configuration from file"""
        try:
            if not os.path.exists(OUTPUT_INFO_FILE):
                output_config_msg.set("Output config file not found")
                return

            with open(OUTPUT_INFO_FILE, 'r') as f:
                lines = f.readlines()

            selected_boxes = []
            has_state_vars = False
            has_process_rates = False
            has_mass_balance = False

            for line in lines[1:]:  # Skip header
                parts = line.split()
                if len(parts) >= 4:
                    box_num = parts[0]
                    state_var = parts[1] == "1"
                    process_rate = parts[2] == "1"
                    mass_bal = parts[3] == "1"

                    if state_var or process_rate or mass_bal:
                        selected_boxes.append(box_num)
                        if state_var:
                            has_state_vars = True
                        if process_rate:
                            has_process_rates = True
                        if mass_bal:
                            has_mass_balance = True

            # Update UI
            ui.update_checkbox_group("output_boxes", selected=selected_boxes)
            output_types = []
            if has_state_vars:
                output_types.append("state_vars")
            if has_process_rates:
                output_types.append("process_rates")
            if has_mass_balance:
                output_types.append("mass_balance")
            ui.update_checkbox_group("output_types", selected=output_types)

            output_config_msg.set(f"Loaded: {len(selected_boxes)} boxes")
            logger.info(f"Loaded output config: {len(selected_boxes)} boxes selected")

        except Exception as e:
            logger.error(f"Error loading output config: {e}")
            output_config_msg.set(f"Error: {e}")

    @reactive.effect
    @reactive.event(input.save_output_config)
    def save_output_config():
        """Save output configuration to file"""
        try:
            selected_boxes = set(input.output_boxes() or [])
            output_types = set(input.output_types() or [])

            state_vars_enabled = "state_vars" in output_types
            process_rates_enabled = "process_rates" in output_types
            mass_balance_enabled = "mass_balance" in output_types

            # Build new file content
            lines = ["#     PELAGIC BOX NO      PRODUCE_PEL_STATE_VAR_OUTPUTS     PRODUCE_PEL_PROCESS_RATE_OUTPUTS     PRODUCE_PEL_MASS_BALANCE_OUTPUTS\n"]

            for box in range(1, 26):
                box_str = str(box)
                if box_str in selected_boxes:
                    sv = "1" if state_vars_enabled else "0"
                    pr = "1" if process_rates_enabled else "0"
                    mb = "1" if mass_balance_enabled else "0"
                else:
                    sv = pr = mb = "0"

                lines.append(f"{box:20d}{sv:>37s}{pr:>37s}{mb:>37s}\n")

            # Write file
            with open(OUTPUT_INFO_FILE, 'w') as f:
                f.writelines(lines)

            # Increment version to trigger dashboard refresh
            output_config_version.set(output_config_version.get() + 1)

            output_config_msg.set(f"Saved: {len(selected_boxes)} boxes")
            ui.notification_show(f"Output config saved ({len(selected_boxes)} boxes)", type="message")
            logger.info(f"Saved output config: {len(selected_boxes)} boxes")

        except Exception as e:
            logger.error(f"Error saving output config: {e}")
            output_config_msg.set(f"Error: {e}")
            ui.notification_show(f"Error: {e}", type="error")

    @render.text
    def output_config_status():
        """Display output config status"""
        return output_config_msg.get()

    # ========== END OUTPUT CONFIGURATION ==========

    # ========== SCENARIOS ==========
    # Reactive values for scenario management
    scenario_mgr = reactive.Value(None)
    scenario_status_msg = reactive.Value("")

    # Initialize scenario manager on session start
    @reactive.effect
    def init_scenario_manager():
        """Initialize the scenario manager"""
        mgr = load_scenario_manager(INPUTS_DIR)
        scenario_mgr.set(mgr)
        logger.info(f"Scenario manager initialized with {len(mgr.list_scenarios())} scenarios")

    @reactive.effect
    @reactive.event(input.refresh_scenarios)
    def refresh_scenario_list():
        """Refresh the scenario list"""
        mgr = scenario_mgr.get()
        if mgr:
            mgr.refresh()
            scenario_status_msg.set("Scenarios refreshed")
            logger.info("Scenarios refreshed")

    @render.ui
    def scenario_info():
        """Display information about selected scenario"""
        mgr = scenario_mgr.get()
        scenario_name = input.scenario_select()

        if not mgr or not scenario_name:
            return ui.tags.p("Select a scenario to view details", class_="text-muted")

        scenario = mgr.get_scenario(scenario_name)
        if not scenario:
            return ui.tags.p("Scenario not found", class_="text-warning")

        # Build info display
        info_items = [
            ui.tags.p(ui.tags.strong("Description: "), scenario.description or "No description"),
            ui.tags.p(
                ui.tags.strong("Created: "),
                scenario.created[:10] if scenario.created else "Unknown"
            ),
            ui.tags.p(ui.tags.strong("Contents:")),
            ui.tags.ul(
                ui.tags.li(f"{len(scenario.parameters)} parameters") if scenario.parameters else None,
                ui.tags.li(f"{len(scenario.initial_conditions)} initial conditions ({scenario.ic_file})") if scenario.initial_conditions else None,
                ui.tags.li(f"{len(scenario.model_options)} model options") if scenario.model_options else None,
                ui.tags.li(f"{len(scenario.extra_constants)} extra constants") if scenario.extra_constants else None,
            ),
        ]

        if scenario.is_builtin:
            info_items.append(ui.tags.span("Built-in preset (read-only)", class_="badge bg-secondary"))

        return ui.tags.div(*[item for item in info_items if item is not None])

    @reactive.effect
    def update_scenario_choices():
        """Update scenario dropdown choices"""
        mgr = scenario_mgr.get()
        if mgr:
            names = mgr.get_scenario_names()
            if names:
                ui.update_select("scenario_select", choices=names, selected=names[0])
            else:
                ui.update_select("scenario_select", choices=["No scenarios available"], selected=None)

    @reactive.effect
    @reactive.event(input.load_scenario)
    def load_selected_scenario():
        """Load and apply the selected scenario"""
        mgr = scenario_mgr.get()
        scenario_name = input.scenario_select()

        if not mgr or not scenario_name:
            scenario_status_msg.set("No scenario selected")
            return

        scenario = mgr.get_scenario(scenario_name)
        if not scenario:
            scenario_status_msg.set(f"Scenario '{scenario_name}' not found")
            return

        logger.info(f"Applying scenario: {scenario_name}")

        success, message = mgr.apply_scenario(scenario)

        if success:
            scenario_status_msg.set(f"Loaded: {message}")
            ui.notification_show(
                f"Successfully applied scenario '{scenario_name}'",
                type="message",
                duration=4
            )
        else:
            scenario_status_msg.set(f"Error: {message}")
            ui.notification_show(
                f"Failed to apply scenario: {message}",
                type="error",
                duration=5
            )

    @reactive.effect
    @reactive.event(input.save_scenario)
    def save_new_scenario():
        """Save current configuration as a new scenario"""
        mgr = scenario_mgr.get()
        name = input.new_scenario_name()
        description = input.new_scenario_desc()

        if not mgr:
            scenario_status_msg.set("Scenario manager not initialized")
            return

        if not name or not name.strip():
            scenario_status_msg.set("Please enter a scenario name")
            ui.notification_show("Please enter a scenario name", type="warning")
            return

        name = name.strip()

        # Check if scenario already exists and is builtin
        existing = mgr.get_scenario(name)
        if existing and existing.is_builtin:
            scenario_status_msg.set("Cannot overwrite built-in scenarios")
            ui.notification_show("Cannot overwrite built-in scenarios", type="error")
            return

        logger.info(f"Capturing current state as scenario: {name}")

        # Capture current state
        scenario, capture_msg = mgr.capture_current_state(
            name=name,
            description=description,
            include_params=input.scenario_include_params(),
            include_ics=input.scenario_include_ics(),
            include_options=input.scenario_include_options(),
            ic_file=input.save_ic_file()
        )

        if not scenario:
            scenario_status_msg.set(f"Failed to capture: {capture_msg}")
            return

        # Save scenario
        success, save_msg = mgr.save_scenario(scenario)

        if success:
            scenario_status_msg.set(f"Saved scenario '{name}'")
            ui.notification_show(
                f"Successfully saved scenario '{name}'",
                type="message",
                duration=4
            )
            # Refresh the dropdown
            mgr.refresh()
            names = mgr.get_scenario_names()
            ui.update_select("scenario_select", choices=names, selected=name)
            # Clear input fields
            ui.update_text("new_scenario_name", value="")
            ui.update_text_area("new_scenario_desc", value="")
        else:
            scenario_status_msg.set(f"Save failed: {save_msg}")
            ui.notification_show(f"Failed to save: {save_msg}", type="error", duration=5)

    @reactive.effect
    @reactive.event(input.delete_scenario)
    def delete_selected_scenario():
        """Delete the selected scenario"""
        mgr = scenario_mgr.get()
        scenario_name = input.scenario_select()

        if not mgr or not scenario_name:
            scenario_status_msg.set("No scenario selected")
            return

        scenario = mgr.get_scenario(scenario_name)
        if not scenario:
            scenario_status_msg.set(f"Scenario '{scenario_name}' not found")
            return

        if scenario.is_builtin:
            scenario_status_msg.set("Cannot delete built-in scenarios")
            ui.notification_show("Cannot delete built-in scenarios", type="warning")
            return

        success, message = mgr.delete_scenario(scenario_name)

        if success:
            scenario_status_msg.set(f"Deleted scenario '{scenario_name}'")
            ui.notification_show(f"Deleted scenario '{scenario_name}'", type="message", duration=3)
            # Refresh dropdown
            mgr.refresh()
            names = mgr.get_scenario_names()
            ui.update_select("scenario_select", choices=names, selected=names[0] if names else None)
        else:
            scenario_status_msg.set(f"Delete failed: {message}")
            ui.notification_show(f"Failed to delete: {message}", type="error", duration=5)

    @render.text
    def scenario_status():
        """Display scenario status message"""
        return scenario_status_msg.get()

    # ========== END SCENARIOS ==========

    # ========== MASS BALANCE ==========
    # Reactive values for mass balance
    mb_results = reactive.Value(None)
    mb_calculator = reactive.Value(None)

    @reactive.effect
    @reactive.event(input.calc_mass_balance)
    def calculate_mass_balance():
        """Calculate mass balance when button is clicked"""
        if not os.path.exists(OUTPUT_CSV):
            logger.warning("OUTPUT.csv not found for mass balance calculation")
            ui.notification_show(
                "OUTPUT.csv not found. Run the model first.",
                type="warning",
                duration=3
            )
            return

        logger.info("Calculating mass balance...")

        # Load stoichiometry from parameters
        param_file = os.path.join(INPUTS_DIR, "WCONST_04.txt")
        stoich = load_stoichiometry_from_params(param_file)

        # Create calculator and calculate
        calc = MassBalanceCalculator(OUTPUT_CSV, stoich)
        if calc.load_data():
            results = calc.calculate_all()
            mb_calculator.set(calc)
            mb_results.set(results)
            logger.info("Mass balance calculation complete")
            ui.notification_show(
                "Mass balance calculated successfully",
                type="message",
                duration=2
            )
        else:
            logger.error("Failed to load data for mass balance")
            ui.notification_show(
                "Failed to load output data",
                type="error",
                duration=3
            )

    @render.table
    def mass_balance_summary():
        """Render mass balance summary table"""
        results = mb_results.get()
        if results is None:
            return pd.DataFrame({
                "Message": ["Click 'Calculate Mass Balance' to analyze model output"]
            })

        # Create summary table
        data = []
        for element, result in results.items():
            status = "✓" if result.is_conserved() else "⚠"
            data.append({
                "Element": element,
                "Initial": f"{result.initial_total:.4f}",
                "Final": f"{result.final_total:.4f}",
                "Change": f"{result.percent_change:+.2f}%",
                "Status": status
            })

        return pd.DataFrame(data)

    @render.ui
    def mass_balance_details():
        """Render detailed pool breakdown for selected element"""
        results = mb_results.get()
        element = input.mb_element()

        if results is None or element not in results:
            return ui.tags.p("Calculate mass balance to see details", class_="text-muted")

        result = results[element]

        # Create pool breakdown
        pool_rows = []
        for pool_name, pool_series in result.pool_breakdown.items():
            initial = pool_series.iloc[0] if len(pool_series) > 0 else 0
            final = pool_series.iloc[-1] if len(pool_series) > 0 else 0
            change = final - initial

            pool_row = ui.tags.div(
                ui.tags.div(
                    ui.tags.strong(pool_name, class_="small"),
                    class_="col-4"
                ),
                ui.tags.div(
                    ui.tags.small(f"{initial:.4f}", class_="text-muted"),
                    class_="col-3 text-end"
                ),
                ui.tags.div(
                    ui.tags.small(f"{final:.4f}", class_="text-info"),
                    class_="col-3 text-end"
                ),
                ui.tags.div(
                    ui.tags.small(
                        f"{change:+.4f}",
                        class_="text-success" if change >= 0 else "text-danger"
                    ),
                    class_="col-2 text-end"
                ),
                class_="row border-bottom py-1"
            )
            pool_rows.append(pool_row)

        return ui.tags.div(
            ui.tags.div(
                ui.tags.div(ui.tags.strong("Pool", class_="small"), class_="col-4"),
                ui.tags.div(ui.tags.strong("Initial", class_="small"), class_="col-3 text-end"),
                ui.tags.div(ui.tags.strong("Final", class_="small"), class_="col-3 text-end"),
                ui.tags.div(ui.tags.strong("Δ", class_="small"), class_="col-2 text-end"),
                class_="row border-bottom py-1 bg-light"
            ),
            *pool_rows,
            ui.tags.div(
                ui.tags.small(
                    f"Total change: {result.percent_change:+.2f}%",
                    class_="text-warning" if not result.is_conserved() else "text-success"
                ),
                class_="mt-2"
            ),
            style="max-height: 300px; overflow-y: auto;"
        )

    @render.ui
    def mass_balance_plot_ui():
        """Render mass balance time series info"""
        results = mb_results.get()
        calc = mb_calculator.get()
        element = input.mb_element()

        if results is None or calc is None or element not in results:
            return ui.tags.p("Calculate mass balance to see time series", class_="text-muted")

        result = results[element]
        time_col = calc.get_time_column()

        # Show statistics
        return ui.tags.div(
            ui.tags.div(
                ui.tags.div(
                    ui.tags.small("Min", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{result.min_total:.4f}"),
                    class_="col-3 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Max", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{result.max_total:.4f}"),
                    class_="col-3 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Mean", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{result.mean_total:.4f}"),
                    class_="col-3 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Range", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{result.max_total - result.min_total:.4f}"),
                    class_="col-3 text-center"
                ),
                class_="row mb-2"
            ),
            ui.tags.div(
                ui.tags.small(
                    f"Time span: {time_col.iloc[0]:.1f} - {time_col.iloc[-1]:.1f}",
                    class_="text-muted"
                ),
                ui.tags.br(),
                ui.tags.small(
                    f"Data points: {len(result.time_series)}",
                    class_="text-muted"
                ),
            ),
            class_="p-2 border rounded"
        )

    # ========== END MASS BALANCE ==========

    # ========== OBSERVATION COMPARISON ==========
    # Reactive values for observations
    obs_data_obj = reactive.Value(None)
    obs_comparison_obj = reactive.Value(None)
    obs_metrics_results = reactive.Value(None)
    obs_files_list = reactive.Value([])  # List of ObservationFile objects
    obs_loaded_file = reactive.Value(None)  # Currently loaded observation file
    obs_file_preview = reactive.Value(None)  # Preview data for selected file

    @reactive.effect
    @reactive.event(input.obs_scan_dir)
    def scan_observations_dir():
        """Scan OBSERVATIONS directory for observation files"""
        obs_dir = os.path.join(ROOT, "OBSERVATIONS")
        logger.info(f"Scanning observations directory: {obs_dir}")
        
        if not os.path.isdir(obs_dir):
            ui.notification_show(f"OBSERVATIONS directory not found: {obs_dir}", type="warning")
            return
        
        files = scan_observations_directory(obs_dir)
        obs_files_list.set(files)
        
        # Update file selector
        if files:
            choices = {f.filepath: f"{f.filename} ({f.file_type})" for f in files}
            ui.update_select("obs_file_select", choices=choices, 
                            selected=files[0].filepath if files else None)
            ui.notification_show(f"Found {len(files)} observation files", type="message")
        else:
            ui.notification_show("No observation files found", type="warning")

    @reactive.effect
    @reactive.event(input.obs_file_select)
    def preview_selected_file():
        """Preview selected observation file"""
        selected = input.obs_file_select()
        if not selected:
            obs_file_preview.set(None)
            return
        
        # Get preview
        preview = get_file_preview(selected)
        obs_file_preview.set(preview)

    @reactive.effect
    @reactive.event(input.obs_load_file)
    def load_selected_obs_file():
        """Load the selected observation file"""
        selected = input.obs_file_select()
        if not selected or not os.path.exists(selected):
            ui.notification_show("No file selected", type="warning")
            return
        
        logger.info(f"Loading observation file: {selected}")
        
        # Load based on file type
        loaded = load_obs_file(selected)
        
        if loaded:
            obs_loaded_file.set(loaded)
            
            # Update variable selector with available variables
            available = loaded.get_available_variables()
            if available:
                choices = {str(idx): f"{idx}: {name} ({count} pts)" 
                          for idx, name, count in available}
                ui.update_select("obs_variable", choices=choices,
                               selected=str(available[0][0]) if available else None)
            
            ui.notification_show(
                f"Loaded {loaded.file_info.filename}: {len(available)} variables with data",
                type="message"
            )
        else:
            ui.notification_show(f"Could not load file: {os.path.basename(selected)}", type="error")

    @render.ui
    def obs_file_info():
        """Render file information"""
        preview = obs_file_preview.get()
        
        if preview is None:
            return ui.tags.div(
                ui.tags.p("Click 'Scan OBSERVATIONS Directory' to discover files", 
                         class_="text-muted text-center"),
                class_="p-3"
            )
        
        info = preview.get("info", {})
        
        if "error" in info:
            return ui.tags.div(
                ui.tags.p(f"Error: {info['error']}", class_="text-danger"),
            )
        
        # File info summary
        return ui.tags.div(
            ui.tags.div(
                ui.tags.div(
                    ui.tags.small("Records", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{info.get('n_records', 'N/A')}"),
                    class_="col-4 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Variables", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{info.get('n_variables', 'N/A')}"),
                    class_="col-4 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Date Range", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(
                        f"{info.get('date_range', ('N/A', 'N/A'))[0] or 'N/A'} to {info.get('date_range', ('N/A', 'N/A'))[1] or 'N/A'}"
                        if isinstance(info.get('date_range'), tuple) else "N/A"
                    ),
                    class_="col-4 text-center"
                ),
                class_="row mb-3"
            ),
            class_="p-2"
        )

    @render.ui
    def obs_variables_table():
        """Render table of available variables in selected file"""
        preview = obs_file_preview.get()
        
        if preview is None or "info" not in preview:
            return ui.tags.div()
        
        info = preview.get("info", {})
        variables = info.get("variables_with_data", [])
        
        if not variables:
            # Show data preview table instead
            data = preview.get("data", [])
            if data:
                df = pd.DataFrame(data)
                return ui.tags.div(
                    ui.tags.h6("Data Preview:", class_="mb-2"),
                    ui.tags.div(
                        ui.HTML(df.head(5).to_html(classes="table table-sm table-striped", 
                                                   index=False, border=0)),
                        style="overflow-x: auto; font-size: 11px;"
                    )
                )
            return ui.tags.p("No data available", class_="text-muted")
        
        # Create variable summary table
        rows = []
        for idx, name, count in variables[:20]:  # Limit to 20 variables
            rows.append({
                "Index": idx,
                "Variable": name.split(" - ")[0] if " - " in name else name,
                "Description": name.split(" - ")[1].split(" (")[0] if " - " in name else "",
                "N": count
            })
        
        if len(variables) > 20:
            rows.append({
                "Index": "...",
                "Variable": f"(+{len(variables)-20} more)",
                "Description": "",
                "N": ""
            })
        
        df = pd.DataFrame(rows)
        
        return ui.tags.div(
            ui.tags.h6("Available Measurements:", class_="mb-2"),
            ui.tags.div(
                ui.HTML(df.to_html(classes="table table-sm table-striped table-hover", 
                                   index=False, border=0)),
                style="max-height: 300px; overflow-y: auto; font-size: 11px;"
            )
        )

    @reactive.effect
    @reactive.event(input.obs_file)
    def load_observation_file():
        """Load uploaded observation file"""
        file_info = input.obs_file()
        if file_info is None or len(file_info) == 0:
            return

        file_path = file_info[0]["datapath"]
        logger.info(f"Loading observation file: {file_info[0]['name']}")

        obs = ObservationData()
        success, msg = obs.load_csv(file_path)

        if success:
            obs_data_obj.set(obs)
            logger.info(f"Loaded observations: {len(obs.variables)} variables")

            # Update variable selector
            ui.update_select("obs_variable", choices=obs.variables,
                           selected=obs.variables[0] if obs.variables else None)

            # Create comparison
            if os.path.exists(OUTPUT_CSV):
                comparison = ModelObservationComparison(OUTPUT_CSV, obs)
                comparison.load_model_data()
                obs_comparison_obj.set(comparison)

                # Calculate all metrics
                metrics = comparison.calculate_all_metrics()
                obs_metrics_results.set(metrics)
                logger.info(f"Calculated metrics for {len(metrics)} variables")

            ui.notification_show(f"Loaded {len(obs.variables)} observation variables", type="message")
        else:
            logger.error(f"Failed to load observations: {msg}")
            ui.notification_show(f"Error: {msg}", type="error")

    @reactive.effect
    @reactive.event(input.generate_sample_obs)
    def generate_sample_observations():
        """Generate sample observation data for testing"""
        if not os.path.exists(OUTPUT_CSV):
            ui.notification_show("OUTPUT.csv not found. Run the model first.", type="warning")
            return

        logger.info("Generating sample observations...")

        # Create sample observations (10% of data, 10% noise)
        sample_df = create_sample_observations(OUTPUT_CSV, noise_level=0.1, sample_fraction=0.1)

        if len(sample_df) == 0:
            ui.notification_show("Failed to generate sample data", type="error")
            return

        # Load into observation data
        obs = ObservationData()
        success, msg = obs.load_from_dataframe(sample_df)

        if success:
            obs_data_obj.set(obs)

            # Update variable selector
            ui.update_select("obs_variable", choices=obs.variables,
                           selected=obs.variables[0] if obs.variables else None)

            # Create comparison
            comparison = ModelObservationComparison(OUTPUT_CSV, obs)
            comparison.load_model_data()
            obs_comparison_obj.set(comparison)

            # Calculate all metrics
            metrics = comparison.calculate_all_metrics()
            obs_metrics_results.set(metrics)

            ui.notification_show(
                f"Generated {len(sample_df)} sample observations with {len(obs.variables)} variables",
                type="message"
            )
        else:
            ui.notification_show(f"Error: {msg}", type="error")

    @render.table
    def obs_comparison_summary():
        """Render comparison summary table"""
        comparison = obs_comparison_obj.get()

        if comparison is None:
            return pd.DataFrame({
                "Message": ["Upload observations or generate sample data to compare with model"]
            })

        return comparison.get_summary_table()

    @render.ui
    def obs_metrics_detail():
        """Render detailed metrics for selected variable"""
        metrics = obs_metrics_results.get()
        variable = input.obs_variable()

        if metrics is None or variable is None or variable not in metrics:
            return ui.tags.p("Select a variable to see detailed metrics", class_="text-muted")

        m = metrics[variable]

        # Create metrics display
        return ui.tags.div(
            ui.tags.div(
                ui.tags.div(
                    ui.tags.small("N Points", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{m.n_points}"),
                    class_="col-4 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("R²", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(f"{m.r_squared:.3f}"),
                    class_="col-4 text-center"
                ),
                ui.tags.div(
                    ui.tags.small("Rating", class_="text-muted"),
                    ui.tags.br(),
                    ui.tags.strong(
                        m.get_rating(),
                        class_="text-success" if m.get_rating() == "Excellent"
                              else "text-info" if m.get_rating() == "Good"
                              else "text-warning" if m.get_rating() == "Fair"
                              else "text-danger"
                    ),
                    class_="col-4 text-center"
                ),
                class_="row mb-3"
            ),
            ui.tags.table(
                ui.tags.tbody(
                    ui.tags.tr(
                        ui.tags.td("Observed Mean", class_="small"),
                        ui.tags.td(f"{m.obs_mean:.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("Modeled Mean", class_="small"),
                        ui.tags.td(f"{m.model_mean:.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("Bias", class_="small"),
                        ui.tags.td(f"{m.bias:+.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("MAE", class_="small"),
                        ui.tags.td(f"{m.mae:.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("RMSE", class_="small"),
                        ui.tags.td(f"{m.rmse:.4f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("NRMSE (%)", class_="small"),
                        ui.tags.td(f"{m.nrmse:.1f}%", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("Correlation", class_="small"),
                        ui.tags.td(f"{m.correlation:.3f}", class_="text-end")
                    ),
                    ui.tags.tr(
                        ui.tags.td("Skill Score", class_="small"),
                        ui.tags.td(f"{m.skill_score:.3f}", class_="text-end")
                    ),
                ),
                class_="table table-sm table-borderless"
            ),
            class_="p-2 border rounded"
        )

    @render.ui
    def obs_scatter_info():
        """Render scatter plot information"""
        comparison = obs_comparison_obj.get()
        variable = input.obs_variable()

        if comparison is None or variable is None:
            return ui.tags.p("Load observations to see scatter plot data", class_="text-muted")

        # Get comparison data
        comp_data = comparison.get_comparison_data(variable)

        if comp_data is None or len(comp_data) == 0:
            return ui.tags.p("No matching data for this variable", class_="text-warning")

        # Show scatter plot statistics
        obs_vals = comp_data['Observed']
        mod_vals = comp_data['Modeled']

        return ui.tags.div(
            ui.tags.small(f"Data points: {len(comp_data)}", class_="text-muted d-block"),
            ui.tags.small(f"Observed range: {obs_vals.min():.4f} - {obs_vals.max():.4f}", class_="text-muted d-block"),
            ui.tags.small(f"Modeled range: {mod_vals.min():.4f} - {mod_vals.max():.4f}", class_="text-muted d-block"),
            ui.tags.hr(),
            ui.tags.small(
                "Tip: Use Plot & Visualization to compare time series",
                class_="text-info"
            ),
            class_="p-2"
        )

    # ========== END OBSERVATION COMPARISON ==========

    # CSV caching - reactive values to cache CSV data
    csv_cache = reactive.Value(None)
    csv_cache_mtime = reactive.Value(0)
    csv_cache_path = reactive.Value(None)

    def _get_cached_data(max_rows=None, file_path=None, file_format=None):
        """Get cached output data or reload if modified.
        
        Args:
            max_rows: Maximum number of rows to read
            file_path: Path to output file
            file_format: 'csv', 'text' (.out), or 'binary' (.bin). Auto-detect if None.
        """
        try:
            target_path = file_path or OUTPUT_CSV
            
            if not os.path.exists(target_path):
                logger.warning(f"Output file does not exist: {target_path}")
                return None

            # Auto-detect format from extension if not specified
            if file_format is None:
                if target_path.endswith('.bin'):
                    file_format = 'binary'
                elif target_path.endswith('.out'):
                    file_format = 'text'
                else:
                    file_format = 'csv'

            current_mtime = os.path.getmtime(target_path)
            cached_mtime = csv_cache_mtime.get()
            cached_path = csv_cache_path.get()

            # Check if cache is valid (same file, same mtime)
            if cached_path == target_path and cached_mtime == current_mtime and csv_cache.get() is not None:
                logger.debug("Using cached output data")
                return csv_cache.get()
            
            logger.info(f"Loading {file_format} output (max_rows={max_rows}, file={os.path.basename(target_path)})")
            start_time = time.time()

            if file_format == 'binary':
                # Read Fortran binary file
                df = read_pelagic_binary(target_path, max_rows=max_rows)
            elif file_format == 'text':
                # Read PELAGIC_BOX whitespace-separated file
                df = read_pelagic_text(target_path, max_rows=max_rows)
            else:
                # Read CSV file
                df = pd.read_csv(target_path, comment='#', skip_blank_lines=True, nrows=max_rows)
                df.columns = [c.strip() for c in df.columns]

            load_time = time.time() - start_time
            logger.info(f"Loaded {len(df)} rows, {len(df.columns)} columns in {load_time:.2f}s")

            if max_rows is None:  # Only cache full reads
                csv_cache.set(df)
                csv_cache_mtime.set(current_mtime)
                csv_cache_path.set(target_path)
                logger.debug("Output data cached for future use")
            return df
        except Exception as e:
            logger.error(f"Error reading output file: {e}", exc_info=True)
            return None

    # Keep old function for compatibility
    def _get_cached_csv(max_rows=None, file_path=None):
        """Legacy wrapper for _get_cached_data."""
        return _get_cached_data(max_rows=max_rows, file_path=file_path)

    # load available Y variables from output file header
    @reactive.Effect
    @reactive.event(input.plot_output_file, input.output_format, input.output_dir_select)
    def _update_variable_choices():
        """Update variable choices when output file, format, or directory changes"""
        try:
            file_format = input.output_format() if hasattr(input, 'output_format') else None
            selected_file = get_selected_output_file_path()
            
            if not selected_file or not os.path.exists(selected_file):
                ui.update_selectize("left_vars", choices=[], selected=[])
                ui.update_selectize("right_vars", choices=[], selected=[])
                return
            
            cols = output_data.get_output_columns(file_path=selected_file, file_format=file_format)
            
            if cols:
                # TIME is first column, get the rest
                y_cols = cols[1:]
                
                # Create grouped choices with descriptive names
                grouped_choices = get_grouped_variable_choices(y_cols)
                
                # Default select first variable on left, none on right
                first_var = y_cols[0] if y_cols else None
                ui.update_selectize("left_vars", choices=grouped_choices, selected=[first_var] if first_var else [])
                ui.update_selectize("right_vars", choices=grouped_choices, selected=[])
                logger.info(f"Updated variable choices: {len(y_cols)} variables available")
            else:
                ui.update_selectize("left_vars", choices=[], selected=[])
                ui.update_selectize("right_vars", choices=[], selected=[])
        except Exception as e:
            logger.debug(f"Variable choices update deferred: {e}")

    # run commands in background and capture logs
    # Note: _log_lines is defined earlier in server() for quick_run access

    def run_command(cmd, cwd=ROOT, env=None):
        """Run a command and capture output to _log_lines.

        Args:
            cmd: Command list to execute
            cwd: Working directory
            env: Optional environment dict (merged with base env if provided)
        """
        logger.info(f"Running command: {cmd}")
        try:
            # Use base environment with Intel library paths
            run_env = get_run_environment()
            if env:
                run_env.update(env)

            # Use separate process group or just standard Popen
            p = subprocess.Popen(cmd, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               text=True, bufsize=1, env=run_env)

            # Read line by line
            if p.stdout:
                for line in p.stdout:
                    logger.debug(line.strip())
                    _log_lines.append(line)
                    # Trim if too long
                    while len(_log_lines) > 500:
                        _log_lines.pop(0)

            p.wait()
            return p.returncode
        except Exception as e:
            logger.error(f"Command execution failed: {e}")
            _log_lines.append(f"Error: {e}\n")
            return -1

    @reactive.effect
    @reactive.event(input.build_run)
    def on_build_run():
        logger.info("User clicked 'Build & Run' button")
        _log_lines.clear()
        _log_lines.append("Starting build & run...\n")

        # Capture current widget values (must be done in reactive context)
        skip_build = input.cmd_skip_build()
        clean_before_build = input.cmd_clean_before_build()
        build_type = input.build_type()
        estas_cmd = build_estas_command()

        # Build type descriptions for logging
        build_type_desc = {
            "debug": "Debug (with bounds checking and backtraces)",
            "release": "Release (standard optimizations)",
            "fast": "Fast (aggressive optimizations)"
        }

        _log_lines.append(f"Build type: {build_type_desc.get(build_type, build_type)}\n")

        # Validate constants file before running
        const_file = input.cmd_constants_file() or ""
        if not const_file:
            try:
                if input.cmd_binary_enabled() or input.cmd_shear_stress_file():
                    const_file = DEFAULT_CONSTANTS_FILE
            except Exception:
                pass

        if const_file:
            is_valid, actual_count, error_msg = validate_constants_file(const_file)
            if not is_valid:
                _log_lines.append(f"❌ VALIDATION ERROR:\n{error_msg}\n")
                _log_lines.append("Model run aborted. Please select a constants file with all required parameters.\n")
                logger.error(f"Constants file validation failed: {error_msg}")
                return
            else:
                _log_lines.append(f"✓ Constants file validated: {const_file} ({actual_count} constants)\n")

        def _work():
            start_time = time.time()
            logger.info("Build & Run thread started")

            # Calculate total steps
            step = 1
            if skip_build:
                total_steps = 1  # Just run
            elif clean_before_build:
                total_steps = 4  # Clean + link + build-lib + build-estas + run
            else:
                total_steps = 3  # link + build-lib + build-estas + run (build-lib and build-estas combined)

            if not skip_build:
                # Clean if requested (important when switching build types)
                if clean_before_build:
                    _log_lines.append(f"Step {step}/{total_steps}: Cleaning previous build\n")
                    run_command(["make", "clean-lib"], cwd=ROOT)
                    step += 1

                _log_lines.append(f"Step {step}/{total_steps}: Running 'make link-data'\n")
                run_command(["make", "link-data"], cwd=ROOT)
                step += 1

                _log_lines.append(f"Step {step}/{total_steps}: Building library and executable (BUILD_TYPE={build_type})\n")
                rc = run_command(["make", f"BUILD_TYPE={build_type}", "build-estas"], cwd=ROOT)
                if rc != 0:
                    _log_lines.append(f"❌ Build failed with return code {rc}\n")
                    return
                step += 1
            else:
                _log_lines.append("Skipping library build (using existing libaquabc.a)\n")

            # Run ESTAS_II with custom command line parameters
            cmd_display = " ".join([c if c else '""' for c in estas_cmd])
            _log_lines.append(f"Step {step}/{total_steps}: Running ESTAS_II\n")
            _log_lines.append(f"Command: {cmd_display}\n")
            
            # Filter out empty strings for actual execution
            exec_cmd = [c for c in estas_cmd if c]
            rc = run_command(exec_cmd, cwd=ROOT)

            elapsed = time.time() - start_time
            final_msg = f"Finished with rc={rc} (total time: {elapsed:.1f}s)\n"
            logger.info(final_msg.strip())
            _log_lines.append(final_msg)

        threading.Thread(target=_work, daemon=True, name="BuildRunThread").start()

    # Track running model process for progress monitoring
    # Use lists as mutable containers for thread-safety (can't use reactive values from threads)
    _model_process = [None]
    _model_running = [False]
    _last_run_time = [None]
    _model_progress = [{"elapsed": "", "rows": 0, "size_kb": 0, "status": "idle"}]

    @reactive.effect
    @reactive.event(input.run)
    def on_run():
        logger.info("User clicked 'Run' button")
        _log_lines.clear()
        _log_lines.append("=" * 50 + "\n")
        _log_lines.append("Starting model run...\n")
        _log_lines.append("=" * 50 + "\n")

        try:
            # Capture current widget values (must be done in reactive context)
            estas_cmd = build_estas_command()

            # Check if executable exists
            try:
                exe_name = input.run_executable()
            except Exception:
                exe_name = "ESTAS_II"

            exe_path = os.path.join(ROOT, exe_name)
            if not os.path.exists(exe_path):
                _log_lines.append(f"❌ ERROR: Executable '{exe_name}' not found.\n")
                _log_lines.append("Please go to Model Build to compile the model first.\n")
                return

            # Check if it's a release build (stripped = no debug output)
            exe_info = get_executable_info(exe_name)
            is_release = exe_info.get("stripped", False) or not exe_info.get("has_debug", True)

            _log_lines.append(f"Executable: {exe_name}\n")
            if is_release:
                _log_lines.append("Build type: Release (optimized, minimal console output)\n")
            else:
                _log_lines.append("Build type: Debug (with diagnostic output)\n")

            # Validate constants file before running
            const_file = input.cmd_constants_file() or ""
            if not const_file:
                try:
                    if input.cmd_binary_enabled():
                        const_file = DEFAULT_CONSTANTS_FILE
                except Exception:
                    pass

            if const_file:
                is_valid, actual_count, error_msg = validate_constants_file(const_file)
                if not is_valid:
                    _log_lines.append(f"❌ VALIDATION ERROR:\n{error_msg}\n")
                    _log_lines.append("Model run aborted. Please select a constants file with all required parameters.\n")
                    logger.error(f"Constants file validation failed: {error_msg}")
                    return
                else:
                    _log_lines.append(f"✓ Constants file validated: {const_file} ({actual_count} constants)\n")

            # Show command
            cmd_display = " ".join([c if c else '""' for c in estas_cmd])
            _log_lines.append(f"\nCommand: {cmd_display}\n")
            _log_lines.append("-" * 50 + "\n")

            if is_release:
                _log_lines.append("ℹ️  Release builds produce minimal output.\n")
                _log_lines.append("    Progress is tracked via OUTPUT.csv file.\n")
                _log_lines.append("-" * 50 + "\n")

        except Exception as e:
            _log_lines.append(f"\n❌ Error preparing model run: {e}\n")
            _log_lines.append(f"Traceback:\n{traceback.format_exc()}\n")
            logger.error(f"Error in on_run setup: {e}\n{traceback.format_exc()}")
            return

        def _work():
            start_time = time.time()
            logger.info("Run thread started")

            # Filter out empty strings for actual execution
            exec_cmd = [c for c in estas_cmd if c]

            try:
                # For Intel executables, wrap command to source Intel environment
                use_shell = False
                final_cmd = exec_cmd
                run_env = os.environ.copy()  # Start with current environment
                
                if is_intel_executable(exe_name):
                    setvars_path = get_intel_setvars_path()
                    if setvars_path:
                        # Use shell command that sources Intel environment first
                        final_cmd, use_shell = build_intel_wrapped_command(exec_cmd)
                        _log_lines.append(f"ℹ️  Sourcing Intel environment: {setvars_path}\n")
                        logger.info(f"Using Intel wrapper with setvars: {setvars_path}")
                    else:
                        # Fall back to LD_LIBRARY_PATH approach
                        run_env = get_run_environment()
                else:
                    # For non-Intel executables, use standard environment
                    run_env = get_run_environment()
                
                # Start the model process
                logger.info(f"Executing: {final_cmd if isinstance(final_cmd, str) else ' '.join(final_cmd)}")
                p = subprocess.Popen(
                    final_cmd,
                    cwd=ROOT,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True,
                    bufsize=1,
                    env=run_env,
                    shell=use_shell,
                    executable="/bin/bash" if use_shell else None
                )
                _model_process[0] = p
                _model_running[0] = True
                _last_run_time[0] = datetime.now()

                # Progress tracking variables
                last_update = time.time()
                spinner = ['|', '/', '-', '\\']
                spinner_idx = 0
                last_lines = 0  # Track from start of this run

                # Read output in non-blocking manner with progress updates
                while p.poll() is None:
                    # Check for output (with timeout)
                    if p.stdout:
                        # Use select for non-blocking read on Unix
                        try:
                            readable, _, _ = select.select([p.stdout], [], [], 0.5)
                            if readable:
                                line = p.stdout.readline()
                                if line:
                                    _log_lines.append(line)
                                    while len(_log_lines) > 500:
                                        _log_lines.pop(0)
                        except Exception:
                            # Fallback: just sleep
                            time.sleep(0.5)

                    # Update progress every 2 seconds
                    now = time.time()
                    if now - last_update >= 2.0:
                        elapsed = now - start_time
                        output_info = output_data.get_output_files_info()
                        file_count = output_info.get("file_count", 0)
                        size_kb = output_info.get("size_kb", 0)
                        out_folder = output_info.get("folder", "OUTPUTS")

                        # Calculate files written since start
                        new_files = file_count - last_lines if file_count > last_lines else 0
                        if new_files > 0:
                            last_lines = file_count

                        # Update progress line (replace last progress line if exists)
                        spinner_char = spinner[spinner_idx % len(spinner)]
                        spinner_idx += 1

                        progress_msg = (
                            f"\r{spinner_char} Running... "
                            f"Elapsed: {output_data.format_elapsed(elapsed)} | "
                            f"{out_folder}/: {file_count} files ({size_kb:.1f} KB)\n"
                        )

                        # Remove old progress line and add new one
                        if _log_lines and _log_lines[-1].startswith(("\r", "|", "/", "-", "\\")):
                            _log_lines[-1] = progress_msg
                        else:
                            _log_lines.append(progress_msg)

                        last_update = now

                # Read any remaining output
                if p.stdout:
                    remaining = p.stdout.read()
                    if remaining:
                        _log_lines.append(remaining)

                p.wait()
                rc = p.returncode

                elapsed = time.time() - start_time
                _log_lines.append("-" * 50 + "\n")

                # Get final output info
                final_output_info = output_data.get_output_files_info()
                final_files = final_output_info.get("file_count", 0)
                final_out = final_output_info.get("out_files", 0)
                final_bin = final_output_info.get("bin_files", 0)
                final_size = final_output_info.get("size_kb", 0)
                out_folder = final_output_info.get("folder", "OUTPUTS")

                if rc == 0:
                    _log_lines.append(f"✓ Model run completed successfully!\n")
                    _log_lines.append(f"  Total time: {output_data.format_elapsed(elapsed)}\n")
                    _log_lines.append(f"  Output folder: {out_folder}/\n")
                    _log_lines.append(f"  Files: {final_files} total ({final_out} .out, {final_bin} .bin), {final_size:.1f} KB\n")
                else:
                    _log_lines.append(f"✗ Model run failed with return code {rc}\n")
                    _log_lines.append(f"  Total time: {output_data.format_elapsed(elapsed)}\n")

                _log_lines.append("=" * 50 + "\n")
                logger.info(f"Model run finished: rc={rc}, elapsed={elapsed:.1f}s")

            except Exception as e:
                _log_lines.append(f"\n❌ Error running model: {e}\n")
                logger.error(f"Model run error: {e}")

            finally:
                _model_process[0] = None
                _model_running[0] = False

        threading.Thread(target=_work, daemon=True, name="RunThread").start()

    @render.text
    def run_log():
        # Poll every 500ms to catch updates from background threads
        reactive.invalidate_later(0.5)
        # Read from shared list (thread-safe)
        return ''.join(_log_lines[-100:])  # Last 100 lines

    # Note: run_log_mini is defined earlier in the server function

    @render.ui
    def run_progress_bar():
        """Display progress status bar for model run"""
        reactive.invalidate_later(0.5)
        progress = _model_progress[0]
        status = progress.get("status", "idle")
        elapsed = progress.get("elapsed", "")
        rows = progress.get("rows", 0)
        size_kb = progress.get("size_kb", 0)

        if status == "idle":
            return ui.div(
                ui.tags.span("Ready", class_="text-muted"),
                class_="p-2 mb-2 rounded",
                style="background-color: #2d2d2d; font-family: monospace;"
            )
        elif status == "running":
            return ui.div(
                ui.tags.span("⏱ ", style="font-size: 1.2em;"),
                ui.tags.span(elapsed, class_="fw-bold text-warning", style="font-size: 1.3em; margin-right: 15px;"),
                ui.tags.span("OUTPUT: ", class_="text-muted"),
                ui.tags.span(f"{rows:,} rows", class_="text-info", style="margin-right: 10px;"),
                ui.tags.span(f"({size_kb:.1f} KB)", class_="text-muted"),
                ui.tags.span(" ● Running", class_="text-success fw-bold", style="margin-left: 15px;"),
                class_="p-2 mb-2 rounded",
                style="background-color: #1a3d1a; font-family: monospace; border: 1px solid #2d5a2d;"
            )
        elif status == "completed":
            return ui.div(
                ui.tags.span("✓ ", style="font-size: 1.2em; color: #4caf50;"),
                ui.tags.span(elapsed, class_="fw-bold text-success", style="font-size: 1.3em; margin-right: 15px;"),
                ui.tags.span("OUTPUT: ", class_="text-muted"),
                ui.tags.span(f"{rows:,} rows", class_="text-info", style="margin-right: 10px;"),
                ui.tags.span(f"({size_kb:.1f} KB)", class_="text-muted"),
                ui.tags.span(" Completed", class_="text-success fw-bold", style="margin-left: 15px;"),
                class_="p-2 mb-2 rounded",
                style="background-color: #1a3d1a; font-family: monospace; border: 1px solid #4caf50;"
            )
        elif status == "failed":
            return ui.div(
                ui.tags.span("✗ ", style="font-size: 1.2em; color: #f44336;"),
                ui.tags.span(elapsed, class_="fw-bold text-danger", style="font-size: 1.3em; margin-right: 15px;"),
                ui.tags.span("OUTPUT: ", class_="text-muted"),
                ui.tags.span(f"{rows:,} rows", class_="text-info", style="margin-right: 10px;"),
                ui.tags.span(f"({size_kb:.1f} KB)", class_="text-muted"),
                ui.tags.span(" Failed", class_="text-danger fw-bold", style="margin-left: 15px;"),
                class_="p-2 mb-2 rounded",
                style="background-color: #3d1a1a; font-family: monospace; border: 1px solid #f44336;"
            )
        else:
            return ui.div(
                ui.tags.span("Error", class_="text-danger"),
                class_="p-2 mb-2 rounded",
                style="background-color: #3d1a1a; font-family: monospace;"
            )

    @render.ui
    def dashboard_run_log():
        """Run log for Dashboard panel with scrollable output"""
        reactive.invalidate_later(0.5)
        # Show last 300 lines in the dashboard log
        log_content = ''.join(_log_lines[-300:])

        # Format the log with proper HTML styling
        return ui.tags.pre(
            log_content,
            style="margin: 0; padding: 0; font-family: 'Consolas', 'Monaco', 'Courier New', monospace; "
                  "font-size: 12px; line-height: 1.4; color: #d4d4d4; white-space: pre-wrap; "
                  "word-wrap: break-word; background: transparent;"
        )

    @render.ui
    def run_timer_display():
        """Large prominent timer display for dashboard"""
        reactive.invalidate_later(0.5)
        progress = _model_progress[0]
        status = progress.get("status", "idle")
        elapsed = progress.get("elapsed", "00:00")
        rows = progress.get("rows", 0)
        size_kb = progress.get("size_kb", 0)

        base_style = "display: flex; align-items: center; justify-content: center; height: 100%; min-height: 48px; padding: 8px 16px; border-radius: 6px; width: 100%;"

        if status == "running":
            return ui.div(
                ui.tags.span("⏱ ", style="font-size: 1.5em; color: #ffc107;"),
                ui.tags.span(elapsed, style="font-size: 1.8em; font-weight: bold; color: #ffc107; font-family: monospace;"),
                ui.tags.span(f"  {rows:,} rows", style="font-size: 0.9em; color: #17a2b8; margin-left: 12px;"),
                ui.tags.span(f" ({size_kb:.1f} KB)", style="font-size: 0.85em; color: #6c757d;"),
                style=base_style + "background: linear-gradient(135deg, #1a3d1a 0%, #2d5a2d 100%); border: 2px solid #4caf50;"
            )
        elif status == "completed":
            return ui.div(
                ui.tags.span("✓ ", style="font-size: 1.5em; color: #4caf50;"),
                ui.tags.span(elapsed, style="font-size: 1.8em; font-weight: bold; color: #4caf50; font-family: monospace;"),
                ui.tags.span(f"  {rows:,} rows", style="font-size: 0.9em; color: #17a2b8; margin-left: 12px;"),
                ui.tags.span(" Done", style="font-size: 0.9em; color: #4caf50; margin-left: 8px;"),
                style=base_style + "background: linear-gradient(135deg, #1a3d1a 0%, #2d5a2d 100%); border: 2px solid #4caf50;"
            )
        elif status == "failed":
            return ui.div(
                ui.tags.span("✗ ", style="font-size: 1.5em; color: #f44336;"),
                ui.tags.span(elapsed, style="font-size: 1.8em; font-weight: bold; color: #f44336; font-family: monospace;"),
                ui.tags.span(" Failed", style="font-size: 0.9em; color: #f44336; margin-left: 12px;"),
                style=base_style + "background: linear-gradient(135deg, #3d1a1a 0%, #5a2d2d 100%); border: 2px solid #f44336;"
            )
        else:
            return ui.div(
                ui.tags.span("○ ", style="font-size: 1.5em; color: #6c757d;"),
                ui.tags.span("Ready", style="font-size: 1.2em; color: #6c757d; font-family: monospace;"),
                style=base_style + "background: #2d2d2d; border: 2px solid #444;"
            )

    @render.ui
    def system_status_compact():
        """Compact system status for dashboard"""
        items = []

        # Working directory
        items.append(ui.div(
            ui.tags.strong("Directory: "),
            ui.tags.span(os.path.basename(ROOT), class_="text-info"),
            class_="mb-1"
        ))

        # Last run info
        if os.path.exists(OUTPUT_CSV):
            mtime = datetime.fromtimestamp(os.path.getmtime(OUTPUT_CSV))
            items.append(ui.div(
                ui.tags.strong("Last Run: "),
                ui.tags.span(mtime.strftime('%m-%d %H:%M'), class_="text-muted"),
                class_="mb-1"
            ))
            try:
                # Efficient line count using file size estimate
                file_size = os.path.getsize(OUTPUT_CSV)
                # Estimate lines (avg ~100 bytes per line in CSV)
                estimated_lines = file_size // 100
                items.append(ui.div(
                    ui.tags.strong("Output: "),
                    ui.tags.span(f"~{estimated_lines:,} rows ({file_size // 1024:,} KB)", class_="text-success"),
                    class_="mb-1"
                ))
            except Exception:
                pass
        else:
            items.append(ui.div(
                ui.tags.strong("Last Run: "),
                ui.tags.span("Never", class_="text-muted"),
                class_="mb-1"
            ))

        # Executable
        try:
            exe_name = input.run_executable() or "ESTAS_II"
        except Exception:
            exe_name = "ESTAS_II"
        exe_exists = os.path.exists(os.path.join(ROOT, exe_name))
        items.append(ui.div(
            ui.tags.strong("Exe: "),
            ui.tags.span(exe_name, class_="text-success" if exe_exists else "text-danger"),
            class_="mb-1"
        ))

        # Command preview
        try:
            cmd = build_estas_command()
            cmd_str = " ".join(cmd)
        except Exception:
            cmd_str = "(error)"
        items.append(ui.div(
            ui.tags.strong("Cmd: "),
            ui.tags.code(cmd_str, style="font-size: 10px; word-break: break-all;"),
            class_="mb-1"
        ))

        return ui.div(*items)

    @render.text
    def dashboard_status_text():
        return "Running" if _model_running[0] else "Ready"

    @render.text
    def dashboard_exe_text():
        try:
            return input.active_executable()
        except Exception:
            return "ESTAS_II"

    @render.text
    def dashboard_last_run_text():
        if _last_run_time[0]:
            return _last_run_time[0].strftime("%Y-%m-%d %H:%M")
        return "Never"

    @render.ui
    def input_txt_variables():
        """Display INPUT.txt variables with labels"""
        reactive.invalidate_later(5.0)  # Refresh every 5 seconds
        # Also refresh when output config is saved
        _ = output_config_version.get()
        # Also refresh when simulation config is saved
        _ = sim_config_save_msg.get()

        def make_row(label, value, unit=""):
            return ui.div(
                ui.tags.span(label + ": ", class_="text-muted", style="font-size: 11px;"),
                ui.tags.span(str(value), class_="fw-bold text-info"),
                ui.tags.span(f" {unit}" if unit else "", class_="text-muted", style="font-size: 10px;"),
                class_="mb-1", style="line-height: 1.3;"
            )

        items = []
        try:
            input_path = os.path.join(ROOT, "INPUT.txt")
            if os.path.exists(input_path):
                with open(input_path, 'r') as f:
                    lines = f.readlines()

                # First pass: get base_year for date conversion
                base_year = 1998  # default
                for i, line in enumerate(lines):
                    if line.strip().startswith("# BASE_YEAR") and i + 1 < len(lines):
                        try:
                            base_year = int(lines[i+1].strip())
                        except Exception:
                            pass
                        break

                def julian_to_date(julian_day, base_year):
                    """Convert Julian day to actual date string"""
                    try:
                        base_date = date(base_year, 1, 1)
                        actual_date = base_date + timedelta(days=int(float(julian_day)) - 1)
                        return actual_date.strftime("%d-%b-%Y")
                    except Exception:
                        return str(julian_day)

                # Parse key variables (skip base year display)
                i = 0
                while i < len(lines):
                    line = lines[i].strip()
                    if line.startswith("# SIMULATION_START") and i + 1 < len(lines):
                        julian = lines[i+1].strip()
                        items.append(make_row("Start Date", julian_to_date(julian, base_year)))
                    elif line.startswith("# SIMULATION_END") and i + 1 < len(lines):
                        julian = lines[i+1].strip()
                        items.append(make_row("End Date", julian_to_date(julian, base_year)))
                    elif line.startswith("# NUM_REPEATS") and i + 1 < len(lines):
                        items.append(make_row("Repeats", lines[i+1].strip()))
                    elif line.startswith("# TIME_STEPS_PER_DAY") and i + 1 < len(lines):
                        items.append(make_row("Steps/Day", lines[i+1].strip()))
                    elif line.startswith("# PRINT_INTERVAL") and i + 1 < len(lines):
                        items.append(make_row("Print Interval", lines[i+1].strip(), "steps"))
                    elif line.startswith("# PELAGIC MODEL INPUT FOLDER") and i + 1 < len(lines):
                        items.append(make_row("Input Folder", lines[i+1].strip()))
                    elif line.startswith("# PELAGIC MODEL OUTPUT FOLDER") and i + 1 < len(lines):
                        items.append(make_row("Output Folder", lines[i+1].strip()))
                    elif line.startswith("# RESUSPENSION_OPTION") and i + 1 < len(lines):
                        val = lines[i+1].strip()
                        label = {"0": "Off", "1": "Prescribed", "2": "Semi-prescribed"}.get(val, val)
                        items.append(make_row("Resuspension", label))
                    elif line.startswith("# MODEL_SEDIMENTS") and i + 1 < len(lines):
                        val = lines[i+1].strip()
                        label = "Yes" if val != "0" else "No"
                        items.append(make_row("Sediments", label))
                    i += 1

                # Calculate simulation days
                try:
                    start_idx = next(i for i, l in enumerate(lines) if "SIMULATION_START" in l)
                    end_idx = next(i for i, l in enumerate(lines) if "SIMULATION_END" in l)
                    start = float(lines[start_idx + 1].strip())
                    end = float(lines[end_idx + 1].strip())
                    days = int(end - start)
                    items.append(ui.tags.hr(style="margin: 5px 0;"))
                    items.append(make_row("Duration", days, "days"))
                except Exception:
                    pass

                # Read output box settings from PELAGIC_OUTPUT_INFORMATION_FILE.txt
                try:
                    output_info_path = os.path.join(ROOT, "INPUTS", "PELAGIC_OUTPUT_INFORMATION_FILE.txt")
                    if os.path.exists(output_info_path):
                        with open(output_info_path, 'r') as f:
                            output_lines = f.readlines()
                        output_boxes = []
                        for line in output_lines[1:]:  # Skip header
                            parts = line.split()
                            if len(parts) >= 4:
                                box_num = parts[0]
                                # Include box if ANY output type is enabled
                                state_var = parts[1] == "1"
                                process_rate = parts[2] == "1"
                                mass_balance = parts[3] == "1"
                                if state_var or process_rate or mass_balance:
                                    output_boxes.append(box_num)
                        if output_boxes:
                            items.append(ui.tags.hr(style="margin: 5px 0;"))
                            items.append(make_row("Output Boxes", ", ".join(output_boxes)))
                except Exception:
                    pass

        except Exception as e:
            items.append(ui.div(f"Error reading INPUT.txt: {e}", class_="text-danger"))

        return ui.div(*items)

    @render.ui
    def run_status_indicator():
        """Show running status indicator"""
        reactive.invalidate_later(1.0)
        is_running = _model_running[0]

        if is_running:
            return ui.div(
                ui.tags.span("● ", class_="text-success", style="font-size: 1.2em;"),
                ui.tags.span("Model is running...", class_="text-success fw-bold"),
                class_="mt-2 mb-2"
            )
        else:
            return ui.div(
                ui.tags.span("○ ", class_="text-muted", style="font-size: 1.2em;"),
                ui.tags.span("Ready", class_="text-muted"),
                class_="mt-2 mb-2"
            )

    @reactive.effect
    @reactive.event(input.stop_run)
    def on_stop_run():
        """Stop the running model"""
        logger.info("User clicked Stop button")
        process = _model_process[0]
        if process and process.poll() is None:
            try:
                # Try graceful termination first
                process.terminate()
                _log_lines.append("\n⚠️ Stop requested - terminating model...\n")

                # Wait a bit for graceful shutdown
                try:
                    process.wait(timeout=3)
                    _log_lines.append("Model terminated gracefully.\n")
                except subprocess.TimeoutExpired:
                    # Force kill if not responding
                    process.kill()
                    _log_lines.append("Model force killed.\n")

                _model_running[0] = False
                _model_process[0] = None
            except Exception as e:
                _log_lines.append(f"Error stopping model: {e}\n")
                logger.error(f"Error stopping model: {e}")
        else:
            _log_lines.append("No model is currently running.\n")

    @reactive.effect
    @reactive.event(input.dashboard_stop)
    def on_dashboard_stop():
        """Stop the running model from dashboard"""
        logger.info("User clicked Dashboard Stop button")
        process = _model_process[0]
        if process and process.poll() is None:
            try:
                # Try graceful termination first
                process.terminate()
                _log_lines.append("\n⚠️ Stop requested - terminating model...\n")

                # Wait a bit for graceful shutdown
                try:
                    process.wait(timeout=3)
                    _log_lines.append("Model terminated gracefully.\n")
                except subprocess.TimeoutExpired:
                    # Force kill if not responding
                    process.kill()
                    _log_lines.append("Model force killed.\n")

                _model_running[0] = False
                _model_process[0] = None
                _model_progress[0] = ({"elapsed": "", "rows": 0, "size_kb": 0, "status": "idle"})
            except Exception as e:
                _log_lines.append(f"Error stopping model: {e}\n")
                logger.error(f"Error stopping model: {e}")
        else:
            _log_lines.append("No model is currently running.\n")

    @render.table
    def out_preview():
        try:
            # Limit rows using nrows to avoid full read
            n = input.nrows()
            if n is None:
                n = 10
            df = pd.read_csv(OUTPUT_CSV, comment='#', nrows=n)
            # Strip whitespace from column names
            df.columns = [c.strip() for c in df.columns]
            return df
        except Exception as e:
            logger.error(f"Error reading OUTPUT.csv preview: {e}")
            return pd.DataFrame([["error reading OUTPUT.csv", str(e)]])

    # ========== OUTPUT DIRECTORY MANAGEMENT ==========
    def analyze_output_directory(dir_name):
        """Analyze files in an output directory and return summary"""
        if dir_name == "ROOT":
            dir_path = ROOT
            files_to_check = ["OUTPUT.csv"]
        else:
            dir_path = os.path.join(ROOT, dir_name)
            if not os.path.isdir(dir_path):
                return {"error": f"Directory not found: {dir_name}"}
            files_to_check = os.listdir(dir_path)
        
        summary = {
            "path": dir_path,
            "files": [],
            "total_size": 0,
            "csv_files": 0,
            "out_files": 0,
            "mtrx_files": 0,
            "bin_files": 0,
        }
        
        for fname in files_to_check:
            if dir_name == "ROOT":
                fpath = os.path.join(ROOT, fname)
            else:
                fpath = os.path.join(dir_path, fname)
            
            if not os.path.isfile(fpath):
                continue
                
            try:
                stat = os.stat(fpath)
                size = stat.st_size
                mtime = datetime.fromtimestamp(stat.st_mtime).strftime('%Y-%m-%d %H:%M:%S')
                
                file_info = {
                    "name": fname,
                    "size": size,
                    "size_str": f"{size / 1024:.1f} KB" if size < 1024*1024 else f"{size / (1024*1024):.2f} MB",
                    "modified": mtime,
                    "type": "unknown"
                }
                
                # Categorize file
                if fname.endswith(".csv"):
                    file_info["type"] = "csv"
                    summary["csv_files"] += 1
                    # Try to get row count for CSV
                    try:
                        with open(fpath, 'rb') as f:
                            lines = sum(1 for _ in f)
                        file_info["rows"] = lines
                    except (OSError, UnicodeDecodeError):
                        file_info["rows"] = "?"
                elif fname.endswith(".out"):
                    file_info["type"] = "output"
                    summary["out_files"] += 1
                elif fname.endswith(".mtrx"):
                    file_info["type"] = "matrix"
                    summary["mtrx_files"] += 1
                elif fname.endswith(".bin"):
                    file_info["type"] = "binary"
                    summary["bin_files"] += 1
                
                summary["files"].append(file_info)
                summary["total_size"] += size
            except Exception as e:
                logger.warning(f"Error analyzing file {fname}: {e}")
        
        # Sort files by type then name
        summary["files"].sort(key=lambda x: (x["type"], x["name"]))
        summary["total_size_str"] = f"{summary['total_size'] / (1024*1024):.2f} MB"
        return summary

    @reactive.effect
    def init_output_dirs():
        """Initialize output directory selection on startup - uses INPUT.txt value"""
        dirs = output_data.get_output_directories()
        if dirs:
            # Try to get output folder from INPUT.txt config
            default_dir = "OUTPUTS"  # fallback
            try:
                if os.path.exists(INPUT_TXT_PATH):
                    scf = SimulationConfigFile(INPUT_TXT_PATH)
                    if scf.parse():
                        # Get output folder from config (strip trailing slash)
                        config_output = scf.config.output_folder.rstrip('/')
                        if config_output in dirs:
                            default_dir = config_output
                            logger.info(f"Using output folder from INPUT.txt: {default_dir}")
                        else:
                            logger.warning(f"Output folder '{config_output}' from INPUT.txt not found, using OUTPUTS")
            except Exception as e:
                logger.warning(f"Could not read output folder from INPUT.txt: {e}")
            
            # Fall back to OUTPUTS if no config or folder not found
            if default_dir not in dirs:
                default_dir = "OUTPUTS" if "OUTPUTS" in dirs else list(dirs.keys())[0]
            
            ui.update_select("output_dir_select", choices=dirs, selected=default_dir)
            # Also update Model Config output directory
            ui.update_select("sim_output_dir", choices=dirs, selected=default_dir)

    @reactive.effect
    @reactive.event(input.refresh_output_dirs)
    def refresh_output_dirs():
        """Refresh the list of output directories"""
        dirs = output_data.get_output_directories()
        current = input.output_dir_select()
        selected = current if current in dirs else (list(dirs.keys())[0] if dirs else None)
        ui.update_select("output_dir_select", choices=dirs, selected=selected)

    @reactive.effect
    @reactive.event(input.refresh_sim_output_dirs)
    def refresh_sim_output_dirs():
        """Refresh the output directory list in Model Config"""
        dirs = output_data.get_output_directories()
        current = input.sim_output_dir()
        selected = current if current in dirs else (list(dirs.keys())[0] if dirs else None)
        ui.update_select("sim_output_dir", choices=dirs, selected=selected)

    @render.text
    def sim_output_dir_info():
        """Show info about selected output directory in Model Config"""
        dir_name = input.sim_output_dir()
        if not dir_name:
            return ""
        if dir_name == "ROOT":
            dir_path = ROOT
        else:
            dir_path = os.path.join(ROOT, dir_name)
        if os.path.exists(dir_path):
            files = [f for f in os.listdir(dir_path) if f.endswith(('.bin', '.out', '.csv'))]
            return f"📁 {len(files)} output files"
        return "Directory not found"

    @render.ui
    def output_file_preview():
        """Display preview info about the selected output file"""
        dir_name = input.output_dir_select()
        file_name = input.plot_output_file()
        
        if not dir_name or not file_name:
            return ui.div(ui.tags.em("Select a file to preview", class_="text-muted"))
        
        # Build file path
        if dir_name == "ROOT":
            file_path = os.path.join(ROOT, file_name)
        else:
            file_path = os.path.join(ROOT, dir_name, file_name)
        
        if not os.path.exists(file_path):
            return ui.div(ui.tags.em(f"File not found: {file_name}", class_="text-danger"))
        
        try:
            # Get file stats
            stat = os.stat(file_path)
            mtime = datetime.fromtimestamp(stat.st_mtime).strftime('%Y-%m-%d %H:%M:%S')
            size = stat.st_size
            size_str = f"{size / 1024:.1f} KB" if size < 1024*1024 else f"{size / (1024*1024):.2f} MB"
            
            # Detect file format and read metadata
            file_ext = os.path.splitext(file_path)[1].lower()
            num_vars = 0
            num_rows = 0
            time_range = ""
            
            if file_ext == '.bin':
                # Binary file - calculate from file size
                num_cols = 37  # PELAGIC_BOX binary format
                bytes_per_row = num_cols * 8  # float64
                num_rows = size // bytes_per_row
                num_vars = num_cols - 1  # Exclude TIME column
                # Read first and last time values
                try:
                    import numpy as np
                    data = np.fromfile(file_path, dtype=np.float64)
                    if len(data) >= num_cols:
                        first_time = data[0]
                        last_time = data[-num_cols]
                        from datetime import datetime as dt, timedelta
                        ref_date = dt(1997, 1, 1)
                        start_date = (ref_date + timedelta(days=float(first_time))).strftime('%Y-%m-%d')
                        end_date = (ref_date + timedelta(days=float(last_time))).strftime('%Y-%m-%d')
                        time_range = f"{start_date} to {end_date}"
                except (OSError, ValueError, IndexError):
                    pass
            elif file_ext == '.csv':
                # CSV file
                df = pd.read_csv(file_path, comment='#', nrows=5)
                num_vars = len(df.columns) - 1  # Exclude TIME
                # Count rows
                with open(file_path, 'rb') as f:
                    num_rows = sum(1 for _ in f) - 1  # Subtract header
            elif file_ext in ['.out', '.txt', '.dat']:
                # Whitespace-delimited file
                df = pd.read_csv(file_path, sep=r'\s+', nrows=5)
                num_vars = len(df.columns) - 1  # Exclude TIME
                # Count rows
                with open(file_path, 'rb') as f:
                    num_rows = sum(1 for _ in f) - 1  # Subtract header
                # Try to get time range
                try:
                    df_full = pd.read_csv(file_path, sep=r'\s+', usecols=[0])
                    first_time = df_full.iloc[0, 0]
                    last_time = df_full.iloc[-1, 0]
                    from datetime import datetime as dt, timedelta
                    ref_date = dt(1997, 1, 1)
                    start_date = (ref_date + timedelta(days=float(first_time))).strftime('%Y-%m-%d')
                    end_date = (ref_date + timedelta(days=float(last_time))).strftime('%Y-%m-%d')
                    time_range = f"{start_date} to {end_date}"
                except (OSError, ValueError, IndexError, KeyError):
                    pass

            # Build preview display
            items = [
                ui.tags.p(ui.tags.strong("📄 File: "), file_name),
                ui.tags.p(ui.tags.strong("📊 Variables: "), str(num_vars)),
                ui.tags.p(ui.tags.strong("📈 Data points: "), f"{num_rows:,}"),
                ui.tags.p(ui.tags.strong("💾 Size: "), size_str),
            ]
            
            if time_range:
                items.append(ui.tags.p(ui.tags.strong("📅 Period: "), time_range))
            
            items.append(ui.tags.p(ui.tags.strong("🕐 Modified: "), mtime))
            
            return ui.div(*items)
            
        except Exception as e:
            logger.warning(f"Error previewing file {file_path}: {e}")
            return ui.div(ui.tags.em(f"Error reading file: {str(e)}", class_="text-danger"))

    @render.ui
    @reactive.event(input.analyze_output_dir)
    def output_files_summary():
        """Analyze and display detailed file summary for selected directory"""
        dir_name = input.output_dir_select()
        if not dir_name:
            return ui.div(ui.tags.em("Select a directory and click 'Analyze Directory'", class_="text-muted"))
        
        summary = analyze_output_directory(dir_name)
        
        if "error" in summary:
            return ui.div(ui.tags.span(summary["error"], class_="text-danger"))
        
        if not summary["files"]:
            return ui.div(ui.tags.em("No files found in directory", class_="text-muted"))
        
        # Build summary cards
        stats_row = ui.layout_columns(
            ui.value_box(
                "Total Files",
                str(len(summary["files"])),
                theme="primary"
            ),
            ui.value_box(
                "CSV Files",
                str(summary["csv_files"]),
                theme="success"
            ),
            ui.value_box(
                "Output Files",
                str(summary["out_files"]),
                theme="info"
            ),
            ui.value_box(
                "Total Size",
                summary["total_size_str"],
                theme="secondary"
            ),
            col_widths=[3, 3, 3, 3]
        )
        
        # Build file table
        table_rows = []
        for f in summary["files"]:
            type_badge = {
                "csv": ("CSV", "bg-success"),
                "output": ("OUT", "bg-info"),
                "matrix": ("MTRX", "bg-warning"),
                "binary": ("BIN", "bg-secondary"),
                "unknown": ("?", "bg-light text-dark"),
            }.get(f["type"], ("?", "bg-light"))
            
            row_content = [
                ui.tags.td(ui.tags.span(type_badge[0], class_=f"badge {type_badge[1]}")),
                ui.tags.td(f["name"]),
                ui.tags.td(f["size_str"]),
                ui.tags.td(f["modified"]),
            ]
            if "rows" in f:
                row_content.append(ui.tags.td(str(f["rows"]) + " rows"))
            else:
                row_content.append(ui.tags.td("-"))
            
            table_rows.append(ui.tags.tr(*row_content))
        
        file_table = ui.tags.table(
            ui.tags.thead(
                ui.tags.tr(
                    ui.tags.th("Type"),
                    ui.tags.th("Filename"),
                    ui.tags.th("Size"),
                    ui.tags.th("Modified"),
                    ui.tags.th("Info"),
                )
            ),
            ui.tags.tbody(*table_rows),
            class_="table table-sm table-striped"
        )
        
        return ui.div(
            stats_row,
            ui.tags.hr(),
            file_table
        )

    # ========== PLOT OUTPUT FILE SELECTION ==========
    def get_selected_output_file_path():
        """Get full path to selected output file"""
        dir_name = input.output_dir_select()
        file_name = input.plot_output_file()
        
        if not dir_name or not file_name:
            return None
        
        if dir_name == "ROOT":
            return os.path.join(ROOT, file_name)
        else:
            return os.path.join(ROOT, dir_name, file_name)

    @reactive.effect
    def update_plot_output_files():
        """Update file selection when output directory or format changes"""
        dir_name = input.output_dir_select()
        file_format = input.output_format() if hasattr(input, 'output_format') else "text"
        files = output_data.get_output_files_from_dir(dir_name, file_format)
        
        # Select first file by default
        selected = None
        if "OUTPUT.csv" in files:
            selected = "OUTPUT.csv"
        elif files:
            selected = list(files.keys())[0]
        
        ui.update_select("plot_output_file", choices=files, selected=selected)

    @reactive.effect
    @reactive.event(input.refresh_plot_files, input.output_format)
    def refresh_plot_output_files():
        """Refresh the list of output files when format changes"""
        dir_name = input.output_dir_select()
        file_format = input.output_format() if hasattr(input, 'output_format') else "text"
        files = output_data.get_output_files_from_dir(dir_name, file_format)
        current = input.plot_output_file()
        selected = current if current in files else (list(files.keys())[0] if files else None)
        ui.update_select("plot_output_file", choices=files, selected=selected)

    @render.ui
    def plot_output_file_info():
        """Display info about selected output file"""
        file_path = get_selected_output_file_path()
        
        if not file_path or not os.path.exists(file_path):
            return ui.div(ui.tags.small("No file selected", class_="text-muted"))
        
        try:
            stat = os.stat(file_path)
            size = stat.st_size
            mtime = datetime.fromtimestamp(stat.st_mtime).strftime('%Y-%m-%d %H:%M')
            
            # Count lines
            with open(file_path, 'rb') as f:
                lines = sum(1 for _ in f)
            
            size_str = f"{size / 1024:.1f} KB" if size < 1024*1024 else f"{size / (1024*1024):.2f} MB"
            
            return ui.div(
                ui.tags.small(f"📄 {size_str} | {lines:,} rows | {mtime}", class_="text-muted")
            )
        except Exception as e:
            return ui.div(ui.tags.small(f"Error: {e}", class_="text-danger"))

    # NOTE: update_plot_variables was removed — _update_variable_choices (earlier)
    # now handles all three triggers: plot_output_file, output_format, output_dir_select

    # ========== INPUT TIMESERIES PLOTTING ==========
    @reactive.effect
    def update_input_ts_boxes():
        """Update box selection when timeseries file changes"""
        ts_file = input.input_ts_file()
        if not ts_file:
            return

        # Get available boxes from the timeseries file
        filepath = os.path.join(INPUTS_DIR, ts_file)
        if os.path.exists(filepath):
            analysis = analyze_input_file(filepath)
            num_vars = analysis.get("num_variables", 0)
            if num_vars > 0:
                # Create box choices (1-indexed)
                boxes = {str(i): f"Box {i}" for i in range(1, min(num_vars + 1, 26))}
                ui.update_selectize("input_ts_boxes", choices=boxes, selected=["1"])

    @render.text
    def input_ts_info():
        """Display info about selected timeseries file"""
        ts_file = input.input_ts_file()
        if not ts_file:
            return "Select a timeseries file"

        filepath = os.path.join(INPUTS_DIR, ts_file)
        if not os.path.exists(filepath):
            return f"File not found: {ts_file}"

        analysis = analyze_input_file(filepath)
        info_parts = []

        if analysis.get("date_start") and analysis.get("date_end"):
            info_parts.append(f"Period: {analysis['date_start']} to {analysis['date_end']}")

        if analysis.get("data_size"):
            info_parts.append(f"Data points: {analysis['data_size']:,}")

        if analysis.get("num_variables"):
            info_parts.append(f"Variables: {analysis['num_variables']}")

        return " | ".join(info_parts) if info_parts else "File info unavailable"

    @render.ui
    def input_ts_date_range():
        """Show date range selector if subset is enabled"""
        if not input.input_ts_subset():
            return ui.TagList()

        ts_file = input.input_ts_file()
        if not ts_file:
            return ui.TagList()

        filepath = os.path.join(INPUTS_DIR, ts_file)
        analysis = analyze_input_file(filepath)

        start_date = analysis.get("date_start", "2008-01-01")
        end_date = analysis.get("date_end", "2015-01-01")

        return ui.TagList(
            ui.input_date("input_ts_start", "Start:", value=start_date),
            ui.input_date("input_ts_end", "End:", value=end_date)
        )

    @render_widget
    @reactive.event(input.plot_input_ts)
    def input_ts_plot():
        """Plot selected input timeseries for selected boxes"""
        ts_file = input.input_ts_file()
        selected_boxes = list(input.input_ts_boxes() or [])

        if not ts_file or not selected_boxes:
            logger.info("No timeseries file or boxes selected")
            return None

        filepath = os.path.join(INPUTS_DIR, ts_file)
        if not os.path.exists(filepath):
            logger.warning(f"Timeseries file not found: {filepath}")
            return None

        logger.info(f"Plotting input timeseries: {ts_file}, boxes: {selected_boxes}")

        try:
            # Read the timeseries file
            # Skip header lines (lines starting with #)
            with open(filepath, 'r') as f:
                lines = f.readlines()

            # Find data start - look for the column header line "# TIME" and start after it
            # The data format has headers with comments, then "# TIME TEMP1 TEMP2 ..." 
            # followed by actual data rows
            data_start = 0
            for i, line in enumerate(lines):
                stripped = line.strip()
                # Look for the column header line that contains "TIME" as a comment
                if stripped.startswith('#') and 'TIME' in stripped.upper():
                    # Data starts on the next line
                    data_start = i + 1
                    # Don't break - keep looking for the LAST such header
            
            # If no TIME header found, fall back to finding first data line
            if data_start == 0:
                for i, line in enumerate(lines):
                    stripped = line.strip()
                    if stripped and not stripped.startswith('#'):
                        parts = stripped.split()
                        if len(parts) >= 2:
                            try:
                                float(parts[0])
                                float(parts[1])
                                data_start = i
                                break
                            except (ValueError, IndexError):
                                continue

            logger.debug(f"Input timeseries data starts at line {data_start}")
            
            # Read as DataFrame
            df = pd.read_csv(filepath, skiprows=data_start, sep=r'\s+', header=None)

            if df.empty:
                logger.warning("No data in timeseries file")
                return None

            # First column is TIME
            time_col = df.iloc[:, 0]

            # Convert Julian days to dates
            reference_date = date(1997, 1, 1)
            dates = [reference_date + timedelta(days=float(t)) for t in time_col]

            # Apply time subsetting if enabled
            if input.input_ts_subset():
                try:
                    start = input.input_ts_start()
                    end = input.input_ts_end()
                    if start and end:
                        mask = [(d >= start and d <= end) for d in dates]
                        df = df[mask]
                        dates = [d for d, m in zip(dates, mask) if m]
                except Exception as e:
                    logger.warning(f"Error applying date filter: {e}")

            # Create plot
            fig = go.Figure()

            # Get variable name base from filename
            var_name = ts_file.replace("_TS.txt", "").replace(".txt", "")

            for box in selected_boxes:
                box_idx = int(box)  # 1-indexed
                if box_idx < df.shape[1]:
                    y_data = df.iloc[:, box_idx]
                    fig.add_trace(go.Scatter(
                        x=dates,
                        y=y_data,
                        mode='lines',
                        name=f"Box {box_idx}"
                    ))

            fig.update_layout(
                title=f"{var_name} - Input Timeseries",
                xaxis_title="Date",
                yaxis_title=var_name,
                hovermode='x unified'
            )

            logger.info(f"Input timeseries plot generated with {len(selected_boxes)} traces")
            return go.FigureWidget(fig)

        except Exception as e:
            logger.error(f"Error plotting input timeseries: {e}")
            return None

    @render_widget
    @reactive.event(input.refresh_plot, ignore_none=False)
    def main_plot():
        logger.info("Generating plot (triggered by 'Refresh plot' button)")
        left = list(input.left_vars() or [])
        right = list(input.right_vars() or [])
        logger.debug(f"Left vars: {left}, Right vars: {right}")

        if not left and not right:
            logger.info("No variables selected, skipping plot")
            return None

        # Get the selected output file path and format
        selected_file = get_selected_output_file_path()
        if not selected_file:
            logger.warning("No output file selected")
            return None
        
        file_format = input.output_format() if hasattr(input, 'output_format') else None
        logger.info(f"Plotting from: {selected_file} (format: {file_format})")

        # Use format-aware data loading
        logger.debug(f"Fetching data (max {DEFAULT_PLOT_ROWS} rows, format={file_format})")
        df = _get_cached_data(max_rows=DEFAULT_PLOT_ROWS, file_path=selected_file, file_format=file_format)
        if df is None or df.empty:
            logger.warning("No data available for plotting")
            return None

        xcol = df.columns[0]
        apply_smooth = input.smooth()
        win = input.smooth_window() if apply_smooth else 1
        logger.debug(f"X-axis: {xcol}, Smoothing: {apply_smooth} (window={win})")

        # Convert Julian days to actual dates for the x-axis
        # Reference date: 1997-01-01 (used in AQUABC model)
        from datetime import datetime, timedelta
        reference_date = datetime(1997, 1, 1)
        try:
            # Create date column from Julian days
            x_dates = [reference_date + timedelta(days=float(jd)) for jd in df[xcol]]
            x_data = x_dates
            x_label = 'Date'
            x_is_date = True
            logger.debug(f"Date conversion successful: {x_dates[0]} to {x_dates[-1]}")
        except (ValueError, TypeError) as e:
            logger.warning(f"Could not convert Julian days to dates: {e}")
            x_data = df[xcol].tolist()
            x_label = xcol
            x_is_date = False

        fig = go.Figure()
        trace_count = 0

        # Helper function to get just the description (no code, no units)
        def get_var_description(var):
            info = get_variable_info(var)
            if info:
                return info['description']
            return var

        # left axis traces
        for var in left:
            if var not in df.columns:
                logger.warning(f"Variable '{var}' not found in CSV columns")
                continue
            y = df[var]
            if apply_smooth and win > 1:
                y = y.rolling(window=win, min_periods=1).mean()
            fig.add_trace(go.Scatter(x=x_data, y=y.tolist(), mode='lines', name=get_var_description(var), yaxis='y'))
            trace_count += 1

        # right axis traces
        for var in right:
            if var not in df.columns:
                logger.warning(f"Variable '{var}' not found in CSV columns")
                continue
            y = df[var]
            if apply_smooth and win > 1:
                y = y.rolling(window=win, min_periods=1).mean()
            fig.add_trace(go.Scatter(x=x_data, y=y.tolist(), mode='lines', name=get_var_description(var), yaxis='y2'))
            trace_count += 1

        logger.debug(f"Created {trace_count} traces")

        # Create descriptive axis titles using just descriptions
        left_descriptions = [get_var_description(var) for var in left]
        right_descriptions = [get_var_description(var) for var in right]
        left_title = ', '.join(left_descriptions) if left else 'Left axis'
        right_title = ', '.join(right_descriptions) if right else 'Right axis'

        # Plot title with variable descriptions
        all_vars = [get_var_description(v) for v in (left + right)]
        plot_title = f"{', '.join(all_vars)} vs {x_label}"

        # Configure xaxis based on whether we have date data
        if x_is_date:
            xaxis_config = dict(
                title=x_label,
                type='date',
                tickformat='%Y-%m-%d',
                tickmode='auto',
                nticks=10  # Show approximately 10 ticks
            )
        else:
            xaxis_config = dict(title=x_label)

        layout = dict(
            title=plot_title,
            yaxis=dict(title=left_title),
            xaxis=xaxis_config,
            # Horizontal legend at the bottom to save space
            legend=dict(
                orientation='h',
                yanchor='top',
                y=-0.15,
                xanchor='center',
                x=0.5
            ),
            margin=dict(b=80)  # Extra bottom margin for legend
        )

        if right:
            layout['yaxis2'] = dict(title=right_title, overlaying='y', side='right')

        log_scale_info = []
        if input.log_left():
            layout['yaxis']['type'] = 'log'
            log_scale_info.append("left")

        if right and input.log_right():
            if 'yaxis2' not in layout:
                layout['yaxis2'] = {}
            layout['yaxis2']['type'] = 'log'
            log_scale_info.append("right")

        if log_scale_info:
            logger.debug(f"Log scale applied to: {', '.join(log_scale_info)}")

        fig.update_layout(**layout)
        logger.info(f"Plot generated successfully with {trace_count} traces")
        return go.FigureWidget(fig)

    # === IPYLEAFLET MAP RENDER ===
    @render_widget
    def pydeck_map():
        """Render an interactive ipyleaflet map with sample data points."""
        # Observation station locations in the Curonian Lagoon (WGS84 / EPSG:4326)
        # sta1ND = Nida, sta2VM = Ventė Cape
        stations = [
            {'lat': 55.3028, 'lon': 21.0003, 'name': 'Nida (sta1ND)', 'value': 15},
            {'lat': 55.3417, 'lon': 21.1900, 'name': 'Ventė (sta2VM)', 'value': 25},
        ]
        
        # Get user inputs
        center_lat = input.map_lat() or 55.32
        center_lon = input.map_lon() or 21.10
        zoom = input.map_zoom() or 10
        map_style = input.map_style() or "OpenStreetMap.Mapnik"
        point_radius = input.map_point_radius() or 1000
        
        # Get basemap from ipyleaflet basemaps
        basemap_parts = map_style.split('.')
        if len(basemap_parts) == 2:
            basemap = getattr(getattr(L.basemaps, basemap_parts[0], L.basemaps.OpenStreetMap), basemap_parts[1], L.basemaps.OpenStreetMap.Mapnik)
        else:
            basemap = getattr(L.basemaps, map_style, L.basemaps.OpenStreetMap.Mapnik)
        
        # Create the ipyleaflet map
        m = L.Map(
            center=(center_lat, center_lon),
            zoom=zoom,
            basemap=basemap,
            scroll_wheel_zoom=True,
            layout={'height': '600px'}
        )
        
        # Add markers for each station
        for station in stations:
            # Color based on value (gradient from green to red)
            value_normalized = station['value'] / 30.0  # Normalize to 0-1
            r = int(200 * value_normalized)
            g = int(200 * (1 - value_normalized))
            color = f'#{r:02x}{g:02x}50'
            
            # Create popup content
            popup_html = f"""
            <div style="font-family: Arial, sans-serif;">
                <h4 style="margin: 0 0 10px 0; color: steelblue;">{station['name']}</h4>
                <table style="width: 100%;">
                    <tr><td><b>Value:</b></td><td>{station['value']}</td></tr>
                    <tr><td><b>Lat:</b></td><td>{station['lat']:.4f}</td></tr>
                    <tr><td><b>Lon:</b></td><td>{station['lon']:.4f}</td></tr>
                </table>
            </div>
            """
            
            # Add circle marker
            circle = L.CircleMarker(
                location=(station['lat'], station['lon']),
                radius=int(point_radius / 100),  # Scale down for leaflet
                color=color,
                fill_color=color,
                fill_opacity=0.7,
                weight=2
            )
            circle.popup = HTML(popup_html)
            m.add_layer(circle)
        
        # Add layer control
        m.add_control(L.LayersControl(position='topright'))
        m.add_control(L.FullScreenControl())
        m.add_control(L.ScaleControl(position='bottomleft'))
        
        return m

    @render.ui
    def map_info():
        """Display map information and instructions."""
        return ui.div(
            ui.tags.p(
                ui.tags.strong("About this map: "),
                "This interactive map displays observation stations in the Curonian Lagoon: Nida (sta1ND) and Ventė Cape (sta2VM). ",
                "Use the controls on the left to adjust the map view and visualization settings."
            ),
            ui.tags.p(
                ui.tags.strong("Features: "),
                ui.tags.ul(
                    ui.tags.li("Circle markers showing station locations (color indicates value)"),
                    ui.tags.li("Click on markers to see station details"),
                    ui.tags.li("Use mouse to pan and scroll to zoom"),
                    ui.tags.li("Full screen control available in top-right corner")
                )
            ),
            ui.tags.p(
                ui.tags.em("Map powered by ipyleaflet with free OpenStreetMap tiles."),
                class_="text-muted"
            ),
            class_="small"
        )

    # ── Model Structure Diagram ──
    @render.ui
    def model_structure_iframe():
        """Generate the ecological model diagram as a static HTML file and serve via iframe"""
        try:
            import json as _json
            diagram_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "www")
            os.makedirs(diagram_dir, exist_ok=True)
            diagram_path = os.path.join(diagram_dir, "model_diagram.html")

            # Only regenerate if file doesn't exist (delete www/model_diagram.html to force refresh)
            if not os.path.exists(diagram_path):
                logger.info("Generating model structure diagram HTML...")

                # ── Node definitions ──
                nodes_data = []
                def _node(nid, label, grp, title="", shape="dot", size=22, bg="#888", border="#555", font_color="#FFF", font_size=12):
                    nodes_data.append({
                        "id": nid, "label": label, "title": title or label, "group": grp,
                        "shape": shape, "size": size,
                        "color": {"background": bg, "border": border},
                        "font": {"color": font_color, "size": font_size}
                    })

                edges_data = []
                def _edge(src, dst, label="", color="#888888", width=1.5, dashes=False):
                    edges_data.append({"from": src, "to": dst, "label": label, "color": color, "width": width, "dashes": dashes})

                # Forcing
                for nid, lbl in [("TEMP","Temperature"),("PAR","Light (PAR)"),("WIND","Wind Speed"),("ICE","Ice Cover"),("SALT","Salinity"),("DEPTH","Depth")]:
                    _node(nid, lbl, "forcing", shape="diamond", size=22, bg="#F59E0B", border="#D97706", font_color="#1F2937", font_size=12)
                # Phytoplankton
                for nid, lbl, ttl in [("DIA","Diatoms\\n(DIA_C)","Si-limited diatoms"),("CYN","Non-fixing\\nCyanobacteria",""),("FIX","N2-fixing\\nCyanobacteria",""),("OPA","Other Plankton\\nAlgae",""),("NOST","Nostocales\\n(NOST_C)","Obligate N2 fixer")]:
                    _node(nid, lbl, "phyto", title=ttl, shape="dot", size=28, bg="#34D399", border="#059669", font_color="#064E3B", font_size=13)
                # Zooplankton
                _node("ZOO","Zooplankton\\n(ZOO_C/N/P)","zoo","Multi-prey Monod grazing","dot",30,"#F87171","#DC2626","#FFFFFF",13)
                # Organic matter
                _node("POM","Particulate OM\\n(POC,PON,POP)","pom",shape="dot",size=24,bg="#D97706",border="#92400E",font_color="#FFFFFF",font_size=12)
                _node("DOM","Dissolved OM\\n(DOC,DON,DOP)","dom",shape="dot",size=24,bg="#A78BFA",border="#7C3AED",font_color="#FFFFFF",font_size=12)
                # Nutrients
                for nid, lbl in [("NH4","NH4-N"),("NO3","NO3-N"),("PO4","PO4-P"),("DSi","Dissolved Si"),("PSi","Biogenic Si")]:
                    _node(nid, lbl, "nutrient", shape="dot", size=22, bg="#3B82F6", border="#1D4ED8", font_color="#FFFFFF", font_size=12)
                # Oxygen
                _node("DO","Dissolved O2","oxygen","Sources: photosynthesis, aeration","star",30,"#38BDF8","#0284C7","#0C4A6E",13)
                # Carbon
                _node("DIC","DIC","carbon",shape="dot",size=22,bg="#FCD34D",border="#F59E0B",font_color="#78350F",font_size=12)
                _node("ALK","Total\\nAlkalinity","carbon",shape="dot",size=22,bg="#FCD34D",border="#F59E0B",font_color="#78350F",font_size=12)
                # Redox
                for nid, lbl in [("FE2","Fe2+"),("FE3","Fe3+"),("MN2","Mn2+"),("MN4","Mn4+"),("SO4","SO4-S"),("H2S","H2S-S"),("CH4","CH4-C")]:
                    _node(nid, lbl, "redox", shape="dot", size=20, bg="#8B5CF6", border="#6D28D9", font_color="#FFFFFF", font_size=11)
                # Macroalgae
                _node("MAC","Macroalgae\\n(C,N,P)","macro",shape="dot",size=25,bg="#14B8A6",border="#0D9488",font_color="#FFFFFF",font_size=12)
                _node("MACDET","Attached\\nDetritus","macro",shape="dot",size=25,bg="#14B8A6",border="#0D9488",font_color="#FFFFFF",font_size=12)
                # Allelopathy
                for nid, lbl in [("SM_DIA","Diatom\\nMetabolites"),("SM_CYN","Cyanobact.\\nMetabolites"),("SM_FIX","Fix-Cyan.\\nMetabolites"),("SM_NOST","Nostocales\\nMetabolites")]:
                    _node(nid, lbl, "allelo", shape="triangle", size=18, bg="#FB7185", border="#E11D48", font_color="#881337", font_size=11)
                # Sediment
                for nid, lbl in [("SED_OM","Sediment OM"),("SED_NUT","Sediment\\nNutrients"),("SED_REDOX","Sediment\\nRedox"),("SED_TRANS","Transport\\n(Diff/Adv)")]:
                    _node(nid, lbl, "sediment", shape="box", size=25, bg="#64748B", border="#334155", font_color="#FFFFFF", font_size=12)
                # CO2SYS
                _node("CO2SYS","CO2SYS\\nCarbonate","co2sys",shape="box",size=25,bg="#E2E8F0",border="#94A3B8",font_color="#1E293B",font_size=12)

                # ── Edges ──
                for t in ["DIA","CYN","FIX","OPA","NOST","ZOO","DOM","POM","DO"]:
                    _edge("TEMP",t,color="#F59E0B44",width=1,dashes=True)
                for p in ["DIA","CYN","FIX","OPA","NOST","MAC"]:
                    _edge("PAR",p,"Light",color="#FBBF24",width=1.5)
                _edge("WIND","DO","Aeration",color="#FBBF24",width=1.5)
                _edge("ICE","DO","Suppress aeration",color="#93C5FD",width=1.5)
                _edge("SALT","CO2SYS","",color="#FBBF24",width=1)
                _edge("DEPTH","DO","",color="#F59E0B44",width=1,dashes=True)
                g="#059669"
                for p in ["DIA","CYN","FIX","OPA","NOST"]:
                    _edge("NH4",p,"N uptake",color=g,width=1.5); _edge("NO3",p,"N uptake",color=g,width=1.5)
                    _edge("PO4",p,"P uptake",color=g,width=1.5); _edge("DIC",p,"Photosynthesis",color="#6EE7B7",width=1.5)
                    _edge(p,"DO","O2",color="#38BDF8",width=2); _edge(p,"POM","Death",color="#B45309",width=1.5)
                    _edge(p,"DOM","Excretion",color="#C4B5FD",width=1)
                _edge("DSi","DIA","Si uptake",color="#1D4ED8",width=2)
                _edge("DIA","ZOO","Graze(0.26)",color="#DC2626",width=2.5); _edge("CYN","ZOO","Graze(0.10)",color="#DC2626",width=1.5)
                _edge("FIX","ZOO","Graze(0.07)",color="#DC2626",width=1.2); _edge("OPA","ZOO","Graze(0.37)",color="#DC2626",width=3)
                _edge("NOST","ZOO","Graze",color="#DC2626",width=1.5); _edge("POM","ZOO","Graze(0.20)",color="#DC2626",width=2)
                _edge("ZOO","POM","Death+faecal",color="#B45309",width=2); _edge("ZOO","DOM","Excretion",color="#C4B5FD",width=1.5)
                _edge("ZOO","DO","Respiration",color="#FCA5A5",width=1.5)
                _edge("POM","DOM","Dissolution",color="#7C3AED",width=3)
                _edge("DOM","DIC","Mineralisation",color="#6D28D9",width=3)
                _edge("DOM","NH4","N mineral.",color="#3B82F6",width=2); _edge("DOM","PO4","P mineral.",color="#3B82F6",width=2)
                _edge("NH4","NO3","Nitrification",color="#1D4ED8",width=2.5)
                _edge("NO3","DIC","Denitrification",color="#7C3AED",width=2)
                _edge("MN4","MN2","Mn reduction",color="#8B5CF6",width=2); _edge("FE3","FE2","Fe reduction",color="#8B5CF6",width=2)
                _edge("SO4","H2S","SO4 reduction",color="#8B5CF6",width=2); _edge("DOM","CH4","Methanogenesis",color="#8B5CF6",width=2)
                _edge("FE2","FE3","Oxidation",color="#A78BFA",width=1.5); _edge("MN2","MN4","Oxidation",color="#A78BFA",width=1.5)
                _edge("H2S","SO4","Oxidation",color="#A78BFA",width=1.5); _edge("CH4","DIC","CH4 oxidation",color="#A78BFA",width=1.5)
                _edge("FE3","PO4","FePO4",color="#E11D48",width=2,dashes=True)
                _edge("DIC","CO2SYS","Equilibrium",color="#94A3B8",width=2); _edge("ALK","CO2SYS","Equilibrium",color="#94A3B8",width=2)
                _edge("CO2SYS","DIC","pH feedback",color="#94A3B8",width=1,dashes=True)
                _edge("DIA","PSi","Diatom mort.",color="#1D4ED8",width=1.5); _edge("PSi","DSi","Dissolution",color="#60A5FA",width=1.5)
                sc="#475569"
                _edge("POM","SED_OM","Settling",color=sc,width=2.5); _edge("PSi","SED_OM","Settling",color=sc,width=1.5)
                for p in ["DIA","CYN","OPA"]: _edge(p,"SED_OM","Settling",color=sc,width=1)
                sf="#64748B"
                _edge("SED_NUT","NH4","Sed flux",color=sf,width=2); _edge("SED_NUT","NO3","Sed flux",color=sf,width=1.5)
                _edge("SED_NUT","PO4","Sed flux",color=sf,width=2); _edge("SED_NUT","DSi","Sed flux",color=sf,width=1.5)
                _edge("SED_NUT","DO","O2 demand",color="#DC2626",width=2.5)
                _edge("SED_OM","DOM","DOC release",color=sf,width=1.5); _edge("SED_OM","DIC","DIC release",color=sf,width=1.5)
                _edge("SED_REDOX","FE2","Fe2+ flux",color=sf,width=1); _edge("SED_REDOX","H2S","H2S flux",color=sf,width=1)
                _edge("SED_REDOX","CH4","CH4 flux",color=sf,width=1)
                _edge("SED_OM","SED_NUT","Mineralisation",color="#334155",width=2)
                _edge("SED_OM","SED_REDOX","Redox coupling",color="#334155",width=2)
                for t in ["SED_OM","SED_NUT","SED_REDOX"]: _edge("SED_TRANS",t,"Transport",color="#475569",width=1,dashes=True)
                mc="#0D9488"
                _edge("NH4","MAC","N uptake",color=mc,width=1.5); _edge("NO3","MAC","N uptake",color=mc,width=1.5)
                _edge("PO4","MAC","P uptake",color=mc,width=1.5); _edge("MAC","DO","O2",color="#38BDF8",width=1.5)
                _edge("MAC","MACDET","Death",color=mc,width=2); _edge("MACDET","POM","Deattach",color=mc,width=2)
                ac="#E11D48"
                _edge("DIA","SM_DIA","Metabolites",color=ac,width=1); _edge("CYN","SM_CYN","Metabolites",color=ac,width=1)
                _edge("FIX","SM_FIX","Metabolites",color=ac,width=1); _edge("NOST","SM_NOST","Metabolites",color=ac,width=1)
                for sm in ["SM_DIA","SM_CYN","SM_FIX","SM_NOST"]:
                    for p in ["DIA","CYN","FIX","OPA","NOST"]:
                        _edge(sm,p,"Inhibition",color="#FDA4AF",width=1,dashes=True)

                # ── Build self-contained HTML with vis-network from CDN ──
                nodes_json = _json.dumps(nodes_data)
                edges_json = _json.dumps(edges_data)

                html_content = f'''<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>AQUABC Ecological Model</title>
<link rel="stylesheet" href="https://unpkg.com/vis-network@9.1.9/dist/dist/vis-network.min.css" />
<script src="https://unpkg.com/vis-network@9.1.9/dist/vis-network.min.js"></script>
<style>
  body {{ margin: 0; font-family: 'Segoe UI', Helvetica, Arial, sans-serif; overflow: hidden; }}
  .diagram-header {{
    background: linear-gradient(135deg, #1E293B 0%, #334155 100%);
    color: #F1F5F9; padding: 14px 24px 10px; text-align: center;
  }}
  .diagram-header h1 {{ margin: 0 0 4px; font-size: 1.4em; font-weight: 600; }}
  .diagram-header p {{ margin: 0; font-size: 0.8em; color: #94A3B8; }}
  .legend {{
    display: flex; flex-wrap: wrap; justify-content: center;
    gap: 8px 14px; padding: 8px 16px; background: #F8FAFC;
    border-bottom: 1px solid #E2E8F0; font-size: 0.75em;
  }}
  .legend-item {{ display: flex; align-items: center; gap: 4px; }}
  .legend-swatch {{
    width: 12px; height: 12px; border-radius: 3px;
    border: 1.5px solid rgba(0,0,0,0.15); flex-shrink: 0;
  }}
  #network-container {{
    width: 100%; height: calc(100vh - 100px);
    background-color: #F5F7FA; border-top: 1px solid lightgray;
  }}
  div.vis-navigation div.vis-button {{
    background-color: #334155 !important;
    border: 1.5px solid #475569 !important;
    border-radius: 6px !important;
  }}
  div.vis-navigation div.vis-button:hover {{
    background-color: #475569 !important;
  }}
</style>
</head>
<body>
<div class="diagram-header">
  <h1>AQUABC v0.3 &mdash; Ecological Model Architecture</h1>
  <p>Interactive biogeochemical network &middot; 32 pelagic + 24 sediment + 6 macroalgae + 4 allelopathy state variables</p>
</div>
<div class="legend">
  <div class="legend-item"><div class="legend-swatch" style="background:#F59E0B;"></div>Forcing</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#34D399;"></div>Phytoplankton</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#F87171;"></div>Zooplankton</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#3B82F6;"></div>Nutrients</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#38BDF8;"></div>Dissolved O&#x2082;</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#FCD34D;"></div>Inorganic C</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#D97706;"></div>Particulate OM</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#A78BFA;"></div>Dissolved OM</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#8B5CF6;"></div>Redox Species</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#64748B;"></div>Sediment</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#14B8A6;"></div>Macroalgae</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#FB7185;"></div>Allelopathy</div>
  <div class="legend-item"><div class="legend-swatch" style="background:#E2E8F0;"></div>CO&#x2082;SYS</div>
</div>
<div id="network-container"></div>
<script>
var nodes = new vis.DataSet({nodes_json});
var edges = new vis.DataSet({edges_json});
var container = document.getElementById("network-container");
var data = {{ nodes: nodes, edges: edges }};
var options = {{
  physics: {{
    barnesHut: {{
      gravitationalConstant: -12000, centralGravity: 0.8,
      springLength: 180, springConstant: 0.02,
      damping: 0.4, avoidOverlap: 0.6
    }},
    maxVelocity: 30, minVelocity: 0.75,
    stabilization: {{ enabled: true, iterations: 500, updateInterval: 25 }}
  }},
  edges: {{
    smooth: {{ type: "curvedCW", roundness: 0.15 }},
    font: {{ size: 10, color: "#555555", strokeWidth: 2, strokeColor: "#FFFFFF" }},
    arrows: {{ to: {{ enabled: true, scaleFactor: 0.7 }} }}
  }},
  nodes: {{
    font: {{ size: 13, face: "Helvetica" }},
    borderWidth: 2, shadow: true
  }},
  interaction: {{ hover: true, tooltipDelay: 200, navigationButtons: true, keyboard: true }}
}};
var network = new vis.Network(container, data, options);
</script>
</body>
</html>'''

                with open(diagram_path, "w", encoding="utf-8") as f:
                    f.write(html_content)
                logger.info(f"Model diagram written to {diagram_path}")

            # Return an iframe pointing to the static file
            return ui.HTML(
                '<iframe src="model_diagram.html" '
                'style="width:100%;height:calc(100vh - 180px);border:none;border-radius:8px;" '
                'loading="lazy" sandbox="allow-scripts allow-same-origin"></iframe>'
            )

        except Exception as e:
            logger.error(f"Error generating model structure diagram: {e}")
            import traceback
            logger.error(traceback.format_exc())
            return ui.div(
                ui.tags.p(f"Error generating diagram: {e}", class_="text-danger"),
                ui.tags.p("Ensure pyvis is installed in the shiny environment.", class_="text-muted small")
            )

    # ========== DIAGNOSTICS ==========
    diagnostics_server(input, output, session, root_dir=ROOT)
    # ========== END DIAGNOSTICS ==========

    logger.info("=" * 60)
    logger.info("Server function initialization complete")
    logger.info("All reactive effects and render functions registered")
    logger.info("=" * 60)


app = App(app_ui, server, static_assets=os.path.join(os.path.dirname(os.path.abspath(__file__)), "www"))

logger.info("=" * 60)
logger.info("App object created successfully")
logger.info("Application is ready to accept connections")
logger.info("=" * 60)

if __name__ == '__main__':
    logger.info("Running in __main__ mode")
    print("=" * 60)
    print("To run this app, use one of these commands:")
    print(f"  shiny run --reload {__file__}")
    print(f"  shiny run --reload shiny_app.app:app")
    print(f"  shiny run --reload --port 8000 shiny_app.app:app")
    print("=" * 60)
