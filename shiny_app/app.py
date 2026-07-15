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
    from simulation_config import (
        SimulationConfigFile, SimulationConfig, load_simulation_config,
        TIME_STEP_PRESETS, OUTPUT_INTERVAL_PRESETS, days_to_date, date_to_days
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
        panel_dashboard, panel_model_control,
    )
except ImportError:
    from ui_panels import (
        panel_dashboard, panel_model_control,
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

try:
    from shiny_app.app_state import RunController, AppState
except ImportError:
    from app_state import RunController, AppState

try:
    from shiny_app.modules.parameters import parameters_ui, parameters_server
except ImportError:
    from modules.parameters import parameters_ui, parameters_server

try:
    from shiny_app.modules.model_structure import model_structure_ui, model_structure_server
except ImportError:
    from modules.model_structure import model_structure_ui, model_structure_server

try:
    from shiny_app.modules.map import map_ui, map_server
except ImportError:
    from modules.map import map_ui, map_server

try:
    from shiny_app.modules.model_options import model_options_ui, model_options_server
except ImportError:
    from modules.model_options import model_options_ui, model_options_server

try:
    from shiny_app.modules.initial_conditions import initial_conditions_ui, initial_conditions_server
except ImportError:
    from modules.initial_conditions import initial_conditions_ui, initial_conditions_server

try:
    from shiny_app.modules.input_files import input_files_ui, input_files_server
except ImportError:
    from modules.input_files import input_files_ui, input_files_server

try:
    from shiny_app.modules.scenarios import scenarios_ui, scenarios_server
except ImportError:
    from modules.scenarios import scenarios_ui, scenarios_server

try:
    from shiny_app.modules.model_build import (
        BUILD_TYPES, COMPILERS, model_build_server, model_build_ui,
    )
except ImportError:
    from modules.model_build import (
        BUILD_TYPES, COMPILERS, model_build_server, model_build_ui,
    )

try:
    from shiny_app.modules.mass_balance import mass_balance_ui, mass_balance_server
except ImportError:
    from modules.mass_balance import mass_balance_ui, mass_balance_server

try:
    from shiny_app.modules.observations import observations_ui, observations_server
except ImportError:
    from modules.observations import observations_ui, observations_server

try:
    from shiny_app.modules.plot import plot_ui, plot_server
except ImportError:
    from modules.plot import plot_ui, plot_server

try:
    from shiny_app.modules.sim_config import sim_config_server
except ImportError:
    from modules.sim_config import sim_config_server

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

# BUILD_TYPES/COMPILERS moved to shiny_app/modules/model_build.py (imported above)
# so the Model Build module owns its own build-configuration data.

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

    # === COMBINE ALL PANELS ===
    main_content = ui.div(
        {"class": "main-content"},
        nav_input_hidden,
        panel_dashboard(),
        ui.panel_conditional("input.navigation === 'nav_model_structure'", model_structure_ui("model_structure")),
        ui.panel_conditional("input.navigation === 'nav_model_build'", model_build_ui("model_build", COMPILERS, BUILD_TYPES)),
        panel_model_control(),
        ui.panel_conditional("input.navigation === 'nav_input_files'", input_files_ui("input_files")),
        ui.panel_conditional("input.navigation === 'nav_parameters'", parameters_ui("parameters")),
        ui.panel_conditional("input.navigation === 'nav_initial_conditions'", initial_conditions_ui("initial_conditions")),
        ui.panel_conditional("input.navigation === 'nav_model_options'", model_options_ui("model_options")),
        ui.panel_conditional("input.navigation === 'nav_scenarios'", scenarios_ui("scenarios")),
        ui.panel_conditional("input.navigation === 'nav_plot'", plot_ui("plot", MIN_SMOOTH_WINDOW)),
        ui.panel_conditional("input.navigation === 'nav_mass_balance'", mass_balance_ui("mass_balance")),
        ui.panel_conditional("input.navigation === 'nav_observations'", observations_ui("observations")),
        ui.panel_conditional("input.navigation === 'nav_map'", map_ui("map")),
        ui.panel_conditional("input.navigation === 'nav_diagnostics'", diagnostics_ui("diagnostics")),
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

    # --- Shared per-session state contract ---
    # navigate() sends a client-global custom message handled by nav_script's
    # aquabc_navigate handler, which sets the "navigation" input and updates
    # the active-link highlight. This is namespace-independent, so a converted
    # module can call it without needing to reach the global input by id.
    async def _navigate(nav_id):
        await session.send_custom_message("aquabc_navigate", {"navId": nav_id})

    state = AppState(
        run=RunController(root=ROOT),
        navigate=_navigate,
        output_config_version=reactive.Value(0),
        sim_config_version=reactive.Value(0),
    )
    state.run.exe_list_version = reactive.Value(0)      # triggers executable_list() re-render
    state.run.active_executable = reactive.Value(None)
    run = state.run                                     # local alias for brevity

    # =================
    # Log Copy Handlers
    # =================
    @reactive.effect
    @reactive.event(input.btn_copy_dashboard_log)
    async def copy_dashboard_log():
        """Copy dashboard run log to clipboard via client-side JS"""
        log_content = "".join(run.run_log_lines)
        if not log_content:
            log_content = "Log is empty."
        await session.send_custom_message("copy_to_clipboard", log_content)
        ui.notification_show("Run log copied to clipboard!", type="message", duration=2)

    @reactive.effect
    @reactive.event(input.btn_copy_mini_log)
    async def copy_mini_log():
        """Copy mini run log to clipboard via client-side JS"""
        log_content = "".join(run.run_log_lines)
        if not log_content:
            log_content = "Log is empty."
        await session.send_custom_message("copy_to_clipboard", log_content)
        ui.notification_show("Run log copied to clipboard!", type="message", duration=2)

    # Mini run log for Model Control panel
    @render.text
    def run_log_mini():
        """Abbreviated run log for sidebar"""
        # Poll every 500ms to catch updates from background threads
        reactive.invalidate_later(0.5)
        # Return last 50 lines from shared list
        return ''.join(run.run_log_lines[-50:])

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

    @reactive.calc
    def _command_config():
        return build_estas_command()
    run.command_config = _command_config

    @render.text
    def cmd_preview():
        """Show preview of the command that will be executed"""
        cmd = build_estas_command()
        return " ".join(cmd)

    # run_control -> dashboard: the Run Model tab's run_executable name (bare string)
    run.run_executable_name = reactive.Value("ESTAS_II")

    @reactive.effect
    def _publish_run_executable_name():
        run.run_executable_name.set(input.run_executable() or "ESTAS_II")

    # run_control -> dashboard: the quick-run constants-validation inputs
    @reactive.calc
    def _constants_config():
        return (input.cmd_constants_file(), input.cmd_binary_enabled(), input.cmd_shear_stress_file())
    run.constants_config = _constants_config

    # =========================================================================
    # Executable info helper + Run Model navigation (Model Build panel logic
    # moved to shiny_app/modules/model_build.py; get_executable_info stays here
    # because run_executable_info/handle_quick_run (Run Model tab, still inline)
    # also depend on it)
    # =========================================================================

    def get_executable_info(exe_name):
        """Get information about an executable (thin wrapper)."""
        return build_commands.get_executable_info(exe_name, ROOT)

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

    @reactive.effect
    @reactive.event(input.goto_build)
    async def navigate_to_build():
        """Navigate to the Model Build panel"""
        await state.navigate("nav_model_build")

    @reactive.effect
    @reactive.event(input.goto_model_config)
    async def navigate_to_model_config():
        """Navigate to the Model Config panel from dashboard"""
        await state.navigate("nav_model_control")

    # =========================================================================
    # End Executable info helper + Run Model navigation
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

    # Quick action handlers (defined early to access run.run_log_lines)
    @reactive.effect
    @reactive.event(input.quick_run)
    def handle_quick_run():
        """Quick run action from dashboard"""
        logger.info("User clicked Quick Run from dashboard")
        run.run_log_lines.clear()
        run.run_log_lines.append("Starting quick run...\n")

        # Validate required input files first
        run.run_log_lines.append("Validating input files...\n")
        is_valid, errors, warnings = validate_required_inputs()

        if warnings:
            for w in warnings:
                run.run_log_lines.append(f"⚠ {w}\n")

        if not is_valid:
            run.run_log_lines.append("❌ INPUT VALIDATION FAILED:\n")
            for e in errors:
                run.run_log_lines.append(f"  • {e}\n")
            run.run_log_lines.append("\nModel run aborted. Please ensure all required input files exist.\n")
            logger.error(f"Input validation failed: {errors}")
            return

        run.run_log_lines.append("✓ Input files validated\n")

        try:
            # Capture current widget values (must be done in reactive context)
            estas_cmd = run.command_config()

            # Check if executable exists
            exe_name = run.run_executable_name()

            exe_path = os.path.join(ROOT, exe_name)
            if not os.path.exists(exe_path):
                run.run_log_lines.append(f"❌ ERROR: Executable '{exe_name}' not found.\n")
                run.run_log_lines.append("Please go to Model Build to compile the model first.\n")
                return

            # Check Intel library requirements for Intel-compiled executables
            if is_intel_executable(exe_name):
                setvars_path = get_intel_setvars_path()
                if setvars_path:
                    run.run_log_lines.append(f"ℹ️  Intel executable detected. Will source Intel environment.\n")
                else:
                    intel_available, intel_path = check_intel_libs_available()
                    if intel_available:
                        run.run_log_lines.append(f"ℹ️  Intel executable detected. Using runtime libs from:\n")
                        run.run_log_lines.append(f"   {intel_path}\n")
                    else:
                        run.run_log_lines.append("⚠️  WARNING: Intel-compiled executable selected but Intel runtime\n")
                        run.run_log_lines.append("   libraries (libimf.so) and setvars.sh not found.\n")
                        run.run_log_lines.append("   The model may fail to start. Consider:\n")
                        run.run_log_lines.append("   • Installing Intel oneAPI or using a gfortran executable\n")
                        run.run_log_lines.append("-" * 50 + "\n")

            # Validate constants file before running
            const_file, binary_enabled, shear_file = run.constants_config()

            if not const_file and (binary_enabled or shear_file):
                const_file = DEFAULT_CONSTANTS_FILE

            if const_file:
                is_valid, actual_count, error_msg = validate_constants_file(const_file)
                if not is_valid:
                    run.run_log_lines.append(f"❌ VALIDATION ERROR:\n{error_msg}\n")
                    run.run_log_lines.append("Model run aborted. Please select a constants file with all required parameters.\n")
                    logger.error(f"Constants file validation failed: {error_msg}")
                    return
                else:
                    run.run_log_lines.append(f"✓ Constants file validated: {const_file} ({actual_count} constants)\n")

            # Show command before starting
            cmd_display = " ".join([c if c else '""' for c in estas_cmd])
            run.run_log_lines.append(f"\nCommand: {cmd_display}\n")
            run.run_log_lines.append("-" * 40 + "\n")

        except Exception as e:
            run.run_log_lines.append(f"\n❌ Error preparing quick run: {e}\n")
            run.run_log_lines.append(f"Traceback:\n{traceback.format_exc()}\n")
            logger.error(f"Error in quick_run setup: {e}\n{traceback.format_exc()}")
            return

        def _work():
            start_time = time.time()
            logger.info("Quick Run thread started")
            run.run_log_lines.append("Starting model execution...\n")

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
                        run.run_log_lines.append(f"ℹ️  Sourcing Intel environment: {setvars_path}\n")
                        logger.info(f"Using Intel wrapper with setvars: {setvars_path}")
                    else:
                        # Fall back to LD_LIBRARY_PATH approach
                        run_env = get_run_environment()
                        ld_path = run_env.get("LD_LIBRARY_PATH", "NOT SET")
                        run.run_log_lines.append(f"LD_LIBRARY_PATH: {ld_path[:200]}...\n")
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
                run.process = p
                run.running = True
                run.last_run_time = datetime.now()
                run.progress = ({"elapsed": "00:00", "rows": 0, "size_kb": 0, "status": "running"})

                last_progress_update = time.time()

                # Read output with progress updates
                while p.poll() is None:
                    if p.stdout:
                        try:
                            readable, _, _ = select.select([p.stdout], [], [], 0.5)
                            if readable:
                                line = p.stdout.readline()
                                if line:
                                    run.run_log_lines.append(line)
                                    while len(run.run_log_lines) > 1000:
                                        run.run_log_lines.pop(0)
                        except Exception:
                            time.sleep(0.5)

                    # Update progress every second
                    now = time.time()
                    if now - last_progress_update >= 1.0:
                        elapsed = now - start_time
                        output_info = get_csv_info()
                        run.progress = ({
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
                        run.run_log_lines.append(remaining)

                p.wait()
                rc = p.returncode

                elapsed = time.time() - start_time
                output_info = get_csv_info()
                run.run_log_lines.append("-" * 40 + "\n")
                if rc == 0:
                    run.run_log_lines.append(f"✓ Model run completed successfully!\n")
                    run.progress = ({
                        "elapsed": format_time(elapsed),
                        "rows": output_info.get("lines", 0),
                        "size_kb": output_info.get("size_kb", 0),
                        "status": "completed"
                    })
                else:
                    run.run_log_lines.append(f"✗ Model run failed with return code {rc}\n")
                    run.progress = ({
                        "elapsed": format_time(elapsed),
                        "rows": output_info.get("lines", 0),
                        "size_kb": output_info.get("size_kb", 0),
                        "status": "failed"
                    })
                run.run_log_lines.append(f"Total time: {format_time(elapsed)}\n")
                logger.info(f"Quick Run finished: rc={rc}, elapsed={elapsed:.1f}s")

            except Exception as e:
                run.run_log_lines.append(f"\n❌ Error running model: {e}\n")
                logger.error(f"Quick Run error: {e}")
                run.progress = ({"elapsed": "", "rows": 0, "size_kb": 0, "status": "error"})
            finally:
                run.process = None
                run.running = False

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

    # Model Build tab is a Shiny module (Phase 4)
    model_build_server("model_build", state)

    # Input Files tab is a Shiny module (Phase 2)
    input_files_server("input_files", state)

    # Parameters tab is a Shiny module (Phase 1)
    parameters_server("parameters", state)

    # Initial Conditions tab is a Shiny module (Phase 2)
    initial_conditions_server("initial_conditions", state)

    # Model Options tab is a Shiny module (Phase 2)
    model_options_server("model_options", state)

    # ========== SIMULATION CONFIGURATION (extracted to modules/sim_config.py) ==========
    sim_config_server("sim_config", state)
    # ========== END SIMULATION CONFIGURATION ==========

    # ========== OUTPUT CONFIGURATION ==========
    output_config_msg = reactive.Value("")
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
            state.output_config_version.set(state.output_config_version.get() + 1)

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

    # Scenarios tab is a Shiny module (Phase 2)
    scenarios_server("scenarios", state)

    # Mass Balance tab is a Shiny module (Phase 3)
    mass_balance_server("mass_balance", state)

    # Observations tab is a Shiny module (Phase 3)
    observations_server("observations", state)

    # Plots tab is a Shiny module (Phase 3 — whole Plots tab; merged plot + output_browser)
    plot_server("plot", state)

    @reactive.effect
    @reactive.event(input.run)
    def on_run():
        logger.info("User clicked 'Run' button")
        run.run_log_lines.clear()
        run.run_log_lines.append("=" * 50 + "\n")
        run.run_log_lines.append("Starting model run...\n")
        run.run_log_lines.append("=" * 50 + "\n")

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
                run.run_log_lines.append(f"❌ ERROR: Executable '{exe_name}' not found.\n")
                run.run_log_lines.append("Please go to Model Build to compile the model first.\n")
                return

            # Check if it's a release build (stripped = no debug output)
            exe_info = get_executable_info(exe_name)
            is_release = exe_info.get("stripped", False) or not exe_info.get("has_debug", True)

            run.run_log_lines.append(f"Executable: {exe_name}\n")
            if is_release:
                run.run_log_lines.append("Build type: Release (optimized, minimal console output)\n")
            else:
                run.run_log_lines.append("Build type: Debug (with diagnostic output)\n")

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
                    run.run_log_lines.append(f"❌ VALIDATION ERROR:\n{error_msg}\n")
                    run.run_log_lines.append("Model run aborted. Please select a constants file with all required parameters.\n")
                    logger.error(f"Constants file validation failed: {error_msg}")
                    return
                else:
                    run.run_log_lines.append(f"✓ Constants file validated: {const_file} ({actual_count} constants)\n")

            # Show command
            cmd_display = " ".join([c if c else '""' for c in estas_cmd])
            run.run_log_lines.append(f"\nCommand: {cmd_display}\n")
            run.run_log_lines.append("-" * 50 + "\n")

            if is_release:
                run.run_log_lines.append("ℹ️  Release builds produce minimal output.\n")
                run.run_log_lines.append("    Progress is tracked via OUTPUT.csv file.\n")
                run.run_log_lines.append("-" * 50 + "\n")

        except Exception as e:
            run.run_log_lines.append(f"\n❌ Error preparing model run: {e}\n")
            run.run_log_lines.append(f"Traceback:\n{traceback.format_exc()}\n")
            logger.error(f"Error in on_run setup: {e}\n{traceback.format_exc()}")
            return

        threading.Thread(
            target=run.start_run, args=(estas_cmd, exe_name),
            daemon=True, name="RunThread",
        ).start()

    @render.ui
    def dashboard_run_log():
        """Run log for Dashboard panel with scrollable output"""
        reactive.invalidate_later(0.5)
        # Show last 300 lines in the dashboard log
        log_content = ''.join(run.run_log_lines[-300:])

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
        progress = run.progress
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
            exe_name = run.run_executable_name()
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
            cmd = run.command_config()
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
        return "Running" if run.running else "Ready"

    @render.text
    def dashboard_exe_text():
        try:
            return run.active_executable()
        except Exception:
            return "ESTAS_II"

    @render.text
    def dashboard_last_run_text():
        if run.last_run_time:
            return run.last_run_time.strftime("%Y-%m-%d %H:%M")
        return "Never"

    @render.ui
    def input_txt_variables():
        """Display INPUT.txt variables with labels"""
        reactive.invalidate_later(5.0)  # Refresh every 5 seconds
        # Also refresh when output config is saved
        _ = state.output_config_version.get()
        # Also refresh when simulation config is saved
        _ = state.sim_config_version.get()

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
        is_running = run.running

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
        logger.info("User clicked Stop button")
        run.stop()

    @reactive.effect
    @reactive.event(input.dashboard_stop)
    def on_dashboard_stop():
        logger.info("User clicked Dashboard Stop button")
        run.stop(reset_progress=True)

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

    # Map tab is a Shiny module (Phase 2)
    map_server("map", state)

    # Model Structure tab is a Shiny module (Phase 2)
    model_structure_server("model_structure", state)

    # ========== DIAGNOSTICS ==========
    diagnostics_server("diagnostics", state)
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
