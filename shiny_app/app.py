#!/usr/bin/env python3
import logging
import os
import subprocess
import sys
from datetime import datetime

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

try:
    from shiny_app.config import LINE_COUNT_TIMEOUT
except ImportError:
    from config import LINE_COUNT_TIMEOUT
from shiny import App, reactive, render, ui

# Input-file analysis and validation helpers (stdlib-only, extracted for testability)
try:
    from shiny_app.input_analysis import INPUT_FILE_CATEGORIES
except ImportError:
    from input_analysis import (
        INPUT_FILE_CATEGORIES,
    )

# Import diagnostics panel (process rate analysis UI)
try:
    from shiny_app.diagnostics import diagnostics_server, diagnostics_ui
except ImportError:
    from diagnostics import diagnostics_server, diagnostics_ui

# Import UI script fragments (phase-2 create_ui() split)
try:
    from shiny_app.ui_scripts import (
        changelog_script,
        help_script,
        nav_script,
        reload_script,
        settings_script,
        theme_script,
    )
except ImportError:
    from ui_scripts import (
        changelog_script,
        help_script,
        nav_script,
        reload_script,
        settings_script,
        theme_script,
    )

# Import UI chrome fragments (phase-2c create_ui() split)
try:
    from shiny_app.ui_chrome import (
        app_header,
        build_sidebar,
        changelog_offcanvas,
        external_css,
        help_offcanvas,
        settings_offcanvas,
    )
except ImportError:
    from ui_chrome import (
        app_header,
        build_sidebar,
        changelog_offcanvas,
        external_css,
        help_offcanvas,
        settings_offcanvas,
    )

try:
    from shiny_app.app_state import AppState, RunController
except ImportError:
    from app_state import AppState, RunController

try:
    from shiny_app.modules.parameters import parameters_server, parameters_ui
except ImportError:
    from modules.parameters import parameters_server, parameters_ui

try:
    from shiny_app.modules.model_structure import (
        model_structure_server,
        model_structure_ui,
    )
except ImportError:
    from modules.model_structure import model_structure_server, model_structure_ui

try:
    from shiny_app.modules.map import map_server, map_ui
except ImportError:
    from modules.map import map_server, map_ui

try:
    from shiny_app.modules.model_options import model_options_server, model_options_ui
except ImportError:
    from modules.model_options import model_options_server, model_options_ui

try:
    from shiny_app.modules.initial_conditions import (
        initial_conditions_server,
        initial_conditions_ui,
    )
except ImportError:
    from modules.initial_conditions import (
        initial_conditions_server,
        initial_conditions_ui,
    )

try:
    from shiny_app.modules.input_files import input_files_server, input_files_ui
except ImportError:
    from modules.input_files import input_files_server, input_files_ui

try:
    from shiny_app.modules.scenarios import scenarios_server, scenarios_ui
except ImportError:
    from modules.scenarios import scenarios_server, scenarios_ui

try:
    from shiny_app.modules.model_build import (
        BUILD_TYPES,
        COMPILERS,
        model_build_server,
        model_build_ui,
    )
except ImportError:
    from modules.model_build import (
        BUILD_TYPES,
        COMPILERS,
        model_build_server,
        model_build_ui,
    )

try:
    from shiny_app.modules.mass_balance import mass_balance_server, mass_balance_ui
except ImportError:
    from modules.mass_balance import mass_balance_server, mass_balance_ui

try:
    from shiny_app.modules.observations import observations_server, observations_ui
except ImportError:
    from modules.observations import observations_server, observations_ui

try:
    from shiny_app.modules.plot import plot_server, plot_ui
except ImportError:
    from modules.plot import plot_server, plot_ui

try:
    from shiny_app.modules.sim_config import sim_config_server, sim_config_ui
except ImportError:
    from modules.sim_config import sim_config_server, sim_config_ui

try:
    from shiny_app.modules.run_control import run_control_server, run_control_ui
except ImportError:
    from modules.run_control import run_control_server, run_control_ui

try:
    from shiny_app.modules.dashboard import dashboard_server, dashboard_ui
except ImportError:
    from modules.dashboard import dashboard_server, dashboard_ui

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
    logger.info("✓ ROOT directory exists")
    logger.info(f"  ROOT is readable: {os.access(ROOT, os.R_OK)}")
    logger.info(f"  ROOT is writable: {os.access(ROOT, os.W_OK)}")
else:
    logger.error(f"✗ ROOT directory does NOT exist: {ROOT}")

if os.path.exists(INPUTS_DIR):
    logger.info("✓ INPUTS directory exists")
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
    logger.info("✓ OUTPUT.csv exists")
    file_size = os.path.getsize(OUTPUT_CSV)
    logger.info(f"  File size: {file_size:,} bytes ({file_size / 1024 / 1024:.2f} MB)")
    logger.info(f"  File is readable: {os.access(OUTPUT_CSV, os.R_OK)}")
    logger.info(f"  Last modified: {datetime.fromtimestamp(os.path.getmtime(OUTPUT_CSV)).strftime('%Y-%m-%d %H:%M:%S')}")
    try:
        # Try to read header
        with open(OUTPUT_CSV) as f:
            first_line = f.readline().strip()
            logger.info(f"  Header preview: {first_line[:100]}")
        # Count lines (quick estimate)
        try:
            result = subprocess.run(['wc', '-l', OUTPUT_CSV], capture_output=True, text=True, timeout=LINE_COUNT_TIMEOUT)
            if result.returncode == 0:
                line_count = result.stdout.split()[0]
                logger.info(f"  Line count: {line_count}")
        except Exception:
            pass
    except Exception as e:
        logger.warning(f"  Could not read OUTPUT.csv header: {e}")
else:
    logger.warning(f"⚠ OUTPUT.csv does NOT exist yet: {OUTPUT_CSV}")
    logger.info("  This is normal if the model hasn't been run yet")

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
        ui.panel_conditional("input.navigation === 'nav_dashboard'", dashboard_ui("dashboard")),
        ui.panel_conditional("input.navigation === 'nav_model_structure'", model_structure_ui("model_structure")),
        ui.panel_conditional("input.navigation === 'nav_model_build'", model_build_ui("model_build", COMPILERS, BUILD_TYPES)),
        ui.panel_conditional(
            "input.navigation === 'nav_model_control'",
            ui.navset_card_tab(sim_config_ui("sim_config"), *run_control_ui("run_control"), id="model_control_tabs")),
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

    # Dashboard tab handlers (copy_dashboard_log, navigate_to_model_config,
    # handle_quick_run, dashboard_run_log, run_timer_display,
    # system_status_compact, dashboard_status_text, dashboard_exe_text,
    # dashboard_last_run_text, input_txt_variables, on_dashboard_stop) moved
    # verbatim to shiny_app/modules/dashboard.py (Phase 4, Task 5).

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

            with open(help_file, encoding='utf-8') as f:
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

            with open(changelog_file, encoding='utf-8') as f:
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

    # Run Model + Output Config tabs are a Shiny module (Phase 4)
    run_control_server("run_control", state)

    # Scenarios tab is a Shiny module (Phase 2)
    scenarios_server("scenarios", state)

    # Mass Balance tab is a Shiny module (Phase 3)
    mass_balance_server("mass_balance", state)

    # Observations tab is a Shiny module (Phase 3)
    observations_server("observations", state)

    # Plots tab is a Shiny module (Phase 3 — whole Plots tab; merged plot + output_browser)
    plot_server("plot", state)

    # Dashboard tab is a Shiny module (Phase 4, Task 5)
    dashboard_server("dashboard", state)

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
    print("  shiny run --reload shiny_app.app:app")
    print("  shiny run --reload --port 8000 shiny_app.app:app")
    print("=" * 60)
