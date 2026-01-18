#!/usr/bin/env python3
import os
import subprocess
import threading
import logging
import sys
import shutil
from datetime import datetime, date

# Add the script's directory and parent to path for module imports
# This ensures imports work both locally and on Shiny server
_script_dir = os.path.dirname(os.path.realpath(__file__))
_parent_dir = os.path.dirname(_script_dir)
if _script_dir not in sys.path:
    sys.path.insert(0, _script_dir)
if _parent_dir not in sys.path:
    sys.path.insert(0, _parent_dir)

import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from shiny import App, ui, reactive, render, req
from shinywidgets import output_widget, render_widget

# Import parameter parser (try both absolute and relative imports)
try:
    from shiny_app.parameter_parser import ParameterFile, PARAMETER_CATEGORIES, load_parameters
    from shiny_app.ic_parser import ICFile, STATE_VARIABLE_CATEGORIES, STATE_VARIABLES, get_available_ic_files
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
    from ic_parser import ICFile, STATE_VARIABLE_CATEGORIES, STATE_VARIABLES, get_available_ic_files
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

# Import scenario manager (try both paths)
try:
    from shiny_app.scenarios import (
        Scenario, ScenarioManager, load_scenario_manager, get_scenarios_dir
    )
except ImportError:
    from scenarios import (
        Scenario, ScenarioManager, load_scenario_manager, get_scenarios_dir
    )

# Configure logging
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    stream=sys.stdout
)
logger = logging.getLogger("AQUABC")

# Try to import shinyswatch for themes
try:
    import shinyswatch
    THEMES_AVAILABLE = True
    logger.info(f"shinyswatch version {shinyswatch.__version__} loaded successfully")
except ImportError:
    THEMES_AVAILABLE = False
    logger.warning("shinyswatch not available - install with: pip install shinyswatch")
logger.info("=" * 60)
logger.info("AQUABC Application starting...")
logger.info("=" * 60)

# Constants
MAX_LOG_LENGTH = 50000
MIN_SMOOTH_WINDOW = 2
DEFAULT_PLOT_ROWS = 10000  # Max rows to read for plotting to avoid OOM
REQUIRED_MODEL_CONSTANTS = 318  # Model requires exactly 318 constants

# Fix ROOT path when running via symlink
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))
INPUTS_DIR = os.path.join(ROOT, 'INPUTS')
OUTPUT_CSV = os.path.join(ROOT, 'OUTPUT.csv')

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
        import subprocess
        try:
            result = subprocess.run(['wc', '-l', OUTPUT_CSV], capture_output=True, text=True, timeout=2)
            if result.returncode == 0:
                line_count = result.stdout.split()[0]
                logger.info(f"  Line count: {line_count}")
        except:
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
except:
    logger.warning("Could not get pandas version")

try:
    import plotly
    logger.info(f"plotly: {plotly.__version__}")
except:
    logger.warning("Could not get plotly version")

try:
    import shiny
    logger.info(f"shiny: {shiny.__version__}")
except:
    logger.warning("Could not get shiny version")

# Available themes list
AVAILABLE_THEMES = [
    "default",
    "cerulean",
    "cosmo",
    "cyborg",
    "darkly",
    "flatly",
    "journal",
    "litera",
    "lumen",
    "lux",
    "materia",
    "minty",
    "morph",
    "pulse",
    "quartz",
    "sandstone",
    "simplex",
    "sketchy",
    "slate",
    "solar",
    "spacelab",
    "superhero",
    "united",
    "vapor",
    "yeti",
    "zephyr"
] if THEMES_AVAILABLE else ["default"]

logger.info("=== Configuration ===")
logger.info(f"MAX_LOG_LENGTH: {MAX_LOG_LENGTH}")
logger.info(f"MIN_SMOOTH_WINDOW: {MIN_SMOOTH_WINDOW}")
logger.info(f"DEFAULT_PLOT_ROWS: {DEFAULT_PLOT_ROWS}")

logger.info("=== Theme Configuration ===")
if THEMES_AVAILABLE:
    logger.info(f"✓ Themes enabled with {len(AVAILABLE_THEMES)} available themes")
    logger.info(f"  Default theme: darkly")
else:
    logger.info("⚠ Themes not available (shinyswatch not installed)")
    logger.info("  Install with: pip install shinyswatch")

logger.info("=" * 60)
logger.info("Startup diagnostics complete. Building UI...")
logger.info("=" * 60)


def validate_constants_file(constants_filename):
    """Validate that a WCONST file has the required number of constants.
    
    Args:
        constants_filename: The filename (e.g., 'WCONST_01.txt') or full path
        
    Returns:
        tuple: (is_valid: bool, actual_count: int, error_message: str or None)
    """
    if not constants_filename:
        return True, 0, None  # No constants file specified, model uses defaults
    
    # Build full path if not already absolute
    if os.path.isabs(constants_filename):
        filepath = constants_filename
    else:
        filepath = os.path.join(INPUTS_DIR, constants_filename)
    
    if not os.path.exists(filepath):
        return False, 0, f"Constants file not found: {filepath}"
    
    try:
        # Count numbered constant lines (format: "   123   CONSTANT_NAME   value  !comment")
        # Lines start with whitespace followed by a number
        import re
        const_count = 0
        max_const_num = 0
        
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                # Match lines starting with a number (the constant index)
                match = re.match(r'^(\d+)\s+\w+', line)
                if match:
                    const_num = int(match.group(1))
                    max_const_num = max(max_const_num, const_num)
                    const_count += 1
        
        if max_const_num < REQUIRED_MODEL_CONSTANTS:
            return False, max_const_num, (
                f"Constants file '{os.path.basename(filepath)}' has only {max_const_num} constants, "
                f"but the model requires {REQUIRED_MODEL_CONSTANTS}.\n"
                f"Missing constants: {max_const_num + 1} to {REQUIRED_MODEL_CONSTANTS}\n"
                f"Try using WCONST_04.txt which has all required constants."
            )
        
        return True, max_const_num, None
        
    except Exception as e:
        return False, 0, f"Error reading constants file: {e}"


# Navigation menu choices (Settings removed - now in top bar icon)
NAV_CHOICES = {
    "nav_dashboard": "Dashboard",
    "nav_model_build": "Model Build",
    "nav_model_control": "Model Config",
    "nav_input_files": "Input Files",
    "nav_parameters": "Parameters",
    "nav_initial_conditions": "Initial Cond.",
    "nav_model_options": "Model Options",
    "nav_scenarios": "Scenarios",
    "nav_plot": "Plots",
    "nav_mass_balance": "Mass Balance",
    "nav_observations": "Observations",
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

# Input file categories and metadata for display in Input Files panel
INPUT_FILE_CATEGORIES = {
    # Timeseries files - standard format with DATA_SIZE, NUMBER_OF_VARIABLES, SCALE_FACTORS, etc.
    "TEMP_TS.txt": {
        "category": "Forcing Timeseries",
        "description": "Water temperature time series for all model boxes",
        "structure": "Standard timeseries format",
        "model_use": "Drives temperature-dependent biogeochemical processes (growth, respiration, decomposition rates)",
        "is_timeseries": True,
    },
    "SALT_TS.txt": {
        "category": "Forcing Timeseries",
        "description": "Salinity time series for all model boxes",
        "structure": "Standard timeseries format",
        "model_use": "Affects ionic strength calculations and species-specific salinity tolerances",
        "is_timeseries": True,
    },
    "FLOW_TS.txt": {
        "category": "Forcing Timeseries",
        "description": "Water flow time series between model boxes",
        "structure": "Standard timeseries format with multiple flow variables",
        "model_use": "Drives advective transport between boxes via ADVECTIVE_LINKS",
        "is_timeseries": True,
    },
    "VELOCITY_TS.txt": {
        "category": "Forcing Timeseries",
        "description": "Water velocity time series",
        "structure": "Standard timeseries format",
        "model_use": "Used for sediment resuspension and transport calculations",
        "is_timeseries": True,
    },
    "WIND_SPEED_TS.txt": {
        "category": "Meteorological Timeseries",
        "description": "Wind speed time series",
        "structure": "Standard timeseries format",
        "model_use": "Affects reaeration rates and surface mixing",
        "is_timeseries": True,
    },
    "AIR_TEMP_TS.txt": {
        "category": "Meteorological Timeseries",
        "description": "Air temperature time series",
        "structure": "Standard timeseries format",
        "model_use": "Heat budget calculations and air-water exchange",
        "is_timeseries": True,
    },
    "SOLAR_RAD_TS.txt": {
        "category": "Meteorological Timeseries",
        "description": "Solar radiation time series",
        "structure": "Standard timeseries format",
        "model_use": "Drives photosynthesis (primary production) in phytoplankton",
        "is_timeseries": True,
    },
    "REL_HUMM_TS.txt": {
        "category": "Meteorological Timeseries",
        "description": "Relative humidity time series",
        "structure": "Standard timeseries format",
        "model_use": "Evaporation and heat flux calculations",
        "is_timeseries": True,
    },
    "CLOUD_COVER_TS.txt": {
        "category": "Meteorological Timeseries",
        "description": "Cloud cover time series",
        "structure": "Standard timeseries format",
        "model_use": "Modifies solar radiation reaching water surface",
        "is_timeseries": True,
    },
    "EVAPORATION_TS.txt": {
        "category": "Meteorological Timeseries",
        "description": "Evaporation time series",
        "structure": "Standard timeseries format",
        "model_use": "Water balance and concentration effects",
        "is_timeseries": True,
    },
    "RAINFALL_TS.txt": {
        "category": "Meteorological Timeseries",
        "description": "Rainfall time series",
        "structure": "Standard timeseries format",
        "model_use": "Water balance, dilution, and atmospheric nutrient input",
        "is_timeseries": True,
    },
    "SHEAR_STRESSES_TS.txt": {
        "category": "Sediment Forcing Timeseries",
        "description": "Bottom shear stress time series for all boxes",
        "structure": "Standard timeseries format",
        "model_use": "Controls sediment resuspension rates when exceeding critical values",
        "is_timeseries": True,
    },
    "BOUNDARY_FLOW_TS.txt": {
        "category": "Boundary Conditions",
        "description": "Boundary flow time series",
        "structure": "Standard timeseries format",
        "model_use": "Specifies flow rates at open boundaries (Baltic Sea, river inputs)",
        "is_timeseries": True,
    },
    "ICE_COVER.txt": {
        "category": "Meteorological Timeseries",
        "description": "Ice cover fraction time series",
        "structure": "Standard timeseries format",
        "model_use": "Reduces gas exchange and light penetration during ice-covered periods",
        "is_timeseries": True,
    },
    "PRECIPITATION.txt": {
        "category": "Meteorological Timeseries",
        "description": "Precipitation time series",
        "structure": "Standard timeseries format",
        "model_use": "Atmospheric nutrient deposition and water balance",
        "is_timeseries": True,
    },
}

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

# Add remaining static configuration files
INPUT_FILE_CATEGORIES.update({
    "EXTRA_WCONST.txt": {
        "category": "Model Constants",
        "description": "Additional model constants beyond the standard set",
        "structure": "Parameter index | name | value | comment format",
        "model_use": "Extended parameters for special processes (allelopathy, metals, etc.)",
        "is_timeseries": False,
    },
    "EXTRA_WCONST_zero.txt": {
        "category": "Model Constants",
        "description": "Zero-initialized extra constants (inactive processes)",
        "structure": "Parameter index | name | value | comment format",
        "model_use": "Template for extra constants with all values set to zero",
        "is_timeseries": False,
    },
    "PELAGIC_INPUTS.txt": {
        "category": "Model Configuration",
        "description": "Main pelagic model configuration file",
        "structure": "Key-value pairs defining model dimensions and file references",
        "model_use": "Master configuration: # state vars, boxes, links, forcing files, etc.",
        "is_timeseries": False,
    },
    "PELAGIC_MODEL_OPTIONS.txt": {
        "category": "Model Configuration",
        "description": "Model option switches and flags",
        "structure": "Option name and integer value (0/1 for on/off)",
        "model_use": "Enables/disables model features (redox, buoyancy, allelopathy, etc.)",
        "is_timeseries": False,
    },
    "PELAGIC_OUTPUT_INFORMATION_FILE.txt": {
        "category": "Model Configuration",
        "description": "Output configuration",
        "structure": "Output specifications",
        "model_use": "Controls which variables are written to OUTPUT.csv",
        "is_timeseries": False,
    },
    "INIT_CONC_1.txt": {
        "category": "Initial Conditions",
        "description": "Initial concentrations set #1 (typically for muddy sediment boxes)",
        "structure": "State variable number | concentration | comment",
        "model_use": "Starting concentrations for all state variables in assigned boxes",
        "is_timeseries": False,
    },
    "INIT_CONC_2.txt": {
        "category": "Initial Conditions",
        "description": "Initial concentrations set #2 (typically for sandy sediment boxes)",
        "structure": "State variable number | concentration | comment",
        "model_use": "Starting concentrations for all state variables in assigned boxes",
        "is_timeseries": False,
    },
    "INIT_CONC_1_per_cube.txt": {
        "category": "Initial Conditions",
        "description": "Initial concentrations set #1 in per-cubic-meter units",
        "structure": "State variable number | concentration | comment",
        "model_use": "Alternative IC format using volumetric concentrations",
        "is_timeseries": False,
    },
    "ADVECTIVE_LINKS.txt": {
        "category": "Transport Configuration",
        "description": "Advective transport links between model boxes",
        "structure": "Link number | upstream box | downstream box | flow TS | flow variable",
        "model_use": "Defines water flow paths - positive values indicate downstream direction",
        "is_timeseries": False,
    },
    "DISPERSIVE_LINKS.txt": {
        "category": "Transport Configuration",
        "description": "Dispersive/diffusive mixing links between boxes",
        "structure": "Link number | first box | second box | mixing TS | mixing length",
        "model_use": "Defines horizontal mixing (bidirectional) between adjacent boxes",
        "is_timeseries": False,
    },
    "FLUXES_FOR_MUDDY_SEDIMENTS.txt": {
        "category": "Sediment Fluxes",
        "description": "Prescribed sediment-water fluxes for muddy sediment boxes",
        "structure": "Standard timeseries format with NH4, NO3, PO4, others, Si, O2",
        "model_use": "Nutrient release/uptake from sediments (positive = release to water)",
        "is_timeseries": True,
    },
    "FLUXES_FOR_SANDY_SEDIMENTS.txt": {
        "category": "Sediment Fluxes",
        "description": "Prescribed sediment-water fluxes for sandy sediment boxes",
        "structure": "Standard timeseries format with NH4, NO3, PO4, others, Si, O2",
        "model_use": "Lower nutrient fluxes than muddy sediments (less organic matter)",
        "is_timeseries": True,
    },
    "FLUXES_FOR_MUDDY_SEDIMENTS_HYPOXIA.txt": {
        "category": "Sediment Fluxes",
        "description": "Muddy sediment fluxes during hypoxic conditions",
        "structure": "Standard timeseries format",
        "model_use": "Enhanced nutrient release under low oxygen (P release increases)",
        "is_timeseries": True,
    },
    "FLUXES_FOR_MUDDY_SEDIMENTS_zero.txt": {
        "category": "Sediment Fluxes",
        "description": "Zero sediment fluxes template (inactive sediments)",
        "structure": "Standard timeseries format with zero values",
        "model_use": "Template for scenarios without sediment influence",
        "is_timeseries": True,
    },
    "FLUXES_FOR_SANDY_SEDIMENTS_zero.txt": {
        "category": "Sediment Fluxes",
        "description": "Zero sandy sediment fluxes template",
        "structure": "Standard timeseries format with zero values",
        "model_use": "Template for scenarios without sandy sediment influence",
        "is_timeseries": True,
    },
    "CRITICAL_SHEAR_STRESSES.txt": {
        "category": "Sediment Parameters",
        "description": "Critical shear stress thresholds per model box",
        "structure": "Box number | critical shear stress value",
        "model_use": "Erosion threshold - resuspension occurs when actual > critical",
        "is_timeseries": False,
    },
    "ALLELOPATHIC_INFORMATION.txt": {
        "category": "Process Parameters",
        "description": "Allelopathy parameters per model box",
        "structure": "Box | CYN-DIA inhibition | CYN-OPA inhibition | half-sat constants",
        "model_use": "Cyanobacteria allelopathic inhibition of diatoms and other phyto",
        "is_timeseries": False,
    },
    "W_SED_CONST.txt": {
        "category": "Sediment Parameters",
        "description": "Sediment model constants",
        "structure": "Parameter index | name | value format",
        "model_use": "Sediment diagenesis parameters (burial, diffusion, etc.)",
        "is_timeseries": False,
    },
    "BOTTOM_SEDIMENT_MODEL_INPUT.txt": {
        "category": "Sediment Configuration",
        "description": "Bottom sediment model configuration",
        "structure": "Configuration parameters",
        "model_use": "Sediment-water interaction model setup",
        "is_timeseries": False,
    },
    "RESUSPENSION_INPUTS_2.txt": {
        "category": "Sediment Parameters",
        "description": "Resuspension model inputs",
        "structure": "Resuspension parameters",
        "model_use": "Controls sediment resuspension dynamics",
        "is_timeseries": False,
    },
    "SHEAR_STRESS.txt": {
        "category": "Sediment Parameters",
        "description": "Static shear stress values",
        "structure": "Shear stress parameters per box",
        "model_use": "Alternative to time-varying shear stress",
        "is_timeseries": False,
    },
    "PRESCRIBED_SEDIMENT_FLUXES.txt": {
        "category": "Sediment Fluxes",
        "description": "Prescribed sediment nutrient fluxes",
        "structure": "Flux specifications",
        "model_use": "Direct specification of sediment-water exchange rates",
        "is_timeseries": False,
    },
    "PRESCRIBED_SEDIMENT_FLUXES_HYPOXIA.txt": {
        "category": "Sediment Fluxes",
        "description": "Prescribed sediment fluxes for hypoxic conditions",
        "structure": "Flux specifications",
        "model_use": "Enhanced fluxes under low oxygen conditions",
        "is_timeseries": False,
    },
})


def analyze_input_file(filepath):
    """Analyze an input file and return metadata including structure and timespan.

    Args:
        filepath: Full path to the input file

    Returns:
        dict with keys: category, description, structure, model_use, is_timeseries,
                       num_lines, num_columns, time_start, time_end, date_start, date_end
    """
    filename = os.path.basename(filepath)

    # Start with metadata from our catalog, or use defaults
    if filename in INPUT_FILE_CATEGORIES:
        info = INPUT_FILE_CATEGORIES[filename].copy()
    else:
        # Try to infer category from filename patterns
        info = {
            "category": "Unknown",
            "description": f"Input file: {filename}",
            "structure": "Unknown structure",
            "model_use": "Unknown purpose",
            "is_timeseries": False,
        }
        # Pattern matching for uncatalogued files
        if "_TS" in filename or filename.endswith("_TS.txt"):
            info["category"] = "Timeseries"
            info["is_timeseries"] = True
            info["structure"] = "Standard timeseries format"
        elif filename.startswith("WCONST"):
            info["category"] = "Model Constants"
            info["structure"] = "Parameter index | name | value format"
        elif filename.startswith("BATHYMETRY"):
            info["category"] = "Box Geometry"
            info["structure"] = "Layer table format"
        elif filename.startswith("INIT_CONC"):
            info["category"] = "Initial Conditions"
            info["structure"] = "State variable | concentration format"
        elif "FLUX" in filename.upper():
            info["category"] = "Sediment Fluxes"
            info["is_timeseries"] = True

    # Add file statistics
    info["num_lines"] = 0
    info["num_columns"] = 0
    info["time_start"] = None
    info["time_end"] = None
    info["date_start"] = None
    info["date_end"] = None
    info["data_size"] = None
    info["num_variables"] = None

    if not os.path.exists(filepath):
        info["error"] = "File not found"
        return info

    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()

        info["num_lines"] = len(lines)

        # Parse header for timeseries files
        data_size = None
        num_vars = None
        first_data_line = None
        last_data_line = None

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # Look for DATA_SIZE
            if "DATA_SIZE" in line_stripped.upper() or "DATA SIZE" in line_stripped.upper():
                # Next non-comment line should have the number
                for j in range(i + 1, min(i + 3, len(lines))):
                    next_line = lines[j].strip()
                    if next_line and not next_line.startswith('#'):
                        try:
                            data_size = int(next_line)
                            info["data_size"] = data_size
                        except ValueError:
                            pass
                        break

            # Look for NUMBER_OF_VARIABLES
            if "NUMBER_OF_VARIABLES" in line_stripped.upper() or "NUMBER OF VARIABLES" in line_stripped.upper():
                for j in range(i + 1, min(i + 3, len(lines))):
                    next_line = lines[j].strip()
                    if next_line and not next_line.startswith('#'):
                        try:
                            num_vars = int(next_line)
                            info["num_variables"] = num_vars
                            info["num_columns"] = num_vars + 1  # +1 for TIME column
                        except ValueError:
                            pass
                        break

            # Find first data line (starts with number, after headers)
            if first_data_line is None and line_stripped and not line_stripped.startswith('#'):
                parts = line_stripped.split()
                if len(parts) >= 2:
                    try:
                        time_val = float(parts[0])
                        # Check if this looks like a julian day (reasonable range)
                        if 0 < time_val < 100000:
                            first_data_line = (time_val, i + 1)
                    except ValueError:
                        pass

        # Find last data line by reading backwards
        for i in range(len(lines) - 1, -1, -1):
            line_stripped = lines[i].strip()
            if line_stripped and not line_stripped.startswith('#'):
                parts = line_stripped.split()
                if len(parts) >= 2:
                    try:
                        time_val = float(parts[0])
                        if 0 < time_val < 100000:
                            last_data_line = (time_val, i + 1)
                            break
                    except ValueError:
                        continue

        # Calculate time span if we found data
        if first_data_line and last_data_line:
            info["time_start"] = first_data_line[0]
            info["time_end"] = last_data_line[0]

            # Convert Julian days to dates (reference: 1997-01-01)
            from datetime import timedelta
            reference_date = date(1997, 1, 1)
            try:
                info["date_start"] = (reference_date + timedelta(days=first_data_line[0])).strftime("%Y-%m-%d")
                info["date_end"] = (reference_date + timedelta(days=last_data_line[0])).strftime("%Y-%m-%d")
            except (ValueError, OverflowError):
                pass

    except Exception as e:
        info["error"] = str(e)

    return info


def get_input_file_categories():
    """Get list of unique categories from cataloged files."""
    categories = set()
    for info in INPUT_FILE_CATEGORIES.values():
        categories.add(info.get("category", "Unknown"))
    return sorted(categories)


# Required input files for model to run
REQUIRED_INPUT_FILES = [
    # Core configuration
    ("PELAGIC_INPUTS.txt", "Main model configuration"),
    ("PELAGIC_MODEL_OPTIONS.txt", "Model options"),
    # Forcing timeseries
    ("TEMP_TS.txt", "Water temperature forcing"),
    ("FLOW_TS.txt", "Water flow forcing"),
    # Geometry and transport
    ("ADVECTIVE_LINKS.txt", "Advective transport links"),
    # Initial conditions (at least one)
    ("INIT_CONC_1.txt", "Initial concentrations"),
]

# Optional but recommended files
RECOMMENDED_INPUT_FILES = [
    ("SALT_TS.txt", "Salinity forcing"),
    ("SOLAR_RAD_TS.txt", "Solar radiation forcing"),
    ("WIND_SPEED_TS.txt", "Wind speed forcing"),
]


def validate_required_inputs():
    """Validate that all required input files exist and have valid data.

    Returns:
        tuple: (is_valid: bool, errors: list, warnings: list)
    """
    errors = []
    warnings = []

    # Check required files
    for filename, description in REQUIRED_INPUT_FILES:
        filepath = os.path.join(INPUTS_DIR, filename)
        if not os.path.exists(filepath):
            errors.append(f"Missing required file: {filename} ({description})")
        else:
            # Check if file has content
            try:
                size = os.path.getsize(filepath)
                if size < 10:  # Essentially empty
                    errors.append(f"File appears empty: {filename}")
            except Exception as e:
                errors.append(f"Cannot read {filename}: {e}")

    # Check recommended files
    for filename, description in RECOMMENDED_INPUT_FILES:
        filepath = os.path.join(INPUTS_DIR, filename)
        if not os.path.exists(filepath):
            warnings.append(f"Missing recommended file: {filename} ({description})")

    # Check at least one bathymetry file exists
    bathy_exists = any(
        os.path.exists(os.path.join(INPUTS_DIR, f"BATHYMETRY_{i}.txt"))
        for i in range(1, 26)
    )
    if not bathy_exists:
        errors.append("No bathymetry files found (BATHYMETRY_*.txt)")

    # Check forcing timeseries have valid timespan
    forcing_files = ["TEMP_TS.txt", "FLOW_TS.txt", "SALT_TS.txt"]
    for filename in forcing_files:
        filepath = os.path.join(INPUTS_DIR, filename)
        if os.path.exists(filepath):
            analysis = analyze_input_file(filepath)
            if analysis.get("is_timeseries"):
                if analysis.get("time_start") is None or analysis.get("time_end") is None:
                    warnings.append(f"Cannot determine timespan for {filename}")
                elif analysis.get("data_size") and analysis["data_size"] < 2:
                    errors.append(f"Insufficient data in {filename} (need at least 2 timesteps)")

    is_valid = len(errors) == 0
    return is_valid, errors, warnings


def get_available_boxes():
    """Get list of available model boxes based on bathymetry files."""
    boxes = []
    for i in range(1, 26):
        filepath = os.path.join(INPUTS_DIR, f"BATHYMETRY_{i}.txt")
        if os.path.exists(filepath):
            boxes.append(i)
    return boxes


def get_timeseries_variables(filename):
    """Extract variable names from a timeseries file header.

    Returns:
        list of variable names (e.g., ['TEMP 1', 'TEMP 2', ...])
    """
    filepath = os.path.join(INPUTS_DIR, filename)
    if not os.path.exists(filepath):
        return []

    variables = []
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            for line in f:
                line = line.strip()
                # Look for header line with variable names (after # and before data)
                if line.startswith('#') and 'TIME' in line.upper():
                    # Parse variable names from header
                    parts = line[1:].split()  # Remove # and split
                    # Skip 'TIME' and get variable names
                    if 'TIME' in parts:
                        idx = parts.index('TIME')
                        variables = parts[idx+1:]
                    break
    except Exception:
        pass

    return variables


# Function to get theme CSS content
def get_theme_css(theme_name):
    """Get CSS content for a shinyswatch theme"""
    if not THEMES_AVAILABLE:
        return ""
    try:
        theme_obj = getattr(shinyswatch.theme, theme_name, None)
        if theme_obj:
            return theme_obj.to_css()
    except Exception as e:
        logger.error(f"Error getting theme CSS: {e}")
    return ""

# UI function with dynamic theme support
def create_ui(theme_name="darkly"):
    """Create UI - theme is applied dynamically via CSS, not at creation time"""
    # JavaScript to handle page reload
    reload_js = ui.tags.script("""
        Shiny.addCustomMessageHandler('reload_page', function(message) {
            console.log('Reloading page:', message);
            setTimeout(function() {
                window.location.reload();
            }, 500);
        });
    """)

    # Custom CSS for pill-style radio buttons
    nav_css = ui.tags.style("""
        /* Remove gap at top of sidebar */
        .sidebar-content {
            padding-top: 0.25rem !important;
        }
        .nav-radio-pills {
            margin-top: 0;
            padding-top: 0;
        }
        .nav-radio-pills .shiny-options-group {
            display: flex;
            flex-direction: column;
            gap: 1px;
            margin-top: 0;
        }
        .nav-radio-pills .radio {
            margin: 0;
        }
        .nav-radio-pills .radio label {
            display: block;
            padding: 7px 12px;
            margin: 0;
            border-radius: 5px;
            cursor: pointer !important;
            transition: background-color 0.15s ease;
            user-select: none;
            font-size: 0.9rem;
        }
        .nav-radio-pills .radio label span {
            cursor: pointer !important;
        }
        .nav-radio-pills .radio label:hover {
            background-color: rgba(var(--bs-primary-rgb), 0.1);
        }
        .nav-radio-pills .radio input[type="radio"] {
            display: none;
        }
        .nav-radio-pills .radio input[type="radio"]:checked + span {
            color: white;
            display: block;
            background-color: var(--bs-primary);
            margin: -7px -12px;
            padding: 7px 12px;
            border-radius: 5px;
            font-weight: 600;
            cursor: pointer !important;
        }
    """)

    # === SIDEBAR: Navigation Menu + Run Log ===
    sidebar_content = ui.sidebar(
        nav_css,
        ui.tags.div(
            ui.input_radio_buttons(
                "navigation",
                None,
                choices=NAV_CHOICES,
                selected="nav_dashboard"
            ),
            class_="nav-radio-pills"
        ),
        ui.tags.hr(),
        ui.h6("Run Log"),
        ui.output_text_verbatim("run_log", placeholder=True),
        width=250,
        open="desktop"
    )

    # === MAIN CONTENT PANELS ===
    # Dashboard Panel
    panel_dashboard = ui.panel_conditional(
        "input.navigation === 'nav_dashboard'",
        ui.card(
            ui.card_header("Dashboard"),
            ui.layout_columns(
                ui.card(
                    ui.card_header("System Status"),
                    ui.output_text_verbatim("status_info"),
                    fill=False
                ),
                ui.card(
                    ui.card_header("Quick Actions"),
                    ui.input_action_button("quick_run", "Quick Run", class_="btn-success w-100 mb-2"),
                    ui.input_action_button("quick_plot", "Refresh Plot", class_="btn-info w-100"),
                    fill=False
                ),
                col_widths=[6, 6]
            )
        )
    )

    # Model Build Panel - Compilation and Executable Management
    panel_model_build = ui.panel_conditional(
        "input.navigation === 'nav_model_build'",
        ui.layout_columns(
            # Left column: Build Configuration
            ui.card(
                ui.card_header("Build Configuration"),

                # Compiler selection
                ui.h6("Compiler"),
                ui.input_radio_buttons(
                    "build_compiler",
                    None,
                    choices={k: v["name"] for k, v in COMPILERS.items()},
                    selected="gfortran"
                ),
                ui.output_ui("compiler_status"),

                ui.tags.hr(),

                # Build type selection
                ui.h6("Build Type"),
                ui.input_radio_buttons(
                    "build_type",
                    None,
                    choices={k: v["name"] for k, v in BUILD_TYPES.items()},
                    selected="release"
                ),
                ui.output_text("build_flags_info"),

                ui.tags.hr(),

                # Build options
                ui.h6("Build Options"),
                ui.input_switch("build_clean_first", "Clean before build", value=False),
                ui.tags.small("Enable when switching compilers or build types", class_="text-muted"),

                ui.tags.hr(),

                # Target executable name
                ui.h6("Target Executable"),
                ui.output_ui("target_exe_name"),

                ui.tags.hr(),

                # Build actions
                ui.layout_columns(
                    ui.input_action_button("btn_build", "Build", class_="btn-primary w-100"),
                    ui.input_action_button("btn_rebuild", "Rebuild All", class_="btn-warning w-100"),
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
    )

    # Model Control Panel (combined with Simulation Config)
    panel_model_control = ui.panel_conditional(
        "input.navigation === 'nav_model_control'",
        ui.navset_card_tab(
            # Tab 1: Simulation Configuration
            ui.nav_panel(
                "Simulation Config",
                ui.input_action_button("load_sim_config", "Load Configuration", class_="btn-secondary mb-3"),
                ui.layout_columns(
                    ui.card(
                        ui.card_header("Time Period"),
                        ui.input_numeric("sim_base_year", "Base Year:", value=1998, min=1900, max=2100),
                        ui.input_date("sim_start_date", "Start Date:", value="2015-01-01"),
                        ui.input_date("sim_end_date", "End Date:", value="2016-01-01"),
                        ui.output_text("sim_duration_info"),
                        fill=False
                    ),
                    ui.card(
                        ui.card_header("Time Stepping"),
                        ui.input_select(
                            "sim_timestep_preset",
                            "Time Step:",
                            choices=list(TIME_STEP_PRESETS.keys()),
                            selected="6 minutes"
                        ),
                        ui.input_numeric("sim_timesteps_per_day", "Steps/Day:", value=240, min=1, max=1440),
                        ui.output_text("sim_timestep_info"),
                        fill=False
                    ),
                    ui.card(
                        ui.card_header("Output Interval"),
                        ui.input_select(
                            "sim_output_preset",
                            "Output Frequency:",
                            choices=list(OUTPUT_INTERVAL_PRESETS.keys()),
                            selected="Hourly"
                        ),
                        ui.input_numeric("sim_print_interval", "Print Interval (steps):", value=24, min=1),
                        ui.output_text("sim_output_info"),
                        fill=False
                    ),
                    col_widths=[4, 4, 4]
                ),
                ui.tags.hr(),
                ui.layout_columns(
                    ui.card(
                        ui.card_header("Model Options"),
                        ui.input_switch("sim_model_sediments", "Enable Sediment Model", value=True),
                        ui.input_select(
                            "sim_resuspension",
                            "Resuspension Option:",
                            choices={"0": "Disabled", "1": "Fully Prescribed", "2": "Semi-Prescribed"},
                            selected="2"
                        ),
                        fill=False
                    ),
                    col_widths=[6]
                ),
                ui.layout_columns(
                    ui.input_action_button("save_sim_config", "Save Configuration", class_="btn-success"),
                    ui.output_text("sim_config_save_status"),
                    col_widths=[3, 9]
                )
            ),
            # Tab 2: Run Model
            ui.nav_panel(
                "Run Model",
                ui.layout_columns(
                    # Left column: Run Parameters
                    ui.card(
                        ui.card_header("Run Parameters"),

                        # Executable selection
                        ui.h6("Executable"),
                        ui.input_select(
                            "run_executable",
                            None,
                            choices=["ESTAS_II"],
                            selected="ESTAS_II"
                        ),
                        ui.output_ui("run_executable_info"),

                        ui.tags.hr(),

                        # Input file selection (Arg 1 - required)
                        ui.h6("Command Line Arguments"),
                        ui.tooltip(
                            ui.input_select(
                                "cmd_input_file",
                                "Input Configuration File:",
                                choices={"INPUT.txt": "INPUT.txt (default)"},
                                selected="INPUT.txt"
                            ),
                            "Main model configuration file (INPUT.txt). Contains simulation time, folders, and model options."
                        ),

                        # Constants file (Arg 2 - optional)
                        ui.tooltip(
                            ui.input_select(
                                "cmd_constants_file",
                                "Pelagic Constants File:",
                                choices={"": "(use model defaults)"},
                                selected=""
                            ),
                            "Override model constants. Leave empty to use compiled defaults."
                        ),

                        # Binary output (Arg 3)
                        ui.tooltip(
                            ui.input_switch(
                                "cmd_binary_enabled",
                                "Enable Binary Output",
                                value=False
                            ),
                            "Write binary output file for faster I/O."
                        ),
                        ui.panel_conditional(
                            "input.cmd_binary_enabled",
                            ui.input_text(
                                "cmd_binary_filename",
                                "Binary Filename:",
                                value="PELAGIC_OUTPUT.bin"
                            ),
                        ),

                        ui.tags.hr(),

                        # Command preview
                        ui.h6("Command Preview"),
                        ui.output_text_verbatim("cmd_preview", placeholder=True),
                        ui.output_ui("constants_validation_status"),

                        fill=False
                    ),

                    # Right column: Execution
                    ui.card(
                        ui.card_header("Execution"),

                        ui.layout_columns(
                            ui.input_action_button("run", "Run Model", class_="btn-success btn-lg w-100"),
                            ui.input_action_button("stop_run", "Stop", class_="btn-danger btn-lg w-100"),
                            col_widths=[8, 4]
                        ),
                        ui.output_ui("run_status_indicator"),

                        ui.tags.hr(),

                        ui.h6("Build Options"),
                        ui.tags.p("To compile the model with different settings, use the Model Build panel.", class_="text-muted"),
                        ui.input_action_button("goto_build", "Go to Model Build", class_="btn-outline-primary w-100"),

                        ui.tags.hr(),

                        ui.card_header("Run Log"),
                        ui.output_text_verbatim("run_log_mini", placeholder=True),

                        fill=False
                    ),
                    col_widths=[6, 6]
                )
            ),
            id="model_control_tabs"
        )
    )

    # Input Files Panel - Enhanced with file analysis
    panel_input_files = ui.panel_conditional(
        "input.navigation === 'nav_input_files'",
        ui.layout_columns(
            # Left column: File selection and info
            ui.card(
                ui.card_header("File Browser"),
                ui.layout_columns(
                    ui.input_select(
                        "file_category_filter",
                        "Filter by category:",
                        choices=["All Categories"] + get_input_file_categories(),
                        selected="All Categories"
                    ),
                    ui.input_action_button("refresh_files", "Refresh", class_="btn-sm btn-secondary mt-4"),
                    col_widths=[10, 2]
                ),
                ui.input_select("file_select", "Select file:", choices=[], size=12),
                ui.tags.hr(),
                ui.card(
                    ui.card_header("File Information"),
                    ui.output_ui("file_info_panel"),
                    style="max-height: 350px; overflow-y: auto;"
                ),
            ),
            # Right column: File contents
            ui.card(
                ui.card_header(ui.output_text("file_header_text")),
                ui.input_text_area("file_contents", "File contents:", value="", rows=25, width="100%"),
                ui.layout_columns(
                    ui.input_action_button("save_file", "Save File", class_="btn-warning"),
                    ui.output_text("save_status"),
                    col_widths=[3, 9]
                ),
                ui.tags.small("Timestamped backups are created automatically", class_="text-muted")
            ),
            col_widths=[4, 8]
        )
    )

    # Parameters Panel
    panel_parameters = ui.panel_conditional(
        "input.navigation === 'nav_parameters'",
        ui.card(
            ui.card_header("Parameters"),
            ui.layout_columns(
                ui.input_select(
                    "param_category",
                    "Category:",
                    choices=list(PARAMETER_CATEGORIES.keys()),
                    selected="Diatoms"
                ),
                ui.input_select(
                    "param_file",
                    "Constants file:",
                    choices=["WCONST_04.txt"],
                    selected="WCONST_04.txt"
                ),
                ui.input_action_button("load_params", "Load", class_="btn-secondary mt-4"),
                col_widths=[5, 5, 2]
            ),
            ui.tags.hr(),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Category Info"),
                    ui.output_text("param_category_info"),
                    fill=False
                ),
                ui.card(
                    ui.card_header("Parameters"),
                    ui.output_ui("param_table"),
                    style="max-height: 500px; overflow-y: auto;"
                ),
                col_widths=[4, 8]
            ),
            ui.layout_columns(
                ui.input_action_button("save_params", "Save All Changes", class_="btn-success"),
                ui.output_text("param_save_status"),
                col_widths=[3, 9]
            )
        )
    )

    # Initial Conditions Panel
    panel_initial_conditions = ui.panel_conditional(
        "input.navigation === 'nav_initial_conditions'",
        ui.card(
            ui.card_header("Initial Conditions"),
            ui.layout_columns(
                ui.input_select(
                    "ic_file",
                    "IC File:",
                    choices=["INIT_CONC_1.txt", "INIT_CONC_2.txt"],
                    selected="INIT_CONC_1.txt"
                ),
                ui.input_select(
                    "ic_category",
                    "Category:",
                    choices=list(STATE_VARIABLE_CATEGORIES.keys()),
                    selected="Nutrients"
                ),
                ui.input_action_button("load_ics", "Load", class_="btn-secondary mt-4"),
                col_widths=[5, 5, 2]
            ),
            ui.tags.hr(),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Category Info"),
                    ui.output_text("ic_category_info"),
                    fill=False
                ),
                ui.card(
                    ui.card_header("State Variables"),
                    ui.output_ui("ic_table"),
                    style="max-height: 500px; overflow-y: auto;"
                ),
                col_widths=[4, 8]
            ),
            ui.layout_columns(
                ui.input_action_button("save_ics", "Save All Changes", class_="btn-success"),
                ui.output_text("ic_save_status"),
                col_widths=[3, 9]
            )
        )
    )

    # Model Options Panel
    panel_model_options = ui.panel_conditional(
        "input.navigation === 'nav_model_options'",
        ui.card(
            ui.card_header("Model Options"),
            ui.layout_columns(
                ui.input_select(
                    "options_category",
                    "Category:",
                    choices=list(OPTION_CATEGORIES.keys()),
                    selected="Cyanobacteria"
                ),
                ui.input_action_button("load_options", "Load Options", class_="btn-secondary mt-4"),
                col_widths=[10, 2]
            ),
            ui.tags.hr(),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Model Switches"),
                    ui.output_ui("options_switches"),
                    style="max-height: 400px; overflow-y: auto;"
                ),
                ui.card(
                    ui.card_header("Extra Constants"),
                    ui.output_ui("options_constants"),
                    style="max-height: 400px; overflow-y: auto;"
                ),
                col_widths=[6, 6]
            ),
            ui.layout_columns(
                ui.input_action_button("save_options", "Save All Changes", class_="btn-success"),
                ui.output_text("options_save_status"),
                col_widths=[3, 9]
            )
        )
    )

    # Simulation Config Panel - now integrated into Model Control
    # (kept as empty conditional for backward compatibility with any references)
    panel_sim_config = ui.panel_conditional(
        "input.navigation === 'nav_sim_config_disabled'",
        ui.tags.div()  # Empty placeholder
    )

    # Scenarios Panel
    panel_scenarios = ui.panel_conditional(
        "input.navigation === 'nav_scenarios'",
        ui.card(
            ui.card_header("Scenario Presets"),
            ui.layout_columns(
                # Load Scenario Section
                ui.card(
                    ui.card_header("Load Scenario"),
                    ui.input_select(
                        "scenario_select",
                        "Select Scenario:",
                        choices=[],
                        selected=None
                    ),
                    ui.layout_columns(
                        ui.input_action_button("load_scenario", "Load", class_="btn-primary"),
                        ui.input_action_button("delete_scenario", "Delete", class_="btn-danger"),
                        ui.input_action_button("refresh_scenarios", "Refresh", class_="btn-secondary"),
                        col_widths=[4, 4, 4]
                    ),
                    ui.tags.hr(),
                    ui.output_ui("scenario_info"),
                    fill=False
                ),
                # Save Scenario Section
                ui.card(
                    ui.card_header("Save Current Configuration"),
                    ui.input_text("new_scenario_name", "Name:", placeholder="Enter scenario name"),
                    ui.input_text_area(
                        "new_scenario_desc",
                        "Description:",
                        placeholder="Brief description of this scenario",
                        rows=2
                    ),
                    ui.tags.p("Include:", class_="fw-bold mt-2 mb-1"),
                    ui.input_checkbox("scenario_include_params", "Parameters (WCONST_04.txt)", value=True),
                    ui.layout_columns(
                        ui.input_checkbox("scenario_include_ics", "Initial Conditions:", value=True),
                        ui.input_select(
                            "save_ic_file",
                            "",
                            choices=["INIT_CONC_1.txt", "INIT_CONC_2.txt"],
                            selected="INIT_CONC_1.txt"
                        ),
                        col_widths=[6, 6]
                    ),
                    ui.input_checkbox("scenario_include_options", "Model Options & Constants", value=True),
                    ui.input_action_button("save_scenario", "Save as New Scenario", class_="btn-success mt-2"),
                    fill=False
                ),
                col_widths=[6, 6]
            ),
            ui.tags.hr(),
            ui.output_text("scenario_status")
        )
    )

    # Plot & Visualization Panel
    panel_plot = ui.panel_conditional(
        "input.navigation === 'nav_plot'",
        ui.card(
            ui.card_header("Plot & Visualization"),
            ui.navset_card_tab(
                # Tab 1: Model Output
                ui.nav_panel(
                    "Model Output",
                    ui.layout_columns(
                        ui.card(
                            ui.card_header("Variables"),
                            ui.input_selectize("left_vars", "Left axis:", choices=[], multiple=True),
                            ui.input_selectize("right_vars", "Right axis:", choices=[], multiple=True),
                            ui.input_checkbox("log_left", "Log scale left"),
                            ui.input_checkbox("log_right", "Log scale right"),
                            fill=False
                        ),
                        ui.card(
                            ui.card_header("Options"),
                            ui.input_checkbox("smooth", "Apply rolling mean"),
                            ui.input_slider("smooth_window", "Window size:", min=MIN_SMOOTH_WINDOW, max=101, value=5, step=1),
                            ui.input_slider("nrows", "Preview rows:", min=10, max=1000, value=200, step=10),
                            ui.input_action_button("refresh_plot", "Refresh Plot", class_="btn-info w-100"),
                            fill=False
                        ),
                        col_widths=[6, 6]
                    ),
                    ui.tags.hr(),
                    ui.div(
                        output_widget("main_plot"),
                        style="min-height: 400px;"
                    )
                ),
                # Tab 2: Input Timeseries
                ui.nav_panel(
                    "Input Timeseries",
                    ui.layout_columns(
                        ui.card(
                            ui.card_header("Select Data"),
                            ui.input_select(
                                "input_ts_file",
                                "Timeseries file:",
                                choices={
                                    "TEMP_TS.txt": "Temperature",
                                    "SALT_TS.txt": "Salinity",
                                    "FLOW_TS.txt": "Flow",
                                    "SOLAR_RAD_TS.txt": "Solar Radiation",
                                    "WIND_SPEED_TS.txt": "Wind Speed",
                                    "AIR_TEMP_TS.txt": "Air Temperature",
                                    "SHEAR_STRESSES_TS.txt": "Shear Stress",
                                }
                            ),
                            ui.input_selectize(
                                "input_ts_boxes",
                                "Select boxes:",
                                choices=[],
                                multiple=True
                            ),
                            ui.input_action_button("plot_input_ts", "Plot Timeseries", class_="btn-info w-100 mt-2"),
                            fill=False
                        ),
                        ui.card(
                            ui.card_header("Time Range"),
                            ui.output_text("input_ts_info"),
                            ui.input_checkbox("input_ts_subset", "Limit time range"),
                            ui.output_ui("input_ts_date_range"),
                            fill=False
                        ),
                        col_widths=[6, 6]
                    ),
                    ui.tags.hr(),
                    ui.div(
                        output_widget("input_ts_plot"),
                        style="min-height: 400px;"
                    )
                ),
                # Tab 3: Output Preview (table)
                ui.nav_panel(
                    "Data Preview",
                    ui.div(
                        ui.output_table("out_preview"),
                        style="max-height: 500px; overflow-y: auto;"
                    )
                ),
                id="plot_tabs"
            )
        )
    )

    # Mass Balance Panel
    panel_mass_balance = ui.panel_conditional(
        "input.navigation === 'nav_mass_balance'",
        ui.card(
            ui.card_header("Mass Balance"),
            ui.input_action_button("calc_mass_balance", "Calculate Mass Balance", class_="btn-primary mb-3"),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Summary"),
                    ui.output_table("mass_balance_summary"),
                ),
                ui.card(
                    ui.card_header("Element Details"),
                    ui.input_select(
                        "mb_element",
                        "Element:",
                        choices=["Nitrogen", "Carbon", "Phosphorus", "Silicon"],
                        selected="Nitrogen"
                    ),
                    ui.output_ui("mass_balance_details"),
                ),
                col_widths=[6, 6]
            ),
            ui.card(
                ui.card_header("Time Series"),
                ui.output_ui("mass_balance_plot_ui"),
            )
        )
    )

    # Observations Panel
    panel_observations = ui.panel_conditional(
        "input.navigation === 'nav_observations'",
        ui.card(
            ui.card_header("Model Validation - Observations"),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Load Observations"),
                    ui.input_file("obs_file", "Upload observation CSV:", accept=[".csv"], multiple=False),
                    ui.tags.small("CSV with TIME column + variable columns", class_="text-muted d-block mb-2"),
                    ui.input_action_button("generate_sample_obs", "Generate Sample Data", class_="btn-outline-secondary btn-sm w-100"),
                    fill=False
                ),
                ui.card(
                    ui.card_header("Comparison Summary"),
                    ui.output_table("obs_comparison_summary"),
                ),
                col_widths=[4, 8]
            ),
            ui.layout_columns(
                ui.card(
                    ui.card_header("Variable Details"),
                    ui.input_select("obs_variable", "Variable:", choices=[], selected=None),
                    ui.output_ui("obs_metrics_detail"),
                ),
                ui.card(
                    ui.card_header("Scatter Plot Info"),
                    ui.output_ui("obs_scatter_info"),
                ),
                col_widths=[6, 6]
            )
        )
    )

    # === COMBINE ALL PANELS ===
    main_content = ui.div(
        panel_dashboard,
        panel_model_build,
        panel_model_control,
        panel_input_files,
        panel_parameters,
        panel_initial_conditions,
        panel_model_options,
        panel_sim_config,
        panel_scenarios,
        panel_plot,
        panel_mass_balance,
        panel_observations,
    )

    # Custom top bar with settings icon
    top_bar = ui.tags.div(
        ui.tags.div(
            # Settings gear icon button (right side)
            ui.input_action_button(
                "settings_toggle",
                ui.tags.i(class_="bi bi-gear-fill"),
                class_="btn btn-link text-light p-1",
                title="Settings"
            ),
            class_="d-flex justify-content-end align-items-center px-3 py-2"
        ),
        class_="bg-dark",
        style="position: sticky; top: 0; z-index: 1030;"
    )

    # Bootstrap Icons CSS
    bootstrap_icons_css = ui.tags.link(
        rel="stylesheet",
        href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
    )

    # Settings modal/offcanvas
    settings_offcanvas = ui.tags.div(
        ui.tags.div(
            ui.tags.div(
                ui.tags.h5("Settings", class_="offcanvas-title"),
                ui.tags.button(
                    type="button",
                    class_="btn-close btn-close-white",
                    **{"data-bs-dismiss": "offcanvas", "aria-label": "Close"}
                ),
                class_="offcanvas-header bg-dark text-light"
            ),
            ui.tags.div(
                ui.layout_columns(
                    ui.card(
                        ui.card_header("Appearance"),
                        ui.input_select(
                            "theme_select",
                            "Theme:",
                            choices=AVAILABLE_THEMES,
                            selected=theme_name
                        ) if THEMES_AVAILABLE else ui.tags.div("Themes not available"),
                        ui.input_action_button("apply_theme", "Apply Theme", class_="btn-info w-100 mt-2") if THEMES_AVAILABLE else None,
                        ui.output_text("theme_status"),
                        fill=False
                    ),
                    ui.card(
                        ui.card_header("About"),
                        ui.tags.h5("AQUABC v0.2"),
                        ui.tags.p("Aquatic Biogeochemical Model"),
                        ui.tags.p("A sophisticated water quality simulation tool with:"),
                        ui.tags.ul(
                            ui.tags.li("318 calibratable parameters"),
                            ui.tags.li("36 state variables"),
                            ui.tags.li("Complex biogeochemical processes"),
                        ),
                        fill=False
                    ),
                    col_widths=[12, 12]
                ),
                class_="offcanvas-body"
            ),
            class_="offcanvas offcanvas-end",
            tabindex="-1",
            id="settingsOffcanvas",
            **{"aria-labelledby": "settingsOffcanvasLabel"}
        )
    )

    # JavaScript to toggle settings offcanvas
    settings_js = ui.tags.script("""
        $(document).on('click', '#settings_toggle', function() {
            var offcanvas = new bootstrap.Offcanvas(document.getElementById('settingsOffcanvas'));
            offcanvas.toggle();
        });
    """)

    # Build final layout - use dynamic theming via output_ui instead of static theme
    content = [
        bootstrap_icons_css,
        reload_js if THEMES_AVAILABLE else None,
        # Dynamic theme CSS placeholder - will be filled by server
        ui.output_ui("dynamic_theme_css"),
        top_bar,
        settings_offcanvas,
        settings_js,
        ui.layout_sidebar(
            sidebar_content,
            main_content
        )
    ]

    # Return page WITHOUT static theme - theme is applied dynamically
    return ui.page_fluid(*content, title="AQUABC")

# Load saved theme preference
def get_saved_theme():
    """Load theme preference from file"""
    theme_file = os.path.join(ROOT, '.aquabc_theme')
    try:
        if os.path.exists(theme_file):
            with open(theme_file, 'r') as f:
                theme = f.read().strip()
                if theme in AVAILABLE_THEMES:
                    logger.info(f"Loaded saved theme preference: {theme}")
                    return theme
                else:
                    logger.warning(f"Saved theme '{theme}' not in available themes, using default")
    except Exception as e:
        logger.error(f"Error loading theme preference: {e}")
    return "darkly"  # Default theme

# Create UI with saved or default theme
saved_theme = get_saved_theme()
app_ui = create_ui(saved_theme)

# Server

def server(input, output, session):
    logger.info("=" * 60)
    logger.info("Server function initializing...")
    logger.info(f"Session ID: {session.id if hasattr(session, 'id') else 'N/A'}")
    logger.info("=" * 60)

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
            with open(OUTPUT_CSV, 'r') as f:
                lines = sum(1 for _ in f)
            status_lines.append(f"Output Rows: {lines}")
        else:
            status_lines.append("Last Run: Never")

        # Count input files
        input_files = len([f for f in os.listdir(INPUTS_DIR) if os.path.isfile(os.path.join(INPUTS_DIR, f))])
        status_lines.append(f"Input Files: {input_files}")

        return "\n".join(status_lines)

    # Mini run log for Model Control panel
    @render.text
    def run_log_mini():
        """Abbreviated run log for sidebar"""
        # Poll every 500ms to catch updates from background threads
        reactive.invalidate_later(0.5)
        # Return last 10 lines from shared list
        return ''.join(_log_lines[-10:])

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
        """Build the model command based on current widget values.

        Command line format:
        - 0 args: uses INPUT.txt by default
        - 1 arg:  INPUT_FILE
        - 2 args: INPUT_FILE + PELAGIC_CONSTANTS_FILE (enables constants override)
        - 3 args: INPUT_FILE + CONSTANTS + BINARY_OUTPUT (enables binary output)
        """
        # Get selected executable - handle case where input isn't ready yet
        exe_name = "ESTAS_II"
        try:
            selected = input.run_executable()
            if selected:
                exe_name = selected
        except Exception:
            pass
        cmd = [f"./{exe_name}"]
        
        # Arg 1: Input file (required)
        input_file = input.cmd_input_file() or "INPUT.txt"
        cmd.append(input_file)
        
        # Get all optional args
        const_file = input.cmd_constants_file() or ""
        
        # Binary file only used if switch is enabled
        binary_enabled = input.cmd_binary_enabled() if hasattr(input, 'cmd_binary_enabled') else False
        binary_file = ""
        if binary_enabled:
            binary_file = input.cmd_binary_filename() if hasattr(input, 'cmd_binary_filename') else ""
            if not binary_file:
                binary_file = "PELAGIC_OUTPUT.bin"  # Default if switch on but name empty
        
        shear_file = input.cmd_shear_stress_file() if hasattr(input, 'cmd_shear_stress_file') else ""
        
        # If binary or shear file is set, we need a constants file
        if (binary_file or shear_file) and not const_file:
            const_file = DEFAULT_CONSTANTS_FILE  # Use default
        
        # Arg 2: Constants file
        if not const_file:
            return cmd  # No more args
        cmd.append(const_file)
        
        # If shear file is set but no binary file, we need a placeholder binary file
        if shear_file and not binary_file:
            binary_file = "PELAGIC_OUTPUT.bin"  # Default binary output
        
        # Arg 3: Binary output file
        if not binary_file:
            return cmd  # No more args
        cmd.append(binary_file)
        
        # Arg 4: Shear stress file (optional)
        if shear_file:
            cmd.append(shear_file)
        
        return cmd

    @render.text
    def cmd_preview():
        """Show preview of the command that will be executed"""
        cmd = build_estas_command()
        return " ".join(cmd)

    # =========================================================================
    # Model Build Panel - Server Logic
    # =========================================================================

    # Build log storage (separate from run log)
    _build_log_lines = reactive.value([])

    def get_available_executables():
        """Scan for available executable files"""
        executables = []
        # Check for known executables in the project root
        exe_patterns = ["ESTAS_II", "AQUABC*"]
        for pattern in exe_patterns:
            import glob
            for f in glob.glob(os.path.join(ROOT, pattern)):
                if os.path.isfile(f) and os.access(f, os.X_OK):
                    executables.append(os.path.basename(f))
        # Also check for any pre-built executables
        for f in ["AQUABC02GFREL", "AQUABC02INTL"]:
            path = os.path.join(ROOT, f)
            if os.path.isfile(path) and os.access(path, os.X_OK):
                if f not in executables:
                    executables.append(f)
        return sorted(set(executables))

    def get_executable_info(exe_name):
        """Get information about an executable"""
        exe_path = os.path.join(ROOT, exe_name)
        if not os.path.exists(exe_path):
            return {"exists": False}

        import subprocess
        info = {
            "exists": True,
            "path": exe_path,
            "size": os.path.getsize(exe_path),
            "modified": datetime.fromtimestamp(os.path.getmtime(exe_path)).strftime('%Y-%m-%d %H:%M:%S'),
        }

        # Check if stripped (no debug symbols)
        try:
            result = subprocess.run(["file", exe_path], capture_output=True, text=True, timeout=5)
            info["file_type"] = result.stdout.strip()
            info["stripped"] = "stripped" in result.stdout.lower()
            info["has_debug"] = "not stripped" in result.stdout.lower()
        except:
            info["file_type"] = "Unknown"
            info["stripped"] = None

        return info

    @render.ui
    def compiler_status():
        """Check if selected compiler is available"""
        compiler = input.build_compiler()
        compiler_info = COMPILERS.get(compiler, {})
        cmd = compiler_info.get("command", compiler)

        try:
            import subprocess
            result = subprocess.run(["which", cmd], capture_output=True, text=True, timeout=5)
            if result.returncode == 0:
                # Get version
                try:
                    ver_result = subprocess.run([cmd, "--version"], capture_output=True, text=True, timeout=5)
                    version = ver_result.stdout.split('\n')[0] if ver_result.returncode == 0 else "Unknown version"
                except:
                    version = "Version check failed"
                return ui.div(
                    ui.tags.small(f"✓ {cmd} available", class_="text-success"),
                    ui.tags.br(),
                    ui.tags.small(version, class_="text-muted")
                )
            else:
                return ui.div(
                    ui.tags.small(f"✗ {cmd} not found in PATH", class_="text-danger"),
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
        """Generate executable name based on compiler and build type"""
        try:
            compiler = input.build_compiler()
            build_type = input.build_type()
        except:
            return "ESTAS_II_gf_release"

        # Short compiler name
        fc_short = {
            "gfortran": "gf",
            "ifort": "ifort",
            "ifx": "ifx"
        }.get(compiler, compiler)

        return f"ESTAS_II_{fc_short}_{build_type}"

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
        executables = get_available_executables()
        if not executables:
            return ui.div(ui.tags.em("No executables found. Build the model first.", class_="text-muted"))

        items = []
        for exe in executables:
            info = get_executable_info(exe)
            if info["exists"]:
                badge_class = "bg-success" if info.get("has_debug") else "bg-secondary"
                badge_text = "debug" if info.get("has_debug") else "release"
                items.append(
                    ui.div(
                        ui.tags.span(exe, class_="fw-bold"),
                        ui.tags.span(badge_text, class_=f"badge {badge_class} ms-2"),
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

        debug_info = "with debug symbols" if info.get("has_debug") else "release build"
        return ui.div(
            ui.tags.small(f"✓ {debug_info}, {info['size'] / 1024:.1f} KB", class_="text-success")
        )

    @render.text
    def build_log():
        """Render the build log"""
        lines = _build_log_lines.get()
        if not lines:
            return "Build log will appear here when you start a build..."
        return "".join(lines[-200:])  # Last 200 lines

    @reactive.effect
    @reactive.event(input.btn_clear_build_log)
    def clear_build_log():
        """Clear the build log"""
        _build_log_lines.set([])

    @reactive.effect
    @reactive.event(input.btn_refresh_executables)
    def refresh_executables():
        """Refresh the executable list"""
        executables = get_available_executables()
        choices = {e: e for e in executables} if executables else {"ESTAS_II": "ESTAS_II"}
        ui.update_select("active_executable", choices=choices)
        ui.update_select("run_executable", choices=choices)

    @reactive.effect
    @reactive.event(input.goto_build)
    def navigate_to_build():
        """Navigate to the Model Build panel"""
        ui.update_radio_buttons("navigation", selected="nav_model_build")

    @reactive.effect
    @reactive.event(input.btn_build)
    def on_build():
        """Handle Build button click - builds named executable"""
        logger.info("User clicked Build button")

        compiler = input.build_compiler()
        build_type = input.build_type()
        clean_first = input.build_clean_first()
        exe_name = get_target_exe_name()

        _build_log_lines.set([
            "=" * 50 + "\n",
            f"Building: {exe_name}\n",
            "=" * 50 + "\n",
            f"Compiler: {compiler}\n",
            f"Build Type: {build_type}\n",
        ])

        def _do_build():
            import time
            start_time = time.time()
            lines = _build_log_lines.get()

            try:
                if clean_first:
                    lines.append("\n=== Cleaning previous build ===\n")
                    _build_log_lines.set(lines)
                    p = subprocess.Popen(
                        ["make", "clean-lib"],
                        cwd=ROOT,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT,
                        text=True
                    )
                    for line in p.stdout:
                        lines.append(line)
                        _build_log_lines.set(lines)
                    p.wait()

                lines.append("\n=== Building library and executable ===\n")
                _build_log_lines.set(lines)

                # Use build-named target for named executables
                p = subprocess.Popen(
                    ["make", f"FC={compiler}", f"BUILD_TYPE={build_type}", "build-named"],
                    cwd=ROOT,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True
                )

                for line in p.stdout:
                    lines.append(line)
                    _build_log_lines.set(lines)
                    if len(lines) > 500:
                        lines = lines[-400:]

                p.wait()
                elapsed = time.time() - start_time

                lines.append("-" * 50 + "\n")
                if p.returncode == 0:
                    lines.append(f"✓ Build completed successfully!\n")
                    lines.append(f"  Executable: {exe_name}\n")
                    lines.append(f"  Time: {elapsed:.1f}s\n")
                else:
                    lines.append(f"✗ Build failed with return code {p.returncode}\n")
                lines.append("=" * 50 + "\n")

                _build_log_lines.set(lines)

            except Exception as e:
                lines.append(f"\nError: {e}\n")
                _build_log_lines.set(lines)

        threading.Thread(target=_do_build, daemon=True, name="BuildThread").start()

    @reactive.effect
    @reactive.event(input.btn_rebuild)
    def on_rebuild():
        """Handle Rebuild All button click - forces clean build"""
        logger.info("User clicked Rebuild All button")

        compiler = input.build_compiler()
        build_type = input.build_type()
        exe_name = get_target_exe_name()

        _build_log_lines.set([
            "=" * 50 + "\n",
            f"Full Rebuild: {exe_name}\n",
            "=" * 50 + "\n",
            f"Compiler: {compiler}\n",
            f"Build Type: {build_type}\n",
        ])

        def _do_rebuild():
            import time
            start_time = time.time()
            lines = _build_log_lines.get()

            try:
                # Always clean first for rebuild
                lines.append("\n=== Cleaning all build artifacts ===\n")
                _build_log_lines.set(lines)
                p = subprocess.Popen(
                    ["make", "clean-lib"],
                    cwd=ROOT,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True
                )
                for line in p.stdout:
                    lines.append(line)
                    _build_log_lines.set(lines)
                p.wait()

                lines.append("\n=== Rebuilding library and executable ===\n")
                _build_log_lines.set(lines)

                # Use build-named target for named executables
                p = subprocess.Popen(
                    ["make", f"FC={compiler}", f"BUILD_TYPE={build_type}", "build-named"],
                    cwd=ROOT,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True
                )

                for line in p.stdout:
                    lines.append(line)
                    _build_log_lines.set(lines)
                    if len(lines) > 500:
                        lines = lines[-400:]

                p.wait()
                elapsed = time.time() - start_time

                lines.append("-" * 50 + "\n")
                if p.returncode == 0:
                    lines.append(f"✓ Rebuild completed successfully!\n")
                    lines.append(f"  Executable: {exe_name}\n")
                    lines.append(f"  Time: {elapsed:.1f}s\n")
                else:
                    lines.append(f"✗ Rebuild failed with return code {p.returncode}\n")
                lines.append("=" * 50 + "\n")

                _build_log_lines.set(lines)

            except Exception as e:
                lines.append(f"\nError: {e}\n")
                _build_log_lines.set(lines)

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
        except:
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
        
        # Capture current widget values (must be done in reactive context)
        estas_cmd = build_estas_command()
        
        # Validate constants file before running
        const_file = input.cmd_constants_file() or ""
        if not const_file and (input.cmd_binary_enabled() or input.cmd_shear_stress_file()):
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
        
        def _work():
            import time
            start_time = time.time()
            logger.info("Quick Run thread started")
            
            cmd_display = " ".join([c if c else '""' for c in estas_cmd])
            _log_lines.append(f"Command: {cmd_display}\n")
            
            # Filter out empty strings for actual execution
            exec_cmd = [c for c in estas_cmd if c]
            rc = run_command(exec_cmd, cwd=ROOT)
            
            elapsed = time.time() - start_time
            final_msg = f"Finished with rc={rc} (time: {elapsed:.1f}s)\n"
            logger.info(final_msg.strip())
            _log_lines.append(final_msg)
        threading.Thread(target=_work, daemon=True, name="QuickRunThread").start()

    # Dynamic theme handling
    current_theme = reactive.Value(get_saved_theme())
    theme_save_status = reactive.Value("")

    @render.ui
    def dynamic_theme_css():
        """Render the current theme CSS dynamically"""
        theme_name = current_theme.get()
        if THEMES_AVAILABLE and theme_name:
            css = get_theme_css(theme_name)
            if css:
                return ui.tags.style(css, id="shinyswatch-theme")
        return ui.TagList()

    if THEMES_AVAILABLE:
        @reactive.effect
        @reactive.event(input.apply_theme)
        def handle_theme_change():
            selected_theme = input.theme_select()
            logger.info(f"User requested theme change to: {selected_theme}")

            # Update the reactive theme value (triggers CSS update)
            current_theme.set(selected_theme)

            # Also save to file for persistence across restarts
            theme_file = os.path.join(ROOT, '.aquabc_theme')
            try:
                with open(theme_file, 'w') as f:
                    f.write(selected_theme)
                logger.info(f"Saved theme preference: {selected_theme}")
                theme_save_status.set(f"Theme '{selected_theme}' applied!")
            except Exception as e:
                logger.error(f"Error saving theme preference: {e}")
                theme_save_status.set(f"Applied but save failed: {e}")

        @render.text
        def theme_status():
            return theme_save_status.get()

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
        path = os.path.join(INPUTS_DIR, f)

        try:
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
        path = os.path.join(INPUTS_DIR, f)
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
        path = os.path.join(INPUTS_DIR, f)
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

    def _get_cached_csv(max_rows=None):
        """Get cached CSV or reload if modified"""
        try:
            if not os.path.exists(OUTPUT_CSV):
                logger.warning(f"OUTPUT.csv does not exist at {OUTPUT_CSV}")
                return None

            current_mtime = os.path.getmtime(OUTPUT_CSV)
            cached_mtime = csv_cache_mtime.get()

            if cached_mtime != current_mtime or csv_cache.get() is None:
                logger.info(f"Loading OUTPUT.csv (max_rows={max_rows}, file modified or no cache)")
                logger.debug(f"  Cached mtime: {cached_mtime}, Current mtime: {current_mtime}")
                import time
                start_time = time.time()

                df = pd.read_csv(OUTPUT_CSV, comment='#', skip_blank_lines=True, nrows=max_rows)
                # Strip whitespace from column names
                df.columns = [c.strip() for c in df.columns]

                load_time = time.time() - start_time
                logger.info(f"Loaded {len(df)} rows, {len(df.columns)} columns in {load_time:.2f}s")

                if max_rows is None:  # Only cache full reads
                    csv_cache.set(df)
                    csv_cache_mtime.set(current_mtime)
                    logger.debug("CSV cached for future use")
                return df
            else:
                logger.debug("Using cached CSV data")
                return csv_cache.get()
        except Exception as e:
            logger.error(f"Error reading OUTPUT.csv: {e}", exc_info=True)
            return None

    # load available Y variables from OUTPUT.csv header
    def _get_output_columns():
        try:
            # Read only header to avoid OOM
            df = pd.read_csv(OUTPUT_CSV, comment='#', skip_blank_lines=True, nrows=0)
            cols = [c.strip() for c in df.columns]
            return cols
        except Exception as e:
            logger.error(f"Error reading OUTPUT.csv header: {e}")
            return []

    @reactive.Effect
    def _():
        cols = _get_output_columns()
        if cols:
            # TIME is first column
            y_choices = cols[1:]
            # default select first variable on left, none on right
            ui.update_selectize("left_vars", choices=y_choices, selected=[y_choices[0]] if y_choices else [])
            ui.update_selectize("right_vars", choices=y_choices, selected=[])

    # run commands in background and capture logs
    # Note: _log_lines is defined earlier in server() for quick_run access

    def run_command(cmd, cwd=ROOT, env=None):
        """Run a command and capture output to _log_lines.

        Args:
            cmd: Command list to execute
            cwd: Working directory
            env: Optional environment dict (merged with os.environ if provided)
        """
        logger.info(f"Running command: {cmd}")
        try:
            # Merge environment if provided
            run_env = os.environ.copy()
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
            except:
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
            import time
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
    _model_process = reactive.value(None)
    _model_running = reactive.value(False)

    def get_output_csv_info():
        """Get info about OUTPUT.csv file for progress tracking"""
        try:
            if os.path.exists(OUTPUT_CSV):
                stat = os.stat(OUTPUT_CSV)
                size_kb = stat.st_size / 1024
                # Count lines (fast method)
                with open(OUTPUT_CSV, 'rb') as f:
                    lines = sum(1 for _ in f)
                return {"exists": True, "size_kb": size_kb, "lines": lines}
        except:
            pass
        return {"exists": False, "size_kb": 0, "lines": 0}

    def format_elapsed(seconds):
        """Format elapsed time as HH:MM:SS"""
        hours = int(seconds // 3600)
        minutes = int((seconds % 3600) // 60)
        secs = int(seconds % 60)
        if hours > 0:
            return f"{hours}h {minutes}m {secs}s"
        elif minutes > 0:
            return f"{minutes}m {secs}s"
        else:
            return f"{secs}s"

    @reactive.effect
    @reactive.event(input.run)
    def on_run():
        logger.info("User clicked 'Run' button")
        _log_lines.clear()
        _log_lines.append("=" * 50 + "\n")
        _log_lines.append("Starting model run...\n")
        _log_lines.append("=" * 50 + "\n")

        # Capture current widget values (must be done in reactive context)
        estas_cmd = build_estas_command()

        # Check if executable exists
        try:
            exe_name = input.run_executable()
        except:
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
            except:
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

        # Get initial OUTPUT.csv state
        initial_output_info = get_output_csv_info()

        def _work():
            import time
            start_time = time.time()
            logger.info("Run thread started")

            # Filter out empty strings for actual execution
            exec_cmd = [c for c in estas_cmd if c]

            try:
                # Start the model process
                run_env = os.environ.copy()
                p = subprocess.Popen(
                    exec_cmd,
                    cwd=ROOT,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True,
                    bufsize=1
                )
                _model_process.set(p)
                _model_running.set(True)

                # Progress tracking variables
                last_update = time.time()
                spinner = ['|', '/', '-', '\\']
                spinner_idx = 0
                last_lines = initial_output_info.get("lines", 0)

                # Read output in non-blocking manner with progress updates
                import select
                while p.poll() is None:
                    # Check for output (with timeout)
                    if p.stdout:
                        # Use select for non-blocking read on Unix
                        try:
                            import select
                            readable, _, _ = select.select([p.stdout], [], [], 0.5)
                            if readable:
                                line = p.stdout.readline()
                                if line:
                                    _log_lines.append(line)
                                    while len(_log_lines) > 500:
                                        _log_lines.pop(0)
                        except:
                            # Fallback: just sleep
                            time.sleep(0.5)

                    # Update progress every 2 seconds
                    now = time.time()
                    if now - last_update >= 2.0:
                        elapsed = now - start_time
                        output_info = get_output_csv_info()
                        current_lines = output_info.get("lines", 0)
                        size_kb = output_info.get("size_kb", 0)

                        # Calculate lines written since start
                        new_lines = current_lines - last_lines if current_lines > last_lines else 0
                        if new_lines > 0:
                            last_lines = current_lines

                        # Update progress line (replace last progress line if exists)
                        spinner_char = spinner[spinner_idx % len(spinner)]
                        spinner_idx += 1

                        progress_msg = (
                            f"\r{spinner_char} Running... "
                            f"Elapsed: {format_elapsed(elapsed)} | "
                            f"OUTPUT.csv: {current_lines} rows ({size_kb:.1f} KB)\n"
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
                final_output_info = get_output_csv_info()
                final_lines = final_output_info.get("lines", 0)
                final_size = final_output_info.get("size_kb", 0)

                if rc == 0:
                    _log_lines.append(f"✓ Model run completed successfully!\n")
                    _log_lines.append(f"  Total time: {format_elapsed(elapsed)}\n")
                    _log_lines.append(f"  OUTPUT.csv: {final_lines} rows ({final_size:.1f} KB)\n")
                else:
                    _log_lines.append(f"✗ Model run failed with return code {rc}\n")
                    _log_lines.append(f"  Total time: {format_elapsed(elapsed)}\n")

                _log_lines.append("=" * 50 + "\n")
                logger.info(f"Model run finished: rc={rc}, elapsed={elapsed:.1f}s")

            except Exception as e:
                _log_lines.append(f"\n❌ Error running model: {e}\n")
                logger.error(f"Model run error: {e}")

            finally:
                _model_process.set(None)
                _model_running.set(False)

        threading.Thread(target=_work, daemon=True, name="RunThread").start()

    @render.text
    def run_log():
        # Poll every 500ms to catch updates from background threads
        reactive.invalidate_later(0.5)
        # Read from shared list (thread-safe)
        return ''.join(_log_lines[-100:])  # Last 100 lines

    @render.text
    def run_log_mini():
        """Mini run log for Model Config panel"""
        reactive.invalidate_later(0.5)
        # Show last 20 lines in the mini log
        return ''.join(_log_lines[-20:])

    @render.ui
    def run_status_indicator():
        """Show running status indicator"""
        reactive.invalidate_later(1.0)
        is_running = _model_running.get()

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
        process = _model_process.get()
        if process and process.poll() is None:
            try:
                import signal
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

                _model_running.set(False)
                _model_process.set(None)
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

            # Find data start (first non-comment, non-empty line after headers)
            data_start = 0
            for i, line in enumerate(lines):
                stripped = line.strip()
                if stripped and not stripped.startswith('#'):
                    # Check if it's a data line (starts with number)
                    try:
                        float(stripped.split()[0])
                        data_start = i
                        break
                    except (ValueError, IndexError):
                        continue

            # Read as DataFrame
            df = pd.read_csv(filepath, skiprows=data_start, sep=r'\s+', header=None)

            if df.empty:
                logger.warning("No data in timeseries file")
                return None

            # First column is TIME
            time_col = df.iloc[:, 0]

            # Convert Julian days to dates
            from datetime import timedelta
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

        # Use cached CSV with row limit to prevent OOM
        logger.debug(f"Fetching CSV data (max {DEFAULT_PLOT_ROWS} rows)")
        df = _get_cached_csv(max_rows=DEFAULT_PLOT_ROWS)
        if df is None or df.empty:
            logger.warning("No data available for plotting")
            return None

        xcol = df.columns[0]
        apply_smooth = input.smooth()
        win = input.smooth_window() if apply_smooth else 1
        logger.debug(f"X-axis: {xcol}, Smoothing: {apply_smooth} (window={win})")

        fig = go.Figure()
        trace_count = 0

        # left axis traces
        for var in left:
            if var not in df.columns:
                logger.warning(f"Variable '{var}' not found in CSV columns")
                continue
            y = df[var]
            if apply_smooth and win > 1:
                y = y.rolling(window=win, min_periods=1).mean()
            fig.add_trace(go.Scatter(x=df[xcol], y=y, mode='lines', name=var, yaxis='y'))
            trace_count += 1

        # right axis traces
        for var in right:
            if var not in df.columns:
                logger.warning(f"Variable '{var}' not found in CSV columns")
                continue
            y = df[var]
            if apply_smooth and win > 1:
                y = y.rolling(window=win, min_periods=1).mean()
            fig.add_trace(go.Scatter(x=df[xcol], y=y, mode='lines', name=var, yaxis='y2'))
            trace_count += 1

        logger.debug(f"Created {trace_count} traces")

        layout = dict(
            title=f"{', '.join(left + right)} vs {xcol}",
            yaxis=dict(title='Left axis'),
            xaxis=dict(title=xcol)
        )

        if right:
            layout['yaxis2'] = dict(title='Right axis', overlaying='y', side='right')

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

    logger.info("=" * 60)
    logger.info("Server function initialization complete")
    logger.info("All reactive effects and render functions registered")
    logger.info("=" * 60)


app = App(app_ui, server)

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
