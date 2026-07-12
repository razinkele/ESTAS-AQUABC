"""Input-file analysis and validation helpers, extracted from app.py."""
import logging
import os
from datetime import date, timedelta

logger = logging.getLogger("AQUABC")

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
INPUTS_DIR = os.path.join(ROOT, 'INPUTS')

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

