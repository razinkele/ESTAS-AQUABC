"""Pure output-file helpers (extracted from server())."""
import os
import logging
import pandas as pd

try:
    from shiny_app.utils import PELAGIC_BOX_COLUMNS
    from shiny_app.simulation_config import SimulationConfigFile
except ImportError:
    from utils import PELAGIC_BOX_COLUMNS
    from simulation_config import SimulationConfigFile

logger = logging.getLogger("AQUABC")

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))
OUTPUT_CSV = os.path.join(ROOT, 'OUTPUT.csv')
INPUT_TXT_PATH = os.path.join(ROOT, 'INPUT.txt')


def looks_numeric(s: str) -> bool:
    """Return True if string looks like a number (int or float)."""
    try:
        float(s)
        return True
    except (ValueError, TypeError):
        return False


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


def get_output_folder_from_config(input_txt_path=INPUT_TXT_PATH):
    """Get output folder from INPUT.txt configuration"""
    try:
        if os.path.exists(input_txt_path):
            scf = SimulationConfigFile(input_txt_path)
            if scf.parse():
                return scf.config.output_folder.rstrip('/')
    except Exception as e:
        logger.warning(f"Could not read output folder from INPUT.txt: {e}")
    return "OUTPUTS"  # fallback


def get_output_files_info(root=ROOT, input_txt_path=INPUT_TXT_PATH):
    """Get info about output files in the configured output folder for progress tracking"""
    try:
        output_folder = get_output_folder_from_config(input_txt_path=input_txt_path)
        output_dir = os.path.join(root, output_folder)

        if not os.path.isdir(output_dir):
            return {"exists": False, "size_kb": 0, "file_count": 0, "folder": output_folder}

        total_size = 0
        file_count = 0
        out_files = 0
        bin_files = 0

        for fname in os.listdir(output_dir):
            fpath = os.path.join(output_dir, fname)
            if os.path.isfile(fpath):
                try:
                    total_size += os.path.getsize(fpath)
                    file_count += 1
                    if fname.endswith('.out'):
                        out_files += 1
                    elif fname.endswith('.bin'):
                        bin_files += 1
                except OSError:
                    pass

        return {
            "exists": True,
            "size_kb": total_size / 1024,
            "file_count": file_count,
            "out_files": out_files,
            "bin_files": bin_files,
            "folder": output_folder
        }
    except Exception as e:
        logger.debug(f"Error getting output info: {e}")
    return {"exists": False, "size_kb": 0, "file_count": 0, "folder": "OUTPUTS"}


def get_output_columns(file_path=None, file_format=None, output_csv=OUTPUT_CSV):
    """Get column names from an output file."""
    target_path = file_path or output_csv

    # Auto-detect format
    if file_format is None:
        if target_path.endswith('.bin'):
            file_format = 'binary'
        elif target_path.endswith('.out'):
            file_format = 'text'
        else:
            file_format = 'csv'

    try:
        if file_format == 'binary':
            # Binary files use fixed column names
            return PELAGIC_BOX_COLUMNS
        elif file_format == 'text':
            # Read header from .out file
            df = pd.read_csv(target_path, sep=r'\s+', nrows=0)
            cols = [c.strip() for c in df.columns]
            # Sanity check: if the first column name looks numeric, the file
            # has no header (e.g. PROCESS_RATES).  A proper header always
            # starts with a string like TIME_DAYS.
            if cols and looks_numeric(cols[0]):
                logger.warning(f"File appears headerless (numeric column names): {os.path.basename(target_path)}")
                if len(cols) == len(PELAGIC_BOX_COLUMNS):
                    return list(PELAGIC_BOX_COLUMNS)
                return [f"V{i}" for i in range(len(cols))]
            return cols
        else:
            # Read header from CSV
            df = pd.read_csv(target_path, comment='#', skip_blank_lines=True, nrows=0)
            return [c.strip() for c in df.columns]
    except Exception as e:
        logger.error(f"Error reading output file header: {e}")
        return []


def get_output_directories(root=ROOT, output_csv=OUTPUT_CSV):
    """Get list of output directories in the workspace"""
    dirs = {}
    # Add root OUTPUT.csv as option
    if os.path.exists(output_csv):
        dirs["ROOT"] = "OUTPUT.csv (root directory)"

    # Find OUTPUTS_* directories
    for item in os.listdir(root):
        if item.startswith("OUTPUTS") and os.path.isdir(os.path.join(root, item)):
            dirs[item] = item
    return dirs


def get_output_files_from_dir(dir_name, file_format="text", root=ROOT):
    """Get list of output files from the selected directory based on format.

    Args:
        dir_name: Directory name (relative to ROOT) or "ROOT"
        file_format: 'text' for .out, 'binary' for .bin, 'csv' for .csv

    Returns:
        dict: {filename: display_name} for UI choices
    """
    files = {}

    if not dir_name:
        return files

    if dir_name == "ROOT":
        dir_path = root
    else:
        dir_path = os.path.join(root, dir_name)

    if not os.path.isdir(dir_path):
        return files

    # Collect files matching the requested format
    if file_format == "binary":
        # For binary, prefer PELAGIC_BOX files
        for f in sorted(os.listdir(dir_path)):
            if f.endswith(".bin") and "PELAGIC_BOX" in f and "PROCESS_RATES" not in f:
                files[f] = f
    elif file_format == "csv":
        for f in sorted(os.listdir(dir_path)):
            if f.endswith(".csv") and os.path.isfile(os.path.join(dir_path, f)):
                files[f] = f
    else:  # text (.out)
        for f in sorted(os.listdir(dir_path)):
            if (f.endswith(".out") and "PELAGIC_BOX" in f
                    and "PROCESS_RATES" not in f
                    and os.path.isfile(os.path.join(dir_path, f))):
                files[f] = f

    return files
