"""Pure output-file helpers (extracted from server())."""
import logging
import os

import pandas as pd

try:
    from shiny_app.simulation_config import SimulationConfigFile
    from shiny_app.utils import PELAGIC_BOX_COLUMNS, pelagic_binary_layout
except ImportError:
    from simulation_config import SimulationConfigFile
    from utils import PELAGIC_BOX_COLUMNS, pelagic_binary_layout

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
            # No header: the layout follows the producing setup's state count
            return pelagic_binary_layout(target_path)
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


def first_box_output(root=ROOT, input_file=None):
    """Lowest-numbered PELAGIC_BOX_NNNNN.out of the run a control file describes, or None."""
    control = os.path.join(root, input_file or "INPUT.txt")
    folder = os.path.join(root, get_output_folder_from_config(input_txt_path=control))
    try:
        names = sorted(f for f in os.listdir(folder)
                       if f.startswith("PELAGIC_BOX_") and f.endswith(".out") and "PROCESS_RATES" not in f)
    except OSError:
        return None
    return os.path.join(folder, names[0]) if names else None


def analysis_csv(root=ROOT, input_file=None, cache_dir=None):
    """A model-output CSV in the 0D ``OUTPUT.csv`` layout, for the features written against it.

    Mass Balance, Observations and Data Preview read ``TIME, NH4N, NO3N, ...`` from a root
    OUTPUT.csv, which only the 0D example writes; ESTAS writes PELAGIC_BOX_NNNNN.out per box
    with other column names. This returns ``(csv_path, source)``:

    * the run's first box output, converted to the OUTPUT.csv column names (state variables are
      in the same order in both) and cached until the box file changes;
    * else the root OUTPUT.csv if there is one;
    * else ``(None, reason)``.

    ``source`` says which file the numbers come from, for the user. One box's concentrations
    are not a closed budget (transport crosses the box), which the callers' messages state.
    """
    import hashlib
    import tempfile

    try:
        from shiny_app.ic_parser import STATE_VARIABLES
    except ImportError:
        from ic_parser import STATE_VARIABLES

    box = first_box_output(root, input_file)
    legacy = os.path.join(root, "OUTPUT.csv")
    if box is None:
        if os.path.exists(legacy):
            return legacy, "OUTPUT.csv"
        return None, "No model output found. Run the model first."
    cache_dir = cache_dir or os.path.join(tempfile.gettempdir(), "aquabc_analysis")
    os.makedirs(cache_dir, exist_ok=True)
    key = hashlib.sha1(os.path.abspath(box).encode()).hexdigest()[:16]      # noqa: S324 (not security)
    out = os.path.join(cache_dir, f"{key}_{os.path.splitext(os.path.basename(box))[0]}.csv")
    source = os.path.relpath(box, root)
    if os.path.exists(out) and os.path.getmtime(out) >= os.path.getmtime(box):
        return out, source
    # Rows with the wrong number of fields are dropped: output folders written by two runs at
    # once (possible before the run guard) hold interleaved lines.
    df = pd.read_csv(box, sep=r"\s+", on_bad_lines="skip")
    df = df[pd.to_numeric(df.iloc[:, 0], errors="coerce").notna()]
    names = ["TIME"]
    for i, col in enumerate(df.columns[1:], start=1):
        names.append(STATE_VARIABLES[i]["csv_name"] if i in STATE_VARIABLES else col.strip())
    df.columns = names
    tmp = out + ".tmp"
    df.to_csv(tmp, index=False, float_format="%.10g")
    os.replace(tmp, out)
    return out, source
