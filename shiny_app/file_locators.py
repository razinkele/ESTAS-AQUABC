"""Output/box file discovery helpers, extracted from app.py."""
import logging
import os

logger = logging.getLogger("AQUABC")

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
INPUTS_DIR = os.path.join(ROOT, 'INPUTS')


def get_output_folder():
    """Get the output folder from INPUT.txt or use default."""
    input_file = os.path.join(ROOT, 'INPUT.txt')
    try:
        if os.path.exists(input_file):
            with open(input_file, 'r') as f:
                lines = f.readlines()
            # Line 22 (1-indexed) contains OUTPUT folder
            if len(lines) >= 22:
                folder = lines[21].strip().split('!')[0].strip()
                return os.path.join(ROOT, folder)
    except Exception as e:
        logger.debug(f"Could not read output folder from INPUT.txt: {e}")
    # Fallback to OUTPUTS directory
    return os.path.join(ROOT, 'OUTPUTS')

def find_pelagic_box_file(output_folder=None, file_type='text'):
    """Find a PELAGIC_BOX output file in the output folder.

    Args:
        output_folder: Output folder path (defaults to get_output_folder())
        file_type: 'text' for .out files, 'binary' for .bin files

    Returns:
        Path to first matching file, or None if not found
    """
    if output_folder is None:
        output_folder = get_output_folder()

    if not output_folder or not os.path.isdir(output_folder):
        return None

    import glob

    if file_type == 'binary':
        # Binary files: patterns like __PELAGIC_BOX_00005.bin
        patterns = [
            os.path.join(output_folder, "__PELAGIC_BOX_*.bin"),
            os.path.join(output_folder, "*PELAGIC_BOX_?????.bin"),
        ]
    else:
        # Text files: PELAGIC_BOX_00005.out
        patterns = [
            os.path.join(output_folder, "PELAGIC_BOX_*.out"),
        ]

    files = []
    for pattern in patterns:
        matches = glob.glob(pattern)
        # Exclude PROCESS_RATES files
        matches = [f for f in matches if "PROCESS_RATES" not in f]
        files.extend(matches)

    # Sort and deduplicate
    files = sorted(set(files))

    if files:
        return files[0]
    return None


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
