"""
AQUABC Initial Conditions Parser Module

Parses and manages AQUABC initial conditions files (INIT_CONC_*.txt).
Provides structured access to 36+ state variable initial concentrations.
"""

import logging
import os
import re
import shutil
from dataclasses import dataclass
from datetime import datetime

logger = logging.getLogger("AQUABC.ic")

# State variable definitions with names, descriptions, units, and CSV column names
# The 'csv_name' field matches column headers in OUTPUT.csv
STATE_VARIABLES = {
    1: {"name": "NH4_N", "csv_name": "NH4N", "description": "Ammonium Nitrogen", "units": "mg N/L", "category": "Nutrients"},
    2: {"name": "NO3_N", "csv_name": "NO3N", "description": "Nitrate Nitrogen", "units": "mg N/L", "category": "Nutrients"},
    3: {"name": "PO4_P", "csv_name": "PO4P", "description": "Orthophosphate Phosphorus", "units": "mg P/L", "category": "Nutrients"},
    4: {"name": "DISS_OXYGEN", "csv_name": "DOXY", "description": "Dissolved Oxygen", "units": "mg O₂/L", "category": "Gases"},
    5: {"name": "DIA_C", "csv_name": "DIAC", "description": "Diatoms Carbon", "units": "mg C/L", "category": "Phytoplankton"},
    6: {"name": "ZOO_C", "csv_name": "ZOOC", "description": "Zooplankton Carbon", "units": "mg C/L", "category": "Zooplankton"},
    7: {"name": "ZOO_N", "csv_name": "ZOON", "description": "Zooplankton Nitrogen", "units": "mg N/L", "category": "Zooplankton"},
    8: {"name": "ZOO_P", "csv_name": "ZOOP", "description": "Zooplankton Phosphorus", "units": "mg P/L", "category": "Zooplankton"},
    9: {"name": "DET_PART_ORG_C", "csv_name": "DETC", "description": "Detritus Particulate Organic Carbon", "units": "mg C/L", "category": "Particulates"},
    10: {"name": "DET_PART_ORG_N", "csv_name": "DETN", "description": "Detritus Particulate Organic Nitrogen", "units": "mg N/L", "category": "Particulates"},
    11: {"name": "DET_PART_ORG_P", "csv_name": "DETP", "description": "Detritus Particulate Organic Phosphorus", "units": "mg P/L", "category": "Particulates"},
    12: {"name": "DISS_ORG_C", "csv_name": "DOC", "description": "Dissolved Organic Carbon", "units": "mg C/L", "category": "Dissolved OM"},
    13: {"name": "DISS_ORG_N", "csv_name": "DON", "description": "Dissolved Organic Nitrogen", "units": "mg N/L", "category": "Dissolved OM"},
    14: {"name": "DISS_ORG_P", "csv_name": "DOP", "description": "Dissolved Organic Phosphorus", "units": "mg P/L", "category": "Dissolved OM"},
    15: {"name": "CYN_C", "csv_name": "NOFIX_CYNC", "description": "Non-fixing Cyanobacteria Carbon", "units": "mg C/L", "category": "Phytoplankton"},
    16: {"name": "OPA_C", "csv_name": "OPA", "description": "Other Phytoplankton Carbon", "units": "mg C/L", "category": "Phytoplankton"},
    17: {"name": "DISS_Si", "csv_name": "DISSOLVED_SILICA", "description": "Dissolved Silica", "units": "mg Si/L", "category": "Nutrients"},
    18: {"name": "PART_Si", "csv_name": "PARTICULATE_SILICA", "description": "Particulate Silica", "units": "mg Si/L", "category": "Particulates"},
    19: {"name": "FIX_CYN_C", "csv_name": "FIX_CYNC", "description": "N-fixing Cyanobacteria Carbon", "units": "mg C/L", "category": "Phytoplankton"},
    20: {"name": "INORG_C", "csv_name": "DIC", "description": "Dissolved Inorganic Carbon", "units": "mol C/L", "category": "Carbonate"},
    21: {"name": "TOT_ALK", "csv_name": "ALKALINITY", "description": "Total Alkalinity", "units": "eq/L", "category": "Carbonate"},
    22: {"name": "FE_II", "csv_name": "FE_II", "description": "Ferrous Iron (Fe²⁺)", "units": "mg Fe/L", "category": "Metals"},
    23: {"name": "FE_III", "csv_name": "FE_III", "description": "Ferric Iron (Fe³⁺)", "units": "mg Fe/L", "category": "Metals"},
    24: {"name": "MN_II", "csv_name": "MN_II", "description": "Manganous Manganese (Mn²⁺)", "units": "mg Mn/L", "category": "Metals"},
    25: {"name": "MN_IV", "csv_name": "MN_IV", "description": "Manganic Manganese (Mn⁴⁺)", "units": "mg Mn/L", "category": "Metals"},
    26: {"name": "CA", "csv_name": "CA", "description": "Calcium", "units": "mg Ca/L", "category": "Metals"},
    27: {"name": "MG", "csv_name": "MG", "description": "Magnesium", "units": "mg Mg/L", "category": "Metals"},
    28: {"name": "S_PLUS_6", "csv_name": "S_PLUS_6", "description": "Sulphate Sulphur (SO₄²⁻)", "units": "mg S/L", "category": "Sulphur"},
    29: {"name": "S_MINUS_2", "csv_name": "S_MINUS_2", "description": "Sulphide Sulphur (H₂S)", "units": "mg S/L", "category": "Sulphur"},
    30: {"name": "CH4_C", "csv_name": "CH4_C", "description": "Methane Carbon", "units": "mg C/L", "category": "Gases"},
    31: {"name": "NOST_VEG_HET_C", "csv_name": "NOST_VEG_HET_C", "description": "Nostocales Heterocyst Carbon", "units": "mg C/L", "category": "Phytoplankton"},
    32: {"name": "AKI_C", "csv_name": "AKI_C", "description": "Akinetes Carbon", "units": "g/m²", "category": "Phytoplankton"},
    33: {"name": "SEC_METAB_DIA", "csv_name": "SEC_METAB_DIA", "description": "Diatom Secondary Metabolites", "units": "relative", "category": "Allelopathy"},
    34: {"name": "SEC_METAB_NOFIX_CYN", "csv_name": "SEC_METAB_NOFIX_CYN", "description": "Non-fixing Cyanobacteria Metabolites", "units": "relative", "category": "Allelopathy"},
    35: {"name": "SEC_METAB_FIX_CYN", "csv_name": "SEC_METAB_FIX_CYN", "description": "N-fixing Cyanobacteria Metabolites", "units": "relative", "category": "Allelopathy"},
    36: {"name": "SEC_METAB_NOST", "csv_name": "SEC_METAB_NOST", "description": "Nostocales Metabolites", "units": "relative", "category": "Allelopathy"},
    37: {"name": "EXTRA_VAR", "csv_name": "EXTRA_VAR", "description": "Extra Variable", "units": "-", "category": "Other"},
}

# Categories for state variables (by index)
STATE_VARIABLE_CATEGORIES = {
    "Nutrients": [1, 2, 3, 17],
    "Dissolved Gases": [4, 30],
    "Phytoplankton": [5, 15, 16, 19, 31, 32],
    "Zooplankton": [6, 7, 8],
    "Particulate Organics": [9, 10, 11, 18],
    "Dissolved Organics": [12, 13, 14],
    "Carbonate System": [20, 21],
    "Metals": [22, 23, 24, 25, 26, 27],
    "Sulphur": [28, 29],
    "Allelopathy": [33, 34, 35, 36],
    "Other": [37],
}

# Build CSV column name to info mapping
CSV_COLUMN_INFO = {}
for idx, info in STATE_VARIABLES.items():
    csv_name = info.get("csv_name", info["name"])
    var_name = info["name"]
    info_dict = {
        "index": idx,
        "name": info["name"],
        "description": info["description"],
        "units": info["units"],
        "category": info.get("category", "Other"),
    }
    # Add by csv_name (e.g., "NH4N")
    CSV_COLUMN_INFO[csv_name] = info_dict
    CSV_COLUMN_INFO[csv_name.lower()] = info_dict
    # Also add by variable name (e.g., "NH4_N") for PELAGIC_BOX output files
    if var_name != csv_name:
        CSV_COLUMN_INFO[var_name] = info_dict
        CSV_COLUMN_INFO[var_name.lower()] = info_dict


def get_variable_display_name(csv_column: str) -> str:
    """Get a human-readable display name for a CSV column.

    Args:
        csv_column: Column name from OUTPUT.csv (e.g., 'NH4N', 'DIAC')

    Returns:
        Descriptive name like 'NH4N - Ammonium Nitrogen (mg N/L)'
    """
    col_clean = csv_column.strip()
    info = CSV_COLUMN_INFO.get(col_clean) or CSV_COLUMN_INFO.get(col_clean.lower())

    if info:
        return f"{col_clean} - {info['description']} ({info['units']})"
    else:
        # Unknown column - return as-is
        return col_clean


def get_variable_info(csv_column: str) -> dict | None:
    """Get full info dict for a CSV column name.

    Args:
        csv_column: Column name from OUTPUT.csv

    Returns:
        Dict with index, name, description, units, category or None
    """
    col_clean = csv_column.strip()
    return CSV_COLUMN_INFO.get(col_clean) or CSV_COLUMN_INFO.get(col_clean.lower())


def get_grouped_variable_choices(columns: list[str]) -> dict[str, dict[str, str]]:
    """Create grouped choices for selectize input from CSV columns.

    Args:
        columns: List of column names from CSV header

    Returns:
        Dict of {category: {csv_name: display_name}} for grouped selectize
    """
    # Group columns by category
    grouped = {}
    unknown = {}

    for col in columns:
        col_clean = col.strip()
        if col_clean.lower() in ['time', 'time_days', 'date', 'datetime', 'julian_day']:
            continue

        info = get_variable_info(col_clean)
        if info:
            category = info['category']
            if category not in grouped:
                grouped[category] = {}
            # Use only the description as display name (no code)
            grouped[category][col_clean] = info['description']
        else:
            unknown[col_clean] = col_clean

    # Add unknown columns at the end
    if unknown:
        grouped["Other"] = unknown

    return grouped


@dataclass
class InitialCondition:
    """Individual state variable initial condition"""
    var_id: int
    value: float
    comment: str
    line_number: int
    original_line: str

    # Metadata from STATE_VARIABLES
    name: str = ""
    description: str = ""
    units: str = ""

    def __post_init__(self):
        """Set metadata from STATE_VARIABLES"""
        if self.var_id in STATE_VARIABLES:
            info = STATE_VARIABLES[self.var_id]
            self.name = info["name"]
            self.description = info["description"]
            self.units = info["units"]
        else:
            self.name = f"VAR_{self.var_id}"
            self.description = f"Variable {self.var_id}"
            self.units = "-"

    def format_line(self) -> str:
        """Format IC back to file line format"""
        # Format: "                         ID          VALUE             ! COMMENT"
        if self.comment:
            return f"{self.var_id:27d}{self.value:15.6g}             ! {self.comment}"
        else:
            return f"{self.var_id:27d}{self.value:15.6g}"

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization"""
        return {
            "var_id": self.var_id,
            "name": self.name,
            "description": self.description,
            "value": self.value,
            "units": self.units,
            "comment": self.comment,
        }


class ICFile:
    """Parse and manage AQUABC initial conditions files"""

    def __init__(self, filepath: str):
        self.filepath = filepath
        self.conditions: dict[int, InitialCondition] = {}  # var_id -> IC
        self.raw_lines: list[str] = []
        self.header_lines: list[str] = []  # Comment lines at top
        self._parsed = False

    def parse(self) -> bool:
        """
        Parse the IC file.
        Returns True if successful, False otherwise.
        """
        if not os.path.exists(self.filepath):
            logger.error(f"IC file not found: {self.filepath}")
            return False

        try:
            with open(self.filepath) as f:
                self.raw_lines = f.readlines()

            self.conditions = {}
            self.header_lines = []

            for line_num, line in enumerate(self.raw_lines, 1):
                # Check for comment lines
                if line.strip().startswith('#'):
                    self.header_lines.append(line)
                    continue

                ic = self._parse_line(line, line_num)
                if ic:
                    self.conditions[ic.var_id] = ic

            self._parsed = True
            logger.info(f"Parsed {len(self.conditions)} initial conditions from {os.path.basename(self.filepath)}")
            return True

        except Exception as e:
            logger.error(f"Error parsing IC file: {e}")
            return False

    def _parse_line(self, line: str, line_num: int) -> InitialCondition | None:
        """
        Parse a single line of the IC file.
        Format: "                         ID          VALUE             ! COMMENT"
        """
        line = line.rstrip('\n\r')

        # Skip empty lines
        if not line.strip():
            return None

        # Split on '!' to separate value part from comment
        comment = ""
        if '!' in line:
            value_part, comment_part = line.split('!', 1)
            comment = comment_part.strip()
            # Clean up comment - remove trailing numbers that might be old values
            comment = re.sub(r'\s+[\d.]+\s*$', '', comment).strip()
        else:
            value_part = line

        # Parse value part: "         ID          VALUE"
        tokens = value_part.split()
        if len(tokens) < 2:
            return None

        try:
            var_id = int(tokens[0])
            value = float(tokens[1])
        except (ValueError, IndexError):
            return None

        return InitialCondition(
            var_id=var_id,
            value=value,
            comment=comment,
            line_number=line_num,
            original_line=line
        )

    def get_condition(self, var_id: int) -> InitialCondition | None:
        """Get a specific IC by variable ID"""
        if not self._parsed:
            self.parse()
        return self.conditions.get(var_id)

    def get_conditions_by_category(self, category: str) -> list[InitialCondition]:
        """Get all ICs in a category"""
        if not self._parsed:
            self.parse()

        if category not in STATE_VARIABLE_CATEGORIES:
            return []

        var_ids = STATE_VARIABLE_CATEGORIES[category]
        return [self.conditions[vid] for vid in var_ids if vid in self.conditions]

    def get_all_conditions(self) -> list[InitialCondition]:
        """Get all ICs sorted by variable ID"""
        if not self._parsed:
            self.parse()
        return sorted(self.conditions.values(), key=lambda x: x.var_id)

    def update_condition(self, var_id: int, new_value: float) -> tuple[bool, str]:
        """
        Update an IC value.
        Returns (success, message)
        """
        if var_id not in self.conditions:
            return False, f"Variable ID {var_id} not found"

        ic = self.conditions[var_id]
        old_value = ic.value
        ic.value = new_value

        # Update the raw line
        new_line = ic.format_line()
        self.raw_lines[ic.line_number - 1] = new_line + '\n'

        logger.info(f"Updated {ic.name}: {old_value} -> {new_value}")
        return True, f"Updated {ic.name}"

    def update_conditions(self, updates: dict[int, float]) -> tuple[int, int, list[str]]:
        """
        Update multiple ICs.
        Returns (success_count, fail_count, messages)
        """
        success_count = 0
        fail_count = 0
        messages = []

        for var_id, new_value in updates.items():
            success, msg = self.update_condition(var_id, new_value)
            if success:
                success_count += 1
            else:
                fail_count += 1
            messages.append(msg)

        return success_count, fail_count, messages

    def save(self, backup: bool = True) -> tuple[bool, str]:
        """
        Save the IC file.
        Returns (success, message)
        """
        try:
            # Create backup if requested
            if backup and os.path.exists(self.filepath):
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                backup_path = f"{self.filepath}.{timestamp}.bak"
                shutil.copy(self.filepath, backup_path)
                logger.info(f"Backup created: {backup_path}")

            # Write updated content
            with open(self.filepath, 'w') as f:
                f.writelines(self.raw_lines)

            logger.info(f"Saved IC file: {self.filepath}")
            return True, "Saved successfully"

        except Exception as e:
            logger.error(f"Error saving IC file: {e}")
            return False, f"Save failed: {e}"

    def get_summary(self) -> dict:
        """Get summary statistics"""
        if not self._parsed:
            self.parse()

        category_counts = {}
        for category, var_ids in STATE_VARIABLE_CATEGORIES.items():
            count = sum(1 for vid in var_ids if vid in self.conditions)
            category_counts[category] = count

        return {
            "total_conditions": len(self.conditions),
            "categories": len(STATE_VARIABLE_CATEGORIES),
            "category_counts": category_counts,
            "filepath": self.filepath,
        }


def get_available_ic_files(inputs_dir: str) -> list[str]:
    """Get list of available IC files"""
    ic_files = []
    if os.path.exists(inputs_dir):
        for f in os.listdir(inputs_dir):
            if f.startswith("INIT_CONC_") and f.endswith(".txt"):
                ic_files.append(f)
    return sorted(ic_files)


def load_ic_file(filepath: str) -> ICFile:
    """Load and parse an IC file"""
    ic = ICFile(filepath)
    ic.parse()
    return ic


# Test function
def test_ic_parser():
    """Test the IC parser"""

    script_dir = os.path.dirname(os.path.realpath(__file__))
    root_dir = os.path.dirname(script_dir)
    inputs_dir = os.path.join(root_dir, "INPUTS")

    # Get available IC files
    ic_files = get_available_ic_files(inputs_dir)
    print(f"Found IC files: {ic_files}")
    print("=" * 60)

    if not ic_files:
        print("No IC files found")
        return

    # Test first IC file
    test_file = os.path.join(inputs_dir, ic_files[0])
    print(f"Testing IC parser with: {test_file}")
    print("=" * 60)

    ic = load_ic_file(test_file)
    summary = ic.get_summary()

    print(f"Total conditions: {summary['total_conditions']}")
    print(f"Categories: {summary['categories']}")
    print()

    print("Conditions by category:")
    for category, count in summary['category_counts'].items():
        print(f"  {category}: {count}")
    print()

    # Show sample conditions
    print("Sample conditions from 'Nutrients' category:")
    nutrient_ics = ic.get_conditions_by_category("Nutrients")
    for cond in nutrient_ics:
        print(f"  {cond.var_id:2d}. {cond.name}: {cond.value} {cond.units} ({cond.description})")
    print()

    print("All conditions:")
    for cond in ic.get_all_conditions():
        print(f"  {cond.var_id:2d}. {cond.name:20s}: {cond.value:12.6g} {cond.units}")

    print()
    print("Test complete!")


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    test_ic_parser()
