"""
AQUABC Parameter Parser Module

Parses and manages AQUABC model parameter files (WCONST_04.txt, EXTRA_WCONST.txt).
Provides structured access to 318+ model parameters organized by category.
"""

import os
import re
import shutil
import logging
from datetime import datetime
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple

logger = logging.getLogger("AQUABC.params")

# Parameter categories with their line ranges (1-indexed, inclusive)
PARAMETER_CATEGORIES = {
    "General": (1, 4),
    "Diatoms": (5, 28),
    "Non-fixing Cyanobacteria": (29, 50),
    "Fixing Cyanobacteria": (51, 74),
    "Other Phytoplankton": (75, 96),
    "Zooplankton": (97, 133),
    "Detritus": (134, 146),
    "Dissolved Organics": (147, 151),
    "Nitrification": (152, 157),
    "Redox Chemistry": (158, 209),
    "Methane": (210, 234),
    "Settling": (235, 250),
    "pH Effects": (251, 266),
    "Nostocales": (267, 318),
}

# Parameter validation rules (parameter_name_pattern -> (min, max, description))
VALIDATION_RULES = {
    r"^KG_.*": (0.0, 10.0, "Growth rate"),
    r"^KR_.*": (0.0, 1.0, "Respiration rate"),
    r"^KD_.*": (0.0, 1.0, "Mortality rate"),
    r"^THETA_.*": (1.0, 1.2, "Temperature coefficient"),
    r"^KHS_.*": (0.0, 100.0, "Half-saturation constant"),
    r"^PREF_.*": (0.0, 1.0, "Preference coefficient"),
    r"^GRAT_.*": (0.0, 5.0, "Grazing rate"),
    r"^FRAC_.*": (0.0, 1.0, "Fraction"),
    r".*_N_TO_C$": (0.05, 0.5, "N:C ratio"),
    r".*_P_TO_C$": (0.005, 0.1, "P:C ratio"),
    r".*_O2_TO_C$": (1.0, 5.0, "O2:C ratio"),
    r".*_C_TO_CHLA$": (10.0, 100.0, "C:Chl-a ratio"),
    r"^EFF_.*": (0.5, 1.0, "Efficiency"),
    r"^I_S_.*": (10.0, 500.0, "Light saturation"),
    r".*_OPT_TEMP_LR$": (0.0, 30.0, "Lower optimal temperature"),
    r".*_OPT_TEMP_UR$": (10.0, 40.0, "Upper optimal temperature"),
}


@dataclass
class Parameter:
    """Individual model parameter with metadata"""
    id: int
    name: str
    value: float
    comment: str
    line_number: int
    original_line: str
    category: str = ""

    # Validation info
    min_value: Optional[float] = None
    max_value: Optional[float] = None
    units: str = ""

    def __post_init__(self):
        """Extract units and set validation bounds"""
        self._parse_units()
        self._set_validation_bounds()

    def _parse_units(self):
        """Try to extract units from comment"""
        # Common unit patterns
        unit_patterns = [
            r'\(([^)]+)\)',  # Text in parentheses
            r'(\d+/day)',
            r'(mg/[Ll])',
            r'(langleys)',
        ]
        for pattern in unit_patterns:
            match = re.search(pattern, self.comment)
            if match:
                self.units = match.group(1)
                break

    def _set_validation_bounds(self):
        """Set min/max bounds based on parameter name patterns"""
        for pattern, (min_val, max_val, _) in VALIDATION_RULES.items():
            if re.match(pattern, self.name):
                self.min_value = min_val
                self.max_value = max_val
                break

    def validate(self, new_value: float) -> Tuple[bool, str]:
        """
        Validate a new value for this parameter.
        Returns (is_valid, message)
        """
        if self.min_value is not None and new_value < self.min_value:
            return False, f"Value {new_value} below minimum {self.min_value}"
        if self.max_value is not None and new_value > self.max_value:
            return False, f"Value {new_value} above maximum {self.max_value}"
        return True, "OK"

    def format_line(self) -> str:
        """Format parameter back to file line format"""
        # Format: "    ID    NAME    VALUE  ! ID   Comment"
        # Maintain original spacing as much as possible
        return f"{self.id:6d}{self.name:>35s}{self.value:18.6g}  ! {self.id:3d}   {self.comment}"

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization"""
        return {
            "id": self.id,
            "name": self.name,
            "value": self.value,
            "comment": self.comment,
            "category": self.category,
            "min_value": self.min_value,
            "max_value": self.max_value,
            "units": self.units,
        }


class ParameterFile:
    """Parse and manage AQUABC parameter files"""

    def __init__(self, filepath: str):
        self.filepath = filepath
        self.parameters: Dict[int, Parameter] = {}  # id -> Parameter
        self.raw_lines: List[str] = []  # Original file lines
        self._parsed = False

    def parse(self) -> bool:
        """
        Parse the parameter file.
        Returns True if successful, False otherwise.
        """
        if not os.path.exists(self.filepath):
            logger.error(f"Parameter file not found: {self.filepath}")
            return False

        try:
            with open(self.filepath, 'r') as f:
                self.raw_lines = f.readlines()

            self.parameters = {}

            for line_num, line in enumerate(self.raw_lines, 1):
                param = self._parse_line(line, line_num)
                if param:
                    # Assign category based on parameter ID
                    param.category = self._get_category(param.id)
                    self.parameters[param.id] = param

            self._parsed = True
            logger.info(f"Parsed {len(self.parameters)} parameters from {os.path.basename(self.filepath)}")
            return True

        except Exception as e:
            logger.error(f"Error parsing parameter file: {e}")
            return False

    def _parse_line(self, line: str, line_num: int) -> Optional[Parameter]:
        """
        Parse a single line of the parameter file.
        Format: "    ID    NAME    VALUE  ! ID   Comment"
        """
        line = line.rstrip('\n\r')

        # Skip empty lines or comment-only lines
        if not line.strip() or line.strip().startswith('#'):
            return None

        # Split on '!' to separate value part from comment
        if '!' not in line:
            return None

        value_part, comment_part = line.split('!', 1)

        # Parse value part: "    ID    NAME    VALUE"
        tokens = value_part.split()
        if len(tokens) < 3:
            return None

        try:
            param_id = int(tokens[0])
            param_name = tokens[1]
            param_value = float(tokens[2])
        except (ValueError, IndexError):
            return None

        # Parse comment part: " ID   Comment text"
        comment = comment_part.strip()
        # Remove leading ID number from comment if present
        comment_match = re.match(r'\s*\d+\s*(.*)', comment)
        if comment_match:
            comment = comment_match.group(1).strip()

        return Parameter(
            id=param_id,
            name=param_name,
            value=param_value,
            comment=comment,
            line_number=line_num,
            original_line=line
        )

    def _get_category(self, param_id: int) -> str:
        """Get category name for a parameter ID"""
        for category, (start, end) in PARAMETER_CATEGORIES.items():
            if start <= param_id <= end:
                return category
        return "Other"

    def get_parameters_by_category(self, category: str) -> List[Parameter]:
        """Get all parameters in a category"""
        if not self._parsed:
            self.parse()

        return [p for p in self.parameters.values() if p.category == category]

    def get_parameter(self, param_id: int) -> Optional[Parameter]:
        """Get a specific parameter by ID"""
        if not self._parsed:
            self.parse()
        return self.parameters.get(param_id)

    def get_parameter_by_name(self, name: str) -> Optional[Parameter]:
        """Get a specific parameter by name"""
        if not self._parsed:
            self.parse()
        for param in self.parameters.values():
            if param.name == name:
                return param
        return None

    def update_parameter(self, param_id: int, new_value: float) -> Tuple[bool, str]:
        """
        Update a parameter value.
        Returns (success, message)
        """
        if param_id not in self.parameters:
            return False, f"Parameter ID {param_id} not found"

        param = self.parameters[param_id]

        # Validate
        is_valid, msg = param.validate(new_value)
        if not is_valid:
            logger.warning(f"Validation warning for {param.name}: {msg}")
            # Still allow update but log warning

        # Update value
        old_value = param.value
        param.value = new_value

        # Update the raw line
        new_line = param.format_line()
        self.raw_lines[param.line_number - 1] = new_line + '\n'

        logger.info(f"Updated {param.name}: {old_value} -> {new_value}")
        return True, f"Updated {param.name}"

    def update_parameters(self, updates: Dict[int, float]) -> Tuple[int, int, List[str]]:
        """
        Update multiple parameters.
        Returns (success_count, fail_count, messages)
        """
        success_count = 0
        fail_count = 0
        messages = []

        for param_id, new_value in updates.items():
            success, msg = self.update_parameter(param_id, new_value)
            if success:
                success_count += 1
            else:
                fail_count += 1
            messages.append(msg)

        return success_count, fail_count, messages

    def save(self, backup: bool = True) -> Tuple[bool, str]:
        """
        Save the parameter file.
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

            logger.info(f"Saved parameter file: {self.filepath}")
            return True, f"Saved successfully"

        except Exception as e:
            logger.error(f"Error saving parameter file: {e}")
            return False, f"Save failed: {e}"

    def get_all_categories(self) -> List[str]:
        """Get list of all category names"""
        return list(PARAMETER_CATEGORIES.keys())

    def get_category_count(self, category: str) -> int:
        """Get number of parameters in a category"""
        if category in PARAMETER_CATEGORIES:
            start, end = PARAMETER_CATEGORIES[category]
            return end - start + 1
        return 0

    def search(self, query: str) -> List[Parameter]:
        """Search parameters by name or comment"""
        if not self._parsed:
            self.parse()

        query = query.lower()
        results = []

        for param in self.parameters.values():
            if query in param.name.lower() or query in param.comment.lower():
                results.append(param)

        return results

    def to_dataframe(self):
        """Convert parameters to pandas DataFrame"""
        import pandas as pd

        if not self._parsed:
            self.parse()

        data = [p.to_dict() for p in self.parameters.values()]
        return pd.DataFrame(data)

    def get_summary(self) -> dict:
        """Get summary statistics"""
        if not self._parsed:
            self.parse()

        category_counts = {}
        for category in PARAMETER_CATEGORIES:
            category_counts[category] = len(self.get_parameters_by_category(category))

        return {
            "total_parameters": len(self.parameters),
            "categories": len(PARAMETER_CATEGORIES),
            "category_counts": category_counts,
            "filepath": self.filepath,
        }


# Convenience function for quick parameter loading
def load_parameters(filepath: str) -> ParameterFile:
    """Load and parse a parameter file"""
    pf = ParameterFile(filepath)
    pf.parse()
    return pf


# Test function
def test_parser():
    """Test the parameter parser"""
    import sys

    # Get the INPUTS directory relative to this file
    script_dir = os.path.dirname(os.path.realpath(__file__))
    root_dir = os.path.dirname(script_dir)
    test_file = os.path.join(root_dir, "INPUTS", "WCONST_04.txt")

    if not os.path.exists(test_file):
        print(f"Test file not found: {test_file}")
        return

    print(f"Testing parameter parser with: {test_file}")
    print("=" * 60)

    pf = load_parameters(test_file)
    summary = pf.get_summary()

    print(f"Total parameters: {summary['total_parameters']}")
    print(f"Categories: {summary['categories']}")
    print()

    print("Parameters by category:")
    for category, count in summary['category_counts'].items():
        print(f"  {category}: {count}")
    print()

    # Test getting parameters by category
    print("Sample parameters from 'Diatoms' category:")
    diatom_params = pf.get_parameters_by_category("Diatoms")
    for p in diatom_params[:5]:
        print(f"  {p.id:3d}. {p.name}: {p.value} ({p.comment[:50]}...)")
    print()

    # Test search
    print("Search for 'growth':")
    results = pf.search("growth")
    for p in results[:5]:
        print(f"  {p.name}: {p.value}")
    print()

    print("Test complete!")


if __name__ == "__main__":
    # Configure logging for testing
    logging.basicConfig(level=logging.DEBUG)
    test_parser()
