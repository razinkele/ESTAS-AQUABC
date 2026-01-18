"""
AQUABC Model Options Parser Module

Parses and manages AQUABC model options files:
- PELAGIC_MODEL_OPTIONS.txt: Boolean switches for model behavior
- EXTRA_WCONST.txt: Additional model constants
"""

import os
import shutil
import logging
from datetime import datetime
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple, Union

logger = logging.getLogger("AQUABC.options")

# Model option definitions with descriptions and types
MODEL_OPTIONS = {
    "ZOOPLANKTON_OPTION": {
        "description": "Zooplankton CNP partitioning",
        "help": "If 0: unrealistic zooplankton CNP partitioning",
        "type": "bool",
        "default": 1
    },
    "ADVANCED_REDOX_SIMULATION": {
        "description": "Advanced Redox Simulation",
        "help": "If 0: not full redox cycle with Mn, Fe, SO4, methane",
        "type": "bool",
        "default": 0
    },
    "LIGHT_EXTINCTION_OPTION": {
        "description": "Light Extinction Option",
        "help": "If 0: empiric equation (Curonian specific)",
        "type": "bool",
        "default": 0
    },
    "CYANO_BOUYANT_STATE_SIMULATION": {
        "description": "Cyanobacteria Buoyancy Simulation",
        "help": "Enable buoyant state simulation for cyanobacteria",
        "type": "bool",
        "default": 1
    },
    "CONSIDER_NON_OBLIGATORY_FIXERS": {
        "description": "Non-obligatory N Fixers",
        "help": "Consider non-obligatory nitrogen fixing cyanobacteria",
        "type": "bool",
        "default": 0
    },
    "CONSIDER_NON": {
        "description": "Non-obligatory N Fixers",
        "help": "Consider non-obligatory nitrogen fixing cyanobacteria",
        "type": "bool",
        "default": 0
    },
    "CONSIDER_HETEROCYST_WITH_AKINETES": {
        "description": "Heterocyst with Akinetes",
        "help": "Consider heterocyst formation with akinetes",
        "type": "bool",
        "default": 1
    },
    "CONSIDER_HETEROCYST": {
        "description": "Heterocyst with Akinetes",
        "help": "Consider heterocyst formation with akinetes",
        "type": "bool",
        "default": 1
    },
    "CONSIDER_ALLELOPATHY": {
        "description": "Allelopathy Simulation",
        "help": "Consider allelopathic interactions between species",
        "type": "bool",
        "default": 1
    },
    "CYN_ALLELOPATHY_FILE_NAME": {
        "description": "Allelopathy Data File",
        "help": "Filename containing allelopathic information",
        "type": "string",
        "default": "ALLELOPATHIC_INFORMATION.txt"
    },
}

# Extra constants definitions
EXTRA_CONSTANTS = {
    "USER_ENTERED_frac_avail_DON": {
        "description": "Fraction Available DON",
        "help": "User-entered fraction of available dissolved organic nitrogen",
        "type": "float",
        "default": 0.0,
        "min": 0.0,
        "max": 1.0
    },
    "K_B_E": {
        "description": "Background Light Extinction",
        "help": "Background light extinction coefficient (1/m)",
        "type": "float",
        "default": 0.70,
        "min": 0.0,
        "max": 5.0
    },
}

# Group options into categories for UI
OPTION_CATEGORIES = {
    "Zooplankton": ["ZOOPLANKTON_OPTION"],
    "Redox Chemistry": ["ADVANCED_REDOX_SIMULATION"],
    "Light": ["LIGHT_EXTINCTION_OPTION", "K_B_E"],
    "Cyanobacteria": [
        "CYANO_BOUYANT_STATE_SIMULATION",
        "CONSIDER_NON_OBLIGATORY_FIXERS",
        "CONSIDER_NON",
        "CONSIDER_HETEROCYST_WITH_AKINETES",
        "CONSIDER_HETEROCYST"
    ],
    "Allelopathy": [
        "CONSIDER_ALLELOPATHY",
        "CYN_ALLELOPATHY_FILE_NAME"
    ],
    "Organic Matter": ["USER_ENTERED_frac_avail_DON"],
}


@dataclass
class ModelOption:
    """Individual model option with metadata"""
    name: str
    value: Union[int, float, str]
    description: str
    help_text: str
    option_type: str  # 'bool', 'float', 'string'
    line_number: int
    original_line: str

    # Validation bounds (for numeric types)
    min_value: Optional[float] = None
    max_value: Optional[float] = None
    default_value: Union[int, float, str, None] = None

    def is_boolean(self) -> bool:
        return self.option_type == "bool"

    def is_numeric(self) -> bool:
        return self.option_type in ("bool", "float")

    def validate(self, new_value) -> Tuple[bool, str]:
        """Validate a new value"""
        if self.option_type == "bool":
            if new_value not in (0, 1, True, False):
                return False, "Boolean option must be 0 or 1"
        elif self.option_type == "float":
            try:
                val = float(new_value)
                if self.min_value is not None and val < self.min_value:
                    return False, f"Value below minimum ({self.min_value})"
                if self.max_value is not None and val > self.max_value:
                    return False, f"Value above maximum ({self.max_value})"
            except ValueError:
                return False, "Invalid numeric value"
        return True, "OK"

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization"""
        return {
            "name": self.name,
            "value": self.value,
            "description": self.description,
            "help": self.help_text,
            "type": self.option_type,
            "min_value": self.min_value,
            "max_value": self.max_value,
            "default_value": self.default_value,
        }


class ModelOptionsFile:
    """Parse and manage PELAGIC_MODEL_OPTIONS.txt"""

    def __init__(self, filepath: str):
        self.filepath = filepath
        self.options: Dict[str, ModelOption] = {}
        self.raw_lines: List[str] = []
        self._parsed = False

    def parse(self) -> bool:
        """Parse the options file"""
        if not os.path.exists(self.filepath):
            logger.error(f"Options file not found: {self.filepath}")
            return False

        try:
            with open(self.filepath, 'r') as f:
                self.raw_lines = f.readlines()

            self.options = {}
            current_comment = ""
            current_name = ""

            for line_num, line in enumerate(self.raw_lines, 1):
                stripped = line.strip()

                # Comment line contains option name
                if stripped.startswith('#'):
                    # Extract option name from comment
                    comment_text = stripped[1:].strip()
                    current_comment = comment_text

                    # Try to extract option name
                    # First check if comment matches a known option name pattern
                    normalized_comment = comment_text.upper().replace(' ', '_').replace('-', '_')

                    # Try to match against known MODEL_OPTIONS keys
                    matched_name = None
                    for known_name in MODEL_OPTIONS.keys():
                        # Check if the normalized comment starts with or contains the known name
                        if normalized_comment.startswith(known_name) or known_name in normalized_comment:
                            matched_name = known_name
                            break

                    if matched_name:
                        current_name = matched_name
                    else:
                        # Fall back to extracting from comment
                        if ' if ' in comment_text.lower():
                            current_name = comment_text.split(' if ')[0].strip()
                        elif ' - ' in comment_text:
                            current_name = comment_text.split(' - ')[0].strip()
                        else:
                            # Take the whole comment as the name (normalized)
                            current_name = normalized_comment

                    # Normalize name
                    current_name = current_name.upper().replace(' ', '_').replace('-', '_')
                    continue

                # Value line
                if stripped and current_name:
                    # Determine value type
                    try:
                        # Try integer first (for boolean options)
                        value = int(stripped)
                        opt_type = "bool"
                    except ValueError:
                        try:
                            value = float(stripped)
                            opt_type = "float"
                        except ValueError:
                            value = stripped
                            opt_type = "string"

                    # Get metadata from definitions
                    meta = MODEL_OPTIONS.get(current_name, {})

                    option = ModelOption(
                        name=current_name,
                        value=value,
                        description=meta.get("description", current_comment),
                        help_text=meta.get("help", current_comment),
                        option_type=meta.get("type", opt_type),
                        line_number=line_num,
                        original_line=line,
                        min_value=meta.get("min"),
                        max_value=meta.get("max"),
                        default_value=meta.get("default")
                    )
                    self.options[current_name] = option
                    current_name = ""
                    current_comment = ""

            self._parsed = True
            logger.info(f"Parsed {len(self.options)} model options from {os.path.basename(self.filepath)}")
            return True

        except Exception as e:
            logger.error(f"Error parsing options file: {e}")
            return False

    def get_option(self, name: str) -> Optional[ModelOption]:
        """Get a specific option"""
        if not self._parsed:
            self.parse()
        return self.options.get(name)

    def get_all_options(self) -> List[ModelOption]:
        """Get all options"""
        if not self._parsed:
            self.parse()
        return list(self.options.values())

    def update_option(self, name: str, new_value) -> Tuple[bool, str]:
        """Update an option value"""
        if name not in self.options:
            return False, f"Option '{name}' not found"

        opt = self.options[name]

        # Validate
        is_valid, msg = opt.validate(new_value)
        if not is_valid:
            return False, msg

        # Convert value
        if opt.option_type == "bool":
            new_value = int(new_value) if isinstance(new_value, bool) else int(new_value)
        elif opt.option_type == "float":
            new_value = float(new_value)

        old_value = opt.value
        opt.value = new_value

        # Update raw line
        self.raw_lines[opt.line_number - 1] = f"            {new_value}\n"

        logger.info(f"Updated {name}: {old_value} -> {new_value}")
        return True, f"Updated {name}"

    def save(self, backup: bool = True) -> Tuple[bool, str]:
        """Save the options file"""
        try:
            if backup and os.path.exists(self.filepath):
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                backup_path = f"{self.filepath}.{timestamp}.bak"
                shutil.copy(self.filepath, backup_path)
                logger.info(f"Backup created: {backup_path}")

            with open(self.filepath, 'w') as f:
                f.writelines(self.raw_lines)

            logger.info(f"Saved options file: {self.filepath}")
            return True, "Saved successfully"

        except Exception as e:
            logger.error(f"Error saving options file: {e}")
            return False, f"Save failed: {e}"

    def get_summary(self) -> dict:
        """Get summary of options"""
        if not self._parsed:
            self.parse()

        bool_count = sum(1 for o in self.options.values() if o.option_type == "bool")
        enabled_count = sum(1 for o in self.options.values()
                          if o.option_type == "bool" and o.value == 1)

        return {
            "total_options": len(self.options),
            "boolean_options": bool_count,
            "enabled_options": enabled_count,
            "filepath": self.filepath,
        }


class ExtraConstantsFile:
    """Parse and manage EXTRA_WCONST.txt"""

    def __init__(self, filepath: str):
        self.filepath = filepath
        self.constants: Dict[str, ModelOption] = {}
        self.raw_lines: List[str] = []
        self._parsed = False

    def parse(self) -> bool:
        """Parse the extra constants file"""
        if not os.path.exists(self.filepath):
            logger.error(f"Extra constants file not found: {self.filepath}")
            return False

        try:
            with open(self.filepath, 'r') as f:
                self.raw_lines = f.readlines()

            self.constants = {}
            current_name = ""
            current_comment = ""

            for line_num, line in enumerate(self.raw_lines, 1):
                stripped = line.strip()

                # Comment line contains constant name
                if stripped.startswith('#'):
                    current_comment = stripped[1:].strip()
                    # Extract name (usually the whole comment or first token)
                    current_name = current_comment.split()[0] if current_comment.split() else ""
                    # Clean up name
                    current_name = current_name.rstrip(' -')
                    continue

                # Value line
                if stripped and current_name:
                    try:
                        value = float(stripped)
                    except ValueError:
                        value = stripped

                    # Get metadata from definitions
                    meta = EXTRA_CONSTANTS.get(current_name, {})

                    const = ModelOption(
                        name=current_name,
                        value=value,
                        description=meta.get("description", current_comment),
                        help_text=meta.get("help", current_comment),
                        option_type=meta.get("type", "float"),
                        line_number=line_num,
                        original_line=line,
                        min_value=meta.get("min"),
                        max_value=meta.get("max"),
                        default_value=meta.get("default")
                    )
                    self.constants[current_name] = const
                    current_name = ""
                    current_comment = ""

            self._parsed = True
            logger.info(f"Parsed {len(self.constants)} extra constants from {os.path.basename(self.filepath)}")
            return True

        except Exception as e:
            logger.error(f"Error parsing extra constants file: {e}")
            return False

    def get_constant(self, name: str) -> Optional[ModelOption]:
        """Get a specific constant"""
        if not self._parsed:
            self.parse()
        return self.constants.get(name)

    def get_all_constants(self) -> List[ModelOption]:
        """Get all constants"""
        if not self._parsed:
            self.parse()
        return list(self.constants.values())

    def update_constant(self, name: str, new_value: float) -> Tuple[bool, str]:
        """Update a constant value"""
        if name not in self.constants:
            return False, f"Constant '{name}' not found"

        const = self.constants[name]

        # Validate
        is_valid, msg = const.validate(new_value)
        if not is_valid:
            logger.warning(f"Validation warning for {name}: {msg}")

        old_value = const.value
        const.value = float(new_value)

        # Update raw line
        self.raw_lines[const.line_number - 1] = f"{new_value}\n"

        logger.info(f"Updated {name}: {old_value} -> {new_value}")
        return True, f"Updated {name}"

    def save(self, backup: bool = True) -> Tuple[bool, str]:
        """Save the constants file"""
        try:
            if backup and os.path.exists(self.filepath):
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                backup_path = f"{self.filepath}.{timestamp}.bak"
                shutil.copy(self.filepath, backup_path)
                logger.info(f"Backup created: {backup_path}")

            with open(self.filepath, 'w') as f:
                f.writelines(self.raw_lines)

            logger.info(f"Saved constants file: {self.filepath}")
            return True, "Saved successfully"

        except Exception as e:
            logger.error(f"Error saving constants file: {e}")
            return False, f"Save failed: {e}"


def load_model_options(filepath: str) -> ModelOptionsFile:
    """Load and parse model options file"""
    mof = ModelOptionsFile(filepath)
    mof.parse()
    return mof


def load_extra_constants(filepath: str) -> ExtraConstantsFile:
    """Load and parse extra constants file"""
    ecf = ExtraConstantsFile(filepath)
    ecf.parse()
    return ecf


# Test function
def test_options_parser():
    """Test the options parser"""
    import sys

    script_dir = os.path.dirname(os.path.realpath(__file__))
    root_dir = os.path.dirname(script_dir)
    inputs_dir = os.path.join(root_dir, "INPUTS")

    # Test PELAGIC_MODEL_OPTIONS.txt
    options_file = os.path.join(inputs_dir, "PELAGIC_MODEL_OPTIONS.txt")
    print(f"Testing options parser with: {options_file}")
    print("=" * 60)

    if os.path.exists(options_file):
        mof = load_model_options(options_file)
        summary = mof.get_summary()
        print(f"Total options: {summary['total_options']}")
        print(f"Boolean options: {summary['boolean_options']}")
        print(f"Enabled: {summary['enabled_options']}")
        print()

        print("All options:")
        for opt in mof.get_all_options():
            status = "ON" if opt.value == 1 else "OFF" if opt.value == 0 else opt.value
            print(f"  {opt.name}: {status} - {opt.description}")
        print()
    else:
        print(f"Options file not found: {options_file}")

    # Test EXTRA_WCONST.txt
    extra_file = os.path.join(inputs_dir, "EXTRA_WCONST.txt")
    print(f"Testing extra constants parser with: {extra_file}")
    print("=" * 60)

    if os.path.exists(extra_file):
        ecf = load_extra_constants(extra_file)
        print(f"Total constants: {len(ecf.constants)}")
        print()

        print("Sample constants:")
        for const in list(ecf.get_all_constants())[:5]:
            print(f"  {const.name}: {const.value} - {const.description}")
        print()
    else:
        print(f"Extra constants file not found: {extra_file}")

    print("Test complete!")


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    test_options_parser()
