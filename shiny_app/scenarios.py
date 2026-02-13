"""
Scenario Management for AQUABC

Handles saving and loading of named scenario presets that capture:
- Model parameters (WCONST_04.txt)
- Initial conditions (INIT_CONC_*.txt)
- Model options (PELAGIC_MODEL_OPTIONS.txt)
- Extra constants (EXTRA_WCONST.txt)

Author: Claude
Date: 2026-01-17
"""

import json
import logging
import os
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger("AQUABC.Scenarios")


@dataclass
class Scenario:
    """Represents a complete model configuration scenario"""
    name: str
    description: str = ""
    created: str = ""  # ISO format datetime string
    modified: str = ""  # ISO format datetime string
    version: str = "1.0"

    # Configuration data
    parameters: dict[str, float] = field(default_factory=dict)  # param_id (as str) -> value
    initial_conditions: dict[str, float] = field(default_factory=dict)  # var_id (as str) -> value
    model_options: dict[str, Any] = field(default_factory=dict)  # option_name -> value
    extra_constants: dict[str, float] = field(default_factory=dict)  # constant_name -> value

    # Metadata
    ic_file: str = "INIT_CONC_1.txt"  # Which IC file this applies to
    is_builtin: bool = False  # Built-in presets are read-only

    def __post_init__(self):
        """Set timestamps if not provided"""
        now = datetime.now().isoformat()
        if not self.created:
            self.created = now
        if not self.modified:
            self.modified = now

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization"""
        return {
            "name": self.name,
            "description": self.description,
            "created": self.created,
            "modified": self.modified,
            "version": self.version,
            "ic_file": self.ic_file,
            "is_builtin": self.is_builtin,
            "parameters": self.parameters,
            "initial_conditions": self.initial_conditions,
            "model_options": self.model_options,
            "extra_constants": self.extra_constants
        }

    @classmethod
    def from_dict(cls, data: dict) -> 'Scenario':
        """Create Scenario from dictionary"""
        return cls(
            name=data.get("name", "Unnamed"),
            description=data.get("description", ""),
            created=data.get("created", ""),
            modified=data.get("modified", ""),
            version=data.get("version", "1.0"),
            ic_file=data.get("ic_file", "INIT_CONC_1.txt"),
            is_builtin=data.get("is_builtin", False),
            parameters=data.get("parameters", {}),
            initial_conditions=data.get("initial_conditions", {}),
            model_options=data.get("model_options", {}),
            extra_constants=data.get("extra_constants", {})
        )

    def get_summary(self) -> str:
        """Get a brief summary of what the scenario contains"""
        parts = []
        if self.parameters:
            parts.append(f"{len(self.parameters)} parameters")
        if self.initial_conditions:
            parts.append(f"{len(self.initial_conditions)} initial conditions")
        if self.model_options:
            parts.append(f"{len(self.model_options)} model options")
        if self.extra_constants:
            parts.append(f"{len(self.extra_constants)} extra constants")
        return ", ".join(parts) if parts else "Empty scenario"


class ScenarioManager:
    """Manages scenario files and operations"""

    def __init__(self, scenarios_dir: str, inputs_dir: str):
        """
        Initialize scenario manager

        Args:
            scenarios_dir: Directory containing scenario JSON files
            inputs_dir: Directory containing model input files (WCONST_04.txt, etc.)
        """
        self.scenarios_dir = scenarios_dir
        self.inputs_dir = inputs_dir
        self._scenarios: dict[str, Scenario] = {}
        self._ensure_dir_exists()
        self._load_all_scenarios()

    def _ensure_dir_exists(self):
        """Create scenarios directory if it doesn't exist"""
        if not os.path.exists(self.scenarios_dir):
            os.makedirs(self.scenarios_dir)
            logger.info(f"Created scenarios directory: {self.scenarios_dir}")

    def _load_all_scenarios(self):
        """Load all scenario files from directory"""
        self._scenarios = {}

        if not os.path.exists(self.scenarios_dir):
            logger.warning(f"Scenarios directory not found: {self.scenarios_dir}")
            return

        for filename in os.listdir(self.scenarios_dir):
            if filename.endswith('.json'):
                filepath = os.path.join(self.scenarios_dir, filename)
                try:
                    with open(filepath) as f:
                        data = json.load(f)
                    scenario = Scenario.from_dict(data)
                    self._scenarios[scenario.name] = scenario
                    logger.debug(f"Loaded scenario: {scenario.name}")
                except Exception as e:
                    logger.error(f"Failed to load scenario {filename}: {e}")

        logger.info(f"Loaded {len(self._scenarios)} scenarios")

    def refresh(self):
        """Reload all scenarios from disk"""
        self._load_all_scenarios()

    def list_scenarios(self) -> list[Scenario]:
        """Get list of all available scenarios"""
        # Sort: built-ins first, then alphabetically
        scenarios = list(self._scenarios.values())
        scenarios.sort(key=lambda s: (not s.is_builtin, s.name.lower()))
        return scenarios

    def get_scenario_names(self) -> list[str]:
        """Get list of scenario names"""
        return [s.name for s in self.list_scenarios()]

    def get_scenario(self, name: str) -> Scenario | None:
        """Get a specific scenario by name"""
        return self._scenarios.get(name)

    def save_scenario(self, scenario: Scenario) -> tuple[bool, str]:
        """
        Save a scenario to file

        Args:
            scenario: Scenario to save

        Returns:
            Tuple of (success, message)
        """
        if scenario.is_builtin:
            return False, "Cannot overwrite built-in scenarios"

        # Update modified timestamp
        scenario.modified = datetime.now().isoformat()

        # Generate filename from scenario name
        safe_name = "".join(c if c.isalnum() or c in "._- " else "_" for c in scenario.name)
        safe_name = safe_name.strip().replace(" ", "_").lower()
        filename = f"{safe_name}.json"
        filepath = os.path.join(self.scenarios_dir, filename)

        try:
            with open(filepath, 'w') as f:
                json.dump(scenario.to_dict(), f, indent=2)

            self._scenarios[scenario.name] = scenario
            logger.info(f"Saved scenario: {scenario.name} to {filepath}")
            return True, f"Saved scenario '{scenario.name}'"

        except Exception as e:
            logger.error(f"Failed to save scenario {scenario.name}: {e}")
            return False, f"Failed to save: {e}"

    def delete_scenario(self, name: str) -> tuple[bool, str]:
        """
        Delete a scenario

        Args:
            name: Name of scenario to delete

        Returns:
            Tuple of (success, message)
        """
        scenario = self._scenarios.get(name)

        if not scenario:
            return False, f"Scenario '{name}' not found"

        if scenario.is_builtin:
            return False, "Cannot delete built-in scenarios"

        # Find and delete the file
        safe_name = "".join(c if c.isalnum() or c in "._- " else "_" for c in name)
        safe_name = safe_name.strip().replace(" ", "_").lower()
        filename = f"{safe_name}.json"
        filepath = os.path.join(self.scenarios_dir, filename)

        try:
            if os.path.exists(filepath):
                os.remove(filepath)

            del self._scenarios[name]
            logger.info(f"Deleted scenario: {name}")
            return True, f"Deleted scenario '{name}'"

        except Exception as e:
            logger.error(f"Failed to delete scenario {name}: {e}")
            return False, f"Failed to delete: {e}"

    def capture_current_state(
        self,
        name: str,
        description: str = "",
        include_params: bool = True,
        include_ics: bool = True,
        include_options: bool = True,
        ic_file: str = "INIT_CONC_1.txt"
    ) -> tuple[Scenario | None, str]:
        """
        Capture current model configuration as a new scenario

        Args:
            name: Name for the new scenario
            description: Optional description
            include_params: Include parameters from WCONST_04.txt
            include_ics: Include initial conditions
            include_options: Include model options and extra constants
            ic_file: Which IC file to capture

        Returns:
            Tuple of (Scenario or None, message)
        """
        from ic_parser import ICFile
        from options_parser import ExtraConstantsFile, ModelOptionsFile
        from parameter_parser import ParameterFile

        scenario = Scenario(
            name=name,
            description=description,
            ic_file=ic_file,
            is_builtin=False
        )

        errors = []

        # Capture parameters
        if include_params:
            param_path = os.path.join(self.inputs_dir, "WCONST_04.txt")
            if os.path.exists(param_path):
                pf = ParameterFile(param_path)
                if pf.parse():
                    for pid, param in pf.parameters.items():
                        scenario.parameters[str(pid)] = param.value
                    logger.info(f"Captured {len(scenario.parameters)} parameters")
                else:
                    errors.append("Failed to parse WCONST_04.txt")
            else:
                errors.append("WCONST_04.txt not found")

        # Capture initial conditions
        if include_ics:
            ic_path = os.path.join(self.inputs_dir, ic_file)
            if os.path.exists(ic_path):
                ic = ICFile(ic_path)
                if ic.parse():
                    for vid, cond in ic.conditions.items():
                        scenario.initial_conditions[str(vid)] = cond.value
                    logger.info(f"Captured {len(scenario.initial_conditions)} initial conditions")
                else:
                    errors.append(f"Failed to parse {ic_file}")
            else:
                errors.append(f"{ic_file} not found")

        # Capture model options
        if include_options:
            options_path = os.path.join(self.inputs_dir, "PELAGIC_MODEL_OPTIONS.txt")
            if os.path.exists(options_path):
                mof = ModelOptionsFile(options_path)
                if mof.parse():
                    for opt_name, opt in mof.options.items():
                        scenario.model_options[opt_name] = opt.value
                    logger.info(f"Captured {len(scenario.model_options)} model options")
                else:
                    errors.append("Failed to parse PELAGIC_MODEL_OPTIONS.txt")

            extra_path = os.path.join(self.inputs_dir, "EXTRA_WCONST.txt")
            if os.path.exists(extra_path):
                ecf = ExtraConstantsFile(extra_path)
                if ecf.parse():
                    for const_name, const in ecf.constants.items():
                        scenario.extra_constants[const_name] = const.value
                    logger.info(f"Captured {len(scenario.extra_constants)} extra constants")

        if errors:
            return scenario, f"Captured with warnings: {'; '.join(errors)}"

        return scenario, "Configuration captured successfully"

    def apply_scenario(self, scenario: Scenario) -> tuple[bool, str]:
        """
        Apply a scenario to model input files

        Args:
            scenario: Scenario to apply

        Returns:
            Tuple of (success, message)
        """
        from ic_parser import ICFile
        from options_parser import ExtraConstantsFile, ModelOptionsFile
        from parameter_parser import ParameterFile

        results = []
        errors = []

        # Apply parameters
        if scenario.parameters:
            param_path = os.path.join(self.inputs_dir, "WCONST_04.txt")
            if os.path.exists(param_path):
                pf = ParameterFile(param_path)
                if pf.parse():
                    # Convert string keys back to int
                    updates = {int(k): v for k, v in scenario.parameters.items()}
                    success, fail, msgs = pf.update_parameters(updates)
                    save_ok, save_msg = pf.save(backup=True)
                    if save_ok:
                        results.append(f"Updated {success} parameters")
                    else:
                        errors.append(f"Failed to save parameters: {save_msg}")
                else:
                    errors.append("Failed to parse WCONST_04.txt")
            else:
                errors.append("WCONST_04.txt not found")

        # Apply initial conditions
        if scenario.initial_conditions:
            ic_path = os.path.join(self.inputs_dir, scenario.ic_file)
            if os.path.exists(ic_path):
                ic = ICFile(ic_path)
                if ic.parse():
                    # Convert string keys back to int
                    updates = {int(k): v for k, v in scenario.initial_conditions.items()}
                    success, fail, msgs = ic.update_conditions(updates)
                    save_ok, save_msg = ic.save(backup=True)
                    if save_ok:
                        results.append(f"Updated {success} initial conditions")
                    else:
                        errors.append(f"Failed to save ICs: {save_msg}")
                else:
                    errors.append(f"Failed to parse {scenario.ic_file}")
            else:
                errors.append(f"{scenario.ic_file} not found")

        # Apply model options
        if scenario.model_options:
            options_path = os.path.join(self.inputs_dir, "PELAGIC_MODEL_OPTIONS.txt")
            if os.path.exists(options_path):
                mof = ModelOptionsFile(options_path)
                if mof.parse():
                    for opt_name, value in scenario.model_options.items():
                        if opt_name in mof.options:
                            mof.options[opt_name].value = value
                    save_ok, save_msg = mof.save(backup=True)
                    if save_ok:
                        results.append(f"Updated {len(scenario.model_options)} model options")
                    else:
                        errors.append(f"Failed to save options: {save_msg}")

        # Apply extra constants
        if scenario.extra_constants:
            extra_path = os.path.join(self.inputs_dir, "EXTRA_WCONST.txt")
            if os.path.exists(extra_path):
                ecf = ExtraConstantsFile(extra_path)
                if ecf.parse():
                    for const_name, value in scenario.extra_constants.items():
                        if const_name in ecf.constants:
                            ecf.constants[const_name].value = value
                    save_ok, save_msg = ecf.save(backup=True)
                    if save_ok:
                        results.append(f"Updated {len(scenario.extra_constants)} extra constants")
                    else:
                        errors.append(f"Failed to save extra constants: {save_msg}")

        if errors:
            return False, f"Applied with errors: {'; '.join(errors)}"

        return True, f"Applied scenario '{scenario.name}': {'; '.join(results)}"


def create_builtin_presets(scenarios_dir: str) -> None:
    """Create built-in preset scenario files if they don't exist"""

    presets = {
        "default": Scenario(
            name="Default",
            description="Standard model configuration with baseline parameters",
            is_builtin=True,
            parameters={},  # Empty means don't change
            initial_conditions={},
            model_options={
                "ZOOPLANKTON_OPTION": 1,
                "ADVANCED_REDOX_SIMULATION": 0,
                "LIGHT_EXTINCTION_OPTION": 0,
                "CYANO_BOUYANT_STATE_SIMULATION": 1,
                "CONSIDER_NON_OBLIGATORY_FIXERS": 0,
                "CONSIDER_HETEROCYST_WITH_AKINETES": 1,
                "CONSIDER_ALLELOPATHY": 1
            },
            extra_constants={}
        ),

        "high_nutrient": Scenario(
            name="High Nutrient",
            description="Elevated nitrogen and phosphorus loading - eutrophic conditions",
            is_builtin=True,
            parameters={},
            initial_conditions={
                "1": 0.15,   # NH4_N - elevated ammonium
                "2": 0.10,   # NO3_N - elevated nitrate
                "3": 0.08,   # PO4_P - elevated phosphate
            },
            model_options={
                "ZOOPLANKTON_OPTION": 1,
                "ADVANCED_REDOX_SIMULATION": 0,
                "CONSIDER_ALLELOPATHY": 1
            },
            extra_constants={}
        ),

        "hypoxic": Scenario(
            name="Hypoxic",
            description="Low oxygen conditions with enhanced redox cycling",
            is_builtin=True,
            parameters={},
            initial_conditions={
                "6": 2.0,    # DISS_OXYGEN - low oxygen (hypoxic)
                "9": 5.0,    # DET_PART_ORG_C - elevated organic matter
            },
            model_options={
                "ZOOPLANKTON_OPTION": 1,
                "ADVANCED_REDOX_SIMULATION": 1,  # Enable full redox
                "CONSIDER_ALLELOPATHY": 0
            },
            extra_constants={}
        ),

        "algal_bloom": Scenario(
            name="Algal Bloom",
            description="Conditions favorable for phytoplankton bloom development",
            is_builtin=True,
            parameters={
                "5": 4.0,    # KG_DIA_OPT_TEMP - higher diatom growth rate
                "29": 3.5,   # KG_CYN_OPT_TEMP - higher cyanobacteria growth rate
            },
            initial_conditions={
                "1": 0.12,   # NH4_N - moderate nutrients
                "3": 0.06,   # PO4_P
                "5": 2.0,    # DIA_C - initial diatom biomass
                "15": 1.5,   # CYN_C - initial cyanobacteria biomass
            },
            model_options={
                "ZOOPLANKTON_OPTION": 1,
                "CYANO_BOUYANT_STATE_SIMULATION": 1,
                "CONSIDER_ALLELOPATHY": 1
            },
            extra_constants={}
        )
    }

    if not os.path.exists(scenarios_dir):
        os.makedirs(scenarios_dir)

    for filename, scenario in presets.items():
        filepath = os.path.join(scenarios_dir, f"{filename}.json")
        if not os.path.exists(filepath):
            try:
                with open(filepath, 'w') as f:
                    json.dump(scenario.to_dict(), f, indent=2)
                logger.info(f"Created built-in preset: {filepath}")
            except Exception as e:
                logger.error(f"Failed to create preset {filename}: {e}")


def get_scenarios_dir() -> str:
    """Get the default scenarios directory path"""
    script_dir = os.path.dirname(os.path.realpath(__file__))
    return os.path.join(script_dir, "scenarios")


def load_scenario_manager(inputs_dir: str) -> ScenarioManager:
    """
    Load or create scenario manager with built-in presets

    Args:
        inputs_dir: Path to model INPUTS directory

    Returns:
        Initialized ScenarioManager
    """
    scenarios_dir = get_scenarios_dir()

    # Create built-in presets if needed
    create_builtin_presets(scenarios_dir)

    # Create and return manager
    return ScenarioManager(scenarios_dir, inputs_dir)
