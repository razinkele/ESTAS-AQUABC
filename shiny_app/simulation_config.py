"""
Simulation Configuration Parser for AQUABC

Parses and manages INPUT.txt which contains simulation control parameters:
- Base year and simulation period (start/end as days from base year)
- Time stepping configuration
- Input/output folder paths
- Model options (sediments, resuspension)

Author: Claude
Date: 2026-01-17
"""

import logging
import os
import re
import shutil
from dataclasses import dataclass, field
from datetime import date, datetime, timedelta

logger = logging.getLogger("AQUABC.SimConfig")


@dataclass
class SimulationConfig:
    """Represents simulation configuration from INPUT.txt"""
    # Description lines (5 lines)
    description: list[str] = field(default_factory=lambda: [""] * 5)

    # Time configuration
    base_year: int = 1998
    simulation_start: float = 0.0  # Days from Jan 1 of base_year
    simulation_end: float = 365.0
    num_repeats: int = 1
    time_steps_per_day: int = 240
    print_interval: int = 24  # In time steps

    # Paths
    input_folder: str = "INPUTS/"
    input_file: str = "PELAGIC_INPUTS.txt"
    output_folder: str = "OUTPUTS/"

    # Model options
    resuspension_option: int = 0
    model_sediments: int = 1
    num_sediment_flux_sets: int = 2
    sediment_flux_files: list[str] = field(default_factory=list)

    def get_start_date(self) -> date:
        """Convert simulation_start (days) to actual date"""
        base = date(self.base_year, 1, 1)
        return base + timedelta(days=int(self.simulation_start))

    def get_end_date(self) -> date:
        """Convert simulation_end (days) to actual date"""
        base = date(self.base_year, 1, 1)
        return base + timedelta(days=int(self.simulation_end))

    def set_start_date(self, d: date) -> None:
        """Set simulation_start from a date"""
        base = date(self.base_year, 1, 1)
        delta = d - base
        self.simulation_start = float(delta.days)

    def set_end_date(self, d: date) -> None:
        """Set simulation_end from a date"""
        base = date(self.base_year, 1, 1)
        delta = d - base
        self.simulation_end = float(delta.days)

    def get_simulation_days(self) -> float:
        """Get total simulation duration in days"""
        return self.simulation_end - self.simulation_start

    def get_output_interval_hours(self) -> float:
        """Get output interval in hours"""
        steps_per_hour = self.time_steps_per_day / 24.0
        return self.print_interval / steps_per_hour

    def set_output_interval_hours(self, hours: float) -> None:
        """Set print_interval from hours"""
        steps_per_hour = self.time_steps_per_day / 24.0
        self.print_interval = int(round(hours * steps_per_hour))

    def get_time_step_seconds(self) -> float:
        """Get time step in seconds"""
        return 86400.0 / self.time_steps_per_day

    def get_time_step_minutes(self) -> float:
        """Get time step in minutes"""
        return 1440.0 / self.time_steps_per_day

    def validate(self) -> tuple[bool, list[str]]:
        """Validate configuration, return (is_valid, error_messages)"""
        errors = []

        if self.base_year < 1900 or self.base_year > 2100:
            errors.append(f"Base year {self.base_year} seems invalid (expected 1900-2100)")

        if self.simulation_start < 0:
            errors.append("Simulation start cannot be negative")

        if self.simulation_end <= self.simulation_start:
            errors.append("Simulation end must be after start")

        if self.time_steps_per_day < 1 or self.time_steps_per_day > 86400:
            errors.append(f"Time steps per day {self.time_steps_per_day} invalid (expected 1-86400)")

        if self.print_interval < 1:
            errors.append("Print interval must be at least 1")

        if self.num_repeats < 1:
            errors.append("Number of repeats must be at least 1")

        return len(errors) == 0, errors


class SimulationConfigFile:
    """Parser for INPUT.txt simulation configuration file"""

    def __init__(self, filepath: str):
        self.filepath = filepath
        self.config = SimulationConfig()
        self.raw_lines: list[str] = []
        self._parsed = False

    def parse(self) -> bool:
        """Parse INPUT.txt file"""
        if not os.path.exists(self.filepath):
            logger.error(f"File not found: {self.filepath}")
            return False

        try:
            with open(self.filepath) as f:
                self.raw_lines = f.readlines()

            logger.info(f"Parsing {self.filepath} ({len(self.raw_lines)} lines)")

            # Parse line by line with state machine
            line_idx = 0
            description_count = 0
            sediment_files_to_read = 0

            while line_idx < len(self.raw_lines):
                line = self.raw_lines[line_idx].strip()

                # Skip empty lines
                if not line:
                    line_idx += 1
                    continue

                # Reading sediment flux files
                if sediment_files_to_read > 0:
                    if not line.startswith('#'):
                        self.config.sediment_flux_files.append(line)
                        sediment_files_to_read -= 1
                    line_idx += 1
                    continue

                # Description lines (first 5 comment lines)
                if line.startswith('# DESCRIPTION'):
                    if description_count < 5:
                        self.config.description[description_count] = line[2:].strip()
                        description_count += 1
                    line_idx += 1
                    continue

                # BASE_YEAR
                if '# BASE_YEAR' in line or line == '# BASE_YEAR':
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.base_year = int(val)
                        logger.debug(f"BASE_YEAR = {self.config.base_year}")
                    line_idx += 1
                    continue

                # SIMULATION_START
                if '# SIMULATION_START' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.simulation_start = float(val)
                        logger.debug(f"SIMULATION_START = {self.config.simulation_start}")
                    line_idx += 1
                    continue

                # SIMULATION_END
                if '# SIMULATION_END' in line:
                    # Value might be on same line or next line
                    match = re.search(r'SIMULATION_END\s+([\d.]+)', line)
                    if match:
                        # Value on same line (commented out old value)
                        pass
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.simulation_end = float(val)
                        logger.debug(f"SIMULATION_END = {self.config.simulation_end}")
                    line_idx += 1
                    continue

                # NUM_REPEATS
                if '# NUM_REPEATS' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.num_repeats = int(val)
                        logger.debug(f"NUM_REPEATS = {self.config.num_repeats}")
                    line_idx += 1
                    continue

                # TIME_STEPS_PER_DAY
                if '# TIME_STEPS_PER_DAY' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.time_steps_per_day = int(val)
                        logger.debug(f"TIME_STEPS_PER_DAY = {self.config.time_steps_per_day}")
                    line_idx += 1
                    continue

                # PRINT_INTERVAL
                if '# PRINT_INTERVAL' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.print_interval = int(val)
                        logger.debug(f"PRINT_INTERVAL = {self.config.print_interval}")
                    line_idx += 1
                    continue

                # INPUT FOLDER
                if '# PELAGIC MODEL INPUT FOLDER' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.input_folder = val
                        logger.debug(f"INPUT_FOLDER = {self.config.input_folder}")
                    line_idx += 1
                    continue

                # INPUT FILE
                if '# PELAGIC MODEL INPUT FILE' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.input_file = val
                        logger.debug(f"INPUT_FILE = {self.config.input_file}")
                    line_idx += 1
                    continue

                # OUTPUT FOLDER
                if '# PELAGIC MODEL OUTPUT FOLDER' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.output_folder = val
                        logger.debug(f"OUTPUT_FOLDER = {self.config.output_folder}")
                    line_idx += 1
                    continue

                # RESUSPENSION_OPTION
                if '# RESUSPENSION_OPTION' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.resuspension_option = int(val)
                        logger.debug(f"RESUSPENSION_OPTION = {self.config.resuspension_option}")
                    line_idx += 1
                    continue

                # MODEL_SEDIMENTS
                if '# MODEL_SEDIMENTS' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.model_sediments = int(val)
                        logger.debug(f"MODEL_SEDIMENTS = {self.config.model_sediments}")
                    line_idx += 1
                    continue

                # NUM_PRESCRIBED_SEDIMENT_FLUX_SETS
                if '# NUM_PRESCRIBED_SEDIMENT_FLUX_SETS' in line:
                    line_idx += 1
                    if line_idx < len(self.raw_lines):
                        val = self.raw_lines[line_idx].strip()
                        self.config.num_sediment_flux_sets = int(val)
                        sediment_files_to_read = self.config.num_sediment_flux_sets
                        logger.debug(f"NUM_SEDIMENT_FLUX_SETS = {self.config.num_sediment_flux_sets}")
                    line_idx += 1
                    continue

                # SEDIMENT MODEL INPUT FILE header
                if '# SEDIMENT MODEL INPUT FILE' in line:
                    line_idx += 1
                    continue

                line_idx += 1

            self._parsed = True
            logger.info(f"Successfully parsed simulation config: base_year={self.config.base_year}, "
                       f"start={self.config.simulation_start}, end={self.config.simulation_end}")
            return True

        except Exception as e:
            logger.error(f"Error parsing {self.filepath}: {e}", exc_info=True)
            return False

    def save(self, backup: bool = True) -> tuple[bool, str]:
        """Save configuration back to INPUT.txt"""
        if not self._parsed:
            return False, "Configuration not loaded"

        try:
            # Create backup
            if backup and os.path.exists(self.filepath):
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                backup_path = f"{self.filepath}.{timestamp}.bak"
                shutil.copy(self.filepath, backup_path)
                logger.info(f"Created backup: {backup_path}")

            # Generate new file content
            content = self._generate_file_content()

            with open(self.filepath, 'w') as f:
                f.write(content)

            logger.info(f"Saved configuration to {self.filepath}")
            return True, "Saved successfully"

        except Exception as e:
            logger.error(f"Error saving {self.filepath}: {e}", exc_info=True)
            return False, str(e)

    def _generate_file_content(self) -> str:
        """Generate INPUT.txt content from current config"""
        lines = []

        # Description lines
        for i, desc in enumerate(self.config.description):
            lines.append(f"# {desc}" if desc else f"# DESCRIPTION LINE {i+1}")

        # BASE_YEAR
        lines.append("# BASE_YEAR")
        lines.append(f"           {self.config.base_year}")

        # SIMULATION_START
        lines.append("# SIMULATION_START")
        lines.append(f"         {self.config.simulation_start:.1f}")

        # SIMULATION_END
        lines.append("# SIMULATION_END")
        lines.append(f"         {self.config.simulation_end:.1f}")

        # NUM_REPEATS
        lines.append("# NUM_REPEATS")
        lines.append(f"              {self.config.num_repeats}")

        # TIME_STEPS_PER_DAY
        lines.append("# TIME_STEPS_PER_DAY")
        lines.append(f"            {self.config.time_steps_per_day}")

        # PRINT_INTERVAL
        lines.append("# PRINT_INTERVAL IN TIME STEPS")
        lines.append(f"             {self.config.print_interval}")

        # INPUT FOLDER
        lines.append("# PELAGIC MODEL INPUT FOLDER write the folder always \"\\\" (windows) or \"/\" (UNIX) in the end")
        lines.append(self.config.input_folder)

        # INPUT FILE
        lines.append("# PELAGIC MODEL INPUT FILE")
        lines.append(f"            {self.config.input_file}")

        # OUTPUT FOLDER
        lines.append("# PELAGIC MODEL OUTPUT FOLDER write the folder always \"\\\" (windows) or \"/\" (UNIX) in the end")
        lines.append(self.config.output_folder)

        # RESUSPENSION_OPTION
        lines.append("# RESUSPENSION_OPTION")
        lines.append(f"          {self.config.resuspension_option}")

        # MODEL_SEDIMENTS
        lines.append("# MODEL_SEDIMENTS")
        lines.append(f"          {self.config.model_sediments}")

        # NUM_PRESCRIBED_SEDIMENT_FLUX_SETS
        lines.append("# NUM_PRESCRIBED_SEDIMENT_FLUX_SETS")
        lines.append(f"          {self.config.num_sediment_flux_sets}")

        # SEDIMENT MODEL INPUT FILE
        lines.append("# SEDIMENT MODEL INPUT FILE")
        for flux_file in self.config.sediment_flux_files:
            lines.append(flux_file)

        # Trailing newlines
        lines.append("")
        lines.append("")

        return "\n".join(lines)

    def update_config(self, **kwargs) -> tuple[int, list[str]]:
        """
        Update multiple config values at once.
        Returns (success_count, error_messages)
        """
        success = 0
        errors = []

        for key, value in kwargs.items():
            if hasattr(self.config, key):
                try:
                    old_value = getattr(self.config, key)
                    setattr(self.config, key, value)
                    logger.debug(f"Updated {key}: {old_value} -> {value}")
                    success += 1
                except Exception as e:
                    errors.append(f"Failed to set {key}: {e}")
            else:
                errors.append(f"Unknown config key: {key}")

        return success, errors


# Convenience function to load simulation config
def load_simulation_config(filepath: str) -> SimulationConfigFile | None:
    """Load and parse simulation configuration file"""
    scf = SimulationConfigFile(filepath)
    if scf.parse():
        return scf
    return None


# Date conversion utilities
def days_to_date(days: float, base_year: int) -> date:
    """Convert days from Jan 1 of base_year to date"""
    base = date(base_year, 1, 1)
    return base + timedelta(days=int(days))


def date_to_days(d: date, base_year: int) -> float:
    """Convert date to days from Jan 1 of base_year"""
    base = date(base_year, 1, 1)
    delta = d - base
    return float(delta.days)


# Predefined time step options
TIME_STEP_PRESETS = {
    "1 hour": 24,
    "30 minutes": 48,
    "15 minutes": 96,
    "10 minutes": 144,
    "6 minutes": 240,
    "5 minutes": 288,
    "1 minute": 1440,
}

# Output interval presets (in hours)
OUTPUT_INTERVAL_PRESETS = {
    "Hourly": 1.0,
    "3-hourly": 3.0,
    "6-hourly": 6.0,
    "12-hourly": 12.0,
    "Daily": 24.0,
    "Weekly": 168.0,
}
