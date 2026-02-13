"""Tests for the AQUABC simulation config parser."""

import textwrap
from datetime import date

import pytest

from shiny_app.simulation_config import SimulationConfig, SimulationConfigFile, load_simulation_config


@pytest.fixture
def sample_config_file(tmp_path):
    """Create a minimal INPUT.txt for testing."""
    content = textwrap.dedent("""\
        # DESCRIPTION LINE 1
        # DESCRIPTION LINE 2
        # DESCRIPTION LINE 3
        # DESCRIPTION LINE 4
        # DESCRIPTION LINE 5
        # BASE_YEAR
                   1998
        # SIMULATION_START
                 6209.0
        # SIMULATION_END
                 6574.0
        # NUM_REPEATS
                      1
        # TIME_STEPS_PER_DAY
                    240
        # PRINT_INTERVAL IN TIME STEPS
                     10
        # PELAGIC MODEL INPUT FOLDER write the folder always "\\" (windows) or "/" (UNIX) in the end
        INPUTS/
        # PELAGIC MODEL INPUT FILE
        PELAGIC_INPUTS.txt
        # OUTPUT FOLDER  write the folder always "\\" (windows) or "/" (UNIX) in the end
        OUTPUTS/
        # RESUSPENSION OPTION
                      0
        # MODEL_SEDIMENTS
                      1
        # NUMBER OF SEDIMENT FLUX SETS
                      2
        SEDIMENT_FLUX_1.txt
        SEDIMENT_FLUX_2.txt
    """)
    f = tmp_path / "INPUT.txt"
    f.write_text(content)
    return str(f)


class TestSimulationConfig:
    def test_date_conversion(self):
        cfg = SimulationConfig(base_year=1998, simulation_start=0.0, simulation_end=365.0)
        assert cfg.get_start_date() == date(1998, 1, 1)
        assert cfg.get_end_date() == date(1999, 1, 1)

    def test_set_date(self):
        cfg = SimulationConfig(base_year=1998)
        cfg.set_start_date(date(1998, 7, 1))
        # July 1 = day 181 (1998 is not a leap year)
        assert cfg.simulation_start == pytest.approx(181.0)


class TestSimulationConfigFile:
    def test_parse_success(self, sample_config_file):
        scf = SimulationConfigFile(sample_config_file)
        assert scf.parse() is True

    def test_parse_values(self, sample_config_file):
        scf = load_simulation_config(sample_config_file)
        assert scf is not None
        assert scf.config.base_year == 1998
        assert scf.config.simulation_start == pytest.approx(6209.0)
        assert scf.config.simulation_end == pytest.approx(6574.0)
        assert scf.config.time_steps_per_day == 240
        assert scf.config.print_interval == 10

    def test_parse_paths(self, sample_config_file):
        scf = load_simulation_config(sample_config_file)
        assert scf.config.input_folder == "INPUTS/"
        assert scf.config.output_folder == "OUTPUTS/"

    def test_parse_nonexistent(self, tmp_path):
        result = load_simulation_config(str(tmp_path / "nonexistent.txt"))
        assert result is None

    def test_save_roundtrip(self, sample_config_file):
        scf = load_simulation_config(sample_config_file)
        scf.config.time_steps_per_day = 120
        ok, msg = scf.save(backup=False)
        assert ok

        scf2 = load_simulation_config(sample_config_file)
        assert scf2.config.time_steps_per_day == 120
        # Unchanged values preserved
        assert scf2.config.base_year == 1998
