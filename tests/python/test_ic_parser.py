"""Tests for the AQUABC initial conditions parser."""

import textwrap

import pytest

from shiny_app.ic_parser import STATE_VARIABLES, ICFile, get_variable_display_name, load_ic_file


@pytest.fixture
def sample_ic_file(tmp_path):
    """Create a minimal IC file for testing."""
    content = textwrap.dedent("""\
        # PELAGIC INITIAL CONCENTRATION DATA SET 1: Test
        #     PELAGIC STATE VAR. NO       PELAGIC CONCENTRATION
                                  1          0.02             ! AMMONIUM NITROGEN
                                  2          0.50             ! NITRATE NITROGEN
                                  3          0.0047           ! ORTHOPHOSPHATE PHOSPHORUS
                                  4          9.50             ! DISSOLVED OXYGEN
    """)
    f = tmp_path / "INIT_CONC_1.txt"
    f.write_text(content)
    return str(f)


class TestICFile:
    def test_parse_success(self, sample_ic_file):
        ic = ICFile(sample_ic_file)
        assert ic.parse() is True
        assert len(ic.conditions) == 4

    def test_parse_values(self, sample_ic_file):
        ic = load_ic_file(sample_ic_file)
        assert ic.conditions[1].value == pytest.approx(0.02)
        assert ic.conditions[2].value == pytest.approx(0.50)
        assert ic.conditions[4].value == pytest.approx(9.50)

    def test_parse_header_lines(self, sample_ic_file):
        ic = load_ic_file(sample_ic_file)
        assert len(ic.header_lines) == 2

    def test_parse_nonexistent_file(self, tmp_path):
        ic = ICFile(str(tmp_path / "nonexistent.txt"))
        assert ic.parse() is False

    def test_update_condition(self, sample_ic_file):
        ic = load_ic_file(sample_ic_file)
        ok, msg = ic.update_condition(1, 0.05)
        assert ok
        assert ic.conditions[1].value == pytest.approx(0.05)

    def test_save_roundtrip(self, sample_ic_file):
        ic = load_ic_file(sample_ic_file)
        ic.update_condition(1, 0.05)
        ok, msg = ic.save(backup=False)
        assert ok

        ic2 = load_ic_file(sample_ic_file)
        assert ic2.conditions[1].value == pytest.approx(0.05)
        assert ic2.conditions[2].value == pytest.approx(0.50)


class TestStateVariables:
    def test_state_variables_count(self):
        assert len(STATE_VARIABLES) >= 30

    def test_state_variable_fields(self):
        for _idx, info in STATE_VARIABLES.items():
            assert "name" in info
            assert "description" in info
            assert "units" in info

    def test_display_name(self):
        name = get_variable_display_name("NH4N")
        assert "Ammonium" in name
        assert "mg N/L" in name

    def test_display_name_unknown(self):
        name = get_variable_display_name("UNKNOWN_COL")
        # Should return something reasonable even for unknown columns
        assert isinstance(name, str)
