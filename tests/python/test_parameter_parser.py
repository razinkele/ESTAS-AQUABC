"""Tests for the AQUABC parameter parser."""

import textwrap

import pytest

from shiny_app.parameter_parser import PARAMETER_CATEGORIES, Parameter, ParameterFile, load_parameters


@pytest.fixture
def sample_param_file(tmp_path):
    """Create a minimal parameter file for testing."""
    content = textwrap.dedent("""\
         1                                K_A              -1.0  !  1   Aeration coefficient (if negative calculates internally)
         2                          THETA_K_A              1.04  !  2   Temperature correction factor for aeration
         3                                XKC              0.08  !  3   Light extinction per chlorophyl unit
         4                              PHIMX            720.00  !  4   Quantum yield const. mg C/mole photon
         5                    KG_DIA_OPT_TEMP               3.7  !  5   Diatoms Growth rate
    """)
    f = tmp_path / "WCONST_04.txt"
    f.write_text(content)
    return str(f)


class TestParameter:
    def test_parameter_creation(self):
        p = Parameter(id=1, name="K_A", value=-1.0, comment="Aeration", line_number=1, original_line="")
        assert p.id == 1
        assert p.name == "K_A"
        assert p.value == -1.0

    def test_parameter_validation_in_range(self):
        p = Parameter(id=5, name="KG_DIA_OPT_TEMP", value=3.7, comment="Growth rate", line_number=5, original_line="")
        valid, msg = p.validate(2.0)
        assert valid

    def test_parameter_validation_below_min(self):
        p = Parameter(id=5, name="KG_DIA_OPT_TEMP", value=3.7, comment="Growth rate", line_number=5, original_line="")
        valid, msg = p.validate(-1.0)
        assert not valid
        assert "below minimum" in msg

    def test_parameter_validation_above_max(self):
        p = Parameter(id=5, name="KG_DIA_OPT_TEMP", value=3.7, comment="Growth rate", line_number=5, original_line="")
        valid, msg = p.validate(15.0)
        assert not valid
        assert "above maximum" in msg

    def test_parameter_to_dict(self):
        p = Parameter(id=1, name="K_A", value=-1.0, comment="Aeration", line_number=1, original_line="")
        d = p.to_dict()
        assert d["id"] == 1
        assert d["name"] == "K_A"
        assert d["value"] == -1.0


class TestParameterFile:
    def test_parse_success(self, sample_param_file):
        pf = ParameterFile(sample_param_file)
        assert pf.parse() is True
        assert len(pf.parameters) == 5

    def test_parse_parameter_values(self, sample_param_file):
        pf = load_parameters(sample_param_file)
        assert pf.parameters[1].name == "K_A"
        assert pf.parameters[1].value == -1.0
        assert pf.parameters[5].name == "KG_DIA_OPT_TEMP"
        assert pf.parameters[5].value == 3.7

    def test_parse_nonexistent_file(self, tmp_path):
        pf = ParameterFile(str(tmp_path / "nonexistent.txt"))
        assert pf.parse() is False

    def test_update_parameter(self, sample_param_file):
        pf = load_parameters(sample_param_file)
        ok, msg = pf.update_parameter(1, -2.0)
        assert ok
        assert pf.parameters[1].value == -2.0

    def test_update_nonexistent_parameter(self, sample_param_file):
        pf = load_parameters(sample_param_file)
        ok, msg = pf.update_parameter(999, 1.0)
        assert not ok

    def test_save_roundtrip(self, sample_param_file, tmp_path):
        pf = load_parameters(sample_param_file)
        pf.update_parameter(1, -2.0)
        ok, msg = pf.save(backup=False)
        assert ok

        # Re-read and verify
        pf2 = load_parameters(sample_param_file)
        assert pf2.parameters[1].value == -2.0
        # Unchanged parameter should still be correct
        assert pf2.parameters[2].value == 1.04

    def test_search(self, sample_param_file):
        pf = load_parameters(sample_param_file)
        results = pf.search("DIA")
        assert len(results) == 1
        assert results[0].name == "KG_DIA_OPT_TEMP"

    def test_get_summary(self, sample_param_file):
        pf = load_parameters(sample_param_file)
        s = pf.get_summary()
        assert s["total_parameters"] == 5
        assert "filepath" in s

    def test_categories_defined(self):
        assert len(PARAMETER_CATEGORIES) > 0
        assert "Diatoms" in PARAMETER_CATEGORIES
        assert "Zooplankton" in PARAMETER_CATEGORIES
