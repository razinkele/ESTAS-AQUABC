"""Tests for the AQUABC model options parser."""

import textwrap

import pytest

from shiny_app.options_parser import MODEL_OPTIONS, ModelOptionsFile, load_model_options


@pytest.fixture
def sample_options_file(tmp_path):
    """Create a minimal PELAGIC_MODEL_OPTIONS.txt for testing."""
    content = textwrap.dedent("""\
        # ZOOPLANKTON OPTION  if 0 unrealistic zooplankton CNP partitioning
                    1
        # ADVANCED REDOX SIMULATION  if 0 not full redox cycle with Mn,Fe,So4,metahne
                    0
        # LIGHT_EXTINCTION_OPTION  if 0 empiric equation Curonian specific
                    0
        # CYANO_BOUYANT_STATE_SIMULATION
                    1
        # CONSIDER NON-OBLIGATORY FIXERS
                    1
    """)
    f = tmp_path / "PELAGIC_MODEL_OPTIONS.txt"
    f.write_text(content)
    return str(f)


class TestModelOptionsFile:
    def test_parse_success(self, sample_options_file):
        mof = ModelOptionsFile(sample_options_file)
        assert mof.parse() is True

    def test_parse_values(self, sample_options_file):
        mof = load_model_options(sample_options_file)
        assert mof.options["ZOOPLANKTON_OPTION"].value == 1
        assert mof.options["ADVANCED_REDOX_SIMULATION"].value == 0

    def test_parse_nonexistent(self, tmp_path):
        mof = ModelOptionsFile(str(tmp_path / "nonexistent.txt"))
        assert mof.parse() is False

    def test_save_roundtrip(self, sample_options_file):
        mof = load_model_options(sample_options_file)
        ok, msg = mof.update_option("ZOOPLANKTON_OPTION", 0)
        assert ok
        ok, msg = mof.save(backup=False)
        assert ok

        mof2 = load_model_options(sample_options_file)
        assert mof2.options["ZOOPLANKTON_OPTION"].value == 0


class TestModelOptionDefinitions:
    def test_options_defined(self):
        assert len(MODEL_OPTIONS) > 0
        assert "ZOOPLANKTON_OPTION" in MODEL_OPTIONS

    def test_option_has_description(self):
        for name, opt in MODEL_OPTIONS.items():
            assert "description" in opt, f"Option {name} missing description"
            assert "type" in opt, f"Option {name} missing type"
