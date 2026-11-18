"""Audit items 27-29: documentation that had drifted from the code."""
import os
import subprocess
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))


def test_parameter_reference_is_generated_and_current():
    """PARAMETER_REFERENCE.md named 53 of the 324 constants; its tables are now generated."""
    rc = subprocess.run([sys.executable, os.path.join(ROOT, "tools", "build_parameter_reference.py"),
                         "--check"], capture_output=True, text=True)
    assert rc.returncode == 0, rc.stdout + rc.stderr
    doc = open(os.path.join(ROOT, "PARAMETER_REFERENCE.md")).read()
    names = [line.split()[1] for line in open(os.path.join(ROOT, "INPUTS", "WCONST_04.txt"))
             if len(line.split()) >= 3 and line.split()[0].isdigit()]
    assert len(names) == 324
    assert all(f"`{n}`" in doc for n in names)


def test_readme_points_at_the_phenology_record_and_the_adopted_inputs():
    readme = open(os.path.join(ROOT, "README.md")).read()
    assert "docs/CL29_phenology_diagnosis.md" in readme
    assert "ESTAS-AQUABC-DATA" in readme
    assert os.path.exists(os.path.join(ROOT, "docs", "CL29_phenology_diagnosis.md"))


def test_cl29_inputs_are_not_described_as_regenerable():
    """The generator gives the pre-calibration set, not the adopted one."""
    setups = open(os.path.join(ROOT, "shiny_app", "setups.py")).read()
    assert "ESTAS-AQUABC-DATA" in setups
    assert 'unavailable_hint="Generate inputs: python tools/eutropy_poc' not in setups
