"""Tests for the CL29 sediment-diagenesis converter additions (Phase 1)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "tools", "eutropy_poc", "eutropy_to_estas.py")
_SPEC = importlib.util.spec_from_file_location("eutropy_to_estas", _PATH)
conv = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(conv)   # executes module; REPO = os.getcwd() = repo root


def _redox_flag(path):
    """Return the integer on the line after '# ADVANCED REDOX SIMULATION'."""
    with open(path, newline="") as fh:
        lines = fh.readlines()
    for i, ln in enumerate(lines):
        if ln.lstrip().startswith("# ADVANCED REDOX SIMULATION"):
            return int(lines[i + 1].split()[0])
    raise AssertionError("redox header not found")


class TestWriteSedimentInputs:
    def test_disabled_writes_nothing(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), False)
        assert not (tmp_path / "W_SED_CONST.txt").exists()
        assert not (tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt").exists()

    def test_enabled_copies_constants_verbatim(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), True)
        out = (tmp_path / "W_SED_CONST.txt").read_bytes()
        src = open(os.path.join(os.getcwd(), "INPUTS", "W_SED_CONST.txt"), "rb").read()
        assert out == src

    def test_enabled_forces_redox_zero(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), True)
        assert _redox_flag(str(tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt")) == 0

    def test_output_names_stay_bare(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), True)
        text = (tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt").read_text()
        assert "BOTTOM_SEDIMENTS_OUTPUTS.out" in text
        assert "/BOTTOM_SEDIMENTS_OUTPUTS.out" not in text  # no path prefix

    def test_carbonate_override_applied(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_SED_CARBONATE_IC", (3.0, 3.1))
        conv._write_sediment_inputs(str(tmp_path), True)
        with open(tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt", newline="") as fh:
            lines = fh.readlines()
        start, _ = conv._sed_ic_block_bounds(lines)
        inorg = [float(x) for x in lines[start + 12].split()]
        alk = [float(x) for x in lines[start + 13].split()]
        assert all(v == 3.0 for v in inorg) and len(inorg) == 7
        assert all(v == 3.1 for v in alk) and len(alk) == 7
