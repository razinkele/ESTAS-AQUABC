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

    @staticmethod
    def _sed_consts(path):
        c = {}
        for ln in open(path):
            t = ln.split()
            if len(t) >= 3 and t[0].isdigit():
                c[t[1]] = t[2]
        return c

    def test_enabled_applies_psi_dissolution_override(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), True)
        out = self._sed_consts(str(tmp_path / "W_SED_CONST.txt"))
        src = self._sed_consts(os.path.join(os.getcwd(), "INPUTS", "W_SED_CONST.txt"))
        # PSi dissolution rates overridden for stability...
        assert float(out["K_OXIC_DISS_PSi"]) == 0.1
        assert float(out["K_ANOXIC_DISS_PSi"]) == 0.02
        # ...while other constants keep their template values.
        assert out["THETA_DISS_PSi"] == src["THETA_DISS_PSi"]
        assert out["SOLID_PART_COEFF_PO4"] == src["SOLID_PART_COEFF_PO4"]

    def test_enabled_applies_stability_geometry(self, tmp_path):
        conv._write_sediment_inputs(str(tmp_path), True)
        with open(tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt", newline="") as fh:
            lines = fh.readlines()

        def _run(test):
            for i, ln in enumerate(lines):
                if test(ln):
                    vals, j = [], i + 1
                    while j < len(lines):
                        try:
                            vals.append(float(lines[j].split("!")[0].strip()))
                        except ValueError:
                            if vals:
                                break
                        j += 1
                    return vals
            return []

        assert _run(lambda l: "SED_DEPTHS" in l and "meters" in l)[:7] == conv.CL29_SED_DEPTHS
        assert _run(lambda l: "SED_BURRIALS" in l and "m/day" in l)[:1] == [conv.CL29_SED_BURIAL]

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
        # Independent anchor: locate the target rows in the ORIGINAL template by their
        # trailing comment tag, NOT via _sed_ic_block_bounds (the function under test).
        # This lets the test catch a wrong start index instead of sharing the bug.
        tmpl = os.path.join(os.getcwd(), "INPUTS", "BOTTOM_SEDIMENT_MODEL_INPUT.txt")
        with open(tmpl, newline="") as fh:
            orig = fh.readlines()

        def _row_index(tag):
            for i, ln in enumerate(orig):
                if ln.strip().endswith(tag):
                    return i
            raise AssertionError(f"template row {tag} not found")

        i_inorg = _row_index("!INORG_C")
        i_alk = _row_index("!TOT_ALK")
        i_nh4 = _row_index("!SED_NH4N")

        conv._write_sediment_inputs(str(tmp_path), True)
        with open(tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt", newline="") as fh:
            out = fh.readlines()

        # Overwritten rows drop the trailing comment -> 7 float tokens each.
        inorg = [float(x) for x in out[i_inorg].split()]
        alk = [float(x) for x in out[i_alk].split()]
        assert len(inorg) == 7 and all(v == 3.0 for v in inorg)
        assert len(alk) == 7 and all(v == 3.1 for v in alk)
        # Control row (SED_NH4N) must be left untouched.
        assert out[i_nh4] == orig[i_nh4]


class TestWriteInputTxt:
    def _write(self, tmp_path, enable):
        conv._write_input_txt(str(tmp_path), [0, 1826], enable_sediments=enable)
        return (tmp_path / "INPUT_CL29.txt").read_text()

    def test_enabled_uses_layout_2(self, tmp_path):
        t = self._write(tmp_path, True)
        assert "# MODEL_SEDIMENTS\n          2\n" in t
        assert "# BOTTOM SEDIMENT MODEL INPUT FILE\nBOTTOM_SEDIMENT_MODEL_INPUT.txt\n" in t
        assert "NUM_PRESCRIBED_SEDIMENT_FLUX_SETS" not in t   # must be absent under ==2
        assert f"{240:15d}\n" in t                            # PRINT_INTERVAL 240

    def test_disabled_matches_baseline(self, tmp_path):
        t = self._write(tmp_path, False)
        assert "# MODEL_SEDIMENTS\n          0\n" in t
        assert "# NUM_PRESCRIBED_SEDIMENT_FLUX_SETS\n          0\n" in t
        assert "# SEDIMENT MODEL INPUT FILE\n" in t
        assert f"{10:15d}\n" in t                             # PRINT_INTERVAL 10
        assert "MODEL_SEDIMENTS\n          2" not in t
