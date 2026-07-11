"""Tests for CL29 wind-modulated diatom settling (#3)."""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "tools", "eutropy_poc", "eutropy_to_estas.py")
_SPEC = importlib.util.spec_from_file_location("eutropy_to_estas", _PATH)
conv = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(conv)   # REPO = os.getcwd() = repo root


class TestWindModulatedSettling:
    def test_calm_returns_w0(self):
        # U=0 -> w_eff = w0/(1+0) = w0
        assert conv.wind_modulated_settling([0.0], 0.3, 4.21) == [0.3]

    def test_half_at_uhalf(self):
        # U=U_c -> w_eff = w0/(1+1) = w0/2
        w = conv.wind_modulated_settling([4.21], 0.3, 4.21)[0]
        assert abs(w - 0.15) < 1e-12

    def test_strictly_decreasing(self):
        w = conv.wind_modulated_settling([0, 2, 4, 6, 8, 10], 0.3, 4.21)
        assert all(w[i] > w[i + 1] for i in range(len(w) - 1))

    def test_always_positive_and_bounded(self):
        w = conv.wind_modulated_settling([0, 5, 10, 20, 50], 0.3, 4.21)
        assert all(0.0 < x <= 0.3 for x in w)
