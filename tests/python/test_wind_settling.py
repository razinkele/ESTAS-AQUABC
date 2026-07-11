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


class TestReadWindDaily:
    def test_committed_file_present_and_sized(self):
        wind = conv._read_wind_daily()
        assert wind is not None
        assert len(wind) == 1827          # 2012-01-01 .. 2016-12-31
        assert all(w > 0 for w in wind)

    def test_absent_file_returns_none(self, tmp_path):
        missing = str(tmp_path / "nope.csv")
        assert conv._read_wind_daily(missing) is None

    def test_skips_comments_and_header(self, tmp_path):
        p = tmp_path / "wind_daily.csv"
        p.write_text(
            "# comment\n# Contains modified Copernicus\nday,wind_ms\n0,5.0\n1,6.5\n"
        )
        assert conv._read_wind_daily(str(p)) == [5.0, 6.5]


def _read_ts_values(path):
    """Return the list of first-column values from an ESTAS TS file
    (rows after the '# TIME AND VALUES' marker)."""
    vals = []
    started = False
    with open(path) as fh:
        for ln in fh:
            if started:
                parts = ln.split()
                if len(parts) >= 2:
                    vals.append(float(parts[1]))
            elif ln.startswith("# TIME AND VALUES"):
                started = True
    return vals


class TestWriteSettlingVelocityFiles:
    def test_wind_mode_writes_daily_series(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_WIND_RESUSPENSION", True)
        conv._write_settling_velocity_files(str(tmp_path))
        v1 = _read_ts_values(str(tmp_path / "SETTLING_VELOCITY_TS_1.txt"))
        assert len(v1) == 1828                      # 1827 daily + 1 sentinel
        wind = conv._read_wind_daily()
        expected0 = conv.wind_modulated_settling(
            wind[:1], conv.CL29_SETTLING_W0, conv.CL29_WIND_UHALF)[0]
        assert abs(v1[0] - expected0) < 1e-6        # day 0 matches the formula
        assert v1[-1] == v1[-2]                      # sentinel repeats last value
        # slots 2-6 stay 2-point constants
        v2 = _read_ts_values(str(tmp_path / "SETTLING_VELOCITY_TS_2.txt"))
        assert v2 == [0.1, 0.1]

    def test_fallback_is_constant(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_WIND_RESUSPENSION", False)
        conv._write_settling_velocity_files(str(tmp_path))
        v1 = _read_ts_values(str(tmp_path / "SETTLING_VELOCITY_TS_1.txt"))
        assert v1 == [conv.CL29_DIATOM_SETTLING, conv.CL29_DIATOM_SETTLING]  # 2 rows

    def test_fallback_byte_identical_to_legacy(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_WIND_RESUSPENSION", False)
        conv._write_settling_velocity_files(str(tmp_path))
        got = (tmp_path / "SETTLING_VELOCITY_TS_1.txt").read_text()
        ref = tmp_path / "ref.txt"
        conv.write_ts(str(ref), "settling velocity 1 m/day", [0, 9999], [[0.1], [0.1]])
        assert got == ref.read_text()

    def test_wind_enabled_but_file_absent_falls_back(self, tmp_path, monkeypatch):
        monkeypatch.setattr(conv, "CL29_WIND_RESUSPENSION", True)
        monkeypatch.setattr(conv, "_read_wind_daily", lambda *a, **k: None)
        conv._write_settling_velocity_files(str(tmp_path))
        v1 = _read_ts_values(str(tmp_path / "SETTLING_VELOCITY_TS_1.txt"))
        assert v1 == [conv.CL29_DIATOM_SETTLING, conv.CL29_DIATOM_SETTLING]


class TestFaciesStrawman:
    def test_active_map_stays_empty(self):
        # #5 stays inert -> CL29 byte-identical until the expert confirms a map.
        assert conv.CL29_SEDIMENT_TYPE == {}

    def test_provisional_covers_all_boxes(self):
        p = conv.CL29_SEDIMENT_TYPE_PROVISIONAL
        assert set(p) == set(range(1, 30))               # all 29 boxes
        assert set(p.values()) <= {"sandy", "muddy"}
        assert p[19] == "muddy"                          # interior muddy exemplar
        # marine-influenced boxes are sandy (spec §3.2)
        for b in (1, 4, 7, 10, 11, 12, 13, 16, 20, 22):
            assert p[b] == "sandy"
