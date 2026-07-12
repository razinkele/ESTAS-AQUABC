"""End-to-end regression test for the AQUABC 0D pelagic example.

Two layers:
  1. Unit tests of the comparison logic itself (tests/regression/compare_0D.py) —
     always run, no Fortran needed, so the Python-only CI job exercises them.
  2. If a freshly built 0D OUTPUT.csv is present (i.e. the model was run in this
     job), diff it against the committed golden. Skipped when absent.
"""
import importlib.util
import os

import pytest

_REPO = os.getcwd()
_CMP_PATH = os.path.join(_REPO, "tests", "regression", "compare_0D.py")
_spec = importlib.util.spec_from_file_location("compare_0D", _CMP_PATH)
compare_0D = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(compare_0D)

_GOLDEN = os.path.join(_REPO, "tests", "regression", "pelagic_0D_golden.csv")
_FRESH = os.path.join(_REPO, "SOURCE_CODE", "AQUABC", "AQUABC_EXAMPLES",
                      "AQUABC_PELAGIC_0D", "OUTPUT.csv")


def _write_csv(path, header, rows):
    with open(path, "w") as fh:
        fh.write(",".join(header) + "\n")
        for r in rows:
            fh.write(",".join(f"{v:.10f}" for v in r) + "\n")


class TestCompareLogic:
    HEADER = ["TIME", "A", "B"]
    ROWS = [[float(i), i * 1.5, i * 2.0] for i in range(6)]

    def test_identical_passes(self, tmp_path):
        a = tmp_path / "a.csv"
        _write_csv(a, self.HEADER, self.ROWS)
        ok, msg = compare_0D.compare(str(a), str(a), stride=1)
        assert ok, msg

    def test_column_reorder_fails(self, tmp_path):
        a = tmp_path / "a.csv"
        b = tmp_path / "b.csv"
        _write_csv(a, self.HEADER, self.ROWS)
        _write_csv(b, ["TIME", "B", "A"], self.ROWS)
        ok, msg = compare_0D.compare(str(b), str(a), stride=1)
        assert not ok and "HEADER" in msg

    def test_column_count_fails(self, tmp_path):
        a = tmp_path / "a.csv"
        b = tmp_path / "b.csv"
        _write_csv(a, self.HEADER, self.ROWS)
        _write_csv(b, ["TIME", "A"], [[r[0], r[1]] for r in self.ROWS])
        ok, msg = compare_0D.compare(str(b), str(a), stride=1)
        assert not ok and "COUNT" in msg

    def test_value_drift_beyond_tol_fails(self, tmp_path):
        a = tmp_path / "a.csv"
        b = tmp_path / "b.csv"
        _write_csv(a, self.HEADER, self.ROWS)
        drifted = [row[:] for row in self.ROWS]
        drifted[3][2] *= 1.001                       # 0.1% change
        _write_csv(b, self.HEADER, drifted)
        ok, msg = compare_0D.compare(str(b), str(a), stride=1, rtol=1e-9)
        assert not ok and "numerical" in msg

    def test_value_drift_within_tol_passes(self, tmp_path):
        a = tmp_path / "a.csv"
        b = tmp_path / "b.csv"
        _write_csv(a, self.HEADER, self.ROWS)
        jittered = [row[:] for row in self.ROWS]
        jittered[3][2] += 1e-11                       # below default tol
        _write_csv(b, self.HEADER, jittered)
        ok, msg = compare_0D.compare(str(b), str(a), stride=1)
        assert ok, msg

    def test_row_count_change_fails(self, tmp_path):
        a = tmp_path / "a.csv"
        b = tmp_path / "b.csv"
        _write_csv(a, self.HEADER, self.ROWS)
        _write_csv(b, self.HEADER, self.ROWS[:-1])
        ok, msg = compare_0D.compare(str(b), str(a), stride=1)
        assert not ok and "row COUNT" in msg


class TestModelOutputRegression:
    def test_golden_present(self):
        assert os.path.exists(_GOLDEN), "golden reference missing"

    @pytest.mark.skipif(not os.path.exists(_FRESH),
                        reason="0D OUTPUT.csv not built in this job")
    def test_fresh_0D_matches_golden(self):
        ok, msg = compare_0D.compare(_FRESH, _GOLDEN)
        assert ok, msg
