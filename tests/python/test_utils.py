"""Tests for shiny_app.utils — extracted business logic from app.py.

Covers: count_file_lines_fast, read_pelagic_binary, read_pelagic_text,
        validate_constants_file
"""

import struct

import numpy as np
import pandas as pd
import pytest

from shiny_app.utils import (
    PELAGIC_BOX_COLUMNS,
    REQUIRED_MODEL_CONSTANTS,
    count_file_lines_fast,
    read_pelagic_binary,
    read_pelagic_text,
    validate_constants_file,
)


# ---------------------------------------------------------------------------
# count_file_lines_fast
# ---------------------------------------------------------------------------
class TestCountFileLinesfast:
    def test_small_file(self, tmp_path):
        """A small file (<1 MB) should be counted exactly."""
        f = tmp_path / "small.txt"
        f.write_text("line1\nline2\nline3\n")
        assert count_file_lines_fast(str(f)) == 3

    def test_small_file_no_trailing_newline(self, tmp_path):
        """Lines without trailing newline — last partial line still counted."""
        f = tmp_path / "notail.txt"
        f.write_text("a\nb\nc")
        # open in 'rb' and sum(1 for _ in f) counts 3 lines
        assert count_file_lines_fast(str(f)) == 3

    def test_empty_file(self, tmp_path):
        f = tmp_path / "empty.txt"
        f.write_text("")
        assert count_file_lines_fast(str(f)) == 0

    def test_single_line(self, tmp_path):
        f = tmp_path / "one.txt"
        f.write_text("hello\n")
        assert count_file_lines_fast(str(f)) == 1

    def test_nonexistent_file(self, tmp_path):
        """Should return 0 for files that don't exist."""
        assert count_file_lines_fast(str(tmp_path / "nope.txt")) == 0

    def test_large_file_estimation(self, tmp_path):
        """Files > 1 MB use sampling; result should be in the right ballpark."""
        f = tmp_path / "big.txt"
        line = "x" * 99 + "\n"  # 100 bytes per line
        num_lines = 12000  # 1.2 MB
        f.write_text(line * num_lines)
        estimated = count_file_lines_fast(str(f))
        # Allow 10 % tolerance
        assert abs(estimated - num_lines) / num_lines < 0.10


# ---------------------------------------------------------------------------
# read_pelagic_binary
# ---------------------------------------------------------------------------
class TestReadPelagicBinary:
    @staticmethod
    def _write_binary(path, nrows):
        """Write a valid binary file with `nrows` rows of 37 float64 values."""
        ncols = len(PELAGIC_BOX_COLUMNS)
        data = np.arange(nrows * ncols, dtype=np.float64).reshape(nrows, ncols)
        data.tofile(str(path))
        return data

    def test_valid_binary(self, tmp_path):
        f = tmp_path / "test.bin"
        expected = self._write_binary(f, 5)
        df = read_pelagic_binary(str(f))
        assert isinstance(df, pd.DataFrame)
        assert list(df.columns) == PELAGIC_BOX_COLUMNS
        assert len(df) == 5
        np.testing.assert_array_almost_equal(df.values, expected)

    def test_max_rows(self, tmp_path):
        f = tmp_path / "test.bin"
        self._write_binary(f, 10)
        df = read_pelagic_binary(str(f), max_rows=3)
        assert len(df) == 3

    def test_max_rows_larger_than_file(self, tmp_path):
        """max_rows bigger than actual rows should return all rows."""
        f = tmp_path / "test.bin"
        self._write_binary(f, 2)
        df = read_pelagic_binary(str(f), max_rows=100)
        assert len(df) == 2

    def test_empty_binary(self, tmp_path):
        f = tmp_path / "empty.bin"
        f.write_bytes(b"")
        df = read_pelagic_binary(str(f))
        assert len(df) == 0

    def test_incomplete_record(self, tmp_path):
        """Extra bytes that don't form a complete row should be truncated."""
        f = tmp_path / "partial.bin"
        ncols = len(PELAGIC_BOX_COLUMNS)
        # 2 complete rows + 5 extra doubles
        data = np.arange(2 * ncols + 5, dtype=np.float64)
        data.tofile(str(f))
        df = read_pelagic_binary(str(f))
        assert len(df) == 2

    def test_column_count(self, tmp_path):
        f = tmp_path / "test.bin"
        self._write_binary(f, 1)
        df = read_pelagic_binary(str(f))
        assert len(df.columns) == 37


# ---------------------------------------------------------------------------
# read_pelagic_text
# ---------------------------------------------------------------------------
class TestReadPelagicText:
    @staticmethod
    def _write_text(path, nrows=3):
        """Write a whitespace-separated text file with a header."""
        cols = ["TIME_DAYS", "NH4_N", "NO3_N"]
        lines = ["  ".join(cols)]
        for i in range(nrows):
            lines.append(f"  {float(i):.1f}  {float(i + 1):.1f}  {float(i + 2):.1f}")
        path.write_text("\n".join(lines) + "\n")
        return cols

    def test_valid_text(self, tmp_path):
        f = tmp_path / "test.out"
        cols = self._write_text(f, nrows=4)
        df = read_pelagic_text(str(f))
        assert isinstance(df, pd.DataFrame)
        assert list(df.columns) == cols
        assert len(df) == 4

    def test_max_rows(self, tmp_path):
        f = tmp_path / "test.out"
        self._write_text(f, nrows=10)
        df = read_pelagic_text(str(f), max_rows=3)
        assert len(df) == 3

    def test_empty_text_file(self, tmp_path):
        """An empty text file should raise (pd.read_csv on empty file)."""
        f = tmp_path / "empty.out"
        f.write_text("")
        with pytest.raises(Exception):
            read_pelagic_text(str(f))

    def test_strips_column_whitespace(self, tmp_path):
        """Column names with leading/trailing spaces should be stripped."""
        f = tmp_path / "spaced.out"
        f.write_text("  COL_A   COL_B \n1.0  2.0\n")
        df = read_pelagic_text(str(f))
        assert list(df.columns) == ["COL_A", "COL_B"]


# ---------------------------------------------------------------------------
# validate_constants_file
# ---------------------------------------------------------------------------
class TestValidateConstantsFile:
    @staticmethod
    def _make_constants(path, n):
        """Write a constants file with n numbered entries."""
        lines = []
        for i in range(1, n + 1):
            lines.append(f"  {i}  CONST_{i:04d}  1.0  ! constant {i}")
        path.write_text("\n".join(lines) + "\n")

    def test_valid_318(self, tmp_path):
        f = tmp_path / "WCONST.txt"
        self._make_constants(f, 318)
        valid, count, msg = validate_constants_file(str(f))
        assert valid is True
        assert count == 318
        assert msg is None

    def test_more_than_required(self, tmp_path):
        """Having more constants than required should still be valid."""
        f = tmp_path / "WCONST.txt"
        self._make_constants(f, 400)
        valid, count, msg = validate_constants_file(str(f))
        assert valid is True
        assert count == 400

    def test_too_few(self, tmp_path):
        f = tmp_path / "WCONST.txt"
        self._make_constants(f, 100)
        valid, count, msg = validate_constants_file(str(f))
        assert valid is False
        assert count == 100
        assert "only 100 constants" in msg

    def test_empty_file(self, tmp_path):
        f = tmp_path / "WCONST.txt"
        f.write_text("")
        valid, count, msg = validate_constants_file(str(f))
        assert valid is False
        assert count == 0

    def test_nonexistent_file(self, tmp_path):
        valid, count, msg = validate_constants_file(str(tmp_path / "nope.txt"))
        assert valid is False
        assert count == 0
        assert "not found" in msg

    def test_none_input(self):
        """Passing None/empty string means no constants file; treated as valid."""
        valid, count, msg = validate_constants_file(None)
        assert valid is True
        assert count == 0
        assert msg is None

    def test_empty_string(self):
        valid, count, msg = validate_constants_file("")
        assert valid is True
        assert count == 0

    def test_comments_and_blanks_ignored(self, tmp_path):
        """Lines starting with # or blank lines should be skipped."""
        f = tmp_path / "WCONST.txt"
        lines = ["# Header comment", ""]
        for i in range(1, 319):
            lines.append(f"  {i}  CONST_{i:04d}  1.0  ! constant {i}")
        lines.append("# footer")
        f.write_text("\n".join(lines) + "\n")
        valid, count, msg = validate_constants_file(str(f))
        assert valid is True
        assert count == 318

    def test_gap_in_numbering(self, tmp_path):
        """File with gaps — max_const_num determines validity, not count of lines."""
        f = tmp_path / "WCONST.txt"
        # Write only constants 1, 2, and 318 (big gap but max == 318)
        lines = [
            "  1  CONST_0001  1.0  ! c1",
            "  2  CONST_0002  1.0  ! c2",
            "  318  CONST_0318  1.0  ! c318",
        ]
        f.write_text("\n".join(lines) + "\n")
        valid, count, msg = validate_constants_file(str(f))
        assert valid is True
        assert count == 318


# ---------------------------------------------------------------------------
# Module-level constants
# ---------------------------------------------------------------------------
class TestConstants:
    def test_pelagic_box_columns_length(self):
        assert len(PELAGIC_BOX_COLUMNS) == 37

    def test_required_model_constants(self):
        assert REQUIRED_MODEL_CONSTANTS == 318

    def test_first_column_is_time(self):
        assert PELAGIC_BOX_COLUMNS[0] == "TIME_DAYS"
