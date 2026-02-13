"""Tests for the safe_resolve path traversal protection in app.py."""

import os

import pytest

from shiny_app.app import safe_resolve


@pytest.fixture
def base_dir(tmp_path):
    """Create a temporary base directory with a test file."""
    test_file = tmp_path / "test.txt"
    test_file.write_text("hello")
    sub = tmp_path / "sub"
    sub.mkdir()
    (sub / "nested.txt").write_text("nested")
    return str(tmp_path)


class TestSafeResolve:
    def test_normal_filename(self, base_dir):
        result = safe_resolve(base_dir, "test.txt")
        assert result == os.path.join(base_dir, "test.txt")

    def test_nested_filename(self, base_dir):
        result = safe_resolve(base_dir, os.path.join("sub", "nested.txt"))
        assert result == os.path.join(base_dir, "sub", "nested.txt")

    def test_rejects_parent_traversal(self, base_dir):
        with pytest.raises(ValueError, match="Invalid filename"):
            safe_resolve(base_dir, "../etc/passwd")

    def test_rejects_double_traversal(self, base_dir):
        with pytest.raises(ValueError, match="Invalid filename"):
            safe_resolve(base_dir, "sub/../../etc/passwd")

    def test_rejects_absolute_path(self, base_dir):
        with pytest.raises(ValueError, match="Invalid filename"):
            safe_resolve(base_dir, "/etc/passwd")

    def test_rejects_empty_filename(self, base_dir):
        with pytest.raises(ValueError, match="Empty filename"):
            safe_resolve(base_dir, "")

    def test_rejects_whitespace_filename(self, base_dir):
        with pytest.raises(ValueError, match="Empty filename"):
            safe_resolve(base_dir, "   ")

    def test_rejects_none_filename(self, base_dir):
        with pytest.raises((ValueError, TypeError)):
            safe_resolve(base_dir, None)

    def test_returns_realpath(self, base_dir):
        result = safe_resolve(base_dir, "test.txt")
        assert os.path.isabs(result)
        assert result == os.path.realpath(result)
