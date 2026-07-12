"""Path-traversal-safe filename resolution.

Extracted from ``app.py`` so the pure path-safety helper can be imported (and
unit-tested) without pulling in the whole Shiny app and its heavy dependencies
(pandas/numpy). Standard library only.
"""

import os


def safe_resolve(base_dir: str, filename: str) -> str:
    """Resolve filename under base_dir, rejecting path traversal.

    Raises ValueError if the resolved path escapes base_dir.
    """
    if not filename or not filename.strip():
        raise ValueError("Empty filename")
    # Reject obvious traversal attempts before joining
    if os.path.isabs(filename) or '..' in filename.split(os.sep):
        raise ValueError(f"Invalid filename: {filename}")
    resolved = os.path.realpath(os.path.join(base_dir, filename))
    base = os.path.realpath(base_dir)
    if not resolved.startswith(base + os.sep) and resolved != base:
        raise ValueError(f"Path escapes base directory: {filename}")
    return resolved
