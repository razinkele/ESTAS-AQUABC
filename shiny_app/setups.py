"""Model-application setup registry for the Shiny UI.

A Setup bundles a complete model configuration (input file + inputs/outputs dirs +
box count + run env). The registry is the single source of truth so box count,
directories, and env become data rather than hardcoded constants.
"""
from __future__ import annotations

import os
from dataclasses import dataclass, field


@dataclass(frozen=True)
class Setup:
    id: str
    name: str
    description: str
    input_file: str        # default repo-root config
    inputs_dir: str
    output_dir: str
    box_count: int
    env: dict = field(default_factory=dict)
    required_input: str = "PELAGIC_INPUTS.txt"   # availability sentinel (ESTAS reads it first)
    unavailable_hint: str = ""


SETUPS = [
    Setup("standard", "Standard (25-box)",
          "The default AQUABC pelagic configuration (committed INPUTS/).",
          "INPUT.txt", "INPUTS", "OUTPUTS", 25),
    Setup("cl29", "CL29 — Curonian Lagoon (29-box)",
          "EUTROPY-derived 29-box Curonian Lagoon; requires ESTAS_HOLD_VOLUME=1.",
          "INPUT_CL29.txt", "INPUTS_CL29", "OUTPUTS_CL29", 29,
          env={"ESTAS_HOLD_VOLUME": "1"},
          unavailable_hint="Generate inputs: python tools/eutropy_poc/eutropy_to_estas.py"),
]

_BY_ID = {s.id: s for s in SETUPS}


def list_setups():
    return list(SETUPS)


def default_setup():
    return SETUPS[0]


def get_setup(setup_id):
    return _BY_ID.get(setup_id, default_setup())


def is_available(setup, root):
    return os.path.isfile(os.path.join(root, setup.inputs_dir, setup.required_input))


def _declared_input_folder(path):
    """Return the PELAGIC_INPUT_FOLDER value (folder name, no trailing slash) or ''."""
    try:
        with open(path) as fh:
            lines = fh.read().splitlines()
        for i, line in enumerate(lines):
            if "PELAGIC MODEL INPUT FOLDER" in line:
                for nxt in lines[i + 1:]:
                    if nxt.strip():
                        return nxt.strip().rstrip("/")
    except OSError:
        pass
    return ""


def input_files_for(setup, root):
    """Repo-root INPUT*.txt files whose declared input folder matches setup.inputs_dir."""
    out = []
    for f in sorted(os.listdir(root)):
        if f.startswith("INPUT") and f.endswith(".txt"):
            if _declared_input_folder(os.path.join(root, f)) == setup.inputs_dir:
                out.append(f)
    return out
