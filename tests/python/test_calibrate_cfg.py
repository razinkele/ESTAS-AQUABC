"""--inputs must reach the differential-evolution workers.

scipy runs differential_evolution's pool under forkserver (scipy >= 1.17 MapWrapper on Python < 3.14), so each worker
re-imports calibrate_cl29 and sees none of the globals main() set. The run configuration therefore
travels with every call and is installed by _apply_cfg. The input folder was missing from it:
every --inputs calibration silently built its trials from the default INPUTS_CL29, and in a git
worktree, where that folder does not exist, every trial scored as a penalty and the search
"converged" after one generation (2026-11-07).
"""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tools"))

import calibrate_cl29 as cal  # noqa: E402


def test_run_cfg_carries_the_input_folder(monkeypatch):
    monkeypatch.setattr(cal.M, "SRC_INPUTS", "/somewhere/INPUTS_X")
    cfg = cal._run_cfg("light")
    assert cfg["src_inputs"] == "/somewhere/INPUTS_X"
    assert cfg["paramset"] == "light"


def test_evaluate_installs_the_input_folder_before_building_a_trial(monkeypatch):
    # A fresh forkserver worker sees the module default, not what main() set.
    monkeypatch.setattr(cal.M, "SRC_INPUTS", "/default/INPUTS_CL29")
    for name in ("DAYS", "WORKDIR", "USE_GROUP_CARBON", "CAL_PHI_VARS", "CAL_PARAMS"):
        monkeypatch.setattr(cal, name, getattr(cal, name))
    seen = {}

    def fake_run(values, tag):
        seen["src_inputs"] = cal.M.SRC_INPUTS
        return "/nonexistent", False

    monkeypatch.setattr(cal, "_run", fake_run)
    cfg = {"days": 730, "workdir": "/tmp/w", "group_carbon": False, "phi_vars": ["NH4"],
           "paramset": "light", "src_inputs": "/somewhere/INPUTS_X"}
    cal.evaluate([0.5] * len(cal.PARAM_SETS["light"]), cfg=cfg)
    assert seen["src_inputs"] == "/somewhere/INPUTS_X"
