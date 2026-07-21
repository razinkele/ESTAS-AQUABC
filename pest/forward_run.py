#!/usr/bin/env python3
"""PEST(++) forward model for CL29.

PEST first writes INPUTS_CL29/WCONST_04.txt from pest/wconst_04.tpl with the trial parameter
values. This script then runs the model and writes pest/model_obs.out — one modelled value per
observation, in the exact order build_pest.py emitted (so it lines up with model_obs.ins):

  1. ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt        (-> OUTPUTS_CL29/)
  2. for each (box, variable, date) obs: interpolate the modelled series to the obs date,
     reusing tools/validate_cl29_vs_epa.py so the scoring matches the validation exactly.

Run from the (worker) directory that holds ESTAS_II, INPUT_CL29.txt, INPUTS_CL29/,
km_observations_tidy.csv and pest/.  Paths are resolved relative to this file's repo root.
"""
from __future__ import annotations

import datetime as dt
import os
import subprocess
import sys

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(ROOT, "tools"))

import build_pest  # noqa: E402  (pest/build_pest.py — identical obs ordering)
from validate_cl29_vs_epa import MODEL_COL, load_box_output  # noqa: E402  (tools/)

BASE_YEAR = 2012


def run_model(root):
    env = dict(os.environ, ESTAS_HOLD_VOLUME="1")
    subprocess.run(["./ESTAS_II", "INPUT_CL29.txt"], cwd=root, env=env, check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)


def main():
    tidy = os.path.join(ROOT, "pest", "km_observations_tidy.csv")
    end_day = build_pest.read_end_day(os.path.join(ROOT, "INPUT_CL29.txt"))
    recs = build_pest.load_obs(tidy, BASE_YEAR, end_day)   # same order as the .pst/.ins

    run_model(ROOT)

    base = dt.date(BASE_YEAR, 1, 1)
    out_dir = os.path.join(ROOT, "OUTPUTS_CL29")
    cache = {}

    def series(box):
        if box not in cache:
            path = os.path.join(out_dir, f"PELAGIC_BOX_{box:05d}.out")
            cache[box] = load_box_output(path, BASE_YEAR)
        return cache[box]

    vals = []
    for _name, box, var, date, _obsval in recs:
        df = series(box)
        col = MODEL_COL[var]
        y = df[col].to_numpy(float) if col in df.columns else np.zeros(len(df))
        vals.append(float(np.interp((date - base).days,
                                    df["TIME_DAYS"].to_numpy(float), y)))

    with open(os.path.join(HERE, "model_obs.out"), "w") as fh:
        fh.write("\n".join(f"{v:.6E}" for v in vals) + "\n")


if __name__ == "__main__":
    main()
