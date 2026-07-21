#!/usr/bin/env python3
"""Generate the PEST(++) calibration files for CL29 from the KM observations.

Produces, in pest/:
  wconst_04.tpl   template  -> INPUTS_CL29/WCONST_04.txt   (the calibrated parameters)
  model_obs.ins   instruction file -> pest/model_obs.out
  cl29.pst        PEST control file, configured for pestpp-ies

The observations come from tools/ingest_km_observations.py (km_observations_tidy.csv),
restricted to the model window (day <= SIMULATION_END). One PEST observation per
(box, variable, date); obs group = variable; weight = 1/group-mean-|value| so the seven
variables contribute comparably to the objective function.

WCONST_04.txt lines are `index NAME VALUE ! comment`; READ_MODEL_CONSTANTS reads
`index name value` free-format and stores by index, so replacing the VALUE token with a
`@NAME@` field keeps the line parse-compatible.

Run from the repo root:  python pest/build_pest.py
Standard library only.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import os

MARK = "@"
FIELD = 18   # width of a templated @NAME@ value field (>= number width)

# WCONST parameter -> (obs-driving bias, group, transform, lower, upper). Initial value is
# read from WCONST_04.txt at build time. Edit this list to re-target the calibration.
PARAMS = [
    ("K_MIN_DOC_NO3N_20",       "denit",   "log",  0.1,   5.0),
    ("KDISS_DET_PART_ORG_P_20", "pmin",    "log",  0.1,   10.0),
    ("KHS_DSi_DIA",             "diatom",  "log",  0.005, 0.15),
    ("KG_DIA_OPT_TEMP",         "diatom",  "none", 1.0,   6.0),
    ("KD_DIA_20",               "diatom",  "log",  0.02,  0.5),
]

# KM variable -> PEST obs-name stem (short; obs name = stem_box_seq, <= 20 chars). Only the
# variables the validator can compare (validate_cl29_vs_epa.MODEL_COL) are used.
VARSTEM = {"NH4": "nh4", "NO3": "no3", "PO4": "po4", "Si": "si",
           "TN": "tn", "TP": "tp", "CHLA": "chla"}


def read_wconst(path):
    """Return (lines, {name: (lineno, value_token, value_float)}) for a WCONST file."""
    lines = open(path).read().splitlines()
    index = {}
    for i, ln in enumerate(lines):
        code = ln.split("!", 1)[0]
        toks = code.split()
        if len(toks) >= 3:                       # index NAME VALUE
            name, valtok = toks[1], toks[-1]
            try:
                index[name] = (i, valtok, float(valtok))
            except ValueError:
                pass
    return lines, index


def template_line(line, name):
    """Replace the VALUE token of a WCONST line with a @NAME@ field."""
    code, sep, comment = line.partition("!")
    valtok = code.split()[-1]
    marker = MARK + name.ljust(max(FIELD - 2, len(name))) + MARK
    pos = code.rfind(valtok)
    new_code = code[:pos] + marker + " " + code[pos + len(valtok):].lstrip()
    return new_code + (sep + comment if sep else "")


def write_tpl(wconst_path, out_tpl):
    lines, index = read_wconst(wconst_path)
    missing = [p for p, *_ in PARAMS if p not in index]
    if missing:
        raise SystemExit(f"parameters not found in {wconst_path}: {missing}")
    for name, *_ in PARAMS:
        i = index[name][0]
        lines[i] = template_line(lines[i], name)
    with open(out_tpl, "w") as fh:
        fh.write(f"ptf {MARK}\n")
        fh.write("\n".join(lines) + "\n")
    return {name: index[name][2] for name, *_ in PARAMS}   # initial values


def load_obs(tidy_csv, base_year, end_day):
    """KM tidy CSV -> ordered list of (obsname, box, var, date, value) within the window."""
    base = dt.date(base_year, 1, 1)
    recs = []
    seq = {}
    with open(tidy_csv, newline="") as fh:
        for r in csv.DictReader(fh):
            var = r["variable"]
            if var not in VARSTEM:
                continue
            date = dt.date.fromisoformat(r["date"])
            off = (date - base).days
            if not (0 <= off <= end_day):
                continue
            box = int(r["box"])
            k = (var, box)
            seq[k] = seq.get(k, 0) + 1
            name = f"{VARSTEM[var]}_{box}_{seq[k]:03d}"
            recs.append((name, box, var, date, float(r["value"])))
    return recs


def obs_weights(recs):
    """weight = 1 / mean(|value|) within each variable group (scale-balancing)."""
    tot, cnt = {}, {}
    for _, _, var, _, val in recs:
        tot[var] = tot.get(var, 0.0) + abs(val)
        cnt[var] = cnt.get(var, 0) + 1
    return {var: (cnt[var] / tot[var] if tot[var] > 0 else 1.0) for var in tot}


def write_ins(recs, out_ins):
    with open(out_ins, "w") as fh:
        fh.write(f"pif {MARK}\n")
        for name, *_ in recs:
            fh.write(f"l1 !{name}!\n")


def write_pst(params, inits, recs, out_pst, tpl_rel, ins_rel, out_rel,
              wconst_rel, command, noptmax, num_reals):
    pgroups = sorted({g for _, g, *_ in params})
    ogroups = sorted({VARSTEM[var] for _, _, var, _, _ in recs})
    w = obs_weights(recs)
    L = []
    L.append("pcf")
    L.append("* control data")
    L.append("restart estimation")
    L.append(f"{len(params)} {len(recs)} {len(pgroups)} 0 {len(ogroups)}")
    L.append("1 1 single point 1 0 0")
    L.append("10.0 -3.0 0.3 0.03 10")
    L.append("10.0 10.0 0.001")
    L.append("0.1")
    L.append(f"{noptmax} 0.005 4 4 0.005 4")
    L.append("1 1 1")
    L.append("* parameter groups")
    for g in pgroups:
        L.append(f"{g} relative 0.01 0.0 switch 2.0 parabolic")
    L.append("* parameter data")
    for name, g, tr, lo, hi in params:
        L.append(f"{name} {tr} factor {inits[name]:.7E} {lo} {hi} {g} 1.0 0.0 1")
    L.append("* observation groups")
    for g in ogroups:
        L.append(g)
    L.append("* observation data")
    for name, _box, var, _date, val in recs:
        L.append(f"{name} {val:.6g} {w[var]:.6g} {VARSTEM[var]}")
    L.append("* model command line")
    L.append(command)
    L.append("* model input/output")
    L.append(f"{tpl_rel} {wconst_rel}")
    L.append(f"{ins_rel} {out_rel}")
    L.append("* prior information")
    L.append(f"++ies_num_reals({num_reals})")
    L.append("++ies_add_base(true)")
    L.append("++ies_bad_phi(1e30)")
    with open(out_pst, "w") as fh:
        fh.write("\n".join(L) + "\n")


def read_end_day(input_file):
    lines = open(input_file).read().splitlines()
    for i, ln in enumerate(lines):
        if "SIMULATION_END" in ln and i + 1 < len(lines):
            return int(float(lines[i + 1].split()[0]))
    return 4016


def main(argv=None):
    here = os.path.dirname(os.path.abspath(__file__))
    root = os.path.dirname(here)
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--wconst", default=os.path.join(root, "INPUTS_CL29", "WCONST_04.txt"))
    p.add_argument("--obs", default=os.path.join(here, "km_observations_tidy.csv"),
                   help="km_observations_tidy.csv from tools/ingest_km_observations.py")
    p.add_argument("--input-file", default=os.path.join(root, "INPUT_CL29.txt"))
    p.add_argument("--base-year", type=int, default=2012)
    p.add_argument("--noptmax", type=int, default=3, help="pestpp-ies iterations")
    p.add_argument("--num-reals", type=int, default=50, help="pestpp-ies ensemble size")
    a = p.parse_args(argv)

    inits = write_tpl(a.wconst, os.path.join(here, "wconst_04.tpl"))
    end_day = read_end_day(a.input_file)
    recs = load_obs(a.obs, a.base_year, end_day)
    if not recs:
        raise SystemExit(f"no observations within the model window (day<= {end_day}) in {a.obs}")
    write_ins(recs, os.path.join(here, "model_obs.ins"))
    write_pst(PARAMS, inits, recs,
              os.path.join(here, "cl29.pst"),
              tpl_rel="pest/wconst_04.tpl", ins_rel="pest/model_obs.ins",
              out_rel="pest/model_obs.out", wconst_rel="INPUTS_CL29/WCONST_04.txt",
              command="python pest/forward_run.py",
              noptmax=a.noptmax, num_reals=a.num_reals)
    ng = len({VARSTEM[v] for _, _, v, _, _ in recs})
    print(f"wrote pest/wconst_04.tpl ({len(PARAMS)} params), pest/model_obs.ins + "
          f"pest/cl29.pst ({len(recs)} obs in {ng} groups, window day<= {end_day})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
