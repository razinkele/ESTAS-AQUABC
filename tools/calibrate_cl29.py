#!/usr/bin/env python3
"""Identifiability-guided calibration of CL29 against the EPA observations.

Follows the Method-of-Morris screen (docs/CL29_Sensitivity_Analysis.md): optimize the parameters the
data can actually constrain and leave the non-identifiable ones fixed. Reuses the Morris forward model
as-is (symlink-farm worker + perturbed WCONST_04.txt + short-window CL29 + validate_cl29_vs_epa.py as Φ),
so it needs no PEST++ install — it is the sandbox-runnable equivalent of `pestpp-ies` on pest/cl29.pst.

Optimizer: scipy.differential_evolution (global, gradient-free), parallel across the local cores. The
objective adds Chl-a to the five EPA state variables so the optimizer cannot buy nutrient fit with a
Chl-a blow-up (the known nutrient<->biomass multivariate trade-off / structural wall).

    python3 tools/calibrate_cl29.py --popsize 5 --maxiter 12 --workers 24 --days 730

Writes a per-generation checkpoint (JSON) and, on completion, a baseline-vs-optimum per-variable table.
The best-fit is a *candidate* — adopting it as shipped WCONST defaults is a separate scientific decision.
"""
import argparse
import csv
import functools
import json
import math
import os
import shutil
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sensitivity_morris as M  # noqa: E402  (sibling module: forward-model helpers + constants)

# Parameter sets, all drawn from the Morris identifiable tiers (the two non-identifiable params,
# KHS_DSi_DIA and KDISS_DET_PART_ORG_P_20, are excluded from every set and left at their defaults):
#   "all"      — the 8 identifiable params (high + moderate tiers).
#   "nutrient" — the wall-respecting subset: nutrient-cycling/loss/affinity levers ONLY, with the four
#                phyto-biomass knobs (cyano/diatom mortality + both growth rates) FIXED at defaults.
#                The "all" run overfits Chl-a/DO by using the biomass knobs to shave nutrients via uptake;
#                "nutrient" keeps the nutrient gains without inflating biomass (see docs).
PARAM_SETS = {
    "all": [
        ("KD_CYN_20",               "log", 0.04,   0.4),    # cyano mortality  (Morris #1)
        ("K_MIN_DOC_NO3N_20",       "log", 0.3,    3.0),    # denitrification  (#2)
        ("K_NITR_20",               "log", 0.2,    2.0),    # nitrification    (#3)
        ("KG_DIA_OPT_TEMP",         "lin", 1.5,    6.0),    # diatom growth    (#4)
        ("KG_CYN_OPT_TEMP",         "lin", 1.0,    5.0),    # cyano growth     (#5)
        ("KDISS_DET_PART_ORG_N_20", "log", 0.08,   1.0),    # PON->NH4         (#6)
        ("KD_DIA_20",               "log", 0.04,   0.4),    # diatom mortality (#7)
        ("KHS_DIP_DIA",             "log", 0.002,  0.03),   # diatom DIP half-sat (#8)
    ],
    "nutrient": [
        ("K_MIN_DOC_NO3N_20",       "log", 0.3,    3.0),    # denitrification (N loss)
        ("K_NITR_20",               "log", 0.2,    2.0),    # nitrification (NH4->NO3)
        ("KDISS_DET_PART_ORG_N_20", "log", 0.08,   1.0),    # PON->NH4 regeneration
        ("KHS_DIP_DIA",             "log", 0.002,  0.03),   # diatom DIP affinity (PO4 uptake efficiency)
    ],
    # Composition-targeted set for the group-carbon objective (--group-carbon): the
    # screened phytoplankton knobs only, nutrient-cycle rates held at the adopted
    # defaults. Bounds identical to the Morris screen.
    "phyto": [
        ("KD_CYN_20",               "log", 0.04,   0.4),    # cyano mortality  (Morris #1)
        ("KG_CYN_OPT_TEMP",         "lin", 1.0,    5.0),    # cyano growth
        ("KG_DIA_OPT_TEMP",         "lin", 1.5,    6.0),    # diatom growth
        ("KD_DIA_20",               "log", 0.04,   0.4),    # diatom mortality
        ("KHS_DIN_CYN",             "log", 0.003,  0.05),   # cyano DIN affinity (NH4-floor competition)
    ],
    # "phyto" + the zooplankton knobs. The 7-yr group-carbon run (2026-08-08) left ZOO_C
    # bias unchanged to 4 decimals — zooplankton is INERT to phytoplankton parameters, so
    # its 5.5x under-prediction can only be addressed by freeing its own rates. Expect a
    # coupled trade-off: more zooplankton = more grazing = less (already-low) phytoplankton.
    "phyto_zoo": [
        ("KD_CYN_20",               "log", 0.04,   0.4),
        ("KG_CYN_OPT_TEMP",         "lin", 1.0,    5.0),
        ("KG_DIA_OPT_TEMP",         "lin", 1.5,    6.0),
        ("KD_DIA_20",               "log", 0.04,   0.4),
        ("KHS_DIN_CYN",             "log", 0.003,  0.05),
        ("KG_ZOO_OPT_TEMP",         "log", 0.15,   1.5),    # zoo growth (default 0.45)
        ("KD_ZOO_20",               "log", 0.05,   0.5),    # zoo mortality (default 0.15)
        ("FOOD_MIN_ZOO",            "log", 0.005,  0.1),    # feeding threshold (default 0.02)
    ],
    # "phyto" + OPA/FIX-specific knobs: the 5-knob run wins CYN_C by driving OPA and the
    # fixers extinct (competition-only trade-off); this set lets DE defend all 4 groups.
    "phyto_all": [
        ("KD_CYN_20",               "log", 0.04,   0.4),
        ("KG_CYN_OPT_TEMP",         "lin", 1.0,    5.0),
        ("KG_DIA_OPT_TEMP",         "lin", 1.5,    6.0),
        ("KD_DIA_20",               "log", 0.04,   0.4),
        ("KHS_DIN_CYN",             "log", 0.003,  0.05),
        ("KG_OPA_OPT_TEMP",         "lin", 1.0,    6.0),    # OPA growth (default 2.9)
        ("KD_OPA_20",               "log", 0.04,   0.4),    # OPA mortality (default 0.11)
        ("KG_FIX_CYN_OPT_TEMP",     "lin", 1.0,    6.0),    # fixer growth (default 3.5)
        ("KD_FIX_CYN_20",           "log", 0.04,   0.4),    # fixer mortality (default 0.10)
    ],
    # The coupled light-climate set (docs/CL29_phenology_diagnosis.md par. 15): run against
    # a base config with the MEASURED optics imposed (K_B_E 2.18, C:Chl 53/78 via --inputs).
    # Under honest, ~2.7x darker water the production engine needs headroom, so the growth
    # uppers are deliberately wide; the N-cycle four re-balance regeneration underneath.
    "light": [
        ("KG_DIA_OPT_TEMP",         "lin", 2.0,   10.0),
        ("KG_CYN_OPT_TEMP",         "lin", 1.0,    8.0),
        ("KG_FIX_CYN_OPT_TEMP",     "lin", 1.0,    8.0),
        ("KG_OPA_OPT_TEMP",         "lin", 1.0,    8.0),
        ("K_MIN_DOC_NO3N_20",       "log", 0.3,    3.0),
        ("K_NITR_20",               "log", 0.2,    2.0),
        ("KDISS_DET_PART_ORG_N_20", "log", 0.08,   1.0),
        ("KHS_DIP_DIA",             "log", 0.002,  0.03),
    ],
    # Path (c) closure set (doc par. 21): the 'light' engine + C:Chl calibrated
    # WITHIN the measured bounds (312 paired determinations: median 53, IQR
    # 36-78; diatom-dominated ~31). Under the honest base with concentrated
    # self-shading, KG falling back from its bounds is the test that
    # compensation is gone.
    "honest": [
        ("KG_DIA_OPT_TEMP",         "lin", 2.0,   10.0),
        ("KG_CYN_OPT_TEMP",         "lin", 1.0,    8.0),
        ("KG_FIX_CYN_OPT_TEMP",     "lin", 1.0,    8.0),
        ("KG_OPA_OPT_TEMP",         "lin", 1.0,    8.0),
        ("K_MIN_DOC_NO3N_20",       "log", 0.3,    3.0),
        ("K_NITR_20",               "log", 0.2,    2.0),
        ("KDISS_DET_PART_ORG_N_20", "log", 0.08,   1.0),
        ("KHS_DIP_DIA",             "log", 0.002,  0.03),
        ("DIA_C_TO_CHLA",           "lin", 25.0,  53.0),
        ("CYN_C_TO_CHLA",           "lin", 36.0,  78.0),
        ("FIX_CYN_C_TO_CHLA",       "lin", 36.0,  78.0),
    ],
    # Form B closure set (doc par. 48/49): re-fit the constants that were compensating
    # for the 24-h-light error which LIGHT_DAYLENGTH_OPTION=2 removes. Run against a
    # Form B --inputs base.
    #
    # ⚠ This deliberately is NOT the 'light' set, and the reason is measured. In the
    # light-limited regime I_s = GITMAX*CCHL*e/(0.083*PHIMX*XKC) is PROPORTIONAL to
    # GITMAX, so GITMAX cancels out of growth = GITMAX*LIM_LIGHT (doc par. 42). February
    # sits at I/I_s = 0.295, deep inside that regime, so the KG knobs are inert there:
    # measured over 730 days on the Form B base, KG_DIA 8.10 -> 10.0 moves February
    # DIA_C by x1.07 while KD_DIA_20 0.12 -> 0.04 moves it by x3.68. The February
    # compensation lives on the LOSS side; 'light' has no loss term at all and would
    # burn the whole DE budget without touching the metric that matters.
    #
    # C:Chl is excluded on purpose (doc par. 22): handed to the objective it fills the
    # chlorophyll gap with pigment rather than biomass. It is never a calibration knob.
    "formb_closure": [
        ("KD_DIA_20",               "log", 0.04,   0.4),    # THE February lever (x3.68)
        ("KD_CYN_20",               "log", 0.04,   0.4),    # same lever for CYN
        ("KG_DIA_OPT_TEMP",         "lin", 2.0,   10.0),    # bites in summer, not winter
        ("KG_CYN_OPT_TEMP",         "lin", 1.0,    8.0),
        ("KG_FIX_CYN_OPT_TEMP",     "lin", 1.0,    8.0),
        ("K_NITR_20",               "log", 0.2,    2.0),
        ("KDISS_DET_PART_ORG_N_20", "log", 0.08,   1.0),
        ("KHS_DIP_DIA",             "log", 0.002,  0.03),
    ],
    # Staged-fixer set (doc par. 30): tunes the staged NOST guild + the demoted FIX_CYN
    # surrogate on a staging-enabled --inputs base (the T4 hand-optimum). The staging
    # option-file scalars (T_GERM_AKI_STAGE/I_FORM_AKI/KR_GERM_BED) stay fixed at T4's
    # values — this harness perturbs WCONST only. Run with --group-carbon so the fixer
    # composition (obs FIX_CYN_C vs model FIX+NOST) stays in the objective.
    "staged": [
        ("KG_NOST_VEG_HET_OPT_TEMP", "lin", 2.0,   8.0),    # staged-guild growth (T4: 7.6)
        ("KD_NOST_VEG_HET_20",       "log", 0.02,  0.2),    # staged-guild mortality (default 0.04)
        ("KHS_DN_NOST_VEG_HET",      "log", 0.003, 0.03),   # staged-guild N affinity
        ("KHS_DP_NOST_VEG_HET",      "log", 0.002, 0.03),   # staged-guild P affinity
        ("KG_FIX_CYN_OPT_TEMP",      "lin", 0.5,   3.0),    # demoted surrogate growth (T4: 1.29)
        ("KD_FIX_CYN_20",            "log", 0.04,  0.4),    # surrogate mortality (default 0.10)
    ],
}
CAL_PARAMS = PARAM_SETS["all"]   # overridden by --paramset in main()
CAL_PHI_VARS = ["NH4", "NO3", "PO4", "DO", "Si", "CHLA"]  # 5 EPA state vars + Chl-a guardrail
# Group-carbon terms appended to Φ under --group-carbon (obs: tools/ingest_km_plankton.py;
# the validator scores obs FIX_CYN_C against model FIX_CYN_C+NOST_VEG_HET_C). ZOO_C joined
# 2026-08-08 once the AAA NDJSON extension gave it in-window coverage (329 station-dates
# 2016-2022) — a grazing-side constraint the phyto-only objective lacked.
GROUP_VARS = ["DIA_C", "CYN_C", "FIX_CYN_C", "OPA_C", "ZOO_C"]
USE_GROUP_CARBON = False     # set by --group-carbon in main() (before the worker fork)
PLANKTON_OBS = None          # resolved in module scope below
PENALTY = 1.0e6          # objective value for a failed forward run
DAYS = 730              # optimization window (days); overridden from --days
WORKDIR = "/tmp/cal_work"
CKPT = "/tmp/cal_work/checkpoint.json"

PLANKTON_OBS = os.path.join(M.REPO, "km_plankton_out", "km_plankton_tidy.csv")


def _agg_metrics(csv_path, agg):
    """Accumulate a validator metrics CSV into {var: [n*rmse², n, n*obs, n*mod, n*bias]}."""
    with open(csv_path) as f:
        for row in csv.DictReader(f):
            v = row["variable"]
            n = float(row["n"] or 0)
            if n <= 0:
                continue
            a = agg.setdefault(v, [0.0, 0.0, 0.0, 0.0, 0.0])
            a[0] += n * float(row["rmse"]) ** 2
            a[1] += n
            a[2] += n * float(row["obs_mean"])
            a[3] += n * float(row["model_mean"])
            a[4] += n * float(row["bias"])


def _metrics_rows(out_dir, tag):
    """Run the validator on out_dir and return {var: (rmse, obs_mean, model_mean, bias, n)} n-weighted.

    With USE_GROUP_CARBON, the plankton-carbon observations are scored in a second
    validator pass and merged (the variable sets are disjoint).
    """
    obs_sets = [(M.OBS, "")]
    if USE_GROUP_CARBON:
        obs_sets.append((PLANKTON_OBS, "_plk"))
    agg = {}
    for obs, suffix in obs_sets:
        val = os.path.join(out_dir, f"val_{tag}{suffix}")
        cmd = ["python3", M.VALIDATOR, "--outputs", os.path.join(out_dir, "OUT"),
               "--obs", obs, "--base-year", "2012", "--out", val, "--no-plots"]
        # Score with the run's own C:Chl ratios — they are model parameters (they drive
        # self-shading), so deriving Chl-a with different ones would silently mis-score a
        # run whose ratios were perturbed. A no-op when they are at their defaults.
        wconst = os.path.join(out_dir, "INPUTS_CL29", "WCONST_04.txt")
        if os.path.exists(wconst):
            cmd += ["--wconst", wconst]
        subprocess.run(cmd, cwd=M.REPO,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=120)
        path = os.path.join(val, "validation_metrics.csv")
        if os.path.exists(path):     # a source with no in-window obs writes no CSV
            _agg_metrics(path, agg)
    out = {}
    for v, (ssr, n, om, mm, bs) in agg.items():
        out[v] = (math.sqrt(ssr / n), om / n, mm / n, bs / n, int(n))
    return out


def _phi_from_rows(rows, phi_vars):
    """Φ = Σ_var (1/obs_mean)·RMSE over phi_vars (same weighting as the PEST objective)."""
    phi = 0.0
    for v in phi_vars:
        if v not in rows:
            continue
        rmse, om = rows[v][0], rows[v][1]
        if om > 1e-9:
            phi += rmse / om
    return phi


def _run(values, tag):
    """Set up a worker dir (perturbed WCONST if `values` else baseline), run CL29, return (out_dir, ok)."""
    wd = os.path.join(WORKDIR, f"cal_{tag}")
    shutil.rmtree(wd, ignore_errors=True)
    os.makedirs(os.path.join(wd, "INPUTS_CL29"), exist_ok=True)
    os.makedirs(os.path.join(wd, "OUT"), exist_ok=True)
    for fn in os.listdir(M.SRC_INPUTS):
        dst = os.path.join(wd, "INPUTS_CL29", fn)
        if fn == "WCONST_04.txt" and values is not None:
            M._write_perturbed_wconst(os.path.join(M.SRC_INPUTS, fn), dst, values)
        else:
            os.symlink(os.path.join(M.SRC_INPUTS, fn), dst)
    with open(os.path.join(M.REPO, "INPUT_CL29.txt")) as f:
        lines = f.readlines()
    out = []
    for ln in lines:
        if ln.startswith("         4016.0"):
            out.append(f"{float(DAYS):15.1f}\n")
        elif ln.startswith("             10"):
            out.append("            240\n")
        elif ln.startswith("OUTPUTS_CL29/"):
            out.append("OUT/\n")
        else:
            out.append(ln)
    with open(os.path.join(wd, "INPUT.txt"), "w") as f:
        f.writelines(out)
    env = dict(os.environ, ESTAS_HOLD_VOLUME="1")
    # Timeout must scale with the window (a full-record run is ~11 min solo and ~2x under contention);
    # a fixed 900s silently times out every concurrent full-record eval -> PENALTY -> corrupted DE.
    tmo = max(1800, int(DAYS * 2.0))
    r = subprocess.run([M.BIN, "INPUT.txt"], cwd=wd, env=env,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=tmo)
    ok = (r.returncode == 0 and os.path.exists(os.path.join(wd, "OUT", "PELAGIC_BOX_00023.out")))
    return wd, ok


def _values_from_x(x):
    return {name: M.x_to_value(x[i], tr, lo, hi) for i, (name, tr, lo, hi) in enumerate(CAL_PARAMS)}


def _x_from_value(v, transform, lo, hi):
    """Inverse of M.x_to_value, clipped inside the open unit interval."""
    x = math.log(v / lo) / math.log(hi / lo) if transform == "log" else (v - lo) / (hi - lo)
    return min(0.98, max(0.02, x))


def _x0_from_seed(seed_path):
    """Build a DE x0 from a previous result.json: seeded values where present, WCONST defaults else."""
    seed = json.load(open(seed_path)).get("values", {})
    with open(os.path.join(M.SRC_INPUTS, "WCONST_04.txt")) as wf:
        defaults = {p[1]: float(p[2]) for p in (ln.split() for ln in wf) if len(p) >= 3}
    return [_x_from_value(seed.get(name, defaults[name]), tr, lo, hi)
            for name, tr, lo, hi in CAL_PARAMS]


def _apply_cfg(cfg):
    """Install the run configuration into module globals (needed in pool workers).

    ⚠ scipy's internal Pool may use the *forkserver* start method, whose workers do NOT
    inherit globals set in main() after import (that silent assumption produced an
    all-PENALTY 'converged' DE: workers saw the default 8-param set with a shorter x →
    instant IndexError). The objective therefore carries its configuration explicitly
    (functools.partial) and installs it per call — correct under fork, forkserver and
    spawn alike.
    """
    global DAYS, WORKDIR, USE_GROUP_CARBON, CAL_PHI_VARS, CAL_PARAMS
    DAYS, WORKDIR = cfg["days"], cfg["workdir"]
    USE_GROUP_CARBON = cfg["group_carbon"]
    CAL_PHI_VARS = cfg["phi_vars"]
    CAL_PARAMS = PARAM_SETS[cfg["paramset"]]


def evaluate(x, cfg=None):
    """DE objective: Φ over the calibration variables for normalized parameter vector x∈[0,1]^n."""
    try:
        if cfg is not None:
            _apply_cfg(cfg)
        wd, ok = _run(_values_from_x(x), f"{os.getpid()}")
        if not ok:
            return PENALTY
        rows = _metrics_rows(wd, "opt")
        phi = _phi_from_rows(rows, CAL_PHI_VARS)
        shutil.rmtree(wd, ignore_errors=True)
        return phi if phi > 0 else PENALTY
    except Exception:
        return PENALTY


def _report_table(base_rows, best_rows):
    print(f"\n{'var':<6}{'obs_mean':>10}{'model(base)':>13}{'model(best)':>13}"
          f"{'bias(base)':>12}{'bias(best)':>12}{'RMSE(base)':>12}{'RMSE(best)':>12}  n")
    for v in CAL_PHI_VARS + ["TN", "TP"]:
        b = base_rows.get(v)
        o = best_rows.get(v)
        if not b or not o:
            continue
        print(f"{v:<6}{b[1]:>10.4g}{b[2]:>13.4g}{o[2]:>13.4g}"
              f"{b[3]:>+12.4g}{o[3]:>+12.4g}{b[0]:>12.4g}{o[0]:>12.4g}  {b[4]}")


def main():
    global DAYS, WORKDIR, CKPT, CAL_PARAMS, CAL_PHI_VARS, USE_GROUP_CARBON
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--paramset", choices=list(PARAM_SETS), default="all",
                    help="'all' = 8 identifiable params; 'nutrient' = wall-respecting cycling-only "
                         "subset; 'phyto' = composition-targeted phytoplankton knobs")
    ap.add_argument("--group-carbon", action="store_true",
                    help="append the plankton group-carbon terms (DIA_C/CYN_C/FIX_CYN_C/OPA_C, "
                         "km_plankton_out obs) to Φ — the window must cover 2015 (>=1461 days) "
                         "for the in-window composition data to bite")
    ap.add_argument("--popsize", type=int, default=5)
    ap.add_argument("--maxiter", type=int, default=12)
    ap.add_argument("--tol", type=float, default=0.02)
    ap.add_argument("--days", type=int, default=730)
    ap.add_argument("--workers", type=int, default=min(24, (os.cpu_count() or 4) - 2))
    ap.add_argument("--seed", type=int, default=7)
    ap.add_argument("--workdir", default="/tmp/cal_work")
    ap.add_argument("--inputs", default=None,
                   help="alternate INPUTS_CL29 folder to calibrate against (e.g. a copy with "
                        "the measured light climate imposed); default = the repo inputs")
    ap.add_argument("--seed-result", default=None,
                    help="result.json of a previous run: its values (plus WCONST defaults for new "
                         "params) become the DE x0 seed individual")
    ap.add_argument("--validate-full", type=int, default=0,
                    help="after optimizing, validate the best on this many days (e.g. 4016 = full record)")
    a = ap.parse_args()
    if a.inputs:
        M.SRC_INPUTS = os.path.abspath(a.inputs)
        print(f"calibrating against inputs: {M.SRC_INPUTS}")
    DAYS, WORKDIR = a.days, a.workdir
    CAL_PARAMS = PARAM_SETS[a.paramset]
    CKPT = os.path.join(WORKDIR, "checkpoint.json")
    os.makedirs(WORKDIR, exist_ok=True)
    if not os.path.exists(M.BIN):
        raise SystemExit("ESTAS_II not built (make build-estas)")
    if a.group_carbon:
        if not os.path.exists(PLANKTON_OBS):
            raise SystemExit(f"plankton obs not found ({PLANKTON_OBS}) — run tools/ingest_km_plankton.py")
        USE_GROUP_CARBON = True
        CAL_PHI_VARS = CAL_PHI_VARS + GROUP_VARS
        if a.days < 1461:
            print(f"WARNING: --days {a.days} < 1461 — the 2015 composition observations "
                  "fall outside the window; group terms will not constrain anything")

    from scipy.optimize import differential_evolution

    print(f"Calibrating {len(CAL_PARAMS)} identifiable params over {'/'.join(CAL_PHI_VARS)} "
          f"({a.days}-day window, DE popsize={a.popsize} maxiter={a.maxiter}, {a.workers} workers)")
    print("baseline (current WCONST defaults):")
    base_wd, ok = _run(None, "baseline")
    base_rows = _metrics_rows(base_wd, "base") if ok else {}
    base_phi = _phi_from_rows(base_rows, CAL_PHI_VARS)
    print(f"  baseline Φ = {base_phi:.4f}")

    gen = {"n": 0}

    def cb(xk, convergence=None):
        gen["n"] += 1
        vals = _values_from_x(xk)
        with open(CKPT, "w") as f:
            json.dump({"generation": gen["n"], "x": list(xk), "values": vals,
                       "convergence": convergence}, f, indent=2)
        print(f"  gen {gen['n']:>2}: best {', '.join(f'{k}={v:.4g}' for k, v in vals.items())}",
              flush=True)

    cfg = {"days": DAYS, "workdir": WORKDIR, "group_carbon": USE_GROUP_CARBON,
           "phi_vars": list(CAL_PHI_VARS), "paramset": a.paramset}
    x0 = _x0_from_seed(a.seed_result) if a.seed_result else None
    if x0 is not None:
        print("seeding x0 from", a.seed_result, "->",
              ", ".join(f"{k}={v:.4g}" for k, v in _values_from_x(x0).items()))
    result = differential_evolution(
        functools.partial(evaluate, cfg=cfg), bounds=[(0.0, 1.0)] * len(CAL_PARAMS),
        popsize=a.popsize, maxiter=a.maxiter, tol=a.tol, mutation=(0.5, 1.0), recombination=0.7,
        init="latinhypercube", polish=False, updating="deferred", workers=a.workers,
        seed=a.seed, callback=cb, x0=x0)

    best_vals = _values_from_x(result.x)
    print(f"\n=== calibration result ===\nbest Φ = {result.fun:.4f}  (baseline {base_phi:.4f}, "
          f"{100*(base_phi-result.fun)/base_phi:+.1f}%)   nfev={result.nfev}")
    print("best parameters (default → calibrated):")
    with open(os.path.join(M.SRC_INPUTS, "WCONST_04.txt")) as wf:
        defaults = {p[1]: float(p[2]) for p in (ln.split() for ln in wf) if len(p) >= 3}
    for name, *_ in CAL_PARAMS:
        print(f"  {name:<26} {defaults.get(name, float('nan')):>10.4g} → {best_vals[name]:.4g}")

    best_wd, ok = _run(best_vals, "best")
    best_rows = _metrics_rows(best_wd, "best") if ok else {}
    _report_table(base_rows, best_rows)
    with open(os.path.join(WORKDIR, "result.json"), "w") as f:
        json.dump({"best_phi": result.fun, "base_phi": base_phi, "values": best_vals,
                   "nfev": result.nfev}, f, indent=2)

    if a.validate_full:
        print(f"\n=== full-record validation ({a.validate_full} days) ===")
        DAYS = a.validate_full
        fb_wd, ok = _run(None, "full_base")
        fb = _metrics_rows(fb_wd, "fbase") if ok else {}
        fo_wd, ok = _run(best_vals, "full_best")
        fo = _metrics_rows(fo_wd, "fbest") if ok else {}
        _report_table(fb, fo)


if __name__ == "__main__":
    main()
