#!/usr/bin/env python3
"""Method-of-Morris identifiability screen for CL29 WCONST model constants.

Answers "which parameters can the EPA data actually constrain?" (paper 11.2 identifiability) with a
cheap elementary-effects screen. Reuses the existing forward model as-is: perturb a curated set of
WCONST_04.txt constants, run a short-window CL29, and score misfit Phi with tools/validate_cl29_vs_epa.py
(the same objective the PEST harness uses). No external dependency beyond the validator; parallel via a
symlink-farm (each worker is a dir of symlinks to INPUTS_CL29 + a perturbed WCONST_04.txt + a short driver).

For a full-record, industry-standard run use PEST++'s `pestpp-sen` on pest/cl29.pst
(see docs/CL29_Calibration_PEST_Workflow.md); this tool is the sandbox-runnable equivalent.

    python3 tools/sensitivity_morris.py --trajectories 6 --workers 24 --days 730

Outputs a mu*/sigma ranking (mu* = mean |elementary effect| = influence; sigma = spread =
nonlinearity/interaction), most-influential first.
"""
import argparse
import csv
import math
import os
import random
import shutil
import subprocess
from concurrent.futures import ProcessPoolExecutor

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BIN = os.path.join(REPO, "ESTAS_II")
OBS = os.path.join(REPO, "epa_observations_out", "epa_observations_tidy.csv")
SRC_INPUTS = os.path.join(REPO, "INPUTS_CL29")
VALIDATOR = os.path.join(REPO, "tools", "validate_cl29_vs_epa.py")

# Curated calibration-relevant constants where the EPA/KM data plausibly has leverage.
# (name, transform, lower, upper)  -- 'log' = multiplicative range, 'lin' = linear.
PARAMS = [
    ("K_MIN_DOC_NO3N_20",       "log", 0.3,    3.0),    # denitrification (NO3)
    ("KDISS_DET_PART_ORG_P_20", "log", 1.0,    10.0),   # POP dissolution (PO4)
    ("KDISS_DET_PART_ORG_N_20", "log", 0.08,   1.0),    # PON dissolution (NH4)
    ("KHS_DSi_DIA",             "log", 0.004,  0.05),   # diatom Si half-saturation
    ("KG_DIA_OPT_TEMP",         "lin", 1.5,    6.0),    # diatom growth rate
    ("KD_DIA_20",               "log", 0.04,   0.4),    # diatom mortality
    ("KG_CYN_OPT_TEMP",         "lin", 1.0,    5.0),    # cyano growth rate
    ("KD_CYN_20",               "log", 0.04,   0.4),    # cyano mortality
    ("KHS_DIN_DIA",             "log", 0.003,  0.05),   # diatom DIN half-saturation
    ("KHS_DIP_DIA",             "log", 0.002,  0.03),   # diatom DIP half-saturation
    ("KHS_DIN_CYN",             "log", 0.003,  0.05),   # cyano DIN half-saturation
    ("KHS_DIP_CYN",             "log", 0.002,  0.03),   # cyano DIP half-saturation
    ("K_MIN_DOC_DOXY_20",       "log", 0.003,  0.05),   # aerobic DOC mineralization (DO)
    ("K_NITR_20",               "log", 0.2,    2.0),    # nitrification (NH4->NO3)
    ("KDISS_PART_Si_20",        "log", 0.0003, 0.005),  # biogenic Si dissolution (Si)
]
PHI_VARS = ["NH4", "NO3", "PO4", "DO", "Si"]   # direct EPA state variables scored in Phi


def x_to_value(x, transform, lo, hi):
    """Map a normalized coord x in [0,1] to a parameter value."""
    if transform == "log":
        return 10.0 ** (math.log10(lo) + x * (math.log10(hi) - math.log10(lo)))
    return lo + x * (hi - lo)


def morris_trajectories(k, r, delta, seed):
    """r elementary-effects trajectories: base point + one +/-delta step per param.

    Returns a list of trajectories; each is (points, steps) where points is a list of k+1
    coordinate vectors and steps[j] = (param_index, signed_delta) for the step points[j]->points[j+1].
    """
    rng = random.Random(seed)  # noqa: S311  (Morris trajectory sampling, not cryptographic)
    trajs = []
    for _ in range(r):
        x = [rng.uniform(0.0, 1.0) for _ in range(k)]
        order = list(range(k))
        rng.shuffle(order)
        pts = [x[:]]
        steps = []
        for i in order:
            x = x[:]
            d = delta if x[i] <= 1.0 - delta else -delta   # keep in [0,1]
            x[i] += d
            pts.append(x[:])
            steps.append((i, d))
        trajs.append((pts, steps))
    return trajs


def _write_perturbed_wconst(src, dst, values):
    """Copy WCONST_04.txt replacing the value (field 3) of each name in `values`."""
    with open(src) as f:
        lines = f.readlines()
    out = []
    for ln in lines:
        parts = ln.split()
        if len(parts) >= 3 and parts[1] in values:
            comment = ln.split("!", 1)
            tail = ("  !" + comment[1].rstrip("\n")) if len(comment) > 1 else ""
            out.append(f"{parts[0]:>6}{parts[1]:>35}   {values[parts[1]]:.6g}{tail}\n")
        else:
            out.append(ln)
    with open(dst, "w") as f:
        f.writelines(out)


def _phi_from_metrics(metrics_csv):
    """Scalar Phi = sum over PHI_VARS of (1/obs_mean) * n-weighted RMSE  (weighted like the PEST objective)."""
    agg = {}  # var -> [sum n*rmse^2, sum n, sum n*obs_mean]
    with open(metrics_csv) as f:
        for row in csv.DictReader(f):
            v = row["variable"]
            if v not in PHI_VARS:
                continue
            n = float(row["n"] or 0)
            if n <= 0:
                continue
            a = agg.setdefault(v, [0.0, 0.0, 0.0])
            a[0] += n * float(row["rmse"]) ** 2
            a[1] += n
            a[2] += n * float(row["obs_mean"])
    phi = 0.0
    for _v, (ssr, n, om) in agg.items():
        if n <= 0:
            continue
        rmse = math.sqrt(ssr / n)
        obs_mean = om / n
        w = 1.0 / obs_mean if obs_mean > 1e-9 else 0.0
        phi += w * rmse
    return phi


def forward(args):
    """Run one CL29 forward evaluation for coordinate vector x; return (idx, Phi) or (idx, None)."""
    idx, x, days, workdir = args
    try:
        wd = os.path.join(workdir, f"w{idx}")
        shutil.rmtree(wd, ignore_errors=True)
        os.makedirs(os.path.join(wd, "INPUTS_CL29", ""), exist_ok=True)
        os.makedirs(os.path.join(wd, "OUT"), exist_ok=True)
        # symlink-farm: link every INPUTS_CL29 file, except WCONST_04.txt which we perturb
        values = {name: x_to_value(x[i], tr, lo, hi) for i, (name, tr, lo, hi) in enumerate(PARAMS)}
        for fn in os.listdir(SRC_INPUTS):
            dst = os.path.join(wd, "INPUTS_CL29", fn)
            if fn == "WCONST_04.txt":
                _write_perturbed_wconst(os.path.join(SRC_INPUTS, fn), dst, values)
            else:
                os.symlink(os.path.join(SRC_INPUTS, fn), dst)
        # short driver: INPUTS_CL29/ + short relative OUT/ + `days`-day window + daily print.
        # Line-anchored (mirrors the verified `sed -e 's#^...#'`); a bare .replace() is unanchored
        # and would corrupt any other line containing the same substring (e.g. a "10.5" value).
        with open(os.path.join(REPO, "INPUT_CL29.txt")) as f:
            lines = f.readlines()
        out = []
        for ln in lines:
            if ln.startswith("         4016.0"):
                out.append(f"{float(days):15.1f}\n")
            elif ln.startswith("             10"):
                out.append("            240\n")
            elif ln.startswith("OUTPUTS_CL29/"):
                out.append("OUT/\n")
            else:
                out.append(ln)
        with open(os.path.join(wd, "INPUT.txt"), "w") as f:
            f.writelines(out)
        env = dict(os.environ, ESTAS_HOLD_VOLUME="1")
        r = subprocess.run([BIN, "INPUT.txt"], cwd=wd, env=env,
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=600)
        if r.returncode != 0 or not os.path.exists(os.path.join(wd, "OUT", "PELAGIC_BOX_00023.out")):
            return (idx, None)
        subprocess.run(["python3", VALIDATOR, "--outputs", os.path.join(wd, "OUT"),
                        "--obs", OBS, "--base-year", "2012", "--out", os.path.join(wd, "val"),
                        "--no-plots"], cwd=REPO, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                       timeout=120)
        phi = _phi_from_metrics(os.path.join(wd, "val", "validation_metrics.csv"))
        shutil.rmtree(wd, ignore_errors=True)
        return (idx, phi)
    except Exception:
        return (idx, None)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--trajectories", type=int, default=6, help="Morris trajectories r")
    ap.add_argument("--delta", type=float, default=0.4, help="normalized perturbation")
    ap.add_argument("--days", type=int, default=730, help="CL29 window (days)")
    ap.add_argument("--workers", type=int, default=min(24, (os.cpu_count() or 4) - 2))
    ap.add_argument("--seed", type=int, default=12345)
    ap.add_argument("--workdir", default="/tmp/morris_work")
    a = ap.parse_args()
    if not os.path.exists(BIN):
        raise SystemExit("ESTAS_II not built (make build-estas)")

    k = len(PARAMS)
    trajs = morris_trajectories(k, a.trajectories, a.delta, a.seed)
    # flatten all points, evaluate Phi in parallel
    flat, meta = [], []   # meta[idx] = (traj, position)
    for t, (pts, _steps) in enumerate(trajs):
        for j, p in enumerate(pts):
            meta.append((t, j))
            flat.append(p)
    os.makedirs(a.workdir, exist_ok=True)
    print(f"Morris screen: {k} params, r={a.trajectories} trajectories, delta={a.delta}, "
          f"{len(flat)} runs ({a.days}-day CL29), {a.workers} workers")
    tasks = [(i, flat[i], a.days, a.workdir) for i in range(len(flat))]
    phis = [None] * len(flat)
    done = 0
    with ProcessPoolExecutor(max_workers=a.workers) as ex:
        for idx, phi in ex.map(forward, tasks):
            phis[idx] = phi
            done += 1
            if done % 10 == 0 or done == len(flat):
                print(f"  {done}/{len(flat)} runs done", flush=True)

    # reassemble per-trajectory Phi and compute elementary effects
    by_traj = {}
    for gi, (t, j) in enumerate(meta):
        by_traj.setdefault(t, {})[j] = phis[gi]
    ee = {i: [] for i in range(k)}
    n_bad = 0
    for t, (_pts, steps) in enumerate(trajs):
        pj = by_traj[t]
        for j, (i, d) in enumerate(steps):
            a0, a1 = pj.get(j), pj.get(j + 1)
            if a0 is None or a1 is None:
                n_bad += 1
                continue
            ee[i].append((a1 - a0) / d)

    def stats(vals):
        if not vals:
            return (float("nan"), float("nan"), 0)
        mu_star = sum(abs(v) for v in vals) / len(vals)
        mean = sum(vals) / len(vals)
        sigma = math.sqrt(sum((v - mean) ** 2 for v in vals) / len(vals)) if len(vals) > 1 else 0.0
        return (mu_star, sigma, len(vals))

    ranking = sorted(((PARAMS[i][0], *stats(ee[i])) for i in range(k)), key=lambda r: -r[1])
    print(f"\n=== Morris identifiability ranking (Phi = weighted RMSE over {'/'.join(PHI_VARS)}) ===")
    print(f"{'rank':>4} {'parameter':<28} {'mu* (influence)':>16} {'sigma':>12} {'n_ee':>5}")
    for rk, (name, mu_star, sigma, n) in enumerate(ranking, 1):
        print(f"{rk:>4} {name:<28} {mu_star:>16.4g} {sigma:>12.4g} {n:>5}")
    if n_bad:
        print(f"\nNOTE: {n_bad} elementary effects dropped (a forward run failed).")


if __name__ == "__main__":
    main()
