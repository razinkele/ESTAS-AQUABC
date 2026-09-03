#!/usr/bin/env python3
"""Direct per-group C:Chl from sample-level pairing.

Joins Fitoplanktonas (species wet biomass) to ChlorofilasA (measured chlorophyll)
by sampling event (reg_nr) at the CL29 LTK stations, groups the taxa into the
model's phytoplankton groups, and reads C:Chl two ways:

  (a) directly, from samples dominated (>=X%) by a single group;
  (b) by sample-level non-negative least squares across all events.

Wet biomass -> carbon uses the ingester's own factor so the result is comparable
with the model's group carbon.
"""
import json
import sys
import numpy as np
from collections import defaultdict
from scipy.optimize import nnls

NDJSON = "/home/razinka/curonian/DATA/JTD/monitoringasjsonl"
STATIONS = {"LTK1", "LTK2", "LTK14", "LTK3B", "LTK12", "LTK5", "LTK7B", "LTK10"}
RATIOS = {"DIA": 0.065, "CYN": 0.18, "FIX": 0.16, "OPA": 0.15}  # project empirical C:wet

DIATOM = {"eupodiscales", "bacillariales"}
CYANO_NOFIX = {"oscillatoriales", "chroococcales", "cyanophyceae"}
FIXERS = {"nostocales"}

def group_of(order):
    o = order.strip().lower()
    if o in DIATOM:
        return "DIA"
    if o in FIXERS:
        return "FIX"
    if o in CYANO_NOFIX:
        return "CYN"
    return "OPA"

bio = defaultdict(lambda: defaultdict(float))   # reg_nr -> group -> wet biomass
chl = {}                                        # reg_nr -> chlorophyll ug/L
meta = {}
for ln in open(NDJSON):
    if "Fitoplanktonas" not in ln and "ChlorofilasA" not in ln:
        continue
    d = json.loads(ln)
    t = d.get("_type", "").split("/")[-1]
    if d.get("m_vietos_kodas") not in STATIONS:
        continue
    reg = d.get("reg_nr")
    if not reg:
        continue
    if t == "Fitoplanktonas":
        try:
            bm = float(d.get("biomase") or 0.0)
        except (TypeError, ValueError):
            continue
        bio[reg][group_of(str(d.get("individu_klase") or ""))] += bm
        if d.get("data"):
            meta[reg] = d["data"][:10]
    elif t == "ChlorofilasA":
        try:
            chl[reg] = float(d.get("rezultatas"))
        except (TypeError, ValueError):
            pass

common = sorted(set(bio) & set(chl))
print(f"phyto events {len(bio)}, chlorophyll events {len(chl)}, "
      f"PAIRED {len(common)}  (C:wet per group {RATIOS})\n")
if not common:
    sys.exit("no paired events")

GROUPS = ["DIA", "CYN", "OPA", "FIX"]
rows, y, frac = [], [], []
for reg in common:
    g = bio[reg]
    tot = sum(g.values())
    c = chl[reg]
    if tot <= 0 or c <= 1.0:
        continue
    carbon = [g.get(k, 0.0) * RATIOS[k] for k in GROUPS]   # mg C/L
    rows.append(carbon)
    y.append(c / 1000.0)                                   # mg Chl/L
    frac.append([v / tot for v in (g.get(k, 0.0) for k in GROUPS)])
A, b, F = np.array(rows), np.array(y), np.array(frac)
print(f"usable paired events: {len(b)}")

print("\n(a) Direct read from single-group-dominated samples:")
print(f"  {'group':>6} {'>=70%':>18} {'>=80%':>18} {'>=90%':>18}")
for gi, gname in enumerate(GROUPS):
    cells = []
    for thr in (0.70, 0.80, 0.90):
        sel = F[:, gi] >= thr
        if sel.sum() >= 3:
            # C:Chl of the dominant group, attributing all chl to total carbon
            r = A[sel].sum(axis=1) / b[sel]
            cells.append(f"{np.median(r):>7.1f} (n={sel.sum():>3})")
        else:
            cells.append(f"{'--':>7}      ")
    print(f"  {gname:>6} " + " ".join(f"{c:>17}" for c in cells))

print("\n(b) Sample-level NNLS across all paired events:")
x, _ = nnls(A, b)
for gname, xi in zip(GROUPS, x):
    print(f"  {gname:<5} C:Chl = {1/xi if xi > 1e-9 else float('inf'):>8.1f}")
pred = A @ x
r2 = 1 - np.sum((b - pred) ** 2) / np.sum((b - b.mean()) ** 2)
print(f"  R2 = {r2:.3f}  n = {len(b)}")

print("\n  bootstrap (200 resamples, 95% interval):")
est = defaultdict(list)
rng = np.random.default_rng(0)
for _ in range(200):
    idx = rng.integers(0, len(b), len(b))
    xb, _ = nnls(A[idx], b[idx])
    for gname, xi in zip(GROUPS, xb):
        est[gname].append(1 / xi if xi > 1e-9 else np.nan)
for gname in GROUPS:
    v = np.array(est[gname], dtype=float)
    v = v[np.isfinite(v)]
    if len(v) > 10:
        print(f"  {gname:<5} {np.percentile(v,50):>7.1f}  "
              f"[{np.percentile(v,2.5):>7.1f}, {np.percentile(v,97.5):>7.1f}]  "
              f"({100*len(v)/200:.0f}% finite)")

print("\n(c) Diatom-dominated samples split by season (the two-guild test):")
seas = {"winter/spring (Feb-May)": (2,3,4,5), "summer/autumn (Aug-Nov)": (8,9,10,11)}
for label, mm in seas.items():
    vals = []
    for i, reg in enumerate([r for r in common if r in meta]):
        pass
    # rebuild with month info
    vals = []
    for reg in common:
        g = bio[reg]; tot = sum(g.values()); c = chl.get(reg, 0)
        if tot <= 0 or c <= 1.0 or reg not in meta:
            continue
        if g.get("DIA", 0.0)/tot < 0.70:
            continue
        m = int(meta[reg][5:7])
        if m in mm:
            carbon = sum(g.get(k, 0.0)*RATIOS[k] for k in GROUPS)
            vals.append(carbon/(c/1000.0))
    if len(vals) >= 4:
        v = np.array(vals)
        print(f"  {label:<26} n={len(v):>3}  median C:Chl = {np.median(v):>6.1f}  "
              f"IQR {np.percentile(v,25):.1f}-{np.percentile(v,75):.1f}")
    else:
        print(f"  {label:<26} n={len(vals):>3}  (too few)")
