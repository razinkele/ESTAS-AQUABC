#!/usr/bin/env python3
"""Convert the full EUTROPY 29-box Curonian Lagoon network into AQUABC inputs.

Extends the single-box PoC to the whole lagoon: emits per-box initial
conditions, per-box daily forcing, box volumes, the box-to-box advective link
topology + daily fluxes, and time-mean boundary concentrations, for the
29-box AQUABC network driver (aquabc_II_pelagic_network.f90).

EUTROPY transport (replicated in the driver): per box, per step,
  C += ( sum_inflow  Q/V * C_source  -  sum_outflow Q/V * C_box ) * dt
where flux columns are named From_<i>_To_<j> (negative i = boundary 1..5) and
flux values are m^3/s (converted here to m^3/day).

Run from the repository root.
"""

from __future__ import annotations

import csv
import datetime as _dt
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from eutropy_to_aquabc import (  # shared mapping/defaults  # noqa: E402
    AQUABC_DEFAULTS, AQUABC_STATE_NAMES, PHYTO_TARGETS, STATE_MAP,
)

EU = os.path.expanduser("~/eutropy/input")
OUT = os.path.join(os.path.dirname(__file__), "net")
BASE_DATE = _dt.date(2012, 1, 1)
MAX_DAY = 1826            # 2012-2017 (EUTROPY calibrated period)
NBOX = 29
BOUNDARIES = 5

# EUTROPY pelagic state-var column order in the CSV inputs.
EU_VARS = ["Cpy", "Cpoc", "Cpon", "Cpop", "Cdoc", "Cdon", "Cdop",
           "Cam", "Cni", "Cph", "Cox"]


def _to_day(v: str) -> int | None:
    v = v.strip()
    try:
        return int(float(v))
    except ValueError:
        pass
    for fmt in ("%m/%d/%Y", "%Y-%m-%d"):
        try:
            return (_dt.datetime.strptime(v, fmt).date() - BASE_DATE).days
        except ValueError:
            continue
    return None


def daily_matrix(path: str, ncols: int):
    """Stream a `time, c1..cNcols` CSV -> list of (day, [mean_1..mean_ncols])."""
    acc: dict[int, list] = {}
    with open(path, newline="") as fh:
        r = csv.reader(fh)
        next(r)  # header
        for row in r:
            if len(row) < ncols + 1:
                continue
            day = _to_day(row[0])
            if day is None or day > MAX_DAY:
                continue
            s, n = acc.get(day, ([0.0] * ncols, 0))
            for k in range(ncols):
                try:
                    s[k] += float(row[k + 1])
                except ValueError:
                    pass
            acc[day] = (s, n + 1)
    return [(d, [x / acc[d][1] for x in acc[d][0]]) for d in sorted(acc)]


def state_vector(eu_row: dict, which: int) -> list[float]:
    vec = [0.0] * 32
    for idx, (ic, bc) in AQUABC_DEFAULTS.items():
        vec[idx - 1] = ic if which == 0 else bc
    for name, idx in STATE_MAP.items():
        if name in eu_row:
            vec[idx - 1] = float(eu_row[name])
    return vec


def write_matrix_csv(path, header, rows):
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(header)
        w.writerows(rows)


def main() -> int:
    os.makedirs(OUT, exist_ok=True)
    STATE_MAP["Cpy"] = PHYTO_TARGETS["cyn"]   # cyanobacteria (best single-box fit)
    print(f"[network] Cpy -> {AQUABC_STATE_NAMES[STATE_MAP['Cpy'] - 1]}, "
          f"{NBOX} boxes, days 0..{MAX_DAY}")

    # --- initial conditions: 29 boxes x 32 vars ---
    ic_rows = []
    with open(os.path.join(EU, "initial_concentrations.csv")) as fh:
        for row in csv.DictReader(fh):
            b = int(row["boxes"])
            ic_rows.append([b] + [f"{v:.6f}" for v in state_vector(row, 0)])
    ic_rows.sort(key=lambda r: r[0])
    write_matrix_csv(os.path.join(OUT, "initial_conditions.csv"),
                     ["box"] + AQUABC_STATE_NAMES, ic_rows)

    # --- depths ---
    depths = {}
    with open(os.path.join(EU, "depth.csv")) as fh:
        for row in csv.DictReader(fh):
            depths[int(row["box"])] = float(row["depth"])
    write_matrix_csv(os.path.join(OUT, "depths.csv"), ["box", "depth_m"],
                     [[b, f"{depths[b]:.4f}"] for b in sorted(depths)])

    # --- per-box daily forcing + volumes ---
    forcings = {
        "temp": ("temp_2012-2022.csv", "forcing_temp.csv"),
        "salt": ("salt_2012-2022.csv", "forcing_salt.csv"),
        "light": ("srad_2012-2022.csv", "forcing_light.csv"),
        "fday": ("Fraction_daylight_2012-2023.csv", "forcing_fday.csv"),
        "vol": ("volume_2012-2022.csv", "volumes.csv"),
    }
    ndays = None
    for _, (src, dst) in forcings.items():
        mat = daily_matrix(os.path.join(EU, src), NBOX)
        ndays = len(mat)
        rows = [[d] + [f"{v:.5f}" for v in vals] for d, vals in mat]
        write_matrix_csv(os.path.join(OUT, dst),
                         ["day"] + [f"box{i}" for i in range(1, NBOX + 1)], rows)
    print(f"[network] forcing + volumes: {ndays} daily rows x {NBOX} boxes")

    # --- advective links + daily fluxes (m^3/day) ---
    with open(os.path.join(EU, "flux_2012-2022.csv")) as fh:
        cols = next(csv.reader(fh))[1:]          # drop 'time'
    links = []
    for c in cols:                                # From_<i>_To_<j>
        parts = c.split("_")
        links.append((int(parts[1]), int(parts[3])))
    nlink = len(links)
    write_matrix_csv(os.path.join(OUT, "links.csv"),
                     ["link", "from_box", "to_box"],
                     [[i + 1, f, t] for i, (f, t) in enumerate(links)])
    fmat = daily_matrix(os.path.join(EU, "flux_2012-2022.csv"), nlink)
    frows = [[d] + [f"{v * 86400.0:.4f}" for v in vals] for d, vals in fmat]
    write_matrix_csv(os.path.join(OUT, "flux.csv"),
                     ["day"] + [f"L{i + 1}" for i in range(nlink)], frows)
    print(f"[network] {nlink} advective links "
          f"({sum(1 for f, _ in links if f < 0)} boundary)")

    # --- time-mean boundary concentrations: 5 x 32 ---
    bfiles = {1: "bc_concentration_BS_average", 2: "bc_concentration_Nemunas",
              3: "bc_concentration_Minija", 4: "bc_concentration_Deima",
              5: "bc_concentration_Madrosovka"}
    brows = []
    for bnd in range(1, BOUNDARIES + 1):
        sums, n = {v: 0.0 for v in EU_VARS}, 0
        with open(os.path.join(EU, bfiles[bnd] + ".csv")) as fh:
            for row in csv.DictReader(fh):
                for v in EU_VARS:
                    if row.get(v):
                        sums[v] += float(row[v])
                n += 1
        mean = {v: sums[v] / n for v in EU_VARS}
        brows.append([bnd] + [f"{v:.6f}" for v in state_vector(mean, 1)])
    write_matrix_csv(os.path.join(OUT, "boundary_mean.csv"),
                     ["boundary"] + AQUABC_STATE_NAMES, brows)

    print(f"[network] wrote 9 files to {OUT}/")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
