#!/usr/bin/env python3
"""Convert EUTROPY single-box data into an AQUABC 0-D (single-box) input set.

Proof-of-concept bridge between two box models of the Curonian Lagoon:
  * EUTROPY  (~/eutropy)  — Python/Numba, 29 boxes, 11 pelagic state vars
  * AQUABC   (this repo)  — Fortran/ESTAS, 32 pelagic state vars

It extracts one EUTROPY box (default 19, the PEST-calibrated box), maps its
11 pelagic state variables onto AQUABC's 32, seeds the 21 AQUABC-only variables
from the Curonian-tuned 0-D example, and writes:

    <out>/initial_conditions.txt      32 AQUABC initial concentrations
    <out>/boundary_concentrations.txt 32 AQUABC boundary concentrations
    <out>/forcing_box<N>.csv          daily forcing time series (temp/salt/light/...)
    <out>/MAPPING.md                  provenance, unit notes, defaults, next steps

Constants are NOT converted: the formulations differ (EUTROPY simple Monod vs
AQUABC CTMI/SU/Platt), so reuse the existing Curonian constant file
`SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/data/const_CL.txt`.
EUTROPY's calibrated values are good priors for a later AQUABC calibration.

Usage:
    python3 tools/eutropy_to_aquabc.py --eutropy-root ~/eutropy --box 19
"""

from __future__ import annotations

import argparse
import csv
import datetime as _dt
import os
import sys

# ---------------------------------------------------------------------------
# SCIENTIFIC DECISION 1 — state-variable mapping (EUTROPY name -> AQUABC index).
# Cpy (EUTROPY's single lumped phytoplankton carbon) is mapped to AQUABC's
# diatom group DIA_C. Both are carbon (mg C/L), so no unit conversion.
# Edit this table to route phytoplankton to a different AQUABC group.
# ---------------------------------------------------------------------------
# EUTROPY's single lumped phytoplankton (Cpy) can be routed to any AQUABC
# phytoplankton group via --phyto-target. The Curonian Lagoon's summer blooms
# are cyanobacteria-dominated, so `cyn` is often a better fit than `dia`.
PHYTO_TARGETS = {"dia": 5, "cyn": 15, "opa": 16}

STATE_MAP = {
    "Cam":  1,   # ammonium N        -> NH4_N
    "Cni":  2,   # nitrate N         -> NO3_N
    "Cph":  3,   # phosphate P       -> PO4_P
    "Cox":  4,   # dissolved oxygen  -> DISS_OXYGEN
    "Cpy":  5,   # phytoplankton C   -> DIA_C (overridden by --phyto-target)
    "Cpoc": 9,   # part. org. C      -> DET_PART_ORG_C
    "Cpon": 10,  # part. org. N      -> DET_PART_ORG_N
    "Cpop": 11,  # part. org. P      -> DET_PART_ORG_P
    "Cdoc": 12,  # diss. org. C      -> DISS_ORG_C
    "Cdon": 13,  # diss. org. N      -> DISS_ORG_N
    "Cdop": 14,  # diss. org. P      -> DISS_ORG_P
}

# AQUABC pelagic state variable names, index 1..32 (from PELAGIC_INPUTS.txt).
AQUABC_STATE_NAMES = [
    "NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C", "ZOO_C", "ZOO_N",
    "ZOO_P", "DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P",
    "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P", "CYN_C", "OPA_C", "DISS_Si",
    "PART_Si", "FIX_CYN_C", "INORG_C", "TOT_ALK", "FE_II", "FE_III",
    "MN_II", "MN_IV", "CA", "MG", "S_PLUS_6", "S_MINUS_2", "CH4_C",
    "NOST_VEG_HET_C", "AKI_C",
]

# ---------------------------------------------------------------------------
# SCIENTIFIC DECISION 2 — seed values for the 21 AQUABC-only state variables
# that EUTROPY does not carry (other phyto groups, zooplankton, silica,
# carbonate system, Fe/Mn/S redox). Defaults are the Curonian-representative
# values baked into the 0-D example (aquabc_II_pelagic_0D.f90). Adjust to
# disable a process (set to 0) or to use your own Curonian values.
# Keyed by AQUABC index -> (initial_condition, boundary_concentration).
# ---------------------------------------------------------------------------
AQUABC_DEFAULTS = {
    6:  (0.020, 0.0400),   # ZOO_C
    7:  (0.000, 0.0100),   # ZOO_N
    8:  (0.000, 0.0020),   # ZOO_P
    15: (0.060, 0.0100),   # CYN_C
    16: (0.022, 0.0200),   # OPA_C
    17: (3.000, 3.0000),   # DISS_Si
    18: (1.500, 1.5000),   # PART_Si
    19: (0.000, 0.0001),   # FIX_CYN_C
    20: (0.0027, 0.0027),  # INORG_C
    21: (0.0027, 0.0027),  # TOT_ALK
    22: (0.230, 0.2300),   # FE_II
    23: (0.550, 0.0500),   # FE_III
    24: (0.100, 0.1000),   # MN_II
    25: (0.100, 0.1000),   # MN_IV
    26: (70.00, 70.0000),  # CA
    27: (15.00, 15.0000),  # MG
    28: (1.350, 1.3500),   # S_PLUS_6
    29: (0.000, 0.0000),   # S_MINUS_2
    30: (0.000, 0.0000),   # CH4_C
    31: (0.000, 0.0000),   # NOST_VEG_HET_C
    32: (8.000, 0.0001),   # AKI_C
}

# Fixed values for the driving functions AQUABC needs but EUTROPY lacks
# (index into AQUABC's 10 driving functions). Documented in MAPPING.md.
AIR_TEMP_FALLBACK = None       # None -> use water temperature as a proxy
WIND_SPEED_MS = 4.0            # m/s, lagoon-representative constant
ELEVATION_M = 0.0             # m, water-level anomaly (0 = mean)
BG_LIGHT_EXTINCTION = 0.6     # 1/m, background (non-chl) attenuation
ICE_COVER = 0.0              # 0-1


def _read_box_row(path: str, box: int) -> dict[str, float]:
    """Read a per-box CSV whose first column is the box id; return the box row."""
    with open(path, newline="") as fh:
        for row in csv.DictReader(fh):
            first = next(iter(row))
            if str(row[first]).strip() == str(box):
                return {k: v for k, v in row.items() if k != first}
    raise ValueError(f"box {box} not found in {path}")


def _to_day(value: str, base_date: _dt.date) -> int | None:
    """Convert a EUTROPY time cell to an integer day index since base_date.

    Handles both formats seen in the inputs: numeric day fractions
    (`0`, `0.0416...`, used by temp/salt/srad, hourly) and calendar dates
    (`M/D/YYYY`, used by the daylight file, daily).
    """
    value = value.strip()
    try:
        return int(float(value))
    except (TypeError, ValueError):
        pass
    for fmt in ("%m/%d/%Y", "%Y-%m-%d", "%d/%m/%Y"):
        try:
            return (_dt.datetime.strptime(value, fmt).date() - base_date).days
        except ValueError:
            continue
    return None


def _daily_mean_column(path: str, box: int,
                       base_date: _dt.date) -> dict[int, float]:
    """Resample a `time, 1..29` forcing CSV to daily means for one box column.

    `time` may be numeric days (hourly rows) or calendar dates. Returns
    {day_index: mean}.
    """
    col = str(box)
    acc: dict[int, list[float]] = {}
    with open(path, newline="") as fh:
        reader = csv.DictReader(fh)
        if col not in reader.fieldnames:
            raise ValueError(f"box column {col!r} not in {path}")
        for row in reader:
            day = _to_day(row["time"], base_date)
            if day is None:
                continue
            try:
                val = float(row[col])
            except (TypeError, ValueError):
                continue
            acc.setdefault(day, []).append(val)
    return {d: sum(v) / len(v) for d, v in acc.items()}


def _boundary_means(path: str) -> dict[str, float]:
    """Time-average each EUTROPY boundary concentration column."""
    sums: dict[str, float] = {}
    counts: dict[str, int] = {}
    with open(path, newline="") as fh:
        for row in csv.DictReader(fh):
            for key, raw in row.items():
                if key in ("time", "") or key is None:
                    continue
                try:
                    val = float(raw)
                except (TypeError, ValueError):
                    continue
                sums[key] = sums.get(key, 0.0) + val
                counts[key] = counts.get(key, 0) + 1
    return {k: sums[k] / counts[k] for k in sums}


def build_state_vector(eutropy_values: dict[str, float], which: int) -> list[float]:
    """Assemble a 32-length AQUABC state vector (which: 0=IC, 1=BC)."""
    vec = [0.0] * 32
    for idx, (ic, bc) in AQUABC_DEFAULTS.items():
        vec[idx - 1] = ic if which == 0 else bc
    for name, idx in STATE_MAP.items():
        if name in eutropy_values:
            vec[idx - 1] = float(eutropy_values[name])
    return vec


def write_state_file(path: str, vec: list[float], title: str) -> None:
    with open(path, "w") as fh:
        fh.write(f"# {title}\n")
        fh.write("#     PELAGIC STATE VAR. NO       PELAGIC CONCENTRATION\n")
        for i, val in enumerate(vec, start=1):
            fh.write(f"{i:27d}{val:20.6f}     ! {AQUABC_STATE_NAMES[i - 1]}\n")


def write_forcing(path: str, box: int, forcing: dict, depth: float,
                  base_date: _dt.date, max_day: int | None) -> int:
    days = sorted(set(forcing["temp"]) & set(forcing["salt"])
                  & set(forcing["srad"]) & set(forcing["fday"]))
    if max_day is not None:
        days = [d for d in days if d <= max_day]
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["day", "date", "temperature_C", "salinity_psu",
                    "light_Wm2", "fraction_of_day", "air_temp_C",
                    "wind_speed_ms", "elevation_m", "depth_m",
                    "bg_light_ext_1m", "ice_cover"])
        for d in days:
            temp = forcing["temp"][d]
            air = temp if AIR_TEMP_FALLBACK is None else AIR_TEMP_FALLBACK
            w.writerow([
                d, (base_date + _dt.timedelta(days=d)).isoformat(),
                f"{temp:.4f}", f"{forcing['salt'][d]:.4f}",
                f"{forcing['srad'][d]:.4f}", f"{forcing['fday'][d]:.4f}",
                f"{air:.4f}", f"{WIND_SPEED_MS:.4f}", f"{ELEVATION_M:.4f}",
                f"{depth:.4f}", f"{BG_LIGHT_EXTINCTION:.4f}", f"{ICE_COVER:.4f}",
            ])
    return len(days)


def write_mapping_doc(path: str, box: int, boundary: str, ic_vec, bc_vec,
                      n_days: int, base_date: _dt.date) -> None:
    mapped = {v: k for k, v in STATE_MAP.items()}
    lines = [
        f"# EUTROPY box {box} -> AQUABC 0-D input set",
        "",
        f"Generated by `tools/eutropy_to_aquabc.py` from `~/eutropy` box {box} "
        f"(boundary: {boundary}).",
        "",
        "## State-variable mapping",
        "",
        "| AQUABC # | AQUABC var | source |",
        "|---|---|---|",
    ]
    for i, name in enumerate(AQUABC_STATE_NAMES, start=1):
        src = f"EUTROPY `{mapped[i]}`" if i in mapped else "0-D default (seed)"
        lines.append(f"| {i} | {name} | {src} |")
    lines += [
        "",
        "## Units",
        "- Concentrations: mg/L (g/m^3); AKI_C is g/m^2.",
        "- `Cpy`->`DIA_C` is carbon->carbon (no conversion). Chlorophyll "
        "comparison uses AQUABC's `DIA_C_TO_CHLA` constant.",
        "",
        "## Provenance / caveats",
        f"- Forcing: {n_days} daily means from EUTROPY box {box} "
        f"(base date {base_date.isoformat()}); temperature/salinity/light/"
        "daylight are real, air-temp is proxied by water temp, wind/elevation/"
        "bg-extinction/ice are constants (see script header).",
        f"- Boundary: time-averaged `{boundary}` concentrations as a fixed BC. "
        "A faithful box-19 BC would be the flux-weighted mix of its neighbours.",
        "- The 21 AQUABC-only variables are seeded from the Curonian 0-D "
        "defaults, NOT from EUTROPY. Review `AQUABC_DEFAULTS` in the script.",
        "- Constants are NOT converted; reuse `const_CL.txt`.",
        "",
        "## Next step",
        "The stock `aquabc_II_pelagic_0D.f90` hardcodes IC/BC/forcing. To run "
        "this PoC it needs a ~30-line patch to read `initial_conditions.txt`, "
        "`boundary_concentrations.txt`, and step through `forcing_box"
        f"{box}.csv`. Then compare `OUTPUT.csv` DIA_C (as chlorophyll) against "
        "EUTROPY output and the box-19 observations.",
    ]
    with open(path, "w") as fh:
        fh.write("\n".join(lines) + "\n")


def main(argv=None) -> int:
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--eutropy-root", default=os.path.expanduser("~/eutropy"))
    p.add_argument("--box", type=int, default=19)
    p.add_argument("--boundary", default="Nemunas",
                   help="EUTROPY boundary file suffix used as the fixed BC")
    p.add_argument("--phyto-target", choices=sorted(PHYTO_TARGETS),
                   default="dia",
                   help="AQUABC phytoplankton group Cpy maps to (default dia)")
    p.add_argument("--out", default=None,
                   help="output dir (default tools/eutropy_poc/box<N>)")
    p.add_argument("--base-date", default="2012-01-01",
                   help="calendar date of EUTROPY forcing day 0")
    p.add_argument("--max-day", type=int, default=1826,
                   help="last forcing day to emit (default 1826 = 2012-2017)")
    args = p.parse_args(argv)

    root = os.path.expanduser(args.eutropy_root)
    inp = os.path.join(root, "input")
    if not os.path.isdir(inp):
        p.error(f"no input/ dir under {root}")
    out = args.out or os.path.join(os.path.dirname(__file__), "eutropy_poc",
                                   f"box{args.box}")
    os.makedirs(out, exist_ok=True)
    base_date = _dt.date.fromisoformat(args.base_date)

    STATE_MAP["Cpy"] = PHYTO_TARGETS[args.phyto_target]
    print(f"[eutropy->aquabc] box {args.box}, boundary {args.boundary}, "
          f"Cpy -> {AQUABC_STATE_NAMES[STATE_MAP['Cpy'] - 1]}")

    # Initial conditions
    ic_src = _read_box_row(os.path.join(inp, "initial_concentrations.csv"),
                           args.box)
    ic_vec = build_state_vector(ic_src, which=0)

    # Boundary concentrations (time-averaged)
    bc_path = os.path.join(inp, f"bc_concentration_{args.boundary}.csv")
    bc_src = _boundary_means(bc_path)
    bc_vec = build_state_vector(bc_src, which=1)

    # Depth for the box
    depths = _read_box_row(os.path.join(inp, "depth.csv"), args.box)
    depth = float(depths.get("depth", 0.0))

    # Forcing (daily means)
    print("[eutropy->aquabc] resampling forcing to daily means ...")
    forcing = {
        "temp": _daily_mean_column(os.path.join(inp, "temp_2012-2022.csv"),
                                   args.box, base_date),
        "salt": _daily_mean_column(os.path.join(inp, "salt_2012-2022.csv"),
                                   args.box, base_date),
        "srad": _daily_mean_column(os.path.join(inp, "srad_2012-2022.csv"),
                                   args.box, base_date),
        "fday": _daily_mean_column(
            os.path.join(inp, "Fraction_daylight_2012-2023.csv"),
            args.box, base_date),
    }

    # Emit
    write_state_file(os.path.join(out, "initial_conditions.txt"), ic_vec,
                     f"AQUABC initial conditions from EUTROPY box {args.box}")
    write_state_file(os.path.join(out, "boundary_concentrations.txt"), bc_vec,
                     f"AQUABC boundary conc. from EUTROPY {args.boundary} (mean)")
    n_days = write_forcing(os.path.join(out, f"forcing_box{args.box}.csv"),
                           args.box, forcing, depth, base_date, args.max_day)
    write_mapping_doc(os.path.join(out, "MAPPING.md"), args.box, args.boundary,
                      ic_vec, bc_vec, n_days, base_date)

    print(f"[eutropy->aquabc] box {args.box} depth {depth:.2f} m, "
          f"{n_days} forcing days")
    print(f"[eutropy->aquabc] wrote 4 files to {out}/")
    return 0


if __name__ == "__main__":
    sys.exit(main())
