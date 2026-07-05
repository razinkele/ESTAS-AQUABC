#!/usr/bin/env python3
"""Generate a 29-box ESTAS production INPUTS set for the Curonian Lagoon.

Milestone target: produce a self-consistent INPUTS_CL29/ + INPUT_CL29.txt that
ESTAS_II can parse/validate. Box-independent content (36 state-var rows, 318
constants, model-options and output-info references) is lifted from the existing
25-box INPUTS/PELAGIC_INPUTS.txt template; box-dependent content (29-box basin
table, bathymetries, 1044-row settling block, 100 advective links, 5x36 open
boundaries, per-box forcing) is generated from the already-daily-resampled
EUTROPY data in tools/eutropy_poc/net/.

Time base: day index 0..1826 (2012-01-01 .. 2016-12-31), used consistently in
the TS files and INPUT_CL29.txt so ESTAS interpolation lines up.

Run from the repository root.
"""

from __future__ import annotations

import csv
import os
import shutil

REPO = os.getcwd()
TMPL = os.path.join(REPO, "INPUTS", "PELAGIC_INPUTS.txt")
NET = os.path.join(REPO, "tools", "eutropy_poc", "net")
OUT = os.path.join(REPO, "INPUTS_CL29")
NBOX = 29
NSTATE = 36                 # ESTAS pelagic state vars (32 core + 4 allelopathy)
NBND = 5
BND_TO_BOX = {1: 12, 2: 24, 3: 24, 4: 3, 5: 3}   # from Eutropy From_-N_To_j


def read_csv_matrix(path):
    with open(path) as fh:
        r = csv.reader(fh)
        header = next(r)
        rows = [row for row in r if row]
    return header, rows


def net_day_cols(fname, ncol):
    """Read a net/ 'day, c1..cN' file -> (days, [[c1..cN] per day])."""
    _, rows = read_csv_matrix(os.path.join(NET, fname))
    days = [int(float(r[0])) for r in rows]
    data = [[float(x) for x in r[1:1 + ncol]] for r in rows]
    return days, data


def write_ts(path, comment, days, cols):
    """Write an ESTAS time-series file: header + time + nvar columns per row."""
    nvar = len(cols[0]) if cols else 0
    with open(path, "w") as fh:
        fh.write(f"# {comment}\n# DATA_SIZE\n{len(days)}\n")
        fh.write(f"# NUMBER_OF_VARIABLES\n{nvar}\n")
        ones = "".join(f"{1.0:20.8f}" for _ in range(nvar)) + "\n"
        fh.write("# SCALE FACTORS\n#\n" + ones)
        fh.write("# UNIT CONVERSION FACTORS\n#\n" + ones)
        fh.write("# INTERPOLATE (1=yes)\n1\n")
        fh.write("# TIME AND VALUES\n")
        for d, row in zip(days, cols):
            fh.write(f"{float(d):.6f} " + " ".join(f"{v:.6f}" for v in row) + "\n")


# ---------------------------------------------------------------------------
# Template slicing: reuse the state-var and constants blocks verbatim.
# ---------------------------------------------------------------------------
def template_blocks():
    with open(TMPL) as fh:
        lines = fh.readlines()
    idx = {}
    for i, ln in enumerate(lines):
        if "***" not in ln:                          # only section headers
            continue
        if "PELAGIC STATE VARIABLES" in ln:
            idx["sv"] = i
        elif "PELAGIC MODEL CONSTANTS" in ln:
            idx["const"] = i
    return lines[idx["sv"]:idx["const"]]              # state-var block (36 rows)


def synth_bathymetry(box, area, depth):
    """Simple hypsographic profile: constant surface area down to the bottom,
    0.5 m layers from -ceil(depth) up to +2 m (matching the template style)."""
    import math
    bottom = -(math.ceil(depth * 2) / 2.0) - 0.5
    top = 2.0
    elevs = []
    e = bottom
    while e < top - 1e-9:
        elevs.append((e, e + 0.5))
        e += 0.5
    lines = [f"BATHYMETRY {box}\n", "NUM_LAYES\n", f"{len(elevs):10d}\n"]
    lines.append("  LAYER_NO     UPPER_ELEVATION     LOWER_ELEVATION"
                 "          UPPER_AREA          LOWER_AREA"
                 "        UPPER_LENGTH        LOWER_LENGTH\n")
    length = 8000.0
    for i, (lo, hi) in enumerate(elevs, start=1):
        ua = area
        la = area if lo > bottom + 1e-9 else area * 0.3
        lines.append(f"{i:10d}{hi:20.4f}{lo:20.4f}{ua:20.2f}{la:20.2f}"
                     f"{length:20.2f}{length:20.2f}\n")
    return "".join(lines)


def main():
    if os.path.isdir(OUT):
        shutil.rmtree(OUT)
    os.makedirs(OUT)
    state_block = template_blocks()

    # ---- source data from net/ ----
    _, ic_rows = read_csv_matrix(os.path.join(NET, "initial_conditions.csv"))
    ic = {int(r[0]): [float(x) for x in r[1:1 + 32]] for r in ic_rows}
    _, depth_rows = read_csv_matrix(os.path.join(NET, "depths.csv"))
    depth = {int(r[0]): float(r[1]) for r in depth_rows}
    _, link_rows = read_csv_matrix(os.path.join(NET, "links.csv"))
    links = [(int(r[1]), int(r[2])) for r in link_rows]   # (from, to)
    area = _load_area()

    tdays, temp = net_day_cols("forcing_temp.csv", NBOX)
    _, salt = net_day_cols("forcing_salt.csv", NBOX)
    _, light = net_day_cols("forcing_light.csv", NBOX)
    fdays, flux = net_day_cols("flux.csv", len(links))
    bdays, bnd = net_day_cols("boundary_daily.csv", NBND * 32)

    # ---- data files ----
    for b in range(1, NBOX + 1):
        with open(os.path.join(OUT, f"BATHYMETRY_{b}.txt"), "w") as fh:
            fh.write(synth_bathymetry(b, area[b], depth[b]))

    # ESTAS multiplies FLOWS by SECONDS_PER_DAY internally, so it expects m3/s;
    # net/flux.csv is m3/day (Eutropy m3/s x 86400), so convert back to m3/s.
    flux_si = [[v / 86400.0 for v in row] for row in flux]
    write_ts(os.path.join(OUT, "FLOW_TS.txt"), "FLOWS m3/s", fdays, flux_si)
    write_ts(os.path.join(OUT, "TEMP_TS.txt"), "WATER TEMPERATURE C", tdays, temp)
    write_ts(os.path.join(OUT, "SALT_TS.txt"), "SALINITY psu", tdays, salt)
    write_ts(os.path.join(OUT, "SOLAR_RAD_TS.txt"), "SOLAR RADIATION W/m2", tdays, light)
    _, fday = net_day_cols("forcing_fday.csv", NBOX)
    write_ts(os.path.join(OUT, "FORC_TS_9.txt"), "FRACTION OF DAY 0-1", tdays, fday)

    # per-boundary forcing TS: 32 mapped vars + 4 allelopathy zeros = 36
    for bi in range(1, NBND + 1):
        cols = []
        for di in range(len(bdays)):
            vec32 = list(bnd[di][(bi - 1) * 32:bi * 32])
            vec32[19] = 3.0   # INORG_C: realistic Curonian DIC (0.0027 breaks CO2SYS)
            vec32[20] = 3.1   # TOT_ALK: realistic Curonian alkalinity
            cols.append(vec32 + [0.0, 0.0, 0.0, 0.0])
        write_ts(os.path.join(OUT, f"FORC_TS_{bi}.txt"),
                 f"boundary {bi} concentrations", bdays, cols)

    # constant meteorology EUTROPY lacks (single-var TS, held constant)
    for name, val in (("AIR_TEMP_TS", 10.0), ("WIND_SPEED_TS", 4.0),
                      ("RAINFALL_TS", 0.0), ("EVAPORATION_TS", 0.0),
                      ("ICE_COVER", 0.0)):
        write_ts(os.path.join(OUT, f"{name}.txt"), name, [tdays[0], tdays[-1]],
                 [[val], [val]])

    # initial conditions: 2 sets (reuse box averages; ESTAS assigns per box)
    _write_init_conc(OUT, ic)

    # model options: box-independent, copy from template
    shutil.copy(os.path.join(REPO, "INPUTS", "PELAGIC_MODEL_OPTIONS.txt"),
                os.path.join(OUT, "PELAGIC_MODEL_OPTIONS.txt"))
    # output info: one row PER BOX (state-var / process-rate / mass-balance flags)
    with open(os.path.join(OUT, "PELAGIC_OUTPUT_INFORMATION_FILE.txt"), "w") as fh:
        fh.write("#  BOX_NO   STATE_VAR_OUT   PROCESS_RATE_OUT   MASS_BALANCE_OUT\n")
        for b in range(1, NBOX + 1):
            fh.write(f"{b:12d}{1:12d}{0:12d}{0:12d}\n")

    # constants files (box-independent): copy main + extra
    for f in ("WCONST_04.txt", "EXTRA_WCONST.txt"):
        shutil.copy(os.path.join(REPO, "INPUTS", f), os.path.join(OUT, f))

    # ---- master PELAGIC_INPUTS.txt ----
    _write_master(OUT, state_block, links, depth, area)
    _write_input_txt(REPO, tdays)

    print(f"[estas] wrote 29-box INPUTS to {OUT}/ "
          f"({NBOX} bathymetries, {len(links)} links, {NBND} boundaries)")
    return 0


def _load_area():
    with open(os.path.expanduser("~/eutropy/input/depth.csv")) as fh:
        return {int(r["box"]): float(r["area"]) for r in csv.DictReader(fh)}


def _write_init_conc(out, ic):
    names = _state_names()
    # Two IC sets: set 1 = box 19 (interior), set 2 = box 24 (river) as exemplars
    for setno, box in ((1, 19), (2, 24)):
        with open(os.path.join(out, f"INIT_CONC_{setno}.txt"), "w") as fh:
            fh.write(f"# PELAGIC INITIAL CONDITION SET {setno} (EUTROPY box {box})\n")
            fh.write("#     PELAGIC STATE VAR. NO       PELAGIC CONCENTRATION\n")
            vec = ic.get(box, [0.0] * 32) + [0.0, 0.0, 0.0, 0.0]
            vec[19], vec[20] = 3.0, 3.1        # realistic INORG_C / TOT_ALK
            for i in range(NSTATE):
                fh.write(f"{i + 1:27d}{vec[i]:20.6f}     ! {names[i]}\n")


def _state_names():
    return ["NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C", "ZOO_C", "ZOO_N",
            "ZOO_P", "DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P",
            "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P", "CYN_C", "OPA_C",
            "DISS_Si", "PART_Si", "FIX_CYN_C", "INORG_C", "TOT_ALK", "FE_II",
            "FE_III", "MN_II", "MN_IV", "CA", "MG", "S_PLUS_6", "S_MINUS_2",
            "CH4_C", "NOST_VEG_HET_C", "AKI_C", "SEC_METAB_DIA",
            "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST"]


def _hdr(name, val):
    return f"# {name}\n{val:>19}\n"


def _write_master(out, state_block, links, depth, area):
    L = []
    L.append("# DESCRIPTION Curonian Lagoon 29-box (EUTROPY-derived)\n")
    L += ["# DESRIPTION LINE %d\n" % i for i in range(2, 6)]
    L.append(_hdr("NUM_PELAGIC_STATE_VARS", NSTATE))
    L.append(_hdr("NUM_MODEL_CONSTANTS", 318))
    L.append(_hdr("NUM_PELAGIC_BASINS", NBOX))
    L.append(_hdr("NUM_BATHYMETRIES", NBOX))
    L.append(_hdr("NUM_PELAGIC_BOXES", NBOX))
    L.append(_hdr("NUM_PELAGIC_INIT_CONC_SETS", 2))
    L.append(_hdr("NUM_PELAGIC_ADVECTIVE_LINKS", len(links)))
    L.append(_hdr("NUM_PELAGIC_DISPERSIVE_LINKS", 0))
    L.append(_hdr("NUM_FLOW_TS", 1))
    L.append(_hdr("NUM_MIXING_TS", 0))
    L.append(_hdr("NUM_SETTLING_VELOCITIES", 6))
    L.append(_hdr("NUM_OPEN_BOUNDARIES", NBND))
    L.append(_hdr("NUM_MASS_LOADS", 0))
    L.append(_hdr("NUM_MASS_WITHDRAWALS", 0))
    L.append(_hdr("NUM_FORCING_TS", 14))
    L.append("# PELAGIC_MODEL_OPTIONS\nPELAGIC_MODEL_OPTIONS.txt\n")
    L.append("# PELAGIC OUTPUT INFORMATION FILE\nPELAGIC_OUTPUT_INFORMATION_FILE.txt\n")
    L.append("# PROCESS RATE OUTPUT TYPE, 1 Volume based 2 Area based\n1\n")
    L += state_block

    # PELAGIC MODEL CONSTANTS: per-box constants file
    L.append("# ********************* PELAGIC MODEL CONSTANTS *********************\n")
    L.append("#     PELAGIC BOX NO       PELAGIC MODEL CONSTANTS FILE NAME\n")
    for b in range(1, NBOX + 1):
        L.append(f"{b:20d}{'WCONST_04.txt':>40}\n")
    L.append("# EXTRA MODEL CONSTANTS FILE NAME\nEXTRA_WCONST.txt\n")

    # BOX INFORMATION
    L.append("# ********************* BOX INFORMATION *********************\n")
    L.append("#     PELAGIC BOX NO       NUM MASS LOADS INTO BOX       NUM MASS WITH. FROM BOX\n")
    for b in range(1, NBOX + 1):
        L.append(f"{b:20d}{0:30d}{0:30d}\n")

    # BASIN INFORMATION: one basin per box (reader skips a comment before each)
    L.append("# ********************* BASIN INFORMATION *********************\n")
    for b in range(1, NBOX + 1):
        L.append("#   PELAGIC BASIN NO           NUM_BOXES       BATHYMERTY NO\n")
        L.append(f"{b:20d}{1:20d}{b:20d}\n")
        L.append("# PELAGIC BOXES\n")
        L.append(f"{b:15d}\n")

    # BATHYMETRIES
    L.append("# ********************* BATHYMETRIES *********************\n")
    L.append("#      BATHYMETRY NO                                        BATHYMETRY FILE NAME\n")
    for b in range(1, NBOX + 1):
        L.append(f"{b:20d}{('BATHYMETRY_%d.txt' % b):>50}\n")

    # INITIAL CONDITIONS: assign set by box (interior=1, river boxes 3/24=2)
    L.append("# ********************* INITIAL CONDITIONS *********************\n")
    L.append("#   PELAGIC BOX NO   INIT COND SET NO   SURFACE ELEV   BOTTOM ELEV\n")
    for b in range(1, NBOX + 1):
        setno = 2 if b in (3, 24, 26) else 1
        L.append(f"{b:20d}{setno:20d}{0.0:20.4f}{-depth[b]:20.4f}\n")

    # MASS LOADS / WITHDRAWALS: none
    L.append("# ********************* MASS LOADS FOR EACH BOX *********************\n")
    for b in range(1, NBOX + 1):
        L.append(f"# PELAGIC BOX {b}    : NO MASS LOADS\n")
    L.append("# ********************* MASS WITHDRAWALS FOR EACH BOX *********************\n")
    for b in range(1, NBOX + 1):
        L.append(f"# PELAGIC BOX {b}    : NO MASS WITHDRAWALS\n")

    # ADVECTIVE LINKS from EUTROPY flux topology. A negative upstream box is an
    # open-boundary inflow (ESTAS: OPEN_BOUNDARY_NO = -UPSTREAM_BOX_NO, mod_SOLVER).
    L.append("# ********************* ADVECTIVE LINKS *********************\n")
    L.append("#  ADVECTIVE LINK NO        UPSTREAM BOX      DOWNSTREAM BOX"
             "             FLOW TS      FLOW TS VAR NO\n")
    for i, (f, t) in enumerate(links, start=1):
        L.append(f"{i:20d}{f:20d}{t:20d}{1:20d}{i:20d}\n")

    L.append("# ********************* DISPERSIVE LINKS *********************\n")
    L.append("#  DISP LINK NO   FIRST BOX   SECOND BOX   MIXING TS NO   MIXING LENGTH\n")

    # SETTLING VELOCITIES: 29 boxes x 36 vars (reader order: after DISPERSIVE)
    settle_vel = {5: 1, 9: 2, 10: 2, 11: 2, 16: 3, 18: 4}   # var -> settling vel no
    diss = {1, 2, 3, 4, 12, 13, 14, 17, 20, 21}
    names = _state_names()
    L.append("# ********************* SETTLING_VELOCITIES *********************\n")
    L.append("#     PELAGIC BOX NO        STATE VAR NO      DISSOLVED FRAC     "
             "SETTLING VEL NO  DEPOSITED FRACTION   CHLA_SUPRESSION_OF_SETTLING\n")
    for b in range(1, NBOX + 1):
        for v in range(1, NSTATE + 1):
            df = 1.00 if v in diss else 0.00
            sv = settle_vel.get(v, 0)
            L.append(f"{b:20d}{v:20d}{df:20.2f}{sv:20d}{0.9:20.6f}{1:30d}"
                     f"     ! BOX {b}: {names[v - 1]}\n")

    L.append("# ********************* OPEN BOUNDARIES *********************\n")
    L.append("#   OPEN BOUNDARY NO        STATE VAR NO       FORCING TS NO   FORCING TS VAR NO\n")
    for bi in range(1, NBND + 1):
        for v in range(1, NSTATE + 1):
            L.append(f"{bi:20d}{v:20d}{bi:20d}{v:20d}\n")

    # MASS LOADS / WITHDRAWALS forcing-ref sections (empty; 2 header lines each)
    L.append("# ********************* MASS LOADS *********************\n")
    L.append("#   MASS LOAD NO   STATE VAR NO   FORCING TS NO   FORCING TS VAR NO\n")
    L.append("# ********************* MASS WITHDRAWALS *********************\n")
    L.append("#   MASS WITHDRAWAL NO   STATE VAR NO   FORCING TS NO   FORCING TS VAR NO\n")

    # FORCING sections: each box -> its forcing TS var
    def forcing_section(title, tsno):
        s = [f"# ********************* {title} *********************\n",
             "#     PELAGIC BOX NO       FORCING TS NO   FORCING TS VAR NO\n"]
        for b in range(1, NBOX + 1):
            s.append(f"{b:20d}{tsno:20d}{b:20d}\n")
        return s
    L += forcing_section("WATER TEMPERATURE", 6)
    L += forcing_section("SALINITY", 7)
    L += forcing_section("SOLAR RADIATION", 8)
    L += forcing_section("FRACTION OF DAY", 9)
    # scalar meteorology: all boxes -> var 1
    def scalar_forcing(title, tsno):
        s = [f"# ********************* {title} *********************\n",
             "#     PELAGIC BOX NO       FORCING TS NO   FORCING TS VAR NO\n"]
        for b in range(1, NBOX + 1):
            s.append(f"{b:20d}{tsno:20d}{1:20d}\n")
        return s
    L += scalar_forcing("AIR TEMPERATURE", 10)
    L += scalar_forcing("WIND SPEED", 11)
    L += scalar_forcing("PRECIPITATION", 12)
    L += scalar_forcing("EVAPORATION", 13)
    L += scalar_forcing("ICE FRACTION", 14)

    # TS reference lists (reader reads INITIAL CONDITIONS set->file list first)
    L.append("# ********************* INITIAL CONDITIONS *********************\n")
    L.append("#   PEL. INIT SET NO      PELAGIC INITIAL CONDITION FILE NAME\n")
    L.append(f"{1:20d}{'INIT_CONC_1.txt':>40}\n")
    L.append(f"{2:20d}{'INIT_CONC_2.txt':>40}\n")
    L.append("# ********************* FLOW TIME SERIES *********************\n")
    L.append("# FLOW TIME SERIE NO                                   FLOW TIME SERIE FILE NAME\n")
    L.append(f"{1:20d}{'FLOW_TS.txt':>50}\n")
    L.append("# ********************* MIXING TIME SERIES *********************\n")
    L.append("# MIX. TIME SERIE NO                MIX. TIME SERIE FILE NAME\n")
    L.append("# ********************* SETTLING VEOCITIES *********************\n")
    L.append("# SET. TIME SERIE NO                                   SET. TIME SERIE FILE NAME\n")
    for i in range(1, 7):
        L.append(f"{i:20d}{('SETTLING_VELOCITY_TS_%d.txt' % i):>50}\n")
    L.append("# ********************* FORCING TIME SERIES *********************\n")
    L.append("# FORC TIME SERIE NO                                   FORC TIME SERIE FILE NAME\n")
    ts_files = ["FORC_TS_1.txt", "FORC_TS_2.txt", "FORC_TS_3.txt", "FORC_TS_4.txt",
                "FORC_TS_5.txt", "TEMP_TS.txt", "SALT_TS.txt", "SOLAR_RAD_TS.txt",
                "FORC_TS_9.txt", "AIR_TEMP_TS.txt", "WIND_SPEED_TS.txt",
                "RAINFALL_TS.txt", "EVAPORATION_TS.txt", "ICE_COVER.txt"]
    for i, f in enumerate(ts_files, start=1):
        L.append(f"{i:20d}{f:>50}\n")

    # trailing output-flag sections (each: 2 comment lines + 1 value)
    L.append("# ECOLOGICAL OUTPUTS\n# PRODUCE_ECOL_OUTPUT\n1\n")
    L.append("# SAVED OUTPUTS\n# CREATE_PELAGIC_SAVED_OUTPUTS\n0\n")
    L.append("# STATE VARIABLE OUTPUTS\n# CREATE_STATE VARIABLE OUTPUTS\n0\n")
    L.append("# COCOA OUTPUTS\n# PRODUCE_COCOA_OUTPUTS\n0\n")
    L.append("# PELAGIC EXERGY INPUTS\n# CALCULATE PELAGIC EXERGY\n0\n")
    L.append("# COST FUNCTION\n# PRODUCE_COST_FUNC\n0\n")

    with open(os.path.join(out, "PELAGIC_INPUTS.txt"), "w") as fh:
        fh.writelines(L)

    # settling velocity TS files (constant velocities, m/day)
    vels = [0.5, 0.1, 0.2, 1.0, 0.5, 0.3]
    for i, v in enumerate(vels, start=1):
        write_ts(os.path.join(out, f"SETTLING_VELOCITY_TS_{i}.txt"),
                 f"settling velocity {i} m/day", [0, 9999], [[v], [v]])


def _write_input_txt(repo, tdays):
    with open(os.path.join(repo, "INPUT_CL29.txt"), "w") as fh:
        fh.write("# DESCRIPTION Curonian Lagoon 29-box EUTROPY-derived\n")
        for i in range(2, 6):
            fh.write(f"# DESCRIPTION LINE {i}\n")
        fh.write("# BASE_YEAR\n           2012\n")
        fh.write(f"# SIMULATION_START\n{float(tdays[0]):15.1f}\n")
        fh.write(f"# SIMULATION_END\n{float(tdays[-1]):15.1f}\n")
        fh.write("# NUM_REPEATS\n              1\n")
        fh.write("# TIME_STEPS_PER_DAY\n            240\n")
        fh.write("# PRINT_INTERVAL IN TIME STEPS\n             10\n")
        fh.write("# PELAGIC MODEL INPUT FOLDER write the folder always with / in the end\n")
        fh.write("INPUTS_CL29/\n")
        fh.write("# PELAGIC MODEL INPUT FILE\n            PELAGIC_INPUTS.txt\n")
        fh.write("# PELAGIC MODEL OUTPUT FOLDER write the folder always with / in the end\n")
        fh.write("OUTPUTS_CL29/\n")
        fh.write("# RESUSPENSION_OPTION\n          0\n")
        fh.write("# MODEL_SEDIMENTS\n          0\n")
        fh.write("# NUM_PRESCRIBED_SEDIMENT_FLUX_SETS\n          0\n")
        fh.write("# SEDIMENT MODEL INPUT FILE\n")


if __name__ == "__main__":
    raise SystemExit(main())
