#!/usr/bin/env python3
"""Evaluate LIM_LIGHT's three day-length forms directly from the model's own
constants and forcings -- no model run required.

This is the calculation behind docs/CL29_phenology_diagnosis.md section 47, kept
as a cheap oracle for the Fortran LIGHT_DAYLENGTH_OPTION implementation and as a
reusable answer to "what would this light-climate change actually do?".

I_A is a DAILY INTEGRAL (aquabc_II_pelagic_model.f90: W/m2 x 0.5 PAR x 86400 s
x 0.238846 J->cal / 1e4 = langley/day), so a photoperiod correction can enter
two ways:

    Form A   LLIGHT = FDAY * f(I_A)          the live smith==0 library branch
    Form B   LLIGHT = FDAY * f(I_A / FDAY)   WASP/EUTRO; cf. CUR_SMITH's IAV

Form B is correct: while light-limited the depth-averaged Steele curve is nearly
linear in I/I_s, so the FDAY cancels -- a fixed daily dose spread over more or
fewer hours cannot change a near-linear daily integral. Form A weights by the
photoperiod without concentrating the dose, so it discards (1-FDAY) of each
day's light.

Usage:
    tools/probe_lim_light.py [--inputs INPUTS_CL29] [--outputs OUTPUTS_CL29]
                             [--group DIA] [--depth 3.5]
"""
import argparse
import collections
import datetime as dt
import math
import os
import statistics as st
import sys

E = math.e

# Per-group WCONST names: (growth rate, T_min, T_opt, T_max, C:Chl, fallback I_s)
GROUPS = {
    "DIA": ("KG_DIA_OPT_TEMP", "DIA_OPT_TEMP_LR", "DIA_OPT_TEMP_UR",
            "KAPPA_DIA_OVER_OPT_TEMP", "DIA_C_TO_CHLA", "I_S_DIA"),
    "CYN": ("KG_CYN_OPT_TEMP", "CYN_OPT_TEMP_LR", "CYN_OPT_TEMP_UR",
            "KAPPA_CYN_OVER_OPT_TEMP", "CYN_C_TO_CHLA", "I_S_CYN"),
    "OPA": ("KG_OPA_OPT_TEMP", "OPA_OPT_TEMP_LR", "OPA_OPT_TEMP_UR",
            "KAPPA_OPA_OVER_OPT_TEMP", "OPA_C_TO_CHLA", "I_S_OPA"),
}

# C:Chl per state variable, for reconstructing modelled CHLA from the .out file
CHLA_GROUPS = {"DIA_C": "DIA_C_TO_CHLA", "CYN_C": "CYN_C_TO_CHLA",
               "OPA_C": "OPA_C_TO_CHLA", "FIX_CYN_C": "FIX_CYN_C_TO_CHLA",
               "NOST_VEG_HET_C": "NOST_C_TO_CHLA"}

MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]


def read_wconst(path):
    """Parse a WCONST file: '<index> <NAME> <value> ! comment'."""
    out = {}
    with open(path) as fh:
        for line in fh:
            parts = line.split()
            if len(parts) >= 3 and parts[0].isdigit():
                try:
                    out[parts[1]] = float(parts[2])
                except ValueError:
                    pass
    return out


def read_ts(path, col=1, base_year=2012):
    """Read an ESTAS forcing time series into {date: value}."""
    out = {}
    base = dt.date(base_year, 1, 1)
    started = False
    with open(path) as fh:
        for line in fh:
            if "TIME AND VALUES" in line:
                started = True
                continue
            if not started:
                continue
            parts = line.split()
            if len(parts) < 2:
                continue
            try:
                out[base + dt.timedelta(days=float(parts[0]))] = float(parts[col])
            except (ValueError, OverflowError):
                pass
    return out


def ctmi(T, t_min, t_opt, t_max):
    """Cardinal Temperature Model with Inflection (Rosso et al. 1993).

    Returns 0 outside [t_min, t_max]. Mirrors GROWTH_AT_TEMP's CTMI branch;
    the caller is responsible for the 2*t_opt > t_min + t_max validity gate.
    """
    if T <= t_min or T >= t_max:
        return 0.0
    num = (T - t_max) * (T - t_min) ** 2
    den = ((t_opt - t_min) *
           ((t_opt - t_min) * (T - t_opt) - (t_opt - t_max) * (t_opt + t_min - 2 * T)))
    if den == 0:
        return 0.0
    return max(0.0, min(1.0, num / den))


def lim_light(Ia, Is, ke, H, fday, opt):
    """Depth-averaged Steele light limitation with the day-length option.

    opt 0 = legacy (no FDAY), 1 = Form A, 2 = Form B. BETA is 0 throughout
    (the CL29 configuration), so the photoinhibition term is a no-op.
    """
    t1 = ke * H
    if t1 < 1.0e-10:
        t1 = 1.0e-10
    t2 = 1.0 / Is
    t3 = math.exp(-t1)

    if opt == 1:
        ia_eff, weight = Ia, max(1e-6, min(1.0, fday))
    elif opt == 2:
        weight = max(1e-6, min(1.0, fday))
        ia_eff = Ia / weight
    else:
        ia_eff, weight = Ia, 1.0

    val = weight * (E / t1) * (math.exp(-t2 * ia_eff * t3) - math.exp(-t2 * ia_eff))
    return max(0.0, min(1.0, val))


def model_kd(k_b_e, chla, option):
    """Reproduce the model's extinction.

    LIGHT_EXTINCTION_OPTION 0 -> light_kd() (aquabc_II_pelagic_auxillary.f90);
    option 1 -> the case(1) expression in aquabc_II_pelagic_model.f90.

    NOTE: CL29 sets 0 in PELAGIC_MODEL_OPTIONS.txt while the AQUABC interface
    DEFAULTS to 1. Read the option file, not the default -- getting this wrong
    published a bad kd table once (doc section 44.3 method note).
    """
    c = max(chla, 0.0)
    if option == 0:
        return k_b_e + 0.4 + 0.02 * c if c <= 50.0 else k_b_e + 1.0 + 0.008 * c
    return k_b_e + 8.8e-3 * c + 5.4e-2 * (c ** (2.0 / 3.0))


def read_scalar_after(path, header_fragment):
    """Read the numeric line following a '# <fragment>' header in EXTRA_WCONST."""
    lines = open(path).read().splitlines()
    for i, line in enumerate(lines):
        if line.lstrip().startswith("#") and header_fragment.lower() in line.lower():
            for nxt in lines[i + 1:]:
                if nxt.strip() and not nxt.lstrip().startswith("#"):
                    return float(nxt.split()[0])
    raise KeyError(header_fragment)


def read_option(path, name):
    """Read an integer option that follows a '# <NAME>' header line."""
    lines = open(path).read().splitlines()
    for i, line in enumerate(lines):
        if line.lstrip().startswith("#") and name in line:
            for nxt in lines[i + 1:]:
                if nxt.strip() and not nxt.lstrip().startswith("#"):
                    return int(float(nxt.split()[0]))
    return None


def daily_chla(out_dir, wconst, box=1):
    """Reconstruct modelled total CHLA (ug/L) per day from a box output file."""
    path = os.path.join(out_dir, "PELAGIC_BOX_%05d.out" % box)
    chla = {}
    base = dt.date(2012, 1, 1)
    with open(path) as fh:
        header = fh.readline().split()
        idx = {n: i for i, n in enumerate(header)}
        for line in fh:
            parts = line.split()
            if len(parts) < len(header):
                continue
            day = base + dt.timedelta(days=float(parts[0]))
            total = 0.0
            for var, ratio_name in CHLA_GROUPS.items():
                ratio = wconst.get(ratio_name)
                if var in idx and ratio:
                    total += float(parts[idx[var]]) * 1000.0 / ratio
            chla[day] = total
    return chla


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--inputs", default="INPUTS_CL29")
    ap.add_argument("--outputs", default="OUTPUTS_CL29")
    ap.add_argument("--wconst", default=None,
                    help="WCONST file (default: <inputs>/WCONST_04.txt)")
    ap.add_argument("--group", default="DIA", choices=sorted(GROUPS))
    ap.add_argument("--depth", type=float, default=3.5,
                    help="representative column depth, m (CL29 mean 3.5)")
    ap.add_argument("--box", type=int, default=1)
    args = ap.parse_args()

    inp = args.inputs
    wc_path = args.wconst or os.path.join(inp, "WCONST_04.txt")
    w = read_wconst(wc_path)

    kg_n, tmin_n, topt_n, tmax_n, cchl_n, is_n = GROUPS[args.group]
    kg, cchl = w[kg_n], w[cchl_n]
    t_min, t_opt, t_max = w[tmin_n], w[topt_n], w[tmax_n]
    xkc, phimx = w["XKC"], w["PHIMX"]
    is_fallback = w[is_n]
    ice_trans = w.get("ICE_LIGHT_TRANS", 1.0)

    # CTMI validity gate -- aquabc_II_pelagic_auxillary.f90:84
    if not (2 * t_opt > t_min + t_max):
        print("WARNING: CTMI invalid for %s (2*T_opt <= T_min + T_max); the model "
              "silently falls back to the plateau branch." % args.group,
              file=sys.stderr)

    opts_path = os.path.join(inp, "PELAGIC_MODEL_OPTIONS.txt")
    ext_opt = read_option(opts_path, "LIGHT_EXTINCTION_OPTION")
    ext_opt = 0 if ext_opt is None else ext_opt
    k_b_e = read_scalar_after(os.path.join(inp, "EXTRA_WCONST.txt"), "K_B_E")

    solar = read_ts(os.path.join(inp, "SOLAR_RAD_TS.txt"))
    fday = read_ts(os.path.join(inp, "FORC_TS_9.txt"))
    temp = read_ts(os.path.join(inp, "TEMP_TS.txt"))
    ice_path = os.path.join(inp, "ICE_COVER.txt")
    ice = read_ts(ice_path) if os.path.exists(ice_path) else {}
    chla = daily_chla(args.outputs, w, args.box)

    print("group %s   KG %.4f   CTMI(%.1f, %.1f, %.1f)   C:Chl %.1f   "
          "XKC %.3f   PHIMX %.0f" % (args.group, kg, t_min, t_opt, t_max, cchl, xkc, phimx))
    print("K_B_E %.3f   LIGHT_EXTINCTION_OPTION %d   ICE_LIGHT_TRANS %.3f   H %.2f m"
          % (k_b_e, ext_opt, ice_trans, args.depth))
    print()

    per_month = collections.defaultdict(list)
    days = sorted(set(solar) & set(fday) & set(temp) & set(chla))
    for day in days:
        gitmax = kg * ctmi(temp[day], t_min, t_opt, t_max)
        if gitmax < 1e-12:
            continue                      # no growth -> LIM_LIGHT is not exercised
        i_s = gitmax * cchl * E / (0.083 * phimx * xkc)
        if i_s < 10.0:                    # LIM_LIGHT's own fallback
            i_s = is_fallback
        i_a = (solar[day] * 0.5 * 8.64e4 * 0.238846) / 1.0e4
        i_a *= 1.0 - (ice.get(day, 0.0) * (1.0 - ice_trans))
        ke = model_kd(k_b_e, chla[day], ext_opt)
        f = fday[day]

        base = lim_light(i_a, i_s, ke, args.depth, f, 0)
        form_a = lim_light(i_a, i_s, ke, args.depth, f, 1)
        form_b = lim_light(i_a, i_s, ke, args.depth, f, 2)
        per_month[day.month].append((base, form_a, form_b, i_a / i_s, ke, f))

    print("%3s %5s %6s %8s %8s %8s %7s %7s" %
          ("mo", "n", "I/Is", "legacy", "FormA", "FormB", "A/cur", "B/cur"))
    ratios = {}
    for m in range(1, 13):
        rows = per_month.get(m)
        if not rows:
            print("%3s %5s   (no growth days: CTMI = 0 throughout)" % (MONTHS[m - 1], "-"))
            continue
        base = st.mean(r[0] for r in rows)
        fa = st.mean(r[1] for r in rows)
        fb = st.mean(r[2] for r in rows)
        u = st.mean(r[3] for r in rows)
        ratios[m] = (fa / base, fb / base)
        print("%3s %5d %6.2f %8.4f %8.4f %8.4f %7.3f %7.3f"
              % (MONTHS[m - 1], len(rows), u, base, fa, fb, fa / base, fb / base))

    print()
    for m, name in ((2, "February"), (5, "May"), (10, "October")):
        if m in ratios:
            a, b = ratios[m]
            print("%-9s Form A %+6.1f%%   Form B %+6.1f%%"
                  % (name, 100 * (a - 1), 100 * (b - 1)))

    if 2 in ratios and 5 in ratios:
        print()
        print("differential (May/Feb):  Form A %.2fx   Form B %.2fx"
              % (ratios[5][0] / ratios[2][0], ratios[5][1] / ratios[2][1]))
        print("Doc section 47.2 reference: Form A 1.68x, Form B 1.04x "
              "(Feb -60.5%/-22.1%, May -33.5%/-19.0%).")


if __name__ == "__main__":
    main()
