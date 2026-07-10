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
import re
import shutil

REPO = os.getcwd()
TMPL = os.path.join(REPO, "INPUTS", "PELAGIC_INPUTS.txt")
NET = os.path.join(REPO, "tools", "eutropy_poc", "net")
OUT = os.path.join(REPO, "INPUTS_CL29")
OUTPUT_FOLDER = "OUTPUTS_CL29"      # ESTAS pelagic output dir (relative to run CWD)
NBOX = 29
NSTATE = 36                 # ESTAS pelagic state vars (32 core + 4 allelopathy)

# ---- CL29 diatom-presence overrides (single source of truth) --------------------
# EUTROPY carries no diatoms, so the raw net/ data has DIA_C = 0 everywhere, and the
# 25-box WCONST temperature constants were calibrated for the OLD plateau temperature
# model. Under CTMI (aquabc GROWTH_AT_TEMP, USE_CTMI=.true.) those un-recalibrated
# constants make diatoms a 24 C warm-water species that cannot grow in their cold
# spring window, so the pool decays to the absorbing-zero state and never blooms.
# These overrides give diatoms a cold CTMI optimum, a small initial seed, and a
# continuous open-boundary refuge. Applied here (not in the shared INPUTS/ source)
# so they stay scoped to the 29-box application and survive regeneration.
# Phytoplankton groups EUTROPY does not carry (raw net/ data has them at 0): give
# each a small initial seed and a continuous open-boundary refuge so the pool cannot
# reach the multiplicative absorbing-zero state. 0-based index into the 32 core state
# vars -> (initial seed, boundary refuge), mg C/L.
CL29_PHYTO_REFUGE = {
    4:  (0.002, 0.02),      # DIA_C     - diatoms
    18: (0.002, 0.02),      # FIX_CYN_C - N-fixing cyanobacteria
}
# CL29 overrides applied to the copied WCONST file, matched by constant name.
# CTMI temperature constants (*_LR -> T_min, *_UR -> T_opt, KAPPA_*_OVER -> T_max):
# warm-group T_max is lowered so each T_opt stays above its range midpoint (avoids
# the CTMI denominator singularity that spikes LIM_TEMP to 1 at the cold cutoff).
#
# NB (see docs/CL29_Parameter_Validation.md): the diatom T_opt=10 / T_max=21 are
# EFFECTIVE-NICHE CALIBRATION values, not physiology (cultured diatom optima are
# ~15-25 C). They make the temperature term a proxy for spring-bloom phenology
# because DISS_Si stays ~1.5 mg/L (>> KHS_DSi 0.013) -- Si never limits, so the
# thermal cutoff is the load-bearing control that ends the spring bloom and hands
# off to cyanobacteria. A test at T_opt=16 / T_max=28 (near-literature) confirmed
# grazing still prevents a summer diatom takeover, but the summer cyano bloom
# weakens and destabilises (worse fit); the low T_opt is kept as a deliberate
# calibration tradeoff, not a physiological claim.
# Other algae (OPA) get an early-summer optimum (T_opt 17), a competitive phosphorus
# half-saturation (KHS_DIP_OPA), and reduced settling (see vels[] below) so they can
# actually bloom in the clear-water phase between the spring diatoms (T_opt 10) and
# summer cyanobacteria (T_opt 26). The thermal window alone leaves OPA phosphate-
# starved (diatoms with KHS_DIP 0.005 draw PO4 below OPA's default 0.013 half-sat),
# and even with P affinity a sharp window + heavy settling caps accumulation; the
# combination (window + KHS_DIP + low settling) lets OPA form a real shoulder bloom.
CL29_WCONST_OVERRIDE = {
    "DIA_OPT_TEMP_LR": -2.0, "DIA_OPT_TEMP_UR": 10.0, "KAPPA_DIA_OVER_OPT_TEMP": 21.0,
    "KAPPA_CYN_OVER_OPT_TEMP": 34.0, "KAPPA_FIX_CYN_OVER_OPT_TEMP": 32.0,
    "OPA_OPT_TEMP_LR": 10.0, "OPA_OPT_TEMP_UR": 17.0, "KAPPA_OPA_OVER_OPT_TEMP": 23.0,
    "KHS_DIP_OPA": 0.006,
    "KAPPA_NOST_VEG_HET_OVER_OPT_TEMP": 33.0,
}

# Phase-1 sediment diagenesis (MODEL_SEDIMENTS=2), opt-in and off by default: when
# False the converter emits no sediment files and INPUT_CL29 keeps MODEL_SEDIMENTS=0,
# so the baseline stays byte-identical. See
# docs/superpowers/specs/2026-07-08-cl29-sediment-diagenesis-phase1-design.md.
CL29_ENABLE_SEDIMENTS = False
# Sediment carbonate ICs. None = use the template's values (INORG_C/TOT_ALK ~0.003,
# a physically realistic pore-water DIC). If the staged run's CO2SYS hard-stops with
# 'pH does not converge', set to (INORG_C, TOT_ALK) ~= (3.0, 3.1): the inflated
# magnitude this codebase's CO2SYS empirically needs (the pelagic uses 3.0/3.1). The
# two readings conflict; the run decides. See spec section 5.
CL29_SED_CARBONATE_IC = None
# CL29 sediment ADVANCED REDOX. 0 = only the oxic + sub-oxic (NO3) DON/DOP
# mineralization pathways (Phase-1/2a default, matching CL29 pelagic redox=0). 1 =
# also enable the anoxic Mn/Fe/SO4/methanogenesis mineralization pathways, so deeply
# anoxic sediment recycles N/P instead of burying it -- the mechanism needed to lift
# box-19 benthic P toward closing the spring-diatom gap (spec section 4.1a). This
# creates a pelagic(0)/sediment(1) redox mismatch, which the model only WARNS on (it
# does not stop). NB: reductive Fe-oxide P release (K_SP_FEPO4) stays disabled -- it
# is commented out in the Fortran, not a constant -- so this enables anoxic
# *mineralization* P return, not Fe-oxide dissolution P.
CL29_SED_ADVANCED_REDOX = 1
# Reductive Fe(III)-P coupling strength (W_SED_CONST #171 FE_P_REDOX_FRAC). Fraction of the
# sediment PO4 solid-sorption capacity released as Fe(III) is reduced under anoxia -- the
# "iron curtain" internal-loading mechanism. 0 = redox-independent sorption (baseline).
# A 2026-07-10 sweep (0..1, 1-yr and 5-yr) showed it lifts the benthic PO4 flux strongly
# and monotonically (box-19 +609% summer at 1.0), but the absolute flux is ~1000x too
# small to move water-column PO4 or diatoms in ANY of the 5 years, so it does NOT close
# the section 4.1a spring-diatom gap. Over 5 yr the baseline flux DECLINES (2.6e-7 ->
# ~5e-9 g P/m2/day as surface sediment P depletes), so the knob's absolute effect shrinks
# with time even as its multiplier grows. The bottleneck is the sediment P-flux magnitude,
# not the redox partitioning. Kept off (0.0) for CL29. If enabling for another application
# with a larger base flux, ~0.3-0.5 is safer than the convex, twitchy top of the range.
CL29_SED_FE_P_REDOX_FRAC = 0.0
# CL29 sediment STABILITY calibration (Phase 1). The template sediment config is
# numerically unstable for CL29's diatom-driven PART_Si deposition: particulate
# silica (SED_PSi) accumulates unbounded in the thin 5 mm surface layer and the
# solver trips ~day 48. Rebalanced so deposition/dissolution/burial balance at a
# bounded, plausible SED_PSi (~1350) over the full 5-yr run. These are STABILITY
# values, NOT yet calibrated to measured Curonian Si fluxes (that is Phase 2).
# NB: the sediment path is NOT OpenMP-safe -- build & run SERIAL (no OPENMP=1).
CL29_SED_DEPTHS = [0.05, 0.05, 0.05, 0.05, 0.05, 0.07, 0.10]  # m; thicker surface layers
CL29_SED_BURIAL = 0.000274        # m/day; 10x template -> spreads deposited Si to depth
CL29_SED_CONST_OVERRIDE = {       # W_SED_CONST constants, matched by name
    "K_OXIC_DISS_PSi":   0.1,     # 20x template: faster particulate-Si dissolution
    "K_ANOXIC_DISS_PSi": 0.02,    # 20x template
    "FE_P_REDOX_FRAC":   CL29_SED_FE_P_REDOX_FRAC,  # reductive Fe(III)-P coupling (0=off)
}

# ---- Phase-2a two-type (sandy/muddy) sediment authoring --------------------------
# Baseline geometry seeds extracted from the shipped template so BOTH per-type profile
# dicts have a defined value for every field (Phase 1 only seeded depths + burial; the
# template supplied porosity/density/mixing verbatim, so there was no constant to seed
# them from). These are placeholders shared by both types until Phase 2b differentiates
# sandy vs muddy from measured fluxes.
CL29_SED_POROSITIES = [0.40, 0.40, 0.40, 0.40, 0.30, 0.25, 0.25]  # template SED_POROSITIES
CL29_SED_DENSITIES  = [1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75]  # template SED_DENSITIES
CL29_SED_MIXING     = 0.0000264                                   # template surface Db0
# Per-type profiles. Each carries all six fields {depths, porosities, densities, burial,
# mixing, ic_overrides}. ic_overrides is a sparse {var_index_1based: [per-layer values]}
# patch applied on top of the template's 24xL IC base.
#
# Phase-2b differentiation (2026-07-10, analytical seed per spec section 4.1 step 1). The
# measured sandy vs muddy benthic fluxes (INPUTS/FLUXES_FOR_{SANDY,MUDDY}_SEDIMENTS.txt;
# Petkuviene 2016) show muddy >> sandy for SOD (summer -79.9 vs -56.8 mmol/m2/d), NH4
# (6.62 vs 0.35), and biogenic Si (6.24 vs 2.17) -- all driven by muddy's higher ORGANIC
# loading, not porosity (which per spec section 4.2 counter-intuitively makes mud MORE oxic in
# this diffusion model). So the differentiator is the IC organic pools (PON/POP/POC/PSi =
# sediment vars 4/7/10/12), within the section-4.2 0.1x-10x band: SANDY keeps the template
# base; MUDDY = 2.5x the organic pools, with modestly higher porosity/bioturbation. These
# are a grounded SEED (right direction, in-bounds), NOT an iterated fit -- the fixed-point
# flux-matching iteration is spec section 4.2's own cycle.
#
# Validation (2026-07-10, 240-d CL29 run, box 19 muddy vs sandy-box mean, summer): the seed
# gets 3 of 4 solute DIRECTIONS right -- NH4 2.4x, SOD 1.24x (measured ~1.4x, close), PO4
# 1.26x (all muddy>sandy, as measured). DISS_Si comes out BACKWARDS (0.30x modeled vs ~2.9x
# measured): per spec section 4.2 the sandy/muddy Si contrast must come from biogenic-Si
# DEPOSITION + temperature, not an IC multiplier -- raising muddy PSi + porosity transiently
# depletes surface PSi and over-oxygenates, suppressing the Si flux. Fixing Si (and tightening
# the others) is the deferred section-4.2 iteration, not the seed's job.
#
# NB (spec section 4.1a, CONFIRMED this session): this differentiation improves FIDELITY to
# measured sandy/muddy fluxes but does NOT close the box-19 spring-diatom gap. Under CL29
# advanced-redox=0 the muddy DIFFUSIVE PO4 flux is small and net-sink (measured spring +0.03,
# summer -0.10 mmol/m2/d); the large muddy P release the gap needs is the separate Fe-redox/
# hypoxia PULSE (FLUXES_FOR_MUDDY_SEDIMENTS_HYPOXIA), which advanced-redox=0 cannot produce.
# The FE_P sweep + 5-yr runs (memory) showed the modeled benthic P is ~1000x too small to move
# water-column PO4. Gap closure needs EXTERNAL P (river/boundary), not a sediment mechanism.
_OM_MUDDY_FACTOR = 2.5
CL29_SED_SANDY = {
    "depths":       CL29_SED_DEPTHS,
    "porosities":   CL29_SED_POROSITIES,      # template (leaner, lower-energy-poor sand)
    "densities":    CL29_SED_DENSITIES,
    "burial":       CL29_SED_BURIAL,
    "mixing":       CL29_SED_MIXING,
    "ic_overrides": {},                       # template IC base
}
CL29_SED_MUDDY = {
    "depths":       CL29_SED_DEPTHS,
    "porosities":   [0.55, 0.50, 0.45, 0.42, 0.35, 0.30, 0.30],  # organic mud, modestly higher
    "densities":    CL29_SED_DENSITIES,
    "burial":       CL29_SED_BURIAL,
    "mixing":       8.0e-5,                    # bioturbated mud (template Db0 2.64e-5)
    # 2.5x the template organic pools (PON/POP/POC/PSi) -> higher SOD/NH4/Si return
    "ic_overrides": {
        4:  [x * _OM_MUDDY_FACTOR for x in [1000.0, 1000.0, 1000.0, 1000.0, 1500.0, 1500.0, 1500.0]],  # PON
        7:  [x * _OM_MUDDY_FACTOR for x in [10.0, 10.0, 10.0, 10.0, 130.0, 165.0, 165.0]],              # POP
        10: [x * _OM_MUDDY_FACTOR for x in [1400.0, 1400.0, 1400.0, 1400.0, 1300.0, 1000.0, 1000.0]],   # POC
        12: [x * _OM_MUDDY_FACTOR for x in [10.0, 10.0, 10.0, 10.0, 130.0, 165.0, 165.0]],              # PSi
    },
}
# Box -> sediment type ('sandy' -> 1, 'muddy' -> 2). A box absent from the map defaults to
# type 1 (sandy). EMPTY by default: the empty map routes _write_sediment_inputs through the
# unmodified Phase-1 single-profile path, keeping that output byte-identical.
#
# DATA-BLOCKED (2026-07-10): a realistic full 29-box assignment needs the actual Curonian
# sediment-facies map, which is NOT derivable from the in-repo data. A depth heuristic does
# NOT work: box 19 -- the muddy interior exemplar (spec section 1) -- is shallow (1.52 m, below
# the 2.91 m median), so depth would misclassify it as sandy. The map is left empty (CL29 stays
# single-type / byte-identical) until the facies map is supplied. The differentiated MUDDY
# profile above is ready; populate e.g. {19: 'muddy', <other interior fine-sediment boxes>: ...}
# to activate the two-type author. Validated as a direction check with {19:'muddy'} (box 19
# shows the muddy SOD/NH4/Si signature vs sandy boxes) -- see the Phase-2b validation.
CL29_SEDIMENT_TYPE = {}
_SED_TYPE_TO_INDEX = {"sandy": 1, "muddy": 2}


def _apply_wconst_overrides(path, overrides):
    """Rewrite named constants into a copied WCONST-style file (pelagic WCONST or
    sediment W_SED_CONST). Matches each constant by name and replaces only its numeric
    value; errors out if any name is missing or ambiguous."""
    with open(path) as fh:
        text = fh.read()
    for name, val in overrides.items():
        text, n = re.subn(r"(\b" + re.escape(name) + r"\s+)-?\d+(?:\.\d+)?",
                          lambda m: m.group(1) + str(val), text)
        if n != 1:
            raise SystemExit(f"WCONST override: '{name}' matched {n} times (expected 1)")
    with open(path, "w") as fh:
        fh.write(text)


def _set_temperature_model_ctmi(path):
    """CL29 opts into the CTMI temperature model: set TEMPERATURE_MODEL = 1 in the
    copied PELAGIC_MODEL_OPTIONS.txt (append the option if the template lacks it)."""
    with open(path) as fh:
        lines = fh.readlines()
    for i, ln in enumerate(lines):
        if ln.strip().startswith("# TEMPERATURE_MODEL"):
            lines[i + 1] = "            1\n"
            break
    else:
        lines.append("# TEMPERATURE_MODEL (0=plateau, 1=CTMI)\n")
        lines.append("            1\n")
    with open(path, "w") as fh:
        fh.writelines(lines)
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
    # ESTAS does not create its output folder; make it so the pipeline runs
    # end-to-end from a clean checkout (INPUT_CL29.txt points pelagic output here).
    os.makedirs(os.path.join(REPO, OUTPUT_FOLDER), exist_ok=True)
    state_block = template_blocks()

    # ---- source data from net/ ----
    _, ic_rows = read_csv_matrix(os.path.join(NET, "initial_conditions.csv"))
    ic = {int(r[0]): [float(x) for x in r[1:1 + 32]] for r in ic_rows}
    for vec in ic.values():                     # CL29: seed groups EUTROPY lacks
        for idx, (seed, _refuge) in CL29_PHYTO_REFUGE.items():
            vec[idx] = max(vec[idx], seed)
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
            for idx, (_seed, refuge) in CL29_PHYTO_REFUGE.items():  # CL29 phyto refuge
                vec32[idx] = max(vec32[idx], refuge)
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
    # CL29 opts into CTMI (its temperature constants are recalibrated for it)
    _set_temperature_model_ctmi(os.path.join(OUT, "PELAGIC_MODEL_OPTIONS.txt"))
    # output info: one row PER BOX (state-var / process-rate / mass-balance flags)
    with open(os.path.join(OUT, "PELAGIC_OUTPUT_INFORMATION_FILE.txt"), "w") as fh:
        fh.write("#  BOX_NO   STATE_VAR_OUT   PROCESS_RATE_OUT   MASS_BALANCE_OUT\n")
        for b in range(1, NBOX + 1):
            fh.write(f"{b:12d}{1:12d}{0:12d}{0:12d}\n")

    # constants files (box-independent): copy main + extra
    for f in ("WCONST_04.txt", "EXTRA_WCONST.txt"):
        shutil.copy(os.path.join(REPO, "INPUTS", f), os.path.join(OUT, f))
    # CL29: apply the diatom/OPA temperature + phosphorus-affinity overrides to the copy
    _apply_wconst_overrides(os.path.join(OUT, "WCONST_04.txt"), CL29_WCONST_OVERRIDE)

    # ---- master PELAGIC_INPUTS.txt ----
    _write_master(OUT, state_block, links, depth, area)
    _write_sediment_inputs(OUT, CL29_ENABLE_SEDIMENTS, CL29_SEDIMENT_TYPE)
    _write_input_txt(REPO, tdays, CL29_ENABLE_SEDIMENTS)

    print(f"[estas] wrote 29-box INPUTS to {OUT}/ "
          f"({NBOX} bathymetries, {len(links)} links, {NBND} boundaries)")
    print("[estas] run with:  ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt")
    print("[estas]   (ESTAS_HOLD_VOLUME=1 holds box volumes constant; EUTROPY's "
          "fluxes are not per-box volume-conserving so ESTAS's flux-derived "
          "volumes would otherwise drift to a negative-mass crash.)")
    return 0


def _load_area():
    # Box surface area (m2) rides in the tracked net/depths.csv, so the pipeline
    # runs from a fresh clone without the external ~/eutropy/input tree.
    with open(os.path.join(NET, "depths.csv")) as fh:
        return {int(r["box"]): float(r["area_m2"]) for r in csv.DictReader(fh)}


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
    # #3 is OPA_C only: reduced 0.2 -> 0.05 (motile green algae) so OPA is not sunk
    # out of its narrow clear-water-phase window before it can accumulate.
    vels = [0.5, 0.1, 0.05, 1.0, 0.5, 0.3]
    for i, v in enumerate(vels, start=1):
        write_ts(os.path.join(out, f"SETTLING_VELOCITY_TS_{i}.txt"),
                 f"settling velocity {i} m/day", [0, 9999], [[v], [v]])


def _replace_leading_number(line, new_val):
    """Replace a value line's numeric content with new_val, preserving indent + EOL."""
    eol = "\r\n" if line.endswith("\r\n") else "\n"
    stripped = line.rstrip("\r\n")
    indent = stripped[:len(stripped) - len(stripped.lstrip())]
    return f"{indent}{new_val}{eol}"


def _sed_ic_block_bounds(lines):
    """(start, end) line indices of the 24-row sediment INITIAL CONDITIONS data block."""
    for i, ln in enumerate(lines):
        # Exact match: '# INIT_SED_STATE_VARS_OPTIONS' appears first and must NOT match.
        if ln.strip() == "# INIT_SED_STATE_VARS":
            start = i + 1
            while start < len(lines) and (
                    not lines[start].strip() or lines[start].lstrip().startswith("#")):
                start += 1
            return start, start + 24
    raise SystemExit("sediment template missing '# INIT_SED_STATE_VARS'")


def _override_sed_carbonate(lines, inorg_c, tot_alk, nlayers=7):
    """Overwrite INORG_C (var 13) and TOT_ALK (var 14) IC rows with nlayers copies.
    Off by default (CL29_SED_CARBONATE_IC is None -- Phase 1 confirmed the template ICs
    converge); this path is UNIT-TESTED ONLY, so do a short end-to-end run before trusting
    it if ever enabled. Writes space-separated floats (list-directed reader parses them)."""
    start, _ = _sed_ic_block_bounds(lines)
    eol = "\r\n" if lines[start].endswith("\r\n") else "\n"
    def row(val):
        return " ".join(f"{val:.6f}" for _ in range(nlayers)) + eol
    lines[start + 12] = row(inorg_c)   # sediment state var 13 = INORG_C
    lines[start + 13] = row(tot_alk)   # sediment state var 14 = TOT_ALK
    return lines


def _override_sed_geometry(lines, depths, burial):
    """Overwrite the SED_DEPTHS (7 layer values) and SED_BURRIALS rows with the CL29
    stability geometry, preserving CRLF. Matches each block by its unit-tagged header."""
    def _replace_run(header_test, values):
        for i, ln in enumerate(lines):
            if header_test(ln):
                j, n = i + 1, 0
                while j < len(lines) and n < len(values):
                    if lines[j].lstrip().startswith("#"):
                        break   # next section reached early -> errors below (no bleed)
                    try:
                        float(lines[j].split("!")[0].strip())
                    except ValueError:
                        j += 1
                        continue
                    eol = "\r\n" if lines[j].endswith("\r\n") else "\n"
                    lines[j] = f"        {values[n]:.6f}{eol}"
                    n += 1
                    j += 1
                if n != len(values):
                    raise SystemExit(f"sediment geometry: expected {len(values)} values")
                return
        raise SystemExit("sediment geometry header not found")
    _replace_run(lambda ln: "SED_DEPTHS" in ln and "meters" in ln, depths)
    _replace_run(lambda ln: "SED_BURRIALS" in ln and "m/day" in ln, [burial])
    return lines


def _sed_template_number(lines, header_test):
    """Return the float on the value line immediately after the first matching header."""
    for i, ln in enumerate(lines):
        if header_test(ln):
            return float(lines[i + 1].split("!")[0].split()[0])
    raise SystemExit("sediment template header not found")


def _sed_ic_base(lines, nvars=24, nlayers=7):
    """Parse the template's 24xL INIT_SED_STATE_VARS block into a list of nvars rows,
    each a list of nlayers floats (the base every per-type IC block is composed from)."""
    start, end = _sed_ic_block_bounds(lines)
    return [[float(x) for x in lines[i].split()[:nlayers]] for i in range(start, end)]


def _compose_type_ic(ic_base, ic_overrides, nlayers=7):
    """Full 24xL IC for one type: template base, then the type's sparse ic_overrides
    ({var_1based: [nlayers values]}), then -- if configured -- the shared carbonate-IC
    floor (INORG_C=var 13, TOT_ALK=var 14), which applies to BOTH types (it reflects a
    pore-water DIC/alkalinity floor CO2SYS needs, not a grain-size property)."""
    ic = [list(row) for row in ic_base]
    for var1, vals in ic_overrides.items():
        if len(vals) != nlayers:
            raise SystemExit(f"ic_overrides var {var1}: expected {nlayers} values")
        ic[var1 - 1] = list(vals)
    if CL29_SED_CARBONATE_IC is not None:
        inorg_c, tot_alk = CL29_SED_CARBONATE_IC
        ic[12] = [inorg_c] * nlayers   # var 13 = INORG_C
        ic[13] = [tot_alk] * nlayers   # var 14 = TOT_ALK
    return ic


def _author_multitype_sediment(out, sediment_type):
    """Emit the extended two-type BOTTOM_SEDIMENT_MODEL_INPUT.txt (spec section 3.1 layout):
    preamble, # NUM_SED_TYPES + count, # SED_TYPE_PER_BOX + one integer per box, then one
    per-type profile block {depths(L), porosities(L), densities(L), mixing(1), burial(1),
    IC(24xL)} sandy-first muddy-second, then GLOBAL ADVECTIVE_VELOCITY and SURF_MIXLEN, then
    the constants + output sections once. Field cardinalities and the 3/3/3/3/3/4 pre-data
    skip records match the reader's positional skip-counts (mod_BOTTOM_SEDIMENTS)."""
    with open(os.path.join(REPO, "INPUTS", "BOTTOM_SEDIMENT_MODEL_INPUT.txt"),
              newline="") as fh:
        tlines = fh.readlines()
    eol = "\r\n" if tlines[0].endswith("\r\n") else "\n"

    nlayers = int(_sed_template_number(tlines, lambda l: l.lstrip().startswith("# NUM_SED_LAYERS")))
    adv_vel = _sed_template_number(tlines, lambda l: "ADVECTIVE_VELOCITY" in l and "m/day" in l)
    surf_mixlen = _sed_template_number(tlines, lambda l: "SURF_MIXLEN" in l and "(m)" in l)
    ic_base = _sed_ic_base(tlines, nvars=24, nlayers=nlayers)

    profiles = [("sandy", CL29_SED_SANDY), ("muddy", CL29_SED_MUDDY)]  # type 1, type 2
    num_types = len(profiles)

    def type_index(box):
        name = sediment_type.get(box, "sandy")
        if name not in _SED_TYPE_TO_INDEX:
            raise SystemExit(f"CL29_SEDIMENT_TYPE box {box}: unknown type '{name}'")
        return _SED_TYPE_TO_INDEX[name]

    L = []

    def emit(s):
        L.append(s + eol)

    # Preamble: description lines + advanced-redox flag + NUM_SED_LAYERS (from template).
    for i in range(5):
        emit("# DESCRIPTION LINE %d" % (i + 1))
    emit("# ADVANCED REDOX SIMULATION")
    emit("                %d" % CL29_SED_ADVANCED_REDOX)
    emit("# NUM_SED_LAYERS")
    emit("        %d" % nlayers)

    # Two-type header + count, then one type index per box (all NBOX boxes, one per line).
    emit("# NUM_SED_TYPES")
    emit("        %d" % num_types)
    emit("# SED_TYPE_PER_BOX")
    for box in range(1, NBOX + 1):
        emit("        %d" % type_index(box))

    # One profile block per type (sandy first, muddy second), only per-type fields.
    for name, prof in profiles:
        emit("# SED_DEPTHS_OPTIONS         (type %s)" % name)
        emit("        1")
        emit("# SED_DEPTHS          (meters)")
        for v in prof["depths"]:
            emit("    %.10g" % v)

        emit("# SED_POROSITIES_OPTIONS")
        emit("        1")
        emit("# SED_POROSITIES     (unitless)")
        for v in prof["porosities"]:
            emit("    %.10g" % v)

        emit("# SED_DENSITIES_OPTIONS")
        emit("        1")
        emit("# SED_DENSITIES      (g/cm^3)")
        for v in prof["densities"]:
            emit("    %.10g" % v)

        emit("# PART_MIXING_COEFFS_OPTIONS")
        emit("        1")
        emit("# PART_MIXING_COEFFS (m^2/day)")
        emit(" %.10g" % prof["mixing"])

        emit("# SED_BURRIALS_OPTIONS")
        emit("        1")
        emit("# SED_BURRIALS       (m/day)")
        emit(" %.10g" % prof["burial"])

        emit("# INIT_SED_STATE_VARS_OPTIONS")
        emit("        1")
        emit("# INIT_SED_STATE_VARS")
        emit("#    Layer 1 ... Layer %d" % nlayers)
        for row in _compose_type_ic(ic_base, prof["ic_overrides"], nlayers):
            emit(" ".join("%.10g" % v for v in row))

    # GLOBAL advective velocity + surface mixing length (once, relocated to the tail).
    emit("# ADVECTIVE_VELOCITY_OPTIONS")
    emit("        1")
    emit("# ADVECTIVE_VELOCITY (m/day)")
    emit("    %.10g" % adv_vel)
    emit("# SURF_MIXLEN_OPTIONS")
    emit("        1")
    emit("# SURF_MIXLEN        (m)")
    emit("    %.10g" % surf_mixlen)

    # Constants + output-organization sections, copied verbatim from the template tail so
    # the reader's common (post-branch) parse is byte-for-byte what it already expects.
    for i, ln in enumerate(tlines):
        if ln.lstrip().startswith("# MODEL_COEFFICIENTS_FOR_BOTTOM_SEDIMENTS_OPTIONS"):
            L.extend(tlines[i:])
            break
    else:
        raise SystemExit("sediment template missing constants section")

    with open(os.path.join(out, "BOTTOM_SEDIMENT_MODEL_INPUT.txt"), "w", newline="") as fh:
        fh.writelines(L)


def _write_sediment_inputs(out, enable_sediments, sediment_type=None):
    """Sediment stand-up. When enabled, copy the 170-constant W_SED_CONST.txt (with the CL29
    stability constant overrides) and author BOTTOM_SEDIMENT_MODEL_INPUT.txt. Two paths:

    * sediment_type empty/None -> the unmodified Phase-1 single-profile template patch
      (advanced-redox from config, CL29 stability geometry, optional carbonate IC). Output
      is byte-identical to Phase 1 -- do NOT route this through the multi-type author.
    * sediment_type non-empty  -> the two-type author emits the extended # NUM_SED_TYPES /
      # SED_TYPE_PER_BOX layout with sandy/muddy profile blocks.

    No-op when disabled. Run SERIAL -- the sediment path is not OpenMP-safe."""
    if not enable_sediments:
        return
    dst_const = os.path.join(out, "W_SED_CONST.txt")
    shutil.copy(os.path.join(REPO, "INPUTS", "W_SED_CONST.txt"), dst_const)
    if CL29_SED_CONST_OVERRIDE:
        _apply_wconst_overrides(dst_const, CL29_SED_CONST_OVERRIDE)

    if sediment_type:
        _author_multitype_sediment(out, sediment_type)
        return

    with open(os.path.join(REPO, "INPUTS", "BOTTOM_SEDIMENT_MODEL_INPUT.txt"),
              newline="") as fh:
        lines = fh.readlines()                       # newline="" preserves CRLF
    # Set sediment ADVANCED REDOX from CL29_SED_ADVANCED_REDOX. CL29 pelagic redox is 0
    # (PELAGIC_MODEL_OPTIONS line 4); a pelagic/sediment redox mismatch only warns, not
    # stops. See CL29_SED_ADVANCED_REDOX above for the rationale and caveats.
    for i, ln in enumerate(lines):
        if ln.lstrip().startswith("# ADVANCED REDOX SIMULATION"):
            lines[i + 1] = _replace_leading_number(lines[i + 1], CL29_SED_ADVANCED_REDOX)
            break
    else:
        raise SystemExit("sediment template missing '# ADVANCED REDOX SIMULATION'")
    lines = _override_sed_geometry(lines, CL29_SED_DEPTHS, CL29_SED_BURIAL)
    if CL29_SED_CARBONATE_IC is not None:
        lines = _override_sed_carbonate(lines, *CL29_SED_CARBONATE_IC)
    with open(os.path.join(out, "BOTTOM_SEDIMENT_MODEL_INPUT.txt"), "w",
              newline="") as fh:
        fh.writelines(lines)


def _write_input_txt(repo, tdays, enable_sediments=False):
    with open(os.path.join(repo, "INPUT_CL29.txt"), "w") as fh:
        fh.write("# DESCRIPTION Curonian Lagoon 29-box EUTROPY-derived\n")
        for i in range(2, 6):
            fh.write(f"# DESCRIPTION LINE {i}\n")
        fh.write("# BASE_YEAR\n           2012\n")
        fh.write(f"# SIMULATION_START\n{float(tdays[0]):15.1f}\n")
        fh.write(f"# SIMULATION_END\n{float(tdays[-1]):15.1f}\n")
        fh.write("# NUM_REPEATS\n              1\n")
        fh.write("# TIME_STEPS_PER_DAY\n            240\n")
        fh.write(f"# PRINT_INTERVAL IN TIME STEPS\n{240 if enable_sediments else 10:15d}\n")
        fh.write("# PELAGIC MODEL INPUT FOLDER write the folder always with / in the end\n")
        fh.write("INPUTS_CL29/\n")
        fh.write("# PELAGIC MODEL INPUT FILE\n            PELAGIC_INPUTS.txt\n")
        fh.write("# PELAGIC MODEL OUTPUT FOLDER write the folder always with / in the end\n")
        fh.write(f"{OUTPUT_FOLDER}/\n")
        fh.write("# RESUSPENSION_OPTION\n          0\n")
        if enable_sediments:
            fh.write("# MODEL_SEDIMENTS\n          2\n")
            fh.write("# BOTTOM SEDIMENT MODEL INPUT FILE\n")
            fh.write("BOTTOM_SEDIMENT_MODEL_INPUT.txt\n")
        else:
            fh.write("# MODEL_SEDIMENTS\n          0\n")
            fh.write("# NUM_PRESCRIBED_SEDIMENT_FLUX_SETS\n          0\n")
            fh.write("# SEDIMENT MODEL INPUT FILE\n")


if __name__ == "__main__":
    raise SystemExit(main())
