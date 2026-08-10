#!/usr/bin/env python3
"""Which term constrains a phytoplankton group, by month?

Answers two CL29 questions without touching the model: why diatoms do not shut down
in winter, and why the summer diazotroph bloom is ~10x too small. Both groups already
export their growth-limitation factors and rate terms to PROCESS_RATES; this reads them.

A limitation factor near 1.0 is NOT limiting. If every factor is high yet biomass is
low, growth is not the constraint and the loss terms are. If a factor is small, that
term is the constraint and is where a fix belongs.

PROCESS_RATES is state-major with NDIAGVAR (=30, mod_GLOBAL.f90) columns per state
variable, and the file has no header row. **Every group uses a different slot layout**
-- DIA puts TEMP at slot 6, FIX_CYN puts it at 9, NOST at 12 -- so the layouts below are
transcribed per group from aquabc_II_pelagic_model.f90. A shared layout silently
mislabels columns: every limitation factor lies in [0,1], so a range check cannot
detect swapping one for another.
"""
from __future__ import annotations

import argparse
import glob
import os
import re

import pandas as pd

# Slot -> name, transcribed per group from the PROCESS_RATES assignments in
# SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90. `lims` are the LIM_KG_*
# growth-limitation factors (dimensionless, 0-1); `rates` are mass fluxes; `extra`
# is anything else worth printing. Slots not listed are deliberately omitted rather
# than guessed.
LAYOUTS = {
    "DIA": {  # DIA_C_INDEX, model line ~2278
        "index": 5,
        "rates": {1: "growth", 2: "respiration", 3: "excretion", 4: "death", 5: "grazed"},
        "lims": {6: "TEMP", 7: "DOXY", 8: "N", 9: "P", 10: "Si", 11: "LIGHT"},
        "extra": {12: "LIGHT_SAT"},
    },
    "CYN": {  # CYN_C_INDEX -- no silica term, so LIGHT moves up to slot 10
        "index": 15,
        "rates": {1: "growth", 2: "respiration", 3: "excretion", 4: "death", 5: "grazed"},
        "lims": {6: "TEMP", 7: "DOXY", 8: "N", 9: "P", 10: "LIGHT"},
        "extra": {11: "I_A", 12: "LIGHT_SAT", 13: "WATER_TEMP"},
    },
    "OPA": {  # OPA_C_INDEX -- only the light factor is exported
        "index": 16,
        "rates": {1: "growth", 2: "respiration", 3: "excretion", 4: "death", 5: "grazed"},
        "lims": {10: "LIGHT"},
        "extra": {11: "LIGHT_SAT"},
    },
    "FIX_CYN": {  # FIX_CYN_C_INDEX, model lines 2318-2350
        "index": 19,
        "rates": {1: "growth", 2: "respiration", 3: "excretion", 4: "death", 5: "grazed",
                  6: "growth_nonfixing", 7: "growth_fixing", 8: "N2_fixation"},
        # FIX_N is an INVERSE Monod, K_FIX/(K_FIX+DIN): it is a fixation *switch*, not a
        # shortage. 1.0 = fixation fully enabled (DIN scarce); 0.0 = fixation suppressed
        # by abundant ambient DIN. Read it opposite to every other factor here.
        "lims": {9: "TEMP", 10: "DOXY", 11: "FIX_N", 12: "FIX_P",
                 13: "NONFIX_N", 14: "NONFIX_P", 15: "LIGHT"},
        "extra": {16: "N_TO_P", 17: "DIN", 18: "LIGHT_SAT"},
    },
    "NOST": {  # NOST_VEG_HET_C_INDEX == FIX_CYN_HET_C_INDEX == 31, model lines 2516-2556
        "index": 31,
        "rates": {1: "growth", 2: "respiration", 3: "excretion", 4: "death", 5: "grazed",
                  6: "germination", 7: "akinete_formation", 8: "density_mortality",
                  9: "N2_fixation"},
        "lims": {11: "LIGHT", 12: "TEMP", 13: "DOXY", 14: "P", 16: "N"},
        "extra": {10: "LIGHT_SAT", 15: "COMMUNITY_GROWTH"},
    },
}

LOSS_TERMS = ("respiration", "excretion", "death", "grazed")
PRINT_PRECISION = 1e-6   # PROCESS_RATES.out is written at six decimals

WINTER = (1, 2, 3, 4, 5)
BLOOM = (7, 8, 9)


def load_group(path, base_year, layout, stride):
    """Read one PROCESS_RATES file and return this group's named columns."""
    df = pd.read_csv(path, sep=r"\s+", header=None)
    base = pd.Timestamp(f"{base_year}-01-01")
    out = {"date": [base + pd.Timedelta(days=float(d)) for d in df[0]]}
    first = 1 + (layout["index"] - 1) * stride
    for group in ("rates", "lims", "extra"):
        for slot, name in layout[group].items():
            col = first + (slot - 1)
            if col < df.shape[1]:
                out[name] = df[col].astype(float).values
    return pd.DataFrame(out)


def check_offset(df, group):
    """Prove the column offset from the data, not from the range of the values.

    FIX_CYN splits its growth into a fixing and a non-fixing part, so slot 1 must equal
    slot 6 + slot 7 row by row. That identity holds only at the correct offset, which
    makes it a far stronger check than asking whether the factors look like fractions.

    The tolerance is absolute, not relative: the output is written at six decimals, so
    each of the three terms is independently rounded and the sum can legitimately miss
    by 1.5 print units. A relative tolerance would reject the correct offset outright,
    because these rates are themselves ~1e-4.
    """
    if group != "FIX_CYN" or not {"growth", "growth_nonfixing", "growth_fixing"} <= set(df):
        return None
    resid = float((df["growth"] - df["growth_nonfixing"] - df["growth_fixing"]).abs().max())
    tol = 2 * PRINT_PRECISION
    return f"growth == fixing + non-fixing to {resid:.1e} (<= {tol:.0e} rounding) -- offset confirmed" \
        if resid <= tol else \
        f"FAIL: growth != fixing + non-fixing (max residual {resid:.3e}); offset is wrong"


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--outputs", required=True, help="folder containing *_PROCESS_RATES.out")
    p.add_argument("--group", default="DIA", choices=sorted(LAYOUTS))
    p.add_argument("--base-year", type=int, default=2012)
    p.add_argument("--stride", type=int, default=30, help="NDIAGVAR, columns per state variable")
    p.add_argument("--season", default="winter", choices=["winter", "bloom"],
                   help="which season to interrogate: winter (Jan-May) or bloom (Jul-Sep)")
    p.add_argument("--out", default="group_limitation.csv")
    a = p.parse_args()

    layout = LAYOUTS[a.group]
    frames = []
    for path in sorted(glob.glob(os.path.join(a.outputs, "*_PROCESS_RATES.out"))):
        df = load_group(path, a.base_year, layout, a.stride)
        df["box"] = int(re.search(r"PELAGIC_BOX_0*(\d+)_", os.path.basename(path)).group(1))
        frames.append(df)

    if not frames:
        raise SystemExit(f"no *_PROCESS_RATES.out found in {a.outputs}")

    df = pd.concat(frames)
    df["month"] = df["date"].dt.month

    # The [0,1] test applies only to the LIM_KG_* factors; rate and diagnostic slots
    # (LIGHT_SAT, DIN, N:P) are unbounded and would trip it spuriously.
    lim_cols = [c for c in layout["lims"].values() if c in df.columns]
    bad = [c for c in lim_cols if not df[c].between(-1e-9, 1.0001).all()]
    if bad:
        raise SystemExit(f"limitation columns {bad} fall outside [0,1]: the layout or stride "
                         f"is wrong (used stride {a.stride})")

    verdict = check_offset(df, a.group)
    if verdict:
        print(verdict)
        if verdict.startswith("FAIL"):
            raise SystemExit(1)

    focus = WINTER if a.season == "winter" else BLOOM
    other = BLOOM if a.season == "winter" else WINTER

    print(f"\n=== {a.group} (state variable {layout['index']}), "
          f"limitation factors by month (1.0 = NOT limiting)")
    print(df.groupby("month")[lim_cols].mean().round(3).to_string())

    print(f"\n{a.season} {focus} vs {other}:")
    f_, o_ = df[df.month.isin(focus)][lim_cols].mean(), df[df.month.isin(other)][lim_cols].mean()
    print(f"{'factor':<10}{a.season:>10}{'other':>9}   verdict")
    for c in lim_cols:
        verdict = ("NOT limiting" if f_[c] > 0.66
                   else "weakly limiting" if f_[c] > 0.33 else "STRONGLY limiting")
        if c == "FIX_N":   # inverse switch: high means fixation is enabled, not starved
            verdict = ("fixation ENABLED" if f_[c] > 0.66
                       else "fixation partly suppressed" if f_[c] > 0.33
                       else "fixation SUPPRESSED by ambient DIN")
        print(f"{c:<10}{f_[c]:>10.3f}{o_[c]:>9.3f}   {verdict}")

    rate_cols = [c for c in layout["rates"].values() if c in df.columns]
    if rate_cols:
        print(f"\n{a.group} rate terms by month:")
        print(df.groupby("month")[rate_cols].mean().round(5).to_string())
        r = df[df.month.isin(focus)][rate_cols].mean()
        loss = sum(r.get(c, 0.0) for c in LOSS_TERMS)
        g = r.get("growth", float("nan"))
        print(f"\n{a.season}: growth {g:.5f} vs total losses {loss:.5f} -> "
              f"{'growth-dominated' if g > loss else 'loss-dominated'}")
        for c in LOSS_TERMS:
            if c in r and loss:
                print(f"    {c:<12}{r[c]:>10.5f}  ({100 * r[c] / loss:.0f}% of losses)")

    extra_cols = [c for c in layout["extra"].values() if c in df.columns]
    if extra_cols:
        print(f"\n{a.group} diagnostics by month:")
        print(df.groupby("month")[extra_cols].mean().round(4).to_string())

    df.to_csv(a.out, index=False)
    print(f"\nwrote {a.out}")


if __name__ == "__main__":
    main()
