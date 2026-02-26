#!/usr/bin/env python3
"""Update initial conditions from R10 MTRX output.

Uses average of sand boxes (5, 6, 8, 9) for IC Set 2 and
average of mud boxes (14, 17, 25) for IC Set 1.

Critically fixes ZOO_N and ZOO_P to stoichiometric values since
the R10 MTRX has ZOO_N=ZOO_P=0 (collapsed from boundary bug).
"""
from pathlib import Path
import numpy as np

ROOT = Path(__file__).resolve().parent.parent
INPUTS = ROOT / "INPUTS"
OUTPUTS = ROOT / "OUTPUTS"

ZOO_N_TO_C = 0.22
ZOO_P_TO_C = 0.024

# Variable names in MTRX order (matches IC file variable order 1-36)
VAR_NAMES = [
    "NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN",
    "DIA_C", "ZOO_C", "ZOO_N", "ZOO_P",
    "DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P",
    "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P",
    "CYN_C", "OPA_C", "DISS_Si", "PART_Si", "FIX_CYN_C",
    "INORG_C", "TOT_ALK",
    "FE_II", "FE_III", "MN_II", "MN_IV",
    "CA", "MG", "S_PLUS_6", "S_MINUS_2", "CH4_C",
    "NOST_VEG_HET_C", "AKI_C",
    "SEC_METAB_DIA", "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST",
]

VAR_COMMENTS = [
    "AMMONIUM NITROGEN", "NITRATE NITROGEN", "ORTHOPHOSPHATE PHOSPHORUS",
    "DISSOLVED OXYGEN", "DIATOMS CARBON", "ZOOPLANKTON CARBON",
    "ZOOPLANKTON NITROGEN", "ZOOPLANKTON PHOSPHORUS",
    "DETRITUS PARTICULATE ORG. CARBON", "DETRITUS PARTICULATE ORG. NITROGEN",
    "DETRITUS PARTICULATE ORG. PHOSPHORUS",
    "DISSOLVED ORGANIC CARBON", "DISSOLVED ORGANIC NITROGEN",
    "DISSOLVED ORGANIC PHOSPHORUS",
    "NON FIXING CYANOBACTERIA CARBON", "OTHER PHYTOPLANKTON CARBON",
    "DISSOLVED SILICA", "PARTICULATE SILICA", "FIXING CYANOBACTERIA CARBON",
    "INORG CARBON", "TOTAL ALKALINITY",
    "IRON (Fe2+)", "IRON (Fe3+)", "MANGANESE (Mn2+)", "MANGANESE (Mn4+)",
    "CALCIUM", "MAGNESIUM", "SULPHATE SULPHUR (S6+)",
    "SULPHIDE SULPHUR (S2-)", "METHANE CARBON",
    "NOSTOCALES", "AKINETES",
    "SEC_METAB_DIA", "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST",
]


def read_mtrx(box_id: int) -> np.ndarray:
    """Read MTRX file, return array of 36 state variable values."""
    fname = OUTPUTS / f"PELAGIC_BOX_{box_id:05d}.mtrx"
    with open(fname) as f:
        vals = f.read().split()
    # First value is TIME, skip it
    return np.array([float(v) for v in vals[1:37]])


def write_ic(filepath: Path, values: np.ndarray, set_name: str, aki_depth: float = 15.0):
    """Write an initial conditions file."""
    lines = []
    lines.append(f"# PELAGIC INITIAL CONCENTRATION DATA SET: {set_name}")
    lines.append(f"# Updated from 3560-day (10-repeat) simulation R10 final state")
    lines.append(f"# ZOO_N/P set stoichiometrically: ZOO_N=ZOO_C*0.22, ZOO_P=ZOO_C*0.024")
    lines.append(f"#     PELAGIC STATE VAR. NO       PELAGIC CONCENTRATION")

    for i, (val, comment) in enumerate(zip(values, VAR_COMMENTS)):
        var_no = i + 1
        if var_no == 32:  # AKI_C has special format with depth
            lines.append(f"                         {var_no:2d}          {val:<14.6f}! {comment}    {aki_depth}")
        elif var_no <= 32:
            lines.append(f"                         {var_no:2d}          {val:<14.6f}! {comment}")
        else:
            # Secondary metabolites - simple format
            lines.append(f"                         {var_no:2d}          {val:.1f}")

    filepath.write_text("\n".join(lines) + "\n")


def main():
    # Read all MTRX files
    sand_boxes = [5, 6, 8, 9]
    mud_boxes = [14, 17, 25]

    sand_vals = np.stack([read_mtrx(b) for b in sand_boxes])
    mud_vals = np.stack([read_mtrx(b) for b in mud_boxes])

    sand_avg = np.mean(sand_vals, axis=0)
    mud_avg = np.mean(mud_vals, axis=0)

    # Fix ZOO_N and ZOO_P stoichiometrically
    # Index 6 = ZOO_N (0-based), Index 7 = ZOO_P, Index 5 = ZOO_C
    for arr in [sand_avg, mud_avg]:
        zoo_c = arr[5]
        arr[6] = zoo_c * ZOO_N_TO_C  # ZOO_N
        arr[7] = zoo_c * ZOO_P_TO_C  # ZOO_P

    print("Sand-box average ICs (from boxes 5, 6, 8, 9):")
    for i, (name, val) in enumerate(zip(VAR_NAMES, sand_avg)):
        flag = " *FIXED*" if name in ("ZOO_N", "ZOO_P") else ""
        print(f"  {i+1:2d}. {name:25s} = {val:.6f}{flag}")

    print(f"\nMud-box average ICs (from boxes 14, 17, 25):")
    for i, (name, val) in enumerate(zip(VAR_NAMES, mud_avg)):
        flag = " *FIXED*" if name in ("ZOO_N", "ZOO_P") else ""
        print(f"  {i+1:2d}. {name:25s} = {val:.6f}{flag}")

    # Write IC files
    # IC Set 1 = Mud, IC Set 2 = Sand (matching existing convention)
    write_ic(INPUTS / "INIT_CONC_1.txt", mud_avg, "Mud (from R10 MTRX, boxes 14,17,25)")
    write_ic(INPUTS / "INIT_CONC_2.txt", sand_avg, "Sand (from R10 MTRX, boxes 5,6,8,9)")

    print(f"\nWritten: INPUTS/INIT_CONC_1.txt (Mud)")
    print(f"Written: INPUTS/INIT_CONC_2.txt (Sand)")


if __name__ == "__main__":
    main()
