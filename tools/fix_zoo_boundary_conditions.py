#!/usr/bin/env python3
"""Fix ZOO_N and ZOO_P boundary conditions in all FORC_TS files.

Sets ZOO_N = ZOO_C * 0.22 (ZOO_N_TO_C from WCONST_04.txt)
Sets ZOO_P = ZOO_C * 0.024 (ZOO_P_TO_C from WCONST_04.txt)

FORC_TS_1.txt: Baltic boundary (30 vars, col 7=ZOO_C, col 8=ZOO_N, col 9=ZOO_P)
FORC_TS_2-5.txt: River boundaries (33 vars with 3 extra bacteria, col 10=ZOO_C, col 11=ZOO_N, col 12=ZOO_P)
FORC_TS_9.txt: Solar radiation only (no ZOO columns, skip)
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
INPUTS = ROOT / "INPUTS"

ZOO_N_TO_C = 0.22
ZOO_P_TO_C = 0.024


def fix_forc_file(filepath: Path, zoo_c_col: int, zoo_n_col: int, zoo_p_col: int):
    """Fix ZOO_N and ZOO_P in a FORC_TS file.
    
    Column indices are 0-based for the data fields (after TIME column).
    In the actual file, col 1 = TIME, so data col 0 = file col 2, etc.
    We use 1-based field indices as seen by awk.
    """
    lines = filepath.read_text().splitlines()
    modified = 0
    new_lines = []
    
    for line in lines:
        # Skip comment lines and header lines
        if line.strip().startswith('#') or not line.strip():
            new_lines.append(line)
            continue
        
        fields = line.split()
        
        # Skip lines that don't have enough fields (headers, size lines, etc.)
        if len(fields) < max(zoo_c_col, zoo_n_col, zoo_p_col) + 1:
            new_lines.append(line)
            continue
        
        # Check if this looks like a data line (first field should be numeric TIME > 100)
        # This distinguishes actual data lines from scale factor lines (all 1.0)
        try:
            time_val = float(fields[0])
        except ValueError:
            new_lines.append(line)
            continue
        
        # Only modify actual data lines: TIME values are Julian dates > 100
        # Scale factor lines have values like 1.0 but no time > 100
        if time_val < 100.0:
            new_lines.append(line)
            continue
        
        # Also check that ZOO_C field is numeric
        try:
            zoo_c = float(fields[zoo_c_col])
        except ValueError:
            new_lines.append(line)
            continue
        
        # Compute correct ZOO_N and ZOO_P
        new_zoo_n = zoo_c * ZOO_N_TO_C
        new_zoo_p = zoo_c * ZOO_P_TO_C
        
        old_zoo_n = float(fields[zoo_n_col])
        old_zoo_p = float(fields[zoo_p_col])
        
        if abs(new_zoo_n - old_zoo_n) > 1e-12 or abs(new_zoo_p - old_zoo_p) > 1e-12:
            # Replace the values, preserving field width
            old_n_str = fields[zoo_n_col]
            old_p_str = fields[zoo_p_col]
            new_n_str = f"{new_zoo_n:.6f}"
            new_p_str = f"{new_zoo_p:.6f}"
            
            # Pad to match original width
            new_n_str = new_n_str.rjust(max(len(old_n_str), len(new_n_str)))
            new_p_str = new_p_str.rjust(max(len(old_p_str), len(new_p_str)))
            
            fields[zoo_n_col] = new_n_str
            fields[zoo_p_col] = new_p_str
            
            # Reconstruct line with consistent spacing
            new_line = "        ".join(fields)
            new_lines.append(new_line)
            modified += 1
        else:
            new_lines.append(line)
    
    filepath.write_text("\n".join(new_lines) + "\n")
    return modified


def main():
    total = 0
    
    # FORC_TS_1: Baltic boundary
    # 30 vars, columns: TIME(1) NH4(2) NO3(3) PO4(4) DO(5) DIA(6) ZOO_C(7) ZOO_N(8) ZOO_P(9) ...
    # 0-based field indices: ZOO_C=6, ZOO_N=7, ZOO_P=8
    f1 = INPUTS / "FORC_TS_1.txt"
    if f1.exists():
        n = fix_forc_file(f1, zoo_c_col=6, zoo_n_col=7, zoo_p_col=8)
        print(f"  FORC_TS_1.txt: {n} data lines modified")
        total += n
    
    # FORC_TS_2-5: River boundaries
    # 33 vars with 3 extra bacteria columns after DO:
    # TIME(1) NH4(2) NO3(3) PO4(4) DO(5) NITR_BAC(6) AER_HET_BAC(7) DENITR_BAC(8) DIA(9) ZOO_C(10) ZOO_N(11) ZOO_P(12) ...
    # 0-based: ZOO_C=9, ZOO_N=10, ZOO_P=11
    for i in range(2, 6):
        f = INPUTS / f"FORC_TS_{i}.txt"
        if f.exists():
            n = fix_forc_file(f, zoo_c_col=9, zoo_n_col=10, zoo_p_col=11)
            print(f"  FORC_TS_{i}.txt: {n} data lines modified")
            total += n
    
    # FORC_TS_9: Solar radiation only, no ZOO columns
    print(f"  FORC_TS_9.txt: skipped (solar radiation only)")
    
    print(f"\nTotal data lines modified: {total}")


if __name__ == "__main__":
    main()
