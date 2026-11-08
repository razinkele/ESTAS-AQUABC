"""Point each box's per-box forcing sections at its own column of the forcing series.

    set_per_box_forcing_columns.py INPUT_DIR            # report only; exit 1 if any section needs a change
    set_per_box_forcing_columns.py INPUT_DIR --write    # rewrite PELAGIC_INPUTS.txt in place

Why: the SHYFEM-coupled input sets (INPUTS_SHYHYDRO*, INPUTS_1993*) were made by copying INPUTS_CL29's
PELAGIC_INPUTS.txt, whose meteorology files have one column, so every box points at var 1. The SHYFEM converter
writes 29-column files (one per box), so every box then reads box 1's rain, evaporation, air temperature, wind and
ice (CL29 note items 70-75, docs §65-§66). For each section below, a box whose forcing file has at least as many
columns as there are boxes is pointed at var = box number; a single-column file is left on var 1; any other width
is an error.
"""
import argparse
import os
import sys

SECTIONS = ("PRECIPITATION", "EVAPORATION", "AIR TEMPERATURE", "WIND SPEED", "ICE FRACTION")


def _section_name(line):
    return line.strip("# *").strip() if line.startswith("# ****") else None


def forcing_files(lines):
    """FORCING TIME SERIES table: series number -> file name."""
    out, inside = {}, False
    for ln in lines:
        name = _section_name(ln)
        if name is not None:
            inside = name == "FORCING TIME SERIES"
            continue
        f = ln.split()
        if inside:
            if len(f) == 2 and f[0].isdigit():
                out[int(f[0])] = f[1]
            elif ln.startswith("#") and out:
                break
    return out


def n_columns(path):
    lines = open(path).read().split("\n")
    i = next(k for k, ln in enumerate(lines) if ln.startswith("# NUMBER_OF_VARIABLES"))
    return int(lines[i + 1].split()[0])


def remap(input_dir):
    """Return (new_lines, changes): changes = [(section, box, old_var, new_var)]."""
    p = os.path.join(input_dir, "PELAGIC_INPUTS.txt")
    lines = open(p).read().split("\n")
    files = forcing_files(lines)
    width = {}
    rows = {s: [] for s in SECTIONS}
    cur = None
    for i, ln in enumerate(lines):
        name = _section_name(ln)
        if name is not None:
            cur = name if name in SECTIONS else None
            continue
        f = ln.split()
        if cur and len(f) == 3 and not ln.startswith("#") and all(x.isdigit() for x in f):
            rows[cur].append((i, int(f[0]), int(f[1]), int(f[2])))
    changes = []
    for s in SECTIONS:
        nbox = len(rows[s])
        if nbox == 0:
            raise SystemExit(f"section {s!r} not found in {p}")
        for i, box, ts, var in rows[s]:
            if ts not in width:
                width[ts] = n_columns(os.path.join(input_dir, files[ts]))
            w = width[ts]
            if w == 1:
                want = 1
            elif w >= nbox:
                want = box
            else:
                raise SystemExit(f"{s}: {files[ts]} has {w} columns for {nbox} boxes")
            if want != var:
                changes.append((s, box, var, want))
                lines[i] = f"{box:20d}{ts:20d}{want:20d}"
    return lines, changes


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("input_dir")
    ap.add_argument("--write", action="store_true", help="rewrite PELAGIC_INPUTS.txt in place")
    a = ap.parse_args(argv)
    lines, changes = remap(a.input_dir)
    by = {}
    for s, *_ in changes:
        by[s] = by.get(s, 0) + 1
    for s in SECTIONS:
        print(f"  {s:16s} {by.get(s, 0):3d} box rows to change")
    if a.write and changes:
        open(os.path.join(a.input_dir, "PELAGIC_INPUTS.txt"), "w").write("\n".join(lines))
        print(f"  wrote {len(changes)} rows")
        return 0
    return 1 if changes else 0


if __name__ == "__main__":
    sys.exit(main())
