#!/usr/bin/env python3
"""Regenerate the constants tables of PARAMETER_REFERENCE.md from the Standard constants file.

The tables list every constant of INPUTS/WCONST_04.txt (id, name, Standard value, the file's
own comment), grouped by the categories of the app's Parameters tab
(shiny_app/parameter_parser.py). The hand-written sections of the document are kept: only the
text between the two marker lines is replaced.

    python tools/build_parameter_reference.py           rewrite PARAMETER_REFERENCE.md
    python tools/build_parameter_reference.py --check   exit 1 if the file is out of date

Standard library only.
"""
from __future__ import annotations

import argparse
import ast
import os
import re

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BEGIN = "<!-- BEGIN GENERATED CONSTANTS (tools/build_parameter_reference.py) -->"
END = "<!-- END GENERATED CONSTANTS -->"


def read_constants(path):
    """[(id, name, value_text, comment)] in file order."""
    rows = []
    for line in open(path, errors="replace"):
        code, _, comment = line.partition("!")
        parts = code.split()
        if len(parts) >= 3 and parts[0].isdigit():
            comment = re.sub(r"^\s*\d+\s*", "", comment).strip()      # the comment repeats the id
            rows.append((int(parts[0]), parts[1], parts[2], comment))
    return rows


def categories():
    """PARAMETER_CATEGORIES of the app, read from its source (no import: that module needs pandas)."""
    src = open(os.path.join(ROOT, "shiny_app", "parameter_parser.py")).read()
    start = src.index("PARAMETER_CATEGORIES = {")
    return ast.literal_eval(src[src.index("{", start):src.index("}", start) + 1])


def render(rows, cats):
    by_id = {r[0]: r for r in rows}
    out = [BEGIN, "",
           f"All {len(rows)} constants of `INPUTS/WCONST_04.txt` (the Standard setup). Values are the "
           "Standard ones; other setups (e.g. CL29) carry their own file. Descriptions are the "
           "file's own comments.", ""]
    for n, (name, (lo, hi)) in enumerate(cats.items(), 1):
        out += [f"### {n}. {name} ({lo}-{hi}) - {hi - lo + 1} parameters", "",
                "| ID | Name | Standard value | Description |", "|---:|---|---:|---|"]
        for i in range(lo, hi + 1):
            _id, cname, value, comment = by_id[i]
            out.append(f"| {i} | `{cname}` | {value} | {comment.replace('|', '/')} |")
        out.append("")
    out.append(END)
    return "\n".join(out)


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--check", action="store_true", help="do not write; exit 1 if out of date")
    a = p.parse_args(argv)
    doc_path = os.path.join(ROOT, "PARAMETER_REFERENCE.md")
    text = open(doc_path).read()
    if BEGIN not in text or END not in text:
        raise SystemExit(f"marker lines not found in {doc_path}")
    block = render(read_constants(os.path.join(ROOT, "INPUTS", "WCONST_04.txt")), categories())
    new = text[:text.index(BEGIN)] + block + text[text.index(END) + len(END):]
    if a.check:
        if new != text:
            print("PARAMETER_REFERENCE.md is out of date: run python tools/build_parameter_reference.py")
            return 1
        return 0
    if new != text:
        open(doc_path, "w").write(new)
    print(f"PARAMETER_REFERENCE.md: {block.count('| `')} constants in {block.count('### ')} categories")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
