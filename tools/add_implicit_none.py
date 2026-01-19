#!/usr/bin/env python3
"""Small utility to add `implicit none` to subroutine/function bodies missing it.

Reads `missing_implicit_none.txt` (repo root) for a list of files and subroutines
and inserts `implicit none` into the first non-comment line after the subroutine
or function declaration and any 'use' statements.
"""
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
MISSING = ROOT / "missing_implicit_none.txt"

def parse_missing_list(path):
    files = []
    for ln in path.read_text().splitlines():
        ln = ln.strip()
        if not ln or ln.startswith('```'):
            continue
        parts = ln.split(':')
        if parts:
            files.append(parts[0])
    # unique
    return sorted(set(files))

SUB_DECL_RE = re.compile(r"^(\s*)(subroutine|function)\b(.*)$", re.IGNORECASE)
USE_RE = re.compile(r"^\s*use\b", re.IGNORECASE)
IMPLICIT_RE = re.compile(r"^\s*implicit\s+none\b", re.IGNORECASE)


def add_implicit_to_file(file_path: Path) -> int:
    text = file_path.read_text()
    lines = text.splitlines()
    changed = 0
    i = 0
    while i < len(lines):
        m = SUB_DECL_RE.match(lines[i])
        if m:
            # scan forward a few lines for use/implicit/comment
            j = i + 1
            # skip comments and blank lines
            while j < len(lines) and (lines[j].strip().startswith('!') or lines[j].strip() == ''):
                j += 1
            # gather contiguous use lines
            k = j
            while k < len(lines) and USE_RE.match(lines[k]):
                k += 1
            # check if implicit none already present in a small window
            found = False
            for x in range(j, min(k+3, len(lines))):
                if IMPLICIT_RE.match(lines[x]):
                    found = True
                    break
            if not found:
                insert_at = k
                # insert '        implicit none' with indentation similar to next line or just four spaces
                indent = ' ' * 8
                lines.insert(insert_at, indent + 'implicit none')
                changed += 1
                i = insert_at + 1
            else:
                i = k
        else:
            i += 1
    if changed:
        file_path.write_text('\n'.join(lines) + '\n')
    return changed


def main():
    files = parse_missing_list(MISSING)
    total = 0
    for f in files:
        p = ROOT / f
        if not p.exists():
            print(f"Warning: {p} not found")
            continue
        c = add_implicit_to_file(p)
        if c:
            print(f"Patched {p}: inserted {c} implicit none")
            total += c
    print(f"Done. Inserted {total} implicit none statements.")

if __name__ == '__main__':
    main()
