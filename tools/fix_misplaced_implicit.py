#!/usr/bin/env python3
"""Fix misplaced 'implicit none' lines inserted between subroutine line and parameter list.

Find patterns like:
  subroutine NAME &
  implicit none
  (arg1, &
    ...)
and remove that 'implicit none' so the file has:
  subroutine NAME &
  (arg1, &
    ...)
  implicit none

"""
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

SUB_LINE_RE = re.compile(r"^\s*subroutine\b.*&\s*$", re.IGNORECASE)
IMPLICIT_LINE_RE = re.compile(r"^\s*implicit\s+none\b\s*$", re.IGNORECASE)
PARAM_START_RE = re.compile(r"^\s*\(.*$")

changed_files = []
for p in ROOT.rglob('SOURCE_CODE/**/*.f90'):
    try:
        text = p.read_text(encoding='utf-8')
    except Exception:
        text = p.read_text(encoding='latin-1')
    lines = text.splitlines()
    changed = False
    i = 0
    new_lines = []
    while i < len(lines):
        new_lines.append(lines[i])
        if SUB_LINE_RE.match(lines[i]):
            # peek ahead for 'implicit none' followed by param start
            j = i + 1
            # skip comments or blanks
            while j < len(lines) and (lines[j].strip().startswith('!') or lines[j].strip() == ''):
                j += 1
            if j < len(lines) and IMPLICIT_LINE_RE.match(lines[j]):
                # check next non-comment line
                k = j + 1
                while k < len(lines) and (lines[k].strip().startswith('!') or lines[k].strip() == ''):
                    k += 1
                if k < len(lines) and lines[k].lstrip().startswith('('):
                    # skip the implicit none (i.e., don't add lines[j])
                    changed = True
                    i = i + 1
                    # consume the implicit none by advancing i (it will be skipped in copy)
                    # continue outer loop
        i += 1
    if changed:
        # rebuild by filtering the pattern
        out_lines = []
        i = 0
        while i < len(lines):
            if SUB_LINE_RE.match(lines[i]):
                out_lines.append(lines[i])
                j = i + 1
                # copy comments/blanks
                while j < len(lines) and (lines[j].strip().startswith('!') or lines[j].strip() == ''):
                    out_lines.append(lines[j])
                    j += 1
                if j < len(lines) and IMPLICIT_LINE_RE.match(lines[j]):
                    k = j + 1
                    while k < len(lines) and (lines[k].strip().startswith('!') or lines[k].strip() == ''):
                        k += 1
                    if k < len(lines) and lines[k].lstrip().startswith('('):
                        # skip the implicit none at j
                        i = j + 1
                        continue
            out_lines.append(lines[i])
            i += 1
        p.write_text('\n'.join(out_lines) + '\n')
        changed_files.append(str(p))

if changed_files:
    print('Fixed misplaced implicit none in files:')
    for f in changed_files:
        print('  -', f)
else:
    print('No misplaced implicit none patterns found.')
