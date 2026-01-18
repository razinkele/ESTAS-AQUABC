#!/usr/bin/env python3
import re
from pathlib import Path

repo = Path('.')
const_out = repo / 'const_out.txt'
backup = repo / 'data' / 'const_CL.txt.bak'
output = repo / 'data' / 'const_CL.txt.formatted'

# Read comments from backup
comments = {}
if backup.exists():
    for line in backup.read_text().splitlines():
        if '!' in line:
            left, right = line.split('!', 1)
            tokens = left.split()
            if len(tokens) >= 2:
                name = tokens[1]
                comments[name] = ' '.join(right.strip().split())

# Parse const_out.txt
entries = []
pat = re.compile(r'^\s*(\d+)\s+(\S+)\s+([0-9EDed+\-.]+)')
for line in const_out.read_text().splitlines():
    m = pat.match(line)
    if m:
        idx = int(m.group(1))
        name = m.group(2)
        val_str = m.group(3).replace('D','E').replace('d','E')
        try:
            val = float(val_str)
        except Exception:
            val = val_str
        entries.append((idx, name, val))

# Write formatted file
with output.open('w') as f:
    f.write("! Formatted constants file generated from const_out.txt and original comments\n")
    f.write("! Backup of previous file is data/const_CL.txt.bak\n")
    f.write('\n')
    for idx, name, val in entries:
        if isinstance(val, float):
            val_s = f"{val:14.6g}"
        else:
            val_s = str(val)
        comment = comments.get(name, '')
        if comment:
            f.write(f"{idx:4d} {name:30s} {val_s:>15s}  ! {comment}\n")
        else:
            f.write(f"{idx:4d} {name:30s} {val_s:>15s}\n")

print('Wrote', output)
