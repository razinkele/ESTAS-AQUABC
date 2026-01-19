#!/usr/bin/env bash
set -euo pipefail
# Merge lines where identifiers were split across line breaks.
# Heuristic: if a line is followed by a line that contains only an identifier fragment ([A-Za-z0-9_]+),
# and the current line ends with an identifier fragment (letters, digits, underscore) or ':: <fragment>',
# join them without adding extra spaces.

if [ "$#" -lt 1 ]; then
  echo "Usage: $0 <path-to-f90-file>"
  exit 1
fi
file="$1"

tmp=$(mktemp)
awk '
  {
    lines[NR] = $0;
  }
  END {
    i=1;
    while (i<=NR) {
      line = lines[i];
      if (i<NR) {
         nextline = lines[i+1];
         # trim nextline
         gsub(/^[ \t]+|[ \t]+$/,"",nextline);
         if (nextline ~ /^[A-Za-z0-9_]+$/) {
           # trim end of current line
           t=line; gsub(/[ \t]+$/,"",t);
           if (t ~ /::$/ || t ~ /::\s*$/) {
             # previous line ends with ::, join with a space
             line = t " " nextline;
             i += 2; print line; continue;
           } else if (t ~ /[A-Za-z0-9_]$/) {
             # previous line ends with identifier fragment, join without space
             line = t nextline;
             i += 2; print line; continue;
           }
         }
      }
      print line;
      i++;
    }
  }
' "$file" > "$tmp"

mv "$tmp" "$file"
chmod 644 "$file"

echo "Processed $file"