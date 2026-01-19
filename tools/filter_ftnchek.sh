#!/usr/bin/env bash
set -euo pipefail
REPORT=tools/ftnchek_report.txt
UNEXPECTED=tools/ftnchek_unexpected.txt
IGNORE=tools/ftnchek_ignore_patterns.txt

# Parse the report and filter error groups by message
# Each error group starts with a line 'Error near line ... file ...:' followed by one or more message lines.
REPORT_FILE="tools/ftnchek_report.txt"
OUTPUT_FILE="tools/ftnchek_unexpected.txt"
IGNORE_FILE="tools/ftnchek_ignore_patterns.txt"

# Build ignore patterns array from patterns file (skip comments and blank lines)
# We'll read patterns into awk and do literal substring matches (index) to avoid ERE escaping issues
if [ ! -f "$REPORT_FILE" ]; then
    echo "Report not found: $REPORT_FILE"
    exit 1
fi

awk -v IGNORE_FILE="$IGNORE_FILE" '
  function trim(s){gsub(/^\s+|\s+$/,"",s); return s}
  BEGIN {
    np = 0;
    while ((getline p < IGNORE_FILE) > 0) {
      if (p ~ /^[[:space:]]*#/ || p ~ /^[[:space:]]*$/) { }
      else pats[++np] = p;
    }
    close(IGNORE_FILE);
  }
  /^Error near line [0-9]+/ {
    header = $0;
    msg = "";
    # Fetch the next up-to-three lines which may contain the message
    if (getline tmp1 > 0) msg = tmp1;
    if (getline tmp2 > 0) {
      if (tmp2 ~ /^Error near line [0-9]+/) { saved = tmp2; }
      else msg = msg "\n" tmp2;
    }
    if (saved) { # push saved back into awk input
      $0 = saved; saved = ""; # next iteration will see it
    }
    combined = header "\n" msg;
    # Normalize whitespace and newlines to catch patterns that may be wrapped
    normalized = combined;
    gsub(/\n/ , " ", normalized);
    gsub(/[[:space:]]+/, " ", normalized);
    skip = 0;
    for (i = 1; i <= np; i++) {
      if (index(normalized, pats[i]) > 0) { skip = 1; break }
    }
    if (!skip) {
      print header;
      print msg;
      print "";
    }
  }
' "$REPORT_FILE" > "$OUTPUT_FILE" || true

LINE_COUNT=$(wc -l < "$OUTPUT_FILE" | tr -d ' ')
echo "Unexpected ftnchek lines: $LINE_COUNT"
if [ "$LINE_COUNT" -gt 0 ]; then
    echo "Saved unexpected lines to $OUTPUT_FILE"
    echo
    sed -n '1,200p' "$OUTPUT_FILE" || true
else
    echo "No unexpected ftnchek lines found. Clean." 
fi
