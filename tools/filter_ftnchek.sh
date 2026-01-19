#!/usr/bin/env bash
set -euo pipefail
REPORT=tools/ftnchek_report.txt
UNEXPECTED=tools/ftnchek_unexpected.txt
IGNORE=tools/ftnchek_ignore_patterns.txt

# Extract lines with 'Error near line' and filter out known false positives
REPORT_FILE="tools/ftnchek_report.txt"
OUTPUT_FILE="tools/ftnchek_unexpected.txt"
IGNORE_FILE="tools/ftnchek_ignore_patterns.txt"

# Build ignore regex from patterns file (skip comments and blank lines)
IGNORE_REGEX=$(grep -vE '^\s*#|^\s*$' "$IGNORE_FILE" | sed -E 's@([\.\^\$\*\+\?\(\)\[\{\\\|])@\\\1@g' | tr '\n' '|' | sed -E 's/\|$//')

# Extract relevant lines
grep -n "Error near line" "$REPORT_FILE" > "$OUTPUT_FILE".raw || true

if [ -n "$IGNORE_REGEX" ]; then
  # filter out ignore patterns
  grep -E -v "$IGNORE_REGEX" "$OUTPUT_FILE".raw > "$OUTPUT_FILE" || true
else
  mv "$OUTPUT_FILE".raw "$OUTPUT_FILE"
fi
rm -f "$OUTPUT_FILE".raw || true

# Summary
LINE_COUNT=$(wc -l < "$OUTPUT_FILE" | tr -d ' ')
echo "Unexpected ftnchek lines: $LINE_COUNT"
if [ "$LINE_COUNT" -gt 0 ]; then
    echo "Saved unexpected lines to $OUTPUT_FILE"
    echo
    head -n 50 "$OUTPUT_FILE" || true
else
    echo "(none)"
fi
