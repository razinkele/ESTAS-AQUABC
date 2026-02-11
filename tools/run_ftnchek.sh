#!/usr/bin/env bash
set -euo pipefail

# Run ftnchek over all .f90 files and produce a report
REPORT=tools/ftnchek_report.txt
mkdir -p tools
find SOURCE_CODE -name "*.f90" -print0 | xargs -0 ftnchek -usage -truncation -f90 -portability -pretty -columns=240 > "$REPORT" 2>&1 || true

echo "ftnchek report saved to $REPORT"
echo "Errors:"
grep -n "Error near line" "$REPORT" | head -n 50 || true

echo "Done."