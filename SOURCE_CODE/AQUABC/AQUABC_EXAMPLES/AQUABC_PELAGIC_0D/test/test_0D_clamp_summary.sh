#!/usr/bin/env bash
set -euo pipefail

OUT=/tmp/aquabc_0d_clamp_summary_out.txt
EXE=./aquabc_II_pelagic_0D

if [ ! -x "$EXE" ]; then
  echo "Executable $EXE not found or not executable. Build first: make aquabc0D" >&2
  exit 2
fi

# Run the 0D example and capture output
$EXE INPUT.txt > "$OUT" 2>&1

# Count summary and detailed lines
SUMMARY_COUNT=$(grep -c "WARN SUMMARY" "$OUT" || true)
DETAIL_COUNT=$(grep -c "WARN: Clamped FIX_CYN process" "$OUT" || true)

# Logic:
# - PASS if SUMMARY_COUNT>0 and DETAIL_COUNT==0 (desired compact summary and no detailed lines)
# - PASS if SUMMARY_COUNT==0 and DETAIL_COUNT==0 (no clamp events on this run, acceptable)
# - FAIL otherwise
if [ "$SUMMARY_COUNT" -gt 0 ] && [ "$DETAIL_COUNT" -eq 0 ]; then
  echo "Test PASSED: compact clamp summary present and detailed warns suppressed"
  grep -n "WARN SUMMARY" "$OUT" || true
  exit 0
fi

if [ "$SUMMARY_COUNT" -eq 0 ] && [ "$DETAIL_COUNT" -eq 0 ]; then
  echo "Test PASSED: no clamp events occurred in this run (no detailed warns and no summaries)"
  exit 0
fi

# Otherwise fail
echo "Test FAILED: unexpected warning pattern. SUMMARY_COUNT=$SUMMARY_COUNT DETAIL_COUNT=$DETAIL_COUNT" >&2
if [ "$DETAIL_COUNT" -gt 0 ]; then
  echo "Detailed clamped WARN lines found (should be suppressed)" >&2
  grep -n "WARN: Clamped FIX_CYN process" "$OUT" >&2 || true
fi
if [ "$SUMMARY_COUNT" -gt 0 ]; then
  echo "Summary lines found:" >&2
  grep -n "WARN SUMMARY" "$OUT" >&2 || true
fi
exit 3
