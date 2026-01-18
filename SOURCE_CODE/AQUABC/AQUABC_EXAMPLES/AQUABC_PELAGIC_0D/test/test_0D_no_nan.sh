#!/usr/bin/env bash
set -euo pipefail

OUT=/tmp/aquabc_0d_test_out.txt
EXE=./aquabc_II_pelagic_0D

if [ ! -x "$EXE" ]; then
  echo "Executable $EXE not found or not executable. Build first: make aquabc0D" >&2
  exit 2
fi

# Run the 0D example and capture output
$EXE INPUT.txt > "$OUT" 2>&1

# Fail if any NaN or CHLA negative flag appears
if grep -n -E "NaN|CHLA_NEG" "$OUT" >/dev/null; then
  echo "Test FAILED: Found NaN or CHLA_NEG in output" >&2
  grep -n -E "NaN|CHLA_NEG" "$OUT" >&2 || true
  exit 3
fi

# Check CHLA numeric values (if present) are non-negative
# Extract CHLA(...) = value patterns and test values
CHLA_LINES=$(grep -n "CHLA(" "$OUT" || true)
if [ -n "$CHLA_LINES" ]; then
  echo "$CHLA_LINES" | awk -F"=" '{print $2}' | while read -r val; do
    # Skip if not a number
    case "$val" in
      *[^0-9Ee+\-\.]* ) continue ;;
    esac
    awk "BEGIN{if ($val < 0) exit 1; else exit 0}" || { echo "Test FAILED: negative CHLA value detected: $val" >&2; exit 4; }
  done
fi

# If all checks passed
echo "Test PASSED: no NaNs and CHLA non-negative" 
exit 0
