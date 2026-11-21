#!/usr/bin/env bash
# A run that aborts must exit with a non-zero status (review finding EST-1, 2026-11-21).
# Before the fix every runtime abort ended with a plain STOP, exit status 0, so the app reported
# success and calibrate_cl29.py scored the truncated record. Run from the repo root, post-build:
#   ESTAS_II built + INPUT.txt + INPUTS/ present.
#
# Doctors a TEMP copy of the standard setup (never a tracked file): nitrate in initial-condition
# set 2 is raised to 2e10 g/m3, so the solver's unrealistic-value guard (> 1e10) fires on the first
# step. The guard writes EXERGY.out, COST_TOTAL and PELAGIC_MODEL_CONSTANTS.txt into the current
# directory, so the run happens inside the temp copy.
set -uo pipefail
cd "$(dirname "$0")/../.." || exit 1
BIN="$PWD/ESTAS_II"
[ -x "$BIN" ] || { echo "abort_exit_status: ESTAS_II not built"; exit 1; }

d=$(mktemp -d) || { echo "FAIL (mktemp failed)"; exit 1; }
[ -n "$d" ] && [ -d "$d" ] || { echo "FAIL (no temp dir)"; exit 1; }
cp INPUT.txt "$d/"; cp -r INPUTS "$d/INPUTS"; mkdir -p "$d/OUTPUTS"
awk '!/^#/ && $1 == "2" { $2 = "2.0E10" } { print }' INPUTS/INIT_CONC_2.txt > "$d/INPUTS/INIT_CONC_2.txt"
if cmp -s INPUTS/INIT_CONC_2.txt "$d/INPUTS/INIT_CONC_2.txt"; then
    echo "FAIL [unrealistic value -> non-zero exit]  (could not doctor INIT_CONC_2.txt)"; rm -rf "$d"; exit 1
fi

( cd "$d" && ESTAS_HOLD_VOLUME=1 timeout 90 "$BIN" INPUT.txt ) >"$d/out.log" 2>"$d/err.log"
rc=$?
fail=0
if [ "$rc" -ne 0 ] && [ "$rc" -ne 124 ] && grep -q 'UNREALISTIC VALUE DETECTED' "$d/out.log"; then
    echo "PASS [unrealistic value -> non-zero exit]  (exit=$rc)"
else
    echo "FAIL [unrealistic value -> non-zero exit]  (exit=$rc, expected non-zero + the guard's message)"
    tail -n 20 "$d/out.log" | sed 's/^/    /'; sed 's/^/    /' "$d/err.log"; fail=1
fi
rm -rf "$d"
exit "$fail"
