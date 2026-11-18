#!/usr/bin/env bash
# Positive test for the fail-loud constants reader (READ_MODEL_CONSTANTS, mod_UTILS_01.f90).
# The tests/fortran harness treats a nonzero exit as failure, so this abort test lives here and is
# run by the CI build-and-run job (which has a built ESTAS_II). Run from the repo root, post-build:
#   ESTAS_II built + INPUT.txt + INPUTS/ present.
#
# Doctors a TEMP copy of the standard setup's WCONST_04.txt (never a tracked file) and asserts the
# reader aborts (nonzero exit) naming the offending index, and that AQUABC_LENIENT_CONSTANTS=1 warns
# but does not abort the reader.
set -uo pipefail
cd "$(dirname "$0")/../.." || exit 1
BIN="$PWD/ESTAS_II"
[ -x "$BIN" ] || { echo "fail_loud_constants: ESTAS_II not built"; exit 1; }
fail=0

# $1 desc  $2 awk-doctor-program  $3 extra-env  $4 mode(FAIL|LENIENT)  $5 stderr-token
run_case() {
    local d; d=$(mktemp -d)
    cp INPUT.txt "$d/"; cp -r INPUTS "$d/INPUTS"
    awk "$2" INPUTS/WCONST_04.txt > "$d/INPUTS/WCONST_04.txt"
    local rc
    ( cd "$d" && env $3 ESTAS_HOLD_VOLUME=1 timeout 90 "$BIN" INPUT.txt ) >/dev/null 2>"$d/err.log"
    rc=$?
    if [ "$4" = FAIL ]; then
        if [ "$rc" -ne 0 ] && grep -qi "$5" "$d/err.log" \
             && grep -q 'READ_MODEL_CONSTANTS' "$d/err.log"; then
            echo "PASS [$1]  (exit=$rc, stderr names '$5')"
        else
            echo "FAIL [$1]  (exit=$rc, expected nonzero + stderr token '$5')"; sed 's/^/    /' "$d/err.log"; fail=1
        fi
    else   # LENIENT: reader must warn but NOT error-stop
        if grep -qi "$5" "$d/err.log" && ! grep -q 'ERROR STOP READ_MODEL_CONSTANTS' "$d/err.log"; then
            echo "PASS [$1]  (warned, reader did not abort)"
        else
            echo "FAIL [$1]  (expected a warning + no reader error stop)"; sed 's/^/    /' "$d/err.log"; fail=1
        fi
    fi
    rm -rf "$d"
}

run_case "dropped constant -> abort"    '$1 != 57'                            ""                            FAIL    'MISSING'
run_case "out-of-range index -> abort"  '{ if ($1==57) $1=9999; print }'      ""                            FAIL    'out of range'
run_case "duplicate index -> abort"     'NR==1 { print "57 DUP_57 0.0" } { print }'  ""                     FAIL    'duplicate'
run_case "lenient: warn, no abort"      '$1 != 57'                            "AQUABC_LENIENT_CONSTANTS=1"   LENIENT 'MISSING'

# Per-box constants: the model applies box 1's constants to every box, so a box that names a
# different constants file must be refused, not silently ignored.
# $1 desc  $2 extra-env  $3 mode(FAIL|LENIENT)
run_box_case() {
    local d; d=$(mktemp -d)
    cp INPUT.txt "$d/"; cp -r INPUTS "$d/INPUTS"
    awk '$2 == "K_A" { $3 = "-2.0" } { print }' INPUTS/WCONST_04.txt > "$d/INPUTS/WCONST_BOX2.txt"
    awk '/PELAGIC MODEL CONSTANTS FILE NAME/ { sec = 1; print; next }
         sec && $1 == 2 && $2 == "WCONST_04.txt" { print "                   2                         WCONST_BOX2.txt"; sec = 0; next }
         { print }' INPUTS/PELAGIC_INPUTS.txt > "$d/INPUTS/PELAGIC_INPUTS.txt"
    grep -q 'WCONST_BOX2.txt' "$d/INPUTS/PELAGIC_INPUTS.txt" || { echo "FAIL [$1]  (could not doctor PELAGIC_INPUTS.txt)"; fail=1; rm -rf "$d"; return; }
    local rc
    ( cd "$d" && env $2 ESTAS_HOLD_VOLUME=1 timeout 90 "$BIN" INPUT.txt ) >/dev/null 2>"$d/err.log"
    rc=$?
    if [ "$3" = FAIL ]; then
        if [ "$rc" -ne 0 ] && grep -q 'differ from box 1' "$d/err.log" \
             && grep -q 'per-box model constants are not supported' "$d/err.log"; then
            echo "PASS [$1]  (exit=$rc, stderr names the box)"
        else
            echo "FAIL [$1]  (exit=$rc, expected nonzero + per-box message)"; sed 's/^/    /' "$d/err.log"; fail=1
        fi
    else
        if grep -q 'differ from box 1' "$d/err.log" && grep -q 'WARNING per-box constants ignored' "$d/err.log" \
             && ! grep -q 'per-box model constants are not supported' "$d/err.log"; then
            echo "PASS [$1]  (warned, did not abort on the per-box check)"
        else
            echo "FAIL [$1]  (expected a warning + no per-box error stop)"; sed 's/^/    /' "$d/err.log"; fail=1
        fi
    fi
    rm -rf "$d"
}

run_box_case "per-box constants -> abort"          ""                            FAIL
run_box_case "per-box constants, lenient -> warn"  "AQUABC_LENIENT_CONSTANTS=1"   LENIENT

if [ "$fail" -eq 0 ]; then
    echo "fail_loud_constants: ALL checks passed"
else
    echo "fail_loud_constants: SOME checks FAILED"; exit 1
fi
