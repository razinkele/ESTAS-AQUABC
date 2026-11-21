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
# The strict cases must not inherit leniency from the caller's environment.
unset AQUABC_LENIENT_CONSTANTS
[ -x "$BIN" ] || { echo "fail_loud_constants: ESTAS_II not built"; exit 1; }
fail=0

# $1 desc  $2 awk-doctor-program  $3 extra-env  $4 mode(FAIL|LENIENT)  $5 stderr-token
run_case() {
    local d; d=$(mktemp -d) || { echo "FAIL [$1]  (mktemp failed)"; fail=1; return; }
    [ -n "$d" ] && [ -d "$d" ] || { echo "FAIL [$1]  (no temp dir)"; fail=1; return; }
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

# Per-box constants: the model applies box 1's constants to every box, so a box whose constants
# differ must be refused, not silently ignored. The fixture gives box 2 a file that differs from
# box 1's in constant 1 (K_A); the awk program in $2 rewrites the constants table of
# PELAGIC_INPUTS.txt (rows `box-number file-name`); it runs inside an awk action, so use `if`.
# $1 desc  $2 awk-doctor for the table rows  $3 extra-env  $4 mode(FAIL|LENIENT)  $5 stderr regex
run_box_case() {
    local d; d=$(mktemp -d) || { echo "FAIL [$1]  (mktemp failed)"; fail=1; return; }
    [ -n "$d" ] && [ -d "$d" ] || { echo "FAIL [$1]  (no temp dir)"; fail=1; return; }
    cp INPUT.txt "$d/"; cp -r INPUTS "$d/INPUTS"
    mkdir -p "$d/OUTPUTS"          # INPUT.txt writes there; the lenient case runs past the reader
    awk '$2 == "K_A" { $3 = "-2.0" } { print }' INPUTS/WCONST_04.txt > "$d/INPUTS/WCONST_BOX2.txt"
    awk '/PELAGIC MODEL CONSTANTS FILE NAME/ { sec = 1; print; next }
         /^#/ { sec = 0 }
         sec { '"$2"' }
         { print }' INPUTS/PELAGIC_INPUTS.txt > "$d/INPUTS/PELAGIC_INPUTS.txt"
    if [ ! -s "$d/INPUTS/PELAGIC_INPUTS.txt" ] || cmp -s INPUTS/PELAGIC_INPUTS.txt "$d/INPUTS/PELAGIC_INPUTS.txt"; then
        echo "FAIL [$1]  (could not doctor PELAGIC_INPUTS.txt)"; fail=1; rm -rf "$d"; return
    fi
    local rc
    ( cd "$d" && env $3 ESTAS_HOLD_VOLUME=1 timeout 90 "$BIN" INPUT.txt ) >"$d/out.log" 2>"$d/err.log"
    rc=$?
    if [ "$4" = FAIL ]; then
        if [ "$rc" -ne 0 ] && [ "$rc" -ne 124 ] && grep -Eq "$5" "$d/err.log"; then
            echo "PASS [$1]  (exit=$rc, stderr matches)"
        else
            echo "FAIL [$1]  (exit=$rc, expected an abort + stderr /$5/)"; sed 's/^/    /' "$d/err.log"; fail=1
        fi
    else   # LENIENT: warn, then actually run (time stepping starts; 124 = still running at the timeout)
        if { [ "$rc" -eq 0 ] || [ "$rc" -eq 124 ]; } && grep -Eq "$5" "$d/err.log" \
             && grep -q 'WARNING per-box constants ignored' "$d/err.log" && grep -q 'TIME :' "$d/out.log"; then
            echo "PASS [$1]  (exit=$rc, warned and the run started stepping)"
        else
            echo "FAIL [$1]  (exit=$rc, expected a warning + a running model)"; sed 's/^/    /' "$d/err.log"; fail=1
        fi
    fi
    rm -rf "$d"
}

BOX2='if ($1 == 2) { print "                   2                         WCONST_BOX2.txt"; next }'
# rows 1 and 2 swapped: the differing file still belongs to the box the row NAMES (2)
SWAP='if ($1 == 1) { print "                   2                         WCONST_BOX2.txt"; next }
      if ($1 == 2) { print "                   1                           WCONST_04.txt"; next }'
DUP='if ($1 == 2) { print "                   3                           WCONST_04.txt"; next }'
RANGE='if ($1 == 2) { print "                 999                           WCONST_04.txt"; next }'
DIFFERS='box +2 +has model constants that differ from box 1 \(first at constant # +1 *\)'

run_box_case "per-box constants -> abort"            "$BOX2"  ""                            FAIL    "$DIFFERS"
run_box_case "rows out of order -> names the box"    "$SWAP"  ""                            FAIL    "$DIFFERS"
run_box_case "duplicate box number -> abort"         "$DUP"   ""                            FAIL    'names box +3 *, which an earlier row already named'
run_box_case "box number out of range -> abort"      "$RANGE" ""                            FAIL    'names box +999 *, outside'
run_box_case "per-box constants, lenient -> warn"    "$BOX2"  "AQUABC_LENIENT_CONSTANTS=1"   LENIENT "$DIFFERS"
run_box_case "lenient needs exactly 1 -> abort"      "$BOX2"  "AQUABC_LENIENT_CONSTANTS=10"  FAIL    "$DIFFERS"

# A constants file that does not exist, named in the table or on the command line, used to fall
# back silently to WCONST_04.txt and run the whole simulation on it (review finding XC-6). It must
# abort; AQUABC_LENIENT_CONSTANTS=1 keeps the fallback, with a warning on stderr.
# $1 desc  $2 awk-doctor for the table rows ("" = table unchanged)  $3 extra-env
# $4 constants file given as the second argument ("" = none)  $5 mode(FAIL|LENIENT)  $6 stderr regex
run_missing_case() {
    local d; d=$(mktemp -d) || { echo "FAIL [$1]  (mktemp failed)"; fail=1; return; }
    [ -n "$d" ] && [ -d "$d" ] || { echo "FAIL [$1]  (no temp dir)"; fail=1; return; }
    cp INPUT.txt "$d/"; cp -r INPUTS "$d/INPUTS"
    mkdir -p "$d/OUTPUTS"          # the lenient case runs past the reader
    if [ -n "$2" ]; then
        awk '/PELAGIC MODEL CONSTANTS FILE NAME/ { sec = 1; print; next }
             /^#/ { sec = 0 }
             sec { '"$2"' }
             { print }' INPUTS/PELAGIC_INPUTS.txt > "$d/INPUTS/PELAGIC_INPUTS.txt"
        if [ ! -s "$d/INPUTS/PELAGIC_INPUTS.txt" ] || cmp -s INPUTS/PELAGIC_INPUTS.txt "$d/INPUTS/PELAGIC_INPUTS.txt"; then
            echo "FAIL [$1]  (could not doctor PELAGIC_INPUTS.txt)"; fail=1; rm -rf "$d"; return
        fi
    fi
    local rc
    ( cd "$d" && env $3 ESTAS_HOLD_VOLUME=1 timeout 90 "$BIN" INPUT.txt $4 ) >"$d/out.log" 2>"$d/err.log"
    rc=$?
    if [ "$5" = FAIL ]; then
        if [ "$rc" -ne 0 ] && [ "$rc" -ne 124 ] && grep -Eq "$6" "$d/err.log" \
             && ! grep -q 'TIME :' "$d/out.log"; then
            echo "PASS [$1]  (exit=$rc, stderr matches, no time stepping)"
        else
            echo "FAIL [$1]  (exit=$rc, expected an abort before time stepping + stderr /$6/)"
            sed 's/^/    /' "$d/err.log"; fail=1
        fi
    else   # LENIENT: warn, fall back, then actually run (124 = still running at the timeout)
        if { [ "$rc" -eq 0 ] || [ "$rc" -eq 124 ]; } && grep -Eq "$6" "$d/err.log" \
             && grep -q 'Falling back to WCONST_04.txt' "$d/err.log" && grep -q 'TIME :' "$d/out.log"; then
            echo "PASS [$1]  (exit=$rc, warned, fell back and the run started stepping)"
        else
            echo "FAIL [$1]  (exit=$rc, expected a fallback warning + a running model)"
            sed 's/^/    /' "$d/err.log"; fail=1
        fi
    fi
    rm -rf "$d"
}

NOPE='if ($1 == 1) { print "                   1                          WCONST_NOPE.txt"; next }'
NOTFOUND='constants file "[^"]*WCONST_NOPE\.txt" not found'

run_missing_case "missing constants file in the table -> abort"    "$NOPE" ""                           ""                FAIL    "$NOTFOUND"
run_missing_case "missing constants file as argument -> abort"     ""      ""                           "WCONST_NOPE.txt" FAIL    "$NOTFOUND"
run_missing_case "missing constants file, lenient -> fall back"    "$NOPE" "AQUABC_LENIENT_CONSTANTS=1" ""                LENIENT "$NOTFOUND"

if [ "$fail" -eq 0 ]; then
    echo "fail_loud_constants: ALL checks passed"
else
    echo "fail_loud_constants: SOME checks FAILED"; exit 1
fi
