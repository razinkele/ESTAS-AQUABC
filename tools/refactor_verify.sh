#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Bit-identical gate for the TODO 1.6 decomposition of AQUABC_PELAGIC_KINETICS.
#
# Run after every phase extraction. For two configs (default all-box, and
# advredox) it rebuilds both binaries, re-runs serial + OMP=8, and diffs each
# (config,threadmode) against its OWN pre-refactor baseline (captured by
# tools/refactor_baseline.sh). Also re-runs the 0D golden regression. Because
# the refactor is pure code motion, every same-config diff must be exactly
# byte-for-byte identical.
#
# A build failure aborts the gate (never diffs a stale binary). Exit 0 iff all
# config/threadmode diffs and the 0D golden are bit-identical; else exit 1.
# ---------------------------------------------------------------------------
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
BASE="$ROOT/verify_baseline"
fail=0

# label  input-config  output-dir
# NOTE: default-only. The advredox config (INPUT_verify_ar.txt) is NOT gated
# because the ADVANCED_REDOX path has pre-existing uninitialised-memory bugs
# (see TODO 1.10/1.11) that make it non-deterministic run-to-run, so it cannot
# serve as a bit-identical oracle. The all-box default config is deterministic
# and covers all 25 nodes / all 8 OpenMP chunks. Re-add advredox here once those
# bugs are fixed.
CONFIGS=(
    "default  INPUT_verify.txt     OUTPUTS_verify"
)

if [ ! -d "$BASE" ] || [ -z "$(ls -A "$BASE" 2>/dev/null)" ]; then
    echo "ERROR: no baseline under $BASE — run tools/refactor_baseline.sh first" >&2
    exit 2
fi

build_or_die() {  # make-args...
    if ! make "$@" >/dev/null; then
        echo "GATE: FAIL — build failed (make $*)"
        exit 1
    fi
}

diff_one() {  # label cfg outdir kind
    local label="$1" cfg="$2" outdir="$3" kind="$4"
    local bdir="$BASE/${label}_${kind}" nd=0 f b
    rm -f "$outdir"/*.out
    if [ "$kind" = omp8 ]; then OMP_NUM_THREADS=8 ./ESTAS_II "$cfg" >/dev/null
    else ./ESTAS_II "$cfg" >/dev/null; fi
    for f in "$bdir"/*.out; do
        b=$(basename "$f")
        if [ ! -f "$outdir/$b" ] || ! cmp -s "$f" "$outdir/$b"; then
            echo "    DIFF: ${label}/${kind}/$b"
            nd=$((nd + 1))
        fi
    done
    if [ "$nd" -eq 0 ]; then
        echo "  [${label}/${kind}] BIT-IDENTICAL ($(ls "$bdir"/*.out | wc -l) files)"
    else
        echo "  [${label}/${kind}] FAILED: $nd file(s) differ"
        fail=1
    fi
}

echo "== [1/3] serial (non-omp build) =="
build_or_die build-estas
for c in "${CONFIGS[@]}"; do
    # shellcheck disable=SC2086
    set -- $c
    diff_one "$1" "$2" "$3" serial
done

echo "== [2/3] OMP_NUM_THREADS=8 (omp build) =="
build_or_die OPENMP=1 build-estas
for c in "${CONFIGS[@]}"; do
    # shellcheck disable=SC2086
    set -- $c
    diff_one "$1" "$2" "$3" omp8
done

echo "== [3/3] 0D golden regression (rtol 1e-9) =="
build_or_die run-0d
if .venv/bin/python tests/regression/compare_0D.py \
        SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/OUTPUT.csv \
        tests/regression/pelagic_0D_golden.csv --rtol 1e-9 >/dev/null 2>&1; then
    echo "  [0D golden] PASS"
else
    echo "  [0D golden] FAILED"
    fail=1
fi

echo "======================================"
if [ "$fail" -eq 0 ]; then
    echo "GATE: PASS — bit-identical (default config x serial+omp8, and 0D golden)"
else
    echo "GATE: FAIL — a same-config diff was not bit-identical (investigate; do NOT loosen tolerance)"
fi
exit "$fail"
