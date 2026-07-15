#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Capture pre-refactor reference outputs for the TODO 1.6 bit-identical gate
# (AQUABC_PELAGIC_KINETICS decomposition).
#
# Run ONCE on the pre-refactor code, before any phase extraction. For EACH of
# two 30-day 25-box configs it builds both binaries and runs serial + OMP=8,
# saving outputs under verify_baseline/ (git-ignored). The gate script
# tools/refactor_verify.sh diffs fresh outputs against these.
#
# The two configs together close the gate's coverage gaps:
#   default  (INPUT_verify.txt)    — all 25 boxes emit state+process outputs, so
#                                     every OpenMP thread-chunk has monitored nodes.
#   advredox (INPUT_verify_ar.txt) — ADVANCED_REDOX_OPTION=1 + LIGHT_EXTINCTION=1,
#                                     exercising the DO_ADVANCED_REDOX_SIMULATION>0
#                                     branches (incl. the REDOX/DOCMIN bundle
#                                     population — the refactor's correctness crux).
#
# serial and omp8 baselines of the same config differ from each other by a few
# PROCESS_RATES files (the TODO 4.2 CO2SYS chunking drift) — which is why the
# gate compares each (config,threadmode) only against its OWN baseline.
# ---------------------------------------------------------------------------
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
BASE="$ROOT/verify_baseline"

# label  input-config  output-dir
CONFIGS=(
    "default  INPUT_verify.txt     OUTPUTS_verify"
    "advredox INPUT_verify_ar.txt  OUTPUTS_verify_ar"
)

rm -rf "$BASE"
mkdir -p "$BASE"

capture() {  # label cfg outdir kind threads
    local label="$1" cfg="$2" outdir="$3" kind="$4" thr="$5"
    mkdir -p "$outdir"
    rm -f "$outdir"/*.out
    OMP_NUM_THREADS="$thr" ./ESTAS_II "$cfg" >/dev/null
    mkdir -p "$BASE/${label}_${kind}"
    cp "$outdir"/*.out "$BASE/${label}_${kind}/"
}

echo "== non-omp build =="
make build-estas >/dev/null
for c in "${CONFIGS[@]}"; do
    # shellcheck disable=SC2086
    set -- $c
    echo "   serial: $1"
    capture "$1" "$2" "$3" serial 1
done

echo "== omp build =="
make OPENMP=1 build-estas >/dev/null
for c in "${CONFIGS[@]}"; do
    # shellcheck disable=SC2086
    set -- $c
    echo "   omp8:   $1"
    capture "$1" "$2" "$3" omp8 8
done

echo "baselines captured under $BASE:"
for d in "$BASE"/*/; do
    echo "  $(basename "$d"): $(ls "$d"*.out 2>/dev/null | wc -l) .out files"
done
