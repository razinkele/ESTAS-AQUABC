#!/usr/bin/env bash
# =============================================================================
# OpenMP performance benchmark for AQUABC_PELAGIC_KINETICS (TODO 4.1 + 4.3).
#
# Builds the OpenMP release library + a micro-benchmark driver (the node-agnostic
# 0D interface path), then sweeps OMP_NUM_THREADS x nkn, timing the real kinetics
# `!$omp parallel` region with omp_get_wtime(). Also runs a thread-affinity
# comparison (4.3). Emits raw results + a speedup/efficiency table.
#
# Usage:   tools/benchmark_openmp.sh [--quick]
#   --quick   fewer timesteps (fast smoke run)
#
# Output:  tools/benchmark_openmp_results.txt   (raw BENCH_RESULT lines + table)
# =============================================================================
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BENCH_DIR="$ROOT/SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D"
BUILD="$ROOT/SOURCE_CODE/build"
FC="${FC:-gfortran}"
RESULTS="$ROOT/tools/benchmark_openmp_results.txt"

THREADS="${THREADS:-1 2 4 8}"     # override e.g. THREADS="1 2 4 8 16"
NKNS="${NKNS:-100 500 1000}"
# timesteps per nkn (targets a ~2-4 s single-thread baseline; overridden by --quick)
declare -A STEPS=( [100]=3000 [500]=600 [1000]=300 )
if [[ "${1:-}" == "--quick" ]]; then
  declare -A STEPS=( [100]=200 [500]=40 [1000]=20 )
fi

echo "== AQUABC OpenMP benchmark =="
echo "host cores (nproc): $(nproc)   compiler: $FC"

echo "-- building OpenMP release library --"
make -C "$ROOT" OPENMP=1 FC="$FC" BUILD_TYPE=release build-lib >/dev/null
# compiler-appropriate optimized OpenMP driver flags (ifx has no -march=native -> -xHost)
case "$(basename "$FC")" in
  ifx*|ifort*) DRV_FLAGS="-qopenmp -O3 -xHost" ;;
  *)           DRV_FLAGS="-fopenmp -O3 -march=native" ;;
esac
echo "-- compiling benchmark driver ($DRV_FLAGS) --"
( cd "$BENCH_DIR" && "$FC" $DRV_FLAGS -I"$BUILD" \
    aquabc_II_pelagic_benchmark.f90 -L"$BUILD" -laquabc -o benchmark_openmp )

run_one() {  # nkn threads steps  [extra env]  -> echoes the BENCH_RESULT line
  local nkn="$1" thr="$2" steps="$3"
  ( cd "$BENCH_DIR" && env BENCH_NKN="$nkn" BENCH_STEPS="$steps" OMP_NUM_THREADS="$thr" ${4:-} \
      ./benchmark_openmp 2>/dev/null | grep '^BENCH_RESULT' )
}

: > "$RESULTS"
echo "== strong-scaling sweep (threads x nkn) ==" | tee -a "$RESULTS"
for nkn in $NKNS; do
  for thr in $THREADS; do
    line="$(run_one "$nkn" "$thr" "${STEPS[$nkn]}")"
    echo "$line" | tee -a "$RESULTS"
  done
done

echo "" | tee -a "$RESULTS"
echo "== thread-affinity comparison (4.3) : nkn=1000, 8 threads ==" | tee -a "$RESULTS"
echo "default :   $(run_one 1000 8 "${STEPS[1000]}")" | tee -a "$RESULTS"
echo "bind/place: $(run_one 1000 8 "${STEPS[1000]}" 'OMP_PROC_BIND=close OMP_PLACES=cores')" | tee -a "$RESULTS"

# ---- speedup / efficiency table (parsed from the raw lines) ----
echo "" | tee -a "$RESULTS"
echo "== speedup / efficiency (baseline = 1 thread, same nkn) ==" | tee -a "$RESULTS"
awk '
/^BENCH_RESULT/ {
  for (i=1;i<=NF;i++){ split($i,a,"="); v[a[1]]=a[2] }
  nkn=v["nkn"]; thr=v["threads"]; sec=v["seconds"]+0; us=v["us_per_step"]+0
  key=nkn; t[key,thr]=sec; u[key,thr]=us; seen[nkn]=1
}
END {
  printf "%-6s %-8s %-12s %-10s %-11s\n","nkn","threads","us/step","speedup","efficiency"
  n=split("100 500 1000",N," "); h=split("1 2 4 8",H," ")
  for(i=1;i<=n;i++){ nkn=N[i]; if(!(nkn in seen)) continue
    base=t[nkn,1]
    for(j=1;j<=h;j++){ thr=H[j]; if((nkn SUBSEP thr) in t){
      sp=base/t[nkn,thr]; ef=100*sp/thr
      printf "%-6s %-8s %-12.1f %-10.2f %-10.1f%%\n", nkn, thr, u[nkn,thr], sp, ef
    }}
    print ""
  }
}' "$RESULTS" | tee -a "$RESULTS"

echo "raw + table written to: $RESULTS"
