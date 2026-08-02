#!/usr/bin/env bash
# Entrypoint for the reproducible AQUABC container.
#   selftest            (default) print the build banner + re-run the 0D golden self-test
#   bash | sh           interactive shell
#   <anything else>     passthrough (e.g. `run_cl29.sh`, `python3 tools/validate_cl29_vs_epa.py ...`)
set -euo pipefail
cd /app

case "${1:-selftest}" in
  selftest)
    echo "=== AQUABC reproducible container ==="
    gfortran --version | head -1
    [ -x ./ESTAS_II ] && echo "ESTAS_II: present ($(wc -c < ./ESTAS_II) bytes)" || echo "ESTAS_II: MISSING"
    echo "--- 0D example self-test (vs committed golden, rtol 1e-6) ---"
    make -C SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D run >/dev/null
    python3 tests/regression/compare_0D.py \
      SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/OUTPUT.csv \
      tests/regression/pelagic_0D_golden.csv --rtol 1e-6 --atol 1e-9
    echo
    echo "Reproducible build verified. To reproduce CL29, mount the (external) data and run e.g.:"
    echo "  docker run --rm -v \"\$PWD/INPUTS_CL29:/app/INPUTS_CL29\" aquabc run_cl29.sh"
    ;;
  bash|sh)
    exec /bin/bash ;;
  *)
    exec "$@" ;;
esac
