#!/bin/bash
# TEMPORARY diagnostic (fix/ifx-build-multipass-hang) — removed before the PR is final.
# Enumerates ALL remaining ifx strict-conformance errors in one pass, instead of discovering
# them one-CI-round-at-a-time. Builds every .mod at -O0 first (module files are already clean),
# then compiles every source at -O2 with all deps present and prints each file's `error #` lines.
set -u
RDFLAGS="-xHost -fp-model precise -warn all -qopenmp -heap-arrays"
IFX="$(command -v ifx)"; "$IFX" --version | head -1
cd "$(dirname "$0")/../SOURCE_CODE/build" || exit 1
SRCS=$(find .. -type f -name '*.f90' -not -path '../AQUABC/AQUABC_EXAMPLES/*' -not -path '../build/*')
rm -f ./*.o ./*.mod
mkdir -p /tmp/errlogs

echo "== phase 1: build all .mod at -O0 =="
remaining="$SRCS"
for round in $(seq 1 20); do
  any=0; nr=""
  for src in $remaining; do
    base=$(basename "$src" .f90)
    [ -f "$base.o" ] && continue
    if timeout 120 "$IFX" -O0 $RDFLAGS -module . -c -o "$base.o" "$src" >"/tmp/errlogs/$base.o0" 2>&1; then
      any=1
    else
      nr="$nr $src"
    fi
  done
  remaining="$nr"
  { [ -z "$remaining" ] || [ "$any" -eq 0 ]; } && break
done
echo "mods built: $(ls ./*.mod 2>/dev/null | wc -l)"

echo ""; echo "== phase 2: ifx -O2 error scan (every source, deps present) =="
nerr=0; files=""
for src in $SRCS; do
  base=$(basename "$src" .f90); md="/tmp/jm_$base"
  if ! timeout 200 "$IFX" -O2 $RDFLAGS -I . -module "$md" -c -o /dev/null "$src" >"/tmp/errlogs/$base.o2" 2>&1; then
    if grep -qE "error #" "/tmp/errlogs/$base.o2"; then
      echo ""; echo "### $base ###"
      grep -E "error #" "/tmp/errlogs/$base.o2" | head -25
      nerr=$((nerr+1)); files="$files $base"
    fi
  fi
  rm -rf "$md"
done
echo ""; echo "===== FILES WITH REAL ifx ERRORS: $nerr =====$files"
