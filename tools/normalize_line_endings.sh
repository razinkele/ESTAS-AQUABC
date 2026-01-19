#!/usr/bin/env bash
set -euo pipefail
# Normalize line endings to LF for Fortran source files to avoid tools misparsing CRLF
find SOURCE_CODE -type f \( -name "*.f90" -o -name "*.F90" -o -name "*.f" -o -name "*.F" \) -print0 | while IFS= read -r -d '' file; do
  if grep -Iq $'\r' "$file"; then
    echo "Normalizing: $file"
    sed -i 's/\r$//' "$file"
  fi
done

echo "Done."