#!/bin/sh
set -e

echo "=============================================="
echo "Building libaquabc.a in $(pwd)"
echo "=============================================="

# Use FC from environment if set, otherwise default to gfortran
if [ -z "$FC" ]; then
    FC="gfortran"
    echo "FC not set, using default: $FC"
else
    echo "Using compiler from environment: $FC"
fi

# Use FFLAGS from environment if set, otherwise default to -O2
if [ -z "$FFLAGS" ]; then
    FFLAGS="-O2"
    echo "FFLAGS not set, using default: $FFLAGS"
else
    echo "Using FFLAGS from environment: $FFLAGS"
fi

# Show build type if set
if [ -n "$BUILD_TYPE" ]; then
    echo "Build type: $BUILD_TYPE"
fi

# Set module output directory flag based on compiler
case "$FC" in
    gfortran*)
        MOD_FLAG="-J."
        ;;
    ifort*|ifx*)
        MOD_FLAG="-module ."
        ;;
    *)
        # Default to gfortran-style
        MOD_FLAG="-J."
        ;;
esac
echo "Module flag: $MOD_FLAG"
echo "=============================================="

# Collect Fortran sources (exclude examples and the build dir itself)
SRCS=$(find .. -type f -name '*.f90' -not -path '../AQUABC/AQUABC_EXAMPLES/*' -not -path './*' -not -path '../build/*' 2>/dev/null)

# Clean old objects
rm -f *.o *.mod libaquabc.a

# Count total sources for progress
total_srcs=$(echo "$SRCS" | wc -w)
echo "Found $total_srcs Fortran source files to compile"

# Try compiling sources in multiple passes to resolve module dependencies automatically
remaining="$SRCS"
compiled_any=1
pass=1
total_compiled=0

while [ -n "$remaining" ] && [ "$compiled_any" -eq 1 ]; do
  echo ""
  echo "=== Compilation pass $pass ==="
  compiled_any=0
  new_remaining=""
  compiled_count=0
  for src in $remaining; do
    base=$(basename "$src" .f90)
    if [ -f "$base.o" ]; then
      continue
    fi
    echo "Compiling: $src"
    if $FC -c $FFLAGS $MOD_FLAG -o "$base.o" "$src" 2>/tmp/compile_err.txt; then
      compiled_any=1
      compiled_count=$((compiled_count + 1))
      total_compiled=$((total_compiled + 1))
    else
      echo "  -> Deferred (missing dependencies)"
      new_remaining="$new_remaining $src"
    fi
  done
  echo "Pass $pass: compiled $compiled_count file(s), total: $total_compiled/$total_srcs"
  remaining="$new_remaining"
  pass=$((pass + 1))
done

if [ -n "$remaining" ]; then
  echo "" >&2
  echo "=============================================="  >&2
  echo "ERROR: Compilation failed!" >&2
  echo "=============================================="  >&2
  echo "Some sources could not be compiled:" >&2
  for src in $remaining; do echo "  $src" >&2; done
  echo "" >&2
  echo "Last compiler error:" >&2
  echo "----------------------------------------------" >&2
  cat /tmp/compile_err.txt >&2 || true
  echo "----------------------------------------------" >&2
  exit 1
fi

# Create static archive
echo ""
echo "Creating static library..."
ar rcs libaquabc.a *.o

echo ""
echo "=============================================="
echo "Build complete!"
echo "Built libaquabc.a with $(ls -1 *.o | wc -l) object file(s)"
echo "Compiler: $FC"
echo "=============================================="
