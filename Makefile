.PHONY: link-data build-lib build-example build-estas build-named rebuild rebuild-named run-example run-estas run-0d clean-model clean-lib clean-all show-config check-compiler

# =============================================================================
# Compiler Configuration
# =============================================================================
# FC options:
#   gfortran - GNU Fortran (default, free)
#   ifort    - Intel Fortran Classic (requires Intel oneAPI)
#   ifx      - Intel Fortran LLVM-based (requires Intel oneAPI)
#
# Usage: make FC=ifort BUILD_TYPE=release build-estas
# =============================================================================

FC ?= gfortran
BUILDDIR = SOURCE_CODE/build
LIBAQUABC = $(BUILDDIR)/libaquabc.a

# =============================================================================
# Build Type Configuration
# =============================================================================
# BUILD_TYPE options:
#   debug   - Full debug info, bounds checking, backtraces (slow but catches errors)
#   release - Standard optimizations (good balance of speed and safety)
#   fast    - Aggressive optimizations (fastest, may hide some numerical issues)
#
# Usage: make BUILD_TYPE=debug build-estas
#        make FC=ifort BUILD_TYPE=fast build-estas
# =============================================================================

BUILD_TYPE ?= release

# =============================================================================
# OpenMP Configuration
# =============================================================================
# OPENMP options:
#   0 - Disabled (default)
#   1 - Enabled (adds -fopenmp for gfortran, -qopenmp for Intel)
#
# Usage: make OPENMP=1 FC=gfortran BUILD_TYPE=release build-estas
# =============================================================================

OPENMP ?= 0

# =============================================================================
# Executable Naming
# =============================================================================
# EXE_NAME: Custom executable name (optional)
# If not set, build-estas creates "ESTAS_II"
# build-named creates "ESTAS_II_<fc>_<build_type>" automatically
#
# Usage: make EXE_NAME=my_model build-estas
#        make FC=gfortran BUILD_TYPE=debug build-named
# =============================================================================

# Extract compiler base name (handles full paths like /opt/intel/oneapi/compiler/2024.2/bin/ifx)
FC_BASE = $(notdir $(FC))

# Short compiler name for executable naming
ifeq ($(FC_BASE),gfortran)
    FC_SHORT = gf
else ifeq ($(FC_BASE),ifort)
    FC_SHORT = ifort
else ifeq ($(FC_BASE),ifx)
    FC_SHORT = ifx
else
    FC_SHORT = $(FC_BASE)
endif

# Default executable name
EXE_NAME ?= ESTAS_II

# Auto-generated name for build-named target
EXE_NAME_AUTO = ESTAS_II_$(FC_SHORT)_$(BUILD_TYPE)

# Set compiler-specific flags based on FC_BASE and BUILD_TYPE
# (FC_BASE extracts the compiler name from full paths)
ifeq ($(FC_BASE),gfortran)
    COMPILER_NAME = GNU Fortran (gfortran)
    ifeq ($(BUILD_TYPE),debug)
        FFLAGS = -g -Og -fcheck=all -fbacktrace -Wall -Wextra -pedantic -fimplicit-none -ffpe-trap=invalid,zero,overflow
        BUILD_DESC = Debug (bounds checking, backtraces, warnings, -fimplicit-none)
    else ifeq ($(BUILD_TYPE),fast)
        # WARNING: -ffast-math breaks IEEE 754 compliance. It enables:
        #   -fno-math-errno, -funsafe-math-optimizations, -ffinite-math-only,
        #   -fno-rounding-math, -fno-signaling-nans, -fcx-limited-range
        # This can produce different results for exp(), log(), and trig functions,
        # and may affect Michaelis-Menten kinetics, light limitation, and DO
        # saturation calculations. Use BUILD_TYPE=release for validated results.
        FFLAGS = -O3 -march=native -mtune=native -funroll-loops -ffast-math -flto -Wall -Wextra -fimplicit-none
        BUILD_DESC = Fast (-O3, -ffast-math, LTO, warnings)
    else
        FFLAGS = -O2 -march=native -mtune=native -Wall -Wextra -fimplicit-none
        BUILD_DESC = Release (-O2, native arch, warnings)
    endif
else ifeq ($(FC_BASE),ifort)
    COMPILER_NAME = Intel Fortran Classic (ifort)
    ifeq ($(BUILD_TYPE),debug)
        FFLAGS = -g -O0 -check all -traceback -warn all -fpe0 -debug full
        BUILD_DESC = Debug (full checking, traceback)
    else ifeq ($(BUILD_TYPE),fast)
        # WARNING: -no-prec-div and -fp-model fast=2 break IEEE 754 compliance.
        # -no-prec-div replaces division with reciprocal multiplication (less accurate).
        # -fp-model fast=2 allows aggressive FP reordering and approximations.
        # This can affect scientific results. Use BUILD_TYPE=release for validated runs.
        FFLAGS = -O3 -xHost -ipo -no-prec-div -fp-model fast=2 -warn all
        BUILD_DESC = Fast (-O3, IPO, fast math, warnings)
    else
        FFLAGS = -O2 -xHost -warn all
        BUILD_DESC = Release (-O2, host arch, warnings)
    endif
else ifeq ($(FC_BASE),ifx)
    COMPILER_NAME = Intel Fortran LLVM (ifx)
    # IFX requires explicit linking with Intel Fortran runtime libraries
    IFX_LIB_PATH = $(dir $(FC))../lib
    LDFLAGS_IFX = -L$(IFX_LIB_PATH) -lifcore -lifport -Wl,-rpath,$(IFX_LIB_PATH)
    ifeq ($(BUILD_TYPE),debug)
        FFLAGS = -g -O0 -check all -traceback -warn all -fpe0 -debug full
        BUILD_DESC = Debug (full checking, traceback)
    else ifeq ($(BUILD_TYPE),fast)
        # WARNING: -no-prec-div and -fp-model fast=2 break IEEE 754 compliance.
        # See ifort fast flags above for details.
        FFLAGS = -O3 -xHost -ipo -no-prec-div -fp-model fast=2 -warn all
        BUILD_DESC = Fast (-O3, IPO, fast math, warnings)
    else
        FFLAGS = -O2 -xHost -warn all
        BUILD_DESC = Release (-O2, host arch, warnings)
    endif
else
    # Unknown compiler - use generic flags
    COMPILER_NAME = $(FC)
    ifeq ($(BUILD_TYPE),debug)
        FFLAGS = -g -O0
        BUILD_DESC = Debug (generic)
    else ifeq ($(BUILD_TYPE),fast)
        FFLAGS = -O3
        BUILD_DESC = Fast (generic)
    else
        FFLAGS = -O2
        BUILD_DESC = Release (generic)
    endif
endif

# Append OpenMP flags if enabled
ifeq ($(OPENMP),1)
    ifeq ($(FC_BASE),gfortran)
        FFLAGS += -fopenmp
    else ifeq ($(FC_BASE),ifort)
        FFLAGS += -qopenmp
    else ifeq ($(FC_BASE),ifx)
        FFLAGS += -qopenmp
    endif
endif

# Export for use in make_lib.sh
export FC
export FFLAGS
export BUILD_TYPE

# =============================================================================
# Library and Model Build Targets
# =============================================================================

link-data:
	@echo "Creating top-level data symlink -> SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/data"
	-rm -rf data
	ln -s $(CURDIR)/SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/data data

build-lib:
	@echo "=============================================="
	@echo "Building libaquabc.a"
	@echo "Compiler:   $(COMPILER_NAME)"
	@echo "Build Type: $(BUILD_DESC)"
	@echo "FFLAGS:     $(FFLAGS)"
	@echo "=============================================="
	cd $(BUILDDIR) && ./make_lib.sh

# Build the full ESTAS_II model executable (default name)
build-estas: build-lib
	@echo "=============================================="
	@echo "Building executable: $(EXE_NAME)"
	@echo "Compiler:   $(COMPILER_NAME)"
	@echo "Build Type: $(BUILD_DESC)"
	@echo "FFLAGS:     $(FFLAGS)"
	@echo "=============================================="
	$(FC) $(FFLAGS) -I$(BUILDDIR) -o $(EXE_NAME) $(BUILDDIR)/ESTAS_II.o -L$(BUILDDIR) -laquabc $(LDFLAGS_IFX)
	@echo ""
	@echo "Executable '$(EXE_NAME)' created successfully"
	@echo "  Compiler:   $(FC)"
	@echo "  Build Type: $(BUILD_TYPE)"
	@ls -lh $(EXE_NAME)

# Build with auto-generated name based on compiler and build type
build-named: build-lib
	@echo "=============================================="
	@echo "Building executable: $(EXE_NAME_AUTO)"
	@echo "Compiler:   $(COMPILER_NAME)"
	@echo "Build Type: $(BUILD_DESC)"
	@echo "FFLAGS:     $(FFLAGS)"
	@echo "=============================================="
	$(FC) $(FFLAGS) -I$(BUILDDIR) -o $(EXE_NAME_AUTO) $(BUILDDIR)/ESTAS_II.o -L$(BUILDDIR) -laquabc $(LDFLAGS_IFX)
	@echo ""
	@echo "Executable '$(EXE_NAME_AUTO)' created successfully"
	@echo "  Compiler:   $(FC)"
	@echo "  Build Type: $(BUILD_TYPE)"
	@ls -lh $(EXE_NAME_AUTO)

# Build all common configurations
build-all-configs: build-lib
	@echo "Building all configurations..."
	@echo ""
	@echo "=== gfortran debug ==="
	$(MAKE) FC=gfortran BUILD_TYPE=debug build-named
	@echo ""
	@echo "=== gfortran release ==="
	$(MAKE) FC=gfortran BUILD_TYPE=release build-named
	@echo ""
	@echo "=== gfortran fast ==="
	$(MAKE) FC=gfortran BUILD_TYPE=fast build-named
	@echo ""
	@echo "All configurations built. Available executables:"
	@ls -lh ESTAS_II_* 2>/dev/null || echo "  (none found)"

# Build the simple 0D example (for testing/comparison)
build-example: build-lib
	cd SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D && make aquabc0D

# =============================================================================
# Rebuild Targets (clean + build in one step)
# =============================================================================
# NOTE: The build system uses a single-pass compilation script (make_lib.sh)
# that compiles all .f90 files in a fixed order. Module dependencies are
# handled by compiling modules before their dependents. If you add a new
# module that is used by existing files, you may need to update the
# compilation order in SOURCE_CODE/build/make_lib.sh.
# =============================================================================

# Full rebuild with default executable name
rebuild: clean-lib build-estas

# Full rebuild with auto-generated name
rebuild-named: clean-lib build-named

# =============================================================================
# Run Targets
# =============================================================================

# Run the full ESTAS_II model (uses INPUT.txt configuration)
run-estas: build-estas
	@echo "Running $(EXE_NAME) model with INPUT.txt..."
	./$(EXE_NAME) INPUT.txt

# Default run-example now uses ESTAS_II (the full model)
run-example: run-estas

# Run the simple 0D example (hardcoded parameters, for testing)
run-0d: build-example
	cd SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D && ./aquabc_II_pelagic_0D

# =============================================================================
# Utility Targets
# =============================================================================

clean-model:
	-rm -f ESTAS_II ESTAS_II_*
	-rm -f OUTPUT.csv
	cd SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D && make clean

# Clean compiled library (forces full rebuild)
clean-lib:
	@echo "Cleaning compiled library objects..."
	-rm -f $(BUILDDIR)/*.o $(BUILDDIR)/*.mod $(BUILDDIR)/libaquabc.a
	@echo "Library cleaned. Next build will recompile all sources."

# Full clean (library + executables)
clean-all: clean-model clean-lib
	@echo "Full clean complete."

# Show current build configuration
show-config:
	@echo "=============================================="
	@echo "Current Build Configuration"
	@echo "=============================================="
	@echo "FC:           $(FC)"
	@echo "FC_SHORT:     $(FC_SHORT)"
	@echo "COMPILER:     $(COMPILER_NAME)"
	@echo "BUILD_TYPE:   $(BUILD_TYPE)"
	@echo "BUILD_DESC:   $(BUILD_DESC)"
	@echo "FFLAGS:       $(FFLAGS)"
	@echo "EXE_NAME:     $(EXE_NAME)"
	@echo "EXE_NAME_AUTO:$(EXE_NAME_AUTO)"
	@echo "BUILDDIR:     $(BUILDDIR)"
	@echo "=============================================="
	@echo ""
	@echo "Available executables:"
	@ls -lh ESTAS_II ESTAS_II_* AQUABC* 2>/dev/null || echo "  (none found)"

# Check if compiler is available
check-compiler:
	@which $(FC) > /dev/null 2>&1 || (echo "ERROR: Compiler '$(FC)' not found in PATH" && exit 1)
	@echo "Compiler $(FC) found at: $$(which $(FC))"
	@$(FC) --version | head -1

# List all available executables
list-executables:
	@echo "Available executables:"
	@echo "======================"
	@for exe in ESTAS_II ESTAS_II_* AQUABC*; do \
		if [ -f "$$exe" ] && [ -x "$$exe" ]; then \
			size=$$(ls -lh "$$exe" | awk '{print $$5}'); \
			date=$$(ls -l "$$exe" | awk '{print $$6, $$7, $$8}'); \
			if file "$$exe" | grep -q "not stripped"; then \
				echo "  $$exe  [debug]  $$size  $$date"; \
			else \
				echo "  $$exe  [release]  $$size  $$date"; \
			fi; \
		fi; \
	done
