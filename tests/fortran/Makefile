# Makefile for AQUABC Fortran unit tests
#
# Usage:
#   make all      - Compile all tests
#   make test     - Compile and run all tests
#   make clean    - Remove compiled binaries
#   make test_do_saturation - Compile and run DO saturation tests
#   make test_chla          - Compile and run CHLA tests

# Compiler settings. -fimplicit-none makes an undeclared name a compile error, as in the model and library build
# (the top-level Makefile); every test program also carries its own `implicit none` statement.
FC = gfortran
FFLAGS = -O2 -Wall -Wextra -fcheck=all -fbacktrace -fimplicit-none

# Source directories
AQUABC_SRC = ../../SOURCE_CODE/AQUABC
PELAGIC_LIB = $(AQUABC_SRC)/PELAGIC/AQUABC_PELAGIC_LIBRARY
CORE_UTILS = ../../SOURCE_CODE/CORE_UTILS
ALLELOPATHY_SRC = ../../SOURCE_CODE/ALLELOPATHY
ALLELOPATHY_LIB = $(ALLELOPATHY_SRC)/ALLELOPATHY_LIBRARY

# Test executables
TEST_PROGS = test_do_saturation test_chla test_para_aqua test_settling test_ph_corr test_ip_soluble test_allelopathy test_iron_ii test_diss_me test_organic_carbon test_growth_temp test_nutrient_prefs test_vector_utils test_ammonia_chem test_light test_settling_suppres test_file_unit test_diatoms test_cyanobacteria test_zooplankton test_redox test_doc_mineralization test_fix_cyn test_opa test_nostocales test_bioturbation test_sed_typemap test_co2sys test_nost_staging test_cyn_droop test_lim_light_daylength test_estas_alerts test_basin_level test_validate_constants

# Source directories for additional modules
PELAGIC_AUX = $(AQUABC_SRC)/PELAGIC/aquabc_II_pelagic_auxillary.f90
POSITIONING_SRC = $(AQUABC_SRC)/PELAGIC/aquabc_positioning_state.f90
NOST_STAGING_SRC = $(AQUABC_SRC)/PELAGIC/aquabc_nost_staging.f90
CYN_DROOP_SRC = $(AQUABC_SRC)/PELAGIC/aquabc_cyn_droop.f90
PRECISION_KINDS_SRC = $(CORE_UTILS)/precision_kinds.f90
SEDIMENT_SRC = $(AQUABC_SRC)/SEDIMENTS

# Source dependencies for each test
DO_SAT_SRCS = $(PHYS_CONST_SRC) $(PELAGIC_LIB)/aquabc_II_pelagic_lib_DO_SATURATION.f90
CHLA_SRCS = $(PELAGIC_LIB)/aquabc_II_pelagic_lib_CHLA.f90
PARA_AQUA_SRCS = $(CORE_UTILS)/STRING_UTILS.f90
SETTLING_SRCS = $(PELAGIC_LIB)/aquabc_II_pelagic_lib_SETTLING.f90
PH_CORR_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 $(PELAGIC_LIB)/aquabc_II_pelagic_lib_PH_CORR.f90
IP_SOLUBLE_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 $(AQUABC_SRC)/PELAGIC/aquabc_physical_constants.f90 $(PELAGIC_LIB)/aquabc_II_pelagic_lib_IP_SOLUBLE_FRACTION.f90
ALLELOPATHY_SRCS = $(ALLELOPATHY_SRC)/mod_ALLELOPATHY.f90 \
    $(ALLELOPATHY_LIB)/allelopathy_INHIBITION_RATES.f90 \
    $(ALLELOPATHY_LIB)/allelopathy_SEC_METABOLITES.f90
IRON_II_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 \
    $(PELAGIC_LIB)/aquabc_II_pelagic_lib_DO_SATURATION.f90 \
    $(PELAGIC_LIB)/aquabc_II_pelagic_lib_IRON_II.f90
DISS_ME_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 \
    $(PELAGIC_LIB)/aquabc_II_pelagic_lib_DISS_ME.f90
PELAGIC_TYPES_SRC = $(AQUABC_SRC)/PELAGIC/aquabc_II_pelagic_types.f90
PELAGIC_MODEL_CONSTANTS_SRC = $(AQUABC_SRC)/PELAGIC/aquabc_II_pelagic_model_constants.f90
ORGANIC_CARBON_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 \
    $(PELAGIC_LIB)/aquabc_II_pelagic_lib_PH_CORR.f90 \
    $(PELAGIC_TYPES_SRC) \
    $(PELAGIC_LIB)/aquabc_II_pelagic_lib_ORGANIC_CARBON.f90
GROWTH_TEMP_SRCS = $(PELAGIC_AUX)
NUTRIENT_PREFS_SRCS = $(PELAGIC_AUX)
VECTOR_UTILS_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 $(CORE_UTILS)/VECTOR_MATRIX_UTILS.f90

# Shared sources for kinetics tests
PHYS_CONST_SRC = $(AQUABC_SRC)/PELAGIC/aquabc_physical_constants.f90
CO2SYS_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 \
    $(CORE_UTILS)/VECTOR_MATRIX_UTILS.f90 \
    $(PHYS_CONST_SRC) \
    $(AQUABC_SRC)/CO2SYS/aquabc_II_co2sys.f90
TEST_DEFAULTS_SRC = test_defaults.f90
# The production auxiliary routines (GROWTH_AT_TEMP, LIM_LIGHT, the nutrient
# preferences, light_kd, CUR_SMITH, settling_suppres_factor, ...). The tests link
# this file itself, not a copy: a trimmed copy used here until 2026-11 kept a
# CTMI-only GROWTH_AT_TEMP after production gained the plateau model, so the
# default temperature path was never tested. Its sediment-coupling routines use
# the ESTAS GLOBAL module, hence the extra modules compiled before it. The rules
# that use it have already compiled precision_kinds, AQUABC_II_GLOBAL, the
# physical constants, STRING_UTILS (para_aqua) and the model constants.
ESTAS_SRC = ../../SOURCE_CODE/ESTAS
AUX_GLOBAL_DEPS = $(AQUABC_SRC)/PELAGIC/aquabc_II_pelagic_svindex.f90 \
    $(ESTAS_SRC)/mod_INTERPOLATE.f90 $(ESTAS_SRC)/mod_TIME_SERIES.f90 $(ESTAS_SRC)/mod_GLOBAL.f90
AUX_OBJS = precision_kinds.o mod_AQUABC_II_GLOBAL.o aquabc_physical_constants.o \
    STRING_UTILS.o aquabc_II_pelagic_model_constants.o aquabc_II_pelagic_svindex.o \
    mod_INTERPOLATE.o mod_TIME_SERIES.o mod_GLOBAL.o aquabc_II_pelagic_auxillary.o
# Every production source BUILD_AUX compiles: the prerequisites of the tests that link it,
# so a change to any of them rebuilds those tests.
AUX_ALL_SRCS = $(PRECISION_KINDS_SRC) $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 $(PHYS_CONST_SRC) \
    $(CORE_UTILS)/STRING_UTILS.f90 $(PELAGIC_MODEL_CONSTANTS_SRC) $(AUX_GLOBAL_DEPS) $(PELAGIC_AUX)
# Compile the production auxiliary file and everything it needs, in order.
define BUILD_AUX
$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
endef

# The targets compile shared objects and .mod files into this one directory, so two recipes
# running at once would overwrite each other's files: build serially even under `make -j`.
.NOTPARALLEL:

# Default target
.PHONY: all
all: $(TEST_PROGS)

# Compile test_do_saturation
test_do_saturation: test_do_saturation.f90 $(DO_SAT_SRCS)
	$(FC) $(FFLAGS) -o $@ $^

# Compile test_chla  
test_chla: test_chla.f90 $(CHLA_SRCS)
	$(FC) $(FFLAGS) -o $@ $^

# Compile test_para_aqua
# Need to compile the module first, then the test
test_para_aqua: test_para_aqua.f90 $(PARA_AQUA_SRCS)
	$(FC) $(FFLAGS) -c $(PARA_AQUA_SRCS)
	$(FC) $(FFLAGS) -o $@ test_para_aqua.f90 *.o

# Compile test_settling
test_settling: test_settling.f90 $(SETTLING_SRCS)
	$(FC) $(FFLAGS) -o $@ $^

# Compile test_ph_corr
# Need to compile precision_kinds and the global module first
test_ph_corr: test_ph_corr.f90 $(PH_CORR_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_PH_CORR.f90
	$(FC) $(FFLAGS) -o $@ test_ph_corr.f90 *.o

# Compile test_ip_soluble
test_ip_soluble: test_ip_soluble.f90 $(IP_SOLUBLE_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/PELAGIC/aquabc_physical_constants.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_IP_SOLUBLE_FRACTION.f90
	$(FC) $(FFLAGS) -o $@ test_ip_soluble.f90 *.o

# Compile test_allelopathy
test_allelopathy: test_allelopathy.f90 $(ALLELOPATHY_SRCS)
	$(FC) $(FFLAGS) -c $(ALLELOPATHY_SRC)/mod_ALLELOPATHY.f90
	$(FC) $(FFLAGS) -c $(ALLELOPATHY_LIB)/allelopathy_INHIBITION_RATES.f90
	$(FC) $(FFLAGS) -c $(ALLELOPATHY_LIB)/allelopathy_SEC_METABOLITES.f90
	$(FC) $(FFLAGS) -o $@ test_allelopathy.f90 *.o

# Compile test_iron_ii
test_iron_ii: test_iron_ii.f90 $(IRON_II_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_DO_SATURATION.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_IRON_II.f90
	$(FC) $(FFLAGS) -o $@ test_iron_ii.f90 *.o

# Compile test_diss_me
test_diss_me: test_diss_me.f90 $(DISS_ME_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_DISS_ME.f90
	$(FC) $(FFLAGS) -o $@ test_diss_me.f90 *.o

# Compile test_organic_carbon
test_organic_carbon: test_organic_carbon.f90 $(ORGANIC_CARBON_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_PH_CORR.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_ORGANIC_CARBON.f90
	$(FC) $(FFLAGS) -o $@ test_organic_carbon.f90 *.o

# Compile test_growth_temp (links the production GROWTH_AT_TEMP)
test_growth_temp: test_growth_temp.f90 $(AUX_ALL_SRCS)
	$(BUILD_AUX)
	$(FC) $(FFLAGS) -o $@ test_growth_temp.f90 $(AUX_OBJS)

# Compile test_nutrient_prefs (links the production the nutrient-preference routines)
test_nutrient_prefs: test_nutrient_prefs.f90 $(AUX_ALL_SRCS)
	$(BUILD_AUX)
	$(FC) $(FFLAGS) -o $@ test_nutrient_prefs.f90 $(AUX_OBJS)

# Compile test_vector_utils
test_vector_utils: test_vector_utils.f90 $(VECTOR_UTILS_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/VECTOR_MATRIX_UTILS.f90
	$(FC) $(FFLAGS) -o $@ test_vector_utils.f90 *.o

# Compile test_sed_typemap (Phase-2a two-type sediment profile assignment)
# Hermetic link (explicit objects, not *.o) so stale objects in this shared
# dir cannot leak in — the pure module only needs precision_kinds.
test_sed_typemap: test_sed_typemap.f90 $(ESTAS_SRC)/mod_SED_TYPEMAP.f90
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC) -o precision_kinds.o
	$(FC) $(FFLAGS) -c $(ESTAS_SRC)/mod_SED_TYPEMAP.f90 -o mod_SED_TYPEMAP.o
	$(FC) $(FFLAGS) -o $@ test_sed_typemap.f90 mod_SED_TYPEMAP.o precision_kinds.o

# Compile test_ammonia_chem (links the production the ammonia routines)
test_ammonia_chem: test_ammonia_chem.f90 $(AUX_ALL_SRCS)
	$(BUILD_AUX)
	$(FC) $(FFLAGS) -o $@ test_ammonia_chem.f90 $(AUX_OBJS)

# Compile test_light (links the production light_kd and CUR_SMITH)
test_light: test_light.f90 $(AUX_ALL_SRCS)
	$(BUILD_AUX)
	$(FC) $(FFLAGS) -o $@ test_light.f90 $(AUX_OBJS)

# Compile test_lim_light_daylength (LIGHT_DAYLENGTH_OPTION in LIM_LIGHT).
# Links the production LIM_LIGHT.
test_lim_light_daylength: test_lim_light_daylength.f90 $(AUX_ALL_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
	$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
	$(FC) $(FFLAGS) -o $@ test_lim_light_daylength.f90 \
	    $(AUX_OBJS)

# Compile test_settling_suppres (links the production settling_suppres_factor)
test_settling_suppres: test_settling_suppres.f90 $(AUX_ALL_SRCS)
	$(BUILD_AUX)
	$(FC) $(FFLAGS) -o $@ test_settling_suppres.f90 $(AUX_OBJS)

# Compile test_estas_alerts (ESTAS per-timestep alert rate limiting)
# Compile test_validate_constants (links the production validator)
test_validate_constants: test_validate_constants.f90 $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_validate_constants.f90 precision_kinds.o mod_AQUABC_II_GLOBAL.o STRING_UTILS.o aquabc_II_pelagic_model_constants.o

ESTAS_ALERTS_SRC = ../../SOURCE_CODE/ESTAS/mod_ESTAS_ALERTS.f90
test_estas_alerts: test_estas_alerts.f90 $(ESTAS_ALERTS_SRC)
	$(FC) $(FFLAGS) -o $@ $(ESTAS_ALERTS_SRC) test_estas_alerts.f90

# Compile test_file_unit (links the production find_unit)
test_file_unit: test_file_unit.f90 $(CORE_UTILS)/FIND_FILE_UNIT.f90
	$(FC) $(FFLAGS) -o $@ test_file_unit.f90 $(CORE_UTILS)/FIND_FILE_UNIT.f90

# Compile test_diatoms (kinetics test)
test_diatoms: test_diatoms.f90 $(TEST_DEFAULTS_SRC) $(AUX_ALL_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
	$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_DIATOMS.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_diatoms.f90 *.o

# Compile test_cyanobacteria (kinetics test)
test_cyanobacteria: test_cyanobacteria.f90 $(TEST_DEFAULTS_SRC) $(AUX_ALL_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
	$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
	$(FC) $(FFLAGS) -c $(POSITIONING_SRC)
	$(FC) $(FFLAGS) -c $(CYN_DROOP_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_CYANOBACTERIA.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_cyanobacteria.f90 *.o

# Compile test_cyn_droop (nitrogen-quota helpers + the flag-gated branch in
# both CYANOBACTERIA variants). Same source set as test_cyanobacteria plus
# the AQUABC_CYN_DROOP module, which the library routines now use.
test_cyn_droop: test_cyn_droop.f90 $(TEST_DEFAULTS_SRC) $(AUX_ALL_SRCS) $(CYN_DROOP_SRC)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
	$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
	$(FC) $(FFLAGS) -c $(POSITIONING_SRC)
	$(FC) $(FFLAGS) -c $(CYN_DROOP_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_CYANOBACTERIA.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_cyn_droop.f90 *.o

# Compile test_zooplankton (kinetics test)
test_zooplankton: test_zooplankton.f90 $(TEST_DEFAULTS_SRC) $(AUX_ALL_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
	$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_ZOOPLANKTON.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_zooplankton.f90 *.o

# Compile test_redox (kinetics test)
test_redox: test_redox.f90 $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_DO_SATURATION.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_REDOX_AND_SPECIATION.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_redox.f90 *.o

# Compile test_doc_mineralization (kinetics test)
test_doc_mineralization: test_doc_mineralization.f90 $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_PH_CORR.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_ORGANIC_CARBON.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_doc_mineralization.f90 *.o

# Compile test_fix_cyn (kinetics test)
test_fix_cyn: test_fix_cyn.f90 $(TEST_DEFAULTS_SRC) $(AUX_ALL_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
	$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
	$(FC) $(FFLAGS) -c $(POSITIONING_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_fix_cyn.f90 *.o

# Compile test_opa (kinetics test)
test_opa: test_opa.f90 $(TEST_DEFAULTS_SRC) $(AUX_ALL_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
	$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_OTHER_PLANKTONIC_ALGAE.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_opa.f90 *.o

# Compile test_nostocales (kinetics test)
test_nostocales: test_nostocales.f90 $(TEST_DEFAULTS_SRC) $(AUX_ALL_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC)
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/STRING_UTILS.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_MODEL_CONSTANTS_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_TYPES_SRC)
	$(FC) $(FFLAGS) -c $(AUX_GLOBAL_DEPS)
	$(FC) $(FFLAGS) -c $(PELAGIC_AUX)
	$(FC) $(FFLAGS) -c $(POSITIONING_SRC)
	$(FC) $(FFLAGS) -c $(NOST_STAGING_SRC)
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_NOSTACALES.f90
	$(FC) $(FFLAGS) -c $(TEST_DEFAULTS_SRC)
	$(FC) $(FFLAGS) -o $@ test_nostocales.f90 *.o

# Compile test_nost_staging (bed akinete bank, EMA cue, latch; pure module,
# only needs mod_AQUABC_II_GLOBAL). Hermetic link (explicit objects, not
# *.o), mirroring test_sed_typemap/test_co2sys -- stale objects left in this
# shared dir by other targets cannot leak in.
test_nost_staging: test_nost_staging.f90 $(NOST_STAGING_SRC)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC) -o precision_kinds.o
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 -o mod_AQUABC_II_GLOBAL.o
	$(FC) $(FFLAGS) -c $(NOST_STAGING_SRC) -o aquabc_nost_staging.o
	$(FC) $(FFLAGS) -o $@ test_nost_staging.f90 aquabc_nost_staging.o mod_AQUABC_II_GLOBAL.o precision_kinds.o

# Compile test_bioturbation
test_bioturbation: test_bioturbation.f90 $(SEDIMENT_SRC)/aquabc_II_sediment_bioturbation.f90
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(SEDIMENT_SRC)/aquabc_II_sediment_bioturbation.f90
	$(FC) $(FFLAGS) -o $@ test_bioturbation.f90 *.o

# Compile test_co2sys (carbonate-system solver; module order matters)
test_co2sys: test_co2sys.f90 $(CO2SYS_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC) -o precision_kinds.o
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 -o mod_AQUABC_II_GLOBAL.o
	$(FC) $(FFLAGS) -c $(PHYS_CONST_SRC) -o aquabc_physical_constants.o
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/VECTOR_MATRIX_UTILS.f90 -o VECTOR_MATRIX_UTILS.o
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/CO2SYS/aquabc_II_co2sys.f90 -o aquabc_II_co2sys.o
	$(FC) $(FFLAGS) -o $@ test_co2sys.f90 aquabc_II_co2sys.o VECTOR_MATRIX_UTILS.o aquabc_physical_constants.o mod_AQUABC_II_GLOBAL.o precision_kinds.o

# Compile test_basin_level (ESTAS level-from-volume inversion, mod_BASIN)
BASIN_LEVEL_SRCS = $(PRECISION_KINDS_SRC) $(ESTAS_SRC)/mod_INTERPOLATE.f90 $(ESTAS_SRC)/mod_TIME_SERIES.f90 \
    $(AQUABC_SRC)/PELAGIC/aquabc_II_pelagic_svindex.f90 $(ESTAS_SRC)/mod_GLOBAL.f90 $(ESTAS_SRC)/mod_BASIN.f90
test_basin_level: test_basin_level.f90 $(BASIN_LEVEL_SRCS)
	$(FC) $(FFLAGS) -o $@ $(BASIN_LEVEL_SRCS) test_basin_level.f90

# Run all tests
.PHONY: test
test: all
	@echo ""
	@echo "=========================================="
	@echo "Running all Fortran unit tests"
	@echo "=========================================="
	@echo ""
	@failed=0; \
	for prog in $(TEST_PROGS); do \
		echo "--- Running $$prog ---"; \
		if ./$$prog; then \
			echo ""; \
		else \
			echo "*** $$prog FAILED ***"; \
			echo ""; \
			failed=1; \
		fi; \
	done; \
	echo "=========================================="; \
	if [ $$failed -eq 0 ]; then \
		echo "All tests PASSED"; \
	else \
		echo "Some tests FAILED"; \
		exit 1; \
	fi

# Run individual tests
.PHONY: run_do_saturation
run_do_saturation: test_do_saturation
	./test_do_saturation

.PHONY: run_chla
run_chla: test_chla
	./test_chla

.PHONY: run_para_aqua
run_para_aqua: test_para_aqua
	./test_para_aqua

# Clean up
.PHONY: clean
clean:
	rm -f $(TEST_PROGS) *.mod *.o

# Help
.PHONY: help
help:
	@echo "AQUABC Fortran Unit Tests Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  all              - Compile all test programs"
	@echo "  test             - Compile and run all tests"
	@echo "  test_do_saturation - Compile DO saturation test"
	@echo "  test_chla        - Compile CHLA test"
	@echo "  run_do_saturation  - Run DO saturation test"
	@echo "  run_chla           - Run CHLA test"
	@echo "  clean            - Remove compiled binaries"
	@echo "  help             - Show this help message"
