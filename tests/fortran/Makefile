# Makefile for AQUABC Fortran unit tests
#
# Usage:
#   make all      - Compile all tests
#   make test     - Compile and run all tests
#   make clean    - Remove compiled binaries
#   make test_do_saturation - Compile and run DO saturation tests
#   make test_chla          - Compile and run CHLA tests

# Compiler settings
FC = gfortran
FFLAGS = -O2 -Wall -Wextra -fcheck=all -fbacktrace

# Source directories
AQUABC_SRC = ../../SOURCE_CODE/AQUABC
PELAGIC_LIB = $(AQUABC_SRC)/PELAGIC/AQUABC_PELAGIC_LIBRARY
CORE_UTILS = ../../SOURCE_CODE/CORE_UTILS
ALLELOPATHY_SRC = ../../SOURCE_CODE/ALLELOPATHY
ALLELOPATHY_LIB = $(ALLELOPATHY_SRC)/ALLELOPATHY_LIBRARY

# Test executables
TEST_PROGS = test_do_saturation test_chla test_para_aqua test_settling test_ph_corr test_ip_soluble test_allelopathy test_iron_ii test_diss_me test_organic_carbon test_growth_temp test_nutrient_prefs test_vector_utils test_ammonia_chem test_light test_settling_suppres test_file_unit

# Source directories for additional modules
PELAGIC_AUX = $(AQUABC_SRC)/PELAGIC/aquabc_II_pelagic_auxillary.f90
PRECISION_KINDS_SRC = $(CORE_UTILS)/precision_kinds.f90

# Source dependencies for each test
DO_SAT_SRCS = $(PELAGIC_LIB)/aquabc_II_pelagic_lib_DO_SATURATION.f90
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
ORGANIC_CARBON_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 \
    $(PELAGIC_LIB)/aquabc_II_pelagic_lib_PH_CORR.f90 \
    $(PELAGIC_LIB)/aquabc_II_pelagic_lib_ORGANIC_CARBON.f90
GROWTH_TEMP_SRCS = $(PELAGIC_AUX)
NUTRIENT_PREFS_SRCS = $(PELAGIC_AUX)
VECTOR_UTILS_SRCS = $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90 $(CORE_UTILS)/VECTOR_MATRIX_UTILS.f90

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
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_PH_CORR.f90
	$(FC) $(FFLAGS) -c $(PELAGIC_LIB)/aquabc_II_pelagic_lib_ORGANIC_CARBON.f90
	$(FC) $(FFLAGS) -o $@ test_organic_carbon.f90 *.o

# Compile test_growth_temp (standalone - includes subroutine copy)
test_growth_temp: test_growth_temp.f90
	$(FC) $(FFLAGS) -o $@ test_growth_temp.f90

# Compile test_nutrient_prefs (standalone - includes subroutine copies)
test_nutrient_prefs: test_nutrient_prefs.f90
	$(FC) $(FFLAGS) -o $@ test_nutrient_prefs.f90

# Compile test_vector_utils
test_vector_utils: test_vector_utils.f90 $(VECTOR_UTILS_SRCS)
	$(FC) $(FFLAGS) -c $(PRECISION_KINDS_SRC)
	$(FC) $(FFLAGS) -c $(AQUABC_SRC)/mod_AQUABC_II_GLOBAL.f90
	$(FC) $(FFLAGS) -c $(CORE_UTILS)/VECTOR_MATRIX_UTILS.f90
	$(FC) $(FFLAGS) -o $@ test_vector_utils.f90 *.o

# Compile test_ammonia_chem (standalone - includes subroutine copies)
test_ammonia_chem: test_ammonia_chem.f90
	$(FC) $(FFLAGS) -o $@ test_ammonia_chem.f90

# Compile test_light (standalone - includes subroutine copies)
test_light: test_light.f90
	$(FC) $(FFLAGS) -o $@ test_light.f90

# Compile test_settling_suppres (standalone - includes subroutine copies)
test_settling_suppres: test_settling_suppres.f90
	$(FC) $(FFLAGS) -o $@ test_settling_suppres.f90

# Compile test_file_unit (standalone - includes subroutine copies)
test_file_unit: test_file_unit.f90
	$(FC) $(FFLAGS) -o $@ test_file_unit.f90

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
