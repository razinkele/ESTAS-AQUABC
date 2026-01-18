
BASELIB = ../../..
BUILDDIR = $(BASELIB)/build
DIRLIB = $(BUILDDIR)
LIBAQUABC = aquabc
LIBLABC   = -L$(DIRLIB) -l$(LIBAQUABC)
LIBFABC  = $(DIRLIB)/lib$(LIBAQUABC).a

OBJS = aquabc_II_pelagic_interface.o
OBJS = 
EXES = aquabc_II_pelagic_0D
EXES = aquabc_II_pelagic_0D aquabc_II_pelagic_0D_old aquabc_II_pelagic_0D_orig

F77 = gfortran
FFLAGS = 
FFLAGS = -fcheck=all -g
FFLAGS = -O

#-----------------------------------------------------------
#
# aquabc_II_pelagic_0D_orig	original version from Ali
# aquabc_II_pelagic_0D_old	reformatted version
# aquabc_II_pelagic_0D		new version with interface
#
#-----------------------------------------------------------

default:
	@echo "possible targets:"
	@echo "  all              same as build aquabc0D"
	@echo "  aquabc0D         compiles acquabc executable"
	@echo "  build            builds new library unconditionally"
	@echo "  run              runs 0D model"
	@echo "  compare          compares output to original one"
	@echo "  plot             plots model results (needs SHYFEM utils)"

#-----------------------------------------------------------

all: build aquabc0D

aquabc0D: lib aquabc_II_pelagic_0D
aquabc_II_pelagic_0D: aquabc_II_pelagic_0D.o $(OBJS) $(LIBFABC)
	$(F77) $@.o $(OBJS) $(LIBLABC) -o $@

orig: aquabc_II_pelagic_0D_orig
aquabc_II_pelagic_0D_orig: aquabc_II_pelagic_0D_orig.o $(LIBFABC)
	$(F77) $@.o $(LIBLABC) -o $@

old: aquabc_II_pelagic_0D_old
aquabc_II_pelagic_0D_old: aquabc_II_pelagic_0D_old.o $(LIBFABC)
	$(F77) $@.o $(LIBLABC) -o $@

build:
	cd $(BUILDDIR); make lib

lib: $(LIBFABC)
$(LIBFABC):
	cd $(BUILDDIR); make lib

run: aquabc0D
	./aquabc_II_pelagic_0D

# Run 0D regression test: fails on NaNs or negative CHLA
test: aquabc0D
	./test/test_0D_no_nan.sh
	./test/test_0D_clamp_summary.sh
	# run unit-level limiter test (directly exercises limiter internals)
	make test-unit-limiter

# Unit test that directly exercises limiter internals
test-unit-limiter:
	$(F77) -I$(BUILDDIR) -c ../../PELAGIC/aquabc_II_pelagic_limiter_test_util.f90 -o pelagic_limiter_test_util.o
	$(F77) -I$(BUILDDIR) test/unit_test_limiter.f90 pelagic_limiter_test_util.o -o unit_test_limiter
	./unit_test_limiter

compare:
	cmp OUTPUT.csv data/OUTPUT_new.csv

plot:
	cd plot; ./plot.sh

#-----------------------------------------------------------

clean:
	-rm -f $(EXES)
	-rm -f a.out *.exe
	-rm -f *.o *.mod
	-rm -f OUTPUT.csv
	-rm -f const_out.txt
	-rm -f col.[0-9]*
	-rm -f orig.[0-9]*
	-rm -f plot.[0-9]*.txt
	-rm -f orig.[0-9]*.txt
	-rm -f *.tmp *.bak *.ps *.pdf

cleanall: clean
	cd plot; make clean

#-----------------------------------------------------------

.PHONY: copy clean plot

.SUFFIXES: .F90 .f90

.f90.o:
	$(F77) -c $(FFLAGS) $<

#-----------------------------------------------------------

aquabc_II_pelagic_0D_orig.o: pelagic_model_constants.mod
aquabc_II_pelagic_0D_old.o: pelagic_model_constants.mod
pelagic_model_constants.mod: $(BUILDDIR)/pelagic_model_constants.mod
	cp $< .

#-----------------------------------------------------------

