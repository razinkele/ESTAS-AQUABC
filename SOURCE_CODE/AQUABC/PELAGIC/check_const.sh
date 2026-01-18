#!/bin/sh
#
# checks the compatibility of model constants
# should be run after any changes to the constants have been made
#
#---------------------------------------------------------------

file=~/work/aquabc/ESTAS_AQUABC/modelCore/AQUABC/PELAGIC
file=aquabc_II_pelagic_model_constants.f90

#---------------------------------------------------------------

./check_const.pl $file

#---------------------------------------------------------------

