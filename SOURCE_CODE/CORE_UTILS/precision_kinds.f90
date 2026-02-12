! Canonical working-precision definition for the entire model.
! Every module that formerly defined its own selected_real_kind(15,307)
! now imports the appropriate alias from here.
module precision_kinds
    implicit none

    integer, parameter :: wp = selected_real_kind(15, 307)

    ! Legacy aliases — keep existing consumer code compiling unchanged
    integer, parameter :: DBL           = wp
    integer, parameter :: DBL_PREC      = wp
    integer, parameter :: DBL_ALLEL     = wp
    integer, parameter :: DBL_PRECISION = wp
end module precision_kinds
