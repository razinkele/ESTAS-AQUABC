module AQUABC_II_GLOBAL
    use precision_kinds, only: DBL_PREC
    implicit none

    ! Temperature-response model selected at runtime by the driver.
    ! .false. = piecewise plateau (default); .true. = CTMI (Rosso et al. 1993).
    ! GROWTH_AT_TEMP reads this; ESTAS sets it from PELAGIC_MODEL_OPTIONS.txt.
    logical :: USE_CTMI_TEMP = .false.
end module AQUABC_II_GLOBAL