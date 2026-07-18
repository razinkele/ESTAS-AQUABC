module AQUABC_II_GLOBAL
    use precision_kinds, only: DBL_PREC
    implicit none

    ! Temperature-response model selected at runtime by the driver.
    ! .false. = piecewise plateau (default); .true. = CTMI (Rosso et al. 1993).
    ! GROWTH_AT_TEMP reads this; ESTAS sets it from PELAGIC_MODEL_OPTIONS.txt.
    logical :: USE_CTMI_TEMP = .false.

    ! FePO4 solubility product (log10 Ksp) used by IP_SOLUBLE_FRACTION for the
    ! water-column iron-phosphate binding. Default -26.4 = crystalline FePO4
    ! (Stumm & Morgan 1996); a larger (less negative) value makes FePO4 more
    ! soluble (weaker P-binding). ESTAS sets this from PELAGIC_MODEL_OPTIONS.txt.
    real(kind = DBL_PREC) :: FEPO4_KSP_LOG10 = -26.4_DBL_PREC
end module AQUABC_II_GLOBAL
