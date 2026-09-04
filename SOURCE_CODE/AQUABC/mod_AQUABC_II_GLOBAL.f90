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

    ! Day-length weighting of the depth-averaged light limitation in LIM_LIGHT.
    ! I_A is a DAILY INTEGRAL (aquabc_II_pelagic_model.f90: W/m2 * 0.5 PAR *
    ! 86400 s * 0.238846 J->cal / 1e4 = langley/day), so a photoperiod
    ! correction can enter two ways:
    !
    !   0  legacy (default): no FDAY. The daily mean is applied for a full 24 h.
    !   1  Form A: LLIGHT = FDAY * f(I_A). Weights by the photoperiod without
    !      concentrating the dose into it, so it DISCARDS (1-FDAY) of each day's
    !      light -- 71 % of it in December. This is the form used by the
    !      smith == 0 branch of the phytoplankton library routines, where it is
    !      a genuine (if inert, since CL29 runs smith == 1) bug. Retained here
    !      only to reproduce the comparison in docs/CL29_phenology_diagnosis.md
    !      section 47. NOT for adoption.
    !   2  Form B: LLIGHT = FDAY * f(I_A / FDAY). Concentrates the daily dose
    !      into the photoperiod, then weights by it (WASP/EUTRO; cf. CUR_SMITH's
    !      IAV = 0.9 * ITOT / FDAY). Correct: while light-limited the P-I curve
    !      is near-linear, so the FDAY cancels -- a fixed daily dose spread over
    !      more or fewer hours cannot change a near-linear daily integral. Form B
    !      therefore departs from option 0 only through P-I curvature, i.e. the
    !      real penalty for receiving the same dose at higher intensity.
    !
    ! Measured over the CL29 record (doc section 47.2): Form A -60.5 % February /
    ! -33.5 % May (a 1.68x differential); Form B -22.1 % / -19.0 % (1.04x, an
    ! offset). ESTAS sets this from PELAGIC_MODEL_OPTIONS.txt.
    integer :: LIGHT_DAYLENGTH_OPTION = 0
end module AQUABC_II_GLOBAL
