! ---------------------------------------------------------------------------------------
! This subroutine callculates the derivatives for the second metabolites
!
!                                       Version 0.1
! ---------------------------------------------------------------------------------------
subroutine allelopathy_SEC_METABOLITES(nkn)

    use ALLELOPATHY

    implicit none
    integer, intent(in) :: nkn


    R_FORM_SEC_METAB_DIA       = S_SEC_METAB_TO_DIA       * ALLEL_R_DEATH_DIA
    R_FORM_SEC_METAB_NOFIX_CYN = S_SEC_METAB_TO_NOFIX_CYN * ALLEL_R_DEATH_NOFIX_CYN
    R_FORM_SEC_METAB_FIX_CYN   = S_SEC_METAB_TO_FIX_CYN   * ALLEL_R_DEATH_FIX_CYN
    R_FORM_SEC_METAB_NOST      = S_SEC_METAB_TO_NOST      * ALLEL_R_DEATH_NOST

    ! BUG FIX: Degradation rate must be proportional to concentration (first-order kinetics)
    ! Previously R_DEG was just k*theta^(T-20), missing the concentration term.
    ! Correct form: R_DEG = k * theta^(T-20) * SEC_METAB_CONCENTRATION
    R_DEG_SEC_METAB_DIA        = &
        k_DEG_SEC_METAB_DIA_20       * &
        (THETA_k_DEG_SEC_METAB_DIA       ** (WATER_TEMP - 20.0D0)) * SEC_METAB_DIA

    R_DEG_SEC_METAB_NOFIX_CYN  = &
        k_DEG_SEC_METAB_NOFIX_CYN_20 * &
        (THETA_k_DEG_SEC_METAB_NOFIX_CYN ** (WATER_TEMP - 20.0D0)) * SEC_METAB_NOFIX_CYN

    R_DEG_SEC_METAB_FIX_CYN    = &
        k_DEG_SEC_METAB_FIX_CYN_20   * &
        (THETA_k_DEG_SEC_METAB_FIX_CYN   ** (WATER_TEMP - 20.0D0)) * SEC_METAB_FIX_CYN

    R_DEG_SEC_METAB_NOST       = &
        k_DEG_SEC_METAB_NOST_20      * &
        (THETA_k_DEG_SEC_METAB_NOST      ** (WATER_TEMP - 20.0D0)) * SEC_METAB_NOST

    DERIVATIVES_SEC_METAB(:, 1) = R_FORM_SEC_METAB_DIA       - R_DEG_SEC_METAB_DIA
    DERIVATIVES_SEC_METAB(:, 2) = R_FORM_SEC_METAB_NOFIX_CYN - R_DEG_SEC_METAB_NOFIX_CYN
    DERIVATIVES_SEC_METAB(:, 3) = R_FORM_SEC_METAB_FIX_CYN   - R_DEG_SEC_METAB_FIX_CYN
    DERIVATIVES_SEC_METAB(:, 4) = R_FORM_SEC_METAB_NOST      - R_DEG_SEC_METAB_NOST

end subroutine allelopathy_SEC_METABOLITES
