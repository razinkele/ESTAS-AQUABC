! ---------------------------------------------------------------------------------------
! This subroutine callculates the derivatives for the second metabolites
!
!                                       Version 0.1
! ---------------------------------------------------------------------------------------
subroutine allelopathy_SEC_METABOLITES(nkn)

    use ALLELOPATHY

    implicit none
    integer, intent(in) :: nkn
    integer :: i


    R_FORM_SEC_METAB_DIA       = S_SEC_METAB_TO_DIA       * ALLEL_R_DEATH_DIA
    R_FORM_SEC_METAB_NOFIX_CYN = S_SEC_METAB_TO_NOFIX_CYN * ALLEL_R_DEATH_NOFIX_CYN
    R_FORM_SEC_METAB_FIX_CYN   = S_SEC_METAB_TO_FIX_CYN   * ALLEL_R_DEATH_FIX_CYN
    R_FORM_SEC_METAB_NOST      = S_SEC_METAB_TO_NOST      * ALLEL_R_DEATH_NOST

    R_DEG_SEC_METAB_DIA        = &
        k_DEG_SEC_METAB_DIA_20       * &
        (THETA_k_DEG_SEC_METAB_DIA       ** (WATER_TEMP - 20.0D0))

    R_DEG_SEC_METAB_NOFIX_CYN  = &
        k_DEG_SEC_METAB_NOFIX_CYN_20 * &
        (THETA_k_DEG_SEC_METAB_NOFIX_CYN ** (WATER_TEMP - 20.0D0))

    R_DEG_SEC_METAB_FIX_CYN    = &
        k_DEG_SEC_METAB_FIX_CYN_20   * &
        (THETA_k_DEG_SEC_METAB_FIX_CYN   ** (WATER_TEMP - 20.0D0))

    R_DEG_SEC_METAB_NOST       = &
        k_DEG_SEC_METAB_NOST_20      * &
        (THETA_k_DEG_SEC_METAB_NOST      ** (WATER_TEMP - 20.0D0))

    DERIVATIVES_SEC_METAB(:, 1) = R_FORM_SEC_METAB_DIA       - R_DEG_SEC_METAB_DIA
    DERIVATIVES_SEC_METAB(:, 2) = R_FORM_SEC_METAB_NOFIX_CYN - R_DEG_SEC_METAB_NOFIX_CYN
    DERIVATIVES_SEC_METAB(:, 3) = R_FORM_SEC_METAB_FIX_CYN   - R_DEG_SEC_METAB_FIX_CYN
    DERIVATIVES_SEC_METAB(:, 4) = R_FORM_SEC_METAB_NOST      - R_DEG_SEC_METAB_NOST

    ! Diagnostic: report unusually large form/deg/derivative for FIX_CYN second metabolites
    do i = 1, nkn
        if (ALLEL_R_DEATH_FIX_CYN(i) .ne. 0.0D0) then
            if (dabs(DERIVATIVES_SEC_METAB(i,3)) > 0.5D0 * dabs(ALLEL_R_DEATH_FIX_CYN(i))) then
                write(6,*) 'DEBUG: ALLELOP HT FIX_CYN node', i
                write(6,*) '  ALLEL_R_DEATH_FIX_CYN=', ALLEL_R_DEATH_FIX_CYN(i), ' R_FORM=', R_FORM_SEC_METAB_FIX_CYN(i)
                write(6,*) '  R_DEG=', R_DEG_SEC_METAB_FIX_CYN(i)
                write(6,*) '  DERIV=', DERIVATIVES_SEC_METAB(i,3)
            end if
        end if
    end do


end subroutine allelopathy_SEC_METABOLITES
