subroutine ALLELOPATHY_INHIBITION_RATES()

    use ALLELOPATHY
    implicit none
    integer :: i


    IHBF_SEC_METAB_DIA_NOFIX_CYN     = &
        K_HS_SEC_METAB_DIA_NOFIX_CYN     / &
        (K_HS_SEC_METAB_DIA_NOFIX_CYN     + SEC_METAB_DIA      )

    IHBF_SEC_METAB_DIA_FIX_CYN       = &
        K_HS_SEC_METAB_DIA_FIX_CYN       / &
        (K_HS_SEC_METAB_DIA_FIX_CYN       + SEC_METAB_DIA      )

    IHBF_SEC_METAB_DIA_NOST          = &
        K_HS_SEC_METAB_DIA_NOST          / &
        (K_HS_SEC_METAB_DIA_NOST          + SEC_METAB_DIA      )

    IHBF_SEC_METAB_DIA_OPA           = &
        K_HS_SEC_METAB_DIA_OPA           / &
        (K_HS_SEC_METAB_DIA_OPA           + SEC_METAB_DIA      )

    IHBF_SEC_METAB_DIA_ZOO           = &
        K_HS_SEC_METAB_DIA_ZOO           / &
        (K_HS_SEC_METAB_DIA_ZOO           + SEC_METAB_DIA      )

    IHBF_SEC_METAB_NOFIX_CYN_DIA     = &
        K_HS_SEC_METAB_NOFIX_CYN_DIA     / &
        (K_HS_SEC_METAB_NOFIX_CYN_DIA     + SEC_METAB_NOFIX_CYN)

    IHBF_SEC_METAB_NOFIX_CYN_FIX_CYN = &
        K_HS_SEC_METAB_NOFIX_CYN_FIX_CYN / &
        (K_HS_SEC_METAB_NOFIX_CYN_FIX_CYN + SEC_METAB_NOFIX_CYN)

    IHBF_SEC_METAB_NOFIX_CYN_NOST    = &
        K_HS_SEC_METAB_NOFIX_CYN_NOST    / &
        (K_HS_SEC_METAB_NOFIX_CYN_NOST    + SEC_METAB_NOFIX_CYN)

    IHBF_SEC_METAB_NOFIX_CYN_OPA     = &
        K_HS_SEC_METAB_NOFIX_CYN_OPA     / &
        (K_HS_SEC_METAB_NOFIX_CYN_OPA     + SEC_METAB_NOFIX_CYN)

    IHBF_SEC_METAB_NOFIX_CYN_ZOO     = &
        K_HS_SEC_METAB_NOFIX_CYN_ZOO     / &
        (K_HS_SEC_METAB_NOFIX_CYN_ZOO     + SEC_METAB_NOFIX_CYN)

    IHBF_SEC_METAB_FIX_CYN_DIA       = &
        K_HS_SEC_METAB_FIX_CYN_DIA       / &
        (K_HS_SEC_METAB_FIX_CYN_DIA       + SEC_METAB_FIX_CYN  )

    IHBF_SEC_METAB_FIX_CYN_NOFIX_CYN = &
        K_HS_SEC_METAB_FIX_CYN_NOFIX_CYN / &
        (K_HS_SEC_METAB_FIX_CYN_NOFIX_CYN + SEC_METAB_FIX_CYN  )

    IHBF_SEC_METAB_FIX_CYN_NOST      = &
        K_HS_SEC_METAB_FIX_CYN_NOST      / &
        (K_HS_SEC_METAB_FIX_CYN_NOST      + SEC_METAB_FIX_CYN  )

    IHBF_SEC_METAB_FIX_CYN_OPA       = &
        K_HS_SEC_METAB_FIX_CYN_OPA       / &
        (K_HS_SEC_METAB_FIX_CYN_OPA       + SEC_METAB_FIX_CYN  )

    IHBF_SEC_METAB_FIX_CYN_ZOO       = &
        K_HS_SEC_METAB_FIX_CYN_ZOO       / &
        (K_HS_SEC_METAB_FIX_CYN_ZOO       + SEC_METAB_FIX_CYN  )

    IHBF_SEC_METAB_NOST_DIA          = &
        K_HS_SEC_METAB_NOST_DIA          / &
        (K_HS_SEC_METAB_NOST_DIA          + SEC_METAB_NOST     )

    IHBF_SEC_METAB_NOST_NOFIX_CYN    = &
        K_HS_SEC_METAB_NOST_NOFIX_CYN    / &
        (K_HS_SEC_METAB_NOST_NOFIX_CYN    + SEC_METAB_NOST     )

    IHBF_SEC_METAB_NOST_FIX_CYN      = &
       K_HS_SEC_METAB_NOST_FIX_CYN      / &
       (K_HS_SEC_METAB_NOST_FIX_CYN      + SEC_METAB_NOST      )

    IHBF_SEC_METAB_NOST_OPA          = &
        K_HS_SEC_METAB_NOST_OPA          / &
        (K_HS_SEC_METAB_NOST_OPA          + SEC_METAB_NOST     )

    IHBF_SEC_METAB_NOST_ZOO          = &
        K_HS_SEC_METAB_NOST_ZOO          / &
        (K_HS_SEC_METAB_NOST_ZOO          + SEC_METAB_NOST     )

    IHBF_SEC_METAB_DIA       = &
        min(IHBF_SEC_METAB_NOFIX_CYN_DIA, IHBF_SEC_METAB_FIX_CYN_DIA      , &
            IHBF_SEC_METAB_NOST_DIA      )

    IHBF_SEC_METAB_NOFIX_CYN = &
        min(IHBF_SEC_METAB_DIA_NOFIX_CYN, IHBF_SEC_METAB_FIX_CYN_NOFIX_CYN, &
            IHBF_SEC_METAB_NOST_NOFIX_CYN)

    IHBF_SEC_METAB_FIX_CYN   = &
        min(IHBF_SEC_METAB_DIA_FIX_CYN  , IHBF_SEC_METAB_NOFIX_CYN_FIX_CYN, &
            IHBF_SEC_METAB_NOST_FIX_CYN  )

    ! Diagnostic: report strong inhibition (IHBF small) for FIX_CYN
    do i = 1, size(IHBF_SEC_METAB_FIX_CYN)
        if (IHBF_SEC_METAB_FIX_CYN(i) < 0.1D0) then
            write(6,*) 'DEBUG: ALLELOP INHIB FIX_CYN node', i, ' IHBF=', IHBF_SEC_METAB_FIX_CYN(i)
            write(6,*) '  SEC_MET_FIX=', SEC_METAB_FIX_CYN(i)
        end if
    end do


    IHBF_SEC_METAB_NOST      = &
        min(IHBF_SEC_METAB_DIA_NOST     , IHBF_SEC_METAB_NOFIX_CYN_NOST   , &
            IHBF_SEC_METAB_FIX_CYN_NOST  )

    IHBF_SEC_METAB_OPA       = &
        min(IHBF_SEC_METAB_DIA_OPA      , IHBF_SEC_METAB_NOFIX_CYN_OPA    , &
           IHBF_SEC_METAB_FIX_CYN_OPA  ,  IHBF_SEC_METAB_NOST_OPA)

    IHBF_SEC_METAB_ZOO       = &
        min(IHBF_SEC_METAB_DIA_ZOO      , IHBF_SEC_METAB_NOFIX_CYN_ZOO    , &
            IHBF_SEC_METAB_FIX_CYN_ZOO  , IHBF_SEC_METAB_NOST_ZOO )

end subroutine ALLELOPATHY_INHIBITION_RATES
