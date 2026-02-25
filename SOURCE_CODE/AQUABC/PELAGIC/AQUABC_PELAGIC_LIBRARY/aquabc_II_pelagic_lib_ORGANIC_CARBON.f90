! Auxilary routines for the pelagic model

! Contents:

!subroutine ORGANIC_CARBON_DISSOLUTION
!subroutine ORGANIC_CARBON_MINERALIZATION

subroutine ORGANIC_CARBON_DISSOLUTION &
           (FAC_PHYT_DET_PART_ORG_C     , &
            KDISS_DET_PART_ORG_C_20     , &
            THETA_KDISS_DET_PART_ORG_C  , &
            KHS_POC_DISS_SAT            , &
            nkn                         , &
            TEMP                        , &
            DET_PART_ORG_C              , &
            PHYT_TOT_C                  , &
            LIM_PHYT_DISS_DET_PART_ORG_C, &
            R_DET_PART_ORG_C_DISSOLUTION)

    use AQUABC_II_GLOBAL
    implicit none

    real(kind = DBL_PREC), intent(in) :: FAC_PHYT_DET_PART_ORG_C
    real(kind = DBL_PREC), intent(in) :: KDISS_DET_PART_ORG_C_20
    real(kind = DBL_PREC), intent(in) :: THETA_KDISS_DET_PART_ORG_C
    real(kind = DBL_PREC), intent(in) :: KHS_POC_DISS_SAT

    integer, intent(in) :: nkn

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DET_PART_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PHYT_TOT_C

    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_PHYT_DISS_DET_PART_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_DET_PART_ORG_C_DISSOLUTION

    !Algal dependent hydrolysis rate
    LIM_PHYT_DISS_DET_PART_ORG_C = FAC_PHYT_DET_PART_ORG_C * PHYT_TOT_C

    R_DET_PART_ORG_C_DISSOLUTION = &
           (KDISS_DET_PART_ORG_C_20 + LIM_PHYT_DISS_DET_PART_ORG_C) * &
           (THETA_KDISS_DET_PART_ORG_C ** (TEMP - 2.0D1)) * DET_PART_ORG_C * &
           (KHS_POC_DISS_SAT/(DET_PART_ORG_C + KHS_POC_DISS_SAT))
end subroutine ORGANIC_CARBON_DISSOLUTION



subroutine ORGANIC_CARBON_MINERALIZATION &
           (nkn                         , &
            TEMP                        , &
            PH                          , &
            PHYT_TOT_C                  , &
            docmin_params               , &
            redox_params                , &
            redox_state                 , &
            redox_lim                   , &
            docmin_outputs)

    ! ----------------------------------------------------------------------------------------
    ! Subroutine for organic carbon mineraliztion
    ! This subroutine is almost completely rewritten to be compitable with the redox sequences
    ! ----------------------------------------------------------------------------------------
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES, only: t_docmin_params, t_redox_params, &
                                    t_redox_lim, t_redox_state, t_docmin_outputs
    implicit none

    integer, intent(in) :: nkn

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PH
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PHYT_TOT_C

    type(t_docmin_params),  intent(in)    :: docmin_params
    type(t_redox_params),   intent(in)    :: redox_params
    type(t_redox_state),    intent(in)    :: redox_state
    type(t_redox_lim),      intent(in)    :: redox_lim
    type(t_docmin_outputs), intent(inout) :: docmin_outputs

    associate( &
        FAC_PHYT_AMIN_DOC        => docmin_params%FAC_PHYT_AMIN_DOC,        &
        K_MIN_DOC_DOXY_20        => docmin_params%K_MIN_DOC_DOXY_20,        &
        K_MIN_DOC_NO3N_20        => docmin_params%K_MIN_DOC_NO3N_20,        &
        K_MIN_DOC_MN_IV_20       => docmin_params%K_MIN_DOC_MN_IV_20,       &
        K_MIN_DOC_FE_III_20      => docmin_params%K_MIN_DOC_FE_III_20,      &
        K_MIN_DOC_S_PLUS_6_20    => docmin_params%K_MIN_DOC_S_PLUS_6_20,    &
        K_MIN_DOC_DOC_20         => docmin_params%K_MIN_DOC_DOC_20,         &
        THETA_K_MIN_DOC_DOXY     => docmin_params%THETA_K_MIN_DOC_DOXY,     &
        THETA_K_MIN_DOC_NO3N     => docmin_params%THETA_K_MIN_DOC_NO3N,     &
        THETA_K_MIN_DOC_MN_IV    => docmin_params%THETA_K_MIN_DOC_MN_IV,    &
        THETA_K_MIN_DOC_FE_III   => docmin_params%THETA_K_MIN_DOC_FE_III,   &
        THETA_K_MIN_DOC_S_PLUS_6 => docmin_params%THETA_K_MIN_DOC_S_PLUS_6, &
        THETA_K_MIN_DOC_DOC      => docmin_params%THETA_K_MIN_DOC_DOC,      &
        K_HS_DOC_MIN_DOXY        => docmin_params%K_HS_DOC_MIN_DOXY,        &
        K_HS_DOC_MIN_NO3N        => docmin_params%K_HS_DOC_MIN_NO3N,        &
        K_HS_DOC_MIN_MN_IV       => docmin_params%K_HS_DOC_MIN_MN_IV,       &
        K_HS_DOC_MIN_FE_III      => docmin_params%K_HS_DOC_MIN_FE_III,      &
        K_HS_DOC_MIN_S_PLUS_6    => docmin_params%K_HS_DOC_MIN_S_PLUS_6,    &
        K_HS_DOC_MIN_DOC         => docmin_params%K_HS_DOC_MIN_DOC,         &
        PH_MIN_DOC_MIN_DOXY      => docmin_params%PH_MIN_DOC_MIN_DOXY,      &
        PH_MIN_DOC_MIN_NO3N      => docmin_params%PH_MIN_DOC_MIN_NO3N,      &
        PH_MIN_DOC_MIN_MN_IV     => docmin_params%PH_MIN_DOC_MIN_MN_IV,     &
        PH_MIN_DOC_MIN_FE_III    => docmin_params%PH_MIN_DOC_MIN_FE_III,    &
        PH_MIN_DOC_MIN_S_PLUS_6  => docmin_params%PH_MIN_DOC_MIN_S_PLUS_6,  &
        PH_MIN_DOC_MIN_DOC       => docmin_params%PH_MIN_DOC_MIN_DOC,       &
        PH_MAX_DOC_MIN_DOXY      => docmin_params%PH_MAX_DOC_MIN_DOXY,      &
        PH_MAX_DOC_MIN_NO3N      => docmin_params%PH_MAX_DOC_MIN_NO3N,      &
        PH_MAX_DOC_MIN_MN_IV     => docmin_params%PH_MAX_DOC_MIN_MN_IV,     &
        PH_MAX_DOC_MIN_FE_III    => docmin_params%PH_MAX_DOC_MIN_FE_III,    &
        PH_MAX_DOC_MIN_S_PLUS_6  => docmin_params%PH_MAX_DOC_MIN_S_PLUS_6,  &
        PH_MAX_DOC_MIN_DOC       => docmin_params%PH_MAX_DOC_MIN_DOC        &
    )

    associate( &
        K_HS_DOXY_RED_LIM      => redox_params%K_HS_DOXY_RED_LIM,      &
        K_HS_NO3N_RED_LIM      => redox_params%K_HS_NO3N_RED_LIM,      &
        K_HS_MN_IV_RED_LIM     => redox_params%K_HS_MN_IV_RED_LIM,     &
        K_HS_FE_III_RED_LIM    => redox_params%K_HS_FE_III_RED_LIM,    &
        K_HS_S_PLUS_6_RED_LIM  => redox_params%K_HS_S_PLUS_6_RED_LIM,  &
        K_HS_DOXY_RED_INHB     => redox_params%K_HS_DOXY_RED_INHB,     &
        K_HS_NO3N_RED_INHB     => redox_params%K_HS_NO3N_RED_INHB,     &
        K_HS_MN_IV_RED_INHB    => redox_params%K_HS_MN_IV_RED_INHB,    &
        K_HS_FE_III_RED_INHB   => redox_params%K_HS_FE_III_RED_INHB,   &
        K_HS_S_PLUS_6_RED_INHB => redox_params%K_HS_S_PLUS_6_RED_INHB  &
    )

    associate( &
        DOXY       => redox_state%DOXY,       &
        NO3N       => redox_state%NO3N,       &
        MN_IV      => redox_state%MN_IV,      &
        FE_III     => redox_state%FE_III,     &
        S_PLUS_6   => redox_state%S_PLUS_6,   &
        DISS_ORG_C => redox_state%DISS_ORG_C  &
    )

    associate( &
        LIM_DOXY_RED     => redox_lim%LIM_DOXY_RED,     &
        LIM_NO3N_RED     => redox_lim%LIM_NO3N_RED,     &
        LIM_MN_IV_RED    => redox_lim%LIM_MN_IV_RED,    &
        LIM_FE_III_RED   => redox_lim%LIM_FE_III_RED,   &
        LIM_S_PLUS_6_RED => redox_lim%LIM_S_PLUS_6_RED, &
        LIM_DOC_RED      => redox_lim%LIM_DOC_RED       &
    )

    associate( &
        LIM_PHYT_AMIN_DOC          => docmin_outputs%LIM_PHYT_AMIN_DOC,          &
        R_ABIOTIC_DOC_MIN_DOXY     => docmin_outputs%R_ABIOTIC_DOC_MIN_DOXY,     &
        R_ABIOTIC_DOC_MIN_NO3N     => docmin_outputs%R_ABIOTIC_DOC_MIN_NO3N,     &
        R_ABIOTIC_DOC_MIN_MN_IV    => docmin_outputs%R_ABIOTIC_DOC_MIN_MN_IV,    &
        R_ABIOTIC_DOC_MIN_FE_III   => docmin_outputs%R_ABIOTIC_DOC_MIN_FE_III,   &
        R_ABIOTIC_DOC_MIN_S_PLUS_6 => docmin_outputs%R_ABIOTIC_DOC_MIN_S_PLUS_6, &
        R_ABIOTIC_DOC_MIN_DOC      => docmin_outputs%R_ABIOTIC_DOC_MIN_DOC,      &
        PH_CORR_DOC_MIN_DOXY       => docmin_outputs%PH_CORR_DOC_MIN_DOXY,       &
        PH_CORR_DOC_MIN_NO3N       => docmin_outputs%PH_CORR_DOC_MIN_NO3N,       &
        PH_CORR_DOC_MIN_MN_IV      => docmin_outputs%PH_CORR_DOC_MIN_MN_IV,      &
        PH_CORR_DOC_MIN_FE_III     => docmin_outputs%PH_CORR_DOC_MIN_FE_III,     &
        PH_CORR_DOC_MIN_S_PLUS_6   => docmin_outputs%PH_CORR_DOC_MIN_S_PLUS_6,   &
        PH_CORR_DOC_MIN_DOC        => docmin_outputs%PH_CORR_DOC_MIN_DOC         &
    )

    LIM_PHYT_AMIN_DOC = FAC_PHYT_AMIN_DOC * PHYT_TOT_C

    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_DOXY    , PH, PH_MIN_DOC_MIN_DOXY    , PH_MAX_DOC_MIN_DOXY    , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_NO3N    , PH, PH_MIN_DOC_MIN_NO3N    , PH_MAX_DOC_MIN_NO3N    , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_MN_IV   , PH, PH_MIN_DOC_MIN_MN_IV   , PH_MAX_DOC_MIN_MN_IV   , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_FE_III  , PH, PH_MIN_DOC_MIN_FE_III  , PH_MAX_DOC_MIN_FE_III  , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_S_PLUS_6, PH, PH_MIN_DOC_MIN_S_PLUS_6, PH_MAX_DOC_MIN_S_PLUS_6, nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_DOC     , PH, PH_MIN_DOC_MIN_DOC     , PH_MAX_DOC_MIN_DOC     , nkn)

    R_ABIOTIC_DOC_MIN_DOXY = &
        (K_MIN_DOC_DOXY_20 + LIM_PHYT_AMIN_DOC) * (THETA_K_MIN_DOC_DOXY ** (TEMP - 2.0D1)) * &
        LIM_DOXY_RED * PH_CORR_DOC_MIN_DOXY * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_DOXY)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_NO3N = &
        K_MIN_DOC_NO3N_20  * (THETA_K_MIN_DOC_NO3N ** (TEMP - 2.0D1)) * &
        LIM_NO3N_RED * PH_CORR_DOC_MIN_NO3N * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_NO3N)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_MN_IV = &
        K_MIN_DOC_MN_IV_20  * (THETA_K_MIN_DOC_MN_IV ** (TEMP - 2.0D1)) * &
        LIM_MN_IV_RED * PH_CORR_DOC_MIN_MN_IV * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_MN_IV)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_FE_III = &
        K_MIN_DOC_FE_III_20  * (THETA_K_MIN_DOC_FE_III ** (TEMP - 2.0D1)) * &
        LIM_FE_III_RED * PH_CORR_DOC_MIN_FE_III * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_FE_III)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_S_PLUS_6 = &
        K_MIN_DOC_S_PLUS_6_20  * (THETA_K_MIN_DOC_S_PLUS_6 ** (TEMP - 2.0D1)) * &
        LIM_S_PLUS_6_RED * PH_CORR_DOC_MIN_S_PLUS_6 * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_S_PLUS_6)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_DOC = &
        (K_MIN_DOC_DOC_20  * (THETA_K_MIN_DOC_DOC ** (TEMP - 2.0D1)) * &
         LIM_DOC_RED * PH_CORR_DOC_MIN_DOC * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_DOC)) * DISS_ORG_C)

    end associate ! docmin_outputs
    end associate ! redox_lim
    end associate ! redox_state
    end associate ! redox_params
    end associate ! docmin_params

end subroutine ORGANIC_CARBON_MINERALIZATION
