! Auxilary routines for the pelagic model

! Contents:
!subroutine FIX_CYANOBACTERIA

subroutine FIX_CYANOBACTERIA  &
           (KG_FIX_CYN_OPT_TEMP          , &
            FIX_CYN_OPT_TEMP_LR          , &
            FIX_CYN_OPT_TEMP_UR          , &
            EFF_FIX_CYN_GROWTH           , &
            KAPPA_FIX_CYN_UNDER_OPT_TEMP , &
            KAPPA_FIX_CYN_OVER_OPT_TEMP  , &
            KR_FIX_CYN_20                , &
            THETA_KR_FIX_CYN             , &
            KD_FIX_CYN_20                , &
            THETA_KD_FIX_CYN             , &
            KHS_DIN_FIX_CYN              , &
            KHS_DIP_FIX_CYN              , &
            KHS_O2_FIX_CYN               , &
            I_S_FIX_CYN                  , &
            DO_STR_HYPOX_FIX_CYN_D       , &
            THETA_HYPOX_FIX_CYN_D        , &
            EXPON_HYPOX_FIX_CYN_D        , &
            FIX_CYN_N_TO_C               , &
            FIX_CYN_P_TO_C               , &
            FIX_CYN_O2_TO_C              , &
            FIX_CYN_C_TO_CHLA            , &
            FIX_CYN_LIGHT_SAT            , &
            FRAC_FIX_CYN_EXCR            , &
            R_FIX                        , &
            K_FIX                        , &
            TIME_STEP                    , &
            SMITH                        , &
            frac_avail_DON               , &
            nkn                          , &
            FDAY                         , &
            I_A                          , &
            K_E                          , &
            DEPTH                        , &
            CHLA                         , &
            TEMP                         , &
            NH4_N                        , &
            NO3_N                        , &
            DON                          , &
            PO4_P                        , &
            DISS_OXYGEN                  , &
            FIX_CYN_C                    , &
            ALPHA_0                      , &
            ALPHA_1                      , &
            KG_FIX_CYN                   , &
            LIM_KG_FIX_CYN_LIGHT         , &
            LIM_KG_FIX_CYN_TEMP          , &
            LIM_KG_FIX_CYN_DOXY          , &
            LIM_KG_NON_FIX_CYN_N         , &
            LIM_KG_NON_FIX_CYN_P         , &
            LIM_KG_NON_FIX_CYN_NUTR      , &
            LIM_KG_FIX_FIX_CYN_N         , &
            LIM_KG_FIX_FIX_CYN_P         , &
            LIM_KG_FIX_FIX_CYN_NUTR      , &
            LIM_KG_NON_FIX_CYN           , &
            LIM_KG_FIX_FIX_CYN           , &
            R_NON_FIX_CYN_GROWTH         , &
            R_FIX_FIX_CYN_GROWTH         , &
            R_FIX_CYN_GROWTH             , &
            R_FIX_CYN_MET                , &
            R_FIX_CYN_RESP               , &
            R_FIX_CYN_EXCR               , &
            R_FIX_CYN_INT_RESP           , &
            KD_FIX_CYN                   , &
            FAC_HYPOX_FIX_CYN_D          , &
            R_FIX_CYN_DEATH              , &
            PREF_NH4N_DON_FIX_CYN)

    use AQUABC_II_GLOBAL
    implicit none

    ! -------------------------------------------------------------------------
    ! Ingoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), intent(in) :: KG_FIX_CYN_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_OPT_TEMP_LR
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_OPT_TEMP_UR
    real(kind = DBL_PREC), intent(in) :: EFF_FIX_CYN_GROWTH
    real(kind = DBL_PREC), intent(in) :: KAPPA_FIX_CYN_UNDER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KAPPA_FIX_CYN_OVER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KR_FIX_CYN_20
    real(kind = DBL_PREC), intent(in) :: THETA_KR_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: KD_FIX_CYN_20
    real(kind = DBL_PREC), intent(in) :: THETA_KD_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: KHS_DIN_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: KHS_DIP_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: KHS_O2_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: I_S_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: DO_STR_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), intent(in) :: THETA_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), intent(in) :: EXPON_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_N_TO_C
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_P_TO_C
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_O2_TO_C
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_C_TO_CHLA
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FIX_CYN_LIGHT_SAT
    real(kind = DBL_PREC), intent(in) :: FRAC_FIX_CYN_EXCR
    real(kind = DBL_PREC), intent(in) :: R_FIX
    real(kind = DBL_PREC), intent(in) :: K_FIX

    real(kind = DBL_PREC), intent(in) :: TIME_STEP
    integer, intent(in) :: SMITH
    integer, intent(in) :: nkn
!
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FDAY
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: I_A
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: K_E
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DEPTH
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: CHLA
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NH4_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3_N
    real(kind = DBL_PREC), intent(in) :: frac_avail_DON
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DON
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PO4_P
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DISS_OXYGEN
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FIX_CYN_C
    ! -------------------------------------------------------------------------
    ! End of ingoing variables
    ! -------------------------------------------------------------------------

    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_0
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_1
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_CYN_LIGHT
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_CYN_TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_CYN_DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_NON_FIX_CYN_N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_NON_FIX_CYN_P
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_NON_FIX_CYN_NUTR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_FIX_CYN_N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_FIX_CYN_P
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_FIX_CYN_NUTR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_NON_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_NON_FIX_CYN_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_FIX_CYN_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_MET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_EXCR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_INT_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KD_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FAC_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_DEATH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PREF_NH4N_DON_FIX_CYN

    !Auxillary variable
    real(kind = DBL_PREC) :: FIX_CYN_DEPTH
    integer :: i
    real(kind = DBL_PREC) :: loss
    real(kind = DBL_PREC) :: scale_loss

    !Caculations for nitrogen fixing cyanobacteria growth limitation by temperature
    call GROWTH_AT_TEMP &
         (TEMP, LIM_KG_FIX_CYN_TEMP, FIX_CYN_OPT_TEMP_LR, FIX_CYN_OPT_TEMP_UR, &
		  KG_FIX_CYN_OPT_TEMP,  KAPPA_FIX_CYN_UNDER_OPT_TEMP, &
		  KAPPA_FIX_CYN_OVER_OPT_TEMP,nkn)

    !if( KG_FIX_CYN_OPT_TEMP .ne.  0.D0) then
        !LIM_KG_FIX_CYN_TEMP = KG_FIX_CYN / KG_FIX_CYN_OPT_TEMP
        ! Temperature limited growth
        KG_FIX_CYN = KG_FIX_CYN_OPT_TEMP * LIM_KG_FIX_CYN_TEMP
    !else
    !   LIM_KG_FIX_CYN_TEMP = 0.D0
    !end if

    if (smith .eq. 0) then
        !May be replaced by Smith formulation
        ALPHA_0 = (I_A / I_S_FIX_CYN) * exp(-1.0D0 * K_E * 0.0D0)
        ALPHA_1 = (I_A / I_S_FIX_CYN) * exp(-1.0D0 * K_E * DEPTH)

        LIM_KG_FIX_CYN_LIGHT = &
            (((2.718 * FDAY) / (K_E * DEPTH)) * &
             (exp(-1.0D0 * ALPHA_1) - exp(-1.0D0 * ALPHA_0)))
        !WC_OUTPUTS(nstate+3) = FIX_CYN_C_TO_CHLA
    end if

    if (smith .eq. 1) then

	     !1.2 is assumed that all fixers are in the layer of this depth
		 ! (Introduced 2013 working with Ali)
		 ! Changed to 1 by Petras 2014 10 13
         FIX_CYN_DEPTH = 1.0

         call LIM_LIGHT &
		      (I_A, CHLA, KG_FIX_CYN, DEPTH, K_E, LIM_KG_FIX_CYN_LIGHT, &
			   FIX_CYN_C_TO_CHLA, I_S_FIX_CYN, FIX_CYN_LIGHT_SAT,nkn)

         LIM_KG_FIX_CYN_LIGHT = FIX_CYN_DEPTH*LIM_KG_FIX_CYN_LIGHT
    end if



    LIM_KG_FIX_CYN_DOXY     = DISS_OXYGEN / (KHS_O2_FIX_CYN + DISS_OXYGEN)

    !Nutrient limitation of fixing cyanobacteria in non-fixing fraction
    LIM_KG_NON_FIX_CYN_N    = (NH4_N + (DON * frac_avail_DON) + NO3_N) / &
                              (KHS_DIN_FIX_CYN + NH4_N +(DON * frac_avail_DON) + NO3_N)

    LIM_KG_NON_FIX_CYN_P    = PO4_P / (KHS_DIP_FIX_CYN + PO4_P)
    LIM_KG_NON_FIX_CYN_NUTR = min(LIM_KG_NON_FIX_CYN_N, LIM_KG_NON_FIX_CYN_P)

    !Nutrient limitation of fixing cyanobacteria in fixing fraction
    LIM_KG_FIX_FIX_CYN_N    = (K_FIX / (K_FIX + NH4_N +(DON * frac_avail_DON) + NO3_N))
    LIM_KG_FIX_FIX_CYN_P    = LIM_KG_NON_FIX_CYN_P
    LIM_KG_FIX_FIX_CYN_NUTR = min(LIM_KG_FIX_FIX_CYN_N, LIM_KG_FIX_FIX_CYN_P)

    !Growth limitation of fixing cyanobacteria in non-fixing fraction
    LIM_KG_NON_FIX_CYN   = &
        LIM_KG_FIX_CYN_LIGHT*min(LIM_KG_FIX_CYN_DOXY, LIM_KG_NON_FIX_CYN_NUTR)

    !Growth limitation of fixing cyanobacteria in fixing fraction
    LIM_KG_FIX_FIX_CYN   = &
        LIM_KG_FIX_CYN_LIGHT* min(LIM_KG_FIX_CYN_DOXY, LIM_KG_FIX_FIX_CYN_NUTR)

    !Growth rate of fixing cyanobacteria in non-fixing fraction
    R_NON_FIX_CYN_GROWTH = KG_FIX_CYN * LIM_KG_NON_FIX_CYN * FIX_CYN_C

    !Growth rate of fixing cyanobacteria in fixing state
    R_FIX_FIX_CYN_GROWTH = R_FIX * KG_FIX_CYN * LIM_KG_FIX_FIX_CYN * FIX_CYN_C

    !Total growth rate of fixing cyanobacteria as a sum of non-fixing and
    !fixing fractions.
    R_FIX_CYN_GROWTH = R_NON_FIX_CYN_GROWTH + R_FIX_FIX_CYN_GROWTH

    !Nitrogen fixing cyanobacteria metabolism, respiration, excretion rate
    R_FIX_CYN_MET = R_FIX_CYN_GROWTH * (1.0D0 - EFF_FIX_CYN_GROWTH)
    R_FIX_CYN_RESP = (1.D0-FRAC_FIX_CYN_EXCR) * R_FIX_CYN_MET
    R_FIX_CYN_EXCR = FRAC_FIX_CYN_EXCR * R_FIX_CYN_MET

    !Nitrogen fixing cyanobacteria dark respiration rate
    R_FIX_CYN_INT_RESP = &
           KR_FIX_CYN_20 * (THETA_KR_FIX_CYN ** (TEMP - 20.0D0)) * &
           LIM_KG_FIX_CYN_DOXY * FIX_CYN_C

    !Nitrogen fixing cyanobacteria death rate
    KD_FIX_CYN = KD_FIX_CYN_20 * (THETA_KD_FIX_CYN ** (TEMP - 20.0D0))
    FAC_HYPOX_FIX_CYN_D = 1.0D0

    if(KD_FIX_CYN_20 .gt. 0.D0) then
        where (DISS_OXYGEN <= DO_STR_HYPOX_FIX_CYN_D)
            where (DISS_OXYGEN / DO_STR_HYPOX_FIX_CYN_D > 1.0D-1)
                FAC_HYPOX_FIX_CYN_D = &
                    THETA_HYPOX_FIX_CYN_D ** &
                      (EXPON_HYPOX_FIX_CYN_D * (DO_STR_HYPOX_FIX_CYN_D - DISS_OXYGEN))
            elsewhere
                FAC_HYPOX_FIX_CYN_D = min(TIME_STEP / (5.0D-1 * KD_FIX_CYN), &
                                          9.0D-1 / (KD_FIX_CYN * TIME_STEP))
                R_FIX_CYN_INT_RESP = 0.0D0
                R_FIX_CYN_RESP     = 0.0D0
                R_FIX_CYN_GROWTH   = 0.0D0
            end where
        elsewhere
            FAC_HYPOX_FIX_CYN_D = 1.0D0
        end where
    end if

    !Nitrogen fixing cyanobacteria death rate
    R_FIX_CYN_DEATH = KD_FIX_CYN * FAC_HYPOX_FIX_CYN_D * FIX_CYN_C

    ! Mass-balance safeguard: limit total losses to available biomass per TIME_STEP
    ! This prevents negative concentrations when loss rates exceed growth
    do i = 1, nkn
        if (FIX_CYN_C(i) > 0.0D0) then
            loss = R_FIX_CYN_DEATH(i) + R_FIX_CYN_EXCR(i) + R_FIX_CYN_INT_RESP(i) + R_FIX_CYN_RESP(i)
            ! Limit total loss to 50% of available biomass per timestep (excluding grazing, handled elsewhere)
            if (loss > 0.5D0 * FIX_CYN_C(i) / TIME_STEP) then
                scale_loss = (0.5D0 * FIX_CYN_C(i) / TIME_STEP) / loss
                R_FIX_CYN_DEATH(i) = R_FIX_CYN_DEATH(i) * scale_loss
                R_FIX_CYN_EXCR(i) = R_FIX_CYN_EXCR(i) * scale_loss
                R_FIX_CYN_INT_RESP(i) = R_FIX_CYN_INT_RESP(i) * scale_loss
                R_FIX_CYN_RESP(i) = R_FIX_CYN_RESP(i) * scale_loss
            end if
        end if
    end do

    !PREF_NH4N_FIX_CYN = NH4_N / (NH4_N + KHS_NH4N_PREF_FIX_CYN)
    call AMMONIA_DON_PREFS &
         (PREF_NH4N_DON_FIX_CYN, NH4_N, DON, frac_avail_DON, NO3_N, KHS_DIN_FIX_CYN,nkn)

end subroutine FIX_CYANOBACTERIA



subroutine FIX_CYANOBACTERIA_BOUYANT  &
           (KG_FIX_CYN_OPT_TEMP          , &
            FIX_CYN_OPT_TEMP_LR          , &
            FIX_CYN_OPT_TEMP_UR          , &
            EFF_FIX_CYN_GROWTH           , &
            KAPPA_FIX_CYN_UNDER_OPT_TEMP , &
            KAPPA_FIX_CYN_OVER_OPT_TEMP  , &
            KR_FIX_CYN_20                , &
            THETA_KR_FIX_CYN             , &
            KD_FIX_CYN_20                , &
            THETA_KD_FIX_CYN             , &
            KHS_DIN_FIX_CYN              , &
            KHS_DIP_FIX_CYN              , &
            KHS_O2_FIX_CYN               , &
            I_S_FIX_CYN                  , &
            DO_STR_HYPOX_FIX_CYN_D       , &
            THETA_HYPOX_FIX_CYN_D        , &
            EXPON_HYPOX_FIX_CYN_D        , &
            FIX_CYN_N_TO_C               , &
            FIX_CYN_P_TO_C               , &
            FIX_CYN_O2_TO_C              , &
            FIX_CYN_C_TO_CHLA            , &
            FIX_CYN_LIGHT_SAT            , &
            FRAC_FIX_CYN_EXCR            , &
            R_FIX                        , &
            K_FIX                        , &
            TIME_STEP                    , &
            SMITH                        , &
            frac_avail_DON               , &
            nkn                          , &
            FDAY                         , &
            I_A                          , &
            K_E                          , &
            DEPTH                        , &
            CHLA                         , &
            TEMP                         , &
            WINDS                        , &
            NH4_N                        , &
            NO3_N                        , &
            DON                          , &
            PO4_P                        , &
            DISS_OXYGEN                  , &
            FIX_CYN_C                    , &
            ALPHA_0                      , &
            ALPHA_1                      , &
            KG_FIX_CYN                   , &
            LIM_KG_FIX_CYN_LIGHT         , &
            LIM_KG_FIX_CYN_TEMP          , &
            LIM_KG_FIX_CYN_DOXY          , &
            LIM_KG_NON_FIX_CYN_N         , &
            LIM_KG_NON_FIX_CYN_P         , &
            LIM_KG_NON_FIX_CYN_NUTR      , &
            LIM_KG_FIX_FIX_CYN_N         , &
            LIM_KG_FIX_FIX_CYN_P         , &
            LIM_KG_FIX_FIX_CYN_NUTR      , &
            LIM_KG_NON_FIX_CYN           , &
            LIM_KG_FIX_FIX_CYN           , &
            R_NON_FIX_CYN_GROWTH         , &
            R_FIX_FIX_CYN_GROWTH         , &
            R_FIX_CYN_GROWTH             , &
            R_FIX_CYN_MET                , &
            R_FIX_CYN_RESP               , &
            R_FIX_CYN_EXCR               , &
            R_FIX_CYN_INT_RESP           , &
            KD_FIX_CYN                   , &
            FAC_HYPOX_FIX_CYN_D          , &
            R_FIX_CYN_DEATH              , &
            PREF_NH4_DON_FIX_CYN)

    use AQUABC_II_GLOBAL
    use para_aqua

    implicit none

    ! -------------------------------------------------------------------------
    ! Ingoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), intent(in) :: KG_FIX_CYN_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_OPT_TEMP_LR
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_OPT_TEMP_UR
    real(kind = DBL_PREC), intent(in) :: EFF_FIX_CYN_GROWTH
    real(kind = DBL_PREC), intent(in) :: KAPPA_FIX_CYN_UNDER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KAPPA_FIX_CYN_OVER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KR_FIX_CYN_20
    real(kind = DBL_PREC), intent(in) :: THETA_KR_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: KD_FIX_CYN_20
    real(kind = DBL_PREC), intent(in) :: THETA_KD_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: KHS_DIN_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: KHS_DIP_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: KHS_O2_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: I_S_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: DO_STR_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), intent(in) :: THETA_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), intent(in) :: EXPON_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_N_TO_C
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_P_TO_C
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_O2_TO_C
    real(kind = DBL_PREC), intent(in) :: FIX_CYN_C_TO_CHLA
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FIX_CYN_LIGHT_SAT
    real(kind = DBL_PREC), intent(in) :: FRAC_FIX_CYN_EXCR
    real(kind = DBL_PREC), intent(in) :: R_FIX
    real(kind = DBL_PREC), intent(in) :: K_FIX

    real(kind = DBL_PREC), intent(in) :: TIME_STEP
    integer, intent(in) :: SMITH
    integer, intent(in) :: nkn
!
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FDAY
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: I_A
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: K_E
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DEPTH
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: CHLA
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NH4_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3_N
    real(kind = DBL_PREC), intent(in) :: frac_avail_DON
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DON
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PO4_P
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DISS_OXYGEN
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FIX_CYN_C
    ! -------------------------------------------------------------------------
    ! End of ingoing variables
    ! -------------------------------------------------------------------------

    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_0
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_1
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_CYN_LIGHT
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_CYN_TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_CYN_DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_NON_FIX_CYN_N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_NON_FIX_CYN_P
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_NON_FIX_CYN_NUTR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_FIX_CYN_N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_FIX_CYN_P
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_FIX_CYN_NUTR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_NON_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_FIX_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_NON_FIX_CYN_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_FIX_CYN_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_MET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_EXCR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_INT_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KD_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FAC_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FIX_CYN_DEATH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PREF_NH4_DON_FIX_CYN

    !Auxillary variables introduced by Pzem 2019-08
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: WINDS  ! Wind speed (input parameter)
    real(kind = DBL_PREC), dimension(nkn) :: FIX_CYN_DEPTH
    real(kind = DBL_PREC) :: EUPHOTIC_DEPTH(nkn)
    real(kind = DBL_PREC) :: MIX_DEPTH     (nkn)

    !Caculations for nitrogen fixing cyanobacteria growth limitation by temperature
    call GROWTH_AT_TEMP &
         (TEMP, LIM_KG_FIX_CYN_TEMP, FIX_CYN_OPT_TEMP_LR, FIX_CYN_OPT_TEMP_UR, KG_FIX_CYN_OPT_TEMP,  &
          KAPPA_FIX_CYN_UNDER_OPT_TEMP, KAPPA_FIX_CYN_OVER_OPT_TEMP,nkn)

    !if( KG_FIX_CYN_OPT_TEMP .ne.  0.D0) then
        !LIM_KG_FIX_CYN_TEMP = KG_FIX_CYN / KG_FIX_CYN_OPT_TEMP
    !Caculation nitrogen fixing cyanobacteria  temperature limited growth
        KG_FIX_CYN = KG_FIX_CYN_OPT_TEMP * LIM_KG_FIX_CYN_TEMP
    !else
        !LIM_KG_FIX_CYN_TEMP = 0.D0
    !end if

    if (smith .eq. 0) then
        !May be replaced by Smith formulation
        ALPHA_0 = (I_A / I_S_FIX_CYN) * exp(-1.0D0 * K_E * 0.0D0)
        ALPHA_1 = (I_A / I_S_FIX_CYN) * exp(-1.0D0 * K_E * DEPTH)

        LIM_KG_FIX_CYN_LIGHT = &
            (((2.718 * FDAY) / (K_E * DEPTH)) * &
             (exp(-1.0D0 * ALPHA_1) - exp(-1.0D0 * ALPHA_0)))
    end if

    if (smith .eq. 1) then
        EUPHOTIC_DEPTH(:) = 4.61D0 / K_E(:)

        ! Introduced by Petras 2019-08-10. The depth in which 1% of ligt is reached
        ! The same for fixers. This fix is valid only for 2d. fixme
        ! Nothing is done to increase selfshading. Concentration is
        ! still evenly distributed to the whole depth. fixme

        !Nagy et al. 2006
        MIX_DEPTH(:) = 0.8121D0 * WINDS(:) + 0.7006D0

        where(MIX_DEPTH(:) .le. EUPHOTIC_DEPTH(:) .and. EUPHOTIC_DEPTH(:) .le. DEPTH(:))
            FIX_CYN_DEPTH(:) = EUPHOTIC_DEPTH(:)
        elsewhere(MIX_DEPTH(:) .gt. EUPHOTIC_DEPTH(:) .and. MIX_DEPTH(:) .le. DEPTH(:))
            FIX_CYN_DEPTH(:) = MIX_DEPTH(:)
        elsewhere
            FIX_CYN_DEPTH(:) = DEPTH(:)
        end where

        write(6,*) 'FIX_CYANO CALL: I_A=', I_A(1:min(8,nkn))
        write(6,*) 'FIX_CYANO CALL: CHLA=', CHLA(1:min(8,nkn))
        write(6,*) 'FIX_CYANO CALL: KG_FIX_CYN=', KG_FIX_CYN(1:min(8,nkn))
        write(6,*) 'FIX_CYANO CALL: DEPTH=', FIX_CYN_DEPTH(1:min(8,nkn))
        write(6,*) 'FIX_CYANO CALL: K_E=', K_E(1:min(8,nkn))
        call LIM_LIGHT(I_A, CHLA, KG_FIX_CYN, FIX_CYN_DEPTH, K_E, &
             LIM_KG_FIX_CYN_LIGHT, FIX_CYN_C_TO_CHLA, I_S_FIX_CYN, FIX_CYN_LIGHT_SAT,nkn)
    end if



    LIM_KG_FIX_CYN_DOXY     = DISS_OXYGEN / (KHS_O2_FIX_CYN + DISS_OXYGEN)

    !Nutrient limitation of fixing cyanobacteria in non-fixing fraction
    LIM_KG_NON_FIX_CYN_N    = &
        (NH4_N + (DON * frac_avail_DON) + NO3_N) / &
        (KHS_DIN_FIX_CYN + NH4_N +(DON * frac_avail_DON) + NO3_N)

    LIM_KG_NON_FIX_CYN_P    = PO4_P / (KHS_DIP_FIX_CYN + PO4_P)
    LIM_KG_NON_FIX_CYN_NUTR = min(LIM_KG_NON_FIX_CYN_N, LIM_KG_NON_FIX_CYN_P)

    !Nutrient limitation of fixing cyanobacteria in fixing fraction.
    LIM_KG_FIX_FIX_CYN_N    = (K_FIX / (K_FIX + NH4_N +(DON * frac_avail_DON) + NO3_N))
    LIM_KG_FIX_FIX_CYN_P    = LIM_KG_NON_FIX_CYN_P
    LIM_KG_FIX_FIX_CYN_NUTR = min(LIM_KG_FIX_FIX_CYN_N, LIM_KG_FIX_FIX_CYN_P)

    !Growth limitation of fixing cyanobacteria in non-fixing fraction
    LIM_KG_NON_FIX_CYN   = &
         LIM_KG_FIX_CYN_LIGHT*min(LIM_KG_FIX_CYN_DOXY, LIM_KG_NON_FIX_CYN_NUTR)

    !Growth limitation of fixing cyanobacteria in fixing fraction
    LIM_KG_FIX_FIX_CYN   = &
        LIM_KG_FIX_CYN_LIGHT* min(LIM_KG_FIX_CYN_DOXY, LIM_KG_FIX_FIX_CYN_NUTR)

    !Growth rate of fixing cyanobacteria in non-fixing fraction
    R_NON_FIX_CYN_GROWTH = KG_FIX_CYN * LIM_KG_NON_FIX_CYN * FIX_CYN_C

    !Growth rate of fixing cyanobacteria in fixing state
    R_FIX_FIX_CYN_GROWTH = R_FIX * KG_FIX_CYN * LIM_KG_FIX_FIX_CYN * FIX_CYN_C

    !Total growth rate of fixing cyanobacteria as a sum of non-fixing and
    !fixing fractions.
    R_FIX_CYN_GROWTH = R_NON_FIX_CYN_GROWTH + R_FIX_FIX_CYN_GROWTH

    !Nitrogen fixing cyanobacteria metabolism, respiration, excretion rate
    R_FIX_CYN_MET = R_FIX_CYN_GROWTH * (1.0D0 - EFF_FIX_CYN_GROWTH)
    R_FIX_CYN_RESP = (1.D0-FRAC_FIX_CYN_EXCR) * R_FIX_CYN_MET
    R_FIX_CYN_EXCR = FRAC_FIX_CYN_EXCR * R_FIX_CYN_MET

    !Nitrogen fixing cyanobacteria dark respiration rate
    R_FIX_CYN_INT_RESP = &
        KR_FIX_CYN_20 * (THETA_KR_FIX_CYN ** (TEMP - 2.0D1)) * &
        LIM_KG_FIX_CYN_DOXY * FIX_CYN_C

    !Nitrogen fixing cyanobacteria death rate
    KD_FIX_CYN = KD_FIX_CYN_20 * (THETA_KD_FIX_CYN ** (TEMP - 2.0D1))

    FAC_HYPOX_FIX_CYN_D = 1.0D0
    if(KD_FIX_CYN_20 .gt. 0.0D0) then
     where (DISS_OXYGEN <= DO_STR_HYPOX_FIX_CYN_D)

         where (DISS_OXYGEN / DO_STR_HYPOX_FIX_CYN_D > 1.0D-1)
             FAC_HYPOX_FIX_CYN_D = THETA_HYPOX_FIX_CYN_D ** &
                  (EXPON_HYPOX_FIX_CYN_D * (DO_STR_HYPOX_FIX_CYN_D - DISS_OXYGEN))
         elsewhere
             FAC_HYPOX_FIX_CYN_D = TIME_STEP / (5.0D-1 * KD_FIX_CYN)
             R_FIX_CYN_INT_RESP = 0.0D0
             R_FIX_CYN_RESP     = 0.0D0
             R_FIX_CYN_GROWTH   = 0.0D0
         end where
     elsewhere
         FAC_HYPOX_FIX_CYN_D = 1.0D0
     end where
    end if

    !Nitrogen fixing cyanobacteria death rate
    R_FIX_CYN_DEATH = KD_FIX_CYN * FAC_HYPOX_FIX_CYN_D * FIX_CYN_C

    call AMMONIA_DON_PREFS&
         (PREF_NH4_DON_FIX_CYN, NH4_N, DON, &
          frac_avail_DON, NO3_N, KHS_DIN_FIX_CYN,nkn)

end subroutine FIX_CYANOBACTERIA_BOUYANT
