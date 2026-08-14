! Auxilary routines for the pelagic model

! Contents:
!subroutine FIX_CYANOBACTERIA

subroutine FIX_CYANOBACTERIA  &
           (params                          , &
            env                             , &
            TIME_STEP                    , &
            SMITH                        , &
            nkn                          , &
            NH4_N                        , &
            NO3_N                        , &
            DON                          , &
            PO4_P                        , &
            FIX_CYN_C                    , &
            FIX_CYN_LIGHT_SAT            , &
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
    use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
    use AQUABC_PELAGIC_TYPES, only: t_fix_cyn_params, t_phyto_env
    implicit none

    ! -------------------------------------------------------------------------
    ! Derived-type parameter block (replaces 26 scalar constants)
    ! -------------------------------------------------------------------------
    type(t_fix_cyn_params), intent(in) :: params
    type(t_phyto_env),      intent(in) :: env

    ! -------------------------------------------------------------------------
    ! Metadata / non-constant arguments
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), intent(in) :: TIME_STEP
    integer, intent(in) :: SMITH
    integer, intent(in) :: nkn

    ! -------------------------------------------------------------------------
    ! Ingoing arrays
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NH4_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DON
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PO4_P
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FIX_CYN_C
    real(kind = DBL_PREC), dimension(nkn), intent(out) :: FIX_CYN_LIGHT_SAT  ! pure output: LIM_LIGHT (intent(out)) writes it, never read before
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

    associate( &
        KG_FIX_CYN_OPT_TEMP          => params%KG_FIX_CYN_OPT_TEMP,          &
        FIX_CYN_OPT_TEMP_LR          => params%FIX_CYN_OPT_TEMP_LR,          &
        FIX_CYN_OPT_TEMP_UR          => params%FIX_CYN_OPT_TEMP_UR,          &
        EFF_FIX_CYN_GROWTH           => params%EFF_FIX_CYN_GROWTH,           &
        KAPPA_FIX_CYN_UNDER_OPT_TEMP => params%KAPPA_FIX_CYN_UNDER_OPT_TEMP, &
        KAPPA_FIX_CYN_OVER_OPT_TEMP  => params%KAPPA_FIX_CYN_OVER_OPT_TEMP,  &
        KR_FIX_CYN_20                => params%KR_FIX_CYN_20,                &
        THETA_KR_FIX_CYN             => params%THETA_KR_FIX_CYN,             &
        KD_FIX_CYN_20                => params%KD_FIX_CYN_20,                &
        THETA_KD_FIX_CYN             => params%THETA_KD_FIX_CYN,             &
        KHS_DIN_FIX_CYN              => params%KHS_DIN_FIX_CYN,              &
        KHS_DIP_FIX_CYN              => params%KHS_DIP_FIX_CYN,              &
        KHS_O2_FIX_CYN               => params%KHS_O2_FIX_CYN,               &
        I_S_FIX_CYN                  => params%I_S_FIX_CYN,                  &
        DO_STR_HYPOX_FIX_CYN_D       => params%DO_STR_HYPOX_FIX_CYN_D,       &
        THETA_HYPOX_FIX_CYN_D        => params%THETA_HYPOX_FIX_CYN_D,        &
        EXPON_HYPOX_FIX_CYN_D        => params%EXPON_HYPOX_FIX_CYN_D,        &
        FIX_CYN_N_TO_C               => params%FIX_CYN_N_TO_C,               &
        FIX_CYN_P_TO_C               => params%FIX_CYN_P_TO_C,               &
        FIX_CYN_O2_TO_C              => params%FIX_CYN_O2_TO_C,              &
        FIX_CYN_C_TO_CHLA            => params%FIX_CYN_C_TO_CHLA,            &
        FRAC_FIX_CYN_EXCR            => params%FRAC_FIX_CYN_EXCR,            &
        R_FIX                        => params%R_FIX,                        &
        K_FIX                        => params%K_FIX,                        &
        BETA_FIX_CYN                 => params%BETA_FIX_CYN,                 &
        frac_avail_DON               => params%frac_avail_DON                &
    )

    associate( &
        TEMP         => env%TEMP,         &
        I_A          => env%I_A,          &
        K_E          => env%K_E,          &
        DEPTH        => env%DEPTH,        &
        CHLA         => env%CHLA,         &
        FDAY         => env%FDAY,         &
        DISS_OXYGEN  => env%DISS_OXYGEN   &
    )

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
        ALPHA_0 = (I_A / I_S_FIX_CYN) * safe_exp(-1.0D0 * K_E * 0.0D0)
        ALPHA_1 = (I_A / I_S_FIX_CYN) * safe_exp(-1.0D0 * K_E * DEPTH)

        LIM_KG_FIX_CYN_LIGHT = &
            (((2.718 * FDAY) / (K_E * DEPTH)) * &
             (safe_exp(-1.0D0 * ALPHA_1) - safe_exp(-1.0D0 * ALPHA_0)))
        ! Clamp to [0,1]: Steele formula can produce tiny negatives at dusk
        LIM_KG_FIX_CYN_LIGHT = max(0.0D0, min(1.0D0, LIM_KG_FIX_CYN_LIGHT))
        !WC_OUTPUTS(nstate+3) = FIX_CYN_C_TO_CHLA
    end if

    if (smith .eq. 1) then

             !1.2 is assumed that all fixers are in the layer of this depth
                 ! (Introduced 2013 working with Ali)
                 ! Changed to 1 by Petras 2014 10 13
         FIX_CYN_DEPTH = 1.0

         call LIM_LIGHT &
                      (I_A, CHLA, KG_FIX_CYN, DEPTH, K_E, LIM_KG_FIX_CYN_LIGHT, &
                           FIX_CYN_C_TO_CHLA, I_S_FIX_CYN, FIX_CYN_LIGHT_SAT, nkn, BETA_FIX_CYN)

         LIM_KG_FIX_CYN_LIGHT = FIX_CYN_DEPTH*LIM_KG_FIX_CYN_LIGHT
    end if



    LIM_KG_FIX_CYN_DOXY     = DISS_OXYGEN / (KHS_O2_FIX_CYN + DISS_OXYGEN)

    !Nutrient limitation of fixing cyanobacteria in non-fixing fraction
    LIM_KG_NON_FIX_CYN_N    = (NH4_N + (DON * frac_avail_DON) + NO3_N) / &
                              (KHS_DIN_FIX_CYN + NH4_N +(DON * frac_avail_DON) + NO3_N)

    LIM_KG_NON_FIX_CYN_P    = PO4_P / (KHS_DIP_FIX_CYN + PO4_P)
    ! Synthesizing Unit colimitation for non-fixing fraction (Saito et al. 2008)
    LIM_KG_NON_FIX_CYN_NUTR = LIM_KG_NON_FIX_CYN_N * LIM_KG_NON_FIX_CYN_P / &
        max(LIM_KG_NON_FIX_CYN_N + LIM_KG_NON_FIX_CYN_P - &
            LIM_KG_NON_FIX_CYN_N * LIM_KG_NON_FIX_CYN_P, 1.0D-20)

    !Nutrient limitation of fixing cyanobacteria in fixing fraction
    ! (Liebig min retained: N term is DIN-inhibition switch, not colimitation)
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

    end associate ! env
    end associate ! params

end subroutine FIX_CYANOBACTERIA



subroutine FIX_CYANOBACTERIA_BOUYANT  &
           (params                          , &
            env                             , &
            TIME_STEP                    , &
            SMITH                        , &
            nkn                          , &
            NH4_N                        , &
            NO3_N                        , &
            DON                          , &
            PO4_P                        , &
            FIX_CYN_C                    , &
            FIX_CYN_LIGHT_SAT            , &
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
            PREF_NH4_DON_FIX_CYN         , &
            CYANO_POS_MODEL              , &
            H_SURF_POS                   , &
            W_CRIT_POS_MIN               , &
            S_CHUNK)

    use AQUABC_II_GLOBAL
    use AQUABC_POSITIONING_STATE, only: CALM_FRACTION, K_POS_UP, K_POS_DISP, W_DISP_POS
    use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
    use para_aqua
    use AQUABC_PELAGIC_TYPES, only: t_fix_cyn_params, t_phyto_env

    implicit none

    ! -------------------------------------------------------------------------
    ! Derived-type parameter block (replaces 26 scalar constants)
    ! -------------------------------------------------------------------------
    type(t_fix_cyn_params), intent(in) :: params
    type(t_phyto_env),      intent(in) :: env

    ! -------------------------------------------------------------------------
    ! Metadata / non-constant arguments
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), intent(in) :: TIME_STEP
    integer, intent(in) :: SMITH
    integer, intent(in) :: nkn

    ! -------------------------------------------------------------------------
    ! Ingoing arrays
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NH4_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DON
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PO4_P
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FIX_CYN_C
    real(kind = DBL_PREC), dimension(nkn), intent(out) :: FIX_CYN_LIGHT_SAT  ! pure output: LIM_LIGHT (intent(out)) writes it, never read before
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
    ! Sub-daily surface-positioning gate: 0 = legacy daily-mean Nagy gate
    ! (default), 1 = calm-fraction surface blend; H_SURF_POS = the surface
    ! layer depth (m) experienced by the positioned fraction.
    integer, intent(in) :: CYANO_POS_MODEL
    real(kind = DBL_PREC), intent(in) :: H_SURF_POS
    real(kind = DBL_PREC), intent(in) :: W_CRIT_POS_MIN
    ! surface-positioned fraction state slice (module S_POS), updated when
    ! CYANO_POS_MODEL >= 2; inert zeros otherwise
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: S_CHUNK

    !Auxillary variables introduced by Pzem 2019-08
    real(kind = DBL_PREC), dimension(nkn) :: FIX_CYN_DEPTH
    real(kind = DBL_PREC) :: EUPHOTIC_DEPTH(nkn)
    real(kind = DBL_PREC) :: MIX_DEPTH     (nkn)
    ! work arrays for the sub-daily positioning blend (CYANO_POS_MODEL = 1)
    real(kind = DBL_PREC), dimension(nkn) :: X_POS, F_CALM, H_SURF_ARR, LIM_SURF, SAT_SCRATCH
    integer :: i
    real(kind = DBL_PREC) :: loss, scale_loss

    associate( &
        KG_FIX_CYN_OPT_TEMP          => params%KG_FIX_CYN_OPT_TEMP,          &
        FIX_CYN_OPT_TEMP_LR          => params%FIX_CYN_OPT_TEMP_LR,          &
        FIX_CYN_OPT_TEMP_UR          => params%FIX_CYN_OPT_TEMP_UR,          &
        EFF_FIX_CYN_GROWTH           => params%EFF_FIX_CYN_GROWTH,           &
        KAPPA_FIX_CYN_UNDER_OPT_TEMP => params%KAPPA_FIX_CYN_UNDER_OPT_TEMP, &
        KAPPA_FIX_CYN_OVER_OPT_TEMP  => params%KAPPA_FIX_CYN_OVER_OPT_TEMP,  &
        KR_FIX_CYN_20                => params%KR_FIX_CYN_20,                &
        THETA_KR_FIX_CYN             => params%THETA_KR_FIX_CYN,             &
        KD_FIX_CYN_20                => params%KD_FIX_CYN_20,                &
        THETA_KD_FIX_CYN             => params%THETA_KD_FIX_CYN,             &
        KHS_DIN_FIX_CYN              => params%KHS_DIN_FIX_CYN,              &
        KHS_DIP_FIX_CYN              => params%KHS_DIP_FIX_CYN,              &
        KHS_O2_FIX_CYN               => params%KHS_O2_FIX_CYN,               &
        I_S_FIX_CYN                  => params%I_S_FIX_CYN,                  &
        DO_STR_HYPOX_FIX_CYN_D       => params%DO_STR_HYPOX_FIX_CYN_D,       &
        THETA_HYPOX_FIX_CYN_D        => params%THETA_HYPOX_FIX_CYN_D,        &
        EXPON_HYPOX_FIX_CYN_D        => params%EXPON_HYPOX_FIX_CYN_D,        &
        FIX_CYN_N_TO_C               => params%FIX_CYN_N_TO_C,               &
        FIX_CYN_P_TO_C               => params%FIX_CYN_P_TO_C,               &
        FIX_CYN_O2_TO_C              => params%FIX_CYN_O2_TO_C,              &
        FIX_CYN_C_TO_CHLA            => params%FIX_CYN_C_TO_CHLA,            &
        FRAC_FIX_CYN_EXCR            => params%FRAC_FIX_CYN_EXCR,            &
        R_FIX                        => params%R_FIX,                        &
        K_FIX                        => params%K_FIX,                        &
        BETA_FIX_CYN                 => params%BETA_FIX_CYN,                 &
        frac_avail_DON               => params%frac_avail_DON                &
    )

    associate( &
        TEMP         => env%TEMP,         &
        I_A          => env%I_A,          &
        K_E          => env%K_E,          &
        DEPTH        => env%DEPTH,        &
        CHLA         => env%CHLA,         &
        FDAY         => env%FDAY,         &
        DISS_OXYGEN  => env%DISS_OXYGEN,  &
        WINDS        => env%WINDS         &
    )

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
        ALPHA_0 = (I_A / I_S_FIX_CYN) * safe_exp(-1.0D0 * K_E * 0.0D0)
        ALPHA_1 = (I_A / I_S_FIX_CYN) * safe_exp(-1.0D0 * K_E * DEPTH)

        LIM_KG_FIX_CYN_LIGHT = &
            (((2.718 * FDAY) / (K_E * DEPTH)) * &
             (safe_exp(-1.0D0 * ALPHA_1) - safe_exp(-1.0D0 * ALPHA_0)))
        ! Clamp to [0,1]: Steele formula can produce tiny negatives at dusk
        LIM_KG_FIX_CYN_LIGHT = max(0.0D0, min(1.0D0, LIM_KG_FIX_CYN_LIGHT))
    end if

    if (smith .eq. 1) then
        EUPHOTIC_DEPTH(:) = 4.61D0 / max(K_E(:), 1.0D-20)

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

        call LIM_LIGHT(I_A, CHLA, KG_FIX_CYN, FIX_CYN_DEPTH, K_E, &
             LIM_KG_FIX_CYN_LIGHT, FIX_CYN_C_TO_CHLA, I_S_FIX_CYN, FIX_CYN_LIGHT_SAT, nkn, BETA_FIX_CYN)

        ! ------------------------------------------------------------------
        ! Sub-daily surface-positioning blend (CYANO_POS_MODEL = 1, opt-in).
        ! The daily-mean Nagy gate above misses the diurnal calm windows in
        ! which buoyant colonies actually reach the surface (doc par. 18: 0 %
        ! full engagement under honest optics). F_CALM is the fraction of the
        ! day with hourly wind below the positioning-critical speed
        ! W_crit (MIX(W)=euphotic), from the within-day W_h/W_day CDF fitted
        ! on ERA5 hourly Nida 2012-2022 (96,432 h; ln F quadratic in ln x,
        ! max error < 0.05 for x <= 1). For that fraction of the day the
        ! population experiences the surface layer H_SURF_POS instead of the
        ! cascade depth; x is capped at 1 because beyond it the cascade gate
        ! above already positions.
        ! ------------------------------------------------------------------
        if (CYANO_POS_MODEL == 1) then
            X_POS = max((EUPHOTIC_DEPTH - 0.7006D0) / 0.8121D0, W_CRIT_POS_MIN, 0.0D0)
            F_CALM = CALM_FRACTION(WINDS, X_POS)
            H_SURF_ARR = min(H_SURF_POS, DEPTH)
            call LIM_LIGHT(I_A, CHLA, KG_FIX_CYN, H_SURF_ARR, K_E, &
                 LIM_SURF, FIX_CYN_C_TO_CHLA, I_S_FIX_CYN, SAT_SCRATCH, nkn, BETA_FIX_CYN)
            LIM_KG_FIX_CYN_LIGHT = (1.0D0 - F_CALM) * LIM_KG_FIX_CYN_LIGHT + F_CALM * LIM_SURF
        end if

        ! Positional ratchet (CYANO_POS_MODEL = 2): S builds during the calm
        ! fraction of the day and is dispersed during the storm fraction,
        ! persisting across time steps (module AQUABC_POSITIONING_STATE).
        ! Forward Euler on the kinetic step, clamped to [0,1]. The blend then
        ! uses S instead of the memoryless within-day calm fraction.
        if (CYANO_POS_MODEL >= 2) then
            X_POS = max((EUPHOTIC_DEPTH - 0.7006D0) / 0.8121D0, W_CRIT_POS_MIN, 0.0D0)
            F_CALM = CALM_FRACTION(WINDS, X_POS)
            LIM_SURF = 1.0D0 - CALM_FRACTION(WINDS, W_DISP_POS)   ! storm fraction (scratch use)
            S_CHUNK = S_CHUNK + TIME_STEP * &
                (K_POS_UP * F_CALM * (1.0D0 - S_CHUNK) - K_POS_DISP * LIM_SURF * S_CHUNK)
            S_CHUNK = max(0.0D0, min(1.0D0, S_CHUNK))
            F_CALM = S_CHUNK
            H_SURF_ARR = min(H_SURF_POS, DEPTH)
            call LIM_LIGHT(I_A, CHLA, KG_FIX_CYN, H_SURF_ARR, K_E, &
                 LIM_SURF, FIX_CYN_C_TO_CHLA, I_S_FIX_CYN, SAT_SCRATCH, nkn, BETA_FIX_CYN)
            LIM_KG_FIX_CYN_LIGHT = (1.0D0 - F_CALM) * LIM_KG_FIX_CYN_LIGHT + F_CALM * LIM_SURF
        end if
    end if



    LIM_KG_FIX_CYN_DOXY     = DISS_OXYGEN / (KHS_O2_FIX_CYN + DISS_OXYGEN)

    !Nutrient limitation of fixing cyanobacteria in non-fixing fraction
    LIM_KG_NON_FIX_CYN_N    = &
        (NH4_N + (DON * frac_avail_DON) + NO3_N) / &
        (KHS_DIN_FIX_CYN + NH4_N +(DON * frac_avail_DON) + NO3_N)

    LIM_KG_NON_FIX_CYN_P    = PO4_P / (KHS_DIP_FIX_CYN + PO4_P)
    ! Synthesizing Unit colimitation for non-fixing fraction (Saito et al. 2008)
    LIM_KG_NON_FIX_CYN_NUTR = LIM_KG_NON_FIX_CYN_N * LIM_KG_NON_FIX_CYN_P / &
        max(LIM_KG_NON_FIX_CYN_N + LIM_KG_NON_FIX_CYN_P - &
            LIM_KG_NON_FIX_CYN_N * LIM_KG_NON_FIX_CYN_P, 1.0D-20)

    !Nutrient limitation of fixing cyanobacteria in fixing fraction.
    ! (Liebig min retained: N term is DIN-inhibition switch, not colimitation)
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
    do i = 1, nkn
        if (FIX_CYN_C(i) > 0.0D0) then
            loss = R_FIX_CYN_DEATH(i) + R_FIX_CYN_EXCR(i) + R_FIX_CYN_INT_RESP(i) + R_FIX_CYN_RESP(i)
            if (loss > 0.5D0 * FIX_CYN_C(i) / TIME_STEP) then
                scale_loss = (0.5D0 * FIX_CYN_C(i) / TIME_STEP) / loss
                R_FIX_CYN_DEATH(i) = R_FIX_CYN_DEATH(i) * scale_loss
                R_FIX_CYN_EXCR(i) = R_FIX_CYN_EXCR(i) * scale_loss
                R_FIX_CYN_INT_RESP(i) = R_FIX_CYN_INT_RESP(i) * scale_loss
                R_FIX_CYN_RESP(i) = R_FIX_CYN_RESP(i) * scale_loss
            end if
        end if
    end do

    call AMMONIA_DON_PREFS&
         (PREF_NH4_DON_FIX_CYN, NH4_N, DON, &
          frac_avail_DON, NO3_N, KHS_DIN_FIX_CYN,nkn)

    end associate ! env
    end associate ! params

end subroutine FIX_CYANOBACTERIA_BOUYANT
