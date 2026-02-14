! Auxilary routines for the pelagic model

! Contents:
!subroutine CYANOBACTERIA
!subroutine CYANOBACTERIA_BOUYANT

subroutine CYANOBACTERIA &
           (params                      , &
            env                         , &
            CYN_LIGHT_SAT           , &
            NH4_N                   , &
            NO3_N                   , &
            DON                     , &
            PO4_P                   , &
            CYN_C                   , &
            ZOO_C                   , &
            TIME_STEP               , &
            SMITH                   , &
            nkn                     , &
            KG_CYN                  , &
            ALPHA_0                 , &
            ALPHA_1                 , &
            LIM_KG_CYN_TEMP         , &
            LIM_KG_CYN_LIGHT        , &
            LIM_KG_CYN_DOXY         , &
            LIM_KG_CYN_N            , &
            LIM_KG_CYN_P            , &
            LIM_KG_CYN_NUTR         , &
            LIM_KG_CYN              , &
            R_CYN_GROWTH            , &
            R_CYN_MET               , &
            R_CYN_RESP              , &
            R_CYN_EXCR              , &
            R_CYN_INT_RESP          , &
            KD_CYN                  , &
            FAC_HYPOX_CYN_D         , &
            R_CYN_DEATH             , &
            PREF_NH4N_DON_CYN)

    use AQUABC_II_GLOBAL
    use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
    use AQUABC_PELAGIC_TYPES, only: t_cyn_params, t_phyto_env
    implicit none

    ! -------------------------------------------------------------------------
    ! Ingoing variables
    ! -------------------------------------------------------------------------
    type(t_cyn_params),  intent(in) :: params
    type(t_phyto_env),   intent(in) :: env

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NH4_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DON

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PO4_P
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: CYN_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: ZOO_C

    real(kind = DBL_PREC), intent(in) :: TIME_STEP

    integer, intent(in) :: SMITH
    integer, intent(in) :: nkn
    ! -------------------------------------------------------------------------
    ! End of ingoing variables
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Outgoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_0
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_1
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_LIGHT
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_P
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_NUTR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_MET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_EXCR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_INT_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KD_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FAC_HYPOX_CYN_D
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_DEATH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PREF_NH4N_DON_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: CYN_LIGHT_SAT
    ! -------------------------------------------------------------------------
    ! End of outgoing variables
    ! -------------------------------------------------------------------------
    ! Local variables for mass-balance safeguard
    integer :: i
    real(kind = DBL_PREC) :: loss, scale_loss

    associate( &
        KG_CYN_OPT_TEMP          => params%KG_CYN_OPT_TEMP, &
        CYN_OPT_TEMP_LR          => params%CYN_OPT_TEMP_LR, &
        CYN_OPT_TEMP_UR          => params%CYN_OPT_TEMP_UR, &
        EFF_CYN_GROWTH            => params%EFF_CYN_GROWTH, &
        KAPPA_CYN_UNDER_OPT_TEMP => params%KAPPA_CYN_UNDER_OPT_TEMP, &
        KAPPA_CYN_OVER_OPT_TEMP  => params%KAPPA_CYN_OVER_OPT_TEMP, &
        KR_CYN_20                 => params%KR_CYN_20, &
        THETA_KR_CYN              => params%THETA_KR_CYN, &
        KD_CYN_20                 => params%KD_CYN_20, &
        THETA_KD_CYN              => params%THETA_KD_CYN, &
        KHS_DIN_CYN               => params%KHS_DIN_CYN, &
        KHS_DIP_CYN               => params%KHS_DIP_CYN, &
        KHS_O2_CYN                => params%KHS_O2_CYN, &
        FRAC_CYN_EXCR             => params%FRAC_CYN_EXCR, &
        I_S_CYN                   => params%I_S_CYN, &
        BETA_CYN                  => params%BETA_CYN, &
        DO_STR_HYPOX_CYN_D        => params%DO_STR_HYPOX_CYN_D, &
        THETA_HYPOX_CYN_D         => params%THETA_HYPOX_CYN_D, &
        EXPON_HYPOX_CYN_D         => params%EXPON_HYPOX_CYN_D, &
        CYN_N_TO_C                => params%CYN_N_TO_C, &
        CYN_P_TO_C                => params%CYN_P_TO_C, &
        CYN_O2_TO_C               => params%CYN_O2_TO_C, &
        CYN_C_TO_CHLA             => params%CYN_C_TO_CHLA, &
        frac_avail_DON             => params%frac_avail_DON)

    associate( &
        TEMP         => env%TEMP,         &
        I_A          => env%I_A,          &
        K_E          => env%K_E,          &
        DEPTH        => env%DEPTH,        &
        CHLA         => env%CHLA,         &
        FDAY         => env%FDAY,         &
        DISS_OXYGEN  => env%DISS_OXYGEN   &
    )

    ! Temperature growth limitation factor
    call GROWTH_AT_TEMP &
         (TEMP, LIM_KG_CYN_TEMP, CYN_OPT_TEMP_LR, CYN_OPT_TEMP_UR, KG_CYN_OPT_TEMP,  &
          KAPPA_CYN_UNDER_OPT_TEMP, KAPPA_CYN_OVER_OPT_TEMP,nkn)

!    if( KG_CYN_OPT_TEMP .ne.  0.D0) then
!       LIM_KG_CYN_TEMP = KG_CYN / KG_CYN_OPT_TEMP
     ! Temperature limited growth rate
     KG_CYN = KG_CYN_OPT_TEMP * LIM_KG_CYN_TEMP
!    else
!       LIM_KG_CYN_TEMP = 0.D0
!    end if

    if (smith .eq. 0) then
        !May be replaced by Smith formulation
        ALPHA_0 = (I_A / I_S_CYN) * safe_exp(-1.0D0 * K_E * 0.0D0)
        ALPHA_1 = (I_A / I_S_CYN) * safe_exp(-1.0D0 * K_E * DEPTH)

        LIM_KG_CYN_LIGHT = &
            (((2.718 * FDAY) / (K_E * DEPTH)) * &
             (safe_exp(-1.0D0 * ALPHA_1) - safe_exp(-1.0D0 * ALPHA_0)))
    end if

    if (smith .eq. 1) then
        write(6,*) 'CYANO CALL: I_A=', I_A(1:min(8,nkn))
        write(6,*) 'CYANO CALL: CHLA=', CHLA(1:min(8,nkn))
        write(6,*) 'CYANO CALL: KG_CYN=', KG_CYN(1:min(8,nkn))
        write(6,*) 'CYANO CALL: DEPTH=', DEPTH(1:min(8,nkn))
        write(6,*) 'CYANO CALL: K_E=', K_E(1:min(8,nkn))
        call LIM_LIGHT(I_A, CHLA, KG_CYN, DEPTH, K_E, LIM_KG_CYN_LIGHT, &
                       CYN_C_TO_CHLA, I_S_CYN, CYN_LIGHT_SAT, nkn, BETA_CYN)
    end if

    LIM_KG_CYN_DOXY = DISS_OXYGEN / (KHS_O2_CYN + DISS_OXYGEN)

    LIM_KG_CYN_N    = (NH4_N + (DON * frac_avail_DON) + NO3_N) / &
                      (KHS_DIN_CYN + NH4_N + (DON * frac_avail_DON) + NO3_N)

    LIM_KG_CYN_P    = PO4_P   / (KHS_DIP_CYN + PO4_P)
    ! Synthesizing Unit colimitation (Saito et al. 2008)
    LIM_KG_CYN_NUTR = LIM_KG_CYN_N * LIM_KG_CYN_P / &
        max(LIM_KG_CYN_N + LIM_KG_CYN_P - LIM_KG_CYN_N * LIM_KG_CYN_P, 1.0D-20)
    LIM_KG_CYN      = LIM_KG_CYN_LIGHT*min(LIM_KG_CYN_DOXY, LIM_KG_CYN_NUTR)
    !Non-fixing cyanobacteria growth rate
    R_CYN_GROWTH    = KG_CYN * LIM_KG_CYN * CYN_C

    !Non-fixing cyanobacteria metabolic rate, respiration and excretion
    R_CYN_MET = R_CYN_GROWTH * (1.0D0 - EFF_CYN_GROWTH)
    R_CYN_RESP = (1.D0-FRAC_CYN_EXCR) * R_CYN_MET
    R_CYN_EXCR = FRAC_CYN_EXCR * R_CYN_MET

    !Non-fixing cyanobacteria dark respiration rate
    R_CYN_INT_RESP = KR_CYN_20 * (THETA_KR_CYN ** (TEMP - 2.0D1)) * LIM_KG_CYN_DOXY * CYN_C

    !Calculations for non-fixing cyanobacteria death rate
    KD_CYN = KD_CYN_20 * (THETA_KD_CYN ** (TEMP - 2.0D1))

    FAC_HYPOX_CYN_D = 1.0D0
    if(KD_CYN_20 .gt. 0.D0) then
     where (DISS_OXYGEN <= DO_STR_HYPOX_CYN_D)

         where (DISS_OXYGEN / DO_STR_HYPOX_CYN_D > 1.0D-1)
             FAC_HYPOX_CYN_D = THETA_HYPOX_CYN_D ** &
             &    (EXPON_HYPOX_CYN_D * (DO_STR_HYPOX_CYN_D - DISS_OXYGEN))
         elsewhere
             FAC_HYPOX_CYN_D = min(TIME_STEP / (5.0D-1 * KD_CYN), &
                                  9.0D-1 / (KD_CYN * TIME_STEP))
             R_CYN_INT_RESP = 0.0D0
             R_CYN_RESP     = 0.0D0
             R_CYN_GROWTH   = 0.0D0
         end where
     elsewhere
         FAC_HYPOX_CYN_D = 1.0D0
     end where
    end if

    !Non-fixing cyanobacteria death rate
    R_CYN_DEATH = KD_CYN * FAC_HYPOX_CYN_D * CYN_C

    ! Mass-balance safeguard: limit total losses to available biomass per TIME_STEP
    do i = 1, nkn
        if (CYN_C(i) > 0.0D0) then
            loss = R_CYN_DEATH(i) + R_CYN_EXCR(i) + R_CYN_INT_RESP(i) + R_CYN_RESP(i)
            if (loss > 0.5D0 * CYN_C(i) / TIME_STEP) then
                scale_loss = (0.5D0 * CYN_C(i) / TIME_STEP) / loss
                R_CYN_DEATH(i) = R_CYN_DEATH(i) * scale_loss
                R_CYN_EXCR(i) = R_CYN_EXCR(i) * scale_loss
                R_CYN_INT_RESP(i) = R_CYN_INT_RESP(i) * scale_loss
                R_CYN_RESP(i) = R_CYN_RESP(i) * scale_loss
            end if
        end if
    end do

    call AMMONIA_DON_PREFS &
         (PREF_NH4N_DON_CYN, NH4_N, DON, frac_avail_DON, NO3_N, KHS_DIN_CYN,nkn)

    end associate ! env
    end associate ! params
end subroutine CYANOBACTERIA



subroutine CYANOBACTERIA_BOUYANT &
           (params                      , &
            env                         , &
            CYN_LIGHT_SAT           , &
            NH4_N                   , &
            NO3_N                   , &
            DON                     , &
            PO4_P                   , &
            CYN_C                   , &
            ZOO_C                   , &
            TIME_STEP               , &
            SMITH                   , &
            nkn                     , &
            KG_CYN                  , &
            ALPHA_0                 , &
            ALPHA_1                 , &
            LIM_KG_CYN_TEMP         , &
            LIM_KG_CYN_LIGHT        , &
            LIM_KG_CYN_DOXY         , &
            LIM_KG_CYN_N            , &
            LIM_KG_CYN_P            , &
            LIM_KG_CYN_NUTR         , &
            LIM_KG_CYN              , &
            R_CYN_GROWTH            , &
            R_CYN_MET               , &
            R_CYN_RESP              , &
            R_CYN_EXCR              , &
            R_CYN_INT_RESP          , &
            KD_CYN                  , &
            FAC_HYPOX_CYN_D         , &
            R_CYN_DEATH             , &
            PREF_DIN_DON_CYN        , &
            PREF_NH4N_CYN)

    use AQUABC_II_GLOBAL
    use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
    use para_aqua
    use AQUABC_PELAGIC_TYPES, only: t_cyn_params, t_phyto_env

    implicit none

    ! -------------------------------------------------------------------------
    ! Ingoing variables
    ! -------------------------------------------------------------------------
    type(t_cyn_params),  intent(in) :: params
    type(t_phyto_env),   intent(in) :: env

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NH4_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DON

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PO4_P
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: CYN_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: ZOO_C

    real(kind = DBL_PREC), intent(in) :: TIME_STEP

    integer, intent(in) :: SMITH
    integer, intent(in) :: nkn
    ! -------------------------------------------------------------------------
    ! End of ingoing variables
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Outgoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_0
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_1
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_LIGHT
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_P
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN_NUTR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_MET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_EXCR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_INT_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KD_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FAC_HYPOX_CYN_D
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_CYN_DEATH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PREF_DIN_DON_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PREF_NH4N_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: CYN_LIGHT_SAT
    real(kind = DBL_PREC) :: CYANO_DEPTH   (nkn)     ! Depth where bacteria are concentrated
    real(kind = DBL_PREC) :: EUPHOTIC_DEPTH (nkn)    ! Euphotic depth
    real(kind = DBL_PREC) :: MIX_DEPTH    (nkn)      ! Mixing depth
    integer :: i
    real(kind = DBL_PREC) :: loss, scale_loss
    ! -------------------------------------------------------------------------
    ! End of outgoing variables
    ! -------------------------------------------------------------------------

    associate( &
        KG_CYN_OPT_TEMP          => params%KG_CYN_OPT_TEMP, &
        CYN_OPT_TEMP_LR          => params%CYN_OPT_TEMP_LR, &
        CYN_OPT_TEMP_UR          => params%CYN_OPT_TEMP_UR, &
        EFF_CYN_GROWTH            => params%EFF_CYN_GROWTH, &
        KAPPA_CYN_UNDER_OPT_TEMP => params%KAPPA_CYN_UNDER_OPT_TEMP, &
        KAPPA_CYN_OVER_OPT_TEMP  => params%KAPPA_CYN_OVER_OPT_TEMP, &
        KR_CYN_20                 => params%KR_CYN_20, &
        THETA_KR_CYN              => params%THETA_KR_CYN, &
        KD_CYN_20                 => params%KD_CYN_20, &
        THETA_KD_CYN              => params%THETA_KD_CYN, &
        KHS_DIN_CYN               => params%KHS_DIN_CYN, &
        KHS_DIP_CYN               => params%KHS_DIP_CYN, &
        KHS_O2_CYN                => params%KHS_O2_CYN, &
        FRAC_CYN_EXCR             => params%FRAC_CYN_EXCR, &
        I_S_CYN                   => params%I_S_CYN, &
        BETA_CYN                  => params%BETA_CYN, &
        DO_STR_HYPOX_CYN_D        => params%DO_STR_HYPOX_CYN_D, &
        THETA_HYPOX_CYN_D         => params%THETA_HYPOX_CYN_D, &
        EXPON_HYPOX_CYN_D         => params%EXPON_HYPOX_CYN_D, &
        CYN_N_TO_C                => params%CYN_N_TO_C, &
        CYN_P_TO_C                => params%CYN_P_TO_C, &
        CYN_O2_TO_C               => params%CYN_O2_TO_C, &
        CYN_C_TO_CHLA             => params%CYN_C_TO_CHLA, &
        frac_avail_DON             => params%frac_avail_DON)

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

    ! Growth limitation by temperature factor
    call GROWTH_AT_TEMP &
         (TEMP, LIM_KG_CYN_TEMP, CYN_OPT_TEMP_LR, CYN_OPT_TEMP_UR, KG_CYN_OPT_TEMP,  &
          KAPPA_CYN_UNDER_OPT_TEMP, KAPPA_CYN_OVER_OPT_TEMP,nkn)

    !if( KG_CYN_OPT_TEMP .ne.  0.D0) then
       !LIM_KG_CYN_TEMP = KG_CYN / KG_CYN_OPT_TEMP
       ! Temerature limited growth rate
       KG_CYN = KG_CYN_OPT_TEMP * LIM_KG_CYN_TEMP
    !else
    !   LIM_KG_CYN_TEMP = 0.D0
    !end if


    if (smith .eq. 0) then
        !May be replaced by Smith formulation
        ALPHA_0 = (I_A / I_S_CYN) * safe_exp(-1.0D0 * K_E * 0.0D0)
        ALPHA_1 = (I_A / I_S_CYN) * safe_exp(-1.0D0 * K_E * DEPTH)

        LIM_KG_CYN_LIGHT = &
            (((2.718 * FDAY) / (K_E * DEPTH)) * &
             (safe_exp(-1.0D0 * ALPHA_1) - safe_exp(-1.0D0 * ALPHA_0)))
    end if

    if (smith .eq. 1) then

        ! Introduced by Petras 2019-08-10. The depth in which 1% of ligt is reached
        ! The same for fixers. This fix is valid only for 2d. fixme
        ! Nothing is done to increase selfshading. Concentration is
        ! still evenly distributed to the whole depth. fixme
        EUPHOTIC_DEPTH(:) = 4.61D0 / max(K_E(:), 1.0D-20)

        !Nagy et al. 2006
        MIX_DEPTH(:) = 0.8121D0 * WINDS(:) + 0.7006D0

        where(MIX_DEPTH(:) .le. EUPHOTIC_DEPTH(:) .and. EUPHOTIC_DEPTH(:) .le. DEPTH(:))
            CYANO_DEPTH(:) = EUPHOTIC_DEPTH(:)
        elsewhere(MIX_DEPTH(:) .gt. EUPHOTIC_DEPTH(:) .and. MIX_DEPTH(:) .le. DEPTH(:))
            CYANO_DEPTH(:) = MIX_DEPTH(:)
        elsewhere
            CYANO_DEPTH(:) = DEPTH(:)
        end where

        call LIM_LIGHT&
             (I_A          , CHLA, KG_CYN, CYANO_DEPTH, K_E, LIM_KG_CYN_LIGHT, &
              CYN_C_TO_CHLA, I_S_CYN, CYN_LIGHT_SAT, nkn, BETA_CYN)
    end if



    LIM_KG_CYN_DOXY = DISS_OXYGEN / (KHS_O2_CYN + DISS_OXYGEN)

    LIM_KG_CYN_N    = (NH4_N + (DON * frac_avail_DON) + NO3_N) / &
                      (KHS_DIN_CYN + NH4_N + (DON * frac_avail_DON) + NO3_N)

    LIM_KG_CYN_P    = PO4_P   / (KHS_DIP_CYN + PO4_P)
    ! Synthesizing Unit colimitation (Saito et al. 2008)
    LIM_KG_CYN_NUTR = LIM_KG_CYN_N * LIM_KG_CYN_P / &
        max(LIM_KG_CYN_N + LIM_KG_CYN_P - LIM_KG_CYN_N * LIM_KG_CYN_P, 1.0D-20)
    LIM_KG_CYN      = LIM_KG_CYN_LIGHT*min(LIM_KG_CYN_DOXY, LIM_KG_CYN_NUTR)

    !Non-fixing cyanobacteria growth rate
    R_CYN_GROWTH    = KG_CYN * LIM_KG_CYN * CYN_C

    !Non-fixing cyanobacteria metabolic rate, respiration and excretion
    R_CYN_MET = R_CYN_GROWTH * (1.0D0 - EFF_CYN_GROWTH)
    R_CYN_RESP = (1.D0-FRAC_CYN_EXCR) * R_CYN_MET
    R_CYN_EXCR = FRAC_CYN_EXCR * R_CYN_MET

    !Non-fixing cyanobacteria dark respiration rate
    R_CYN_INT_RESP = &
        KR_CYN_20 * (THETA_KR_CYN ** (TEMP - 2.0D1)) * LIM_KG_CYN_DOXY * CYN_C

    !Calculations for non-fixing cyanobacteria death rate
    KD_CYN = KD_CYN_20 * (THETA_KD_CYN ** (TEMP - 2.0D1))

    FAC_HYPOX_CYN_D = 1.0D0
    if(KD_CYN_20 .gt. 0.D0) then
     where (DISS_OXYGEN <= DO_STR_HYPOX_CYN_D)

         where (DISS_OXYGEN / DO_STR_HYPOX_CYN_D > 1.0D-1)
             FAC_HYPOX_CYN_D = THETA_HYPOX_CYN_D ** &
                  (EXPON_HYPOX_CYN_D * (DO_STR_HYPOX_CYN_D - DISS_OXYGEN))
         elsewhere
             FAC_HYPOX_CYN_D = min(TIME_STEP / (5.0D-1 * KD_CYN), &
                                  9.0D-1 / (KD_CYN * TIME_STEP))
             R_CYN_INT_RESP = 0.0D0
             R_CYN_RESP     = 0.0D0
             R_CYN_GROWTH   = 0.0D0
         end where
     elsewhere
         FAC_HYPOX_CYN_D = 1.0D0
     end where
    end if

    !Non-fixing cyanobacteria death rate
    R_CYN_DEATH = KD_CYN * FAC_HYPOX_CYN_D * CYN_C

    ! Mass-balance safeguard: limit total losses to available biomass per TIME_STEP
    do i = 1, nkn
        if (CYN_C(i) > 0.0D0) then
            loss = R_CYN_DEATH(i) + R_CYN_EXCR(i) + R_CYN_INT_RESP(i) + R_CYN_RESP(i)
            if (loss > 0.5D0 * CYN_C(i) / TIME_STEP) then
                scale_loss = (0.5D0 * CYN_C(i) / TIME_STEP) / loss
                R_CYN_DEATH(i) = R_CYN_DEATH(i) * scale_loss
                R_CYN_EXCR(i) = R_CYN_EXCR(i) * scale_loss
                R_CYN_INT_RESP(i) = R_CYN_INT_RESP(i) * scale_loss
                R_CYN_RESP(i) = R_CYN_RESP(i) * scale_loss
            end if
        end if
    end do

    call DIN_DON_PREFS &
         (PREF_DIN_DON_CYN, NH4_N, DON, frac_avail_DON, NO3_N, KHS_DIN_CYN, nkn)
    call AMMONIA_PREFS &
         (PREF_NH4N_CYN, (NH4_N * PREF_DIN_DON_CYN), (NO3_N * PREF_DIN_DON_CYN), KHS_DIN_CYN, nkn)

    end associate ! env
    end associate ! params
end subroutine CYANOBACTERIA_BOUYANT
