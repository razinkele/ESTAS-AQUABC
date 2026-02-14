! Auxilary routines for the pelagic model

! Contents:
!subroutine NOSTOCALES



! Submodel for the life cycle of Nostacles, a large order of (fixing) Cyanobacteria
subroutine NOSTOCALES &
           (params                                , &
            env                                   , &
            TIME_STEP                         , &
            DAY_OF_YEAR                       , &
            SMITH                             , &
            nkn                               , &
            NOST_LIGHT_SAT                    , &
            DIN                               , &   ! bioavailable DIN
            DON                               , &   ! bioavailable DON
            DP                                , &
            NOST_VEG_HET_C                    , &
            NOST_AKI_C                        , &
            KG_NOST_VEG_HET                   , &
            LIM_KG_NOST_VEG_HET_LIGHT         , &
            LIM_KG_NOST_VEG_HET_TEMP          , &
            LIM_KG_NOST_VEG_HET_DOXY          , &
            LIM_KG_NOST_VEG_HET_N             , &
            LIM_KG_NOST_VEG_HET_P             , &
            LIM_KG_NOST_VEG_HET_FIX           , &
            LIM_KG_NOST_VEG_HET_NON_FIX       , &
            R_NOST_VEG_HET_GROWTH             , &
            R_NOST_VEG_HET_FIX_GROWTH         , &
            R_NOST_VEG_HET_NON_FIX_GROWTH     , &
            R_NOST_VEG_HET_MET                , &
            R_NOST_VEG_HET_RESP               , &
            R_NOST_VEG_HET_EXCR               , &
            R_NOST_VEG_HET_INT_RESP           , &
            RD_NOST_VEG_HET                   , &
            FAC_HYPOX_NOST_VEG_HET_D          , &
            R_NOST_VEG_HET_DEATH              , &
            R_DENS_MORT_NOST_VEG_HET          , &
            R_GERM_NOST_AKI                   , &
            R_FORM_NOST_AKI                   , &
            R_LOSS_AKI                        , &
            R_MORT_AKI)

   use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
   use AQUABC_PELAGIC_TYPES, only: t_nost_params, t_phyto_env
   implicit none

   ! ------------------------------------------------------------------------------------
   ! INGOING VARIABLES
   ! ------------------------------------------------------------------------------------
   type(t_nost_params), intent(in) :: params
   type(t_phyto_env),   intent(in) :: env
   double precision, intent(in) :: TIME_STEP
   integer         , intent(in) :: DAY_OF_YEAR
   integer         , intent(in) :: SMITH
   integer         , intent(in) :: nkn
   double precision, dimension(nkn), intent(in) :: DIN
   double precision, dimension(nkn), intent(in) :: DON
   double precision, dimension(nkn), intent(in) :: DP
   double precision, dimension(nkn), intent(in) :: NOST_VEG_HET_C
   double precision, dimension(nkn), intent(in) :: NOST_AKI_C
   ! ------------------------------------------------------------------------------------


   ! ------------------------------------------------------------------------------------
   ! OUTGOING VARIABLES
   ! ------------------------------------------------------------------------------------
   double precision, dimension(nkn), intent(inout) :: KG_NOST_VEG_HET
   double precision, dimension(nkn), intent(inout) :: LIM_KG_NOST_VEG_HET_LIGHT
   double precision, dimension(nkn), intent(inout) :: LIM_KG_NOST_VEG_HET_TEMP
   double precision, dimension(nkn), intent(inout) :: LIM_KG_NOST_VEG_HET_DOXY
   double precision, dimension(nkn), intent(inout) :: LIM_KG_NOST_VEG_HET_N
   double precision, dimension(nkn), intent(inout) :: LIM_KG_NOST_VEG_HET_P
   double precision, dimension(nkn), intent(inout) :: LIM_KG_NOST_VEG_HET_FIX
   double precision, dimension(nkn), intent(inout) :: LIM_KG_NOST_VEG_HET_NON_FIX
   double precision, dimension(nkn), intent(inout) :: R_NOST_VEG_HET_GROWTH
   double precision, dimension(nkn), intent(inout) :: R_NOST_VEG_HET_FIX_GROWTH
   double precision, dimension(nkn), intent(inout) :: R_NOST_VEG_HET_NON_FIX_GROWTH
   double precision, dimension(nkn), intent(inout) :: R_NOST_VEG_HET_MET
   double precision, dimension(nkn), intent(inout) :: R_NOST_VEG_HET_RESP
   double precision, dimension(nkn), intent(inout) :: R_NOST_VEG_HET_EXCR
   double precision, dimension(nkn), intent(inout) :: R_NOST_VEG_HET_INT_RESP
   double precision, dimension(nkn), intent(inout) :: RD_NOST_VEG_HET
   double precision, dimension(nkn), intent(inout) :: FAC_HYPOX_NOST_VEG_HET_D
   double precision, dimension(nkn), intent(inout) :: R_NOST_VEG_HET_DEATH
   double precision, dimension(nkn), intent(inout) :: R_DENS_MORT_NOST_VEG_HET
   double precision, dimension(nkn), intent(inout) :: R_GERM_NOST_AKI
   double precision, dimension(nkn), intent(inout) :: R_FORM_NOST_AKI
   double precision, dimension(nkn), intent(inout) :: R_LOSS_AKI
   double precision, dimension(nkn), intent(inout) :: R_MORT_AKI
   ! ------------------------------------------------------------------------------------

   ! ------------------------------------------------------------------------------------
   ! AUXILLARY VARIABLES
   ! ------------------------------------------------------------------------------------
   double precision, dimension(nkn) :: ALPHA_0
   double precision, dimension(nkn) :: ALPHA_1
   double precision, dimension(nkn) :: NOST_VEG_HET_DEPTH
   double precision, dimension(nkn) :: EUPHOTIC_DEPTH
   double precision, dimension(nkn) :: MIX_DEPTH
   !double precision, dimension(nkn) :: R_MORT_DENS_NOST_VEG_HET
   double precision, dimension(nkn) :: AKI_GERM ! Germination rate constanst for Akinetes
   double precision, dimension(nkn) :: AKI_FORM ! Formation rate constanst for Akinetes

   double precision NOST_LIGHT_SAT(nkn) !light saturation obtained fom lim_light, just for control

   integer, dimension(nkn)  :: IND_GERM ! Indicator of germination start
   integer :: i
   double precision :: loss, scale_loss
   ! ------------------------------------------------------------------------------------

   associate( &
       KG_NOST_VEG_HET_OPT_TEMP          => params%KG_NOST_VEG_HET_OPT_TEMP, &
       FRAC_NOST_GROWTH                   => params%FRAC_NOST_GROWTH, &
       NOST_VEG_HET_OPT_TEMP_LR          => params%NOST_VEG_HET_OPT_TEMP_LR, &
       NOST_VEG_HET_OPT_TEMP_UR          => params%NOST_VEG_HET_OPT_TEMP_UR, &
       EFF_NOST_VEG_HET_GROWTH           => params%EFF_NOST_VEG_HET_GROWTH, &
       KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP => params%KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP, &
       KAPPA_NOST_VEG_HET_OVER_OPT_TEMP  => params%KAPPA_NOST_VEG_HET_OVER_OPT_TEMP, &
       KR_NOST_VEG_HET_20                => params%KR_NOST_VEG_HET_20, &
       THETA_KR_NOST_VEG_HET             => params%THETA_KR_NOST_VEG_HET, &
       KD_NOST_VEG_HET_20                => params%KD_NOST_VEG_HET_20, &
       THETA_KD_NOST_VEG_HET             => params%THETA_KD_NOST_VEG_HET, &
       KHS_DN_NOST_VEG_HET               => params%KHS_DN_NOST_VEG_HET, &
       KHS_DP_NOST_VEG_HET               => params%KHS_DP_NOST_VEG_HET, &
       KHS_O2_NOST_VEG_HET               => params%KHS_O2_NOST_VEG_HET, &
       I_S_NOST_VEG_HET                  => params%I_S_NOST_VEG_HET, &
       DO_STR_HYPOX_NOST_VEG_HET_D       => params%DO_STR_HYPOX_NOST_VEG_HET_D, &
       THETA_HYPOX_NOST_VEG_HET_D        => params%THETA_HYPOX_NOST_VEG_HET_D, &
       EXPON_HYPOX_NOST_VEG_HET_D        => params%EXPON_HYPOX_NOST_VEG_HET_D, &
       NOST_C_TO_CHLA                    => params%NOST_C_TO_CHLA, &
       FRAC_NOST_VEG_HET_EXCR            => params%FRAC_NOST_VEG_HET_EXCR, &
       KR_GERM_AKI                       => params%KR_GERM_AKI, &
       KN_GERM_AKI                       => params%KN_GERM_AKI, &
       KR_FORM_AKI                       => params%KR_FORM_AKI, &
       DAY_FORM_AKI                      => params%DAY_FORM_AKI, &
       T_FORM_AKI                        => params%T_FORM_AKI, &
       T_GERM_AKI                        => params%T_GERM_AKI, &
       K_LOSS_AKI                        => params%K_LOSS_AKI, &
       K_MORT_AKI_20                     => params%K_MORT_AKI_20, &
       THETA_K_MORT_AKI                  => params%THETA_K_MORT_AKI, &
       KM_DENS_VEG_HET                   => params%KM_DENS_VEG_HET, &
       BETA_NOST_VEG_HET                 => params%BETA_NOST_VEG_HET &
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

   ! ------------------------------------------------------------------------------------
   ! CODE TO CALCULATE THE GROWTH RATE OF VEGATATIVE + HETEROCYST STAGE NOSTACLE CELLS
   ! ------------------------------------------------------------------------------------

   ! ------------------------------------------------------------------------------------
   ! Calculate the temperature limitation factor
   ! ------------------------------------------------------------------------------------
    if(DAY_OF_YEAR .lt. 1) then
      IND_GERM = 0
    end if

    call GROWTH_AT_TEMP &
         (TEMP, LIM_KG_NOST_VEG_HET_TEMP      , NOST_VEG_HET_OPT_TEMP_LR        , &
          NOST_VEG_HET_OPT_TEMP_UR            , KG_NOST_VEG_HET_OPT_TEMP        , &
          KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP   , KAPPA_NOST_VEG_HET_OVER_OPT_TEMP, nkn)

   !LIM_KG_NOST_VEG_HET_TEMP = KG_NOST_VEG_HET / KG_NOST_VEG_HET_OPT_TEMP
   ! Calculate the temperature limited growth
   KG_NOST_VEG_HET = KG_NOST_VEG_HET_OPT_TEMP * LIM_KG_NOST_VEG_HET_TEMP
   ! ------------------------------------------------------------------------------------


   ! ------------------------------------------------------------------------------------
   ! Calculate the light limitation factor
   ! ------------------------------------------------------------------------------------
    if (smith .eq. 0) then
        ALPHA_0 = (I_A / I_S_NOST_VEG_HET) * safe_exp(-1.0D0 * K_E * 0.0D0)
        ALPHA_1 = (I_A / I_S_NOST_VEG_HET) * safe_exp(-1.0D0 * K_E * DEPTH)

        LIM_KG_NOST_VEG_HET_LIGHT = &
            (((2.718 * FDAY) / (K_E * DEPTH)) * &
             (safe_exp(-1.0D0 * ALPHA_1) - safe_exp(-1.0D0 * ALPHA_0)))
    end if

    if (smith .eq. 1) then

        EUPHOTIC_DEPTH(:) = 4.61D0 / max(K_E(:), 1.0D-20)

        write(6,*) 'NOST CALL: I_A=', I_A(1:min(8,nkn))
        write(6,*) 'NOST CALL: CHLA=', CHLA(1:min(8,nkn))
        write(6,*) 'NOST CALL: KG_NOST=', KG_NOST_VEG_HET(1:min(8,nkn))
        write(6,*) 'NOST CALL: NOST_DEPTH=', NOST_VEG_HET_DEPTH(1:min(8,nkn))
        write(6,*) 'NOST CALL: K_E=', K_E(1:min(8,nkn))

        ! Introduced by Petras 2019-08-10. The depth in which 1% of ligt is reached
        ! The same for fixers. This fix is valid only for 2d. fixme
        ! Nothing is done to increase selfshading. Concentration is
        ! still evenly distributed to the whole depth. fixme

        !Nagy et al. 2006
        MIX_DEPTH(:) = 0.8121D0 * WINDS(:) + 0.7006D0

        where((MIX_DEPTH .le. EUPHOTIC_DEPTH) .and. (EUPHOTIC_DEPTH(:) .le. DEPTH(:)))
            NOST_VEG_HET_DEPTH(:) = EUPHOTIC_DEPTH(:)
        elsewhere((MIX_DEPTH(:) .gt. EUPHOTIC_DEPTH(:)) .and. (MIX_DEPTH(:) .le. DEPTH(:)))
            NOST_VEG_HET_DEPTH(:) = MIX_DEPTH(:)
        elsewhere
            NOST_VEG_HET_DEPTH(:) = DEPTH(:)
        end where

        call LIM_LIGHT(I_A, CHLA, KG_NOST_VEG_HET, NOST_VEG_HET_DEPTH, K_E, &
                       LIM_KG_NOST_VEG_HET_LIGHT , NOST_C_TO_CHLA, I_S_NOST_VEG_HET, &
                       NOST_LIGHT_SAT, nkn, BETA_NOST_VEG_HET)
    end if
   ! ------------------------------------------------------------------------------------

   ! ------------------------------------------------------------------------------------
   ! Calculate the nutrient limitation factor. Since this sub-model assumes obligatory
   ! nitrogen fixation as nitrogen uptake mechanism, the only limiting nutrient will be
   ! phosphorus in form dissolved phosphorus.
   ! ------------------------------------------------------------------------------------
   LIM_KG_NOST_VEG_HET_P = DP / (KHS_DP_NOST_VEG_HET + DP)
   ! ------------------------------------------------------------------------------------
   ! Limitation by dissolved nitrogen
   LIM_KG_NOST_VEG_HET_N = (DIN + DON) / (KHS_DN_NOST_VEG_HET + DIN + DON)
   ! ------------------------------------------------------------------------------------
   ! Limitation of growth by dissolved oxygen
   ! ------------------------------------------------------------------------------------
   LIM_KG_NOST_VEG_HET_DOXY = DISS_OXYGEN / (KHS_O2_NOST_VEG_HET + DISS_OXYGEN)
   ! ------------------------------------------------------------------------------------

   LIM_KG_NOST_VEG_HET_FIX = &
         LIM_KG_NOST_VEG_HET_LIGHT * min(LIM_KG_NOST_VEG_HET_DOXY, LIM_KG_NOST_VEG_HET_P)
   ! Non-fixing fraction: Synthesizing Unit colimitation for N-P (Saito et al. 2008)
   LIM_KG_NOST_VEG_HET_NON_FIX = &
         LIM_KG_NOST_VEG_HET_LIGHT * min(LIM_KG_NOST_VEG_HET_DOXY, &
         LIM_KG_NOST_VEG_HET_P * LIM_KG_NOST_VEG_HET_N / &
         max(LIM_KG_NOST_VEG_HET_P + LIM_KG_NOST_VEG_HET_N - &
             LIM_KG_NOST_VEG_HET_P * LIM_KG_NOST_VEG_HET_N, 1.0D-20))

   R_NOST_VEG_HET_FIX_GROWTH     = FRAC_NOST_GROWTH * KG_NOST_VEG_HET * LIM_KG_NOST_VEG_HET_FIX * NOST_VEG_HET_C
   R_NOST_VEG_HET_NON_FIX_GROWTH = (1.D0 -FRAC_NOST_GROWTH) * KG_NOST_VEG_HET * LIM_KG_NOST_VEG_HET_NON_FIX * NOST_VEG_HET_C
   R_NOST_VEG_HET_GROWTH         = R_NOST_VEG_HET_FIX_GROWTH + R_NOST_VEG_HET_NON_FIX_GROWTH
   ! ------------------------------------------------------------------------------------
   ! END OF CODE TO CALCULATE THE GROWTH RATE OF VEGATATIVE + HETEROCYST STAGE
   ! NOSTACLE CELLS
   ! ------------------------------------------------------------------------------------



    !Vegeatative + heterocyst stage nostacle metabolism, respiration, excretion rate
    R_NOST_VEG_HET_MET  = R_NOST_VEG_HET_GROWTH * (1.0D0 - EFF_NOST_VEG_HET_GROWTH)
    R_NOST_VEG_HET_RESP = (1.0D0 - FRAC_NOST_VEG_HET_EXCR) * R_NOST_VEG_HET_MET
    R_NOST_VEG_HET_EXCR = FRAC_NOST_VEG_HET_EXCR * R_NOST_VEG_HET_MET

    !Vegeatative + heterocyst stage nostacles dark respiration rate
    R_NOST_VEG_HET_INT_RESP = &
        KR_NOST_VEG_HET_20 * (THETA_KR_NOST_VEG_HET ** (TEMP - 2.0D1)) * &
        LIM_KG_NOST_VEG_HET_DOXY * NOST_VEG_HET_C

    ! ------------------------------------------------------------------------------------
    ! CODE TO CALCULATE THE MORTALITY RATE OF VEGATATIVE + HETEROCYST STAGE NOSTACLE CELLS
    ! ------------------------------------------------------------------------------------

    !Vegeatative + heterocyst stage nostacles death rate
    RD_NOST_VEG_HET = KD_NOST_VEG_HET_20 * (THETA_KD_NOST_VEG_HET ** (TEMP - 2.0D1))


    FAC_HYPOX_NOST_VEG_HET_D = 1.0D0

    if(KD_NOST_VEG_HET_20 .gt. 0.0D0) then
     where (DISS_OXYGEN <= DO_STR_HYPOX_NOST_VEG_HET_D)

         where (DISS_OXYGEN / DO_STR_HYPOX_NOST_VEG_HET_D > 1.0D-1)
             FAC_HYPOX_NOST_VEG_HET_D = THETA_HYPOX_NOST_VEG_HET_D ** &
                  (EXPON_HYPOX_NOST_VEG_HET_D * (DO_STR_HYPOX_NOST_VEG_HET_D - DISS_OXYGEN))
         elsewhere
             FAC_HYPOX_NOST_VEG_HET_D = min(TIME_STEP / (5.0D-1 * RD_NOST_VEG_HET), &
                                          9.0D-1 / (RD_NOST_VEG_HET * TIME_STEP))
             R_NOST_VEG_HET_INT_RESP = 0.0D0
             R_NOST_VEG_HET_RESP     = 0.0D0
             R_NOST_VEG_HET_GROWTH   = 0.0D0
         end where
     elsewhere
         FAC_HYPOX_NOST_VEG_HET_D = 1.0D0
     end where
    end if

    !Vegeatative + heterocyst stage nostacles death rate
    R_NOST_VEG_HET_DEATH = RD_NOST_VEG_HET * FAC_HYPOX_NOST_VEG_HET_D * NOST_VEG_HET_C

    ! Mass-balance safeguard: limit total losses to available biomass per TIME_STEP
    do i = 1, nkn
        if (NOST_VEG_HET_C(i) > 0.0D0) then
            loss = R_NOST_VEG_HET_DEATH(i) + R_NOST_VEG_HET_EXCR(i) + &
                   R_NOST_VEG_HET_INT_RESP(i) + R_NOST_VEG_HET_RESP(i)
            if (loss > 0.5D0 * NOST_VEG_HET_C(i) / TIME_STEP) then
                scale_loss = (0.5D0 * NOST_VEG_HET_C(i) / TIME_STEP) / loss
                R_NOST_VEG_HET_DEATH(i) = R_NOST_VEG_HET_DEATH(i) * scale_loss
                R_NOST_VEG_HET_EXCR(i) = R_NOST_VEG_HET_EXCR(i) * scale_loss
                R_NOST_VEG_HET_INT_RESP(i) = R_NOST_VEG_HET_INT_RESP(i) * scale_loss
                R_NOST_VEG_HET_RESP(i) = R_NOST_VEG_HET_RESP(i) * scale_loss
            end if
        end if
    end do

    ! ------------------------------------------------------------------------------------
    ! END OF CODE TO CALCULATE THE MORTALITY RATE OF VEGATATIVE + HETEROCYST STAGE
    ! NOSTACLE CELLS
    ! ------------------------------------------------------------------------------------


    ! ------------------------------------------------------------------------------------
    ! CODE TO CALCULATE THE DENSITY MORTALITY RATE OF VEGATATIVE + HETEROCYST STAGE
    ! NOSTACLE CELLS
    ! ------------------------------------------------------------------------------------

    ! This process is simulating the carrying capacity and is considered as a second
    ! order process related to biovolume


    !R_MORT_DENS_NOST_VEG_HET = M_DENS_VEG_HET * NOST_VEG_HET_C * NOST_VEG_HET_C
    R_DENS_MORT_NOST_VEG_HET = KM_DENS_VEG_HET * NOST_VEG_HET_C * NOST_VEG_HET_C


    ! ------------------------------------------------------------------------------------
    ! END OF CODE TO CALCULATE THE DENSITY MORTALITY RATE OF VEGATATIVE + HETEROCYST STAGE
    ! NOSTACLE CELLS
    ! ------------------------------------------------------------------------------------


    ! ------------------------------------------------------------------------------------
    ! CODE TO CALCULATE THE GERMINATION RATE OF NOSTACLE AKINETS
    ! ------------------------------------------------------------------------------------
    where (DIN < KN_GERM_AKI .and. TEMP > T_GERM_AKI)
        AKI_GERM = KR_GERM_AKI
        IND_GERM = 1
    elsewhere
        AKI_GERM = 0.0D0
    end where

    R_GERM_NOST_AKI = AKI_GERM * NOST_AKI_C

    ! ------------------------------------------------------------------------------------
    ! END OF CODE TO CALCULATE THE GERMINATION RATE OF NOSTACLE AKINETS
    ! ------------------------------------------------------------------------------------


    ! ------------------------------------------------------------------------------------
    ! CODE TO CALCULATE THE FORMATION RATE OF NOSTACLE AKINETS
    ! ------------------------------------------------------------------------------------
    where ((TEMP < T_FORM_AKI).and.(DAY_OF_YEAR > int(DAY_FORM_AKI) .and. DAY_OF_YEAR < 365))
        AKI_FORM = KR_FORM_AKI
    elsewhere
        AKI_FORM = 0.0D0
    end where

    R_FORM_NOST_AKI = AKI_FORM * NOST_VEG_HET_C
    ! ------------------------------------------------------------------------------------
    ! END OF CODE TO CALCULATE THE FORMATION RATE OF NOSTACLE AKINETS
    ! ------------------------------------------------------------------------------------


    ! ------------------------------------------------------------------------------------
    ! CODE TO LOSS RATE OF NOSTACLE AKINETS
    ! ------------------------------------------------------------------------------------
    R_LOSS_AKI = K_LOSS_AKI      * NOST_AKI_C
    R_MORT_AKI = K_MORT_AKI_20 * (THETA_K_MORT_AKI**(TEMP - 20.0D0)) * NOST_AKI_C

   end associate ! env
   end associate ! params

end subroutine NOSTOCALES
