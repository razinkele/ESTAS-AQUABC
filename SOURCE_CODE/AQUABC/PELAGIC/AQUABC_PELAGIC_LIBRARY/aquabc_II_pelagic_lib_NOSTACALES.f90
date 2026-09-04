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
            R_MORT_AKI                        , &
            CYANO_POS_MODEL                   , &
            H_SURF_POS                   , &
            W_CRIT_POS_MIN               , &
            S_CHUNK                           , &
            NOST_STAGE_MODEL                  , &   ! integer, intent(in)
            BED_AKI_CHUNK                     , &   ! real(nkn), intent(in)   g C/m2
            FORM_LATCH_CHUNK                  , &   ! logical(nkn), intent(in)
            SETTLE_FLUX_CHUNK                 , &   ! real(nkn), intent(out)  g C/m2/d
            GERM_FLUX_CHUNK                   , &   ! real(nkn), intent(out)  g C/m2/d
            FORM_FLUX_CHUNK                   , &   ! real(nkn), intent(out)  g C/m2/d (= R_FORM_NOST_AKI*env%DEPTH)
            GERM_COND_CHUNK                   , &   ! logical(nkn), intent(out)
            R_GERM_BED_AKI                    , &   ! real(nkn), intent(out)  g C/m3/d (VEG source)
            R_SETTLE_AKI)                            ! real(nkn), intent(out)  g C/m3/d (AKI_C sink)

   use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
   use AQUABC_POSITIONING_STATE, only: CALM_FRACTION, K_POS_UP, K_POS_DISP, W_DISP_POS, KD_PER_CHL_POS
   use AQUABC_PELAGIC_TYPES, only: t_nost_params, t_phyto_env
   use AQUABC_NOST_STAGING, only: KR_GERM_BED, V_SETTLE_AKI, T_GERM_AKI_STAGE, EPS_GERM_TEMP_LIM
   use AQUABC_II_GLOBAL, only: NOST_FIX_SWITCH, K_FIX_NOST, R_FIX_NOST
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
   ! Sub-daily surface-positioning gate (0 = legacy, 1 = calm-fraction blend)
   integer, intent(in) :: CYANO_POS_MODEL
   double precision, intent(in) :: H_SURF_POS
   double precision, intent(in) :: W_CRIT_POS_MIN
   ! surface-positioned fraction state slice (module S_POS); updated when
   ! CYANO_POS_MODEL >= 2, inert zeros otherwise
   double precision, dimension(nkn), intent(inout) :: S_CHUNK
   ! NOST akinete life-cycle staging (0 = legacy akinete gates, default; 1 = bed
   ! akinete bank with growth-viability-gated germination and a latch-driven
   ! formation switch -- spec docs/superpowers/specs/2026-08-23-nost-akinete-
   ! staging-design.md secs 4.2/4.3). Inert (all outs zero/false) when 0.
   integer, intent(in) :: NOST_STAGE_MODEL
   double precision, dimension(nkn), intent(in) :: BED_AKI_CHUNK
   logical, dimension(nkn), intent(in) :: FORM_LATCH_CHUNK
   double precision, dimension(nkn), intent(out) :: SETTLE_FLUX_CHUNK
   double precision, dimension(nkn), intent(out) :: GERM_FLUX_CHUNK
   double precision, dimension(nkn), intent(out) :: FORM_FLUX_CHUNK
   logical, dimension(nkn), intent(out) :: GERM_COND_CHUNK
   double precision, dimension(nkn), intent(out) :: R_GERM_BED_AKI
   double precision, dimension(nkn), intent(out) :: R_SETTLE_AKI
   ! ------------------------------------------------------------------------------------

   ! ------------------------------------------------------------------------------------
   ! AUXILLARY VARIABLES
   ! ------------------------------------------------------------------------------------
   double precision, dimension(nkn) :: ALPHA_0
   double precision, dimension(nkn) :: ALPHA_1
   double precision, dimension(nkn) :: NOST_VEG_HET_DEPTH
   double precision, dimension(nkn) :: EUPHOTIC_DEPTH
   double precision, dimension(nkn) :: MIX_DEPTH
   ! work arrays for the sub-daily positioning blend (CYANO_POS_MODEL = 1)
   double precision, dimension(nkn) :: X_POS, F_CALM, H_SURF_ARR, LIM_SURF, SAT_SCRATCH, K_SURF_POS
   !double precision, dimension(nkn) :: R_MORT_DENS_NOST_VEG_HET
   double precision, dimension(nkn) :: AKI_GERM ! Germination rate constanst for Akinetes
   double precision, dimension(nkn) :: AKI_FORM ! Formation rate constanst for Akinetes
   double precision, dimension(nkn) :: FRAC_FIX_EFF ! effective fixation share of growth

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
        ! I_A is a DAILY INTEGRAL, so the P-I curve must see the DAYLIGHT-MEAN
        ! irradiance I_A/FDAY; the result is then weighted by FDAY below.
        ALPHA_0 = I_A / (max(1.0D-6, min(1.0D0, FDAY)) * I_S_NOST_VEG_HET)
        ALPHA_1 = ALPHA_0 * safe_exp(-1.0D0 * K_E * DEPTH)

        LIM_KG_NOST_VEG_HET_LIGHT = &
            (((2.718 * max(1.0D-6, min(1.0D0, FDAY))) / (K_E * DEPTH)) * &
             (safe_exp(-1.0D0 * ALPHA_1) - safe_exp(-1.0D0 * ALPHA_0)))
        ! Clamp to [0,1]: Steele formula can produce tiny negatives at dusk
        LIM_KG_NOST_VEG_HET_LIGHT = max(0.0D0, min(1.0D0, LIM_KG_NOST_VEG_HET_LIGHT))
    end if

    if (smith .eq. 1) then

        EUPHOTIC_DEPTH(:) = 4.61D0 / max(K_E(:), 1.0D-20)

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
                       NOST_LIGHT_SAT, nkn, BETA_NOST_VEG_HET, FDAY)

        ! ------------------------------------------------------------------
        ! Sub-daily surface-positioning blend (CYANO_POS_MODEL = 1, opt-in).
        ! F_CALM = fraction of the day with hourly wind below the
        ! positioning-critical speed W_crit (MIX(W)=euphotic), from the
        ! within-day W_h/W_day CDF fitted on ERA5 hourly Nida 2012-2022
        ! (96,432 h; ln F quadratic in ln x, max error < 0.05 for x <= 1).
        ! That fraction of the day the population experiences the surface
        ! layer H_SURF_POS instead of the cascade depth; x capped at 1
        ! because beyond it the cascade above already positions.
        ! ------------------------------------------------------------------
        if (CYANO_POS_MODEL == 1) then
            X_POS = max((EUPHOTIC_DEPTH - 0.7006D0) / 0.8121D0, W_CRIT_POS_MIN, 0.0D0)
            F_CALM = CALM_FRACTION(WINDS, X_POS)
            H_SURF_ARR = min(H_SURF_POS, DEPTH)
            call LIM_LIGHT(I_A, CHLA, KG_NOST_VEG_HET, H_SURF_ARR, K_E, &
                 LIM_SURF, NOST_C_TO_CHLA, I_S_NOST_VEG_HET, SAT_SCRATCH, nkn, BETA_NOST_VEG_HET, FDAY)
            LIM_KG_NOST_VEG_HET_LIGHT = (1.0D0 - F_CALM) * LIM_KG_NOST_VEG_HET_LIGHT + F_CALM * LIM_SURF
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
            ! Concentrated self-shading: the positioned fraction packs the
            ! group's biomass into the surface layer, so that layer sees the
            ! group's chlorophyll in excess of the column average.
            ! excess (ug/L) = C[mg C/L]*1000/CChl * S * (H/H_surf - 1)
            K_SURF_POS = K_E + KD_PER_CHL_POS * &
                max(NOST_VEG_HET_C * 1.0D3 / NOST_C_TO_CHLA * S_CHUNK * &
                    (DEPTH / max(H_SURF_ARR, 1.0D-2) - 1.0D0), 0.0D0)
            call LIM_LIGHT(I_A, CHLA, KG_NOST_VEG_HET, H_SURF_ARR, K_SURF_POS, &
                 LIM_SURF, NOST_C_TO_CHLA, I_S_NOST_VEG_HET, SAT_SCRATCH, nkn, BETA_NOST_VEG_HET, FDAY)
            LIM_KG_NOST_VEG_HET_LIGHT = (1.0D0 - F_CALM) * LIM_KG_NOST_VEG_HET_LIGHT + F_CALM * LIM_SURF
        end if
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

   ! ------------------------------------------------------------------------------------
   ! Fixation share of growth (NOST_FIX_SWITCH, mod_AQUABC_II_GLOBAL.f90).
   !
   !   0 (default) legacy: a FIXED share, independent of nitrogen. Measured
   !     consequence (doc s.51.3): fixation then supplies only 10-21 % of this
   !     guild's growth, so the guild carrying the entire modelled fixer biomass
   !     grows 80-90 % on DIN and competes head-to-head with CYN instead of
   !     occupying a diazotroph niche.
   !   1 DIN-gated: an inverse Monod in dissolved nitrogen -- fixation is
   !     suppressed while N is available and enabled as it depletes, which is
   !     heterocyst induction. Mirrors the switch FIX_CYN already uses
   !     (aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90:205); s.31's role swap moved
   !     the fixer role to THIS guild, which lacked it.
   !
   ! DIN here is (NH4 + NO3) and DON arrives already scaled by
   ! frac_avail_DON_NOST, so the expression matches FIX_CYN's term for term.
   ! Option 0 leaves FRAC_FIX_EFF = FRAC_NOST_GROWTH, reproducing the original
   ! two lines exactly.
   ! ------------------------------------------------------------------------------------
   if (NOST_FIX_SWITCH > 0) then
       FRAC_FIX_EFF = K_FIX_NOST / (K_FIX_NOST + DIN + DON)
   else
       FRAC_FIX_EFF = FRAC_NOST_GROWTH
   end if

   ! R_FIX_NOST scales the FIXING channel only (mirrors FIX_CYN's R_FIX, WCONST 73).
   ! < 1 is an energetic cost on N2 fixation; 1.0 (default) is no cost and is
   ! byte-identical to the pre-existing expression.
   R_NOST_VEG_HET_FIX_GROWTH     = R_FIX_NOST * FRAC_FIX_EFF * KG_NOST_VEG_HET * LIM_KG_NOST_VEG_HET_FIX * NOST_VEG_HET_C
   R_NOST_VEG_HET_NON_FIX_GROWTH = (1.D0 - FRAC_FIX_EFF) * KG_NOST_VEG_HET * LIM_KG_NOST_VEG_HET_NON_FIX * NOST_VEG_HET_C
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
    ! CODE TO CALCULATE THE GERMINATION AND FORMATION RATES OF NOSTACLE AKINETS
    ! NOST_STAGE_MODEL > 0 (opt-in, spec secs 4.2/4.3) replaces both legacy season-gated
    ! where-blocks with a bed akinete bank: germination is growth-viability gated and
    ! mutually exclusive with the formation latch; formation is latch-driven only (the
    ! rate constant KR_FORM_AKI is unchanged). NOST_STAGE_MODEL = 0 (default) keeps the
    ! legacy blocks verbatim and zeroes every new export -- byte-identical.
    ! ------------------------------------------------------------------------------------
    if (NOST_STAGE_MODEL > 0) then
        ! germination: bed-only, growth-viability gated (spec s.4.2)
        GERM_COND_CHUNK = (DIN < KN_GERM_AKI) .and. &
                          (LIM_KG_NOST_VEG_HET_TEMP > EPS_GERM_TEMP_LIM) .and. &
                          (TEMP > T_GERM_AKI_STAGE)
        where (GERM_COND_CHUNK .and. .not. FORM_LATCH_CHUNK)
            GERM_FLUX_CHUNK = KR_GERM_BED * BED_AKI_CHUNK
        elsewhere
            GERM_FLUX_CHUNK = 0.0D0
        end where
        R_GERM_BED_AKI  = GERM_FLUX_CHUNK / DEPTH
        R_GERM_NOST_AKI = 0.0D0                       ! water-pool germination off
        ! formation: latch-driven (spec s.4.3); rate constant unchanged
        where (FORM_LATCH_CHUNK)
            AKI_FORM = KR_FORM_AKI
        elsewhere
            AKI_FORM = 0.0D0
        end where
        R_FORM_NOST_AKI = AKI_FORM * NOST_VEG_HET_C
        ! settling of water akinetes toward the bed
        SETTLE_FLUX_CHUNK = V_SETTLE_AKI * NOST_AKI_C
        R_SETTLE_AKI      = SETTLE_FLUX_CHUNK / DEPTH
        FORM_FLUX_CHUNK   = R_FORM_NOST_AKI * DEPTH   ! diagnostic export (CUM_FORM/V6 ratio)
    else
        ! ------------------------------------------------------------------------------------
        ! CODE TO CALCULATE THE GERMINATION RATE OF NOSTACLE AKINETS (legacy, verbatim)
        ! ------------------------------------------------------------------------------------
        where (DIN < KN_GERM_AKI .and. TEMP > T_GERM_AKI)
            AKI_GERM = KR_GERM_AKI
            IND_GERM = 1
        elsewhere
            AKI_GERM = 0.0D0
        end where

        R_GERM_NOST_AKI = AKI_GERM * NOST_AKI_C

        ! ------------------------------------------------------------------------------------
        ! CODE TO CALCULATE THE FORMATION RATE OF NOSTACLE AKINETS (legacy, verbatim)
        ! ------------------------------------------------------------------------------------
        where ((TEMP < T_FORM_AKI).and.(DAY_OF_YEAR > int(DAY_FORM_AKI) .and. DAY_OF_YEAR < 365))
            AKI_FORM = KR_FORM_AKI
        elsewhere
            AKI_FORM = 0.0D0
        end where

        R_FORM_NOST_AKI = AKI_FORM * NOST_VEG_HET_C

        ! Staging exports are inert under the legacy path
        GERM_COND_CHUNK = .false.; GERM_FLUX_CHUNK = 0.0D0; SETTLE_FLUX_CHUNK = 0.0D0
        FORM_FLUX_CHUNK = 0.0D0
        R_GERM_BED_AKI = 0.0D0;    R_SETTLE_AKI = 0.0D0
    end if
    ! ------------------------------------------------------------------------------------
    ! END OF CODE TO CALCULATE THE GERMINATION AND FORMATION RATES OF NOSTACLE AKINETS
    ! ------------------------------------------------------------------------------------


    ! ------------------------------------------------------------------------------------
    ! CODE TO LOSS RATE OF NOSTACLE AKINETS
    ! ------------------------------------------------------------------------------------
    R_LOSS_AKI = K_LOSS_AKI      * NOST_AKI_C
    R_MORT_AKI = K_MORT_AKI_20 * (THETA_K_MORT_AKI**(TEMP - 20.0D0)) * NOST_AKI_C

   end associate ! env
   end associate ! params

end subroutine NOSTOCALES
