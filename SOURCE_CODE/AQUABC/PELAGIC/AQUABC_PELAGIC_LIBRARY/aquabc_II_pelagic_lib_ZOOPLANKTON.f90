! Auxilary routines for the pelagic model

! Contents:
!subroutine ZOOPLANKTON

subroutine ZOOPLANKTON &
           (KG_ZOO_OPT_TEMP               , &
            ZOO_OPT_TEMP_LR               , &
            ZOO_OPT_TEMP_UR               , &
            EFF_ZOO_GROWTH                , &
            KAPPA_ZOO_UNDER_OPT_TEMP      , &
            KAPPA_ZOO_OVER_OPT_TEMP       , &
            GRAT_ZOO_DIA                  , &
            GRAT_ZOO_CYN                  , &
            GRAT_ZOO_OPA                  , &
            GRAT_ZOO_FIX_CYN              , &
            GRAT_ZOO_NOST_VEG_HET         , &
            GRAT_ZOO_DET_PART_ORG_C       , &
            PREF_ZOO_DIA                  , &
            PREF_ZOO_CYN                  , &
            PREF_ZOO_FIX_CYN              , &
            PREF_ZOO_NOST_VEG_HET         , &
            PREF_ZOO_OPA                  , &
            PREF_ZOO_DET_PART_ORG_C       , &
            KHS_DIA_C_ZOO                 , &
            KHS_CYN_C_ZOO                 , &
            KHS_FIX_CYN_C_ZOO             , &
            KHS_NOST_VEG_HET_C_ZOO        , &
            KHS_OPA_C_ZOO                 , &
            KHS_DET_PART_ORG_C_ZOO        , &
            FOOD_MIN_ZOO                  , &
            KE_ZOO                        , &
            FRAC_ZOO_EX_ORG               , &
            KR_ZOO_20                     , &
            THETA_KR_ZOO                  , &
            KD_ZOO_20                     , &
            THETA_KD_ZOO                  , &
            DO_STR_HYPOX_ZOO_D            , &
            THETA_HYPOX_ZOO_D             , &
            EXPON_HYPOX_ZOO_D             , &
            ZOO_N_TO_C                    , &
            ZOO_P_TO_C                    , &
            ZOO_O2_TO_C                   , &
            TEMP                          , &
            DISS_OXYGEN                   , &
            DIA_C                         , &
            CYN_C                         , &
            OPA_C                         , &
            FIX_CYN_C                     , &
            NOST_VEG_HET_C                , &
            DET_PART_ORG_C                , &
            ZOO_C                         , &
            TIME_STEP                     , &
            nkn                           , &
            KG_ZOO                        , &
            KG_ZOO_DIA                    , &
            KG_ZOO_CYN                    , &
            KG_ZOO_OPA                    , &
            KG_ZOO_FIX_CYN                , &
            KG_ZOO_NOST_VEG_HET           , &
            KG_ZOO_DET_PART_ORG_C         , &
            KD_ZOO                        , &
            FOOD_FACTOR_ZOO_DIA           , &
            FOOD_FACTOR_ZOO_CYN           , &
            FOOD_FACTOR_ZOO_OPA           , &
            FOOD_FACTOR_ZOO_FIX_CYN       , &
            FOOD_FACTOR_ZOO_NOST_VEG_HET  , &
            FOOD_FACTOR_ZOO_DET_PART_ORG_C, &
            R_ZOO_FEEDING_DIA             , &
            R_ZOO_FEEDING_CYN             , &
            R_ZOO_FEEDING_FIX_CYN         , &
            R_ZOO_FEEDING_NOST_VEG_HET    , &
            R_ZOO_FEEDING_OPA             , &
            R_ZOO_FEEDING_DET_PART_ORG_C  , &
            R_ZOO_INT_RESP                , &
            R_ZOO_RESP                    , &
            R_ZOO_EX_DON                  , &
            R_ZOO_EX_DOP                  , &
            R_ZOO_EX_DOC                  , &
            R_ZOO_DEATH                   , &
            ACTUAL_ZOO_N_TO_C             , &
            ACTUAL_ZOO_P_TO_C             , &
            R_ZOO_GROWTH                  , &
            FAC_HYPOX_ZOO_D)

    use AQUABC_II_GLOBAL
    implicit none

    ! -------------------------------------------------------------------------
    ! Ingoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), intent(in) :: KG_ZOO_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: ZOO_OPT_TEMP_LR
    real(kind = DBL_PREC), intent(in) :: ZOO_OPT_TEMP_UR
    real(kind = DBL_PREC), intent(in) :: EFF_ZOO_GROWTH
    real(kind = DBL_PREC), intent(in) :: KAPPA_ZOO_UNDER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KAPPA_ZOO_OVER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: GRAT_ZOO_DIA
    real(kind = DBL_PREC), intent(in) :: GRAT_ZOO_CYN
    real(kind = DBL_PREC), intent(in) :: GRAT_ZOO_OPA
    real(kind = DBL_PREC), intent(in) :: GRAT_ZOO_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: GRAT_ZOO_NOST_VEG_HET
    real(kind = DBL_PREC), intent(in) :: GRAT_ZOO_DET_PART_ORG_C
    real(kind = DBL_PREC), intent(in) :: PREF_ZOO_DIA
    real(kind = DBL_PREC), intent(in) :: PREF_ZOO_CYN
    real(kind = DBL_PREC), intent(in) :: PREF_ZOO_FIX_CYN
    real(kind = DBL_PREC), intent(in) :: PREF_ZOO_NOST_VEG_HET
    real(kind = DBL_PREC), intent(in) :: PREF_ZOO_OPA
    real(kind = DBL_PREC), intent(in) :: PREF_ZOO_DET_PART_ORG_C
    real(kind = DBL_PREC), intent(in) :: KHS_DIA_C_ZOO
    real(kind = DBL_PREC), intent(in) :: KHS_CYN_C_ZOO
    real(kind = DBL_PREC), intent(in) :: KHS_FIX_CYN_C_ZOO
    real(kind = DBL_PREC), intent(in) :: KHS_NOST_VEG_HET_C_ZOO
    real(kind = DBL_PREC), intent(in) :: KHS_OPA_C_ZOO
    real(kind = DBL_PREC), intent(in) :: KHS_DET_PART_ORG_C_ZOO
    real(kind = DBL_PREC), intent(in) :: FOOD_MIN_ZOO
    real(kind = DBL_PREC), intent(in) :: KE_ZOO
    real(kind = DBL_PREC), intent(in) :: FRAC_ZOO_EX_ORG
    real(kind = DBL_PREC), intent(in) :: KR_ZOO_20
    real(kind = DBL_PREC), intent(in) :: THETA_KR_ZOO
    real(kind = DBL_PREC), intent(in) :: KD_ZOO_20
    real(kind = DBL_PREC), intent(in) :: THETA_KD_ZOO
    real(kind = DBL_PREC), intent(in) :: DO_STR_HYPOX_ZOO_D
    real(kind = DBL_PREC), intent(in) :: THETA_HYPOX_ZOO_D
    real(kind = DBL_PREC), intent(in) :: EXPON_HYPOX_ZOO_D
    real(kind = DBL_PREC), intent(in) :: ZOO_N_TO_C
    real(kind = DBL_PREC), intent(in) :: ZOO_P_TO_C
    real(kind = DBL_PREC), intent(in) :: ZOO_O2_TO_C

    real(kind = DBL_PREC), intent(in) :: TIME_STEP
    integer, intent(in) :: nkn

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DISS_OXYGEN
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DIA_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: CYN_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: OPA_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FIX_CYN_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NOST_VEG_HET_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DET_PART_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: ZOO_C
    ! -------------------------------------------------------------------------
    ! End of ingoing variables
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Outgoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_ZOO
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_ZOO_DIA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_ZOO_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_ZOO_OPA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_ZOO_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_ZOO_NOST_VEG_HET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_ZOO_DET_PART_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KD_ZOO
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FOOD_FACTOR_ZOO_DIA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FOOD_FACTOR_ZOO_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FOOD_FACTOR_ZOO_OPA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FOOD_FACTOR_ZOO_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FOOD_FACTOR_ZOO_NOST_VEG_HET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FOOD_FACTOR_ZOO_DET_PART_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_FEEDING_DIA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_FEEDING_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_FEEDING_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_FEEDING_NOST_VEG_HET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_FEEDING_OPA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_FEEDING_DET_PART_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_INT_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_EX_DON
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_EX_DOP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_EX_DOC
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_DEATH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ACTUAL_ZOO_N_TO_C
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ACTUAL_ZOO_P_TO_C
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ZOO_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FAC_HYPOX_ZOO_D

    real(kind = DBL_PREC), dimension(nkn) ::LIM_TEMP_ZOO
    integer :: i
    real(kind = DBL_PREC) :: pred_limit
    
    ! Active switching model variables
    real(kind = DBL_PREC), dimension(nkn) :: TOTAL_FOOD
    real(kind = DBL_PREC), dimension(nkn) :: DYN_PREF_DIA
    real(kind = DBL_PREC), dimension(nkn) :: DYN_PREF_CYN
    real(kind = DBL_PREC), dimension(nkn) :: DYN_PREF_OPA
    real(kind = DBL_PREC), dimension(nkn) :: DYN_PREF_FIX_CYN
    real(kind = DBL_PREC), dimension(nkn) :: DYN_PREF_NOST
    real(kind = DBL_PREC), dimension(nkn) :: DYN_PREF_DET
    real(kind = DBL_PREC) :: SWITCHING_POWER
    real(kind = DBL_PREC) :: loss, scale_loss
    ! -------------------------------------------------------------------------
    ! End of outgoing variables
    ! -------------------------------------------------------------------------

    !Zooplankton growth limitation by temperature
    call GROWTH_AT_TEMP &
         (TEMP,LIM_TEMP_ZOO, ZOO_OPT_TEMP_LR, ZOO_OPT_TEMP_UR, KG_ZOO_OPT_TEMP,  &
          KAPPA_ZOO_UNDER_OPT_TEMP, KAPPA_ZOO_OVER_OPT_TEMP,nkn)
    ! Zooplankton temperature limited growth
    KG_ZOO = KG_ZOO_OPT_TEMP * LIM_TEMP_ZOO

    KG_ZOO_DIA            = KG_ZOO * GRAT_ZOO_DIA
    KG_ZOO_CYN            = KG_ZOO * GRAT_ZOO_CYN
    KG_ZOO_OPA            = KG_ZOO * GRAT_ZOO_OPA
    KG_ZOO_FIX_CYN        = KG_ZOO * GRAT_ZOO_FIX_CYN
    KG_ZOO_NOST_VEG_HET   = KG_ZOO * GRAT_ZOO_NOST_VEG_HET
    KG_ZOO_DET_PART_ORG_C = KG_ZOO * GRAT_ZOO_DET_PART_ORG_C

    ! =========================================================================
    ! ACTIVE SWITCHING MODEL
    ! Dynamic preferences scale with prey's fraction of total available food
    ! This creates adaptive behavior: zooplankton shift effort to abundant prey
    ! Reference: Gentleman et al. (2003), Kiorboe (2008)
    ! =========================================================================
    
    ! Switching power: 1.0 = linear, 2.0 = strong switching
    SWITCHING_POWER = 1.5D0
    
    ! Calculate total preference-weighted food availability
    TOTAL_FOOD = PREF_ZOO_DIA            * max(DIA_C - FOOD_MIN_ZOO, 0.0D0) + &
                 PREF_ZOO_CYN            * max(CYN_C - FOOD_MIN_ZOO, 0.0D0) + &
                 PREF_ZOO_OPA            * max(OPA_C - FOOD_MIN_ZOO, 0.0D0) + &
                 PREF_ZOO_FIX_CYN        * max(FIX_CYN_C - FOOD_MIN_ZOO, 0.0D0) + &
                 PREF_ZOO_NOST_VEG_HET   * max(NOST_VEG_HET_C - FOOD_MIN_ZOO, 0.0D0) + &
                 PREF_ZOO_DET_PART_ORG_C * max(DET_PART_ORG_C - FOOD_MIN_ZOO, 0.0D0)
    
    ! Calculate dynamic (switching) preferences for each prey type
    ! Formula: DYN_PREF_i = BASE_PREF_i * (relative_abundance_i)^(n-1)
    ! where relative_abundance_i = (PREF_i * PREY_i) / TOTAL_FOOD
    ! This makes preference increase for abundant prey
    
    where (TOTAL_FOOD > 1.0D-10)
        DYN_PREF_DIA = PREF_ZOO_DIA * &
            ((PREF_ZOO_DIA * max(DIA_C - FOOD_MIN_ZOO, 0.0D0) / TOTAL_FOOD) ** (SWITCHING_POWER - 1.0D0))
        DYN_PREF_CYN = PREF_ZOO_CYN * &
            ((PREF_ZOO_CYN * max(CYN_C - FOOD_MIN_ZOO, 0.0D0) / TOTAL_FOOD) ** (SWITCHING_POWER - 1.0D0))
        DYN_PREF_OPA = PREF_ZOO_OPA * &
            ((PREF_ZOO_OPA * max(OPA_C - FOOD_MIN_ZOO, 0.0D0) / TOTAL_FOOD) ** (SWITCHING_POWER - 1.0D0))
        DYN_PREF_FIX_CYN = PREF_ZOO_FIX_CYN * &
            ((PREF_ZOO_FIX_CYN * max(FIX_CYN_C - FOOD_MIN_ZOO, 0.0D0) / TOTAL_FOOD) ** (SWITCHING_POWER - 1.0D0))
        DYN_PREF_NOST = PREF_ZOO_NOST_VEG_HET * &
            ((PREF_ZOO_NOST_VEG_HET * max(NOST_VEG_HET_C - FOOD_MIN_ZOO, 0.0D0) / TOTAL_FOOD) ** (SWITCHING_POWER - 1.0D0))
        DYN_PREF_DET = PREF_ZOO_DET_PART_ORG_C * &
            ((PREF_ZOO_DET_PART_ORG_C * max(DET_PART_ORG_C - FOOD_MIN_ZOO, 0.0D0) / TOTAL_FOOD) ** (SWITCHING_POWER - 1.0D0))
    elsewhere
        ! No food available - use base preferences
        DYN_PREF_DIA     = PREF_ZOO_DIA
        DYN_PREF_CYN     = PREF_ZOO_CYN
        DYN_PREF_OPA     = PREF_ZOO_OPA
        DYN_PREF_FIX_CYN = PREF_ZOO_FIX_CYN
        DYN_PREF_NOST    = PREF_ZOO_NOST_VEG_HET
        DYN_PREF_DET     = PREF_ZOO_DET_PART_ORG_C
    end where
    ! =========================================================================
    ! END ACTIVE SWITCHING MODEL
    ! =========================================================================

    ! Use DYNAMIC preferences (from active switching model) instead of static ones
    where (DIA_C > FOOD_MIN_ZOO)
        FOOD_FACTOR_ZOO_DIA = (DYN_PREF_DIA * (DIA_C - FOOD_MIN_ZOO)) / &
            (DIA_C + KHS_DIA_C_ZOO)
    elsewhere
        FOOD_FACTOR_ZOO_DIA = 0.0D0
    end where

    where (CYN_C > FOOD_MIN_ZOO)
        FOOD_FACTOR_ZOO_CYN = (DYN_PREF_CYN * (CYN_C - FOOD_MIN_ZOO)) / &
            (CYN_C + KHS_CYN_C_ZOO)
    elsewhere
        FOOD_FACTOR_ZOO_CYN = 0.0D0
    end where

    where (OPA_C > FOOD_MIN_ZOO)
        FOOD_FACTOR_ZOO_OPA = (DYN_PREF_OPA * (OPA_C - FOOD_MIN_ZOO)) / &
            (OPA_C + KHS_OPA_C_ZOO)
    elsewhere
        FOOD_FACTOR_ZOO_OPA = 0.0D0
    end where

    where (FIX_CYN_C > FOOD_MIN_ZOO)
        FOOD_FACTOR_ZOO_FIX_CYN = (DYN_PREF_FIX_CYN * (FIX_CYN_C - FOOD_MIN_ZOO)) / &
            (FIX_CYN_C + KHS_FIX_CYN_C_ZOO)
    elsewhere
        FOOD_FACTOR_ZOO_FIX_CYN = 0.0D0
    end where

    where (NOST_VEG_HET_C > FOOD_MIN_ZOO)
        FOOD_FACTOR_ZOO_NOST_VEG_HET = (DYN_PREF_NOST * (NOST_VEG_HET_C - FOOD_MIN_ZOO)) / &
            (NOST_VEG_HET_C + KHS_NOST_VEG_HET_C_ZOO)
    elsewhere
        FOOD_FACTOR_ZOO_NOST_VEG_HET = 0.0D0
    end where

    where (DET_PART_ORG_C > FOOD_MIN_ZOO)
        FOOD_FACTOR_ZOO_DET_PART_ORG_C = &
            (DYN_PREF_DET * (DET_PART_ORG_C - FOOD_MIN_ZOO)) / &
            (DET_PART_ORG_C + KHS_DET_PART_ORG_C_ZOO)
    elsewhere
        FOOD_FACTOR_ZOO_DET_PART_ORG_C = 0.0D0
    end where

    R_ZOO_FEEDING_DIA            = KG_ZOO_DIA            * FOOD_FACTOR_ZOO_DIA            * ZOO_C
    R_ZOO_FEEDING_CYN            = KG_ZOO_CYN            * FOOD_FACTOR_ZOO_CYN            * ZOO_C
    R_ZOO_FEEDING_FIX_CYN        = KG_ZOO_FIX_CYN        * FOOD_FACTOR_ZOO_FIX_CYN        * ZOO_C
    R_ZOO_FEEDING_NOST_VEG_HET   = KG_ZOO_NOST_VEG_HET   * FOOD_FACTOR_ZOO_NOST_VEG_HET   * ZOO_C
    R_ZOO_FEEDING_OPA            = KG_ZOO_OPA            * FOOD_FACTOR_ZOO_OPA            * ZOO_C
    R_ZOO_FEEDING_DET_PART_ORG_C = KG_ZOO_DET_PART_ORG_C * FOOD_FACTOR_ZOO_DET_PART_ORG_C * ZOO_C

    ! Mass-balance safeguard: limit grazing rates to available prey biomass per TIME_STEP
    ! This prevents removing more biomass than exists when ZOO_C >> prey biomass
    pred_limit = 0.5D0  ! max fraction of prey that can be consumed in one TIME_STEP
    do i = 1, nkn
        ! Limit DIA grazing
        if (DIA_C(i) > 0.0D0) then
            if (R_ZOO_FEEDING_DIA(i) > pred_limit * DIA_C(i) / TIME_STEP) then
                R_ZOO_FEEDING_DIA(i) = pred_limit * DIA_C(i) / TIME_STEP
            end if
        end if
        ! Limit CYN grazing
        if (CYN_C(i) > 0.0D0) then
            if (R_ZOO_FEEDING_CYN(i) > pred_limit * CYN_C(i) / TIME_STEP) then
                R_ZOO_FEEDING_CYN(i) = pred_limit * CYN_C(i) / TIME_STEP
            end if
        end if
        ! Limit FIX_CYN grazing
        if (FIX_CYN_C(i) > 0.0D0) then
            if (R_ZOO_FEEDING_FIX_CYN(i) > pred_limit * FIX_CYN_C(i) / TIME_STEP) then
                R_ZOO_FEEDING_FIX_CYN(i) = pred_limit * FIX_CYN_C(i) / TIME_STEP
            end if
        end if
        ! Limit OPA grazing
        if (OPA_C(i) > 0.0D0) then
            if (R_ZOO_FEEDING_OPA(i) > pred_limit * OPA_C(i) / TIME_STEP) then
                R_ZOO_FEEDING_OPA(i) = pred_limit * OPA_C(i) / TIME_STEP
            end if
        end if
        ! Limit NOST grazing
        if (NOST_VEG_HET_C(i) > 0.0D0) then
            if (R_ZOO_FEEDING_NOST_VEG_HET(i) > pred_limit * NOST_VEG_HET_C(i) / TIME_STEP) then
                R_ZOO_FEEDING_NOST_VEG_HET(i) = pred_limit * NOST_VEG_HET_C(i) / TIME_STEP
            end if
        end if
        ! Limit DET_PART_ORG_C grazing
        if (DET_PART_ORG_C(i) > 0.0D0) then
            if (R_ZOO_FEEDING_DET_PART_ORG_C(i) > pred_limit * DET_PART_ORG_C(i) / TIME_STEP) then
                R_ZOO_FEEDING_DET_PART_ORG_C(i) = pred_limit * DET_PART_ORG_C(i) / TIME_STEP
            end if
        end if
    end do

    !Zooplankton excretion rate
    ACTUAL_ZOO_N_TO_C = ZOO_N_TO_C
    ACTUAL_ZOO_P_TO_C = ZOO_P_TO_C

    !Zooplankton Growth rate
    R_ZOO_GROWTH =  R_ZOO_FEEDING_DIA         + R_ZOO_FEEDING_CYN            + &
                    R_ZOO_FEEDING_OPA         + R_ZOO_FEEDING_NOST_VEG_HET   + &
                    R_ZOO_FEEDING_DET_PART_ORG_C

    R_ZOO_RESP = R_ZOO_GROWTH * (1.0D0 - EFF_ZOO_GROWTH)
    R_ZOO_INT_RESP = KR_ZOO_20 * (THETA_KR_ZOO ** (TEMP - 2.0D1)) * ZOO_C

    !Zooplankton death rate
    KD_ZOO = KD_ZOO_20 * (THETA_KD_ZOO ** (TEMP - 2.0D1))

    FAC_HYPOX_ZOO_D = 1.0D0
    if(KD_ZOO_20 .gt. 0.D0) then
     where (DISS_OXYGEN <= DO_STR_HYPOX_ZOO_D)

         where (DISS_OXYGEN / DO_STR_HYPOX_ZOO_D > 1.0D-1)
             FAC_HYPOX_ZOO_D = THETA_HYPOX_ZOO_D ** &
                 (EXPON_HYPOX_ZOO_D * &
                     (DO_STR_HYPOX_ZOO_D - DISS_OXYGEN))
         elsewhere
             FAC_HYPOX_ZOO_D = min(TIME_STEP / (5.0D-1 * KD_ZOO), &
                                  9.0D-1 / (KD_ZOO * TIME_STEP))
             R_ZOO_FEEDING_DIA            = 0.0D0
             R_ZOO_FEEDING_CYN            = 0.0D0
             R_ZOO_FEEDING_FIX_CYN        = 0.0D0
             R_ZOO_FEEDING_NOST_VEG_HET   = 0.0D0
             R_ZOO_FEEDING_OPA            = 0.0D0
             R_ZOO_FEEDING_DET_PART_ORG_C = 0.0D0
             R_ZOO_INT_RESP               = 0.0D0
             R_ZOO_RESP                   = 0.0D0
             R_ZOO_EX_DON                 = 0.0D0
             R_ZOO_EX_DOP                 = 0.0D0
             R_ZOO_EX_DOC                 = 0.0D0
         end where
     elsewhere
         FAC_HYPOX_ZOO_D = 1.0D0
     end where
    end if

    R_ZOO_DEATH = KD_ZOO * FAC_HYPOX_ZOO_D * ZOO_C

    ! Mass-balance safeguard: limit total losses to available biomass per TIME_STEP
    do i = 1, nkn
        if (ZOO_C(i) > 0.0D0) then
            loss = R_ZOO_DEATH(i) + R_ZOO_INT_RESP(i) + R_ZOO_RESP(i)
            if (loss > 0.5D0 * ZOO_C(i) / TIME_STEP) then
                scale_loss = (0.5D0 * ZOO_C(i) / TIME_STEP) / loss
                R_ZOO_DEATH(i) = R_ZOO_DEATH(i) * scale_loss
                R_ZOO_INT_RESP(i) = R_ZOO_INT_RESP(i) * scale_loss
                R_ZOO_RESP(i) = R_ZOO_RESP(i) * scale_loss
            end if
        end if
    end do

end subroutine ZOOPLANKTON
