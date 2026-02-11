! Auxilary routines for the pelagic model

! Contents:
!subroutine OTHER_PLANKTONIC_ALGAE

subroutine OTHER_PLANKTONIC_ALGAE &
           (KG_OPA_OPT_TEMP         , &
            OPA_OPT_TEMP_LR         , &
            OPA_OPT_TEMP_UR         , &
            EFF_OPA_GROWTH          , &
            KAPPA_OPA_UNDER_OPT_TEMP, &
            KAPPA_OPA_OVER_OPT_TEMP , &
            KR_OPA_20               , &
            THETA_KR_OPA            , &
            KD_OPA_20               , &
            THETA_KD_OPA            , &
            KHS_DIN_OPA             , &
            KHS_DIP_OPA             , &
            KHS_O2_OPA              , &
            FRAC_OPA_EXCR           , &
            I_S_OPA                 , &
            DO_STR_HYPOX_OPA_D      , &
            THETA_HYPOX_OPA_D       , &
            EXPON_HYPOX_OPA_D       , &
            OPA_N_TO_C              , &
            OPA_P_TO_C              , &
            OPA_O2_TO_C             , &
            OPA_C_TO_CHLA           , &
            OPA_LIGHT_SAT           , &
            NH4_N                   , &
            NO3_N                   , &
            PO4_P                   , &
            DISS_OXYGEN             , &
            OPA_C                   , &
            ZOO_C                   , &
            TEMP                    , &
            I_A                     , &
            K_E                     , &
            DEPTH                   , &
            CHLA                    , &
            FDAY                    , &
            TIME_STEP               , &
            SMITH                   , &
            nkn                     , &
            KG_OPA                  , &
            ALPHA_0                 , &
            ALPHA_1                 , &
            LIM_KG_OPA_TEMP         , &
            LIM_KG_OPA_LIGHT        , &
            LIM_KG_OPA_DOXY         , &
            LIM_KG_OPA_N            , &
            LIM_KG_OPA_P            , &
            LIM_KG_OPA_NUTR         , &
            LIM_KG_OPA              , &
            R_OPA_GROWTH            , &
            R_OPA_MET               , &
            R_OPA_RESP              , &
            R_OPA_EXCR              , &
            R_OPA_INT_RESP          , &
            KD_OPA                  , &
            FAC_HYPOX_OPA_D         , &
            R_OPA_DEATH             , &
            PREF_NH4N_OPA)

    use AQUABC_II_GLOBAL
    use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
    implicit none

    ! -------------------------------------------------------------------------
    ! Ingoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), intent(in) :: KG_OPA_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: OPA_OPT_TEMP_LR
    real(kind = DBL_PREC), intent(in) :: OPA_OPT_TEMP_UR
    real(kind = DBL_PREC), intent(in) :: EFF_OPA_GROWTH
    real(kind = DBL_PREC), intent(in) :: KAPPA_OPA_UNDER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KAPPA_OPA_OVER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KR_OPA_20
    real(kind = DBL_PREC), intent(in) :: THETA_KR_OPA
    real(kind = DBL_PREC), intent(in) :: KD_OPA_20
    real(kind = DBL_PREC), intent(in) :: THETA_KD_OPA
    real(kind = DBL_PREC), intent(in) :: KHS_DIN_OPA
    real(kind = DBL_PREC), intent(in) :: KHS_DIP_OPA
    real(kind = DBL_PREC), intent(in) :: KHS_O2_OPA
    real(kind = DBL_PREC), intent(in) :: FRAC_OPA_EXCR
    real(kind = DBL_PREC), intent(in) :: I_S_OPA
    real(kind = DBL_PREC), intent(in) :: DO_STR_HYPOX_OPA_D
    real(kind = DBL_PREC), intent(in) :: THETA_HYPOX_OPA_D
    real(kind = DBL_PREC), intent(in) :: EXPON_HYPOX_OPA_D
    real(kind = DBL_PREC), intent(in) :: OPA_N_TO_C
    real(kind = DBL_PREC), intent(in) :: OPA_P_TO_C
    real(kind = DBL_PREC), intent(in) :: OPA_O2_TO_C
    real(kind = DBL_PREC), intent(in) :: OPA_C_TO_CHLA

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NH4_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PO4_P
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DISS_OXYGEN
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: OPA_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: ZOO_C

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: I_A
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: K_E
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DEPTH
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: CHLA

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FDAY(nkn)
    real(kind = DBL_PREC), intent(in) :: TIME_STEP

    integer, intent(in) :: SMITH
    integer, intent(in) :: nkn
    ! -------------------------------------------------------------------------
    ! End of ingoing variables
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Outgoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_OPA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_0
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_1
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_OPA_TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_OPA_LIGHT
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_OPA_DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_OPA_N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_OPA_P
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_OPA_NUTR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_OPA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_OPA_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_OPA_MET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_OPA_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_OPA_EXCR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_OPA_INT_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KD_OPA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FAC_HYPOX_OPA_D
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_OPA_DEATH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PREF_NH4N_OPA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: OPA_LIGHT_SAT
    ! -------------------------------------------------------------------------
    ! End of outgoing variables
    ! -------------------------------------------------------------------------
    integer :: i
    real(kind = DBL_PREC) :: loss, scale_loss

    ALPHA_0 = (I_A / I_S_OPA) * safe_exp(-1.0D0 * K_E * 0.0D0)
    ALPHA_1 = (I_A / I_S_OPA) * safe_exp(-1.0D0 * K_E * DEPTH)

    ! Temperature limitation for growth
    call GROWTH_AT_TEMP &
         (TEMP, LIM_KG_OPA_TEMP, OPA_OPT_TEMP_LR, OPA_OPT_TEMP_UR, KG_OPA_OPT_TEMP,  &
          KAPPA_OPA_UNDER_OPT_TEMP, KAPPA_OPA_OVER_OPT_TEMP,nkn)

    !if( KG_OPA_OPT_TEMP .ne.  0.D0) then
    !    LIM_KG_OPA_TEMP = KG_OPA / KG_OPA_OPT_TEMP

    ! Temperature limited growth rate
    KG_OPA = KG_OPA_OPT_TEMP * LIM_KG_OPA_TEMP
    !else
    !    LIM_KG_OPA_TEMP = 0.D0
    !end if

    if (smith .eq. 0) then
        !May be replaced by Smith formulation
        LIM_KG_OPA_LIGHT = &
             (((2.718 * FDAY) / (K_E * DEPTH)) * &
              (safe_exp(-1.0D0 * ALPHA_1) - safe_exp(-1.0D0 * ALPHA_0)))
    end if

    if (smith .eq. 1) then
        write(6,*) 'OPA CALL: I_A=', I_A(1:min(8,nkn))
        write(6,*) 'OPA CALL: CHLA=', CHLA(1:min(8,nkn))
        write(6,*) 'OPA CALL: KG_OPA=', KG_OPA(1:min(8,nkn))
        write(6,*) 'OPA CALL: DEPTH=', DEPTH(1:min(8,nkn))
        write(6,*) 'OPA CALL: K_E=', K_E(1:min(8,nkn))
        call LIM_LIGHT(I_A, CHLA, KG_OPA, DEPTH, K_E, LIM_KG_OPA_LIGHT, &
                       OPA_C_TO_CHLA, I_S_OPA, OPA_LIGHT_SAT,nkn)
    end if


    LIM_KG_OPA_DOXY = DISS_OXYGEN / (KHS_O2_OPA + DISS_OXYGEN)
    LIM_KG_OPA_N    = (NH4_N + NO3_N) / (KHS_DIN_OPA + NH4_N + NO3_N)
    LIM_KG_OPA_P    = PO4_P   / (KHS_DIP_OPA + PO4_P)
    ! Synthesizing Unit colimitation (Saito et al. 2008)
    LIM_KG_OPA_NUTR = LIM_KG_OPA_N * LIM_KG_OPA_P / &
        max(LIM_KG_OPA_N + LIM_KG_OPA_P - LIM_KG_OPA_N * LIM_KG_OPA_P, 1.0D-20)
    !    LIM_KG_OPA      = min(LIM_KG_OPA_DOXY, LIM_KG_OPA_NUTR, LIM_KG_OPA_LIGHT)
    LIM_KG_OPA      = LIM_KG_OPA_LIGHT * min(LIM_KG_OPA_DOXY, LIM_KG_OPA_NUTR) !changed by Petras

    !Other planktonic algae growth rate
    R_OPA_GROWTH    = KG_OPA * LIM_KG_OPA * OPA_C

    !Other planktonic algae metabolism, respiration, excretion rate

    R_OPA_MET    = R_OPA_GROWTH * (1.0D0 - EFF_OPA_GROWTH)
    R_OPA_RESP  =  (1.D0-FRAC_OPA_EXCR) * R_OPA_MET
    R_OPA_EXCR = FRAC_OPA_EXCR * R_OPA_MET

    !Other planktonic algae dark respiration rate
    R_OPA_INT_RESP  = KR_OPA_20 * (THETA_KR_OPA ** (TEMP - 2.0D1)) * LIM_KG_OPA_DOXY * OPA_C

    !Calculations for other planktonic algae death
    KD_OPA = KD_OPA_20 * (THETA_KD_OPA ** (TEMP - 2.0D1))

    FAC_HYPOX_OPA_D = 1.0D0
    if(KD_OPA_20 .gt. 0.D0) then
     where (DISS_OXYGEN <= DO_STR_HYPOX_OPA_D)

         where (DISS_OXYGEN / DO_STR_HYPOX_OPA_D > 1.0D-1)
             FAC_HYPOX_OPA_D = THETA_HYPOX_OPA_D ** &
                 (EXPON_HYPOX_OPA_D * &
                     (DO_STR_HYPOX_OPA_D - DISS_OXYGEN))
         elsewhere
             FAC_HYPOX_OPA_D = min(TIME_STEP / (5.0D-1 * KD_OPA), &
                                  9.0D-1 / (KD_OPA * TIME_STEP))
             R_OPA_INT_RESP  = 0.0D0
             R_OPA_RESP      = 0.0D0
             R_OPA_GROWTH    = 0.0D0
         end where
     elsewhere
         FAC_HYPOX_OPA_D = 1.0D0
     end where
    end if

    !Other planktonic algae death rate
    R_OPA_DEATH = KD_OPA * FAC_HYPOX_OPA_D * OPA_C

    ! Mass-balance safeguard: limit total losses to available biomass per TIME_STEP
    do i = 1, nkn
        if (OPA_C(i) > 0.0D0) then
            loss = R_OPA_DEATH(i) + R_OPA_EXCR(i) + R_OPA_INT_RESP(i) + R_OPA_RESP(i)
            if (loss > 0.5D0 * OPA_C(i) / TIME_STEP) then
                scale_loss = (0.5D0 * OPA_C(i) / TIME_STEP) / loss
                R_OPA_DEATH(i) = R_OPA_DEATH(i) * scale_loss
                R_OPA_EXCR(i) = R_OPA_EXCR(i) * scale_loss
                R_OPA_INT_RESP(i) = R_OPA_INT_RESP(i) * scale_loss
                R_OPA_RESP(i) = R_OPA_RESP(i) * scale_loss
            end if
        end if
    end do

    !PREF_NH4N_OPA = NH4_N / (NH4_N + KHS_NH4N_PREF_OPA)
    call AMMONIA_PREFS(PREF_NH4N_OPA, NH4_N, NO3_N, KHS_DIN_OPA,nkn)
end subroutine OTHER_PLANKTONIC_ALGAE
