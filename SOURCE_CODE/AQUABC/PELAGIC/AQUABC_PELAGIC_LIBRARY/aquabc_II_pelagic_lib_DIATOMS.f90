!
! Contents:
!subroutine DIATOMS

subroutine DIATOMS(KG_DIA_OPT_TEMP         , &
                   DIA_OPT_TEMP_LR         , &
                   DIA_OPT_TEMP_UR         , &
                   EFF_DIA_GROWTH          , &
                   KAPPA_DIA_UNDER_OPT_TEMP, &
                   KAPPA_DIA_OVER_OPT_TEMP , &
                   KR_DIA_20               , &
                   THETA_KR_DIA            , &
                   KD_DIA_20               , &
                   THETA_KD_DIA            , &
                   KHS_DIN_DIA             , &
                   KHS_DIP_DIA             , &
                   KHS_DSi_DIA             , &
                   KHS_O2_DIA              , &
                   FRAC_DIA_EXCR           , &
                   I_S_DIA                 , &
                   DO_STR_HYPOX_DIA_D      , &
                   THETA_HYPOX_DIA_D       , &
                   EXPON_HYPOX_DIA_D       , &
                   DIA_N_TO_C              , &
                   DIA_P_TO_C              , &
                   DIA_Si_TO_C             , &
                   DIA_O2_TO_C             , &
                   DIA_C_TO_CHLA           , &
                   DIA_LIGHT_SAT           , &
                   NH4_N                   , &
                   NO3_N                   , &
                   PO4_P                   , &
                   DISS_OXYGEN             , &
                   DIA_C                   , &
                   ZOO_C                   , &
                   DISS_Si                 , &
                   TEMP                    , &
                   I_A                     , &
                   K_E                     , &
                   DEPTH                   , &
                   CHLA                    , &
                   FDAY                    , &
                   TIME_STEP               , &
                   SMITH                   , &
                   nkn                     , &
                   KG_DIA                  , &
                   ALPHA_0                 , &
                   ALPHA_1                 , &
                   LIM_KG_DIA_TEMP         , &
                   LIM_KG_DIA_LIGHT        , &
                   LIM_KG_DIA_DOXY         , &
                   LIM_KG_DIA_N            , &
                   LIM_KG_DIA_P            , &
                   LIM_KG_DIA_DISS_Si      , &
                   LIM_KG_DIA_NUTR         , &
                   LIM_KG_DIA              , &
                   R_DIA_GROWTH            , &
                   R_DIA_MET               , &
                   R_DIA_RESP              , &
                   R_DIA_EXCR              , &
                   R_DIA_INT_RESP          , &
                   KD_DIA                  , &
                   FAC_HYPOX_DIA_D         , &
                   R_DIA_DEATH             , &
                   PREF_NH4N_DIA)

    use AQUABC_II_GLOBAL
    use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
    implicit none

    ! -------------------------------------------------------------------------
    ! Ingoing variables
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), intent(in) :: KG_DIA_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: DIA_OPT_TEMP_LR
    real(kind = DBL_PREC), intent(in) :: DIA_OPT_TEMP_UR
    real(kind = DBL_PREC), intent(in) :: EFF_DIA_GROWTH
    real(kind = DBL_PREC), intent(in) :: KAPPA_DIA_UNDER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KAPPA_DIA_OVER_OPT_TEMP
    real(kind = DBL_PREC), intent(in) :: KR_DIA_20
    real(kind = DBL_PREC), intent(in) :: THETA_KR_DIA
    real(kind = DBL_PREC), intent(in) :: KD_DIA_20
    real(kind = DBL_PREC), intent(in) :: THETA_KD_DIA
    real(kind = DBL_PREC), intent(in) :: KHS_DIN_DIA
    real(kind = DBL_PREC), intent(in) :: KHS_DIP_DIA
    real(kind = DBL_PREC), intent(in) :: KHS_DSi_DIA
    real(kind = DBL_PREC), intent(in) :: KHS_O2_DIA
    real(kind = DBL_PREC), intent(in) :: FRAC_DIA_EXCR
    real(kind = DBL_PREC), intent(in) :: I_S_DIA
    real(kind = DBL_PREC), intent(in) :: DO_STR_HYPOX_DIA_D
    real(kind = DBL_PREC), intent(in) :: THETA_HYPOX_DIA_D
    real(kind = DBL_PREC), intent(in) :: EXPON_HYPOX_DIA_D
    real(kind = DBL_PREC), intent(in) :: DIA_N_TO_C
    real(kind = DBL_PREC), intent(in) :: DIA_P_TO_C
    real(kind = DBL_PREC), intent(in) :: DIA_Si_TO_C
    real(kind = DBL_PREC), intent(in) :: DIA_O2_TO_C
    real(kind = DBL_PREC), intent(in) :: DIA_C_TO_CHLA

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NH4_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3_N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PO4_P
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DISS_OXYGEN
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DIA_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: ZOO_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DISS_Si

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
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KG_DIA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_0
    integer :: i, imax
    real(kind = DBL_PREC) :: loss, scale_loss
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: ALPHA_1
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_DIA_TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_DIA_LIGHT
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_DIA_DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_DIA_N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_DIA_P
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_DIA_DISS_Si
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_DIA_NUTR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_KG_DIA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_DIA_GROWTH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_DIA_MET
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_DIA_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_DIA_EXCR
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_DIA_INT_RESP
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: KD_DIA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FAC_HYPOX_DIA_D
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_DIA_DEATH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PREF_NH4N_DIA
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DIA_LIGHT_SAT
    ! -------------------------------------------------------------------------
    ! End of outgoing variables
    ! -------------------------------------------------------------------------
     !Temperature limitation growth factor
     call GROWTH_AT_TEMP &
          (TEMP, LIM_KG_DIA_TEMP, DIA_OPT_TEMP_LR, DIA_OPT_TEMP_UR, KG_DIA_OPT_TEMP,  &
           KAPPA_DIA_UNDER_OPT_TEMP, KAPPA_DIA_OVER_OPT_TEMP,nkn)

     ! if( KG_DIA_OPT_TEMP .ne.  0.D0) then
     !     LIM_KG_DIA_TEMP = KG_DIA / KG_DIA_OPT_TEMP
     ! Temperature limited growth rate
      KG_DIA = KG_DIA_OPT_TEMP * LIM_KG_DIA_TEMP
     ! else
     !     LIM_KG_DIA_TEMP = 0.0D0
     ! end if

     if (smith .eq. 0) then
         !May be replaced by Smith formulation
         ALPHA_0 = (I_A / I_S_DIA) * safe_exp(-1.0D0 * K_E * 0.0D0)
         ALPHA_1 = (I_A / I_S_DIA) * safe_exp(-1.0D0 * K_E * DEPTH)

         LIM_KG_DIA_LIGHT = &
               (((2.718 * FDAY) / (K_E * DEPTH)) * &
                (safe_exp(-1.0D0 * ALPHA_1) - safe_exp(-1.0D0 * ALPHA_0)))
     end if

     if (smith .eq. 1) then
        write(6,*) 'DIATOMS CALL: I_A=', I_A(1:min(8,nkn))
        write(6,*) 'DIATOMS CALL: CHLA=', CHLA(1:min(8,nkn))
        write(6,*) 'DIATOMS CALL: KG_DIA=', KG_DIA(1:min(8,nkn))
        write(6,*) 'DIATOMS CALL: DEPTH=', DEPTH(1:min(8,nkn))
        write(6,*) 'DIATOMS CALL: K_E=', K_E(1:min(8,nkn))
        call LIM_LIGHT(I_A, CHLA, KG_DIA, DEPTH, K_E, LIM_KG_DIA_LIGHT, &
                       DIA_C_TO_CHLA, I_S_DIA, DIA_LIGHT_SAT,nkn)
    end if



     LIM_KG_DIA_DOXY    = DISS_OXYGEN / (KHS_O2_DIA + DISS_OXYGEN)
     LIM_KG_DIA_N       = (NH4_N + NO3_N) / (KHS_DIN_DIA + NH4_N + NO3_N)
     LIM_KG_DIA_P       = PO4_P   / (KHS_DIP_DIA + PO4_P)
     LIM_KG_DIA_DISS_Si = DISS_Si / (KHS_DSi_DIA + DISS_Si)
     ! Synthesizing Unit colimitation (Saito et al. 2008): SU lies between
     ! Liebig minimum (upper bound) and multiplicative (lower bound)
     LIM_KG_DIA_NUTR = LIM_KG_DIA_N * LIM_KG_DIA_P / &
         max(LIM_KG_DIA_N + LIM_KG_DIA_P - LIM_KG_DIA_N * LIM_KG_DIA_P, 1.0D-20)
     LIM_KG_DIA_NUTR = LIM_KG_DIA_NUTR * LIM_KG_DIA_DISS_Si / &
         max(LIM_KG_DIA_NUTR + LIM_KG_DIA_DISS_Si - LIM_KG_DIA_NUTR * LIM_KG_DIA_DISS_Si, 1.0D-20)
     LIM_KG_DIA         = LIM_KG_DIA_LIGHT * min(LIM_KG_DIA_DOXY, LIM_KG_DIA_NUTR)

     !Diatom photo growth rate
     R_DIA_GROWTH       = KG_DIA * LIM_KG_DIA * DIA_C

    !Diatom  metabolic rate. Formulation of division metabolic rate to excretion
    !and respiration is correct only for high oxygen conc. Fixme

    R_DIA_MET  = R_DIA_GROWTH * (1.0D0 - EFF_DIA_GROWTH)
    R_DIA_RESP = (1.D0-FRAC_DIA_EXCR) * R_DIA_MET
    R_DIA_EXCR = FRAC_DIA_EXCR * R_DIA_MET

    !Diatom basal respiration rate
    R_DIA_INT_RESP = &
        KR_DIA_20 * (THETA_KR_DIA ** (TEMP - 2.0D1)) * LIM_KG_DIA_DOXY * DIA_C

    !Calculations for diatom death
    KD_DIA = KD_DIA_20 * (THETA_KD_DIA ** (TEMP - 2.0D1))

    FAC_HYPOX_DIA_D = 1.0D0
    if(KD_DIA_20 .gt. 0.D0) then
        where (DISS_OXYGEN <= DO_STR_HYPOX_DIA_D)

            where (DISS_OXYGEN / DO_STR_HYPOX_DIA_D > 1.0D-1)
                FAC_HYPOX_DIA_D = THETA_HYPOX_DIA_D ** &
                    (EXPON_HYPOX_DIA_D * &
                        (DO_STR_HYPOX_DIA_D - DISS_OXYGEN))
            elsewhere
                FAC_HYPOX_DIA_D = min(TIME_STEP / (5.0D-1 * KD_DIA), &
                                      9.0D-1 / (KD_DIA * TIME_STEP))
                R_DIA_INT_RESP = 0.0D0
                R_DIA_RESP     = 0.0D0
                R_DIA_GROWTH   = 0.0D0
            end where
        elsewhere
            FAC_HYPOX_DIA_D = 1.0D0
        end where
    end if

    R_DIA_DEATH = KD_DIA * FAC_HYPOX_DIA_D * DIA_C

    ! Mass-balance safeguard: limit total losses to available biomass per TIME_STEP
    do i = 1, nkn
        if (DIA_C(i) > 0.0D0) then
            loss = R_DIA_DEATH(i) + R_DIA_EXCR(i) + R_DIA_INT_RESP(i) + R_DIA_RESP(i)
            if (loss > 0.5D0 * DIA_C(i) / TIME_STEP) then
                scale_loss = (0.5D0 * DIA_C(i) / TIME_STEP) / loss
                R_DIA_DEATH(i) = R_DIA_DEATH(i) * scale_loss
                R_DIA_EXCR(i) = R_DIA_EXCR(i) * scale_loss
                R_DIA_INT_RESP(i) = R_DIA_INT_RESP(i) * scale_loss
                R_DIA_RESP(i) = R_DIA_RESP(i) * scale_loss
            end if
        end if
    end do

    call AMMONIA_PREFS(PREF_NH4N_DIA,NH4_N, NO3_N, KHS_DIN_DIA,nkn)
end subroutine DIATOMS
