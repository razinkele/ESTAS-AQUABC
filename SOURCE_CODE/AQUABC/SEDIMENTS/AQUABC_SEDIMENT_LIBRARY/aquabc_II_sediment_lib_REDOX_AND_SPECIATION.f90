! Content:
!subroutine SED_REDOX_AND_SPECIATION

subroutine SED_REDOX_AND_SPECIATION &
           (DOXY, NO3N, MN_IV, FE_III, S_PLUS_6, DISS_ORG_C, &
            S_MINUS_2 , MN_II, FE_II , HCO3    , CO3       , &
            TEMP, SALT, PH, ELEVATION, &
            K_HS_DOXY_RED_LIM   , K_HS_NO3N_RED_LIM , K_HS_MN_IV_RED_LIM , &
            K_HS_FE_III_RED_LIM , K_HS_S_PLUS_6_RED_LIM, &
            K_HS_DOXY_RED_INHB  , K_HS_NO3N_RED_INHB, K_HS_MN_IV_RED_INHB, &
            K_HS_FE_III_RED_INHB, K_HS_S_PLUS_6_RED_INHB, nkn, NUM_SED_LAYERS, &
            LIM_DOXY_RED        , LIM_NO3N_RED          , LIM_MN_IV_RED  , &
            LIM_FE_III_RED      , LIM_S_PLUS_6_RED      , LIM_DOC_RED, &
            PE, FE_II_DISS, FE_III_DISS, MN_II_DISS)

    use AQUABC_II_GLOBAL
    use AQUABC_PHYSICAL_CONSTANTS, only: FE_MOLAR_MASS_MG, MN_MOLAR_MASS_MG, S_MOLAR_MASS_MG
    use, intrinsic :: IEEE_ARITHMETIC
    implicit none

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: DOXY          ! Dissolved oxygen (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: NO3N          ! Nitrate nitrogen (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: MN_IV         ! Mn IV            (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: FE_III        ! Fe III           (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: S_PLUS_6      ! S +VI            (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: DISS_ORG_C    ! DOC              (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: S_MINUS_2     ! S -II            (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: MN_II         ! Mn II            (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: FE_II         ! Fe II            (mg/L)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: HCO3          ! Bicarbonates     (moles)
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: CO3           ! Carbonate        (moles)

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: SALT
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: PH
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: ELEVATION

    real(kind = DBL_PREC), intent(in) :: K_HS_DOXY_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_NO3N_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_MN_IV_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_FE_III_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_S_PLUS_6_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_DOXY_RED_INHB
    real(kind = DBL_PREC), intent(in) :: K_HS_NO3N_RED_INHB
    real(kind = DBL_PREC), intent(in) :: K_HS_MN_IV_RED_INHB
    real(kind = DBL_PREC), intent(in) :: K_HS_FE_III_RED_INHB
    real(kind = DBL_PREC), intent(in) :: K_HS_S_PLUS_6_RED_INHB

    integer, intent(in) :: nkn, NUM_SED_LAYERS

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_DOXY_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_NO3N_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_MN_IV_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_FE_III_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_S_PLUS_6_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_DOC_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: PE
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: FE_II_DISS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: FE_III_DISS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: MN_II_DISS

    integer, dimension(nkn, NUM_SED_LAYERS) :: REDUCED_AGENT_NO

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS, 6) :: REDUCER_LIM_FACTORS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: CS, H_PLUS, SO4_MOLAR, HS_MOLAR, S_MINUS_2_MOLAR

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_CO3_OVER_FE_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_OH_2_OVER_FE_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_S_OVER_FE_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_S_2_OVER_FE_II

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_3_O_4_OVER_FE_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_2_O_3_OVER_FE_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_OOH_OVER_FE_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_OH_3_OVER_FE_II

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: MN_CO3_OVER_MN_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: MN_OH_2_OVER_MN_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: MN_S_OVER_MN_II

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS, 4) :: FE_II_ACTIVITY_RATIOS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS, 3) :: FE_III_ACTIVITY_RATIOS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS, 3) :: MN_II_ACTIVITY_RATIOS

    integer, dimension(nkn, NUM_SED_LAYERS) :: FE_II_SALT_NO
    integer, dimension(nkn, NUM_SED_LAYERS) :: FE_III_SALT_NO
    integer, dimension(nkn, NUM_SED_LAYERS) :: MN_II_SALT_NO

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FREE_FE_II
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FREE_FE_III
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FREE_MN_II

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: TEMP_K
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: LOG_KW
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: KW
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: OH_MINUS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: FE_III_FREE
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: H_PLUS_OVER_2
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: H_PLUS_OVER_3
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS) :: H_PLUS_OVER_4


    real(kind = DBL_PREC) :: K1_FE_OH_3
    real(kind = DBL_PREC) :: BETA_2_FE_OH_3
    real(kind = DBL_PREC) :: BETA_3_FE_OH_3
    real(kind = DBL_PREC) :: BETA_4_FE_OH_3
    real(kind = DBL_PREC) :: BETA_2_2_FE_OH_3
    real(kind = DBL_PREC) :: BETA_4_3_FE_OH_3

    integer :: i, j

    ! Calculate H+ concentration from pH (standard conversion: [H+] = 10^(-pH))
    H_PLUS(:,:) = 10.0D0**(-PH)

    LIM_DOXY_RED = DOXY  / (DOXY + K_HS_DOXY_RED_LIM)

    LIM_NO3N_RED = (NO3N / (NO3N + K_HS_NO3N_RED_LIM)) * &
        (K_HS_DOXY_RED_INHB / (DOXY + K_HS_DOXY_RED_INHB))

    LIM_MN_IV_RED = (MN_IV  / (MN_IV + K_HS_MN_IV_RED_LIM)) * &
        (K_HS_DOXY_RED_INHB / (DOXY + K_HS_DOXY_RED_INHB))  * &
        (K_HS_NO3N_RED_INHB / (NO3N + K_HS_NO3N_RED_INHB))

    LIM_FE_III_RED = (FE_III  / (FE_III + K_HS_FE_III_RED_LIM)) * &
        (K_HS_DOXY_RED_INHB  / (DOXY  + K_HS_DOXY_RED_INHB))    * &
        (K_HS_NO3N_RED_INHB  / (NO3N  + K_HS_NO3N_RED_INHB))    * &
        (K_HS_MN_IV_RED_INHB / (MN_IV + K_HS_MN_IV_RED_INHB))

    LIM_S_PLUS_6_RED = (S_PLUS_6 / (S_PLUS_6 + K_HS_S_PLUS_6_RED_LIM)) * &
        (K_HS_DOXY_RED_INHB   / (DOXY   + K_HS_DOXY_RED_INHB))  * &
        (K_HS_NO3N_RED_INHB   / (NO3N   + K_HS_NO3N_RED_INHB))  * &
        (K_HS_MN_IV_RED_INHB  / (MN_IV  + K_HS_MN_IV_RED_INHB)) * &
        (K_HS_FE_III_RED_INHB / (FE_III + K_HS_FE_III_RED_INHB))

    LIM_DOC_RED = 1.0D0 - &
        (LIM_DOXY_RED + LIM_NO3N_RED + LIM_MN_IV_RED + LIM_FE_III_RED + LIM_S_PLUS_6_RED)

    where (LIM_DOC_RED < 0.0D0)
        LIM_DOC_RED = 0.0D0
    end where

    REDUCER_LIM_FACTORS(:,:,1) = LIM_DOXY_RED
    REDUCER_LIM_FACTORS(:,:,2) = LIM_NO3N_RED
    REDUCER_LIM_FACTORS(:,:,3) = LIM_MN_IV_RED
    REDUCER_LIM_FACTORS(:,:,4) = LIM_FE_III_RED
    REDUCER_LIM_FACTORS(:,:,5) = LIM_S_PLUS_6_RED
    REDUCER_LIM_FACTORS(:,:,6) = LIM_DOC_RED

    REDUCED_AGENT_NO = maxloc(REDUCER_LIM_FACTORS, dim = 3)

    call DO_SATURATION_MAT(TEMP, SALT, ELEVATION, nkn, NUM_SED_LAYERS, CS)

    ! Dissolved oxygen is reduced
    where (REDUCED_AGENT_NO == 1)
        PE = 20.75D0 - log10(1.0D0 / (((0.21D0 * (DOXY/CS))**0.25D0) * H_PLUS))
    end where

    ! Nitrate is reduced
    where (REDUCED_AGENT_NO == 2)
        PE = 21.05 - log10(1.0D0 / ((NO3N/14000.0D0)*(H_PLUS**1.2D0)))
    end where

    ! Mn IV is reduced
    where (REDUCED_AGENT_NO == 3)
        PE = &
		    20.8D0 - &
			log10(((MN_II/MN_MOLAR_MASS_MG) ** 0.5D0) / (((MN_IV/MN_MOLAR_MASS_MG) ** 0.5D0)*(H_PLUS**2.0D0)))
    end where

    ! FE III is reduced
    where (REDUCED_AGENT_NO == 4)
        PE = 13.0D0 - log10(FE_II / FE_III)
    end where

    ! S VI is reduced
    where (REDUCED_AGENT_NO == 5)
        HS_MOLAR = (S_MINUS_2 / S_MOLAR_MASS_MG) * &
            ((H_PLUS * 8.9D-8)  / ((H_PLUS * H_PLUS) + (H_PLUS * 8.9D-8) + (8.9D-8 * 1.2D-13)))

        PE = &
		    4.25D0 - &
			log((HS_MOLAR**0.125D0) / (((S_PLUS_6 / S_MOLAR_MASS_MG) **0.125D0) * (H_PLUS**1.125D0)))
    end where

    ! Methanogenesis
    where (REDUCED_AGENT_NO == 6)
        PE = -0.2D0 - log10(1.0D0 / ((DISS_ORG_C / 12000D0)**0.25D0) * H_PLUS)
    end where

    ! Since PE is known, let's have a look in FE_II and FE_III solids following
    ! the Appendix 8.1, pp 513-515 in Stumm and Morgen (1996),
    ! "Aquatic Chemistry, Chemical Equilibria and Rates in Natural Waters"

    ! FE_CO3  / DISS_FE_II
    FE_CO3_OVER_FE_II  = 10.0D0**(-0.2D0 +  PH + log10(HCO3))

    ! FE_OH_2 / DISS_FE_II
    FE_OH_2_OVER_FE_II = 10.0D0**(-11.7D0 + (2.0D0*PH))

    ! FE_S / DISS_FE_II
    FE_S_OVER_FE_II    = &
        10.0D0**(38.0D0  - (8.0D0*PH) + log10((S_PLUS_6 / S_MOLAR_MASS_MG)) - (8.0D0*PE))


    ! FE_S_2 / DISS_FE_II
    FE_S_2_OVER_FE_II  = &
        10.0D0**(86.8D0  - (16.0D0*PH) + (2.0D0*log10((S_PLUS_6 / S_MOLAR_MASS_MG))) - &
                 (14.0D0*PE))

    ! Now find out which Fe II salt is more likely to form for each reactor
    FE_II_ACTIVITY_RATIOS (:, :, 1) = FE_CO3_OVER_FE_II
    FE_II_ACTIVITY_RATIOS (:, :, 2) = FE_OH_2_OVER_FE_II
    FE_II_ACTIVITY_RATIOS (:, :, 3) = FE_S_OVER_FE_II
    FE_II_ACTIVITY_RATIOS (:, :, 4) = FE_S_2_OVER_FE_II

    FE_II_SALT_NO  = maxloc(FE_II_ACTIVITY_RATIOS , dim = 2)

    !FeCO3
    where(FE_II_SALT_NO == 1)
        FREE_FE_II = 10.0D0**(-0.3D0 - PH + log10(HCO3))
    end where

    !Fe(OH)2
    where(FE_II_SALT_NO == 2)
        FREE_FE_II = 10.0D0**(13.3D0 - (2.0D0*PH))
    end where

    S_MINUS_2_MOLAR = (S_MINUS_2 / S_MOLAR_MASS_MG) * &
        ((8.9D-8 * 1.2D-13)  / ((H_PLUS * H_PLUS) + (H_PLUS * 8.9D-8) + &
         (8.9D-8 * 1.2D-13)))

    !FeS
    where((FE_II_SALT_NO == 3).and.(S_MINUS_2_MOLAR > 1.0D-12) )
        FREE_FE_II = 10.0D0**(-18.64D0) / S_MINUS_2_MOLAR
    end where

    where((FE_II_SALT_NO == 3).and.(S_MINUS_2_MOLAR <= 1.0D-12) )
        FREE_FE_II = FE_II / FE_MOLAR_MASS_MG
    end where


    !FeS2
    where((FE_II_SALT_NO == 4).and.(S_MINUS_2_MOLAR > 1.0D-12) )
        FREE_FE_II = (10.0D0**(-26.89D0)) / (4.0D0 * S_MINUS_2_MOLAR * S_MINUS_2_MOLAR)
    end where

    where((FE_II_SALT_NO == 4).and.(S_MINUS_2_MOLAR <= 1.0D-12) )
        FREE_FE_II = FE_II / FE_MOLAR_MASS_MG
    end where

    ! For a while no complex formation (to be fixed)
    FE_II_DISS = FREE_FE_II

    ! -------------------------------------------------------------------------
    ! END OF FE_II Species
    ! -------------------------------------------------------------------------


    ! (FE_3_O_4^(1/3)) / DISS_FE_II
    !FE_3_O_4_OVER_FE_II = 10.0D0**(3.0D0*(-10.1+((2.66D0)*PH) + ((0.66D0)*PE)))

    ! -------------------------------------------------------------------------
    ! FE_III Species
    ! -------------------------------------------------------------------------


    !! (FE_2_O_3^(1/2)) / DISS_FE_II
    !FE_2_O_3_OVER_FE_II = 10.0D0**(2.0D0 * (-11.1D0 + (3.0D0*PH) + PE))
    !
    !! FE_OOH / DISS_FE_II
    !FE_OOH_OVER_FE_II   = 10.0D0**(-11.3D0 + (3.0D0*PH) + PE)
    !
    !! FE_OH_3 / DISS_FE_II
    !FE_OH_3_OVER_FE_II  = 10.0D0**(-17.1D0 + (3.0D0*PH) + PE)
    !
    !
    !! Now find out which Fe III salt is more likely to form for each reactor
    !FE_III_ACTIVITY_RATIOS(:, :, 1) = FE_2_O_3_OVER_FE_II
    !FE_III_ACTIVITY_RATIOS(:, :, 2) = FE_OOH_OVER_FE_II
    !FE_III_ACTIVITY_RATIOS(:, :, 3) = FE_OH_3_OVER_FE_II
    !
    !FE_III_SALT_NO = maxloc(FE_III_ACTIVITY_RATIOS, dim = 2)
    !
    !!Fe2O3
    !where(FE_III_SALT_NO == 1)
    !    FREE_FE_III = 0.0D0     !Better approximation some time
    !end where
    !
    !!FeOOH
    !where(FE_III_SALT_NO == 2)
    !    TEMP_K = TEMP + 273.15D0
    !
    !    LOG_KW = &
    !        -2.839710D2 + (1.3323D4 / TEMP_K) - (5.069842D-2 * TEMP_K) + &
    !        (1.0224447D2 * log10(TEMP_K)) - (1.119669D6 / (TEMP_K * TEMP_K))
    !
    !    KW = 10**(LOG_KW)
    !    OH_MINUS = H_PLUS / KW
    !    FREE_FE_III = (10.0D0**(42.97D0))/(27.0D0 * OH_MINUS * OH_MINUS * OH_MINUS)
    !end where
    !
    ! if(any(isnan(FREE_FE_III))) then
    !   print *,'REDOX_AND_SPECIATION:'
    !   print *,'1.FREE_FE_III is NaN:', FREE_FE_III
    !   stop
    ! end if
    !
    !!Fe(OH)3
    !where(FE_III_SALT_NO == 3)
    !    TEMP_K = TEMP + 273.15D0
    !
    !    LOG_KW = &
    !        -2.839710D2 + (1.3323D4 / TEMP_K) - (5.069842D-2 * TEMP_K) + &
    !        (1.0224447D2 * log10(TEMP_K)) - (1.119669D6 / (TEMP_K * TEMP_K))
    !
    !    OH_MINUS = H_PLUS / KW
    !    FREE_FE_III = (10.0D0**(37.08D0))/(27.0D0 * OH_MINUS * OH_MINUS * OH_MINUS)
    !end where
    !
    ! if(any(isnan(FREE_FE_III))) then
    !   print *,'REDOX_AND_SPECIATION:'
    !   print *,'2.FREE_FE_III is NaN:', FREE_FE_III
    !   print *,'OH_MINUS:',OH_MINUS
    !   print *,'KW:',KW
    !   print *,'H_PLUS:',H_PLUS
    !   stop
    ! end if

    ! Calculate the complexation inn case of iron III Hydroxides
    K1_FE_OH_3       = 10.0D0**(-3.05D0)
    BETA_2_FE_OH_3   = 10.0D0**(-6.31D0)
    BETA_3_FE_OH_3   = 10.0D0**(-13.8D0)
    BETA_4_FE_OH_3   = 10.0D0**(-22.7D0)
    BETA_2_2_FE_OH_3 = 10.0D0**(-2.91D0)
    BETA_4_3_FE_OH_3 = 10.0D0**(-5.77D0)

    !where((FE_III_SALT_NO == 2).or.(FE_III_SALT_NO == 3))
    !for a while assume that iron 3+ solubity is only related to Fe(OH3)
        TEMP_K = TEMP + 273.15D0

        LOG_KW = &
            -2.839710D2 + (1.3323D4 / TEMP_K) - (5.069842D-2 * TEMP_K) + &
            (1.0224447D2 * log10(TEMP_K)) - (1.119669D6 / (TEMP_K * TEMP_K))

        KW = 10.0D0**(LOG_KW)
        OH_MINUS = KW / H_PLUS
        !FREE_FE_III = (6.0D0**(-38.0D0))/(OH_MINUS * OH_MINUS * OH_MINUS)
        FE_III_FREE = 10.0D0**(3.96D0 - (3.0D0 * PH))

        H_PLUS_OVER_2 = H_PLUS * H_PLUS
        H_PLUS_OVER_3 = H_PLUS * H_PLUS * H_PLUS
        H_PLUS_OVER_4 = H_PLUS * H_PLUS * H_PLUS * H_PLUS

        FE_III_DISS = FE_III_FREE * &
            (1.0D0 + (K1_FE_OH_3 / (H_PLUS))  + (BETA_2_FE_OH_3 / H_PLUS_OVER_2) + &
             (BETA_3_FE_OH_3 / H_PLUS_OVER_3) + (BETA_4_FE_OH_3 / H_PLUS_OVER_4) + &
             ((2.0D0*BETA_2_2_FE_OH_3*FE_III_FREE)/H_PLUS_OVER_2) + &
             ((3.0D0*BETA_4_3_FE_OH_3*FE_III_FREE*FE_III_FREE)/H_PLUS_OVER_4));

    !end where

    if(any(IEEE_IS_NAN(FE_III_DISS))) then
       print *,'FE_III_DISS is NaN,  SEDIMENT LIBRARY'

       write(unit = *, fmt = '(2A15, 13A20)') 'BOX', 'SED LAYER', 'PH'&
             'FE_III_DISS', 'FE_III_FREE', 'K1_FE_OH_3', &
             'H_PLUS', 'H_PLUS_OVER_2', 'H_PLUS_OVER_3', 'H_PLUS_OVER_4', &
             'BETA_2_FE_OH_3', 'BETA_3_FE_OH_3', 'BETA_4_FE_OH_3', &
             'BETA_2_2_FE_OH_3', 'BETA_4_3_FE_OH_3'

       do i = 1, nkn
           do j = 1, NUM_SED_LAYERS
               write(unit = *, fmt = '(2i15, 13f20.15)') i, j, PH(i, j), &
                     FE_III_DISS   (i, j), FE_III_FREE     (i, j), K1_FE_OH_3            , &
                     H_PLUS        (i, j), H_PLUS_OVER_2   (i, j), H_PLUS_OVER_3   (i, j), &
                     H_PLUS_OVER_4 (i, j), BETA_2_FE_OH_3        , BETA_3_FE_OH_3        , &
                     BETA_4_FE_OH_3      , BETA_2_2_FE_OH_3      , BETA_4_3_FE_OH_3
           end do
       end do

       stop
    end if



    ! -------------------------------------------------------------------------
    ! END OF FE_III Species
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! MN_II Species
    ! -------------------------------------------------------------------------

    ! MN_CO3  / DISS_MN_II
    MN_CO3_OVER_MN_II  = 10.0D0**(-0.2D0 +  PH + log10(HCO3))

    ! MN_OH_2 / DISS_MN_II
    MN_OH_2_OVER_MN_II = 10.0D0**(-15.0D0 + (2.0D0*PH))

    ! MN_S / DISS_MN_II
    MN_S_OVER_MN_II    = &
        10.0D0**(34.0D0  - (8.0D0*PH) + log10((S_PLUS_6 / S_MOLAR_MASS_MG)) - (8.0D0*PE))

    ! Now find out which Mn II salt is more likely to form for each reactor
    MN_II_ACTIVITY_RATIOS (:, :, 1) = MN_CO3_OVER_MN_II
    MN_II_ACTIVITY_RATIOS (:, :, 2) = MN_OH_2_OVER_MN_II
    MN_II_ACTIVITY_RATIOS (:, :, 3) = MN_S_OVER_MN_II

    MN_II_SALT_NO  = maxloc(MN_II_ACTIVITY_RATIOS , dim = 3)

    !MnCO3
    where(MN_II_SALT_NO == 1)
        FREE_MN_II = (10.0D0 ** (8.03D0)) / CO3
    end where

    !Mn(OH)2
    where(MN_II_SALT_NO == 2)
        FREE_MN_II = (10.0D0 ** (11.14D0))/ (4 * OH_MINUS * OH_MINUS)
    end where

    !MnS
    where((MN_II_SALT_NO == 3).and.(S_MINUS_2_MOLAR > 1.0D-12))
        FREE_MN_II = 10.0D0**(-10.19D0) / S_MINUS_2_MOLAR
    end where

    where((MN_II_SALT_NO == 3).and.(S_MINUS_2_MOLAR <= 1.0D-12))
        FREE_MN_II = MN_II / MN_MOLAR_MASS_MG
    end where

    ! For a while no complex formation (to be fixed)
    MN_II_DISS = FREE_MN_II
    ! -------------------------------------------------------------------------
    ! END OF MN_II Species
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! MN_IV Species
    ! -------------------------------------------------------------------------
    ! We assume that MnO2 is the main MN 4+ species
    ! -------------------------------------------------------------------------
    ! END OF MN_IV Species
    ! -------------------------------------------------------------------------
end subroutine SED_REDOX_AND_SPECIATION
