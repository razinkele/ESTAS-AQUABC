module AQUABC_PELAGIC_TYPES
    use AQUABC_II_GLOBAL
    implicit none

    ! =========================================================================
    ! Derived types that bundle organism-specific kinetics constants.
    ! Each type field uses the same name as the corresponding dummy argument
    ! in the library subroutine, so an associate block provides a zero-change
    ! mapping for the subroutine body.
    ! =========================================================================

    ! ----- DIATOMS (25 constants) --------------------------------------------
    type :: t_diatom_params
        real(kind = DBL_PREC) :: KG_DIA_OPT_TEMP
        real(kind = DBL_PREC) :: DIA_OPT_TEMP_LR
        real(kind = DBL_PREC) :: DIA_OPT_TEMP_UR
        real(kind = DBL_PREC) :: EFF_DIA_GROWTH
        real(kind = DBL_PREC) :: KAPPA_DIA_UNDER_OPT_TEMP
        real(kind = DBL_PREC) :: KAPPA_DIA_OVER_OPT_TEMP
        real(kind = DBL_PREC) :: KR_DIA_20
        real(kind = DBL_PREC) :: THETA_KR_DIA
        real(kind = DBL_PREC) :: KD_DIA_20
        real(kind = DBL_PREC) :: THETA_KD_DIA
        real(kind = DBL_PREC) :: KHS_DIN_DIA
        real(kind = DBL_PREC) :: KHS_DIP_DIA
        real(kind = DBL_PREC) :: KHS_DSi_DIA
        real(kind = DBL_PREC) :: KHS_O2_DIA
        real(kind = DBL_PREC) :: FRAC_DIA_EXCR
        real(kind = DBL_PREC) :: I_S_DIA
        real(kind = DBL_PREC) :: DO_STR_HYPOX_DIA_D
        real(kind = DBL_PREC) :: THETA_HYPOX_DIA_D
        real(kind = DBL_PREC) :: EXPON_HYPOX_DIA_D
        real(kind = DBL_PREC) :: DIA_N_TO_C
        real(kind = DBL_PREC) :: DIA_P_TO_C
        real(kind = DBL_PREC) :: DIA_Si_TO_C
        real(kind = DBL_PREC) :: DIA_O2_TO_C
        real(kind = DBL_PREC) :: DIA_C_TO_CHLA
        real(kind = DBL_PREC) :: BETA_DIA
    end type t_diatom_params

    ! ----- NON-FIXING CYANOBACTERIA (24 constants) ---------------------------
    type :: t_cyn_params
        real(kind = DBL_PREC) :: KG_CYN_OPT_TEMP
        real(kind = DBL_PREC) :: CYN_OPT_TEMP_LR
        real(kind = DBL_PREC) :: CYN_OPT_TEMP_UR
        real(kind = DBL_PREC) :: EFF_CYN_GROWTH
        real(kind = DBL_PREC) :: KAPPA_CYN_UNDER_OPT_TEMP
        real(kind = DBL_PREC) :: KAPPA_CYN_OVER_OPT_TEMP
        real(kind = DBL_PREC) :: KR_CYN_20
        real(kind = DBL_PREC) :: THETA_KR_CYN
        real(kind = DBL_PREC) :: KD_CYN_20
        real(kind = DBL_PREC) :: THETA_KD_CYN
        real(kind = DBL_PREC) :: KHS_DIN_CYN
        real(kind = DBL_PREC) :: KHS_DIP_CYN
        real(kind = DBL_PREC) :: KHS_O2_CYN
        real(kind = DBL_PREC) :: FRAC_CYN_EXCR
        real(kind = DBL_PREC) :: I_S_CYN
        real(kind = DBL_PREC) :: BETA_CYN
        real(kind = DBL_PREC) :: DO_STR_HYPOX_CYN_D
        real(kind = DBL_PREC) :: THETA_HYPOX_CYN_D
        real(kind = DBL_PREC) :: EXPON_HYPOX_CYN_D
        real(kind = DBL_PREC) :: CYN_N_TO_C
        real(kind = DBL_PREC) :: CYN_P_TO_C
        real(kind = DBL_PREC) :: CYN_O2_TO_C
        real(kind = DBL_PREC) :: CYN_C_TO_CHLA
        real(kind = DBL_PREC) :: frac_avail_DON
    end type t_cyn_params

    ! ----- NITROGEN-FIXING CYANOBACTERIA (25 constants) ----------------------
    type :: t_fix_cyn_params
        real(kind = DBL_PREC) :: KG_FIX_CYN_OPT_TEMP
        real(kind = DBL_PREC) :: FIX_CYN_OPT_TEMP_LR
        real(kind = DBL_PREC) :: FIX_CYN_OPT_TEMP_UR
        real(kind = DBL_PREC) :: EFF_FIX_CYN_GROWTH
        real(kind = DBL_PREC) :: KAPPA_FIX_CYN_UNDER_OPT_TEMP
        real(kind = DBL_PREC) :: KAPPA_FIX_CYN_OVER_OPT_TEMP
        real(kind = DBL_PREC) :: KR_FIX_CYN_20
        real(kind = DBL_PREC) :: THETA_KR_FIX_CYN
        real(kind = DBL_PREC) :: KD_FIX_CYN_20
        real(kind = DBL_PREC) :: THETA_KD_FIX_CYN
        real(kind = DBL_PREC) :: KHS_DIN_FIX_CYN
        real(kind = DBL_PREC) :: KHS_DIP_FIX_CYN
        real(kind = DBL_PREC) :: KHS_O2_FIX_CYN
        real(kind = DBL_PREC) :: I_S_FIX_CYN
        real(kind = DBL_PREC) :: DO_STR_HYPOX_FIX_CYN_D
        real(kind = DBL_PREC) :: THETA_HYPOX_FIX_CYN_D
        real(kind = DBL_PREC) :: EXPON_HYPOX_FIX_CYN_D
        real(kind = DBL_PREC) :: FIX_CYN_N_TO_C
        real(kind = DBL_PREC) :: FIX_CYN_P_TO_C
        real(kind = DBL_PREC) :: FIX_CYN_O2_TO_C
        real(kind = DBL_PREC) :: FIX_CYN_C_TO_CHLA
        real(kind = DBL_PREC) :: FRAC_FIX_CYN_EXCR
        real(kind = DBL_PREC) :: R_FIX
        real(kind = DBL_PREC) :: K_FIX
        real(kind = DBL_PREC) :: BETA_FIX_CYN
        real(kind = DBL_PREC) :: frac_avail_DON
    end type t_fix_cyn_params

    ! ----- OTHER PLANKTONIC ALGAE (23 constants) -----------------------------
    type :: t_opa_params
        real(kind = DBL_PREC) :: KG_OPA_OPT_TEMP
        real(kind = DBL_PREC) :: OPA_OPT_TEMP_LR
        real(kind = DBL_PREC) :: OPA_OPT_TEMP_UR
        real(kind = DBL_PREC) :: EFF_OPA_GROWTH
        real(kind = DBL_PREC) :: KAPPA_OPA_UNDER_OPT_TEMP
        real(kind = DBL_PREC) :: KAPPA_OPA_OVER_OPT_TEMP
        real(kind = DBL_PREC) :: KR_OPA_20
        real(kind = DBL_PREC) :: THETA_KR_OPA
        real(kind = DBL_PREC) :: KD_OPA_20
        real(kind = DBL_PREC) :: THETA_KD_OPA
        real(kind = DBL_PREC) :: KHS_DIN_OPA
        real(kind = DBL_PREC) :: KHS_DIP_OPA
        real(kind = DBL_PREC) :: KHS_O2_OPA
        real(kind = DBL_PREC) :: FRAC_OPA_EXCR
        real(kind = DBL_PREC) :: I_S_OPA
        real(kind = DBL_PREC) :: DO_STR_HYPOX_OPA_D
        real(kind = DBL_PREC) :: THETA_HYPOX_OPA_D
        real(kind = DBL_PREC) :: EXPON_HYPOX_OPA_D
        real(kind = DBL_PREC) :: OPA_N_TO_C
        real(kind = DBL_PREC) :: OPA_P_TO_C
        real(kind = DBL_PREC) :: OPA_O2_TO_C
        real(kind = DBL_PREC) :: OPA_C_TO_CHLA
        real(kind = DBL_PREC) :: BETA_OPA
    end type t_opa_params

    ! ----- NOSTOCALES (32 constants) -----------------------------------------
    type :: t_nost_params
        real(kind = DBL_PREC) :: KG_NOST_VEG_HET_OPT_TEMP
        real(kind = DBL_PREC) :: FRAC_NOST_GROWTH
        real(kind = DBL_PREC) :: NOST_VEG_HET_OPT_TEMP_LR
        real(kind = DBL_PREC) :: NOST_VEG_HET_OPT_TEMP_UR
        real(kind = DBL_PREC) :: EFF_NOST_VEG_HET_GROWTH
        real(kind = DBL_PREC) :: KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP
        real(kind = DBL_PREC) :: KAPPA_NOST_VEG_HET_OVER_OPT_TEMP
        real(kind = DBL_PREC) :: KR_NOST_VEG_HET_20
        real(kind = DBL_PREC) :: THETA_KR_NOST_VEG_HET
        real(kind = DBL_PREC) :: KD_NOST_VEG_HET_20
        real(kind = DBL_PREC) :: THETA_KD_NOST_VEG_HET
        real(kind = DBL_PREC) :: KHS_DN_NOST_VEG_HET
        real(kind = DBL_PREC) :: KHS_DP_NOST_VEG_HET
        real(kind = DBL_PREC) :: KHS_O2_NOST_VEG_HET
        real(kind = DBL_PREC) :: I_S_NOST_VEG_HET
        real(kind = DBL_PREC) :: DO_STR_HYPOX_NOST_VEG_HET_D
        real(kind = DBL_PREC) :: THETA_HYPOX_NOST_VEG_HET_D
        real(kind = DBL_PREC) :: EXPON_HYPOX_NOST_VEG_HET_D
        real(kind = DBL_PREC) :: NOST_C_TO_CHLA
        real(kind = DBL_PREC) :: FRAC_NOST_VEG_HET_EXCR
        real(kind = DBL_PREC) :: KR_GERM_AKI
        real(kind = DBL_PREC) :: KN_GERM_AKI
        real(kind = DBL_PREC) :: KR_FORM_AKI
        real(kind = DBL_PREC) :: DAY_FORM_AKI
        real(kind = DBL_PREC) :: T_FORM_AKI
        real(kind = DBL_PREC) :: T_GERM_AKI
        real(kind = DBL_PREC) :: K_LOSS_AKI
        real(kind = DBL_PREC) :: K_MORT_AKI_20
        real(kind = DBL_PREC) :: THETA_K_MORT_AKI
        real(kind = DBL_PREC) :: KM_DENS_VEG_HET
        real(kind = DBL_PREC) :: BETA_NOST_VEG_HET
    end type t_nost_params

    ! ----- ZOOPLANKTON (37 constants) ----------------------------------------
    type :: t_zoo_params
        real(kind = DBL_PREC) :: KG_ZOO_OPT_TEMP
        real(kind = DBL_PREC) :: ZOO_OPT_TEMP_LR
        real(kind = DBL_PREC) :: ZOO_OPT_TEMP_UR
        real(kind = DBL_PREC) :: EFF_ZOO_GROWTH
        real(kind = DBL_PREC) :: KAPPA_ZOO_UNDER_OPT_TEMP
        real(kind = DBL_PREC) :: KAPPA_ZOO_OVER_OPT_TEMP
        real(kind = DBL_PREC) :: GRAT_ZOO_DIA
        real(kind = DBL_PREC) :: GRAT_ZOO_CYN
        real(kind = DBL_PREC) :: GRAT_ZOO_OPA
        real(kind = DBL_PREC) :: GRAT_ZOO_FIX_CYN
        real(kind = DBL_PREC) :: GRAT_ZOO_NOST_VEG_HET
        real(kind = DBL_PREC) :: GRAT_ZOO_DET_PART_ORG_C
        real(kind = DBL_PREC) :: PREF_ZOO_DIA
        real(kind = DBL_PREC) :: PREF_ZOO_CYN
        real(kind = DBL_PREC) :: PREF_ZOO_FIX_CYN
        real(kind = DBL_PREC) :: PREF_ZOO_NOST_VEG_HET
        real(kind = DBL_PREC) :: PREF_ZOO_OPA
        real(kind = DBL_PREC) :: PREF_ZOO_DET_PART_ORG_C
        real(kind = DBL_PREC) :: KHS_DIA_C_ZOO
        real(kind = DBL_PREC) :: KHS_CYN_C_ZOO
        real(kind = DBL_PREC) :: KHS_FIX_CYN_C_ZOO
        real(kind = DBL_PREC) :: KHS_NOST_VEG_HET_C_ZOO
        real(kind = DBL_PREC) :: KHS_OPA_C_ZOO
        real(kind = DBL_PREC) :: KHS_DET_PART_ORG_C_ZOO
        real(kind = DBL_PREC) :: FOOD_MIN_ZOO
        real(kind = DBL_PREC) :: KE_ZOO
        real(kind = DBL_PREC) :: FRAC_ZOO_EX_ORG
        real(kind = DBL_PREC) :: KR_ZOO_20
        real(kind = DBL_PREC) :: THETA_KR_ZOO
        real(kind = DBL_PREC) :: KD_ZOO_20
        real(kind = DBL_PREC) :: THETA_KD_ZOO
        real(kind = DBL_PREC) :: DO_STR_HYPOX_ZOO_D
        real(kind = DBL_PREC) :: THETA_HYPOX_ZOO_D
        real(kind = DBL_PREC) :: EXPON_HYPOX_ZOO_D
        real(kind = DBL_PREC) :: ZOO_N_TO_C
        real(kind = DBL_PREC) :: ZOO_P_TO_C
        real(kind = DBL_PREC) :: ZOO_O2_TO_C
    end type t_zoo_params

contains

    ! =========================================================================
    ! Populate routines: copy module-level globals into the derived types.
    ! Called once during initialization (CALLED_BEFORE block).
    ! =========================================================================

    subroutine populate_diatom_params(p)
        use AQUABC_PELAGIC_MODEL_CONSTANTS
        type(t_diatom_params), intent(out) :: p
        p%KG_DIA_OPT_TEMP          = KG_DIA_OPT_TEMP
        p%DIA_OPT_TEMP_LR          = DIA_OPT_TEMP_LR
        p%DIA_OPT_TEMP_UR          = DIA_OPT_TEMP_UR
        p%EFF_DIA_GROWTH           = EFF_DIA_GROWTH
        p%KAPPA_DIA_UNDER_OPT_TEMP = KAPPA_DIA_UNDER_OPT_TEMP
        p%KAPPA_DIA_OVER_OPT_TEMP  = KAPPA_DIA_OVER_OPT_TEMP
        p%KR_DIA_20                = KR_DIA_20
        p%THETA_KR_DIA             = THETA_KR_DIA
        p%KD_DIA_20                = KD_DIA_20
        p%THETA_KD_DIA             = THETA_KD_DIA
        p%KHS_DIN_DIA              = KHS_DIN_DIA
        p%KHS_DIP_DIA              = KHS_DIP_DIA
        p%KHS_DSi_DIA              = KHS_DSi_DIA
        p%KHS_O2_DIA               = KHS_O2_DIA
        p%FRAC_DIA_EXCR            = FRAC_DIA_EXCR
        p%I_S_DIA                  = I_S_DIA
        p%DO_STR_HYPOX_DIA_D       = DO_STR_HYPOX_DIA_D
        p%THETA_HYPOX_DIA_D        = THETA_HYPOX_DIA_D
        p%EXPON_HYPOX_DIA_D        = EXPON_HYPOX_DIA_D
        p%DIA_N_TO_C               = DIA_N_TO_C
        p%DIA_P_TO_C               = DIA_P_TO_C
        p%DIA_Si_TO_C              = DIA_Si_TO_C
        p%DIA_O2_TO_C              = DIA_O2_TO_C
        p%DIA_C_TO_CHLA            = DIA_C_TO_CHLA
        p%BETA_DIA                 = BETA_DIA
    end subroutine populate_diatom_params

    subroutine populate_cyn_params(p)
        use AQUABC_PELAGIC_MODEL_CONSTANTS
        type(t_cyn_params), intent(out) :: p
        p%KG_CYN_OPT_TEMP          = KG_CYN_OPT_TEMP
        p%CYN_OPT_TEMP_LR          = CYN_OPT_TEMP_LR
        p%CYN_OPT_TEMP_UR          = CYN_OPT_TEMP_UR
        p%EFF_CYN_GROWTH           = EFF_CYN_GROWTH
        p%KAPPA_CYN_UNDER_OPT_TEMP = KAPPA_CYN_UNDER_OPT_TEMP
        p%KAPPA_CYN_OVER_OPT_TEMP  = KAPPA_CYN_OVER_OPT_TEMP
        p%KR_CYN_20                = KR_CYN_20
        p%THETA_KR_CYN             = THETA_KR_CYN
        p%KD_CYN_20                = KD_CYN_20
        p%THETA_KD_CYN             = THETA_KD_CYN
        p%KHS_DIN_CYN              = KHS_DIN_CYN
        p%KHS_DIP_CYN              = KHS_DIP_CYN
        p%KHS_O2_CYN               = KHS_O2_CYN
        p%FRAC_CYN_EXCR            = FRAC_CYN_EXCR
        p%I_S_CYN                  = I_S_CYN
        p%BETA_CYN                 = BETA_CYN
        p%DO_STR_HYPOX_CYN_D       = DO_STR_HYPOX_CYN_D
        p%THETA_HYPOX_CYN_D        = THETA_HYPOX_CYN_D
        p%EXPON_HYPOX_CYN_D        = EXPON_HYPOX_CYN_D
        p%CYN_N_TO_C               = CYN_N_TO_C
        p%CYN_P_TO_C               = CYN_P_TO_C
        p%CYN_O2_TO_C              = CYN_O2_TO_C
        p%CYN_C_TO_CHLA            = CYN_C_TO_CHLA
        p%frac_avail_DON           = frac_avail_DON
    end subroutine populate_cyn_params

    subroutine populate_fix_cyn_params(p)
        use AQUABC_PELAGIC_MODEL_CONSTANTS
        type(t_fix_cyn_params), intent(out) :: p
        p%KG_FIX_CYN_OPT_TEMP          = KG_FIX_CYN_OPT_TEMP
        p%FIX_CYN_OPT_TEMP_LR          = FIX_CYN_OPT_TEMP_LR
        p%FIX_CYN_OPT_TEMP_UR          = FIX_CYN_OPT_TEMP_UR
        p%EFF_FIX_CYN_GROWTH           = EFF_FIX_CYN_GROWTH
        p%KAPPA_FIX_CYN_UNDER_OPT_TEMP = KAPPA_FIX_CYN_UNDER_OPT_TEMP
        p%KAPPA_FIX_CYN_OVER_OPT_TEMP  = KAPPA_FIX_CYN_OVER_OPT_TEMP
        p%KR_FIX_CYN_20                = KR_FIX_CYN_20
        p%THETA_KR_FIX_CYN             = THETA_KR_FIX_CYN
        p%KD_FIX_CYN_20                = KD_FIX_CYN_20
        p%THETA_KD_FIX_CYN             = THETA_KD_FIX_CYN
        p%KHS_DIN_FIX_CYN              = KHS_DIN_FIX_CYN
        p%KHS_DIP_FIX_CYN              = KHS_DIP_FIX_CYN
        p%KHS_O2_FIX_CYN               = KHS_O2_FIX_CYN
        p%I_S_FIX_CYN                  = I_S_FIX_CYN
        p%DO_STR_HYPOX_FIX_CYN_D       = DO_STR_HYPOX_FIX_CYN_D
        p%THETA_HYPOX_FIX_CYN_D        = THETA_HYPOX_FIX_CYN_D
        p%EXPON_HYPOX_FIX_CYN_D        = EXPON_HYPOX_FIX_CYN_D
        p%FIX_CYN_N_TO_C               = FIX_CYN_N_TO_C
        p%FIX_CYN_P_TO_C               = FIX_CYN_P_TO_C
        p%FIX_CYN_O2_TO_C              = FIX_CYN_O2_TO_C
        p%FIX_CYN_C_TO_CHLA            = FIX_CYN_C_TO_CHLA
        p%FRAC_FIX_CYN_EXCR            = FRAC_FIX_CYN_EXCR
        p%R_FIX                         = R_FIX
        p%K_FIX                         = K_FIX
        p%BETA_FIX_CYN                  = BETA_FIX_CYN
        p%frac_avail_DON                = frac_avail_DON
    end subroutine populate_fix_cyn_params

    subroutine populate_opa_params(p)
        use AQUABC_PELAGIC_MODEL_CONSTANTS
        type(t_opa_params), intent(out) :: p
        p%KG_OPA_OPT_TEMP          = KG_OPA_OPT_TEMP
        p%OPA_OPT_TEMP_LR          = OPA_OPT_TEMP_LR
        p%OPA_OPT_TEMP_UR          = OPA_OPT_TEMP_UR
        p%EFF_OPA_GROWTH           = EFF_OPA_GROWTH
        p%KAPPA_OPA_UNDER_OPT_TEMP = KAPPA_OPA_UNDER_OPT_TEMP
        p%KAPPA_OPA_OVER_OPT_TEMP  = KAPPA_OPA_OVER_OPT_TEMP
        p%KR_OPA_20                = KR_OPA_20
        p%THETA_KR_OPA             = THETA_KR_OPA
        p%KD_OPA_20                = KD_OPA_20
        p%THETA_KD_OPA             = THETA_KD_OPA
        p%KHS_DIN_OPA              = KHS_DIN_OPA
        p%KHS_DIP_OPA              = KHS_DIP_OPA
        p%KHS_O2_OPA               = KHS_O2_OPA
        p%FRAC_OPA_EXCR            = FRAC_OPA_EXCR
        p%I_S_OPA                  = I_S_OPA
        p%DO_STR_HYPOX_OPA_D       = DO_STR_HYPOX_OPA_D
        p%THETA_HYPOX_OPA_D        = THETA_HYPOX_OPA_D
        p%EXPON_HYPOX_OPA_D        = EXPON_HYPOX_OPA_D
        p%OPA_N_TO_C               = OPA_N_TO_C
        p%OPA_P_TO_C               = OPA_P_TO_C
        p%OPA_O2_TO_C              = OPA_O2_TO_C
        p%OPA_C_TO_CHLA            = OPA_C_TO_CHLA
        p%BETA_OPA                 = BETA_OPA
    end subroutine populate_opa_params

    subroutine populate_nost_params(p)
        use AQUABC_PELAGIC_MODEL_CONSTANTS
        type(t_nost_params), intent(out) :: p
        p%KG_NOST_VEG_HET_OPT_TEMP          = KG_NOST_VEG_HET_OPT_TEMP
        p%FRAC_NOST_GROWTH                   = FRAC_NOST_GROWTH
        p%NOST_VEG_HET_OPT_TEMP_LR          = NOST_VEG_HET_OPT_TEMP_LR
        p%NOST_VEG_HET_OPT_TEMP_UR          = NOST_VEG_HET_OPT_TEMP_UR
        p%EFF_NOST_VEG_HET_GROWTH           = EFF_NOST_VEG_HET_GROWTH
        p%KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP = KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP
        p%KAPPA_NOST_VEG_HET_OVER_OPT_TEMP  = KAPPA_NOST_VEG_HET_OVER_OPT_TEMP
        p%KR_NOST_VEG_HET_20                = KR_NOST_VEG_HET_20
        p%THETA_KR_NOST_VEG_HET             = THETA_KR_NOST_VEG_HET
        p%KD_NOST_VEG_HET_20                = KD_NOST_VEG_HET_20
        p%THETA_KD_NOST_VEG_HET             = THETA_KD_NOST_VEG_HET
        p%KHS_DN_NOST_VEG_HET               = KHS_DN_NOST_VEG_HET
        p%KHS_DP_NOST_VEG_HET               = KHS_DP_NOST_VEG_HET
        p%KHS_O2_NOST_VEG_HET               = KHS_O2_NOST_VEG_HET
        p%I_S_NOST_VEG_HET                  = I_S_NOST_VEG_HET
        p%DO_STR_HYPOX_NOST_VEG_HET_D       = DO_STR_HYPOX_NOST_VEG_HET_D
        p%THETA_HYPOX_NOST_VEG_HET_D        = THETA_HYPOX_NOST_VEG_HET_D
        p%EXPON_HYPOX_NOST_VEG_HET_D        = EXPON_HYPOX_NOST_VEG_HET_D
        p%NOST_C_TO_CHLA                    = NOST_C_TO_CHLA
        p%FRAC_NOST_VEG_HET_EXCR            = FRAC_NOST_VEG_HET_EXCR
        ! Note: module vars have different names from dummy args for these
        p%KR_GERM_AKI                       = P_GERM_AKI
        p%KN_GERM_AKI                       = N_GERM_AKI
        p%KR_FORM_AKI                       = P_FORM_AKI
        p%DAY_FORM_AKI                      = DAY_FORM_AKI
        p%T_FORM_AKI                        = T_FORM_AKI
        p%T_GERM_AKI                        = T_GERM_AKI
        p%K_LOSS_AKI                        = K_LOSS_AKI
        p%K_MORT_AKI_20                     = K_MORT_AKI_20
        p%THETA_K_MORT_AKI                  = THETA_K_MORT_AKI
        p%KM_DENS_VEG_HET                   = M_DENS_VEG_HET
        p%BETA_NOST_VEG_HET                 = BETA_NOST_VEG_HET
    end subroutine populate_nost_params

    subroutine populate_zoo_params(p)
        use AQUABC_PELAGIC_MODEL_CONSTANTS
        type(t_zoo_params), intent(out) :: p
        p%KG_ZOO_OPT_TEMP          = KG_ZOO_OPT_TEMP
        p%ZOO_OPT_TEMP_LR          = ZOO_OPT_TEMP_LR
        p%ZOO_OPT_TEMP_UR          = ZOO_OPT_TEMP_UR
        p%EFF_ZOO_GROWTH           = EFF_ZOO_GROWTH
        p%KAPPA_ZOO_UNDER_OPT_TEMP = KAPPA_ZOO_UNDER_OPT_TEMP
        p%KAPPA_ZOO_OVER_OPT_TEMP  = KAPPA_ZOO_OVER_OPT_TEMP
        p%GRAT_ZOO_DIA             = GRAT_ZOO_DIA
        p%GRAT_ZOO_CYN             = GRAT_ZOO_CYN
        p%GRAT_ZOO_OPA             = GRAT_ZOO_OPA
        p%GRAT_ZOO_FIX_CYN        = GRAT_ZOO_FIX_CYN
        p%GRAT_ZOO_NOST_VEG_HET   = GRAT_ZOO_NOST_VEG_HET
        p%GRAT_ZOO_DET_PART_ORG_C = GRAT_ZOO_DET_PART_ORG_C
        p%PREF_ZOO_DIA             = PREF_ZOO_DIA
        p%PREF_ZOO_CYN             = PREF_ZOO_CYN
        p%PREF_ZOO_FIX_CYN        = PREF_ZOO_FIX_CYN
        p%PREF_ZOO_NOST_VEG_HET   = PREF_ZOO_NOST_VEG_HET
        p%PREF_ZOO_OPA             = PREF_ZOO_OPA
        p%PREF_ZOO_DET_PART_ORG_C = PREF_ZOO_DET_PART_ORG_C
        p%KHS_DIA_C_ZOO            = KHS_DIA_C_ZOO
        p%KHS_CYN_C_ZOO           = KHS_CYN_C_ZOO
        p%KHS_FIX_CYN_C_ZOO       = KHS_FIX_CYN_C_ZOO
        p%KHS_NOST_VEG_HET_C_ZOO  = KHS_NOST_VEG_HET_C_ZOO
        p%KHS_OPA_C_ZOO           = KHS_OPA_C_ZOO
        p%KHS_DET_PART_ORG_C_ZOO  = KHS_DET_PART_ORG_C_ZOO
        p%FOOD_MIN_ZOO             = FOOD_MIN_ZOO
        p%KE_ZOO                   = KE_ZOO
        p%FRAC_ZOO_EX_ORG         = FRAC_ZOO_EX_ORG
        p%KR_ZOO_20               = KR_ZOO_20
        p%THETA_KR_ZOO            = THETA_KR_ZOO
        p%KD_ZOO_20               = KD_ZOO_20
        p%THETA_KD_ZOO            = THETA_KD_ZOO
        p%DO_STR_HYPOX_ZOO_D      = DO_STR_HYPOX_ZOO_D
        p%THETA_HYPOX_ZOO_D       = THETA_HYPOX_ZOO_D
        p%EXPON_HYPOX_ZOO_D       = EXPON_HYPOX_ZOO_D
        p%ZOO_N_TO_C              = ZOO_N_TO_C
        p%ZOO_P_TO_C              = ZOO_P_TO_C
        p%ZOO_O2_TO_C             = ZOO_O2_TO_C
    end subroutine populate_zoo_params

end module AQUABC_PELAGIC_TYPES
