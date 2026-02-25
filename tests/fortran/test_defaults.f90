! Shared test defaults module
! Provides subroutines that populate derived types with realistic parameter
! values from DEFAULT_PELAGIC_MODEL_CONSTANTS for use in kinetics unit tests.

module test_defaults
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    implicit none

contains

    subroutine set_default_diatom_params(p)
        type(t_diatom_params), intent(out) :: p
        p%KG_DIA_OPT_TEMP          = 3.7D0
        p%DIA_OPT_TEMP_LR          = 1.0D0
        p%DIA_OPT_TEMP_UR          = 24.0D0
        p%EFF_DIA_GROWTH           = 0.95D0
        p%KAPPA_DIA_UNDER_OPT_TEMP = 0.0D0
        p%KAPPA_DIA_OVER_OPT_TEMP  = 35.0D0
        p%KR_DIA_20                = 0.05D0
        p%THETA_KR_DIA             = 1.04D0
        p%KD_DIA_20                = 0.12D0
        p%THETA_KD_DIA             = 1.02D0
        p%KHS_DIN_DIA              = 0.010D0
        p%KHS_DIP_DIA              = 0.005D0
        p%KHS_DSi_DIA              = 0.013D0
        p%KHS_O2_DIA               = 0.60D0
        p%FRAC_DIA_EXCR            = 0.30D0
        p%I_S_DIA                  = 100.0D0
        p%DO_STR_HYPOX_DIA_D       = 0.70D0
        p%THETA_HYPOX_DIA_D        = 1.20D0
        p%EXPON_HYPOX_DIA_D        = 1.04D0
        p%DIA_N_TO_C               = 0.22D0
        p%DIA_P_TO_C               = 0.024D0
        p%DIA_Si_TO_C              = 0.25D0
        p%DIA_O2_TO_C              = 2.66D0
        p%DIA_C_TO_CHLA            = 30.0D0
        p%BETA_DIA                 = 0.0D0
    end subroutine set_default_diatom_params

    subroutine set_default_cyn_params(p)
        type(t_cyn_params), intent(out) :: p
        p%KG_CYN_OPT_TEMP          = 2.4D0
        p%CYN_OPT_TEMP_LR          = 15.0D0
        p%CYN_OPT_TEMP_UR          = 26.0D0
        p%EFF_CYN_GROWTH           = 0.95D0
        p%KAPPA_CYN_UNDER_OPT_TEMP = 0.0D0
        p%KAPPA_CYN_OVER_OPT_TEMP  = 38.0D0
        p%KR_CYN_20                = 0.06D0
        p%THETA_KR_CYN             = 1.04D0
        p%KD_CYN_20                = 0.125D0
        p%THETA_KD_CYN             = 1.05D0
        p%KHS_DIN_CYN              = 0.009D0
        p%KHS_DIP_CYN              = 0.008D0
        p%KHS_O2_CYN               = 0.60D0
        p%FRAC_CYN_EXCR            = 0.30D0
        p%I_S_CYN                  = 100.0D0
        p%BETA_CYN                 = 0.0D0
        p%DO_STR_HYPOX_CYN_D       = 0.70D0
        p%THETA_HYPOX_CYN_D        = 1.50D0
        p%EXPON_HYPOX_CYN_D        = 1.10D0
        p%CYN_N_TO_C               = 0.220D0
        p%CYN_P_TO_C               = 0.024D0
        p%CYN_O2_TO_C              = 2.66D0
        p%CYN_C_TO_CHLA            = 40.0D0
        p%frac_avail_DON           = 0.0D0
    end subroutine set_default_cyn_params

    subroutine set_default_zoo_params(p)
        type(t_zoo_params), intent(out) :: p
        p%KG_ZOO_OPT_TEMP          = 0.45D0
        p%ZOO_OPT_TEMP_LR          = 10.0D0
        p%ZOO_OPT_TEMP_UR          = 25.0D0
        p%EFF_ZOO_GROWTH           = 0.80D0
        p%KAPPA_ZOO_UNDER_OPT_TEMP = 0.0D0
        p%KAPPA_ZOO_OVER_OPT_TEMP  = 35.0D0
        p%GRAT_ZOO_DIA             = 1.0D0
        p%GRAT_ZOO_CYN             = 1.0D0
        p%GRAT_ZOO_OPA             = 1.0D0
        p%GRAT_ZOO_FIX_CYN        = 1.0D0
        p%GRAT_ZOO_NOST_VEG_HET   = 1.0D0
        p%GRAT_ZOO_DET_PART_ORG_C = 0.50D0
        p%PREF_ZOO_DIA             = 0.26D0
        p%PREF_ZOO_CYN             = 0.10D0
        p%PREF_ZOO_FIX_CYN        = 0.07D0
        p%PREF_ZOO_NOST_VEG_HET   = 0.00D0
        p%PREF_ZOO_OPA             = 0.37D0
        p%PREF_ZOO_DET_PART_ORG_C = 0.20D0
        p%KHS_DIA_C_ZOO            = 0.10D0
        p%KHS_CYN_C_ZOO           = 0.07D0
        p%KHS_FIX_CYN_C_ZOO       = 0.07D0
        p%KHS_NOST_VEG_HET_C_ZOO  = 0.07D0
        p%KHS_OPA_C_ZOO           = 0.15D0
        p%KHS_DET_PART_ORG_C_ZOO  = 0.50D0
        p%FOOD_MIN_ZOO             = 0.02D0
        p%KE_ZOO                   = 0.05D0
        p%FRAC_ZOO_EX_ORG         = 0.30D0
        p%KR_ZOO_20               = 0.03D0
        p%THETA_KR_ZOO            = 1.04D0
        p%KD_ZOO_20               = 0.15D0
        p%THETA_KD_ZOO            = 1.04D0
        p%DO_STR_HYPOX_ZOO_D      = 2.00D0
        p%THETA_HYPOX_ZOO_D       = 1.20D0
        p%EXPON_HYPOX_ZOO_D       = 1.06D0
        p%ZOO_N_TO_C              = 0.220D0
        p%ZOO_P_TO_C              = 0.024D0
        p%ZOO_O2_TO_C             = 2.66D0
    end subroutine set_default_zoo_params

    subroutine set_default_redox_params(p)
        type(t_redox_params), intent(out) :: p
        p%K_HS_DOXY_RED_LIM      = 1.0D0
        p%K_HS_NO3N_RED_LIM      = 1.0D0
        p%K_HS_MN_IV_RED_LIM     = 1.0D0
        p%K_HS_FE_III_RED_LIM    = 1.0D0
        p%K_HS_S_PLUS_6_RED_LIM  = 1.0D0
        p%K_HS_DOXY_RED_INHB     = 0.10D0
        p%K_HS_NO3N_RED_INHB     = 0.10D0
        p%K_HS_MN_IV_RED_INHB    = 0.10D0
        p%K_HS_FE_III_RED_INHB   = 0.10D0
        p%K_HS_S_PLUS_6_RED_INHB = 0.10D0
    end subroutine set_default_redox_params

    subroutine set_default_docmin_params(p)
        type(t_docmin_params), intent(out) :: p
        p%FAC_PHYT_AMIN_DOC          = 0.0045D0
        p%K_MIN_DOC_DOXY_20          = 0.010D0
        p%K_MIN_DOC_NO3N_20          = 0.025D0
        p%K_MIN_DOC_MN_IV_20         = 0.025D0
        p%K_MIN_DOC_FE_III_20        = 0.025D0
        p%K_MIN_DOC_S_PLUS_6_20      = 0.025D0
        p%K_MIN_DOC_DOC_20           = 0.025D0
        p%THETA_K_MIN_DOC_DOXY       = 1.04D0
        p%THETA_K_MIN_DOC_NO3N       = 1.04D0
        p%THETA_K_MIN_DOC_MN_IV      = 1.04D0
        p%THETA_K_MIN_DOC_FE_III     = 1.04D0
        p%THETA_K_MIN_DOC_S_PLUS_6   = 1.04D0
        p%THETA_K_MIN_DOC_DOC        = 1.04D0
        p%K_HS_DOC_MIN_DOXY          = 1.0D0
        p%K_HS_DOC_MIN_NO3N          = 1.0D0
        p%K_HS_DOC_MIN_MN_IV         = 1.0D0
        p%K_HS_DOC_MIN_FE_III        = 1.0D0
        p%K_HS_DOC_MIN_S_PLUS_6      = 1.0D0
        p%K_HS_DOC_MIN_DOC           = 1.0D0
        p%PH_MIN_DOC_MIN_DOXY        = 6.0D0
        p%PH_MIN_DOC_MIN_NO3N        = 6.0D0
        p%PH_MIN_DOC_MIN_MN_IV       = 6.0D0
        p%PH_MIN_DOC_MIN_FE_III      = 6.0D0
        p%PH_MIN_DOC_MIN_S_PLUS_6    = 6.0D0
        p%PH_MIN_DOC_MIN_DOC         = 6.0D0
        p%PH_MAX_DOC_MIN_DOXY        = 9.0D0
        p%PH_MAX_DOC_MIN_NO3N        = 9.0D0
        p%PH_MAX_DOC_MIN_MN_IV       = 9.0D0
        p%PH_MAX_DOC_MIN_FE_III      = 9.0D0
        p%PH_MAX_DOC_MIN_S_PLUS_6    = 9.0D0
        p%PH_MAX_DOC_MIN_DOC         = 9.0D0
    end subroutine set_default_docmin_params

    subroutine set_default_fix_cyn_params(p)
        type(t_fix_cyn_params), intent(out) :: p
        p%KG_FIX_CYN_OPT_TEMP          = 3.5D0
        p%FIX_CYN_OPT_TEMP_LR          = 18.0D0
        p%FIX_CYN_OPT_TEMP_UR          = 26.0D0
        p%EFF_FIX_CYN_GROWTH           = 0.95D0
        p%KAPPA_FIX_CYN_UNDER_OPT_TEMP = 0.0D0
        p%KAPPA_FIX_CYN_OVER_OPT_TEMP  = 38.0D0
        p%KR_FIX_CYN_20                = 0.06D0
        p%THETA_KR_FIX_CYN             = 1.04D0
        p%KD_FIX_CYN_20                = 0.10D0
        p%THETA_KD_FIX_CYN             = 1.05D0
        p%KHS_DIN_FIX_CYN              = 0.01D0
        p%KHS_DIP_FIX_CYN              = 0.005D0
        p%KHS_O2_FIX_CYN               = 0.60D0
        p%I_S_FIX_CYN                  = 100.0D0
        p%DO_STR_HYPOX_FIX_CYN_D       = 0.70D0
        p%THETA_HYPOX_FIX_CYN_D        = 1.50D0
        p%EXPON_HYPOX_FIX_CYN_D        = 1.10D0
        p%FIX_CYN_N_TO_C               = 0.220D0
        p%FIX_CYN_P_TO_C               = 0.024D0
        p%FIX_CYN_O2_TO_C              = 2.66D0
        p%FIX_CYN_C_TO_CHLA            = 40.0D0
        p%FRAC_FIX_CYN_EXCR            = 0.30D0
        p%R_FIX                         = 1.0D0
        p%K_FIX                         = 0.008D0
        p%BETA_FIX_CYN                  = 0.0D0
        p%frac_avail_DON                = 0.0D0
    end subroutine set_default_fix_cyn_params

    subroutine set_default_opa_params(p)
        type(t_opa_params), intent(out) :: p
        p%KG_OPA_OPT_TEMP          = 2.9D0
        p%OPA_OPT_TEMP_LR          = 9.0D0
        p%OPA_OPT_TEMP_UR          = 20.0D0
        p%EFF_OPA_GROWTH           = 0.95D0
        p%KAPPA_OPA_UNDER_OPT_TEMP = 0.0D0
        p%KAPPA_OPA_OVER_OPT_TEMP  = 33.0D0
        p%KR_OPA_20                = 0.06D0
        p%THETA_KR_OPA             = 1.02D0
        p%KD_OPA_20                = 0.11D0
        p%THETA_KD_OPA             = 1.02D0
        p%KHS_DIN_OPA              = 0.015D0
        p%KHS_DIP_OPA              = 0.013D0
        p%KHS_O2_OPA               = 0.60D0
        p%FRAC_OPA_EXCR            = 0.30D0
        p%I_S_OPA                  = 100.0D0
        p%DO_STR_HYPOX_OPA_D       = 0.70D0
        p%THETA_HYPOX_OPA_D        = 1.20D0
        p%EXPON_HYPOX_OPA_D        = 1.04D0
        p%OPA_N_TO_C               = 0.220D0
        p%OPA_P_TO_C               = 0.024D0
        p%OPA_O2_TO_C              = 2.66D0
        p%OPA_C_TO_CHLA            = 30.0D0
        p%BETA_OPA                 = 0.0D0
    end subroutine set_default_opa_params

    subroutine set_default_nost_params(p)
        type(t_nost_params), intent(out) :: p
        p%KG_NOST_VEG_HET_OPT_TEMP          = 1.29D0
        p%FRAC_NOST_GROWTH                   = 0.10D0
        p%NOST_VEG_HET_OPT_TEMP_LR          = 16.0D0
        p%NOST_VEG_HET_OPT_TEMP_UR          = 26.0D0
        p%EFF_NOST_VEG_HET_GROWTH           = 0.95D0
        p%KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP = 0.0D0
        p%KAPPA_NOST_VEG_HET_OVER_OPT_TEMP  = 38.0D0
        p%KR_NOST_VEG_HET_20                = 0.06D0
        p%THETA_KR_NOST_VEG_HET             = 1.04D0
        p%KD_NOST_VEG_HET_20                = 0.040D0
        p%THETA_KD_NOST_VEG_HET             = 1.05D0
        p%KHS_DN_NOST_VEG_HET               = 0.0072D0
        p%KHS_DP_NOST_VEG_HET               = 0.005D0
        p%KHS_O2_NOST_VEG_HET               = 0.60D0
        p%I_S_NOST_VEG_HET                  = 100.0D0
        p%DO_STR_HYPOX_NOST_VEG_HET_D       = 0.70D0
        p%THETA_HYPOX_NOST_VEG_HET_D        = 1.50D0
        p%EXPON_HYPOX_NOST_VEG_HET_D        = 1.10D0
        p%NOST_C_TO_CHLA                    = 40.0D0
        p%FRAC_NOST_VEG_HET_EXCR            = 0.30D0
        p%KR_GERM_AKI                       = 0.3D0
        p%KN_GERM_AKI                       = 0.1D0
        p%KR_FORM_AKI                       = 0.1D0
        p%DAY_FORM_AKI                      = 200.0D0
        p%T_FORM_AKI                        = 16.0D0
        p%T_GERM_AKI                        = 21.0D0
        p%K_LOSS_AKI                        = 0.0D0
        p%K_MORT_AKI_20                     = 0.0D0
        p%THETA_K_MORT_AKI                  = 1.02D0
        p%KM_DENS_VEG_HET                   = 0.001D0
        p%BETA_NOST_VEG_HET                 = 0.0D0
    end subroutine set_default_nost_params

    subroutine setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DISS_OXYGEN, WINDS)
        type(t_phyto_env), intent(inout) :: env
        real(kind = DBL_PREC), target, intent(in) :: TEMP(:)
        real(kind = DBL_PREC), target, intent(in) :: I_A(:)
        real(kind = DBL_PREC), target, intent(in) :: K_E(:)
        real(kind = DBL_PREC), target, intent(in) :: DEPTH(:)
        real(kind = DBL_PREC), target, intent(in) :: CHLA(:)
        real(kind = DBL_PREC), target, intent(in) :: FDAY(:)
        real(kind = DBL_PREC), target, intent(in) :: DISS_OXYGEN(:)
        real(kind = DBL_PREC), target, intent(in), optional :: WINDS(:)

        env%TEMP        => TEMP
        env%I_A         => I_A
        env%K_E         => K_E
        env%DEPTH       => DEPTH
        env%CHLA        => CHLA
        env%FDAY        => FDAY
        env%DISS_OXYGEN => DISS_OXYGEN
        if (present(WINDS)) then
            env%WINDS => WINDS
        end if
    end subroutine setup_phyto_env

end module test_defaults
