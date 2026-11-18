
! Pelagic kinetic model ALUKAS_II
! Version with variables calculated in subroutines
! Version with dissolved inorganic carbon and alkalinity as
! state variables.

! Contains:
!
! module PELAGIC_MODEL_CONSTANTS
!
! subroutine INIT_PELAGIC_MODEL_CONSTANTS       !inits constants from array
! subroutine INSERT_PELAGIC_MODEL_CONSTANTS     !inserts constants into array
! subroutine DEFAULT_PELAGIC_MODEL_CONSTANTS    !sets default constants
! subroutine READ_PELAGIC_MODEL_CONSTANTS(file) !reads constants from file
! subroutine WRITE_PELAGIC_MODEL_CONSTANTS(file)!writes constants to file

!==========================================================================
module AQUABC_PELAGIC_MODEL_CONSTANTS
!==========================================================================

    use AQUABC_II_GLOBAL

    implicit none

    !Model constants
    real(kind = DBL_PREC) ::                               K_A !Model constant no   1 : Aeration coefficient (if negative calculates internally)
    real(kind = DBL_PREC) ::                         THETA_K_A !Model constant no   2 : Temperature correction factor for aeration
    real(kind = DBL_PREC) ::                               XKC !Model constant no   3 : Light extinction per chlorophyl unit,( mcg Chla/l/m)
    real(kind = DBL_PREC) ::                             PHIMX !Model constant no   4 : Quantum yield const. mg C/mole photon
    real(kind = DBL_PREC) ::                   KG_DIA_OPT_TEMP !Model constant no   5 : Diatoms Growth rate
    real(kind = DBL_PREC) ::                   DIA_OPT_TEMP_LR !Model constant no   6 : Diatoms CTMI minimum cardinal temperature (T_min)
    real(kind = DBL_PREC) ::                   DIA_OPT_TEMP_UR !Model constant no   7 : Diatoms CTMI optimal temperature (T_opt)
    real(kind = DBL_PREC) ::                    EFF_DIA_GROWTH !Model constant no   8 : Diatoms Effective growth. (1-EG)*growth - losses for respiration and excretion
    real(kind = DBL_PREC) ::          KAPPA_DIA_UNDER_OPT_TEMP !Model constant no   9 : (unused, kept for backward compat)
    real(kind = DBL_PREC) ::          KAPPA_DIA_OVER_OPT_TEMP  !Model constant no  10 : Diatoms CTMI maximum cardinal temperature (T_max)
    real(kind = DBL_PREC) ::                         KR_DIA_20 !Model constant no  11 : Diatoms Respiration rate
    real(kind = DBL_PREC) ::                      THETA_KR_DIA !Model constant no  12 : Diatoms Temperature correction for basal respiration rate
    real(kind = DBL_PREC) ::                         KD_DIA_20 !Model constant no  13 : Diatoms Mortality rate
    real(kind = DBL_PREC) ::                      THETA_KD_DIA !Model constant no  14 : Diatoms Temperature correction for Mortality rate
    real(kind = DBL_PREC) ::                       KHS_DIN_DIA !Model constant no  15 : Diatoms Half saturation growth for DIN
    real(kind = DBL_PREC) ::                       KHS_DIP_DIA !Model constant no  16 : Diatoms Half saturation growth for DIP
    real(kind = DBL_PREC) ::                       KHS_DSi_DIA !Model constant no  17 : Diatoms Half saturation growth for DSi
    real(kind = DBL_PREC) ::                        KHS_O2_DIA !Model constant no  18 : Diatoms Half saturation growth for O2
    real(kind = DBL_PREC) ::                     FRAC_DIA_EXCR !Model constant no  19 : Diatoms Fraction of excretion in metabolism rate
    real(kind = DBL_PREC) ::                           I_S_DIA !Model constant no  20 : Diatoms Light saturation (langleys)
    real(kind = DBL_PREC) ::                DO_STR_HYPOX_DIA_D !Model constant no  21 : Diatoms Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    real(kind = DBL_PREC) ::                 THETA_HYPOX_DIA_D !Model constant no  22 : Diatoms Multiplier of the exponent for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                 EXPON_HYPOX_DIA_D !Model constant no  23 : Diatoms Exponent constant for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                        DIA_N_TO_C !Model constant no  24 : Diatoms Nitrogen to Carbon ratio
    real(kind = DBL_PREC) ::                        DIA_P_TO_C !Model constant no  25 : Diatoms Phosphorus to Carbon ratio
    real(kind = DBL_PREC) ::                       DIA_Si_TO_C !Model constant no  26 : Diatoms Silica to Carbon ratio
    real(kind = DBL_PREC) ::                       DIA_O2_TO_C !Model constant no  27 : Diatoms Oxygen to Carbon ratio for respiration
    real(kind = DBL_PREC) ::                     DIA_C_TO_CHLA !Model constant no  28 : Diatoms Carbon to Chlorophil a ratio
    real(kind = DBL_PREC) ::                   KG_CYN_OPT_TEMP !Model constant no  29 : Non-fixing cyanobacteria Growth rate
    real(kind = DBL_PREC) ::                   CYN_OPT_TEMP_LR !Model constant no  30 : Non-fixing cyanobacteria CTMI minimum cardinal temperature (T_min)
    real(kind = DBL_PREC) ::                   CYN_OPT_TEMP_UR !Model constant no  31 : Non-fixing cyanobacteria CTMI optimal temperature (T_opt)
    real(kind = DBL_PREC) ::                    EFF_CYN_GROWTH !Model constant no  32 : Non-fixing cyanobacteria Effective growth. (1-EG)*growth - losses for respiration and excretion
    real(kind = DBL_PREC) ::          KAPPA_CYN_UNDER_OPT_TEMP !Model constant no  33 : (unused, kept for backward compat)
    real(kind = DBL_PREC) ::           KAPPA_CYN_OVER_OPT_TEMP !Model constant no  34 : Non-fixing cyanobacteria CTMI maximum cardinal temperature (T_max)
    real(kind = DBL_PREC) ::                         KR_CYN_20 !Model constant no  35 : Non-fixing cyanobacteria Respiration rate
    real(kind = DBL_PREC) ::                      THETA_KR_CYN !Model constant no  36 : Non-fixing cyanobacteria Temperature correction for respiration rate
    real(kind = DBL_PREC) ::                         KD_CYN_20 !Model constant no  37 : Non-fixing cyanobacteria Mortality rate
    real(kind = DBL_PREC) ::                      THETA_KD_CYN !Model constant no  38 : Non-fixing cyanobacteria Temperature correction for Mortality rate
    real(kind = DBL_PREC) ::                       KHS_DIN_CYN !Model constant no  39 : Non-fixing cyanobacteria Half saturation growth for DIN
    real(kind = DBL_PREC) ::                       KHS_DIP_CYN !Model constant no  40 : Non-fixing cyanobacteria Half saturation growth for DIP
    real(kind = DBL_PREC) ::                        KHS_O2_CYN !Model constant no  41 : Non-fixing cyanobacteria Half saturation growth for O2
    real(kind = DBL_PREC) ::                     FRAC_CYN_EXCR !Model constant no  42 : Non-fixing cyanobacteria Fraction of excretion in metabolism rate
    real(kind = DBL_PREC) ::                           I_S_CYN !Model constant no  43 : Non-fixing cyanobacteria Light saturation (langleys).Not used, is calculated!
    real(kind = DBL_PREC) ::                DO_STR_HYPOX_CYN_D !Model constant no  44 : Non-fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    real(kind = DBL_PREC) ::                 THETA_HYPOX_CYN_D !Model constant no  45 : Non-fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                 EXPON_HYPOX_CYN_D !Model constant no  46 : Non-fixing cyanobacteria Exponent constant for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                        CYN_N_TO_C !Model constant no  47 : Non-fixing cyanobacteria Nitrogen to Carbon ratio ,was 0.1
    real(kind = DBL_PREC) ::                        CYN_P_TO_C !Model constant no  48 : Non-fixing cyanobacteria Phosphorus to Carbon ratio
    real(kind = DBL_PREC) ::                       CYN_O2_TO_C !Model constant no  49 : Non-fixing cyanobacteria Oxygen to Carbon ratio for respiration
    real(kind = DBL_PREC) ::                     CYN_C_TO_CHLA !Model constant no  50 : Non-fixing cyanobacteria Carbon to Chlorophyl a ratio
    real(kind = DBL_PREC) ::               KG_FIX_CYN_OPT_TEMP !Model constant no  51 : Fixing cyanobacteria Growth rate constant
    real(kind = DBL_PREC) ::               FIX_CYN_OPT_TEMP_LR !Model constant no  52 : Fixing Cyanobacteria CTMI minimum cardinal temperature (T_min)
    real(kind = DBL_PREC) ::               FIX_CYN_OPT_TEMP_UR !Model constant no  53 : Fixing Cyanobacteria CTMI optimal temperature (T_opt)
    real(kind = DBL_PREC) ::                EFF_FIX_CYN_GROWTH !Model constant no  54 : Fixing cyanobacteria Effective growth. (1-EG)*growth - losses for RESP and excretion
    real(kind = DBL_PREC) ::      KAPPA_FIX_CYN_UNDER_OPT_TEMP !Model constant no  55 : (unused, kept for backward compat)
    real(kind = DBL_PREC) ::       KAPPA_FIX_CYN_OVER_OPT_TEMP !Model constant no  56 : Fixing cyanobacteria CTMI maximum cardinal temperature (T_max)
    real(kind = DBL_PREC) ::                     KR_FIX_CYN_20 !Model constant no  57 : Fixing cyanobacteria RESP rate constant
    real(kind = DBL_PREC) ::                  THETA_KR_FIX_CYN !Model constant no  58 : Fixing cyanobacteria Temperature correction for RESP rate
    real(kind = DBL_PREC) ::                     KD_FIX_CYN_20 !Model constant no  59 : Fixing cyanobacteria Mortality rate constant
    real(kind = DBL_PREC) ::                  THETA_KD_FIX_CYN !Model constant no  60 : Fixing cyanobacteria Temperature correction for Mortality rate
    real(kind = DBL_PREC) ::                   KHS_DIN_FIX_CYN !Model constant no  61 : Fixing cyanobacteria Half saturation growth for DIN
    real(kind = DBL_PREC) ::                   KHS_DIP_FIX_CYN !Model constant no  62 : Fixing cyanobacteria Half saturation growth for DIP
    real(kind = DBL_PREC) ::                    KHS_O2_FIX_CYN !Model constant no  63 : Fixing cyanobacteria Half saturation growth for O2
    real(kind = DBL_PREC) ::                 FRAC_FIX_CYN_EXCR !Model constant no  64 : Fixing cyanobacteria Fraction of excretion in metabolism rate
    real(kind = DBL_PREC) ::                       I_S_FIX_CYN !Model constant no  65 : Fixing cyanobacteria Light saturation (langleys). Not used, is calculated!
    real(kind = DBL_PREC) ::            DO_STR_HYPOX_FIX_CYN_D !Model constant no  66 : Fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    real(kind = DBL_PREC) ::             THETA_HYPOX_FIX_CYN_D !Model constant no  67 : Fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress
    real(kind = DBL_PREC) ::             EXPON_HYPOX_FIX_CYN_D !Model constant no  68 : Fixing cyanobacteria Exponent constant for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                    FIX_CYN_N_TO_C !Model constant no  69 : Fixing cyanobacteria Nitrogen to Carbon ratio
    real(kind = DBL_PREC) ::                    FIX_CYN_P_TO_C !Model constant no  70 : Fixing cyanobacteria Phosphorus to Carbon ratio
    real(kind = DBL_PREC) ::                   FIX_CYN_O2_TO_C !Model constant no  71 : Fixing cyanobacteria Oxygen to Carbon ratio for respiration
    real(kind = DBL_PREC) ::                 FIX_CYN_C_TO_CHLA !Model constant no  72 : Fixing cyanobacteria Carbon to Chlorophyl a ratio
    real(kind = DBL_PREC) ::                             R_FIX !Model constant no  73 : Fixing cyanobacteria Ratio between non-fixing and fixing fractions growth rate
    real(kind = DBL_PREC) ::                             K_FIX !Model constant no  74 : Fixing cyanobacteria Effectivity parameter of switching to nitrogen fixation
    real(kind = DBL_PREC) ::                   KG_OPA_OPT_TEMP !Model constant no  75 : OtherPhyto Growth rate constant
    real(kind = DBL_PREC) ::                   OPA_OPT_TEMP_LR !Model constant no  76 : OtherPhyto CTMI minimum cardinal temperature (T_min)
    real(kind = DBL_PREC) ::                   OPA_OPT_TEMP_UR !Model constant no  77 : OtherPhyto CTMI optimal temperature (T_opt)
    real(kind = DBL_PREC) ::                    EFF_OPA_GROWTH !Model constant no  78 : OtherPhyto Effective growth. (1-EG)*growth - losses for respiration and excretion
    real(kind = DBL_PREC) ::          KAPPA_OPA_UNDER_OPT_TEMP !Model constant no  79 : (unused, kept for backward compat)
    real(kind = DBL_PREC) ::           KAPPA_OPA_OVER_OPT_TEMP !Model constant no  80 : OtherPhyto CTMI maximum cardinal temperature (T_max)
    real(kind = DBL_PREC) ::                         KR_OPA_20 !Model constant no  81 : OtherPhyto Respiration rate constant
    real(kind = DBL_PREC) ::                      THETA_KR_OPA !Model constant no  82 : OtherPhyto Temperature correction for respiration rate
    real(kind = DBL_PREC) ::                         KD_OPA_20 !Model constant no  83 : OtherPhyto Mortality rate constant
    real(kind = DBL_PREC) ::                      THETA_KD_OPA !Model constant no  84 : OtherPhyto Temperature correction for Mortality rate
    real(kind = DBL_PREC) ::                       KHS_DIN_OPA !Model constant no  85 : OtherPhyto Half saturation growth for DIN
    real(kind = DBL_PREC) ::                       KHS_DIP_OPA !Model constant no  86 : OtherPhyto Half saturation growth for DIP
    real(kind = DBL_PREC) ::                        KHS_O2_OPA !Model constant no  87 : OtherPhyto Half saturation growth for O2
    real(kind = DBL_PREC) ::                     FRAC_OPA_EXCR !Model constant no  88 : OtherPhyto Fraction of excretion in metabolism rate
    real(kind = DBL_PREC) ::                           I_S_OPA !Model constant no  89 : OtherPhyto Light saturation (langleys). Not used, is calculated!
    real(kind = DBL_PREC) ::                DO_STR_HYPOX_OPA_D !Model constant no  90 : OtherPhyto Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    real(kind = DBL_PREC) ::                 THETA_HYPOX_OPA_D !Model constant no  91 : OtherPhyto Multiplier of the exponent for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                 EXPON_HYPOX_OPA_D !Model constant no  92 : OtherPhyto Exponent constant for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                        OPA_N_TO_C !Model constant no  93 : OtherPhyto Nitrogen to Carbon ratio
    real(kind = DBL_PREC) ::                        OPA_P_TO_C !Model constant no  94 : OtherPhyto Phosphorus to Carbon ratio
    real(kind = DBL_PREC) ::                       OPA_O2_TO_C !Model constant no  95 : OtherPhyto Oxygen to Carbon ratio for respiration
    real(kind = DBL_PREC) ::                     OPA_C_TO_CHLA !Model constant no  96 : OtherPhyto Carbon to Chlorophyl a ratio
    real(kind = DBL_PREC) ::                   KG_ZOO_OPT_TEMP !Model constant no  97 : Zooplankton Growth rate
    real(kind = DBL_PREC) ::                   ZOO_OPT_TEMP_LR !Model constant no  98 : Zooplankton CTMI minimum cardinal temperature (T_min)
    real(kind = DBL_PREC) ::                   ZOO_OPT_TEMP_UR !Model constant no  99 : Zooplankton CTMI optimal temperature (T_opt)
    real(kind = DBL_PREC) ::                    EFF_ZOO_GROWTH !Model constant no 100 : Zooplankton Effective growth. (1-EG)*growth - losses for respiration and excretion
    real(kind = DBL_PREC) ::          KAPPA_ZOO_UNDER_OPT_TEMP !Model constant no 101 : (unused, kept for backward compat)
    real(kind = DBL_PREC) ::           KAPPA_ZOO_OVER_OPT_TEMP !Model constant no 102 : Zooplankton CTMI maximum cardinal temperature (T_max)
    real(kind = DBL_PREC) ::                      GRAT_ZOO_DIA !Model constant no 103 : Zooplankton Grazing rate (growhth rate multiplier) on diatoms
    real(kind = DBL_PREC) ::                      GRAT_ZOO_CYN !Model constant no 104 : Zooplankton Grazing rate (growhth rate multiplier) on Cyanobacteria
    real(kind = DBL_PREC) ::                      GRAT_ZOO_OPA !Model constant no 105 : Zooplankton Grazing rate (growhth rate multiplier) on OtherPhyto
    real(kind = DBL_PREC) ::                  GRAT_ZOO_FIX_CYN !Model constant no 106 : Zooplankton Grazing rate (growhth rate multiplier) on fixing Cyanobacteria
    real(kind = DBL_PREC) ::             GRAT_ZOO_NOST_VEG_HET !Model constant no 107 : Zooplankton Grazing rate (growhth rate multiplier) on Nostocles (veg + het)
    real(kind = DBL_PREC) ::           GRAT_ZOO_DET_PART_ORG_C !Model constant no 108 : Zooplankton Grazing rate (growhth rate multiplier) on part. ORG_C
    real(kind = DBL_PREC) ::                      PREF_ZOO_DIA !Model constant no 109 : Zooplankton Preference for Diatoms
    real(kind = DBL_PREC) ::                      PREF_ZOO_CYN !Model constant no 110 : Zooplankton Preference for Cyanobacteria
    real(kind = DBL_PREC) ::                  PREF_ZOO_FIX_CYN !Model constant no 111 : Zooplankton Preference for fixing Cyanobacteria
    real(kind = DBL_PREC) ::             PREF_ZOO_NOST_VEG_HET !Model constant no 112 : Zooplankton Preference for nostocales (veg + het)
    real(kind = DBL_PREC) ::                      PREF_ZOO_OPA !Model constant no 113 : Zooplankton Preference for OtherPhyto
    real(kind = DBL_PREC) ::           PREF_ZOO_DET_PART_ORG_C !Model constant no 114 : Zooplankton Preference for Part. ORG_C
    real(kind = DBL_PREC) ::                     KHS_DIA_C_ZOO !Model constant no 115 : Zooplankton Half saturation growth for diatoms
    real(kind = DBL_PREC) ::                     KHS_CYN_C_ZOO !Model constant no 116 : Zooplankton Half saturation growth for Cyanobacteria
    real(kind = DBL_PREC) ::                 KHS_FIX_CYN_C_ZOO !Model constant no 117 : Zooplankton Half saturation growth for fixing Cyanobacteria
    real(kind = DBL_PREC) ::            KHS_NOST_VEG_HET_C_ZOO !Model constant no 118 : Zooplankton Half saturation growth for Nostocales (veg + het)
    real(kind = DBL_PREC) ::                     KHS_OPA_C_ZOO !Model constant no 119 : Zooplankton Half saturation growth for OtherPhyto
    real(kind = DBL_PREC) ::            KHS_DET_PART_ORG_C_ZOO !Model constant no 120 : Zooplankton Half saturation growth for part. ORG_C
    real(kind = DBL_PREC) ::                      FOOD_MIN_ZOO !Model constant no 121 : Zooplankton Minimum food conc. for feeding
    real(kind = DBL_PREC) ::                            KE_ZOO !Model constant no 122 : not used Zooplankton Excretion rate as growth fraction
    real(kind = DBL_PREC) ::                   FRAC_ZOO_EX_ORG !Model constant no 123 : not used Zooplankton Excretion rate organic fraction
    real(kind = DBL_PREC) ::                         KR_ZOO_20 !Model constant no 124 : Zooplankton Respiration rate
    real(kind = DBL_PREC) ::                      THETA_KR_ZOO !Model constant no 125 : Zooplankton Respiration rate Temperature correction
    real(kind = DBL_PREC) ::                         KD_ZOO_20 !Model constant no 126 : Zooplankton Mortality rate
    real(kind = DBL_PREC) ::                      THETA_KD_ZOO !Model constant no 127 : Zooplankton Mortality rate Temperature correction
    real(kind = DBL_PREC) ::                DO_STR_HYPOX_ZOO_D !Model constant no 128 : Zooplankton Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    real(kind = DBL_PREC) ::                 THETA_HYPOX_ZOO_D !Model constant no 129 : Zooplankton Multiplier of the exponent for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                 EXPON_HYPOX_ZOO_D !Model constant no 130 : Zooplankton Exponent constant for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                        ZOO_N_TO_C !Model constant no 131 : Zooplankton Nitrogen to Carbon ratio
    real(kind = DBL_PREC) ::                        ZOO_P_TO_C !Model constant no 132 : Zooplankton Phosphorus to Carbon ratio
    real(kind = DBL_PREC) ::                       ZOO_O2_TO_C !Model constant no 133 : Zooplankton Oxygen to Carbon ratio for respiration
    real(kind = DBL_PREC) ::           KDISS_DET_PART_ORG_C_20 !Model constant no 134 : POC Dissolution rate not dependent on phytoplankton
    real(kind = DBL_PREC) ::        THETA_KDISS_DET_PART_ORG_C !Model constant no 135 : POC Carbon Dissolution rate Temperature correction
    real(kind = DBL_PREC) ::           FAC_PHYT_DET_PART_ORG_C !Model constant no 136 : POC Carbon Phytoplankton linear factor for dissolution rate
    real(kind = DBL_PREC) ::           KDISS_DET_PART_ORG_N_20 !Model constant no 137 : PON Dissolution rate not dependent on phytoplankton
    real(kind = DBL_PREC) ::        THETA_KDISS_DET_PART_ORG_N !Model constant no 138 : PON Dissolution rate Temperature correction
    real(kind = DBL_PREC) ::                        KHS_DISS_N !Model constant no 139 : PON dissolution reverse half saturation for DIN
    real(kind = DBL_PREC) ::           FAC_PHYT_DET_PART_ORG_N !Model constant no 140 : PON Phytoplankton linear factor for dissolution rate
    real(kind = DBL_PREC) ::           KDISS_DET_PART_ORG_P_20 !Model constant no 141 : POP Phosphorus Dissolution rate not dependent on phytoplankton
    real(kind = DBL_PREC) ::        THETA_KDISS_DET_PART_ORG_P !Model constant no 142 : POP Dissolution rate Temperature correction
    real(kind = DBL_PREC) ::                        KHS_DISS_P !Model constant no 143 : POP Dissolution reverse half saturation for DIP
    real(kind = DBL_PREC) ::           FAC_PHYT_DET_PART_ORG_P !Model constant no 144 : POP Phytoplankton linear factor for dissolution rate
    real(kind = DBL_PREC) ::                  KDISS_PART_Si_20 !Model constant no 145 : Particulate Silica Dissolution rate
    real(kind = DBL_PREC) ::               THETA_KDISS_PART_Si !Model constant no 146 : Particulate Silica Dissolution rate Temperature correction
    real(kind = DBL_PREC) ::                 FAC_PHYT_AMIN_DOC !Model constant no 147 : DOC  Phytoplankton linear factor for mineralisation rate
    real(kind = DBL_PREC) ::                        KHS_AMIN_N !Model constant no 148 : DON  reverse half saturation for DIN
    real(kind = DBL_PREC) ::                 FAC_PHYT_AMIN_DON !Model constant no 149 : DON Phytoplankton linear factor for mineralisation rate
    real(kind = DBL_PREC) ::                        KHS_AMIN_P !Model constant no 150 : DOP reverse half saturation for DIP
    real(kind = DBL_PREC) ::                 FAC_PHYT_AMIN_DOP !Model constant no 151 : DOP Phytoplankton linear factor for mineralisation rate
    real(kind = DBL_PREC) ::                         K_NITR_20 !Model constant no 152 : Amonia nitrification rate
    real(kind = DBL_PREC) ::                      THETA_K_NITR !Model constant no 153 : Amonia nitrification rate Temperature constant
    real(kind = DBL_PREC) ::                      KHS_NITR_OXY !Model constant no 154 : Amonia nitrification half saturation for Oxygen
    real(kind = DBL_PREC) ::                    KHS_NITR_NH4_N !Model constant no 155 : Amonia nitrification half saturation for Amonia
    real(kind = DBL_PREC) ::                   PH_NITR_NH4_MIN !Model constant no 156 : optimum lower range for pH correction factor for nitrification
    real(kind = DBL_PREC) ::                   PH_NITR_NH4_MAX !Model constant no 157 : optimum upper range for pH correction factor for nitrification
    real(kind = DBL_PREC) ::                        k_OX_FE_II !Model constant no 158 : Oxidation rate for iron 2+
    real(kind = DBL_PREC) ::                      k_RED_FE_III !Model constant no 159 : reduction rate for iron 3+
    real(kind = DBL_PREC) ::                        k_OX_MN_II !Model constant no 160 : oxidation rate for manganese 2+
    real(kind = DBL_PREC) ::                       k_RED_MN_IV !Model constant no 161 : reduction rate for manganese 4+
    real(kind = DBL_PREC) ::               KHS_DOXY_FE_III_RED !Model constant no 162 : reversed Monod half saturation of DOXY for iron 3+ reduction
    real(kind = DBL_PREC) ::                KHS_DOXY_MN_IV_RED !Model constant no 163 : reversed Monod half saturation of DOXY for manganese 4+ reduction
    real(kind = DBL_PREC) ::                 K_MIN_DOC_DOXY_20 !Model constant no 164 : Mineralization rate constant of DOC at 20 C for dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::                 K_MIN_DOC_NO3N_20 !Model constant no 165 : Mineralization rate constant of DOC at 20 C for nitrate as final electron acceptor
    real(kind = DBL_PREC) ::                K_MIN_DOC_MN_IV_20 !Model constant no 166 : Mineralization rate constant of DOC at 20 C for Mn IV as final electron acceptor
    real(kind = DBL_PREC) ::               K_MIN_DOC_FE_III_20 !Model constant no 167 : Mineralization rate constant of DOC at 20 C for Fe III as final electron acceptor
    real(kind = DBL_PREC) ::             K_MIN_DOC_S_PLUS_6_20 !Model constant no 168 : Mineralization rate constant of DOC at 20 C for sulphate as final electron acceptor
    real(kind = DBL_PREC) ::                  K_MIN_DOC_DOC_20 !Model constant no 169 : Mineralization rate constant of DOC at 20 C for DOC itself as final electron acceptor
    real(kind = DBL_PREC) ::              THETA_K_MIN_DOC_DOXY !Model constant no 170 : Temperature correction factor for mineralization process rate constant of DOC for dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::              THETA_K_MIN_DOC_NO3N !Model constant no 171 : Temperature correction factor for mineralization process rate constant of DOC for nitrate as final electron acceptor
    real(kind = DBL_PREC) ::             THETA_K_MIN_DOC_MN_IV !Model constant no 172 : Temperature correction factor for mineralization process rate constant of DOC for MN IV as final electron acceptor
    real(kind = DBL_PREC) ::            THETA_K_MIN_DOC_FE_III !Model constant no 173 : Temperature correction factor for mineralization process rate constant of DOC for FE III as final electron acceptor
    real(kind = DBL_PREC) ::          THETA_K_MIN_DOC_S_PLUS_6 !Model constant no 174 : Temperature correction factor for mineralization process rate constant of DOC for Suphate as final electron acceptor
    real(kind = DBL_PREC) ::               THETA_K_MIN_DOC_DOC !Model constant no 175 : Temperature correction factor for mineralization process rate constant of DOC for DOC
    real(kind = DBL_PREC) ::                 K_HS_DOC_MIN_DOXY !Model constant no 176 : Monod type half-saturation concentration of DOC for DOC mineralization for dissolved oxygen as the final electron acceptor
    real(kind = DBL_PREC) ::                 K_HS_DOC_MIN_NO3N !Model constant no 177 : Monod type half-saturation concentration of DOC for DOC mineralization for nitrate as the final electron acceptor
    real(kind = DBL_PREC) ::                K_HS_DOC_MIN_MN_IV !Model constant no 178 : Monod type half-saturation concentration of DOC for DOC mineralization for Mn IV as the final electron acceptor
    real(kind = DBL_PREC) ::               K_HS_DOC_MIN_FE_III !Model constant no 179 : Monod type half-saturation concentration of DOC for DOC mineralization for Fe III as the final electron acceptor
    real(kind = DBL_PREC) ::             K_HS_DOC_MIN_S_PLUS_6 !Model constant no 180 : Monod type half-saturation concentration of DOC for DOC mineralization for sulphate as the final electron acceptor
    real(kind = DBL_PREC) ::                  K_HS_DOC_MIN_DOC !Model constant no 181 : Monod type half-saturation concentration of DOC for DOC mineralization for DOC itself as the final electron acceptor
    real(kind = DBL_PREC) ::                 K_HS_DOXY_RED_LIM !Model constant no 182 : Monod type half-saturation concentration of dissolved oxygen to limit the consumption of dissolved oxygen as the final electron acce
    real(kind = DBL_PREC) ::                 K_HS_NO3N_RED_LIM !Model constant no 183 : Monod type half-saturation concentration of nitrate nitrogen to limit the consumption of nitrate as the final electron acceptor for
    real(kind = DBL_PREC) ::                K_HS_MN_IV_RED_LIM !Model constant no 184 : Monod type half-saturation concentration of MN_IV to limit the consumption of Mn IV as the final electron acceptor for DOC mineraliz
    real(kind = DBL_PREC) ::               K_HS_FE_III_RED_LIM !Model constant no 185 : Monod type half-saturation concentration of FE_III to limit the consumption of Fe III as the final electron acceptor for DOC mineral
    real(kind = DBL_PREC) ::             K_HS_S_PLUS_6_RED_LIM !Model constant no 186 : Monod type half-saturation concentration of sulphate to limit the consumption of sulphate as the final electron acceptor for DOC min
    real(kind = DBL_PREC) ::                K_HS_DOXY_RED_INHB !Model constant no 187 : Reversed Monod type half saturation concentration that simulate the inhibition effect of dissolved oxygen on consumption of NO3, Mn
    real(kind = DBL_PREC) ::                K_HS_NO3N_RED_INHB !Model constant no 188 : Reversed Monod type half saturation concentration that simulate the inhibition effect of nitrate on consumption of Mn IV, Fe III, S
    real(kind = DBL_PREC) ::               K_HS_MN_IV_RED_INHB !Model constant no 189 : Reversed Monod type half saturation concentration that simulate the inhibition effect of Mn IV on consumption of Fe III, S VI and DO
    real(kind = DBL_PREC) ::              K_HS_FE_III_RED_INHB !Model constant no 190 : Reversed Monod type half saturation concentration that simulate the inhibition effect of Fe III on consumption of S VI and DOC itsel
    real(kind = DBL_PREC) ::            K_HS_S_PLUS_6_RED_INHB !Model constant no 191 : Reversed Monod type half saturation concentration that simulate the inhibition effect of S VI on consumption of DOC itself for DOC m
    real(kind = DBL_PREC) ::               PH_MIN_DOC_MIN_DOXY !Model constant no 192 : Min. pH for the optimum pH range for DOC mineralization with DOXY as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MIN_DOC_MIN_NO3N !Model constant no 193 : Min. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor
    real(kind = DBL_PREC) ::              PH_MIN_DOC_MIN_MN_IV !Model constant no 194 : Min. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor
    real(kind = DBL_PREC) ::             PH_MIN_DOC_MIN_FE_III !Model constant no 195 : Min. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor
    real(kind = DBL_PREC) ::           PH_MIN_DOC_MIN_S_PLUS_6 !Model constant no 196 : Min. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor
    real(kind = DBL_PREC) ::                PH_MIN_DOC_MIN_DOC !Model constant no 197 : Min. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MAX_DOC_MIN_DOXY !Model constant no 198 : Max. pH for the optimum pH range for DOC mineralization with dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MAX_DOC_MIN_NO3N !Model constant no 199 : Max. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor
    real(kind = DBL_PREC) ::              PH_MAX_DOC_MIN_MN_IV !Model constant no 200 : Max. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor
    real(kind = DBL_PREC) ::             PH_MAX_DOC_MIN_FE_III !Model constant no 201 : Max. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor
    real(kind = DBL_PREC) ::           PH_MAX_DOC_MIN_S_PLUS_6 !Model constant no 202 : Max. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor
    real(kind = DBL_PREC) ::                PH_MAX_DOC_MIN_DOC !Model constant no 203 : Max. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor
    real(kind = DBL_PREC) ::                 K_MIN_DON_DOXY_20 !Model constant no 204 : Mineralization rate constant of DON at 20 C for dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::                 K_MIN_DON_NO3N_20 !Model constant no 205 : Mineralization rate constant of DON at 20 C for NO3_N as final electron acceptor
    real(kind = DBL_PREC) ::                K_MIN_DON_MN_IV_20 !Model constant no 206 : Mineralization rate constant of DON at 20 C for Mn IV as final electron acceptor
    real(kind = DBL_PREC) ::               K_MIN_DON_FE_III_20 !Model constant no 207 : Mineralization rate constant of DON at 20 C for FE III as final electron acceptor
    real(kind = DBL_PREC) ::             K_MIN_DON_S_PLUS_6_20 !Model constant no 208 : Mineralization rate constant of DON at 20 C for S_PLUS_6 as final electron acceptor
    real(kind = DBL_PREC) ::                  K_MIN_DON_DOC_20 !Model constant no 209 : Mineralization rate constant of DON at 20 C for DOC as final electron acceptor
    real(kind = DBL_PREC) ::              THETA_K_MIN_DON_DOXY !Model constant no 210 : Temperature correction factor for mineralization process rate constant of DON for dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::              THETA_K_MIN_DON_NO3N !Model constant no 211 : Temperature correction factor for mineralization process rate constant of DON for nitrate as final electron acceptor
    real(kind = DBL_PREC) ::             THETA_K_MIN_DON_MN_IV !Model constant no 212 : Temperature correction factor for mineralization process rate constant of DON for MN_IV as final electron acceptor
    real(kind = DBL_PREC) ::            THETA_K_MIN_DON_FE_III !Model constant no 213 : Temperature correction factor for mineralization process rate constant of DON for FE_III as final electron acceptor
    real(kind = DBL_PREC) ::          THETA_K_MIN_DON_S_PLUS_6 !Model constant no 214 : Temperature correction factor for mineralization process rate constant of DON for sulphate as final electron acceptor
    real(kind = DBL_PREC) ::               THETA_K_MIN_DON_DOC !Model constant no 215 : Temperature correction factor for mineralization process rate constant of DON for DOC
    real(kind = DBL_PREC) ::                 K_HS_DON_MIN_DOXY !Model constant no 216 : Monod type half-saturation concentration of DON for DON mineralization for dissolved oxygen as the final electron acceptor
    real(kind = DBL_PREC) ::                 K_HS_DON_MIN_NO3N !Model constant no 217 : Monod type half-saturation concentration of DON for DON mineralization for nitrate nitrogen as the final electron acceptor
    real(kind = DBL_PREC) ::                K_HS_DON_MIN_MN_IV !Model constant no 218 : Monod type half-saturation concentration of DON for DON mineralization for MN_IV as the final electron acceptor
    real(kind = DBL_PREC) ::               K_HS_DON_MIN_FE_III !Model constant no 219 : Monod type half-saturation concentration of DON for DON mineralization for FE_III as the final electron acceptor
    real(kind = DBL_PREC) ::             K_HS_DON_MIN_S_PLUS_6 !Model constant no 220 : Monod type half-saturation concentration of DON for DON mineralization for sulphate sulphur as the final electron acceptor
    real(kind = DBL_PREC) ::                  K_HS_DON_MIN_DOC !Model constant no 221 : Monod type half-saturation concentration of DON for DON mineralization for DOC as the final electron acceptor
    real(kind = DBL_PREC) ::               PH_MIN_DON_MIN_DOXY !Model constant no 222 : Min. pH for the optimum pH range for DON mineralization with DOXY as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MIN_DON_MIN_NO3N !Model constant no 223 : Min. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor
    real(kind = DBL_PREC) ::              PH_MIN_DON_MIN_MN_IV !Model constant no 224 : Min. pH for the optimum pH range for DON mineralization with MN IV as final electron acceptor
    real(kind = DBL_PREC) ::             PH_MIN_DON_MIN_FE_III !Model constant no 225 : Min. pH for the optimum pH range for DON mineralization with FE III as final electron acceptor
    real(kind = DBL_PREC) ::           PH_MIN_DON_MIN_S_PLUS_6 !Model constant no 226 : Min. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor
    real(kind = DBL_PREC) ::                PH_MIN_DON_MIN_DOC !Model constant no 227 : Min. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MAX_DON_MIN_DOXY !Model constant no 228 : Max. pH for the optimum pH range for DON mineralization with dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MAX_DON_MIN_NO3N !Model constant no 229 : Max. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor
    real(kind = DBL_PREC) ::              PH_MAX_DON_MIN_MN_IV !Model constant no 230 : Max. pH for the optimum pH range for DON mineralization with Mn IV as final electron acceptor
    real(kind = DBL_PREC) ::             PH_MAX_DON_MIN_FE_III !Model constant no 231 : Max. pH for the optimum pH range for DON mineralization with FE_III as final electron acceptor
    real(kind = DBL_PREC) ::           PH_MAX_DON_MIN_S_PLUS_6 !Model constant no 232 : Max. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor
    real(kind = DBL_PREC) ::                PH_MAX_DON_MIN_DOC !Model constant no 233 : Max. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor
    real(kind = DBL_PREC) ::                 K_MIN_DOP_DOXY_20 !Model constant no 234 : Mineralization rate constant of DOP at 20 C for dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::                 K_MIN_DOP_NO3N_20 !Model constant no 235 : Mineralization rate constant of DOP at 20 C for NO3_N as final electron acceptor
    real(kind = DBL_PREC) ::                K_MIN_DOP_MN_IV_20 !Model constant no 236 : Mineralization rate constant of DOP at 20 C for Mn IV as final electron acceptor
    real(kind = DBL_PREC) ::               K_MIN_DOP_FE_III_20 !Model constant no 237 : Mineralization rate constant of DOP at 20 C for FE_III as final electron acceptor
    real(kind = DBL_PREC) ::             K_MIN_DOP_S_PLUS_6_20 !Model constant no 238 : Mineralization rate constant of DOP at 20 C for S_PLUS_6 as final electron acceptor
    real(kind = DBL_PREC) ::                  K_MIN_DOP_DOC_20 !Model constant no 239 : Mineralization rate constant of DOP at 20 C for DOC as final electron acceptor
    real(kind = DBL_PREC) ::              THETA_K_MIN_DOP_DOXY !Model constant no 240 : Temperature correction factor for mineralization process rate constant of DOP for dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::              THETA_K_MIN_DOP_NO3N !Model constant no 241 : Temperature correction factor for mineralization process rate constant of DOP for nitrate as final electron acceptor
    real(kind = DBL_PREC) ::             THETA_K_MIN_DOP_MN_IV !Model constant no 242 : Temperature correction factor for mineralization process rate constant of DOP for Mn IV as final electron acceptor
    real(kind = DBL_PREC) ::            THETA_K_MIN_DOP_FE_III !Model constant no 243 : Temperature correction factor for mineralization process rate constant of DOP for FE_III as final electron acceptor
    real(kind = DBL_PREC) ::          THETA_K_MIN_DOP_S_PLUS_6 !Model constant no 244 : Temperature correction factor for mineralization process rate constant of DOP for sulphate as final electron acceptor
    real(kind = DBL_PREC) ::               THETA_K_MIN_DOP_DOC !Model constant no 245 : Temperature correction factor for mineralization process rate constant of DOP for DOC
    real(kind = DBL_PREC) ::                 K_HS_DOP_MIN_DOXY !Model constant no 246 : Monod type half-saturation concentration of DOP for DOP mineralization for dissolved oxygen as the final electron acceptor
    real(kind = DBL_PREC) ::                 K_HS_DOP_MIN_NO3N !Model constant no 247 : Monod type half-saturation concentration of DOP for DOP mineralization for nitrate nitrogen as the final electron acceptor
    real(kind = DBL_PREC) ::                K_HS_DOP_MIN_MN_IV !Model constant no 248 : Monod type half-saturation concentration of DOP for DOP mineralization for MN_IV as the final electron acceptor
    real(kind = DBL_PREC) ::               K_HS_DOP_MIN_FE_III !Model constant no 249 : Monod type half-saturation concentration of DOP for DOP mineralization for FE_III as the final electron acceptor
    real(kind = DBL_PREC) ::             K_HS_DOP_MIN_S_PLUS_6 !Model constant no 250 : Monod type half-saturation concentration of DOP for DOP mineralization for sulphate sulphur as the final electron acceptor
    real(kind = DBL_PREC) ::                  K_HS_DOP_MIN_DOC !Model constant no 251 : Monod type half-saturation concentration of DOP for DOP mineralization for DOC as the final electron acceptor
    real(kind = DBL_PREC) ::               PH_MIN_DOP_MIN_DOXY !Model constant no 252 : Min. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MIN_DOP_MIN_NO3N !Model constant no 253 : Min. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor
    real(kind = DBL_PREC) ::              PH_MIN_DOP_MIN_MN_IV !Model constant no 254 : Min. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor
    real(kind = DBL_PREC) ::             PH_MIN_DOP_MIN_FE_III !Model constant no 255 : Min. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor
    real(kind = DBL_PREC) ::           PH_MIN_DOP_MIN_S_PLUS_6 !Model constant no 256 : Min. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor
    real(kind = DBL_PREC) ::                PH_MIN_DOP_MIN_DOC !Model constant no 257 : Min. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MAX_DOP_MIN_DOXY !Model constant no 258 : Max. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor
    real(kind = DBL_PREC) ::               PH_MAX_DOP_MIN_NO3N !Model constant no 259 : Max. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor
    real(kind = DBL_PREC) ::              PH_MAX_DOP_MIN_MN_IV !Model constant no 260 : Max. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor
    real(kind = DBL_PREC) ::             PH_MAX_DOP_MIN_FE_III !Model constant no 261 : Max. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor
    real(kind = DBL_PREC) ::           PH_MAX_DOP_MIN_S_PLUS_6 !Model constant no 262 : Max. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor
    real(kind = DBL_PREC) ::                PH_MAX_DOP_MIN_DOC !Model constant no 263 : Max. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor
    real(kind = DBL_PREC) ::                          k_OX_CH4 !Model constant no 264 : Methane oxidation rate constant at 20 C
    real(kind = DBL_PREC) ::                    THETA_k_OX_CH4 !Model constant no 265 : Arhenius type temperature correction for methane oxidation rate constant
    real(kind = DBL_PREC) ::                  k_HS_OX_CH4_DOXY !Model constant no 266 : Monod type Half saturation of methane oxidation for dissolved oxygen
    real(kind = DBL_PREC) ::                          k_OX_H2S !Model constant no 267 : Hydrogen sulphide oxidation rate constant at 20 C
    real(kind = DBL_PREC) ::                    THETA_k_OX_H2S !Model constant no 268 : Arhenius type temperature correction for sulphide oxidation rate constant
    real(kind = DBL_PREC) ::                  k_HS_OX_H2S_DOXY !Model constant no 269 : Monod type Half saturation of hydrogen sulphide oxidation for dissolved oxygen
    real(kind = DBL_PREC) ::                   k_DISS_FE_II_20 !Model constant no 270 : Dissolution rate constant for Fe II at 20 C
    real(kind = DBL_PREC) ::                THETA_k_DISS_FE_II !Model constant no 271 : Dissolution rate constant for Fe II
    real(kind = DBL_PREC) ::              INIT_MULT_FE_II_DISS !Model constant no 272 : Initial fraction of dissolved Fe II
    real(kind = DBL_PREC) ::                  k_DISS_FE_III_20 !Model constant no 273 : Dissolution rate constant for Fe III at 20 C
    real(kind = DBL_PREC) ::               THETA_k_DISS_FE_III !Model constant no 274 : Dissolution rate constant for Fe III
    real(kind = DBL_PREC) ::             INIT_MULT_FE_III_DISS !Model constant no 275 : Initial fraction of dissolved Fe III                                                                                                                               K_A
    real(kind = DBL_PREC) ::          KG_NOST_VEG_HET_OPT_TEMP !Model constant no 276 : Nostocales (veg + het) Growth rate constant
    real(kind = DBL_PREC) ::          NOST_VEG_HET_OPT_TEMP_LR !Model constant no 277 : Nostocales (veg + het) CTMI minimum cardinal temperature (T_min)
    real(kind = DBL_PREC) ::          NOST_VEG_HET_OPT_TEMP_UR !Model constant no 278 : Nostocales (veg + het) CTMI optimal temperature (T_opt)
    real(kind = DBL_PREC) ::           EFF_NOST_VEG_HET_GROWTH !Model constant no 279 : Nostocales (veg + het) Effective growth. (1-EG)*growth - losses for RESP and excretion
    real(kind = DBL_PREC) :: KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP !Model constant no 280 : (unused, kept for backward compat)
    real(kind = DBL_PREC) ::  KAPPA_NOST_VEG_HET_OVER_OPT_TEMP !Model constant no 281 : Nostocales (veg + het) CTMI maximum cardinal temperature (T_max)
    real(kind = DBL_PREC) ::                KR_NOST_VEG_HET_20 !Model constant no 282 : Nostocales (veg + het) RESP rate constant
    real(kind = DBL_PREC) ::             THETA_KR_NOST_VEG_HET !Model constant no 283 : Nostocales (veg + het) Temperature correction for RESP rate
    real(kind = DBL_PREC) ::                KD_NOST_VEG_HET_20 !Model constant no 284 : Nostocales (veg + het) Mortality rate constant
    real(kind = DBL_PREC) ::             THETA_KD_NOST_VEG_HET !Model constant no 285 : Nostocales (veg + het) Temperature correction for Mortality rate
    real(kind = DBL_PREC) ::                    M_DENS_VEG_HET !Model constant no 286 : Nostocales (veg + het) mortality rate constant because of too dense population
    real(kind = DBL_PREC) ::               KHS_DP_NOST_VEG_HET !Model constant no 287 : Nostocales (veg + het) Half saturation growth for DP
    real(kind = DBL_PREC) ::               KHS_O2_NOST_VEG_HET !Model constant no 288 : Nostocales (veg + het) Half saturation growth for O2
    real(kind = DBL_PREC) ::            FRAC_NOST_VEG_HET_EXCR !Model constant no 289 : Nostocales (veg + het) Fraction of excretion in metabolism rate
    real(kind = DBL_PREC) ::                  I_S_NOST_VEG_HET !Model constant no 290 : Nostocales (veg + het) Light saturation (langleys). Not used, is calculated!
    real(kind = DBL_PREC) ::       DO_STR_HYPOX_NOST_VEG_HET_D !Model constant no 291 : Nostocales (veg + het) Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    real(kind = DBL_PREC) ::        THETA_HYPOX_NOST_VEG_HET_D !Model constant no 292 : Nostocales (veg + het) Multiplier of the exponent for Dissolved oxygen stress
    real(kind = DBL_PREC) ::        EXPON_HYPOX_NOST_VEG_HET_D !Model constant no 293 : Nostocales (veg + het) Exponent constant for Dissolved oxygen stress
    real(kind = DBL_PREC) ::                       NOST_N_TO_C !Model constant no 294 : Nostocales (veg + het) Nitrogen to Carbon ratio
    real(kind = DBL_PREC) ::                       NOST_P_TO_C !Model constant no 295 : Nostocales (veg + het) Phosphorus to Carbon ratio
    real(kind = DBL_PREC) ::                      NOST_O2_TO_C !Model constant no 296 : Nostocales (veg + het) Oxygen to Carbon ratio for respiration
    real(kind = DBL_PREC) ::                    NOST_C_TO_CHLA !Model constant no 297 : Nostocales (veg + het) Carbon to Chlorophyl a ratio
    real(kind = DBL_PREC) ::                        P_GERM_AKI !Model constant no 298 : Gerimination rate constant for akinets
    real(kind = DBL_PREC) ::                        N_GERM_AKI !Model constant no 299 : Low dissolved nitrogen concentration to trigger gerimination (below that gerimination starts)
    real(kind = DBL_PREC) ::                        P_FORM_AKI !Model constant no 300 : Formation rate constant for akinets
    real(kind = DBL_PREC) ::                      DAY_FORM_AKI !Model constant no 301 : Day of the year from which formation of akinetes is allowed
    real(kind = DBL_PREC) ::                        T_FORM_AKI !Model constant no 302 : Temperature to trigger formation (below that formation starts)
    real(kind = DBL_PREC) ::                        K_LOSS_AKI !Model constant no 303 : Loss rate constant for akinetes
    real(kind = DBL_PREC) ::                     K_MORT_AKI_20 !Model constant no 304 : Decompositon rate constant of akinetes at 20 degrees celcisus
    real(kind = DBL_PREC) ::                  THETA_K_MORT_AKI !Model constant no 305 : Arhenius temperature correction factor for akinete decomposition rate constant
    real(kind = DBL_PREC) ::                        T_GERM_AKI !Model constant no 306 : Temperature to trigger germination when DIN is also low
    real(kind = DBL_PREC) ::                            KHS_POC_DISS_SAT !Model constant no 307 :
    real(kind = DBL_PREC) ::                  KHS_PON_DISS_SAT !Model constant no 308 :
    real(kind = DBL_PREC) ::                  KHS_POP_DISS_SAT !Model constant no 309 :
    real(kind = DBL_PREC) ::                    frac_avail_DON !Model constant no 310 :
    real(kind = DBL_PREC) ::                    frac_avail_DOP !Model constant no 311 :
    real(kind = DBL_PREC) ::               frac_avail_DON_NOST !Model constant no 312 :
    real(kind = DBL_PREC) ::               KHS_DN_NOST_VEG_HET !Model constant no 313 :
    real(kind = DBL_PREC) ::         FRAC_FIX_N_FOR_GR_VEG_HET !Model constant no 314 :
    real(kind = DBL_PREC) ::                  FRAC_NOST_GROWTH !Model constant no 315 :
    real(kind = DBL_PREC) ::               K_MIN_PHYT_AMIN_DOC !Model constant no 316 :
    real(kind = DBL_PREC) ::               K_MIN_PHYT_AMIN_DON !Model constant no 317 :
    real(kind = DBL_PREC) ::               K_MIN_PHYT_AMIN_DOP !Model constant no 318 :
    real(kind = DBL_PREC) ::                          BETA_DIA !Model constant no 319 : Diatoms photoinhibition parameter (dimensionless, 0=none)
    real(kind = DBL_PREC) ::                          BETA_CYN !Model constant no 320 : Non-fixing cyanobacteria photoinhibition parameter
    real(kind = DBL_PREC) ::                      BETA_FIX_CYN !Model constant no 321 : Fixing cyanobacteria photoinhibition parameter
    real(kind = DBL_PREC) ::                          BETA_OPA !Model constant no 322 : OtherPhyto photoinhibition parameter
    real(kind = DBL_PREC) ::                 BETA_NOST_VEG_HET !Model constant no 323 : Nostocales photoinhibition parameter
    real(kind = DBL_PREC) ::                  ICE_LIGHT_TRANS !Model constant no 324 : under-ice PAR transmittance (0-1)

!==========================================================================
end module AQUABC_PELAGIC_MODEL_CONSTANTS
!==========================================================================

subroutine INIT_PELAGIC_MODEL_CONSTANTS

    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use para_aqua

    implicit none

    call para_get_value('K_A'                              ,                               K_A) !Model constant no   1 : Aeration coefficient (if negative calculates internally)
    call para_get_value('THETA_K_A'                        ,                         THETA_K_A) !Model constant no   2 : Temperature correction factor for aeration
    call para_get_value('XKC'                              ,                               XKC) !Model constant no   3 : Light extinction per chlorophyl unit,( mcg Chla/l/m) to calculate light saturation for Smith
    call para_get_value('PHIMX'                            ,                             PHIMX) !Model constant no   4 : Quantum yield const. mg C/mole photon
    call para_get_value('KG_DIA_OPT_TEMP'                  ,                   KG_DIA_OPT_TEMP) !Model constant no   5 : Diatoms Growth rate
    call para_get_value('DIA_OPT_TEMP_LR'                  ,                   DIA_OPT_TEMP_LR) !Model constant no   6 : Diatoms optimal temperature lower range
    call para_get_value('DIA_OPT_TEMP_UR'                  ,                   DIA_OPT_TEMP_UR) !Model constant no   7 : Diatoms optimal temperature upper range
    call para_get_value('EFF_DIA_GROWTH'                   ,                    EFF_DIA_GROWTH) !Model constant no   8 : Diatoms Effective growth. (1-EG)*growth - losses for respiration and excretion
    call para_get_value('KAPPA_DIA_UNDER_OPT_TEMP'         ,          KAPPA_DIA_UNDER_OPT_TEMP) !Model constant no   9 : Diatoms Temperature correction for growth lower temperature
    call para_get_value('KAPPA_DIA_OVER_OPT_TEMP'          ,           KAPPA_DIA_OVER_OPT_TEMP) !Model constant no  10 : Diatoms Temperature correction for growth upper temperature
    call para_get_value('KR_DIA_20'                        ,                         KR_DIA_20) !Model constant no  11 : Diatoms Respiration rate
    call para_get_value('THETA_KR_DIA'                     ,                      THETA_KR_DIA) !Model constant no  12 : Diatoms Temperature correction for basal respiration rate
    call para_get_value('KD_DIA_20'                        ,                         KD_DIA_20) !Model constant no  13 : Diatoms Mortality rate
    call para_get_value('THETA_KD_DIA'                     ,                      THETA_KD_DIA) !Model constant no  14 : Diatoms Temperature correction for Mortality rate
    call para_get_value('KHS_DIN_DIA'                      ,                       KHS_DIN_DIA) !Model constant no  15 : Diatoms Half saturation growth for DIN
    call para_get_value('KHS_DIP_DIA'                      ,                       KHS_DIP_DIA) !Model constant no  16 : Diatoms Half saturation growth for DIP
    call para_get_value('KHS_DSi_DIA'                      ,                       KHS_DSi_DIA) !Model constant no  17 : Diatoms Half saturation growth for DSi
    call para_get_value('KHS_O2_DIA'                       ,                        KHS_O2_DIA) !Model constant no  18 : Diatoms Half saturation growth for O2
    call para_get_value('FRAC_DIA_EXCR'                    ,                     FRAC_DIA_EXCR) !Model constant no  19 : Diatoms Fraction of excretion in metabolism rate
    call para_get_value('I_S_DIA'                          ,                           I_S_DIA) !Model constant no  20 : Diatoms Light saturation (langleys)
    call para_get_value('DO_STR_HYPOX_DIA_D'               ,                DO_STR_HYPOX_DIA_D) !Model constant no  21 : Diatoms Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_get_value('THETA_HYPOX_DIA_D'                ,                 THETA_HYPOX_DIA_D) !Model constant no  22 : Diatoms Multiplier of the exponent for Dissolved oxygen stress
    call para_get_value('EXPON_HYPOX_DIA_D'                ,                 EXPON_HYPOX_DIA_D) !Model constant no  23 : Diatoms Exponent constant for Dissolved oxygen stress
    call para_get_value('DIA_N_TO_C'                       ,                        DIA_N_TO_C) !Model constant no  24 : Diatoms Nitrogen to Carbon ratio
    call para_get_value('DIA_P_TO_C'                       ,                        DIA_P_TO_C) !Model constant no  25 : Diatoms Phosphorus to Carbon ratio
    call para_get_value('DIA_Si_TO_C'                      ,                       DIA_Si_TO_C) !Model constant no  26 : Diatoms Silica to Carbon ratio
    call para_get_value('DIA_O2_TO_C'                      ,                       DIA_O2_TO_C) !Model constant no  27 : Diatoms Oxygen to Carbon ratio for respiration
    call para_get_value('DIA_C_TO_CHLA'                    ,                     DIA_C_TO_CHLA) !Model constant no  28 : Diatoms Carbon to Chlorophil a ratio
    call para_get_value('KG_CYN_OPT_TEMP'                  ,                   KG_CYN_OPT_TEMP) !Model constant no  29 : Non-fixing cyanobacteria Growth rate
    call para_get_value('CYN_OPT_TEMP_LR'                  ,                   CYN_OPT_TEMP_LR) !Model constant no  30 : Non-fixing cyanobacteria optimal temperature lower range
    call para_get_value('CYN_OPT_TEMP_UR'                  ,                   CYN_OPT_TEMP_UR) !Model constant no  31 : Non-fixing cyanobacteria optimal temperature upper range
    call para_get_value('EFF_CYN_GROWTH'                   ,                    EFF_CYN_GROWTH) !Model constant no  32 : Non-fixing cyanobacteria Effective growth. (1-EG)*growth - losses for respiration and excretion
    call para_get_value('KAPPA_CYN_UNDER_OPT_TEMP'         ,          KAPPA_CYN_UNDER_OPT_TEMP) !Model constant no  33 : Non-fixing cyanobacteria Temperature correction for growth lower temperature
    call para_get_value('KAPPA_CYN_OVER_OPT_TEMP'          ,           KAPPA_CYN_OVER_OPT_TEMP) !Model constant no  34 : Non-fixing cyanobacteria Temperature correction for growth upper temperature
    call para_get_value('KR_CYN_20'                        ,                         KR_CYN_20) !Model constant no  35 : Non-fixing cyanobacteria Respiration rate
    call para_get_value('THETA_KR_CYN'                     ,                      THETA_KR_CYN) !Model constant no  36 : Non-fixing cyanobacteria Temperature correction for respiration rate
    call para_get_value('KD_CYN_20'                        ,                         KD_CYN_20) !Model constant no  37 : Non-fixing cyanobacteria Mortality rate
    call para_get_value('THETA_KD_CYN'                     ,                      THETA_KD_CYN) !Model constant no  38 : Non-fixing cyanobacteria Temperature correction for Mortality rate
    call para_get_value('KHS_DIN_CYN'                      ,                       KHS_DIN_CYN) !Model constant no  39 : Non-fixing cyanobacteria Half saturation growth for DIN
    call para_get_value('KHS_DIP_CYN'                      ,                       KHS_DIP_CYN) !Model constant no  40 : Non-fixing cyanobacteria Half saturation growth for DIP
    call para_get_value('KHS_O2_CYN'                       ,                        KHS_O2_CYN) !Model constant no  41 : Non-fixing cyanobacteria Half saturation growth for O2
    call para_get_value('FRAC_CYN_EXCR'                    ,                     FRAC_CYN_EXCR) !Model constant no  42 : Non-fixing cyanobacteria Fraction of excretion in metabolism rate
    call para_get_value('I_S_CYN'                          ,                           I_S_CYN) !Model constant no  43 : Non-fixing cyanobacteria Light saturation (langleys).Not used, is calculated!
    call para_get_value('DO_STR_HYPOX_CYN_D'               ,                DO_STR_HYPOX_CYN_D) !Model constant no  44 : Non-fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_get_value('THETA_HYPOX_CYN_D'                ,                 THETA_HYPOX_CYN_D) !Model constant no  45 : Non-fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress
    call para_get_value('EXPON_HYPOX_CYN_D'                ,                 EXPON_HYPOX_CYN_D) !Model constant no  46 : Non-fixing cyanobacteria Exponent constant for Dissolved oxygen stress
    call para_get_value('CYN_N_TO_C'                       ,                        CYN_N_TO_C) !Model constant no  47 : Non-fixing cyanobacteria Nitrogen to Carbon ratio ,was 0.1
    call para_get_value('CYN_P_TO_C'                       ,                        CYN_P_TO_C) !Model constant no  48 : Non-fixing cyanobacteria Phosphorus to Carbon ratio
    call para_get_value('CYN_O2_TO_C'                      ,                       CYN_O2_TO_C) !Model constant no  49 : Non-fixing cyanobacteria Oxygen to Carbon ratio for respiration
    call para_get_value('CYN_C_TO_CHLA'                    ,                     CYN_C_TO_CHLA) !Model constant no  50 : Non-fixing cyanobacteria Carbon to Chlorophyl a ratio
    call para_get_value('KG_FIX_CYN_OPT_TEMP'              ,               KG_FIX_CYN_OPT_TEMP) !Model constant no  51 : Fixing cyanobacteria Growth rate constant
    call para_get_value('FIX_CYN_OPT_TEMP_LR'              ,               FIX_CYN_OPT_TEMP_LR) !Model constant no  52 : Fixing Cyanobacteria optimal temperature lower range
    call para_get_value('FIX_CYN_OPT_TEMP_UR'              ,               FIX_CYN_OPT_TEMP_UR) !Model constant no  53 : Fixing Cyanobacteria optimal temperature upper range
    call para_get_value('EFF_FIX_CYN_GROWTH'               ,                EFF_FIX_CYN_GROWTH) !Model constant no  54 : Fixing cyanobacteria Effective growth. (1-EG)*growth - losses for RESP and excretion
    call para_get_value('KAPPA_FIX_CYN_UNDER_OPT_TEMP'     ,      KAPPA_FIX_CYN_UNDER_OPT_TEMP) !Model constant no  55 : Fixing cyanobacteria Temperature correction for growth lower temperature
    call para_get_value('KAPPA_FIX_CYN_OVER_OPT_TEMP'      ,       KAPPA_FIX_CYN_OVER_OPT_TEMP) !Model constant no  56 : Fixing cyanobacteria Temperature correction for growth upper temperature
    call para_get_value('KR_FIX_CYN_20'                    ,                     KR_FIX_CYN_20) !Model constant no  57 : Fixing cyanobacteria RESP rate constant
    call para_get_value('THETA_KR_FIX_CYN'                 ,                  THETA_KR_FIX_CYN) !Model constant no  58 : Fixing cyanobacteria Temperature correction for RESP rate
    call para_get_value('KD_FIX_CYN_20'                    ,                     KD_FIX_CYN_20) !Model constant no  59 : Fixing cyanobacteria Mortality rate constant
    call para_get_value('THETA_KD_FIX_CYN'                 ,                  THETA_KD_FIX_CYN) !Model constant no  60 : Fixing cyanobacteria Temperature correction for Mortality rate
    call para_get_value('KHS_DIN_FIX_CYN'                  ,                   KHS_DIN_FIX_CYN) !Model constant no  61 : Fixing cyanobacteria Half saturation growth for DIN
    call para_get_value('KHS_DIP_FIX_CYN'                  ,                   KHS_DIP_FIX_CYN) !Model constant no  62 : Fixing cyanobacteria Half saturation growth for DIP
    call para_get_value('KHS_O2_FIX_CYN'                   ,                    KHS_O2_FIX_CYN) !Model constant no  63 : Fixing cyanobacteria Half saturation growth for O2
    call para_get_value('FRAC_FIX_CYN_EXCR'                ,                 FRAC_FIX_CYN_EXCR) !Model constant no  64 : Fixing cyanobacteria Fraction of excretion in metabolism rate
    call para_get_value('I_S_FIX_CYN'                      ,                       I_S_FIX_CYN) !Model constant no  65 : Fixing cyanobacteria Light saturation (langleys). Not used, is calculated!
    call para_get_value('DO_STR_HYPOX_FIX_CYN_D'           ,            DO_STR_HYPOX_FIX_CYN_D) !Model constant no  66 : Fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_get_value('THETA_HYPOX_FIX_CYN_D'            ,             THETA_HYPOX_FIX_CYN_D) !Model constant no  67 : Fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress
    call para_get_value('EXPON_HYPOX_FIX_CYN_D'            ,             EXPON_HYPOX_FIX_CYN_D) !Model constant no  68 : Fixing cyanobacteria Exponent constant for Dissolved oxygen stress
    call para_get_value('FIX_CYN_N_TO_C'                   ,                    FIX_CYN_N_TO_C) !Model constant no  69 : Fixing cyanobacteria Nitrogen to Carbon ratio
    call para_get_value('FIX_CYN_P_TO_C'                   ,                    FIX_CYN_P_TO_C) !Model constant no  70 : Fixing cyanobacteria Phosphorus to Carbon ratio
    call para_get_value('FIX_CYN_O2_TO_C'                  ,                   FIX_CYN_O2_TO_C) !Model constant no  71 : Fixing cyanobacteria Oxygen to Carbon ratio for respiration
    call para_get_value('FIX_CYN_C_TO_CHLA'                ,                 FIX_CYN_C_TO_CHLA) !Model constant no  72 : Fixing cyanobacteria Carbon to Chlorophyl a ratio
    call para_get_value('R_FIX'                            ,                             R_FIX) !Model constant no  73 : Fixing cyanobacteria Ratio between non-fixing and fixing fractions growth rate
    call para_get_value('K_FIX'                            ,                             K_FIX) !Model constant no  74 : Fixing cyanobacteria Effectivity parameter of switching to nitrogen fixation
    call para_get_value('KG_OPA_OPT_TEMP'                  ,                   KG_OPA_OPT_TEMP) !Model constant no  75 : OtherPhyto Growth rate constant
    call para_get_value('OPA_OPT_TEMP_LR'                  ,                   OPA_OPT_TEMP_LR) !Model constant no  76 : OtherPhyto optimal temperature lower range
    call para_get_value('OPA_OPT_TEMP_UR'                  ,                   OPA_OPT_TEMP_UR) !Model constant no  77 : OtherPhyto optimal temperature upper range
    call para_get_value('EFF_OPA_GROWTH'                   ,                    EFF_OPA_GROWTH) !Model constant no  78 : OtherPhyto Effective growth. (1-EG)*growth - losses for respiration and excretion
    call para_get_value('KAPPA_OPA_UNDER_OPT_TEMP'         ,          KAPPA_OPA_UNDER_OPT_TEMP) !Model constant no  79 : OtherPhyto Temperature correction for growth lower temperature
    call para_get_value('KAPPA_OPA_OVER_OPT_TEMP'          ,           KAPPA_OPA_OVER_OPT_TEMP) !Model constant no  80 : OtherPhyto Temperature correction for growth upper temperature
    call para_get_value('KR_OPA_20'                        ,                         KR_OPA_20) !Model constant no  81 : OtherPhyto Respiration rate constant
    call para_get_value('THETA_KR_OPA'                     ,                      THETA_KR_OPA) !Model constant no  82 : OtherPhyto Temperature correction for respiration rate
    call para_get_value('KD_OPA_20'                        ,                         KD_OPA_20) !Model constant no  83 : OtherPhyto Mortality rate constant
    call para_get_value('THETA_KD_OPA'                     ,                      THETA_KD_OPA) !Model constant no  84 : OtherPhyto Temperature correction for Mortality rate
    call para_get_value('KHS_DIN_OPA'                      ,                       KHS_DIN_OPA) !Model constant no  85 : OtherPhyto Half saturation growth for DIN
    call para_get_value('KHS_DIP_OPA'                      ,                       KHS_DIP_OPA) !Model constant no  86 : OtherPhyto Half saturation growth for DIP
    call para_get_value('KHS_O2_OPA'                       ,                        KHS_O2_OPA) !Model constant no  87 : OtherPhyto Half saturation growth for O2
    call para_get_value('FRAC_OPA_EXCR'                    ,                     FRAC_OPA_EXCR) !Model constant no  88 : OtherPhyto Fraction of excretion in metabolism rate
    call para_get_value('I_S_OPA'                          ,                           I_S_OPA) !Model constant no  89 : OtherPhyto Light saturation (langleys). Not used, is calculated!
    call para_get_value('DO_STR_HYPOX_OPA_D'               ,                DO_STR_HYPOX_OPA_D) !Model constant no  90 : OtherPhyto Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_get_value('THETA_HYPOX_OPA_D'                ,                 THETA_HYPOX_OPA_D) !Model constant no  91 : OtherPhyto Multiplier of the exponent for Dissolved oxygen stress
    call para_get_value('EXPON_HYPOX_OPA_D'                ,                 EXPON_HYPOX_OPA_D) !Model constant no  92 : OtherPhyto Exponent constant for Dissolved oxygen stress
    call para_get_value('OPA_N_TO_C'                       ,                        OPA_N_TO_C) !Model constant no  93 : OtherPhyto Nitrogen to Carbon ratio
    call para_get_value('OPA_P_TO_C'                       ,                        OPA_P_TO_C) !Model constant no  94 : OtherPhyto Phosphorus to Carbon ratio
    call para_get_value('OPA_O2_TO_C'                      ,                       OPA_O2_TO_C) !Model constant no  95 : OtherPhyto Oxygen to Carbon ratio for respiration
    call para_get_value('OPA_C_TO_CHLA'                    ,                     OPA_C_TO_CHLA) !Model constant no  96 : OtherPhyto Carbon to Chlorophyl a ratio
    call para_get_value('KG_ZOO_OPT_TEMP'                  ,                   KG_ZOO_OPT_TEMP) !Model constant no  97 : Zooplankton Growth rate
    call para_get_value('ZOO_OPT_TEMP_LR'                  ,                   ZOO_OPT_TEMP_LR) !Model constant no  98 : Zooplankton optimal temperature lower range
    call para_get_value('ZOO_OPT_TEMP_UR'                  ,                   ZOO_OPT_TEMP_UR) !Model constant no  99 : Zooplankton optimal temperature upper range
    call para_get_value('EFF_ZOO_GROWTH'                   ,                    EFF_ZOO_GROWTH) !Model constant no 100 : Zooplankton Effective growth. (1-EG)*growth - losses for respiration and excretion
    call para_get_value('KAPPA_ZOO_UNDER_OPT_TEMP'         ,          KAPPA_ZOO_UNDER_OPT_TEMP) !Model constant no 101 : Zooplankton Temperature correction for growth lower temperature
    call para_get_value('KAPPA_ZOO_OVER_OPT_TEMP'          ,           KAPPA_ZOO_OVER_OPT_TEMP) !Model constant no 102 : Zooplankton Temperature correction for growth upper temperature
    call para_get_value('GRAT_ZOO_DIA'                     ,                      GRAT_ZOO_DIA) !Model constant no 103 : Zooplankton Grazing rate (growhth rate multiplier) on diatoms
    call para_get_value('GRAT_ZOO_CYN'                     ,                      GRAT_ZOO_CYN) !Model constant no 104 : Zooplankton Grazing rate (growhth rate multiplier) on Cyanobacteria
    call para_get_value('GRAT_ZOO_OPA'                     ,                      GRAT_ZOO_OPA) !Model constant no 105 : Zooplankton Grazing rate (growhth rate multiplier) on OtherPhyto
    call para_get_value('GRAT_ZOO_FIX_CYN'                 ,                  GRAT_ZOO_FIX_CYN) !Model constant no 106 : Zooplankton Grazing rate (growhth rate multiplier) on fixing Cyanobacteria
    call para_get_value('GRAT_ZOO_NOST_VEG_HET'            ,             GRAT_ZOO_NOST_VEG_HET) !Model constant no 107 : Zooplankton Grazing rate (growhth rate multiplier) on Nostocles (veg + het)
    call para_get_value('GRAT_ZOO_DET_PART_ORG_C'          ,           GRAT_ZOO_DET_PART_ORG_C) !Model constant no 108 : Zooplankton Grazing rate (growhth rate multiplier) on part. ORG_C
    call para_get_value('PREF_ZOO_DIA'                     ,                      PREF_ZOO_DIA) !Model constant no 109 : Zooplankton Preference for Diatoms
    call para_get_value('PREF_ZOO_CYN'                     ,                      PREF_ZOO_CYN) !Model constant no 110 : Zooplankton Preference for Cyanobacteria
    call para_get_value('PREF_ZOO_FIX_CYN'                 ,                  PREF_ZOO_FIX_CYN) !Model constant no 111 : Zooplankton Preference for fixing Cyanobacteria
    call para_get_value('PREF_ZOO_NOST_VEG_HET'            ,             PREF_ZOO_NOST_VEG_HET) !Model constant no 112 : Zooplankton Preference for nostocales (veg + het)
    call para_get_value('PREF_ZOO_OPA'                     ,                      PREF_ZOO_OPA) !Model constant no 113 : Zooplankton Preference for OtherPhyto
    call para_get_value('PREF_ZOO_DET_PART_ORG_C'          ,           PREF_ZOO_DET_PART_ORG_C) !Model constant no 114 : Zooplankton Preference for Part. ORG_C
    call para_get_value('KHS_DIA_C_ZOO'                    ,                     KHS_DIA_C_ZOO) !Model constant no 115 : Zooplankton Half saturation growth for diatoms
    call para_get_value('KHS_CYN_C_ZOO'                    ,                     KHS_CYN_C_ZOO) !Model constant no 116 : Zooplankton Half saturation growth for Cyanobacteria
    call para_get_value('KHS_FIX_CYN_C_ZOO'                ,                 KHS_FIX_CYN_C_ZOO) !Model constant no 117 : Zooplankton Half saturation growth for fixing Cyanobacteria
    call para_get_value('KHS_NOST_VEG_HET_C_ZOO'           ,            KHS_NOST_VEG_HET_C_ZOO) !Model constant no 118 : Zooplankton Half saturation growth for Nostocales (veg + het)
    call para_get_value('KHS_OPA_C_ZOO'                    ,                     KHS_OPA_C_ZOO) !Model constant no 119 : Zooplankton Half saturation growth for OtherPhyto
    call para_get_value('KHS_DET_PART_ORG_C_ZOO'           ,            KHS_DET_PART_ORG_C_ZOO) !Model constant no 120 : Zooplankton Half saturation growth for part. ORG_C
    call para_get_value('FOOD_MIN_ZOO'                     ,                      FOOD_MIN_ZOO) !Model constant no 121 : Zooplankton Minimum food conc. for feeding
    call para_get_value('KE_ZOO'                           ,                            KE_ZOO) !Model constant no 122 : not used Zooplankton Excretion rate as growth fraction
    call para_get_value('FRAC_ZOO_EX_ORG'                  ,                   FRAC_ZOO_EX_ORG) !Model constant no 123 : not used Zooplankton Excretion rate organic fraction
    call para_get_value('KR_ZOO_20'                        ,                         KR_ZOO_20) !Model constant no 124 : Zooplankton Respiration rate
    call para_get_value('THETA_KR_ZOO'                     ,                      THETA_KR_ZOO) !Model constant no 125 : Zooplankton Respiration rate Temperature correction
    call para_get_value('KD_ZOO_20'                        ,                         KD_ZOO_20) !Model constant no 126 : Zooplankton Mortality rate
    call para_get_value('THETA_KD_ZOO'                     ,                      THETA_KD_ZOO) !Model constant no 127 : Zooplankton Mortality rate Temperature correction
    call para_get_value('DO_STR_HYPOX_ZOO_D'               ,                DO_STR_HYPOX_ZOO_D) !Model constant no 128 : Zooplankton Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_get_value('THETA_HYPOX_ZOO_D'                ,                 THETA_HYPOX_ZOO_D) !Model constant no 129 : Zooplankton Multiplier of the exponent for Dissolved oxygen stress
    call para_get_value('EXPON_HYPOX_ZOO_D'                ,                 EXPON_HYPOX_ZOO_D) !Model constant no 130 : Zooplankton Exponent constant for Dissolved oxygen stress
    call para_get_value('ZOO_N_TO_C'                       ,                        ZOO_N_TO_C) !Model constant no 131 : Zooplankton Nitrogen to Carbon ratio
    call para_get_value('ZOO_P_TO_C'                       ,                        ZOO_P_TO_C) !Model constant no 132 : Zooplankton Phosphorus to Carbon ratio
    call para_get_value('ZOO_O2_TO_C'                      ,                       ZOO_O2_TO_C) !Model constant no 133 : Zooplankton Oxygen to Carbon ratio for respiration
    call para_get_value('KDISS_DET_PART_ORG_C_20'          ,           KDISS_DET_PART_ORG_C_20) !Model constant no 134 : POC Dissolution rate not dependent on phytoplankton
    call para_get_value('THETA_KDISS_DET_PART_ORG_C'       ,        THETA_KDISS_DET_PART_ORG_C) !Model constant no 135 : POC Carbon Dissolution rate Temperature correction
    call para_get_value('FAC_PHYT_DET_PART_ORG_C'          ,           FAC_PHYT_DET_PART_ORG_C) !Model constant no 136 : POC Carbon Phytoplankton linear factor for dissolution rate
    call para_get_value('KDISS_DET_PART_ORG_N_20'          ,           KDISS_DET_PART_ORG_N_20) !Model constant no 137 : PON Dissolution rate not dependent on phytoplankton
    call para_get_value('THETA_KDISS_DET_PART_ORG_N'       ,        THETA_KDISS_DET_PART_ORG_N) !Model constant no 138 : PON Dissolution rate Temperature correction
    call para_get_value('KHS_DISS_N'                       ,                        KHS_DISS_N) !Model constant no 139 : PON dissolution reverse half saturation for DIN
    call para_get_value('FAC_PHYT_DET_PART_ORG_N'          ,           FAC_PHYT_DET_PART_ORG_N) !Model constant no 140 : PON Phytoplankton linear factor for dissolution rate
    call para_get_value('KDISS_DET_PART_ORG_P_20'          ,           KDISS_DET_PART_ORG_P_20) !Model constant no 141 : POP Phosphorus Dissolution rate not dependent on phytoplankton
    call para_get_value('THETA_KDISS_DET_PART_ORG_P'       ,        THETA_KDISS_DET_PART_ORG_P) !Model constant no 142 : POP Dissolution rate Temperature correction
    call para_get_value('KHS_DISS_P'                       ,                        KHS_DISS_P) !Model constant no 143 : POP Dissolution reverse half saturation for DIP
    call para_get_value('FAC_PHYT_DET_PART_ORG_P'          ,           FAC_PHYT_DET_PART_ORG_P) !Model constant no 144 : POP Phytoplankton linear factor for dissolution rate
    call para_get_value('KDISS_PART_Si_20'                 ,                  KDISS_PART_Si_20) !Model constant no 145 : Particulate Silica Dissolution rate
    call para_get_value('THETA_KDISS_PART_Si'              ,               THETA_KDISS_PART_Si) !Model constant no 146 : Particulate Silica Dissolution rate Temperature correction
    call para_get_value('FAC_PHYT_AMIN_DOC'                ,                 FAC_PHYT_AMIN_DOC) !Model constant no 147 : DOC  Phytoplankton linear factor for mineralisation rate
    call para_get_value('KHS_AMIN_N'                       ,                        KHS_AMIN_N) !Model constant no 148 : DON  reverse half saturation for DIN
    call para_get_value('FAC_PHYT_AMIN_DON'                ,                 FAC_PHYT_AMIN_DON) !Model constant no 149 : DON Phytoplankton linear factor for mineralisation rate
    call para_get_value('KHS_AMIN_P'                       ,                        KHS_AMIN_P) !Model constant no 150 : DOP reverse half saturation for DIP
    call para_get_value('FAC_PHYT_AMIN_DOP'                ,                 FAC_PHYT_AMIN_DOP) !Model constant no 151 : DOP Phytoplankton linear factor for mineralisation rate
    call para_get_value('K_NITR_20'                        ,                         K_NITR_20) !Model constant no 152 : Amonia nitrification rate
    call para_get_value('THETA_K_NITR'                     ,                      THETA_K_NITR) !Model constant no 153 : Amonia nitrification rate Temperature constant
    call para_get_value('KHS_NITR_OXY'                     ,                      KHS_NITR_OXY) !Model constant no 154 : Amonia nitrification half saturation for Oxygen
    call para_get_value('KHS_NITR_NH4_N'                   ,                    KHS_NITR_NH4_N) !Model constant no 155 : Amonia nitrification half saturation for Amonia
    call para_get_value('PH_NITR_NH4_MIN'                  ,                   PH_NITR_NH4_MIN) !Model constant no 156 : optimum lower range for pH correction factor for nitrification
    call para_get_value('PH_NITR_NH4_MAX'                  ,                   PH_NITR_NH4_MAX) !Model constant no 157 : optimum upper range for pH correction factor for nitrification
    call para_get_value('k_OX_FE_II'                       ,                        k_OX_FE_II) !Model constant no 158 : Oxidation rate for iron 2+
    call para_get_value('k_RED_FE_III'                     ,                      k_RED_FE_III) !Model constant no 159 : reduction rate for iron 3+
    call para_get_value('k_OX_MN_II'                       ,                        k_OX_MN_II) !Model constant no 160 : oxidation rate for manganese 2+
    call para_get_value('k_RED_MN_IV'                      ,                       k_RED_MN_IV) !Model constant no 161 : reduction rate for manganese 4+
    call para_get_value('KHS_DOXY_FE_III_RED'              ,               KHS_DOXY_FE_III_RED) !Model constant no 162 : reversed Monod half saturation of DOXY for iron 3+ reduction
    call para_get_value('KHS_DOXY_MN_IV_RED'               ,                KHS_DOXY_MN_IV_RED) !Model constant no 163 : reversed Monod half saturation of DOXY for manganese 4+ reduction
    call para_get_value('K_MIN_DOC_DOXY_20'                ,                 K_MIN_DOC_DOXY_20) !Model constant no 164 : Mineralization rate constant of DOC at 20 C for dissolved oxygen as final electron acceptor
    call para_get_value('K_MIN_DOC_NO3N_20'                ,                 K_MIN_DOC_NO3N_20) !Model constant no 165 : Mineralization rate constant of DOC at 20 C for nitrate as final electron acceptor
    call para_get_value('K_MIN_DOC_MN_IV_20'               ,                K_MIN_DOC_MN_IV_20) !Model constant no 166 : Mineralization rate constant of DOC at 20 C for Mn IV as final electron acceptor
    call para_get_value('K_MIN_DOC_FE_III_20'              ,               K_MIN_DOC_FE_III_20) !Model constant no 167 : Mineralization rate constant of DOC at 20 C for Fe III as final electron acceptor
    call para_get_value('K_MIN_DOC_S_PLUS_6_20'            ,             K_MIN_DOC_S_PLUS_6_20) !Model constant no 168 : Mineralization rate constant of DOC at 20 C for sulphate as final electron acceptor
    call para_get_value('K_MIN_DOC_DOC_20'                 ,                  K_MIN_DOC_DOC_20) !Model constant no 169 : Mineralization rate constant of DOC at 20 C for DOC itself as final electron acceptor
    call para_get_value('THETA_K_MIN_DOC_DOXY'             ,              THETA_K_MIN_DOC_DOXY) !Model constant no 170 : Temperature correction factor for mineralization process rate constant of DOC for dissolved oxygen as final electron acceptor
    call para_get_value('THETA_K_MIN_DOC_NO3N'             ,              THETA_K_MIN_DOC_NO3N) !Model constant no 171 : Temperature correction factor for mineralization process rate constant of DOC for nitrate as final electron acceptor
    call para_get_value('THETA_K_MIN_DOC_MN_IV'            ,             THETA_K_MIN_DOC_MN_IV) !Model constant no 172 : Temperature correction factor for mineralization process rate constant of DOC for MN IV as final electron acceptor
    call para_get_value('THETA_K_MIN_DOC_FE_III'           ,            THETA_K_MIN_DOC_FE_III) !Model constant no 173 : Temperature correction factor for mineralization process rate constant of DOC for FE III as final electron acceptor
    call para_get_value('THETA_K_MIN_DOC_S_PLUS_6'         ,          THETA_K_MIN_DOC_S_PLUS_6) !Model constant no 174 : Temperature correction factor for mineralization process rate constant of DOC for Suphate as final electron acceptor
    call para_get_value('THETA_K_MIN_DOC_DOC'              ,               THETA_K_MIN_DOC_DOC) !Model constant no 175 : Temperature correction factor for mineralization process rate constant of DOC for DOC
    call para_get_value('K_HS_DOC_MIN_DOXY'                ,                 K_HS_DOC_MIN_DOXY) !Model constant no 176 : Monod type half-saturation concentration of DOC for DOC mineralization for dissolved oxygen as the final electron acceptor
    call para_get_value('K_HS_DOC_MIN_NO3N'                ,                 K_HS_DOC_MIN_NO3N) !Model constant no 177 : Monod type half-saturation concentration of DOC for DOC mineralization for nitrate as the final electron acceptor
    call para_get_value('K_HS_DOC_MIN_MN_IV'               ,                K_HS_DOC_MIN_MN_IV) !Model constant no 178 : Monod type half-saturation concentration of DOC for DOC mineralization for Mn IV as the final electron acceptor
    call para_get_value('K_HS_DOC_MIN_FE_III'              ,               K_HS_DOC_MIN_FE_III) !Model constant no 179 : Monod type half-saturation concentration of DOC for DOC mineralization for Fe III as the final electron acceptor
    call para_get_value('K_HS_DOC_MIN_S_PLUS_6'            ,             K_HS_DOC_MIN_S_PLUS_6) !Model constant no 180 : Monod type half-saturation concentration of DOC for DOC mineralization for sulphate as the final electron acceptor
    call para_get_value('K_HS_DOC_MIN_DOC'                 ,                  K_HS_DOC_MIN_DOC) !Model constant no 181 : Monod type half-saturation concentration of DOC for DOC mineralization for DOC itself as the final electron acceptor
    call para_get_value('K_HS_DOXY_RED_LIM'                ,                 K_HS_DOXY_RED_LIM) !Model constant no 182 : Monod type half-saturation concentration of dissolved oxygen to limit the consumption of dissolved oxygen as the final electron acce
    call para_get_value('K_HS_NO3N_RED_LIM'                ,                 K_HS_NO3N_RED_LIM) !Model constant no 183 : Monod type half-saturation concentration of nitrate nitrogen to limit the consumption of nitrate as the final electron acceptor for
    call para_get_value('K_HS_MN_IV_RED_LIM'               ,                K_HS_MN_IV_RED_LIM) !Model constant no 184 : Monod type half-saturation concentration of MN_IV to limit the consumption of Mn IV as the final electron acceptor for DOC mineraliz
    call para_get_value('K_HS_FE_III_RED_LIM'              ,               K_HS_FE_III_RED_LIM) !Model constant no 185 : Monod type half-saturation concentration of FE_III to limit the consumption of Fe III as the final electron acceptor for DOC mineral
    call para_get_value('K_HS_S_PLUS_6_RED_LIM'            ,             K_HS_S_PLUS_6_RED_LIM) !Model constant no 186 : Monod type half-saturation concentration of sulphate to limit the consumption of sulphate as the final electron acceptor for DOC min
    call para_get_value('K_HS_DOXY_RED_INHB'               ,                K_HS_DOXY_RED_INHB) !Model constant no 187 : Reversed Monod type half saturation concentration that simulate the inhibition effect of dissolved oxygen on consumption of NO3, Mn
    call para_get_value('K_HS_NO3N_RED_INHB'               ,                K_HS_NO3N_RED_INHB) !Model constant no 188 : Reversed Monod type half saturation concentration that simulate the inhibition effect of nitrate on consumption of Mn IV, Fe III, S
    call para_get_value('K_HS_MN_IV_RED_INHB'              ,               K_HS_MN_IV_RED_INHB) !Model constant no 189 : Reversed Monod type half saturation concentration that simulate the inhibition effect of Mn IV on consumption of Fe III, S VI and DO
    call para_get_value('K_HS_FE_III_RED_INHB'             ,              K_HS_FE_III_RED_INHB) !Model constant no 190 : Reversed Monod type half saturation concentration that simulate the inhibition effect of Fe III on consumption of S VI and DOC itsel
    call para_get_value('K_HS_S_PLUS_6_RED_INHB'           ,            K_HS_S_PLUS_6_RED_INHB) !Model constant no 191 : Reversed Monod type half saturation concentration that simulate the inhibition effect of S VI on consumption of DOC itself for DOC m
    call para_get_value('PH_MIN_DOC_MIN_DOXY'              ,               PH_MIN_DOC_MIN_DOXY) !Model constant no 192 : Min. pH for the optimum pH range for DOC mineralization with DOXY as final electron acceptor
    call para_get_value('PH_MIN_DOC_MIN_NO3N'              ,               PH_MIN_DOC_MIN_NO3N) !Model constant no 193 : Min. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor
    call para_get_value('PH_MIN_DOC_MIN_MN_IV'             ,              PH_MIN_DOC_MIN_MN_IV) !Model constant no 194 : Min. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor
    call para_get_value('PH_MIN_DOC_MIN_FE_III'            ,             PH_MIN_DOC_MIN_FE_III) !Model constant no 195 : Min. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor
    call para_get_value('PH_MIN_DOC_MIN_S_PLUS_6'          ,           PH_MIN_DOC_MIN_S_PLUS_6) !Model constant no 196 : Min. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor
    call para_get_value('PH_MIN_DOC_MIN_DOC'               ,                PH_MIN_DOC_MIN_DOC) !Model constant no 197 : Min. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor
    call para_get_value('PH_MAX_DOC_MIN_DOXY'              ,               PH_MAX_DOC_MIN_DOXY) !Model constant no 198 : Max. pH for the optimum pH range for DOC mineralization with dissolved oxygen as final electron acceptor
    call para_get_value('PH_MAX_DOC_MIN_NO3N'              ,               PH_MAX_DOC_MIN_NO3N) !Model constant no 199 : Max. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor
    call para_get_value('PH_MAX_DOC_MIN_MN_IV'             ,              PH_MAX_DOC_MIN_MN_IV) !Model constant no 200 : Max. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor
    call para_get_value('PH_MAX_DOC_MIN_FE_III'            ,             PH_MAX_DOC_MIN_FE_III) !Model constant no 201 : Max. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor
    call para_get_value('PH_MAX_DOC_MIN_S_PLUS_6'          ,           PH_MAX_DOC_MIN_S_PLUS_6) !Model constant no 202 : Max. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor
    call para_get_value('PH_MAX_DOC_MIN_DOC'               ,                PH_MAX_DOC_MIN_DOC) !Model constant no 203 : Max. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor
    call para_get_value('K_MIN_DON_DOXY_20'                ,                 K_MIN_DON_DOXY_20) !Model constant no 204 : Mineralization rate constant of DON at 20 C for dissolved oxygen as final electron acceptor
    call para_get_value('K_MIN_DON_NO3N_20'                ,                 K_MIN_DON_NO3N_20) !Model constant no 205 : Mineralization rate constant of DON at 20 C for NO3_N as final electron acceptor
    call para_get_value('K_MIN_DON_MN_IV_20'               ,                K_MIN_DON_MN_IV_20) !Model constant no 206 : Mineralization rate constant of DON at 20 C for Mn IV as final electron acceptor
    call para_get_value('K_MIN_DON_FE_III_20'              ,               K_MIN_DON_FE_III_20) !Model constant no 207 : Mineralization rate constant of DON at 20 C for FE III as final electron acceptor
    call para_get_value('K_MIN_DON_S_PLUS_6_20'            ,             K_MIN_DON_S_PLUS_6_20) !Model constant no 208 : Mineralization rate constant of DON at 20 C for S_PLUS_6 as final electron acceptor
    call para_get_value('K_MIN_DON_DOC_20'                 ,                  K_MIN_DON_DOC_20) !Model constant no 209 : Mineralization rate constant of DON at 20 C for DOC as final electron acceptor
    call para_get_value('THETA_K_MIN_DON_DOXY'             ,              THETA_K_MIN_DON_DOXY) !Model constant no 210 : Temperature correction factor for mineralization process rate constant of DON for dissolved oxygen as final electron acceptor
    call para_get_value('THETA_K_MIN_DON_NO3N'             ,              THETA_K_MIN_DON_NO3N) !Model constant no 211 : Temperature correction factor for mineralization process rate constant of DON for nitrate as final electron acceptor
    call para_get_value('THETA_K_MIN_DON_MN_IV'            ,             THETA_K_MIN_DON_MN_IV) !Model constant no 212 : Temperature correction factor for mineralization process rate constant of DON for MN_IV as final electron acceptor
    call para_get_value('THETA_K_MIN_DON_FE_III'           ,            THETA_K_MIN_DON_FE_III) !Model constant no 213 : Temperature correction factor for mineralization process rate constant of DON for FE_III as final electron acceptor
    call para_get_value('THETA_K_MIN_DON_S_PLUS_6'         ,          THETA_K_MIN_DON_S_PLUS_6) !Model constant no 214 : Temperature correction factor for mineralization process rate constant of DON for sulphate as final electron acceptor
    call para_get_value('THETA_K_MIN_DON_DOC'              ,               THETA_K_MIN_DON_DOC) !Model constant no 215 : Temperature correction factor for mineralization process rate constant of DON for DOC
    call para_get_value('K_HS_DON_MIN_DOXY'                ,                 K_HS_DON_MIN_DOXY) !Model constant no 216 : Monod type half-saturation concentration of DON for DON mineralization for dissolved oxygen as the final electron acceptor
    call para_get_value('K_HS_DON_MIN_NO3N'                ,                 K_HS_DON_MIN_NO3N) !Model constant no 217 : Monod type half-saturation concentration of DON for DON mineralization for nitrate nitrogen as the final electron acceptor
    call para_get_value('K_HS_DON_MIN_MN_IV'               ,                K_HS_DON_MIN_MN_IV) !Model constant no 218 : Monod type half-saturation concentration of DON for DON mineralization for MN_IV as the final electron acceptor
    call para_get_value('K_HS_DON_MIN_FE_III'              ,               K_HS_DON_MIN_FE_III) !Model constant no 219 : Monod type half-saturation concentration of DON for DON mineralization for FE_III as the final electron acceptor
    call para_get_value('K_HS_DON_MIN_S_PLUS_6'            ,             K_HS_DON_MIN_S_PLUS_6) !Model constant no 220 : Monod type half-saturation concentration of DON for DON mineralization for sulphate sulphur as the final electron acceptor
    call para_get_value('K_HS_DON_MIN_DOC'                 ,                  K_HS_DON_MIN_DOC) !Model constant no 221 : Monod type half-saturation concentration of DON for DON mineralization for DOC as the final electron acceptor
    call para_get_value('PH_MIN_DON_MIN_DOXY'              ,               PH_MIN_DON_MIN_DOXY) !Model constant no 222 : Min. pH for the optimum pH range for DON mineralization with DOXY as final electron acceptor
    call para_get_value('PH_MIN_DON_MIN_NO3N'              ,               PH_MIN_DON_MIN_NO3N) !Model constant no 223 : Min. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor
    call para_get_value('PH_MIN_DON_MIN_MN_IV'             ,              PH_MIN_DON_MIN_MN_IV) !Model constant no 224 : Min. pH for the optimum pH range for DON mineralization with MN IV as final electron acceptor
    call para_get_value('PH_MIN_DON_MIN_FE_III'            ,             PH_MIN_DON_MIN_FE_III) !Model constant no 225 : Min. pH for the optimum pH range for DON mineralization with FE III as final electron acceptor
    call para_get_value('PH_MIN_DON_MIN_S_PLUS_6'          ,           PH_MIN_DON_MIN_S_PLUS_6) !Model constant no 226 : Min. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor
    call para_get_value('PH_MIN_DON_MIN_DOC'               ,                PH_MIN_DON_MIN_DOC) !Model constant no 227 : Min. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor
    call para_get_value('PH_MAX_DON_MIN_DOXY'              ,               PH_MAX_DON_MIN_DOXY) !Model constant no 228 : Max. pH for the optimum pH range for DON mineralization with dissolved oxygen as final electron acceptor
    call para_get_value('PH_MAX_DON_MIN_NO3N'              ,               PH_MAX_DON_MIN_NO3N) !Model constant no 229 : Max. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor
    call para_get_value('PH_MAX_DON_MIN_MN_IV'             ,              PH_MAX_DON_MIN_MN_IV) !Model constant no 230 : Max. pH for the optimum pH range for DON mineralization with Mn IV as final electron acceptor
    call para_get_value('PH_MAX_DON_MIN_FE_III'            ,             PH_MAX_DON_MIN_FE_III) !Model constant no 231 : Max. pH for the optimum pH range for DON mineralization with FE_III as final electron acceptor
    call para_get_value('PH_MAX_DON_MIN_S_PLUS_6'          ,           PH_MAX_DON_MIN_S_PLUS_6) !Model constant no 232 : Max. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor
    call para_get_value('PH_MAX_DON_MIN_DOC'               ,                PH_MAX_DON_MIN_DOC) !Model constant no 233 : Max. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor
    call para_get_value('K_MIN_DOP_DOXY_20'                ,                 K_MIN_DOP_DOXY_20) !Model constant no 234 : Mineralization rate constant of DOP at 20 C for dissolved oxygen as final electron acceptor
    call para_get_value('K_MIN_DOP_NO3N_20'                ,                 K_MIN_DOP_NO3N_20) !Model constant no 235 : Mineralization rate constant of DOP at 20 C for NO3_N as final electron acceptor
    call para_get_value('K_MIN_DOP_MN_IV_20'               ,                K_MIN_DOP_MN_IV_20) !Model constant no 236 : Mineralization rate constant of DOP at 20 C for Mn IV as final electron acceptor
    call para_get_value('K_MIN_DOP_FE_III_20'              ,               K_MIN_DOP_FE_III_20) !Model constant no 237 : Mineralization rate constant of DOP at 20 C for FE_III as final electron acceptor
    call para_get_value('K_MIN_DOP_S_PLUS_6_20'            ,             K_MIN_DOP_S_PLUS_6_20) !Model constant no 238 : Mineralization rate constant of DOP at 20 C for S_PLUS_6 as final electron acceptor
    call para_get_value('K_MIN_DOP_DOC_20'                 ,                  K_MIN_DOP_DOC_20) !Model constant no 239 : Mineralization rate constant of DOP at 20 C for DOC as final electron acceptor
    call para_get_value('THETA_K_MIN_DOP_DOXY'             ,              THETA_K_MIN_DOP_DOXY) !Model constant no 240 : Temperature correction factor for mineralization process rate constant of DOP for dissolved oxygen as final electron acceptor
    call para_get_value('THETA_K_MIN_DOP_NO3N'             ,              THETA_K_MIN_DOP_NO3N) !Model constant no 241 : Temperature correction factor for mineralization process rate constant of DOP for nitrate as final electron acceptor
    call para_get_value('THETA_K_MIN_DOP_MN_IV'            ,             THETA_K_MIN_DOP_MN_IV) !Model constant no 242 : Temperature correction factor for mineralization process rate constant of DOP for Mn IV as final electron acceptor
    call para_get_value('THETA_K_MIN_DOP_FE_III'           ,            THETA_K_MIN_DOP_FE_III) !Model constant no 243 : Temperature correction factor for mineralization process rate constant of DOP for FE_III as final electron acceptor
    call para_get_value('THETA_K_MIN_DOP_S_PLUS_6'         ,          THETA_K_MIN_DOP_S_PLUS_6) !Model constant no 244 : Temperature correction factor for mineralization process rate constant of DOP for sulphate as final electron acceptor
    call para_get_value('THETA_K_MIN_DOP_DOC'              ,               THETA_K_MIN_DOP_DOC) !Model constant no 245 : Temperature correction factor for mineralization process rate constant of DOP for DOC
    call para_get_value('K_HS_DOP_MIN_DOXY'                ,                 K_HS_DOP_MIN_DOXY) !Model constant no 246 : Monod type half-saturation concentration of DOP for DOP mineralization for dissolved oxygen as the final electron acceptor
    call para_get_value('K_HS_DOP_MIN_NO3N'                ,                 K_HS_DOP_MIN_NO3N) !Model constant no 247 : Monod type half-saturation concentration of DOP for DOP mineralization for nitrate nitrogen as the final electron acceptor
    call para_get_value('K_HS_DOP_MIN_MN_IV'               ,                K_HS_DOP_MIN_MN_IV) !Model constant no 248 : Monod type half-saturation concentration of DOP for DOP mineralization for MN_IV as the final electron acceptor
    call para_get_value('K_HS_DOP_MIN_FE_III'              ,               K_HS_DOP_MIN_FE_III) !Model constant no 249 : Monod type half-saturation concentration of DOP for DOP mineralization for FE_III as the final electron acceptor
    call para_get_value('K_HS_DOP_MIN_S_PLUS_6'            ,             K_HS_DOP_MIN_S_PLUS_6) !Model constant no 250 : Monod type half-saturation concentration of DOP for DOP mineralization for sulphate sulphur as the final electron acceptor
    call para_get_value('K_HS_DOP_MIN_DOC'                 ,                  K_HS_DOP_MIN_DOC) !Model constant no 251 : Monod type half-saturation concentration of DOP for DOP mineralization for DOC as the final electron acceptor
    call para_get_value('PH_MIN_DOP_MIN_DOXY'              ,               PH_MIN_DOP_MIN_DOXY) !Model constant no 252 : Min. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor
    call para_get_value('PH_MIN_DOP_MIN_NO3N'              ,               PH_MIN_DOP_MIN_NO3N) !Model constant no 253 : Min. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor
    call para_get_value('PH_MIN_DOP_MIN_MN_IV'             ,              PH_MIN_DOP_MIN_MN_IV) !Model constant no 254 : Min. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor
    call para_get_value('PH_MIN_DOP_MIN_FE_III'            ,             PH_MIN_DOP_MIN_FE_III) !Model constant no 255 : Min. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor
    call para_get_value('PH_MIN_DOP_MIN_S_PLUS_6'          ,           PH_MIN_DOP_MIN_S_PLUS_6) !Model constant no 256 : Min. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor
    call para_get_value('PH_MIN_DOP_MIN_DOC'               ,                PH_MIN_DOP_MIN_DOC) !Model constant no 257 : Min. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor
    call para_get_value('PH_MAX_DOP_MIN_DOXY'              ,               PH_MAX_DOP_MIN_DOXY) !Model constant no 258 : Max. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor
    call para_get_value('PH_MAX_DOP_MIN_NO3N'              ,               PH_MAX_DOP_MIN_NO3N) !Model constant no 259 : Max. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor
    call para_get_value('PH_MAX_DOP_MIN_MN_IV'             ,              PH_MAX_DOP_MIN_MN_IV) !Model constant no 260 : Max. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor
    call para_get_value('PH_MAX_DOP_MIN_FE_III'            ,             PH_MAX_DOP_MIN_FE_III) !Model constant no 261 : Max. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor
    call para_get_value('PH_MAX_DOP_MIN_S_PLUS_6'          ,           PH_MAX_DOP_MIN_S_PLUS_6) !Model constant no 262 : Max. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor
    call para_get_value('PH_MAX_DOP_MIN_DOC'               ,                PH_MAX_DOP_MIN_DOC) !Model constant no 263 : Max. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor
    call para_get_value('k_OX_CH4'                         ,                          k_OX_CH4) !Model constant no 264 : Methane oxidation rate constant at 20 C
    call para_get_value('THETA_k_OX_CH4'                   ,                    THETA_k_OX_CH4) !Model constant no 265 : Arhenius type temperature correction for methane oxidation rate constant
    call para_get_value('k_HS_OX_CH4_DOXY'                 ,                  k_HS_OX_CH4_DOXY) !Model constant no 266 : Monod type Half saturation of methane oxidation for dissolved oxygen
    call para_get_value('k_OX_H2S'                         ,                          k_OX_H2S) !Model constant no 267 : Hydrogen sulphide oxidation rate constant at 20 C
    call para_get_value('THETA_k_OX_H2S'                   ,                    THETA_k_OX_H2S) !Model constant no 268 : Arhenius type temperature correction for sulphide oxidation rate constant
    call para_get_value('k_HS_OX_H2S_DOXY'                 ,                  k_HS_OX_H2S_DOXY) !Model constant no 269 : Monod type Half saturation of hydrogen sulphide oxidation for dissolved oxygen
    call para_get_value('k_DISS_FE_II_20'                  ,                   k_DISS_FE_II_20) !Model constant no 270 : Dissolution rate constant for Fe II at 20 C
    call para_get_value('THETA_k_DISS_FE_II'               ,                THETA_k_DISS_FE_II) !Model constant no 271 : Dissolution rate constant for Fe II
    call para_get_value('INIT_MULT_FE_II_DISS'             ,              INIT_MULT_FE_II_DISS) !Model constant no 272 : Initial fraction of dissolved Fe II
    call para_get_value('k_DISS_FE_III_20'                 ,                  k_DISS_FE_III_20) !Model constant no 273 : Dissolution rate constant for Fe III at 20 C
    call para_get_value('THETA_k_DISS_FE_III'              ,               THETA_k_DISS_FE_III) !Model constant no 274 : Dissolution rate constant for Fe III
    call para_get_value('INIT_MULT_FE_III_DISS'            ,             INIT_MULT_FE_III_DISS) !Model constant no 275 : Initial fraction of dissolved Fe III                                                                                                                               K_A
    call para_get_value('KG_NOST_VEG_HET_OPT_TEMP'         ,          KG_NOST_VEG_HET_OPT_TEMP) !Model constant no 276 : Nostocales (veg + het) Growth rate constant
    call para_get_value('NOST_VEG_HET_OPT_TEMP_LR'         ,          NOST_VEG_HET_OPT_TEMP_LR) !Model constant no 277 : Nostocales (veg + het) optimal temperature lower range
    call para_get_value('NOST_VEG_HET_OPT_TEMP_UR'         ,          NOST_VEG_HET_OPT_TEMP_UR) !Model constant no 278 : Nostocales (veg + het) optimal temperature upper range
    call para_get_value('EFF_NOST_VEG_HET_GROWTH'          ,           EFF_NOST_VEG_HET_GROWTH) !Model constant no 279 : Nostocales (veg + het) Effective growth. (1-EG)*growth - losses for RESP and excretion
    call para_get_value('KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP', KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP) !Model constant no 280 : Nostocales (veg + het) Temperature correction for growth lower temperature
    call para_get_value('KAPPA_NOST_VEG_HET_OVER_OPT_TEMP' ,  KAPPA_NOST_VEG_HET_OVER_OPT_TEMP) !Model constant no 281 : Nostocales (veg + het) Temperature correction for growth upper temperature
    call para_get_value('KR_NOST_VEG_HET_20'               ,                KR_NOST_VEG_HET_20) !Model constant no 282 : Nostocales (veg + het) RESP rate constant
    call para_get_value('THETA_KR_NOST_VEG_HET'            ,             THETA_KR_NOST_VEG_HET) !Model constant no 283 : Nostocales (veg + het) Temperature correction for RESP rate
    call para_get_value('KD_NOST_VEG_HET_20'               ,                KD_NOST_VEG_HET_20) !Model constant no 284 : Nostocales (veg + het) Mortality rate constant
    call para_get_value('THETA_KD_NOST_VEG_HET'            ,             THETA_KD_NOST_VEG_HET) !Model constant no 285 : Nostocales (veg + het) Temperature correction for Mortality rate
    call para_get_value('M_DENS_VEG_HET'                   ,                    M_DENS_VEG_HET) !Model constant no 286 : Nostocales (veg + het) mortality rate constant because of too dense population
    call para_get_value('KHS_DP_NOST_VEG_HET'              ,               KHS_DP_NOST_VEG_HET) !Model constant no 287 : Nostocales (veg + het) Half saturation growth for DP
    call para_get_value('KHS_O2_NOST_VEG_HET'              ,               KHS_O2_NOST_VEG_HET) !Model constant no 288 : Nostocales (veg + het) Half saturation growth for O2
    call para_get_value('FRAC_NOST_VEG_HET_EXCR'           ,            FRAC_NOST_VEG_HET_EXCR) !Model constant no 289 : Nostocales (veg + het) Fraction of excretion in metabolism rate
    call para_get_value('I_S_NOST_VEG_HET'                 ,                  I_S_NOST_VEG_HET) !Model constant no 290 : Nostocales (veg + het) Light saturation (langleys). Not used, is calculated!
    call para_get_value('DO_STR_HYPOX_NOST_VEG_HET_D'      ,       DO_STR_HYPOX_NOST_VEG_HET_D) !Model constant no 291 : Nostocales (veg + het) Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_get_value('THETA_HYPOX_NOST_VEG_HET_D'       ,        THETA_HYPOX_NOST_VEG_HET_D) !Model constant no 292 : Nostocales (veg + het) Multiplier of the exponent for Dissolved oxygen stress
    call para_get_value('EXPON_HYPOX_NOST_VEG_HET_D'       ,        EXPON_HYPOX_NOST_VEG_HET_D) !Model constant no 293 : Nostocales (veg + het) Exponent constant for Dissolved oxygen stress
    call para_get_value('NOST_N_TO_C'                      ,                       NOST_N_TO_C) !Model constant no 294 : Nostocales (veg + het) Nitrogen to Carbon ratio
    call para_get_value('NOST_P_TO_C'                      ,                       NOST_P_TO_C) !Model constant no 295 : Nostocales (veg + het) Phosphorus to Carbon ratio
    call para_get_value('NOST_O2_TO_C'                     ,                      NOST_O2_TO_C) !Model constant no 296 : Nostocales (veg + het) Oxygen to Carbon ratio for respiration
    call para_get_value('NOST_C_TO_CHLA'                   ,                    NOST_C_TO_CHLA) !Model constant no 297 : Nostocales (veg + het) Carbon to Chlorophyl a ratio
    call para_get_value('P_GERM_AKI'                       ,                        P_GERM_AKI) !Model constant no 298 : Gerimination rate constant for akinets
    call para_get_value('N_GERM_AKI'                       ,                        N_GERM_AKI) !Model constant no 299 : DIN concentration to trigger gerimination (below that gerimination starts)
    call para_get_value('P_FORM_AKI'                       ,                        P_FORM_AKI) !Model constant no 300 : Formation rate constant for akinets
    call para_get_value('DAY_FORM_AKI'                     ,                      DAY_FORM_AKI) !Model constant no 301 :
    call para_get_value('T_FORM_AKI'                       ,                        T_FORM_AKI) !Model constant no 302 : Temperature to trigger formation (below that formation starts)
    call para_get_value('K_LOSS_AKI'                       ,                        K_LOSS_AKI) !Model constant no 303 : Loss rate constant for akinetes
    call para_get_value('K_MORT_AKI_20'                    ,                     K_MORT_AKI_20) !Model constant no 304 : Decompositon rate constant of akinetes at 20 degrees celcisus
    call para_get_value('THETA_K_MORT_AKI'                 ,                  THETA_K_MORT_AKI) !Model constant no 305 : Arhenius temperature correction factor for akinete decomposition rate constant
    call para_get_value('T_GERM_AKI'                       ,                        T_GERM_AKI) !Model constant no 306 : Temperature where the gerimination of akinetes is initiated
    call para_get_value('KHS_POC_DISS_SAT'                 ,                  KHS_POC_DISS_SAT) !Model constant no 307 :
    call para_get_value('KHS_PON_DISS_SAT'                 ,                  KHS_PON_DISS_SAT) !Model constant no 308 :
    call para_get_value('KHS_POP_DISS_SAT'                 ,                  KHS_POP_DISS_SAT) !Model constant no 309 :
    call para_get_value('frac_avail_DON'                   ,                    frac_avail_DON) !Model constant no 310 :
    call para_get_value('frac_avail_DOP'                   ,                    frac_avail_DOP) !Model constant no 311 :
    call para_get_value('frac_avail_DON_NOST'              ,               frac_avail_DON_NOST) !Model constant no 312 :
    call para_get_value('KHS_DN_NOST_VEG_HET'              ,               KHS_DN_NOST_VEG_HET) !Model constant no 313 :
    call para_get_value('FRAC_FIX_N_FOR_GR_VEG_HET'        ,         FRAC_FIX_N_FOR_GR_VEG_HET) !Model constant no 314 :
    call para_get_value('FRAC_NOST_GROWTH'                 ,                  FRAC_NOST_GROWTH) !Model constant no 315 :
    call para_get_value('K_MIN_PHYT_AMIN_DOC'              ,               K_MIN_PHYT_AMIN_DOC) !Model constant no 316 :
    call para_get_value('K_MIN_PHYT_AMIN_DON'              ,               K_MIN_PHYT_AMIN_DON) !Model constant no 317 :
    call para_get_value('K_MIN_PHYT_AMIN_DOP'              ,               K_MIN_PHYT_AMIN_DOP) !Model constant no 318 :
    call para_get_value('BETA_DIA'                         ,                          BETA_DIA) !Model constant no 319 : Diatoms photoinhibition parameter
    call para_get_value('BETA_CYN'                         ,                          BETA_CYN) !Model constant no 320 : Non-fixing cyanobacteria photoinhibition parameter
    call para_get_value('BETA_FIX_CYN'                     ,                      BETA_FIX_CYN) !Model constant no 321 : Fixing cyanobacteria photoinhibition parameter
    call para_get_value('BETA_OPA'                         ,                          BETA_OPA) !Model constant no 322 : OtherPhyto photoinhibition parameter
    call para_get_value('BETA_NOST_VEG_HET'                ,                 BETA_NOST_VEG_HET) !Model constant no 323 : Nostocales photoinhibition parameter
    ! Optional: setups predating the ice-light coupling do not carry this name, and
    ! para_get_value hard-stops on a missing key. Absent => 1.0 (no attenuation).
    if (para_index_value('ICE_LIGHT_TRANS') > 0) then
        call para_get_value('ICE_LIGHT_TRANS', ICE_LIGHT_TRANS) !Model constant no 324
    else
        ICE_LIGHT_TRANS = 1.0D0
    end if

    call VALIDATE_PELAGIC_MODEL_CONSTANTS()

end subroutine INIT_PELAGIC_MODEL_CONSTANTS

!--------------------------------------------------------------------------

subroutine VALIDATE_PELAGIC_MODEL_CONSTANTS

    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use AQUABC_II_GLOBAL

    implicit none

    integer :: n_fixes
    real(kind = DBL_PREC), parameter :: KHS_MIN = 1.0D-6
    real(kind = DBL_PREC), parameter :: RATIO_MIN = 1.0D-6

    n_fixes = 0

    ! ------------------------------------------------------------------
    ! Check half-saturation constants > 0
    ! A zero K_HS causes division by zero in Monod: S/(S+K_HS)
    ! ------------------------------------------------------------------
    call CHK_POS(KHS_DIN_DIA,          'KHS_DIN_DIA',          n_fixes)
    call CHK_POS(KHS_DIP_DIA,          'KHS_DIP_DIA',          n_fixes)
    call CHK_POS(KHS_DSi_DIA,          'KHS_DSi_DIA',          n_fixes)
    call CHK_POS(KHS_O2_DIA,           'KHS_O2_DIA',           n_fixes)
    call CHK_POS(KHS_DIN_CYN,          'KHS_DIN_CYN',          n_fixes)
    call CHK_POS(KHS_DIP_CYN,          'KHS_DIP_CYN',          n_fixes)
    call CHK_POS(KHS_O2_CYN,           'KHS_O2_CYN',           n_fixes)
    call CHK_POS(KHS_DIN_FIX_CYN,      'KHS_DIN_FIX_CYN',      n_fixes)
    call CHK_POS(KHS_DIP_FIX_CYN,      'KHS_DIP_FIX_CYN',      n_fixes)
    call CHK_POS(KHS_O2_FIX_CYN,       'KHS_O2_FIX_CYN',       n_fixes)
    call CHK_POS(KHS_DIN_OPA,          'KHS_DIN_OPA',          n_fixes)
    call CHK_POS(KHS_DIP_OPA,          'KHS_DIP_OPA',          n_fixes)
    call CHK_POS(KHS_O2_OPA,           'KHS_O2_OPA',           n_fixes)
    call CHK_POS(KHS_DIA_C_ZOO,        'KHS_DIA_C_ZOO',        n_fixes)
    call CHK_POS(KHS_CYN_C_ZOO,        'KHS_CYN_C_ZOO',        n_fixes)
    call CHK_POS(KHS_FIX_CYN_C_ZOO,    'KHS_FIX_CYN_C_ZOO',    n_fixes)
    call CHK_POS(KHS_NOST_VEG_HET_C_ZOO, &
                                        'KHS_NOST_VEG_HET_C_ZOO', n_fixes)
    call CHK_POS(KHS_OPA_C_ZOO,        'KHS_OPA_C_ZOO',        n_fixes)
    call CHK_POS(KHS_DET_PART_ORG_C_ZOO, &
                                        'KHS_DET_PART_ORG_C_ZOO', n_fixes)
    call CHK_POS(KHS_DISS_N,           'KHS_DISS_N',           n_fixes)
    call CHK_POS(KHS_DISS_P,           'KHS_DISS_P',           n_fixes)
    call CHK_POS(KHS_AMIN_N,           'KHS_AMIN_N',           n_fixes)
    call CHK_POS(KHS_AMIN_P,           'KHS_AMIN_P',           n_fixes)
    call CHK_POS(KHS_NITR_OXY,         'KHS_NITR_OXY',         n_fixes)
    call CHK_POS(KHS_NITR_NH4_N,       'KHS_NITR_NH4_N',       n_fixes)
    call CHK_POS(KHS_DOXY_FE_III_RED,  'KHS_DOXY_FE_III_RED',  n_fixes)
    call CHK_POS(KHS_DOXY_MN_IV_RED,   'KHS_DOXY_MN_IV_RED',   n_fixes)
    call CHK_POS(K_HS_DOC_MIN_DOXY,    'K_HS_DOC_MIN_DOXY',    n_fixes)
    call CHK_POS(K_HS_DOC_MIN_NO3N,    'K_HS_DOC_MIN_NO3N',    n_fixes)
    call CHK_POS(K_HS_DOC_MIN_MN_IV,   'K_HS_DOC_MIN_MN_IV',   n_fixes)
    call CHK_POS(K_HS_DOC_MIN_FE_III,  'K_HS_DOC_MIN_FE_III',  n_fixes)
    call CHK_POS(K_HS_DOC_MIN_S_PLUS_6, &
                                        'K_HS_DOC_MIN_S_PLUS_6', n_fixes)
    call CHK_POS(K_HS_DOC_MIN_DOC,     'K_HS_DOC_MIN_DOC',     n_fixes)
    call CHK_POS(K_HS_DOXY_RED_LIM,    'K_HS_DOXY_RED_LIM',    n_fixes)
    call CHK_POS(K_HS_NO3N_RED_LIM,    'K_HS_NO3N_RED_LIM',    n_fixes)
    call CHK_POS(K_HS_MN_IV_RED_LIM,   'K_HS_MN_IV_RED_LIM',   n_fixes)
    call CHK_POS(K_HS_FE_III_RED_LIM,  'K_HS_FE_III_RED_LIM',  n_fixes)
    call CHK_POS(K_HS_S_PLUS_6_RED_LIM, &
                                        'K_HS_S_PLUS_6_RED_LIM', n_fixes)
    call CHK_POS(K_HS_DOXY_RED_INHB,   'K_HS_DOXY_RED_INHB',   n_fixes)
    call CHK_POS(K_HS_NO3N_RED_INHB,   'K_HS_NO3N_RED_INHB',   n_fixes)
    call CHK_POS(K_HS_MN_IV_RED_INHB,  'K_HS_MN_IV_RED_INHB',  n_fixes)
    call CHK_POS(K_HS_FE_III_RED_INHB, 'K_HS_FE_III_RED_INHB', n_fixes)
    call CHK_POS(K_HS_S_PLUS_6_RED_INHB, &
                                        'K_HS_S_PLUS_6_RED_INHB', n_fixes)
    call CHK_POS(K_HS_DON_MIN_DOXY,    'K_HS_DON_MIN_DOXY',    n_fixes)
    call CHK_POS(K_HS_DON_MIN_NO3N,    'K_HS_DON_MIN_NO3N',    n_fixes)
    call CHK_POS(K_HS_DON_MIN_MN_IV,   'K_HS_DON_MIN_MN_IV',   n_fixes)
    call CHK_POS(K_HS_DON_MIN_FE_III,  'K_HS_DON_MIN_FE_III',  n_fixes)
    call CHK_POS(K_HS_DON_MIN_S_PLUS_6, &
                                        'K_HS_DON_MIN_S_PLUS_6', n_fixes)
    call CHK_POS(K_HS_DON_MIN_DOC,     'K_HS_DON_MIN_DOC',     n_fixes)
    call CHK_POS(K_HS_DOP_MIN_DOXY,    'K_HS_DOP_MIN_DOXY',    n_fixes)
    call CHK_POS(K_HS_DOP_MIN_NO3N,    'K_HS_DOP_MIN_NO3N',    n_fixes)
    call CHK_POS(K_HS_DOP_MIN_MN_IV,   'K_HS_DOP_MIN_MN_IV',   n_fixes)
    call CHK_POS(K_HS_DOP_MIN_FE_III,  'K_HS_DOP_MIN_FE_III',  n_fixes)
    call CHK_POS(K_HS_DOP_MIN_S_PLUS_6, &
                                        'K_HS_DOP_MIN_S_PLUS_6', n_fixes)
    call CHK_POS(K_HS_DOP_MIN_DOC,     'K_HS_DOP_MIN_DOC',     n_fixes)
    call CHK_POS(k_HS_OX_CH4_DOXY,     'k_HS_OX_CH4_DOXY',     n_fixes)
    call CHK_POS(k_HS_OX_H2S_DOXY,     'k_HS_OX_H2S_DOXY',     n_fixes)
    call CHK_POS(KHS_DP_NOST_VEG_HET,  'KHS_DP_NOST_VEG_HET',  n_fixes)
    call CHK_POS(KHS_O2_NOST_VEG_HET,  'KHS_O2_NOST_VEG_HET',  n_fixes)
    call CHK_POS(KHS_DN_NOST_VEG_HET,  'KHS_DN_NOST_VEG_HET',  n_fixes)
    call CHK_POS(KHS_POC_DISS_SAT,     'KHS_POC_DISS_SAT',     n_fixes)
    call CHK_POS(KHS_PON_DISS_SAT,     'KHS_PON_DISS_SAT',     n_fixes)
    call CHK_POS(KHS_POP_DISS_SAT,     'KHS_POP_DISS_SAT',     n_fixes)

    ! ------------------------------------------------------------------
    ! Check the temperature-response constants against the SELECTED model.
    ! The same slots mean different things (see GROWTH_AT_TEMP):
    !   CTMI    : OPT_TEMP_LR = T_min, OPT_TEMP_UR = T_opt, KAPPA_*_OVER = T_max (degC)
    !   plateau : OPT_TEMP_LR / _UR = plateau edges, KAPPA_*_OVER = decay rate (1/degC)
    ! so the T_max rules apply under CTMI only. Under the plateau model a decay
    ! rate (e.g. 0.07) is valid and must not be turned into a T_max; a T_max-like
    ! value there is reported but not changed, since the intended rate is unknown.
    ! ------------------------------------------------------------------
    call CHK_TEMP(DIA_OPT_TEMP_LR, DIA_OPT_TEMP_UR, KAPPA_DIA_OVER_OPT_TEMP, &
                  'DIA', 35.0D0, n_fixes)
    call CHK_TEMP(CYN_OPT_TEMP_LR, CYN_OPT_TEMP_UR, KAPPA_CYN_OVER_OPT_TEMP, &
                  'CYN', 38.0D0, n_fixes)
    call CHK_TEMP(FIX_CYN_OPT_TEMP_LR, FIX_CYN_OPT_TEMP_UR, KAPPA_FIX_CYN_OVER_OPT_TEMP, &
                  'FIX_CYN', 38.0D0, n_fixes)
    call CHK_TEMP(OPA_OPT_TEMP_LR, OPA_OPT_TEMP_UR, KAPPA_OPA_OVER_OPT_TEMP, &
                  'OPA', 33.0D0, n_fixes)
    call CHK_TEMP(ZOO_OPT_TEMP_LR, ZOO_OPT_TEMP_UR, KAPPA_ZOO_OVER_OPT_TEMP, &
                  'ZOO', 35.0D0, n_fixes)
    call CHK_TEMP(NOST_VEG_HET_OPT_TEMP_LR, NOST_VEG_HET_OPT_TEMP_UR, &
                  KAPPA_NOST_VEG_HET_OVER_OPT_TEMP, 'NOST_VEG_HET', 38.0D0, n_fixes)

    ! ------------------------------------------------------------------
    ! Check stoichiometric ratios > 0
    ! ------------------------------------------------------------------
    call CHK_RATIO(DIA_N_TO_C,      'DIA_N_TO_C',      n_fixes)
    call CHK_RATIO(DIA_P_TO_C,      'DIA_P_TO_C',      n_fixes)
    call CHK_RATIO(DIA_Si_TO_C,     'DIA_Si_TO_C',     n_fixes)
    call CHK_RATIO(DIA_O2_TO_C,     'DIA_O2_TO_C',     n_fixes)
    call CHK_RATIO(CYN_N_TO_C,      'CYN_N_TO_C',      n_fixes)
    call CHK_RATIO(CYN_P_TO_C,      'CYN_P_TO_C',      n_fixes)
    call CHK_RATIO(CYN_O2_TO_C,     'CYN_O2_TO_C',     n_fixes)
    call CHK_RATIO(FIX_CYN_N_TO_C,  'FIX_CYN_N_TO_C',  n_fixes)
    call CHK_RATIO(FIX_CYN_P_TO_C,  'FIX_CYN_P_TO_C',  n_fixes)
    call CHK_RATIO(FIX_CYN_O2_TO_C, 'FIX_CYN_O2_TO_C', n_fixes)
    call CHK_RATIO(OPA_N_TO_C,      'OPA_N_TO_C',      n_fixes)
    call CHK_RATIO(OPA_P_TO_C,      'OPA_P_TO_C',      n_fixes)
    call CHK_RATIO(OPA_O2_TO_C,     'OPA_O2_TO_C',     n_fixes)
    call CHK_RATIO(ZOO_N_TO_C,      'ZOO_N_TO_C',      n_fixes)
    call CHK_RATIO(ZOO_P_TO_C,      'ZOO_P_TO_C',      n_fixes)
    call CHK_RATIO(ZOO_O2_TO_C,     'ZOO_O2_TO_C',     n_fixes)
    call CHK_RATIO(NOST_N_TO_C,     'NOST_N_TO_C',     n_fixes)
    call CHK_RATIO(NOST_P_TO_C,     'NOST_P_TO_C',     n_fixes)
    call CHK_RATIO(NOST_O2_TO_C,    'NOST_O2_TO_C',    n_fixes)

    ! ------------------------------------------------------------------
    ! Check photoinhibition beta >= 0
    ! ------------------------------------------------------------------
    call CHK_NONNEG(BETA_DIA,          'BETA_DIA',          n_fixes)
    call CHK_NONNEG(BETA_CYN,          'BETA_CYN',          n_fixes)
    call CHK_NONNEG(BETA_FIX_CYN,      'BETA_FIX_CYN',      n_fixes)
    call CHK_NONNEG(BETA_OPA,          'BETA_OPA',          n_fixes)
    call CHK_NONNEG(BETA_NOST_VEG_HET, 'BETA_NOST_VEG_HET', n_fixes)

    if (ICE_LIGHT_TRANS < 0.0D0 .or. ICE_LIGHT_TRANS > 1.0D0) then
        write(*,*) 'WARNING: ICE_LIGHT_TRANS outside [0,1]: ', ICE_LIGHT_TRANS, trim(ACTION('clamping'))
        if (.not. VALIDATE_CONSTANTS_REPORT_ONLY) &
            ICE_LIGHT_TRANS = min(1.0D0, max(0.0D0, ICE_LIGHT_TRANS))
        n_fixes = n_fixes + 1
    end if

    ! ------------------------------------------------------------------
    ! Summary
    ! ------------------------------------------------------------------
    if (n_fixes > 0) then
        write(*,*) '======================================='
        if (VALIDATE_CONSTANTS_REPORT_ONLY) then
            write(*,*) 'VALIDATE_PELAGIC_MODEL_CONSTANTS:', &
                       ' found', n_fixes, 'problem(s); values left as read'
        else
            write(*,*) 'VALIDATE_PELAGIC_MODEL_CONSTANTS:', &
                       ' corrected', n_fixes, 'parameter(s)'
        end if
        write(*,*) '======================================='
    end if

contains

    ! Text appended to a warning: what happens to the value.
    function ACTION(what) result(txt)
        implicit none
        character(len=*), intent(in) :: what
        character(len=64) :: txt
        if (VALIDATE_CONSTANTS_REPORT_ONLY) then
            txt = ' (report only, left unchanged)'
        else
            txt = ', ' // what
        end if
    end function ACTION

    subroutine CHK_TEMP(t_lr, t_ur, kappa_over, group, tmax_default, nfix)
        implicit none
        real(kind = DBL_PREC), intent(inout) :: t_lr, t_ur, kappa_over
        character(len=*), intent(in) :: group
        real(kind = DBL_PREC), intent(in) :: tmax_default
        integer, intent(inout) :: nfix
        real(kind = DBL_PREC) :: swap

        if (USE_CTMI_TEMP .and. kappa_over < 1.0D0) then
            write(*,*) 'WARNING: KAPPA_', group, '_OVER_OPT_TEMP =', kappa_over, &
                       ' looks like a plateau decay rate, but CTMI reads it as T_max', &
                       trim(ACTION('setting T_max to')), tmax_default
            if (.not. VALIDATE_CONSTANTS_REPORT_ONLY) kappa_over = tmax_default
            nfix = nfix + 1
        end if

        ! Lower edge / T_min must lie below upper edge / T_opt in both models
        if (t_lr >= t_ur) then
            write(*,*) 'WARNING: ', group, '_OPT_TEMP_LR >= ', group, '_OPT_TEMP_UR', &
                       trim(ACTION('swapping'))
            if (.not. VALIDATE_CONSTANTS_REPORT_ONLY) then
                swap = t_lr
                t_lr = t_ur
                t_ur = swap
            end if
            nfix = nfix + 1
        end if

        if (USE_CTMI_TEMP) then
            if (t_ur >= kappa_over) then
                write(*,*) 'WARNING: ', group, ' T_opt >= T_max', &
                           trim(ACTION('setting T_max = T_opt + 10'))
                if (.not. VALIDATE_CONSTANTS_REPORT_ONLY) kappa_over = t_ur + 10.0D0
                nfix = nfix + 1
            end if
        else if (kappa_over >= 1.0D0) then
            ! Never repaired: the plateau decay rate the user meant is unknown.
            write(*,*) 'WARNING: KAPPA_', group, '_OVER_OPT_TEMP =', kappa_over, &
                       ' looks like a CTMI T_max, but the plateau model reads it as a', &
                       ' decay rate per degC (growth ~0 just above the plateau).', &
                       ' Set TEMPERATURE_MODEL = 1 or give a decay rate. Left unchanged.'
            nfix = nfix + 1
        end if
    end subroutine CHK_TEMP

    subroutine CHK_NONNEG(val, name, nfix)
        implicit none
        real(kind = DBL_PREC), intent(inout) :: val
        character(len=*), intent(in) :: name
        integer, intent(inout) :: nfix
        if (val < 0.0D0) then
            write(*,*) 'WARNING: ', trim(name), ' < 0', trim(ACTION('setting to 0'))
            if (.not. VALIDATE_CONSTANTS_REPORT_ONLY) val = 0.0D0
            nfix = nfix + 1
        end if
    end subroutine CHK_NONNEG

    subroutine CHK_POS(val, name, nfix)
        implicit none
        real(kind = DBL_PREC), intent(inout) :: val
        character(len=*), intent(in) :: name
        integer, intent(inout) :: nfix
        if (val <= 0.0D0) then
            if (VALIDATE_CONSTANTS_REPORT_ONLY) then
                write(*,*) 'WARNING: ', trim(name), ' <= 0', trim(ACTION(''))
            else
                write(*,*) 'WARNING: ', trim(name), ' <= 0, setting to', KHS_MIN
            end if
            if (.not. VALIDATE_CONSTANTS_REPORT_ONLY) val = KHS_MIN
            nfix = nfix + 1
        end if
    end subroutine CHK_POS

    subroutine CHK_RATIO(val, name, nfix)
        implicit none
        real(kind = DBL_PREC), intent(inout) :: val
        character(len=*), intent(in) :: name
        integer, intent(inout) :: nfix
        if (val <= 0.0D0) then
            if (VALIDATE_CONSTANTS_REPORT_ONLY) then
                write(*,*) 'WARNING: ', trim(name), ' <= 0', trim(ACTION(''))
            else
                write(*,*) 'WARNING: ', trim(name), ' <= 0, setting to', RATIO_MIN
            end if
            if (.not. VALIDATE_CONSTANTS_REPORT_ONLY) val = RATIO_MIN
            nfix = nfix + 1
        end if
    end subroutine CHK_RATIO

end subroutine VALIDATE_PELAGIC_MODEL_CONSTANTS

!--------------------------------------------------------------------------

subroutine INSERT_PELAGIC_MODEL_CONSTANTS

    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use para_aqua

    implicit none

    call para_insert_value('K_A'                              ,                               K_A) !Model constant no   1 : Aeration coefficient (if negative calculates internally)
    call para_insert_value('THETA_K_A'                        ,                         THETA_K_A) !Model constant no   2 : Temperature correction factor for aeration
    call para_insert_value('XKC'                              ,                               XKC) !Model constant no   3 : Light extinction per chlorophyl unit,( mcg Chla/l/m) to calculate light saturation for Smith
    call para_insert_value('PHIMX'                            ,                             PHIMX) !Model constant no   4 : Quantum yield const. mg C/mole photon
    call para_insert_value('KG_DIA_OPT_TEMP'                  ,                   KG_DIA_OPT_TEMP) !Model constant no   5 : Diatoms Growth rate
    call para_insert_value('DIA_OPT_TEMP_LR'                  ,                   DIA_OPT_TEMP_LR) !Model constant no   6 : Diatoms optimal temperature lower range
    call para_insert_value('DIA_OPT_TEMP_UR'                  ,                   DIA_OPT_TEMP_UR) !Model constant no   7 : Diatoms optimal temperature upper range
    call para_insert_value('EFF_DIA_GROWTH'                   ,                    EFF_DIA_GROWTH) !Model constant no   8 : Diatoms Effective growth. (1-EG)*growth - losses for respiration and excretion
    call para_insert_value('KAPPA_DIA_UNDER_OPT_TEMP'         ,          KAPPA_DIA_UNDER_OPT_TEMP) !Model constant no   9 : Diatoms Temperature correction for growth lower temperature
    call para_insert_value('KAPPA_DIA_OVER_OPT_TEMP'          ,           KAPPA_DIA_OVER_OPT_TEMP) !Model constant no  10 : Diatoms Temperature correction for growth upper temperature
    call para_insert_value('KR_DIA_20'                        ,                         KR_DIA_20) !Model constant no  11 : Diatoms Respiration rate
    call para_insert_value('THETA_KR_DIA'                     ,                      THETA_KR_DIA) !Model constant no  12 : Diatoms Temperature correction for basal respiration rate
    call para_insert_value('KD_DIA_20'                        ,                         KD_DIA_20) !Model constant no  13 : Diatoms Mortality rate
    call para_insert_value('THETA_KD_DIA'                     ,                      THETA_KD_DIA) !Model constant no  14 : Diatoms Temperature correction for Mortality rate
    call para_insert_value('KHS_DIN_DIA'                      ,                       KHS_DIN_DIA) !Model constant no  15 : Diatoms Half saturation growth for DIN
    call para_insert_value('KHS_DIP_DIA'                      ,                       KHS_DIP_DIA) !Model constant no  16 : Diatoms Half saturation growth for DIP
    call para_insert_value('KHS_DSi_DIA'                      ,                       KHS_DSi_DIA) !Model constant no  17 : Diatoms Half saturation growth for DSi
    call para_insert_value('KHS_O2_DIA'                       ,                        KHS_O2_DIA) !Model constant no  18 : Diatoms Half saturation growth for O2
    call para_insert_value('FRAC_DIA_EXCR'                    ,                     FRAC_DIA_EXCR) !Model constant no  19 : Diatoms Fraction of excretion in metabolism rate
    call para_insert_value('I_S_DIA'                          ,                           I_S_DIA) !Model constant no  20 : Diatoms Light saturation (langleys)
    call para_insert_value('DO_STR_HYPOX_DIA_D'               ,                DO_STR_HYPOX_DIA_D) !Model constant no  21 : Diatoms Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_insert_value('THETA_HYPOX_DIA_D'                ,                 THETA_HYPOX_DIA_D) !Model constant no  22 : Diatoms Multiplier of the exponent for Dissolved oxygen stress
    call para_insert_value('EXPON_HYPOX_DIA_D'                ,                 EXPON_HYPOX_DIA_D) !Model constant no  23 : Diatoms Exponent constant for Dissolved oxygen stress
    call para_insert_value('DIA_N_TO_C'                       ,                        DIA_N_TO_C) !Model constant no  24 : Diatoms Nitrogen to Carbon ratio
    call para_insert_value('DIA_P_TO_C'                       ,                        DIA_P_TO_C) !Model constant no  25 : Diatoms Phosphorus to Carbon ratio
    call para_insert_value('DIA_Si_TO_C'                      ,                       DIA_Si_TO_C) !Model constant no  26 : Diatoms Silica to Carbon ratio
    call para_insert_value('DIA_O2_TO_C'                      ,                       DIA_O2_TO_C) !Model constant no  27 : Diatoms Oxygen to Carbon ratio for respiration
    call para_insert_value('DIA_C_TO_CHLA'                    ,                     DIA_C_TO_CHLA) !Model constant no  28 : Diatoms Carbon to Chlorophil a ratio
    call para_insert_value('KG_CYN_OPT_TEMP'                  ,                   KG_CYN_OPT_TEMP) !Model constant no  29 : Non-fixing cyanobacteria Growth rate
    call para_insert_value('CYN_OPT_TEMP_LR'                  ,                   CYN_OPT_TEMP_LR) !Model constant no  30 : Non-fixing cyanobacteria optimal temperature lower range
    call para_insert_value('CYN_OPT_TEMP_UR'                  ,                   CYN_OPT_TEMP_UR) !Model constant no  31 : Non-fixing cyanobacteria optimal temperature upper range
    call para_insert_value('EFF_CYN_GROWTH'                   ,                    EFF_CYN_GROWTH) !Model constant no  32 : Non-fixing cyanobacteria Effective growth. (1-EG)*growth - losses for respiration and excretion
    call para_insert_value('KAPPA_CYN_UNDER_OPT_TEMP'         ,          KAPPA_CYN_UNDER_OPT_TEMP) !Model constant no  33 : Non-fixing cyanobacteria Temperature correction for growth lower temperature
    call para_insert_value('KAPPA_CYN_OVER_OPT_TEMP'          ,           KAPPA_CYN_OVER_OPT_TEMP) !Model constant no  34 : Non-fixing cyanobacteria Temperature correction for growth upper temperature
    call para_insert_value('KR_CYN_20'                        ,                         KR_CYN_20) !Model constant no  35 : Non-fixing cyanobacteria Respiration rate
    call para_insert_value('THETA_KR_CYN'                     ,                      THETA_KR_CYN) !Model constant no  36 : Non-fixing cyanobacteria Temperature correction for respiration rate
    call para_insert_value('KD_CYN_20'                        ,                         KD_CYN_20) !Model constant no  37 : Non-fixing cyanobacteria Mortality rate
    call para_insert_value('THETA_KD_CYN'                     ,                      THETA_KD_CYN) !Model constant no  38 : Non-fixing cyanobacteria Temperature correction for Mortality rate
    call para_insert_value('KHS_DIN_CYN'                      ,                       KHS_DIN_CYN) !Model constant no  39 : Non-fixing cyanobacteria Half saturation growth for DIN
    call para_insert_value('KHS_DIP_CYN'                      ,                       KHS_DIP_CYN) !Model constant no  40 : Non-fixing cyanobacteria Half saturation growth for DIP
    call para_insert_value('KHS_O2_CYN'                       ,                        KHS_O2_CYN) !Model constant no  41 : Non-fixing cyanobacteria Half saturation growth for O2
    call para_insert_value('FRAC_CYN_EXCR'                    ,                     FRAC_CYN_EXCR) !Model constant no  42 : Non-fixing cyanobacteria Fraction of excretion in metabolism rate
    call para_insert_value('I_S_CYN'                          ,                           I_S_CYN) !Model constant no  43 : Non-fixing cyanobacteria Light saturation (langleys).Not used, is calculated!
    call para_insert_value('DO_STR_HYPOX_CYN_D'               ,                DO_STR_HYPOX_CYN_D) !Model constant no  44 : Non-fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_insert_value('THETA_HYPOX_CYN_D'                ,                 THETA_HYPOX_CYN_D) !Model constant no  45 : Non-fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress
    call para_insert_value('EXPON_HYPOX_CYN_D'                ,                 EXPON_HYPOX_CYN_D) !Model constant no  46 : Non-fixing cyanobacteria Exponent constant for Dissolved oxygen stress
    call para_insert_value('CYN_N_TO_C'                       ,                        CYN_N_TO_C) !Model constant no  47 : Non-fixing cyanobacteria Nitrogen to Carbon ratio ,was 0.1
    call para_insert_value('CYN_P_TO_C'                       ,                        CYN_P_TO_C) !Model constant no  48 : Non-fixing cyanobacteria Phosphorus to Carbon ratio
    call para_insert_value('CYN_O2_TO_C'                      ,                       CYN_O2_TO_C) !Model constant no  49 : Non-fixing cyanobacteria Oxygen to Carbon ratio for respiration
    call para_insert_value('CYN_C_TO_CHLA'                    ,                     CYN_C_TO_CHLA) !Model constant no  50 : Non-fixing cyanobacteria Carbon to Chlorophyl a ratio
    call para_insert_value('KG_FIX_CYN_OPT_TEMP'              ,               KG_FIX_CYN_OPT_TEMP) !Model constant no  51 : Fixing cyanobacteria Growth rate constant
    call para_insert_value('FIX_CYN_OPT_TEMP_LR'              ,               FIX_CYN_OPT_TEMP_LR) !Model constant no  52 : Fixing Cyanobacteria optimal temperature lower range
    call para_insert_value('FIX_CYN_OPT_TEMP_UR'              ,               FIX_CYN_OPT_TEMP_UR) !Model constant no  53 : Fixing Cyanobacteria optimal temperature upper range
    call para_insert_value('EFF_FIX_CYN_GROWTH'               ,                EFF_FIX_CYN_GROWTH) !Model constant no  54 : Fixing cyanobacteria Effective growth. (1-EG)*growth - losses for RESP and excretion
    call para_insert_value('KAPPA_FIX_CYN_UNDER_OPT_TEMP'     ,      KAPPA_FIX_CYN_UNDER_OPT_TEMP) !Model constant no  55 : Fixing cyanobacteria Temperature correction for growth lower temperature
    call para_insert_value('KAPPA_FIX_CYN_OVER_OPT_TEMP'      ,       KAPPA_FIX_CYN_OVER_OPT_TEMP) !Model constant no  56 : Fixing cyanobacteria Temperature correction for growth upper temperature
    call para_insert_value('KR_FIX_CYN_20'                    ,                     KR_FIX_CYN_20) !Model constant no  57 : Fixing cyanobacteria RESP rate constant
    call para_insert_value('THETA_KR_FIX_CYN'                 ,                  THETA_KR_FIX_CYN) !Model constant no  58 : Fixing cyanobacteria Temperature correction for RESP rate
    call para_insert_value('KD_FIX_CYN_20'                    ,                     KD_FIX_CYN_20) !Model constant no  59 : Fixing cyanobacteria Mortality rate constant
    call para_insert_value('THETA_KD_FIX_CYN'                 ,                  THETA_KD_FIX_CYN) !Model constant no  60 : Fixing cyanobacteria Temperature correction for Mortality rate
    call para_insert_value('KHS_DIN_FIX_CYN'                  ,                   KHS_DIN_FIX_CYN) !Model constant no  61 : Fixing cyanobacteria Half saturation growth for DIN
    call para_insert_value('KHS_DIP_FIX_CYN'                  ,                   KHS_DIP_FIX_CYN) !Model constant no  62 : Fixing cyanobacteria Half saturation growth for DIP
    call para_insert_value('KHS_O2_FIX_CYN'                   ,                    KHS_O2_FIX_CYN) !Model constant no  63 : Fixing cyanobacteria Half saturation growth for O2
    call para_insert_value('FRAC_FIX_CYN_EXCR'                ,                 FRAC_FIX_CYN_EXCR) !Model constant no  64 : Fixing cyanobacteria Fraction of excretion in metabolism rate
    call para_insert_value('I_S_FIX_CYN'                      ,                       I_S_FIX_CYN) !Model constant no  65 : Fixing cyanobacteria Light saturation (langleys). Not used, is calculated!
    call para_insert_value('DO_STR_HYPOX_FIX_CYN_D'           ,            DO_STR_HYPOX_FIX_CYN_D) !Model constant no  66 : Fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_insert_value('THETA_HYPOX_FIX_CYN_D'            ,             THETA_HYPOX_FIX_CYN_D) !Model constant no  67 : Fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress
    call para_insert_value('EXPON_HYPOX_FIX_CYN_D'            ,             EXPON_HYPOX_FIX_CYN_D) !Model constant no  68 : Fixing cyanobacteria Exponent constant for Dissolved oxygen stress
    call para_insert_value('FIX_CYN_N_TO_C'                   ,                    FIX_CYN_N_TO_C) !Model constant no  69 : Fixing cyanobacteria Nitrogen to Carbon ratio
    call para_insert_value('FIX_CYN_P_TO_C'                   ,                    FIX_CYN_P_TO_C) !Model constant no  70 : Fixing cyanobacteria Phosphorus to Carbon ratio
    call para_insert_value('FIX_CYN_O2_TO_C'                  ,                   FIX_CYN_O2_TO_C) !Model constant no  71 : Fixing cyanobacteria Oxygen to Carbon ratio for respiration
    call para_insert_value('FIX_CYN_C_TO_CHLA'                ,                 FIX_CYN_C_TO_CHLA) !Model constant no  72 : Fixing cyanobacteria Carbon to Chlorophyl a ratio
    call para_insert_value('R_FIX'                            ,                             R_FIX) !Model constant no  73 : Fixing cyanobacteria Ratio between non-fixing and fixing fractions growth rate
    call para_insert_value('K_FIX'                            ,                             K_FIX) !Model constant no  74 : Fixing cyanobacteria Effectivity parameter of switching to nitrogen fixation
    call para_insert_value('KG_OPA_OPT_TEMP'                  ,                   KG_OPA_OPT_TEMP) !Model constant no  75 : OtherPhyto Growth rate constant
    call para_insert_value('OPA_OPT_TEMP_LR'                  ,                   OPA_OPT_TEMP_LR) !Model constant no  76 : OtherPhyto optimal temperature lower range
    call para_insert_value('OPA_OPT_TEMP_UR'                  ,                   OPA_OPT_TEMP_UR) !Model constant no  77 : OtherPhyto optimal temperature upper range
    call para_insert_value('EFF_OPA_GROWTH'                   ,                    EFF_OPA_GROWTH) !Model constant no  78 : OtherPhyto Effective growth. (1-EG)*growth - losses for respiration and excretion
    call para_insert_value('KAPPA_OPA_UNDER_OPT_TEMP'         ,          KAPPA_OPA_UNDER_OPT_TEMP) !Model constant no  79 : OtherPhyto Temperature correction for growth lower temperature
    call para_insert_value('KAPPA_OPA_OVER_OPT_TEMP'          ,           KAPPA_OPA_OVER_OPT_TEMP) !Model constant no  80 : OtherPhyto Temperature correction for growth upper temperature
    call para_insert_value('KR_OPA_20'                        ,                         KR_OPA_20) !Model constant no  81 : OtherPhyto Respiration rate constant
    call para_insert_value('THETA_KR_OPA'                     ,                      THETA_KR_OPA) !Model constant no  82 : OtherPhyto Temperature correction for respiration rate
    call para_insert_value('KD_OPA_20'                        ,                         KD_OPA_20) !Model constant no  83 : OtherPhyto Mortality rate constant
    call para_insert_value('THETA_KD_OPA'                     ,                      THETA_KD_OPA) !Model constant no  84 : OtherPhyto Temperature correction for Mortality rate
    call para_insert_value('KHS_DIN_OPA'                      ,                       KHS_DIN_OPA) !Model constant no  85 : OtherPhyto Half saturation growth for DIN
    call para_insert_value('KHS_DIP_OPA'                      ,                       KHS_DIP_OPA) !Model constant no  86 : OtherPhyto Half saturation growth for DIP
    call para_insert_value('KHS_O2_OPA'                       ,                        KHS_O2_OPA) !Model constant no  87 : OtherPhyto Half saturation growth for O2
    call para_insert_value('FRAC_OPA_EXCR'                    ,                     FRAC_OPA_EXCR) !Model constant no  88 : OtherPhyto Fraction of excretion in metabolism rate
    call para_insert_value('I_S_OPA'                          ,                           I_S_OPA) !Model constant no  89 : OtherPhyto Light saturation (langleys). Not used, is calculated!
    call para_insert_value('DO_STR_HYPOX_OPA_D'               ,                DO_STR_HYPOX_OPA_D) !Model constant no  90 : OtherPhyto Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_insert_value('THETA_HYPOX_OPA_D'                ,                 THETA_HYPOX_OPA_D) !Model constant no  91 : OtherPhyto Multiplier of the exponent for Dissolved oxygen stress
    call para_insert_value('EXPON_HYPOX_OPA_D'                ,                 EXPON_HYPOX_OPA_D) !Model constant no  92 : OtherPhyto Exponent constant for Dissolved oxygen stress
    call para_insert_value('OPA_N_TO_C'                       ,                        OPA_N_TO_C) !Model constant no  93 : OtherPhyto Nitrogen to Carbon ratio
    call para_insert_value('OPA_P_TO_C'                       ,                        OPA_P_TO_C) !Model constant no  94 : OtherPhyto Phosphorus to Carbon ratio
    call para_insert_value('OPA_O2_TO_C'                      ,                       OPA_O2_TO_C) !Model constant no  95 : OtherPhyto Oxygen to Carbon ratio for respiration
    call para_insert_value('OPA_C_TO_CHLA'                    ,                     OPA_C_TO_CHLA) !Model constant no  96 : OtherPhyto Carbon to Chlorophyl a ratio
    call para_insert_value('KG_ZOO_OPT_TEMP'                  ,                   KG_ZOO_OPT_TEMP) !Model constant no  97 : Zooplankton Growth rate
    call para_insert_value('ZOO_OPT_TEMP_LR'                  ,                   ZOO_OPT_TEMP_LR) !Model constant no  98 : Zooplankton optimal temperature lower range
    call para_insert_value('ZOO_OPT_TEMP_UR'                  ,                   ZOO_OPT_TEMP_UR) !Model constant no  99 : Zooplankton optimal temperature upper range
    call para_insert_value('EFF_ZOO_GROWTH'                   ,                    EFF_ZOO_GROWTH) !Model constant no 100 : Zooplankton Effective growth. (1-EG)*growth - losses for respiration and excretion
    call para_insert_value('KAPPA_ZOO_UNDER_OPT_TEMP'         ,          KAPPA_ZOO_UNDER_OPT_TEMP) !Model constant no 101 : Zooplankton Temperature correction for growth lower temperature
    call para_insert_value('KAPPA_ZOO_OVER_OPT_TEMP'          ,           KAPPA_ZOO_OVER_OPT_TEMP) !Model constant no 102 : Zooplankton Temperature correction for growth upper temperature
    call para_insert_value('GRAT_ZOO_DIA'                     ,                      GRAT_ZOO_DIA) !Model constant no 103 : Zooplankton Grazing rate (growhth rate multiplier) on diatoms
    call para_insert_value('GRAT_ZOO_CYN'                     ,                      GRAT_ZOO_CYN) !Model constant no 104 : Zooplankton Grazing rate (growhth rate multiplier) on Cyanobacteria
    call para_insert_value('GRAT_ZOO_OPA'                     ,                      GRAT_ZOO_OPA) !Model constant no 105 : Zooplankton Grazing rate (growhth rate multiplier) on OtherPhyto
    call para_insert_value('GRAT_ZOO_FIX_CYN'                 ,                  GRAT_ZOO_FIX_CYN) !Model constant no 106 : Zooplankton Grazing rate (growhth rate multiplier) on fixing Cyanobacteria
    call para_insert_value('GRAT_ZOO_NOST_VEG_HET'            ,             GRAT_ZOO_NOST_VEG_HET) !Model constant no 107 : Zooplankton Grazing rate (growhth rate multiplier) on Nostocles (veg + het)
    call para_insert_value('GRAT_ZOO_DET_PART_ORG_C'          ,           GRAT_ZOO_DET_PART_ORG_C) !Model constant no 108 : Zooplankton Grazing rate (growhth rate multiplier) on part. ORG_C
    call para_insert_value('PREF_ZOO_DIA'                     ,                      PREF_ZOO_DIA) !Model constant no 109 : Zooplankton Preference for Diatoms
    call para_insert_value('PREF_ZOO_CYN'                     ,                      PREF_ZOO_CYN) !Model constant no 110 : Zooplankton Preference for Cyanobacteria
    call para_insert_value('PREF_ZOO_FIX_CYN'                 ,                  PREF_ZOO_FIX_CYN) !Model constant no 111 : Zooplankton Preference for fixing Cyanobacteria
    call para_insert_value('PREF_ZOO_NOST_VEG_HET'            ,             PREF_ZOO_NOST_VEG_HET) !Model constant no 112 : Zooplankton Preference for nostocales (veg + het)
    call para_insert_value('PREF_ZOO_OPA'                     ,                      PREF_ZOO_OPA) !Model constant no 113 : Zooplankton Preference for OtherPhyto
    call para_insert_value('PREF_ZOO_DET_PART_ORG_C'          ,           PREF_ZOO_DET_PART_ORG_C) !Model constant no 114 : Zooplankton Preference for Part. ORG_C
    call para_insert_value('KHS_DIA_C_ZOO'                    ,                     KHS_DIA_C_ZOO) !Model constant no 115 : Zooplankton Half saturation growth for diatoms
    call para_insert_value('KHS_CYN_C_ZOO'                    ,                     KHS_CYN_C_ZOO) !Model constant no 116 : Zooplankton Half saturation growth for Cyanobacteria
    call para_insert_value('KHS_FIX_CYN_C_ZOO'                ,                 KHS_FIX_CYN_C_ZOO) !Model constant no 117 : Zooplankton Half saturation growth for fixing Cyanobacteria
    call para_insert_value('KHS_NOST_VEG_HET_C_ZOO'           ,            KHS_NOST_VEG_HET_C_ZOO) !Model constant no 118 : Zooplankton Half saturation growth for Nostocales (veg + het)
    call para_insert_value('KHS_OPA_C_ZOO'                    ,                     KHS_OPA_C_ZOO) !Model constant no 119 : Zooplankton Half saturation growth for OtherPhyto
    call para_insert_value('KHS_DET_PART_ORG_C_ZOO'           ,            KHS_DET_PART_ORG_C_ZOO) !Model constant no 120 : Zooplankton Half saturation growth for part. ORG_C
    call para_insert_value('FOOD_MIN_ZOO'                     ,                      FOOD_MIN_ZOO) !Model constant no 121 : Zooplankton Minimum food conc. for feeding
    call para_insert_value('KE_ZOO'                           ,                            KE_ZOO) !Model constant no 122 : not used Zooplankton Excretion rate as growth fraction
    call para_insert_value('FRAC_ZOO_EX_ORG'                  ,                   FRAC_ZOO_EX_ORG) !Model constant no 123 : not used Zooplankton Excretion rate organic fraction
    call para_insert_value('KR_ZOO_20'                        ,                         KR_ZOO_20) !Model constant no 124 : Zooplankton Respiration rate
    call para_insert_value('THETA_KR_ZOO'                     ,                      THETA_KR_ZOO) !Model constant no 125 : Zooplankton Respiration rate Temperature correction
    call para_insert_value('KD_ZOO_20'                        ,                         KD_ZOO_20) !Model constant no 126 : Zooplankton Mortality rate
    call para_insert_value('THETA_KD_ZOO'                     ,                      THETA_KD_ZOO) !Model constant no 127 : Zooplankton Mortality rate Temperature correction
    call para_insert_value('DO_STR_HYPOX_ZOO_D'               ,                DO_STR_HYPOX_ZOO_D) !Model constant no 128 : Zooplankton Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_insert_value('THETA_HYPOX_ZOO_D'                ,                 THETA_HYPOX_ZOO_D) !Model constant no 129 : Zooplankton Multiplier of the exponent for Dissolved oxygen stress
    call para_insert_value('EXPON_HYPOX_ZOO_D'                ,                 EXPON_HYPOX_ZOO_D) !Model constant no 130 : Zooplankton Exponent constant for Dissolved oxygen stress
    call para_insert_value('ZOO_N_TO_C'                       ,                        ZOO_N_TO_C) !Model constant no 131 : Zooplankton Nitrogen to Carbon ratio
    call para_insert_value('ZOO_P_TO_C'                       ,                        ZOO_P_TO_C) !Model constant no 132 : Zooplankton Phosphorus to Carbon ratio
    call para_insert_value('ZOO_O2_TO_C'                      ,                       ZOO_O2_TO_C) !Model constant no 133 : Zooplankton Oxygen to Carbon ratio for respiration
    call para_insert_value('KDISS_DET_PART_ORG_C_20'          ,           KDISS_DET_PART_ORG_C_20) !Model constant no 134 : POC Dissolution rate not dependent on phytoplankton
    call para_insert_value('THETA_KDISS_DET_PART_ORG_C'       ,        THETA_KDISS_DET_PART_ORG_C) !Model constant no 135 : POC Carbon Dissolution rate Temperature correction
    call para_insert_value('FAC_PHYT_DET_PART_ORG_C'          ,           FAC_PHYT_DET_PART_ORG_C) !Model constant no 136 : POC Carbon Phytoplankton linear factor for dissolution rate
    call para_insert_value('KDISS_DET_PART_ORG_N_20'          ,           KDISS_DET_PART_ORG_N_20) !Model constant no 137 : PON Dissolution rate not dependent on phytoplankton
    call para_insert_value('THETA_KDISS_DET_PART_ORG_N'       ,        THETA_KDISS_DET_PART_ORG_N) !Model constant no 138 : PON Dissolution rate Temperature correction
    call para_insert_value('KHS_DISS_N'                       ,                        KHS_DISS_N) !Model constant no 139 : PON dissolution reverse half saturation for DIN
    call para_insert_value('FAC_PHYT_DET_PART_ORG_N'          ,           FAC_PHYT_DET_PART_ORG_N) !Model constant no 140 : PON Phytoplankton linear factor for dissolution rate
    call para_insert_value('KDISS_DET_PART_ORG_P_20'          ,           KDISS_DET_PART_ORG_P_20) !Model constant no 141 : POP Phosphorus Dissolution rate not dependent on phytoplankton
    call para_insert_value('THETA_KDISS_DET_PART_ORG_P'       ,        THETA_KDISS_DET_PART_ORG_P) !Model constant no 142 : POP Dissolution rate Temperature correction
    call para_insert_value('KHS_DISS_P'                       ,                        KHS_DISS_P) !Model constant no 143 : POP Dissolution reverse half saturation for DIP
    call para_insert_value('FAC_PHYT_DET_PART_ORG_P'          ,           FAC_PHYT_DET_PART_ORG_P) !Model constant no 144 : POP Phytoplankton linear factor for dissolution rate
    call para_insert_value('KDISS_PART_Si_20'                 ,                  KDISS_PART_Si_20) !Model constant no 145 : Particulate Silica Dissolution rate
    call para_insert_value('THETA_KDISS_PART_Si'              ,               THETA_KDISS_PART_Si) !Model constant no 146 : Particulate Silica Dissolution rate Temperature correction
    call para_insert_value('FAC_PHYT_AMIN_DOC'                ,                 FAC_PHYT_AMIN_DOC) !Model constant no 147 : DOC  Phytoplankton linear factor for mineralisation rate
    call para_insert_value('KHS_AMIN_N'                       ,                        KHS_AMIN_N) !Model constant no 148 : DON  reverse half saturation for DIN
    call para_insert_value('FAC_PHYT_AMIN_DON'                ,                 FAC_PHYT_AMIN_DON) !Model constant no 149 : DON Phytoplankton linear factor for mineralisation rate
    call para_insert_value('KHS_AMIN_P'                       ,                        KHS_AMIN_P) !Model constant no 150 : DOP reverse half saturation for DIP
    call para_insert_value('FAC_PHYT_AMIN_DOP'                ,                 FAC_PHYT_AMIN_DOP) !Model constant no 151 : DOP Phytoplankton linear factor for mineralisation rate
    call para_insert_value('K_NITR_20'                        ,                         K_NITR_20) !Model constant no 152 : Amonia nitrification rate
    call para_insert_value('THETA_K_NITR'                     ,                      THETA_K_NITR) !Model constant no 153 : Amonia nitrification rate Temperature constant
    call para_insert_value('KHS_NITR_OXY'                     ,                      KHS_NITR_OXY) !Model constant no 154 : Amonia nitrification half saturation for Oxygen
    call para_insert_value('KHS_NITR_NH4_N'                   ,                    KHS_NITR_NH4_N) !Model constant no 155 : Amonia nitrification half saturation for Amonia
    call para_insert_value('PH_NITR_NH4_MIN'                  ,                   PH_NITR_NH4_MIN) !Model constant no 156 : optimum lower range for pH correction factor for nitrification
    call para_insert_value('PH_NITR_NH4_MAX'                  ,                   PH_NITR_NH4_MAX) !Model constant no 157 : optimum upper range for pH correction factor for nitrification
    call para_insert_value('k_OX_FE_II'                       ,                        k_OX_FE_II) !Model constant no 158 : Oxidation rate for iron 2+
    call para_insert_value('k_RED_FE_III'                     ,                      k_RED_FE_III) !Model constant no 159 : reduction rate for iron 3+
    call para_insert_value('k_OX_MN_II'                       ,                        k_OX_MN_II) !Model constant no 160 : oxidation rate for manganese 2+
    call para_insert_value('k_RED_MN_IV'                      ,                       k_RED_MN_IV) !Model constant no 161 : reduction rate for manganese 4+
    call para_insert_value('KHS_DOXY_FE_III_RED'              ,               KHS_DOXY_FE_III_RED) !Model constant no 162 : reversed Monod half saturation of DOXY for iron 3+ reduction
    call para_insert_value('KHS_DOXY_MN_IV_RED'               ,                KHS_DOXY_MN_IV_RED) !Model constant no 163 : reversed Monod half saturation of DOXY for manganese 4+ reduction
    call para_insert_value('K_MIN_DOC_DOXY_20'                ,                 K_MIN_DOC_DOXY_20) !Model constant no 164 : Mineralization rate constant of DOC at 20 C for dissolved oxygen as final electron acceptor
    call para_insert_value('K_MIN_DOC_NO3N_20'                ,                 K_MIN_DOC_NO3N_20) !Model constant no 165 : Mineralization rate constant of DOC at 20 C for nitrate as final electron acceptor
    call para_insert_value('K_MIN_DOC_MN_IV_20'               ,                K_MIN_DOC_MN_IV_20) !Model constant no 166 : Mineralization rate constant of DOC at 20 C for Mn IV as final electron acceptor
    call para_insert_value('K_MIN_DOC_FE_III_20'              ,               K_MIN_DOC_FE_III_20) !Model constant no 167 : Mineralization rate constant of DOC at 20 C for Fe III as final electron acceptor
    call para_insert_value('K_MIN_DOC_S_PLUS_6_20'            ,             K_MIN_DOC_S_PLUS_6_20) !Model constant no 168 : Mineralization rate constant of DOC at 20 C for sulphate as final electron acceptor
    call para_insert_value('K_MIN_DOC_DOC_20'                 ,                  K_MIN_DOC_DOC_20) !Model constant no 169 : Mineralization rate constant of DOC at 20 C for DOC itself as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOC_DOXY'             ,              THETA_K_MIN_DOC_DOXY) !Model constant no 170 : Temperature correction factor for mineralization process rate constant of DOC for dissolved oxygen as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOC_NO3N'             ,              THETA_K_MIN_DOC_NO3N) !Model constant no 171 : Temperature correction factor for mineralization process rate constant of DOC for nitrate as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOC_MN_IV'            ,             THETA_K_MIN_DOC_MN_IV) !Model constant no 172 : Temperature correction factor for mineralization process rate constant of DOC for MN IV as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOC_FE_III'           ,            THETA_K_MIN_DOC_FE_III) !Model constant no 173 : Temperature correction factor for mineralization process rate constant of DOC for FE III as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOC_S_PLUS_6'         ,          THETA_K_MIN_DOC_S_PLUS_6) !Model constant no 174 : Temperature correction factor for mineralization process rate constant of DOC for Suphate as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOC_DOC'              ,               THETA_K_MIN_DOC_DOC) !Model constant no 175 : Temperature correction factor for mineralization process rate constant of DOC for DOC
    call para_insert_value('K_HS_DOC_MIN_DOXY'                ,                 K_HS_DOC_MIN_DOXY) !Model constant no 176 : Monod type half-saturation concentration of DOC for DOC mineralization for dissolved oxygen as the final electron acceptor
    call para_insert_value('K_HS_DOC_MIN_NO3N'                ,                 K_HS_DOC_MIN_NO3N) !Model constant no 177 : Monod type half-saturation concentration of DOC for DOC mineralization for nitrate as the final electron acceptor
    call para_insert_value('K_HS_DOC_MIN_MN_IV'               ,                K_HS_DOC_MIN_MN_IV) !Model constant no 178 : Monod type half-saturation concentration of DOC for DOC mineralization for Mn IV as the final electron acceptor
    call para_insert_value('K_HS_DOC_MIN_FE_III'              ,               K_HS_DOC_MIN_FE_III) !Model constant no 179 : Monod type half-saturation concentration of DOC for DOC mineralization for Fe III as the final electron acceptor
    call para_insert_value('K_HS_DOC_MIN_S_PLUS_6'            ,             K_HS_DOC_MIN_S_PLUS_6) !Model constant no 180 : Monod type half-saturation concentration of DOC for DOC mineralization for sulphate as the final electron acceptor
    call para_insert_value('K_HS_DOC_MIN_DOC'                 ,                  K_HS_DOC_MIN_DOC) !Model constant no 181 : Monod type half-saturation concentration of DOC for DOC mineralization for DOC itself as the final electron acceptor
    call para_insert_value('K_HS_DOXY_RED_LIM'                ,                 K_HS_DOXY_RED_LIM) !Model constant no 182 : Monod type half-saturation concentration of dissolved oxygen to limit the consumption of dissolved oxygen as the final electron acce
    call para_insert_value('K_HS_NO3N_RED_LIM'                ,                 K_HS_NO3N_RED_LIM) !Model constant no 183 : Monod type half-saturation concentration of nitrate nitrogen to limit the consumption of nitrate as the final electron acceptor for
    call para_insert_value('K_HS_MN_IV_RED_LIM'               ,                K_HS_MN_IV_RED_LIM) !Model constant no 184 : Monod type half-saturation concentration of MN_IV to limit the consumption of Mn IV as the final electron acceptor for DOC mineraliz
    call para_insert_value('K_HS_FE_III_RED_LIM'              ,               K_HS_FE_III_RED_LIM) !Model constant no 185 : Monod type half-saturation concentration of FE_III to limit the consumption of Fe III as the final electron acceptor for DOC mineral
    call para_insert_value('K_HS_S_PLUS_6_RED_LIM'            ,             K_HS_S_PLUS_6_RED_LIM) !Model constant no 186 : Monod type half-saturation concentration of sulphate to limit the consumption of sulphate as the final electron acceptor for DOC min
    call para_insert_value('K_HS_DOXY_RED_INHB'               ,                K_HS_DOXY_RED_INHB) !Model constant no 187 : Reversed Monod type half saturation concentration that simulate the inhibition effect of dissolved oxygen on consumption of NO3, Mn
    call para_insert_value('K_HS_NO3N_RED_INHB'               ,                K_HS_NO3N_RED_INHB) !Model constant no 188 : Reversed Monod type half saturation concentration that simulate the inhibition effect of nitrate on consumption of Mn IV, Fe III, S
    call para_insert_value('K_HS_MN_IV_RED_INHB'              ,               K_HS_MN_IV_RED_INHB) !Model constant no 189 : Reversed Monod type half saturation concentration that simulate the inhibition effect of Mn IV on consumption of Fe III, S VI and DO
    call para_insert_value('K_HS_FE_III_RED_INHB'             ,              K_HS_FE_III_RED_INHB) !Model constant no 190 : Reversed Monod type half saturation concentration that simulate the inhibition effect of Fe III on consumption of S VI and DOC itsel
    call para_insert_value('K_HS_S_PLUS_6_RED_INHB'           ,            K_HS_S_PLUS_6_RED_INHB) !Model constant no 191 : Reversed Monod type half saturation concentration that simulate the inhibition effect of S VI on consumption of DOC itself for DOC m
    call para_insert_value('PH_MIN_DOC_MIN_DOXY'              ,               PH_MIN_DOC_MIN_DOXY) !Model constant no 192 : Min. pH for the optimum pH range for DOC mineralization with DOXY as final electron acceptor
    call para_insert_value('PH_MIN_DOC_MIN_NO3N'              ,               PH_MIN_DOC_MIN_NO3N) !Model constant no 193 : Min. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor
    call para_insert_value('PH_MIN_DOC_MIN_MN_IV'             ,              PH_MIN_DOC_MIN_MN_IV) !Model constant no 194 : Min. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor
    call para_insert_value('PH_MIN_DOC_MIN_FE_III'            ,             PH_MIN_DOC_MIN_FE_III) !Model constant no 195 : Min. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor
    call para_insert_value('PH_MIN_DOC_MIN_S_PLUS_6'          ,           PH_MIN_DOC_MIN_S_PLUS_6) !Model constant no 196 : Min. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor
    call para_insert_value('PH_MIN_DOC_MIN_DOC'               ,                PH_MIN_DOC_MIN_DOC) !Model constant no 197 : Min. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor
    call para_insert_value('PH_MAX_DOC_MIN_DOXY'              ,               PH_MAX_DOC_MIN_DOXY) !Model constant no 198 : Max. pH for the optimum pH range for DOC mineralization with dissolved oxygen as final electron acceptor
    call para_insert_value('PH_MAX_DOC_MIN_NO3N'              ,               PH_MAX_DOC_MIN_NO3N) !Model constant no 199 : Max. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor
    call para_insert_value('PH_MAX_DOC_MIN_MN_IV'             ,              PH_MAX_DOC_MIN_MN_IV) !Model constant no 200 : Max. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor
    call para_insert_value('PH_MAX_DOC_MIN_FE_III'            ,             PH_MAX_DOC_MIN_FE_III) !Model constant no 201 : Max. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor
    call para_insert_value('PH_MAX_DOC_MIN_S_PLUS_6'          ,           PH_MAX_DOC_MIN_S_PLUS_6) !Model constant no 202 : Max. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor
    call para_insert_value('PH_MAX_DOC_MIN_DOC'               ,                PH_MAX_DOC_MIN_DOC) !Model constant no 203 : Max. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor
    call para_insert_value('K_MIN_DON_DOXY_20'                ,                 K_MIN_DON_DOXY_20) !Model constant no 204 : Mineralization rate constant of DON at 20 C for dissolved oxygen as final electron acceptor
    call para_insert_value('K_MIN_DON_NO3N_20'                ,                 K_MIN_DON_NO3N_20) !Model constant no 205 : Mineralization rate constant of DON at 20 C for NO3_N as final electron acceptor
    call para_insert_value('K_MIN_DON_MN_IV_20'               ,                K_MIN_DON_MN_IV_20) !Model constant no 206 : Mineralization rate constant of DON at 20 C for Mn IV as final electron acceptor
    call para_insert_value('K_MIN_DON_FE_III_20'              ,               K_MIN_DON_FE_III_20) !Model constant no 207 : Mineralization rate constant of DON at 20 C for FE III as final electron acceptor
    call para_insert_value('K_MIN_DON_S_PLUS_6_20'            ,             K_MIN_DON_S_PLUS_6_20) !Model constant no 208 : Mineralization rate constant of DON at 20 C for S_PLUS_6 as final electron acceptor
    call para_insert_value('K_MIN_DON_DOC_20'                 ,                  K_MIN_DON_DOC_20) !Model constant no 209 : Mineralization rate constant of DON at 20 C for DOC as final electron acceptor
    call para_insert_value('THETA_K_MIN_DON_DOXY'             ,              THETA_K_MIN_DON_DOXY) !Model constant no 210 : Temperature correction factor for mineralization process rate constant of DON for dissolved oxygen as final electron acceptor
    call para_insert_value('THETA_K_MIN_DON_NO3N'             ,              THETA_K_MIN_DON_NO3N) !Model constant no 211 : Temperature correction factor for mineralization process rate constant of DON for nitrate as final electron acceptor
    call para_insert_value('THETA_K_MIN_DON_MN_IV'            ,             THETA_K_MIN_DON_MN_IV) !Model constant no 212 : Temperature correction factor for mineralization process rate constant of DON for MN_IV as final electron acceptor
    call para_insert_value('THETA_K_MIN_DON_FE_III'           ,            THETA_K_MIN_DON_FE_III) !Model constant no 213 : Temperature correction factor for mineralization process rate constant of DON for FE_III as final electron acceptor
    call para_insert_value('THETA_K_MIN_DON_S_PLUS_6'         ,          THETA_K_MIN_DON_S_PLUS_6) !Model constant no 214 : Temperature correction factor for mineralization process rate constant of DON for sulphate as final electron acceptor
    call para_insert_value('THETA_K_MIN_DON_DOC'              ,               THETA_K_MIN_DON_DOC) !Model constant no 215 : Temperature correction factor for mineralization process rate constant of DON for DOC
    call para_insert_value('K_HS_DON_MIN_DOXY'                ,                 K_HS_DON_MIN_DOXY) !Model constant no 216 : Monod type half-saturation concentration of DON for DON mineralization for dissolved oxygen as the final electron acceptor
    call para_insert_value('K_HS_DON_MIN_NO3N'                ,                 K_HS_DON_MIN_NO3N) !Model constant no 217 : Monod type half-saturation concentration of DON for DON mineralization for nitrate nitrogen as the final electron acceptor
    call para_insert_value('K_HS_DON_MIN_MN_IV'               ,                K_HS_DON_MIN_MN_IV) !Model constant no 218 : Monod type half-saturation concentration of DON for DON mineralization for MN_IV as the final electron acceptor
    call para_insert_value('K_HS_DON_MIN_FE_III'              ,               K_HS_DON_MIN_FE_III) !Model constant no 219 : Monod type half-saturation concentration of DON for DON mineralization for FE_III as the final electron acceptor
    call para_insert_value('K_HS_DON_MIN_S_PLUS_6'            ,             K_HS_DON_MIN_S_PLUS_6) !Model constant no 220 : Monod type half-saturation concentration of DON for DON mineralization for sulphate sulphur as the final electron acceptor
    call para_insert_value('K_HS_DON_MIN_DOC'                 ,                  K_HS_DON_MIN_DOC) !Model constant no 221 : Monod type half-saturation concentration of DON for DON mineralization for DOC as the final electron acceptor
    call para_insert_value('PH_MIN_DON_MIN_DOXY'              ,               PH_MIN_DON_MIN_DOXY) !Model constant no 222 : Min. pH for the optimum pH range for DON mineralization with DOXY as final electron acceptor
    call para_insert_value('PH_MIN_DON_MIN_NO3N'              ,               PH_MIN_DON_MIN_NO3N) !Model constant no 223 : Min. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor
    call para_insert_value('PH_MIN_DON_MIN_MN_IV'             ,              PH_MIN_DON_MIN_MN_IV) !Model constant no 224 : Min. pH for the optimum pH range for DON mineralization with MN IV as final electron acceptor
    call para_insert_value('PH_MIN_DON_MIN_FE_III'            ,             PH_MIN_DON_MIN_FE_III) !Model constant no 225 : Min. pH for the optimum pH range for DON mineralization with FE III as final electron acceptor
    call para_insert_value('PH_MIN_DON_MIN_S_PLUS_6'          ,           PH_MIN_DON_MIN_S_PLUS_6) !Model constant no 226 : Min. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor
    call para_insert_value('PH_MIN_DON_MIN_DOC'               ,                PH_MIN_DON_MIN_DOC) !Model constant no 227 : Min. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor
    call para_insert_value('PH_MAX_DON_MIN_DOXY'              ,               PH_MAX_DON_MIN_DOXY) !Model constant no 228 : Max. pH for the optimum pH range for DON mineralization with dissolved oxygen as final electron acceptor
    call para_insert_value('PH_MAX_DON_MIN_NO3N'              ,               PH_MAX_DON_MIN_NO3N) !Model constant no 229 : Max. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor
    call para_insert_value('PH_MAX_DON_MIN_MN_IV'             ,              PH_MAX_DON_MIN_MN_IV) !Model constant no 230 : Max. pH for the optimum pH range for DON mineralization with Mn IV as final electron acceptor
    call para_insert_value('PH_MAX_DON_MIN_FE_III'            ,             PH_MAX_DON_MIN_FE_III) !Model constant no 231 : Max. pH for the optimum pH range for DON mineralization with FE_III as final electron acceptor
    call para_insert_value('PH_MAX_DON_MIN_S_PLUS_6'          ,           PH_MAX_DON_MIN_S_PLUS_6) !Model constant no 232 : Max. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor
    call para_insert_value('PH_MAX_DON_MIN_DOC'               ,                PH_MAX_DON_MIN_DOC) !Model constant no 233 : Max. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor
    call para_insert_value('K_MIN_DOP_DOXY_20'                ,                 K_MIN_DOP_DOXY_20) !Model constant no 234 : Mineralization rate constant of DOP at 20 C for dissolved oxygen as final electron acceptor
    call para_insert_value('K_MIN_DOP_NO3N_20'                ,                 K_MIN_DOP_NO3N_20) !Model constant no 235 : Mineralization rate constant of DOP at 20 C for NO3_N as final electron acceptor
    call para_insert_value('K_MIN_DOP_MN_IV_20'               ,                K_MIN_DOP_MN_IV_20) !Model constant no 236 : Mineralization rate constant of DOP at 20 C for Mn IV as final electron acceptor
    call para_insert_value('K_MIN_DOP_FE_III_20'              ,               K_MIN_DOP_FE_III_20) !Model constant no 237 : Mineralization rate constant of DOP at 20 C for FE_III as final electron acceptor
    call para_insert_value('K_MIN_DOP_S_PLUS_6_20'            ,             K_MIN_DOP_S_PLUS_6_20) !Model constant no 238 : Mineralization rate constant of DOP at 20 C for S_PLUS_6 as final electron acceptor
    call para_insert_value('K_MIN_DOP_DOC_20'                 ,                  K_MIN_DOP_DOC_20) !Model constant no 239 : Mineralization rate constant of DOP at 20 C for DOC as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOP_DOXY'             ,              THETA_K_MIN_DOP_DOXY) !Model constant no 240 : Temperature correction factor for mineralization process rate constant of DOP for dissolved oxygen as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOP_NO3N'             ,              THETA_K_MIN_DOP_NO3N) !Model constant no 241 : Temperature correction factor for mineralization process rate constant of DOP for nitrate as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOP_MN_IV'            ,             THETA_K_MIN_DOP_MN_IV) !Model constant no 242 : Temperature correction factor for mineralization process rate constant of DOP for Mn IV as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOP_FE_III'           ,            THETA_K_MIN_DOP_FE_III) !Model constant no 243 : Temperature correction factor for mineralization process rate constant of DOP for FE_III as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOP_S_PLUS_6'         ,          THETA_K_MIN_DOP_S_PLUS_6) !Model constant no 244 : Temperature correction factor for mineralization process rate constant of DOP for sulphate as final electron acceptor
    call para_insert_value('THETA_K_MIN_DOP_DOC'              ,               THETA_K_MIN_DOP_DOC) !Model constant no 245 : Temperature correction factor for mineralization process rate constant of DOP for DOC
    call para_insert_value('K_HS_DOP_MIN_DOXY'                ,                 K_HS_DOP_MIN_DOXY) !Model constant no 246 : Monod type half-saturation concentration of DOP for DOP mineralization for dissolved oxygen as the final electron acceptor
    call para_insert_value('K_HS_DOP_MIN_NO3N'                ,                 K_HS_DOP_MIN_NO3N) !Model constant no 247 : Monod type half-saturation concentration of DOP for DOP mineralization for nitrate nitrogen as the final electron acceptor
    call para_insert_value('K_HS_DOP_MIN_MN_IV'               ,                K_HS_DOP_MIN_MN_IV) !Model constant no 248 : Monod type half-saturation concentration of DOP for DOP mineralization for MN_IV as the final electron acceptor
    call para_insert_value('K_HS_DOP_MIN_FE_III'              ,               K_HS_DOP_MIN_FE_III) !Model constant no 249 : Monod type half-saturation concentration of DOP for DOP mineralization for FE_III as the final electron acceptor
    call para_insert_value('K_HS_DOP_MIN_S_PLUS_6'            ,             K_HS_DOP_MIN_S_PLUS_6) !Model constant no 250 : Monod type half-saturation concentration of DOP for DOP mineralization for sulphate sulphur as the final electron acceptor
    call para_insert_value('K_HS_DOP_MIN_DOC'                 ,                  K_HS_DOP_MIN_DOC) !Model constant no 251 : Monod type half-saturation concentration of DOP for DOP mineralization for DOC as the final electron acceptor
    call para_insert_value('PH_MIN_DOP_MIN_DOXY'              ,               PH_MIN_DOP_MIN_DOXY) !Model constant no 252 : Min. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor
    call para_insert_value('PH_MIN_DOP_MIN_NO3N'              ,               PH_MIN_DOP_MIN_NO3N) !Model constant no 253 : Min. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor
    call para_insert_value('PH_MIN_DOP_MIN_MN_IV'             ,              PH_MIN_DOP_MIN_MN_IV) !Model constant no 254 : Min. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor
    call para_insert_value('PH_MIN_DOP_MIN_FE_III'            ,             PH_MIN_DOP_MIN_FE_III) !Model constant no 255 : Min. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor
    call para_insert_value('PH_MIN_DOP_MIN_S_PLUS_6'          ,           PH_MIN_DOP_MIN_S_PLUS_6) !Model constant no 256 : Min. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor
    call para_insert_value('PH_MIN_DOP_MIN_DOC'               ,                PH_MIN_DOP_MIN_DOC) !Model constant no 257 : Min. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor
    call para_insert_value('PH_MAX_DOP_MIN_DOXY'              ,               PH_MAX_DOP_MIN_DOXY) !Model constant no 258 : Max. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor
    call para_insert_value('PH_MAX_DOP_MIN_NO3N'              ,               PH_MAX_DOP_MIN_NO3N) !Model constant no 259 : Max. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor
    call para_insert_value('PH_MAX_DOP_MIN_MN_IV'             ,              PH_MAX_DOP_MIN_MN_IV) !Model constant no 260 : Max. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor
    call para_insert_value('PH_MAX_DOP_MIN_FE_III'            ,             PH_MAX_DOP_MIN_FE_III) !Model constant no 261 : Max. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor
    call para_insert_value('PH_MAX_DOP_MIN_S_PLUS_6'          ,           PH_MAX_DOP_MIN_S_PLUS_6) !Model constant no 262 : Max. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor
    call para_insert_value('PH_MAX_DOP_MIN_DOC'               ,                PH_MAX_DOP_MIN_DOC) !Model constant no 263 : Max. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor
    call para_insert_value('k_OX_CH4'                         ,                          k_OX_CH4) !Model constant no 264 : Methane oxidation rate constant at 20 C
    call para_insert_value('THETA_k_OX_CH4'                   ,                    THETA_k_OX_CH4) !Model constant no 265 : Arhenius type temperature correction for methane oxidation rate constant
    call para_insert_value('k_HS_OX_CH4_DOXY'                 ,                  k_HS_OX_CH4_DOXY) !Model constant no 266 : Monod type Half saturation of methane oxidation for dissolved oxygen
    call para_insert_value('k_OX_H2S'                         ,                          k_OX_H2S) !Model constant no 267 : Hydrogen sulphide oxidation rate constant at 20 C
    call para_insert_value('THETA_k_OX_H2S'                   ,                    THETA_k_OX_H2S) !Model constant no 268 : Arhenius type temperature correction for sulphide oxidation rate constant
    call para_insert_value('k_HS_OX_H2S_DOXY'                 ,                  k_HS_OX_H2S_DOXY) !Model constant no 269 : Monod type Half saturation of hydrogen sulphide oxidation for dissolved oxygen
    call para_insert_value('k_DISS_FE_II_20'                  ,                   k_DISS_FE_II_20) !Model constant no 270 : Dissolution rate constant for Fe II at 20 C
    call para_insert_value('THETA_k_DISS_FE_II'               ,                THETA_k_DISS_FE_II) !Model constant no 271 : Dissolution rate constant for Fe II
    call para_insert_value('INIT_MULT_FE_II_DISS'             ,              INIT_MULT_FE_II_DISS) !Model constant no 272 : Initial fraction of dissolved Fe II
    call para_insert_value('k_DISS_FE_III_20'                 ,                  k_DISS_FE_III_20) !Model constant no 273 : Dissolution rate constant for Fe III at 20 C
    call para_insert_value('THETA_k_DISS_FE_III'              ,               THETA_k_DISS_FE_III) !Model constant no 274 : Dissolution rate constant for Fe III
    call para_insert_value('INIT_MULT_FE_III_DISS'            ,             INIT_MULT_FE_III_DISS) !Model constant no 275 : Initial fraction of dissolved Fe III                                                                                                                               K_A
    call para_insert_value('KG_NOST_VEG_HET_OPT_TEMP'         ,          KG_NOST_VEG_HET_OPT_TEMP) !Model constant no 276 : Nostocales (veg + het) Growth rate constant
    call para_insert_value('NOST_VEG_HET_OPT_TEMP_LR'         ,          NOST_VEG_HET_OPT_TEMP_LR) !Model constant no 277 : Nostocales (veg + het) optimal temperature lower range
    call para_insert_value('NOST_VEG_HET_OPT_TEMP_UR'         ,          NOST_VEG_HET_OPT_TEMP_UR) !Model constant no 278 : Nostocales (veg + het) optimal temperature upper range
    call para_insert_value('EFF_NOST_VEG_HET_GROWTH'          ,           EFF_NOST_VEG_HET_GROWTH) !Model constant no 279 : Nostocales (veg + het) Effective growth. (1-EG)*growth - losses for RESP and excretion
    call para_insert_value('KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP', KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP) !Model constant no 280 : Nostocales (veg + het) Temperature correction for growth lower temperature
    call para_insert_value('KAPPA_NOST_VEG_HET_OVER_OPT_TEMP' ,  KAPPA_NOST_VEG_HET_OVER_OPT_TEMP) !Model constant no 281 : Nostocales (veg + het) Temperature correction for growth upper temperature
    call para_insert_value('KR_NOST_VEG_HET_20'               ,                KR_NOST_VEG_HET_20) !Model constant no 282 : Nostocales (veg + het) RESP rate constant
    call para_insert_value('THETA_KR_NOST_VEG_HET'            ,             THETA_KR_NOST_VEG_HET) !Model constant no 283 : Nostocales (veg + het) Temperature correction for RESP rate
    call para_insert_value('KD_NOST_VEG_HET_20'               ,                KD_NOST_VEG_HET_20) !Model constant no 284 : Nostocales (veg + het) Mortality rate constant
    call para_insert_value('THETA_KD_NOST_VEG_HET'            ,             THETA_KD_NOST_VEG_HET) !Model constant no 285 : Nostocales (veg + het) Temperature correction for Mortality rate
    call para_insert_value('M_DENS_VEG_HET'                   ,                    M_DENS_VEG_HET) !Model constant no 286 : Nostocales (veg + het) mortality rate constant because of too dense population
    call para_insert_value('KHS_DP_NOST_VEG_HET'              ,               KHS_DP_NOST_VEG_HET) !Model constant no 287 : Nostocales (veg + het) Half saturation growth for DP
    call para_insert_value('KHS_O2_NOST_VEG_HET'              ,               KHS_O2_NOST_VEG_HET) !Model constant no 288 : Nostocales (veg + het) Half saturation growth for O2
    call para_insert_value('FRAC_NOST_VEG_HET_EXCR'           ,            FRAC_NOST_VEG_HET_EXCR) !Model constant no 289 : Nostocales (veg + het) Fraction of excretion in metabolism rate
    call para_insert_value('I_S_NOST_VEG_HET'                 ,                  I_S_NOST_VEG_HET) !Model constant no 290 : Nostocales (veg + het) Light saturation (langleys). Not used, is calculated!
    call para_insert_value('DO_STR_HYPOX_NOST_VEG_HET_D'      ,       DO_STR_HYPOX_NOST_VEG_HET_D) !Model constant no 291 : Nostocales (veg + het) Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
    call para_insert_value('THETA_HYPOX_NOST_VEG_HET_D'       ,        THETA_HYPOX_NOST_VEG_HET_D) !Model constant no 292 : Nostocales (veg + het) Multiplier of the exponent for Dissolved oxygen stress
    call para_insert_value('EXPON_HYPOX_NOST_VEG_HET_D'       ,        EXPON_HYPOX_NOST_VEG_HET_D) !Model constant no 293 : Nostocales (veg + het) Exponent constant for Dissolved oxygen stress
    call para_insert_value('NOST_N_TO_C'                      ,                       NOST_N_TO_C) !Model constant no 294 : Nostocales (veg + het) Nitrogen to Carbon ratio
    call para_insert_value('NOST_P_TO_C'                      ,                       NOST_P_TO_C) !Model constant no 295 : Nostocales (veg + het) Phosphorus to Carbon ratio
    call para_insert_value('NOST_O2_TO_C'                     ,                      NOST_O2_TO_C) !Model constant no 296 : Nostocales (veg + het) Oxygen to Carbon ratio for respiration
    call para_insert_value('NOST_C_TO_CHLA'                   ,                    NOST_C_TO_CHLA) !Model constant no 297 : Nostocales (veg + het) Carbon to Chlorophyl a ratio
    call para_insert_value('P_GERM_AKI'                       ,                        P_GERM_AKI) !Model constant no 298 : Gerimination rate constant for akinets
    call para_insert_value('N_GERM_AKI'                       ,                        N_GERM_AKI) !Model constant no 299 : DIN concentration to trigger gerimination (below that gerimination starts)
    call para_insert_value('P_FORM_AKI'                       ,                        P_FORM_AKI) !Model constant no 300 : Formation rate constant for akinets
    call para_insert_value('DAY_FORM_AKI'                     ,                      DAY_FORM_AKI) !Model constant no 301 :
    call para_insert_value('T_FORM_AKI'                       ,                        T_FORM_AKI) !Model constant no 302 : Temperature to trigger formation (below that formation starts)
    call para_insert_value('K_LOSS_AKI'                       ,                        K_LOSS_AKI) !Model constant no 303 : Loss rate constant for akinetes
    call para_insert_value('K_MORT_AKI_20'                    ,                     K_MORT_AKI_20) !Model constant no 304 : Decompositon rate constant of akinetes at 20 degrees celcisus
    call para_insert_value('THETA_K_MORT_AKI'                 ,                  THETA_K_MORT_AKI) !Model constant no 305 : Arhenius temperature correction factor for akinete decomposition rate constant
    call para_insert_value('T_GERM_AKI'                       ,                        T_GERM_AKI) !Model constant no 306 : Temperature where the gerimination of akinetes is initiated
    call para_insert_value('KHS_POC_DISS_SAT'                 ,                  KHS_POC_DISS_SAT) !Model constant no 307 :
    call para_insert_value('KHS_PON_DISS_SAT'                 ,                  KHS_PON_DISS_SAT) !Model constant no 308 :
    call para_insert_value('KHS_POP_DISS_SAT'                 ,                  KHS_POP_DISS_SAT) !Model constant no 309 :
    call para_insert_value('frac_avail_DON'                   ,                    frac_avail_DON) !Model constant no 310 :
    call para_insert_value('frac_avail_DOP'                   ,                    frac_avail_DOP) !Model constant no 311 :
    call para_insert_value('frac_avail_DON_NOST'              ,               frac_avail_DON_NOST) !Model constant no 312 :
    call para_insert_value('KHS_DN_NOST_VEG_HET'              ,               KHS_DN_NOST_VEG_HET) !Model constant no 313 :
    call para_insert_value('FRAC_FIX_N_FOR_GR_VEG_HET'        ,         FRAC_FIX_N_FOR_GR_VEG_HET) !Model constant no 314 :
    call para_insert_value('FRAC_NOST_GROWTH'                 ,                  FRAC_NOST_GROWTH) !Model constant no 315 :
    call para_insert_value('K_MIN_PHYT_AMIN_DOC'              ,               K_MIN_PHYT_AMIN_DOC) !Model constant no 316 :
    call para_insert_value('K_MIN_PHYT_AMIN_DON'              ,               K_MIN_PHYT_AMIN_DON) !Model constant no 317 :
    call para_insert_value('K_MIN_PHYT_AMIN_DOP'              ,               K_MIN_PHYT_AMIN_DOP) !Model constant no 318 :
    call para_insert_value('BETA_DIA'                         ,                          BETA_DIA) !Model constant no 319 : Diatoms photoinhibition parameter
    call para_insert_value('BETA_CYN'                         ,                          BETA_CYN) !Model constant no 320 : Non-fixing cyanobacteria photoinhibition parameter
    call para_insert_value('BETA_FIX_CYN'                     ,                      BETA_FIX_CYN) !Model constant no 321 : Fixing cyanobacteria photoinhibition parameter
    call para_insert_value('BETA_OPA'                         ,                          BETA_OPA) !Model constant no 322 : OtherPhyto photoinhibition parameter
    call para_insert_value('BETA_NOST_VEG_HET'                ,                 BETA_NOST_VEG_HET) !Model constant no 323 : Nostocales photoinhibition parameter
    call para_insert_value('ICE_LIGHT_TRANS'                  ,                   ICE_LIGHT_TRANS) !Model constant no 324 : under-ice PAR transmittance

end subroutine INSERT_PELAGIC_MODEL_CONSTANTS

!--------------------------------------------------------------------------

subroutine DEFAULT_PELAGIC_MODEL_CONSTANTS

    use AQUABC_PELAGIC_MODEL_CONSTANTS

    implicit none
                                  K_A =   -1.0D0  !Aeration coefficient (if negative calculates internally)
                            THETA_K_A =   1.04D0  !Temperature correction factor for aeration
                                  XKC =   0.08D0  !fixme. Light extinction per chlorophyl unit,( mcg Chla/l/m) for calculation of saturation for Smith. Should be di
                                PHIMX = 720.00D0  !Quantum yield const. mg C/mole photon
                      KG_DIA_OPT_TEMP =    3.7D0  !3.3 3.1  Diatoms Growth rate
                      DIA_OPT_TEMP_LR =    1.0D0  !Diatoms CTMI T_min (minimum cardinal temperature)
                      DIA_OPT_TEMP_UR =   24.0D0  !Diatoms CTMI T_opt (optimal temperature)
                       EFF_DIA_GROWTH =   0.95D0  !Diatoms Effective growth. (1-EG)*growth - losses for respiration and excretion
             KAPPA_DIA_UNDER_OPT_TEMP =   0.0D0   !(unused, kept for backward compat)
             KAPPA_DIA_OVER_OPT_TEMP  =  35.0D0   !Diatoms CTMI T_max (maximum cardinal temperature)
                            KR_DIA_20 =   0.05D0  !Diatoms Respiration rate
                         THETA_KR_DIA =   1.04D0  !Diatoms Temperature correction for basal respiration rate
                            KD_DIA_20 =   0.12D0  !Diatoms Mortality rate
                         THETA_KD_DIA =   1.02D0  !Diatoms Temperature correction for Mortality rate
                          KHS_DIN_DIA =  0.010D0  !Diatoms Half saturation growth for DIN
                          KHS_DIP_DIA =  0.005D0  !Diatoms Half saturation growth for DIP
                          KHS_DSi_DIA =  0.013D0  !Diatoms Half saturation growth for DSi
                           KHS_O2_DIA =   0.60D0  !Diatoms Half saturation growth for O2
                        FRAC_DIA_EXCR =   0.30D0  !Diatoms Fraction of excretion in metabolism rate
                              I_S_DIA = 100.00D0  !Diatoms Light saturation (langleys)
                   DO_STR_HYPOX_DIA_D =   0.70D0  !Diatoms Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
                    THETA_HYPOX_DIA_D =   1.20D0  !Diatoms Multiplier of the exponent for Dissolved oxygen stress
                    EXPON_HYPOX_DIA_D =   1.04D0  !Diatoms Exponent constant for Dissolved oxygen stress
                           DIA_N_TO_C =   0.22D0  !Diatoms Nitrogen to Carbon ratio
                           DIA_P_TO_C =  0.024D0  !Diatoms Phosphorus to Carbon ratio
                          DIA_Si_TO_C =   0.25D0  !Diatoms Silica to Carbon ratio
                          DIA_O2_TO_C =   2.66D0  !Diatoms Oxygen to Carbon ratio for respiration
                        DIA_C_TO_CHLA =  30.00D0  !Diatoms Carbon to Chlorophil a ratio
                      KG_CYN_OPT_TEMP =    2.4D0  !Non-fixing cyanobacteria Growth rate
                      CYN_OPT_TEMP_LR =   15.0D0  !Non-fixing cyanobacteria CTMI T_min
                      CYN_OPT_TEMP_UR =   26.0D0  !Non-fixing cyanobacteria CTMI T_opt
                       EFF_CYN_GROWTH =   0.95D0  !Non-fixing cyanobacteria Effective growth. (1-EG)*growth - losses for respiration and excretion
             KAPPA_CYN_UNDER_OPT_TEMP =   0.0D0   !(unused)
              KAPPA_CYN_OVER_OPT_TEMP =  38.0D0   !Non-fixing cyanobacteria CTMI T_max
                            KR_CYN_20 =   0.06D0  !Non-fixing cyanobacteria Respiration rate
                         THETA_KR_CYN =   1.04D0  !Non-fixing cyanobacteria Temperature correction for respiration rate
                            KD_CYN_20 =  0.125D0  !Non-fixing cyanobacteria Mortality rate
                         THETA_KD_CYN =   1.05D0  !Non-fixing cyanobacteria Temperature correction for Mortality rate
                          KHS_DIN_CYN =  0.009D0  !Non-fixing cyanobacteria Half saturation growth for DIN
                          KHS_DIP_CYN =  0.008D0  !Non-fixing cyanobacteria Half saturation growth for DIP
                           KHS_O2_CYN =   0.60D0  !Non-fixing cyanobacteria Half saturation growth for O2
                        FRAC_CYN_EXCR =   0.30D0  !Non-fixing cyanobacteria Fraction of excretion in metabolism rate
                              I_S_CYN = 100.00D0  !Non-fixing cyanobacteria Light saturation (langleys).Not used, is calculated!
                   DO_STR_HYPOX_CYN_D =   0.70D0  !Non-fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponential
                    THETA_HYPOX_CYN_D =   1.50D0  !Non-fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress
                    EXPON_HYPOX_CYN_D =   1.10D0  !Non-fixing cyanobacteria Exponent constant for Dissolved oxygen stress
                           CYN_N_TO_C =  0.220D0  !Non-fixing cyanobacteria Nitrogen to Carbon ratio ,was 0.1
                           CYN_P_TO_C =  0.024D0  !Non-fixing cyanobacteria Phosphorus to Carbon ratio
                          CYN_O2_TO_C =   2.66D0  !Non-fixing cyanobacteria Oxygen to Carbon ratio for respiration
                        CYN_C_TO_CHLA =  40.00D0  !Non-fixing cyanobacteria Carbon to Chlorophyl a ratio
                  KG_FIX_CYN_OPT_TEMP =    3.5D0  !Fixing cyanobacteria Growth rate constant
                  FIX_CYN_OPT_TEMP_LR =   18.0D0  !Fixing Cyanobacteria CTMI T_min
                  FIX_CYN_OPT_TEMP_UR =   26.0D0  !Fixing Cyanobacteria CTMI T_opt
                   EFF_FIX_CYN_GROWTH =   0.95D0  !Fixing cyanobacteria Effective growth. (1-EG)*growth - losses for RESP and excretion
         KAPPA_FIX_CYN_UNDER_OPT_TEMP =   0.0D0   !(unused)
          KAPPA_FIX_CYN_OVER_OPT_TEMP =  38.0D0   !Fixing cyanobacteria CTMI T_max
                        KR_FIX_CYN_20 =   0.06D0  !Fixing cyanobacteria RESP rate constant
                     THETA_KR_FIX_CYN =   1.04D0  !Fixing cyanobacteria Temperature correction for RESP rate
                        KD_FIX_CYN_20 =   0.10D0  !Fixing cyanobacteria Mortality rate constant
                     THETA_KD_FIX_CYN =   1.05D0  !Fixing cyanobacteria Temperature correction for Mortality rate
                      KHS_DIN_FIX_CYN =   0.01D0  !Fixing cyanobacteria Half saturation growth for DIN
                      KHS_DIP_FIX_CYN =  0.005D0  !Fixing cyanobacteria Half saturation growth for DIP
                       KHS_O2_FIX_CYN =   0.60D0  !Fixing cyanobacteria Half saturation growth for O2
                    FRAC_FIX_CYN_EXCR =   0.30D0  !Fixing cyanobacteria Fraction of excretion in metabolism rate
                          I_S_FIX_CYN = 100.00D0  !Fixing cyanobacteria Light saturation (langleys). Not used, is calculated!
               DO_STR_HYPOX_FIX_CYN_D =   0.70D0  !Fixing cyanobacteria Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
                THETA_HYPOX_FIX_CYN_D =   1.50D0  !Fixing cyanobacteria Multiplier of the exponent for Dissolved oxygen stress
                EXPON_HYPOX_FIX_CYN_D =   1.10D0  !Fixing cyanobacteria Exponent constant for Dissolved oxygen stress
                       FIX_CYN_N_TO_C =  0.220D0  !Fixing cyanobacteria Nitrogen to Carbon ratio
                       FIX_CYN_P_TO_C =  0.024D0  !Fixing cyanobacteria Phosphorus to Carbon ratio
                      FIX_CYN_O2_TO_C =   2.66D0  !Fixing cyanobacteria Oxygen to Carbon ratio for respiration
                    FIX_CYN_C_TO_CHLA =  40.00D0  !Fixing cyanobacteria Carbon to Chlorophyl a ratio
                                R_FIX =    1.0D0  !Fixing cyanobacteria Ratio between non-fixing and fixing fractions growth rate
                                K_FIX =  0.008D0  !Fixing cyanobacteria Effectivity parameter of switching to nitrogen fixation
                      KG_OPA_OPT_TEMP =    2.9D0  !OtherPhyto Growth rate constant
                      OPA_OPT_TEMP_LR =    9.0D0  !OtherPhyto CTMI T_min
                      OPA_OPT_TEMP_UR =   20.0D0  !OtherPhyto CTMI T_opt
                       EFF_OPA_GROWTH =   0.95D0  !OtherPhyto Effective growth. (1-EG)*growth - losses for respiration and excretion
             KAPPA_OPA_UNDER_OPT_TEMP =   0.0D0   !(unused)
              KAPPA_OPA_OVER_OPT_TEMP =  33.0D0   !OtherPhyto CTMI T_max
                            KR_OPA_20 =   0.06D0  !OtherPhyto Respiration rate constant
                         THETA_KR_OPA =   1.02D0  !OtherPhyto Temperature correction for respiration rate
                            KD_OPA_20 =   0.11D0  !OtherPhyto Mortality rate constant
                         THETA_KD_OPA =   1.02D0  !OtherPhyto Temperature correction for Mortality rate
                          KHS_DIN_OPA =  0.015D0  !OtherPhyto Half saturation growth for DIN
                          KHS_DIP_OPA =  0.013D0  !OtherPhyto Half saturation growth for DIP
                           KHS_O2_OPA =   0.60D0  !OtherPhyto Half saturation growth for O2
                        FRAC_OPA_EXCR =   0.30D0  !OtherPhyto Fraction of excretion in metabolism rate
                              I_S_OPA = 100.00D0  !OtherPhyto Light saturation (langleys). Not used, is calculated!
                   DO_STR_HYPOX_OPA_D =   0.70D0  !OtherPhyto Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
                    THETA_HYPOX_OPA_D =   1.20D0  !OtherPhyto Multiplier of the exponent for Dissolved oxygen stress
                    EXPON_HYPOX_OPA_D =   1.04D0  !OtherPhyto Exponent constant for Dissolved oxygen stress
                           OPA_N_TO_C =  0.220D0  !OtherPhyto Nitrogen to Carbon ratio
                           OPA_P_TO_C =  0.024D0  !OtherPhyto Phosphorus to Carbon ratio
                          OPA_O2_TO_C =   2.66D0  !OtherPhyto Oxygen to Carbon ratio for respiration
                        OPA_C_TO_CHLA =  30.00D0  !OtherPhyto Carbon to Chlorophyl a ratio
                      KG_ZOO_OPT_TEMP =   0.45D0  !Zooplankton Growth rate
                      ZOO_OPT_TEMP_LR =   10.0D0  !Zooplankton CTMI T_min
                      ZOO_OPT_TEMP_UR =   25.0D0  !Zooplankton CTMI T_opt
                       EFF_ZOO_GROWTH =   0.80D0  !Zooplankton Effective growth. (1-EG)*growth - losses for respiration and excretion
             KAPPA_ZOO_UNDER_OPT_TEMP =   0.0D0   !(unused)
              KAPPA_ZOO_OVER_OPT_TEMP =  35.0D0   !Zooplankton CTMI T_max
                         GRAT_ZOO_DIA =   1.00D0  !Zooplankton Grazing rate (growhth rate multiplier) on diatoms
                         GRAT_ZOO_CYN =   1.00D0  !Zooplankton Grazing rate (growhth rate multiplier) on Cyanobacteria
                         GRAT_ZOO_OPA =   1.00D0  !Zooplankton Grazing rate (growhth rate multiplier) on OtherPhyto
                     GRAT_ZOO_FIX_CYN =   1.00D0  !Zooplankton Grazing rate (growhth rate multiplier) on fixing Cyanobacteria
                GRAT_ZOO_NOST_VEG_HET =   1.00D0  !Zooplankton Grazing rate (growhth rate multiplier) on Nostocles (veg + het)
              GRAT_ZOO_DET_PART_ORG_C =   0.50D0  !Zooplankton Grazing rate (growhth rate multiplier) on part. ORG_C
                         PREF_ZOO_DIA =   0.26D0  !Zooplankton Preference for Diatoms
                         PREF_ZOO_CYN =   0.10D0  !Zooplankton Preference for Cyanobacteria
                     PREF_ZOO_FIX_CYN =   0.07D0  !Zooplankton Preference for fixing Cyanobacteria
                PREF_ZOO_NOST_VEG_HET =   0.00D0  !Zooplankton Preference for nostocales (veg + het)
                         PREF_ZOO_OPA =   0.37D0  !Zooplankton Preference for OtherPhyto
              PREF_ZOO_DET_PART_ORG_C =   0.20D0  !Zooplankton Preference for Part. ORG_C
                        KHS_DIA_C_ZOO =   0.10D0  !Zooplankton Half saturation growth for diatoms
                        KHS_CYN_C_ZOO =   0.07D0  !Zooplankton Half saturation growth for Cyanobacteria
                    KHS_FIX_CYN_C_ZOO =   0.07D0  !Zooplankton Half saturation growth for fixing Cyanobacteria
               KHS_NOST_VEG_HET_C_ZOO =   0.07D0  !Zooplankton Half saturation growth for Nostocales (veg + het)
                        KHS_OPA_C_ZOO =   0.15D0  !Zooplankton Half saturation growth for OtherPhyto
               KHS_DET_PART_ORG_C_ZOO =   0.50D0  !Zooplankton Half saturation growth for part. ORG_C
                         FOOD_MIN_ZOO =   0.02D0  !Zooplankton Minimum food conc. for feeding
                               KE_ZOO =   0.05D0  !Zooplankton Excretion rate as growth fraction
                      FRAC_ZOO_EX_ORG =   0.30D0  !Zooplankton Excretion rate organic fraction
                            KR_ZOO_20 =   0.03D0  !Zooplankton Respiration rate
                         THETA_KR_ZOO =   1.04D0  !Zooplankton Respiration rate Temperature correction
                            KD_ZOO_20 =   0.15D0  !Zooplankton Mortality rate
                         THETA_KD_ZOO =   1.04D0  !Zooplankton Mortality rate Temperature correction
                   DO_STR_HYPOX_ZOO_D =   2.00D0  !Zooplankton Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
                    THETA_HYPOX_ZOO_D =   1.20D0  !Zooplankton Multiplier of the exponent for Dissolved oxygen stress
                    EXPON_HYPOX_ZOO_D =   1.06D0  !Zooplankton Exponent constant for Dissolved oxygen stress
                           ZOO_N_TO_C =  0.220D0  !Zooplankton Nitrogen to Carbon ratio
                           ZOO_P_TO_C =  0.024D0  !Zooplankton Phosphorus to Carbon ratio
                          ZOO_O2_TO_C =   2.66D0  !Zooplankton Oxygen to Carbon ratio for respiration
              KDISS_DET_PART_ORG_C_20 =   0.1D0   !POC Dissolution rate not dependent on phytoplankton
           THETA_KDISS_DET_PART_ORG_C =   1.06D0  !POC Carbon Dissolution rate Temperature correction
              FAC_PHYT_DET_PART_ORG_C =     0.D0  !POC Carbon Phytoplankton linear factor for dissolution rate
              KDISS_DET_PART_ORG_N_20 =   0.25D0  !PON Dissolution rate not dependent on phytoplankton
           THETA_KDISS_DET_PART_ORG_N =   1.06D0  !PON Dissolution rate Temperature correction
                           KHS_DISS_N =   100.D0  !PON dissolution reverse half saturation for DIN
              FAC_PHYT_DET_PART_ORG_N =     0.D0  !PON Phytoplankton linear factor for dissolution rate
              KDISS_DET_PART_ORG_P_20 =   0.48D0  !POP Phosphorus Dissolution rate not dependent on phytoplankton
           THETA_KDISS_DET_PART_ORG_P =   1.06D0  !POP Dissolution rate Temperature correction
                           KHS_DISS_P =   100.D0  !POP Dissolution reverse half saturation for DIP
              FAC_PHYT_DET_PART_ORG_P =   0.D0    !POP Phytoplankton linear factor for dissolution rate
                     KDISS_PART_Si_20 =  0.001D0  !Particulate Silica Dissolution rate
                  THETA_KDISS_PART_Si =   1.04D0  !Particulate Silica Dissolution rate Temperature correction
                    FAC_PHYT_AMIN_DOC = 0.0045D0  !DOC Phytoplankton linear factor for mineralisation rate
                           KHS_AMIN_N =   100.D0  !DON mineralisation reverse half saturation for DIN
                    FAC_PHYT_AMIN_DON =  0.008D0  !DON Phytoplankton linear factor for mineralisation rate
                           KHS_AMIN_P =   100.D0  !DOP reverse half saturation for DIP
                    FAC_PHYT_AMIN_DOP =   0.90D0  !DOP Phytoplankton linear factor for mineralisation rate
                            K_NITR_20 =  0.600D0  !Amonia nitrification rate
                         THETA_K_NITR =  1.045D0  !Amonia nitrification rate Temperature constant
                         KHS_NITR_OXY =  2.000D0  !Amonia nitrification half saturation for Oxygen
                       KHS_NITR_NH4_N =  0.030D0  !Amonia nitrification half saturation for Amonia
                      PH_NITR_NH4_MIN =   6.90D0  !optimum lower range for pH correction factor for nitrification
                      PH_NITR_NH4_MAX =   8.20D0  !optimum upper range for pH correction factor for nitrification
                           k_OX_FE_II = .00125D0  !Oxidation rate for iron 2+
                         k_RED_FE_III =   2.00D0  !Reduction rate for iron 3+
                           k_OX_MN_II =   0.01D0  !Oxidation rate for manganese 2+
                          k_RED_MN_IV =   2.00D0  !Reduction rate for manganese 4+
                  KHS_DOXY_FE_III_RED =   0.20D0  !Reversed Monod half saturation of DOXY for iron 3+ reduction
                   KHS_DOXY_MN_IV_RED =   0.20D0  !Reversed Monod half saturation of DOXY for manganese 4+ reduction
                    K_MIN_DOC_DOXY_20 =  0.010D0  !Mineralization rate constant of DOC at 20 C for dissolved oxygen as final electron acceptor
                    K_MIN_DOC_NO3N_20 =  0.025D0  !Mineralization rate constant of DOC at 20 C for nitrate as final electron acceptor
                   K_MIN_DOC_MN_IV_20 =  0.025D0  !Mineralization rate constant of DOC at 20 C for Mn IV as final electron acceptor
                  K_MIN_DOC_FE_III_20 =  0.025D0  !Mineralization rate constant of DOC at 20 C for Fe III as final electron acceptor
                K_MIN_DOC_S_PLUS_6_20 =  0.025D0  !Mineralization rate constant of DOC at 20 C for sulphate as final electron acceptor
                     K_MIN_DOC_DOC_20 =  0.025D0  !Mineralization rate constant of DOC at 20 C for DOC itself as final electron acceptor
                 THETA_K_MIN_DOC_DOXY =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOC for dissolved oxygen as final elect
                 THETA_K_MIN_DOC_NO3N =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOC for nitrate as final electron accep
                THETA_K_MIN_DOC_MN_IV =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOC for MN IV as final electron accepto
               THETA_K_MIN_DOC_FE_III =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOC for FE III as final electron accept
             THETA_K_MIN_DOC_S_PLUS_6 =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOC for Suphate as final electron accep
                  THETA_K_MIN_DOC_DOC =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOC for DOC
                    K_HS_DOC_MIN_DOXY =   1.00D0  !Monod type half-saturation concentration of DOC for DOC mineralization for dissolved oxygen as the final electron
                    K_HS_DOC_MIN_NO3N =   1.00D0  !Monod type half-saturation concentration of DOC for DOC mineralization for nitrate as the final electron acceptor
                   K_HS_DOC_MIN_MN_IV =   1.00D0  !Monod type half-saturation concentration of DOC for DOC mineralization for Mn IV as the final electron acceptor
                  K_HS_DOC_MIN_FE_III =   1.00D0  !Monod type half-saturation concentration of DOC for DOC mineralization for Fe III as the final electron acceptor
                K_HS_DOC_MIN_S_PLUS_6 =   1.00D0  !Monod type half-saturation concentration of DOC for DOC mineralization for sulphate as the final electron accepto
                     K_HS_DOC_MIN_DOC =   1.00D0  !Monod type half-saturation concentration of DOC for DOC mineralization for DOC itself as the final electron accep
                    K_HS_DOXY_RED_LIM =   1.00D0  !Monod type half-saturation concentration of dissolved oxygen to limit the consumption of dissolved oxygen as the
                    K_HS_NO3N_RED_LIM =   1.00D0  !Monod type half-saturation concentration of nitrate nitrogen to limit the consumption of nitrate as the final ele
                   K_HS_MN_IV_RED_LIM =   1.00D0  !Monod type half-saturation concentration of MN_IV to limit the consumption of Mn IV as the final electron accepto
                  K_HS_FE_III_RED_LIM =   1.00D0  !Monod type half-saturation concentration of FE_III to limit the consumption of Fe III as the final electron accep
                K_HS_S_PLUS_6_RED_LIM =   1.00D0  !Monod type half-saturation concentration of sulphate to limit the consumption of sulphate as the final electron a
                   K_HS_DOXY_RED_INHB =   0.10D0  !Reversed Monod type half saturation concentration that simulate the inhibition effect of dissolved oxygen on cons
                   K_HS_NO3N_RED_INHB =   0.10D0  !Reversed Monod type half saturation concentration that simulate the inhibition effect of nitrate on consumption o
                  K_HS_MN_IV_RED_INHB =   0.10D0  !Reversed Monod type half saturation concentration that simulate the inhibition effect of Mn IV on consumption of
                 K_HS_FE_III_RED_INHB =   0.10D0  !Reversed Monod type half saturation concentration that simulate the inhibition effect of Fe III on consumption of
               K_HS_S_PLUS_6_RED_INHB =   0.10D0  !Reversed Monod type half saturation concentration that simulate the inhibition effect of S VI on consumption of D
                  PH_MIN_DOC_MIN_DOXY =   6.00D0  !Min. pH for the optimum pH range for DOC mineralization with DOXY as final electron acceptor
                  PH_MIN_DOC_MIN_NO3N =   6.00D0  !Min. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor
                 PH_MIN_DOC_MIN_MN_IV =   6.00D0  !Min. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor
                PH_MIN_DOC_MIN_FE_III =   6.00D0  !Min. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor
              PH_MIN_DOC_MIN_S_PLUS_6 =   6.00D0  !Min. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor
                   PH_MIN_DOC_MIN_DOC =   6.00D0  !Min. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor
                  PH_MAX_DOC_MIN_DOXY =   9.00D0  !Max. pH for the optimum pH range for DOC mineralization with dissolved oxygen as final electron acceptor
                  PH_MAX_DOC_MIN_NO3N =   9.00D0  !Max. pH for the optimum pH range for DOC mineralization with NO3N as final electron acceptor
                 PH_MAX_DOC_MIN_MN_IV =   9.00D0  !Max. pH for the optimum pH range for DOC mineralization with MN_IV as final electron acceptor
                PH_MAX_DOC_MIN_FE_III =   9.00D0  !Max. pH for the optimum pH range for DOC mineralization with FE_III as final electron acceptor
              PH_MAX_DOC_MIN_S_PLUS_6 =   9.00D0  !Max. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor
                   PH_MAX_DOC_MIN_DOC =   9.00D0  !Max. pH for the optimum pH range for DOC mineralization with DOC as final electron acceptor
                    K_MIN_DON_DOXY_20 =  0.100D0  !Mineralization rate constant of DON at 20 C for dissolved oxygen as final electron acceptor
                    K_MIN_DON_NO3N_20 = 0.0012D0  !Mineralization rate constant of DON at 20 C for NO3_N as final electron acceptor
                   K_MIN_DON_MN_IV_20 = 0.0012D0  !Mineralization rate constant of DON at 20 C for Mn IV as final electron acceptor
                  K_MIN_DON_FE_III_20 = 0.0012D0  !Mineralization rate constant of DON at 20 C for FE III as final electron acceptor
                K_MIN_DON_S_PLUS_6_20 = 0.0012D0  !Mineralization rate constant of DON at 20 C for S_PLUS_6 as final electron acceptor
                     K_MIN_DON_DOC_20 = 0.0012D0  !Mineralization rate constant of DON at 20 C for DOC as final electron acceptor
                 THETA_K_MIN_DON_DOXY =   1.08D0  !Temperature correction factor for mineralization process rate constant of DON for dissolved oxygen as final elect
                 THETA_K_MIN_DON_NO3N =   1.08D0  !Temperature correction factor for mineralization process rate constant of DON for nitrate as final electron accep
                THETA_K_MIN_DON_MN_IV =   1.08D0  !Temperature correction factor for mineralization process rate constant of DON for MN_IV as final electron accepto
               THETA_K_MIN_DON_FE_III =   1.08D0  !Temperature correction factor for mineralization process rate constant of DON for FE_III as final electron accept
             THETA_K_MIN_DON_S_PLUS_6 =   1.08D0  !Temperature correction factor for mineralization process rate constant of DON for sulphate as final electron acce
                  THETA_K_MIN_DON_DOC =   1.08D0  !Temperature correction factor for mineralization process rate constant of DON for DOC
                    K_HS_DON_MIN_DOXY =   0.05D0  !Monod type half-saturation concentration of DON for DON mineralization for dissolved oxygen as the final electron
                    K_HS_DON_MIN_NO3N =   0.05D0  !Monod type half-saturation concentration of DON for DON mineralization for nitrate nitrogen as the final electron
                   K_HS_DON_MIN_MN_IV =   0.05D0  !Monod type half-saturation concentration of DON for DON mineralization for MN_IV as the final electron acceptor
                  K_HS_DON_MIN_FE_III =   0.05D0  !Monod type half-saturation concentration of DON for DON mineralization for FE_III as the final electron acceptor
                K_HS_DON_MIN_S_PLUS_6 =   0.05D0  !Monod type half-saturation concentration of DON for DON mineralization for sulphate sulphur as the final electron
                     K_HS_DON_MIN_DOC =   0.05D0  !Monod type half-saturation concentration of DON for DON mineralization for DOC as the final electron acceptor
                  PH_MIN_DON_MIN_DOXY =   6.00D0  !Min. pH for the optimum pH range for DON mineralization with DOXY as final electron acceptor
                  PH_MIN_DON_MIN_NO3N =   6.00D0  !Min. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor
                 PH_MIN_DON_MIN_MN_IV =   6.00D0  !Min. pH for the optimum pH range for DON mineralization with MN IV as final electron acceptor
                PH_MIN_DON_MIN_FE_III =   6.00D0  !Min. pH for the optimum pH range for DON mineralization with FE III as final electron acceptor
              PH_MIN_DON_MIN_S_PLUS_6 =   6.00D0  !Min. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor
                   PH_MIN_DON_MIN_DOC =   6.00D0  !Min. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor
                  PH_MAX_DON_MIN_DOXY =   9.00D0  !Max. pH for the optimum pH range for DON mineralization with dissolved oxygen as final electron acceptor
                  PH_MAX_DON_MIN_NO3N =   9.00D0  !Max. pH for the optimum pH range for DON mineralization with NO3_N as final electron acceptor
                 PH_MAX_DON_MIN_MN_IV =   9.00D0  !Max. pH for the optimum pH range for DON mineralization with Mn IV as final electron acceptor
                PH_MAX_DON_MIN_FE_III =   9.00D0  !Max. pH for the optimum pH range for DON mineralization with FE_III as final electron acceptor
              PH_MAX_DON_MIN_S_PLUS_6 =   9.00D0  !Max. pH for the optimum pH range for DON mineralization with S_PLUS_6 as final electron acceptor
                   PH_MAX_DON_MIN_DOC =   9.00D0  !Max. pH for the optimum pH range for DON mineralization with DOC as final electron acceptor
                    K_MIN_DOP_DOXY_20 =   0.70D0  !Mineralization rate constant of DOP at 20 C for dissolved oxygen as final electron acceptor
                    K_MIN_DOP_NO3N_20 =   0.03D0  !Mineralization rate constant of DOP at 20 C for NO3_N as final electron acceptor
                   K_MIN_DOP_MN_IV_20 =   0.03D0  !Mineralization rate constant of DOP at 20 C for Mn IV as final electron acceptor
                  K_MIN_DOP_FE_III_20 =   0.03D0  !Mineralization rate constant of DOP at 20 C for FE_III as final electron acceptor
                K_MIN_DOP_S_PLUS_6_20 =   0.03D0  !Mineralization rate constant of DOP at 20 C for S_PLUS_6 as final electron acceptor
                     K_MIN_DOP_DOC_20 =   0.03D0  !Mineralization rate constant of DOP at 20 C for DOC as final electron acceptor
                 THETA_K_MIN_DOP_DOXY =   1.06D0  !Temperature correction factor for mineralization process rate constant of DOP for dissolved oxygen as final elect
                 THETA_K_MIN_DOP_NO3N =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOP for nitrate as final electron accep
                THETA_K_MIN_DOP_MN_IV =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOP for Mn IV as final electron accepto
               THETA_K_MIN_DOP_FE_III =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOP for FE_III as final electron accept
             THETA_K_MIN_DOP_S_PLUS_6 =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOP for sulphate as final electron acce
                  THETA_K_MIN_DOP_DOC =   1.04D0  !Temperature correction factor for mineralization process rate constant of DOP for DOC
                    K_HS_DOP_MIN_DOXY =  0.052D0  !Monod type half-saturation concentration of DOP for DOP mineralization for dissolved oxygen as the final electron
                    K_HS_DOP_MIN_NO3N =  0.052D0  !Monod type half-saturation concentration of DOP for DOP mineralization for nitrate nitrogen as the final electron
                   K_HS_DOP_MIN_MN_IV =  0.052D0  !Monod type half-saturation concentration of DOP for DOP mineralization for MN_IV as the final electron acceptor
                  K_HS_DOP_MIN_FE_III =  0.052D0  !Monod type half-saturation concentration of DOP for DOP mineralization for FE_III as the final electron acceptor
                K_HS_DOP_MIN_S_PLUS_6 =  0.052D0  !Monod type half-saturation concentration of DOP for DOP mineralization for sulphate sulphur as the final electron
                     K_HS_DOP_MIN_DOC =  0.052D0  !Monod type half-saturation concentration of DOP for DOP mineralization for DOC as the final electron acceptor
                  PH_MIN_DOP_MIN_DOXY =   6.00D0  !Min. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor
                  PH_MIN_DOP_MIN_NO3N =   6.00D0  !Min. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor
                 PH_MIN_DOP_MIN_MN_IV =   6.00D0  !Min. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor
                PH_MIN_DOP_MIN_FE_III =   6.00D0  !Min. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor
              PH_MIN_DOP_MIN_S_PLUS_6 =   6.00D0  !Min. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor
                   PH_MIN_DOP_MIN_DOC =   6.00D0  !Min. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor
                  PH_MAX_DOP_MIN_DOXY =   9.00D0  !Max. pH for the optimum pH range for DOP mineralization with dissolved oxygen as final electron acceptor
                  PH_MAX_DOP_MIN_NO3N =   9.00D0  !Max. pH for the optimum pH range for DOP mineralization with NO3_N as final electron acceptor
                 PH_MAX_DOP_MIN_MN_IV =   9.00D0  !Max. pH for the optimum pH range for DOP mineralization with Mn IV as final electron acceptor
                PH_MAX_DOP_MIN_FE_III =   9.00D0  !Max. pH for the optimum pH range for DOP mineralization with FE III as final electron acceptor
              PH_MAX_DOP_MIN_S_PLUS_6 =   9.00D0  !Max. pH for the optimum pH range for DOP mineralization with S_PLUS_6 as final electron acceptor
                   PH_MAX_DOP_MIN_DOC =   9.00D0  !Max. pH for the optimum pH range for DOP mineralization with DOC as final electron acceptor
                             k_OX_CH4 =    1.0D0  !Methane oxidation rate constant at 20 C
                       THETA_k_OX_CH4 =   1.04D0  !Arhenius type temperature correction for methane oxidation rate constant
                     k_HS_OX_CH4_DOXY =    1.5D0  !Monod type Half saturation of methane oxidation for dissolved oxygen
                             k_OX_H2S =    1.0D0  !Hydrogen sulphide oxidation rate constant at 20 C
                       THETA_k_OX_H2S =   1.04D0  !Arhenius type temperature correction for sulphide oxidation rate constant
                     k_HS_OX_H2S_DOXY =    1.5D0  !Monod type Half saturation of hydrogen sulphide oxidation for dissolved oxygen
                      k_DISS_FE_II_20 =    0.1D0  !Dissolution rate constant for Fe II at 20 C
                   THETA_k_DISS_FE_II =   1.04D0  !Dissolution rate constant for Fe II
                 INIT_MULT_FE_II_DISS =   0.01D0  !Initial fraction of dissolved Fe II
                     k_DISS_FE_III_20 =   0.10D0  !Dissolution rate constant for Fe III at 20 C
                  THETA_k_DISS_FE_III =   1.04D0  !Dissolution rate constant for Fe III
                INIT_MULT_FE_III_DISS =   0.50D0  !Initial fraction of dissolved Fe III
             KG_NOST_VEG_HET_OPT_TEMP =   1.29D0  !Nostocales (veg + het) Growth rate constant
             NOST_VEG_HET_OPT_TEMP_LR =   16.0D0  !Nostocales (veg + het) CTMI T_min
             NOST_VEG_HET_OPT_TEMP_UR =   26.0D0  !Nostocales (veg + het) CTMI T_opt
              EFF_NOST_VEG_HET_GROWTH =   0.95D0  !Nostocales (veg + het) Effective growth. (1-EG)*growth - losses for RESP and excretion
    KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP =   0.0D0   !(unused)
     KAPPA_NOST_VEG_HET_OVER_OPT_TEMP =  38.0D0   !Nostocales (veg + het) CTMI T_max
                   KR_NOST_VEG_HET_20 =   0.06D0  !Nostocales (veg + het) RESP rate constant
                THETA_KR_NOST_VEG_HET =   1.04D0  !Nostocales (veg + het) Temperature correction for RESP rate
                   KD_NOST_VEG_HET_20 =  0.040D0  !Nostocales (veg + het) Mortality rate constant
                THETA_KD_NOST_VEG_HET =   1.05D0  !Nostocales (veg + het) Temperature correction for Mortality rate
                       M_DENS_VEG_HET =  0.001D0  !Nostocales (veg + het) mortality rate constant because of too dense population
                  KHS_DP_NOST_VEG_HET =  0.005D0  !Nostocales (veg + het) Half saturation growth for DP
                  KHS_O2_NOST_VEG_HET =   0.60D0  !Nostocales (veg + het) Half saturation growth for O2
               FRAC_NOST_VEG_HET_EXCR =   0.30D0  !Nostocales (veg + het) Fraction of excretion in metabolism rate
                     I_S_NOST_VEG_HET =   100.D0  !Nostocales (veg + het) Light saturation (langleys). Not used, is calculated!
          DO_STR_HYPOX_NOST_VEG_HET_D =   0.70D0  !Nostocales (veg + het) Dissolved oxygen stress in oxygen units (mortality increase below this value exponentialy
           THETA_HYPOX_NOST_VEG_HET_D =   1.50D0  !Nostocales (veg + het) Multiplier of the exponent for Dissolved oxygen stress
           EXPON_HYPOX_NOST_VEG_HET_D =   1.10D0  !Nostocales (veg + het) Exponent constant for Dissolved oxygen stress
                          NOST_N_TO_C =  0.220D0  !Nostocales (veg + het) Nitrogen to Carbon ratio
                          NOST_P_TO_C =  0.024D0  !Nostocales (veg + het) Phosphorus to Carbon ratio
                         NOST_O2_TO_C =   2.66D0  !Nostocales (veg + het) Oxygen to Carbon ratio for respiration
                       NOST_C_TO_CHLA =  40.00D0  !Nostocales (veg + het) Carbon to Chlorophyl a ratio
                           P_GERM_AKI =    0.3D0  !Germination rate constant for akinets, 1/day
                           N_GERM_AKI =    0.1D0  !DIN concentration to trigger germination (below that germination starts)
                           P_FORM_AKI =    0.1D0  !Formation rate constant for akinets
                         DAY_FORM_AKI =    200  !Day of the year from which formation of akinetes is allowed
                           T_FORM_AKI =   16.0D0  !Temperature to trigger formation (below that formation starts)
                           K_LOSS_AKI =  0.000D0  !Loss rate constant for akinetes
                        K_MORT_AKI_20 =  0.000D0  !Mortality rate constant of akinetes at 20 degrees celsius
                     THETA_K_MORT_AKI =  1.020D0  !Arhenius temperature correction factor for akinete mortality rate constant
                           T_GERM_AKI =   21.0D0  !Temperature to trigger germination when DIN is also low
                     KHS_POC_DISS_SAT =  1.250D0
                     KHS_PON_DISS_SAT =  0.250D0
                     KHS_POP_DISS_SAT = 0.0250D0
                       frac_avail_DON =   0.00D0
                       frac_avail_DOP =   0.00D0
                  frac_avail_DON_NOST =    0.0D0
                  KHS_DN_NOST_VEG_HET = 0.0072D0
            FRAC_FIX_N_FOR_GR_VEG_HET =  0.650D0
                     FRAC_NOST_GROWTH =   0.10D0
                  K_MIN_PHYT_AMIN_DOC =   4.00D0
                  K_MIN_PHYT_AMIN_DON =   4.00D0
                  K_MIN_PHYT_AMIN_DOP =   4.00D0
                             BETA_DIA =   0.00D0  !Diatoms photoinhibition (0=Steele default, >0=stronger inhibition)
                             BETA_CYN =   0.00D0  !Non-fixing cyanobacteria photoinhibition
                         BETA_FIX_CYN =   0.00D0  !Fixing cyanobacteria photoinhibition
                             BETA_OPA =   0.00D0  !OtherPhyto photoinhibition
                    BETA_NOST_VEG_HET =   0.00D0  !Nostocales photoinhibition
                      ICE_LIGHT_TRANS =   0.05D0  !under-ice PAR transmittance (snow-covered ice)

end subroutine DEFAULT_PELAGIC_MODEL_CONSTANTS

!--------------------------------------------------------------------------

subroutine READ_PELAGIC_MODEL_CONSTANTS(file)

    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use para_aqua

    implicit none

    character*(*) file

    integer iu,i,j,ios,nconsts
    character*80 name,format
    double precision value

    iu = 77
    format='(2i5,d14.6,2x,a)'

    write(6,*) 'reading constants file ',trim(file)

    open(iu,file=file,status='old',form='formatted',iostat=ios)
    if( ios /= 0 ) then
        write(6,*) 'cannot open file ',trim(file)
        stop 'error stop READ_PELAGIC_MODEL_CONSTANTS: no such file'
    end if

    nconsts = para_get_fill()

    j = 0
    do
      read(iu,*,iostat=ios) i,name,value
      if( ios < 0 ) exit
      j = j + 1
      call para_put_value(name,value)
    end do

    close(iu)

    write(6,*) j,' constants from file read'

    if( j /= nconsts ) then
      write(6,*) 'error in file ',trim(file)
      write(6,*) '  number of constants expected: ',nconsts
      write(6,*) '  number of constants read:     ',j
      !stop 'error stop READ_PELAGIC_MODEL_CONSTANTS: input file'
    end if

end subroutine READ_PELAGIC_MODEL_CONSTANTS

!--------------------------------------------------------------------------

subroutine WRITE_PELAGIC_MODEL_CONSTANTS(file)

    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use para_aqua

    implicit none

    character*(*) file

    integer iu,i,ios,nconsts
    character*80 name,format
    character*35 rname
    double precision value

    iu = 77
    format='(i6,a,3x,d14.6)'

    write(6,*) 'writing constants file ',trim(file)

    open(iu,file=file,status='unknown',form='formatted',iostat=ios)
    if( ios /= 0 ) then
        write(6,*) 'cannot open file ',trim(file)
        stop 'error stop WRITE_PELAGIC_MODEL_CONSTANTS: cannot open file'
    end if

    nconsts = para_get_fill()

    do i=1,nconsts
      call para_get_name(i,name)
      call para_get_value(name,value)
      rname = name(1:35)
      rname = adjustr(rname)
      write(iu,format) i,rname,value
    end do

    close(iu)

    write(6,*) nfill,' constants to file written'

end subroutine WRITE_PELAGIC_MODEL_CONSTANTS

!--------------------------------------------------------------------------