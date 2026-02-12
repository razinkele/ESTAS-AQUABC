! module SED_MODEL_CONSTANTS
! subroutine INIT_SED_MODEL_CONSTANTS

module AQUABC_BSED_MODEL_CONSTANTS
    use AQUABC_II_GLOBAL
    implicit none

    !SEDIMENT MODEL COEFFICIENTS
    !Dissolution of particulate organic carbon
    real(kind = DBL_PREC) K_OXIC_DISS_POC
    real(kind = DBL_PREC) K_ANOXIC_DISS_POC
    real(kind = DBL_PREC) THETA_DISS_POC
    real(kind = DBL_PREC) KHS_DISS_POC

    !Dissolution of particulate organic nitrogen
    real(kind = DBL_PREC) K_OXIC_DISS_PON
    real(kind = DBL_PREC) K_ANOXIC_DISS_PON
    real(kind = DBL_PREC) THETA_DISS_PON
    real(kind = DBL_PREC) KHS_DISS_PON

    !Dissolution of particulate organic phosphorus
    real(kind = DBL_PREC) K_OXIC_DISS_POP
    real(kind = DBL_PREC) K_ANOXIC_DISS_POP
    real(kind = DBL_PREC) THETA_DISS_POP
    real(kind = DBL_PREC) KHS_DISS_POP

    !Dissolution of particulate silicon
    real(kind = DBL_PREC) K_OXIC_DISS_PSi
    real(kind = DBL_PREC) K_ANOXIC_DISS_PSi
    real(kind = DBL_PREC) THETA_DISS_PSi
    real(kind = DBL_PREC) KHS_DISS_PSi

    !Mineralization of dissolved organic carbon
    real(kind = DBL_PREC) K_OXIC_MINER_DOC
    real(kind = DBL_PREC) K_ANOXIC_MINER_DOC
    real(kind = DBL_PREC) THETA_MINER_DOC
    real(kind = DBL_PREC) KHS_MINER_DOC
    real(kind = DBL_PREC) O_TO_C

    !Mineralization of dissolved organic nitrogen
    real(kind = DBL_PREC) K_OXIC_MINER_DON
    real(kind = DBL_PREC) K_ANOXIC_MINER_DON
    real(kind = DBL_PREC) THETA_MINER_DON
    real(kind = DBL_PREC) KHS_MINER_DON

    !Mineralization of dissolved organic phosphorus
    real(kind = DBL_PREC) K_OXIC_MINER_DOP
    real(kind = DBL_PREC) K_ANOXIC_MINER_DOP
    real(kind = DBL_PREC) THETA_MINER_DOP
    real(kind = DBL_PREC) KHS_MINER_DOP

    !Nitrification
    real(kind = DBL_PREC) K_NITR
    real(kind = DBL_PREC) THETA_NITR
    real(kind = DBL_PREC) KHS_NITR_NH4N
    real(kind = DBL_PREC) KHS_NITR_DOXY

    !Denitrification
    real(kind = DBL_PREC) K_DENITR
    real(kind = DBL_PREC) THETA_DENITR
    real(kind = DBL_PREC) KHS_DENITR_NO3N
    real(kind = DBL_PREC) KHS_DENITR_DOC
    real(kind = DBL_PREC) KHS_DENITR_DOXY
    real(kind = DBL_PREC) DENITR_YIELD

    !Anoxia
    real(kind = DBL_PREC) DOXY_AT_ANOXIA

    !Solid partition
    real(kind = DBL_PREC) SOLID_PART_COEFF_NH4
    real(kind = DBL_PREC) SOLID_PART_COEFF_PO4

    real(kind = DBL_PREC) SED_PH_MIN_DOC_MIN
    real(kind = DBL_PREC) SED_PH_MIN_DOC_MAX
    real(kind = DBL_PREC) SED_PH_MIN_DON_MIN
    real(kind = DBL_PREC) SED_PH_MIN_DON_MAX
    real(kind = DBL_PREC) SED_PH_MIN_DOP_MIN
    real(kind = DBL_PREC) SED_PH_MIN_DOP_MAX
    real(kind = DBL_PREC) SED_PH_NITR_NH4_MIN
    real(kind = DBL_PREC) SED_PH_NITR_NH4_MAX
    real(kind = DBL_PREC) SED_PH_DENITR_NO3_MIN
    real(kind = DBL_PREC) SED_PH_DENITR_NO3_MAX

    !New model constants added in 10th of September 2015
    real(kind = DBL_PREC) :: SED_k_OX_FE_II
    real(kind = DBL_PREC) :: SED_k_RED_FE_III
    real(kind = DBL_PREC) :: SED_k_OX_MN_II
    real(kind = DBL_PREC) :: SED_k_RED_MN_IV
    real(kind = DBL_PREC) :: SED_KHS_DOXY_FE_III_RED
    real(kind = DBL_PREC) :: SED_KHS_DOXY_MN_IV_RED
    !End of new model constants added in 10th of September 2015

    ! New model constats added in 28 th of January 2016
    real(kind = DBL_PREC) :: SED_K_MIN_DOC_DOXY_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOC_NO3N_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOC_MN_IV_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOC_FE_III_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOC_S_PLUS_6_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOC_DOC_20
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOC_DOXY
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOC_NO3N
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOC_MN_IV
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOC_FE_III
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOC_S_PLUS_6
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOC_DOC
    real(kind = DBL_PREC) :: SED_K_HS_DOC_MIN_DOXY
    real(kind = DBL_PREC) :: SED_K_HS_DOC_MIN_NO3N
    real(kind = DBL_PREC) :: SED_K_HS_DOC_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_K_HS_DOC_MIN_FE_III
    real(kind = DBL_PREC) :: SED_K_HS_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_K_HS_DOC_MIN_DOC
    real(kind = DBL_PREC) :: SED_K_HS_DOXY_RED_LIM
    real(kind = DBL_PREC) :: SED_K_HS_NO3N_RED_LIM
    real(kind = DBL_PREC) :: SED_K_HS_MN_IV_RED_LIM
    real(kind = DBL_PREC) :: SED_K_HS_FE_III_RED_LIM
    real(kind = DBL_PREC) :: SED_K_HS_S_PLUS_6_RED_LIM
    real(kind = DBL_PREC) :: SED_K_HS_DOXY_RED_INHB
    real(kind = DBL_PREC) :: SED_K_HS_NO3N_RED_INHB
    real(kind = DBL_PREC) :: SED_K_HS_MN_IV_RED_INHB
    real(kind = DBL_PREC) :: SED_K_HS_FE_III_RED_INHB
    real(kind = DBL_PREC) :: SED_K_HS_S_PLUS_6_RED_INHB
    real(kind = DBL_PREC) :: SED_PH_MIN_DOC_MIN_DOXY
    real(kind = DBL_PREC) :: SED_PH_MIN_DOC_MIN_NO3N
    real(kind = DBL_PREC) :: SED_PH_MIN_DOC_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_PH_MIN_DOC_MIN_FE_III
    real(kind = DBL_PREC) :: SED_PH_MIN_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_PH_MIN_DOC_MIN_DOC
    real(kind = DBL_PREC) :: SED_PH_MAX_DOC_MIN_DOXY
    real(kind = DBL_PREC) :: SED_PH_MAX_DOC_MIN_NO3N
    real(kind = DBL_PREC) :: SED_PH_MAX_DOC_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_PH_MAX_DOC_MIN_FE_III
    real(kind = DBL_PREC) :: SED_PH_MAX_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_PH_MAX_DOC_MIN_DOC

    real(kind = DBL_PREC) :: SED_K_MIN_DON_DOXY_20
    real(kind = DBL_PREC) :: SED_K_MIN_DON_NO3N_20
    real(kind = DBL_PREC) :: SED_K_MIN_DON_MN_IV_20
    real(kind = DBL_PREC) :: SED_K_MIN_DON_FE_III_20
    real(kind = DBL_PREC) :: SED_K_MIN_DON_S_PLUS_6_20
    real(kind = DBL_PREC) :: SED_K_MIN_DON_DOC_20
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DON_DOXY
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DON_NO3N
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DON_MN_IV
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DON_FE_III
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DON_S_PLUS_6
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DON_DOC
    real(kind = DBL_PREC) :: SED_K_HS_DON_MIN_DOXY
    real(kind = DBL_PREC) :: SED_K_HS_DON_MIN_NO3N
    real(kind = DBL_PREC) :: SED_K_HS_DON_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_K_HS_DON_MIN_FE_III
    real(kind = DBL_PREC) :: SED_K_HS_DON_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_K_HS_DON_MIN_DOC
    real(kind = DBL_PREC) :: SED_PH_MIN_DON_MIN_DOXY
    real(kind = DBL_PREC) :: SED_PH_MIN_DON_MIN_NO3N
    real(kind = DBL_PREC) :: SED_PH_MIN_DON_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_PH_MIN_DON_MIN_FE_III
    real(kind = DBL_PREC) :: SED_PH_MIN_DON_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_PH_MIN_DON_MIN_DOC
    real(kind = DBL_PREC) :: SED_PH_MAX_DON_MIN_DOXY
    real(kind = DBL_PREC) :: SED_PH_MAX_DON_MIN_NO3N
    real(kind = DBL_PREC) :: SED_PH_MAX_DON_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_PH_MAX_DON_MIN_FE_III
    real(kind = DBL_PREC) :: SED_PH_MAX_DON_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_PH_MAX_DON_MIN_DOC

    real(kind = DBL_PREC) :: SED_K_MIN_DOP_DOXY_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOP_NO3N_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOP_MN_IV_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOP_FE_III_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOP_S_PLUS_6_20
    real(kind = DBL_PREC) :: SED_K_MIN_DOP_DOC_20
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOP_DOXY
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOP_NO3N
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOP_MN_IV
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOP_FE_III
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOP_S_PLUS_6
    real(kind = DBL_PREC) :: SED_THETA_K_MIN_DOP_DOC
    real(kind = DBL_PREC) :: SED_K_HS_DOP_MIN_DOXY
    real(kind = DBL_PREC) :: SED_K_HS_DOP_MIN_NO3N
    real(kind = DBL_PREC) :: SED_K_HS_DOP_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_K_HS_DOP_MIN_FE_III
    real(kind = DBL_PREC) :: SED_K_HS_DOP_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_K_HS_DOP_MIN_DOC
    real(kind = DBL_PREC) :: SED_PH_MIN_DOP_MIN_DOXY
    real(kind = DBL_PREC) :: SED_PH_MIN_DOP_MIN_NO3N
    real(kind = DBL_PREC) :: SED_PH_MIN_DOP_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_PH_MIN_DOP_MIN_FE_III
    real(kind = DBL_PREC) :: SED_PH_MIN_DOP_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_PH_MIN_DOP_MIN_DOC
    real(kind = DBL_PREC) :: SED_PH_MAX_DOP_MIN_DOXY
    real(kind = DBL_PREC) :: SED_PH_MAX_DOP_MIN_NO3N
    real(kind = DBL_PREC) :: SED_PH_MAX_DOP_MIN_MN_IV
    real(kind = DBL_PREC) :: SED_PH_MAX_DOP_MIN_FE_III
    real(kind = DBL_PREC) :: SED_PH_MAX_DOP_MIN_S_PLUS_6
    real(kind = DBL_PREC) :: SED_PH_MAX_DOP_MIN_DOC
    ! End of new model constats added in 28 th of January 2016

    ! New model constats added in 29 th of January 2016
    real(kind = DBL_PREC) :: SED_k_OX_CH4
    real(kind = DBL_PREC) :: SED_THETA_k_OX_CH4
    real(kind = DBL_PREC) :: SED_k_HS_OX_CH4_DOXY
    real(kind = DBL_PREC) :: SED_k_OX_H2S
    real(kind = DBL_PREC) :: SED_THETA_k_OX_H2S
    real(kind = DBL_PREC) :: SED_k_HS_OX_H2S_DOXY
    ! End of new model constats added in 29 th of January 2016

    real(kind = DBL_PREC) :: SED_k_DISS_FE_II_20
    real(kind = DBL_PREC) :: SED_THETA_k_DISS_FE_II
    real(kind = DBL_PREC) :: SED_INIT_MULT_FE_II_DISS

    real(kind = DBL_PREC) :: SED_k_DISS_FE_III_20
    real(kind = DBL_PREC) :: SED_THETA_k_DISS_FE_III
    real(kind = DBL_PREC) :: SED_INIT_MULT_FE_III_DISS


    ! END OF COEFFICIENTS
end module AQUABC_BSED_MODEL_CONSTANTS



subroutine INIT_BSED_MODEL_CONSTANTS
    use AQUABC_BSED_MODEL_CONSTANTS
    use para_aqua

    !Constants
        implicit none
    call para_get_value('K_OXIC_DISS_POC'       , K_OXIC_DISS_POC      ) !1 ! PARTICULATE ORGANIC CARBON   Dissolution rate constant  at 20 C (aerobic) - 1/day
    call para_get_value('K_ANOXIC_DISS_POC'     , K_ANOXIC_DISS_POC    ) !2 ! PARTICULATE ORGANIC CARBON   Dissolution rate constant n at 20 C (anoxic) - 1/day
    call para_get_value('THETA_DISS_POC'        , THETA_DISS_POC       ) !3 ! PARTICULATE ORGANIC CARBON   Temperature correction for dissolution
    call para_get_value('KHS_DISS_POC'          , KHS_DISS_POC         ) !4 ! PARTICULATE ORGANIC CARBON   Half saturation concentration  for dissolution
    call para_get_value('K_OXIC_DISS_PON'       , K_OXIC_DISS_PON      ) !5 ! PARTICULATE ORGANIC NITROGEN  Dissolution rate constant  at 20 C (aerobic) - 1/day
    call para_get_value('K_ANOXIC_DISS_PON'     , K_ANOXIC_DISS_PON    ) !6 ! PARTICULATE ORGANIC NITROGEN  Dissolution rate constant  at 20 C (anoxic) - 1/day
    call para_get_value('THETA_DISS_PON'        , THETA_DISS_PON       ) !7 ! PARTICULATE ORGANIC NITROGEN  Temperature correction for dissolution
    call para_get_value('KHS_DISS_PON'          , KHS_DISS_PON         ) !8 ! PARTICULATE ORGANIC NITROGEN  Half saturation concentration  for dissolution
    call para_get_value('K_OXIC_DISS_POP'       , K_OXIC_DISS_POP      ) !9 ! PARTICULATE ORGANIC PHOSPHORUS  Dissolution rate constant  at 20 C (aerobic) - 1/day
    call para_get_value('K_ANOXIC_DISS_POP'     , K_ANOXIC_DISS_POP    ) !10! PARTICULATE ORGANIC PHOSPHORUS  Dissolution rate constant  at 20 C (anoxic) - 1/day
    call para_get_value('THETA_DISS_POP'        , THETA_DISS_POP       ) !11! PARTICULATE ORGANIC PHOSPHORUS  Temperature correction for dissolution
    call para_get_value('KHS_DISS_POP'          , KHS_DISS_POP         ) !12! PARTICULATE ORGANIC PHOSPHORUS  Half saturation concentration   for dissolution
    call para_get_value('K_OXIC_DISS_PSi'       , K_OXIC_DISS_PSi      ) !13! PARTICULATE SILICON    Dissolution rate constant  at 20 C (aerobic) - 1/day
    call para_get_value('K_ANOXIC_DISS_PSi'     , K_ANOXIC_DISS_PSi    ) !14! PARTICULATE SILICON    Dissolution rate constant  at 20 C (anoxic) - 1/day
    call para_get_value('THETA_DISS_PSi'        , THETA_DISS_PSi       ) !15! PARTICULATE SILICON    Temperature correction for dissolution
    call para_get_value('KHS_DISS_PSi'          , KHS_DISS_PSi         ) !16! PARTICULATE SILICON    Half saturation concentration  for dissolution
    call para_get_value('K_OXIC_MINER_DOC'      , K_OXIC_MINER_DOC     ) !17! DISSOLVED ORGANIC CARBON   Mineralization rate constant  at 20 C (aerobic) - 1/day
    call para_get_value('K_ANOXIC_MINER_DOC'    , K_ANOXIC_MINER_DOC   ) !18! DISSOLVED ORGANIC CARBON   Mineralization rate constant  at 20 C (anoxic) - 1/day
    call para_get_value('THETA_MINER_DOC'       , THETA_MINER_DOC      ) !19! DISSOLVED ORGANIC CARBON   Temperature correction for dissolution
    call para_get_value('KHS_MINER_DOC'         , KHS_MINER_DOC        ) !20! DISSOLVED ORGANIC CARBON   Half saturation concentration  for mineralization
    call para_get_value('K_OXIC_MINER_DON'      , K_OXIC_MINER_DON     ) !21! DISSOLVED ORGANIC NITROGEN  Mineralization rate constant  at 20 C (aerobic) - 1/day
    call para_get_value('K_ANOXIC_MINER_DON'    , K_ANOXIC_MINER_DON   ) !22! DISSOLVED ORGANIC NITROGEN  Mineralization rate constant  at 20 C (anoxic) - 1/day
    call para_get_value('THETA_MINER_DON'       , THETA_MINER_DON      ) !23! DISSOLVED ORGANIC NITROGEN  Temperature correction
    call para_get_value('KHS_MINER_DON'         , KHS_MINER_DON        ) !24! DISSOLVED ORGANIC NITROGEN  Half saturation concentration  for mineralization
    call para_get_value('K_OXIC_MINER_DOP'      , K_OXIC_MINER_DOP     ) !25! DISSOLVED ORGANIC PHOSPHORUS Mineralization rate constant   at 20 C (aerobic) - 1/day
    call para_get_value('K_ANOXIC_MINER_DOP'    , K_ANOXIC_MINER_DOP   ) !26! DISSOLVED ORGANIC PHOSPHORUS Mineralization rate constant   at 20 C (anoxic) - 1/day
    call para_get_value('THETA_MINER_DOP'       , THETA_MINER_DOP      ) !27! DISSOLVED ORGANIC PHOSPHORUS Temperature correction for dissolution
    call para_get_value('KHS_MINER_DOP'         , KHS_MINER_DOP        ) !28! DISSOLVED ORGANIC PHOSPHORUS Half saturation concentration  for mineralization
    call para_get_value('O_TO_C'                , O_TO_C               ) !29! Oxygen to carbon ratio
    call para_get_value('K_NITR'                , K_NITR               ) !30! Nitrification rate constant at 20 C - 1/day
    call para_get_value('THETA_NITR'            , THETA_NITR           ) !31! Temperature correction for nitrification
    call para_get_value('KHS_NITR_NH4N'         , KHS_NITR_NH4N        ) !32! Half saturation constant of nitrification for NH4N - mg/L N
    call para_get_value('KHS_NITR_DOXY'         , KHS_NITR_DOXY        ) !33! Half saturation constant of nitrification for DOXY - mg/L O2
    call para_get_value('K_DENITR'              , K_DENITR             ) !34! Denitrification rate constant at 20 C - 1/day
    call para_get_value('THETA_DENITR'          , THETA_DENITR         ) !35! Temperature correction for denitrification
    call para_get_value('KHS_DENITR_NO3N'       , KHS_DENITR_NO3N      ) !36! Half saturation constant of denitrification for NO3N - mg/L N
    call para_get_value('KHS_DENITR_DOC'        , KHS_DENITR_DOC       ) !37! Half saturation constant of denitrification for DOC - mg/L C
    call para_get_value('KHS_DENITR_DOXY'       , KHS_DENITR_DOXY      ) !38! Half saturation constant of denitrification for DOXY - mg/L O
    call para_get_value('DENITR_YIELD'          , DENITR_YIELD         ) !39! Denitrification yield
    call para_get_value('DOXY_AT_ANOXIA'        , DOXY_AT_ANOXIA       ) !40! DOXY, under which anoxia begins - mg/L O2
    call para_get_value('SOLID_PART_COEFF_NH4'  , SOLID_PART_COEFF_NH4 ) !41! Solid part coeff for ammonium nitrogen (kg^-1)
    call para_get_value('SOLID_PART_COEFF_PO4'  , SOLID_PART_COEFF_PO4 ) !42! Solid part coeff for phosphate phosphorus (kg^-1)

    call para_get_value('SED_PH_MIN_DOC_MIN'    , SED_PH_MIN_DOC_MIN   ) !43!   optimum lower range for pH correction factor for DOC
    call para_get_value('SED_PH_MIN_DOC_MAX'    , SED_PH_MIN_DOC_MAX   ) !44!   optimum upper range for pH correction factor for DOC
    call para_get_value('SED_PH_MIN_DON_MIN'    , SED_PH_MIN_DON_MIN   ) !45!   optimum lower range for pH correction factor for DON
    call para_get_value('SED_PH_MIN_DON_MAX'    , SED_PH_MIN_DON_MAX   ) !46!   optimum upper range for pH correction factor for DON
    call para_get_value('SED_PH_MIN_DOP_MIN'    , SED_PH_MIN_DOP_MIN   ) !47!   optimum lower range for pH correction factor for DOP
    call para_get_value('SED_PH_MIN_DOP_MAX'    , SED_PH_MIN_DOP_MAX   ) !48!   optimum upper range for pH correction factor for DOP
    call para_get_value('SED_PH_NITR_NH4_MIN'   , SED_PH_NITR_NH4_MIN  ) !49!   optimum lower range for pH correction factor for nitrification
    call para_get_value('SED_PH_NITR_NH4_MAX'   , SED_PH_NITR_NH4_MAX  ) !50!   optimum upper range for pH correction factor for nitrification
    call para_get_value('SED_PH_DENITR_NO3_MIN' , SED_PH_DENITR_NO3_MIN) !51!   optimum lower range for pH correction factor for denitrification
    call para_get_value('SED_PH_DENITR_NO3_MAX' , SED_PH_DENITR_NO3_MAX) !52!   optimum upper range for pH correction factor for denitrification

    call para_get_value('SED_k_OX_FE_II'          , SED_k_OX_FE_II         ) !Introduced 10 09M 2015   Oxidation rate for iron 2+
    call para_get_value('SED_k_RED_FE_III'        , SED_k_RED_FE_III       ) !Introduced 10 09M 2015   reduction rate for iron 3+
    call para_get_value('SED_k_OX_MN_II'          , SED_k_OX_MN_II         ) !Introduced 10 09M 2015   oxidation rate for manganese 2+
    call para_get_value('SED_k_RED_MN_IV'         , SED_k_RED_MN_IV        ) !Introduced 10 09M 2015   reduction rate for manganese 4+
    call para_get_value('SED_KHS_DOXY_FE_III_RED' , SED_KHS_DOXY_FE_III_RED) !Introduced 10 09M 2015   reversed Monod half saturation of DOXY for iron 3+ reduction
    call para_get_value('SED_KHS_DOXY_MN_IV_RED'  , SED_KHS_DOXY_MN_IV_RED ) !Introduced 10 09M 2015   reversed Monod half saturation of DOXY for manganese 4+ reduction

    ! New model constats added in 28 th of January 2016
    call para_get_value('SED_K_MIN_DOC_DOXY_20'         , SED_K_MIN_DOC_DOXY_20        )
    call para_get_value('SED_K_MIN_DOC_NO3N_20'         , SED_K_MIN_DOC_NO3N_20        )
    call para_get_value('SED_K_MIN_DOC_MN_IV_20'        , SED_K_MIN_DOC_MN_IV_20       )
    call para_get_value('SED_K_MIN_DOC_FE_III_20'       , SED_K_MIN_DOC_FE_III_20      )
    call para_get_value('SED_K_MIN_DOC_S_PLUS_6_20'     , SED_K_MIN_DOC_S_PLUS_6_20    )
    call para_get_value('SED_K_MIN_DOC_DOC_20'          , SED_K_MIN_DOC_DOC_20         )
    call para_get_value('SED_THETA_K_MIN_DOC_DOXY'      , SED_THETA_K_MIN_DOC_DOXY     )
    call para_get_value('SED_THETA_K_MIN_DOC_NO3N'      , SED_THETA_K_MIN_DOC_NO3N     )
    call para_get_value('SED_THETA_K_MIN_DOC_MN_IV'     , SED_THETA_K_MIN_DOC_MN_IV    )
    call para_get_value('SED_THETA_K_MIN_DOC_FE_III'    , SED_THETA_K_MIN_DOC_FE_III   )
    call para_get_value('SED_THETA_K_MIN_DOC_S_PLUS_6'  , SED_THETA_K_MIN_DOC_S_PLUS_6 )
    call para_get_value('SED_THETA_K_MIN_DOC_DOC'       , SED_THETA_K_MIN_DOC_DOC      )
    call para_get_value('SED_K_HS_DOC_MIN_DOXY'         , SED_K_HS_DOC_MIN_DOXY        )
    call para_get_value('SED_K_HS_DOC_MIN_NO3N'         , SED_K_HS_DOC_MIN_NO3N        )
    call para_get_value('SED_K_HS_DOC_MIN_MN_IV'        , SED_K_HS_DOC_MIN_MN_IV       )
    call para_get_value('SED_K_HS_DOC_MIN_FE_III'       , SED_K_HS_DOC_MIN_FE_III      )
    call para_get_value('SED_K_HS_DOC_MIN_S_PLUS_6'     , SED_K_HS_DOC_MIN_S_PLUS_6    )
    call para_get_value('SED_K_HS_DOC_MIN_DOC'          , SED_K_HS_DOC_MIN_DOC         )
    call para_get_value('SED_K_HS_DOXY_RED_LIM'         , SED_K_HS_DOXY_RED_LIM        )
    call para_get_value('SED_K_HS_NO3N_RED_LIM'         , SED_K_HS_NO3N_RED_LIM        )
    call para_get_value('SED_K_HS_MN_IV_RED_LIM'        , SED_K_HS_MN_IV_RED_LIM       )
    call para_get_value('SED_K_HS_FE_III_RED_LIM'       , SED_K_HS_FE_III_RED_LIM      )
    call para_get_value('SED_K_HS_S_PLUS_6_RED_LIM'     , SED_K_HS_S_PLUS_6_RED_LIM    )
    call para_get_value('SED_K_HS_DOXY_RED_INHB'        , SED_K_HS_DOXY_RED_INHB       )
    call para_get_value('SED_K_HS_NO3N_RED_INHB'        , SED_K_HS_NO3N_RED_INHB       )
    call para_get_value('SED_K_HS_MN_IV_RED_INHB'       , SED_K_HS_MN_IV_RED_INHB      )
    call para_get_value('SED_K_HS_FE_III_RED_INHB'      , SED_K_HS_FE_III_RED_INHB     )
    call para_get_value('SED_K_HS_S_PLUS_6_RED_INHB'    , SED_K_HS_S_PLUS_6_RED_INHB   )
    call para_get_value('SED_PH_MIN_DOC_MIN_DOXY'       , SED_PH_MIN_DOC_MIN_DOXY      )
    call para_get_value('SED_PH_MIN_DOC_MIN_NO3N'       , SED_PH_MIN_DOC_MIN_NO3N      )
    call para_get_value('SED_PH_MIN_DOC_MIN_MN_IV'      , SED_PH_MIN_DOC_MIN_MN_IV     )
    call para_get_value('SED_PH_MIN_DOC_MIN_FE_III'     , SED_PH_MIN_DOC_MIN_FE_III    )
    call para_get_value('SED_PH_MIN_DOC_MIN_S_PLUS_6'   , SED_PH_MIN_DOC_MIN_S_PLUS_6  )
    call para_get_value('SED_PH_MIN_DOC_MIN_DOC'        , SED_PH_MIN_DOC_MIN_DOC       )
    call para_get_value('SED_PH_MAX_DOC_MIN_DOXY'       , SED_PH_MAX_DOC_MIN_DOXY      )
    call para_get_value('SED_PH_MAX_DOC_MIN_NO3N'       , SED_PH_MAX_DOC_MIN_NO3N      )
    call para_get_value('SED_PH_MAX_DOC_MIN_MN_IV'      , SED_PH_MAX_DOC_MIN_MN_IV     )
    call para_get_value('SED_PH_MAX_DOC_MIN_FE_III'     , SED_PH_MAX_DOC_MIN_FE_III    )
    call para_get_value('SED_PH_MAX_DOC_MIN_S_PLUS_6'   , SED_PH_MAX_DOC_MIN_S_PLUS_6  )
    call para_get_value('SED_PH_MAX_DOC_MIN_DOC'        , SED_PH_MAX_DOC_MIN_DOC       )
    call para_get_value('SED_K_MIN_DON_DOXY_20'         , SED_K_MIN_DON_DOXY_20        )
    call para_get_value('SED_K_MIN_DON_NO3N_20'         , SED_K_MIN_DON_NO3N_20        )
    call para_get_value('SED_K_MIN_DON_MN_IV_20'        , SED_K_MIN_DON_MN_IV_20       )
    call para_get_value('SED_K_MIN_DON_FE_III_20'       , SED_K_MIN_DON_FE_III_20      )
    call para_get_value('SED_K_MIN_DON_S_PLUS_6_20'     , SED_K_MIN_DON_S_PLUS_6_20    )
    call para_get_value('SED_K_MIN_DON_DOC_20'          , SED_K_MIN_DON_DOC_20         )
    call para_get_value('SED_THETA_K_MIN_DON_DOXY'      , SED_THETA_K_MIN_DON_DOXY     )
    call para_get_value('SED_THETA_K_MIN_DON_NO3N'      , SED_THETA_K_MIN_DON_NO3N     )
    call para_get_value('SED_THETA_K_MIN_DON_MN_IV'     , SED_THETA_K_MIN_DON_MN_IV    )
    call para_get_value('SED_THETA_K_MIN_DON_FE_III'    , SED_THETA_K_MIN_DON_FE_III   )
    call para_get_value('SED_THETA_K_MIN_DON_S_PLUS_6'  , SED_THETA_K_MIN_DON_S_PLUS_6 )
    call para_get_value('SED_THETA_K_MIN_DON_DOC'       , SED_THETA_K_MIN_DON_DOC      )
    call para_get_value('SED_K_HS_DON_MIN_DOXY'         , SED_K_HS_DON_MIN_DOXY        )
    call para_get_value('SED_K_HS_DON_MIN_NO3N'         , SED_K_HS_DON_MIN_NO3N        )
    call para_get_value('SED_K_HS_DON_MIN_MN_IV'        , SED_K_HS_DON_MIN_MN_IV       )
    call para_get_value('SED_K_HS_DON_MIN_FE_III'       , SED_K_HS_DON_MIN_FE_III      )
    call para_get_value('SED_K_HS_DON_MIN_S_PLUS_6'     , SED_K_HS_DON_MIN_S_PLUS_6    )
    call para_get_value('SED_K_HS_DON_MIN_DOC'          , SED_K_HS_DON_MIN_DOC         )
    call para_get_value('SED_PH_MIN_DON_MIN_DOXY'       , SED_PH_MIN_DON_MIN_DOXY      )
    call para_get_value('SED_PH_MIN_DON_MIN_NO3N'       , SED_PH_MIN_DON_MIN_NO3N      )
    call para_get_value('SED_PH_MIN_DON_MIN_MN_IV'      , SED_PH_MIN_DON_MIN_MN_IV     )
    call para_get_value('SED_PH_MIN_DON_MIN_FE_III'     , SED_PH_MIN_DON_MIN_FE_III    )
    call para_get_value('SED_PH_MIN_DON_MIN_S_PLUS_6'   , SED_PH_MIN_DON_MIN_S_PLUS_6  )
    call para_get_value('SED_PH_MIN_DON_MIN_DOC'        , SED_PH_MIN_DON_MIN_DOC       )
    call para_get_value('SED_PH_MAX_DON_MIN_DOXY'       , SED_PH_MAX_DON_MIN_DOXY      )
    call para_get_value('SED_PH_MAX_DON_MIN_NO3N'       , SED_PH_MAX_DON_MIN_NO3N      )
    call para_get_value('SED_PH_MAX_DON_MIN_MN_IV'      , SED_PH_MAX_DON_MIN_MN_IV     )
    call para_get_value('SED_PH_MAX_DON_MIN_FE_III'     , SED_PH_MAX_DON_MIN_FE_III    )
    call para_get_value('SED_PH_MAX_DON_MIN_S_PLUS_6'   , SED_PH_MAX_DON_MIN_S_PLUS_6  )
    call para_get_value('SED_PH_MAX_DON_MIN_DOC'        , SED_PH_MAX_DON_MIN_DOC       )
    call para_get_value('SED_K_MIN_DOP_DOXY_20'         , SED_K_MIN_DOP_DOXY_20        )
    call para_get_value('SED_K_MIN_DOP_NO3N_20'         , SED_K_MIN_DOP_NO3N_20        )
    call para_get_value('SED_K_MIN_DOP_MN_IV_20'        , SED_K_MIN_DOP_MN_IV_20       )
    call para_get_value('SED_K_MIN_DOP_FE_III_20'       , SED_K_MIN_DOP_FE_III_20      )
    call para_get_value('SED_K_MIN_DOP_S_PLUS_6_20'     , SED_K_MIN_DOP_S_PLUS_6_20    )
    call para_get_value('SED_K_MIN_DOP_DOC_20'          , SED_K_MIN_DOP_DOC_20         )
    call para_get_value('SED_THETA_K_MIN_DOP_DOXY'      , SED_THETA_K_MIN_DOP_DOXY     )
    call para_get_value('SED_THETA_K_MIN_DOP_NO3N'      , SED_THETA_K_MIN_DOP_NO3N     )
    call para_get_value('SED_THETA_K_MIN_DOP_MN_IV'     , SED_THETA_K_MIN_DOP_MN_IV    )
    call para_get_value('SED_THETA_K_MIN_DOP_FE_III'    , SED_THETA_K_MIN_DOP_FE_III   )
    call para_get_value('SED_THETA_K_MIN_DOP_S_PLUS_6'  , SED_THETA_K_MIN_DOP_S_PLUS_6 )
    call para_get_value('SED_THETA_K_MIN_DOP_DOC'       , SED_THETA_K_MIN_DOP_DOC      )
    call para_get_value('SED_K_HS_DOP_MIN_DOXY'         , SED_K_HS_DOP_MIN_DOXY        )
    call para_get_value('SED_K_HS_DOP_MIN_NO3N'         , SED_K_HS_DOP_MIN_NO3N        )
    call para_get_value('SED_K_HS_DOP_MIN_MN_IV'        , SED_K_HS_DOP_MIN_MN_IV       )
    call para_get_value('SED_K_HS_DOP_MIN_FE_III'       , SED_K_HS_DOP_MIN_FE_III      )
    call para_get_value('SED_K_HS_DOP_MIN_S_PLUS_6'     , SED_K_HS_DOP_MIN_S_PLUS_6    )
    call para_get_value('SED_K_HS_DOP_MIN_DOC'          , SED_K_HS_DOP_MIN_DOC         )
    call para_get_value('SED_PH_MIN_DOP_MIN_DOXY'       , SED_PH_MIN_DOP_MIN_DOXY      )
    call para_get_value('SED_PH_MIN_DOP_MIN_NO3N'       , SED_PH_MIN_DOP_MIN_NO3N      )
    call para_get_value('SED_PH_MIN_DOP_MIN_MN_IV'      , SED_PH_MIN_DOP_MIN_MN_IV     )
    call para_get_value('SED_PH_MIN_DOP_MIN_FE_III'     , SED_PH_MIN_DOP_MIN_FE_III    )
    call para_get_value('SED_PH_MIN_DOP_MIN_S_PLUS_6'   , SED_PH_MIN_DOP_MIN_S_PLUS_6  )
    call para_get_value('SED_PH_MIN_DOP_MIN_DOC'        , SED_PH_MIN_DOP_MIN_DOC       )
    call para_get_value('SED_PH_MAX_DOP_MIN_DOXY'       , SED_PH_MAX_DOP_MIN_DOXY      )
    call para_get_value('SED_PH_MAX_DOP_MIN_NO3N'       , SED_PH_MAX_DOP_MIN_NO3N      )
    call para_get_value('SED_PH_MAX_DOP_MIN_MN_IV'      , SED_PH_MAX_DOP_MIN_MN_IV     )
    call para_get_value('SED_PH_MAX_DOP_MIN_FE_III'     , SED_PH_MAX_DOP_MIN_FE_III    )
    call para_get_value('SED_PH_MAX_DOP_MIN_S_PLUS_6'   , SED_PH_MAX_DOP_MIN_S_PLUS_6  )
    call para_get_value('SED_PH_MAX_DOP_MIN_DOC'        , SED_PH_MAX_DOP_MIN_DOC       )

    ! New model constats added in 29 th of January 2016
    call para_get_value('SED_k_OX_CH4'                  , SED_k_OX_CH4          )
    call para_get_value('SED_THETA_k_OX_CH4'            , SED_THETA_k_OX_CH4    )
    call para_get_value('SED_k_HS_OX_CH4_DOXY'          , SED_k_HS_OX_CH4_DOXY  )
    call para_get_value('SED_k_OX_H2S'                  , SED_k_OX_H2S          )
    call para_get_value('SED_THETA_k_OX_H2S'            , SED_THETA_k_OX_H2S    )
    call para_get_value('SED_k_HS_OX_H2S_DOXY'          , SED_k_HS_OX_H2S_DOXY  )
    ! 2016 08 21
    call para_get_value('SED_k_DISS_FE_II_20'           , SED_k_DISS_FE_II_20      )
    call para_get_value('SED_THETA_k_DISS_FE_II'        , SED_THETA_k_DISS_FE_II   )
    call para_get_value('SED_INIT_MULT_FE_II_DISS'      , SED_INIT_MULT_FE_II_DISS )
    call para_get_value('SED_k_DISS_FE_III_20'          , SED_k_DISS_FE_III_20     )
    call para_get_value('SED_THETA_k_DISS_FE_III'       , SED_THETA_k_DISS_FE_III  )
    call para_get_value('SED_INIT_MULT_FE_III_DISS'     , SED_INIT_MULT_FE_III_DISS)

end subroutine INIT_BSED_MODEL_CONSTANTS
