module BOTTOM_SEDIMENTS

    use GLOBAL
    use UTILS_1
    implicit none
contains

    ! Subroutine that initializes bottom sediment submodel

    subroutine INIT_BSED_MODEL_CONSTANTS
        use GLOBAL
        use AQUABC_BSED_MODEL_CONSTANTS
        implicit none

        ! AQUABC_II BS model constants
        ! Notes for SHYFEM compatibility

        K_OXIC_DISS_POC              = SED_MODEL_CONSTANTS(  1)
        K_ANOXIC_DISS_POC            = SED_MODEL_CONSTANTS(  2)
        THETA_DISS_POC               = SED_MODEL_CONSTANTS(  3)
        KHS_DISS_POC                 = SED_MODEL_CONSTANTS(  4)
        K_OXIC_DISS_PON              = SED_MODEL_CONSTANTS(  5)
        K_ANOXIC_DISS_PON            = SED_MODEL_CONSTANTS(  6)
        THETA_DISS_PON               = SED_MODEL_CONSTANTS(  7)
        KHS_DISS_PON                 = SED_MODEL_CONSTANTS(  8)
        K_OXIC_DISS_POP              = SED_MODEL_CONSTANTS(  9)
        K_ANOXIC_DISS_POP            = SED_MODEL_CONSTANTS( 10)
        THETA_DISS_POP               = SED_MODEL_CONSTANTS( 11)
        KHS_DISS_POP                 = SED_MODEL_CONSTANTS( 12)
        K_OXIC_DISS_PSi              = SED_MODEL_CONSTANTS( 13)
        K_ANOXIC_DISS_PSi            = SED_MODEL_CONSTANTS( 14)
        THETA_DISS_PSi               = SED_MODEL_CONSTANTS( 15)
        KHS_DISS_PSi                 = SED_MODEL_CONSTANTS( 16)
        K_OXIC_MINER_DOC             = SED_MODEL_CONSTANTS( 17)
        K_ANOXIC_MINER_DOC           = SED_MODEL_CONSTANTS( 18)
        THETA_MINER_DOC              = SED_MODEL_CONSTANTS( 19)
        KHS_MINER_DOC                = SED_MODEL_CONSTANTS( 20)
        K_OXIC_MINER_DON             = SED_MODEL_CONSTANTS( 21)
        K_ANOXIC_MINER_DON           = SED_MODEL_CONSTANTS( 22)
        THETA_MINER_DON              = SED_MODEL_CONSTANTS( 23)
        KHS_MINER_DON                = SED_MODEL_CONSTANTS( 24)
        K_OXIC_MINER_DOP             = SED_MODEL_CONSTANTS( 25)
        K_ANOXIC_MINER_DOP           = SED_MODEL_CONSTANTS( 26)
        THETA_MINER_DOP              = SED_MODEL_CONSTANTS( 27)
        KHS_MINER_DOP                = SED_MODEL_CONSTANTS( 28)
        O_TO_C                       = SED_MODEL_CONSTANTS( 29)
        K_NITR                       = SED_MODEL_CONSTANTS( 30)
        THETA_NITR                   = SED_MODEL_CONSTANTS( 31)
        KHS_NITR_NH4N                = SED_MODEL_CONSTANTS( 32)
        KHS_NITR_DOXY                = SED_MODEL_CONSTANTS( 33)
        K_DENITR                     = SED_MODEL_CONSTANTS( 34)
        THETA_DENITR                 = SED_MODEL_CONSTANTS( 35)
        KHS_DENITR_NO3N              = SED_MODEL_CONSTANTS( 36)
        KHS_DENITR_DOC               = SED_MODEL_CONSTANTS( 37)
        KHS_DENITR_DOXY              = SED_MODEL_CONSTANTS( 38)
        DENITR_YIELD                 = SED_MODEL_CONSTANTS( 39)
        DOXY_AT_ANOXIA               = SED_MODEL_CONSTANTS( 40)
        SOLID_PART_COEFF_NH4         = SED_MODEL_CONSTANTS( 41)
        SOLID_PART_COEFF_PO4         = SED_MODEL_CONSTANTS( 42)
        SED_PH_MIN_DOC_MIN           = SED_MODEL_CONSTANTS( 43)
        SED_PH_MIN_DOC_MAX           = SED_MODEL_CONSTANTS( 44)
        SED_PH_MIN_DON_MIN           = SED_MODEL_CONSTANTS( 45)
        SED_PH_MIN_DON_MAX           = SED_MODEL_CONSTANTS( 46)
        SED_PH_MIN_DOP_MIN           = SED_MODEL_CONSTANTS( 47)
        SED_PH_MIN_DOP_MAX           = SED_MODEL_CONSTANTS( 48)
        SED_PH_NITR_NH4_MIN          = SED_MODEL_CONSTANTS( 49)
        SED_PH_NITR_NH4_MAX          = SED_MODEL_CONSTANTS( 50)
        SED_PH_DENITR_NO3_MIN        = SED_MODEL_CONSTANTS( 51)
        SED_PH_DENITR_NO3_MAX        = SED_MODEL_CONSTANTS( 52)
        SED_k_OX_FE_II               = SED_MODEL_CONSTANTS( 53)
        SED_k_RED_FE_III             = SED_MODEL_CONSTANTS( 54)
        SED_k_OX_MN_II               = SED_MODEL_CONSTANTS( 55)
        SED_k_RED_MN_IV              = SED_MODEL_CONSTANTS( 56)
        SED_KHS_DOXY_FE_III_RED      = SED_MODEL_CONSTANTS( 57)
        SED_KHS_DOXY_MN_IV_RED       = SED_MODEL_CONSTANTS( 58)
        SED_K_MIN_DOC_DOXY_20        = SED_MODEL_CONSTANTS( 59)
        SED_K_MIN_DOC_NO3N_20        = SED_MODEL_CONSTANTS( 60)
        SED_K_MIN_DOC_MN_IV_20       = SED_MODEL_CONSTANTS( 61)
        SED_K_MIN_DOC_FE_III_20      = SED_MODEL_CONSTANTS( 62)
        SED_K_MIN_DOC_S_PLUS_6_20    = SED_MODEL_CONSTANTS( 63)
        SED_K_MIN_DOC_DOC_20         = SED_MODEL_CONSTANTS( 64)
        SED_THETA_K_MIN_DOC_DOXY     = SED_MODEL_CONSTANTS( 65)
        SED_THETA_K_MIN_DOC_NO3N     = SED_MODEL_CONSTANTS( 66)
        SED_THETA_K_MIN_DOC_MN_IV    = SED_MODEL_CONSTANTS( 67)
        SED_THETA_K_MIN_DOC_FE_III   = SED_MODEL_CONSTANTS( 68)
        SED_THETA_K_MIN_DOC_S_PLUS_6 = SED_MODEL_CONSTANTS( 69)
        SED_THETA_K_MIN_DOC_DOC      = SED_MODEL_CONSTANTS( 70)
        SED_K_HS_DOC_MIN_DOXY        = SED_MODEL_CONSTANTS( 71)
        SED_K_HS_DOC_MIN_NO3N        = SED_MODEL_CONSTANTS( 72)
        SED_K_HS_DOC_MIN_MN_IV       = SED_MODEL_CONSTANTS( 73)
        SED_K_HS_DOC_MIN_FE_III      = SED_MODEL_CONSTANTS( 74)
        SED_K_HS_DOC_MIN_S_PLUS_6    = SED_MODEL_CONSTANTS( 75)
        SED_K_HS_DOC_MIN_DOC         = SED_MODEL_CONSTANTS( 76)
        SED_K_HS_DOXY_RED_LIM        = SED_MODEL_CONSTANTS( 77)
        SED_K_HS_NO3N_RED_LIM        = SED_MODEL_CONSTANTS( 78)
        SED_K_HS_MN_IV_RED_LIM       = SED_MODEL_CONSTANTS( 79)
        SED_K_HS_FE_III_RED_LIM      = SED_MODEL_CONSTANTS( 80)
        SED_K_HS_S_PLUS_6_RED_LIM    = SED_MODEL_CONSTANTS( 81)
        SED_K_HS_DOXY_RED_INHB       = SED_MODEL_CONSTANTS( 82)
        SED_K_HS_NO3N_RED_INHB       = SED_MODEL_CONSTANTS( 83)
        SED_K_HS_MN_IV_RED_INHB      = SED_MODEL_CONSTANTS( 84)
        SED_K_HS_FE_III_RED_INHB     = SED_MODEL_CONSTANTS( 85)
        SED_K_HS_S_PLUS_6_RED_INHB   = SED_MODEL_CONSTANTS( 86)
        SED_PH_MIN_DOC_MIN_DOXY      = SED_MODEL_CONSTANTS( 87)
        SED_PH_MIN_DOC_MIN_NO3N      = SED_MODEL_CONSTANTS( 88)
        SED_PH_MIN_DOC_MIN_MN_IV     = SED_MODEL_CONSTANTS( 89)
        SED_PH_MIN_DOC_MIN_FE_III    = SED_MODEL_CONSTANTS( 90)
        SED_PH_MIN_DOC_MIN_S_PLUS_6  = SED_MODEL_CONSTANTS( 91)
        SED_PH_MIN_DOC_MIN_DOC       = SED_MODEL_CONSTANTS( 92)
        SED_PH_MAX_DOC_MIN_DOXY      = SED_MODEL_CONSTANTS( 93)
        SED_PH_MAX_DOC_MIN_NO3N      = SED_MODEL_CONSTANTS( 94)
        SED_PH_MAX_DOC_MIN_MN_IV     = SED_MODEL_CONSTANTS( 95)
        SED_PH_MAX_DOC_MIN_FE_III    = SED_MODEL_CONSTANTS( 96)
        SED_PH_MAX_DOC_MIN_S_PLUS_6  = SED_MODEL_CONSTANTS( 97)
        SED_PH_MAX_DOC_MIN_DOC       = SED_MODEL_CONSTANTS( 98)
        SED_K_MIN_DON_DOXY_20        = SED_MODEL_CONSTANTS( 99)
        SED_K_MIN_DON_NO3N_20        = SED_MODEL_CONSTANTS(100)
        SED_K_MIN_DON_MN_IV_20       = SED_MODEL_CONSTANTS(101)
        SED_K_MIN_DON_FE_III_20      = SED_MODEL_CONSTANTS(102)
        SED_K_MIN_DON_S_PLUS_6_20    = SED_MODEL_CONSTANTS(103)
        SED_K_MIN_DON_DOC_20         = SED_MODEL_CONSTANTS(104)
        SED_THETA_K_MIN_DON_DOXY     = SED_MODEL_CONSTANTS(105)
        SED_THETA_K_MIN_DON_NO3N     = SED_MODEL_CONSTANTS(106)
        SED_THETA_K_MIN_DON_MN_IV    = SED_MODEL_CONSTANTS(107)
        SED_THETA_K_MIN_DON_FE_III   = SED_MODEL_CONSTANTS(108)
        SED_THETA_K_MIN_DON_S_PLUS_6 = SED_MODEL_CONSTANTS(109)
        SED_THETA_K_MIN_DON_DOC      = SED_MODEL_CONSTANTS(110)
        SED_K_HS_DON_MIN_DOXY        = SED_MODEL_CONSTANTS(111)
        SED_K_HS_DON_MIN_NO3N        = SED_MODEL_CONSTANTS(112)
        SED_K_HS_DON_MIN_MN_IV       = SED_MODEL_CONSTANTS(113)
        SED_K_HS_DON_MIN_FE_III      = SED_MODEL_CONSTANTS(114)
        SED_K_HS_DON_MIN_S_PLUS_6    = SED_MODEL_CONSTANTS(115)
        SED_K_HS_DON_MIN_DOC         = SED_MODEL_CONSTANTS(116)
        SED_PH_MIN_DON_MIN_DOXY      = SED_MODEL_CONSTANTS(117)
        SED_PH_MIN_DON_MIN_NO3N      = SED_MODEL_CONSTANTS(118)
        SED_PH_MIN_DON_MIN_MN_IV     = SED_MODEL_CONSTANTS(119)
        SED_PH_MIN_DON_MIN_FE_III    = SED_MODEL_CONSTANTS(120)
        SED_PH_MIN_DON_MIN_S_PLUS_6  = SED_MODEL_CONSTANTS(121)
        SED_PH_MIN_DON_MIN_DOC       = SED_MODEL_CONSTANTS(122)
        SED_PH_MAX_DON_MIN_DOXY      = SED_MODEL_CONSTANTS(123)
        SED_PH_MAX_DON_MIN_NO3N      = SED_MODEL_CONSTANTS(124)
        SED_PH_MAX_DON_MIN_MN_IV     = SED_MODEL_CONSTANTS(125)
        SED_PH_MAX_DON_MIN_FE_III    = SED_MODEL_CONSTANTS(126)
        SED_PH_MAX_DON_MIN_S_PLUS_6  = SED_MODEL_CONSTANTS(127)
        SED_PH_MAX_DON_MIN_DOC       = SED_MODEL_CONSTANTS(128)
        SED_K_MIN_DOP_DOXY_20        = SED_MODEL_CONSTANTS(129)
        SED_K_MIN_DOP_NO3N_20        = SED_MODEL_CONSTANTS(130)
        SED_K_MIN_DOP_MN_IV_20       = SED_MODEL_CONSTANTS(131)
        SED_K_MIN_DOP_FE_III_20      = SED_MODEL_CONSTANTS(132)
        SED_K_MIN_DOP_S_PLUS_6_20    = SED_MODEL_CONSTANTS(133)
        SED_K_MIN_DOP_DOC_20         = SED_MODEL_CONSTANTS(134)
        SED_THETA_K_MIN_DOP_DOXY     = SED_MODEL_CONSTANTS(135)
        SED_THETA_K_MIN_DOP_NO3N     = SED_MODEL_CONSTANTS(136)
        SED_THETA_K_MIN_DOP_MN_IV    = SED_MODEL_CONSTANTS(137)
        SED_THETA_K_MIN_DOP_FE_III   = SED_MODEL_CONSTANTS(138)
        SED_THETA_K_MIN_DOP_S_PLUS_6 = SED_MODEL_CONSTANTS(139)
        SED_THETA_K_MIN_DOP_DOC      = SED_MODEL_CONSTANTS(140)
        SED_K_HS_DOP_MIN_DOXY        = SED_MODEL_CONSTANTS(141)
        SED_K_HS_DOP_MIN_NO3N        = SED_MODEL_CONSTANTS(142)
        SED_K_HS_DOP_MIN_MN_IV       = SED_MODEL_CONSTANTS(143)
        SED_K_HS_DOP_MIN_FE_III      = SED_MODEL_CONSTANTS(144)
        SED_K_HS_DOP_MIN_S_PLUS_6    = SED_MODEL_CONSTANTS(145)
        SED_K_HS_DOP_MIN_DOC         = SED_MODEL_CONSTANTS(146)
        SED_PH_MIN_DOP_MIN_DOXY      = SED_MODEL_CONSTANTS(147)
        SED_PH_MIN_DOP_MIN_NO3N      = SED_MODEL_CONSTANTS(148)
        SED_PH_MIN_DOP_MIN_MN_IV     = SED_MODEL_CONSTANTS(149)
        SED_PH_MIN_DOP_MIN_FE_III    = SED_MODEL_CONSTANTS(150)
        SED_PH_MIN_DOP_MIN_S_PLUS_6  = SED_MODEL_CONSTANTS(151)
        SED_PH_MIN_DOP_MIN_DOC       = SED_MODEL_CONSTANTS(152)
        SED_PH_MAX_DOP_MIN_DOXY      = SED_MODEL_CONSTANTS(153)
        SED_PH_MAX_DOP_MIN_NO3N      = SED_MODEL_CONSTANTS(154)
        SED_PH_MAX_DOP_MIN_MN_IV     = SED_MODEL_CONSTANTS(155)
        SED_PH_MAX_DOP_MIN_FE_III    = SED_MODEL_CONSTANTS(156)
        SED_PH_MAX_DOP_MIN_S_PLUS_6  = SED_MODEL_CONSTANTS(157)
        SED_PH_MAX_DOP_MIN_DOC       = SED_MODEL_CONSTANTS(158)
        SED_k_OX_CH4                 = SED_MODEL_CONSTANTS(159)
        SED_THETA_k_OX_CH4           = SED_MODEL_CONSTANTS(160)
        SED_k_HS_OX_CH4_DOXY         = SED_MODEL_CONSTANTS(161)
        SED_k_OX_H2S                 = SED_MODEL_CONSTANTS(162)
        SED_THETA_k_OX_H2S           = SED_MODEL_CONSTANTS(163)
        SED_k_HS_OX_H2S_DOXY         = SED_MODEL_CONSTANTS(164)
        SED_k_DISS_FE_II_20          = SED_MODEL_CONSTANTS(165)
        SED_THETA_k_DISS_FE_II       = SED_MODEL_CONSTANTS(166)
        SED_INIT_MULT_FE_II_DISS     = SED_MODEL_CONSTANTS(167)
        SED_k_DISS_FE_III_20         = SED_MODEL_CONSTANTS(168)
        SED_THETA_k_DISS_FE_III      = SED_MODEL_CONSTANTS(169)
        SED_INIT_MULT_FE_III_DISS    = SED_MODEL_CONSTANTS(170)

    end subroutine INIT_BSED_MODEL_CONSTANTS

    subroutine SED_MOD_1_CVISC()
        implicit none
        ! Dummy; ensure implicit none present
    end subroutine SED_MOD_1_CVISC

        

    !subroutine UPDATE_BOTTOM_SEDIMENT_INPUTS &
    !           (TIME              , &
    !            SED_DEPTHS        , SED_POROSITIES, SED_DENSITIES, PART_MIXING_COEFFS, &
    !            ADVECTIVE_VELOCITY, SED_DIFFUSIONS, SURF_MIXLEN  , SED_BURRIALS      , &
    !            SED_TEMPS         , SED_SALTS     , SED_FLAGS    , NUM_SED_FLAGS     , &
    !            nkn               , NUM_SED_LAYERS, NUM_SED_VARS , MODEL_BENTHIC_ANIMALS)

    !    use AQUABC_II_GLOBAL
    !    use TIME_SERIES

    !    implicit none

    !    real(kind=DBL_PREC)                                              :: TIME               !Time
    !    real(kind=DBL_PREC), dimension(nkn,NUM_SED_LAYERS)               :: SED_DEPTHS         !Sediment depths
    !    real(kind=DBL_PREC), dimension(nkn,NUM_SED_LAYERS)               :: SED_POROSITIES     !Sediment porosities
    !    real(kind=DBL_PREC), dimension(nkn,NUM_SED_LAYERS)               :: SED_DENSITIES      !Bulk wet density
    !    real(kind=DBL_PREC), dimension(nkn,NUM_SED_LAYERS, NUM_SED_VARS) :: PART_MIXING_COEFFS !Particle mixing coeff
    !    real(kind=DBL_PREC)                                              :: ADVECTIVE_VELOCITY !Advective  velocity
    !    real(kind=DBL_PREC), dimension(nkn,NUM_SED_LAYERS, NUM_SED_VARS) :: SED_DIFFUSIONS     !Sediment diffusions
    !    real(kind=DBL_PREC)                                              :: SURF_MIXLEN        !Mixing length with surface water
    !    real(kind=DBL_PREC), dimension(nkn,NUM_SED_LAYERS)               :: SED_BURRIALS       !Sediment burrial rate
    !    real(kind=DBL_PREC), dimension(nkn,NUM_SED_LAYERS)               :: SED_TEMPS          !Sediment temperatures
    !    real(kind=DBL_PREC), dimension(nkn,NUM_SED_LAYERS)               :: SED_SALTS          !Sediment salinites
    !    integer            , dimension(NUM_SED_FLAGS)                    :: SED_FLAGS

    !    integer :: nkn                   !Number of boxes
    !    integer :: NUM_SED_LAYERS        !Number of bottom sediment layers
    !    integer :: NUM_SED_VARS          !Number of botton sediment state variables
    !    integer :: MODEL_BENTHIC_ANIMALS !Flag for modeling benthic animals
    !    integer :: NUM_SED_FLAGS

    !    integer :: BOX_NO, LAYER_NO, SED_STATE_VAR_NO

        !Declaration for function calculating modecular diffusion coefficients
    !    real(kind=DBL_PREC) :: SED_MOD_1_ALUKAS_MOLDI_C

    !    real(kind=DBL_PREC), dimension(nkn) :: TABLE_FUNCTION_VALUES

    !    if (TIME_STEP_NO == 1) then
    !        SED_FLAGS(1) = 1
    !    else
    !        SED_FLAGS(1) = 0
    !    end if

    !    SED_FLAGS(2) = 2
    !    SED_FLAGS(3) = 2

    !    call SED_MOD_1_ALUKAS_MOLDI_C_VEC &
    !         (SED_TEMPS, SED_SALTS, nkn, NUM_SED_LAYERS, NUM_SED_VARS, SED_DIFFUSIONS)

    !    SED_DIFFUSIONS = (1.0D0/6.0D0) * 86400.0D0 * SED_DIFFUSIONS
    !end subroutine UPDATE_BOTTOM_SEDIMENT_INPUTS




    !subroutine SEDIMENT_TRANSPORT &
    !           (SETTLING_VELOCITIES, DISSOLVED_FRACTIONS, FRACTION_OF_DEPOSITION, &
    !            SAVED_OUTPUTS      , nkn, nstate, n_saved_outputs, TIME)

    !    use AQUABC_II_GLOBAL
    !    implicit none

    !    real(kind = DBL_PREC), dimension(nkn, nstate)          :: SETTLING_VELOCITIES
    !    real(kind = DBL_PREC), dimension(nkn, nstate)          :: DISSOLVED_FRACTIONS
    !    real(kind = DBL_PREC), dimension(nkn, nstate)          :: FRACTION_OF_DEPOSITION
    !    real(kind = DBL_PREC), dimension(nkn, n_saved_outputs) :: SAVED_OUTPUTS

    !    integer :: nkn
    !    integer :: nstAte
    !    integer :: n_saved_outputs
    !    real(kind = DBL_PREC) :: TIME

        ! Calculate settling velocities. This simple example assumes that the settling
        ! velocities do not change in time and space units: m/day
    !    SETTLING_VELOCITIES = SETTLING_VELOCITIES_OUTPUT

        ! Calcululate dissolved fractions
    !    DISSOLVED_FRACTIONS = EFFECTIVE_DISSLOVED_FRACTIONS

        ! Calculate not deposited fluxes
    !    FRACTION_OF_DEPOSITION = EFFECTIVE_DEPOSITION_FRACTIONS
    !end subroutine SEDIMENT_TRANSPORT

    subroutine UPDATE_BOTTOM_SEDIMENT_INPUTS &
               (TIME                  , &
                SED_DEPTHS_LOC        , SED_POROSITIES_LOC, SED_DENSITIES_LOC     , &
                PART_MIXING_COEFFS_LOC, ADVECTIVE_VELOCITY_LOC, SED_DIFFUSIONS_LOC, &
                SURF_MIXLEN_LOC       , SED_BURRIALS_LOC      , SED_TEMPS_LOC     , &
                SED_SALTS             , SED_FLAGS_LOC         , NUM_SED_FLAGS_LOC , &
                nkn_loc               , NUM_SED_LAYERS_LOC    , NUM_SED_VARS_LOC  , &
                MODEL_BENTHIC_ANIMALS_LOC)

        use AQUABC_II_GLOBAL
        use TIME_SERIES

        implicit none

        !Time
        real(kind=DBL_PREC)                                                          :: &
            TIME

        !Sediment layer depths
        real(kind=DBL_PREC), dimension(nkn_loc,NUM_SED_LAYERS_LOC)                   :: &
            SED_DEPTHS_LOC

        !Sediment porosities
        real(kind=DBL_PREC), dimension(nkn_loc,NUM_SED_LAYERS_LOC)                   :: &
            SED_POROSITIES_LOC

        !Bulk wet density
        real(kind=DBL_PREC), dimension(nkn_loc,NUM_SED_LAYERS_LOC)                   :: &
            SED_DENSITIES_LOC

        !Particle mixing coeff
        real(kind=DBL_PREC), dimension(nkn_loc,NUM_SED_LAYERS_LOC, NUM_SED_VARS_LOC) :: &
            PART_MIXING_COEFFS_LOC

        !Advective  velocity
        real(kind=DBL_PREC)                                                          :: &
            ADVECTIVE_VELOCITY_LOC

        !Sediment diffusions
        real(kind=DBL_PREC), dimension(nkn_loc,NUM_SED_LAYERS_LOC, NUM_SED_VARS_LOC) :: &
            SED_DIFFUSIONS_LOC

        !Mixing length with surface water
        real(kind=DBL_PREC)                                                          :: &
            SURF_MIXLEN_LOC

        !Sediment burrial rate
        real(kind=DBL_PREC), dimension(nkn_loc,NUM_SED_LAYERS_LOC)                   :: &
            SED_BURRIALS_LOC

        !Sediment temperatures
        real(kind=DBL_PREC), dimension(nkn_loc,NUM_SED_LAYERS_LOC)                   :: &
            SED_TEMPS_LOC

        !Sediment salinites
        real(kind=DBL_PREC), dimension(nkn_loc,NUM_SED_LAYERS_LOC)                   :: &
            SED_SALTS

        integer            , dimension(NUM_SED_FLAGS_LOC)                            :: &
            SED_FLAGS_LOC

        integer :: nkn_loc                   !Number of boxes
        integer :: NUM_SED_LAYERS_LOC        !Number of bottom sediment layers
        integer :: NUM_SED_VARS_LOC          !Number of botton sediment state variables
        integer :: MODEL_BENTHIC_ANIMALS_LOC !Flag for modeling benthic animals
        integer :: NUM_SED_FLAGS_LOC

        integer :: BOX_NO, LAYER_NO, SED_STATE_VAR_NO

        !Declaration for function calculating modecular diffusion coefficients
        real(kind=DBL_PREC) :: SED_MOD_1_ALUKAS_MOLDI_C

        real(kind=DBL_PREC), dimension(nkn_loc) :: TABLE_FUNCTION_VALUES

        if (TIME_STEP_NO == 1) then
            SED_FLAGS_LOC(1) = 1
        else
            SED_FLAGS_LOC(1) = 0
        end if

        SED_FLAGS_LOC(2) = 2
        SED_FLAGS_LOC(3) = 2

        call SED_MOD_1_ALUKAS_MOLDI_C_VEC &
             (SED_TEMPS_LOC   , SED_SALTS, nkn_loc, NUM_SED_LAYERS_LOC,  &
              NUM_SED_VARS_LOC, SED_DIFFUSIONS_LOC)

        SED_DIFFUSIONS_LOC = (1.0D0/6.0D0) * 86400.0D0 * SED_DIFFUSIONS_LOC
    end subroutine UPDATE_BOTTOM_SEDIMENT_INPUTS


    subroutine SEDIMENT_TRANSPORT &
               (SETTLING_VELOCITIES, DISSOLVED_FRACTIONS, FRACTION_OF_DEPOSITION, &
                SAVED_OUTPUTS      , nkn_loc, nstate, n_saved_outputs, TIME)

        use AQUABC_II_GLOBAL
        implicit none

        real(kind = DBL_PREC), dimension(nkn_loc, nstate)          :: SETTLING_VELOCITIES
        real(kind = DBL_PREC), dimension(nkn_loc, nstate)          :: DISSOLVED_FRACTIONS
        real(kind = DBL_PREC), dimension(nkn_loc, nstate)          :: FRACTION_OF_DEPOSITION
        real(kind = DBL_PREC), dimension(nkn_loc, n_saved_outputs) :: SAVED_OUTPUTS

        integer :: nkn_loc
        integer :: nstAte
        integer :: n_saved_outputs
        real(kind = DBL_PREC) :: TIME

        ! Calculate settling velocities. This simple example assumes that the settling
        ! velocities do not change in time and space units: m/day
        SETTLING_VELOCITIES = SETTLING_VELOCITIES_OUTPUT

        ! Calcululate dissolved fractions
        DISSOLVED_FRACTIONS = EFFECTIVE_DISSLOVED_FRACTIONS

        ! Calculate not deposited fluxes
        FRACTION_OF_DEPOSITION = EFFECTIVE_DEPOSITION_FRACTIONS
    end subroutine SEDIMENT_TRANSPORT



    subroutine READ_BOTTOM_SEDIMENTS_MODEL_INPUTS(IN_FILE)
        implicit none
        integer, intent(in) :: IN_FILE

        integer :: i, j, LAYER_NO
        integer :: BOX_NO, SED_LAYER_NO, SED_VAR_NO

        real(kind = DBL) :: AUX_DOUBLE
        real(kind = DBL), allocatable, dimension(:, :) :: BSED_ARRAY
        character(len = 2048) :: FILE_NAME

        !READ DESCRIPTION LINES
        do i = 1, 5
            read(IN_FILE, *)
        end do

        !Read the advanced redox simulation option
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) BOTTOM_SED_ADVANCED_REDOX_SIMULATION

        read(IN_FILE, *)
        read(IN_FILE, *) NUM_SED_LAYERS

        write(*,*) NUM_SED_VARS, NUM_SED_LAYERS
        allocate(BSED_ARRAY(NUM_SED_VARS, NUM_SED_LAYERS))

        ! -----------------------------------------------------------------------------------
        ! Allocate AQUABC sediments arrays
        ! -----------------------------------------------------------------------------------
        allocate(INIT_SED_STATE_VARS     (nkn,NUM_SED_LAYERS, NUM_SED_VARS))
        allocate(SED_DEPTHS              (nkn,NUM_SED_LAYERS)              )
        allocate(SED_POROSITIES          (nkn,NUM_SED_LAYERS)              )
        allocate(SED_DENSITIES           (nkn,NUM_SED_LAYERS)              )
        allocate(PART_MIXING_COEFFS      (nkn,NUM_SED_LAYERS, NUM_SED_VARS))
        allocate(SED_DIFFUSIONS          (nkn,NUM_SED_LAYERS, NUM_SED_VARS))

        allocate(SED_BURRIALS            (nkn,NUM_SED_LAYERS)                            )
        allocate(SURF_WATER_CONCS        (nkn,NUM_SED_VARS)                              )
        allocate(SED_TEMPS               (nkn,NUM_SED_LAYERS)                            )
        allocate(SED_MODEL_CONSTANTS     (NUM_SED_CONSTS)                                )
        allocate(PROCESSES_sed           (nkn,NUM_SED_LAYERS, NUM_SED_VARS, NDIAGVAR_sed))
        allocate(SED_DRIVING_FUNCTIONS   (NUM_SED_LAYERS, NUM_SED_DRIV)                  )
        allocate(FLUXES_TO_SEDIMENTS     (nkn,NUM_FLUXES_TO_SEDIMENTS)                   )
        allocate(SED_BURRIAL_RATE_OUTPUTS(nkn,NUM_SED_LAYERS, NUM_SED_VARS))

        allocate(H_ERODEP                (nkn)          )
        allocate(SED_FLAGS               (NUM_SED_FLAGS))

        allocate(FINAL_SED_STATE_VARS    (nkn,NUM_SED_LAYERS, NUM_SED_VARS         ))
        allocate(FLUXES_FROM_SEDIMENTS   (nkn,NUM_FLUXES_FROM_SEDIMENTS            ))
        allocate(SED_OUTPUTS             (nkn,NUM_SED_LAYERS, NUM_SED_OUTPUTS      ))
        allocate(SED_SAVED_OUTPUTS       (nkn,NUM_SED_LAYERS, NUM_SED_SAVED_OUTPUTS))
        ! -----------------------------------------------------------------------------------
        ! End of allocate AQUABC sediments arrays
        ! -----------------------------------------------------------------------------------

        ! Read sediment depths
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)

        do SED_LAYER_NO = 1, NUM_SED_LAYERS
            read(IN_FILE, *) AUX_DOUBLE
            SED_DEPTHS(:, SED_LAYER_NO) = AUX_DOUBLE
        end do

        ! Read sediment porosities
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)

        do SED_LAYER_NO = 1, NUM_SED_LAYERS
            read(IN_FILE, *) AUX_DOUBLE
            SED_POROSITIES(:, SED_LAYER_NO) = AUX_DOUBLE
        end do

        ! Read sediment densities
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)

        do SED_LAYER_NO = 1, NUM_SED_LAYERS
            read(IN_FILE, *) AUX_DOUBLE
            SED_DENSITIES(:, SED_LAYER_NO) = AUX_DOUBLE
        end do

        ! Read advective velocities
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *) ADVECTIVE_VELOCITY

        ! Read particle mixing coefficients
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *) AUX_DOUBLE
        PART_MIXING_COEFFS(:,:,:) = AUX_DOUBLE  !m2/day

        ! Read sediment burials
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *) AUX_DOUBLE
        SED_BURRIALS      (:,:)   = AUX_DOUBLE  !m/day

        ! Read sediment mixing length with surface water
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *) SURF_MIXLEN            !m

        ! Read the initial concentrations for bottom sediments
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)

        do SED_VAR_NO = 1, NUM_SED_VARS
            read(IN_FILE, *) BSED_ARRAY(SED_VAR_NO, :)
        end do

        do BOX_NO = 1,nkn
            do LAYER_NO = 1, NUM_SED_LAYERS
                do SED_VAR_NO = 1, NUM_SED_VARS
                    INIT_SED_STATE_VARS(BOX_NO,LAYER_NO,SED_VAR_NO) = &
                        BSED_ARRAY(SED_VAR_NO, LAYER_NO)
                end do
            end do
        end do

        ! Read the bottom sediment model constants
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *)
        read(IN_FILE, *) FILE_NAME

        open(unit   = IN_FILE + 1, &
             file   = trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
             status = 'OLD')

        call READ_MODEL_CONSTANTS(SED_MODEL_CONSTANTS, IN_FILE + 1)
        close(IN_FILE + 1)
        !call INIT_BS_MODEL_CONSTANTS

        deallocate(BSED_ARRAY)

        ! Read the information related to bottom sediment model outputs
        read(IN_FILE, *)

        read(IN_FILE, *)
        read(IN_FILE, *) BOTTOM_SEDIMENT_CONCENTRATIONS_FILENAME

        read(IN_FILE, *)
        read(IN_FILE, *) BOTTOM_SEDIMENT_FLUXES_FILENAME

        if (PRODUCE_COCOA_OUTPUTS > 0) then
            read(IN_FILE, *)
            read(IN_FILE, *) COCOA_SEDIMENT_PROCESS_RATES_FILENAME

            read(IN_FILE, *)
            read(IN_FILE, *) COCOA_SEDIMENT_BURIAL_RATES_FILENAME

            read(IN_FILE, *)
            read(IN_FILE, *) COCOA_FLUXES_FROM_SEDIMENTS_FILENAME

            read(IN_FILE, *)
            read(IN_FILE, *) COCOA_FLUXES_TO_SEDIMENTS_FILENAME
        end if
    end subroutine READ_BOTTOM_SEDIMENTS_MODEL_INPUTS

end module BOTTOM_SEDIMENTS
