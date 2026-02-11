! Contains
!     module AQUABC_PELAGIC_INTERNAL
!     subroutine ALLOC_AQUABC_PELAGIC_INTERNAL
!     subroutine DEALLOC_AQUABC_PELAGIC_INTERNAL

module AQUABC_PELAGIC_INTERNAL
    use AQUABC_II_GLOBAL
    implicit none

    !ALL INTERNAL VARIABLE DEFINITIONS

    !State variables
    real(kind = DBL_PREC), allocatable, dimension(:) :: NH4_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: NO3_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: PO4_P
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_OXYGEN
    real(kind = DBL_PREC), allocatable, dimension(:) :: DIA_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: ZOO_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: ZOO_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: ZOO_P
    real(kind = DBL_PREC), allocatable, dimension(:) :: DET_PART_ORG_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: DET_PART_ORG_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: DET_PART_ORG_P
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_ORG_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_ORG_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_ORG_P
    real(kind = DBL_PREC), allocatable, dimension(:) :: CYN_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: OPA_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_Si
    real(kind = DBL_PREC), allocatable, dimension(:) :: PART_Si
    real(kind = DBL_PREC), allocatable, dimension(:) :: FIX_CYN_C
    ! New state variables added 22 September 2014
    real(kind = DBL_PREC), allocatable, dimension(:) :: INORG_C   !Inorganic carbon
    real(kind = DBL_PREC), allocatable, dimension(:) :: TOT_ALK   !Total alkalinity
    ! End of new state variables added 22 September 2014

    ! New state variables added 9 September 2015
    real(kind = DBL_PREC), allocatable, dimension(:) :: FE_II
    real(kind = DBL_PREC), allocatable, dimension(:) :: FE_III
    real(kind = DBL_PREC), allocatable, dimension(:) :: MN_II
    real(kind = DBL_PREC), allocatable, dimension(:) :: MN_IV
    ! End of new state variables added 22 September 2015

    ! New state variables added 27 January 2016
    real(kind = DBL_PREC), allocatable, dimension(:) :: CA
    real(kind = DBL_PREC), allocatable, dimension(:) :: MG
    real(kind = DBL_PREC), allocatable, dimension(:) :: S_PLUS_6
    real(kind = DBL_PREC), allocatable, dimension(:) :: S_MINUS_2
    real(kind = DBL_PREC), allocatable, dimension(:) :: CH4_C
    ! End of new state variables added 27 January 2016

    ! Vegatative + heterocyst stage carbon
    real(kind = DBL_PREC), allocatable, dimension(:) :: NOST_VEG_HET_C

    ! Akinets carbon
    real(kind = DBL_PREC), allocatable, dimension(:) :: NOST_AKI_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: PHYT_TOT_C

    !Driving functions
    real(kind = DBL_PREC), allocatable, dimension(:) :: TEMP
    real(kind = DBL_PREC), allocatable, dimension(:) :: FDAY
    real(kind = DBL_PREC), allocatable, dimension(:) :: DEPTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: I_A
    real(kind = DBL_PREC), allocatable, dimension(:) :: SALT
    real(kind = DBL_PREC), allocatable, dimension(:) :: ELEVATION
    real(kind = DBL_PREC), allocatable, dimension(:) :: AIRTEMP
    real(kind = DBL_PREC), allocatable, dimension(:) :: WINDS
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_B_E
    real(kind = DBL_PREC), allocatable, dimension(:) :: ice_cover

    !Flags
    integer :: SAFE_MODE
    integer :: SURFACE_BOX

    ! Introduced by Ali
    integer :: FIRST_TIME_STEP
    integer :: INIT_OPTION_OF_FE_II_DISS
    integer :: INIT_OPTION_OF_FE_III_DISS
    ! End of flags

	real(kind = DBL_PREC), allocatable, dimension(:) :: K_A_CALC   !calculated aeration reactLr specific coefficient

    !Main process rates
    ! New kinetic rates added 9 September 2015
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FE_II_OXIDATION
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FE_III_REDUCTION
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_MN_II_OXIDATION
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_MN_IV_REDUCTION
    ! End of new kinetic rates added 9 September 2015

    ! New kinetic rates introduced 27 January 2016 for the redox sequences
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOC_MIN_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOC_MIN_NO3N
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOC_MIN_MN_IV
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOC_MIN_FE_III
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOC_MIN_DOC
    ! End of new kinetic rates introduced 27 January 2016 for the redox sequences

    ! New kinetic rates introduced 28 January 2016 for the redox sequences
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DON_MIN_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DON_MIN_NO3N
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DON_MIN_MN_IV
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DON_MIN_FE_III
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DON_MIN_S_PLUS_6
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DON_MIN_DOC

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOP_MIN_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOP_MIN_NO3N
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOP_MIN_MN_IV
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOP_MIN_FE_III
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOP_MIN_S_PLUS_6
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOP_MIN_DOC
    ! End of new kinetic rates introduced 28 January 2016 for the redox sequences

    ! New kinetic rates introduced 29 January 2016 for the redox sequences
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_SULPHATE_REDUCTION
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_SULPHIDE_OXIDATION
    real(kind = DBL_PREC), allocatable, dimension(:) :: H2S_ATM_EXCHANGE
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_METHANOGENESIS
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_METHANE_OXIDATION
    real(kind = DBL_PREC), allocatable, dimension(:) :: CH4_ATM_EXCHANGE
    ! End of new kinetic rates introduced 29 January 2016 for the redox sequences

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_AERATION

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DIA_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DIA_MET
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DIA_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DIA_EXCR

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DIA_INT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DIA_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DIA_DEATH

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_CYN_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_CYN_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_CYN_MET
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_CYN_EXCR

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_CYN_INT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_CYN_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_CYN_DEATH

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FIX_CYN_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FIX_CYN_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FIX_CYN_MET
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FIX_CYN_EXCR
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FIX_CYN_INT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FIX_CYN_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FIX_CYN_DEATH

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_OPA_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_OPA_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_OPA_MET
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_OPA_EXCR

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_OPA_INT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_OPA_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_OPA_DEATH

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_FEEDING_DIA
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_FEEDING_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_FEEDING_OPA
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_FEEDING_FIX_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_FEEDING_DET_PART_ORG_C

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_INT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_DEATH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_EX_DON
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_EX_DOP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_EX_DOC

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DET_PART_ORG_C_DISSOLUTION
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_PHYT_DISS_DET_PART_ORG_C

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_N_MIN_DON_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_P_MIN_DOP_P

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_PART_SI_DISS
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NON_FIX_CYN_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FIX_FIX_CYN_GROWTH

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOC_MIN
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DON_MIN
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_DOP_MIN

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_PHYT_AMIN_DOC

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_N_AMIN_DON
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_PHY_N_AMIN_DON
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_DON_DON

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_P_AMIN_DOP
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_PHY_P_AMIN_DOP
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_DISS_ORG_P


    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ABIOTIC_NITR
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_NITR_OXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_NITR_NH4_N

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_AMMONIA_VOLATIL   !Ammonia volatilization (mgN/L)

    !Derived process rates
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DENITRIFICATION

    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DET_PART_ORG_N_DISSOLUTION
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_N_DISS_DET_PART_ORG_N

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_PHY_N_DISS_DET_PART_ORG_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DET_PART_ORG_P_DISSOLUTION
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_P_DISS_DET_PART_ORG_P

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_PHY_P_DISS_DET_PART_ORG_P

    !Auxillary variables
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_OXYGEN_SAT
    real(kind = DBL_PREC), allocatable, dimension(:) :: CHLA
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_E
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_DIA
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_DIA
    real(kind = DBL_PREC), allocatable, dimension(:) :: FAC_HYPOX_DIA_D
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_NH4N_DIA
    real(kind = DBL_PREC), allocatable, dimension(:) :: KD_DIA
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_ZOO
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_ZOO_DIA
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_ZOO_DET_PART_ORG_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: FOOD_AVAIL_ZOO
    real(kind = DBL_PREC), allocatable, dimension(:) :: FOOD_FACTOR_ZOO_DIA
    real(kind = DBL_PREC), allocatable, dimension(:) :: FOOD_FACTOR_ZOO_DET_PART_ORG_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: ACTUAL_ZOO_N_TO_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: ACTUAL_ZOO_P_TO_C

    real(kind = DBL_PREC), allocatable, dimension(:) :: FAC_HYPOX_ZOO_D
    real(kind = DBL_PREC), allocatable, dimension(:) :: ACTUAL_DET_N_TO_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: ACTUAL_DET_P_TO_C
    real(kind = DBL_PREC), allocatable, dimension(:) :: KD_ZOO

    real(kind = DBL_PREC), allocatable, dimension(:) :: KD_FIX_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: FAC_HYPOX_FIX_CYN_D
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_NH4N_DON_FIX_CYN

    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_ZOO_FIX_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: FOOD_FACTOR_ZOO_FIX_CYN

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_DIA_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_DIA_NUTR
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_DIA_LIGHT

    real(kind = DBL_PREC), allocatable, dimension(:) :: DIA_LIGHT_SAT
    real(kind = DBL_PREC), allocatable, dimension(:) :: CYN_LIGHT_SAT
    real(kind = DBL_PREC), allocatable, dimension(:) :: FIX_CYN_LIGHT_SAT
    real(kind = DBL_PREC), allocatable, dimension(:) :: OPA_LIGHT_SAT
    real(kind = DBL_PREC), allocatable, dimension(:) :: NOST_LIGHT_SAT

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_CYN_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_CYN_NUTR
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_CYN_LIGHT
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: KD_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: FAC_HYPOX_CYN_D
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_NH4N_DON_CYN ! old still in used by others CYNs fixme
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_DIN_DON_CYN  ! New used only by buoyant CYNs fixme
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_NH4N_CYN     ! New used only by buoyant CYNs fixme
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_OPA
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_OPA_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_OPA_NUTR
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_OPA_LIGHT
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_OPA
    real(kind = DBL_PREC), allocatable, dimension(:) :: KD_OPA
    real(kind = DBL_PREC), allocatable, dimension(:) :: FAC_HYPOX_OPA_D
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_NH4N_OPA
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_ZOO_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_ZOO_OPA

    real(kind = DBL_PREC), allocatable, dimension(:) :: FOOD_FACTOR_ZOO_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: FOOD_FACTOR_ZOO_OPA

    !limitation factors
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_DIA_TEMP
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_DIA_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_DIA_P
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_DIA_DISS_Si

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_CYN_TEMP
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_CYN_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_CYN_P

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_OPA_TEMP
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_OPA_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_OPA_P

    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_FIX_CYN

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_FIX_CYN_TEMP
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_FIX_CYN_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_FIX_CYN_LIGHT
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NON_FIX_CYN_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NON_FIX_CYN_P
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NON_FIX_CYN_NUTR
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NON_FIX_CYN

    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_FIX_FIX_CYN_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_FIX_FIX_CYN_P
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_FIX_FIX_CYN_NUTR
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_FIX_FIX_CYN

    real(kind = DBL_PREC), allocatable, dimension(:) :: ALPHA_0
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALPHA_1

    integer smith
    real(kind = DBL_PREC) :: FIX_CYN_DEPTH ! equivalent depth for the fixers selfshading (assumed they are on the surface)
    ! 1 - Smith light limitation with constant C:chla
    ! 0 - Ali light limitation


    !18 September 2014
    !New variables for DIC and ALK

    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_PAR1
    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_PAR2
    integer,              allocatable, dimension(:) :: CO2SYS_PAR1TYPE
    integer,              allocatable, dimension(:) :: CO2SYS_PAR2TYPE
    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_SALT
    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_TEMPIN
    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_TEMPOUT
    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_PRESIN
    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_PRESOUT
    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_SI
    real(kind = DBL_PREC),allocatable, dimension(:) :: CO2SYS_PO4
    integer,              allocatable, dimension(:) :: CO2SYS_pHSCALEIN
    integer,              allocatable, dimension(:) :: CO2SYS_K1K2CONSTANTS
    integer,              allocatable, dimension(:) :: CO2SYS_KSO4CONSTANTS

    real(kind = DBL_PREC),allocatable, dimension(:,:) :: CO2SYS_OUT_DATA
    character(len=34),    allocatable, dimension(:)   :: CO2SYS_NICEHEADERS

    ! Variables and constansts for Dissolved Inorganic Carbon
    real(kind = DBL_PREC), allocatable, dimension(:) :: TOTAL_DIC_KINETIC_SOURCES
    real(kind = DBL_PREC), allocatable, dimension(:) :: TOTAL_DIC_KINETIC_SINKS

    real(kind = DBL_PREC), allocatable, dimension(:) :: T_A
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_K_H
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_H
    real(kind = DBL_PREC), allocatable, dimension(:) :: POWER
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_CO2
    real(kind = DBL_PREC), allocatable, dimension(:) :: CO2_SAT
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_A_CALC_CO2
    real(kind = DBL_PREC), allocatable, dimension(:) :: CO2_ATM_EXHANGE
    real(kind = DBL_PREC), allocatable, dimension(:) :: DIC_KINETIC_DERIVATIVE
    real(kind = DBL_PREC), allocatable, dimension(:) :: H2CO3

    !Introduced by Petras instead of using co2sys_outdata directly
    real(kind = DBL_PREC), allocatable, dimension(:) :: HCO3
    real(kind = DBL_PREC), allocatable, dimension(:) :: CO3


    ! Variables and constansts for Alkalinity?
    real(kind = DBL_PREC), allocatable, dimension(:) :: PKH
    real(kind = DBL_PREC), allocatable, dimension(:) :: FRAC_NH3
    real(kind = DBL_PREC), allocatable, dimension(:) :: FRAC_NH4
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_DIA_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_CYN_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_OPA_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_FIX_CYN_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_ZOO_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_ABIOTIC_DON_MIN
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALK_GAINED_BY_AMMONIUM_GEN
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_DENITRIFICATION
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_DIA_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_CYN_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_OPA_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_NON_FIX_CYN_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALK_GAINED_BY_NITRATE_CONS
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALK_LOST_BY_AMMONIUM_CONS
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_NITRIFICATION_NH4
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_NITRIFICATION_NH3
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALK_LOST_BY_NITRIFICATION
    real(kind = DBL_PREC), allocatable, dimension(:) :: H_PLUS
    real(kind = DBL_PREC), allocatable, dimension(:) :: N_NOST_VEG_HET_NH4_RELEASE
    real(kind = DBL_PREC), allocatable, dimension(:) :: NO3N_NOST_VEG_HET_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: NH4N_NOST_VEG_HET_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: NH4N_SEDIMENT_INFLUX
    real(kind = DBL_PREC), allocatable, dimension(:) :: NH4N_SEDIMENT_OUTFLUX
    real(kind = DBL_PREC), allocatable, dimension(:) :: NO3N_SEDIMENT_INFLUX
    real(kind = DBL_PREC), allocatable, dimension(:) :: NO3N_SEDIMENT_OUTFLUX
    real(kind = DBL_PREC), allocatable, dimension(:) :: PO4P_SEDIMENT_INFLUX
    real(kind = DBL_PREC), allocatable, dimension(:) :: PO4P_SEDIMENT_OUTFLUX

    ! Indicators
    integer :: TEST_MODE
    integer :: KP_OPTION
    ! Constansts:
    real(kind = DBL_PREC) :: A_1
    real(kind = DBL_PREC) :: A_2
    real(kind = DBL_PREC) :: A_3
    real(kind = DBL_PREC) :: B_1
    real(kind = DBL_PREC) :: B_2
    real(kind = DBL_PREC) :: C_1
    real(kind = DBL_PREC) :: C_2

    real(kind = DBL_PREC), allocatable, dimension(:) :: K_ONE_TIP
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_TWO_TIP
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_THREE_TIP
    real(kind = DBL_PREC), allocatable, dimension(:) :: FRACTION_DIVISOR_TIP
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALPHA_H2PO4
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALPHA_HPO4
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALPHA_PO4
    real(kind = DBL_PREC), allocatable, dimension(:) :: PHOSPHATE_EQ_CONSTANT
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALK_GAINED_BY_PHOSPHATE_CONS
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALK_LOST_BY_PHOSPHATE_GEN
    real(kind = DBL_PREC), allocatable, dimension(:) :: ALK_KINETIC_DERIVATIVE

    real(kind = DBL_PREC), allocatable, dimension(:) :: P_DIA_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_CYN_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_OPA_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_FIX_CYN_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_ZOO_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_ABIOTIC_DOP_MIN
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_DIA_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_CYN_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_OPA_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_NON_FIX_CYN_GROWTH

    ! -------------------------------------------------------------------------
    ! Variables added for the new pH correction algorithm
    ! 31st August 2015
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_NITR_NH4
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DENITR_NO3

    ! -------------------------------------------------------------------------
    ! End of variables added for the new pH correction algorithm
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Variables added 30 November 2015 for dissolved and particulate species
    ! of FE_II, FE_III, MN_II, MN_IV. These will be internally calculated by
    ! this subroutine and will be returned in some suitable way to AQUABC since
    ! they will be used in settling calculations. Job by Petras and his fellows.
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), allocatable, dimension(:) :: MULT_FE_II_DISS  !Dissolved fraction of FE_II
    real(kind = DBL_PREC), allocatable, dimension(:) :: MULT_FE_II_PART  !Particulate fraction of FE_II
    real(kind = DBL_PREC), allocatable, dimension(:) :: MULT_FE_III_DISS !Dissolved fraction of FE_III
    real(kind = DBL_PREC), allocatable, dimension(:) :: MULT_FE_III_PART !Particulate fraction of FE_III

    real(kind = DBL_PREC), allocatable, dimension(:) :: MULT_MN_II_DISS  !Dissolved fraction of MN_II
    real(kind = DBL_PREC), allocatable, dimension(:) :: MULT_MN_II_PART  !Particulate fraction of MN_II
    real(kind = DBL_PREC), allocatable, dimension(:) :: MULT_MN_IV_DISS  !Dissolved fraction of MN_IV
    real(kind = DBL_PREC), allocatable, dimension(:) :: MULT_MN_IV_PART  !Particulate fraction of MN_IV
    ! -------------------------------------------------------------------------
    ! End of variables added 30 November 2015 for dissolved and particulate
    ! species of FE_II, FE_III, MN_II, MN_IV
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Variables added 25 January 2016 for a simple version of equlibrium
    ! based aquatic chemistry calculations for Fe2+ and Fe3+
    ! -------------------------------------------------------------------------
    !
    ! Variable list:
    ! K_EQ_S_1                    : 1st Dissociaciton constant for H2S
    ! K_EQ_S_2                    : 2nd Dissociaciton constant for H2S
    ! K_SP_FES                    : Solubility pproduct for FeS
    ! HS2_TOT                     : Total H2S in moles, (will be replaced by state variable)
    ! H2S_DIVISOR                 : Auxillary variable
    ! FRAC_H2S_IN_H2S_TOT         : Fraction of S-- in total H2S
    ! FRAC_HS_MINUS_IN_H2S_TOT    : Fraction of HS- in total H2S
    ! FRAC_S_MINUS_TWO_IN_H2S_TOT : Fraction of H2S in total H2S
    ! H2S                         : H2S in moles
    ! HS_MINUS                    : HS- in moles
    ! S_MINUS_TWO                 : S-- in moles
    ! FE_II_DISS                  : Dissolved Fe2+
    ! FE_II_PART                  : Particulate Fe2+
    ! FE_III_DISS                 : Dissolved Fe3+
    ! FE_III_PART                 : Particulate Fe3+

    real(kind = DBL_PREC), allocatable, dimension(:) :: K_EQ_S_1
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_EQ_S_2
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_SP_FES
    real(kind = DBL_PREC), allocatable, dimension(:) :: HS2_TOT
    real(kind = DBL_PREC), allocatable, dimension(:) :: H2S_DIVISOR
    real(kind = DBL_PREC), allocatable, dimension(:) :: FRAC_H2S_IN_H2S_TOT
    real(kind = DBL_PREC), allocatable, dimension(:) :: FRAC_HS_MINUS_IN_H2S_TOT
    real(kind = DBL_PREC), allocatable, dimension(:) :: FRAC_S_MINUS_TWO_IN_H2S_TOT
    real(kind = DBL_PREC), allocatable, dimension(:) :: H2S
    real(kind = DBL_PREC), allocatable, dimension(:) :: HS_MINUS
    real(kind = DBL_PREC), allocatable, dimension(:) :: S_MINUS_TWO
    real(kind = DBL_PREC), allocatable, dimension(:) :: FE_II_DISS

    real(kind = DBL_PREC), allocatable, dimension(:) :: FE_II_PART
    real(kind = DBL_PREC), allocatable, dimension(:) :: FE_III_DISS
    real(kind = DBL_PREC), allocatable, dimension(:) :: FE_III_PART
    real(kind = DBL_PREC), allocatable, dimension(:) :: MN_II_DISS
    real(kind = DBL_PREC), allocatable, dimension(:) :: MN_II_PART

    ! -------------------------------------------------------------------------
    ! New variables added (9 August 2016)
    ! -------------------------------------------------------------------------
    ! FE_II_DISS_EQ               : Dissolved Fe2+ in equilibrium (solubility of Fe2+)
    ! FE_III_DISS_EQ              : Dissolved Fe2+ in equilibrium (solubility of Fe3+)
    real(kind = DBL_PREC), allocatable, dimension(:) :: FE_II_DISS_EQ
    real(kind = DBL_PREC), allocatable, dimension(:) :: FE_III_DISS_EQ
    ! -------------------------------------------------------------------------
    ! End of new variables added (9 August 2016)
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! End of variables added 25 January 2016 for a simple version of equlibrium
    ! based aquatic chemistry calculations for Fe2+ and Fe3+
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! New auxillary variables introduced 27 January 2016
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOC_MIN_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOC_MIN_NO3N
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOC_MIN_MN_IV
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOC_MIN_FE_III
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOC_MIN_DOC
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_DOXY_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_NO3N_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_MN_IV_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_FE_III_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_S_PLUS_6_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_DOC_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_NO3_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_MN_IV_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_FE_III_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_S_PLUS_6_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_DOC_RED
    real(kind = DBL_PREC), allocatable, dimension(:) :: MN_IV_DISS
    ! -------------------------------------------------------------------------
    ! End of new auxillary variables introduced 27 January 2016
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! New auxillary variables introduced 28 January 2016
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DON_MIN_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DON_MIN_NO3N
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DON_MIN_MN_IV
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DON_MIN_FE_III
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DON_MIN_S_PLUS_6
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DON_MIN_DOC

    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOP_MIN_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOP_MIN_NO3N
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOP_MIN_MN_IV
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOP_MIN_FE_III
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOP_MIN_S_PLUS_6
    real(kind = DBL_PREC), allocatable, dimension(:) :: PH_CORR_DOP_MIN_DOC
    ! -------------------------------------------------------------------------
    ! End of new auxillary variables introduced 28 January 2016
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! New auxillary variables introduced 29 January 2016
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_A_CH4
    real(kind = DBL_PREC), allocatable, dimension(:) :: K_A_H2S
    real(kind = DBL_PREC), allocatable, dimension(:) :: CH4_SAT
    real(kind = DBL_PREC), allocatable, dimension(:) :: H2S_SAT
    ! -------------------------------------------------------------------------
    ! End of new auxillary variables introduced 29 January 2016
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! New auxillary variables introduced / January 2016
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), allocatable, dimension(:) :: PE
    ! -------------------------------------------------------------------------
    ! End of new auxillary variables introduced / January 2016
    ! -------------------------------------------------------------------------


    ! -------------------------------------------------------------------------
    ! New auxillary variables introduced 6 July 2016
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), allocatable, dimension(:) :: DIP_OVER_IP
    ! -------------------------------------------------------------------------
    ! End of new auxillary variables introduced 6 July 2016
    ! -------------------------------------------------------------------------
    integer     :: iron_oxidation

    ! -------------------------------------------------------------------------
    ! New auxillary variables introduced 9 August 2016
    ! -------------------------------------------------------------------------
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_FE_II_CONC_TS_END
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_FE_II_CONC_TS_AVG
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_FE_III_CONC_TS_END
    real(kind = DBL_PREC), allocatable, dimension(:) :: DISS_FE_III_CONC_TS_AVG

    integer  :: DO_ADVANCED_REDOX_SIMULATION
    integer  :: DO_CYANO_BOUYANT_STATE_SIMULATION
    integer  :: DO_NON_OBLIGATORY_FIXERS
    integer  :: DO_NOSTOCALES

    real(kind = DBL_PREC) :: J_DAY

    ! -----------------------------------------------------------------------------------
    ! New auxillary variablesfor nostacles incorporated 10 September 2019
    ! -----------------------------------------------------------------------------------

    ! Zooplankton (nostocales feeding related)
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_ZOO_NOST_VEG_HET
    real(kind = DBL_PREC), allocatable, dimension(:) :: FOOD_FACTOR_ZOO_NOST_VEG_HET
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_ZOO_FEEDING_NOST_VEG_HET

    ! Nostocales
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_DIP_DOP_NOST
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_NH4N_NOST
    real(kind = DBL_PREC), allocatable, dimension(:) :: PREF_DIN_DON_NOST
    real(kind = DBL_PREC), allocatable, dimension(:) :: KG_NOST_VEG_HET
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NOST_VEG_HET_LIGHT
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NOST_VEG_HET_TEMP
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NOST_VEG_HET_DOXY
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NOST_VEG_HET_N
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NOST_VEG_HET_P
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NOST_VEG_HET_FIX
    real(kind = DBL_PREC), allocatable, dimension(:) :: LIM_KG_NOST_VEG_HET_NON_FIX
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_FIX_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_NON_FIX_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_MET
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_EXCR
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_INT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: KD_NOST_VEG_HET
    real(kind = DBL_PREC), allocatable, dimension(:) :: FAC_HYPOX_NOST_VEG_HET_D
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_DEATH
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_DENS_MORT_NOST_VEG_HET
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_GERM_NOST_AKI
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_FORM_NOST_AKI
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_LOSS_AKI
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_MORT_AKI
    real(kind = DBL_PREC), allocatable, dimension(:) :: R_NOST_VEG_HET_TOT_RESP

    real(kind = DBL_PREC), allocatable, dimension(:) :: N_NOST_VEG_HET_TOT_RESP
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_FIX_CYN_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_NOST_VEG_HET_GROWTH
    real(kind = DBL_PREC), allocatable, dimension(:) :: P_NOST_VEG_HET_TOT_RESP
    ! -----------------------------------------------------------------------------------
    ! End of new auxillary variablesfor nostacles incorporated 10 September 2019
    ! -----------------------------------------------------------------------------------


    ! For strange values processing
    ! Strange values

    real(kind = DBL_PREC),allocatable, dimension (:) :: STRANGERS

    ! node numbers with strange values (numbers of active nodes in the layer)
    integer              ,allocatable, dimension (:) :: NODES_STRANGE

    !internal node numbers with strange values
    integer              ,allocatable, dimension (:) :: NODES_STRANGE_int

    !external node numbers with strange values
    integer              ,allocatable, dimension (:) :: NODES_STRANGE_ext

    ! number of nodes with strange values
    integer :: nstrange
    logical debug_stranger

    integer :: NODE_COUNT
    !End of new variables for DIC and ALK

    logical, allocatable, dimension(:) :: VALUE_strange !For NaN and Inf checking

    real(kind = DBL_PREC), allocatable, dimension(:) :: GROWTH_INHIB_FACTOR_DIA
    real(kind = DBL_PREC), allocatable, dimension(:) :: GROWTH_INHIB_FACTOR_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: GROWTH_INHIB_FACTOR_FIX_CYN
    real(kind = DBL_PREC), allocatable, dimension(:) :: GROWTH_INHIB_FACTOR_NOST
    real(kind = DBL_PREC), allocatable, dimension(:) :: GROWTH_INHIB_FACTOR_OPA
    real(kind = DBL_PREC), allocatable, dimension(:) :: GROWTH_INHIB_FACTOR_ZOO


end module AQUABC_PELAGIC_INTERNAL


subroutine ALLOC_AQUABC_PELAGIC_INTERNAL(nkn)
    use AQUABC_PELAGIC_INTERNAL
    implicit none
    integer, intent(in) :: nkn
    integer :: ierr

    !State variables
    allocate(NH4_N                         (nkn), stat=ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: ALLOC_AQUABC_PELAGIC_INTERNAL: state variables allocation failed, nkn=', nkn
        stop
    end if
    allocate(NO3_N                         (nkn))
    allocate(PO4_P                         (nkn))
    allocate(DISS_OXYGEN                   (nkn))
    allocate(DIA_C                         (nkn))
    allocate(ZOO_C                         (nkn))
    allocate(ZOO_N                         (nkn))
    allocate(ZOO_P                         (nkn))
    allocate(DET_PART_ORG_C                (nkn))
    allocate(DET_PART_ORG_N                (nkn))
    allocate(DET_PART_ORG_P                (nkn))
    allocate(DISS_ORG_C                    (nkn))
    allocate(DISS_ORG_N                    (nkn))
    allocate(DISS_ORG_P                    (nkn))
    allocate(CYN_C                         (nkn))
    allocate(OPA_C                         (nkn))
    allocate(DISS_Si                       (nkn))
    allocate(PART_Si                       (nkn))
    allocate(FIX_CYN_C                     (nkn))
    allocate(INORG_C                       (nkn))
    allocate(TOT_ALK                       (nkn))
    allocate(FE_II                         (nkn))
    allocate(FE_III                        (nkn))
    allocate(MN_II                         (nkn))
    allocate(MN_IV                         (nkn))
    allocate(CA                            (nkn))
    allocate(MG                            (nkn))
    allocate(S_PLUS_6                      (nkn))
    allocate(S_MINUS_2                     (nkn))
    allocate(CH4_C                         (nkn))
    allocate(NOST_VEG_HET_C                (nkn))
    allocate(NOST_AKI_C                    (nkn))

    allocate(PHYT_TOT_C                    (nkn))

    !Driving functions
    allocate(TEMP                          (nkn), stat=ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: ALLOC_AQUABC_PELAGIC_INTERNAL: driving functions allocation failed, nkn=', nkn
        stop
    end if
    allocate(FDAY                          (nkn))
    allocate(DEPTH                         (nkn))
    allocate(I_A                           (nkn))
    allocate(SALT                          (nkn))
    allocate(ELEVATION                     (nkn))
    allocate(AIRTEMP                       (nkn))
    allocate(WINDS                         (nkn))
    allocate(K_B_E                         (nkn))
    allocate(ice_cover                     (nkn))
	allocate(K_A_CALC                      (nkn))   !calculated aeration reactLr specific coefficient

    !Main process rates
    allocate(R_FE_II_OXIDATION             (nkn), stat=ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: ALLOC_AQUABC_PELAGIC_INTERNAL: main process rates allocation failed, nkn=', nkn
        stop
    end if
    allocate(R_FE_III_REDUCTION            (nkn))
    allocate(R_MN_II_OXIDATION             (nkn))
    allocate(R_MN_IV_REDUCTION             (nkn))
    allocate(R_ABIOTIC_DOC_MIN_DOXY        (nkn))
    allocate(R_ABIOTIC_DOC_MIN_NO3N        (nkn))
    allocate(R_ABIOTIC_DOC_MIN_MN_IV       (nkn))
    allocate(R_ABIOTIC_DOC_MIN_FE_III      (nkn))
    allocate(R_ABIOTIC_DOC_MIN_S_PLUS_6    (nkn))
    allocate(R_ABIOTIC_DOC_MIN_DOC         (nkn))
    allocate(R_ABIOTIC_DON_MIN_DOXY        (nkn))
    allocate(R_ABIOTIC_DON_MIN_NO3N        (nkn))
    allocate(R_ABIOTIC_DON_MIN_MN_IV       (nkn))
    allocate(R_ABIOTIC_DON_MIN_FE_III      (nkn))
    allocate(R_ABIOTIC_DON_MIN_S_PLUS_6    (nkn))
    allocate(R_ABIOTIC_DON_MIN_DOC         (nkn))
    allocate(R_ABIOTIC_DOP_MIN_DOXY        (nkn))
    allocate(R_ABIOTIC_DOP_MIN_NO3N        (nkn))
    allocate(R_ABIOTIC_DOP_MIN_MN_IV       (nkn))
    allocate(R_ABIOTIC_DOP_MIN_FE_III      (nkn))
    allocate(R_ABIOTIC_DOP_MIN_S_PLUS_6    (nkn))
    allocate(R_ABIOTIC_DOP_MIN_DOC         (nkn))
    allocate(R_SULPHATE_REDUCTION          (nkn))
    allocate(R_SULPHIDE_OXIDATION          (nkn))
    allocate(H2S_ATM_EXCHANGE              (nkn))
    allocate(R_METHANOGENESIS              (nkn))
    allocate(R_METHANE_OXIDATION           (nkn))
    allocate(CH4_ATM_EXCHANGE              (nkn))
    allocate(R_AERATION                    (nkn))
    allocate(R_DIA_GROWTH                  (nkn))
    allocate(R_DIA_MET                     (nkn))
    allocate(R_DIA_RESP                    (nkn))
    allocate(R_DIA_EXCR                    (nkn))
    allocate(R_DIA_INT_RESP                (nkn))
    allocate(R_DIA_TOT_RESP                (nkn))
    allocate(R_DIA_DEATH                   (nkn))
    allocate(R_CYN_GROWTH                  (nkn))
    allocate(R_CYN_RESP                    (nkn))
    allocate(R_CYN_MET                     (nkn))
    allocate(R_CYN_EXCR                    (nkn))
    allocate(R_CYN_INT_RESP                (nkn))
    allocate(R_CYN_TOT_RESP                (nkn))
    allocate(R_CYN_DEATH                   (nkn))
    allocate(R_FIX_CYN_GROWTH              (nkn))
    allocate(R_FIX_CYN_RESP                (nkn))
    allocate(R_FIX_CYN_MET                 (nkn))
    allocate(R_FIX_CYN_EXCR                (nkn))
    allocate(R_FIX_CYN_INT_RESP            (nkn))
    allocate(R_FIX_CYN_TOT_RESP            (nkn))
    allocate(R_FIX_CYN_DEATH               (nkn))
    allocate(R_OPA_GROWTH                  (nkn))
    allocate(R_OPA_RESP                    (nkn))
    allocate(R_OPA_MET                     (nkn))
    allocate(R_OPA_EXCR                    (nkn))
    allocate(R_OPA_INT_RESP                (nkn))
    allocate(R_OPA_TOT_RESP                (nkn))
    allocate(R_OPA_DEATH                   (nkn))
    allocate(R_ZOO_GROWTH                  (nkn))
    allocate(R_ZOO_FEEDING_DIA             (nkn))
    allocate(R_ZOO_FEEDING_CYN             (nkn))
    allocate(R_ZOO_FEEDING_OPA             (nkn))
    allocate(R_ZOO_FEEDING_FIX_CYN         (nkn))
    allocate(R_ZOO_FEEDING_DET_PART_ORG_C  (nkn))
    allocate(R_ZOO_RESP                    (nkn))
    allocate(R_ZOO_INT_RESP                (nkn))
    allocate(R_ZOO_TOT_RESP                (nkn))
    allocate(R_ZOO_DEATH                   (nkn))
    allocate(R_ZOO_EX_DON                  (nkn))
    allocate(R_ZOO_EX_DOP                  (nkn))
    allocate(R_ZOO_EX_DOC                  (nkn))
    allocate(R_DET_PART_ORG_C_DISSOLUTION  (nkn))
    allocate(LIM_PHYT_DISS_DET_PART_ORG_C  (nkn))
    allocate(LIM_N_MIN_DON_N               (nkn))
    allocate(LIM_P_MIN_DOP_P               (nkn))
    allocate(R_PART_SI_DISS                (nkn))
    allocate(R_NON_FIX_CYN_GROWTH          (nkn))
    allocate(R_FIX_FIX_CYN_GROWTH          (nkn))
    allocate(R_ABIOTIC_DOC_MIN             (nkn))
    allocate(R_ABIOTIC_DON_MIN             (nkn))
    allocate(R_ABIOTIC_DOP_MIN             (nkn))
    allocate(LIM_PHYT_AMIN_DOC             (nkn))
    allocate(LIM_N_AMIN_DON                (nkn))
    allocate(LIM_PHY_N_AMIN_DON            (nkn))
    allocate(LIM_DON_DON                   (nkn))
    allocate(LIM_P_AMIN_DOP                (nkn))
    allocate(LIM_PHY_P_AMIN_DOP            (nkn))
    allocate(LIM_DISS_ORG_P                (nkn))
    allocate(R_ABIOTIC_NITR                (nkn))
    allocate(LIM_NITR_OXY                  (nkn))
    allocate(LIM_NITR_NH4_N                (nkn))
    allocate(R_AMMONIA_VOLATIL             (nkn))

    !Derived process rates
    allocate(R_DENITRIFICATION             (nkn), stat=ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: ALLOC_AQUABC_PELAGIC_INTERNAL: derived process rates allocation failed, nkn=', nkn
        stop
    end if
    allocate(R_DET_PART_ORG_N_DISSOLUTION  (nkn))
    allocate(LIM_N_DISS_DET_PART_ORG_N     (nkn))
    allocate(LIM_PHY_N_DISS_DET_PART_ORG_N (nkn))
    allocate(R_DET_PART_ORG_P_DISSOLUTION  (nkn))
    allocate(LIM_P_DISS_DET_PART_ORG_P     (nkn))
    allocate(LIM_PHY_P_DISS_DET_PART_ORG_P (nkn))

    !Auxillary variables
    allocate(DISS_OXYGEN_SAT               (nkn), stat=ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: ALLOC_AQUABC_PELAGIC_INTERNAL: auxillary variables allocation failed, nkn=', nkn
        stop
    end if
    allocate(CHLA                          (nkn))
    allocate(K_E                           (nkn))
    allocate(KG_DIA                        (nkn))
    allocate(LIM_KG_DIA                    (nkn))
    allocate(FAC_HYPOX_DIA_D               (nkn))
    allocate(PREF_NH4N_DIA                 (nkn))
    allocate(KD_DIA                        (nkn))
    allocate(KG_ZOO                        (nkn))
    allocate(KG_ZOO_DIA                    (nkn))
    allocate(KG_ZOO_DET_PART_ORG_C         (nkn))
    allocate(FOOD_AVAIL_ZOO                (nkn))
    allocate(FOOD_FACTOR_ZOO_DIA           (nkn))
    allocate(FOOD_FACTOR_ZOO_DET_PART_ORG_C(nkn))
    allocate(ACTUAL_ZOO_N_TO_C             (nkn))
    allocate(ACTUAL_ZOO_P_TO_C             (nkn))
    allocate(FAC_HYPOX_ZOO_D               (nkn))
    allocate(ACTUAL_DET_N_TO_C             (nkn))
    allocate(ACTUAL_DET_P_TO_C             (nkn))
    allocate(KD_ZOO                        (nkn))
    allocate(KD_FIX_CYN                    (nkn))
    allocate(FAC_HYPOX_FIX_CYN_D           (nkn))
    allocate(PREF_NH4N_DON_FIX_CYN         (nkn))
    allocate(KG_ZOO_FIX_CYN                (nkn))
    allocate(FOOD_FACTOR_ZOO_FIX_CYN       (nkn))
    allocate(LIM_KG_DIA_DOXY               (nkn))
    allocate(LIM_KG_DIA_NUTR               (nkn))
    allocate(LIM_KG_DIA_LIGHT              (nkn))
    allocate(DIA_LIGHT_SAT                 (nkn))
    allocate(CYN_LIGHT_SAT                 (nkn))
    allocate(FIX_CYN_LIGHT_SAT             (nkn))
    allocate(OPA_LIGHT_SAT                 (nkn))
    allocate(NOST_LIGHT_SAT                (nkn))
    allocate(LIM_KG_CYN_DOXY               (nkn))
    allocate(LIM_KG_CYN_NUTR               (nkn))
    allocate(LIM_KG_CYN_LIGHT              (nkn))
    allocate(LIM_KG_CYN                    (nkn))
    allocate(KD_CYN                        (nkn))
    allocate(FAC_HYPOX_CYN_D               (nkn))
    allocate(PREF_NH4N_DON_CYN             (nkn))       ! old still in used by others CYNs fixme
    allocate(PREF_DIN_DON_CYN              (nkn))       ! New used only by buoyant CYNs fixme
    allocate(PREF_NH4N_CYN                 (nkn))       ! New used only by buoyant CYNs fixme
    allocate(KG_OPA                        (nkn))
    allocate(KG_CYN                        (nkn))
    allocate(LIM_KG_OPA_DOXY               (nkn))
    allocate(LIM_KG_OPA_NUTR               (nkn))
    allocate(LIM_KG_OPA_LIGHT              (nkn))
    allocate(LIM_KG_OPA                    (nkn))
    allocate(KD_OPA                        (nkn))
    allocate(FAC_HYPOX_OPA_D               (nkn))
    allocate(PREF_NH4N_OPA                 (nkn))
    allocate(KG_ZOO_CYN                    (nkn))
    allocate(KG_ZOO_OPA                    (nkn))
    allocate(FOOD_FACTOR_ZOO_CYN           (nkn))
    allocate(FOOD_FACTOR_ZOO_OPA           (nkn))

    allocate(LIM_KG_DIA_TEMP               (nkn))
    allocate(LIM_KG_DIA_N                  (nkn))
    allocate(LIM_KG_DIA_P                  (nkn))
    allocate(LIM_KG_DIA_DISS_Si            (nkn))
    allocate(LIM_KG_CYN_TEMP               (nkn))
    allocate(LIM_KG_CYN_N                  (nkn))
    allocate(LIM_KG_CYN_P                  (nkn))
    allocate(LIM_KG_OPA_TEMP               (nkn))
    allocate(LIM_KG_OPA_N                  (nkn))
    allocate(LIM_KG_OPA_P                  (nkn))
    allocate(KG_FIX_CYN                    (nkn))
    allocate(LIM_KG_FIX_CYN_TEMP           (nkn))
    allocate(LIM_KG_FIX_CYN_DOXY           (nkn))
    allocate(LIM_KG_FIX_CYN_LIGHT          (nkn))
    allocate(LIM_KG_NON_FIX_CYN_N          (nkn))
    allocate(LIM_KG_NON_FIX_CYN_P          (nkn))
    allocate(LIM_KG_NON_FIX_CYN_NUTR       (nkn))
    allocate(LIM_KG_NON_FIX_CYN            (nkn))
    allocate(LIM_KG_FIX_FIX_CYN_N          (nkn))
    allocate(LIM_KG_FIX_FIX_CYN_P          (nkn))
    allocate(LIM_KG_FIX_FIX_CYN_NUTR       (nkn))
    allocate(LIM_KG_FIX_FIX_CYN            (nkn))
    allocate(ALPHA_0                       (nkn))
    allocate(ALPHA_1                       (nkn))
    allocate(TOTAL_DIC_KINETIC_SOURCES     (nkn))
    allocate(TOTAL_DIC_KINETIC_SINKS       (nkn))
    allocate(T_A                           (nkn))
    allocate(P_K_H                         (nkn))
    allocate(K_H                           (nkn))
    allocate(POWER                         (nkn))
    allocate(P_CO2                         (nkn))
    allocate(CO2_SAT                       (nkn))
    allocate(K_A_CALC_CO2                  (nkn))
    allocate(CO2_ATM_EXHANGE               (nkn))
    allocate(DIC_KINETIC_DERIVATIVE        (nkn))
    allocate(H2CO3                         (nkn))
    allocate(HCO3                          (nkn))
    allocate(CO3                           (nkn))
    allocate(PKH                           (nkn))
    allocate(FRAC_NH3                      (nkn))
    allocate(FRAC_NH4                      (nkn))
    allocate(N_DIA_TOT_RESP                (nkn))
    allocate(N_CYN_TOT_RESP                (nkn))
    allocate(N_OPA_TOT_RESP                (nkn))
    allocate(N_FIX_CYN_TOT_RESP            (nkn))
    allocate(N_ZOO_TOT_RESP                (nkn))
    allocate(N_ABIOTIC_DON_MIN             (nkn))
    allocate(ALK_GAINED_BY_AMMONIUM_GEN    (nkn))
    allocate(N_DENITRIFICATION             (nkn))
    allocate(N_DIA_GROWTH                  (nkn))
    allocate(N_CYN_GROWTH                  (nkn))
    allocate(N_OPA_GROWTH                  (nkn))
    allocate(N_NON_FIX_CYN_GROWTH          (nkn))
    allocate(ALK_GAINED_BY_NITRATE_CONS    (nkn))
    allocate(ALK_LOST_BY_AMMONIUM_CONS     (nkn))
    allocate(N_NITRIFICATION_NH4           (nkn))
    allocate(N_NITRIFICATION_NH3           (nkn))
    allocate(ALK_LOST_BY_NITRIFICATION     (nkn))
    allocate(H_PLUS                        (nkn))
    allocate(N_NOST_VEG_HET_NH4_RELEASE    (nkn))
    allocate(NO3N_NOST_VEG_HET_GROWTH      (nkn))
    allocate(NH4N_NOST_VEG_HET_GROWTH      (nkn))
    allocate(NH4N_SEDIMENT_INFLUX          (nkn))
    allocate(NH4N_SEDIMENT_OUTFLUX         (nkn))
    allocate(NO3N_SEDIMENT_INFLUX          (nkn))
    allocate(NO3N_SEDIMENT_OUTFLUX         (nkn))
    allocate(PO4P_SEDIMENT_INFLUX          (nkn))
    allocate(PO4P_SEDIMENT_OUTFLUX         (nkn))
    allocate(K_ONE_TIP                     (nkn))
    allocate(K_TWO_TIP                     (nkn))
    allocate(K_THREE_TIP                   (nkn))
    allocate(FRACTION_DIVISOR_TIP          (nkn))
    allocate(ALPHA_H2PO4                   (nkn))
    allocate(ALPHA_HPO4                    (nkn))
    allocate(ALPHA_PO4                     (nkn))
    allocate(PHOSPHATE_EQ_CONSTANT         (nkn))
    allocate(ALK_GAINED_BY_PHOSPHATE_CONS  (nkn))
    allocate(ALK_LOST_BY_PHOSPHATE_GEN     (nkn))
    allocate(ALK_KINETIC_DERIVATIVE        (nkn))
    allocate(P_DIA_TOT_RESP                (nkn))
    allocate(P_CYN_TOT_RESP                (nkn))
    allocate(P_OPA_TOT_RESP                (nkn))
    allocate(P_FIX_CYN_TOT_RESP            (nkn))
    allocate(P_ZOO_TOT_RESP                (nkn))
    allocate(P_ABIOTIC_DOP_MIN             (nkn))
    allocate(P_DIA_GROWTH                  (nkn))
    allocate(P_CYN_GROWTH                  (nkn))
    allocate(P_OPA_GROWTH                  (nkn))
    allocate(P_NON_FIX_CYN_GROWTH          (nkn))
    allocate(PH_CORR_NITR_NH4              (nkn))
    allocate(PH_CORR_DENITR_NO3            (nkn))
    allocate(MULT_FE_II_DISS               (nkn))   !Dissolved fraction of FE_II
    allocate(MULT_FE_II_PART               (nkn))   !Particulate fraction of FE_II
    allocate(MULT_FE_III_DISS              (nkn))   !Dissolved fraction of FE_III
    allocate(MULT_FE_III_PART              (nkn))   !Particulate fraction of FE_III
    allocate(MULT_MN_II_DISS               (nkn))   !Dissolved fraction of MN_II
    allocate(MULT_MN_II_PART               (nkn))   !Particulate fraction of MN_II
    allocate(MULT_MN_IV_DISS               (nkn))   !Dissolved fraction of MN_IV
    allocate(MULT_MN_IV_PART               (nkn))   !Particulate fraction of MN_IV
    allocate(K_EQ_S_1                      (nkn))
    allocate(K_EQ_S_2                      (nkn))
    allocate(K_SP_FES                      (nkn))
    allocate(HS2_TOT                       (nkn))
    allocate(H2S_DIVISOR                   (nkn))
    allocate(FRAC_H2S_IN_H2S_TOT           (nkn))
    allocate(FRAC_HS_MINUS_IN_H2S_TOT      (nkn))
    allocate(FRAC_S_MINUS_TWO_IN_H2S_TOT   (nkn))
    allocate(H2S                           (nkn))
    allocate(HS_MINUS                      (nkn))
    allocate(S_MINUS_TWO                   (nkn))
    allocate(FE_II_DISS                    (nkn))
    allocate(FE_II_PART                    (nkn))
    allocate(FE_III_DISS                   (nkn))
    allocate(FE_III_PART                   (nkn))
    allocate(MN_II_DISS                    (nkn))
    allocate(MN_II_PART                    (nkn))
    allocate(FE_II_DISS_EQ                 (nkn))
    allocate(FE_III_DISS_EQ                (nkn))
    allocate(PH_CORR_DOC_MIN_DOXY          (nkn))
    allocate(PH_CORR_DOC_MIN_NO3N          (nkn))
    allocate(PH_CORR_DOC_MIN_MN_IV         (nkn))
    allocate(PH_CORR_DOC_MIN_FE_III        (nkn))
    allocate(PH_CORR_DOC_MIN_S_PLUS_6      (nkn))
    allocate(PH_CORR_DOC_MIN_DOC           (nkn))
    allocate(LIM_DOXY_RED                  (nkn))
    allocate(LIM_NO3N_RED                  (nkn))
    allocate(LIM_MN_IV_RED                 (nkn))
    allocate(LIM_FE_III_RED                (nkn))
    allocate(LIM_S_PLUS_6_RED              (nkn))
    allocate(LIM_DOC_RED                   (nkn))
    allocate(K_NO3_RED                     (nkn))
    allocate(K_MN_IV_RED                   (nkn))
    allocate(K_FE_III_RED                  (nkn))
    allocate(K_S_PLUS_6_RED                (nkn))
    allocate(K_DOC_RED                     (nkn))
    allocate(MN_IV_DISS                    (nkn))
    allocate(PH_CORR_DON_MIN_DOXY          (nkn))
    allocate(PH_CORR_DON_MIN_NO3N          (nkn))
    allocate(PH_CORR_DON_MIN_MN_IV         (nkn))
    allocate(PH_CORR_DON_MIN_FE_III        (nkn))
    allocate(PH_CORR_DON_MIN_S_PLUS_6      (nkn))
    allocate(PH_CORR_DON_MIN_DOC           (nkn))
    allocate(PH_CORR_DOP_MIN_DOXY          (nkn))
    allocate(PH_CORR_DOP_MIN_NO3N          (nkn))
    allocate(PH_CORR_DOP_MIN_MN_IV         (nkn))
    allocate(PH_CORR_DOP_MIN_FE_III        (nkn))
    allocate(PH_CORR_DOP_MIN_S_PLUS_6      (nkn))
    allocate(PH_CORR_DOP_MIN_DOC           (nkn))
    allocate(K_A_CH4                       (nkn))
    allocate(K_A_H2S                       (nkn))
    allocate(CH4_SAT                       (nkn))
    allocate(H2S_SAT                       (nkn))
    allocate(PE                            (nkn))
    allocate(DIP_OVER_IP                   (nkn))
    allocate(DISS_FE_II_CONC_TS_END        (nkn))
    allocate(DISS_FE_II_CONC_TS_AVG        (nkn))
    allocate(DISS_FE_III_CONC_TS_END       (nkn))
    allocate(DISS_FE_III_CONC_TS_AVG       (nkn))
    allocate(KG_ZOO_NOST_VEG_HET           (nkn))
    allocate(FOOD_FACTOR_ZOO_NOST_VEG_HET  (nkn))
    allocate(R_ZOO_FEEDING_NOST_VEG_HET    (nkn))
    allocate(PREF_DIP_DOP_NOST             (nkn))
    allocate(PREF_NH4N_NOST                (nkn))
    allocate(PREF_DIN_DON_NOST             (nkn))
    allocate(KG_NOST_VEG_HET               (nkn))
    allocate(LIM_KG_NOST_VEG_HET_LIGHT     (nkn))
    allocate(LIM_KG_NOST_VEG_HET_TEMP      (nkn))
    allocate(LIM_KG_NOST_VEG_HET_DOXY      (nkn))
    allocate(LIM_KG_NOST_VEG_HET_N         (nkn))
    allocate(LIM_KG_NOST_VEG_HET_P         (nkn))
    allocate(LIM_KG_NOST_VEG_HET_FIX       (nkn))
    allocate(LIM_KG_NOST_VEG_HET_NON_FIX   (nkn))
    allocate(R_NOST_VEG_HET_GROWTH         (nkn))
    allocate(R_NOST_VEG_HET_FIX_GROWTH     (nkn))
    allocate(R_NOST_VEG_HET_NON_FIX_GROWTH (nkn))
    allocate(R_NOST_VEG_HET_MET            (nkn))
    allocate(R_NOST_VEG_HET_RESP           (nkn))
    allocate(R_NOST_VEG_HET_EXCR           (nkn))
    allocate(R_NOST_VEG_HET_INT_RESP       (nkn))
    allocate(KD_NOST_VEG_HET               (nkn))
    allocate(FAC_HYPOX_NOST_VEG_HET_D      (nkn))
    allocate(R_NOST_VEG_HET_DEATH          (nkn))
    allocate(R_DENS_MORT_NOST_VEG_HET      (nkn))
    allocate(R_GERM_NOST_AKI               (nkn))
    allocate(R_FORM_NOST_AKI               (nkn))
    allocate(R_LOSS_AKI                    (nkn))
    allocate(R_MORT_AKI                    (nkn))
    allocate(R_NOST_VEG_HET_TOT_RESP       (nkn))
    allocate(N_NOST_VEG_HET_TOT_RESP       (nkn))
    allocate(P_FIX_CYN_GROWTH              (nkn))
    allocate(P_NOST_VEG_HET_GROWTH         (nkn))
    allocate(P_NOST_VEG_HET_TOT_RESP       (nkn))

    allocate(VALUE_strange                 (nkn))

    if (.not.allocated(GROWTH_INHIB_FACTOR_DIA)) then
        allocate(GROWTH_INHIB_FACTOR_DIA(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_DIA).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_DIA)
            allocate(GROWTH_INHIB_FACTOR_DIA(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_CYN)) then
        allocate(GROWTH_INHIB_FACTOR_CYN(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_CYN).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_CYN)
            allocate(GROWTH_INHIB_FACTOR_CYN(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_FIX_CYN)) then
        allocate(GROWTH_INHIB_FACTOR_FIX_CYN(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_FIX_CYN).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_FIX_CYN)
            allocate(GROWTH_INHIB_FACTOR_FIX_CYN(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_OPA)) then
        allocate(GROWTH_INHIB_FACTOR_OPA(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_OPA).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_OPA)
            allocate(GROWTH_INHIB_FACTOR_OPA(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_NOST)) then
        allocate(GROWTH_INHIB_FACTOR_NOST(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_NOST).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_NOST)
            allocate(GROWTH_INHIB_FACTOR_NOST(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_ZOO)) then
        allocate(GROWTH_INHIB_FACTOR_ZOO(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_ZOO).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_ZOO)
            allocate(GROWTH_INHIB_FACTOR_ZOO(nkn))
        end if
    end if

    GROWTH_INHIB_FACTOR_DIA    (:) = 1.0D0
    GROWTH_INHIB_FACTOR_CYN    (:) = 1.0D0
    GROWTH_INHIB_FACTOR_FIX_CYN(:) = 1.0D0
    GROWTH_INHIB_FACTOR_NOST   (:) = 1.0D0
    GROWTH_INHIB_FACTOR_OPA    (:) = 1.0D0
    GROWTH_INHIB_FACTOR_ZOO    (:) = 1.0D0

    ! CO2SYS Allocations
    allocate(CO2SYS_PAR1         (nkn))
    allocate(CO2SYS_PAR2         (nkn))
    allocate(CO2SYS_PAR1TYPE     (nkn))
    allocate(CO2SYS_PAR2TYPE     (nkn))
    allocate(CO2SYS_SALT         (nkn))
    allocate(CO2SYS_TEMPIN       (nkn))
    allocate(CO2SYS_TEMPOUT      (nkn))
    allocate(CO2SYS_PRESIN       (nkn))
    allocate(CO2SYS_PRESOUT      (nkn))
    allocate(CO2SYS_SI           (nkn))
    allocate(CO2SYS_PO4          (nkn))
    allocate(CO2SYS_pHSCALEIN    (nkn))
    allocate(CO2SYS_K1K2CONSTANTS(nkn))
    allocate(CO2SYS_KSO4CONSTANTS(nkn))
    allocate(CO2SYS_OUT_DATA     (nkn, 100)) ! Assuming 100 parameters as per CO2SYS output size
    allocate(CO2SYS_NICEHEADERS  (100))

end subroutine ALLOC_AQUABC_PELAGIC_INTERNAL


subroutine DEALLOC_AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_INTERNAL
    implicit none

    !State variables
    deallocate(NH4_N                         )
    deallocate(NO3_N                         )
    deallocate(PO4_P                         )
    deallocate(DISS_OXYGEN                   )
    deallocate(DIA_C                         )
    deallocate(ZOO_C                         )
    deallocate(ZOO_N                         )
    deallocate(ZOO_P                         )
    deallocate(DET_PART_ORG_C                )
    deallocate(DET_PART_ORG_N                )
    deallocate(DET_PART_ORG_P                )
    deallocate(DISS_ORG_C                    )
    deallocate(DISS_ORG_N                    )
    deallocate(DISS_ORG_P                    )
    deallocate(CYN_C                         )
    deallocate(OPA_C                         )
    deallocate(DISS_Si                       )
    deallocate(PART_Si                       )
    deallocate(FIX_CYN_C                     )
    deallocate(INORG_C                       )
    deallocate(TOT_ALK                       )
    deallocate(FE_II                         )
    deallocate(FE_III                        )
    deallocate(MN_II                         )
    deallocate(MN_IV                         )
    deallocate(CA                            )
    deallocate(MG                            )
    deallocate(S_PLUS_6                      )
    deallocate(S_MINUS_2                     )
    deallocate(CH4_C                         )
    deallocate(NOST_VEG_HET_C                )
    deallocate(NOST_AKI_C                    )

    deallocate(PHYT_TOT_C                    )

    !Driving functions
    deallocate(TEMP                          )
    deallocate(FDAY                          )
    deallocate(DEPTH                         )
    deallocate(I_A                           )
    deallocate(SALT                          )
    deallocate(ELEVATION                     )
    deallocate(AIRTEMP                       )
    deallocate(WINDS                         )
    deallocate(K_B_E                         )
    deallocate(ice_cover                     )
	deallocate(K_A_CALC                      )   !calculated aeration reactLr specific coefficient

    !Main process rates
    deallocate(R_FE_II_OXIDATION             )
    deallocate(R_FE_III_REDUCTION            )
    deallocate(R_MN_II_OXIDATION             )
    deallocate(R_MN_IV_REDUCTION             )
    deallocate(R_ABIOTIC_DOC_MIN_DOXY        )
    deallocate(R_ABIOTIC_DOC_MIN_NO3N        )
    deallocate(R_ABIOTIC_DOC_MIN_MN_IV       )
    deallocate(R_ABIOTIC_DOC_MIN_FE_III      )
    deallocate(R_ABIOTIC_DOC_MIN_S_PLUS_6    )
    deallocate(R_ABIOTIC_DOC_MIN_DOC         )
    deallocate(R_ABIOTIC_DON_MIN_DOXY        )
    deallocate(R_ABIOTIC_DON_MIN_NO3N        )
    deallocate(R_ABIOTIC_DON_MIN_MN_IV       )
    deallocate(R_ABIOTIC_DON_MIN_FE_III      )
    deallocate(R_ABIOTIC_DON_MIN_S_PLUS_6    )
    deallocate(R_ABIOTIC_DON_MIN_DOC         )
    deallocate(R_ABIOTIC_DOP_MIN_DOXY        )
    deallocate(R_ABIOTIC_DOP_MIN_NO3N        )
    deallocate(R_ABIOTIC_DOP_MIN_MN_IV       )
    deallocate(R_ABIOTIC_DOP_MIN_FE_III      )
    deallocate(R_ABIOTIC_DOP_MIN_S_PLUS_6    )
    deallocate(R_ABIOTIC_DOP_MIN_DOC         )
    deallocate(R_SULPHATE_REDUCTION          )
    deallocate(R_SULPHIDE_OXIDATION          )
    deallocate(H2S_ATM_EXCHANGE              )
    deallocate(R_METHANOGENESIS              )
    deallocate(R_METHANE_OXIDATION           )
    deallocate(CH4_ATM_EXCHANGE              )
    deallocate(R_AERATION                    )
    deallocate(R_DIA_GROWTH                  )
    deallocate(R_DIA_MET                     )
    deallocate(R_DIA_RESP                    )
    deallocate(R_DIA_EXCR                    )
    deallocate(R_DIA_INT_RESP                )
    deallocate(R_DIA_TOT_RESP                )
    deallocate(R_DIA_DEATH                   )
    deallocate(R_CYN_GROWTH                  )
    deallocate(R_CYN_RESP                    )
    deallocate(R_CYN_MET                     )
    deallocate(R_CYN_EXCR                    )
    deallocate(R_CYN_INT_RESP                )
    deallocate(R_CYN_TOT_RESP                )
    deallocate(R_CYN_DEATH                   )
    deallocate(R_FIX_CYN_GROWTH              )
    deallocate(R_FIX_CYN_RESP                )
    deallocate(R_FIX_CYN_MET                 )
    deallocate(R_FIX_CYN_EXCR                )
    deallocate(R_FIX_CYN_INT_RESP            )
    deallocate(R_FIX_CYN_TOT_RESP            )
    deallocate(R_FIX_CYN_DEATH               )
    deallocate(R_OPA_GROWTH                  )
    deallocate(R_OPA_RESP                    )
    deallocate(R_OPA_MET                     )
    deallocate(R_OPA_EXCR                    )
    deallocate(R_OPA_INT_RESP                )
    deallocate(R_OPA_TOT_RESP                )
    deallocate(R_OPA_DEATH                   )
    deallocate(R_ZOO_GROWTH                  )
    deallocate(R_ZOO_FEEDING_DIA             )
    deallocate(R_ZOO_FEEDING_CYN             )
    deallocate(R_ZOO_FEEDING_OPA             )
    deallocate(R_ZOO_FEEDING_FIX_CYN         )
    deallocate(R_ZOO_FEEDING_DET_PART_ORG_C  )
    deallocate(R_ZOO_RESP                    )
    deallocate(R_ZOO_INT_RESP                )
    deallocate(R_ZOO_TOT_RESP                )
    deallocate(R_ZOO_DEATH                   )
    deallocate(R_ZOO_EX_DON                  )
    deallocate(R_ZOO_EX_DOP                  )
    deallocate(R_ZOO_EX_DOC                  )
    deallocate(R_DET_PART_ORG_C_DISSOLUTION  )
    deallocate(LIM_PHYT_DISS_DET_PART_ORG_C  )
    deallocate(LIM_N_MIN_DON_N               )
    deallocate(LIM_P_MIN_DOP_P               )
    deallocate(R_PART_SI_DISS                )
    deallocate(R_NON_FIX_CYN_GROWTH          )
    deallocate(R_FIX_FIX_CYN_GROWTH          )
    deallocate(R_ABIOTIC_DOC_MIN             )
    deallocate(R_ABIOTIC_DON_MIN             )
    deallocate(R_ABIOTIC_DOP_MIN             )
    deallocate(LIM_PHYT_AMIN_DOC             )
    deallocate(LIM_N_AMIN_DON                )
    deallocate(LIM_PHY_N_AMIN_DON            )
    deallocate(LIM_DON_DON                   )
    deallocate(LIM_P_AMIN_DOP                )
    deallocate(LIM_PHY_P_AMIN_DOP            )
    deallocate(LIM_DISS_ORG_P                )
    deallocate(R_ABIOTIC_NITR                )
    deallocate(LIM_NITR_OXY                  )
    deallocate(LIM_NITR_NH4_N                )
    deallocate(R_AMMONIA_VOLATIL             )

    !Derived process rates
    deallocate(R_DENITRIFICATION             )
    deallocate(R_DET_PART_ORG_N_DISSOLUTION  )
    deallocate(LIM_N_DISS_DET_PART_ORG_N     )
    deallocate(LIM_PHY_N_DISS_DET_PART_ORG_N )
    deallocate(R_DET_PART_ORG_P_DISSOLUTION  )
    deallocate(LIM_P_DISS_DET_PART_ORG_P     )
    deallocate(LIM_PHY_P_DISS_DET_PART_ORG_P )

    !Auxillary variables
    deallocate(DISS_OXYGEN_SAT               )
    deallocate(CHLA                          )
    deallocate(K_E                           )
    deallocate(KG_DIA                        )
    deallocate(LIM_KG_DIA                    )
    deallocate(FAC_HYPOX_DIA_D               )
    deallocate(PREF_NH4N_DIA                 )
    deallocate(KD_DIA                        )
    deallocate(KG_ZOO                        )
    deallocate(KG_ZOO_DIA                    )
    deallocate(KG_ZOO_DET_PART_ORG_C         )
    deallocate(FOOD_AVAIL_ZOO                )
    deallocate(FOOD_FACTOR_ZOO_DIA           )
    deallocate(FOOD_FACTOR_ZOO_DET_PART_ORG_C)
    deallocate(ACTUAL_ZOO_N_TO_C             )
    deallocate(ACTUAL_ZOO_P_TO_C             )
    deallocate(FAC_HYPOX_ZOO_D               )
    deallocate(ACTUAL_DET_N_TO_C             )
    deallocate(ACTUAL_DET_P_TO_C             )
    deallocate(KD_ZOO                        )
    deallocate(KD_FIX_CYN                    )
    deallocate(FAC_HYPOX_FIX_CYN_D           )
    deallocate(PREF_NH4N_DON_FIX_CYN         )
    deallocate(KG_ZOO_FIX_CYN                )
    deallocate(FOOD_FACTOR_ZOO_FIX_CYN       )
    deallocate(LIM_KG_DIA_DOXY               )
    deallocate(LIM_KG_DIA_NUTR               )
    deallocate(LIM_KG_DIA_LIGHT              )
    deallocate(DIA_LIGHT_SAT                 )
    deallocate(CYN_LIGHT_SAT                 )
    deallocate(FIX_CYN_LIGHT_SAT             )
    deallocate(OPA_LIGHT_SAT                 )
    deallocate(NOST_LIGHT_SAT                )
    deallocate(LIM_KG_CYN_DOXY               )
    deallocate(LIM_KG_CYN_NUTR               )
    deallocate(LIM_KG_CYN_LIGHT              )
    deallocate(LIM_KG_CYN                    )
    deallocate(KD_CYN                        )
    deallocate(FAC_HYPOX_CYN_D               )
    deallocate(PREF_NH4N_DON_CYN             )       ! old still in used by others CYNs fixme
    deallocate(PREF_DIN_DON_CYN              )       ! New used only by buoyant CYNs fixme
    deallocate(PREF_NH4N_CYN                 )       ! New used only by buoyant CYNs fixme
    deallocate(KG_OPA                        )
    deallocate(KG_CYN                        )
    deallocate(LIM_KG_OPA_DOXY               )
    deallocate(LIM_KG_OPA_NUTR               )
    deallocate(LIM_KG_OPA_LIGHT              )
    deallocate(LIM_KG_OPA                    )
    deallocate(KD_OPA                        )
    deallocate(FAC_HYPOX_OPA_D               )
    deallocate(PREF_NH4N_OPA                 )
    deallocate(KG_ZOO_CYN                    )
    deallocate(KG_ZOO_OPA                    )
    deallocate(FOOD_FACTOR_ZOO_CYN           )
    deallocate(FOOD_FACTOR_ZOO_OPA           )

    deallocate(LIM_KG_DIA_TEMP               )
    deallocate(LIM_KG_DIA_N                  )
    deallocate(LIM_KG_DIA_P                  )
    deallocate(LIM_KG_DIA_DISS_Si            )
    deallocate(LIM_KG_CYN_TEMP               )
    deallocate(LIM_KG_CYN_N                  )
    deallocate(LIM_KG_CYN_P                  )
    deallocate(LIM_KG_OPA_TEMP               )
    deallocate(LIM_KG_OPA_N                  )
    deallocate(LIM_KG_OPA_P                  )
    deallocate(KG_FIX_CYN                    )
    deallocate(LIM_KG_FIX_CYN_TEMP           )
    deallocate(LIM_KG_FIX_CYN_DOXY           )
    deallocate(LIM_KG_FIX_CYN_LIGHT          )
    deallocate(LIM_KG_NON_FIX_CYN_N          )
    deallocate(LIM_KG_NON_FIX_CYN_P          )
    deallocate(LIM_KG_NON_FIX_CYN_NUTR       )
    deallocate(LIM_KG_NON_FIX_CYN            )
    deallocate(LIM_KG_FIX_FIX_CYN_N          )
    deallocate(LIM_KG_FIX_FIX_CYN_P          )
    deallocate(LIM_KG_FIX_FIX_CYN_NUTR       )
    deallocate(LIM_KG_FIX_FIX_CYN            )
    deallocate(ALPHA_0                       )
    deallocate(ALPHA_1                       )
    deallocate(TOTAL_DIC_KINETIC_SOURCES     )
    deallocate(TOTAL_DIC_KINETIC_SINKS       )
    deallocate(T_A                           )
    deallocate(P_K_H                         )
    deallocate(K_H                           )
    deallocate(POWER                         )
    deallocate(P_CO2                         )
    deallocate(CO2_SAT                       )
    deallocate(K_A_CALC_CO2                  )
    deallocate(CO2_ATM_EXHANGE               )
    deallocate(DIC_KINETIC_DERIVATIVE        )
    deallocate(H2CO3                         )
    deallocate(HCO3                          )
    deallocate(CO3                           )
    deallocate(PKH                           )
    deallocate(FRAC_NH3                      )
    deallocate(FRAC_NH4                      )
    deallocate(N_DIA_TOT_RESP                )
    deallocate(N_CYN_TOT_RESP                )
    deallocate(N_OPA_TOT_RESP                )
    deallocate(N_FIX_CYN_TOT_RESP            )
    deallocate(N_ZOO_TOT_RESP                )
    deallocate(N_ABIOTIC_DON_MIN             )
    deallocate(ALK_GAINED_BY_AMMONIUM_GEN    )
    deallocate(N_DENITRIFICATION             )
    deallocate(N_DIA_GROWTH                  )
    deallocate(N_CYN_GROWTH                  )
    deallocate(N_OPA_GROWTH                  )
    deallocate(N_NON_FIX_CYN_GROWTH          )
    deallocate(ALK_GAINED_BY_NITRATE_CONS    )
    deallocate(ALK_LOST_BY_AMMONIUM_CONS     )
    deallocate(N_NITRIFICATION_NH4           )
    deallocate(N_NITRIFICATION_NH3           )
    deallocate(ALK_LOST_BY_NITRIFICATION     )
    deallocate(H_PLUS                        )
    deallocate(N_NOST_VEG_HET_NH4_RELEASE    )
    deallocate(NO3N_NOST_VEG_HET_GROWTH      )
    deallocate(NH4N_NOST_VEG_HET_GROWTH      )
    deallocate(NH4N_SEDIMENT_INFLUX          )
    deallocate(NH4N_SEDIMENT_OUTFLUX         )
    deallocate(NO3N_SEDIMENT_INFLUX          )
    deallocate(NO3N_SEDIMENT_OUTFLUX         )
    deallocate(PO4P_SEDIMENT_INFLUX          )
    deallocate(PO4P_SEDIMENT_OUTFLUX         )
    deallocate(K_ONE_TIP                     )
    deallocate(K_TWO_TIP                     )
    deallocate(K_THREE_TIP                   )
    deallocate(FRACTION_DIVISOR_TIP          )
    deallocate(ALPHA_H2PO4                   )
    deallocate(ALPHA_HPO4                    )
    deallocate(ALPHA_PO4                     )
    deallocate(PHOSPHATE_EQ_CONSTANT         )
    deallocate(ALK_GAINED_BY_PHOSPHATE_CONS  )
    deallocate(ALK_LOST_BY_PHOSPHATE_GEN     )
    deallocate(ALK_KINETIC_DERIVATIVE        )
    deallocate(P_DIA_TOT_RESP                )
    deallocate(P_CYN_TOT_RESP                )
    deallocate(P_OPA_TOT_RESP                )
    deallocate(P_FIX_CYN_TOT_RESP            )
    deallocate(P_ZOO_TOT_RESP                )
    deallocate(P_ABIOTIC_DOP_MIN             )
    deallocate(P_DIA_GROWTH                  )
    deallocate(P_CYN_GROWTH                  )
    deallocate(P_OPA_GROWTH                  )
    deallocate(P_NON_FIX_CYN_GROWTH          )
    deallocate(PH_CORR_NITR_NH4              )
    deallocate(PH_CORR_DENITR_NO3            )
    deallocate(MULT_FE_II_DISS               )   !Dissolved fraction of FE_II
    deallocate(MULT_FE_II_PART               )   !Particulate fraction of FE_II
    deallocate(MULT_FE_III_DISS              )   !Dissolved fraction of FE_III
    deallocate(MULT_FE_III_PART              )   !Particulate fraction of FE_III
    deallocate(MULT_MN_II_DISS               )   !Dissolved fraction of MN_II
    deallocate(MULT_MN_II_PART               )   !Particulate fraction of MN_II
    deallocate(MULT_MN_IV_DISS               )   !Dissolved fraction of MN_IV
    deallocate(MULT_MN_IV_PART               )   !Particulate fraction of MN_IV
    deallocate(K_EQ_S_1                      )
    deallocate(K_EQ_S_2                      )
    deallocate(K_SP_FES                      )
    deallocate(HS2_TOT                       )
    deallocate(H2S_DIVISOR                   )
    deallocate(FRAC_H2S_IN_H2S_TOT           )
    deallocate(FRAC_HS_MINUS_IN_H2S_TOT      )
    deallocate(FRAC_S_MINUS_TWO_IN_H2S_TOT   )
    deallocate(H2S                           )
    deallocate(HS_MINUS                      )
    deallocate(S_MINUS_TWO                   )
    deallocate(FE_II_DISS                    )
    deallocate(FE_II_PART                    )
    deallocate(FE_III_DISS                   )
    deallocate(FE_III_PART                   )
    deallocate(MN_II_DISS                    )
    deallocate(MN_II_PART                    )
    deallocate(FE_II_DISS_EQ                 )
    deallocate(FE_III_DISS_EQ                )
    deallocate(PH_CORR_DOC_MIN_DOXY          )
    deallocate(PH_CORR_DOC_MIN_NO3N          )
    deallocate(PH_CORR_DOC_MIN_MN_IV         )
    deallocate(PH_CORR_DOC_MIN_FE_III        )
    deallocate(PH_CORR_DOC_MIN_S_PLUS_6      )
    deallocate(PH_CORR_DOC_MIN_DOC           )
    deallocate(LIM_DOXY_RED                  )
    deallocate(LIM_NO3N_RED                  )
    deallocate(LIM_MN_IV_RED                 )
    deallocate(LIM_FE_III_RED                )
    deallocate(LIM_S_PLUS_6_RED              )
    deallocate(LIM_DOC_RED                   )
    deallocate(K_NO3_RED                     )
    deallocate(K_MN_IV_RED                   )
    deallocate(K_FE_III_RED                  )
    deallocate(K_S_PLUS_6_RED                )
    deallocate(K_DOC_RED                     )
    deallocate(MN_IV_DISS                    )
    deallocate(PH_CORR_DON_MIN_DOXY          )
    deallocate(PH_CORR_DON_MIN_NO3N          )
    deallocate(PH_CORR_DON_MIN_MN_IV         )
    deallocate(PH_CORR_DON_MIN_FE_III        )
    deallocate(PH_CORR_DON_MIN_S_PLUS_6      )
    deallocate(PH_CORR_DON_MIN_DOC           )
    deallocate(PH_CORR_DOP_MIN_DOXY          )
    deallocate(PH_CORR_DOP_MIN_NO3N          )
    deallocate(PH_CORR_DOP_MIN_MN_IV         )
    deallocate(PH_CORR_DOP_MIN_FE_III        )
    deallocate(PH_CORR_DOP_MIN_S_PLUS_6      )
    deallocate(PH_CORR_DOP_MIN_DOC           )
    deallocate(K_A_CH4                       )
    deallocate(K_A_H2S                       )
    deallocate(CH4_SAT                       )
    deallocate(H2S_SAT                       )
    deallocate(PE                            )
    deallocate(DIP_OVER_IP                   )
    deallocate(DISS_FE_II_CONC_TS_END        )
    deallocate(DISS_FE_II_CONC_TS_AVG        )
    deallocate(DISS_FE_III_CONC_TS_END       )
    deallocate(DISS_FE_III_CONC_TS_AVG       )
    deallocate(KG_ZOO_NOST_VEG_HET           )
    deallocate(FOOD_FACTOR_ZOO_NOST_VEG_HET  )
    deallocate(R_ZOO_FEEDING_NOST_VEG_HET    )
    deallocate(PREF_DIP_DOP_NOST             )
    deallocate(PREF_NH4N_NOST                )
    deallocate(PREF_DIN_DON_NOST             )
    deallocate(KG_NOST_VEG_HET               )
    deallocate(LIM_KG_NOST_VEG_HET_LIGHT     )
    deallocate(LIM_KG_NOST_VEG_HET_TEMP      )
    deallocate(LIM_KG_NOST_VEG_HET_DOXY      )
    deallocate(LIM_KG_NOST_VEG_HET_N         )
    deallocate(LIM_KG_NOST_VEG_HET_P         )
    deallocate(LIM_KG_NOST_VEG_HET_FIX       )
    deallocate(LIM_KG_NOST_VEG_HET_NON_FIX   )
    deallocate(R_NOST_VEG_HET_GROWTH         )
    deallocate(R_NOST_VEG_HET_FIX_GROWTH     )
    deallocate(R_NOST_VEG_HET_NON_FIX_GROWTH )
    deallocate(R_NOST_VEG_HET_MET            )
    deallocate(R_NOST_VEG_HET_RESP           )
    deallocate(R_NOST_VEG_HET_EXCR           )
    deallocate(R_NOST_VEG_HET_INT_RESP       )
    deallocate(KD_NOST_VEG_HET               )
    deallocate(FAC_HYPOX_NOST_VEG_HET_D      )
    deallocate(R_NOST_VEG_HET_DEATH          )
    deallocate(R_DENS_MORT_NOST_VEG_HET      )
    deallocate(R_GERM_NOST_AKI               )
    deallocate(R_FORM_NOST_AKI               )
    deallocate(R_LOSS_AKI                    )
    deallocate(R_MORT_AKI                    )
    deallocate(R_NOST_VEG_HET_TOT_RESP       )
    deallocate(N_NOST_VEG_HET_TOT_RESP       )
    deallocate(P_FIX_CYN_GROWTH              )
    deallocate(P_NOST_VEG_HET_GROWTH         )
    deallocate(P_NOST_VEG_HET_TOT_RESP       )

    deallocate(VALUE_strange                 )

    deallocate(GROWTH_INHIB_FACTOR_DIA       )
    deallocate(GROWTH_INHIB_FACTOR_CYN       )
    deallocate(GROWTH_INHIB_FACTOR_FIX_CYN   )
    deallocate(GROWTH_INHIB_FACTOR_NOST      )
    deallocate(GROWTH_INHIB_FACTOR_OPA       )
    deallocate(GROWTH_INHIB_FACTOR_ZOO       )

    ! CO2SYS Deallocations
    if (allocated(CO2SYS_PAR1))          deallocate(CO2SYS_PAR1)
    if (allocated(CO2SYS_PAR2))          deallocate(CO2SYS_PAR2)
    if (allocated(CO2SYS_PAR1TYPE))      deallocate(CO2SYS_PAR1TYPE)
    if (allocated(CO2SYS_PAR2TYPE))      deallocate(CO2SYS_PAR2TYPE)
    if (allocated(CO2SYS_SALT))          deallocate(CO2SYS_SALT)
    if (allocated(CO2SYS_TEMPIN))        deallocate(CO2SYS_TEMPIN)
    if (allocated(CO2SYS_TEMPOUT))       deallocate(CO2SYS_TEMPOUT)
    if (allocated(CO2SYS_PRESIN))        deallocate(CO2SYS_PRESIN)
    if (allocated(CO2SYS_PRESOUT))       deallocate(CO2SYS_PRESOUT)
    if (allocated(CO2SYS_SI))            deallocate(CO2SYS_SI)
    if (allocated(CO2SYS_PO4))           deallocate(CO2SYS_PO4)
    if (allocated(CO2SYS_pHSCALEIN))     deallocate(CO2SYS_pHSCALEIN)
    if (allocated(CO2SYS_K1K2CONSTANTS)) deallocate(CO2SYS_K1K2CONSTANTS)
    if (allocated(CO2SYS_KSO4CONSTANTS)) deallocate(CO2SYS_KSO4CONSTANTS)
    if (allocated(CO2SYS_OUT_DATA))      deallocate(CO2SYS_OUT_DATA)
    if (allocated(CO2SYS_NICEHEADERS))   deallocate(CO2SYS_NICEHEADERS)

end subroutine DEALLOC_AQUABC_PELAGIC_INTERNAL
