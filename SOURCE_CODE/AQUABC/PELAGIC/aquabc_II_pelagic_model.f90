! Pelagic kinetic model ALUKAS_II
! Version with variables calculated in subroutines
! Version with dissolved inorganic carbon and alkalinity as state variables.

!Contains:
! subroutine PELAGIC_KINETICS


!******************************************************************
!******************************************************************
!******************************************************************

!******************************************************************
! 30th of Novemver 2015
!
! Initial additions for interfacing dissolved and particulate
! fractions of FE_II, FE_IV, MN_II, MN_IV
!
!******************************************************************

!******************************************************************
!
! Change by Ali Erturk 25th of November 2016
!
!          Initial version of balance beased aquatic chemistry model
!          just for iron
!
! Change by Ali Erturk and Petras Zemlys 27th of November 2016
!
!         - Redox sequence
!
!         - New state variables added
!
!              - CA        (Calcium)
!              - MG        (Magnesium)
!              - S_PLUS_6  (Sulphate sulphur)
!              - S_MINUS_2 (Sulphide sulphur)
!              - CH4_C      (Methane carbon)
!
! Changes by Ali Erturk for KUMADUBI 3 rd of July 2016
!
! 9 September 2018
! Changes by Ali Erturk related to clean the code, where all
! useless bacteria state variables and related processes are
! removed
!
!*******************************************************************


subroutine AQUABC_PELAGIC_KINETICS &
           (node_active                     , &
            nkn                             , &
            STATE_VARIABLES                 , &
            DERIVATIVES                     , &
            nstate                          , &
            MODEL_CONSTANTS                 , &
            nconst                          , &
            DRIVING_FUNCTIONS               , &
            n_driving_functions             , &
            FLAGS                           , &
            nflags                          , &
            PROCESS_RATES                   , &
            NDIAGVAR                        , &
            SAVED_OUTPUTS                   , &
            n_saved_outputs                 , &
            PH                              , &
            TIME                            , &
            TIME_STEP                       , &
            DAY_OF_YEAR                     , &
            SEDIMENT_FLUXES                 , &
            CALLED_BEFORE                   , &
            SURFACE_BOXES                   , &
            ZOOP_OPTION_1                   , &
            ADVANCED_REDOX_OPTION           , &
            USER_ENTERED_frac_avail_DON     , &
            LIGHT_EXTINCTION_OPTION         , &
            CYANO_BOUYANT_STATE_SIMULATION  , &
            CONSIDER_NON_OBLIGATORY_FIXERS  , &
            CONSIDER_NOSTOCALES)

    use CO2SYS_CDIAC
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_II_wc_ini
    use chla_diagnostics, only: CHLA_NEG_FLAG, CHLA_NEG_NODE, CHLA_NEG_VALUE, CHLA_DUMP_DONE, &
         CLAMP_WARNED, CLAMP_COUNT, CLAMP_PROC_COUNT
    use AQUABC_PEL_STATE_VAR_INDEXES
    use AQUABC_PELAGIC_INTERNAL
    use GLOBAL, only: NSTATE_CHECK => nstate, NCONST_CHECK => nconst, &
                      NDRIV_CHECK => n_driving_functions, NFLAGS_CHECK => nflags
    use AQUABC_PELAGIC_TYPES
    use AQUABC_PHYSICAL_CONSTANTS, only: CELSIUS_TO_KELVIN, &
         FE_MOLAR_MASS_MG, S_MOLAR_MASS_MG, MIN_CONCENTRATION
    !use basin, only: ipv ! array of external node numbers for debugging, should be commented when used in ESTAS

    implicit none


    ! -------------------------------------------------------------------------------------------------------------------------
    ! VARIABLES IN THE ARGUMENT LIST
    ! -------------------------------------------------------------------------------------------------------------------------
    integer                                                                  :: nkn
    integer                                                                  :: nstate
    integer                                                                  :: nconst
    integer                                                                  :: n_driving_functions
    integer                                                                  :: nflags
    integer                                                                  :: n_saved_outputs
    integer                                                                  :: NDIAGVAR

    integer                                                                  :: error

    !internal numbers (specific for this routine) of nodes, used for diagnostics
    integer              , dimension(nkn)                                    :: node_active
    integer                                                                      :: dump_node
    real(kind = DBL_PREC)                                                       :: total_removal, allowed_rate, scale
    real(kind = DBL_PREC)                                                       :: allowed_rate_local, old_rate, sum_removals


    real(kind = DBL_PREC), dimension(nkn,nstate),              intent(in)    :: STATE_VARIABLES
    real(kind = DBL_PREC), dimension(nkn,nstate),              intent(out)   :: DERIVATIVES
    real(kind = DBL_PREC), dimension(nconst),                  intent(in)    :: MODEL_CONSTANTS
    real(kind = DBL_PREC), dimension(nkn,n_driving_functions), intent(in)    :: DRIVING_FUNCTIONS
    integer,               dimension(nflags),                  intent(in)    :: FLAGS
    real(kind = DBL_PREC), dimension(nkn,nstate, NDIAGVAR),    intent(out)   :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn) :: PH
    real(kind = DBL_PREC), dimension(nkn) :: chla_pos

    !For saving some variables to be used for all nodes?
    real(kind = DBL_PREC), dimension(nkn,n_saved_outputs),     intent(inout) :: SAVED_OUTPUTS
    real(kind = DBL_PREC),                                     intent(in)    :: TIME
    real(kind = DBL_PREC),                                     intent(in)    :: TIME_STEP
    integer,                                                   intent(in)    :: DAY_OF_YEAR
    real(kind = DBL_PREC), dimension(nkn, nstate),             intent(in)    :: SEDIMENT_FLUXES
    integer,                                                   intent(inout) :: CALLED_BEFORE


    integer, dimension(nkn),                          intent(in)    :: SURFACE_BOXES

    integer                ,                          intent(in)    :: ZOOP_OPTION_1
    integer                ,                          intent(in)    :: ADVANCED_REDOX_OPTION
    real(kind = DBL_PREC)  ,                          intent(in)    :: USER_ENTERED_frac_avail_DON
    integer                ,                          intent(in)    :: LIGHT_EXTINCTION_OPTION
    integer                ,                          intent(in)    :: CYANO_BOUYANT_STATE_SIMULATION
    integer                ,                          intent(in)    :: CONSIDER_NON_OBLIGATORY_FIXERS
    integer                ,                          intent(in)    :: CONSIDER_NOSTOCALES
    ! -------------------------------------------------------------------------------------------------------------------------
    ! END OF VARIABLES IN THE ARGUMENT LIST
    ! -------------------------------------------------------------------------------------------------------------------------

    integer :: i,k
    !*********************************************'
    !*                                           *'
    !* PELAGIC ECOLOGY KINETICS
    !*                                           *'
    !*********************************************'

    !FUNCTIONS
    real(kind = DBL_PREC) :: DO_SATURATION
    real(kind = DBL_PREC) :: KAWIND

    integer :: CO2SYS_NUM_SAMPLES
    integer :: RUN_CO2SYS
    parameter (RUN_CO2SYS = 1)
    integer :: CO2SYS_ntps     ! number of nodes

    integer :: CONSIDER_ALKALNITY_DERIVATIVE
    integer :: CONSIDER_INORG_C_DERIVATIVE
    integer :: CONSIDER_CO2_REARATION

    ! Derived-type parameter bundles (populated once in CALLED_BEFORE block)
    type(t_diatom_params),   save :: DIA_PARAMS
    type(t_cyn_params),      save :: CYN_PARAMS
    type(t_fix_cyn_params),  save :: FIX_CYN_PARAMS
    type(t_opa_params),      save :: OPA_PARAMS
    type(t_nost_params),     save :: NOST_PARAMS
    type(t_zoo_params),      save :: ZOO_PARAMS

    !If called first time
    if (CALLED_BEFORE < 1) then
        !Allocate the arrays in the module AQUABC_PELAGIC_INTERNAL
        call ALLOC_AQUABC_PELAGIC_INTERNAL(nkn)

        ! Populate organism parameter types from module-level constants
        call populate_diatom_params(DIA_PARAMS)
        call populate_cyn_params(CYN_PARAMS)
        call populate_fix_cyn_params(FIX_CYN_PARAMS)
        call populate_opa_params(OPA_PARAMS)
        call populate_nost_params(NOST_PARAMS)
        call populate_zoo_params(ZOO_PARAMS)

        CALLED_BEFORE = 1
    end if

    ! Initialize one-shot per-node clamped-warning flags and counters (allocate once)
    if (.not. allocated(CLAMP_WARNED)) then
        allocate(CLAMP_WARNED(nkn))
        CLAMP_WARNED = .false.
    end if
    if (.not. allocated(CLAMP_COUNT)) then
        allocate(CLAMP_COUNT(nkn))
        CLAMP_COUNT = 0
    end if
    if (NDIAGVAR > 0) then
        if (.not. allocated(CLAMP_PROC_COUNT)) then
            allocate(CLAMP_PROC_COUNT(nkn, NDIAGVAR))
            CLAMP_PROC_COUNT = 0
        end if
    end if

    do i=1,nkn
        node_active(i) = i
    end do

    debug_stranger = .true. ! True if check for strange values

    CONSIDER_ALKALNITY_DERIVATIVE = 1
    CONSIDER_INORG_C_DERIVATIVE   = 1
    CONSIDER_CO2_REARATION        = 1

    ! indicator for light limitation  algorithm:
    ! 1 - modified Smith, 0 - Ali
    smith = 1

    ! indicator of iron oxidation formulation
    !iron_oxidation = 1  !complex, no calibration
    iron_oxidation = 0  !simple, with calibration parameter

    PROCESS_RATES(:,:,:) = 0.0D0
    DERIVATIVES  (:,:)   = 0.0D0
    error = 0

    DO_ADVANCED_REDOX_SIMULATION      = 0
    DO_NON_OBLIGATORY_FIXERS          = 0
    DO_NOSTOCALES                     = 1


    if (ADVANCED_REDOX_OPTION > 0) then
        DO_ADVANCED_REDOX_SIMULATION = 1
    else
        DO_ADVANCED_REDOX_SIMULATION = 0
    end if


    if (CONSIDER_NON_OBLIGATORY_FIXERS > 0) then
        DO_NON_OBLIGATORY_FIXERS = 1
    else
        DO_NON_OBLIGATORY_FIXERS = 0
    end if


    if (CONSIDER_NOSTOCALES > 0) then
        DO_NOSTOCALES = 1
    else
        DO_NOSTOCALES = 0
    end if

    if(debug_stranger) then
        call DBGSTR_PEL_GEN_01(STATE_VARIABLES, nkn, nstate, node_active, error)
    end if

    !INITIALIZE STATE VARIABLES
    if (nstate .ne. NSTATE_CHECK) then
        write(*,*) 'PELAGIC_KINETICS: Number of state variables is wrong : ', nstate
        write(*,*) 'Expected: ', NSTATE_CHECK
        stop
    end if

    NH4_N           (:)      = STATE_VARIABLES(:,NH4_N_INDEX         )     ! AMMONIUM NITROGEN
    NO3_N           (:)      = STATE_VARIABLES(:,NO3_N_INDEX         )     ! NITRATE NITROGEN
    PO4_P           (:)      = STATE_VARIABLES(:,PO4_P_INDEX         )     ! ORTHOPHOSPHATE PHOSPHORUS
    DISS_OXYGEN     (:)      = STATE_VARIABLES(:,DISS_OXYGEN_INDEX   )     ! DISSOLVED OXYGEN
    DIA_C           (:)      = STATE_VARIABLES(:,DIA_C_INDEX         )     ! DIATOMS CARBON
    ZOO_C           (:)      = STATE_VARIABLES(:,ZOO_C_INDEX         )     ! ZOOPLANKTON CARBON
    ZOO_N           (:)      = STATE_VARIABLES(:,ZOO_N_INDEX         )     ! ZOOPLANKTON NITROGEN
    ZOO_P           (:)      = STATE_VARIABLES(:,ZOO_P_INDEX         )     ! ZOOPLANKTON PHOSPHORUS
    DET_PART_ORG_C  (:)      = STATE_VARIABLES(:,DET_PART_ORG_C_INDEX)     ! DETRITUS PARTICULATE ORG. CARBON
    DET_PART_ORG_N  (:)      = STATE_VARIABLES(:,DET_PART_ORG_N_INDEX)     ! DETRITUS PARTICULATE ORG. NITROGEN
    DET_PART_ORG_P  (:)      = STATE_VARIABLES(:,DET_PART_ORG_P_INDEX)     ! DETRITUS PARTICULATE ORG. PHOSPHORUS
    DISS_ORG_C      (:)      = STATE_VARIABLES(:,DISS_ORG_C_INDEX    )     ! DISSOLVED ORGANIC CARBON
    DISS_ORG_N      (:)      = STATE_VARIABLES(:,DISS_ORG_N_INDEX    )     ! DISSOLVED ORGANIC NITROGEN
    DISS_ORG_P      (:)      = STATE_VARIABLES(:,DISS_ORG_P_INDEX    )     ! DISSOLVED ORGANIC PHOSPHORUS
    CYN_C           (:)      = STATE_VARIABLES(:,CYN_C_INDEX         )     ! NON FIXING CYANOBACTERIA CARBON
    OPA_C           (:)      = STATE_VARIABLES(:,OPA_C_INDEX         )     ! OTHER PHYTOPLANKTON CARBON
    DISS_Si         (:)      = STATE_VARIABLES(:,DISS_Si_INDEX       )     ! DISSOLOVED SILICA
    PART_Si         (:)      = STATE_VARIABLES(:,PART_Si_INDEX       )     ! PARTICULATE SILICA
    FIX_CYN_C       (:)      = STATE_VARIABLES(:,FIX_CYN_C_INDEX     )     ! FIXING CYANOBACTERIA CARBON
    INORG_C         (:)      = STATE_VARIABLES(:,INORG_C_INDEX       )     ! INORG CARBON CARBON
    TOT_ALK         (:)      = STATE_VARIABLES(:,TOT_ALK_INDEX       )     ! TOTAL ALKALNITY
    FE_II           (:)      = STATE_VARIABLES(:,FE_II_INDEX         )     ! Iron charged as plus 2
    FE_III          (:)      = STATE_VARIABLES(:,FE_III_INDEX        )     ! Iron chargen as plus 3
    MN_II           (:)      = STATE_VARIABLES(:,MN_II_INDEX         )     ! Manganese charged as plus 2
    MN_IV           (:)      = STATE_VARIABLES(:,MN_IV_INDEX         )     ! Manganese charged as plus 4
    CA              (:)      = STATE_VARIABLES(:,CA_INDEX            )     ! Total calcium
    MG              (:)      = STATE_VARIABLES(:,MG_INDEX            )     ! Total magensium
    S_PLUS_6        (:)      = STATE_VARIABLES(:,S_PLUS_6_INDEX      )     ! Sulphur charged as 6+ (SO4-S)
    S_MINUS_2       (:)      = STATE_VARIABLES(:,S_MINUS_2_INDEX     )     ! Sulphur charged as 2- (H2S-S)
    CH4_C           (:)      = STATE_VARIABLES(:,CH4_C_INDEX         )     ! Methane carbon
    NOST_VEG_HET_C  (:)      = STATE_VARIABLES(:,NOST_VEG_HET_C_INDEX)     ! Nostacales cells in vegatative state
    NOST_AKI_C      (:)      = STATE_VARIABLES(:,NOST_AKI_C_INDEX    )     ! Nostacales cells in akinete state

    !INITIALIZE DRIVING_FUNCTIONS
    if(n_driving_functions .ne. NDRIV_CHECK) then
       write(*,*) 'PELAGIC_KINETICS: Number of elements in DRIVING_FUNCTIONS is wrong', n_driving_functions
       write(*,*) 'Expected: ', NDRIV_CHECK
       stop
    end if

    TEMP     (1:nkn) = DRIVING_FUNCTIONS(1:nkn, 1)
    TEMP     (1:nkn) = max(0.0D0, min(45.0D0, TEMP(1:nkn)))
    SALT     (1:nkn) = DRIVING_FUNCTIONS(1:nkn, 2)
    SALT     (1:nkn) = max(0.0D0, SALT(1:nkn))

    ! Conversion from W/m^2 to langleys
    I_A      (1:nkn) =(DRIVING_FUNCTIONS(1:nkn, 3) * 5.0D-1 * 8.64D4 * 0.238846) / 1.0D4
    FDAY     (1:nkn) = DRIVING_FUNCTIONS(1:nkn, 4)
    AIRTEMP  (1:nkn) = DRIVING_FUNCTIONS(1:nkn, 5)
    WINDS    (1:nkn) = DRIVING_FUNCTIONS(1:nkn, 6)
    ELEVATION(1:nkn) = DRIVING_FUNCTIONS(1:nkn, 7)
    DEPTH    (1:nkn) = DRIVING_FUNCTIONS(1:nkn, 8)
    K_B_E    (1:nkn) = DRIVING_FUNCTIONS(1:nkn, 9)
    ice_cover(1:nkn) = DRIVING_FUNCTIONS(1:nkn,10)

    !INITIALIZE FLAGS
    if(nflags .ne. NFLAGS_CHECK) then
       write(*,*) 'PELAGIC_KINETICS: Number of elements in FLAGS is wrong', nflags
       write(*,*) 'Expected: ', NFLAGS_CHECK
       stop
    end if

    SAFE_MODE                  = FLAGS(1)
    SURFACE_BOX                = FLAGS(2)
    FIRST_TIME_STEP            = FLAGS(3)
    INIT_OPTION_OF_FE_II_DISS  = FLAGS(4)
    INIT_OPTION_OF_FE_III_DISS = FLAGS(5)

    !INITIALIZE MODEL CONSTANTS
    if (nconst .ne. NCONST_CHECK) then
        write(*,*) 'PELAGIC_KINETICS: Number of elements in MODEL_CONSTANTS is wrong', nconst
        write(*,*) 'Expected: ', NCONST_CHECK
        stop
    end if

    ! Calling CO2SYS
    CO2SYS_NUM_SAMPLES = nkn ! number of nodes
    CO2SYS_ntps = nkn ! correction of bug: just CO2SYS_NUM_SAMPLES is not passed to co2sys

    ! Allocation moved to AQUABC_PELAGIC_INTERNAL initialization for performance

    if (RUN_CO2SYS .eq. 1) then
        CO2SYS_PAR1         (1:nkn) = TOT_ALK(1:nkn) * 1.0D6
        CO2SYS_PAR2         (1:nkn) = INORG_C(1:nkn) * 1.0D6
        CO2SYS_PAR1TYPE     (1:nkn) = 1
        CO2SYS_PAR2TYPE     (1:nkn) = 2
        CO2SYS_SALT         (1:nkn) = SALT(1:nkn)
        CO2SYS_TEMPIN       (1:nkn) = TEMP(1:nkn)
        CO2SYS_TEMPOUT      (1:nkn) = 0.0D0  !Does not matter for this case
        CO2SYS_PRESIN       (1:nkn) = 0.0D0  !Does not matter for this case
        CO2SYS_PRESOUT      (1:nkn) = 0.0D0  !Does not matter for this case
        CO2SYS_SI           (1:nkn) = (Diss_SI(1:nkn) / 28.0855D0) * 1.0D3
        CO2SYS_PO4          (1:nkn) = (PO4_P  (1:nkn) / 30.9737D0) * 1.0D3
        CO2SYS_pHSCALEIN    (1:nkn) = 1
        CO2SYS_K1K2CONSTANTS(1:nkn) = 4 !CO2SYS_K1K2CONSTANTS(1:nkn) = 4
        CO2SYS_KSO4CONSTANTS(1:nkn) = 1

        call CO2SYS(CO2SYS_PAR1         , CO2SYS_PAR2         , CO2SYS_PAR1TYPE , &
                    CO2SYS_PAR2TYPE     , CO2SYS_SALT         , CO2SYS_TEMPIN   , &
                    CO2SYS_TEMPOUT      , CO2SYS_PRESIN       , CO2SYS_PRESOUT  , &
                    CO2SYS_SI           , CO2SYS_PO4          , CO2SYS_pHSCALEIN, &
                    CO2SYS_K1K2CONSTANTS, CO2SYS_KSO4CONSTANTS, CO2SYS_OUT_DATA , &
                    CO2SYS_NICEHEADERS  , &
                    CO2SYS_ntps)

        pH         (1:nkn) = CO2SYS_OUT_DATA(1:nkn, 18)
        K_ONE_TIP  (1:nkn) = CO2SYS_OUT_DATA(1:nkn, 75)
        K_TWO_TIP  (1:nkn) = CO2SYS_OUT_DATA(1:nkn, 76)
        K_THREE_TIP(1:nkn) = CO2SYS_OUT_DATA(1:nkn, 77)
        H2CO3      (1:nkn) = CO2SYS_OUT_DATA(1:nkn, 23)
        H_PLUS     (1:nkn) = 10.0D0 ** (-CO2SYS_OUT_DATA(1:nkn,18))
        HCO3       (1:nkn) = CO2SYS_OUT_DATA(1:nkn, 21)
        CO3        (1:nkn) = CO2SYS_OUT_DATA(1:nkn, 22)

        ! Deallocation handled by AQUABC_PELAGIC_INTERNAL cleanup
    end if ! call co2sys

    HCO3 = HCO3 / 1000000.0
    CO3  = CO3  / 1000000.0
    ! -----------------------------------------------------------------
    ! Additions made 30th November 2015 related to dissolved and solid
    ! iron and mangese species, soon to be replaced by the
    ! aquatic chemistry model that will be called
    !
    !
    !        EXACTLY HERE
    !
    ! -----------------------------------------------------------------


    ! -------------------------------------------------------------------------
    ! New equlibrium calculations added at 25th of January 2016
    ! This is the first version of the aquatic chemistry model that was
    ! promissed 30th November 2015
    ! -------------------------------------------------------------------------

        ! CO2SYS_OUT_DATA(:, 21), CO2SYS_OUT_DATA(:, 22)can not be used because deallocated

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then
            call REDOX_AND_SPECIATION &
            (DISS_OXYGEN, NO3_N, MN_IV, FE_III, S_PLUS_6, DISS_ORG_C, &
             S_MINUS_2 , MN_II, FE_II , HCO3, CO3, &
             TEMP, SALT, PH, ELEVATION, &
             K_HS_DOXY_RED_LIM   , K_HS_NO3N_RED_LIM , K_HS_MN_IV_RED_LIM , &
             K_HS_FE_III_RED_LIM , K_HS_S_PLUS_6_RED_LIM, &
             K_HS_DOXY_RED_INHB  , K_HS_NO3N_RED_INHB, K_HS_MN_IV_RED_INHB, &
             K_HS_FE_III_RED_INHB, K_HS_S_PLUS_6_RED_INHB, nkn, &
             LIM_DOXY_RED        , LIM_NO3N_RED          , LIM_MN_IV_RED  , &
             LIM_FE_III_RED      , LIM_S_PLUS_6_RED      , LIM_DOC_RED, &
             PE, FE_II_DISS_EQ   , FE_III_DISS_EQ, MN_II_DISS)

        FE_III_DISS_EQ = FE_III_DISS_EQ * FE_MOLAR_MASS_MG
        PROCESS_RATES(1:nkn,FE_III_INDEX, 4) = FE_III_DISS_EQ
        ! ---------------------------------------------------------------------------
        ! Handle Fe2+ dissolution changes by Ali Ert�rk, 2 nd of july 2016
        ! ---------------------------------------------------------------------------

        ! For now, take fixed values for equlibrium and solubility constants
        ! In future, change these to temperature and/or other environmental
        ! variable based equations.
        K_EQ_S_1  (:) = 8.90D-8
        K_EQ_S_2  (:) = 1.20D-13
        K_SP_FES  (:) = 8.00D-19
        !K_SP_FEOH3(:) = 1.00D-36
        !K_SP_FEPO4(:) = 9.91D-16

        ! -------------------------------------------------------------------------
        ! Calculate H2S Species
        ! -------------------------------------------------------------------------

        ! Convert sulphide to moles for aquatic chemistry calculations
        HS2_TOT(:) = S_MINUS_2(:) / S_MOLAR_MASS_MG

        H2S_DIVISOR(:) = (H_PLUS(:) * H_PLUS(:)) + &
            (H_PLUS(:) * K_EQ_S_1(:)) + (K_EQ_S_1(:) * K_EQ_S_2(:))

        FRAC_HS_MINUS_IN_H2S_TOT   (:) = (H_PLUS(:)   * K_EQ_S_1(:)) / H2S_DIVISOR(:)
        FRAC_S_MINUS_TWO_IN_H2S_TOT(:) = (K_EQ_S_1(:) * K_EQ_S_2(:)) / H2S_DIVISOR(:)

        FRAC_H2S_IN_H2S_TOT(:)         = &
            1.0D0 - (FRAC_HS_MINUS_IN_H2S_TOT(:) + FRAC_S_MINUS_TWO_IN_H2S_TOT(:))

        H2S        (:) = HS2_TOT(:) * FRAC_H2S_IN_H2S_TOT        (:)
        HS_MINUS   (:) = HS2_TOT(:) * FRAC_HS_MINUS_IN_H2S_TOT   (:)
        S_MINUS_TWO(:) = HS2_TOT(:) * FRAC_S_MINUS_TWO_IN_H2S_TOT(:)
        ! -------------------------------------------------------------------------
        ! End of calculate H2S Species
        ! -------------------------------------------------------------------------

        ! -------------------------------------------------------------------------
        ! Changes by Ali Ert�rk (2 July 2016)
        ! Change updated by Ali (9 August 2016)
        ! -------------------------------------------------------------------------
        call IRON_II_DISSOLUTION(HS2_TOT, PH, TOT_ALK, nkn, FE_II_DISS_EQ)
        FE_II_DISS_EQ = FE_II_DISS_EQ * FE_MOLAR_MASS_MG

        ! -------------------------------------------------------------------------
        ! End of changes by Ali Ert�rk (2 July 2016)
        ! -------------------------------------------------------------------------

        if(debug_stranger) then
            call DBGSTR_PEL_FE_II_DISS_01(STATE_VARIABLES, PH, TIME, nkn, nstate, node_active, error)
        end if

        ! -------------------------------------------------------------------------
        ! Updated by Ali and Petras, 9 August 2016
        !
        ! If it is the first timestep, initialize the dissolved fraction by assuming
        ! that the system is in equlibrum (option 1) or the dissolved fractions will
        ! be initialized (option 2). Otherwise calculate the dissolved fractions will
        ! be calculated by simple submodel based on the analytical solution.
        ! -------------------------------------------------------------------------
        where (FE_II_DISS_EQ > FE_II)
            FE_II_DISS_EQ = FE_II
        end where

        if (FIRST_TIME_STEP > 0) then

            select case (INIT_OPTION_OF_FE_II_DISS)

                case(1)
                    ! Handle the different between saturated and nonsaturated cases.
                    ! If more Fe2+ is allowed to dissolve than the total Fe2+ present then
                    !
                    ! Assume unsaturated case, ie
                    !     - Dissolved Fe2+ is equal to total Fe2+
                    !     - Multiplier for dissolved Fe2+ is equal to 1
                    !     - Multiplier for particulate Fe2+ is equal to 0
                    ! otherwise
                    !     - Dissolved Fe2+ is smaller than to total Fe2+
                    !     - Multiplier for dissolved Fe2+ is between 0 and 1
                    !     - Multiplier for particulate Fe2+ is between 0 and 1

                    where(FE_II_DISS_EQ >= FE_II)
                        FE_II_DISS      = FE_II
                        MULT_FE_II_DISS = 1.0D0
                        MULT_FE_II_PART = 0.0D0
                    elsewhere(FE_II .lt. 1.0D-20)
                        ! Guard against division by zero when total Fe2+ is depleted
                        FE_II_DISS      = 0.0D0
                        MULT_FE_II_DISS = 1.0D0
                        MULT_FE_II_PART = 0.0D0
                    elsewhere
                        MULT_FE_II_DISS = FE_II_DISS_EQ / FE_II
                        MULT_FE_II_PART = 1.0D0 - MULT_FE_II_DISS
                    end where

                case(2)
                    ! Get the initial fraction of dissolved Fe2+ from model constants and
                    ! recalculate the initial concentration accordingly.
                    MULT_FE_II_DISS = INIT_MULT_FE_II_DISS
                    MULT_FE_II_PART = 1.0D0 - MULT_FE_II_DISS

            end select

            call CALC_DISS_ME_CONC &
                 (FE_II                                                    , & ! Total Fe2+
                  (MULT_FE_II_DISS * FE_II)                                , & ! Dissolved Fe2+ from previous time step
                  FE_II_DISS_EQ                                            , & ! Equilibrium concentration for dissolved Fe2+
                  (k_DISS_FE_II_20 * (THETA_k_DISS_FE_II**(TEMP - 20.0D0))), & ! Dissolution rate constant for Fe2+
                  TIME_STEP                                                , & ! Time step in days
                  nkn                                                      , &
                  1                                                        , & ! number of layers
                  DISS_FE_II_CONC_TS_END                                   , & ! Estimated dissolved Fe2+ at the end of time step (for output)
                  DISS_FE_II_CONC_TS_AVG)                                      ! Estimated avg. dissolved Fe2+ during the timestep to be used for kinetic calculations
        else
            call CALC_DISS_ME_CONC &
                 (FE_II                                                    , & ! Total Fe2+
                  (SAVED_OUTPUTS(:,1) * FE_II)                             , & ! Dissolved Fe2+ from previous time step
                  FE_II_DISS_EQ                                            , & ! Equilibrium concentration for dissolved Fe2+
                  (k_DISS_FE_II_20 * (THETA_k_DISS_FE_II**(TEMP - 20.0D0))), & ! Dissolution rate constant for Fe2+
                  TIME_STEP                                                , & ! Time step in days
                  nkn                                                      , &
                  1                                                        , & ! number of layers
                  DISS_FE_II_CONC_TS_END                                   , & ! Estimated dissolved Fe2+ at the end of time step (for output)
                  DISS_FE_II_CONC_TS_AVG)                                      ! Estimated avg. dissolved Fe2+ during the timestep to be used for kinetic calculations
        end if


        where(DISS_FE_II_CONC_TS_AVG >= FE_II)
            FE_II_DISS      = FE_II
            MULT_FE_II_DISS = 1.0D0
            MULT_FE_II_PART = 0.0D0
        elsewhere(FE_II .lt. 1.0D-20)
            ! Guard against division by zero when total Fe2+ is depleted
            FE_II_DISS      = 0.0D0
            MULT_FE_II_DISS = 1.0D0
            MULT_FE_II_PART = 0.0D0
        elsewhere
            MULT_FE_II_DISS = DISS_FE_II_CONC_TS_AVG / FE_II
            MULT_FE_II_PART = 1.0D0 - MULT_FE_II_DISS
        end where

        where(DISS_FE_II_CONC_TS_END >= FE_II)
            DISS_FE_II_CONC_TS_END = FE_II
        end where

        ! -------------------------------------------------------------------------
        ! End of calculate the dissolved and particulate fractions of Fe2+
        ! -------------------------------------------------------------------------

        ! -------------------------------------------------------------------------
        ! Calculate the dissolved and particulate fractions of Fe3+
        ! -------------------------------------------------------------------------

        ! -------------------------------------------------------------------------
        ! Updated by Ali and Petras, 9 August 2016
        !
        ! If it is the first timestep, initialize the dissolved fraction by assuming
        ! that the system is in equlibrum (option 1) or the dissolved fractions will
        ! be initialized (option 2). Otherwise calculate the dissolved fractions will
        ! be calculated by simple submodel based on the analytical solution.
        ! -------------------------------------------------------------------------
        PROCESS_RATES(1:nkn,FE_III_INDEX, 5) = FE_III_DISS_EQ
        PROCESS_RATES(1:nkn,FE_III_INDEX, 6) = FE_III

        where (FE_III_DISS_EQ > FE_III)
            FE_III_DISS_EQ = FE_III
        end where

        PROCESS_RATES(1:nkn,FE_III_INDEX, 7) = FE_III_DISS_EQ

        if (FIRST_TIME_STEP > 0) then

            select case (INIT_OPTION_OF_FE_III_DISS)

                case(1)
                    ! Handle the different between saturated and nonsaturated cases.
                    ! If more Fe3+ is allowed to dissolve than the total Fe3+ present then
                    !
                    ! Assume unsaturated case, ie
                    !     - Dissolved Fe3+ is equal to total Fe3+
                    !     - Multiplier for dissolved Fe3+ is equal to 1
                    !     - Multiplier for particulate Fe3+ is equal to 0
                    ! otherwise
                    !     - Dissolved Fe3+ is smaller than to total Fe3+
                    !     - Multiplier for dissolved Fe3+ is between 0 and 1
                    !     - Multiplier for particulate Fe3+ is between 0 and 1

                    where(FE_III_DISS_EQ >= FE_III)
                        FE_III_DISS      = FE_III
                        MULT_FE_III_DISS = 1.0D0
                        MULT_FE_III_PART = 0.0D0
                    elsewhere
                        MULT_FE_III_DISS = FE_III_DISS_EQ / FE_III
                        MULT_FE_III_PART = 1.0D0 - MULT_FE_III_DISS
                    end where

                case(2)
                    ! Get the initial fraction of dissolved Fe3+ from model constants and
                    ! recalculate the initial concentration accordingly.
                    MULT_FE_III_DISS = INIT_MULT_FE_III_DISS
                    MULT_FE_III_PART = 1.0D0 - MULT_FE_III_DISS

                end select


            call CALC_DISS_ME_CONC &
                 (FE_III                                                     , & ! Total Fe3+
                  (MULT_FE_III_DISS * FE_III)                                , & ! Dissolved Fe3+ from previous time step
                  FE_III_DISS_EQ                                             , & ! Equilibrium concentration for dissolved Fe3+
                  (k_DISS_FE_III_20 * (THETA_k_DISS_FE_III**(TEMP - 20.0D0))), & ! Dissolution rate constant for Fe3+
                  TIME_STEP                                                  , & ! Time step in days
                  nkn                                                        , &
                  1                                                          , & ! number of layers
                  DISS_FE_III_CONC_TS_END                                    , & ! Estimated dissolved Fe3+ at the end of time step (for output)
                  DISS_FE_III_CONC_TS_AVG)                                       ! Estimated avg. dissolved Fe3+ during the timestep to be used for kinetic calculations
        else
            call CALC_DISS_ME_CONC &
                 (FE_III                                                     , & ! Total Fe3+
                  (SAVED_OUTPUTS(:,2) * FE_III)                              , & ! Dissolved Fe3+ from previous time step
                  FE_III_DISS_EQ                                             , & ! Equilibrium concentration for dissolved Fe3+
                  (k_DISS_FE_III_20 * (THETA_k_DISS_FE_III**(TEMP - 20.0D0))), & ! Dissolution rate constant for Fe3+
                  TIME_STEP                                                  , & ! Time step in days
                  nkn                                                        , &
                  1                                                          , & ! number of layers
                  DISS_FE_III_CONC_TS_END                                    , & ! Estimated dissolved Fe3+ at the end of time step (for output)
                  DISS_FE_III_CONC_TS_AVG)                                       ! Estimated avg. dissolved Fe3+ during the timestep to be used for kinetic calculations
        end if

        PROCESS_RATES(1:nkn,FE_III_INDEX, 8) = DISS_FE_III_CONC_TS_AVG
        PROCESS_RATES(1:nkn,FE_III_INDEX, 9) = DISS_FE_III_CONC_TS_END


        where(DISS_FE_III_CONC_TS_AVG >= FE_III)
            FE_III_DISS      = FE_III
            MULT_FE_III_DISS = 1.0D0
            MULT_FE_III_PART = 0.0D0
        elsewhere(FE_III .lt. 1.0D-20)
            ! Guard against division by zero when total Fe3+ is depleted
            FE_III_DISS      = 0.0D0
            MULT_FE_III_DISS = 1.0D0
            MULT_FE_III_PART = 0.0D0
        elsewhere
            MULT_FE_III_DISS = DISS_FE_III_CONC_TS_AVG / FE_III
            MULT_FE_III_PART = 1.0D0 - MULT_FE_III_DISS
        end where

        where(DISS_FE_III_CONC_TS_END >= FE_III)
            DISS_FE_III_CONC_TS_END = FE_III
        end where


        !MULT_FE_III_DISS = DISS_FE_III_CONC_TS_AVG / FE_III
        !MULT_FE_III_PART = 1.0D0 - MULT_FE_III_DISS
        ! -------------------------------------------------------------------------
        ! End of calculate the dissolved and particulate fractions of Fe3+
        ! -------------------------------------------------------------------------

        ! Handle the different between saturated and nonsaturated cases.
        ! If more Mn2+ is allowed to dissolve than the total Mn2+ present then
        !
        ! Assume unsaturated case, ie
        !     - Dissolved Mn2+ is equal to total Mn2+
        !     - Multiplier for dissolved Mn2+ is equal to 1
        !     - Multiplier for particulate Mn2+ is equal to 0
        ! otherwise
        !     - Dissolved Mn2+ is smaller than to total Mn2+
        !     - Multiplier for dissolved Mn2+ is between 0 and 1
        !     - Multiplier for particulate Mn2+ is between 0 and 1

        where(MN_II_DISS >= MN_II)
            MN_II_DISS      = MN_II
            MULT_MN_II_DISS = 1.0D0
            MULT_MN_II_PART = 0.0D0
        elsewhere(MN_II .lt. 1.0D-20)
            ! Guard against division by zero when total Mn2+ is depleted
            MN_II_DISS      = 0.0D0
            MULT_MN_II_DISS = 1.0D0
            MULT_MN_II_PART = 0.0D0
        elsewhere
            MULT_MN_II_DISS = MN_II_DISS / MN_II
            MULT_MN_II_PART = 1.0D0 - MULT_MN_II_DISS
        end where

        ! -------------------------------------------------------------------------
        ! New equlibrium calculations added at 25th of January 2016
        ! -------------------------------------------------------------------------

        MULT_MN_IV_DISS(:)  = 0.0D0
        MULT_MN_IV_PART(:)  = 1.0D0
        ! -----------------------------------------------------------------------
        ! End of additions made 30th November 2015 related to dissolved and solid
        ! iron and mangese species
        ! -----------------------------------------------------------------------

        ! -----------------------------------------------------------------------
        ! Introduced 26th of January 2016 to allow dissolved fractions of iron
        ! (Fe2+ and Fe3+) and manganese (Mn2+ and Mn4+) to outside to be used
        ! by AQUABC settling calculations
        !
        ! Updated by Ali and Petras 9th of August 2016
        ! -----------------------------------------------------------------------
        ! Guard against division by zero for saved outputs
        where(FE_II .lt. 1.0D-20)
            SAVED_OUTPUTS(:,1) = 1.0D0
        elsewhere
            SAVED_OUTPUTS(:,1) = DISS_FE_II_CONC_TS_END  / FE_II
        end where
        where(FE_III .lt. 1.0D-20)
            SAVED_OUTPUTS(:,2) = 1.0D0
        elsewhere
            SAVED_OUTPUTS(:,2) = DISS_FE_III_CONC_TS_END / FE_III
        end where
        SAVED_OUTPUTS(:,3) = MULT_MN_II_DISS(:)
        SAVED_OUTPUTS(:,4) = MULT_MN_IV_DISS(:)
        ! -----------------------------------------------------------------------
        ! End of additions 26th of January 2016
        ! -----------------------------------------------------------------------

        !Calculate the dissolved MN IV
        MN_IV_DISS(:) = MN_IV(:) * MULT_MN_IV_DISS(:)

        call IP_SOLUBLE_FRACTION &
             (FE_III     , &
              PO4_P      , &
              K_ONE_TIP  , &
              K_TWO_TIP  , &
              K_THREE_TIP, &
              PH         , &
              nkn        , &
              1          , &
              DIP_OVER_IP)

        SAVED_OUTPUTS(:,5) = DIP_OVER_IP(:)

    else  ! NO ADVANCED REDOX

        LIM_DOXY_RED = DISS_OXYGEN  / (DISS_OXYGEN + K_HS_DOXY_RED_LIM)

        LIM_NO3N_RED = (NO3_N / (NO3_N + K_HS_NO3N_RED_LIM)) * &
            (K_HS_DOXY_RED_INHB / (DISS_OXYGEN + K_HS_DOXY_RED_INHB))

        DIP_OVER_IP = 1.0D0

        SAVED_OUTPUTS(:,1) = 0.0D0
        SAVED_OUTPUTS(:,2) = 0.0D0
        SAVED_OUTPUTS(:,3) = 0.0D0
        SAVED_OUTPUTS(:,4) = 0.0D0
        SAVED_OUTPUTS(:,5) = 1.0D0
    end if
    !*****************************************
    !     D I S S O L V E D  O X Y G E N     !
    !*****************************************
    do k=1,nkn
            DISS_OXYGEN_SAT(k) = DO_SATURATION(TEMP(k), SALT(k), ELEVATION(k))

            if (SURFACE_BOXES(k) == 1) then !first layer

                if (K_A < 0.0D0) then
                    K_A_CALC(k) = KAWIND(WINDS(k), TEMP(k), AIRTEMP(k), DEPTH(k), 3.0D0)
                    R_AERATION(k) = K_A_CALC(k) * (DISS_OXYGEN_SAT(k) - DISS_OXYGEN(k))
                else
                    K_A_CALC(k) = K_A

                    R_AERATION(k) = K_A_CALC(k) * (DISS_OXYGEN_SAT(k) - DISS_OXYGEN(k)) * &
                         (THETA_K_A ** (TEMP(k) - 2.0D1))
                end if

                !----------------------------------------------------------------------
                ! 2 February 2015
                ! New code added to account the effect of ice cover.
                !----------------------------------------------------------------------
                R_AERATION(k) = (1.0D0 - ice_cover(k)) * R_AERATION(k)
                !----------------------------------------------------------------------
                ! End of new code added to account the effect of ice cover.
                !----------------------------------------------------------------------
            else
                R_AERATION(k) = 0.0D0 ! other layers
            end if
    end do

    ! Calculate the total phytoplankton.
    PHYT_TOT_C = DIA_C + CYN_C + OPA_C + FIX_CYN_C + NOST_VEG_HET_C

    !**********************************!
    !**********************************!
    !     P H Y T O P L A N K T O N    !
    !**********************************!
    !**********************************!

    ! total chlorophyl in micrograms
    CHLA = ((DIA_C          / DIA_C_TO_CHLA    ) + (CYN_C / CYN_C_TO_CHLA) + &
            (FIX_CYN_C      / FIX_CYN_C_TO_CHLA) + (OPA_C / OPA_C_TO_CHLA) + &
            (NOST_VEG_HET_C / NOST_C_TO_CHLA)) * 1.0D3

    ! Debug: print CHLA components for node 1 to find negative values (include time/context)
    ! Note: Commented out to reduce log noise - uncomment for detailed CHLA debugging
    ! write(6,'(A,F12.4,A,ES10.3)') 'DEBUG: CHLA node1: TIME=', TIME, ' DT=', TIME_STEP
    ! write(6,'(A,5ES12.4)') '  DIA/CYN/FIX/OPA/NOST=', DIA_C(1), CYN_C(1), FIX_CYN_C(1), OPA_C(1), NOST_VEG_HET_C(1)

    select case (LIGHT_EXTINCTION_OPTION)

            case (0)
                call light_kd(K_B_E, K_E, CHLA, nkn)

            case (1)
                ! Defensive guard: ensure CHLA is non-negative before fractional exponent
                do i = 1, nkn
                    if (CHLA(i) < 0.0D0) then
                        write(6,'(A,I4,A,ES12.4)') 'WARN: negative CHLA at node ', i, ' value=', CHLA(i)
                    end if
                end do
                chla_pos = CHLA
                where (chla_pos .lt. 0.0D0)
                    chla_pos = 0.0D0
                end where
                K_E = K_B_E + (8.8D-3 * chla_pos) + (5.4D-2 * (chla_pos ** (2.0D0 / 3.0D0)))

        end select

        ! Debug print: K_E and DEPTH for troubleshooting NaNs (commented to reduce log noise)
        ! write(6,'(A,3ES12.4)') 'DEBUG: K_E(1)/DEPTH(1)/CHLA(1)=', K_E(1), DEPTH(1), CHLA(1)


    !********************
    !      DIATOMS      !
    !********************

    !Calculations for diatom growth
    call DIATOMS(DIA_PARAMS              , &
                 DIA_LIGHT_SAT           , &
                 NH4_N                   , &
                 NO3_N                   , &
                 (PO4_P * DIP_OVER_IP)   , & ! Change, 6 July 2016, original call was PO4P
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

    ! Consider the effect of growth inhibition which is supplied from outside
    ! by external models
    R_DIA_GROWTH(:) = R_DIA_GROWTH(:) * GROWTH_INHIB_FACTOR_DIA(:)

    !**************************************
    ! NON-NITROGEN FIXING CYANOBACTERIA   !
    !*************************************

    !Calculations for non-fixing cyanobacteria growth
    call CYANOBACTERIA_BOUYANT &
         (CYN_PARAMS              , &
          CYN_LIGHT_SAT           , &
          NH4_N                   , &
          NO3_N                   , &
          DISS_ORG_N              , &
          (PO4_P * DIP_OVER_IP)   , &
          DISS_OXYGEN             , &
          CYN_C                   , &
          ZOO_C                   , &
          TEMP                    , &
          WINDS                   , &
          I_A                     , &
          K_E                     , &
          DEPTH                   , &
          CHLA                    , &
          FDAY                    , &
          TIME_STEP               , &
          SMITH                   , &
          nkn                     , &
          KG_CYN                  , &
          ALPHA_0                 , &
          ALPHA_1                 , &
          LIM_KG_CYN_TEMP         , &
          LIM_KG_CYN_LIGHT        , &
          LIM_KG_CYN_DOXY         , &
          LIM_KG_CYN_N            , &
          LIM_KG_CYN_P            , &
          LIM_KG_CYN_NUTR         , &
          LIM_KG_CYN              , &
          R_CYN_GROWTH            , &
          R_CYN_MET               , &
          R_CYN_RESP              , &
          R_CYN_EXCR              , &
          R_CYN_INT_RESP          , &
          KD_CYN                  , &
          FAC_HYPOX_CYN_D         , &
          R_CYN_DEATH             , &
          PREF_DIN_DON_CYN        , &
          PREF_NH4N_CYN)

    ! Consider the effect of growth inhibition which is supplied from outside
    ! by external models
    R_CYN_GROWTH(:) = R_CYN_GROWTH(:) * GROWTH_INHIB_FACTOR_CYN(:)


    !********************************
    ! NITROGEN FIXING CYANOBACTERIA !
    !********************************
    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        call FIX_CYANOBACTERIA_BOUYANT  &
               (FIX_CYN_PARAMS               , &
                TIME_STEP                    , &
                SMITH                        , &
                nkn                          , &
                FDAY                         , &
                I_A                          , &
                K_E                          , &
                DEPTH                        , &
                CHLA                         , &
                TEMP                         , &
                WINDS                        , &
                NH4_N                        , &
                NO3_N                        , &
                DISS_ORG_N                   , &
                (PO4_P * DIP_OVER_IP)        , &
                DISS_OXYGEN                  , &
                FIX_CYN_C                    , &
                FIX_CYN_LIGHT_SAT            , &
                ALPHA_0                      , &
                ALPHA_1                      , &
                KG_FIX_CYN                   , &
                LIM_KG_FIX_CYN_LIGHT         , &
                LIM_KG_FIX_CYN_TEMP          , &
                LIM_KG_FIX_CYN_DOXY          , &
                LIM_KG_NON_FIX_CYN_N         , &
                LIM_KG_NON_FIX_CYN_P         , &
                LIM_KG_NON_FIX_CYN_NUTR      , &
                LIM_KG_FIX_FIX_CYN_N         , &
                LIM_KG_FIX_FIX_CYN_P         , &
                LIM_KG_FIX_FIX_CYN_NUTR      , &
                LIM_KG_NON_FIX_CYN           , &
                LIM_KG_FIX_FIX_CYN           , &
                R_NON_FIX_CYN_GROWTH         , &
                R_FIX_FIX_CYN_GROWTH         , &
                R_FIX_CYN_GROWTH             , &
                R_FIX_CYN_MET                , &
                R_FIX_CYN_RESP               , &
                R_FIX_CYN_EXCR               , &
                R_FIX_CYN_INT_RESP           , &
                KD_FIX_CYN                   , &
                FAC_HYPOX_FIX_CYN_D          , &
                R_FIX_CYN_DEATH              , &
                PREF_NH4N_DON_FIX_CYN)

        ! Consider the effect of growth inhibition which is supplied from outside
        ! by external models
        R_FIX_CYN_GROWTH(:)     = R_FIX_CYN_GROWTH    (:) * GROWTH_INHIB_FACTOR_FIX_CYN(:)
        R_NON_FIX_CYN_GROWTH(:) = R_NON_FIX_CYN_GROWTH(:) * GROWTH_INHIB_FACTOR_FIX_CYN(:)
        R_FIX_FIX_CYN_GROWTH(:) = R_FIX_FIX_CYN_GROWTH(:) * GROWTH_INHIB_FACTOR_FIX_CYN(:)
    else
        FIX_CYN_C           (:) = 0.0D0
        R_NON_FIX_CYN_GROWTH(:) = 0.0D0
        R_FIX_FIX_CYN_GROWTH(:) = 0.0D0
        R_FIX_CYN_GROWTH    (:) = 0.0D0
        R_FIX_CYN_MET       (:) = 0.0D0
        R_FIX_CYN_RESP      (:) = 0.0D0
        R_FIX_CYN_EXCR      (:) = 0.0D0
        R_FIX_CYN_INT_RESP  (:) = 0.0D0
        R_FIX_CYN_DEATH     (:) = 0.0D0
    end if


    !*****************************
    ! OTHER PLANKTONIC ALGAE     !
    !*****************************
    call OTHER_PLANKTONIC_ALGAE &
           (OPA_PARAMS              , &
            OPA_LIGHT_SAT           , &
            NH4_N                   , &
            NO3_N                   , &
            (PO4_P * DIP_OVER_IP)   , & ! Change, 6 July 2016, original call was PO4P
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

        R_OPA_GROWTH(:) = R_OPA_GROWTH(:) * GROWTH_INHIB_FACTOR_OPA(:)
    !******************************!
    !******************************!
    !          NOSTOCALES          !
    !******************************!
    !******************************!


    if (DO_NOSTOCALES > 0) then

        ! Calls to the routines that should be incorporated to NOSTOCALES
        call DOP_DIP_PREFS(PREF_DIP_DOP_NOST, (frac_avail_DOP * DISS_ORG_P), &
              (PO4_P * DIP_OVER_IP) , KHS_DP_NOST_VEG_HET, nkn)

        call DIN_DON_PREFS &
             (PREF_DIN_DON_NOST, NH4_N, DISS_ORG_N, &
              frac_avail_DON_NOST, NO3_N, KHS_DN_NOST_VEG_HET, nkn)

        call AMMONIA_PREFS &
            (PREF_NH4N_NOST, (NH4_N * PREF_DIN_DON_NOST), &
             (NO3_N * PREF_DIN_DON_NOST), KHS_DN_NOST_VEG_HET, nkn)

        call NOSTOCALES &
           (NOST_PARAMS                       , &
            TIME_STEP                         , &
            DAY_OF_YEAR                       , &
            SMITH                             , &
            nkn                               , &
            NOST_LIGHT_SAT                    , &
            FDAY                              , &
            I_A                               , &
            K_E                               , &
            DEPTH                             , &
            CHLA                              , &
            TEMP                              , &
            WINDS                             , &
            (NH4_N + NO3_N)                   , &
            frac_avail_DON_NOST * DISS_ORG_N  , &
            (PO4_P * DIP_OVER_IP + frac_avail_DOP * DISS_ORG_P) , &
            DISS_OXYGEN                       , &
            NOST_VEG_HET_C                    , &
            NOST_AKI_C                        , &
            KG_NOST_VEG_HET                   , &
            LIM_KG_NOST_VEG_HET_LIGHT         , &
            LIM_KG_NOST_VEG_HET_TEMP          , &
            LIM_KG_NOST_VEG_HET_DOXY          , &
            LIM_KG_NOST_VEG_HET_N             , &
            LIM_KG_NOST_VEG_HET_P             , &
            LIM_KG_NOST_VEG_HET_FIX           , &
            LIM_KG_NOST_VEG_HET_NON_FIX       , &
            R_NOST_VEG_HET_GROWTH             , &
            R_NOST_VEG_HET_FIX_GROWTH         , &
            R_NOST_VEG_HET_NON_FIX_GROWTH     , &
            R_NOST_VEG_HET_MET                , &
            R_NOST_VEG_HET_RESP               , &
            R_NOST_VEG_HET_EXCR               , &
            R_NOST_VEG_HET_INT_RESP           , &
            KD_NOST_VEG_HET                   , &
            FAC_HYPOX_NOST_VEG_HET_D          , &
            R_NOST_VEG_HET_DEATH              , &
            R_DENS_MORT_NOST_VEG_HET          , &
            R_GERM_NOST_AKI                   , &
            R_FORM_NOST_AKI                   , &
            R_LOSS_AKI                        , &
            R_MORT_AKI)

            ! Consider the effect of growth inhibition which is supplied from outside
            ! by external models
            R_NOST_VEG_HET_GROWTH        (:) = &
                R_NOST_VEG_HET_GROWTH        (:) * GROWTH_INHIB_FACTOR_NOST(:)

            R_NOST_VEG_HET_NON_FIX_GROWTH(:) = &
                R_NOST_VEG_HET_NON_FIX_GROWTH(:) * GROWTH_INHIB_FACTOR_NOST(:)

            R_NOST_VEG_HET_FIX_GROWTH    (:) = &
                R_NOST_VEG_HET_FIX_GROWTH    (:) * GROWTH_INHIB_FACTOR_NOST(:)
    else
        R_NOST_VEG_HET_GROWTH    = 0.0D0
        R_NOST_VEG_HET_MET       = 0.0D0
        R_NOST_VEG_HET_RESP      = 0.0D0
        R_NOST_VEG_HET_EXCR      = 0.0D0
        R_NOST_VEG_HET_INT_RESP  = 0.0D0
        R_NOST_VEG_HET_DEATH     = 0.0D0
        R_DENS_MORT_NOST_VEG_HET = 0.0D0
        R_GERM_NOST_AKI          = 0.0D0
        R_FORM_NOST_AKI          = 0.0D0
        R_LOSS_AKI               = 0.0D0
        R_MORT_AKI               = 0.0D0
    end if


    !******************************!
    !******************************!
    !     Z O O P L A N K T O N    !
    !******************************!
    !******************************!
    call ZOOPLANKTON &
           (ZOO_PARAMS                       , &
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

    R_ZOO_FEEDING_DIA           (:) = &
        R_ZOO_FEEDING_DIA           (:) * GROWTH_INHIB_FACTOR_ZOO(:)

    R_ZOO_FEEDING_CYN           (:) = &
        R_ZOO_FEEDING_CYN           (:) * GROWTH_INHIB_FACTOR_ZOO(:)

    R_ZOO_FEEDING_FIX_CYN       (:) = &
        R_ZOO_FEEDING_FIX_CYN       (:) * GROWTH_INHIB_FACTOR_ZOO(:)

    R_ZOO_FEEDING_NOST_VEG_HET  (:) = &
        R_ZOO_FEEDING_NOST_VEG_HET  (:) * GROWTH_INHIB_FACTOR_ZOO(:)

    R_ZOO_FEEDING_OPA           (:) = &
        R_ZOO_FEEDING_OPA           (:) * GROWTH_INHIB_FACTOR_ZOO(:)

    R_ZOO_FEEDING_DET_PART_ORG_C(:) = &
        R_ZOO_FEEDING_DET_PART_ORG_C(:) * GROWTH_INHIB_FACTOR_ZOO(:)

    if (ZOOP_OPTION_1 > 0) then
        ACTUAL_ZOO_N_TO_C = ZOO_N / max(ZOO_C, MIN_CONCENTRATION)
        ACTUAL_ZOO_P_TO_C = ZOO_P / max(ZOO_C, MIN_CONCENTRATION)
    end if

    if(debug_stranger) then
        call DBGSTR_PEL_R_ZOO_GROWTH_01(TIME, nkn, nstate, node_active, error)
    end if

    if(debug_stranger) then
        call DBGSTR_PEL_R_ZOO_RESP_01(TIME, nkn, nstate, node_active, error)
    end if


    !*********************************************************************!
    !     D E A T H   O R G A N I C     P A R T I C L E S    DISSOLUTION !
    !*********************************************************************!

    call ORGANIC_CARBON_DISSOLUTION &
           (FAC_PHYT_DET_PART_ORG_C     , &
            KDISS_DET_PART_ORG_C_20     , &
            THETA_KDISS_DET_PART_ORG_C  , &
            KHS_POC_DISS_SAT            , &
            nkn                         , &
            TEMP                        , &
            DET_PART_ORG_C              , &
            PHYT_TOT_C                  , &
            LIM_PHYT_DISS_DET_PART_ORG_C, &
            R_DET_PART_ORG_C_DISSOLUTION)

    ACTUAL_DET_N_TO_C = DET_PART_ORG_N / max(DET_PART_ORG_C, MIN_CONCENTRATION)
    ACTUAL_DET_P_TO_C = DET_PART_ORG_P / max(DET_PART_ORG_C, MIN_CONCENTRATION)

    ! Nitrogen dissolution

    ! Accerelation of hydrolysis when DIN is scarce
    LIM_N_DISS_DET_PART_ORG_N = KHS_DISS_N / (KHS_DISS_N + (NH4_N + NO3_N))
    LIM_PHY_N_DISS_DET_PART_ORG_N = LIM_N_DISS_DET_PART_ORG_N * FAC_PHYT_DET_PART_ORG_N * PHYT_TOT_C

    R_DET_PART_ORG_N_DISSOLUTION = (KDISS_DET_PART_ORG_N_20 + LIM_PHY_N_DISS_DET_PART_ORG_N) * &
       (THETA_KDISS_DET_PART_ORG_N ** (TEMP - 2.0D1)) * DET_PART_ORG_N * &
       (KHS_PON_DISS_SAT/(DET_PART_ORG_N + KHS_PON_DISS_SAT))

    ! Phosphorus dissolution

    ! Accerelation of hydrolysis when DIP is scarce
    LIM_P_DISS_DET_PART_ORG_P = KHS_DISS_P / (KHS_DISS_P + DIP_OVER_IP*PO4_P)
    LIM_PHY_P_DISS_DET_PART_ORG_P = LIM_P_DISS_DET_PART_ORG_P * FAC_PHYT_DET_PART_ORG_P * PHYT_TOT_C

    R_DET_PART_ORG_P_DISSOLUTION = (KDISS_DET_PART_ORG_P_20 + LIM_PHY_P_DISS_DET_PART_ORG_P) * &
       (THETA_KDISS_DET_PART_ORG_P ** (TEMP - 2.0D1)) * DET_PART_ORG_P * &
       (KHS_POP_DISS_SAT/(DET_PART_ORG_P + KHS_POP_DISS_SAT))

    !Diatom total respiration rate
    R_DIA_TOT_RESP               = R_DIA_RESP                + R_DIA_INT_RESP

    !Non-fixing cyanobacteria total respiration rate
    R_CYN_TOT_RESP               = R_CYN_RESP                + R_CYN_INT_RESP

    !Other planktonic algae total respiration rate
    R_OPA_TOT_RESP               = R_OPA_RESP                + R_OPA_INT_RESP

    !Nitrogen fixing cyanobacteria total respiration rate
    R_FIX_CYN_TOT_RESP           = R_FIX_CYN_RESP            + R_FIX_CYN_INT_RESP

    !Nostacles
    R_NOST_VEG_HET_TOT_RESP      = R_NOST_VEG_HET_RESP       + R_NOST_VEG_HET_INT_RESP

    !Zooplankton total respiration rate
    R_ZOO_TOT_RESP               = R_ZOO_INT_RESP            + R_ZOO_RESP


    !*********************************************************************!
    !     SILICON                                                         !
    !*********************************************************************!

    !Dissolution rate of biogenic silicon
    R_PART_Si_DISS = KDISS_PART_SI_20 * &
            (THETA_KDISS_PART_SI ** (TEMP - 2.0D1)) * PART_SI


    !*********************************************************************!
    !     MINERALIZATION OF DOC, DON, DOP whith bacteria are not modelled.
    !     Called abiotic in the sense of modelling method
    !*********************************************************************!

    !Algal dependent mineralisation rate

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then
        call ORGANIC_CARBON_MINERALIZATION &
                (FAC_PHYT_AMIN_DOC           , &
                 K_MIN_DOC_DOXY_20           , &
                 K_MIN_DOC_NO3N_20           , &
                 K_MIN_DOC_MN_IV_20          , &
                 K_MIN_DOC_FE_III_20         , &
                 K_MIN_DOC_S_PLUS_6_20       , &
                 K_MIN_DOC_DOC_20            , &
                 THETA_K_MIN_DOC_DOXY        , &
                 THETA_K_MIN_DOC_NO3N        , &
                 THETA_K_MIN_DOC_MN_IV       , &
                 THETA_K_MIN_DOC_FE_III      , &
                 THETA_K_MIN_DOC_S_PLUS_6    , &
                 THETA_K_MIN_DOC_DOC         , &
                 K_HS_DOC_MIN_DOXY           , &
                 K_HS_DOC_MIN_NO3N           , &
                 K_HS_DOC_MIN_MN_IV          , &
                 K_HS_DOC_MIN_FE_III         , &
                 K_HS_DOC_MIN_S_PLUS_6       , &
                 K_HS_DOC_MIN_DOC            , &
                 K_HS_DOXY_RED_LIM           , &
                 K_HS_NO3N_RED_LIM           , &
                 K_HS_MN_IV_RED_LIM          , &
                 K_HS_FE_III_RED_LIM         , &
                 K_HS_S_PLUS_6_RED_LIM       , &
                 K_HS_DOXY_RED_INHB          , &
                 K_HS_NO3N_RED_INHB          , &
                 K_HS_MN_IV_RED_INHB         , &
                 K_HS_FE_III_RED_INHB        , &
                 K_HS_S_PLUS_6_RED_INHB      , &
                 PH_MIN_DOC_MIN_DOXY         , &  !Min. pH for the optimum pH range for DOC mineralization with DOXY     as final electron acceptor (subroutine input)
                 PH_MIN_DOC_MIN_NO3N         , &  !Min. pH for the optimum pH range for DOC mineralization with NO3N     as final electron acceptor (subroutine input)
                 PH_MIN_DOC_MIN_MN_IV        , &  !Min. pH for the optimum pH range for DOC mineralization with MN_IV    as final electron acceptor (subroutine input)
                 PH_MIN_DOC_MIN_FE_III       , &  !Min. pH for the optimum pH range for DOC mineralization with FE_III   as final electron acceptor (subroutine input)
                 PH_MIN_DOC_MIN_S_PLUS_6     , &  !Min. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor (subroutine input)
                 PH_MIN_DOC_MIN_DOC          , &  !Min. pH for the optimum pH range for DOC mineralization with DOC      as final electron acceptor (subroutine input)
                 PH_MAX_DOC_MIN_DOXY         , &  !Max. pH for the optimum pH range for DOC mineralization with DOXY     as final electron acceptor (subroutine input)
                 PH_MAX_DOC_MIN_NO3N         , &  !Max. pH for the optimum pH range for DOC mineralization with NO3N     as final electron acceptor (subroutine input)
                 PH_MAX_DOC_MIN_MN_IV        , &  !Max. pH for the optimum pH range for DOC mineralization with MN_IV    as final electron acceptor (subroutine input)
                 PH_MAX_DOC_MIN_FE_III       , &  !Max. pH for the optimum pH range for DOC mineralization with FE_III   as final electron acceptor (subroutine input)
                 PH_MAX_DOC_MIN_S_PLUS_6     , &  !Max. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor (subroutine input)
                 PH_MAX_DOC_MIN_DOC          , &  !Max. pH for the optimum pH range for DOC mineralization with DOC      as final electron acceptor (subroutine input)
                 nkn                         , &
                 TEMP                        , &
                 DISS_ORG_C                  , &
                 PHYT_TOT_C                  , &
                 DISS_OXYGEN                 , &
                 NO3_N                       , &
                 MN_IV                       , &
                 FE_III                      , &
                 S_PLUS_6                    , &
                 PH                          , &
                 LIM_DOXY_RED                , &
                 LIM_NO3N_RED                , &
                 LIM_MN_IV_RED               , &
                 LIM_FE_III_RED              , &
                 LIM_S_PLUS_6_RED            , &
                 LIM_DOC_RED                 , &
                 LIM_PHYT_AMIN_DOC           , &
                 PH_CORR_DOC_MIN_DOXY        , &  !pH correction for DOC mineralization with DOXY     as final electron acceptor (subroutine output)
                 PH_CORR_DOC_MIN_NO3N        , &  !pH correction for DOC mineralization with NO3N     as final electron acceptor (subroutine output)
                 PH_CORR_DOC_MIN_MN_IV       , &  !pH correction for DOC mineralization with MN_IV    as final electron acceptor (subroutine output)
                 PH_CORR_DOC_MIN_FE_III      , &  !pH correction for DOC mineralization with FE_III   as final electron acceptor (subroutine output)
                 PH_CORR_DOC_MIN_S_PLUS_6    , &  !pH correction for DOC mineralization with S_PLUS_6 as final electron acceptor (subroutine output)
                 PH_CORR_DOC_MIN_DOC         , &  !pH correction for DOC mineralization with DOC      as final electron acceptor (subroutine output)
                 K_NO3_RED                   , &
                 K_MN_IV_RED                 , &
                 K_FE_III_RED                , &
                 K_S_PLUS_6_RED              , &
                 K_DOC_RED                   , &
                 R_ABIOTIC_DOC_MIN_DOXY      , &  !Process rate  for DOC mineralization with DOXY     as final electron acceptor (subroutine output)
                 R_ABIOTIC_DOC_MIN_NO3N      , &  !Process rate  for DOC mineralization with NO3N     as final electron acceptor (subroutine output)
                 R_ABIOTIC_DOC_MIN_MN_IV     , &  !Process rate  for DOC mineralization with MN_IV    as final electron acceptor (subroutine output)
                 R_ABIOTIC_DOC_MIN_FE_III    , &  !Process rate  for DOC mineralization with FE_III   as final electron acceptor (subroutine output)
                 R_ABIOTIC_DOC_MIN_S_PLUS_6  , &  !Process rate  for DOC mineralization with S_PLUS_6 as final electron acceptor (subroutine output)
                 R_ABIOTIC_DOC_MIN_DOC)           !Process rate  for DOC mineralization with DOC      as final electron acceptor (subroutine output)
    else
        where (PHYT_TOT_C .gt. K_MIN_PHYT_AMIN_DOC)
            LIM_PHYT_AMIN_DOC = FAC_PHYT_AMIN_DOC * (PHYT_TOT_C - K_MIN_PHYT_AMIN_DOC)
        elsewhere
            LIM_PHYT_AMIN_DOC = 0.D0
        end where

        call CALCULATE_PH_CORR &
             (PH_CORR_DOC_MIN_DOXY, PH, PH_MIN_DOC_MIN_DOXY, PH_MAX_DOC_MIN_DOXY, nkn)

        call CALCULATE_PH_CORR &
             (PH_CORR_DOC_MIN_NO3N, PH, PH_MIN_DOC_MIN_NO3N, PH_MAX_DOC_MIN_NO3N, nkn)

        R_ABIOTIC_DOC_MIN_DOXY = &
            (K_MIN_DOC_DOXY_20 + LIM_PHYT_AMIN_DOC) * &
            (THETA_K_MIN_DOC_DOXY ** (TEMP - 2.0D1)) * LIM_DOXY_RED * &
            PH_CORR_DOC_MIN_DOXY * &
            (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_DOXY)) * DISS_ORG_C

        R_ABIOTIC_DOC_MIN_NO3N = &
            K_MIN_DOC_NO3N_20  * (THETA_K_MIN_DOC_NO3N ** (TEMP - 2.0D1)) * &
            LIM_NO3N_RED * PH_CORR_DOC_MIN_NO3N * &
            (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_NO3N)) * DISS_ORG_C

        R_ABIOTIC_DOC_MIN_MN_IV    = 0.0D0
        R_ABIOTIC_DOC_MIN_FE_III   = 0.0D0
        R_ABIOTIC_DOC_MIN_S_PLUS_6 = 0.0D0
        R_ABIOTIC_DOC_MIN_DOC      = 0.0D0
    end if

    ! Accerelation of mineralisation when DIN is scarce
    LIM_N_AMIN_DON = KHS_AMIN_N / (KHS_AMIN_N + (NH4_N + NO3_N))

    where (PHYT_TOT_C .gt. K_MIN_PHYT_AMIN_DON)
        LIM_PHY_N_AMIN_DON = LIM_N_AMIN_DON * FAC_PHYT_AMIN_DON * (PHYT_TOT_C - K_MIN_PHYT_AMIN_DON)
    elsewhere
        LIM_PHY_N_AMIN_DON = 0.D0
    end where

    ! -------------------------------------------------------------------------
    ! DON mineralization compatible with redox cycle
    ! -------------------------------------------------------------------------

    ! 28 January 2016, the following commented lines are replaced in order to be
    ! compitable with the redox sequence

    !(1 - frac_avail_DON) counts fraction available for minerasilation bybacteria
    !frac_avail_DON - fraction available for cyanobacteria

    call CALCULATE_PH_CORR &
         (PH_CORR_DON_MIN_DOXY, PH, PH_MIN_DON_MIN_DOXY, PH_MAX_DON_MIN_DOXY, nkn)

    call CALCULATE_PH_CORR &
         (PH_CORR_DON_MIN_NO3N, PH, PH_MIN_DON_MIN_NO3N, PH_MAX_DON_MIN_NO3N, nkn)

    where(DISS_ORG_N .le. 0.D0)
        LIM_DON_DON = 0.D0
    elsewhere
        LIM_DON_DON = DISS_ORG_N / (DISS_ORG_N + K_HS_DON_MIN_DOXY)
    end where

    R_ABIOTIC_DON_MIN_DOXY = &
        (K_MIN_DON_DOXY_20 + LIM_PHY_N_AMIN_DON) * &
        (THETA_K_MIN_DON_DOXY ** (TEMP - 2.0D1)) * &
        LIM_DOXY_RED * PH_CORR_DON_MIN_DOXY * &
        LIM_DON_DON * &
        DISS_ORG_N !(1.0D0 - frac_avail_DON)

    ! No phytoplankton or cyanobacteria when there is no oxygen so mineralization
    ! rate calculation differs
    R_ABIOTIC_DON_MIN_NO3N = &
        K_MIN_DON_NO3N_20  * (THETA_K_MIN_DON_NO3N ** (TEMP - 2.0D1)) * &
        LIM_NO3N_RED * PH_CORR_DON_MIN_NO3N * &
        (DISS_ORG_N / (DISS_ORG_N + K_HS_DON_MIN_NO3N)) * DISS_ORG_N

    R_ABIOTIC_DON_MIN_MN_IV    = 0.0D0
    R_ABIOTIC_DON_MIN_FE_III   = 0.0D0
    R_ABIOTIC_DON_MIN_S_PLUS_6 = 0.0D0
    R_ABIOTIC_DON_MIN_DOC      = 0.0D0

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then

        call CALCULATE_PH_CORR &
             (PH_CORR_DON_MIN_MN_IV   , PH, PH_MIN_DON_MIN_MN_IV   , &
              PH_MAX_DON_MIN_MN_IV   , nkn)

        call CALCULATE_PH_CORR &
             (PH_CORR_DON_MIN_FE_III  , PH, PH_MIN_DON_MIN_FE_III  , &
              PH_MAX_DON_MIN_FE_III  , nkn)

        call CALCULATE_PH_CORR &
             (PH_CORR_DON_MIN_S_PLUS_6, PH, PH_MIN_DON_MIN_S_PLUS_6, &
              PH_MAX_DON_MIN_S_PLUS_6, nkn)

        call CALCULATE_PH_CORR &
             (PH_CORR_DON_MIN_DOC     , PH, PH_MIN_DON_MIN_DOC     , &
              PH_MAX_DON_MIN_DOC     , nkn)

        R_ABIOTIC_DON_MIN_MN_IV   = &
            K_MIN_DON_MN_IV_20     * (THETA_K_MIN_DON_MN_IV    ** (TEMP - 2.0D1)) * &
            LIM_MN_IV_RED * PH_CORR_DON_MIN_MN_IV * &
            (DISS_ORG_N / (DISS_ORG_N + K_HS_DON_MIN_MN_IV)) * DISS_ORG_N

        R_ABIOTIC_DON_MIN_FE_III   = &
            K_MIN_DON_FE_III_20    * (THETA_K_MIN_DON_FE_III   ** (TEMP - 2.0D1)) * &
            LIM_FE_III_RED * PH_CORR_DON_MIN_FE_III * &
            (DISS_ORG_N / (DISS_ORG_N + K_HS_DON_MIN_FE_III)) * DISS_ORG_N

        R_ABIOTIC_DON_MIN_S_PLUS_6 = &
            K_MIN_DON_S_PLUS_6_20  * (THETA_K_MIN_DON_S_PLUS_6 ** (TEMP - 2.0D1)) * &
            LIM_S_PLUS_6_RED * PH_CORR_DON_MIN_S_PLUS_6 * &
            (DISS_ORG_N / (DISS_ORG_N + K_HS_DON_MIN_S_PLUS_6)) * DISS_ORG_N

        R_ABIOTIC_DON_MIN_DOC      = &
            (K_MIN_DON_DOC_20      * (THETA_K_MIN_DON_DOC      ** (TEMP - 2.0D1)) * &
            LIM_DOC_RED * PH_CORR_DON_MIN_DOC * &
            (DISS_ORG_N / (DISS_ORG_N + K_HS_DON_MIN_DOC)) * DISS_ORG_N)
    end if


    ! -------------------------------------------------------------------------
    ! End of DON mineralization compatible with redox cycle
    ! -------------------------------------------------------------------------

    ! Accerelation of mineralisation when DIP is scarce
    LIM_P_AMIN_DOP = KHS_AMIN_P / (KHS_AMIN_P + DIP_OVER_IP*PO4_P)



    where (PHYT_TOT_C .gt. K_MIN_PHYT_AMIN_DOP)
        LIM_PHY_P_AMIN_DOP = LIM_P_AMIN_DOP * FAC_PHYT_AMIN_DOP * (PHYT_TOT_C - K_MIN_PHYT_AMIN_DOP)
    elsewhere
        LIM_PHY_P_AMIN_DOP = 0.D0
    end where

    ! -------------------------------------------------------------------------
    ! DOP mineralization compatible with redox cycle
    ! -------------------------------------------------------------------------

    call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_DOXY    , PH, PH_MIN_DOP_MIN_DOXY    , PH_MAX_DOP_MIN_DOXY    , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_NO3N    , PH, PH_MIN_DOP_MIN_NO3N    , PH_MAX_DOP_MIN_NO3N    , nkn)

    where (DISS_ORG_P .le. 0.D0 .and. K_HS_DOP_MIN_DOXY .le. 0.D0)
        LIM_DISS_ORG_P = 1.D0
    elsewhere
        LIM_DISS_ORG_P = (DISS_ORG_P / (DISS_ORG_P + K_HS_DOP_MIN_DOXY))
    end where

    if(any(DISS_ORG_P .le. 0.D0)) then
     print *, 'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW'
     print *, 'PELAGIC MODEL: Warning, some DISS_ORG_P <= 0'
     print *, 'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW'
    end if

    R_ABIOTIC_DOP_MIN_DOXY = &
        (K_MIN_DOP_DOXY_20 + LIM_PHY_P_AMIN_DOP) * (THETA_K_MIN_DOP_DOXY ** (TEMP - 2.0D1)) * &
        LIM_DOXY_RED * PH_CORR_DOP_MIN_DOXY * LIM_DISS_ORG_P * &
        DISS_ORG_P

    ! No phytoplankton or cyanobacteria when there is no oxygen so mineralization
    ! rate calculation differs
    R_ABIOTIC_DOP_MIN_NO3N = &
        K_MIN_DOP_NO3N_20  * (THETA_K_MIN_DOP_NO3N ** (TEMP - 2.0D1)) * &
        LIM_NO3N_RED * PH_CORR_DOP_MIN_NO3N * (DISS_ORG_P / (DISS_ORG_P + K_HS_DOP_MIN_NO3N)) * &
        DISS_ORG_P

    R_ABIOTIC_DOP_MIN_MN_IV    = 0.0D0
    R_ABIOTIC_DOP_MIN_FE_III   = 0.0D0
    R_ABIOTIC_DOP_MIN_S_PLUS_6 = 0.0D0
    R_ABIOTIC_DOP_MIN_DOC      = 0.0D0

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then
        call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_MN_IV   , PH, PH_MIN_DOP_MIN_MN_IV   , PH_MAX_DOP_MIN_MN_IV   , nkn)
        call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_FE_III  , PH, PH_MIN_DOP_MIN_FE_III  , PH_MAX_DOP_MIN_FE_III  , nkn)
        call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_S_PLUS_6, PH, PH_MIN_DOP_MIN_S_PLUS_6, PH_MAX_DOP_MIN_S_PLUS_6, nkn)
        call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_DOC     , PH, PH_MIN_DOP_MIN_DOC     , PH_MAX_DOP_MIN_DOC     , nkn)

        R_ABIOTIC_DOP_MIN_MN_IV = &
            K_MIN_DOP_MN_IV_20  * (THETA_K_MIN_DOP_MN_IV ** (TEMP - 2.0D1)) * &
            LIM_MN_IV_RED * PH_CORR_DOP_MIN_MN_IV * &
            (DISS_ORG_P / (DISS_ORG_P + K_HS_DOP_MIN_MN_IV)) * DISS_ORG_P

        R_ABIOTIC_DOP_MIN_FE_III = &
            K_MIN_DOP_FE_III_20  * (THETA_K_MIN_DOP_FE_III ** (TEMP - 2.0D1)) * &
            LIM_FE_III_RED * PH_CORR_DOP_MIN_FE_III * &
            (DISS_ORG_P / (DISS_ORG_P + K_HS_DOP_MIN_FE_III)) * DISS_ORG_P

        R_ABIOTIC_DOP_MIN_S_PLUS_6 = &
            K_MIN_DOP_S_PLUS_6_20  * (THETA_K_MIN_DOP_S_PLUS_6 ** (TEMP - 2.0D1)) * &
            LIM_S_PLUS_6_RED * PH_CORR_DOP_MIN_S_PLUS_6 * &
            (DISS_ORG_P / (DISS_ORG_P + K_HS_DOP_MIN_S_PLUS_6)) * DISS_ORG_P

        R_ABIOTIC_DOP_MIN_DOC = &
            (K_MIN_DOP_DOC_20  * (THETA_K_MIN_DOP_DOC ** (TEMP - 2.0D1)) * &
             LIM_DOC_RED * PH_CORR_DOP_MIN_DOC * &
            (DISS_ORG_P / (DISS_ORG_P + K_HS_DOP_MIN_DOC)) * DISS_ORG_P)
    end if
    ! -------------------------------------------------------------------------
    ! End of DOP mineralization compatible with redox cycle
    ! -------------------------------------------------------------------------

    !*******************************************************************************************!
    !     Nitrification of ammonia by bacteria are not modelled.
    !     Called abiotic in the sense of modelling method
    !*******************************************************************************************!
    LIM_NITR_OXY = DISS_OXYGEN / (KHS_NITR_OXY + DISS_OXYGEN)
    LIM_NITR_NH4_N = NH4_N / (KHS_NITR_NH4_N + NH4_N)

    call CALCULATE_PH_CORR(PH_CORR_NITR_NH4, PH, PH_NITR_NH4_MIN, PH_NITR_NH4_MAX, nkn)

    R_ABIOTIC_NITR = K_NITR_20 * LIM_NITR_OXY * LIM_NITR_NH4_N * &
                     PH_CORR_NITR_NH4 * (THETA_K_NITR ** (TEMP - 2.0D1)) * NH4_N

    ! -------------------------------------------------------------------------
    ! DENITRIFICATION
    ! -------------------------------------------------------------------------

    ! Introduced 28 January 2016 by Ali
    !
    ! [CH2O] + 4/5[NO3N-] -----> 1/2[N2] + [CO2]
    !
    ! Therefore, for each gram of DOC, that is mineralized over denitrification
    ! process,
    !
    !  - 14/(12 * 1.25) = 0.93 grams of NO3N is converted to N2
    !  - 1 gram of carbondioxide carbon is produced

    R_DENITRIFICATION = 0.93D0 * R_ABIOTIC_DOC_MIN_NO3N
    ! -------------------------------------------------------------------------
    ! END OF DENITRIFICATION
    ! -------------------------------------------------------------------------


    ! -------------------------------------------------------------------------
    ! MANGANESE REDUCTION
    ! -------------------------------------------------------------------------

    ! Introduced 29 January 2016 by Ali
    !
    ! [CH2O] + 2[MN_IV] -----> [CO2] + 2[MN_II]
    !
    ! Therefore, for each gram of DOC, that is mineralized over denitrification
    ! process,
    !
    !  - (2*52)/12 = 8.66 grams of manganese IV is reduced to manganese II
    !  - 1 gram of carbondioxide carbon is produced

    R_MN_IV_REDUCTION = 8.66D0 * R_ABIOTIC_DOC_MIN_MN_IV
    ! -------------------------------------------------------------------------
    ! END OF MANGANESE REDUCTION
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! IRON REDUCTION
    ! -------------------------------------------------------------------------

    ! Introduced 29 January 2016 by Ali
    !
    ! [CH2O] + 4[FE_III] -----> [CO2] + 4[FE_II]
    !
    ! Therefore, for each gram of DOC, that is mineralized over denitrification
    ! process,
    !
    !  - (4*56)/12 = 18.66 grams of iron III is reduced to iron II
    !  - 1 gram of carbondioxide carbon is produced

    R_FE_III_REDUCTION = 18.66D0 * R_ABIOTIC_DOC_MIN_FE_III
    ! -------------------------------------------------------------------------
    ! END OF IRON REDUCTION
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! SULPHATE REDUCTION
    ! -------------------------------------------------------------------------

    ! Introduced 28 January 2016 by Ali
    !
    ! [CH2O] + 1/2[SO4--] -----> [CO2] + 1/2[S--]
    !
    ! Therefore, for each gram of DOC, that is mineralized over denitrification
    ! process,
    !
    !  - (32/2)/12 = 1.33 grams of S_PLUS_6 is converted to S_MINUS_2
    !  - 1 gram of carbondioxide is produced

    R_SULPHATE_REDUCTION = 1.33D0 * R_ABIOTIC_DOC_MIN_S_PLUS_6
    ! -------------------------------------------------------------------------
    ! END OF SULPHATE REDUCTION
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! METHANOGENESIS
    ! -------------------------------------------------------------------------

    ! Introduced 29 January 2016 by Ali
    !
    ! [CH2O]  -----> 1/2[CH4] + 1/2[CO2]
    !
    ! Therefore, for each gram of DOC, that is mineralized over denitrification
    ! process,
    !
    !  - 0.5 grams of methane carbon is produced from DON
    !  - 0.5 grams of carbondioxide carbon is produced

    R_METHANOGENESIS = 0.5D0 * R_ABIOTIC_DOC_MIN_DOC
    ! -------------------------------------------------------------------------
    ! END OF METHANOGENESIS
    ! -------------------------------------------------------------------------

    !*********************************************************************!
    !     VOLATILIZATION OF UNIONIZED AMMONI.
    !*********************************************************************!
    call AMMONIA_VOLATILIZATION(R_AMMONIA_VOLATIL, NH4_N, pH, TEMP, K_A_CALC,nkn)

    !----------------------------------------------------------------------
    ! 2 February 2015
    ! New code added to account the effect of ice cover.
    !----------------------------------------------------------------------
    R_AMMONIA_VOLATIL = R_AMMONIA_VOLATIL * (1.0D0 - ice_cover)

    where (R_AMMONIA_VOLATIL < 0.0D0)
        R_AMMONIA_VOLATIL = 0.0D0
    end where

    !----------------------------------------------------------------------
    ! End of new code added to account the effect of ice cover.
    !----------------------------------------------------------------------

    ! ---------------------------------------------------------------------
    ! Changes by Ali Ert�rk, 6 th of July 2016
    !
    ! Following lines are commented
    ! ---------------------------------------------------------------------

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then
        ! New kinetic rate calculations added 9 September 2015
        ! For now, no temparature corrections. Effect on temperature and other
        ! environmental conditions may be included after more detailed investigations

        ! Updated in 25th January 2016 where only dissolved fractions are allowed to be oxidized or reduced

        ! Iron
        if(iron_oxidation .eq. 0) then
            !simple formulation, k_OX_FE_II is calibration parameter
            where (DISS_OXYGEN < 1)
                R_FE_II_OXIDATION  = k_OX_FE_II * DISS_OXYGEN * (10.0D0 ** (PH - 7.0D0)) * FE_II
            elsewhere
                R_FE_II_OXIDATION  = k_OX_FE_II * (10.0D0 ** (PH - 7.0D0)) * FE_II
            end where
        end if

        if(iron_oxidation .eq. 1) then
            ! In the future include several options for heavy metal oxidation and possibly reduction
            ! Morgen and Lahav (2007) formulation. No calibration
            call IRON_II_OXIDATION(FE_II_DISS, DISS_OXYGEN, PH, TEMP, SALT, ELEVATION, nkn, R_FE_II_OXIDATION)
            ! ---------------------------------------------------------------------
            ! End of changes by Ali Ert�rk, 6 th of July 2016
            ! ---------------------------------------------------------------------
        end if
        ! 29 January 2016
        ! Following commented lines are replaced by the new redox sequence based DOC
        ! DOC mineralization it is the next visit of Ali and the redox sequences as described
        ! by Van Chappen and Wang 2015 and Katsev papers are now included.

        ! Manganese
        where (DISS_OXYGEN < 1)
            R_MN_II_OXIDATION  = k_OX_MN_II * DISS_OXYGEN * (10.0D0 ** (PH - 7.0D0))* MN_II
        elsewhere
            R_MN_II_OXIDATION  = k_OX_MN_II * (10.0D0 ** (PH - 7.0D0)) * MN_II
        end where

        ! 29 January 2016
        ! Following commented lines are replaced by the new redox sequence based DOC
        ! mineralization it is the next visit of Ali and the redox sequences as described
        ! by Van Chappen and Wang 2015 and Katsev papers are now included.

        ! End of new kinetic rate calculations added 9 September 2015

        ! -------------------------------------------------------------------------------
        ! 29 January 2016 KINETICS OF NEW STATE VARIABLES
        ! -------------------------------------------------------------------------------
        K_A_CH4 = K_A_CALC * 1.188D0
        K_A_H2S = K_A_CALC * 0.984D0
        CH4_SAT = 0.0D0 ! Assume that no methane is present in the atmosphere
        H2S_SAT = 0.0D0 ! Assume that no H2S is present in the atmosphere

        CH4_ATM_EXCHANGE = K_A_CH4 * (CH4_SAT - CH4_C)
        H2S_ATM_EXCHANGE = K_A_H2S * (H2S_SAT - (H2S * 32000.D0))

        R_METHANE_OXIDATION = &
            k_OX_CH4 * (THETA_k_OX_CH4 ** (TEMP - 20.0D0)) * CH4_C * &
            (DISS_OXYGEN / (k_HS_OX_CH4_DOXY + DISS_OXYGEN))

        R_SULPHIDE_OXIDATION = &
            k_OX_H2S * (THETA_k_OX_H2S ** (TEMP - 20.0D0)) * S_MINUS_2 * &
            (DISS_OXYGEN / (k_HS_OX_H2S_DOXY + DISS_OXYGEN))
    else
        R_FE_II_OXIDATION    = 0.0D0
        R_MN_II_OXIDATION    = 0.0D0
        R_SULPHIDE_OXIDATION = 0.0D0
        R_METHANE_OXIDATION  = 0.0D0
    end if
    ! -------------------------------------------------------------------------
    ! END OF KINETICS OF NEW STATE VARIABLES
    ! -------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------------
    ! Final calculation of derivatives
    !------------------------------------------------------------------------------------------------

    !AMMONIA NITROGEN
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 1) = R_DIA_TOT_RESP * DIA_N_TO_C
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 2) = R_CYN_TOT_RESP * CYN_N_TO_C
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 3) = R_OPA_TOT_RESP * OPA_N_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 4) = R_FIX_CYN_TOT_RESP * FIX_CYN_N_TO_C
    else
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,NH4_N_INDEX, 5) = R_ZOO_TOT_RESP * ACTUAL_ZOO_N_TO_C
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 6) = R_DIA_GROWTH   * PREF_NH4N_DIA * DIA_N_TO_C

    PROCESS_RATES(1:nkn,NH4_N_INDEX, 7) = R_CYN_GROWTH * CYN_N_TO_C * PREF_DIN_DON_CYN * PREF_NH4N_CYN

    PROCESS_RATES(1:nkn,NH4_N_INDEX, 8) = R_OPA_GROWTH * PREF_NH4N_OPA * OPA_N_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 9) = &
            R_NON_FIX_CYN_GROWTH * PREF_NH4N_DON_FIX_CYN * FIX_CYN_N_TO_C * &
            (NH4_N / max(NH4_N + (DISS_ORG_N * frac_avail_DON), 1.0D-10))
    else
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 9)  = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 23) = &
            R_NOST_VEG_HET_NON_FIX_GROWTH * NOST_N_TO_C * PREF_DIN_DON_NOST * PREF_NH4N_NOST

        !NH4 production during N-fixation
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 22) = &
            R_NOST_VEG_HET_FIX_GROWTH * NOST_N_TO_C * &
            ((1.D0 - FRAC_FIX_N_FOR_GR_VEG_HET)/FRAC_FIX_N_FOR_GR_VEG_HET)
    else
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 22) = 0.0D0
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 23) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,NH4_N_INDEX, 10) = R_ABIOTIC_NITR

    PROCESS_RATES(1:nkn,NH4_N_INDEX, 11) = &
        R_ABIOTIC_DON_MIN_DOXY   + R_ABIOTIC_DON_MIN_NO3N     + R_ABIOTIC_DON_MIN_MN_IV + &
        R_ABIOTIC_DON_MIN_FE_III + R_ABIOTIC_DON_MIN_S_PLUS_6 + R_ABIOTIC_DON_MIN_DOC

    PROCESS_RATES(1:nkn,NH4_N_INDEX, 12) = R_AMMONIA_VOLATIL

    ! Auxiliary
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 13) = PREF_NH4N_DIA
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 14) = 0.0D0  ! Old PREF_NH4N_DON_CYN deprecated
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 15) = PREF_NH4N_OPA
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 16) = PREF_NH4N_DON_FIX_CYN
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 18) = PREF_NH4N_CYN
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 19) = PREF_DIN_DON_CYN
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 20) = PREF_NH4N_NOST
    PROCESS_RATES(1:nkn,NH4_N_INDEX, 21) = PREF_DIN_DON_NOST

    ! New process information incorporated 10 September 2019
    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 17) = R_NOST_VEG_HET_TOT_RESP * NOST_N_TO_C
    else
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 17) = 0.0D0
    end if
    ! End of new process information incorporated 10 September 2019

    DERIVATIVES(1:nkn,NH4_N_INDEX) = &
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 1)  + PROCESS_RATES(1:nkn,NH4_N_INDEX, 2)  + &
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 3)  + PROCESS_RATES(1:nkn,NH4_N_INDEX, 4)  + &
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 5)  - PROCESS_RATES(1:nkn,NH4_N_INDEX, 6)  - &
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 7)  - PROCESS_RATES(1:nkn,NH4_N_INDEX, 8)  - &
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 9)  - PROCESS_RATES(1:nkn,NH4_N_INDEX, 10) + &
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 11) - PROCESS_RATES(1:nkn,NH4_N_INDEX, 12) + &
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 17) + PROCESS_RATES(1:nkn,NH4_N_INDEX, 22) - &
        PROCESS_RATES(1:nkn,NH4_N_INDEX, 23)

    ! Code to debug NH4N
    if(debug_stranger) then
        call DBGSTR_PEL_NH4N_01&
             (PROCESS_RATES, DERIVATIVES, TIME, nkn, nstate, NDIAGVAR, node_active, error)
    end if

    !NITRATE NITROGEN
    PROCESS_RATES(1:nkn,NO3_N_INDEX, 1) = R_ABIOTIC_NITR
    PROCESS_RATES(1:nkn,NO3_N_INDEX, 2) = R_DENITRIFICATION

    PROCESS_RATES(1:nkn,NO3_N_INDEX, 3) = &
        R_DIA_GROWTH * (1.0D0 - PREF_NH4N_DIA) * DIA_N_TO_C

    PROCESS_RATES(1:nkn,NO3_N_INDEX, 4) = &
        R_CYN_GROWTH * CYN_N_TO_C * PREF_DIN_DON_CYN * (1.0D0 - PREF_NH4N_CYN)

    PROCESS_RATES(1:nkn,NO3_N_INDEX, 5) = R_OPA_GROWTH         * (1.0D0 - PREF_NH4N_OPA)         * OPA_N_TO_C

    if (DO_NOSTOCALES > 0) then
      PROCESS_RATES(1:nkn,NO3_N_INDEX, 6) = &
          R_NOST_VEG_HET_NON_FIX_GROWTH * NOST_N_TO_C * PREF_DIN_DON_NOST * (1-PREF_NH4N_NOST)
    else
      PROCESS_RATES(1:nkn,NO3_N_INDEX, 6) = &
          R_NON_FIX_CYN_GROWTH * (1.0D0 - PREF_NH4N_DON_FIX_CYN) * FIX_CYN_N_TO_C
    end if


    ! Auxiliary
    PROCESS_RATES(1:nkn,NO3_N_INDEX, 7) = PREF_NH4N_DIA
    PROCESS_RATES(1:nkn,NO3_N_INDEX, 8) = 0.0D0  ! Old PREF_NH4N_DON_CYN deprecated
    PROCESS_RATES(1:nkn,NO3_N_INDEX, 9) = PREF_NH4N_OPA

    DERIVATIVES(1:nkn,NO3_N_INDEX) = &
        PROCESS_RATES(1:nkn,NO3_N_INDEX, 1) - PROCESS_RATES(1:nkn,NO3_N_INDEX, 2) - &
        PROCESS_RATES(1:nkn,NO3_N_INDEX, 3) - PROCESS_RATES(1:nkn,NO3_N_INDEX, 4) - &
        PROCESS_RATES(1:nkn,NO3_N_INDEX, 5) - PROCESS_RATES(1:nkn,NO3_N_INDEX, 6)

    !PHOSPHATE PHOSPHORUS
    PROCESS_RATES(1:nkn,PO4_P_INDEX, 1) = R_DIA_TOT_RESP * DIA_P_TO_C
    PROCESS_RATES(1:nkn,PO4_P_INDEX, 2) = R_CYN_TOT_RESP * CYN_P_TO_C
    PROCESS_RATES(1:nkn,PO4_P_INDEX, 3) = R_OPA_TOT_RESP * OPA_P_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 4) = R_FIX_CYN_TOT_RESP * FIX_CYN_P_TO_C
    else
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,PO4_P_INDEX, 5) = R_ZOO_TOT_RESP * ACTUAL_ZOO_P_TO_C
    PROCESS_RATES(1:nkn,PO4_P_INDEX, 6) = R_DIA_GROWTH   * DIA_P_TO_C
    PROCESS_RATES(1:nkn,PO4_P_INDEX, 7) = R_CYN_GROWTH   * CYN_P_TO_C
    PROCESS_RATES(1:nkn,PO4_P_INDEX, 8) = R_OPA_GROWTH   * OPA_P_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 9) = R_FIX_CYN_GROWTH * FIX_CYN_P_TO_C
    else
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 9) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,PO4_P_INDEX, 10) = &
        R_ABIOTIC_DOP_MIN_DOXY   + R_ABIOTIC_DOP_MIN_NO3N     + R_ABIOTIC_DOP_MIN_MN_IV + &
        R_ABIOTIC_DOP_MIN_FE_III + R_ABIOTIC_DOP_MIN_S_PLUS_6 + R_ABIOTIC_DOP_MIN_DOC

    ! Auxiliary
    PROCESS_RATES(1:nkn,PO4_P_INDEX, 11) = TEMP
    PROCESS_RATES(1:nkn,PO4_P_INDEX, 12) = DISS_ORG_P

    ! New process information incorporated 10 September 2019
    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 13) = R_NOST_VEG_HET_TOT_RESP * NOST_P_TO_C

        PROCESS_RATES(1:nkn,PO4_P_INDEX, 14) = &
            R_NOST_VEG_HET_GROWTH   * NOST_P_TO_C * PREF_DIP_DOP_NOST
    else
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 13) = 0.0D0
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 14) = 0.0D0
    end if
    ! End of new process information incorporated 10 September 2019

    DERIVATIVES(1:nkn,PO4_P_INDEX) = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 1 ) + PROCESS_RATES(1:nkn,PO4_P_INDEX, 2 ) + &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 3 ) + PROCESS_RATES(1:nkn,PO4_P_INDEX, 4 ) + &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 5 ) - PROCESS_RATES(1:nkn,PO4_P_INDEX, 6 ) - &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 7 ) - PROCESS_RATES(1:nkn,PO4_P_INDEX, 8 ) - &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 9 ) + PROCESS_RATES(1:nkn,PO4_P_INDEX, 10) + &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 13) - PROCESS_RATES(1:nkn,PO4_P_INDEX, 14)

    ! Debug for PO4P
    if(debug_stranger) then
        call DBGSTR_PEL_PO4P_01&
             (PROCESS_RATES, DERIVATIVES, TIME, nkn, nstate, NDIAGVAR, node_active, error)
    end if

    !DISSOLVED SILICA SILICON
    PROCESS_RATES(1:nkn,DISS_Si_INDEX, 1) = R_PART_Si_DISS

    !Should it come from respiration? Silica is in shell.

    !Physiologically not but respiration is a process which decreases the diatom biomass.
    !So in order to keep the model mass conserving, respiration should set stochimetrically
    !correct amount of silicon in dissolved form free. Because of the same reason, excrtion
    !t
    PROCESS_RATES(1:nkn,DISS_Si_INDEX, 2) = R_DIA_TOT_RESP * DIA_SI_TO_C


    PROCESS_RATES(1:nkn,DISS_Si_INDEX, 3) = R_DIA_EXCR * DIA_SI_TO_C

    PROCESS_RATES(1:nkn,DISS_Si_INDEX, 4) = R_DIA_GROWTH   * DIA_SI_TO_C

    DERIVATIVES(1:nkn,DISS_Si_INDEX) = &
        PROCESS_RATES(1:nkn,DISS_Si_INDEX, 1) + PROCESS_RATES(1:nkn,DISS_Si_INDEX, 2) + &
        PROCESS_RATES(1:nkn,DISS_Si_INDEX, 3) - PROCESS_RATES(1:nkn,DISS_Si_INDEX, 4)

    !DISSOLVED OXYGEN
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 1)  = R_AERATION

    ! formulation from EFDC
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 2)  = &
        R_DIA_GROWTH * (1.3D0 - 0.3D0*PREF_NH4N_DIA)    * DIA_O2_TO_C

    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 3)  = &
        R_CYN_GROWTH * (1.3D0 - 0.3D0 * PREF_NH4N_CYN * PREF_DIN_DON_CYN) * CYN_O2_TO_C

    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 4) = &
        R_OPA_GROWTH  * (1.3D0 - 0.3D0*PREF_NH4N_OPA) * OPA_O2_TO_C

    if(DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 5)= &
            R_NOST_VEG_HET_GROWTH * &
            NOST_O2_TO_C * (1.3D0 - 0.3D0*PREF_NH4N_NOST*PREF_DIN_DON_NOST)
    else
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 5)  = &
            R_FIX_CYN_GROWTH   * (1.3D0 - 0.3D0*PREF_NH4N_DON_FIX_CYN)* FIX_CYN_O2_TO_C
    endif

    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 6)  = R_DIA_TOT_RESP * DIA_O2_TO_C
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 7)  = R_CYN_TOT_RESP * CYN_O2_TO_C
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 8)  = R_OPA_TOT_RESP * OPA_O2_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 9) = R_FIX_CYN_TOT_RESP * FIX_CYN_O2_TO_C
    else
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 9) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 10) = R_ZOO_TOT_RESP * ZOO_O2_TO_C
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 11) = R_ABIOTIC_NITR * 4.57D0
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 12) = 2.66D0 * R_ABIOTIC_DOC_MIN_DOXY
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 13) = 0.43D0 * R_FE_II_OXIDATION
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 14) = 0.88D0 * R_MN_II_OXIDATION
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 15) = 2.00D0 * R_SULPHIDE_OXIDATION
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 16) = 5.33D0 * R_METHANE_OXIDATION

    ! Nostocales O2 production is already accounted in index 5 (EFDC-corrected).
    ! Index 19 is reserved for diagnostic/auxiliary use only — do NOT add to derivative.
    ! Nostocales respiration O2 is accounted via index 20 (only sink, not in index 8/9).
    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 19) = 0.0D0
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 20) = R_NOST_VEG_HET_TOT_RESP * NOST_O2_TO_C
    else
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 19) = 0.0D0
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 20) = 0.0D0
    end if

    ! Auxiliary
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 17) = K_A_CALC
    PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 18) = DISS_OXYGEN_SAT

    DERIVATIVES(1:nkn,DISS_OXYGEN_INDEX) = &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 1)  + &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 2)  + &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 3)  + &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 4)  + &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 5)  - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 6)  - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 7)  - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 8)  - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 9)  - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 10) - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 11) - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 12) - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 13) - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 14) - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 15) - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 16) + &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 19) - &
        PROCESS_RATES(1:nkn,DISS_OXYGEN_INDEX, 20)

    !Debug code for dissolved oxygen
    if(debug_stranger) then
        call DBGSTR_PEL_DOXY_01&
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if

    !DIATOMS CARBON
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 1)  = R_DIA_GROWTH
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 2)  = R_DIA_TOT_RESP
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 3)  = R_DIA_EXCR
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 4)  = R_DIA_DEATH
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 5)  = R_ZOO_FEEDING_DIA
    ! Auxiliary
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 6)  = LIM_KG_DIA_TEMP
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 7)  = LIM_KG_DIA_DOXY
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 8)  = LIM_KG_DIA_N
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 9)  = LIM_KG_DIA_P
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 10) = LIM_KG_DIA_DISS_Si
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 11) = LIM_KG_DIA_LIGHT
    PROCESS_RATES(1:nkn,DIA_C_INDEX, 12) = DIA_LIGHT_SAT

    DERIVATIVES(1:nkn,DIA_C_INDEX) = &
        PROCESS_RATES(1:nkn,DIA_C_INDEX, 1) - PROCESS_RATES(1:nkn,DIA_C_INDEX, 2) - &
        PROCESS_RATES(1:nkn,DIA_C_INDEX, 3) - PROCESS_RATES(1:nkn,DIA_C_INDEX, 4) - &
        PROCESS_RATES(1:nkn,DIA_C_INDEX, 5)

    !NON-NITROGEN FIXING CYANOBACTERIA CARBON
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 1)  = R_CYN_GROWTH
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 2)  = R_CYN_TOT_RESP
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 3)  = R_CYN_EXCR
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 4)  = R_CYN_DEATH
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 5)  = R_ZOO_FEEDING_CYN
    ! Auxiliary
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 6)  = LIM_KG_CYN_TEMP
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 7)  = LIM_KG_CYN_DOXY
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 8)  = LIM_KG_CYN_N
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 9)  = LIM_KG_CYN_P
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 10) = LIM_KG_CYN_LIGHT
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 11) = I_A             !light langlays
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 12) = CYN_LIGHT_SAT
    PROCESS_RATES(1:nkn,CYN_C_INDEX, 13) = TEMP

    DERIVATIVES(1:nkn,CYN_C_INDEX) = &
        PROCESS_RATES(1:nkn,CYN_C_INDEX, 1) - PROCESS_RATES(1:nkn,CYN_C_INDEX, 2) - &
        PROCESS_RATES(1:nkn,CYN_C_INDEX, 3) - PROCESS_RATES(1:nkn,CYN_C_INDEX, 4) - &
                PROCESS_RATES(1:nkn,CYN_C_INDEX, 5)

    ! Debug: print derivatives for CYN_C node1 (commented to reduce log noise)
    ! write(6,'(A,ES14.6)') 'DEBUG: DERIV CYN node1=', DERIVATIVES(1,CYN_C_INDEX)

    ! Debug code for CYN_C
    if(debug_stranger) then
        call DBGSTR_PEL_CYNC_01 &
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if

    !NITROGEN FIXING CYANOBACTERIA CARBON
    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 1)  = R_FIX_CYN_GROWTH
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 2)  = R_FIX_CYN_TOT_RESP
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 3)  = R_FIX_CYN_EXCR
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 4)  = R_FIX_CYN_DEATH
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 5)  = R_ZOO_FEEDING_FIX_CYN

        ! Auxiliary
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 6)  = R_NON_FIX_CYN_GROWTH
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 7)  = R_FIX_FIX_CYN_GROWTH

        !Nitrogen fixation
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 8)  = &
            (R_FIX_FIX_CYN_GROWTH  * FIX_CYN_N_TO_C)

        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 9)  = LIM_KG_FIX_CYN_TEMP
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 10) = LIM_KG_FIX_CYN_DOXY
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 11) = LIM_KG_FIX_FIX_CYN_N
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 12) = LIM_KG_FIX_FIX_CYN_P
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 13) = LIM_KG_NON_FIX_CYN_N
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 14) = LIM_KG_NON_FIX_CYN_P
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 15) = LIM_KG_FIX_CYN_LIGHT

        ! NP molar ratio calculation with guard against division by zero
        ! When phosphorus is depleted, set ratio to very high value indicating P limitation
        where (DIP_OVER_IP * PO4_P .lt. 1.0D-10)
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 16) = 999.0D0  ! Extreme N:P indicates P limitation
        elsewhere
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 16) = &
                ((NH4_N + NO3_N)/14.D0)/(DIP_OVER_IP*PO4_P/31.D0)
        end where

        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 17) = NH4_N + NO3_N
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 18) = FIX_CYN_LIGHT_SAT

        DERIVATIVES(1:nkn,FIX_CYN_C_INDEX) = &
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 1) - PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 2) - &
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 3) - PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 4) - &
                PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 5)

        ! Diagnostic check: if applying derivative over TIME_STEP would make concentration negative, dump context and stop
        do i = 1, nkn
            if (FIX_CYN_C(i) + DERIVATIVES(i,FIX_CYN_C_INDEX) * TIME_STEP < 0.0D0) then
                write(6,'(A,F12.4,A,ES12.4,A,I4)') &
                    'ERROR: PREDICTED NEGATIVE FIX_CYN_C: TIME=', TIME, ' DT=', TIME_STEP, ' BOX=', i
                write(6,'(A,ES14.6,A,ES14.6)') &
                    '  FIX_CYN_C=', FIX_CYN_C(i), '  DERIV=', DERIVATIVES(i,FIX_CYN_C_INDEX)
                write(6,'(A,6ES12.4)') '  PROCESS_RATES(1:6)=', PROCESS_RATES(i,FIX_CYN_C_INDEX,1:6)
                write(6,'(A,3ES14.6)') '  NH4_N/NO3_N/PO4_P=', NH4_N(i), NO3_N(i), PO4_P(i)
                stop
            end if

            ! New diagnostic: check actual process rates (indices 1-5) for unusually large values
            ! Note: Indices 6-18 are diagnostic values (limiters, ratios) not actual rates
            if (FIX_CYN_C(i) > 1.0D-12) then
                do k = 1, 5  ! Only check actual process rates, not diagnostic values
                    if (dabs(PROCESS_RATES(i,FIX_CYN_C_INDEX,k)) * TIME_STEP > 0.2D0 * FIX_CYN_C(i)) then
                        write(6,'(A,F12.4,A,ES12.4,A,I4)') &
                            'DEBUG: LARGE PROCESS ON FIX_CYN: TIME=', TIME, ' DT=', TIME_STEP, ' BOX=', i
                        write(6,'(A,ES14.6,A,I3,A,ES14.6)') &
                            '  FIX_CYN_C=', FIX_CYN_C(i), '  PROC_IDX=', k, '  RATE=', PROCESS_RATES(i,FIX_CYN_C_INDEX,k)
                        write(6,'(A,ES14.6,A,ES14.6)') &
                            '  DERIV_FIX=', DERIVATIVES(i,FIX_CYN_C_INDEX), '  DERIV_CYN=', DERIVATIVES(i,CYN_C_INDEX)
                        write(6,'(A,3ES14.6)') &
                            '  NH4_N/NO3_N/PO4_P=', NH4_N(i), NO3_N(i), PO4_P(i)

                        ! Safety clamp for single large process (only for actual rates 2-5)
                        if (k >= 2) then
                            allowed_rate_local = FIX_CYN_C(i) / max(TIME_STEP, 1.0D-12)
                            if (dabs(PROCESS_RATES(i,FIX_CYN_C_INDEX,k)) > allowed_rate_local) then
                                write(6,'(A)') '  Applying removal-limiter scaling...'
                                old_rate = PROCESS_RATES(i,FIX_CYN_C_INDEX,k)
                                PROCESS_RATES(i,FIX_CYN_C_INDEX,k) = sign(allowed_rate_local, PROCESS_RATES(i,FIX_CYN_C_INDEX,k))

                                ! Recompute derivative for this node (sum only actual removals 2-5)
                                sum_removals = PROCESS_RATES(i,FIX_CYN_C_INDEX,2) + &
                                               PROCESS_RATES(i,FIX_CYN_C_INDEX,3) + &
                                               PROCESS_RATES(i,FIX_CYN_C_INDEX,4) + &
                                               PROCESS_RATES(i,FIX_CYN_C_INDEX,5)
                                DERIVATIVES(i,FIX_CYN_C_INDEX) = PROCESS_RATES(i,FIX_CYN_C_INDEX,1) - sum_removals

                                ! Increment compact per-node and per-process clamp counters (summary printed once later)
                                if (.not. allocated(CLAMP_COUNT)) then
                                    allocate(CLAMP_COUNT(nkn))
                                    CLAMP_COUNT = 0
                                end if
                                CLAMP_COUNT(i) = CLAMP_COUNT(i) + 1
                                if (NDIAGVAR > 0) then
                                    if (.not. allocated(CLAMP_PROC_COUNT)) then
                                        allocate(CLAMP_PROC_COUNT(nkn, NDIAGVAR))
                                        CLAMP_PROC_COUNT = 0
                                    end if
                                    CLAMP_PROC_COUNT(i,k) = CLAMP_PROC_COUNT(i,k) + 1
                                end if
                            end if
                        end if
                    end if
                end do
            end if

            ! Removal limiter: ensure total instantaneous removal does not exceed available biomass in one TIME_STEP
            ! total removal = respiration + excretion + internal resp + death + grazing
            total_removal = R_FIX_CYN_TOT_RESP(i) + R_FIX_CYN_EXCR(i) + R_FIX_CYN_INT_RESP(i) + &
                           R_FIX_CYN_DEATH(i) + R_ZOO_FEEDING_FIX_CYN(i)
            if (total_removal > 0.0D0) then
                allowed_rate = FIX_CYN_C(i) / max(TIME_STEP, 1.0D-12)

                ! Log when total removal is a significant fraction of allowed rate to aid debugging
                if (total_removal > 0.1D0 * allowed_rate) then
                    write(6,'(A,F12.4,A,I4)') 'DEBUG: REMOVAL LIMITER CHECK: TIME=', TIME, ' BOX=', i
                    write(6,'(A,ES12.4,A,ES12.4,A,ES12.4)') &
                        '  TOT_REM=', total_removal, ' ALLOW=', allowed_rate, ' FIX_CYN_C=', FIX_CYN_C(i)
                    write(6,'(A,5ES11.3)') '  RESP/INT/EXCR/DEATH/GRAZ=', &
                        R_FIX_CYN_RESP(i), R_FIX_CYN_INT_RESP(i), R_FIX_CYN_EXCR(i), &
                        R_FIX_CYN_DEATH(i), R_ZOO_FEEDING_FIX_CYN(i)
                end if

                if (total_removal > allowed_rate) then
                    scale = allowed_rate / total_removal
                    ! Scale per-process arrays
                    R_FIX_CYN_RESP(i) = R_FIX_CYN_RESP(i) * scale
                    R_FIX_CYN_INT_RESP(i) = R_FIX_CYN_INT_RESP(i) * scale
                    R_FIX_CYN_TOT_RESP(i) = R_FIX_CYN_RESP(i) + R_FIX_CYN_INT_RESP(i)
                    R_FIX_CYN_EXCR(i) = R_FIX_CYN_EXCR(i) * scale
                    R_FIX_CYN_DEATH(i) = R_FIX_CYN_DEATH(i) * scale
                    R_ZOO_FEEDING_FIX_CYN(i) = R_ZOO_FEEDING_FIX_CYN(i) * scale

                    ! Update PROCESS_RATES entries to reflect scaled rates
                    PROCESS_RATES(i,FIX_CYN_C_INDEX,2) = R_FIX_CYN_TOT_RESP(i)
                    PROCESS_RATES(i,FIX_CYN_C_INDEX,3) = R_FIX_CYN_EXCR(i)
                    PROCESS_RATES(i,FIX_CYN_C_INDEX,4) = R_FIX_CYN_DEATH(i)
                    PROCESS_RATES(i,FIX_CYN_C_INDEX,5) = R_ZOO_FEEDING_FIX_CYN(i)

                    ! Update dependent process rates that were set earlier
                    PROCESS_RATES(i,NH4_N_INDEX,4) = R_FIX_CYN_TOT_RESP(i) * FIX_CYN_N_TO_C
                    PROCESS_RATES(i,PO4_P_INDEX,4) = R_FIX_CYN_TOT_RESP(i) * FIX_CYN_P_TO_C
                    PROCESS_RATES(i,DISS_OXYGEN_INDEX,9) = R_FIX_CYN_TOT_RESP(i) * FIX_CYN_O2_TO_C
                    PROCESS_RATES(i,DET_PART_ORG_C_INDEX,4) = R_FIX_CYN_DEATH(i)
                    PROCESS_RATES(i,DET_PART_ORG_N_INDEX,4) = R_FIX_CYN_DEATH(i) * FIX_CYN_N_TO_C
                    PROCESS_RATES(i,DET_PART_ORG_P_INDEX,4) = R_FIX_CYN_DEATH(i) * FIX_CYN_P_TO_C
                    PROCESS_RATES(i,DISS_ORG_C_INDEX,8) = R_FIX_CYN_EXCR(i)
                    PROCESS_RATES(i,DISS_ORG_N_INDEX,10) = R_FIX_CYN_EXCR(i) * FIX_CYN_N_TO_C
                    PROCESS_RATES(i,DISS_ORG_P_INDEX,8) = R_FIX_CYN_EXCR(i) * FIX_CYN_P_TO_C

                    ! Recompute derivative for this node
                    DERIVATIVES(i,FIX_CYN_C_INDEX) = PROCESS_RATES(i,FIX_CYN_C_INDEX,1) - &
                        PROCESS_RATES(i,FIX_CYN_C_INDEX,2) - PROCESS_RATES(i,FIX_CYN_C_INDEX,3) - &
                        PROCESS_RATES(i,FIX_CYN_C_INDEX,4) - PROCESS_RATES(i,FIX_CYN_C_INDEX,5)

                    write(6,'(A,F12.4,A,I4,A,ES10.3)') &
                        'WARN: Scaled FIX_CYN removal: TIME=', TIME, ' BOX=', i, ' SCALE=', scale
                    write(6,'(A,ES12.4,A,ES12.4)') &
                        '  TOT_REM_BEFORE=', total_removal, ' FIX_CYN_C=', FIX_CYN_C(i)
                end if
            end if
        end do
    else
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 1)  = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 2)  = 0.0D0
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 3)  = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 4)  = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 5)  = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 6)  = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 7)  = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 8)  = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 9)  = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 10) = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 11) = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 12) = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 13) = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 14) = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 15) = 0.0D0
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 16) = 0.0D0
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 17) = 0.0D0
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 18) = 0.0D0

        DERIVATIVES  (1:nkn,FIX_CYN_C_INDEX)     = 0.0D0
    end if

    ! Process rates for the new state variables incorporated 10 September 2019
    if (DO_NOSTOCALES > 0) then

        ! -------------------------------------------------------------------------------
        ! Nostaocacles in vegetative + heterocyst staged form
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 1) = R_NOST_VEG_HET_GROWTH
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 2) = R_NOST_VEG_HET_TOT_RESP
            PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 3) = R_NOST_VEG_HET_EXCR
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 4) = R_NOST_VEG_HET_DEATH
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 5) = R_ZOO_FEEDING_NOST_VEG_HET

        ! -------------------------------------------------------------------------------
        ! Akinetes before germination are located in the mud but internally still in
        ! units gC/m^3 though initial condition is given in gC/m^2 and coverted to gC/m^3
        ! In case of 3d they should rise with negative settling velocity
        ! to the WC nearbottom layer and next layers up, fixme
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 6) = R_GERM_NOST_AKI                                                                            !
        ! -------------------------------------------------------------------------------

        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 7) = R_FORM_NOST_AKI
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 8) = R_DENS_MORT_NOST_VEG_HET


        ! Nitrogen fixation by nostacales only
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 9) = &
            R_NOST_VEG_HET_FIX_GROWTH * NOST_N_TO_C * (1.D0/FRAC_FIX_N_FOR_GR_VEG_HET)

        ! Update the total nitrogen fixation
        PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 9) = &
            PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX, 9) + &
            PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 9)

        ! Auxilaries
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 10) = NOST_LIGHT_SAT

        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX,11) = LIM_KG_NOST_VEG_HET_LIGHT
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX,12) = LIM_KG_NOST_VEG_HET_TEMP
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX,13) = LIM_KG_NOST_VEG_HET_DOXY
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX,14) = LIM_KG_NOST_VEG_HET_P

        ! What is this????
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX,15) = &
            R_NOST_VEG_HET_GROWTH + R_CYN_GROWTH + R_OPA_GROWTH + R_DIA_GROWTH

        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX,16) = LIM_KG_NOST_VEG_HET_N


        DERIVATIVES(1:nkn,NOST_VEG_HET_C_INDEX) = &
             PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 1)  &
           - PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 2)  &
           - PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 3)  &
           - PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 4)  &
           - PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 5)  &
           + PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 6)  &
           - PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 7)  &
           - PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 8)
        ! -------------------------------------------------------------------------------

        ! -------------------------------------------------------------------------------
        ! Nostocales akinets
        ! -------------------------------------------------------------------------------

        ! Assummed formed akinetes immediatelly go to the bottom but still
        ! in units gC/m^3 bu goes to the output as gC/m^2
        ! They should settle trough the layers to the bottom, fixme
        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 1) = R_FORM_NOST_AKI

        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 2) = R_GERM_NOST_AKI
        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 3) = R_LOSS_AKI

        ! -------------------------------------------------------------------------------
        ! Keep it zero. Mortality of akinetes is negligible
        ! Before germination they sit in mud, not in WC
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 4) = R_MORT_AKI
        ! -------------------------------------------------------------------------------

        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 5) = DEPTH

        DERIVATIVES(1:nkn,NOST_AKI_C_INDEX) = &
            PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 1) - &
            PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 2) - &
            PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 3) - &
            PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 4)
        ! -------------------------------------------------------------------------------
    else
        ! -------------------------------------------------------------------------------
        ! Nostaocacles in vegetative + heterocyst staged form
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 1) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 2) = 0.0D0
            PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 3) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 4) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 5) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 6) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 7) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 8) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 9) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 10) = 0.0D0

        DERIVATIVES  (1:nkn,NOST_VEG_HET_C_INDEX)     = 0.0D0

        ! -------------------------------------------------------------------------------
        ! Nostaocacles akinets
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 1) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 2) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 3) = 0.0D0
        PROCESS_RATES(1:nkn,NOST_AKI_C_INDEX, 4) = 0.0D0

        DERIVATIVES  (1:nkn,NOST_AKI_C_INDEX)    = 0.0D0
        ! -------------------------------------------------------------------------------
    end if
    ! End of process rates for the new state variables incorporated 10 September 2019

    !OTHER PLANKTONIC ALGAE CARBON
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 1) = R_OPA_GROWTH
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 2) = R_OPA_TOT_RESP
        PROCESS_RATES(1:nkn,OPA_C_INDEX, 3) = R_OPA_EXCR
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 4) = R_OPA_DEATH
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 5) = R_ZOO_FEEDING_OPA

    ! Auxiliary
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 6 ) = LIM_KG_OPA_TEMP
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 7 ) = LIM_KG_OPA_DOXY
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 8 ) = LIM_KG_OPA_N
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 9 ) = LIM_KG_OPA_P
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 10) = LIM_KG_OPA_LIGHT
    PROCESS_RATES(1:nkn,OPA_C_INDEX, 11) = OPA_LIGHT_SAT

    DERIVATIVES(1:nkn,OPA_C_INDEX) = &
        PROCESS_RATES(1:nkn,OPA_C_INDEX, 1) - PROCESS_RATES(1:nkn,OPA_C_INDEX, 2) - &
        PROCESS_RATES(1:nkn,OPA_C_INDEX, 3) - PROCESS_RATES(1:nkn,OPA_C_INDEX, 4) - &
                PROCESS_RATES(1:nkn,OPA_C_INDEX, 5)

    !ZOOPLANKTON CARBON
    PROCESS_RATES(1:nkn,ZOO_C_INDEX, 1) = R_ZOO_GROWTH

    PROCESS_RATES(1:nkn,ZOO_C_INDEX, 2) = 0.0

    if (ZOOP_OPTION_1 > 0) then
        PROCESS_RATES(1:nkn,ZOO_C_INDEX, 2) = R_ZOO_EX_DOC
    end if

    PROCESS_RATES(1:nkn,ZOO_C_INDEX, 3) = R_ZOO_TOT_RESP
    PROCESS_RATES(1:nkn,ZOO_C_INDEX, 4) = R_ZOO_DEATH

    PROCESS_RATES(1:nkn,ZOO_C_INDEX, 5) = R_ZOO_FEEDING_DIA
    PROCESS_RATES(1:nkn,ZOO_C_INDEX, 6) = R_ZOO_FEEDING_CYN
    PROCESS_RATES(1:nkn,ZOO_C_INDEX, 7) = R_ZOO_FEEDING_OPA

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,ZOO_C_INDEX, 8) = R_ZOO_FEEDING_FIX_CYN
    else
        PROCESS_RATES(1:nkn,ZOO_C_INDEX, 8) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,ZOO_C_INDEX, 9) = R_ZOO_FEEDING_DET_PART_ORG_C

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,ZOO_C_INDEX, 10) = R_ZOO_FEEDING_NOST_VEG_HET
    else
        PROCESS_RATES(1:nkn,ZOO_C_INDEX, 10) = 0.0D0
    end if

    DERIVATIVES(1:nkn,ZOO_C_INDEX) = &
        PROCESS_RATES(1:nkn,ZOO_C_INDEX, 1) - PROCESS_RATES(1:nkn,ZOO_C_INDEX, 2) - &
        PROCESS_RATES(1:nkn,ZOO_C_INDEX, 3) - PROCESS_RATES(1:nkn,ZOO_C_INDEX, 4)

    ! Debug code for ZOO_C
    if(debug_stranger) then
        call DBGSTR_PEL_ZOOC_01 &
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if

    !ZOOPLANKTON NITROGEN
    DERIVATIVES(1:nkn,ZOO_N_INDEX) = DERIVATIVES(1:nkn,ZOO_C_INDEX) * ACTUAL_ZOO_N_TO_C

    !ZOOPLANKTON PHOSPHORUS
    DERIVATIVES(1:nkn,ZOO_P_INDEX) = DERIVATIVES(1:nkn,ZOO_C_INDEX) * ACTUAL_ZOO_P_TO_C

    if (ZOOP_OPTION_1 > 0) then

            PROCESS_RATES(1:nkn,ZOO_N_INDEX, 1) = R_ZOO_FEEDING_DIA * DIA_N_TO_C
            PROCESS_RATES(1:nkn,ZOO_N_INDEX, 2) = R_ZOO_FEEDING_CYN * CYN_N_TO_C
            PROCESS_RATES(1:nkn,ZOO_N_INDEX, 3) = R_ZOO_FEEDING_OPA * OPA_N_TO_C

            if (DO_NON_OBLIGATORY_FIXERS > 0) then
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 4) = R_ZOO_FEEDING_FIX_CYN * FIX_CYN_N_TO_C
            else
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 4) = 0.0D0
            end if

            PROCESS_RATES(1:nkn,ZOO_N_INDEX, 5) = R_ZOO_FEEDING_DET_PART_ORG_C * ACTUAL_DET_N_TO_C
            PROCESS_RATES(1:nkn,ZOO_N_INDEX, 6) = R_ZOO_EX_DON
            PROCESS_RATES(1:nkn,ZOO_N_INDEX, 7) = R_ZOO_TOT_RESP               * ACTUAL_ZOO_N_TO_C
            PROCESS_RATES(1:nkn,ZOO_N_INDEX, 8) = R_ZOO_DEATH                  * ACTUAL_ZOO_N_TO_C

            PROCESS_RATES(1:nkn,ZOO_N_INDEX, 9) = ACTUAL_ZOO_N_TO_C

            if (DO_NOSTOCALES > 0) then
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 10) = R_ZOO_FEEDING_NOST_VEG_HET * NOST_N_TO_C
            else
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 10) = 0.0D0
            end if

            PROCESS_RATES(1:nkn,ZOO_P_INDEX, 1) = R_ZOO_FEEDING_DIA            * DIA_P_TO_C
            PROCESS_RATES(1:nkn,ZOO_P_INDEX, 2) = R_ZOO_FEEDING_CYN            * CYN_P_TO_C
            PROCESS_RATES(1:nkn,ZOO_P_INDEX, 3) = R_ZOO_FEEDING_OPA            * OPA_P_TO_C

            if (DO_NON_OBLIGATORY_FIXERS > 0) then
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 4) = R_ZOO_FEEDING_FIX_CYN * FIX_CYN_P_TO_C
            else
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 4) = 0.0D0
            end if

            PROCESS_RATES(1:nkn,ZOO_P_INDEX, 5) = R_ZOO_FEEDING_DET_PART_ORG_C * ACTUAL_DET_P_TO_C
            PROCESS_RATES(1:nkn,ZOO_P_INDEX, 6) = R_ZOO_EX_DOP
            PROCESS_RATES(1:nkn,ZOO_P_INDEX, 7) = R_ZOO_TOT_RESP               * ACTUAL_ZOO_P_TO_C
            PROCESS_RATES(1:nkn,ZOO_P_INDEX, 8) = R_ZOO_DEATH                  * ACTUAL_ZOO_P_TO_C

            PROCESS_RATES(1:nkn,ZOO_P_INDEX, 9) = ACTUAL_ZOO_P_TO_C

            if (DO_NOSTOCALES > 0) then
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 10) = R_ZOO_FEEDING_NOST_VEG_HET * NOST_P_TO_C
            else
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 10) = 0.0D0
            end if

            DERIVATIVES(1:nkn,ZOO_N_INDEX) = &
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 1)  + PROCESS_RATES(1:nkn,ZOO_N_INDEX, 2)  + &
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 3)  + PROCESS_RATES(1:nkn,ZOO_N_INDEX, 4)  + &
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 5)  - PROCESS_RATES(1:nkn,ZOO_N_INDEX, 6)  - &
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 7)  - PROCESS_RATES(1:nkn,ZOO_N_INDEX, 8)  + &
                PROCESS_RATES(1:nkn,ZOO_N_INDEX, 10)

            DERIVATIVES(1:nkn,ZOO_P_INDEX) = &
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 1)  + PROCESS_RATES(1:nkn,ZOO_P_INDEX, 2)  + &
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 3)  + PROCESS_RATES(1:nkn,ZOO_P_INDEX, 4)  + &
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 5)  - PROCESS_RATES(1:nkn,ZOO_P_INDEX, 6)  - &
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 7)  - PROCESS_RATES(1:nkn,ZOO_P_INDEX, 8)  + &
                PROCESS_RATES(1:nkn,ZOO_P_INDEX, 10)
    end if

    !DEAD ORGANIC CARBON PARTICLES
    PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 1) = R_DIA_DEATH
    PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 2) = R_CYN_DEATH
    PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 3) = R_OPA_DEATH

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 4) = R_FIX_CYN_DEATH
    else
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 5) = R_ZOO_DEATH
    PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 6) = R_ZOO_FEEDING_DET_PART_ORG_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 7) = R_DET_PART_ORG_C_DISSOLUTION

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 8) = R_NOST_VEG_HET_DEATH
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 9) = R_DENS_MORT_NOST_VEG_HET

        ! Keep it zero. Mortality of akinetes is negligible
        ! Before germination they sit in mud, not in WC
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX,10) = R_MORT_AKI
    else
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 8)  = 0.0D0
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 9)  = 0.0D0
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 10) = 0.0D0
    end if

    DERIVATIVES(1:nkn,DET_PART_ORG_C_INDEX) = &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 1) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 2) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 3) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 4) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 5) - &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 6) - &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 7) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 8) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 9) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_C_INDEX, 10)

    ! Debug code for DET_PART_ORG_C
    if(debug_stranger) then
        call DBGSTR_PEL_DET_PART_ORG_C_01 &
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if

    !DEAD ORGANIC NITROGEN PARTICLES
    PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 1) = R_DIA_DEATH * DIA_N_TO_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 2) = R_CYN_DEATH * CYN_N_TO_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 3) = R_OPA_DEATH * OPA_N_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 4) = R_FIX_CYN_DEATH * FIX_CYN_N_TO_C
    else
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 5) = R_ZOO_DEATH                  * ACTUAL_ZOO_N_TO_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 6) = R_ZOO_FEEDING_DET_PART_ORG_C * ACTUAL_DET_N_TO_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 7) = R_DET_PART_ORG_N_DISSOLUTION

    ! Auxiliary
    PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 8) = ACTUAL_DET_N_TO_C

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 9 ) = R_NOST_VEG_HET_DEATH * NOST_N_TO_C
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 10) = R_DENS_MORT_NOST_VEG_HET * NOST_N_TO_C

        ! Keep it zero. Mortality of akinetes is negligible
        ! Before germination they st in mud, not in WC
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 11) = R_MORT_AKI * NOST_N_TO_C
    else
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX,  9) = 0.0D0
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 10) = 0.0D0
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 11) = 0.0D0
    end if

    DERIVATIVES(1:nkn,DET_PART_ORG_N_INDEX) = &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 1)  + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 2)  + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 3)  + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 4)  + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 5)  - &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 6)  - &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 7)  + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 9)  + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 10) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_N_INDEX, 11)

    !DEAD ORGANIC PHOSPHORUS PARTICLES
    PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 1) = R_DIA_DEATH * DIA_P_TO_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 2) = R_CYN_DEATH * CYN_P_TO_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 3) = R_OPA_DEATH * OPA_P_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 4) = R_FIX_CYN_DEATH * FIX_CYN_P_TO_C
    else
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 5) = R_ZOO_DEATH                  * ACTUAL_ZOO_P_TO_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 6) = R_ZOO_FEEDING_DET_PART_ORG_C * ACTUAL_DET_P_TO_C
    PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 7) = R_DET_PART_ORG_P_DISSOLUTION

    ! Auxiliary
    PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 8) = ACTUAL_DET_P_TO_C

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 9)  = R_NOST_VEG_HET_DEATH * NOST_P_TO_C
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 10) = R_DENS_MORT_NOST_VEG_HET * NOST_P_TO_C

        ! Keep it zero. Mortality of akinetes is negligible
        ! Before germination they sit in mud, not in WC
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 11) = R_MORT_AKI * NOST_P_TO_C
    else
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX,  9) = 0.0D0
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 10) = 0.0D0
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 11) = 0.0D0
    end if

    DERIVATIVES(1:nkn,DET_PART_ORG_P_INDEX) = &
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 1)  + PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 2) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 3)  + PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 4) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 5)  - PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 6) - &
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 7)  + PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 9) + &
        PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 10) + PROCESS_RATES(1:nkn,DET_PART_ORG_P_INDEX, 11)

    !PARTICULATE SILICA
    PROCESS_RATES(1:nkn,PART_Si_INDEX, 1) = R_DIA_DEATH * DIA_Si_TO_C
    PROCESS_RATES(1:nkn,PART_Si_INDEX, 2) = R_ZOO_FEEDING_DIA * DIA_Si_TO_C
    PROCESS_RATES(1:nkn,PART_Si_INDEX, 3) = R_PART_Si_DISS

    DERIVATIVES(1:nkn,PART_Si_INDEX) = &
        PROCESS_RATES(1:nkn,PART_Si_INDEX, 1) + PROCESS_RATES(1:nkn,PART_Si_INDEX, 2) - &
        PROCESS_RATES(1:nkn,PART_Si_INDEX, 3)

    !DISSOLVED ORGANIC CARBON
    PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 1) = R_DET_PART_ORG_C_DISSOLUTION
    PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 2) = R_ZOO_EX_DOC

    PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 3)  = &
        R_ABIOTIC_DOC_MIN_DOXY   + R_ABIOTIC_DOC_MIN_NO3N     + R_ABIOTIC_DOC_MIN_MN_IV + &
        R_ABIOTIC_DOC_MIN_FE_III + R_ABIOTIC_DOC_MIN_S_PLUS_6 + R_ABIOTIC_DOC_MIN_DOC

    PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 4) = R_DIA_EXCR + R_CYN_EXCR + R_OPA_EXCR
    PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 5) = R_DIA_EXCR
    PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 6) = R_CYN_EXCR
    PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 7) = R_OPA_EXCR

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 4) = &
            PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 4) + R_FIX_CYN_EXCR

        PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 8) = R_FIX_CYN_EXCR
    else
        PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 8) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 4) = &
            PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 4) + R_NOST_VEG_HET_EXCR

        PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 9) = R_NOST_VEG_HET_EXCR
    else
        PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 9) = 0.0D0
    end if

    DERIVATIVES(1:nkn,DISS_ORG_C_INDEX) = &
        PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 1) + PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 2) - &
        PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 3) + PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX, 4)

    !DISSOLVED ORGANIC NITROGEN
    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 1) = R_DET_PART_ORG_N_DISSOLUTION
    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 2) = R_ZOO_EX_DON

    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 3)  = &
        R_ABIOTIC_DON_MIN_DOXY   + R_ABIOTIC_DON_MIN_NO3N     + R_ABIOTIC_DON_MIN_MN_IV + &
        R_ABIOTIC_DON_MIN_FE_III + R_ABIOTIC_DON_MIN_S_PLUS_6 + R_ABIOTIC_DON_MIN_DOC

    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 4) = &
        (R_DIA_EXCR * DIA_N_TO_C) + (R_CYN_EXCR * CYN_N_TO_C) + (R_OPA_EXCR * OPA_N_TO_C)

    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 5) = &
        R_CYN_GROWTH * CYN_N_TO_C * (1.D0 - PREF_DIN_DON_CYN)

    if(DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 6) = &
            R_NOST_VEG_HET_NON_FIX_GROWTH * NOST_N_TO_C * (1.D0 - PREF_DIN_DON_NOST)
    else
       PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 6) = &
           R_NON_FIX_CYN_GROWTH * PREF_NH4N_DON_FIX_CYN * CYN_N_TO_C * &
           ((DISS_ORG_N * frac_avail_DON) / max(NH4_N + (DISS_ORG_N * frac_avail_DON), 1.0D-10))
    end if

    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 7) = R_DIA_EXCR
    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 8) = R_CYN_EXCR
    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 9) = R_OPA_EXCR

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 4) = &
            PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 4) + (R_FIX_CYN_EXCR * FIX_CYN_N_TO_C)

        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 10) = R_FIX_CYN_EXCR * FIX_CYN_N_TO_C
    else
        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 10) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 4) = &
            PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 4) + (R_NOST_VEG_HET_EXCR * NOST_N_TO_C)

        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 11) = R_NOST_VEG_HET_EXCR * NOST_N_TO_C

         PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 5) = &
        (R_CYN_GROWTH * CYN_N_TO_C * (1.D0 - PREF_DIN_DON_CYN))
    else
        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 11) = 0.0D0
    end if
    PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 12) = PHYT_TOT_C

    DERIVATIVES(1:nkn,DISS_ORG_N_INDEX) = &
        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 1) + PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 2) - &
        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 3) + PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 4) - &
        PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 5) - PROCESS_RATES(1:nkn,DISS_ORG_N_INDEX, 6)

     ! Debug code for  DISS_ORG_N
     if(debug_stranger) then
         call DBGSTR_PEL_DET_DISS_ORG_N_01 &
              (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if


    !DISSOLVED ORGANIC PHOSPHORUS
    PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 1) = R_DET_PART_ORG_P_DISSOLUTION
    PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 2) = R_ZOO_EX_DOP

    PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 3) =  &
        R_ABIOTIC_DOP_MIN_DOXY   + R_ABIOTIC_DOP_MIN_NO3N     + R_ABIOTIC_DOP_MIN_MN_IV + &
        R_ABIOTIC_DOP_MIN_FE_III + R_ABIOTIC_DOP_MIN_S_PLUS_6 + R_ABIOTIC_DOP_MIN_DOC

    PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 4) = &
        (R_DIA_EXCR * DIA_P_TO_C) + (R_CYN_EXCR * CYN_P_TO_C) + (R_OPA_EXCR * OPA_P_TO_C)

    PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 5) = R_DIA_EXCR
    PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 6) = R_CYN_EXCR
    PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 7) = R_OPA_EXCR

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 4) = &
            PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 4) + (R_FIX_CYN_EXCR * FIX_CYN_P_TO_C)

        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 8) = R_FIX_CYN_EXCR * FIX_CYN_P_TO_C
    else
        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 8) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 4) = &
            PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 4) + (R_NOST_VEG_HET_EXCR * NOST_P_TO_C)

        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 9) = R_NOST_VEG_HET_EXCR * NOST_P_TO_C

        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 10) = &
        (R_NOST_VEG_HET_GROWTH * NOST_P_TO_C * (1.D0 - PREF_DIP_DOP_NOST))
    else
        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 9)  = 0.0D0
        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 10) = 0.0D0
    end if

    DERIVATIVES(1:nkn,DISS_ORG_P_INDEX) = &
        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 1) + PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 2) - &
        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 3) + PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 4) - &
        PROCESS_RATES(1:nkn,DISS_ORG_P_INDEX, 10)

    ! Kinetic sub model for dissolved inorganic carbon

    ! Sources
    R_DIA_TOT_RESP          = PROCESS_RATES(1:nkn,DIA_C_INDEX          , 2)
    R_CYN_TOT_RESP          = PROCESS_RATES(1:nkn,CYN_C_INDEX          , 2)
    R_FIX_CYN_TOT_RESP      = PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX      , 2)
    R_OPA_TOT_RESP          = PROCESS_RATES(1:nkn,OPA_C_INDEX          , 2)
    R_ZOO_RESP              = PROCESS_RATES(1:nkn,ZOO_C_INDEX          , 3)
    R_ABIOTIC_DOC_MIN       = PROCESS_RATES(1:nkn,DISS_ORG_C_INDEX     , 3)
    R_NOST_VEG_HET_TOT_RESP = PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX , 2)

    TOTAL_DIC_KINETIC_SOURCES = &
        R_DIA_TOT_RESP             + R_CYN_TOT_RESP             + &
        R_OPA_TOT_RESP             + R_ZOO_RESP                 + &
        R_ABIOTIC_DOC_MIN_DOXY     + R_ABIOTIC_DOC_MIN_NO3N     + &
        R_ABIOTIC_DOC_MIN_MN_IV    + R_ABIOTIC_DOC_MIN_FE_III   + &
        R_ABIOTIC_DOC_MIN_S_PLUS_6 + R_METHANOGENESIS + &
        R_METHANE_OXIDATION

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        TOTAL_DIC_KINETIC_SOURCES = TOTAL_DIC_KINETIC_SOURCES + R_FIX_CYN_TOT_RESP
    end if

    if (DO_NOSTOCALES > 0) then
        TOTAL_DIC_KINETIC_SOURCES = TOTAL_DIC_KINETIC_SOURCES + R_NOST_VEG_HET_TOT_RESP
    end if

    ! One-shot CHLA negative dump: if computed CHLA went negative in CHLA routine, print full local context once
    if (CHLA_NEG_FLAG .and. (.not. CHLA_DUMP_DONE)) then
        dump_node = CHLA_NEG_NODE
        write(6,'(A,F12.4,A,I4,A,ES12.4)') &
            'ONE-SHOT CHLA NEG DUMP: TIME=', TIME, ' NODE=', dump_node, ' CHLA=', CHLA_NEG_VALUE
        write(6,'(A,ES12.4,A,ES12.4)') &
            '  FIX_CYN_C=', FIX_CYN_C(dump_node), ' CYN_C=', CYN_C(dump_node)
        write(6,'(A,ES12.4,A,ES12.4)') &
            '  DERIV_FIX=', DERIVATIVES(dump_node,FIX_CYN_C_INDEX), ' DERIV_CYN=', DERIVATIVES(dump_node,CYN_C_INDEX)
        write(6,'(A,3ES12.4)') '  NH4_N/NO3_N/PO4_P=', NH4_N(dump_node), NO3_N(dump_node), PO4_P(dump_node)
        CHLA_DUMP_DONE = .true.
    end if

    ! Sinks
    R_DIA_GROWTH          = PROCESS_RATES(1:nkn,DIA_C_INDEX         , 1)
    R_CYN_GROWTH          = PROCESS_RATES(1:nkn,CYN_C_INDEX         , 1)
    R_FIX_CYN_GROWTH      = PROCESS_RATES(1:nkn,FIX_CYN_C_INDEX     , 1)
    R_OPA_GROWTH          = PROCESS_RATES(1:nkn,OPA_C_INDEX         , 1)
    R_NOST_VEG_HET_GROWTH = PROCESS_RATES(1:nkn,NOST_VEG_HET_C_INDEX, 1)

    TOTAL_DIC_KINETIC_SINKS = R_DIA_GROWTH + R_CYN_GROWTH + R_OPA_GROWTH

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        TOTAL_DIC_KINETIC_SINKS = TOTAL_DIC_KINETIC_SINKS + R_FIX_CYN_GROWTH
    end if

    if (DO_NOSTOCALES > 0) then
        TOTAL_DIC_KINETIC_SINKS = TOTAL_DIC_KINETIC_SINKS + R_NOST_VEG_HET_GROWTH
    end if

    ! Atmospheric exchange
    T_A = TEMP + CELSIUS_TO_KELVIN

    ! Calculate the saturaion concentration of CO2 in water
    P_K_H   = -(2385.73D0 / T_A) - (0.0152642D0 * T_A) + 14.0184D0
    K_H     = 10.0D0 ** (-P_K_H)
    P_CO2   = 10.0D0 ** (-3.416D0)
    CO2_SAT = K_H * P_CO2         !In moles

    ! Calculate the rearation rate constant for CO2
    K_A_CALC_CO2 = K_A_CALC * 0.923D0

    ! Finally calculate the atmospheric exchange rate
    CO2_ATM_EXHANGE = K_A_CALC_CO2 * (CO2_SAT - (H2CO3/1.0D6))

    !----------------------------------------------------------------------
    ! 2 February 2015
    ! New code added to account the effect of ice cover.
    !----------------------------------------------------------------------
    CO2_ATM_EXHANGE = CO2_ATM_EXHANGE * (1.0D0 - ice_cover)
    !----------------------------------------------------------------------
    ! End of new code added to account the effect of ice cover.
    !----------------------------------------------------------------------

    ! End of atmospheric exchange

    ! Calculate the total derivative and convert it to moles
    if (CONSIDER_INORG_C_DERIVATIVE > 0) then
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 1) = TOTAL_DIC_KINETIC_SOURCES / 12000.0D0
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 2) = TOTAL_DIC_KINETIC_SINKS   / 12000.0D0

        if (CONSIDER_CO2_REARATION > 0) then
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 3) = CO2_ATM_EXHANGE
        else
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 3) = 0.0D0
            CO2_ATM_EXHANGE = 0.0D0
        end if

        PROCESS_RATES(1:nkn,INORG_C_INDEX, 4)  = R_DIA_TOT_RESP
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 5)  = R_CYN_TOT_RESP

        PROCESS_RATES(1:nkn,INORG_C_INDEX, 7)  = R_OPA_TOT_RESP
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 8)  = R_ZOO_RESP
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 9)  = R_ABIOTIC_DOC_MIN
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 10) = R_DIA_GROWTH
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 11) = R_CYN_GROWTH
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 13) = R_OPA_GROWTH

        if (DO_NON_OBLIGATORY_FIXERS > 0) then
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 6)  = R_FIX_CYN_TOT_RESP
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 12) = R_FIX_CYN_GROWTH
        else
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 6)  = 0.0D0
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 12) = 0.0D0
        end if

        if (DO_NOSTOCALES > 0) then
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 14) = R_NOST_VEG_HET_TOT_RESP
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 15) = R_NOST_VEG_HET_GROWTH
        else
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 14) = 0.0D0
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 15) = 0.0D0
        end if

        DIC_KINETIC_DERIVATIVE = CO2_ATM_EXHANGE + &
            ((TOTAL_DIC_KINETIC_SOURCES - TOTAL_DIC_KINETIC_SINKS) / 12000.0D0)

        DERIVATIVES(1:nkn,INORG_C_INDEX) = DIC_KINETIC_DERIVATIVE
    else
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 1) = 0.0D0
        PROCESS_RATES(1:nkn,INORG_C_INDEX, 2) = 0.0D0

        if (CONSIDER_CO2_REARATION > 0) then
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 3) = CO2_ATM_EXHANGE
        else
            PROCESS_RATES(1:nkn,INORG_C_INDEX, 3) = 0.0D0
            CO2_ATM_EXHANGE = 0.0D0
        end if

        PROCESS_RATES(1:nkn,INORG_C_INDEX, 4:15) = 0.0D0

        if (CONSIDER_CO2_REARATION > 0) then
            DERIVATIVES(1:nkn,INORG_C_INDEX) = CO2_ATM_EXHANGE
        else
            DERIVATIVES(1:nkn,INORG_C_INDEX) = 0.0D0
        end if
    end if

    ! -------------------------------------------------------------------------
    ! 29 JANUARY 2016, KINETIC DERIVATIVES FOR THE NEW STATE VARIABLES
    ! -------------------------------------------------------------------------
    if (DO_ADVANCED_REDOX_SIMULATION > 0) then

        ! Calcium
        DERIVATIVES(1:nkn,CA_INDEX) = 0.0D0

        ! Magnesium
        DERIVATIVES(1:nkn,MG_INDEX) = 0.0D0

        ! Suphate sulphur
        PROCESS_RATES(1:nkn, S_PLUS_6_INDEX, 1) = R_SULPHIDE_OXIDATION
        PROCESS_RATES(1:nkn, S_PLUS_6_INDEX, 2) = R_SULPHATE_REDUCTION

        DERIVATIVES(1:nkn,S_PLUS_6_INDEX) = &
            PROCESS_RATES(1:nkn, S_PLUS_6_INDEX, 1) - &
            PROCESS_RATES(1:nkn, S_PLUS_6_INDEX, 2)

        ! Sulphide sulphur
        PROCESS_RATES(1:nkn, S_MINUS_2_INDEX, 1) = H2S_ATM_EXCHANGE
        PROCESS_RATES(1:nkn, S_MINUS_2_INDEX, 2) = R_SULPHATE_REDUCTION
        PROCESS_RATES(1:nkn, S_MINUS_2_INDEX, 3) = R_SULPHIDE_OXIDATION

        DERIVATIVES(1:nkn,S_MINUS_2_INDEX) = &
            PROCESS_RATES(1:nkn, S_MINUS_2_INDEX, 1) + &
            PROCESS_RATES(1:nkn, S_MINUS_2_INDEX, 2) - &
            PROCESS_RATES(1:nkn, S_MINUS_2_INDEX, 3)

        ! Methane carbon
        PROCESS_RATES(1:nkn, CH4_C_INDEX, 1) = CH4_ATM_EXCHANGE
        PROCESS_RATES(1:nkn, CH4_C_INDEX, 2) = R_METHANOGENESIS
        PROCESS_RATES(1:nkn, CH4_C_INDEX, 3) = R_METHANE_OXIDATION

        DERIVATIVES(1:nkn,CH4_C_INDEX) = &
            PROCESS_RATES(1:nkn, CH4_C_INDEX, 1) + PROCESS_RATES(1:nkn, CH4_C_INDEX, 2) - &
            PROCESS_RATES(1:nkn, CH4_C_INDEX, 3)
    end if

    ! Debug code for Methane
    if(debug_stranger) then
        call DBGSTR_PEL_DET_CH4_C_01 &
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if

    ! -------------------------------------------------------------------------
    ! END OF 29 JANUARY 2016, KINETIC DERIVATIVES FOR THE NEW STATE VARIABLES
    ! -------------------------------------------------------------------------


    ! After the introduction of changes in November 30 th 2015, it will be necessary
    ! to reconsider alkalinity since metals will change ion balance
    ! Job to be conducted wiht Petras together, and completed during next visit of
    ! of Ali to Lithuania. Also for iron and mangenese, the redox sequences as described
    ! by Van Chappen and Wang 2015 and Katsev papers should be included.

    ! BE CAREFUL THAT THIS CHANGES WILL NEED THE METAL KITEIC PROCESS RATES TO BE
    ! CALCULATED BEFORE THE ALKALINITY SUBMODEL WHICH IS NOT THE CASE IN PRESENT
    ! SITUATION !!!!!!


    ! -------------------------------------------------------------------------
    ! KINETIC SUBMODEL FOR ALKALINITY
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! PREPARE FOR ALKALINITY-NITROGEN INTERACTIONS
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the NH4 and NH3 fractions in ammonia
    ! -------------------------------------------------------------------------
    T_A      = TEMP + CELSIUS_TO_KELVIN
    pKH      = 9.018D-2 + (2.72992D3 / T_A)
    FRAC_NH3 = 1.0D0 / (1.0D0 + (10.0D0 ** (pKH - PH)))
    FRAC_NH4 = 1.0D0 - FRAC_NH3
    ! -------------------------------------------------------------------------
    ! End of calculate NH4 and NH3 fractions in ammonia
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the NH4N in fluxes and out fluxes from fluxes
    ! -------------------------------------------------------------------------
    where(SEDIMENT_FLUXES(1:nkn, NH4_N_INDEX) .ge. 0.0D0)
        NH4N_SEDIMENT_INFLUX  = SEDIMENT_FLUXES(1:nkn, NH4_N_INDEX)* FRAC_NH4
        NH4N_SEDIMENT_OUTFLUX = 0.0D0
    else where
        NH4N_SEDIMENT_INFLUX  = 0.0D0
        NH4N_SEDIMENT_OUTFLUX = (-SEDIMENT_FLUXES(1:nkn, NH4_N_INDEX))*FRAC_NH4
    end where
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the NO3N in fluxes and out fluxes from fluxes
    ! -------------------------------------------------------------------------
    where(SEDIMENT_FLUXES(1:nkn, NH4_N_INDEX) .ge. 0.0D0)
        NO3N_SEDIMENT_INFLUX  = SEDIMENT_FLUXES(1:nkn, NO3_N_INDEX)
        NO3N_SEDIMENT_OUTFLUX = 0.0D0
    else where
        NO3N_SEDIMENT_INFLUX  = 0.0D0
        NO3N_SEDIMENT_OUTFLUX = (-SEDIMENT_FLUXES(1:nkn, NO3_N_INDEX))
    end where

    ! -------------------------------------------------------------------------
    ! END OF PREPARE FOR ALKALINITY-NITROGEN INTERACTIONS
    ! -------------------------------------------------------------------------


    ! -------------------------------------------------------------------------
    ! NITROGEN BASED SOURCES
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the alkalinity gain by ammonium generation
    ! (1 eq alk for each ammonium generated since one positive ion is gained)
    ! -------------------------------------------------------------------------
    N_DIA_TOT_RESP            = PROCESS_RATES(1:nkn,NH4_N_INDEX, 1)  * FRAC_NH4
    N_CYN_TOT_RESP            = PROCESS_RATES(1:nkn,NH4_N_INDEX, 2)  * FRAC_NH4
    N_OPA_TOT_RESP            = PROCESS_RATES(1:nkn,NH4_N_INDEX, 3)  * FRAC_NH4
    N_ZOO_TOT_RESP            = PROCESS_RATES(1:nkn,NH4_N_INDEX, 5)  * FRAC_NH4
    N_ABIOTIC_DON_MIN         = PROCESS_RATES(1:nkn,NH4_N_INDEX, 11) * FRAC_NH4

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        N_FIX_CYN_TOT_RESP = PROCESS_RATES(1:nkn,NH4_N_INDEX, 4)  * FRAC_NH4
    else
        N_FIX_CYN_TOT_RESP = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        N_NOST_VEG_HET_TOT_RESP    = PROCESS_RATES(1:nkn,NH4_N_INDEX, 17) * FRAC_NH4
        N_NOST_VEG_HET_NH4_RELEASE = PROCESS_RATES(1:nkn,NH4_N_INDEX, 22) * FRAC_NH4
    else
        N_NOST_VEG_HET_TOT_RESP    = 0.0D0
        N_NOST_VEG_HET_NH4_RELEASE = 0.0D0
    end if

    ALK_GAINED_BY_AMMONIUM_GEN = &
        (N_DIA_TOT_RESP            + N_CYN_TOT_RESP             + &
         N_OPA_TOT_RESP            + N_FIX_CYN_TOT_RESP         + &
         N_NOST_VEG_HET_TOT_RESP   + N_ZOO_TOT_RESP             + &
         N_ABIOTIC_DON_MIN         + N_NOST_VEG_HET_NH4_RELEASE + &
         NH4N_SEDIMENT_INFLUX) / 14007.0D0
    ! -------------------------------------------------------------------------
    ! End of calculate the alkalinity gain by ammonium generation
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the alkality gain by nitrate consumption
    ! 1 eq alk for each nitrate consumed since one negative ion is lost
    ! -------------------------------------------------------------------------
    N_DENITRIFICATION     = PROCESS_RATES(1:nkn,NO3_N_INDEX, 2)
    N_DIA_GROWTH          = PROCESS_RATES(1:nkn,NO3_N_INDEX, 3)
    N_CYN_GROWTH          = PROCESS_RATES(1:nkn,NO3_N_INDEX, 4)
    N_OPA_GROWTH          = PROCESS_RATES(1:nkn,NO3_N_INDEX, 5)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        N_NON_FIX_CYN_GROWTH = PROCESS_RATES(1:nkn,NO3_N_INDEX, 6)
    else
        N_NON_FIX_CYN_GROWTH = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        NO3N_NOST_VEG_HET_GROWTH = PROCESS_RATES(1:nkn,NO3_N_INDEX, 6)
    else
        NO3N_NOST_VEG_HET_GROWTH = 0.0D0
    end if

    ! Nothing for nostocales since they are fixers

    ALK_GAINED_BY_NITRATE_CONS = &
        (N_DENITRIFICATION        + N_DIA_GROWTH + N_CYN_GROWTH + &
         N_OPA_GROWTH             + N_NON_FIX_CYN_GROWTH        + &
         NO3N_NOST_VEG_HET_GROWTH + NO3N_SEDIMENT_OUTFLUX) / 14007.0D0
    ! -------------------------------------------------------------------------
    ! End of calculate the alkality gain by denitrification
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! END OF NITROGEN BASED SOURCES
    ! -------------------------------------------------------------------------


    ! -------------------------------------------------------------------------
    ! NITROGEN BASED SINKS
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the alkalinity loss by ammonium consumption
    ! 1 eq alk for each ammonium consumed since one positive ion is lost
    ! -------------------------------------------------------------------------
    N_DIA_GROWTH          = PROCESS_RATES(1:nkn,NH4_N_INDEX, 6) * FRAC_NH4
    N_CYN_GROWTH          = PROCESS_RATES(1:nkn,NH4_N_INDEX, 7) * FRAC_NH4
    N_OPA_GROWTH          = PROCESS_RATES(1:nkn,NH4_N_INDEX, 8) * FRAC_NH4

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        N_NON_FIX_CYN_GROWTH  = PROCESS_RATES(1:nkn,NH4_N_INDEX, 9) * FRAC_NH4
    else
        N_NON_FIX_CYN_GROWTH = 0.0D0
    end if


    if (DO_NOSTOCALES > 0) then
        !growth supported by water ammonia (not fixed)
        NH4N_NOST_VEG_HET_GROWTH = PROCESS_RATES(1:nkn,NH4_N_INDEX, 23) * FRAC_NH4
    else
        NH4N_NOST_VEG_HET_GROWTH = 0.0D0
    end if

    ALK_LOST_BY_AMMONIUM_CONS = &
        (N_DIA_GROWTH             + N_CYN_GROWTH         + &
         N_OPA_GROWTH             + N_NON_FIX_CYN_GROWTH + &
         NH4N_NOST_VEG_HET_GROWTH + NH4N_SEDIMENT_OUTFLUX) / 14007.0D0
    ! -------------------------------------------------------------------------
    ! End of calculate the alkalinity loss by ammonium consumption
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the alkalinity loss by nitrification
    ! - 2 eq alk for each ammonium nitrified since one positive ion is lost and
    !   one negative ion is gained
    !
    ! - 1 eq alk for each NH3 nitrified since one uncharged nitrogen is lost and
    !   one negative ion is gained
    ! -------------------------------------------------------------------------
    N_NITRIFICATION_NH4 = PROCESS_RATES(1:nkn,NH4_N_INDEX,10) * FRAC_NH4
    N_NITRIFICATION_NH3 = PROCESS_RATES(1:nkn,NH4_N_INDEX,10) * FRAC_NH3

    ALK_LOST_BY_NITRIFICATION = &
        ((2.0D0 * N_NITRIFICATION_NH4) + N_NITRIFICATION_NH3) / 14007.0D0
    ! -------------------------------------------------------------------------
    ! END OF NITROGEN BASED SINKS
    ! -------------------------------------------------------------------------


    ! -------------------------------------------------------------------------
    ! PREPARE FOR PHOSPHORUS-ALKALINITY COUPLING
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the dissociation constants of H2PO3
    ! -------------------------------------------------------------------------

    KP_OPTION = 1

    do i =1, nkn
        if (SALT(i) .lt. 0.d0) then
            if (SALT(i) .gt. -1.d-5) then
                SALT(i) = 0.d0
            else
                write(*,*) 'aquabc_II_pelagic_model: SALINITY IS NEGATIVE'
                write(*,*) 'SALT=', SALT(i), 'node=', i
            end if
        end if


    select case (KP_OPTION)

        ! Option 0 : Use fixed values from the general chemistry book
        case (0)
            K_ONE_TIP   = 10.0D0 ** (-2.15D0)
            K_TWO_TIP   = 10.0D0 ** (-7.20D0)
            K_THREE_TIP = 10.0D0 ** (-2.15D0)

        ! Option 1 : Use values calculated by CO2SYS
        case (1)
            continue

        ! Option 2 : Use values from the chemical ocenography book, the unsafe formula
        case (2)
            A_1 = 115.525D0
            A_2 = -4576.7525D0
            A_3 = -18.453D0
            B_1 = 0.69171D0
            B_2 = -106.736D0
            C_1 = -0.01844D0
            C_2 = -0.6543D0

            K_ONE_TIP   = exp(A_1 + (A_2 / TEMP) + (A_3 * log(TEMP))   + &
                              ((B_1 + (B_2 / TEMP)) * (SALT ** 0.5D0)) + &
                              ((C_1 + (C_2 / TEMP)) * SALT))

            A_1 = 172.0883D0
            A_2 = -8814.715D0
            A_3 = -27.927D0
            B_1 = 1.3566D0
            B_2 = -160.34D0
            C_1 = -0.05778D0
            C_2 = -0.37335D0

            K_TWO_TIP   = exp(A_1 + (A_2 / TEMP) + (A_3 * log(TEMP))   + &
                              ((B_1 + (B_2 / TEMP)) * (SALT ** 0.5D0)) + &
                              ((C_1 + (C_2 / TEMP)) * SALT))

            A_1 = -18.141D0
            A_2 = -3070.75D0
            A_3 = 0.0D0
            B_1 = 2.81197D0
            B_2 = 17.27039D0
            C_1 = -0.09984D0
            C_2 = -44.99486D0

            K_THREE_TIP = exp(A_1 + (A_2 / TEMP) + (A_3 * log(TEMP))   + &
                              ((B_1 + (B_2 / TEMP)) * (SALT ** 0.5D0)) + &
                              ((C_1 + (C_2 / TEMP)) * SALT))
    end select

    FRACTION_DIVISOR_TIP = &
        (H_PLUS * H_PLUS * H_PLUS) + (K_ONE_TIP * H_PLUS * H_PLUS) + &
        (K_ONE_TIP * K_TWO_TIP * H_PLUS) + (K_ONE_TIP * K_TWO_TIP * K_THREE_TIP)

    ALPHA_H2PO4 = (K_ONE_TIP * H_PLUS    * H_PLUS)      / FRACTION_DIVISOR_TIP
    ALPHA_HPO4  = (K_ONE_TIP * K_TWO_TIP * H_PLUS)      / FRACTION_DIVISOR_TIP
    ALPHA_PO4   = (K_ONE_TIP * K_TWO_TIP * K_THREE_TIP) / FRACTION_DIVISOR_TIP

    PHOSPHATE_EQ_CONSTANT = &
        ALPHA_H2PO4 + (2.0D0 * ALPHA_HPO4) + (3.0D0 * ALPHA_PO4)
    end do
    ! -------------------------------------------------------------------------
    ! End of calculate the dissociation constants of H2PO3
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the NH4N in fluxes and out fluxes from fluxes
    ! -------------------------------------------------------------------------
    where(SEDIMENT_FLUXES(1:nkn, PO4_P_INDEX) .ge. 0.0D0)

        PO4P_SEDIMENT_INFLUX  = &
            SEDIMENT_FLUXES(1:nkn, PO4_P_INDEX)    * PHOSPHATE_EQ_CONSTANT

        PO4P_SEDIMENT_OUTFLUX = 0.0D0
    else where
        PO4P_SEDIMENT_INFLUX  = 0.0D0

        PO4P_SEDIMENT_OUTFLUX = &
            (-SEDIMENT_FLUXES(1:nkn, PO4_P_INDEX)) * PHOSPHATE_EQ_CONSTANT
    end where
    ! -------------------------------------------------------------------------


    ! -------------------------------------------------------------------------
    ! END OF PREPARE FOR PHOSPHORUS-ALKALINITY COUPLING
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! PHOSPHORUS BASED SOURCES
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the alkality gain by phosphate consumption
    ! 1 eq alk for each H2PO4 consumed since one negative ion charge is lost
    ! 2 eq alk for each HPO4 consumed since two negative ion charges are lost
    ! 3 eq alk for each PO4 consumed since three negative ion charges are lost
    ! -------------------------------------------------------------------------
    P_DIA_GROWTH          = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 6) * PHOSPHATE_EQ_CONSTANT

    P_CYN_GROWTH          = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 7) * PHOSPHATE_EQ_CONSTANT

    P_OPA_GROWTH          = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 8) * PHOSPHATE_EQ_CONSTANT

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        P_FIX_CYN_GROWTH = &
            PROCESS_RATES(1:nkn,PO4_P_INDEX, 9) * PHOSPHATE_EQ_CONSTANT
    else
        P_FIX_CYN_GROWTH = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        P_NOST_VEG_HET_GROWTH = &
            PROCESS_RATES(1:nkn,PO4_P_INDEX, 14) * PHOSPHATE_EQ_CONSTANT
    else
        P_NOST_VEG_HET_GROWTH = 0.0D0
    end if

    ALK_GAINED_BY_PHOSPHATE_CONS = &
        (P_DIA_GROWTH     + P_CYN_GROWTH          + P_OPA_GROWTH         + &
         P_FIX_CYN_GROWTH + P_NOST_VEG_HET_GROWTH + PO4P_SEDIMENT_OUTFLUX) / &
        30974.0D0
    ! -------------------------------------------------------------------------
    ! End of calculate the alkality gain by phosphate consumption
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! END OF PHOSPHORUS BASED SOURCES
    ! -------------------------------------------------------------------------


    ! -------------------------------------------------------------------------
    ! PHOSPHORUS BASED SINKS
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the alkality loss by phosphate generation
    ! 1 eq alk for each H2PO4 consumed since one negative ion charge is lost
    ! 2 eq alk for each HPO4 consumed since two negative ion charges are lost
    ! 3 eq alk for each PO4 consumed since three negative ion charges are lost
    ! -------------------------------------------------------------------------
    P_DIA_TOT_RESP     = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 1)  * PHOSPHATE_EQ_CONSTANT

    P_CYN_TOT_RESP     = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 2)  * PHOSPHATE_EQ_CONSTANT

    P_OPA_TOT_RESP     = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 3)  * PHOSPHATE_EQ_CONSTANT

    P_ZOO_TOT_RESP     = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 5)  * PHOSPHATE_EQ_CONSTANT

    P_ABIOTIC_DOP_MIN  = &
        PROCESS_RATES(1:nkn,PO4_P_INDEX, 10) * PHOSPHATE_EQ_CONSTANT

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        P_FIX_CYN_TOT_RESP = &
            PROCESS_RATES(1:nkn,PO4_P_INDEX, 4)  * PHOSPHATE_EQ_CONSTANT
    else
        P_FIX_CYN_TOT_RESP = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        P_NOST_VEG_HET_TOT_RESP = &
            PROCESS_RATES(1:nkn,PO4_P_INDEX, 13)  * PHOSPHATE_EQ_CONSTANT
    else
        P_NOST_VEG_HET_TOT_RESP = 0.0D0
    end if

    ALK_LOST_BY_PHOSPHATE_GEN = &
        (P_DIA_TOT_RESP          + P_CYN_TOT_RESP + P_OPA_TOT_RESP    + &
         P_FIX_CYN_TOT_RESP      + P_ZOO_TOT_RESP + P_ABIOTIC_DOP_MIN + &
         P_NOST_VEG_HET_TOT_RESP + PO4P_SEDIMENT_INFLUX) / 30974.0D0
    ! -------------------------------------------------------------------------
    ! End of calculate the alkality loss by phosphate generation
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! END OF PHOSPHORUS BASED SINKS
    ! -------------------------------------------------------------------------



    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! TO DO : Processes that should be added for nutrients
    !         - Precipitation / settling
    !         - Sediment fluxes (partly done)
    !         - Adsorption
    !         - Desorption
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate the total derivative and convert in moles
    if (CONSIDER_ALKALNITY_DERIVATIVE > 0) then
        ALK_KINETIC_DERIVATIVE = &
            ALK_GAINED_BY_AMMONIUM_GEN         + &
            ALK_GAINED_BY_NITRATE_CONS         + &
            ALK_GAINED_BY_PHOSPHATE_CONS       - &
            ALK_LOST_BY_AMMONIUM_CONS          - &
            ALK_LOST_BY_NITRIFICATION          - &
            (NO3N_SEDIMENT_INFLUX / 14007.0D0) - & ! Alkal�nity lost by NO3N influx
            ALK_LOST_BY_PHOSPHATE_GEN
    else
        ALK_KINETIC_DERIVATIVE       = 0.0D0
        ALK_GAINED_BY_AMMONIUM_GEN   = 0.0D0
        ALK_GAINED_BY_NITRATE_CONS   = 0.0D0
        ALK_GAINED_BY_PHOSPHATE_CONS = 0.0D0
        ALK_LOST_BY_AMMONIUM_CONS    = 0.0D0
        ALK_LOST_BY_NITRIFICATION    = 0.0D0
        ALK_LOST_BY_PHOSPHATE_GEN    = 0.0D0
    end if
    ! -------------------------------------------------------------------------
    ! END OF KINETIC SUBMODEL FOR ALKALINITY
    ! -------------------------------------------------------------------------

    DERIVATIVES(1:nkn,TOT_ALK_INDEX) = ALK_KINETIC_DERIVATIVE

    PROCESS_RATES(1:nkn,TOT_ALK_INDEX, 1) = ALK_GAINED_BY_AMMONIUM_GEN
    PROCESS_RATES(1:nkn,TOT_ALK_INDEX, 2) = ALK_GAINED_BY_NITRATE_CONS
    PROCESS_RATES(1:nkn,TOT_ALK_INDEX, 3) = ALK_GAINED_BY_PHOSPHATE_CONS
    PROCESS_RATES(1:nkn,TOT_ALK_INDEX, 4) = ALK_LOST_BY_AMMONIUM_CONS
    PROCESS_RATES(1:nkn,TOT_ALK_INDEX, 5) = ALK_LOST_BY_NITRIFICATION
    PROCESS_RATES(1:nkn,TOT_ALK_INDEX, 6) = ALK_LOST_BY_PHOSPHATE_GEN
    PROCESS_RATES(1:nkn,TOT_ALK_INDEX, 7) = pH(1:nkn)

    if(debug_stranger) then
        call DBGSTR_PEL_DET_TOT_ALK_01 &
             (PROCESS_RATES, DERIVATIVES, PH, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if

    ! New kinetic derivatives calculations added 9 September 2015

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then
        ! After the introduction of changes in November 30 th 2015, it will be necessary
        ! to reconsinder kinetics, especially the mineral disoolotion processes for metals
        ! Job to be conducted wiht Petras together, and completed during next visit of
        ! of Ali to Lithuania. Also for iron and mangenese, the redox sequences as described
        ! by Van Chappen and Wang 2015 and Katsev papers should be included.


        ! FE_II
        PROCESS_RATES(1:nkn,FE_II_INDEX, 1) = R_FE_III_REDUCTION
        PROCESS_RATES(1:nkn,FE_II_INDEX, 2) = R_FE_II_OXIDATION

        ! Diagnostics:
        PROCESS_RATES(1:nkn,FE_II_INDEX, 3) = FE_II_DISS
        PROCESS_RATES(1:nkn,FE_II_INDEX, 4) = FE_II_DISS/FE_MOLAR_MASS_MG

        DERIVATIVES(1:nkn,FE_II_INDEX) = &
            PROCESS_RATES(1:nkn,FE_II_INDEX, 1) - PROCESS_RATES(1:nkn,FE_II_INDEX, 2)

        ! FE_III
        PROCESS_RATES(1:nkn,FE_III_INDEX, 1) = R_FE_II_OXIDATION
        PROCESS_RATES(1:nkn,FE_III_INDEX, 2) = R_FE_III_REDUCTION
        PROCESS_RATES(1:nkn,FE_III_INDEX, 3) = FE_III_DISS_EQ

        DERIVATIVES(1:nkn,FE_III_INDEX) = &
            PROCESS_RATES(1:nkn,FE_III_INDEX, 1) - PROCESS_RATES(1:nkn,FE_III_INDEX, 2)


        ! MN_II
        PROCESS_RATES(1:nkn,MN_II_INDEX, 1) = R_MN_IV_REDUCTION
        PROCESS_RATES(1:nkn,MN_II_INDEX, 2) = R_MN_II_OXIDATION

        DERIVATIVES(1:nkn,MN_II_INDEX) = &
            PROCESS_RATES(1:nkn,MN_II_INDEX, 1) - PROCESS_RATES(1:nkn,MN_II_INDEX, 2)


        ! MN_IV
        PROCESS_RATES(1:nkn,MN_IV_INDEX, 1) = R_MN_II_OXIDATION
        PROCESS_RATES(1:nkn,MN_IV_INDEX, 2) = R_MN_IV_REDUCTION

        DERIVATIVES(1:nkn,MN_IV_INDEX) = &
            PROCESS_RATES(1:nkn,MN_IV_INDEX, 1) - PROCESS_RATES(1:nkn,MN_IV_INDEX, 2)
        ! End of new kinetic derivatives calculations added 9 September 2015

        ! -------------------------------------------------------------------------
        ! END OF INSERT INTO KINETIC DERIVATIVES
        ! -------------------------------------------------------------------------


        ! Checking all derivatives
        if(debug_stranger) then
            call DBGSTR_PEL_GEN_02 &
                 (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
        end if

        ! Compact per-node/process clamp summary: one-shot, prints counts and sets CLAMP_WARNED
        if (allocated(CLAMP_COUNT)) then
            do i=1,nkn
                if (CLAMP_COUNT(i) > 0 .and. .not. CLAMP_WARNED(i)) then
                    write(6,'(A,F12.4,A,I4,A,I6)') &
                        'WARN SUMMARY: TIME=', TIME, ' BOX=', i, ' CLAMPS=', CLAMP_COUNT(i)
                    if (NDIAGVAR > 0 .and. allocated(CLAMP_PROC_COUNT)) then
                        do k=1,min(NDIAGVAR, size(CLAMP_PROC_COUNT,2))
                            if (CLAMP_PROC_COUNT(i,k) > 0) then
                                write(6,'(A,I3,A,I6)') '  PROC k=', k, ' cnt=', CLAMP_PROC_COUNT(i,k)
                            end if
                        end do
                    end if
                    CLAMP_WARNED(i) = .true.
                end if
            end do
        end if
    end if
    !*********************************************'
    !*                                           *'
    !*          END OF ECOLOGY KINETICS          *'
    !*                                           *'
    !*      DO NOT CHANGE THE FOLLOWING CODE     *'
    !*                                           *'
    !*********************************************'

end subroutine AQUABC_PELAGIC_KINETICS
