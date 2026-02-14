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
    !$ use omp_lib

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
    real(kind = DBL_PREC) :: loss, scale_loss

    ! OpenMP thread chunk variables (serial defaults: ns=1, ne=nkn)
    integer :: ns, ne, nkn_local, nthreads, tid, chunk_size

    ! Thread-local derived type bundles (private copies for each thread)
    type(t_phyto_env)      :: ENV_CHUNK
    type(t_redox_state)    :: REDOX_STATE_CHUNK
    type(t_redox_lim)      :: REDOX_LIM_CHUNK
    type(t_docmin_outputs) :: DOCMIN_CHUNK

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
    type(t_redox_params),    save :: REDOX_PARAMS
    type(t_docmin_params),   save :: DOCMIN_PARAMS

    ! Environmental input bundle (pointer-populated each timestep)
    type(t_phyto_env) :: PHYTO_ENV

    ! Redox state variables bundle (pointer-populated each timestep)
    type(t_redox_state) :: REDOX_STATE

    ! Redox limitation factors bundle (pointer-populated each timestep)
    type(t_redox_lim) :: REDOX_LIM

    ! DOC mineralization outputs bundle (pointer-populated each timestep)
    type(t_docmin_outputs) :: DOCMIN_OUTPUTS

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
        call populate_redox_params(REDOX_PARAMS)
        call populate_docmin_params(DOCMIN_PARAMS)

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
        ! Populate redox state bundle (zero-copy pointer assignment)
        REDOX_STATE%DOXY       => DISS_OXYGEN
        REDOX_STATE%NO3N       => NO3_N
        REDOX_STATE%MN_IV      => MN_IV
        REDOX_STATE%FE_III     => FE_III
        REDOX_STATE%S_PLUS_6   => S_PLUS_6
        REDOX_STATE%DISS_ORG_C => DISS_ORG_C
        REDOX_STATE%S_MINUS_2  => S_MINUS_2
        REDOX_STATE%MN_II      => MN_II
        REDOX_STATE%FE_II      => FE_II
        REDOX_STATE%HCO3       => HCO3
        REDOX_STATE%CO3        => CO3

        ! Populate redox limitation bundle (zero-copy pointer assignment)
        REDOX_LIM%LIM_DOXY_RED     => LIM_DOXY_RED
        REDOX_LIM%LIM_NO3N_RED     => LIM_NO3N_RED
        REDOX_LIM%LIM_MN_IV_RED    => LIM_MN_IV_RED
        REDOX_LIM%LIM_FE_III_RED   => LIM_FE_III_RED
        REDOX_LIM%LIM_S_PLUS_6_RED => LIM_S_PLUS_6_RED
        REDOX_LIM%LIM_DOC_RED      => LIM_DOC_RED

            call REDOX_AND_SPECIATION &
            (nkn, TEMP, SALT, PH, ELEVATION, &
             REDOX_PARAMS, REDOX_STATE, REDOX_LIM, &
             PE, FE_II_DISS_EQ, FE_III_DISS_EQ, MN_II_DISS)

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
                    elsewhere(FE_III .lt. 1.0D-20)
                        ! Guard against division by zero when total Fe3+ is depleted
                        FE_III_DISS      = 0.0D0
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

    ! =========================================================================
    ! BEGIN OpenMP PARALLEL REGION
    ! Each thread processes a contiguous chunk of nodes [ns:ne].
    ! When compiled without -fopenmp, !$ sentinel lines are comments and
    ! the serial defaults (ns=1, ne=nkn) give identical sequential behaviour.
    ! =========================================================================

    ! Serial defaults (overridden inside parallel region)
    ns = 1
    ne = nkn
    nkn_local = nkn

    !$omp parallel default(shared) &
    !$omp& private(ns, ne, nkn_local, tid, nthreads, chunk_size) &
    !$omp& private(ENV_CHUNK, REDOX_STATE_CHUNK, REDOX_LIM_CHUNK, DOCMIN_CHUNK) &
    !$omp& private(i, k, loss, scale_loss, total_removal, allowed_rate, scale) &
    !$omp& private(allowed_rate_local, old_rate, sum_removals)

    ! Compute per-thread chunk bounds
    nthreads = 1
    tid = 0
    !$ nthreads = omp_get_num_threads()
    !$ tid = omp_get_thread_num()
    chunk_size = (nkn + nthreads - 1) / nthreads
    ns = tid * chunk_size + 1
    ne = min(ns + chunk_size - 1, nkn)
    nkn_local = ne - ns + 1

    if (nkn_local > 0) then

    !*****************************************
    !     D I S S O L V E D  O X Y G E N     !
    !*****************************************
    do k=ns,ne
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
    PHYT_TOT_C(ns:ne) = DIA_C(ns:ne) + CYN_C(ns:ne) + OPA_C(ns:ne) + FIX_CYN_C(ns:ne) + NOST_VEG_HET_C(ns:ne)

    !**********************************!
    !**********************************!
    !     P H Y T O P L A N K T O N    !
    !**********************************!
    !**********************************!

    ! total chlorophyl in micrograms
    CHLA(ns:ne) = ((DIA_C(ns:ne)          / DIA_C_TO_CHLA    ) + (CYN_C(ns:ne) / CYN_C_TO_CHLA) + &
            (FIX_CYN_C(ns:ne)      / FIX_CYN_C_TO_CHLA) + (OPA_C(ns:ne) / OPA_C_TO_CHLA) + &
            (NOST_VEG_HET_C(ns:ne) / NOST_C_TO_CHLA)) * 1.0D3

    ! Debug: print CHLA components for node 1 to find negative values (include time/context)
    ! Note: Commented out to reduce log noise - uncomment for detailed CHLA debugging
    ! write(6,'(A,F12.4,A,ES10.3)') 'DEBUG: CHLA node1: TIME=', TIME, ' DT=', TIME_STEP
    ! write(6,'(A,5ES12.4)') '  DIA/CYN/FIX/OPA/NOST=', DIA_C(1), CYN_C(1), FIX_CYN_C(1), OPA_C(1), NOST_VEG_HET_C(1)

    select case (LIGHT_EXTINCTION_OPTION)

            case (0)
                call light_kd(K_B_E(ns:ne), K_E(ns:ne), CHLA(ns:ne), nkn_local)

            case (1)
                ! Defensive guard: ensure CHLA is non-negative before fractional exponent
                do i = ns, ne
                    if (CHLA(i) < 0.0D0) then
                        !$omp critical
                        write(6,'(A,I4,A,ES12.4)') 'WARN: negative CHLA at node ', i, ' value=', CHLA(i)
                        !$omp end critical
                    end if
                end do
                chla_pos(ns:ne) = CHLA(ns:ne)
                where (chla_pos(ns:ne) .lt. 0.0D0)
                    chla_pos(ns:ne) = 0.0D0
                end where
                K_E(ns:ne) = K_B_E(ns:ne) + (8.8D-3 * chla_pos(ns:ne)) + (5.4D-2 * (chla_pos(ns:ne) ** (2.0D0 / 3.0D0)))

        end select

        ! Debug print: K_E and DEPTH for troubleshooting NaNs (commented to reduce log noise)
        ! write(6,'(A,3ES12.4)') 'DEBUG: K_E(1)/DEPTH(1)/CHLA(1)=', K_E(1), DEPTH(1), CHLA(1)

    ! Populate per-thread phytoplankton environmental input bundle
    ENV_CHUNK%TEMP         => TEMP(ns:ne)
    ENV_CHUNK%I_A          => I_A(ns:ne)
    ENV_CHUNK%K_E          => K_E(ns:ne)
    ENV_CHUNK%DEPTH        => DEPTH(ns:ne)
    ENV_CHUNK%CHLA         => CHLA(ns:ne)
    ENV_CHUNK%FDAY         => FDAY(ns:ne)
    ENV_CHUNK%DISS_OXYGEN  => DISS_OXYGEN(ns:ne)
    ENV_CHUNK%WINDS        => WINDS(ns:ne)

    !********************
    !      DIATOMS      !
    !********************

    !Calculations for diatom growth
    call DIATOMS(DIA_PARAMS              , &
                 ENV_CHUNK               , &
                 DIA_LIGHT_SAT(ns:ne)           , &
                 NH4_N(ns:ne)                   , &
                 NO3_N(ns:ne)                   , &
                 (PO4_P(ns:ne) * DIP_OVER_IP(ns:ne))   , & ! Change, 6 July 2016, original call was PO4P
                 DIA_C(ns:ne)                   , &
                 ZOO_C(ns:ne)                   , &
                 DISS_Si(ns:ne)                 , &
                 TIME_STEP               , &
                 SMITH                   , &
                 nkn_local                     , &
                 KG_DIA(ns:ne)                  , &
                 ALPHA_0(ns:ne)                 , &
                 ALPHA_1(ns:ne)                 , &
                 LIM_KG_DIA_TEMP(ns:ne)         , &
                 LIM_KG_DIA_LIGHT(ns:ne)        , &
                 LIM_KG_DIA_DOXY(ns:ne)         , &
                 LIM_KG_DIA_N(ns:ne)            , &
                 LIM_KG_DIA_P(ns:ne)            , &
                 LIM_KG_DIA_DISS_Si(ns:ne)      , &
                 LIM_KG_DIA_NUTR(ns:ne)         , &
                 LIM_KG_DIA(ns:ne)              , &
                 R_DIA_GROWTH(ns:ne)            , &
                 R_DIA_MET(ns:ne)               , &
                 R_DIA_RESP(ns:ne)              , &
                 R_DIA_EXCR(ns:ne)              , &
                 R_DIA_INT_RESP(ns:ne)          , &
                 KD_DIA(ns:ne)                  , &
                 FAC_HYPOX_DIA_D(ns:ne)         , &
                 R_DIA_DEATH(ns:ne)             , &
                 PREF_NH4N_DIA(ns:ne))

    ! Consider the effect of growth inhibition which is supplied from outside
    ! by external models
    R_DIA_GROWTH(ns:ne) = R_DIA_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_DIA(ns:ne)

    !**************************************
    ! NON-NITROGEN FIXING CYANOBACTERIA   !
    !*************************************

    !Calculations for non-fixing cyanobacteria growth
    call CYANOBACTERIA_BOUYANT &
         (CYN_PARAMS              , &
          ENV_CHUNK               , &
          CYN_LIGHT_SAT(ns:ne)           , &
          NH4_N(ns:ne)                   , &
          NO3_N(ns:ne)                   , &
          DISS_ORG_N(ns:ne)              , &
          (PO4_P(ns:ne) * DIP_OVER_IP(ns:ne))   , &
          CYN_C(ns:ne)                   , &
          ZOO_C(ns:ne)                   , &
          TIME_STEP               , &
          SMITH                   , &
          nkn_local                     , &
          KG_CYN(ns:ne)                  , &
          ALPHA_0(ns:ne)                 , &
          ALPHA_1(ns:ne)                 , &
          LIM_KG_CYN_TEMP(ns:ne)         , &
          LIM_KG_CYN_LIGHT(ns:ne)        , &
          LIM_KG_CYN_DOXY(ns:ne)         , &
          LIM_KG_CYN_N(ns:ne)            , &
          LIM_KG_CYN_P(ns:ne)            , &
          LIM_KG_CYN_NUTR(ns:ne)         , &
          LIM_KG_CYN(ns:ne)              , &
          R_CYN_GROWTH(ns:ne)            , &
          R_CYN_MET(ns:ne)               , &
          R_CYN_RESP(ns:ne)              , &
          R_CYN_EXCR(ns:ne)              , &
          R_CYN_INT_RESP(ns:ne)          , &
          KD_CYN(ns:ne)                  , &
          FAC_HYPOX_CYN_D(ns:ne)         , &
          R_CYN_DEATH(ns:ne)             , &
          PREF_DIN_DON_CYN(ns:ne)        , &
          PREF_NH4N_CYN(ns:ne))

    ! Consider the effect of growth inhibition which is supplied from outside
    ! by external models
    R_CYN_GROWTH(ns:ne) = R_CYN_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_CYN(ns:ne)


    !********************************
    ! NITROGEN FIXING CYANOBACTERIA !
    !********************************
    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        call FIX_CYANOBACTERIA_BOUYANT  &
               (FIX_CYN_PARAMS               , &
                ENV_CHUNK                    , &
                TIME_STEP                    , &
                SMITH                        , &
                nkn_local                          , &
                NH4_N(ns:ne)                        , &
                NO3_N(ns:ne)                        , &
                DISS_ORG_N(ns:ne)                   , &
                (PO4_P(ns:ne) * DIP_OVER_IP(ns:ne))        , &
                FIX_CYN_C(ns:ne)                    , &
                FIX_CYN_LIGHT_SAT(ns:ne)            , &
                ALPHA_0(ns:ne)                      , &
                ALPHA_1(ns:ne)                      , &
                KG_FIX_CYN(ns:ne)                   , &
                LIM_KG_FIX_CYN_LIGHT(ns:ne)         , &
                LIM_KG_FIX_CYN_TEMP(ns:ne)          , &
                LIM_KG_FIX_CYN_DOXY(ns:ne)          , &
                LIM_KG_NON_FIX_CYN_N(ns:ne)         , &
                LIM_KG_NON_FIX_CYN_P(ns:ne)         , &
                LIM_KG_NON_FIX_CYN_NUTR(ns:ne)      , &
                LIM_KG_FIX_FIX_CYN_N(ns:ne)         , &
                LIM_KG_FIX_FIX_CYN_P(ns:ne)         , &
                LIM_KG_FIX_FIX_CYN_NUTR(ns:ne)      , &
                LIM_KG_NON_FIX_CYN(ns:ne)           , &
                LIM_KG_FIX_FIX_CYN(ns:ne)           , &
                R_NON_FIX_CYN_GROWTH(ns:ne)         , &
                R_FIX_FIX_CYN_GROWTH(ns:ne)         , &
                R_FIX_CYN_GROWTH(ns:ne)             , &
                R_FIX_CYN_MET(ns:ne)                , &
                R_FIX_CYN_RESP(ns:ne)               , &
                R_FIX_CYN_EXCR(ns:ne)               , &
                R_FIX_CYN_INT_RESP(ns:ne)           , &
                KD_FIX_CYN(ns:ne)                   , &
                FAC_HYPOX_FIX_CYN_D(ns:ne)          , &
                R_FIX_CYN_DEATH(ns:ne)              , &
                PREF_NH4N_DON_FIX_CYN(ns:ne))

        ! Consider the effect of growth inhibition which is supplied from outside
        ! by external models
        R_FIX_CYN_GROWTH(ns:ne)     = R_FIX_CYN_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_FIX_CYN(ns:ne)
        R_NON_FIX_CYN_GROWTH(ns:ne) = R_NON_FIX_CYN_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_FIX_CYN(ns:ne)
        R_FIX_FIX_CYN_GROWTH(ns:ne) = R_FIX_FIX_CYN_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_FIX_CYN(ns:ne)
    else
        FIX_CYN_C(ns:ne) = 0.0D0
        R_NON_FIX_CYN_GROWTH(ns:ne) = 0.0D0
        R_FIX_FIX_CYN_GROWTH(ns:ne) = 0.0D0
        R_FIX_CYN_GROWTH(ns:ne) = 0.0D0
        R_FIX_CYN_MET(ns:ne) = 0.0D0
        R_FIX_CYN_RESP(ns:ne) = 0.0D0
        R_FIX_CYN_EXCR(ns:ne) = 0.0D0
        R_FIX_CYN_INT_RESP(ns:ne) = 0.0D0
        R_FIX_CYN_DEATH(ns:ne) = 0.0D0
    end if


    !*****************************
    ! OTHER PLANKTONIC ALGAE     !
    !*****************************
    call OTHER_PLANKTONIC_ALGAE &
           (OPA_PARAMS              , &
            ENV_CHUNK               , &
            OPA_LIGHT_SAT(ns:ne)           , &
            NH4_N(ns:ne)                   , &
            NO3_N(ns:ne)                   , &
            (PO4_P(ns:ne) * DIP_OVER_IP(ns:ne))   , & ! Change, 6 July 2016, original call was PO4P
            OPA_C(ns:ne)                   , &
            ZOO_C(ns:ne)                   , &
            TIME_STEP               , &
            SMITH                   , &
            nkn_local                     , &
            KG_OPA(ns:ne)                  , &
            ALPHA_0(ns:ne)                 , &
            ALPHA_1(ns:ne)                 , &
            LIM_KG_OPA_TEMP(ns:ne)         , &
            LIM_KG_OPA_LIGHT(ns:ne)        , &
            LIM_KG_OPA_DOXY(ns:ne)         , &
            LIM_KG_OPA_N(ns:ne)            , &
            LIM_KG_OPA_P(ns:ne)            , &
            LIM_KG_OPA_NUTR(ns:ne)         , &
            LIM_KG_OPA(ns:ne)              , &
            R_OPA_GROWTH(ns:ne)            , &
            R_OPA_MET(ns:ne)               , &
            R_OPA_RESP(ns:ne)              , &
            R_OPA_EXCR(ns:ne)              , &
            R_OPA_INT_RESP(ns:ne)          , &
            KD_OPA(ns:ne)                  , &
            FAC_HYPOX_OPA_D(ns:ne)         , &
            R_OPA_DEATH(ns:ne)             , &
            PREF_NH4N_OPA(ns:ne))

        R_OPA_GROWTH(ns:ne) = R_OPA_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_OPA(ns:ne)
    !******************************!
    !******************************!
    !          NOSTOCALES          !
    !******************************!
    !******************************!


    if (DO_NOSTOCALES > 0) then

        ! Calls to the routines that should be incorporated to NOSTOCALES
        call DOP_DIP_PREFS(PREF_DIP_DOP_NOST(ns:ne), (frac_avail_DOP * DISS_ORG_P(ns:ne)), &
              (PO4_P(ns:ne) * DIP_OVER_IP(ns:ne)) , KHS_DP_NOST_VEG_HET, nkn_local)

        call DIN_DON_PREFS &
             (PREF_DIN_DON_NOST(ns:ne), NH4_N(ns:ne), DISS_ORG_N(ns:ne), &
              frac_avail_DON_NOST, NO3_N(ns:ne), KHS_DN_NOST_VEG_HET, nkn_local)

        call AMMONIA_PREFS &
            (PREF_NH4N_NOST(ns:ne), (NH4_N(ns:ne) * PREF_DIN_DON_NOST(ns:ne)), &
             (NO3_N(ns:ne) * PREF_DIN_DON_NOST(ns:ne)), KHS_DN_NOST_VEG_HET, nkn_local)

        call NOSTOCALES &
           (NOST_PARAMS                       , &
            ENV_CHUNK                         , &
            TIME_STEP                         , &
            DAY_OF_YEAR                       , &
            SMITH                             , &
            nkn_local                               , &
            NOST_LIGHT_SAT(ns:ne)                    , &
            (NH4_N(ns:ne) + NO3_N(ns:ne))                   , &
            frac_avail_DON_NOST * DISS_ORG_N(ns:ne)  , &
            (PO4_P(ns:ne) * DIP_OVER_IP(ns:ne) + frac_avail_DOP * DISS_ORG_P(ns:ne)) , &
            NOST_VEG_HET_C(ns:ne)                    , &
            NOST_AKI_C(ns:ne)                        , &
            KG_NOST_VEG_HET(ns:ne)                   , &
            LIM_KG_NOST_VEG_HET_LIGHT(ns:ne)         , &
            LIM_KG_NOST_VEG_HET_TEMP(ns:ne)          , &
            LIM_KG_NOST_VEG_HET_DOXY(ns:ne)          , &
            LIM_KG_NOST_VEG_HET_N(ns:ne)             , &
            LIM_KG_NOST_VEG_HET_P(ns:ne)             , &
            LIM_KG_NOST_VEG_HET_FIX(ns:ne)           , &
            LIM_KG_NOST_VEG_HET_NON_FIX(ns:ne)       , &
            R_NOST_VEG_HET_GROWTH(ns:ne)             , &
            R_NOST_VEG_HET_FIX_GROWTH(ns:ne)         , &
            R_NOST_VEG_HET_NON_FIX_GROWTH(ns:ne)     , &
            R_NOST_VEG_HET_MET(ns:ne)                , &
            R_NOST_VEG_HET_RESP(ns:ne)               , &
            R_NOST_VEG_HET_EXCR(ns:ne)               , &
            R_NOST_VEG_HET_INT_RESP(ns:ne)           , &
            KD_NOST_VEG_HET(ns:ne)                   , &
            FAC_HYPOX_NOST_VEG_HET_D(ns:ne)          , &
            R_NOST_VEG_HET_DEATH(ns:ne)              , &
            R_DENS_MORT_NOST_VEG_HET(ns:ne)          , &
            R_GERM_NOST_AKI(ns:ne)                   , &
            R_FORM_NOST_AKI(ns:ne)                   , &
            R_LOSS_AKI(ns:ne)                        , &
            R_MORT_AKI(ns:ne))

            ! Consider the effect of growth inhibition which is supplied from outside
            ! by external models
            R_NOST_VEG_HET_GROWTH(ns:ne) = &
                R_NOST_VEG_HET_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_NOST(ns:ne)

            R_NOST_VEG_HET_NON_FIX_GROWTH(ns:ne) = &
                R_NOST_VEG_HET_NON_FIX_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_NOST(ns:ne)

            R_NOST_VEG_HET_FIX_GROWTH(ns:ne) = &
                R_NOST_VEG_HET_FIX_GROWTH(ns:ne) * GROWTH_INHIB_FACTOR_NOST(ns:ne)
    else
        R_NOST_VEG_HET_GROWTH(ns:ne)    = 0.0D0
        R_NOST_VEG_HET_MET(ns:ne)       = 0.0D0
        R_NOST_VEG_HET_RESP(ns:ne)      = 0.0D0
        R_NOST_VEG_HET_EXCR(ns:ne)      = 0.0D0
        R_NOST_VEG_HET_INT_RESP(ns:ne)  = 0.0D0
        R_NOST_VEG_HET_DEATH(ns:ne)     = 0.0D0
        R_DENS_MORT_NOST_VEG_HET(ns:ne) = 0.0D0
        R_GERM_NOST_AKI(ns:ne)          = 0.0D0
        R_FORM_NOST_AKI(ns:ne)          = 0.0D0
        R_LOSS_AKI(ns:ne)               = 0.0D0
        R_MORT_AKI(ns:ne)               = 0.0D0
    end if


    !******************************!
    !******************************!
    !     Z O O P L A N K T O N    !
    !******************************!
    !******************************!
    call ZOOPLANKTON &
           (ZOO_PARAMS                       , &
            ENV_CHUNK                        , &
            DIA_C(ns:ne)                         , &
            CYN_C(ns:ne)                         , &
            OPA_C(ns:ne)                         , &
            FIX_CYN_C(ns:ne)                     , &
            NOST_VEG_HET_C(ns:ne)                , &
            DET_PART_ORG_C(ns:ne)                , &
            ZOO_C(ns:ne)                         , &
            TIME_STEP                     , &
            nkn_local                           , &
            KG_ZOO(ns:ne)                        , &
            KG_ZOO_DIA(ns:ne)                    , &
            KG_ZOO_CYN(ns:ne)                    , &
            KG_ZOO_OPA(ns:ne)                    , &
            KG_ZOO_FIX_CYN(ns:ne)                , &
            KG_ZOO_NOST_VEG_HET(ns:ne)           , &
            KG_ZOO_DET_PART_ORG_C(ns:ne)         , &
            KD_ZOO(ns:ne)                        , &
            FOOD_FACTOR_ZOO_DIA(ns:ne)           , &
            FOOD_FACTOR_ZOO_CYN(ns:ne)           , &
            FOOD_FACTOR_ZOO_OPA(ns:ne)           , &
            FOOD_FACTOR_ZOO_FIX_CYN(ns:ne)       , &
            FOOD_FACTOR_ZOO_NOST_VEG_HET(ns:ne)  , &
            FOOD_FACTOR_ZOO_DET_PART_ORG_C(ns:ne), &
            R_ZOO_FEEDING_DIA(ns:ne)             , &
            R_ZOO_FEEDING_CYN(ns:ne)             , &
            R_ZOO_FEEDING_FIX_CYN(ns:ne)         , &
            R_ZOO_FEEDING_NOST_VEG_HET(ns:ne)    , &
            R_ZOO_FEEDING_OPA(ns:ne)             , &
            R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne)  , &
            R_ZOO_INT_RESP(ns:ne)                , &
            R_ZOO_RESP(ns:ne)                    , &
            R_ZOO_EX_DON(ns:ne)                  , &
            R_ZOO_EX_DOP(ns:ne)                  , &
            R_ZOO_EX_DOC(ns:ne)                  , &
            R_ZOO_DEATH(ns:ne)                   , &
            ACTUAL_ZOO_N_TO_C(ns:ne)             , &
            ACTUAL_ZOO_P_TO_C(ns:ne)             , &
            R_ZOO_GROWTH(ns:ne)                  , &
            FAC_HYPOX_ZOO_D(ns:ne))

    R_ZOO_FEEDING_DIA(ns:ne) = &
        R_ZOO_FEEDING_DIA(ns:ne) * GROWTH_INHIB_FACTOR_ZOO(ns:ne)

    R_ZOO_FEEDING_CYN(ns:ne) = &
        R_ZOO_FEEDING_CYN(ns:ne) * GROWTH_INHIB_FACTOR_ZOO(ns:ne)

    R_ZOO_FEEDING_FIX_CYN(ns:ne) = &
        R_ZOO_FEEDING_FIX_CYN(ns:ne) * GROWTH_INHIB_FACTOR_ZOO(ns:ne)

    R_ZOO_FEEDING_NOST_VEG_HET(ns:ne) = &
        R_ZOO_FEEDING_NOST_VEG_HET(ns:ne) * GROWTH_INHIB_FACTOR_ZOO(ns:ne)

    R_ZOO_FEEDING_OPA(ns:ne) = &
        R_ZOO_FEEDING_OPA(ns:ne) * GROWTH_INHIB_FACTOR_ZOO(ns:ne)

    R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne) = &
        R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne) * GROWTH_INHIB_FACTOR_ZOO(ns:ne)

    if (ZOOP_OPTION_1 > 0) then
        ACTUAL_ZOO_N_TO_C(ns:ne) = ZOO_N(ns:ne) / max(ZOO_C(ns:ne), MIN_CONCENTRATION)
        ACTUAL_ZOO_P_TO_C(ns:ne) = ZOO_P(ns:ne) / max(ZOO_C(ns:ne), MIN_CONCENTRATION)
    end if

    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_R_ZOO_GROWTH_01(TIME, nkn, nstate, node_active, error)
    end if
    !$omp end master
    !$omp barrier

    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_R_ZOO_RESP_01(TIME, nkn, nstate, node_active, error)
    end if
    !$omp end master
    !$omp barrier


    !*********************************************************************!
    !     D E A T H   O R G A N I C     P A R T I C L E S    DISSOLUTION !
    !*********************************************************************!

    call ORGANIC_CARBON_DISSOLUTION &
           (FAC_PHYT_DET_PART_ORG_C     , &
            KDISS_DET_PART_ORG_C_20     , &
            THETA_KDISS_DET_PART_ORG_C  , &
            KHS_POC_DISS_SAT            , &
            nkn_local                         , &
            TEMP(ns:ne)                        , &
            DET_PART_ORG_C(ns:ne)              , &
            PHYT_TOT_C(ns:ne)                  , &
            LIM_PHYT_DISS_DET_PART_ORG_C(ns:ne), &
            R_DET_PART_ORG_C_DISSOLUTION(ns:ne))

    ACTUAL_DET_N_TO_C(ns:ne) = DET_PART_ORG_N(ns:ne) / max(DET_PART_ORG_C(ns:ne), MIN_CONCENTRATION)
    ACTUAL_DET_P_TO_C(ns:ne) = DET_PART_ORG_P(ns:ne) / max(DET_PART_ORG_C(ns:ne), MIN_CONCENTRATION)

    ! Nitrogen dissolution

    ! Accerelation of hydrolysis when DIN is scarce
    LIM_N_DISS_DET_PART_ORG_N(ns:ne) = KHS_DISS_N / (KHS_DISS_N + (NH4_N(ns:ne) + NO3_N(ns:ne)))
    LIM_PHY_N_DISS_DET_PART_ORG_N(ns:ne) = LIM_N_DISS_DET_PART_ORG_N(ns:ne) * FAC_PHYT_DET_PART_ORG_N * PHYT_TOT_C(ns:ne)

    R_DET_PART_ORG_N_DISSOLUTION(ns:ne) = (KDISS_DET_PART_ORG_N_20 + LIM_PHY_N_DISS_DET_PART_ORG_N(ns:ne)) * &
       (THETA_KDISS_DET_PART_ORG_N ** (TEMP(ns:ne) - 2.0D1)) * DET_PART_ORG_N(ns:ne) * &
       (KHS_PON_DISS_SAT/(DET_PART_ORG_N(ns:ne) + KHS_PON_DISS_SAT))

    ! Phosphorus dissolution

    ! Accerelation of hydrolysis when DIP is scarce
    LIM_P_DISS_DET_PART_ORG_P(ns:ne) = KHS_DISS_P / (KHS_DISS_P + DIP_OVER_IP(ns:ne)*PO4_P(ns:ne))
    LIM_PHY_P_DISS_DET_PART_ORG_P(ns:ne) = LIM_P_DISS_DET_PART_ORG_P(ns:ne) * FAC_PHYT_DET_PART_ORG_P * PHYT_TOT_C(ns:ne)

    R_DET_PART_ORG_P_DISSOLUTION(ns:ne) = (KDISS_DET_PART_ORG_P_20 + LIM_PHY_P_DISS_DET_PART_ORG_P(ns:ne)) * &
       (THETA_KDISS_DET_PART_ORG_P ** (TEMP(ns:ne) - 2.0D1)) * DET_PART_ORG_P(ns:ne) * &
       (KHS_POP_DISS_SAT/(DET_PART_ORG_P(ns:ne) + KHS_POP_DISS_SAT))

    !Diatom total respiration rate
    R_DIA_TOT_RESP(ns:ne)               = R_DIA_RESP(ns:ne)                + R_DIA_INT_RESP(ns:ne)

    !Non-fixing cyanobacteria total respiration rate
    R_CYN_TOT_RESP(ns:ne)               = R_CYN_RESP(ns:ne)                + R_CYN_INT_RESP(ns:ne)

    !Other planktonic algae total respiration rate
    R_OPA_TOT_RESP(ns:ne)               = R_OPA_RESP(ns:ne)                + R_OPA_INT_RESP(ns:ne)

    !Nitrogen fixing cyanobacteria total respiration rate
    R_FIX_CYN_TOT_RESP(ns:ne)           = R_FIX_CYN_RESP(ns:ne)            + R_FIX_CYN_INT_RESP(ns:ne)

    !Nostacles
    R_NOST_VEG_HET_TOT_RESP(ns:ne)      = R_NOST_VEG_HET_RESP(ns:ne)       + R_NOST_VEG_HET_INT_RESP(ns:ne)

    !Zooplankton total respiration rate
    R_ZOO_TOT_RESP(ns:ne)               = R_ZOO_INT_RESP(ns:ne)            + R_ZOO_RESP(ns:ne)


    !*********************************************************************!
    !     SILICON                                                         !
    !*********************************************************************!

    !Dissolution rate of biogenic silicon
    R_PART_Si_DISS(ns:ne) = KDISS_PART_SI_20 * &
            (THETA_KDISS_PART_SI ** (TEMP(ns:ne) - 2.0D1)) * PART_SI(ns:ne)


    !*********************************************************************!
    !     MINERALIZATION OF DOC, DON, DOP whith bacteria are not modelled.
    !     Called abiotic in the sense of modelling method
    !*********************************************************************!

    !Algal dependent mineralisation rate

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then
        ! Populate per-thread DOC mineralization outputs bundle
        DOCMIN_CHUNK%LIM_PHYT_AMIN_DOC          => LIM_PHYT_AMIN_DOC(ns:ne)
        DOCMIN_CHUNK%R_ABIOTIC_DOC_MIN_DOXY     => R_ABIOTIC_DOC_MIN_DOXY(ns:ne)
        DOCMIN_CHUNK%R_ABIOTIC_DOC_MIN_NO3N     => R_ABIOTIC_DOC_MIN_NO3N(ns:ne)
        DOCMIN_CHUNK%R_ABIOTIC_DOC_MIN_MN_IV    => R_ABIOTIC_DOC_MIN_MN_IV(ns:ne)
        DOCMIN_CHUNK%R_ABIOTIC_DOC_MIN_FE_III   => R_ABIOTIC_DOC_MIN_FE_III(ns:ne)
        DOCMIN_CHUNK%R_ABIOTIC_DOC_MIN_S_PLUS_6 => R_ABIOTIC_DOC_MIN_S_PLUS_6(ns:ne)
        DOCMIN_CHUNK%R_ABIOTIC_DOC_MIN_DOC      => R_ABIOTIC_DOC_MIN_DOC(ns:ne)
        DOCMIN_CHUNK%PH_CORR_DOC_MIN_DOXY       => PH_CORR_DOC_MIN_DOXY(ns:ne)
        DOCMIN_CHUNK%PH_CORR_DOC_MIN_NO3N       => PH_CORR_DOC_MIN_NO3N(ns:ne)
        DOCMIN_CHUNK%PH_CORR_DOC_MIN_MN_IV      => PH_CORR_DOC_MIN_MN_IV(ns:ne)
        DOCMIN_CHUNK%PH_CORR_DOC_MIN_FE_III     => PH_CORR_DOC_MIN_FE_III(ns:ne)
        DOCMIN_CHUNK%PH_CORR_DOC_MIN_S_PLUS_6   => PH_CORR_DOC_MIN_S_PLUS_6(ns:ne)
        DOCMIN_CHUNK%PH_CORR_DOC_MIN_DOC        => PH_CORR_DOC_MIN_DOC(ns:ne)

        ! Populate per-thread redox state and limitation bundles
        REDOX_STATE_CHUNK%DOXY       => DISS_OXYGEN(ns:ne)
        REDOX_STATE_CHUNK%NO3N       => NO3_N(ns:ne)
        REDOX_STATE_CHUNK%MN_IV      => MN_IV(ns:ne)
        REDOX_STATE_CHUNK%FE_III     => FE_III(ns:ne)
        REDOX_STATE_CHUNK%S_PLUS_6   => S_PLUS_6(ns:ne)
        REDOX_STATE_CHUNK%DISS_ORG_C => DISS_ORG_C(ns:ne)
        REDOX_STATE_CHUNK%S_MINUS_2  => S_MINUS_2(ns:ne)
        REDOX_STATE_CHUNK%MN_II      => MN_II(ns:ne)
        REDOX_STATE_CHUNK%FE_II      => FE_II(ns:ne)
        REDOX_STATE_CHUNK%HCO3       => HCO3(ns:ne)
        REDOX_STATE_CHUNK%CO3        => CO3(ns:ne)

        REDOX_LIM_CHUNK%LIM_DOXY_RED     => LIM_DOXY_RED(ns:ne)
        REDOX_LIM_CHUNK%LIM_NO3N_RED     => LIM_NO3N_RED(ns:ne)
        REDOX_LIM_CHUNK%LIM_MN_IV_RED    => LIM_MN_IV_RED(ns:ne)
        REDOX_LIM_CHUNK%LIM_FE_III_RED   => LIM_FE_III_RED(ns:ne)
        REDOX_LIM_CHUNK%LIM_S_PLUS_6_RED => LIM_S_PLUS_6_RED(ns:ne)
        REDOX_LIM_CHUNK%LIM_DOC_RED      => LIM_DOC_RED(ns:ne)

        call ORGANIC_CARBON_MINERALIZATION &
                (nkn_local                   , &
                 TEMP(ns:ne)                        , &
                 PH(ns:ne)                          , &
                 PHYT_TOT_C(ns:ne)                  , &
                 DOCMIN_PARAMS               , &
                 REDOX_PARAMS                , &
                 REDOX_STATE_CHUNK           , &
                 REDOX_LIM_CHUNK             , &
                 DOCMIN_CHUNK)
    else
        where (PHYT_TOT_C(ns:ne) .gt. K_MIN_PHYT_AMIN_DOC)
            LIM_PHYT_AMIN_DOC(ns:ne) = FAC_PHYT_AMIN_DOC * (PHYT_TOT_C(ns:ne) - K_MIN_PHYT_AMIN_DOC)
        elsewhere
            LIM_PHYT_AMIN_DOC(ns:ne) = 0.D0
        end where

        call CALCULATE_PH_CORR &
             (PH_CORR_DOC_MIN_DOXY(ns:ne), PH(ns:ne), PH_MIN_DOC_MIN_DOXY, PH_MAX_DOC_MIN_DOXY, nkn_local)

        call CALCULATE_PH_CORR &
             (PH_CORR_DOC_MIN_NO3N(ns:ne), PH(ns:ne), PH_MIN_DOC_MIN_NO3N, PH_MAX_DOC_MIN_NO3N, nkn_local)

        R_ABIOTIC_DOC_MIN_DOXY(ns:ne) = &
            (K_MIN_DOC_DOXY_20 + LIM_PHYT_AMIN_DOC(ns:ne)) * &
            (THETA_K_MIN_DOC_DOXY ** (TEMP(ns:ne) - 2.0D1)) * LIM_DOXY_RED(ns:ne) * &
            PH_CORR_DOC_MIN_DOXY(ns:ne) * &
            (DISS_ORG_C(ns:ne) / (DISS_ORG_C(ns:ne) + K_HS_DOC_MIN_DOXY)) * DISS_ORG_C(ns:ne)

        R_ABIOTIC_DOC_MIN_NO3N(ns:ne) = &
            K_MIN_DOC_NO3N_20  * (THETA_K_MIN_DOC_NO3N ** (TEMP(ns:ne) - 2.0D1)) * &
            LIM_NO3N_RED(ns:ne) * PH_CORR_DOC_MIN_NO3N(ns:ne) * &
            (DISS_ORG_C(ns:ne) / (DISS_ORG_C(ns:ne) + K_HS_DOC_MIN_NO3N)) * DISS_ORG_C(ns:ne)

        R_ABIOTIC_DOC_MIN_MN_IV(ns:ne)    = 0.0D0
        R_ABIOTIC_DOC_MIN_FE_III(ns:ne)   = 0.0D0
        R_ABIOTIC_DOC_MIN_S_PLUS_6(ns:ne) = 0.0D0
        R_ABIOTIC_DOC_MIN_DOC(ns:ne)      = 0.0D0
    end if

    ! Accerelation of mineralisation when DIN is scarce
    LIM_N_AMIN_DON(ns:ne) = KHS_AMIN_N / (KHS_AMIN_N + (NH4_N(ns:ne) + NO3_N(ns:ne)))

    where (PHYT_TOT_C(ns:ne) .gt. K_MIN_PHYT_AMIN_DON)
        LIM_PHY_N_AMIN_DON(ns:ne) = LIM_N_AMIN_DON(ns:ne) * FAC_PHYT_AMIN_DON * (PHYT_TOT_C(ns:ne) - K_MIN_PHYT_AMIN_DON)
    elsewhere
        LIM_PHY_N_AMIN_DON(ns:ne) = 0.D0
    end where

    ! -------------------------------------------------------------------------
    ! DON mineralization compatible with redox cycle
    ! -------------------------------------------------------------------------

    ! 28 January 2016, the following commented lines are replaced in order to be
    ! compitable with the redox sequence

    !(1 - frac_avail_DON) counts fraction available for minerasilation bybacteria
    !frac_avail_DON - fraction available for cyanobacteria

    call CALCULATE_PH_CORR &
         (PH_CORR_DON_MIN_DOXY(ns:ne), PH(ns:ne), PH_MIN_DON_MIN_DOXY, PH_MAX_DON_MIN_DOXY, nkn_local)

    call CALCULATE_PH_CORR &
         (PH_CORR_DON_MIN_NO3N(ns:ne), PH(ns:ne), PH_MIN_DON_MIN_NO3N, PH_MAX_DON_MIN_NO3N, nkn_local)

    where(DISS_ORG_N(ns:ne) .le. 0.D0)
        LIM_DON_DON(ns:ne) = 0.D0
    elsewhere
        LIM_DON_DON(ns:ne) = DISS_ORG_N(ns:ne) / (DISS_ORG_N(ns:ne) + K_HS_DON_MIN_DOXY)
    end where

    R_ABIOTIC_DON_MIN_DOXY(ns:ne) = &
        (K_MIN_DON_DOXY_20 + LIM_PHY_N_AMIN_DON(ns:ne)) * &
        (THETA_K_MIN_DON_DOXY ** (TEMP(ns:ne) - 2.0D1)) * &
        LIM_DOXY_RED(ns:ne) * PH_CORR_DON_MIN_DOXY(ns:ne) * &
        LIM_DON_DON(ns:ne) * &
        DISS_ORG_N(ns:ne) !(1.0D0 - frac_avail_DON)

    ! No phytoplankton or cyanobacteria when there is no oxygen so mineralization
    ! rate calculation differs
    R_ABIOTIC_DON_MIN_NO3N(ns:ne) = &
        K_MIN_DON_NO3N_20  * (THETA_K_MIN_DON_NO3N ** (TEMP(ns:ne) - 2.0D1)) * &
        LIM_NO3N_RED(ns:ne) * PH_CORR_DON_MIN_NO3N(ns:ne) * &
        (DISS_ORG_N(ns:ne) / (DISS_ORG_N(ns:ne) + K_HS_DON_MIN_NO3N)) * DISS_ORG_N(ns:ne)

    R_ABIOTIC_DON_MIN_MN_IV(ns:ne)    = 0.0D0
    R_ABIOTIC_DON_MIN_FE_III(ns:ne)   = 0.0D0
    R_ABIOTIC_DON_MIN_S_PLUS_6(ns:ne) = 0.0D0
    R_ABIOTIC_DON_MIN_DOC(ns:ne)      = 0.0D0

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then

        call CALCULATE_PH_CORR &
             (PH_CORR_DON_MIN_MN_IV(ns:ne)   , PH(ns:ne), PH_MIN_DON_MIN_MN_IV   , &
              PH_MAX_DON_MIN_MN_IV   , nkn_local)

        call CALCULATE_PH_CORR &
             (PH_CORR_DON_MIN_FE_III(ns:ne)  , PH(ns:ne), PH_MIN_DON_MIN_FE_III  , &
              PH_MAX_DON_MIN_FE_III  , nkn_local)

        call CALCULATE_PH_CORR &
             (PH_CORR_DON_MIN_S_PLUS_6(ns:ne), PH(ns:ne), PH_MIN_DON_MIN_S_PLUS_6, &
              PH_MAX_DON_MIN_S_PLUS_6, nkn_local)

        call CALCULATE_PH_CORR &
             (PH_CORR_DON_MIN_DOC(ns:ne)     , PH(ns:ne), PH_MIN_DON_MIN_DOC     , &
              PH_MAX_DON_MIN_DOC     , nkn_local)

        R_ABIOTIC_DON_MIN_MN_IV(ns:ne)   = &
            K_MIN_DON_MN_IV_20     * (THETA_K_MIN_DON_MN_IV    ** (TEMP(ns:ne) - 2.0D1)) * &
            LIM_MN_IV_RED(ns:ne) * PH_CORR_DON_MIN_MN_IV(ns:ne) * &
            (DISS_ORG_N(ns:ne) / (DISS_ORG_N(ns:ne) + K_HS_DON_MIN_MN_IV)) * DISS_ORG_N(ns:ne)

        R_ABIOTIC_DON_MIN_FE_III(ns:ne)   = &
            K_MIN_DON_FE_III_20    * (THETA_K_MIN_DON_FE_III   ** (TEMP(ns:ne) - 2.0D1)) * &
            LIM_FE_III_RED(ns:ne) * PH_CORR_DON_MIN_FE_III(ns:ne) * &
            (DISS_ORG_N(ns:ne) / (DISS_ORG_N(ns:ne) + K_HS_DON_MIN_FE_III)) * DISS_ORG_N(ns:ne)

        R_ABIOTIC_DON_MIN_S_PLUS_6(ns:ne) = &
            K_MIN_DON_S_PLUS_6_20  * (THETA_K_MIN_DON_S_PLUS_6 ** (TEMP(ns:ne) - 2.0D1)) * &
            LIM_S_PLUS_6_RED(ns:ne) * PH_CORR_DON_MIN_S_PLUS_6(ns:ne) * &
            (DISS_ORG_N(ns:ne) / (DISS_ORG_N(ns:ne) + K_HS_DON_MIN_S_PLUS_6)) * DISS_ORG_N(ns:ne)

        R_ABIOTIC_DON_MIN_DOC(ns:ne)      = &
            (K_MIN_DON_DOC_20      * (THETA_K_MIN_DON_DOC      ** (TEMP(ns:ne) - 2.0D1)) * &
            LIM_DOC_RED(ns:ne) * PH_CORR_DON_MIN_DOC(ns:ne) * &
            (DISS_ORG_N(ns:ne) / (DISS_ORG_N(ns:ne) + K_HS_DON_MIN_DOC)) * DISS_ORG_N(ns:ne))
    end if


    ! -------------------------------------------------------------------------
    ! End of DON mineralization compatible with redox cycle
    ! -------------------------------------------------------------------------

    ! Accerelation of mineralisation when DIP is scarce
    LIM_P_AMIN_DOP(ns:ne) = KHS_AMIN_P / (KHS_AMIN_P + DIP_OVER_IP(ns:ne)*PO4_P(ns:ne))



    where (PHYT_TOT_C(ns:ne) .gt. K_MIN_PHYT_AMIN_DOP)
        LIM_PHY_P_AMIN_DOP(ns:ne) = LIM_P_AMIN_DOP(ns:ne) * FAC_PHYT_AMIN_DOP * (PHYT_TOT_C(ns:ne) - K_MIN_PHYT_AMIN_DOP)
    elsewhere
        LIM_PHY_P_AMIN_DOP(ns:ne) = 0.D0
    end where

    ! -------------------------------------------------------------------------
    ! DOP mineralization compatible with redox cycle
    ! -------------------------------------------------------------------------

    call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_DOXY(ns:ne), PH(ns:ne), PH_MIN_DOP_MIN_DOXY, PH_MAX_DOP_MIN_DOXY, nkn_local)
    call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_NO3N(ns:ne), PH(ns:ne), PH_MIN_DOP_MIN_NO3N, PH_MAX_DOP_MIN_NO3N, nkn_local)

    where (DISS_ORG_P(ns:ne) .le. 0.D0 .and. K_HS_DOP_MIN_DOXY .le. 0.D0)
        LIM_DISS_ORG_P(ns:ne) = 1.D0
    elsewhere
        LIM_DISS_ORG_P(ns:ne) = (DISS_ORG_P(ns:ne) / (DISS_ORG_P(ns:ne) + K_HS_DOP_MIN_DOXY))
    end where

    !$omp critical
    if(any(DISS_ORG_P(ns:ne) .le. 0.D0)) then
     print *, 'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW'
     print *, 'PELAGIC MODEL: Warning, some DISS_ORG_P <= 0'
     print *, 'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW'
    end if
    !$omp end critical

    R_ABIOTIC_DOP_MIN_DOXY(ns:ne) = &
        (K_MIN_DOP_DOXY_20 + LIM_PHY_P_AMIN_DOP(ns:ne)) * (THETA_K_MIN_DOP_DOXY ** (TEMP(ns:ne) - 2.0D1)) * &
        LIM_DOXY_RED(ns:ne) * PH_CORR_DOP_MIN_DOXY(ns:ne) * LIM_DISS_ORG_P(ns:ne) * &
        DISS_ORG_P(ns:ne)

    ! No phytoplankton or cyanobacteria when there is no oxygen so mineralization
    ! rate calculation differs
    R_ABIOTIC_DOP_MIN_NO3N(ns:ne) = &
        K_MIN_DOP_NO3N_20  * (THETA_K_MIN_DOP_NO3N ** (TEMP(ns:ne) - 2.0D1)) * &
        LIM_NO3N_RED(ns:ne) * PH_CORR_DOP_MIN_NO3N(ns:ne) * (DISS_ORG_P(ns:ne) / (DISS_ORG_P(ns:ne) + K_HS_DOP_MIN_NO3N)) * &
        DISS_ORG_P(ns:ne)

    R_ABIOTIC_DOP_MIN_MN_IV(ns:ne)    = 0.0D0
    R_ABIOTIC_DOP_MIN_FE_III(ns:ne)   = 0.0D0
    R_ABIOTIC_DOP_MIN_S_PLUS_6(ns:ne) = 0.0D0
    R_ABIOTIC_DOP_MIN_DOC(ns:ne)      = 0.0D0

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then
        call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_MN_IV(ns:ne), PH(ns:ne), &
             PH_MIN_DOP_MIN_MN_IV, PH_MAX_DOP_MIN_MN_IV, nkn_local)
        call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_FE_III(ns:ne), PH(ns:ne), &
             PH_MIN_DOP_MIN_FE_III, PH_MAX_DOP_MIN_FE_III, nkn_local)
        call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_S_PLUS_6(ns:ne), PH(ns:ne), &
             PH_MIN_DOP_MIN_S_PLUS_6, PH_MAX_DOP_MIN_S_PLUS_6, nkn_local)
        call CALCULATE_PH_CORR(PH_CORR_DOP_MIN_DOC(ns:ne), PH(ns:ne), &
             PH_MIN_DOP_MIN_DOC, PH_MAX_DOP_MIN_DOC, nkn_local)

        R_ABIOTIC_DOP_MIN_MN_IV(ns:ne) = &
            K_MIN_DOP_MN_IV_20  * (THETA_K_MIN_DOP_MN_IV ** (TEMP(ns:ne) - 2.0D1)) * &
            LIM_MN_IV_RED(ns:ne) * PH_CORR_DOP_MIN_MN_IV(ns:ne) * &
            (DISS_ORG_P(ns:ne) / (DISS_ORG_P(ns:ne) + K_HS_DOP_MIN_MN_IV)) * DISS_ORG_P(ns:ne)

        R_ABIOTIC_DOP_MIN_FE_III(ns:ne) = &
            K_MIN_DOP_FE_III_20  * (THETA_K_MIN_DOP_FE_III ** (TEMP(ns:ne) - 2.0D1)) * &
            LIM_FE_III_RED(ns:ne) * PH_CORR_DOP_MIN_FE_III(ns:ne) * &
            (DISS_ORG_P(ns:ne) / (DISS_ORG_P(ns:ne) + K_HS_DOP_MIN_FE_III)) * DISS_ORG_P(ns:ne)

        R_ABIOTIC_DOP_MIN_S_PLUS_6(ns:ne) = &
            K_MIN_DOP_S_PLUS_6_20  * (THETA_K_MIN_DOP_S_PLUS_6 ** (TEMP(ns:ne) - 2.0D1)) * &
            LIM_S_PLUS_6_RED(ns:ne) * PH_CORR_DOP_MIN_S_PLUS_6(ns:ne) * &
            (DISS_ORG_P(ns:ne) / (DISS_ORG_P(ns:ne) + K_HS_DOP_MIN_S_PLUS_6)) * DISS_ORG_P(ns:ne)

        R_ABIOTIC_DOP_MIN_DOC(ns:ne) = &
            (K_MIN_DOP_DOC_20  * (THETA_K_MIN_DOP_DOC ** (TEMP(ns:ne) - 2.0D1)) * &
             LIM_DOC_RED(ns:ne) * PH_CORR_DOP_MIN_DOC(ns:ne) * &
            (DISS_ORG_P(ns:ne) / (DISS_ORG_P(ns:ne) + K_HS_DOP_MIN_DOC)) * DISS_ORG_P(ns:ne))
    end if
    ! -------------------------------------------------------------------------
    ! End of DOP mineralization compatible with redox cycle
    ! -------------------------------------------------------------------------

    !*******************************************************************************************!
    !     Nitrification of ammonia by bacteria are not modelled.
    !     Called abiotic in the sense of modelling method
    !*******************************************************************************************!
    LIM_NITR_OXY(ns:ne) = DISS_OXYGEN(ns:ne) / (KHS_NITR_OXY + DISS_OXYGEN(ns:ne))
    LIM_NITR_NH4_N(ns:ne) = NH4_N(ns:ne) / (KHS_NITR_NH4_N + NH4_N(ns:ne))

    call CALCULATE_PH_CORR(PH_CORR_NITR_NH4(ns:ne), PH(ns:ne), PH_NITR_NH4_MIN, PH_NITR_NH4_MAX, nkn_local)

    R_ABIOTIC_NITR(ns:ne) = K_NITR_20 * LIM_NITR_OXY(ns:ne) * LIM_NITR_NH4_N(ns:ne) * &
                     PH_CORR_NITR_NH4(ns:ne) * (THETA_K_NITR ** (TEMP(ns:ne) - 2.0D1)) * NH4_N(ns:ne)

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

    R_DENITRIFICATION(ns:ne) = 0.93D0 * R_ABIOTIC_DOC_MIN_NO3N(ns:ne)
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

    R_MN_IV_REDUCTION(ns:ne) = 8.66D0 * R_ABIOTIC_DOC_MIN_MN_IV(ns:ne)
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

    R_FE_III_REDUCTION(ns:ne) = 18.66D0 * R_ABIOTIC_DOC_MIN_FE_III(ns:ne)
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

    R_SULPHATE_REDUCTION(ns:ne) = 1.33D0 * R_ABIOTIC_DOC_MIN_S_PLUS_6(ns:ne)
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

    R_METHANOGENESIS(ns:ne) = 0.5D0 * R_ABIOTIC_DOC_MIN_DOC(ns:ne)
    ! -------------------------------------------------------------------------
    ! END OF METHANOGENESIS
    ! -------------------------------------------------------------------------

    !*********************************************************************!
    !     VOLATILIZATION OF UNIONIZED AMMONI.
    !*********************************************************************!
    call AMMONIA_VOLATILIZATION(R_AMMONIA_VOLATIL(ns:ne), NH4_N(ns:ne), pH(ns:ne), TEMP(ns:ne), K_A_CALC(ns:ne), nkn_local)

    !----------------------------------------------------------------------
    ! 2 February 2015
    ! New code added to account the effect of ice cover.
    !----------------------------------------------------------------------
    R_AMMONIA_VOLATIL(ns:ne) = R_AMMONIA_VOLATIL(ns:ne) * (1.0D0 - ice_cover(ns:ne))

    where (R_AMMONIA_VOLATIL(ns:ne) < 0.0D0)
        R_AMMONIA_VOLATIL(ns:ne) = 0.0D0
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
            where (DISS_OXYGEN(ns:ne) < 1)
                R_FE_II_OXIDATION(ns:ne)  = k_OX_FE_II * DISS_OXYGEN(ns:ne) * (10.0D0 ** (PH(ns:ne) - 7.0D0)) * FE_II(ns:ne)
            elsewhere
                R_FE_II_OXIDATION(ns:ne)  = k_OX_FE_II * (10.0D0 ** (PH(ns:ne) - 7.0D0)) * FE_II(ns:ne)
            end where
        end if

        if(iron_oxidation .eq. 1) then
            ! In the future include several options for heavy metal oxidation and possibly reduction
            ! Morgen and Lahav (2007) formulation. No calibration
            call IRON_II_OXIDATION(FE_II_DISS(ns:ne), DISS_OXYGEN(ns:ne), &
                 PH(ns:ne), TEMP(ns:ne), SALT(ns:ne), ELEVATION(ns:ne), &
                 nkn_local, R_FE_II_OXIDATION(ns:ne))
            ! ---------------------------------------------------------------------
            ! End of changes by Ali Ert�rk, 6 th of July 2016
            ! ---------------------------------------------------------------------
        end if
        ! 29 January 2016
        ! Following commented lines are replaced by the new redox sequence based DOC
        ! DOC mineralization it is the next visit of Ali and the redox sequences as described
        ! by Van Chappen and Wang 2015 and Katsev papers are now included.

        ! Manganese
        where (DISS_OXYGEN(ns:ne) < 1)
            R_MN_II_OXIDATION(ns:ne)  = k_OX_MN_II * DISS_OXYGEN(ns:ne) * (10.0D0 ** (PH(ns:ne) - 7.0D0))* MN_II(ns:ne)
        elsewhere
            R_MN_II_OXIDATION(ns:ne)  = k_OX_MN_II * (10.0D0 ** (PH(ns:ne) - 7.0D0)) * MN_II(ns:ne)
        end where

        ! 29 January 2016
        ! Following commented lines are replaced by the new redox sequence based DOC
        ! mineralization it is the next visit of Ali and the redox sequences as described
        ! by Van Chappen and Wang 2015 and Katsev papers are now included.

        ! End of new kinetic rate calculations added 9 September 2015

        ! -------------------------------------------------------------------------------
        ! 29 January 2016 KINETICS OF NEW STATE VARIABLES
        ! -------------------------------------------------------------------------------
        K_A_CH4(ns:ne) = K_A_CALC(ns:ne) * 1.188D0
        K_A_H2S(ns:ne) = K_A_CALC(ns:ne) * 0.984D0
        CH4_SAT(ns:ne) = 0.0D0 ! Assume that no methane is present in the atmosphere
        H2S_SAT(ns:ne) = 0.0D0 ! Assume that no H2S(ns:ne) is present in the atmosphere

        CH4_ATM_EXCHANGE(ns:ne) = K_A_CH4(ns:ne) * (CH4_SAT(ns:ne) - CH4_C(ns:ne))
        H2S_ATM_EXCHANGE(ns:ne) = K_A_H2S(ns:ne) * (H2S_SAT(ns:ne) - (H2S(ns:ne) * 32000.D0))

        R_METHANE_OXIDATION(ns:ne) = &
            k_OX_CH4 * (THETA_k_OX_CH4 ** (TEMP(ns:ne) - 20.0D0)) * CH4_C(ns:ne) * &
            (DISS_OXYGEN(ns:ne) / (k_HS_OX_CH4_DOXY + DISS_OXYGEN(ns:ne)))

        R_SULPHIDE_OXIDATION(ns:ne) = &
            k_OX_H2S * (THETA_k_OX_H2S ** (TEMP(ns:ne) - 20.0D0)) * S_MINUS_2(ns:ne) * &
            (DISS_OXYGEN(ns:ne) / (k_HS_OX_H2S_DOXY + DISS_OXYGEN(ns:ne)))
    else
        R_FE_II_OXIDATION(ns:ne)    = 0.0D0
        R_MN_II_OXIDATION(ns:ne)    = 0.0D0
        R_SULPHIDE_OXIDATION(ns:ne) = 0.0D0
        R_METHANE_OXIDATION(ns:ne)  = 0.0D0
    end if
    ! -------------------------------------------------------------------------
    ! END OF KINETICS OF NEW STATE VARIABLES
    ! -------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------------
    ! Final calculation of derivatives
    !------------------------------------------------------------------------------------------------

    !AMMONIA NITROGEN
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 1) = R_DIA_TOT_RESP(ns:ne) * DIA_N_TO_C
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 2) = R_CYN_TOT_RESP(ns:ne) * CYN_N_TO_C
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 3) = R_OPA_TOT_RESP(ns:ne) * OPA_N_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 4) = R_FIX_CYN_TOT_RESP(ns:ne) * FIX_CYN_N_TO_C
    else
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,NH4_N_INDEX, 5) = R_ZOO_TOT_RESP(ns:ne) * ACTUAL_ZOO_N_TO_C(ns:ne)
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 6) = R_DIA_GROWTH(ns:ne)   * PREF_NH4N_DIA(ns:ne) * DIA_N_TO_C

    PROCESS_RATES(ns:ne,NH4_N_INDEX, 7) = R_CYN_GROWTH(ns:ne) * CYN_N_TO_C * PREF_DIN_DON_CYN(ns:ne) * PREF_NH4N_CYN(ns:ne)

    PROCESS_RATES(ns:ne,NH4_N_INDEX, 8) = R_OPA_GROWTH(ns:ne) * PREF_NH4N_OPA(ns:ne) * OPA_N_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 9) = &
            R_NON_FIX_CYN_GROWTH(ns:ne) * PREF_NH4N_DON_FIX_CYN(ns:ne) * FIX_CYN_N_TO_C * &
            (NH4_N(ns:ne) / max(NH4_N(ns:ne) + (DISS_ORG_N(ns:ne) * frac_avail_DON), 1.0D-10))
    else
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 9)  = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 23) = &
            R_NOST_VEG_HET_NON_FIX_GROWTH(ns:ne) * NOST_N_TO_C * PREF_DIN_DON_NOST(ns:ne) * PREF_NH4N_NOST(ns:ne)

        !NH4 production during N-fixation
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 22) = &
            R_NOST_VEG_HET_FIX_GROWTH(ns:ne) * NOST_N_TO_C * &
            ((1.D0 - FRAC_FIX_N_FOR_GR_VEG_HET)/FRAC_FIX_N_FOR_GR_VEG_HET)
    else
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 22) = 0.0D0
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 23) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,NH4_N_INDEX, 10) = R_ABIOTIC_NITR(ns:ne)

    PROCESS_RATES(ns:ne,NH4_N_INDEX, 11) = &
        R_ABIOTIC_DON_MIN_DOXY(ns:ne)   + R_ABIOTIC_DON_MIN_NO3N(ns:ne)     + R_ABIOTIC_DON_MIN_MN_IV(ns:ne) + &
        R_ABIOTIC_DON_MIN_FE_III(ns:ne) + R_ABIOTIC_DON_MIN_S_PLUS_6(ns:ne) + R_ABIOTIC_DON_MIN_DOC(ns:ne)

    PROCESS_RATES(ns:ne,NH4_N_INDEX, 12) = R_AMMONIA_VOLATIL(ns:ne)

    ! Auxiliary
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 13) = PREF_NH4N_DIA(ns:ne)
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 14) = 0.0D0  ! Old PREF_NH4N_DON_CYN deprecated
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 15) = PREF_NH4N_OPA(ns:ne)
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 16) = PREF_NH4N_DON_FIX_CYN(ns:ne)
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 18) = PREF_NH4N_CYN(ns:ne)
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 19) = PREF_DIN_DON_CYN(ns:ne)
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 20) = PREF_NH4N_NOST(ns:ne)
    PROCESS_RATES(ns:ne,NH4_N_INDEX, 21) = PREF_DIN_DON_NOST(ns:ne)

    ! New process information incorporated 10 September 2019
    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 17) = R_NOST_VEG_HET_TOT_RESP(ns:ne) * NOST_N_TO_C
    else
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 17) = 0.0D0
    end if
    ! End of new process information incorporated 10 September 2019

    DERIVATIVES(ns:ne,NH4_N_INDEX) = &
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 1)  + PROCESS_RATES(ns:ne,NH4_N_INDEX, 2)  + &
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 3)  + PROCESS_RATES(ns:ne,NH4_N_INDEX, 4)  + &
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 5)  - PROCESS_RATES(ns:ne,NH4_N_INDEX, 6)  - &
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 7)  - PROCESS_RATES(ns:ne,NH4_N_INDEX, 8)  - &
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 9)  - PROCESS_RATES(ns:ne,NH4_N_INDEX, 10) + &
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 11) - PROCESS_RATES(ns:ne,NH4_N_INDEX, 12) + &
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 17) + PROCESS_RATES(ns:ne,NH4_N_INDEX, 22) - &
        PROCESS_RATES(ns:ne,NH4_N_INDEX, 23)

    ! Code to debug NH4N
    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_NH4N_01&
             (PROCESS_RATES, DERIVATIVES, TIME, nkn, nstate, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier

    !NITRATE NITROGEN
    PROCESS_RATES(ns:ne,NO3_N_INDEX, 1) = R_ABIOTIC_NITR(ns:ne)
    PROCESS_RATES(ns:ne,NO3_N_INDEX, 2) = R_DENITRIFICATION(ns:ne)

    PROCESS_RATES(ns:ne,NO3_N_INDEX, 3) = &
        R_DIA_GROWTH(ns:ne) * (1.0D0 - PREF_NH4N_DIA(ns:ne)) * DIA_N_TO_C

    PROCESS_RATES(ns:ne,NO3_N_INDEX, 4) = &
        R_CYN_GROWTH(ns:ne) * CYN_N_TO_C * PREF_DIN_DON_CYN(ns:ne) * (1.0D0 - PREF_NH4N_CYN(ns:ne))

    PROCESS_RATES(ns:ne,NO3_N_INDEX, 5) = R_OPA_GROWTH(ns:ne)         * (1.0D0 - PREF_NH4N_OPA(ns:ne))         * OPA_N_TO_C

    if (DO_NOSTOCALES > 0) then
      PROCESS_RATES(ns:ne,NO3_N_INDEX, 6) = &
          R_NOST_VEG_HET_NON_FIX_GROWTH(ns:ne) * NOST_N_TO_C * PREF_DIN_DON_NOST(ns:ne) * (1-PREF_NH4N_NOST(ns:ne))
    else
      PROCESS_RATES(ns:ne,NO3_N_INDEX, 6) = &
          R_NON_FIX_CYN_GROWTH(ns:ne) * (1.0D0 - PREF_NH4N_DON_FIX_CYN(ns:ne)) * FIX_CYN_N_TO_C
    end if


    ! Auxiliary
    PROCESS_RATES(ns:ne,NO3_N_INDEX, 7) = PREF_NH4N_DIA(ns:ne)
    PROCESS_RATES(ns:ne,NO3_N_INDEX, 8) = 0.0D0  ! Old PREF_NH4N_DON_CYN deprecated
    PROCESS_RATES(ns:ne,NO3_N_INDEX, 9) = PREF_NH4N_OPA(ns:ne)

    DERIVATIVES(ns:ne,NO3_N_INDEX) = &
        PROCESS_RATES(ns:ne,NO3_N_INDEX, 1) - PROCESS_RATES(ns:ne,NO3_N_INDEX, 2) - &
        PROCESS_RATES(ns:ne,NO3_N_INDEX, 3) - PROCESS_RATES(ns:ne,NO3_N_INDEX, 4) - &
        PROCESS_RATES(ns:ne,NO3_N_INDEX, 5) - PROCESS_RATES(ns:ne,NO3_N_INDEX, 6)

    !PHOSPHATE PHOSPHORUS
    PROCESS_RATES(ns:ne,PO4_P_INDEX, 1) = R_DIA_TOT_RESP(ns:ne) * DIA_P_TO_C
    PROCESS_RATES(ns:ne,PO4_P_INDEX, 2) = R_CYN_TOT_RESP(ns:ne) * CYN_P_TO_C
    PROCESS_RATES(ns:ne,PO4_P_INDEX, 3) = R_OPA_TOT_RESP(ns:ne) * OPA_P_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 4) = R_FIX_CYN_TOT_RESP(ns:ne) * FIX_CYN_P_TO_C
    else
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,PO4_P_INDEX, 5) = R_ZOO_TOT_RESP(ns:ne) * ACTUAL_ZOO_P_TO_C(ns:ne)
    PROCESS_RATES(ns:ne,PO4_P_INDEX, 6) = R_DIA_GROWTH(ns:ne)   * DIA_P_TO_C
    PROCESS_RATES(ns:ne,PO4_P_INDEX, 7) = R_CYN_GROWTH(ns:ne)   * CYN_P_TO_C
    PROCESS_RATES(ns:ne,PO4_P_INDEX, 8) = R_OPA_GROWTH(ns:ne)   * OPA_P_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 9) = R_FIX_CYN_GROWTH(ns:ne) * FIX_CYN_P_TO_C
    else
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 9) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,PO4_P_INDEX, 10) = &
        R_ABIOTIC_DOP_MIN_DOXY(ns:ne)   + R_ABIOTIC_DOP_MIN_NO3N(ns:ne)     + R_ABIOTIC_DOP_MIN_MN_IV(ns:ne) + &
        R_ABIOTIC_DOP_MIN_FE_III(ns:ne) + R_ABIOTIC_DOP_MIN_S_PLUS_6(ns:ne) + R_ABIOTIC_DOP_MIN_DOC(ns:ne)

    ! Auxiliary
    PROCESS_RATES(ns:ne,PO4_P_INDEX, 11) = TEMP(ns:ne)
    PROCESS_RATES(ns:ne,PO4_P_INDEX, 12) = DISS_ORG_P(ns:ne)

    ! New process information incorporated 10 September 2019
    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 13) = R_NOST_VEG_HET_TOT_RESP(ns:ne) * NOST_P_TO_C

        PROCESS_RATES(ns:ne,PO4_P_INDEX, 14) = &
            R_NOST_VEG_HET_GROWTH(ns:ne)   * NOST_P_TO_C * PREF_DIP_DOP_NOST(ns:ne)
    else
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 13) = 0.0D0
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 14) = 0.0D0
    end if
    ! End of new process information incorporated 10 September 2019

    DERIVATIVES(ns:ne,PO4_P_INDEX) = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 1 ) + PROCESS_RATES(ns:ne,PO4_P_INDEX, 2 ) + &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 3 ) + PROCESS_RATES(ns:ne,PO4_P_INDEX, 4 ) + &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 5 ) - PROCESS_RATES(ns:ne,PO4_P_INDEX, 6 ) - &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 7 ) - PROCESS_RATES(ns:ne,PO4_P_INDEX, 8 ) - &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 9 ) + PROCESS_RATES(ns:ne,PO4_P_INDEX, 10) + &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 13) - PROCESS_RATES(ns:ne,PO4_P_INDEX, 14)

    ! Debug for PO4P
    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_PO4P_01&
             (PROCESS_RATES, DERIVATIVES, TIME, nkn, nstate, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier

    !DISSOLVED SILICA SILICON
    PROCESS_RATES(ns:ne,DISS_Si_INDEX, 1) = R_PART_Si_DISS(ns:ne)

    !Should it come from respiration? Silica is in shell.

    !Physiologically not but respiration is a process which decreases the diatom biomass.
    !So in order to keep the model mass conserving, respiration should set stochimetrically
    !correct amount of silicon in dissolved form free. Because of the same reason, excrtion
    !t
    PROCESS_RATES(ns:ne,DISS_Si_INDEX, 2) = R_DIA_TOT_RESP(ns:ne) * DIA_SI_TO_C


    PROCESS_RATES(ns:ne,DISS_Si_INDEX, 3) = R_DIA_EXCR(ns:ne) * DIA_SI_TO_C

    PROCESS_RATES(ns:ne,DISS_Si_INDEX, 4) = R_DIA_GROWTH(ns:ne)   * DIA_SI_TO_C

    DERIVATIVES(ns:ne,DISS_Si_INDEX) = &
        PROCESS_RATES(ns:ne,DISS_Si_INDEX, 1) + PROCESS_RATES(ns:ne,DISS_Si_INDEX, 2) + &
        PROCESS_RATES(ns:ne,DISS_Si_INDEX, 3) - PROCESS_RATES(ns:ne,DISS_Si_INDEX, 4)

    !DISSOLVED OXYGEN
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 1)  = R_AERATION(ns:ne)

    ! formulation from EFDC
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 2)  = &
        R_DIA_GROWTH(ns:ne) * (1.3D0 - 0.3D0*PREF_NH4N_DIA(ns:ne))    * DIA_O2_TO_C

    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 3)  = &
        R_CYN_GROWTH(ns:ne) * (1.3D0 - 0.3D0 * PREF_NH4N_CYN(ns:ne) * PREF_DIN_DON_CYN(ns:ne)) * CYN_O2_TO_C

    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 4) = &
        R_OPA_GROWTH(ns:ne)  * (1.3D0 - 0.3D0*PREF_NH4N_OPA(ns:ne)) * OPA_O2_TO_C

    if(DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 5)= &
            R_NOST_VEG_HET_GROWTH(ns:ne) * &
            NOST_O2_TO_C * (1.3D0 - 0.3D0*PREF_NH4N_NOST(ns:ne)*PREF_DIN_DON_NOST(ns:ne))
    else
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 5)  = &
            R_FIX_CYN_GROWTH(ns:ne)   * (1.3D0 - 0.3D0*PREF_NH4N_DON_FIX_CYN(ns:ne))* FIX_CYN_O2_TO_C
    endif

    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 6)  = R_DIA_TOT_RESP(ns:ne) * DIA_O2_TO_C
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 7)  = R_CYN_TOT_RESP(ns:ne) * CYN_O2_TO_C
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 8)  = R_OPA_TOT_RESP(ns:ne) * OPA_O2_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 9) = R_FIX_CYN_TOT_RESP(ns:ne) * FIX_CYN_O2_TO_C
    else
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 9) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 10) = R_ZOO_TOT_RESP(ns:ne) * ZOO_O2_TO_C
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 11) = R_ABIOTIC_NITR(ns:ne) * 4.57D0
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 12) = 2.66D0 * R_ABIOTIC_DOC_MIN_DOXY(ns:ne)
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 13) = 0.43D0 * R_FE_II_OXIDATION(ns:ne)
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 14) = 0.88D0 * R_MN_II_OXIDATION(ns:ne)
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 15) = 2.00D0 * R_SULPHIDE_OXIDATION(ns:ne)
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 16) = 5.33D0 * R_METHANE_OXIDATION(ns:ne)

    ! Nostocales O2 production is already accounted in index 5 (EFDC-corrected).
    ! Index 19 is reserved for diagnostic/auxiliary use only — do NOT add to derivative.
    ! Nostocales respiration O2 is accounted via index 20 (only sink, not in index 8/9).
    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 19) = 0.0D0
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 20) = R_NOST_VEG_HET_TOT_RESP(ns:ne) * NOST_O2_TO_C
    else
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 19) = 0.0D0
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 20) = 0.0D0
    end if

    ! Auxiliary
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 17) = K_A_CALC(ns:ne)
    PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 18) = DISS_OXYGEN_SAT(ns:ne)

    DERIVATIVES(ns:ne,DISS_OXYGEN_INDEX) = &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 1)  + &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 2)  + &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 3)  + &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 4)  + &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 5)  - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 6)  - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 7)  - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 8)  - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 9)  - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 10) - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 11) - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 12) - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 13) - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 14) - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 15) - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 16) + &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 19) - &
        PROCESS_RATES(ns:ne,DISS_OXYGEN_INDEX, 20)

    !Debug code for dissolved oxygen
    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_DOXY_01&
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier

    !DIATOMS CARBON
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 1)  = R_DIA_GROWTH(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 2)  = R_DIA_TOT_RESP(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 3)  = R_DIA_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 4)  = R_DIA_DEATH(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 5)  = R_ZOO_FEEDING_DIA(ns:ne)
    ! Auxiliary
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 6)  = LIM_KG_DIA_TEMP(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 7)  = LIM_KG_DIA_DOXY(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 8)  = LIM_KG_DIA_N(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 9)  = LIM_KG_DIA_P(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 10) = LIM_KG_DIA_DISS_Si(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 11) = LIM_KG_DIA_LIGHT(ns:ne)
    PROCESS_RATES(ns:ne,DIA_C_INDEX, 12) = DIA_LIGHT_SAT(ns:ne)

    DERIVATIVES(ns:ne,DIA_C_INDEX) = &
        PROCESS_RATES(ns:ne,DIA_C_INDEX, 1) - PROCESS_RATES(ns:ne,DIA_C_INDEX, 2) - &
        PROCESS_RATES(ns:ne,DIA_C_INDEX, 3) - PROCESS_RATES(ns:ne,DIA_C_INDEX, 4) - &
        PROCESS_RATES(ns:ne,DIA_C_INDEX, 5)

    !NON-NITROGEN FIXING CYANOBACTERIA CARBON
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 1)  = R_CYN_GROWTH(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 2)  = R_CYN_TOT_RESP(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 3)  = R_CYN_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 4)  = R_CYN_DEATH(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 5)  = R_ZOO_FEEDING_CYN(ns:ne)
    ! Auxiliary
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 6)  = LIM_KG_CYN_TEMP(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 7)  = LIM_KG_CYN_DOXY(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 8)  = LIM_KG_CYN_N(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 9)  = LIM_KG_CYN_P(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 10) = LIM_KG_CYN_LIGHT(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 11) = I_A(ns:ne)             !light langlays
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 12) = CYN_LIGHT_SAT(ns:ne)
    PROCESS_RATES(ns:ne,CYN_C_INDEX, 13) = TEMP(ns:ne)

    DERIVATIVES(ns:ne,CYN_C_INDEX) = &
        PROCESS_RATES(ns:ne,CYN_C_INDEX, 1) - PROCESS_RATES(ns:ne,CYN_C_INDEX, 2) - &
        PROCESS_RATES(ns:ne,CYN_C_INDEX, 3) - PROCESS_RATES(ns:ne,CYN_C_INDEX, 4) - &
                PROCESS_RATES(ns:ne,CYN_C_INDEX, 5)

    ! Debug: print derivatives for CYN_C node1 (commented to reduce log noise)
    ! write(6,'(A,ES14.6)') 'DEBUG: DERIV CYN node1=', DERIVATIVES(1,CYN_C_INDEX)

    ! Debug code for CYN_C
    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_CYNC_01 &
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier

    !NITROGEN FIXING CYANOBACTERIA CARBON
    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 1)  = R_FIX_CYN_GROWTH(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 2)  = R_FIX_CYN_TOT_RESP(ns:ne)
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 3)  = R_FIX_CYN_EXCR(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 4)  = R_FIX_CYN_DEATH(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 5)  = R_ZOO_FEEDING_FIX_CYN(ns:ne)

        ! Auxiliary
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 6)  = R_NON_FIX_CYN_GROWTH(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 7)  = R_FIX_FIX_CYN_GROWTH(ns:ne)

        !Nitrogen fixation
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 8)  = &
            (R_FIX_FIX_CYN_GROWTH(ns:ne)  * FIX_CYN_N_TO_C)

        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 9)  = LIM_KG_FIX_CYN_TEMP(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 10) = LIM_KG_FIX_CYN_DOXY(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 11) = LIM_KG_FIX_FIX_CYN_N(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 12) = LIM_KG_FIX_FIX_CYN_P(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 13) = LIM_KG_NON_FIX_CYN_N(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 14) = LIM_KG_NON_FIX_CYN_P(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 15) = LIM_KG_FIX_CYN_LIGHT(ns:ne)

        ! NP molar ratio calculation with guard against division by zero
        ! When phosphorus is depleted, set ratio to very high value indicating P limitation
        where (DIP_OVER_IP(ns:ne) * PO4_P(ns:ne) .lt. 1.0D-10)
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 16) = 999.0D0  ! Extreme N:P indicates P limitation
        elsewhere
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 16) = &
                ((NH4_N(ns:ne) + NO3_N(ns:ne))/14.D0)/(DIP_OVER_IP(ns:ne)*PO4_P(ns:ne)/31.D0)
        end where

        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 17) = NH4_N(ns:ne) + NO3_N(ns:ne)
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 18) = FIX_CYN_LIGHT_SAT(ns:ne)

        DERIVATIVES(ns:ne,FIX_CYN_C_INDEX) = &
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 1) - PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 2) - &
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 3) - PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 4) - &
                PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 5)

        ! Diagnostic check: if applying derivative over TIME_STEP would make concentration negative, dump context and stop
        do i = ns, ne
            if (FIX_CYN_C(i) + DERIVATIVES(i,FIX_CYN_C_INDEX) * TIME_STEP < 0.0D0) then
                !$omp critical
                write(6,'(A,F12.4,A,ES12.4,A,I4)') &
                    'ERROR: PREDICTED NEGATIVE FIX_CYN_C: TIME=', TIME, ' DT=', TIME_STEP, ' BOX=', i
                write(6,'(A,ES14.6,A,ES14.6)') &
                    '  FIX_CYN_C=', FIX_CYN_C(i), '  DERIV=', DERIVATIVES(i,FIX_CYN_C_INDEX)
                write(6,'(A,6ES12.4)') '  PROCESS_RATES(1:6)=', PROCESS_RATES(i,FIX_CYN_C_INDEX,1:6)
                write(6,'(A,3ES14.6)') '  NH4_N/NO3_N/PO4_P=', NH4_N(i), NO3_N(i), PO4_P(i)
                !$omp end critical
                stop
            end if

            ! New diagnostic: check actual process rates (indices 1-5) for unusually large values
            ! Note: Indices 6-18 are diagnostic values (limiters, ratios) not actual rates
            if (FIX_CYN_C(i) > 1.0D-12) then
                do k = 1, 5  ! Only check actual process rates, not diagnostic values
                    if (dabs(PROCESS_RATES(i,FIX_CYN_C_INDEX,k)) * TIME_STEP > 0.2D0 * FIX_CYN_C(i)) then
                        !$omp critical
                        write(6,'(A,F12.4,A,ES12.4,A,I4)') &
                            'DEBUG: LARGE PROCESS ON FIX_CYN: TIME=', TIME, ' DT=', TIME_STEP, ' BOX=', i
                        write(6,'(A,ES14.6,A,I3,A,ES14.6)') &
                            '  FIX_CYN_C=', FIX_CYN_C(i), '  PROC_IDX=', k, '  RATE=', PROCESS_RATES(i,FIX_CYN_C_INDEX,k)
                        write(6,'(A,ES14.6,A,ES14.6)') &
                            '  DERIV_FIX=', DERIVATIVES(i,FIX_CYN_C_INDEX), '  DERIV_CYN=', DERIVATIVES(i,CYN_C_INDEX)
                        write(6,'(A,3ES14.6)') &
                            '  NH4_N/NO3_N/PO4_P=', NH4_N(i), NO3_N(i), PO4_P(i)
                        !$omp end critical

                        ! Safety clamp for single large process (only for actual rates 2-5)
                        if (k >= 2) then
                            allowed_rate_local = FIX_CYN_C(i) / max(TIME_STEP, 1.0D-12)
                            if (dabs(PROCESS_RATES(i,FIX_CYN_C_INDEX,k)) > allowed_rate_local) then
                                !$omp critical
                                write(6,'(A)') '  Applying removal-limiter scaling...'
                                !$omp end critical
                                old_rate = PROCESS_RATES(i,FIX_CYN_C_INDEX,k)
                                PROCESS_RATES(i,FIX_CYN_C_INDEX,k) = sign(allowed_rate_local, PROCESS_RATES(i,FIX_CYN_C_INDEX,k))

                                ! Recompute derivative for this node (sum only actual removals 2-5)
                                sum_removals = PROCESS_RATES(i,FIX_CYN_C_INDEX,2) + &
                                               PROCESS_RATES(i,FIX_CYN_C_INDEX,3) + &
                                               PROCESS_RATES(i,FIX_CYN_C_INDEX,4) + &
                                               PROCESS_RATES(i,FIX_CYN_C_INDEX,5)
                                DERIVATIVES(i,FIX_CYN_C_INDEX) = PROCESS_RATES(i,FIX_CYN_C_INDEX,1) - sum_removals

                                ! Increment compact per-node and per-process clamp counters (summary printed once later)
                                !$omp critical
                                if (.not. allocated(CLAMP_COUNT)) then
                                    allocate(CLAMP_COUNT(nkn))
                                    CLAMP_COUNT = 0
                                end if
                                !$omp end critical
                                CLAMP_COUNT(i) = CLAMP_COUNT(i) + 1
                                if (NDIAGVAR > 0) then
                                    !$omp critical
                                    if (.not. allocated(CLAMP_PROC_COUNT)) then
                                        allocate(CLAMP_PROC_COUNT(nkn, NDIAGVAR))
                                        CLAMP_PROC_COUNT = 0
                                    end if
                                    !$omp end critical
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
                    !$omp critical
                    write(6,'(A,F12.4,A,I4)') 'DEBUG: REMOVAL LIMITER CHECK: TIME=', TIME, ' BOX=', i
                    write(6,'(A,ES12.4,A,ES12.4,A,ES12.4)') &
                        '  TOT_REM=', total_removal, ' ALLOW=', allowed_rate, ' FIX_CYN_C=', FIX_CYN_C(i)
                    write(6,'(A,5ES11.3)') '  RESP/INT/EXCR/DEATH/GRAZ=', &
                        R_FIX_CYN_RESP(i), R_FIX_CYN_INT_RESP(i), R_FIX_CYN_EXCR(i), &
                        R_FIX_CYN_DEATH(i), R_ZOO_FEEDING_FIX_CYN(i)
                    !$omp end critical
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

                    !$omp critical
                    write(6,'(A,F12.4,A,I4,A,ES10.3)') &
                        'WARN: Scaled FIX_CYN removal: TIME=', TIME, ' BOX=', i, ' SCALE=', scale
                    write(6,'(A,ES12.4,A,ES12.4)') &
                        '  TOT_REM_BEFORE=', total_removal, ' FIX_CYN_C=', FIX_CYN_C(i)
                    !$omp end critical
                end if
            end if
        end do
    else
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 1)  = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 2)  = 0.0D0
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 3)  = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 4)  = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 5)  = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 6)  = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 7)  = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 8)  = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 9)  = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 10) = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 11) = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 12) = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 13) = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 14) = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 15) = 0.0D0
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 16) = 0.0D0
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 17) = 0.0D0
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 18) = 0.0D0

        DERIVATIVES  (ns:ne,FIX_CYN_C_INDEX)     = 0.0D0
    end if

    ! Process rates for the new state variables incorporated 10 September 2019
    if (DO_NOSTOCALES > 0) then

        ! -------------------------------------------------------------------------------
        ! Nostaocacles in vegetative + heterocyst staged form
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 1) = R_NOST_VEG_HET_GROWTH(ns:ne)
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 2) = R_NOST_VEG_HET_TOT_RESP(ns:ne)
            PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 3) = R_NOST_VEG_HET_EXCR(ns:ne)
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 4) = R_NOST_VEG_HET_DEATH(ns:ne)
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 5) = R_ZOO_FEEDING_NOST_VEG_HET(ns:ne)

        ! -------------------------------------------------------------------------------
        ! Akinetes before germination are located in the mud but internally still in
        ! units gC/m^3 though initial condition is given in gC/m^2 and coverted to gC/m^3
        ! In case of 3d they should rise with negative settling velocity
        ! to the WC nearbottom layer and next layers up, fixme
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 6) = R_GERM_NOST_AKI(ns:ne)                                                                            !
        ! -------------------------------------------------------------------------------

        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 7) = R_FORM_NOST_AKI(ns:ne)
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 8) = R_DENS_MORT_NOST_VEG_HET(ns:ne)


        ! Nitrogen fixation by nostacales only
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 9) = &
            R_NOST_VEG_HET_FIX_GROWTH(ns:ne) * NOST_N_TO_C * (1.D0/FRAC_FIX_N_FOR_GR_VEG_HET)

        ! Update the total nitrogen fixation
        PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 9) = &
            PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX, 9) + &
            PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 9)

        ! Auxilaries
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 10) = NOST_LIGHT_SAT(ns:ne)

        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX,11) = LIM_KG_NOST_VEG_HET_LIGHT(ns:ne)
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX,12) = LIM_KG_NOST_VEG_HET_TEMP(ns:ne)
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX,13) = LIM_KG_NOST_VEG_HET_DOXY(ns:ne)
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX,14) = LIM_KG_NOST_VEG_HET_P(ns:ne)

        ! What is this????
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX,15) = &
            R_NOST_VEG_HET_GROWTH(ns:ne) + R_CYN_GROWTH(ns:ne) + R_OPA_GROWTH(ns:ne) + R_DIA_GROWTH(ns:ne)

        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX,16) = LIM_KG_NOST_VEG_HET_N(ns:ne)


        DERIVATIVES(ns:ne,NOST_VEG_HET_C_INDEX) = &
             PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 1)  &
           - PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 2)  &
           - PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 3)  &
           - PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 4)  &
           - PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 5)  &
           + PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 6)  &
           - PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 7)  &
           - PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 8)
        ! -------------------------------------------------------------------------------

        ! -------------------------------------------------------------------------------
        ! Nostocales akinets
        ! -------------------------------------------------------------------------------

        ! Assummed formed akinetes immediatelly go to the bottom but still
        ! in units gC/m^3 bu goes to the output as gC/m^2
        ! They should settle trough the layers to the bottom, fixme
        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 1) = R_FORM_NOST_AKI(ns:ne)

        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 2) = R_GERM_NOST_AKI(ns:ne)
        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 3) = R_LOSS_AKI(ns:ne)

        ! -------------------------------------------------------------------------------
        ! Keep it zero. Mortality of akinetes is negligible
        ! Before germination they sit in mud, not in WC
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 4) = R_MORT_AKI(ns:ne)
        ! -------------------------------------------------------------------------------

        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 5) = DEPTH(ns:ne)

        DERIVATIVES(ns:ne,NOST_AKI_C_INDEX) = &
            PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 1) - &
            PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 2) - &
            PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 3) - &
            PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 4)
        ! -------------------------------------------------------------------------------
    else
        ! -------------------------------------------------------------------------------
        ! Nostaocacles in vegetative + heterocyst staged form
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 1) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 2) = 0.0D0
            PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 3) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 4) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 5) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 6) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 7) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 8) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 9) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 10) = 0.0D0

        DERIVATIVES  (ns:ne,NOST_VEG_HET_C_INDEX)     = 0.0D0

        ! -------------------------------------------------------------------------------
        ! Nostaocacles akinets
        ! -------------------------------------------------------------------------------
        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 1) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 2) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 3) = 0.0D0
        PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 4) = 0.0D0

        DERIVATIVES  (ns:ne,NOST_AKI_C_INDEX)    = 0.0D0
        ! -------------------------------------------------------------------------------
    end if
    ! End of process rates for the new state variables incorporated 10 September 2019

    !OTHER PLANKTONIC ALGAE CARBON
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 1) = R_OPA_GROWTH(ns:ne)
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 2) = R_OPA_TOT_RESP(ns:ne)
        PROCESS_RATES(ns:ne,OPA_C_INDEX, 3) = R_OPA_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 4) = R_OPA_DEATH(ns:ne)
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 5) = R_ZOO_FEEDING_OPA(ns:ne)

    ! Auxiliary
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 6 ) = LIM_KG_OPA_TEMP(ns:ne)
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 7 ) = LIM_KG_OPA_DOXY(ns:ne)
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 8 ) = LIM_KG_OPA_N(ns:ne)
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 9 ) = LIM_KG_OPA_P(ns:ne)
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 10) = LIM_KG_OPA_LIGHT(ns:ne)
    PROCESS_RATES(ns:ne,OPA_C_INDEX, 11) = OPA_LIGHT_SAT(ns:ne)

    DERIVATIVES(ns:ne,OPA_C_INDEX) = &
        PROCESS_RATES(ns:ne,OPA_C_INDEX, 1) - PROCESS_RATES(ns:ne,OPA_C_INDEX, 2) - &
        PROCESS_RATES(ns:ne,OPA_C_INDEX, 3) - PROCESS_RATES(ns:ne,OPA_C_INDEX, 4) - &
                PROCESS_RATES(ns:ne,OPA_C_INDEX, 5)

    !ZOOPLANKTON CARBON
    PROCESS_RATES(ns:ne,ZOO_C_INDEX, 1) = R_ZOO_GROWTH(ns:ne)

    PROCESS_RATES(ns:ne,ZOO_C_INDEX, 2) = 0.0

    if (ZOOP_OPTION_1 > 0) then
        PROCESS_RATES(ns:ne,ZOO_C_INDEX, 2) = R_ZOO_EX_DOC(ns:ne)
    end if

    PROCESS_RATES(ns:ne,ZOO_C_INDEX, 3) = R_ZOO_TOT_RESP(ns:ne)
    PROCESS_RATES(ns:ne,ZOO_C_INDEX, 4) = R_ZOO_DEATH(ns:ne)

    PROCESS_RATES(ns:ne,ZOO_C_INDEX, 5) = R_ZOO_FEEDING_DIA(ns:ne)
    PROCESS_RATES(ns:ne,ZOO_C_INDEX, 6) = R_ZOO_FEEDING_CYN(ns:ne)
    PROCESS_RATES(ns:ne,ZOO_C_INDEX, 7) = R_ZOO_FEEDING_OPA(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,ZOO_C_INDEX, 8) = R_ZOO_FEEDING_FIX_CYN(ns:ne)
    else
        PROCESS_RATES(ns:ne,ZOO_C_INDEX, 8) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,ZOO_C_INDEX, 9) = R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne)

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,ZOO_C_INDEX, 10) = R_ZOO_FEEDING_NOST_VEG_HET(ns:ne)
    else
        PROCESS_RATES(ns:ne,ZOO_C_INDEX, 10) = 0.0D0
    end if

    DERIVATIVES(ns:ne,ZOO_C_INDEX) = &
        PROCESS_RATES(ns:ne,ZOO_C_INDEX, 1) - PROCESS_RATES(ns:ne,ZOO_C_INDEX, 2) - &
        PROCESS_RATES(ns:ne,ZOO_C_INDEX, 3) - PROCESS_RATES(ns:ne,ZOO_C_INDEX, 4)

    ! Debug code for ZOO_C
    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_ZOOC_01 &
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier

    !ZOOPLANKTON NITROGEN
    DERIVATIVES(ns:ne,ZOO_N_INDEX) = DERIVATIVES(ns:ne,ZOO_C_INDEX) * ACTUAL_ZOO_N_TO_C(ns:ne)

    !ZOOPLANKTON PHOSPHORUS
    DERIVATIVES(ns:ne,ZOO_P_INDEX) = DERIVATIVES(ns:ne,ZOO_C_INDEX) * ACTUAL_ZOO_P_TO_C(ns:ne)

    if (ZOOP_OPTION_1 > 0) then

            PROCESS_RATES(ns:ne,ZOO_N_INDEX, 1) = R_ZOO_FEEDING_DIA(ns:ne) * DIA_N_TO_C
            PROCESS_RATES(ns:ne,ZOO_N_INDEX, 2) = R_ZOO_FEEDING_CYN(ns:ne) * CYN_N_TO_C
            PROCESS_RATES(ns:ne,ZOO_N_INDEX, 3) = R_ZOO_FEEDING_OPA(ns:ne) * OPA_N_TO_C

            if (DO_NON_OBLIGATORY_FIXERS > 0) then
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 4) = R_ZOO_FEEDING_FIX_CYN(ns:ne) * FIX_CYN_N_TO_C
            else
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 4) = 0.0D0
            end if

            PROCESS_RATES(ns:ne,ZOO_N_INDEX, 5) = R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne) * ACTUAL_DET_N_TO_C(ns:ne)
            PROCESS_RATES(ns:ne,ZOO_N_INDEX, 6) = R_ZOO_EX_DON(ns:ne)
            PROCESS_RATES(ns:ne,ZOO_N_INDEX, 7) = R_ZOO_TOT_RESP(ns:ne)               * ACTUAL_ZOO_N_TO_C(ns:ne)
            PROCESS_RATES(ns:ne,ZOO_N_INDEX, 8) = R_ZOO_DEATH(ns:ne)                  * ACTUAL_ZOO_N_TO_C(ns:ne)

            PROCESS_RATES(ns:ne,ZOO_N_INDEX, 9) = ACTUAL_ZOO_N_TO_C(ns:ne)

            if (DO_NOSTOCALES > 0) then
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 10) = R_ZOO_FEEDING_NOST_VEG_HET(ns:ne) * NOST_N_TO_C
            else
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 10) = 0.0D0
            end if

            PROCESS_RATES(ns:ne,ZOO_P_INDEX, 1) = R_ZOO_FEEDING_DIA(ns:ne)            * DIA_P_TO_C
            PROCESS_RATES(ns:ne,ZOO_P_INDEX, 2) = R_ZOO_FEEDING_CYN(ns:ne)            * CYN_P_TO_C
            PROCESS_RATES(ns:ne,ZOO_P_INDEX, 3) = R_ZOO_FEEDING_OPA(ns:ne)            * OPA_P_TO_C

            if (DO_NON_OBLIGATORY_FIXERS > 0) then
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 4) = R_ZOO_FEEDING_FIX_CYN(ns:ne) * FIX_CYN_P_TO_C
            else
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 4) = 0.0D0
            end if

            PROCESS_RATES(ns:ne,ZOO_P_INDEX, 5) = R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne) * ACTUAL_DET_P_TO_C(ns:ne)
            PROCESS_RATES(ns:ne,ZOO_P_INDEX, 6) = R_ZOO_EX_DOP(ns:ne)
            PROCESS_RATES(ns:ne,ZOO_P_INDEX, 7) = R_ZOO_TOT_RESP(ns:ne)               * ACTUAL_ZOO_P_TO_C(ns:ne)
            PROCESS_RATES(ns:ne,ZOO_P_INDEX, 8) = R_ZOO_DEATH(ns:ne)                  * ACTUAL_ZOO_P_TO_C(ns:ne)

            PROCESS_RATES(ns:ne,ZOO_P_INDEX, 9) = ACTUAL_ZOO_P_TO_C(ns:ne)

            if (DO_NOSTOCALES > 0) then
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 10) = R_ZOO_FEEDING_NOST_VEG_HET(ns:ne) * NOST_P_TO_C
            else
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 10) = 0.0D0
            end if

            DERIVATIVES(ns:ne,ZOO_N_INDEX) = &
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 1)  + PROCESS_RATES(ns:ne,ZOO_N_INDEX, 2)  + &
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 3)  + PROCESS_RATES(ns:ne,ZOO_N_INDEX, 4)  + &
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 5)  - PROCESS_RATES(ns:ne,ZOO_N_INDEX, 6)  - &
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 7)  - PROCESS_RATES(ns:ne,ZOO_N_INDEX, 8)  + &
                PROCESS_RATES(ns:ne,ZOO_N_INDEX, 10)

            DERIVATIVES(ns:ne,ZOO_P_INDEX) = &
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 1)  + PROCESS_RATES(ns:ne,ZOO_P_INDEX, 2)  + &
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 3)  + PROCESS_RATES(ns:ne,ZOO_P_INDEX, 4)  + &
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 5)  - PROCESS_RATES(ns:ne,ZOO_P_INDEX, 6)  - &
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 7)  - PROCESS_RATES(ns:ne,ZOO_P_INDEX, 8)  + &
                PROCESS_RATES(ns:ne,ZOO_P_INDEX, 10)
    end if

    !DEAD ORGANIC CARBON PARTICLES
    PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 1) = R_DIA_DEATH(ns:ne)
    PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 2) = R_CYN_DEATH(ns:ne)
    PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 3) = R_OPA_DEATH(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 4) = R_FIX_CYN_DEATH(ns:ne)
    else
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 5) = R_ZOO_DEATH(ns:ne)
    PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 6) = R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne)
    PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 7) = R_DET_PART_ORG_C_DISSOLUTION(ns:ne)

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 8) = R_NOST_VEG_HET_DEATH(ns:ne)
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 9) = R_DENS_MORT_NOST_VEG_HET(ns:ne)

        ! Keep it zero. Mortality of akinetes is negligible
        ! Before germination they sit in mud, not in WC
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX,10) = R_MORT_AKI(ns:ne)
    else
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 8)  = 0.0D0
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 9)  = 0.0D0
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 10) = 0.0D0
    end if

    DERIVATIVES(ns:ne,DET_PART_ORG_C_INDEX) = &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 1) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 2) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 3) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 4) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 5) - &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 6) - &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 7) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 8) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 9) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_C_INDEX, 10)

    ! Debug code for DET_PART_ORG_C
    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_DET_PART_ORG_C_01 &
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier

    !DEAD ORGANIC NITROGEN PARTICLES
    PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 1) = R_DIA_DEATH(ns:ne) * DIA_N_TO_C
    PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 2) = R_CYN_DEATH(ns:ne) * CYN_N_TO_C
    PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 3) = R_OPA_DEATH(ns:ne) * OPA_N_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 4) = R_FIX_CYN_DEATH(ns:ne) * FIX_CYN_N_TO_C
    else
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 5) = R_ZOO_DEATH(ns:ne)                  * ACTUAL_ZOO_N_TO_C(ns:ne)
    PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 6) = R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne) * ACTUAL_DET_N_TO_C(ns:ne)
    PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 7) = R_DET_PART_ORG_N_DISSOLUTION(ns:ne)

    ! Auxiliary
    PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 8) = ACTUAL_DET_N_TO_C(ns:ne)

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 9 ) = R_NOST_VEG_HET_DEATH(ns:ne) * NOST_N_TO_C
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 10) = R_DENS_MORT_NOST_VEG_HET(ns:ne) * NOST_N_TO_C

        ! Keep it zero. Mortality of akinetes is negligible
        ! Before germination they st in mud, not in WC
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 11) = R_MORT_AKI(ns:ne) * NOST_N_TO_C
    else
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX,  9) = 0.0D0
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 10) = 0.0D0
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 11) = 0.0D0
    end if

    DERIVATIVES(ns:ne,DET_PART_ORG_N_INDEX) = &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 1)  + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 2)  + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 3)  + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 4)  + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 5)  - &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 6)  - &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 7)  + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 9)  + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 10) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_N_INDEX, 11)

    !DEAD ORGANIC PHOSPHORUS PARTICLES
    PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 1) = R_DIA_DEATH(ns:ne) * DIA_P_TO_C
    PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 2) = R_CYN_DEATH(ns:ne) * CYN_P_TO_C
    PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 3) = R_OPA_DEATH(ns:ne) * OPA_P_TO_C

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 4) = R_FIX_CYN_DEATH(ns:ne) * FIX_CYN_P_TO_C
    else
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 4) = 0.0D0
    end if

    PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 5) = R_ZOO_DEATH(ns:ne)                  * ACTUAL_ZOO_P_TO_C(ns:ne)
    PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 6) = R_ZOO_FEEDING_DET_PART_ORG_C(ns:ne) * ACTUAL_DET_P_TO_C(ns:ne)
    PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 7) = R_DET_PART_ORG_P_DISSOLUTION(ns:ne)

    ! Auxiliary
    PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 8) = ACTUAL_DET_P_TO_C(ns:ne)

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 9)  = R_NOST_VEG_HET_DEATH(ns:ne) * NOST_P_TO_C
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 10) = R_DENS_MORT_NOST_VEG_HET(ns:ne) * NOST_P_TO_C

        ! Keep it zero. Mortality of akinetes is negligible
        ! Before germination they sit in mud, not in WC
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 11) = R_MORT_AKI(ns:ne) * NOST_P_TO_C
    else
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX,  9) = 0.0D0
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 10) = 0.0D0
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 11) = 0.0D0
    end if

    DERIVATIVES(ns:ne,DET_PART_ORG_P_INDEX) = &
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 1)  + PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 2) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 3)  + PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 4) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 5)  - PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 6) - &
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 7)  + PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 9) + &
        PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 10) + PROCESS_RATES(ns:ne,DET_PART_ORG_P_INDEX, 11)

    !PARTICULATE SILICA
    PROCESS_RATES(ns:ne,PART_Si_INDEX, 1) = R_DIA_DEATH(ns:ne) * DIA_Si_TO_C
    PROCESS_RATES(ns:ne,PART_Si_INDEX, 2) = R_ZOO_FEEDING_DIA(ns:ne) * DIA_Si_TO_C
    PROCESS_RATES(ns:ne,PART_Si_INDEX, 3) = R_PART_Si_DISS(ns:ne)

    DERIVATIVES(ns:ne,PART_Si_INDEX) = &
        PROCESS_RATES(ns:ne,PART_Si_INDEX, 1) + PROCESS_RATES(ns:ne,PART_Si_INDEX, 2) - &
        PROCESS_RATES(ns:ne,PART_Si_INDEX, 3)

    !DISSOLVED ORGANIC CARBON
    PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 1) = R_DET_PART_ORG_C_DISSOLUTION(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 2) = R_ZOO_EX_DOC(ns:ne)

    PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 3)  = &
        R_ABIOTIC_DOC_MIN_DOXY(ns:ne)   + R_ABIOTIC_DOC_MIN_NO3N(ns:ne)     + R_ABIOTIC_DOC_MIN_MN_IV(ns:ne) + &
        R_ABIOTIC_DOC_MIN_FE_III(ns:ne) + R_ABIOTIC_DOC_MIN_S_PLUS_6(ns:ne) + R_ABIOTIC_DOC_MIN_DOC(ns:ne)

    PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 4) = R_DIA_EXCR(ns:ne) + R_CYN_EXCR(ns:ne) + R_OPA_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 5) = R_DIA_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 6) = R_CYN_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 7) = R_OPA_EXCR(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 4) = &
            PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 4) + R_FIX_CYN_EXCR(ns:ne)

        PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 8) = R_FIX_CYN_EXCR(ns:ne)
    else
        PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 8) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 4) = &
            PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 4) + R_NOST_VEG_HET_EXCR(ns:ne)

        PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 9) = R_NOST_VEG_HET_EXCR(ns:ne)
    else
        PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 9) = 0.0D0
    end if

    DERIVATIVES(ns:ne,DISS_ORG_C_INDEX) = &
        PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 1) + PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 2) - &
        PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 3) + PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX, 4)

    !DISSOLVED ORGANIC NITROGEN
    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 1) = R_DET_PART_ORG_N_DISSOLUTION(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 2) = R_ZOO_EX_DON(ns:ne)

    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 3)  = &
        R_ABIOTIC_DON_MIN_DOXY(ns:ne)   + R_ABIOTIC_DON_MIN_NO3N(ns:ne)     + R_ABIOTIC_DON_MIN_MN_IV(ns:ne) + &
        R_ABIOTIC_DON_MIN_FE_III(ns:ne) + R_ABIOTIC_DON_MIN_S_PLUS_6(ns:ne) + R_ABIOTIC_DON_MIN_DOC(ns:ne)

    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 4) = &
        (R_DIA_EXCR(ns:ne) * DIA_N_TO_C) + (R_CYN_EXCR(ns:ne) * CYN_N_TO_C) + (R_OPA_EXCR(ns:ne) * OPA_N_TO_C)

    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 5) = &
        R_CYN_GROWTH(ns:ne) * CYN_N_TO_C * (1.D0 - PREF_DIN_DON_CYN(ns:ne))

    if(DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 6) = &
            R_NOST_VEG_HET_NON_FIX_GROWTH(ns:ne) * NOST_N_TO_C * (1.D0 - PREF_DIN_DON_NOST(ns:ne))
    else
       PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 6) = &
           R_NON_FIX_CYN_GROWTH(ns:ne) * PREF_NH4N_DON_FIX_CYN(ns:ne) * CYN_N_TO_C * &
           ((DISS_ORG_N(ns:ne) * frac_avail_DON) / max(NH4_N(ns:ne) + (DISS_ORG_N(ns:ne) * frac_avail_DON), 1.0D-10))
    end if

    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 7) = R_DIA_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 8) = R_CYN_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 9) = R_OPA_EXCR(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 4) = &
            PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 4) + (R_FIX_CYN_EXCR(ns:ne) * FIX_CYN_N_TO_C)

        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 10) = R_FIX_CYN_EXCR(ns:ne) * FIX_CYN_N_TO_C
    else
        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 10) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 4) = &
            PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 4) + (R_NOST_VEG_HET_EXCR(ns:ne) * NOST_N_TO_C)

        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 11) = R_NOST_VEG_HET_EXCR(ns:ne) * NOST_N_TO_C

         PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 5) = &
        (R_CYN_GROWTH(ns:ne) * CYN_N_TO_C * (1.D0 - PREF_DIN_DON_CYN(ns:ne)))
    else
        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 11) = 0.0D0
    end if
    PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 12) = PHYT_TOT_C(ns:ne)

    DERIVATIVES(ns:ne,DISS_ORG_N_INDEX) = &
        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 1) + PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 2) - &
        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 3) + PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 4) - &
        PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 5) - PROCESS_RATES(ns:ne,DISS_ORG_N_INDEX, 6)

     ! Debug code for  DISS_ORG_N
    !$omp barrier
    !$omp master
     if(debug_stranger) then
         call DBGSTR_PEL_DET_DISS_ORG_N_01 &
              (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier


    !DISSOLVED ORGANIC PHOSPHORUS
    PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 1) = R_DET_PART_ORG_P_DISSOLUTION(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 2) = R_ZOO_EX_DOP(ns:ne)

    PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 3) =  &
        R_ABIOTIC_DOP_MIN_DOXY(ns:ne)   + R_ABIOTIC_DOP_MIN_NO3N(ns:ne)     + R_ABIOTIC_DOP_MIN_MN_IV(ns:ne) + &
        R_ABIOTIC_DOP_MIN_FE_III(ns:ne) + R_ABIOTIC_DOP_MIN_S_PLUS_6(ns:ne) + R_ABIOTIC_DOP_MIN_DOC(ns:ne)

    PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 4) = &
        (R_DIA_EXCR(ns:ne) * DIA_P_TO_C) + (R_CYN_EXCR(ns:ne) * CYN_P_TO_C) + (R_OPA_EXCR(ns:ne) * OPA_P_TO_C)

    PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 5) = R_DIA_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 6) = R_CYN_EXCR(ns:ne)
    PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 7) = R_OPA_EXCR(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 4) = &
            PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 4) + (R_FIX_CYN_EXCR(ns:ne) * FIX_CYN_P_TO_C)

        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 8) = R_FIX_CYN_EXCR(ns:ne) * FIX_CYN_P_TO_C
    else
        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 8) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 4) = &
            PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 4) + (R_NOST_VEG_HET_EXCR(ns:ne) * NOST_P_TO_C)

        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 9) = R_NOST_VEG_HET_EXCR(ns:ne) * NOST_P_TO_C

        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 10) = &
        (R_NOST_VEG_HET_GROWTH(ns:ne) * NOST_P_TO_C * (1.D0 - PREF_DIP_DOP_NOST(ns:ne)))
    else
        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 9)  = 0.0D0
        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 10) = 0.0D0
    end if

    DERIVATIVES(ns:ne,DISS_ORG_P_INDEX) = &
        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 1) + PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 2) - &
        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 3) + PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 4) - &
        PROCESS_RATES(ns:ne,DISS_ORG_P_INDEX, 10)

    ! Kinetic sub model for dissolved inorganic carbon

    ! Sources
    R_DIA_TOT_RESP(ns:ne)          = PROCESS_RATES(ns:ne,DIA_C_INDEX          , 2)
    R_CYN_TOT_RESP(ns:ne)          = PROCESS_RATES(ns:ne,CYN_C_INDEX          , 2)
    R_FIX_CYN_TOT_RESP(ns:ne)      = PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX      , 2)
    R_OPA_TOT_RESP(ns:ne)          = PROCESS_RATES(ns:ne,OPA_C_INDEX          , 2)
    R_ZOO_RESP(ns:ne)              = PROCESS_RATES(ns:ne,ZOO_C_INDEX          , 3)
    R_ABIOTIC_DOC_MIN(ns:ne)       = PROCESS_RATES(ns:ne,DISS_ORG_C_INDEX     , 3)
    R_NOST_VEG_HET_TOT_RESP(ns:ne) = PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX , 2)

    TOTAL_DIC_KINETIC_SOURCES(ns:ne) = &
        R_DIA_TOT_RESP(ns:ne)             + R_CYN_TOT_RESP(ns:ne)             + &
        R_OPA_TOT_RESP(ns:ne)             + R_ZOO_RESP(ns:ne)                 + &
        R_ABIOTIC_DOC_MIN_DOXY(ns:ne)     + R_ABIOTIC_DOC_MIN_NO3N(ns:ne)     + &
        R_ABIOTIC_DOC_MIN_MN_IV(ns:ne)    + R_ABIOTIC_DOC_MIN_FE_III(ns:ne)   + &
        R_ABIOTIC_DOC_MIN_S_PLUS_6(ns:ne) + R_METHANOGENESIS(ns:ne) + &
        R_METHANE_OXIDATION(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        TOTAL_DIC_KINETIC_SOURCES(ns:ne) = TOTAL_DIC_KINETIC_SOURCES(ns:ne) + R_FIX_CYN_TOT_RESP(ns:ne)
    end if

    if (DO_NOSTOCALES > 0) then
        TOTAL_DIC_KINETIC_SOURCES(ns:ne) = TOTAL_DIC_KINETIC_SOURCES(ns:ne) + R_NOST_VEG_HET_TOT_RESP(ns:ne)
    end if

    ! One-shot CHLA negative dump: if computed CHLA went negative in CHLA routine, print full local context once
    !$omp barrier
    !$omp master
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
    !$omp end master
    !$omp barrier

    ! Sinks
    R_DIA_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,DIA_C_INDEX         , 1)
    R_CYN_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,CYN_C_INDEX         , 1)
    R_FIX_CYN_GROWTH(ns:ne)      = PROCESS_RATES(ns:ne,FIX_CYN_C_INDEX     , 1)
    R_OPA_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,OPA_C_INDEX         , 1)
    R_NOST_VEG_HET_GROWTH(ns:ne) = PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 1)

    TOTAL_DIC_KINETIC_SINKS(ns:ne) = R_DIA_GROWTH(ns:ne) + R_CYN_GROWTH(ns:ne) + R_OPA_GROWTH(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        TOTAL_DIC_KINETIC_SINKS(ns:ne) = TOTAL_DIC_KINETIC_SINKS(ns:ne) + R_FIX_CYN_GROWTH(ns:ne)
    end if

    if (DO_NOSTOCALES > 0) then
        TOTAL_DIC_KINETIC_SINKS(ns:ne) = TOTAL_DIC_KINETIC_SINKS(ns:ne) + R_NOST_VEG_HET_GROWTH(ns:ne)
    end if

    ! Atmospheric exchange
    T_A(ns:ne) = TEMP(ns:ne) + CELSIUS_TO_KELVIN

    ! Calculate the saturaion concentration of CO2 in water
    P_K_H(ns:ne)   = -(2385.73D0 / T_A(ns:ne)) - (0.0152642D0 * T_A(ns:ne)) + 14.0184D0
    K_H(ns:ne)     = 10.0D0 ** (-P_K_H(ns:ne))
    P_CO2(ns:ne)   = 10.0D0 ** (-3.416D0)
    CO2_SAT(ns:ne) = K_H(ns:ne) * P_CO2(ns:ne)         !In moles

    ! Calculate the rearation rate constant for CO2
    K_A_CALC_CO2(ns:ne) = K_A_CALC(ns:ne) * 0.923D0

    ! Finally calculate the atmospheric exchange rate
    CO2_ATM_EXHANGE(ns:ne) = K_A_CALC_CO2(ns:ne) * (CO2_SAT(ns:ne) - (H2CO3(ns:ne)/1.0D6))

    !----------------------------------------------------------------------
    ! 2 February 2015
    ! New code added to account the effect of ice cover.
    !----------------------------------------------------------------------
    CO2_ATM_EXHANGE(ns:ne) = CO2_ATM_EXHANGE(ns:ne) * (1.0D0 - ice_cover(ns:ne))
    !----------------------------------------------------------------------
    ! End of new code added to account the effect of ice cover.
    !----------------------------------------------------------------------

    ! End of atmospheric exchange

    ! Calculate the total derivative and convert it to moles
    if (CONSIDER_INORG_C_DERIVATIVE > 0) then
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 1) = TOTAL_DIC_KINETIC_SOURCES(ns:ne) / 12000.0D0
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 2) = TOTAL_DIC_KINETIC_SINKS(ns:ne)   / 12000.0D0

        if (CONSIDER_CO2_REARATION > 0) then
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 3) = CO2_ATM_EXHANGE(ns:ne)
        else
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 3) = 0.0D0
            CO2_ATM_EXHANGE(ns:ne) = 0.0D0
        end if

        PROCESS_RATES(ns:ne,INORG_C_INDEX, 4)  = R_DIA_TOT_RESP(ns:ne)
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 5)  = R_CYN_TOT_RESP(ns:ne)

        PROCESS_RATES(ns:ne,INORG_C_INDEX, 7)  = R_OPA_TOT_RESP(ns:ne)
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 8)  = R_ZOO_RESP(ns:ne)
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 9)  = R_ABIOTIC_DOC_MIN(ns:ne)
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 10) = R_DIA_GROWTH(ns:ne)
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 11) = R_CYN_GROWTH(ns:ne)
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 13) = R_OPA_GROWTH(ns:ne)

        if (DO_NON_OBLIGATORY_FIXERS > 0) then
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 6)  = R_FIX_CYN_TOT_RESP(ns:ne)
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 12) = R_FIX_CYN_GROWTH(ns:ne)
        else
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 6)  = 0.0D0
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 12) = 0.0D0
        end if

        if (DO_NOSTOCALES > 0) then
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 14) = R_NOST_VEG_HET_TOT_RESP(ns:ne)
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 15) = R_NOST_VEG_HET_GROWTH(ns:ne)
        else
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 14) = 0.0D0
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 15) = 0.0D0
        end if

        DIC_KINETIC_DERIVATIVE(ns:ne) = CO2_ATM_EXHANGE(ns:ne) + &
            ((TOTAL_DIC_KINETIC_SOURCES(ns:ne) - TOTAL_DIC_KINETIC_SINKS(ns:ne)) / 12000.0D0)

        DERIVATIVES(ns:ne,INORG_C_INDEX) = DIC_KINETIC_DERIVATIVE(ns:ne)
    else
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 1) = 0.0D0
        PROCESS_RATES(ns:ne,INORG_C_INDEX, 2) = 0.0D0

        if (CONSIDER_CO2_REARATION > 0) then
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 3) = CO2_ATM_EXHANGE(ns:ne)
        else
            PROCESS_RATES(ns:ne,INORG_C_INDEX, 3) = 0.0D0
            CO2_ATM_EXHANGE(ns:ne) = 0.0D0
        end if

        PROCESS_RATES(ns:ne,INORG_C_INDEX, 4:15) = 0.0D0

        if (CONSIDER_CO2_REARATION > 0) then
            DERIVATIVES(ns:ne,INORG_C_INDEX) = CO2_ATM_EXHANGE(ns:ne)
        else
            DERIVATIVES(ns:ne,INORG_C_INDEX) = 0.0D0
        end if
    end if

    ! -------------------------------------------------------------------------
    ! 29 JANUARY 2016, KINETIC DERIVATIVES FOR THE NEW STATE VARIABLES
    ! -------------------------------------------------------------------------
    if (DO_ADVANCED_REDOX_SIMULATION > 0) then

        ! Calcium
        DERIVATIVES(ns:ne,CA_INDEX) = 0.0D0

        ! Magnesium
        DERIVATIVES(ns:ne,MG_INDEX) = 0.0D0

        ! Suphate sulphur
        PROCESS_RATES(ns:ne, S_PLUS_6_INDEX, 1) = R_SULPHIDE_OXIDATION(ns:ne)
        PROCESS_RATES(ns:ne, S_PLUS_6_INDEX, 2) = R_SULPHATE_REDUCTION(ns:ne)

        DERIVATIVES(ns:ne,S_PLUS_6_INDEX) = &
            PROCESS_RATES(ns:ne, S_PLUS_6_INDEX, 1) - &
            PROCESS_RATES(ns:ne, S_PLUS_6_INDEX, 2)

        ! Sulphide sulphur
        PROCESS_RATES(ns:ne, S_MINUS_2_INDEX, 1) = H2S_ATM_EXCHANGE(ns:ne)
        PROCESS_RATES(ns:ne, S_MINUS_2_INDEX, 2) = R_SULPHATE_REDUCTION(ns:ne)
        PROCESS_RATES(ns:ne, S_MINUS_2_INDEX, 3) = R_SULPHIDE_OXIDATION(ns:ne)

        DERIVATIVES(ns:ne,S_MINUS_2_INDEX) = &
            PROCESS_RATES(ns:ne, S_MINUS_2_INDEX, 1) + &
            PROCESS_RATES(ns:ne, S_MINUS_2_INDEX, 2) - &
            PROCESS_RATES(ns:ne, S_MINUS_2_INDEX, 3)

        ! Methane carbon
        PROCESS_RATES(ns:ne, CH4_C_INDEX, 1) = CH4_ATM_EXCHANGE(ns:ne)
        PROCESS_RATES(ns:ne, CH4_C_INDEX, 2) = R_METHANOGENESIS(ns:ne)
        PROCESS_RATES(ns:ne, CH4_C_INDEX, 3) = R_METHANE_OXIDATION(ns:ne)

        DERIVATIVES(ns:ne,CH4_C_INDEX) = &
            PROCESS_RATES(ns:ne, CH4_C_INDEX, 1) + PROCESS_RATES(ns:ne, CH4_C_INDEX, 2) - &
            PROCESS_RATES(ns:ne, CH4_C_INDEX, 3)
    end if

    ! Debug code for Methane
    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_DET_CH4_C_01 &
             (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier

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
    T_A(ns:ne)      = TEMP(ns:ne) + CELSIUS_TO_KELVIN
    pKH(ns:ne)      = 9.018D-2 + (2.72992D3 / T_A(ns:ne))
    FRAC_NH3(ns:ne) = 1.0D0 / (1.0D0 + (10.0D0 ** (pKH(ns:ne) - PH(ns:ne))))
    FRAC_NH4(ns:ne) = 1.0D0 - FRAC_NH3(ns:ne)
    ! -------------------------------------------------------------------------
    ! End of calculate NH4 and NH3 fractions in ammonia
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the NH4N in fluxes and out fluxes from fluxes
    ! -------------------------------------------------------------------------
    where(SEDIMENT_FLUXES(ns:ne, NH4_N_INDEX) .ge. 0.0D0)
        NH4N_SEDIMENT_INFLUX(ns:ne)  = SEDIMENT_FLUXES(ns:ne, NH4_N_INDEX)* FRAC_NH4(ns:ne)
        NH4N_SEDIMENT_OUTFLUX(ns:ne) = 0.0D0
    else where
        NH4N_SEDIMENT_INFLUX(ns:ne)  = 0.0D0
        NH4N_SEDIMENT_OUTFLUX(ns:ne) = (-SEDIMENT_FLUXES(ns:ne, NH4_N_INDEX))*FRAC_NH4(ns:ne)
    end where
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the NO3N in fluxes and out fluxes from fluxes
    ! -------------------------------------------------------------------------
    where(SEDIMENT_FLUXES(ns:ne, NH4_N_INDEX) .ge. 0.0D0)
        NO3N_SEDIMENT_INFLUX(ns:ne)  = SEDIMENT_FLUXES(ns:ne, NO3_N_INDEX)
        NO3N_SEDIMENT_OUTFLUX(ns:ne) = 0.0D0
    else where
        NO3N_SEDIMENT_INFLUX(ns:ne)  = 0.0D0
        NO3N_SEDIMENT_OUTFLUX(ns:ne) = (-SEDIMENT_FLUXES(ns:ne, NO3_N_INDEX))
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
    N_DIA_TOT_RESP(ns:ne)            = PROCESS_RATES(ns:ne,NH4_N_INDEX, 1)  * FRAC_NH4(ns:ne)
    N_CYN_TOT_RESP(ns:ne)            = PROCESS_RATES(ns:ne,NH4_N_INDEX, 2)  * FRAC_NH4(ns:ne)
    N_OPA_TOT_RESP(ns:ne)            = PROCESS_RATES(ns:ne,NH4_N_INDEX, 3)  * FRAC_NH4(ns:ne)
    N_ZOO_TOT_RESP(ns:ne)            = PROCESS_RATES(ns:ne,NH4_N_INDEX, 5)  * FRAC_NH4(ns:ne)
    N_ABIOTIC_DON_MIN(ns:ne)         = PROCESS_RATES(ns:ne,NH4_N_INDEX, 11) * FRAC_NH4(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        N_FIX_CYN_TOT_RESP(ns:ne) = PROCESS_RATES(ns:ne,NH4_N_INDEX, 4)  * FRAC_NH4(ns:ne)
    else
        N_FIX_CYN_TOT_RESP(ns:ne) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        N_NOST_VEG_HET_TOT_RESP(ns:ne)    = PROCESS_RATES(ns:ne,NH4_N_INDEX, 17) * FRAC_NH4(ns:ne)
        N_NOST_VEG_HET_NH4_RELEASE(ns:ne) = PROCESS_RATES(ns:ne,NH4_N_INDEX, 22) * FRAC_NH4(ns:ne)
    else
        N_NOST_VEG_HET_TOT_RESP(ns:ne)    = 0.0D0
        N_NOST_VEG_HET_NH4_RELEASE(ns:ne) = 0.0D0
    end if

    ALK_GAINED_BY_AMMONIUM_GEN(ns:ne) = &
        (N_DIA_TOT_RESP(ns:ne)            + N_CYN_TOT_RESP(ns:ne)             + &
         N_OPA_TOT_RESP(ns:ne)            + N_FIX_CYN_TOT_RESP(ns:ne)         + &
         N_NOST_VEG_HET_TOT_RESP(ns:ne)   + N_ZOO_TOT_RESP(ns:ne)             + &
         N_ABIOTIC_DON_MIN(ns:ne)         + N_NOST_VEG_HET_NH4_RELEASE(ns:ne) + &
         NH4N_SEDIMENT_INFLUX(ns:ne)) / 14007.0D0
    ! -------------------------------------------------------------------------
    ! End of calculate the alkalinity gain by ammonium generation
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the alkality gain by nitrate consumption
    ! 1 eq alk for each nitrate consumed since one negative ion is lost
    ! -------------------------------------------------------------------------
    N_DENITRIFICATION(ns:ne)     = PROCESS_RATES(ns:ne,NO3_N_INDEX, 2)
    N_DIA_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,NO3_N_INDEX, 3)
    N_CYN_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,NO3_N_INDEX, 4)
    N_OPA_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,NO3_N_INDEX, 5)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        N_NON_FIX_CYN_GROWTH(ns:ne) = PROCESS_RATES(ns:ne,NO3_N_INDEX, 6)
    else
        N_NON_FIX_CYN_GROWTH(ns:ne) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        NO3N_NOST_VEG_HET_GROWTH(ns:ne) = PROCESS_RATES(ns:ne,NO3_N_INDEX, 6)
    else
        NO3N_NOST_VEG_HET_GROWTH(ns:ne) = 0.0D0
    end if

    ! Nothing for nostocales since they are fixers

    ALK_GAINED_BY_NITRATE_CONS(ns:ne) = &
        (N_DENITRIFICATION(ns:ne)        + N_DIA_GROWTH(ns:ne) + N_CYN_GROWTH(ns:ne) + &
         N_OPA_GROWTH(ns:ne)             + N_NON_FIX_CYN_GROWTH(ns:ne)        + &
         NO3N_NOST_VEG_HET_GROWTH(ns:ne) + NO3N_SEDIMENT_OUTFLUX(ns:ne)) / 14007.0D0
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
    N_DIA_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,NH4_N_INDEX, 6) * FRAC_NH4(ns:ne)
    N_CYN_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,NH4_N_INDEX, 7) * FRAC_NH4(ns:ne)
    N_OPA_GROWTH(ns:ne)          = PROCESS_RATES(ns:ne,NH4_N_INDEX, 8) * FRAC_NH4(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        N_NON_FIX_CYN_GROWTH(ns:ne)  = PROCESS_RATES(ns:ne,NH4_N_INDEX, 9) * FRAC_NH4(ns:ne)
    else
        N_NON_FIX_CYN_GROWTH(ns:ne) = 0.0D0
    end if


    if (DO_NOSTOCALES > 0) then
        !growth supported by water ammonia (not fixed)
        NH4N_NOST_VEG_HET_GROWTH(ns:ne) = PROCESS_RATES(ns:ne,NH4_N_INDEX, 23) * FRAC_NH4(ns:ne)
    else
        NH4N_NOST_VEG_HET_GROWTH(ns:ne) = 0.0D0
    end if

    ALK_LOST_BY_AMMONIUM_CONS(ns:ne) = &
        (N_DIA_GROWTH(ns:ne)             + N_CYN_GROWTH(ns:ne)         + &
         N_OPA_GROWTH(ns:ne)             + N_NON_FIX_CYN_GROWTH(ns:ne) + &
         NH4N_NOST_VEG_HET_GROWTH(ns:ne) + NH4N_SEDIMENT_OUTFLUX(ns:ne)) / 14007.0D0
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
    N_NITRIFICATION_NH4(ns:ne) = PROCESS_RATES(ns:ne,NH4_N_INDEX,10) * FRAC_NH4(ns:ne)
    N_NITRIFICATION_NH3(ns:ne) = PROCESS_RATES(ns:ne,NH4_N_INDEX,10) * FRAC_NH3(ns:ne)

    ALK_LOST_BY_NITRIFICATION(ns:ne) = &
        ((2.0D0 * N_NITRIFICATION_NH4(ns:ne)) + N_NITRIFICATION_NH3(ns:ne)) / 14007.0D0
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

    do i = ns, ne
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
            K_ONE_TIP(ns:ne)   = 10.0D0 ** (-2.15D0)
            K_TWO_TIP(ns:ne)   = 10.0D0 ** (-7.20D0)
            K_THREE_TIP(ns:ne) = 10.0D0 ** (-2.15D0)

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

            K_ONE_TIP(ns:ne)   = exp(A_1 + (A_2 / TEMP(ns:ne)) + (A_3 * log(TEMP(ns:ne)))   + &
                              ((B_1 + (B_2 / TEMP(ns:ne))) * (SALT(ns:ne) ** 0.5D0)) + &
                              ((C_1 + (C_2 / TEMP(ns:ne))) * SALT(ns:ne)))

            A_1 = 172.0883D0
            A_2 = -8814.715D0
            A_3 = -27.927D0
            B_1 = 1.3566D0
            B_2 = -160.34D0
            C_1 = -0.05778D0
            C_2 = -0.37335D0

            K_TWO_TIP(ns:ne)   = exp(A_1 + (A_2 / TEMP(ns:ne)) + (A_3 * log(TEMP(ns:ne)))   + &
                              ((B_1 + (B_2 / TEMP(ns:ne))) * (SALT(ns:ne) ** 0.5D0)) + &
                              ((C_1 + (C_2 / TEMP(ns:ne))) * SALT(ns:ne)))

            A_1 = -18.141D0
            A_2 = -3070.75D0
            A_3 = 0.0D0
            B_1 = 2.81197D0
            B_2 = 17.27039D0
            C_1 = -0.09984D0
            C_2 = -44.99486D0

            K_THREE_TIP(ns:ne) = exp(A_1 + (A_2 / TEMP(ns:ne)) + (A_3 * log(TEMP(ns:ne)))   + &
                              ((B_1 + (B_2 / TEMP(ns:ne))) * (SALT(ns:ne) ** 0.5D0)) + &
                              ((C_1 + (C_2 / TEMP(ns:ne))) * SALT(ns:ne)))
    end select

    FRACTION_DIVISOR_TIP(ns:ne) = &
        (H_PLUS(ns:ne) * H_PLUS(ns:ne) * H_PLUS(ns:ne)) + (K_ONE_TIP(ns:ne) * H_PLUS(ns:ne) * H_PLUS(ns:ne)) + &
        (K_ONE_TIP(ns:ne) * K_TWO_TIP(ns:ne) * H_PLUS(ns:ne)) + (K_ONE_TIP(ns:ne) * K_TWO_TIP(ns:ne) * K_THREE_TIP(ns:ne))

    ALPHA_H2PO4(ns:ne) = (K_ONE_TIP(ns:ne) * H_PLUS(ns:ne)    * H_PLUS(ns:ne))      / FRACTION_DIVISOR_TIP(ns:ne)
    ALPHA_HPO4(ns:ne)  = (K_ONE_TIP(ns:ne) * K_TWO_TIP(ns:ne) * H_PLUS(ns:ne))      / FRACTION_DIVISOR_TIP(ns:ne)
    ALPHA_PO4(ns:ne)   = (K_ONE_TIP(ns:ne) * K_TWO_TIP(ns:ne) * K_THREE_TIP(ns:ne)) / FRACTION_DIVISOR_TIP(ns:ne)

    PHOSPHATE_EQ_CONSTANT(ns:ne) = &
        ALPHA_H2PO4(ns:ne) + (2.0D0 * ALPHA_HPO4(ns:ne)) + (3.0D0 * ALPHA_PO4(ns:ne))
    end do
    ! -------------------------------------------------------------------------
    ! End of calculate the dissociation constants of H2PO3
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Calculate the NH4N in fluxes and out fluxes from fluxes
    ! -------------------------------------------------------------------------
    where(SEDIMENT_FLUXES(ns:ne, PO4_P_INDEX) .ge. 0.0D0)

        PO4P_SEDIMENT_INFLUX(ns:ne)  = &
            SEDIMENT_FLUXES(ns:ne, PO4_P_INDEX)    * PHOSPHATE_EQ_CONSTANT(ns:ne)

        PO4P_SEDIMENT_OUTFLUX(ns:ne) = 0.0D0
    else where
        PO4P_SEDIMENT_INFLUX(ns:ne)  = 0.0D0

        PO4P_SEDIMENT_OUTFLUX(ns:ne) = &
            (-SEDIMENT_FLUXES(ns:ne, PO4_P_INDEX)) * PHOSPHATE_EQ_CONSTANT(ns:ne)
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
    P_DIA_GROWTH(ns:ne)          = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 6) * PHOSPHATE_EQ_CONSTANT(ns:ne)

    P_CYN_GROWTH(ns:ne)          = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 7) * PHOSPHATE_EQ_CONSTANT(ns:ne)

    P_OPA_GROWTH(ns:ne)          = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 8) * PHOSPHATE_EQ_CONSTANT(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        P_FIX_CYN_GROWTH(ns:ne) = &
            PROCESS_RATES(ns:ne,PO4_P_INDEX, 9) * PHOSPHATE_EQ_CONSTANT(ns:ne)
    else
        P_FIX_CYN_GROWTH(ns:ne) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        P_NOST_VEG_HET_GROWTH(ns:ne) = &
            PROCESS_RATES(ns:ne,PO4_P_INDEX, 14) * PHOSPHATE_EQ_CONSTANT(ns:ne)
    else
        P_NOST_VEG_HET_GROWTH(ns:ne) = 0.0D0
    end if

    ALK_GAINED_BY_PHOSPHATE_CONS(ns:ne) = &
        (P_DIA_GROWTH(ns:ne)     + P_CYN_GROWTH(ns:ne)          + P_OPA_GROWTH(ns:ne)         + &
         P_FIX_CYN_GROWTH(ns:ne) + P_NOST_VEG_HET_GROWTH(ns:ne) + PO4P_SEDIMENT_OUTFLUX(ns:ne)) / &
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
    P_DIA_TOT_RESP(ns:ne)     = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 1)  * PHOSPHATE_EQ_CONSTANT(ns:ne)

    P_CYN_TOT_RESP(ns:ne)     = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 2)  * PHOSPHATE_EQ_CONSTANT(ns:ne)

    P_OPA_TOT_RESP(ns:ne)     = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 3)  * PHOSPHATE_EQ_CONSTANT(ns:ne)

    P_ZOO_TOT_RESP(ns:ne)     = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 5)  * PHOSPHATE_EQ_CONSTANT(ns:ne)

    P_ABIOTIC_DOP_MIN(ns:ne)  = &
        PROCESS_RATES(ns:ne,PO4_P_INDEX, 10) * PHOSPHATE_EQ_CONSTANT(ns:ne)

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        P_FIX_CYN_TOT_RESP(ns:ne) = &
            PROCESS_RATES(ns:ne,PO4_P_INDEX, 4)  * PHOSPHATE_EQ_CONSTANT(ns:ne)
    else
        P_FIX_CYN_TOT_RESP(ns:ne) = 0.0D0
    end if

    if (DO_NOSTOCALES > 0) then
        P_NOST_VEG_HET_TOT_RESP(ns:ne) = &
            PROCESS_RATES(ns:ne,PO4_P_INDEX, 13)  * PHOSPHATE_EQ_CONSTANT(ns:ne)
    else
        P_NOST_VEG_HET_TOT_RESP(ns:ne) = 0.0D0
    end if

    ALK_LOST_BY_PHOSPHATE_GEN(ns:ne) = &
        (P_DIA_TOT_RESP(ns:ne)          + P_CYN_TOT_RESP(ns:ne) + P_OPA_TOT_RESP(ns:ne)    + &
         P_FIX_CYN_TOT_RESP(ns:ne)      + P_ZOO_TOT_RESP(ns:ne) + P_ABIOTIC_DOP_MIN(ns:ne) + &
         P_NOST_VEG_HET_TOT_RESP(ns:ne) + PO4P_SEDIMENT_INFLUX(ns:ne)) / 30974.0D0
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
        ALK_KINETIC_DERIVATIVE(ns:ne) = &
            ALK_GAINED_BY_AMMONIUM_GEN(ns:ne)         + &
            ALK_GAINED_BY_NITRATE_CONS(ns:ne)         + &
            ALK_GAINED_BY_PHOSPHATE_CONS(ns:ne)       - &
            ALK_LOST_BY_AMMONIUM_CONS(ns:ne)          - &
            ALK_LOST_BY_NITRIFICATION(ns:ne)          - &
            (NO3N_SEDIMENT_INFLUX(ns:ne) / 14007.0D0) - & ! Alkal�nity lost by NO3N influx
            ALK_LOST_BY_PHOSPHATE_GEN(ns:ne)
    else
        ALK_KINETIC_DERIVATIVE(ns:ne)       = 0.0D0
        ALK_GAINED_BY_AMMONIUM_GEN(ns:ne)   = 0.0D0
        ALK_GAINED_BY_NITRATE_CONS(ns:ne)   = 0.0D0
        ALK_GAINED_BY_PHOSPHATE_CONS(ns:ne) = 0.0D0
        ALK_LOST_BY_AMMONIUM_CONS(ns:ne)    = 0.0D0
        ALK_LOST_BY_NITRIFICATION(ns:ne)    = 0.0D0
        ALK_LOST_BY_PHOSPHATE_GEN(ns:ne)    = 0.0D0
    end if
    ! -------------------------------------------------------------------------
    ! END OF KINETIC SUBMODEL FOR ALKALINITY
    ! -------------------------------------------------------------------------

    DERIVATIVES(ns:ne,TOT_ALK_INDEX) = ALK_KINETIC_DERIVATIVE(ns:ne)

    PROCESS_RATES(ns:ne,TOT_ALK_INDEX, 1) = ALK_GAINED_BY_AMMONIUM_GEN(ns:ne)
    PROCESS_RATES(ns:ne,TOT_ALK_INDEX, 2) = ALK_GAINED_BY_NITRATE_CONS(ns:ne)
    PROCESS_RATES(ns:ne,TOT_ALK_INDEX, 3) = ALK_GAINED_BY_PHOSPHATE_CONS(ns:ne)
    PROCESS_RATES(ns:ne,TOT_ALK_INDEX, 4) = ALK_LOST_BY_AMMONIUM_CONS(ns:ne)
    PROCESS_RATES(ns:ne,TOT_ALK_INDEX, 5) = ALK_LOST_BY_NITRIFICATION(ns:ne)
    PROCESS_RATES(ns:ne,TOT_ALK_INDEX, 6) = ALK_LOST_BY_PHOSPHATE_GEN(ns:ne)
    PROCESS_RATES(ns:ne,TOT_ALK_INDEX, 7) = pH(ns:ne)

    !$omp barrier
    !$omp master
    if(debug_stranger) then
        call DBGSTR_PEL_DET_TOT_ALK_01 &
             (PROCESS_RATES, DERIVATIVES, PH, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    end if
    !$omp end master
    !$omp barrier

    ! New kinetic derivatives calculations added 9 September 2015

    if (DO_ADVANCED_REDOX_SIMULATION > 0) then
        ! After the introduction of changes in November 30 th 2015, it will be necessary
        ! to reconsinder kinetics, especially the mineral disoolotion processes for metals
        ! Job to be conducted wiht Petras together, and completed during next visit of
        ! of Ali to Lithuania. Also for iron and mangenese, the redox sequences as described
        ! by Van Chappen and Wang 2015 and Katsev papers should be included.


        ! FE_II
        PROCESS_RATES(ns:ne,FE_II_INDEX, 1) = R_FE_III_REDUCTION(ns:ne)
        PROCESS_RATES(ns:ne,FE_II_INDEX, 2) = R_FE_II_OXIDATION(ns:ne)

        ! Diagnostics:
        PROCESS_RATES(ns:ne,FE_II_INDEX, 3) = FE_II_DISS(ns:ne)
        PROCESS_RATES(ns:ne,FE_II_INDEX, 4) = FE_II_DISS(ns:ne)/FE_MOLAR_MASS_MG

        DERIVATIVES(ns:ne,FE_II_INDEX) = &
            PROCESS_RATES(ns:ne,FE_II_INDEX, 1) - PROCESS_RATES(ns:ne,FE_II_INDEX, 2)

        ! FE_III
        PROCESS_RATES(ns:ne,FE_III_INDEX, 1) = R_FE_II_OXIDATION(ns:ne)
        PROCESS_RATES(ns:ne,FE_III_INDEX, 2) = R_FE_III_REDUCTION(ns:ne)
        PROCESS_RATES(ns:ne,FE_III_INDEX, 3) = FE_III_DISS_EQ(ns:ne)

        DERIVATIVES(ns:ne,FE_III_INDEX) = &
            PROCESS_RATES(ns:ne,FE_III_INDEX, 1) - PROCESS_RATES(ns:ne,FE_III_INDEX, 2)


        ! MN_II
        PROCESS_RATES(ns:ne,MN_II_INDEX, 1) = R_MN_IV_REDUCTION(ns:ne)
        PROCESS_RATES(ns:ne,MN_II_INDEX, 2) = R_MN_II_OXIDATION(ns:ne)

        DERIVATIVES(ns:ne,MN_II_INDEX) = &
            PROCESS_RATES(ns:ne,MN_II_INDEX, 1) - PROCESS_RATES(ns:ne,MN_II_INDEX, 2)


        ! MN_IV
        PROCESS_RATES(ns:ne,MN_IV_INDEX, 1) = R_MN_II_OXIDATION(ns:ne)
        PROCESS_RATES(ns:ne,MN_IV_INDEX, 2) = R_MN_IV_REDUCTION(ns:ne)

        DERIVATIVES(ns:ne,MN_IV_INDEX) = &
            PROCESS_RATES(ns:ne,MN_IV_INDEX, 1) - PROCESS_RATES(ns:ne,MN_IV_INDEX, 2)
        ! End of new kinetic derivatives calculations added 9 September 2015

        ! -------------------------------------------------------------------------
        ! END OF INSERT INTO KINETIC DERIVATIVES
        ! -------------------------------------------------------------------------


        ! Checking all derivatives
    !$omp barrier
    !$omp master
        if(debug_stranger) then
            call DBGSTR_PEL_GEN_02 &
                 (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
        end if
    !$omp end master
    !$omp barrier

        ! Compact per-node/process clamp summary: one-shot, prints counts and sets CLAMP_WARNED
        if (allocated(CLAMP_COUNT)) then
            do i = ns, ne
                if (CLAMP_COUNT(i) > 0 .and. .not. CLAMP_WARNED(i)) then
                    !$omp critical
                    write(6,'(A,F12.4,A,I4,A,I6)') &
                        'WARN SUMMARY: TIME=', TIME, ' BOX=', i, ' CLAMPS=', CLAMP_COUNT(i)
                    if (NDIAGVAR > 0 .and. allocated(CLAMP_PROC_COUNT)) then
                        do k=1,min(NDIAGVAR, size(CLAMP_PROC_COUNT,2))
                            if (CLAMP_PROC_COUNT(i,k) > 0) then
                                write(6,'(A,I3,A,I6)') '  PROC k=', k, ' cnt=', CLAMP_PROC_COUNT(i,k)
                            end if
                        end do
                    end if
                    !$omp end critical
                    CLAMP_WARNED(i) = .true.
                end if
            end do
        end if
    end if

    end if  ! nkn_local > 0
    !$omp end parallel
    ! =========================================================================
    ! END OpenMP PARALLEL REGION
    ! =========================================================================

    !*********************************************'
    !*                                           *'
    !*          END OF ECOLOGY KINETICS          *'
    !*                                           *'
    !*      DO NOT CHANGE THE FOLLOWING CODE     *'
    !*                                           *'
    !*********************************************'

end subroutine AQUABC_PELAGIC_KINETICS
