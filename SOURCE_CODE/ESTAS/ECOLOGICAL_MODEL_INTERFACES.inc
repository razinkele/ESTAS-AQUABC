        interface
            subroutine AQUABC_PELAGIC_KINETICS &
               (node_active      ,       nkn, &
                STATE_VARIABLES  , DERIVATIVES  , nstate, &
                MODEL_CONSTANTS  , nconst               , &
                DRIVING_FUNCTIONS, n_driving_functions  , &
                FLAGS            , nflags               , &
                PROCESS_RATES    , NDIAGVAR             , &
                SAVED_OUTPUTS    , n_saved_outputs      , &
                PH               , &
                TIME, TIME_STEP  , DAY_OF_YEAR          , & 
				SEDIMENT_FLUXES  , &
				CALLED_BEFORE    , &
                SURFACE_BOXES    , ZOOP_OPTION_1        , &
                ADVANCED_REDOX_OPTION                   , &
                USER_ENTERED_frac_avail_DON             , &
                LIGHT_EXTINCTION_OPTION                 , &
                VERTICALLY_MIXED_OVER_DEPTH             , &
                CONSIDER_NON_OBLIGATORY_FIXERS          , &
                CONSIDER_NOSTOCALES)

                integer  :: nkn, nstate, nconst, n_driving_functions, nflags
                integer  :: n_saved_outputs, NDIAGVAR

                !internal numbers of nodes, used for diagnostics. Not implemented yet
                integer node_active(nkn)

                double precision, dimension(nkn,nstate),             intent(in)   :: STATE_VARIABLES
                double precision, dimension(nkn,nstate),             intent(out)  :: DERIVATIVES
                double precision, dimension(nconst),                 intent(in)   :: MODEL_CONSTANTS
                double precision, dimension(nkn,n_driving_functions),intent(in)   :: DRIVING_FUNCTIONS
                integer,          dimension(nflags),                 intent(in)   :: FLAGS
                double precision, dimension(nkn,nstate, NDIAGVAR),   intent(out)  :: PROCESS_RATES
                double precision, dimension(nkn)                                  :: PH
                double precision, dimension(nkn,n_saved_outputs),    intent(inout):: SAVED_OUTPUTS
                double precision, intent(in)                                      :: TIME
                double precision, intent(in)                                      :: TIME_STEP
				integer         , intent(in)                                      :: DAY_OF_YEAR
                double precision, intent(in) :: SEDIMENT_FLUXES(nkn, nstate)
                integer, intent(inout)                                            :: CALLED_BEFORE
                
                !Optional arguments
                integer, dimension(nkn), intent(in), optional                     :: SURFACE_BOXES
                integer                , intent(in), optional                     :: ZOOP_OPTION_1
                integer                , intent(in), optional                     :: ADVANCED_REDOX_OPTION
                double precision       , intent(in), optional                     :: USER_ENTERED_frac_avail_DON
                integer                , intent(in), optional                     :: LIGHT_EXTINCTION_OPTION
                integer                , intent(in), optional                     :: VERTICALLY_MIXED_OVER_DEPTH
                integer                , intent(in), optional                     :: CONSIDER_NON_OBLIGATORY_FIXERS
                integer                , intent(in), optional                     :: CONSIDER_NOSTOCALES
            end subroutine AQUABC_PELAGIC_KINETICS
        end interface

        interface
            subroutine chlorophyl_a_vec &
                       (DIA_C            , &
                        CYN_C            , &
                        OPA_C            , &
                        FIX_CYN_C        , &
                        NOST_VEG_HET_C   , &
                        DIA_C_TO_CHLA    , &
                        CYN_C_TO_CHLA    , &   
                        OPA_C_TO_CHLA    , &   
                        FIX_CYN_C_TO_CHLA, &
                        NOST_C_TO_CHLA   , &
                        nkn              , & 
                        CHLA             , &
                        CONSIDER_NON_OBLIGATORY_FIXERS, &
                        CONSIDER_NOSTOCALES)
            
                implicit none
            
                double precision, dimension(nkn), intent(in) :: DIA_C
                double precision, dimension(nkn), intent(in) :: CYN_C
                double precision, dimension(nkn), intent(in) :: OPA_C
                double precision, dimension(nkn), intent(in) :: FIX_CYN_C
                double precision, dimension(nkn), intent(in) :: NOST_VEG_HET_C
            
                double precision, intent(in) :: DIA_C_TO_CHLA
                double precision, intent(in) :: CYN_C_TO_CHLA
                double precision, intent(in) :: FIX_CYN_C_TO_CHLA
                double precision, intent(in) :: OPA_C_TO_CHLA
                double precision, intent(in) :: NOST_C_TO_CHLA
            
                integer, intent(in) :: nkn
            
                double precision, dimension(nkn), intent(inout) :: chla
            
                integer, intent(in), optional :: CONSIDER_NON_OBLIGATORY_FIXERS
                integer, intent(in), optional :: CONSIDER_NOSTOCALES
            end subroutine chlorophyl_a_vec 
        end interface

        interface
            subroutine AQUABC_SEDIMENT_MODEL_1 &
               (nkn,INIT_SED_STATE_VARS, SED_DEPTHS , SED_POROSITIES,  &
                SED_DENSITIES          , PART_MIXING_COEFFS         ,  &
                SED_DIFFUSIONS         , SURF_MIXLEN, SED_BURRIALS  ,  &
                SURF_WATER_CONCS       , SED_TEMPS                  ,  &
                NUM_SED_VARS           , NUM_SED_LAYERS             ,  &
                SED_MODEL_CONSTANTS    , NUM_SED_CONSTS             ,  &
                SED_DRIVING_FUNCTIONS  , NUM_SED_DRIV               ,  & ! not used yet
                SED_FLAGS              , NUM_SED_FLAGS              ,  &
                FLUXES_TO_SEDIMENTS    , NUM_FLUXES_TO_SEDIMENTS    ,  &
                NUM_FLUX_RECEIVING_SED_LAYERS, ADVECTIVE_VELOCITY   ,  &
                PSTIME, TIME_STEP                                   ,  &
                H_ERODEP                                            ,  &
                FINAL_SED_STATE_VARS                                ,  &
                FLUXES_FROM_SEDIMENTS, NUM_FLUXES_FROM_SEDIMENTS    ,  &
                PROCESSES_sed        , NDIAGVAR_sed                 ,  &
                SED_OUTPUTS          , NUM_SED_OUTPUTS              ,  &
                SED_SAVED_OUTPUTS    , NUM_SED_SAVED_OUTPUTS        ,  &
                SED_BURRIAL_RATE_OUTPUTS, ADVANCED_REDOX_OPTION)

                integer NUM_SED_VARS
                integer NUM_SED_LAYERS
                integer NUM_SED_CONSTS
                integer NUM_SED_DRIV
                integer NUM_FLUXES_TO_SEDIMENTS   ! for BS state variables
                integer NUM_FLUXES_FROM_SEDIMENTS ! for BS state variables
                integer NDIAGVAR_sed
                integer NUM_FLUX_RECEIVING_SED_LAYERS
                integer NUM_SED_OUTPUTS
                integer NUM_SED_FLAGS
                integer NUM_SED_SAVED_OUTPUTS

                !INPUT ARGUMENTS
                integer nkn      ! number of reactors (nodes)
                double precision INIT_SED_STATE_VARS  (nkn,NUM_SED_LAYERS, NUM_SED_VARS)
                double precision SED_DEPTHS           (nkn,NUM_SED_LAYERS)
                double precision SED_POROSITIES       (nkn,NUM_SED_LAYERS)
                double precision SED_DENSITIES        (nkn,NUM_SED_LAYERS) !Bulk wet density
                double precision PART_MIXING_COEFFS   (nkn,NUM_SED_LAYERS, NUM_SED_VARS)
                double precision SED_DIFFUSIONS       (nkn,NUM_SED_LAYERS, NUM_SED_VARS)
                double precision SURF_MIXLEN
                double precision SED_BURRIALS         (nkn,NUM_SED_LAYERS)
                double precision SURF_WATER_CONCS     (nkn,NUM_SED_VARS)
                double precision SED_TEMPS            (nkn,NUM_SED_LAYERS) !Water temperature
                double precision SED_MODEL_CONSTANTS  (NUM_SED_CONSTS)
                double precision PROCESSES_sed        (nkn,NUM_SED_LAYERS, NUM_SED_VARS, NDIAGVAR_sed)
                double precision SED_DRIVING_FUNCTIONS(NUM_SED_LAYERS, NUM_SED_DRIV)
                double precision FLUXES_TO_SEDIMENTS  (nkn,NUM_FLUXES_TO_SEDIMENTS)
                double precision ADVECTIVE_VELOCITY
                double precision PSTIME
                double precision TIME_STEP
                double precision H_ERODEP             (nkn) ! Eroded(>0) or deposited(<0) thickness of first layer per time step, m

                !OUTPUT ARGUMENTS
                double precision FINAL_SED_STATE_VARS (nkn,NUM_SED_LAYERS, NUM_SED_VARS)
                double precision FLUXES_FROM_SEDIMENTS(nkn,NUM_FLUXES_FROM_SEDIMENTS)
                double precision SED_OUTPUTS          (nkn,NUM_SED_LAYERS, NUM_SED_OUTPUTS) ! it is assigned to other array
                                                                                            ! with indices order required by hydrodynamics
                double precision, dimension(nkn,NUM_SED_LAYERS, NUM_SED_SAVED_OUTPUTS)   :: SED_SAVED_OUTPUTS
                integer, dimension(NUM_SED_FLAGS) :: SED_FLAGS
                
                !Optional arguments
                double precision, optional, dimension(nkn, NUM_SED_LAYERS, NUM_SED_VARS) :: SED_BURRIAL_RATE_OUTPUTS
				integer                , intent(in), optional                     :: ADVANCED_REDOX_OPTION
            end subroutine AQUABC_SEDIMENT_MODEL_1
        end interface