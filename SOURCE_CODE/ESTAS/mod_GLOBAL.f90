!Module for keeping global definitions
module GLOBAL
    use TIME_SERIES
    use aquabc_pel_state_var_indexes
    use precision_kinds, only: DBL
    implicit none

    ! -------------------------------------------------------------------------
    ! AQUABC Model dimensions
    ! -------------------------------------------------------------------------

    ! Water column
    integer            :: nkn

    ! Number of water column state variables must be 32
    integer, parameter :: nstate                        = 32


    ! Number of pelagic model constants
    integer, parameter :: nconst                        = 318

    ! Number of driving functions must be 10
    integer, parameter :: n_driving_functions           = 10

    ! Number of flags must be 5
    integer, parameter :: nflags                        = 5

    ! Number of saved outputs not less 5
    integer, parameter :: n_saved_outputs               = 5

    ! Not less than 30
    integer, parameter :: NDIAGVAR                      = 30

    ! Sediments

    ! Number of sediment state variables (must be 24 for this model)
    integer, parameter :: NUM_SED_VARS                  = 24

    ! Number of sediment model constants (must be 171 for this model)
    integer, parameter :: NUM_SED_CONSTS                = 171

    ! Number of sediment model driving constants ()
    integer, parameter :: NUM_SED_DRIV                  = 1

    ! Number of variables representing flux to sediments
    integer, parameter :: NUM_FLUXES_TO_SEDIMENTS       = 24

    ! Number of variables representing flux from
    integer, parameter :: NUM_FLUXES_FROM_SEDIMENTS     = 30

    ! Number of sediment diagnostics state variables
    integer, parameter :: NDIAGVAR_sed                  = 25

    ! Number of sediment outputs (must be 26 for this model)
    integer, parameter :: NUM_SED_OUTPUTS               = 26

    ! Number of sediment model flags (must be 3)
    integer, parameter :: NUM_SED_FLAGS                 = 3

    ! Number of sediment saved outputs
    integer, parameter :: NUM_SED_SAVED_OUTPUTS         = 5
    ! -------------------------------------------------------------------------
    ! End of AQUABC Model dimensions
    ! -------------------------------------------------------------------------

    ! Switch to indicate if the bottom sediments will be modelled or not
    integer :: MODEL_BOTTOM_SEDIMENTS

    ! Number of prescribed sediment flux sets. Only active if bottom sediments
    ! are modelled.
    integer :: NUM_PRESCRIBED_SEDIMENT_FLUX_SETS

    ! Number of sediment layers (make it user enterable)
    integer :: NUM_SED_LAYERS

    ! Switch to indicate if the effect of the bentic animals on bottom sediments
    ! will be modelled or not
    integer :: MODEL_BENTHIC_ANIMALS

    ! Switch to indcate if the COCOA outputs will be produced or not. These
    ! outputs were specific to COCOA (Nutrient COcktail in COAstal zones
    ! of the Baltic Sea) projects, however they were considered as of
    ! general interest
    integer :: PRODUCE_COCOA_OUTPUTS

    integer(4) :: TIME_STEP_NO

    ! Variables for water column kinetics submodel

    ! Input  : Active nodes for diagnostic
    ! (not implmented but must be used for interface compatibility)
    integer              , allocatable, dimension(:) :: node_active

    !Input  : Vector of state variables
    real(kind = DBL), allocatable, dimension(:,:)    :: STATE_VARIABLES

    !Output : Time derivatives of state variables
    real(kind = DBL), allocatable, dimension(:,:)    :: DERIVATIVES

    !Input  : Vector of kinetic and stochiometric model constants.
    !See subroutine INIT_WC_MODEL_CONSTANTS
    real(kind = DBL), allocatable, dimension(:)      :: MODEL_CONSTANTS

    !Input  : Values for environmental conditions.
    real(kind = DBL), allocatable, dimension(:,:)    :: DRIVING_FUNCTIONS

    !Input  : Values for flags.
    integer         , allocatable, dimension(:)      :: FLAGS

    !Output : Diagnostic variables documented in PELAGIC_KINETICS
    real(kind = DBL), allocatable, dimension(:,:,:)  :: PROCESS_RATES

    !Input and Output : Any output that should be saved for the next time step
    real(kind = DBL), allocatable, dimension(:,:)    :: SAVED_OUTPUTS

    !Old style PH array  must be used for interface compatibility
    real(kind = DBL), allocatable, dimension(:)      :: pH

    integer :: CALLED_BEFORE

    !Output : Chl-A as a useful derived variable
    real(kind = DBL), allocatable, dimension(:)     :: CHLA

    real(kind = DBL), allocatable, dimension(:, :)  :: WATER_COLUMN_OUTPUT

    integer, allocatable, dimension(:) :: SURFACE_BOXES
    ! End of variables for water column kinetics submodel

    ! Variables for bottom sediment submodel

    !Input  : Initial values of sediment state variables
    real(kind=DBL), allocatable, dimension(:, :, :)    :: INIT_SED_STATE_VARS

    !Input  : Sediment depths
    real(kind=DBL), allocatable, dimension(:, :)       :: SED_DEPTHS

    !Input  : Sediment porosities
    real(kind=DBL), allocatable, dimension(:, :)       :: SED_POROSITIES

    !Input  : Bulk wet density
    real(kind=DBL), allocatable, dimension(:, :)       :: SED_DENSITIES

    !Input  : Particle mixing coeff
    real(kind=DBL), allocatable, dimension(:, :, :)    :: PART_MIXING_COEFFS

    !Input  : Sediment diffusions
    real(kind=DBL), allocatable, dimension(:, :, :)    :: SED_DIFFUSIONS

    !Input  : Mixing length with surface water
    real(kind=DBL)                                     :: SURF_MIXLEN

    !Input  : Sediment burrial rate
    real(kind=DBL), allocatable, dimension(:, :)       :: SED_BURRIALS

    !Input  : Surface water concentration of sediment state variable
    real(kind=DBL), allocatable, dimension(:, :)       :: SURF_WATER_CONCS

    !Input  : Sediment temperatures (take the same as water temperature)
    real(kind=DBL), allocatable, dimension(:, :)       :: SED_TEMPS

    !Input  : Model constants for sediment model
    real(kind=DBL), allocatable, dimension(:)          :: SED_MODEL_CONSTANTS

    !Input  : Output : Diagnostic variables documented in sediments
    real(kind=DBL), allocatable, dimension(:, :, :, :) :: PROCESSES_sed

    !Input  : Driving functions in sediments
    real(kind=DBL), allocatable, dimension(:, :)       :: SED_DRIVING_FUNCTIONS

    !Input  : Fluxes to sediments from water column by settling/deposition
    real(kind=DBL), allocatable, dimension(:, :)       :: FLUXES_TO_SEDIMENTS

    !Input  : Advective velocity (make it later array of nkn)
    real(kind=DBL)                                     :: ADVECTIVE_VELOCITY

    !Input  : Eroded(>0) or deposited(<0) thickness of first layer per time step, m
    real(kind=DBL), allocatable, dimension(:)          :: H_ERODEP

    !Input  : Sediment model flag
    integer        , allocatable, dimension(:)         :: SED_FLAGS

    !Input  : Number of sediment layers receiving flux from water column
    !         through settling and deposition
    integer                                            :: NUM_FLUX_RECEIVING_SED_LAYERS

    !Output : Final values of sediment state variables
    real(kind=DBL), allocatable, dimension(:, :, :)    :: FINAL_SED_STATE_VARS

    !Output : Fluxes from sediments into the water column
    real(kind=DBL), allocatable, dimension(:, :)       :: FLUXES_FROM_SEDIMENTS

    !Output : Additional output required for SHYFEM compatibility
    real(kind=DBL), allocatable, dimension(:, :, :)    :: SED_OUTPUTS

    !Output : Saved outputs required by sediment model
    real(kind=DBL), allocatable, dimension(:, :, :)    :: SED_SAVED_OUTPUTS

    !Output : For COCOA process rate outputs. Optional argument for sediment model.
    real(kind=DBL), allocatable, dimension(:, :, :)    :: SED_BURRIAL_RATE_OUTPUTS
    ! End of variables for bottom sediment submodel

    ! Variables for water column - bottom sediment interaction
    real(kind = DBL), allocatable, dimension(:, :) :: FLUXES_TO_WATER_COLUMN
    real(kind = DBL), allocatable, dimension(:, :) :: FLUXES_OUTPUT_TO_WATER_COLUMN
    ! End of variables for water column - bottom sediment interaction

    real(kind = DBL), allocatable, dimension(:, :) :: DISSOLVED_FRACTIONS
    real(kind = DBL), allocatable, dimension(:, :) :: FRACTION_OF_DEPOSITION
    real(kind = DBL), allocatable, dimension(:, :) :: SETTLING_RATES
    real(kind = DBL), allocatable, dimension(:, :) :: NOT_DEPOSITED_FLUXES
    real(kind = DBL), allocatable, dimension(:, :) :: FLUXES

    real(kind = DBL), allocatable, dimension(:, :) :: SETTLING_VELOCITIES_OUTPUT
    real(kind = DBL), allocatable, dimension(:, :) :: EFFECTIVE_DISSLOVED_FRACTIONS
    real(kind = DBL), allocatable, dimension(:, :) :: EFFECTIVE_DEPOSITION_FRACTIONS
    real(kind = DBL), allocatable, dimension(:, :) :: DEPOSITION_AREA_RATIOS

    character(len = 2048) :: COCOA_PELAGIC_OUTPUTS_FILENAME

    character(len = 2048) :: BOTTOM_SEDIMENT_CONCENTRATIONS_FILENAME
    character(len = 2048) :: BOTTOM_SEDIMENT_FLUXES_FILENAME
    character(len = 2048) :: COCOA_SEDIMENT_PROCESS_RATES_FILENAME
    character(len = 2048) :: COCOA_SEDIMENT_BURIAL_RATES_FILENAME
    character(len = 2048) :: COCOA_FLUXES_FROM_SEDIMENTS_FILENAME
    character(len = 2048) :: COCOA_FLUXES_TO_SEDIMENTS_FILENAME

    integer :: USE_PELAGIC_CONSTANTS_FILE_NAME
    character(len = 2048) :: PELAGIC_CONSTANTS_FILE_NAME

    integer :: PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT
    character(len = 2048) :: BINARY_PELAGIC_OUTPUT_FILE_NAME

    integer :: AUX_OUTPUT_UNIT
    real(kind = DBL) :: INIT_TIME

    character(len = 2048) :: PELAGIC_INPUT_FOLDER
    character(len = 2048) :: PELAGIC_OUTPUT_FOLDER

    integer :: BOTTOM_SED_ADVANCED_REDOX_SIMULATION
    real(kind = DBL) :: USER_ENTERED_frac_avail_DON
    real(kind = DBL) :: USER_ENTERED_K_B_E

    ! -----------------------------------------------------------------------------------
    ! Variables related to sediment resuspension
    ! -----------------------------------------------------------------------------------
    integer :: NUM_RESUSPENSION_TS
    integer :: RESUSPENSION_OPTION
    integer :: CONSIDER_RESUSPENSION
    integer         , allocatable, dimension(:)      :: ACTIVATE_RESUSPENSIONS
    real(kind = DBL), allocatable, dimension(:)      :: FRAC_RESUSPENSION_AREAS
    integer         , allocatable, dimension(:, :)   :: RESUSPENSION_CONC_TS_NOS
    integer         , allocatable, dimension(:, :)   :: RESUSPENSION_CONC_TS_VAR_NOS
    integer         , allocatable, dimension(:)      :: RESUSPENSION_VEL_TS_NOS
    integer         , allocatable, dimension(:)      :: RESUSPENSION_VEL_TS_VAR_NOS
    character(len = 2048)                            :: RESUSPENSION_INPUT_FILE_NAME
    character(len = 2048), allocatable, dimension(:) :: RESUSPENSION_TS_FILE_NAMES
    type(TIME_SERIE)     , allocatable, dimension(:) :: RESUSPENSION_TS
    character(len = 2048)                            :: RESUSPENSION_INPUT_FOLDER
    character(len = 2048)                            :: RESUSPENSION_OUTPUT_FOLDER
    real(kind = DBL), allocatable, dimension(:)      :: BOX_CRITICAL_SHEAR_STRESSES
    character(len = 2048)                            :: CRITICAL_SHEAR_STRESS_FILENAME
    integer                                          :: CRIT_SHEAR_FNAME_FROM_OUTSIDE
    integer         , allocatable, dimension(:)      :: SHEAR_STRESS_TS_NOS
    integer         , allocatable, dimension(:)      :: SHEAR_STRESS_TS_VAR_NOS
    integer                                          :: SHUT_DOWN_SETTLING
    ! -----------------------------------------------------------------------------------
    ! End of variables related to sediment resuspension
    ! -----------------------------------------------------------------------------------

    !Pelagic process rate output option
    ! 0: Based on volumes - g/m^3/day^-1
    ! 1: Based on areas   - g/m23/day^-1
    integer :: PEL_PROCESS_RATE_OUTPUT_OPTION

    !PELAGIC OPTIONS
    integer :: ZOOPLANKTON_OPTION
    integer :: ADVANCED_REDOX_SIMULATION
    integer :: LIGHT_EXTINCTION_OPTION
    integer :: CYANO_BOUYANT_STATE_SIMULATION
    integer :: CONSIDER_NON_OBLIGATORY_FIXERS
    integer :: CONSIDER_NOSTOCALES
    integer :: CONSIDER_ALLELOPATHY

    integer, parameter :: NUM_ALLOLOPATHY_STATE_VARS = 4
end module GLOBAL
