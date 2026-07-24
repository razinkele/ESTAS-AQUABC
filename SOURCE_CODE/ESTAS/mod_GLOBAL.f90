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
    integer, parameter :: nconst                        = 323

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

    ! Number of distinct sediment profile types (e.g. 1=sandy, 2=muddy). Defaults to 1
    ! (legacy single-profile broadcast) when the input file has no # NUM_SED_TYPES header.
    integer :: NUM_SED_TYPES

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

    ! -----------------------------------------------------------------------------------
    ! Bottom-sediment submodel state (24 members) moved to the derived type `bsed`
    ! (type sediment_state_t) in module BOTTOM_SEDIMENTS — see
    ! docs/superpowers/specs/2026-07-23-sediment-state-derived-type-design.md.
    ! -----------------------------------------------------------------------------------
    ! End of variables for bottom sediment submodel

    ! -----------------------------------------------------------------------------------
    ! Particle settling/deposition + water<->sediment flux coupling state (11 members)
    ! moved to the derived type `wsc` (type wsc_state_t) in module WATER_SEDIMENT_COUPLING
    ! — see docs/superpowers/specs/2026-07-23-water-sediment-coupling-derived-type-design.md.
    ! -----------------------------------------------------------------------------------

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
    ! Sediment-resuspension / shear-stress state moved to the derived type `resusp`
    ! (type resuspension_t) in module RESUSPENSION — see
    ! docs/superpowers/specs/2026-07-22-resuspension-state-derived-type-design.md.
    ! -----------------------------------------------------------------------------------
    ! SHUT_DOWN_SETTLING is a *settling* control (not a resuspension variable), so it
    ! stays here in GLOBAL; it is referenced bare by mod_AQUATIC_MODEL and mod_SOLVER.
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
