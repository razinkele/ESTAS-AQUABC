
! -----------------------------------------------------------------------------
! This is a simplified interface to call the AQUABC model
! -----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
!
! callable routines:
!
!   subroutine aquabc_init	            ( &
!			nkn                 , & ! number of boxes
!			nstate              , & ! number of state vars (32)
!			n_driving_functions , & ! number of forcings (10)
!			SURFACE_BOXES       )   ! is surface box?
!
!   subroutine aquabc_init_flags            ( &
!   			nflags              , & ! number of flags (must be 12)
!   			flags               )	! flags for model
!
!   subroutine aquabc_run                   ( &
!                       time                , &	! simulation time [days]
!                       time_step           , & ! time step [days]
!			STATE_VARIABLES     , & ! state variables [mg/L]
!			PH                  , & ! ph [1-14]
!			DRIVING_FUNCTIONS   , & ! forcings (see documentation)
!			SEDIMENT_FLUXES     )  	! sediment fluxes [??]
!
!   subroutine aquabc_read_constants(file)	! reads constants file
!   subroutine aquabc_write_constants(file)	! writes constants file
!
! -----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
!
! calling sequence:
!
!    call aquabc_init
!    call aquabc_init_flags (optional, changes default flag values)
!    call aquabc_read_constants (optional, changes default constants values)
!
!    do i=1,ntime_steps
!      time = time + time_step
!      call aquabc_run
!    end do
!
! -----------------------------------------------------------------------------

module aquabc_II_pelagic_interface

    implicit none

    integer, save :: nkn

    ! These values must match the parameters in mod_GLOBAL.f90, except nflags
    ! which is 12 at the interface level (GLOBAL has nflags = 5 for kinetics only;
    ! the interface packs additional options into flags 6-12).
    integer, save :: nstate              = 32
    integer, save :: nconst              = 318
    integer, save :: n_driving_functions = 10
    integer, save :: nflags              = 12
    integer, save :: n_saved_outputs     = 5	! was 4 -> bug (ggu)
    integer, save :: NDIAGVAR            = 30

    logical, save :: binitialized = .false.
    logical, save :: bfirstcall = .true.
    integer, save :: CALLED_BEFORE = 0
    double precision, save :: TIME_FIRST = -1.	! first time of call

    integer, save :: FLAGS(12)

    integer, save :: ZOOP_OPTION_1
    integer, save :: ADVANCED_REDOX_SIMULATION
    integer, save :: LIGHT_EXTINCTION_OPTION
    integer, save :: CYANO_BOUYANT_STATE_SIMULATION
    integer, save :: CONSIDER_NON_OBLIGATORY_FIXERS
    integer, save :: CONSIDER_NOSTOCALES
    double precision, save :: USER_ENTERED_frac_avail_DON

    integer         , save, allocatable, dimension(:)     :: node_active
    double precision, save, allocatable, dimension(:,:)   :: DERIVATIVES
    double precision, save, allocatable, dimension(:)     :: MODEL_CONSTANTS
    double precision, save, allocatable, dimension(:,:,:) :: PROCESS_RATES
    double precision, save, allocatable, dimension(:,:)   :: SAVED_OUTPUTS
    integer         , save, allocatable, dimension(:)     :: SURFACE_BOXES

end module aquabc_II_pelagic_interface

!==================================================================

subroutine aquabc_init		              ( &
			nkn_g                 , & ! number of boxes
			nstate_g              , & ! number of state vars (32)
			n_driving_functions_g , & ! number of forcings (10)
			SURFACE_BOXES_g       )   ! is surface box?

! initialize system - must be called first
!
! this routine must be called before any other routine

    use aquabc_II_pelagic_interface

    implicit none

    integer, intent(in) :: nkn_g                   ! number of boxes
    integer, intent(in) :: nstate_g                ! number of state variables (must be 32)
    integer, intent(in) :: n_driving_functions_g   ! number of driving functions (forcings, 10)
    integer, intent(in) :: SURFACE_BOXES_g(nkn_g)  ! indicates box is on surface

    ! -------------------------------------------------------------------------
    ! Initial checks
    ! -------------------------------------------------------------------------

    nkn = nkn_g

    if( nstate_g /= nstate ) then
	write(6,*) 'nstate given is ',nstate_g
	write(6,*) 'nstate must be  ',nstate
	stop 'error stop: nstate_g /= nstate'
    end if

    if( n_driving_functions_g /= n_driving_functions ) then
	write(6,*) 'n_driving_functions given is ',n_driving_functions_g
	write(6,*) 'n_driving_functions must be  ',n_driving_functions
	stop 'error stop: n_driving_functions_g /= n_driving_functions'
    end if

    binitialized = .true.

    ! -------------------------------------------------------------------------
    ! The following flags initialize the options to run AQUABC pelagic module
    ! -------------------------------------------------------------------------

    ZOOP_OPTION_1                  = 1  !Use the variable stochiometry option
    ADVANCED_REDOX_SIMULATION      = 0  !Use the advanced redox simulation opt.
    LIGHT_EXTINCTION_OPTION        = 1  !Use the general light extinction opt.
    CYANO_BOUYANT_STATE_SIMULATION = 0  !Allow for cyanobacteria bouyant state
    CONSIDER_NON_OBLIGATORY_FIXERS = 1  !Consider the non fixing cyanobacteria
    CONSIDER_NOSTOCALES            = 0  !Consider nostocales
    USER_ENTERED_frac_avail_DON    = 0.15 !(probably not important)

    FLAGS(1)  = 1	! safe mode - not used
    FLAGS(2)  = 1	! not used
    FLAGS(3)  = 1	! first time called?
    FLAGS(4)  = 1	! use initial conditions Fe2+
    FLAGS(5)  = 1	! use initial conditions Fe3+
    FLAGS(6)  = ZOOP_OPTION_1
    FLAGS(7)  = ADVANCED_REDOX_SIMULATION
    FLAGS(8)  = LIGHT_EXTINCTION_OPTION
    FLAGS(9)  = CYANO_BOUYANT_STATE_SIMULATION
    FLAGS(10) = CONSIDER_NON_OBLIGATORY_FIXERS
    FLAGS(11) = CONSIDER_NOSTOCALES
    FLAGS(12) = nint(USER_ENTERED_frac_avail_DON * 100.0D0)

    ! -------------------------------------------------------------------------
    ! Allocate the arrays
    ! -------------------------------------------------------------------------

    allocate(node_active      (nkn))
    allocate(DERIVATIVES      (nkn,nstate))
    allocate(MODEL_CONSTANTS  (nconst))
    allocate(PROCESS_RATES    (nkn,nstate,NDIAGVAR))
    allocate(SAVED_OUTPUTS    (nkn,n_saved_outputs))
    allocate(SURFACE_BOXES    (nkn))

    node_active       = 1
    DERIVATIVES       = 0.0D0
    MODEL_CONSTANTS   = 0.0D0
    PROCESS_RATES     = 0.0D0
    SAVED_OUTPUTS     = 0.0D0

    SURFACE_BOXES(:) = SURFACE_BOXES_g(:)	!store for later

    call DEFAULT_PELAGIC_MODEL_CONSTANTS	!sets vars with default values
    call INSERT_PELAGIC_MODEL_CONSTANTS		!insert vars into array

end subroutine

!==================================================================

subroutine aquabc_init_flags		    ( &
				nflags_g    , & ! number of flags (must be 12)
				flags_g     )	! flags for model

    use aquabc_II_pelagic_interface

! initialize flags if needed (do not use default values)
!
! this routine must be called after aquabc_init but before aquabc_run
! meaning of flags is given above in subroutine aquabc_init
! value for USER_ENTERED_frac_avail_DON (flags_g(12)) should be given in %

    implicit none

    integer, intent(in) :: nflags_g
    integer, intent(in) :: flags_g(nflags_g)

    integer, parameter :: nflags_l = 12

    if( nflags_g /= nflags_l ) then
	write(6,*) 'nflags given is ',nflags_g
	write(6,*) 'nflags must be  ',nflags_l
	stop 'error stop: nflags_g /= nflags_l'
    end if

    if( .not. binitialized ) then
        write(6,*) 'AQUABC is not initialized'
        write(6,*) 'must call aquabc_init first'
        stop 'error stop aquabc_init_flags: AQUABC not initialized'
    end if

    FLAGS(1:5) = flags_g(1:5)

    ZOOP_OPTION_1                  = flags_g(6)
    ADVANCED_REDOX_SIMULATION      = flags_g(7)
    LIGHT_EXTINCTION_OPTION        = flags_g(8)
    CYANO_BOUYANT_STATE_SIMULATION = flags_g(9)
    CONSIDER_NON_OBLIGATORY_FIXERS = flags_g(10)
    CONSIDER_NOSTOCALES            = flags_g(11)
    USER_ENTERED_frac_avail_DON    = flags_g(12) / 100.

    FLAGS(3) = 1

end subroutine

!==================================================================

subroutine aquabc_run                       ( &
                        time                , &	! simulation time [days]
                        time_step           , & ! time step [days]
			STATE_VARIABLES     , & ! state variables [mg/L]
			PH                  , & ! ph [1-14]
			DRIVING_FUNCTIONS   , & ! forcings (see below)
			SEDIMENT_FLUXES     )  	! sediment fluxes [??]

! runs the ACQUABC model
!
! aquabc_init must be called before this routine can be called

    use aquabc_II_pelagic_interface

    implicit none

    double precision :: time
    double precision :: time_step
    double precision :: STATE_VARIABLES      (nkn,nstate)
    double precision :: PH                   (nkn)
    double precision :: DRIVING_FUNCTIONS    (nkn,n_driving_functions)
    double precision :: SEDIMENT_FLUXES      (nkn,nstate)

    integer :: DAY_OF_YEAR		! julian day

	if( .not. binitialized ) then
	    write(6,*) 'AQUABC is not initialized'
	    write(6,*) 'must call aquabc_init first'
	    stop 'error stop aquabc_run: AQUABC not initialized'
	end if

        DAY_OF_YEAR   = 1 + mod(int(TIME)-1,365)

	if( bfirstcall ) then
	  bfirstcall = .false.
	  TIME_FIRST = TIME
	end if
	if( CALLED_BEFORE == 0 .and. TIME /= TIME_FIRST ) then
          CALLED_BEFORE = 1
          FLAGS(3)      = 0
	end if

        call AQUABC_PELAGIC_KINETICS &
            (node_active                   , & 	! debug variable, not used
             nkn                           , &	! number of boxes
             STATE_VARIABLES               , &  ! state_variables [mg/L]
             DERIVATIVES                   , &	! state_variables/time (out)
             nstate                        , &	! total number of state vars
             MODEL_CONSTANTS               , &  ! process constants, not used
             nconst                        , &	! total number of constants
             DRIVING_FUNCTIONS             , &	! external forcings
             n_driving_functions           , &	! total number of ext forc
             FLAGS                         , &	! flags (see above)
             nflags                        , &	! total number of flags
             PROCESS_RATES                 , &	! process rates for STV (out)
             NDIAGVAR                      , &	! total number of process rates
             SAVED_OUTPUTS                 , &	! values for next call (in/out)
             n_saved_outputs               , &	! total values needed next call
             PH                            , &	! [1-14] (in/out)
             TIME                          , &	! [days]
             TIME_STEP                     , &	! [days]
             DAY_OF_YEAR                   , &	! [julian day]
             SEDIMENT_FLUXES               , &	! imposed sedim fluxes [??]
             CALLED_BEFORE                 , &	! has been called before?
             SURFACE_BOXES                 , &	! box is on surface?
             ZOOP_OPTION_1                 , &	! next are options
             ADVANCED_REDOX_SIMULATION     , &
             USER_ENTERED_frac_avail_DON   , &
             LIGHT_EXTINCTION_OPTION       , &
             CYANO_BOUYANT_STATE_SIMULATION, &
             CONSIDER_NON_OBLIGATORY_FIXERS, &
             CONSIDER_NOSTOCALES)

        STATE_VARIABLES = STATE_VARIABLES + DERIVATIVES * TIME_STEP

end subroutine

!==================================================================

subroutine aquabc_read_constants(file)

    use aquabc_II_pelagic_interface

    implicit none

    character*(*) file	! file containing constants

    if( .not. binitialized ) then
      write(6,*) 'model has not been initialized.'
      write(6,*) 'must call aquabc_init before.'
      stop 'error stop aquabc_read_constants: not initialized'
    end if

    call READ_PELAGIC_MODEL_CONSTANTS(file)	!reads from file into array
    call INIT_PELAGIC_MODEL_CONSTANTS		!initializes vars from array

end subroutine

!==================================================================

subroutine aquabc_write_constants(file)

    use aquabc_II_pelagic_interface

    implicit none

    character*(*) file	! file containing constants

    if( .not. binitialized ) then
      write(6,*) 'model has not been initialized.'
      write(6,*) 'must call aquabc_init before.'
      stop 'error stop aquabc_write_constants: not initialized'
    end if

    call WRITE_PELAGIC_MODEL_CONSTANTS(file)

end subroutine

!==================================================================

