
! ----------------------------------------------------------------------------- 
! This is the very basic examples for a pelagic model setup, one
! box only. All of the external forcings are fixed in time, so
! if compiled correctly, all of the state variables should 
! reach a steady state.
! ----------------------------------------------------------------------------- 

program aquabc_II_pelagic_0D

    implicit none
    
    integer :: nkn
    integer :: nstate
    integer :: nconst
    integer :: n_driving_functions
    integer :: nflags
    integer :: n_saved_outputs
    integer :: NDIAGVAR

    double precision :: TIME		! time of simulation [days]
    double precision :: TIME_STEP	! time step of simulation [days]
    
    double precision, allocatable, dimension(:,:)   :: STATE_VARIABLES
    double precision, allocatable, dimension(:,:)   :: STATE_VARIABLES_OLD
    double precision, allocatable, dimension(:,:)   :: DRIVING_FUNCTIONS
    double precision, allocatable, dimension(:)     :: PH
    double precision, allocatable, dimension(:,:)   :: SEDIMENT_FLUXES
    integer         , allocatable, dimension(:)     :: SURFACE_BOXES    

    integer :: SIMULATION_TIME_IN_DAYS
    integer :: NUM_TIME_STEPS_PER_DAY
    integer :: NUM_TIME_STEPS
    integer :: i
    
    double precision, allocatable, dimension(:,:) :: VOLUMES
    double precision, allocatable, dimension(:,:) :: FLOW_RATES
    double precision, allocatable, dimension(:,:) :: BOUNDARY_CONCENTRATIONS
    double precision :: VOLUME
    double precision :: FLOW_RATE

    double precision :: val
    double precision getpar
    
    ! This is a 0D model, so the number of boxes should be 1

    nkn = 1
    !nkn = 10
    
    ! ------------------------------------------------------------------------- 
    ! The following are the array dimensions as necessary in order to run
    ! AQUABC pelagic model properly
    ! ------------------------------------------------------------------------- 
    nstate              = 32
    n_driving_functions = 10
    ! ------------------------------------------------------------------------- 

    ! -------------------------------------------------------------------------
    ! Initialize the time related information
    ! -------------------------------------------------------------------------
    SIMULATION_TIME_IN_DAYS = 3 * 365 ! three years
    NUM_TIME_STEPS_PER_DAY  = 24
    NUM_TIME_STEPS          = NUM_TIME_STEPS_PER_DAY * SIMULATION_TIME_IN_DAYS
    TIME                    = 1.0D0
    TIME_STEP               = 1.0D0 / NUM_TIME_STEPS_PER_DAY
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Allocate the arrays
    ! -------------------------------------------------------------------------
    allocate(STATE_VARIABLES      (nkn,nstate))
    allocate(STATE_VARIABLES_OLD  (nkn,nstate))
    allocate(DRIVING_FUNCTIONS    (nkn,n_driving_functions))
    allocate(PH                   (nkn))
    allocate(SEDIMENT_FLUXES      (nkn,nstate))
    allocate(SURFACE_BOXES        (nkn))
    
    allocate(VOLUMES(nkn,nstate))
    allocate(FLOW_RATES(nkn,nstate))
    allocate(BOUNDARY_CONCENTRATIONS(nkn,nstate))
    ! -------------------------------------------------------------------------
    
    STATE_VARIABLES       = 0.0D0
    STATE_VARIABLES_OLD   = 0.0D0
    DRIVING_FUNCTIONS     = 0.0D0
    PH                    = 0.0D0
    SEDIMENT_FLUXES       = 0.0D0
    SURFACE_BOXES         = 1

    VOLUME    = 6200000000.0D0
    FLOW_RATE = 700.0D0 * 86400.0D0
    
    ! -------------------------------------------------------------------------
    ! Initial conditions
    ! -------------------------------------------------------------------------
    STATE_VARIABLES  (:, 1) = 0.05   ! AMMONIUM NITROGEN
    STATE_VARIABLES  (:, 2) = 0.50   ! NITRATE NITROGEN
    STATE_VARIABLES  (:, 3) = 0.047  ! ORTHOPHOSPHATE PHOSPHORUS
    STATE_VARIABLES  (:, 4) = 14.0   ! DISSOLVED OXYGEN
    STATE_VARIABLES  (:, 5) = 1.75   ! DIATOMS CARBON
    STATE_VARIABLES  (:, 6) = 0.020  ! ZOOPLANKTON CARBON
    STATE_VARIABLES  (:, 7) = 0.000  ! ZOOPLANKTON NITROGEN
    STATE_VARIABLES  (:, 8) = 0.0000 ! ZOOPLANKTON PHOSPHORUS
    STATE_VARIABLES  (:, 9) = 6.5    ! DETRITUS PARTICULATE ORG. CARBON
    STATE_VARIABLES  (:,10) = 1.0    ! DETRITUS PARTICULATE ORG. NITROGEN
    STATE_VARIABLES  (:,11) = 0.015  ! DETRITUS PARTICULATE ORG. PHOSPHORUS
    STATE_VARIABLES  (:,12) = 8.0    ! DISSOLVED ORGANIC CARBON
    STATE_VARIABLES  (:,13) = 1.5    ! DISSOLVED ORGANIC NITROGEN
    STATE_VARIABLES  (:,14) = 0.025  ! DISSOLVED ORGANIC PHOSPHORUS
    STATE_VARIABLES  (:,15) = 0.06   ! NON FIXING CYANOBACTERIA CARBON
    STATE_VARIABLES  (:,16) = 0.022  ! OTHER PHYTOPLANKTON CARBON
    STATE_VARIABLES  (:,17) = 3.0    ! DISSOLOVED SILICA
    STATE_VARIABLES  (:,18) = 1.5    ! PARTICULATE SILICA
    STATE_VARIABLES  (:,19) = 0.00   ! FIXING CYANOBACTERIA CARBON
    STATE_VARIABLES  (:,20) = 0.0027 ! INORG CARBON CARBON
    STATE_VARIABLES  (:,21) = 0.0027 ! TOTAL ALKALNITY
    STATE_VARIABLES  (:,22) = 0.23   ! Iron charged as plus 2
    STATE_VARIABLES  (:,23) = 0.55   ! Iron chargen as plus 3
    STATE_VARIABLES  (:,24) = 0.1    ! Manganese charged as plus 2
    STATE_VARIABLES  (:,25) = 0.1    ! Manganese charged as plus 4
    STATE_VARIABLES  (:,26) = 70.    ! Calcium
    STATE_VARIABLES  (:,27) = 15.    ! Magnesium
    STATE_VARIABLES  (:,28) = 1.35   ! Sulphur 6+ (Sulphate sulphur)
    STATE_VARIABLES  (:,29) = 0.0    ! Sulphur 2- (Sulphide sulphur)
    STATE_VARIABLES  (:,30) = 0.0    ! Methane carbon
    STATE_VARIABLES  (:,31) = 0.0    ! Nostocales
    STATE_VARIABLES  (:,32) = 8.0    ! Akinetes g/m^2
    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    ! Boundary conditions assuming fixed values in time.
    ! These numbers are fake. The intention is just to test the model.
    ! -------------------------------------------------------------------------
    BOUNDARY_CONCENTRATIONS(:, 1) =  0.0500   ! AMMONIUM NITROGEN
    BOUNDARY_CONCENTRATIONS(:, 2) =  1.0000   ! NITRATE NITROGEN
    BOUNDARY_CONCENTRATIONS(:, 3) =  0.0400   ! ORTHOPHOSPHATE PHOSPHORUS
    BOUNDARY_CONCENTRATIONS(:, 4) = 10.0000   ! DISSOLVED OXYGEN
    BOUNDARY_CONCENTRATIONS(:, 5) =  0.2000   ! DIATOMS CARBON
    BOUNDARY_CONCENTRATIONS(:, 6) =  0.0400   ! ZOOPLANKTON CARBON
    BOUNDARY_CONCENTRATIONS(:, 7) =  0.0100   ! ZOOPLANKTON NITROGEN
    BOUNDARY_CONCENTRATIONS(:, 8) =  0.0020   ! ZOOPLANKTON PHOSPHORUS
    BOUNDARY_CONCENTRATIONS(:, 9) =  3.5000   ! DETRITUS PARTICULATE ORG. C
    BOUNDARY_CONCENTRATIONS(:,10) =  0.3000   ! DETRITUS PARTICULATE ORG. N
    BOUNDARY_CONCENTRATIONS(:,11) =  0.0500   ! DETRITUS PARTICULATE ORG. P
    BOUNDARY_CONCENTRATIONS(:,12) =  5.0000   ! DISSOLVED ORGANIC CARBON
    BOUNDARY_CONCENTRATIONS(:,13) =  0.5000   ! DISSOLVED ORGANIC NITROGEN
    BOUNDARY_CONCENTRATIONS(:,14) =  0.0500   ! DISSOLVED ORGANIC PHOSPHORUS
    BOUNDARY_CONCENTRATIONS(:,15) =  0.0100   ! NON FIXING CYANOBACTERIA CARBON
    BOUNDARY_CONCENTRATIONS(:,16) =  0.0200   ! OTHER PHYTOPLANKTON CARBON
    BOUNDARY_CONCENTRATIONS(:,17) =  3.0000   ! DISSOLOVED SILICA
    BOUNDARY_CONCENTRATIONS(:,18) =  1.5000   ! PARTICULATE SILICA
    BOUNDARY_CONCENTRATIONS(:,19) =  0.0001   ! FIXING CYANOBACTERIA CARBON
    BOUNDARY_CONCENTRATIONS(:,20) =  0.0027   ! INORG CARBON CARBON
    BOUNDARY_CONCENTRATIONS(:,21) =  0.0027   ! TOTAL ALKALNITY
    BOUNDARY_CONCENTRATIONS(:,22) =  0.2300   ! Iron charged as plus 2
    BOUNDARY_CONCENTRATIONS(:,23) =  0.0500   ! Iron chargen as plus 3
    BOUNDARY_CONCENTRATIONS(:,24) =  0.1000   ! Manganese charged as plus 2
    BOUNDARY_CONCENTRATIONS(:,25) =  0.1000   ! Manganese charged as plus 4
    BOUNDARY_CONCENTRATIONS(:,26) = 70.0000   ! Calcium
    BOUNDARY_CONCENTRATIONS(:,27) = 15.0000   ! Magnesium
    BOUNDARY_CONCENTRATIONS(:,28) =  1.3500   ! Sulphur 6+ (Sulphate sulphur)
    BOUNDARY_CONCENTRATIONS(:,29) =  0.0000   ! Sulphur 2- (Sulphide sulphur)
    BOUNDARY_CONCENTRATIONS(:,30) =  0.0000   ! Methane carbon
    BOUNDARY_CONCENTRATIONS(:,31) =  0.0000   ! Nostocales
    BOUNDARY_CONCENTRATIONS(:,32) =  0.0001   ! Akinetes g/m^2
    ! -------------------------------------------------------------------------
    
    ! -------------------------------------------------------------------------
    ! Driving functions assumed being fixed in time
    ! -------------------------------------------------------------------------
    DRIVING_FUNCTIONS(:,1 ) =   14.0D0 ! Water temperature [C]
    DRIVING_FUNCTIONS(:,2 ) =    2.0D0 ! Salinity [psu]
    DRIVING_FUNCTIONS(:,3 ) =  200.0D0 ! Available light [W/m**2]
    DRIVING_FUNCTIONS(:,4 ) =    0.5D0 ! Fraction of day [0-1]
    DRIVING_FUNCTIONS(:,5 ) =   12.0D0 ! Air temperature [C]
    DRIVING_FUNCTIONS(:,6 ) =    3.0D0 ! Wind speed [m/s]
    DRIVING_FUNCTIONS(:,7 ) =    2.0D0 ! Elevation [m]
    DRIVING_FUNCTIONS(:,8 ) =    3.6D0 ! Depth [m]
    DRIVING_FUNCTIONS(:,9 ) =    0.1D0 ! Background light extinction [1/m]
    DRIVING_FUNCTIONS(:,10) =    0.0D0 ! Ice cover [0-1]
    ! -------------------------------------------------------------------------

    call aquabc_init(nkn,nstate,n_driving_functions,SURFACE_BOXES)
    
    call aquabc_read_constants('data/const_CL.txt')
    !call aquabc_read_constants('data/const_default.txt')
    call aquabc_write_constants('const_out.txt')

    call initialize_output('OUTPUT.csv')
    call write_output(time,nstate,STATE_VARIABLES)

    FLOW_RATES = FLOW_RATE
    VOLUMES    = VOLUME

    write(6,*) 'starting 0D simulation with boxes ',nkn

    do i = 1,NUM_TIME_STEPS

        TIME                = TIME + TIME_STEP

	if( mod(i,NUM_TIME_STEPS/10) == 0 ) write(6,*) i,time

        STATE_VARIABLES_OLD = STATE_VARIABLES
        
        call aquabc_run 		   ( &
             TIME                          , &	! [days]
             TIME_STEP                     , &	! [days]
             STATE_VARIABLES               , &  ! state_variables [mg/L]
             PH                            , &	! [1-14] (in/out)
             DRIVING_FUNCTIONS             , &	! external forcings
             SEDIMENT_FLUXES               )  	! imposed sedim fluxes [??]
        
        STATE_VARIABLES = STATE_VARIABLES + (                        &
               (BOUNDARY_CONCENTRATIONS * (FLOW_RATES /  VOLUMES)) - &
               (STATE_VARIABLES_OLD     * (FLOW_RATES /  VOLUMES))   &
               ) * TIME_STEP

        call write_output(time,nstate,STATE_VARIABLES)
    end do 
    
    write(6,*) 'simulation finished ',NUM_TIME_STEPS,TIME

    call finalize_output()

    ! -------------------------------------------------------------------------
    ! Clean up
    ! -------------------------------------------------------------------------
    deallocate(STATE_VARIABLES)
    deallocate(STATE_VARIABLES_OLD)
    deallocate(PH)
    deallocate(DRIVING_FUNCTIONS)
    deallocate(SEDIMENT_FLUXES)
    deallocate(SURFACE_BOXES)

    deallocate(VOLUMES)
    deallocate(FLOW_RATES)
    deallocate(BOUNDARY_CONCENTRATIONS)

    ! -------------------------------------------------------------------------    
end program

!=======================================================================

subroutine initialize_output(file)

    implicit none

    character*(*) file

    open (unit = 10, file = file, status = 'UNKNOWN')

    write(unit = 10, fmt = '(a15,10a42,a21)') &
               '           TIME',  &
          ',                NH4N,                NO3N', &
          ',                PO4P,                DOXY', &
          ',                DIAC,                ZOOC', &
          ',                ZOON,                ZOOP', &
          ',                DETC,                DETN', &
          ',                DETP,                 DOC', &
          ',                 DON,                 DOP', &
          ',          NOFIX_CYNC,                 OPA', &        
          ',    DISSOLVED_SILICA,  PARTICULATE_SILICA', &
          ',            FIX_CYNC,                 DIC', &
          ',          ALKALINITY'

end subroutine

subroutine write_output(time,nstate,STATE_VARIABLES)

    implicit none

    double precision time
    integer nstate
    double precision STATE_VARIABLES(1,nstate)

    write(unit = 10,  &
          fmt  = '(f15.6,21(a1,f20.10))') &
          TIME, &
          ',', STATE_VARIABLES  (1,  1), ',', STATE_VARIABLES  (1,  2), &
          ',', STATE_VARIABLES  (1,  3), ',', STATE_VARIABLES  (1,  4), &
          ',', STATE_VARIABLES  (1,  5), ',', STATE_VARIABLES  (1,  6), &
          ',', STATE_VARIABLES  (1,  7), ',', STATE_VARIABLES  (1,  8), &
          ',', STATE_VARIABLES  (1,  9), ',', STATE_VARIABLES  (1, 10), &
          ',', STATE_VARIABLES  (1, 11), ',', STATE_VARIABLES  (1, 12), &
          ',', STATE_VARIABLES  (1, 13), ',', STATE_VARIABLES  (1, 14), &
          ',', STATE_VARIABLES  (1, 15), ',', STATE_VARIABLES  (1, 16), &
          ',', STATE_VARIABLES  (1, 17), ',', STATE_VARIABLES  (1, 18), &
          ',', STATE_VARIABLES  (1, 19), ',', STATE_VARIABLES  (1, 20), &
          ',', STATE_VARIABLES  (1, 21)

end subroutine

subroutine finalize_output

    implicit none

    close(10)

end subroutine

!=======================================================================

