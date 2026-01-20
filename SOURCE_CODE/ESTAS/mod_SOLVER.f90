module PELAGIC_SOLVER

    use GLOBAL
    use PELAGIC_BOX_MODEL
    use PELAGIC_ECOLOGY
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use BOTTOM_SEDIMENTS


    implicit none

    ! Optimization: Module-level arrays to avoid reallocation
    real(kind = DBL), allocatable, dimension(:)   :: FLOWS
    real(kind = DBL), allocatable, dimension(:,:) :: BOUND_CONCS
    real(kind = DBL), allocatable, dimension(:)   :: DISPERSION_COEFFS
    real(kind = DBL), allocatable, dimension(:)   :: INTERFACE_AREAS
    real(kind = DBL), allocatable, dimension(:,:) :: SETTLING_VELOCITIES
    real(kind = DBL), allocatable, dimension(:)   :: SURFACE_AREAS
    real(kind = DBL), allocatable, dimension(:)   :: BOTTOM_AREAS
    real(kind = DBL), allocatable, dimension(:,:) :: MASS_LOADS
    real(kind = DBL), allocatable, dimension(:,:) :: MASS_WITHDRAWALS

contains

    subroutine SOLVE(PELAGIC_BOX_MODEL_DATA        , &
                     TIME, TIME_STEP               , &
                     PELAGIC_SOLVER_NO             , &
                     SETTLING_VELOCITIES_OUTPUT    , &
                     EFFECTIVE_DISSLOVED_FRACTIONS , &
                     EFFECTIVE_DEPOSITION_FRACTIONS, &
                     DEPOSITION_AREA_RATIOS        , &
                     nkn, nstate, NUM_ALLOLOPATHY_STATE_VARS)

        implicit none
        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA
        real(kind = DBL)          , intent(in)    :: TIME
        real(kind = DBL)          , intent(in)    :: TIME_STEP
        integer                   , intent(in)    :: PELAGIC_SOLVER_NO
        integer                   , intent(in)    :: nkn
        integer                   , intent(in)    :: nstate
        integer                   , intent(in)    :: NUM_ALLOLOPATHY_STATE_VARS

        real(kind = DBL)          , intent(inout), &
            dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)) :: &
            SETTLING_VELOCITIES_OUTPUT

        real(kind = DBL), dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
            intent(inout) :: EFFECTIVE_DISSLOVED_FRACTIONS

        real(kind = DBL), dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
            intent(inout) :: EFFECTIVE_DEPOSITION_FRACTIONS

        real(kind = DBL), dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
            intent(inout) :: DEPOSITION_AREA_RATIOS


        real(kind = DBL), dimension(nkn) :: CHLA
        real(kind = DBL), dimension(nkn) :: SETTLING_VELOCITY_FACTORS

        real(kind = DBL), dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)) :: &
             PRESCRIBED_SEDIMENT_FLUXES

        integer :: i
        integer :: j
        integer :: k

        ! Debug temporaries for mass update diagnostics
        real(kind = DBL) :: tot_deriv, old_mass, new_mass

        integer :: NUM_PELAGIC_ADVECTIVE_LINKS
        integer :: NUM_OPEN_BOUNDARIES
        integer :: NUM_PELAGIC_STATE_VARS
        integer :: NUM_PELAGIC_DISPERSIVE_LINKS
        integer :: NUM_PELAGIC_BOXES
        integer :: NUM_MASS_LOADS
        integer :: NUM_MASS_WITHDRAWALS

        integer :: STATE_VAR_NO
        integer :: SED_STATE_VAR_NO
        integer :: SED_LAYER_NO

        NUM_PELAGIC_ADVECTIVE_LINKS  = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_ADVECTIVE_LINKS
        NUM_OPEN_BOUNDARIES          = PELAGIC_BOX_MODEL_DATA % NUM_OPEN_BOUNDARIES
        NUM_PELAGIC_STATE_VARS       = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
        NUM_PELAGIC_DISPERSIVE_LINKS = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_DISPERSIVE_LINKS
        NUM_PELAGIC_BOXES            = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        NUM_MASS_LOADS               = PELAGIC_BOX_MODEL_DATA % NUM_MASS_LOADS
        NUM_MASS_WITHDRAWALS         = PELAGIC_BOX_MODEL_DATA % NUM_MASS_WITHDRAWALS

        if (.not. allocated(BOUND_CONCS)) then
            allocate(BOUND_CONCS        (NUM_OPEN_BOUNDARIES , NUM_PELAGIC_STATE_VARS))
            allocate(SETTLING_VELOCITIES(NUM_PELAGIC_BOXES   , NUM_PELAGIC_STATE_VARS))
            allocate(MASS_LOADS         (NUM_MASS_LOADS      , NUM_PELAGIC_STATE_VARS))
            allocate(MASS_WITHDRAWALS   (NUM_MASS_WITHDRAWALS, NUM_PELAGIC_STATE_VARS))
            allocate(FLOWS              (NUM_PELAGIC_ADVECTIVE_LINKS))
            allocate(DISPERSION_COEFFS  (NUM_PELAGIC_DISPERSIVE_LINKS))
            allocate(INTERFACE_AREAS    (NUM_PELAGIC_DISPERSIVE_LINKS))
            allocate(SURFACE_AREAS      (NUM_PELAGIC_BOXES))
            allocate(BOTTOM_AREAS       (NUM_PELAGIC_BOXES))
        end if

        if (PELAGIC_SOLVER_NO == 1) then

            call UPDATE_TIME_FUNCS &
                 (PELAGIC_BOX_MODEL_DATA  , TIME, &
                  FLOWS                   , &
                  BOUND_CONCS             , DISPERSION_COEFFS, INTERFACE_AREAS , &
                  SETTLING_VELOCITIES     , SURFACE_AREAS    , &
                  BOTTOM_AREAS, MASS_LOADS, MASS_WITHDRAWALS , &
                  PRESCRIBED_SEDIMENT_FLUXES)

            do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
                STATE_VARIABLES(i, :) = &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS
            end do

            call CALC_DERIV &
                 (PELAGIC_BOX_MODEL_DATA, TIME             , TIME_STEP            , &
                  FLOWS, BOUND_CONCS    , DISPERSION_COEFFS, INTERFACE_AREAS      , &
                  SETTLING_VELOCITIES   , SURFACE_AREAS, BOTTOM_AREAS, MASS_LOADS , &
                  MASS_WITHDRAWALS      , PRESCRIBED_SEDIMENT_FLUXES, 1     , &
                  EFFECTIVE_DISSLOVED_FRACTIONS , &
                  EFFECTIVE_DEPOSITION_FRACTIONS, &
                  DEPOSITION_AREA_RATIOS        , &
                  nkn, nstate, NUM_ALLOLOPATHY_STATE_VARS)

            SETTLING_VELOCITIES_OUTPUT(:,:) = SETTLING_VELOCITIES

            !CALCULATE THE STATE VARIABLES IN PELAGIC BOXES
            do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES

                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME = &
                     PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME + &
                     (PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(i, 1) * TIME_STEP)

                do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                    ! Compute total derivative for this state variable
                    tot_deriv = (PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS               (i, j, 1) + &
                      PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS              (i, j, 1) + &
                      PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS                (i, j, 1) + &
                      PELAGIC_BOX_MODEL_DATA % ECOL_MASS_LOAD_DERIVS               (i, j, 1) + &
                      PELAGIC_BOX_MODEL_DATA % ECOL_MASS_WITHDRAWAL_DERIVS         (i, j, 1) + &
                      PELAGIC_BOX_MODEL_DATA % ECOL_KINETIC_DERIVS                 (i, j, 1) + &
                      PELAGIC_BOX_MODEL_DATA % ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS(i, j, 1))

                    old_mass = PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % MASSES(j)
                    new_mass = old_mass + tot_deriv * TIME_STEP

                    if (new_mass < 0.0D0) then
                        write(6,*) 'NEGATIVE MASS PREDICTED: TIME=', TIME, ' BOX=', i, ' STATE=', j
                        write(6,*) '  OLD_MASS=', old_mass
                        write(6,*) '  TOT_DERIV=', tot_deriv, ' TIME_STEP=', TIME_STEP
                        write(6,*) '  NEW_MASS=', new_mass
                        write(6,*) '  CONC_BEFORE=', PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j)
                        write(6,*) '  VOLUME=', PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME
                        ! Detailed derivative breakdown for states 33-36 (allelopathy)
                        if (j >= 33 .and. j <= 36) then
                            write(6,*) '  DERIV BREAKDOWN for STATE', j, ':'
                            write(6,*) '    ADVECTION=', PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS(i, j, 1)
                            write(6,*) '    DISPERSION=', PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS(i, j, 1)
                            write(6,*) '    SETTLING=', PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS(i, j, 1)
                            write(6,*) '    MASS_LOAD=', PELAGIC_BOX_MODEL_DATA % ECOL_MASS_LOAD_DERIVS(i, j, 1)
                            write(6,*) '    MASS_WITHDRAWAL=', PELAGIC_BOX_MODEL_DATA % ECOL_MASS_WITHDRAWAL_DERIVS(i, j, 1)
                            write(6,*) '    KINETIC=', PELAGIC_BOX_MODEL_DATA % ECOL_KINETIC_DERIVS(i, j, 1)
                            write(6,*) '    SED_FLUX=', PELAGIC_BOX_MODEL_DATA % ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS(i, j, 1)
                        end if
                    end if

                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % MASSES(j) = new_mass

                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j) = &
                        new_mass / PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME

                    ! Diagnostic: if concentration becomes strongly negative, print context
                    if (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j) < -1.0D-12) then
                        write(6,*) 'ALERT: NEGATIVE CONC AFTER UPDATE: TIME=', TIME, ' BOX=', i, ' STATE=', j
                        write(6,*) '  NEW_MASS=', new_mass, ' OLD_MASS=', old_mass, ' TIME_STEP=', TIME_STEP
                        write(6,*) '  CONC='
                        write(6,*) PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j)
                        write(6,*) '  VOLUME='
                        write(6,*) PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME
                    end if

                    if (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j) < 1.0D-10) then
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j) = 1.0D-10

                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % MASSES(j) = &
                             1.0D-10 * PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME
                    end if

                    if (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j) > 1.0D10) then
                        write(*,*) '!!! ERROR IN SUBROUTINE PELAGIC SOLVER !!!'
                        write(*,*) '!!!     UNREALISTIC VALUE DETECTED     !!!'
                        write(*,*)
                        write(*,*) 'TIME            : ', TIME
                        write(*,*) 'BOX             : ', i
                        write(*,*) 'STATE VARIABLE  : ', j

                        write(*,*) 'CONCENTRATION   : ', &
                            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) %  CONCENTRATIONS(j)

                        write(*,*)

                        write(*,*) 'ADVECTION DERIVATIVE       : ', &
                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS      (i, j, 1)

                        write(*,*) 'DISPERSION DERIVATIVE      : ', &
                            PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS     (i, j, 1)

                        write(*,*) 'SETTLING DERIVATIVE        : ', &
                            PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS       (i, j, 1)

                        write(*,*) 'MASS LOAD DERIVATIVE       : ', &
                            PELAGIC_BOX_MODEL_DATA % ECOL_MASS_LOAD_DERIVS      (i, j, 1)

                        write(*,*) 'MASS WITHDRAWAL DERIVATIVE : ', &
                            PELAGIC_BOX_MODEL_DATA % ECOL_MASS_WITHDRAWAL_DERIVS(i, j, 1)

                        write(*,*) 'KINETIC DERIVATIVE         : ', &
                            PELAGIC_BOX_MODEL_DATA % ECOL_KINETIC_DERIVS        (i, j, 1)

                        write(*,*) 'KINETIC DERIVATIVE         : ', &
                            PELAGIC_BOX_MODEL_DATA % &
                                ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS            (i, j, 1)

                        do k = 1, NDIAGVAR
                            write(unit = *, fmt = '(a18, i5, a1, i5, a1, i5, a4, f30.10)') &
                                  '    PROCESS_RATES(', i, ',', j, ',', k, ') = ', PROCESS_RATES(i,j,k)
                        end do

                        write(*,*) 'VOLUME OF THE BOX          : ', &
                            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME

                        call DUMP_PELAGIC_MODEL_CONSTANTS_ON_FILE

                        open (unit = 20, file = 'EXERGY.out', status = 'UNKNOWN')
                        write(unit = 20, fmt = '(F20.6)') -9.99D5
                        close(20)

                        open (unit = 20, file = 'COST_TOTAL', status = 'UNKNOWN')
                        write(unit = 20, fmt = '(F20.6)')  9.99D5
                        close(20)

                        do STATE_VAR_NO = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                            write(*,fmt = '(a30 : F20.6)') &
                                  PELAGIC_BOX_MODEL_DATA % PELAGIC_STATE_VAR_NAMES(STATE_VAR_NO), &
                                  PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(STATE_VAR_NO)
                        end do

                        do SED_STATE_VAR_NO = 1, NUM_SED_VARS
                            write(*,fmt = '(a35)')      '-----------------------------------'
                            write(*,fmt = '(a30 : i5)') 'Sediment state variable : ', SED_STATE_VAR_NO
                            write(*,fmt = '(a35)')      '-----------------------------------'

                            do SED_LAYER_NO = 1,NUM_SED_LAYERS
                                write(*, fmt = '(a10 : F20.6)') &
                                      'Layer', INIT_SED_STATE_VARS(i, SED_LAYER_NO, SED_STATE_VAR_NO)
                            end do

                            write(*,fmt = '(a35)')      '-----------------------------------'
                        end do

                        stop
                    end if
                end do
            end do
        end if

    end subroutine SOLVE


    subroutine UPDATE_TIME_FUNCS &
               (PELAGIC_BOX_MODEL_DATA, TIME, FLOWS, BOUND_CONCS, &
                DISPERSION_COEFFS, INTERFACE_AREAS , SETTLING_VELOCITIES, &
                SURFACE_AREAS, BOTTOM_AREAS, MASS_LOADS, MASS_WITHDRAWALS, &
                PRESCRIBED_SEDIMENT_FLUXES)

        implicit none

        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA
        real(kind = DBL), intent(in) :: TIME
        real(kind = DBL), dimension(:)  , intent(inout) :: FLOWS
        real(kind = DBL), dimension(:,:), intent(inout) :: BOUND_CONCS
        real(kind = DBL), dimension(:)  , intent(inout) :: DISPERSION_COEFFS
        real(kind = DBL), dimension(:)  , intent(inout) :: INTERFACE_AREAS
        real(kind = DBL), dimension(:,:), intent(inout) :: SETTLING_VELOCITIES
        real(kind = DBL), dimension(:)  , intent(inout) :: SURFACE_AREAS
        real(kind = DBL), dimension(:)  , intent(inout) :: BOTTOM_AREAS
        real(kind = DBL), dimension(:,:), intent(inout) :: MASS_LOADS
        real(kind = DBL), dimension(:,:), intent(inout) :: MASS_WITHDRAWALS
        real(kind = DBL), dimension(:,:), intent(inout) :: PRESCRIBED_SEDIMENT_FLUXES

        integer :: i
        integer :: j
        integer :: k

        !Geometry related
        integer :: BATHYMETRY_NO
        integer :: BOX_NO

        !Advection related
        integer :: FLOW_TS_NO
        integer :: FLOW_TS_VAR_NO

        !Open boundary related
        integer :: FORCING_TS_NO
        integer :: FORCING_TS_VAR_NO

        !Dispersion related
        integer :: MIXING_TS_NO

        !Settling related
        integer ::SETTLING_VELOCITY_NO

        real(kind = DBL) :: SURFACE_ELEVATION
        real(kind = DBL) :: BOTTOM_ELEVATION
        real(kind = DBL) :: VOLUME

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_FORCING_TS
            do j = 1, PELAGIC_BOX_MODEL_DATA % FORCING_TS(i) % NUMBER_OF_VARIABLES
                PELAGIC_BOX_MODEL_DATA % FORCINGS(i, j) = VALUE_FROM_TIME_SERIE &
                     (PELAGIC_BOX_MODEL_DATA % FORCING_TS(i), TIME, j)
            end do
        end do

        !UPDATE GEOMETRY
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_BASINS
            do j = PELAGIC_BOX_MODEL_DATA % BASINS(i) % NUM_PELAGIC_BOXES, 1, -1

                BOX_NO            = &
                    PELAGIC_BOX_MODEL_DATA % BASINS(i) % PELAGIC_BOXES(j)

                BATHYMETRY_NO     = &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % BATHYMETRY_NO

                BOTTOM_ELEVATION  = &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % BOTTOM_ELEVATION

                VOLUME            = &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % VOLUME

                !Uncomment this if you want to check the flows
                !write(100 + BOX_NO,*) TIME, ',',
                !      PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % DEPTH, ',', VOLUME

                BATHYMETRY_NO     = &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % BATHYMETRY_NO

                SURFACE_ELEVATION = &
                    CALCULATE_SURFACE_ELEVATION &
                        (PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(BATHYMETRY_NO), &
                         BOTTOM_ELEVATION, VOLUME)

                PELAGIC_BOX_MODEL_DATA % &
                    PELAGIC_BOXES(i) % SURFACE_ELEVATION = SURFACE_ELEVATION

                PELAGIC_BOX_MODEL_DATA % &
                    PELAGIC_BOXES(BOX_NO) % DEPTH = SURFACE_ELEVATION - BOTTOM_ELEVATION

                if (j > 1) then
                    BOX_NO = PELAGIC_BOX_MODEL_DATA % BASINS(i) % PELAGIC_BOXES(j - 1)

                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                        BOTTOM_ELEVATION = SURFACE_ELEVATION
                end if
            end do
        end do

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES

            BATHYMETRY_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % BATHYMETRY_NO

            SURFACE_ELEVATION = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_ELEVATION

            BOTTOM_ELEVATION  = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % BOTTOM_ELEVATION

            SURFACE_AREAS(i)  = CALCULATE_SURFACE_AREA &
                 (PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(BATHYMETRY_NO), SURFACE_ELEVATION)

            BOTTOM_AREAS(i)   = CALCULATE_SURFACE_AREA &
                 (PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(BATHYMETRY_NO), BOTTOM_ELEVATION)
        end do

        !UPDATE OPEN BOUNDARY TIME SERIES FOR THE PELAGIC BOX MODEL
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_OPEN_BOUNDARIES
            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                FORCING_TS_NO     = &
                    PELAGIC_BOX_MODEL_DATA % OPEN_BOUNDARIES(i) % FORCING_TS_NOS(j)

                FORCING_TS_VAR_NO = &
                    PELAGIC_BOX_MODEL_DATA % OPEN_BOUNDARIES(i) % FORCING_TS_VAR_NOS(j)

                BOUND_CONCS(i, j) = &
                    PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)
            end do
        end do

        !UPDATE ADVECTIVE LINK TIME SERIES FOR THE PELAGIC BOX MODEL
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_ADVECTIVE_LINKS

            FLOW_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % FLOW_TS_NO

            FLOW_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % FLOW_TS_VAR_NO

            FLOWS(i)       = &
                VALUE_FROM_TIME_SERIE &
                    (PELAGIC_BOX_MODEL_DATA % FLOWS(FLOW_TS_NO), TIME, FLOW_TS_VAR_NO)
        end do

        !UPDATE DISPERSION LINK TIME SERIES FOR THE PELAGIC BOX MODEL
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_DISPERSIVE_LINKS

            MIXING_TS_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % MIXING_TS_NO

            DISPERSION_COEFFS(MIXING_TS_NO) = VALUE_FROM_TIME_SERIE &
                 (PELAGIC_BOX_MODEL_DATA % MIXING_EXCHANGES(MIXING_TS_NO), TIME, 1)

            INTERFACE_AREAS  (MIXING_TS_NO) = VALUE_FROM_TIME_SERIE &
                 (PELAGIC_BOX_MODEL_DATA % MIXING_EXCHANGES(MIXING_TS_NO), TIME, 2)
        end do

        !UPDATE SETTLING TIME SERIES FOR THE PELAGIC BOX MODEL
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES

            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                SETTLING_VELOCITY_NO = &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SETTLING_TS_NOS(j)

                if ((SETTLING_VELOCITY_NO > 0).and.(SETTLING_VELOCITY_NO <= &
                     PELAGIC_BOX_MODEL_DATA % NUM_SETTLING_VELOCITIES)) then

                    SETTLING_VELOCITIES(i, j) = &
                        VALUE_FROM_TIME_SERIE(PELAGIC_BOX_MODEL_DATA % &
                                              SETTLING_VELOCITIES(SETTLING_VELOCITY_NO), &
                                              TIME, 1)
                else
                    SETTLING_VELOCITIES(i, j) = 0.0D0
                end if
            end do
        end do

        !UPDATE MASS LOADS FOR THE PELAGIC BOX MODEL
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_MASS_LOADS

            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                FORCING_TS_NO     = &
                    PELAGIC_BOX_MODEL_DATA % MASS_LOADS(i) % FORCING_TS_NOS(j)

                FORCING_TS_VAR_NO = &
                    PELAGIC_BOX_MODEL_DATA % MASS_LOADS(i) % FORCING_TS_VAR_NOS(j)

                MASS_LOADS(i, j)  = &
                    PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)
            end do
        end do

        !UPDATE MASS WITHDRAWALS FOR THE PELAGIC BOX MODEL
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_MASS_WITHDRAWALS

            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                FORCING_TS_NO          = &
                    PELAGIC_BOX_MODEL_DATA % MASS_WITHDRAWALS(i) % FORCING_TS_NOS(j)

                FORCING_TS_VAR_NO      = &
                    PELAGIC_BOX_MODEL_DATA % MASS_WITHDRAWALS(i) % FORCING_TS_VAR_NOS(j)

                MASS_WITHDRAWALS(i, j) = &
                    PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)
            end do
        end do

        !UPDATE PRESCRIBED SEDIMENT FLUXES
        if (MODEL_BOTTOM_SEDIMENTS == 1) then
            PRESCRIBED_SEDIMENT_FLUXES(:,:) = 0.0D0

            do i = 1, NUM_PRESCRIBED_SEDIMENT_FLUX_SETS
                do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
                    do k = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                        FORCING_TS_NO     = &
                            PELAGIC_BOX_MODEL_DATA % SEDIMENT_FLUX_TS_NOS    (i, j, k)

                        FORCING_TS_VAR_NO = &
                            PELAGIC_BOX_MODEL_DATA % SEDIMENT_FLUX_TS_VAR_NOS(i, j, k)

                        PRESCRIBED_SEDIMENT_FLUXES(j, k) = &
                             PRESCRIBED_SEDIMENT_FLUXES(j, k) + &
                             PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)
                    end do
                end do
            end do
        end if

        !UPDATE PELAGIC ECOLOGY DRIVING FUNCTIONS FOR THE PELAGIC BOX MODEL
        call UPDATE_PELAGIC_DRIVING_FUNCS(PELAGIC_BOX_MODEL_DATA)

    end subroutine UPDATE_TIME_FUNCS


    subroutine CALC_DERIV &
               (PELAGIC_BOX_MODEL_DATA, TIME             , TIME_STEP            , &
                FLOWS, BOUND_CONCS    , DISPERSION_COEFFS, INTERFACE_AREAS      , &
                SETTLING_VELOCITIES   , SURFACE_AREAS, BOTTOM_AREAS, MASS_LOADS , &
                MASS_WITHDRAWALS      , PRESCRIBED_SEDIMENT_FLUXES ,DERIV_NO    , &
                EFFECTIVE_DISSLOVED_FRACTIONS , &
                EFFECTIVE_DEPOSITION_FRACTIONS, &
                DEPOSITION_AREA_RATIOS        , &
                nkn, nstate, NUM_ALLOLOPATHY_STATE_VARS)

        implicit none

        type(PELAGIC_BOX_MODEL_DS)         , intent(inout) :: PELAGIC_BOX_MODEL_DATA
        real(kind = DBL)                   , intent(in)    :: TIME
        real(kind = DBL)                   , intent(in)    :: TIME_STEP

        real(kind = DBL), dimension(:)     , intent(in)    :: FLOWS
        real(kind = DBL), dimension(:,:)   , intent(inout) :: BOUND_CONCS
        real(kind = DBL), dimension(:)     , intent(inout) :: DISPERSION_COEFFS
        real(kind = DBL), dimension(:)     , intent(inout) :: INTERFACE_AREAS
        real(kind = DBL), dimension(:,:)   , intent(inout) :: SETTLING_VELOCITIES
        real(kind = DBL), dimension(:)     , intent(inout) :: SURFACE_AREAS
        real(kind = DBL), dimension(:)     , intent(inout) :: BOTTOM_AREAS
        real(kind = DBL), dimension(:,:)   , intent(inout) :: MASS_LOADS
        real(kind = DBL), dimension(:,:)   , intent(inout) :: MASS_WITHDRAWALS

        integer, intent(in) :: DERIV_NO

        integer, intent(in) :: nkn
        integer, intent(in) :: nstate
        integer, intent(in) :: NUM_ALLOLOPATHY_STATE_VARS

        real(kind = DBL), &
            dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
            intent(inout) :: EFFECTIVE_DISSLOVED_FRACTIONS

        real(kind = DBL), &
            dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
            intent(inout) :: EFFECTIVE_DEPOSITION_FRACTIONS

        real(kind = DBL), &
            dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
            intent(inout) :: DEPOSITION_AREA_RATIOS

        real(kind = DBL), &
            dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
            intent(inout) :: PRESCRIBED_SEDIMENT_FLUXES

        integer :: i
        integer :: j
        integer :: k
        integer :: FLOW_TS_NO
        integer :: FLOW_TS_VAR_NO
        integer :: UPSTREAM_BOX_NO
        integer :: DOWNSTREAM_BOX_NO
        integer :: OPEN_BOUNDARY_NO
        integer :: FIRST_BOX_NO
        integer :: SECOND_BOX_NO
        integer :: O_BOX
        integer :: U_BOX
        integer :: MASS_LOAD_NO
        integer :: MASS_WITHDRAWAL_NO
        integer :: BOX_NO
        integer :: BASIN_NO

        real(kind = DBL) :: UPSTREAM_CONC
        real(kind = DBL) :: DOWNSTREAM_CONC
        real(kind = DBL) :: FIRST_BOX_CONC
        real(kind = DBL) :: SECOND_BOX_CONC
        real(kind = DBL) :: DISPERSION_COEFF
        real(kind = DBL) :: INTERFACE_AREA
        real(kind = DBL) :: MIXING_LENGTH

        real(kind = DBL), &
            dimension(PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS) :: CONCENTRATIONS

        real(kind = DBL), &
            dimension(PELAGIC_BOX_MODEL_DATA % NUM_MODEL_CONSTANTS)    :: MODEL_CONSTANTS

        real(kind = DBL), &
            dimension(PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS) :: KINETIC_DERIVS

        integer, dimension(PELAGIC_BOX_MODEL_DATA % NUM_FLAGS) :: FLAGS
        integer :: PELAGIC_ECOLOGY_CALLED_BEFORE

        !SEDIMENT DIAGENESIS MODEL REALETED DECLARATIONS
        integer :: PROCESS_RATE_BEGIN_NO
        integer :: PROCESS_RATE_END_NO
        integer :: SED_LAYER_NO
        integer :: STATE_VAR_NO

        real(kind = DBL) :: MASS_SETTLING_INTO
        real(kind = DBL) :: MASS_SETTLING_FROM

        integer :: SAVED_OUTPUT_NO

        real(kind = DBL) :: RESUSPENSION_VELOCITY

        real(kind = DBL), dimension((nstate + NUM_ALLOLOPATHY_STATE_VARS)) :: &
             RESUSPENSION_CONCENTRATIONS

        real(kind = DBL) :: SHEAR_STRESS

        real(kind = DBL), dimension(nkn) :: CHLA
        real(kind = DBL), dimension(nkn) :: SETTLING_VELOCITY_FACTORS

        real(kind = DBL), dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)) :: &
             SEDIMENT_FLUXES

        call CALCULATE_SETTLING_SUPRESSION(SETTLING_VELOCITY_FACTORS)

        ! ---------------------------------------------------------------------------
        ! Consideration how settling suppression factor by chlorophyl-a will
        ! be considered for the particular box and state variable
        !
        !    CHLA_SUPRESSION_OF_SETTLING
        !       1                 : Consider the supression as calculated above
        !       0                 : No supression effect
        ! ---------------------------------------------------------------------------
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                if (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                        CHLA_SUPRESSION_OF_SETTLING(j) == 1) then
                   SETTLING_VELOCITIES(i,j) = &
                       SETTLING_VELOCITIES(i, j) * SETTLING_VELOCITY_FACTORS(i)
                end if
            end do
        end do
        ! ---------------------------------------------------------------------------

        !Initialize the derivatives from previous calculations to zero
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(i, DERIV_NO) = 0.0D0

            PELAGIC_BOX_MODEL_DATA % SALT_ADVECTION_DERIVS (i, DERIV_NO) = 0.0D0
            PELAGIC_BOX_MODEL_DATA % SALT_DISPERSION_DERIVS(i, DERIV_NO) = 0.0D0

            PELAGIC_BOX_MODEL_DATA % TEMP_ADVECTION_DERIVS (i, DERIV_NO) = 0.0D0
            PELAGIC_BOX_MODEL_DATA % TEMP_DISPERSION_DERIVS(i, DERIV_NO) = 0.0D0

            MODEL_CONSTANTS   = 0.0D0
            DRIVING_FUNCTIONS = 0.0D0
            PROCESS_RATES     = 0.0D0
            SAVED_OUTPUTS     = 0.0D0
            FLAGS             = 0

            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS      &
                    (i, j, DERIV_NO) = 0.0D0

                PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS     &
                    (i, j, DERIV_NO) = 0.0D0

                PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS       &
                    (i, j, DERIV_NO) = 0.0D0

                PELAGIC_BOX_MODEL_DATA % ECOL_MASS_LOAD_DERIVS      &
                    (i, j, DERIV_NO) = 0.0D0

                PELAGIC_BOX_MODEL_DATA % ECOL_MASS_WITHDRAWAL_DERIVS&
                    (i, j, DERIV_NO) = 0.0D0
            end do
        end do

        !--------------------------------------------------------------------------------
        !Consider precipitation and evaporation for the boxes at the top of basins. This
        !part will be totally reprogrammed when the drying and wetting option for the
        !boxes will be implemented.
        !--------------------------------------------------------------------------------
        do BASIN_NO = 1, PELAGIC_BOX_MODEL_DATA % NUM_BASINS
            BOX_NO = PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % PELAGIC_BOXES(1)

            !----------------------------------------------------------------------------
            !Consider the increase of box volume by precipitation
            !Convert precipitation from mm/day to m^3/day
            !----------------------------------------------------------------------------
            FLOW_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                PRECIPITATION_TS_NO

            FLOW_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                PRECIPITATION_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(BOX_NO, DERIV_NO) = &
                PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(BOX_NO, DERIV_NO) + &
                (PELAGIC_BOX_MODEL_DATA % FORCINGS(FLOW_TS_NO, FLOW_TS_VAR_NO) * &
                 SURFACE_AREAS(BOX_NO) * 1.0D-3)
            !----------------------------------------------------------------------------


            !----------------------------------------------------------------------------
            !Consider the decrease of box volume by evaporation
            !Convert evaporation from mm/day to m^3/day
            !----------------------------------------------------------------------------
            FLOW_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                EVAPORATION_TS_NO

            FLOW_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                EVAPORATION_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(BOX_NO, DERIV_NO) = &
                PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(BOX_NO, DERIV_NO) - &
                (PELAGIC_BOX_MODEL_DATA % FORCINGS(FLOW_TS_NO, FLOW_TS_VAR_NO) * &
                 SURFACE_AREAS(BOX_NO) * 1.0D-3)
            !----------------------------------------------------------------------------
        end do
        !--------------------------------------------------------------------------------

        !CALCULATE THE ADVECTIVE DERIVATIVES
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_ADVECTIVE_LINKS

            FLOW_TS_NO        = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % FLOW_TS_NO

            FLOW_TS_VAR_NO    = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % FLOW_TS_VAR_NO

            UPSTREAM_BOX_NO   = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % UPSTREAM_BOX_NO

            DOWNSTREAM_BOX_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % DOWNSTREAM_BOX_NO

            if (FLOWS(i) >= 0.0D0) then
                if (UPSTREAM_BOX_NO > 0) then

                    PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(UPSTREAM_BOX_NO, DERIV_NO) = &
                         PELAGIC_BOX_MODEL_DATA % &
                             VOLUME_DERIVS(UPSTREAM_BOX_NO, DERIV_NO)   - &
                         (FLOWS(i) * 8.64D4)
                end if

                if (DOWNSTREAM_BOX_NO > 0) then

                    PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(DOWNSTREAM_BOX_NO, DERIV_NO) = &
                         PELAGIC_BOX_MODEL_DATA % &
                             VOLUME_DERIVS(DOWNSTREAM_BOX_NO, DERIV_NO) + &
                         (FLOWS(i) * 8.64D4)
                end if
            else
                if (UPSTREAM_BOX_NO > 0) then

                    PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(UPSTREAM_BOX_NO, DERIV_NO) = &
                         PELAGIC_BOX_MODEL_DATA % &
                             VOLUME_DERIVS(UPSTREAM_BOX_NO, DERIV_NO)   + &
                         (FLOWS(i) * 8.64D4)
                end if

                if (DOWNSTREAM_BOX_NO > 0) then

                    PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(DOWNSTREAM_BOX_NO, DERIV_NO) = &
                         PELAGIC_BOX_MODEL_DATA % &
                             VOLUME_DERIVS(DOWNSTREAM_BOX_NO, DERIV_NO) - &
                         (FLOWS(i) * 8.64D4)
                end if
            end if


            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                if (PELAGIC_BOX_MODEL_DATA % ADVECTION_ON(j) == 1) then
                    !If the upstream box is a pelagic box then use its
                    !concentration, else use the corresponding open boundary
                    !concentration
                    if (UPSTREAM_BOX_NO > 0) then

                        UPSTREAM_CONC    = &
                            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(UPSTREAM_BOX_NO) % &
                            CONCENTRATIONS(j)
                    else
                        OPEN_BOUNDARY_NO = (-1) * UPSTREAM_BOX_NO
                        UPSTREAM_CONC    = BOUND_CONCS(OPEN_BOUNDARY_NO, j)
                    end if

                    !If the downstream box is a pelagic box then use its
                    !concentration, else use the corresponding open boundary
                    !concentration
                    if (DOWNSTREAM_BOX_NO > 0) then

                        DOWNSTREAM_CONC  = &
                            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(DOWNSTREAM_BOX_NO) % &
                            CONCENTRATIONS(j)
                    else
                        OPEN_BOUNDARY_NO = (-1) * DOWNSTREAM_BOX_NO
                        DOWNSTREAM_CONC  = BOUND_CONCS(OPEN_BOUNDARY_NO, j)
                    end if

                    if (FLOWS(i) >= 0.0D0) then

                        if (UPSTREAM_BOX_NO > 0) then

                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS &
                                (UPSTREAM_BOX_NO, j, DERIV_NO) = &
                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS  &
                                (UPSTREAM_BOX_NO, j, DERIV_NO) - &
                                (FLOWS(i) * 8.64D4 * UPSTREAM_CONC)
                        end if

                        if (DOWNSTREAM_BOX_NO > 0) then

                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS &
                                 (DOWNSTREAM_BOX_NO, j, DERIV_NO) = &
                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS  &
                                 (DOWNSTREAM_BOX_NO, j, DERIV_NO) + &
                                 (FLOWS(i) * 8.64D4 * UPSTREAM_CONC)
                        end if
                    else
                        if (UPSTREAM_BOX_NO > 0) then

                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS &
                                (UPSTREAM_BOX_NO, j, DERIV_NO) = &
                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS  &
                                (UPSTREAM_BOX_NO, j, DERIV_NO) + &
                            (FLOWS(i) * 8.64D4 * DOWNSTREAM_CONC)
                        end if

                        if (DOWNSTREAM_BOX_NO > 0) then

                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS &
                                (DOWNSTREAM_BOX_NO, j, DERIV_NO) = &
                            PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS  &
                                (DOWNSTREAM_BOX_NO, j, DERIV_NO) - &
                            (FLOWS(i) * 8.64D4 * DOWNSTREAM_CONC)
                        end if
                    end if
                end if
            end do
        end do

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                if (PELAGIC_BOX_MODEL_DATA % ADVECTION_ON(j) == 0) then
                    PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS(i, j, DERIV_NO) = 0.0D0
                end if
            end do
        end do

        !CALCULATE THE DISPERSIVE DERIVATIVES
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_DISPERSIVE_LINKS

            FIRST_BOX_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % FIRST_BOX_NO

            SECOND_BOX_NO    = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % SECOND_BOX_NO

            DISPERSION_COEFF = DISPERSION_COEFFS(i)

            INTERFACE_AREA   = INTERFACE_AREAS(i)

            MIXING_LENGTH    = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % MIXING_LENGTH

            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                if (PELAGIC_BOX_MODEL_DATA % DIFFUSION_ON(j) == 1) then
                    !If the first box is a pelagic box then use its concentration,
                    !else use the corresponding open boundary concentration
                    if (FIRST_BOX_NO > 0) then

                        FIRST_BOX_CONC = &
                            PELAGIC_BOX_MODEL_DATA % &
                                PELAGIC_BOXES(FIRST_BOX_NO) % CONCENTRATIONS(j)
                    else
                        OPEN_BOUNDARY_NO = (-1) * FIRST_BOX_NO
                        FIRST_BOX_CONC   = BOUND_CONCS(OPEN_BOUNDARY_NO, j)
                    end if

                    !If the second box is a pelagic box then use its concentration,
                    !else use the corresponding open boundary concentration
                    if (SECOND_BOX_NO > 0) then
                        SECOND_BOX_CONC  = &
                            PELAGIC_BOX_MODEL_DATA % &
                                PELAGIC_BOXES(SECOND_BOX_NO) % CONCENTRATIONS(j)
                    else
                        OPEN_BOUNDARY_NO = (-1) * SECOND_BOX_NO
                        SECOND_BOX_CONC  = BOUND_CONCS(OPEN_BOUNDARY_NO, j)
                    end if

                    if (FIRST_BOX_NO > 0) then

                        PELAGIC_BOX_MODEL_DATA % &
                            ECOL_DISPERSION_DERIVS (FIRST_BOX_NO, j, DERIV_NO) = &
                        PELAGIC_BOX_MODEL_DATA % &
                            ECOL_DISPERSION_DERIVS(FIRST_BOX_NO, j, DERIV_NO) + &
                            (((DISPERSION_COEFF * 8.64D4 * INTERFACE_AREA) / MIXING_LENGTH) * &
                             (SECOND_BOX_CONC - FIRST_BOX_CONC))
                    end if

                    if (SECOND_BOX_NO > 0) then

                        PELAGIC_BOX_MODEL_DATA % &
                            ECOL_DISPERSION_DERIVS (SECOND_BOX_NO, j, DERIV_NO) = &
                        PELAGIC_BOX_MODEL_DATA % &
                            ECOL_DISPERSION_DERIVS(SECOND_BOX_NO, j, DERIV_NO) + &
                            (((DISPERSION_COEFF * 8.64D4 * INTERFACE_AREA) / MIXING_LENGTH) * &
                             (FIRST_BOX_CONC - SECOND_BOX_CONC))
                    end if

                    if ((FIRST_BOX_NO > 0).and.(SECOND_BOX_NO > 0)) then

                        if ((PELAGIC_BOX_MODEL_DATA % &
                                ECOL_DISPERSION_DERIVS(FIRST_BOX_NO , j, DERIV_NO) > 1.0D30).or. &
                            (PELAGIC_BOX_MODEL_DATA % &
                                ECOL_DISPERSION_DERIVS(SECOND_BOX_NO, j, DERIV_NO) > 1.0D30)) then

                            write(*,*)
                            write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                            write(*,*) '!!           DISPERSION DERIVATIVE MAY BE UNREALISTIC             !!'
                            write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                            write(*,*)

                            write(*,*) 'Dispersive derivative for the first box  : ', &
                                       PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS &
                                           (FIRST_BOX_NO , j, DERIV_NO)

                            write(*,*) 'Dispersive derivative for the second box : ', &
                                       PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS &
                                           (SECOND_BOX_NO , j, DERIV_NO)

                            write(*,*) 'Mixing length                   : ', MIXING_LENGTH
                            write(*,*) 'Concentration in the first box  : ', FIRST_BOX_CONC
                            write(*,*) 'Concentration in the second box : ', SECOND_BOX_CONC
                            write(*,*) 'Dispersion coefficient          : ', DISPERSION_COEFF
                            write(*,*) 'Interface area                  : ', INTERFACE_AREA
                        end if
                    end if

                    if ((FIRST_BOX_NO > 0).and.(SECOND_BOX_NO < 0)) then
                        if ((PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS &
                                (FIRST_BOX_NO , j, DERIV_NO) > 1.0D30)) then
                            write(*,*)
                            write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                            write(*,*) '!!           DISPERSION DERIVATIVE MAY BE UNREALISTIC             !!'
                            write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                            write(*,*)

                            write(*,*) 'Dispersive derivative for the first box  : ', &
                                       PELAGIC_BOX_MODEL_DATA % &
                                           ECOL_DISPERSION_DERIVS (FIRST_BOX_NO , j, DERIV_NO)

                            write(*,*) 'Mixing length                            : ', MIXING_LENGTH
                            write(*,*) 'Concentration in the first box           : ', FIRST_BOX_CONC
                            write(*,*) 'Concentration in the second box          : ', SECOND_BOX_CONC
                            write(*,*) 'Dispersion coefficient                   : ', DISPERSION_COEFF
                            write(*,*) 'Interface area                           : ', INTERFACE_AREA
                        end if
                    end if

                    if ((FIRST_BOX_NO < 0).and.(SECOND_BOX_NO > 0)) then
                        if ((PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS &
                                (SECOND_BOX_NO , j, DERIV_NO) > 1.0D30)) then
                            write(*,*)
                            write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                            write(*,*) '!!           DISPERSION DERIVATIVE MAY BE UNREALISTIC             !!'
                            write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                            write(*,*)

                            write(*,*) 'Dispersive derivative for the second box : ', &
                                       PELAGIC_BOX_MODEL_DATA % &
                                           ECOL_DISPERSION_DERIVS (SECOND_BOX_NO , j, DERIV_NO)

                            write(*,*) 'Mixing length                            : ', MIXING_LENGTH
                            write(*,*) 'Concentration in the first box           : ', FIRST_BOX_CONC
                            write(*,*) 'Concentration in the second box          : ', SECOND_BOX_CONC
                            write(*,*) 'Dispersion coefficient                   : ', DISPERSION_COEFF
                            write(*,*) 'Interface area                           : ', INTERFACE_AREA
                        end if
                    end if
                end if
            end do
        end do

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                if (PELAGIC_BOX_MODEL_DATA % DIFFUSION_ON(j) == 0) then
                    PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS(i, j, DERIV_NO) = 0.0D0
                end if
            end do
        end do

        !CALCULATE THE SETTLING DERIVATIVES
        do BASIN_NO = 1, PELAGIC_BOX_MODEL_DATA % NUM_BASINS
            do BOX_NO = 1, PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % NUM_PELAGIC_BOXES
                i = PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % PELAGIC_BOXES(BOX_NO)

                if (RESUSPENSION_OPTION == 2) then
                    if (ACTIVATE_RESUSPENSIONS(i) > 0) then
                        SHUT_DOWN_SETTLING = 0

                        ! Get the shear stress from the box
                        SHEAR_STRESS = &
                            VALUE_FROM_TIME_SERIE &
                                (RESUSPENSION_TS(SHEAR_STRESS_TS_NOS(i)), TIME, &
                                 SHEAR_STRESS_TS_VAR_NOS(i))

                        if (SHEAR_STRESS > BOX_CRITICAL_SHEAR_STRESSES(i)) then
                            SHUT_DOWN_SETTLING = 1
                        end if
                    end if
                end if

                if (SHUT_DOWN_SETTLING == 0) then
                    do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                        if (PELAGIC_BOX_MODEL_DATA % SETTLING_ON(j) == 1) then
                            if (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                                DISSOLVED_FRACTIONS(j) < 0.0D0) then

                                SAVED_OUTPUT_NO = &
                                    int(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                                        DISSOLVED_FRACTIONS(j) * (-1.0D0) )

                                EFFECTIVE_DISSLOVED_FRACTIONS(i, j) = &
                                    PELAGIC_BOX_MODEL_DATA % &
                                        PELAGIC_BOXES(i) % SAVED_OUTPUTS(SAVED_OUTPUT_NO)
                            else
                                EFFECTIVE_DISSLOVED_FRACTIONS(i, j) = &
                                    PELAGIC_BOX_MODEL_DATA % &
                                        PELAGIC_BOXES(i) % DISSOLVED_FRACTIONS(j)
                            end if

                            if (PELAGIC_BOX_MODEL_DATA % &
                                    PELAGIC_BOXES(i) % OVERLAYING_PELAGIC_BOX == 0) then

                                  PELAGIC_BOX_MODEL_DATA % &
                                      ECOL_SETTLING_DERIVS(i, j, DERIV_NO) = &
                                  SURFACE_AREAS(i) * SETTLING_VELOCITIES(i, j) *  &
                                      PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                                          CONCENTRATIONS(j) * &
                                       (1.0D0 - EFFECTIVE_DISSLOVED_FRACTIONS(i, j)) * (-1.0D0)

                                ! If there is no underlaying model box, then effective undeposited fraction
                                ! is equal to deposited fraction, otherwise it is deposited fraction times
                                ! depotisition area ratio
                                if (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                                        UNDERLAYING_PELAGIC_BOX == 0) then

                                    EFFECTIVE_DEPOSITION_FRACTIONS(i, j) = &
                                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % DEPOSITED_FRACTIONS(j)
                                else
                                    DEPOSITION_AREA_RATIOS        (i, j) = &
                                        (SURFACE_AREAS(i) - BOTTOM_AREAS(i)) / SURFACE_AREAS(i)

                                    EFFECTIVE_DEPOSITION_FRACTIONS(i, j) = DEPOSITION_AREA_RATIOS(i, j) * &
                                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % DEPOSITED_FRACTIONS(j)
                                end if

                                PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS(i, j, DERIV_NO) = &
                                    PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS(i, j, DERIV_NO) * &
                                    EFFECTIVE_DEPOSITION_FRACTIONS(i, j)
                            else
                                O_BOX = PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % OVERLAYING_PELAGIC_BOX

                                MASS_SETTLING_INTO = &
                                    (BOTTOM_AREAS(O_BOX) * SETTLING_VELOCITIES(O_BOX, j) *  &
                                     PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(O_BOX) % CONCENTRATIONS(j)) * &
                                     (1.0D0 - EFFECTIVE_DISSLOVED_FRACTIONS(i, j))

                                MASS_SETTLING_FROM = &
                                    SURFACE_AREAS(i) * SETTLING_VELOCITIES(i, j) *  &
                                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j) * &
                                    (1.0D0 - EFFECTIVE_DISSLOVED_FRACTIONS(i, j))

                                if (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                                        UNDERLAYING_PELAGIC_BOX == 0) then

                                    EFFECTIVE_DEPOSITION_FRACTIONS(i, j) = &
                                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % DEPOSITED_FRACTIONS(j)
                                else
                                    DEPOSITION_AREA_RATIOS        (i, j) = &
                                        (SURFACE_AREAS(i) - BOTTOM_AREAS(i)) / SURFACE_AREAS(i)

                                    EFFECTIVE_DEPOSITION_FRACTIONS(i, j) = DEPOSITION_AREA_RATIOS(i, j) * &
                                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % DEPOSITED_FRACTIONS(j)
                                end if

                                MASS_SETTLING_FROM = &
                                    MASS_SETTLING_FROM * EFFECTIVE_DEPOSITION_FRACTIONS(i, j)

                                PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS(i, j, DERIV_NO) = &
                                    MASS_SETTLING_INTO - MASS_SETTLING_FROM
                            end if
                        else
                            PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS(i, j, DERIV_NO) = 0.0D0
                        end if
                    end do
                else
                    PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS(i, :, DERIV_NO) = 0.0D0
                end if
            end do
        end do

        ! CALCULATE THE PRESCRIBED SEDIMENT FLUXES
        if (MODEL_BOTTOM_SEDIMENTS == 1) then
            do BASIN_NO = 1, PELAGIC_BOX_MODEL_DATA % NUM_BASINS
                do BOX_NO = 1, PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % NUM_PELAGIC_BOXES
                    i = PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % PELAGIC_BOXES(BOX_NO)

                    do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                        PELAGIC_BOX_MODEL_DATA % &
                            ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS(i, j, DERIV_NO) = &
                        SURFACE_AREAS(i) * PRESCRIBED_SEDIMENT_FLUXES(i, j)

                        !----------------------------------------------------------------
                        !If there is no underlaying model box, then flux is not corrected
                        !otherwise it must be corrected as in the following if clause
                        !----------------------------------------------------------------
                        if (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                                UNDERLAYING_PELAGIC_BOX .ne. 0) then

                            PELAGIC_BOX_MODEL_DATA % &
                                ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS(i, j, DERIV_NO) = &
                            PELAGIC_BOX_MODEL_DATA % &
                                ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS(i, j, DERIV_NO) * &
                            (SURFACE_AREAS(i) - BOTTOM_AREAS(i)) / SURFACE_AREAS(i)
                        end if
                        !----------------------------------------------------------------
                    end do
                end do
            end do
        else
            PELAGIC_BOX_MODEL_DATA % ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS(:,:,:) = 0.0D0
        end if

        !CALCULATE THE MASS LOAD AND MASS WITHDRAWAL DERIVATIVES
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                do k = 1, PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % NUM_MASS_LOADS

                    MASS_LOAD_NO = &
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % MASS_LOAD_NOS(k)

                    PELAGIC_BOX_MODEL_DATA % ECOL_MASS_LOAD_DERIVS(i, j, DERIV_NO) = &
                        PELAGIC_BOX_MODEL_DATA % ECOL_MASS_LOAD_DERIVS(i, j, DERIV_NO) + &
                        (MASS_LOADS(MASS_LOAD_NO, j) * 1.0D3)
                end do

                do k = 1, PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % NUM_MASS_WITHDRAWALS

                    MASS_WITHDRAWAL_NO = &
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % MASS_WITHDRAWAL_NOS(k)

                    PELAGIC_BOX_MODEL_DATA % &
                        ECOL_MASS_WITHDRAWAL_DERIVS(i, j, DERIV_NO) = &
                    PELAGIC_BOX_MODEL_DATA % &
                        ECOL_MASS_WITHDRAWAL_DERIVS(i, j, DERIV_NO) + &
                        (MASS_WITHDRAWALS(MASS_WITHDRAWAL_NO, j) * 1.0D3)
                end do
            end do
        end do

        FLAGS           = PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(1) % FLAGS

        CALLED_BEFORE   = &
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(1) % PELAGIC_ECOLOGY_CALLED_BEFORE

        !CALCULATE KINETIC DERIVATIVES
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            STATE_VARIABLES(i, :)          = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS

            DRIVING_FUNCTIONS(i, :) = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % DRIVING_FUNCTIONS

            SAVED_OUTPUTS(i, :) = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SAVED_OUTPUTS
        end do

        SEDIMENT_FLUXES(:, :) = 0.0D0

        if (MODEL_BOTTOM_SEDIMENTS == 1) then
            do i = 1, nkn
                do j = 1, nstate

                    SEDIMENT_FLUXES(i, j) = &
                        PELAGIC_BOX_MODEL_DATA % &
                            ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS(i, j, 1) / &
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME
                end do
            end do
        end if

        call PELAGIC_KINETICS &
             (PELAGIC_BOX_MODEL_DATA, SEDIMENT_FLUXES, TIME, TIME_STEP, &
              CALLED_BEFORE)

        do i = 1, nkn
            do j = 1, nstate
                if (STATE_VARIABLES(i,j) /= STATE_VARIABLES(i,j)) then
                    open (unit = 88, file = 'ERROR_LOG.txt', status = 'UNKNOWN')

                    write(unit = 88, fmt = *) 'A Nan is detected'
                    write(unit = 88, fmt = *) '-----------------'
                    write(unit = 88, fmt = *) 'Box no            : ', i
                    write(unit = 88, fmt = *) 'State variable no : ', j
                    write(unit = 88, fmt = *)
                    write(unit = 88, fmt = *) 'Related variables are written below'

                    do k = 1, NDIAGVAR
                        write(unit = 88, fmt = '(a14, i2, a1, i2, a1, i2, a4, f30.15)') &
                              'PROCESS_RATES(', i, ',', j, ',', k, ') = ', PROCESS_RATES(i, j, k)
                    end do

                    close(88)
                    write(unit = *, fmt = *) 'Calculated pelagic concentration is a NaN ...'
                    write(unit = *, fmt = *) 'See the file ERROR_LOG.txt for further investigation ...'
                    stop
                end if
            end do
        end do

        !For testing the transport. If you uncomment the following line,
        !Kinetics will be shut down.
        !DERIVATIVES = 0.0D0

        ! Account for resuspension derivatives
        if (CONSIDER_RESUSPENSION > 0) then

            select case (RESUSPENSION_OPTION)

                ! Resuspension option 1
                case(1)

                    ! Loop of pegalic boxes
                    do i = 1, nkn
                        if (ACTIVATE_RESUSPENSIONS(i) > 0) then

                            ! Get the resuspension velocity (in m/days)
                            RESUSPENSION_VELOCITY = &
                                VALUE_FROM_TIME_SERIE &
                                    (RESUSPENSION_TS(RESUSPENSION_VEL_TS_NOS(i)), TIME, &
                                     RESUSPENSION_VEL_TS_VAR_NOS(i))

                            ! Get the resuspension concentration for each state variable
                            do j = 1, nstate
                                RESUSPENSION_CONCENTRATIONS(j) = &
                                    VALUE_FROM_TIME_SERIE &
                                        (RESUSPENSION_TS(RESUSPENSION_CONC_TS_NOS(i,j)), &
                                         TIME, RESUSPENSION_CONC_TS_VAR_NOS(i, j))
                            end do

                            DERIVATIVES(i,:) = DERIVATIVES(i,:) + &
                                (FRAC_RESUSPENSION_AREAS(i) * SURFACE_AREAS(i) * &
                                 RESUSPENSION_VELOCITY * RESUSPENSION_CONCENTRATIONS(:))
                        end if
                    end do
                    ! End of loop of pegalic boxes

            end select
        end if

        if (MODEL_BOTTOM_SEDIMENTS > 1) then
            ! Get sediment temperature from water column assuming it is equal
            do SED_LAYER_NO = 1,NUM_SED_LAYERS
                SED_TEMPS(:,SED_LAYER_NO) = DRIVING_FUNCTIONS(:,1)
            end do

            ! Uptate the time dependent forcings in sediments
            call UPDATE_BOTTOM_SEDIMENT_INPUTS &
                 (TIME                          , &
                  SED_DEPTHS                    , &
                  SED_POROSITIES                , &
                  SED_DENSITIES                 , &
                  PART_MIXING_COEFFS            , &
                  ADVECTIVE_VELOCITY            , &
                  SED_DIFFUSIONS                , &
                  SURF_MIXLEN                   , &
                  SED_BURRIALS                  , &
                  SED_TEMPS                     , &
                  INIT_SED_STATE_VARS(:,:,15)   , & ! Salt
                  SED_FLAGS                     , &
                  NUM_SED_FLAGS                 , &
                  nkn                           , &
                  NUM_SED_LAYERS                , &
                  NUM_SED_VARS                  , &
                  MODEL_BENTHIC_ANIMALS)

            ! Calculate the surface water concentrations. For this translation of
            ! water column state variables to bottom sediment state variables is necessary.

            ! Surface water concentrations are needed for dissolved sediment state variables
            ! and not used here. However, to avoid internal check errors, (such as division by zero)
            ! be set to their values very small concentrations.
            SURF_WATER_CONCS(:,  1)  =  STATE_VARIABLES  (:, NH4_N_INDEX)
            SURF_WATER_CONCS(:,  2)  =  STATE_VARIABLES  (:, NO3_N_INDEX)
            SURF_WATER_CONCS(:,  3)  =  STATE_VARIABLES  (:, DET_PART_ORG_N_INDEX)
            SURF_WATER_CONCS(:,  4)  =  1.0D-10
            SURF_WATER_CONCS(:,  5)  =  STATE_VARIABLES  (:, PO4_P_INDEX)
            SURF_WATER_CONCS(:,  6)  =  STATE_VARIABLES  (:, DET_PART_ORG_P_INDEX)
            SURF_WATER_CONCS(:,  7)  =  1.0D-10
            SURF_WATER_CONCS(:,  8)  =  STATE_VARIABLES  (:, DISS_OXYGEN_INDEX)
            SURF_WATER_CONCS(:,  9)  =  STATE_VARIABLES  (:, DET_PART_ORG_C_INDEX)
            SURF_WATER_CONCS(:, 10)  =  1.0D-10
            SURF_WATER_CONCS(:, 11)  =  STATE_VARIABLES  (:, DISS_Si_INDEX)
            SURF_WATER_CONCS(:, 12)  =  1.0D-10
            SURF_WATER_CONCS(:, 13)  =  STATE_VARIABLES  (:, INORG_C_INDEX)
            SURF_WATER_CONCS(:, 14)  =  STATE_VARIABLES  (:, TOT_ALK_INDEX)
            SURF_WATER_CONCS(:, 15)  =  DRIVING_FUNCTIONS(:, 2) !Salinity will come from second driving function
            SURF_WATER_CONCS(:, 16)  =  STATE_VARIABLES  (:, FE_II_INDEX)
            SURF_WATER_CONCS(:, 17)  =  STATE_VARIABLES  (:, FE_III_INDEX)
            SURF_WATER_CONCS(:, 18)  =  STATE_VARIABLES  (:, MN_II_INDEX)
            SURF_WATER_CONCS(:, 19)  =  STATE_VARIABLES  (:, MN_IV_INDEX)
            SURF_WATER_CONCS(:, 20)  =  STATE_VARIABLES  (:, CA_INDEX)
            SURF_WATER_CONCS(:, 21)  =  STATE_VARIABLES  (:, MG_INDEX)
            SURF_WATER_CONCS(:, 22)  =  STATE_VARIABLES  (:, S_PLUS_6_INDEX)
            SURF_WATER_CONCS(:, 23)  =  STATE_VARIABLES  (:, S_MINUS_2_INDEX)
            SURF_WATER_CONCS(:, 24)  =  STATE_VARIABLES  (:, CH4_C_INDEX)

            ! Assume no sediment erosion in this simple example
            H_ERODEP(:) = 0.0D0

            ! Get sediment transport
            DISSOLVED_FRACTIONS    = EFFECTIVE_DISSLOVED_FRACTIONS
            FRACTION_OF_DEPOSITION = EFFECTIVE_DEPOSITION_FRACTIONS

            call FLX_ALUKAS_II_TO_SED_MOD_1_VEC &
               (STATE_VARIABLES    , nkn        , nstate, &
                MODEL_CONSTANTS    , nconst     ,         &
                DRIVING_FUNCTIONS  , n_driving_functions, &
                SETTLING_VELOCITIES, DISSOLVED_FRACTIONS, &
                1.0D0              , 1, TIME            , &
                SETTLING_RATES     , FLUXES_TO_SEDIMENTS, &
                NUM_SED_VARS                            , &
                1 , FRACTION_OF_DEPOSITION              , &
                NOT_DEPOSITED_FLUXES, nstate            , &
                CONSIDER_NON_OBLIGATORY_FIXERS          , &
                CONSIDER_NOSTOCALES)

            ! For now 1, will depend on current or wave submodel in the future
            NUM_FLUX_RECEIVING_SED_LAYERS = 1

            !Call sediment submodel
            call AQUABC_SEDIMENT_MODEL_1 &
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
                 TIME, TIME_STEP                                     ,  &
                 H_ERODEP                                            ,  &
                 FINAL_SED_STATE_VARS                                ,  &
                 FLUXES_FROM_SEDIMENTS, NUM_FLUXES_FROM_SEDIMENTS    ,  &
                 PROCESSES_sed        , NDIAGVAR_sed                 ,  &
                 SED_OUTPUTS          , NUM_SED_OUTPUTS              ,  &
                 SED_SAVED_OUTPUTS    , NUM_SED_SAVED_OUTPUTS        ,  &
                 SED_BURRIAL_RATE_OUTPUTS, BOTTOM_SED_ADVANCED_REDOX_SIMULATION)

            where (FINAL_SED_STATE_VARS <= 0.0D0)
                FINAL_SED_STATE_VARS(:, :, :) = 0.0D0
            end where

            call FLX_SED_MOD_1_TO_ALUKAS_II_VEC &
                 (FLUXES_FROM_SEDIMENTS , NUM_FLUXES_FROM_SEDIMENTS, &
                  FLUXES_TO_WATER_COLUMN, nkn, nstate)

            ! NOT_DEPOSITED_FLUXES
            ! --------------------
            ! Unit        : g/m^2/day
            !
            ! Description : Portion of settling fluxes that settled towards sediment but
            !               not deposited there.  Mass related to it will be eventually
            !               reflected back to water column. This simple example skips
            !               the sediment - water coulmn boundary layer processes and
            !               assumes that not deposited fluxes are reflexted back to
            !               water column immidiately. These fluxes must be divided by
            !               water column depth to be converted from g/m^2/day to g/m^3/day

            ! SETTLING_RATES
            ! --------------------
            ! Unit        : g/m^2/day
            !
            ! Description : Portion of settling fluxes that settled towards sediment
            !               They must be withdrawn from water column in order to have correct
            !               mass balamce. These fluxes must be divided by
            !               water column depth to be converted from g/m^2/day to g/m^3/day

            ! FLUXES_TO_WATER_COLUMN
            ! Unit        : g/m^2/day
            !
            ! Description : Fluxes from sediments (positive or negative) to water column.
            !              These fluxes must be divided by
            !               water column depth to be converted from g/m^2/day to g/m^3/day

            ! DRIVING_FUNCTIONS(:, 8) is water column depth

            FLUXES_OUTPUT_TO_WATER_COLUMN(:,:) = FLUXES_TO_WATER_COLUMN(:,:)

            do STATE_VAR_NO = 1, nstate
                FLUXES_TO_WATER_COLUMN(:,STATE_VAR_NO) = &
                    FLUXES_TO_WATER_COLUMN(:,STATE_VAR_NO) / DRIVING_FUNCTIONS(:, 8)

                NOT_DEPOSITED_FLUXES  (:,STATE_VAR_NO) = &
                    NOT_DEPOSITED_FLUXES  (:,STATE_VAR_NO) / DRIVING_FUNCTIONS(:, 8)

                SETTLING_RATES        (:,STATE_VAR_NO) = &
                    SETTLING_RATES        (:,STATE_VAR_NO) / DRIVING_FUNCTIONS(:, 8)
            end do
        end if

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                PROCESS_RATE_BEGIN_NO = ((j - 1) * NDIAGVAR) + 1
                PROCESS_RATE_END_NO   = j * NDIAGVAR

                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                    PROCESS_RATES(PROCESS_RATE_BEGIN_NO:PROCESS_RATE_END_NO) = &
                PROCESS_RATES(i,j,:)
            end do

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                SAVED_OUTPUTS = SAVED_OUTPUTS(i,:)

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                PELAGIC_ECOLOGY_CALLED_BEFORE = CALLED_BEFORE

            PELAGIC_BOX_MODEL_DATA % ECOL_KINETIC_DERIVS(i, :, DERIV_NO) = &
                (DERIVATIVES(i,:) + FLUXES_TO_WATER_COLUMN(i,:)) * &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME
        end do

    end subroutine CALC_DERIV

end module PELAGIC_SOLVER
