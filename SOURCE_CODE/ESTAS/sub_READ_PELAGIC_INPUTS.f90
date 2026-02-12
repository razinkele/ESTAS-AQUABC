subroutine READ_PELAGIC_BOX_MODEL_INPUTS(PELAGIC_BOX_MODEL_DATA, IN_FILE, OUT_FILE)

    use PELAGIC_BOX_MODEL
    implicit none
    ! Modules GLOBAL, TIME_SERIES, PELAGIC_BOX, PELAGIC_LINK, EXTERNAL_FORCING
    ! INITIAL_CONDITIONS, BASIN, UTILS_1, COST_FUNCTION are accessed though
    ! the use of the PELAGIC_BOX_MODEL

    type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA
    integer, intent(in) :: IN_FILE
    integer, intent(in) :: OUT_FILE

    integer :: i
    integer :: j

    character(len = 2048) :: FILE_NAME
    character(len = 2048) :: constants_file
    character(len = 2048) :: constants_path
    logical :: exists

    integer :: NUM_PELAGIC_STATE_VARS
    integer :: NUM_MODEL_CONSTANTS
    integer :: NUM_BASINS
    integer :: NUM_BATHYMETRIES
    integer :: NUM_PELAGIC_BOXES
    integer :: NUM_PELAGIC_INIT_COND_SETS
    integer :: NUM_PELAGIC_ADVECTIVE_LINKS
    integer :: NUM_PELAGIC_DISPERSIVE_LINKS
    integer :: NUM_FLOW_TS
    integer :: NUM_MIXING_TS
    integer :: NUM_SETTLING_VELOCITIES
    integer :: NUM_OPEN_BOUNDARIES
    integer :: NUM_FORCING_TS

    integer :: NUM_MASS_LOADS
    integer :: NUM_MASS_WITHDRAWALS
    integer :: NUM_MASS_LOADS_OF_BOX
    integer :: NUM_MASS_WITHDRAWALS_OF_BOX

    integer :: BASIN_NO
    integer :: BATHYMETRY_NO
    integer :: NUM_BOXES
    integer :: BOX_NO
    integer :: PELAGIC_INIT_COND_SET_NO
    integer :: ADVECTIVE_LINK_NO
    integer :: UPSTREAM_BOX_NO
    integer :: DOWNSTREAM_BOX_NO
    integer :: FLOW_TS_NO
    integer :: FLOW_TS_VAR_NO

    integer :: DISPERSIVE_LINK_NO
    integer :: FIRST_BOX_NO
    integer :: SECOND_BOX_NO
    integer :: MIXING_TS_NO

    real(kind = SELECTED_REAL_KIND(15,307)) :: MIXING_LENGTH
    real(kind = SELECTED_REAL_KIND(15,307)) :: SURFACE_ELEVATION
    real(kind = SELECTED_REAL_KIND(15,307)) :: BOTTOM_ELEVATION
    real(kind = SELECTED_REAL_KIND(15,307)) :: DISSOLVED_FRACTION
    real(kind = SELECTED_REAL_KIND(15,307)) :: DEPOSITED_FRACTION
    integer :: CHLA_SUPRESSION_OF_SETTLING

    integer :: SETTLING_VELOCITY_NO

    integer :: OPEN_BOUNDARY_NO
    integer :: MASS_LOAD_NO
    integer :: MASS_WITHDRAWAL_NO
    integer :: PELAGIC_STATE_VAR_NO
    integer :: FORCING_TS_NO
    integer :: FORCING_TS_VAR_NO

    integer :: STATE_VAR_NO
    character(len = 30) :: STATE_VAR_NAME
    integer :: NUM_BOXES_WITH_MEASURED_VALS
    integer :: AUX_INT

    integer :: PRODUCE_PEL_STATE_VAR_OUTPUT
    integer :: PRODUCE_PEL_PROCESS_RATE_OUTPUT
    integer :: PRODUCE_PEL_MASS_BALANCE_OUTPUT

    character(len = 20) :: FORMAT_STRING
    character(len = 5)  :: INTEGER_STRING
    character(len = 5)  :: BOX_NO_STRING

    integer          :: AUX_INTEGER_1, AUX_INTEGER_2
    real(kind = DBL) :: AUX_DOUBLE_1

    real(kind = DBL) :: TIME

    !READ DESCRIPTION LINES
    do i = 1, 5
        read(IN_FILE, *)
    end do

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_PELAGIC_STATE_VARS
    write(*,*) 'Number of pelagic state variables : ', NUM_PELAGIC_STATE_VARS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_MODEL_CONSTANTS
    write(*,*) 'Number of model constants         : ', NUM_MODEL_CONSTANTS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_BASINS
    write(*,*) 'Number basins                     : ', NUM_BASINS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_BATHYMETRIES
    write(*,*) 'Number of bathymetries            : ', NUM_BATHYMETRIES

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_PELAGIC_BOXES
    write(*,*) 'Number of pelagic boxes           : ', NUM_PELAGIC_BOXES

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_PELAGIC_INIT_COND_SETS
    write(*,*) 'Number of initial conditions      : ', NUM_PELAGIC_INIT_COND_SETS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_PELAGIC_ADVECTIVE_LINKS
    write(*,*) 'Number of advective links         : ', NUM_PELAGIC_ADVECTIVE_LINKS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_PELAGIC_DISPERSIVE_LINKS
    write(*,*) 'Number of diffusion links         : ', NUM_PELAGIC_DISPERSIVE_LINKS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_FLOW_TS
    write(*,*) 'Number of flow time series        : ', NUM_FLOW_TS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_MIXING_TS
    write(*,*) 'Number of mixing time series      : ', NUM_MIXING_TS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_SETTLING_VELOCITIES
    write(*,*) 'Number of settling velocities     : ', NUM_SETTLING_VELOCITIES

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_OPEN_BOUNDARIES
    write(*,*) 'Number of open boundries          : ', NUM_OPEN_BOUNDARIES

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_MASS_LOADS
    write(*,*) 'Number of mass loads              : ', NUM_MASS_LOADS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_MASS_WITHDRAWALS
    write(*,*) 'Number of mass withdrawals        : ', NUM_MASS_WITHDRAWALS

    read(IN_FILE, *)
    read(IN_FILE, *) NUM_FORCING_TS
    write(*,*) 'Number of forcing time series     : ', NUM_FORCING_TS

    call ALLOC_PELAGIC_BOX_MODEL_DATA(PELAGIC_BOX_MODEL_DATA,  &
                NUM_PELAGIC_STATE_VARS      , NUM_MODEL_CONSTANTS         , &
                NUM_BASINS                  , NUM_BATHYMETRIES            , &
                NUM_PELAGIC_BOXES           , NUM_PELAGIC_INIT_COND_SETS  , &
                NUM_PELAGIC_ADVECTIVE_LINKS , NUM_PELAGIC_DISPERSIVE_LINKS, &
                NUM_SETTLING_VELOCITIES     , NUM_OPEN_BOUNDARIES         , &
                NUM_FLOW_TS                 , NUM_MIXING_TS               , &
                NUM_MASS_LOADS              , NUM_MASS_WITHDRAWALS        , &
                NUM_FORCING_TS)

    ! -------------------------------------------------------------------------------
    ! Code reading pelagic model options
    ! -------------------------------------------------------------------------------
    read(IN_FILE, *)
    read(IN_FILE, *) FILE_NAME

    open(unit   = IN_FILE + 1, &
         file   = trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
         status = 'OLD')

    call READ_PELAGIC_MODEL_OPTIONS(IN_FILE)

    close(IN_FILE + 1)
    ! -------------------------------------------------------------------------------
    ! End of code reading pelagic model options
    ! -------------------------------------------------------------------------------


    ! READ THE MODEL OUTPUT OPTION
    read(IN_FILE, *)
    read(IN_FILE, *) FILE_NAME

    open(unit   = IN_FILE + 1, &
         file   = trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
         status = 'OLD')

    read(IN_FILE + 1, *)

    do i = 1,  NUM_PELAGIC_BOXES
        read(IN_FILE + 1, *) &
            BOX_NO, PRODUCE_PEL_STATE_VAR_OUTPUT, PRODUCE_PEL_PROCESS_RATE_OUTPUT, &
            PRODUCE_PEL_MASS_BALANCE_OUTPUT

        PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_STATE_VAR_OUTPUTS   (BOX_NO) = &
            PRODUCE_PEL_STATE_VAR_OUTPUT

        PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_PROCESS_RATE_OUTPUTS(BOX_NO) = &
            PRODUCE_PEL_PROCESS_RATE_OUTPUT

        PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_MASS_BALANCE_OUTPUTS(BOX_NO) = &
            PRODUCE_PEL_MASS_BALANCE_OUTPUT
    end do

    close(IN_FILE + 1)

    read(IN_FILE, *)
    read(IN_FILE, *) PEL_PROCESS_RATE_OUTPUT_OPTION

    if ((PEL_PROCESS_RATE_OUTPUT_OPTION < 1).or. &
        (PEL_PROCESS_RATE_OUTPUT_OPTION > 2)) then
        write(*,*) 'Wrong option was entered for the pelagic process output option'
        write(*,*) 'Enter 1 for the volume based output - g/m^3/day'
        write(*,*) 'Enter 2 for the area   based output - g/m^2/day'
        stop
    else
        if (PEL_PROCESS_RATE_OUTPUT_OPTION.eq.1) then
            write(*,*) 'Process rates will be written in g/m^3/day'
        else
            write(*,*) 'Process rates will be written in g/m^2/day'
        end if
    end if

    !READ PELAGIC STATE VAR INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do  i = 1, NUM_PELAGIC_STATE_VARS
        read(IN_FILE, *) &
             STATE_VAR_NO, STATE_VAR_NAME, NUM_BOXES_WITH_MEASURED_VALS, &
             AUX_DOUBLE_1, AUX_INTEGER_1 , AUX_INTEGER_2

        PELAGIC_BOX_MODEL_DATA % PELAGIC_STATE_VAR_NAMES(STATE_VAR_NO) = &
            trim(adjustl(STATE_VAR_NAME))

        PELAGIC_BOX_MODEL_DATA % MEASUREMENT_ERRORS               &
            (STATE_VAR_NO) = AUX_DOUBLE_1

        PELAGIC_BOX_MODEL_DATA % STATE_VAR_OUTPUT_TYPES           &
            (STATE_VAR_NO) = AUX_INTEGER_1

        PELAGIC_BOX_MODEL_DATA % STATE_VAR_INITIAL_CONDITION_TYPES&
            (STATE_VAR_NO) = AUX_INTEGER_2

        call ALLOC_MEASURED_VALUE &
             (PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(STATE_VAR_NO), &
              NUM_BOXES_WITH_MEASURED_VALS)

    end do

    !READ PELAGIC MODEL CONSTANTS INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_BOXES
        read(IN_FILE, *) AUX_INT, FILE_NAME

        ! Determine which constants file to use and fallback to WCONST_04.txt

        if (USE_PELAGIC_CONSTANTS_FILE_NAME > 0) then
            constants_file = trim(adjustl(PELAGIC_CONSTANTS_FILE_NAME))
        else
            constants_file = trim(adjustl(FILE_NAME))
        end if

        constants_path = trim(adjustl(PELAGIC_INPUT_FOLDER)) // constants_file
        inquire(file=trim(constants_path), exist=exists)
        if (.not. exists) then
            write(*,*) 'Warning: constants file "', trim(constants_file), '" not found. Falling back to WCONST_04.txt'
            constants_file = 'WCONST_04.txt'
            constants_path = trim(adjustl(PELAGIC_INPUT_FOLDER)) // constants_file
            inquire(file=trim(constants_path), exist=exists)
            if (.not. exists) then
                write(*,*) 'Error: fallback constants file "WCONST_04.txt" not found in ', trim(adjustl(PELAGIC_INPUT_FOLDER))
                stop 'Missing constants file'
            end if
        end if

        open(unit   = IN_FILE + 1, &
             file   = trim(constants_path), &
             status = 'OLD')

        call READ_MODEL_CONSTANTS &
             (PELAGIC_BOX_MODEL_DATA % MODEL_CONSTANTS(:, i), IN_FILE + 1)

        close(IN_FILE + 1)
    end do


    ! -------------------------------------------------------------------------------
    ! Code reading extra model constant information
    ! -------------------------------------------------------------------------------
    read(IN_FILE, *)
    read(IN_FILE, *) FILE_NAME

    open(unit   = IN_FILE + 1, &
         file   = trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
         status = 'OLD')

    call READ_EXTRA_PELAGIC_MODEL_CONSTS(IN_FILE)

    close(IN_FILE + 1)
    ! -------------------------------------------------------------------------------
    ! End of code reading extra model constant information
    ! -------------------------------------------------------------------------------

    !READ PELAGIC BOX INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_BOXES
        read(IN_FILE, *) &
            BOX_NO, NUM_MASS_LOADS_OF_BOX, NUM_MASS_WITHDRAWALS_OF_BOX

        call ALLOC_TOPLOGY_AND_LOADS &
             (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO), &
              NUM_MASS_LOADS_OF_BOX, NUM_MASS_WITHDRAWALS_OF_BOX)
    end do

    !READ PELAGIC BASIN INFORMATION
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_BASINS

        read(IN_FILE, *)
        read(IN_FILE, *) BASIN_NO, NUM_BOXES, BATHYMETRY_NO

        call ALLOCATE_BASIN_DATA &
                 (PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO), NUM_BOXES)

        read(IN_FILE, *)

        do j = 1, NUM_BOXES
            read(IN_FILE, *) BOX_NO

            PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % &
                PELAGIC_BOXES(j)   = BOX_NO

            PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % &
                BATHYMETRY_NO      = BATHYMETRY_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                BASIN_NO      = BASIN_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                BATHYMETRY_NO = BATHYMETRY_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % BASIN_BOX_NO  = j
        end do

    end do

    !READ PELAGIC BATHYMETRY INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_BATHYMETRIES

        read(IN_FILE, *) BATHYMETRY_NO, FILE_NAME

        open(unit   = IN_FILE + 1, &
             file   = trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
             status = 'OLD')

        call READ_BATHYMETRY_DATA_FROM_FILE &
             (PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(BATHYMETRY_NO), IN_FILE + 1)

        PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(BATHYMETRY_NO) % ID_NUM = BATHYMETRY_NO

        close(IN_FILE + 1)
    end do

    !READ INITIAL CONDITIONS
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_BOXES

        read(IN_FILE, *) &
             BOX_NO, PELAGIC_INIT_COND_SET_NO, SURFACE_ELEVATION, BOTTOM_ELEVATION

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            PELAGIC_INIT_COND_SET_NO = PELAGIC_INIT_COND_SET_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            SURFACE_ELEVATION        = SURFACE_ELEVATION

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            BOTTOM_ELEVATION         = BOTTOM_ELEVATION

        BATHYMETRY_NO = &
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % BATHYMETRY_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % VOLUME = &
            CALCULATE_VOLUME_BETWEEN_LEVELS &
                (PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(BATHYMETRY_NO), &
                 SURFACE_ELEVATION, BOTTOM_ELEVATION)

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % DEPTH = &
            SURFACE_ELEVATION - BOTTOM_ELEVATION

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % SURFACE_AREA = &
                CALCULATE_SURFACE_AREA &
                   (PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(BATHYMETRY_NO), &
                    SURFACE_ELEVATION)
    end do

    !READ LOAD INFORMATION : MASS LOADS
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_BOXES
        read(IN_FILE, *)

        NUM_MASS_LOADS_OF_BOX = &
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % NUM_MASS_LOADS

        do j = 1, NUM_MASS_LOADS_OF_BOX
            read(unit = IN_FILE, fmt = *) &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % MASS_LOAD_NOS(j)
        end do
    end do

    !READ LOAD INFORMATION : MASS WITHDRAWALS
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_BOXES
        read(IN_FILE, *)

        NUM_MASS_WITHDRAWALS_OF_BOX = &
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % NUM_MASS_WITHDRAWALS

        do j = 1, NUM_MASS_WITHDRAWALS_OF_BOX
            read(unit = IN_FILE, fmt = *) &
                 PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % MASS_WITHDRAWAL_NOS(j)
        end do
    end do

    !READ ADVECTIVE LINK INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_ADVECTIVE_LINKS

        read(IN_FILE, *) ADVECTIVE_LINK_NO, UPSTREAM_BOX_NO, &
                         DOWNSTREAM_BOX_NO, FLOW_TS_NO, FLOW_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % &
            ADVECTIVE_LINK_NO = ADVECTIVE_LINK_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % &
            UPSTREAM_BOX_NO   = UPSTREAM_BOX_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % &
            DOWNSTREAM_BOX_NO = DOWNSTREAM_BOX_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % &
            FLOW_TS_NO        = FLOW_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS(i) % &
            FLOW_TS_VAR_NO    = FLOW_TS_VAR_NO
    end do

    !READ DISPERSIVE LINK INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_DISPERSIVE_LINKS

        read(IN_FILE, *) DISPERSIVE_LINK_NO, FIRST_BOX_NO, SECOND_BOX_NO, &
                         MIXING_TS_NO, MIXING_LENGTH

        PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % &
            DISPERSIVE_LINK_NO = DISPERSIVE_LINK_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % &
            FIRST_BOX_NO       = FIRST_BOX_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % &
            SECOND_BOX_NO      = SECOND_BOX_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % &
            MIXING_TS_NO       = MIXING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS(i) % &
            MIXING_LENGTH      = MIXING_LENGTH
    end do

    !READ SETTLING INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_BOXES * NUM_PELAGIC_STATE_VARS

        read(IN_FILE, *) &
            BOX_NO, PELAGIC_STATE_VAR_NO, DISSOLVED_FRACTION, &
            SETTLING_VELOCITY_NO, DEPOSITED_FRACTION, CHLA_SUPRESSION_OF_SETTLING

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            DISSOLVED_FRACTIONS(PELAGIC_STATE_VAR_NO) = DISSOLVED_FRACTION

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            SETTLING_TS_NOS    (PELAGIC_STATE_VAR_NO) = SETTLING_VELOCITY_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            DEPOSITED_FRACTIONS(PELAGIC_STATE_VAR_NO) = DEPOSITED_FRACTION

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            CHLA_SUPRESSION_OF_SETTLING(PELAGIC_STATE_VAR_NO) = &
        CHLA_SUPRESSION_OF_SETTLING
    end do

    !READ OPEN BOUNDARY INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_OPEN_BOUNDARIES * NUM_PELAGIC_STATE_VARS

        read(IN_FILE, *) &
            OPEN_BOUNDARY_NO, PELAGIC_STATE_VAR_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % OPEN_BOUNDARIES(OPEN_BOUNDARY_NO) % &
            FORCING_TS_NOS    (PELAGIC_STATE_VAR_NO) = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % OPEN_BOUNDARIES(OPEN_BOUNDARY_NO) % &
            FORCING_TS_VAR_NOS(PELAGIC_STATE_VAR_NO) = FORCING_TS_VAR_NO
    end do

    !READ MASS LOAD INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_MASS_LOADS * NUM_PELAGIC_STATE_VARS

        read(IN_FILE, *) &
            MASS_LOAD_NO , PELAGIC_STATE_VAR_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % MASS_LOADS(MASS_LOAD_NO) % &
            FORCING_TS_NOS    (PELAGIC_STATE_VAR_NO)     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % MASS_LOADS(MASS_LOAD_NO) % &
            FORCING_TS_VAR_NOS(PELAGIC_STATE_VAR_NO) = FORCING_TS_VAR_NO
    end do

    !READ MASS WITHDRAWAL INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_MASS_WITHDRAWALS * NUM_PELAGIC_STATE_VARS

        read(IN_FILE, *) MASS_WITHDRAWAL_NO, PELAGIC_STATE_VAR_NO, &
                         FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % MASS_WITHDRAWALS(MASS_WITHDRAWAL_NO) % &
                FORCING_TS_NOS(PELAGIC_STATE_VAR_NO)     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % MASS_WITHDRAWALS(MASS_WITHDRAWAL_NO) % &
                FORCING_TS_VAR_NOS(PELAGIC_STATE_VAR_NO) = FORCING_TS_VAR_NO
    end do

    !READ TEMPERATURE INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            TEMPERATURE_TS_NO     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            TEMPERATURE_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ SALINITY INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            SALINITY_TS_NO    = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            SALINITY_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ SOLAR RADIATION INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            SOLAR_RADIATION_TS_NO     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            SOLAR_RADIATION_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ FRACTION OF DAY INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            FRACTION_OF_DAY_TS_NO     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            FRACTION_OF_DAY_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ AIR TEMPERATURE INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            AIR_TEMPERATURE_TS_NO     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            AIR_TEMPERATURE_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ WIND SPEED INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            WIND_SPEED_TS_NO     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            WIND_SPEED_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ PRECIPITATION INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            PRECIPITATION_TS_NO     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            PRECIPITATION_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ EVAPORATION INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            EVAPORATION_TS_NO     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            EVAPORATION_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ ICE FRACTION INFORMATION
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        read(IN_FILE, *) BOX_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            ICE_FRACTION_TS_NO     = FORCING_TS_NO

        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
            ICE_FRACTION_TS_VAR_NO = FORCING_TS_VAR_NO
    end do

    !READ INITIAL CONDITIONS
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_PELAGIC_INIT_COND_SETS
        read(unit = IN_FILE, fmt = *) PELAGIC_INIT_COND_SET_NO, FILE_NAME

        open(unit   = IN_FILE + 1, &
             file   = trim(adjustl(PELAGIC_INPUT_FOLDER))//trim(adjustl(FILE_NAME)),&
             status = 'OLD')

        call READ_INITIAL_CONCENTRATIONS &
             (PELAGIC_BOX_MODEL_DATA % &
                  PELAGIC_INIT_COND_SETS(PELAGIC_INIT_COND_SET_NO), IN_FILE + 1)

        close(IN_FILE + 1)
    end do

    !READ FLOW TIME SERIES
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_FLOW_TS
        read(unit = IN_FILE, fmt = *) FLOW_TS_NO, FILE_NAME

        open(unit   = IN_FILE + 1, &
             file   = trim(adjustl(PELAGIC_INPUT_FOLDER))//trim(adjustl(FILE_NAME)),&
             status = 'OLD')

        call INITIALIZE_TIME_SERIE(PELAGIC_BOX_MODEL_DATA % FLOWS(FLOW_TS_NO))

        call READ_TIME_SERIE_FROM_FILE &
                 (PELAGIC_BOX_MODEL_DATA % FLOWS(FLOW_TS_NO), IN_FILE + 1)

        close(IN_FILE + 1)
    end do

    !READ MIXING TIME SERIES
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_MIXING_TS
        read(unit = IN_FILE, fmt = *) MIXING_TS_NO, FILE_NAME

        open(unit   = IN_FILE + 1, &
             file   = trim(adjustl(PELAGIC_INPUT_FOLDER))//trim(adjustl(FILE_NAME)),&
             status = 'OLD')

        call INITIALIZE_TIME_SERIE &
                 (PELAGIC_BOX_MODEL_DATA % MIXING_EXCHANGES(MIXING_TS_NO))

        call READ_TIME_SERIE_FROM_FILE &
                 (PELAGIC_BOX_MODEL_DATA % MIXING_EXCHANGES(MIXING_TS_NO), &
                  IN_FILE + 1)

        close(IN_FILE + 1)
    end do

    !READ SETTLING VELOCITY TIME SERIES
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_SETTLING_VELOCITIES
        read(unit = IN_FILE, fmt = *) SETTLING_VELOCITY_NO, FILE_NAME

        open(unit   = IN_FILE + 1, &
             file   = trim(adjustl(PELAGIC_INPUT_FOLDER))//trim(adjustl(FILE_NAME)),&
             status = 'OLD')

        call INITIALIZE_TIME_SERIE &
                 (PELAGIC_BOX_MODEL_DATA % SETTLING_VELOCITIES(SETTLING_VELOCITY_NO))

        call READ_TIME_SERIE_FROM_FILE &
                 (PELAGIC_BOX_MODEL_DATA % &
                     SETTLING_VELOCITIES(SETTLING_VELOCITY_NO), IN_FILE + 1)

        close(IN_FILE + 1)
    end do

    !READ FORCING TIME SERIES
    read(IN_FILE, *)
    read(IN_FILE, *)

    do i = 1, NUM_FORCING_TS
        read(unit = IN_FILE, fmt = *) FORCING_TS_NO, FILE_NAME

        open(unit   = IN_FILE + 1, &
             file   = trim(adjustl(PELAGIC_INPUT_FOLDER))//trim(adjustl(FILE_NAME)),&
             status = 'OLD')

        call INITIALIZE_TIME_SERIE &
             (PELAGIC_BOX_MODEL_DATA % FORCING_TS(FORCING_TS_NO))

        call READ_TIME_SERIE_FROM_FILE &
             (PELAGIC_BOX_MODEL_DATA % FORCING_TS(FORCING_TS_NO), &
              IN_FILE + 1)

        close(IN_FILE + 1)
    end do

    !READ PELAGIC OUTPUT INFORMARION
    PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_ECOL_OUTPUT = 0

    read(unit = IN_FILE, fmt = *)
    read(unit = IN_FILE, fmt = *)

    read(unit = IN_FILE, fmt = *) &
         PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_ECOL_OUTPUT

    read(unit = IN_FILE, fmt = *)
    read(unit = IN_FILE, fmt = *)

    read(unit = IN_FILE, fmt = *) &
         PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_SAVED_OUTPUT

    read(unit = IN_FILE, fmt = *)
    read(unit = IN_FILE, fmt = *)

    read(unit = IN_FILE, fmt = *) &
         PELAGIC_BOX_MODEL_DATA % CREATE_STATE_VARIABLE_OUTPUT

    read(unit = IN_FILE, fmt = *)
    read(unit = IN_FILE, fmt = *)
    read(unit = IN_FILE, fmt = *) PRODUCE_COCOA_OUTPUTS

    if (PRODUCE_COCOA_OUTPUTS > 0) then
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) COCOA_PELAGIC_OUTPUTS_FILENAME
    end if

    !READ PELAGIC EXERGY OUTPUT INFORMATION
    PELAGIC_BOX_MODEL_DATA % CALCULATE_PELAGIC_EXERGY      = 0
    PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_EXERGY_OUTPUTS = 0
    PELAGIC_BOX_MODEL_DATA % START_REPEAT_NO_PEL_EX_OUTS   = 0

    read(unit = IN_FILE, fmt = *)
    read(unit = IN_FILE, fmt = *)

    read(unit = IN_FILE, fmt = *) &
         PELAGIC_BOX_MODEL_DATA % CALCULATE_PELAGIC_EXERGY

    if (PELAGIC_BOX_MODEL_DATA % CALCULATE_PELAGIC_EXERGY > 0) then
        read(unit = IN_FILE, fmt = *)

        read(unit = IN_FILE, fmt = *) &
             PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_EXERGY_OUTPUTS

        if (PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_EXERGY_OUTPUTS > 0) then
            read(unit = IN_FILE, fmt = *)

            read(unit = IN_FILE, fmt = *) &
                 PELAGIC_BOX_MODEL_DATA % START_REPEAT_NO_PEL_EX_OUTS
        end if

    end if

    !READ THE COST FUNCTION INFORMATION
    read(unit = IN_FILE, fmt = *)
    read(unit = IN_FILE, fmt = *)

    read(unit = IN_FILE, fmt = *) &
         PELAGIC_BOX_MODEL_DATA % PRODUCE_COST_FUNCTION

    if (PELAGIC_BOX_MODEL_DATA % PRODUCE_COST_FUNCTION > 0) then

        read(unit = IN_FILE, fmt = *)

        do i = 1, NUM_PELAGIC_STATE_VARS
            read(unit = IN_FILE, fmt = *)
            read(unit = IN_FILE, fmt = *)

            do j = 1, PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i) % NUM_BOXES

                read(unit = IN_FILE, fmt = *) &
                     PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i) % &
                         BOX_NOS           (j), &
                     PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i) % &
                         FORCING_TS_NOS    (j), &
                     PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i) % &
                        FORCING_TS_VAR_NOS(j)
            end do
        end do
    end if

    !INITIALIZE THE OUTPUT FILES
    TIME                   = INIT_TIME
    NUM_BOXES              = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
    NUM_PELAGIC_STATE_VARS = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

    if (PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_ECOL_OUTPUT > 0) then

        if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT == 0) then

            !Pelagic state variables output files
            if (PELAGIC_BOX_MODEL_DATA % CREATE_STATE_VARIABLE_OUTPUT > 0) then
                do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                    open(unit = AUX_OUTPUT_UNIT, &
                         file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                                trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                             PELAGIC_STATE_VAR_NAMES(i))) &
                                // '.out', status = 'UNKNOWN')

                    FORMAT_STRING = '(a20, '
                    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_BOXES
                    FORMAT_STRING = '(a20, ' // INTEGER_STRING // 'i20)'

                    write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) &
                          '         TIME_DAYS', (j, j = 1, NUM_BOXES)

                    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_BOXES + 1
                    FORMAT_STRING = '(' // INTEGER_STRING // 'f20.6)'

                    write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) TIME, &
                          (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(j) % &
                           CONCENTRATIONS(i), j = 1, NUM_BOXES)

                    close(AUX_OUTPUT_UNIT)
                end do
            end if

            !Pelagic derived variables output files
            if (PELAGIC_BOX_MODEL_DATA % CREATE_STATE_VARIABLE_OUTPUT > 0) then
                do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                    open(unit = AUX_OUTPUT_UNIT, &
                         file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                                trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                             PELAGIC_STATE_VAR_NAMES(i))) &
                                // '.out', status = 'UNKNOWN')

                    FORMAT_STRING = '(a20, '
                    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_BOXES
                    FORMAT_STRING = '(a20, ' // INTEGER_STRING // 'i20)'

                    write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) &
                          '         TIME_DAYS', (j, j = 1, NUM_BOXES)

                    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_BOXES + 1
                    FORMAT_STRING = '(' // INTEGER_STRING // 'f20.6)'

                    write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) TIME, &
                          (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(j) % &
                           CONCENTRATIONS(i), j = 1, NUM_BOXES)

                    close(AUX_OUTPUT_UNIT)
                end do
            end if

            !Pelagic box files
            do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
                if (PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_STATE_VAR_OUTPUTS(i) > 0) then

                    PELAGIC_INIT_COND_SET_NO = &
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % PELAGIC_INIT_COND_SET_NO

                    do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j) = &
                            PELAGIC_BOX_MODEL_DATA % &
                                PELAGIC_INIT_COND_SETS(PELAGIC_INIT_COND_SET_NO) % &
                                PELAGIC_CONCENTRATIONS(j)
                    end do

                    write(unit = BOX_NO_STRING, fmt = '(i5.5)') i

                    open(unit   = AUX_OUTPUT_UNIT, &
                         file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                                  trim(adjustl('PELAGIC_BOX_' // BOX_NO_STRING // '.out')), &
                         status = 'UNKNOWN')

                    FORMAT_STRING = '(a20, '
                    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_PELAGIC_STATE_VARS
                    FORMAT_STRING = '(a20, ' // INTEGER_STRING // 'a20)'

                    write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) '         TIME_DAYS', &
                         (trim(adjustl(PELAGIC_BOX_MODEL_DATA % PELAGIC_STATE_VAR_NAMES(j))), &
                                       j = 1, NUM_PELAGIC_STATE_VARS)

                    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_PELAGIC_STATE_VARS + 1
                    FORMAT_STRING = '(' // INTEGER_STRING // 'f20.6)'

                    write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) INIT_TIME, &
                          (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j), &
                          j = 1, NUM_PELAGIC_STATE_VARS)

                    close(AUX_OUTPUT_UNIT)

                    write(unit = BOX_NO_STRING, fmt = '(i5.5)') i

                    open(unit = AUX_OUTPUT_UNIT, &
                         file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                                trim(adjustl('PELAGIC_BOX_' // BOX_NO_STRING // &
                                             '.mtrx')), status = 'UNKNOWN')

                    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_PELAGIC_STATE_VARS + 1
                    FORMAT_STRING = '(' // INTEGER_STRING // 'f20.6)'

                    write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) INIT_TIME, &
                          (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j), &
                           j = 1, NUM_PELAGIC_STATE_VARS)

                    close(AUX_OUTPUT_UNIT)
                end if
            end do
        end if
    end if

    FORMAT_STRING = '(a20, '
    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_BOXES
    FORMAT_STRING = '(a20, ' // INTEGER_STRING // 'i20)'

    !Pelagic saved output files
    if (PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_SAVED_OUTPUT == 1) then
        if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT == 0) then
            do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_SAVED_OUTPUTS

                open(unit = AUX_OUTPUT_UNIT, &
                     file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                            trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                             SAVED_OUTPUT_NAMES(i))) // '.out', &
                     status = 'UNKNOWN')

                write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) &
                      '         TIME_DAYS', (j, j = 1, NUM_BOXES)

                close(AUX_OUTPUT_UNIT)
            end do
        end if
    end if

    !Pelagic exergy output files
    if (PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_EXERGY_OUTPUTS == 1) then
        if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT == 0) then
            do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_EXERGY_COMPONENTS

                open(unit  = AUX_OUTPUT_UNIT, &
                     file  = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                             trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                             EXERGY_COMPONENT_NAMES(i)))// '.out',  &
                     status = 'UNKNOWN')

                write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING) &
                      '         TIME_DAYS', (j, j = 1, NUM_BOXES)

                close(AUX_OUTPUT_UNIT)
            end do
        end if
    end if
end subroutine READ_PELAGIC_BOX_MODEL_INPUTS
