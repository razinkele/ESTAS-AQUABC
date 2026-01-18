module SIMULATE

    use GLOBAL
    use AQUATIC_MODEL
    use PELAGIC_SOLVER
    use PELAGIC_EXERGY
    use TIME_SERIES
    use UTILS_1

    implicit none

contains


    subroutine RUN_SIMULATION(AQUATIC_MODEL_DATA)

        implicit none
        type(AQUATIC_MODEL_DS), intent(inout) :: AQUATIC_MODEL_DATA

        integer :: PELAGIC_SOLVER_NO
        integer :: COUNTER

        integer :: i
        integer :: j
        integer :: k
        integer :: REPEAT_NO
        integer :: AUX_OUTPUT_UNIT
        integer :: PRINT_INTERVAL

        integer :: NUM_PELAGIC_OUTPUT_LINES
        integer :: NUM_PELAGIC_EX_OUTPUT_LINES
        integer :: PRINT_PELA_OUTPUT_TO_MEMORY
        integer :: PELA_OUTPUT_LINE_NO
        integer :: PELA_EX_OUTPUT_LINE_NO
        integer :: WRITE_PELAGIC_EXERGY_OUTPUT
        integer :: WRITE_PELAGIC_SAVED_OUTPUT

        real(kind = DBL) :: TIME_STEP
        real(kind = DBL) :: TIME
        real(kind = DBL) :: WTIME
        real(kind = DBL) :: PELAGIC_TEMP

        real(kind = DBL), &
            dimension(AQUATIC_MODEL_DATA % &
                      PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS) :: &
            PELAGIC_STATE_VARIABLES

        real(kind = DBL), &
            dimension(AQUATIC_MODEL_DATA % &
                      PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES, &
                      AQUATIC_MODEL_DATA % &
                      PELAGIC_BOX_MODEL_DATA % NUM_EXERGY_COMPONENTS)  :: &
            SUM_PELAGIC_EXERGIES

        real(kind = DBL), &
            dimension(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES) :: &
            SUM_PELAGIC_VOLUMES_EX


        real(kind = DBL), &
            dimension(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_EXERGY_COMPONENTS) :: &
            PELAGIC_EXERGY_COMPONENTS, AVG_SUM_PELAGIC_EXERGIES, AVG_TOTAL_PELAGIC_EXERGIES

        real(kind = DBL) :: TOTAL_VOLUME_EX

        integer :: NUM_PELAGIC_EXERGY_COMPONENTS
        character(len = 20) :: FORMAT_STRING
        character(len = 5)  :: INTEGER_STRING

        !Cost function related
        real(kind = DBL) :: MEASURED_VALUE
        real(kind = DBL) :: CALCULATED_VALUE
        real(kind = DBL) :: RELATIVE_ERROR

        real(kind = DBL), &
            dimension(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS) :: &
            SUM_RELATIVE_ERRORS

        integer, &
            dimension(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS) :: &
            NUM_RELATIVE_ERRORS

        integer :: FORCING_TS_NO
        integer :: BOX_NO
        integer :: TIME_INDEX
        integer :: FORCING_TS_VAR_NO
        integer :: TS_NO

        integer :: MEM_ERROR

        do i = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
            SUM_RELATIVE_ERRORS = 0.0D0
            NUM_RELATIVE_ERRORS = 0
        end do

        AUX_OUTPUT_UNIT   = 20
        PELAGIC_SOLVER_NO = 1
        MEM_ERROR         = 0

        TIME_STEP                   = AQUATIC_MODEL_DATA % TIME_STEP
        PRINT_INTERVAL              = AQUATIC_MODEL_DATA % PRINT_INTERVAL
        PRINT_PELA_OUTPUT_TO_MEMORY = 0
        PELA_OUTPUT_LINE_NO         = 0
        PELA_EX_OUTPUT_LINE_NO      = 0
        WRITE_PELAGIC_EXERGY_OUTPUT = 0
        WRITE_PELAGIC_SAVED_OUTPUT  = 0

        SUM_PELAGIC_EXERGIES        = 0.0D0
        SUM_PELAGIC_VOLUMES_EX      = 0.0D0
        AVG_TOTAL_PELAGIC_EXERGIES  = 0.0D0
        TOTAL_VOLUME_EX             = 0.0D0

        if (PRINT_PELA_OUTPUT_TO_MEMORY /= 0) then

            if (int((AQUATIC_MODEL_DATA % SIMULATION_END - &
                       AQUATIC_MODEL_DATA % SIMULATION_START) / &
                    (TIME_STEP * PRINT_INTERVAL)) < &
                   ((AQUATIC_MODEL_DATA % SIMULATION_END - &
                       AQUATIC_MODEL_DATA % SIMULATION_START) / &
                    (TIME_STEP * PRINT_INTERVAL))) then

                NUM_PELAGIC_OUTPUT_LINES = &
                     AQUATIC_MODEL_DATA % NUM_REPEATS * &
                     (int((AQUATIC_MODEL_DATA % SIMULATION_END - &
                           AQUATIC_MODEL_DATA % SIMULATION_START) / &
                          (TIME_STEP * PRINT_INTERVAL)) + 1)

                NUM_PELAGIC_EX_OUTPUT_LINES =  &
                     (AQUATIC_MODEL_DATA % NUM_REPEATS - &
                      AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                          START_REPEAT_NO_PEL_EX_OUTS + 1) * &
                     (int((AQUATIC_MODEL_DATA % SIMULATION_END - &
                           AQUATIC_MODEL_DATA % SIMULATION_START) / &
                          (TIME_STEP * PRINT_INTERVAL)) + 1)
            else
                NUM_PELAGIC_OUTPUT_LINES = &
                     AQUATIC_MODEL_DATA % NUM_REPEATS * &
                     (int((AQUATIC_MODEL_DATA % SIMULATION_END - &
                           AQUATIC_MODEL_DATA % SIMULATION_START) / &
                          (TIME_STEP * PRINT_INTERVAL)))

                NUM_PELAGIC_EX_OUTPUT_LINES =  &
                     (AQUATIC_MODEL_DATA % NUM_REPEATS - &
                      AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                           START_REPEAT_NO_PEL_EX_OUTS + 1) * &
                     (int((AQUATIC_MODEL_DATA % SIMULATION_END - &
                           AQUATIC_MODEL_DATA % SIMULATION_START) / &
                          (TIME_STEP * PRINT_INTERVAL)))
            end if

            ! ------------------------------------------------------------------------------------------
            allocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % ECOL_RESULTS &
                     (NUM_PELAGIC_OUTPUT_LINES, &
                      0:AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES, &
                      AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS), &
                     stat = MEM_ERROR)

            allocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % AREA_BASED_ECOL_RESULTS &
                     (NUM_PELAGIC_OUTPUT_LINES, &
                      0:AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES, &
                      AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS), &
                     stat = MEM_ERROR)

            if (MEM_ERROR > 0) then
                write(*,*) 'Memory error during allocation of the pelagic model'
                write(*,*) 'state variable results memory output array'

                write(*,*) 'Subroutine : RUN_SIMULATION'
                write(*,*) 'Related allocate statement returned stat code ', MEM_ERROR
                stop
            else
                write(*,*) 'Memory allocated for the pelagic model state variable results '
                write(*,*) 'memory output array'

                write(*,*)
            end if
            ! ------------------------------------------------------------------------------------------


            ! ------------------------------------------------------------------------------------------
            allocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % PROCESS_RATE_RESULTS &
                     (NUM_PELAGIC_OUTPUT_LINES, &
                      0:AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES, &
                      AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PROCESS_RATES), &
                     stat = MEM_ERROR)

            if (MEM_ERROR > 0) then
                write(*,*) 'Memory error during allocation of the pelagic model'
                write(*,*) 'process rate results memory output array'

                write(*,*) 'Subroutine : RUN_SIMULATION'
                write(*,*) 'Related allocate statement returned stat code ', MEM_ERROR
                stop
            else
                write(*,*) 'Memory allocated for the pelagic model process rate results '
                write(*,*) 'memory output array'
                write(*,*)
            end if
            ! ------------------------------------------------------------------------------------------


            ! ------------------------------------------------------------------------------------------
            allocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_RESULTS &
                     (NUM_PELAGIC_OUTPUT_LINES, &
                      0:AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES, &
                      AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_SAVED_OUTPUTS), &
                     stat = MEM_ERROR)

            if (MEM_ERROR > 0) then
                write(*,*) 'Memory error during allocation of the pelagic model'
                write(*,*) 'saved outputs memory output array'

                write(*,*) 'Subroutine : RUN_SIMULATION'
                write(*,*) 'Related allocate statement returned stat code ', MEM_ERROR
                stop
            else
                write(*,*) 'Memory allocated for the pelagic model saved outputs '
                write(*,*) 'memory output array'
                write(*,*)
            end if
            ! ------------------------------------------------------------------------------------------

            if (AQUATIC_MODEL_DATA % &
                PELAGIC_BOX_MODEL_DATA % CREATE_PELAGIC_EXERGY_OUTPUTS > 0) then

                allocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % EXERGY_RESULTS &
                         (NUM_PELAGIC_EX_OUTPUT_LINES,&
                          0:AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES, &
                          AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_EXERGY_COMPONENTS), &
                         stat = MEM_ERROR)

                if (MEM_ERROR > 0) then
                    write(*,*) 'Memory error during allocation of the pelagic model'
                    write(*,*) 'exergy results memory output array'

                    write(*,*) 'Subroutine : RUN_SIMULATION'
                    write(*,*) 'Related allocate statement returned stat code ', MEM_ERROR
                    stop
                else
                    write(*,*) 'Memory allocated for the pelagic model exegy memory output array'
                    write(*,*)
                end if
            end if
        end if

        do REPEAT_NO = 1, AQUATIC_MODEL_DATA % NUM_REPEATS
            write(*, *) 'REPEAT : ', REPEAT_NO

            TIME    = AQUATIC_MODEL_DATA % SIMULATION_START
            COUNTER = 0
            TIME_STEP_NO = 1

            if (REPEAT_NO > 1) then

                do TS_NO = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                  NUM_FORCING_TS

                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                        FORCING_TS         (TS_NO) % TIME_INDEX = 1
                end do

                do TS_NO = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                  NUM_FLOW_TS

                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                        FLOWS             (TS_NO) % TIME_INDEX = 1
                end do

                do TS_NO = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                  NUM_MIXING_TS

                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                       MIXING_EXCHANGES    (TS_NO) % TIME_INDEX = 1
                end do

                do TS_NO = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                  NUM_SETTLING_VELOCITIES

                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                        SETTLING_VELOCITIES(TS_NO) % TIME_INDEX = 1
                end do
            else
                if (PRINT_PELA_OUTPUT_TO_MEMORY == 0) then
write(*,*) 'A1'
                    if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                        CREATE_PELAGIC_ECOL_OUTPUT > 0) then

                        if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT > 0) then
                            call WRITE_PELAGIC_BINARY_OUTPUT &
                                    (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                                     TIME, AUX_OUTPUT_UNIT, &
                                     WRITE_PELAGIC_SAVED_OUTPUT, &
                                     WRITE_PELAGIC_EXERGY_OUTPUT, 1)
                        else
                            call WRITE_PELAGIC_OUTPUT &
                                    (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                                     TIME, AUX_OUTPUT_UNIT, &
                                     WRITE_PELAGIC_SAVED_OUTPUT, &
                                     WRITE_PELAGIC_EXERGY_OUTPUT, 1)
                        end if
                    end if
                end if
            end if

            do while (TIME <= AQUATIC_MODEL_DATA % SIMULATION_END)

                !Solve mass balance equations
                call SOLVE &
                     (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                      TIME, TIME_STEP                            , &
                      PELAGIC_SOLVER_NO                          , &
                      SETTLING_VELOCITIES_OUTPUT                 , &
                      EFFECTIVE_DISSLOVED_FRACTIONS              , &
                      EFFECTIVE_DEPOSITION_FRACTIONS             , &
                      DEPOSITION_AREA_RATIOS                     , &
                      nkn, nstate, NUM_ALLOLOPATHY_STATE_VARS)

                if (MODEL_BOTTOM_SEDIMENTS > 1) then
                    INIT_SED_STATE_VARS = FINAL_SED_STATE_VARS
                end if

                !Cost function
                if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % PRODUCE_COST_FUNCTION > 0) then
                    do i = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                        do j = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                  MEASURED_VALUES(i) % NUM_BOXES

                            FORCING_TS_NO = AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                            MEASURED_VALUES(i) % FORCING_TS_NOS(j)

                            BOX_NO        = AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                            MEASURED_VALUES(i) % BOX_NOS(j)

                            TIME_INDEX    = GET_TIME_INDEX(AQUATIC_MODEL_DATA % &
                                            PELAGIC_BOX_MODEL_DATA % FORCING_TS(FORCING_TS_NO), WTIME)

                            if (TIME_INDEX > 0) then

                                FORCING_TS_VAR_NO = AQUATIC_MODEL_DATA % &
                                     PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i) % FORCING_TS_VAR_NOS(j)

                                MEASURED_VALUE = AQUATIC_MODEL_DATA % &
                                     PELAGIC_BOX_MODEL_DATA % FORCING_TS(FORCING_TS_NO) % &
                                     VARIABLES(TIME_INDEX, FORCING_TS_VAR_NO)

                                CALCULATED_VALUE = AQUATIC_MODEL_DATA % &
                                     PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % CONCENTRATIONS(i)

                                RELATIVE_ERROR = abs(MEASURED_VALUE - CALCULATED_VALUE) / &
                                     AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % MEASUREMENT_ERRORS(i)

                                SUM_RELATIVE_ERRORS(i) = SUM_RELATIVE_ERRORS(i) + RELATIVE_ERROR
                                NUM_RELATIVE_ERRORS(i) = NUM_RELATIVE_ERRORS(i) + 1
                            end if
                        end do
                    end do
                end if

                !Calculate pelagic exergy
                if ((AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                     CALCULATE_PELAGIC_EXERGY > 0).and. &
                    (REPEAT_NO >= AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                  START_REPEAT_NO_PEL_EX_OUTS)) then

                    WRITE_PELAGIC_EXERGY_OUTPUT = 1

                    do i = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES

                        PELAGIC_STATE_VARIABLES = AQUATIC_MODEL_DATA % &
                             PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS

                        PELAGIC_TEMP            = AQUATIC_MODEL_DATA % &
                             PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                             DRIVING_FUNCTIONS(1)

                        call CALCULATE_PELAGIC_EXERGY &
                             (PELAGIC_STATE_VARIABLES, PELAGIC_TEMP, &
                              PELAGIC_EXERGY_COMPONENTS, TIME, i)

                        SUM_PELAGIC_VOLUMES_EX(i) = SUM_PELAGIC_VOLUMES_EX(i) + &
                            AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                            PELAGIC_BOXES(i) % VOLUME

                        do j = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                            NUM_EXERGY_COMPONENTS

                            SUM_PELAGIC_EXERGIES(i, j) = SUM_PELAGIC_EXERGIES(i, j) + &
                                PELAGIC_EXERGY_COMPONENTS(j) * &
                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                PELAGIC_BOXES(i) % VOLUME
                        end do

                        AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                            PELAGIC_BOXES(i) % EXERGY_COMPONENTS = &
                        PELAGIC_EXERGY_COMPONENTS
                    end do
                end if

                TIME  = TIME + TIME_STEP

                WTIME = TIME + &
                    ((AQUATIC_MODEL_DATA % SIMULATION_END - &
                      AQUATIC_MODEL_DATA % SIMULATION_START) * real(REPEAT_NO - 1))

                COUNTER = COUNTER + 1

                AQUATIC_MODEL_DATA % DAY_OF_YEAR = &
                    CALCULATE_DAY_OF_YEAR(WTIME, AQUATIC_MODEL_DATA % BASE_YEAR)

                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % DAY_OF_YEAR = &
                    AQUATIC_MODEL_DATA % DAY_OF_YEAR

                if (COUNTER == PRINT_INTERVAL) then
                    write(*, fmt = '(a8, f15.4, i15)') &
                        'TIME : ', WTIME, AQUATIC_MODEL_DATA % DAY_OF_YEAR

                    ! Print out the water levels and volumes.
                    ! This will be done in a cleaner way in the near future.
                    if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT == 0) then
                        do i = 1, nkn
                            write(unit = 1000,  &
                                  fmt  = '(F10.4, A1, I10, A1, F30.4, A1, F30.4, A1, F30.4, A1, F30.4)') &
                                  WTIME, ',', i, ',', &
                                  AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                      PELAGIC_BOXES(i) % SURFACE_ELEVATION , ',', &
                                  AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                      PELAGIC_BOXES(i) % VOLUME, ',', &
                                  AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                      PELAGIC_BOXES(i) % DEPTH, ',', &
                                  (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                       PELAGIC_BOXES(i) % VOLUME / &
                                   AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                       PELAGIC_BOXES(i) % SURFACE_AREA)
                        end do
                    end if

                    if (PRINT_PELA_OUTPUT_TO_MEMORY == 0) then

                        if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                            CREATE_PELAGIC_ECOL_OUTPUT > 0) then

                            if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT > 0) then

                                call WRITE_PELAGIC_BINARY_OUTPUT &
                                        (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                                         WTIME, AUX_OUTPUT_UNIT, &
                                         WRITE_PELAGIC_SAVED_OUTPUT, &
                                         WRITE_PELAGIC_EXERGY_OUTPUT, 0)
                            else
                                !write(*,*) 'PRINTING'
                                call WRITE_PELAGIC_OUTPUT &
                                         (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                                          WTIME, AUX_OUTPUT_UNIT, &
                                          WRITE_PELAGIC_SAVED_OUTPUT, &
                                          WRITE_PELAGIC_EXERGY_OUTPUT, 0)
                            end if
                        end if
                    else
                        PELA_OUTPUT_LINE_NO = PELA_OUTPUT_LINE_NO + 1

                        if ((AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                 CREATE_PELAGIC_EXERGY_OUTPUTS == 1).and. &
                            (REPEAT_NO >= AQUATIC_MODEL_DATA % &
                                 PELAGIC_BOX_MODEL_DATA % START_REPEAT_NO_PEL_EX_OUTS)) then

                                PELA_EX_OUTPUT_LINE_NO = PELA_EX_OUTPUT_LINE_NO + 1
                        end if

                        do i = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                      NUM_PELAGIC_BOXES

                            do j = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                          NUM_PELAGIC_STATE_VARS

                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                     ECOL_RESULTS(PELA_OUTPUT_LINE_NO, 0, j) = WTIME

                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                     AREA_BASED_ECOL_RESULTS(PELA_OUTPUT_LINE_NO, 0, j) = WTIME

                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                    ECOL_RESULTS(PELA_OUTPUT_LINE_NO, i, j) = &
                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                    PELAGIC_BOXES(i) % CONCENTRATIONS(j)

                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                    AREA_BASED_ECOL_RESULTS(PELA_OUTPUT_LINE_NO, i, j) = &
                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                    PELAGIC_BOXES(i) % CONCENTRATIONS(j) * &
                                (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                    PELAGIC_BOXES(i) % VOLUME       /   &
                                 AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                    PELAGIC_BOXES(i) % SURFACE_AREA)

                            end do

                            do j = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                          NUM_PROCESS_RATES

                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                     PROCESS_RATE_RESULTS(PELA_OUTPUT_LINE_NO, 0, j) = WTIME

                                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                    PROCESS_RATE_RESULTS(PELA_OUTPUT_LINE_NO, i, j) = &
                                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                        PELAGIC_BOXES(i) % PROCESS_RATES(j)
                            end do

                            if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                    CREATE_PELAGIC_SAVED_OUTPUT == 1) then

                                do j = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_SAVED_OUTPUTS

                                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                         SAVED_OUTPUT_RESULTS(PELA_OUTPUT_LINE_NO, 0, j) = WTIME

                                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                         SAVED_OUTPUT_RESULTS(PELA_OUTPUT_LINE_NO, i, j) = &
                                       AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                           PELAGIC_BOXES(i) % SAVED_OUTPUTS(j)
                                end do
                            end if

                            if ((AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                     CREATE_PELAGIC_EXERGY_OUTPUTS == 1).and. &
                                (REPEAT_NO >= AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                     START_REPEAT_NO_PEL_EX_OUTS)) then

                                do j = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                              NUM_EXERGY_COMPONENTS

                                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                         EXERGY_RESULTS(PELA_EX_OUTPUT_LINE_NO, 0, j) = WTIME

                                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                        EXERGY_RESULTS(PELA_EX_OUTPUT_LINE_NO, i, j) = &
                                    AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                        PELAGIC_BOXES(i) % EXERGY_COMPONENTS(j)
                                end do
                            end if
                        end do
                    end if

                    ! Print out the pelagic mass balance files. This will be done in a cleaner way in
                    ! the near future.
                    do i = 1, nkn
                        if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                PRODUCE_PEL_MASS_BALANCE_OUTPUTS(i) > 0) then

                            do j = 1, nstate
                                if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT == 0) then

                                    write(unit = 1001, fmt = '(F10.4, 2i10, 7F30.6)') WTIME, i, j, &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_ADVECTION_DERIVS      (i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_DISPERSION_DERIVS     (i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_SETTLING_DERIVS       (i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_MASS_LOAD_DERIVS      (i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_MASS_WITHDRAWAL_DERIVS(i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (DERIVATIVES(i, j) - &
                                               FLUXES_TO_WATER_COLUMN(i,j)), &
                                               FLUXES_TO_WATER_COLUMN(i, j)
                                else
                                    write(unit = 1001) WTIME, dble(i), dble(j), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_ADVECTION_DERIVS      (i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_DISPERSION_DERIVS     (i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_SETTLING_DERIVS       (i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_MASS_LOAD_DERIVS      (i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               ECOL_MASS_WITHDRAWAL_DERIVS(i, j, 1) / &
                                           AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                               PELAGIC_BOXES(i) % VOLUME), &
                                          (DERIVATIVES(i, j) - &
                                               FLUXES_TO_WATER_COLUMN(i,j)), &
                                               FLUXES_TO_WATER_COLUMN(i, j)
                                end if
                            end do
                        end if
                    end do

                    if (PRODUCE_COCOA_OUTPUTS > 0) then
                        ! ------------------------------------------------------------------------------------------------------------
                        ! DOCUMENTATION FOR N_PEL_ASSIM
                        ! ------------------------------------------------------------------------------------------------------------
                        !Related to ammonia nitrogen
                        !PROCESS_RATES(1:nkn,1, 11) Chemoautotrpoh (nitrifier) growth (for now inactive in the model) uptaking ammonia
                        !PROCESS_RATES(1:nkn,1, 12) Diatom growth uptaking ammonia
                        !PROCESS_RATES(1:nkn,1, 13) Non-fixing cyanobacteria growth uptaking ammonia
                        !PROCESS_RATES(1:nkn,1, 14) Other planktonic algae growth uptaking ammonia
                        !PROCESS_RATES(1:nkn,1, 15) Fixing cyanobacteria growth when enough nitrogen is present uptaking ammonia

                        !Related to nitrate nitrate nitrogen
                        !PROCESS_RATES(1:nkn,2, 3)  Diatom growth uptaking nitrate
                        !PROCESS_RATES(1:nkn,2, 4)  Non-fixing cyanobacteria growth uptaking nitrate
                        !PROCESS_RATES(1:nkn,2, 5)  Other planktonic algae growth uptaking nitrate
                        !PROCESS_RATES(1:nkn,2, 6)  Fixing cyanobacteria growth when enough nitrogen is present uptaking nitrate

                        !Relatae to dissolved organic nitrogen
                        !(pelagic kinetic model assumes that cyanobacteria are able to consume DON if DIN is scarce)
                        !PROCESS_RATES(1:nkn,16, 7) Non-fixing cyanobacteria growth uptaking DON
                        !PROCESS_RATES(1:nkn,16, 8) fixing cyanobacteria growth uptaking DON
                        ! ------------------------------------------------------------------------------------------------------------
                        ! END OF DOCUMENTATION FOR N_PEL_ASSIM
                        ! ------------------------------------------------------------------------------------------------------------

                        ! ------------------------------------------------------------------------------------------------------------
                        ! DOCUMENTATION FOR N_PEL_EXCRET
                        ! ------------------------------------------------------------------------------------------------------------
                        !Related to PON: Some of the food will be not assimilated during feeding
                        !PROCESS_RATES(1:nkn,13, 9)

                        !Related to DON: This is the real excretion (after digestion) by zooplankton
                        !however it is set to zero in the code by Petras (I do not know why) and will take
                        !some time for me to put it back in a compatible way for both versions of
                        !AQUABC (our box model and SHYFEM implementation)
                        !PROCESS_RATES(1:nkn,16, 2)
                        ! ------------------------------------------------------------------------------------------------------------
                        ! END OF DOCUMENTATION FOR N_PEL_EXCRET
                        ! ------------------------------------------------------------------------------------------------------------


                        ! ------------------------------------------------------------------------------------------------------------
                        ! DOCUMENTATION FOR N_PEL_DECOMP_OF_DET
                        ! ------------------------------------------------------------------------------------------------------------
                        !Related to PON: The only kinetic fate of detritus in the pelagic model is dissolution
                        !where detrital nitrogen is assumed to be converted to dissolved organic nitrogen (DON)
                        !PROCESS_RATES(1:nkn,13, 10)
                        ! ------------------------------------------------------------------------------------------------------------
                        ! DOCUMENTATION FOR N_PEL_DECOMP_OF_DET
                        ! ------------------------------------------------------------------------------------------------------------


                        ! ------------------------------------------------------------------------------------------------------------
                        ! DOCUMENTATION FOR P_PEL_ASSIM
                        ! ------------------------------------------------------------------------------------------------------------
                        !PROCCESS_RATES(1:nkn,3, 11) Chemoautotrpoh (nitrifier) growth (for now inactive in the model) uptaking phosphorus
                        !PROCCESS_RATES(1:nkn,3, 12) Diatom growth uptaking phosphorus
                        !PROCCESS_RATES(1:nkn,3, 13) Non-fixing cyanobacteria growth uptaking phosphorus
                        !PROCCESS_RATES(1:nkn,3, 14) Other planktonic algae growth uptaking phosphorus
                        !PROCCESS_RATES(1:nkn,3, 15) Fixing cyanobacteria growth when enough nitrogen is present uptaking phosphorus
                        ! ------------------------------------------------------------------------------------------------------------
                        ! END OF DOCUMENTATION FOR P_PEL_ASSIM
                        ! ------------------------------------------------------------------------------------------------------------


                        ! ------------------------------------------------------------------------------------------------------------
                        ! DOCUMENTATION FOR P_PEL_EXCRET
                        ! ------------------------------------------------------------------------------------------------------------
                        !Related to POP: Some of the food will be not assimilated during feeding
                        !(1:nkn,14, 9)

                        !Related to DOP: This is the real excretion (after digestion) by zooplankton
                        !however it is set to zero in the code by Petras (I do not know why) and will take
                        !some time for me to put it back in a compatible way for both versions of
                        !AQUABC (our box model and SHYFEM implementation)
                        !(1:nkn,17, 2)
                        ! ------------------------------------------------------------------------------------------------------------
                        ! END OF DOCUMENTATION FOR P_PEL_EXCRET
                        ! ------------------------------------------------------------------------------------------------------------


                        ! ------------------------------------------------------------------------------------------------------------
                        ! DOCUMENTATION FOR N_PEL_DECOMP_OF_DET
                        ! ------------------------------------------------------------------------------------------------------------
                        !Related to POP: The only kinetic fate of detritus in the pelagic model is dissolution
                        !where detrital phosphorus is assumed to be converted to dissolved organic phosphorus (DOP)
                        !PROCESS_RATES(1:nkn,14, 10)
                        ! ------------------------------------------------------------------------------------------------------------
                        ! END OF DOCUMENTATION FOR N_PEL_DECOMP_OF_DET
                        ! ------------------------------------------------------------------------------------------------------------


                        ! Print out the water column process rates
                        do i = 1, nkn
                            write(unit = 2020, fmt  = '(F10.4,2I10,8F30.8)') &
                                  WTIME, i, 1, &
                                  PROCESS_RATES(i, 2, 2) , &                                    ! N_PEL_DENITRIFICATION
                                  PROCESS_RATES(i, 22, 7), &                                    ! N_FIXATION
                                  (PROCESS_RATES(i, 1 , 11) + PROCESS_RATES(i, 1 , 12) + &      ! Compute N_PEL_ASSIM
                                   PROCESS_RATES(i, 1 , 13) + PROCESS_RATES(i, 1 , 14) + &      ! Compute N_PEL_ASSIM
                                   PROCESS_RATES(i, 1 , 15) + PROCESS_RATES(i, 2 , 3 ) + &      ! Compute N_PEL_ASSIM
                                   PROCESS_RATES(i, 2 , 4 ) + PROCESS_RATES(i, 2 , 5 ) + &      ! Compute N_PEL_ASSIM
                                   PROCESS_RATES(i, 2 , 6 ) + PROCESS_RATES(i, 16, 7 ) + &      ! Compute N_PEL_ASSIM
                                   PROCESS_RATES(i, 16, 8)) , &                                 ! Compute N_PEL_ASSIM
                                  (PROCESS_RATES(i, 13, 9 ) + PROCESS_RATES(i, 16, 2 )), &      ! Compute N_PEL_EXCRET
                                   PROCESS_RATES(i , 13, 10), &                                 ! N_PEL_DECOMP_OF_DET
                                  (PROCESS_RATES(i, 3 , 11) + PROCESS_RATES(i, 3 , 12) + &      ! Compute P_PEL_ASSIM
                                   PROCESS_RATES(i, 3 , 13) + PROCESS_RATES(i, 3 , 14) + &      ! Compute P_PEL_ASSIM
                                   PROCESS_RATES(i, 3 , 15)), &                                 ! Compute P_PEL_ASSIM
                                  (PROCESS_RATES(i, 14, 9 ) + PROCESS_RATES(i, 17, 2 )), &      ! Compute P_PEL_EXCRET
                                   PROCESS_RATES(i , 16, 10)                                    ! P_PEL_DECOMP_OF_DET
                        end do
                    end if

                    ! Sediments
                    if (MODEL_BOTTOM_SEDIMENTS > 1) then
                        do i = 1, nkn
                            write(unit = 1023, fmt = '(F10.4,I10,33F20.10)') &
                                  WTIME, i, FLUXES_OUTPUT_TO_WATER_COLUMN(nkn,:)

                            do j = 1, NUM_SED_LAYERS
                                write(unit = 1021, fmt = '(F10.4,2I10,24F20.10)') &
                                      WTIME, i, j, INIT_SED_STATE_VARS(i, j, :)
                            end do

                            if (PRODUCE_COCOA_OUTPUTS > 0) then
                                write(unit = 2031, fmt  = '(F10.4,2I10,2F30.8)') WTIME, i, 1, &
                                      (FLUXES_FROM_SEDIMENTS(i, 1) + FLUXES_FROM_SEDIMENTS(i, 1)), & ! Compute N_OUT_FROM_SED
                                      FLUXES_FROM_SEDIMENTS(i, 5)                                    ! P_OUT_FROM_SED

                                write(unit = 2032, fmt  = '(F10.4,2I10,3F40.8)') WTIME, i, 1, &
                                      FLUXES_TO_SEDIMENTS(i, 2), FLUXES_TO_SEDIMENTS(i, 4), FLUXES_TO_SEDIMENTS(i, 7)

                                do j = 1, NUM_SED_LAYERS
                                    write(unit = 2021, fmt = '(F10.4,2I10,3F30.8)') WTIME, i, j, &
                                          PROCESSES_sed(i, j , 2, 11), &         !N_SED_DENITRIFICATION
                                          PROCESSES_sed(i, j , 3, 11), &         !N_SED_REMINARALIZATION
                                          PROCESSES_sed(i, j , 6, 11)            !P_SED_REMINERALIZATION

                                    write(unit = 2022, fmt = '(F10.4,2I10,2F30.8)') WTIME, i, j, &
                                          (SED_BURRIAL_RATE_OUTPUTS(i, j, 1) + & ! Compute N_BURRIAL
                                           SED_BURRIAL_RATE_OUTPUTS(i, j, 2) + & ! Compute N_BURRIAL
                                           SED_BURRIAL_RATE_OUTPUTS(i, j, 3) + & ! Compute N_BURRIAL
                                           SED_BURRIAL_RATE_OUTPUTS(i, j, 4)), & ! Compute N_BURRIAL
                                          (SED_BURRIAL_RATE_OUTPUTS(i, j, 5) + & ! Compute P_BURRIAL
                                           SED_BURRIAL_RATE_OUTPUTS(i, j, 6) + & ! Compute P_BURRIAL
                                           SED_BURRIAL_RATE_OUTPUTS(i, j, 7))    ! Compute P_BURRIAL
                                end do
                            end if
                        end do
                    end if

                    COUNTER = 0
                end if

                TIME_STEP_NO = TIME_STEP_NO + 1
            end do
        end do

        if (PRINT_PELA_OUTPUT_TO_MEMORY /= 0) then

            if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                 CREATE_PELAGIC_ECOL_OUTPUT > 0) then

                if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT > 0) then
                    call WRITE_PELAGIC_MEM_BINARY_OUTPUT &
                         (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, AUX_OUTPUT_UNIT, &
                          NUM_PELAGIC_OUTPUT_LINES)
                else
                    call WRITE_PELAGIC_MEM_OUTPUT &
                         (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, AUX_OUTPUT_UNIT, &
                          NUM_PELAGIC_OUTPUT_LINES)
                end if
            end if

            write (*,*) AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                    CREATE_PELAGIC_SAVED_OUTPUT

            if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                    CREATE_PELAGIC_SAVED_OUTPUT > 0) then

                if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT > 0) then
                    call WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT &
                         (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                          AUX_OUTPUT_UNIT, NUM_PELAGIC_OUTPUT_LINES)
                else
                    call WRITE_PELAGIC_MEM_SAVED_OUTPUT &
                         (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                          AUX_OUTPUT_UNIT, NUM_PELAGIC_OUTPUT_LINES)
                end if
            end if

            if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                CREATE_PELAGIC_EXERGY_OUTPUTS > 0) then

                if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT > 0) then
                    call WRITE_PELAGIC_EX_MEM_BINARY_OUTPUT &
                         (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                          AUX_OUTPUT_UNIT, NUM_PELAGIC_EX_OUTPUT_LINES)
                else
                    call WRITE_PELAGIC_EX_MEM_OUTPUT &
                         (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, &
                          AUX_OUTPUT_UNIT, NUM_PELAGIC_EX_OUTPUT_LINES)
                end if
            end if

            deallocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % ECOL_RESULTS           )
            deallocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % AREA_BASED_ECOL_RESULTS)
            deallocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % PROCESS_RATE_RESULTS   )
            deallocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_RESULTS   )

            if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                    CREATE_PELAGIC_EXERGY_OUTPUTS > 0) then
                deallocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % EXERGY_RESULTS)
            end if
        end if

        !Write exergy summary
        !if ((AQUATIC_MODEL_DATA % &
        !&       PELAGIC_BOX_MODEL_DATA % &
        !&       CREATE_PELAGIC_EXERGY_OUTPUTS == 1)) then

        !    NUM_PELAGIC_EXERGY_COMPONENTS = AQUATIC_MODEL_DATA % &
        !    &   PELAGIC_BOX_MODEL_DATA % NUM_EXERGY_COMPONENTS

        !    write(unit = INTEGER_STRING, fmt = '(i5)') &
        !    &    NUM_PELAGIC_EXERGY_COMPONENTS

        !    FORMAT_STRING = '(i10,' // INTEGER_STRING // 'f20.6)'

        !    open(unit   = AUX_OUTPUT_UNIT, &
        !         file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // 'BOX_EXERGIES.out', &
        !    &    status = 'UNKNOWN')

        !    do i = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
        !    &         NUM_PELAGIC_BOXES

        !        AVG_SUM_PELAGIC_EXERGIES = 0.0D0

        !        do j = 1, AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
        !        &         NUM_EXERGY_COMPONENTS
        !            AVG_SUM_PELAGIC_EXERGIES(j) = &
        !            &   SUM_PELAGIC_EXERGIES(i, j) / SUM_PELAGIC_VOLUMES_EX(i)

        !            AVG_TOTAL_PELAGIC_EXERGIES(j) = &
        !            &   AVG_TOTAL_PELAGIC_EXERGIES(j) + SUM_PELAGIC_EXERGIES(i, j)
        !        end do

        !        write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING)  &
        !        &    i, (AVG_SUM_PELAGIC_EXERGIES(j), &
        !        &        j = 1, NUM_PELAGIC_EXERGY_COMPONENTS)

        !    end do

        !    close(AUX_OUTPUT_UNIT)

        !    TOTAL_VOLUME_EX = sum(SUM_PELAGIC_VOLUMES_EX)

        !    AVG_TOTAL_PELAGIC_EXERGIES = &
        !    &   AVG_TOTAL_PELAGIC_EXERGIES / TOTAL_VOLUME_EX

        !    FORMAT_STRING = '(' // INTEGER_STRING // 'f20.6)'

        !    open(unit   = AUX_OUTPUT_UNIT, &
        !        file    = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // 'EXERGIES.out', &
        !    &    status = 'UNKNOWN')

        !    write(unit = AUX_OUTPUT_UNIT, fmt = FORMAT_STRING)  &
        !    &    (AVG_TOTAL_PELAGIC_EXERGIES(i), &
        !    &        i = 1, NUM_PELAGIC_EXERGY_COMPONENTS)

        !    close(AUX_OUTPUT_UNIT)

        !    open(unit   = AUX_OUTPUT_UNIT, &
        !         file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // 'EXERGY.out', &
        !    &    status = 'UNKNOWN')

        !    write(unit = AUX_OUTPUT_UNIT, fmt = '(F20.6)') &
        !    &     sum(AVG_TOTAL_PELAGIC_EXERGIES)

        !    close(AUX_OUTPUT_UNIT)

        !end if

        !Write cost function file
        if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % PRODUCE_COST_FUNCTION > 0) then
            open(unit = AUX_OUTPUT_UNIT, &
                 file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // 'COST_TOTAL', status = 'UNKNOWN')

            write(unit = AUX_OUTPUT_UNIT, fmt = '(F20.6)') &
                  sum(SUM_RELATIVE_ERRORS) / sum(NUM_RELATIVE_ERRORS)

            close(AUX_OUTPUT_UNIT)
        end if

    end subroutine RUN_SIMULATION

end module SIMULATE
