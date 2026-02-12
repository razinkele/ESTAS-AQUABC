subroutine WRITE_PELAGIC_MEM_BINARY_OUTPUT &
              (PELAGIC_BOX_MODEL_DATA, RES_OUTPUT_UNIT, NUM_LINES)

    use PELAGIC_BOX_MODEL
    implicit none

    type(PELAGIC_BOX_MODEL_DS), intent(in) :: PELAGIC_BOX_MODEL_DATA
    integer, intent(in) :: RES_OUTPUT_UNIT
    integer, intent(in) :: NUM_LINES

    real(kind = DBL) :: TIME

    integer i
    integer j
    integer LINE_NO
    integer NUM_BOXES
    integer NUM_STATE_VARS
    integer NUM_PROCESS_RATES

    character(len = 5)  :: BOX_NO_STRING

    NUM_BOXES         = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
    NUM_PROCESS_RATES = PELAGIC_BOX_MODEL_DATA % NUM_PROCESS_RATES
    NUM_STATE_VARS    = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

    if (PELAGIC_BOX_MODEL_DATA % CREATE_STATE_VARIABLE_OUTPUT > 0) then
        do i = 1, NUM_STATE_VARS
            open(unit = RES_OUTPUT_UNIT, &
                 file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                        trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) // "_" // &
            trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                      PELAGIC_STATE_VAR_NAMES(i))) // '.bin', &
                 status = 'UNKNOWN', access = 'STREAM')

            do LINE_NO = 1, NUM_LINES
                TIME = PELAGIC_BOX_MODEL_DATA % ECOL_RESULTS(LINE_NO, 0, i)

                ! If the desired output is in g/m^2 than convert the out concentration
                ! from g/m^3 to g/m^2
                if (PELAGIC_BOX_MODEL_DATA % STATE_VAR_OUTPUT_TYPES(i) > 1) then
                    write(unit = RES_OUTPUT_UNIT) TIME, &
                          (PELAGIC_BOX_MODEL_DATA % &
                              AREA_BASED_ECOL_RESULTS(LINE_NO, j, i), &
                          j = 1, NUM_BOXES)
                else
                    write(unit = RES_OUTPUT_UNIT) TIME, &
                          (PELAGIC_BOX_MODEL_DATA % ECOL_RESULTS(LINE_NO, j, i), &
                               j = 1, NUM_BOXES)
                end if
            end do

            close(RES_OUTPUT_UNIT)
        end do
    end if

    do i = 1, NUM_BOXES
        if (PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_STATE_VAR_OUTPUTS(i) > 0) then
            write(unit = BOX_NO_STRING, fmt = '(i5.5)') i

            open(unit = RES_OUTPUT_UNIT, &
                 file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                        trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) // "_" // &
                        trim(adjustl('PELAGIC_BOX_' // BOX_NO_STRING // '.bin')), &
                 status = 'UNKNOWN', access = 'STREAM')

            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                ! If the desired output is in g/m^2 than convert the out concentration
                ! from g/m^3 to g/m^2
                if (PELAGIC_BOX_MODEL_DATA % STATE_VAR_OUTPUT_TYPES(j) > 1) then
                    PELAGIC_BOX_MODEL_DATA % ECOL_RESULTS(:, i, j) = &
                        PELAGIC_BOX_MODEL_DATA % AREA_BASED_ECOL_RESULTS(:, i, j)
                end if
            end do

            do LINE_NO = 1, NUM_LINES
                TIME = PELAGIC_BOX_MODEL_DATA % ECOL_RESULTS(LINE_NO, 0, 1)

                write(unit = RES_OUTPUT_UNIT) TIME, &
                      (PELAGIC_BOX_MODEL_DATA % ECOL_RESULTS(LINE_NO, i, j), &
                       j = 1, NUM_STATE_VARS)
            end do

            close(RES_OUTPUT_UNIT)
        end if
    end do

    do i = 1, NUM_BOXES
        if (PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_PROCESS_RATE_OUTPUTS(i) > 0) then
            write(unit = BOX_NO_STRING, fmt = '(i5.5)') i

            open(unit   = RES_OUTPUT_UNIT, &
                 file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                          trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) // "_" // &
                              trim(adjustl('PELAGIC_BOX_' // BOX_NO_STRING // &
                         '_PROCESS_RATES.bin')), status = 'UNKNOWN', access = 'STREAM')

            do LINE_NO = 1, NUM_LINES
                TIME = PELAGIC_BOX_MODEL_DATA % PROCESS_RATE_RESULTS(LINE_NO, 0, 1)

                if (PEL_PROCESS_RATE_OUTPUT_OPTION == 1) then
                    write(unit = RES_OUTPUT_UNIT) TIME, &
                          (PELAGIC_BOX_MODEL_DATA % PROCESS_RATE_RESULTS(LINE_NO, i, j), &
                           j = 1, NUM_PROCESS_RATES)
                else
                    write(unit = RES_OUTPUT_UNIT) TIME, &
                          ((PELAGIC_BOX_MODEL_DATA % PROCESS_RATE_RESULTS(LINE_NO, i, j) * &
                            (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME       /   &
                             PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_AREA)), &
                           j = 1, NUM_PROCESS_RATES)
                end if
            end do

            close(RES_OUTPUT_UNIT)
        end if
    end do
end subroutine WRITE_PELAGIC_MEM_BINARY_OUTPUT