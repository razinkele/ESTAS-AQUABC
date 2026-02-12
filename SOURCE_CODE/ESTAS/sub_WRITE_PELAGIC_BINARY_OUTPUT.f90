subroutine WRITE_PELAGIC_BINARY_OUTPUT &
           (PELAGIC_BOX_MODEL_DATA, TIME, RES_OUTPUT_UNIT, &
            WRITE_SAVED_OUTPUT, WRITE_PELAGIC_EXERGY_OUTPUT, &
            MODEL_AT_INIT)

    use PELAGIC_BOX_MODEL
    implicit none

    type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA
    real(kind = DBL), intent(in) :: TIME
    integer, intent(in) :: RES_OUTPUT_UNIT
    integer, intent(in) :: WRITE_SAVED_OUTPUT
    integer, intent(in) :: WRITE_PELAGIC_EXERGY_OUTPUT
    integer, intent(in) :: MODEL_AT_INIT

    integer i
    integer j
    integer NUM_BOXES
    integer NUM_PROCESS_RATES

    double precision, &
        dimension(nstate + NUM_ALLOLOPATHY_STATE_VARS) :: WRITTEN_CONC

    double precision, &
        dimension(nkn, ((nstate + NUM_ALLOLOPATHY_STATE_VARS) * NDIAGVAR)) :: &
        WRITTEN_PROCESS_RATE

    double precision, dimension(nkn)    :: MEAN_BOX_DEPTHS
    character(len = 5)  :: BOX_NO_STRING

    if (CONSIDER_ALLELOPATHY > 0)then
        NUM_PROCESS_RATES = (nstate + NUM_ALLOLOPATHY_STATE_VARS) * NDIAGVAR
    else
        NUM_PROCESS_RATES = nstate * NDIAGVAR
    end if

    do i = 1, nkn

        MEAN_BOX_DEPTHS(i) = &
             PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME       /   &
             PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_AREA
    end do

    NUM_BOXES = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES


    ! -------------------------------------------------------------------------------
    ! CREATE PELAGIC BOX OUTPUT FILES
    ! -------------------------------------------------------------------------------
    do i = 1, NUM_BOXES
        if (PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_STATE_VAR_OUTPUTS(i) > 0) then
            write(unit = BOX_NO_STRING, fmt = '(i5.5)') i

            ! If the desired output is in g/m^2 than convert the out concentration
            ! from g/m^3 to g/m^2
            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
                if (PELAGIC_BOX_MODEL_DATA % STATE_VAR_OUTPUT_TYPES(j) > 1) then

                    WRITTEN_CONC(j) = &
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                        CONCENTRATIONS(j) * MEAN_BOX_DEPTHS(i)
                else
                    WRITTEN_CONC(j) = &
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % CONCENTRATIONS(j)
                end if
            end do

            if (MODEL_AT_INIT > 0) then
                open(unit   = RES_OUTPUT_UNIT, &
                     file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                              trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) &
                              // "_" // &
                              trim(adjustl('PELAGIC_BOX_' // &
                                            BOX_NO_STRING // '.bin')), &
                     status = 'REPLACE', access = 'STREAM')

                write(unit = RES_OUTPUT_UNIT)  &
                      TIME, ((WRITTEN_CONC(j)), &
                          j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS)

                close(RES_OUTPUT_UNIT)
            else
                open(unit   = RES_OUTPUT_UNIT, &
                     file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                              trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME))  &
                              // "_" // &
                              trim(adjustl('PELAGIC_BOX_' // BOX_NO_STRING // &
                              '.bin')), &
                     status = 'OLD', position = 'APPEND', access = 'STREAM')

                write(unit = RES_OUTPUT_UNIT)  &
                      TIME, ((WRITTEN_CONC(j)), &
                          j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS)

                close(RES_OUTPUT_UNIT)
            end if
        end if
    end do
    ! -------------------------------------------------------------------------------
    ! END OF CREATE PELAGIC BOX OUTPUT FILES
    ! -------------------------------------------------------------------------------


    ! -------------------------------------------------------------------------------
    ! CREATE PELAGIC BOX PROCESS RATES OUTPUT FILES
    ! -------------------------------------------------------------------------------
    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        if (PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_PROCESS_RATE_OUTPUTS(i) > 0) then
            write(unit = BOX_NO_STRING, fmt = '(i5.5)') i

            if (MODEL_AT_INIT > 0) then
                open(unit   = RES_OUTPUT_UNIT, &
                     file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                              trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) &
                              // "_" // &
                              trim(adjustl('PELAGIC_BOX_' // BOX_NO_STRING // &
                                           '_PROCESS_RATES.bin')), &
                     status ='REPLACE')

                close(RES_OUTPUT_UNIT)
            end if

            open(unit   = RES_OUTPUT_UNIT, &
                 file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                          trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) &
                          // "_" // &
                          trim(adjustl('PELAGIC_BOX_' // BOX_NO_STRING // &
                                       '_PROCESS_RATES.bin')), &
                 status = 'OLD', position = 'APPEND', access = 'STREAM')

            ! If the desired output is in g/m^2 than convert the out concentration
            ! from g/m^3 to g/m^2
            do j = 1, NUM_PROCESS_RATES
                if (PEL_PROCESS_RATE_OUTPUT_OPTION == 1) then

                    WRITTEN_PROCESS_RATE(i, j) = &
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % PROCESS_RATES(j)
                else
                    WRITTEN_PROCESS_RATE(i, j) = &
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % PROCESS_RATES(j) * &
                        (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME / &
                         PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_AREA)
                end if
            end do

            write(unit = RES_OUTPUT_UNIT)  &
                  TIME, (WRITTEN_PROCESS_RATE(i, j), j = 1, NUM_PROCESS_RATES)

            close(RES_OUTPUT_UNIT)
        end if
    end do
    ! -------------------------------------------------------------------------------
    ! END OF CREATE PELAGIC BOX PROCESS RATES OUTPUT FILES
    ! -------------------------------------------------------------------------------


    if (PELAGIC_BOX_MODEL_DATA % CREATE_STATE_VARIABLE_OUTPUT > 0) then
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
            open(unit = RES_OUTPUT_UNIT, &
                 file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                        trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) // "_" &
            // trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                        PELAGIC_STATE_VAR_NAMES(i))) // '.bin', &
                    status = 'UNKNOWN', position = 'APPEND', access = 'STREAM')

            ! If the desired output is in g/m^2 than convert the out concentration
            ! from g/m^3 to g/m^2
            if (PELAGIC_BOX_MODEL_DATA % STATE_VAR_OUTPUT_TYPES(i) > 1) then

                   write(unit = RES_OUTPUT_UNIT ) TIME, &
                      ((PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(j) % &
                          CONCENTRATIONS(i) *  MEAN_BOX_DEPTHS(j)), j = 1, NUM_BOXES)
            else
                   write(unit = RES_OUTPUT_UNIT ) TIME, &
                      (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(j) % &
                          CONCENTRATIONS(i), j = 1, NUM_BOXES)
            end if

            close(RES_OUTPUT_UNIT)
        end do
    end if

    if (WRITE_SAVED_OUTPUT == 1) then
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_SAVED_OUTPUTS

            open(unit = RES_OUTPUT_UNIT, &
                 file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                        trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) // "_" &
            // trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                        SAVED_OUTPUT_NAMES(i))) // '.bin', &
                        status = 'UNKNOWN', position = 'APPEND', access = 'STREAM')

            write(unit = RES_OUTPUT_UNIT) TIME, &
                  (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(j) % &
                       SAVED_OUTPUTS(i), j = 1, NUM_BOXES)

            close(RES_OUTPUT_UNIT)
        end do
    end if


    if (WRITE_PELAGIC_EXERGY_OUTPUT == 1) then
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_EXERGY_COMPONENTS

            open(unit = RES_OUTPUT_UNIT, &
                 file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                        trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) // "_" &
            // trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                         EXERGY_COMPONENT_NAMES(i))) // '.bin', &
                        status = 'UNKNOWN', position = 'APPEND', access = 'STREAM')

            write(unit = RES_OUTPUT_UNIT) TIME, &
                  (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(j) % &
                       EXERGY_COMPONENTS(i), j = 1, NUM_BOXES)

            close(RES_OUTPUT_UNIT)
        end do
    end if
end subroutine WRITE_PELAGIC_BINARY_OUTPUT
