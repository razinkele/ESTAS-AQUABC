subroutine WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT &
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
    integer NUM_SAVED_OUTPUTS

    NUM_BOXES = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_SAVED_OUTPUTS

        open(unit = RES_OUTPUT_UNIT, &
             file = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                    trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) // "_" // &
        trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                        SAVED_OUTPUT_NAMES(i))) // '.bin', &
                    status = 'UNKNOWN', access = 'STREAM')

        do LINE_NO = 1, NUM_LINES
            TIME = PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_RESULTS(LINE_NO, 0, i)

            write(unit = RES_OUTPUT_UNIT) TIME, &
                  (PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_RESULTS(LINE_NO, j, i), &
                   j = 1, NUM_BOXES)
        end do

        close(RES_OUTPUT_UNIT)
    end do
end subroutine WRITE_PELAGIC_MEM_SAVED_BINARY_OUTPUT