subroutine WRITE_PELAGIC_MEM_SAVED_OUTPUT &
           (PELAGIC_BOX_MODEL_DATA, RES_OUTPUT_UNIT, NUM_LINES)

    use PELAGIC_BOX_MODEL
    implicit none

    type(PELAGIC_BOX_MODEL_DS), intent(in) :: PELAGIC_BOX_MODEL_DATA
    integer, intent(in) :: RES_OUTPUT_UNIT
    integer, intent(in) :: NUM_LINES

    real(kind = DBL) :: TIME

    character(len = 20) :: FORMAT_STRING
    character(len = 5)  :: INTEGER_STRING

    integer i
    integer j
    integer LINE_NO
    integer NUM_BOXES

    NUM_BOXES  = PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
    write(unit = INTEGER_STRING, fmt = '(i5)') NUM_BOXES + 1
    FORMAT_STRING = '(' // INTEGER_STRING // 'f20.6)'

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_SAVED_OUTPUTS

        open(unit   = RES_OUTPUT_UNIT, &
             file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                      trim(adjustl(PELAGIC_BOX_MODEL_DATA % &
                                     SAVED_OUTPUT_NAMES(i))) // '.out', &
             status = 'UNKNOWN', position = 'APPEND')

        do LINE_NO = 1, NUM_LINES
            TIME = PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_RESULTS(LINE_NO, 0, i)

            write(unit = RES_OUTPUT_UNIT, fmt = FORMAT_STRING) TIME, &
                  (PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_RESULTS(LINE_NO, j, i), &
                   j = 1, NUM_BOXES)
        end do

        close(RES_OUTPUT_UNIT)
    end do

end subroutine WRITE_PELAGIC_MEM_SAVED_OUTPUT
