! -----------------------------------------------------------------------------
! Zaman serileri ile ilgili veri yap�lar�n� ve y�netim fonksiyonlar�n�
! i�eren mod�l
! -----------------------------------------------------------------------------
module TIME_SERIES

    use INTERPOLATE

    implicit none

    type, public :: TIME_SERIE
        integer :: DATA_SIZE
        integer :: NUMBER_OF_VARIABLES
        integer :: ALLOCATED
        integer :: INTERPOLATE
        integer :: TIME_INDEX
        
        real(kind = DBL_PRECISION), pointer, dimension(:)   :: TIMES
        real(kind = DBL_PRECISION), pointer, dimension(:)   :: SCALE_FACTORS
        real(kind = DBL_PRECISION), pointer, dimension(:)   :: UNIT_CONVERSION_FACTORS
        real(kind = DBL_PRECISION), pointer, dimension(:,:) :: VARIABLES
    end type TIME_SERIE

contains

    subroutine INITIALIZE_TIME_SERIE(TIME_FUNCTION)

        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION

        TIME_FUNCTION % DATA_SIZE           = 0
        TIME_FUNCTION % NUMBER_OF_VARIABLES = 0
        TIME_FUNCTION % ALLOCATED           = 0
        TIME_FUNCTION % INTERPOLATE         = 0
        TIME_FUNCTION % TIME_INDEX          = 1
    end subroutine INITIALIZE_TIME_SERIE


    subroutine ALLOCATE_TIME_SERIE(TIME_FUNCTION, DATA_SIZE, NUMBER_OF_VARIABLES)
        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION
        integer, intent(in) :: DATA_SIZE
        integer, intent(in) :: NUMBER_OF_VARIABLES

        if (TIME_FUNCTION % ALLOCATED == 1) then
            deallocate(TIME_FUNCTION % TIMES)
            deallocate(TIME_FUNCTION % SCALE_FACTORS)
            deallocate(TIME_FUNCTION % UNIT_CONVERSION_FACTORS)
            deallocate(TIME_FUNCTION % VARIABLES)
        end if

        allocate(TIME_FUNCTION % TIMES(DATA_SIZE))
        allocate(TIME_FUNCTION % SCALE_FACTORS(NUMBER_OF_VARIABLES))
        allocate(TIME_FUNCTION % UNIT_CONVERSION_FACTORS(NUMBER_OF_VARIABLES))
        allocate(TIME_FUNCTION % VARIABLES(DATA_SIZE, NUMBER_OF_VARIABLES))

        TIME_FUNCTION % DATA_SIZE = DATA_SIZE
        TIME_FUNCTION % NUMBER_OF_VARIABLES = NUMBER_OF_VARIABLES
        TIME_FUNCTION % ALLOCATED = 1
    end subroutine ALLOCATE_TIME_SERIE


    subroutine DEALLOCATE_TIME_SERIE(TIME_FUNCTION)

        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION

        if (TIME_FUNCTION % ALLOCATED == 1) then
            deallocate(TIME_FUNCTION % TIMES)
            deallocate(TIME_FUNCTION % SCALE_FACTORS)
            deallocate(TIME_FUNCTION % UNIT_CONVERSION_FACTORS)
            deallocate(TIME_FUNCTION % VARIABLES)
        end if

        TIME_FUNCTION % DATA_SIZE = 0
        TIME_FUNCTION % NUMBER_OF_VARIABLES = 0
        TIME_FUNCTION % ALLOCATED = 0
    end subroutine DEALLOCATE_TIME_SERIE


    subroutine TIMES_AND_VALUES_TO_TIME_SERIE(TIME_FUNCTION, VALUE_ARRAY)

        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION

        real(kind = DBL_PRECISION), dimension(:,:), intent(in) :: VALUE_ARRAY

        integer :: DATA_SIZE, NUMBER_OF_VARIABLES, i, j

        if (TIME_FUNCTION % ALLOCATED == 1) then
            DATA_SIZE = TIME_FUNCTION % DATA_SIZE
            NUMBER_OF_VARIABLES = TIME_FUNCTION % NUMBER_OF_VARIABLES

            if ((size(VALUE_ARRAY, dim = 2) /= NUMBER_OF_VARIABLES + 1).or. &
                (size(VALUE_ARRAY, dim = 1) /= DATA_SIZE)) then

                write(6,*) 'ERROR'
                write(6,*) 'SUBROUTINE : TIMES_AND_VALUES_TO_TIME_SERIE'

                write(6,*) 'Dimensions of the data array are not compatile ', &
                           'with the dimensions of time serie.'

                write(6,*) 'Number of lines in the array          : ', &
                            size(VALUE_ARRAY, dim = 1)

                write(6,*) 'Number of data in the time serie      : ', DATA_SIZE

                write(6,*) 'Number of columns in the array        : ', &
                            size(VALUE_ARRAY, dim = 2)

                write(6,*) 'Number of variables in the time serie : ', &
                            NUMBER_OF_VARIABLES

                stop
            end if
        else
            DATA_SIZE = size(VALUE_ARRAY, dim = 1)
            NUMBER_OF_VARIABLES = size(VALUE_ARRAY, dim = 2) - 1

            if (NUMBER_OF_VARIABLES == 0) then
                write(6,*) 'ERROR'
                write(6,*) 'SUBROUTINE : TIMES_AND_VALUES_TO_TIME_SERIE'
                write(6,*) 'The time serie should contain at least one variable.'
                stop
            else
                call ALLOCATE_TIME_SERIE(TIME_FUNCTION, DATA_SIZE, NUMBER_OF_VARIABLES)
            end if

        end if

        do i = 1, DATA_SIZE
            TIME_FUNCTION % TIMES(i) = VALUE_ARRAY(i, 1)

            do j = 1, NUMBER_OF_VARIABLES
                TIME_FUNCTION % VARIABLES(i, j) = VALUE_ARRAY(i, j + 1)
            end do
        end do
    end subroutine TIMES_AND_VALUES_TO_TIME_SERIE


    subroutine READ_TIME_SERIE_FROM_FILE(TIME_FUNCTION, INPUT_FILE)

        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION
        integer, intent(in) :: INPUT_FILE

        integer :: DATA_SIZE, NUMBER_OF_VARIABLES, INTERPOLATE, i, j

        real(kind = DBL_PRECISION), dimension(:), allocatable   :: SCALE_FACTOR_ARRAY
        real(kind = DBL_PRECISION), dimension(:), allocatable   :: UNIT_CONV_FACTOR_ARRAY
        real(kind = DBL_PRECISION), dimension(:), allocatable   :: ROW_ARRAY
        real(kind = DBL_PRECISION), dimension(:,:), allocatable :: VALUE_ARRAY

        read(unit = INPUT_FILE, fmt = *)

        read(unit = INPUT_FILE, fmt = *)
        read(unit = INPUT_FILE, fmt = *) DATA_SIZE

        read(unit = INPUT_FILE, fmt = *)
        read(unit = INPUT_FILE, fmt = *) NUMBER_OF_VARIABLES

        allocate(SCALE_FACTOR_ARRAY(NUMBER_OF_VARIABLES))
        allocate(UNIT_CONV_FACTOR_ARRAY(NUMBER_OF_VARIABLES))

        read(unit = INPUT_FILE, fmt = *)
        read(unit = INPUT_FILE, fmt = *)
        read(unit = INPUT_FILE, fmt = *) SCALE_FACTOR_ARRAY

        read(unit = INPUT_FILE, fmt = *)
        read(unit = INPUT_FILE, fmt = *)
        read(unit = INPUT_FILE, fmt = *) UNIT_CONV_FACTOR_ARRAY

        read(unit = INPUT_FILE, fmt = *)
        read(unit = INPUT_FILE, fmt = *) INTERPOLATE

        allocate(ROW_ARRAY(NUMBER_OF_VARIABLES + 1))
        allocate(VALUE_ARRAY(DATA_SIZE, NUMBER_OF_VARIABLES + 1))

        read(unit = INPUT_FILE, fmt = *)

        do i = 1, DATA_SIZE 
            read(unit = INPUT_FILE, fmt = *) ROW_ARRAY

            do j = 1, NUMBER_OF_VARIABLES + 1
                VALUE_ARRAY(i, j) = ROW_ARRAY(j)
            end do
        end do

        call TIMES_AND_VALUES_TO_TIME_SERIE(TIME_FUNCTION, VALUE_ARRAY)

        if (INTERPOLATE == 0) then
            TIME_FUNCTION % INTERPOLATE = 0
        else
            TIME_FUNCTION % INTERPOLATE = 1
        end if

        do i = 1, NUMBER_OF_VARIABLES
            TIME_FUNCTION % SCALE_FACTORS(i)           = SCALE_FACTOR_ARRAY(i)
            TIME_FUNCTION % UNIT_CONVERSION_FACTORS(i) = UNIT_CONV_FACTOR_ARRAY(i)
        end do

        deallocate(ROW_ARRAY)
        deallocate(VALUE_ARRAY)
        deallocate(SCALE_FACTOR_ARRAY)
        deallocate(UNIT_CONV_FACTOR_ARRAY)
    end subroutine READ_TIME_SERIE_FROM_FILE


    function VALUE_FROM_TIME_SERIE(TIME_FUNCTION, TIME, VARIABLE_NO) result(VALUE)

        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION
        real(kind = DBL_PRECISION), intent(in) :: TIME
        integer, intent(in) :: VARIABLE_NO

        real(kind = DBL_PRECISION) :: VALUE

        integer :: DATA_SIZE, NUMBER_OF_VARIABLES, i

        real(kind = DBL_PRECISION) :: PREVIOUS_TIME, NEXT_TIME, PREVIOUS_VALUE, NEXT_VALUE

        NUMBER_OF_VARIABLES = TIME_FUNCTION % NUMBER_OF_VARIABLES

        if ((VARIABLE_NO < 1).or.(VARIABLE_NO > NUMBER_OF_VARIABLES)) then
            write(6,*) 'ERROR'
            write(6,*) 'FUNCTION : VALUE_FROM_TIME_SERIE'
            write(6,*) 'The required variable is not found in the time serie.'
            stop
        end if

        if (TIME_FUNCTION % ALLOCATED == 0) then 
            VALUE = 0.0D0
        else

            if(TIME < TIME_FUNCTION % TIMES(1)) then
                VALUE = 0.0D0
            else
                DATA_SIZE = TIME_FUNCTION % DATA_SIZE

                if(TIME > TIME_FUNCTION % TIMES(DATA_SIZE)) then
                    VALUE = TIME_FUNCTION % VARIABLES(DATA_SIZE, VARIABLE_NO)

                    VALUE = VALUE * TIME_FUNCTION % SCALE_FACTORS(VARIABLE_NO) * &
                         TIME_FUNCTION % UNIT_CONVERSION_FACTORS(VARIABLE_NO)
                else
                    do i = TIME_FUNCTION % TIME_INDEX, DATA_SIZE
                        if (TIME_FUNCTION % TIMES(i) > TIME) then
                            PREVIOUS_TIME  = TIME_FUNCTION % TIMES(i - 1)
                            PREVIOUS_VALUE = TIME_FUNCTION % VARIABLES(i - 1, VARIABLE_NO)
                            NEXT_TIME      = TIME_FUNCTION % TIMES(i)
                            NEXT_VALUE     = TIME_FUNCTION % VARIABLES(i, VARIABLE_NO)
                            exit
                        end if
                        
                        TIME_FUNCTION % TIME_INDEX = TIME_FUNCTION % TIME_INDEX + 1
                    end do

                    if (TIME_FUNCTION % INTERPOLATE == 1) then
                        VALUE = LDPS_INTERPOLATE &
                            (TIME, PREVIOUS_TIME,  PREVIOUS_VALUE, NEXT_TIME, NEXT_VALUE)
                    else
                        VALUE = PREVIOUS_VALUE
                    end if

                    VALUE = VALUE * &
                         TIME_FUNCTION % SCALE_FACTORS(VARIABLE_NO) * &
                         TIME_FUNCTION % UNIT_CONVERSION_FACTORS(VARIABLE_NO)
                end if
            end if
        end if
    end function VALUE_FROM_TIME_SERIE

    function GET_TIME_INDEX(TIME_FUNCTION, TIME) result(TIME_INDEX)

        implicit none
        type(TIME_SERIE), intent(in) :: TIME_FUNCTION
        real(kind = DBL_PRECISION), intent(in) :: TIME

        integer :: TIME_INDEX

        integer :: DATA_SIZE, i

        real(kind = DBL_PRECISION) :: PREVIOUS_TIME, NEXT_TIME, PREVIOUS_VALUE, NEXT_VALUE

        TIME_INDEX = 0

        if (TIME_FUNCTION % ALLOCATED == 0) then 
            TIME_INDEX = 0
        else
            if(TIME < TIME_FUNCTION % TIMES(1)) then
                TIME_INDEX = 0
            else
                DATA_SIZE = TIME_FUNCTION % DATA_SIZE

                if(TIME > TIME_FUNCTION % TIMES(DATA_SIZE)) then
                    TIME_INDEX = 0
                else

                    do i = 1, DATA_SIZE

                        if (abs(TIME_FUNCTION % TIMES(i) -  TIME) < 1.0D-10) then
                            TIME_INDEX = i
                            exit
                        end if
                    end do
                end if
            end if
        end if
    end function GET_TIME_INDEX


    subroutine MULTIPLY_TIME_SERIE_WITH_SCALAR(TIME_FUNCTION, SCALAR)
        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION
        real(kind = DBL_PRECISION), intent(in) :: SCALAR

        integer :: DATA_SIZE, NUMBER_OF_VARIABLES, i, j

        DATA_SIZE = TIME_FUNCTION % DATA_SIZE
        NUMBER_OF_VARIABLES = TIME_FUNCTION % NUMBER_OF_VARIABLES

        do i = 1, DATA_SIZE
            do j = 1, NUMBER_OF_VARIABLES
                TIME_FUNCTION % VARIABLES(i, j) = TIME_FUNCTION % VARIABLES(i, j) * SCALAR
            end do
        end do
    end subroutine MULTIPLY_TIME_SERIE_WITH_SCALAR


    subroutine MULTIPLY_TS_COLUMN_WITH_SCALAR(TIME_FUNCTION, SCALAR, COLUMN)

        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION
        real(kind = DBL_PRECISION), intent(in) :: SCALAR
        integer, intent(in) :: COLUMN

        integer :: DATA_SIZE, NUMBER_OF_VARIABLES, i

        DATA_SIZE = TIME_FUNCTION % DATA_SIZE
        NUMBER_OF_VARIABLES = TIME_FUNCTION % NUMBER_OF_VARIABLES

        if ((COLUMN > NUMBER_OF_VARIABLES).or.(COLUMN < 1)) then
            write(6,*) 'ERROR'
            write(6,*) 'SUBROUTINE : MULTIPLY_TS_COLUMN_WITH_SCALAR'

            write(6,*) 'The column that should be multiplied with a scalar ', &
            &          'does not exist in the time serie.'
            stop
        end if

        do i = 1, DATA_SIZE

            TIME_FUNCTION % VARIABLES(i, COLUMN) = &
            &               TIME_FUNCTION % VARIABLES(i, COLUMN) * SCALAR
        end do

    end subroutine MULTIPLY_TS_COLUMN_WITH_SCALAR


    subroutine ADD_TS_COLUMN_WITH_SCALAR(TIME_FUNCTION, SCALAR, COLUMN)

        implicit none
        type(TIME_SERIE), intent(inout) :: TIME_FUNCTION
        real(kind = DBL_PRECISION), intent(in) :: SCALAR
        integer, intent(in) :: COLUMN

        integer :: DATA_SIZE, NUMBER_OF_VARIABLES, i

        DATA_SIZE = TIME_FUNCTION % DATA_SIZE
        NUMBER_OF_VARIABLES = TIME_FUNCTION % NUMBER_OF_VARIABLES

        if ((COLUMN > NUMBER_OF_VARIABLES).or.(COLUMN < 1)) then
            write(6,*) 'ERROR'
            write(6,*) 'SUBROUTINE : ADD_TS_COLUMN_WITH_SCALAR'

            write(6,*) 'The column that should be added with a scalar ', &
                       'does not exist in the time serie.'
            stop
        end if

        do i = 1, DATA_SIZE

            TIME_FUNCTION % VARIABLES(i, COLUMN) = &
                 TIME_FUNCTION % VARIABLES(i, COLUMN) + SCALAR
        end do
    end subroutine ADD_TS_COLUMN_WITH_SCALAR

end module TIME_SERIES