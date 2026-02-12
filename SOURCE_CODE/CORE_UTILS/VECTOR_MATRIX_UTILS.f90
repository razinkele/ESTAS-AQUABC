!************************************************************************
!**************************co2sys routines***************************
!************************************************************************
!Includes:
!  module VECTOR_MATRIX_UTILS

module VECTOR_MATRIX_UTILS
    use AQUABC_II_GLOBAL
    implicit none
contains
    !**************************************************************************
    ! UTILITY FUNCTIONS BETWEEN FORTRAN AND MATLAB
    !**************************************************************************
    function SAME_ELEMENTS_INETEGER_VECTOR(VECTOR) result(UNIQUE)

        !INGOING VARIABLES
        implicit none
        integer, intent(in), dimension(:) :: VECTOR
        !END OF INGOING VARIABLES

        !RETURNED VARIABLE
        integer :: UNIQUE
        !END OF RETURNED VARIABLE

        ! AUXILLARY VARIABLES
        integer :: FIRST_ELEMENTS_VALUE
        integer :: i
        ! END OF AUXILLARY VARIABLES

        UNIQUE = 1
        FIRST_ELEMENTS_VALUE = VECTOR(1)

        do i = 1, size(VECTOR)
            if (VECTOR(i) .ne. FIRST_ELEMENTS_VALUE) then
                UNIQUE = 0
                return
            end if
        end do

    end function SAME_ELEMENTS_INETEGER_VECTOR



    subroutine ASSIGN_DBL_VECTOR_CONTENT(ARRAY_TO, ARRAY_FROM)
    ! subroutine is not used more?
    
        !INGOING VARIABLES
        implicit none
        real(kind = DBL_PREC), intent(in)   , dimension(:) :: ARRAY_FROM
        real(kind = DBL_PREC), intent(inout), allocatable, dimension(:) :: ARRAY_TO
        integer :: ierr
        !END OF INGOING VARIABLES

        if(allocated(ARRAY_TO)) then
            deallocate(ARRAY_TO)
        endif

        allocate(ARRAY_TO(size(ARRAY_FROM)), stat=ierr)
        if (ierr /= 0) then
            write(*,*) 'ERROR: ASSIGN_DBL_VECTOR_CONTENT: allocation failed, size=', size(ARRAY_FROM)
            stop
        end if
        ARRAY_TO(:) = ARRAY_FROM(:)
    end subroutine ASSIGN_DBL_VECTOR_CONTENT


    subroutine ASSIGN_INT_VECTOR_CONTENT(ARRAY_TO, ARRAY_FROM)
     ! subroutine is not used more?
        !INGOING VARIABLES
        implicit none
        integer, intent(in)   , dimension(:) :: ARRAY_FROM
        integer, intent(inout), allocatable, dimension(:) :: ARRAY_TO
        integer :: ierr
        !END OF INGOING VARIABLES

        if(allocated(ARRAY_TO)) then
            deallocate(ARRAY_TO)
        endif

        allocate(ARRAY_TO(size(ARRAY_FROM)), stat=ierr)
        if (ierr /= 0) then
            write(*,*) 'ERROR: ASSIGN_INT_VECTOR_CONTENT: allocation failed, size=', size(ARRAY_FROM)
            stop
        end if
        ARRAY_TO(:) = ARRAY_FROM(:)
    end subroutine ASSIGN_INT_VECTOR_CONTENT


    subroutine LOGICAL_VECTOR_TO_INT_VECTOR(LOGICAL_VECTOR, INT_VECTOR)
    ! subroutine is not used more?
        implicit none
        logical, intent(in)   , dimension(:)              :: LOGICAL_VECTOR
        integer, intent(inout), allocatable, dimension(:) :: INT_VECTOR
        integer :: ierr

        if(size(LOGICAL_VECTOR) > 0) then
            if(allocated(INT_VECTOR)) then
                deallocate(INT_VECTOR)
            end if

        allocate(INT_VECTOR(size(LOGICAL_VECTOR)), stat=ierr)
        if (ierr /= 0) then
            write(*,*) 'ERROR: LOGICAL_VECTOR_TO_INT_VECTOR: allocation failed, size=', size(LOGICAL_VECTOR)
            stop
        end if
        
            INT_VECTOR = 0

            where(LOGICAL_VECTOR)
                INT_VECTOR = 1
            end where
        else
            write(*,*) 'Error in subroutine '
            write(*,*) 'LOGICAL_VECTOR_TO_INT_VECTOR(LOGICAL_VECTOR, INT_VECTOR)'
            write(*,*) 'The size of the argument LOGICAL_VECTOR passed to subroutine '
            write(*,*) 'LOGICAL_VECTOR_TO_INT_VECTOR should be greater than zero'
            stop            
        end if
    end subroutine LOGICAL_VECTOR_TO_INT_VECTOR


    subroutine GENERATE_INDEX_ARRAY(INDEX_ARRAY_MASK, INDEX_ARRAY)
        implicit none
        integer, intent(in)   , allocatable, dimension(:) :: INDEX_ARRAY_MASK
        integer, intent(inout), allocatable, dimension(:) :: INDEX_ARRAY
        integer :: i
        integer :: j
        integer :: ierr

        if (allocated(INDEX_ARRAY)) then
            deallocate(INDEX_ARRAY)
        end if

        allocate(INDEX_ARRAY(sum(INDEX_ARRAY_MASK)), stat=ierr)
        if (ierr /= 0) then
            write(*,*) 'ERROR: GENERATE_INDEX_ARRAY: allocation failed, size=', sum(INDEX_ARRAY_MASK)
            stop
        end if
        j = 1

        do i = lbound(INDEX_ARRAY_MASK,1), ubound(INDEX_ARRAY_MASK,1)
            if(INDEX_ARRAY_MASK(i) == 1) then
                INDEX_ARRAY(j) = i
                j = j + 1
            end if
        end do

    end subroutine GENERATE_INDEX_ARRAY

    ! END OF UTILITY FUNCTIONS BETWEEN FORTAN AND MATLAB
end module VECTOR_MATRIX_UTILS

!*****************************************************
!*****************************************************