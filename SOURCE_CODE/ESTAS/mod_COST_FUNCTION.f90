module COST_FUNCTION
    implicit none

    type MEASURED_VALUE_DS
        integer :: NUM_BOXES
        integer, pointer, dimension(:) :: NUM_DATA_POINTS
        integer, pointer, dimension(:) :: BOX_NOS
        integer, pointer, dimension(:) :: FORCING_TS_NOS
        integer, pointer, dimension(:) :: FORCING_TS_VAR_NOS
    end type MEASURED_VALUE_DS

contains

    subroutine ALLOC_MEASURED_VALUE(MEASURED_VALUE, NUM_BOXES)
        implicit none
        type(MEASURED_VALUE_DS), intent(inout) :: MEASURED_VALUE
        integer, intent(in) :: NUM_BOXES

        allocate(MEASURED_VALUE % NUM_DATA_POINTS   (NUM_BOXES))
        allocate(MEASURED_VALUE % BOX_NOS           (NUM_BOXES))
        allocate(MEASURED_VALUE % FORCING_TS_NOS    (NUM_BOXES))
        allocate(MEASURED_VALUE % FORCING_TS_VAR_NOS(NUM_BOXES))

        MEASURED_VALUE % NUM_BOXES = NUM_BOXES
    end subroutine ALLOC_MEASURED_VALUE



    subroutine DEALLOC_MEASURED_VALUE(MEASURED_VALUE)
        implicit none
        type(MEASURED_VALUE_DS), intent(inout) :: MEASURED_VALUE

        deallocate(MEASURED_VALUE % NUM_DATA_POINTS)
        deallocate(MEASURED_VALUE % BOX_NOS)
        deallocate(MEASURED_VALUE % FORCING_TS_NOS)
        deallocate(MEASURED_VALUE % FORCING_TS_VAR_NOS)

        MEASURED_VALUE % NUM_BOXES = 0
    end subroutine DEALLOC_MEASURED_VALUE

end module COST_FUNCTION