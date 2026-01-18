module EXTERNAL_FORCING

    use TIME_SERIES

    implicit none

    type MASS_LOAD_DS
        integer :: MASS_LOAD_NO
        integer :: NUM_STATE_VARS
        integer, pointer, dimension(:) :: FORCING_TS_NOS
        integer, pointer, dimension(:) :: FORCING_TS_VAR_NOS
    end type MASS_LOAD_DS


    type MASS_WITHDRAWAL_DS
        integer :: MASS_WITHDRAWAL_NO
        integer :: NUM_STATE_VARS
        integer, pointer, dimension(:) :: FORCING_TS_NOS
        integer, pointer, dimension(:) :: FORCING_TS_VAR_NOS
    end type MASS_WITHDRAWAL_DS


    type OPEN_BOUNDARY_DS
        integer :: OPEN_BOUNDARY_NO
        integer :: NUM_STATE_VARS
        integer, pointer, dimension(:) :: FORCING_TS_NOS
        integer, pointer, dimension(:) :: FORCING_TS_VAR_NOS
    end type OPEN_BOUNDARY_DS


contains


    subroutine ALLOC_MASS_LOAD_DATA(MASS_LOAD_DATA, NUM_STATE_VARS)
        implicit none

        type(MASS_LOAD_DS), intent(inout) :: MASS_LOAD_DATA
        integer, intent(in) :: NUM_STATE_VARS

        allocate(MASS_LOAD_DATA % FORCING_TS_NOS    (NUM_STATE_VARS))
        allocate(MASS_LOAD_DATA % FORCING_TS_VAR_NOS(NUM_STATE_VARS))

        MASS_LOAD_DATA % NUM_STATE_VARS = NUM_STATE_VARS
    end subroutine ALLOC_MASS_LOAD_DATA


    subroutine DEALLOC_MASS_LOAD_DATA(MASS_LOAD_DATA)
        implicit none

        type(MASS_LOAD_DS), intent(inout) :: MASS_LOAD_DATA

        deallocate(MASS_LOAD_DATA % FORCING_TS_NOS)
        deallocate(MASS_LOAD_DATA % FORCING_TS_VAR_NOS)

        MASS_LOAD_DATA % NUM_STATE_VARS = 0
    end subroutine DEALLOC_MASS_LOAD_DATA


    subroutine ALLOC_MASS_WITHDRAWAL_DATA(MASS_WITHDRAWAL_DATA, NUM_STATE_VARS)
        implicit none

        type(MASS_WITHDRAWAL_DS), intent(inout) :: MASS_WITHDRAWAL_DATA
        integer, intent(in) :: NUM_STATE_VARS

        allocate(MASS_WITHDRAWAL_DATA % FORCING_TS_NOS    (NUM_STATE_VARS))
        allocate(MASS_WITHDRAWAL_DATA % FORCING_TS_VAR_NOS(NUM_STATE_VARS))

        MASS_WITHDRAWAL_DATA % NUM_STATE_VARS = NUM_STATE_VARS
    end subroutine ALLOC_MASS_WITHDRAWAL_DATA


    subroutine DEALLOC_MASS_WITHDRAWAL_DATA(MASS_WITHDRAWAL_DATA)
        implicit none

        type(MASS_WITHDRAWAL_DS), intent(inout) :: MASS_WITHDRAWAL_DATA

        deallocate(MASS_WITHDRAWAL_DATA % FORCING_TS_NOS)
        deallocate(MASS_WITHDRAWAL_DATA % FORCING_TS_VAR_NOS)

        MASS_WITHDRAWAL_DATA % NUM_STATE_VARS = 0
    end subroutine DEALLOC_MASS_WITHDRAWAL_DATA


    subroutine ALLOC_OPEN_BOUNDARY_DATA(OPEN_BOUNDARY_DATA, NUM_STATE_VARS)
        implicit none

        type(OPEN_BOUNDARY_DS), intent(inout) :: OPEN_BOUNDARY_DATA
        integer, intent(in) :: NUM_STATE_VARS

        allocate(OPEN_BOUNDARY_DATA % FORCING_TS_NOS    (NUM_STATE_VARS))
        allocate(OPEN_BOUNDARY_DATA % FORCING_TS_VAR_NOS(NUM_STATE_VARS))

        OPEN_BOUNDARY_DATA % NUM_STATE_VARS = NUM_STATE_VARS
    end subroutine ALLOC_OPEN_BOUNDARY_DATA


    subroutine DEALLOC_OPEN_BOUNDARY_DATA(OPEN_BOUNDARY_DATA)
        implicit none

        type(OPEN_BOUNDARY_DS), intent(inout) :: OPEN_BOUNDARY_DATA

        deallocate(OPEN_BOUNDARY_DATA % FORCING_TS_NOS)
        deallocate(OPEN_BOUNDARY_DATA % FORCING_TS_VAR_NOS)

        OPEN_BOUNDARY_DATA % NUM_STATE_VARS = 0
    end subroutine DEALLOC_OPEN_BOUNDARY_DATA

end module EXTERNAL_FORCING