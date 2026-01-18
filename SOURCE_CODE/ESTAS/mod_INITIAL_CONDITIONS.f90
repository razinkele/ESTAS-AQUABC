module INITIAL_CONDITIONS

    use GLOBAL 
    implicit none


    type PELAGIC_INIT_COND_SET_DS
        integer :: NUM_PELAGIC_STATE_VARS
        real(kind = DBL), pointer, dimension(:) :: PELAGIC_CONCENTRATIONS
    end type


    type SED_DIAG_INIT_COND_SET_DS
        integer :: NUM_VARS
        real(kind = DBL), pointer, dimension(:) :: CONCENTRATIONS
    end type

contains


    subroutine ALLOC_PELAGIC_INIT_COND_SET(PELAGIC_INIT_COND_SET_DATA, NUM_PELAGIC_STATE_VARS)

        implicit none
        type(PELAGIC_INIT_COND_SET_DS) :: PELAGIC_INIT_COND_SET_DATA
        integer, intent(in) :: NUM_PELAGIC_STATE_VARS

        allocate(PELAGIC_INIT_COND_SET_DATA % PELAGIC_CONCENTRATIONS(NUM_PELAGIC_STATE_VARS))
        PELAGIC_INIT_COND_SET_DATA % NUM_PELAGIC_STATE_VARS = NUM_PELAGIC_STATE_VARS
    end subroutine ALLOC_PELAGIC_INIT_COND_SET



    subroutine DEALLOC_PELAGIC_INIT_COND_SET(PELAGIC_INIT_COND_SET_DATA)

        implicit none
        type(PELAGIC_INIT_COND_SET_DS) :: PELAGIC_INIT_COND_SET_DATA

        deallocate(PELAGIC_INIT_COND_SET_DATA % PELAGIC_CONCENTRATIONS)

        PELAGIC_INIT_COND_SET_DATA % NUM_PELAGIC_STATE_VARS = 0
    end subroutine DEALLOC_PELAGIC_INIT_COND_SET



    subroutine READ_INITIAL_CONCENTRATIONS(PELAGIC_INIT_COND_SET_DATA, IN_FILE)

        implicit none
        type(PELAGIC_INIT_COND_SET_DS) :: PELAGIC_INIT_COND_SET_DATA
        integer, intent(in) :: IN_FILE

        integer :: i
        integer :: j
        integer :: PELAGIC_STATE_VAR_NO

        real(kind = DBL) :: PELAGIC_CONCENTRATION

        read(unit = IN_FILE, fmt = *) 
        read(unit = IN_FILE, fmt = *) 

        do i = 1, PELAGIC_INIT_COND_SET_DATA % NUM_PELAGIC_STATE_VARS
            read(unit = IN_FILE, fmt = *) PELAGIC_STATE_VAR_NO, PELAGIC_CONCENTRATION
            
            PELAGIC_INIT_COND_SET_DATA % &
                PELAGIC_CONCENTRATIONS(PELAGIC_STATE_VAR_NO) = PELAGIC_CONCENTRATION
        end do

    end subroutine READ_INITIAL_CONCENTRATIONS



    subroutine ALLOC_SED_DIAG_INIT_COND_SET(SED_DIAG_INIT_COND_SET_DATA, NUM_VARS)

        implicit none
        type(SED_DIAG_INIT_COND_SET_DS) :: SED_DIAG_INIT_COND_SET_DATA
        integer, intent(in) :: NUM_VARS

        allocate(SED_DIAG_INIT_COND_SET_DATA % CONCENTRATIONS(NUM_VARS))
        SED_DIAG_INIT_COND_SET_DATA % NUM_VARS = NUM_VARS
    end subroutine ALLOC_SED_DIAG_INIT_COND_SET



    subroutine DEALLOC_SED_DIAG_INIT_COND_SET(SED_DIAG_INIT_COND_SET_DATA)

        implicit none
        type(SED_DIAG_INIT_COND_SET_DS) :: SED_DIAG_INIT_COND_SET_DATA

        deallocate(SED_DIAG_INIT_COND_SET_DATA % CONCENTRATIONS)
        SED_DIAG_INIT_COND_SET_DATA % NUM_VARS = 0
    end subroutine DEALLOC_SED_DIAG_INIT_COND_SET


    subroutine READ_SED_DIAG_INIT_CONDS(SED_DIAG_INIT_COND_SET_DATA, IN_FILE)

        implicit none
        type(SED_DIAG_INIT_COND_SET_DS) :: SED_DIAG_INIT_COND_SET_DATA
        integer, intent(in) :: IN_FILE

        integer :: i
        integer :: VAR_NO
        real(kind = DBL) :: CONCENTRATION

        read(unit = IN_FILE, fmt = *) 
        read(unit = IN_FILE, fmt = *) 

        do i = 1, SED_DIAG_INIT_COND_SET_DATA % NUM_VARS
            read(unit = IN_FILE, fmt = *) VAR_NO, CONCENTRATION
            SED_DIAG_INIT_COND_SET_DATA % CONCENTRATIONS(VAR_NO) = CONCENTRATION
        end do

    end subroutine READ_SED_DIAG_INIT_CONDS

end module INITIAL_CONDITIONS