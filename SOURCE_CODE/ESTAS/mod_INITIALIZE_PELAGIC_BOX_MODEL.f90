module INITIALIZE_PELAGIC_BOX_MODEL

    use GLOBAL
    use PELAGIC_BOX_MODEL
    use PELAGIC_ECOLOGY
    use PELAGIC_EXERGY

    implicit none

contains

    subroutine INIT_PELAGIC_BOX_MODEL(PELAGIC_BOX_MODEL_DATA, TIME)

        implicit none
        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA
        real(kind = DBL), intent(in) :: TIME

        integer :: i
        integer :: j
        integer :: PELAGIC_INIT_COND_SET_NO
        integer :: MAXIMUM_VALUE

        real(kind = DBL) :: PELAGIC_CONCENTRATION

        integer :: BOX_NO
        integer :: OVERLAYING_BOX_NO
        integer :: UNDERLAYING_BOX_NO
        integer :: NUM_PELAGIC_BOXES
        integer :: FORCING_TS_NO

        integer, dimension(PELAGIC_BOX_MODEL_DATA % NUM_FORCING_TS) :: NUM_FORCING_VARIABLES

        AUX_OUTPUT_UNIT = 20

        if (PELAGIC_BOX_MODEL_DATA % NUM_FORCING_TS > 0) then 
            MAXIMUM_VALUE = PELAGIC_BOX_MODEL_DATA % FORCING_TS(1) % NUMBER_OF_VARIABLES

            do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_FORCING_TS
                
                NUM_FORCING_VARIABLES(i) = &
                    PELAGIC_BOX_MODEL_DATA % FORCING_TS(i) % NUMBER_OF_VARIABLES

                if (NUM_FORCING_VARIABLES(i) > MAXIMUM_VALUE) then
                    MAXIMUM_VALUE = NUM_FORCING_VARIABLES(i)
                end if
            end do

            allocate(PELAGIC_BOX_MODEL_DATA % &
                         FORCINGS(PELAGIC_BOX_MODEL_DATA % NUM_FORCING_TS, MAXIMUM_VALUE))
        end if

        !ALLOCATE MEMORY FOR PELAGIC ECOLOGY MODEL
        call ALLOCATE_PELAGIC_ECOLOGY(PELAGIC_BOX_MODEL_DATA)

        !ALLOCATE MEMORY FOR PELAGIC EXERGY MODEL
        call ALLOCATE_PELAGIC_EXERGY (PELAGIC_BOX_MODEL_DATA)

        !INITIAL CONCENTRATIONS
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES

            PELAGIC_INIT_COND_SET_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % PELAGIC_INIT_COND_SET_NO

            do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                PELAGIC_CONCENTRATION = &
                    PELAGIC_BOX_MODEL_DATA % &
                        PELAGIC_INIT_COND_SETS(PELAGIC_INIT_COND_SET_NO) % &
                            PELAGIC_CONCENTRATIONS(j)

                !If the inital concentration is given in g/m^2, convert it tp g/m^3
                if (PELAGIC_BOX_MODEL_DATA % STATE_VAR_INITIAL_CONDITION_TYPES(j) > 1) then

                    PELAGIC_CONCENTRATION = &
                        PELAGIC_CONCENTRATION * &
                        (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_AREA / &
                         PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME)
                end if
                
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                    CONCENTRATIONS(j) = PELAGIC_CONCENTRATION

                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) %  MASSES(j) = &
                    PELAGIC_CONCENTRATION * &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME
            end do
        end do


        !TOPOLOGY
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_BASINS
            BOX_NO = PELAGIC_BOX_MODEL_DATA % BASINS(i) % PELAGIC_BOXES(1)
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % OVERLAYING_PELAGIC_BOX = 0
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % SURFACE_BOX            = 1

            if (PELAGIC_BOX_MODEL_DATA % BASINS(i) % NUM_PELAGIC_BOXES == 1) then
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                    UNDERLAYING_PELAGIC_BOX = 0
            else
                UNDERLAYING_BOX_NO = &
                    PELAGIC_BOX_MODEL_DATA % BASINS(i) % PELAGIC_BOXES(2)

                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                    UNDERLAYING_PELAGIC_BOX = UNDERLAYING_BOX_NO

                do j = 2, PELAGIC_BOX_MODEL_DATA % BASINS(i) % NUM_PELAGIC_BOXES - 1
                    BOX_NO = PELAGIC_BOX_MODEL_DATA % BASINS(i) % PELAGIC_BOXES(j)
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % SURFACE_BOX = 0
                    
                    OVERLAYING_BOX_NO  = &
                        PELAGIC_BOX_MODEL_DATA % BASINS(i) % PELAGIC_BOXES(j - 1)
                    
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                        OVERLAYING_PELAGIC_BOX = OVERLAYING_BOX_NO
                    
                    UNDERLAYING_BOX_NO = &
                        PELAGIC_BOX_MODEL_DATA % BASINS(i) % PELAGIC_BOXES(j + 1)
                    
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                        UNDERLAYING_PELAGIC_BOX = UNDERLAYING_BOX_NO
                end do

                NUM_PELAGIC_BOXES = &
                    PELAGIC_BOX_MODEL_DATA % BASINS(i) % NUM_PELAGIC_BOXES
                
                BOX_NO = &
                    PELAGIC_BOX_MODEL_DATA % BASINS(i) % PELAGIC_BOXES(NUM_PELAGIC_BOXES)
                
                OVERLAYING_BOX_NO  = PELAGIC_BOX_MODEL_DATA % BASINS(i) % &
                    PELAGIC_BOXES(NUM_PELAGIC_BOXES - 1)
                
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                    OVERLAYING_PELAGIC_BOX = OVERLAYING_BOX_NO
                
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % &
                    UNDERLAYING_PELAGIC_BOX = 0
                
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % SURFACE_BOX = 0
            end if
        end do

        !COST FUNCTION
        if (PELAGIC_BOX_MODEL_DATA % PRODUCE_COST_FUNCTION > 0) then
            if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT == 0) then                           
                do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

                    do j = 1, PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i) % NUM_BOXES
                        
                        FORCING_TS_NO = &
                            PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i) % &
                                FORCING_TS_NOS(j)

                        PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i) % &
                            NUM_DATA_POINTS(j) = &
                        PELAGIC_BOX_MODEL_DATA % FORCING_TS(FORCING_TS_NO) % DATA_SIZE
                    end do
                end do
            end if
        end if

        call INIT_TRANSPORT_FIELDS(PELAGIC_BOX_MODEL_DATA)

    end subroutine INIT_PELAGIC_BOX_MODEL

end module INITIALIZE_PELAGIC_BOX_MODEL