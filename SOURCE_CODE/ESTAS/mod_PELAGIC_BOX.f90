module PELAGIC_BOX

    use GLOBAL
    use EXTERNAL_FORCING

    implicit none

    type PELAGIC_BOX_DS
        integer :: BOX_NO

        integer :: NUM_MASS_LOADS
        integer :: NUM_MASS_WITHDRAWALS

        integer :: BASIN_NO
        integer :: BASIN_BOX_NO
        integer :: BATHYMETRY_NO

        integer :: OVERLAYING_PELAGIC_BOX
        real(kind = DBL) :: OVERLAYING_AREA

        integer :: UNDERLAYING_PELAGIC_BOX
        real(kind = DBL) :: UNDERLAYING_AREA

        real(kind = DBL) :: VOLUME
        real(kind = DBL) :: DEPTH
        real(kind = DBL) :: SURFACE_AREA
        real(kind = DBL) :: BOTTOM_AREA
        real(kind = DBL) :: SURFACE_ELEVATION
        real(kind = DBL) :: BOTTOM_ELEVATION

        real(kind = DBL), pointer, dimension(:) :: MASSES
        real(kind = DBL), pointer, dimension(:) :: CONCENTRATIONS
        real(kind = DBL), pointer, dimension(:) :: DRIVING_VAR_CONCENTRATIONS
        real(kind = DBL), pointer, dimension(:) :: DISSOLVED_FRACTIONS
        real(kind = DBL), pointer, dimension(:) :: DEPOSITED_FRACTIONS
        integer, pointer, dimension(:) :: CHLA_SUPRESSION_OF_SETTLING

        integer :: PELAGIC_INIT_COND_SET_NO
        
        integer, pointer, dimension(:) :: MASS_LOAD_NOS
        integer, pointer, dimension(:) :: MASS_WITHDRAWAL_NOS
        integer, pointer, dimension(:) :: SETTLING_TS_NOS

        integer :: TEMPERATURE_TS_NO
        integer :: TEMPERATURE_TS_VAR_NO

        integer :: SALINITY_TS_NO
        integer :: SALINITY_TS_VAR_NO

        integer :: SOLAR_RADIATION_TS_NO
        integer :: SOLAR_RADIATION_TS_VAR_NO

        integer :: FRACTION_OF_DAY_TS_NO
        integer :: FRACTION_OF_DAY_TS_VAR_NO

        integer :: AIR_TEMPERATURE_TS_NO
        integer :: AIR_TEMPERATURE_TS_VAR_NO

        integer :: WIND_SPEED_TS_NO
        integer :: WIND_SPEED_TS_VAR_NO

        integer :: PRECIPITATION_TS_NO
        integer :: PRECIPITATION_TS_VAR_NO

        integer :: EVAPORATION_TS_NO
        integer :: EVAPORATION_TS_VAR_NO

        integer :: ICE_FRACTION_TS_NO
        integer :: ICE_FRACTION_TS_VAR_NO

        integer :: SURFACE_BOX

        !Interface to pelagic ecological model
        real(kind = DBL), pointer, dimension(:) :: DRIVING_FUNCTIONS
        integer         , pointer, dimension(:) :: FLAGS

        real(kind = DBL), pointer, dimension(:) :: PELAGIC_ECOLOGY_DERIVATIVES
        real(kind = DBL), pointer, dimension(:) :: PROCESS_RATES
        real(kind = DBL), pointer, dimension(:) :: SAVED_OUTPUTS

        integer :: PELAGIC_ECOLOGY_CALLED_BEFORE

        real(kind = DBL), pointer, dimension(:) :: EXERGY_COMPONENTS
        real(kind = DBL), pointer, dimension(:) :: AVERAGED_EXERGY_COMPONENTS

        real(kind = DBL) :: TOTAL_EXERGY
    end type PELAGIC_BOX_DS


contains


    subroutine ALLOC_PELAGIC_BOX_DATA(PELAGIC_BOX_DATA, NUM_PELAGIC_STATE_VARS)

        implicit none
        type(PELAGIC_BOX_DS), intent(inout) :: PELAGIC_BOX_DATA
        integer, intent(in) :: NUM_PELAGIC_STATE_VARS

        allocate(PELAGIC_BOX_DATA % MASSES                     (NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_DATA % CONCENTRATIONS             (NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_DATA % SETTLING_TS_NOS            (NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_DATA % DISSOLVED_FRACTIONS        (NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_DATA % DEPOSITED_FRACTIONS        (NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_DATA % CHLA_SUPRESSION_OF_SETTLING(NUM_PELAGIC_STATE_VARS))

        PELAGIC_BOX_DATA % PELAGIC_ECOLOGY_CALLED_BEFORE = 0
    end subroutine ALLOC_PELAGIC_BOX_DATA


    subroutine DEALLOC_PELAGIC_BOX_DATA(PELAGIC_BOX_DATA)

        implicit none
        type(PELAGIC_BOX_DS), intent(inout) :: PELAGIC_BOX_DATA

        deallocate(PELAGIC_BOX_DATA % MASSES)
        deallocate(PELAGIC_BOX_DATA % CONCENTRATIONS)
        deallocate(PELAGIC_BOX_DATA % SETTLING_TS_NOS)
        deallocate(PELAGIC_BOX_DATA % DISSOLVED_FRACTIONS)
        deallocate(PELAGIC_BOX_DATA % DEPOSITED_FRACTIONS)
        deallocate(PELAGIC_BOX_DATA % CHLA_SUPRESSION_OF_SETTLING)

        deallocate(PELAGIC_BOX_DATA % PELAGIC_ECOLOGY_DERIVATIVES)
        deallocate(PELAGIC_BOX_DATA % PROCESS_RATES)
        deallocate(PELAGIC_BOX_DATA % SAVED_OUTPUTS)

        deallocate(PELAGIC_BOX_DATA % EXERGY_COMPONENTS)
        deallocate(PELAGIC_BOX_DATA % AVERAGED_EXERGY_COMPONENTS)

        PELAGIC_BOX_DATA % PELAGIC_ECOLOGY_CALLED_BEFORE = 0
    end subroutine DEALLOC_PELAGIC_BOX_DATA


    subroutine ALLOC_TOPLOGY_AND_LOADS(PELAGIC_BOX_DATA, NUM_MASS_LOADS, NUM_MASS_WITHDRAWALS)           

        implicit none
        type(PELAGIC_BOX_DS), intent(inout) :: PELAGIC_BOX_DATA
        integer, intent(in) :: NUM_MASS_LOADS
        integer, intent(in) :: NUM_MASS_WITHDRAWALS

        allocate(PELAGIC_BOX_DATA % MASS_LOAD_NOS       (NUM_MASS_LOADS))
        allocate(PELAGIC_BOX_DATA % MASS_WITHDRAWAL_NOS (NUM_MASS_WITHDRAWALS))

        PELAGIC_BOX_DATA % NUM_MASS_LOADS        = NUM_MASS_LOADS
        PELAGIC_BOX_DATA % NUM_MASS_WITHDRAWALS  = NUM_MASS_WITHDRAWALS
    end subroutine ALLOC_TOPLOGY_AND_LOADS


    subroutine DEALLOC_TOPLOGY_AND_LOADS(PELAGIC_BOX_DATA)

        implicit none
        type(PELAGIC_BOX_DS), intent(inout) :: PELAGIC_BOX_DATA

        deallocate(PELAGIC_BOX_DATA % MASS_LOAD_NOS)
        deallocate(PELAGIC_BOX_DATA % MASS_WITHDRAWAL_NOS)

        PELAGIC_BOX_DATA % NUM_MASS_LOADS        = 0
        PELAGIC_BOX_DATA % NUM_MASS_WITHDRAWALS  = 0
    end subroutine DEALLOC_TOPLOGY_AND_LOADS


    subroutine ALLOCATE_PELAGIC_KINETICS &
               (PELAGIC_BOX_DATA, NUM_DRIVING_FUNCTIONS, NUM_PROCESS_RATES, &
                NUM_SAVED_OUTPUTS)

        implicit none
        type(PELAGIC_BOX_DS), intent(inout) :: PELAGIC_BOX_DATA
        integer, intent(in) :: NUM_DRIVING_FUNCTIONS
        integer, intent(in) :: NUM_PROCESS_RATES
        integer, intent(in) :: NUM_SAVED_OUTPUTS

        allocate(PELAGIC_BOX_DATA % DRIVING_FUNCTIONS(NUM_DRIVING_FUNCTIONS))
        allocate(PELAGIC_BOX_DATA % PROCESS_RATES    (NUM_PROCESS_RATES))
        allocate(PELAGIC_BOX_DATA % SAVED_OUTPUTS    (NUM_SAVED_OUTPUTS))
    end subroutine ALLOCATE_PELAGIC_KINETICS


    subroutine DEALLOCATE_PELAGIC_KINETICS(PELAGIC_BOX_DATA)

        implicit none
        type(PELAGIC_BOX_DS), intent(inout) :: PELAGIC_BOX_DATA

        deallocate(PELAGIC_BOX_DATA % DRIVING_FUNCTIONS)
        deallocate(PELAGIC_BOX_DATA % PROCESS_RATES)
        deallocate(PELAGIC_BOX_DATA % SAVED_OUTPUTS)
    end subroutine DEALLOCATE_PELAGIC_KINETICS

end module PELAGIC_BOX