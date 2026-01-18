module PELAGIC_BOX_MODEL

    use GLOBAL
    use TIME_SERIES
    use PELAGIC_BOX
    use PELAGIC_LINK
    use EXTERNAL_FORCING
    use INITIAL_CONDITIONS
    use BASIN
    use UTILS_1
    use COST_FUNCTION

    implicit none

    type PELAGIC_BOX_MODEL_DS
        integer :: NUM_PELAGIC_STATE_VARS
        integer :: NUM_BASINS
        integer :: NUM_BATHYMETRIES
        integer :: NUM_PELAGIC_BOXES
        integer :: NUM_PELAGIC_INIT_COND_SETS
        integer :: NUM_PELAGIC_ADVECTIVE_LINKS
        integer :: NUM_PELAGIC_DISPERSIVE_LINKS
        integer :: NUM_SETTLING_VELOCITIES
        integer :: NUM_OPEN_BOUNDARIES
        integer :: NUM_MASS_LOADS
        integer :: NUM_MASS_WITHDRAWALS

        integer :: NUM_FLOW_TS
        integer :: NUM_MIXING_TS
        integer :: NUM_FORCING_TS

        integer :: NUM_MODEL_CONSTANTS
        integer :: NUM_DRIVING_FUNCTIONS
        integer :: NUM_FLAGS
        integer :: NUM_PROCESS_RATES
        integer :: NUM_SAVED_OUTPUTS

        integer :: NUM_EXERGY_COMPONENTS
        integer :: PRODUCE_COST_FUNC

        character(len = 30), pointer, dimension(:) :: PELAGIC_STATE_VAR_NAMES

        real(kind = DBL), pointer, dimension(:) :: MEASUREMENT_ERRORS

        type(PELAGIC_BASIN_DS), pointer, dimension(:) :: BASINS
        type(BATHYMETRY_DS)   , pointer, dimension(:) :: BATHYMETRIES
        type(PELAGIC_BOX_DS)  , pointer, dimension(:) :: PELAGIC_BOXES

        type(PELAGIC_INIT_COND_SET_DS)  , pointer, dimension(:) :: PELAGIC_INIT_COND_SETS
        type(PELAGIC_ADVECTIVE_LINK_DS) , pointer, dimension(:) :: PELAGIC_ADVECTIVE_LINKS
        type(PELAGIC_DISPERSIVE_LINK_DS), pointer, dimension(:) :: PELAGIC_DISPERSIVE_LINKS

        type(OPEN_BOUNDARY_DS)  , pointer, dimension(:) :: OPEN_BOUNDARIES
        type(MASS_LOAD_DS)      , pointer, dimension(:) :: MASS_LOADS
        type(MASS_WITHDRAWAL_DS), pointer, dimension(:) :: MASS_WITHDRAWALS

        type(TIME_SERIE), pointer, dimension(:) :: FORCING_TS
        type(TIME_SERIE), pointer, dimension(:) :: FLOWS
        type(TIME_SERIE), pointer, dimension(:) :: MIXING_EXCHANGES
        type(TIME_SERIE), pointer, dimension(:) :: SETTLING_VELOCITIES

        real(kind = DBL), pointer, dimension(:,:) :: FORCINGS

        integer :: DAY_OF_YEAR
        !Exergy
        integer :: CALCULATE_PELAGIC_EXERGY
        integer :: CREATE_PELAGIC_EXERGY_OUTPUTS
        integer :: CREATE_PELAGIC_SAVED_OUTPUT
        integer :: START_REPEAT_NO_PEL_EX_OUTS
        integer :: CREATE_PELAGIC_ECOL_OUTPUT

        real(kind = DBL)    , pointer, dimension(:) :: EXERGY_COMPONENTS
        real(kind = DBL)    , pointer, dimension(:) :: AVERAGED_EXERGY_COMPONENTS
        character(len = 128), pointer, dimension(:) :: EXERGY_COMPONENT_NAMES
        real(kind = DBL) :: TOTAL_EXERGY

        !Internal data storage
        real(kind = DBL), pointer, dimension(:,:)   :: VOLUME_DERIVS
        real(kind = DBL), pointer, dimension(:,:)   :: SALT_ADVECTION_DERIVS
        real(kind = DBL), pointer, dimension(:,:)   :: TEMP_ADVECTION_DERIVS
        real(kind = DBL), pointer, dimension(:,:,:) :: ECOL_ADVECTION_DERIVS
        real(kind = DBL), pointer, dimension(:,:)   :: SALT_DISPERSION_DERIVS
        real(kind = DBL), pointer, dimension(:,:)   :: TEMP_DISPERSION_DERIVS
        real(kind = DBL), pointer, dimension(:,:,:) :: ECOL_DISPERSION_DERIVS
        real(kind = DBL), pointer, dimension(:,:,:) :: ECOL_SETTLING_DERIVS
        real(kind = DBL), pointer, dimension(:,:,:) :: ECOL_MASS_LOAD_DERIVS
        real(kind = DBL), pointer, dimension(:,:,:) :: ECOL_MASS_WITHDRAWAL_DERIVS
        real(kind = DBL), pointer, dimension(:,:,:) :: ECOL_KINETIC_DERIVS
        real(kind = DBL), pointer, dimension(:,:,:) :: ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS
        real(kind = DBL), pointer, dimension(:,:)   :: MODEL_CONSTANTS

        !OUTPUT STORAGE
        real(kind = DBL), pointer, dimension(:,:,:) :: ECOL_RESULTS
        real(kind = DBL), pointer, dimension(:,:,:) :: AREA_BASED_ECOL_RESULTS
        real(kind = DBL), pointer, dimension(:,:,:) :: SAVED_OUTPUT_RESULTS
        real(kind = DBL), pointer, dimension(:,:,:) :: EXERGY_RESULTS

        !MEASURED VALUES
        type(MEASURED_VALUE_DS), pointer, dimension(:) :: MEASURED_VALUES
        integer :: PRODUCE_COST_FUNCTION

        !SAVED OUTPUTS
        character(len = 128), pointer, dimension(:) :: SAVED_OUTPUT_NAMES

        !PROCESSES
        real(kind = DBL), pointer, dimension(:,:,:) :: PROCESS_RATE_RESULTS

        !PELAGIC OUTPUT INFORMATION
        integer, pointer, dimension(:) :: PRODUCE_PEL_STATE_VAR_OUTPUTS
        integer, pointer, dimension(:) :: PRODUCE_PEL_PROCESS_RATE_OUTPUTS
        integer, pointer, dimension(:) :: PRODUCE_PEL_MASS_BALANCE_OUTPUTS
        integer :: CREATE_STATE_VARIABLE_OUTPUT

        integer, allocatable, dimension(:, :, :) :: SEDIMENT_FLUX_TS_NOS
        integer, allocatable, dimension(:, :, :) :: SEDIMENT_FLUX_TS_VAR_NOS

        integer, pointer, dimension(:) :: ADVECTION_ON
        integer, pointer, dimension(:) :: DIFFUSION_ON
        integer, pointer, dimension(:) :: SETTLING_ON
        integer, pointer, dimension(:) :: STATE_VAR_OUTPUT_TYPES
        integer, pointer, dimension(:) :: STATE_VAR_INITIAL_CONDITION_TYPES
    end type PELAGIC_BOX_MODEL_DS


contains


    subroutine ALLOC_PELAGIC_BOX_MODEL_DATA &
               (PELAGIC_BOX_MODEL_DATA      ,  &
                NUM_PELAGIC_STATE_VARS      , NUM_MODEL_CONSTANTS         , &
                NUM_BASINS                  , NUM_BATHYMETRIES            , &
                NUM_PELAGIC_BOXES           , NUM_PELAGIC_INIT_COND_SETS  , &
                NUM_PELAGIC_ADVECTIVE_LINKS , NUM_PELAGIC_DISPERSIVE_LINKS, &
                NUM_SETTLING_VELOCITIES     , NUM_OPEN_BOUNDARIES         , &
                NUM_FLOW_TS                 , NUM_MIXING_TS               , &
                NUM_MASS_LOADS              , NUM_MASS_WITHDRAWALS        , &
                NUM_FORCING_TS)

        implicit none

        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA
        integer, intent(in) :: NUM_PELAGIC_STATE_VARS
        integer, intent(in) :: NUM_MODEL_CONSTANTS
        integer, intent(in) :: NUM_BASINS
        integer, intent(in) :: NUM_BATHYMETRIES
        integer, intent(in) :: NUM_PELAGIC_BOXES
        integer, intent(in) :: NUM_PELAGIC_INIT_COND_SETS
        integer, intent(in) :: NUM_PELAGIC_ADVECTIVE_LINKS
        integer, intent(in) :: NUM_PELAGIC_DISPERSIVE_LINKS
        integer, intent(in) :: NUM_SETTLING_VELOCITIES
        integer, intent(in) :: NUM_OPEN_BOUNDARIES
        integer, intent(in) :: NUM_FLOW_TS
        integer, intent(in) :: NUM_MIXING_TS
        integer, intent(in) :: NUM_MASS_LOADS
        integer, intent(in) :: NUM_MASS_WITHDRAWALS
        integer, intent(in) :: NUM_FORCING_TS

        integer :: i

        allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_STATE_VAR_NAMES(NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_MODEL_DATA % MEASUREMENT_ERRORS(NUM_PELAGIC_STATE_VARS))

        allocate(PELAGIC_BOX_MODEL_DATA % MODEL_CONSTANTS(NUM_MODEL_CONSTANTS, NUM_PELAGIC_BOXES))
        allocate(PELAGIC_BOX_MODEL_DATA % BASINS(NUM_BASINS))
        allocate(PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(NUM_BATHYMETRIES))
        allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(NUM_PELAGIC_BOXES))
        allocate(PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(NUM_PELAGIC_BOXES, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % SALT_ADVECTION_DERIVS(NUM_PELAGIC_BOXES, 1))
        allocate(PELAGIC_BOX_MODEL_DATA % TEMP_ADVECTION_DERIVS(NUM_PELAGIC_BOXES, 1))
        allocate(PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS(NUM_PELAGIC_BOXES, NUM_PELAGIC_STATE_VARS, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % SALT_DISPERSION_DERIVS(NUM_PELAGIC_BOXES, 1))
        allocate(PELAGIC_BOX_MODEL_DATA % TEMP_DISPERSION_DERIVS(NUM_PELAGIC_BOXES, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS               &
                     (NUM_PELAGIC_BOXES, NUM_PELAGIC_STATE_VARS, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS                 &
                     (NUM_PELAGIC_BOXES, NUM_PELAGIC_STATE_VARS, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % ECOL_MASS_LOAD_DERIVS                &
                     (NUM_PELAGIC_BOXES, NUM_PELAGIC_STATE_VARS, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % ECOL_MASS_WITHDRAWAL_DERIVS          &
                     (NUM_PELAGIC_BOXES, NUM_PELAGIC_STATE_VARS, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % ECOL_KINETIC_DERIVS                  &
                     (NUM_PELAGIC_BOXES, NUM_PELAGIC_STATE_VARS, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS &
                     (NUM_PELAGIC_BOXES, NUM_PELAGIC_STATE_VARS, 1))

        allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_INIT_COND_SETS               &
                     (NUM_PELAGIC_INIT_COND_SETS))

        do i = 1, NUM_PELAGIC_INIT_COND_SETS
            call ALLOC_PELAGIC_INIT_COND_SET&
                     (PELAGIC_BOX_MODEL_DATA % PELAGIC_INIT_COND_SETS(i), &
                      NUM_PELAGIC_STATE_VARS)
        end do

        allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS  &
                     (NUM_PELAGIC_ADVECTIVE_LINKS))

        allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS &
                    (NUM_PELAGIC_DISPERSIVE_LINKS))

        allocate(PELAGIC_BOX_MODEL_DATA % FLOWS(NUM_FLOW_TS))

        allocate(PELAGIC_BOX_MODEL_DATA % MIXING_EXCHANGES(NUM_MIXING_TS))

        allocate(PELAGIC_BOX_MODEL_DATA % SETTLING_VELOCITIES(NUM_SETTLING_VELOCITIES))

        allocate(PELAGIC_BOX_MODEL_DATA % OPEN_BOUNDARIES(NUM_OPEN_BOUNDARIES))

        do i = 1, NUM_OPEN_BOUNDARIES
            call ALLOC_OPEN_BOUNDARY_DATA &
                     (PELAGIC_BOX_MODEL_DATA % OPEN_BOUNDARIES(i), NUM_PELAGIC_STATE_VARS)
        end do

        allocate(PELAGIC_BOX_MODEL_DATA % MASS_LOADS(NUM_MASS_LOADS))

        do i = 1, NUM_MASS_LOADS
            call ALLOC_MASS_LOAD_DATA &
                     (PELAGIC_BOX_MODEL_DATA % MASS_LOADS(i), NUM_PELAGIC_STATE_VARS)
        end do

        allocate(PELAGIC_BOX_MODEL_DATA % MASS_WITHDRAWALS(NUM_MASS_WITHDRAWALS))

        do i = 1, NUM_MASS_WITHDRAWALS
            call ALLOC_MASS_WITHDRAWAL_DATA &
                     (PELAGIC_BOX_MODEL_DATA % MASS_WITHDRAWALS(i), NUM_PELAGIC_STATE_VARS)
        end do

        allocate(PELAGIC_BOX_MODEL_DATA % FORCING_TS(NUM_FORCING_TS))

        allocate(PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_STATE_VAR_OUTPUTS   (NUM_PELAGIC_BOXES))
        allocate(PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_PROCESS_RATE_OUTPUTS(NUM_PELAGIC_BOXES))
        allocate(PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_MASS_BALANCE_OUTPUTS(NUM_PELAGIC_BOXES))

        allocate(PELAGIC_BOX_MODEL_DATA % ADVECTION_ON(NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_MODEL_DATA % DIFFUSION_ON(NUM_PELAGIC_STATE_VARS))
        allocate(PELAGIC_BOX_MODEL_DATA % SETTLING_ON (NUM_PELAGIC_STATE_VARS))

        allocate(PELAGIC_BOX_MODEL_DATA % &
                 STATE_VAR_OUTPUT_TYPES           (NUM_PELAGIC_STATE_VARS))

        allocate(PELAGIC_BOX_MODEL_DATA % &
                 STATE_VAR_INITIAL_CONDITION_TYPES(NUM_PELAGIC_STATE_VARS))

        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS       = NUM_PELAGIC_STATE_VARS
        PELAGIC_BOX_MODEL_DATA % NUM_MODEL_CONSTANTS          = NUM_MODEL_CONSTANTS
        PELAGIC_BOX_MODEL_DATA % NUM_BASINS                   = NUM_BASINS
        PELAGIC_BOX_MODEL_DATA % NUM_BATHYMETRIES             = NUM_BATHYMETRIES
        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES            = NUM_PELAGIC_BOXES
        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_INIT_COND_SETS   = NUM_PELAGIC_INIT_COND_SETS
        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_ADVECTIVE_LINKS  = NUM_PELAGIC_ADVECTIVE_LINKS
        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_DISPERSIVE_LINKS = NUM_PELAGIC_DISPERSIVE_LINKS
        PELAGIC_BOX_MODEL_DATA % NUM_FLOW_TS                  = NUM_FLOW_TS
        PELAGIC_BOX_MODEL_DATA % NUM_MIXING_TS                = NUM_MIXING_TS
        PELAGIC_BOX_MODEL_DATA % NUM_SETTLING_VELOCITIES      = NUM_SETTLING_VELOCITIES
        PELAGIC_BOX_MODEL_DATA % NUM_OPEN_BOUNDARIES          = NUM_OPEN_BOUNDARIES
        PELAGIC_BOX_MODEL_DATA % NUM_MASS_LOADS               = NUM_MASS_LOADS
        PELAGIC_BOX_MODEL_DATA % NUM_MASS_WITHDRAWALS         = NUM_MASS_WITHDRAWALS
        PELAGIC_BOX_MODEL_DATA % NUM_FORCING_TS               = NUM_FORCING_TS

        do i = 1, NUM_PELAGIC_BOXES
            call ALLOC_PELAGIC_BOX_DATA &
                    (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i), NUM_PELAGIC_STATE_VARS)
        end do

    end subroutine ALLOC_PELAGIC_BOX_MODEL_DATA


    subroutine DEALLOC_PELAGIC_BOX_MODEL_DATA(PELAGIC_BOX_MODEL_DATA)

        implicit none
        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA

        integer :: i

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_BASINS
            call DEALLOCATE_BASIN_DATA(PELAGIC_BOX_MODEL_DATA % BASINS(i))
        end do

        deallocate(PELAGIC_BOX_MODEL_DATA % BASINS)

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_BATHYMETRIES
            call DEALLOCATE_BATHYMETRY_DATA(PELAGIC_BOX_MODEL_DATA % BATHYMETRIES(i))
        end do

        deallocate(PELAGIC_BOX_MODEL_DATA % BATHYMETRIES)
        deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_STATE_VAR_NAMES)
        deallocate(PELAGIC_BOX_MODEL_DATA % MODEL_CONSTANTS)
        deallocate(PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS)

        deallocate(PELAGIC_BOX_MODEL_DATA % SALT_ADVECTION_DERIVS)
        deallocate(PELAGIC_BOX_MODEL_DATA % TEMP_ADVECTION_DERIVS)
        deallocate(PELAGIC_BOX_MODEL_DATA % ECOL_ADVECTION_DERIVS)

        deallocate(PELAGIC_BOX_MODEL_DATA % SALT_DISPERSION_DERIVS)
        deallocate(PELAGIC_BOX_MODEL_DATA % TEMP_DISPERSION_DERIVS)
        deallocate(PELAGIC_BOX_MODEL_DATA % ECOL_DISPERSION_DERIVS)

        deallocate(PELAGIC_BOX_MODEL_DATA % ECOL_SETTLING_DERIVS)
        deallocate(PELAGIC_BOX_MODEL_DATA % ECOL_MASS_LOAD_DERIVS)
        deallocate(PELAGIC_BOX_MODEL_DATA % ECOL_MASS_WITHDRAWAL_DERIVS)
        deallocate(PELAGIC_BOX_MODEL_DATA % ECOL_KINETIC_DERIVS)
        deallocate(PELAGIC_BOX_MODEL_DATA % ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS)

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            call DEALLOC_PELAGIC_BOX_DATA(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i))
        end do

        deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES)

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_INIT_COND_SETS
            call DEALLOC_PELAGIC_INIT_COND_SET(PELAGIC_BOX_MODEL_DATA % PELAGIC_INIT_COND_SETS(i))
        end do

        deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_INIT_COND_SETS)
        deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_ADVECTIVE_LINKS)
        deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_DISPERSIVE_LINKS)
        deallocate(PELAGIC_BOX_MODEL_DATA % FLOWS)
        deallocate(PELAGIC_BOX_MODEL_DATA % MIXING_EXCHANGES)
        deallocate(PELAGIC_BOX_MODEL_DATA % SETTLING_VELOCITIES)

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_OPEN_BOUNDARIES
            call DEALLOC_OPEN_BOUNDARY_DATA(PELAGIC_BOX_MODEL_DATA % OPEN_BOUNDARIES(i))
        end do

        deallocate(PELAGIC_BOX_MODEL_DATA % OPEN_BOUNDARIES)

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_MASS_LOADS
            call DEALLOC_MASS_LOAD_DATA(PELAGIC_BOX_MODEL_DATA % MASS_LOADS(i))
        end do

        deallocate(PELAGIC_BOX_MODEL_DATA % MASS_LOADS)

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_MASS_WITHDRAWALS
            call DEALLOC_MASS_WITHDRAWAL_DATA(PELAGIC_BOX_MODEL_DATA % MASS_WITHDRAWALS(i))
        end do

        deallocate(PELAGIC_BOX_MODEL_DATA % MASS_WITHDRAWALS)

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
            call DEALLOC_MEASURED_VALUE(PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES(i))
        end do

        deallocate(PELAGIC_BOX_MODEL_DATA % MEASURED_VALUES)
        deallocate(PELAGIC_BOX_MODEL_DATA % MEASUREMENT_ERRORS)

        deallocate(PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_STATE_VAR_OUTPUTS)
        deallocate(PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_PROCESS_RATE_OUTPUTS)
        deallocate(PELAGIC_BOX_MODEL_DATA % PRODUCE_PEL_MASS_BALANCE_OUTPUTS)

        if (allocated(PELAGIC_BOX_MODEL_DATA % SEDIMENT_FLUX_TS_NOS)) then
            deallocate(PELAGIC_BOX_MODEL_DATA % SEDIMENT_FLUX_TS_NOS)
        end if

        if (allocated(PELAGIC_BOX_MODEL_DATA % SEDIMENT_FLUX_TS_VAR_NOS)) then
            deallocate(PELAGIC_BOX_MODEL_DATA % SEDIMENT_FLUX_TS_VAR_NOS)
        end if

        deallocate(PELAGIC_BOX_MODEL_DATA % ADVECTION_ON)
        deallocate(PELAGIC_BOX_MODEL_DATA % DIFFUSION_ON)
        deallocate(PELAGIC_BOX_MODEL_DATA % SETTLING_ON )

        deallocate(PELAGIC_BOX_MODEL_DATA % STATE_VAR_OUTPUT_TYPES           )
        deallocate(PELAGIC_BOX_MODEL_DATA % STATE_VAR_INITIAL_CONDITION_TYPES)

        PELAGIC_BOX_MODEL_DATA % MODEL_CONSTANTS              = 0
        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES            = 0
        PELAGIC_BOX_MODEL_DATA % NUM_BASINS                   = 0
        PELAGIC_BOX_MODEL_DATA % NUM_BATHYMETRIES             = 0
        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_INIT_COND_SETS   = 0
        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_ADVECTIVE_LINKS  = 0
        PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_DISPERSIVE_LINKS = 0
        PELAGIC_BOX_MODEL_DATA % NUM_FLOW_TS                  = 0
        PELAGIC_BOX_MODEL_DATA % NUM_MIXING_TS                = 0
        PELAGIC_BOX_MODEL_DATA % NUM_SETTLING_VELOCITIES      = 0
        PELAGIC_BOX_MODEL_DATA % NUM_OPEN_BOUNDARIES          = 0
        PELAGIC_BOX_MODEL_DATA % NUM_MASS_LOADS               = 0
        PELAGIC_BOX_MODEL_DATA % NUM_MASS_WITHDRAWALS         = 0
        PELAGIC_BOX_MODEL_DATA % NUM_FORCING_TS               = 0
    end subroutine DEALLOC_PELAGIC_BOX_MODEL_DATA

end module PELAGIC_BOX_MODEL
