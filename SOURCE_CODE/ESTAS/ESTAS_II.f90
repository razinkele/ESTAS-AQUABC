program ESTAS_II
    use GLOBAL
    use AQUATIC_MODEL
    !use INITIALIZE_AQUATIC_MODEL
    use SIMULATE
    use BOTTOM_SEDIMENTS
    
    implicit none

    integer :: NUM_COMMAND_ARGUMENTS
    character(len = 2048)  :: INPUT_FILE_NAME
    type(AQUATIC_MODEL_DS) :: AQUATIC_MODEL_DATA

    integer :: i, j

    USE_PELAGIC_CONSTANTS_FILE_NAME    = 0
    PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT = 0

    ! These variables will be read from external file
    PRODUCE_COCOA_OUTPUTS         = 0
    CRIT_SHEAR_FNAME_FROM_OUTSIDE = 0
    
    NUM_COMMAND_ARGUMENTS = command_argument_count();

    if (NUM_COMMAND_ARGUMENTS < 1) then
        open(unit = 10, file = 'INPUT.txt', status = 'OLD')
    else
        select case (NUM_COMMAND_ARGUMENTS)
            case (1)
                call get_command_argument(1, INPUT_FILE_NAME)
                open(unit = 10, file = trim(adjustl(INPUT_FILE_NAME)), status = 'OLD')
                
            case (2)
                call get_command_argument(1, INPUT_FILE_NAME)
                call get_command_argument(2, PELAGIC_CONSTANTS_FILE_NAME)
                
                open(unit = 10, file = trim(adjustl(INPUT_FILE_NAME)), status = 'OLD')
                USE_PELAGIC_CONSTANTS_FILE_NAME = 1

            case (3)
                call get_command_argument(1, INPUT_FILE_NAME)
                call get_command_argument(2, PELAGIC_CONSTANTS_FILE_NAME)
                call get_command_argument(3, BINARY_PELAGIC_OUTPUT_FILE_NAME)
                
                open(unit = 10, file = trim(adjustl(INPUT_FILE_NAME)), status = 'OLD')
                USE_PELAGIC_CONSTANTS_FILE_NAME    = 1
                PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT = 1

            case (4:)
                call get_command_argument(1, INPUT_FILE_NAME)
                call get_command_argument(2, PELAGIC_CONSTANTS_FILE_NAME)
                call get_command_argument(3, BINARY_PELAGIC_OUTPUT_FILE_NAME)
                call get_command_argument(4, CRITICAL_SHEAR_STRESS_FILENAME)

                open(unit = 10, file = trim(adjustl(INPUT_FILE_NAME)), status = 'OLD')
                USE_PELAGIC_CONSTANTS_FILE_NAME    = 1
                PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT = 1
                CRIT_SHEAR_FNAME_FROM_OUTSIDE      = 1
        end select
    end if

    ! -----------------------------------------------------------------------------------
    ! INITIALIZATION OF THE WATER COLUMN MODEL
    ! -----------------------------------------------------------------------------------
    call READ_AQUATIC_MODEL_INPUTS(AQUATIC_MODEL_DATA, 10, 11)
    call RUN_SIMULATION(AQUATIC_MODEL_DATA)

    ! -----------------------------------------------------------------------------------
    ! Deallocate AQUABC water column arrays
    ! -----------------------------------------------------------------------------------
    deallocate(node_active      )
    deallocate(STATE_VARIABLES  )
    deallocate(DERIVATIVES      )
    deallocate(MODEL_CONSTANTS  )
    deallocate(DRIVING_FUNCTIONS)
    deallocate(FLAGS            )
    deallocate(PROCESS_RATES    )
    deallocate(SAVED_OUTPUTS    )
    deallocate(pH               )
    deallocate(CHLA             )
    deallocate(FLUXES_TO_WATER_COLUMN       )
    deallocate(FLUXES_OUTPUT_TO_WATER_COLUMN)
    ! -----------------------------------------------------------------------------------
    ! End of deallocate AQUABC water column arrays
    ! -----------------------------------------------------------------------------------
    
    deallocate(DISSOLVED_FRACTIONS           )
    deallocate(FRACTION_OF_DEPOSITION        )
    deallocate(SETTLING_RATES                )
    deallocate(NOT_DEPOSITED_FLUXES          )
    deallocate(FLUXES                        )
    deallocate(SETTLING_VELOCITIES_OUTPUT    )
    deallocate(EFFECTIVE_DISSLOVED_FRACTIONS )
    deallocate(EFFECTIVE_DEPOSITION_FRACTIONS)
    deallocate(DEPOSITION_AREA_RATIOS        )
 
	! -----------------------------------------------------------------------------------
    ! Deallocate AQUABC sediment arrays
    ! -----------------------------------------------------------------------------------
    if (MODEL_BOTTOM_SEDIMENTS > 1) then
        deallocate(INIT_SED_STATE_VARS)
        deallocate(SED_DEPTHS         )
        deallocate(SED_POROSITIES     )
        deallocate(SED_DENSITIES      )
        deallocate(PART_MIXING_COEFFS )
        deallocate(SED_DIFFUSIONS     )

        deallocate(SED_BURRIALS         )
        deallocate(SURF_WATER_CONCS     )
        deallocate(SED_TEMPS            )
        deallocate(SED_MODEL_CONSTANTS  )
        deallocate(PROCESSES_sed        )
        deallocate(SED_DRIVING_FUNCTIONS)
        deallocate(FLUXES_TO_SEDIMENTS  )
        deallocate(SED_BURRIAL_RATE_OUTPUTS)
    
        deallocate(H_ERODEP )
        deallocate(SED_FLAGS)

        deallocate(FINAL_SED_STATE_VARS )
        deallocate(FLUXES_FROM_SEDIMENTS)
        deallocate(SED_OUTPUTS          )
        deallocate(SED_SAVED_OUTPUTS    )
    end if
    ! -----------------------------------------------------------------------------------
    ! End of deallocate AQUABC sediment arrays
    ! -----------------------------------------------------------------------------------
 
    close(1000)
    close(1001)

    if (MODEL_BOTTOM_SEDIMENTS > 1) then
        close(1023)
        close(1021)
    end if
    
    if (PRODUCE_COCOA_OUTPUTS > 0) then
        close(2020)

        if (MODEL_BOTTOM_SEDIMENTS > 1) then
            close(2021)
            close(2031)
            close(2032)
        end if
    end if

end program ESTAS_II