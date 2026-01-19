! This module is used in ESTAS framework, with stadart subroutine and
! interfaces, however all the subroutines must be programmed specific
! to pelagic kinetics.
module PELAGIC_ECOLOGY

    use GLOBAL
    use PELAGIC_BOX_MODEL
    use UTILS_1
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use ALLELOPATHY

    implicit none

contains

    subroutine ALLOCATE_PELAGIC_ECOLOGY(PELAGIC_BOX_MODEL_DATA)
        implicit none
        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA

        integer :: i

        integer :: NUM_MODEL_CONSTANTS
        integer :: NUM_DRIVING_FUNCTIONS
        integer :: NUM_FLAGS
        integer :: NUM_PROCESS_RATES
        integer :: NUM_SAVED_OUTPUTS
        integer :: NUM_STATE_VARIABLES

        NUM_DRIVING_FUNCTIONS = 10
        NUM_FLAGS             = 5

        if (CONSIDER_ALLELOPATHY > 0) then
            NUM_PROCESS_RATES = (nstate + NUM_ALLOLOPATHY_STATE_VARS) * NDIAGVAR
        else
            NUM_PROCESS_RATES = nstate * NDIAGVAR
        end if

        NUM_SAVED_OUTPUTS     = n_saved_outputs

        allocate(PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_NAMES(NUM_SAVED_OUTPUTS))

        PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_NAMES(1) = "FE_II_DISS_OVER_FE_II"
        PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_NAMES(2) = "FE_III_DISS_OVER_FE_III"
        PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_NAMES(3) = "MN_II_DISS_OVER_MN_II"
        PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_NAMES(4) = "MN_IV_DISS_OVER_MN_IV"
        PELAGIC_BOX_MODEL_DATA % SAVED_OUTPUT_NAMES(5) = "DIP_OVER_IP"

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % DRIVING_FUNCTIONS(NUM_DRIVING_FUNCTIONS))
            allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS(NUM_FLAGS))
            allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % PROCESS_RATES(NUM_PROCESS_RATES))
            allocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SAVED_OUTPUTS(NUM_SAVED_OUTPUTS))

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % DRIVING_FUNCTIONS = 0.0D0
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS             = 0
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % PROCESS_RATES     = 0.0D0
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SAVED_OUTPUTS     = 0.0D0

            PELAGIC_BOX_MODEL_DATA % NUM_DRIVING_FUNCTIONS = NUM_DRIVING_FUNCTIONS
            PELAGIC_BOX_MODEL_DATA % NUM_FLAGS             = NUM_FLAGS
            PELAGIC_BOX_MODEL_DATA % NUM_PROCESS_RATES     = NUM_PROCESS_RATES
            PELAGIC_BOX_MODEL_DATA % NUM_SAVED_OUTPUTS     = NUM_SAVED_OUTPUTS
        end do

    end subroutine ALLOCATE_PELAGIC_ECOLOGY


    subroutine DEALLOCATE_PELAGIC_ECOLOGY(PELAGIC_BOX_MODEL_DATA)
        implicit none
        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA

        integer :: i

        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % DRIVING_FUNCTIONS)
            deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS)
            deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % PROCESS_RATES)
            deallocate(PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SAVED_OUTPUTS)

            PELAGIC_BOX_MODEL_DATA % NUM_DRIVING_FUNCTIONS = 0
            PELAGIC_BOX_MODEL_DATA % NUM_FLAGS             = 0
            PELAGIC_BOX_MODEL_DATA % NUM_PROCESS_RATES     = 0
            PELAGIC_BOX_MODEL_DATA % NUM_SAVED_OUTPUTS     = 0
        end do

    end subroutine DEALLOCATE_PELAGIC_ECOLOGY


    subroutine UPDATE_PELAGIC_DRIVING_FUNCS(PELAGIC_BOX_MODEL_DATA)
        implicit none
        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA

        integer :: i
        integer :: j
        integer :: FORCING_TS_NO
        integer :: FORCING_TS_VAR_NO
        integer :: BOX_NO
        integer :: BASIN_NO
        integer :: BASIN_BOX_NO

        real(kind = DBL) :: DEPTH
        real(kind = DBL) :: LIGHT_EXTINCTION
        real(kind = DBL) :: SOLAR_RADIATION

        !OTHER FORCING FUNCTIONS RELATED TO PELAGIC BOXES
        do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES

            !Temperature
            FORCING_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % TEMPERATURE_TS_NO

            FORCING_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % TEMPERATURE_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                 DRIVING_FUNCTIONS(1) = &
            PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)

            !Salinity
            FORCING_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SALINITY_TS_NO

            FORCING_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SALINITY_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                 DRIVING_FUNCTIONS(2) = &
            PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)

            !Solar radiation
            if (PELAGIC_BOX_MODEL_DATA % &
                PELAGIC_BOXES(i) % OVERLAYING_PELAGIC_BOX /= 0) then

                BASIN_NO     = PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % BASIN_NO
                BASIN_BOX_NO = PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % BASIN_BOX_NO

                do j = 1, BASIN_BOX_NO

                    if (j == 1) then
                        BOX_NO            = &
                            PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % PELAGIC_BOXES(j)

                        FORCING_TS_NO     = &
                            PELAGIC_BOX_MODEL_DATA % &
                                PELAGIC_BOXES(BOX_NO) % SOLAR_RADIATION_TS_NO

                        FORCING_TS_VAR_NO = &
                            PELAGIC_BOX_MODEL_DATA % &
                                PELAGIC_BOXES(BOX_NO) % SOLAR_RADIATION_TS_VAR_NO

                        SOLAR_RADIATION   = &
                            PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)
                    else
                        BOX_NO = PELAGIC_BOX_MODEL_DATA % BASINS(BASIN_NO) % PELAGIC_BOXES(j - 1)

                        DEPTH  = (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % VOLUME) / &
                                 (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % SURFACE_AREA)

                        if (STRANGERSD(PELAGIC_BOX_MODEL_DATA % &
                                       PELAGIC_BOXES(BOX_NO) % SAVED_OUTPUTS(1)).eq.1) then
                            LIGHT_EXTINCTION = 0.1
                        else
                            LIGHT_EXTINCTION = &
                                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(BOX_NO) % SAVED_OUTPUTS(1)
                        end if

                        SOLAR_RADIATION = &
                            SOLAR_RADIATION * exp((-1.0D0) * LIGHT_EXTINCTION * DEPTH)
                    end if

                end do

                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                    DRIVING_FUNCTIONS(3) = SOLAR_RADIATION
            else
                FORCING_TS_NO     = &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SOLAR_RADIATION_TS_NO

                FORCING_TS_VAR_NO = &
                    PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SOLAR_RADIATION_TS_VAR_NO

                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                     DRIVING_FUNCTIONS(3) = &
                PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)
            end if

            !Fraction of day
            FORCING_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FRACTION_OF_DAY_TS_NO

            FORCING_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FRACTION_OF_DAY_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                 DRIVING_FUNCTIONS(4) = &
            PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)

            !Air temperature
            FORCING_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % AIR_TEMPERATURE_TS_NO

            FORCING_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % AIR_TEMPERATURE_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                 DRIVING_FUNCTIONS(5) = &
            PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)

            !Wind speed
            FORCING_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % WIND_SPEED_TS_NO

            FORCING_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % WIND_SPEED_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                 DRIVING_FUNCTIONS(6) = &
            PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)

            !Elavation
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                 DRIVING_FUNCTIONS(7) = &
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_ELEVATION

            !Depth
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                 DRIVING_FUNCTIONS(8) = &
            (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME) / &
            (PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_AREA)

            !Background light extinction
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                DRIVING_FUNCTIONS(9) = USER_ENTERED_K_B_E

            !----------------------------------------------------------------------------
            !Ice cover
            !----------------------------------------------------------------------------
            FORCING_TS_NO     = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % ICE_FRACTION_TS_NO

            FORCING_TS_VAR_NO = &
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % ICE_FRACTION_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % &
                 DRIVING_FUNCTIONS(10) = &
            PELAGIC_BOX_MODEL_DATA % FORCINGS(FORCING_TS_NO, FORCING_TS_VAR_NO)
            !----------------------------------------------------------------------------


            !Safe mode
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS(1) = 0

            !Flag for surface box
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS(2) = &
                 PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_BOX

            if (TIME_STEP_NO == 1) then
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS(3) = 1
            else
                PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS(3) = 0
            end if

            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS(4) = 2
            PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % FLAGS(5) = 2
        end do

    end subroutine


    subroutine GENERATE_PELAGIC_DERIVED_VARS &
               (STATE_VARIABLES, MODEL_CONSTANTS, SAVED_OUTPUTS, &
                TIME, BOX_NO, DERIVED_VARIABLES)

        use aquabc_pel_state_var_indexes

        implicit none

        real(kind = DBL), dimension(:), intent(in)    :: STATE_VARIABLES
        real(kind = DBL), dimension(:), intent(in)    :: MODEL_CONSTANTS
        real(kind = DBL), dimension(:), intent(in)    :: SAVED_OUTPUTS
        real(kind = DBL), dimension(:), intent(inout) :: DERIVED_VARIABLES
        real(kind = DBL), intent(in) :: TIME

        integer, intent(in) :: BOX_NO

        integer :: i

        real(kind = DBL) :: NH4_N
        real(kind = DBL) :: NO3_N
        real(kind = DBL) :: PO4_P
        real(kind = DBL) :: DISS_OXYGEN
        real(kind = DBL) :: DIA_C
        real(kind = DBL) :: CYN_C
        real(kind = DBL) :: FIX_CYN_C
        real(kind = DBL) :: OPA_C
        real(kind = DBL) :: ZOO_C
        real(kind = DBL) :: ZOO_N
        real(kind = DBL) :: ZOO_P
        real(kind = DBL) :: DET_PART_ORG_C
        real(kind = DBL) :: DET_PART_ORG_N
        real(kind = DBL) :: DET_PART_ORG_P
        real(kind = DBL) :: DISS_ORG_C
        real(kind = DBL) :: DISS_ORG_N
        real(kind = DBL) :: DISS_ORG_P
        real(kind = DBL) :: ALKALINITY
        real(kind = DBL) :: INORG_C

        real(kind = DBL) :: DIA_N_TO_C
        real(kind = DBL) :: DIA_P_TO_C
        real(kind = DBL) :: DIA_C_TO_CHLA

        real(kind = DBL) :: CYN_N_TO_C
        real(kind = DBL) :: CYN_P_TO_C
        real(kind = DBL) :: CYN_C_TO_CHLA
        real(kind = DBL) :: FIX_CYN_N_TO_C
        real(kind = DBL) :: FIX_CYN_P_TO_C
        real(kind = DBL) :: FIX_CYN_C_TO_CHLA
        real(kind = DBL) :: OPA_N_TO_C
        real(kind = DBL) :: OPA_P_TO_C
        real(kind = DBL) :: OPA_C_TO_CHLA

        real(kind = DBL) :: CHLA, TOC, TN, TP

        NH4_N          = STATE_VARIABLES(NH4_N_INDEX)
        NO3_N          = STATE_VARIABLES(NO3_N_INDEX)
        PO4_P          = STATE_VARIABLES(PO4_P_INDEX)
        DISS_OXYGEN    = STATE_VARIABLES(DISS_OXYGEN_INDEX)
        DIA_C          = STATE_VARIABLES(DIA_C_INDEX)
        ZOO_C          = STATE_VARIABLES(ZOO_C_INDEX)
        ZOO_N          = STATE_VARIABLES(ZOO_N_INDEX)
        ZOO_P          = STATE_VARIABLES(ZOO_P_INDEX)
        DET_PART_ORG_C = STATE_VARIABLES(DET_PART_ORG_C_INDEX)
        DET_PART_ORG_N = STATE_VARIABLES(DET_PART_ORG_N_INDEX)
        DET_PART_ORG_P = STATE_VARIABLES(DET_PART_ORG_P_INDEX)
        DISS_ORG_C     = STATE_VARIABLES(DISS_ORG_C_INDEX)
        DISS_ORG_N     = STATE_VARIABLES(DISS_ORG_N_INDEX)
        DISS_ORG_P     = STATE_VARIABLES(DISS_ORG_P_INDEX)
        CYN_C          = STATE_VARIABLES(CYN_C_INDEX)
        OPA_C          = STATE_VARIABLES(OPA_C_INDEX)
        FIX_CYN_C      = STATE_VARIABLES(FIX_CYN_C_INDEX)

        ! Diagnostic: report negative cyanobacteria states immediately when read from STATE_VARIABLES

        if (FIX_CYN_C < 0.0D0) then
            write(6,*) 'ALERT: NEGATIVE FIX_CYN_C: BOX=', BOX_NO, ' FIX_CYN_C=', FIX_CYN_C
        end if
        if (CYN_C < 0.0D0) then
            write(6,*) 'ALERT: NEGATIVE CYN_C: BOX=', BOX_NO, ' CYN_C=', CYN_C
        end if

        DIA_N_TO_C         = MODEL_CONSTANTS(24)
        DIA_P_TO_C         = MODEL_CONSTANTS(25)
        DIA_C_TO_CHLA      = MODEL_CONSTANTS(28)

        CYN_N_TO_C         = MODEL_CONSTANTS(47)
        CYN_P_TO_C         = MODEL_CONSTANTS(48)
        CYN_C_TO_CHLA      = MODEL_CONSTANTS(50)

        FIX_CYN_N_TO_C     = MODEL_CONSTANTS(69)
        FIX_CYN_P_TO_C     = MODEL_CONSTANTS(70)
        FIX_CYN_C_TO_CHLA  = MODEL_CONSTANTS(72)

        OPA_N_TO_C         = MODEL_CONSTANTS(93)
        OPA_P_TO_C         = MODEL_CONSTANTS(94)
        OPA_C_TO_CHLA      = MODEL_CONSTANTS(96)

        CHLA = ((DIA_C / DIA_C_TO_CHLA) + (CYN_C / CYN_C_TO_CHLA) + &
                (OPA_C / OPA_C_TO_CHLA)) * 1.0D3

        TOC  = DIA_C + CYN_C + OPA_C + FIX_CYN_C + ZOO_C + DET_PART_ORG_C + DISS_ORG_C

        TN   = (DIA_C       * DIA_N_TO_C)           + &
               (CYN_C       * CYN_N_TO_C)           + &
               (FIX_CYN_C   * FIX_CYN_N_TO_C)       + &
               (OPA_C       * OPA_N_TO_C)           + &
               ZOO_N + DET_PART_ORG_N + DISS_ORG_N + NH4_N + NO3_N

        TP   = (DIA_C       * DIA_P_TO_C)           + &
               (CYN_C       * CYN_P_TO_C)           + &
               (FIX_CYN_C   * FIX_CYN_P_TO_C)       + &
               (OPA_C       * OPA_P_TO_C)           + &
               ZOO_P + DET_PART_ORG_P + DISS_ORG_P + PO4_P

       DERIVED_VARIABLES(1) = CHLA
       DERIVED_VARIABLES(2) = TOC
       DERIVED_VARIABLES(3) = TN
       DERIVED_VARIABLES(4) = TP
    end subroutine GENERATE_PELAGIC_DERIVED_VARS


    subroutine INIT_PELAGIC_MODEL_CONSTANTS
        use GLOBAL
        use AQUABC_PELAGIC_MODEL_CONSTANTS

        implicit none
                                      K_A = MODEL_CONSTANTS(  1)
                                THETA_K_A = MODEL_CONSTANTS(  2)
                                      XKC = MODEL_CONSTANTS(  3)
                                    PHIMX = MODEL_CONSTANTS(  4)
                          KG_DIA_OPT_TEMP = MODEL_CONSTANTS(  5)
                          DIA_OPT_TEMP_LR = MODEL_CONSTANTS(  6)
                          DIA_OPT_TEMP_UR = MODEL_CONSTANTS(  7)
                           EFF_DIA_GROWTH = MODEL_CONSTANTS(  8)
                 KAPPA_DIA_UNDER_OPT_TEMP = MODEL_CONSTANTS(  9)
                 KAPPA_DIA_OVER_OPT_TEMP  = MODEL_CONSTANTS( 10)
                                KR_DIA_20 = MODEL_CONSTANTS( 11)
                             THETA_KR_DIA = MODEL_CONSTANTS( 12)
                                KD_DIA_20 = MODEL_CONSTANTS( 13)
                             THETA_KD_DIA = MODEL_CONSTANTS( 14)
                              KHS_DIN_DIA = MODEL_CONSTANTS( 15)
                              KHS_DIP_DIA = MODEL_CONSTANTS( 16)
                              KHS_DSi_DIA = MODEL_CONSTANTS( 17)
                               KHS_O2_DIA = MODEL_CONSTANTS( 18)
                            FRAC_DIA_EXCR = MODEL_CONSTANTS( 19)
                                  I_S_DIA = MODEL_CONSTANTS( 20)
                       DO_STR_HYPOX_DIA_D = MODEL_CONSTANTS( 21)
                        THETA_HYPOX_DIA_D = MODEL_CONSTANTS( 22)
                        EXPON_HYPOX_DIA_D = MODEL_CONSTANTS( 23)
                               DIA_N_TO_C = MODEL_CONSTANTS( 24)
                               DIA_P_TO_C = MODEL_CONSTANTS( 25)
                              DIA_Si_TO_C = MODEL_CONSTANTS( 26)
                              DIA_O2_TO_C = MODEL_CONSTANTS( 27)
                            DIA_C_TO_CHLA = MODEL_CONSTANTS( 28)
                          KG_CYN_OPT_TEMP = MODEL_CONSTANTS( 29)
                          CYN_OPT_TEMP_LR = MODEL_CONSTANTS( 30)
                          CYN_OPT_TEMP_UR = MODEL_CONSTANTS( 31)
                           EFF_CYN_GROWTH = MODEL_CONSTANTS( 32)
                 KAPPA_CYN_UNDER_OPT_TEMP = MODEL_CONSTANTS( 33)
                  KAPPA_CYN_OVER_OPT_TEMP = MODEL_CONSTANTS( 34)
                                KR_CYN_20 = MODEL_CONSTANTS( 35)
                             THETA_KR_CYN = MODEL_CONSTANTS( 36)
                                KD_CYN_20 = MODEL_CONSTANTS( 37)
                             THETA_KD_CYN = MODEL_CONSTANTS( 38)
                              KHS_DIN_CYN = MODEL_CONSTANTS( 39)
                              KHS_DIP_CYN = MODEL_CONSTANTS( 40)
                               KHS_O2_CYN = MODEL_CONSTANTS( 41)
                            FRAC_CYN_EXCR = MODEL_CONSTANTS( 42)
                                  I_S_CYN = MODEL_CONSTANTS( 43)
                       DO_STR_HYPOX_CYN_D = MODEL_CONSTANTS( 44)
                        THETA_HYPOX_CYN_D = MODEL_CONSTANTS( 45)
                        EXPON_HYPOX_CYN_D = MODEL_CONSTANTS( 46)
                               CYN_N_TO_C = MODEL_CONSTANTS( 47)
                               CYN_P_TO_C = MODEL_CONSTANTS( 48)
                              CYN_O2_TO_C = MODEL_CONSTANTS( 49)
                            CYN_C_TO_CHLA = MODEL_CONSTANTS( 50)
                      KG_FIX_CYN_OPT_TEMP = MODEL_CONSTANTS( 51)
                      FIX_CYN_OPT_TEMP_LR = MODEL_CONSTANTS( 52)
                      FIX_CYN_OPT_TEMP_UR = MODEL_CONSTANTS( 53)
                       EFF_FIX_CYN_GROWTH = MODEL_CONSTANTS( 54)
             KAPPA_FIX_CYN_UNDER_OPT_TEMP = MODEL_CONSTANTS( 55)
              KAPPA_FIX_CYN_OVER_OPT_TEMP = MODEL_CONSTANTS( 56)
                            KR_FIX_CYN_20 = MODEL_CONSTANTS( 57)
                         THETA_KR_FIX_CYN = MODEL_CONSTANTS( 58)
                            KD_FIX_CYN_20 = MODEL_CONSTANTS( 59)
                         THETA_KD_FIX_CYN = MODEL_CONSTANTS( 60)
                          KHS_DIN_FIX_CYN = MODEL_CONSTANTS( 61)
                          KHS_DIP_FIX_CYN = MODEL_CONSTANTS( 62)
                           KHS_O2_FIX_CYN = MODEL_CONSTANTS( 63)
                        FRAC_FIX_CYN_EXCR = MODEL_CONSTANTS( 64)
                              I_S_FIX_CYN = MODEL_CONSTANTS( 65)
                   DO_STR_HYPOX_FIX_CYN_D = MODEL_CONSTANTS( 66)
                    THETA_HYPOX_FIX_CYN_D = MODEL_CONSTANTS( 67)
                    EXPON_HYPOX_FIX_CYN_D = MODEL_CONSTANTS( 68)
                           FIX_CYN_N_TO_C = MODEL_CONSTANTS( 69)
                           FIX_CYN_P_TO_C = MODEL_CONSTANTS( 70)
                          FIX_CYN_O2_TO_C = MODEL_CONSTANTS( 71)
                        FIX_CYN_C_TO_CHLA = MODEL_CONSTANTS( 72)
                                    R_FIX = MODEL_CONSTANTS( 73)
                                    K_FIX = MODEL_CONSTANTS( 74)
                          KG_OPA_OPT_TEMP = MODEL_CONSTANTS( 75)
                          OPA_OPT_TEMP_LR = MODEL_CONSTANTS( 76)
                          OPA_OPT_TEMP_UR = MODEL_CONSTANTS( 77)
                           EFF_OPA_GROWTH = MODEL_CONSTANTS( 78)
                 KAPPA_OPA_UNDER_OPT_TEMP = MODEL_CONSTANTS( 79)
                  KAPPA_OPA_OVER_OPT_TEMP = MODEL_CONSTANTS( 80)
                                KR_OPA_20 = MODEL_CONSTANTS( 81)
                             THETA_KR_OPA = MODEL_CONSTANTS( 82)
                                KD_OPA_20 = MODEL_CONSTANTS( 83)
                             THETA_KD_OPA = MODEL_CONSTANTS( 84)
                              KHS_DIN_OPA = MODEL_CONSTANTS( 85)
                              KHS_DIP_OPA = MODEL_CONSTANTS( 86)
                               KHS_O2_OPA = MODEL_CONSTANTS( 87)
                            FRAC_OPA_EXCR = MODEL_CONSTANTS( 88)
                                  I_S_OPA = MODEL_CONSTANTS( 89)
                       DO_STR_HYPOX_OPA_D = MODEL_CONSTANTS( 90)
                        THETA_HYPOX_OPA_D = MODEL_CONSTANTS( 91)
                        EXPON_HYPOX_OPA_D = MODEL_CONSTANTS( 92)
                               OPA_N_TO_C = MODEL_CONSTANTS( 93)
                               OPA_P_TO_C = MODEL_CONSTANTS( 94)
                              OPA_O2_TO_C = MODEL_CONSTANTS( 95)
                            OPA_C_TO_CHLA = MODEL_CONSTANTS( 96)
                          KG_ZOO_OPT_TEMP = MODEL_CONSTANTS( 97)
                          ZOO_OPT_TEMP_LR = MODEL_CONSTANTS( 98)
                          ZOO_OPT_TEMP_UR = MODEL_CONSTANTS( 99)
                           EFF_ZOO_GROWTH = MODEL_CONSTANTS(100)
                 KAPPA_ZOO_UNDER_OPT_TEMP = MODEL_CONSTANTS(101)
                  KAPPA_ZOO_OVER_OPT_TEMP = MODEL_CONSTANTS(102)
                             GRAT_ZOO_DIA = MODEL_CONSTANTS(103)
                             GRAT_ZOO_CYN = MODEL_CONSTANTS(104)
                             GRAT_ZOO_OPA = MODEL_CONSTANTS(105)
                         GRAT_ZOO_FIX_CYN = MODEL_CONSTANTS(106)
                    GRAT_ZOO_NOST_VEG_HET = MODEL_CONSTANTS(107)
                  GRAT_ZOO_DET_PART_ORG_C = MODEL_CONSTANTS(108)
                             PREF_ZOO_DIA = MODEL_CONSTANTS(109)
                             PREF_ZOO_CYN = MODEL_CONSTANTS(110)
                         PREF_ZOO_FIX_CYN = MODEL_CONSTANTS(111)
                    PREF_ZOO_NOST_VEG_HET = MODEL_CONSTANTS(112)
                             PREF_ZOO_OPA = MODEL_CONSTANTS(113)
                  PREF_ZOO_DET_PART_ORG_C = MODEL_CONSTANTS(114)
                            KHS_DIA_C_ZOO = MODEL_CONSTANTS(115)
                            KHS_CYN_C_ZOO = MODEL_CONSTANTS(116)
                        KHS_FIX_CYN_C_ZOO = MODEL_CONSTANTS(117)
                   KHS_NOST_VEG_HET_C_ZOO = MODEL_CONSTANTS(118)
                            KHS_OPA_C_ZOO = MODEL_CONSTANTS(119)
                   KHS_DET_PART_ORG_C_ZOO = MODEL_CONSTANTS(120)
                             FOOD_MIN_ZOO = MODEL_CONSTANTS(121)
                                   KE_ZOO = MODEL_CONSTANTS(122)
                          FRAC_ZOO_EX_ORG = MODEL_CONSTANTS(123)
                                KR_ZOO_20 = MODEL_CONSTANTS(124)
                             THETA_KR_ZOO = MODEL_CONSTANTS(125)
                                KD_ZOO_20 = MODEL_CONSTANTS(126)
                             THETA_KD_ZOO = MODEL_CONSTANTS(127)
                       DO_STR_HYPOX_ZOO_D = MODEL_CONSTANTS(128)
                        THETA_HYPOX_ZOO_D = MODEL_CONSTANTS(129)
                        EXPON_HYPOX_ZOO_D = MODEL_CONSTANTS(130)
                               ZOO_N_TO_C = MODEL_CONSTANTS(131)
                               ZOO_P_TO_C = MODEL_CONSTANTS(132)
                              ZOO_O2_TO_C = MODEL_CONSTANTS(133)
                  KDISS_DET_PART_ORG_C_20 = MODEL_CONSTANTS(134)
               THETA_KDISS_DET_PART_ORG_C = MODEL_CONSTANTS(135)
                  FAC_PHYT_DET_PART_ORG_C = MODEL_CONSTANTS(136)
                  KDISS_DET_PART_ORG_N_20 = MODEL_CONSTANTS(137)
               THETA_KDISS_DET_PART_ORG_N = MODEL_CONSTANTS(138)
                               KHS_DISS_N = MODEL_CONSTANTS(139)
                  FAC_PHYT_DET_PART_ORG_N = MODEL_CONSTANTS(140)
                  KDISS_DET_PART_ORG_P_20 = MODEL_CONSTANTS(141)
               THETA_KDISS_DET_PART_ORG_P = MODEL_CONSTANTS(142)
                               KHS_DISS_P = MODEL_CONSTANTS(143)
                  FAC_PHYT_DET_PART_ORG_P = MODEL_CONSTANTS(144)
                         KDISS_PART_Si_20 = MODEL_CONSTANTS(145)
                      THETA_KDISS_PART_Si = MODEL_CONSTANTS(146)
                        FAC_PHYT_AMIN_DOC = MODEL_CONSTANTS(147)
                               KHS_AMIN_N = MODEL_CONSTANTS(148)
                        FAC_PHYT_AMIN_DON = MODEL_CONSTANTS(149)
                               KHS_AMIN_P = MODEL_CONSTANTS(150)
                        FAC_PHYT_AMIN_DOP = MODEL_CONSTANTS(151)
                                K_NITR_20 = MODEL_CONSTANTS(152)
                             THETA_K_NITR = MODEL_CONSTANTS(153)
                             KHS_NITR_OXY = MODEL_CONSTANTS(154)
                           KHS_NITR_NH4_N = MODEL_CONSTANTS(155)
                          PH_NITR_NH4_MIN = MODEL_CONSTANTS(156)
                          PH_NITR_NH4_MAX = MODEL_CONSTANTS(157)
                               k_OX_FE_II = MODEL_CONSTANTS(158)
                             k_RED_FE_III = MODEL_CONSTANTS(159)
                               k_OX_MN_II = MODEL_CONSTANTS(160)
                              k_RED_MN_IV = MODEL_CONSTANTS(161)
                      KHS_DOXY_FE_III_RED = MODEL_CONSTANTS(162)
                       KHS_DOXY_MN_IV_RED = MODEL_CONSTANTS(163)
                        K_MIN_DOC_DOXY_20 = MODEL_CONSTANTS(164)
                        K_MIN_DOC_NO3N_20 = MODEL_CONSTANTS(165)
                       K_MIN_DOC_MN_IV_20 = MODEL_CONSTANTS(166)
                      K_MIN_DOC_FE_III_20 = MODEL_CONSTANTS(167)
                    K_MIN_DOC_S_PLUS_6_20 = MODEL_CONSTANTS(168)
                         K_MIN_DOC_DOC_20 = MODEL_CONSTANTS(169)
                     THETA_K_MIN_DOC_DOXY = MODEL_CONSTANTS(170)
                     THETA_K_MIN_DOC_NO3N = MODEL_CONSTANTS(171)
                    THETA_K_MIN_DOC_MN_IV = MODEL_CONSTANTS(172)
                   THETA_K_MIN_DOC_FE_III = MODEL_CONSTANTS(173)
                 THETA_K_MIN_DOC_S_PLUS_6 = MODEL_CONSTANTS(174)
                      THETA_K_MIN_DOC_DOC = MODEL_CONSTANTS(175)
                        K_HS_DOC_MIN_DOXY = MODEL_CONSTANTS(176)
                        K_HS_DOC_MIN_NO3N = MODEL_CONSTANTS(177)
                       K_HS_DOC_MIN_MN_IV = MODEL_CONSTANTS(178)
                      K_HS_DOC_MIN_FE_III = MODEL_CONSTANTS(179)
                    K_HS_DOC_MIN_S_PLUS_6 = MODEL_CONSTANTS(180)
                         K_HS_DOC_MIN_DOC = MODEL_CONSTANTS(181)
                        K_HS_DOXY_RED_LIM = MODEL_CONSTANTS(182)
                        K_HS_NO3N_RED_LIM = MODEL_CONSTANTS(183)
                       K_HS_MN_IV_RED_LIM = MODEL_CONSTANTS(184)
                      K_HS_FE_III_RED_LIM = MODEL_CONSTANTS(185)
                    K_HS_S_PLUS_6_RED_LIM = MODEL_CONSTANTS(186)
                       K_HS_DOXY_RED_INHB = MODEL_CONSTANTS(187)
                       K_HS_NO3N_RED_INHB = MODEL_CONSTANTS(188)
                      K_HS_MN_IV_RED_INHB = MODEL_CONSTANTS(189)
                     K_HS_FE_III_RED_INHB = MODEL_CONSTANTS(190)
                   K_HS_S_PLUS_6_RED_INHB = MODEL_CONSTANTS(191)
                      PH_MIN_DOC_MIN_DOXY = MODEL_CONSTANTS(192)
                      PH_MIN_DOC_MIN_NO3N = MODEL_CONSTANTS(193)
                     PH_MIN_DOC_MIN_MN_IV = MODEL_CONSTANTS(194)
                    PH_MIN_DOC_MIN_FE_III = MODEL_CONSTANTS(195)
                  PH_MIN_DOC_MIN_S_PLUS_6 = MODEL_CONSTANTS(196)
                       PH_MIN_DOC_MIN_DOC = MODEL_CONSTANTS(197)
                      PH_MAX_DOC_MIN_DOXY = MODEL_CONSTANTS(198)
                      PH_MAX_DOC_MIN_NO3N = MODEL_CONSTANTS(199)
                     PH_MAX_DOC_MIN_MN_IV = MODEL_CONSTANTS(200)
                    PH_MAX_DOC_MIN_FE_III = MODEL_CONSTANTS(201)
                  PH_MAX_DOC_MIN_S_PLUS_6 = MODEL_CONSTANTS(202)
                       PH_MAX_DOC_MIN_DOC = MODEL_CONSTANTS(203)
                        K_MIN_DON_DOXY_20 = MODEL_CONSTANTS(204)
                        K_MIN_DON_NO3N_20 = MODEL_CONSTANTS(205)
                       K_MIN_DON_MN_IV_20 = MODEL_CONSTANTS(206)
                      K_MIN_DON_FE_III_20 = MODEL_CONSTANTS(207)
                    K_MIN_DON_S_PLUS_6_20 = MODEL_CONSTANTS(208)
                         K_MIN_DON_DOC_20 = MODEL_CONSTANTS(209)
                     THETA_K_MIN_DON_DOXY = MODEL_CONSTANTS(210)
                     THETA_K_MIN_DON_NO3N = MODEL_CONSTANTS(211)
                    THETA_K_MIN_DON_MN_IV = MODEL_CONSTANTS(212)
                   THETA_K_MIN_DON_FE_III = MODEL_CONSTANTS(213)
                 THETA_K_MIN_DON_S_PLUS_6 = MODEL_CONSTANTS(214)
                      THETA_K_MIN_DON_DOC = MODEL_CONSTANTS(215)
                        K_HS_DON_MIN_DOXY = MODEL_CONSTANTS(216)
                        K_HS_DON_MIN_NO3N = MODEL_CONSTANTS(217)
                       K_HS_DON_MIN_MN_IV = MODEL_CONSTANTS(218)
                      K_HS_DON_MIN_FE_III = MODEL_CONSTANTS(219)
                    K_HS_DON_MIN_S_PLUS_6 = MODEL_CONSTANTS(220)
                         K_HS_DON_MIN_DOC = MODEL_CONSTANTS(221)
                      PH_MIN_DON_MIN_DOXY = MODEL_CONSTANTS(222)
                      PH_MIN_DON_MIN_NO3N = MODEL_CONSTANTS(223)
                     PH_MIN_DON_MIN_MN_IV = MODEL_CONSTANTS(224)
                    PH_MIN_DON_MIN_FE_III = MODEL_CONSTANTS(225)
                  PH_MIN_DON_MIN_S_PLUS_6 = MODEL_CONSTANTS(226)
                       PH_MIN_DON_MIN_DOC = MODEL_CONSTANTS(227)
                      PH_MAX_DON_MIN_DOXY = MODEL_CONSTANTS(228)
                      PH_MAX_DON_MIN_NO3N = MODEL_CONSTANTS(229)
                     PH_MAX_DON_MIN_MN_IV = MODEL_CONSTANTS(230)
                    PH_MAX_DON_MIN_FE_III = MODEL_CONSTANTS(231)
                  PH_MAX_DON_MIN_S_PLUS_6 = MODEL_CONSTANTS(232)
                       PH_MAX_DON_MIN_DOC = MODEL_CONSTANTS(233)
                        K_MIN_DOP_DOXY_20 = MODEL_CONSTANTS(234)
                        K_MIN_DOP_NO3N_20 = MODEL_CONSTANTS(235)
                       K_MIN_DOP_MN_IV_20 = MODEL_CONSTANTS(236)
                      K_MIN_DOP_FE_III_20 = MODEL_CONSTANTS(237)
                    K_MIN_DOP_S_PLUS_6_20 = MODEL_CONSTANTS(238)
                         K_MIN_DOP_DOC_20 = MODEL_CONSTANTS(239)
                     THETA_K_MIN_DOP_DOXY = MODEL_CONSTANTS(240)
                     THETA_K_MIN_DOP_NO3N = MODEL_CONSTANTS(241)
                    THETA_K_MIN_DOP_MN_IV = MODEL_CONSTANTS(242)
                   THETA_K_MIN_DOP_FE_III = MODEL_CONSTANTS(243)
                 THETA_K_MIN_DOP_S_PLUS_6 = MODEL_CONSTANTS(244)
                      THETA_K_MIN_DOP_DOC = MODEL_CONSTANTS(245)
                        K_HS_DOP_MIN_DOXY = MODEL_CONSTANTS(246)
                        K_HS_DOP_MIN_NO3N = MODEL_CONSTANTS(247)
                       K_HS_DOP_MIN_MN_IV = MODEL_CONSTANTS(248)
                      K_HS_DOP_MIN_FE_III = MODEL_CONSTANTS(249)
                    K_HS_DOP_MIN_S_PLUS_6 = MODEL_CONSTANTS(250)
                         K_HS_DOP_MIN_DOC = MODEL_CONSTANTS(251)
                      PH_MIN_DOP_MIN_DOXY = MODEL_CONSTANTS(252)
                      PH_MIN_DOP_MIN_NO3N = MODEL_CONSTANTS(253)
                     PH_MIN_DOP_MIN_MN_IV = MODEL_CONSTANTS(254)
                    PH_MIN_DOP_MIN_FE_III = MODEL_CONSTANTS(255)
                  PH_MIN_DOP_MIN_S_PLUS_6 = MODEL_CONSTANTS(256)
                       PH_MIN_DOP_MIN_DOC = MODEL_CONSTANTS(257)
                      PH_MAX_DOP_MIN_DOXY = MODEL_CONSTANTS(258)
                      PH_MAX_DOP_MIN_NO3N = MODEL_CONSTANTS(259)
                     PH_MAX_DOP_MIN_MN_IV = MODEL_CONSTANTS(260)
                    PH_MAX_DOP_MIN_FE_III = MODEL_CONSTANTS(261)
                  PH_MAX_DOP_MIN_S_PLUS_6 = MODEL_CONSTANTS(262)
                       PH_MAX_DOP_MIN_DOC = MODEL_CONSTANTS(263)
                                 k_OX_CH4 = MODEL_CONSTANTS(264)
                           THETA_k_OX_CH4 = MODEL_CONSTANTS(265)
                         k_HS_OX_CH4_DOXY = MODEL_CONSTANTS(266)
                                 k_OX_H2S = MODEL_CONSTANTS(267)
                           THETA_k_OX_H2S = MODEL_CONSTANTS(268)
                         k_HS_OX_H2S_DOXY = MODEL_CONSTANTS(269)
                          k_DISS_FE_II_20 = MODEL_CONSTANTS(270)
                       THETA_k_DISS_FE_II = MODEL_CONSTANTS(271)
                     INIT_MULT_FE_II_DISS = MODEL_CONSTANTS(272)
                         k_DISS_FE_III_20 = MODEL_CONSTANTS(273)
                      THETA_k_DISS_FE_III = MODEL_CONSTANTS(274)
                    INIT_MULT_FE_III_DISS = MODEL_CONSTANTS(275)
                 KG_NOST_VEG_HET_OPT_TEMP = MODEL_CONSTANTS(276)
                 NOST_VEG_HET_OPT_TEMP_LR = MODEL_CONSTANTS(277)
                 NOST_VEG_HET_OPT_TEMP_UR = MODEL_CONSTANTS(278)
                  EFF_NOST_VEG_HET_GROWTH = MODEL_CONSTANTS(279)
        KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP = MODEL_CONSTANTS(280)
         KAPPA_NOST_VEG_HET_OVER_OPT_TEMP = MODEL_CONSTANTS(281)
                       KR_NOST_VEG_HET_20 = MODEL_CONSTANTS(282)
                    THETA_KR_NOST_VEG_HET = MODEL_CONSTANTS(283)
                       KD_NOST_VEG_HET_20 = MODEL_CONSTANTS(284)
                    THETA_KD_NOST_VEG_HET = MODEL_CONSTANTS(285)
                           M_DENS_VEG_HET = MODEL_CONSTANTS(286)
                      KHS_DP_NOST_VEG_HET = MODEL_CONSTANTS(287)
                      KHS_O2_NOST_VEG_HET = MODEL_CONSTANTS(288)
                   FRAC_NOST_VEG_HET_EXCR = MODEL_CONSTANTS(289)
                         I_S_NOST_VEG_HET = MODEL_CONSTANTS(290)
              DO_STR_HYPOX_NOST_VEG_HET_D = MODEL_CONSTANTS(291)
               THETA_HYPOX_NOST_VEG_HET_D = MODEL_CONSTANTS(292)
               EXPON_HYPOX_NOST_VEG_HET_D = MODEL_CONSTANTS(293)
                              NOST_N_TO_C = MODEL_CONSTANTS(294)
                              NOST_P_TO_C = MODEL_CONSTANTS(295)
                             NOST_O2_TO_C = MODEL_CONSTANTS(296)
                           NOST_C_TO_CHLA = MODEL_CONSTANTS(297)
                               P_GERM_AKI = MODEL_CONSTANTS(298)
                               N_GERM_AKI = MODEL_CONSTANTS(299)
                               P_FORM_AKI = MODEL_CONSTANTS(300)
                             DAY_FORM_AKI = MODEL_CONSTANTS(301)
                               T_FORM_AKI = MODEL_CONSTANTS(302)
                               K_LOSS_AKI = MODEL_CONSTANTS(303)
                            K_MORT_AKI_20 = MODEL_CONSTANTS(304)
                         THETA_K_MORT_AKI = MODEL_CONSTANTS(305)
                               T_GERM_AKI = MODEL_CONSTANTS(306)
                         KHS_POC_DISS_SAT = MODEL_CONSTANTS(307)
                         KHS_PON_DISS_SAT = MODEL_CONSTANTS(308)
                         KHS_POP_DISS_SAT = MODEL_CONSTANTS(309)
                           frac_avail_DON = MODEL_CONSTANTS(310)
                           frac_avail_DOP = MODEL_CONSTANTS(311)
                      frac_avail_DON_NOST = MODEL_CONSTANTS(312)
                      KHS_DN_NOST_VEG_HET = MODEL_CONSTANTS(313)
                FRAC_FIX_N_FOR_GR_VEG_HET = MODEL_CONSTANTS(314)
                         FRAC_NOST_GROWTH = MODEL_CONSTANTS(315)
                      K_MIN_PHYT_AMIN_DOC = MODEL_CONSTANTS(316)
                      K_MIN_PHYT_AMIN_DON = MODEL_CONSTANTS(317)
                      K_MIN_PHYT_AMIN_DOP = MODEL_CONSTANTS(318)

    end subroutine INIT_PELAGIC_MODEL_CONSTANTS


    subroutine INIT_TRANSPORT_FIELDS(PELAGIC_BOX_MODEL_DATA)
        implicit none
        type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA

        PELAGIC_BOX_MODEL_DATA % ADVECTION_ON(1:31) = 1
        PELAGIC_BOX_MODEL_DATA % ADVECTION_ON(32)   = 0

        PELAGIC_BOX_MODEL_DATA % DIFFUSION_ON(1:31) = 1
        PELAGIC_BOX_MODEL_DATA % DIFFUSION_ON(32)   = 0

        PELAGIC_BOX_MODEL_DATA % SETTLING_ON(1:31)  = 1
        PELAGIC_BOX_MODEL_DATA % SETTLING_ON(32)    = 0

        if (CONSIDER_ALLELOPATHY > 0) then
            PELAGIC_BOX_MODEL_DATA % &
                ADVECTION_ON((nstate + 1):(nstate + NUM_ALLOLOPATHY_STATE_VARS)) = 1

            PELAGIC_BOX_MODEL_DATA % &
                DIFFUSION_ON((nstate + 1):(nstate + NUM_ALLOLOPATHY_STATE_VARS)) = 1

            PELAGIC_BOX_MODEL_DATA % &
                SETTLING_ON ((nstate + 1):(nstate + NUM_ALLOLOPATHY_STATE_VARS)) = 1
        end if

        write(*,*) PELAGIC_BOX_MODEL_DATA % ADVECTION_ON(:)
    end subroutine INIT_TRANSPORT_FIELDS


    subroutine DUMP_PELAGIC_MODEL_CONSTANTS_ON_FILE
        implicit none
        open(unit = 88, file = 'PELAGIC_MODEL_CONSTANTS.txt', status = 'UNKNOWN')

        write( unit = 88, fmt = '(a40, f30.10)') '                              K_A',                               K_A
        write( unit = 88, fmt = '(a40, f30.10)') '                        THETA_K_A',                         THETA_K_A
        write( unit = 88, fmt = '(a40, f30.10)') '                              XKC',                               XKC
        write( unit = 88, fmt = '(a40, f30.10)') '                            PHIMX',                             PHIMX
        write( unit = 88, fmt = '(a40, f30.10)') '                  KG_DIA_OPT_TEMP',                   KG_DIA_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                  DIA_OPT_TEMP_LR',                   DIA_OPT_TEMP_LR
        write( unit = 88, fmt = '(a40, f30.10)') '                  DIA_OPT_TEMP_UR',                   DIA_OPT_TEMP_UR
        write( unit = 88, fmt = '(a40, f30.10)') '                   EFF_DIA_GROWTH',                    EFF_DIA_GROWTH
        write( unit = 88, fmt = '(a40, f30.10)') '         KAPPA_DIA_UNDER_OPT_TEMP',          KAPPA_DIA_UNDER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '         KAPPA_DIA_OVER_OPT_TEMP ',          KAPPA_DIA_OVER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                        KR_DIA_20',                         KR_DIA_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_KR_DIA',                      THETA_KR_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '                        KD_DIA_20',                         KD_DIA_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_KD_DIA',                      THETA_KD_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '                      KHS_DIN_DIA',                       KHS_DIN_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '                      KHS_DIP_DIA',                       KHS_DIP_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '                      KHS_DSi_DIA',                       KHS_DSi_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '                       KHS_O2_DIA',                        KHS_O2_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '                    FRAC_DIA_EXCR',                     FRAC_DIA_EXCR
        write( unit = 88, fmt = '(a40, f30.10)') '                          I_S_DIA',                           I_S_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '               DO_STR_HYPOX_DIA_D',                DO_STR_HYPOX_DIA_D
        write( unit = 88, fmt = '(a40, f30.10)') '                THETA_HYPOX_DIA_D',                 THETA_HYPOX_DIA_D
        write( unit = 88, fmt = '(a40, f30.10)') '                EXPON_HYPOX_DIA_D',                 EXPON_HYPOX_DIA_D
        write( unit = 88, fmt = '(a40, f30.10)') '                       DIA_N_TO_C',                        DIA_N_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                       DIA_P_TO_C',                        DIA_P_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                      DIA_Si_TO_C',                       DIA_Si_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                      DIA_O2_TO_C',                       DIA_O2_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                    DIA_C_TO_CHLA',                     DIA_C_TO_CHLA
        write( unit = 88, fmt = '(a40, f30.10)') '                  KG_CYN_OPT_TEMP',                   KG_CYN_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                  CYN_OPT_TEMP_LR',                   CYN_OPT_TEMP_LR
        write( unit = 88, fmt = '(a40, f30.10)') '                  CYN_OPT_TEMP_UR',                   CYN_OPT_TEMP_UR
        write( unit = 88, fmt = '(a40, f30.10)') '                   EFF_CYN_GROWTH',                    EFF_CYN_GROWTH
        write( unit = 88, fmt = '(a40, f30.10)') '         KAPPA_CYN_UNDER_OPT_TEMP',          KAPPA_CYN_UNDER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '          KAPPA_CYN_OVER_OPT_TEMP',           KAPPA_CYN_OVER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                        KR_CYN_20',                         KR_CYN_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_KR_CYN',                      THETA_KR_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                        KD_CYN_20',                         KD_CYN_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_KD_CYN',                      THETA_KD_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                      KHS_DIN_CYN',                       KHS_DIN_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                      KHS_DIP_CYN',                       KHS_DIP_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                       KHS_O2_CYN',                        KHS_O2_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                    FRAC_CYN_EXCR',                     FRAC_CYN_EXCR
        write( unit = 88, fmt = '(a40, f30.10)') '                          I_S_CYN',                           I_S_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '               DO_STR_HYPOX_CYN_D',                DO_STR_HYPOX_CYN_D
        write( unit = 88, fmt = '(a40, f30.10)') '                THETA_HYPOX_CYN_D',                 THETA_HYPOX_CYN_D
        write( unit = 88, fmt = '(a40, f30.10)') '                EXPON_HYPOX_CYN_D',                 EXPON_HYPOX_CYN_D
        write( unit = 88, fmt = '(a40, f30.10)') '                       CYN_N_TO_C',                        CYN_N_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                       CYN_P_TO_C',                        CYN_P_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                      CYN_O2_TO_C',                       CYN_O2_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                    CYN_C_TO_CHLA',                     CYN_C_TO_CHLA
        write( unit = 88, fmt = '(a40, f30.10)') '              KG_FIX_CYN_OPT_TEMP',               KG_FIX_CYN_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '              FIX_CYN_OPT_TEMP_LR',               FIX_CYN_OPT_TEMP_LR
        write( unit = 88, fmt = '(a40, f30.10)') '              FIX_CYN_OPT_TEMP_UR',               FIX_CYN_OPT_TEMP_UR
        write( unit = 88, fmt = '(a40, f30.10)') '               EFF_FIX_CYN_GROWTH',                EFF_FIX_CYN_GROWTH
        write( unit = 88, fmt = '(a40, f30.10)') '     KAPPA_FIX_CYN_UNDER_OPT_TEMP',      KAPPA_FIX_CYN_UNDER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '      KAPPA_FIX_CYN_OVER_OPT_TEMP',       KAPPA_FIX_CYN_OVER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                    KR_FIX_CYN_20',                     KR_FIX_CYN_20
        write( unit = 88, fmt = '(a40, f30.10)') '                 THETA_KR_FIX_CYN',                  THETA_KR_FIX_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                    KD_FIX_CYN_20',                     KD_FIX_CYN_20
        write( unit = 88, fmt = '(a40, f30.10)') '                 THETA_KD_FIX_CYN',                  THETA_KD_FIX_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                  KHS_DIN_FIX_CYN',                   KHS_DIN_FIX_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                  KHS_DIP_FIX_CYN',                   KHS_DIP_FIX_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                   KHS_O2_FIX_CYN',                    KHS_O2_FIX_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                FRAC_FIX_CYN_EXCR',                 FRAC_FIX_CYN_EXCR
        write( unit = 88, fmt = '(a40, f30.10)') '                      I_S_FIX_CYN',                       I_S_FIX_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '           DO_STR_HYPOX_FIX_CYN_D',            DO_STR_HYPOX_FIX_CYN_D
        write( unit = 88, fmt = '(a40, f30.10)') '            THETA_HYPOX_FIX_CYN_D',             THETA_HYPOX_FIX_CYN_D
        write( unit = 88, fmt = '(a40, f30.10)') '            EXPON_HYPOX_FIX_CYN_D',             EXPON_HYPOX_FIX_CYN_D
        write( unit = 88, fmt = '(a40, f30.10)') '                   FIX_CYN_N_TO_C',                    FIX_CYN_N_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                   FIX_CYN_P_TO_C',                    FIX_CYN_P_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                  FIX_CYN_O2_TO_C',                   FIX_CYN_O2_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                FIX_CYN_C_TO_CHLA',                 FIX_CYN_C_TO_CHLA
        write( unit = 88, fmt = '(a40, f30.10)') '                            R_FIX',                             R_FIX
        write( unit = 88, fmt = '(a40, f30.10)') '                            K_FIX',                             K_FIX
        write( unit = 88, fmt = '(a40, f30.10)') '                  KG_OPA_OPT_TEMP',                   KG_OPA_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                  OPA_OPT_TEMP_LR',                   OPA_OPT_TEMP_LR
        write( unit = 88, fmt = '(a40, f30.10)') '                  OPA_OPT_TEMP_UR',                   OPA_OPT_TEMP_UR
        write( unit = 88, fmt = '(a40, f30.10)') '                   EFF_OPA_GROWTH',                    EFF_OPA_GROWTH
        write( unit = 88, fmt = '(a40, f30.10)') '         KAPPA_OPA_UNDER_OPT_TEMP',          KAPPA_OPA_UNDER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '          KAPPA_OPA_OVER_OPT_TEMP',           KAPPA_OPA_OVER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                        KR_OPA_20',                         KR_OPA_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_KR_OPA',                      THETA_KR_OPA
        write( unit = 88, fmt = '(a40, f30.10)') '                        KD_OPA_20',                         KD_OPA_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_KD_OPA',                      THETA_KD_OPA
        write( unit = 88, fmt = '(a40, f30.10)') '                      KHS_DIN_OPA',                       KHS_DIN_OPA
        write( unit = 88, fmt = '(a40, f30.10)') '                      KHS_DIP_OPA',                       KHS_DIP_OPA
        write( unit = 88, fmt = '(a40, f30.10)') '                       KHS_O2_OPA',                        KHS_O2_OPA
        write( unit = 88, fmt = '(a40, f30.10)') '                    FRAC_OPA_EXCR',                     FRAC_OPA_EXCR
        write( unit = 88, fmt = '(a40, f30.10)') '                          I_S_OPA',                           I_S_OPA
        write( unit = 88, fmt = '(a40, f30.10)') '               DO_STR_HYPOX_OPA_D',                DO_STR_HYPOX_OPA_D
        write( unit = 88, fmt = '(a40, f30.10)') '                THETA_HYPOX_OPA_D',                 THETA_HYPOX_OPA_D
        write( unit = 88, fmt = '(a40, f30.10)') '                EXPON_HYPOX_OPA_D',                 EXPON_HYPOX_OPA_D
        write( unit = 88, fmt = '(a40, f30.10)') '                       OPA_N_TO_C',                        OPA_N_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                       OPA_P_TO_C',                        OPA_P_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                      OPA_O2_TO_C',                       OPA_O2_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                    OPA_C_TO_CHLA',                     OPA_C_TO_CHLA
        write( unit = 88, fmt = '(a40, f30.10)') '                  KG_ZOO_OPT_TEMP',                   KG_ZOO_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                  ZOO_OPT_TEMP_LR',                   ZOO_OPT_TEMP_LR
        write( unit = 88, fmt = '(a40, f30.10)') '                  ZOO_OPT_TEMP_UR',                   ZOO_OPT_TEMP_UR
        write( unit = 88, fmt = '(a40, f30.10)') '                   EFF_ZOO_GROWTH',                    EFF_ZOO_GROWTH
        write( unit = 88, fmt = '(a40, f30.10)') '         KAPPA_ZOO_UNDER_OPT_TEMP',          KAPPA_ZOO_UNDER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '          KAPPA_ZOO_OVER_OPT_TEMP',           KAPPA_ZOO_OVER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '                     GRAT_ZOO_DIA',                      GRAT_ZOO_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '                     GRAT_ZOO_CYN',                      GRAT_ZOO_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                     GRAT_ZOO_OPA',                      GRAT_ZOO_OPA
        write( unit = 88, fmt = '(a40, f30.10)') '                 GRAT_ZOO_FIX_CYN',                  GRAT_ZOO_FIX_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '            GRAT_ZOO_NOST_VEG_HET',             GRAT_ZOO_NOST_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '          GRAT_ZOO_DET_PART_ORG_C',           GRAT_ZOO_DET_PART_ORG_C
        write( unit = 88, fmt = '(a40, f30.10)') '                     PREF_ZOO_DIA',                      PREF_ZOO_DIA
        write( unit = 88, fmt = '(a40, f30.10)') '                     PREF_ZOO_CYN',                      PREF_ZOO_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '                 PREF_ZOO_FIX_CYN',                  PREF_ZOO_FIX_CYN
        write( unit = 88, fmt = '(a40, f30.10)') '            PREF_ZOO_NOST_VEG_HET',             PREF_ZOO_NOST_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '                     PREF_ZOO_OPA',                      PREF_ZOO_OPA
        write( unit = 88, fmt = '(a40, f30.10)') '          PREF_ZOO_DET_PART_ORG_C',           PREF_ZOO_DET_PART_ORG_C
        write( unit = 88, fmt = '(a40, f30.10)') '                    KHS_DIA_C_ZOO',                     KHS_DIA_C_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '                    KHS_CYN_C_ZOO',                     KHS_CYN_C_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '                KHS_FIX_CYN_C_ZOO',                 KHS_FIX_CYN_C_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '           KHS_NOST_VEG_HET_C_ZOO',            KHS_NOST_VEG_HET_C_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '                    KHS_OPA_C_ZOO',                     KHS_OPA_C_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '           KHS_DET_PART_ORG_C_ZOO',            KHS_DET_PART_ORG_C_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '                     FOOD_MIN_ZOO',                      FOOD_MIN_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '                           KE_ZOO',                            KE_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '                  FRAC_ZOO_EX_ORG',                   FRAC_ZOO_EX_ORG
        write( unit = 88, fmt = '(a40, f30.10)') '                        KR_ZOO_20',                         KR_ZOO_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_KR_ZOO',                      THETA_KR_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '                        KD_ZOO_20',                         KD_ZOO_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_KD_ZOO',                      THETA_KD_ZOO
        write( unit = 88, fmt = '(a40, f30.10)') '               DO_STR_HYPOX_ZOO_D',                DO_STR_HYPOX_ZOO_D
        write( unit = 88, fmt = '(a40, f30.10)') '                THETA_HYPOX_ZOO_D',                 THETA_HYPOX_ZOO_D
        write( unit = 88, fmt = '(a40, f30.10)') '                EXPON_HYPOX_ZOO_D',                 EXPON_HYPOX_ZOO_D
        write( unit = 88, fmt = '(a40, f30.10)') '                       ZOO_N_TO_C',                        ZOO_N_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                       ZOO_P_TO_C',                        ZOO_P_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                      ZOO_O2_TO_C',                       ZOO_O2_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '          KDISS_DET_PART_ORG_C_20',           KDISS_DET_PART_ORG_C_20
        write( unit = 88, fmt = '(a40, f30.10)') '       THETA_KDISS_DET_PART_ORG_C',        THETA_KDISS_DET_PART_ORG_C
        write( unit = 88, fmt = '(a40, f30.10)') '          FAC_PHYT_DET_PART_ORG_C',           FAC_PHYT_DET_PART_ORG_C
        write( unit = 88, fmt = '(a40, f30.10)') '          KDISS_DET_PART_ORG_N_20',           KDISS_DET_PART_ORG_N_20
        write( unit = 88, fmt = '(a40, f30.10)') '       THETA_KDISS_DET_PART_ORG_N',        THETA_KDISS_DET_PART_ORG_N
        write( unit = 88, fmt = '(a40, f30.10)') '                       KHS_DISS_N',                        KHS_DISS_N
        write( unit = 88, fmt = '(a40, f30.10)') '          FAC_PHYT_DET_PART_ORG_N',           FAC_PHYT_DET_PART_ORG_N
        write( unit = 88, fmt = '(a40, f30.10)') '          KDISS_DET_PART_ORG_P_20',           KDISS_DET_PART_ORG_P_20
        write( unit = 88, fmt = '(a40, f30.10)') '       THETA_KDISS_DET_PART_ORG_P',        THETA_KDISS_DET_PART_ORG_P
        write( unit = 88, fmt = '(a40, f30.10)') '                       KHS_DISS_P',                        KHS_DISS_P
        write( unit = 88, fmt = '(a40, f30.10)') '          FAC_PHYT_DET_PART_ORG_P',           FAC_PHYT_DET_PART_ORG_P
        write( unit = 88, fmt = '(a40, f30.10)') '                 KDISS_PART_Si_20',                  KDISS_PART_Si_20
        write( unit = 88, fmt = '(a40, f30.10)') '              THETA_KDISS_PART_Si',               THETA_KDISS_PART_Si
        write( unit = 88, fmt = '(a40, f30.10)') '                FAC_PHYT_AMIN_DOC',                 FAC_PHYT_AMIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '                       KHS_AMIN_N',                        KHS_AMIN_N
        write( unit = 88, fmt = '(a40, f30.10)') '                FAC_PHYT_AMIN_DON',                 FAC_PHYT_AMIN_DON
        write( unit = 88, fmt = '(a40, f30.10)') '                       KHS_AMIN_P',                        KHS_AMIN_P
        write( unit = 88, fmt = '(a40, f30.10)') '                FAC_PHYT_AMIN_DOP',                 FAC_PHYT_AMIN_DOP
        write( unit = 88, fmt = '(a40, f30.10)') '                        K_NITR_20',                         K_NITR_20
        write( unit = 88, fmt = '(a40, f30.10)') '                     THETA_K_NITR',                      THETA_K_NITR
        write( unit = 88, fmt = '(a40, f30.10)') '                     KHS_NITR_OXY',                      KHS_NITR_OXY
        write( unit = 88, fmt = '(a40, f30.10)') '                   KHS_NITR_NH4_N',                    KHS_NITR_NH4_N
        write( unit = 88, fmt = '(a40, f30.10)') '                  PH_NITR_NH4_MIN',                   PH_NITR_NH4_MIN
        write( unit = 88, fmt = '(a40, f30.10)') '                  PH_NITR_NH4_MAX',                   PH_NITR_NH4_MAX
        write( unit = 88, fmt = '(a40, f30.10)') '                       k_OX_FE_II',                        k_OX_FE_II
        write( unit = 88, fmt = '(a40, f30.10)') '                     k_RED_FE_III',                      k_RED_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '                       k_OX_MN_II',                        k_OX_MN_II
        write( unit = 88, fmt = '(a40, f30.10)') '                      k_RED_MN_IV',                       k_RED_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '              KHS_DOXY_FE_III_RED',               KHS_DOXY_FE_III_RED
        write( unit = 88, fmt = '(a40, f30.10)') '               KHS_DOXY_MN_IV_RED',                KHS_DOXY_MN_IV_RED
        write( unit = 88, fmt = '(a40, f30.10)') '                K_MIN_DOC_DOXY_20',                 K_MIN_DOC_DOXY_20
        write( unit = 88, fmt = '(a40, f30.10)') '                K_MIN_DOC_NO3N_20',                 K_MIN_DOC_NO3N_20
        write( unit = 88, fmt = '(a40, f30.10)') '               K_MIN_DOC_MN_IV_20',                K_MIN_DOC_MN_IV_20
        write( unit = 88, fmt = '(a40, f30.10)') '              K_MIN_DOC_FE_III_20',               K_MIN_DOC_FE_III_20
        write( unit = 88, fmt = '(a40, f30.10)') '            K_MIN_DOC_S_PLUS_6_20',             K_MIN_DOC_S_PLUS_6_20
        write( unit = 88, fmt = '(a40, f30.10)') '                 K_MIN_DOC_DOC_20',                  K_MIN_DOC_DOC_20
        write( unit = 88, fmt = '(a40, f30.10)') '             THETA_K_MIN_DOC_DOXY',              THETA_K_MIN_DOC_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '             THETA_K_MIN_DOC_NO3N',              THETA_K_MIN_DOC_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '            THETA_K_MIN_DOC_MN_IV',             THETA_K_MIN_DOC_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '           THETA_K_MIN_DOC_FE_III',            THETA_K_MIN_DOC_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '         THETA_K_MIN_DOC_S_PLUS_6',          THETA_K_MIN_DOC_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '              THETA_K_MIN_DOC_DOC',               THETA_K_MIN_DOC_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '                K_HS_DOC_MIN_DOXY',                 K_HS_DOC_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '                K_HS_DOC_MIN_NO3N',                 K_HS_DOC_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '               K_HS_DOC_MIN_MN_IV',                K_HS_DOC_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '              K_HS_DOC_MIN_FE_III',               K_HS_DOC_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '            K_HS_DOC_MIN_S_PLUS_6',             K_HS_DOC_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '                 K_HS_DOC_MIN_DOC',                  K_HS_DOC_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '                K_HS_DOXY_RED_LIM',                 K_HS_DOXY_RED_LIM
        write( unit = 88, fmt = '(a40, f30.10)') '                K_HS_NO3N_RED_LIM',                 K_HS_NO3N_RED_LIM
        write( unit = 88, fmt = '(a40, f30.10)') '               K_HS_MN_IV_RED_LIM',                K_HS_MN_IV_RED_LIM
        write( unit = 88, fmt = '(a40, f30.10)') '              K_HS_FE_III_RED_LIM',               K_HS_FE_III_RED_LIM
        write( unit = 88, fmt = '(a40, f30.10)') '            K_HS_S_PLUS_6_RED_LIM',             K_HS_S_PLUS_6_RED_LIM
        write( unit = 88, fmt = '(a40, f30.10)') '               K_HS_DOXY_RED_INHB',                K_HS_DOXY_RED_INHB
        write( unit = 88, fmt = '(a40, f30.10)') '               K_HS_NO3N_RED_INHB',                K_HS_NO3N_RED_INHB
        write( unit = 88, fmt = '(a40, f30.10)') '              K_HS_MN_IV_RED_INHB',               K_HS_MN_IV_RED_INHB
        write( unit = 88, fmt = '(a40, f30.10)') '             K_HS_FE_III_RED_INHB',              K_HS_FE_III_RED_INHB
        write( unit = 88, fmt = '(a40, f30.10)') '           K_HS_S_PLUS_6_RED_INHB',            K_HS_S_PLUS_6_RED_INHB
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MIN_DOC_MIN_DOXY',               PH_MIN_DOC_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MIN_DOC_MIN_NO3N',               PH_MIN_DOC_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '             PH_MIN_DOC_MIN_MN_IV',              PH_MIN_DOC_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '            PH_MIN_DOC_MIN_FE_III',             PH_MIN_DOC_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '          PH_MIN_DOC_MIN_S_PLUS_6',           PH_MIN_DOC_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '               PH_MIN_DOC_MIN_DOC',                PH_MIN_DOC_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MAX_DOC_MIN_DOXY',               PH_MAX_DOC_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MAX_DOC_MIN_NO3N',               PH_MAX_DOC_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '             PH_MAX_DOC_MIN_MN_IV',              PH_MAX_DOC_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '            PH_MAX_DOC_MIN_FE_III',             PH_MAX_DOC_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '          PH_MAX_DOC_MIN_S_PLUS_6',           PH_MAX_DOC_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '               PH_MAX_DOC_MIN_DOC',                PH_MAX_DOC_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '                K_MIN_DON_DOXY_20',                 K_MIN_DON_DOXY_20
        write( unit = 88, fmt = '(a40, f30.10)') '                K_MIN_DON_NO3N_20',                 K_MIN_DON_NO3N_20
        write( unit = 88, fmt = '(a40, f30.10)') '               K_MIN_DON_MN_IV_20',                K_MIN_DON_MN_IV_20
        write( unit = 88, fmt = '(a40, f30.10)') '              K_MIN_DON_FE_III_20',               K_MIN_DON_FE_III_20
        write( unit = 88, fmt = '(a40, f30.10)') '            K_MIN_DON_S_PLUS_6_20',             K_MIN_DON_S_PLUS_6_20
        write( unit = 88, fmt = '(a40, f30.10)') '                 K_MIN_DON_DOC_20',                  K_MIN_DON_DOC_20
        write( unit = 88, fmt = '(a40, f30.10)') '             THETA_K_MIN_DON_DOXY',              THETA_K_MIN_DON_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '             THETA_K_MIN_DON_NO3N',              THETA_K_MIN_DON_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '            THETA_K_MIN_DON_MN_IV',             THETA_K_MIN_DON_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '           THETA_K_MIN_DON_FE_III',            THETA_K_MIN_DON_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '         THETA_K_MIN_DON_S_PLUS_6',          THETA_K_MIN_DON_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '              THETA_K_MIN_DON_DOC',               THETA_K_MIN_DON_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '                K_HS_DON_MIN_DOXY',                 K_HS_DON_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '                K_HS_DON_MIN_NO3N',                 K_HS_DON_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '               K_HS_DON_MIN_MN_IV',                K_HS_DON_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '              K_HS_DON_MIN_FE_III',               K_HS_DON_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '            K_HS_DON_MIN_S_PLUS_6',             K_HS_DON_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '                 K_HS_DON_MIN_DOC',                  K_HS_DON_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MIN_DON_MIN_DOXY',               PH_MIN_DON_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MIN_DON_MIN_NO3N',               PH_MIN_DON_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '             PH_MIN_DON_MIN_MN_IV',              PH_MIN_DON_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '            PH_MIN_DON_MIN_FE_III',             PH_MIN_DON_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '          PH_MIN_DON_MIN_S_PLUS_6',           PH_MIN_DON_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '               PH_MIN_DON_MIN_DOC',                PH_MIN_DON_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MAX_DON_MIN_DOXY',               PH_MAX_DON_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MAX_DON_MIN_NO3N',               PH_MAX_DON_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '             PH_MAX_DON_MIN_MN_IV',              PH_MAX_DON_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '            PH_MAX_DON_MIN_FE_III',             PH_MAX_DON_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '          PH_MAX_DON_MIN_S_PLUS_6',           PH_MAX_DON_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '               PH_MAX_DON_MIN_DOC',                PH_MAX_DON_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '                K_MIN_DOP_DOXY_20',                 K_MIN_DOP_DOXY_20
        write( unit = 88, fmt = '(a40, f30.10)') '                K_MIN_DOP_NO3N_20',                 K_MIN_DOP_NO3N_20
        write( unit = 88, fmt = '(a40, f30.10)') '               K_MIN_DOP_MN_IV_20',                K_MIN_DOP_MN_IV_20
        write( unit = 88, fmt = '(a40, f30.10)') '              K_MIN_DOP_FE_III_20',               K_MIN_DOP_FE_III_20
        write( unit = 88, fmt = '(a40, f30.10)') '            K_MIN_DOP_S_PLUS_6_20',             K_MIN_DOP_S_PLUS_6_20
        write( unit = 88, fmt = '(a40, f30.10)') '                 K_MIN_DOP_DOC_20',                  K_MIN_DOP_DOC_20
        write( unit = 88, fmt = '(a40, f30.10)') '             THETA_K_MIN_DOP_DOXY',              THETA_K_MIN_DOP_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '             THETA_K_MIN_DOP_NO3N',              THETA_K_MIN_DOP_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '            THETA_K_MIN_DOP_MN_IV',             THETA_K_MIN_DOP_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '           THETA_K_MIN_DOP_FE_III',            THETA_K_MIN_DOP_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '         THETA_K_MIN_DOP_S_PLUS_6',          THETA_K_MIN_DOP_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '              THETA_K_MIN_DOP_DOC',               THETA_K_MIN_DOP_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '                K_HS_DOP_MIN_DOXY',                 K_HS_DOP_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '                K_HS_DOP_MIN_NO3N',                 K_HS_DOP_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '               K_HS_DOP_MIN_MN_IV',                K_HS_DOP_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '              K_HS_DOP_MIN_FE_III',               K_HS_DOP_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '            K_HS_DOP_MIN_S_PLUS_6',             K_HS_DOP_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '                 K_HS_DOP_MIN_DOC',                  K_HS_DOP_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MIN_DOP_MIN_DOXY',               PH_MIN_DOP_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MIN_DOP_MIN_NO3N',               PH_MIN_DOP_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '             PH_MIN_DOP_MIN_MN_IV',              PH_MIN_DOP_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '            PH_MIN_DOP_MIN_FE_III',             PH_MIN_DOP_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '          PH_MIN_DOP_MIN_S_PLUS_6',           PH_MIN_DOP_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '               PH_MIN_DOP_MIN_DOC',                PH_MIN_DOP_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MAX_DOP_MIN_DOXY',               PH_MAX_DOP_MIN_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '              PH_MAX_DOP_MIN_NO3N',               PH_MAX_DOP_MIN_NO3N
        write( unit = 88, fmt = '(a40, f30.10)') '             PH_MAX_DOP_MIN_MN_IV',              PH_MAX_DOP_MIN_MN_IV
        write( unit = 88, fmt = '(a40, f30.10)') '            PH_MAX_DOP_MIN_FE_III',             PH_MAX_DOP_MIN_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '          PH_MAX_DOP_MIN_S_PLUS_6',           PH_MAX_DOP_MIN_S_PLUS_6
        write( unit = 88, fmt = '(a40, f30.10)') '               PH_MAX_DOP_MIN_DOC',                PH_MAX_DOP_MIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '                         k_OX_CH4',                          k_OX_CH4
        write( unit = 88, fmt = '(a40, f30.10)') '                   THETA_k_OX_CH4',                    THETA_k_OX_CH4
        write( unit = 88, fmt = '(a40, f30.10)') '                 k_HS_OX_CH4_DOXY',                  k_HS_OX_CH4_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '                         k_OX_H2S',                          k_OX_H2S
        write( unit = 88, fmt = '(a40, f30.10)') '                   THETA_k_OX_H2S',                    THETA_k_OX_H2S
        write( unit = 88, fmt = '(a40, f30.10)') '                 k_HS_OX_H2S_DOXY',                  k_HS_OX_H2S_DOXY
        write( unit = 88, fmt = '(a40, f30.10)') '                  k_DISS_FE_II_20',                   k_DISS_FE_II_20
        write( unit = 88, fmt = '(a40, f30.10)') '               THETA_k_DISS_FE_II',                THETA_k_DISS_FE_II
        write( unit = 88, fmt = '(a40, f30.10)') '             INIT_MULT_FE_II_DISS',              INIT_MULT_FE_II_DISS
        write( unit = 88, fmt = '(a40, f30.10)') '                 k_DISS_FE_III_20',                  k_DISS_FE_III_20
        write( unit = 88, fmt = '(a40, f30.10)') '              THETA_k_DISS_FE_III',               THETA_k_DISS_FE_III
        write( unit = 88, fmt = '(a40, f30.10)') '            INIT_MULT_FE_III_DISS',             INIT_MULT_FE_III_DISS
        write( unit = 88, fmt = '(a40, f30.10)') '         KG_NOST_VEG_HET_OPT_TEMP',          KG_NOST_VEG_HET_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '         NOST_VEG_HET_OPT_TEMP_LR',          NOST_VEG_HET_OPT_TEMP_LR
        write( unit = 88, fmt = '(a40, f30.10)') '         NOST_VEG_HET_OPT_TEMP_UR',          NOST_VEG_HET_OPT_TEMP_UR
        write( unit = 88, fmt = '(a40, f30.10)') '          EFF_NOST_VEG_HET_GROWTH',           EFF_NOST_VEG_HET_GROWTH
        write( unit = 88, fmt = '(a40, f30.10)') 'KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP', KAPPA_NOST_VEG_HET_UNDER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') ' KAPPA_NOST_VEG_HET_OVER_OPT_TEMP',  KAPPA_NOST_VEG_HET_OVER_OPT_TEMP
        write( unit = 88, fmt = '(a40, f30.10)') '               KR_NOST_VEG_HET_20',                KR_NOST_VEG_HET_20
        write( unit = 88, fmt = '(a40, f30.10)') '            THETA_KR_NOST_VEG_HET',             THETA_KR_NOST_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '               KD_NOST_VEG_HET_20',                KD_NOST_VEG_HET_20
        write( unit = 88, fmt = '(a40, f30.10)') '            THETA_KD_NOST_VEG_HET',             THETA_KD_NOST_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '                   M_DENS_VEG_HET',                    M_DENS_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '              KHS_DP_NOST_VEG_HET',               KHS_DP_NOST_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '              KHS_O2_NOST_VEG_HET',               KHS_O2_NOST_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '           FRAC_NOST_VEG_HET_EXCR',            FRAC_NOST_VEG_HET_EXCR
        write( unit = 88, fmt = '(a40, f30.10)') '                 I_S_NOST_VEG_HET',                  I_S_NOST_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '      DO_STR_HYPOX_NOST_VEG_HET_D',       DO_STR_HYPOX_NOST_VEG_HET_D
        write( unit = 88, fmt = '(a40, f30.10)') '       THETA_HYPOX_NOST_VEG_HET_D',        THETA_HYPOX_NOST_VEG_HET_D
        write( unit = 88, fmt = '(a40, f30.10)') '       EXPON_HYPOX_NOST_VEG_HET_D',        EXPON_HYPOX_NOST_VEG_HET_D
        write( unit = 88, fmt = '(a40, f30.10)') '                      NOST_N_TO_C',                       NOST_N_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                      NOST_P_TO_C',                       NOST_P_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                     NOST_O2_TO_C',                      NOST_O2_TO_C
        write( unit = 88, fmt = '(a40, f30.10)') '                   NOST_C_TO_CHLA',                    NOST_C_TO_CHLA
        write( unit = 88, fmt = '(a40, f30.10)') '                       P_GERM_AKI',                        P_GERM_AKI
        write( unit = 88, fmt = '(a40, f30.10)') '                       N_GERM_AKI',                        N_GERM_AKI
        write( unit = 88, fmt = '(a40, f30.10)') '                       P_FORM_AKI',                        P_FORM_AKI
        write( unit = 88, fmt = '(a40, f30.10)') '                     DAY_FORM_AKI',                      DAY_FORM_AKI
        write( unit = 88, fmt = '(a40, f30.10)') '                       T_FORM_AKI',                        T_FORM_AKI
        write( unit = 88, fmt = '(a40, f30.10)') '                       K_LOSS_AKI',                        K_LOSS_AKI
        write( unit = 88, fmt = '(a40, f30.10)') '                    K_MORT_AKI_20',                     K_MORT_AKI_20
        write( unit = 88, fmt = '(a40, f30.10)') '                 THETA_K_MORT_AKI',                  THETA_K_MORT_AKI
        write( unit = 88, fmt = '(a40, f30.10)') '                       T_GERM_AKI',                        T_GERM_AKI
        write( unit = 88, fmt = '(a40, f30.10)') '                 KHS_POC_DISS_SAT',                  KHS_POC_DISS_SAT
        write( unit = 88, fmt = '(a40, f30.10)') '                 KHS_PON_DISS_SAT',                  KHS_PON_DISS_SAT
        write( unit = 88, fmt = '(a40, f30.10)') '                 KHS_POP_DISS_SAT',                  KHS_POP_DISS_SAT
        write( unit = 88, fmt = '(a40, f30.10)') '                   frac_avail_DON',                    frac_avail_DON
        write( unit = 88, fmt = '(a40, f30.10)') '                   frac_avail_DOP',                    frac_avail_DOP
        write( unit = 88, fmt = '(a40, f30.10)') '              frac_avail_DON_NOST',               frac_avail_DON_NOST
        write( unit = 88, fmt = '(a40, f30.10)') '              KHS_DN_NOST_VEG_HET',               KHS_DN_NOST_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '        FRAC_FIX_N_FOR_GR_VEG_HET',         FRAC_FIX_N_FOR_GR_VEG_HET
        write( unit = 88, fmt = '(a40, f30.10)') '                 FRAC_NOST_GROWTH',                  FRAC_NOST_GROWTH
        write( unit = 88, fmt = '(a40, f30.10)') '              K_MIN_PHYT_AMIN_DOC',               K_MIN_PHYT_AMIN_DOC
        write( unit = 88, fmt = '(a40, f30.10)') '              K_MIN_PHYT_AMIN_DON',               K_MIN_PHYT_AMIN_DON
        write( unit = 88, fmt = '(a40, f30.10)') '              K_MIN_PHYT_AMIN_DOP',               K_MIN_PHYT_AMIN_DOP

        close(88)
    end subroutine DUMP_PELAGIC_MODEL_CONSTANTS_ON_FILE

end module PELAGIC_ECOLOGY


subroutine READ_PELAGIC_MODEL_OPTIONS(IN_FILE)

    use GLOBAL
    use ALLELOPATHY

    implicit none

    integer, intent(in) :: IN_FILE

    read(IN_FILE + 1, *)
    read(IN_FILE + 1, *) ZOOPLANKTON_OPTION

    read(IN_FILE + 1, *)
    read(IN_FILE + 1, *) ADVANCED_REDOX_SIMULATION

    if (ADVANCED_REDOX_SIMULATION > 0) then
        write(*,*) 'Advanced redox simulation for AQUABC is on.'
    else
        write(*,*) 'Advanced redox simulation for AQUABC is off.'
    end if

    read(IN_FILE + 1, *)
    read(IN_FILE + 1, *) LIGHT_EXTINCTION_OPTION

    if (LIGHT_EXTINCTION_OPTION > 0) then
        write(*,*) 'Light extinction modelling option for AQUABC as given in "KD".'
    else
        write(*,*) 'Standard light extinction sub model for AQUABC'
    end if

    read(IN_FILE + 1, *)
    read(IN_FILE + 1, *) CYANO_BOUYANT_STATE_SIMULATION

    if (CYANO_BOUYANT_STATE_SIMULATION > 0) then
        write(*,*) 'Bouyant cyanobacteria option for AQUABC is on.'
    else
        write(*,*) 'Bouyant cyanobacteria option for AQUABC is off. ' // &
                   'Nostocales (if simulated) are allways considered bouyant.'
    end if

    read(IN_FILE + 1, *)
    read(IN_FILE + 1, *) CONSIDER_NON_OBLIGATORY_FIXERS

    if (CONSIDER_NON_OBLIGATORY_FIXERS > 0) then
        write(*,*) 'Non-abligatory fixers will be modelled by AQUABC'
    else
        write(*,*) 'Non-abligatory fixers will not be modelled by AQUABC'
    end if

    read(IN_FILE + 1, *)
    read(IN_FILE + 1, *) CONSIDER_NOSTOCALES

    if (CONSIDER_NOSTOCALES > 0) then
        write(*,*) 'Heterocyst forming fixers and akinetes will be modelled by AQUABC.'
    else
        write(*,*) 'Heterocyst forming fixers and akinetes will not be modelled by AQUABC'
    end if

    read(IN_FILE + 1, *)
    read(IN_FILE + 1, *) CONSIDER_ALLELOPATHY

    if (CONSIDER_ALLELOPATHY > 0) then
        write(*,*) 'Allelopathy will be considered. '
        write(*,*) 'Four additional state variables will be dealt in addition to '
        write(*,*) 'AQUABC state variables.'
    else
        write(*,*) 'Allelopathy will not be considered.'
    end if

end subroutine READ_PELAGIC_MODEL_OPTIONS


subroutine READ_EXTRA_PELAGIC_MODEL_CONSTS(IN_FILE)
    use GLOBAL
    use ALLELOPATHY
    implicit none

    integer, intent(in) :: IN_FILE

    !  Read the user entered fraction of available DON for cyanobacteria uptake
    read(IN_FILE + 1, *)
    read(IN_FILE + 1, fmt = *) USER_ENTERED_frac_avail_DON

    !  Read the user entered fraction of available DON for cyanobacteria uptake
    read(IN_FILE + 1, *)
    read(IN_FILE + 1, fmt = *) USER_ENTERED_K_B_E

    ! Read the model constants related to allelopathy simulation
    if (CONSIDER_ALLELOPATHY > 0) then
        read(IN_FILE + 1, *)

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_DIA_NOFIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_DIA_FIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_DIA_NOST

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_DIA_OPA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_DIA_ZOO

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) k_DEG_SEC_METAB_DIA_20

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) THETA_k_DEG_SEC_METAB_DIA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOFIX_CYN_DIA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOFIX_CYN_FIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOFIX_CYN_NOST

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOFIX_CYN_OPA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOFIX_CYN_ZOO

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) k_DEG_SEC_METAB_NOFIX_CYN_20

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) THETA_k_DEG_SEC_METAB_NOFIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_FIX_CYN_DIA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_FIX_CYN_NOFIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_FIX_CYN_NOST

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_FIX_CYN_OPA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_FIX_CYN_ZOO

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) k_DEG_SEC_METAB_FIX_CYN_20

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) THETA_k_DEG_SEC_METAB_FIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOST_DIA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOST_NOFIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOST_FIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOST_OPA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) K_HS_SEC_METAB_NOST_ZOO

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) k_DEG_SEC_METAB_NOST_20

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) THETA_k_DEG_SEC_METAB_NOST

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) S_SEC_METAB_TO_DIA

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) S_SEC_METAB_TO_NOFIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) S_SEC_METAB_TO_FIX_CYN

        read(IN_FILE + 1, fmt = *)
        read(IN_FILE + 1, fmt = *) S_SEC_METAB_TO_NOST
    end if

end subroutine READ_EXTRA_PELAGIC_MODEL_CONSTS


subroutine CALCULATE_SETTLING_SUPRESSION(SETTLING_VELOCITY_FACTORS)

    use GLOBAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes
    use chla_diagnostics

    implicit none

    real(kind = DBL), dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
        intent(inout) :: SETTLING_VELOCITY_FACTORS

    integer :: i, nd

    ! ---------------------------------------------------------------------------
    ! Calculate chlorophyl-a
    ! ---------------------------------------------------------------------------

    call chlorophyl_a_vec &
         (STATE_VARIABLES(:, DIA_C_INDEX         ) , &
          STATE_VARIABLES(:, CYN_C_INDEX         ) , &
          STATE_VARIABLES(:, OPA_C_INDEX         ) , &
          STATE_VARIABLES(:, FIX_CYN_C_INDEX     ) , &
          STATE_VARIABLES(:, NOST_VEG_HET_C_INDEX) , &
          DIA_C_TO_CHLA                            , &
          CYN_C_TO_CHLA                            , &
          OPA_C_TO_CHLA                            , &
          FIX_CYN_C_TO_CHLA                        , &
          NOST_C_TO_CHLA                           , &
          nkn                                      , &
          CHLA                                     , &
          CONSIDER_NON_OBLIGATORY_FIXERS           , &
          CONSIDER_NOSTOCALES)

    ! One-shot full dump if CHLA negative was detected in the CHLA routine
    if (CHLA_NEG_FLAG .and. .not. CHLA_DUMP_DONE) then
        nd = CHLA_NEG_NODE
        write(6,*) '=== ONE-SHOT CHLA_NEGATIVE DUMP ==='
        write(6,*) ' BOX=', nd
        write(6,*) ' CHLA_NEG_VALUE=', CHLA_NEG_VALUE, ' CHLA(nd)=', CHLA(nd)
        write(6,*) ' STATE VARS at BOX=', nd
        write(6,*) '  DIA_C=', STATE_VARIABLES(nd, DIA_C_INDEX)
        write(6,*) '  CYN_C=', STATE_VARIABLES(nd, CYN_C_INDEX)
        write(6,*) '  FIX_CYN_C=', STATE_VARIABLES(nd, FIX_CYN_C_INDEX)
        write(6,*) '  OPA_C=', STATE_VARIABLES(nd, OPA_C_INDEX)
        write(6,*) '  NOST_VEG_HET_C=', STATE_VARIABLES(nd, NOST_VEG_HET_C_INDEX)
        write(6,*) '  NH4_N=', STATE_VARIABLES(nd, NH4_N_INDEX)
        write(6,*) '  NO3_N=', STATE_VARIABLES(nd, NO3_N_INDEX)
        write(6,*) '  PO4_P=', STATE_VARIABLES(nd, PO4_P_INDEX)
        write(6,*) ' PROCESS_RATES FIX_CYN (first 18)=', PROCESS_RATES(nd, FIX_CYN_C_INDEX, 1:min(18,NDIAGVAR))
        write(6,*) ' PROCESS_RATES CYN     (first 18)=', PROCESS_RATES(nd, CYN_C_INDEX, 1:min(18,NDIAGVAR))
        write(6,*) ' DERIVATIVES FIX_CYN=', DERIVATIVES(nd, FIX_CYN_C_INDEX)
        write(6,*) ' DERIVATIVE CYN=', DERIVATIVES(nd, CYN_C_INDEX)
        write(6,*) ' All process rates (first 18) for all states at node', nd
        do i = 1, nstate
            write(6,*) '  STATE=', i
            write(6,*) '    PR(1:18)=', PROCESS_RATES(nd,i,1:min(18,NDIAGVAR))
        end do
        CHLA_DUMP_DONE = .true.
        ! Stop to preserve dump in logs for analysis
        stop
    end if

    ! ---------------------------------------------------------------------------

    ! ---------------------------------------------------------------------------
    ! Calculate the settling suppression factor by chlorophyl-a
    ! ---------------------------------------------------------------------------
    call settling_suppres_factor_vec(CHLA, nkn, SETTLING_VELOCITY_FACTORS)
    ! ---------------------------------------------------------------------------
end subroutine CALCULATE_SETTLING_SUPRESSION


subroutine PELAGIC_KINETICS &
           (PELAGIC_BOX_MODEL_DATA, SEDIMENT_FLUXES, TIME, TIME_STEP, &
            AQUABC_CALLED_BEFORE)

    use GLOBAL
    use PELAGIC_BOX_MODEL
    use aquabc_pel_state_var_indexes
    use AQUABC_PELAGIC_INTERNAL
    use ALLELOPATHY

    implicit none

    type(PELAGIC_BOX_MODEL_DS), intent(in) :: PELAGIC_BOX_MODEL_DATA

    real(kind = DBL), dimension(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)), &
        intent(in) :: SEDIMENT_FLUXES

    real(kind = DBL), intent(in)    :: TIME
    real(kind = DBL), intent(in)    :: TIME_STEP
    real(kind = DBL), intent(inout) :: AQUABC_CALLED_BEFORE

    real(kind = DBL), dimension(nkn, nstate)           :: AQUABC_DERIVATIVES
    real(kind = DBL), dimension(nkn, nstate)           :: AQUABC_STATE_VARIABLES
    real(kind = DBL), dimension(nkn, nstate, NDIAGVAR) :: AQUABC_PROCESS_RATES

    real(kind = DBL), dimension(nkn, NUM_ALLOLOPATHY_STATE_VARS) :: SEC_MET_DERIVATIVES
    integer :: i
    real(kind = DBL) :: fix_cyn

    AQUABC_STATE_VARIABLES(:,:)   = STATE_VARIABLES(:, 1:nstate)
    AQUABC_DERIVATIVES    (:,:)   = 0.0D0
    SEC_MET_DERIVATIVES   (:,:)   = 0.0D0
    AQUABC_PROCESS_RATES  (:,:,:) = 0.0D0

    if (.not.allocated(GROWTH_INHIB_FACTOR_DIA)) then
        allocate(GROWTH_INHIB_FACTOR_DIA(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_DIA).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_DIA)
            allocate(GROWTH_INHIB_FACTOR_DIA(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_CYN)) then
        allocate(GROWTH_INHIB_FACTOR_CYN(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_CYN).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_CYN)
            allocate(GROWTH_INHIB_FACTOR_CYN(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_FIX_CYN)) then
        allocate(GROWTH_INHIB_FACTOR_FIX_CYN(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_FIX_CYN).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_FIX_CYN)
            allocate(GROWTH_INHIB_FACTOR_FIX_CYN(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_OPA)) then
        allocate(GROWTH_INHIB_FACTOR_OPA(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_OPA).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_OPA)
            allocate(GROWTH_INHIB_FACTOR_OPA(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_NOST)) then
        allocate(GROWTH_INHIB_FACTOR_NOST(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_NOST).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_NOST)
            allocate(GROWTH_INHIB_FACTOR_NOST(nkn))
        end if
    end if

    if (.not.allocated(GROWTH_INHIB_FACTOR_ZOO)) then
        allocate(GROWTH_INHIB_FACTOR_ZOO(nkn))
    else
        if (size(GROWTH_INHIB_FACTOR_ZOO).ne.nkn) then
            deallocate(GROWTH_INHIB_FACTOR_ZOO)
            allocate(GROWTH_INHIB_FACTOR_ZOO(nkn))
        end if
    end if

    if (CONSIDER_ALLELOPATHY > 0) then
        call ALLELOPATHY_INHIBITION_RATES()
        GROWTH_INHIB_FACTOR_DIA    (:) = IHBF_SEC_METAB_DIA      (:)
        GROWTH_INHIB_FACTOR_CYN    (:) = IHBF_SEC_METAB_NOFIX_CYN(:)
        GROWTH_INHIB_FACTOR_FIX_CYN(:) = IHBF_SEC_METAB_FIX_CYN  (:)
        GROWTH_INHIB_FACTOR_NOST   (:) = IHBF_SEC_METAB_NOST     (:)
        GROWTH_INHIB_FACTOR_OPA    (:) = IHBF_SEC_METAB_OPA      (:)
        GROWTH_INHIB_FACTOR_ZOO    (:) = IHBF_SEC_METAB_ZOO      (:)
    else
        GROWTH_INHIB_FACTOR_DIA    (:) = 1.0D0
        GROWTH_INHIB_FACTOR_CYN    (:) = 1.0D0
        GROWTH_INHIB_FACTOR_FIX_CYN(:) = 1.0D0
        GROWTH_INHIB_FACTOR_NOST   (:) = 1.0D0
        GROWTH_INHIB_FACTOR_OPA    (:) = 1.0D0
        GROWTH_INHIB_FACTOR_ZOO    (:) = 1.0D0
    end if

    call AQUABC_PELAGIC_KINETICS &
         (node_active                                            , &
          nkn                                                    , &
          AQUABC_STATE_VARIABLES                                 , &
          AQUABC_DERIVATIVES                                     , &
          nstate                                                 , &
          MODEL_CONSTANTS                                        , &
          nconst                                                 , &
          DRIVING_FUNCTIONS                                      , &
          n_driving_functions                                    , &
          FLAGS                                                  , &
          nflags                                                 , &
          AQUABC_PROCESS_RATES                                   , &
          NDIAGVAR                                               , &
          SAVED_OUTPUTS                                          , &
          n_saved_outputs                                        , &
          PH                                                     , &
          TIME                                                   , &
          TIME_STEP                                              , &
          PELAGIC_BOX_MODEL_DATA % DAY_OF_YEAR                   , &
          SEDIMENT_FLUXES                                        , &
          AQUABC_CALLED_BEFORE                                   , &
          SURFACE_BOXES                                          , &
          ZOOPLANKTON_OPTION                                     , &
          ADVANCED_REDOX_SIMULATION                              , &
          USER_ENTERED_frac_avail_DON                            , &
          LIGHT_EXTINCTION_OPTION                                , &
          CYANO_BOUYANT_STATE_SIMULATION                         , &
          CONSIDER_NON_OBLIGATORY_FIXERS                         , &
          CONSIDER_NOSTOCALES)

    DERIVATIVES  (:,1:nstate)    = AQUABC_DERIVATIVES  (:,:)
    PROCESS_RATES(:,1:nstate, :) = AQUABC_PROCESS_RATES(:,:,:)

    if (CONSIDER_ALLELOPATHY > 0) then

        WATER_TEMP(:) = DRIVING_FUNCTIONS(:, 1)

        ALLEL_R_DEATH_DIA       = AQUABC_PROCESS_RATES(:, DIA_C_INDEX         , 4)
        ALLEL_R_DEATH_NOFIX_CYN = AQUABC_PROCESS_RATES(:, CYN_C_INDEX         , 4)
        ALLEL_R_DEATH_FIX_CYN   = AQUABC_PROCESS_RATES(:, FIX_CYN_C_INDEX     , 4)
        ALLEL_R_DEATH_NOST      = AQUABC_PROCESS_RATES(:, NOST_VEG_HET_C_INDEX, 4)

        ! Diagnostic: check allelopathy death relative to biomass for fixing cyanobacteria
        do i = 1, nkn
            fix_cyn = STATE_VARIABLES(i, FIX_CYN_C_INDEX)
            if (fix_cyn > 0.0D0) then
                if (ALLEL_R_DEATH_FIX_CYN(i) > 0.5D0 * fix_cyn / TIME_STEP) then
                    write(6,*) 'DEBUG: ALLEL_R_DEATH large relative to FIX_CYN biomass at node', i
                    write(6,*) '  TIME=', TIME, 'TIME_STEP=', TIME_STEP
                    write(6,*) '  FIX_CYN_C=', fix_cyn
                    write(6,*) '  ALLEL_R_DEATH_FIX_CYN=', ALLEL_R_DEATH_FIX_CYN(i)
                    write(6,*) '  PROCESS_RATES FIX_CYN (1:6)=', AQUABC_PROCESS_RATES(i,FIX_CYN_C_INDEX,1:6)
                end if
            end if
        end do

        call allelopathy_SEC_METABOLITES(nkn)

        ! DERIVATIVES_SEC_METAB is fetched from unit ALLELOPATHY
        SEC_MET_DERIVATIVES(:,:) = DERIVATIVES_SEC_METAB(:,:)

        DERIVATIVES(:,(nstate+1):(nstate+NUM_ALLOLOPATHY_STATE_VARS)) = &
            SEC_MET_DERIVATIVES(:,:)
    else
        SEC_MET_DERIVATIVES(:,:) = 0.0D0
    end if

end subroutine
