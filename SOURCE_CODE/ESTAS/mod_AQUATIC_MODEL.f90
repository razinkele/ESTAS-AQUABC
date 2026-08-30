! ---------------------------------------------------------------------------------------
! This is the module for the AQUATIC model as a general framwwork containing
!
!     - The pelagic modeling infrastructure
!     - The bottom sediments modeling infrastructure
!     - The resuspension modeling infrastructure
! ---------------------------------------------------------------------------------------
! The aquatic model data strucutre (type AQUATIC_MODEL_DS) includes a pelagic model
! data structure (PELAGIC_BOX_MODEL_DATA) but no other sub-model data structures such
! as BOTTOM_SEDIMENTS or RESUSPENSION data structure. This is because that only the 
! pelagic sub-model is able to transfer the materials from basin to basin horizontally
! with advection and diffusion in any direction hence it forms the shape of the
! entire model domain. Bottom sediment and resuspension sub-models are simply associated
! and interacting with the designated pelagic boxes in vertical direction only.
! ---------------------------------------------------------------------------------------
module AQUATIC_MODEL
    use GLOBAL
    use WATER_SEDIMENT_COUPLING, only: wsc
    use PELAGIC_BOX_MODEL
    use RESUSPENSION
    use BOTTOM_SEDIMENTS
    use INITIALIZE_PELAGIC_BOX_MODEL
    use TIME_SERIES

    implicit none

    type AQUATIC_MODEL_DS
        integer          :: BASE_YEAR
        real(kind = DBL) :: SIMULATION_START
        real(kind = DBL) :: SIMULATION_END
        integer          :: TIME_STEPS_PER_DAY
        real(kind = DBL) :: TIME_STEP
        integer          :: PRINT_INTERVAL
        integer          :: NUM_REPEATS
        integer          :: DAY_OF_YEAR

        type(PELAGIC_BOX_MODEL_DS) :: PELAGIC_BOX_MODEL_DATA
    end type AQUATIC_MODEL_DS

contains

    ! ---------------------------------------------------------------------------------------
    ! SUBROUTINE TO MANAGE THE READING AND PARTLY PREPROCESSING OPERATION OF THE AQUATIC
    ! MODEL INPUTS
    ! ---------------------------------------------------------------------------------------
    subroutine READ_AQUATIC_MODEL_INPUTS(AQUATIC_MODEL_DATA, IN_FILE, OUT_FILE)

        implicit none
        type(AQUATIC_MODEL_DS), intent(inout) :: AQUATIC_MODEL_DATA
        integer, intent(in) :: IN_FILE
        integer, intent(in) :: OUT_FILE

        integer :: i, j
        character(len = 2048) :: FILE_NAME
        character(len = 2048) :: RESUSP_CANDIDATE_PATH
        logical :: exists
        real(kind = DBL)      :: TIME

        integer :: NUM_CHECK_PELAGIC_STATE_VARS
        character(len = 2048) :: TMP_STR
        logical :: MODEL_BOTTOM_SED_PRESET
        integer :: MODEL_BOTTOM_SEDIMENTS_PRESET
        integer :: iostat_tmp

        integer :: nlen
        character(len=1) :: LASTCHAR

        !READ DESCRIPTION LINES
        do i = 1, 5
            read(unit = IN_FILE, fmt = *)
        end do

        !READ BASIC MODEL SETUP INFORMATION
        read (unit = IN_FILE, fmt = *)
        read (unit = IN_FILE, fmt = *) AQUATIC_MODEL_DATA % BASE_YEAR

        read (unit = IN_FILE, fmt = *)
        read (unit = IN_FILE, fmt = *) AQUATIC_MODEL_DATA % SIMULATION_START

        write(unit = *, fmt = *) &
              'SIMULATION START  (Julian days) : ', AQUATIC_MODEL_DATA % SIMULATION_START

        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) AQUATIC_MODEL_DATA % SIMULATION_END

        write(unit = *, fmt = *) &
              'SIMULATION END     (Julian days) : ', AQUATIC_MODEL_DATA % SIMULATION_END

        !Read the number of repeating simulations
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) AQUATIC_MODEL_DATA % NUM_REPEATS

        write(unit = *, fmt = *) &
              'NUMBER OF REPEATS (Julian days) : ', AQUATIC_MODEL_DATA % SIMULATION_END

        !Read time step
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) AQUATIC_MODEL_DATA % TIME_STEPS_PER_DAY

        AQUATIC_MODEL_DATA % TIME_STEP = 1.0D0 / float(AQUATIC_MODEL_DATA % TIME_STEPS_PER_DAY)

        write(unit = *, fmt = *) &
              'TIME STEP               (days) : ', AQUATIC_MODEL_DATA % TIME_STEP

        !Read the print interval
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) AQUATIC_MODEL_DATA % PRINT_INTERVAL

        write(unit = *, fmt = *) &
              'PRINT INTERVAL   (time steps) : ', AQUATIC_MODEL_DATA % PRINT_INTERVAL

        !Read the pelagic model output folder
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) PELAGIC_INPUT_FOLDER

        ! This is for linux
        !DEC$ IF DEFINED(_WIN32)
        print *,'Windows'
        !DEC$ ELSEIF DEFINED(__linux)
        print *,'Operating system is Linux'
        PELAGIC_INPUT_FOLDER = trim(PELAGIC_INPUT_FOLDER) // '/'
        ! End linux
        !DEC$ ELSE
        print *, 'Oops'
        !DEC$ ENDIF


        write(unit = *, fmt = *) &
              'PELAGIC_INPUT_FOLDER : ', trim(adjustl(PELAGIC_INPUT_FOLDER))

        !Read the pelagic model input file name
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) FILE_NAME

        call OPEN_INPUT_FILE(IN_FILE + 2, &
             trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
             'model input')

        !Read the pelagic model output folder
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) PELAGIC_OUTPUT_FOLDER

        ! This is for linux
        !DEC$ IF DEFINED(_WIN32)
        print *,'Windows'
        !DEC$ ELSEIF DEFINED(__linux)
        print *,'Operating system is Linux'
        PELAGIC_OUTPUT_FOLDER = trim(PELAGIC_OUTPUT_FOLDER) // '/'
        ! End linux
        !DEC$ ELSE
        print *, 'Oops'
        !DEC$ ENDIF

        write(unit = *, fmt = *) &
              'PELAGIC_OUTPUT_FOLDER : ', trim(adjustl(PELAGIC_OUTPUT_FOLDER))

        !Read the pelagic model inputs
        call READ_PELAGIC_BOX_MODEL_INPUTS &
             (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, IN_FILE + 2, IN_FILE + 4)

        close(IN_FILE + 2)
        close(IN_FILE + 4)

        TIME      = AQUATIC_MODEL_DATA % SIMULATION_START
        INIT_TIME = TIME
        call INIT_AQUATIC_MODEL(AQUATIC_MODEL_DATA, TIME)

        nkn = AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES

        ! The input file declares the TOTAL state-variable count, i.e. the
        ! AQUABC states plus the secondary-metabolite block when allelopathy is
        ! on. The allelopathy branch used to compare that number with itself,
        ! which asserted nothing; with a build-variant state count (VARN,
        ! nstate = 33) a mis-paired setup must fail loudly instead.
        NUM_CHECK_PELAGIC_STATE_VARS = &
            nstate + merge(NUM_ALLOLOPATHY_STATE_VARS, 0, CONSIDER_ALLELOPATHY > 0)

        if (CONSIDER_ALLELOPATHY > 0) then
            call ALLOC_ALLEOPATHY(nkn)
        end if

        if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS.ne. &
            NUM_CHECK_PELAGIC_STATE_VARS) then

            write(*,*) &
                 'The number of pelagic state variables are not compitable', &
                 ' with AQUABC pelagic module'

            write(*,*) &
                'The number of pelagic state variables                     : ', &
                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS

            write (*,*) 'The number of pelagic state variables requested by AQUABC : ', &
                NUM_CHECK_PELAGIC_STATE_VARS
            error stop "error stop"
        end if

        if (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_MODEL_CONSTANTS.ne. &
            nconst) then

            write(*,*) &
                 'The number of pelagic model constants are not compitable', &
                 ' with AQUABC pelagic module'

            write(*,*) &
                'The number of pelagic model constants                             : ', &
                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % NUM_MODEL_CONSTANTS

            write (*,*) 'The number of pelagic model constants requested by AQUABC : ', nconst
            stop "error stop"
        end if

        allocate(pcore%node_active(nkn)                                      )
        allocate(STATE_VARIABLES  (nkn,(nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(pcore%DERIVATIVES      (nkn,(nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(MODEL_CONSTANTS  (nconst)                                   )
        allocate(pcore%DRIVING_FUNCTIONS(nkn,n_driving_functions)                  )
        allocate(pcore%FLAGS            (nflags)                                   )
        allocate(PROCESS_RATES    (nkn,(nstate + NUM_ALLOLOPATHY_STATE_VARS), NDIAGVAR))
        allocate(pcore%SAVED_OUTPUTS(nkn,n_saved_outputs)                                )
        allocate(pH               (nkn)                                                )
        allocate(pcore%CHLA(nkn)                                                )
        allocate(pcore%SURFACE_BOXES(nkn)                                                )

        allocate(wsc%FLUXES_TO_WATER_COLUMN       (nkn,(nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(wsc%FLUXES_OUTPUT_TO_WATER_COLUMN(nkn,(nstate + NUM_ALLOLOPATHY_STATE_VARS)))

        do i = 1, nkn
            pcore%SURFACE_BOXES(i) = &
                AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % SURFACE_BOX
        end do

        MODEL_CONSTANTS = AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % MODEL_CONSTANTS(:, 1)

        allocate(wsc%DISSOLVED_FRACTIONS           (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(wsc%FRACTION_OF_DEPOSITION        (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(wsc%SETTLING_RATES                (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(wsc%NOT_DEPOSITED_FLUXES          (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(wsc%FLUXES                        (nkn, NUM_SED_VARS                         ))
        allocate(wsc%SETTLING_VELOCITIES_OUTPUT    (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(wsc%EFFECTIVE_DISSLOVED_FRACTIONS (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(wsc%EFFECTIVE_DEPOSITION_FRACTIONS(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(wsc%DEPOSITION_AREA_RATIOS        (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))

        call INIT_PELAGIC_MODEL_CONSTANTS()

        ! Print out the water levels
        if (PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT == 0) then

            open (unit   = 1000, &
                  file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // 'WATER_LEVELS.out', &
                  status = 'UNKNOWN')

            write(unit = 1000, fmt = '(2A10, 3A30)') &
                  '      TIME', '    BOX NO',  &
                  '         SURFACE ELEVATION (m)', '                  VOLUME (m^3)', &
                  '                     DEPTH (m)'

            open (unit   = 1001, &
                  file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // 'MASS_BALANCES.out', &
                  status = 'UNKNOWN')

            write(unit = 1001, fmt = '(3A10, 7A30)') &
                  '      TIME', '    BOX NO',  '    VAR_NO', &
                  '        ADVECTION (g/m^3/days)', '        DIFFUSION (g/m^3/days)', &
                  '         SETTLING (g/m^3/days)', '       MASS LOADS (g/m^3/days)', &
                  ' MASS WITHDRAWALS (g/m^3/days)', '         KINETICS (g/m^3/days)', &
                  '  SEDIMENT FLUXES (g/m^3/days)'
        else
            open(unit   = 1001, &
                 file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                          trim(adjustl(BINARY_PELAGIC_OUTPUT_FILE_NAME)) // "_" // &
                          'MASS_BALANCES.bin', &
                 status = 'UNKNOWN', access = 'STREAM')
        end if

        if (PRODUCE_COCOA_OUTPUTS > 0) then
            open(unit   = 2020, &
                 file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                          trim(adjustl(COCOA_PELAGIC_OUTPUTS_FILENAME)), &
                 status = 'UNKNOWN')

            write(unit = 2020, fmt = '(3A10, 8A30)') &
                  '      TIME', '    BOX NO', '  LAYER NO', &
                  '         N_PEL_DENITRIFICATION', '                    N_FIXATION', &
                  '                   N_PEL_ASSIM', '                  N_PEL_EXCRET', &
                  '           N_PEL_DECOMP_OF_DET', '                   P_PEL_ASSIM', &
                  '                  P_PEL_EXCRET', '           P_PEL_DECOMP_OF_DET'
        end if
        ! -----------------------------------------------------------------------------------
        ! END OF INITIALIZATION OF THE WATER COLUMN MODEL
        ! -----------------------------------------------------------------------------------

        ! -----------------------------------------------------------------------------------
        ! READING THE RESUSPENSION RELATED MODEL INPUTS
        ! -----------------------------------------------------------------------------------
        ! Option 1 : Fully prescribed resuspension. The user supplies the time series
        !            for the resuspension velocities and resuspended concentrations.
        !            A resuspension velocity v_r (m/day) and, for each state variable j, 
        !            a resuspended bed concentration C_bed,j are read from time series. 
        !            An upward source term is then added to every state-variable derivative,
        !            dC_j/dt  +=  f_area · A · v_r · C_bed,j,   where f_area is the 
        !            resuspension-active area fraction of the box and A its surface area. 
        !            This explicitly returns bed material — predominantly particulate matter 
        !            and its bound nutrients — to the overlying water.
        !
        ! Option 2 : Semi-prescribed resuspension.
        !            For each activated box the bottom shear stress is read from a prescribed 
        !            time series and compared with a per-box critical shear stress 
        !            (BOX_CRITICAL_SHEAR_STRESSES, read from an input file). When the bottom 
        !            shear stress exceeds the critical shear stress the flag 
        !            SHUT_DOWN_SETTLING is set to 1 and the entire settling-derivative 
        !            block is skipped for that box and time step. Physically, an erosive bed 
        !            keeps particles in suspension: material that would otherwise settle out 
        !            is retained in the water column. This mode adds no mass — it simply 
        !            switches off a loss term during energetic (e.g. windy) periods.
        !
        ! Option 3 : TO BE IMPLEMENTED, such as option one except that the concentrations 
        !            will be the cocentrations from the bottom sediment diagenesis model
        !            here, care must be taken to ensure that the resuspended masses would be
        !            substracted from the bottom sediments. Also it may be important to 
        !            consider the number of bottom sediment layers involved
        !
        ! Option 4 : TO BE IMPLEMENTED, almost like option 3 except that the sediment 
        !            resuspension will only proceed if the critical shear stress is exceeded
        !            and the settling is stopped in such a case.
        !
        ! Further options:  TO BE IMPLEMENTED, according to document 
        !                   "Sediment_Resuspension_in_AQUABC.docx" produced by Claude in
        !                   23/7/2026 Klaipeda
        ! -----------------------------------------------------------------------------------
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) resusp%RESUSPENSION_OPTION
        write(unit = *, fmt = *) 'RESUSPENSION_OPTION : ', resusp%RESUSPENSION_OPTION

        ! NOST akinete life-cycle staging (NOST_STAGE_MODEL, read earlier by
        ! READ_PELAGIC_BOX_MODEL_INPUTS -> READ_PELAGIC_MODEL_OPTIONS above) is
        ! incompatible with resuspension: BED_AKI is a staging-only pool, invisible to
        ! bed erosion, so an active resuspension option would silently fail to
        ! resuspend it. Both flags are in scope here (GLOBAL, RESUSPENSION); fail fast
        ! at read time on the file-requested option, before any later downgrade to 0.
        if (NOST_STAGE_MODEL > 0 .and. resusp%RESUSPENSION_OPTION > 0) then
            error stop 'NOST_STAGE_MODEL=1 is incompatible with resuspension: BED_AKI is invisible '// &
                       'to bed erosion (see the 2026-08-23 staging spec, s.6.1)'
        end if

        SHUT_DOWN_SETTLING = 0
        MODEL_BOTTOM_SED_PRESET = .false.

        ! Default to NO bottom-sediment model unless the input file or a preset
        ! explicitly requests sediments. This makes the "no sediment" configuration
        ! the default behavior for backward compatibility with lightweight runs.
        MODEL_BOTTOM_SEDIMENTS = 0

        if (resusp%RESUSPENSION_OPTION < 1) then
            resusp%CONSIDER_RESUSPENSION = 0
            write(unit = *, fmt = *) &
                  'RESUSPENSION_OPTION : ', resusp%RESUSPENSION_OPTION

            write(unit = *, fmt = *) 'Resuspension will not be considered'
        else
            !Read the resuspension model output folder
            ! Read raw lines and skip blank/comment lines to find the next data token
            TMP_STR = ''
            do
                read(unit = IN_FILE, fmt = '(A)', iostat = iostat_tmp) TMP_STR
                if (iostat_tmp /= 0) then
                    TMP_STR = ''
                    exit
                end if
                if (len_trim(TMP_STR) == 0) cycle
                if (index(adjustl(TMP_STR),'#') == 1) cycle
                exit
            end do

            MODEL_BOTTOM_SED_PRESET = .false.
            TMP_STR = adjustl(trim(TMP_STR))
            ! If TMP_STR contains only digits, treat it as MODEL_BOTTOM_SEDIMENTS preset
            if (len_trim(TMP_STR) == 1 .and. TMP_STR(1:1) >= '0' .and. TMP_STR(1:1) <= '9') then
                read(TMP_STR, *, iostat = iostat_tmp) MODEL_BOTTOM_SEDIMENTS_PRESET
                if (iostat_tmp == 0) then
                    MODEL_BOTTOM_SED_PRESET = .true.

                    write(unit = *, fmt = *) &
                        'Notice: RESUSPENSION block missing in INPUT. Skipping resuspension.'

                    resusp%RESUSPENSION_OPTION = 0
                else
                    resusp%RESUSPENSION_INPUT_FOLDER = TMP_STR
                end if
            else if (len_trim(TMP_STR) > 0 .and. verify(TMP_STR, '0123456789') == 0) then
                ! Multi-digit numeric string
                read(TMP_STR, *, iostat = iostat_tmp) MODEL_BOTTOM_SEDIMENTS_PRESET
                if (iostat_tmp == 0) then
                    MODEL_BOTTOM_SED_PRESET = .true.

                    write(unit = *, fmt = *) &
                        'Notice: RESUSPENSION block missing in INPUT. Skipping resuspension.'

                    resusp%RESUSPENSION_OPTION = 0
                else
                    resusp%RESUSPENSION_INPUT_FOLDER = TMP_STR
                end if
            else
                resusp%RESUSPENSION_INPUT_FOLDER = TMP_STR
            end if

            if (.not. MODEL_BOTTOM_SED_PRESET) then
                ! Ensure trailing slash (portable, works with gfortran and ifort)
                if (len_trim(resusp%RESUSPENSION_INPUT_FOLDER) > 0) then
                    nlen = len_trim(resusp%RESUSPENSION_INPUT_FOLDER)
                    LASTCHAR = resusp%RESUSPENSION_INPUT_FOLDER(nlen:nlen)
                    if (LASTCHAR /= '/' .and. LASTCHAR /= '\\') then
                        resusp%RESUSPENSION_INPUT_FOLDER = trim(resusp%RESUSPENSION_INPUT_FOLDER) // '/'
                    end if
                end if

                write(unit = *, fmt = *) &
                      'RESUSPENSION INPUT FOLDER : ', trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER))
            end if

            if (.not. MODEL_BOTTOM_SED_PRESET) then
                !Read the resuspension model input file name
                read(unit = IN_FILE, fmt = *)
                read(unit = IN_FILE, fmt = *) FILE_NAME

                !Read the resuspension model output folder
                read(unit = IN_FILE, fmt = *)
                read(unit = IN_FILE, fmt = *) resusp%RESUSPENSION_OUTPUT_FOLDER

                ! Ensure trailing slash (portable, works with gfortran and ifort)
                if (len_trim(resusp%RESUSPENSION_OUTPUT_FOLDER) > 0) then
                        nlen = len_trim(resusp%RESUSPENSION_OUTPUT_FOLDER)
                        LASTCHAR = resusp%RESUSPENSION_OUTPUT_FOLDER(nlen:nlen)
                        if (LASTCHAR /= '/' .and. LASTCHAR /= '\\') then
                            resusp%RESUSPENSION_OUTPUT_FOLDER = trim(resusp%RESUSPENSION_OUTPUT_FOLDER) // '/'
                        end if
                    end if
                write(unit = *, fmt = *) &
                      'RESUSPENSION OUTPUT FOLDER : ', &
                      trim(adjustl(resusp%RESUSPENSION_OUTPUT_FOLDER))

                ! Verify the resuspension input file exists. If not, try PELAGIC_INPUT_FOLDER as a fallback.
                RESUSP_CANDIDATE_PATH = trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER)) // trim(adjustl(FILE_NAME))
                inquire(file = trim(RESUSP_CANDIDATE_PATH), exist = exists)
                if (.not. exists) then
                    write(unit = *, fmt = *) 'Warning: Resuspension input file "' // &
                         trim(adjustl(FILE_NAME)) // '"'
                    write(unit = *, fmt = *) ' not found in folder "' // &
                         trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER)) // '"'
                    write(unit = *, fmt = *) 'Trying PELAGIC_INPUT_FOLDER.'
                    RESUSP_CANDIDATE_PATH = trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME))
                    inquire(file = trim(RESUSP_CANDIDATE_PATH), exist = exists)
                    if (exists) then
                        write(unit = *, fmt = *) 'Info: Found resuspension file in PELAGIC_INPUT_FOLDER: ' // &
                             trim(adjustl(RESUSP_CANDIDATE_PATH))

                        resusp%RESUSPENSION_INPUT_FOLDER = trim(PELAGIC_INPUT_FOLDER)
                    else
                        write(unit = *, fmt = *) 'Error: Resuspension file "' // &
                             trim(adjustl(FILE_NAME)) // &
                             '" not found in either RESUSPENSION_INPUT_FOLDER or PELAGIC_INPUT_FOLDER.'

                        write(unit = *, fmt = *) 'Skipping resuspension.'
                        resusp%CONSIDER_RESUSPENSION = 0
                        resusp%RESUSPENSION_OPTION   = 0
                    end if
                end if
            end if

            select case (resusp%RESUSPENSION_OPTION)

                case (1)
                    resusp%CONSIDER_RESUSPENSION = 1

                    write(unit = *, fmt = *) &
                        'Resuspension will be considered as in Option 1'

                    !Open the resuspension model input file
                    call OPEN_INPUT_FILE(IN_FILE + 2, &
                         trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
                         'model input')

                    !Read the resuspension model inputs
                    call READ_RESUSPENSION_FILE_OPTION_1(IN_FILE + 2)
                    close(IN_FILE + 2)

                case (2)
                    resusp%CONSIDER_RESUSPENSION = 1
                    write(unit = *, fmt = *) &
                          'RESUSPENSION_OPTION : ', resusp%RESUSPENSION_OPTION

                    write(unit = *, fmt = *) &
                        'Resuspension will be considered as in Option 2'

                    !Open the resuspension model input file
                    call OPEN_INPUT_FILE(IN_FILE + 2, &
                         trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
                         'model input')

                    call READ_RESUSPENSION_FILE_OPTION_2(IN_FILE + 2)
                    close(IN_FILE + 2)

                case (3)
                    resusp%CONSIDER_RESUSPENSION = 1

                    write(unit = *, fmt = *) &
                        'RESUSPENSION_OPTION : ', resusp%RESUSPENSION_OPTION

                    write(unit = *, fmt = *) &
                        'Resuspension will be considered as in Option 3'

                    !Open the resuspension model input file
                    call OPEN_INPUT_FILE(IN_FILE + 2, &
                         trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
                         'model input')

                    call READ_RESUSPENSION_FILE_OPTION_3(IN_FILE + 2)
                    close(IN_FILE + 2)

            end select
        end if
        ! -----------------------------------------------------------------------------------
        ! END OF READING THE RESUSPENSION RELATED MODEL INPUTS
        ! -----------------------------------------------------------------------------------

        ! -----------------------------------------------------------------------------------
        ! READING THE BOTTOM SEDIMENT MODEL INPUTS
        ! -----------------------------------------------------------------------------------
        if (MODEL_BOTTOM_SED_PRESET) then

            write(unit = *, fmt = *) &
                'Using preset MODEL_BOTTOM_SEDIMENTS: ', MODEL_BOTTOM_SEDIMENTS_PRESET

            MODEL_BOTTOM_SEDIMENTS = MODEL_BOTTOM_SEDIMENTS_PRESET
        else
            read(unit = IN_FILE, fmt = *)
            read(unit = IN_FILE, fmt = *) MODEL_BOTTOM_SEDIMENTS
        end if

        if (MODEL_BOTTOM_SEDIMENTS == 1) then
            read(unit = IN_FILE, fmt = *)
            read(unit = IN_FILE, fmt = *) NUM_PRESCRIBED_SEDIMENT_FLUX_SETS
            read(unit = IN_FILE, fmt = *)

            allocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                         SEDIMENT_FLUX_TS_NOS &
                             (NUM_PRESCRIBED_SEDIMENT_FLUX_SETS, nkn, &
                              AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                  NUM_PELAGIC_STATE_VARS))

            allocate(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                         SEDIMENT_FLUX_TS_VAR_NOS &
                             (NUM_PRESCRIBED_SEDIMENT_FLUX_SETS, nkn, &
                              AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA % &
                                  NUM_PELAGIC_STATE_VARS))

            do i = 1, NUM_PRESCRIBED_SEDIMENT_FLUX_SETS
                read(unit = IN_FILE, fmt = *) FILE_NAME

                call OPEN_INPUT_FILE(IN_FILE + 2, &
                     trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
                     'model input')

                call READ_BOTTOM_SEDS_FLUXES_INPUTS &
                         (AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, IN_FILE + 2, i)

                close(IN_FILE + 2)
            end do
        end if

        if (MODEL_BOTTOM_SEDIMENTS > 1) then
            if ((resusp%RESUSPENSION_OPTION == 1) .or. &
                (resusp%RESUSPENSION_OPTION == 2)) then
                write(unit = *, fmt = *) &
                      'Bottom sediments are not coupled with resuspension ' // &
                      'in this version of ESTAS-AQUABC. Program halted.'
                stop
            end if

            read(unit = IN_FILE, fmt = *)
            read(unit = IN_FILE, fmt = *) FILE_NAME

            call OPEN_INPUT_FILE(IN_FILE + 2, &
                 trim(adjustl(PELAGIC_INPUT_FOLDER)) // trim(adjustl(FILE_NAME)), &
                 'model input')

            call READ_BOTTOM_SEDIMENTS_MODEL_INPUTS(IN_FILE + 2)

            if (ADVANCED_REDOX_SIMULATION.ne. &
                BOTTOM_SED_ADVANCED_REDOX_SIMULATION) then

                write(*, *) 'Pelagic model and bottom sediment model cannot have different'
                write(*, *) 'options for advanced redox simulation'

                if (ADVANCED_REDOX_SIMULATION > 0) then

                    write(*, *) 'Advanced redox simulation option is ON by the pelagic model'
                else
                    write(*, *) 'Advanced redox simulation option is OFF by the pelagic model'
                end if

                if (BOTTOM_SED_ADVANCED_REDOX_SIMULATION > 0) then

                    write(*, *) 'Advanced redox simulation option is ON by the bottom sediment model'
                else
                    write(*, *) 'Advanced redox simulation option is OFF by the bottom sediment model'
                end if
            end if

            call INIT_BSED_MODEL_CONSTANTS()

            !call INIT_BS(INIT_SED_STATE_VARS, nkn, NUM_SED_LAYERS, NUM_SED_VARS)


            open(unit   = 1021, &
                 file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                          trim(adjustl(BOTTOM_SEDIMENT_CONCENTRATIONS_FILENAME)), &
                 status = 'UNKNOWN')

            open(unit   = 1023, &
                 file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                          trim(adjustl(BOTTOM_SEDIMENT_FLUXES_FILENAME)), &
                 status = 'UNKNOWN')

            if (PRODUCE_COCOA_OUTPUTS > 0) then

                open(unit   = 2021, &
                     file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                              trim(adjustl(COCOA_SEDIMENT_PROCESS_RATES_FILENAME)), &
                     status = 'UNKNOWN')

                write(unit  = 2021, fmt = '(3A10, 8A30)') &
                      '      TIME', '    BOX NO', '  LAYER NO', &
                      '         N_SED_DENITRIFICATION', '        N_SED_REMINERALIZATION', &
                      '        P_SED_REMINERALIZATION'


                open(unit   = 2022, &
                     file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                              trim(adjustl(COCOA_SEDIMENT_BURIAL_RATES_FILENAME)) , &
                     status = 'UNKNOWN')

                write(unit  = 2022, fmt = '(3A10, 2A30)') &
                      '      TIME', '    BOX NO', '  LAYER NO', &
                      '                     N_BURRIAL', '                     P_BURRIAL'


                open(unit   = 2031, &
                     file   = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                              trim(adjustl(COCOA_FLUXES_FROM_SEDIMENTS_FILENAME)) , &
                     status = 'UNKNOWN')

                write(unit  = 2031, fmt = '(3A10, 2A30)') &
                      '      TIME', '    BOX NO', '  LAYER NO', &
                      '                N_OUT_FROM_SED', '                P_OUT_FROM_SED'


                open(unit    = 2032, &
                     file    = trim(adjustl(PELAGIC_OUTPUT_FOLDER)) // &
                               trim(adjustl(COCOA_FLUXES_TO_SEDIMENTS_FILENAME))   , &
                      status = 'UNKNOWN')

                write(unit  = 2032, fmt = '(3A10, 3A40)') &
                      '      TIME', '    BOX NO', '  LAYER NO', &
                      '    N_FLX_FROM_WATER_TO_SED_DUE_TO_DENIT', &
                      '              N_PARTICULATE_ORG_INTO_SED', &
                      '              P_PARTICULATE_ORG_INTO_SED'
            end if

            do i = 1, nkn
                do j = 1, NUM_SED_LAYERS
                    write(unit = 1021, fmt = '(F10.4,2I10,24F20.10)') &
                          TIME, i, j, bsed%INIT_SED_STATE_VARS(i, j, :)
                end do
            end do

            close(IN_FILE + 2)
        else
            wsc%FLUXES_TO_WATER_COLUMN        = 0.0D0
            wsc%FLUXES_OUTPUT_TO_WATER_COLUMN = 0.0D0
        end if
        ! -----------------------------------------------------------------------------------
        ! READING THE BOTTOM SEDIMENT MODEL INPUTS
        ! -----------------------------------------------------------------------------------

    end subroutine READ_AQUATIC_MODEL_INPUTS
    ! ---------------------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------------------


    ! ---------------------------------------------------------------------------------------
    ! SUBROUTINE TO MANAGE THE INITIALIZATION OF THE AQUATIC MODEL THROUGH THE 
    ! AQUATIC_MODEL_DS
    ! ---------------------------------------------------------------------------------------
    subroutine INIT_AQUATIC_MODEL(AQUATIC_MODEL_DATA, TIME)
        implicit none
        type(AQUATIC_MODEL_DS), intent(inout) :: AQUATIC_MODEL_DATA
        real(kind = DBL), intent(in) :: TIME


        !INITIALIZE THE PELAGIC BOX MODEL
        call INIT_PELAGIC_BOX_MODEL(AQUATIC_MODEL_DATA % PELAGIC_BOX_MODEL_DATA, TIME)

    end subroutine INIT_AQUATIC_MODEL
    ! ---------------------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------------------

end module AQUATIC_MODEL
