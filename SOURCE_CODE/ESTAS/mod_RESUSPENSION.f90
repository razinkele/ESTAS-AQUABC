module RESUSPENSION
    use GLOBAL

    implicit none

contains

    subroutine READ_RESUSPENSION_FILE_OPTION_1(IN_FILE)
        implicit none
        integer, intent(in) :: IN_FILE

        integer :: BOX_NO, STATE_VAR_NO, RESUSPENSION_TS_NO
        integer :: AUX_INTEGER_1, AUX_INTEGER_2, AUX_INTEGER_3, AUX_INTEGER_4
        real(kind = DBL) :: AUX_DBL_1
        integer :: SUM_ACTIVE_BOXES

        character(len = 2048) :: AUX_STRING

        allocate(ACTIVATE_RESUSPENSIONS      (nkn))
        allocate(RESUSPENSION_VEL_TS_NOS     (nkn))
        allocate(RESUSPENSION_VEL_TS_VAR_NOS (nkn))
        allocate(RESUSPENSION_CONC_TS_NOS    (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(RESUSPENSION_CONC_TS_VAR_NOS(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(FRAC_RESUSPENSION_AREAS     (nkn))

        SUM_ACTIVE_BOXES = 0

        ! Read the info lines
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)

        ! Read the number of resuspension time series
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) NUM_RESUSPENSION_TS
        allocate(RESUSPENSION_TS_FILE_NAMES(NUM_RESUSPENSION_TS))
        allocate(RESUSPENSION_TS           (NUM_RESUSPENSION_TS))

        ! Read the resuspension file names
        read(unit = IN_FILE, fmt = *)

        do RESUSPENSION_TS_NO = 1, NUM_RESUSPENSION_TS
            read(unit = IN_FILE, fmt = *) AUX_INTEGER_1, AUX_STRING
            RESUSPENSION_TS_FILE_NAMES(AUX_INTEGER_1) = AUX_STRING

            open(unit   = IN_FILE + 1, &
                 file   = trim(adjustl(RESUSPENSION_INPUT_FOLDER)) // &
                          trim(adjustl(RESUSPENSION_TS_FILE_NAMES(AUX_INTEGER_1))), &
                 status = 'OLD')

            call INITIALIZE_TIME_SERIE    (RESUSPENSION_TS(RESUSPENSION_TS_NO))
            call READ_TIME_SERIE_FROM_FILE(RESUSPENSION_TS(RESUSPENSION_TS_NO), IN_FILE+1)
            close(IN_FILE + 1)
        end do

        ! Read the pelegic box options for resuspension
        read(unit = IN_FILE, fmt = *)

        do BOX_NO = 1,nkn
            read(unit = IN_FILE, fmt = *) &
                 AUX_INTEGER_1, AUX_INTEGER_2, AUX_DBL_1, AUX_INTEGER_3, AUX_INTEGER_4

            if (AUX_INTEGER_2 > 1) then
                AUX_INTEGER_2 = 1
            end if

            if (AUX_INTEGER_2 < 0) then
                AUX_INTEGER_2 = 0
            end if

            ACTIVATE_RESUSPENSIONS     (AUX_INTEGER_1) = AUX_INTEGER_2
            FRAC_RESUSPENSION_AREAS    (AUX_INTEGER_1) = AUX_DBL_1
            RESUSPENSION_VEL_TS_NOS    (AUX_INTEGER_1) = AUX_INTEGER_3
            RESUSPENSION_VEL_TS_VAR_NOS(AUX_INTEGER_1) = AUX_INTEGER_4

            SUM_ACTIVE_BOXES = SUM_ACTIVE_BOXES + AUX_INTEGER_2
        end do

        ! Read the resuspension velocity time series pelagic boxes
        read(unit = IN_FILE, fmt = *)

        do BOX_NO = 1, SUM_ACTIVE_BOXES
            do STATE_VAR_NO = 1, nstate

                read(unit = IN_FILE, fmt = *) &
                     AUX_INTEGER_1, AUX_INTEGER_2, AUX_INTEGER_3, AUX_INTEGER_4

                RESUSPENSION_CONC_TS_NOS    (AUX_INTEGER_1, AUX_INTEGER_2) = AUX_INTEGER_3
                RESUSPENSION_CONC_TS_VAR_NOS(AUX_INTEGER_1, AUX_INTEGER_2) = AUX_INTEGER_4
            end do
        end do

    end subroutine READ_RESUSPENSION_FILE_OPTION_1


    ! This is the subroutine for option 2. It basically reads a critical shear stress file
    ! for all the model boxes.
    subroutine READ_RESUSPENSION_FILE_OPTION_2(IN_FILE)
        implicit none
        integer, intent(in) :: IN_FILE

        integer :: AUX_INTEGER_1, AUX_INTEGER_2, AUX_INTEGER_3, AUX_INTEGER_4
        integer :: i, RESUSPENSION_TS_NO, BOX_NO
        real(kind = DBL) :: AUX_DBL_1
        character(len = 2048) :: AUX_STRING

        allocate (BOX_CRITICAL_SHEAR_STRESSES(nkn))

        ! Read the info lines
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)

        ! Read the number of resuspension time series
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) NUM_RESUSPENSION_TS

        write(unit = *, fmt = *) &
            'Number of resuspension time series : ', NUM_RESUSPENSION_TS

        allocate(RESUSPENSION_TS_FILE_NAMES(NUM_RESUSPENSION_TS))
        allocate(RESUSPENSION_TS           (NUM_RESUSPENSION_TS))
        allocate(ACTIVATE_RESUSPENSIONS    (nkn))
        allocate(SHEAR_STRESS_TS_NOS       (nkn))
        allocate(SHEAR_STRESS_TS_VAR_NOS   (nkn))

        ! Read the resuspension time serie file names
        read(unit = IN_FILE, fmt = *)

        do RESUSPENSION_TS_NO = 1, NUM_RESUSPENSION_TS
            read(unit = IN_FILE, fmt = *) AUX_INTEGER_1, AUX_STRING
            RESUSPENSION_TS_FILE_NAMES(AUX_INTEGER_1) = AUX_STRING

            open(unit   = IN_FILE + 1, &
                 file   = trim(adjustl(RESUSPENSION_INPUT_FOLDER)) // &
                          trim(adjustl(RESUSPENSION_TS_FILE_NAMES(AUX_INTEGER_1))), &
                 status = 'OLD')

            call INITIALIZE_TIME_SERIE    (RESUSPENSION_TS(RESUSPENSION_TS_NO))
            call READ_TIME_SERIE_FROM_FILE(RESUSPENSION_TS(RESUSPENSION_TS_NO), IN_FILE+1)
            close(IN_FILE + 1)
        end do

        ! Read the pelegic box options for resuspension
        read(unit = IN_FILE, fmt = *)

        do BOX_NO = 1,nkn
            read(unit = IN_FILE, fmt = *) &
                 AUX_INTEGER_1, AUX_INTEGER_2, AUX_INTEGER_3, AUX_INTEGER_4

            if (AUX_INTEGER_2 > 1) then
                AUX_INTEGER_2 = 1
            end if

            if (AUX_INTEGER_2 < 0) then
                AUX_INTEGER_2 = 0
            end if

            ACTIVATE_RESUSPENSIONS (AUX_INTEGER_1) = AUX_INTEGER_2
            SHEAR_STRESS_TS_NOS    (AUX_INTEGER_1) = AUX_INTEGER_3
            SHEAR_STRESS_TS_VAR_NOS(AUX_INTEGER_1) = AUX_INTEGER_4
        end do

        ! Read the critical shear stress filename
        if (CRIT_SHEAR_FNAME_FROM_OUTSIDE < 1) then
            read(unit = IN_FILE, fmt = *)
            read(unit = IN_FILE, fmt = *) CRITICAL_SHEAR_STRESS_FILENAME

            write(unit = *, fmt = *) &
                'Critical shear stress filename : ', &
                trim(adjustl(CRITICAL_SHEAR_STRESS_FILENAME))
        end if

        open(unit   = IN_FILE + 1, &
             file   = trim(adjustl(RESUSPENSION_INPUT_FOLDER)) // &
                      trim(adjustl(CRITICAL_SHEAR_STRESS_FILENAME)), &
             status = 'OLD')

        do i = 1, nkn
            read(unit = IN_FILE + 1, fmt = *) AUX_INTEGER_1, AUX_DBL_1
            BOX_CRITICAL_SHEAR_STRESSES(AUX_INTEGER_1) = AUX_DBL_1
        end do

    end subroutine READ_RESUSPENSION_FILE_OPTION_2

end module RESUSPENSION
