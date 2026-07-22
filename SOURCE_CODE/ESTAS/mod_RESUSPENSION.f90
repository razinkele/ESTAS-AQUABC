module RESUSPENSION
    use GLOBAL

    implicit none

    ! Sediment-resuspension / shear-stress state, encapsulated out of the GLOBAL
    ! god-module. Behaviour-preserving move — see
    ! docs/superpowers/specs/2026-07-22-resuspension-state-derived-type-design.md.
    ! TIME_SERIE and DBL are in scope via `use GLOBAL`.
    type, public :: resuspension_t
        integer :: NUM_RESUSPENSION_TS
        integer :: RESUSPENSION_OPTION
        integer :: CONSIDER_RESUSPENSION
        integer,               allocatable, dimension(:)    :: ACTIVATE_RESUSPENSIONS
        real(kind = DBL),      allocatable, dimension(:)    :: FRAC_RESUSPENSION_AREAS
        integer,               allocatable, dimension(:, :) :: RESUSPENSION_CONC_TS_NOS
        integer,               allocatable, dimension(:, :) :: RESUSPENSION_CONC_TS_VAR_NOS
        integer,               allocatable, dimension(:)    :: RESUSPENSION_VEL_TS_NOS
        integer,               allocatable, dimension(:)    :: RESUSPENSION_VEL_TS_VAR_NOS
        character(len = 2048)                               :: RESUSPENSION_INPUT_FILE_NAME
        character(len = 2048), allocatable, dimension(:)    :: RESUSPENSION_TS_FILE_NAMES
        type(TIME_SERIE),      allocatable, dimension(:)    :: RESUSPENSION_TS
        character(len = 2048)                               :: RESUSPENSION_INPUT_FOLDER
        character(len = 2048)                               :: RESUSPENSION_OUTPUT_FOLDER
        real(kind = DBL),      allocatable, dimension(:)    :: BOX_CRITICAL_SHEAR_STRESSES
        character(len = 2048)                               :: CRITICAL_SHEAR_STRESS_FILENAME
        integer                                             :: CRIT_SHEAR_FNAME_FROM_OUTSIDE
        integer,               allocatable, dimension(:)    :: SHEAR_STRESS_TS_NOS
        integer,               allocatable, dimension(:)    :: SHEAR_STRESS_TS_VAR_NOS
    end type resuspension_t

    type(resuspension_t), public :: resusp

contains

    subroutine READ_RESUSPENSION_FILE_OPTION_1(IN_FILE)
        implicit none
        integer, intent(in) :: IN_FILE

        integer :: BOX_NO, STATE_VAR_NO, RESUSPENSION_TS_NO
        integer :: AUX_INTEGER_1, AUX_INTEGER_2, AUX_INTEGER_3, AUX_INTEGER_4
        real(kind = DBL) :: AUX_DBL_1
        integer :: SUM_ACTIVE_BOXES

        character(len = 2048) :: AUX_STRING

        allocate(resusp%ACTIVATE_RESUSPENSIONS      (nkn))
        allocate(resusp%RESUSPENSION_VEL_TS_NOS     (nkn))
        allocate(resusp%RESUSPENSION_VEL_TS_VAR_NOS (nkn))
        allocate(resusp%RESUSPENSION_CONC_TS_NOS    (nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(resusp%RESUSPENSION_CONC_TS_VAR_NOS(nkn, (nstate + NUM_ALLOLOPATHY_STATE_VARS)))
        allocate(resusp%FRAC_RESUSPENSION_AREAS     (nkn))

        SUM_ACTIVE_BOXES = 0

        ! Read the info lines
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)

        ! Read the number of resuspension time series
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) resusp%NUM_RESUSPENSION_TS
        allocate(resusp%RESUSPENSION_TS_FILE_NAMES(resusp%NUM_RESUSPENSION_TS))
        allocate(resusp%RESUSPENSION_TS           (resusp%NUM_RESUSPENSION_TS))

        ! Read the resuspension file names
        read(unit = IN_FILE, fmt = *)

        do RESUSPENSION_TS_NO = 1, resusp%NUM_RESUSPENSION_TS
            read(unit = IN_FILE, fmt = *) AUX_INTEGER_1, AUX_STRING
            resusp%RESUSPENSION_TS_FILE_NAMES(AUX_INTEGER_1) = AUX_STRING

            call OPEN_INPUT_FILE(IN_FILE + 1, &
                 trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER)) // &
                 trim(adjustl(resusp%RESUSPENSION_TS_FILE_NAMES(AUX_INTEGER_1))), &
                 'resuspension input')

            call INITIALIZE_TIME_SERIE    (resusp%RESUSPENSION_TS(RESUSPENSION_TS_NO))
            call READ_TIME_SERIE_FROM_FILE(resusp%RESUSPENSION_TS(RESUSPENSION_TS_NO), IN_FILE+1)
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

            resusp%ACTIVATE_RESUSPENSIONS     (AUX_INTEGER_1) = AUX_INTEGER_2
            resusp%FRAC_RESUSPENSION_AREAS    (AUX_INTEGER_1) = AUX_DBL_1
            resusp%RESUSPENSION_VEL_TS_NOS    (AUX_INTEGER_1) = AUX_INTEGER_3
            resusp%RESUSPENSION_VEL_TS_VAR_NOS(AUX_INTEGER_1) = AUX_INTEGER_4

            SUM_ACTIVE_BOXES = SUM_ACTIVE_BOXES + AUX_INTEGER_2
        end do

        ! Read the resuspension velocity time series pelagic boxes
        read(unit = IN_FILE, fmt = *)

        do BOX_NO = 1, SUM_ACTIVE_BOXES
            do STATE_VAR_NO = 1, nstate

                read(unit = IN_FILE, fmt = *) &
                     AUX_INTEGER_1, AUX_INTEGER_2, AUX_INTEGER_3, AUX_INTEGER_4

                resusp%RESUSPENSION_CONC_TS_NOS    (AUX_INTEGER_1, AUX_INTEGER_2) = AUX_INTEGER_3
                resusp%RESUSPENSION_CONC_TS_VAR_NOS(AUX_INTEGER_1, AUX_INTEGER_2) = AUX_INTEGER_4
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

        allocate (resusp%BOX_CRITICAL_SHEAR_STRESSES(nkn))

        ! Read the info lines
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *)

        ! Read the number of resuspension time series
        read(unit = IN_FILE, fmt = *)
        read(unit = IN_FILE, fmt = *) resusp%NUM_RESUSPENSION_TS

        write(unit = *, fmt = *) &
            'Number of resuspension time series : ', resusp%NUM_RESUSPENSION_TS

        allocate(resusp%RESUSPENSION_TS_FILE_NAMES(resusp%NUM_RESUSPENSION_TS))
        allocate(resusp%RESUSPENSION_TS           (resusp%NUM_RESUSPENSION_TS))
        allocate(resusp%ACTIVATE_RESUSPENSIONS    (nkn))
        allocate(resusp%SHEAR_STRESS_TS_NOS       (nkn))
        allocate(resusp%SHEAR_STRESS_TS_VAR_NOS   (nkn))

        ! Read the resuspension time serie file names
        read(unit = IN_FILE, fmt = *)

        do RESUSPENSION_TS_NO = 1, resusp%NUM_RESUSPENSION_TS
            read(unit = IN_FILE, fmt = *) AUX_INTEGER_1, AUX_STRING
            resusp%RESUSPENSION_TS_FILE_NAMES(AUX_INTEGER_1) = AUX_STRING

            call OPEN_INPUT_FILE(IN_FILE + 1, &
                 trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER)) // &
                 trim(adjustl(resusp%RESUSPENSION_TS_FILE_NAMES(AUX_INTEGER_1))), &
                 'resuspension input')

            call INITIALIZE_TIME_SERIE    (resusp%RESUSPENSION_TS(RESUSPENSION_TS_NO))
            call READ_TIME_SERIE_FROM_FILE(resusp%RESUSPENSION_TS(RESUSPENSION_TS_NO), IN_FILE+1)
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

            resusp%ACTIVATE_RESUSPENSIONS (AUX_INTEGER_1) = AUX_INTEGER_2
            resusp%SHEAR_STRESS_TS_NOS    (AUX_INTEGER_1) = AUX_INTEGER_3
            resusp%SHEAR_STRESS_TS_VAR_NOS(AUX_INTEGER_1) = AUX_INTEGER_4
        end do

        ! Read the critical shear stress filename
        if (resusp%CRIT_SHEAR_FNAME_FROM_OUTSIDE < 1) then
            read(unit = IN_FILE, fmt = *)
            read(unit = IN_FILE, fmt = *) resusp%CRITICAL_SHEAR_STRESS_FILENAME

            write(unit = *, fmt = *) &
                'Critical shear stress filename : ', &
                trim(adjustl(resusp%CRITICAL_SHEAR_STRESS_FILENAME))
        end if

        call OPEN_INPUT_FILE(IN_FILE + 1, &
             trim(adjustl(resusp%RESUSPENSION_INPUT_FOLDER)) // trim(adjustl(resusp%CRITICAL_SHEAR_STRESS_FILENAME)), &
             'resuspension input')

        do i = 1, nkn
            read(unit = IN_FILE + 1, fmt = *) AUX_INTEGER_1, AUX_DBL_1
            resusp%BOX_CRITICAL_SHEAR_STRESSES(AUX_INTEGER_1) = AUX_DBL_1
        end do

    end subroutine READ_RESUSPENSION_FILE_OPTION_2

end module RESUSPENSION
