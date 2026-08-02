module UTILS_1
    use GLOBAL
    implicit none

contains

    subroutine READ_MODEL_CONSTANTS(MODEL_CONSTANTS, INPUT_NO)

        use iso_fortran_env, only: error_unit
        implicit none
        real(kind = DBL), dimension(:), intent(out) :: MODEL_CONSTANTS

        integer :: INPUT_NO

        integer :: CONSTANT_NO
        character(len = 100) :: CONSTANT_NAME
        real(kind = DBL) :: CONSTANT_VALUE

        ! Fail-loud on an incomplete/invalid constants file (positional, name-blind reader shared by the
        ! pelagic WCONST and the sediment W_SED_CONST). n = expected count = size of the passed slice.
        ! Byte-identical for an index-complete file (every slot overwritten -> zero-init irrelevant,
        ! n_bad = 0 -> no diagnostics, no stop). AQUABC_LENIENT_CONSTANTS=1 restores warn-and-continue.
        integer :: n, ios, i, n_bad
        logical, allocatable :: seen(:)
        logical :: lenient
        character(len = 32) :: env

        n = size(MODEL_CONSTANTS)
        allocate(seen(n))
        seen = .false.
        MODEL_CONSTANTS = 0.0_DBL       ! dropped slots -> deterministic 0 (used only in lenient mode)
        n_bad = 0

        do
            read(unit = INPUT_NO, fmt = *, iostat = ios) &
                 CONSTANT_NO, CONSTANT_NAME, CONSTANT_VALUE
            if (ios < 0) exit                                     ! EOF
            if (ios > 0) then                                     ! malformed line (conversion error)
                write(error_unit, *) 'READ_MODEL_CONSTANTS: malformed line in constants file'
                n_bad = n_bad + 1
                exit
            end if
            if (CONSTANT_NO < 1 .or. CONSTANT_NO > n) then        ! would corrupt memory
                write(error_unit, *) 'READ_MODEL_CONSTANTS: index out of range [1,', n, ']: ', CONSTANT_NO
                n_bad = n_bad + 1
                cycle
            end if
            if (seen(CONSTANT_NO)) then
                write(error_unit, *) 'READ_MODEL_CONSTANTS: duplicate index: ', CONSTANT_NO
                n_bad = n_bad + 1
            end if
            MODEL_CONSTANTS(CONSTANT_NO) = CONSTANT_VALUE
            seen(CONSTANT_NO) = .true.
        end do

        do i = 1, n
            if (.not. seen(i)) then
                write(error_unit, *) 'READ_MODEL_CONSTANTS: constant #', i, ' MISSING from file (defaulted to 0)'
                n_bad = n_bad + 1
            end if
        end do

        deallocate(seen)

        call get_environment_variable('AQUABC_LENIENT_CONSTANTS', env)
        lenient = (trim(adjustl(env)) == '1')
        if (n_bad > 0 .and. .not. lenient) then
            error stop 'READ_MODEL_CONSTANTS: incomplete/invalid constants file (see stderr above)'
        end if

    end subroutine READ_MODEL_CONSTANTS


    subroutine READ_MODEL_CONSTANTS_PRICE(MODEL_CONSTANTS, INPUT_NO)

        implicit none
        real(kind = DBL), dimension(:), intent(out) :: MODEL_CONSTANTS

        integer :: INPUT_NO
        integer :: NUM_CONSTANTS
        integer :: i

        read(unit = INPUT_NO, fmt = *) NUM_CONSTANTS

        do i = 1, NUM_CONSTANTS
            read(unit = INPUT_NO, fmt = *) MODEL_CONSTANTS(i)
        end do

    end subroutine READ_MODEL_CONSTANTS_PRICE


    integer function STRANGER(SUSPECTED_VALUE)
        use, intrinsic :: IEEE_ARITHMETIC
        implicit none
        ! Checks for NaN and Inf
        real(kind = DBL), intent(in) :: SUSPECTED_VALUE

        STRANGER = 0
        if (IEEE_IS_NAN(SUSPECTED_VALUE) .or. .not. IEEE_IS_FINITE(SUSPECTED_VALUE)) then
            STRANGER = 1
        end if
    end function STRANGER


    integer function STRANGERSD(SUSPECTED_VALUE)
        use, intrinsic :: IEEE_ARITHMETIC
        implicit none
        ! Checks for NaN and Inf
        ! Input is real(kind = DBL)
        real(kind = DBL), intent(in) :: SUSPECTED_VALUE

        STRANGERSD = 0
        if (IEEE_IS_NAN(SUSPECTED_VALUE) .or. .not. IEEE_IS_FINITE(SUSPECTED_VALUE)) then
            STRANGERSD = 1
        end if
    end function STRANGERSD


    character function INTEGER_TO_STRING(I_VALUE)
        implicit none
        integer, intent(in) :: I_VALUE
        character(len = 2048) :: INTERNAL_FILE
        write(INTERNAL_FILE, *) I_VALUE
        INTEGER_TO_STRING = trim(adjustl(INTERNAL_FILE))
    end function INTEGER_TO_STRING


    integer function CALCULATE_DAY_OF_YEAR(DAY_OF_SIMULATION, BASE_YEAR) result (DAY_OF_YEAR)
        implicit none

        double precision, intent(in) :: DAY_OF_SIMULATION
        integer         , intent(in) :: BASE_YEAR

        integer :: YEAR
        integer :: NUM_DAYS_IN_BASE_YEAR
        integer :: NUM_DAYS_IN_YEAR

        integer :: SIM_DAY

        SIM_DAY               = int(DAY_OF_SIMULATION)
        NUM_DAYS_IN_BASE_YEAR = CALCULATE_NUM_DAYS_IN_YEAR(BASE_YEAR)

        if (SIM_DAY .le. NUM_DAYS_IN_BASE_YEAR) then
            DAY_OF_YEAR = int(DAY_OF_SIMULATION)
        else
            YEAR    = BASE_YEAR + 1
            SIM_DAY = SIM_DAY - NUM_DAYS_IN_BASE_YEAR
            NUM_DAYS_IN_YEAR = CALCULATE_NUM_DAYS_IN_YEAR(YEAR)

            do while (SIM_DAY .gt. NUM_DAYS_IN_YEAR)
                NUM_DAYS_IN_YEAR = CALCULATE_NUM_DAYS_IN_YEAR(YEAR)
                SIM_DAY          = SIM_DAY - NUM_DAYS_IN_YEAR
                YEAR             = YEAR + 1
            end do
        end if

        DAY_OF_YEAR = SIM_DAY
    end function CALCULATE_DAY_OF_YEAR


    integer function CALCULATE_NUM_DAYS_IN_YEAR(YEAR) result(NUM_DAYS_IN_YEAR)
        implicit none
        integer, intent(in) :: YEAR

        if ((mod(YEAR, 4) == 0 .and. mod(YEAR, 100) /= 0) .or. &
            (mod(YEAR, 400) == 0)) then
            NUM_DAYS_IN_YEAR = 366
        else
            NUM_DAYS_IN_YEAR = 365
        end if

    end function CALCULATE_NUM_DAYS_IN_YEAR

end module UTILS_1
