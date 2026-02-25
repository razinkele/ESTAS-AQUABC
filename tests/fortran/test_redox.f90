! Unit tests for REDOX_AND_SPECIATION subroutine
program test_redox
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "REDOX_AND_SPECIATION Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_oxic_conditions()
    call test_anoxic_high_no3()
    call test_pe_oxic_vs_anoxic()
    call test_dissolved_metals_non_negative()
    call test_no_nan_extreme_ph()
    call test_redox_lim_range()

    print *, ""
    print *, "=========================================="
    print *, "Test Summary"
    print *, "=========================================="
    print '(A,I3,A)', " Passed: ", passed, " tests"
    print '(A,I3,A)', " Failed: ", failed, " tests"
    print *, "=========================================="

    if (failed > 0) then
        print *, "SOME TESTS FAILED!"
        stop 1
    else
        print *, "ALL TESTS PASSED!"
    end if

contains

    subroutine assert_true(cond, test_name)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: test_name
        if (cond) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            failed = failed + 1
        end if
    end subroutine assert_true

    subroutine assert_not_nan(value, test_name)
        real(kind=DBL_PREC), intent(in) :: value
        character(len=*), intent(in) :: test_name
        if (value == value) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A)', "    Value was NaN"
            failed = failed + 1
        end if
    end subroutine assert_not_nan

    ! Helper: allocate and set up redox state/lim arrays, then call subroutine
    subroutine run_redox(nkn, TEMP, SALT, PH, ELEVATION, &
                         DOXY_val, NO3N_val, MN_IV_val, FE_III_val, &
                         S_PLUS_6_val, DOC_val, S_MINUS_2_val, &
                         MN_II_val, FE_II_val, HCO3_val, CO3_val, &
                         PE, FE_II_DISS, FE_III_DISS, MN_II_DISS, &
                         LIM_DOXY_RED, LIM_NO3N_RED)
        integer, intent(in) :: nkn
        real(kind=DBL_PREC), intent(in) :: TEMP(nkn), SALT(nkn), PH(nkn), ELEVATION(nkn)
        real(kind=DBL_PREC), intent(in) :: DOXY_val, NO3N_val, MN_IV_val, FE_III_val
        real(kind=DBL_PREC), intent(in) :: S_PLUS_6_val, DOC_val, S_MINUS_2_val
        real(kind=DBL_PREC), intent(in) :: MN_II_val, FE_II_val, HCO3_val, CO3_val
        real(kind=DBL_PREC), intent(out) :: PE(nkn)
        real(kind=DBL_PREC), intent(out) :: FE_II_DISS(nkn), FE_III_DISS(nkn)
        real(kind=DBL_PREC), intent(out) :: MN_II_DISS(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_DOXY_RED(nkn), LIM_NO3N_RED(nkn)

        type(t_redox_params) :: redox_params
        type(t_redox_state) :: redox_state
        type(t_redox_lim) :: redox_lim

        ! Target arrays for pointer assignment
        real(kind=DBL_PREC), target :: arr_DOXY(nkn), arr_NO3N(nkn)
        real(kind=DBL_PREC), target :: arr_MN_IV(nkn), arr_FE_III(nkn)
        real(kind=DBL_PREC), target :: arr_S_PLUS_6(nkn), arr_DOC(nkn)
        real(kind=DBL_PREC), target :: arr_S_MINUS_2(nkn), arr_MN_II(nkn)
        real(kind=DBL_PREC), target :: arr_FE_II(nkn), arr_HCO3(nkn), arr_CO3(nkn)
        real(kind=DBL_PREC), target :: arr_LIM_DOXY(nkn), arr_LIM_NO3N(nkn)
        real(kind=DBL_PREC), target :: arr_LIM_MN_IV(nkn), arr_LIM_FE_III(nkn)
        real(kind=DBL_PREC), target :: arr_LIM_S_PLUS_6(nkn), arr_LIM_DOC(nkn)

        call set_default_redox_params(redox_params)

        ! Populate state arrays
        arr_DOXY = DOXY_val; arr_NO3N = NO3N_val; arr_MN_IV = MN_IV_val
        arr_FE_III = FE_III_val; arr_S_PLUS_6 = S_PLUS_6_val; arr_DOC = DOC_val
        arr_S_MINUS_2 = S_MINUS_2_val; arr_MN_II = MN_II_val
        arr_FE_II = FE_II_val; arr_HCO3 = HCO3_val; arr_CO3 = CO3_val

        redox_state%DOXY       => arr_DOXY
        redox_state%NO3N       => arr_NO3N
        redox_state%MN_IV      => arr_MN_IV
        redox_state%FE_III     => arr_FE_III
        redox_state%S_PLUS_6   => arr_S_PLUS_6
        redox_state%DISS_ORG_C => arr_DOC
        redox_state%S_MINUS_2  => arr_S_MINUS_2
        redox_state%MN_II      => arr_MN_II
        redox_state%FE_II      => arr_FE_II
        redox_state%HCO3       => arr_HCO3
        redox_state%CO3        => arr_CO3

        ! Populate lim arrays (outputs)
        arr_LIM_DOXY = 0.0D0; arr_LIM_NO3N = 0.0D0
        arr_LIM_MN_IV = 0.0D0; arr_LIM_FE_III = 0.0D0
        arr_LIM_S_PLUS_6 = 0.0D0; arr_LIM_DOC = 0.0D0

        redox_lim%LIM_DOXY_RED     => arr_LIM_DOXY
        redox_lim%LIM_NO3N_RED     => arr_LIM_NO3N
        redox_lim%LIM_MN_IV_RED    => arr_LIM_MN_IV
        redox_lim%LIM_FE_III_RED   => arr_LIM_FE_III
        redox_lim%LIM_S_PLUS_6_RED => arr_LIM_S_PLUS_6
        redox_lim%LIM_DOC_RED      => arr_LIM_DOC

        PE = 0.0D0; FE_II_DISS = 0.0D0; FE_III_DISS = 0.0D0; MN_II_DISS = 0.0D0

        call REDOX_AND_SPECIATION(nkn, TEMP, SALT, PH, ELEVATION, &
                                  redox_params, redox_state, redox_lim, &
                                  PE, FE_II_DISS, FE_III_DISS, MN_II_DISS)

        LIM_DOXY_RED = arr_LIM_DOXY
        LIM_NO3N_RED = arr_LIM_NO3N
    end subroutine run_redox

    ! Oxic conditions: high DO -> LIM_DOXY_RED near 1.0
    subroutine test_oxic_conditions()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: TEMP(nkn), SALT(nkn), PH(nkn), ELEV(nkn)
        real(kind=DBL_PREC) :: PE(nkn), FE_II_D(nkn), FE_III_D(nkn), MN_II_D(nkn)
        real(kind=DBL_PREC) :: LIM_DOXY(nkn), LIM_NO3N(nkn)

        print *, "Test: Oxic conditions (high DO)"

        TEMP = 20.0D0; SALT = 0.5D0; PH = 7.5D0; ELEV = 0.0D0

        call run_redox(nkn, TEMP, SALT, PH, ELEV, &
                       8.0D0, 1.0D0, 0.5D0, 0.5D0, &    ! DOXY=8, NO3N=1, MN_IV=0.5, FE_III=0.5
                       1.0D0, 5.0D0, 0.01D0, &            ! S_PLUS_6=1, DOC=5, S_MINUS_2=0.01
                       0.1D0, 0.1D0, 50.0D0, 1.0D0, &    ! MN_II=0.1, FE_II=0.1, HCO3=50, CO3=1
                       PE, FE_II_D, FE_III_D, MN_II_D, LIM_DOXY, LIM_NO3N)

        call assert_true(LIM_DOXY(1) > 0.8D0, &
                         "LIM_DOXY_RED > 0.8 under oxic conditions")
    end subroutine test_oxic_conditions

    ! Anoxic with high NO3: LIM_NO3N_RED > 0
    subroutine test_anoxic_high_no3()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: TEMP(nkn), SALT(nkn), PH(nkn), ELEV(nkn)
        real(kind=DBL_PREC) :: PE(nkn), FE_II_D(nkn), FE_III_D(nkn), MN_II_D(nkn)
        real(kind=DBL_PREC) :: LIM_DOXY(nkn), LIM_NO3N(nkn)

        print *, "Test: Anoxic + high NO3"

        TEMP = 20.0D0; SALT = 0.5D0; PH = 7.5D0; ELEV = 0.0D0

        call run_redox(nkn, TEMP, SALT, PH, ELEV, &
                       0.01D0, 5.0D0, 0.5D0, 0.5D0, &   ! DOXY=0.01 (anoxic), NO3N=5
                       1.0D0, 5.0D0, 0.01D0, &
                       0.1D0, 0.1D0, 50.0D0, 1.0D0, &
                       PE, FE_II_D, FE_III_D, MN_II_D, LIM_DOXY, LIM_NO3N)

        call assert_true(LIM_NO3N(1) > 0.0D0, &
                         "LIM_NO3N_RED > 0 under anoxic + high NO3")
    end subroutine test_anoxic_high_no3

    ! PE: oxic > anoxic
    subroutine test_pe_oxic_vs_anoxic()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: TEMP(nkn), SALT(nkn), PH(nkn), ELEV(nkn)
        real(kind=DBL_PREC) :: PE_oxic(nkn), PE_anox(nkn)
        real(kind=DBL_PREC) :: FE_II_D(nkn), FE_III_D(nkn), MN_II_D(nkn)
        real(kind=DBL_PREC) :: LIM_DOXY(nkn), LIM_NO3N(nkn)

        print *, "Test: PE higher under oxic than anoxic"

        TEMP = 20.0D0; SALT = 0.5D0; PH = 7.5D0; ELEV = 0.0D0

        ! Oxic
        call run_redox(nkn, TEMP, SALT, PH, ELEV, &
                       8.0D0, 1.0D0, 0.5D0, 0.5D0, &
                       1.0D0, 5.0D0, 0.01D0, &
                       0.1D0, 0.1D0, 50.0D0, 1.0D0, &
                       PE_oxic, FE_II_D, FE_III_D, MN_II_D, LIM_DOXY, LIM_NO3N)

        ! Anoxic
        call run_redox(nkn, TEMP, SALT, PH, ELEV, &
                       0.01D0, 0.01D0, 0.01D0, 0.01D0, &
                       0.01D0, 5.0D0, 1.0D0, &
                       1.0D0, 1.0D0, 50.0D0, 1.0D0, &
                       PE_anox, FE_II_D, FE_III_D, MN_II_D, LIM_DOXY, LIM_NO3N)

        call assert_true(PE_oxic(1) > PE_anox(1), &
                         "PE is higher under oxic than anoxic conditions")
    end subroutine test_pe_oxic_vs_anoxic

    ! Dissolved metals non-negative
    subroutine test_dissolved_metals_non_negative()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: TEMP(nkn), SALT(nkn), PH(nkn), ELEV(nkn)
        real(kind=DBL_PREC) :: PE(nkn), FE_II_D(nkn), FE_III_D(nkn), MN_II_D(nkn)
        real(kind=DBL_PREC) :: LIM_DOXY(nkn), LIM_NO3N(nkn)

        print *, "Test: Dissolved metals are non-negative"

        TEMP = 20.0D0; SALT = 0.5D0; PH = 7.5D0; ELEV = 0.0D0

        call run_redox(nkn, TEMP, SALT, PH, ELEV, &
                       4.0D0, 1.0D0, 0.5D0, 0.5D0, &
                       1.0D0, 5.0D0, 0.1D0, &
                       0.5D0, 0.5D0, 50.0D0, 1.0D0, &
                       PE, FE_II_D, FE_III_D, MN_II_D, LIM_DOXY, LIM_NO3N)

        call assert_true(FE_II_D(1) >= 0.0D0, "FE_II_DISS >= 0")
        call assert_true(FE_III_D(1) >= 0.0D0, "FE_III_DISS >= 0")
        call assert_true(MN_II_D(1) >= 0.0D0, "MN_II_DISS >= 0")
    end subroutine test_dissolved_metals_non_negative

    ! No NaN with extreme pH values
    subroutine test_no_nan_extreme_ph()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: TEMP(nkn), SALT(nkn), PH_low(nkn), PH_high(nkn), ELEV(nkn)
        real(kind=DBL_PREC) :: PE(nkn), FE_II_D(nkn), FE_III_D(nkn), MN_II_D(nkn)
        real(kind=DBL_PREC) :: LIM_DOXY(nkn), LIM_NO3N(nkn)

        print *, "Test: No NaN with extreme pH values"

        TEMP = 20.0D0; SALT = 0.5D0; ELEV = 0.0D0

        ! Low pH
        PH_low = 5.0D0
        call run_redox(nkn, TEMP, SALT, PH_low, ELEV, &
                       4.0D0, 1.0D0, 0.5D0, 0.5D0, &
                       1.0D0, 5.0D0, 0.1D0, &
                       0.5D0, 0.5D0, 50.0D0, 1.0D0, &
                       PE, FE_II_D, FE_III_D, MN_II_D, LIM_DOXY, LIM_NO3N)

        call assert_not_nan(PE(1), "PE not NaN at pH=5")

        ! High pH
        PH_high = 10.0D0
        call run_redox(nkn, TEMP, SALT, PH_high, ELEV, &
                       4.0D0, 1.0D0, 0.5D0, 0.5D0, &
                       1.0D0, 5.0D0, 0.1D0, &
                       0.5D0, 0.5D0, 50.0D0, 1.0D0, &
                       PE, FE_II_D, FE_III_D, MN_II_D, LIM_DOXY, LIM_NO3N)

        call assert_not_nan(PE(1), "PE not NaN at pH=10")
    end subroutine test_no_nan_extreme_ph

    ! All redox limitation factors in [0,1] range
    subroutine test_redox_lim_range()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: TEMP(nkn), SALT(nkn), PH(nkn), ELEV(nkn)
        real(kind=DBL_PREC) :: PE(nkn), FE_II_D(nkn), FE_III_D(nkn), MN_II_D(nkn)
        real(kind=DBL_PREC) :: LIM_DOXY(nkn), LIM_NO3N(nkn)

        print *, "Test: Redox limitation factors in [0,1]"

        TEMP = 20.0D0; SALT = 0.5D0; PH = 7.5D0; ELEV = 0.0D0

        call run_redox(nkn, TEMP, SALT, PH, ELEV, &
                       4.0D0, 1.0D0, 0.5D0, 0.5D0, &
                       1.0D0, 5.0D0, 0.1D0, &
                       0.5D0, 0.5D0, 50.0D0, 1.0D0, &
                       PE, FE_II_D, FE_III_D, MN_II_D, LIM_DOXY, LIM_NO3N)

        call assert_true(LIM_DOXY(1) >= 0.0D0 .and. LIM_DOXY(1) <= 1.0D0, &
                         "LIM_DOXY_RED in [0,1]")
        call assert_true(LIM_NO3N(1) >= 0.0D0 .and. LIM_NO3N(1) <= 1.0D0, &
                         "LIM_NO3N_RED in [0,1]")
    end subroutine test_redox_lim_range

end program test_redox
