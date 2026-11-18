! Unit tests for GROWTH_AT_TEMP subroutine
! Tests temperature limitation factor calculations for phytoplankton growth
!
! This is a core function used by DIATOMS, CYANOBACTERIA, and other algae modules
! The function returns 1.0 when temperature is in optimal range,
! and decreases exponentially outside that range

program test_growth_temp
    use AQUABC_II_GLOBAL, only: USE_CTMI_TEMP
    implicit none
    
    integer, parameter :: DBL_PREC = selected_real_kind(15, 307)
    
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "GROWTH_AT_TEMP Unit Tests"
    print *, "=========================================="
    print *, ""
    
    ! Run all tests
    call test_optimal_range()
    call test_below_optimal()
    call test_above_optimal()
    call test_at_boundaries()
    call test_extreme_cold()
    call test_extreme_hot()
    call test_vector_operation()
    call test_kappa_sensitivity()
    call test_plateau_is_the_default()
    call test_ctmi_constants_under_plateau()
    call test_ctmi_model()
    call test_ctmi_falls_back_on_invalid_constants()
    
    ! Print summary
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

    ! GROWTH_AT_TEMP: the production routine (aquabc_II_pelagic_auxillary.f90) is linked.

    subroutine assert_approx_equal(value, expected, tol, test_name)
        real(kind=DBL_PREC), intent(in) :: value, expected, tol
        character(len=*), intent(in) :: test_name
        
        if (abs(value - expected) <= tol) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,F10.6,A,F10.6)', "    Got: ", value, " Expected: ", expected
            failed = failed + 1
        end if
    end subroutine assert_approx_equal
    
    subroutine assert_less_than(value1, value2, test_name)
        real(kind=DBL_PREC), intent(in) :: value1, value2
        character(len=*), intent(in) :: test_name
        
        if (value1 < value2) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,F10.6,A,F10.6)', "    Expected ", value1, " < ", value2
            failed = failed + 1
        end if
    end subroutine assert_less_than
    
    subroutine assert_in_range(value, min_val, max_val, test_name)
        real(kind=DBL_PREC), intent(in) :: value, min_val, max_val
        character(len=*), intent(in) :: test_name
        
        if (value >= min_val .and. value <= max_val) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,F10.6,A,F10.6,A,F10.6)', "    Value: ", value, " not in [", min_val, ", ", max_val, "]"
            failed = failed + 1
        end if
    end subroutine assert_in_range

    ! Test when temperature is within optimal range
    subroutine test_optimal_range()
        integer, parameter :: nkn = 3
        double precision, dimension(nkn) :: TEMP, LIM_TEMP_GROWTH
        double precision :: Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP
        double precision :: KAPPA_UNDER, KAPPA_OVER
        
        print *, "Test: Temperature in optimal range gives LIM = 1.0"
        
        ! Typical optimal range for diatoms: 15-25°C
        Lower_TEMP = 15.0D0
        Upper_TEMP = 25.0D0
        K_AT_OPT_TEMP = 2.0D0     ! Not used in current implementation
        KAPPA_UNDER = 0.05D0
        KAPPA_OVER = 0.1D0
        
        TEMP = (/ 16.0D0, 20.0D0, 24.0D0 /)   ! All within range
        LIM_TEMP_GROWTH = 0.0D0
        
        call GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, KAPPA_UNDER, KAPPA_OVER, nkn)
        
        call assert_approx_equal(LIM_TEMP_GROWTH(1), 1.0D0, 1.0D-10, &
            "16°C (in range 15-25) gives LIM = 1.0")
        call assert_approx_equal(LIM_TEMP_GROWTH(2), 1.0D0, 1.0D-10, &
            "20°C (in range 15-25) gives LIM = 1.0")
        call assert_approx_equal(LIM_TEMP_GROWTH(3), 1.0D0, 1.0D-10, &
            "24°C (in range 15-25) gives LIM = 1.0")
        
    end subroutine test_optimal_range
    
    ! Test below optimal temperature
    subroutine test_below_optimal()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: TEMP, LIM_TEMP_GROWTH
        double precision :: Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP
        double precision :: KAPPA_UNDER, KAPPA_OVER
        double precision :: expected
        
        print *, "Test: Temperature below optimal gives LIM < 1.0"
        
        Lower_TEMP = 15.0D0
        Upper_TEMP = 25.0D0
        K_AT_OPT_TEMP = 2.0D0
        KAPPA_UNDER = 0.1D0       ! Decay rate below optimal
        KAPPA_OVER = 0.1D0
        
        TEMP = (/ 10.0D0 /)       ! 5 degrees below optimal
        LIM_TEMP_GROWTH = 0.0D0
        
        call GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, KAPPA_UNDER, KAPPA_OVER, nkn)
        
        ! Expected: exp(-0.1 * |15-10|) = exp(-0.5) ≈ 0.6065
        expected = exp(-KAPPA_UNDER * 5.0D0)
        
        call assert_approx_equal(LIM_TEMP_GROWTH(1), expected, 1.0D-10, &
            "10°C gives expected exponential decay")
        call assert_less_than(LIM_TEMP_GROWTH(1), 1.0D0, &
            "10°C (below optimal 15) gives LIM < 1.0")
        
    end subroutine test_below_optimal
    
    ! Test above optimal temperature
    subroutine test_above_optimal()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: TEMP, LIM_TEMP_GROWTH
        double precision :: Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP
        double precision :: KAPPA_UNDER, KAPPA_OVER
        double precision :: expected
        
        print *, "Test: Temperature above optimal gives LIM < 1.0"
        
        Lower_TEMP = 15.0D0
        Upper_TEMP = 25.0D0
        K_AT_OPT_TEMP = 2.0D0
        KAPPA_UNDER = 0.1D0
        KAPPA_OVER = 0.15D0       ! Steeper decay above optimal
        
        TEMP = (/ 30.0D0 /)       ! 5 degrees above optimal
        LIM_TEMP_GROWTH = 0.0D0
        
        call GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, KAPPA_UNDER, KAPPA_OVER, nkn)
        
        ! Expected: exp(-0.15 * |25-30|) = exp(-0.75) ≈ 0.4724
        expected = exp(-KAPPA_OVER * 5.0D0)
        
        call assert_approx_equal(LIM_TEMP_GROWTH(1), expected, 1.0D-10, &
            "30°C gives expected exponential decay")
        call assert_less_than(LIM_TEMP_GROWTH(1), 1.0D0, &
            "30°C (above optimal 25) gives LIM < 1.0")
        
    end subroutine test_above_optimal
    
    ! Test at boundary temperatures
    subroutine test_at_boundaries()
        integer, parameter :: nkn = 2
        double precision, dimension(nkn) :: TEMP, LIM_TEMP_GROWTH
        double precision :: Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP
        double precision :: KAPPA_UNDER, KAPPA_OVER
        
        print *, "Test: Temperature at boundaries"
        
        Lower_TEMP = 15.0D0
        Upper_TEMP = 25.0D0
        K_AT_OPT_TEMP = 2.0D0
        KAPPA_UNDER = 0.1D0
        KAPPA_OVER = 0.1D0
        
        ! At lower boundary: uses the <= condition, so LIM = exp(-kappa*0) = 1.0
        ! At upper boundary: uses the >= condition, so LIM = exp(-kappa*0) = 1.0
        TEMP = (/ 15.0D0, 25.0D0 /)
        LIM_TEMP_GROWTH = 0.0D0
        
        call GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, KAPPA_UNDER, KAPPA_OVER, nkn)
        
        ! At exact boundaries, distance is 0, so exp(-k*0) = 1.0
        call assert_approx_equal(LIM_TEMP_GROWTH(1), 1.0D0, 1.0D-10, &
            "At lower boundary (15°C), LIM = 1.0")
        call assert_approx_equal(LIM_TEMP_GROWTH(2), 1.0D0, 1.0D-10, &
            "At upper boundary (25°C), LIM = 1.0")
        
    end subroutine test_at_boundaries
    
    ! Test extreme cold
    subroutine test_extreme_cold()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: TEMP, LIM_TEMP_GROWTH
        double precision :: Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP
        double precision :: KAPPA_UNDER, KAPPA_OVER
        
        print *, "Test: Extreme cold gives very low growth"
        
        Lower_TEMP = 15.0D0
        Upper_TEMP = 25.0D0
        K_AT_OPT_TEMP = 2.0D0
        KAPPA_UNDER = 0.1D0
        KAPPA_OVER = 0.1D0
        
        TEMP = (/ 0.0D0 /)        ! Near freezing
        LIM_TEMP_GROWTH = 0.0D0
        
        call GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, KAPPA_UNDER, KAPPA_OVER, nkn)
        
        ! exp(-0.1 * 15) = exp(-1.5) ≈ 0.223
        call assert_in_range(LIM_TEMP_GROWTH(1), 0.1D0, 0.3D0, &
            "0°C gives heavily reduced growth (0.1-0.3)")
        
    end subroutine test_extreme_cold
    
    ! Test extreme heat
    subroutine test_extreme_hot()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: TEMP, LIM_TEMP_GROWTH
        double precision :: Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP
        double precision :: KAPPA_UNDER, KAPPA_OVER
        
        print *, "Test: Extreme heat gives very low growth"
        
        Lower_TEMP = 15.0D0
        Upper_TEMP = 25.0D0
        K_AT_OPT_TEMP = 2.0D0
        KAPPA_UNDER = 0.1D0
        KAPPA_OVER = 0.2D0        ! Heat stress more severe
        
        TEMP = (/ 40.0D0 /)       ! Very hot water
        LIM_TEMP_GROWTH = 0.0D0
        
        call GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, KAPPA_UNDER, KAPPA_OVER, nkn)
        
        ! exp(-0.2 * 15) = exp(-3.0) ≈ 0.05
        call assert_in_range(LIM_TEMP_GROWTH(1), 0.01D0, 0.1D0, &
            "40°C gives severely reduced growth (0.01-0.1)")
        
    end subroutine test_extreme_hot
    
    ! Test vector operation
    subroutine test_vector_operation()
        integer, parameter :: nkn = 7
        double precision, dimension(nkn) :: TEMP, LIM_TEMP_GROWTH
        double precision :: Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP
        double precision :: KAPPA_UNDER, KAPPA_OVER
        integer :: i
        logical :: all_valid
        
        print *, "Test: Vector operation with gradient"
        
        Lower_TEMP = 15.0D0
        Upper_TEMP = 25.0D0
        K_AT_OPT_TEMP = 2.0D0
        KAPPA_UNDER = 0.1D0
        KAPPA_OVER = 0.1D0
        
        ! Temperature gradient from cold to hot
        TEMP = (/ 5.0D0, 10.0D0, 15.0D0, 20.0D0, 25.0D0, 30.0D0, 35.0D0 /)
        LIM_TEMP_GROWTH = 0.0D0
        
        call GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, KAPPA_UNDER, KAPPA_OVER, nkn)
        
        ! All values should be in [0, 1]
        all_valid = .true.
        do i = 1, nkn
            if (LIM_TEMP_GROWTH(i) < 0.0D0 .or. LIM_TEMP_GROWTH(i) > 1.0D0) then
                all_valid = .false.
            end if
        end do
        
        if (all_valid) then
            print '(A,A)', "  PASS: ", "All values in valid range [0, 1]"
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", "Some values outside [0, 1]"
            failed = failed + 1
        end if
        
        ! Values in optimal range (indices 3, 4, 5) should be 1.0
        call assert_approx_equal(LIM_TEMP_GROWTH(4), 1.0D0, 1.0D-10, &
            "20°C (middle of optimal) gives LIM = 1.0")
        
    end subroutine test_vector_operation
    
    ! Test sensitivity to kappa parameter
    subroutine test_kappa_sensitivity()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: TEMP, LIM_low, LIM_high
        double precision :: Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP
        
        print *, "Test: Higher kappa gives steeper decline"
        
        Lower_TEMP = 15.0D0
        Upper_TEMP = 25.0D0
        K_AT_OPT_TEMP = 2.0D0
        
        TEMP = (/ 10.0D0 /)
        LIM_low = 0.0D0
        LIM_high = 0.0D0
        
        ! Low kappa
        call GROWTH_AT_TEMP(TEMP, LIM_low, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, 0.05D0, 0.05D0, nkn)
        
        ! High kappa
        call GROWTH_AT_TEMP(TEMP, LIM_high, Lower_TEMP, Upper_TEMP, &
                           K_AT_OPT_TEMP, 0.2D0, 0.2D0, nkn)
        
        ! Higher kappa should give lower limitation factor
        call assert_less_than(LIM_high(1), LIM_low(1), &
            "Higher kappa gives steeper decline (lower LIM)")
        
    end subroutine test_kappa_sensitivity

    ! The production default is the plateau model
    subroutine test_plateau_is_the_default()
        double precision :: T(3), L(3)

        print *, "Test: plateau model is the default (USE_CTMI_TEMP = .false.)"
        if (USE_CTMI_TEMP) then
            print '(A)', "  FAIL: USE_CTMI_TEMP defaults to .true."
            failed = failed + 1
        else
            print '(A)', "  PASS: USE_CTMI_TEMP defaults to .false."
            passed = passed + 1
        end if
        T = [5.0D0, 15.0D0, 30.0D0]
        call GROWTH_AT_TEMP(T, L, 10.0D0, 20.0D0, 2.0D0, 0.08D0, 0.05D0, 3)
        call assert_approx_equal(L(1), exp(-0.08D0 * 5.0D0), 1.0D-12, "cold side: exp(-kappa_under * dT)")
        call assert_approx_equal(L(2), 1.0D0, 1.0D-12, "inside the plateau: 1")
        call assert_approx_equal(L(3), exp(-0.05D0 * 10.0D0), 1.0D-12, "warm side: exp(-kappa_over * dT)")
    end subroutine test_plateau_is_the_default

    ! CTMI-style constants (KAPPA_UNDER = 0, KAPPA_OVER = T_max) read by the plateau
    ! model: no cold limitation at all and a collapse just above the plateau. This is
    ! the mismatch the Standard constants file carried until 2026-11; the routine
    ! cannot detect it, so the test pins what it does (the constants validator and
    ! tests/python/test_audit_contracts.py guard the files).
    subroutine test_ctmi_constants_under_plateau()
        double precision :: T(2), L(2)

        print *, "Test: CTMI-style constants under the plateau model"
        T = [0.0D0, 27.0D0]
        call GROWTH_AT_TEMP(T, L, 15.0D0, 26.0D0, 2.4D0, 0.0D0, 38.0D0, 2)
        call assert_approx_equal(L(1), 1.0D0, 1.0D-12, "kappa_under = 0: no cold limitation at 0 degC")
        call assert_less_than(L(2), 1.0D-15, "kappa_over = 38: growth gone 1 degC above the plateau")
    end subroutine test_ctmi_constants_under_plateau

    ! CTMI (Rosso et al. 1993): 0 at T_min and T_max, 1 at T_opt
    subroutine test_ctmi_model()
        double precision :: T(5), L(5)

        print *, "Test: CTMI model (USE_CTMI_TEMP = .true.)"
        USE_CTMI_TEMP = .true.
        T = [1.0D0, 12.0D0, 24.0D0, 30.0D0, 35.0D0]
        ! Lower_TEMP = T_min = 1, Upper_TEMP = T_opt = 24, KAPPA_OVER = T_max = 35
        call GROWTH_AT_TEMP(T, L, 1.0D0, 24.0D0, 3.7D0, 0.0D0, 35.0D0, 5)
        USE_CTMI_TEMP = .false.
        call assert_approx_equal(L(1), 0.0D0, 1.0D-12, "0 at T_min")
        call assert_approx_equal(L(3), 1.0D0, 1.0D-12, "1 at T_opt")
        call assert_approx_equal(L(5), 0.0D0, 1.0D-12, "0 at T_max")
        call assert_in_range(L(2), 1.0D-3, 0.999D0, "between T_min and T_opt: in (0, 1)")
        call assert_in_range(L(4), 1.0D-3, 0.999D0, "between T_opt and T_max: in (0, 1)")
        call assert_less_than(L(2), L(3), "rises towards T_opt")
        call assert_less_than(L(4), L(3), "falls after T_opt")
    end subroutine test_ctmi_model

    ! CTMI requested with constants CTMI cannot use (here plateau decay rates, so
    ! "T_max" = 0.05 < T_opt): the routine falls back to the plateau model
    subroutine test_ctmi_falls_back_on_invalid_constants()
        double precision :: T(1), L(1), Lp(1)

        print *, "Test: CTMI with invalid constants falls back to plateau"
        T = [5.0D0]
        call GROWTH_AT_TEMP(T, Lp, 10.0D0, 20.0D0, 2.0D0, 0.08D0, 0.05D0, 1)
        USE_CTMI_TEMP = .true.
        call GROWTH_AT_TEMP(T, L, 10.0D0, 20.0D0, 2.0D0, 0.08D0, 0.05D0, 1)
        USE_CTMI_TEMP = .false.
        call assert_approx_equal(L(1), Lp(1), 1.0D-15, "same value as the plateau model")
    end subroutine test_ctmi_falls_back_on_invalid_constants

end program test_growth_temp
