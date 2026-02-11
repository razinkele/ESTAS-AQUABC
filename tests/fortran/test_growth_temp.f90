! Unit tests for GROWTH_AT_TEMP subroutine
! Tests temperature limitation factor calculations for phytoplankton growth
!
! This is a core function used by DIATOMS, CYANOBACTERIA, and other algae modules
! The function returns 1.0 when temperature is in optimal range,
! and decreases exponentially outside that range

program test_growth_temp
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

    ! Local copy of GROWTH_AT_TEMP for testing (extracted from pelagic_auxillary)
    subroutine GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP, &
                              KAPPA_UNDER_OPT_TEMP, KAPPA_OVER_OPT_TEMP, nkn)
        implicit none
        integer :: nkn
        double precision, intent(in) :: TEMP(nkn)
        double precision :: LIM_TEMP_GROWTH(nkn)
        double precision, intent(in) :: Lower_TEMP, Upper_TEMP
        double precision, intent(in) :: K_AT_OPT_TEMP
        double precision, intent(in) :: KAPPA_UNDER_OPT_TEMP
        double precision, intent(in) :: KAPPA_OVER_OPT_TEMP

        where (TEMP <= Lower_TEMP)
            LIM_TEMP_GROWTH = 1.0D0 * &
                exp((-1.0D0) * KAPPA_UNDER_OPT_TEMP * dabs(Lower_TEMP - TEMP))
        end where

        where ((TEMP > Lower_TEMP) .and. (TEMP < Upper_TEMP))
            LIM_TEMP_GROWTH = 1.D0
        end where

        where (TEMP >= Upper_TEMP)
            LIM_TEMP_GROWTH = 1.0D0 * &
                exp((-1.0D0) * KAPPA_OVER_OPT_TEMP * dabs(Upper_TEMP - TEMP))
        end where
    end subroutine GROWTH_AT_TEMP

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

end program test_growth_temp
