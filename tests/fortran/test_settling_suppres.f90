! Unit tests for settling suppression factor
! Tests the empirical relationship between chlorophyll and settling velocity

program test_settling_suppres
    implicit none
    
    integer, parameter :: DBL_PREC = selected_real_kind(15, 307)
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "Settling Suppression Factor Unit Tests"
    print *, "=========================================="
    print *, ""
    
    call test_low_chla()
    call test_mid_chla()
    call test_high_chla()
    call test_transition_points()
    call test_factor_range()
    call test_gradient()
    
    print *, ""
    print *, "=========================================="
    print *, "Test Summary"
    print *, "=========================================="
    print '(A,I3,A)', " Passed: ", passed, " tests"
    print '(A,I3,A)', " Failed: ", failed, " tests"
    print *, "=========================================="
    
    if (failed == 0) then
        print *, "ALL TESTS PASSED!"
    else
        print *, "SOME TESTS FAILED"
        stop 1
    end if

contains

    subroutine assert_in_range(value, min_val, max_val, test_name)
        double precision, intent(in) :: value, min_val, max_val
        character(len=*), intent(in) :: test_name
        
        if (value >= min_val .and. value <= max_val) then
            print *, "  PASS: ", trim(test_name)
            passed = passed + 1
        else
            print *, "  FAIL: ", trim(test_name)
            print '(A,E12.6,A,E12.6,A,E12.6,A)', "    Value: ", value, " not in [", min_val, ", ", max_val, "]"
            failed = failed + 1
        end if
    end subroutine assert_in_range
    
    subroutine assert_greater_than(val1, val2, test_name)
        double precision, intent(in) :: val1, val2
        character(len=*), intent(in) :: test_name
        
        if (val1 > val2) then
            print *, "  PASS: ", trim(test_name)
            passed = passed + 1
        else
            print *, "  FAIL: ", trim(test_name)
            print '(A,E12.6,A,E12.6)', "    Expected ", val1, " > ", val2
            failed = failed + 1
        end if
    end subroutine assert_greater_than
    
    subroutine assert_approx_equal(val1, val2, tol, test_name)
        double precision, intent(in) :: val1, val2, tol
        character(len=*), intent(in) :: test_name
        
        if (abs(val1 - val2) <= tol) then
            print *, "  PASS: ", trim(test_name)
            passed = passed + 1
        else
            print *, "  FAIL: ", trim(test_name)
            print '(A,E12.6,A,E12.6,A,E12.6)', "    |", val1, " - ", val2, "| > ", tol
            failed = failed + 1
        end if
    end subroutine assert_approx_equal

    !------------------------------------------------------------------
    ! Helper function for settling velocity calculation
    !------------------------------------------------------------------
    double precision function calc_settl_vel(chla_val)
        implicit none
        double precision, intent(in) :: chla_val
        calc_settl_vel = -(0.0061D0 * chla_val) + 1.0383D0
    end function calc_settl_vel

    !------------------------------------------------------------------
    ! Local copy of settling_suppres_factor subroutine
    !------------------------------------------------------------------
    subroutine settling_suppres_factor(chla, factor)
        implicit none
        double precision, intent(in) :: chla
        double precision, intent(out) :: factor
        double precision :: chlamin, chlamax, settl_max
        
        ! Observed interval endpoints for chla impacting settling velocity
        chlamin = 44.0D0
        chlamax = 140.0D0
        
        settl_max = calc_settl_vel(chlamin)
        
        if (chla < chlamin) then
            factor = 1.0D0
        else if (chla >= chlamin .and. chla <= chlamax) then
            factor = calc_settl_vel(chla) / settl_max
        else
            factor = calc_settl_vel(chlamax) / settl_max
        end if
    end subroutine settling_suppres_factor

    !------------------------------------------------------------------
    ! Test: Low chlorophyll (< 44 µg/L) gives factor = 1.0
    !------------------------------------------------------------------
    subroutine test_low_chla()
        double precision :: chla, factor
        
        print *, "Test: Low chlorophyll gives factor = 1.0"
        
        chla = 20.0D0
        call settling_suppres_factor(chla, factor)
        
        call assert_approx_equal(factor, 1.0D0, 0.001D0, &
            "chla=20: factor = 1.0 (no suppression)")
        
        chla = 0.0D0
        call settling_suppres_factor(chla, factor)
        
        call assert_approx_equal(factor, 1.0D0, 0.001D0, &
            "chla=0: factor = 1.0 (no suppression)")
        
        chla = 43.0D0
        call settling_suppres_factor(chla, factor)
        
        call assert_approx_equal(factor, 1.0D0, 0.001D0, &
            "chla=43: factor = 1.0 (just below threshold)")
    end subroutine test_low_chla
    
    !------------------------------------------------------------------
    ! Test: Mid-range chlorophyll (44-140 µg/L)
    !------------------------------------------------------------------
    subroutine test_mid_chla()
        double precision :: chla, factor
        
        print *, "Test: Mid-range chlorophyll reduces settling"
        
        ! At minimum threshold (44), factor should be exactly 1.0
        chla = 44.0D0
        call settling_suppres_factor(chla, factor)
        
        call assert_approx_equal(factor, 1.0D0, 0.001D0, &
            "At chla=44 (threshold), factor = 1.0")
        
        ! At intermediate value, factor should be < 1.0
        chla = 90.0D0
        call settling_suppres_factor(chla, factor)
        
        call assert_in_range(factor, 0.5D0, 0.99D0, &
            "At chla=90, factor is reduced (0.5-0.99)")
    end subroutine test_mid_chla
    
    !------------------------------------------------------------------
    ! Test: High chlorophyll (> 140 µg/L) - capped factor
    !------------------------------------------------------------------
    subroutine test_high_chla()
        double precision :: chla, factor, factor_at_max
        
        print *, "Test: High chlorophyll gives capped factor"
        
        ! Get factor at maximum threshold
        chla = 140.0D0
        call settling_suppres_factor(chla, factor_at_max)
        
        ! Above max, factor should be same as at max
        chla = 200.0D0
        call settling_suppres_factor(chla, factor)
        
        call assert_approx_equal(factor, factor_at_max, 0.001D0, &
            "chla=200: factor capped at max threshold value")
        
        chla = 500.0D0
        call settling_suppres_factor(chla, factor)
        
        call assert_approx_equal(factor, factor_at_max, 0.001D0, &
            "chla=500: factor still capped")
    end subroutine test_high_chla
    
    !------------------------------------------------------------------
    ! Test: Transition points
    !------------------------------------------------------------------
    subroutine test_transition_points()
        double precision :: chla, factor_below, factor_at
        
        print *, "Test: Transition at thresholds is smooth"
        
        ! Just below 44
        chla = 43.9D0
        call settling_suppres_factor(chla, factor_below)
        
        ! At exactly 44
        chla = 44.0D0
        call settling_suppres_factor(chla, factor_at)
        
        ! Both should be 1.0 (or very close)
        call assert_approx_equal(factor_below, factor_at, 0.01D0, &
            "Transition at 44 is continuous")
    end subroutine test_transition_points
    
    !------------------------------------------------------------------
    ! Test: Factor is always in [0, 1]
    !------------------------------------------------------------------
    subroutine test_factor_range()
        double precision :: chla, factor
        double precision :: test_values(10)
        integer :: i
        logical :: all_valid
        
        print *, "Test: Factor always in valid range [0, 1]"
        
        test_values = (/ 0.0D0, 10.0D0, 44.0D0, 60.0D0, 90.0D0, &
                         120.0D0, 140.0D0, 200.0D0, 500.0D0, 1000.0D0 /)
        
        all_valid = .true.
        do i = 1, 10
            call settling_suppres_factor(test_values(i), factor)
            if (factor < 0.0D0 .or. factor > 1.0D0) then
                all_valid = .false.
                print '(A,F8.1,A,F8.4)', "    Invalid at chla=", test_values(i), ": factor=", factor
            end if
        end do
        
        if (all_valid) then
            print *, "  PASS: All factors in [0, 1] range"
            passed = passed + 1
        else
            print *, "  FAIL: Some factors out of range"
            failed = failed + 1
        end if
    end subroutine test_factor_range
    
    !------------------------------------------------------------------
    ! Test: Higher chlorophyll gives lower factor (more suppression)
    !------------------------------------------------------------------
    subroutine test_gradient()
        double precision :: factor_60, factor_100
        
        print *, "Test: Higher chlorophyll gives more suppression"
        
        call settling_suppres_factor(60.0D0, factor_60)
        call settling_suppres_factor(100.0D0, factor_100)
        
        call assert_greater_than(factor_60, factor_100, &
            "factor at chla=60 > factor at chla=100")
    end subroutine test_gradient

end program test_settling_suppres
