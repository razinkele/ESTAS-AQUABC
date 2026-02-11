! Unit tests for ammonia chemistry functions
! Tests UNIONIZED_AMMONIA and AMMONIA_VOLATILIZATION subroutines

program test_ammonia_chem
    implicit none
    
    integer, parameter :: DBL_PREC = selected_real_kind(15, 307)
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "Ammonia Chemistry Unit Tests"
    print *, "=========================================="
    print *, ""
    
    ! Unionized ammonia tests
    call test_nh3_basic()
    call test_nh3_ph_effect()
    call test_nh3_temp_effect()
    call test_nh3_zero_nh4()
    call test_nh3_vector()
    
    ! Ammonia volatilization tests
    call test_volatil_basic()
    call test_volatil_ph_effect()
    call test_volatil_temp_effect()
    
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
    ! Local copy of UNIONIZED_AMMONIA subroutine
    !------------------------------------------------------------------
    subroutine UNIONIZED_AMMONIA(NH3N, NH4N, pH, TEMP, nkn)
        implicit none
        integer, intent(in) :: nkn
        double precision, intent(out) :: NH3N(nkn)
        double precision, intent(in) :: NH4N(nkn)
        double precision, intent(in) :: pH(nkn)
        double precision, intent(in) :: TEMP(nkn)
        
        double precision :: FRAC_NH3(nkn)
        double precision :: pKH(nkn)
        double precision :: T_KELVIN(nkn)
        
        T_KELVIN = TEMP + 2.7316D2
        pKH      = 9.018D-2 + (2.72992D3 / T_KELVIN)
        FRAC_NH3 = 1.0D0 / (1.0D0 + (1.0D1 ** (pKH - pH)))
        NH3N     = FRAC_NH3 * NH4N
    end subroutine UNIONIZED_AMMONIA
    
    !------------------------------------------------------------------
    ! Local copy of AMMONIA_VOLATILIZATION subroutine
    !------------------------------------------------------------------
    subroutine AMMONIA_VOLATILIZATION(AMMONIA_VOLATIL_RATE, NH4N, pH, TEMP, KA, nkn)
        implicit none
        integer, intent(in) :: nkn
        double precision, intent(out) :: AMMONIA_VOLATIL_RATE(nkn)
        double precision, intent(in) :: NH4N(nkn)
        double precision, intent(in) :: pH(nkn)
        double precision, intent(in) :: TEMP(nkn)
        double precision, intent(in) :: KA(nkn)
        
        double precision :: NH3N(nkn)
        double precision :: NH3S
        
        NH3S = 0.0D0  ! Assuming zero partial pressure of NH3 in atmosphere
        call UNIONIZED_AMMONIA(NH3N, NH4N, pH, TEMP, nkn)
        
        ! 1.4D1/1.70D1 : Ratio of NH3N:NH3
        ! 3.2D1/1.7D1  : Ratio of molecular weight of oxygen to ammonia
        AMMONIA_VOLATIL_RATE = KA * (NH3N - (NH3S * (1.4D1/1.70D1))) * ((3.2D1/1.7D1)**2.5D-1)
    end subroutine AMMONIA_VOLATILIZATION

    !------------------------------------------------------------------
    ! Test: Basic unionized ammonia calculation
    !------------------------------------------------------------------
    subroutine test_nh3_basic()
        integer, parameter :: nkn = 1
        double precision :: NH3N(nkn), NH4N(nkn), pH(nkn), TEMP(nkn)
        
        print *, "Test: Basic unionized ammonia calculation"
        
        ! Typical conditions: pH 8.0, 20°C, 1 mg/L NH4-N
        NH4N = (/ 1.0D0 /)
        pH = (/ 8.0D0 /)
        TEMP = (/ 20.0D0 /)
        
        call UNIONIZED_AMMONIA(NH3N, NH4N, pH, TEMP, nkn)
        
        ! At pH 8.0, ~3-5% of ammonia is unionized at 20°C
        call assert_in_range(NH3N(1), 0.02D0, 0.10D0, &
            "NH3-N fraction at pH 8.0, 20C is reasonable")
    end subroutine test_nh3_basic
    
    !------------------------------------------------------------------
    ! Test: pH effect on unionized ammonia
    !------------------------------------------------------------------
    subroutine test_nh3_ph_effect()
        integer, parameter :: nkn = 1
        double precision :: NH3N_low(nkn), NH3N_high(nkn)
        double precision :: NH4N(nkn), pH_low(nkn), pH_high(nkn), TEMP(nkn)
        
        print *, "Test: Higher pH increases unionized ammonia fraction"
        
        NH4N = (/ 1.0D0 /)
        pH_low = (/ 7.0D0 /)
        pH_high = (/ 9.0D0 /)
        TEMP = (/ 20.0D0 /)
        
        call UNIONIZED_AMMONIA(NH3N_low, NH4N, pH_low, TEMP, nkn)
        call UNIONIZED_AMMONIA(NH3N_high, NH4N, pH_high, TEMP, nkn)
        
        call assert_greater_than(NH3N_high(1), NH3N_low(1), &
            "Higher pH gives more NH3-N")
        
        ! At low pH, almost all is ionized (NH4+)
        call assert_in_range(NH3N_low(1), 0.0D0, 0.02D0, &
            "At pH 7, NH3-N is < 2% of total")
            
        ! At high pH, significant fraction is unionized
        call assert_in_range(NH3N_high(1), 0.2D0, 0.6D0, &
            "At pH 9, NH3-N is 20-60% of total")
    end subroutine test_nh3_ph_effect
    
    !------------------------------------------------------------------
    ! Test: Temperature effect on unionized ammonia
    !------------------------------------------------------------------
    subroutine test_nh3_temp_effect()
        integer, parameter :: nkn = 1
        double precision :: NH3N_cold(nkn), NH3N_warm(nkn)
        double precision :: NH4N(nkn), pH(nkn), TEMP_cold(nkn), TEMP_warm(nkn)
        
        print *, "Test: Higher temperature increases unionized ammonia"
        
        NH4N = (/ 1.0D0 /)
        pH = (/ 8.5D0 /)
        TEMP_cold = (/ 5.0D0 /)
        TEMP_warm = (/ 25.0D0 /)
        
        call UNIONIZED_AMMONIA(NH3N_cold, NH4N, pH, TEMP_cold, nkn)
        call UNIONIZED_AMMONIA(NH3N_warm, NH4N, pH, TEMP_warm, nkn)
        
        call assert_greater_than(NH3N_warm(1), NH3N_cold(1), &
            "Warmer water has more NH3-N")
    end subroutine test_nh3_temp_effect
    
    !------------------------------------------------------------------
    ! Test: Zero NH4 gives zero NH3
    !------------------------------------------------------------------
    subroutine test_nh3_zero_nh4()
        integer, parameter :: nkn = 1
        double precision :: NH3N(nkn), NH4N(nkn), pH(nkn), TEMP(nkn)
        
        print *, "Test: Zero NH4 gives zero NH3"
        
        NH4N = (/ 0.0D0 /)
        pH = (/ 8.5D0 /)
        TEMP = (/ 20.0D0 /)
        
        call UNIONIZED_AMMONIA(NH3N, NH4N, pH, TEMP, nkn)
        
        call assert_approx_equal(NH3N(1), 0.0D0, 1.0D-10, &
            "Zero NH4 produces zero NH3")
    end subroutine test_nh3_zero_nh4
    
    !------------------------------------------------------------------
    ! Test: Vector operation
    !------------------------------------------------------------------
    subroutine test_nh3_vector()
        integer, parameter :: nkn = 5
        double precision :: NH3N(nkn), NH4N(nkn), pH(nkn), TEMP(nkn)
        integer :: i
        logical :: all_positive
        
        print *, "Test: Vector operation with multiple nodes"
        
        NH4N = (/ 0.5D0, 1.0D0, 1.5D0, 2.0D0, 2.5D0 /)
        pH = (/ 8.0D0, 8.0D0, 8.0D0, 8.0D0, 8.0D0 /)
        TEMP = (/ 20.0D0, 20.0D0, 20.0D0, 20.0D0, 20.0D0 /)
        
        call UNIONIZED_AMMONIA(NH3N, NH4N, pH, TEMP, nkn)
        
        all_positive = .true.
        do i = 1, nkn
            if (NH3N(i) <= 0.0D0) all_positive = .false.
        end do
        
        if (all_positive) then
            print *, "  PASS: All vector elements positive"
            passed = passed + 1
        else
            print *, "  FAIL: Some vector elements not positive"
            failed = failed + 1
        end if
        
        ! Check proportionality: NH3 should scale with NH4
        call assert_greater_than(NH3N(5), NH3N(1), &
            "Higher NH4 gives higher NH3")
    end subroutine test_nh3_vector
    
    !------------------------------------------------------------------
    ! Test: Basic volatilization rate
    !------------------------------------------------------------------
    subroutine test_volatil_basic()
        integer, parameter :: nkn = 1
        double precision :: RATE(nkn), NH4N(nkn), pH(nkn), TEMP(nkn), KA(nkn)
        
        print *, "Test: Basic ammonia volatilization"
        
        NH4N = (/ 2.0D0 /)      ! 2 mg/L NH4-N
        pH = (/ 9.0D0 /)        ! High pH for significant volatilization
        TEMP = (/ 20.0D0 /)
        KA = (/ 5.0D0 /)        ! Reaeration rate (1/day)
        
        call AMMONIA_VOLATILIZATION(RATE, NH4N, pH, TEMP, KA, nkn)
        
        ! Volatilization rate should be positive
        call assert_in_range(RATE(1), 0.0D0, 10.0D0, &
            "Volatilization rate is positive and reasonable")
    end subroutine test_volatil_basic
    
    !------------------------------------------------------------------
    ! Test: pH effect on volatilization
    !------------------------------------------------------------------
    subroutine test_volatil_ph_effect()
        integer, parameter :: nkn = 1
        double precision :: RATE_low(nkn), RATE_high(nkn)
        double precision :: NH4N(nkn), pH_low(nkn), pH_high(nkn), TEMP(nkn), KA(nkn)
        
        print *, "Test: Higher pH increases volatilization rate"
        
        NH4N = (/ 2.0D0 /)
        pH_low = (/ 7.0D0 /)
        pH_high = (/ 9.5D0 /)
        TEMP = (/ 20.0D0 /)
        KA = (/ 5.0D0 /)
        
        call AMMONIA_VOLATILIZATION(RATE_low, NH4N, pH_low, TEMP, KA, nkn)
        call AMMONIA_VOLATILIZATION(RATE_high, NH4N, pH_high, TEMP, KA, nkn)
        
        call assert_greater_than(RATE_high(1), RATE_low(1), &
            "Higher pH gives higher volatilization rate")
    end subroutine test_volatil_ph_effect
    
    !------------------------------------------------------------------
    ! Test: Temperature effect on volatilization
    !------------------------------------------------------------------
    subroutine test_volatil_temp_effect()
        integer, parameter :: nkn = 1
        double precision :: RATE_cold(nkn), RATE_warm(nkn)
        double precision :: NH4N(nkn), pH(nkn), TEMP_cold(nkn), TEMP_warm(nkn), KA(nkn)
        
        print *, "Test: Higher temperature increases volatilization"
        
        NH4N = (/ 2.0D0 /)
        pH = (/ 8.5D0 /)
        TEMP_cold = (/ 5.0D0 /)
        TEMP_warm = (/ 25.0D0 /)
        KA = (/ 5.0D0 /)
        
        call AMMONIA_VOLATILIZATION(RATE_cold, NH4N, pH, TEMP_cold, KA, nkn)
        call AMMONIA_VOLATILIZATION(RATE_warm, NH4N, pH, TEMP_warm, KA, nkn)
        
        call assert_greater_than(RATE_warm(1), RATE_cold(1), &
            "Warmer temperature gives higher volatilization")
    end subroutine test_volatil_temp_effect

end program test_ammonia_chem
