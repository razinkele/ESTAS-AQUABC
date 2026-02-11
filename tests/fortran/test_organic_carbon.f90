! Unit tests for ORGANIC_CARBON module
! Tests for ORGANIC_CARBON_DISSOLUTION subroutine
!
! This subroutine calculates:
! - Dissolution rate of particulate organic carbon (POC to DOC)
! - Algal-dependent hydrolysis enhancement

program test_organic_carbon
    implicit none
    
    integer, parameter :: DBL_PREC = selected_real_kind(15, 307)
    
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "ORGANIC_CARBON Module Unit Tests"
    print *, "=========================================="
    print *, ""
    
    ! Run all tests
    call test_dissolution_basic()
    call test_temperature_effect()
    call test_phytoplankton_enhancement()
    call test_saturation_kinetics()
    call test_zero_poc()
    call test_array_handling()
    
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

    subroutine assert_positive(value, test_name)
        real(kind=DBL_PREC), intent(in) :: value
        character(len=*), intent(in) :: test_name
        
        if (value > 0.0D0) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5)', "    Value was: ", value
            failed = failed + 1
        end if
    end subroutine assert_positive
    
    subroutine assert_greater_than(value1, value2, test_name)
        real(kind=DBL_PREC), intent(in) :: value1, value2
        character(len=*), intent(in) :: test_name
        
        if (value1 > value2) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5,A,E12.5)', "    Expected ", value1, " > ", value2
            failed = failed + 1
        end if
    end subroutine assert_greater_than
    
    subroutine assert_approx_equal(value, expected, tol, test_name)
        real(kind=DBL_PREC), intent(in) :: value, expected, tol
        character(len=*), intent(in) :: test_name
        
        if (abs(value - expected) <= tol) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5,A,E12.5)', "    Got: ", value, " Expected: ", expected
            failed = failed + 1
        end if
    end subroutine assert_approx_equal
    
    subroutine assert_non_negative(value, test_name)
        real(kind=DBL_PREC), intent(in) :: value
        character(len=*), intent(in) :: test_name
        
        if (value >= 0.0D0) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5)', "    Value was: ", value
            failed = failed + 1
        end if
    end subroutine assert_non_negative

    ! Test basic dissolution rate calculation
    subroutine test_dissolution_basic()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: FAC_PHYT_DET_PART_ORG_C, KDISS_DET_PART_ORG_C_20
        real(kind=DBL_PREC) :: THETA_KDISS_DET_PART_ORG_C, KHS_POC_DISS_SAT
        real(kind=DBL_PREC), dimension(nkn) :: TEMP, DET_PART_ORG_C, PHYT_TOT_C
        real(kind=DBL_PREC), dimension(nkn) :: LIM_PHYT, R_DISSOLUTION
        
        print *, "Test: Basic POC dissolution rate"
        
        ! Typical model constants
        FAC_PHYT_DET_PART_ORG_C = 0.001D0     ! Phytoplankton enhancement factor
        KDISS_DET_PART_ORG_C_20 = 0.05D0      ! Base rate at 20°C (1/day)
        THETA_KDISS_DET_PART_ORG_C = 1.08D0   ! Temperature coefficient
        KHS_POC_DISS_SAT = 10.0D0             ! Half-saturation (mg/L)
        
        TEMP = 20.0D0                         ! Reference temperature
        DET_PART_ORG_C = 5.0D0                ! POC concentration (mg/L)
        PHYT_TOT_C = 2.0D0                    ! Phytoplankton carbon (mg/L)
        
        LIM_PHYT = 0.0D0
        R_DISSOLUTION = 0.0D0
        
        call ORGANIC_CARBON_DISSOLUTION( &
            FAC_PHYT_DET_PART_ORG_C, KDISS_DET_PART_ORG_C_20, &
            THETA_KDISS_DET_PART_ORG_C, KHS_POC_DISS_SAT, nkn, &
            TEMP, DET_PART_ORG_C, PHYT_TOT_C, LIM_PHYT, R_DISSOLUTION)
        
        call assert_positive(R_DISSOLUTION(1), "POC dissolution rate is positive")
        call assert_positive(LIM_PHYT(1), "Phytoplankton limitation factor is positive")
        
    end subroutine test_dissolution_basic
    
    ! Test temperature effect on dissolution
    subroutine test_temperature_effect()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT
        real(kind=DBL_PREC), dimension(nkn) :: TEMP_cold, TEMP_warm
        real(kind=DBL_PREC), dimension(nkn) :: DET_PART_ORG_C, PHYT_TOT_C
        real(kind=DBL_PREC), dimension(nkn) :: LIM_PHYT, R_cold, R_warm
        
        print *, "Test: Temperature effect on dissolution"
        
        FAC_PHYT = 0.001D0
        KDISS_20 = 0.05D0
        THETA_KDISS = 1.08D0
        KHS_SAT = 10.0D0
        
        TEMP_cold = 10.0D0    ! Cold water
        TEMP_warm = 25.0D0    ! Warm water
        DET_PART_ORG_C = 5.0D0
        PHYT_TOT_C = 2.0D0
        
        LIM_PHYT = 0.0D0
        R_cold = 0.0D0
        R_warm = 0.0D0
        
        call ORGANIC_CARBON_DISSOLUTION(FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT, nkn, &
            TEMP_cold, DET_PART_ORG_C, PHYT_TOT_C, LIM_PHYT, R_cold)
        
        call ORGANIC_CARBON_DISSOLUTION(FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT, nkn, &
            TEMP_warm, DET_PART_ORG_C, PHYT_TOT_C, LIM_PHYT, R_warm)
        
        ! Warm water should have higher dissolution rate (Arrhenius-type kinetics)
        call assert_greater_than(R_warm(1), R_cold(1), &
            "Higher temperature increases dissolution rate")
        
    end subroutine test_temperature_effect
    
    ! Test phytoplankton enhancement
    subroutine test_phytoplankton_enhancement()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT
        real(kind=DBL_PREC), dimension(nkn) :: TEMP, DET_PART_ORG_C
        real(kind=DBL_PREC), dimension(nkn) :: PHYT_low, PHYT_high
        real(kind=DBL_PREC), dimension(nkn) :: LIM_low, LIM_high, R_low, R_high
        
        print *, "Test: Phytoplankton enhancement of dissolution"
        
        FAC_PHYT = 0.01D0      ! 1% enhancement per mg/L phytoplankton
        KDISS_20 = 0.05D0
        THETA_KDISS = 1.08D0
        KHS_SAT = 10.0D0
        
        TEMP = 20.0D0
        DET_PART_ORG_C = 5.0D0
        PHYT_low = 1.0D0       ! Low phytoplankton
        PHYT_high = 10.0D0     ! High phytoplankton
        
        LIM_low = 0.0D0
        LIM_high = 0.0D0
        R_low = 0.0D0
        R_high = 0.0D0
        
        call ORGANIC_CARBON_DISSOLUTION(FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT, nkn, &
            TEMP, DET_PART_ORG_C, PHYT_low, LIM_low, R_low)
        
        call ORGANIC_CARBON_DISSOLUTION(FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT, nkn, &
            TEMP, DET_PART_ORG_C, PHYT_high, LIM_high, R_high)
        
        ! Higher phytoplankton should increase dissolution rate
        call assert_greater_than(R_high(1), R_low(1), &
            "Higher phytoplankton biomass increases dissolution")
        call assert_greater_than(LIM_high(1), LIM_low(1), &
            "Higher phytoplankton gives higher limitation factor")
        
    end subroutine test_phytoplankton_enhancement
    
    ! Test saturation kinetics (POC concentration effect)
    subroutine test_saturation_kinetics()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT
        real(kind=DBL_PREC), dimension(nkn) :: TEMP, PHYT_TOT_C
        real(kind=DBL_PREC), dimension(nkn) :: POC_low, POC_high
        real(kind=DBL_PREC), dimension(nkn) :: LIM_PHYT, R_low, R_high
        real(kind=DBL_PREC) :: R_ratio, POC_ratio
        
        print *, "Test: Saturation kinetics"
        
        FAC_PHYT = 0.001D0
        KDISS_20 = 0.05D0
        THETA_KDISS = 1.0D0    ! No temp effect (theta = 1)
        KHS_SAT = 10.0D0
        
        TEMP = 20.0D0
        PHYT_TOT_C = 0.0D0     ! No phytoplankton enhancement
        POC_low = 2.0D0        ! Low POC
        POC_high = 50.0D0      ! High POC (saturating)
        
        LIM_PHYT = 0.0D0
        R_low = 0.0D0
        R_high = 0.0D0
        
        call ORGANIC_CARBON_DISSOLUTION(FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT, nkn, &
            TEMP, POC_low, PHYT_TOT_C, LIM_PHYT, R_low)
        
        call ORGANIC_CARBON_DISSOLUTION(FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT, nkn, &
            TEMP, POC_high, PHYT_TOT_C, LIM_PHYT, R_high)
        
        ! Rate should increase with POC, but less than proportionally due to saturation
        R_ratio = R_high(1) / R_low(1)
        POC_ratio = POC_high(1) / POC_low(1)
        
        call assert_greater_than(R_high(1), R_low(1), &
            "Higher POC increases dissolution rate")
        call assert_greater_than(POC_ratio, R_ratio, &
            "Rate increase is less than proportional (saturation)")
        
    end subroutine test_saturation_kinetics
    
    ! Test with zero POC (should give zero rate)
    subroutine test_zero_poc()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT
        real(kind=DBL_PREC), dimension(nkn) :: TEMP, DET_PART_ORG_C, PHYT_TOT_C
        real(kind=DBL_PREC), dimension(nkn) :: LIM_PHYT, R_DISSOLUTION
        
        print *, "Test: Zero POC gives zero dissolution"
        
        FAC_PHYT = 0.001D0
        KDISS_20 = 0.05D0
        THETA_KDISS = 1.08D0
        KHS_SAT = 10.0D0
        
        TEMP = 20.0D0
        DET_PART_ORG_C = 0.0D0   ! Zero POC
        PHYT_TOT_C = 2.0D0
        
        LIM_PHYT = 0.0D0
        R_DISSOLUTION = 0.0D0
        
        call ORGANIC_CARBON_DISSOLUTION(FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT, nkn, &
            TEMP, DET_PART_ORG_C, PHYT_TOT_C, LIM_PHYT, R_DISSOLUTION)
        
        call assert_approx_equal(R_DISSOLUTION(1), 0.0D0, 1.0D-15, &
            "Zero POC produces zero dissolution rate")
        
    end subroutine test_zero_poc
    
    ! Test array handling
    subroutine test_array_handling()
        integer, parameter :: nkn = 5
        real(kind=DBL_PREC) :: FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT
        real(kind=DBL_PREC), dimension(nkn) :: TEMP, DET_PART_ORG_C, PHYT_TOT_C
        real(kind=DBL_PREC), dimension(nkn) :: LIM_PHYT, R_DISSOLUTION
        integer :: i
        logical :: all_valid
        
        print *, "Test: Array handling with varying conditions"
        
        FAC_PHYT = 0.001D0
        KDISS_20 = 0.05D0
        THETA_KDISS = 1.08D0
        KHS_SAT = 10.0D0
        
        ! Varying conditions across nodes
        TEMP = (/ 10.0D0, 15.0D0, 20.0D0, 25.0D0, 30.0D0 /)
        DET_PART_ORG_C = (/ 1.0D0, 3.0D0, 5.0D0, 7.0D0, 10.0D0 /)
        PHYT_TOT_C = (/ 0.5D0, 1.0D0, 2.0D0, 3.0D0, 5.0D0 /)
        
        LIM_PHYT = 0.0D0
        R_DISSOLUTION = 0.0D0
        
        call ORGANIC_CARBON_DISSOLUTION(FAC_PHYT, KDISS_20, THETA_KDISS, KHS_SAT, nkn, &
            TEMP, DET_PART_ORG_C, PHYT_TOT_C, LIM_PHYT, R_DISSOLUTION)
        
        ! All rates should be positive and increasing (temp, POC, phyto all increase)
        all_valid = .true.
        do i = 1, nkn
            if (R_DISSOLUTION(i) <= 0.0D0) all_valid = .false.
        end do
        
        if (all_valid) then
            print '(A,A)', "  PASS: ", "All array elements have positive dissolution rates"
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", "Some array elements have non-positive rates"
            failed = failed + 1
        end if
        
    end subroutine test_array_handling

end program test_organic_carbon
