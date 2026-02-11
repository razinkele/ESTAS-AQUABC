! Unit tests for IRON_II module
! Tests for IRON_II_DISSOLUTION and IRON_II_OXIDATION subroutines
!
! These subroutines calculate:
! - Iron II dissolution based on pH, sulfide, and alkalinity (Stumm and Lee, 1960)
! - Iron II oxidation rates based on DO, pH, temperature, salinity (Morgan and Lahav, 2007)

program test_iron_ii
    implicit none
    
    integer, parameter :: DBL_PREC = selected_real_kind(15, 307)
    
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "IRON_II Module Unit Tests"
    print *, "=========================================="
    print *, ""
    
    ! Run all tests
    call test_dissolution_basic()
    call test_dissolution_ph_effect()
    call test_dissolution_low_sulfide()
    call test_oxidation_basic()
    call test_oxidation_temperature_effect()
    call test_oxidation_ph_effect()
    
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
    
    subroutine assert_in_range(value, min_val, max_val, test_name)
        real(kind=DBL_PREC), intent(in) :: value, min_val, max_val
        character(len=*), intent(in) :: test_name
        
        if (value >= min_val .and. value <= max_val) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5,A,E12.5,A,E12.5)', "    Value: ", value, " not in [", min_val, ", ", max_val, "]"
            failed = failed + 1
        end if
    end subroutine assert_in_range
    
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

    ! Test IRON_II_DISSOLUTION with typical conditions
    subroutine test_dissolution_basic()
        integer, parameter :: nkn = 2
        real(kind=DBL_PREC), dimension(nkn) :: HS2_TOT, PH, TOT_ALK, FE_II_TOT
        
        print *, "Test: IRON_II_DISSOLUTION basic functionality"
        
        ! Set up typical lake conditions
        HS2_TOT = 1.0D-6   ! Low sulfide (mol/L)
        PH = 7.5D0          ! Neutral to slightly alkaline
        TOT_ALK = 2.0D-3    ! Typical alkalinity (mol/L)
        FE_II_TOT = 0.0D0   ! Will be calculated
        
        call IRON_II_DISSOLUTION(HS2_TOT, PH, TOT_ALK, nkn, FE_II_TOT)
        
        ! Fe II total solubility should be positive
        call assert_positive(FE_II_TOT(1), "Fe II dissolution produces positive result")
        
    end subroutine test_dissolution_basic
    
    ! Test pH effect on dissolution
    subroutine test_dissolution_ph_effect()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC), dimension(nkn) :: HS2_TOT, PH_low, PH_high, TOT_ALK
        real(kind=DBL_PREC), dimension(nkn) :: FE_II_low_ph, FE_II_high_ph
        
        print *, "Test: IRON_II_DISSOLUTION pH effect"
        
        HS2_TOT = 1.0D-6
        PH_low = 6.0D0      ! Lower pH
        PH_high = 8.5D0     ! Higher pH
        TOT_ALK = 2.0D-3
        
        FE_II_low_ph = 0.0D0
        FE_II_high_ph = 0.0D0
        
        call IRON_II_DISSOLUTION(HS2_TOT, PH_low, TOT_ALK, nkn, FE_II_low_ph)
        call IRON_II_DISSOLUTION(HS2_TOT, PH_high, TOT_ALK, nkn, FE_II_high_ph)
        
        ! Lower pH should increase Fe II solubility (more acidic conditions dissolve more iron)
        call assert_greater_than(FE_II_low_ph(1), FE_II_high_ph(1), &
            "Lower pH increases Fe II solubility")
        
    end subroutine test_dissolution_ph_effect
    
    ! Test with very low sulfide (should use carbonate/hydroxide path)
    subroutine test_dissolution_low_sulfide()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC), dimension(nkn) :: HS2_TOT, PH, TOT_ALK, FE_II_TOT
        
        print *, "Test: IRON_II_DISSOLUTION with very low sulfide"
        
        HS2_TOT = 1.0D-21   ! Essentially zero sulfide
        PH = 7.0D0
        TOT_ALK = 2.0D-3
        FE_II_TOT = 0.0D0
        
        call IRON_II_DISSOLUTION(HS2_TOT, PH, TOT_ALK, nkn, FE_II_TOT)
        
        ! Should still calculate a valid solubility (hydroxide path)
        call assert_positive(FE_II_TOT(1), "Low sulfide path produces positive Fe II")
        
    end subroutine test_dissolution_low_sulfide
    
    ! Test IRON_II_OXIDATION with typical conditions
    subroutine test_oxidation_basic()
        integer, parameter :: nkn = 2
        real(kind=DBL_PREC), dimension(nkn) :: FE_II_DISS, DOXY, PH, TEMP, SALT, ELEVATION
        real(kind=DBL_PREC), dimension(nkn) :: R_FE_II_OXIDATION
        
        print *, "Test: IRON_II_OXIDATION basic functionality"
        
        ! Typical oxic water conditions
        FE_II_DISS = 1.0D0      ! 1 mg/L dissolved Fe II
        DOXY = 8.0D0            ! 8 mg/L DO (near saturation at 20°C)
        PH = 7.0D0              ! Neutral pH
        TEMP = 20.0D0           ! 20°C
        SALT = 0.0D0            ! Freshwater
        ELEVATION = 0.0D0       ! Sea level
        
        R_FE_II_OXIDATION = 0.0D0
        
        call IRON_II_OXIDATION(FE_II_DISS, DOXY, PH, TEMP, SALT, ELEVATION, nkn, R_FE_II_OXIDATION)
        
        ! Oxidation rate should be positive in oxic conditions
        call assert_positive(R_FE_II_OXIDATION(1), "Fe II oxidation rate is positive in oxic water")
        
    end subroutine test_oxidation_basic
    
    ! Test temperature effect on oxidation
    subroutine test_oxidation_temperature_effect()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC), dimension(nkn) :: FE_II_DISS, DOXY, PH, TEMP_low, TEMP_high, SALT, ELEVATION
        real(kind=DBL_PREC), dimension(nkn) :: R_low_temp, R_high_temp
        
        print *, "Test: IRON_II_OXIDATION temperature sensitivity"
        
        FE_II_DISS = 1.0D0
        DOXY = 8.0D0
        PH = 7.0D0
        TEMP_low = 10.0D0    ! Cold water
        TEMP_high = 25.0D0   ! Warm water
        SALT = 0.0D0
        ELEVATION = 0.0D0
        
        R_low_temp = 0.0D0
        R_high_temp = 0.0D0
        
        call IRON_II_OXIDATION(FE_II_DISS, DOXY, PH, TEMP_low, SALT, ELEVATION, nkn, R_low_temp)
        call IRON_II_OXIDATION(FE_II_DISS, DOXY, PH, TEMP_high, SALT, ELEVATION, nkn, R_high_temp)
        
        ! Both should produce valid rates (temperature effect is indirect through DO saturation)
        call assert_positive(R_low_temp(1), "Cold water oxidation rate is valid")
        call assert_positive(R_high_temp(1), "Warm water oxidation rate is valid")
        
    end subroutine test_oxidation_temperature_effect
    
    ! Test pH effect on oxidation
    subroutine test_oxidation_ph_effect()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC), dimension(nkn) :: FE_II_DISS, DOXY, PH_low, PH_high, TEMP, SALT, ELEVATION
        real(kind=DBL_PREC), dimension(nkn) :: R_low_ph, R_high_ph
        
        print *, "Test: IRON_II_OXIDATION pH sensitivity"
        
        FE_II_DISS = 1.0D0
        DOXY = 8.0D0
        PH_low = 6.0D0       ! Acidic
        PH_high = 8.0D0      ! Alkaline
        TEMP = 20.0D0
        SALT = 0.0D0
        ELEVATION = 0.0D0
        
        R_low_ph = 0.0D0
        R_high_ph = 0.0D0
        
        call IRON_II_OXIDATION(FE_II_DISS, DOXY, PH_low, TEMP, SALT, ELEVATION, nkn, R_low_ph)
        call IRON_II_OXIDATION(FE_II_DISS, DOXY, PH_high, TEMP, SALT, ELEVATION, nkn, R_high_ph)
        
        ! Higher pH should increase oxidation rate (Fe II oxidation is faster in alkaline conditions)
        call assert_greater_than(R_high_ph(1), R_low_ph(1), &
            "Higher pH increases Fe II oxidation rate")
        
    end subroutine test_oxidation_ph_effect

end program test_iron_ii
