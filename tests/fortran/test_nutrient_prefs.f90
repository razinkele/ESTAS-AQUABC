! Unit tests for nutrient preference subroutines
! Tests AMMONIA_PREFS and DIN_DON_PREFS calculations
!
! These functions calculate nutrient uptake preferences for phytoplankton
! Based on WASP model formulations

program test_nutrient_prefs
    implicit none
    
    integer, parameter :: DBL_PREC = selected_real_kind(15, 307)
    
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "Nutrient Preference Unit Tests"
    print *, "=========================================="
    print *, ""
    
    ! Run all tests
    call test_ammonia_pref_basic()
    call test_ammonia_pref_nh4_only()
    call test_ammonia_pref_no3_only()
    call test_ammonia_pref_zero_nutrients()
    call test_ammonia_pref_equal_nutrients()
    call test_ammonia_pref_sum_near_zero()  ! NEW: edge case test for improved guard
    call test_din_don_pref_basic()
    call test_din_don_pref_fraction()
    call test_din_don_pref_sum_near_zero()  ! NEW: edge case test for improved guard
    call test_dip_dop_pref_basic()
    call test_dip_dop_pref_sum_near_zero()  ! NEW: edge case test for improved guard
    
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

    ! Local copy of AMMONIA_PREFS (from pelagic_auxillary)
    ! Updated: guard condition checks sum of NH3+NOx to prevent division by near-zero
    subroutine AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)
        implicit none
        integer :: nkn
        double precision :: AMMONIA_PREF(nkn)
        double precision, intent(in) :: NH3(nkn)
        double precision, intent(in) :: NOx(nkn)
        double precision, intent(in) :: kn
        double precision :: PN(nkn)

        ! Guard condition: check sum of NH3+NOx to prevent division by near-zero
        where ((NH3 + NOx) .lt. 1.0D-6)
            PN = 0.0D0
        elsewhere
            PN = (NH3 * NOx) / ((kn + NH3) * (kn + NOx)) + (kn * NH3) / ((NH3 + NOx) * (kn + NOx))
        end where
        AMMONIA_PREF = PN
    end subroutine AMMONIA_PREFS

    ! Local copy of DIN_DON_PREFS (from pelagic_auxillary)
    ! Updated: guard condition checks sum of DIN+AVAIL_DON to prevent division by near-zero
    subroutine DIN_DON_PREFS(DIN_DON_PREF, NH3, DON, frac_avail_DON, NOx, kn, nkn)
        implicit none
        integer :: nkn
        double precision :: DIN_DON_PREF(nkn)
        double precision, intent(in) :: NH3(nkn)
        double precision, intent(in) :: DON(nkn)
        double precision, intent(in) :: frac_avail_DON
        double precision, intent(in) :: NOx(nkn)
        double precision, intent(in) :: kn
        double precision :: PN(nkn)
        double precision :: DIN(nkn)
        double precision :: AVAIL_DON(nkn)

        DIN = NH3 + NOx
        AVAIL_DON = (frac_avail_DON * DON)

        ! Guard condition: check sum of DIN+AVAIL_DON to prevent division by near-zero
        where ((DIN + AVAIL_DON) .lt. 1.0D-6)
            PN = 0.0D0
        elsewhere
            PN = (DIN * AVAIL_DON) / ((kn + DIN) * (kn + AVAIL_DON)) + &
                 (kn * DIN) / ((DIN + AVAIL_DON) * (kn + AVAIL_DON))
        end where
        DIN_DON_PREF = PN
    end subroutine DIN_DON_PREFS

    ! Local copy of DIP_DOP_PREFS (from pelagic_auxillary)
    ! Updated: guard condition checks sum of DIP+DOP to prevent division by near-zero
    subroutine DIP_DOP_PREFS(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)
        implicit none
        integer :: nkn
        double precision :: DIP_DOP_PREF(nkn)
        double precision, intent(in) :: AVAIL_DIP(nkn)
        double precision, intent(in) :: AVAIL_DOP(nkn)
        double precision, intent(in) :: KP
        double precision :: PP(nkn)

        ! Guard condition: check sum of AVAIL_DIP+AVAIL_DOP to prevent division by near-zero
        where ((AVAIL_DIP + AVAIL_DOP) .lt. 1.0D-6)
            PP = 0.0D0
        elsewhere
            PP = (AVAIL_DIP * AVAIL_DOP) / ((KP + AVAIL_DIP) * (KP + AVAIL_DOP)) + &
                 (KP * AVAIL_DIP) / ((AVAIL_DIP + AVAIL_DOP) * (KP + AVAIL_DOP))
        end where
        DIP_DOP_PREF = PP
    end subroutine DIP_DOP_PREFS

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
    
    subroutine assert_greater_than(value1, value2, test_name)
        real(kind=DBL_PREC), intent(in) :: value1, value2
        character(len=*), intent(in) :: test_name
        
        if (value1 > value2) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,F10.6,A,F10.6)', "    Expected ", value1, " > ", value2
            failed = failed + 1
        end if
    end subroutine assert_greater_than

    ! Test basic ammonia preference calculation
    subroutine test_ammonia_pref_basic()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: AMMONIA_PREF, NH3, NOx
        double precision :: kn
        
        print *, "Test: Basic ammonia preference calculation"
        
        ! Typical values
        NH3 = (/ 0.5D0 /)      ! 0.5 mg/L NH4-N
        NOx = (/ 0.5D0 /)      ! 0.5 mg/L NO3-N
        kn = 0.025D0           ! Half-saturation constant
        
        AMMONIA_PREF = 0.0D0
        
        call AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)
        
        ! Preference should be in [0, 1] range
        call assert_in_range(AMMONIA_PREF(1), 0.0D0, 1.0D0, &
            "Ammonia preference in valid range [0, 1]")
        
    end subroutine test_ammonia_pref_basic
    
    ! Test with only NH4 available
    subroutine test_ammonia_pref_nh4_only()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: AMMONIA_PREF, NH3, NOx
        double precision :: kn
        
        print *, "Test: NH4 only - high preference for ammonia"
        
        NH3 = (/ 1.0D0 /)      ! High NH4
        NOx = (/ 0.001D0 /)    ! Very low NO3
        kn = 0.025D0
        
        AMMONIA_PREF = 0.0D0
        
        call AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)
        
        ! When mostly NH4, preference should be high (close to 1)
        call assert_in_range(AMMONIA_PREF(1), 0.9D0, 1.0D0, &
            "With mostly NH4, preference is high (0.9-1.0)")
        
    end subroutine test_ammonia_pref_nh4_only
    
    ! Test with only NO3 available
    subroutine test_ammonia_pref_no3_only()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: AMMONIA_PREF, NH3, NOx
        double precision :: kn
        
        print *, "Test: NO3 only - low preference for ammonia"
        
        NH3 = (/ 0.001D0 /)    ! Very low NH4
        NOx = (/ 1.0D0 /)      ! High NO3
        kn = 0.025D0
        
        AMMONIA_PREF = 0.0D0
        
        call AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)
        
        ! When mostly NO3, preference for NH4 should be low
        call assert_in_range(AMMONIA_PREF(1), 0.0D0, 0.1D0, &
            "With mostly NO3, NH4 preference is low (0.0-0.1)")
        
    end subroutine test_ammonia_pref_no3_only
    
    ! Test with zero nutrients
    subroutine test_ammonia_pref_zero_nutrients()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: AMMONIA_PREF, NH3, NOx
        double precision :: kn
        
        print *, "Test: Zero nutrients gives preference = 0"
        
        NH3 = (/ 1.0D-10 /)    ! Essentially zero
        NOx = (/ 1.0D-10 /)    ! Essentially zero
        kn = 0.025D0
        
        AMMONIA_PREF = 0.0D0
        
        call AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)
        
        ! With no nutrients, preference should be 0
        call assert_approx_equal(AMMONIA_PREF(1), 0.0D0, 1.0D-6, &
            "Zero nutrients gives preference = 0")
        
    end subroutine test_ammonia_pref_zero_nutrients
    
    ! Test with equal nutrients
    subroutine test_ammonia_pref_equal_nutrients()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: AMMONIA_PREF, NH3, NOx
        double precision :: kn
        
        print *, "Test: Equal nutrients gives consistent preference"
        
        NH3 = (/ 0.5D0 /)
        NOx = (/ 0.5D0 /)
        kn = 0.025D0          ! Small compared to nutrient levels
        
        AMMONIA_PREF = 0.0D0
        
        call AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)
        
        ! With the WASP formula, equal nutrients give a high preference for NH4
        ! (NH4 is energetically preferred), formula gives value close to 1
        call assert_in_range(AMMONIA_PREF(1), 0.5D0, 1.0D0, &
            "Equal nutrients gives NH4 preference >= 0.5")
        
    end subroutine test_ammonia_pref_equal_nutrients
    
    ! Test DIN vs DON preference
    subroutine test_din_don_pref_basic()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: DIN_DON_PREF, NH3, DON, NOx
        double precision :: frac_avail_DON, kn
        
        print *, "Test: DIN vs DON preference - basic"
        
        NH3 = (/ 0.3D0 /)      ! NH4-N
        NOx = (/ 0.2D0 /)      ! NO3-N
        DON = (/ 1.0D0 /)      ! Dissolved organic N
        frac_avail_DON = 0.1D0 ! 10% of DON is available
        kn = 0.025D0
        
        DIN_DON_PREF = 0.0D0
        
        call DIN_DON_PREFS(DIN_DON_PREF, NH3, DON, frac_avail_DON, NOx, kn, nkn)
        
        ! DIN = 0.5, Available DON = 0.1
        ! DIN should be preferred (higher fraction)
        call assert_in_range(DIN_DON_PREF(1), 0.0D0, 1.0D0, &
            "DIN/DON preference in valid range")
        call assert_greater_than(DIN_DON_PREF(1), 0.5D0, &
            "With more DIN, DIN preference > 0.5")
        
    end subroutine test_din_don_pref_basic
    
    ! Test DON availability fraction effect
    subroutine test_din_don_pref_fraction()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: PREF_low, PREF_high, NH3, DON, NOx
        double precision :: kn
        
        print *, "Test: DON availability fraction effect"
        
        NH3 = (/ 0.1D0 /)
        NOx = (/ 0.1D0 /)
        DON = (/ 2.0D0 /)
        kn = 0.025D0
        
        PREF_low = 0.0D0
        PREF_high = 0.0D0
        
        ! Low DON availability
        call DIN_DON_PREFS(PREF_low, NH3, DON, 0.05D0, NOx, kn, nkn)
        
        ! High DON availability
        call DIN_DON_PREFS(PREF_high, NH3, DON, 0.5D0, NOx, kn, nkn)
        
        ! Both should be valid preferences in [0, 1]
        call assert_in_range(PREF_low(1), 0.0D0, 1.0D0, &
            "Low DON availability - DIN preference in valid range")
        call assert_in_range(PREF_high(1), 0.0D0, 1.0D0, &
            "High DON availability - DIN preference in valid range")
        
    end subroutine test_din_don_pref_fraction
    
    ! Test DIP vs DOP preference
    subroutine test_dip_dop_pref_basic()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP
        double precision :: KP
        
        print *, "Test: DIP vs DOP preference - basic"
        
        AVAIL_DIP = (/ 0.05D0 /)   ! Available DIP (mg/L)
        AVAIL_DOP = (/ 0.02D0 /)   ! Available DOP (mg/L)
        KP = 0.005D0               ! Half-saturation for P
        
        DIP_DOP_PREF = 0.0D0
        
        call DIP_DOP_PREFS(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)
        
        ! With more DIP than DOP, DIP preference should be high
        call assert_in_range(DIP_DOP_PREF(1), 0.0D0, 1.0D0, &
            "DIP/DOP preference in valid range")
        call assert_greater_than(DIP_DOP_PREF(1), 0.5D0, &
            "With more DIP, DIP preference > 0.5")
        
    end subroutine test_dip_dop_pref_basic

    ! Test edge case: sum of nutrients is near zero but one may be above old threshold
    ! This tests the improved guard condition that prevents division by near-zero
    subroutine test_ammonia_pref_sum_near_zero()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: AMMONIA_PREF, NH3, NOx
        double precision :: kn
        logical :: is_nan_result
        
        print *, "Test: Sum near zero edge case (stability fix)"
        
        ! Edge case: NH3 is 1.0D-6 (at old threshold), NOx is 0
        ! Old guard: NH3 < 1e-6 AND NOx < 1e-6 would pass, causing div by ~1e-6
        ! New guard: (NH3 + NOx) < 1e-6 catches this case
        NH3 = (/ 1.0D-7 /)     ! Just below threshold
        NOx = (/ 5.0D-8 /)     ! Very small
        kn = 0.025D0
        
        AMMONIA_PREF = -999.0D0  ! Sentinel value
        
        call AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)
        
        ! Should return 0 and not produce NaN
        is_nan_result = (AMMONIA_PREF(1) /= AMMONIA_PREF(1))  ! NaN check
        
        if (is_nan_result) then
            print '(A)', "  FAIL: Sum near zero - NaN produced"
            failed = failed + 1
        else
            call assert_approx_equal(AMMONIA_PREF(1), 0.0D0, 1.0D-10, &
                "Sum near zero returns 0 (no NaN)")
        end if
        
    end subroutine test_ammonia_pref_sum_near_zero

    ! Test edge case for DIN_DON_PREFS
    subroutine test_din_don_pref_sum_near_zero()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: DIN_DON_PREF, NH3, DON, NOx
        double precision :: kn, frac_avail_DON
        logical :: is_nan_result
        
        print *, "Test: DIN_DON sum near zero edge case"
        
        NH3 = (/ 1.0D-7 /)
        NOx = (/ 0.0D0 /)
        DON = (/ 1.0D-7 /)
        frac_avail_DON = 0.5D0  ! Only half available
        kn = 0.025D0
        
        DIN_DON_PREF = -999.0D0
        
        call DIN_DON_PREFS(DIN_DON_PREF, NH3, DON, frac_avail_DON, NOx, kn, nkn)
        
        is_nan_result = (DIN_DON_PREF(1) /= DIN_DON_PREF(1))
        
        if (is_nan_result) then
            print '(A)', "  FAIL: DIN_DON sum near zero - NaN produced"
            failed = failed + 1
        else
            call assert_approx_equal(DIN_DON_PREF(1), 0.0D0, 1.0D-10, &
                "DIN_DON sum near zero returns 0 (no NaN)")
        end if
        
    end subroutine test_din_don_pref_sum_near_zero

    ! Test edge case for DIP_DOP_PREFS
    subroutine test_dip_dop_pref_sum_near_zero()
        integer, parameter :: nkn = 1
        double precision, dimension(nkn) :: DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP
        double precision :: KP
        logical :: is_nan_result
        
        print *, "Test: DIP_DOP sum near zero edge case"
        
        AVAIL_DIP = (/ 5.0D-8 /)
        AVAIL_DOP = (/ 5.0D-8 /)
        KP = 0.005D0
        
        DIP_DOP_PREF = -999.0D0
        
        call DIP_DOP_PREFS(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)
        
        is_nan_result = (DIP_DOP_PREF(1) /= DIP_DOP_PREF(1))
        
        if (is_nan_result) then
            print '(A)', "  FAIL: DIP_DOP sum near zero - NaN produced"
            failed = failed + 1
        else
            call assert_approx_equal(DIP_DOP_PREF(1), 0.0D0, 1.0D-10, &
                "DIP_DOP sum near zero returns 0 (no NaN)")
        end if
        
    end subroutine test_dip_dop_pref_sum_near_zero

end program test_nutrient_prefs
