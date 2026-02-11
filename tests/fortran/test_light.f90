! Unit tests for light extinction and light limitation functions
! Tests light_kd and related light calculations

program test_light
    implicit none
    
    integer, parameter :: DBL_PREC = selected_real_kind(15, 307)
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "Light Extinction Unit Tests"
    print *, "=========================================="
    print *, ""
    
    ! Light extinction tests
    call test_light_kd_basic()
    call test_light_kd_low_chla()
    call test_light_kd_high_chla()
    call test_light_kd_transition()
    call test_light_kd_vector()
    call test_light_attenuation()
    call test_cur_smith_shallow_depth()  ! NEW: stability fix test
    
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
    ! Local copy of light_kd subroutine
    ! Derived from measured kd in Curonian lagoon
    !------------------------------------------------------------------
    subroutine light_kd(kdb, kd, chla, nkn)
        implicit none
        integer, intent(in) :: nkn
        double precision, intent(in) :: kdb(nkn)    ! free of chla extinction
        double precision, intent(out) :: kd(nkn)    ! total extinction
        double precision, intent(in) :: chla(nkn)
        
        where(chla <= 50.0D0)
            kd = kdb + 0.4D0 + 0.02D0*chla
        elsewhere(chla >= 50.0D0)
            kd = kdb + 1.0D0 + 0.008D0*chla
        end where
    end subroutine light_kd

    !------------------------------------------------------------------
    ! Test: Basic light extinction calculation
    !------------------------------------------------------------------
    subroutine test_light_kd_basic()
        integer, parameter :: nkn = 1
        double precision :: kdb(nkn), kd(nkn), chla(nkn)
        
        print *, "Test: Basic light extinction calculation"
        
        kdb = (/ 0.5D0 /)       ! Background extinction (1/m)
        chla = (/ 20.0D0 /)     ! 20 µg/L chlorophyll-a
        
        call light_kd(kdb, kd, chla, nkn)
        
        ! Expected: 0.5 + 0.4 + 0.02*20 = 0.5 + 0.4 + 0.4 = 1.3
        call assert_approx_equal(kd(1), 1.3D0, 0.01D0, &
            "kd = 1.3/m for kdb=0.5, chla=20")
    end subroutine test_light_kd_basic
    
    !------------------------------------------------------------------
    ! Test: Low chlorophyll regime (< 50 µg/L)
    !------------------------------------------------------------------
    subroutine test_light_kd_low_chla()
        integer, parameter :: nkn = 1
        double precision :: kdb(nkn), kd(nkn), chla(nkn)
        
        print *, "Test: Low chlorophyll regime"
        
        kdb = (/ 0.3D0 /)
        chla = (/ 10.0D0 /)     ! Low chlorophyll
        
        call light_kd(kdb, kd, chla, nkn)
        
        ! Expected: 0.3 + 0.4 + 0.02*10 = 0.9
        call assert_approx_equal(kd(1), 0.9D0, 0.01D0, &
            "Low chla formula: kdb + 0.4 + 0.02*chla")
    end subroutine test_light_kd_low_chla
    
    !------------------------------------------------------------------
    ! Test: High chlorophyll regime (>= 50 µg/L)
    !------------------------------------------------------------------
    subroutine test_light_kd_high_chla()
        integer, parameter :: nkn = 1
        double precision :: kdb(nkn), kd(nkn), chla(nkn)
        
        print *, "Test: High chlorophyll regime"
        
        kdb = (/ 0.3D0 /)
        chla = (/ 100.0D0 /)    ! High chlorophyll (bloom conditions)
        
        call light_kd(kdb, kd, chla, nkn)
        
        ! Expected: 0.3 + 1.0 + 0.008*100 = 2.1
        call assert_approx_equal(kd(1), 2.1D0, 0.01D0, &
            "High chla formula: kdb + 1.0 + 0.008*chla")
    end subroutine test_light_kd_high_chla
    
    !------------------------------------------------------------------
    ! Test: Transition at 50 µg/L
    !------------------------------------------------------------------
    subroutine test_light_kd_transition()
        integer, parameter :: nkn = 1
        double precision :: kdb(nkn), kd_at50(nkn), chla(nkn)
        double precision :: expected_low, expected_high
        
        print *, "Test: Transition at 50 µg/L chlorophyll"
        
        kdb = (/ 0.3D0 /)
        chla = (/ 50.0D0 /)
        
        call light_kd(kdb, kd_at50, chla, nkn)
        
        ! At exactly 50, both formulas apply (where condition)
        ! Low: 0.3 + 0.4 + 0.02*50 = 1.7
        ! High: 0.3 + 1.0 + 0.008*50 = 1.7
        expected_low = kdb(1) + 0.4D0 + 0.02D0*50.0D0
        expected_high = kdb(1) + 1.0D0 + 0.008D0*50.0D0
        
        ! Both should give same value at transition point
        call assert_approx_equal(expected_low, expected_high, 0.01D0, &
            "Formulas are continuous at transition")
        
        call assert_in_range(kd_at50(1), 1.6D0, 1.8D0, &
            "kd at transition is ~1.7/m")
    end subroutine test_light_kd_transition
    
    !------------------------------------------------------------------
    ! Test: Vector operation with varying chlorophyll
    !------------------------------------------------------------------
    subroutine test_light_kd_vector()
        integer, parameter :: nkn = 5
        double precision :: kdb(nkn), kd(nkn), chla(nkn)
        integer :: i
        logical :: all_positive, monotonic
        
        print *, "Test: Vector operation with chlorophyll gradient"
        
        kdb = (/ 0.3D0, 0.3D0, 0.3D0, 0.3D0, 0.3D0 /)
        chla = (/ 10.0D0, 30.0D0, 50.0D0, 70.0D0, 100.0D0 /)
        
        call light_kd(kdb, kd, chla, nkn)
        
        all_positive = .true.
        do i = 1, nkn
            if (kd(i) <= 0.0D0) all_positive = .false.
        end do
        
        if (all_positive) then
            print *, "  PASS: All extinction coefficients positive"
            passed = passed + 1
        else
            print *, "  FAIL: Some extinction coefficients not positive"
            failed = failed + 1
        end if
        
        ! Higher chlorophyll should give higher extinction
        monotonic = .true.
        do i = 2, nkn
            if (kd(i) < kd(i-1)) monotonic = .false.
        end do
        
        if (monotonic) then
            print *, "  PASS: kd increases with chlorophyll"
            passed = passed + 1
        else
            print *, "  FAIL: kd not monotonically increasing with chla"
            failed = failed + 1
        end if
    end subroutine test_light_kd_vector
    
    !------------------------------------------------------------------
    ! Test: Light attenuation with depth (Beer-Lambert)
    !------------------------------------------------------------------
    subroutine test_light_attenuation()
        integer, parameter :: nkn = 1
        double precision :: kdb(nkn), kd(nkn), chla(nkn)
        double precision :: I_surface, I_1m, I_5m, depth
        
        print *, "Test: Light attenuation with depth"
        
        kdb = (/ 0.5D0 /)
        chla = (/ 30.0D0 /)
        
        call light_kd(kdb, kd, chla, nkn)
        
        ! Beer-Lambert law: I(z) = I0 * exp(-kd * z)
        I_surface = 1000.0D0    ! Surface light (arbitrary units)
        
        depth = 1.0D0
        I_1m = I_surface * exp(-kd(1) * depth)
        
        depth = 5.0D0
        I_5m = I_surface * exp(-kd(1) * depth)
        
        ! Light should decrease with depth
        call assert_greater_than(I_surface, I_1m, &
            "Light decreases from surface to 1m")
        call assert_greater_than(I_1m, I_5m, &
            "Light decreases from 1m to 5m")
        
        ! At kd ≈ 1.5, I at 1m should be ~22% of surface
        call assert_in_range(I_1m/I_surface, 0.1D0, 0.4D0, &
            "Reasonable attenuation at 1m depth")
    end subroutine test_light_attenuation

    !------------------------------------------------------------------
    ! Local copy of CUR_SMITH with shallow depth guard (stability fix)
    !------------------------------------------------------------------
    subroutine CUR_SMITH(Ia, TCHLA, CCHLXI, GITMAX, H, ke, LLIGHT, CCHLX)
        implicit none
        
        double precision, intent(in) :: Ia       ! Light intensity
        double precision, intent(in) :: TCHLA    ! Total chlorophyll
        double precision, intent(in) :: CCHLXI   ! C:Chla ratio input
        double precision, intent(in) :: GITMAX   ! Max growth rate
        double precision, intent(in) :: H        ! Depth
        double precision, intent(in) :: ke       ! Background extinction
        double precision, intent(out) :: LLIGHT  ! Light limitation factor
        double precision, intent(out) :: CCHLX   ! C:Chla ratio output
        
        double precision :: XKC, PHIMX, FDAY, ITOT, CCHL1, KESHD, SKE
        double precision :: TEMP1, TEMP2, TEMP3, RLIGHT, IAV, IAVSG
        
        XKC   = 0.016D0
        PHIMX = 720.0D0
        FDAY  = 1.0D0
        ITOT  = Ia
        CCHL1 = CCHLXI
        KESHD = XKC * TCHLA
        SKE   = ke + KESHD
        TEMP1 = SKE * H
        
        if (GITMAX .lt. 1D-20 .or. CCHL1 .lt. 1D-20) then
            LLIGHT = 0.0D0
            CCHLX = 15.0D0
            return
        end if
        
        ! Guard against division by zero when TEMP1 is very small
        ! This can happen at very shallow depths or with zero extinction
        if (TEMP1 .lt. 1.0D-10) then
            LLIGHT = 1.0D0  ! Full light at surface
            CCHLX = 15.0D0  ! Minimum C:Chla ratio
            return
        end if
        
        TEMP2 = 0.083D0 * PHIMX * XKC / (GITMAX * CCHL1 * 2.718D0)
        TEMP3 = EXP(-TEMP1)
        
        RLIGHT = 2.7183D0 / TEMP1 * (EXP(-TEMP2 * ITOT * TEMP3) - EXP(-TEMP2 * ITOT))
        LLIGHT = RLIGHT
        
        IAV = 0.9D0 * ITOT / FDAY
        IAVSG = IAV * (1.0D0 - TEMP3) / TEMP1
        CCHLX = 0.3D0 * 0.083D0 * PHIMX * XKC * IAVSG / (GITMAX * 2.718D0)
        
        if (CCHLX .lt. 15.0D0) CCHLX = 15.0D0
    end subroutine CUR_SMITH

    !------------------------------------------------------------------
    ! Test: CUR_SMITH at very shallow depth (stability fix)
    !------------------------------------------------------------------
    subroutine test_cur_smith_shallow_depth()
        double precision :: Ia, TCHLA, CCHLXI, GITMAX, H, ke
        double precision :: LLIGHT, CCHLX
        logical :: is_nan
        
        print *, "Test: CUR_SMITH shallow depth edge case (stability fix)"
        
        ! Edge case: very shallow depth where TEMP1 = ke * H is near zero
        Ia = 500.0D0        ! Light intensity
        TCHLA = 10.0D0      ! Chlorophyll (µg/L)
        CCHLXI = 50.0D0     ! C:Chla ratio
        GITMAX = 2.0D0      ! Max growth rate
        H = 1.0D-12         ! Extremely shallow depth (near zero)
        ke = 0.5D0          ! Background extinction
        
        LLIGHT = -999.0D0   ! Sentinel
        CCHLX = -999.0D0
        
        call CUR_SMITH(Ia, TCHLA, CCHLXI, GITMAX, H, ke, LLIGHT, CCHLX)
        
        ! Check for NaN
        is_nan = (LLIGHT /= LLIGHT) .or. (CCHLX /= CCHLX)
        
        if (is_nan) then
            print *, "  FAIL: Shallow depth - NaN produced"
            failed = failed + 1
        else
            ! With new guard, should return LLIGHT = 1.0 at very shallow depth
            call assert_approx_equal(LLIGHT, 1.0D0, 0.01D0, &
                "Shallow depth returns LLIGHT = 1.0 (full light)")
            call assert_approx_equal(CCHLX, 15.0D0, 0.01D0, &
                "Shallow depth returns minimum CCHLX = 15")
        end if
        
        ! Also test with zero extinction
        H = 1.0D0
        ke = 0.0D0
        TCHLA = 0.0D0       ! No chlorophyll, so total extinction = 0
        
        call CUR_SMITH(Ia, TCHLA, CCHLXI, GITMAX, H, ke, LLIGHT, CCHLX)
        
        is_nan = (LLIGHT /= LLIGHT) .or. (CCHLX /= CCHLX)
        
        if (is_nan) then
            print *, "  FAIL: Zero extinction - NaN produced"
            failed = failed + 1
        else
            call assert_approx_equal(LLIGHT, 1.0D0, 0.01D0, &
                "Zero extinction returns LLIGHT = 1.0 (full light)")
        end if
        
    end subroutine test_cur_smith_shallow_depth

end program test_light
