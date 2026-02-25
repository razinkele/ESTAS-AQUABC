! Test program for sediment bioturbation module
! Tests all bioturbation functions: depth attenuation, O2 scaling,
! seasonal variability, bioirrigation, and the combined effective Db.
!
! Compile with:
!   gfortran -O2 -Wall -fcheck=all -o test_bioturbation \
!     ../../SOURCE_CODE/CORE_UTILS/precision_kinds.f90 \
!     ../../SOURCE_CODE/AQUABC/SEDIMENTS/aquabc_II_sediment_bioturbation.f90 \
!     test_bioturbation.f90
!
! Run: ./test_bioturbation

program test_bioturbation
    use precision_kinds, only: DBL_PREC
    use AQUABC_SEDIMENT_BIOTURBATION
    implicit none

    integer :: num_passed, num_failed, num_tests

    num_passed = 0
    num_failed = 0
    num_tests = 0

    write(*,*) '=============================================='
    write(*,*) 'Sediment Bioturbation Test Suite'
    write(*,*) '=============================================='
    write(*,*)

    ! Depth attenuation tests
    call test_depth_atten_surface(num_passed, num_failed, num_tests)
    call test_depth_atten_decay(num_passed, num_failed, num_tests)
    call test_depth_atten_deep(num_passed, num_failed, num_tests)
    call test_depth_atten_zero_zmix(num_passed, num_failed, num_tests)

    ! O2 scaling tests
    call test_o2_scaling_high_o2(num_passed, num_failed, num_tests)
    call test_o2_scaling_half_sat(num_passed, num_failed, num_tests)
    call test_o2_scaling_anoxic(num_passed, num_failed, num_tests)
    call test_o2_scaling_negative(num_passed, num_failed, num_tests)

    ! Seasonal factor tests
    call test_seasonal_peak_day(num_passed, num_failed, num_tests)
    call test_seasonal_minimum(num_passed, num_failed, num_tests)
    call test_seasonal_no_amplitude(num_passed, num_failed, num_tests)
    call test_seasonal_clamp_positive(num_passed, num_failed, num_tests)

    ! Effective Db tests
    call test_effective_db_combined(num_passed, num_failed, num_tests)
    call test_effective_db_anoxic(num_passed, num_failed, num_tests)
    call test_effective_db_deep_layer(num_passed, num_failed, num_tests)

    ! Bioirrigation tests
    call test_bioirrig_surface(num_passed, num_failed, num_tests)
    call test_bioirrig_deep(num_passed, num_failed, num_tests)
    call test_bioirrig_anoxic(num_passed, num_failed, num_tests)
    call test_bioirrig_always_ge_one(num_passed, num_failed, num_tests)

    ! APPLY_BIOTURBATION_COEFFS integration test
    call test_apply_coeffs_integration(num_passed, num_failed, num_tests)

    ! Last layer boundary condition test
    call test_last_layer_bc(num_passed, num_failed, num_tests)

    ! Monotonicity tests
    call test_depth_monotonic_decrease(num_passed, num_failed, num_tests)
    call test_o2_monotonic_increase(num_passed, num_failed, num_tests)

    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,'(A,I3,A)')      ' Total:   ', num_tests, ' tests'
    write(*,*) '=============================================='

    if (num_failed > 0) then
        stop 1
    end if

contains

    ! ================================================================
    ! Helper: check if two values are approximately equal
    ! ================================================================
    logical function approx_eq(a, b, tol)
        real(kind=DBL_PREC), intent(in) :: a, b, tol
        approx_eq = abs(a - b) < tol
    end function approx_eq

    ! ================================================================
    ! DEPTH ATTENUATION TESTS
    ! ================================================================

    subroutine test_depth_atten_surface(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: Db

        total = total + 1
        write(*,*) 'Test: Depth attenuation at surface (z=0) returns Db0'

        Db = BIOTURB_DEPTH_ATTEN(1.0D-4, 0.0D0, 0.05D0)

        if (approx_eq(Db, 1.0D-4, 1.0D-15)) then
            write(*,*) '  PASSED: Db =', Db
            passed = passed + 1
        else
            write(*,*) '  FAILED: Db =', Db, '(expected 1.0D-4)'
            failed = failed + 1
        end if
    end subroutine test_depth_atten_surface

    subroutine test_depth_atten_decay(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: Db, expected

        total = total + 1
        write(*,*) 'Test: Depth attenuation at z_mix depth = Db0*exp(-1)'

        ! At z = z_mix, Db = Db0 * exp(-1)
        Db = BIOTURB_DEPTH_ATTEN(1.0D-4, 0.05D0, 0.05D0)
        expected = 1.0D-4 * exp(-1.0D0)

        if (approx_eq(Db, expected, 1.0D-15)) then
            write(*,*) '  PASSED: Db =', Db
            passed = passed + 1
        else
            write(*,*) '  FAILED: Db =', Db, '(expected', expected, ')'
            failed = failed + 1
        end if
    end subroutine test_depth_atten_decay

    subroutine test_depth_atten_deep(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: Db

        total = total + 1
        write(*,*) 'Test: Depth attenuation at 5*z_mix is very small'

        ! At z = 5*z_mix, Db = Db0 * exp(-5) ≈ 0.67% of Db0
        Db = BIOTURB_DEPTH_ATTEN(1.0D-4, 0.25D0, 0.05D0)

        if (Db < 0.01D0 * 1.0D-4 .and. Db > 0.0D0) then
            write(*,*) '  PASSED: Db =', Db, '(< 1% of Db0)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Db =', Db
            failed = failed + 1
        end if
    end subroutine test_depth_atten_deep

    subroutine test_depth_atten_zero_zmix(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: Db

        total = total + 1
        write(*,*) 'Test: Depth attenuation with near-zero z_mix doesnt crash'

        ! z_mix = 0 should be guarded, not NaN/Inf
        Db = BIOTURB_DEPTH_ATTEN(1.0D-4, 0.01D0, 0.0D0)

        if (Db >= 0.0D0 .and. Db == Db) then  ! finite and non-negative
            write(*,*) '  PASSED: Db =', Db, '(finite, non-negative)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Db =', Db, '(NaN or negative)'
            failed = failed + 1
        end if
    end subroutine test_depth_atten_zero_zmix

    ! ================================================================
    ! O2 SCALING TESTS
    ! ================================================================

    subroutine test_o2_scaling_high_o2(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f

        total = total + 1
        write(*,*) 'Test: O2 scaling approaches 1.0 at high O2'

        f = BIOTURB_O2_SCALING(10.0D0, 2.0D0)  ! 10 mg/L >> KHS=2

        if (f > 0.8D0 .and. f < 1.0D0) then
            write(*,*) '  PASSED: f(O2) =', f
            passed = passed + 1
        else
            write(*,*) '  FAILED: f(O2) =', f
            failed = failed + 1
        end if
    end subroutine test_o2_scaling_high_o2

    subroutine test_o2_scaling_half_sat(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f

        total = total + 1
        write(*,*) 'Test: O2 scaling = 0.5 at O2 = KHS'

        f = BIOTURB_O2_SCALING(2.0D0, 2.0D0)  ! O2 = KHS

        if (approx_eq(f, 0.5D0, 1.0D-10)) then
            write(*,*) '  PASSED: f(O2) =', f
            passed = passed + 1
        else
            write(*,*) '  FAILED: f(O2) =', f, '(expected 0.5)'
            failed = failed + 1
        end if
    end subroutine test_o2_scaling_half_sat

    subroutine test_o2_scaling_anoxic(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f

        total = total + 1
        write(*,*) 'Test: O2 scaling = 0.0 when O2 = 0'

        f = BIOTURB_O2_SCALING(0.0D0, 2.0D0)

        if (approx_eq(f, 0.0D0, 1.0D-20)) then
            write(*,*) '  PASSED: f(O2) =', f
            passed = passed + 1
        else
            write(*,*) '  FAILED: f(O2) =', f, '(expected 0.0)'
            failed = failed + 1
        end if
    end subroutine test_o2_scaling_anoxic

    subroutine test_o2_scaling_negative(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f

        total = total + 1
        write(*,*) 'Test: O2 scaling handles negative O2 gracefully'

        f = BIOTURB_O2_SCALING(-1.0D0, 2.0D0)

        if (approx_eq(f, 0.0D0, 1.0D-20)) then
            write(*,*) '  PASSED: f(O2) =', f, '(clamped to 0)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: f(O2) =', f, '(expected 0.0)'
            failed = failed + 1
        end if
    end subroutine test_o2_scaling_negative

    ! ================================================================
    ! SEASONAL FACTOR TESTS
    ! ================================================================

    subroutine test_seasonal_peak_day(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f

        total = total + 1
        write(*,*) 'Test: Seasonal factor = 1+amplitude at peak day'

        ! At day_of_year = day_peak, cos(0) = 1, so f = 1 + amplitude
        f = BIOTURB_SEASONAL_FACTOR(200.0D0, 0.5D0, 200.0D0)

        if (approx_eq(f, 1.5D0, 1.0D-10)) then
            write(*,*) '  PASSED: f_season =', f
            passed = passed + 1
        else
            write(*,*) '  FAILED: f_season =', f, '(expected 1.5)'
            failed = failed + 1
        end if
    end subroutine test_seasonal_peak_day

    subroutine test_seasonal_minimum(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f

        total = total + 1
        write(*,*) 'Test: Seasonal factor = 1-amplitude 182.5 days from peak'

        ! At 182.5 days from peak (half year), cos(pi) = -1, so f = 1 - amplitude
        f = BIOTURB_SEASONAL_FACTOR(200.0D0 + 182.5D0, 0.5D0, 200.0D0)

        if (approx_eq(f, 0.5D0, 1.0D-6)) then
            write(*,*) '  PASSED: f_season =', f
            passed = passed + 1
        else
            write(*,*) '  FAILED: f_season =', f, '(expected 0.5)'
            failed = failed + 1
        end if
    end subroutine test_seasonal_minimum

    subroutine test_seasonal_no_amplitude(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f

        total = total + 1
        write(*,*) 'Test: Seasonal factor = 1.0 when amplitude = 0'

        f = BIOTURB_SEASONAL_FACTOR(100.0D0, 0.0D0, 200.0D0)

        if (approx_eq(f, 1.0D0, 1.0D-15)) then
            write(*,*) '  PASSED: f_season =', f
            passed = passed + 1
        else
            write(*,*) '  FAILED: f_season =', f, '(expected 1.0)'
            failed = failed + 1
        end if
    end subroutine test_seasonal_no_amplitude

    subroutine test_seasonal_clamp_positive(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f

        total = total + 1
        write(*,*) 'Test: Seasonal factor clamped >= 0 for extreme amplitude'

        ! amplitude = 2.0, at minimum would give 1 - 2 = -1, should be clamped to 0
        f = BIOTURB_SEASONAL_FACTOR(200.0D0 + 182.5D0, 2.0D0, 200.0D0)

        if (f >= 0.0D0) then
            write(*,*) '  PASSED: f_season =', f, '(>= 0)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: f_season =', f, '(negative!)'
            failed = failed + 1
        end if
    end subroutine test_seasonal_clamp_positive

    ! ================================================================
    ! EFFECTIVE Db TESTS
    ! ================================================================

    subroutine test_effective_db_combined(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: Db_eff, expected

        total = total + 1
        write(*,*) 'Test: Effective Db combines all factors correctly'

        ! Surface (z=0), high O2 (10 mg/L, KHS=2), peak day, amp=0.3
        ! Expected: Db0 * 1.0 (depth) * 10/(10+2) (O2) * 1.3 (season)
        Db_eff = BIOTURB_EFFECTIVE_DB(1.0D-4, 0.0D0, 0.05D0, &
                                       10.0D0, 2.0D0, &
                                       200.0D0, 0.3D0, 200.0D0)
        expected = 1.0D-4 * (10.0D0/12.0D0) * 1.3D0

        if (approx_eq(Db_eff, expected, 1.0D-12)) then
            write(*,*) '  PASSED: Db_eff =', Db_eff
            passed = passed + 1
        else
            write(*,*) '  FAILED: Db_eff =', Db_eff, '(expected', expected, ')'
            failed = failed + 1
        end if
    end subroutine test_effective_db_combined

    subroutine test_effective_db_anoxic(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: Db_eff

        total = total + 1
        write(*,*) 'Test: Effective Db = 0 under anoxic conditions'

        Db_eff = BIOTURB_EFFECTIVE_DB(1.0D-4, 0.0D0, 0.05D0, &
                                       0.0D0, 2.0D0, &
                                       200.0D0, 0.3D0, 200.0D0)

        if (approx_eq(Db_eff, 0.0D0, 1.0D-20)) then
            write(*,*) '  PASSED: Db_eff =', Db_eff
            passed = passed + 1
        else
            write(*,*) '  FAILED: Db_eff =', Db_eff, '(expected 0.0)'
            failed = failed + 1
        end if
    end subroutine test_effective_db_anoxic

    subroutine test_effective_db_deep_layer(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: Db_surface, Db_deep

        total = total + 1
        write(*,*) 'Test: Effective Db at depth < surface Db'

        Db_surface = BIOTURB_EFFECTIVE_DB(1.0D-4, 0.0D0, 0.05D0, &
                                           8.0D0, 2.0D0, &
                                           200.0D0, 0.0D0, 200.0D0)
        Db_deep = BIOTURB_EFFECTIVE_DB(1.0D-4, 0.10D0, 0.05D0, &
                                        8.0D0, 2.0D0, &
                                        200.0D0, 0.0D0, 200.0D0)

        if (Db_deep < Db_surface .and. Db_deep > 0.0D0) then
            write(*,*) '  PASSED: Db_deep =', Db_deep, '< Db_surface =', Db_surface
            passed = passed + 1
        else
            write(*,*) '  FAILED: Db_deep =', Db_deep, 'Db_surface =', Db_surface
            failed = failed + 1
        end if
    end subroutine test_effective_db_deep_layer

    ! ================================================================
    ! BIOIRRIGATION TESTS
    ! ================================================================

    subroutine test_bioirrig_surface(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: alpha, expected

        total = total + 1
        write(*,*) 'Test: Bioirrigation factor at surface with good O2'

        ! z=0, alpha0=3, z_irr=0.04, O2=10, KHS=2, peak day, amp=0.0
        ! Expected: 1 + 3 * exp(0) * 10/12 * 1.0 = 1 + 3*0.8333 = 3.5
        alpha = BIOIRRIGATION_FACTOR(3.0D0, 0.0D0, 0.04D0, &
                                      10.0D0, 2.0D0, &
                                      200.0D0, 0.0D0, 200.0D0)
        expected = 1.0D0 + 3.0D0 * (10.0D0/12.0D0)

        if (approx_eq(alpha, expected, 1.0D-10)) then
            write(*,*) '  PASSED: alpha =', alpha
            passed = passed + 1
        else
            write(*,*) '  FAILED: alpha =', alpha, '(expected', expected, ')'
            failed = failed + 1
        end if
    end subroutine test_bioirrig_surface

    subroutine test_bioirrig_deep(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: alpha

        total = total + 1
        write(*,*) 'Test: Bioirrigation factor approaches 1.0 at depth'

        ! At z = 5*z_irr = 0.20, exp(-5) ≈ 0.0067, so alpha ≈ 1.0
        alpha = BIOIRRIGATION_FACTOR(3.0D0, 0.20D0, 0.04D0, &
                                      10.0D0, 2.0D0, &
                                      200.0D0, 0.0D0, 200.0D0)

        if (alpha > 1.0D0 .and. alpha < 1.05D0) then
            write(*,*) '  PASSED: alpha =', alpha, '(close to 1.0 at depth)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: alpha =', alpha
            failed = failed + 1
        end if
    end subroutine test_bioirrig_deep

    subroutine test_bioirrig_anoxic(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: alpha

        total = total + 1
        write(*,*) 'Test: Bioirrigation factor = 1.0 under anoxia'

        alpha = BIOIRRIGATION_FACTOR(3.0D0, 0.0D0, 0.04D0, &
                                      0.0D0, 2.0D0, &
                                      200.0D0, 0.0D0, 200.0D0)

        if (approx_eq(alpha, 1.0D0, 1.0D-15)) then
            write(*,*) '  PASSED: alpha =', alpha, '(no irrigation under anoxia)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: alpha =', alpha, '(expected 1.0)'
            failed = failed + 1
        end if
    end subroutine test_bioirrig_anoxic

    subroutine test_bioirrig_always_ge_one(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: alpha
        integer :: i_doy
        logical :: all_ge_one

        total = total + 1
        write(*,*) 'Test: Bioirrigation factor >= 1.0 for all conditions'

        all_ge_one = .true.
        do i_doy = 1, 365
            alpha = BIOIRRIGATION_FACTOR(5.0D0, 0.0D0, 0.04D0, &
                                          10.0D0, 2.0D0, &
                                          real(i_doy, DBL_PREC), 0.9D0, 200.0D0)
            if (alpha < 1.0D0) then
                all_ge_one = .false.
                exit
            end if
        end do

        if (all_ge_one) then
            write(*,*) '  PASSED: alpha >= 1.0 for all days of year'
            passed = passed + 1
        else
            write(*,*) '  FAILED: alpha < 1.0 found at day', i_doy
            failed = failed + 1
        end if
    end subroutine test_bioirrig_always_ge_one

    ! ================================================================
    ! APPLY_BIOTURBATION_COEFFS INTEGRATION TEST
    ! ================================================================

    subroutine test_apply_coeffs_integration(passed, failed, total)
        integer, intent(inout) :: passed, failed, total

        integer, parameter :: nkn_t = 2, nl = 3, nv = 4
        real(kind=DBL_PREC) :: DEPTHS(nkn_t, nl), DOXY(nkn_t, nl)
        real(kind=DBL_PREC) :: PMC(nkn_t, nl, nv), IRR(nkn_t, nl)
        logical :: ok

        total = total + 1
        write(*,*) 'Test: APPLY_BIOTURBATION_COEFFS populates arrays correctly'

        ! Set up layer depths
        DEPTHS(:, 1) = 0.005D0  ! 0.5 cm
        DEPTHS(:, 2) = 0.010D0  ! 1.0 cm
        DEPTHS(:, 3) = 0.020D0  ! 2.0 cm

        ! Set dissolved oxygen per layer
        DOXY(:, 1) = 8.0D0   ! well-oxygenated
        DOXY(:, 2) = 4.0D0   ! moderate
        DOXY(:, 3) = 0.5D0   ! near-anoxic

        call APPLY_BIOTURBATION_COEFFS( &
            nkn_t, nl, nv, &
            2.64D-5, 0.05D0, 2.0D0, &  ! Db0, z_mix, KHS_O2
            3.0D0, 0.04D0, &            ! alpha0, z_irr
            0.3D0, 200.0D0, &           ! seasonal_amp, day_peak
            DEPTHS, DOXY, &
            200.0D0, &                   ! day_of_year = peak
            PMC, IRR)

        ok = .true.

        ! Check: layer 1 Db > layer 2 Db > layer 3 Db (depth decay + lower O2)
        if (PMC(1,1,1) <= PMC(1,2,1) .or. PMC(1,2,1) <= PMC(1,3,1)) then
            write(*,*) '  FAILED: Db should decrease with depth'
            write(*,*) '    Layer1:', PMC(1,1,1), ' Layer2:', PMC(1,2,1), ' Layer3:', PMC(1,3,1)
            ok = .false.
        end if

        ! Check: irrigation factor decreases with depth
        if (IRR(1,1) <= IRR(1,2) .or. IRR(1,2) <= IRR(1,3)) then
            write(*,*) '  FAILED: Irrigation should decrease with depth'
            write(*,*) '    Layer1:', IRR(1,1), ' Layer2:', IRR(1,2), ' Layer3:', IRR(1,3)
            ok = .false.
        end if

        ! Check: all values are positive and finite
        if (any(PMC < 0.0D0) .or. any(IRR < 1.0D0)) then
            write(*,*) '  FAILED: Negative Db or irrigation < 1'
            ok = .false.
        end if

        ! Check: both nodes have same values (same inputs)
        if (abs(PMC(1,1,1) - PMC(2,1,1)) > 1.0D-20) then
            write(*,*) '  FAILED: Node 1 and 2 should have same Db'
            ok = .false.
        end if

        ! Check: all variables get the same Db per layer
        if (abs(PMC(1,1,1) - PMC(1,1,nv)) > 1.0D-20) then
            write(*,*) '  FAILED: All variables should get the same Db per layer'
            ok = .false.
        end if

        if (ok) then
            write(*,*) '  PASSED: Arrays populated correctly'
            write(*,*) '    Db L1:', PMC(1,1,1), ' L2:', PMC(1,2,1), ' L3:', PMC(1,3,1)
            write(*,*) '    Irr L1:', IRR(1,1), ' L2:', IRR(1,2), ' L3:', IRR(1,3)
            passed = passed + 1
        else
            failed = failed + 1
        end if
    end subroutine test_apply_coeffs_integration

    ! ================================================================
    ! LAST LAYER BOUNDARY CONDITION TEST
    ! ================================================================

    subroutine test_last_layer_bc(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: rate, deriv, expected

        total = total + 1
        write(*,*) 'Test: Last layer BC gives -PART_MIXING_RATE(N)'

        rate = 1.5D-3
        deriv = BIOTURB_LAST_LAYER_MIXING_RATE(rate)
        expected = -rate

        if (approx_eq(deriv, expected, 1.0D-15)) then
            write(*,*) '  PASSED: deriv =', deriv
            passed = passed + 1
        else
            write(*,*) '  FAILED: deriv =', deriv, '(expected', expected, ')'
            failed = failed + 1
        end if
    end subroutine test_last_layer_bc

    ! ================================================================
    ! MONOTONICITY TESTS
    ! ================================================================

    subroutine test_depth_monotonic_decrease(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: Db_prev, Db_curr
        integer :: i_depth
        logical :: ok

        total = total + 1
        write(*,*) 'Test: Db decreases monotonically with depth'

        ok = .true.
        Db_prev = BIOTURB_DEPTH_ATTEN(1.0D-4, 0.0D0, 0.05D0)

        do i_depth = 1, 20
            Db_curr = BIOTURB_DEPTH_ATTEN(1.0D-4, &
                real(i_depth, DBL_PREC) * 0.01D0, 0.05D0)
            if (Db_curr >= Db_prev) then
                ok = .false.
                exit
            end if
            Db_prev = Db_curr
        end do

        if (ok) then
            write(*,*) '  PASSED: Db monotonically decreasing over 20 cm'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Non-monotonic at depth', i_depth * 0.01
            failed = failed + 1
        end if
    end subroutine test_depth_monotonic_decrease

    subroutine test_o2_monotonic_increase(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind=DBL_PREC) :: f_prev, f_curr
        integer :: i_o2
        logical :: ok

        total = total + 1
        write(*,*) 'Test: O2 scaling increases monotonically with O2'

        ok = .true.
        f_prev = BIOTURB_O2_SCALING(0.0D0, 2.0D0)

        do i_o2 = 1, 20
            f_curr = BIOTURB_O2_SCALING(real(i_o2, DBL_PREC), 2.0D0)
            if (f_curr <= f_prev) then
                ok = .false.
                exit
            end if
            f_prev = f_curr
        end do

        if (ok) then
            write(*,*) '  PASSED: O2 scaling monotonically increasing'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Non-monotonic at O2 =', i_o2
            failed = failed + 1
        end if
    end subroutine test_o2_monotonic_increase

end program test_bioturbation
