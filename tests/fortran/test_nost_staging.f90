! Unit tests for the AQUABC_NOST_STAGING module (bed akinete bank, EMA formation
! cue, germination/formation latch) -- spec docs/superpowers/specs/2026-08-23-
! nost-akinete-staging-design.md secs 4.1-4.4. Pure per-box state + arithmetic;
! nothing else in the model calls this module yet, so this is its only exercise.
program test_nost_staging
    use AQUABC_II_GLOBAL
    use AQUABC_NOST_STAGING
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "AQUABC_NOST_STAGING Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_case1_mass_balance()
    call test_case2_conservation_identity()
    call test_case3_rad_ema()
    call test_case4_latch_on()
    call test_case5_latch_off()
    call test_case6_non_negativity()
    call test_case8_bed_identity()
    call test_case9_cum_form_no_bed_effect()
    call test_case7_params_override()      ! last: mutates module scalars

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

    subroutine assert_approx_equal(value, expected, tol, test_name)
        real(kind=DBL_PREC), intent(in) :: value, expected, tol
        character(len=*), intent(in) :: test_name

        if (abs(value - expected) <= tol) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E16.9,A,E16.9)', "    Got: ", value, " Expected: ", expected
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
            print '(A,A,E16.9)', "  FAIL: ", test_name, value
            failed = failed + 1
        end if
    end subroutine assert_non_negative

    ! Case 1: mass balance -- n=1, BED=10, f_settle=2, f_germ=1, dt=0.5
    ! -> BED = 10 + (2-1-0.001*10)*0.5 = 10.495; BURIED = 0.005
    subroutine test_case1_mass_balance()
        integer, parameter :: n = 1
        real(kind=DBL_PREC) :: solar(n), fset(n), fgerm(n), fform(n)
        logical :: gcond(n)

        print *, "Test: Case 1 -- mass balance"

        call ENSURE_NOST_STAGING_STATE(n)
        call RESET_NOST_STAGING_STATE()

        ! ENSURE/RESET contract: zeroed state, latch off, RAD_EMA sentinel.
        call assert_approx_equal(BED_AKI(1), 0.0D0, 1.0D-14, "case1: RESET zeroes BED_AKI")
        call assert_true(.not. FORM_LATCH(1), "case1: RESET clears FORM_LATCH")
        call assert_approx_equal(RAD_EMA(1), -1.0D0, 1.0D-14, "case1: RESET sets RAD_EMA sentinel")

        BED_AKI(1) = 10.0D0
        solar = 200.0D0; fset = 2.0D0; fgerm = 1.0D0; fform = 0.0D0; gcond = .false.

        call ADVANCE_NOST_STAGING(n, 0.5D0, solar, fset, fgerm, fform, gcond)

        call assert_approx_equal(BED_AKI(1), 10.495D0, 1.0D-12, "case1: BED_AKI mass balance")
        call assert_approx_equal(BURIED_AKI(1), 0.005D0, 1.0D-12, "case1: BURIED_AKI mass balance")
    end subroutine test_case1_mass_balance

    ! Case 2: conservation identity -- delta(BED) + delta(BURIED) ==
    ! (f_settle - f_germ)*dt, to 1e-12.
    subroutine test_case2_conservation_identity()
        integer, parameter :: n = 1
        real(kind=DBL_PREC) :: solar(n), fset(n), fgerm(n), fform(n)
        logical :: gcond(n)
        real(kind=DBL_PREC) :: bed0, buried0, lhs, rhs
        real(kind=DBL_PREC), parameter :: dt = 0.5D0

        print *, "Test: Case 2 -- conservation identity"

        call ENSURE_NOST_STAGING_STATE(n)
        call RESET_NOST_STAGING_STATE()

        BED_AKI(1) = 10.0D0
        bed0 = BED_AKI(1); buried0 = BURIED_AKI(1)
        solar = 200.0D0; fset = 2.0D0; fgerm = 1.0D0; fform = 0.0D0; gcond = .false.

        call ADVANCE_NOST_STAGING(n, dt, solar, fset, fgerm, fform, gcond)

        lhs = (BED_AKI(1) - bed0) + (BURIED_AKI(1) - buried0)
        rhs = (fset(1) - fgerm(1)) * dt
        call assert_approx_equal(lhs, rhs, 1.0D-12, "case2: delta(BED)+delta(BURIED) == (f_settle-f_germ)*dt")
    end subroutine test_case2_conservation_identity

    ! Case 3: RAD_EMA -- first call sets RAD_EMA = solar exactly; second call
    ! with solar=0, dt=1 -> RAD_EMA = 120*(6/7) (a partial, not full, step).
    subroutine test_case3_rad_ema()
        integer, parameter :: n = 1
        real(kind=DBL_PREC) :: solar(n), fset(n), fgerm(n), fform(n)
        logical :: gcond(n)
        real(kind=DBL_PREC) :: expected

        print *, "Test: Case 3 -- RAD_EMA first-call init and one-step decay"

        call ENSURE_NOST_STAGING_STATE(n)
        call RESET_NOST_STAGING_STATE()

        fset = 0.0D0; fgerm = 0.0D0; fform = 0.0D0; gcond = .false.

        solar = 120.0D0
        call ADVANCE_NOST_STAGING(n, 1.0D0, solar, fset, fgerm, fform, gcond)
        call assert_approx_equal(RAD_EMA(1), 120.0D0, 1.0D-12, "case3: first call sets RAD_EMA = solar exactly")

        solar = 0.0D0
        call ADVANCE_NOST_STAGING(n, 1.0D0, solar, fset, fgerm, fform, gcond)
        expected = 120.0D0 + (1.0D0 / 7.0D0) * (0.0D0 - 120.0D0)
        call assert_approx_equal(RAD_EMA(1), expected, 1.0D-12, "case3: one-day EMA step toward 0")
    end subroutine test_case3_rad_ema

    ! Case 4: latch ON -- RAD_EMA driven below I_FORM_AKI -> FORM_LATCH .true.;
    ! stays ON while germ_cond=.false. even if RAD_EMA rises again.
    subroutine test_case4_latch_on()
        integer, parameter :: n = 1
        real(kind=DBL_PREC) :: solar(n), fset(n), fgerm(n), fform(n)
        logical :: gcond(n)
        integer :: i

        print *, "Test: Case 4 -- formation latch turns ON and holds"

        call ENSURE_NOST_STAGING_STATE(n)
        call RESET_NOST_STAGING_STATE()

        fset = 0.0D0; fgerm = 0.0D0; fform = 0.0D0; gcond = .false.

        ! Init RAD_EMA well above I_FORM_AKI (120): latch must start/stay off.
        solar = 200.0D0
        call ADVANCE_NOST_STAGING(n, 1.0D0, solar, fset, fgerm, fform, gcond)
        call assert_true(.not. FORM_LATCH(1), "case4: latch off while RAD_EMA above threshold")

        ! Drive RAD_EMA down with zero solar input until it crosses I_FORM_AKI.
        solar = 0.0D0
        do i = 1, 10
            call ADVANCE_NOST_STAGING(n, 1.0D0, solar, fset, fgerm, fform, gcond)
        end do
        call assert_true(RAD_EMA(1) < I_FORM_AKI, "case4: RAD_EMA driven below I_FORM_AKI")
        call assert_true(FORM_LATCH(1), "case4: latch turns ON once RAD_EMA < I_FORM_AKI")

        ! Solar rises again but germ_cond stays false: latch must hold ON.
        solar = 500.0D0
        do i = 1, 5
            call ADVANCE_NOST_STAGING(n, 1.0D0, solar, fset, fgerm, fform, gcond)
        end do
        call assert_true(RAD_EMA(1) > I_FORM_AKI, "case4: RAD_EMA has risen back above threshold")
        call assert_true(FORM_LATCH(1), "case4: latch stays ON while germ_cond=.false.")
    end subroutine test_case4_latch_on

    ! Case 5: latch OFF -- germ_cond=.true. on the next ADVANCE call flips an
    ! ON latch to .false. (spring release, spec sec 4.3).
    subroutine test_case5_latch_off()
        integer, parameter :: n = 1
        real(kind=DBL_PREC) :: solar(n), fset(n), fgerm(n), fform(n)
        logical :: gcond(n)

        print *, "Test: Case 5 -- formation latch releases on germ_cond"

        call ENSURE_NOST_STAGING_STATE(n)
        call RESET_NOST_STAGING_STATE()

        fset = 0.0D0; fgerm = 0.0D0; fform = 0.0D0
        FORM_LATCH(1) = .true.
        RAD_EMA(1) = 50.0D0        ! still below I_FORM_AKI: latch would stay ON on its own

        gcond = .true.
        solar = 50.0D0
        call ADVANCE_NOST_STAGING(n, 1.0D0, solar, fset, fgerm, fform, gcond)

        call assert_true(.not. FORM_LATCH(1), "case5: germ_cond=.true. releases an ON latch")
    end subroutine test_case5_latch_off

    ! Case 6: non-negativity -- BED=1e-6, f_germ=KR_GERM_BED*BED (recomputed
    ! from the current state each step, as a real caller would), 240 steps of
    ! dt=1/240 -> BED stays >= 0 throughout (no positivity clamp in ADVANCE).
    subroutine test_case6_non_negativity()
        integer, parameter :: n = 1
        real(kind=DBL_PREC) :: solar(n), fset(n), fgerm(n), fform(n)
        logical :: gcond(n)
        real(kind=DBL_PREC), parameter :: dt = 1.0D0 / 240.0D0
        integer :: i
        logical :: stayed_non_negative

        print *, "Test: Case 6 -- non-negativity under germination-only drain"

        call ENSURE_NOST_STAGING_STATE(n)
        call RESET_NOST_STAGING_STATE()

        BED_AKI(1) = 1.0D-6
        fset = 0.0D0; fform = 0.0D0; solar = 200.0D0; gcond = .false.
        stayed_non_negative = .true.

        do i = 1, 240
            fgerm(1) = KR_GERM_BED * BED_AKI(1)
            call ADVANCE_NOST_STAGING(n, dt, solar, fset, fgerm, fform, gcond)
            if (BED_AKI(1) < 0.0D0) stayed_non_negative = .false.
        end do

        call assert_true(stayed_non_negative, "case6: BED_AKI stays non-negative over 240 steps")
        call assert_non_negative(BED_AKI(1), "case6: BED_AKI non-negative after 240 steps")
    end subroutine test_case6_non_negativity

    ! Case 8: exact bed identity after N random-flux ADVANCE calls --
    ! BED - BED0 == CUM_SETTLE - CUM_GERM - BURIED, to 1e-12 (the V4 identity).
    subroutine test_case8_bed_identity()
        integer, parameter :: n = 1
        real(kind=DBL_PREC) :: solar(n), fset(n), fgerm(n), fform(n)
        logical :: gcond(n)
        real(kind=DBL_PREC) :: bed0, lhs, rhs, r
        integer :: i

        print *, "Test: Case 8 -- exact bed identity (V4 audit)"

        call ENSURE_NOST_STAGING_STATE(n)
        call RESET_NOST_STAGING_STATE()

        BED_AKI(1) = 5.0D0
        bed0 = BED_AKI(1)

        do i = 1, 50
            call random_number(r); fset(1) = r * 3.0D0
            call random_number(r); fgerm(1) = r * 2.0D0
            call random_number(r); fform(1) = r * 1.5D0
            call random_number(r); solar(1) = r * 300.0D0
            gcond(1) = (mod(i, 5) == 0)
            call ADVANCE_NOST_STAGING(n, 0.37D0, solar, fset, fgerm, fform, gcond)
        end do

        lhs = BED_AKI(1) - bed0
        rhs = CUM_SETTLE_AKI(1) - CUM_GERM_AKI(1) - BURIED_AKI(1)
        call assert_approx_equal(lhs, rhs, 1.0D-12, "case8: BED-BED0 == CUM_SETTLE-CUM_GERM-BURIED")
    end subroutine test_case8_bed_identity

    ! Case 9: CUM_FORM_AKI accumulates f_form*dt, and BED_AKI is unaffected by
    ! f_form (it feeds only the diagnostic integral, never the bed budget).
    subroutine test_case9_cum_form_no_bed_effect()
        integer, parameter :: n = 1
        real(kind=DBL_PREC) :: solar(n), fset(n), fgerm(n), fform(n)
        logical :: gcond(n)
        real(kind=DBL_PREC), parameter :: dt = 0.6D0
        integer :: i
        real(kind=DBL_PREC) :: bed_noform, bed_withform

        print *, "Test: Case 9 -- CUM_FORM_AKI accumulates, BED_AKI unaffected"

        call ENSURE_NOST_STAGING_STATE(n)
        call RESET_NOST_STAGING_STATE()

        BED_AKI(1) = 5.0D0
        solar = 200.0D0; fset = 1.0D0; fgerm = 0.4D0; fform = 0.0D0; gcond = .false.
        do i = 1, 5
            call ADVANCE_NOST_STAGING(n, dt, solar, fset, fgerm, fform, gcond)
        end do
        bed_noform = BED_AKI(1)
        call assert_approx_equal(CUM_FORM_AKI(1), 0.0D0, 1.0D-14, "case9: CUM_FORM_AKI stays 0 with f_form=0")

        call RESET_NOST_STAGING_STATE()
        BED_AKI(1) = 5.0D0
        fform = 3.7D0
        do i = 1, 5
            call ADVANCE_NOST_STAGING(n, dt, solar, fset, fgerm, fform, gcond)
        end do
        bed_withform = BED_AKI(1)

        call assert_approx_equal(bed_withform, bed_noform, 1.0D-12, "case9: BED_AKI unaffected by f_form")
        call assert_approx_equal(CUM_FORM_AKI(1), 3.7D0 * dt * 5.0D0, 1.0D-12, "case9: CUM_FORM_AKI == f_form*dt*steps")
    end subroutine test_case9_cum_form_no_bed_effect

    ! Case 7: SET_NOST_STAGING_PARAMS overrides all five scalars. Run last --
    ! these are module-level globals and every earlier case relies on defaults.
    subroutine test_case7_params_override()
        print *, "Test: Case 7 -- SET_NOST_STAGING_PARAMS overrides all five scalars"

        call SET_NOST_STAGING_PARAMS(20.0D0, 80.0D0, 0.1D0, 5.0D-3, 1.2D0)

        call assert_approx_equal(T_GERM_AKI_STAGE, 20.0D0, 1.0D-14, "case7: T_GERM_AKI_STAGE overridden")
        call assert_approx_equal(I_FORM_AKI, 80.0D0, 1.0D-14, "case7: I_FORM_AKI overridden")
        call assert_approx_equal(KR_GERM_BED, 0.1D0, 1.0D-14, "case7: KR_GERM_BED overridden")
        call assert_approx_equal(K_MORT_BED_AKI, 5.0D-3, 1.0D-14, "case7: K_MORT_BED_AKI overridden")
        call assert_approx_equal(V_SETTLE_AKI, 1.2D0, 1.0D-14, "case7: V_SETTLE_AKI overridden")

        ! Restore defaults for hygiene (this is the last test, but be tidy).
        call SET_NOST_STAGING_PARAMS(12.0D0, 120.0D0, 0.05D0, 1.0D-3, 0.5D0)
    end subroutine test_case7_params_override

end program test_nost_staging
