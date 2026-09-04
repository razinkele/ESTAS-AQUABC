! Test program for LIGHT_DAYLENGTH_OPTION in LIM_LIGHT.
!
! Covers the three day-length forms and, most importantly, the two identities
! that make the option safe and make Form B the correct one:
!
!   opt 0  legacy -- must be bit-identical to the pre-option expression
!   opt 1  Form A -- LLIGHT = FDAY * base, exactly
!   opt 2  Form B -- LLIGHT = FDAY * f(I_A/FDAY); bracketed strictly between
!          Form A and base, and converging to base as FDAY -> 1 (the
!          cancellation identity: while light-limited the P-I curve is
!          near-linear, so the FDAY divides out).
!
! Background: docs/CL29_phenology_diagnosis.md section 47.
!
! Compile with:
!   gfortran -o test_lim_light_daylength <deps> test_lim_light_daylength.f90
! Run: ./test_lim_light_daylength

program test_lim_light_daylength
    use AQUABC_II_GLOBAL, only: LIGHT_DAYLENGTH_OPTION
    implicit none

    integer :: num_passed, num_failed, num_tests

    num_passed = 0
    num_failed = 0
    num_tests  = 0

    write(*,*) '=============================================='
    write(*,*) 'LIM_LIGHT day-length option test suite'
    write(*,*) '=============================================='
    write(*,*)

    call test_legacy_ignores_fday(num_passed, num_failed, num_tests)
    call test_form_a_is_exact_scaling(num_passed, num_failed, num_tests)
    call test_form_b_brackets(num_passed, num_failed, num_tests)
    call test_form_b_cancels_at_full_day(num_passed, num_failed, num_tests)
    call test_form_b_is_less_differential(num_passed, num_failed, num_tests)

    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I0,A,I0,A)') '  Passed: ', num_passed, ' / ', num_tests, ' tests'
    write(*,*) '=============================================='

    if (num_failed > 0) then
        write(*,*) 'FAILURES: ', num_failed
        error stop 1
    end if

contains

    ! Evaluate LIM_LIGHT at a chosen operating point.
    !
    ! GITMAX = 0 deliberately routes LIM_LIGHT to its user-defined-saturation
    ! branch, so K_LIGHT_SAT IS the saturation intensity I_s. That lets the test
    ! set I_a and I_s exactly instead of depending on the module's XKC/PHIMX.
    subroutine eval_at(opt, ia_val, is_val, ke_val, fday_val, out)
        integer,          intent(in)  :: opt
        double precision, intent(in)  :: ia_val, is_val, ke_val, fday_val
        double precision, intent(out) :: out

        double precision :: Ia(1), TCHLA(1), GIT(1), Hh(1), ke(1), FD(1)
        double precision :: LL(1), LS(1)

        Ia    = ia_val
        TCHLA = 2.0D0
        GIT   = 0.0D0          ! -> user-defined saturation, I_s = K_LIGHT_SAT
        Hh    = 3.5D0          ! CL29 mean depth
        ke    = ke_val
        FD    = fday_val

        LIGHT_DAYLENGTH_OPTION = opt
        call LIM_LIGHT(Ia, TCHLA, GIT, Hh, ke, LL, 53.0D0, is_val, LS, 1, 0.0D0, FD)
        LIGHT_DAYLENGTH_OPTION = 0   ! never leak state between tests

        out = LL(1)
    end subroutine eval_at

    ! February-like CL29 operating point: I_a 32.64 langley/d, I_s 110.62,
    ! kd 2.617, photoperiod 0.389 -- all monthly means of the live CL29 record
    ! (doc s.44.3 corrected for kd; INPUTS_CL29/FORC_TS_9.txt for FDAY).
    subroutine eval_ll(opt, fday_val, out)
        integer,          intent(in)  :: opt
        double precision, intent(in)  :: fday_val
        double precision, intent(out) :: out

        call eval_at(opt, 32.64D0, 110.62D0, 2.617D0, fday_val, out)
    end subroutine eval_ll

    subroutine check(cond, label, np, nf, nt)
        logical,          intent(in)    :: cond
        character(len=*), intent(in)    :: label
        integer,          intent(inout) :: np, nf, nt

        nt = nt + 1
        if (cond) then
            np = np + 1
            write(*,*) '  PASS: ', label
        else
            nf = nf + 1
            write(*,*) '  FAIL: ', label
        end if
    end subroutine check

    ! Option 0 must ignore FDAY entirely -- this is what guarantees the
    ! standard build stays byte-identical.
    subroutine test_legacy_ignores_fday(np, nf, nt)
        integer, intent(inout) :: np, nf, nt
        double precision :: a, b

        write(*,*) 'Test: option 0 ignores FDAY (byte-identity guarantee)'
        call eval_ll(0, 0.389D0, a)
        call eval_ll(0, 1.000D0, b)
        call check(a == b, 'opt 0 identical for FDAY 0.389 vs 1.0', np, nf, nt)
        call check(a > 0.0D0, 'opt 0 returns a positive limitation factor', np, nf, nt)
    end subroutine test_legacy_ignores_fday

    ! Form A is a pure outer multiply, so it must equal FDAY * base exactly.
    subroutine test_form_a_is_exact_scaling(np, nf, nt)
        integer, intent(inout) :: np, nf, nt
        double precision :: base, formA, fd

        write(*,*) 'Test: Form A = FDAY * base, exactly'
        fd = 0.389D0
        call eval_ll(0, fd, base)
        call eval_ll(1, fd, formA)
        call check(abs(formA - fd*base) <= 1.0D-14 * max(1.0D0, abs(fd*base)), &
                   'opt 1 equals FDAY * base to round-off', np, nf, nt)
    end subroutine test_form_a_is_exact_scaling

    ! Form B recovers most of the dose Form A discards, but pays the P-I
    ! curvature penalty -- so it must sit strictly between the two.
    subroutine test_form_b_brackets(np, nf, nt)
        integer, intent(inout) :: np, nf, nt
        double precision :: base, formA, formB, fd

        write(*,*) 'Test: Form A < Form B < base'
        fd = 0.389D0
        call eval_ll(0, fd, base)
        call eval_ll(1, fd, formA)
        call eval_ll(2, fd, formB)
        call check(formB > formA, 'Form B exceeds Form A (recovers discarded light)', np, nf, nt)
        call check(formB < base,  'Form B below legacy (pays curvature penalty)',    np, nf, nt)
    end subroutine test_form_b_brackets

    ! The cancellation identity: at FDAY = 1 the photoperiod is the whole day,
    ! so Form B must reduce exactly to the legacy expression.
    subroutine test_form_b_cancels_at_full_day(np, nf, nt)
        integer, intent(inout) :: np, nf, nt
        double precision :: base, formB

        write(*,*) 'Test: Form B -> base as FDAY -> 1 (cancellation identity)'
        call eval_ll(0, 1.0D0, base)
        call eval_ll(2, 1.0D0, formB)
        call check(abs(formB - base) <= 1.0D-14 * max(1.0D0, abs(base)), &
                   'opt 2 at FDAY=1 equals legacy', np, nf, nt)
    end subroutine test_form_b_cancels_at_full_day

    ! The headline result of doc s.47: Form A is strongly month-differential
    ! while Form B is close to a uniform offset.
    !
    ! This must compare REALISTIC February and May operating points -- I_a, I_s
    ! and kd all co-vary seasonally alongside FDAY, and varying FDAY alone at a
    ! fixed I_a measures P-I curvature sensitivity rather than the realized
    ! seasonal differential. Monthly means of the live CL29 record:
    !
    !   Feb  I_a  32.64  I_s 110.62  kd 2.617  FDAY 0.389   (I/I_s 0.295)
    !   May  I_a 243.33  I_s 235.04  kd 2.648  FDAY 0.665   (I/I_s 1.035)
    !
    ! Expected: Form A 1.7095 (exactly the FDAY ratio -- it is a pure multiply),
    ! Form B 1.0055. Doc s.47.2 reports 1.68x and 1.04x; it averages daily
    ! values where this uses monthly-mean inputs, so the two differ slightly by
    ! Jensen's inequality. The qualitative result is identical.
    subroutine test_form_b_is_less_differential(np, nf, nt)
        integer, intent(inout) :: np, nf, nt
        double precision :: bFeb, bMay, aFeb, aMay, fFeb, fMay
        double precision :: ratioA, ratioB

        write(*,*) 'Test: Form B is near-uniform, Form A is differential'
        call eval_at(0,  32.64D0, 110.62D0, 2.617D0, 0.389D0, bFeb)
        call eval_at(0, 243.33D0, 235.04D0, 2.648D0, 0.665D0, bMay)
        call eval_at(1,  32.64D0, 110.62D0, 2.617D0, 0.389D0, aFeb)
        call eval_at(1, 243.33D0, 235.04D0, 2.648D0, 0.665D0, aMay)
        call eval_at(2,  32.64D0, 110.62D0, 2.617D0, 0.389D0, fFeb)
        call eval_at(2, 243.33D0, 235.04D0, 2.648D0, 0.665D0, fMay)

        ! ratio = (May attenuation) / (Feb attenuation); 1.0 means uniform
        ratioA = (aMay/bMay) / (aFeb/bFeb)
        ratioB = (fMay/bMay) / (fFeb/bFeb)

        write(*,'(A,F8.4,A,F8.4)') '        Form A differential ', ratioA, &
                                   '   Form B differential ', ratioB
        call check(abs(ratioA - 0.665D0/0.389D0) < 1.0D-9, &
                   'Form A differential IS the FDAY ratio (pure multiply)', np, nf, nt)
        call check(ratioA > 1.5D0,  'Form A differential exceeds 1.5x',       np, nf, nt)
        call check(ratioB < 1.10D0, 'Form B differential below 1.10x (offset)', np, nf, nt)
        call check(ratioB < ratioA, 'Form B less differential than Form A',   np, nf, nt)
    end subroutine test_form_b_is_less_differential

end program test_lim_light_daylength
