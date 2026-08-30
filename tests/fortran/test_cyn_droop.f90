! Unit tests for the CYN nitrogen-quota (Droop) mechanism -- opt-in
! CYN_VARIABLE_N, spec docs/superpowers/specs/2026-08-30-cyn-droop-n-rescoped-design.md
! section 2.
!
! Two layers are covered:
!   (a) the pure helpers of AQUABC_CYN_DROOP (F_DOWN, LIM_N_QUOTA, R_UPTAKE);
!   (b) the flag-gated branch inside the CYANOBACTERIA / CYANOBACTERIA_BOUYANT
!       library routines -- pass-through when the flag is 0, quota kinetics and
!       the rate-level N balance when it is 1.
program test_cyn_droop
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use AQUABC_CYN_DROOP
    use test_defaults
    implicit none

    integer :: passed, failed

    ! Committed constants (spec section 2). Restated here so the expected values
    ! below are readable; SET_CYN_DROOP_PARAMS is called with exactly these in
    ! reset_droop_defaults() before every case that depends on them.
    real(kind = DBL_PREC), parameter :: QMIN_D = 0.10D0
    real(kind = DBL_PREC), parameter :: QMAX_D = 0.25D0
    real(kind = DBL_PREC), parameter :: VMAX_D = 0.44D0
    real(kind = DBL_PREC), parameter :: KHS_D  = 0.003D0

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "CYN Droop-N (quota) Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_f_down_endpoints()
    call test_lim_n_quota_clamps()
    call test_r_uptake_value()
    call test_flag_off_passthrough()
    call test_rate_level_n_balance()
    call test_don_invariance_under_flag()

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

    subroutine assert_close(got, expected, tol, test_name)
        real(kind = DBL_PREC), intent(in) :: got, expected, tol
        character(len=*), intent(in) :: test_name
        if (abs(got - expected) <= tol) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,ES22.14,A,ES22.14)', "        got=", got, " expected=", expected
            failed = failed + 1
        end if
    end subroutine assert_close

    subroutine reset_droop_defaults()
        call SET_CYN_DROOP_PARAMS(QMIN_D, QMAX_D, VMAX_D, KHS_D)
    end subroutine reset_droop_defaults

    ! -------------------------------------------------------------------------
    ! Case 1: F_DOWN endpoints. Spec section 2:
    !     f_down = max(0, (Q_MAX - Q)/(Q_MAX - Q_MIN))
    ! Note the spec has NO upper clamp -- below Q_MIN f_down exceeds 1, which is
    ! the intended starvation over-drive (Q can fall below Q_MIN because CYN_N
    ! and CYN_C are transported independently).
    ! -------------------------------------------------------------------------
    subroutine test_f_down_endpoints()
        print *, "Test: F_DOWN endpoints"
        call reset_droop_defaults()

        call assert_close(F_DOWN(QMIN_D), 1.0D0, 1.0D-14, "F_DOWN(Q_MIN) = 1")
        call assert_close(F_DOWN(QMAX_D), 0.0D0, 1.0D-14, "F_DOWN(Q_MAX) = 0")
        call assert_close(F_DOWN(0.175D0), 0.5D0, 1.0D-14, "F_DOWN(midpoint) = 0.5")
        call assert_close(F_DOWN(0.30D0), 0.0D0, 1.0D-14, &
            "F_DOWN(Q > Q_MAX) clamped at 0")
        call assert_close(F_DOWN(0.05D0), (0.25D0 - 0.05D0) / 0.15D0, 1.0D-14, &
            "F_DOWN(Q < Q_MIN) is NOT clamped above (spec: max(0,.) only)")
    end subroutine test_f_down_endpoints

    ! -------------------------------------------------------------------------
    ! Case 2: LIM_N_QUOTA clamps. Spec section 2 (Caperon-Meyer):
    !     LIM_KG_CYN_N = clamp((Q - Q_MIN)/(Q_MAX - Q_MIN), 0, 1)
    ! -------------------------------------------------------------------------
    subroutine test_lim_n_quota_clamps()
        print *, "Test: LIM_N_QUOTA clamps"
        call reset_droop_defaults()

        call assert_close(LIM_N_QUOTA(QMIN_D), 0.0D0, 1.0D-14, "LIM_N_QUOTA(Q_MIN) = 0")
        call assert_close(LIM_N_QUOTA(QMAX_D), 1.0D0, 1.0D-14, "LIM_N_QUOTA(Q_MAX) = 1")
        call assert_close(LIM_N_QUOTA(0.175D0), 0.5D0, 1.0D-14, "LIM_N_QUOTA(midpoint) = 0.5")
        call assert_close(LIM_N_QUOTA(0.05D0), 0.0D0, 1.0D-14, "LIM_N_QUOTA(Q < Q_MIN) clamped at 0")
        call assert_close(LIM_N_QUOTA(0.40D0), 1.0D0, 1.0D-14, "LIM_N_QUOTA(Q > Q_MAX) clamped at 1")
    end subroutine test_lim_n_quota_clamps

    ! -------------------------------------------------------------------------
    ! Case 3: R_UPTAKE at DIN = 0.004, Q = 0.15, CYN_C = 1.0. Spec section 2:
    !     R = VMAX * DIN/(KHS_UPT + DIN) * f_down(Q) * CYN_C
    ! The expected value is written as the exact expression -- never a rounded
    ! decimal, which cannot survive a 1e-10 comparison.
    ! -------------------------------------------------------------------------
    subroutine test_r_uptake_value()
        real(kind = DBL_PREC) :: expected
        print *, "Test: R_UPTAKE value"
        call reset_droop_defaults()

        expected = 0.44d0*(0.004d0/0.007d0)*((0.25d0-0.15d0)/0.15d0)
        call assert_close(R_UPTAKE(0.004D0, 0.15D0, 1.0D0), expected, 1.0D-14, &
            "R_UPTAKE(DIN 0.004, Q 0.15, C 1.0)")

        ! Linear in biomass, zero at zero DIN, zero at Q_MAX.
        call assert_close(R_UPTAKE(0.004D0, 0.15D0, 2.0D0), 2.0D0 * expected, 1.0D-14, &
            "R_UPTAKE linear in CYN_C")
        call assert_close(R_UPTAKE(0.0D0, 0.15D0, 1.0D0), 0.0D0, 1.0D-14, &
            "R_UPTAKE = 0 at DIN = 0")
        call assert_close(R_UPTAKE(0.004D0, QMAX_D, 1.0D0), 0.0D0, 1.0D-14, &
            "R_UPTAKE = 0 at Q = Q_MAX")
    end subroutine test_r_uptake_value

    ! -------------------------------------------------------------------------
    ! Case 4: flag = 0 pass-through. Both library variants must reproduce the
    ! legacy Monod N-limitation verbatim and zero the new outputs.
    ! -------------------------------------------------------------------------
    subroutine test_flag_off_passthrough()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn), CYN_N(nkn)
        real(kind=DBL_PREC) :: LIM_N(nkn), UPT(nkn), PREF_NH4(nkn), R_GROWTH(nkn)
        real(kind=DBL_PREC) :: expected_lim, avail_n

        print *, "Test: flag = 0 pass-through (legacy Monod, new outs zero)"
        call reset_droop_defaults()

        call set_default_cyn_params(params)
        params%frac_avail_DON = 0.15D0     ! exercise the legacy DON share
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.004D0; NO3_N = 0.010D0; DON = 0.400D0; PO4_P = 0.05D0
        CYN_C = 1.0D0; ZOO_C = 0.5D0
        CYN_N = 0.0D0                      ! not supplied when the flag is off

        avail_n = NH4_N(1) + (DON(1) * params%frac_avail_DON) + NO3_N(1)
        expected_lim = avail_n / (params%KHS_DIN_CYN + avail_n)

        call run_bouyant(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                         0, CYN_N, LIM_N, UPT, PREF_NH4, R_GROWTH)
        call assert_close(LIM_N(1), expected_lim, 1.0D-14, &
            "BOUYANT flag=0: LIM_KG_CYN_N is the legacy Monod")
        call assert_close(UPT(1), 0.0D0, 0.0D0, &
            "BOUYANT flag=0: R_CYN_N_UPTAKE is exactly zero")

        call run_plain(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                       0, CYN_N, LIM_N, UPT, R_GROWTH)
        call assert_close(LIM_N(1), expected_lim, 1.0D-14, &
            "plain flag=0: LIM_KG_CYN_N is the legacy Monod")
        call assert_close(UPT(1), 0.0D0, 0.0D0, &
            "plain flag=0: R_CYN_N_UPTAKE is exactly zero")

        ! Flag on, same inputs: the N limitation must come from the quota
        ! instead (Q = 0.15 -> 1/3), proving the branch actually switches.
        ! Asserted for BOTH library variants -- the Droop block is duplicated
        ! in each, so a divergence between the two copies must be caught here.
        CYN_N = 0.15D0 * CYN_C
        call run_bouyant(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                         1, CYN_N, LIM_N, UPT, PREF_NH4, R_GROWTH)
        call assert_close(LIM_N(1), (0.15D0 - QMIN_D) / (QMAX_D - QMIN_D), 1.0D-14, &
            "BOUYANT flag=1: LIM_KG_CYN_N is the Caperon-Meyer quota term")
        call assert_true(UPT(1) > 0.0D0, "BOUYANT flag=1: uptake is positive")

        call run_plain(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                       1, CYN_N, LIM_N, UPT, R_GROWTH)
        call assert_close(LIM_N(1), (0.15D0 - QMIN_D) / (QMAX_D - QMIN_D), 1.0D-14, &
            "plain flag=1: LIM_KG_CYN_N is the Caperon-Meyer quota term")
        call assert_close(UPT(1), &
            0.44d0*((0.004d0+0.010d0)/(0.003d0+0.004d0+0.010d0)) &
                  *((0.25d0-0.15d0)/0.15d0)*1.0d0, 1.0D-14, &
            "plain flag=1: R_CYN_N_UPTAKE matches R_UPTAKE(NH4+NO3, Q, CYN_C)")

        ! The quota term must actually REACH the growth rate through the Saito
        ! SU co-limitation -- at Q = Q_MIN the N limitation is exactly 0, so
        ! growth must be exactly 0 in BOTH variants. Nothing else verifies that
        ! LIM_KG_CYN_N is used rather than computed and discarded.
        CYN_N = QMIN_D * CYN_C
        call run_bouyant(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                         1, CYN_N, LIM_N, UPT, PREF_NH4, R_GROWTH)
        call assert_close(LIM_N(1), 0.0D0, 0.0D0, "BOUYANT flag=1: LIM_N = 0 at Q = Q_MIN")
        call assert_close(R_GROWTH(1), 0.0D0, 0.0D0, &
            "BOUYANT flag=1: growth is exactly 0 at Q = Q_MIN (quota reaches LIM_KG_CYN)")

        call run_plain(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                       1, CYN_N, LIM_N, UPT, R_GROWTH)
        call assert_close(LIM_N(1), 0.0D0, 0.0D0, "plain flag=1: LIM_N = 0 at Q = Q_MIN")
        call assert_close(R_GROWTH(1), 0.0D0, 0.0D0, &
            "plain flag=1: growth is exactly 0 at Q = Q_MIN (quota reaches LIM_KG_CYN)")
    end subroutine test_flag_off_passthrough

    ! -------------------------------------------------------------------------
    ! Case 5: rate-level N balance. Grazing is a TEST-INJECTED input -- the
    ! library routine does not compute grazing; the model supplies it from
    ! R_ZOO_FEEDING_CYN. Routing (spec section 2): respiration -> NH4,
    ! death -> DET_N, excretion -> DON, grazing -> ZOO_N, all Q-weighted;
    ! uptake debits NH4/NO3 split by PREF_NH4N_CYN.
    !     d(CYN_N)/dt = uptake - Q * (resp_tot + death + excr + graze)
    !
    ! HONESTY NOTE: assembled from the routine's returned rates, the closure
    ! below cancels SYMBOLICALLY -- it checks that the routing algebra written
    ! here (and mirrored in aquabc_II_pelagic_model.f90) creates and destroys
    ! nothing, not that the model wires it up that way. The empirical
    ! conservation gate is spec sec 6 V4 (flag-on 0D integration), which is a
    ! later task. What is genuinely tested here is that the flag-on routine
    ! returns a positive uptake and non-zero losses to feed that algebra.
    ! -------------------------------------------------------------------------
    subroutine test_rate_level_n_balance()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn), CYN_N(nkn)
        real(kind=DBL_PREC) :: KG_CYN(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_L(nkn), LIM_D(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: LIM_P(nkn), LIM_NU(nkn), LIM_K(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_RESP(nkn), R_EXCR(nkn)
        real(kind=DBL_PREC) :: R_INT_RESP(nkn), KD(nkn), FH(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_DIN_DON(nkn), PREF_NH4(nkn), S_TEST(nkn)
        real(kind=DBL_PREC) :: CYN_LIGHT_SAT(nkn), UPT(nkn)
        real(kind=DBL_PREC) :: Q, resp_tot, graze_injected
        real(kind=DBL_PREC) :: d_nh4, d_no3, d_cyn_n, d_don, d_det_n, d_zoo_n
        real(kind=DBL_PREC) :: quota_net, total

        print *, "Test: rate-level N balance under the flag (grazing injected)"
        call reset_droop_defaults()

        call set_default_cyn_params(params)
        params%frac_avail_DON = 0.15D0
        TEMP = 22.0D0; I_A = 300.0D0; K_E = 2.0D0
        DEPTH = 3.5D0; CHLA = 20.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 4.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.004D0; NO3_N = 0.010D0; DON = 0.400D0; PO4_P = 0.05D0
        CYN_C = 0.80D0; ZOO_C = 0.30D0
        CYN_N = 0.17D0 * CYN_C             ! Q = 0.17, inside [Q_MIN, Q_MAX]

        CYN_LIGHT_SAT = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        KG_CYN = 0.0D0; LIM_T = 0.0D0; LIM_L = 0.0D0; LIM_D = 0.0D0
        LIM_N = 0.0D0; LIM_P = 0.0D0; LIM_NU = 0.0D0; LIM_K = 0.0D0
        R_GROWTH = 0.0D0; R_MET = 0.0D0; R_RESP = 0.0D0; R_EXCR = 0.0D0
        R_INT_RESP = 0.0D0; KD = 0.0D0; FH = 0.0D0; R_DEATH = 0.0D0
        PREF_DIN_DON = 0.0D0; PREF_NH4 = 0.0D0; S_TEST = 0.0D0; UPT = 0.0D0

        call CYANOBACTERIA_BOUYANT(params, env, CYN_LIGHT_SAT, &
                NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, 1.0D0, 1, nkn, &
                KG_CYN, ALPHA_0, ALPHA_1, LIM_T, LIM_L, LIM_D, LIM_N, &
                LIM_P, LIM_NU, LIM_K, R_GROWTH, R_MET, R_RESP, R_EXCR, &
                R_INT_RESP, KD, FH, R_DEATH, PREF_DIN_DON, PREF_NH4, &
                0, 0.5D0, 0.0D0, S_TEST, 1, CYN_N, UPT)

        ! Q as the routine itself evaluates it, and the rates the routine
        ! RETURNED (its own loss safeguard may have rescaled them).
        Q        = CYN_N(1) / max(CYN_C(1), EPS_CYN_C)
        resp_tot = R_RESP(1) + R_INT_RESP(1)      ! the model's R_CYN_TOT_RESP
        graze_injected = 0.0123D0                 ! test-injected R_ZOO_FEEDING_CYN

        call assert_true(UPT(1) > 0.0D0, "balance setup: uptake is positive")
        call assert_true(R_DEATH(1) > 0.0D0, "balance setup: death is positive")

        ! Model routing, term by term.
        d_nh4   =  Q * resp_tot - UPT(1) * PREF_NH4(1)
        d_no3   = -UPT(1) * (1.0D0 - PREF_NH4(1))
        d_don   =  Q * R_EXCR(1)
        d_det_n =  Q * R_DEATH(1)
        d_zoo_n =  Q * graze_injected
        d_cyn_n =  UPT(1) - Q * (resp_tot + R_DEATH(1) + R_EXCR(1) + graze_injected)

        ! Brief's form: uptake - (resp + death + excr + graze) * Q == quota net.
        quota_net = UPT(1) - (resp_tot + R_DEATH(1) + R_EXCR(1) + graze_injected) * Q
        call assert_close(d_cyn_n, quota_net, 1.0D-12, &
            "quota net = uptake - Q * (resp + death + excr + graze)")

        ! Full closure: no N created or destroyed by the CYN routing.
        total = d_nh4 + d_no3 + d_cyn_n + d_don + d_det_n + d_zoo_n
        call assert_close(total, 0.0D0, 1.0D-12, &
            "routing algebra conserves N (symbolic identity; V4 is the empirical gate)")
    end subroutine test_rate_level_n_balance

    ! -------------------------------------------------------------------------
    ! Case 6: the DON-sink invariant (spec section 2 FIX 1). Under the flag the
    ! growth-time DON uptake is gone: N limitation and N uptake are pure
    ! functions of the quota and of DIN, so changing DON must not move either
    ! by a single bit. frac_avail_DON is deliberately non-zero, so this test
    ! FAILS on the legacy path.
    ! -------------------------------------------------------------------------
    subroutine test_don_invariance_under_flag()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn), CYN_N(nkn)
        real(kind=DBL_PREC) :: LIM_LO(nkn), UPT_LO(nkn), PREF_LO(nkn), RG_LO(nkn)
        real(kind=DBL_PREC) :: LIM_HI(nkn), UPT_HI(nkn), PREF_HI(nkn), RG_HI(nkn)

        print *, "Test: DON-sink invariant under the flag"
        call reset_droop_defaults()

        call set_default_cyn_params(params)
        params%frac_avail_DON = 0.15D0
        TEMP = 22.0D0; I_A = 300.0D0; K_E = 2.0D0
        DEPTH = 3.5D0; CHLA = 20.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 4.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.004D0; NO3_N = 0.010D0; PO4_P = 0.05D0
        CYN_C = 0.80D0; ZOO_C = 0.30D0
        CYN_N = 0.17D0 * CYN_C

        DON = 0.0D0
        call run_bouyant(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                         1, CYN_N, LIM_LO, UPT_LO, PREF_LO, RG_LO)
        DON = 10.0D0
        call run_bouyant(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                         1, CYN_N, LIM_HI, UPT_HI, PREF_HI, RG_HI)

        call assert_close(LIM_HI(1), LIM_LO(1), 0.0D0, &
            "flag=1: LIM_KG_CYN_N is bit-identical for DON 0 vs 10")
        call assert_close(UPT_HI(1), UPT_LO(1), 0.0D0, &
            "flag=1: R_CYN_N_UPTAKE is bit-identical for DON 0 vs 10")
        call assert_close(RG_HI(1), RG_LO(1), 0.0D0, &
            "flag=1: R_CYN_GROWTH is bit-identical for DON 0 vs 10")
        call assert_true(UPT_LO(1) > 0.0D0, "flag=1: uptake non-trivial in the DON test")
    end subroutine test_don_invariance_under_flag

    ! -------------------------------------------------------------------------
    ! Helpers: one call of each library variant, returning the N-relevant
    ! outputs. Every dummy is initialised so -fcheck=all never sees garbage.
    ! -------------------------------------------------------------------------
    subroutine run_bouyant(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                           FLAG, CYN_N, LIM_N_OUT, UPT_OUT, PREF_NH4_OUT, GROWTH_OUT)
        type(t_cyn_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        real(kind=DBL_PREC), intent(in) :: NH4_N(1), NO3_N(1), DON(1), PO4_P(1)
        real(kind=DBL_PREC), intent(in) :: CYN_C(1), ZOO_C(1), CYN_N(1)
        integer, intent(in) :: FLAG
        real(kind=DBL_PREC), intent(out) :: LIM_N_OUT(1), UPT_OUT(1), PREF_NH4_OUT(1)
        real(kind=DBL_PREC), intent(out) :: GROWTH_OUT(1)
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: CYN_LIGHT_SAT(nkn), KG(nkn), A0(nkn), A1(nkn)
        real(kind=DBL_PREC) :: LT(nkn), LL(nkn), LD(nkn), LN(nkn), LP(nkn)
        real(kind=DBL_PREC) :: LNU(nkn), LK(nkn)
        real(kind=DBL_PREC) :: RG(nkn), RM(nkn), RR(nkn), RE(nkn), RI(nkn)
        real(kind=DBL_PREC) :: KD(nkn), FH(nkn), RD(nkn), PDD(nkn), PNH(nkn)
        real(kind=DBL_PREC) :: S_R(nkn), UPT(nkn)

        CYN_LIGHT_SAT = 0.0D0
        KG=0; A0=0; A1=0; LT=0; LL=0; LD=0; LN=0; LP=0; LNU=0; LK=0
        RG=0; RM=0; RR=0; RE=0; RI=0; KD=0; FH=0; RD=0; PDD=0; PNH=0
        S_R = 0.0D0; UPT = 0.0D0

        call CYANOBACTERIA_BOUYANT(params, env, CYN_LIGHT_SAT, &
                NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, 1.0D0, 1, nkn, &
                KG, A0, A1, LT, LL, LD, LN, LP, LNU, LK, &
                RG, RM, RR, RE, RI, KD, FH, RD, PDD, PNH, &
                0, 0.5D0, 0.0D0, S_R, FLAG, CYN_N, UPT)

        LIM_N_OUT    = LN
        UPT_OUT      = UPT
        PREF_NH4_OUT = PNH
        GROWTH_OUT   = RG
    end subroutine run_bouyant

    subroutine run_plain(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                         FLAG, CYN_N, LIM_N_OUT, UPT_OUT, GROWTH_OUT)
        type(t_cyn_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        real(kind=DBL_PREC), intent(in) :: NH4_N(1), NO3_N(1), DON(1), PO4_P(1)
        real(kind=DBL_PREC), intent(in) :: CYN_C(1), ZOO_C(1), CYN_N(1)
        integer, intent(in) :: FLAG
        real(kind=DBL_PREC), intent(out) :: LIM_N_OUT(1), UPT_OUT(1), GROWTH_OUT(1)
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: CYN_LIGHT_SAT(nkn), KG(nkn), A0(nkn), A1(nkn)
        real(kind=DBL_PREC) :: LT(nkn), LL(nkn), LD(nkn), LN(nkn), LP(nkn)
        real(kind=DBL_PREC) :: LNU(nkn), LK(nkn)
        real(kind=DBL_PREC) :: RG(nkn), RM(nkn), RR(nkn), RE(nkn), RI(nkn)
        real(kind=DBL_PREC) :: KD(nkn), FH(nkn), RD(nkn), PND(nkn), UPT(nkn)

        CYN_LIGHT_SAT = 0.0D0
        KG=0; A0=0; A1=0; LT=0; LL=0; LD=0; LN=0; LP=0; LNU=0; LK=0
        RG=0; RM=0; RR=0; RE=0; RI=0; KD=0; FH=0; RD=0; PND=0; UPT=0

        call CYANOBACTERIA(params, env, CYN_LIGHT_SAT, NH4_N, NO3_N, DON, &
                PO4_P, CYN_C, ZOO_C, 1.0D0, 1, nkn, &
                KG, A0, A1, LT, LL, LD, LN, LP, LNU, LK, &
                RG, RM, RR, RE, RI, KD, FH, RD, PND, &
                FLAG, CYN_N, UPT)

        LIM_N_OUT  = LN
        UPT_OUT    = UPT
        GROWTH_OUT = RG
    end subroutine run_plain

end program test_cyn_droop
