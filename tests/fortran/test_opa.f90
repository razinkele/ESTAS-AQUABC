! Unit tests for OTHER_PLANKTONIC_ALGAE kinetics subroutine
program test_opa
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "OTHER_PLANKTONIC_ALGAE Kinetics Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_smoke()
    call test_growth_positive()
    call test_nutrient_n_limitation()
    call test_nutrient_p_limitation()
    call test_temperature_response()
    call test_zero_biomass()
    call test_losses_bounded()
    call test_light_limitation()

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

    subroutine assert_not_nan(value, test_name)
        real(kind=DBL_PREC), intent(in) :: value
        character(len=*), intent(in) :: test_name
        if (value == value) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A)', "    Value was NaN"
            failed = failed + 1
        end if
    end subroutine assert_not_nan

    ! Helper: run OTHER_PLANKTONIC_ALGAE with given inputs
    subroutine run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                       TIME_STEP, nkn, &
                       R_OPA_GROWTH, R_OPA_MET, R_OPA_DEATH, &
                       LIM_KG_OPA_TEMP, LIM_KG_OPA_N, LIM_KG_OPA_P, &
                       LIM_KG_OPA_LIGHT, PREF_NH4N_OPA)
        type(t_opa_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        integer, intent(in) :: nkn
        real(kind=DBL_PREC), intent(in) :: NH4_N(nkn), NO3_N(nkn)
        real(kind=DBL_PREC), intent(in) :: PO4_P(nkn), OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC), intent(in) :: TIME_STEP
        real(kind=DBL_PREC), intent(out) :: R_OPA_GROWTH(nkn), R_OPA_MET(nkn)
        real(kind=DBL_PREC), intent(out) :: R_OPA_DEATH(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_OPA_TEMP(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_OPA_N(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_OPA_P(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_OPA_LIGHT(nkn)
        real(kind=DBL_PREC), intent(out) :: PREF_NH4N_OPA(nkn)

        real(kind=DBL_PREC) :: OPA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_OPA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_OPA_DOXY(nkn)
        real(kind=DBL_PREC) :: LIM_KG_OPA_NUTR(nkn), LIM_KG_OPA(nkn)
        real(kind=DBL_PREC) :: R_OPA_RESP(nkn), R_OPA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_OPA_INT_RESP(nkn), KD_OPA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_OPA_D(nkn)

        OPA_LIGHT_SAT = 0.0D0; KG_OPA = 0.0D0
        ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_OPA_TEMP = 0.0D0; LIM_KG_OPA_LIGHT = 0.0D0
        LIM_KG_OPA_DOXY = 0.0D0; LIM_KG_OPA_N = 0.0D0
        LIM_KG_OPA_P = 0.0D0; LIM_KG_OPA_NUTR = 0.0D0; LIM_KG_OPA = 0.0D0
        R_OPA_GROWTH = 0.0D0; R_OPA_MET = 0.0D0
        R_OPA_RESP = 0.0D0; R_OPA_EXCR = 0.0D0
        R_OPA_INT_RESP = 0.0D0; KD_OPA = 0.0D0
        FAC_HYPOX_OPA_D = 0.0D0; R_OPA_DEATH = 0.0D0
        PREF_NH4N_OPA = 0.0D0

        call OTHER_PLANKTONIC_ALGAE(params, env, OPA_LIGHT_SAT, &
                                    NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                                    TIME_STEP, 1, nkn, &
                                    KG_OPA, ALPHA_0, ALPHA_1, &
                                    LIM_KG_OPA_TEMP, LIM_KG_OPA_LIGHT, &
                                    LIM_KG_OPA_DOXY, LIM_KG_OPA_N, &
                                    LIM_KG_OPA_P, LIM_KG_OPA_NUTR, &
                                    LIM_KG_OPA, R_OPA_GROWTH, R_OPA_MET, &
                                    R_OPA_RESP, R_OPA_EXCR, R_OPA_INT_RESP, &
                                    KD_OPA, FAC_HYPOX_OPA_D, R_OPA_DEATH, &
                                    PREF_NH4N_OPA)
    end subroutine run_opa

    ! Smoke test: typical conditions produce plausible outputs
    subroutine test_smoke()
        integer, parameter :: nkn = 1
        type(t_opa_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn), LIM_P(nkn), LIM_L(nkn)
        real(kind=DBL_PREC) :: PREF(nkn)

        print *, "Test: Smoke test (typical conditions)"

        call set_default_opa_params(params)
        TEMP = 18.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.1D0; NO3_N = 0.5D0; PO4_P = 0.05D0
        OPA_C = 1.0D0; ZOO_C = 0.5D0

        call run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                     1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                     LIM_P, LIM_L, PREF)

        call assert_not_nan(R_GROWTH(1), "Growth rate is not NaN")
        call assert_true(R_GROWTH(1) > 0.0D0, "Growth rate is positive")
    end subroutine test_smoke

    ! Growth is positive at optimal conditions
    subroutine test_growth_positive()
        integer, parameter :: nkn = 1
        type(t_opa_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn), LIM_P(nkn), LIM_L(nkn)
        real(kind=DBL_PREC) :: PREF(nkn)

        print *, "Test: Growth is positive and bounded"

        call set_default_opa_params(params)
        ! Optimal temp for OPA is OPA_OPT_TEMP_UR = 20
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        OPA_C = 2.0D0; ZOO_C = 0.5D0

        call run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                     1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                     LIM_P, LIM_L, PREF)

        call assert_true(R_GROWTH(1) > 0.0D0, "Growth is positive")
        call assert_true(R_GROWTH(1) <= params%KG_OPA_OPT_TEMP * OPA_C(1), &
                         "Growth bounded by max rate * biomass")
    end subroutine test_growth_positive

    ! Low DIN should limit growth
    subroutine test_nutrient_n_limitation()
        integer, parameter :: nkn = 1
        type(t_opa_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn), LIM_P(nkn), LIM_L(nkn)
        real(kind=DBL_PREC) :: PREF(nkn)

        print *, "Test: Low DIN limits growth"

        call set_default_opa_params(params)
        TEMP = 18.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.001D0; NO3_N = 0.001D0; PO4_P = 0.1D0
        OPA_C = 1.0D0; ZOO_C = 0.5D0

        call run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                     1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                     LIM_P, LIM_L, PREF)

        call assert_true(LIM_N(1) < 0.5D0, "N limitation < 0.5 under low DIN")
    end subroutine test_nutrient_n_limitation

    ! Low PO4 should limit growth
    subroutine test_nutrient_p_limitation()
        integer, parameter :: nkn = 1
        type(t_opa_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn), LIM_P(nkn), LIM_L(nkn)
        real(kind=DBL_PREC) :: PREF(nkn)

        print *, "Test: Low PO4 limits growth"

        call set_default_opa_params(params)
        TEMP = 18.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.001D0  ! Very low P
        OPA_C = 1.0D0; ZOO_C = 0.5D0

        call run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                     1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                     LIM_P, LIM_L, PREF)

        call assert_true(LIM_P(1) < 0.5D0, "P limitation < 0.5 under low PO4")
    end subroutine test_nutrient_p_limitation

    ! Optimal temperature gives high temp limitation
    subroutine test_temperature_response()
        integer, parameter :: nkn = 1
        type(t_opa_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn), LIM_P(nkn), LIM_L(nkn)
        real(kind=DBL_PREC) :: PREF(nkn)

        print *, "Test: Optimal temperature gives high temp limitation"

        call set_default_opa_params(params)
        ! Optimal temp for OPA is OPA_OPT_TEMP_UR = 20
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        OPA_C = 1.0D0; ZOO_C = 0.5D0

        call run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                     1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                     LIM_P, LIM_L, PREF)

        call assert_true(LIM_T(1) > 0.9D0, &
                         "Temp limitation near 1.0 at optimal temperature")
    end subroutine test_temperature_response

    ! Zero biomass should produce zero rates
    subroutine test_zero_biomass()
        integer, parameter :: nkn = 1
        type(t_opa_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn), LIM_P(nkn), LIM_L(nkn)
        real(kind=DBL_PREC) :: PREF(nkn)

        print *, "Test: Zero biomass gives zero rates"

        call set_default_opa_params(params)
        TEMP = 18.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        OPA_C = 0.0D0  ! Zero biomass
        ZOO_C = 0.5D0

        call run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                     1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                     LIM_P, LIM_L, PREF)

        call assert_not_nan(R_GROWTH(1), "Growth not NaN with zero biomass")
        call assert_true(abs(R_GROWTH(1)) < 1.0D-10, "Growth near zero")
        call assert_true(abs(R_DEATH(1)) < 1.0D-10, "Death near zero")
    end subroutine test_zero_biomass

    ! Losses bounded
    subroutine test_losses_bounded()
        integer, parameter :: nkn = 1
        type(t_opa_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn), LIM_P(nkn), LIM_L(nkn)
        real(kind=DBL_PREC) :: PREF(nkn)
        real(kind=DBL_PREC) :: TS

        print *, "Test: Losses bounded below 50%% of biomass"

        call set_default_opa_params(params)
        TEMP = 18.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        OPA_C = 2.0D0; ZOO_C = 0.5D0
        TS = 1.0D0

        call run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                     TS, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                     LIM_P, LIM_L, PREF)

        call assert_true((R_MET(1) + R_DEATH(1)) * TS < 0.5D0 * OPA_C(1), &
                         "Met + death < 50% biomass per timestep")
    end subroutine test_losses_bounded

    ! Light limitation in [0,1]
    subroutine test_light_limitation()
        integer, parameter :: nkn = 1
        type(t_opa_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: OPA_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn), LIM_P(nkn), LIM_L(nkn)
        real(kind=DBL_PREC) :: PREF(nkn)

        print *, "Test: Light limitation in [0,1]"

        call set_default_opa_params(params)
        TEMP = 18.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        OPA_C = 1.0D0; ZOO_C = 0.5D0

        call run_opa(params, env, NH4_N, NO3_N, PO4_P, OPA_C, ZOO_C, &
                     1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                     LIM_P, LIM_L, PREF)

        call assert_true(LIM_L(1) >= 0.0D0 .and. LIM_L(1) <= 1.0D0, &
                         "LIM_KG_OPA_LIGHT in [0,1]")
    end subroutine test_light_limitation

end program test_opa
