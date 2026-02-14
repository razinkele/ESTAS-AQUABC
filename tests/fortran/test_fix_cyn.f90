! Unit tests for FIX_CYANOBACTERIA kinetics subroutine
program test_fix_cyn
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "FIX_CYANOBACTERIA Kinetics Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_smoke()
    call test_growth_positive()
    call test_nfix_switching()
    call test_nutrient_n_limitation()
    call test_temperature_response()
    call test_zero_biomass()
    call test_losses_bounded()
    call test_nh4_preference()
    call test_ke_zero()

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

    subroutine assert_finite(value, test_name)
        real(kind=DBL_PREC), intent(in) :: value
        character(len=*), intent(in) :: test_name
        if (value /= value) then
            print '(A,A)', "  FAIL: ", test_name
            print '(A)', "    Value was NaN"
            failed = failed + 1
        else if (abs(value) > huge(value) * 0.5D0) then
            print '(A,A)', "  FAIL: ", test_name
            print '(A,ES12.4)', "    Value was Inf: ", value
            failed = failed + 1
        else
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        end if
    end subroutine assert_finite

    ! Helper: run FIX_CYANOBACTERIA with given inputs, return key outputs
    subroutine run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                           TIME_STEP, nkn, &
                           R_FIX_CYN_GROWTH, R_FIX_CYN_MET, R_FIX_CYN_DEATH, &
                           LIM_KG_FIX_CYN_TEMP, LIM_KG_NON_FIX_CYN_N, &
                           R_NON_FIX_CYN_GROWTH, R_FIX_FIX_CYN_GROWTH, &
                           PREF_NH4N_DON_FIX_CYN)
        type(t_fix_cyn_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        integer, intent(in) :: nkn
        real(kind=DBL_PREC), intent(in) :: NH4_N(nkn), NO3_N(nkn), DON(nkn)
        real(kind=DBL_PREC), intent(in) :: PO4_P(nkn), FIX_CYN_C(nkn)
        real(kind=DBL_PREC), intent(in) :: TIME_STEP
        real(kind=DBL_PREC), intent(out) :: R_FIX_CYN_GROWTH(nkn)
        real(kind=DBL_PREC), intent(out) :: R_FIX_CYN_MET(nkn)
        real(kind=DBL_PREC), intent(out) :: R_FIX_CYN_DEATH(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_FIX_CYN_TEMP(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_NON_FIX_CYN_N(nkn)
        real(kind=DBL_PREC), intent(out) :: R_NON_FIX_CYN_GROWTH(nkn)
        real(kind=DBL_PREC), intent(out) :: R_FIX_FIX_CYN_GROWTH(nkn)
        real(kind=DBL_PREC), intent(out) :: PREF_NH4N_DON_FIX_CYN(nkn)

        real(kind=DBL_PREC) :: FIX_CYN_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: KG_FIX_CYN(nkn)
        real(kind=DBL_PREC) :: LIM_KG_FIX_CYN_LIGHT(nkn), LIM_KG_FIX_CYN_DOXY(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NON_FIX_CYN_P(nkn), LIM_KG_NON_FIX_CYN_NUTR(nkn)
        real(kind=DBL_PREC) :: LIM_KG_FIX_FIX_CYN_N(nkn), LIM_KG_FIX_FIX_CYN_P(nkn)
        real(kind=DBL_PREC) :: LIM_KG_FIX_FIX_CYN_NUTR(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NON_FIX_CYN(nkn), LIM_KG_FIX_FIX_CYN(nkn)
        real(kind=DBL_PREC) :: R_FIX_CYN_RESP(nkn), R_FIX_CYN_EXCR(nkn)
        real(kind=DBL_PREC) :: R_FIX_CYN_INT_RESP(nkn), KD_FIX_CYN(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_FIX_CYN_D(nkn)

        FIX_CYN_LIGHT_SAT = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        KG_FIX_CYN = 0.0D0
        LIM_KG_FIX_CYN_LIGHT = 0.0D0; LIM_KG_FIX_CYN_TEMP = 0.0D0
        LIM_KG_FIX_CYN_DOXY = 0.0D0
        LIM_KG_NON_FIX_CYN_N = 0.0D0; LIM_KG_NON_FIX_CYN_P = 0.0D0
        LIM_KG_NON_FIX_CYN_NUTR = 0.0D0
        LIM_KG_FIX_FIX_CYN_N = 0.0D0; LIM_KG_FIX_FIX_CYN_P = 0.0D0
        LIM_KG_FIX_FIX_CYN_NUTR = 0.0D0
        LIM_KG_NON_FIX_CYN = 0.0D0; LIM_KG_FIX_FIX_CYN = 0.0D0
        R_NON_FIX_CYN_GROWTH = 0.0D0; R_FIX_FIX_CYN_GROWTH = 0.0D0
        R_FIX_CYN_GROWTH = 0.0D0; R_FIX_CYN_MET = 0.0D0
        R_FIX_CYN_RESP = 0.0D0; R_FIX_CYN_EXCR = 0.0D0
        R_FIX_CYN_INT_RESP = 0.0D0; KD_FIX_CYN = 0.0D0
        FAC_HYPOX_FIX_CYN_D = 0.0D0; R_FIX_CYN_DEATH = 0.0D0
        PREF_NH4N_DON_FIX_CYN = 0.0D0

        call FIX_CYANOBACTERIA(params, env, TIME_STEP, 1, nkn, &
                               NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                               FIX_CYN_LIGHT_SAT, ALPHA_0, ALPHA_1, &
                               KG_FIX_CYN, LIM_KG_FIX_CYN_LIGHT, &
                               LIM_KG_FIX_CYN_TEMP, LIM_KG_FIX_CYN_DOXY, &
                               LIM_KG_NON_FIX_CYN_N, LIM_KG_NON_FIX_CYN_P, &
                               LIM_KG_NON_FIX_CYN_NUTR, &
                               LIM_KG_FIX_FIX_CYN_N, LIM_KG_FIX_FIX_CYN_P, &
                               LIM_KG_FIX_FIX_CYN_NUTR, &
                               LIM_KG_NON_FIX_CYN, LIM_KG_FIX_FIX_CYN, &
                               R_NON_FIX_CYN_GROWTH, R_FIX_FIX_CYN_GROWTH, &
                               R_FIX_CYN_GROWTH, R_FIX_CYN_MET, &
                               R_FIX_CYN_RESP, R_FIX_CYN_EXCR, &
                               R_FIX_CYN_INT_RESP, KD_FIX_CYN, &
                               FAC_HYPOX_FIX_CYN_D, R_FIX_CYN_DEATH, &
                               PREF_NH4N_DON_FIX_CYN)
    end subroutine run_fix_cyn

    ! Smoke test: typical conditions produce plausible outputs
    subroutine test_smoke()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX(nkn), R_FIX_FIX(nkn), PREF(nkn)

        print *, "Test: Smoke test (typical conditions)"

        call set_default_fix_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.1D0; NO3_N = 0.5D0; DON = 0.1D0; PO4_P = 0.05D0
        FIX_CYN_C = 1.0D0

        call run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                         1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                         R_NON_FIX, R_FIX_FIX, PREF)

        call assert_not_nan(R_GROWTH(1), "Growth rate is not NaN")
        call assert_true(R_GROWTH(1) > 0.0D0, "Growth rate is positive")
    end subroutine test_smoke

    ! Growth is positive at optimal conditions
    subroutine test_growth_positive()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX(nkn), R_FIX_FIX(nkn), PREF(nkn)

        print *, "Test: Growth is positive at optimal conditions"

        call set_default_fix_cyn_params(params)
        TEMP = 26.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.5D0; NO3_N = 1.0D0; DON = 0.5D0; PO4_P = 0.1D0
        FIX_CYN_C = 2.0D0

        call run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                         1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                         R_NON_FIX, R_FIX_FIX, PREF)

        call assert_true(R_GROWTH(1) > 0.0D0, "Growth is positive")
    end subroutine test_growth_positive

    ! N-fixation switching: low DIN should boost fixing fraction
    subroutine test_nfix_switching()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX(nkn), R_FIX_FIX(nkn), PREF(nkn)

        print *, "Test: Low DIN triggers N-fixation"

        call set_default_fix_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        ! Very low DIN should favor fixing fraction
        NH4_N = 0.001D0; NO3_N = 0.001D0; DON = 0.0D0; PO4_P = 0.1D0
        FIX_CYN_C = 1.0D0

        call run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                         1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                         R_NON_FIX, R_FIX_FIX, PREF)

        ! With very low DIN, fixing growth should exceed non-fixing growth
        call assert_true(R_FIX_FIX(1) > R_NON_FIX(1), &
                         "Fixing growth > non-fixing growth under low DIN")
    end subroutine test_nfix_switching

    ! Low DIN should limit non-fixing fraction growth
    subroutine test_nutrient_n_limitation()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX(nkn), R_FIX_FIX(nkn), PREF(nkn)

        print *, "Test: Low DIN limits non-fixing fraction"

        call set_default_fix_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.001D0; NO3_N = 0.001D0; DON = 0.0D0; PO4_P = 0.1D0
        FIX_CYN_C = 1.0D0

        call run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                         1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                         R_NON_FIX, R_FIX_FIX, PREF)

        call assert_true(LIM_N(1) < 0.5D0, "N limitation < 0.5 under low DIN")
    end subroutine test_nutrient_n_limitation

    ! Optimal temperature gives high temp limitation
    subroutine test_temperature_response()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX(nkn), R_FIX_FIX(nkn), PREF(nkn)

        print *, "Test: Optimal temperature gives high temp limitation"

        call set_default_fix_cyn_params(params)
        ! Optimal temp for FIX_CYN is FIX_CYN_OPT_TEMP_UR = 26
        TEMP = 26.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.5D0; NO3_N = 1.0D0; DON = 0.5D0; PO4_P = 0.1D0
        FIX_CYN_C = 1.0D0

        call run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                         1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                         R_NON_FIX, R_FIX_FIX, PREF)

        call assert_true(LIM_T(1) > 0.9D0, &
                         "Temp limitation near 1.0 at optimal temperature")
    end subroutine test_temperature_response

    ! Zero biomass should produce zero rates
    subroutine test_zero_biomass()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX(nkn), R_FIX_FIX(nkn), PREF(nkn)

        print *, "Test: Zero biomass gives zero rates"

        call set_default_fix_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.5D0; NO3_N = 1.0D0; DON = 0.5D0; PO4_P = 0.1D0
        FIX_CYN_C = 0.0D0  ! Zero biomass

        call run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                         1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                         R_NON_FIX, R_FIX_FIX, PREF)

        call assert_not_nan(R_GROWTH(1), "Growth not NaN with zero biomass")
        call assert_true(abs(R_GROWTH(1)) < 1.0D-10, "Growth near zero")
        call assert_true(abs(R_DEATH(1)) < 1.0D-10, "Death near zero")
    end subroutine test_zero_biomass

    ! Losses bounded
    subroutine test_losses_bounded()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX(nkn), R_FIX_FIX(nkn), PREF(nkn)
        real(kind=DBL_PREC) :: TS

        print *, "Test: Losses bounded below 50%% of biomass"

        call set_default_fix_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.5D0; NO3_N = 1.0D0; DON = 0.5D0; PO4_P = 0.1D0
        FIX_CYN_C = 2.0D0
        TS = 1.0D0

        call run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                         TS, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                         R_NON_FIX, R_FIX_FIX, PREF)

        call assert_true((R_MET(1) + R_DEATH(1)) * TS < 0.5D0 * FIX_CYN_C(1), &
                         "Met + death < 50% biomass per timestep")
    end subroutine test_losses_bounded

    ! NH4 preference in [0,1]
    subroutine test_nh4_preference()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX(nkn), R_FIX_FIX(nkn), PREF(nkn)

        print *, "Test: NH4 preference in [0,1]"

        call set_default_fix_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.1D0; NO3_N = 0.5D0; DON = 0.1D0; PO4_P = 0.05D0
        FIX_CYN_C = 1.0D0

        call run_fix_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                         1.0D0, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, LIM_N, &
                         R_NON_FIX, R_FIX_FIX, PREF)

        call assert_true(PREF(1) >= 0.0D0 .and. PREF(1) <= 1.0D0, &
                         "PREF_NH4N in [0,1]")
    end subroutine test_nh4_preference

    ! K_E = 0 should not produce NaN or Inf (division by zero guard)
    ! Uses FIX_CYANOBACTERIA_BOUYANT with SMITH=1 to trigger euphotic depth path
    subroutine test_ke_zero()
        integer, parameter :: nkn = 1
        type(t_fix_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn)

        real(kind=DBL_PREC) :: FIX_CYN_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: KG_FIX_CYN(nkn)
        real(kind=DBL_PREC) :: LIM_KG_FIX_CYN_LIGHT(nkn), LIM_KG_FIX_CYN_TEMP(nkn)
        real(kind=DBL_PREC) :: LIM_KG_FIX_CYN_DOXY(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NON_FIX_CYN_N(nkn), LIM_KG_NON_FIX_CYN_P(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NON_FIX_CYN_NUTR(nkn)
        real(kind=DBL_PREC) :: LIM_KG_FIX_FIX_CYN_N(nkn), LIM_KG_FIX_FIX_CYN_P(nkn)
        real(kind=DBL_PREC) :: LIM_KG_FIX_FIX_CYN_NUTR(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NON_FIX_CYN(nkn), LIM_KG_FIX_FIX_CYN(nkn)
        real(kind=DBL_PREC) :: R_NON_FIX_CYN_GROWTH(nkn), R_FIX_FIX_CYN_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_FIX_CYN_GROWTH(nkn), R_FIX_CYN_MET(nkn)
        real(kind=DBL_PREC) :: R_FIX_CYN_RESP(nkn), R_FIX_CYN_EXCR(nkn)
        real(kind=DBL_PREC) :: R_FIX_CYN_INT_RESP(nkn), KD_FIX_CYN(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_FIX_CYN_D(nkn), R_FIX_CYN_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4_DON_FIX_CYN(nkn)

        print *, "Test: K_E=0 does not produce NaN/Inf (BOUYANT, SMITH=1)"

        call set_default_fix_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 0.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.1D0; NO3_N = 0.5D0; DON = 0.1D0; PO4_P = 0.05D0
        FIX_CYN_C = 1.0D0

        FIX_CYN_LIGHT_SAT = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        KG_FIX_CYN = 0.0D0
        LIM_KG_FIX_CYN_LIGHT = 0.0D0; LIM_KG_FIX_CYN_TEMP = 0.0D0
        LIM_KG_FIX_CYN_DOXY = 0.0D0
        LIM_KG_NON_FIX_CYN_N = 0.0D0; LIM_KG_NON_FIX_CYN_P = 0.0D0
        LIM_KG_NON_FIX_CYN_NUTR = 0.0D0
        LIM_KG_FIX_FIX_CYN_N = 0.0D0; LIM_KG_FIX_FIX_CYN_P = 0.0D0
        LIM_KG_FIX_FIX_CYN_NUTR = 0.0D0
        LIM_KG_NON_FIX_CYN = 0.0D0; LIM_KG_FIX_FIX_CYN = 0.0D0
        R_NON_FIX_CYN_GROWTH = 0.0D0; R_FIX_FIX_CYN_GROWTH = 0.0D0
        R_FIX_CYN_GROWTH = 0.0D0; R_FIX_CYN_MET = 0.0D0
        R_FIX_CYN_RESP = 0.0D0; R_FIX_CYN_EXCR = 0.0D0
        R_FIX_CYN_INT_RESP = 0.0D0; KD_FIX_CYN = 0.0D0
        FAC_HYPOX_FIX_CYN_D = 0.0D0; R_FIX_CYN_DEATH = 0.0D0
        PREF_NH4_DON_FIX_CYN = 0.0D0

        ! Call BOUYANT variant with SMITH=1 to trigger euphotic depth
        call FIX_CYANOBACTERIA_BOUYANT(params, env, 1.0D0, 1, nkn, &
                NH4_N, NO3_N, DON, PO4_P, FIX_CYN_C, &
                FIX_CYN_LIGHT_SAT, ALPHA_0, ALPHA_1, &
                KG_FIX_CYN, LIM_KG_FIX_CYN_LIGHT, &
                LIM_KG_FIX_CYN_TEMP, LIM_KG_FIX_CYN_DOXY, &
                LIM_KG_NON_FIX_CYN_N, LIM_KG_NON_FIX_CYN_P, &
                LIM_KG_NON_FIX_CYN_NUTR, &
                LIM_KG_FIX_FIX_CYN_N, LIM_KG_FIX_FIX_CYN_P, &
                LIM_KG_FIX_FIX_CYN_NUTR, &
                LIM_KG_NON_FIX_CYN, LIM_KG_FIX_FIX_CYN, &
                R_NON_FIX_CYN_GROWTH, R_FIX_FIX_CYN_GROWTH, &
                R_FIX_CYN_GROWTH, R_FIX_CYN_MET, &
                R_FIX_CYN_RESP, R_FIX_CYN_EXCR, &
                R_FIX_CYN_INT_RESP, KD_FIX_CYN, &
                FAC_HYPOX_FIX_CYN_D, R_FIX_CYN_DEATH, &
                PREF_NH4_DON_FIX_CYN)

        call assert_finite(R_FIX_CYN_GROWTH(1), "Growth finite with K_E=0")
        call assert_finite(R_FIX_CYN_MET(1), "Metabolism finite with K_E=0")
        call assert_finite(R_FIX_CYN_DEATH(1), "Death finite with K_E=0")
    end subroutine test_ke_zero

end program test_fix_cyn
