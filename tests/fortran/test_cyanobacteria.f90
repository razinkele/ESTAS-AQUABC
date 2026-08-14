! Unit tests for CYANOBACTERIA kinetics subroutine
program test_cyanobacteria
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "CYANOBACTERIA Kinetics Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_smoke()
    call test_persistence_ratchet()
    call test_growth_positive()
    call test_warm_temperature_preference()
    call test_nutrient_n_limitation()
    call test_zero_biomass()
    call test_losses_bounded()
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

    ! Helper: run CYANOBACTERIA with given inputs, return outputs
    subroutine run_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                       TIME_STEP, nkn, &
                       KG_CYN, LIM_KG_CYN_TEMP, LIM_KG_CYN_N, &
                       R_CYN_GROWTH, R_CYN_MET, R_CYN_DEATH)
        type(t_cyn_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        integer, intent(in) :: nkn
        real(kind=DBL_PREC), intent(in) :: NH4_N(nkn), NO3_N(nkn), DON(nkn)
        real(kind=DBL_PREC), intent(in) :: PO4_P(nkn), CYN_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC), intent(in) :: TIME_STEP
        real(kind=DBL_PREC), intent(out) :: KG_CYN(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_CYN_TEMP(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_CYN_N(nkn)
        real(kind=DBL_PREC), intent(out) :: R_CYN_GROWTH(nkn)
        real(kind=DBL_PREC), intent(out) :: R_CYN_MET(nkn)
        real(kind=DBL_PREC), intent(out) :: R_CYN_DEATH(nkn)

        real(kind=DBL_PREC) :: CYN_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_CYN_LIGHT(nkn), LIM_KG_CYN_DOXY(nkn)
        real(kind=DBL_PREC) :: LIM_KG_CYN_P(nkn), LIM_KG_CYN_NUTR(nkn)
        real(kind=DBL_PREC) :: LIM_KG_CYN(nkn)
        real(kind=DBL_PREC) :: R_CYN_RESP(nkn), R_CYN_EXCR(nkn)
        real(kind=DBL_PREC) :: R_CYN_INT_RESP(nkn), KD_CYN(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_CYN_D(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DON_CYN(nkn)

        CYN_LIGHT_SAT = 0.0D0
        ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_CYN_LIGHT = 0.0D0; LIM_KG_CYN_DOXY = 0.0D0
        LIM_KG_CYN_P = 0.0D0; LIM_KG_CYN_NUTR = 0.0D0
        LIM_KG_CYN = 0.0D0
        KG_CYN = 0.0D0; LIM_KG_CYN_TEMP = 0.0D0; LIM_KG_CYN_N = 0.0D0
        R_CYN_GROWTH = 0.0D0; R_CYN_MET = 0.0D0
        R_CYN_RESP = 0.0D0; R_CYN_EXCR = 0.0D0
        R_CYN_INT_RESP = 0.0D0; KD_CYN = 0.0D0
        FAC_HYPOX_CYN_D = 0.0D0; R_CYN_DEATH = 0.0D0
        PREF_NH4N_DON_CYN = 0.0D0

        call CYANOBACTERIA(params, env, CYN_LIGHT_SAT, NH4_N, NO3_N, DON, &
                           PO4_P, CYN_C, ZOO_C, TIME_STEP, 1, nkn, &
                           KG_CYN, ALPHA_0, ALPHA_1, LIM_KG_CYN_TEMP, &
                           LIM_KG_CYN_LIGHT, LIM_KG_CYN_DOXY, LIM_KG_CYN_N, &
                           LIM_KG_CYN_P, LIM_KG_CYN_NUTR, LIM_KG_CYN, &
                           R_CYN_GROWTH, R_CYN_MET, R_CYN_RESP, R_CYN_EXCR, &
                           R_CYN_INT_RESP, KD_CYN, FAC_HYPOX_CYN_D, &
                           R_CYN_DEATH, PREF_NH4N_DON_CYN)
    end subroutine run_cyn

    ! Smoke test: typical conditions produce plausible outputs
    subroutine test_smoke()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: KG_CYN(nkn), LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)

        print *, "Test: Smoke test (typical conditions)"

        call set_default_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.1D0; NO3_N = 0.5D0; DON = 0.1D0; PO4_P = 0.05D0
        CYN_C = 1.0D0; ZOO_C = 0.5D0

        call run_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                     1.0D0, nkn, KG_CYN, LIM_T, LIM_N, R_GROWTH, R_MET, R_DEATH)

        call assert_not_nan(R_GROWTH(1), "Growth rate is not NaN")
        call assert_true(R_GROWTH(1) > 0.0D0, "Growth rate is positive")
    end subroutine test_smoke

    ! Growth is positive at optimal conditions
    subroutine test_growth_positive()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: KG_CYN(nkn), LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)

        print *, "Test: Growth is positive at optimal temp/nutrients"

        call set_default_cyn_params(params)
        ! Optimal temp for CYN is CYN_OPT_TEMP_UR = 26
        TEMP = 26.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.5D0; NO3_N = 1.0D0; DON = 0.5D0; PO4_P = 0.1D0
        CYN_C = 2.0D0; ZOO_C = 0.5D0

        call run_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                     1.0D0, nkn, KG_CYN, LIM_T, LIM_N, R_GROWTH, R_MET, R_DEATH)

        call assert_true(R_GROWTH(1) > 0.0D0, "Growth is positive")
        call assert_true(R_GROWTH(1) <= params%KG_CYN_OPT_TEMP * CYN_C(1), &
                         "Growth bounded by max rate * biomass")
    end subroutine test_growth_positive

    ! CYN prefer warmer water (OPT_TEMP_LR=15 vs DIA OPT_TEMP_LR=1)
    subroutine test_warm_temperature_preference()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP_cold(nkn), TEMP_warm(nkn)
        real(kind=DBL_PREC), target :: I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: KG1(nkn), LIM_T_cold(nkn), LIM_N1(nkn)
        real(kind=DBL_PREC) :: R_G1(nkn), R_M1(nkn), R_D1(nkn)
        real(kind=DBL_PREC) :: KG2(nkn), LIM_T_warm(nkn), LIM_N2(nkn)
        real(kind=DBL_PREC) :: R_G2(nkn), R_M2(nkn), R_D2(nkn)

        print *, "Test: CYN prefer warm water (low temp gives low LIM_TEMP)"

        call set_default_cyn_params(params)
        I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0

        NH4_N = 0.5D0; NO3_N = 1.0D0; DON = 0.5D0; PO4_P = 0.1D0
        CYN_C = 1.0D0; ZOO_C = 0.5D0

        ! Cold water (below CYN_OPT_TEMP_LR = 15)
        TEMP_cold = 10.0D0
        call setup_phyto_env(env, TEMP_cold, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)
        call run_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                     1.0D0, nkn, KG1, LIM_T_cold, LIM_N1, R_G1, R_M1, R_D1)

        ! Warm water (at optimal)
        TEMP_warm = 26.0D0
        call setup_phyto_env(env, TEMP_warm, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)
        call run_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                     1.0D0, nkn, KG2, LIM_T_warm, LIM_N2, R_G2, R_M2, R_D2)

        call assert_true(LIM_T_warm(1) > LIM_T_cold(1), &
                         "Warmer water gives higher temp limitation factor")
    end subroutine test_warm_temperature_preference

    ! Low DIN should limit growth
    subroutine test_nutrient_n_limitation()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: KG_CYN(nkn), LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)

        print *, "Test: Low DIN limits cyanobacteria growth"

        call set_default_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.001D0; NO3_N = 0.001D0; DON = 0.0D0  ! Very low N
        PO4_P = 0.1D0; CYN_C = 1.0D0; ZOO_C = 0.5D0

        call run_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                     1.0D0, nkn, KG_CYN, LIM_T, LIM_N, R_GROWTH, R_MET, R_DEATH)

        call assert_true(LIM_N(1) < 0.5D0, "N limitation < 0.5 under low DIN")
    end subroutine test_nutrient_n_limitation

    ! Zero biomass should produce zero rates
    subroutine test_zero_biomass()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: KG_CYN(nkn), LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)

        print *, "Test: Zero biomass gives zero rates"

        call set_default_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.5D0; NO3_N = 1.0D0; DON = 0.5D0; PO4_P = 0.1D0
        CYN_C = 0.0D0  ! Zero biomass
        ZOO_C = 0.5D0

        call run_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                     1.0D0, nkn, KG_CYN, LIM_T, LIM_N, R_GROWTH, R_MET, R_DEATH)

        call assert_not_nan(R_GROWTH(1), "Growth not NaN with zero biomass")
        call assert_true(abs(R_GROWTH(1)) < 1.0D-10, "Growth near zero")
        call assert_true(abs(R_DEATH(1)) < 1.0D-10, "Death near zero")
    end subroutine test_zero_biomass

    ! Losses bounded
    subroutine test_losses_bounded()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: KG_CYN(nkn), LIM_T(nkn), LIM_N(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: TS

        print *, "Test: Losses bounded below 50%% of biomass"

        call set_default_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.5D0; NO3_N = 1.0D0; DON = 0.5D0; PO4_P = 0.1D0
        CYN_C = 2.0D0; ZOO_C = 0.5D0
        TS = 1.0D0

        call run_cyn(params, env, NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                     TS, nkn, KG_CYN, LIM_T, LIM_N, R_GROWTH, R_MET, R_DEATH)

        call assert_true((R_MET(1) + R_DEATH(1)) * TS < 0.5D0 * CYN_C(1), &
                         "Met + death < 50% biomass per timestep")
    end subroutine test_losses_bounded

    ! K_E = 0 should not produce NaN or Inf (division by zero guard)
    ! Uses CYANOBACTERIA_BOUYANT with SMITH=1 to trigger euphotic depth path
    subroutine test_ke_zero()
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), DON(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: S_TEST(1)
        real(kind=DBL_PREC) :: CYN_C(nkn), ZOO_C(nkn)

        real(kind=DBL_PREC) :: CYN_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: KG_CYN(nkn)
        real(kind=DBL_PREC) :: LIM_KG_CYN_TEMP(nkn), LIM_KG_CYN_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_CYN_DOXY(nkn)
        real(kind=DBL_PREC) :: LIM_KG_CYN_N(nkn), LIM_KG_CYN_P(nkn)
        real(kind=DBL_PREC) :: LIM_KG_CYN_NUTR(nkn), LIM_KG_CYN(nkn)
        real(kind=DBL_PREC) :: R_CYN_GROWTH(nkn), R_CYN_MET(nkn)
        real(kind=DBL_PREC) :: R_CYN_RESP(nkn), R_CYN_EXCR(nkn)
        real(kind=DBL_PREC) :: R_CYN_INT_RESP(nkn), KD_CYN(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_CYN_D(nkn), R_CYN_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_DIN_DON_CYN(nkn), PREF_NH4N_CYN(nkn)

        print *, "Test: K_E=0 does not produce NaN/Inf (BOUYANT, SMITH=1)"

        call set_default_cyn_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 0.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        NH4_N = 0.1D0; NO3_N = 0.5D0; DON = 0.1D0; PO4_P = 0.05D0
        CYN_C = 1.0D0; ZOO_C = 0.5D0

        CYN_LIGHT_SAT = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        KG_CYN = 0.0D0; LIM_KG_CYN_TEMP = 0.0D0; LIM_KG_CYN_LIGHT = 0.0D0
        LIM_KG_CYN_DOXY = 0.0D0; LIM_KG_CYN_N = 0.0D0; LIM_KG_CYN_P = 0.0D0
        LIM_KG_CYN_NUTR = 0.0D0; LIM_KG_CYN = 0.0D0
        R_CYN_GROWTH = 0.0D0; R_CYN_MET = 0.0D0
        R_CYN_RESP = 0.0D0; R_CYN_EXCR = 0.0D0
        R_CYN_INT_RESP = 0.0D0; KD_CYN = 0.0D0
        FAC_HYPOX_CYN_D = 0.0D0; R_CYN_DEATH = 0.0D0
        PREF_DIN_DON_CYN = 0.0D0; PREF_NH4N_CYN = 0.0D0

        ! Call BOUYANT variant with SMITH=1 to trigger euphotic depth
        S_TEST = 0.0D0
        call CYANOBACTERIA_BOUYANT(params, env, CYN_LIGHT_SAT, &
                NH4_N, NO3_N, DON, PO4_P, CYN_C, ZOO_C, &
                1.0D0, 1, nkn, &
                KG_CYN, ALPHA_0, ALPHA_1, LIM_KG_CYN_TEMP, &
                LIM_KG_CYN_LIGHT, LIM_KG_CYN_DOXY, LIM_KG_CYN_N, &
                LIM_KG_CYN_P, LIM_KG_CYN_NUTR, LIM_KG_CYN, &
                R_CYN_GROWTH, R_CYN_MET, R_CYN_RESP, R_CYN_EXCR, &
                R_CYN_INT_RESP, KD_CYN, FAC_HYPOX_CYN_D, &
                R_CYN_DEATH, PREF_DIN_DON_CYN, PREF_NH4N_CYN, &
                     0, 0.5D0, 0.0D0, S_TEST)

        call assert_finite(R_CYN_GROWTH(1), "Growth finite with K_E=0")
        call assert_finite(R_CYN_MET(1), "Metabolism finite with K_E=0")
        call assert_finite(R_CYN_DEATH(1), "Death finite with K_E=0")
    end subroutine test_ke_zero

    ! Positional ratchet (CYANO_POS_MODEL = 2): repeated calm days build the
    ! surface fraction S (raising the light factor across calls); one stormy
    ! day collapses it. Drives the S slice directly (module state untouched).
    subroutine test_persistence_ratchet()
        use AQUABC_POSITIONING_STATE, only: CALM_FRACTION
        integer, parameter :: nkn = 1
        type(t_cyn_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC), target :: W_CALM(nkn), W_STORM(nkn)
        real(kind=DBL_PREC) :: S_R(nkn), LIM1(nkn), LIM3(nkn), LIMS(nkn)
        real(kind=DBL_PREC) :: S_after_calm
        integer :: i

        print *, "Test: positional ratchet (CYANO_POS_MODEL=2)"

        call set_default_cyn_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 2.9D0
        DEPTH = 3.8D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        W_CALM = 1.0D0; W_STORM = 12.0D0

        ! sanity on the CDF itself
        call assert_true(CALM_FRACTION(1.0D0, 3.0D0) > 0.99D0, &
            "CDF: fully calm day -> F ~ 1")
        call assert_true(1.0D0 - CALM_FRACTION(1.0D0, 4.0D0) < 0.01D0, &
            "CDF: storm fraction ~ 0 on a calm day")

        S_R = 0.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, W_CALM)
        call run_cyn_pos(params, env, S_R, LIM1)          ! day 1
        S_after_calm = S_R(1)
        call assert_true(S_after_calm > 0.5D0, "one calm day builds S > 0.5")
        call run_cyn_pos(params, env, S_R, LIM3)          ! days 2-3
        call run_cyn_pos(params, env, S_R, LIM3)
        call assert_true(S_R(1) > S_after_calm - 1.0D-12, "S persists/ratchets across days")
        call assert_true(LIM3(1) > LIM1(1) - 1.0D-12, "light factor rises with S")

        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, W_STORM)
        call run_cyn_pos(params, env, S_R, LIMS)          ! storm day
        call assert_true(S_R(1) < 0.05D0, "one storm day collapses S")
    end subroutine test_persistence_ratchet

    ! Helper: one CYANOBACTERIA_BOUYANT call with the ratchet on, returning the
    ! light limitation and updating the caller's S slice.
    subroutine run_cyn_pos(params, env, S_R, LIM_OUT)
        type(t_cyn_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        real(kind=DBL_PREC), intent(inout) :: S_R(1)
        real(kind=DBL_PREC), intent(out) :: LIM_OUT(1)
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: CYN_LIGHT_SAT(nkn), NH4(nkn), NO3(nkn), DON(nkn)
        real(kind=DBL_PREC) :: PO4(nkn), CYN_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: KG(nkn), A0(nkn), A1(nkn)
        real(kind=DBL_PREC) :: LT(nkn), LL(nkn), LD(nkn), LN(nkn), LP(nkn), LNU(nkn), LK(nkn)
        real(kind=DBL_PREC) :: RG(nkn), RM(nkn), RR(nkn), RE(nkn), RI(nkn)
        real(kind=DBL_PREC) :: KD(nkn), FH(nkn), RD(nkn), PDD(nkn), PNH(nkn)

        CYN_LIGHT_SAT = 0.0D0; NH4 = 0.05D0; NO3 = 0.3D0; DON = 0.5D0
        PO4 = 0.05D0; CYN_C = 0.5D0; ZOO_C = 0.01D0
        KG=0; A0=0; A1=0; LT=0; LL=0; LD=0; LN=0; LP=0; LNU=0; LK=0
        RG=0; RM=0; RR=0; RE=0; RI=0; KD=0; FH=0; RD=0; PDD=0; PNH=0

        call CYANOBACTERIA_BOUYANT(params, env, CYN_LIGHT_SAT, &
                     NH4, NO3, DON, PO4, CYN_C, ZOO_C, 1.0D0, 1, nkn, &
                     KG, A0, A1, LT, LL, LD, LN, LP, LNU, LK, &
                     RG, RM, RR, RE, RI, KD, FH, RD, PDD, PNH, &
                     2, 0.5D0, 3.0D0, S_R)
        LIM_OUT = LL
    end subroutine run_cyn_pos

end program test_cyanobacteria
