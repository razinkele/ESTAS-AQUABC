! Unit tests for DIATOMS kinetics subroutine
program test_diatoms
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "DIATOMS Kinetics Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_smoke()
    call test_growth_positive()
    call test_nutrient_n_limitation()
    call test_nutrient_p_limitation()
    call test_silica_limitation()
    call test_temperature_response()
    call test_zero_biomass()
    call test_losses_bounded()

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
        if (value == value) then  ! NaN /= NaN
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A)', "    Value was NaN"
            failed = failed + 1
        end if
    end subroutine assert_not_nan

    ! Smoke test: typical conditions produce plausible outputs
    subroutine test_smoke()
        integer, parameter :: nkn = 1
        type(t_diatom_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), ZOO_C(nkn), DISS_Si(nkn)
        real(kind=DBL_PREC) :: DIA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_DIA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_TEMP(nkn), LIM_KG_DIA_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_DOXY(nkn), LIM_KG_DIA_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_P(nkn), LIM_KG_DIA_DISS_Si(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_NUTR(nkn), LIM_KG_DIA(nkn)
        real(kind=DBL_PREC) :: R_DIA_GROWTH(nkn), R_DIA_MET(nkn)
        real(kind=DBL_PREC) :: R_DIA_RESP(nkn), R_DIA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_DIA_INT_RESP(nkn), KD_DIA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_DIA_D(nkn), R_DIA_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DIA(nkn)
        integer :: SMITH

        print *, "Test: Smoke test (typical conditions)"

        call set_default_diatom_params(params)

        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.1D0; NO3_N = 0.5D0; PO4_P = 0.05D0
        DIA_C = 1.0D0; ZOO_C = 0.5D0; DISS_Si = 0.5D0
        DIA_LIGHT_SAT = 0.0D0
        SMITH = 1

        KG_DIA = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_DIA_TEMP = 0.0D0; LIM_KG_DIA_LIGHT = 0.0D0
        LIM_KG_DIA_DOXY = 0.0D0; LIM_KG_DIA_N = 0.0D0
        LIM_KG_DIA_P = 0.0D0; LIM_KG_DIA_DISS_Si = 0.0D0
        LIM_KG_DIA_NUTR = 0.0D0; LIM_KG_DIA = 0.0D0
        R_DIA_GROWTH = 0.0D0; R_DIA_MET = 0.0D0
        R_DIA_RESP = 0.0D0; R_DIA_EXCR = 0.0D0
        R_DIA_INT_RESP = 0.0D0; KD_DIA = 0.0D0
        FAC_HYPOX_DIA_D = 0.0D0; R_DIA_DEATH = 0.0D0
        PREF_NH4N_DIA = 0.0D0

        call DIATOMS(params, env, DIA_LIGHT_SAT, NH4_N, NO3_N, PO4_P, &
                     DIA_C, ZOO_C, DISS_Si, 1.0D0, SMITH, nkn, &
                     KG_DIA, ALPHA_0, ALPHA_1, LIM_KG_DIA_TEMP, &
                     LIM_KG_DIA_LIGHT, LIM_KG_DIA_DOXY, LIM_KG_DIA_N, &
                     LIM_KG_DIA_P, LIM_KG_DIA_DISS_Si, LIM_KG_DIA_NUTR, &
                     LIM_KG_DIA, R_DIA_GROWTH, R_DIA_MET, R_DIA_RESP, &
                     R_DIA_EXCR, R_DIA_INT_RESP, KD_DIA, FAC_HYPOX_DIA_D, &
                     R_DIA_DEATH, PREF_NH4N_DIA)

        call assert_not_nan(R_DIA_GROWTH(1), "Growth rate is not NaN")
        call assert_true(R_DIA_GROWTH(1) > 0.0D0, "Growth rate is positive")
    end subroutine test_smoke

    ! Growth should be positive under favorable conditions
    subroutine test_growth_positive()
        integer, parameter :: nkn = 1
        type(t_diatom_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), ZOO_C(nkn), DISS_Si(nkn)
        real(kind=DBL_PREC) :: DIA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_DIA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_TEMP(nkn), LIM_KG_DIA_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_DOXY(nkn), LIM_KG_DIA_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_P(nkn), LIM_KG_DIA_DISS_Si(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_NUTR(nkn), LIM_KG_DIA(nkn)
        real(kind=DBL_PREC) :: R_DIA_GROWTH(nkn), R_DIA_MET(nkn)
        real(kind=DBL_PREC) :: R_DIA_RESP(nkn), R_DIA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_DIA_INT_RESP(nkn), KD_DIA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_DIA_D(nkn), R_DIA_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DIA(nkn)

        print *, "Test: Growth is positive and bounded"

        call set_default_diatom_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        DIA_C = 2.0D0; ZOO_C = 0.5D0; DISS_Si = 1.0D0
        DIA_LIGHT_SAT = 0.0D0

        KG_DIA = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_DIA_TEMP = 0.0D0; LIM_KG_DIA_LIGHT = 0.0D0
        LIM_KG_DIA_DOXY = 0.0D0; LIM_KG_DIA_N = 0.0D0
        LIM_KG_DIA_P = 0.0D0; LIM_KG_DIA_DISS_Si = 0.0D0
        LIM_KG_DIA_NUTR = 0.0D0; LIM_KG_DIA = 0.0D0
        R_DIA_GROWTH = 0.0D0; R_DIA_MET = 0.0D0
        R_DIA_RESP = 0.0D0; R_DIA_EXCR = 0.0D0
        R_DIA_INT_RESP = 0.0D0; KD_DIA = 0.0D0
        FAC_HYPOX_DIA_D = 0.0D0; R_DIA_DEATH = 0.0D0
        PREF_NH4N_DIA = 0.0D0

        call DIATOMS(params, env, DIA_LIGHT_SAT, NH4_N, NO3_N, PO4_P, &
                     DIA_C, ZOO_C, DISS_Si, 1.0D0, 1, nkn, &
                     KG_DIA, ALPHA_0, ALPHA_1, LIM_KG_DIA_TEMP, &
                     LIM_KG_DIA_LIGHT, LIM_KG_DIA_DOXY, LIM_KG_DIA_N, &
                     LIM_KG_DIA_P, LIM_KG_DIA_DISS_Si, LIM_KG_DIA_NUTR, &
                     LIM_KG_DIA, R_DIA_GROWTH, R_DIA_MET, R_DIA_RESP, &
                     R_DIA_EXCR, R_DIA_INT_RESP, KD_DIA, FAC_HYPOX_DIA_D, &
                     R_DIA_DEATH, PREF_NH4N_DIA)

        ! Growth bounded by KG_DIA_OPT_TEMP * DIA_C
        call assert_true(R_DIA_GROWTH(1) <= params%KG_DIA_OPT_TEMP * DIA_C(1), &
                         "Growth bounded by max rate * biomass")
    end subroutine test_growth_positive

    ! Low DIN should limit growth
    subroutine test_nutrient_n_limitation()
        integer, parameter :: nkn = 1
        type(t_diatom_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), ZOO_C(nkn), DISS_Si(nkn)
        real(kind=DBL_PREC) :: DIA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_DIA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_TEMP(nkn), LIM_KG_DIA_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_DOXY(nkn), LIM_KG_DIA_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_P(nkn), LIM_KG_DIA_DISS_Si(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_NUTR(nkn), LIM_KG_DIA(nkn)
        real(kind=DBL_PREC) :: R_DIA_GROWTH(nkn), R_DIA_MET(nkn)
        real(kind=DBL_PREC) :: R_DIA_RESP(nkn), R_DIA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_DIA_INT_RESP(nkn), KD_DIA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_DIA_D(nkn), R_DIA_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DIA(nkn)

        print *, "Test: Low DIN limits growth"

        call set_default_diatom_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        ! Very low nitrogen
        NH4_N = 0.001D0; NO3_N = 0.001D0
        PO4_P = 0.1D0; DIA_C = 1.0D0; ZOO_C = 0.5D0; DISS_Si = 1.0D0
        DIA_LIGHT_SAT = 0.0D0

        KG_DIA = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_DIA_TEMP = 0.0D0; LIM_KG_DIA_LIGHT = 0.0D0
        LIM_KG_DIA_DOXY = 0.0D0; LIM_KG_DIA_N = 0.0D0
        LIM_KG_DIA_P = 0.0D0; LIM_KG_DIA_DISS_Si = 0.0D0
        LIM_KG_DIA_NUTR = 0.0D0; LIM_KG_DIA = 0.0D0
        R_DIA_GROWTH = 0.0D0; R_DIA_MET = 0.0D0
        R_DIA_RESP = 0.0D0; R_DIA_EXCR = 0.0D0
        R_DIA_INT_RESP = 0.0D0; KD_DIA = 0.0D0
        FAC_HYPOX_DIA_D = 0.0D0; R_DIA_DEATH = 0.0D0
        PREF_NH4N_DIA = 0.0D0

        call DIATOMS(params, env, DIA_LIGHT_SAT, NH4_N, NO3_N, PO4_P, &
                     DIA_C, ZOO_C, DISS_Si, 1.0D0, 1, nkn, &
                     KG_DIA, ALPHA_0, ALPHA_1, LIM_KG_DIA_TEMP, &
                     LIM_KG_DIA_LIGHT, LIM_KG_DIA_DOXY, LIM_KG_DIA_N, &
                     LIM_KG_DIA_P, LIM_KG_DIA_DISS_Si, LIM_KG_DIA_NUTR, &
                     LIM_KG_DIA, R_DIA_GROWTH, R_DIA_MET, R_DIA_RESP, &
                     R_DIA_EXCR, R_DIA_INT_RESP, KD_DIA, FAC_HYPOX_DIA_D, &
                     R_DIA_DEATH, PREF_NH4N_DIA)

        call assert_true(LIM_KG_DIA_N(1) < 0.5D0, "N limitation < 0.5 under low DIN")
    end subroutine test_nutrient_n_limitation

    ! Low PO4 should limit growth
    subroutine test_nutrient_p_limitation()
        integer, parameter :: nkn = 1
        type(t_diatom_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), ZOO_C(nkn), DISS_Si(nkn)
        real(kind=DBL_PREC) :: DIA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_DIA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_TEMP(nkn), LIM_KG_DIA_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_DOXY(nkn), LIM_KG_DIA_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_P(nkn), LIM_KG_DIA_DISS_Si(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_NUTR(nkn), LIM_KG_DIA(nkn)
        real(kind=DBL_PREC) :: R_DIA_GROWTH(nkn), R_DIA_MET(nkn)
        real(kind=DBL_PREC) :: R_DIA_RESP(nkn), R_DIA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_DIA_INT_RESP(nkn), KD_DIA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_DIA_D(nkn), R_DIA_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DIA(nkn)

        print *, "Test: Low PO4 limits growth"

        call set_default_diatom_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0
        PO4_P = 0.001D0  ! Very low P
        DIA_C = 1.0D0; ZOO_C = 0.5D0; DISS_Si = 1.0D0
        DIA_LIGHT_SAT = 0.0D0

        KG_DIA = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_DIA_TEMP = 0.0D0; LIM_KG_DIA_LIGHT = 0.0D0
        LIM_KG_DIA_DOXY = 0.0D0; LIM_KG_DIA_N = 0.0D0
        LIM_KG_DIA_P = 0.0D0; LIM_KG_DIA_DISS_Si = 0.0D0
        LIM_KG_DIA_NUTR = 0.0D0; LIM_KG_DIA = 0.0D0
        R_DIA_GROWTH = 0.0D0; R_DIA_MET = 0.0D0
        R_DIA_RESP = 0.0D0; R_DIA_EXCR = 0.0D0
        R_DIA_INT_RESP = 0.0D0; KD_DIA = 0.0D0
        FAC_HYPOX_DIA_D = 0.0D0; R_DIA_DEATH = 0.0D0
        PREF_NH4N_DIA = 0.0D0

        call DIATOMS(params, env, DIA_LIGHT_SAT, NH4_N, NO3_N, PO4_P, &
                     DIA_C, ZOO_C, DISS_Si, 1.0D0, 1, nkn, &
                     KG_DIA, ALPHA_0, ALPHA_1, LIM_KG_DIA_TEMP, &
                     LIM_KG_DIA_LIGHT, LIM_KG_DIA_DOXY, LIM_KG_DIA_N, &
                     LIM_KG_DIA_P, LIM_KG_DIA_DISS_Si, LIM_KG_DIA_NUTR, &
                     LIM_KG_DIA, R_DIA_GROWTH, R_DIA_MET, R_DIA_RESP, &
                     R_DIA_EXCR, R_DIA_INT_RESP, KD_DIA, FAC_HYPOX_DIA_D, &
                     R_DIA_DEATH, PREF_NH4N_DIA)

        call assert_true(LIM_KG_DIA_P(1) < 0.5D0, "P limitation < 0.5 under low PO4")
    end subroutine test_nutrient_p_limitation

    ! Low dissolved silica should limit diatom growth (unique to diatoms)
    subroutine test_silica_limitation()
        integer, parameter :: nkn = 1
        type(t_diatom_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), ZOO_C(nkn), DISS_Si(nkn)
        real(kind=DBL_PREC) :: DIA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_DIA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_TEMP(nkn), LIM_KG_DIA_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_DOXY(nkn), LIM_KG_DIA_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_P(nkn), LIM_KG_DIA_DISS_Si(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_NUTR(nkn), LIM_KG_DIA(nkn)
        real(kind=DBL_PREC) :: R_DIA_GROWTH(nkn), R_DIA_MET(nkn)
        real(kind=DBL_PREC) :: R_DIA_RESP(nkn), R_DIA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_DIA_INT_RESP(nkn), KD_DIA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_DIA_D(nkn), R_DIA_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DIA(nkn)

        print *, "Test: Low dissolved silica limits diatom growth"

        call set_default_diatom_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        DIA_C = 1.0D0; ZOO_C = 0.5D0
        DISS_Si = 0.002D0  ! Very low silica
        DIA_LIGHT_SAT = 0.0D0

        KG_DIA = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_DIA_TEMP = 0.0D0; LIM_KG_DIA_LIGHT = 0.0D0
        LIM_KG_DIA_DOXY = 0.0D0; LIM_KG_DIA_N = 0.0D0
        LIM_KG_DIA_P = 0.0D0; LIM_KG_DIA_DISS_Si = 0.0D0
        LIM_KG_DIA_NUTR = 0.0D0; LIM_KG_DIA = 0.0D0
        R_DIA_GROWTH = 0.0D0; R_DIA_MET = 0.0D0
        R_DIA_RESP = 0.0D0; R_DIA_EXCR = 0.0D0
        R_DIA_INT_RESP = 0.0D0; KD_DIA = 0.0D0
        FAC_HYPOX_DIA_D = 0.0D0; R_DIA_DEATH = 0.0D0
        PREF_NH4N_DIA = 0.0D0

        call DIATOMS(params, env, DIA_LIGHT_SAT, NH4_N, NO3_N, PO4_P, &
                     DIA_C, ZOO_C, DISS_Si, 1.0D0, 1, nkn, &
                     KG_DIA, ALPHA_0, ALPHA_1, LIM_KG_DIA_TEMP, &
                     LIM_KG_DIA_LIGHT, LIM_KG_DIA_DOXY, LIM_KG_DIA_N, &
                     LIM_KG_DIA_P, LIM_KG_DIA_DISS_Si, LIM_KG_DIA_NUTR, &
                     LIM_KG_DIA, R_DIA_GROWTH, R_DIA_MET, R_DIA_RESP, &
                     R_DIA_EXCR, R_DIA_INT_RESP, KD_DIA, FAC_HYPOX_DIA_D, &
                     R_DIA_DEATH, PREF_NH4N_DIA)

        call assert_true(LIM_KG_DIA_DISS_Si(1) < 0.5D0, &
                         "Si limitation < 0.5 under low dissolved silica")
    end subroutine test_silica_limitation

    ! Optimal temperature should give high temperature limitation factor
    subroutine test_temperature_response()
        integer, parameter :: nkn = 1
        type(t_diatom_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), ZOO_C(nkn), DISS_Si(nkn)
        real(kind=DBL_PREC) :: DIA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_DIA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_TEMP(nkn), LIM_KG_DIA_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_DOXY(nkn), LIM_KG_DIA_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_P(nkn), LIM_KG_DIA_DISS_Si(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_NUTR(nkn), LIM_KG_DIA(nkn)
        real(kind=DBL_PREC) :: R_DIA_GROWTH(nkn), R_DIA_MET(nkn)
        real(kind=DBL_PREC) :: R_DIA_RESP(nkn), R_DIA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_DIA_INT_RESP(nkn), KD_DIA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_DIA_D(nkn), R_DIA_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DIA(nkn)

        print *, "Test: Optimal temperature gives high temp limitation"

        call set_default_diatom_params(params)
        ! Optimal temp for diatoms is DIA_OPT_TEMP_UR = 24
        TEMP = 24.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        DIA_C = 1.0D0; ZOO_C = 0.5D0; DISS_Si = 1.0D0
        DIA_LIGHT_SAT = 0.0D0

        KG_DIA = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_DIA_TEMP = 0.0D0; LIM_KG_DIA_LIGHT = 0.0D0
        LIM_KG_DIA_DOXY = 0.0D0; LIM_KG_DIA_N = 0.0D0
        LIM_KG_DIA_P = 0.0D0; LIM_KG_DIA_DISS_Si = 0.0D0
        LIM_KG_DIA_NUTR = 0.0D0; LIM_KG_DIA = 0.0D0
        R_DIA_GROWTH = 0.0D0; R_DIA_MET = 0.0D0
        R_DIA_RESP = 0.0D0; R_DIA_EXCR = 0.0D0
        R_DIA_INT_RESP = 0.0D0; KD_DIA = 0.0D0
        FAC_HYPOX_DIA_D = 0.0D0; R_DIA_DEATH = 0.0D0
        PREF_NH4N_DIA = 0.0D0

        call DIATOMS(params, env, DIA_LIGHT_SAT, NH4_N, NO3_N, PO4_P, &
                     DIA_C, ZOO_C, DISS_Si, 1.0D0, 1, nkn, &
                     KG_DIA, ALPHA_0, ALPHA_1, LIM_KG_DIA_TEMP, &
                     LIM_KG_DIA_LIGHT, LIM_KG_DIA_DOXY, LIM_KG_DIA_N, &
                     LIM_KG_DIA_P, LIM_KG_DIA_DISS_Si, LIM_KG_DIA_NUTR, &
                     LIM_KG_DIA, R_DIA_GROWTH, R_DIA_MET, R_DIA_RESP, &
                     R_DIA_EXCR, R_DIA_INT_RESP, KD_DIA, FAC_HYPOX_DIA_D, &
                     R_DIA_DEATH, PREF_NH4N_DIA)

        call assert_true(LIM_KG_DIA_TEMP(1) > 0.9D0, &
                         "Temp limitation near 1.0 at optimal temperature")
    end subroutine test_temperature_response

    ! Zero biomass should produce zero rates
    subroutine test_zero_biomass()
        integer, parameter :: nkn = 1
        type(t_diatom_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), ZOO_C(nkn), DISS_Si(nkn)
        real(kind=DBL_PREC) :: DIA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_DIA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_TEMP(nkn), LIM_KG_DIA_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_DOXY(nkn), LIM_KG_DIA_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_P(nkn), LIM_KG_DIA_DISS_Si(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_NUTR(nkn), LIM_KG_DIA(nkn)
        real(kind=DBL_PREC) :: R_DIA_GROWTH(nkn), R_DIA_MET(nkn)
        real(kind=DBL_PREC) :: R_DIA_RESP(nkn), R_DIA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_DIA_INT_RESP(nkn), KD_DIA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_DIA_D(nkn), R_DIA_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DIA(nkn)

        print *, "Test: Zero biomass gives zero rates"

        call set_default_diatom_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        DIA_C = 0.0D0  ! Zero biomass
        ZOO_C = 0.5D0; DISS_Si = 1.0D0
        DIA_LIGHT_SAT = 0.0D0

        KG_DIA = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_DIA_TEMP = 0.0D0; LIM_KG_DIA_LIGHT = 0.0D0
        LIM_KG_DIA_DOXY = 0.0D0; LIM_KG_DIA_N = 0.0D0
        LIM_KG_DIA_P = 0.0D0; LIM_KG_DIA_DISS_Si = 0.0D0
        LIM_KG_DIA_NUTR = 0.0D0; LIM_KG_DIA = 0.0D0
        R_DIA_GROWTH = 0.0D0; R_DIA_MET = 0.0D0
        R_DIA_RESP = 0.0D0; R_DIA_EXCR = 0.0D0
        R_DIA_INT_RESP = 0.0D0; KD_DIA = 0.0D0
        FAC_HYPOX_DIA_D = 0.0D0; R_DIA_DEATH = 0.0D0
        PREF_NH4N_DIA = 0.0D0

        call DIATOMS(params, env, DIA_LIGHT_SAT, NH4_N, NO3_N, PO4_P, &
                     DIA_C, ZOO_C, DISS_Si, 1.0D0, 1, nkn, &
                     KG_DIA, ALPHA_0, ALPHA_1, LIM_KG_DIA_TEMP, &
                     LIM_KG_DIA_LIGHT, LIM_KG_DIA_DOXY, LIM_KG_DIA_N, &
                     LIM_KG_DIA_P, LIM_KG_DIA_DISS_Si, LIM_KG_DIA_NUTR, &
                     LIM_KG_DIA, R_DIA_GROWTH, R_DIA_MET, R_DIA_RESP, &
                     R_DIA_EXCR, R_DIA_INT_RESP, KD_DIA, FAC_HYPOX_DIA_D, &
                     R_DIA_DEATH, PREF_NH4N_DIA)

        call assert_not_nan(R_DIA_GROWTH(1), "Growth not NaN with zero biomass")
        call assert_true(abs(R_DIA_GROWTH(1)) < 1.0D-10, &
                         "Growth near zero with zero biomass")
        call assert_true(abs(R_DIA_DEATH(1)) < 1.0D-10, &
                         "Death near zero with zero biomass")
    end subroutine test_zero_biomass

    ! Losses should not exceed biomass in a single timestep
    subroutine test_losses_bounded()
        integer, parameter :: nkn = 1
        type(t_diatom_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: NH4_N(nkn), NO3_N(nkn), PO4_P(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), ZOO_C(nkn), DISS_Si(nkn)
        real(kind=DBL_PREC) :: DIA_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_DIA(nkn), ALPHA_0(nkn), ALPHA_1(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_TEMP(nkn), LIM_KG_DIA_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_DOXY(nkn), LIM_KG_DIA_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_P(nkn), LIM_KG_DIA_DISS_Si(nkn)
        real(kind=DBL_PREC) :: LIM_KG_DIA_NUTR(nkn), LIM_KG_DIA(nkn)
        real(kind=DBL_PREC) :: R_DIA_GROWTH(nkn), R_DIA_MET(nkn)
        real(kind=DBL_PREC) :: R_DIA_RESP(nkn), R_DIA_EXCR(nkn)
        real(kind=DBL_PREC) :: R_DIA_INT_RESP(nkn), KD_DIA(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_DIA_D(nkn), R_DIA_DEATH(nkn)
        real(kind=DBL_PREC) :: PREF_NH4N_DIA(nkn)
        real(kind=DBL_PREC) :: TIME_STEP

        print *, "Test: Losses bounded below 50%% of biomass per timestep"

        call set_default_diatom_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        NH4_N = 0.5D0; NO3_N = 1.0D0; PO4_P = 0.1D0
        DIA_C = 2.0D0; ZOO_C = 0.5D0; DISS_Si = 1.0D0
        DIA_LIGHT_SAT = 0.0D0
        TIME_STEP = 1.0D0

        KG_DIA = 0.0D0; ALPHA_0 = 0.0D0; ALPHA_1 = 0.0D0
        LIM_KG_DIA_TEMP = 0.0D0; LIM_KG_DIA_LIGHT = 0.0D0
        LIM_KG_DIA_DOXY = 0.0D0; LIM_KG_DIA_N = 0.0D0
        LIM_KG_DIA_P = 0.0D0; LIM_KG_DIA_DISS_Si = 0.0D0
        LIM_KG_DIA_NUTR = 0.0D0; LIM_KG_DIA = 0.0D0
        R_DIA_GROWTH = 0.0D0; R_DIA_MET = 0.0D0
        R_DIA_RESP = 0.0D0; R_DIA_EXCR = 0.0D0
        R_DIA_INT_RESP = 0.0D0; KD_DIA = 0.0D0
        FAC_HYPOX_DIA_D = 0.0D0; R_DIA_DEATH = 0.0D0
        PREF_NH4N_DIA = 0.0D0

        call DIATOMS(params, env, DIA_LIGHT_SAT, NH4_N, NO3_N, PO4_P, &
                     DIA_C, ZOO_C, DISS_Si, TIME_STEP, 1, nkn, &
                     KG_DIA, ALPHA_0, ALPHA_1, LIM_KG_DIA_TEMP, &
                     LIM_KG_DIA_LIGHT, LIM_KG_DIA_DOXY, LIM_KG_DIA_N, &
                     LIM_KG_DIA_P, LIM_KG_DIA_DISS_Si, LIM_KG_DIA_NUTR, &
                     LIM_KG_DIA, R_DIA_GROWTH, R_DIA_MET, R_DIA_RESP, &
                     R_DIA_EXCR, R_DIA_INT_RESP, KD_DIA, FAC_HYPOX_DIA_D, &
                     R_DIA_DEATH, PREF_NH4N_DIA)

        call assert_true((R_DIA_MET(1) + R_DIA_DEATH(1)) * TIME_STEP < &
                         0.5D0 * DIA_C(1), &
                         "Met + death losses < 50% of biomass per timestep")
    end subroutine test_losses_bounded

end program test_diatoms
