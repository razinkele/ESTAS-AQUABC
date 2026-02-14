! Unit tests for NOSTOCALES kinetics subroutine
program test_nostocales
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "NOSTOCALES Kinetics Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_smoke()
    call test_growth_positive()
    call test_nfix_growth()
    call test_nonfix_growth()
    call test_temperature_response()
    call test_akinete_formation()
    call test_akinete_germination()
    call test_zero_biomass()
    call test_density_mortality()
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

    ! Helper: run NOSTOCALES with given inputs, return key outputs
    subroutine run_nost(params, env, DIN, DON, DP, NOST_VEG_HET_C, NOST_AKI_C, &
                        TIME_STEP, DAY_OF_YEAR, nkn, &
                        R_NOST_VEG_HET_GROWTH, R_NOST_VEG_HET_MET, &
                        R_NOST_VEG_HET_DEATH, LIM_KG_NOST_VEG_HET_TEMP, &
                        R_NOST_VEG_HET_FIX_GROWTH, R_NOST_VEG_HET_NON_FIX_GROWTH, &
                        R_DENS_MORT_NOST_VEG_HET, R_GERM_NOST_AKI, R_FORM_NOST_AKI)
        type(t_nost_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        integer, intent(in) :: nkn, DAY_OF_YEAR
        real(kind=DBL_PREC), intent(in) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC), intent(in) :: NOST_VEG_HET_C(nkn), NOST_AKI_C(nkn)
        real(kind=DBL_PREC), intent(in) :: TIME_STEP
        real(kind=DBL_PREC), intent(out) :: R_NOST_VEG_HET_GROWTH(nkn)
        real(kind=DBL_PREC), intent(out) :: R_NOST_VEG_HET_MET(nkn)
        real(kind=DBL_PREC), intent(out) :: R_NOST_VEG_HET_DEATH(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_NOST_VEG_HET_TEMP(nkn)
        real(kind=DBL_PREC), intent(out) :: R_NOST_VEG_HET_FIX_GROWTH(nkn)
        real(kind=DBL_PREC), intent(out) :: R_NOST_VEG_HET_NON_FIX_GROWTH(nkn)
        real(kind=DBL_PREC), intent(out) :: R_DENS_MORT_NOST_VEG_HET(nkn)
        real(kind=DBL_PREC), intent(out) :: R_GERM_NOST_AKI(nkn)
        real(kind=DBL_PREC), intent(out) :: R_FORM_NOST_AKI(nkn)

        real(kind=DBL_PREC) :: NOST_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_NOST_VEG_HET(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_DOXY(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_P(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_FIX(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_NON_FIX(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_RESP(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_EXCR(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_INT_RESP(nkn)
        real(kind=DBL_PREC) :: RD_NOST_VEG_HET(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_NOST_VEG_HET_D(nkn)
        real(kind=DBL_PREC) :: R_LOSS_AKI(nkn), R_MORT_AKI(nkn)

        NOST_LIGHT_SAT = 0.0D0; KG_NOST_VEG_HET = 0.0D0
        LIM_KG_NOST_VEG_HET_LIGHT = 0.0D0; LIM_KG_NOST_VEG_HET_TEMP = 0.0D0
        LIM_KG_NOST_VEG_HET_DOXY = 0.0D0
        LIM_KG_NOST_VEG_HET_N = 0.0D0; LIM_KG_NOST_VEG_HET_P = 0.0D0
        LIM_KG_NOST_VEG_HET_FIX = 0.0D0; LIM_KG_NOST_VEG_HET_NON_FIX = 0.0D0
        R_NOST_VEG_HET_GROWTH = 0.0D0
        R_NOST_VEG_HET_FIX_GROWTH = 0.0D0; R_NOST_VEG_HET_NON_FIX_GROWTH = 0.0D0
        R_NOST_VEG_HET_MET = 0.0D0; R_NOST_VEG_HET_RESP = 0.0D0
        R_NOST_VEG_HET_EXCR = 0.0D0; R_NOST_VEG_HET_INT_RESP = 0.0D0
        RD_NOST_VEG_HET = 0.0D0; FAC_HYPOX_NOST_VEG_HET_D = 0.0D0
        R_NOST_VEG_HET_DEATH = 0.0D0; R_DENS_MORT_NOST_VEG_HET = 0.0D0
        R_GERM_NOST_AKI = 0.0D0; R_FORM_NOST_AKI = 0.0D0
        R_LOSS_AKI = 0.0D0; R_MORT_AKI = 0.0D0

        call NOSTOCALES(params, env, TIME_STEP, DAY_OF_YEAR, 0, nkn, &
                        NOST_LIGHT_SAT, DIN, DON, DP, &
                        NOST_VEG_HET_C, NOST_AKI_C, &
                        KG_NOST_VEG_HET, LIM_KG_NOST_VEG_HET_LIGHT, &
                        LIM_KG_NOST_VEG_HET_TEMP, LIM_KG_NOST_VEG_HET_DOXY, &
                        LIM_KG_NOST_VEG_HET_N, LIM_KG_NOST_VEG_HET_P, &
                        LIM_KG_NOST_VEG_HET_FIX, LIM_KG_NOST_VEG_HET_NON_FIX, &
                        R_NOST_VEG_HET_GROWTH, R_NOST_VEG_HET_FIX_GROWTH, &
                        R_NOST_VEG_HET_NON_FIX_GROWTH, &
                        R_NOST_VEG_HET_MET, R_NOST_VEG_HET_RESP, &
                        R_NOST_VEG_HET_EXCR, R_NOST_VEG_HET_INT_RESP, &
                        RD_NOST_VEG_HET, FAC_HYPOX_NOST_VEG_HET_D, &
                        R_NOST_VEG_HET_DEATH, R_DENS_MORT_NOST_VEG_HET, &
                        R_GERM_NOST_AKI, R_FORM_NOST_AKI, &
                        R_LOSS_AKI, R_MORT_AKI)
    end subroutine run_nost

    ! Smoke test: typical conditions produce plausible outputs
    subroutine test_smoke()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: Smoke test (typical conditions)"

        call set_default_nost_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.1D0; DON = 0.1D0; DP = 0.05D0
        NOST_VEG = 1.0D0; NOST_AKI = 0.5D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_not_nan(R_GROWTH(1), "Growth rate is not NaN")
        call assert_true(R_GROWTH(1) > 0.0D0, "Growth rate is positive")
    end subroutine test_smoke

    ! Growth is positive at optimal conditions
    subroutine test_growth_positive()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: Growth is positive at optimal conditions"

        call set_default_nost_params(params)
        ! Optimal temp for NOST is NOST_VEG_HET_OPT_TEMP_UR = 26
        TEMP = 26.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.5D0; DON = 0.5D0; DP = 0.1D0
        NOST_VEG = 2.0D0; NOST_AKI = 0.5D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_true(R_GROWTH(1) > 0.0D0, "Growth is positive")
    end subroutine test_growth_positive

    ! N-fixation: low DIN should produce positive fixing growth
    subroutine test_nfix_growth()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: N-fixation growth > 0 under low DIN"

        call set_default_nost_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.001D0; DON = 0.0D0; DP = 0.1D0
        NOST_VEG = 1.0D0; NOST_AKI = 0.5D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_true(R_FIX(1) > 0.0D0, "Fixing growth > 0")
    end subroutine test_nfix_growth

    ! Non-fixation: high DIN should produce positive non-fixing growth
    subroutine test_nonfix_growth()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: Non-fixing growth > 0 under high DIN"

        call set_default_nost_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 1.0D0; DON = 0.5D0; DP = 0.1D0
        NOST_VEG = 1.0D0; NOST_AKI = 0.5D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_true(R_NONFIX(1) > 0.0D0, "Non-fixing growth > 0")
    end subroutine test_nonfix_growth

    ! Optimal temperature gives high temp limitation
    subroutine test_temperature_response()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: Optimal temperature gives high temp limitation"

        call set_default_nost_params(params)
        TEMP = 26.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.5D0; DON = 0.5D0; DP = 0.1D0
        NOST_VEG = 1.0D0; NOST_AKI = 0.5D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_true(LIM_T(1) > 0.9D0, &
                         "Temp limitation near 1.0 at optimal temperature")
    end subroutine test_temperature_response

    ! Akinete formation: correct season (DAY > 200, TEMP < 16)
    subroutine test_akinete_formation()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: Akinete formation in correct season"

        call set_default_nost_params(params)
        ! Conditions for akinete formation: TEMP < T_FORM_AKI=16, DAY > DAY_FORM_AKI=200
        TEMP = 12.0D0; I_A = 200.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.4D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.5D0; DON = 0.5D0; DP = 0.1D0
        NOST_VEG = 2.0D0; NOST_AKI = 0.5D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 250, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_true(R_FORM(1) > 0.0D0, "Akinete formation rate > 0")
    end subroutine test_akinete_formation

    ! Akinete germination: low DIN + warm temp
    subroutine test_akinete_germination()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: Akinete germination under correct conditions"

        call set_default_nost_params(params)
        ! Conditions for germination: DIN < KN_GERM_AKI=0.1, TEMP > T_GERM_AKI=21
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.05D0  ! Below KN_GERM_AKI=0.1
        DON = 0.0D0; DP = 0.1D0
        NOST_VEG = 0.5D0; NOST_AKI = 2.0D0  ! Plenty of akinetes

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_true(R_GERM(1) > 0.0D0, "Akinete germination rate > 0")
    end subroutine test_akinete_germination

    ! Zero VEG_HET biomass should produce zero growth/metabolism
    subroutine test_zero_biomass()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: Zero VEG_HET biomass gives zero growth"

        call set_default_nost_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.5D0; DON = 0.5D0; DP = 0.1D0
        NOST_VEG = 0.0D0  ! Zero vegetative biomass
        NOST_AKI = 0.5D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_not_nan(R_GROWTH(1), "Growth not NaN with zero biomass")
        call assert_true(abs(R_GROWTH(1)) < 1.0D-10, "Growth near zero")
        call assert_true(abs(R_DEATH(1)) < 1.0D-10, "Death near zero")
    end subroutine test_zero_biomass

    ! Density-dependent mortality: high biomass should increase R_DENS_MORT
    subroutine test_density_mortality()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)

        print *, "Test: High biomass increases density-dependent mortality"

        call set_default_nost_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.5D0; DON = 0.5D0; DP = 0.1D0
        NOST_VEG = 10.0D0  ! High biomass
        NOST_AKI = 0.5D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_true(R_DENS(1) > 0.0D0, "Density mortality > 0 at high biomass")
    end subroutine test_density_mortality

    ! Losses bounded
    subroutine test_losses_bounded()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_GROWTH(nkn), R_MET(nkn), R_DEATH(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn), R_FIX(nkn), R_NONFIX(nkn)
        real(kind=DBL_PREC) :: R_DENS(nkn), R_GERM(nkn), R_FORM(nkn)
        real(kind=DBL_PREC) :: TS

        print *, "Test: Losses bounded below 50%% of biomass"

        call set_default_nost_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.5D0; DON = 0.5D0; DP = 0.1D0
        NOST_VEG = 2.0D0; NOST_AKI = 0.5D0
        TS = 1.0D0

        call run_nost(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      TS, 180, nkn, R_GROWTH, R_MET, R_DEATH, LIM_T, &
                      R_FIX, R_NONFIX, R_DENS, R_GERM, R_FORM)

        call assert_true((R_MET(1) + R_DEATH(1)) * TS < 0.5D0 * NOST_VEG(1), &
                         "Met + death < 50% biomass per timestep")
    end subroutine test_losses_bounded

    ! K_E = 0 should not produce NaN or Inf (division by zero guard)
    ! Uses SMITH=1 to trigger the euphotic depth calculation path
    subroutine test_ke_zero()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)

        real(kind=DBL_PREC) :: NOST_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_NOST_VEG_HET(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_LIGHT(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_TEMP(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_DOXY(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_N(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_P(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_FIX(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_NON_FIX(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_FIX_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_NON_FIX_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_MET(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_RESP(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_EXCR(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_INT_RESP(nkn)
        real(kind=DBL_PREC) :: RD_NOST_VEG_HET(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_NOST_VEG_HET_D(nkn)
        real(kind=DBL_PREC) :: R_NOST_VEG_HET_DEATH(nkn)
        real(kind=DBL_PREC) :: R_DENS_MORT_NOST_VEG_HET(nkn)
        real(kind=DBL_PREC) :: R_GERM_NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_FORM_NOST_AKI(nkn)
        real(kind=DBL_PREC) :: R_LOSS_AKI(nkn), R_MORT_AKI(nkn)

        print *, "Test: K_E=0 does not produce NaN/Inf (SMITH=1 path)"

        call set_default_nost_params(params)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 0.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.1D0; DON = 0.1D0; DP = 0.05D0
        NOST_VEG = 1.0D0; NOST_AKI = 0.5D0

        NOST_LIGHT_SAT = 0.0D0; KG_NOST_VEG_HET = 0.0D0
        LIM_KG_NOST_VEG_HET_LIGHT = 0.0D0; LIM_KG_NOST_VEG_HET_TEMP = 0.0D0
        LIM_KG_NOST_VEG_HET_DOXY = 0.0D0
        LIM_KG_NOST_VEG_HET_N = 0.0D0; LIM_KG_NOST_VEG_HET_P = 0.0D0
        LIM_KG_NOST_VEG_HET_FIX = 0.0D0; LIM_KG_NOST_VEG_HET_NON_FIX = 0.0D0
        R_NOST_VEG_HET_GROWTH = 0.0D0
        R_NOST_VEG_HET_FIX_GROWTH = 0.0D0; R_NOST_VEG_HET_NON_FIX_GROWTH = 0.0D0
        R_NOST_VEG_HET_MET = 0.0D0; R_NOST_VEG_HET_RESP = 0.0D0
        R_NOST_VEG_HET_EXCR = 0.0D0; R_NOST_VEG_HET_INT_RESP = 0.0D0
        RD_NOST_VEG_HET = 0.0D0; FAC_HYPOX_NOST_VEG_HET_D = 0.0D0
        R_NOST_VEG_HET_DEATH = 0.0D0; R_DENS_MORT_NOST_VEG_HET = 0.0D0
        R_GERM_NOST_AKI = 0.0D0; R_FORM_NOST_AKI = 0.0D0
        R_LOSS_AKI = 0.0D0; R_MORT_AKI = 0.0D0

        ! Call with SMITH=1 to trigger euphotic depth calculation
        call NOSTOCALES(params, env, 1.0D0, 180, 1, nkn, &
                        NOST_LIGHT_SAT, DIN, DON, DP, &
                        NOST_VEG, NOST_AKI, &
                        KG_NOST_VEG_HET, LIM_KG_NOST_VEG_HET_LIGHT, &
                        LIM_KG_NOST_VEG_HET_TEMP, LIM_KG_NOST_VEG_HET_DOXY, &
                        LIM_KG_NOST_VEG_HET_N, LIM_KG_NOST_VEG_HET_P, &
                        LIM_KG_NOST_VEG_HET_FIX, LIM_KG_NOST_VEG_HET_NON_FIX, &
                        R_NOST_VEG_HET_GROWTH, R_NOST_VEG_HET_FIX_GROWTH, &
                        R_NOST_VEG_HET_NON_FIX_GROWTH, &
                        R_NOST_VEG_HET_MET, R_NOST_VEG_HET_RESP, &
                        R_NOST_VEG_HET_EXCR, R_NOST_VEG_HET_INT_RESP, &
                        RD_NOST_VEG_HET, FAC_HYPOX_NOST_VEG_HET_D, &
                        R_NOST_VEG_HET_DEATH, R_DENS_MORT_NOST_VEG_HET, &
                        R_GERM_NOST_AKI, R_FORM_NOST_AKI, &
                        R_LOSS_AKI, R_MORT_AKI)

        call assert_finite(R_NOST_VEG_HET_GROWTH(1), "Growth finite with K_E=0")
        call assert_finite(R_NOST_VEG_HET_MET(1), "Metabolism finite with K_E=0")
        call assert_finite(R_NOST_VEG_HET_DEATH(1), "Death finite with K_E=0")
    end subroutine test_ke_zero

end program test_nostocales
