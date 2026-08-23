! Unit tests for NOSTOCALES kinetics subroutine
program test_nostocales
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    use AQUABC_NOST_STAGING
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
    call test_flag0_legacy_unchanged()
    call test_stage_dead_water_gate()
    call test_stage_latch_blocks_germination()
    call test_stage_germination_rate_and_water_pool_off()
    call test_stage_formation_and_settling()

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

    subroutine assert_approx_equal(value, expected, tol, test_name)
        real(kind=DBL_PREC), intent(in) :: value, expected, tol
        character(len=*), intent(in) :: test_name
        if (abs(value - expected) <= tol) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,ES16.9,A,ES16.9)', "    Got: ", value, " Expected: ", expected
            failed = failed + 1
        end if
    end subroutine assert_approx_equal

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
        double precision :: S_NOST_TEST(nkn)
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
        ! Task-3 staging dummies: flag=0 keeps this helper's callers on the legacy
        ! path, so these are just inert placeholders (never read under flag=0).
        real(kind=DBL_PREC) :: BED_AKI_ZERO(nkn), SETTLE_FLUX_ZERO(nkn)
        real(kind=DBL_PREC) :: GERM_FLUX_ZERO(nkn), FORM_FLUX_ZERO(nkn)
        real(kind=DBL_PREC) :: R_GERM_BED_AKI_ZERO(nkn), R_SETTLE_AKI_ZERO(nkn)
        logical :: FORM_LATCH_ZERO(nkn), GERM_COND_ZERO(nkn)

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
        BED_AKI_ZERO = 0.0D0; FORM_LATCH_ZERO = .false.
        SETTLE_FLUX_ZERO = 0.0D0; GERM_FLUX_ZERO = 0.0D0; FORM_FLUX_ZERO = 0.0D0
        GERM_COND_ZERO = .false.
        R_GERM_BED_AKI_ZERO = 0.0D0; R_SETTLE_AKI_ZERO = 0.0D0

        S_NOST_TEST = 0.0D0
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
                        R_LOSS_AKI, R_MORT_AKI, &
                        0, 0.5D0, 0.0D0, S_NOST_TEST, &
                        0, BED_AKI_ZERO, FORM_LATCH_ZERO, &
                        SETTLE_FLUX_ZERO, GERM_FLUX_ZERO, FORM_FLUX_ZERO, &
                        GERM_COND_ZERO, R_GERM_BED_AKI_ZERO, R_SETTLE_AKI_ZERO)
    end subroutine run_nost

    ! Helper: run NOSTOCALES exercising the Task-3 staging extension (the flag +
    ! seven array/logical dummies appended after S_CHUNK), returning both the
    ! legacy AKI rates and the new staging exports.
    subroutine run_nost_staged(params, env, DIN, DON, DP, NOST_VEG_HET_C, NOST_AKI_C, &
                        TIME_STEP, DAY_OF_YEAR, nkn, &
                        NOST_STAGE_MODEL, BED_AKI_CHUNK, FORM_LATCH_CHUNK, &
                        R_GERM_NOST_AKI, R_FORM_NOST_AKI, R_LOSS_AKI, R_MORT_AKI, &
                        LIM_KG_NOST_VEG_HET_TEMP, &
                        SETTLE_FLUX_CHUNK, GERM_FLUX_CHUNK, FORM_FLUX_CHUNK, &
                        GERM_COND_CHUNK, R_GERM_BED_AKI, R_SETTLE_AKI)
        type(t_nost_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        integer, intent(in) :: nkn, DAY_OF_YEAR
        real(kind=DBL_PREC), intent(in) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC), intent(in) :: NOST_VEG_HET_C(nkn), NOST_AKI_C(nkn)
        real(kind=DBL_PREC), intent(in) :: TIME_STEP
        integer, intent(in) :: NOST_STAGE_MODEL
        real(kind=DBL_PREC), intent(in) :: BED_AKI_CHUNK(nkn)
        logical, intent(in) :: FORM_LATCH_CHUNK(nkn)
        real(kind=DBL_PREC), intent(out) :: R_GERM_NOST_AKI(nkn), R_FORM_NOST_AKI(nkn)
        real(kind=DBL_PREC), intent(out) :: R_LOSS_AKI(nkn), R_MORT_AKI(nkn)
        real(kind=DBL_PREC), intent(out) :: LIM_KG_NOST_VEG_HET_TEMP(nkn)
        real(kind=DBL_PREC), intent(out) :: SETTLE_FLUX_CHUNK(nkn), GERM_FLUX_CHUNK(nkn)
        real(kind=DBL_PREC), intent(out) :: FORM_FLUX_CHUNK(nkn)
        logical, intent(out) :: GERM_COND_CHUNK(nkn)
        real(kind=DBL_PREC), intent(out) :: R_GERM_BED_AKI(nkn), R_SETTLE_AKI(nkn)

        double precision :: S_NOST_TEST(nkn)
        real(kind=DBL_PREC) :: NOST_LIGHT_SAT(nkn)
        real(kind=DBL_PREC) :: KG_NOST_VEG_HET(nkn)
        real(kind=DBL_PREC) :: LIM_KG_NOST_VEG_HET_LIGHT(nkn)
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
        SETTLE_FLUX_CHUNK = 0.0D0; GERM_FLUX_CHUNK = 0.0D0; FORM_FLUX_CHUNK = 0.0D0
        GERM_COND_CHUNK = .false.
        R_GERM_BED_AKI = 0.0D0; R_SETTLE_AKI = 0.0D0
        S_NOST_TEST = 0.0D0

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
                        R_LOSS_AKI, R_MORT_AKI, &
                        0, 0.5D0, 0.0D0, S_NOST_TEST, &
                        NOST_STAGE_MODEL, BED_AKI_CHUNK, FORM_LATCH_CHUNK, &
                        SETTLE_FLUX_CHUNK, GERM_FLUX_CHUNK, FORM_FLUX_CHUNK, &
                        GERM_COND_CHUNK, R_GERM_BED_AKI, R_SETTLE_AKI)
    end subroutine run_nost_staged

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
        double precision :: S_NOST_TEST(nkn)
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
        real(kind=DBL_PREC) :: BED_AKI_ZERO(nkn), SETTLE_FLUX_ZERO(nkn)
        real(kind=DBL_PREC) :: GERM_FLUX_ZERO(nkn), FORM_FLUX_ZERO(nkn)
        real(kind=DBL_PREC) :: R_GERM_BED_AKI_ZERO(nkn), R_SETTLE_AKI_ZERO(nkn)
        logical :: FORM_LATCH_ZERO(nkn), GERM_COND_ZERO(nkn)

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
        BED_AKI_ZERO = 0.0D0; FORM_LATCH_ZERO = .false.
        SETTLE_FLUX_ZERO = 0.0D0; GERM_FLUX_ZERO = 0.0D0; FORM_FLUX_ZERO = 0.0D0
        GERM_COND_ZERO = .false.
        R_GERM_BED_AKI_ZERO = 0.0D0; R_SETTLE_AKI_ZERO = 0.0D0

        ! Call with SMITH=1 to trigger euphotic depth calculation
        S_NOST_TEST = 0.0D0
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
                        R_LOSS_AKI, R_MORT_AKI, &
                        0, 0.5D0, 0.0D0, S_NOST_TEST, &
                        0, BED_AKI_ZERO, FORM_LATCH_ZERO, &
                        SETTLE_FLUX_ZERO, GERM_FLUX_ZERO, FORM_FLUX_ZERO, &
                        GERM_COND_ZERO, R_GERM_BED_AKI_ZERO, R_SETTLE_AKI_ZERO)

        call assert_finite(R_NOST_VEG_HET_GROWTH(1), "Growth finite with K_E=0")
        call assert_finite(R_NOST_VEG_HET_MET(1), "Metabolism finite with K_E=0")
        call assert_finite(R_NOST_VEG_HET_DEATH(1), "Death finite with K_E=0")
    end subroutine test_ke_zero

    ! -----------------------------------------------------------------------------
    ! Task-3 staging tests (NOST_STAGE_MODEL): flag=0 regression + flag=1 cases
    ! (a)-(g) from the task-3 brief. Staging module scalars (T_GERM_AKI_STAGE,
    ! KR_GERM_BED, V_SETTLE_AKI) are set explicitly in every case via
    ! SET_NOST_STAGING_PARAMS so no case depends on another's leftover state.
    ! -----------------------------------------------------------------------------

    ! flag=0: the four legacy AKI rates (R_GERM_NOST_AKI, R_FORM_NOST_AKI,
    ! R_LOSS_AKI, R_MORT_AKI) must equal the legacy formulas exactly, and every
    ! new staging export must be zero/false (Step 3's else branch).
    subroutine test_flag0_legacy_unchanged()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: BED_AKI_CHUNK(nkn)
        logical :: FORM_LATCH_CHUNK(nkn)
        real(kind=DBL_PREC) :: R_GERM(nkn), R_FORM(nkn), R_LOSS(nkn), R_MORT(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn)
        real(kind=DBL_PREC) :: SETTLE_FLUX(nkn), GERM_FLUX(nkn), FORM_FLUX(nkn)
        logical :: GERM_COND(nkn)
        real(kind=DBL_PREC) :: R_GERM_BED(nkn), R_SETTLE(nkn)
        real(kind=DBL_PREC) :: expected

        print *, "Test: flag=0 -- legacy AKI rates unchanged, staging exports zero"

        call set_default_nost_params(params)
        params%K_LOSS_AKI    = 0.02D0   ! defaults are 0.0 -- would make the check trivial
        params%K_MORT_AKI_20 = 0.01D0
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.05D0; DON = 0.0D0; DP = 0.1D0    ! DIN < KN_GERM_AKI=0.1, TEMP=25 > T_GERM_AKI=21
        NOST_VEG = 1.0D0; NOST_AKI = 2.0D0
        BED_AKI_CHUNK = 0.0D0; FORM_LATCH_CHUNK = .false.

        call run_nost_staged(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, 0, BED_AKI_CHUNK, FORM_LATCH_CHUNK, &
                      R_GERM, R_FORM, R_LOSS, R_MORT, LIM_T, &
                      SETTLE_FLUX, GERM_FLUX, FORM_FLUX, GERM_COND, R_GERM_BED, R_SETTLE)

        expected = params%KR_GERM_AKI * NOST_AKI(1)
        call assert_approx_equal(R_GERM(1), expected, 1.0D-12, "Legacy R_GERM_NOST_AKI unchanged")

        expected = 0.0D0   ! TEMP=25 not < T_FORM_AKI=16
        call assert_approx_equal(R_FORM(1), expected, 1.0D-12, "Legacy R_FORM_NOST_AKI unchanged")

        expected = params%K_LOSS_AKI * NOST_AKI(1)
        call assert_approx_equal(R_LOSS(1), expected, 1.0D-12, "Legacy R_LOSS_AKI unchanged")

        expected = params%K_MORT_AKI_20 * (params%THETA_K_MORT_AKI ** (TEMP(1) - 20.0D0)) * NOST_AKI(1)
        call assert_approx_equal(R_MORT(1), expected, 1.0D-12, "Legacy R_MORT_AKI unchanged")

        call assert_true(.not. GERM_COND(1), "flag=0: GERM_COND_CHUNK is false")
        call assert_approx_equal(GERM_FLUX(1), 0.0D0, 1.0D-12, "flag=0: GERM_FLUX_CHUNK is zero")
        call assert_approx_equal(SETTLE_FLUX(1), 0.0D0, 1.0D-12, "flag=0: SETTLE_FLUX_CHUNK is zero")
        call assert_approx_equal(FORM_FLUX(1), 0.0D0, 1.0D-12, "flag=0: FORM_FLUX_CHUNK is zero")
        call assert_approx_equal(R_GERM_BED(1), 0.0D0, 1.0D-12, "flag=0: R_GERM_BED_AKI is zero")
        call assert_approx_equal(R_SETTLE(1), 0.0D0, 1.0D-12, "flag=0: R_SETTLE_AKI is zero")
    end subroutine test_flag0_legacy_unchanged

    ! Case (a): dead-water gate -- germination stays zero when the growth-viability
    ! limiter is at/under EPS_GERM_TEMP_LIM, even though DIN is low and TEMP is well
    ! above T_GERM_AKI_STAGE. This test link's GROWTH_AT_TEMP stub (test_pelagic_
    ! aux_subset.f90) is CTMI-only (T_min=16, T_opt=26, T_max=KAPPA_OVER=38); TEMP=
    ! 37.9 sits just under the thermal ceiling T_max, driving LIM_KG_NOST_VEG_HET_TEMP
    ! to ~0.017 (production's plateau model gives an even smaller value at this TEMP).
    subroutine test_stage_dead_water_gate()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: BED_AKI_CHUNK(nkn)
        logical :: FORM_LATCH_CHUNK(nkn)
        real(kind=DBL_PREC) :: R_GERM(nkn), R_FORM(nkn), R_LOSS(nkn), R_MORT(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn)
        real(kind=DBL_PREC) :: SETTLE_FLUX(nkn), GERM_FLUX(nkn), FORM_FLUX(nkn)
        logical :: GERM_COND(nkn)
        real(kind=DBL_PREC) :: R_GERM_BED(nkn), R_SETTLE(nkn)

        print *, "Test: stage (a) -- dead-water gate blocks germination"

        call set_default_nost_params(params)
        call SET_NOST_STAGING_PARAMS(12.0D0, 120.0D0, 0.05D0, 1.0D-3, 0.5D0)
        TEMP = 37.9D0; I_A = 300.0D0; K_E = 1.0D0    ! near CTMI T_max=38 -> LIM_TEMP ~ 0.017
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.01D0; DON = 0.0D0; DP = 0.1D0        ! DIN low
        NOST_VEG = 1.0D0; NOST_AKI = 0.5D0
        BED_AKI_CHUNK = 3.0D0                         ! nonzero bed -- would germinate if the gate failed
        FORM_LATCH_CHUNK = .false.

        call run_nost_staged(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, 1, BED_AKI_CHUNK, FORM_LATCH_CHUNK, &
                      R_GERM, R_FORM, R_LOSS, R_MORT, LIM_T, &
                      SETTLE_FLUX, GERM_FLUX, FORM_FLUX, GERM_COND, R_GERM_BED, R_SETTLE)

        call assert_true(LIM_T(1) <= 0.05D0, "LIM_KG_NOST_VEG_HET_TEMP <= EPS_GERM_TEMP_LIM")
        call assert_true(.not. GERM_COND(1), "Dead-water gate: GERM_COND_CHUNK is false")
        call assert_approx_equal(GERM_FLUX(1), 0.0D0, 1.0D-12, "Dead-water gate: GERM_FLUX_CHUNK is zero")
        call assert_approx_equal(R_GERM_BED(1), 0.0D0, 1.0D-12, "Dead-water gate: R_GERM_BED_AKI is zero")
    end subroutine test_stage_dead_water_gate

    ! Case (b): FORM_LATCH_CHUNK=.true. blocks germination even though the
    ! non-latch conditions (DIN, temp-viability, T_GERM_AKI_STAGE) are all met.
    subroutine test_stage_latch_blocks_germination()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: BED_AKI_CHUNK(nkn)
        logical :: FORM_LATCH_CHUNK(nkn)
        real(kind=DBL_PREC) :: R_GERM(nkn), R_FORM(nkn), R_LOSS(nkn), R_MORT(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn)
        real(kind=DBL_PREC) :: SETTLE_FLUX(nkn), GERM_FLUX(nkn), FORM_FLUX(nkn)
        logical :: GERM_COND(nkn)
        real(kind=DBL_PREC) :: R_GERM_BED(nkn), R_SETTLE(nkn)

        print *, "Test: stage (b) -- formation latch blocks germination"

        call set_default_nost_params(params)
        call SET_NOST_STAGING_PARAMS(12.0D0, 120.0D0, 0.05D0, 1.0D-3, 0.5D0)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0    ! plateau: LIM_TEMP = 1.0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.01D0; DON = 0.0D0; DP = 0.1D0
        NOST_VEG = 1.0D0; NOST_AKI = 0.5D0
        BED_AKI_CHUNK = 3.0D0
        FORM_LATCH_CHUNK = .true.                     ! latch ON blocks germination

        call run_nost_staged(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, 1, BED_AKI_CHUNK, FORM_LATCH_CHUNK, &
                      R_GERM, R_FORM, R_LOSS, R_MORT, LIM_T, &
                      SETTLE_FLUX, GERM_FLUX, FORM_FLUX, GERM_COND, R_GERM_BED, R_SETTLE)

        call assert_true(GERM_COND(1), "Non-latch germ conditions all met: GERM_COND_CHUNK true")
        call assert_approx_equal(GERM_FLUX(1), 0.0D0, 1.0D-12, "Latch ON: GERM_FLUX_CHUNK is zero")
        call assert_approx_equal(R_GERM_BED(1), 0.0D0, 1.0D-12, "Latch ON: R_GERM_BED_AKI is zero")
    end subroutine test_stage_latch_blocks_germination

    ! Cases (c) and (d): with all gates passing and the latch off, germination
    ! fires at KR_GERM_BED*BED_AKI_CHUNK, R_GERM_BED_AKI is the depth-normalised
    ! form of the same flux, and the legacy water-pool germination is forced to
    ! zero under the flag (case d) even though TEMP=25 > legacy T_GERM_AKI=21
    ! would otherwise fire it.
    subroutine test_stage_germination_rate_and_water_pool_off()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: BED_AKI_CHUNK(nkn)
        logical :: FORM_LATCH_CHUNK(nkn)
        real(kind=DBL_PREC) :: R_GERM(nkn), R_FORM(nkn), R_LOSS(nkn), R_MORT(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn)
        real(kind=DBL_PREC) :: SETTLE_FLUX(nkn), GERM_FLUX(nkn), FORM_FLUX(nkn)
        logical :: GERM_COND(nkn)
        real(kind=DBL_PREC) :: R_GERM_BED(nkn), R_SETTLE(nkn)
        real(kind=DBL_PREC), parameter :: KR_GERM_BED_TEST = 0.07D0   ! distinct from the 0.05 default

        print *, "Test: stage (c)/(d) -- germination rate, bed source, water-pool off"

        call set_default_nost_params(params)
        call SET_NOST_STAGING_PARAMS(12.0D0, 120.0D0, KR_GERM_BED_TEST, 1.0D-3, 0.5D0)
        TEMP = 25.0D0; I_A = 300.0D0; K_E = 1.0D0     ! also > legacy T_GERM_AKI=21
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        WINDS = 3.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)

        DIN = 0.01D0; DON = 0.0D0; DP = 0.1D0
        NOST_VEG = 1.0D0; NOST_AKI = 2.0D0             ! legacy would give R_GERM_NOST_AKI = 0.3*2.0 = 0.6
        BED_AKI_CHUNK = 4.0D0
        FORM_LATCH_CHUNK = .false.

        call run_nost_staged(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 180, nkn, 1, BED_AKI_CHUNK, FORM_LATCH_CHUNK, &
                      R_GERM, R_FORM, R_LOSS, R_MORT, LIM_T, &
                      SETTLE_FLUX, GERM_FLUX, FORM_FLUX, GERM_COND, R_GERM_BED, R_SETTLE)

        call assert_approx_equal(GERM_FLUX(1), KR_GERM_BED_TEST * BED_AKI_CHUNK(1), 1.0D-12, &
                                  "(c) GERM_FLUX_CHUNK = KR_GERM_BED * BED_AKI_CHUNK")
        call assert_approx_equal(R_GERM_BED(1), GERM_FLUX(1) / DEPTH(1), 1.0D-12, &
                                  "(c) R_GERM_BED_AKI = GERM_FLUX_CHUNK / DEPTH")
        call assert_approx_equal(R_GERM(1), 0.0D0, 1.0D-12, &
                                  "(d) R_GERM_NOST_AKI = 0 under the flag")
    end subroutine test_stage_germination_rate_and_water_pool_off

    ! Cases (e), (f), (g): formation is latch-driven (fires iff FORM_LATCH_CHUNK,
    ! independent of the legacy season gate), settling of the water akinete pool
    ! is unconditional, and FORM_FLUX_CHUNK/SETTLE_FLUX_CHUNK/GERM_FLUX_CHUNK are
    ! all distinct nonzero-vs-zero values across the two sub-calls below so a
    ! positional swap among the three consecutive real(nkn) intent(out) dummies
    ! (or the two logicals) would be caught.
    subroutine test_stage_formation_and_settling()
        integer, parameter :: nkn = 1
        type(t_nost_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn)
        real(kind=DBL_PREC), target :: DO_arr(nkn), WINDS(nkn)
        real(kind=DBL_PREC) :: DIN(nkn), DON(nkn), DP(nkn)
        real(kind=DBL_PREC) :: NOST_VEG(nkn), NOST_AKI(nkn)
        real(kind=DBL_PREC) :: BED_AKI_CHUNK(nkn)
        logical :: FORM_LATCH_CHUNK(nkn)
        real(kind=DBL_PREC) :: R_GERM(nkn), R_FORM(nkn), R_LOSS(nkn), R_MORT(nkn)
        real(kind=DBL_PREC) :: LIM_T(nkn)
        real(kind=DBL_PREC) :: SETTLE_FLUX(nkn), GERM_FLUX(nkn), FORM_FLUX(nkn)
        logical :: GERM_COND(nkn)
        real(kind=DBL_PREC) :: R_GERM_BED(nkn), R_SETTLE(nkn)
        real(kind=DBL_PREC), parameter :: V_SETTLE_TEST = 0.6D0   ! distinct from the 0.5 default
        real(kind=DBL_PREC) :: expected_settle, expected_r_settle

        print *, "Test: stage (e)/(f)/(g) -- formation latch-driven, settling unconditional"

        call set_default_nost_params(params)
        call SET_NOST_STAGING_PARAMS(12.0D0, 120.0D0, 0.05D0, 1.0D-3, V_SETTLE_TEST)
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        I_A = 300.0D0; K_E = 1.0D0; WINDS = 3.0D0
        NOST_VEG = 2.5D0; NOST_AKI = 1.5D0
        DP = 0.1D0
        expected_settle   = V_SETTLE_TEST * NOST_AKI(1)
        expected_r_settle = expected_settle / DEPTH(1)

        ! Sub-call 1: latch ON. DIN high so germination gates fail independently
        ! (isolates the formation/settling checks from germination).
        TEMP = 25.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)
        DIN = 0.5D0; DON = 0.0D0
        BED_AKI_CHUNK = 0.0D0
        FORM_LATCH_CHUNK = .true.

        call run_nost_staged(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 100, nkn, 1, BED_AKI_CHUNK, FORM_LATCH_CHUNK, &
                      R_GERM, R_FORM, R_LOSS, R_MORT, LIM_T, &
                      SETTLE_FLUX, GERM_FLUX, FORM_FLUX, GERM_COND, R_GERM_BED, R_SETTLE)

        call assert_approx_equal(R_FORM(1), params%KR_FORM_AKI * NOST_VEG(1), 1.0D-12, &
                                  "(e) Latch ON: R_FORM_NOST_AKI = KR_FORM_AKI * VEG")
        call assert_approx_equal(FORM_FLUX(1), R_FORM(1) * DEPTH(1), 1.0D-12, &
                                  "(g) Latch ON: FORM_FLUX_CHUNK = R_FORM_NOST_AKI * DEPTH")
        call assert_approx_equal(SETTLE_FLUX(1), expected_settle, 1.0D-12, &
                                  "(f) SETTLE_FLUX_CHUNK = V_SETTLE_AKI * AKI_C (latch ON)")
        call assert_approx_equal(R_SETTLE(1), expected_r_settle, 1.0D-12, &
                                  "(f) R_SETTLE_AKI = SETTLE_FLUX_CHUNK / DEPTH (latch ON)")
        call assert_approx_equal(GERM_FLUX(1), 0.0D0, 1.0D-12, &
                                  "Latch ON sub-call: GERM_FLUX_CHUNK is zero (DIN too high)")

        ! Sub-call 2: latch OFF, DIN low -- germination fires, formation must not.
        TEMP = 20.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr, WINDS)
        DIN = 0.01D0
        BED_AKI_CHUNK = 3.0D0
        FORM_LATCH_CHUNK = .false.

        call run_nost_staged(params, env, DIN, DON, DP, NOST_VEG, NOST_AKI, &
                      1.0D0, 100, nkn, 1, BED_AKI_CHUNK, FORM_LATCH_CHUNK, &
                      R_GERM, R_FORM, R_LOSS, R_MORT, LIM_T, &
                      SETTLE_FLUX, GERM_FLUX, FORM_FLUX, GERM_COND, R_GERM_BED, R_SETTLE)

        call assert_approx_equal(R_FORM(1), 0.0D0, 1.0D-12, &
                                  "(e) Latch OFF: R_FORM_NOST_AKI is zero")
        call assert_approx_equal(FORM_FLUX(1), 0.0D0, 1.0D-12, &
                                  "(g) Latch OFF: FORM_FLUX_CHUNK is zero")
        call assert_approx_equal(GERM_FLUX(1), 0.05D0 * BED_AKI_CHUNK(1), 1.0D-12, &
                                  "Latch OFF sub-call: GERM_FLUX_CHUNK = KR_GERM_BED * BED_AKI_CHUNK")
        call assert_approx_equal(SETTLE_FLUX(1), expected_settle, 1.0D-12, &
                                  "(f) SETTLE_FLUX_CHUNK unchanged by latch state")
        call assert_approx_equal(R_SETTLE(1), expected_r_settle, 1.0D-12, &
                                  "(f) R_SETTLE_AKI unchanged by latch state")
    end subroutine test_stage_formation_and_settling

end program test_nostocales
