! Unit tests for ZOOPLANKTON kinetics subroutine
program test_zooplankton
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "ZOOPLANKTON Kinetics Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_smoke()
    call test_single_prey()
    call test_food_minimum()
    call test_temperature_response()
    call test_respiration_positive()
    call test_zero_zooplankton()
    call test_excretion_partitioning()
    call test_saturating_food()

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

    ! Helper: run ZOOPLANKTON with given inputs
    subroutine run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, &
                       NOST_C, DET_C, ZOO_C, TIME_STEP, nkn, &
                       R_ZOO_FEEDING_DIA, R_ZOO_FEEDING_CYN, &
                       R_ZOO_FEEDING_OPA, R_ZOO_RESP, R_ZOO_DEATH, &
                       R_ZOO_GROWTH, R_ZOO_EX_DOC, KG_ZOO, &
                       zoo_food_model, khs_food_tot, closure_ref)
        type(t_zoo_params), intent(in) :: params
        type(t_phyto_env), intent(in) :: env
        integer, intent(in) :: nkn
        real(kind=DBL_PREC), intent(in) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC), intent(in) :: FIX_CYN_C(nkn), NOST_C(nkn)
        real(kind=DBL_PREC), intent(in) :: DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC), intent(in) :: TIME_STEP
        real(kind=DBL_PREC), intent(out) :: R_ZOO_FEEDING_DIA(nkn)
        real(kind=DBL_PREC), intent(out) :: R_ZOO_FEEDING_CYN(nkn)
        real(kind=DBL_PREC), intent(out) :: R_ZOO_FEEDING_OPA(nkn)
        real(kind=DBL_PREC), intent(out) :: R_ZOO_RESP(nkn)
        real(kind=DBL_PREC), intent(out) :: R_ZOO_DEATH(nkn)
        real(kind=DBL_PREC), intent(out) :: R_ZOO_GROWTH(nkn)
        real(kind=DBL_PREC), intent(out) :: R_ZOO_EX_DOC(nkn)
        real(kind=DBL_PREC), intent(out) :: KG_ZOO(nkn)
        ! optional: exercise the saturating total-food response (default legacy)
        integer, intent(in), optional :: zoo_food_model
        real(kind=DBL_PREC), intent(in), optional :: khs_food_tot, closure_ref
        integer :: zfm
        real(kind=DBL_PREC) :: khs, zref

        real(kind=DBL_PREC) :: KG_ZOO_DIA(nkn), KG_ZOO_CYN(nkn)
        real(kind=DBL_PREC) :: KG_ZOO_OPA(nkn), KG_ZOO_FIX_CYN(nkn)
        real(kind=DBL_PREC) :: KG_ZOO_NOST(nkn), KG_ZOO_DET(nkn)
        real(kind=DBL_PREC) :: KD_ZOO(nkn)
        real(kind=DBL_PREC) :: FF_DIA(nkn), FF_CYN(nkn), FF_OPA(nkn)
        real(kind=DBL_PREC) :: FF_FIX(nkn), FF_NOST(nkn), FF_DET(nkn)
        real(kind=DBL_PREC) :: R_ZOO_FEEDING_FIX_CYN(nkn)
        real(kind=DBL_PREC) :: R_ZOO_FEEDING_NOST(nkn)
        real(kind=DBL_PREC) :: R_ZOO_FEEDING_DET(nkn)
        real(kind=DBL_PREC) :: R_ZOO_INT_RESP(nkn)
        real(kind=DBL_PREC) :: R_ZOO_EX_DON(nkn), R_ZOO_EX_DOP(nkn)
        real(kind=DBL_PREC) :: ACTUAL_ZOO_N_TO_C(nkn), ACTUAL_ZOO_P_TO_C(nkn)
        real(kind=DBL_PREC) :: FAC_HYPOX_ZOO_D(nkn)

        KG_ZOO = 0.0D0; KG_ZOO_DIA = 0.0D0; KG_ZOO_CYN = 0.0D0
        KG_ZOO_OPA = 0.0D0; KG_ZOO_FIX_CYN = 0.0D0
        KG_ZOO_NOST = 0.0D0; KG_ZOO_DET = 0.0D0; KD_ZOO = 0.0D0
        FF_DIA = 0.0D0; FF_CYN = 0.0D0; FF_OPA = 0.0D0
        FF_FIX = 0.0D0; FF_NOST = 0.0D0; FF_DET = 0.0D0
        R_ZOO_FEEDING_DIA = 0.0D0; R_ZOO_FEEDING_CYN = 0.0D0
        R_ZOO_FEEDING_FIX_CYN = 0.0D0; R_ZOO_FEEDING_NOST = 0.0D0
        R_ZOO_FEEDING_OPA = 0.0D0; R_ZOO_FEEDING_DET = 0.0D0
        R_ZOO_INT_RESP = 0.0D0; R_ZOO_RESP = 0.0D0
        R_ZOO_EX_DON = 0.0D0; R_ZOO_EX_DOP = 0.0D0; R_ZOO_EX_DOC = 0.0D0
        R_ZOO_DEATH = 0.0D0
        ACTUAL_ZOO_N_TO_C = 0.0D0; ACTUAL_ZOO_P_TO_C = 0.0D0
        R_ZOO_GROWTH = 0.0D0; FAC_HYPOX_ZOO_D = 0.0D0

        zfm = 0; khs = 0.5D0; zref = 0.05D0
        if (present(zoo_food_model)) zfm = zoo_food_model
        if (present(khs_food_tot))   khs = khs_food_tot
        if (present(closure_ref))    zref = closure_ref

        call ZOOPLANKTON(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, &
                         NOST_C, DET_C, ZOO_C, TIME_STEP, nkn, &
                         KG_ZOO, KG_ZOO_DIA, KG_ZOO_CYN, KG_ZOO_OPA, &
                         KG_ZOO_FIX_CYN, KG_ZOO_NOST, KG_ZOO_DET, KD_ZOO, &
                         FF_DIA, FF_CYN, FF_OPA, FF_FIX, FF_NOST, FF_DET, &
                         R_ZOO_FEEDING_DIA, R_ZOO_FEEDING_CYN, &
                         R_ZOO_FEEDING_FIX_CYN, R_ZOO_FEEDING_NOST, &
                         R_ZOO_FEEDING_OPA, R_ZOO_FEEDING_DET, &
                         R_ZOO_INT_RESP, R_ZOO_RESP, &
                         R_ZOO_EX_DON, R_ZOO_EX_DOP, R_ZOO_EX_DOC, &
                         R_ZOO_DEATH, ACTUAL_ZOO_N_TO_C, ACTUAL_ZOO_P_TO_C, &
                         R_ZOO_GROWTH, FAC_HYPOX_ZOO_D, &
                         zfm, khs, zref)
    end subroutine run_zoo

    ! Smoke test: prey available, positive grazing
    subroutine test_smoke()
        integer, parameter :: nkn = 1
        type(t_zoo_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn), NOST_C(nkn), DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_F_DIA(nkn), R_F_CYN(nkn), R_F_OPA(nkn)
        real(kind=DBL_PREC) :: R_RESP(nkn), R_DEATH(nkn), R_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_EX_DOC(nkn), KG_ZOO(nkn)

        print *, "Test: Smoke test (prey available)"

        call set_default_zoo_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        DIA_C = 1.0D0; CYN_C = 0.5D0; OPA_C = 0.5D0
        FIX_CYN_C = 0.2D0; NOST_C = 0.1D0; DET_C = 1.0D0; ZOO_C = 0.5D0

        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA, R_F_CYN, R_F_OPA, R_RESP, R_DEATH, &
                     R_GROWTH, R_EX_DOC, KG_ZOO)

        call assert_not_nan(R_GROWTH(1), "Growth rate is not NaN")
        call assert_true(R_F_DIA(1) > 0.0D0, "Feeding on diatoms is positive")
    end subroutine test_smoke

    ! Saturating total-food response (ZOO_FOOD_MODEL=1): the ceiling is lifted
    ! and the closure is quadratic. Same state as the smoke test.
    subroutine test_saturating_food()
        integer, parameter :: nkn = 1
        type(t_zoo_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn), NOST_C(nkn), DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_F_DIA(nkn), R_F_CYN(nkn), R_F_OPA(nkn)
        real(kind=DBL_PREC) :: R_RESP(nkn), R_DEATH0(nkn), R_DEATH1(nkn), R_GROWTH0(nkn)
        real(kind=DBL_PREC) :: R_GROWTH1(nkn), R_EX_DOC(nkn), KG_ZOO(nkn)

        print *, "Test: Saturating total-food response (ZOO_FOOD_MODEL=1)"

        call set_default_zoo_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        ! abundant prey of every type: the legacy sum stays preference-bounded,
        ! the saturating response approaches KG_ZOO
        DIA_C = 5.0D0; CYN_C = 5.0D0; OPA_C = 5.0D0
        FIX_CYN_C = 5.0D0; NOST_C = 5.0D0; DET_C = 5.0D0
        ZOO_C = 0.1D0    ! 2x the closure reference (0.05)

        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA, R_F_CYN, R_F_OPA, R_RESP, R_DEATH0, &
                     R_GROWTH0, R_EX_DOC, KG_ZOO)
        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA, R_F_CYN, R_F_OPA, R_RESP, R_DEATH1, &
                     R_GROWTH1, R_EX_DOC, KG_ZOO, &
                     zoo_food_model = 1, khs_food_tot = 0.5D0, closure_ref = 5.0D-2)

        call assert_not_nan(R_GROWTH1(1), "Saturating growth is not NaN")
        call assert_true(R_GROWTH1(1) > R_GROWTH0(1), &
            "Abundant food: saturating ingestion exceeds the legacy ceiling")
        ! quadratic closure at ZOO_C = 2x reference doubles the specific death rate
        call assert_true(abs(R_DEATH1(1) - 2.0D0 * R_DEATH0(1)) < 1.0D-9, &
            "Closure is quadratic (2x reference -> 2x legacy death)")
    end subroutine test_saturating_food

    ! Single prey: only DIA_C > 0
    subroutine test_single_prey()
        integer, parameter :: nkn = 1
        type(t_zoo_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn), NOST_C(nkn), DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_F_DIA(nkn), R_F_CYN(nkn), R_F_OPA(nkn)
        real(kind=DBL_PREC) :: R_RESP(nkn), R_DEATH(nkn), R_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_EX_DOC(nkn), KG_ZOO(nkn)

        print *, "Test: Single prey (only diatoms available)"

        call set_default_zoo_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        DIA_C = 2.0D0  ! Only diatoms available
        CYN_C = 0.0D0; OPA_C = 0.0D0; FIX_CYN_C = 0.0D0
        NOST_C = 0.0D0; DET_C = 0.0D0; ZOO_C = 0.5D0

        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA, R_F_CYN, R_F_OPA, R_RESP, R_DEATH, &
                     R_GROWTH, R_EX_DOC, KG_ZOO)

        call assert_true(R_F_DIA(1) > 0.0D0, "Feeding on diatoms is positive")
        call assert_true(abs(R_F_CYN(1)) < 1.0D-10, "Feeding on CYN near zero")
    end subroutine test_single_prey

    ! Food minimum: total food below threshold
    subroutine test_food_minimum()
        integer, parameter :: nkn = 1
        type(t_zoo_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn), NOST_C(nkn), DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_F_DIA(nkn), R_F_CYN(nkn), R_F_OPA(nkn)
        real(kind=DBL_PREC) :: R_RESP(nkn), R_DEATH(nkn), R_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_EX_DOC(nkn), KG_ZOO(nkn)

        print *, "Test: Near-zero grazing below food minimum"

        call set_default_zoo_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        ! All food below FOOD_MIN_ZOO = 0.02
        DIA_C = 0.001D0; CYN_C = 0.001D0; OPA_C = 0.001D0
        FIX_CYN_C = 0.001D0; NOST_C = 0.001D0; DET_C = 0.001D0
        ZOO_C = 0.5D0

        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA, R_F_CYN, R_F_OPA, R_RESP, R_DEATH, &
                     R_GROWTH, R_EX_DOC, KG_ZOO)

        ! When all prey < FOOD_MIN_ZOO, feeding = 0 so net growth <= 0
        call assert_true(R_GROWTH(1) <= 0.0D0, &
                         "No net growth when food below minimum")
    end subroutine test_food_minimum

    ! Optimal temperature
    subroutine test_temperature_response()
        integer, parameter :: nkn = 1
        type(t_zoo_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP_opt(nkn), TEMP_cold(nkn)
        real(kind=DBL_PREC), target :: I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn), NOST_C(nkn), DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_F_DIA1(nkn), R_F_CYN1(nkn), R_F_OPA1(nkn)
        real(kind=DBL_PREC) :: R_RESP1(nkn), R_DEATH1(nkn), R_GROWTH1(nkn)
        real(kind=DBL_PREC) :: R_EX1(nkn), KG1(nkn)
        real(kind=DBL_PREC) :: R_F_DIA2(nkn), R_F_CYN2(nkn), R_F_OPA2(nkn)
        real(kind=DBL_PREC) :: R_RESP2(nkn), R_DEATH2(nkn), R_GROWTH2(nkn)
        real(kind=DBL_PREC) :: R_EX2(nkn), KG2(nkn)

        print *, "Test: Optimal temperature gives higher growth"

        call set_default_zoo_params(params)
        I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0

        DIA_C = 2.0D0; CYN_C = 1.0D0; OPA_C = 1.0D0
        FIX_CYN_C = 0.5D0; NOST_C = 0.2D0; DET_C = 1.0D0; ZOO_C = 0.5D0

        ! Optimal temp = 25
        TEMP_opt = 25.0D0
        call setup_phyto_env(env, TEMP_opt, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)
        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA1, R_F_CYN1, R_F_OPA1, R_RESP1, R_DEATH1, &
                     R_GROWTH1, R_EX1, KG1)

        ! Cold temp
        TEMP_cold = 5.0D0
        call setup_phyto_env(env, TEMP_cold, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)
        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA2, R_F_CYN2, R_F_OPA2, R_RESP2, R_DEATH2, &
                     R_GROWTH2, R_EX2, KG2)

        call assert_true(KG1(1) > KG2(1), &
                         "Higher growth rate at optimal vs cold temperature")
    end subroutine test_temperature_response

    ! Respiration should be positive
    subroutine test_respiration_positive()
        integer, parameter :: nkn = 1
        type(t_zoo_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn), NOST_C(nkn), DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_F_DIA(nkn), R_F_CYN(nkn), R_F_OPA(nkn)
        real(kind=DBL_PREC) :: R_RESP(nkn), R_DEATH(nkn), R_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_EX_DOC(nkn), KG_ZOO(nkn)

        print *, "Test: Respiration is positive"

        call set_default_zoo_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        DIA_C = 2.0D0; CYN_C = 1.0D0; OPA_C = 1.0D0
        FIX_CYN_C = 0.5D0; NOST_C = 0.2D0; DET_C = 1.0D0; ZOO_C = 1.0D0

        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA, R_F_CYN, R_F_OPA, R_RESP, R_DEATH, &
                     R_GROWTH, R_EX_DOC, KG_ZOO)

        call assert_true(R_RESP(1) > 0.0D0, "Respiration rate is positive")
    end subroutine test_respiration_positive

    ! Zero zooplankton: zero rates
    subroutine test_zero_zooplankton()
        integer, parameter :: nkn = 1
        type(t_zoo_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn), NOST_C(nkn), DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_F_DIA(nkn), R_F_CYN(nkn), R_F_OPA(nkn)
        real(kind=DBL_PREC) :: R_RESP(nkn), R_DEATH(nkn), R_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_EX_DOC(nkn), KG_ZOO(nkn)

        print *, "Test: Zero zooplankton gives zero rates"

        call set_default_zoo_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        DIA_C = 2.0D0; CYN_C = 1.0D0; OPA_C = 1.0D0
        FIX_CYN_C = 0.5D0; NOST_C = 0.2D0; DET_C = 1.0D0
        ZOO_C = 0.0D0  ! Zero zooplankton

        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA, R_F_CYN, R_F_OPA, R_RESP, R_DEATH, &
                     R_GROWTH, R_EX_DOC, KG_ZOO)

        call assert_not_nan(R_GROWTH(1), "Growth not NaN with zero biomass")
        call assert_true(abs(R_GROWTH(1)) < 1.0D-10, "Growth near zero")
        call assert_true(abs(R_DEATH(1)) < 1.0D-10, "Death near zero")
    end subroutine test_zero_zooplankton

    ! Excretion partitioning: organic fraction
    subroutine test_excretion_partitioning()
        integer, parameter :: nkn = 1
        type(t_zoo_params) :: params
        type(t_phyto_env) :: env
        real(kind=DBL_PREC), target :: TEMP(nkn), I_A(nkn), K_E(nkn)
        real(kind=DBL_PREC), target :: DEPTH(nkn), CHLA(nkn), FDAY(nkn), DO_arr(nkn)
        real(kind=DBL_PREC) :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        real(kind=DBL_PREC) :: FIX_CYN_C(nkn), NOST_C(nkn), DET_C(nkn), ZOO_C(nkn)
        real(kind=DBL_PREC) :: R_F_DIA(nkn), R_F_CYN(nkn), R_F_OPA(nkn)
        real(kind=DBL_PREC) :: R_RESP(nkn), R_DEATH(nkn), R_GROWTH(nkn)
        real(kind=DBL_PREC) :: R_EX_DOC(nkn), KG_ZOO(nkn)

        print *, "Test: Excretion organic fraction is reasonable"

        call set_default_zoo_params(params)
        TEMP = 20.0D0; I_A = 300.0D0; K_E = 1.0D0
        DEPTH = 3.0D0; CHLA = 5.0D0; FDAY = 0.5D0; DO_arr = 8.0D0
        call setup_phyto_env(env, TEMP, I_A, K_E, DEPTH, CHLA, FDAY, DO_arr)

        DIA_C = 2.0D0; CYN_C = 1.0D0; OPA_C = 1.0D0
        FIX_CYN_C = 0.5D0; NOST_C = 0.2D0; DET_C = 1.0D0; ZOO_C = 1.0D0

        call run_zoo(params, env, DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_C, &
                     DET_C, ZOO_C, 1.0D0, nkn, &
                     R_F_DIA, R_F_CYN, R_F_OPA, R_RESP, R_DEATH, &
                     R_GROWTH, R_EX_DOC, KG_ZOO)

        ! R_ZOO_EX_DOC should be non-negative (excretion produces DOC)
        call assert_true(R_EX_DOC(1) >= 0.0D0, "DOC excretion is non-negative")
    end subroutine test_excretion_partitioning

end program test_zooplankton
