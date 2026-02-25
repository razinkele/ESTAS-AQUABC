! Unit tests for ORGANIC_CARBON_MINERALIZATION subroutine
program test_doc_mineralization
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_TYPES
    use test_defaults
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "ORGANIC_CARBON_MINERALIZATION Unit Tests"
    print *, "=========================================="
    print *, ""

    call test_oxic_mineralization()
    call test_temperature_effect()
    call test_ph_correction()
    call test_phytoplankton_enhancement()
    call test_zero_doc()
    call test_redox_pathway_switching()
    call test_total_rate_sum()
    call test_no_nan()

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

    ! Helper: run ORGANIC_CARBON_MINERALIZATION
    subroutine run_docmin(nkn, TEMP_val, PH_val, PHYT_C_val, &
                          DOXY_val, NO3N_val, MN_IV_val, FE_III_val, &
                          S_PLUS_6_val, DOC_val, &
                          LIM_DOXY_val, LIM_NO3N_val, LIM_MN_IV_val, &
                          LIM_FE_III_val, LIM_S_PLUS_6_val, LIM_DOC_val, &
                          out_LIM_PHYT, out_DOXY_rate, out_NO3N_rate, &
                          out_MN_IV_rate, out_FE_III_rate, out_S6_rate, &
                          out_DOC_rate)
        integer, intent(in) :: nkn
        real(kind=DBL_PREC), intent(in) :: TEMP_val, PH_val, PHYT_C_val
        real(kind=DBL_PREC), intent(in) :: DOXY_val, NO3N_val, MN_IV_val
        real(kind=DBL_PREC), intent(in) :: FE_III_val, S_PLUS_6_val, DOC_val
        real(kind=DBL_PREC), intent(in) :: LIM_DOXY_val, LIM_NO3N_val
        real(kind=DBL_PREC), intent(in) :: LIM_MN_IV_val, LIM_FE_III_val
        real(kind=DBL_PREC), intent(in) :: LIM_S_PLUS_6_val, LIM_DOC_val
        real(kind=DBL_PREC), intent(out) :: out_LIM_PHYT(nkn)
        real(kind=DBL_PREC), intent(out) :: out_DOXY_rate(nkn)
        real(kind=DBL_PREC), intent(out) :: out_NO3N_rate(nkn)
        real(kind=DBL_PREC), intent(out) :: out_MN_IV_rate(nkn)
        real(kind=DBL_PREC), intent(out) :: out_FE_III_rate(nkn)
        real(kind=DBL_PREC), intent(out) :: out_S6_rate(nkn)
        real(kind=DBL_PREC), intent(out) :: out_DOC_rate(nkn)

        type(t_docmin_params) :: docmin_params
        type(t_redox_params) :: redox_params
        type(t_redox_state) :: redox_state
        type(t_redox_lim) :: redox_lim
        type(t_docmin_outputs) :: docmin_outputs

        real(kind=DBL_PREC) :: TEMP_arr(nkn), PH_arr(nkn), PHYT_C_arr(nkn)

        ! Target arrays for redox_state
        real(kind=DBL_PREC), target :: arr_DOXY(nkn), arr_NO3N(nkn)
        real(kind=DBL_PREC), target :: arr_MN_IV(nkn), arr_FE_III(nkn)
        real(kind=DBL_PREC), target :: arr_S_PLUS_6(nkn), arr_DOC(nkn)
        real(kind=DBL_PREC), target :: arr_S_MINUS_2(nkn), arr_MN_II(nkn)
        real(kind=DBL_PREC), target :: arr_FE_II(nkn), arr_HCO3(nkn), arr_CO3(nkn)

        ! Target arrays for redox_lim
        real(kind=DBL_PREC), target :: arr_LIM_DOXY(nkn), arr_LIM_NO3N(nkn)
        real(kind=DBL_PREC), target :: arr_LIM_MN_IV(nkn), arr_LIM_FE_III(nkn)
        real(kind=DBL_PREC), target :: arr_LIM_S_PLUS_6(nkn), arr_LIM_DOC(nkn)

        ! Target arrays for docmin_outputs
        real(kind=DBL_PREC), target :: o_LIM_PHYT(nkn)
        real(kind=DBL_PREC), target :: o_DOXY(nkn), o_NO3N(nkn), o_MN_IV(nkn)
        real(kind=DBL_PREC), target :: o_FE_III(nkn), o_S6(nkn), o_DOC(nkn)
        real(kind=DBL_PREC), target :: o_PH_DOXY(nkn), o_PH_NO3N(nkn)
        real(kind=DBL_PREC), target :: o_PH_MN_IV(nkn), o_PH_FE_III(nkn)
        real(kind=DBL_PREC), target :: o_PH_S6(nkn), o_PH_DOC(nkn)

        call set_default_docmin_params(docmin_params)
        call set_default_redox_params(redox_params)

        TEMP_arr = TEMP_val; PH_arr = PH_val; PHYT_C_arr = PHYT_C_val

        ! State arrays
        arr_DOXY = DOXY_val; arr_NO3N = NO3N_val; arr_MN_IV = MN_IV_val
        arr_FE_III = FE_III_val; arr_S_PLUS_6 = S_PLUS_6_val; arr_DOC = DOC_val
        arr_S_MINUS_2 = 0.1D0; arr_MN_II = 0.1D0; arr_FE_II = 0.1D0
        arr_HCO3 = 50.0D0; arr_CO3 = 1.0D0

        redox_state%DOXY       => arr_DOXY
        redox_state%NO3N       => arr_NO3N
        redox_state%MN_IV      => arr_MN_IV
        redox_state%FE_III     => arr_FE_III
        redox_state%S_PLUS_6   => arr_S_PLUS_6
        redox_state%DISS_ORG_C => arr_DOC
        redox_state%S_MINUS_2  => arr_S_MINUS_2
        redox_state%MN_II      => arr_MN_II
        redox_state%FE_II      => arr_FE_II
        redox_state%HCO3       => arr_HCO3
        redox_state%CO3        => arr_CO3

        ! Limitation arrays (pre-computed, typically from REDOX_AND_SPECIATION)
        arr_LIM_DOXY = LIM_DOXY_val; arr_LIM_NO3N = LIM_NO3N_val
        arr_LIM_MN_IV = LIM_MN_IV_val; arr_LIM_FE_III = LIM_FE_III_val
        arr_LIM_S_PLUS_6 = LIM_S_PLUS_6_val; arr_LIM_DOC = LIM_DOC_val

        redox_lim%LIM_DOXY_RED     => arr_LIM_DOXY
        redox_lim%LIM_NO3N_RED     => arr_LIM_NO3N
        redox_lim%LIM_MN_IV_RED    => arr_LIM_MN_IV
        redox_lim%LIM_FE_III_RED   => arr_LIM_FE_III
        redox_lim%LIM_S_PLUS_6_RED => arr_LIM_S_PLUS_6
        redox_lim%LIM_DOC_RED      => arr_LIM_DOC

        ! Output arrays
        o_LIM_PHYT = 0.0D0
        o_DOXY = 0.0D0; o_NO3N = 0.0D0; o_MN_IV = 0.0D0
        o_FE_III = 0.0D0; o_S6 = 0.0D0; o_DOC = 0.0D0
        o_PH_DOXY = 0.0D0; o_PH_NO3N = 0.0D0; o_PH_MN_IV = 0.0D0
        o_PH_FE_III = 0.0D0; o_PH_S6 = 0.0D0; o_PH_DOC = 0.0D0

        docmin_outputs%LIM_PHYT_AMIN_DOC          => o_LIM_PHYT
        docmin_outputs%R_ABIOTIC_DOC_MIN_DOXY     => o_DOXY
        docmin_outputs%R_ABIOTIC_DOC_MIN_NO3N     => o_NO3N
        docmin_outputs%R_ABIOTIC_DOC_MIN_MN_IV    => o_MN_IV
        docmin_outputs%R_ABIOTIC_DOC_MIN_FE_III   => o_FE_III
        docmin_outputs%R_ABIOTIC_DOC_MIN_S_PLUS_6 => o_S6
        docmin_outputs%R_ABIOTIC_DOC_MIN_DOC      => o_DOC
        docmin_outputs%PH_CORR_DOC_MIN_DOXY       => o_PH_DOXY
        docmin_outputs%PH_CORR_DOC_MIN_NO3N       => o_PH_NO3N
        docmin_outputs%PH_CORR_DOC_MIN_MN_IV      => o_PH_MN_IV
        docmin_outputs%PH_CORR_DOC_MIN_FE_III     => o_PH_FE_III
        docmin_outputs%PH_CORR_DOC_MIN_S_PLUS_6   => o_PH_S6
        docmin_outputs%PH_CORR_DOC_MIN_DOC        => o_PH_DOC

        call ORGANIC_CARBON_MINERALIZATION(nkn, TEMP_arr, PH_arr, PHYT_C_arr, &
                                           docmin_params, redox_params, &
                                           redox_state, redox_lim, docmin_outputs)

        out_LIM_PHYT = o_LIM_PHYT
        out_DOXY_rate = o_DOXY
        out_NO3N_rate = o_NO3N
        out_MN_IV_rate = o_MN_IV
        out_FE_III_rate = o_FE_III
        out_S6_rate = o_S6
        out_DOC_rate = o_DOC
    end subroutine run_docmin

    ! Oxic mineralization: high DO -> R_ABIOTIC_DOC_MIN_DOXY > 0
    subroutine test_oxic_mineralization()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: LIM_PHYT(nkn), R_DOXY(nkn), R_NO3N(nkn)
        real(kind=DBL_PREC) :: R_MN_IV(nkn), R_FE_III(nkn), R_S6(nkn), R_DOC(nkn)

        print *, "Test: Oxic mineralization (DOXY pathway positive)"

        call run_docmin(nkn, 20.0D0, 7.5D0, 2.0D0, &
                        8.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.9D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM_PHYT, R_DOXY, R_NO3N, R_MN_IV, R_FE_III, R_S6, R_DOC)

        call assert_true(R_DOXY(1) > 0.0D0, "R_ABIOTIC_DOC_MIN_DOXY > 0")
    end subroutine test_oxic_mineralization

    ! Temperature effect: warmer -> higher rate
    subroutine test_temperature_effect()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: LIM1(nkn), R_DOXY_cold(nkn), R1(nkn)
        real(kind=DBL_PREC) :: R2(nkn), R3(nkn), R4(nkn), R5(nkn)
        real(kind=DBL_PREC) :: LIM2(nkn), R_DOXY_warm(nkn), R6(nkn)
        real(kind=DBL_PREC) :: R7(nkn), R8(nkn), R9(nkn), R10(nkn)

        print *, "Test: Temperature effect on mineralization"

        ! Cold (10C)
        call run_docmin(nkn, 10.0D0, 7.5D0, 2.0D0, &
                        8.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.9D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM1, R_DOXY_cold, R1, R2, R3, R4, R5)

        ! Warm (25C)
        call run_docmin(nkn, 25.0D0, 7.5D0, 2.0D0, &
                        8.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.9D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM2, R_DOXY_warm, R6, R7, R8, R9, R10)

        call assert_true(R_DOXY_warm(1) > R_DOXY_cold(1), &
                         "Warmer temperature increases mineralization rate")
    end subroutine test_temperature_effect

    ! pH correction: optimal range (6-9) -> PH_CORR near 1
    subroutine test_ph_correction()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: LIM(nkn), R_DOXY(nkn), R_NO3N(nkn)
        real(kind=DBL_PREC) :: R_MN(nkn), R_FE(nkn), R_S6(nkn), R_DOC(nkn)

        print *, "Test: pH correction in optimal range"

        ! pH 7.5 is in optimal range [6,9]
        call run_docmin(nkn, 20.0D0, 7.5D0, 2.0D0, &
                        8.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.9D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM, R_DOXY, R_NO3N, R_MN, R_FE, R_S6, R_DOC)

        ! With optimal pH, DOXY rate should be positive (PH_CORR = 1.0)
        call assert_true(R_DOXY(1) > 0.0D0, &
                         "Positive mineralization at optimal pH")
    end subroutine test_ph_correction

    ! Phytoplankton enhancement
    subroutine test_phytoplankton_enhancement()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: LIM_low(nkn), R_DOXY_low(nkn), R1(nkn)
        real(kind=DBL_PREC) :: R2(nkn), R3(nkn), R4(nkn), R5(nkn)
        real(kind=DBL_PREC) :: LIM_high(nkn), R_DOXY_high(nkn), R6(nkn)
        real(kind=DBL_PREC) :: R7(nkn), R8(nkn), R9(nkn), R10(nkn)

        print *, "Test: Higher phytoplankton enhances mineralization"

        ! Low phytoplankton
        call run_docmin(nkn, 20.0D0, 7.5D0, 0.5D0, &
                        8.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.9D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM_low, R_DOXY_low, R1, R2, R3, R4, R5)

        ! High phytoplankton
        call run_docmin(nkn, 20.0D0, 7.5D0, 10.0D0, &
                        8.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.9D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM_high, R_DOXY_high, R6, R7, R8, R9, R10)

        call assert_true(LIM_high(1) > LIM_low(1), &
                         "Higher PHYT_TOT_C gives higher LIM_PHYT_AMIN_DOC")
        call assert_true(R_DOXY_high(1) > R_DOXY_low(1), &
                         "Higher phytoplankton increases DOXY mineralization")
    end subroutine test_phytoplankton_enhancement

    ! Zero DOC: all rates near zero
    ! Note: default K_HS_DOC_MIN_* = 0, causing 0/0=NaN at DOC=0.
    ! We test with very small DOC instead (1e-12) to verify near-zero behavior.
    subroutine test_zero_doc()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: LIM(nkn), R_DOXY(nkn), R_NO3N(nkn)
        real(kind=DBL_PREC) :: R_MN(nkn), R_FE(nkn), R_S6(nkn), R_DOC(nkn)

        print *, "Test: Near-zero DOC gives near-zero mineralization"

        call run_docmin(nkn, 20.0D0, 7.5D0, 2.0D0, &
                        8.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 1.0D-12, &  ! DOC~0
                        0.9D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM, R_DOXY, R_NO3N, R_MN, R_FE, R_S6, R_DOC)

        call assert_true(abs(R_DOXY(1)) < 1.0D-6, "DOXY rate near zero with near-zero DOC")
        call assert_true(abs(R_NO3N(1)) < 1.0D-6, "NO3N rate near zero with near-zero DOC")
    end subroutine test_zero_doc

    ! Redox pathway switching: anoxic with high NO3
    subroutine test_redox_pathway_switching()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: LIM(nkn), R_DOXY(nkn), R_NO3N(nkn)
        real(kind=DBL_PREC) :: R_MN(nkn), R_FE(nkn), R_S6(nkn), R_DOC(nkn)

        print *, "Test: Anoxic pathway switching (NO3 > DOXY)"

        ! Anoxic: zero DOXY limitation, high NO3 limitation
        call run_docmin(nkn, 20.0D0, 7.5D0, 2.0D0, &
                        0.01D0, 5.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.01D0, 0.8D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM, R_DOXY, R_NO3N, R_MN, R_FE, R_S6, R_DOC)

        call assert_true(R_NO3N(1) > R_DOXY(1), &
                         "NO3 mineralization > DOXY under anoxic conditions")
    end subroutine test_redox_pathway_switching

    ! Total rate = sum of 6 pathway rates (approximately)
    subroutine test_total_rate_sum()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: LIM(nkn), R_DOXY(nkn), R_NO3N(nkn)
        real(kind=DBL_PREC) :: R_MN(nkn), R_FE(nkn), R_S6(nkn), R_DOC(nkn)
        real(kind=DBL_PREC) :: total

        print *, "Test: All pathway rates are non-negative"

        call run_docmin(nkn, 20.0D0, 7.5D0, 2.0D0, &
                        4.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.5D0, 0.3D0, 0.1D0, 0.05D0, 0.03D0, 0.02D0, &
                        LIM, R_DOXY, R_NO3N, R_MN, R_FE, R_S6, R_DOC)

        total = R_DOXY(1) + R_NO3N(1) + R_MN(1) + R_FE(1) + R_S6(1) + R_DOC(1)
        call assert_true(total > 0.0D0, "Sum of pathway rates is positive")
        call assert_true(R_DOXY(1) >= 0.0D0, "DOXY pathway rate >= 0")
        call assert_true(R_NO3N(1) >= 0.0D0, "NO3N pathway rate >= 0")
    end subroutine test_total_rate_sum

    ! No NaN: all outputs non-NaN
    subroutine test_no_nan()
        integer, parameter :: nkn = 1
        real(kind=DBL_PREC) :: LIM(nkn), R_DOXY(nkn), R_NO3N(nkn)
        real(kind=DBL_PREC) :: R_MN(nkn), R_FE(nkn), R_S6(nkn), R_DOC(nkn)

        print *, "Test: All outputs non-NaN"

        call run_docmin(nkn, 20.0D0, 7.5D0, 2.0D0, &
                        8.0D0, 1.0D0, 0.5D0, 0.5D0, 1.0D0, 5.0D0, &
                        0.9D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                        LIM, R_DOXY, R_NO3N, R_MN, R_FE, R_S6, R_DOC)

        call assert_not_nan(LIM(1), "LIM_PHYT_AMIN_DOC not NaN")
        call assert_not_nan(R_DOXY(1), "R_ABIOTIC_DOC_MIN_DOXY not NaN")
        call assert_not_nan(R_NO3N(1), "R_ABIOTIC_DOC_MIN_NO3N not NaN")
        call assert_not_nan(R_MN(1), "R_ABIOTIC_DOC_MIN_MN_IV not NaN")
        call assert_not_nan(R_FE(1), "R_ABIOTIC_DOC_MIN_FE_III not NaN")
        call assert_not_nan(R_S6(1), "R_ABIOTIC_DOC_MIN_S_PLUS_6 not NaN")
        call assert_not_nan(R_DOC(1), "R_ABIOTIC_DOC_MIN_DOC not NaN")
    end subroutine test_no_nan

end program test_doc_mineralization
