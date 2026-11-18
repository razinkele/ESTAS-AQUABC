! Unit tests for VALIDATE_PELAGIC_MODEL_CONSTANTS (production routine, linked directly).
!
! The validator's temperature rules depend on the temperature model:
!   CTMI    reads KAPPA_*_OVER_OPT_TEMP as T_max (degC) and repairs kappa-style values;
!   plateau reads the same slot as a decay rate per degC and must NOT be "repaired".
! In report-only mode (the ESTAS driver) it reports problems but changes nothing.

program test_validate_constants
    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "VALIDATE_PELAGIC_MODEL_CONSTANTS tests"
    print *, "=========================================="
    print *, ""

    call test_plateau_keeps_decay_rates()
    call test_ctmi_repairs_kappa_style_tmax()
    call test_ctmi_keeps_valid_cardinals()
    call test_plateau_keeps_tmax_like_value()
    call test_zero_half_saturation_is_clamped()
    call test_report_only_changes_nothing()

    print *, ""
    print '(A,I3,A)', " Passed: ", passed, " tests"
    print '(A,I3,A)', " Failed: ", failed, " tests"
    if (failed == 0) then
        print *, "ALL TESTS PASSED!"
    else
        print *, "SOME TESTS FAILED"
        stop 1
    end if

contains

    subroutine check(cond, label)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: label
        if (cond) then
            passed = passed + 1
            print *, "  PASS: ", label
        else
            failed = failed + 1
            print *, "  FAIL: ", label
        end if
    end subroutine check

    ! Defaults, then the plateau-era decay rates of the Standard set.
    subroutine setup(ctmi, report_only)
        logical, intent(in) :: ctmi, report_only
        call DEFAULT_PELAGIC_MODEL_CONSTANTS()
        USE_CTMI_TEMP = ctmi
        VALIDATE_CONSTANTS_REPORT_ONLY = report_only
    end subroutine setup

    subroutine set_plateau_rates()
        KAPPA_DIA_OVER_OPT_TEMP          = 0.07D0
        KAPPA_CYN_OVER_OPT_TEMP          = 0.05D0
        KAPPA_FIX_CYN_OVER_OPT_TEMP      = 0.05D0
        KAPPA_OPA_OVER_OPT_TEMP          = 0.10D0
        KAPPA_ZOO_OVER_OPT_TEMP          = 0.01D0
        KAPPA_NOST_VEG_HET_OVER_OPT_TEMP = 0.05D0
    end subroutine set_plateau_rates

    subroutine test_plateau_keeps_decay_rates()
        call setup(ctmi=.false., report_only=.false.)
        call set_plateau_rates()
        call VALIDATE_PELAGIC_MODEL_CONSTANTS()
        call check(KAPPA_DIA_OVER_OPT_TEMP == 0.07D0 .and. KAPPA_CYN_OVER_OPT_TEMP == 0.05D0 .and. &
                   KAPPA_FIX_CYN_OVER_OPT_TEMP == 0.05D0 .and. KAPPA_OPA_OVER_OPT_TEMP == 0.10D0 .and. &
                   KAPPA_ZOO_OVER_OPT_TEMP == 0.01D0 .and. KAPPA_NOST_VEG_HET_OVER_OPT_TEMP == 0.05D0, &
                   "plateau: decay rates are not rewritten to T_max")
    end subroutine test_plateau_keeps_decay_rates

    subroutine test_ctmi_repairs_kappa_style_tmax()
        call setup(ctmi=.true., report_only=.false.)
        call set_plateau_rates()
        call VALIDATE_PELAGIC_MODEL_CONSTANTS()
        call check(KAPPA_DIA_OVER_OPT_TEMP == 35.0D0 .and. KAPPA_CYN_OVER_OPT_TEMP == 38.0D0 .and. &
                   KAPPA_FIX_CYN_OVER_OPT_TEMP == 38.0D0 .and. KAPPA_OPA_OVER_OPT_TEMP == 33.0D0 .and. &
                   KAPPA_ZOO_OVER_OPT_TEMP == 35.0D0 .and. KAPPA_NOST_VEG_HET_OVER_OPT_TEMP == 38.0D0, &
                   "CTMI: kappa-style values become the default T_max")
    end subroutine test_ctmi_repairs_kappa_style_tmax

    subroutine test_ctmi_keeps_valid_cardinals()
        call setup(ctmi=.true., report_only=.false.)
        DIA_OPT_TEMP_LR = 0.0D0
        DIA_OPT_TEMP_UR = 15.0D0
        KAPPA_DIA_OVER_OPT_TEMP = 21.0D0
        call VALIDATE_PELAGIC_MODEL_CONSTANTS()
        call check(DIA_OPT_TEMP_LR == 0.0D0 .and. DIA_OPT_TEMP_UR == 15.0D0 .and. &
                   KAPPA_DIA_OVER_OPT_TEMP == 21.0D0, "CTMI: valid T_min < T_opt < T_max untouched")
        KAPPA_DIA_OVER_OPT_TEMP = 12.0D0            ! T_max below T_opt
        call VALIDATE_PELAGIC_MODEL_CONSTANTS()
        call check(KAPPA_DIA_OVER_OPT_TEMP == 25.0D0, "CTMI: T_opt >= T_max -> T_max = T_opt + 10")
    end subroutine test_ctmi_keeps_valid_cardinals

    subroutine test_plateau_keeps_tmax_like_value()
        ! A T_max under the plateau model is reported, never silently "fixed": the
        ! validator cannot know the intended decay rate.
        call setup(ctmi=.false., report_only=.false.)
        KAPPA_DIA_OVER_OPT_TEMP = 35.0D0
        DIA_OPT_TEMP_UR = 40.0D0                    ! would trip the CTMI T_opt >= T_max rule
        call VALIDATE_PELAGIC_MODEL_CONSTANTS()
        call check(KAPPA_DIA_OVER_OPT_TEMP == 35.0D0, "plateau: T_max-like value reported, not changed")
    end subroutine test_plateau_keeps_tmax_like_value

    subroutine test_zero_half_saturation_is_clamped()
        call setup(ctmi=.false., report_only=.false.)
        call set_plateau_rates()
        K_HS_DOC_MIN_DOXY = 0.0D0
        call VALIDATE_PELAGIC_MODEL_CONSTANTS()
        call check(K_HS_DOC_MIN_DOXY == 1.0D-6, "correcting mode: zero half-saturation clamped")
    end subroutine test_zero_half_saturation_is_clamped

    subroutine test_report_only_changes_nothing()
        call setup(ctmi=.true., report_only=.true.)
        call set_plateau_rates()
        K_HS_DOC_MIN_DOXY = 0.0D0
        DIA_N_TO_C        = 0.0D0
        BETA_DIA          = -1.0D0
        ICE_LIGHT_TRANS   = 2.0D0
        OPA_OPT_TEMP_LR   = 30.0D0                  ! above OPA_OPT_TEMP_UR
        call VALIDATE_PELAGIC_MODEL_CONSTANTS()
        call check(K_HS_DOC_MIN_DOXY == 0.0D0 .and. DIA_N_TO_C == 0.0D0 .and. BETA_DIA == -1.0D0 .and. &
                   ICE_LIGHT_TRANS == 2.0D0 .and. OPA_OPT_TEMP_LR == 30.0D0 .and. &
                   KAPPA_DIA_OVER_OPT_TEMP == 0.07D0 .and. KAPPA_OPA_OVER_OPT_TEMP == 0.10D0, &
                   "report-only: every problem reported, no value changed")
        VALIDATE_CONSTANTS_REPORT_ONLY = .false.
    end subroutine test_report_only_changes_nothing

end program test_validate_constants
