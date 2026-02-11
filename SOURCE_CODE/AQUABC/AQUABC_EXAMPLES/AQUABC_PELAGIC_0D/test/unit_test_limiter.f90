program unit_test_limiter
    use AQUABC_II_GLOBAL, only: DBL_PREC
    use pelagic_limiter_test
    use aquabc_pel_state_var_indexes, only: FIX_CYN_C_INDEX
    implicit none

    integer, parameter :: nkn = 1
    integer, parameter :: nstate = 50
    integer, parameter :: NDIAGVAR = 20

    real(kind=DBL_PREC) :: TIME_STEP
    real(kind=DBL_PREC), dimension(nkn) :: FIX_CYN_C
    real(kind=DBL_PREC), dimension(nkn,nstate,NDIAGVAR) :: PROCESS_RATES
    real(kind=DBL_PREC) :: allowed
    integer :: clamps_applied
    integer, dimension(NDIAGVAR) :: clamp_counts
    integer :: k

    TIME_STEP = 0.04166666666666664d0

    ! Initialize tiny biomass so allowed rate is small
    FIX_CYN_C = 1.0d-8    ! small biomass => allowed_rate ~ 2.4e-7

    PROCESS_RATES = 0.0d0

    ! Create a large process at k=9 to force clamping
    PROCESS_RATES(1,FIX_CYN_C_INDEX,9) = 0.74d0
    PROCESS_RATES(1,FIX_CYN_C_INDEX,10) = 0.95d0

    call apply_fix_cyn_clamp(FIX_CYN_C, PROCESS_RATES, TIME_STEP, NDIAGVAR, 1, clamps_applied, clamp_counts)

    ! Expected: clamps_applied > 0 and clamp_counts(9) >= 1
    if (clamps_applied <= 0) then
        print *, 'UNIT TEST FAILED: No clamps applied (expected > 0)'
        stop 1
    end if
    if (clamp_counts(9) < 1) then
        print *, 'UNIT TEST FAILED: clamp_counts(9) < 1 (actual=', clamp_counts(9), ')'
        stop 2
    end if

    ! Also verify the process value was clamped to allowed_rate
    allowed = FIX_CYN_C(1) / max(TIME_STEP, 1.0d-12)
    if (dabs(PROCESS_RATES(1,FIX_CYN_C_INDEX,9)) > allowed + 1.0d-15) then
        print *, 'UNIT TEST FAILED: process not clamped to allowed'
        print *, '  value=', PROCESS_RATES(1,FIX_CYN_C_INDEX,9), ' allowed=', allowed
        stop 3
    end if

    print *, 'UNIT TEST PASSED: clamp logic engaged and counts recorded.'
    stop 0
end program unit_test_limiter
