module pelagic_limiter_test
    use AQUABC_II_GLOBAL, only: DBL_PREC
    use aquabc_pel_state_var_indexes, only: FIX_CYN_C_INDEX
    implicit none
contains
    subroutine apply_fix_cyn_clamp(FIX_CYN_C, PROCESS_RATES, TIME_STEP, NDIAGVAR, node_idx, clamps_applied, clamp_counts)
        implicit none
        integer, intent(in) :: NDIAGVAR, node_idx
        real(kind=DBL_PREC), intent(in) :: TIME_STEP
        real(kind=DBL_PREC), dimension(:), intent(in) :: FIX_CYN_C
        real(kind=DBL_PREC), dimension(:,:,:), intent(inout) :: PROCESS_RATES
        integer, intent(out) :: clamps_applied
        integer, dimension(:), intent(out) :: clamp_counts

        real(kind=DBL_PREC) :: allowed_rate_local
        integer :: k
        real(kind=DBL_PREC) :: old_rate

        clamps_applied = 0
        clamp_counts = 0

        if (node_idx < 1) return
        allowed_rate_local = FIX_CYN_C(node_idx) / max(TIME_STEP, 1.0d-12)

        do k = 1, min(NDIAGVAR, size(PROCESS_RATES,3))
            if (dabs(PROCESS_RATES(node_idx, FIX_CYN_C_INDEX, k)) > allowed_rate_local) then
                old_rate = PROCESS_RATES(node_idx, FIX_CYN_C_INDEX, k)
                PROCESS_RATES(node_idx, FIX_CYN_C_INDEX, k) = sign(allowed_rate_local, PROCESS_RATES(node_idx, FIX_CYN_C_INDEX, k))
                clamps_applied = clamps_applied + 1
                clamp_counts(k) = clamp_counts(k) + 1
            end if
        end do
    end subroutine apply_fix_cyn_clamp
end module pelagic_limiter_test
