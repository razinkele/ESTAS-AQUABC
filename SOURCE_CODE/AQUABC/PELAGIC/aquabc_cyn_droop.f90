! -----------------------------------------------------------------------------
! CYN nitrogen-quota (Droop) mechanism -- opt-in `CYN_VARIABLE_N`, VARN build
! only (docs/superpowers/specs/2026-08-30-cyn-droop-n-rescoped-design.md sec 2).
! This task only carries the graceful-option scalar parameters and their
! setter (the review's orphan-setter fix, so READ_PELAGIC_MODEL_OPTIONS has
! somewhere to call into even before the physics exists). CYN_N_QMIN/QMAX
! bound the quota Q = CYN_N/CYN_C [gN/gC]; CYN_N_VMAX [gN/gC/d] and
! CYN_N_KHS_UPT [mg N/L] parameterize the Monod-down-regulated uptake rate.
! EPS_CYN_C is the small-biomass floor used wherever Q = CYN_N/CYN_C is
! evaluated (guards the division, spec sec 2). Task 3 adds the pure physics
! helpers (F_DOWN, LIM_N_QUOTA, R_UPTAKE) to this same module.
! -----------------------------------------------------------------------------
module AQUABC_CYN_DROOP
    use AQUABC_II_GLOBAL
    implicit none

    ! Scalar parameters (graceful option-line overrides; PELAGIC_MODEL_OPTIONS.txt,
    ! spec sec 2 "Committed constants"):
    real(kind = DBL_PREC) :: CYN_N_QMIN    = 0.10D0   ! gN/gC, quota floor
    real(kind = DBL_PREC) :: CYN_N_QMAX    = 0.25D0   ! gN/gC, quota ceiling
    real(kind = DBL_PREC) :: CYN_N_VMAX    = 0.44D0   ! gN/gC/d, max N-uptake rate
    real(kind = DBL_PREC) :: CYN_N_KHS_UPT = 0.003D0  ! mg N/L, uptake half-saturation

    ! Small-biomass floor for Q = CYN_N/CYN_C (spec sec 2); not option-line
    ! overridable.
    real(kind = DBL_PREC), parameter :: EPS_CYN_C = 1.0D-10

contains

    subroutine SET_CYN_DROOP_PARAMS(qmin, qmax, vmax, khs)
        real(kind = DBL_PREC), intent(in) :: qmin, qmax, vmax, khs
        CYN_N_QMIN    = qmin
        CYN_N_QMAX    = qmax
        CYN_N_VMAX    = vmax
        CYN_N_KHS_UPT = khs
    end subroutine SET_CYN_DROOP_PARAMS

end module AQUABC_CYN_DROOP
