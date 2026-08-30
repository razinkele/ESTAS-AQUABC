! -----------------------------------------------------------------------------
! CYN nitrogen-quota (Droop) mechanism -- opt-in `CYN_VARIABLE_N`, VARN build
! only (docs/superpowers/specs/2026-08-30-cyn-droop-n-rescoped-design.md sec 2).
! This task only carries the graceful-option scalar parameters and their
! setter (the review's orphan-setter fix, so READ_PELAGIC_MODEL_OPTIONS has
! somewhere to call into even before the physics exists). CYN_N_QMIN/QMAX
! bound the quota Q = CYN_N/CYN_C [gN/gC]; CYN_N_VMAX [gN/gC/d] and
! CYN_N_KHS_UPT [mg N/L] parameterize the Monod-down-regulated uptake rate.
! EPS_CYN_C is the small-biomass floor used wherever Q = CYN_N/CYN_C is
! evaluated (guards the division, spec sec 2). The three pure helpers below
! (F_DOWN, LIM_N_QUOTA, R_UPTAKE) implement spec sec 2 verbatim and are the
! single source of the quota physics for both CYANOBACTERIA variants.
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


    ! Down-regulation of N uptake as the quota fills (spec sec 2):
    !     f_down = max(0, (Q_MAX - Q)/(Q_MAX - Q_MIN))
    ! Deliberately NOT clamped from above: below Q_MIN (reachable, because
    ! CYN_N and CYN_C are transported independently) a starved population
    ! takes up faster than VMAX*CYN_C, which is the intended behaviour.
    pure function F_DOWN(Q) result(F)
        real(kind = DBL_PREC), intent(in) :: Q
        real(kind = DBL_PREC) :: F
        F = max(0.0D0, (CYN_N_QMAX - Q) / max(CYN_N_QMAX - CYN_N_QMIN, EPS_CYN_C))
    end function F_DOWN


    ! Caperon-Meyer linear-quota growth limitation (spec sec 2):
    !     LIM_KG_CYN_N = clamp((Q - Q_MIN)/(Q_MAX - Q_MIN), 0, 1)
    pure function LIM_N_QUOTA(Q) result(LIM)
        real(kind = DBL_PREC), intent(in) :: Q
        real(kind = DBL_PREC) :: LIM
        LIM = (Q - CYN_N_QMIN) / max(CYN_N_QMAX - CYN_N_QMIN, EPS_CYN_C)
        LIM = max(0.0D0, min(1.0D0, LIM))
    end function LIM_N_QUOTA


    ! N uptake into the quota, mg N/L/d (spec sec 2):
    !     R = VMAX * DIN/(KHS_UPT + DIN) * f_down(Q) * CYN_C
    ! DIN is NH4_N + NO3_N only -- DON uptake into the quota is explicitly
    ! out of scope (spec sec 4).
    pure function R_UPTAKE(DIN, Q, CYN_C) result(R)
        real(kind = DBL_PREC), intent(in) :: DIN
        real(kind = DBL_PREC), intent(in) :: Q
        real(kind = DBL_PREC), intent(in) :: CYN_C
        real(kind = DBL_PREC) :: R
        R = CYN_N_VMAX * (DIN / max(CYN_N_KHS_UPT + DIN, EPS_CYN_C)) * &
            F_DOWN(Q) * CYN_C
    end function R_UPTAKE

end module AQUABC_CYN_DROOP
