! -----------------------------------------------------------------------------
! Surface-bloom positional persistence state (positional ratchet) --
! docs/superpowers/plans/2026-08-14-surface-persistence-state.md.
! S_POS(k,g) in [0,1] is the surface-concentrated fraction of buoyant group g in
! box k: built during within-day calm fractions, dispersed by storm fractions,
! persistent across time steps (the ratchet the section-19 ladder proved
! necessary). Positional memory only -- carries no mass, is not advected.
! -----------------------------------------------------------------------------
module AQUABC_POSITIONING_STATE
    use AQUABC_II_GLOBAL
    implicit none

    ! (nkn, 3): 1 = CYN, 2 = FIX_CYN, 3 = NOST
    real(kind = DBL_PREC), allocatable :: S_POS(:, :)
    integer, parameter :: POS_CYN = 1, POS_FIX = 2, POS_NOST = 3

    ! Ratchet constants (scale values; graceful option lines override):
    ! K_POS_UP   -- surfacing rate (1/d): colonies rise 0.5-3 m/h, so a fully
    !               calm day builds S ~ 0.95.
    ! K_POS_DISP -- storm dispersal rate (1/d): a scum is mixed down in hours.
    ! W_DISP_POS -- dispersal wind threshold (m/s), > the formation floor
    !               (W_CRIT_POS_MIN): the gap between them IS the hysteresis.
    real(kind = DBL_PREC) :: K_POS_UP   = 3.0D0
    real(kind = DBL_PREC) :: K_POS_DISP = 10.0D0
    real(kind = DBL_PREC) :: W_DISP_POS = 4.0D0
    ! Self-shading slope of the POSITIONED fraction (m-1 per ug Chl a / L):
    ! the Curonian empiric kd relation's chlorophyll coefficient. A scum
    ! concentrates the group's biomass into H_SURF_POS, so the surface layer
    ! sees the group's chlorophyll in EXCESS of the column average; that excess
    ! attenuates the surface light. 0 disables (pre-closure behaviour).
    real(kind = DBL_PREC) :: KD_PER_CHL_POS = 0.02D0

contains

    subroutine ENSURE_POSITIONING_STATE(n)
        ! Serial-context allocation (called before the kinetics parallel region).
        integer, intent(in) :: n
        if (.not. allocated(S_POS)) then
            allocate(S_POS(n, 3))
            S_POS = 0.0D0
        end if
    end subroutine ENSURE_POSITIONING_STATE

    subroutine RESET_POSITIONING_STATE()
        ! For unit tests: clear the ratchet between cases.
        if (allocated(S_POS)) S_POS = 0.0D0
    end subroutine RESET_POSITIONING_STATE

    subroutine SET_POSITIONING_PARAMS(kup, kdisp, wdisp, kdchl)
        real(kind = DBL_PREC), intent(in) :: kup, kdisp, wdisp
        real(kind = DBL_PREC), intent(in), optional :: kdchl
        K_POS_UP   = kup
        K_POS_DISP = kdisp
        W_DISP_POS = wdisp
        if (present(kdchl)) KD_PER_CHL_POS = kdchl
    end subroutine SET_POSITIONING_PARAMS

    elemental function CALM_FRACTION(w_day, w_thresh) result(f)
        ! Fraction of the day the HOURLY wind sits below w_thresh, given the
        ! daily-mean wind w_day: the within-day W_h/W_day CDF fitted on 96,432
        ! ERA5 hours at Nida 2012-2022 (ln F quadratic in ln x; max error < 0.05
        ! for x <= 1, < 0.01 for x <= 0.75; F saturates to 1 by x ~ 1.23).
        real(kind = DBL_PREC), intent(in) :: w_day, w_thresh
        real(kind = DBL_PREC) :: f, x, l
        ! No hard cap on x: the min(0, .) below caps F at 1 naturally (F = 1 for
        ! x >= ~1.23), which is what both uses need -- a fully calm day has
        ! F_calm = 1, and a threshold far above the daily mean has F = 1 so the
        ! storm fraction (1 - F) is 0.
        x = w_thresh / max(w_day, 1.0D-1)
        if (x > 1.0D-3) then
            l = log(x)
            f = exp(min(0.0D0, 0.6218D0*l*l + 3.8137D0*l - 0.7987D0))
        else
            f = 0.0D0
        end if
    end function CALM_FRACTION

end module AQUABC_POSITIONING_STATE
