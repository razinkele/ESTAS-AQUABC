! -----------------------------------------------------------------------------
! NOST akinete life-cycle staging (bed bank, formation cue, germination latch) --
! docs/superpowers/specs/2026-08-23-nost-akinete-staging-design.md secs 4.1-4.4.
! BED_AKI(k) [g C/m2] is the settled akinete bank in box k's sediment surface,
! fed by AKI_C settling and drained by bed germination (-> NOST_VEG_HET_C) and
! bed mortality (burial, permanent carbon loss). RAD_EMA is a 7-day EMA of the
! raw daily surface solar radiation that drives FORM_LATCH, the seasonal
! formation/germination mutual-exclusivity switch (spec sec 4.3). Solver-
! agnostic: ADVANCE_NOST_STAGING integrates the bed budget once per completed
! step from caller-supplied (possibly RK2 stage-averaged) fluxes -- never
! inside the kinetics, where a double evaluation would double-bank mass
! (spec sec 4.4). Modeled on AQUABC_POSITIONING_STATE (same allocation and
! ENSURE/RESET/SET pattern).
! -----------------------------------------------------------------------------
module AQUABC_NOST_STAGING
    use AQUABC_II_GLOBAL
    implicit none

    ! Bed akinete bank and life-cycle bookkeeping (nkn):
    real(kind = DBL_PREC), allocatable :: BED_AKI(:)         ! g C/m2
    real(kind = DBL_PREC), allocatable :: RAD_EMA(:)         ! W/m2
    logical,               allocatable :: FORM_LATCH(:)
    real(kind = DBL_PREC), allocatable :: BURIED_AKI(:)      ! g C/m2, cumulative (V4 audit)
    ! Exact cumulative integrals, updated ONLY in ADVANCE_NOST_STAGING (V4/V6 audit trail --
    ! sampled instantaneous columns cannot close a conservation identity; these close it exactly):
    real(kind = DBL_PREC), allocatable :: CUM_SETTLE_AKI(:)  ! g C/m2
    real(kind = DBL_PREC), allocatable :: CUM_GERM_AKI(:)    ! g C/m2
    real(kind = DBL_PREC), allocatable :: CUM_FORM_AKI(:)    ! g C/m2 (diagnostic only, no bed effect)
    ! Single-slot kinetics exports, OVERWRITTEN on every kinetics evaluation (written by the
    ! kinetics routine, not by ADVANCE_NOST_STAGING -- this module only reads a caller-supplied
    ! germ_cond in ADVANCE_NOST_STAGING):
    real(kind = DBL_PREC), allocatable :: STG_SETTLE_FLUX(:) ! g C/m2/d
    real(kind = DBL_PREC), allocatable :: STG_GERM_FLUX(:)   ! g C/m2/d
    real(kind = DBL_PREC), allocatable :: STG_FORM_FLUX(:)   ! g C/m2/d (= R_FORM_NOST_AKI*DEPTH)
    logical,               allocatable :: STG_GERM_COND(:)   ! non-latch germ conditions met

    ! Scalar parameters (graceful option-line overrides; PELAGIC_MODEL_OPTIONS.txt spec sec 4.5):
    real(kind = DBL_PREC) :: T_GERM_AKI_STAGE = 12.0D0   ! pre-season germination temp guard, degC
    real(kind = DBL_PREC) :: I_FORM_AKI       = 120.0D0  ! formation-latch radiation threshold, W/m2
    real(kind = DBL_PREC) :: KR_GERM_BED      = 0.05D0   ! bed germination rate, 1/d
    real(kind = DBL_PREC) :: K_MORT_BED_AKI   = 1.0D-3   ! bed mortality (burial) rate, 1/d
    real(kind = DBL_PREC) :: V_SETTLE_AKI     = 0.5D0    ! akinete settling velocity, m/d

    ! Fixed constants (spec sec 4.2/4.3; not option-line overridable):
    real(kind = DBL_PREC), parameter :: EPS_GERM_TEMP_LIM = 0.05D0  ! growth-viability gate epsilon
    real(kind = DBL_PREC), parameter :: TAU_RAD_EMA_DAYS  = 7.0D0   ! RAD_EMA e-folding time, d

contains

    subroutine ENSURE_NOST_STAGING_STATE(n)
        ! Serial-context allocation (called before the kinetics parallel region).
        integer, intent(in) :: n
        if (.not. allocated(BED_AKI)) then
            allocate(BED_AKI(n));         BED_AKI = 0.0D0
            allocate(RAD_EMA(n));         RAD_EMA = -1.0D0   ! sentinel: not yet initialized
            allocate(FORM_LATCH(n));      FORM_LATCH = .false.
            allocate(BURIED_AKI(n));      BURIED_AKI = 0.0D0
            allocate(CUM_SETTLE_AKI(n));  CUM_SETTLE_AKI = 0.0D0
            allocate(CUM_GERM_AKI(n));    CUM_GERM_AKI = 0.0D0
            allocate(CUM_FORM_AKI(n));    CUM_FORM_AKI = 0.0D0
            allocate(STG_SETTLE_FLUX(n)); STG_SETTLE_FLUX = 0.0D0
            allocate(STG_GERM_FLUX(n));   STG_GERM_FLUX = 0.0D0
            allocate(STG_FORM_FLUX(n));   STG_FORM_FLUX = 0.0D0
            allocate(STG_GERM_COND(n));   STG_GERM_COND = .false.
        end if
    end subroutine ENSURE_NOST_STAGING_STATE

    subroutine RESET_NOST_STAGING_STATE()
        ! For unit tests: restore the ENSURE-time zero/sentinel state between cases.
        if (allocated(BED_AKI))         BED_AKI = 0.0D0
        if (allocated(RAD_EMA))         RAD_EMA = -1.0D0
        if (allocated(FORM_LATCH))      FORM_LATCH = .false.
        if (allocated(BURIED_AKI))      BURIED_AKI = 0.0D0
        if (allocated(CUM_SETTLE_AKI))  CUM_SETTLE_AKI = 0.0D0
        if (allocated(CUM_GERM_AKI))    CUM_GERM_AKI = 0.0D0
        if (allocated(CUM_FORM_AKI))    CUM_FORM_AKI = 0.0D0
        if (allocated(STG_SETTLE_FLUX)) STG_SETTLE_FLUX = 0.0D0
        if (allocated(STG_GERM_FLUX))   STG_GERM_FLUX = 0.0D0
        if (allocated(STG_FORM_FLUX))   STG_FORM_FLUX = 0.0D0
        if (allocated(STG_GERM_COND))   STG_GERM_COND = .false.
    end subroutine RESET_NOST_STAGING_STATE

    subroutine SET_NOST_STAGING_PARAMS(tgerm, iform, krgerm, kmort, vsettle)
        real(kind = DBL_PREC), intent(in) :: tgerm, iform, krgerm, kmort, vsettle
        T_GERM_AKI_STAGE = tgerm
        I_FORM_AKI       = iform
        KR_GERM_BED      = krgerm
        K_MORT_BED_AKI   = kmort
        V_SETTLE_AKI     = vsettle
    end subroutine SET_NOST_STAGING_PARAMS

    subroutine ADVANCE_NOST_STAGING(n, dt_days, solar_rad, f_settle, f_germ, f_form, germ_cond)
        ! Solver-side once-per-step update (spec sec 4.4): integrates the bed budget,
        ! burial, and the three exact cumulative integrals; advances the radiation EMA;
        ! then updates the formation latch (spec sec 4.3). The caller supplies the
        ! (possibly RK2 stage-averaged) fluxes -- this routine performs no kinetics of
        ! its own and is agnostic to which solver produced them. No positivity clamp:
        ! with KR_GERM_BED*dt << 1 the pool stays non-negative analytically, and a
        ! clamp would break the V4 conservation identity (case 8 in the unit test).
        integer, intent(in) :: n
        real(kind = DBL_PREC), intent(in) :: dt_days
        real(kind = DBL_PREC), intent(in) :: solar_rad(n)
        real(kind = DBL_PREC), intent(in) :: f_settle(n), f_germ(n), f_form(n)
        logical, intent(in) :: germ_cond(n)
        integer :: k
        real(kind = DBL_PREC) :: mort

        do k = 1, n
            mort       = K_MORT_BED_AKI * BED_AKI(k)
            BED_AKI(k) = BED_AKI(k) + (f_settle(k) - f_germ(k) - mort) * dt_days
            BURIED_AKI(k) = BURIED_AKI(k) + mort * dt_days
            CUM_SETTLE_AKI(k) = CUM_SETTLE_AKI(k) + f_settle(k) * dt_days
            CUM_GERM_AKI(k)   = CUM_GERM_AKI(k)   + f_germ(k)   * dt_days
            CUM_FORM_AKI(k)   = CUM_FORM_AKI(k)   + f_form(k)   * dt_days
            if (RAD_EMA(k) < 0.0D0) then
                RAD_EMA(k) = solar_rad(k)                       ! first-call init
            else
                RAD_EMA(k) = RAD_EMA(k) + (dt_days / TAU_RAD_EMA_DAYS) * (solar_rad(k) - RAD_EMA(k))
            end if
            if (FORM_LATCH(k)) then
                if (germ_cond(k)) FORM_LATCH(k) = .false.       ! spring release, spec sec 4.3
            else
                if (RAD_EMA(k) < I_FORM_AKI) FORM_LATCH(k) = .true.
            end if
        end do
    end subroutine ADVANCE_NOST_STAGING

end module AQUABC_NOST_STAGING
