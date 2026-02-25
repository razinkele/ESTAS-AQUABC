! =========================================================================
! Module: AQUABC_SEDIMENT_BIOTURBATION
!
! Bioturbation processes for the bottom sediment model:
!   1. Depth-dependent biodiffusion coefficient Db(z) with exponential decay
!   2. Oxygen-dependent scaling of Db
!   3. Seasonal (sinusoidal) variability of Db
!   4. Bioirrigation enhancement factor for solute diffusion
!
! All functions are elemental/pure where possible for vectorised use.
!
! References:
!   - Boudreau (1997) Diagenetic Models and Their Implementation
!   - Soetaert et al. (1996) On the coupled benthic-pelagic dynamics
!   - Middelburg et al. (1997) Empirical relationships for Db
!
! Author: Auto-generated bioturbation module
! Date:   February 2026
! =========================================================================
module AQUABC_SEDIMENT_BIOTURBATION
    use precision_kinds, only: DBL_PREC
    implicit none

    private
    public :: BIOTURB_DEPTH_ATTEN, &
              BIOTURB_O2_SCALING, &
              BIOTURB_SEASONAL_FACTOR, &
              BIOTURB_EFFECTIVE_DB, &
              BIOIRRIGATION_FACTOR, &
              APPLY_BIOTURBATION_COEFFS, &
              BIOTURB_LAST_LAYER_MIXING_RATE

    ! Mathematical constants
    real(kind=DBL_PREC), parameter :: PI = 3.141592653589793D0

contains

    ! =====================================================================
    ! BIOTURB_DEPTH_ATTEN
    !
    ! Exponential depth attenuation of the biodiffusion coefficient.
    ! Db(z) = Db0 * exp(-z / z_mix)
    !
    ! where z is the midpoint depth of the layer and z_mix is the
    ! characteristic mixing depth (typically 5-15 cm in coastal sediments).
    !
    ! Arguments:
    !   Db0      : Surface biodiffusion coefficient (m^2/day)
    !   z_mid    : Midpoint depth of the layer (m)
    !   z_mix    : Characteristic bioturbation mixing depth (m)
    !
    ! Returns:
    !   Db at depth z_mid
    ! =====================================================================
    elemental function BIOTURB_DEPTH_ATTEN(Db0, z_mid, z_mix) result(Db)
        real(kind=DBL_PREC), intent(in) :: Db0, z_mid, z_mix
        real(kind=DBL_PREC) :: Db
        real(kind=DBL_PREC) :: z_mix_safe

        z_mix_safe = max(z_mix, 1.0D-20)
        Db = Db0 * exp(-z_mid / z_mix_safe)
    end function BIOTURB_DEPTH_ATTEN

    ! =====================================================================
    ! BIOTURB_O2_SCALING
    !
    ! Oxygen-dependent scaling of the biodiffusion coefficient.
    ! Uses a Monod (half-saturation) formulation:
    !
    !   f(O2) = O2 / (O2 + KHS_O2_BIOTURB)
    !
    ! Under anoxic conditions, bioturbation ceases as benthic fauna
    ! cannot survive. This provides a smooth transition.
    !
    ! Arguments:
    !   DOXY           : Dissolved oxygen concentration (g/m^3 = mg/L)
    !   KHS_O2_BIOTURB : Half-saturation O2 for bioturbation (g/m^3)
    !
    ! Returns:
    !   Scaling factor [0, 1)
    ! =====================================================================
    elemental function BIOTURB_O2_SCALING(DOXY, KHS_O2_BIOTURB) result(f_O2)
        real(kind=DBL_PREC), intent(in) :: DOXY, KHS_O2_BIOTURB
        real(kind=DBL_PREC) :: f_O2
        real(kind=DBL_PREC) :: O2_pos

        O2_pos = max(DOXY, 0.0D0)
        f_O2 = O2_pos / (O2_pos + max(KHS_O2_BIOTURB, 1.0D-20))
    end function BIOTURB_O2_SCALING

    ! =====================================================================
    ! BIOTURB_SEASONAL_FACTOR
    !
    ! Sinusoidal seasonal modulation of bioturbation activity.
    ! Biological activity peaks in summer (day_peak ~ 200, mid-July in
    ! Northern Hemisphere) and is lowest in winter.
    !
    !   f_season = 1 + amplitude * sin(2*pi*(day_of_year - day_peak)/365 + pi/2)
    !            = 1 + amplitude * cos(2*pi*(day_of_year - day_peak)/365)
    !
    ! The amplitude controls the range: e.g. amplitude=0.5 gives
    ! seasonal variation between 0.5 and 1.5 times the mean.
    !
    ! Arguments:
    !   day_of_year     : Current day of the year (1-365)
    !   amplitude       : Seasonal amplitude (0 = no seasonality, 0.5 = ±50%)
    !   day_peak        : Day of year with maximum bioturbation (default ~200)
    !
    ! Returns:
    !   Seasonal scaling factor (always > 0 if amplitude < 1)
    ! =====================================================================
    elemental function BIOTURB_SEASONAL_FACTOR(day_of_year, amplitude, day_peak) result(f_season)
        real(kind=DBL_PREC), intent(in) :: day_of_year, amplitude, day_peak
        real(kind=DBL_PREC) :: f_season

        f_season = 1.0D0 + amplitude * cos(2.0D0 * PI * (day_of_year - day_peak) / 365.0D0)
        ! Clamp to avoid negative values if amplitude >= 1
        f_season = max(f_season, 0.0D0)
    end function BIOTURB_SEASONAL_FACTOR

    ! =====================================================================
    ! BIOTURB_EFFECTIVE_DB
    !
    ! Compute the effective biodiffusion coefficient combining all factors:
    !   Db_eff = Db0 * depth_atten * O2_scaling * seasonal_factor
    !
    ! This is the main entry point for computing the full Db at a given
    ! layer, node, and time.
    !
    ! Arguments:
    !   Db0             : Surface biodiffusion coefficient (m^2/day)
    !   z_mid           : Midpoint depth of the layer (m)
    !   z_mix           : Characteristic bioturbation depth (m)
    !   DOXY            : Dissolved oxygen (g/m^3)
    !   KHS_O2_BIOTURB  : Half-saturation O2 for bioturbation (g/m^3)
    !   day_of_year     : Day of year (1-365)
    !   seasonal_amp    : Seasonal amplitude (0 = none)
    !   day_peak        : Day of peak bioturbation activity
    !
    ! Returns:
    !   Effective Db (m^2/day)
    ! =====================================================================
    elemental function BIOTURB_EFFECTIVE_DB(Db0, z_mid, z_mix, &
                                            DOXY, KHS_O2_BIOTURB, &
                                            day_of_year, seasonal_amp, day_peak) result(Db_eff)
        real(kind=DBL_PREC), intent(in) :: Db0, z_mid, z_mix
        real(kind=DBL_PREC), intent(in) :: DOXY, KHS_O2_BIOTURB
        real(kind=DBL_PREC), intent(in) :: day_of_year, seasonal_amp, day_peak
        real(kind=DBL_PREC) :: Db_eff

        Db_eff = BIOTURB_DEPTH_ATTEN(Db0, z_mid, z_mix)    &
               * BIOTURB_O2_SCALING(DOXY, KHS_O2_BIOTURB)   &
               * BIOTURB_SEASONAL_FACTOR(day_of_year, seasonal_amp, day_peak)
    end function BIOTURB_EFFECTIVE_DB

    ! =====================================================================
    ! BIOIRRIGATION_FACTOR
    !
    ! Bioirrigation enhancement factor for solute diffusion.
    ! Burrow-dwelling organisms pump overlying water through the sediment,
    ! enhancing solute exchange beyond what molecular diffusion provides.
    !
    ! The irrigation factor is modelled as:
    !   alpha_irr = 1 + alpha0 * f(O2) * exp(-z / z_irr) * f_season
    !
    ! where alpha0 is the maximum irrigation enhancement at the surface.
    ! The factor is applied as a multiplier to porewater diffusion rates
    ! (only for solute-phase species, IN_WHICH_PHASE = 0 or 2).
    !
    ! Arguments:
    !   alpha0          : Maximum irrigation enhancement factor at surface (dimensionless)
    !   z_mid           : Midpoint depth of layer (m)
    !   z_irr           : Characteristic irrigation depth (m)
    !   DOXY            : Dissolved oxygen (g/m^3) — irrigation ceases under anoxia
    !   KHS_O2_BIOTURB  : Half-saturation O2 (g/m^3)
    !   day_of_year     : Day of year
    !   seasonal_amp    : Seasonal amplitude
    !   day_peak        : Day of peak activity
    !
    ! Returns:
    !   Irrigation multiplier >= 1.0 (1.0 = no enhancement)
    ! =====================================================================
    elemental function BIOIRRIGATION_FACTOR(alpha0, z_mid, z_irr, &
                                             DOXY, KHS_O2_BIOTURB, &
                                             day_of_year, seasonal_amp, day_peak) result(alpha)
        real(kind=DBL_PREC), intent(in) :: alpha0, z_mid, z_irr
        real(kind=DBL_PREC), intent(in) :: DOXY, KHS_O2_BIOTURB
        real(kind=DBL_PREC), intent(in) :: day_of_year, seasonal_amp, day_peak
        real(kind=DBL_PREC) :: alpha
        real(kind=DBL_PREC) :: z_irr_safe

        z_irr_safe = max(z_irr, 1.0D-20)
        alpha = 1.0D0 + alpha0 * exp(-z_mid / z_irr_safe) &
                       * BIOTURB_O2_SCALING(DOXY, KHS_O2_BIOTURB) &
                       * BIOTURB_SEASONAL_FACTOR(day_of_year, seasonal_amp, day_peak)
    end function BIOIRRIGATION_FACTOR

    ! =====================================================================
    ! APPLY_BIOTURBATION_COEFFS
    !
    ! Populate the PART_MIXING_COEFFS array and compute bioirrigation
    ! multipliers for all nodes, layers, and variables.
    !
    ! This subroutine is called once per sub-timestep from the sediment model
    ! before the transport derivative calculation.
    !
    ! Arguments:
    !   nkn               : Number of nodes
    !   NUM_SED_LAYERS    : Number of sediment layers
    !   NUM_SED_VARS      : Number of state variables
    !   Db0               : Surface biodiffusion coefficient (m^2/day) — scalar input
    !   z_mix             : Bioturbation mixing depth (m)
    !   KHS_O2_BIOTURB    : Half-saturation O2 for bioturbation (g/m^3)
    !   alpha0_irr        : Max bioirrigation enhancement factor
    !   z_irr             : Bioirrigation depth (m)
    !   seasonal_amp      : Seasonal amplitude
    !   day_peak          : Day of peak bioturbation
    !   SED_DEPTHS        : Layer thicknesses (nkn, NUM_SED_LAYERS)
    !   SED_DOXY          : Layer dissolved oxygen (nkn, NUM_SED_LAYERS)
    !   day_of_year       : Current day of year
    !   PART_MIXING_COEFFS: Output — biodiffusion coefficients (nkn, layers, vars)
    !   IRRIG_FACTORS     : Output — bioirrigation multipliers (nkn, layers)
    ! =====================================================================
    subroutine APPLY_BIOTURBATION_COEFFS(nkn, NUM_SED_LAYERS, NUM_SED_VARS, &
                                          Db0, z_mix, KHS_O2_BIOTURB,        &
                                          alpha0_irr, z_irr,                  &
                                          seasonal_amp, day_peak,             &
                                          SED_DEPTHS, SED_DOXY,              &
                                          day_of_year,                        &
                                          PART_MIXING_COEFFS, IRRIG_FACTORS)
        integer, intent(in) :: nkn, NUM_SED_LAYERS, NUM_SED_VARS
        real(kind=DBL_PREC), intent(in) :: Db0, z_mix, KHS_O2_BIOTURB
        real(kind=DBL_PREC), intent(in) :: alpha0_irr, z_irr
        real(kind=DBL_PREC), intent(in) :: seasonal_amp, day_peak
        real(kind=DBL_PREC), intent(in) :: SED_DEPTHS(nkn, NUM_SED_LAYERS)
        real(kind=DBL_PREC), intent(in) :: SED_DOXY(nkn, NUM_SED_LAYERS)
        real(kind=DBL_PREC), intent(in) :: day_of_year
        real(kind=DBL_PREC), intent(out) :: PART_MIXING_COEFFS(nkn, NUM_SED_LAYERS, NUM_SED_VARS)
        real(kind=DBL_PREC), intent(out) :: IRRIG_FACTORS(nkn, NUM_SED_LAYERS)

        integer :: i, k
        real(kind=DBL_PREC) :: z_mid, Db_eff, alpha_val

        ! Compute cumulative midpoint depth per layer and effective Db
        do i = 1, NUM_SED_LAYERS
            do k = 1, nkn
                ! Compute midpoint depth of this layer (from sediment surface)
                z_mid = LAYER_MIDPOINT_DEPTH(SED_DEPTHS, nkn, NUM_SED_LAYERS, k, i)

                ! Effective biodiffusion coefficient
                Db_eff = BIOTURB_EFFECTIVE_DB(Db0, z_mid, z_mix, &
                                              SED_DOXY(k, i), KHS_O2_BIOTURB, &
                                              day_of_year, seasonal_amp, day_peak)
                PART_MIXING_COEFFS(k, i, :) = Db_eff

                ! Bioirrigation factor
                alpha_val = BIOIRRIGATION_FACTOR(alpha0_irr, z_mid, z_irr, &
                                                  SED_DOXY(k, i), KHS_O2_BIOTURB, &
                                                  day_of_year, seasonal_amp, day_peak)
                IRRIG_FACTORS(k, i) = alpha_val
            end do
        end do
    end subroutine APPLY_BIOTURBATION_COEFFS

    ! =====================================================================
    ! LAYER_MIDPOINT_DEPTH
    !
    ! Computes the midpoint depth of a sediment layer from the
    ! sediment-water interface, given layer thicknesses.
    !
    ! z_mid(i) = sum(depths(1:i-1)) + 0.5 * depths(i)
    ! =====================================================================
    pure function LAYER_MIDPOINT_DEPTH(SED_DEPTHS, nkn, NUM_SED_LAYERS, k, i) result(z_mid)
        integer, intent(in) :: nkn, NUM_SED_LAYERS, k, i
        real(kind=DBL_PREC), intent(in) :: SED_DEPTHS(nkn, NUM_SED_LAYERS)
        real(kind=DBL_PREC) :: z_mid
        integer :: l

        z_mid = 0.0D0
        do l = 1, i - 1
            z_mid = z_mid + SED_DEPTHS(k, l)
        end do
        z_mid = z_mid + 0.5D0 * SED_DEPTHS(k, i)
    end function LAYER_MIDPOINT_DEPTH

    ! =====================================================================
    ! BIOTURB_LAST_LAYER_MIXING_RATE
    !
    ! Compute the particle mixing rate for the last (deepest) sediment
    ! layer using a zero-gradient (Neumann) lower boundary condition.
    !
    ! Instead of the previous hard-coded zero, we apply:
    !   PART_MIXING_RATE(N) = -PART_MIXING_RATE(N)
    !
    ! i.e., the mixing derivative for the last layer equals the negative
    ! of the rate at the last layer (no flux below), giving:
    !   PART_MIXING_DERIV(N) = 0 - PART_MIXING_RATE(N)
    !                        = -PART_MIXING_RATE(N)
    !
    ! This is equivalent to assuming no mixing below the deepest layer
    ! (zero-flux boundary), which is physically reasonable and consistent
    ! with how diffusion is handled at the lower boundary.
    !
    ! Arguments:
    !   PART_MIXING_RATE_N : Particle mixing rate at the last layer
    !
    ! Returns:
    !   Particle mixing derivative for the last layer
    ! =====================================================================
    elemental function BIOTURB_LAST_LAYER_MIXING_RATE(PART_MIXING_RATE_N) result(deriv)
        real(kind=DBL_PREC), intent(in) :: PART_MIXING_RATE_N
        real(kind=DBL_PREC) :: deriv

        deriv = -PART_MIXING_RATE_N
    end function BIOTURB_LAST_LAYER_MIXING_RATE

end module AQUABC_SEDIMENT_BIOTURBATION
