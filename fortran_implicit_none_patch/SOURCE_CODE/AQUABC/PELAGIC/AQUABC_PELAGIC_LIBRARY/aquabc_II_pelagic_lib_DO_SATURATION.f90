! Auxilary routines for the pelagic model

! Contents:
!subroutine DO_SATURATION_VEC

subroutine DO_SATURATION_VEC(T, S, H, nkn, CS)
    implicit none

    !Water temperature (in Celcius)
    double precision, dimension(nkn), intent(in) :: T

    !Salinity (in ppt)
    double precision, dimension(nkn), intent(in) :: S

    !Elevation (in m)
    double precision, dimension(nkn), intent(in) :: H

    integer, intent(in) :: nkn

    !Water temperature (in Kelvin)
    double precision, dimension(nkn) :: T_KELVIN

    !Altitude (in feet)
    double precision, dimension(nkn) :: H_FEET

    double precision, dimension(nkn) :: LN_CSF
    double precision, dimension(nkn) :: LN_CSS
    double precision, dimension(nkn) :: CSS
    double precision, dimension(nkn) :: CSP

    double precision, dimension(nkn) :: CS
    !Pressure at altitude H (in atm)
    double precision, dimension(nkn) :: P

    !Standart pressure (in mmHg)
    double precision, dimension(nkn) :: P0

    double precision, dimension(nkn) :: LN_PWV

    !Partial pressure of water vapor (in atm)
    double precision, dimension(nkn) :: PWV

    !A constant
    double precision, dimension(nkn) :: THETA

    T_KELVIN = T + 273.15
    H_FEET = H / 0.3048D0

    !Calculate the effect of temperature on dissolved oxygen saturation
    LN_CSF = -139.34411D0 + (157570.1d0 / T_KELVIN) - &
    &        (66423080.0D0     / (T_KELVIN * T_KELVIN)) + &
    &        (12438000000.0D0  / (T_KELVIN * T_KELVIN * T_KELVIN)) - &
    &        (862194900000.0D0 / (T_KELVIN * T_KELVIN * T_KELVIN * T_KELVIN))

    !Calculate the effect of salinity on dissolved oxygen saturation
    LN_CSS = LN_CSF - S * &
    &        (0.017674D0 - (10.754 / T_KELVIN) + &
    &         (2140.7D0 / (T_KELVIN ** 2.0D0)))

    CSS = exp(LN_CSS)

    !Calculate the effect of altitude on dissolved oxygen saturation

    !Calculate THETA
    THETA = 0.000975 - (0.00001426 * T) + (0.00000006436 * (T ** 2.0D0))

    !Set standard pressure to mean sea level
    P0 = 760.0

    !Calculate atmospheric pressure at altitude H
    P = (P0 - (0.02667 * H_FEET)) / 760.0

    !Calculate vapour pressure of water(DIKKAT)
    LN_PWV = 11.8571 - (3840.7 / T_KELVIN) - &
    &        (216961.0 / (T_KELVIN ** 2.0D0))

    PWV = exp(LN_PWV)

    !Final calculation including altitude effect
    CSP = CSS  * P * (((1.0D0 - (PWV / P)) * (1.0D0 - (THETA * P))) &
    &   / ((1 - PWV) * (1.0D0 - THETA)))

    CS = CSP
end subroutine DO_SATURATION_VEC
