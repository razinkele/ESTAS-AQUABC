module AQUABC_PHYSICAL_CONSTANTS
    implicit none

    ! Temperature conversion
    double precision, parameter :: CELSIUS_TO_KELVIN = 273.15D0

    ! Length conversion
    double precision, parameter :: METERS_TO_FEET = 0.3048D0

    ! Mathematical constants
    double precision, parameter :: PI = 3.14159265358979323846D0
    double precision, parameter :: EULER_E = 2.718281828459045D0

    ! Physical constants
    double precision, parameter :: VON_KARMAN = 0.4D0

    ! Time conversion
    double precision, parameter :: SECONDS_PER_DAY = 86400.0D0

    ! Pressure
    double precision, parameter :: STD_PRESSURE_MMHG = 760.0D0

    ! Unit conversions
    double precision, parameter :: MM_TO_M = 1.0D-3

    ! Molar masses in mg/mol (= g/mol * 1000), rounded for model consistency
    double precision, parameter :: FE_MOLAR_MASS_MG = 56000.0D0   ! Iron      (~55.845 g/mol)
    double precision, parameter :: MN_MOLAR_MASS_MG = 54938.0D0   ! Manganese ( 54.938 g/mol)
    double precision, parameter :: S_MOLAR_MASS_MG  = 32000.0D0   ! Sulfur    (~32.06  g/mol)

    ! Water ion product at 25 degrees C
    double precision, parameter :: K_W_25C = 1.0D-14

    ! Numerical safety guards
    double precision, parameter :: EPSILON_GUARD = 1.0D-15
    double precision, parameter :: SAFE_EXP_MIN = -700.0D0
    double precision, parameter :: SAFE_EXP_MAX = 700.0D0

    ! Threshold for detecting unrealistic values (NaN/Inf proxy)
    double precision, parameter :: STRANGER_THRESHOLD = 1.0D30

    ! Minimum concentration floor (used to prevent negative/zero concentrations)
    double precision, parameter :: MIN_CONCENTRATION = 1.0D-10

contains

    elemental double precision function safe_exp(x)
        ! Clamps the argument to [SAFE_EXP_MIN, SAFE_EXP_MAX] before calling exp()
        ! to prevent overflow/underflow in exponential calculations.
        implicit none
        double precision, intent(in) :: x
        double precision :: x_clamped

        x_clamped = max(SAFE_EXP_MIN, min(SAFE_EXP_MAX, x))
        safe_exp = exp(x_clamped)
    end function safe_exp

    elemental double precision function monod_substrate(s, k)
        ! Monod substrate term s/(s+k), defined as 0 where s+k is exactly 0.
        ! With k = 0 (first-order kinetics, as both shipped constant sets use for
        ! K_HS_DOC/DON/DOP_MIN_DOXY) the bare expression is 0/0 = NaN at s = 0;
        ! the term always multiplies s, so 0 is the value that keeps the rate at 0.
        ! Every other input gives exactly the bare s/(s+k).
        implicit none
        double precision, intent(in) :: s, k

        if (s + k /= 0.0D0) then
            monod_substrate = s / (s + k)
        else
            monod_substrate = 0.0D0
        end if
    end function monod_substrate

end module AQUABC_PHYSICAL_CONSTANTS
