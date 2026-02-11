! Pelagic kinetic model ALUKAS_II
! Version with variables calculated in subroutines
! Version with dissolved inorganic carbon and alkalinity as
! state variables.

!Contains:
! SUBROUTINE cur_smith
! subroutine LIM_LIGHT
! subroutine GROWTH_AT_TEMP
! subroutine AMMONIA_PREFS
! subroutine DUAL_NUTRIENT_PREFERENCE
! function DO_SATURATION
! function KAWIND
! function AMMONIA_VOLATILIZATION
! subroutine CALCULATE_PH_CORR
! subroutine FLX_ALUKAS_II_TO_SED_MOD_1
! subroutine FLX_ALUKAS_II_TO_SED_MOD_1_vec
! function STRANGERSD
! subroutine chlorophyl_a
! subroutine settling_suppres_factor




!*********************************************'
!*                                           *'
!*            AUXILLARY FUNCTIONS            *'
!*                                           *'
!*********************************************'

!Subroutine to calculate temperature limitation factor for growth
! Uses the Cardinal Temperature Model with Inflection (CTMI)
! (Rosso et al. 1993, J. Theor. Biol. 162:447-463)
!
! phi(T) = (T-T_max)*(T-T_min)^2 / ((T_opt-T_min)*D)
! where D = (T_opt-T_min)*(T-T_opt) - (T_opt-T_max)*(T_opt+T_min-2*T)
! phi = 0 outside [T_min, T_max], peaks at 1.0 when T = T_opt
!
! Parameter reinterpretation (signature unchanged for call-site compatibility):
!   Lower_TEMP          -> T_min  (minimum cardinal temperature, zero growth)
!   Upper_TEMP          -> T_opt  (optimal temperature, maximum growth)
!   KAPPA_OVER_OPT_TEMP -> T_max  (maximum cardinal temperature, zero growth)
!   K_AT_OPT_TEMP       -> unused
!   KAPPA_UNDER_OPT_TEMP-> unused
subroutine GROWTH_AT_TEMP(TEMP, LIM_TEMP_GROWTH, Lower_TEMP, Upper_TEMP, K_AT_OPT_TEMP, &
                          KAPPA_UNDER_OPT_TEMP, KAPPA_OVER_OPT_TEMP, nkn)

    implicit none

    integer, intent(in) :: nkn

    double precision, intent(in)  :: TEMP(nkn)
    double precision, intent(out) :: LIM_TEMP_GROWTH(nkn)

    double precision, intent(in) :: Lower_TEMP, Upper_TEMP
    double precision, intent(in) :: K_AT_OPT_TEMP
    double precision, intent(in) :: KAPPA_UNDER_OPT_TEMP
    double precision, intent(in) :: KAPPA_OVER_OPT_TEMP

    ! Local: CTMI aliases for clarity
    double precision :: T_min, T_opt, T_max
    double precision :: denom(nkn)

    T_min = Lower_TEMP
    T_opt = Upper_TEMP
    T_max = KAPPA_OVER_OPT_TEMP

    ! CTMI denominator (computed for all T, guarded later)
    denom = (T_opt - T_min) * ((T_opt - T_min) * (TEMP - T_opt) - &
            (T_opt - T_max) * (T_opt + T_min - 2.0D0 * TEMP))

    where (TEMP <= T_min .or. TEMP >= T_max .or. abs(denom) < 1.0D-20)
        LIM_TEMP_GROWTH = 0.0D0
    elsewhere
        LIM_TEMP_GROWTH = (TEMP - T_max) * (TEMP - T_min)**2 / denom
    end where

    ! Clamp to [0, 1] for numerical safety
    LIM_TEMP_GROWTH = max(0.0D0, min(1.0D0, LIM_TEMP_GROWTH))

end subroutine GROWTH_AT_TEMP


! Generic dual-nutrient preference function (WASP formulation)
! Computes the preference for source1 over source2, given half-saturation ks.
! Used by AMMONIA_PREFS, DIN_DON_PREFS, DIP_DOP_PREFS, AMMONIA_DON_PREFS.
subroutine DUAL_NUTRIENT_PREFERENCE(pref_out, source1, source2, ks, nkn)

    use AQUABC_PHYSICAL_CONSTANTS, only: EPSILON_GUARD

    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: pref_out(nkn)
    double precision, intent(in)  :: source1(nkn)
    double precision, intent(in)  :: source2(nkn)
    double precision, intent(in)  :: ks

    where ((source1 + source2) .lt. 1.0D-6)
        pref_out = 0.0D0
    elsewhere
        pref_out = (source1 * source2) / &
                   (max(ks + source1, EPSILON_GUARD) * max(ks + source2, EPSILON_GUARD)) + &
                   (ks * source1) / &
                   (max(source1 + source2, EPSILON_GUARD) * max(ks + source2, EPSILON_GUARD))
    end where

end subroutine DUAL_NUTRIENT_PREFERENCE


!Function to calculate ammonia preference (WASP)
subroutine AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)

    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: AMMONIA_PREF(nkn)
    double precision, intent(in)  :: NH3(nkn)
    double precision, intent(in)  :: NOx(nkn)
    double precision, intent(in)  :: kn

    call DUAL_NUTRIENT_PREFERENCE(AMMONIA_PREF, NH3, NOx, kn, nkn)

end subroutine AMMONIA_PREFS

!Subroutine to calculate dissolved inorganic nitrogen preference against DON
subroutine DIN_DON_PREFS(DIN_DON_PREF, NH3, DON, frac_avail_DON, NOx, kn, nkn)

    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: DIN_DON_PREF(nkn)
    double precision, intent(in)  :: NH3(nkn)
    double precision, intent(in)  :: DON(nkn)
    double precision, intent(in)  :: frac_avail_DON
    double precision, intent(in)  :: NOx(nkn)
    double precision, intent(in)  :: kn

    double precision :: DIN(nkn)
    double precision :: AVAIL_DON(nkn)

    DIN = NH3 + NOx
    AVAIL_DON = (frac_avail_DON * DON)

    call DUAL_NUTRIENT_PREFERENCE(DIN_DON_PREF, DIN, AVAIL_DON, kn, nkn)

end subroutine DIN_DON_PREFS

!Subroutine to calculate dissolved DIP preference against DOP
subroutine DIP_DOP_PREFS(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)

    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: DIP_DOP_PREF(nkn)
    double precision, intent(in)  :: AVAIL_DIP(nkn)
    double precision, intent(in)  :: AVAIL_DOP(nkn)
    double precision, intent(in)  :: KP

    call DUAL_NUTRIENT_PREFERENCE(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)

end subroutine DIP_DOP_PREFS

!Subroutine to calculate dissolved DOP preference against DIP
subroutine DOP_DIP_PREFS(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)
    ! DIP and DOP should be changed by place during call to routine
    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: DIP_DOP_PREF(nkn)
    double precision, intent(in)  :: AVAIL_DIP(nkn)
    double precision, intent(in)  :: AVAIL_DOP(nkn)
    double precision, intent(in)  :: KP

    double precision :: PP(nkn)

    ! Compute DOP preference, then invert to get DIP preference
    call DUAL_NUTRIENT_PREFERENCE(PP, AVAIL_DIP, AVAIL_DOP, KP, nkn)

    ! Calculating DIP preference in order not to change equations in pelagic kinetics
    ! written for the case when DIP is prefered or DIP pref is given
    DIP_DOP_PREF = 1.D0 - PP

end subroutine DOP_DIP_PREFS

!Subroutine to calculate dissolved nitrogen preference (WASP)
subroutine AMMONIA_DON_PREFS(AMMONIA_DON_PREF, NH3, DON, frac_avail_DON, NOx, kn, nkn)
    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: AMMONIA_DON_PREF(nkn)
    double precision, intent(in)  :: NH3(nkn)
    double precision, intent(in)  :: DON(nkn)
    double precision, intent(in)  :: frac_avail_DON
    double precision, intent(in)  :: NOx(nkn)
    double precision, intent(in)  :: kn

    double precision :: NH3_AND_AVAIL_DON(nkn)

    NH3_AND_AVAIL_DON = NH3 + (frac_avail_DON * DON)

    call DUAL_NUTRIENT_PREFERENCE(AMMONIA_DON_PREF, NH3_AND_AVAIL_DON, NOx, kn, nkn)

end subroutine AMMONIA_DON_PREFS


!Function, which returns saturation concentration of dissolved oxygen
double precision function DO_SATURATION(T, S, H)

    use AQUABC_PHYSICAL_CONSTANTS, only: CELSIUS_TO_KELVIN, METERS_TO_FEET, &
                                         STD_PRESSURE_MMHG, safe_exp

    !Water temperature (in Celcius)
    double precision, intent(in) :: T

    !Salinity (in ppt)
    double precision, intent(in) :: S

    !Elevation (in m)
    double precision, intent(in) :: H


    !Water temperature (in Kelvin)
    double precision :: T_KELVIN

    !Altitude (in feet)
    double precision :: H_FEET

    double precision :: LN_CSF
    double precision :: LN_CSS
    double precision :: CSS
    double precision :: CSP

    double precision       :: CS
    !Pressure at altitude H (in atm)
    double precision :: P

    double precision :: LN_PWV

    !Partial pressure of water vapor (in atm)
    double precision :: PWV

    !A constant
    double precision :: THETA

    T_KELVIN = T + CELSIUS_TO_KELVIN
    H_FEET = H / METERS_TO_FEET

    !Calculate the effect of temperature on dissolved oxygen saturation
    LN_CSF = -139.34411 + (157570.1 / T_KELVIN) - &
             (66423080.0 / (T_KELVIN ** 2.0D0)) + (12438000000.0 / (T_KELVIN ** 3.0D0)) - &
             (862194900000.0 / (T_KELVIN ** 4.0D0))

    !Calculate the effect of salinity on dissolved oxygen saturation
    LN_CSS = LN_CSF - S * &
             (0.017674 - (10.754 / T_KELVIN) + (2140.7 / (T_KELVIN ** 2.0D0)))

    CSS = safe_exp(LN_CSS)

    !Calculate the effect of altitude on dissolved oxygen saturation

    !Calculate THETA
    THETA = 0.000975 - (0.00001426 * T) + (0.00000006436 * (T ** 2.0D0))

    !Calculate atmospheric pressure at altitude H
    P = (STD_PRESSURE_MMHG - (0.02667 * H_FEET)) / STD_PRESSURE_MMHG

    !Calculate vapour pressure of water(DIKKAT)
    LN_PWV = 11.8571 - (3840.7 / T_KELVIN) - (216961.0 / (T_KELVIN ** 2.0D0))

    PWV = safe_exp(LN_PWV)

    !Final calculation including altitude effect
    CSP = CSS  * P * (((1.0D0 - (PWV / P)) * (1.0D0 - (THETA * P))) &
    &   / ((1 - PWV) * (1.0D0 - THETA)))

    CS = CSP
    DO_SATURATION = CS
end function DO_SATURATION


!Function to calculate kind based reareation constant
!Borrowed from EUTRO5, Ambrose et al., 1993
double precision function KAWIND(WINDS, TW, TA, DEPTH, WTYPE)

    use AQUABC_PHYSICAL_CONSTANTS, only: VON_KARMAN, SECONDS_PER_DAY, EULER_E, safe_exp

    !WS         wind speed, m/s
    !TW         water temperature C
    !TA         air temperature C
    !DEPTH      defined depth(segmax) in geometrical
    !WTYPE      type of water body

    double precision, intent(in) :: WINDS
    double precision, intent(in) :: TW
    double precision, intent(in) :: TA
    double precision, intent(in) :: DEPTH
    double precision, intent(in) :: WTYPE

    double precision :: KA_W

    double precision :: WS

    !RK         reareation term calculated in kawind
    double precision :: RK
    double precision :: R0MIN

    integer :: IWTYPE
    integer :: N

    !UT     : SHEAR VELOCITY (CM/SEC)
    !UC     : CRITICAL SHEAR VELOCITY (CM/SEC)
    !KARMAN : VONKARMAN'S CONSTANT
    !ZE     : EQUILIBRIUM ROUGHNESS (CM)
    !1/LAM  : A REYNOLD'S NUMBER
    !GAM    : NONDIMENSIONAL COEFFICIENT DEPENDENT ON WATER BODY SIZE.
    double precision :: UT
    double precision :: UC
    double precision :: KARMAN
    double precision :: ZE
    double precision :: LAM
    double precision :: GAM

    !DifF : DifFUSIVITY OF OXYGEN IN WATER (CM**2/SEC)
    !VW   : VISCOSITY OF WATER             (CM**2/SEC)
    !VA   : VISCOSITY OF AIR               (CM**2/SEC)
    !PW   : DENSITY OF WATER               (G/CM**3)
    !PA   : DENSITY OF AIR                 (G/CM**3)
    double precision :: DifF
    double precision :: VW
    double precision :: VA
    double precision :: PA
    double precision :: PW

    double precision :: KA3
    double precision :: WH
    double precision :: SRCD
    double precision :: ERR
    double precision :: EF
    double precision :: F1
    double precision :: F2
    double precision :: FP1
    double precision :: FP2
    double precision :: FP3
    double precision :: FP4
    double precision :: SRCD2
    double precision :: CDDRAG
    double precision :: US
    double precision :: Z0
    double precision :: RK1
    double precision :: RK2
    double precision :: RK3
    double precision :: GAMU

    R0MIN = 1.e-15
    IWTYPE = int(WTYPE)
    WS = WINDS

    if (IWTYPE.EQ.3) then
        UT  = 10.0
        UC  = 11.0
        ZE  = 0.35
        LAM = 3.0
        GAM = 5.0
    else
        if (IWTYPE .EQ.1) then
            UT  = 9.0
            UC  = 22.0
            ZE  = 0.25
            LAM = 10.0
            GAM = 10.0
        else
            if (IWTYPE.EQ.2) then
                UT  = 10.0
                UC  = 11.0
                ZE  = 0.25
                LAM = 3.0
                GAM = 6.5
            else
                WRITE(*,*) 'KAWIND : WRONG VALUE FOR WATERBODY TYPE'
                STOP
            end if
        end if

    end if


    DifF = 4.58E-07 * TW + 1.2E-05
    VW   = 0.0164 - 0.00024514*TW
    VA   = 0.133 + 0.0009*TA
    PA   = 0.00129 - 0.0000040*TA
    PW   = 1.00
    WS   = WS * 100.0
    RK   = 1.0

    !NEWTON RAPHSON METHOD TO CALCULATE THE SQUARE ROOT OF THE DRAG
    !COEFFICIENT
    N = 0

    KARMAN = VON_KARMAN
    KA3    = KARMAN**0.3333
    WH     = 1000.0

    !INITIAL GUESS FOR SQUARE ROOT OF THE DRAG COEFFICIENT
    SRCD = 0.04
    ERR  = 1.0

    do N = 1, 9
        !CALCULATE VALUE OF FUNCTION(F2) AND
        !DERIVATIVE OF FUNCTION(FP)
        EF  = safe_exp( - SRCD*WS/UT)
        F1  = LOG((WH/ZE) + (WH*LAM/VA)*SRCD*WS*EF)
        F2  = F1 - KARMAN / SRCD
        FP1 = 1.0/((WH/ZE) + (LAM*WH/VA)*SRCD*WS*EF)
        FP2 = ((WH*LAM)/(VA*UT))*SRCD*(WS**2.0)*EF
        FP3 = (WH*(LAM / VA)) * WS * EF
        FP4 = FP1*(FP2 + FP3) + (KARMAN/(SRCD**2.0))

       !A NEW GUESS FOR SQUARE ROOT OF DRAG AND COMPARE TO
       !PREVIOUS GUESS AND LOOP BACK THROUGH N-R WITH NEW GUESS
       !if APPROPRIATE
       SRCD2 = SRCD - F2/FP4
       ERR = ABS(SRCD - SRCD2)
       SRCD = SRCD2

       if (ERR.LE.0.0005) then
           EXIT
       end if

    end do

    if ((ERR.GT.0.005).AND.(N.EQ.9)) then
        WRITE(*,*) 'KAWIND : SOLUTION DID NOT CONVERGE'
        STOP
    end if

    CDDRAG = SRCD**2.0
    US     = SRCD * WS
    Z0     = 1.0 / ((1.0 / ZE) + LAM * US * safe_exp(-US / UT) / VA)
    WS     = WS / 100.0

    if (WS.LT.6.0) then
        RK1 = ((DifF / VW)**0.666667) * SRCD * ((PA / PW)**0.5)
        RK  = RK1 * KA3 * (WS/GAM)
        RK  = RK * SECONDS_PER_DAY
        RK  = RK / DEPTH
    end if

    if ((WS.GE.6.0).AND.(WS.LE.20.0)) then
        GAMU = GAM * US * (safe_exp(-(US / UC) + 1.0) / UC)
        RK1  = ((DifF/VW)**0.6667) * KA3 * ((PA/PW)**0.5) * (US / GAMU)
        RK2  = ((DifF * US * PA * VA) / (KARMAN * Z0 * PW * VW))**0.5
        RK3  = (1.0 / RK1) + (1.0 / RK2)
        RK   = 1.0 / RK3
        RK   = RK * SECONDS_PER_DAY / 100.0
        RK   = RK / DEPTH
    end if

    if (WS.GT.20.0) then
        RK = ((DifF * PA * VA * US) / (KARMAN * ZE * PW * VW))**0.5
        RK = RK * SECONDS_PER_DAY / 100.0
        RK = RK / DEPTH
    end if

    KA_W = RK
    KAWIND = KA_W

end function KAWIND

!************************************************************************

!********************************************************************
!********************************************************************
subroutine LIM_LIGHT(Ia, TCHLA, GITMAX, H, ke, LLIGHT, CCHL_RATIO, K_LIGHT_SAT, LIGHT_SAT, nkn, BETA)

    !   Depth-averaged Steele light limitation with tunable photoinhibition
    !
    !   The base Steele (1962) depth-averaged P-I curve is extended with
    !   a Platt-style photoinhibition parameter BETA (dimensionless, >= 0):
    !
    !     f(I) = (1+BETA) * (I/I_s) * exp(1 - (1+BETA)*I/I_s)
    !
    !   Depth-averaged analytically:
    !     <f> = (e / (ke*H)) * [exp(-(1+BETA)*I_bot/I_s) - exp(-(1+BETA)*I_surf/I_s)]
    !
    !   When BETA=0 this reduces exactly to the original Steele formula.
    !   When BETA>0 the optimum shifts to lower light: I_opt = I_s/(1+BETA),
    !   and photoinhibition above I_opt is stronger.
    !
    !   Parameters:
    ! Inputs:
    !      Ia         -   Instanteneous light intensity (langleys/day PAR)
    !      TCHLA      -   total chlorophyl for all phytoplankton groups, mcg/l
    !      CCHL_RATIO -   carbon to chlorophyl ratio for the phyto group
    !      GITMAX     -   temperature corrected maximum relative growth rate, 1/day
    !      H          -   depth, m
    !      ke         -   light extinction coefficient, 1/m
    !      K_LIGHT_SAT-   user defined light saturation, langleys/day PAR
    !      BETA       -   photoinhibition parameter (0=Steele default, >0=stronger)
    ! Outputs:
    !      LLIGHT     -   light limitation factor [0,~1]
    !      LIGHT_SAT  -   saturation light intensity, returned for control

    ! Constants:
    !      XKC    - Chlorophyll light extinction coefficient (1/m), parameter
    !      PHIMX - Max. Quantum Yield, parameter

    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use AQUABC_PHYSICAL_CONSTANTS, only: EULER_E, safe_exp
    use, intrinsic :: ieee_arithmetic

    implicit none
    integer, intent(in) :: nkn

    double precision, intent(in)  :: Ia(nkn)
    double precision, intent(in)  :: TCHLA(nkn)
    double precision, intent(in)  :: GITMAX(nkn)
    double precision, intent(in)  :: H(nkn)
    double precision, intent(in)  :: ke(nkn)
    double precision, intent(in)  :: CCHL_RATIO
    double precision, intent(in)  :: K_LIGHT_SAT
    double precision, intent(out) :: LLIGHT(nkn)
    double precision, intent(out) :: LIGHT_SAT(nkn)
    double precision, intent(in)  :: BETA

    double precision :: KESHD(nkn)
    double precision :: SKE(nkn)
    double precision :: TEMP1(nkn)
    double precision :: TEMP2(nkn)
    double precision :: TEMP3(nkn)
    double precision :: chla_pos(nkn)
    double precision :: BETA_LOC  ! local copy of photoinhibition parameter

    logical VALUE_strange(nkn)
    integer STRANGERSD
    integer :: i

    integer user_defined_saturation

    user_defined_saturation = 0

    ! Set local photoinhibition parameter (0 = original Steele)
    BETA_LOC = max(BETA, 0.0D0)

    if((PHIMX .le. 0.D0) .or.  (XKC .le. 0.D0) .or. all((GITMAX .le. 0.D0))) then
      user_defined_saturation = 1
    end if

    SKE       = ke

    if (STRANGERSD(ke, VALUE_strange, nkn).eq.1) then
        write(6,*) 'LIM_LIGHT: ke has strange values'
        write(6,*) 'ke=', ke
    end if

    if (STRANGERSD(H, VALUE_strange, nkn).eq.1) then
        write(6,*) 'LIM_LIGHT: H has strange values'
        write(6,*) 'H=', H
    end if

    if (STRANGERSD(Ia, VALUE_strange, nkn).eq.1) then
        write(6,*) 'LIM_LIGHT: Ia has strange values'
        write(6,*) 'Ia=', Ia
    end if

    if (STRANGERSD(TCHLA, VALUE_strange, nkn).eq.1) then
        write(6,*) 'LIM_LIGHT: TCHLA has strange values'
        write(6,*) 'TCHLA=', TCHLA
    end if

    TEMP1     = SKE * H

    ! Check TEMP1 finiteness per index and report offending index
    do i = 1, nkn
        if (.not. ieee_is_finite(TEMP1(i))) then
            write(6,*) 'LIM_LIGHT: NON-FINITE TEMP1 at i=', i, 'ke=', ke(i), 'H=', H(i), 'TEMP1=', TEMP1(i)
            stop
        end if
    end do

    ! Guard against zero TEMP1 (ke*H) — at very shallow depth or zero extinction
    do i = 1, nkn
        if (TEMP1(i) .lt. 1.0D-10) then
            TEMP1(i) = 1.0D-10
        end if
    end do

    if(user_defined_saturation .eq. 0) then
     !1/TEMP2 - the saturating light intensity
     ! Guard: ensure denominator is not zero
     do i = 1, nkn
         if (abs(GITMAX(i)) .lt. 1.0D-20) then
             TEMP2(i) = 1.0D0 / K_LIGHT_SAT
         else
             TEMP2(i) = (0.083D0 * PHIMX * XKC) / (GITMAX(i) * CCHL_RATIO * EULER_E)
         end if
     end do
     ! Guard: ensure TEMP2 is not zero before inversion
     where (abs(TEMP2) .lt. 1.0D-30)
         LIGHT_SAT = K_LIGHT_SAT
     elsewhere
         LIGHT_SAT = 1.0D0 / TEMP2
     end where
     where(LIGHT_SAT .lt. 10.D0)
      LIGHT_SAT = K_LIGHT_SAT
     end where
    else
     if (abs(K_LIGHT_SAT) .lt. 1.0D-20) then
         write(6,*) 'LIM_LIGHT: K_LIGHT_SAT is zero or near-zero, cannot compute TEMP2'
         stop
     end if
     TEMP2     = 1.0D0/K_LIGHT_SAT
     LIGHT_SAT = K_LIGHT_SAT
    end if

    if (STRANGERSD(LIGHT_SAT,VALUE_strange,nkn).eq.1) then
        write(6,*) 'LIM_LIGHT: TEMP2 is NaN '

        write(6,*) 'LIGHT_SAT=',LIGHT_SAT
        write(6,*) 'TEMP2=',TEMP2
        write(6,*) 'GITMAX=', GITMAX
        write(6,*) 'CCHL_RATIO=', CCHL_RATIO
        stop
    end if

    ! Photoinhibition: multiply TEMP2 (= 1/I_s) by (1+BETA) to shift
    ! the optimum to lower light and strengthen inhibition above I_opt.
    ! When BETA=0 this is a no-op and the formula is the original Steele.
    TEMP2 = TEMP2 * (1.0D0 + BETA_LOC)

    TEMP3  = safe_exp( - TEMP1)
    LLIGHT = (EULER_E / TEMP1) * (safe_exp( -TEMP2 * Ia * TEMP3) - safe_exp( -TEMP2 * Ia))

    if (STRANGERSD(LLIGHT,VALUE_strange,nkn).eq.1) then
        write(6,*) 'LIM_LIGT: Light limitation value is strange'

        write(6,*) 'LLIGHT=',LLIGHT
        write(6,*) 'LIGHT_SAT=',LIGHT_SAT
        write(6,*) 'TEMP1', TEMP1
        write(6,*) 'TEMP2=',TEMP2
        write(6,*) 'TEMP3=',TEMP3
        write(6,*) 'GITMAX=', GITMAX
        write(6,*) 'Ia', Ia
        write(6,*) 'CCHL_RATIO=', CCHL_RATIO
        write(6,*) 'K_LIGHT_SAT=', K_LIGHT_SAT
        write(6,*) 'ke=', ke
        write(6,*) 'H=', H
        stop
    end if


 end subroutine LIM_LIGHT

!********************************************************************
!********************************************************************


!***********************************
subroutine light_kd(kdb, kd, chla, nkn)
    implicit none

    ! Derived from measured kd in Curonian lagoon
   integer, intent(in) :: nkn
   double precision, intent(in)  :: kdb(nkn)
   double precision, intent(out) :: kd(nkn)
   double precision, intent(in)  :: chla(nkn)

   where(chla .le. 50.d0 )
       kd = kdb + 0.4d0 + 0.02d0*chla
   elsewhere(chla .ge. 50.d0 )
       kd = kdb + 1.d0 + 0.008d0*chla
   end where

end subroutine light_kd



!********************************************************************
subroutine CUR_SMITH(Ia, TCHLA, CCHLXI, GITMAX, H, ke, LLIGHT, CCHLX)

    use AQUABC_PHYSICAL_CONSTANTS, only: PI, EULER_E, safe_exp

    implicit none

    !    Can not be used for instanteneous light
    !    while total dayllight is unknown to adjust C to Chla ratio!

    !   Dick-Smith light limitation formulation(adapted from EUTRO).
    !
    !   Version to use in ALUKAS with instanteneous light intensity and variable
    !   (calculated inside) C to Chla ratio
    !   Pytoplankton growth is modelled full day
    !   Double precision variables
    !
    !   Parameters:
    !      PHOTO - fraction of the day with light, renamed by FDAY later in the program.
    !      Ia    - Instanteneous light intensity (langleys), renamed to ITOT in the program
    !      TCHLA - total chlorophyl for all phytoplankton groups
    !      CCHLXI- carbon to chlorophyl ratio for the current step
    !      GITMAX - temperature corrected maximum relative growth for phytoplankton group
    !      H      - depth
    !      ke     - light extinction coefficient for the water free of phytoplankton
    !      CCHLX  - carbon to chlorophyl ratio calculated by subroutine for the next time step
    !      XKC    - HARDCODED. Chlorophyll light extinction coefficient (1/m)
    !      PHIMAX - HARDCODED. Max. Quantum Yield

    double precision, intent(in)  :: Ia
    double precision, intent(in)  :: TCHLA
    double precision, intent(in)  :: CCHLXI
    double precision, intent(in)  :: GITMAX
    double precision, intent(in)  :: H
    double precision, intent(in)  :: ke
    double precision, intent(out) :: LLIGHT
    double precision, intent(out) :: CCHLX

    double precision :: PHOTO
    double precision :: XKC
    double precision :: PHIMX

    double precision :: FDAY
    double precision :: ITOT
    double precision :: CCHL1
    double precision :: KESHD
    double precision :: SKE
    double precision :: TEMP1
    double precision :: TEMP2
    double precision :: TEMP3
    double precision :: IMAX
    double precision :: SUM
    double precision :: DTDAY
    double precision :: I0
    double precision :: RLIGHT
    double precision :: IAV
    double precision :: IAVSG
    integer :: I

    ! Chloroph. extinction, ( mcg Chla/l/m)
    ! 0.04 is approximatelly the value that corresponds to the angle of curve given in Chapra
    XKC   =  0.016
    PHIMX = 720.0   !PHY   Quantum yield const. mg C/mole photon
    FDAY  = 1.0D0
    ITOT  = Ia
    CCHL1 = CCHLXI
    KESHD = XKC * TCHLA   !Chla commes in micrograms
    SKE   = ke
    SKE   = SKE + KESHD
    TEMP1 = SKE * H

    if (GITMAX .lt. 1D-20 .or. CCHL1 .lt. 1D-20) then
        write(6,*) 'SMITH: TEMP2 is NaN ', 'GITMAX=', GITMAX,'CCHL1=', CCHL1
        stop
    end if

    ! Guard against division by zero when TEMP1 (extinction * depth) is very small
    ! This can happen at very shallow depths or with zero extinction coefficient
    if (TEMP1 .lt. 1.0D-10) then
        ! At very shallow depth or no extinction, assume full light availability
        LLIGHT = 1.0D0
        CCHLX  = 15.0D0  ! Minimum C:Chla ratio
        return
    end if

    !1/TEMP2 - the saturating light intensity
    TEMP2 = 0.083D0 * PHIMX * XKC / (GITMAX * CCHL1 * EULER_E)
    TEMP3 = safe_exp( - TEMP1) !fraction of the light at bottom

    !Light limitation varies during the day
    RLIGHT = EULER_E / TEMP1 * (safe_exp( -TEMP2 * ITOT * TEMP3) - safe_exp( -TEMP2 * ITOT))
    LLIGHT = RLIGHT

    !Adapt carbon to chlorophyll ratio:

    ! It can not be used for instantenous light because total light is unknown!
    IAV=0.9D0 * ITOT/FDAY
    IAVSG=IAV*(1.0D0-TEMP3)/TEMP1
    CCHLX=0.3D0 * 0.083D0 * PHIMX * XKC * IAVSG / (GITMAX * EULER_E)

    if (CCHLX.LT.15.0D0) then
        CCHLX=15.0D0
    end if
end subroutine CUR_SMITH

!********************************************************************
!********************************************************************
!Function to calculate the volatilization rate of unionized ammonia
subroutine AMMONIA_VOLATILIZATION(AMMONIA_VOLATIL_RATE, NH4N, pH, TEMP, KA, nkn)

! Output:
!    AMMONIA_VOLATIL_RATE
! Inputs:
!    NH4N,
!    pH,
!    TEMP,
!    KA,
!    nkn

    implicit none
    integer, intent(in) :: nkn

    double precision, intent(in)  :: NH4N(nkn)
    double precision, intent(in)  :: pH(nkn)
    double precision, intent(in)  :: TEMP(nkn)
    double precision, intent(in)  :: KA(nkn)

    double precision, intent(out) :: AMMONIA_VOLATIL_RATE(nkn)
    double precision :: NH3N(nkn)
    double precision :: NH3S

    NH3S = 0.0D0                              !Taken zero for a while assuming that the partial pressure of
                                              !unionized ammonia is zero in the atmosphere (unpolluted air)
    call UNIONIZED_AMMONIA(NH3N, NH4N, pH, TEMP, nkn)

    !1.4D1/1.70D1 : Ratio of NH3N:NH3
    !3.2D1/1.7D1  : Ratio of molecular weight of oxygen to ammonia
    AMMONIA_VOLATIL_RATE = KA * (NH3N - (NH3S * (1.4D1/1.70D1))) * ((3.2D1/1.7D1)**2.5D-1)
end subroutine AMMONIA_VOLATILIZATION

!************************************************************************
!************************************************************************

!Function to calculate the concentration of unionized ammonia
subroutine UNIONIZED_AMMONIA(NH3N, NH4N, pH, TEMP, nkn)
    ! Output:
    !     NH3N
    ! Inputs:
    !   NH4N,
    !   pH,
    !   TEMP,
    !   nkn

    use AQUABC_PHYSICAL_CONSTANTS, only: CELSIUS_TO_KELVIN

    implicit none
    integer, intent(in) :: nkn

    double precision, intent(out) :: NH3N(nkn)

    double precision, intent(in) :: NH4N(nkn)
    double precision, intent(in) :: pH(nkn)
    double precision, intent(in) :: TEMP(nkn)

    double precision :: FRAC_NH3(nkn)
    double precision :: pKH(nkn)
    double precision :: T_KELVIN(nkn)

    T_KELVIN = TEMP + CELSIUS_TO_KELVIN
    pKH      = 9.018D-2 + (2.72992D3 / T_KELVIN)
    FRAC_NH3 = 1.0D0 / (1.0D0 + (1.0D1 ** (pKH - pH)))
    NH3N     = FRAC_NH3 * NH4N
end subroutine UNIONIZED_AMMONIA

!************************************************************************
!************************************************************************



subroutine FLX_ALUKAS_II_TO_SED_MOD_1 &
           (STATE_VARIABLES               , NUM_VARS                , &
            MODEL_CONSTANTS               , NUM_CONSTS              , &
            DRIVING_FUNCTIONS             , NUM_DRIV                , &
            SETTLING_VELOCITIES           , DISSOLVED_FRACTIONS     , &
            BOTTOM_FACTOR                 , &
			CELLNO                        , PSTIME                  , &
            SETTLING_RATES                , FLUXES, NUM_FLUXES      , &
            SEDIMENT_TYPE                 , FRACTION_OF_DEPOSITION  , &
            NOT_DEPOSITED_FLUXES          , NUM_NOT_DEPOSITED_FLUXES, &
            CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES)

    ! Outputs :
	!
    !  SETTLING_RATES       - potential flux by settling, g/m2/day
	!
    !  FLUXES               - after substraction of not deposited fluxes
	!                         for each BS state variable in g/m2/day
    !
	!  NOT_DEPOSITED_FLUXES - for each WC state variable, g/m2/day

    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use AQUABC_PEL_STATE_VAR_INDEXES
    use GLOBAL, only: nstate, NUM_FLUXES_TO_SEDIMENTS

    implicit none

    integer, intent(in) :: NUM_VARS
    integer, intent(in) :: NUM_CONSTS
    integer, intent(in) :: NUM_FLUXES
    integer, intent(in) :: NUM_DRIV

    integer, intent(in) :: CONSIDER_NON_OBLIGATORY_FIXERS
    integer, intent(in) :: CONSIDER_NOSTOCALES

    double precision, intent(in) :: STATE_VARIABLES(NUM_VARS)
    double precision, intent(in) :: MODEL_CONSTANTS(NUM_CONSTS)

    double precision, intent(in) :: SETTLING_VELOCITIES(NUM_VARS)
    double precision, intent(in) :: DISSOLVED_FRACTIONS(NUM_VARS)

    double precision, intent(out) :: SETTLING_RATES(NUM_VARS)
    double precision, intent(out) :: FLUXES(NUM_FLUXES)

    double precision :: FLUXES_FROM_WC(NUM_VARS)

    double precision, intent(in) :: DRIVING_FUNCTIONS(NUM_DRIV)

    integer, intent(in) :: CELLNO
    integer :: LAYER

    double precision, intent(in) :: PSTIME
    double precision, intent(in) :: BOTTOM_FACTOR

    integer, intent(in) :: SEDIMENT_TYPE
    integer, intent(in) :: NUM_NOT_DEPOSITED_FLUXES
    double precision, intent(in)  :: FRACTION_OF_DEPOSITION(NUM_NOT_DEPOSITED_FLUXES)
    double precision, intent(out) :: NOT_DEPOSITED_FLUXES(NUM_NOT_DEPOSITED_FLUXES)

    double precision :: SETTLING_FACTORS(NUM_VARS)

    double precision chla, settlsup_factor

    integer DO_NON_OBLIGATORY_FIXERS
    integer DO_NOSTOCALES

    integer I

    if (CONSIDER_NON_OBLIGATORY_FIXERS > 0) then
        DO_NON_OBLIGATORY_FIXERS = 1
    else
        DO_NON_OBLIGATORY_FIXERS = 0
    end if


    if (CONSIDER_NOSTOCALES > 0) then
        DO_NOSTOCALES = 1
    else
        DO_NOSTOCALES = 0
    end if

    if(num_vars .ne. nstate) then
        print *, 'FLX_ALUKAS_II_TO_SED_MOD_1:'
        print *, 'To get values correctly by fluxes from WC and not deposited fluxes'
        print *, 'number of state variables should be equal to', nstate, 'but is ', NUM_VARS
        stop
    end if

	! Added by PZem 2019-07-23
    call chlorophyl_a(state_variables, num_vars, chla, &
     CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES)

    ! to account impact of high concentrations of phytoplankton to
	! settling velocity
    call settling_suppres_factor(chla, settlsup_factor)

    do i = 1, NUM_VARS
        !bottom factor =1 always when ST is switched off. It is zero when erosion.
        ! Last multiplier added by PZem 2019-07-23
        SETTLING_FACTORS(i) = &
            BOTTOM_FACTOR * &
                (1.0D+0 - DISSOLVED_FRACTIONS(i)) * SETTLING_VELOCITIES(i) * &
                settlsup_factor

        if (SETTLING_FACTORS(i).lt.0.0D0) then
            SETTLING_FACTORS(i) = 0.0D0
        end if

        ! PZem: 2019-02-20 for control settling by erosion-deposition
        SETTLING_RATES(i) = STATE_VARIABLES(i)*SETTLING_FACTORS(i)
    end do

    FLUXES_FROM_WC(1:NUM_VARS) = &
	    STATE_VARIABLES(1:NUM_VARS) * SETTLING_FACTORS(1:NUM_VARS)

    !NOT DEPOSITED FLUXES
    NOT_DEPOSITED_FLUXES(1:NUM_VARS) = &
	    FLUXES_FROM_WC(1:NUM_VARS) * (1.0D0 - FRACTION_OF_DEPOSITION(1:NUM_VARS))

    ! FLUXES FROM WC TO BS FOR BS VARIABLES
    if(NUM_FLUXES .ne. NUM_FLUXES_TO_SEDIMENTS) then
        print *, 'FLX_ALUKAS_II_TO_SED_MOD_1:'
        print *, 'To get values correctly by fluxes to sediments and not deposited fluxes'
        print *, 'number of BS state vars(fluxes) should be equal to', NUM_FLUXES_TO_SEDIMENTS, &
                 'but is ', NUM_FLUXES
        stop
    end if

    !AMMONIA NITROGEN FLUX
    FLUXES(1) = FLUXES_FROM_WC(NH4_N_INDEX) * FRACTION_OF_DEPOSITION(NH4_N_INDEX)

    !NITRATE NITROGEN FLUX
    FLUXES(2) = FLUXES_FROM_WC(NO3_N_INDEX) * FRACTION_OF_DEPOSITION(NO3_N_INDEX)

    !DISSOLVED ORGANIC NITROGEN FLUX
    FLUXES(3) = FLUXES_FROM_WC(DISS_ORG_N_INDEX) * FRACTION_OF_DEPOSITION(DISS_ORG_N_INDEX)

    !PARTICULATE ORGANIC NITROGEN FLUX
    FLUXES(4) = &
       (FLUXES_FROM_WC(DIA_C_INDEX)          * FRACTION_OF_DEPOSITION(DIA_C_INDEX) * DIA_N_TO_C) + &
       (FLUXES_FROM_WC(CYN_C_INDEX)          * FRACTION_OF_DEPOSITION(CYN_C_INDEX) * CYN_N_TO_C) + &
       (FLUXES_FROM_WC(OPA_C_INDEX)          * FRACTION_OF_DEPOSITION(OPA_C_INDEX) * OPA_N_TO_C) + &
       (FLUXES_FROM_WC(ZOO_N_INDEX)          * FRACTION_OF_DEPOSITION(ZOO_N_INDEX)             ) + &
       (FLUXES_FROM_WC(DET_PART_ORG_N_INDEX) * FRACTION_OF_DEPOSITION(DET_PART_ORG_N_INDEX)    )

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
       FLUXES(4) = &
           FLUXES(4) + &
           (FLUXES_FROM_WC(FIX_CYN_C_INDEX) * &
            FRACTION_OF_DEPOSITION(FIX_CYN_C_INDEX)      * FIX_CYN_N_TO_C)
    end if

    if (DO_NOSTOCALES > 0) then
       FLUXES(4) = &
           FLUXES(4) + &
           (FLUXES_FROM_WC(NOST_VEG_HET_C_INDEX) * &
            FRACTION_OF_DEPOSITION(NOST_VEG_HET_C_INDEX) * NOST_N_TO_C)
    end if

    !PHOSPHATE FLUX
    FLUXES(5) = FLUXES_FROM_WC(PO4_P_INDEX) * FRACTION_OF_DEPOSITION(PO4_P_INDEX)

    !DISSOLVED ORGANIC PHOSPHORUS FLUX
    FLUXES(6) = FLUXES_FROM_WC(DISS_ORG_P_INDEX) * FRACTION_OF_DEPOSITION(DISS_ORG_P_INDEX)

    !PARTICULATE ORGANIC PHOSPHORUS FLUX
    FLUXES(7) = &
       (FLUXES_FROM_WC(DIA_C_INDEX)          * FRACTION_OF_DEPOSITION(DIA_C_INDEX)     * DIA_P_TO_C    )  + &
       (FLUXES_FROM_WC(CYN_C_INDEX)          * FRACTION_OF_DEPOSITION(CYN_C_INDEX)     * CYN_P_TO_C    )  + &
       (FLUXES_FROM_WC(OPA_C_INDEX)          * FRACTION_OF_DEPOSITION(OPA_C_INDEX)     * OPA_P_TO_C    )  + &
       (FLUXES_FROM_WC(ZOO_P_INDEX)          * FRACTION_OF_DEPOSITION(ZOO_P_INDEX)                     )  + &
       (FLUXES_FROM_WC(DET_PART_ORG_P_INDEX) * FRACTION_OF_DEPOSITION(DET_PART_ORG_P_INDEX)            )

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
       FLUXES(7) = &
           FLUXES(7) + &
           (FLUXES_FROM_WC(FIX_CYN_C_INDEX) * &
            FRACTION_OF_DEPOSITION(FIX_CYN_C_INDEX)      * FIX_CYN_P_TO_C)
    end if

    if (DO_NOSTOCALES > 0) then
       FLUXES(7) = &
           FLUXES(7) + &
           (FLUXES_FROM_WC(NOST_VEG_HET_C_INDEX) * &
            FRACTION_OF_DEPOSITION(NOST_VEG_HET_C_INDEX) * NOST_P_TO_C)
    end if

    !DISSOLVED OXYGEN
    FLUXES(8) = 0.0D0

    !DISSOLVED ORGANIC CARBON
    FLUXES(9) = FLUXES_FROM_WC(DISS_ORG_C_INDEX) * FRACTION_OF_DEPOSITION(DISS_ORG_C_INDEX)

    !PARTICULATE ORGANIC CARBON FLUX
    FLUXES(10) = &
       (FLUXES_FROM_WC(DIA_C_INDEX)          * FRACTION_OF_DEPOSITION(DIA_C_INDEX)         ) + &
       (FLUXES_FROM_WC(CYN_C_INDEX)          * FRACTION_OF_DEPOSITION(CYN_C_INDEX)         ) + &
       (FLUXES_FROM_WC(OPA_C_INDEX)          * FRACTION_OF_DEPOSITION(OPA_C_INDEX)         ) + &
       (FLUXES_FROM_WC(ZOO_C_INDEX)          * FRACTION_OF_DEPOSITION(ZOO_C_INDEX)         ) + &
       (FLUXES_FROM_WC(DET_PART_ORG_C_INDEX) * FRACTION_OF_DEPOSITION(DET_PART_ORG_C_INDEX))

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
       FLUXES(10) = &
           FLUXES(10) + &
           (FLUXES_FROM_WC(FIX_CYN_C_INDEX)      * FRACTION_OF_DEPOSITION(FIX_CYN_C_INDEX))
    end if

    if (DO_NOSTOCALES > 0) then
       FLUXES(10) = &
           FLUXES(10) + &
           (FLUXES_FROM_WC(NOST_VEG_HET_C_INDEX) * &
            FRACTION_OF_DEPOSITION(NOST_VEG_HET_C_INDEX))
    end if

    !DISSOLVED SILICON
    FLUXES(11) = FLUXES_FROM_WC(DISS_Si_INDEX)   * FRACTION_OF_DEPOSITION(DISS_Si_INDEX)

    !PARTICULATE  SILICON FLUX
    FLUXES(12) = &
       (FLUXES_FROM_WC(DIA_C_INDEX)   * FRACTION_OF_DEPOSITION(DIA_C_INDEX) * DIA_Si_TO_C) + &
       (FLUXES_FROM_WC(PART_Si_INDEX) * FRACTION_OF_DEPOSITION(PART_Si_INDEX))

    FLUXES(13) = FLUXES_FROM_WC(INORG_C_INDEX) * FRACTION_OF_DEPOSITION(INORG_C_INDEX) !INORGANIC CARBON FLUX
    FLUXES(14) = FLUXES_FROM_WC(TOT_ALK_INDEX) * FRACTION_OF_DEPOSITION(TOT_ALK_INDEX) !ALKALINITY FLUX
    FLUXES(15) = 0.D0 !Salinity flux

    FLUXES(16) =  FLUXES_FROM_WC(FE_II_INDEX    ) *  FRACTION_OF_DEPOSITION(FE_II_INDEX    ) !FEII  FLUX
    FLUXES(17) =  FLUXES_FROM_WC(FE_III_INDEX   ) *  FRACTION_OF_DEPOSITION(FE_III_INDEX   ) !FEIII FLUX
    FLUXES(18) =  FLUXES_FROM_WC(MN_II_INDEX    ) *  FRACTION_OF_DEPOSITION(MN_II_INDEX    ) !MNII  FLUX
    FLUXES(19) =  FLUXES_FROM_WC(MN_IV_INDEX    ) *  FRACTION_OF_DEPOSITION(MN_IV_INDEX    ) !MNIV  FLUX
    FLUXES(20) =  FLUXES_FROM_WC(CA_INDEX       ) *  FRACTION_OF_DEPOSITION(CA_INDEX       ) !CA    FLUX
    FLUXES(21) =  FLUXES_FROM_WC(MG_INDEX       ) *  FRACTION_OF_DEPOSITION(MG_INDEX       ) !MG    FLUX
    FLUXES(22) =  FLUXES_FROM_WC(S_PLUS_6_INDEX ) *  FRACTION_OF_DEPOSITION(S_PLUS_6_INDEX ) !S_PLUS_6  FLUX
    FLUXES(23) =  FLUXES_FROM_WC(S_MINUS_2_INDEX) *  FRACTION_OF_DEPOSITION(S_MINUS_2_INDEX) !S_MINUS_2 FLUX
    FLUXES(24) =  FLUXES_FROM_WC(CH4_C_INDEX    ) *  FRACTION_OF_DEPOSITION(CH4_C_INDEX    ) !CH4_C     FLUX
end subroutine FLX_ALUKAS_II_TO_SED_MOD_1

!********************************************************************
!********************************************************************

subroutine FLX_ALUKAS_II_TO_SED_MOD_1_VEC &
           (STATE_VARIABLES               , nkn        , NUM_VARS, &
            MODEL_CONSTANTS               , NUM_CONSTS ,         &
            DRIVING_FUNCTIONS             , NUM_DRIV   ,         &
            SETTLING_VELOCITIES           , DISSOLVED_FRACTIONS, &
            BOTTOM_FACTOR                 , CELLNO, PSTIME     , &
            SETTLING_RATES                , FLUXES, NUM_FLUXES    , &
            SEDIMENT_TYPE                 , FRACTION_OF_DEPOSITION,  &
            NOT_DEPOSITED_FLUXES          , NUM_NOT_DEPOSITED_FLUXES, &
            CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES)

    ! Outputs:
    !  SETTLING_RATES       - potential flux by settling, g/m2/day
    !  FLUXES               - after substraction of not deposited fluxes for each BS state variable in g/m2/day
    !  NOT_DEPOSITED_FLUXES - for each WC state variable, g/m2/day

    use AQUABC_II_GLOBAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
	use AQUABC_PEL_STATE_VAR_INDEXES
    use GLOBAL, only: nstate, NUM_FLUXES_TO_SEDIMENTS

    implicit none

    integer, intent(in) :: CONSIDER_NON_OBLIGATORY_FIXERS
    integer, intent(in) :: CONSIDER_NOSTOCALES

    integer, intent(in) :: nkn
    integer, intent(in) :: NUM_VARS
    integer, intent(in) :: NUM_CONSTS
    integer, intent(in) :: NUM_FLUXES
    integer, intent(in) :: NUM_DRIV

    integer :: DO_NON_OBLIGATORY_FIXERS
    integer :: DO_NOSTOCALES

    double precision, intent(in) :: STATE_VARIABLES(nkn, NUM_VARS)
    double precision, intent(in) :: MODEL_CONSTANTS(NUM_CONSTS)

    double precision, intent(in) :: SETTLING_VELOCITIES(nkn, NUM_VARS)
    double precision, intent(in) :: DISSOLVED_FRACTIONS(nkn, NUM_VARS)

    double precision, intent(out) :: SETTLING_RATES(nkn, NUM_VARS)
    double precision, intent(out) :: FLUXES(nkn, NUM_FLUXES)

    double precision :: FLUXES_FROM_WC(nkn, NUM_VARS)

    double precision, intent(in) :: DRIVING_FUNCTIONS(nkn, NUM_DRIV)

    integer, intent(in) :: CELLNO
    integer :: LAYER

    double precision, intent(in) :: PSTIME
    double precision, intent(in) :: BOTTOM_FACTOR

    integer, intent(in) :: SEDIMENT_TYPE
    integer, intent(in) :: NUM_NOT_DEPOSITED_FLUXES
    double precision, intent(in)  :: FRACTION_OF_DEPOSITION(nkn, NUM_NOT_DEPOSITED_FLUXES)
    double precision, intent(out) :: NOT_DEPOSITED_FLUXES(nkn, NUM_NOT_DEPOSITED_FLUXES)

    double precision :: SETTLING_FACTORS(nkn, NUM_VARS)

    integer I

    DO_NON_OBLIGATORY_FIXERS          = 0
    DO_NOSTOCALES                     = 1

    if (CONSIDER_NON_OBLIGATORY_FIXERS > 0) then
        DO_NON_OBLIGATORY_FIXERS = 1
    else
        DO_NON_OBLIGATORY_FIXERS = 0
    end if

    if (CONSIDER_NOSTOCALES > 0) then
        DO_NOSTOCALES = 1
    else
        DO_NOSTOCALES = 0
    end if

    if(num_vars .ne. nstate) then
        print *, 'FLX_ALUKAS_II_TO_SED_MOD_1_VEC:'
        print *, 'To get values correctly by fluxes from WC and not deposited fluxes'
        print *, 'number of state variables should be equal to', nstate, 'but is ', NUM_VARS
        stop
    end if

    ! FLUXES FROM WC TO BS FOR BS VARIABLES
    if(NUM_FLUXES .ne. NUM_FLUXES_TO_SEDIMENTS) then
        print *, 'FLX_ALUKAS_II_TO_SED_MOD_1_VEC:'
        print *, 'To get values correctly by fluxes to sediments and not deposited fluxes'
        print *, 'number of BS state vars(fluxes) should be equal to', NUM_FLUXES_TO_SEDIMENTS, &
                 'but is ', NUM_FLUXES
        stop
    end if

    SETTLING_FACTORS = BOTTOM_FACTOR * (1.0D+0 - DISSOLVED_FRACTIONS) * SETTLING_VELOCITIES

    where (SETTLING_FACTORS.lt.0.0D0)
        SETTLING_FACTORS = 0.0D0
    end where

    SETTLING_RATES       = STATE_VARIABLES *  (1.0D+0 - DISSOLVED_FRACTIONS) * SETTLING_VELOCITIES
    FLUXES_FROM_WC       = STATE_VARIABLES * SETTLING_FACTORS
    NOT_DEPOSITED_FLUXES = FLUXES_FROM_WC  * (1.0D0 - FRACTION_OF_DEPOSITION)

    !AMMONIA NITROGEN FLUX
    FLUXES(:, 1) = FLUXES_FROM_WC(:, NH4_N_INDEX) * FRACTION_OF_DEPOSITION(:, NH4_N_INDEX)

    !NITRATE NITROGEN FLUX
    FLUXES(:, 2) = FLUXES_FROM_WC(:, NO3_N_INDEX) * FRACTION_OF_DEPOSITION(:, NO3_N_INDEX)

    !DISSOLVED ORGANIC NITROGEN FLUX
    FLUXES(:, 3) = FLUXES_FROM_WC(:, DISS_ORG_N_INDEX) * FRACTION_OF_DEPOSITION(:, DISS_ORG_N_INDEX)

    !PARTICULATE ORGANIC NITROGEN FLUX
    FLUXES(:, 4) = &
       (FLUXES_FROM_WC(:, DIA_C_INDEX)          * FRACTION_OF_DEPOSITION(:, DIA_C_INDEX) * DIA_N_TO_C) + &
       (FLUXES_FROM_WC(:, CYN_C_INDEX)          * FRACTION_OF_DEPOSITION(:, CYN_C_INDEX) * CYN_N_TO_C) + &
       (FLUXES_FROM_WC(:, OPA_C_INDEX)          * FRACTION_OF_DEPOSITION(:, OPA_C_INDEX) * OPA_N_TO_C) + &
       (FLUXES_FROM_WC(:, ZOO_N_INDEX)          * FRACTION_OF_DEPOSITION(:, ZOO_N_INDEX)             ) + &
       (FLUXES_FROM_WC(:, DET_PART_ORG_N_INDEX) * FRACTION_OF_DEPOSITION(:, DET_PART_ORG_N_INDEX)    )

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
       FLUXES(:, 4) = &
           FLUXES(:, 4) + &
           (FLUXES_FROM_WC(:, FIX_CYN_C_INDEX) * &
            FRACTION_OF_DEPOSITION(:, FIX_CYN_C_INDEX)      * FIX_CYN_N_TO_C)
    end if

    if (DO_NOSTOCALES > 0) then
       FLUXES(:, 4) = &
           FLUXES(:, 4) + &
           (FLUXES_FROM_WC(:, NOST_VEG_HET_C_INDEX) * &
            FRACTION_OF_DEPOSITION(:, NOST_VEG_HET_C_INDEX) * NOST_N_TO_C)
    end if

    !PHOSPHATE FLUX
    FLUXES(:, 5) = FLUXES_FROM_WC(:, PO4_P_INDEX) * FRACTION_OF_DEPOSITION(:, PO4_P_INDEX)

    !DISSOLVED ORGANIC PHOSPHORUS FLUX
    FLUXES(:, 6) = FLUXES_FROM_WC(:, DISS_ORG_P_INDEX) * FRACTION_OF_DEPOSITION(:, DISS_ORG_P_INDEX)

    !PARTICULATE ORGANIC PHOSPHORUS FLUX
    FLUXES(:, 7) = &
       (FLUXES_FROM_WC(:, DIA_C_INDEX)          * FRACTION_OF_DEPOSITION(:, DIA_C_INDEX) * DIA_P_TO_C)  + &
       (FLUXES_FROM_WC(:, CYN_C_INDEX)          * FRACTION_OF_DEPOSITION(:, CYN_C_INDEX) * CYN_P_TO_C)  + &
       (FLUXES_FROM_WC(:, OPA_C_INDEX)          * FRACTION_OF_DEPOSITION(:, OPA_C_INDEX) * OPA_P_TO_C)  + &
       (FLUXES_FROM_WC(:, ZOO_P_INDEX)          * FRACTION_OF_DEPOSITION(:, ZOO_P_INDEX)             )  + &
       (FLUXES_FROM_WC(:, DET_PART_ORG_P_INDEX) * FRACTION_OF_DEPOSITION(:, DET_PART_ORG_P_INDEX)    )

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
       FLUXES(:, 7) = &
           FLUXES(:, 7) + &
           (FLUXES_FROM_WC(:, FIX_CYN_C_INDEX) * &
            FRACTION_OF_DEPOSITION(:, FIX_CYN_C_INDEX)      * FIX_CYN_P_TO_C)
    end if

    if (DO_NOSTOCALES > 0) then
       FLUXES(:, 7) = &
           FLUXES(:, 7) + &
           (FLUXES_FROM_WC(:, NOST_VEG_HET_C_INDEX) * &
            FRACTION_OF_DEPOSITION(:, NOST_VEG_HET_C_INDEX) * NOST_P_TO_C)
    end if

    !DISSOLVED OXYGEN
    FLUXES(:, 8) = 0.0D0

    !DISSOLVED ORGANIC CARBON
    FLUXES(:, 9) = FLUXES_FROM_WC(:, DISS_ORG_C_INDEX) * FRACTION_OF_DEPOSITION(:, DISS_ORG_C_INDEX)

    !PARTICULATE ORGANIC CARBON FLUX
    FLUXES(:, 10) = &
       (FLUXES_FROM_WC(:, DIA_C_INDEX)          * FRACTION_OF_DEPOSITION(:, DIA_C_INDEX)         ) + &
       (FLUXES_FROM_WC(:, CYN_C_INDEX)          * FRACTION_OF_DEPOSITION(:, CYN_C_INDEX)         ) + &
       (FLUXES_FROM_WC(:, OPA_C_INDEX)          * FRACTION_OF_DEPOSITION(:, OPA_C_INDEX)         ) + &
       (FLUXES_FROM_WC(:, ZOO_C_INDEX)          * FRACTION_OF_DEPOSITION(:, ZOO_C_INDEX)         ) + &
       (FLUXES_FROM_WC(:, DET_PART_ORG_C_INDEX) * FRACTION_OF_DEPOSITION(:, DET_PART_ORG_C_INDEX))

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
       FLUXES(:, 10) = &
           FLUXES(:, 10) + &
           (FLUXES_FROM_WC(:, FIX_CYN_C_INDEX)      * FRACTION_OF_DEPOSITION(:, FIX_CYN_C_INDEX))
    end if

    if (DO_NOSTOCALES > 0) then
       FLUXES(:, 10) = &
           FLUXES(:, 10) + &
           (FLUXES_FROM_WC(:, NOST_VEG_HET_C_INDEX) * &
            FRACTION_OF_DEPOSITION(:, NOST_VEG_HET_C_INDEX))
    end if

    !DISSOLVED SILICON
    FLUXES(:, 11) = FLUXES_FROM_WC(:, DISS_Si_INDEX)   * FRACTION_OF_DEPOSITION(:, DISS_Si_INDEX)

    !PARTICULATE  SILICON FLUX
    FLUXES(:, 12) = &
       (FLUXES_FROM_WC(:, DIA_C_INDEX)   * FRACTION_OF_DEPOSITION(:, DIA_C_INDEX) * DIA_Si_TO_C) + &
       (FLUXES_FROM_WC(:, PART_Si_INDEX) * FRACTION_OF_DEPOSITION(:, PART_Si_INDEX))

    FLUXES(:, 13) = FLUXES_FROM_WC(:, INORG_C_INDEX) * FRACTION_OF_DEPOSITION(:, INORG_C_INDEX) !INORGANIC CARBON FLUX
    FLUXES(:, 14) = FLUXES_FROM_WC(:, TOT_ALK_INDEX) * FRACTION_OF_DEPOSITION(:, TOT_ALK_INDEX) !ALKALINITY FLUX
    FLUXES(:, 15) = 0.D0 !Salinity flux

    FLUXES(:, 16) =  FLUXES_FROM_WC(:, FE_II_INDEX    ) *  FRACTION_OF_DEPOSITION(:, FE_II_INDEX    ) !FEII  FLUX
    FLUXES(:, 17) =  FLUXES_FROM_WC(:, FE_III_INDEX   ) *  FRACTION_OF_DEPOSITION(:, FE_III_INDEX   ) !FEIII FLUX
    FLUXES(:, 18) =  FLUXES_FROM_WC(:, MN_II_INDEX    ) *  FRACTION_OF_DEPOSITION(:, MN_II_INDEX    ) !MNII  FLUX
    FLUXES(:, 19) =  FLUXES_FROM_WC(:, MN_IV_INDEX    ) *  FRACTION_OF_DEPOSITION(:, MN_IV_INDEX    ) !MNIV  FLUX
    FLUXES(:, 20) =  FLUXES_FROM_WC(:, CA_INDEX       ) *  FRACTION_OF_DEPOSITION(:, CA_INDEX       ) !CA    FLUX
    FLUXES(:, 21) =  FLUXES_FROM_WC(:, MG_INDEX       ) *  FRACTION_OF_DEPOSITION(:, MG_INDEX       ) !MG    FLUX
    FLUXES(:, 22) =  FLUXES_FROM_WC(:, S_PLUS_6_INDEX ) *  FRACTION_OF_DEPOSITION(:, S_PLUS_6_INDEX ) !S_PLUS_6  FLUX
    FLUXES(:, 23) =  FLUXES_FROM_WC(:, S_MINUS_2_INDEX) *  FRACTION_OF_DEPOSITION(:, S_MINUS_2_INDEX) !S_MINUS_2 FLUX
    FLUXES(:, 24) =  FLUXES_FROM_WC(:, CH4_C_INDEX    ) *  FRACTION_OF_DEPOSITION(:, CH4_C_INDEX    ) !CH4_C     FLUX
end subroutine FLX_ALUKAS_II_TO_SED_MOD_1_VEC

!********************************************************************
!********************************************************************

integer function STRANGERSD(VALUE, VALUE_strange, nkn)

    ! Checks for NaN and Inf in 1D array with nkn elements
    ! Input is double precision
      use, intrinsic :: IEEE_ARITHMETIC
      use AQUABC_PHYSICAL_CONSTANTS, only: STRANGER_THRESHOLD

      implicit none

      integer, intent(in) :: nkn

      double precision, intent(in)  :: VALUE(nkn)
      logical :: VALUE_NaN(nkn)
      logical :: VALUE_Inf(nkn)
      logical, intent(out) :: VALUE_strange(nkn)

      integer :: i

      STRANGERSD = 0

      do i = 1, nkn
          VALUE_NaN(i) = IEEE_IS_NAN(VALUE(i))
          VALUE_Inf(i) = .not. IEEE_IS_FINITE(VALUE(i))
      end do

      VALUE_strange = VALUE_NaN .or. VALUE_Inf

      if(any(VALUE_strange)) then
          STRANGERSD = 1
      end if

end function STRANGERSD
!************************************************************************
!************************************************************************

subroutine chlorophyl_a &
           (state, nstate, chla, &
            CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES)

    ! Calculates total chlorophyl a for one node in mcg/l

    use para_aqua
    use aquabc_pel_state_var_indexes

    implicit none

    integer, intent(in) :: nstate
    double precision, intent(in)  :: state(nstate)
    double precision, intent(out) :: chla

    double precision DIA_C
    double precision CYN_C
    double precision OPA_C
    double precision FIX_CYN_C
    double precision NOST_VEG_HET_C

    double precision DIA_C_TO_CHLA
    double precision CYN_C_TO_CHLA
    double precision FIX_CYN_C_TO_CHLA
    double precision OPA_C_TO_CHLA
    double precision NOST_C_TO_CHLA

    integer, intent(in) :: CONSIDER_NON_OBLIGATORY_FIXERS
    integer, intent(in) :: CONSIDER_NOSTOCALES

    integer DO_NON_OBLIGATORY_FIXERS
    integer DO_NOSTOCALES

    if (CONSIDER_NON_OBLIGATORY_FIXERS > 0) then
        DO_NON_OBLIGATORY_FIXERS = 1
    else
        DO_NON_OBLIGATORY_FIXERS = 0
    end if

    if (CONSIDER_NOSTOCALES > 0) then
        DO_NOSTOCALES = 1
    else
        DO_NOSTOCALES = 0
    end if

    ! Ratios C to Chla
    call para_get_value('DIA_C_TO_CHLA'    ,DIA_C_TO_CHLA)
    call para_get_value('CYN_C_TO_CHLA'    ,CYN_C_TO_CHLA)
    call para_get_value('OPA_C_TO_CHLA'    ,OPA_C_TO_CHLA)

    DIA_C          = state(DIA_C_INDEX)
    CYN_C          = state(CYN_C_INDEX)
    OPA_C          = state(OPA_C_INDEX)

    ! Total chorophyl a in mcg/l
    CHLA = ((DIA_C     / DIA_C_TO_CHLA)     + &
            (CYN_C     / CYN_C_TO_CHLA)     + &
            (OPA_C     / OPA_C_TO_CHLA)) * 1.0D3

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        call para_get_value('FIX_CYN_C_TO_CHLA',FIX_CYN_C_TO_CHLA)
        FIX_CYN_C = state(FIX_CYN_C_INDEX)
        CHLA = CHLA + ((FIX_CYN_C / FIX_CYN_C_TO_CHLA) * 1.0D3)
    end if

    if (DO_NOSTOCALES > 0) then
        call para_get_value('NOST_C_TO_CHLA'   ,NOST_C_TO_CHLA)
        NOST_VEG_HET_C = state(NOST_VEG_HET_C_INDEX)
        CHLA = CHLA + ((NOST_VEG_HET_C / NOST_C_TO_CHLA) * 1.0D3)
    end if
end subroutine chlorophyl_a


subroutine settling_suppres_factor(chla, factor)

    ! Calculates settling velocity supprss factor do to high chlorophyl a
    ! concentrations
    ! Chla in mcg/l

    implicit none
    double precision, intent(in)  :: chla
    double precision, intent(out) :: factor
    double precision :: setl_vel, chlamin, chlamax, settl_max

    ! observed interval endpoints for chla impacting settling velocity
    ! We do not know what happens outside of this interval and assume
    ! the same factor values as on endpoints there

    chlamin =  44.0D0
    chlamax = 140.0D0

    settl_max = settl_vel(chlamin)

    if(chla .lt. chlamin) then
        factor = 1.0d0
    else if(chla .ge. chlamin .and. chla .le. chlamax) then
        factor = settl_vel(chla)/settl_max
    else
       factor = settl_vel(chlamax)/settl_max
    end if

contains

    double precision function settl_vel(chla)
        double precision, intent(in) :: chla
        settl_vel = -(0.0061D0 * chla) + 1.0383
    end function settl_vel

end subroutine settling_suppres_factor
