! Subset of aquabc_II_pelagic_auxillary.f90 for unit testing.
! Contains only the standalone subroutines needed by kinetics tests
! (DIATOMS, CYANOBACTERIA, ZOOPLANKTON), avoiding the deep dependency
! chain through the GLOBAL module used by FLX_ALUKAS_II_TO_SED_MOD_1.

!---------------------------------------------------------------------------
! GROWTH_AT_TEMP - Cardinal Temperature Model with Inflection (CTMI)
!---------------------------------------------------------------------------
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

    double precision :: T_min, T_opt, T_max
    double precision :: denom(nkn)

    T_min = Lower_TEMP
    T_opt = Upper_TEMP
    T_max = KAPPA_OVER_OPT_TEMP

    denom = (T_opt - T_min) * ((T_opt - T_min) * (TEMP - T_opt) - &
            (T_opt - T_max) * (T_opt + T_min - 2.0D0 * TEMP))

    where (TEMP <= T_min .or. TEMP >= T_max .or. abs(denom) < 1.0D-20)
        LIM_TEMP_GROWTH = 0.0D0
    elsewhere
        LIM_TEMP_GROWTH = (TEMP - T_max) * (TEMP - T_min)**2 / denom
    end where

    LIM_TEMP_GROWTH = max(0.0D0, min(1.0D0, LIM_TEMP_GROWTH))

end subroutine GROWTH_AT_TEMP


!---------------------------------------------------------------------------
! DUAL_NUTRIENT_PREFERENCE - WASP formulation
!---------------------------------------------------------------------------
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


!---------------------------------------------------------------------------
! AMMONIA_PREFS
!---------------------------------------------------------------------------
subroutine AMMONIA_PREFS(AMMONIA_PREF, NH3, NOx, kn, nkn)

    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: AMMONIA_PREF(nkn)
    double precision, intent(in)  :: NH3(nkn)
    double precision, intent(in)  :: NOx(nkn)
    double precision, intent(in)  :: kn

    call DUAL_NUTRIENT_PREFERENCE(AMMONIA_PREF, NH3, NOx, kn, nkn)

end subroutine AMMONIA_PREFS


!---------------------------------------------------------------------------
! DIN_DON_PREFS
!---------------------------------------------------------------------------
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


!---------------------------------------------------------------------------
! DIP_DOP_PREFS
!---------------------------------------------------------------------------
subroutine DIP_DOP_PREFS(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)

    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: DIP_DOP_PREF(nkn)
    double precision, intent(in)  :: AVAIL_DIP(nkn)
    double precision, intent(in)  :: AVAIL_DOP(nkn)
    double precision, intent(in)  :: KP

    call DUAL_NUTRIENT_PREFERENCE(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)

end subroutine DIP_DOP_PREFS


!---------------------------------------------------------------------------
! DOP_DIP_PREFS
!---------------------------------------------------------------------------
subroutine DOP_DIP_PREFS(DIP_DOP_PREF, AVAIL_DIP, AVAIL_DOP, KP, nkn)

    implicit none

    integer, intent(in) :: nkn

    double precision, intent(out) :: DIP_DOP_PREF(nkn)
    double precision, intent(in)  :: AVAIL_DIP(nkn)
    double precision, intent(in)  :: AVAIL_DOP(nkn)
    double precision, intent(in)  :: KP

    double precision :: PP(nkn)

    call DUAL_NUTRIENT_PREFERENCE(PP, AVAIL_DIP, AVAIL_DOP, KP, nkn)

    DIP_DOP_PREF = 1.D0 - PP

end subroutine DOP_DIP_PREFS


!---------------------------------------------------------------------------
! AMMONIA_DON_PREFS
!---------------------------------------------------------------------------
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


!---------------------------------------------------------------------------
! LIM_LIGHT - Depth-averaged Steele light limitation with photoinhibition
!---------------------------------------------------------------------------
subroutine LIM_LIGHT(Ia, TCHLA, GITMAX, H, ke, LLIGHT, CCHL_RATIO, K_LIGHT_SAT, LIGHT_SAT, nkn, BETA, &
                     FDAY)

    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use AQUABC_PHYSICAL_CONSTANTS, only: EULER_E, safe_exp
    use AQUABC_II_GLOBAL, only: LIGHT_DAYLENGTH_OPTION
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
    double precision, intent(in)  :: FDAY(nkn)

    double precision :: SKE(nkn)
    double precision :: TEMP1(nkn)
    double precision :: TEMP2(nkn)
    double precision :: TEMP3(nkn)
    double precision :: BETA_LOC
    double precision :: IA_EFF(nkn)
    double precision :: FD_W(nkn)

    logical VALUE_strange(nkn)
    integer STRANGERSD
    integer :: i

    integer user_defined_saturation

    user_defined_saturation = 0

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

    do i = 1, nkn
        if (.not. ieee_is_finite(TEMP1(i))) then
            write(6,*) 'LIM_LIGHT: NON-FINITE TEMP1 at i=', i, 'ke=', ke(i), 'H=', H(i), 'TEMP1=', TEMP1(i)
            stop
        end if
    end do

    do i = 1, nkn
        if (TEMP1(i) .lt. 1.0D-10) then
            TEMP1(i) = 1.0D-10
        end if
    end do

    if(user_defined_saturation .eq. 0) then
     do i = 1, nkn
         if (abs(GITMAX(i)) .lt. 1.0D-20) then
             TEMP2(i) = 1.0D0 / K_LIGHT_SAT
         else
             TEMP2(i) = (0.083D0 * PHIMX * XKC) / (GITMAX(i) * CCHL_RATIO * EULER_E)
         end if
     end do
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

    TEMP2 = TEMP2 * (1.0D0 + BETA_LOC)

    ! Day-length handling -- mirrors aquabc_II_pelagic_auxillary.f90 exactly.
    ! 0 = legacy (no FDAY), 1 = Form A (weight only), 2 = Form B (WASP).
    select case (LIGHT_DAYLENGTH_OPTION)
        case (1)
            IA_EFF = Ia
            FD_W   = max(1.0D-6, min(1.0D0, FDAY))
        case (2)
            FD_W   = max(1.0D-6, min(1.0D0, FDAY))
            IA_EFF = Ia / FD_W
        case default
            IA_EFF = Ia
            FD_W   = 1.0D0
    end select

    TEMP3  = safe_exp( - TEMP1)
    LLIGHT = FD_W * (EULER_E / TEMP1) * &
             (safe_exp( -TEMP2 * IA_EFF * TEMP3) - safe_exp( -TEMP2 * IA_EFF))

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


!---------------------------------------------------------------------------
! STRANGERSD - Checks for NaN and Inf in 1D array
!---------------------------------------------------------------------------
integer function STRANGERSD(VALUE, VALUE_strange, nkn)

    use, intrinsic :: IEEE_ARITHMETIC

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
