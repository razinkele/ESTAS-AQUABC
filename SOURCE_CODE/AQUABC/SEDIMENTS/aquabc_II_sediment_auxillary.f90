!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Main routines for bottom sediment model No1
! CONTENT:
!  FUNCTION STRANGER(VALUE)      -  checks for NaNs (input type is double precision), not vectorised
!  subroutine FLX_ALUKAS_II_TO_SED_MOD_1
!  FUNCTION SED_MOD_1_ALUKAS_MOLDI_C - molecular diffusion coefficients
!  subroutine SED_MOD_1_CVISC
!  subroutine FLX_SED_MOD_1_TO_ALUKAS_II
!  subroutine CALCULATE_PH_CORR_SED

!*******************************************************************
!*******************************************************************
INTEGER FUNCTION STRANGER(VALUE)
    ! cheks for NaN and Inf in double precision

    DOUBLE PRECISION VALUE, BIGNUMBER, RATIO, LLIMIT, ULIMIT
    LLIMIT    = -1.0D4
    ULIMIT    =  1.0D4
    BIGNUMBER = 1.0D300
    RATIO     = 1.0D0
    STRANGER  = 0

    if (isnan(VALUE)) then
        STRANGER=1
    end if

    if (isnan(VALUE)) then
        STRANGER=1
    end if

    if(RATIO .eq. 0.D0) then
        STRANGER=1
    end if

    if(VALUE .le. LLIMIT .or. VALUE .ge. ULIMIT) then
        STRANGER=1
    end if

    return
end
!************************************************************************
!************************************************************************




!THIS IS A USER PROGRAMMED FUNCTION. IT IS THE USERS RESPONSIBILITY
!TO PROGRAM THE EQUATIONS WHICH GIVE THE MOLECULAR DIFFUSION COEFFICIENTS
!OF STATE VARIABLES IN WATER AS A FUNCTION OF TEMPERATURE AND SALINITY
function SED_MOD_1_ALUKAS_MOLDI_C(SVARNO, T, SAL, TVAR) result(MOL_DIFF)
    implicit none
    !INPUTS:
    !SVARNO : State variable no
    !T      : Temperature in Celsius
    !SAL    : Salinity (ppt)
    !TVAR   : Generic temporal variable
    !OTPUT:
    !MOL_DIFF : Molecular diffusion coefficient, units cm^2/sec inside
    !           and converted to m^2/s for the output

    integer SVARNO
    double precision T
    double precision SAL
    double precision TVAR

    double precision TS
    double precision SS
    double precision V25
    double precision ONE
    double precision VTK
    double precision ZERO
    double precision D
    double precision TK

    double precision MOL_DIFF

    if(SVARNO .gt. 24) then
        print *,'SED_MOD_1_ALUKAS_MOLDI_C:'
        print *,'To get values correctly by Molecular diffusion'
        print *,'BS statevars No should not gt than 24 but is', SVARNO
        stop
    end if

    TK = T + 273.16

    ! Initialize reference conditions for Stokes-Einstein scaling
    TS = 25.0D0
    SS = 36.1D0

    ! Shear viscosities
    call SED_MOD_1_CVISC(V25, SS , TS, 1.0D0)
    call SED_MOD_1_CVISC(VTK, SAL, T , 1.0D0)

    !NH4N
    !Boudreau 1997, Springer-Verlag
    if (SVARNO.eq.1) then
        MOL_DIFF = (9.5D0 + 4.13D-1 * T) * 1.0D-6
    end if

    !NO3N
    !Boudreau 1997, Springer-Verlag
    if (SVARNO.eq.2) then
        MOL_DIFF = (9.5D0 + 3.88D-1 * T) * 1.0D-6
    end if

    !Dissolved organic nitrogen
    if (SVARNO.eq.3) then
        MOL_DIFF = 1.0D-6
    end if

    !Particulate organic nitrogen
    if (SVARNO.eq.4) then
        MOL_DIFF = 0.0D0
    end if

    !PO4P
    !Boudreau 1997, Springer-Verlag
    if (SVARNO.eq.5) then
        MOL_DIFF = (2.62D0 + 1.43D-1 * T) * 1.0D-6
    end if

    !Dissolved organic phosphorus
    if (SVARNO.eq.6) then
        MOL_DIFF = 1.0D-6
    end if

    !Particulate organic phosphorus
    if (SVARNO.eq.7) then
        MOL_DIFF = 0.0D0
    end if

    !DOXY
    !Reference : Fossing et al., 2004 (NERI Technical report)
    if (SVARNO.eq.8) then
        MOL_DIFF = (1.17D1 + (3.44D-1 * T) + &
                  (5.05D-3 * (T ** 2.0D0))) * 1.0D-6
    end if

    !Dissolved organic carbon
    if (SVARNO.eq.9) then
         MOL_DIFF = 1.0D-6
    end if

    !Particulate organic carbon
    if (SVARNO.eq.10) then
        MOL_DIFF = 0.0D0
    end if

    !DSi
    !From Boudreau 1997, Springer-Verlag
    !Wollast and Garrels (1971) found D(H4SiO4) at 25 deg C
    !and 36.1 ppt S., Assume that this value can be scaled by
    !the Stokes-Einstein relationship to any other temperature.
    if (SVARNO.eq.11) then
        MOL_DIFF = 1.0D-5
        MOL_DIFF = MOL_DIFF * (V25 / 298.16D0) * (TK / VTK)
    end if

    !Particulate silicon
    if (SVARNO.eq.12) then
        MOL_DIFF = 0.0D0
    end if

    !Dissolved inorganic carbon (to be corrected with better formulation)
    if (SVARNO.eq.13) then
        MOL_DIFF = 1.0D-6
    end if

    !Alkalinity (to be corrected with better formulation)  !fixme
    if (SVARNO.eq.14) then
        MOL_DIFF = 1.0D-6
    end if

    !Salinity (to be corrected with better formulation)  !fixme
    if (SVARNO.eq.15) then
        MOL_DIFF = 1.0D-6
    end if

    !FEII
    if (SVARNO.eq.16) then
        MOL_DIFF = (3.31D0 + 0.15D0 * T) * 1.0D-5
    end if

    !FEIII
    if (SVARNO.eq.17) then
        MOL_DIFF = 1.0D-6
    end if

    !MNII
    if (SVARNO.eq.18) then
        MOL_DIFF = (3.18D0 + 0.1553D0 * T) * 1.0D-5
    end if


    !MNIV
    if (SVARNO.eq.19) then
        MOL_DIFF = 1.0D-6
    end if


    !Ca
    if (SVARNO.eq.20) then
        MOL_DIFF = (3.60D0 + 0.179D0 * T) * 1.0D-5
    end if

    !Mg
    if (SVARNO.eq.21) then
        MOL_DIFF = (3.43D0 + 0.144D0 * T) * 1.0D-5
    end if

    !S_PLUS_6
    if (SVARNO.eq.22) then
        MOL_DIFF = 4.72D-9 * TK/(35.2D0**0.6) * (V25/VTK)
    end if

    !S_MINUS_2
    if (SVARNO.eq.23) then
       MOL_DIFF = (4.88D0 + 0.232D0 * T) * 1.0D-5
    end if

    !CH4_C
    if (SVARNO.eq.24) then
        MOL_DIFF = 5.7524D-3 * exp(-(3300D0 + 8.94104D-4*(TK-228)**1.5)/(1.9858775*TK))
    end if

    ! Converting to m^2/sec
    MOL_DIFF = MOL_DIFF * 1.0D-4

end function SED_MOD_1_ALUKAS_MOLDI_C

!***********************************************************************
!***********************************************************************


subroutine SED_MOD_1_CVISC(V,S,T,P)
    implicit none

    !Calculates the shear viscosity of water using the equation
    !given by Kukulka et al. (1987).
    !Calculated viscosity is in centipoise.
    !
    !Valid for 0<T<30 and 0<S<36.

    double precision V
    double precision S
    double precision T
    double precision P

    V = 1.7910 - T * (6.144D-02 - T*(1.4510D-03 - T*1.6826D-05)) - &
        1.5290D-04 * P + 8.3885D-08 * P * P + 2.4727D-03 * S + &
        (6.0574D-06*P - 2.6760D-09*P*P)*T + &
        (T * (4.8429D-05 - T * (4.7172D-06 - T * 7.5986D-08))) * S

end subroutine SED_MOD_1_CVISC

!*******************************************************************
!*******************************************************************


subroutine FLX_SED_MOD_1_TO_ALUKAS_II &
           (FLUXES_FROM_SEDIMENT, NUM_FLUXES_FROM_SEDIMENT, &
            FLUXES_TO_ALUKAS    , NUM_FLUXES_TO_ALUKAS)

    ! Note: routine does not take into account particulate material resuspension
    ! as a flux yet. Everytthing what is resuspended is added to the same fluxes. fixme

    implicit none

    integer :: NUM_FLUXES_FROM_SEDIMENT
    integer :: NUM_FLUXES_TO_ALUKAS

    double precision :: FLUXES_FROM_SEDIMENT(NUM_FLUXES_FROM_SEDIMENT)
    double precision :: FLUXES_TO_ALUKAS    (NUM_FLUXES_TO_ALUKAS)

    if(NUM_FLUXES_TO_ALUKAS .ne. 30) then
        print *,'FLX_SED_MOD_1_TO_ALUKAS_II:'
        print *,'To get values correctly by FLUXES_TO_ALUKAS'
        print *,'num. of statevars should be 33 but is', NUM_FLUXES_TO_ALUKAS
        stop
    end if

    !NH4 FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(1) = FLUXES_FROM_SEDIMENT(1)

    !NO3 FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(2) = FLUXES_FROM_SEDIMENT(2)

    !PO4 FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(3) = FLUXES_FROM_SEDIMENT(5)

    !DISS_OXYGEN FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(4) = FLUXES_FROM_SEDIMENT(8)

    !DIA_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(5) = 0.0D0

    !ZOO_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(6) = 0.0D0

    !ZOO_N FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(7) = 0.0D0

    !ZOO_P FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(8) = 0.0D0

    !DET_PART_ORG_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(9) = 0.0D0

    !DET_PART_ORG_N FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(10) = 0.0D0

    !DET_PART_ORG_P FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(11) = 0.0D0

    !DISS_ORG_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(12) = FLUXES_FROM_SEDIMENT(9)

    !DISS_ORG_N FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(13) = FLUXES_FROM_SEDIMENT(3)

    !DISS_ORG_P FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(14) = FLUXES_FROM_SEDIMENT(6)

    !CYN_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(15) = 0.0D0

    !OPA_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(16) = 0.0D0

    !DISS_Si FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(17) = FLUXES_FROM_SEDIMENT(11)

    !PART_Si FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(18) = FLUXES_FROM_SEDIMENT(12)

    !FIX_CYN_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(19) = 0.0D0

    !INORG_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(20) = FLUXES_FROM_SEDIMENT(13)

    !INORG_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(21) = FLUXES_FROM_SEDIMENT(14)

    ! Fluxes to metals in WC
    FLUXES_TO_ALUKAS(22) = FLUXES_FROM_SEDIMENT(16)
    FLUXES_TO_ALUKAS(23) = FLUXES_FROM_SEDIMENT(17)
    FLUXES_TO_ALUKAS(24) = FLUXES_FROM_SEDIMENT(18)
    FLUXES_TO_ALUKAS(25) = FLUXES_FROM_SEDIMENT(19)

    FLUXES_TO_ALUKAS(26) = FLUXES_FROM_SEDIMENT(20)
    FLUXES_TO_ALUKAS(27) = FLUXES_FROM_SEDIMENT(21)
    FLUXES_TO_ALUKAS(28) = FLUXES_FROM_SEDIMENT(22)
    FLUXES_TO_ALUKAS(29) = FLUXES_FROM_SEDIMENT(23)
    FLUXES_TO_ALUKAS(30) = FLUXES_FROM_SEDIMENT(24)
    end subroutine FLX_SED_MOD_1_TO_ALUKAS_II

!************************************************************
!************************************************************


subroutine FLX_SED_MOD_1_TO_ALUKAS_II_VEC &
           (FLUXES_FROM_SEDIMENT, NUM_FLUXES_FROM_SEDIMENT, &
            FLUXES_TO_ALUKAS    , nkn, NUM_FLUXES_TO_ALUKAS)

    ! Note: routine does not take into account particulate material resuspension
    ! as a flux yet. Everytthing what is resuspended is added to the same fluxes. fixme

    implicit none

    integer :: nkn
    integer :: NUM_FLUXES_FROM_SEDIMENT
    integer :: NUM_FLUXES_TO_ALUKAS

    double precision :: FLUXES_FROM_SEDIMENT(nkn, NUM_FLUXES_FROM_SEDIMENT)
    double precision :: FLUXES_TO_ALUKAS    (nkn, NUM_FLUXES_TO_ALUKAS)

    if(NUM_FLUXES_TO_ALUKAS .ne. 32) then
        print *,'FLX_SED_MOD_1_TO_ALUKAS_II:'
        print *,'To get values correctly by FLUXES_TO_ALUKAS'
        print *,'num. of statevars should be 30 but is', NUM_FLUXES_TO_ALUKAS
        stop
    end if

    !NH4 FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 1) = FLUXES_FROM_SEDIMENT(:, 1)

    !NO3 FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 2) = FLUXES_FROM_SEDIMENT(:, 2)

    !PO4 FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 3) = FLUXES_FROM_SEDIMENT(:, 5)

    !DISS_OXYGEN FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 4) = FLUXES_FROM_SEDIMENT(:, 8)

    !DIA_C FLUX TO ALUKAS
    !ZOO_C FLUX TO ALUKAS
    !ZOO_N FLUX TO ALUKAS
    !ZOO_P FLUX TO ALUKAS
    !DET_PART_ORG_C FLUX TO ALUKAS
    !DET_PART_ORG_N FLUX TO ALUKAS
    !DET_PART_ORG_P FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 5:11) = 0.0D0

    !DISS_ORG_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 12) = FLUXES_FROM_SEDIMENT(:, 9)

    !DISS_ORG_N FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 13) = FLUXES_FROM_SEDIMENT(:, 3)

    !DISS_ORG_P FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 14) = FLUXES_FROM_SEDIMENT(:, 6)

    !CYN_C FLUX TO ALUKAS
    !OPA_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 15:16) = 0.0D0

    !DISS_Si FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 17) = FLUXES_FROM_SEDIMENT(:, 11)

    !PART_Si FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 18) = FLUXES_FROM_SEDIMENT(:, 12)

    !FIX_CYN_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 19) = 0.0D0

    !INORG_C FLUX TO ALUKAS
    FLUXES_TO_ALUKAS(:, 20) = FLUXES_FROM_SEDIMENT(:, 13)
    FLUXES_TO_ALUKAS(:, 21) = FLUXES_FROM_SEDIMENT(:, 14)

    !Metal fluxes to ALUKAS
    FLUXES_TO_ALUKAS(:,22:30) = FLUXES_FROM_SEDIMENT(:,16:24)

    !Nostocales flux to ALUKAS
    FLUXES_TO_ALUKAS(:,31:32) = 0.0D0
end subroutine FLX_SED_MOD_1_TO_ALUKAS_II_VEC



!THIS IS A USER PROGRAMMED FUNCTION. IT IS THE USERS RESPONSIBILITY
!TO PROGRAM THE EQUATIONS WHICH GIVE THE MOLECULAR DIFFUSION COEFFICIENTS
!OF STATE VARIABLES IN WATER AS A FUNCTION OF TEMPERATURE AND SALINITY
subroutine SED_MOD_1_ALUKAS_MOLDI_C_VEC(T,  SAL, nkn, NUM_SED_LAYERS, NUM_SED_VARS, MOL_DIFF)
    implicit none
    !INPUTS:
    !SVARNO : State variable no
    !T      : Temperature in Celsius
    !SAL    : Salinity (ppt)
    !OTPUT:
    !MOL_DIFF : Molecular diffusion coefficient, units cm^2/sec inside
    !           and converted to m^2/s for the output

    integer nkn, NUM_SED_LAYERS, NUM_SED_VARS

    double precision T       (nkn, NUM_SED_LAYERS)
    double precision SAL     (nkn, NUM_SED_LAYERS)
    double precision MOL_DIFF(nkn, NUM_SED_LAYERS, NUM_SED_VARS)

    double precision TS   !Standard temperature
    double precision SS   !Standard salinity
    double precision P    !Pressure

    double precision V25(nkn, NUM_SED_LAYERS)
    double precision VTK(nkn, NUM_SED_LAYERS)
    double precision TK(nkn, NUM_SED_LAYERS)

    if(NUM_SED_VARS .ne. 24) then
        print *,'SED_MOD_1_ALUKAS_MOLDI_C:'
        print *,'To get values correctly by Molecular diffusion'
        print *,'BS statevars No should not gt than 24 but is', NUM_SED_VARS
        stop
    end if

    TK = T + 273.16
    TS = 25.0D0
    SS = 36.1D0
    P  =  1.0D0

    ! Shear viscosities
    !call SED_MOD_1_CVISC(V25, SS , TS, 1.0D0)
    !call SED_MOD_1_CVISC(VTK, SAL, T , 1.0D0)

    V25 = 1.7910 - TS * (6.144D-02 - TS*(1.4510D-03 - TS*1.6826D-05)) - &
          1.5290D-04 * P + 8.3885D-08 * P * P + 2.4727D-03 * SS + &
          (6.0574D-06*P - 2.6760D-09*P*P)*TS + (TS * (4.8429D-05 - &
          TS * (4.7172D-06 - TS * 7.5986D-08))) * SS

    VTK = 1.7910 - T * (6.144D-02 - T*(1.4510D-03 - T*1.6826D-05)) - &
          1.5290D-04 * P + 8.3885D-08 * P * P + 2.4727D-03 * SAL + &
          (6.0574D-06*P - 2.6760D-09*P*P)*T + (T * (4.8429D-05 - &
          T * (4.7172D-06 - T * 7.5986D-08))) * SAL

    MOL_DIFF(:,:,1) = (9.5D0 + 4.13D-1 * T(:,:)) * 1.0D-6    !NH4N: Boudreau 1997, Springer-Verlag
    MOL_DIFF(:,:,2) = (9.5D0 + 3.88D-1 * T(:,:)) * 1.0D-6    !NO3N: Boudreau 1997, Springer-Verlag
    MOL_DIFF(:,:,3) = 1.0D-6                                 !Dissolved organic nitrogen (assumed value)
    MOL_DIFF(:,:,4) = 0.0D0                                  !Particulate organic nitrogen (not subjected to molecular diffusion)
    MOL_DIFF(:,:,5) = (2.62D0 + 1.43D-1 * T(:,:)) * 1.0D-6   !PO4P: Boudreau 1997, Springer-Verlag
    MOL_DIFF(:,:,6) = 1.0D-6                                 !Dissolved organic phosphorus (assumed value)
    MOL_DIFF(:,:,7) = 0.0D0                                  !Particulate organic phosphorus (not subjected to molecular diffusion)

    !DOXY: Fossing et al., 2004 (NERI Technical report)
    MOL_DIFF(:,:,8) = (1.17D1 + (3.44D-1 * T(:,:)) + (5.05D-3 * (T(:,:) ** 2.0D0))) * 1.0D-6

    MOL_DIFF(:,:,9)  = 1.0D-6                                !Dissolved organic carbon (assumed value)
    MOL_DIFF(:,:,10) = 0.0D0                                 !Particulate organic carbon (not subjected to molecular diffusion)

    !DSi
    !From Boudreau 1997, Springer-Verlag
    !Wollast and Garrels (1971) found D(H4SiO4) at 25 deg C
    !and 36.1 ppt S., Assume that this value can be scaled by
    !the Stokes-Einstein relationship to any other temperature.
    MOL_DIFF(:,:,11) = 1.0D-5 * (V25(:,:) / 298.16D0) * (TK(:,:) / VTK(:,:))

    !Particulate organic silicon (not subjected to molecular diffusion)
    MOL_DIFF(:,:,12) = 0.0D0

    !Dissolved inorganic carbon, alkalinity, salinity (assumed value 1.0D-6 to be corrected with better formulation)
    MOL_DIFF(:,:,13:15) = 1.0D-6

    MOL_DIFF(:,:,16) = (3.31D0 + 0.1500D0 * T(:,:)) * 1.0D-5     !FEII
    MOL_DIFF(:,:,17) = 1.0D-6                                    !FEIII
    MOL_DIFF(:,:,18) = (3.18D0 + 0.1553D0 * T(:,:)) * 1.0D-5     !MNII
    MOL_DIFF(:,:,19) = 1.0D-6                                    !MN_IV

    MOL_DIFF(:,:,20) = (3.60D0 + 0.179D0 * T(:,:)) * 1.0D-5    !Ca
    MOL_DIFF(:,:,21) = (3.43D0 + 0.144D0 * T(:,:)) * 1.0D-5    !Mg
    MOL_DIFF(:,:,22) = 4.72D-9 * TK(:,:)/(35.2D0**0.6) * (V25(:,:)/VTK(:,:))    !S_PLUS_6
    MOL_DIFF(:,:,23) = (4.88D0 + 0.232D0 * T(:,:)) * 1.0D-5    !S_MINUS_2

    !CH4_C
    MOL_DIFF(:,:,24) = 5.7524D-3 * exp(-(3300D0 + 8.94104D-4*(TK(:,:)-228D0)**1.5D0)/(1.9858775D0*TK(:,:)))

    ! Converting to m^2/sec
    MOL_DIFF = MOL_DIFF * 1.0D-4

end subroutine SED_MOD_1_ALUKAS_MOLDI_C_VEC



!************************************************************************
!************************************************************************
