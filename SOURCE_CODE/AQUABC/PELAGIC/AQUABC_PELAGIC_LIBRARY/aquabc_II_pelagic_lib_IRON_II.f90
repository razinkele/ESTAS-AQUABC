! Auxilary routines for the pelagic model

! Contents:
!subroutine IRON_II_DISSOLUTION
!subroutine IRON_II_OXIDATION


! -----------------------------------------------------------------------------
! The subroutine below calculates the Iron II dissolution according to
! Stumm and Lee (1960).
! -----------------------------------------------------------------------------
!
!                       Initial development 2 nd of July 2016
!
!                                by Ali Ertürk
! -----------------------------------------------------------------------------
subroutine IRON_II_DISSOLUTION(HS2_TOT, PH, TOT_ALK, nkn, FE_II_TOT)
    use AQUABC_II_GLOBAL
    implicit none

    integer, intent(in) ::  nkn

    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: HS2_TOT       ! [H2S] + [HS-]  +  [S]   (mol/L)
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PH            ! PH
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: TOT_ALK       ! Total alkalinity (mol/L)

    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: FE_II_TOT     ! Total Fe2+ (mol/L)


    real(kind = DBL_PREC), dimension(nkn) :: K_1      ! Equilibrium constant for the reaction : Fe(OH)2    <-------->    Fe2+    +     2(OH-)
    real(kind = DBL_PREC), dimension(nkn) :: K_2      ! Equilibrium constant for the reaction : Fe(OH)2    <-------->    [Fe(OH)]+     +    OH-
    real(kind = DBL_PREC), dimension(nkn) :: K_3      ! Equilibrium constant for the reaction : Fe(OH)2    +    OH-    <-------->     [Fe(OH)3]-
    real(kind = DBL_PREC), dimension(nkn) :: K_4      ! Equilibrium constant for the reaction : Fe(CO3)    <-------->    Fe++    +     (CO3)--
    real(kind = DBL_PREC), dimension(nkn) :: K_5      ! Equilibrium constant for the reaction : Fe(OH)2    +    OH-    <-------->     [Fe(OH)]+    +    CO3--
    real(kind = DBL_PREC), dimension(nkn) :: K_6      ! Equilibrium constant for the reaction : HCO3-      <-------->    H+      +    CO3--
    real(kind = DBL_PREC), dimension(nkn) :: K_7      ! Equilibrium constant for the reaction : FeS        <-------->    Fe++    +    S--
    real(kind = DBL_PREC), dimension(nkn) :: K_8      ! Equilibrium constant for the reaction : FeS        +    OH-    <-------->     [Fe(OH)]+    +    S--
    real(kind = DBL_PREC), dimension(nkn) :: K_9      ! Equilibrium constant for the reaction : FeS        +  3(OH-)   <-------->     [Fe(OH3)]-   +    S--
    real(kind = DBL_PREC), dimension(nkn) :: K_10_A   ! Equilibrium constant for the reaction : H2S        +    H+     <-------->     HS-
    real(kind = DBL_PREC), dimension(nkn) :: K_10_B   ! Equilibrium constant for the reaction : HS-        +    H+     <-------->     S--
    real(kind = DBL_PREC), dimension(nkn) :: K_W      ! Equilibrium constant for water dissociation

    real(kind = DBL_PREC), dimension(nkn) :: H_PLUS               ! [H+]    (mol/L)
    real(kind = DBL_PREC), dimension(nkn) :: OH_MINUS             ! [OH-]   (mol/L)

    real(kind = DBL_PREC), dimension(nkn,3) :: ALL_FE_II
    ! ALL_FE_II(:,1) : Fe(OH)2 solubility (mol/L)
    ! ALL_FE_II(:,2) : FeCO3   solubility (mol/L)
    ! ALL_FE_II(:,3) : FeS     solubility (mol/L)

    ! For now, equlibrium constants are hard-coded. In the future, they could be
    ! new nodel constants. Another option to calculate more realistic values
    ! for the model constants is to use the temperature and thermodynamic constants.
    K_1    = 8.0D-16
    K_2    = 4.0D-10
    K_3    = 8.3D-6
    K_4    = 2.1D-11
    K_5    = 1.0D-5
    K_6    = 4.8D-11
    K_7    = 6.0D-18
    K_8    = 3.0D-12
    K_9    = 6.2D-8
    K_10_A = 1.0D-7
    K_10_B = 1.3D-13
    K_W    = 1.0D-14

    H_PLUS   = 10.0D0 ** (-PH)
    OH_MINUS = K_W / H_PLUS

    ALL_FE_II(:,1) = &
        ((K_1  / (K_W * K_W)) * H_PLUS * H_PLUS) + ((K_2 / K_W) * H_PLUS) + &
        ((K_3 * K_W) / H_PLUS)

    ALL_FE_II(:,2) = &
        ((H_PLUS + (2.0D0 * K_6)) / (TOT_ALK * K_6)) * (K_4 + ((K_5 * K_W) / H_PLUS))

    ALL_FE_II(:,3) = 1.0D0

    where (HS2_TOT < 1.0D-20)
        HS2_TOT   = 1.0D-20
        FE_II_TOT = minval(ALL_FE_II(:,1:2), dim=2)
    elsewhere
        ALL_FE_II(:,3)  = &
            ((K_7 / HS2_TOT) * &
             (1.0D0 + (H_PLUS / K_10_B) + ((H_PLUS * H_PLUS) / (K_10_B * K_10_A)))) + &
             (K_7 * ((OH_MINUS / K_8) + ((27.0D0 * OH_MINUS * OH_MINUS * OH_MINUS) / K_9)))

      FE_II_TOT(:) = minval(ALL_FE_II, dim=2)
    end where

end subroutine IRON_II_DISSOLUTION



! -----------------------------------------------------------------------------
! The subroutine below calculates the Iron II oxidation according to
! Morgen and Lahav (2007).
! -----------------------------------------------------------------------------
!
!                       Initial development 6 th of July 2016
!
!                                by Ali Ertürk
! -----------------------------------------------------------------------------
subroutine IRON_II_OXIDATION(FE_II_DISS, DOXY, PH, TEMP, SALT, ELEVATION, nkn, R_FE_II_OXIDATION)
    use AQUABC_II_GLOBAL
    implicit none

    integer, intent(in) ::  nkn

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FE_II_DISS    ! Total dissolved Fe2+   (mg/L)
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PH            ! PH
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DOXY          ! Dissolved oxygen
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: SALT
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: ELEVATION

    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_FE_II_OXIDATION     ! Rate of Fe2+ oxidation (mg/L/day)

    ! Use environmental conditions to estimate the values of following rate and equilibrium constants
    real(kind = DBL_PREC), dimension(nkn) :: K_1           ! Equilibrium constant K_1
    real(kind = DBL_PREC), dimension(nkn) :: K_2           ! Equilibrium constant K_2
    real(kind = DBL_PREC), dimension(nkn) :: K_3           ! Equilibrium constant K_3
    real(kind = DBL_PREC), dimension(nkn) :: K_W           ! Equilibrium constant for water dissociation
    real(kind = DBL_PREC), dimension(nkn) :: k_FE_II       ! Rate constant for Fe2+ oxidation
    real(kind = DBL_PREC), dimension(nkn) :: k_FE_OH_PLUS  ! Rate constant for Fe2(OH)+ oxidation
    real(kind = DBL_PREC), dimension(nkn) :: k_FE_OH_2     ! Rate constant for Fe2(OH)2 oxidation

    ! Auxillary variables
    real(kind = DBL_PREC), dimension(nkn) :: H_PLUS        ! [H+]    (mol/L)
    real(kind = DBL_PREC), dimension(nkn) :: OH_MINUS      ! [OH-]   (mol/L)
    real(kind = DBL_PREC), dimension(nkn) :: FE_II_TOT     ! Fe2+Tot (mol/L)
    real(kind = DBL_PREC), dimension(nkn) :: CS

    ! For now, equlibrium constants are hard-coded. In the future, they could be
    ! new nodel constants. Another option to calculate more realistic values
    ! for the model constants is to use the temperature and thermodynamic constants.
    K_1    = 10.0D0**(4.50D0)
    K_2    = 10.0D0**(2.93D0)
    K_3    = 10.0D0**(3.57D0)

    K_W    = 1.0D-14

    k_FE_II      = 6.0D-5
    k_FE_OH_PLUS = 1.7D0
    k_FE_OH_2    = 4.3D5

    H_PLUS    = 10.0D0 ** (-PH)
    OH_MINUS  = K_W / H_PLUS
    FE_II_TOT = FE_II_DISS / 56000.0D0

    call DO_SATURATION_VEC(TEMP, SALT, ELEVATION, nkn, CS);

    R_FE_II_OXIDATION = &
        ((k_FE_II      / (1.0D0 + ((K_1*K_W) / (H_PLUS))  + &
                           ((K_1*K_2*K_W*K_W) / (H_PLUS*H_PLUS)) + &
                           ((K_1*K_2*K_W*K_W*K_W*K_W) / (H_PLUS*H_PLUS*H_PLUS)))) + &
         (k_FE_OH_PLUS / (1.0D0 + ((H_PLUS)  / (K_1*K_W)) + &
                          ((K_2*K_W) / (H_PLUS))  +  &
                          ((K_2*K_3*K_W*K_W) / (H_PLUS*H_PLUS))))  + &
         (k_FE_OH_2    / (1.0D0 + ((K_3*K_W) / (H_PLUS))  +  &
                          ((H_PLUS) / (K_2*K_W)) + &
                           ((H_PLUS*H_PLUS) / (K_1*K_2*K_W*K_W))))) * FE_II_TOT

    R_FE_II_OXIDATION = (R_FE_II_OXIDATION * (DOXY / CS)) * 56000.0D0
end subroutine IRON_II_OXIDATION
