! Auxilary routines for the pelagic model

! Contents:
!subroutine IP_SOLUBLE_FRACTION


! -----------------------------------------------------------------------------
! The subroutine below calculates the inorganic phosphorus dissolution according
! to Veroni Snoeyink and Jenkins, 1980.
! -----------------------------------------------------------------------------
!
!                       Initial development 6 th of July 2016
!
!                                by Ali Ertürk
! -----------------------------------------------------------------------------
subroutine IP_SOLUBLE_FRACTION &
            (FE_III, PO4P, K_A_1, K_A_2, K_A_3, PH, nkn, nlayers, DIP_OVER_IP)

    use AQUABC_II_GLOBAL
    implicit none

    integer, intent(in) ::  nkn, nlayers

    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: FE_III
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: K_A_1    ! First dissociation constant for H3PO4
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: K_A_2    ! Second dissociation constant for H3PO4
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: K_A_3    ! Third dissociation constant for H3PO4
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: PO4P     ! Total inorganic phosphorus (mg/L)
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: PH

    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(inout) :: DIP_OVER_IP

    ! Auxillary variables
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: C_T_FE_III ! [Fe3+]tot (moles/L)
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: COEFF_1    ! Intermediate ceofficient as a function of [H+], K_1, K_4
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: COEFF_2    ! Intermediate ceofficient as a function of [H+], K_A_1, K_A_2, K_A_3
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: KS0        ! Equlibrium constant for AlPO4 <-----> Al3+  +  PO4---
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: H_PLUS     ! [H+]    (mol/L)
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: C_T_PO4    ! PO4 solubility in moles
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: K_1        ! Equlibrium constant for Al+++    +     H2O    <-------->    Al(OH)++    +     H+
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: K_4        ! Equlibrium constant for Al+++    +    4H2O    <-------->    Al(OH)4-    +     4H+

    KS0        = 1.0D-21
    K_1        = 1.0D-5
    K_4        = 10.0D0 ** (-21.7D0)
    H_PLUS     = 10.0D0 ** (-PH)
    C_T_FE_III = FE_III / 56000.0D0
    COEFF_1    = 1.0D0 + (K_1/H_PLUS) + (K_4/(H_PLUS*H_PLUS*H_PLUS*H_PLUS))

    COEFF_2    = 1.0D0 + ((H_PLUS*H_PLUS*H_PLUS)/(K_A_1*K_A_2*K_A_3)) + &
	             ((H_PLUS*H_PLUS)/(K_A_2*K_A_3)) + (H_PLUS/K_A_3)

    ! Main equation
    ! KS0 = (C_T_FE_III / COEFF_1) * (C_T_PO4 / COEFF_2)
    !   ==> C_T_PO4 = (KS0 * COEFF_1 * COEFF_2) / C_T_FE_III
    C_T_PO4 = (KS0 * COEFF_1 * COEFF_2) / C_T_FE_III

    DIP_OVER_IP = C_T_PO4 / C_T_FE_III

    where(DIP_OVER_IP > 1.0D0)
        DIP_OVER_IP = 1.0D0
    end where

end subroutine IP_SOLUBLE_FRACTION
