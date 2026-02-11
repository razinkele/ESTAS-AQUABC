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
!                                by Ali Ert�rk
! -----------------------------------------------------------------------------
subroutine IP_SOLUBLE_FRACTION &
            (FE_III, PO4P, K_A_1, K_A_2, K_A_3, PH, nkn, nlayers, DIP_OVER_IP)

    use AQUABC_II_GLOBAL
    use AQUABC_PHYSICAL_CONSTANTS, only: FE_MOLAR_MASS_MG
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
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: COEFF_1    ! Fe(III) hydrolysis side-reaction coefficient
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: COEFF_2    ! PO4 protonation side-reaction coefficient
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: KS0        ! Solubility product for FePO4 <-> Fe3+ + PO4---
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: H_PLUS     ! [H+]    (mol/L)
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: C_T_PO4    ! PO4 solubility in moles
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: C_T_PO4_IP ! Total inorganic P in mol/L
    ! Fe(III) hydrolysis constants (Stumm and Morgan, 1996)
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: K_1_FE     ! Fe3+ + H2O <-> FeOH2+ + H+       (10^-3.05)
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: K_2_FE     ! Fe3+ + 2H2O <-> Fe(OH)2+ + 2H+   (10^-6.31)
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: K_3_FE     ! Fe3+ + 3H2O <-> Fe(OH)3 + 3H+    (10^-13.8)
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: K_4_FE     ! Fe3+ + 4H2O <-> Fe(OH)4- + 4H+   (10^-22.7)

    ! FePO4 solubility product (Stumm and Morgan, 1996): log Ksp ~ -26.4
    KS0        = 10.0D0 ** (-26.4D0)
    ! Fe(III) stepwise/overall hydrolysis constants at 25 C
    K_1_FE     = 10.0D0 ** (-3.05D0)
    K_2_FE     = 10.0D0 ** (-6.31D0)
    K_3_FE     = 10.0D0 ** (-13.8D0)
    K_4_FE     = 10.0D0 ** (-22.7D0)
    H_PLUS     = 10.0D0 ** (-max(4.0D0, min(11.0D0, PH)))
    C_T_FE_III = FE_III / FE_MOLAR_MASS_MG

    ! COEFF_1: accounts for all 4 Fe(III) hydrolysis products
    COEFF_1    = 1.0D0 + (K_1_FE / H_PLUS) + &
                 (K_2_FE / (H_PLUS * H_PLUS)) + &
                 (K_3_FE / (H_PLUS * H_PLUS * H_PLUS)) + &
                 (K_4_FE / (H_PLUS * H_PLUS * H_PLUS * H_PLUS))

    COEFF_2    = 1.0D0 + ((H_PLUS*H_PLUS*H_PLUS)/(K_A_1*K_A_2*K_A_3)) + &
                 ((H_PLUS*H_PLUS)/(K_A_2*K_A_3)) + (H_PLUS/K_A_3)

    ! Convert total inorganic P from mg-P/L to mol/L (P molar mass = 30974 mg/mol)
    C_T_PO4_IP = PO4P / 30974.0D0

    ! Main equation
    ! KS0 = (C_T_FE_III / COEFF_1) * (C_T_PO4 / COEFF_2)
    !   ==> C_T_PO4 = (KS0 * COEFF_1 * COEFF_2) / C_T_FE_III
    ! DIP_OVER_IP = dissolved PO4 / total inorganic P
    where (C_T_FE_III > 1.0D-30 .and. C_T_PO4_IP > 1.0D-30)
        C_T_PO4     = (KS0 * COEFF_1 * COEFF_2) / C_T_FE_III
        DIP_OVER_IP = C_T_PO4 / C_T_PO4_IP
    elsewhere
        ! No Fe(III) or no P present: all phosphorus remains dissolved
        DIP_OVER_IP = 1.0D0
    end where

    where(DIP_OVER_IP > 1.0D0)
        DIP_OVER_IP = 1.0D0
    end where

end subroutine IP_SOLUBLE_FRACTION
