! Pelagic kinetic model ALUKAS_II
! Version with variables calculated in subroutines
! Version with dissolved inorganic carbon and alkalinity as state variables.

!Contains:
! subroutine CALCULATE_PH_CORR



! ---------------------------------------------------------------------------- !
! Subroutine that calculates the PH_CORRECTION factor according to             !
! the euqations (Eq. 162 and Eq. 163) in AQUATOX Relase 3.1 Plus documentation !
! (EPA-829-R-14-007)                                                           !
! Specialised for WC by Petras                                                 !
! ---------------------------------------------------------------------------- !
!                                DOCUMENTATION                                 !
!                                                                              !
! INPUTS :                                                                     !
! --------                                                                     !
! PH_MIN  : Minimum value of pH for the optimum range (scalar)                 !
! PH_MAX  : Maximum value of pH for the optimum range (scalar)                 !
! PH      : PH values for which correction factor will be calculated (matrix)  !
! nkn     : Number of rows in matrices PH and PH_CORR                          !
! nlay    : Number of columns in in matrices PH and PH_CORR                    !
!                                                                              !
! OUTPUTS                                                                      !
! PH_CORR : pH correction factors calculated for PH (matrix)                   !
! ---------------------------------------------------------------------------- !
!                    Development date : 31st August 2015                       !
!                                                                              !
!                            Developer : Ali Ertürk                            !
! ---------------------------------------------------------------------------- !
subroutine CALCULATE_PH_CORR(PH_CORR, PH, PH_MIN, PH_MAX, nkn)

    use AQUABC_II_GLOBAL
    implicit none

    ! Ingoing variables
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PH
    real(kind = DBL_PREC), intent(in) :: PH_MIN
    real(kind = DBL_PREC), intent(in) :: PH_MAX
    integer, intent(in) :: nkn

    ! Outgoing variables
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PH_CORR

    integer i,j,error

    error   = 0
    PH_CORR = 1

    !return

    where(PH(:) < PH_MIN)
        PH_CORR(:) = exp(PH(:) - PH_MIN)
    end where

    where(PH(:) > PH_MAX)
        PH_CORR(:) = exp(PH_MAX - PH(:))
    end where

    do i=1,nkn

        if (PH_CORR(i) .gt. 1.D0 .or. PH_CORR(i) .lt. 0.D0) then
            print *, 'CALCULATE_PH_CORR: Incorrect PH_CORR'
            print *, 'PH_CORR',PH_CORR(i)
            print *, 'PH_MIN:', PH_MIN
            print *, 'PH_MAX:', PH_MAX
            print *, 'PH:', PH(i)
            print *, 'Internal node number:',i
            error=1
        end if

        if (error .eq. 1) stop
    end do


end subroutine CALCULATE_PH_CORR
