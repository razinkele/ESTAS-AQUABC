! Main routines for bottom sediment model No1
! CONTENT:



! ----------------------------------------------------------------------------!
! Subroutine that calculates the PH_CORRECTION factor according to            !
! the euqations (Eq. 162 and Eq. 163) in AQUTOX Relase 3.1 Plus documentation !
! (EPA-829-R-14-007)                                                          !
! Specialised for  for BS  by Petras                                          !
! --------------------------------------------------------------------------- !
!                                DOCUMENTATION                                !
!                                                                             !
! INPUTS :                                                                    !
! --------                                                                    !
! PH_MIN  : Minimum value of pH for the optimum range (scalar)                !
! PH_MAX  : Maximum value of pH for the optimum range (scalar)                !
! PH      : PH values for which correction factor will be calculated (matrix) !
! nkn     : Number of rows in matrices PH and PH_CORR                         !
! nlay    : Number of columns in in matrices PH and PH_CORR                   !
!                                                                             !
! OUTPUTS                                                                     !
! PH_CORR : pH correction factors calculated for PH (matrix)                  !
! --------------------------------------------------------------------------- !
!                    Development date : 31st August 2015                      !
!                                                                             !
!                            Developer : Ali Ertürk                           !
! --------------------------------------------------------------------------- !
subroutine CALCULATE_PH_CORR_SED(PH_CORR, PH, PH_MIN, PH_MAX, nkn, nlay)

    use AQUABC_II_GLOBAL
    implicit none

    ! Ingoing variables
    real(kind = DBL_PREC), dimension(nkn, nlay), intent(in) :: PH
    real(kind = DBL_PREC), intent(in) :: PH_MIN
    real(kind = DBL_PREC), intent(in) :: PH_MAX
    integer, intent(in) :: nkn, nlay
    ! Outgoing variables
    real(kind = DBL_PREC), dimension(nkn, nlay), intent(inout) :: PH_CORR

    integer i,j, error

    error = 0


    PH_CORR = 1


    !return
    !PH_MIN_ARRAY(:) = PH_MIN
    !PH_MAX_ARRAY(:) = PH_MAX

    do j=1,nlay

      where(PH(:,j) < PH_MIN)
          PH_CORR(:,j) = exp(PH(:,j) - PH_MIN)
      end where

      where(PH(:,j) > PH_MAX)
          PH_CORR(:,j) = exp(PH_MAX - PH(:,j))
      end where

    end do

    do i=1,nkn
        do j=1,nlay
            if (PH_CORR(i,j) .gt. 1.D0 .or. PH_CORR(i,j) .lt. 0.D0) then
                print *, 'CALCULATE_PH_CORR: Incorrect PH_CORR'
                print *, 'PH_CORR',PH_CORR(i,j)
                print *, 'PH_MIN:', PH_MIN
                print *, 'PH_MAX:', PH_MAX
                print *, 'PH:', PH(i,j)
                print *, 'Internal node number:',i
                print *, 'Layer number:',j
                print *, 'Number of layers:',nlay
                error =1
            end if
        end do
    end do

   if(error .eq. 1)stop

end subroutine CALCULATE_PH_CORR_SED
