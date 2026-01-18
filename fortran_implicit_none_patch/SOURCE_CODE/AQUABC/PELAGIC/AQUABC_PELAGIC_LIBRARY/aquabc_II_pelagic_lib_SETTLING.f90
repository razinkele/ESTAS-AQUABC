! Auxilary routines for the pelagic model

! Contents:

!subroutine settling_suppres_factor_vec

subroutine settling_suppres_factor_vec(chla, nkn, factor)

    ! Calculates settling velocity supprss factor do to high chlorophyl a
    ! concentrations
    ! Chla in mcg/l

    implicit none

    double precision, dimension(nkn), intent(in) :: chla
    integer                         , intent(in) :: nkn

    double precision, dimension(nkn), intent(inout) :: factor

    double precision, dimension(nkn) :: factor_chla
    double precision, dimension(nkn) :: factor_chlamax

    double precision, dimension(nkn) :: setl_vel
    double precision, dimension(nkn) :: chlamin
    double precision, dimension(nkn) :: chlamax
    double precision, dimension(nkn) :: settl_max

    ! observed interval endpoints for chla impacting settling velocity
    ! We do not know what happens outside of this interval and assume
    ! the same factor values as on endpoints there3

    chlamin =  44.0D0
    chlamax = 140.0D0

    call settl_vel_vec(chlamin, nkn, settl_max)
    call settl_vel_vec(chlamin, nkn, factor_chla)
    call settl_vel_vec(chlamin, nkn, factor_chlamax)

    where (chla .lt. chlamin)
        factor = 1.0
    elsewhere((chla .ge. chlamin) .and. (chla .le. chlamax))
        factor = factor_chla    / settl_max
    elsewhere
        factor = factor_chlamax / settl_max
    end where

end subroutine settling_suppres_factor_vec


subroutine settl_vel_vec(chla, nkn, VELS)
    implicit none
    double precision, dimension(nkn), intent(in) :: chla
    integer,  intent(in) :: nkn

    double precision, dimension(nkn), intent(inout) :: VELS

    VELS = -(0.0061D0 * chla) + 1.0383
end subroutine settl_vel_vec
