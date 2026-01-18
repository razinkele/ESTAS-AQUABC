! -----------------------------------------------------------------------------
! Interpolasyon mod�l�
! -----------------------------------------------------------------------------
module INTERPOLATE
    implicit none
    integer, parameter :: DBL_PRECISION = selected_real_kind(15, 307)
contains

    function LDPS_INTERPOLATE(X_VALUE, X1, Y1, X2, Y2) result(Y_VALUE)

        implicit none
        real(kind = DBL_PRECISION), intent(in) :: X_VALUE
        real(kind = DBL_PRECISION), intent(in) :: X1
        real(kind = DBL_PRECISION), intent(in) :: Y1
        real(kind = DBL_PRECISION), intent(in) :: X2
        real(kind = DBL_PRECISION), intent(in) :: Y2

        real(kind = DBL_PRECISION) :: Y_VALUE

        real(kind = DBL_PRECISION) :: SMALL_X, Y_OF_SMALL_X, LARGE_X, Y_OF_LARGE_X
        real(kind = DBL_PRECISION) :: DELTA_X, DELTA_Y, SLOPE

        if(dabs(X1 - X2) < 1.0D-50) then
            Y_VALUE = (Y1 + Y2) / 2.0
        else
            if (X1 < X2) then
                SMALL_X = X1
                Y_OF_SMALL_X = Y1
                LARGE_X = X2
                Y_OF_LARGE_X = Y2
            else
                SMALL_X = X2
                Y_OF_SMALL_X = Y2
                LARGE_X = X1
                Y_OF_LARGE_X = Y1
            end if

            if ((X_VALUE < SMALL_X).or.(X_VALUE > LARGE_X)) then
                Y_VALUE = 0.0
            else
                DELTA_X = LARGE_X - SMALL_X
                DELTA_Y = Y_OF_LARGE_X - Y_OF_SMALL_X
                SLOPE   = DELTA_Y / DELTA_X
                Y_VALUE = Y_OF_SMALL_X + ((X_VALUE - SMALL_X) * SLOPE)
            end if

        end if

    end function LDPS_INTERPOLATE

end module INTERPOLATE