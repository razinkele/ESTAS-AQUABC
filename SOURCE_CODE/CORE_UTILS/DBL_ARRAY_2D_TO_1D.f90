function DBL_ARRAY_2D_TO_1D(TWO_D) result(ONE_D)
    use GLOBAL
    implicit none

    real(kind = DBL), intent(in), dimension(:,:) :: TWO_D
    real(kind = DBL), allocatable, dimension(:) :: ONE_D
    integer :: n

    n = size(TWO_D)
    allocate(ONE_D(n))
    ONE_D = reshape(TWO_D, [n])

end function DBL_ARRAY_2D_TO_1D
