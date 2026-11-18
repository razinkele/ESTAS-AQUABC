! Unit tests for CALCULATE_SURFACE_ELEVATION (mod_BASIN): the level-from-volume inversion.
! Regression for the first-branch sign bug (QUAD_C = +2 V): with the water inside the layer that holds the
! box bottom, a constant-area layer had no positive root, the level stayed at the bottom and DEPTH read 0
! with water in the box (CL29 note item 73, docs §66).

program test_basin_level
    use precision_kinds, only: DBL
    use BASIN
    implicit none

    integer :: passed, failed
    type(BATHYMETRY_DS) :: B
    real(kind = DBL), parameter :: A = 28428956.5D0          ! box 18 of CL29
    real(kind = DBL), parameter :: ZB = -1.311D0             ! its bottom elevation
    real(kind = DBL) :: z, v, h, alo, a_slope
    integer :: i

    passed = 0
    failed = 0
    print *, ""
    print *, "=========================================="
    print *, "Basin level-from-volume tests"
    print *, "=========================================="

    ! the synthetic EUTROPY profile: 0.5 m layers from -2.0 to +2.0, constant area, bottom layer tapering to 0.3 A
    call ALLOCATE_BATHYMETRY_DATA(B, 8)
    do i = 1, 8
        B % LOWER_ELEVATION(i) = -2.0D0 + 0.5D0 * (i - 1)
        B % UPPER_ELEVATION(i) = B % LOWER_ELEVATION(i) + 0.5D0
        B % UPPER_AREA(i) = A
        B % LOWER_AREA(i) = A
    end do
    B % LOWER_AREA(1) = 0.3D0 * A

    ! 1. water inside the bottom's own constant-area layer (the bug's case): h = V / A
    v = 6.71D6
    z = CALCULATE_SURFACE_ELEVATION(B, ZB, v)
    call check(z - ZB, v / A, 1.0D-9, "constant-area bottom layer: depth = V / A (was 0)")

    ! 2. a volume exactly filling the bottom's layer to its top
    v = A * (-1.0D0 - ZB)
    z = CALCULATE_SURFACE_ELEVATION(B, ZB, v * (1.0D0 - 1.0D-12))
    call check(z, -1.0D0, 1.0D-6, "volume filling the bottom layer: level at the layer top")

    ! 3. above the bottom layer (the later-layer branch, unchanged): level 0 for V = A |ZB|
    v = A * (-ZB)
    z = CALCULATE_SURFACE_ELEVATION(B, ZB, v)
    call check(z, 0.0D0, 1.0D-9, "volume above the bottom layer: level 0")

    ! 4. a tapering layer: bottom at -2.0 where the area is 0.3 A; (a/2) h^2 + A_lo h = V
    alo = 0.3D0 * A
    a_slope = (A - alo) / 0.5D0
    v = 2.0D6
    h = (-alo + sqrt(alo**2 + 2.0D0 * a_slope * v)) / a_slope
    z = CALCULATE_SURFACE_ELEVATION(B, -2.0D0, v)
    call check(z + 2.0D0, h, 1.0D-9, "tapering bottom layer: quadratic root")

    ! 5. a zero-area bottom (a cone-like first layer): h = sqrt(2 V / a)
    B % LOWER_AREA(1) = 0.0D0
    a_slope = A / 0.5D0
    v = 1.0D6
    z = CALCULATE_SURFACE_ELEVATION(B, -2.0D0, v)
    call check(z + 2.0D0, sqrt(2.0D0 * v / a_slope), 1.0D-9, "zero-area bottom: h = sqrt(2 V / a) (was 0/0)")
    B % LOWER_AREA(1) = 0.3D0 * A

    ! 6. round trip over a range of volumes from a mid-layer bottom
    do i = 1, 9
        v = A * 0.15D0 * i
        z = CALCULATE_SURFACE_ELEVATION(B, ZB, v)
        call check(CALCULATE_VOLUME_BETWEEN_LEVELS(B, ZB, z) / v, 1.0D0, 1.0D-9, "round trip V -> level -> V")
    end do

    print *, ""
    print '(A,I3,A)', " Passed: ", passed, " tests"
    print '(A,I3,A)', " Failed: ", failed, " tests"
    if (failed == 0) then
        print *, "ALL TESTS PASSED!"
    else
        print *, "SOME TESTS FAILED"
        stop 1
    end if

contains

    subroutine check(got, want, tol, name)
        real(kind = DBL), intent(in) :: got, want, tol
        character(len=*), intent(in) :: name
        if (abs(got - want) <= tol) then
            print *, "  PASS: ", trim(name)
            passed = passed + 1
        else
            print '(A,A,A,ES16.8,A,ES16.8)', "  FAIL: ", trim(name), "  got ", got, " want ", want
            failed = failed + 1
        end if
    end subroutine check

end program test_basin_level
