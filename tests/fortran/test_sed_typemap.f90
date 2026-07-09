! Test program for the two-type (sandy/muddy) sediment profile assignment.
!
! Exercises SED_TYPEMAP::ASSIGN_SED_PROFILES_TO_BOXES in isolation — the pure
! core of the Phase-2a reader extension (mod_BOTTOM_SEDIMENTS). Verifies:
!   1. a 2-type box->type map gives each box its type's geometry + IC block,
!      with the IC copied under the legacy transpose
!      init_sed_state_vars(box,layer,var) = type_ic(type,var,layer);
!   2. a single-type map broadcasts profile 1 to every box (legacy behaviour).
!
! Compile with:
!   gfortran -O2 -Wall -Wextra -fcheck=all -o test_sed_typemap \
!     ../../SOURCE_CODE/CORE_UTILS/precision_kinds.f90 \
!     ../../SOURCE_CODE/ESTAS/mod_SED_TYPEMAP.f90 \
!     test_sed_typemap.f90
!
! Run: ./test_sed_typemap

program test_sed_typemap
    use precision_kinds, only: DBL
    use SED_TYPEMAP, only: ASSIGN_SED_PROFILES_TO_BOXES
    implicit none

    integer :: num_passed, num_failed, num_tests

    num_passed = 0
    num_failed = 0
    num_tests  = 0

    write(*,*) '=============================================='
    write(*,*) 'Sediment Two-Type Profile Assignment Test Suite'
    write(*,*) '=============================================='
    write(*,*)

    call test_two_type_map(num_passed, num_failed, num_tests)
    call test_single_type_broadcast(num_passed, num_failed, num_tests)

    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,'(A,I3,A)')      ' Total:   ', num_tests, ' tests'
    write(*,*) '=============================================='

    if (num_failed > 0) then
        stop 1
    end if

contains

    ! Distinct, easily-checked per-type profile values for NUM_SED_TYPES = 2.
    subroutine build_two_types(nl, nv, td, tp, tden, tmix, tbur, tic)
        integer, intent(in) :: nl, nv
        real(kind=DBL), intent(out) :: td(2,nl), tp(2,nl), tden(2,nl)
        real(kind=DBL), intent(out) :: tmix(2), tbur(2), tic(2,nv,nl)
        integer :: layer, var

        do layer = 1, nl
            td(1,layer)   = 10.0D0 + real(layer, DBL)      ! sandy depths
            td(2,layer)   = 20.0D0 + real(layer, DBL)      ! muddy depths
            tp(1,layer)   = 0.30D0 + 0.01D0*real(layer, DBL)
            tp(2,layer)   = 0.80D0 + 0.01D0*real(layer, DBL)
            tden(1,layer) = 2.60D0                          ! quartz sand
            tden(2,layer) = 1.20D0                          ! organic mud
        end do
        tmix(1) = 1.0D-3; tmix(2) = 5.0D-2
        tbur(1) = 1.0D-4; tbur(2) = 5.0D-4
        do var = 1, nv
            do layer = 1, nl
                tic(1,var,layer) = 100.0D0 + 10.0D0*real(var, DBL) + real(layer, DBL)
                tic(2,var,layer) = 900.0D0 + 10.0D0*real(var, DBL) + real(layer, DBL)
            end do
        end do
    end subroutine build_two_types

    subroutine test_two_type_map(passed, failed, total)
        integer, intent(inout) :: passed, failed, total

        integer, parameter :: nkn = 4, nl = 2, nv = 3
        integer :: type_per_box(nkn)
        real(kind=DBL) :: td(2,nl), tp(2,nl), tden(2,nl), tmix(2), tbur(2), tic(2,nv,nl)
        real(kind=DBL) :: depths(nkn,nl), poros(nkn,nl), dens(nkn,nl), bur(nkn,nl)
        real(kind=DBL) :: pmix(nkn,nl,nv), ic(nkn,nl,nv)
        integer :: box, layer, var, t
        logical :: ok
        real(kind=DBL), parameter :: TOL = 1.0D-12

        total = total + 1
        write(*,*) 'Test: 2-type map assigns each box its type geometry + IC transpose'

        type_per_box = [1, 2, 2, 1]
        call build_two_types(nl, nv, td, tp, tden, tmix, tbur, tic)

        call ASSIGN_SED_PROFILES_TO_BOXES(nkn, nl, nv, type_per_box, &
             td, tp, tden, tmix, tbur, tic, &
             depths, poros, dens, bur, pmix, ic)

        ok = .true.
        do box = 1, nkn
            t = type_per_box(box)
            do layer = 1, nl
                if (abs(depths(box,layer) - td(t,layer))  > TOL) ok = .false.
                if (abs(poros(box,layer)  - tp(t,layer))  > TOL) ok = .false.
                if (abs(dens(box,layer)   - tden(t,layer))> TOL) ok = .false.
                if (abs(bur(box,layer)    - tbur(t))      > TOL) ok = .false.   ! scalar broadcast
                do var = 1, nv
                    if (abs(pmix(box,layer,var) - tmix(t)) > TOL) ok = .false.  ! scalar broadcast
                    ! IC transpose: ic(box,layer,var) == type_ic(t,var,layer)
                    if (abs(ic(box,layer,var) - tic(t,var,layer)) > TOL) ok = .false.
                end do
            end do
        end do

        if (ok) then
            write(*,*) '  PASSED: box1/4 -> sandy, box2/3 -> muddy, IC transposed'
            passed = passed + 1
        else
            write(*,*) '  FAILED: per-box profile/IC assignment incorrect'
            write(*,*) '    box2 depth L1 got', depths(2,1), 'want', td(2,1)
            write(*,*) '    box2 ic(2,1,1) got', ic(2,1,1), 'want tic(2,1,1)', tic(2,1,1)
            failed = failed + 1
        end if
    end subroutine test_two_type_map

    subroutine test_single_type_broadcast(passed, failed, total)
        integer, intent(inout) :: passed, failed, total

        integer, parameter :: nkn = 3, nl = 2, nv = 3
        integer :: type_per_box(nkn)
        real(kind=DBL) :: td(1,nl), tp(1,nl), tden(1,nl), tmix(1), tbur(1), tic(1,nv,nl)
        real(kind=DBL) :: depths(nkn,nl), poros(nkn,nl), dens(nkn,nl), bur(nkn,nl)
        real(kind=DBL) :: pmix(nkn,nl,nv), ic(nkn,nl,nv)
        integer :: box, layer, var
        logical :: ok
        real(kind=DBL), parameter :: TOL = 1.0D-12

        total = total + 1
        write(*,*) 'Test: single-type map broadcasts profile 1 to every box (legacy)'

        type_per_box = 1
        do layer = 1, nl
            td(1,layer) = 5.0D0 + real(layer, DBL)
            tp(1,layer) = 0.4D0
            tden(1,layer) = 1.75D0
        end do
        tmix(1) = 2.64D-5; tbur(1) = 2.74D-4
        do var = 1, nv
            do layer = 1, nl
                tic(1,var,layer) = 7.0D0*real(var, DBL) + real(layer, DBL)
            end do
        end do

        call ASSIGN_SED_PROFILES_TO_BOXES(nkn, nl, nv, type_per_box, &
             td, tp, tden, tmix, tbur, tic, &
             depths, poros, dens, bur, pmix, ic)

        ok = .true.
        do box = 1, nkn
            do layer = 1, nl
                if (abs(depths(box,layer) - td(1,layer)) > TOL) ok = .false.
                if (abs(poros(box,layer)  - tp(1,layer)) > TOL) ok = .false.
                if (abs(dens(box,layer)   - tden(1,layer)) > TOL) ok = .false.
                if (abs(bur(box,layer)    - tbur(1)) > TOL) ok = .false.
                do var = 1, nv
                    if (abs(pmix(box,layer,var) - tmix(1)) > TOL) ok = .false.
                    if (abs(ic(box,layer,var) - tic(1,var,layer)) > TOL) ok = .false.
                end do
            end do
        end do

        if (ok) then
            write(*,*) '  PASSED: all boxes identical to profile 1'
            passed = passed + 1
        else
            write(*,*) '  FAILED: single-type broadcast not identical across boxes'
            failed = failed + 1
        end if
    end subroutine test_single_type_broadcast

end program test_sed_typemap
