! Test program for pH correction factor calculations
! Tests the CALCULATE_PH_CORR subroutine
!
! Compile with:
!   gfortran -o test_ph_corr test_ph_corr.f90 \
!     ../../SOURCE_CODE/AQUABC/mod_AQUABC_II_GLOBAL.f90 \
!     ../../SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_PH_CORR.f90
!
! Run: ./test_ph_corr

program test_ph_corr
    use AQUABC_II_GLOBAL
    implicit none
    
    integer :: num_passed, num_failed, num_tests
    
    num_passed = 0
    num_failed = 0
    num_tests = 0
    
    write(*,*) '=============================================='
    write(*,*) 'pH Correction Factor Test Suite'
    write(*,*) '=============================================='
    write(*,*)
    
    call test_optimal_ph_range(num_passed, num_failed, num_tests)
    call test_ph_below_minimum(num_passed, num_failed, num_tests)
    call test_ph_above_maximum(num_passed, num_failed, num_tests)
    call test_ph_at_boundary(num_passed, num_failed, num_tests)
    call test_symmetric_correction(num_passed, num_failed, num_tests)
    call test_vector_ph_correction(num_passed, num_failed, num_tests)
    
    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,*) '=============================================='
    
    if (num_failed > 0) then
        stop 1
    end if
    
contains

    subroutine test_optimal_ph_range(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        real(kind = DBL_PREC) :: PH(nkn), PH_CORR(nkn)
        real(kind = DBL_PREC) :: PH_MIN, PH_MAX
        
        total = total + 1
        write(*,*) 'Test: pH within optimal range gives correction = 1.0'
        
        PH_MIN = 6.5D0
        PH_MAX = 8.5D0
        PH(1) = 7.5D0  ! Within optimal range
        PH_CORR(1) = 0.0D0
        
        call CALCULATE_PH_CORR(PH_CORR, PH, PH_MIN, PH_MAX, nkn)
        
        if (abs(PH_CORR(1) - 1.0D0) < 1.0D-10) then
            write(*,*) '  PASSED: PH_CORR =', PH_CORR(1), '(expected 1.0)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: PH_CORR =', PH_CORR(1), '(expected 1.0)'
            failed = failed + 1
        end if
    end subroutine test_optimal_ph_range
    
    subroutine test_ph_below_minimum(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        real(kind = DBL_PREC) :: PH(nkn), PH_CORR(nkn)
        real(kind = DBL_PREC) :: PH_MIN, PH_MAX
        real(kind = DBL_PREC) :: expected
        
        total = total + 1
        write(*,*) 'Test: pH below minimum gives correction < 1.0'
        
        PH_MIN = 6.5D0
        PH_MAX = 8.5D0
        PH(1) = 5.5D0  ! Below minimum
        PH_CORR(1) = 0.0D0
        
        call CALCULATE_PH_CORR(PH_CORR, PH, PH_MIN, PH_MAX, nkn)
        
        ! Expected: exp(5.5 - 6.5) = exp(-1.0) ≈ 0.368
        expected = exp(PH(1) - PH_MIN)
        
        if (abs(PH_CORR(1) - expected) < 1.0D-6 .and. PH_CORR(1) < 1.0D0) then
            write(*,*) '  PASSED: PH_CORR =', PH_CORR(1), '(expected', expected, ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: PH_CORR =', PH_CORR(1), '(expected', expected, ')'
            failed = failed + 1
        end if
    end subroutine test_ph_below_minimum
    
    subroutine test_ph_above_maximum(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        real(kind = DBL_PREC) :: PH(nkn), PH_CORR(nkn)
        real(kind = DBL_PREC) :: PH_MIN, PH_MAX
        real(kind = DBL_PREC) :: expected
        
        total = total + 1
        write(*,*) 'Test: pH above maximum gives correction < 1.0'
        
        PH_MIN = 6.5D0
        PH_MAX = 8.5D0
        PH(1) = 9.5D0  ! Above maximum
        PH_CORR(1) = 0.0D0
        
        call CALCULATE_PH_CORR(PH_CORR, PH, PH_MIN, PH_MAX, nkn)
        
        ! Expected: exp(8.5 - 9.5) = exp(-1.0) ≈ 0.368
        expected = exp(PH_MAX - PH(1))
        
        if (abs(PH_CORR(1) - expected) < 1.0D-6 .and. PH_CORR(1) < 1.0D0) then
            write(*,*) '  PASSED: PH_CORR =', PH_CORR(1), '(expected', expected, ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: PH_CORR =', PH_CORR(1), '(expected', expected, ')'
            failed = failed + 1
        end if
    end subroutine test_ph_above_maximum
    
    subroutine test_ph_at_boundary(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 2
        
        real(kind = DBL_PREC) :: PH(nkn), PH_CORR(nkn)
        real(kind = DBL_PREC) :: PH_MIN, PH_MAX
        
        total = total + 1
        write(*,*) 'Test: pH at boundaries gives correction = 1.0'
        
        PH_MIN = 6.5D0
        PH_MAX = 8.5D0
        PH(1) = 6.5D0  ! At minimum boundary
        PH(2) = 8.5D0  ! At maximum boundary
        PH_CORR = 0.0D0
        
        call CALCULATE_PH_CORR(PH_CORR, PH, PH_MIN, PH_MAX, nkn)
        
        if (abs(PH_CORR(1) - 1.0D0) < 1.0D-10 .and. abs(PH_CORR(2) - 1.0D0) < 1.0D-10) then
            write(*,*) '  PASSED: Both boundary values = 1.0'
            passed = passed + 1
        else
            write(*,*) '  FAILED: PH_CORR =', PH_CORR(1), PH_CORR(2), '(expected 1.0, 1.0)'
            failed = failed + 1
        end if
    end subroutine test_ph_at_boundary
    
    subroutine test_symmetric_correction(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 2
        
        real(kind = DBL_PREC) :: PH(nkn), PH_CORR(nkn)
        real(kind = DBL_PREC) :: PH_MIN, PH_MAX
        
        total = total + 1
        write(*,*) 'Test: Symmetric deviation from range gives equal correction'
        
        PH_MIN = 6.5D0
        PH_MAX = 8.5D0
        PH(1) = 5.5D0  ! 1.0 below minimum
        PH(2) = 9.5D0  ! 1.0 above maximum
        PH_CORR = 0.0D0
        
        call CALCULATE_PH_CORR(PH_CORR, PH, PH_MIN, PH_MAX, nkn)
        
        ! Both should give exp(-1.0) ≈ 0.368
        if (abs(PH_CORR(1) - PH_CORR(2)) < 1.0D-10) then
            write(*,*) '  PASSED: Symmetric corrections:', PH_CORR(1), '=', PH_CORR(2)
            passed = passed + 1
        else
            write(*,*) '  FAILED: Asymmetric corrections:', PH_CORR(1), '/=', PH_CORR(2)
            failed = failed + 1
        end if
    end subroutine test_symmetric_correction
    
    subroutine test_vector_ph_correction(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 7
        
        real(kind = DBL_PREC) :: PH(nkn), PH_CORR(nkn)
        real(kind = DBL_PREC) :: PH_MIN, PH_MAX
        integer :: i
        logical :: all_valid
        
        total = total + 1
        write(*,*) 'Test: Vector pH correction calculation'
        
        PH_MIN = 6.5D0
        PH_MAX = 8.5D0
        
        ! Range of pH values
        PH(1) = 4.5D0   ! Very acidic
        PH(2) = 5.5D0   ! Acidic
        PH(3) = 6.5D0   ! At min boundary
        PH(4) = 7.5D0   ! Optimal
        PH(5) = 8.5D0   ! At max boundary
        PH(6) = 9.5D0   ! Basic
        PH(7) = 10.5D0  ! Very basic
        
        PH_CORR = 0.0D0
        
        call CALCULATE_PH_CORR(PH_CORR, PH, PH_MIN, PH_MAX, nkn)
        
        all_valid = .true.
        do i = 1, nkn
            if (PH_CORR(i) < 0.0D0 .or. PH_CORR(i) > 1.0D0) then
                all_valid = .false.
            end if
        end do
        
        if (all_valid) then
            write(*,*) '  PASSED: All corrections in valid range [0, 1]'
            write(*,*) '    PH_CORR:', PH_CORR
            passed = passed + 1
        else
            write(*,*) '  FAILED: Some corrections out of range'
            write(*,*) '    PH_CORR:', PH_CORR
            failed = failed + 1
        end if
    end subroutine test_vector_ph_correction

end program test_ph_corr
