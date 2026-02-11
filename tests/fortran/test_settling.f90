! Test program for settling velocity suppression calculations
! Tests the settling_suppres_factor_vec and settl_vel_vec subroutines
!
! Compile with:
!   gfortran -o test_settling test_settling.f90 \
!     ../../SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_SETTLING.f90
!
! Run: ./test_settling

program test_settling
    implicit none
    
    integer :: num_passed, num_failed, num_tests
    
    ! External subroutine declarations
    external :: settling_suppres_factor_vec
    external :: settl_vel_vec
    
    num_passed = 0
    num_failed = 0
    num_tests = 0
    
    write(*,*) '=============================================='
    write(*,*) 'Settling Velocity Test Suite'
    write(*,*) '=============================================='
    write(*,*)
    
    call test_settl_vel_basic(num_passed, num_failed, num_tests)
    call test_settl_vel_relationship(num_passed, num_failed, num_tests)
    call test_suppression_low_chla(num_passed, num_failed, num_tests)
    call test_suppression_mid_chla(num_passed, num_failed, num_tests)
    call test_suppression_high_chla(num_passed, num_failed, num_tests)
    call test_vector_settling(num_passed, num_failed, num_tests)
    
    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,*) '=============================================='
    
    if (num_failed > 0) then
        stop 1
    end if
    
contains

    subroutine test_settl_vel_basic(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: chla(nkn), vels(nkn)
        
        total = total + 1
        write(*,*) 'Test: Basic settling velocity calculation'
        
        ! Test with moderate chlorophyll-a
        chla(1) = 50.0D0  ! μg/L
        
        call settl_vel_vec(chla, nkn, vels)
        
        ! Expected: -(0.0061 * 50) + 1.0383 = -0.305 + 1.0383 = 0.7333
        if (abs(vels(1) - 0.7333D0) < 0.01D0) then
            write(*,*) '  PASSED: VELS =', vels(1), '(expected ~0.733)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: VELS =', vels(1), '(expected ~0.733)'
            failed = failed + 1
        end if
    end subroutine test_settl_vel_basic
    
    subroutine test_settl_vel_relationship(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 2
        
        double precision :: chla(nkn), vels(nkn)
        
        total = total + 1
        write(*,*) 'Test: Higher CHLA leads to lower settling velocity'
        
        chla(1) = 50.0D0   ! Lower chlorophyll-a
        chla(2) = 100.0D0  ! Higher chlorophyll-a
        
        call settl_vel_vec(chla, nkn, vels)
        
        ! Higher CHLA should result in lower settling velocity (inverse relationship)
        if (vels(1) > vels(2)) then
            write(*,*) '  PASSED: Low CHLA vel (', vels(1), ') > High CHLA vel (', vels(2), ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Expected low CHLA to have higher settling velocity'
            failed = failed + 1
        end if
    end subroutine test_settl_vel_relationship
    
    subroutine test_suppression_low_chla(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: chla(nkn), factor(nkn)
        
        total = total + 1
        write(*,*) 'Test: Low CHLA (< 44 μg/L) gives factor = 1.0'
        
        chla(1) = 20.0D0  ! Below minimum threshold
        
        call settling_suppres_factor_vec(chla, nkn, factor)
        
        ! Below chlamin (44), factor should be 1.0 (no suppression)
        if (abs(factor(1) - 1.0D0) < 0.01D0) then
            write(*,*) '  PASSED: Factor =', factor(1), '(expected 1.0)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Factor =', factor(1), '(expected 1.0)'
            failed = failed + 1
        end if
    end subroutine test_suppression_low_chla
    
    subroutine test_suppression_mid_chla(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: chla(nkn), factor(nkn)
        
        total = total + 1
        write(*,*) 'Test: Mid-range CHLA (44-140 μg/L) gives factor <= 1.0'
        
        chla(1) = 90.0D0  ! Middle of range
        
        call settling_suppres_factor_vec(chla, nkn, factor)
        
        ! In mid-range, factor should be calculated and <= 1.0
        if (factor(1) <= 1.0D0 .and. factor(1) >= 0.0D0) then
            write(*,*) '  PASSED: Factor =', factor(1), '(in valid range 0-1)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Factor =', factor(1), '(expected 0 <= factor <= 1)'
            failed = failed + 1
        end if
    end subroutine test_suppression_mid_chla
    
    subroutine test_suppression_high_chla(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: chla(nkn), factor(nkn)
        
        total = total + 1
        write(*,*) 'Test: High CHLA (> 140 μg/L) gives constant factor'
        
        chla(1) = 200.0D0  ! Above maximum threshold
        
        call settling_suppres_factor_vec(chla, nkn, factor)
        
        ! Above chlamax, factor should be constant (capped)
        if (factor(1) >= 0.0D0 .and. factor(1) <= 1.0D0) then
            write(*,*) '  PASSED: Factor =', factor(1), '(capped value)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Factor =', factor(1), '(unexpected value)'
            failed = failed + 1
        end if
    end subroutine test_suppression_high_chla
    
    subroutine test_vector_settling(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 5
        
        double precision :: chla(nkn), vels(nkn)
        integer :: i
        logical :: all_valid
        
        total = total + 1
        write(*,*) 'Test: Vector settling velocity calculation'
        
        ! Range of chlorophyll-a values
        chla(1) = 10.0D0
        chla(2) = 50.0D0
        chla(3) = 90.0D0
        chla(4) = 130.0D0
        chla(5) = 170.0D0
        
        call settl_vel_vec(chla, nkn, vels)
        
        ! Check monotonic decrease with increasing CHLA
        all_valid = .true.
        do i = 1, nkn - 1
            if (vels(i) <= vels(i+1)) then
                all_valid = .false.
            end if
        end do
        
        if (all_valid) then
            write(*,*) '  PASSED: Monotonic decrease with increasing CHLA'
            write(*,*) '    VELS:', vels
            passed = passed + 1
        else
            write(*,*) '  FAILED: Expected monotonic decrease'
            write(*,*) '    VELS:', vels
            failed = failed + 1
        end if
    end subroutine test_vector_settling

end program test_settling
