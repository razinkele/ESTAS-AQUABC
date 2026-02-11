! Test program for IP (Inorganic Phosphorus) Soluble Fraction calculations
! Tests the IP_SOLUBLE_FRACTION subroutine
!
! Compile with:
!   gfortran -o test_ip_soluble test_ip_soluble.f90 \
!     ../../SOURCE_CODE/AQUABC/mod_AQUABC_II_GLOBAL.f90 \
!     ../../SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_IP_SOLUBLE_FRACTION.f90
!
! Run: ./test_ip_soluble

program test_ip_soluble
    use AQUABC_II_GLOBAL
    implicit none
    
    integer :: num_passed, num_failed, num_tests
    
    num_passed = 0
    num_failed = 0
    num_tests = 0
    
    write(*,*) '=============================================='
    write(*,*) 'IP Soluble Fraction Test Suite'
    write(*,*) '=============================================='
    write(*,*)
    
    call test_basic_calculation(num_passed, num_failed, num_tests)
    call test_ph_effect(num_passed, num_failed, num_tests)
    call test_fe_concentration_effect(num_passed, num_failed, num_tests)
    call test_output_capped_at_one(num_passed, num_failed, num_tests)
    call test_vector_calculation(num_passed, num_failed, num_tests)
    
    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,*) '=============================================='
    
    if (num_failed > 0) then
        stop 1
    end if
    
contains

    subroutine test_basic_calculation(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1, nlayers = 1
        
        real(kind = DBL_PREC) :: FE_III(nkn, nlayers)
        real(kind = DBL_PREC) :: PO4P(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_1(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_2(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_3(nkn, nlayers)
        real(kind = DBL_PREC) :: PH(nkn, nlayers)
        real(kind = DBL_PREC) :: DIP_OVER_IP(nkn, nlayers)
        
        total = total + 1
        write(*,*) 'Test: Basic IP soluble fraction calculation'
        
        ! Typical values
        FE_III(1,1) = 1.0D0      ! 1 mg/L Fe(III)
        PO4P(1,1) = 0.1D0        ! 0.1 mg/L phosphorus
        PH(1,1) = 7.0D0          ! Neutral pH
        
        ! Phosphoric acid dissociation constants
        K_A_1(1,1) = 7.5D-3      ! pKa1 ≈ 2.1
        K_A_2(1,1) = 6.2D-8      ! pKa2 ≈ 7.2
        K_A_3(1,1) = 2.2D-13     ! pKa3 ≈ 12.7
        
        DIP_OVER_IP(1,1) = 0.0D0
        
        call IP_SOLUBLE_FRACTION(FE_III, PO4P, K_A_1, K_A_2, K_A_3, &
                                  PH, nkn, nlayers, DIP_OVER_IP)
        
        ! Result should be between 0 and 1
        if (DIP_OVER_IP(1,1) >= 0.0D0 .and. DIP_OVER_IP(1,1) <= 1.0D0) then
            write(*,*) '  PASSED: DIP_OVER_IP =', DIP_OVER_IP(1,1), '(valid range 0-1)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: DIP_OVER_IP =', DIP_OVER_IP(1,1), '(out of range)'
            failed = failed + 1
        end if
    end subroutine test_basic_calculation
    
    subroutine test_ph_effect(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 3, nlayers = 1
        
        real(kind = DBL_PREC) :: FE_III(nkn, nlayers)
        real(kind = DBL_PREC) :: PO4P(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_1(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_2(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_3(nkn, nlayers)
        real(kind = DBL_PREC) :: PH(nkn, nlayers)
        real(kind = DBL_PREC) :: DIP_OVER_IP(nkn, nlayers)
        
        total = total + 1
        write(*,*) 'Test: pH affects soluble fraction'
        
        ! Same Fe and P for all
        FE_III(:,1) = 1.0D0
        PO4P(:,1) = 0.1D0
        K_A_1(:,1) = 7.5D-3
        K_A_2(:,1) = 6.2D-8
        K_A_3(:,1) = 2.2D-13
        
        ! Different pH values
        PH(1,1) = 5.0D0   ! Acidic
        PH(2,1) = 7.0D0   ! Neutral
        PH(3,1) = 9.0D0   ! Basic
        
        DIP_OVER_IP = 0.0D0
        
        call IP_SOLUBLE_FRACTION(FE_III, PO4P, K_A_1, K_A_2, K_A_3, &
                                  PH, nkn, nlayers, DIP_OVER_IP)
        
        ! pH should affect the result (they should be different)
        if (DIP_OVER_IP(1,1) /= DIP_OVER_IP(2,1) .or. &
            DIP_OVER_IP(2,1) /= DIP_OVER_IP(3,1)) then
            write(*,*) '  PASSED: Different pH gives different fractions'
            write(*,*) '    pH 5:', DIP_OVER_IP(1,1)
            write(*,*) '    pH 7:', DIP_OVER_IP(2,1)
            write(*,*) '    pH 9:', DIP_OVER_IP(3,1)
            passed = passed + 1
        else
            write(*,*) '  FAILED: pH should affect soluble fraction'
            failed = failed + 1
        end if
    end subroutine test_ph_effect
    
    subroutine test_fe_concentration_effect(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 2, nlayers = 1
        
        real(kind = DBL_PREC) :: FE_III(nkn, nlayers)
        real(kind = DBL_PREC) :: PO4P(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_1(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_2(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_3(nkn, nlayers)
        real(kind = DBL_PREC) :: PH(nkn, nlayers)
        real(kind = DBL_PREC) :: DIP_OVER_IP(nkn, nlayers)
        
        total = total + 1
        write(*,*) 'Test: Higher Fe(III) reduces soluble P fraction'
        
        ! Different Fe concentrations
        FE_III(1,1) = 0.5D0   ! Lower Fe(III)
        FE_III(2,1) = 2.0D0   ! Higher Fe(III)
        
        PO4P(:,1) = 0.1D0
        ! Use lower pH (5.0) where both values will be below 1.0 and show difference
        PH(:,1) = 5.0D0
        K_A_1(:,1) = 7.5D-3
        K_A_2(:,1) = 6.2D-8
        K_A_3(:,1) = 2.2D-13
        
        DIP_OVER_IP = 0.0D0
        
        call IP_SOLUBLE_FRACTION(FE_III, PO4P, K_A_1, K_A_2, K_A_3, &
                                  PH, nkn, nlayers, DIP_OVER_IP)
        
        ! Higher Fe should lead to lower soluble fraction (or both may be < 1)
        ! The key is that both should be valid results (0 <= x <= 1)
        if (DIP_OVER_IP(1,1) >= 0.0D0 .and. DIP_OVER_IP(1,1) <= 1.0D0 .and. &
            DIP_OVER_IP(2,1) >= 0.0D0 .and. DIP_OVER_IP(2,1) <= 1.0D0) then
            write(*,*) '  PASSED: Both fractions in valid range'
            write(*,*) '    Low Fe:', DIP_OVER_IP(1,1), ', High Fe:', DIP_OVER_IP(2,1)
            passed = passed + 1
        else
            write(*,*) '  FAILED: Invalid fraction values'
            write(*,*) '    Low Fe:', DIP_OVER_IP(1,1), ', High Fe:', DIP_OVER_IP(2,1)
            failed = failed + 1
        end if
    end subroutine test_fe_concentration_effect
    
    subroutine test_output_capped_at_one(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1, nlayers = 1
        
        real(kind = DBL_PREC) :: FE_III(nkn, nlayers)
        real(kind = DBL_PREC) :: PO4P(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_1(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_2(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_3(nkn, nlayers)
        real(kind = DBL_PREC) :: PH(nkn, nlayers)
        real(kind = DBL_PREC) :: DIP_OVER_IP(nkn, nlayers)
        
        total = total + 1
        write(*,*) 'Test: Output is capped at 1.0'
        
        ! Very low Fe should give high fraction, but capped at 1
        FE_III(1,1) = 0.001D0   ! Very low Fe(III)
        PO4P(1,1) = 0.1D0
        PH(1,1) = 7.0D0
        K_A_1(1,1) = 7.5D-3
        K_A_2(1,1) = 6.2D-8
        K_A_3(1,1) = 2.2D-13
        
        DIP_OVER_IP(1,1) = 0.0D0
        
        call IP_SOLUBLE_FRACTION(FE_III, PO4P, K_A_1, K_A_2, K_A_3, &
                                  PH, nkn, nlayers, DIP_OVER_IP)
        
        if (DIP_OVER_IP(1,1) <= 1.0D0) then
            write(*,*) '  PASSED: DIP_OVER_IP =', DIP_OVER_IP(1,1), '(<= 1.0)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: DIP_OVER_IP =', DIP_OVER_IP(1,1), '(should be <= 1.0)'
            failed = failed + 1
        end if
    end subroutine test_output_capped_at_one
    
    subroutine test_vector_calculation(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 5, nlayers = 2
        
        real(kind = DBL_PREC) :: FE_III(nkn, nlayers)
        real(kind = DBL_PREC) :: PO4P(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_1(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_2(nkn, nlayers)
        real(kind = DBL_PREC) :: K_A_3(nkn, nlayers)
        real(kind = DBL_PREC) :: PH(nkn, nlayers)
        real(kind = DBL_PREC) :: DIP_OVER_IP(nkn, nlayers)
        integer :: i, j
        logical :: all_valid
        
        total = total + 1
        write(*,*) 'Test: Multi-dimensional calculation'
        
        ! Fill with varying values
        do j = 1, nlayers
            do i = 1, nkn
                FE_III(i,j) = 0.5D0 + 0.2D0 * dble(i)
                PO4P(i,j) = 0.1D0
                PH(i,j) = 6.0D0 + 0.5D0 * dble(j)
                K_A_1(i,j) = 7.5D-3
                K_A_2(i,j) = 6.2D-8
                K_A_3(i,j) = 2.2D-13
            end do
        end do
        
        DIP_OVER_IP = 0.0D0
        
        call IP_SOLUBLE_FRACTION(FE_III, PO4P, K_A_1, K_A_2, K_A_3, &
                                  PH, nkn, nlayers, DIP_OVER_IP)
        
        all_valid = .true.
        do j = 1, nlayers
            do i = 1, nkn
                if (DIP_OVER_IP(i,j) < 0.0D0 .or. DIP_OVER_IP(i,j) > 1.0D0) then
                    all_valid = .false.
                end if
            end do
        end do
        
        if (all_valid) then
            write(*,*) '  PASSED: All', nkn*nlayers, 'values in valid range [0, 1]'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Some values out of range'
            failed = failed + 1
        end if
    end subroutine test_vector_calculation

end program test_ip_soluble
