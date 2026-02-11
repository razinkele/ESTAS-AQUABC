! Test program for DO_SATURATION routines
! Tests dissolved oxygen saturation calculations
!
! Compile with:
!   gfortran -o test_do_saturation test_do_saturation.f90 \
!     ../../SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_DO_SATURATION.f90
!
! Run: ./test_do_saturation

program test_do_saturation
    implicit none
    
    integer :: num_passed, num_failed, num_tests
    
    num_passed = 0
    num_failed = 0
    num_tests = 0
    
    write(*,*) '=============================================='
    write(*,*) 'DO_SATURATION Test Suite'
    write(*,*) '=============================================='
    write(*,*)
    
    call test_basic_saturation(num_passed, num_failed, num_tests)
    call test_temperature_effect(num_passed, num_failed, num_tests)
    call test_salinity_effect(num_passed, num_failed, num_tests)
    call test_altitude_effect(num_passed, num_failed, num_tests)
    call test_vector_operation(num_passed, num_failed, num_tests)
    call test_extreme_values(num_passed, num_failed, num_tests)
    
    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,*) '=============================================='
    
    if (num_failed > 0) then
        stop 1
    end if
    
contains

    subroutine test_basic_saturation(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: T(1), S(1), H(1), CS(1)
        double precision :: expected_cs
        
        total = total + 1
        write(*,*) 'Test: Basic saturation at 20C, 0 ppt, sea level'
        
        ! Standard conditions: 20°C, freshwater, sea level
        T(1) = 20.0D0
        S(1) = 0.0D0
        H(1) = 0.0D0
        
        call DO_SATURATION_VEC(T, S, H, 1, CS)
        
        ! Expected DO saturation at 20C freshwater is approximately 9.09 mg/L
        expected_cs = 9.09D0
        
        if (abs(CS(1) - expected_cs) < 0.5D0) then
            write(*,*) '  PASSED: CS =', CS(1), ' (expected ~', expected_cs, ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: CS =', CS(1), ' (expected ~', expected_cs, ')'
            failed = failed + 1
        end if
    end subroutine test_basic_saturation
    
    subroutine test_temperature_effect(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: T_cold(1), T_warm(1), S(1), H(1)
        double precision :: CS_cold(1), CS_warm(1)
        
        total = total + 1
        write(*,*) 'Test: Temperature effect (cold water should have higher DO)'
        
        ! Cold water (5°C) vs warm water (25°C)
        T_cold(1) = 5.0D0
        T_warm(1) = 25.0D0
        S(1) = 0.0D0
        H(1) = 0.0D0
        
        call DO_SATURATION_VEC(T_cold, S, H, 1, CS_cold)
        call DO_SATURATION_VEC(T_warm, S, H, 1, CS_warm)
        
        ! Cold water should hold more oxygen
        if (CS_cold(1) > CS_warm(1)) then
            write(*,*) '  PASSED: Cold (', CS_cold(1), ') > Warm (', CS_warm(1), ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Cold (', CS_cold(1), ') should be > Warm (', CS_warm(1), ')'
            failed = failed + 1
        end if
    end subroutine test_temperature_effect
    
    subroutine test_salinity_effect(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: T(1), S_fresh(1), S_salt(1), H(1)
        double precision :: CS_fresh(1), CS_salt(1)
        
        total = total + 1
        write(*,*) 'Test: Salinity effect (fresh water should have higher DO)'
        
        T(1) = 20.0D0
        S_fresh(1) = 0.0D0
        S_salt(1) = 35.0D0  ! Ocean salinity
        H(1) = 0.0D0
        
        call DO_SATURATION_VEC(T, S_fresh, H, 1, CS_fresh)
        call DO_SATURATION_VEC(T, S_salt, H, 1, CS_salt)
        
        ! Fresh water should hold more oxygen than salt water
        if (CS_fresh(1) > CS_salt(1)) then
            write(*,*) '  PASSED: Fresh (', CS_fresh(1), ') > Salt (', CS_salt(1), ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Fresh (', CS_fresh(1), ') should be > Salt (', CS_salt(1), ')'
            failed = failed + 1
        end if
    end subroutine test_salinity_effect
    
    subroutine test_altitude_effect(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: T(1), S(1), H_sea(1), H_mountain(1)
        double precision :: CS_sea(1), CS_mountain(1)
        
        total = total + 1
        write(*,*) 'Test: Altitude effect (sea level should have higher DO)'
        
        T(1) = 20.0D0
        S(1) = 0.0D0
        H_sea(1) = 0.0D0
        H_mountain(1) = 3000.0D0  ! 3000m elevation
        
        call DO_SATURATION_VEC(T, S, H_sea, 1, CS_sea)
        call DO_SATURATION_VEC(T, S, H_mountain, 1, CS_mountain)
        
        ! Sea level should have higher DO saturation
        if (CS_sea(1) > CS_mountain(1)) then
            write(*,*) '  PASSED: Sea level (', CS_sea(1), ') > Mountain (', CS_mountain(1), ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Sea level (', CS_sea(1), ') should be > Mountain (', CS_mountain(1), ')'
            failed = failed + 1
        end if
    end subroutine test_altitude_effect
    
    subroutine test_vector_operation(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 5
        double precision :: T(nkn), S(nkn), H(nkn), CS(nkn)
        integer :: i
        logical :: all_positive
        
        total = total + 1
        write(*,*) 'Test: Vector operation with multiple nodes'
        
        ! Set up varying conditions
        do i = 1, nkn
            T(i) = 15.0D0 + dble(i) * 2.0D0  ! 17, 19, 21, 23, 25
            S(i) = dble(i) * 5.0D0           ! 5, 10, 15, 20, 25
            H(i) = 0.0D0
        end do
        
        call DO_SATURATION_VEC(T, S, H, nkn, CS)
        
        ! All values should be positive
        all_positive = .true.
        do i = 1, nkn
            if (CS(i) <= 0.0D0) all_positive = .false.
        end do
        
        if (all_positive) then
            write(*,*) '  PASSED: All ', nkn, ' values are positive'
            write(*,*) '    CS values:', CS
            passed = passed + 1
        else
            write(*,*) '  FAILED: Some values are not positive'
            write(*,*) '    CS values:', CS
            failed = failed + 1
        end if
    end subroutine test_vector_operation
    
    subroutine test_extreme_values(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: T(1), S(1), H(1), CS(1)
        logical :: test_ok
        
        total = total + 1
        write(*,*) 'Test: Extreme temperature values'
        
        test_ok = .true.
        
        ! Test near-freezing
        T(1) = 0.1D0
        S(1) = 0.0D0
        H(1) = 0.0D0
        call DO_SATURATION_VEC(T, S, H, 1, CS)
        if (CS(1) <= 0.0D0 .or. CS(1) > 20.0D0) then
            write(*,*) '  Near-freezing test: unexpected CS =', CS(1)
            test_ok = .false.
        else
            write(*,*) '  Near-freezing (0.1C): CS =', CS(1)
        end if
        
        ! Test warm tropical water
        T(1) = 30.0D0
        S(1) = 35.0D0
        H(1) = 0.0D0
        call DO_SATURATION_VEC(T, S, H, 1, CS)
        if (CS(1) <= 0.0D0 .or. CS(1) > 20.0D0) then
            write(*,*) '  Tropical test: unexpected CS =', CS(1)
            test_ok = .false.
        else
            write(*,*) '  Tropical (30C, 35ppt): CS =', CS(1)
        end if
        
        if (test_ok) then
            write(*,*) '  PASSED: All extreme values handled correctly'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Some extreme values gave unexpected results'
            failed = failed + 1
        end if
    end subroutine test_extreme_values

end program test_do_saturation
