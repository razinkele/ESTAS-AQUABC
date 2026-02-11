! Test program for para_aqua module (parameter dictionary)
! Tests parameter storage and retrieval functions
!
! Compile with:
!   gfortran -o test_para_aqua test_para_aqua.f90 \
!     ../../SOURCE_CODE/CORE_UTILS/STRING_UTILS.f90
!
! Run: ./test_para_aqua

program test_para_aqua
    use para_aqua
    implicit none
    
    integer :: num_passed, num_failed, num_tests
    
    num_passed = 0
    num_failed = 0
    num_tests = 0
    
    write(*,*) '=============================================='
    write(*,*) 'Parameter Dictionary (para_aqua) Test Suite'
    write(*,*) '=============================================='
    write(*,*)
    
    call test_add_and_get_value(num_passed, num_failed, num_tests)
    call test_exists_name(num_passed, num_failed, num_tests)
    call test_put_value(num_passed, num_failed, num_tests)
    call test_insert_value(num_passed, num_failed, num_tests)
    call test_multiple_parameters(num_passed, num_failed, num_tests)
    call test_get_name(num_passed, num_failed, num_tests)
    call test_para_get_fill(num_passed, num_failed, num_tests)
    
    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,*) '=============================================='
    
    if (num_failed > 0) then
        stop 1
    end if
    
contains

    subroutine test_add_and_get_value(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: value
        
        total = total + 1
        write(*,*) 'Test: Add and retrieve a parameter value'
        
        call para_add_value('test_param_1', 123.456D0)
        call para_get_value('test_param_1', value)
        
        if (abs(value - 123.456D0) < 1.0D-10) then
            write(*,*) '  PASSED: Retrieved value =', value
            passed = passed + 1
        else
            write(*,*) '  FAILED: Expected 123.456, got', value
            failed = failed + 1
        end if
    end subroutine test_add_and_get_value
    
    subroutine test_exists_name(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        logical :: exists
        
        total = total + 1
        write(*,*) 'Test: Check if parameter name exists'
        
        ! test_param_1 was added in previous test
        exists = para_exists_name('test_param_1')
        
        if (exists) then
            write(*,*) '  PASSED: test_param_1 exists'
            passed = passed + 1
        else
            write(*,*) '  FAILED: test_param_1 should exist'
            failed = failed + 1
        end if
        
        total = total + 1
        write(*,*) 'Test: Check non-existent parameter'
        
        exists = para_exists_name('nonexistent_param')
        
        if (.not. exists) then
            write(*,*) '  PASSED: nonexistent_param does not exist'
            passed = passed + 1
        else
            write(*,*) '  FAILED: nonexistent_param should not exist'
            failed = failed + 1
        end if
    end subroutine test_exists_name
    
    subroutine test_put_value(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: value
        
        total = total + 1
        write(*,*) 'Test: Update existing parameter value'
        
        ! Update test_param_1
        call para_put_value('test_param_1', 999.999D0)
        call para_get_value('test_param_1', value)
        
        if (abs(value - 999.999D0) < 1.0D-10) then
            write(*,*) '  PASSED: Updated value =', value
            passed = passed + 1
        else
            write(*,*) '  FAILED: Expected 999.999, got', value
            failed = failed + 1
        end if
    end subroutine test_put_value
    
    subroutine test_insert_value(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: value
        
        total = total + 1
        write(*,*) 'Test: Insert new value (adds if not exists)'
        
        call para_insert_value('new_param', 42.0D0)
        call para_get_value('new_param', value)
        
        if (abs(value - 42.0D0) < 1.0D-10) then
            write(*,*) '  PASSED: New param value =', value
            passed = passed + 1
        else
            write(*,*) '  FAILED: Expected 42.0, got', value
            failed = failed + 1
        end if
        
        total = total + 1
        write(*,*) 'Test: Insert updates existing value'
        
        call para_insert_value('new_param', 84.0D0)
        call para_get_value('new_param', value)
        
        if (abs(value - 84.0D0) < 1.0D-10) then
            write(*,*) '  PASSED: Updated value =', value
            passed = passed + 1
        else
            write(*,*) '  FAILED: Expected 84.0, got', value
            failed = failed + 1
        end if
    end subroutine test_insert_value
    
    subroutine test_multiple_parameters(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        double precision :: val1, val2, val3
        integer :: i
        character(len=20) :: param_name
        
        total = total + 1
        write(*,*) 'Test: Add and retrieve multiple parameters'
        
        ! Add several parameters
        do i = 1, 10
            write(param_name, '(A,I2.2)') 'multi_param_', i
            call para_add_value(trim(param_name), dble(i * 100))
        end do
        
        ! Retrieve and verify some
        call para_get_value('multi_param_01', val1)
        call para_get_value('multi_param_05', val2)
        call para_get_value('multi_param_10', val3)
        
        if (abs(val1 - 100.0D0) < 1.0D-10 .and. &
            abs(val2 - 500.0D0) < 1.0D-10 .and. &
            abs(val3 - 1000.0D0) < 1.0D-10) then
            write(*,*) '  PASSED: All values correct (100, 500, 1000)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Got', val1, val2, val3
            failed = failed + 1
        end if
    end subroutine test_multiple_parameters
    
    subroutine test_get_name(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        character(len=80) :: name
        
        total = total + 1
        write(*,*) 'Test: Get parameter name by index'
        
        call para_get_name(1, name)
        
        if (trim(name) == 'test_param_1') then
            write(*,*) '  PASSED: First name is "', trim(name), '"'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Expected test_param_1, got "', trim(name), '"'
            failed = failed + 1
        end if
    end subroutine test_get_name
    
    subroutine test_para_get_fill(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer :: nfill_val
        
        total = total + 1
        write(*,*) 'Test: Get total number of parameters'
        
        nfill_val = para_get_fill()
        
        ! We added: test_param_1, new_param, multi_param_01..10 = 12 total
        if (nfill_val == 12) then
            write(*,*) '  PASSED: nfill =', nfill_val, '(expected 12)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: nfill =', nfill_val, '(expected 12)'
            failed = failed + 1
        end if
    end subroutine test_para_get_fill

end program test_para_aqua
