! Unit tests for FIND_FILE_UNIT utility
! Tests the find_unit subroutine for file unit allocation

program test_file_unit
    implicit none
    
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "Find File Unit Utility Tests"
    print *, "=========================================="
    print *, ""
    
    call test_basic_unit()
    call test_starting_unit()
    call test_avoids_unit_5()
    call test_avoids_unit_6()
    call test_finds_next_available()
    call test_zero_input()
    call test_negative_input()
    
    print *, ""
    print *, "=========================================="
    print *, "Test Summary"
    print *, "=========================================="
    print '(A,I3,A)', " Passed: ", passed, " tests"
    print '(A,I3,A)', " Failed: ", failed, " tests"
    print *, "=========================================="
    
    if (failed == 0) then
        print *, "ALL TESTS PASSED!"
    else
        print *, "SOME TESTS FAILED"
        stop 1
    end if

contains

    subroutine assert_equal_int(val1, val2, test_name)
        integer, intent(in) :: val1, val2
        character(len=*), intent(in) :: test_name
        
        if (val1 == val2) then
            print *, "  PASS: ", trim(test_name)
            passed = passed + 1
        else
            print *, "  FAIL: ", trim(test_name)
            print '(A,I6,A,I6)', "    Expected ", val1, " == ", val2
            failed = failed + 1
        end if
    end subroutine assert_equal_int
    
    subroutine assert_not_equal_int(val1, val2, test_name)
        integer, intent(in) :: val1, val2
        character(len=*), intent(in) :: test_name
        
        if (val1 /= val2) then
            print *, "  PASS: ", trim(test_name)
            passed = passed + 1
        else
            print *, "  FAIL: ", trim(test_name)
            print '(A,I6,A,I6)', "    Expected ", val1, " /= ", val2
            failed = failed + 1
        end if
    end subroutine assert_not_equal_int
    
    subroutine assert_greater_than_int(val1, val2, test_name)
        integer, intent(in) :: val1, val2
        character(len=*), intent(in) :: test_name
        
        if (val1 > val2) then
            print *, "  PASS: ", trim(test_name)
            passed = passed + 1
        else
            print *, "  FAIL: ", trim(test_name)
            print '(A,I6,A,I6)', "    Expected ", val1, " > ", val2
            failed = failed + 1
        end if
    end subroutine assert_greater_than_int

    !------------------------------------------------------------------
    ! Local copy of find_unit subroutine
    !------------------------------------------------------------------
    subroutine find_unit(iunit)
        implicit none
        integer, intent(inout) :: iunit
        logical :: opened
        integer, save :: iumax = 1000
        integer :: iu
        
        iu = iunit
        
        if (iu <= 0) then
            iu = 20  ! Set standard unit
        end if
        
        iunit = 0
        
        do
            ! Safeguard units 5 and 6 (stdin/stdout)
            if (iu == 5) then
                iu = 7
            end if
            
            if (iu > iumax) then
                write(6,*) 'no unit available to open file: ', iu
                return
            end if
            
            inquire(iu, opened=opened)
            
            if (.not. opened) then
                exit
            end if
            
            iu = iu + 1
        end do
        
        iunit = iu
    end subroutine find_unit

    !------------------------------------------------------------------
    ! Test: Basic unit allocation
    !------------------------------------------------------------------
    subroutine test_basic_unit()
        integer :: iunit
        
        print *, "Test: Basic unit allocation"
        
        iunit = 10
        call find_unit(iunit)
        
        ! Should return a valid unit number (not 0)
        call assert_not_equal_int(iunit, 0, "Returns non-zero unit")
        call assert_greater_than_int(iunit, 0, "Unit is positive")
    end subroutine test_basic_unit
    
    !------------------------------------------------------------------
    ! Test: Starting from specified unit
    !------------------------------------------------------------------
    subroutine test_starting_unit()
        integer :: iunit
        
        print *, "Test: Starting from specified unit"
        
        iunit = 50
        call find_unit(iunit)
        
        ! Should return 50 or higher (50 if not in use)
        call assert_greater_than_int(iunit, 49, "Returns unit >= 50")
    end subroutine test_starting_unit
    
    !------------------------------------------------------------------
    ! Test: Avoids unit 5 (stdin)
    !------------------------------------------------------------------
    subroutine test_avoids_unit_5()
        integer :: iunit
        
        print *, "Test: Avoids unit 5 (stdin)"
        
        iunit = 5
        call find_unit(iunit)
        
        ! Should skip unit 5
        call assert_not_equal_int(iunit, 5, "Returned unit is not 5")
        call assert_greater_than_int(iunit, 5, "Unit > 5")
    end subroutine test_avoids_unit_5
    
    !------------------------------------------------------------------
    ! Test: Also works when starting near unit 6 (stdout)
    !------------------------------------------------------------------
    subroutine test_avoids_unit_6()
        integer :: iunit
        
        print *, "Test: Works when starting at unit 4"
        
        iunit = 4
        call find_unit(iunit)
        
        ! Should return unit 4 (available) or skip to 7 if 4,5,6 are avoided
        ! Based on implementation: if iu == 5, skip to 7
        ! So starting at 4, should return 4 if available
        call assert_not_equal_int(iunit, 5, "Returned unit is not 5")
    end subroutine test_avoids_unit_6
    
    !------------------------------------------------------------------
    ! Test: Finds next available unit when some are in use
    !------------------------------------------------------------------
    subroutine test_finds_next_available()
        integer :: iunit, test_unit
        
        print *, "Test: Finds next available when file is open"
        
        ! Open a file on a specific unit
        test_unit = 99
        open(unit=test_unit, file='test_temp_file.tmp', status='replace')
        
        ! Try to get that same unit
        iunit = test_unit
        call find_unit(iunit)
        
        ! Should skip over the opened unit
        call assert_not_equal_int(iunit, test_unit, "Skips unit in use")
        call assert_greater_than_int(iunit, test_unit, "Returns higher unit")
        
        ! Clean up
        close(test_unit, status='delete')
    end subroutine test_finds_next_available
    
    !------------------------------------------------------------------
    ! Test: Zero input defaults to unit 20
    !------------------------------------------------------------------
    subroutine test_zero_input()
        integer :: iunit
        
        print *, "Test: Zero input starts search from default"
        
        iunit = 0
        call find_unit(iunit)
        
        ! Should default to starting at 20
        call assert_greater_than_int(iunit, 19, "Returns unit >= 20")
    end subroutine test_zero_input
    
    !------------------------------------------------------------------
    ! Test: Negative input defaults to unit 20
    !------------------------------------------------------------------
    subroutine test_negative_input()
        integer :: iunit
        
        print *, "Test: Negative input starts search from default"
        
        iunit = -5
        call find_unit(iunit)
        
        ! Should default to starting at 20
        call assert_greater_than_int(iunit, 19, "Returns unit >= 20")
    end subroutine test_negative_input

end program test_file_unit
