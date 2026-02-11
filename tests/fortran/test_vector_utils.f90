! Unit tests for VECTOR_MATRIX_UTILS module
! Tests utility functions for vector/matrix operations
!
! Functions tested:
! - SAME_ELEMENTS_INETEGER_VECTOR: Check if all elements are identical

program test_vector_utils
    use AQUABC_II_GLOBAL
    use VECTOR_MATRIX_UTILS
    implicit none
    
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "VECTOR_MATRIX_UTILS Module Unit Tests"
    print *, "=========================================="
    print *, ""
    
    ! Run all tests
    call test_same_elements_all_same()
    call test_same_elements_different()
    call test_same_elements_single()
    call test_same_elements_zeros()
    call test_same_elements_negatives()
    
    ! Print summary
    print *, ""
    print *, "=========================================="
    print *, "Test Summary"
    print *, "=========================================="
    print '(A,I3,A)', " Passed: ", passed, " tests"
    print '(A,I3,A)', " Failed: ", failed, " tests"
    print *, "=========================================="
    
    if (failed > 0) then
        print *, "SOME TESTS FAILED!"
        stop 1
    else
        print *, "ALL TESTS PASSED!"
    end if

contains

    subroutine assert_equals_int(value, expected, test_name)
        integer, intent(in) :: value, expected
        character(len=*), intent(in) :: test_name
        
        if (value == expected) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,I5,A,I5)', "    Got: ", value, " Expected: ", expected
            failed = failed + 1
        end if
    end subroutine assert_equals_int

    ! Test with all same elements
    subroutine test_same_elements_all_same()
        real(kind = DBL_PREC), dimension(5) :: vec
        integer :: result
        
        print *, "Test: All elements same returns 1"
        
        vec = (/ 3.14D0, 3.14D0, 3.14D0, 3.14D0, 3.14D0 /)
        result = SAME_ELEMENTS_INETEGER_VECTOR(vec)
        
        call assert_equals_int(result, 1, "All same elements returns 1 (unique)")
        
    end subroutine test_same_elements_all_same
    
    ! Test with different elements
    subroutine test_same_elements_different()
        real(kind = DBL_PREC), dimension(5) :: vec
        integer :: result
        
        print *, "Test: Different elements returns 0"
        
        vec = (/ 1.0D0, 2.0D0, 3.0D0, 4.0D0, 5.0D0 /)
        result = SAME_ELEMENTS_INETEGER_VECTOR(vec)
        
        call assert_equals_int(result, 0, "Different elements returns 0 (not unique)")
        
    end subroutine test_same_elements_different
    
    ! Test with single element
    subroutine test_same_elements_single()
        real(kind = DBL_PREC), dimension(1) :: vec
        integer :: result
        
        print *, "Test: Single element returns 1"
        
        vec = (/ 42.0D0 /)
        result = SAME_ELEMENTS_INETEGER_VECTOR(vec)
        
        call assert_equals_int(result, 1, "Single element returns 1 (unique)")
        
    end subroutine test_same_elements_single
    
    ! Test with all zeros
    subroutine test_same_elements_zeros()
        real(kind = DBL_PREC), dimension(4) :: vec
        integer :: result
        
        print *, "Test: All zeros returns 1"
        
        vec = (/ 0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
        result = SAME_ELEMENTS_INETEGER_VECTOR(vec)
        
        call assert_equals_int(result, 1, "All zeros returns 1 (unique)")
        
    end subroutine test_same_elements_zeros
    
    ! Test with negative values
    subroutine test_same_elements_negatives()
        real(kind = DBL_PREC), dimension(3) :: vec
        integer :: result
        
        print *, "Test: Same negative values returns 1"
        
        vec = (/ -5.5D0, -5.5D0, -5.5D0 /)
        result = SAME_ELEMENTS_INETEGER_VECTOR(vec)
        
        call assert_equals_int(result, 1, "Same negative values returns 1 (unique)")
        
    end subroutine test_same_elements_negatives

end program test_vector_utils
