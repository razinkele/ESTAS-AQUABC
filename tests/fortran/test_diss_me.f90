! Unit tests for CALC_DISS_ME_CONC subroutine
! Tests dissolved metal concentration calculations (dissolution/precipitation kinetics)
!
! This subroutine calculates:
! - Dissolved metal concentration at end of timestep
! - Average dissolved concentration over timestep
! For metals like Fe_II, Fe_III, Ca, Mg, etc.

program test_diss_me
    implicit none
    
    integer, parameter :: DBL_PREC = selected_real_kind(15, 307)
    
    integer :: passed, failed
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "=========================================="
    print *, "CALC_DISS_ME_CONC Unit Tests"
    print *, "=========================================="
    print *, ""
    
    ! Run all tests
    call test_equilibrium_state()
    call test_undersaturated_dissolution()
    call test_oversaturated_precipitation()
    call test_array_handling()
    call test_small_timestep()
    call test_large_timestep()
    
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

    subroutine assert_approx_equal(value, expected, tol, test_name)
        real(kind=DBL_PREC), intent(in) :: value, expected, tol
        character(len=*), intent(in) :: test_name
        
        if (abs(value - expected) <= tol) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5,A,E12.5)', "    Got: ", value, " Expected: ", expected
            failed = failed + 1
        end if
    end subroutine assert_approx_equal
    
    subroutine assert_greater_than(value1, value2, test_name)
        real(kind=DBL_PREC), intent(in) :: value1, value2
        character(len=*), intent(in) :: test_name
        
        if (value1 > value2) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5,A,E12.5)', "    Expected ", value1, " > ", value2
            failed = failed + 1
        end if
    end subroutine assert_greater_than
    
    subroutine assert_less_than(value1, value2, test_name)
        real(kind=DBL_PREC), intent(in) :: value1, value2
        character(len=*), intent(in) :: test_name
        
        if (value1 < value2) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5,A,E12.5)', "    Expected ", value1, " < ", value2
            failed = failed + 1
        end if
    end subroutine assert_less_than
    
    subroutine assert_in_range(value, min_val, max_val, test_name)
        real(kind=DBL_PREC), intent(in) :: value, min_val, max_val
        character(len=*), intent(in) :: test_name
        
        if (value >= min_val .and. value <= max_val) then
            print '(A,A)', "  PASS: ", test_name
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", test_name
            print '(A,E12.5,A,E12.5,A,E12.5)', "    Value: ", value, " not in [", min_val, ", ", max_val, "]"
            failed = failed + 1
        end if
    end subroutine assert_in_range

    ! Test when already at equilibrium (no change expected)
    subroutine test_equilibrium_state()
        integer, parameter :: nkn = 1, nlayers = 1
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: k_DISS_ME, DISS_ME_END, DISS_ME_AVG
        real(kind=DBL_PREC) :: t
        
        print *, "Test: Equilibrium state (no change)"
        
        TOT_ME = 5.0D0
        ME_DISS_INIT = 2.0D0    ! Already at equilibrium
        ME_SOLUB_EQ = 2.0D0     ! Equal to initial
        k_DISS_ME = 0.1D0
        t = 1.0D0               ! 1 day timestep
        
        DISS_ME_END = 0.0D0
        DISS_ME_AVG = 0.0D0
        
        call CALC_DISS_ME_CONC(TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ, k_DISS_ME, t, &
                               nkn, nlayers, DISS_ME_END, DISS_ME_AVG)
        
        ! When at equilibrium, concentration should not change
        call assert_approx_equal(DISS_ME_END(1,1), ME_DISS_INIT(1,1), 1.0D-10, &
            "No change when at equilibrium")
        
    end subroutine test_equilibrium_state
    
    ! Test undersaturated case (dissolution should occur)
    subroutine test_undersaturated_dissolution()
        integer, parameter :: nkn = 1, nlayers = 1
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: k_DISS_ME, DISS_ME_END, DISS_ME_AVG
        real(kind=DBL_PREC) :: t
        
        print *, "Test: Undersaturated dissolution"
        
        TOT_ME = 10.0D0
        ME_DISS_INIT = 1.0D0    ! Below equilibrium
        ME_SOLUB_EQ = 5.0D0     ! Equilibrium is higher
        k_DISS_ME = 0.1D0
        t = 1.0D0
        
        DISS_ME_END = 0.0D0
        DISS_ME_AVG = 0.0D0
        
        call CALC_DISS_ME_CONC(TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ, k_DISS_ME, t, &
                               nkn, nlayers, DISS_ME_END, DISS_ME_AVG)
        
        ! Dissolved concentration should increase toward equilibrium
        call assert_greater_than(DISS_ME_END(1,1), ME_DISS_INIT(1,1), &
            "Undersaturated: dissolved concentration increases")
        
        ! Average should be between initial and final
        call assert_in_range(DISS_ME_AVG(1,1), ME_DISS_INIT(1,1), DISS_ME_END(1,1), &
            "Average is between initial and final")
        
    end subroutine test_undersaturated_dissolution
    
    ! Test oversaturated case (precipitation should occur)
    subroutine test_oversaturated_precipitation()
        integer, parameter :: nkn = 1, nlayers = 1
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: k_DISS_ME, DISS_ME_END, DISS_ME_AVG
        real(kind=DBL_PREC) :: t
        
        print *, "Test: Oversaturated precipitation"
        
        TOT_ME = 10.0D0
        ME_DISS_INIT = 8.0D0    ! Above equilibrium
        ME_SOLUB_EQ = 3.0D0     ! Equilibrium is lower
        k_DISS_ME = 0.1D0
        t = 1.0D0
        
        DISS_ME_END = 0.0D0
        DISS_ME_AVG = 0.0D0
        
        call CALC_DISS_ME_CONC(TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ, k_DISS_ME, t, &
                               nkn, nlayers, DISS_ME_END, DISS_ME_AVG)
        
        ! Dissolved concentration should decrease toward equilibrium
        call assert_less_than(DISS_ME_END(1,1), ME_DISS_INIT(1,1), &
            "Oversaturated: dissolved concentration decreases")
        
    end subroutine test_oversaturated_precipitation
    
    ! Test with multi-dimensional arrays
    subroutine test_array_handling()
        integer, parameter :: nkn = 3, nlayers = 2
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: k_DISS_ME, DISS_ME_END, DISS_ME_AVG
        real(kind=DBL_PREC) :: t
        logical :: all_valid
        integer :: i, j
        
        print *, "Test: Multi-dimensional array handling"
        
        ! Set up different conditions across the grid
        TOT_ME = 10.0D0
        ME_DISS_INIT = 2.0D0
        ME_SOLUB_EQ = 5.0D0
        k_DISS_ME = 0.1D0
        t = 1.0D0
        
        DISS_ME_END = 0.0D0
        DISS_ME_AVG = 0.0D0
        
        call CALC_DISS_ME_CONC(TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ, k_DISS_ME, t, &
                               nkn, nlayers, DISS_ME_END, DISS_ME_AVG)
        
        ! All values should be valid (positive)
        all_valid = .true.
        do i = 1, nkn
            do j = 1, nlayers
                if (DISS_ME_END(i,j) <= 0.0D0) all_valid = .false.
            end do
        end do
        
        if (all_valid) then
            print '(A,A)', "  PASS: ", "All array elements have valid results"
            passed = passed + 1
        else
            print '(A,A)', "  FAIL: ", "Some array elements have invalid results"
            failed = failed + 1
        end if
        
    end subroutine test_array_handling
    
    ! Test with small timestep
    subroutine test_small_timestep()
        integer, parameter :: nkn = 1, nlayers = 1
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: k_DISS_ME, DISS_ME_END, DISS_ME_AVG
        real(kind=DBL_PREC) :: t
        
        print *, "Test: Small timestep behavior"
        
        TOT_ME = 10.0D0
        ME_DISS_INIT = 2.0D0
        ME_SOLUB_EQ = 5.0D0
        k_DISS_ME = 0.1D0
        t = 0.001D0   ! Very small timestep (< 2 minutes in days)
        
        DISS_ME_END = 0.0D0
        DISS_ME_AVG = 0.0D0
        
        call CALC_DISS_ME_CONC(TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ, k_DISS_ME, t, &
                               nkn, nlayers, DISS_ME_END, DISS_ME_AVG)
        
        ! With very small timestep, change should be minimal
        call assert_in_range(DISS_ME_END(1,1), ME_DISS_INIT(1,1) * 0.99D0, &
                            ME_DISS_INIT(1,1) * 1.1D0, &
                            "Small timestep: minimal change from initial")
        
    end subroutine test_small_timestep
    
    ! Test with large timestep (approach equilibrium)
    subroutine test_large_timestep()
        integer, parameter :: nkn = 1, nlayers = 1
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ
        real(kind=DBL_PREC), dimension(nkn,nlayers) :: k_DISS_ME, DISS_ME_END, DISS_ME_AVG
        real(kind=DBL_PREC) :: t
        
        print *, "Test: Large timestep approaches equilibrium"
        
        TOT_ME = 10.0D0
        ME_DISS_INIT = 2.0D0
        ME_SOLUB_EQ = 5.0D0
        k_DISS_ME = 1.0D0       ! Fast kinetics
        t = 100.0D0             ! Very long time
        
        DISS_ME_END = 0.0D0
        DISS_ME_AVG = 0.0D0
        
        call CALC_DISS_ME_CONC(TOT_ME, ME_DISS_INIT, ME_SOLUB_EQ, k_DISS_ME, t, &
                               nkn, nlayers, DISS_ME_END, DISS_ME_AVG)
        
        ! With long time and fast kinetics, should approach equilibrium
        call assert_in_range(DISS_ME_END(1,1), ME_SOLUB_EQ(1,1) * 0.8D0, &
                            ME_SOLUB_EQ(1,1) * 1.5D0, &
                            "Large timestep: approaches equilibrium range")
        
    end subroutine test_large_timestep

end program test_diss_me
