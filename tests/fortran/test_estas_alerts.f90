! Unit tests for the ESTAS_ALERTS module: the per-timestep diagnostic alerts
! (negative mass / negative concentration) are all counted, but only the first
! ALERT_LIMIT of each kind is printed in full.

program test_estas_alerts
    use ESTAS_ALERTS
    implicit none

    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=========================================="
    print *, "ESTAS alert rate-limit tests"
    print *, "=========================================="
    print *, ""

    call test_parse_limit()
    call test_limit_prints_first_n()
    call test_kinds_counted_separately()
    call test_negative_limit_prints_all()
    call test_zero_limit_counts_but_prints_none()

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

    subroutine check(cond, label)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: label
        if (cond) then
            passed = passed + 1
            print *, "  PASS: ", label
        else
            failed = failed + 1
            print *, "  FAIL: ", label
        end if
    end subroutine check

    subroutine test_parse_limit()
        print *, "Test: ESTAS_ALERT_LIMIT parsing"
        call check(PARSE_ALERT_LIMIT("7", 20) == 7, "a number sets the limit")
        call check(PARSE_ALERT_LIMIT("-1", 20) == -1, "a negative number is kept (print all)")
        call check(PARSE_ALERT_LIMIT("", 20) == 20, "empty keeps the default")
        call check(PARSE_ALERT_LIMIT("abc", 20) == 20, "garbage keeps the default")
    end subroutine test_parse_limit

    subroutine test_limit_prints_first_n()
        logical :: show(5)
        integer :: i
        print *, "Test: limit 2 prints the first two of five"
        call SET_ALERT_LIMIT(2)
        do i = 1, 5
            call COUNT_ALERT(ALERT_KIND_NEG_CONC, show(i))
        end do
        call check(all(show .eqv. [.true., .true., .false., .false., .false.]), "first two shown")
        call check(ALERT_COUNT(ALERT_KIND_NEG_CONC) == 5_8, "all five counted")
    end subroutine test_limit_prints_first_n

    subroutine test_kinds_counted_separately()
        logical :: show
        print *, "Test: kinds have their own counters and limits"
        call SET_ALERT_LIMIT(1)
        call COUNT_ALERT(ALERT_KIND_NEG_MASS, show)
        call check(show, "first negative-mass alert shown")
        call COUNT_ALERT(ALERT_KIND_NEG_CONC, show)
        call check(show, "first negative-conc alert shown despite the negative-mass one")
        call check(ALERT_COUNT(ALERT_KIND_RK2_NEG_MASS) == 0_8, "untouched kind stays at zero")
    end subroutine test_kinds_counted_separately

    subroutine test_negative_limit_prints_all()
        logical :: show(4)
        integer :: i
        print *, "Test: a negative limit prints every alert"
        call SET_ALERT_LIMIT(-1)
        do i = 1, 4
            call COUNT_ALERT(ALERT_KIND_NEG_MASS, show(i))
        end do
        call check(all(show), "all shown")
        call check(ALERT_COUNT(ALERT_KIND_NEG_MASS) == 4_8, "all counted")
    end subroutine test_negative_limit_prints_all

    subroutine test_zero_limit_counts_but_prints_none()
        logical :: show
        print *, "Test: limit 0 prints none but counts"
        call SET_ALERT_LIMIT(0)
        call COUNT_ALERT(ALERT_KIND_RK2_NEG_MASS, show)
        call check(.not. show, "not shown")
        call check(ALERT_COUNT(ALERT_KIND_RK2_NEG_MASS) == 1_8, "counted")
    end subroutine test_zero_limit_counts_but_prints_none

end program test_estas_alerts
