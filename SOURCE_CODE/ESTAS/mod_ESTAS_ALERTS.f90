! Rate limiting for ESTAS's per-timestep diagnostic alerts.
!
! The mass checks in mod_PELAGIC_ECOLOGY.f90 and the RK2 solver print a block for
! every negative mass or concentration they meet. A CL29 2012-2022 run meets 14-16
! million of them: printed in full they make a ~9 GB stdout log (a dozen such logs
! filled the host's disk on 2026-11-06) and writing them slows the run about 3x.
!
! Every alert is still counted. Only the first ALERT_LIMIT() of each kind is printed
! in full, one notice marks the cut-off, and ALERT_SUMMARY reports the totals at the
! end of the run. The limit is DEFAULT_LIMIT unless the environment variable
! ESTAS_ALERT_LIMIT sets it; a negative value prints every alert, the behaviour
! before 2026-11. Printing does not touch the model state.
!
! COUNT_ALERT updates shared counters: call it where the alert is printed, inside the
! existing !$omp critical sections.
module ESTAS_ALERTS
    implicit none
    private

    public :: ALERT_KIND_NEG_MASS, ALERT_KIND_NEG_CONC, ALERT_KIND_RK2_NEG_MASS
    public :: PARSE_ALERT_LIMIT, ALERT_LIMIT, SET_ALERT_LIMIT
    public :: COUNT_ALERT, ALERT_COUNT, ALERT_SUMMARY

    integer, parameter :: ALERT_KIND_NEG_MASS     = 1
    integer, parameter :: ALERT_KIND_NEG_CONC     = 2
    integer, parameter :: ALERT_KIND_RK2_NEG_MASS = 3
    integer, parameter :: N_KINDS = 3
    integer, parameter :: DEFAULT_LIMIT = 20
    character(len=32), parameter :: KIND_NAMES(N_KINDS) = [character(len=32) :: &
        'NEGATIVE MASS PREDICTED', 'NEGATIVE CONC AFTER UPDATE', 'RK2: NEGATIVE MASS PREDICTED']

    integer(kind=8), save :: counts(N_KINDS) = 0_8
    integer,         save :: limit = DEFAULT_LIMIT
    logical,         save :: limit_read = .false.

contains

    ! The limit given by text (the value of ESTAS_ALERT_LIMIT), or default_limit when
    ! text is empty or not an integer.
    integer function PARSE_ALERT_LIMIT(text, default_limit)
        character(len=*), intent(in) :: text
        integer,          intent(in) :: default_limit
        integer :: value, ios

        PARSE_ALERT_LIMIT = default_limit
        if (len_trim(text) == 0) return
        read(text, *, iostat=ios) value
        if (ios == 0) PARSE_ALERT_LIMIT = value
    end function PARSE_ALERT_LIMIT

    ! The print limit, read from ESTAS_ALERT_LIMIT on first use.
    integer function ALERT_LIMIT()
        character(len=32) :: env
        integer :: status

        if (.not. limit_read) then
            limit_read = .true.
            call get_environment_variable('ESTAS_ALERT_LIMIT', env, status=status)
            if (status == 0) limit = PARSE_ALERT_LIMIT(env, DEFAULT_LIMIT)
        end if
        ALERT_LIMIT = limit
    end function ALERT_LIMIT

    ! Fix the limit and reset the counters (unit tests; callers wanting a set limit).
    subroutine SET_ALERT_LIMIT(new_limit)
        integer, intent(in) :: new_limit

        limit      = new_limit
        limit_read = .true.
        counts     = 0_8
    end subroutine SET_ALERT_LIMIT

    ! Count one alert of the given kind; show says whether to print its details.
    subroutine COUNT_ALERT(kind_no, show)
        integer, intent(in)  :: kind_no
        logical, intent(out) :: show
        integer :: lim

        lim = ALERT_LIMIT()
        counts(kind_no) = counts(kind_no) + 1_8
        show = (lim < 0) .or. (counts(kind_no) <= int(lim, 8))
        if (lim >= 0 .and. counts(kind_no) == int(lim, 8) + 1_8) then
            write(6, '(3a, i0, a)') ' Further "', trim(KIND_NAMES(kind_no)), &
                '" alerts are counted but not printed (limit ', lim, &
                '; ESTAS_ALERT_LIMIT=-1 prints them all).'
        end if
    end subroutine COUNT_ALERT

    integer(kind=8) function ALERT_COUNT(kind_no)
        integer, intent(in) :: kind_no

        ALERT_COUNT = counts(kind_no)
    end function ALERT_COUNT

    ! Totals of every alert kind, printed once at the end of the run.
    subroutine ALERT_SUMMARY()
        integer :: k

        write(6, '(a, i0, a)') ' ALERT SUMMARY (first ', ALERT_LIMIT(), &
            ' of each kind printed in full; negative = all):'
        do k = 1, N_KINDS
            write(6, '(3x, a, a, i0)') KIND_NAMES(k), ': ', counts(k)
        end do
    end subroutine ALERT_SUMMARY

end module ESTAS_ALERTS
