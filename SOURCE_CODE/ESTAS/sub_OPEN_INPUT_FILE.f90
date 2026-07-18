! ---------------------------------------------------------------------------
! Open an existing input file for reading, aborting with a clear, actionable
! message if it cannot be opened (missing or unreadable) instead of the opaque
! Fortran runtime error that a bare open(status='OLD') produces on a bad path.
!
! Centralizes IOSTAT handling for the status='OLD' input opens (TODO 1.9).
! Standalone external subroutine so it is callable from both modules and
! standalone subroutines without introducing a module dependency; it is picked
! up automatically by the build's source glob.
! ---------------------------------------------------------------------------
subroutine OPEN_INPUT_FILE(unit_no, path, description)
    implicit none

    integer,          intent(in) :: unit_no      ! logical unit to open on
    character(len=*), intent(in) :: path         ! full path of the input file
    character(len=*), intent(in) :: description  ! short category, for the message

    integer :: ios

    open(unit = unit_no, file = path, status = 'OLD', iostat = ios)

    if (ios /= 0) then
        write(*, '(a)')    ' ERROR: cannot open ' // trim(description) // ' file:'
        write(*, '(a)')    '   ' // trim(path)
        write(*, '(a, i0)') '   IOSTAT = ', ios
        write(*, '(a)')    '   Check that the path exists and is readable.'
        error stop 'input file open failed'
    end if
end subroutine OPEN_INPUT_FILE
