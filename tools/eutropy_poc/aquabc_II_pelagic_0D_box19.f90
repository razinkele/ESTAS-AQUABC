! -----------------------------------------------------------------------------
! AQUABC 0-D driver for the EUTROPY box-19 proof of concept.
!
! Unlike the stock aquabc_II_pelagic_0D.f90 (which hardcodes ICs/BCs/forcing),
! this variant reads them from the files produced by tools/eutropy_to_aquabc.py:
!     tools/eutropy_poc/box19/initial_conditions.txt      (32 AQUABC ICs)
!     tools/eutropy_poc/box19/boundary_concentrations.txt (32 AQUABC BCs)
!     tools/eutropy_poc/box19/forcing_box19.csv           (daily forcing)
! and steps the AQUABC pelagic kinetics through the 2012-2017 forcing, writing
! one OUTPUT row per day for comparison against EUTROPY and box-19 observations.
!
! RUN FROM THE REPOSITORY ROOT (paths below are repo-root-relative).
! -----------------------------------------------------------------------------

program aquabc_II_pelagic_0D_box19

    implicit none

    integer, parameter :: NSTATE = 32
    integer, parameter :: NDRIVE = 10

    ! PoC transport: relaxation toward boundary water over a residence time.
    ! The real box 19 exchanges through the full flux network; this is a
    ! deliberate single-box simplification. Edit to change flushing strength.
    double precision, parameter :: RESIDENCE_TIME_DAYS = 30.0D0

    character(len=256) :: DIR, OUT_FILE, CONST_FILE, arg3
    integer :: nkn, ndays, d, s
    integer :: NSTEPS_PER_DAY                          ! sub-daily kinetics steps
    double precision :: TIME, TIME_STEP

    double precision :: IC(NSTATE), BC(NSTATE)
    double precision, allocatable :: FORC(:,:)          ! (ndays, NDRIVE)
    double precision, allocatable :: STATE_VARIABLES(:,:)
    double precision, allocatable :: STATE_VARIABLES_OLD(:,:)
    double precision, allocatable :: DRIVING_FUNCTIONS(:,:)
    double precision, allocatable :: PH(:)
    double precision, allocatable :: SEDIMENT_FLUXES(:,:)
    integer,          allocatable :: SURFACE_BOXES(:)

    nkn = 1

    ! Command-line arguments (all optional):
    !   1: input directory (default box19/)
    !   2: constants file  (default const_CL.txt)
    !   3: sub-daily steps (default 240; lower = faster, for calibration)
    call get_command_argument(1, DIR)
    if (len_trim(DIR) == 0) DIR = 'tools/eutropy_poc/box19/'
    if (DIR(len_trim(DIR):len_trim(DIR)) /= '/') DIR = trim(DIR)//'/'
    OUT_FILE = trim(DIR)//'OUTPUT_aquabc.csv'

    call get_command_argument(2, CONST_FILE)
    if (len_trim(CONST_FILE) == 0) CONST_FILE = &
        'SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/data/const_CL.txt'

    call get_command_argument(3, arg3)
    if (len_trim(arg3) == 0) then
        NSTEPS_PER_DAY = 240
    else
        read(arg3, *) NSTEPS_PER_DAY
    end if

    call read_state_file(trim(DIR)//'initial_conditions.txt', IC, NSTATE)
    call read_state_file(trim(DIR)//'boundary_concentrations.txt', BC, NSTATE)
    call count_data_rows(trim(DIR)//'forcing_box19.csv', ndays)
    allocate(FORC(ndays, NDRIVE))
    call read_forcing(trim(DIR)//'forcing_box19.csv', FORC, ndays, NDRIVE)

    write(6,*) 'box-19 PoC: ', ndays, ' forcing days, ', &
               NSTEPS_PER_DAY, ' steps/day'

    allocate(STATE_VARIABLES     (nkn, NSTATE))
    allocate(STATE_VARIABLES_OLD (nkn, NSTATE))
    allocate(DRIVING_FUNCTIONS   (nkn, NDRIVE))
    allocate(PH                  (nkn))
    allocate(SEDIMENT_FLUXES     (nkn, NSTATE))
    allocate(SURFACE_BOXES       (nkn))

    STATE_VARIABLES(1, :) = IC(:)
    PH              = 0.0D0
    SEDIMENT_FLUXES = 0.0D0
    SURFACE_BOXES   = 1

    TIME      = 1.0D0
    TIME_STEP = 1.0D0 / dble(NSTEPS_PER_DAY)

    call aquabc_init(nkn, NSTATE, NDRIVE, SURFACE_BOXES)
    call aquabc_read_constants(trim(CONST_FILE))

    call initialize_output(trim(OUT_FILE))
    call write_output(TIME, NSTATE, STATE_VARIABLES)

    do d = 1, ndays

        DRIVING_FUNCTIONS(1, :) = FORC(d, :)

        do s = 1, NSTEPS_PER_DAY
            TIME = TIME + TIME_STEP
            STATE_VARIABLES_OLD = STATE_VARIABLES

            call aquabc_run          ( &
                 TIME                , &
                 TIME_STEP           , &
                 STATE_VARIABLES     , &
                 PH                  , &
                 DRIVING_FUNCTIONS   , &
                 SEDIMENT_FLUXES     )

            ! Flushing toward boundary water (residence-time relaxation)
            STATE_VARIABLES(1, :) = STATE_VARIABLES(1, :) + &
                (BC(:) - STATE_VARIABLES_OLD(1, :)) * &
                (TIME_STEP / RESIDENCE_TIME_DAYS)
        end do

        call write_output(TIME, NSTATE, STATE_VARIABLES)
        if (mod(d, 365) == 0) write(6,*) '  simulated day', d
    end do

    call finalize_output()
    write(6,*) 'box-19 PoC finished:', ndays, 'days ->', OUT_FILE

    deallocate(STATE_VARIABLES, STATE_VARIABLES_OLD, DRIVING_FUNCTIONS)
    deallocate(PH, SEDIMENT_FLUXES, SURFACE_BOXES, FORC)

end program

!=======================================================================

! Read a "<index> <value> ! <name>" state file into vec(1:n).
subroutine read_state_file(fname, vec, n)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(in) :: n
    double precision, intent(out) :: vec(n)
    character(len=256) :: line
    integer :: u, ios, idx
    double precision :: val

    vec = 0.0D0
    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        write(6,*) 'ERROR: cannot open ', trim(fname)
        stop 1
    end if
    do
        read(u, '(a)', iostat=ios) line
        if (ios /= 0) exit
        line = adjustl(line)
        if (len_trim(line) == 0) cycle
        if (line(1:1) == '#') cycle
        read(line, *, iostat=ios) idx, val       ! ignores trailing "! name"
        if (ios /= 0) cycle
        if (idx >= 1 .and. idx <= n) vec(idx) = val
    end do
    close(u)
end subroutine

! Count data rows (excluding the header) in the forcing CSV.
subroutine count_data_rows(fname, n)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(out) :: n
    character(len=512) :: line
    integer :: u, ios
    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        write(6,*) 'ERROR: cannot open ', trim(fname)
        stop 1
    end if
    n = 0
    read(u, '(a)', iostat=ios) line                 ! header
    do
        read(u, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        n = n + 1
    end do
    close(u)
end subroutine

! Fill forc(ndays,ndrive) from the forcing CSV (day,date,+ndrive reals).
subroutine read_forcing(fname, forc, ndays, ndrive)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(in) :: ndays, ndrive
    double precision, intent(out) :: forc(ndays, ndrive)
    character(len=512) :: line
    character(len=32)  :: cdate
    integer :: u, ios, iday, k, i
    double precision :: row(ndrive)

    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        write(6,*) 'ERROR: cannot open ', trim(fname)
        stop 1
    end if
    read(u, '(a)', iostat=ios) line                 ! header
    i = 0
    do
        read(u, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        do k = 1, len_trim(line)                    ! CSV -> space separated
            if (line(k:k) == ',') line(k:k) = ' '
        end do
        read(line, *, iostat=ios) iday, cdate, (row(k), k = 1, ndrive)
        if (ios /= 0) cycle
        if (i >= ndays) exit
        i = i + 1
        forc(i, :) = row(:)
    end do
    close(u)
end subroutine

!=======================================================================

subroutine initialize_output(file)
    implicit none
    character*(*) file
    open(unit=10, file=file, status='UNKNOWN')
    write(unit=10, fmt='(a15,10a42,a21)') &
               '           TIME',  &
          ',                NH4N,                NO3N', &
          ',                PO4P,                DOXY', &
          ',                DIAC,                ZOOC', &
          ',                ZOON,                ZOOP', &
          ',                DETC,                DETN', &
          ',                DETP,                 DOC', &
          ',                 DON,                 DOP', &
          ',          NOFIX_CYNC,                 OPA', &
          ',    DISSOLVED_SILICA,  PARTICULATE_SILICA', &
          ',            FIX_CYNC,                 DIC', &
          ',          ALKALINITY'
end subroutine

subroutine write_output(time, nstate, STATE_VARIABLES)
    implicit none
    double precision time
    integer nstate
    double precision STATE_VARIABLES(1, nstate)
    write(unit=10, fmt='(f15.6,21(a1,f20.10))') &
          TIME, &
          ',', STATE_VARIABLES(1,  1), ',', STATE_VARIABLES(1,  2), &
          ',', STATE_VARIABLES(1,  3), ',', STATE_VARIABLES(1,  4), &
          ',', STATE_VARIABLES(1,  5), ',', STATE_VARIABLES(1,  6), &
          ',', STATE_VARIABLES(1,  7), ',', STATE_VARIABLES(1,  8), &
          ',', STATE_VARIABLES(1,  9), ',', STATE_VARIABLES(1, 10), &
          ',', STATE_VARIABLES(1, 11), ',', STATE_VARIABLES(1, 12), &
          ',', STATE_VARIABLES(1, 13), ',', STATE_VARIABLES(1, 14), &
          ',', STATE_VARIABLES(1, 15), ',', STATE_VARIABLES(1, 16), &
          ',', STATE_VARIABLES(1, 17), ',', STATE_VARIABLES(1, 18), &
          ',', STATE_VARIABLES(1, 19), ',', STATE_VARIABLES(1, 20), &
          ',', STATE_VARIABLES(1, 21)
end subroutine

subroutine finalize_output
    implicit none
    close(10)
end subroutine
