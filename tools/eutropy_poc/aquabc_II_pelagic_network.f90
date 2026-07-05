! -----------------------------------------------------------------------------
! AQUABC 29-box network driver for the EUTROPY Curonian Lagoon PoC.
!
! Runs the AQUABC pelagic kinetics over all 29 boxes (vectorised) and couples
! them with EUTROPY's box-to-box advective transport each step:
!   C_b += ( sum_inflow  Q/V_b * C_source  -  sum_outflow Q/V_b * C_b ) * dt
! Kinetics and transport are operator-split from the start-of-step state.
!
! Inputs (from tools/eutropy_poc/net/, produced by eutropy_to_aquabc_network.py):
!   initial_conditions.csv  depths.csv  volumes.csv
!   forcing_{temp,salt,light,fday}.csv  links.csv  flux.csv  boundary_mean.csv
! Args (optional): 1 constants file, 2 sub-daily steps (default 48).
! Output: net/OUTPUT_cyn_C.csv  (day + CYN_C mg C/L per box).
! RUN FROM THE REPOSITORY ROOT.
! -----------------------------------------------------------------------------

program aquabc_II_pelagic_network

    implicit none

    integer, parameter :: NSTATE = 32, NDRIVE = 10, NBOX = 29, NBND = 5
    integer, parameter :: CYN_COL = 15
    character(*), parameter :: DIR = 'tools/eutropy_poc/net/'

    character(len=256) :: CONST_FILE, arg2
    integer :: nlink, ndays, d, s, b, L, nsteps
    double precision :: TIME, TIME_STEP, Q, VB

    double precision :: IC(NBOX, NSTATE), BND(NBND, NSTATE), DEPTH(NBOX)
    double precision, allocatable :: VOL(:,:), FT(:,:), FS(:,:), FL(:,:), FD(:,:)
    double precision, allocatable :: FLUX(:,:)
    integer,          allocatable :: LFROM(:), LTO(:)
    double precision, allocatable :: STATE(:,:), STATE_OLD(:,:), DRIVING(:,:)
    double precision, allocatable :: PH(:), SED(:,:), INFLOW(:), OUTFLOW(:)
    integer,          allocatable :: SURFACE_BOXES(:)

    call get_command_argument(1, CONST_FILE)
    if (len_trim(CONST_FILE) == 0) CONST_FILE = &
        'tools/eutropy_poc/box19_cyn/const_cyn_calibrated.txt'
    call get_command_argument(2, arg2)
    if (len_trim(arg2) == 0) then
        nsteps = 48
    else
        read(arg2, *) nsteps
    end if

    ! ---- read inputs ----
    call read_box_matrix(DIR//'initial_conditions.csv', IC, NBOX, NSTATE)
    call read_box_matrix(DIR//'boundary_mean.csv', BND, NBND, NSTATE)
    call read_depths(DIR//'depths.csv', DEPTH, NBOX)

    call count_rows(DIR//'volumes.csv', ndays)
    allocate(VOL(ndays, NBOX), FT(ndays, NBOX), FS(ndays, NBOX))
    allocate(FL(ndays, NBOX), FD(ndays, NBOX))
    call read_day_matrix(DIR//'volumes.csv',       VOL, ndays, NBOX)
    call read_day_matrix(DIR//'forcing_temp.csv',  FT,  ndays, NBOX)
    call read_day_matrix(DIR//'forcing_salt.csv',  FS,  ndays, NBOX)
    call read_day_matrix(DIR//'forcing_light.csv', FL,  ndays, NBOX)
    call read_day_matrix(DIR//'forcing_fday.csv',  FD,  ndays, NBOX)

    call count_rows(DIR//'links.csv', nlink)
    allocate(LFROM(nlink), LTO(nlink), FLUX(ndays, nlink))
    call read_links(DIR//'links.csv', LFROM, LTO, nlink)
    call read_day_matrix(DIR//'flux.csv', FLUX, ndays, nlink)

    write(6,*) 'network: ', NBOX, ' boxes, ', nlink, ' links, ', &
               ndays, ' days, ', nsteps, ' steps/day'

    allocate(STATE(NBOX, NSTATE), STATE_OLD(NBOX, NSTATE))
    allocate(DRIVING(NBOX, NDRIVE), PH(NBOX), SED(NBOX, NSTATE))
    allocate(SURFACE_BOXES(NBOX), INFLOW(NSTATE), OUTFLOW(NSTATE))
    STATE = IC
    PH = 0.0D0
    SED = 0.0D0
    SURFACE_BOXES = 1

    TIME = 1.0D0
    TIME_STEP = 1.0D0 / dble(nsteps)

    call aquabc_init(NBOX, NSTATE, NDRIVE, SURFACE_BOXES)
    call aquabc_read_constants(trim(CONST_FILE))
    call init_output(DIR//'OUTPUT_cyn_C.csv', NBOX)
    call write_output(TIME, STATE, NBOX, NSTATE, CYN_COL)

    do d = 1, ndays
        ! per-box forcing for this day
        do b = 1, NBOX
            DRIVING(b, 1)  = FT(d, b)
            DRIVING(b, 2)  = FS(d, b)
            DRIVING(b, 3)  = FL(d, b)
            DRIVING(b, 4)  = FD(d, b)
            DRIVING(b, 5)  = FT(d, b)      ! air temp ~ water temp
            DRIVING(b, 6)  = 4.0D0         ! wind speed
            DRIVING(b, 7)  = 0.0D0         ! elevation
            DRIVING(b, 8)  = DEPTH(b)
            DRIVING(b, 9)  = 0.6D0         ! background light extinction
            DRIVING(b, 10) = 0.0D0         ! ice cover
        end do

        do s = 1, nsteps
            TIME = TIME + TIME_STEP
            STATE_OLD = STATE

            call aquabc_run(TIME, TIME_STEP, STATE, PH, DRIVING, SED)

            ! advective transport (from start-of-step concentrations)
            do b = 1, NBOX
                VB = max(VOL(d, b), 1.0D0)
                INFLOW = 0.0D0
                OUTFLOW = 0.0D0
                do L = 1, nlink
                    Q = FLUX(d, L)
                    if (LTO(L) == b) then
                        if (LFROM(L) < 0) then
                            INFLOW = INFLOW + (Q / VB) * BND(-LFROM(L), :)
                        else
                            INFLOW = INFLOW + (Q / VB) * STATE_OLD(LFROM(L), :)
                        end if
                    end if
                    if (LFROM(L) == b) then
                        OUTFLOW = OUTFLOW + (Q / VB) * STATE_OLD(b, :)
                    end if
                end do
                STATE(b, :) = STATE(b, :) + (INFLOW - OUTFLOW) * TIME_STEP
            end do
        end do

        call write_output(TIME, STATE, NBOX, NSTATE, CYN_COL)
        if (mod(d, 365) == 0) write(6,*) '  day', d
    end do

    call finalize_output()
    write(6,*) 'network finished:', ndays, 'days ->', DIR//'OUTPUT_cyn_C.csv'

end program

!=======================================================================

subroutine count_rows(fname, n)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(out) :: n
    character(len=4096) :: line
    integer :: u, ios
    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then; write(6,*) 'ERROR open ', trim(fname); stop 1; end if
    n = 0
    read(u, '(a)', iostat=ios) line       ! header
    do
        read(u, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) > 0) n = n + 1
    end do
    close(u)
end subroutine

! Read "day,c1..cncol" rows (day column ignored) into mat(nrow,ncol) in order.
subroutine read_day_matrix(fname, mat, nrow, ncol)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(in) :: nrow, ncol
    double precision, intent(out) :: mat(nrow, ncol)
    character(len=8192) :: line
    integer :: u, ios, i, k, iday
    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then; write(6,*) 'ERROR open ', trim(fname); stop 1; end if
    read(u, '(a)', iostat=ios) line       ! header
    i = 0
    do
        read(u, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        do k = 1, len_trim(line)
            if (line(k:k) == ',') line(k:k) = ' '
        end do
        i = i + 1
        if (i > nrow) exit
        read(line, *, iostat=ios) iday, (mat(i, k), k = 1, ncol)
    end do
    close(u)
end subroutine

! Read "box,v1..v32" rows into mat(nrow,ncol) indexed by box.
subroutine read_box_matrix(fname, mat, nrow, ncol)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(in) :: nrow, ncol
    double precision, intent(out) :: mat(nrow, ncol)
    character(len=8192) :: line
    integer :: u, ios, k, idx
    mat = 0.0D0
    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then; write(6,*) 'ERROR open ', trim(fname); stop 1; end if
    read(u, '(a)', iostat=ios) line
    do
        read(u, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        do k = 1, len_trim(line)
            if (line(k:k) == ',') line(k:k) = ' '
        end do
        read(line, *, iostat=ios) idx, (mat(idx, k), k = 1, ncol)
    end do
    close(u)
end subroutine

subroutine read_depths(fname, depth, n)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(in) :: n
    double precision, intent(out) :: depth(n)
    character(len=256) :: line
    integer :: u, ios, idx, k
    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then; write(6,*) 'ERROR open ', trim(fname); stop 1; end if
    read(u, '(a)', iostat=ios) line
    do
        read(u, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        do k = 1, len_trim(line)
            if (line(k:k) == ',') line(k:k) = ' '
        end do
        read(line, *, iostat=ios) idx, depth(idx)
    end do
    close(u)
end subroutine

subroutine read_links(fname, lfrom, lto, n)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(in) :: n
    integer, intent(out) :: lfrom(n), lto(n)
    character(len=256) :: line
    integer :: u, ios, id, k
    open(newunit=u, file=fname, status='old', action='read', iostat=ios)
    if (ios /= 0) then; write(6,*) 'ERROR open ', trim(fname); stop 1; end if
    read(u, '(a)', iostat=ios) line
    do
        read(u, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        do k = 1, len_trim(line)
            if (line(k:k) == ',') line(k:k) = ' '
        end do
        read(line, *, iostat=ios) id, lfrom(id), lto(id)
    end do
    close(u)
end subroutine

!=======================================================================

subroutine init_output(fname, nbox)
    implicit none
    character(*), intent(in) :: fname
    integer, intent(in) :: nbox
    integer :: b
    open(unit=20, file=fname, status='UNKNOWN')
    write(20, '(a)', advance='no') 'day'
    do b = 1, nbox
        write(20, '(a,i0)', advance='no') ',box', b
    end do
    write(20, '(a)') ''
end subroutine

subroutine write_output(time, state, nbox, nstate, col)
    implicit none
    double precision, intent(in) :: time, state(nbox, nstate)
    integer, intent(in) :: nbox, nstate, col
    integer :: b
    write(20, '(f0.3)', advance='no') time
    do b = 1, nbox
        write(20, '(a,f0.6)', advance='no') ',', state(b, col)
    end do
    write(20, '(a)') ''
end subroutine

subroutine finalize_output()
    implicit none
    close(20)
end subroutine
