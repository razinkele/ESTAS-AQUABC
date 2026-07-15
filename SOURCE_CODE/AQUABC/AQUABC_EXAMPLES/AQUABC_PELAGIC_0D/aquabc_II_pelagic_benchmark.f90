! -----------------------------------------------------------------------------
! OpenMP benchmark harness for AQUABC_PELAGIC_KINETICS (TODO 4.1).
!
! Reuses the node-agnostic 0D interface path (aquabc_init / aquabc_run) to call
! the REAL kinetics subroutine + its real per-thread derived-type bundles and
! the real !$omp parallel region over `nkn` homogeneous (replicated) nodes.
!
! Parameters come from environment variables so a single binary can be swept:
!   BENCH_NKN    number of spatial nodes/boxes    (default 100)
!   BENCH_STEPS  timed kinetics calls             (default 500)
! Thread count is controlled by OMP_NUM_THREADS (read via omp_get_max_threads()).
!
! The state is reset to fixed initial conditions before every timed call, so each
! kinetics evaluation does identical work (reproducible timing, no drift/NaN).
! One untimed warm-up call absorbs the one-time internal allocation/param setup.
!
! Emits a single machine-parseable result line to stdout:
!   BENCH_RESULT nkn=<n> threads=<t> steps=<s> seconds=<sec> us_per_step=<us>
! -----------------------------------------------------------------------------

program aquabc_II_pelagic_benchmark

    use omp_lib
    implicit none

    integer :: nkn
    integer :: nstate
    integer :: n_driving_functions

    double precision :: TIME
    double precision :: TIME_STEP

    double precision, allocatable, dimension(:,:) :: STATE_VARIABLES
    double precision, allocatable, dimension(:,:) :: STATE_VARIABLES_IC
    double precision, allocatable, dimension(:,:) :: DRIVING_FUNCTIONS
    double precision, allocatable, dimension(:)   :: PH
    double precision, allocatable, dimension(:,:) :: SEDIMENT_FLUXES
    integer         , allocatable, dimension(:)   :: SURFACE_BOXES

    integer :: NUM_TIME_STEPS
    integer :: i
    integer :: nthreads
    double precision :: t0, t1, elapsed, us_per_step

    ! ------------------------------------------------------------------------
    ! Read benchmark parameters from the environment (with defaults)
    ! ------------------------------------------------------------------------
    nkn            = read_env_int("BENCH_NKN",   100)
    NUM_TIME_STEPS = read_env_int("BENCH_STEPS", 500)

    nstate              = 32
    n_driving_functions = 10

    TIME      = 1.0D0
    TIME_STEP = 1.0D0 / 24.0D0

    ! ------------------------------------------------------------------------
    ! Allocate + initialise (broadcast to all nkn nodes via array assignment)
    ! ------------------------------------------------------------------------
    allocate(STATE_VARIABLES    (nkn, nstate))
    allocate(STATE_VARIABLES_IC (nkn, nstate))
    allocate(DRIVING_FUNCTIONS  (nkn, n_driving_functions))
    allocate(PH                 (nkn))
    allocate(SEDIMENT_FLUXES    (nkn, nstate))
    allocate(SURFACE_BOXES      (nkn))

    STATE_VARIABLES = 0.0D0
    PH              = 0.0D0
    SEDIMENT_FLUXES = 0.0D0
    SURFACE_BOXES   = 1

    ! Initial conditions (same representative values as the 0D example)
    STATE_VARIABLES(:, 1) = 0.05D0
    STATE_VARIABLES(:, 2) = 0.50D0
    STATE_VARIABLES(:, 3) = 0.047D0
    STATE_VARIABLES(:, 4) = 14.0D0
    STATE_VARIABLES(:, 5) = 1.75D0
    STATE_VARIABLES(:, 6) = 0.020D0
    STATE_VARIABLES(:, 7) = 0.000D0
    STATE_VARIABLES(:, 8) = 0.0000D0
    STATE_VARIABLES(:, 9) = 6.5D0
    STATE_VARIABLES(:,10) = 1.0D0
    STATE_VARIABLES(:,11) = 0.015D0
    STATE_VARIABLES(:,12) = 8.0D0
    STATE_VARIABLES(:,13) = 1.5D0
    STATE_VARIABLES(:,14) = 0.025D0
    STATE_VARIABLES(:,15) = 0.06D0
    STATE_VARIABLES(:,16) = 0.022D0
    STATE_VARIABLES(:,17) = 3.0D0
    STATE_VARIABLES(:,18) = 1.5D0
    STATE_VARIABLES(:,19) = 0.00D0
    STATE_VARIABLES(:,20) = 0.0027D0
    STATE_VARIABLES(:,21) = 0.0027D0
    STATE_VARIABLES(:,22) = 0.23D0
    STATE_VARIABLES(:,23) = 0.55D0
    STATE_VARIABLES(:,24) = 0.1D0
    STATE_VARIABLES(:,25) = 0.1D0
    STATE_VARIABLES(:,26) = 70.0D0
    STATE_VARIABLES(:,27) = 15.0D0
    STATE_VARIABLES(:,28) = 1.35D0
    STATE_VARIABLES(:,29) = 0.0D0
    STATE_VARIABLES(:,30) = 0.0D0
    STATE_VARIABLES(:,31) = 0.0D0
    STATE_VARIABLES(:,32) = 8.0D0

    ! Driving functions (fixed in time)
    DRIVING_FUNCTIONS(:, 1) =  14.0D0   ! Water temperature [C]
    DRIVING_FUNCTIONS(:, 2) =   2.0D0   ! Salinity [psu]
    DRIVING_FUNCTIONS(:, 3) = 200.0D0   ! Available light [W/m**2]
    DRIVING_FUNCTIONS(:, 4) =   0.5D0   ! Fraction of day [0-1]
    DRIVING_FUNCTIONS(:, 5) =  12.0D0   ! Air temperature [C]
    DRIVING_FUNCTIONS(:, 6) =   3.0D0   ! Wind speed [m/s]
    DRIVING_FUNCTIONS(:, 7) =   2.0D0   ! Elevation [m]
    DRIVING_FUNCTIONS(:, 8) =   3.6D0   ! Depth [m]
    DRIVING_FUNCTIONS(:, 9) =   0.1D0   ! Background light extinction [1/m]
    DRIVING_FUNCTIONS(:,10) =   0.0D0   ! Ice cover [0-1]

    STATE_VARIABLES_IC = STATE_VARIABLES

    call aquabc_init(nkn, nstate, n_driving_functions, SURFACE_BOXES)
    call aquabc_read_constants('data/const_CL.txt')

    nthreads = 1
    nthreads = omp_get_max_threads()

    ! ------------------------------------------------------------------------
    ! Warm-up (untimed): absorbs the one-time internal allocation + param setup
    ! ------------------------------------------------------------------------
    STATE_VARIABLES = STATE_VARIABLES_IC
    call aquabc_run(TIME, TIME_STEP, STATE_VARIABLES, PH, DRIVING_FUNCTIONS, SEDIMENT_FLUXES)

    ! ------------------------------------------------------------------------
    ! Timed loop: identical kinetics evaluation NUM_TIME_STEPS times
    ! ------------------------------------------------------------------------
    t0 = omp_get_wtime()
    do i = 1, NUM_TIME_STEPS
        STATE_VARIABLES = STATE_VARIABLES_IC
        call aquabc_run(TIME, TIME_STEP, STATE_VARIABLES, PH, DRIVING_FUNCTIONS, SEDIMENT_FLUXES)
    end do
    t1 = omp_get_wtime()

    elapsed     = t1 - t0
    us_per_step = 1.0D6 * elapsed / dble(NUM_TIME_STEPS)

    write(6, '(A,I0,A,I0,A,I0,A,F0.6,A,F0.3)') &
        'BENCH_RESULT nkn=', nkn, ' threads=', nthreads, ' steps=', NUM_TIME_STEPS, &
        ' seconds=', elapsed, ' us_per_step=', us_per_step

contains

    integer function read_env_int(name, default_val) result(val)
        character(len=*), intent(in) :: name
        integer,          intent(in) :: default_val
        character(len=64) :: buf
        integer :: ln, st, ios
        val = default_val
        call get_environment_variable(name, buf, ln, st)
        if (st == 0 .and. ln > 0) then
            read(buf, *, iostat=ios) val
            if (ios /= 0) val = default_val
        end if
    end function read_env_int

end program aquabc_II_pelagic_benchmark
