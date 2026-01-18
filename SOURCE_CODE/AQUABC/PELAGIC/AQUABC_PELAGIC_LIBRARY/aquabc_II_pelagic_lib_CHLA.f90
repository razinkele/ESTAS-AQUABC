! Auxilary routines for the pelagic model

! Contents:

module chla_diagnostics
    implicit none
    logical :: CHLA_NEG_FLAG = .false.     ! set when a computed CHLA < 0 is detected
    integer :: CHLA_NEG_NODE = 0
    double precision :: CHLA_NEG_VALUE = 0.0D0
    logical :: CHLA_DUMP_DONE = .false.    ! ensure a one-shot dump
    logical, allocatable :: CLAMP_WARNED(:)  ! one-shot per-node clamped-warning
    integer, allocatable :: CLAMP_COUNT(:)    ! total clamp events per node (one-shot summary)
    integer, allocatable :: CLAMP_PROC_COUNT(:,:) ! clamp counts per node/process (nkn x NDIAGVAR)
end module chla_diagnostics

!subroutine chlorophyl_a_vec

subroutine chlorophyl_a_vec &
           (DIA_C            , &
            CYN_C            , &
            OPA_C            , &
            FIX_CYN_C        , &
            NOST_VEG_HET_C   , &
            DIA_C_TO_CHLA    , &
            CYN_C_TO_CHLA    , &
            OPA_C_TO_CHLA    , &
            FIX_CYN_C_TO_CHLA, &
            NOST_C_TO_CHLA   , &
            nkn              , &
            CHLA             , &
            CONSIDER_NON_OBLIGATORY_FIXERS, &
            CONSIDER_NOSTOCALES)

    use chla_diagnostics, only: CHLA_NEG_FLAG, CHLA_NEG_NODE, CHLA_NEG_VALUE, CHLA_DUMP_DONE
    implicit none

    double precision, dimension(nkn), intent(in) :: DIA_C
    double precision, dimension(nkn), intent(in) :: CYN_C
    double precision, dimension(nkn), intent(in) :: OPA_C
    double precision, dimension(nkn), intent(in) :: FIX_CYN_C
    double precision, dimension(nkn), intent(in) :: NOST_VEG_HET_C

    double precision, intent(in) :: DIA_C_TO_CHLA
    double precision, intent(in) :: CYN_C_TO_CHLA
    double precision, intent(in) :: FIX_CYN_C_TO_CHLA
    double precision, intent(in) :: OPA_C_TO_CHLA
    double precision, intent(in) :: NOST_C_TO_CHLA

    integer, intent(in) :: nkn

    double precision, dimension(nkn), intent(inout) :: chla

    integer, intent(in) :: CONSIDER_NON_OBLIGATORY_FIXERS
    integer, intent(in) :: CONSIDER_NOSTOCALES

    integer :: DO_NON_OBLIGATORY_FIXERS
    integer :: DO_NOSTOCALES
    integer :: i
    double precision, save :: prev_FIX_CYN_C_1 = 0.0D0
    logical, save :: prev_init = .false.

    DO_NON_OBLIGATORY_FIXERS = 0
    DO_NOSTOCALES            = 1

    !if (present(CONSIDER_NON_OBLIGATORY_FIXERS)) then
        if (CONSIDER_NON_OBLIGATORY_FIXERS > 0) then
            DO_NON_OBLIGATORY_FIXERS = 1
        else
            DO_NON_OBLIGATORY_FIXERS = 0
        end if
    !end if

    !if (present(CONSIDER_NOSTOCALES)) then
        if (CONSIDER_NOSTOCALES > 0) then
            DO_NOSTOCALES = 1
        else
            DO_NOSTOCALES = 0
        end if
    !end if

    ! Total chorophyl a in mcg/l
    CHLA = ((DIA_C     / DIA_C_TO_CHLA)  +     &
            (CYN_C     / CYN_C_TO_CHLA)  +     &
            (OPA_C     / OPA_C_TO_CHLA)) * 1.0D3

    if (DO_NON_OBLIGATORY_FIXERS > 0) then
        CHLA = CHLA + ((FIX_CYN_C      / FIX_CYN_C_TO_CHLA) * 1.0D3)
    end if

    if (DO_NOSTOCALES > 0) then
        CHLA = CHLA + ((NOST_VEG_HET_C / NOST_C_TO_CHLA   ) * 1.0D3)
    end if

    ! Diagnostic: if any computed CHLA is negative, dump per-node state and stop
    do i = 1, nkn
        ! 0D quick-detect diagnostic: track node 1 FIX_CYN_C across calls
        if (i == 1) then
            if (prev_init .and. (prev_FIX_CYN_C_1 > 0.0D0) .and. (FIX_CYN_C(1) < 0.0D0)) then
                write(6,*) 'ALERT: FIX_CYN flipped sign between CHLA calls at node 1'
                write(6,*) '  PREV_FIX_CYN_C_1=', prev_FIX_CYN_C_1, ' CUR_FIX_CYN_C=', FIX_CYN_C(1)
                write(6,*) '  DIA_C=', DIA_C(1), ' CYN_C=', CYN_C(1), ' FIX_CYN_C=', FIX_CYN_C(1), ' OPA_C=', OPA_C(1)
                stop
            end if
            prev_FIX_CYN_C_1 = FIX_CYN_C(1)
            prev_init = .true.
        end if

        if (CHLA(i) < 0.0D0) then
            ! Set one-shot CHLA negative flag and record node/value for caller to dump full context
            CHLA_NEG_FLAG = .true.
            CHLA_NEG_NODE = i
            CHLA_NEG_VALUE = CHLA(i)
            return
        end if
    end do

end subroutine chlorophyl_a_vec
