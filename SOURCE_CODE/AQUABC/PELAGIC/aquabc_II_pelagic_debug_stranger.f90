subroutine DBGSTR_PEL_GEN_01(STATE_VARIABLES, nkn, nstate, node_active, error)
    use AQUABC_PELAGIC_INTERNAL
    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate), intent(in)    :: STATE_VARIABLES
    integer                                      , intent(in)    :: nkn
    integer                                      , intent(in)    :: nstate
    integer              , dimension(nkn)        , intent(in)    :: node_active
    integer                                      , intent(inout) :: error

    integer :: i
    integer :: j
    integer :: k

    integer :: STRANGERSD

    do i = 1, nstate
        if(STRANGERSD(STATE_VARIABLES(1:nkn,i),VALUE_strange,nkn) .eq. 1) then
            nstrange = count(VALUE_strange)
            allocate(STRANGERS    (nstrange))
            allocate(NODES_STRANGE(nstrange))
            allocate(NODES_STRANGE_int(nstrange))
            allocate(NODES_STRANGE_ext(nstrange))

            j=1

            do k=1,nkn
                if(VALUE_strange(k)) then
                    STRANGERS    (j) = STATE_VARIABLES(k,i)
                    NODES_STRANGE(j) = k
                    NODES_STRANGE_int(j) = node_active(k)
                    NODES_STRANGE_ext(j) = (node_active(k))
                    j=j+1
                end if
            end do

            print *, '========================================='
            print *, 'PELAGIC_KINETICS:  Variable ',i !'Cell ',k
            print *, 'Initial state is NaN or Inf:'
            print *, 'NODE_NUMBERS int. =',NODES_STRANGE_int
            print *, 'NODE_NUMBERS ext. =',NODES_STRANGE_ext
            print *, 'VALUES            =',STRANGERS

            deallocate(STRANGERS)
            deallocate(NODES_STRANGE)
            deallocate(NODES_STRANGE_int)
            deallocate(NODES_STRANGE_ext)

            error =1
        end if
    end do

    if(error .eq. 1) stop
end subroutine DBGSTR_PEL_GEN_01


subroutine DBGSTR_PEL_FE_II_DISS_01(STATE_VARIABLES, PH, TIME, nkn, nstate, node_active, error)
    use AQUABC_PELAGIC_INTERNAL
    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate), intent(in)    :: STATE_VARIABLES
    real(kind = DBL_PREC), dimension(nkn)        , intent(in)    :: PH
    real(kind = DBL_PREC),                         intent(in)    :: TIME
    integer                                      , intent(in)    :: nkn
    integer                                      , intent(in)    :: nstate
    integer              , dimension(nkn)        , intent(in)    :: node_active
    integer                                      , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(FE_II_DISS,VALUE_strange,nkn).eq.1) then
        print *, '========================================'
        nstrange = count(VALUE_strange)
        allocate(STRANGERS    (nstrange))
        allocate(NODES_STRANGE(nstrange))

        j=1

        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS    (j) = FE_II_DISS(k)
                NODES_STRANGE(j) = node_active(k)
                j=j+1
            end if
        end do

        print *, 'PELAGIC KINETICS: '
        write(*,*) 'TIME          : ', TIME
        write(*,*) 'FE_II_DISS is NaN or Inf:'
        print *, 'NODE_NUMBERS=',NODES_STRANGE
        print *, 'VALUES=',STRANGERS

        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'
        write(*,*) 'HS2_TOT      : ', (HS2_TOT(NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'PH           : ', (PH     (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'TOT_ALK      : ', (TOT_ALK(NODES_STRANGE(j)),j=1,nstrange)

        deallocate(STRANGERS)
        deallocate(NODES_STRANGE)
        stop
    end if
end subroutine DBGSTR_PEL_FE_II_DISS_01


subroutine DBGSTR_PEL_R_ZOO_GROWTH_01(TIME, nkn, nstate, node_active, error)
    use AQUABC_PELAGIC_INTERNAL
    implicit none

    real(kind = DBL_PREC),                         intent(in)    :: TIME
    integer                                      , intent(in)    :: nkn
    integer                                      , intent(in)    :: nstate
    integer              , dimension(nkn)        , intent(in)    :: node_active
    integer                                      , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(R_ZOO_GROWTH,VALUE_strange,nkn).eq.1) then
        nstrange = count(VALUE_strange)
        allocate(STRANGERS    (nstrange))
        allocate(NODES_STRANGE(nstrange))

        j=1

        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS    (j) = R_ZOO_GROWTH(k)
                NODES_STRANGE(j) = node_active(k)
                j=j+1
            end if
        end do

        print *, 'PELAGIC KINETICS: '
        write(*,*) 'TIME          : ', TIME
        write(*,*) 'R_ZOO_GROWTH is NaN or Inf:'
        print *, 'NODE_NUMBERS=',NODES_STRANGE
        print *, 'VALUES=',STRANGERS

        deallocate(STRANGERS)
        deallocate(NODES_STRANGE)

        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'
        write(*,*) 'R_ZOO_FEEDING_DIA                : ', R_ZOO_FEEDING_DIA
        write(*,*) 'R_ZOO_FEEDING_CYN                : ', R_ZOO_FEEDING_CYN
        write(*,*) 'R_ZOO_FEEDING_OPA                : ', R_ZOO_FEEDING_OPA
        write(*,*) 'R_ZOO_FEEDING_FIX_CYN            : ', R_ZOO_FEEDING_CYN
        write(*,*) 'R_ZOO_FEEDING_NOST_VEG_HET       : ', R_ZOO_FEEDING_NOST_VEG_HET
        write(*,*) 'R_ZOO_FEEDING_DET_PART_ORG_C     : ', R_ZOO_FEEDING_DET_PART_ORG_C

        write(*,*) '    ZOO_C                        : ', ZOO_C
        write(*,*) '    KG_ZOO_DIA                   : ', KG_ZOO_DIA
        write(*,*) '    KG_ZOO_CYN                   : ', KG_ZOO_CYN
        write(*,*) '    KG_ZOO_OPA                   : ', KG_ZOO_OPA
        write(*,*) '    FOOD_FACTOR_ZOO_OPA          : ', FOOD_FACTOR_ZOO_OPA
        write(*,*) '    KG_ZOO_DET_PART_ORG_C        : ', KG_ZOO_DET_PART_ORG_C
        write(*,*) '    KG_ZOO_DET_PART_ORG_C        : ', KG_ZOO_DET_PART_ORG_C
        write(*,*) '    KG_ZOO                       : ', KG_ZOO
        stop
    end if
end subroutine DBGSTR_PEL_R_ZOO_GROWTH_01



subroutine DBGSTR_PEL_R_ZOO_RESP_01(TIME, nkn, nstate, node_active, error)
    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS

    implicit none

    real(kind = DBL_PREC)                 , intent(in)    :: TIME
    integer                               , intent(in)    :: nkn
    integer                               , intent(in)    :: nstate
    integer              , dimension(nkn) , intent(in)    :: node_active
    integer                               , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(R_ZOO_RESP,VALUE_strange,nkn).eq.1) then
        print *, 'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
        print *, 'PELAGIC_KINETICS:'
        write(*,*) 'TIME   : ', TIME


        nstrange = count(VALUE_strange)
        allocate(STRANGERS    (nstrange))
        allocate(NODES_STRANGE(nstrange))

        j=1
        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS    (j) = R_ZOO_RESP(k)
                NODES_STRANGE(j) = node_active(k)
                j=j+1
            end if
        end do

        write(*,*) 'R_ZOO_RESP is NaN or Inf:'
        print *, 'Initial state is NaN or Inf:'
        print *, 'NODE_NUMBERS=',NODES_STRANGE
        print *, 'VALUES=',STRANGERS

        deallocate(STRANGERS)
        deallocate(NODES_STRANGE)

        write(*,*) 'R_ZOO_RESP is not a number or infinite :', R_ZOO_RESP
        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'
        write(*,*) 'R_ZOO_GROWTH      : ', R_ZOO_GROWTH
        write(*,*) 'EFF_ZOO_GROWTH    : ', EFF_ZOO_GROWTH
        stop
    end if
end subroutine DBGSTR_PEL_R_ZOO_RESP_01


subroutine DBGSTR_PEL_NH4N_01(PROCESS_RATES, DERIVATIVES, TIME, nkn, nstate, NDIAGVAR, node_active, error)
    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in) :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,NH4_N_INDEX),VALUE_strange,nkn).eq.1) then
        nstrange = count(VALUE_strange)
        allocate(STRANGERS        (nstrange))
        allocate(NODES_STRANGE    (nstrange))
        allocate(NODES_STRANGE_int(nstrange))
        allocate(NODES_STRANGE_ext(nstrange))

        j=1
        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS        (j) = DERIVATIVES(k,NH4_N_INDEX)
                NODES_STRANGE    (j) = node_active(k)
                NODES_STRANGE_int(j) = node_active(k)
                NODES_STRANGE_ext(j) = (node_active(k))
                j=j+1
            end if
        end do

        print *, 'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
        print *, 'PELAGIC_KINETICS:'
        write(*,*) 'TIME   : ', TIME

        write(*,*) 'DERIVATIVES(NH4_N_INDEX) is not a number or infinite:'
        print *,   'NODE_NUMBERS      =', NODES_STRANGE
        print *,   'NODE_NUMBERS int. =', NODES_STRANGE_int
        print *,   'NODE_NUMBERS ext. =', NODES_STRANGE_ext
        print *,   'VALUES            =', STRANGERS

        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'

        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 1)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 1) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 2)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 2) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 3)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 3) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 4)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 4) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 5)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 5) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 6)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 6) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 7)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 7) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 8)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 8) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 9)  ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 9) ,j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 10) ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 10),j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 11) ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 11),j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 12) ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 12),j=1,nstrange)
        print *, 'PROCESS_RATES(:,NH4_N_INDEX, 17) ',  (PROCESS_RATES(nodes_strange(j),NH4_N_INDEX, 17),j=1,nstrange)
        print *,' '

        write(*,*) 'NH4_N                        : ',  (NH4_N                  (NODES_STRANGE(j)),j=1,nstrange)
        print *, 'R_NOST_VEG_HET_RESP    :',           (R_NOST_VEG_HET_RESP    (NODES_STRANGE(j)),j=1,nstrange)
        print *, 'R_NOST_VEG_HET_INT_RESP:',           (R_NOST_VEG_HET_INT_RESP(NODES_STRANGE(j)),j=1,nstrange)
        print *, 'R_NOST_VEG_HET_GROWTH  :',           (R_NOST_VEG_HET_GROWTH  (NODES_STRANGE(j)),j=1,nstrange)
        print *, 'NOST_VEG_HET_C          :',          (NOST_VEG_HET_C          (NODES_STRANGE(j)),j=1,nstrange)
        print *, 'LIM_KG_NOST_VEG_HET_DOXY:',          (LIM_KG_NOST_VEG_HET_DOXY(NODES_STRANGE(j)),j=1,nstrange)
        print *, 'LIM_KG_NOST_VEG_HET_P   :',          (LIM_KG_NOST_VEG_HET_P   (NODES_STRANGE(j)),j=1,nstrange)
        print *, 'LIM_KG_NOST_VEG_HET_LIGHT',          (LIM_KG_NOST_VEG_HET_LIGHT(NODES_STRANGE(j)),j=1,nstrange)
        print *, 'KG_NOST_VEG_HET         :',          (KG_NOST_VEG_HET         (NODES_STRANGE(j)),j=1,nstrange)

        print *, 'EFF_NOST_VEG_HET_GROWTH:',            EFF_NOST_VEG_HET_GROWTH
        print *, 'FRAC_NOST_VEG_HET_EXCR :',            FRAC_NOST_VEG_HET_EXCR
        print *, 'R_ABIOTIC_DON_MIN_DOXY',              R_ABIOTIC_DON_MIN_DOXY
        print *, 'DISS_ORG_N',                          DISS_ORG_N
        write(*,*) '**************************************************************'

        deallocate(STRANGERS)
        deallocate(NODES_STRANGE)
        stop
    end if
end subroutine DBGSTR_PEL_NH4N_01


subroutine DBGSTR_PEL_PO4P_01(PROCESS_RATES, DERIVATIVES, TIME, nkn, nstate, NDIAGVAR, node_active, error)
    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in) :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,PO4_P_INDEX),VALUE_strange,nkn).eq.1) then

    nstrange = count(VALUE_strange)
    allocate(STRANGERS    (nstrange))
    allocate(NODES_STRANGE(nstrange))

    j=1
        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS    (j) = DERIVATIVES(k,PO4_P_INDEX)
                NODES_STRANGE(j) = k
                j=j+1
            end if
        end do

        print *, 'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
        write(*,*) 'TIME   : ', TIME
        print *, 'PELAGIC_KINETICS:  Derivative(3) '
        print *, 'is NaN or Inf:'
        print *, 'NODE_NUMBERS=',NODES_STRANGE
        print *, 'VALUES=',STRANGERS
        error=1

        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'
        write(*,*) 'R_ABIOTIC_DOP_MIN_DOXY   : ', (R_ABIOTIC_DOP_MIN_DOXY(NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) '    TEMP            : ', (TEMP(NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) '    DISS_ORG_P      : ', (DISS_ORG_P(NODES_STRANGE(j)),j=1,nstrange)

        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 1 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 1 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 2 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 2 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 3 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 3 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 4 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 4 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 5 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 5 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 6 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 6 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 7 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 7 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 8 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 8 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 9 )', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 9 ),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 10)', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 10),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 13)', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 13),j=1,nstrange)
        print *, 'PROCESS_RATES(1:nkn,PO4_P_INDEX, 14)', (PROCESS_RATES(NODES_STRANGE(j),PO4_P_INDEX, 14),j=1,nstrange)

        deallocate(STRANGERS)
        deallocate(NODES_STRANGE)
        stop
    end if
end subroutine DBGSTR_PEL_PO4P_01


subroutine DBGSTR_PEL_DOXY_01(PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)
    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in)    :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer              ,               dimension(nflags) , intent(in)    :: FLAGS
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: nflags
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,DISS_OXYGEN_INDEX),VALUE_strange,nkn).eq.1) then
        error=1

        nstrange = count(VALUE_strange)
        allocate(STRANGERS    (nstrange))
        allocate(NODES_STRANGE(nstrange))
        allocate(NODES_STRANGE_int(nstrange))
        allocate(NODES_STRANGE_ext(nstrange))

        j=1

        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS    (j) = DERIVATIVES(k,DISS_OXYGEN_INDEX)
                NODES_STRANGE(j) = k
                NODES_STRANGE_int(j) = node_active(k)
                NODES_STRANGE_ext(j) = (node_active(k))
                j=j+1
            end if
        end do

        print *, 'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
        write(*,*) 'TIME   : ', TIME
        print *, 'PELAGIC_KINETICS:  Derivative(DISS_OXYGEN_INDEX) '
        print *, 'is NaN or Inf:'
        print *, 'NODE_NUMBERS=',NODES_STRANGE
        print *, 'NODE_NUMBERS int.=',NODES_STRANGE_int
        print *, 'NODE_NUMBERS ext.=',NODES_STRANGE_ext
        print *, 'VALUES=',STRANGERS

        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'

        write(*,*) '    ZOO_O2_TO_C          : ', ZOO_O2_TO_C
        write(*,*) '    DIA_O2_TO_C          : ', DIA_O2_TO_C
        write(*,*) '    CYN_O2_TO_C          : ', CYN_O2_TO_C
        write(*,*) '    OPA_O2_TO_C          : ', OPA_O2_TO_C
        print *,'SURFACE_BOX:', SURFACE_BOX
        print *,'FLAGS(2)=',FLAGS(2)

        write(*,*) '    K_A parameter        : ',  K_A

        write(*,*) ''
        write(*,*) 'R_AERATION               : ', (R_AERATION      &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'DISS_OXYGEN              : ', (DISS_OXYGEN     &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'DISS_OXYGEN_SAT          : ', (DISS_OXYGEN_SAT &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'K_A_CALC                 : ', (K_A_CALC        &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_DIA_GROWTH             : ', (R_DIA_GROWTH &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_CYN_GROWTH             : ', (R_CYN_GROWTH &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_OPA_GROWTH             : ', (R_OPA_GROWTH &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_DIA_TOT_RESP           : ', (R_DIA_TOT_RESP          &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_CYN_TOT_RESP           : ', (R_CYN_TOT_RESP          &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_OPA_TOT_RESP           : ', (R_OPA_TOT_RESP          &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_ZOO_EX_DOC             : ', (R_ZOO_EX_DOC            &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_ZOO_TOT_RESP           : ', (R_ZOO_TOT_RESP          &
                                                  (NODES_STRANGE(j)),j=1,nstrange)

        write(*,*) ''
        write(*,*) 'R_FE_II_OXIDATION        : ', (R_FE_II_OXIDATION        &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_MN_II_OXIDATION        : ', (R_MN_II_OXIDATION        &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_METHANE_OXIDATION      : ', (R_METHANE_OXIDATION      &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) ''
        write(*,*) 'R_SULPHIDE_OXIDATION     : ', (R_SULPHIDE_OXIDATION     &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        print *,'TEMP     =', (TEMP       (NODES_STRANGE(j)),j=1,nstrange)
        print *,'SALT     =', (SALT       (NODES_STRANGE(j)),j=1,nstrange)
        print *,'AIRTEMP  =', (AIRTEMP    (NODES_STRANGE(j)),j=1,nstrange)
        print *,'WINDS    =', (WINDS      (NODES_STRANGE(j)),j=1,nstrange)
        print *,'ELEVATION=', (ELEVATION  (NODES_STRANGE(j)),j=1,nstrange)
        print *,'DEPTH    =', (DEPTH      (NODES_STRANGE(j)),j=1,nstrange)

        stop
    end if
end subroutine DBGSTR_PEL_DOXY_01


subroutine DBGSTR_PEL_CYNC_01 &
    (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)

    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in)    :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer              ,               dimension(nflags) , intent(in)    :: FLAGS
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: nflags
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,CYN_C_INDEX),VALUE_strange,nkn).eq.1) then
        write(*,*) 'PELAGIC_KINETICS:'
        write(*,*) 'TIME   : ', TIME
        write(*,*) 'DERIVATIVES(CYN_C_INDEX) is not a number or infinite.'
        write(*,*) DERIVATIVES(1:nkn,CYN_C_INDEX)
        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'
        write(*,*) 'R_CYN_GROWTH             : ', R_CYN_GROWTH
        write(*,*) '    LIM_KG_CYN           : ', LIM_KG_CYN
        write(*,*) '        LIM_KG_CYN_DOXY  : ', LIM_KG_CYN_DOXY
        write(*,*) '        LIM_KG_CYN_NUTR  : ', LIM_KG_CYN_NUTR
        write(*,*) '        LIM_KG_CYN_LIGHT : ', LIM_KG_CYN_LIGHT
        write(*,*) '            ALPHA_0      : ', ALPHA_0
        write(*,*) '            ALPHA_1      : ', ALPHA_1
        write(*,*) '            I_A          : ', I_A
        write(*,*) '            I_S_CYN      : ', I_S_CYN
        write(*,*) '    CYN_C                : ', CYN_C
        write(*,*) '    KG_CYN               : ', KG_CYN
        write(*,*) ''
        write(*,*) 'R_CYN_TOT_RESP           : ', R_CYN_TOT_RESP
        write(*,*) ''
        write(*,*) 'R_CYN_DEATH              : ', R_CYN_DEATH
        write(*,*) ''
        write(*,*) 'R_ZOO_FEEDING_CYN        : ', R_ZOO_FEEDING_CYN
        stop
    end if
end subroutine DBGSTR_PEL_CYNC_01



subroutine DBGSTR_PEL_ZOOC_01 &
           (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)

    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in)    :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer              ,               dimension(nflags) , intent(in)    :: FLAGS
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: nflags
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,ZOO_C_INDEX),VALUE_strange,nkn).eq.1) then
        print *, 'PELAGIC_KINETICS:'
        write(*,*) 'TIME   : ', TIME
        write(*,*) 'DERIVATIVES(ZOO_C_INDEX) is not a number or infinite'
        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'
        write(*,*) 'ZOO_C                                  : ', ZOO_C
        write(*,*) 'R_ZOO_GROWTH                           : ', R_ZOO_GROWTH
        write(*,*) '    R_ZOO_FEEDING_DIA                  : ', R_ZOO_FEEDING_DIA
        write(*,*) '    R_ZOO_FEEDING_CYN                  : ', R_ZOO_FEEDING_CYN
        write(*,*) '    R_ZOO_FEEDING_OPA                  : ', R_ZOO_FEEDING_OPA
        write(*,*) '    R_ZOO_FEEDING_FIX_CYN              : ', R_ZOO_FEEDING_FIX_CYN
        write(*,*) '    R_ZOO_FEEDING_DET_PART_ORG_C       : ', R_ZOO_FEEDING_DET_PART_ORG_C
        write(*,*) '        KG_ZOO_DET_PART_ORG_C          : ', KG_ZOO_DET_PART_ORG_C
        write(*,*) '        FOOD_FACTOR_ZOO_DET_PART_ORG_C : ', FOOD_FACTOR_ZOO_DET_PART_ORG_C
        write(*,*) '            PREF_ZOO_DET_PART_ORG_C    : ', PREF_ZOO_DET_PART_ORG_C
        write(*,*) '            DET_PART_ORG_C             : ', DET_PART_ORG_C
        write(*,*) '            FOOD_MIN_ZOO               : ', FOOD_MIN_ZOO
        write(*,*) '            FOOD_AVAIL_ZOO             : ', FOOD_AVAIL_ZOO
        write(*,*) '            KHS_DET_PART_ORG_C_ZOO     : ', KHS_DET_PART_ORG_C_ZOO
        write(*,*) 'R_ZOO_EX_DOC                           : ', R_ZOO_EX_DOC
        write(*,*) 'R_ZOO_TOT_RESP                         : ', R_ZOO_TOT_RESP
        write(*,*) 'R_ZOO_DEATH                            : ', R_ZOO_DEATH
        stop
    end if
end subroutine DBGSTR_PEL_ZOOC_01


subroutine DBGSTR_PEL_DET_PART_ORG_C_01 &
           (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)

    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in)    :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer              ,               dimension(nflags) , intent(in)    :: FLAGS
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: nflags
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,DET_PART_ORG_C_INDEX),VALUE_strange,nkn).eq.1) then
        print *, 'PELAGIC_KINETICS:'
        write(*,*) 'TIME   : ', TIME
        write(*,*) 'DERIVATIVES(DET_PART_ORG_C_INDEX) is not a number or infinite.'
        write(*,*) DERIVATIVES(1:nkn,DET_PART_ORG_C_INDEX)
        write(*,*)
        write(*,*) 'Related variables'
        write(*,*) '-----------------'
        write(*,*) 'R_DIA_DEATH                        : ', R_DIA_DEATH
        write(*,*) 'R_CYN_DEATH                        : ', R_CYN_DEATH
        write(*,*) 'R_OPA_DEATH                        : ', R_OPA_DEATH
        write(*,*) 'R_FIX_CYN_DEATH                    : ', R_FIX_CYN_DEATH
        write(*,*) 'R_ZOO_DEATH                        : ', R_ZOO_DEATH
        write(*,*) 'R_ZOO_FEEDING_DET_PART_ORG_C       : ', R_ZOO_FEEDING_DET_PART_ORG_C
        write(*,*) 'R_DET_PART_ORG_C_DISSOLUTION       : ', R_DET_PART_ORG_C_DISSOLUTION
        write(*,*) 'R_NOST_VEG_HET_DEATH               : ', R_NOST_VEG_HET_DEATH
        write(*,*) 'R_DENS_MORT_NOST_VEG_HET           : ', R_DENS_MORT_NOST_VEG_HET
        write(*,*) 'R_MORT_AKI                         : ', R_MORT_AKI
        write(*,*) 'FAC_HYPOX_NOST_VEG_HET_D           : ', FAC_HYPOX_NOST_VEG_HET_D

        write(*,*) '    KDISS_DET_PART_ORG_C_20        : ', KDISS_DET_PART_ORG_C_20
        write(*,*) '    THETA_KDISS_DET_PART_ORG_C     : ', THETA_KDISS_DET_PART_ORG_C
        write(*,*) '    DET_PART_ORG_C                 : ', DET_PART_ORG_C
        write(*,*) '    DISS_OXYGEN                    : ', DISS_OXYGEN
        stop
    end if

end subroutine DBGSTR_PEL_DET_PART_ORG_C_01


subroutine DBGSTR_PEL_DET_DISS_ORG_N_01 &
           (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)

    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in)    :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer              ,               dimension(nflags) , intent(in)    :: FLAGS
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: nflags
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,DISS_ORG_N_INDEX),VALUE_strange,nkn).eq.1) then
        nstrange = count(VALUE_strange)
        allocate(STRANGERS    (nstrange))
        allocate(NODES_STRANGE(nstrange))
        j=1

        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS    (j) = DERIVATIVES(k,DISS_ORG_N_INDEX)
                NODES_STRANGE(j) = node_active(k)
                j=j+1
            end if
        end do
        print *, 'PELAGIC_KINETICS:  Derivative 16 is strange'
        write(*,*) 'TIME   : ', TIME
        print *, 'NODE_NUMBERS=',NODES_STRANGE
        print *, 'VALUES=',STRANGERS

        print *, 'RELATED VARIABLES     :'
        write(*,*) 'K_MIN_DON_DOC_20    : ',   K_MIN_DON_DOC_20
        write(*,*) 'THETA_K_MIN_DON_DOC : ',   THETA_K_MIN_DON_DOC
        write(*,*) 'K_HS_DON_MIN_DOC    : ',   K_HS_DON_MIN_DOC

        write(*,*) 'R_ABIOTIC_DON_MIN_DOXY : ',   (R_ABIOTIC_DON_MIN_DOXY    &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'R_ABIOTIC_DON_MIN_NO3N : ',   (R_ABIOTIC_DON_MIN_NO3N    &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'R_ABIOTIC_DON_MIN_MN_IV: ',   (R_ABIOTIC_DON_MIN_MN_IV    &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'R_ABIOTIC_DON_MIN_FE_III : ', (R_ABIOTIC_DON_MIN_FE_III &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'R_ABIOTIC_DON_MIN_S_PLUS_6: ',(R_ABIOTIC_DON_MIN_S_PLUS_6 &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'R_ABIOTIC_DON_MIN_DOC     : ',(R_ABIOTIC_DON_MIN_DOC &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'R_ABIOTIC_DOC_MIN_DOC     : ',(R_ABIOTIC_DOC_MIN_DOC &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'LIM_DOC_RED               : ',(LIM_DOC_RED &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'PH_CORR_DON_MIN_DOC       : ',(PH_CORR_DON_MIN_DOC &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'DISS_ORG_N                : ',(DISS_ORG_N &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'LIM_NO3N_RED              : ',(LIM_NO3N_RED &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'LIM_MN_IV_RED             : ',(LIM_MN_IV_RED &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'LIM_FE_III_RED            : ',(LIM_FE_III_RED &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        write(*,*) 'LIM_S_PLUS_6_RED          : ',(LIM_S_PLUS_6_RED &
                                                  (NODES_STRANGE(j)),j=1,nstrange)
        deallocate(STRANGERS)
        deallocate(NODES_STRANGE)

        stop
    end if
end subroutine DBGSTR_PEL_DET_DISS_ORG_N_01


subroutine DBGSTR_PEL_DET_CH4_C_01 &
           (PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)

    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in)    :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer              ,               dimension(nflags) , intent(in)    :: FLAGS
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: nflags
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,CH4_C_INDEX),VALUE_strange,nkn).eq.1) then
        nstrange = count(VALUE_strange)
        allocate(STRANGERS    (nstrange))
        allocate(NODES_STRANGE(nstrange))
        j=1

        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS    (j) = DERIVATIVES(k, CH4_C_INDEX)
                NODES_STRANGE(j) = node_active(k)
                j=j+1
            end if
        end do
        print *, 'PELAGIC_KINETICS:  Derivative related to CH4_C is strange'
        write(*,*) 'TIME   : ', TIME
        print *, 'NODE_NUMBERS=',NODES_STRANGE
        print *, 'VALUES=',STRANGERS

        print *, 'RELATED VARIABLES     :'

        write(*,*) 'CH4_ATM_EXCHANGE    : ',   &
            (CH4_ATM_EXCHANGE(NODES_STRANGE(j)),j=1,nstrange)

        write(*,*) 'R_METHANOGENESIS    : ',   &
            (R_ABIOTIC_DON_MIN_NO3N(NODES_STRANGE(j)),j=1,nstrange)

        write(*,*) 'R_METHANE_OXIDATION : ',   &
            (R_ABIOTIC_DON_MIN_MN_IV(NODES_STRANGE(j)),j=1,nstrange)

        deallocate(STRANGERS)
        deallocate(NODES_STRANGE)
        stop
    end if
end subroutine DBGSTR_PEL_DET_CH4_C_01



subroutine DBGSTR_PEL_DET_TOT_ALK_01 &
           (PROCESS_RATES, DERIVATIVES, PH, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)

    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in)    :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC), dimension(nkn)                  , intent(in)    :: PH
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer              ,               dimension(nflags) , intent(in)    :: FLAGS
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: nflags
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: j
    integer :: k

    integer :: STRANGERSD

    if (STRANGERSD(DERIVATIVES(1:nkn,TOT_ALK_INDEX),VALUE_strange,nkn).eq.1) then
        nstrange = count(VALUE_strange)
        allocate(STRANGERS    (nstrange))
        allocate(NODES_STRANGE(nstrange))
        allocate(NODES_STRANGE_int(nstrange))
        allocate(NODES_STRANGE_ext(nstrange))
        j=1

        do k=1,nkn
            if(VALUE_strange(k)) then
                STRANGERS    (j) = DERIVATIVES(k,TOT_ALK_INDEX)
                NODES_STRANGE(j) = k
                j=j+1
            end if
        end do

        print *, 'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
        print *, 'PELAGIC_KINETICS:  Derivative 24'
        print *, 'is NaN or Inf:'
        print *, 'NODE_NUMBERS=',NODES_STRANGE
        print *, 'VALUES=',STRANGERS

        print *, '----------------------------------'
        print *, 'Related variables:'
        print *,'pH                          :',(pH                           &
                                                (NODES_STRANGE(j)),j=1,nstrange)
        print *,'ALK_GAINED_BY_AMMONIUM_GEN  :',(ALK_GAINED_BY_AMMONIUM_GEN   &
                                                (NODES_STRANGE(j)),j=1,nstrange)
        print *,'ALK_GAINED_BY_NITRATE_CONS  :',(ALK_GAINED_BY_NITRATE_CONS   &
                                                (NODES_STRANGE(j)),j=1,nstrange)
        print *,'ALK_GAINED_BY_PHOSPHATE_CONS:',(ALK_GAINED_BY_PHOSPHATE_CONS &
                                                (NODES_STRANGE(j)),j=1,nstrange)
        print *,'ALK_LOST_BY_AMMONIUM_CONS   :',(ALK_LOST_BY_AMMONIUM_CONS    &
                                                (NODES_STRANGE(j)),j=1,nstrange)
        print *,'ALK_LOST_BY_NITRIFICATION   :',(ALK_LOST_BY_NITRIFICATION    &
                                                (NODES_STRANGE(j)),j=1,nstrange)
        print *,'ALK_LOST_BY_PHOSPHATE_GEN   :',(ALK_LOST_BY_PHOSPHATE_GEN    &
                                                (NODES_STRANGE(j)),j=1,nstrange)

        error=1
        deallocate(STRANGERS    )
        deallocate(NODES_STRANGE)
        deallocate(NODES_STRANGE_int)
        deallocate(NODES_STRANGE_ext)
    end if

    if(error .eq. 1) stop
end subroutine DBGSTR_PEL_DET_TOT_ALK_01


subroutine DBGSTR_PEL_GEN_02(PROCESS_RATES, DERIVATIVES, FLAGS, TIME, nkn, nstate, nflags, NDIAGVAR, node_active, error)

    use AQUABC_PELAGIC_INTERNAL
    use AQUABC_PELAGIC_MODEL_CONSTANTS
    use aquabc_pel_state_var_indexes

    implicit none

    real(kind = DBL_PREC), dimension(nkn, nstate, NDIAGVAR), intent(in)    :: PROCESS_RATES
    real(kind = DBL_PREC), dimension(nkn, nstate)          , intent(in)    :: DERIVATIVES
    real(kind = DBL_PREC)                                  , intent(in)    :: TIME
    integer              ,               dimension(nflags) , intent(in)    :: FLAGS
    integer                                                , intent(in)    :: nkn
    integer                                                , intent(in)    :: nstate
    integer                                                , intent(in)    :: nflags
    integer                                                , intent(in)    :: NDIAGVAR
    integer              , dimension(nkn)                  , intent(in)    :: node_active
    integer                                                , intent(inout) :: error

    integer :: i
    integer :: j
    integer :: k

    integer :: STRANGERSD

    do i = 1, nstate
        if (STRANGERSD(DERIVATIVES(1:nkn,i),VALUE_strange,nkn).eq.1) then
            nstrange = count(VALUE_strange)
            allocate(STRANGERS    (nstrange))
            allocate(NODES_STRANGE(nstrange))

            j=1

            do k=1,nkn
                if(VALUE_strange(k)) then
                    STRANGERS    (j) = DERIVATIVES(k,i)
                    NODES_STRANGE(j) = k
                    j=j+1
                end if
            end do

            print *, 'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
            print *, 'PELAGIC_KINETICS:  Derivative ',i !'Cell ',k
            print *, 'is NaN or Inf:'
            print *, 'NODE_NUMBERS=',NODES_STRANGE
            print *, 'VALUES=',STRANGERS
            error=1
            deallocate(STRANGERS)
            deallocate(NODES_STRANGE)
        end if
    end do

    if(error .eq. 1) stop

end subroutine DBGSTR_PEL_GEN_02
