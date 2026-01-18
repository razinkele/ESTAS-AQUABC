        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:52 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DBGSTR_PEL_PO4P_01__genmod
          INTERFACE 
            SUBROUTINE DBGSTR_PEL_PO4P_01(PROCESS_RATES,DERIVATIVES,TIME&
     &,NKN,NSTATE,NDIAGVAR,NODE_ACTIVE,ERROR)
              INTEGER(KIND=4), INTENT(IN) :: NDIAGVAR
              INTEGER(KIND=4), INTENT(IN) :: NSTATE
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: PROCESS_RATES(NKN,NSTATE,     &
     &NDIAGVAR)
              REAL(KIND=8), INTENT(IN) :: DERIVATIVES(NKN,NSTATE)
              REAL(KIND=8), INTENT(IN) :: TIME
              INTEGER(KIND=4), INTENT(IN) :: NODE_ACTIVE(NKN)
              INTEGER(KIND=4), INTENT(INOUT) :: ERROR
            END SUBROUTINE DBGSTR_PEL_PO4P_01
          END INTERFACE 
        END MODULE DBGSTR_PEL_PO4P_01__genmod
