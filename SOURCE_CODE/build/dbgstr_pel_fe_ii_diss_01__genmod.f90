        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:52 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DBGSTR_PEL_FE_II_DISS_01__genmod
          INTERFACE 
            SUBROUTINE DBGSTR_PEL_FE_II_DISS_01(STATE_VARIABLES,PH,TIME,&
     &NKN,NSTATE,NODE_ACTIVE,ERROR)
              INTEGER(KIND=4), INTENT(IN) :: NSTATE
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: STATE_VARIABLES(NKN,NSTATE)
              REAL(KIND=8), INTENT(IN) :: PH(NKN)
              REAL(KIND=8), INTENT(IN) :: TIME
              INTEGER(KIND=4), INTENT(IN) :: NODE_ACTIVE(NKN)
              INTEGER(KIND=4), INTENT(INOUT) :: ERROR
            END SUBROUTINE DBGSTR_PEL_FE_II_DISS_01
          END INTERFACE 
        END MODULE DBGSTR_PEL_FE_II_DISS_01__genmod
