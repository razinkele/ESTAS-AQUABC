        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:46 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CALCULATE_PH_CORR__genmod
          INTERFACE 
            SUBROUTINE CALCULATE_PH_CORR(PH_CORR,PH,PH_MIN,PH_MAX,NKN)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(INOUT) :: PH_CORR(NKN)
              REAL(KIND=8), INTENT(IN) :: PH(NKN)
              REAL(KIND=8), INTENT(IN) :: PH_MIN
              REAL(KIND=8), INTENT(IN) :: PH_MAX
            END SUBROUTINE CALCULATE_PH_CORR
          END INTERFACE 
        END MODULE CALCULATE_PH_CORR__genmod
