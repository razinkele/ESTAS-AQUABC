        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:51 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UNIONIZED_AMMONIA__genmod
          INTERFACE 
            SUBROUTINE UNIONIZED_AMMONIA(NH3N,NH4N,PH,TEMP,NKN)
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8) :: NH3N(NKN)
              REAL(KIND=8), INTENT(IN) :: NH4N(NKN)
              REAL(KIND=8), INTENT(IN) :: PH(NKN)
              REAL(KIND=8), INTENT(IN) :: TEMP(NKN)
            END SUBROUTINE UNIONIZED_AMMONIA
          END INTERFACE 
        END MODULE UNIONIZED_AMMONIA__genmod
