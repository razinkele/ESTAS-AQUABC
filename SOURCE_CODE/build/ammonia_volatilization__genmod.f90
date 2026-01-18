        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:52 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE AMMONIA_VOLATILIZATION__genmod
          INTERFACE 
            SUBROUTINE AMMONIA_VOLATILIZATION(AMMONIA_VOLATIL_RATE,NH4N,&
     &PH,TEMP,KA,NKN)
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8) :: AMMONIA_VOLATIL_RATE(NKN)
              REAL(KIND=8), INTENT(IN) :: NH4N(NKN)
              REAL(KIND=8), INTENT(IN) :: PH(NKN)
              REAL(KIND=8), INTENT(IN) :: TEMP(NKN)
              REAL(KIND=8), INTENT(IN) :: KA(NKN)
            END SUBROUTINE AMMONIA_VOLATILIZATION
          END INTERFACE 
        END MODULE AMMONIA_VOLATILIZATION__genmod
