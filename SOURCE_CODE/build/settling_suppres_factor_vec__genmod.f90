        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:44 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETTLING_SUPPRES_FACTOR_VEC__genmod
          INTERFACE 
            SUBROUTINE SETTLING_SUPPRES_FACTOR_VEC(CHLA,NKN,FACTOR)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: CHLA(NKN)
              REAL(KIND=8), INTENT(INOUT) :: FACTOR(NKN)
            END SUBROUTINE SETTLING_SUPPRES_FACTOR_VEC
          END INTERFACE 
        END MODULE SETTLING_SUPPRES_FACTOR_VEC__genmod
