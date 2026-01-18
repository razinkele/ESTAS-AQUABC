        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:46 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE IRON_II_DISSOLUTION__genmod
          INTERFACE 
            SUBROUTINE IRON_II_DISSOLUTION(HS2_TOT,PH,TOT_ALK,NKN,      &
     &FE_II_TOT)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(INOUT) :: HS2_TOT(NKN)
              REAL(KIND=8), INTENT(INOUT) :: PH(NKN)
              REAL(KIND=8), INTENT(INOUT) :: TOT_ALK(NKN)
              REAL(KIND=8), INTENT(INOUT) :: FE_II_TOT(NKN)
            END SUBROUTINE IRON_II_DISSOLUTION
          END INTERFACE 
        END MODULE IRON_II_DISSOLUTION__genmod
