        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:45 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SED_IRON_II_DISSOLUTION__genmod
          INTERFACE 
            SUBROUTINE SED_IRON_II_DISSOLUTION(HS2_TOT,PH,TOT_ALK,NKN,  &
     &NUM_SED_LAYERS,FE_II_TOT)
              INTEGER(KIND=4), INTENT(IN) :: NUM_SED_LAYERS
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(INOUT) :: HS2_TOT(NKN,NUM_SED_LAYERS)
              REAL(KIND=8), INTENT(INOUT) :: PH(NKN,NUM_SED_LAYERS)
              REAL(KIND=8), INTENT(INOUT) :: TOT_ALK(NKN,NUM_SED_LAYERS)
              REAL(KIND=8), INTENT(INOUT) :: FE_II_TOT(NKN,             &
     &NUM_SED_LAYERS)
            END SUBROUTINE SED_IRON_II_DISSOLUTION
          END INTERFACE 
        END MODULE SED_IRON_II_DISSOLUTION__genmod
