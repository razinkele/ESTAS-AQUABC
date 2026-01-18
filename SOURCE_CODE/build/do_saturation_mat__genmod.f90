        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:45 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DO_SATURATION_MAT__genmod
          INTERFACE 
            SUBROUTINE DO_SATURATION_MAT(T,S,H,NKN,NUM_SED_LAYERS,CS)
              INTEGER(KIND=4), INTENT(IN) :: NUM_SED_LAYERS
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: T(NKN,NUM_SED_LAYERS)
              REAL(KIND=8), INTENT(IN) :: S(NKN,NUM_SED_LAYERS)
              REAL(KIND=8), INTENT(IN) :: H(NKN,NUM_SED_LAYERS)
              REAL(KIND=8) :: CS(NKN,NUM_SED_LAYERS)
            END SUBROUTINE DO_SATURATION_MAT
          END INTERFACE 
        END MODULE DO_SATURATION_MAT__genmod
