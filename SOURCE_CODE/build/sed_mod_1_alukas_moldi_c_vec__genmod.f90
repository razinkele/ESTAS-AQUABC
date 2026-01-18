        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:44 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SED_MOD_1_ALUKAS_MOLDI_C_VEC__genmod
          INTERFACE 
            SUBROUTINE SED_MOD_1_ALUKAS_MOLDI_C_VEC(T,SAL,NKN,          &
     &NUM_SED_LAYERS,NUM_SED_VARS,MOL_DIFF)
              INTEGER(KIND=4) :: NUM_SED_VARS
              INTEGER(KIND=4) :: NUM_SED_LAYERS
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8) :: T(NKN,NUM_SED_LAYERS)
              REAL(KIND=8) :: SAL(NKN,NUM_SED_LAYERS)
              REAL(KIND=8) :: MOL_DIFF(NKN,NUM_SED_LAYERS,NUM_SED_VARS)
            END SUBROUTINE SED_MOD_1_ALUKAS_MOLDI_C_VEC
          END INTERFACE 
        END MODULE SED_MOD_1_ALUKAS_MOLDI_C_VEC__genmod
