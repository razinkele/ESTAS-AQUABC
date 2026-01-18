        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:44 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FLX_SED_MOD_1_TO_ALUKAS_II_VEC__genmod
          INTERFACE 
            SUBROUTINE FLX_SED_MOD_1_TO_ALUKAS_II_VEC(                  &
     &FLUXES_FROM_SEDIMENT,NUM_FLUXES_FROM_SEDIMENT,FLUXES_TO_ALUKAS,NKN&
     &,NUM_FLUXES_TO_ALUKAS)
              INTEGER(KIND=4) :: NUM_FLUXES_TO_ALUKAS
              INTEGER(KIND=4) :: NKN
              INTEGER(KIND=4) :: NUM_FLUXES_FROM_SEDIMENT
              REAL(KIND=8) :: FLUXES_FROM_SEDIMENT(NKN,                 &
     &NUM_FLUXES_FROM_SEDIMENT)
              REAL(KIND=8) :: FLUXES_TO_ALUKAS(NKN,NUM_FLUXES_TO_ALUKAS)
            END SUBROUTINE FLX_SED_MOD_1_TO_ALUKAS_II_VEC
          END INTERFACE 
        END MODULE FLX_SED_MOD_1_TO_ALUKAS_II_VEC__genmod
