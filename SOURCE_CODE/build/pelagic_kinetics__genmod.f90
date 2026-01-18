        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:57 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PELAGIC_KINETICS__genmod
          INTERFACE 
            SUBROUTINE PELAGIC_KINETICS(PELAGIC_BOX_MODEL_DATA,         &
     &SEDIMENT_FLUXES,TIME,TIME_STEP,AQUABC_CALLED_BEFORE)
              USE GLOBAL
              USE PELAGIC_BOX_MODEL
              TYPE (PELAGIC_BOX_MODEL_DS), INTENT(IN) ::                &
     &PELAGIC_BOX_MODEL_DATA
              REAL(KIND=8), INTENT(IN) :: SEDIMENT_FLUXES(NKN,36)
              REAL(KIND=8), INTENT(IN) :: TIME
              REAL(KIND=8), INTENT(IN) :: TIME_STEP
              REAL(KIND=8), INTENT(INOUT) :: AQUABC_CALLED_BEFORE
            END SUBROUTINE PELAGIC_KINETICS
          END INTERFACE 
        END MODULE PELAGIC_KINETICS__genmod
