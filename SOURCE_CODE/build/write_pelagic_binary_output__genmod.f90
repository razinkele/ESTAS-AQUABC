        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:55 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WRITE_PELAGIC_BINARY_OUTPUT__genmod
          INTERFACE 
            SUBROUTINE WRITE_PELAGIC_BINARY_OUTPUT(                     &
     &PELAGIC_BOX_MODEL_DATA,TIME,RES_OUTPUT_UNIT,WRITE_SAVED_OUTPUT,   &
     &WRITE_PELAGIC_EXERGY_OUTPUT,MODEL_AT_INIT)
              USE PELAGIC_BOX_MODEL
              TYPE (PELAGIC_BOX_MODEL_DS), INTENT(INOUT) ::             &
     &PELAGIC_BOX_MODEL_DATA
              REAL(KIND=8), INTENT(IN) :: TIME
              INTEGER(KIND=4), INTENT(IN) :: RES_OUTPUT_UNIT
              INTEGER(KIND=4), INTENT(IN) :: WRITE_SAVED_OUTPUT
              INTEGER(KIND=4), INTENT(IN) :: WRITE_PELAGIC_EXERGY_OUTPUT
              INTEGER(KIND=4), INTENT(IN) :: MODEL_AT_INIT
            END SUBROUTINE WRITE_PELAGIC_BINARY_OUTPUT
          END INTERFACE 
        END MODULE WRITE_PELAGIC_BINARY_OUTPUT__genmod
