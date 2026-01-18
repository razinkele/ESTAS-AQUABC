        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:55 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WRITE_PELAGIC_MEM_BINARY_OUTPUT__genmod
          INTERFACE 
            SUBROUTINE WRITE_PELAGIC_MEM_BINARY_OUTPUT(                 &
     &PELAGIC_BOX_MODEL_DATA,RES_OUTPUT_UNIT,NUM_LINES)
              USE PELAGIC_BOX_MODEL
              TYPE (PELAGIC_BOX_MODEL_DS), INTENT(IN) ::                &
     &PELAGIC_BOX_MODEL_DATA
              INTEGER(KIND=4), INTENT(IN) :: RES_OUTPUT_UNIT
              INTEGER(KIND=4), INTENT(IN) :: NUM_LINES
            END SUBROUTINE WRITE_PELAGIC_MEM_BINARY_OUTPUT
          END INTERFACE 
        END MODULE WRITE_PELAGIC_MEM_BINARY_OUTPUT__genmod
