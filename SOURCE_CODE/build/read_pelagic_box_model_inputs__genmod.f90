        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:54 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READ_PELAGIC_BOX_MODEL_INPUTS__genmod
          INTERFACE 
            SUBROUTINE READ_PELAGIC_BOX_MODEL_INPUTS(                   &
     &PELAGIC_BOX_MODEL_DATA,IN_FILE,OUT_FILE)
              USE PELAGIC_BOX_MODEL
              TYPE (PELAGIC_BOX_MODEL_DS), INTENT(INOUT) ::             &
     &PELAGIC_BOX_MODEL_DATA
              INTEGER(KIND=4), INTENT(IN) :: IN_FILE
              INTEGER(KIND=4), INTENT(IN) :: OUT_FILE
            END SUBROUTINE READ_PELAGIC_BOX_MODEL_INPUTS
          END INTERFACE 
        END MODULE READ_PELAGIC_BOX_MODEL_INPUTS__genmod
