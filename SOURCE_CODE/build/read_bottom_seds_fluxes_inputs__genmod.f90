        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:55 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READ_BOTTOM_SEDS_FLUXES_INPUTS__genmod
          INTERFACE 
            SUBROUTINE READ_BOTTOM_SEDS_FLUXES_INPUTS(                  &
     &PELAGIC_BOX_MODEL_DATA,IN_FILE,FLUX_SET_NO)
              USE PELAGIC_BOX_MODEL
              TYPE (PELAGIC_BOX_MODEL_DS), INTENT(INOUT) ::             &
     &PELAGIC_BOX_MODEL_DATA
              INTEGER(KIND=4), INTENT(IN) :: IN_FILE
              INTEGER(KIND=4), INTENT(IN) :: FLUX_SET_NO
            END SUBROUTINE READ_BOTTOM_SEDS_FLUXES_INPUTS
          END INTERFACE 
        END MODULE READ_BOTTOM_SEDS_FLUXES_INPUTS__genmod
