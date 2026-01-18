        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:44 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE AQUABC_RUN__genmod
          INTERFACE 
            SUBROUTINE AQUABC_RUN(TIME,TIME_STEP,STATE_VARIABLES,PH,    &
     &DRIVING_FUNCTIONS,SEDIMENT_FLUXES)
              USE AQUABC_II_PELAGIC_INTERFACE
              REAL(KIND=8) :: TIME
              REAL(KIND=8) :: TIME_STEP
              REAL(KIND=8) :: STATE_VARIABLES(NKN,NSTATE)
              REAL(KIND=8) :: PH(NKN)
              REAL(KIND=8) :: DRIVING_FUNCTIONS(NKN,N_DRIVING_FUNCTIONS)
              REAL(KIND=8) :: SEDIMENT_FLUXES(NKN,NSTATE)
            END SUBROUTINE AQUABC_RUN
          END INTERFACE 
        END MODULE AQUABC_RUN__genmod
