        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:52 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHLOROPHYL_A__genmod
          INTERFACE 
            SUBROUTINE CHLOROPHYL_A(STATE,NSTATE,CHLA,                  &
     &CONSIDER_NON_OBLIGATORY_FIXERS,CONSIDER_NOSTOCALES)
              INTEGER(KIND=4) :: NSTATE
              REAL(KIND=8) :: STATE(NSTATE)
              REAL(KIND=8) :: CHLA
              INTEGER(KIND=4), INTENT(IN) ::                            &
     &CONSIDER_NON_OBLIGATORY_FIXERS
              INTEGER(KIND=4), INTENT(IN) :: CONSIDER_NOSTOCALES
            END SUBROUTINE CHLOROPHYL_A
          END INTERFACE 
        END MODULE CHLOROPHYL_A__genmod
