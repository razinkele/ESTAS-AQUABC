        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:51 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE AMMONIA_PREFS__genmod
          INTERFACE 
            SUBROUTINE AMMONIA_PREFS(AMMONIA_PREF,NH3,NOX,KN,NKN)
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8) :: AMMONIA_PREF(NKN)
              REAL(KIND=8), INTENT(IN) :: NH3(NKN)
              REAL(KIND=8), INTENT(IN) :: NOX(NKN)
              REAL(KIND=8), INTENT(IN) :: KN
            END SUBROUTINE AMMONIA_PREFS
          END INTERFACE 
        END MODULE AMMONIA_PREFS__genmod
