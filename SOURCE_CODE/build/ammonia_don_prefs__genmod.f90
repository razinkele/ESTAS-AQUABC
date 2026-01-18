        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:51 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE AMMONIA_DON_PREFS__genmod
          INTERFACE 
            SUBROUTINE AMMONIA_DON_PREFS(AMMONIA_DON_PREF,NH3,DON,      &
     &FRAC_AVAIL_DON,NOX,KN,NKN)
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8) :: AMMONIA_DON_PREF(NKN)
              REAL(KIND=8), INTENT(IN) :: NH3(NKN)
              REAL(KIND=8), INTENT(IN) :: DON(NKN)
              REAL(KIND=8), INTENT(IN) :: FRAC_AVAIL_DON
              REAL(KIND=8), INTENT(IN) :: NOX(NKN)
              REAL(KIND=8), INTENT(IN) :: KN
            END SUBROUTINE AMMONIA_DON_PREFS
          END INTERFACE 
        END MODULE AMMONIA_DON_PREFS__genmod
