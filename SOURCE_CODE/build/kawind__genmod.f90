        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:51 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE KAWIND__genmod
          INTERFACE 
            FUNCTION KAWIND(WINDS,TW,TA,DEPTH,WTYPE)
              REAL(KIND=8), INTENT(IN) :: WINDS
              REAL(KIND=8), INTENT(IN) :: TW
              REAL(KIND=8), INTENT(IN) :: TA
              REAL(KIND=8), INTENT(IN) :: DEPTH
              REAL(KIND=8), INTENT(IN) :: WTYPE
              REAL(KIND=8) :: KAWIND
            END FUNCTION KAWIND
          END INTERFACE 
        END MODULE KAWIND__genmod
