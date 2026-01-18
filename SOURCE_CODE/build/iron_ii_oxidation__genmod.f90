        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:46 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE IRON_II_OXIDATION__genmod
          INTERFACE 
            SUBROUTINE IRON_II_OXIDATION(FE_II_DISS,DOXY,PH,TEMP,SALT,  &
     &ELEVATION,NKN,R_FE_II_OXIDATION)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: FE_II_DISS(NKN)
              REAL(KIND=8), INTENT(IN) :: DOXY(NKN)
              REAL(KIND=8), INTENT(IN) :: PH(NKN)
              REAL(KIND=8), INTENT(IN) :: TEMP(NKN)
              REAL(KIND=8), INTENT(IN) :: SALT(NKN)
              REAL(KIND=8), INTENT(IN) :: ELEVATION(NKN)
              REAL(KIND=8), INTENT(INOUT) :: R_FE_II_OXIDATION(NKN)
            END SUBROUTINE IRON_II_OXIDATION
          END INTERFACE 
        END MODULE IRON_II_OXIDATION__genmod
