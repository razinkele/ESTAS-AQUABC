        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:51 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GROWTH_AT_TEMP__genmod
          INTERFACE 
            SUBROUTINE GROWTH_AT_TEMP(TEMP,LIM_TEMP_GROWTH,LOWER_TEMP,  &
     &UPPER_TEMP,K_AT_OPT_TEMP,KAPPA_UNDER_OPT_TEMP,KAPPA_OVER_OPT_TEMP,&
     &NKN)
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8), INTENT(IN) :: TEMP(NKN)
              REAL(KIND=8) :: LIM_TEMP_GROWTH(NKN)
              REAL(KIND=8), INTENT(IN) :: LOWER_TEMP
              REAL(KIND=8), INTENT(IN) :: UPPER_TEMP
              REAL(KIND=8), INTENT(IN) :: K_AT_OPT_TEMP
              REAL(KIND=8), INTENT(IN) :: KAPPA_UNDER_OPT_TEMP
              REAL(KIND=8), INTENT(IN) :: KAPPA_OVER_OPT_TEMP
            END SUBROUTINE GROWTH_AT_TEMP
          END INTERFACE 
        END MODULE GROWTH_AT_TEMP__genmod
