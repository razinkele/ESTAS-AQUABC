        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:44 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETTL_VEL_VEC__genmod
          INTERFACE 
            SUBROUTINE SETTL_VEL_VEC(CHLA,NKN,VELS)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: CHLA(NKN)
              REAL(KIND=8), INTENT(INOUT) :: VELS(NKN)
            END SUBROUTINE SETTL_VEL_VEC
          END INTERFACE 
        END MODULE SETTL_VEL_VEC__genmod
