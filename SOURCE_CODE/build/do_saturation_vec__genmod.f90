        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:44 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DO_SATURATION_VEC__genmod
          INTERFACE 
            SUBROUTINE DO_SATURATION_VEC(T,S,H,NKN,CS)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: T(NKN)
              REAL(KIND=8), INTENT(IN) :: S(NKN)
              REAL(KIND=8), INTENT(IN) :: H(NKN)
              REAL(KIND=8) :: CS(NKN)
            END SUBROUTINE DO_SATURATION_VEC
          END INTERFACE 
        END MODULE DO_SATURATION_VEC__genmod
