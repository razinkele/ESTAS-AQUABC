        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:46 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE IP_SOLUBLE_FRACTION__genmod
          INTERFACE 
            SUBROUTINE IP_SOLUBLE_FRACTION(FE_III,PO4P,K_A_1,K_A_2,K_A_3&
     &,PH,NKN,NLAYERS,DIP_OVER_IP)
              INTEGER(KIND=4), INTENT(IN) :: NLAYERS
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: FE_III(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: PO4P(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: K_A_1(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: K_A_2(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: K_A_3(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: PH(NKN,NLAYERS)
              REAL(KIND=8), INTENT(INOUT) :: DIP_OVER_IP(NKN,NLAYERS)
            END SUBROUTINE IP_SOLUBLE_FRACTION
          END INTERFACE 
        END MODULE IP_SOLUBLE_FRACTION__genmod
