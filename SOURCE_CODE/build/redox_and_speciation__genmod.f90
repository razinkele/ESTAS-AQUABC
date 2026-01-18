        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:46 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE REDOX_AND_SPECIATION__genmod
          INTERFACE 
            SUBROUTINE REDOX_AND_SPECIATION(DOXY,NO3N,MN_IV,FE_III,     &
     &S_PLUS_6,DISS_ORG_C,S_MINUS_2,MN_II,FE_II,HCO3,CO3,TEMP,SALT,PH,  &
     &ELEVATION,K_HS_DOXY_RED_LIM,K_HS_NO3N_RED_LIM,K_HS_MN_IV_RED_LIM, &
     &K_HS_FE_III_RED_LIM,K_HS_S_PLUS_6_RED_LIM,K_HS_DOXY_RED_INHB,     &
     &K_HS_NO3N_RED_INHB,K_HS_MN_IV_RED_INHB,K_HS_FE_III_RED_INHB,      &
     &K_HS_S_PLUS_6_RED_INHB,NKN,LIM_DOXY_RED,LIM_NO3N_RED,LIM_MN_IV_RED&
     &,LIM_FE_III_RED,LIM_S_PLUS_6_RED,LIM_DOC_RED,PE,FE_II_DISS,       &
     &FE_III_DISS,MN_II_DISS)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: DOXY(NKN)
              REAL(KIND=8), INTENT(IN) :: NO3N(NKN)
              REAL(KIND=8), INTENT(IN) :: MN_IV(NKN)
              REAL(KIND=8), INTENT(IN) :: FE_III(NKN)
              REAL(KIND=8), INTENT(IN) :: S_PLUS_6(NKN)
              REAL(KIND=8), INTENT(IN) :: DISS_ORG_C(NKN)
              REAL(KIND=8), INTENT(IN) :: S_MINUS_2(NKN)
              REAL(KIND=8), INTENT(IN) :: MN_II(NKN)
              REAL(KIND=8), INTENT(IN) :: FE_II(NKN)
              REAL(KIND=8), INTENT(IN) :: HCO3(NKN)
              REAL(KIND=8), INTENT(IN) :: CO3(NKN)
              REAL(KIND=8), INTENT(IN) :: TEMP(NKN)
              REAL(KIND=8), INTENT(IN) :: SALT(NKN)
              REAL(KIND=8), INTENT(IN) :: PH(NKN)
              REAL(KIND=8), INTENT(IN) :: ELEVATION(NKN)
              REAL(KIND=8), INTENT(IN) :: K_HS_DOXY_RED_LIM
              REAL(KIND=8), INTENT(IN) :: K_HS_NO3N_RED_LIM
              REAL(KIND=8), INTENT(IN) :: K_HS_MN_IV_RED_LIM
              REAL(KIND=8), INTENT(IN) :: K_HS_FE_III_RED_LIM
              REAL(KIND=8), INTENT(IN) :: K_HS_S_PLUS_6_RED_LIM
              REAL(KIND=8), INTENT(IN) :: K_HS_DOXY_RED_INHB
              REAL(KIND=8), INTENT(IN) :: K_HS_NO3N_RED_INHB
              REAL(KIND=8), INTENT(IN) :: K_HS_MN_IV_RED_INHB
              REAL(KIND=8), INTENT(IN) :: K_HS_FE_III_RED_INHB
              REAL(KIND=8), INTENT(IN) :: K_HS_S_PLUS_6_RED_INHB
              REAL(KIND=8), INTENT(INOUT) :: LIM_DOXY_RED(NKN)
              REAL(KIND=8), INTENT(INOUT) :: LIM_NO3N_RED(NKN)
              REAL(KIND=8), INTENT(INOUT) :: LIM_MN_IV_RED(NKN)
              REAL(KIND=8), INTENT(INOUT) :: LIM_FE_III_RED(NKN)
              REAL(KIND=8), INTENT(INOUT) :: LIM_S_PLUS_6_RED(NKN)
              REAL(KIND=8), INTENT(INOUT) :: LIM_DOC_RED(NKN)
              REAL(KIND=8), INTENT(INOUT) :: PE(NKN)
              REAL(KIND=8), INTENT(INOUT) :: FE_II_DISS(NKN)
              REAL(KIND=8), INTENT(INOUT) :: FE_III_DISS(NKN)
              REAL(KIND=8), INTENT(INOUT) :: MN_II_DISS(NKN)
            END SUBROUTINE REDOX_AND_SPECIATION
          END INTERFACE 
        END MODULE REDOX_AND_SPECIATION__genmod
