        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:46 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ORGANIC_CARBON_DISSOLUTION__genmod
          INTERFACE 
            SUBROUTINE ORGANIC_CARBON_DISSOLUTION(                      &
     &FAC_PHYT_DET_PART_ORG_C,KDISS_DET_PART_ORG_C_20,                  &
     &THETA_KDISS_DET_PART_ORG_C,KHS_POC_DISS_SAT,NKN,TEMP,             &
     &DET_PART_ORG_C,PHYT_TOT_C,LIM_PHYT_DISS_DET_PART_ORG_C,           &
     &R_DET_PART_ORG_C_DISSOLUTION)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: FAC_PHYT_DET_PART_ORG_C
              REAL(KIND=8), INTENT(IN) :: KDISS_DET_PART_ORG_C_20
              REAL(KIND=8), INTENT(IN) :: THETA_KDISS_DET_PART_ORG_C
              REAL(KIND=8), INTENT(IN) :: KHS_POC_DISS_SAT
              REAL(KIND=8), INTENT(IN) :: TEMP(NKN)
              REAL(KIND=8), INTENT(IN) :: DET_PART_ORG_C(NKN)
              REAL(KIND=8), INTENT(IN) :: PHYT_TOT_C(NKN)
              REAL(KIND=8), INTENT(INOUT) ::                            &
     &LIM_PHYT_DISS_DET_PART_ORG_C(NKN)
              REAL(KIND=8), INTENT(INOUT) ::                            &
     &R_DET_PART_ORG_C_DISSOLUTION(NKN)
            END SUBROUTINE ORGANIC_CARBON_DISSOLUTION
          END INTERFACE 
        END MODULE ORGANIC_CARBON_DISSOLUTION__genmod
