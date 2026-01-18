        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:44 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SED_POC_DISSOLUTION__genmod
          INTERFACE 
            SUBROUTINE SED_POC_DISSOLUTION(K_OXIC_DISS_POC,             &
     &K_ANOXIC_DISS_POC,THETA_DISS_POC,KHS_DISS_POC,DOXY_AT_ANOXIA,     &
     &SED_POC,SED_DOXY,SED_TEMPS,NKN,NUM_SED_LAYERS,R_DISS_POC)
              INTEGER(KIND=4) :: NUM_SED_LAYERS
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8), INTENT(IN) :: K_OXIC_DISS_POC
              REAL(KIND=8), INTENT(IN) :: K_ANOXIC_DISS_POC
              REAL(KIND=8), INTENT(IN) :: THETA_DISS_POC
              REAL(KIND=8), INTENT(IN) :: KHS_DISS_POC
              REAL(KIND=8), INTENT(IN) :: DOXY_AT_ANOXIA
              REAL(KIND=8), INTENT(IN) :: SED_POC(NKN,NUM_SED_LAYERS)
              REAL(KIND=8), INTENT(IN) :: SED_DOXY(NKN,NUM_SED_LAYERS)
              REAL(KIND=8), INTENT(IN) :: SED_TEMPS(NKN,NUM_SED_LAYERS)
              REAL(KIND=8), INTENT(INOUT) :: R_DISS_POC(NKN,            &
     &NUM_SED_LAYERS)
            END SUBROUTINE SED_POC_DISSOLUTION
          END INTERFACE 
        END MODULE SED_POC_DISSOLUTION__genmod
