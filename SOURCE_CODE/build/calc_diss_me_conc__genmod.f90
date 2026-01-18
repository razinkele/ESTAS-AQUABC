        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:46 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CALC_DISS_ME_CONC__genmod
          INTERFACE 
            SUBROUTINE CALC_DISS_ME_CONC(TOT_ME,ME_DISS_INIT,ME_SOLUB_EQ&
     &,K_DISS_ME,T,NKN,NLAYERS,DISS_ME_CONC_TS_END,DISS_ME_CONC_TS_AVG)
              INTEGER(KIND=4), INTENT(IN) :: NLAYERS
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: TOT_ME(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: ME_DISS_INIT(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: ME_SOLUB_EQ(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: K_DISS_ME(NKN,NLAYERS)
              REAL(KIND=8), INTENT(IN) :: T
              REAL(KIND=8), INTENT(INOUT) :: DISS_ME_CONC_TS_END(NKN,   &
     &NLAYERS)
              REAL(KIND=8), INTENT(INOUT) :: DISS_ME_CONC_TS_AVG(NKN,   &
     &NLAYERS)
            END SUBROUTINE CALC_DISS_ME_CONC
          END INTERFACE 
        END MODULE CALC_DISS_ME_CONC__genmod
