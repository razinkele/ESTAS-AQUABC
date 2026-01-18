        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:44 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHLOROPHYL_A_VEC__genmod
          INTERFACE 
            SUBROUTINE CHLOROPHYL_A_VEC(DIA_C,CYN_C,OPA_C,FIX_CYN_C,    &
     &NOST_VEG_HET_C,DIA_C_TO_CHLA,CYN_C_TO_CHLA,OPA_C_TO_CHLA,         &
     &FIX_CYN_C_TO_CHLA,NOST_C_TO_CHLA,NKN,CHLA,                        &
     &CONSIDER_NON_OBLIGATORY_FIXERS,CONSIDER_NOSTOCALES)
              INTEGER(KIND=4), INTENT(IN) :: NKN
              REAL(KIND=8), INTENT(IN) :: DIA_C(NKN)
              REAL(KIND=8), INTENT(IN) :: CYN_C(NKN)
              REAL(KIND=8), INTENT(IN) :: OPA_C(NKN)
              REAL(KIND=8), INTENT(IN) :: FIX_CYN_C(NKN)
              REAL(KIND=8), INTENT(IN) :: NOST_VEG_HET_C(NKN)
              REAL(KIND=8), INTENT(IN) :: DIA_C_TO_CHLA
              REAL(KIND=8), INTENT(IN) :: CYN_C_TO_CHLA
              REAL(KIND=8), INTENT(IN) :: OPA_C_TO_CHLA
              REAL(KIND=8), INTENT(IN) :: FIX_CYN_C_TO_CHLA
              REAL(KIND=8), INTENT(IN) :: NOST_C_TO_CHLA
              REAL(KIND=8), INTENT(INOUT) :: CHLA(NKN)
              INTEGER(KIND=4), INTENT(IN) ::                            &
     &CONSIDER_NON_OBLIGATORY_FIXERS
              INTEGER(KIND=4), INTENT(IN) :: CONSIDER_NOSTOCALES
            END SUBROUTINE CHLOROPHYL_A_VEC
          END INTERFACE 
        END MODULE CHLOROPHYL_A_VEC__genmod
