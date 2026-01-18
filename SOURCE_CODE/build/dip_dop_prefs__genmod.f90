        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:51 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DIP_DOP_PREFS__genmod
          INTERFACE 
            SUBROUTINE DIP_DOP_PREFS(DIP_DOP_PREF,AVAIL_DIP,AVAIL_DOP,KP&
     &,NKN)
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8) :: DIP_DOP_PREF(NKN)
              REAL(KIND=8), INTENT(IN) :: AVAIL_DIP(NKN)
              REAL(KIND=8), INTENT(IN) :: AVAIL_DOP(NKN)
              REAL(KIND=8), INTENT(IN) :: KP
            END SUBROUTINE DIP_DOP_PREFS
          END INTERFACE 
        END MODULE DIP_DOP_PREFS__genmod
