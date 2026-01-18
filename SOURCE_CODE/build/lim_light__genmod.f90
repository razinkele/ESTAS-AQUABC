        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:52 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LIM_LIGHT__genmod
          INTERFACE 
            SUBROUTINE LIM_LIGHT(IA,TCHLA,GITMAX,H,KE,LLIGHT,CCHL_RATIO,&
     &K_LIGHT_SAT,LIGHT_SAT,NKN)
              INTEGER(KIND=4) :: NKN
              REAL(KIND=8) :: IA(NKN)
              REAL(KIND=8) :: TCHLA(NKN)
              REAL(KIND=8) :: GITMAX(NKN)
              REAL(KIND=8) :: H(NKN)
              REAL(KIND=8) :: KE(NKN)
              REAL(KIND=8) :: LLIGHT(NKN)
              REAL(KIND=8) :: CCHL_RATIO
              REAL(KIND=8) :: K_LIGHT_SAT
              REAL(KIND=8) :: LIGHT_SAT(NKN)
            END SUBROUTINE LIM_LIGHT
          END INTERFACE 
        END MODULE LIM_LIGHT__genmod
