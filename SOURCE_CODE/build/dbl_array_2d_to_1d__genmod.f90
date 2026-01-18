        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 18 10:38:48 2026
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DBL_ARRAY_2D_TO_1D__genmod
          INTERFACE 
            FUNCTION DBL_ARRAY_2D_TO_1D(TWO_D) RESULT(ONE_D)
              REAL(KIND=8), INTENT(IN) :: TWO_D(:,:)
              REAL(KIND=8) ,ALLOCATABLE :: ONE_D(:)
            END FUNCTION DBL_ARRAY_2D_TO_1D
          END INTERFACE 
        END MODULE DBL_ARRAY_2D_TO_1D__genmod
