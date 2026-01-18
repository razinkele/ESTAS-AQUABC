! Auxilary routines for the pelagic model

! Contents:

!subroutine ORGANIC_CARBON_DISSOLUTION
!subroutine ORGANIC_CARBON_MINERALIZATION

subroutine ORGANIC_CARBON_DISSOLUTION &
           (FAC_PHYT_DET_PART_ORG_C     , &
            KDISS_DET_PART_ORG_C_20     , &
            THETA_KDISS_DET_PART_ORG_C  , &
            KHS_POC_DISS_SAT            , &
            nkn                         , &
            TEMP                        , &
            DET_PART_ORG_C              , &
            PHYT_TOT_C                  , &
            LIM_PHYT_DISS_DET_PART_ORG_C, &
            R_DET_PART_ORG_C_DISSOLUTION)

    use AQUABC_II_GLOBAL
    implicit none

    real(kind = DBL_PREC), intent(in) :: FAC_PHYT_DET_PART_ORG_C
    real(kind = DBL_PREC), intent(in) :: KDISS_DET_PART_ORG_C_20
    real(kind = DBL_PREC), intent(in) :: THETA_KDISS_DET_PART_ORG_C
    real(kind = DBL_PREC), intent(in) :: KHS_POC_DISS_SAT

    integer, intent(in) :: nkn

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DET_PART_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PHYT_TOT_C

    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_PHYT_DISS_DET_PART_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_DET_PART_ORG_C_DISSOLUTION

    !Algal dependent hydrolysis rate
    LIM_PHYT_DISS_DET_PART_ORG_C = FAC_PHYT_DET_PART_ORG_C * PHYT_TOT_C

    R_DET_PART_ORG_C_DISSOLUTION = &
           (KDISS_DET_PART_ORG_C_20 + LIM_PHYT_DISS_DET_PART_ORG_C) * &
           (THETA_KDISS_DET_PART_ORG_C ** (TEMP - 2.0D1)) * DET_PART_ORG_C * &
           (KHS_POC_DISS_SAT/(DET_PART_ORG_C + KHS_POC_DISS_SAT))
end subroutine ORGANIC_CARBON_DISSOLUTION



subroutine ORGANIC_CARBON_MINERALIZATION &
           (FAC_PHYT_AMIN_DOC           , &
            K_MIN_DOC_DOXY_20           , &
            K_MIN_DOC_NO3N_20           , &
            K_MIN_DOC_MN_IV_20          , &
            K_MIN_DOC_FE_III_20         , &
            K_MIN_DOC_S_PLUS_6_20       , &
            K_MIN_DOC_DOC_20            , &
            THETA_K_MIN_DOC_DOXY        , &
            THETA_K_MIN_DOC_NO3N        , &
            THETA_K_MIN_DOC_MN_IV       , &
            THETA_K_MIN_DOC_FE_III      , &
            THETA_K_MIN_DOC_S_PLUS_6    , &
            THETA_K_MIN_DOC_DOC         , &
            K_HS_DOC_MIN_DOXY           , &
            K_HS_DOC_MIN_NO3N           , &
            K_HS_DOC_MIN_MN_IV          , &
            K_HS_DOC_MIN_FE_III         , &
            K_HS_DOC_MIN_S_PLUS_6       , &
            K_HS_DOC_MIN_DOC            , &
            K_HS_DOXY_RED_LIM           , &
            K_HS_NO3N_RED_LIM           , &
            K_HS_MN_IV_RED_LIM          , &
            K_HS_FE_III_RED_LIM         , &
            K_HS_S_PLUS_6_RED_LIM       , &
            K_HS_DOXY_RED_INHB          , &
            K_HS_NO3N_RED_INHB          , &
            K_HS_MN_IV_RED_INHB         , &
            K_HS_FE_III_RED_INHB        , &
            K_HS_S_PLUS_6_RED_INHB      , &
            PH_MIN_DOC_MIN_DOXY         , &  !Min. pH for the optimum pH range for DOC mineralization with DOXY     as final electron acceptor (subroutine input)
            PH_MIN_DOC_MIN_NO3N         , &  !Min. pH for the optimum pH range for DOC mineralization with NO3N     as final electron acceptor (subroutine input)
            PH_MIN_DOC_MIN_MN_IV        , &  !Min. pH for the optimum pH range for DOC mineralization with MN_IV    as final electron acceptor (subroutine input)
            PH_MIN_DOC_MIN_FE_III       , &  !Min. pH for the optimum pH range for DOC mineralization with FE_III   as final electron acceptor (subroutine input)
            PH_MIN_DOC_MIN_S_PLUS_6     , &  !Min. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor (subroutine input)
            PH_MIN_DOC_MIN_DOC          , &  !Min. pH for the optimum pH range for DOC mineralization with DOC      as final electron acceptor (subroutine input)
            PH_MAX_DOC_MIN_DOXY         , &  !Max. pH for the optimum pH range for DOC mineralization with DOXY     as final electron acceptor (subroutine input)
            PH_MAX_DOC_MIN_NO3N         , &  !Max. pH for the optimum pH range for DOC mineralization with NO3N     as final electron acceptor (subroutine input)
            PH_MAX_DOC_MIN_MN_IV        , &  !Max. pH for the optimum pH range for DOC mineralization with MN_IV    as final electron acceptor (subroutine input)
            PH_MAX_DOC_MIN_FE_III       , &  !Max. pH for the optimum pH range for DOC mineralization with FE_III   as final electron acceptor (subroutine input)
            PH_MAX_DOC_MIN_S_PLUS_6     , &  !Max. pH for the optimum pH range for DOC mineralization with S_PLUS_6 as final electron acceptor (subroutine input)
            PH_MAX_DOC_MIN_DOC          , &  !Max. pH for the optimum pH range for DOC mineralization with DOC      as final electron acceptor (subroutine input)
            nkn                         , &
            TEMP                        , &
            DISS_ORG_C                  , &
            PHYT_TOT_C                  , &
            DOXY                        , &
            NO3N                        , &
            MN_IV                       , &
            FE_III                      , &
            S_PLUS_6                    , &
            PH                          , &
            LIM_DOXY_RED                , &
            LIM_NO3N_RED                , &
            LIM_MN_IV_RED               , &
            LIM_FE_III_RED              , &
            LIM_S_PLUS_6_RED            , &
            LIM_DOC_RED                 , &
            LIM_PHYT_AMIN_DOC           , &
            PH_CORR_DOC_MIN_DOXY        , &  !pH correction for DOC mineralization with DOXY     as final electron acceptor (subroutine output)
            PH_CORR_DOC_MIN_NO3N        , &  !pH correction for DOC mineralization with NO3N     as final electron acceptor (subroutine output)
            PH_CORR_DOC_MIN_MN_IV       , &  !pH correction for DOC mineralization with MN_IV    as final electron acceptor (subroutine output)
            PH_CORR_DOC_MIN_FE_III      , &  !pH correction for DOC mineralization with FE_III   as final electron acceptor (subroutine output)
            PH_CORR_DOC_MIN_S_PLUS_6    , &  !pH correction for DOC mineralization with S_PLUS_6 as final electron acceptor (subroutine output)
            PH_CORR_DOC_MIN_DOC         , &  !pH correction for DOC mineralization with DOC      as final electron acceptor (subroutine output)
            K_NO3_RED                   , &
            K_MN_IV_RED                 , &
            K_FE_III_RED                , &
            K_S_PLUS_6_RED              , &
            K_DOC_RED                   , &
            R_ABIOTIC_DOC_MIN_DOXY      , &  !Process rate  for DOC mineralization with DOXY     as final electron acceptor (subroutine output)
            R_ABIOTIC_DOC_MIN_NO3N      , &  !Process rate  for DOC mineralization with NO3N     as final electron acceptor (subroutine output)
            R_ABIOTIC_DOC_MIN_MN_IV     , &  !Process rate  for DOC mineralization with MN_IV    as final electron acceptor (subroutine output)
            R_ABIOTIC_DOC_MIN_FE_III    , &  !Process rate  for DOC mineralization with FE_III   as final electron acceptor (subroutine output)
            R_ABIOTIC_DOC_MIN_S_PLUS_6  , &  !Process rate  for DOC mineralization with S_PLUS_6 as final electron acceptor (subroutine output)
            R_ABIOTIC_DOC_MIN_DOC)           !Process rate  for DOC mineralization with DOC      as final electron acceptor (subroutine output)

    ! ----------------------------------------------------------------------------------------
    ! Subroutine for organic carbon mineraliztion
    ! This subroutine is almost completely rewritten to be compitable with the redox sequences
    ! ----------------------------------------------------------------------------------------
    use AQUABC_II_GLOBAL
    implicit none

    real(kind = DBL_PREC), intent(in) :: FAC_PHYT_AMIN_DOC

    real(kind = DBL_PREC), intent(in) :: K_MIN_DOC_DOXY_20
    real(kind = DBL_PREC), intent(in) :: K_MIN_DOC_NO3N_20
    real(kind = DBL_PREC), intent(in) :: K_MIN_DOC_MN_IV_20
    real(kind = DBL_PREC), intent(in) :: K_MIN_DOC_FE_III_20
    real(kind = DBL_PREC), intent(in) :: K_MIN_DOC_S_PLUS_6_20
    real(kind = DBL_PREC), intent(in) :: K_MIN_DOC_DOC_20
    real(kind = DBL_PREC), intent(in) :: THETA_K_MIN_DOC_DOXY
    real(kind = DBL_PREC), intent(in) :: THETA_K_MIN_DOC_NO3N
    real(kind = DBL_PREC), intent(in) :: THETA_K_MIN_DOC_MN_IV
    real(kind = DBL_PREC), intent(in) :: THETA_K_MIN_DOC_FE_III
    real(kind = DBL_PREC), intent(in) :: THETA_K_MIN_DOC_S_PLUS_6
    real(kind = DBL_PREC), intent(in) :: THETA_K_MIN_DOC_DOC
    real(kind = DBL_PREC), intent(in) :: K_HS_DOC_MIN_DOXY
    real(kind = DBL_PREC), intent(in) :: K_HS_DOC_MIN_NO3N
    real(kind = DBL_PREC), intent(in) :: K_HS_DOC_MIN_MN_IV
    real(kind = DBL_PREC), intent(in) :: K_HS_DOC_MIN_FE_III
    real(kind = DBL_PREC), intent(in) :: K_HS_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), intent(in) :: K_HS_DOC_MIN_DOC
    real(kind = DBL_PREC), intent(in) :: K_HS_DOXY_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_NO3N_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_MN_IV_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_FE_III_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_S_PLUS_6_RED_LIM
    real(kind = DBL_PREC), intent(in) :: K_HS_DOXY_RED_INHB
    real(kind = DBL_PREC), intent(in) :: K_HS_NO3N_RED_INHB
    real(kind = DBL_PREC), intent(in) :: K_HS_MN_IV_RED_INHB
    real(kind = DBL_PREC), intent(in) :: K_HS_FE_III_RED_INHB
    real(kind = DBL_PREC), intent(in) :: K_HS_S_PLUS_6_RED_INHB
    real(kind = DBL_PREC), intent(in) :: PH_MIN_DOC_MIN_DOXY
    real(kind = DBL_PREC), intent(in) :: PH_MIN_DOC_MIN_NO3N
    real(kind = DBL_PREC), intent(in) :: PH_MIN_DOC_MIN_MN_IV
    real(kind = DBL_PREC), intent(in) :: PH_MIN_DOC_MIN_FE_III
    real(kind = DBL_PREC), intent(in) :: PH_MIN_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), intent(in) :: PH_MIN_DOC_MIN_DOC
    real(kind = DBL_PREC), intent(in) :: PH_MAX_DOC_MIN_DOXY
    real(kind = DBL_PREC), intent(in) :: PH_MAX_DOC_MIN_NO3N
    real(kind = DBL_PREC), intent(in) :: PH_MAX_DOC_MIN_MN_IV
    real(kind = DBL_PREC), intent(in) :: PH_MAX_DOC_MIN_FE_III
    real(kind = DBL_PREC), intent(in) :: PH_MAX_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), intent(in) :: PH_MAX_DOC_MIN_DOC

    integer, intent(in) :: nkn

    real(kind = DBL_PREC), dimension(nkn), intent(in) :: TEMP
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DISS_ORG_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PHYT_TOT_C
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: NO3N
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: MN_IV
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: FE_III
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: S_PLUS_6
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: PH
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: LIM_DOXY_RED
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: LIM_NO3N_RED
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: LIM_MN_IV_RED
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: LIM_FE_III_RED
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: LIM_S_PLUS_6_RED
    real(kind = DBL_PREC), dimension(nkn), intent(in) :: LIM_DOC_RED

    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: LIM_PHYT_AMIN_DOC
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PH_CORR_DOC_MIN_DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PH_CORR_DOC_MIN_NO3N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PH_CORR_DOC_MIN_MN_IV
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PH_CORR_DOC_MIN_FE_III
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PH_CORR_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PH_CORR_DOC_MIN_DOC
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ABIOTIC_DOC_MIN_DOXY
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ABIOTIC_DOC_MIN_NO3N
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ABIOTIC_DOC_MIN_MN_IV
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ABIOTIC_DOC_MIN_FE_III
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ABIOTIC_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: R_ABIOTIC_DOC_MIN_DOC
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: K_NO3_RED
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: K_MN_IV_RED
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: K_FE_III_RED
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: K_S_PLUS_6_RED
    real(kind = DBL_PREC), dimension(nkn), intent(inout) :: K_DOC_RED


    LIM_PHYT_AMIN_DOC = FAC_PHYT_AMIN_DOC * PHYT_TOT_C

    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_DOXY    , PH, PH_MIN_DOC_MIN_DOXY    , PH_MAX_DOC_MIN_DOXY    , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_NO3N    , PH, PH_MIN_DOC_MIN_NO3N    , PH_MAX_DOC_MIN_NO3N    , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_MN_IV   , PH, PH_MIN_DOC_MIN_MN_IV   , PH_MAX_DOC_MIN_MN_IV   , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_FE_III  , PH, PH_MIN_DOC_MIN_FE_III  , PH_MAX_DOC_MIN_FE_III  , nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_S_PLUS_6, PH, PH_MIN_DOC_MIN_S_PLUS_6, PH_MAX_DOC_MIN_S_PLUS_6, nkn)
    call CALCULATE_PH_CORR(PH_CORR_DOC_MIN_DOC     , PH, PH_MIN_DOC_MIN_DOC     , PH_MAX_DOC_MIN_DOC     , nkn)

    R_ABIOTIC_DOC_MIN_DOXY = &
        (K_MIN_DOC_DOXY_20 + LIM_PHYT_AMIN_DOC) * (THETA_K_MIN_DOC_DOXY ** (TEMP - 2.0D1)) * &
        LIM_DOXY_RED * PH_CORR_DOC_MIN_DOXY * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_DOXY)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_NO3N = &
        K_MIN_DOC_NO3N_20  * (THETA_K_MIN_DOC_NO3N ** (TEMP - 2.0D1)) * &
        LIM_NO3N_RED * PH_CORR_DOC_MIN_NO3N * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_NO3N)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_MN_IV = &
        K_MIN_DOC_MN_IV_20  * (THETA_K_MIN_DOC_MN_IV ** (TEMP - 2.0D1)) * &
        LIM_MN_IV_RED * PH_CORR_DOC_MIN_MN_IV * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_MN_IV)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_FE_III = &
        K_MIN_DOC_FE_III_20  * (THETA_K_MIN_DOC_FE_III ** (TEMP - 2.0D1)) * &
        LIM_FE_III_RED * PH_CORR_DOC_MIN_FE_III * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_FE_III)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_S_PLUS_6 = &
        K_MIN_DOC_S_PLUS_6_20  * (THETA_K_MIN_DOC_S_PLUS_6 ** (TEMP - 2.0D1)) * &
        LIM_S_PLUS_6_RED * PH_CORR_DOC_MIN_S_PLUS_6 * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_S_PLUS_6)) * &
        DISS_ORG_C

    R_ABIOTIC_DOC_MIN_DOC = &
        (K_MIN_DOC_DOC_20  * (THETA_K_MIN_DOC_DOC ** (TEMP - 2.0D1)) * &
         LIM_DOC_RED * PH_CORR_DOC_MIN_DOXY * (DISS_ORG_C / (DISS_ORG_C + K_HS_DOC_MIN_DOC)) * DISS_ORG_C)

end subroutine ORGANIC_CARBON_MINERALIZATION
