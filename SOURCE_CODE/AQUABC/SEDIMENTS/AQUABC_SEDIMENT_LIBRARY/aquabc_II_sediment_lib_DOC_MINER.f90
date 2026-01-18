! Content:
!subroutine SED_DOC_MINERALIZATION

subroutine SED_DOC_MINERALIZATION &
           (SED_K_MIN_DOC_DOXY_20       , &
            SED_K_MIN_DOC_NO3N_20       , &
            SED_K_MIN_DOC_MN_IV_20      , &
            SED_K_MIN_DOC_FE_III_20     , &
            SED_K_MIN_DOC_S_PLUS_6_20   , &
            SED_K_MIN_DOC_DOC_20        , &
            SED_THETA_K_MIN_DOC_DOXY    , &
            SED_THETA_K_MIN_DOC_NO3N    , &
            SED_THETA_K_MIN_DOC_MN_IV   , &
            SED_THETA_K_MIN_DOC_FE_III  , &
            SED_THETA_K_MIN_DOC_S_PLUS_6, &
            SED_THETA_K_MIN_DOC_DOC     , &
            SED_K_HS_DOC_MIN_DOXY       , &
            SED_K_HS_DOC_MIN_NO3N       , &
            SED_K_HS_DOC_MIN_MN_IV      , &
            SED_K_HS_DOC_MIN_FE_III     , &
            SED_K_HS_DOC_MIN_S_PLUS_6   , &
            SED_K_HS_DOC_MIN_DOC        , &
            SED_K_HS_DOXY_RED_LIM       , &
            SED_K_HS_NO3N_RED_LIM       , &
            SED_K_HS_MN_IV_RED_LIM      , &
            SED_K_HS_FE_III_RED_LIM     , &
            SED_K_HS_S_PLUS_6_RED_LIM   , &
            SED_K_HS_DOXY_RED_INHB      , &
            SED_K_HS_NO3N_RED_INHB      , &
            SED_K_HS_MN_IV_RED_INHB     , &
            SED_K_HS_FE_III_RED_INHB    , &
            SED_K_HS_S_PLUS_6_RED_INHB  , &
            SED_PH_MIN_DOC_MIN_DOXY     , &
            SED_PH_MIN_DOC_MIN_NO3N     , &
            SED_PH_MIN_DOC_MIN_MN_IV    , &
            SED_PH_MIN_DOC_MIN_FE_III   , &
            SED_PH_MIN_DOC_MIN_S_PLUS_6 , &
            SED_PH_MIN_DOC_MIN_DOC      , &
            SED_PH_MAX_DOC_MIN_DOXY     , &
            SED_PH_MAX_DOC_MIN_NO3N     , &
            SED_PH_MAX_DOC_MIN_MN_IV    , &
            SED_PH_MAX_DOC_MIN_FE_III   , &
            SED_PH_MAX_DOC_MIN_S_PLUS_6 , &
            SED_PH_MAX_DOC_MIN_DOC      , &
            SED_TEMPS                   , &
            SED_DOC                     , &
            SED_DOXY                    , &
            SED_NO3N                    , &
            MN_IV_DISS                  , &
            FE_III_DISS                 , &
            S_PLUS_6                    , &
            PH                          , &
            nkn                         , &
            NUM_SED_LAYERS              , &
            PH_CORR_DOC_MIN_DOXY        , &
            PH_CORR_DOC_MIN_NO3N        , &
            PH_CORR_DOC_MIN_MN_IV       , &
            PH_CORR_DOC_MIN_FE_III      , &
            PH_CORR_DOC_MIN_S_PLUS_6    , &
            PH_CORR_DOC_MIN_DOC         , &
            LIM_DOXY_RED                , &
            LIM_NO3N_RED                , &
            LIM_MN_IV_RED               , &
            LIM_FE_III_RED              , &
            LIM_S_PLUS_6_RED            , &
            LIM_DOC_RED                 , &
            K_NO3_RED                   , &
            K_MN_IV_RED                 , &
            K_FE_III_RED                , &
            K_S_PLUS_6_RED              , &
            K_DOC_RED                   , &
            R_MINER_DOC_DOXY            , &
            R_MINER_DOC_NO3N            , &
            R_MINER_DOC_MN_IV           , &
            R_MINER_DOC_FE_III          , &
            R_MINER_DOC_S_PLUS_6        , &
            R_MINER_DOC_DOC)

    use AQUABC_II_GLOBAL
    implicit none

    real(kind = DBL_PREC), intent(in) :: SED_K_MIN_DOC_DOXY_20
    real(kind = DBL_PREC), intent(in) :: SED_K_MIN_DOC_NO3N_20
    real(kind = DBL_PREC), intent(in) :: SED_K_MIN_DOC_MN_IV_20
    real(kind = DBL_PREC), intent(in) :: SED_K_MIN_DOC_FE_III_20
    real(kind = DBL_PREC), intent(in) :: SED_K_MIN_DOC_S_PLUS_6_20
    real(kind = DBL_PREC), intent(in) :: SED_K_MIN_DOC_DOC_20
    real(kind = DBL_PREC), intent(in) :: SED_THETA_K_MIN_DOC_DOXY
    real(kind = DBL_PREC), intent(in) :: SED_THETA_K_MIN_DOC_NO3N
    real(kind = DBL_PREC), intent(in) :: SED_THETA_K_MIN_DOC_MN_IV
    real(kind = DBL_PREC), intent(in) :: SED_THETA_K_MIN_DOC_FE_III
    real(kind = DBL_PREC), intent(in) :: SED_THETA_K_MIN_DOC_S_PLUS_6
    real(kind = DBL_PREC), intent(in) :: SED_THETA_K_MIN_DOC_DOC
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_DOC_MIN_DOXY
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_DOC_MIN_NO3N
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_DOC_MIN_MN_IV
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_DOC_MIN_FE_III
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_DOC_MIN_DOC
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_DOXY_RED_LIM
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_NO3N_RED_LIM
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_MN_IV_RED_LIM
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_FE_III_RED_LIM
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_S_PLUS_6_RED_LIM
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_DOXY_RED_INHB
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_NO3N_RED_INHB
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_MN_IV_RED_INHB
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_FE_III_RED_INHB
    real(kind = DBL_PREC), intent(in) :: SED_K_HS_S_PLUS_6_RED_INHB
    real(kind = DBL_PREC), intent(in) :: SED_PH_MIN_DOC_MIN_DOXY
    real(kind = DBL_PREC), intent(in) :: SED_PH_MIN_DOC_MIN_NO3N
    real(kind = DBL_PREC), intent(in) :: SED_PH_MIN_DOC_MIN_MN_IV
    real(kind = DBL_PREC), intent(in) :: SED_PH_MIN_DOC_MIN_FE_III
    real(kind = DBL_PREC), intent(in) :: SED_PH_MIN_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), intent(in) :: SED_PH_MIN_DOC_MIN_DOC
    real(kind = DBL_PREC), intent(in) :: SED_PH_MAX_DOC_MIN_DOXY
    real(kind = DBL_PREC), intent(in) :: SED_PH_MAX_DOC_MIN_NO3N
    real(kind = DBL_PREC), intent(in) :: SED_PH_MAX_DOC_MIN_MN_IV
    real(kind = DBL_PREC), intent(in) :: SED_PH_MAX_DOC_MIN_FE_III
    real(kind = DBL_PREC), intent(in) :: SED_PH_MAX_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), intent(in) :: SED_PH_MAX_DOC_MIN_DOC

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: SED_TEMPS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: SED_DOC
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: SED_DOXY
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: SED_NO3N
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: MN_IV_DISS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: FE_III_DISS
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: S_PLUS_6
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: PH

    integer :: nkn
    integer :: NUM_SED_LAYERS

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: PH_CORR_DOC_MIN_DOXY
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: PH_CORR_DOC_MIN_NO3N
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: PH_CORR_DOC_MIN_MN_IV
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: PH_CORR_DOC_MIN_FE_III
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: PH_CORR_DOC_MIN_S_PLUS_6
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: PH_CORR_DOC_MIN_DOC
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_DOXY_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_NO3N_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_MN_IV_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_FE_III_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_S_PLUS_6_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: LIM_DOC_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: K_NO3_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: K_MN_IV_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: K_FE_III_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: K_S_PLUS_6_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: K_DOC_RED
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: R_MINER_DOC_DOXY
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: R_MINER_DOC_NO3N
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: R_MINER_DOC_MN_IV
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: R_MINER_DOC_FE_III
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: R_MINER_DOC_S_PLUS_6
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: R_MINER_DOC_DOC


    call CALCULATE_PH_CORR_SED &
             (PH_CORR_DOC_MIN_DOXY     , PH , SED_PH_MIN_DOC_MIN_DOXY  , &
              SED_PH_MAX_DOC_MIN_DOXY  , nkn, NUM_SED_LAYERS)

    call CALCULATE_PH_CORR_SED &
             (PH_CORR_DOC_MIN_NO3N     , PH , SED_PH_MIN_DOC_MIN_NO3N  , &
              SED_PH_MAX_DOC_MIN_NO3N  , nkn, NUM_SED_LAYERS)

    call CALCULATE_PH_CORR_SED &
             (PH_CORR_DOC_MIN_MN_IV    , PH , SED_PH_MIN_DOC_MIN_MN_IV , &
              SED_PH_MAX_DOC_MIN_MN_IV , nkn, NUM_SED_LAYERS)

    call CALCULATE_PH_CORR_SED &
             (PH_CORR_DOC_MIN_FE_III   , PH , SED_PH_MIN_DOC_MIN_FE_III, &
              SED_PH_MAX_DOC_MIN_FE_III, nkn, NUM_SED_LAYERS)

    call CALCULATE_PH_CORR_SED &
	     (PH_CORR_DOC_MIN_S_PLUS_6   , PH , SED_PH_MIN_DOC_MIN_S_PLUS_6, &
		  SED_PH_MAX_DOC_MIN_S_PLUS_6, nkn, NUM_SED_LAYERS)

    call CALCULATE_PH_CORR_SED &
	     (PH_CORR_DOC_MIN_DOC        , PH, SED_PH_MIN_DOC_MIN_DOC      , &
		  SED_PH_MAX_DOC_MIN_DOC     , nkn, NUM_SED_LAYERS)

    LIM_DOXY_RED = SED_DOXY  / (SED_DOXY + SED_K_HS_DOXY_RED_LIM)

    LIM_NO3N_RED = (SED_NO3N / (SED_NO3N + SED_K_HS_NO3N_RED_LIM)) * &
        (SED_K_HS_DOXY_RED_INHB / (SED_DOXY + SED_K_HS_DOXY_RED_INHB))

    LIM_MN_IV_RED = (MN_IV_DISS  / (MN_IV_DISS + SED_K_HS_MN_IV_RED_LIM)) * &
        (SED_K_HS_DOXY_RED_INHB / (SED_DOXY + SED_K_HS_DOXY_RED_INHB))            * &
        (SED_K_HS_NO3N_RED_INHB / (SED_NO3N + SED_K_HS_NO3N_RED_INHB))

    LIM_FE_III_RED = (FE_III_DISS  / (FE_III_DISS + SED_K_HS_FE_III_RED_LIM)) * &
        (SED_K_HS_DOXY_RED_INHB  / (SED_DOXY   + SED_K_HS_DOXY_RED_INHB))         * &
        (SED_K_HS_NO3N_RED_INHB  / (SED_NO3N   + SED_K_HS_NO3N_RED_INHB))         * &
        (SED_K_HS_MN_IV_RED_INHB / (MN_IV_DISS + SED_K_HS_MN_IV_RED_INHB))

    LIM_S_PLUS_6_RED = (S_PLUS_6 / (S_PLUS_6 + SED_K_HS_S_PLUS_6_RED_LIM)) * &
        (SED_K_HS_DOXY_RED_INHB   / (SED_DOXY    + SED_K_HS_DOXY_RED_INHB))    * &
        (SED_K_HS_NO3N_RED_INHB   / (SED_NO3N    + SED_K_HS_NO3N_RED_INHB))    * &
        (SED_K_HS_MN_IV_RED_INHB  / (MN_IV_DISS  + SED_K_HS_MN_IV_RED_INHB))   * &
        (SED_K_HS_FE_III_RED_INHB / (FE_III_DISS + SED_K_HS_FE_III_RED_INHB))

    LIM_DOC_RED = 1.0D0 - &
        (LIM_DOXY_RED + LIM_NO3N_RED + LIM_MN_IV_RED + LIM_FE_III_RED + LIM_S_PLUS_6_RED)

    where (LIM_DOC_RED < 0.0D0)
        LIM_DOC_RED = 0.0D0
    end where

    R_MINER_DOC_DOXY = &
        SED_K_MIN_DOC_DOXY_20 * (SED_THETA_K_MIN_DOC_DOXY ** (SED_TEMPS - 2.0D1)) * &
        LIM_DOXY_RED * PH_CORR_DOC_MIN_DOXY * (SED_DOC / (SED_DOC + SED_K_HS_DOC_MIN_DOXY)) * &
        SED_DOC

    R_MINER_DOC_NO3N = &
        SED_K_MIN_DOC_NO3N_20 * (SED_THETA_K_MIN_DOC_NO3N ** (SED_TEMPS - 2.0D1)) * &
        LIM_NO3N_RED * PH_CORR_DOC_MIN_NO3N * (SED_DOC / (SED_DOC + SED_K_HS_DOC_MIN_NO3N)) * &
        SED_DOC

    R_MINER_DOC_MN_IV = &
        SED_K_MIN_DOC_MN_IV_20 * (SED_THETA_K_MIN_DOC_MN_IV ** (SED_TEMPS - 2.0D1)) * &
        LIM_MN_IV_RED * PH_CORR_DOC_MIN_MN_IV * (SED_DOC / (SED_DOC + SED_K_HS_DOC_MIN_MN_IV)) * &
        SED_DOC

    R_MINER_DOC_FE_III = &
        SED_K_MIN_DOC_FE_III_20 * (SED_THETA_K_MIN_DOC_FE_III ** (SED_TEMPS - 2.0D1)) * &
        LIM_FE_III_RED * PH_CORR_DOC_MIN_FE_III * (SED_DOC / (SED_DOC + SED_K_HS_DOC_MIN_FE_III)) * &
        SED_DOC

    R_MINER_DOC_S_PLUS_6 = &
        SED_K_MIN_DOC_S_PLUS_6_20 * (SED_THETA_K_MIN_DOC_S_PLUS_6 ** (SED_TEMPS - 2.0D1)) * &
        LIM_S_PLUS_6_RED * PH_CORR_DOC_MIN_S_PLUS_6 * (SED_DOC / (SED_DOC + SED_K_HS_DOC_MIN_S_PLUS_6)) * &
        SED_DOC

    R_MINER_DOC_DOC = &
        (SED_K_MIN_DOC_DOC_20 * (SED_THETA_K_MIN_DOC_DOC ** (SED_TEMPS - 2.0D1)) * &
         LIM_DOC_RED * PH_CORR_DOC_MIN_DOXY * (SED_DOC / (SED_DOC + SED_K_HS_DOC_MIN_DOC)) * SED_DOC)

end subroutine SED_DOC_MINERALIZATION
