
! Content:
!subroutine SED_POC_DISSOLUTION

subroutine SED_POC_DISSOLUTION &
           (K_OXIC_DISS_POC  , &
            K_ANOXIC_DISS_POC, &
            THETA_DISS_POC   , &
            KHS_DISS_POC     , &
            DOXY_AT_ANOXIA   , &
            SED_POC          , &
            SED_DOXY         , &
            SED_TEMPS        , &
            nkn              , &
            NUM_SED_LAYERS   , &
            R_DISS_POC)

    use AQUABC_II_GLOBAL
    implicit none

    real(kind = DBL_PREC), intent(in) :: K_OXIC_DISS_POC
    real(kind = DBL_PREC), intent(in) :: K_ANOXIC_DISS_POC
    real(kind = DBL_PREC), intent(in) :: THETA_DISS_POC
    real(kind = DBL_PREC), intent(in) :: KHS_DISS_POC
    real(kind = DBL_PREC), intent(in) :: DOXY_AT_ANOXIA

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: SED_POC
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: SED_DOXY
    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(in) :: SED_TEMPS

    integer :: nkn
    integer :: NUM_SED_LAYERS

    real(kind = DBL_PREC), dimension(nkn, NUM_SED_LAYERS), intent(inout) :: R_DISS_POC

    where (SED_DOXY.GE.DOXY_AT_ANOXIA)

        R_DISS_POC  = &
            K_OXIC_DISS_POC    * (THETA_DISS_POC  ** (SED_TEMPS - 2.0D1)) * &
            (SED_POC / (SED_POC + KHS_DISS_POC)) * SED_POC

    end where

    where (SED_DOXY.LT.DOXY_AT_ANOXIA)

        R_DISS_POC  = &
            K_ANOXIC_DISS_POC  * (THETA_DISS_POC  ** (SED_TEMPS - 2.0D1)) * &
            (SED_POC / (SED_POC + KHS_DISS_POC))  * SED_POC
    end where

end subroutine SED_POC_DISSOLUTION
