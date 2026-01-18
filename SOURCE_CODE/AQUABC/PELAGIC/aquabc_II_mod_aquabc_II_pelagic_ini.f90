! Pelagic kinetic model ALUKAS_II
! Version with variables calculated in subroutines
! Version with dissolved inorganic carbon and alkalinity as
! state variables.

!Contains:
! module aquabc_II_wc_ini
!     contains : subroutine calc_frac_avail_DON
!                subroutine calc_frac_avail_DOP
!                subroutine calc_mineral_pars
!                subroutine calc_saturation_pars


!==================================================================
 module aquabc_II_wc_ini
!==================================================================
!
! Initilizes some variables necessary for WC calculations
!
    implicit none
!    double precision, save :: frac_avail_DON
!    double precision, save :: frac_avail_DON_NOST
!    double precision, save :: frac_avail_DOP
!    double precision, save :: KHS_DN_NOST_VEG_HET
!    double precision, save :: FRAC_FIX_N_FOR_GR_VEG_HET
!    double precision, save :: FRAC_NOST_GROWTH
!
!    double precision, save :: K_MIN_PHYT_AMIN_DOC
!    double precision, save :: K_MIN_PHYT_AMIN_DON
!    double precision, save :: K_MIN_PHYT_AMIN_DOP
!
!    ! dissolution saturation
!    double precision, save :: KHS_POC_DISS_SAT
!    double precision, save :: KHS_PON_DISS_SAT
!    double precision, save :: KHS_POP_DISS_SAT
!
!!==================================================================
! contains
!!==================================================================
!    subroutine calc_frac_avail_DON
!        ! Calculates fraction of available DON for cyanobacteria
!        ! It is not used in this version
!        ! Fraction is passed from outside of pelagic model routine
!
!        frac_avail_DON = 0.0D0! 0.25 not used
!     end subroutine calc_frac_avail_DON
!
!     subroutine calc_frac_avail_DOP
!        ! Calculates fraction of available DOP for nostocales
!        ! it is called in pelagic model every step
!
!        !frac_avail_DOP = 0.1D0
!        frac_avail_DOP = 0.0D0
!
!        !PREF_DIP_DOP_NOST = 1.D0
!        !PREF_DIP_DOP_NOST = 0.3
!     end subroutine calc_frac_avail_DOP
!
!     subroutine calc_nost_nitro_pars
!        ! Temporary routine for constants of nitrogen uptake for nostocales
!        ! All these constants should go to constants file fixme
!        ! it is called in pelagic model every step
!
!        frac_avail_DON_NOST       = 0.D0          !0.25D0 Fraction of available DON for nostocales growth
!
!        KHS_DN_NOST_VEG_HET       = 0.0072D0 !half saturation constant for
!                                            !limitation of nost. growth on nitrogen
!        FRAC_FIX_N_FOR_GR_VEG_HET = 0.65D0  ! fraction of fixed ntrogen used for nostacles for growth.
!                                            !1-frac is released to environment
!        FRAC_NOST_GROWTH          = 0.1D0   !fraction of maximum growth rate supported by fixed nitrogen
!                                            !1-frac - supported by N forms in WC
!     end subroutine calc_nost_nitro_pars
!
!     subroutine calc_mineral_pars
!      ! Temporary routines for mineralisation parameters
!
!      !minimum  phytoplankton conc. when mineralisation rate starts to depend on it
!       K_MIN_PHYT_AMIN_DOC = 4.0D0
!       K_MIN_PHYT_AMIN_DON = 4.0D0
!       K_MIN_PHYT_AMIN_DOP = 4.0D0
!
!     end subroutine calc_mineral_pars
!
!     subroutine calc_saturation_pars
!        ! Temporary routine for detritus dissolution saturation
!        ! parameters
!
!        ! Reverse half saturation parameters to slow down dissolution when OM conc is high
!        KHS_POC_DISS_SAT       = 1.25D0
!        KHS_PON_DISS_SAT       = 0.25D0
!        KHS_POP_DISS_SAT       = 0.025D0
!
!     end subroutine calc_saturation_pars
!
!==================================================================
end module aquabc_II_wc_ini
!==================================================================
