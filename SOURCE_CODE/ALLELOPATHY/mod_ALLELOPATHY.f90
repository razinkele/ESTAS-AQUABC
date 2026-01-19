module ALLELOPATHY

    implicit none
    integer :: DBL_ALLEL
    parameter(DBL_ALLEL = selected_real_kind(15, 307))

    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_NOFIX_CYN
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_FIX_CYN
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_NOST
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_OPA
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_ZOO
    real(kind = DBL_ALLEL) :: k_DEG_SEC_METAB_DIA_20
    real(kind = DBL_ALLEL) :: THETA_k_DEG_SEC_METAB_DIA

    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_DIA
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_FIX_CYN
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_NOST
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_OPA
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_ZOO
    real(kind = DBL_ALLEL) :: k_DEG_SEC_METAB_NOFIX_CYN_20
    real(kind = DBL_ALLEL) :: THETA_k_DEG_SEC_METAB_NOFIX_CYN

    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_DIA
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_NOFIX_CYN
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_NOST
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_OPA
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_ZOO
    real(kind = DBL_ALLEL) :: k_DEG_SEC_METAB_FIX_CYN_20
    real(kind = DBL_ALLEL) :: THETA_k_DEG_SEC_METAB_FIX_CYN

    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_DIA
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_NOFIX_CYN
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_FIX_CYN
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_OPA
    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_ZOO
    real(kind = DBL_ALLEL) :: k_DEG_SEC_METAB_NOST_20
    real(kind = DBL_ALLEL) :: THETA_k_DEG_SEC_METAB_NOST

    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_NOST

    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_DIA
    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_NOFIX_CYN
    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_FIX_CYN
    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_NOST

    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_NOST

    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_NOST

    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_ZOO

    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_ZOO

    ! Additional allocatable arrays used by library routines (naming variants)
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_ZOO

    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_ZOO

    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_ZOO

    real(kind = DBL_ALLEL), dimension(:,:), allocatable :: DERIVATIVES_SEC_METAB

    real(kind = DBL_ALLEL), dimension(:), allocatable :: WATER_TEMP
    real(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_NOST

contains

    subroutine ALLOC_ALLEOPATHY(nkn)

        implicit none
        integer, intent(in) :: nkn

        allocate(SEC_METAB_DIA                   (nkn))
        allocate(SEC_METAB_NOFIX_CYN             (nkn))
        allocate(SEC_METAB_FIX_CYN               (nkn))
        allocate(SEC_METAB_NOST                  (nkn))
        allocate(R_FORM_SEC_METAB_DIA            (nkn))
        allocate(R_FORM_SEC_METAB_NOFIX_CYN      (nkn))
        allocate(R_FORM_SEC_METAB_FIX_CYN        (nkn))
        allocate(R_FORM_SEC_METAB_NOST           (nkn))
        allocate(R_DEG_SEC_METAB_DIA             (nkn))
        allocate(R_DEG_SEC_METAB_NOFIX_CYN       (nkn))
        allocate(R_DEG_SEC_METAB_FIX_CYN         (nkn))
        allocate(R_DEG_SEC_METAB_NOST            (nkn))
        allocate(IHBF_SEC_METAB_DIA_NOFIX_CYN    (nkn))
        allocate(IHBF_SEC_METAB_DIA_FIX_CYN      (nkn))
        allocate(IHBF_SEC_METAB_DIA_NOST         (nkn))
        allocate(IHBF_SEC_METAB_DIA_OPA          (nkn))
        allocate(IHBF_SEC_METAB_DIA_ZOO          (nkn))
        allocate(IHBF_SEC_METAB_NOFIX_CYN_DIA    (nkn))
        allocate(IHBF_SEC_METAB_NOFIX_CYN_FIX_CYN(nkn))
        allocate(IHBF_SEC_METAB_NOFIX_CYN_NOST   (nkn))
        allocate(IHBF_SEC_METAB_NOFIX_CYN_OPA    (nkn))
        allocate(IHBF_SEC_METAB_NOFIX_CYN_ZOO    (nkn))
        allocate(IHBF_SEC_METAB_FIX_CYN_DIA      (nkn))
        allocate(IHBF_SEC_METAB_FIX_CYN_NOFIX_CYN(nkn))
        allocate(IHBF_SEC_METAB_FIX_CYN_NOST     (nkn))
        allocate(IHBF_SEC_METAB_FIX_CYN_OPA      (nkn))
        allocate(IHBF_SEC_METAB_FIX_CYN_ZOO      (nkn))
        allocate(IHBF_SEC_METAB_NOST_DIA         (nkn))
        allocate(IHBF_SEC_METAB_NOST_NOFIX_CYN   (nkn))
        allocate(IHBF_SEC_METAB_NOST_FIX_CYN     (nkn))
        allocate(IHBF_SEC_METAB_NOST_OPA         (nkn))
        allocate(IHBF_SEC_METAB_NOST_ZOO         (nkn))
        allocate(IHBF_SEC_METAB_DIA              (nkn))
        allocate(IHBF_SEC_METAB_NOFIX_CYN        (nkn))
        allocate(IHBF_SEC_METAB_FIX_CYN          (nkn))
        allocate(IHBF_SEC_METAB_NOST             (nkn))
        allocate(IHBF_SEC_METAB_OPA              (nkn))
        allocate(IHBF_SEC_METAB_ZOO              (nkn))

        allocate(DERIVATIVES_SEC_METAB(nkn, 4))

        allocate(WATER_TEMP             (nkn))
        allocate(ALLEL_R_DEATH_DIA      (nkn))
        allocate(ALLEL_R_DEATH_NOFIX_CYN(nkn))
        allocate(ALLEL_R_DEATH_FIX_CYN  (nkn))
        allocate(ALLEL_R_DEATH_NOST     (nkn))
    end subroutine ALLOC_ALLEOPATHY


    subroutine DEALLOC_ALLEOPATHY()
        implicit none
        deallocate(SEC_METAB_DIA                   )
        deallocate(SEC_METAB_NOFIX_CYN             )
        deallocate(SEC_METAB_FIX_CYN               )
        deallocate(SEC_METAB_NOST                  )
        deallocate(R_FORM_SEC_METAB_DIA            )
        deallocate(R_FORM_SEC_METAB_NOFIX_CYN      )
        deallocate(R_FORM_SEC_METAB_FIX_CYN        )
        deallocate(R_FORM_SEC_METAB_NOST           )
        deallocate(R_DEG_SEC_METAB_DIA             )
        deallocate(R_DEG_SEC_METAB_NOFIX_CYN       )
        deallocate(R_DEG_SEC_METAB_FIX_CYN         )
        deallocate(R_DEG_SEC_METAB_NOST            )
        deallocate(IHBF_SEC_METAB_DIA_NOFIX_CYN    )
        deallocate(IHBF_SEC_METAB_NOFIX_CYN_NOST   )
        deallocate(IHBF_SEC_METAB_NOFIX_CYN_OPA    )
        deallocate(IHBF_SEC_METAB_NOFIX_CYN_ZOO    )
        deallocate(IHBF_SEC_METAB_FIX_CYN_DIA      )
        deallocate(IHBF_SEC_METAB_FIX_CYN_NOFIX_CYN)
        deallocate(IHBF_SEC_METAB_FIX_CYN_NOST     )
        deallocate(IHBF_SEC_METAB_FIX_CYN_OPA      )
        deallocate(IHBF_SEC_METAB_FIX_CYN_ZOO      )
        deallocate(IHBF_SEC_METAB_NOST_DIA         )
        deallocate(IHBF_SEC_METAB_NOST_NOFIX_CYN   )
        deallocate(IHBF_SEC_METAB_NOST_FIX_CYN     )
        deallocate(IHBF_SEC_METAB_NOST_OPA         )
        deallocate(IHBF_SEC_METAB_NOST_ZOO         )
        deallocate(IHBF_SEC_METAB_DIA              )
        deallocate(IHBF_SEC_METAB_NOFIX_CYN        )
        deallocate(IHBF_SEC_METAB_FIX_CYN          )
        deallocate(IHBF_SEC_METAB_NOST             )
        deallocate(IHBF_SEC_METAB_OPA              )
        deallocate(IHBF_SEC_METAB_ZOO              )
        deallocate(DERIVATIVES_SEC_METAB           )

        deallocate(DERIVATIVES_SEC_METAB)

        deallocate(WATER_TEMP             )
        deallocate(ALLEL_R_DEATH_DIA      )
        deallocate(ALLEL_R_DEATH_NOFIX_CYN)
        deallocate(ALLEL_R_DEATH_FIX_CYN  )
        deallocate(ALLEL_R_DEATH_NOST     )

    end subroutine DEALLOC_ALLEOPATHY

end module ALLELOPATHY
