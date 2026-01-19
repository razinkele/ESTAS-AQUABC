module ALLELOPATHY

    implicit nonnteger :: DBL_ALLEarameter(DBL_ALLEL = selected_real_kind(15, 307))

    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_NOFIX_CYeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_FIX_CYeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_NOSeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_OPeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_DIA_ZOeal(kind = DBL_ALLEL) :: k_DEG_SEC_METAB_DIA_2eal(kind = DBL_ALLEL) :: THETA_k_DEG_SEC_METAB_DIA

    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_DIeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_FIX_CYeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_NOSeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_OPeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOFIX_CYN_ZOeal(kind = DBL_ALLEL) :: k_DEG_SEC_METAB_NOFIX_CYN_2eal(kind = DBL_ALLEL) :: THETA_k_DEG_SEC_METAB_NOFIX_CYN

    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_DIeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_NOFIX_CYeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_NOSeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_OPeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_FIX_CYN_ZOeal(kind = DBL_ALLEL) :: k_DEG_SEC_METAB_FIX_CYN_2eal(kind = DBL_ALLEL) :: THETA_k_DEG_SEC_METAB_FIX_CYN

    real(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_DIeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_NOFIX_CYeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_FIX_CYeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_OPeal(kind = DBL_ALLEL) :: K_HS_SEC_METAB_NOST_ZOeal(kind = DBL_ALLEL) :: k_DEG_SEC_METAB_NOST_2eal(kind = DBL_ALLEL) :: THETA_k_DEG_SEC_METAB_NOST

    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_DIeal(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_NOFIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_FIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_NOST

    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_DIAeal(kind = DBL_ALLEL) :: S_SEC_METAB_TO_NOFIX_CYNeal(kind = DBL_ALLEL) :: S_SEC_METAB_TO_FIX_CYNeal(kind = DBL_ALLEL) :: S_SEC_METAB_TO_NOSTC

    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_DIeal(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_NOFIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_FIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_NOST

    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_DIeal(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_NOFIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_FIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_NOST

    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_NOFIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_FIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_NOSeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_OPeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_ZOeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_DIeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_FIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_NOSeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_OPeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_ZOeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_DIeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_NOFIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_NOSeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_OPeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_ZOeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_DIeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_NOFIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_FIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_OPeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_ZOO

    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOSeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_OPeal(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_ZOO

    real(kind = DBL_ALLEL), dimension(:,:), allocatable :: DERIVATIVES_SEC_METAB

    real(kind = DBL_ALLEL), dimension(:), allocatable :: WATER_TEMeal(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_DIeal(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_NOFIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_FIX_CYeal(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_NOST

contains

    subroutine ALLOC_ALLEOPATHY(nkn)

        implicit nonnteger, intent(in) :: nkn

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
        deallocate(IHBF_SEC_METAB_DIA_FIX_CYN      )
        deallocate(IHBF_SEC_METAB_DIA_NOST         )
        deallocate(IHBF_SEC_METAB_DIA_OPA          )
        deallocate(IHBF_SEC_METAB_DIA_ZOO          )
        deallocate(IHBF_SEC_METAB_NOFIX_CYN_DIA    )
        deallocate(IHBF_SEC_METAB_NOFIX_CYN_FIX_CYN)
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
