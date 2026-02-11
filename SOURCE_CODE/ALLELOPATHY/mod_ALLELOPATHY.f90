
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
    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_DIA
    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_NOFIX_CYN
    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_FIX_CYN
    real(kind = DBL_ALLEL) :: S_SEC_METAB_TO_NOST

    real(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: ALLEL_R_DEATH_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_DIA_ZOO
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_FIX_CYN_ZOO
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOFIX_CYN_ZOO
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_NOST_ZOO
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_OPA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: IHBF_SEC_METAB_ZOO
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_DEG_SEC_METAB_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: R_FORM_SEC_METAB_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_DIA
    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_FIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_NOFIX_CYN
    real(kind = DBL_ALLEL), dimension(:), allocatable :: SEC_METAB_NOST
    real(kind = DBL_ALLEL), dimension(:), allocatable :: WATER_TEMP
    real(kind = DBL_ALLEL), dimension(:,:), allocatable :: DERIVATIVES_SEC_METAB


contains

    subroutine ALLOC_ALLEOPATHY(nkn)

        implicit none
        integer, intent(in) :: nkn
        integer :: ierr

        allocate(SEC_METAB_DIA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(SEC_METAB_NOFIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(SEC_METAB_FIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(SEC_METAB_NOST(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(R_FORM_SEC_METAB_DIA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(R_FORM_SEC_METAB_NOFIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(R_FORM_SEC_METAB_FIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(R_FORM_SEC_METAB_NOST(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(R_DEG_SEC_METAB_DIA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(R_DEG_SEC_METAB_NOFIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(R_DEG_SEC_METAB_FIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(R_DEG_SEC_METAB_NOST(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_DIA_NOFIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_DIA_FIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_DIA_NOST(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_DIA_OPA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_DIA_ZOO(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOFIX_CYN_DIA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOFIX_CYN_FIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOFIX_CYN_NOST(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOFIX_CYN_OPA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOFIX_CYN_ZOO(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_FIX_CYN_DIA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_FIX_CYN_NOFIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_FIX_CYN_NOST(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_FIX_CYN_OPA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_FIX_CYN_ZOO(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOST_DIA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOST_NOFIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOST_FIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOST_OPA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOST_ZOO(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_DIA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOFIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_FIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_NOST(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_OPA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(IHBF_SEC_METAB_ZOO(nkn), stat=ierr)
        if (ierr /= 0) goto 99

        allocate(DERIVATIVES_SEC_METAB(nkn,4), stat=ierr)
        if (ierr /= 0) goto 99

        allocate(WATER_TEMP(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(ALLEL_R_DEATH_DIA(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(ALLEL_R_DEATH_NOFIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(ALLEL_R_DEATH_FIX_CYN(nkn), stat=ierr)
        if (ierr /= 0) goto 99
        allocate(ALLEL_R_DEATH_NOST(nkn), stat=ierr)
        if (ierr /= 0) goto 99

        return

     99 write(*,*) 'ERROR: ALLOC_ALLEOPATHY: allocation failed, nkn=', nkn
        stop 'ALLOC_ALLEOPATHY: memory allocation failed'
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
        deallocate(WATER_TEMP             )
        deallocate(ALLEL_R_DEATH_DIA      )
        deallocate(ALLEL_R_DEATH_NOFIX_CYN)
        deallocate(ALLEL_R_DEATH_FIX_CYN  )
        deallocate(ALLEL_R_DEATH_NOST     )

    end subroutine DEALLOC_ALLEOPATHY

end module ALLELOPATHY
