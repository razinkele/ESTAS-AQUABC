! Pure mapping of per-type (sandy/muddy) sediment profiles onto per-box arrays.
!
! Phase-2a core of the two-type sediment reader (mod_BOTTOM_SEDIMENTS): given a
! box->type index map and one profile per type, fill each box's geometry, mixing,
! burial and initial-condition arrays from its type's profile. Kept pure and free
! of GLOBAL state so it is unit-testable in isolation (tests/fortran/test_sed_typemap).
!
! The initial-condition copy reproduces the legacy transpose the single-profile
! reader performs (BSED_ARRAY(var,layer) -> INIT_SED_STATE_VARS(box,layer,var)):
! per-type buffers are var-major/layer-minor, the per-box target is layer-major/
! var-minor, so the axes are swapped explicitly rather than block-copied.
!
! The caller is responsible for validating that every type_per_box(box) lies in
! [1, num_types] before calling (the reader stops with a diagnostic otherwise).
module SED_TYPEMAP
    use precision_kinds, only: DBL
    implicit none
    private
    public :: ASSIGN_SED_PROFILES_TO_BOXES

contains

    pure subroutine ASSIGN_SED_PROFILES_TO_BOXES(nkn, num_layers, num_vars, type_per_box, &
            type_depths, type_porosities, type_densities, type_mixing, type_burial, type_ic, &
            sed_depths, sed_porosities, sed_densities, sed_burrials, part_mixing_coeffs, &
            init_sed_state_vars)
        integer,        intent(in)  :: nkn, num_layers, num_vars
        integer,        intent(in)  :: type_per_box(:)                       ! (nkn)
        real(kind=DBL), intent(in)  :: type_depths(:,:), type_porosities(:,:), type_densities(:,:)  ! (num_types,num_layers)
        real(kind=DBL), intent(in)  :: type_mixing(:), type_burial(:)        ! (num_types) scalar per type
        real(kind=DBL), intent(in)  :: type_ic(:,:,:)                        ! (num_types,num_vars,num_layers)
        real(kind=DBL), intent(out) :: sed_depths(:,:), sed_porosities(:,:), sed_densities(:,:)     ! (nkn,num_layers)
        real(kind=DBL), intent(out) :: sed_burrials(:,:)                     ! (nkn,num_layers)
        real(kind=DBL), intent(out) :: part_mixing_coeffs(:,:,:)             ! (nkn,num_layers,num_vars)
        real(kind=DBL), intent(out) :: init_sed_state_vars(:,:,:)            ! (nkn,num_layers,num_vars)

        integer :: box, layer, var, t

        do box = 1, nkn
            t = type_per_box(box)
            do layer = 1, num_layers
                sed_depths(box, layer)     = type_depths(t, layer)
                sed_porosities(box, layer) = type_porosities(t, layer)
                sed_densities(box, layer)  = type_densities(t, layer)
                sed_burrials(box, layer)   = type_burial(t)                  ! scalar broadcast over layers
                do var = 1, num_vars
                    part_mixing_coeffs(box, layer, var)  = type_mixing(t)    ! scalar broadcast over layers,vars
                    init_sed_state_vars(box, layer, var) = type_ic(t, var, layer)  ! transpose
                end do
            end do
        end do
    end subroutine ASSIGN_SED_PROFILES_TO_BOXES

end module SED_TYPEMAP
