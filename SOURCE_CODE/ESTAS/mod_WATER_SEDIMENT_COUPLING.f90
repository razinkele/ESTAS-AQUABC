module WATER_SEDIMENT_COUPLING
    use precision_kinds, only: DBL
    implicit none

    ! Particle settling/deposition + water<->sediment flux coupling state,
    ! moved out of module GLOBAL (Phase 5.1). See
    ! docs/superpowers/specs/2026-07-23-water-sediment-coupling-derived-type-design.md
    type, public :: wsc_state_t
        real(kind = DBL), allocatable, dimension(:, :) :: FLUXES_TO_WATER_COLUMN
        real(kind = DBL), allocatable, dimension(:, :) :: FLUXES_OUTPUT_TO_WATER_COLUMN
        real(kind = DBL), allocatable, dimension(:, :) :: DISSOLVED_FRACTIONS
        real(kind = DBL), allocatable, dimension(:, :) :: FRACTION_OF_DEPOSITION
        real(kind = DBL), allocatable, dimension(:, :) :: SETTLING_RATES
        real(kind = DBL), allocatable, dimension(:, :) :: NOT_DEPOSITED_FLUXES
        real(kind = DBL), allocatable, dimension(:, :) :: FLUXES
        real(kind = DBL), allocatable, dimension(:, :) :: SETTLING_VELOCITIES_OUTPUT
        real(kind = DBL), allocatable, dimension(:, :) :: EFFECTIVE_DISSLOVED_FRACTIONS
        real(kind = DBL), allocatable, dimension(:, :) :: EFFECTIVE_DEPOSITION_FRACTIONS
        real(kind = DBL), allocatable, dimension(:, :) :: DEPOSITION_AREA_RATIOS
    end type wsc_state_t

    type(wsc_state_t), public :: wsc
end module WATER_SEDIMENT_COUPLING
