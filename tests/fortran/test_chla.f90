! Test program for CHLA (Chlorophyll-a) calculations
! Tests the chlorophyl_a_vec subroutine
!
! Compile with:
!   gfortran -o test_chla test_chla.f90 \
!     ../../SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_CHLA.f90
!
! Run: ./test_chla

program test_chla
    implicit none
    
    integer :: num_passed, num_failed, num_tests
    
    ! External subroutine declaration
    external :: chlorophyl_a_vec
    
    num_passed = 0
    num_failed = 0
    num_tests = 0
    
    write(*,*) '=============================================='
    write(*,*) 'Chlorophyll-a Calculation Test Suite'
    write(*,*) '=============================================='
    write(*,*)
    
    call test_basic_chla_calculation(num_passed, num_failed, num_tests)
    call test_zero_biomass(num_passed, num_failed, num_tests)
    call test_single_species(num_passed, num_failed, num_tests)
    call test_with_nitrogen_fixers(num_passed, num_failed, num_tests)
    call test_with_nostocales(num_passed, num_failed, num_tests)
    call test_vector_calculation(num_passed, num_failed, num_tests)
    call test_proportionality(num_passed, num_failed, num_tests)
    
    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,*) '=============================================='
    
    if (num_failed > 0) then
        stop 1
    end if
    
contains

    subroutine test_basic_chla_calculation(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        double precision :: FIX_CYN_C(nkn), NOST_VEG_HET_C(nkn)
        double precision :: CHLA(nkn)
        
        ! C:Chla ratios (mg C / mg Chla)
        double precision :: DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA
        double precision :: FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA
        
        integer :: CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES
        
        total = total + 1
        write(*,*) 'Test: Basic CHLA calculation with typical values'
        
        ! Typical phytoplankton biomass (mg C/L)
        DIA_C(1) = 0.5D0      ! Diatoms
        CYN_C(1) = 0.3D0      ! Cyanobacteria
        OPA_C(1) = 0.2D0      ! Other planktonic algae
        FIX_CYN_C(1) = 0.0D0
        NOST_VEG_HET_C(1) = 0.0D0
        
        ! Typical C:Chla ratios
        DIA_C_TO_CHLA = 50.0D0
        CYN_C_TO_CHLA = 60.0D0
        OPA_C_TO_CHLA = 55.0D0
        FIX_CYN_C_TO_CHLA = 60.0D0
        NOST_C_TO_CHLA = 65.0D0
        
        CONSIDER_NON_OBLIGATORY_FIXERS = 0
        CONSIDER_NOSTOCALES = 0
        
        CHLA(1) = 0.0D0
        
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA, &
                              CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES)
        
        ! CHLA should be positive and reasonable (typically 1-100 μg/L)
        if (CHLA(1) > 0.0D0 .and. CHLA(1) < 100.0D0) then
            write(*,*) '  PASSED: CHLA =', CHLA(1), 'μg/L (expected positive value)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: CHLA =', CHLA(1), 'μg/L (unexpected value)'
            failed = failed + 1
        end if
    end subroutine test_basic_chla_calculation
    
    subroutine test_zero_biomass(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        double precision :: FIX_CYN_C(nkn), NOST_VEG_HET_C(nkn)
        double precision :: CHLA(nkn)
        double precision :: DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA
        double precision :: FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA
        integer :: CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES
        
        total = total + 1
        write(*,*) 'Test: Zero biomass should give zero CHLA'
        
        DIA_C(1) = 0.0D0
        CYN_C(1) = 0.0D0
        OPA_C(1) = 0.0D0
        FIX_CYN_C(1) = 0.0D0
        NOST_VEG_HET_C(1) = 0.0D0
        
        DIA_C_TO_CHLA = 50.0D0
        CYN_C_TO_CHLA = 60.0D0
        OPA_C_TO_CHLA = 55.0D0
        FIX_CYN_C_TO_CHLA = 60.0D0
        NOST_C_TO_CHLA = 65.0D0
        
        CONSIDER_NON_OBLIGATORY_FIXERS = 0
        CONSIDER_NOSTOCALES = 0
        
        CHLA(1) = -999.0D0  ! Initialize to non-zero to check it gets set
        
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA, &
                              CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES)
        
        if (abs(CHLA(1)) < 1.0D-10) then
            write(*,*) '  PASSED: CHLA =', CHLA(1), '(essentially zero)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: CHLA =', CHLA(1), '(expected zero)'
            failed = failed + 1
        end if
    end subroutine test_zero_biomass
    
    subroutine test_single_species(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        double precision :: FIX_CYN_C(nkn), NOST_VEG_HET_C(nkn)
        double precision :: CHLA(nkn)
        double precision :: DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA
        double precision :: FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA
        double precision :: expected_chla
        integer :: CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES
        
        total = total + 1
        write(*,*) 'Test: Single species (diatoms only)'
        
        ! Only diatoms present
        DIA_C(1) = 1.0D0  ! 1 mg C/L
        CYN_C(1) = 0.0D0
        OPA_C(1) = 0.0D0
        FIX_CYN_C(1) = 0.0D0
        NOST_VEG_HET_C(1) = 0.0D0
        
        DIA_C_TO_CHLA = 50.0D0  ! 50 mg C per mg Chla
        CYN_C_TO_CHLA = 60.0D0
        OPA_C_TO_CHLA = 55.0D0
        FIX_CYN_C_TO_CHLA = 60.0D0
        NOST_C_TO_CHLA = 65.0D0
        
        CONSIDER_NON_OBLIGATORY_FIXERS = 0
        CONSIDER_NOSTOCALES = 0
        
        ! Expected: (1.0 / 50.0) * 1000 = 20 μg/L
        expected_chla = (DIA_C(1) / DIA_C_TO_CHLA) * 1000.0D0
        
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA, &
                              CONSIDER_NON_OBLIGATORY_FIXERS, CONSIDER_NOSTOCALES)
        
        if (abs(CHLA(1) - expected_chla) < 0.01D0) then
            write(*,*) '  PASSED: CHLA =', CHLA(1), '(expected', expected_chla, ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: CHLA =', CHLA(1), '(expected', expected_chla, ')'
            failed = failed + 1
        end if
    end subroutine test_single_species
    
    subroutine test_with_nitrogen_fixers(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        double precision :: FIX_CYN_C(nkn), NOST_VEG_HET_C(nkn)
        double precision :: CHLA_without(nkn), CHLA_with(nkn)
        double precision :: DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA
        double precision :: FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA
        
        total = total + 1
        write(*,*) 'Test: Including nitrogen fixers increases CHLA'
        
        DIA_C(1) = 0.5D0
        CYN_C(1) = 0.3D0
        OPA_C(1) = 0.2D0
        FIX_CYN_C(1) = 0.4D0  ! Non-zero nitrogen fixers
        NOST_VEG_HET_C(1) = 0.0D0
        
        DIA_C_TO_CHLA = 50.0D0
        CYN_C_TO_CHLA = 60.0D0
        OPA_C_TO_CHLA = 55.0D0
        FIX_CYN_C_TO_CHLA = 60.0D0
        NOST_C_TO_CHLA = 65.0D0
        
        ! First without fixers
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA_without, 0, 0)
        
        ! Then with fixers
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA_with, 1, 0)
        
        if (CHLA_with(1) > CHLA_without(1)) then
            write(*,*) '  PASSED: With fixers (', CHLA_with(1), ') > Without (', CHLA_without(1), ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: With fixers (', CHLA_with(1), ') should be > Without (', CHLA_without(1), ')'
            failed = failed + 1
        end if
    end subroutine test_with_nitrogen_fixers
    
    subroutine test_with_nostocales(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        double precision :: FIX_CYN_C(nkn), NOST_VEG_HET_C(nkn)
        double precision :: CHLA_without(nkn), CHLA_with(nkn)
        double precision :: DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA
        double precision :: FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA
        
        total = total + 1
        write(*,*) 'Test: Including Nostocales increases CHLA'
        
        DIA_C(1) = 0.5D0
        CYN_C(1) = 0.3D0
        OPA_C(1) = 0.2D0
        FIX_CYN_C(1) = 0.0D0
        NOST_VEG_HET_C(1) = 0.3D0  ! Non-zero Nostocales
        
        DIA_C_TO_CHLA = 50.0D0
        CYN_C_TO_CHLA = 60.0D0
        OPA_C_TO_CHLA = 55.0D0
        FIX_CYN_C_TO_CHLA = 60.0D0
        NOST_C_TO_CHLA = 65.0D0
        
        ! First without Nostocales
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA_without, 0, 0)
        
        ! Then with Nostocales
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA_with, 0, 1)
        
        if (CHLA_with(1) > CHLA_without(1)) then
            write(*,*) '  PASSED: With Nostocales (', CHLA_with(1), ') > Without (', CHLA_without(1), ')'
            passed = passed + 1
        else
            write(*,*) '  FAILED: With Nostocales (', CHLA_with(1), ') should be > Without (', CHLA_without(1), ')'
            failed = failed + 1
        end if
    end subroutine test_with_nostocales
    
    subroutine test_vector_calculation(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 5
        
        double precision :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        double precision :: FIX_CYN_C(nkn), NOST_VEG_HET_C(nkn)
        double precision :: CHLA(nkn)
        double precision :: DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA
        double precision :: FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA
        integer :: i
        logical :: all_positive
        
        total = total + 1
        write(*,*) 'Test: Vector calculation with multiple nodes'
        
        ! Set up varying conditions
        do i = 1, nkn
            DIA_C(i) = 0.1D0 * dble(i)
            CYN_C(i) = 0.05D0 * dble(i)
            OPA_C(i) = 0.03D0 * dble(i)
            FIX_CYN_C(i) = 0.0D0
            NOST_VEG_HET_C(i) = 0.0D0
        end do
        
        DIA_C_TO_CHLA = 50.0D0
        CYN_C_TO_CHLA = 60.0D0
        OPA_C_TO_CHLA = 55.0D0
        FIX_CYN_C_TO_CHLA = 60.0D0
        NOST_C_TO_CHLA = 65.0D0
        
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA, 0, 0)
        
        all_positive = .true.
        do i = 1, nkn
            if (CHLA(i) <= 0.0D0) all_positive = .false.
        end do
        
        if (all_positive) then
            write(*,*) '  PASSED: All ', nkn, ' values are positive'
            write(*,*) '    CHLA values:', CHLA
            passed = passed + 1
        else
            write(*,*) '  FAILED: Some values are not positive'
            write(*,*) '    CHLA values:', CHLA
            failed = failed + 1
        end if
    end subroutine test_vector_calculation
    
    subroutine test_proportionality(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        integer, parameter :: nkn = 1
        
        double precision :: DIA_C(nkn), CYN_C(nkn), OPA_C(nkn)
        double precision :: FIX_CYN_C(nkn), NOST_VEG_HET_C(nkn)
        double precision :: CHLA1(nkn), CHLA2(nkn)
        double precision :: DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA
        double precision :: FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA
        double precision :: ratio
        
        total = total + 1
        write(*,*) 'Test: Doubling biomass doubles CHLA'
        
        DIA_C_TO_CHLA = 50.0D0
        CYN_C_TO_CHLA = 60.0D0
        OPA_C_TO_CHLA = 55.0D0
        FIX_CYN_C_TO_CHLA = 60.0D0
        NOST_C_TO_CHLA = 65.0D0
        
        ! First calculation
        DIA_C(1) = 0.5D0
        CYN_C(1) = 0.3D0
        OPA_C(1) = 0.2D0
        FIX_CYN_C(1) = 0.0D0
        NOST_VEG_HET_C(1) = 0.0D0
        
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA1, 0, 0)
        
        ! Double the biomass
        DIA_C(1) = 1.0D0
        CYN_C(1) = 0.6D0
        OPA_C(1) = 0.4D0
        
        call chlorophyl_a_vec(DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, &
                              DIA_C_TO_CHLA, CYN_C_TO_CHLA, OPA_C_TO_CHLA, &
                              FIX_CYN_C_TO_CHLA, NOST_C_TO_CHLA, &
                              nkn, CHLA2, 0, 0)
        
        ratio = CHLA2(1) / CHLA1(1)
        
        if (abs(ratio - 2.0D0) < 0.01D0) then
            write(*,*) '  PASSED: Ratio =', ratio, '(expected 2.0)'
            passed = passed + 1
        else
            write(*,*) '  FAILED: Ratio =', ratio, '(expected 2.0)'
            failed = failed + 1
        end if
    end subroutine test_proportionality

end program test_chla
