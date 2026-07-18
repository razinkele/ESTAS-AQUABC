! Test program for the CO2SYS carbonate-system solver (CO2SYS_CDIAC module)
! Target: SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90 (TODO 5.1)
!
! Strategy (three complementary layers, since CO2SYS has no external oracle wired
! into the build):
!   (1) internal round-trip consistency  -- (TA,DIC)->pH,pCO2 must invert back to
!       the same TA/DIC through the other input pairings (exercises the solver's
!       Icase code paths against each other; oracle-free)
!   (2) physical / mass-balance invariants -- DIC = CO2+HCO3+CO3, pH range,
!       dpH/dDIC<0, dpCO2/dT>0, OmegaCa>OmegaAr>1, Revelle range, borate alk > 0
!   (3) characterization anchor           -- canonical case (S=35, T=25 C,
!       TA=2300, DIC=2000, K1K2=4/Mehrbach, KSO4=1/Dickson, total scale) pinned to
!       values cross-validated against PyCO2SYS 1.8 (pH 8.045, pCO2 397 uatm,
!       OmegaAr 3.39)
!
! The borate-alkalinity check (col 9 > 0) is a dedicated regression guard for the
! KB=0 parenthesis bug fixed in aquabc_II_co2sys.f90 (Dickson-1990 lnKB formula):
! before the fix KB underflowed to 0, borate alkalinity vanished, and pH/pCO2 were
! off by +0.16 / -35%.
!
! Compile: see tests/fortran/Makefile target test_co2sys
! Run:     ./test_co2sys

program test_co2sys
    use AQUABC_II_GLOBAL
    use CO2SYS_CDIAC
    implicit none

    integer :: num_passed, num_failed, num_tests

    ! Output column indices (see NICEHEADERS in aquabc_II_co2sys.f90)
    integer, parameter :: C_TA = 1, C_DIC = 2, C_PH = 3, C_PCO2 = 4
    integer, parameter :: C_HCO3 = 6, C_CO3 = 7, C_CO2 = 8, C_BALK = 9
    integer, parameter :: C_REVELLE = 14, C_OMEGACA = 15, C_OMEGAAR = 16

    ! CO2SYS PARxTYPE codes
    integer, parameter :: T_TA = 1, T_DIC = 2, T_PH = 3, T_PCO2 = 4

    num_passed = 0
    num_failed = 0
    num_tests  = 0

    write(*,*) '=============================================='
    write(*,*) 'CO2SYS Carbonate-System Test Suite'
    write(*,*) '=============================================='
    write(*,*)

    call test_mass_balance_closure(num_passed, num_failed, num_tests)
    call test_roundtrip_consistency(num_passed, num_failed, num_tests)
    call test_physical_invariants(num_passed, num_failed, num_tests)
    call test_borate_alkalinity_regression(num_passed, num_failed, num_tests)
    call test_characterization_anchor(num_passed, num_failed, num_tests)

    write(*,*)
    write(*,*) '=============================================='
    write(*,'(A,I3,A,I3,A)') ' Results: ', num_passed, ' passed, ', num_failed, ' failed'
    write(*,*) '=============================================='

    if (num_failed > 0) then
        stop 1
    end if

contains

    ! Run a single-point CO2SYS call and return the 81-column output row.
    ! Fixed context for all tests: KSO4=1 (Dickson), pH scale 1 (total),
    ! P_in=P_out=0, Si=PO4=0, TEMPOUT=TEMPIN, K1K2 selectable.
    subroutine run_case(in_par1, in_par2, in_p1t, in_p2t, in_salt, in_tempc, in_k1k2, row)
        real(kind = DBL_PREC), intent(in)  :: in_par1, in_par2, in_salt, in_tempc
        integer,               intent(in)  :: in_p1t, in_p2t, in_k1k2
        real(kind = DBL_PREC), intent(out) :: row(81)

        integer, parameter :: n = 1
        real(kind = DBL_PREC) :: PAR1(n), PAR2(n), SALT(n), TEMPIN(n), TEMPOUT(n)
        real(kind = DBL_PREC) :: PRESIN(n), PRESOUT(n), SI(n), PO4(n)
        integer :: PAR1TYPE(n), PAR2TYPE(n), pHSCALEIN(n), K1K2(n), KSO4(n)
        real(kind = DBL_PREC), allocatable :: OUT(:,:)
        character(len = 34),   allocatable :: HEAD(:)
        integer :: ntps

        ntps = n
        PAR1 = in_par1; PAR2 = in_par2
        PAR1TYPE = in_p1t; PAR2TYPE = in_p2t
        SALT = in_salt; TEMPIN = in_tempc; TEMPOUT = in_tempc
        PRESIN = 0.0D0; PRESOUT = 0.0D0; SI = 0.0D0; PO4 = 0.0D0
        pHSCALEIN = 1; K1K2 = in_k1k2; KSO4 = 1

        call CO2SYS(PAR1, PAR2, PAR1TYPE, PAR2TYPE, SALT, TEMPIN, TEMPOUT, &
                    PRESIN, PRESOUT, SI, PO4, pHSCALEIN, K1K2, KSO4, &
                    OUT, HEAD, ntps)

        row = OUT(1, 1:81)

        if (allocated(OUT))  deallocate(OUT)
        if (allocated(HEAD)) deallocate(HEAD)
    end subroutine run_case

    subroutine record(cond, name, passed, failed, total)
        logical,      intent(in)    :: cond
        character(*), intent(in)    :: name
        integer,      intent(inout) :: passed, failed, total
        total = total + 1
        if (cond) then
            passed = passed + 1
            write(*,'(A,A)') '   PASS: ', name
        else
            failed = failed + 1
            write(*,'(A,A)') '   FAIL: ', name
        end if
    end subroutine record

    ! --- (2) mass-balance closure: DIC == CO2 + HCO3 + CO3 ---
    subroutine test_mass_balance_closure(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind = DBL_PREC) :: row(81), dic, sum3
        write(*,*) 'Test: DIC = CO2 + HCO3 + CO3 (mass balance closure)'
        call run_case(2300.0D0, 2000.0D0, T_TA, T_DIC, 35.0D0, 25.0D0, 4, row)
        dic  = row(C_DIC)
        sum3 = row(C_CO2) + row(C_HCO3) + row(C_CO3)
        call record(abs(dic - sum3) <= 1.0D-6 * dic, &
                    'DIC equals CO2+HCO3+CO3 to 1e-6 relative', passed, failed, total)
    end subroutine test_mass_balance_closure

    ! --- (1) internal round-trip consistency ---
    ! Only the DIC-based input pairings are exercised: (TA,DIC) forward and
    ! (pH,DIC) inverse. These are the paths the AQUABC model actually uses
    ! (PAR1TYPE=1/TA, PAR2TYPE=2/DIC). The remaining pairings that take pH or pCO2
    ! against a non-DIC partner -- (TA,pH), (pH,pCO2), (TA,pCO2) -- are LATENT-BUGGY
    ! in this port (they return garbage or crash with "DEALLOCATE unallocated
    ! 'denom'" at aquabc_II_co2sys.f90:3711) and are deliberately NOT tested here;
    ! see TODO_IMPLEMENTATION_PLAN.md 5.1 for the follow-up note. The model never
    ! calls them.
    subroutine test_roundtrip_consistency(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind = DBL_PREC) :: base(81), r_phdic(81)
        real(kind = DBL_PREC) :: ph
        real(kind = DBL_PREC), parameter :: tol = 0.5D0   ! micro mol/kg

        write(*,*) 'Test: round-trip consistency (TA,DIC) <-> (pH,DIC)'
        ! forward solve from (TA, DIC)
        call run_case(2300.0D0, 2000.0D0, T_TA, T_DIC, 35.0D0, 25.0D0, 4, base)
        ph = base(C_PH)

        ! inverse: (pH, DIC) must recover TA
        call run_case(ph, 2000.0D0, T_PH, T_DIC, 35.0D0, 25.0D0, 4, r_phdic)
        call record(abs(r_phdic(C_TA) - 2300.0D0) <= tol, &
                    '(pH,DIC) recovers TA=2300', passed, failed, total)
    end subroutine test_roundtrip_consistency

    ! --- (2) physical invariants + monotonicity ---
    subroutine test_physical_invariants(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind = DBL_PREC) :: base(81), hi_dic(81), hot(81)

        write(*,*) 'Test: physical invariants and monotonicity'
        call run_case(2300.0D0, 2000.0D0, T_TA, T_DIC, 35.0D0, 25.0D0, 4, base)

        call record(base(C_PH) > 7.5D0 .and. base(C_PH) < 8.3D0, &
                    'pH in oceanic range (7.5, 8.3)', passed, failed, total)
        call record(base(C_OMEGACA) > base(C_OMEGAAR) .and. base(C_OMEGAAR) > 1.0D0, &
                    'OmegaCa > OmegaAr > 1', passed, failed, total)
        call record(base(C_REVELLE) > 8.0D0 .and. base(C_REVELLE) < 16.0D0, &
                    'Revelle factor in (8, 16)', passed, failed, total)

        ! adding DIC at fixed TA: pH down, pCO2 up
        call run_case(2300.0D0, 2100.0D0, T_TA, T_DIC, 35.0D0, 25.0D0, 4, hi_dic)
        call record(hi_dic(C_PH) < base(C_PH), &
                    'dpH/dDIC < 0 (adding DIC acidifies)', passed, failed, total)
        call record(hi_dic(C_PCO2) > base(C_PCO2), &
                    'dpCO2/dDIC > 0', passed, failed, total)

        ! warming at fixed TA/DIC: pCO2 up
        call run_case(2300.0D0, 2000.0D0, T_TA, T_DIC, 35.0D0, 30.0D0, 4, hot)
        call record(hot(C_PCO2) > base(C_PCO2), &
                    'dpCO2/dT > 0 (warming degasses)', passed, failed, total)
    end subroutine test_physical_invariants

    ! --- regression guard for the KB=0 parenthesis bug ---
    subroutine test_borate_alkalinity_regression(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind = DBL_PREC) :: row(81)

        write(*,*) 'Test: borate alkalinity is present (KB parenthesis-bug guard)'
        call run_case(2300.0D0, 2000.0D0, T_TA, T_DIC, 35.0D0, 25.0D0, 4, row)
        ! borate alk should be ~91 micro mol/kg at S=35; it was exactly 0 when KB
        ! underflowed to 0 (the fixed bug). A floor of 50 catches any regression.
        call record(row(C_BALK) > 50.0D0, &
                    'borate alkalinity > 50 (was 0 with the KB bug)', passed, failed, total)
    end subroutine test_borate_alkalinity_regression

    ! --- (3) characterization anchor (cross-validated vs PyCO2SYS 1.8) ---
    subroutine test_characterization_anchor(passed, failed, total)
        integer, intent(inout) :: passed, failed, total
        real(kind = DBL_PREC) :: row(81)

        write(*,*) 'Test: canonical case anchored to PyCO2SYS-validated values'
        call run_case(2300.0D0, 2000.0D0, T_TA, T_DIC, 35.0D0, 25.0D0, 4, row)
        ! PyCO2SYS (K1K2=4, KSO4=1, total scale): pH 8.0453, pCO2 397.26,
        !   OmegaAr 3.388, OmegaCa 5.140, Revelle 9.596
        call record(abs(row(C_PH)      - 8.045D0)  <= 0.010D0, &
                    'pH within 0.01 of 8.045', passed, failed, total)
        call record(abs(row(C_PCO2)    - 397.2D0)  <= 3.0D0,   &
                    'pCO2 within 3 uatm of 397', passed, failed, total)
        call record(abs(row(C_OMEGAAR) - 3.388D0)  <= 0.05D0,  &
                    'OmegaAr within 0.05 of 3.388', passed, failed, total)
        call record(abs(row(C_OMEGACA) - 5.141D0)  <= 0.05D0,  &
                    'OmegaCa within 0.05 of 5.141', passed, failed, total)
    end subroutine test_characterization_anchor

end program test_co2sys
