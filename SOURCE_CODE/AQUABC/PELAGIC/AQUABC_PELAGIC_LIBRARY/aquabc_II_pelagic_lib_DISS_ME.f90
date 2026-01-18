! Auxilary routines for the pelagic model

! Contents:
!subroutine CALC_DISS_ME_CONC


! ---------------------------------------------------------------------------------------
! Subroutine to calculate the dissolved metal concentration at the end of the timestep
! and averaged over the timestep using some simplified dissolution kinetics, where
! dissolution is directly propotional to the concentration gradient from equlibrium
! solubility. If more metal is dissolved than the equibirium solubility, then
! the equation will work in a reverse way.
! ---------------------------------------------------------------------------------------
!                  Developer        : Ali Erturk
!                  Development date : 9 August 2016
! ---------------------------------------------------------------------------------------
!
! INPUT VARIABLES
!     TOT_ME              : Total concentration of the metal (FE_II, FE_III, CA, MG, etc.)
!     ME_DISS_INIT        : Initial dissolved metal concentration
!     ME_SOLUB_EQ         : Estimated solubility
!     k_DISS_ME           : Dissolution precipitation rate constant
!     t                   : Time (to be taken as time step)
!     nkn                 : Number of nodes
!     nlayers             : Number of layers
!
! OUTPUT VARIABLES
!     DISS_ME_CONC_TS_END : Dissolved metal concentration at the end of timestep
!     DISS_ME_CONC_TS_AVG : Dissolved metal concentration averaged over the timestep
! ---------------------------------------------------------------------------------------
subroutine CALC_DISS_ME_CONC &
           (TOT_ME             , &! Pass from the state variable representing the total metal concentration (FE_II, FE_III, CA, etc.)
            ME_DISS_INIT       , &! Pass from the last time step (SAVED_OUTPUTS)
            ME_SOLUB_EQ        , &! Pass from equlibrum chemistry calculations (for example FE_II_DISS_EQ)
            k_DISS_ME          , &! Rate constant
            t                  , &! This is the time step
            nkn                , &
            nlayers            , &
            DISS_ME_CONC_TS_END, &
            DISS_ME_CONC_TS_AVG)

    use AQUABC_II_GLOBAL
    implicit none

    ! In going variables
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: TOT_ME
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: ME_DISS_INIT
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: ME_SOLUB_EQ

    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(in) :: k_DISS_ME
    real(kind = DBL_PREC), intent(in) :: t

    integer, intent(in) :: nkn
    integer, intent(in) :: nlayers

    ! Out going variables
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(inout) :: DISS_ME_CONC_TS_END
    real(kind = DBL_PREC), dimension(nkn, nlayers), intent(inout) :: DISS_ME_CONC_TS_AVG

    ! Auxillary variables
    !real(kind = DBL_PREC), dimension(nkn, nlayers) :: A_2
    !real(kind = DBL_PREC), dimension(nkn, nlayers) :: A_3
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: C
    !real(kind = DBL_PREC), dimension(nkn, nlayers) :: Y_0
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: INT_ME_DISS_t
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: INT_ME_DISS_zero
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: LOG_INT_ME_DISS_t
    real(kind = DBL_PREC), dimension(nkn, nlayers) :: LOG_INT_ME_DISS_zero


    ! Updated by Ali and Petras, 15 th of August 2016.

    !Y_0 = ME_DISS_INIT

    where (ME_DISS_INIT > ME_SOLUB_EQ)
        ! This is the oversaturated case, so dissolution reaction will move oppsite way to
        ! chemical precipitation. In this case the rate is dependent on dissolved metal as
        ! well and we end up with nonlinear equation of second degree due to dissolved metal
        ! (Bernoulli type ODE) and is still analytically solveable.

        ! Function              : ME_DISS(t) = -k_DISS_ME * (ME_DISS - ME_SOLUB_EQ) * (ME_DISS - ME_SOLUB_EQ)

        !In future more advanced model considering cyristal or particle growth will be developed.

        DISS_ME_CONC_TS_END = ME_SOLUB_EQ + (1.0D0 / (k_DISS_ME + (1.0D0/(ME_DISS_INIT - ME_SOLUB_EQ))))

        LOG_INT_ME_DISS_t    = log(dabs((1.0D0 / (ME_DISS_INIT - ME_SOLUB_EQ)) + (k_DISS_ME * t)))
        LOG_INT_ME_DISS_zero = log(dabs((1.0D0 / (ME_DISS_INIT - ME_SOLUB_EQ))                  ))

        INT_ME_DISS_t    =  (ME_SOLUB_EQ * t) + ((1.0D0 / (k_DISS_ME)) * LOG_INT_ME_DISS_t)
        INT_ME_DISS_zero =  (1.0D0 / (k_DISS_ME)) * LOG_INT_ME_DISS_zero

        DISS_ME_CONC_TS_AVG = (1.0D0/t) * (INT_ME_DISS_t - INT_ME_DISS_zero)

        ! Old code
        !A_2 = k_DISS_ME * ME_SOLUB_EQ
        !A_3 = k_DISS_ME
        !C   = (log(Y_0) / A_2) - (log(A_2 - (A_3 * Y_0)) / A_2)

        ! Calculate the dissolved metal at the end of timestep
        !DISS_ME_CONC_TS_END = (A_2 * exp(A_2 * (t + C))) / ((A_3 * exp(A_2 * (t + C))) + 1.0D0)

        ! Calculate the definite integral for the solution and divide it by time to
        ! get dissolved metal averaged over timestep
        !DISS_ME_CONC_TS_AVG  = (1.0D0/t) * &
        !    (((1.0D0 * A_3)*(log(dabs((1.0D0/A_3)*((A_3*exp(C*A_2)*exp(t*A_2))+1.0D0)))-(C * A_2))) - &
        !    ((1.0D0 * A_3)*(log(dabs((1.0D0/A_3)*((A_3*exp(C*A_2))+1.0D0)))-(C * A_2))))

    else where (ME_DISS_INIT < ME_SOLUB_EQ)
        ! This is the undersaturated case where more metals will be dissolved.
        ! In this case the rate is dependent on particulate metal as
        ! well and we end up with nonlinear equation of second degree due to dissolved metal
        ! (Ricatti type ODE) and is still analytically solveable.

        ! Function              : ME_DISS(t)
        ! Differential equation : diff(ME_DISS) == k_DISS_ME * (ME_SOLUB_EQ - ME_DISS) * (TOT_ME - ME_DISS - ME_SOLUB_EQ)
        ! Initial condition     : ME_DISS(0) == ME_DISS_INIT)

        ! In future more advanced model considering dissolution related to particle
        ! size and more environmental colditions will be considered.

        ! Calculate the dissolved metal at the end of timestep
        DISS_ME_CONC_TS_END = ME_SOLUB_EQ + &
            ((3.0D0*ME_SOLUB_EQ)/ &
             (1.0D0 -(((ME_DISS_INIT-(4.0D0*ME_SOLUB_EQ))/(ME_DISS_INIT-ME_SOLUB_EQ))* &
                      exp(3.0D0*t*k_DISS_ME*ME_SOLUB_EQ))))

        ! Calculate the definite integral for the solution and divide it by time to
        ! get dissolved metal averaged over timestep

        ! 19th of August 2016 updates
        C = (ME_DISS_INIT - (4.0D0*ME_SOLUB_EQ)) / (ME_DISS_INIT - ME_SOLUB_EQ)

        LOG_INT_ME_DISS_t    = log(dabs(C*exp(3.0D0*k_DISS_ME*ME_SOLUB_EQ*t) - 1.0D0))
        LOG_INT_ME_DISS_zero = log(dabs(C - 1.0D0))

        INT_ME_DISS_t    = &
            (ME_SOLUB_EQ * t) + &
            (3.0D0*ME_SOLUB_EQ*(t - (LOG_INT_ME_DISS_t/(3.0D0*k_DISS_ME*ME_SOLUB_EQ))))

        INT_ME_DISS_zero = &
            (3.0D0*ME_SOLUB_EQ*(-(LOG_INT_ME_DISS_zero/(3.0D0*k_DISS_ME*ME_SOLUB_EQ))))

        ! Old code
        !LOG_INT_ME_DISS_t    = (1.0D0/((4.0D0*ME_SOLUB_EQ) - ME_DISS_INIT)) * &
        !    (ME_DISS_INIT - ME_SOLUB_EQ + (((4.0D0*ME_SOLUB_EQ) - ME_DISS_INIT)*exp(3.0D0*k_DISS_ME*ME_SOLUB_EQ*t)))
        !
        !LOG_INT_ME_DISS_zero = (1.0D0/((4.0D0*ME_SOLUB_EQ)-ME_DISS_INIT))* &
        !    (ME_DISS_INIT - ME_SOLUB_EQ+((4.0D0*ME_SOLUB_EQ)-ME_DISS_INIT))
        !
        !INT_ME_DISS_t    = (-1.0D0/k_DISS_ME)*(log(dabs(LOG_INT_ME_DISS_t)) - (4.0D0*t*ME_SOLUB_EQ*k_DISS_ME))
        !INT_ME_DISS_zero = (-1.0D0/k_DISS_ME)*log(dabs(LOG_INT_ME_DISS_zero))

        DISS_ME_CONC_TS_AVG = (1.0D0/t) * (INT_ME_DISS_t - INT_ME_DISS_zero)
    else where
        DISS_ME_CONC_TS_END = ME_DISS_INIT
        DISS_ME_CONC_TS_AVG = ME_DISS_INIT
    end where

    ! Check for saturation case where logarithms in both over and undersaturation solutions may give Nans
    ! and assume that neither dissolution nor precipitation occurs.
    ! where (isnan(DISS_ME_CONC_TS_END).or.isnan(DISS_ME_CONC_TS_AVG))
    !     DISS_ME_CONC_TS_END = ME_DISS_INIT
    !     DISS_ME_CONC_TS_AVG = ME_DISS_INIT
    ! end where
    DISS_ME_CONC_TS_AVG = 0.5 * (ME_DISS_INIT + DISS_ME_CONC_TS_END)
end subroutine CALC_DISS_ME_CONC
