    
    
module MacroAlgae
    use AQUABC_II_GLOBAL
    implicit none

     ! -------------------    ---------------------------   --------------------------------------------------------------------------------------------------------------  ----------
     ! TYPE                   CODED NAME                    DESCRIPTION                                                                                                     UNIT
     ! -------------------    ---------------------------   --------------------------------------------------------------------------------------------------------------  ----------
     real(kind = DBL_PREC) :: k_G_20_MAC_ALGAE              ! Growth rate constant of macro algae at 20 degrees                                                             (1/day)
     real(kind = DBL_PREC) :: THETA_k_G_MAC_ALGAE           ! Arrhenius correction factor for the growth rate constant of macro algae                                       (Unitless)
     real(kind = DBL_PREC) :: k_R_20_MAC_ALGAE              ! Respiration rate constant of macro algae at 20 degrees                                                        (1/day)
     real(kind = DBL_PREC) :: THETA_k_R_MAC_ALGAE           ! Arrhenius correction factor for the respiration rate constant of macro algae                                  (Unitless)
     real(kind = DBL_PREC) :: k_E_20_MAC_ALGAE              ! Excretion rate constant of macro algae at 20 degrees                                                          (1/day)
     real(kind = DBL_PREC) :: THETA_k_E_MAC_ALGAE           ! Arrhenius correction factor for the excretion rate constant of macro algae                                    (Unitless)
     real(kind = DBL_PREC) :: k_D_20_MAC_ALGAE              ! Death rate constant of macro algae at 20 degrees                                                              (1/day)
     real(kind = DBL_PREC) :: THETA_k_D_MAC_ALGAE           ! Arrhenius correction factor for the death rate constant of macro algae                                        (Unitless)
     real(kind = DBL_PREC) :: Q_ZERO_N_MAC_ALGAE            ! Minimum nitrogen cell quata for macro algae                                                                   (gN/gC)
     real(kind = DBL_PREC) :: Q_ZERO_P_MAC_ALGAE            ! Minimum phosphorus cell quata for macro algae                                                                 (gP/gC)
     real(kind = DBL_PREC) :: SIGMA_m_N_MAC_ALGAE           ! Uptake rate of external dissolved inorganic nitrogen                                                          (1/day)
     real(kind = DBL_PREC) :: SIGMA_m_P_MAC_ALGAE           ! Uptake rate of external solubre reactive phosphorus                                                           (1/day)
     real(kind = DBL_PREC) :: K_HS_DIN_MAC_ALGAE            ! Monod half saturation concentration for macro algae DIN uptake from water column                              (gN/m^3)
     real(kind = DBL_PREC) :: K_HS_SRP_MAC_ALGAE            ! Monod half saturation concentration for macro algae SRP uptake from water column                              (gP/m^3)
     real(kind = DBL_PREC) :: KHS_NH4PREF_MAC_ALGAE         ! Monod half saturation concentration for macro algae NH4N prereference from water column                       (gN/m^3)
     real(kind = DBL_PREC) :: A_O2_TO_C_MAC_ALGAE           ! Stochiometric ratio of dissolved oxygen to macro algae biomass                                                (gO2/gC)
     real(kind = DBL_PREC) :: K_LB_MAC_ALGAE                ! Relevant light limitation parameter for the macro algae light limitation submodel                             (Unitless)
     real(kind = DBL_PREC) :: MAC_ALGAE_C_MAX               ! Carrying capacity for macro algae                                                                              (gC/m^2)
     integer               :: LIGHT_LIM_SUBMODEL_NO         ! Light limitation submodel no                                                                                  (Unitless)
     real(kind = DBL_PREC) :: k_DISS_MALG_ATT_DETPOC        ! Attached dead organic carbon dissolution rate constant at 20 degrees                                          (1/day)
     real(kind = DBL_PREC) :: THETA_k_DISS_MALG_ATT_DETPOC  ! Arrhenius correction factor for the attached dead organic carbon dissolution rate constant                    (Unitless)
     real(kind = DBL_PREC) :: k_DISS_MALG_ATT_DETPON        ! Attached dead organic nitrogen dissolution rate constant at 20 degrees                                        (1/day)
     real(kind = DBL_PREC) :: THETA_k_DISS_MALG_ATT_DETPON  ! Arrhenius correction factor for the attached dead organic nitrogen dissolution rate constant at 20 degrees    (Unitless)
     real(kind = DBL_PREC) :: k_DISS_MALG_ATT_DETPOP        ! Attached dead organic phosphorus dissolution rate constant at 20 degrees                                      (1/day)
     real(kind = DBL_PREC) :: THETA_k_DISS_MALG_ATT_DETPOP  ! Arrhenius correction factor for the attached dead organic phosphorus dissolution rate constant at 20 degrees  (Unitless)
     ! -------------------    ---------------------------   --------------------------------------------------------------------------------------------------------------   ----------
    contains

    ! ------------------------------------------------------------------------------------------------------------------
    ! SUBROUTINE FOR THE MACROALGE KINETICS
    ! ------------------------------------------------------------------------------------------------------------------
    ! The submodel is simple and contains 6 state variables listed below
    !
    !     - Macro algae carbon
    !     - Macro algae nitrogen
    !     - Macro algae phosphorus
    !     - Attached dead organic carbon     (Carbon     equivalent of dead macro algae still attached  to the bottom)
    !     - Attached dead organic nitrogen   (Nitrogen   equivalent of dead macro algae still attached  to the bottom)
    !     - Attached dead organic phosphorus (Phosphorus equivalent of dead macro algae still attached  to the bottom)
    ! ------------------------------------------------------------------------------------------------------------------
    !
    ! Developer        : Ali Ert�rk
    ! Development date : October 2019, during the visit to ISMAR, CNR, Venice 
    !
    ! Further enhancements to be included
    ! 
    !     - Effect of hypoxic condions on macroalgae
    !
    ! -------------------------------------------------------------------------------------------------------------------

                                                     ! -------------    ------------------------------------------------------------------------------------------------------------------ ------------
                                                     ! INPUT/OUTPUT     DESCRIPTION                                                                                                        UNIT
    subroutine MACRO_ALGAE_KINETICS &                ! -------------    ------------------------------------------------------------------------------------------------------------------ ------------
        (nkn                                     , & ! SUBMODEL_INPUT : Number of array element (number of boxes) to enable the submodel to be called in a vectorized way                  (Unitless)
         I_A                                     , & ! SUBMODEL_INPUT : Surface light                                                                                                      (langleys)
         K_E                                     , & ! SUBMODEL_INPUT : Light extinction coefficient                                                                                       (1/m)
         TEMP                                    , & ! SUBMODEL_INPUT : Temperature                                                                                                        (Degrees C)
         H                                       , & ! SUBMODEL_INPUT : Water depth                                                                                                        (m)
         MAC_ALGAE_C                             , & ! SUBMODEL_INPUT : Macro algae carbon                                                                                                 (gC/m^2)
         MAC_ALGAE_N                             , & ! SUBMODEL_INPUT : Macro algae carbon                                                                                                 (gN/m^2)
         MAC_ALGAE_P                             , & ! SUBMODEL_INPUT : Macro algae carbon                                                                                                 (gP/m^2)
         MAC_ALGAE_ATT_DETPOC                    , & ! SUBMODEL_INPUT : Attached dead particulate organic carbon                                                                           (gC/m^2)
         MAC_ALGAE_ATT_DETPON                    , & ! SUBMODEL_INPUT : Attached dead particulate organic nitrogen                                                                         (gN/m^2)
         MAC_ALGAE_ATT_DETPOP                    , & ! SUBMODEL_INPUT : Attached dead particulate organic phosphorus                                                                       (gP/m^2)
         WC_NH4N                                 , & ! SUBMODEL_INPUT : Ammonia nitrogen in the water column                                                                               (gN/m^3)
         WC_NO3N                                 , & ! SUBMODEL_INPUT : Nitrate nitrogen in the water column                                                                               (gN/m^3)
         WC_SRP                                  , & ! SUBMODEL_INPUT : Soluble reactive phosphorus in water column                                                                        (gP/m^3)
         WC_DOC                                  , & ! SUBMODEL_INPUT : Dissolved orgranic carbon in water column                                                                          (gC/m^3)
         WC_DO2                                  , & ! SUBMODEL_INPUT : Dissolved oxygen in water column                                                                                   (gO2/m^3)
         k_DEATT_MAC_ALGAGE                      , & ! SUBMODEL_INPUT : Deattachment rate constant for macro algae                                                                         (1/day)
         k_DEATT_MALG_ATT_DET                    , & ! SUBMODEL_INPUT : Deattachment rate constant for macro algae based attached detritus                                                 (1/day)
         d_MALG_C_over_dt                        , & ! SUBMODEL_OUTPUT: Total change of macro algae carbon over time                                                                       (gC/m^2/day)
         d_MALG_N_over_dt                        , & ! SUBMODEL_OUTPUT: Total change of macro algae nitrogen  over time                                                                    (gN/m^2/day)
         d_MALG_P_over_dt                        , & ! SUBMODEL_OUTPUT: Macro algae phosphorus over time                                                                                   (gP/m^2/day)
         d_MALG_ATT_DETPOC_over_dt               , & ! SUBMODEL_OUTPUT: Total change of macro algeae based attached dead particulate organic carbon over time                              (gC/m^2/day)
         d_MALG_ATT_DETPON_over_dt               , & ! SUBMODEL_OUTPUT: Total change of macro algeae based attached dead particulate organic nitrogen over time                            (gN/m^2/day)
         d_MALG_ATT_DETPOP_over_dt               , & ! SUBMODEL_OUTPUT: Total change of macro algeae based attached dead particulate organic phosphorus over time                          (gP/m^2/day)
         DIC_UPTAKE_FROM_WC_MALG_GROWTH          , & ! SUBMODEL_OUTPUT: Uptake of dissolved inorganic carbon from water column by macro algae growth                                       (gC/m^3/day)
         NH4N_UPTAKE_FROM_WC_MALG_GROWTH         , & ! SUBMODEL_OUTPUT: Uptake of ammonia nitrogen from water column by macro algae growth                                                 (gN/m^3/day)
         NO3N_UPTAKE_FROM_WC_MALG_GROWTH         , & ! SUBMODEL_OUTPUT: Uptake of nitrate nitrogen from water column by macro algae growth                                                 (gN/m^3/day)
         SRP_UPTAKE_FROM_WC_MALG_GROWTH          , & ! SUBMODEL_OUTPUT: Uptake of soluble reactive phosphorus from water column by macro algae growth                                      (gN/m^3/day)
         DO2_TO_WC_MALG_GROWTH                   , & ! SUBMODEL_OUTPUT: Release of dissolved oxygen to water column by macro algae growth                                                  (gO2/m^3/day)
         DIC_TO_WC_MALG_RESP                     , & ! SUBMODEL_OUTPUT: Release of dissolved inorganic carbon to the water column by macro algae respiration                               (gC/m^3/day)
         NH4N_TO_WC_MALG_RESP                    , & ! SUBMODEL_OUTPUT: Release of ammonia nitrogen to the water column by macro algae respiration                                         (gN/m^3/day)
         SRP_TO_WC_MALG_RESP                     , & ! SUBMODEL_OUTPUT: Release of soluble reactive phosphorus to the water column by macro algae respiration                              (gP/m^3/day)
         DO2_UPTAKE_FROM_WC_MALG_RESP            , & ! SUBMODEL_OUTPUT: Uptake of dissolved oxygen from water column by attached alage respiration                                         (gO2/m^3/day)
         DOC_TO_WC_MALG_EXCR                     , & ! SUBMODEL_OUTPUT: Release of dissolved organic carbon to the water column by macro algae excretion                                   (gC/m^3/day)
         DON_TO_WC_MALG_EXCR                     , & ! SUBMODEL_OUTPUT: Release of dissolved organic nitrogen to the water column by macro algae excretion                                 (gN/m^3/day)
         DOP_TO_WC_MALG_EXCR                     , & ! SUBMODEL_OUTPUT: Release of dissolved organic phosphorus to the water column by macro algae excretion                               (gP/m^3/day)
         DOC_TO_WC_MALG_ATT_DETPOC_DISS          , & ! SUBMODEL_OUTPUT: Release of dissolved organic carbon to the water column by macro algeae based attached dead POC dissolution        (gC/m^3/day)
         DON_TO_WC_MALG_ATT_DETPON_DISS          , & ! SUBMODEL_OUTPUT: Release of dissolved organic nitrogen to the water column by macro algeae based attached dead POC dissolution      (gN/m^3/day)
         DOP_TO_WC_MALG_ATT_DETPOP_DISS          , & ! SUBMODEL_OUTPUT: Release of dissolved organic phosphorus to the water column by macro algeae based attached dead POC dissolution    (gP/m^3/day)
         POC_TO_WC_MALG_DEATT                    , & ! SUBMODEL_OUTPUT: Release of particulate organic carbon to the water column by macro algae biomass deattachment                      (gC/m^3/day)
         PON_TO_WC_MALG_DEATT                    , & ! SUBMODEL_OUTPUT: Release of particulate organic nitrogen to the water column by macro algae biomass deattachment                    (gN/m^3/day)
         POP_TO_WC_MALG_DEATT                    , & ! SUBMODEL_OUTPUT: Release of particulate organic phosphorus to the water column macro algae biomass deattachment                     (gP/m^3/day)
         POC_TO_WC_MALG_ATT_DETPOC_DEATT         , & ! SUBMODEL_OUTPUT: Release of particulate organic carbon to the water column by macro algeae based attached dead POC deattachment     (gC/m^3/day)
         PON_TO_WC_MALG_ATT_DETPON_DEATT         , & ! SUBMODEL_OUTPUT: Release of particulate organic nitrogen to the water column by macro algeae based attached dead POC deattachment   (gN/m^3/day)
         POP_TO_WC_MALG_ATT_DETPOP_DEATT           & ! SUBMODEL_OUTPUT: Release of particulate organic phosphorus to the water column by macro algeae based attached dead POC deattachment (gP/m^3/day)
    )
        implicit none

        ! -----------------------------------------------------------------------------------------
        ! INPUTS
        ! -----------------------------------------------------------------------------------------
        integer                              , intent(in)    :: nkn
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: I_A
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: K_E
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: TEMP
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: H
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: MAC_ALGAE_C
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: MAC_ALGAE_N
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: MAC_ALGAE_P
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: MAC_ALGAE_ATT_DETPOC
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: MAC_ALGAE_ATT_DETPON
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: MAC_ALGAE_ATT_DETPOP
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: WC_NH4N
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: WC_NO3N
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: WC_SRP
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: WC_DOC
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: WC_DO2
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: k_DEATT_MAC_ALGAGE
        real(kind = DBL_PREC), dimension(nkn), intent(in)    :: k_DEATT_MALG_ATT_DET

        ! -----------------------------------------------------------------------------------------
        ! OUTPUTS
        ! -----------------------------------------------------------------------------------------
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: d_MALG_C_over_dt
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: d_MALG_N_over_dt
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: d_MALG_P_over_dt
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: d_MALG_ATT_DETPOC_over_dt
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: d_MALG_ATT_DETPON_over_dt
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: d_MALG_ATT_DETPOP_over_dt
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DIC_UPTAKE_FROM_WC_MALG_GROWTH
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: NH4N_UPTAKE_FROM_WC_MALG_GROWTH
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: NO3N_UPTAKE_FROM_WC_MALG_GROWTH
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: SRP_UPTAKE_FROM_WC_MALG_GROWTH
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DO2_TO_WC_MALG_GROWTH
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DIC_TO_WC_MALG_RESP
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: NH4N_TO_WC_MALG_RESP
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: SRP_TO_WC_MALG_RESP
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DO2_UPTAKE_FROM_WC_MALG_RESP
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DOC_TO_WC_MALG_EXCR
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DON_TO_WC_MALG_EXCR
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DOP_TO_WC_MALG_EXCR
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DOC_TO_WC_MALG_ATT_DETPOC_DISS
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DON_TO_WC_MALG_ATT_DETPON_DISS
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: DOP_TO_WC_MALG_ATT_DETPOP_DISS
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: POC_TO_WC_MALG_DEATT
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PON_TO_WC_MALG_DEATT
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: POP_TO_WC_MALG_DEATT
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: POC_TO_WC_MALG_ATT_DETPOC_DEATT
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: PON_TO_WC_MALG_ATT_DETPON_DEATT
        real(kind = DBL_PREC), dimension(nkn), intent(inout) :: POP_TO_WC_MALG_ATT_DETPOP_DEATT
        
        ! -----------------------------------------------------------------------------------------
        ! AUXILLARY
        ! -----------------------------------------------------------------------------------------
        real(kind = DBL_PREC), dimension(nkn) :: WC_DIN                    ! Dissolved inorganic nitrogen in water column                  (gN/m^3)
        real(kind = DBL_PREC), dimension(nkn) :: C_GB                      ! Temperature dependent first order gwroth rate                 (1/day)
        real(kind = DBL_PREC), dimension(nkn) :: Q_N_MAC_ALGAE             ! Nitrogen cell quata for macroalgae                            (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: Q_P_MAC_ALGAE             ! Phosphorus cell quata for macroalgae                          (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_GROWTH_N_LIM    ! Nitrogen based growth limitation factor for macroalgae        (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_GROWTH_P_LIM    ! Phosphorus based growth limitation factor for macroalgae      (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: PHI_NB                    ! Nutrient limitation factor for macroalgae                     (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: PHI_LB                    ! Light limitation factor for macroalgae                        (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: PHI_SB                    ! Space limitation factor for macroalgae                        (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_GROWTH          ! Process rate of macroalgae growth                             (gC/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_RESP_TEMP_LIM   ! Temperature limitation factor for macroalgae respiration      (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_RESP            ! Process rate of macroalgae respiration                        (gC/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_EXCR_TEMP_LIM   ! Temperature limitation factor for macroalgae excretion        (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_EXCR            ! Process rate of macroalgae excretion                          (gC/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_DEATH_TEMP_LIM  ! Temperature limitation factor for macroalgae death            (Unitless)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_DEATH           ! Process rate of macroalgae death                              (gC/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_N_UPTAKE        ! Process rate of macroalgae nitrogen uptake                    (gN/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_P_UPTAKE        ! Process rate of macroalgae phosphorus uptake                  (gP/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_NH4N_PREF       ! Process rate of macroalgae phosphorus uptake                  (gP/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_ATT_DETPOC_DISS ! Process rate of attached dead organic carbon dissolution      (gC/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_ATT_DETPON_DISS ! Process rate of attached dead organic nitrogen dissolution    (gN/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: MAC_ALGAE_ATT_DETPOP_DISS ! Process rate of attached dead organic phposphorus dissolution (gN/m^2/day)
        real(kind = DBL_PREC), dimension(nkn) :: H_SAFE                    ! Water depth clamped to avoid division by zero                 (m)

        ! -------------------------------------------------------------------------------
        ! END OF DECLATATIONS, CALCULATIONS START HERE
        ! -------------------------------------------------------------------------------
        
        WC_DIN = WC_NH4N + WC_NO3N
        H_SAFE = max(H, 1.0D-20)

        ! -------------------------------------------------------------------------------
        ! MARCO ALGAE CARBON
        ! -------------------------------------------------------------------------------

        ! Calculate the temperature limitation for macro algae
        ! C_GB   : Temperature dependent first order growth rate
        C_GB = k_G_20_MAC_ALGAE * (THETA_k_G_MAC_ALGAE**(TEMP - 20.0D0))

        ! Calculate the nutrient limitation for macro algae
        ! PHI_NB : Nutrient limitation
        ! Guard against division by zero when biomass is absent
        where (MAC_ALGAE_C > 1.0D-20)
            Q_N_MAC_ALGAE = MAC_ALGAE_N / MAC_ALGAE_C
            Q_P_MAC_ALGAE = MAC_ALGAE_P / MAC_ALGAE_C
        elsewhere
            Q_N_MAC_ALGAE = 0.0D0
            Q_P_MAC_ALGAE = 0.0D0
        end where

        MAC_ALGAE_GROWTH_N_LIM = 1.0 - (Q_ZERO_N_MAC_ALGAE / max(Q_N_MAC_ALGAE, 1.0D-20))

        where (MAC_ALGAE_GROWTH_N_LIM < 0.0D0)
            MAC_ALGAE_GROWTH_N_LIM = 0.0D0
        end where

        MAC_ALGAE_GROWTH_P_LIM = 1.0 - (Q_ZERO_P_MAC_ALGAE / max(Q_P_MAC_ALGAE, 1.0D-20))
        
        where (MAC_ALGAE_GROWTH_P_LIM < 0.0D0)
            MAC_ALGAE_GROWTH_P_LIM = 0.0D0
        end where

        PHI_NB = min(MAC_ALGAE_GROWTH_N_LIM, MAC_ALGAE_GROWTH_P_LIM)

        ! Calculate the light limitation for macro algae
        ! PHI_LB : Light limitation
        select case (LIGHT_LIM_SUBMODEL_NO)

            ! Baly, (1935)
            case (1)
                PHI_LB = (I_A * exp(-K_E*H)) / (K_LB_MAC_ALGAE + (I_A * exp(-K_E*H)))

            ! Smith, (1936)
            case (2)
                PHI_LB = (I_A * exp(-K_E*H)) / &
                    (sqrt((K_LB_MAC_ALGAE*K_LB_MAC_ALGAE) + (I_A * I_A * exp(-K_E*H) * exp(-K_E*H))))

            ! Steele, (1962)
            case (3)
                PHI_LB = (I_A * exp(-K_E * H) / K_LB_MAC_ALGAE) * &
                    exp(1.0 + ((I_A*exp(-K_E*H))/K_LB_MAC_ALGAE))

            case default
                PHI_LB = 0.0D0
        end select

        ! Calculate the space limitation for macro algae
        ! PHI_SB : Space limitation
        PHI_SB = 1.0D0 - (MAC_ALGAE_C/MAC_ALGAE_C_MAX)

        where (PHI_LB < 0.0D0)
            PHI_LB = 0.0D0
        end where

        MAC_ALGAE_GROWTH = C_GB * PHI_NB * PHI_LB * PHI_SB * MAC_ALGAE_C

        ! Calculate the respiration rate
        MAC_ALGAE_RESP_TEMP_LIM  = THETA_k_R_MAC_ALGAE ** (TEMP - 20.0)
        MAC_ALGAE_RESP = k_R_20_MAC_ALGAE * MAC_ALGAE_RESP_TEMP_LIM * MAC_ALGAE_C

        ! Calculate the excretion rate
        MAC_ALGAE_EXCR_TEMP_LIM  = THETA_k_E_MAC_ALGAE ** (TEMP - 20.0)
        MAC_ALGAE_EXCR = k_E_20_MAC_ALGAE * MAC_ALGAE_EXCR_TEMP_LIM * MAC_ALGAE_C

        ! Calculate the death rate
        MAC_ALGAE_DEATH_TEMP_LIM  = THETA_k_D_MAC_ALGAE  ** (TEMP - 20.0)
        MAC_ALGAE_DEATH = k_D_20_MAC_ALGAE * MAC_ALGAE_DEATH_TEMP_LIM * MAC_ALGAE_C

        d_MALG_C_over_dt = MAC_ALGAE_GROWTH - MAC_ALGAE_RESP - MAC_ALGAE_EXCR - MAC_ALGAE_DEATH
        ! -------------------------------------------------------------------------------
        ! END OF macro algae CARBON
        ! -------------------------------------------------------------------------------


        ! -------------------------------------------------------------------------------
        ! macro algae NITROGEN AND PHOSPHORUS
        ! -------------------------------------------------------------------------------

        ! macro algae nitrogen
        MAC_ALGAE_N_UPTAKE = SIGMA_m_N_MAC_ALGAE * (WC_DIN / (K_HS_DIN_MAC_ALGAE + WC_DIN))

        d_MALG_N_over_dt   = MAC_ALGAE_N_UPTAKE - &
            (Q_N_MAC_ALGAE * ((k_D_20_MAC_ALGAE * MAC_ALGAE_DEATH_TEMP_LIM) + &
            (k_E_20_MAC_ALGAE * MAC_ALGAE_EXCR_TEMP_LIM))) * MAC_ALGAE_C

        ! macro algae phosphorus
        MAC_ALGAE_P_UPTAKE = SIGMA_m_P_MAC_ALGAE * (WC_SRP / (K_HS_SRP_MAC_ALGAE + WC_SRP))
        
        d_MALG_P_over_dt   = MAC_ALGAE_P_UPTAKE - &
            (Q_P_MAC_ALGAE * ((k_D_20_MAC_ALGAE * MAC_ALGAE_DEATH_TEMP_LIM) + &
            (k_E_20_MAC_ALGAE * MAC_ALGAE_EXCR_TEMP_LIM))) * MAC_ALGAE_C

        ! Calculate the macro algae preference to uptake ammonia nitrogen
        ! over nitrate nitrogen
        where (WC_DIN > 1.0D-20)
            MAC_ALGAE_NH4N_PREF = &
                ((WC_NH4N * WC_NO3N) / &
                 ((KHS_NH4PREF_MAC_ALGAE + WC_NH4N) * (KHS_NH4PREF_MAC_ALGAE + WC_NO3N))) + &
                ((WC_NH4N * KHS_NH4PREF_MAC_ALGAE) / (WC_DIN * (KHS_NH4PREF_MAC_ALGAE + WC_NO3N)))
        elsewhere
            MAC_ALGAE_NH4N_PREF = 0.0D0
        end where

        ! Effects of macro algae growth on water column nutrients and dissolved oxygen
        DIC_UPTAKE_FROM_WC_MALG_GROWTH  = MAC_ALGAE_GROWTH / H_SAFE
        NH4N_UPTAKE_FROM_WC_MALG_GROWTH = (MAC_ALGAE_NH4N_PREF * MAC_ALGAE_N_UPTAKE) / H_SAFE
        NO3N_UPTAKE_FROM_WC_MALG_GROWTH = ((1.0 - MAC_ALGAE_NH4N_PREF) * MAC_ALGAE_N_UPTAKE) / H_SAFE
        SRP_UPTAKE_FROM_WC_MALG_GROWTH  = MAC_ALGAE_P_UPTAKE / H_SAFE
        DO2_TO_WC_MALG_GROWTH           = (A_O2_TO_C_MAC_ALGAE * MAC_ALGAE_GROWTH) / H_SAFE

        ! Effects of macro algae respiration on water column nutrients and dissolved oxygen
        DIC_TO_WC_MALG_RESP          = MAC_ALGAE_RESP                         / H_SAFE
        NH4N_TO_WC_MALG_RESP         = (MAC_ALGAE_RESP * Q_N_MAC_ALGAE)       / H_SAFE
        SRP_TO_WC_MALG_RESP          = (MAC_ALGAE_RESP * Q_P_MAC_ALGAE)       / H_SAFE
        DO2_UPTAKE_FROM_WC_MALG_RESP = (MAC_ALGAE_RESP * A_O2_TO_C_MAC_ALGAE) / H_SAFE

        ! Effects of macro algae excretion on water column nutrients
        DOC_TO_WC_MALG_EXCR  = MAC_ALGAE_EXCR / H_SAFE
        DON_TO_WC_MALG_EXCR  = (MAC_ALGAE_EXCR * Q_N_MAC_ALGAE) / H_SAFE
        DOP_TO_WC_MALG_EXCR  = (MAC_ALGAE_EXCR * Q_P_MAC_ALGAE) / H_SAFE
        
        ! -------------------------------------------------------------------------------
        ! END OF MACRO ALGAGE NITROGEN AND PHOSPHORUS
        ! -------------------------------------------------------------------------------

        ! -------------------------------------------------------------------------------
        ! DEAD ORGANIC MATTER
        ! -------------------------------------------------------------------------------
        MAC_ALGAE_ATT_DETPOC_DISS = &
            k_DISS_MALG_ATT_DETPOC * &
            (THETA_k_DISS_MALG_ATT_DETPOC**(TEMP - 20.0)) * MAC_ALGAE_ATT_DETPOC
        
        MAC_ALGAE_ATT_DETPON_DISS = &
            k_DISS_MALG_ATT_DETPON * &
            (THETA_k_DISS_MALG_ATT_DETPON**(TEMP - 20.0)) * MAC_ALGAE_ATT_DETPON
        
        MAC_ALGAE_ATT_DETPOP_DISS = &
            k_DISS_MALG_ATT_DETPOP * (THETA_k_DISS_MALG_ATT_DETPOP **(TEMP - 20.0)) * &
            MAC_ALGAE_ATT_DETPOP

        d_MALG_ATT_DETPOC_over_dt = MAC_ALGAE_DEATH  - MAC_ALGAE_ATT_DETPOC_DISS
        d_MALG_ATT_DETPON_over_dt = (Q_N_MAC_ALGAE * MAC_ALGAE_DEATH) - MAC_ALGAE_ATT_DETPON_DISS
        d_MALG_ATT_DETPOP_over_dt = (Q_P_MAC_ALGAE * MAC_ALGAE_DEATH) - MAC_ALGAE_ATT_DETPOP_DISS

        ! Effects of particulate dead organic matter dissolution on water column
        DOC_TO_WC_MALG_ATT_DETPOC_DISS = MAC_ALGAE_ATT_DETPOC_DISS / H_SAFE
        DON_TO_WC_MALG_ATT_DETPON_DISS = MAC_ALGAE_ATT_DETPON_DISS / H_SAFE
        DOP_TO_WC_MALG_ATT_DETPOP_DISS = MAC_ALGAE_ATT_DETPOP_DISS / H_SAFE

        ! -------------------------------------------------------------------------------
        ! END OF DEAD ORGANIC MATTER
        ! -------------------------------------------------------------------------------

        ! Effects of macroalgae biomass and macroalgae based attached detritus deattachment
        POC_TO_WC_MALG_DEATT            = (k_DEATT_MAC_ALGAGE   * MAC_ALGAE_C)                 / H_SAFE        
        PON_TO_WC_MALG_DEATT            = (k_DEATT_MAC_ALGAGE   * Q_N_MAC_ALGAE * MAC_ALGAE_C) / H_SAFE 
        POP_TO_WC_MALG_DEATT            = (k_DEATT_MAC_ALGAGE   * Q_P_MAC_ALGAE * MAC_ALGAE_C) / H_SAFE
        POC_TO_WC_MALG_ATT_DETPOC_DEATT = (k_DEATT_MALG_ATT_DET * MAC_ALGAE_ATT_DETPOC)        / H_SAFE
        PON_TO_WC_MALG_ATT_DETPON_DEATT = (k_DEATT_MALG_ATT_DET * MAC_ALGAE_ATT_DETPON)        / H_SAFE
        POP_TO_WC_MALG_ATT_DETPOP_DEATT = (k_DEATT_MALG_ATT_DET * MAC_ALGAE_ATT_DETPOP)        / H_SAFE

    end subroutine MACRO_ALGAE_KINETICS
    
end module MacroAlgae
