"""
Complete PROCESS_RATES slot mapping for AQUABC pelagic model.

Extracted from: SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90

PROCESS_RATES is 3D: PROCESS_RATES(nkn, nstate, NDIAGVAR)
  nstate = 32 (or 36 with allelopathy extras)
  NDIAGVAR = 30

For each state variable:
  - 'slots' maps slot_number -> description of what is stored
  - 'derivative_formula' shows how DERIVATIVES is assembled from slots
    '+N' means add slot N, '-N' means subtract slot N
  - 'conditions' notes which conditional blocks affect slot population

Slots marked (AUX) are diagnostic/auxiliary and NOT used in the derivative
formula. They store limitation factors, preferences, or other diagnostics.
"""

# fmt: off

SLOT_MAP = {
    # =========================================================================
    # 1. NH4_N  (var_index = NH4_N_INDEX = 1)
    # =========================================================================
    'NH4_N': {
        'var_index': 1,
        'slots': {
            1:  'R_DIA_TOT_RESP * DIA_N_TO_C  (DIA respiration NH4 release)',
            2:  'R_CYN_TOT_RESP * CYN_N_TO_C  (CYN respiration NH4 release)',
            3:  'R_OPA_TOT_RESP * OPA_N_TO_C  (OPA respiration NH4 release)',
            4:  'R_FIX_CYN_TOT_RESP * FIX_CYN_N_TO_C  (FIX_CYN resp NH4 release) [DO_NON_OBLIGATORY_FIXERS]',
            5:  'R_ZOO_TOT_RESP * ACTUAL_ZOO_N_TO_C  (ZOO resp NH4 release)',
            6:  'R_DIA_GROWTH * PREF_NH4N_DIA * DIA_N_TO_C  (DIA NH4 uptake)',
            7:  'R_CYN_GROWTH * CYN_N_TO_C * PREF_DIN_DON_CYN * PREF_NH4N_CYN  (CYN NH4 uptake)',
            8:  'R_OPA_GROWTH * PREF_NH4N_OPA * OPA_N_TO_C  (OPA NH4 uptake)',
            9:  'R_NON_FIX_CYN_GROWTH * PREF_NH4N_DON_FIX_CYN * FIX_CYN_N_TO_C * NH4/(NH4+DON*frac)  (FIX_CYN NH4 uptake via non-fix path) [DO_NON_OBLIGATORY_FIXERS]',
            10: 'R_ABIOTIC_NITR  (nitrification loss of NH4)',
            11: 'Sum(R_ABIOTIC_DON_MIN_*)  (DON mineralization -> NH4: DOXY+NO3N+MN_IV+FE_III+S_PLUS_6+DOC pathways)',
            12: 'R_AMMONIA_VOLATIL  (NH3 volatilization loss)',
            13: 'PREF_NH4N_DIA  (AUX: DIA NH4 preference factor)',
            14: '0.0  (AUX: deprecated, was PREF_NH4N_DON_CYN)',
            15: 'PREF_NH4N_OPA  (AUX: OPA NH4 preference factor)',
            16: 'PREF_NH4N_DON_FIX_CYN  (AUX: FIX_CYN NH4-vs-DON preference)',
            17: 'R_NOST_VEG_HET_TOT_RESP * NOST_N_TO_C  (NOST resp NH4 release) [DO_NOSTOCALES]',
            18: 'PREF_NH4N_CYN  (AUX: CYN NH4 preference factor)',
            19: 'PREF_DIN_DON_CYN  (AUX: CYN DIN-vs-DON preference)',
            20: 'PREF_NH4N_NOST  (AUX: NOST NH4 preference factor)',
            21: 'PREF_DIN_DON_NOST  (AUX: NOST DIN-vs-DON preference)',
            22: 'R_NOST_VEG_HET_FIX_GROWTH * NOST_N_TO_C * ((1-FRAC_FIX_N_FOR_GR)/FRAC_FIX_N_FOR_GR)  (NH4 produced during N-fixation) [DO_NOSTOCALES]',
            23: 'R_NOST_VEG_HET_NON_FIX_GROWTH * NOST_N_TO_C * PREF_DIN_DON_NOST * PREF_NH4N_NOST  (NOST NH4 uptake) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 +3 +4 +5 -6 -7 -8 -9 -10 +11 -12 +17 +22 -23',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 17, 22, 23],
        'conditions': 'Slots 4,9 need DO_NON_OBLIGATORY_FIXERS; Slots 17,22,23 need DO_NOSTOCALES',
    },

    # =========================================================================
    # 2. NO3_N  (var_index = NO3_N_INDEX = 2)
    # =========================================================================
    'NO3_N': {
        'var_index': 2,
        'slots': {
            1: 'R_ABIOTIC_NITR  (nitrification: NH4 -> NO3 gain)',
            2: 'R_DENITRIFICATION  (= 0.93 * R_ABIOTIC_DOC_MIN_NO3N, denitrification loss)',
            3: 'R_DIA_GROWTH * (1-PREF_NH4N_DIA) * DIA_N_TO_C  (DIA NO3 uptake)',
            4: 'R_CYN_GROWTH * CYN_N_TO_C * PREF_DIN_DON_CYN * (1-PREF_NH4N_CYN)  (CYN NO3 uptake)',
            5: 'R_OPA_GROWTH * (1-PREF_NH4N_OPA) * OPA_N_TO_C  (OPA NO3 uptake)',
            6: '[if DO_NOSTOCALES]: R_NOST_VEG_HET_NON_FIX_GROWTH * NOST_N_TO_C * PREF_DIN_DON_NOST * (1-PREF_NH4N_NOST)  (NOST NO3 uptake) '
               '[else]: R_NON_FIX_CYN_GROWTH * (1-PREF_NH4N_DON_FIX_CYN) * FIX_CYN_N_TO_C  (FIX_CYN NO3 uptake)',
            7: 'PREF_NH4N_DIA  (AUX)',
            8: '0.0  (AUX: deprecated)',
            9: 'PREF_NH4N_OPA  (AUX)',
        },
        'derivative_formula': '+1 -2 -3 -4 -5 -6',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6],
        'conditions': 'Slot 6 content depends on DO_NOSTOCALES',
    },

    # =========================================================================
    # 3. PO4_P  (var_index = PO4_P_INDEX = 3)
    # =========================================================================
    'PO4_P': {
        'var_index': 3,
        'slots': {
            1:  'R_DIA_TOT_RESP * DIA_P_TO_C  (DIA resp PO4 release)',
            2:  'R_CYN_TOT_RESP * CYN_P_TO_C  (CYN resp PO4 release)',
            3:  'R_OPA_TOT_RESP * OPA_P_TO_C  (OPA resp PO4 release)',
            4:  'R_FIX_CYN_TOT_RESP * FIX_CYN_P_TO_C  (FIX_CYN resp PO4 release) [DO_NON_OBLIGATORY_FIXERS]',
            5:  'R_ZOO_TOT_RESP * ACTUAL_ZOO_P_TO_C  (ZOO resp PO4 release)',
            6:  'R_DIA_GROWTH * DIA_P_TO_C  (DIA PO4 uptake)',
            7:  'R_CYN_GROWTH * CYN_P_TO_C  (CYN PO4 uptake)',
            8:  'R_OPA_GROWTH * OPA_P_TO_C  (OPA PO4 uptake)',
            9:  'R_FIX_CYN_GROWTH * FIX_CYN_P_TO_C  (FIX_CYN PO4 uptake) [DO_NON_OBLIGATORY_FIXERS]',
            10: 'Sum(R_ABIOTIC_DOP_MIN_*)  (DOP mineralization -> PO4: DOXY+NO3N+MN_IV+FE_III+S_PLUS_6+DOC pathways)',
            11: 'TEMP  (AUX: temperature)',
            12: 'DISS_ORG_P  (AUX: dissolved organic P concentration)',
            13: 'R_NOST_VEG_HET_TOT_RESP * NOST_P_TO_C  (NOST resp PO4 release) [DO_NOSTOCALES]',
            14: 'R_NOST_VEG_HET_GROWTH * NOST_P_TO_C * PREF_DIP_DOP_NOST  (NOST DIP uptake) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 +3 +4 +5 -6 -7 -8 -9 +10 +13 -14',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14],
        'conditions': 'Slots 4,9 need DO_NON_OBLIGATORY_FIXERS; Slots 13,14 need DO_NOSTOCALES',
    },

    # =========================================================================
    # 4. DISS_OXYGEN  (var_index = DISS_OXYGEN_INDEX = 4)
    # =========================================================================
    'DISS_OXYGEN': {
        'var_index': 4,
        'slots': {
            1:  'R_AERATION  (O2 reaeration, surface boxes only)',
            2:  'R_DIA_GROWTH * (1.3 - 0.3*PREF_NH4N_DIA) * DIA_O2_TO_C  (DIA photosynthetic O2 production)',
            3:  'R_CYN_GROWTH * (1.3 - 0.3*PREF_NH4N_CYN*PREF_DIN_DON_CYN) * CYN_O2_TO_C  (CYN O2 production)',
            4:  'R_OPA_GROWTH * (1.3 - 0.3*PREF_NH4N_OPA) * OPA_O2_TO_C  (OPA O2 production)',
            5:  '[if DO_NOSTOCALES]: R_NOST_VEG_HET_GROWTH * NOST_O2_TO_C * (1.3-0.3*PREF_NH4N_NOST*PREF_DIN_DON_NOST)  (NOST O2 production) '
                '[else]: R_FIX_CYN_GROWTH * (1.3-0.3*PREF_NH4N_DON_FIX_CYN) * FIX_CYN_O2_TO_C  (FIX_CYN O2 production)',
            6:  'R_DIA_TOT_RESP * DIA_O2_TO_C  (DIA respiration O2 consumption)',
            7:  'R_CYN_TOT_RESP * CYN_O2_TO_C  (CYN respiration O2 consumption)',
            8:  'R_OPA_TOT_RESP * OPA_O2_TO_C  (OPA respiration O2 consumption)',
            9:  'R_FIX_CYN_TOT_RESP * FIX_CYN_O2_TO_C  (FIX_CYN resp O2 consumption) [DO_NON_OBLIGATORY_FIXERS]',
            10: 'R_ZOO_TOT_RESP * ZOO_O2_TO_C  (ZOO respiration O2 consumption)',
            11: 'R_ABIOTIC_NITR * 4.57  (nitrification O2 demand)',
            12: '2.66 * R_ABIOTIC_DOC_MIN_DOXY  (aerobic DOC mineralization O2 demand)',
            13: '0.43 * R_FE_II_OXIDATION  (Fe2+ oxidation O2 demand) [ADVANCED_REDOX]',
            14: '0.88 * R_MN_II_OXIDATION  (Mn2+ oxidation O2 demand) [ADVANCED_REDOX]',
            15: '2.00 * R_SULPHIDE_OXIDATION  (sulphide oxidation O2 demand) [ADVANCED_REDOX]',
            16: '5.33 * R_METHANE_OXIDATION  (methane oxidation O2 demand) [ADVANCED_REDOX]',
            17: 'K_A_CALC  (AUX: reaeration rate constant)',
            18: 'DISS_OXYGEN_SAT  (AUX: O2 saturation concentration)',
            19: '[if DO_NOSTOCALES && DO_NON_OBLIGATORY_FIXERS]: R_FIX_CYN_GROWTH*(1.3-0.3*PREF)*FIX_CYN_O2_TO_C  (FIX_CYN O2 production when slot 5=NOST) '
                '[else]: 0.0',
            20: 'R_NOST_VEG_HET_TOT_RESP * NOST_O2_TO_C  (NOST resp O2 consumption) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 +3 +4 +5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16 +19 -20',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 19, 20],
        'conditions': 'Slot 5 meaning depends on DO_NOSTOCALES; Slot 9 needs DO_NON_OBLIGATORY_FIXERS; '
                      'Slots 13-16 need ADVANCED_REDOX; Slot 19 needs both DO_NOSTOCALES+DO_NON_OBLIGATORY_FIXERS; '
                      'Slot 20 needs DO_NOSTOCALES',
    },

    # =========================================================================
    # 5. DIA_C  (var_index = DIA_C_INDEX = 5)
    # =========================================================================
    'DIA_C': {
        'var_index': 5,
        'slots': {
            1:  'R_DIA_GROWTH  (diatom growth)',
            2:  'R_DIA_TOT_RESP  (diatom total respiration = basal + internal)',
            3:  'R_DIA_EXCR  (diatom excretion to DOC)',
            4:  'R_DIA_DEATH  (diatom mortality -> detritus)',
            5:  'R_ZOO_FEEDING_DIA  (zooplankton grazing on diatoms)',
            6:  'LIM_KG_DIA_TEMP  (AUX: temperature limitation)',
            7:  'LIM_KG_DIA_DOXY  (AUX: oxygen limitation)',
            8:  'LIM_KG_DIA_N  (AUX: nitrogen limitation)',
            9:  'LIM_KG_DIA_P  (AUX: phosphorus limitation)',
            10: 'LIM_KG_DIA_DISS_Si  (AUX: silica limitation)',
            11: 'LIM_KG_DIA_LIGHT  (AUX: light limitation)',
            12: 'DIA_LIGHT_SAT  (AUX: light saturation intensity)',
        },
        'derivative_formula': '+1 -2 -3 -4 -5',
        'derivative_slots_used': [1, 2, 3, 4, 5],
        'conditions': 'None',
    },

    # =========================================================================
    # 6. ZOO_C  (var_index = ZOO_C_INDEX = 6)
    # =========================================================================
    'ZOO_C': {
        'var_index': 6,
        'slots': {
            1:  'R_ZOO_GROWTH  (zooplankton net growth)',
            2:  'R_ZOO_EX_DOC  (ZOO excretion of DOC) [ZOOP_OPTION_1, else 0]',
            3:  'R_ZOO_TOT_RESP  (ZOO total respiration)',
            4:  'R_ZOO_DEATH  (ZOO mortality -> detritus)',
            5:  'R_ZOO_FEEDING_DIA  (AUX: grazing rate on DIA)',
            6:  'R_ZOO_FEEDING_CYN  (AUX: grazing rate on CYN)',
            7:  'R_ZOO_FEEDING_OPA  (AUX: grazing rate on OPA)',
            8:  'R_ZOO_FEEDING_FIX_CYN  (AUX: grazing on FIX_CYN) [DO_NON_OBLIGATORY_FIXERS]',
            9:  'R_ZOO_FEEDING_DET_PART_ORG_C  (AUX: grazing on detritus)',
            10: 'R_ZOO_FEEDING_NOST_VEG_HET  (AUX: grazing on NOST) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 -2 -3 -4',
        'derivative_slots_used': [1, 2, 3, 4],
        'conditions': 'Slot 2 needs ZOOP_OPTION_1; Slot 8 needs DO_NON_OBLIGATORY_FIXERS; Slot 10 needs DO_NOSTOCALES',
    },

    # =========================================================================
    # 7. ZOO_N  (var_index = ZOO_N_INDEX = 7)
    # =========================================================================
    'ZOO_N': {
        'var_index': 7,
        'slots': {
            1:  'R_ZOO_FEEDING_DIA * DIA_N_TO_C  (N ingested from DIA) [ZOOP_OPTION_1]',
            2:  'R_ZOO_FEEDING_CYN * CYN_N_TO_C  (N ingested from CYN) [ZOOP_OPTION_1]',
            3:  'R_ZOO_FEEDING_OPA * OPA_N_TO_C  (N ingested from OPA) [ZOOP_OPTION_1]',
            4:  'R_ZOO_FEEDING_FIX_CYN * FIX_CYN_N_TO_C  (N ingested from FIX_CYN) [ZOOP_OPTION_1 + DO_NON_OBLIGATORY_FIXERS]',
            5:  'R_ZOO_FEEDING_DET_PART_ORG_C * ACTUAL_DET_N_TO_C  (N ingested from detritus) [ZOOP_OPTION_1]',
            6:  'R_ZOO_EX_DON  (DON excretion loss) [ZOOP_OPTION_1]',
            7:  'R_ZOO_TOT_RESP * ACTUAL_ZOO_N_TO_C  (N lost via respiration) [ZOOP_OPTION_1]',
            8:  'R_ZOO_DEATH * ACTUAL_ZOO_N_TO_C  (N lost via mortality) [ZOOP_OPTION_1]',
            9:  'ACTUAL_ZOO_N_TO_C  (AUX: actual N:C ratio) [ZOOP_OPTION_1]',
            10: 'R_ZOO_FEEDING_NOST_VEG_HET * NOST_N_TO_C  (N ingested from NOST) [ZOOP_OPTION_1 + DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 +3 +4 +5 -6 -7 -8 +10',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 8, 10],
        'conditions': 'All slots and explicit derivative only under ZOOP_OPTION_1; '
                      'otherwise DERIV = DERIV(ZOO_C) * ACTUAL_ZOO_N_TO_C',
    },

    # =========================================================================
    # 8. ZOO_P  (var_index = ZOO_P_INDEX = 8)
    # =========================================================================
    'ZOO_P': {
        'var_index': 8,
        'slots': {
            1:  'R_ZOO_FEEDING_DIA * DIA_P_TO_C  (P ingested from DIA) [ZOOP_OPTION_1]',
            2:  'R_ZOO_FEEDING_CYN * CYN_P_TO_C  (P ingested from CYN) [ZOOP_OPTION_1]',
            3:  'R_ZOO_FEEDING_OPA * OPA_P_TO_C  (P ingested from OPA) [ZOOP_OPTION_1]',
            4:  'R_ZOO_FEEDING_FIX_CYN * FIX_CYN_P_TO_C  (P ingested from FIX_CYN) [ZOOP_OPTION_1 + DO_NON_OBLIGATORY_FIXERS]',
            5:  'R_ZOO_FEEDING_DET_PART_ORG_C * ACTUAL_DET_P_TO_C  (P ingested from detritus) [ZOOP_OPTION_1]',
            6:  'R_ZOO_EX_DOP  (DOP excretion loss) [ZOOP_OPTION_1]',
            7:  'R_ZOO_TOT_RESP * ACTUAL_ZOO_P_TO_C  (P lost via respiration) [ZOOP_OPTION_1]',
            8:  'R_ZOO_DEATH * ACTUAL_ZOO_P_TO_C  (P lost via mortality) [ZOOP_OPTION_1]',
            9:  'ACTUAL_ZOO_P_TO_C  (AUX: actual P:C ratio) [ZOOP_OPTION_1]',
            10: 'R_ZOO_FEEDING_NOST_VEG_HET * NOST_P_TO_C  (P ingested from NOST) [ZOOP_OPTION_1 + DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 +3 +4 +5 -6 -7 -8 +10',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 8, 10],
        'conditions': 'All slots and explicit derivative only under ZOOP_OPTION_1; '
                      'otherwise DERIV = DERIV(ZOO_C) * ACTUAL_ZOO_P_TO_C',
    },

    # =========================================================================
    # 9. DET_PART_ORG_C  (var_index = DET_PART_ORG_C_INDEX = 9)
    # =========================================================================
    'DET_PART_ORG_C': {
        'var_index': 9,
        'slots': {
            1:  'R_DIA_DEATH  (DIA death -> POC)',
            2:  'R_CYN_DEATH  (CYN death -> POC)',
            3:  'R_OPA_DEATH  (OPA death -> POC)',
            4:  'R_FIX_CYN_DEATH  (FIX_CYN death -> POC) [DO_NON_OBLIGATORY_FIXERS]',
            5:  'R_ZOO_DEATH  (ZOO death -> POC)',
            6:  'R_ZOO_FEEDING_DET_PART_ORG_C  (ZOO grazing on POC, loss)',
            7:  'R_DET_PART_ORG_C_DISSOLUTION  (POC dissolution -> DOC)',
            8:  'R_NOST_VEG_HET_DEATH  (NOST death -> POC) [DO_NOSTOCALES]',
            9:  'R_DENS_MORT_NOST_VEG_HET  (NOST density-dependent mortality -> POC) [DO_NOSTOCALES]',
            10: 'R_MORT_AKI  (Akinete mortality -> POC) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 +3 +4 +5 -6 -7 +8 +9 +10',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        'conditions': 'Slot 4 needs DO_NON_OBLIGATORY_FIXERS; Slots 8,9,10 need DO_NOSTOCALES',
    },

    # =========================================================================
    # 10. DET_PART_ORG_N  (var_index = DET_PART_ORG_N_INDEX = 10)
    # =========================================================================
    'DET_PART_ORG_N': {
        'var_index': 10,
        'slots': {
            1:  'R_DIA_DEATH * DIA_N_TO_C  (DIA death -> PON)',
            2:  'R_CYN_DEATH * CYN_N_TO_C  (CYN death -> PON)',
            3:  'R_OPA_DEATH * OPA_N_TO_C  (OPA death -> PON)',
            4:  'R_FIX_CYN_DEATH * FIX_CYN_N_TO_C  (FIX_CYN death -> PON) [DO_NON_OBLIGATORY_FIXERS]',
            5:  'R_ZOO_DEATH * ACTUAL_ZOO_N_TO_C  (ZOO death -> PON)',
            6:  'R_ZOO_FEEDING_DET_PART_ORG_C * ACTUAL_DET_N_TO_C  (ZOO grazing -> PON loss)',
            7:  'R_DET_PART_ORG_N_DISSOLUTION  (PON dissolution -> DON)',
            8:  'ACTUAL_DET_N_TO_C  (AUX: detritus N:C ratio)',
            9:  'R_NOST_VEG_HET_DEATH * NOST_N_TO_C  (NOST death -> PON) [DO_NOSTOCALES]',
            10: 'R_DENS_MORT_NOST_VEG_HET * NOST_N_TO_C  (NOST density mort -> PON) [DO_NOSTOCALES]',
            11: 'R_MORT_AKI * NOST_N_TO_C  (Akinete mortality -> PON) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 +3 +4 +5 -6 -7 +9 +10 +11',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 9, 10, 11],
        'conditions': 'Slot 4 needs DO_NON_OBLIGATORY_FIXERS; Slots 9,10,11 need DO_NOSTOCALES; '
                      'Slot 8 is AUX only',
    },

    # =========================================================================
    # 11. DET_PART_ORG_P  (var_index = DET_PART_ORG_P_INDEX = 11)
    # =========================================================================
    'DET_PART_ORG_P': {
        'var_index': 11,
        'slots': {
            1:  'R_DIA_DEATH * DIA_P_TO_C  (DIA death -> POP)',
            2:  'R_CYN_DEATH * CYN_P_TO_C  (CYN death -> POP)',
            3:  'R_OPA_DEATH * OPA_P_TO_C  (OPA death -> POP)',
            4:  'R_FIX_CYN_DEATH * FIX_CYN_P_TO_C  (FIX_CYN death -> POP) [DO_NON_OBLIGATORY_FIXERS]',
            5:  'R_ZOO_DEATH * ACTUAL_ZOO_P_TO_C  (ZOO death -> POP)',
            6:  'R_ZOO_FEEDING_DET_PART_ORG_C * ACTUAL_DET_P_TO_C  (ZOO grazing -> POP loss)',
            7:  'R_DET_PART_ORG_P_DISSOLUTION  (POP dissolution -> DOP)',
            8:  'ACTUAL_DET_P_TO_C  (AUX: detritus P:C ratio)',
            9:  'R_NOST_VEG_HET_DEATH * NOST_P_TO_C  (NOST death -> POP) [DO_NOSTOCALES]',
            10: 'R_DENS_MORT_NOST_VEG_HET * NOST_P_TO_C  (NOST density mort -> POP) [DO_NOSTOCALES]',
            11: 'R_MORT_AKI * NOST_P_TO_C  (Akinete mortality -> POP) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 +3 +4 +5 -6 -7 +9 +10 +11',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 9, 10, 11],
        'conditions': 'Slot 4 needs DO_NON_OBLIGATORY_FIXERS; Slots 9,10,11 need DO_NOSTOCALES; '
                      'Slot 8 is AUX only',
    },

    # =========================================================================
    # 12. DISS_ORG_C  (var_index = DISS_ORG_C_INDEX = 12)
    # =========================================================================
    'DISS_ORG_C': {
        'var_index': 12,
        'slots': {
            1: 'R_DET_PART_ORG_C_DISSOLUTION  (POC dissolution -> DOC gain)',
            2: 'R_ZOO_EX_DOC  (ZOO DOC excretion gain)',
            3: 'Sum(R_ABIOTIC_DOC_MIN_*)  (total DOC mineralization loss: DOXY+NO3N+MN_IV+FE_III+S_PLUS_6+DOC)',
            4: 'Sum of all phyto excretion: R_DIA_EXCR + R_CYN_EXCR + R_OPA_EXCR '
               '[+ R_FIX_CYN_EXCR if DO_NON_OBLIGATORY_FIXERS] [+ R_NOST_VEG_HET_EXCR if DO_NOSTOCALES]  (total phyto DOC excretion gain)',
            5: 'R_DIA_EXCR  (AUX: DIA excretion component)',
            6: 'R_CYN_EXCR  (AUX: CYN excretion component)',
            7: 'R_OPA_EXCR  (AUX: OPA excretion component)',
            8: 'R_FIX_CYN_EXCR  (AUX: FIX_CYN excretion component) [DO_NON_OBLIGATORY_FIXERS]',
            9: 'R_NOST_VEG_HET_EXCR  (AUX: NOST excretion component) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 -3 +4',
        'derivative_slots_used': [1, 2, 3, 4],
        'conditions': 'Slot 4 accumulates FIX_CYN and NOST excretion conditionally; '
                      'Slot 8 needs DO_NON_OBLIGATORY_FIXERS; Slot 9 needs DO_NOSTOCALES',
    },

    # =========================================================================
    # 13. DISS_ORG_N  (var_index = DISS_ORG_N_INDEX = 13)
    # =========================================================================
    'DISS_ORG_N': {
        'var_index': 13,
        'slots': {
            1:  'R_DET_PART_ORG_N_DISSOLUTION  (PON dissolution -> DON gain)',
            2:  'R_ZOO_EX_DON  (ZOO DON excretion gain)',
            3:  'Sum(R_ABIOTIC_DON_MIN_*)  (total DON mineralization loss: DOXY+NO3N+MN_IV+FE_III+S_PLUS_6+DOC)',
            4:  'Sum of all phyto excretion*N_TO_C: (R_DIA_EXCR*DIA_N_TO_C + R_CYN_EXCR*CYN_N_TO_C + R_OPA_EXCR*OPA_N_TO_C) '
                '[+ R_FIX_CYN_EXCR*FIX_CYN_N_TO_C if DO_NON_OBLIGATORY_FIXERS] [+ R_NOST_VEG_HET_EXCR*NOST_N_TO_C if DO_NOSTOCALES]  (total phyto DON excretion gain)',
            5:  'R_CYN_GROWTH * CYN_N_TO_C * (1-PREF_DIN_DON_CYN)  (CYN DON uptake loss)',
            6:  '[if DO_NOSTOCALES]: R_NOST_VEG_HET_NON_FIX_GROWTH * NOST_N_TO_C * (1-PREF_DIN_DON_NOST)  (NOST DON uptake) '
                '[else]: R_NON_FIX_CYN_GROWTH * PREF_NH4N_DON_FIX_CYN * FIX_CYN_N_TO_C * DON_frac/(NH4+DON_frac)  (FIX_CYN DON uptake)',
            7:  'R_DIA_EXCR  (AUX: DIA excretion rate)',
            8:  'R_CYN_EXCR  (AUX: CYN excretion rate)',
            9:  'R_OPA_EXCR  (AUX: OPA excretion rate)',
            10: 'R_FIX_CYN_EXCR * FIX_CYN_N_TO_C  (AUX: FIX_CYN N excretion) [DO_NON_OBLIGATORY_FIXERS]',
            11: 'R_NOST_VEG_HET_EXCR * NOST_N_TO_C  (AUX: NOST N excretion) [DO_NOSTOCALES]',
            12: 'PHYT_TOT_C  (AUX: total phytoplankton carbon)',
        },
        'derivative_formula': '+1 +2 -3 +4 -5 -6',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6],
        'conditions': 'Slot 4 accumulates conditionally; Slot 5 recalculated if DO_NOSTOCALES; '
                      'Slot 6 content depends on DO_NOSTOCALES; '
                      'Slot 10 needs DO_NON_OBLIGATORY_FIXERS; Slot 11 needs DO_NOSTOCALES',
    },

    # =========================================================================
    # 14. DISS_ORG_P  (var_index = DISS_ORG_P_INDEX = 14)
    # =========================================================================
    'DISS_ORG_P': {
        'var_index': 14,
        'slots': {
            1:  'R_DET_PART_ORG_P_DISSOLUTION  (POP dissolution -> DOP gain)',
            2:  'R_ZOO_EX_DOP  (ZOO DOP excretion gain)',
            3:  'Sum(R_ABIOTIC_DOP_MIN_*)  (total DOP mineralization loss: DOXY+NO3N+MN_IV+FE_III+S_PLUS_6+DOC)',
            4:  'Sum of all phyto excretion*P_TO_C: (R_DIA_EXCR*DIA_P_TO_C + R_CYN_EXCR*CYN_P_TO_C + R_OPA_EXCR*OPA_P_TO_C) '
                '[+ R_FIX_CYN_EXCR*FIX_CYN_P_TO_C if DO_NON_OBLIGATORY_FIXERS] [+ R_NOST_VEG_HET_EXCR*NOST_P_TO_C if DO_NOSTOCALES]  (total phyto DOP excretion gain)',
            5:  'R_DIA_EXCR  (AUX: DIA excretion rate)',
            6:  'R_CYN_EXCR  (AUX: CYN excretion rate)',
            7:  'R_OPA_EXCR  (AUX: OPA excretion rate)',
            8:  'R_FIX_CYN_EXCR * FIX_CYN_P_TO_C  (AUX: FIX_CYN P excretion) [DO_NON_OBLIGATORY_FIXERS]',
            9:  'R_NOST_VEG_HET_EXCR * NOST_P_TO_C  (AUX: NOST P excretion) [DO_NOSTOCALES]',
            10: 'R_NOST_VEG_HET_GROWTH * NOST_P_TO_C * (1-PREF_DIP_DOP_NOST)  (NOST DOP uptake loss) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+1 +2 -3 +4 -10',
        'derivative_slots_used': [1, 2, 3, 4, 10],
        'conditions': 'Slot 4 accumulates conditionally; Slot 8 needs DO_NON_OBLIGATORY_FIXERS; '
                      'Slots 9,10 need DO_NOSTOCALES',
    },

    # =========================================================================
    # 15. CYN_C  (var_index = CYN_C_INDEX = 15)
    # =========================================================================
    'CYN_C': {
        'var_index': 15,
        'slots': {
            1:  'R_CYN_GROWTH  (CYN growth)',
            2:  'R_CYN_TOT_RESP  (CYN total respiration)',
            3:  'R_CYN_EXCR  (CYN excretion)',
            4:  'R_CYN_DEATH  (CYN mortality)',
            5:  'R_ZOO_FEEDING_CYN  (ZOO grazing on CYN)',
            6:  'LIM_KG_CYN_TEMP  (AUX: temperature limitation)',
            7:  'LIM_KG_CYN_DOXY  (AUX: oxygen limitation)',
            8:  'LIM_KG_CYN_N  (AUX: nitrogen limitation)',
            9:  'LIM_KG_CYN_P  (AUX: phosphorus limitation)',
            10: 'LIM_KG_CYN_LIGHT  (AUX: light limitation)',
            11: 'I_A  (AUX: light in langleys)',
            12: 'CYN_LIGHT_SAT  (AUX: light saturation)',
            13: 'TEMP  (AUX: temperature)',
        },
        'derivative_formula': '+1 -2 -3 -4 -5',
        'derivative_slots_used': [1, 2, 3, 4, 5],
        'conditions': 'None',
    },

    # =========================================================================
    # 16. OPA_C  (var_index = OPA_C_INDEX = 16)
    # =========================================================================
    'OPA_C': {
        'var_index': 16,
        'slots': {
            1:  'R_OPA_GROWTH  (OPA growth)',
            2:  'R_OPA_TOT_RESP  (OPA total respiration)',
            3:  'R_OPA_EXCR  (OPA excretion)',
            4:  'R_OPA_DEATH  (OPA mortality)',
            5:  'R_ZOO_FEEDING_OPA  (ZOO grazing on OPA)',
            6:  'LIM_KG_OPA_TEMP  (AUX: temperature limitation)',
            7:  'LIM_KG_OPA_DOXY  (AUX: oxygen limitation)',
            8:  'LIM_KG_OPA_N  (AUX: nitrogen limitation)',
            9:  'LIM_KG_OPA_P  (AUX: phosphorus limitation)',
            10: 'LIM_KG_OPA_LIGHT  (AUX: light limitation)',
            11: 'OPA_LIGHT_SAT  (AUX: light saturation)',
        },
        'derivative_formula': '+1 -2 -3 -4 -5',
        'derivative_slots_used': [1, 2, 3, 4, 5],
        'conditions': 'None',
    },

    # =========================================================================
    # 17. DISS_Si  (var_index = DISS_Si_INDEX = 17)
    # =========================================================================
    'DISS_Si': {
        'var_index': 17,
        'slots': {
            1: 'R_PART_Si_DISS  (biogenic Si dissolution -> diss Si gain)',
            2: 'R_DIA_TOT_RESP * DIA_SI_TO_C  (DIA resp Si release)',
            3: 'R_DIA_EXCR * DIA_SI_TO_C  (DIA excretion Si release)',
            4: 'R_DIA_GROWTH * DIA_SI_TO_C  (DIA Si uptake)',
        },
        'derivative_formula': '+1 +2 +3 -4',
        'derivative_slots_used': [1, 2, 3, 4],
        'conditions': 'None',
    },

    # =========================================================================
    # 18. PART_Si  (var_index = PART_Si_INDEX = 18)
    # =========================================================================
    'PART_Si': {
        'var_index': 18,
        'slots': {
            1: 'R_DIA_DEATH * DIA_Si_TO_C  (DIA death -> particulate Si)',
            2: 'R_ZOO_FEEDING_DIA * DIA_Si_TO_C  (ZOO grazing DIA -> particulate Si)',
            3: 'R_PART_Si_DISS  (particulate Si dissolution loss)',
        },
        'derivative_formula': '+1 +2 -3',
        'derivative_slots_used': [1, 2, 3],
        'conditions': 'None',
    },

    # =========================================================================
    # 19. FIX_CYN_C  (var_index = FIX_CYN_C_INDEX = 19)
    # =========================================================================
    'FIX_CYN_C': {
        'var_index': 19,
        'slots': {
            1:  'R_FIX_CYN_GROWTH  (FIX_CYN total growth = non-fix + fix)',
            2:  'R_FIX_CYN_TOT_RESP  (FIX_CYN total respiration)',
            3:  'R_FIX_CYN_EXCR  (FIX_CYN excretion)',
            4:  'R_FIX_CYN_DEATH  (FIX_CYN mortality)',
            5:  'R_ZOO_FEEDING_FIX_CYN  (ZOO grazing on FIX_CYN)',
            6:  'R_NON_FIX_CYN_GROWTH  (AUX: non-fixing pathway growth rate)',
            7:  'R_FIX_FIX_CYN_GROWTH  (AUX: N-fixing pathway growth rate)',
            8:  'R_FIX_FIX_CYN_GROWTH * FIX_CYN_N_TO_C  (AUX: N fixation rate by FIX_CYN)',
            9:  'LIM_KG_FIX_CYN_TEMP  (AUX: temp limitation; NOTE: later overwritten += NOST N fixation if DO_NOSTOCALES, '
                'becoming total N fixation diagnostic)',
            10: 'LIM_KG_FIX_CYN_DOXY  (AUX: O2 limitation)',
            11: 'LIM_KG_FIX_FIX_CYN_N  (AUX: N limitation for fixing path)',
            12: 'LIM_KG_FIX_FIX_CYN_P  (AUX: P limitation for fixing path)',
            13: 'LIM_KG_NON_FIX_CYN_N  (AUX: N limitation for non-fixing path)',
            14: 'LIM_KG_NON_FIX_CYN_P  (AUX: P limitation for non-fixing path)',
            15: 'LIM_KG_FIX_CYN_LIGHT  (AUX: light limitation)',
            16: '(NH4_N+NO3_N)/14 / (DIP*PO4_P/31)  (AUX: molar N:P ratio)',
            17: 'NH4_N + NO3_N  (AUX: total DIN)',
            18: 'FIX_CYN_LIGHT_SAT  (AUX: light saturation)',
        },
        'derivative_formula': '+1 -2 -3 -4 -5',
        'derivative_slots_used': [1, 2, 3, 4, 5],
        'conditions': 'Entire block conditional on DO_NON_OBLIGATORY_FIXERS; '
                      'Slot 9 is overwritten by NOST block when DO_NOSTOCALES is active. '
                      'Removal limiter may rescale slots 2-5 per node.',
    },

    # =========================================================================
    # 20. INORG_C (DIC)  (var_index = INORG_C_INDEX = 20)
    # =========================================================================
    'INORG_C': {
        'var_index': 20,
        'slots': {
            1:  'TOTAL_DIC_KINETIC_SOURCES / 12000  (total kinetic DIC sources in mol/L/day)',
            2:  'TOTAL_DIC_KINETIC_SINKS / 12000  (total kinetic DIC sinks in mol/L/day)',
            3:  'CO2_ATM_EXHANGE  (CO2 atmospheric exchange in mol/L/day)',
            4:  'R_DIA_TOT_RESP  (AUX: DIA resp rate in mgC/L/day)',
            5:  'R_CYN_TOT_RESP  (AUX: CYN resp rate)',
            6:  'R_FIX_CYN_TOT_RESP  (AUX: FIX_CYN resp rate) [DO_NON_OBLIGATORY_FIXERS]',
            7:  'R_OPA_TOT_RESP  (AUX: OPA resp rate)',
            8:  'R_ZOO_RESP  (AUX: ZOO resp rate, = ZOO_C slot 3)',
            9:  'R_ABIOTIC_DOC_MIN  (AUX: total DOC mineralization = DISS_ORG_C slot 3)',
            10: 'R_DIA_GROWTH  (AUX: DIA growth rate)',
            11: 'R_CYN_GROWTH  (AUX: CYN growth rate)',
            12: 'R_FIX_CYN_GROWTH  (AUX: FIX_CYN growth rate) [DO_NON_OBLIGATORY_FIXERS]',
            13: 'R_OPA_GROWTH  (AUX: OPA growth rate)',
            14: 'R_NOST_VEG_HET_TOT_RESP  (AUX: NOST resp rate) [DO_NOSTOCALES]',
            15: 'R_NOST_VEG_HET_GROWTH  (AUX: NOST growth rate) [DO_NOSTOCALES]',
        },
        'derivative_formula': '+3 +1 -2',
        'derivative_slots_used': [1, 2, 3],
        'conditions': 'Entire block conditional on CONSIDER_INORG_C_DERIVATIVE; '
                      'Slot 3 conditional on CONSIDER_CO2_REARATION; '
                      'TOTAL_DIC_KINETIC_SOURCES = DIA_resp + CYN_resp + OPA_resp + ZOO_resp + '
                      'DOC_min(DOXY+NO3N+MN_IV+FE_III+S_PLUS_6) + methanogenesis + methane_oxidation '
                      '[+ FIX_CYN_resp if DO_NON_OBLIGATORY_FIXERS] [+ NOST_resp if DO_NOSTOCALES]; '
                      'TOTAL_DIC_KINETIC_SINKS = DIA_growth + CYN_growth + OPA_growth '
                      '[+ FIX_CYN_growth if DO_NON_OBLIGATORY_FIXERS] [+ NOST_growth if DO_NOSTOCALES]',
    },

    # =========================================================================
    # 21. TOT_ALK  (var_index = TOT_ALK_INDEX = 21)
    # =========================================================================
    'TOT_ALK': {
        'var_index': 21,
        'slots': {
            1: 'ALK_GAINED_BY_AMMONIUM_GEN  (alk gain from NH4 generation, all sources / 14007)',
            2: 'ALK_GAINED_BY_NITRATE_CONS  (alk gain from NO3 consumption, all sinks / 14007)',
            3: 'ALK_GAINED_BY_PHOSPHATE_CONS  (alk gain from PO4 consumption / 30974)',
            4: 'ALK_LOST_BY_AMMONIUM_CONS  (alk loss from NH4 consumption / 14007)',
            5: 'ALK_LOST_BY_NITRIFICATION  (alk loss: 2*NH4_nitr + 1*NH3_nitr / 14007)',
            6: 'ALK_LOST_BY_PHOSPHATE_GEN  (alk loss from PO4 generation / 30974)',
            7: 'pH  (AUX: current pH value)',
        },
        'derivative_formula': '+1 +2 +3 -4 -5 -6',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6],
        'conditions': 'Entire block conditional on CONSIDER_ALKALNITY_DERIVATIVE',
    },

    # =========================================================================
    # 22. FE_II  (var_index = FE_II_INDEX = 22)
    # =========================================================================
    'FE_II': {
        'var_index': 22,
        'slots': {
            1: 'R_FE_III_REDUCTION  (= 18.66 * R_ABIOTIC_DOC_MIN_FE_III, Fe3+ -> Fe2+ gain)',
            2: 'R_FE_II_OXIDATION  (Fe2+ -> Fe3+ oxidation loss)',
            3: 'FE_II_DISS  (AUX: dissolved Fe2+ concentration)',
            4: 'FE_II_DISS / FE_MOLAR_MASS_MG  (AUX: dissolved Fe2+ in molar)',
        },
        'derivative_formula': '+1 -2',
        'derivative_slots_used': [1, 2],
        'conditions': 'Entire block conditional on ADVANCED_REDOX',
    },

    # =========================================================================
    # 23. FE_III  (var_index = FE_III_INDEX = 23)
    # =========================================================================
    'FE_III': {
        'var_index': 23,
        'slots': {
            1: 'R_FE_II_OXIDATION  (Fe2+ -> Fe3+ oxidation gain)',
            2: 'R_FE_III_REDUCTION  (Fe3+ -> Fe2+ reduction loss)',
            3: 'FE_III_DISS_EQ  (AUX: equilibrium dissolved Fe3+, final)',
            4: 'FE_III_DISS_EQ (early redox block)  (AUX: equilibrium dissolved Fe3+ before clamping, in mg)',
            5: 'FE_III_DISS_EQ (pre-clamp)  (AUX: equilibrium dissolved Fe3+)',
            6: 'FE_III  (AUX: total Fe3+ state)',
            7: 'FE_III_DISS_EQ (post-clamp)  (AUX: equilibrium dissolved Fe3+ after clamping to total)',
            8: 'DISS_FE_III_CONC_TS_AVG  (AUX: time-step average dissolved Fe3+)',
            9: 'DISS_FE_III_CONC_TS_END  (AUX: end-of-timestep dissolved Fe3+)',
        },
        'derivative_formula': '+1 -2',
        'derivative_slots_used': [1, 2],
        'conditions': 'Entire block conditional on ADVANCED_REDOX; '
                      'Slots 4-9 set early in the redox speciation block (not in final derivative section)',
    },

    # =========================================================================
    # 24. MN_II  (var_index = MN_II_INDEX = 24)
    # =========================================================================
    'MN_II': {
        'var_index': 24,
        'slots': {
            1: 'R_MN_IV_REDUCTION  (= 8.66 * R_ABIOTIC_DOC_MIN_MN_IV, Mn4+ -> Mn2+ gain)',
            2: 'R_MN_II_OXIDATION  (Mn2+ -> Mn4+ oxidation loss)',
        },
        'derivative_formula': '+1 -2',
        'derivative_slots_used': [1, 2],
        'conditions': 'Entire block conditional on ADVANCED_REDOX',
    },

    # =========================================================================
    # 25. MN_IV  (var_index = MN_IV_INDEX = 25)
    # =========================================================================
    'MN_IV': {
        'var_index': 25,
        'slots': {
            1: 'R_MN_II_OXIDATION  (Mn2+ -> Mn4+ oxidation gain)',
            2: 'R_MN_IV_REDUCTION  (Mn4+ -> Mn2+ reduction loss)',
        },
        'derivative_formula': '+1 -2',
        'derivative_slots_used': [1, 2],
        'conditions': 'Entire block conditional on ADVANCED_REDOX',
    },

    # =========================================================================
    # 26. CA  (var_index = CA_INDEX = 26)
    # =========================================================================
    'CA': {
        'var_index': 26,
        'slots': {},
        'derivative_formula': '0  (no kinetic processes)',
        'derivative_slots_used': [],
        'conditions': 'ADVANCED_REDOX only; DERIVATIVES = 0',
    },

    # =========================================================================
    # 27. MG  (var_index = MG_INDEX = 27)
    # =========================================================================
    'MG': {
        'var_index': 27,
        'slots': {},
        'derivative_formula': '0  (no kinetic processes)',
        'derivative_slots_used': [],
        'conditions': 'ADVANCED_REDOX only; DERIVATIVES = 0',
    },

    # =========================================================================
    # 28. S_PLUS_6  (var_index = S_PLUS_6_INDEX = 28)
    # =========================================================================
    'S_PLUS_6': {
        'var_index': 28,
        'slots': {
            1: 'R_SULPHIDE_OXIDATION  (= k_OX_H2S * theta^(T-20) * S_MINUS_2 * DOXY_lim, sulphide -> sulphate gain)',
            2: 'R_SULPHATE_REDUCTION  (= 1.33 * R_ABIOTIC_DOC_MIN_S_PLUS_6, sulphate reduction loss)',
        },
        'derivative_formula': '+1 -2',
        'derivative_slots_used': [1, 2],
        'conditions': 'Entire block conditional on ADVANCED_REDOX',
    },

    # =========================================================================
    # 29. S_MINUS_2  (var_index = S_MINUS_2_INDEX = 29)
    # =========================================================================
    'S_MINUS_2': {
        'var_index': 29,
        'slots': {
            1: 'H2S_ATM_EXCHANGE  (= K_A_H2S * (H2S_SAT - H2S*32000), atmospheric H2S exchange)',
            2: 'R_SULPHATE_REDUCTION  (sulphate -> sulphide gain)',
            3: 'R_SULPHIDE_OXIDATION  (sulphide -> sulphate oxidation loss)',
        },
        'derivative_formula': '+1 +2 -3',
        'derivative_slots_used': [1, 2, 3],
        'conditions': 'Entire block conditional on ADVANCED_REDOX',
    },

    # =========================================================================
    # 30. CH4_C  (var_index = CH4_C_INDEX = 30)
    # =========================================================================
    'CH4_C': {
        'var_index': 30,
        'slots': {
            1: 'CH4_ATM_EXCHANGE  (= K_A_CH4 * (CH4_SAT - CH4_C), atmospheric CH4 exchange)',
            2: 'R_METHANOGENESIS  (= 0.5 * R_ABIOTIC_DOC_MIN_DOC, methanogenesis CH4 production)',
            3: 'R_METHANE_OXIDATION  (= k_OX_CH4 * theta^(T-20) * CH4_C * DOXY_lim, methane oxidation loss)',
        },
        'derivative_formula': '+1 +2 -3',
        'derivative_slots_used': [1, 2, 3],
        'conditions': 'Entire block conditional on ADVANCED_REDOX',
    },

    # =========================================================================
    # 31. NOST_VEG_HET_C  (var_index = NOST_VEG_HET_C_INDEX = 31)
    # =========================================================================
    'NOST_VEG_HET_C': {
        'var_index': 31,
        'slots': {
            1:  'R_NOST_VEG_HET_GROWTH  (NOST total growth = fix + non-fix)',
            2:  'R_NOST_VEG_HET_TOT_RESP  (NOST total respiration)',
            3:  'R_NOST_VEG_HET_EXCR  (NOST excretion)',
            4:  'R_NOST_VEG_HET_DEATH  (NOST mortality)',
            5:  'R_ZOO_FEEDING_NOST_VEG_HET  (ZOO grazing on NOST)',
            6:  'R_GERM_NOST_AKI  (akinete germination -> vegetative gain)',
            7:  'R_FORM_NOST_AKI  (akinete formation loss)',
            8:  'R_DENS_MORT_NOST_VEG_HET  (density-dependent mortality)',
            9:  'R_NOST_VEG_HET_FIX_GROWTH * NOST_N_TO_C * (1/FRAC_FIX_N_FOR_GR_VEG_HET)  (AUX: NOST N fixation rate)',
            10: 'NOST_LIGHT_SAT  (AUX: light saturation)',
            11: 'LIM_KG_NOST_VEG_HET_LIGHT  (AUX: light limitation)',
            12: 'LIM_KG_NOST_VEG_HET_TEMP  (AUX: temperature limitation)',
            13: 'LIM_KG_NOST_VEG_HET_DOXY  (AUX: oxygen limitation)',
            14: 'LIM_KG_NOST_VEG_HET_P  (AUX: phosphorus limitation)',
            15: 'R_NOST_VEG_HET_GROWTH + R_CYN_GROWTH + R_OPA_GROWTH + R_DIA_GROWTH  (AUX: total phyto growth)',
            16: 'LIM_KG_NOST_VEG_HET_N  (AUX: nitrogen limitation)',
        },
        'derivative_formula': '+1 -2 -3 -4 -5 +6 -7 -8',
        'derivative_slots_used': [1, 2, 3, 4, 5, 6, 7, 8],
        'conditions': 'Entire block conditional on DO_NOSTOCALES; '
                      'Slot 9 also adds to FIX_CYN_C slot 9 (total N fixation diagnostic)',
    },

    # =========================================================================
    # 32. AKI_C (NOST_AKI_C)  (var_index = NOST_AKI_C_INDEX = 32)
    # =========================================================================
    'AKI_C': {
        'var_index': 32,
        'slots': {
            1: 'R_FORM_NOST_AKI  (akinete formation gain from vegetative)',
            2: 'R_GERM_NOST_AKI  (akinete germination loss -> vegetative)',
            3: 'R_LOSS_AKI  (akinete loss)',
            4: 'R_MORT_AKI  (akinete mortality)',
            5: 'DEPTH  (AUX: water depth)',
        },
        'derivative_formula': '+1 -2 -3 -4',
        'derivative_slots_used': [1, 2, 3, 4],
        'conditions': 'Entire block conditional on DO_NOSTOCALES',
    },
}

# fmt: on


# ─────────────────────────────────────────────────────────────────────────────
# Convenience: derivative sign map for each variable
# Keys: variable name. Values: dict of {slot: +1 or -1}
# ─────────────────────────────────────────────────────────────────────────────
DERIVATIVE_SIGNS = {}
for var_name, info in SLOT_MAP.items():
    signs = {}
    formula = info['derivative_formula']
    # Parse simple "+N -N" tokens from the formula string
    for token in formula.replace(',', ' ').split():
        token = token.strip()
        if token.startswith('+') and token[1:].isdigit():
            signs[int(token[1:])] = +1
        elif token.startswith('-') and token[1:].isdigit():
            signs[int(token[1:])] = -1
    DERIVATIVE_SIGNS[var_name] = signs


# ─────────────────────────────────────────────────────────────────────────────
# Summary printer
# ─────────────────────────────────────────────────────────────────────────────
def print_slot_map():
    """Print the complete slot map in a readable format."""
    for var_name, info in SLOT_MAP.items():
        print(f"\n{'='*78}")
        print(f"  {var_name}  (var_index = {info['var_index']})")
        print(f"{'='*78}")
        if info['slots']:
            for slot, desc in sorted(info['slots'].items()):
                is_aux = '(AUX' in desc
                sign_map = DERIVATIVE_SIGNS.get(var_name, {})
                sign = sign_map.get(slot, None)
                sign_str = {1: ' [+]', -1: ' [-]', None: '     '}[sign]
                marker = ' AUX' if is_aux else sign_str
                print(f"    slot {slot:2d}{marker}:  {desc}")
        else:
            print(f"    (no process rate slots)")
        print(f"  DERIVATIVE: {info['derivative_formula']}")
        if info['conditions'] and info['conditions'] != 'None':
            print(f"  CONDITIONS: {info['conditions']}")


if __name__ == '__main__':
    print_slot_map()
