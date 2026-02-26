#!/usr/bin/env python3
"""
Generate a PDF analysis document comparing the AQUABC and ESTAS reference
manuals against the Ertürk et al. (2023) paper and the actual Fortran code.
"""

from __future__ import annotations

from pathlib import Path
from fpdf import FPDF

OUT_DIR = Path(__file__).resolve().parent.parent / "docs"


# ── Severity helpers ─────────────────────────────────────────────────────────
SEV_CRIT = "CRITICAL"
SEV_MOD  = "MODERATE"
SEV_LOW  = "LOW"
SEV_INFO = "INFO"


# ── Finding dataclass ────────────────────────────────────────────────────────
class Finding:
    def __init__(self, fid: str, title: str, severity: str,
                 manual: str, paper: str, code: str, analysis: str,
                 recommendation: str):
        self.fid = fid
        self.title = title
        self.severity = severity
        self.manual = manual
        self.paper = paper
        self.code = code
        self.analysis = analysis
        self.recommendation = recommendation


# ── All findings ─────────────────────────────────────────────────────────────
FINDINGS: list[Finding] = [
    # ── F-01 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-01",
        "State-Variable Index Ordering Mismatch",
        SEV_CRIT,
        manual=(
            "AQUABC Reference Manual Table (Section 2.1) lists: "
            "index 5=DIA-C, 6=CYN-C, 7=OPA-C, 8=FIX-CYN-C, 9=ZOO-C, "
            "10=DET-C, ... 20=Mn(II), 21=Mn(IV), 22=Fe(II), 23=Fe(III), "
            "24=S(-II), 25=S(VI), 26=CH4, 27-28=NOST, 29-32=Allelopathy."
        ),
        paper=(
            "Paper Table 1 lists 21 variables with a completely different "
            "numbering: 1=NH4, 2=NO3, 3=PO4, 4=DSi, 5=DO, 6=Dia-C, "
            "7=Cyn-NOFIX, 8=Cyn-FIX, 9=OPA-C, 10=Zoo-C, ..."
        ),
        code=(
            "aquabc_II_pelagic_svindex.f90 defines: 1=NH4, 2=NO3, 3=PO4, "
            "4=DOXY, 5=DIA-C, 6=ZOO-C, 7=ZOO-N, 8=ZOO-P, "
            "9=DET_POC, 10=DET_PON, 11=DET_POP, 12=DOC, 13=DON, "
            "14=DOP, 15=CYN-C, 16=OPA-C, 17=DSi, 18=PSi, "
            "19=FIX_CYN-C, 20=DIC, 21=ALK, 22=FE_II, 23=FE_III, "
            "24=MN_II, 25=MN_IV, 26=CA, 27=MG, 28=S(+6), "
            "29=S(-2), 30=CH4-C, 31=NOST_VEG_HET, 32=NOST_AKI."
        ),
        analysis=(
            "Neither the manual NOR the paper match the actual code ordering. "
            "The manual claims index 6 = CYN-C but the code has ZOO-C at "
            "index 6. The paper claims index 6 = Dia-C but the code has "
            "Dia-C at index 5. Zooplankton C/N/P occupy indices 6-8 in the "
            "code, not 9-11 as either document suggests. CYN-C is at index "
            "15, OPA-C at 16, FIX_CYN-C at 19 -- none matching the "
            "manual's or paper's listing. Furthermore, the manual claims "
            "indices 29-32 are allelopathic metabolites; the code has "
            "S(-II), CH4, and Nostocales stages at 29-32, with allelopathy "
            "at 33-36."
        ),
        recommendation=(
            "The AQUABC Reference Manual state variable table must be "
            "completely rewritten to match the actual index assignments in "
            "aquabc_II_pelagic_svindex.f90. The paper's Table 1 uses a "
            "logical (not code) numbering which should be noted."
        ),
    ),
    # ── F-02 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-02",
        "Allelopathy Variable Indices Wrong in AQUABC Manual",
        SEV_CRIT,
        manual=(
            "AQUABC manual lists indices 29-32 as 'SEC-METAB 1-4' "
            "(allelopathic secondary metabolites)."
        ),
        paper="Paper does not mention allelopathy.",
        code=(
            "mod_GLOBAL.f90: nstate=32, NUM_ALLOLOPATHY_STATE_VARS=4. "
            "Allelopathy variables are indices 33-36, appended AFTER "
            "the 32 standard state variables. Indices 29-32 in the code "
            "are S(-II), CH4-C, NOST_VEG_HET, NOST_AKI."
        ),
        analysis=(
            "The manual incorrectly maps allelopathy metabolites to "
            "indices 29-32, which actually hold S(-II), CH4, and "
            "Nostocales life-cycle stages. The total transported variables "
            "are 36 (32 standard + 4 allelopathy), not 32."
        ),
        recommendation=(
            "Correct the manual table. Clearly state that the 32 standard "
            "state variables are followed by 4 allelopathic metabolites "
            "(indices 33-36) for a total of 36 transported variables."
        ),
    ),
    # ── F-03 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-03",
        "Fortran Standard: Paper Says 2003, Manuals Say 90/95",
        SEV_MOD,
        manual="ESTAS manual: 'Fortran 90/95'.",
        paper="Paper Section 2.1: 'Fortran 2003 was used'.",
        code=(
            "No -std= flag in Makefile. Zero F2003/F2008 features found "
            "(no class(), abstract, type-extends, move_alloc, select type, "
            "or iso_fortran_env). All constructs are Fortran 90/95 "
            "compatible: module/use, type definitions, allocatable, where, "
            "SELECTED_REAL_KIND."
        ),
        analysis=(
            "The paper's claim of Fortran 2003 is incorrect. The code uses "
            "exclusively Fortran 90/95 features. The manuals are accurate."
        ),
        recommendation=(
            "Note this as a minor erratum in the paper. Both manuals "
            "already have the correct designation (Fortran 90/95)."
        ),
    ),
    # ── F-04 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-04",
        "RK2 Solver Described as 'Future' -- Actually Implemented",
        SEV_MOD,
        manual=(
            "ESTAS manual Section 6: 'The infrastructure supports future "
            "addition of higher-order methods (e.g. Runge-Kutta 2)'."
        ),
        paper="Paper does not discuss numerical solver.",
        code=(
            "mod_SOLVER.f90: PELAGIC_SOLVER_NO == 2 triggers a FULLY "
            "IMPLEMENTED Heun's method (RK2) at lines 280-410+, with "
            "two-stage derivative calculation, averaged update, negative "
            "mass handling, and concentration clamping. Default is still "
            "Forward Euler (PELAGIC_SOLVER_NO = 1)."
        ),
        analysis=(
            "RK2 is not 'future' -- it is fully implemented and selectable. "
            "The manual understates the current capability."
        ),
        recommendation=(
            "Update the ESTAS manual to document RK2 as an available solver "
            "option, with guidance on when to use it (e.g. stiff problems, "
            "larger time steps)."
        ),
    ),
    # ── F-05 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-05",
        "Constant Category Ranges Incomplete in AQUABC Manual",
        SEV_MOD,
        manual=(
            "AQUABC manual lists 15 categories, last being "
            "'Nostocales (276-298)'. Constants 299-323 are not "
            "accounted for in the category listing."
        ),
        paper="Paper says 183 calibrated constants (Supplementary B).",
        code=(
            "MODEL_CONSTANTS(1:318) array. Constants 299-306 are "
            "additional Nostocales parameters (akinete T/N thresholds). "
            "307-309: POM dissolution saturation. 310-315: DON/DOP "
            "availability fractions. 316-318: phytoplankton-mediated "
            "mineralisation caps. 319-323: BETA photoinhibition "
            "(loaded via para_get_value, not the array)."
        ),
        analysis=(
            "The manual misses constants 299-323 (25 constants). "
            "The manual's category ranges also have overlaps and gaps "
            "(e.g. 'General 1-5' but category 2 starts at 5; "
            "'POM dissolution 134-145' vs constant reference 134-146, "
            "etc.). The paper's '183' refers to calibrated subset only."
        ),
        recommendation=(
            "Rewrite the constant category section to cover all 323 "
            "constants with accurate, non-overlapping ranges. Add the "
            "missing categories: POM dissolution saturation (307-309), "
            "DOM availability (310-315), phytoplankton mineralisation "
            "caps (316-318), and photoinhibition BETA (319-323)."
        ),
    ),
    # ── F-06 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-06",
        "Sediment Constant Count: Manual Says 170, Code Says 171",
        SEV_LOW,
        manual="AQUABC manual: '170 parameters'.",
        paper="Paper does not specify sediment constant count.",
        code="mod_GLOBAL.f90 line 40: NUM_SED_CONSTS = 171.",
        analysis="Off by one error in the manual.",
        recommendation="Correct the manual to state 171 sediment constants.",
    ),
    # ── F-07 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-07",
        "mod_GLOBAL.f90 Line Count: Manual Says 286, Actual Is 285",
        SEV_LOW,
        manual="ESTAS manual source file table: 'mod_GLOBAL.f90 | 286'.",
        paper="N/A.",
        code="wc -l returns 285 lines.",
        analysis="Off by one. Trivial but reflects stale documentation.",
        recommendation="Update line count in the ESTAS manual or remove exact line counts (they become stale quickly).",
    ),
    # ── F-08 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-08",
        "Paper State Variable Count (21) vs Code (32)",
        SEV_INFO,
        manual="AQUABC manual correctly states 32 state variables.",
        paper=(
            "Paper Table 1 lists 21 state variables. Does not mention "
            "Fe(II/III), Mn(II/IV), Ca, Mg, SO4, H2S, CH4, or "
            "Nostocales life-cycle stages."
        ),
        code="nstate = 32 in mod_GLOBAL.f90.",
        analysis=(
            "The paper describes a simplified configuration with only 21 "
            "active state variables, used for the Curonian Lagoon case "
            "study. The additional 11 variables (Fe, Mn, Ca, Mg, S, CH4, "
            "Nostocales stages) exist in the code but were deactivated "
            "via flags (ADVANCED_REDOX_OPTION, DO_NOSTOCALES, etc.)."
        ),
        recommendation=(
            "This is expected and documented in the paper as a design "
            "choice. The AQUABC manual correctly lists all 32 (once the "
            "index ordering is fixed per F-01)."
        ),
    ),
    # ── F-09 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-09",
        "Synthesizing Unit vs Liebig Minimum: Nuanced Reality",
        SEV_MOD,
        manual=(
            "AQUABC manual: SU 'replaces Liebig's minimum' for all "
            "phytoplankton groups."
        ),
        paper="Paper Supplementary A implies Monod-type nutrient limitation.",
        code=(
            "SU is used for N-P colimitation in all groups' non-fixing "
            "fraction. However: (1) Fixing fraction of N-fixing "
            "cyanobacteria retains Liebig min (because N-term is an "
            "inhibition switch, not colimitation); (2) SU of N-P is then "
            "cascaded with Si via SU for diatoms; (3) final growth "
            "limitation uses Liebig min(nutrient_lim, oxygen_lim); "
            "(4) Nostocales fixing fraction uses P-only limitation."
        ),
        analysis=(
            "The manual's claim that SU 'replaces' Liebig is an "
            "oversimplification. SU replaces Liebig for N-P nutrient "
            "colimitation specifically, but Liebig min() is still used "
            "elsewhere (fixing fraction, oxygen-nutrient combination). "
            "There is no user-configurable switch between SU and Liebig."
        ),
        recommendation=(
            "Revise the manual to explain that SU is used for N-P "
            "colimitation in the non-fixing pathway, while Liebig min "
            "is retained for the fixing fraction and for combining "
            "nutrient limitation with other factors (oxygen, light)."
        ),
    ),
    # ── F-10 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-10",
        "Macroalgae Model: In Manual, Not in Paper",
        SEV_INFO,
        manual=(
            "AQUABC manual describes Macroalgae model with 6 state "
            "variables (Droop quota). ESTAS manual references "
            "mod_MACROALGAE.f90."
        ),
        paper=(
            "Paper does not mention macroalgae at all. Table 2 notes "
            "some other models include 'Macroalgae or submerged aquatic "
            "vegetation' but AQUABC's entry shows 'No'."
        ),
        code=(
            "mod_MACROALGAE.f90 exists (372 lines) with 6 state variables "
            "and functional Droop quota kinetics."
        ),
        analysis=(
            "The macroalgae module was developed after/alongside the paper "
            "and was not active in the Curonian Lagoon application. "
            "Curiously, the paper's Table 2 marks AQUABC as 'No' for "
            "macroalgae, which was accurate at the time of writing."
        ),
        recommendation=(
            "This is an extension. The manuals correctly document it. "
            "No action needed, but the manuals could note that macroalgae "
            "was not used in the published case study."
        ),
    ),
    # ── F-11 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-11",
        "Transport Equation Terminology Differences",
        SEV_LOW,
        manual=(
            "ESTAS manual: 7 components -- advection, dispersion, "
            "settling, mass loads, mass withdrawals, kinetics, "
            "prescribed sediment fluxes."
        ),
        paper=(
            "Paper Eq. 1: inflow from neighbours, outflow to neighbours, "
            "diffusion, settling from overlaying box, settling out of box, "
            "boundary forcing, kinetics."
        ),
        code=(
            "mod_SOLVER.f90: tot_deriv sums 7 arrays -- "
            "ECOL_ADVECTION_DERIVS, ECOL_DISPERSION_DERIVS, "
            "ECOL_SETTLING_DERIVS, ECOL_MASS_LOAD_DERIVS, "
            "ECOL_MASS_WITHDRAWAL_DERIVS, ECOL_KINETIC_DERIVS, "
            "ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS."
        ),
        analysis=(
            "The ESTAS manual matches the code exactly. The paper's "
            "formulation combines mass loads and withdrawals into "
            "'boundary forcing' and splits settling into receiving/losing "
            "terms. Conceptually equivalent, but the paper omits "
            "prescribed sediment fluxes as a separate term."
        ),
        recommendation=(
            "Add a note to the ESTAS manual mapping between the paper's "
            "Eq. 1 terminology and the code's derivative array names."
        ),
    ),
    # ── F-12 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-12",
        "Paper Constant Count (183) vs Code (323+)",
        SEV_INFO,
        manual="AQUABC manual: '318 parameters' (actually 323 with BETAs).",
        paper="Paper: '183 constants' in Supplementary Material B.",
        code=(
            "nconst=318 array + 5 BETA named params = 323 in WCONST_04.txt. "
            "Plus EXTRA_WCONST.txt (allelopathy, ~30 more) and sediment "
            "constants (171)."
        ),
        analysis=(
            "The paper's 183 refers to the subset of constants that were "
            "actively calibrated for the Curonian Lagoon case study. "
            "The manuals correctly document the full model constant set. "
            "The 140 additional constants cover extended modules (redox, "
            "Nostocales, multi-EA anaerobic pathways, photoinhibition)."
        ),
        recommendation=(
            "Both manuals should clarify the distinction between "
            "'calibrated constants' (paper's 183) and 'total model "
            "constants' (323+ pelagic, 171 sediment, allelopathy extras)."
        ),
    ),
    # ── F-13 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-13",
        "Zooplankton Switching Power Hardcoded but Undocumented",
        SEV_LOW,
        manual=(
            "AQUABC manual describes Active Switching Model "
            "(Gentleman et al. 2003) but does not mention the switching "
            "power exponent value."
        ),
        paper="Paper does not detail zooplankton feeding formulation.",
        code=(
            "aquabc_II_pelagic_lib_ZOOPLANKTON.f90 line 194: "
            "switching power = 1.5 (hardcoded, not configurable via WCONST)."
        ),
        analysis=(
            "The switching power is an important model characteristic that "
            "affects food-web dynamics. It is hardcoded at 1.5 and cannot "
            "be changed without modifying source code."
        ),
        recommendation=(
            "Document the switching power value (1.5) in the AQUABC manual "
            "zooplankton section and note that it is hardcoded."
        ),
    ),
    # ── F-14 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-14",
        "AQUABC Manual Missing CTMI Parameter Name Clarification",
        SEV_MOD,
        manual=(
            "AQUABC manual presents CTMI formula with T_min, T_opt, T_max "
            "but does not map these to the actual WCONST parameter names "
            "(OPT_TEMP_LR, OPT_TEMP_UR, KAPPA_OVER_OPT_TEMP)."
        ),
        paper="Paper does not explain parameter naming.",
        code=(
            "aquabc_II_pelagic_auxillary.f90 comments document: "
            "Lower_TEMP = T_min, Upper_TEMP = T_opt, "
            "KAPPA_OVER_OPT_TEMP = T_max, KAPPA_UNDER_OPT_TEMP = unused."
        ),
        analysis=(
            "The counter-intuitive naming (OPT_TEMP_LR sounds like 'lower "
            "optimal range' but is T_min; OPT_TEMP_UR sounds like 'upper "
            "optimal range' but is T_opt) is a persistent source of "
            "confusion. The manual should explicitly map parameter file "
            "names to their CTMI roles."
        ),
        recommendation=(
            "Add a mapping table to the AQUABC manual CTMI section: "
            "OPT_TEMP_LR -> T_min, OPT_TEMP_UR -> T_opt, "
            "KAPPA_OVER_OPT_TEMP -> T_max, KAPPA_UNDER_OPT_TEMP -> unused."
        ),
    ),
    # ── F-15 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-15",
        "ESTAS Manual Missing Repeat-Cycle Spin-Up Details",
        SEV_LOW,
        manual=(
            "ESTAS manual mentions 'repeat cycles' for spin-up but "
            "gives no detail on how state is carried between cycles."
        ),
        paper=(
            "Paper does not mention spin-up procedure."
        ),
        code=(
            "mod_SIMULATE.f90: the outer DO loop over NUM_REPEATS "
            "resets time to the simulation start but carries forward "
            "the final state variables as initial conditions for the "
            "next cycle. All output files continue appending."
        ),
        analysis=(
            "The spin-up mechanism is straightforward (state carried "
            "forward, time reset), but this is important operational "
            "information for users."
        ),
        recommendation=(
            "Document the spin-up procedure in the ESTAS manual: "
            "state variables are preserved across repeat cycles, "
            "time is reset to start, output continues appending."
        ),
    ),
    # ── F-16 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-16",
        "Hypoxia Three-Regime System Underdocumented",
        SEV_MOD,
        manual=(
            "AQUABC manual describes 'mortality enhanced under hypoxia' "
            "but does not detail the three-regime system."
        ),
        paper="Paper mentions 'hypoxia stress on organisms' generically.",
        code=(
            "All phytoplankton libraries implement three regimes: "
            "(1) DO > threshold: FAC_HYPOX = 1.0; "
            "(2) 0.1 < DO/threshold <= 1.0: FAC_HYPOX = "
            "THETA^(EXPON*(DO_thr - DO)); "
            "(3) DO/threshold <= 0.1: crash regime with capped mortality, "
            "growth and respiration zeroed."
        ),
        analysis=(
            "The crash regime (regime 3) is a critical numerical safeguard "
            "that prevents mass-balance overshoot under near-anoxia. "
            "Neither the paper nor the manuals document it."
        ),
        recommendation=(
            "Add full three-regime hypoxia documentation to the AQUABC "
            "manual with equations for each regime."
        ),
    ),
    # ── F-17 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-17",
        "Nostocales Naming Inconsistency in Code",
        SEV_LOW,
        manual=(
            "AQUABC manual uses NOST-VEG, NOST-HET notation."
        ),
        paper="Paper does not mention Nostocales life cycle.",
        code=(
            "Two different naming conventions coexist: "
            "FIX_CYN_HET_C_INDEX / FIX_CYN_AK_C_INDEX (old convention) "
            "and NOST_VEG_HET_C_INDEX / NOST_AKI_C_INDEX (new convention). "
            "Both refer to the same state variables (indices 31-32)."
        ),
        analysis=(
            "The dual naming is confusing but functionally harmless. "
            "The manuals use the newer NOST* convention, which is clearer."
        ),
        recommendation=(
            "Deprecate the FIX_CYN_HET_C / FIX_CYN_AK_C names in favor "
            "of NOST_VEG_HET_C / NOST_AKI_C. Add deprecation comment."
        ),
    ),
    # ── F-18 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-18",
        "ESTAS Manual Correctly Documents 7 Derivative Components",
        SEV_INFO,
        manual=(
            "7 components: advection, dispersion, settling, loads, "
            "withdrawals, kinetics, sediment fluxes."
        ),
        paper="Paper Eq. 1: conceptually similar but different grouping.",
        code="mod_SOLVER.f90 sums exactly these 7 derivative arrays.",
        analysis=(
            "The ESTAS manual is more accurate than the paper for this. "
            "The code implementation matches the manual exactly."
        ),
        recommendation="No action needed. Manual is correct.",
    ),
    # ── F-19 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-19",
        "CO2SYS K1K2 Constants Selection Undocumented",
        SEV_LOW,
        manual=(
            "AQUABC manual references CO2SYS CDIAC implementation "
            "but does not specify which K1K2 constant set is used."
        ),
        paper="Paper cites Lewis & Wallace (1998) and van Heuven et al. (2011).",
        code=(
            "aquabc_II_pelagic_model.f90: K1K2CONSTANTS = 4 "
            "(Mehrbach refit by Dickson & Millero 1987)."
        ),
        analysis=(
            "The choice of K1K2 constants affects pH calculation accuracy. "
            "Mehrbach/Dickson-Millero (option 4) is the standard choice "
            "for estuarine/coastal applications."
        ),
        recommendation=(
            "Document the K1K2 constant selection (option 4 = Mehrbach/"
            "Dickson-Millero) in the AQUABC manual CO2SYS section."
        ),
    ),
    # ── F-20 ─────────────────────────────────────────────────────────────────
    Finding(
        "F-20",
        "Eco-Exergy Output Disabled by Default -- Manual Doesn't Note This",
        SEV_LOW,
        manual=(
            "ESTAS manual describes eco-exergy diagnostics but does not "
            "mention that output is disabled by default."
        ),
        paper="Paper does not mention exergy.",
        code=(
            "mod_SIMULATE.f90: WRITE_PELAGIC_EXERGY_OUTPUT = 0 (disabled). "
            "mod_PELAGIC_EXERGY.f90 exists and is functional (123 lines)."
        ),
        analysis=(
            "Users would expect exergy output based on the manual but "
            "would not get it without changing the source code flag."
        ),
        recommendation=(
            "Document how to enable exergy output in the ESTAS manual."
        ),
    ),
]


# ── PDF generator ────────────────────────────────────────────────────────────
class AnalysisPDF(FPDF):
    def __init__(self):
        super().__init__(orientation="P", unit="mm", format="A4")
        self.set_auto_page_break(auto=True, margin=18)
        self.alias_nb_pages()

    def header(self):
        self.set_font("Helvetica", "B", 8)
        self.set_text_color(100, 100, 100)
        self.cell(0, 5,
                  "ESTAS-AQUABC  |  Reference Manuals vs. Paper vs. Code  |  Analysis Report",
                  align="C")
        self.ln(7)
        self.set_draw_color(180, 180, 180)
        self.line(self.l_margin, self.get_y(),
                  self.w - self.r_margin, self.get_y())
        self.ln(2)

    def footer(self):
        self.set_y(-14)
        self.set_font("Helvetica", "I", 7)
        self.set_text_color(140, 140, 140)
        self.cell(0, 8, f"Page {self.page_no()}/{{nb}}", align="C")

    # ── severity colour ──────────────────────────────────────────────────
    def _sev_color(self, sev: str):
        if sev == SEV_CRIT:
            self.set_fill_color(220, 50, 50)
            self.set_text_color(255, 255, 255)
        elif sev == SEV_MOD:
            self.set_fill_color(230, 160, 30)
            self.set_text_color(0, 0, 0)
        elif sev == SEV_LOW:
            self.set_fill_color(60, 140, 200)
            self.set_text_color(255, 255, 255)
        else:
            self.set_fill_color(120, 180, 120)
            self.set_text_color(255, 255, 255)

    # ── section helpers ──────────────────────────────────────────────────
    def big_title(self, text: str):
        self.set_font("Helvetica", "B", 20)
        self.set_text_color(30, 50, 80)
        self.multi_cell(0, 10, text, align="C")
        self.set_text_color(0, 0, 0)
        self.ln(3)

    def subtitle(self, text: str):
        self.set_font("Helvetica", "", 11)
        self.set_text_color(80, 80, 80)
        self.multi_cell(0, 5.5, text, align="C")
        self.set_text_color(0, 0, 0)
        self.ln(2)

    def section(self, text: str):
        self.set_font("Helvetica", "B", 13)
        self.set_fill_color(30, 50, 80)
        self.set_text_color(255, 255, 255)
        self.cell(0, 8, f"  {text}",
                  new_x="LMARGIN", new_y="NEXT", fill=True)
        self.set_text_color(0, 0, 0)
        self.ln(3)

    def para(self, text: str, size: float = 9):
        self.set_font("Helvetica", "", size)
        self.set_text_color(30, 30, 30)
        self.multi_cell(0, 4.5, text)
        self.set_text_color(0, 0, 0)
        self.ln(2)

    def labeled(self, label: str, text: str, size: float = 8):
        self.set_font("Helvetica", "B", size)
        self.set_text_color(30, 50, 80)
        self.cell(28, 4, f"{label}:", new_x="END")
        self.set_font("Helvetica", "", size)
        self.set_text_color(40, 40, 40)
        x0 = self.get_x()
        w = self.w - self.r_margin - x0
        self.multi_cell(w, 3.8, text)
        self.set_text_color(0, 0, 0)
        self.ln(1)

    # ── finding block ────────────────────────────────────────────────────
    def finding(self, f: Finding):
        # check page space
        if self.get_y() > self.h - 70:
            self.add_page()

        # badge
        self._sev_color(f.severity)
        self.set_font("Helvetica", "B", 9)
        self.cell(18, 5.5, f" {f.fid} ", fill=True)
        self.cell(2, 5.5, "")
        self.cell(24, 5.5, f" {f.severity} ", fill=True)
        self.set_text_color(0, 0, 0)
        self.set_fill_color(255, 255, 255)
        self.cell(2, 5.5, "")
        self.set_font("Helvetica", "B", 10)
        self.cell(0, 5.5, f.title, new_x="LMARGIN", new_y="NEXT")
        self.ln(2)

        self.labeled("Manual says", f.manual)
        self.labeled("Paper says", f.paper)
        self.labeled("Code shows", f.code)
        self.ln(1)

        self.set_font("Helvetica", "B", 8)
        self.set_text_color(30, 50, 80)
        self.cell(0, 4, "Analysis:", new_x="LMARGIN", new_y="NEXT")
        self.set_font("Helvetica", "", 8)
        self.set_text_color(40, 40, 40)
        self.set_fill_color(248, 248, 240)
        self.multi_cell(0, 3.8, f.analysis, fill=True)
        self.ln(1)

        self.set_font("Helvetica", "B", 8)
        self.set_text_color(30, 50, 80)
        self.cell(0, 4, "Recommendation:", new_x="LMARGIN", new_y="NEXT")
        self.set_font("Helvetica", "", 8)
        self.set_text_color(40, 40, 40)
        self.set_fill_color(240, 248, 240)
        self.multi_cell(0, 3.8, f.recommendation, fill=True)
        self.set_text_color(0, 0, 0)
        self.ln(4)

    # ── summary row ──────────────────────────────────────────────────────
    def summary_row(self, fid: str, sev: str, title: str, alt: bool):
        if self.get_y() > self.h - 12:
            self.add_page()
            self._summary_header()
        if alt:
            self.set_fill_color(245, 245, 250)
        else:
            self.set_fill_color(255, 255, 255)

        self._sev_color(sev)
        self.set_font("Helvetica", "B", 7)
        self.cell(12, 5, fid, border="LTB", fill=True, align="C")
        self.cell(22, 5, sev, border="TB", fill=True, align="C")

        self.set_text_color(0, 0, 0)
        if alt:
            self.set_fill_color(245, 245, 250)
        else:
            self.set_fill_color(255, 255, 255)
        self.set_font("Helvetica", "", 7.5)
        self.cell(0, 5, f"  {title}", border="TBR",
                  fill=True, new_x="LMARGIN", new_y="NEXT")

    def _summary_header(self):
        self.set_font("Helvetica", "B", 7.5)
        self.set_fill_color(220, 220, 220)
        self.set_text_color(0, 0, 0)
        self.cell(12, 5, "ID", border=1, fill=True, align="C")
        self.cell(22, 5, "Severity", border=1, fill=True, align="C")
        self.cell(0, 5, "  Title", border=1, fill=True,
                  new_x="LMARGIN", new_y="NEXT")


def build_pdf() -> Path:
    pdf = AnalysisPDF()
    pdf.add_page()

    # ── Title ────────────────────────────────────────────────────────────
    pdf.ln(12)
    pdf.big_title("Reference Manual Cross-Validation")
    pdf.big_title("Analysis Report")
    pdf.ln(3)
    pdf.subtitle(
        "AQUABC_Reference_Manual.md  &  ESTAS_Reference_Manual.md\n"
        "vs.  Erturk et al. (2023) Ecological Modelling 486, 110509\n"
        "vs.  Actual Fortran Source Code"
    )
    pdf.ln(6)
    pdf.set_font("Helvetica", "", 9)
    pdf.set_fill_color(240, 244, 248)
    summary_text = (
        "This document presents a systematic cross-validation of the two "
        "ESTAS-AQUABC reference manuals against the published paper by "
        "Erturk et al. (2023) and the actual Fortran source code. "
        "Each finding compares what the manual says, what the paper says, "
        "and what the code actually does.\n\n"
        "20 findings were identified across four severity levels:\n"
        "  - CRITICAL (2): Errors that could mislead users or cause "
        "integration failures.\n"
        "  - MODERATE (5): Inaccuracies or significant omissions.\n"
        "  - LOW (7): Minor discrepancies or stale information.\n"
        "  - INFO (6): Expected differences or confirmations.\n\n"
        "Documents analysed:\n"
        "  [1] AQUABC_Reference_Manual.md (410 lines) - Ecological "
        "model documentation\n"
        "  [2] ESTAS_Reference_Manual.md (525 lines) - Transport "
        "framework documentation\n"
        "  [3] Erturk et al. (2023) Ecological Modelling 486, 110509 - "
        "Primary model description paper\n"
        "  [4] SOURCE_CODE/ - All Fortran source files (ground truth)\n\n"
        "Date: February 2026"
    )
    pdf.multi_cell(0, 4.5, summary_text, fill=True)
    pdf.ln(6)

    # ── Summary table ────────────────────────────────────────────────────
    pdf.section("Executive Summary")
    crits = [f for f in FINDINGS if f.severity == SEV_CRIT]
    mods = [f for f in FINDINGS if f.severity == SEV_MOD]
    lows = [f for f in FINDINGS if f.severity == SEV_LOW]
    infos = [f for f in FINDINGS if f.severity == SEV_INFO]
    pdf.para(
        f"Total findings: {len(FINDINGS)}  |  "
        f"Critical: {len(crits)}  |  Moderate: {len(mods)}  |  "
        f"Low: {len(lows)}  |  Info: {len(infos)}"
    )
    pdf._summary_header()
    for i, f in enumerate(FINDINGS):
        pdf.summary_row(f.fid, f.severity, f.title, i % 2 == 1)
    pdf.ln(6)

    # ── Key statistics ───────────────────────────────────────────────────
    pdf.section("Key Numerical Comparisons")
    stats = [
        ("State variables", "32 (manual)", "21 (paper)", "32 (code)", "Manual correct"),
        ("Pelagic constants", "318 (manual)", "183 (paper)", "323 (code)", "All different"),
        ("Sediment constants", "170 (manual)", "N/A (paper)", "171 (code)", "Off by one"),
        ("Transported vars", "32 (manual)", "21 (paper)", "36 (code)", "Manual wrong"),
        ("Fortran standard", "90/95 (manual)", "2003 (paper)", "90/95 (code)", "Manual correct"),
        ("Solver options", "Euler only (manual)", "N/A (paper)", "Euler + RK2 (code)", "Manual understates"),
    ]
    pdf.set_font("Helvetica", "B", 7.5)
    pdf.set_fill_color(220, 220, 220)
    pdf.cell(35, 5, "Aspect", border=1, fill=True)
    pdf.cell(30, 5, "Manual", border=1, fill=True, align="C")
    pdf.cell(30, 5, "Paper", border=1, fill=True, align="C")
    pdf.cell(30, 5, "Code", border=1, fill=True, align="C")
    pdf.cell(0, 5, "Verdict", border=1, fill=True, align="C",
             new_x="LMARGIN", new_y="NEXT")
    for i, (aspect, man, pap, cod, verd) in enumerate(stats):
        alt = i % 2 == 1
        fc = (245, 245, 250) if alt else (255, 255, 255)
        pdf.set_fill_color(*fc)
        pdf.set_font("Helvetica", "", 7)
        pdf.cell(35, 5, aspect, border=1, fill=True)
        pdf.cell(30, 5, man, border=1, fill=True, align="C")
        pdf.cell(30, 5, pap, border=1, fill=True, align="C")
        pdf.cell(30, 5, cod, border=1, fill=True, align="C")
        pdf.set_font("Helvetica", "B", 7)
        pdf.cell(0, 5, verd, border=1, fill=True, align="C",
                 new_x="LMARGIN", new_y="NEXT")
    pdf.ln(6)

    # ── Detailed findings ────────────────────────────────────────────────
    pdf.add_page()
    pdf.section("Detailed Findings")
    pdf.ln(2)

    for f in FINDINGS:
        pdf.finding(f)

    # ── Correct State Variable Table ─────────────────────────────────────
    pdf.add_page()
    pdf.section("Appendix A: Correct State Variable Index Table")
    pdf.para(
        "The following table shows the ACTUAL state variable indices from "
        "aquabc_II_pelagic_svindex.f90, which should replace the incorrect "
        "table in the AQUABC Reference Manual."
    )
    sv_correct = [
        ("1", "NH4_N", "mg N/L", "Ammonium nitrogen"),
        ("2", "NO3_N", "mg N/L", "Nitrate nitrogen"),
        ("3", "PO4_P", "mg P/L", "Orthophosphate phosphorus"),
        ("4", "DOXY", "mg O2/L", "Dissolved oxygen"),
        ("5", "DIA_C", "mg C/L", "Diatom carbon"),
        ("6", "ZOO_C", "mg C/L", "Zooplankton carbon"),
        ("7", "ZOO_N", "mg N/L", "Zooplankton nitrogen"),
        ("8", "ZOO_P", "mg P/L", "Zooplankton phosphorus"),
        ("9", "DET_PART_ORG_C", "mg C/L", "Detrital particulate organic C"),
        ("10", "DET_PART_ORG_N", "mg N/L", "Detrital particulate organic N"),
        ("11", "DET_PART_ORG_P", "mg P/L", "Detrital particulate organic P"),
        ("12", "DISS_ORG_C", "mg C/L", "Dissolved organic carbon"),
        ("13", "DISS_ORG_N", "mg N/L", "Dissolved organic nitrogen"),
        ("14", "DISS_ORG_P", "mg P/L", "Dissolved organic phosphorus"),
        ("15", "CYN_C", "mg C/L", "Non-fixing cyanobacteria carbon"),
        ("16", "OPA_C", "mg C/L", "Other planktonic algae carbon"),
        ("17", "DISS_Si", "mg Si/L", "Dissolved silica"),
        ("18", "PART_Si", "mg Si/L", "Particulate (biogenic) silica"),
        ("19", "FIX_CYN_C", "mg C/L", "N-fixing cyanobacteria carbon"),
        ("20", "INORG_C", "mg C/L", "Dissolved inorganic carbon"),
        ("21", "TOT_ALK", "meq/L", "Alkalinity"),
        ("22", "FE_II", "mg Fe/L", "Dissolved ferrous iron"),
        ("23", "FE_III", "mg Fe/L", "Particulate ferric iron"),
        ("24", "MN_II", "mg Mn/L", "Dissolved manganese(II)"),
        ("25", "MN_IV", "mg Mn/L", "Particulate manganese(IV)"),
        ("26", "CA", "mg Ca/L", "Calcium"),
        ("27", "MG", "mg Mg/L", "Magnesium"),
        ("28", "S_PLUS_6", "mg S/L", "Dissolved sulphate"),
        ("29", "S_MINUS_2", "mg S/L", "Dissolved sulphide"),
        ("30", "CH4_C", "mg C/L", "Dissolved methane"),
        ("31", "NOST_VEG_HET_C", "mg C/L", "Nostocales vegetative + heterocysts"),
        ("32", "NOST_AKI_C", "mg C/L", "Nostocales akinetes"),
        ("33", "SEC_METAB_1", "mg/L", "Allelopathic metabolite 1"),
        ("34", "SEC_METAB_2", "mg/L", "Allelopathic metabolite 2"),
        ("35", "SEC_METAB_3", "mg/L", "Allelopathic metabolite 3"),
        ("36", "SEC_METAB_4", "mg/L", "Allelopathic metabolite 4"),
    ]
    pdf.set_font("Helvetica", "B", 7)
    pdf.set_fill_color(220, 220, 220)
    pdf.cell(10, 5, "Idx", border=1, fill=True, align="C")
    pdf.cell(38, 5, "Code Name", border=1, fill=True)
    pdf.cell(20, 5, "Unit", border=1, fill=True, align="C")
    pdf.cell(0, 5, "Description", border=1, fill=True,
             new_x="LMARGIN", new_y="NEXT")
    for i, (idx, name, unit, desc) in enumerate(sv_correct):
        alt = i % 2 == 1
        if int(idx) > 32:
            pdf.set_fill_color(255, 240, 220)  # orange tint for allelopathy
        elif int(idx) > 21:
            pdf.set_fill_color(240, 248, 255)  # blue tint for extended
        elif alt:
            pdf.set_fill_color(245, 245, 250)
        else:
            pdf.set_fill_color(255, 255, 255)
        pdf.set_font("Helvetica", "", 7)
        pdf.cell(10, 4.5, idx, border=1, fill=True, align="C")
        pdf.set_font("Courier", "", 6)
        pdf.cell(38, 4.5, name, border=1, fill=True)
        pdf.set_font("Helvetica", "I", 6.5)
        pdf.cell(20, 4.5, unit, border=1, fill=True, align="C")
        pdf.set_font("Helvetica", "", 7)
        pdf.cell(0, 4.5, desc, border=1, fill=True,
                 new_x="LMARGIN", new_y="NEXT")
    pdf.ln(2)
    pdf.set_font("Helvetica", "I", 7)
    pdf.set_text_color(80, 80, 80)
    pdf.multi_cell(0, 3.5,
                   "White/alternating: core 21 variables (paper config). "
                   "Blue tint: extended variables (code only). "
                   "Orange tint: allelopathy variables (appended).")
    pdf.set_text_color(0, 0, 0)

    # ── Appendix B: Document-Specific Recommendations ────────────────────
    pdf.add_page()
    pdf.section("Appendix B: Recommended Corrections by Document")
    pdf.ln(2)

    pdf.set_font("Helvetica", "B", 11)
    pdf.set_text_color(30, 50, 80)
    pdf.cell(0, 7, "AQUABC Reference Manual", new_x="LMARGIN", new_y="NEXT")
    pdf.set_text_color(0, 0, 0)
    aquabc_recs = [
        "F-01: Rewrite state variable index table (CRITICAL)",
        "F-02: Fix allelopathy variable indices 33-36, not 29-32 (CRITICAL)",
        "F-05: Complete constant category listing to cover 299-323 (MODERATE)",
        "F-06: Change '170' to '171' sediment constants (LOW)",
        "F-09: Clarify SU vs Liebig: SU for N-P non-fixing, Liebig elsewhere (MODERATE)",
        "F-13: Document zooplankton switching power = 1.5 hardcoded (LOW)",
        "F-14: Add CTMI parameter name mapping table (MODERATE)",
        "F-16: Document three-regime hypoxia system with equations (MODERATE)",
        "F-17: Note Nostocales naming: prefer NOST_* over FIX_CYN_* (LOW)",
        "F-19: Document CO2SYS K1K2 = 4 (Mehrbach/Dickson-Millero) (LOW)",
    ]
    for r in aquabc_recs:
        pdf.set_font("Helvetica", "", 8)
        pdf.cell(4, 4, "-")
        pdf.cell(0, 4, r, new_x="LMARGIN", new_y="NEXT")
    pdf.ln(3)

    pdf.set_font("Helvetica", "B", 11)
    pdf.set_text_color(30, 50, 80)
    pdf.cell(0, 7, "ESTAS Reference Manual", new_x="LMARGIN", new_y="NEXT")
    pdf.set_text_color(0, 0, 0)
    estas_recs = [
        "F-04: Document RK2 (Heun's method) as available solver, not 'future' (MODERATE)",
        "F-07: Update mod_GLOBAL.f90 line count or remove line counts (LOW)",
        "F-11: Add terminology mapping between paper Eq.1 and code derivative names (LOW)",
        "F-15: Document spin-up (repeat cycle) state carryover mechanism (LOW)",
        "F-20: Document how to enable eco-exergy output (LOW)",
    ]
    for r in estas_recs:
        pdf.set_font("Helvetica", "", 8)
        pdf.cell(4, 4, "-")
        pdf.cell(0, 4, r, new_x="LMARGIN", new_y="NEXT")
    pdf.ln(3)

    pdf.set_font("Helvetica", "B", 11)
    pdf.set_text_color(30, 50, 80)
    pdf.cell(0, 7, "Paper Errata (Minor)", new_x="LMARGIN", new_y="NEXT")
    pdf.set_text_color(0, 0, 0)
    paper_recs = [
        "F-03: Paper claims 'Fortran 2003'; code uses only Fortran 90/95 features",
        "F-08: Paper lists 21 state variables; code has 32 (paper describes subset -- acceptable)",
        "F-12: Paper says 183 constants; code has 323+ (paper describes calibrated subset -- acceptable)",
    ]
    for r in paper_recs:
        pdf.set_font("Helvetica", "", 8)
        pdf.cell(4, 4, "-")
        pdf.cell(0, 4, r, new_x="LMARGIN", new_y="NEXT")
    pdf.ln(6)

    # ── Appendix C: References ───────────────────────────────────────────
    pdf.add_page()
    pdf.section("Appendix C: References Consulted")
    refs = [
        "Erturk, A., Sakurova, I., Zilius, M., Zemlys, P., Umgiesser, G., "
        "Kaynaroglu, B., Pilkaityte, R. & Razinkovas-Baziukas, A. (2023) "
        "Development of a pelagic biogeochemical model with enhanced "
        "computational performance by optimizing ecological complexity and "
        "spatial resolution. Ecological Modelling, 486, 110509.",

        "Rosso, L., Lobry, J.R. & Flandrois, J.P. (1993) An unexpected "
        "correlation between cardinal temperatures of microbial growth "
        "highlighted by a new model. J. Theor. Biol., 162, 447-463.",

        "Saito, M.A., Goepfert, T.J. & Ritt, J.T. (2008) Some thoughts "
        "on the concept of colimitation. Limnol. Oceanogr., 53, 276-290.",

        "Gentleman, W., Leising, A., Frost, B., Strom, S. & Murray, J. "
        "(2003) Functional responses for zooplankton feeding on multiple "
        "resources. J. Plankton Res., 25, 1215-1234.",

        "Lewis, E. & Wallace, D.W.R. (1998) Program Developed for CO2 "
        "System Calculations. ORNL/CDIAC-105.",

        "Boudreau, B.P. (1997) Diagenetic Models and Their Implementation. "
        "Springer.",

        "Soetaert, K., Herman, P.M.J. & Middelburg, J.J. (1996) A model "
        "of early diagenetic processes from the shelf to abyssal depths. "
        "Geochim. Cosmochim. Acta, 60, 1019-1040.",

        "Droop, M.R. (1973) Some thoughts on nutrient limitation in algae. "
        "J. Phycol., 9, 264-272.",
    ]
    for i, ref in enumerate(refs, 1):
        pdf.set_font("Helvetica", "", 7.5)
        pdf.multi_cell(0, 3.5, f"[{i}] {ref}")
        pdf.ln(1.5)

    # ── output ───────────────────────────────────────────────────────────
    out = OUT_DIR / "Manual_vs_Paper_vs_Code_Analysis.pdf"
    out.parent.mkdir(parents=True, exist_ok=True)
    pdf.output(str(out))
    return out


def main():
    out = build_pdf()
    print(f"PDF written to {out}  ({out.stat().st_size / 1024:.0f} KB)")
    print(f"Total findings: {len(FINDINGS)}")
    for sev in [SEV_CRIT, SEV_MOD, SEV_LOW, SEV_INFO]:
        n = sum(1 for f in FINDINGS if f.severity == sev)
        print(f"  {sev}: {n}")


if __name__ == "__main__":
    main()
