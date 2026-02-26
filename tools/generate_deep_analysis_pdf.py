#!/usr/bin/env python3
"""
Generate a PDF report summarising the deep analysis of three remaining
AQUABC model issues: AKI_C, Detritus C:N, and Calcium washout.

Includes root-cause Fortran code analysis, constant values, applied fixes,
and before/after verification results.
"""

from __future__ import annotations

from datetime import date
from pathlib import Path

from fpdf import FPDF

OUT_DIR = Path(__file__).resolve().parent.parent / "docs"


# ── Status labels ────────────────────────────────────────────────────────────
STAT_FIXED   = "FIXED"
STAT_BY_DESIGN = "BY DESIGN"
STAT_UNDERSTOOD = "UNDERSTOOD"


# ── Issue dataclass ──────────────────────────────────────────────────────────
class Issue:
    def __init__(
        self,
        iid: str,
        title: str,
        status: str,
        symptom: str,
        root_cause: str,
        fortran_code: str,
        constants: str,
        fix_applied: str,
        verification: str,
    ):
        self.iid = iid
        self.title = title
        self.status = status
        self.symptom = symptom
        self.root_cause = root_cause
        self.fortran_code = fortran_code
        self.constants = constants
        self.fix_applied = fix_applied
        self.verification = verification


# ── All issues ───────────────────────────────────────────────────────────────
ISSUES: list[Issue] = [
    # ── Issue 1: AKI_C ──────────────────────────────────────────────────
    Issue(
        "ISS-01",
        "AKI_C = 8.0 g/m^2 Constant in Mud Boxes",
        STAT_BY_DESIGN,
        symptom=(
            "State variable 32 (AKI_C, cyanobacteria akinetes) remains "
            "constant at exactly 8.0 g/m^2 in all mud-bottom boxes (3, 14, "
            "15, 17, 18, 19, 21, 23, 25) and exactly 0.0 in all sand-bottom "
            "boxes throughout the entire 200-day simulation."
        ),
        root_cause=(
            "AKI_C is a STATIC SEED BANK by design. The derivative is "
            "zero because all four processes that control it evaluate to "
            "zero:\n\n"
            "1) FORMATION: R_FORM_NOST_AKI = AKI_FORM * NOST_VEG_HET_C. "
            "Since NOST_VEG_HET_C starts at 0.0 in both IC sets, formation "
            "rate = 0.\n"
            "2) GERMINATION: requires DIN < 0.1 g/m^3 AND temperature > "
            "21 C simultaneously -- conditions not typically met.\n"
            "3) LOSS: K_LOSS_AKI = 0.000 in WCONST_04.txt (line 303).\n"
            "4) MORTALITY: K_MORT_AKI_20 = 0.000 in WCONST_04.txt "
            "(line 304).\n\n"
            "The derivative = Formation - Germination - Loss - Mortality "
            "= 0 - 0 - 0 - 0 = 0."
        ),
        fortran_code=(
            "Derivative (aquabc_II_pelagic_model.f90 L2433-L2437):\n"
            "  DERIVATIVES(NOST_AKI_C_INDEX) =\n"
            "      R_FORM_NOST_AKI - R_GERM_NOST_AKI\n"
            "      - R_LOSS_AKI - R_MORT_AKI\n\n"
            "Germination trigger (aquabc_II_pelagic_lib_NOSTACALES.f90 "
            "L341-L348):\n"
            "  where (DIN < KN_GERM_AKI .and. TEMP > T_GERM_AKI)\n"
            "      AKI_GERM = KR_GERM_AKI  ! = 0.3 /day\n"
            "  elsewhere\n"
            "      AKI_GERM = 0.0D0\n"
            "  end where\n\n"
            "Formation trigger (L358-L363):\n"
            "  where (TEMP < T_FORM_AKI .and. DAY_OF_YEAR > DAY_FORM_AKI)\n"
            "      AKI_FORM = KR_FORM_AKI  ! = 0.1 /day\n"
            "  elsewhere\n"
            "      AKI_FORM = 0.0D0\n"
            "  end where\n\n"
            "Code comments confirm benthic storage:\n"
            "  'Akinetes before germination are located in the mud but\n"
            "   internally still in units gC/m^3 though initial condition\n"
            "   is given in gC/m^2 and converted to gC/m^3'"
        ),
        constants=(
            "WCONST_04.txt akinete constants (lines 298-306):\n"
            "  298  KR_GERM_AKI      = 0.3     Germination rate (1/day)\n"
            "  299  KN_GERM_AKI      = 0.1     DIN threshold (g/m^3)\n"
            "  300  KR_FORM_AKI      = 0.1     Formation rate (1/day)\n"
            "  301  DAY_FORM_AKI     = 200     Day-of-year trigger\n"
            "  302  T_FORM_AKI       = 16.0    Temp. for formation (C)\n"
            "  303  K_LOSS_AKI       = 0.000   Loss rate\n"
            "  304  K_MORT_AKI_20    = 0.000   Mortality rate at 20C\n"
            "  305  THETA_K_MORT_AKI = 1.020   Temperature correction\n"
            "  306  T_GERM_AKI       = 21.0    Temp. for germination (C)\n\n"
            "INITIAL_CONDITION_TYPE = 2 (area-based g/m^2, unique among "
            "all 36 state variables).\n"
            "OUTPUT_TYPE = 2 (converted back to g/m^2 for output).\n"
            "SETTLING_VEL_NO = 0 (no settling -- sits at bottom by design)."
        ),
        fix_applied=(
            "NO FIX NEEDED. This is expected model behaviour.\n\n"
            "AKI_C represents a dormant akinete seed bank in the sediment. "
            "It will only change when:\n"
            "  (a) Vegetative Nostocales (NOST_VEG_HET_C) grow large enough "
            "to produce akinetes via formation (autumn, T < 16 C, DOY > 200),"
            " OR\n"
            "  (b) Environmental conditions trigger germination (spring/summer"
            ", T > 21 C, DIN < 0.1 g/m^3).\n\n"
            "The 8.0 g/m^2 value in mud boxes is the initial seed bank."
        ),
        verification=(
            "Initial conditions (INIT_CONC_1.txt, mud boxes): AKI_C = 8.0\n"
            "Initial conditions (INIT_CONC_2.txt, sand boxes): AKI_C = 0.0\n"
            "200-day output: AKI_C = 8.0 in all mud boxes (confirmed constant)"
            "\n200-day output: AKI_C = 0.0 in all sand boxes (confirmed "
            "constant)\n\nStatus: Behaving as designed."
        ),
    ),
    # ── Issue 2: Detritus C:N ───────────────────────────────────────────
    Issue(
        "ISS-02",
        "Detritus C:N Ratio Crashes to 0.08 (Should Be ~6.6 Redfield)",
        STAT_FIXED,
        symptom=(
            "The mass ratio DET_PART_ORG_C / DET_PART_ORG_N drops from "
            "6.0 (IC) to ~0.07-0.11 within 10-20 days. This means carbon "
            "is stripped from detritus ~50-100x faster than nitrogen, "
            "producing an ecologically impossible C:N ratio."
        ),
        root_cause=(
            "Two parameter errors in WCONST_04.txt caused catastrophically "
            "asymmetric POC vs PON dissolution:\n\n"
            "1) KDISS_DET_PART_ORG_C_20 was set to 10.0 /day, while "
            "KDISS_DET_PART_ORG_N_20 was 0.25 /day -- a 40x disparity. "
            "The original backup file (const_CL.txt.bak) had 0.1 for carbon."
            "\n\n"
            "2) FAC_PHYT_DET_PART_ORG_C was 2.0 (phytoplankton-dependent "
            "boost to C dissolution), further amplifying carbon removal. "
            "The original backup had 0.0.\n\n"
            "Combined effect: carbon dissolved at rate (10.0 + 2.0 * "
            "PHYT_TOT_C) * theta^(T-20), ~40-100x faster than nitrogen "
            "which dissolved at only 0.25 /day.\n\n"
            "The dissolution equations are structurally identical for C and "
            "N, so with matched rates the C:N ratio is preserved."
        ),
        fortran_code=(
            "POC dissolution (aquabc_II_pelagic_lib_ORGANIC_CARBON.f90 "
            "L40-44):\n"
            "  R_DET_PART_ORG_C_DISSOLUTION =\n"
            "    (KDISS_DET_PART_ORG_C_20 + FAC_PHYT * PHYT_TOT_C) *\n"
            "    (THETA ^ (TEMP - 20)) * DET_PART_ORG_C *\n"
            "    (KHS_POC_DISS_SAT / (DET_C + KHS_POC_DISS_SAT))\n\n"
            "PON dissolution (aquabc_II_pelagic_model.f90 L1310-1312):\n"
            "  R_DET_PART_ORG_N_DISSOLUTION =\n"
            "    (KDISS_DET_PART_ORG_N_20 + FAC_PHYT_N * PHYT_TOT_C) *\n"
            "    (THETA ^ (TEMP - 20)) * DET_PART_ORG_N *\n"
            "    (KHS_PON_DISS_SAT / (DET_N + KHS_PON_DISS_SAT))\n\n"
            "DET_C derivative (L2630-2640): +mortality inputs - zoo feeding "
            "- dissolution\n"
            "DET_N derivative (L2683-2695): same structure, scaled by N:C "
            "ratios\n\n"
            "The equations are symmetric -- the C:N imbalance came entirely "
            "from the 40x rate constant difference."
        ),
        constants=(
            "WCONST_04.txt BEFORE fix:\n"
            "  134  KDISS_DET_PART_ORG_C_20    = 10.0   (40x too high)\n"
            "  135  THETA_KDISS_DET_PART_ORG_C = 1.06\n"
            "  136  FAC_PHYT_DET_PART_ORG_C    = 2.0    (should be 0.0)\n"
            "  137  KDISS_DET_PART_ORG_N_20    = 0.25\n"
            "  138  THETA_KDISS_DET_PART_ORG_N = 1.06\n"
            "  140  FAC_PHYT_DET_PART_ORG_N    = 0.0\n\n"
            "WCONST_04.txt AFTER fix:\n"
            "  134  KDISS_DET_PART_ORG_C_20    = 0.25   (matches N rate)\n"
            "  136  FAC_PHYT_DET_PART_ORG_C    = 0.0    (restored)\n\n"
            "Reference backup (const_CL.txt.bak, original values):\n"
            "  134  KDISS_DET_PART_ORG_C_20    = 0.1\n"
            "  136  FAC_PHYT_DET_PART_ORG_C    = 0.0\n\n"
            "Saturation half-constants:\n"
            "  307  KHS_POC_DISS_SAT = 1.250\n"
            "  308  KHS_PON_DISS_SAT = 0.250\n"
            "  309  KHS_POP_DISS_SAT = 0.025"
        ),
        fix_applied=(
            "Two changes to INPUTS/WCONST_04.txt:\n\n"
            "1) Line 134: KDISS_DET_PART_ORG_C_20 changed from 10.0 to "
            "0.25\n"
            "   Rationale: Match the nitrogen dissolution rate (0.25 /day) "
            "to ensure Redfield-consistent C:N decay. The original backup "
            "value was 0.1; we chose 0.25 to exactly match N and produce "
            "balanced dissolution.\n\n"
            "2) Line 136: FAC_PHYT_DET_PART_ORG_C changed from 2.0 to 0.0\n"
            "   Rationale: Restore original value. The phytoplankton-"
            "dependent boost to C dissolution was absent in the original "
            "and absent for N (FAC_PHYT_DET_PART_ORG_N = 0.0). Setting it "
            "to zero ensures symmetric treatment."
        ),
        verification=(
            "200-day simulation results (final timestep, day ~6574):\n\n"
            "          DET_C    DET_N    C:N(mass)    C:N(molar)\n"
            "  Box 5:  0.0996   0.0158   6.29         7.33\n"
            "  Box 6:  0.1085   0.0171   6.33         7.38\n"
            "  Box 8:  0.0726   0.0116   6.26         7.30\n"
            "  Box 9:  0.0735   0.0118   6.24         7.28\n"
            "  Box 14: 0.0306   0.0050   6.14         7.17\n"
            "  Box 17: 0.0707   0.0111   6.34         7.40\n"
            "  Box 25: 0.1535   0.0236   6.50         7.58\n\n"
            "C:N molar ratios 7.2-7.6 across all boxes. Redfield = 6.6.\n"
            "BEFORE fix: C:N molar = 0.07-0.13 (catastrophic imbalance).\n"
            "IMPROVEMENT: C:N ratio restored to ecologically realistic range."
        ),
    ),
    # ── Issue 3: Calcium washout ────────────────────────────────────────
    Issue(
        "ISS-03",
        "Calcium Declining 84-97% Over 200 Days",
        STAT_UNDERSTOOD,
        symptom=(
            "Calcium (state variable 26) declines from its initial "
            "condition of 70 mg/L to 2-14 mg/L over the 200-day simulation, "
            "even after river boundary Ca was set to 50 mg/L. The decline "
            "is steeper in boxes closer to the Baltic Sea boundary."
        ),
        root_cause=(
            "Calcium is a PURELY CONSERVATIVE variable with zero kinetic "
            "derivatives. Its concentration is governed entirely by "
            "advective transport and mixing.\n\n"
            "The problem is a simple BOUNDARY MISMATCH:\n"
            "  - Initial condition: Ca = 70 mg/L (both IC sets)\n"
            "  - Baltic Sea boundary (FORC_TS_1): Ca = 2.2 mg/L (real data)\n"
            "  - River boundaries (FORC_TS_2-5): Ca = 50 mg/L (our fix)\n\n"
            "The lagoon is approaching a flow-weighted equilibrium between "
            "the Baltic (2.2 mg/L) and rivers (50 mg/L). With dominant "
            "Baltic exchange, the equilibrium is much lower than 70 mg/L.\n\n"
            "This is expected physics -- the initial condition of 70 mg/L "
            "was simply too high for the boundary conditions supplied."
        ),
        fortran_code=(
            "Calcium derivative (aquabc_II_pelagic_model.f90 L3039):\n"
            "  DERIVATIVES(ns:ne, CA_INDEX) = 0.0D0\n\n"
            "This means Ca has NO biogeochemical processes -- it is purely "
            "conservative. Its evolution is controlled entirely by:\n"
            "  - Advective transport (FLOW_TS, ADVECTIVE_LINKS)\n"
            "  - Dispersive mixing (DISPERSIVE_LINKS)\n"
            "  - Open boundary forcing (FORC_TS files)\n\n"
            "Magnesium has identical treatment:\n"
            "  DERIVATIVES(ns:ne, MG_INDEX) = 0.0D0\n\n"
            "State variable index (aquabc_II_pelagic_svindex.f90 L31):\n"
            "  integer, parameter :: CA_INDEX = 26\n\n"
            "Deposited fraction from PELAGIC_INPUTS.txt:\n"
            "  DISSOLVED_FRAC = 0.00, DEPOSITED_FRACTION = 0.80-0.99\n"
            "  (Ca is treated as dissolved in the model)."
        ),
        constants=(
            "No kinetic constants for Calcium (DERIVATIVES = 0).\n\n"
            "Boundary concentrations from forcing files:\n"
            "  FORC_TS_1 (Baltic Sea): Ca ~ 2.22 mg/L (varies slightly)\n"
            "  FORC_TS_2-5 (Rivers):   Ca = 50 mg/L (corrected from 0.0)\n\n"
            "Initial conditions:\n"
            "  INIT_CONC_1.txt (mud boxes):  Ca = 70 mg/L\n"
            "  INIT_CONC_2.txt (sand boxes): Ca = 70 mg/L\n\n"
            "Final values after 200 days:\n"
            "  Box 5:  Ca = 7.38 mg/L\n"
            "  Box 6:  Ca = 8.47 mg/L\n"
            "  Box 9:  Ca = 5.71 mg/L\n"
            "  Box 14: Ca = 3.07 mg/L (close to Baltic)\n"
            "  Box 25: Ca = 14.01 mg/L (most upstream)"
        ),
        fix_applied=(
            "No additional fix applied at this time.\n\n"
            "The Ca decline is physically correct given the boundary "
            "conditions. Two potential actions for the future:\n\n"
            "  Option A: Reduce the Ca initial condition from 70 to ~20-30 "
            "mg/L to better match the flow-weighted equilibrium, reducing "
            "the spin-up transient.\n\n"
            "  Option B: Review the Baltic Sea boundary Ca data. The value "
            "of ~2.2 mg/L seems low for brackish water (typical Baltic "
            "surface Ca = 50-100 mg/L in the southern Baltic). This might "
            "indicate that the boundary forcing data uses different units "
            "or was not properly set for Ca.\n\n"
            "Recommendation: Investigate Baltic boundary Ca units. If the "
            "FORC_TS_1 Ca data is in mmol/L rather than mg/L, it needs to "
            "be multiplied by 40 (molar mass of Ca), giving ~89 mg/L which "
            "would be physically consistent."
        ),
        verification=(
            "200-day simulation confirms Ca is declining toward boundary "
            "equilibrium:\n\n"
            "  Time       Box 5    Box 14   Box 25\n"
            "  Day 0:     70.00    70.00    70.00\n"
            "  Day 10:    17.86    --       --\n"
            "  Day 100:   3.92     --       --\n"
            "  Day 200:   7.38     3.07     14.01\n\n"
            "Pattern: Boxes closer to Baltic (14) equilibrate to ~3 mg/L "
            "(near Baltic 2.2). Upstream box 25 retains more Ca (14 mg/L) "
            "due to river input of 50 mg/L.\n\n"
            "Status: Physically correct transport behaviour. Initial "
            "condition was simply inconsistent with boundary forcing."
        ),
    ),
]


# ── Previously fixed issues (earlier session) ───────────────────────────────
PRIOR_FIXES = [
    {
        "title": "DIC/INORG_C near zero (0.003 mg C/L)",
        "fix": "IC changed from 0.003 to 25.0 mg C/L; Baltic boundary "
               "set to 22.0; river boundary set to 30.0",
        "result": "DIC now 2.4-25 mg C/L (realistic range)",
    },
    {
        "title": "TOT_ALK near zero (0.003 meq/L)",
        "fix": "IC changed from 0.003 to 2.8 meq/L; Baltic boundary "
               "set to 1.8; river boundary set to 3.5",
        "result": "ALK now 1.9-3.3 meq/L (realistic range)",
    },
    {
        "title": "ZOO_N and ZOO_P = 0 (broken stoichiometry)",
        "fix": "IC changed from 0.0 to ZOO_N=0.004, ZOO_P=0.0005",
        "result": "Zoo C:N now 6-11 (was 440-1700)",
    },
    {
        "title": "River Ca, Mg, Fe, Mn, SO4 all zero",
        "fix": "River boundaries set to Ca=50, Mg=10, Fe_II=0.1, "
               "Fe_III=0.2, Mn_II=0.02, Mn_IV=0.03, SO4=15",
        "result": "Conservative species now have non-zero supply",
    },
]


# ═══════════════════════════════════════════════════════════════════════════
class DeepAnalysisPDF(FPDF):
    """Custom PDF with styled header/footer."""

    def __init__(self):
        super().__init__(orientation="P", unit="mm", format="A4")
        self.set_auto_page_break(auto=True, margin=18)
        self.alias_nb_pages()

    # ── header / footer ──────────────────────────────────────────────────
    def header(self):
        self.set_font("Helvetica", "B", 8)
        self.set_text_color(100, 100, 100)
        self.cell(
            0, 5,
            "ESTAS-AQUABC  |  Deep Model Analysis  |  "
            "Problems & Solutions Report",
            align="C",
        )
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

    # ── status colour ────────────────────────────────────────────────────
    def _status_color(self, status: str):
        if status == STAT_FIXED:
            self.set_fill_color(46, 139, 87)     # green
            self.set_text_color(255, 255, 255)
        elif status == STAT_BY_DESIGN:
            self.set_fill_color(70, 130, 180)     # steel-blue
            self.set_text_color(255, 255, 255)
        else:  # UNDERSTOOD
            self.set_fill_color(210, 160, 50)     # amber
            self.set_text_color(0, 0, 0)

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

    def subsection(self, text: str):
        self.set_font("Helvetica", "B", 10)
        self.set_text_color(30, 50, 80)
        self.cell(0, 6, text, new_x="LMARGIN", new_y="NEXT")
        self.set_text_color(0, 0, 0)
        self.ln(1)

    def para(self, text: str, size: float = 9):
        self.set_font("Helvetica", "", size)
        self.set_text_color(30, 30, 30)
        self.multi_cell(0, 4.5, text)
        self.set_text_color(0, 0, 0)
        self.ln(2)

    def code_block(self, text: str, size: float = 7.5):
        self.set_font("Courier", "", size)
        self.set_fill_color(245, 245, 250)
        self.set_text_color(40, 40, 60)
        self.multi_cell(0, 3.6, text, fill=True)
        self.set_text_color(0, 0, 0)
        self.set_font("Helvetica", "", 9)
        self.ln(2)

    def labeled(self, label: str, text: str, size: float = 8):
        self.set_font("Helvetica", "B", size)
        self.set_text_color(30, 50, 80)
        self.cell(30, 4, f"{label}:", new_x="END")
        self.set_font("Helvetica", "", size)
        self.set_text_color(40, 40, 40)
        x0 = self.get_x()
        w = self.w - self.r_margin - x0
        self.multi_cell(w, 3.8, text)
        self.set_text_color(0, 0, 0)
        self.ln(1)

    # ── issue block ──────────────────────────────────────────────────────
    def issue_block(self, iss: Issue):
        self.add_page()

        # Badge line
        self._status_color(iss.status)
        self.set_font("Helvetica", "B", 9)
        self.cell(16, 5.5, f" {iss.iid} ", fill=True)
        self.cell(2, 5.5, "")
        self.cell(28, 5.5, f" {iss.status} ", fill=True)
        self.set_text_color(0, 0, 0)
        self.set_fill_color(255, 255, 255)
        self.cell(2, 5.5, "")
        self.set_font("Helvetica", "B", 11)
        self.cell(0, 5.5, iss.title, new_x="LMARGIN", new_y="NEXT")
        self.ln(4)

        # Symptom
        self.subsection("Symptom")
        self.para(iss.symptom, 8.5)

        # Root Cause
        self.subsection("Root-Cause Analysis")
        self.para(iss.root_cause, 8.5)

        # Fortran Code
        self.subsection("Fortran Source Code Evidence")
        self.code_block(iss.fortran_code)

        # Constants
        self.subsection("Model Constants (WCONST_04.txt)")
        self.code_block(iss.constants)

        # Fix Applied
        self.subsection("Fix Applied")
        if iss.status == STAT_FIXED:
            self.set_fill_color(230, 248, 230)
        elif iss.status == STAT_BY_DESIGN:
            self.set_fill_color(230, 240, 255)
        else:
            self.set_fill_color(255, 248, 230)
        self.set_font("Helvetica", "", 8.5)
        self.set_text_color(30, 30, 30)
        self.multi_cell(0, 4.2, iss.fix_applied, fill=True)
        self.set_text_color(0, 0, 0)
        self.ln(3)

        # Verification
        self.subsection("Verification Results")
        self.code_block(iss.verification, 7)

    # ── summary table row ────────────────────────────────────────────────
    def summary_row(self, iid: str, status: str, title: str, alt: bool):
        if alt:
            self.set_fill_color(245, 245, 250)
        else:
            self.set_fill_color(255, 255, 255)

        self._status_color(status)
        self.set_font("Helvetica", "B", 7.5)
        self.cell(14, 5.5, iid, border="LTB", fill=True, align="C")
        self.cell(24, 5.5, status, border="TB", fill=True, align="C")

        self.set_text_color(0, 0, 0)
        if alt:
            self.set_fill_color(245, 245, 250)
        else:
            self.set_fill_color(255, 255, 255)
        self.set_font("Helvetica", "", 7.5)
        self.cell(0, 5.5, f"  {title}", border="TBR",
                  fill=True, new_x="LMARGIN", new_y="NEXT")

    def summary_header(self):
        self.set_font("Helvetica", "B", 7.5)
        self.set_fill_color(220, 220, 220)
        self.set_text_color(0, 0, 0)
        self.cell(14, 5.5, "ID", border=1, fill=True, align="C")
        self.cell(24, 5.5, "Status", border=1, fill=True, align="C")
        self.cell(0, 5.5, "  Title", border=1, fill=True,
                  new_x="LMARGIN", new_y="NEXT")


# ═══════════════════════════════════════════════════════════════════════════
def build_pdf() -> Path:
    pdf = DeepAnalysisPDF()
    pdf.add_page()

    # ── Title page ───────────────────────────────────────────────────────
    pdf.ln(12)
    pdf.big_title("AQUABC Model Deep Analysis")
    pdf.big_title("Problems & Solutions Report")
    pdf.ln(3)
    pdf.subtitle(
        "Root-Cause Investigation of Remaining Model Issues\n"
        "AKI_C Dynamics  |  Detritus C:N Ratio  |  Calcium Washout"
    )
    pdf.ln(6)
    pdf.set_font("Helvetica", "", 9)
    pdf.set_fill_color(240, 244, 248)
    summary = (
        "This document presents a deep technical analysis of three model "
        "behaviour issues identified during the AQUABC ecological model QA "
        "process. Each issue is investigated at the Fortran source code "
        "level, with root-cause analysis, relevant model constants, applied "
        "fixes, and verification results.\n\n"
        "Three issues were investigated:\n"
        "  ISS-01  AKI_C = 8.0 g/m^2 constant in mud boxes (BY DESIGN)\n"
        "  ISS-02  Detritus C:N crashes to 0.08 (FIXED)\n"
        "  ISS-03  Calcium declining 84-97% over 200 days (UNDERSTOOD)\n\n"
        "These investigations follow an earlier round of corrections that "
        "fixed 5 critical issues with initial conditions and boundary "
        "forcing files (DIC, ALK, ZOO_N/P, river metals).\n\n"
        "Model: ESTAS-AQUABC v0.2 (Curonian Lagoon, 25 boxes)\n"
        "Simulation: 200 days (Julian days 6209-6574)\n"
        "36 state variables, 318 model constants\n"
        f"Date: {date.today().strftime('%d %B %Y')}"
    )
    pdf.multi_cell(0, 4.5, summary, fill=True)
    pdf.ln(6)

    # ── Executive summary table ──────────────────────────────────────────
    pdf.section("Executive Summary")
    pdf.summary_header()
    for i, iss in enumerate(ISSUES):
        pdf.summary_row(iss.iid, iss.status, iss.title, i % 2 == 1)
    pdf.ln(6)

    # ── Prior fixes table ────────────────────────────────────────────────
    pdf.section("Previously Applied Corrections (Earlier Session)")
    pdf.para(
        "Before the deep investigation, the following critical issues were "
        "identified and corrected in the initial conditions and boundary "
        "forcing files:",
        9,
    )
    pdf.set_font("Helvetica", "B", 7.5)
    pdf.set_fill_color(220, 220, 220)
    pdf.cell(60, 5.5, "  Issue", border=1, fill=True)
    pdf.cell(70, 5.5, "  Fix Applied", border=1, fill=True)
    pdf.cell(0, 5.5, "  Result", border=1, fill=True,
             new_x="LMARGIN", new_y="NEXT")
    for i, pf in enumerate(PRIOR_FIXES):
        alt = i % 2 == 1
        bg = (245, 245, 250) if alt else (255, 255, 255)
        pdf.set_fill_color(*bg)
        pdf.set_font("Helvetica", "", 7)
        y0 = pdf.get_y()
        x0 = pdf.get_x()
        # Write each cell
        pdf.multi_cell(60, 3.5, pf["title"], border="LTB", fill=True)
        h1 = pdf.get_y() - y0
        pdf.set_xy(x0 + 60, y0)
        pdf.multi_cell(70, 3.5, pf["fix"], border="TB", fill=True)
        h2 = pdf.get_y() - y0
        pdf.set_xy(x0 + 130, y0)
        pdf.multi_cell(0, 3.5, pf["result"], border="TBR", fill=True)
        h3 = pdf.get_y() - y0
        max_h = max(h1, h2, h3)
        pdf.set_y(y0 + max_h)
    pdf.ln(4)

    # ── Detailed findings ────────────────────────────────────────────────
    for iss in ISSUES:
        pdf.issue_block(iss)

    # ── Files modified ───────────────────────────────────────────────────
    pdf.add_page()
    pdf.section("Files Modified")
    files_info = (
        "Current Session:\n"
        "  INPUTS/WCONST_04.txt\n"
        "    Line 134: KDISS_DET_PART_ORG_C_20: 10.0 -> 0.25\n"
        "    Line 136: FAC_PHYT_DET_PART_ORG_C: 2.0 -> 0.0\n\n"
        "Previous Session (commit b5b0779):\n"
        "  INPUTS/INIT_CONC_1.txt (mud boxes IC)\n"
        "    DIC: 0.003 -> 25.0, ALK: 0.003 -> 2.8\n"
        "    ZOO_N: 0.0 -> 0.004, ZOO_P: 0.0 -> 0.0005\n\n"
        "  INPUTS/INIT_CONC_2.txt (sand boxes IC)\n"
        "    Same corrections as above\n\n"
        "  INPUTS/FORC_TS_1.txt (Baltic Sea boundary)\n"
        "    DIC: -> 22.0, ALK: -> 1.8\n\n"
        "  INPUTS/FORC_TS_2.txt through FORC_TS_5.txt (River boundaries)\n"
        "    DIC: -> 30, ALK: -> 3.5\n"
        "    Ca: 0 -> 50, Mg: 0 -> 10\n"
        "    Fe_II: 0 -> 0.1, Fe_III: 0 -> 0.2\n"
        "    Mn_II: 0 -> 0.02, Mn_IV: 0 -> 0.03\n"
        "    SO4: 0 -> 15"
    )
    pdf.code_block(files_info, 8)

    # ── Methodology ──────────────────────────────────────────────────────
    pdf.section("Methodology")
    pdf.para(
        "1. Comprehensive value analysis: All 36 state variables were "
        "analysed across all 7 output boxes, checking concentration "
        "ranges, stoichiometric ratios (C:N, C:P, N:P), and temporal "
        "dynamics over 200 days.\n\n"
        "2. Root-cause Fortran analysis: The AQUABC source code was "
        "searched to find the exact derivative equations, process rate "
        "formulations, and kinetic constants controlling each problematic "
        "variable.\n\n"
        "3. Constant comparison: Current WCONST_04.txt values were "
        "compared against the original backup file (const_CL.txt.bak) "
        "to identify modifications that may have introduced errors.\n\n"
        "4. Fix and verify: Corrections were applied to the parameter "
        "file, the 200-day simulation re-run, and results analysed to "
        "confirm improvements.",
        9,
    )

    # ── Save ─────────────────────────────────────────────────────────────
    out = OUT_DIR / "Deep_Model_Analysis_Report.pdf"
    out.parent.mkdir(parents=True, exist_ok=True)
    pdf.output(str(out))
    return out


# ═══════════════════════════════════════════════════════════════════════════
if __name__ == "__main__":
    path = build_pdf()
    print(f"PDF saved to {path}")
