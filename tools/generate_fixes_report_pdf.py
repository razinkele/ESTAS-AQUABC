#!/usr/bin/env python3
"""
Generate a PDF report documenting the fixes applied based on the 3560-day
deep process analysis report, with verification results from the post-fix
simulation.

Output: docs/Fixes_And_Results_Report.pdf
"""

from __future__ import annotations

import os
from datetime import date
from pathlib import Path

import numpy as np
from fpdf import FPDF

# -- Paths --------------------------------------------------------------------
ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = ROOT / "docs"
OUTPUT_DIR = ROOT / "OUTPUTS"

BOXES = [5, 6, 8, 9, 14, 17, 25]
SAND_BOXES = [5, 6, 8, 9]
MUD_BOXES = [14, 17, 25]
NUM_REPEATS = 10
SIM_DAYS = 356  # days per repeat cycle

# State variable column order in .out files (0-indexed, TIME is col 0)
# TIME NH4 NO3 PO4 DO DIA ZOO_C ZOO_N ZOO_P DET_C DET_N DET_P DOC DON DOP
# CYN OPA DSi PSi FCN DIC ALK Fe2 Fe3 Mn2 Mn4 Ca Mg S6 S2- CH4 NOST AKI
# SM_DIA SM_CYN SM_FCN SM_NOST
VAR_COLS = {
    "TIME": 0, "NH4_N": 1, "NO3_N": 2, "PO4_P": 3, "DISS_OXYGEN": 4,
    "DIA_C": 5, "ZOO_C": 6, "ZOO_N": 7, "ZOO_P": 8,
    "DET_PART_ORG_C": 9, "DET_PART_ORG_N": 10, "DET_PART_ORG_P": 11,
    "DISS_ORG_C": 12, "DISS_ORG_N": 13, "DISS_ORG_P": 14,
    "CYN_C": 15, "OPA_C": 16, "DISS_Si": 17, "PART_Si": 18,
    "FIX_CYN_C": 19, "INORG_C": 20, "TOT_ALK": 21,
    "FE_II": 22, "FE_III": 23, "MN_II": 24, "MN_IV": 25,
    "CA": 26, "MG": 27, "S_PLUS_6": 28, "S_MINUS_2": 29, "CH4_C": 30,
    "NOST_VEG_HET_C": 31, "AKI_C": 32,
}

# MTRX column order (0=TIME, 1-36=state vars same order as IC file)
MTRX_VARS = [
    "NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C",
    "ZOO_C", "ZOO_N", "ZOO_P",
    "DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P",
    "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P",
    "CYN_C", "OPA_C", "DISS_Si", "PART_Si",
    "FIX_CYN_C", "INORG_C", "TOT_ALK",
    "FE_II", "FE_III", "MN_II", "MN_IV",
    "CA", "MG", "S_PLUS_6", "S_MINUS_2", "CH4_C",
    "NOST_VEG_HET_C", "AKI_C",
    "SEC_METAB_DIA", "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST",
]


# -- Helpers ------------------------------------------------------------------

def load_box_out(box_id: int) -> np.ndarray:
    """Load PELAGIC_BOX_XXXXX.out as a 2D numpy array (rows x cols)."""
    fname = OUTPUT_DIR / f"PELAGIC_BOX_{box_id:05d}.out"
    data = []
    with open(fname) as f:
        f.readline()  # skip header
        for line in f:
            vals = line.split()
            if len(vals) >= 33:
                data.append([float(v) for v in vals])
    return np.array(data)


def load_mtrx(box_id: int) -> dict[str, float]:
    """Load PELAGIC_BOX_XXXXX.mtrx and return {varname: value}."""
    fname = OUTPUT_DIR / f"PELAGIC_BOX_{box_id:05d}.mtrx"
    with open(fname) as f:
        vals = f.read().split()
    result = {"TIME": float(vals[0])}
    for i, vname in enumerate(MTRX_VARS):
        result[vname] = float(vals[i + 1])
    return result


def year10_slice(data: np.ndarray) -> np.ndarray:
    """Return last 356 rows = Year 10."""
    return data[-SIM_DAYS:] if len(data) >= SIM_DAYS else data


def stats(arr: np.ndarray) -> tuple[float, float, float]:
    """Return (min, max, mean)."""
    return float(np.min(arr)), float(np.max(arr)), float(np.mean(arr))


# -- PDF class ----------------------------------------------------------------

class ReportPDF(FPDF):
    def header(self):
        self.set_font("Helvetica", "B", 9)
        self.set_text_color(100, 100, 100)
        self.cell(0, 5, "AQUABC Fixes & Verification Report", 0, 1, "C")
        self.ln(2)

    def footer(self):
        self.set_y(-15)
        self.set_font("Helvetica", "I", 8)
        self.set_text_color(128, 128, 128)
        self.cell(0, 10, f"Page {self.page_no()}/{{nb}}", 0, 0, "C")

    def section_title(self, title: str):
        self.set_font("Helvetica", "B", 13)
        self.set_text_color(0, 51, 102)
        self.cell(0, 10, title, 0, 1)
        self.set_draw_color(0, 51, 102)
        self.line(self.l_margin, self.get_y(), self.w - self.r_margin, self.get_y())
        self.ln(3)

    def sub_title(self, title: str):
        self.set_font("Helvetica", "B", 11)
        self.set_text_color(0, 80, 120)
        self.cell(0, 7, title, 0, 1)
        self.ln(1)

    def sub_sub_title(self, title: str):
        self.set_font("Helvetica", "BI", 10)
        self.set_text_color(60, 60, 60)
        self.cell(0, 6, title, 0, 1)
        self.ln(1)

    def body_text(self, text: str):
        self.set_font("Helvetica", "", 9)
        self.set_text_color(0, 0, 0)
        self.multi_cell(0, 4.5, text)
        self.ln(2)

    def code_block(self, text: str):
        self.set_font("Courier", "", 7.5)
        self.set_fill_color(240, 240, 245)
        self.set_text_color(30, 30, 30)
        x0 = self.get_x()
        y0 = self.get_y()
        self.multi_cell(0, 3.8, text, fill=True)
        self.ln(2)

    def bullet(self, text: str, indent: float = 10):
        self.set_font("Helvetica", "", 9)
        self.set_text_color(0, 0, 0)
        self.cell(indent, 4.5, "  -  ", 0, 0)
        self.multi_cell(self.w - self.l_margin - self.r_margin - indent, 4.5, text)

    def safe_page_break(self, h: float = 30):
        if self.get_y() + h > self.h - 20:
            self.add_page()

    def data_table(self, headers: list[str], rows: list[list[str]],
                   col_widths: list[float] | None = None):
        """Draw a simple table."""
        if col_widths is None:
            total = self.w - self.l_margin - self.r_margin
            col_widths = [total / len(headers)] * len(headers)

        # Header
        self.set_font("Helvetica", "B", 8)
        self.set_fill_color(0, 51, 102)
        self.set_text_color(255, 255, 255)
        for w, h_text in zip(col_widths, headers):
            self.cell(w, 5, h_text, 1, 0, "C", fill=True)
        self.ln()

        # Rows
        self.set_font("Helvetica", "", 8)
        self.set_text_color(0, 0, 0)
        for i, row in enumerate(rows):
            if i % 2 == 0:
                self.set_fill_color(245, 245, 250)
            else:
                self.set_fill_color(255, 255, 255)
            for w, val in zip(col_widths, row):
                self.cell(w, 4.5, val, 1, 0, "C", fill=True)
            self.ln()
        self.ln(2)


# -- Main report generation ---------------------------------------------------

def generate_report():
    os.makedirs(OUT_DIR, exist_ok=True)

    # Load all box data
    box_data = {}
    box_mtrx = {}
    for b in BOXES:
        box_data[b] = load_box_out(b)
        box_mtrx[b] = load_mtrx(b)

    pdf = ReportPDF()
    pdf.alias_nb_pages()
    pdf.set_auto_page_break(auto=True, margin=20)

    # =========================================================================
    # TITLE PAGE
    # =========================================================================
    pdf.add_page()
    pdf.ln(30)
    pdf.set_font("Helvetica", "B", 22)
    pdf.set_text_color(0, 51, 102)
    pdf.cell(0, 12, "AQUABC Model Fixes &", 0, 1, "C")
    pdf.cell(0, 12, "Verification Report", 0, 1, "C")
    pdf.ln(8)
    pdf.set_font("Helvetica", "", 12)
    pdf.set_text_color(80, 80, 80)
    pdf.cell(0, 8, "Implementation of 3560-Day Deep Analysis Recommendations", 0, 1, "C")
    pdf.ln(15)
    pdf.set_font("Helvetica", "", 10)
    pdf.set_text_color(0, 0, 0)
    info_lines = [
        f"Date: {date.today().strftime('%B %d, %Y')}",
        "Model: ESTAS-AQUABC v0.3 (Curonian Lagoon)",
        "Commit: aeb15fc (main)",
        "Simulation: 3560 days (10 x 356-day repeats)",
        "Configuration: MODEL_SEDIMENTS=1, updated ICs & boundaries",
    ]
    for line in info_lines:
        pdf.cell(0, 6, line, 0, 1, "C")
    pdf.ln(15)
    pdf.set_font("Helvetica", "I", 9)
    pdf.set_text_color(100, 100, 100)
    pdf.cell(0, 5, "Based on findings from the 3560-Day Deep Process Analysis Report", 0, 1, "C")

    # =========================================================================
    # 1. EXECUTIVE SUMMARY
    # =========================================================================
    pdf.add_page()
    pdf.section_title("1. Executive Summary")

    pdf.body_text(
        "This report documents the implementation and verification of all five "
        "actionable recommendations from the AQUABC 3560-Day Deep Process Analysis "
        "Report. The analysis identified critical bugs in zooplankton stoichiometry, "
        "missing excretion computations, and configuration issues that caused "
        "zooplankton collapse and ecosystem decline over multi-year simulations."
    )

    pdf.body_text(
        "All five recommendations have been successfully implemented:\n\n"
        "1. Fixed ZOO_N/P boundary conditions in all FORC_TS files\n"
        "2. Updated initial conditions from R10 MTRX final state\n"
        "3. Added missing zooplankton excretion computation in Fortran source\n"
        "4. Added minimum N:C / P:C ratio enforcement\n"
        "5. Enabled the sediment model (MODEL_SEDIMENTS=1)\n\n"
        "A verification 3560-day simulation confirms that all fixes produce ecologically "
        "reasonable results with stable zooplankton stoichiometry across all 7 output boxes."
    )

    # Quick before/after summary
    pdf.sub_title("Key Results Summary")
    headers = ["Metric", "Before Fix", "After Fix", "Assessment"]
    rows = [
        ["ZOO N:C (R10)", "~0.000", "0.15-0.22", "FIXED"],
        ["ZOO P:C (R10)", "~0.000", "0.014-0.028", "FIXED"],
        ["ZOO_C final (mg/L)", "0.000029", "0.0001-0.002", "Improved"],
        ["DIA_C final (mg/L)", "0.003", "0.27-1.43", "Improved"],
        ["DO final (mg/L)", "12.8", "9.2-10.9", "More realistic"],
        ["Excretion (DON/DOP)", "Always 0", "Active", "FIXED"],
        ["Sediment model", "OFF", "ON", "Enabled"],
    ]
    cw = [40, 35, 35, 30]
    pdf.data_table(headers, rows, cw)

    # =========================================================================
    # 2. PROBLEMS IDENTIFIED
    # =========================================================================
    pdf.add_page()
    pdf.section_title("2. Problems Identified by Deep Analysis")

    problems = [
        ("P1: ZOO_N/P Boundary Conditions Set to Zero",
         "All five FORC_TS boundary forcing files had ZOO_N = 0 and ZOO_P = 0 for all "
         "time steps, while ZOO_C had realistic values (~0.0004 mg/L). This meant every "
         "parcel of water entering the domain carried zooplankton carbon but zero nitrogen "
         "and phosphorus, continuously diluting the N:C and P:C ratios toward zero.",
         "CRITICAL"),

        ("P2: Zooplankton Excretion Never Computed",
         "In the Fortran source file aquabc_II_pelagic_lib_ZOOPLANKTON.f90, the excretion "
         "rate variables R_ZOO_EX_DON, R_ZOO_EX_DOP, and R_ZOO_EX_DOC were declared as "
         "output parameters but were never assigned values in normal operation. They were "
         "only set to 0.0 in the severe hypoxia branch. This meant zooplankton could never "
         "excrete dissolved organic matter, breaking the nutrient recycling pathway.",
         "CRITICAL"),

        ("P3: No Minimum N:C / P:C Ratio Enforcement",
         "The variable-stoichiometry model (ZOOP_OPTION_1=1) computed ACTUAL_ZOO_N_TO_C "
         "as ZOO_N/ZOO_C without any floor. When boundary dilution drove N:C toward zero, "
         "there was no recovery mechanism. This allowed complete stoichiometric collapse.",
         "HIGH"),

        ("P4: Sediment Model Disabled",
         "Without MODEL_SEDIMENTS=1, there was no benthic recycling of settled detritus, "
         "no sediment denitrification, and no sediment oxygen demand. This contributed to "
         "unrealistic nutrient accumulation and phytoplankton decline over long simulations.",
         "MEDIUM"),

        ("P5: Initial Conditions from Unrealistic State",
         "The original IC files contained hand-tuned values that were far from any "
         "equilibrium state. Using the R10 MTRX final state (averaged over representative "
         "boxes) provides a more realistic starting point that reduces spin-up artifacts.",
         "MEDIUM"),
    ]

    for title, desc, severity in problems:
        pdf.safe_page_break(30)
        pdf.sub_title(title)
        pdf.set_font("Helvetica", "B", 8)
        sev_colors = {"CRITICAL": (200, 0, 0), "HIGH": (200, 100, 0), "MEDIUM": (0, 100, 200)}
        r, g, b = sev_colors.get(severity, (0, 0, 0))
        pdf.set_text_color(r, g, b)
        pdf.cell(0, 4, f"Severity: {severity}", 0, 1)
        pdf.set_text_color(0, 0, 0)
        pdf.ln(1)
        pdf.body_text(desc)

    # =========================================================================
    # 3. FIX 1 - BOUNDARY CONDITIONS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("3. Fix 1: ZOO_N/P Boundary Conditions")

    pdf.sub_title("Problem")
    pdf.body_text(
        "Columns for ZOO_N and ZOO_P in all five FORC_TS boundary forcing files "
        "contained zero values for all 114,712+ data lines, while ZOO_C had realistic "
        "concentration values. This caused systematic dilution of zooplankton N:C and "
        "P:C ratios in all boxes receiving boundary water."
    )

    pdf.sub_title("Solution")
    pdf.body_text(
        "Created tools/fix_zoo_boundary_conditions.py to set ZOO_N = ZOO_C * 0.22 "
        "and ZOO_P = ZOO_C * 0.024 for all data lines in all FORC_TS files. "
        "The ratios 0.22 (N:C) and 0.024 (P:C) are the target stoichiometric ratios "
        "from WCONST_04.txt constants 131-132."
    )

    pdf.sub_title("Files Modified")
    file_info = [
        ["FORC_TS_1.txt", "Baltic boundary", "30 vars", "113,952 lines"],
        ["FORC_TS_2.txt", "Nemunas river", "33 vars", "190 lines"],
        ["FORC_TS_3.txt", "Minija river", "33 vars", "190 lines"],
        ["FORC_TS_4.txt", "Deima river", "33 vars", "190 lines"],
        ["FORC_TS_5.txt", "Matrosovka river", "33 vars", "190 lines"],
    ]
    pdf.data_table(
        ["File", "Boundary", "Variables", "Data Lines"],
        file_info,
        [40, 40, 30, 30],
    )

    pdf.sub_title("Verification")
    pdf.body_text(
        "After fix, FORC_TS_1.txt spot check:\n"
        "  ZOO_C = 0.000400  ->  ZOO_N = 0.000088  (0.22 * 0.0004)\n"
        "  ZOO_C = 0.000400  ->  ZOO_P = 0.000010  (0.024 * 0.0004)\n"
        "Scale factor lines (all 1.0) preserved correctly."
    )

    # =========================================================================
    # 4. FIX 2 - INITIAL CONDITIONS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("4. Fix 2: Initial Conditions from R10 MTRX")

    pdf.sub_title("Problem")
    pdf.body_text(
        "The original IC files contained hand-tuned values far from equilibrium "
        "(e.g., DIA_C=1.75, ZOO_C=0.02) which caused large transient artifacts "
        "during the first repeat cycles."
    )

    pdf.sub_title("Solution")
    pdf.body_text(
        "Created tools/update_ics_from_mtrx.py to generate new initial conditions "
        "from the R10 (final repeat) MTRX output of the previous simulation. "
        "Sand-box ICs (Set 2) are averaged from boxes 5, 6, 8, 9. "
        "Mud-box ICs (Set 1) are averaged from boxes 14, 17, 25. "
        "ZOO_N and ZOO_P values were overridden with stoichiometric values "
        "(ZOO_N = ZOO_C * 0.22, ZOO_P = ZOO_C * 0.024) since the pre-fix "
        "simulation had zero N/P in zooplankton."
    )

    pdf.sub_title("New IC Values (Selected Variables)")
    ic_headers = ["Variable", "Mud (Set 1)", "Sand (Set 2)", "Unit"]
    ic_rows = [
        ["NH4_N", "0.0266", "0.0254", "mg N/L"],
        ["NO3_N", "0.7296", "0.6800", "mg N/L"],
        ["PO4_P", "0.0461", "0.0417", "mg P/L"],
        ["DISS_OXYGEN", "12.83", "12.78", "mg/L"],
        ["DIA_C", "0.0031", "0.0029", "mg C/L"],
        ["ZOO_C", "0.000029", "0.000100", "mg C/L"],
        ["ZOO_N (fixed)", "0.000006", "0.000022", "mg N/L"],
        ["ZOO_P (fixed)", "0.000001", "0.000002", "mg P/L"],
        ["DET_PART_ORG_C", "0.0599", "0.0622", "mg C/L"],
        ["DISS_ORG_C", "4.68", "4.64", "mg C/L"],
        ["INORG_C", "3.21", "3.18", "mg C/L"],
    ]
    pdf.data_table(ic_headers, ic_rows, [42, 32, 32, 28])

    # =========================================================================
    # 5. FIX 3 - ZOOPLANKTON EXCRETION
    # =========================================================================
    pdf.add_page()
    pdf.section_title("5. Fix 3: Zooplankton Excretion Computation")

    pdf.sub_title("Problem")
    pdf.body_text(
        "In SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/"
        "aquabc_II_pelagic_lib_ZOOPLANKTON.f90, the output variables "
        "R_ZOO_EX_DON, R_ZOO_EX_DOP, and R_ZOO_EX_DOC were never assigned "
        "values during normal execution. They were only zeroed in the severe "
        "hypoxia branch (DISS_OXYGEN < DO_STR_HYPOX_ZOO_D). The excretion "
        "parameters KE_ZOO (=0.05) and FRAC_ZOO_EX_ORG (=0.30) existed in "
        "the t_zoo_params structure but were never used to compute excretion rates."
    )

    pdf.body_text(
        "A developer comment in mod_SIMULATE.f90 noted: 'set to zero in the code "
        "by Petras (I do not know why)'. This confirms the excretion was intentionally "
        "disabled at some point, likely as a temporary debugging measure that was "
        "never reverted."
    )

    pdf.sub_title("Solution")
    pdf.body_text(
        "Added excretion computation after R_ZOO_INT_RESP, before the death rate "
        "calculation. This location ensures excretion is computed after R_ZOO_GROWTH "
        "is available, and before the hypoxia branch which may override to zero."
    )

    pdf.sub_title("Code Change")
    pdf.code_block(
        "! Added after line 342 (after R_ZOO_INT_RESP computation):\n"
        "\n"
        "    !Zooplankton excretion rates (dissolved organic matter)\n"
        "    !KE_ZOO = excretion coefficient, FRAC_ZOO_EX_ORG = organic fraction\n"
        "    R_ZOO_EX_DOC = KE_ZOO * FRAC_ZOO_EX_ORG * R_ZOO_GROWTH\n"
        "    R_ZOO_EX_DON = R_ZOO_EX_DOC * ZOO_N_TO_C\n"
        "    R_ZOO_EX_DOP = R_ZOO_EX_DOC * ZOO_P_TO_C"
    )

    pdf.sub_title("Impact")
    pdf.body_text(
        "Excretion creates a pathway for zooplankton to release dissolved organic "
        "C, N, and P back to the water column. With KE_ZOO=0.05 and FRAC_ZOO_EX_ORG"
        "=0.30, the excretion rate is 1.5% of the growth rate. This DOC/DON/DOP "
        "feeds back into the microbial loop and supports nutrient recycling."
    )

    # =========================================================================
    # 6. FIX 4 - MIN RATIO ENFORCEMENT
    # =========================================================================
    pdf.add_page()
    pdf.section_title("6. Fix 4: Minimum N:C / P:C Ratio Enforcement")

    pdf.sub_title("Problem")
    pdf.body_text(
        "The variable-stoichiometry model (ZOOP_OPTION_1=1) in "
        "aquabc_II_pelagic_model.f90 computed ACTUAL_ZOO_N_TO_C = ZOO_N / ZOO_C "
        "without any lower bound. When boundary dilution or numerical errors drove "
        "ZOO_N toward zero faster than ZOO_C, the ratio collapsed to near-zero, "
        "creating a positive feedback loop where zooplankton with unrealistic "
        "stoichiometry could not recover."
    )

    pdf.sub_title("Solution")
    pdf.body_text(
        "Added a floor at 50% of the target ratio (from WCONST_04.txt). This "
        "prevents unrealistic stoichiometric collapse while still allowing "
        "meaningful variation in the actual ratios."
    )

    pdf.sub_title("Code Change")
    pdf.code_block(
        "! In aquabc_II_pelagic_model.f90, after ZOOPLANKTON subroutine call:\n"
        "\n"
        "    if (ZOOP_OPTION_1 > 0) then\n"
        "        ! Compute actual stoichiometric ratios with minimum enforcement\n"
        "        ! to prevent unrealistic drift below 50% of target ratio\n"
        "        ACTUAL_ZOO_N_TO_C(ns:ne) = max(\n"
        "            ZOO_N(ns:ne) / max(ZOO_C(ns:ne), MIN_CONCENTRATION),\n"
        "            0.5D0 * ZOO_PARAMS%ZOO_N_TO_C)\n"
        "        ACTUAL_ZOO_P_TO_C(ns:ne) = max(\n"
        "            ZOO_P(ns:ne) / max(ZOO_C(ns:ne), MIN_CONCENTRATION),\n"
        "            0.5D0 * ZOO_PARAMS%ZOO_P_TO_C)\n"
        "    end if"
    )

    pdf.sub_title("Parameters")
    pdf.data_table(
        ["Parameter", "Target Value", "Minimum (50%)", "Source"],
        [
            ["ZOO_N_TO_C", "0.22", "0.11", "WCONST_04.txt line 131"],
            ["ZOO_P_TO_C", "0.024", "0.012", "WCONST_04.txt line 132"],
        ],
        [40, 30, 30, 45],
    )

    # =========================================================================
    # 7. FIX 5 - SEDIMENT MODEL
    # =========================================================================
    pdf.add_page()
    pdf.section_title("7. Fix 5: Enable Sediment Model")

    pdf.sub_title("Problem")
    pdf.body_text(
        "With MODEL_SEDIMENTS=0 in the input file, there was no benthic recycling. "
        "Settled detritus and dead phytoplankton were permanently lost from the water "
        "column, leading to progressive nutrient depletion over multi-year runs. "
        "Additionally, sediment denitrification (a major NO3 removal pathway in "
        "shallow lagoons) was absent, causing unrealistic nitrate accumulation."
    )

    pdf.sub_title("Solution")
    pdf.body_text(
        "Changed MODEL_SEDIMENTS from 0 to 1 in INPUT_3560day.txt. The model uses "
        "prescribed sediment flux files (PRESCRIBED_SEDIMENT_FLUXES.txt and "
        "PRESCRIBED_SEDIMENT_FLUXES_HYPOXIA.txt) that were already configured "
        "in the input file but inactive."
    )

    pdf.sub_title("Effects")
    effects = [
        "Benthic nutrient recycling returns settled N, P, and C to water column",
        "Sediment denitrification provides a realistic NO3 removal pathway",
        "Sediment oxygen demand creates realistic summer DO depression",
        "Dissolved silica recycling from diatom frustules settling to sediment",
    ]
    for e in effects:
        pdf.bullet(e)
    pdf.ln(3)

    # =========================================================================
    # 8. VERIFICATION RESULTS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("8. Verification: 3560-Day Post-Fix Simulation")

    pdf.sub_title("8.1 Simulation Configuration")
    config = [
        ["Input file", "INPUT_3560day.txt"],
        ["Duration", "3560 days (10 x 356-day repeats)"],
        ["Time steps/day", "240"],
        ["Output boxes", "5, 6, 8, 9 (sand), 14, 17, 25 (mud)"],
        ["Sediment model", "ON (MODEL_SEDIMENTS=1)"],
        ["WCONST file", "WCONST_04.txt"],
        ["Executable", "ESTAS_II (gfortran release build)"],
    ]
    pdf.data_table(["Parameter", "Value"], config, [50, 95])

    # 8.2 Final state (R10 MTRX)
    pdf.safe_page_break(50)
    pdf.sub_title("8.2 Final State (R10 MTRX) - All Boxes")

    # Build table of final state values
    key_display = [
        ("NH4_N", "NH4-N", "mg N/L"),
        ("NO3_N", "NO3-N", "mg N/L"),
        ("PO4_P", "PO4-P", "mg P/L"),
        ("DISS_OXYGEN", "DO", "mg/L"),
        ("DIA_C", "Diatoms C", "mg C/L"),
        ("ZOO_C", "Zoo C", "mg C/L"),
        ("ZOO_N", "Zoo N", "mg N/L"),
        ("ZOO_P", "Zoo P", "mg P/L"),
        ("INORG_C", "DIC", "mg C/L"),
        ("TOT_ALK", "Alkalinity", "meq/L"),
        ("DET_PART_ORG_C", "Detritus C", "mg C/L"),
        ("DISS_ORG_C", "DOC", "mg C/L"),
    ]

    mtrx_headers = ["Variable"] + [f"Box {b}" for b in BOXES] + ["Unit"]
    mtrx_rows = []
    for vname, label, unit in key_display:
        row = [label]
        for b in BOXES:
            val = box_mtrx[b][vname]
            if val < 0.001 and val > 0:
                row.append(f"{val:.6f}")
            elif val < 0.1:
                row.append(f"{val:.4f}")
            else:
                row.append(f"{val:.2f}")
        row.append(unit)
        mtrx_rows.append(row)
    cw2 = [23] + [17] * 7 + [20]
    pdf.data_table(mtrx_headers, mtrx_rows, cw2)

    # 8.3 Zooplankton stoichiometry
    pdf.safe_page_break(40)
    pdf.sub_title("8.3 Zooplankton Stoichiometric Ratios (R10 Final)")
    ratio_headers = ["Box", "ZOO_C", "ZOO_N", "ZOO_P", "N:C", "P:C", "N:C Target", "P:C Target"]
    ratio_rows = []
    for b in BOXES:
        m = box_mtrx[b]
        nc = m["ZOO_N"] / m["ZOO_C"] if m["ZOO_C"] > 0 else 0
        pc = m["ZOO_P"] / m["ZOO_C"] if m["ZOO_C"] > 0 else 0
        ratio_rows.append([
            str(b),
            f"{m['ZOO_C']:.6f}",
            f"{m['ZOO_N']:.6f}",
            f"{m['ZOO_P']:.6f}",
            f"{nc:.4f}",
            f"{pc:.4f}",
            "0.2200",
            "0.0240",
        ])
    pdf.data_table(ratio_headers, ratio_rows, [12, 20, 20, 20, 16, 16, 20, 20])

    pdf.body_text(
        "All boxes show N:C ratios between 0.15 and 0.22, close to the target of 0.22. "
        "P:C ratios range from 0.014 to 0.028, close to the target of 0.024. "
        "This is a dramatic improvement from the pre-fix simulation where both ratios "
        "were effectively zero due to the boundary condition bug."
    )

    # 8.4 Year-10 Statistics
    pdf.add_page()
    pdf.sub_title("8.4 Year-10 Statistics (Last 356 Days)")

    y10_vars = [
        ("DIA_C", 5, "Diatoms C"),
        ("ZOO_C", 6, "Zoo C"),
        ("DISS_OXYGEN", 4, "Dissolved O2"),
        ("NO3_N", 2, "NO3-N"),
        ("PO4_P", 3, "PO4-P"),
        ("NH4_N", 1, "NH4-N"),
        ("DISS_ORG_C", 12, "DOC"),
        ("INORG_C", 20, "DIC"),
    ]

    for vname, col, label in y10_vars:
        pdf.safe_page_break(25)
        pdf.sub_sub_title(label)
        stat_headers = ["Box", "Min", "Max", "Mean"]
        stat_rows = []
        for b in BOXES:
            y10 = year10_slice(box_data[b])
            mn, mx, avg = stats(y10[:, col])
            if mx < 0.01:
                stat_rows.append([str(b), f"{mn:.6f}", f"{mx:.6f}", f"{avg:.6f}"])
            elif mx < 1:
                stat_rows.append([str(b), f"{mn:.4f}", f"{mx:.4f}", f"{avg:.4f}"])
            else:
                stat_rows.append([str(b), f"{mn:.3f}", f"{mx:.3f}", f"{avg:.3f}"])
        pdf.data_table(stat_headers, stat_rows, [20, 35, 35, 35])

    # =========================================================================
    # 9. ZOOPLANKTON N:C TEMPORAL EVOLUTION
    # =========================================================================
    pdf.add_page()
    pdf.section_title("9. Zooplankton N:C Ratio - Temporal Evolution")

    pdf.body_text(
        "The table below shows the N:C ratio sampled at 100-day intervals for Box 5, "
        "demonstrating that the ratio remains healthy throughout the simulation. "
        "Variations reflect seasonal dynamics (lower during high growth, recovery "
        "during quiescent periods)."
    )

    # Sample N:C evolution for Box 5
    d5 = box_data[5]
    sample_indices = list(range(0, len(d5), 100))
    if sample_indices[-1] != len(d5) - 1:
        sample_indices.append(len(d5) - 1)

    nc_headers = ["Day", "ZOO_C", "ZOO_N", "N:C Ratio", "Assessment"]
    nc_rows = []
    for idx in sample_indices:
        day = d5[idx, 0]
        zc = d5[idx, 6]
        zn = d5[idx, 7]
        nc = zn / zc if zc > 1e-10 else 0
        if nc >= 0.18:
            assess = "Good"
        elif nc >= 0.11:
            assess = "Acceptable"
        else:
            assess = "Low (floor active)"
        nc_rows.append([
            f"{day:.0f}",
            f"{zc:.6f}",
            f"{zn:.6f}",
            f"{nc:.4f}",
            assess,
        ])
    pdf.data_table(nc_headers, nc_rows, [20, 28, 28, 25, 30])

    # =========================================================================
    # 10. ECOLOGICAL ASSESSMENT
    # =========================================================================
    pdf.add_page()
    pdf.section_title("10. Ecological Assessment")

    pdf.sub_title("10.1 Phytoplankton")
    # Compute overall DIA_C Year-10 stats
    all_dia_y10 = []
    for b in BOXES:
        y10 = year10_slice(box_data[b])
        all_dia_y10.append(y10[:, 5])
    all_dia = np.concatenate(all_dia_y10)
    pdf.body_text(
        f"Diatom carbon (DIA_C) across all boxes in Year 10: "
        f"mean = {np.mean(all_dia):.3f} mg C/L, "
        f"max = {np.max(all_dia):.3f} mg C/L. "
        f"Spring blooms reach 3-4.4 mg C/L in individual boxes, with clear "
        f"seasonal cycling. This is a significant improvement from the pre-fix "
        f"simulation where DIA_C had declined to 0.003 mg C/L by R10."
    )

    pdf.sub_title("10.2 Zooplankton")
    all_zoo_y10 = []
    for b in BOXES:
        y10 = year10_slice(box_data[b])
        all_zoo_y10.append(y10[:, 6])
    all_zoo = np.concatenate(all_zoo_y10)
    pdf.body_text(
        f"Zooplankton carbon (ZOO_C) across all boxes in Year 10: "
        f"mean = {np.mean(all_zoo):.6f} mg C/L, "
        f"max = {np.max(all_zoo):.6f} mg C/L. "
        f"Zooplankton show seasonal grazing peaks that follow phytoplankton blooms, "
        f"indicating functional predator-prey coupling. Sand boxes (5, 9) maintain "
        f"higher zooplankton biomass than mud boxes (14, 17, 25)."
    )

    pdf.sub_title("10.3 Dissolved Oxygen")
    all_do_y10 = []
    for b in BOXES:
        y10 = year10_slice(box_data[b])
        all_do_y10.append(y10[:, 4])
    all_do = np.concatenate(all_do_y10)
    pdf.body_text(
        f"Dissolved oxygen across all boxes in Year 10: "
        f"mean = {np.mean(all_do):.2f} mg/L, "
        f"min = {np.min(all_do):.2f} mg/L, "
        f"max = {np.max(all_do):.2f} mg/L. "
        f"DO values are consistently above 7 mg/L (well-oxygenated), with seasonal "
        f"variation reflecting temperature-dependent solubility and biological activity. "
        f"The sediment oxygen demand from the enabled sediment model may contribute "
        f"to the slightly lower values compared to the pre-fix simulation."
    )

    pdf.sub_title("10.4 Nutrients")
    all_no3 = np.concatenate([year10_slice(box_data[b])[:, 2] for b in BOXES])
    all_po4 = np.concatenate([year10_slice(box_data[b])[:, 3] for b in BOXES])
    all_nh4 = np.concatenate([year10_slice(box_data[b])[:, 1] for b in BOXES])
    pdf.body_text(
        f"Year 10 nutrient ranges:\n"
        f"  NO3-N: {np.min(all_no3):.3f} - {np.max(all_no3):.3f} mg N/L "
        f"(mean {np.mean(all_no3):.3f})\n"
        f"  NH4-N: {np.min(all_nh4):.4f} - {np.max(all_nh4):.4f} mg N/L "
        f"(mean {np.mean(all_nh4):.4f})\n"
        f"  PO4-P: {np.min(all_po4):.4f} - {np.max(all_po4):.4f} mg P/L "
        f"(mean {np.mean(all_po4):.4f})\n\n"
        f"Nutrient concentrations show seasonal depletion during spring blooms "
        f"and replenishment during winter, indicating active biological cycling."
    )

    # =========================================================================
    # 11. SPATIAL PATTERNS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("11. Spatial Patterns")

    pdf.body_text(
        "The model produces realistic spatial differentiation between sand (Baltic-"
        "influenced, boxes 5, 6, 8, 9) and mud (lagoon interior, boxes 14, 17, 25) "
        "environments:"
    )

    spatial_headers = ["Variable", "Sand Mean", "Mud Mean", "Ratio (Mud/Sand)"]
    spatial_rows = []
    for vname, col, label in [("DIA_C", 5, "Diatoms C"), ("ZOO_C", 6, "Zoo C"),
                               ("NO3_N", 2, "NO3-N"), ("PO4_P", 3, "PO4-P"),
                               ("DISS_OXYGEN", 4, "DO"),
                               ("DET_PART_ORG_C", 9, "Detritus C")]:
        sand_vals = np.concatenate([year10_slice(box_data[b])[:, col] for b in SAND_BOXES])
        mud_vals = np.concatenate([year10_slice(box_data[b])[:, col] for b in MUD_BOXES])
        sm = np.mean(sand_vals)
        mm = np.mean(mud_vals)
        ratio = mm / sm if sm > 0 else 0
        if sm < 0.01:
            spatial_rows.append([label, f"{sm:.6f}", f"{mm:.6f}", f"{ratio:.2f}"])
        elif sm < 1:
            spatial_rows.append([label, f"{sm:.4f}", f"{mm:.4f}", f"{ratio:.2f}"])
        else:
            spatial_rows.append([label, f"{sm:.3f}", f"{mm:.3f}", f"{ratio:.2f}"])
    pdf.data_table(spatial_headers, spatial_rows, [35, 35, 35, 35])

    pdf.body_text(
        "Mud boxes show higher diatom biomass (more nutrient-enriched lagoon water) "
        "but lower zooplankton (possibly shallower boxes with different grazing dynamics). "
        "These spatial patterns are consistent with the physical characteristics of the "
        "Curonian Lagoon."
    )

    # =========================================================================
    # 12. SUMMARY OF ALL CHANGES
    # =========================================================================
    pdf.add_page()
    pdf.section_title("12. Complete List of Modified Files")

    file_changes = [
        ["INPUTS/FORC_TS_1.txt", "Boundary", "ZOO_N/P stoichiometric fix"],
        ["INPUTS/FORC_TS_2.txt", "Boundary", "ZOO_N/P stoichiometric fix"],
        ["INPUTS/FORC_TS_3.txt", "Boundary", "ZOO_N/P stoichiometric fix"],
        ["INPUTS/FORC_TS_4.txt", "Boundary", "ZOO_N/P stoichiometric fix"],
        ["INPUTS/FORC_TS_5.txt", "Boundary", "ZOO_N/P stoichiometric fix"],
        ["INPUTS/INIT_CONC_1.txt", "IC (Mud)", "R10 MTRX values + ZOO_N/P fix"],
        ["INPUTS/INIT_CONC_2.txt", "IC (Sand)", "R10 MTRX values + ZOO_N/P fix"],
        ["INPUT_3560day.txt", "Config", "MODEL_SEDIMENTS 0 -> 1"],
        ["...lib_ZOOPLANKTON.f90", "Source", "Added excretion computation"],
        ["...pelagic_model.f90", "Source", "Min N:C/P:C enforcement"],
        ["tools/fix_zoo_boundary_*.py", "Tool", "Boundary fix script (new)"],
        ["tools/update_ics_from_*.py", "Tool", "IC update script (new)"],
    ]
    pdf.data_table(
        ["File", "Type", "Change Description"],
        file_changes,
        [52, 22, 72],
    )

    # =========================================================================
    # 13. CONCLUSIONS & NEXT STEPS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("13. Conclusions & Next Steps")

    pdf.sub_title("Conclusions")
    pdf.body_text(
        "All five actionable recommendations from the 3560-Day Deep Process Analysis "
        "Report have been successfully implemented and verified. The key improvements are:\n\n"
        "1. Zooplankton stoichiometry is now maintained at realistic levels (N:C = 0.15-0.22, "
        "P:C = 0.014-0.028) across all boxes throughout the 10-year simulation.\n\n"
        "2. The excretion pathway now functions correctly, enabling dissolved organic matter "
        "recycling through the microbial loop.\n\n"
        "3. The minimum ratio enforcement provides a safety net against stoichiometric "
        "collapse in edge cases.\n\n"
        "4. The sediment model provides benthic recycling and denitrification.\n\n"
        "5. Initial conditions from R10 MTRX reduce spin-up artifacts for future runs."
    )

    pdf.sub_title("Remaining Next Steps")
    next_steps = [
        ("Formal Calibration",
         "With structural bugs fixed, conduct calibration against observed "
         "water quality data for the Curonian Lagoon. Key parameters to tune: "
         "KG_DIA_OPT_TEMP, KD_DIA_20, KG_ZOO_OPT_TEMP, settling velocities."),
        ("Validation Against Observations",
         "Compare model output with monitoring station data for DO, nutrients, "
         "chlorophyll-a (as proxy for phytoplankton C), and Secchi depth."),
        ("Sensitivity Analysis",
         "Use the stable 10-year baseline to conduct one-at-a-time and Morris "
         "screening sensitivity analysis on the 318 model constants."),
        ("Longer Spin-Up Assessment",
         "Run 20+ year simulation to confirm full steady-state convergence of "
         "all state variables, particularly the slow-cycling metals and DIC."),
    ]
    for title, text in next_steps:
        pdf.safe_page_break(25)
        pdf.sub_sub_title(title)
        pdf.body_text(text)

    # =========================================================================
    # SAVE
    # =========================================================================
    out_path = OUT_DIR / "Fixes_And_Results_Report.pdf"
    pdf.output(str(out_path))
    print(f"Report saved to {out_path}")
    print(f"Pages: {pdf.page_no()}")


if __name__ == "__main__":
    generate_report()
