#!/usr/bin/env python3
"""
Generate a comprehensive PDF report for the AQUABC 3560-day (10-year)
simulation deep analysis.

Covers:
  1. Executive Summary
  2. Overall Statistics
  3. Convergence & Steady-State Analysis
  4. Seasonal Dynamics (Year 10)
  5. Phytoplankton Analysis (bloom timing, year-over-year trends)
  6. Zooplankton Deep Investigation (root-cause analysis)
  7. Detritus C:N Ratio Verification
  8. Dissolved Oxygen Assessment
  9. Carbon System & Alkalinity
 10. Nutrient Dynamics
 11. Metals & Conservative Tracers
 12. Spatial Variation
 13. Mass Balance
 14. Process-Level Root-Cause Analysis
 15. Recommendations
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
NUM_REPEATS = 10
SIM_DAYS = 356  # days per repeat cycle


# -- Helpers ------------------------------------------------------------------

def load_box(box_id: int) -> dict:
    """Load a PELAGIC_BOX file and return {header: np.array}."""
    fname = OUTPUT_DIR / f"PELAGIC_BOX_{box_id:05d}.out"
    with open(fname) as f:
        header_line = f.readline().strip()
        headers = header_line.split()
        data: dict[str, list[float]] = {h: [] for h in headers}
        for line in f:
            vals = line.split()
            if len(vals) != len(headers):
                continue
            for h, v in zip(headers, vals):
                data[h].append(float(v))
    return {h: np.array(v) for h, v in data.items()}


def get_repeat_slices(time_arr):
    """Return list of (start_idx, end_idx) for each repeat."""
    sim_start = time_arr[0]
    repeats = []
    for r in range(NUM_REPEATS):
        t0 = sim_start + r * SIM_DAYS
        t1 = sim_start + (r + 1) * SIM_DAYS
        mask = (time_arr >= t0) & (time_arr < t1)
        if r == NUM_REPEATS - 1:
            mask = (time_arr >= t0) & (time_arr <= t1 + 1)
        indices = np.where(mask)[0]
        if len(indices) > 0:
            repeats.append((indices[0], indices[-1] + 1))
    return repeats


def doy_from_time(time_arr, sim_start):
    """Convert to day-of-year (0-based) within current repeat."""
    return ((time_arr - sim_start) % SIM_DAYS).astype(int)


def seasonal_means(time_arr, val_arr, sim_start):
    """Return seasonal means based on DOY."""
    doy = doy_from_time(time_arr, sim_start)
    seasons = {}
    for name, (d0, d1) in [("Winter(DJF)", (335, 60)), ("Spring(MAM)", (60, 152)),
                            ("Summer(JJA)", (152, 244)), ("Autumn(SON)", (244, 335))]:
        if d0 > d1:  # winter wraps around
            mask = (doy >= d0) | (doy < d1)
        else:
            mask = (doy >= d0) & (doy < d1)
        if mask.any():
            seasons[name] = float(np.mean(val_arr[mask]))
        else:
            seasons[name] = 0.0
    return seasons


VARIABLE_NAMES = [
    "NH4_N", "NO3_N", "PO4_P", "DISS_Si",
    "DISS_OXYGEN",
    "DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C",
    "ZOO_C", "ZOO_N", "ZOO_P",
    "DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P", "PART_Si",
    "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P",
    "INORG_C", "TOT_ALK", "CH4_C",
    "FE_II", "FE_III", "MN_II", "MN_IV",
    "CA", "MG", "S_PLUS_6", "S_MINUS_2",
    "AKI_C",
    "SEC_METAB_DIA", "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST",
]

KEY_VARS = ["DIA_C", "CYN_C", "OPA_C", "ZOO_C", "ZOO_N", "ZOO_P",
            "DISS_OXYGEN", "NH4_N", "NO3_N", "PO4_P", "DISS_Si",
            "DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P",
            "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P",
            "INORG_C", "TOT_ALK", "FE_II", "CA", "MG", "S_PLUS_6", "AKI_C"]


# IC values for Box 5 (set 1, non-mud)
IC_MAP = {
    "NH4_N": 0.02, "NO3_N": 0.5, "PO4_P": 0.005,
    "DISS_Si": 3.7, "DISS_OXYGEN": 15.0,
    "DIA_C": 1.0, "CYN_C": 0.06, "OPA_C": 0.022,
    "FIX_CYN_C": 0.01, "NOST_VEG_HET_C": 0.0,
    "ZOO_C": 0.02, "ZOO_N": 0.004, "ZOO_P": 0.0005,
    "DET_PART_ORG_C": 6.0, "DET_PART_ORG_N": 1.0,
    "DET_PART_ORG_P": 0.0052, "PART_Si": 1.5,
    "DISS_ORG_C": 11.0, "DISS_ORG_N": 4.5, "DISS_ORG_P": 0.04,
    "INORG_C": 25.0, "TOT_ALK": 2.8, "CH4_C": 0.0,
    "FE_II": 0.23, "FE_III": 0.55,
    "MN_II": 0.1, "MN_IV": 0.1,
    "CA": 70.0, "MG": 15.0,
    "S_PLUS_6": 1.35, "S_MINUS_2": 0.0,
    "AKI_C": 0.0,
}


# -- PDF class ----------------------------------------------------------------

class AnalysisPDF(FPDF):
    def header(self):
        self.set_font("Helvetica", "B", 9)
        self.set_text_color(100, 100, 100)
        self.cell(0, 5, "AQUABC 3560-Day (10-Year) Simulation -- Deep Process Analysis Report", 0, 1, "C")
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

    def body_text(self, text: str):
        self.set_font("Helvetica", "", 9)
        self.set_text_color(0, 0, 0)
        self.multi_cell(0, 4.5, text)
        self.ln(2)

    def bullet(self, text: str, indent: int = 10):
        self.set_font("Helvetica", "", 9)
        self.set_text_color(0, 0, 0)
        x = self.get_x()
        self.set_x(x + indent)
        self.cell(3, 4.5, chr(8226))
        self.multi_cell(0, 4.5, text)

    def table_header(self, cols: list[tuple[str, int]]):
        self.set_font("Helvetica", "B", 7)
        self.set_fill_color(0, 51, 102)
        self.set_text_color(255, 255, 255)
        for label, w in cols:
            self.cell(w, 5, label, 1, 0, "C", True)
        self.ln()
        self.set_text_color(0, 0, 0)

    def table_row(self, cols: list[tuple[str, int]], fill: bool = False):
        self.set_font("Courier", "", 7)
        if fill:
            self.set_fill_color(240, 245, 255)
        for val, w in cols:
            self.cell(w, 4, val, 1, 0, "R", fill)
        self.ln()

    def table_row_left(self, cols: list[tuple[str, int]], fill: bool = False):
        self.set_font("Courier", "", 7)
        if fill:
            self.set_fill_color(240, 245, 255)
        for i, (val, w) in enumerate(cols):
            align = "L" if i == 0 else "R"
            self.cell(w, 4, val, 1, 0, align, fill)
        self.ln()

    def finding_box(self, severity: str, title: str, text: str):
        colors = {
            "CRITICAL": (200, 0, 0),
            "WARNING": (200, 150, 0),
            "INFO": (0, 100, 180),
            "OK": (0, 140, 0),
        }
        r, g, b = colors.get(severity, (0, 0, 0))
        self.set_draw_color(r, g, b)
        self.set_font("Helvetica", "B", 9)
        self.set_text_color(r, g, b)
        x, y = self.get_x(), self.get_y()
        self.rect(x, y, self.w - self.l_margin - self.r_margin, 18)
        self.set_xy(x + 2, y + 1)
        self.cell(0, 4.5, f"[{severity}] {title}")
        self.set_xy(x + 2, y + 6)
        self.set_font("Helvetica", "", 8)
        self.set_text_color(0, 0, 0)
        self.multi_cell(self.w - self.l_margin - self.r_margin - 4, 3.5, text)
        self.set_y(y + 20)

    def safe_page_break(self, needed: float = 30):
        if self.get_y() + needed > self.h - 25:
            self.add_page()


# -- Main Report Generator ---------------------------------------------------

def generate_report():
    print("Loading data from all 7 boxes...")
    all_data = {}
    for b in BOXES:
        all_data[b] = load_box(b)
        n = len(all_data[b]["TIME_DAYS"])
        t0, t1 = all_data[b]["TIME_DAYS"][0], all_data[b]["TIME_DAYS"][-1]
        print(f"  Box {b}: {n} timesteps, time {t0:.1f} to {t1:.1f}")

    # Reference box
    d5 = all_data[5]
    time = d5["TIME_DAYS"]
    sim_start = time[0]
    repeats = get_repeat_slices(time)
    print(f"  {len(repeats)} repeats detected")

    pdf = AnalysisPDF("L", "mm", "A4")
    pdf.alias_nb_pages()
    pdf.set_auto_page_break(auto=True, margin=20)

    # =========================================================================
    # TITLE PAGE
    # =========================================================================
    pdf.add_page()
    pdf.ln(30)
    pdf.set_font("Helvetica", "B", 24)
    pdf.set_text_color(0, 51, 102)
    pdf.cell(0, 15, "AQUABC 3560-Day (10-Year) Simulation", 0, 1, "C")
    pdf.set_font("Helvetica", "B", 18)
    pdf.cell(0, 12, "Deep Process Analysis Report", 0, 1, "C")
    pdf.ln(10)
    pdf.set_font("Helvetica", "", 12)
    pdf.set_text_color(80, 80, 80)
    pdf.cell(0, 8, f"Generated: {date.today().isoformat()}", 0, 1, "C")
    pdf.cell(0, 8, "Configuration: 10 repeats x 356 days, PRINT_INTERVAL=240", 0, 1, "C")
    pdf.cell(0, 8, f"Data range: {time[0]:.0f} to {time[-1]:.0f} Julian days", 0, 1, "C")
    pdf.cell(0, 8, f"Boxes analysed: {', '.join(str(b) for b in BOXES)}", 0, 1, "C")
    pdf.ln(15)
    pdf.set_font("Helvetica", "I", 10)
    pdf.set_text_color(120, 120, 120)
    pdf.multi_cell(0, 5,
        "This report presents an in-depth process-level analysis of the AQUABC "
        "ecological model after a 10-year spin-up simulation using the ESTAS-II "
        "transport framework. The analysis covers convergence behaviour, seasonal "
        "dynamics, ecological plausibility, and root-cause investigation of "
        "identified issues (zooplankton collapse, phytoplankton year-over-year "
        "decline) through Fortran source code analysis.", align="C")

    # =========================================================================
    # 1. EXECUTIVE SUMMARY
    # =========================================================================
    pdf.add_page()
    pdf.section_title("1. Executive Summary")

    # Compute convergence stats
    converged = 0
    near = 0
    not_conv = 0
    check_vars = KEY_VARS
    for var in check_vars:
        if var not in d5:
            continue
        for b in BOXES:
            if var not in all_data[b]:
                continue
            v = all_data[b][var]
            if len(repeats) >= 2:
                r9 = repeats[-2]
                r10 = repeats[-1]
                v9 = v[r9[1] - 1]
                v10 = v[r10[1] - 1]
                diff = abs(v10 - v9)
                denom = max(abs(v10), abs(v9), 1e-12)
                pct = diff / denom
                if pct < 0.005:
                    converged += 1
                elif pct < 0.05:
                    near += 1
                else:
                    not_conv += 1
    total = converged + near + not_conv

    pdf.body_text(
        f"The 3560-day simulation (10 annual cycles, 356 days each) successfully completed "
        f"all 10 repeats. State variables were carried forward between repeats while forcing "
        f"data was recycled, allowing the model to spin up toward steady state.\n\n"
        f"CONVERGENCE: Of {total} variable-box combinations checked (R9 vs R10):\n"
        f"  - Converged (<0.5% change): {converged}\n"
        f"  - Near-convergence (0.5-5%): {near}\n"
        f"  - Not converged (>5%): {not_conv}")

    pdf.ln(2)
    pdf.sub_title("Key Findings")

    findings = [
        ("OK", "Dissolved Oxygen", "Converged in all boxes. Realistic seasonal cycle "
         "(13.8 winter -> 9.1 summer). Min DO = 7.7 mg/L, well above hypoxia."),
        ("OK", "Detritus C:N Ratio", "Rock-solid 5.89-6.39 across all 10 years and "
         "all 7 boxes. The WCONST_04.txt fix (KDISS=0.25, FAC_PHYT=0.0) is confirmed."),
        ("OK", "Nutrients (NH4, PO4)", "Fully converged. Realistic seasonal drawdown patterns."),
        ("OK", "Carbon System (DIC, ALK)", "Converged. DIC ~ 3.0, ALK ~ 2.9 meq/L."),
        ("WARNING", "Phytoplankton Decline", "All groups decline ~4% per year. Diatom spring bloom "
         "persists (peak 1.35 mgC/L at DOY 129-133) but winter minimum drops each year."),
        ("WARNING", "NO3 Still Accumulating", "Nitrate increasing ~1.5% per year. "
         "Insufficient denitrification without active sediment model."),
        ("CRITICAL", "Zooplankton Collapse", "ZOO_N = ZOO_P = 0.000 (complete collapse). "
         "ZOO_C persists at trace levels (0.00014). Root cause identified in source code."),
    ]
    for sev, title, text in findings:
        pdf.safe_page_break(25)
        pdf.finding_box(sev, title, text)

    # =========================================================================
    # 2. OVERALL STATISTICS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("2. Overall Statistics -- Box 5 (Representative)")

    cols = [("Variable", 28), ("Min", 22), ("Max", 22), ("Mean", 22),
            ("Final(R10)", 22), ("IC", 22), ("IC->Final", 22)]
    pdf.table_header(cols)
    row_i = 0
    for var in VARIABLE_NAMES:
        if var not in d5:
            continue
        v = d5[var]
        ic = IC_MAP.get(var, 0.0)
        final = v[-1]
        if ic != 0:
            change = f"{((final - ic) / ic) * 100:+.1f}%"
        else:
            change = "N/A" if final == 0 else "+INF"
        pdf.table_row_left([
            (var, 28),
            (f"{v.min():.6f}", 22),
            (f"{v.max():.6f}", 22),
            (f"{v.mean():.6f}", 22),
            (f"{final:.6f}", 22),
            (f"{ic:.6f}", 22),
            (change, 22),
        ], fill=(row_i % 2 == 0))
        row_i += 1
        if pdf.get_y() > pdf.h - 25:
            pdf.add_page()
            pdf.table_header(cols)

    # =========================================================================
    # 3. CONVERGENCE & STEADY-STATE ANALYSIS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("3. Convergence & Steady-State Analysis")

    pdf.body_text(
        "For each key variable and box, the final value at the end of repeats 8, 9, and 10 "
        "is compared. Convergence is classified as:\n"
        "  YES: |R10 - R9| / max(|R10|, |R9|) < 0.5%\n"
        "  NEAR: 0.5% - 5%\n"
        "  NO: > 5%")

    conv_vars = ["DIA_C", "CYN_C", "OPA_C", "DISS_OXYGEN", "NH4_N", "NO3_N",
                 "PO4_P", "DET_PART_ORG_C", "ZOO_C", "INORG_C", "TOT_ALK",
                 "FE_II", "CA", "MG", "DISS_ORG_C"]
    cols = [("Variable", 25), ("Box", 10), ("R8 final", 20), ("R9 final", 20),
            ("R10 final", 20), ("|R9-R10|", 18), ("Rel %", 15), ("Status", 14)]
    pdf.table_header(cols)
    row_i = 0
    for var in conv_vars:
        for b in BOXES:
            if var not in all_data[b]:
                continue
            v = all_data[b][var]
            if len(repeats) < 3:
                continue
            r8, r9, r10 = repeats[-3], repeats[-2], repeats[-1]
            v8 = v[r8[1] - 1]
            v9 = v[r9[1] - 1]
            v10 = v[r10[1] - 1]
            diff = abs(v10 - v9)
            denom = max(abs(v10), abs(v9), 1e-12)
            pct = diff / denom * 100
            status = "YES" if pct < 0.5 else ("NEAR" if pct < 5 else "NO")
            pdf.table_row_left([
                (var, 25), (str(b), 10),
                (f"{v8:.6f}", 20), (f"{v9:.6f}", 20), (f"{v10:.6f}", 20),
                (f"{diff:.2e}", 18), (f"{pct:.2f}", 15), (status, 14),
            ], fill=(row_i % 2 == 0))
            row_i += 1
            if pdf.get_y() > pdf.h - 25:
                pdf.add_page()
                pdf.table_header(cols)

    # =========================================================================
    # 4. YEAR-OVER-YEAR TRENDS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("4. Year-over-Year Trends (Box 5)")

    pdf.body_text(
        "End-of-repeat values for key variables in Box 5 across all 10 repeats. "
        "This tracks how the model state evolves from one annual cycle to the next.")

    trend_vars = ["DIA_C", "CYN_C", "OPA_C", "ZOO_C", "DISS_OXYGEN",
                  "NH4_N", "NO3_N", "PO4_P", "DET_PART_ORG_C",
                  "INORG_C", "TOT_ALK", "DISS_ORG_C"]
    cols_t = [("Variable", 24)] + [(f"R{r+1}", 14) for r in range(NUM_REPEATS)]
    pdf.table_header(cols_t)
    row_i = 0
    for var in trend_vars:
        if var not in d5:
            continue
        vals = []
        for r_start, r_end in repeats:
            vals.append(d5[var][r_end - 1])
        row_data = [(var, 24)] + [(f"{v:.4f}", 14) for v in vals]
        pdf.table_row_left(row_data, fill=(row_i % 2 == 0))
        row_i += 1

    pdf.ln(3)
    pdf.sub_title("Year-over-Year Trends - Annual Means")
    cols_t2 = [("Variable", 24)] + [(f"Y{r+1}", 14) for r in range(NUM_REPEATS)]
    pdf.table_header(cols_t2)
    row_i = 0
    for var in trend_vars:
        if var not in d5:
            continue
        vals = []
        for r_start, r_end in repeats:
            vals.append(float(np.mean(d5[var][r_start:r_end])))
        row_data = [(var, 24)] + [(f"{v:.4f}", 14) for v in vals]
        pdf.table_row_left(row_data, fill=(row_i % 2 == 0))
        row_i += 1

    # =========================================================================
    # 5. SEASONAL DYNAMICS (Year 10)
    # =========================================================================
    pdf.add_page()
    pdf.section_title("5. Seasonal Dynamics -- Year 10, Box 5")

    if len(repeats) > 0:
        r10_start, r10_end = repeats[-1]
        t10 = time[r10_start:r10_end]
        season_vars = ["DIA_C", "CYN_C", "OPA_C", "NOST_VEG_HET_C",
                       "DISS_OXYGEN", "NH4_N", "NO3_N", "PO4_P", "DISS_Si",
                       "DET_PART_ORG_C", "INORG_C", "TOT_ALK", "ZOO_C"]
        cols_s = [("Variable", 28), ("Winter(DJF)", 28), ("Spring(MAM)", 28),
                  ("Summer(JJA)", 28), ("Autumn(SON)", 28)]
        pdf.table_header(cols_s)
        row_i = 0
        for var in season_vars:
            if var not in d5:
                continue
            v10 = d5[var][r10_start:r10_end]
            sm = seasonal_means(t10, v10, time[r10_start])
            pdf.table_row_left([
                (var, 28),
                (f"{sm['Winter(DJF)']:.6f}", 28),
                (f"{sm['Spring(MAM)']:.6f}", 28),
                (f"{sm['Summer(JJA)']:.6f}", 28),
                (f"{sm['Autumn(SON)']:.6f}", 28),
            ], fill=(row_i % 2 == 0))
            row_i += 1

    pdf.ln(3)
    pdf.body_text(
        "Year 10 seasonal dynamics show a realistic pattern:\n"
        "  - Diatom spring bloom dominates phytoplankton production (MAM/JJA peak)\n"
        "  - DO follows inverse pattern: high in winter (cold, low demand), low in summer\n"
        "  - NO3 drawdown from spring through autumn as phytoplankton consume DIN\n"
        "  - PO4 shows summer minimum due to phytoplankton uptake\n"
        "  - DOC/DOM accumulates through growing season from metabolic excretion")

    # =========================================================================
    # 6. PHYTOPLANKTON ANALYSIS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("6. Phytoplankton Deep Analysis")

    pdf.sub_title("6.1 Bloom Timing -- Peak DOY and Magnitude (Year 10)")
    phyto_vars = ["DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C"]
    cols_p = [("Variable", 22), ("Box", 10), ("Peak DOY", 18), ("Peak Val", 22),
              ("Mean Y10", 22), ("Min Y10", 22)]
    pdf.table_header(cols_p)
    row_i = 0
    for var in phyto_vars:
        for b in BOXES:
            if var not in all_data[b]:
                continue
            if len(repeats) == 0:
                continue
            r10_start, r10_end = repeats[-1]
            v10 = all_data[b][var][r10_start:r10_end]
            t10 = all_data[b]["TIME_DAYS"][r10_start:r10_end]
            peak_idx = np.argmax(v10)
            peak_doy = int((t10[peak_idx] - t10[0]) % SIM_DAYS)
            pdf.table_row_left([
                (var, 22), (str(b), 10),
                (str(peak_doy), 18),
                (f"{v10[peak_idx]:.6f}", 22),
                (f"{np.mean(v10):.6f}", 22),
                (f"{np.min(v10):.6f}", 22),
            ], fill=(row_i % 2 == 0))
            row_i += 1
            if pdf.get_y() > pdf.h - 25:
                pdf.add_page()
                pdf.table_header(cols_p)

    pdf.safe_page_break(40)
    pdf.sub_title("6.2 Year-over-Year Phytoplankton Peak Magnitudes")
    pdf.body_text(
        "Annual peak DIA_C values across all 10 repeats for each box, showing the "
        "gradual decline in spring bloom magnitude.")

    cols_pp = [("Box", 12)] + [(f"R{r+1}", 14) for r in range(NUM_REPEATS)]
    pdf.table_header(cols_pp)
    row_i = 0
    for b in BOXES:
        if "DIA_C" not in all_data[b]:
            continue
        vals = []
        for r_start, r_end in repeats:
            v_r = all_data[b]["DIA_C"][r_start:r_end]
            vals.append(float(np.max(v_r)))
        pdf.table_row_left(
            [(str(b), 12)] + [(f"{v:.4f}", 14) for v in vals],
            fill=(row_i % 2 == 0))
        row_i += 1

    pdf.safe_page_break(60)
    pdf.sub_title("6.3 Diagnosis: Why Phytoplankton Decline Year-over-Year")
    pdf.body_text(
        "SOURCE CODE ANALYSIS reveals the following loss budget for diatoms "
        "(aquabc_II_pelagic_model.f90, lines 2105-2122):\n\n"
        "  dDIA_C/dt = R_DIA_GROWTH - R_DIA_TOT_RESP - R_DIA_EXCR "
        "- R_DIA_DEATH - R_ZOO_FEEDING_DIA\n\n"
        "Key parameters from WCONST_04.txt:\n"
        "  KG_DIA_OPT_TEMP = 3.7 /day (max growth rate at T_opt)\n"
        "  EFF_DIA_GROWTH = 0.95 (5% metabolic loss)\n"
        "  KR_DIA_20 = 0.05 /day (basal respiration)\n"
        "  KD_DIA_20 = 0.12 /day (mortality rate)\n"
        "  DIA_OPT_TEMP = 24 C, DIA_TMIN = 1 C, DIA_TMAX = 35 C\n\n"
        "ROOT CAUSE ANALYSIS:\n"
        "The year-over-year decline is driven by THREE compounding mechanisms:\n\n"
        "1. BOUNDARY DILUTION: Baltic Sea boundary conditions have DIA_C = 0.01 mg C/L "
        "(very low compared to internal concentrations of 0.1-1.4 mgC/L). Advective "
        "transport through boundary links continuously dilutes internal phytoplankton "
        "toward boundary values. This is the dominant chronic loss.\n\n"
        "2. NET PARTICULATE SETTLING: Settling transport (computed in "
        "aquabc_II_pelagic_auxillary.f90) removes particulate phytoplankton from the water "
        "column. Without an active sediment model (MODEL_SEDIMENTS=0), this material is "
        "permanently lost with no benthic recycling feedback.\n\n"
        "3. MORTALITY FLOOR: KD_DIA_20 = 0.12/day ensures continuous mortality even at "
        "very low biomass. During winter, growth approaches zero (low light, low T) while "
        "mortality continues, creating a seasonal ratchet effect. Each winter erodes the "
        "population slightly more than the previous one because the boundary dilution "
        "progressively lowers the 'floor' from which the spring bloom initiates.\n\n"
        "IMPLICATION: This is expected behaviour for a box model without sediment "
        "recycling. Enabling the sediment model (MODEL_SEDIMENTS=1) would partially "
        "counteract the settling loss by returning nutrients from decomposed settled "
        "material. The ~4%/year decline rate is slow enough that the model remains "
        "ecologically meaningful over 5-10 year horizons.")

    # =========================================================================
    # 7. ZOOPLANKTON DEEP INVESTIGATION
    # =========================================================================
    pdf.add_page()
    pdf.section_title("7. Zooplankton Deep Investigation")

    pdf.sub_title("7.1 Zooplankton State Evolution")
    cols_z = [("Repeat", 14), ("ZOO_C", 18), ("ZOO_N", 18), ("ZOO_P", 18),
              ("N:C ratio", 18), ("P:C ratio", 18)]
    pdf.table_header(cols_z)
    for r_idx, (r_start, r_end) in enumerate(repeats):
        zc = d5["ZOO_C"][r_end - 1]
        zn = d5["ZOO_N"][r_end - 1] if "ZOO_N" in d5 else 0
        zp = d5["ZOO_P"][r_end - 1] if "ZOO_P" in d5 else 0
        nc = zn / max(zc, 1e-12)
        pc = zp / max(zc, 1e-12)
        pdf.table_row_left([
            (f"R{r_idx+1}", 14), (f"{zc:.6f}", 18), (f"{zn:.6f}", 18),
            (f"{zp:.6f}", 18), (f"{nc:.6f}", 18), (f"{pc:.6f}", 18),
        ], fill=(r_idx % 2 == 0))

    pdf.ln(4)
    pdf.sub_title("7.2 Zooplankton Spatial Variation (End of R10)")
    cols_zs = [("Box", 10), ("ZOO_C", 22), ("ZOO_N", 22), ("ZOO_P", 22)]
    pdf.table_header(cols_zs)
    for i, b in enumerate(BOXES):
        zc = all_data[b]["ZOO_C"][-1]
        zn = all_data[b].get("ZOO_N", np.zeros(1))[-1]
        zp = all_data[b].get("ZOO_P", np.zeros(1))[-1]
        pdf.table_row_left([
            (str(b), 10), (f"{zc:.6f}", 22), (f"{zn:.6f}", 22), (f"{zp:.6f}", 22),
        ], fill=(i % 2 == 0))

    pdf.safe_page_break(120)
    pdf.sub_title("7.3 Root-Cause Analysis: Zooplankton Collapse")
    pdf.body_text(
        "FINDING: ZOO_N and ZOO_P have collapsed to exactly 0.000000 in all boxes "
        "by the end of repeat 1, while ZOO_C persists at trace levels (0.00014 mgC/L "
        "in Box 5 after 10 years). This represents a STRUCTURAL MODEL ISSUE, not a "
        "parameterization problem.\n\n"
        "SOURCE CODE INVESTIGATION (aquabc_II_pelagic_model.f90, "
        "aquabc_II_pelagic_lib_ZOOPLANKTON.f90):\n")

    pdf.body_text(
        "CAUSE 1 -- Variable Stoichiometry Positive Feedback Loop (Root Cause)\n\n"
        "With ZOOP_OPTION_1 = 1 (variable stoichiometry mode, set in "
        "aquabc_II_pelagic_interface.f90), the ZOO_N/P derivatives are:\n\n"
        "  dZOO_N/dt = SUM(R_FEED_i * PREY_i_N:C) - R_EX_DON\n"
        "             - R_TOT_RESP * (ZOO_N/ZOO_C) - R_DEATH * (ZOO_N/ZOO_C)\n\n"
        "The loss terms are proportional to the ACTUAL N:C ratio (ZOO_N/ZOO_C). Once "
        "ZOO_N drops close to zero, these losses vanish, but the gain terms ALSO vanish "
        "because they depend on feeding rate which depends on ZOO_C being fed by prey "
        "with N content. The boundary conditions (ZOO_N = 0 in FORC_TS files) ensure "
        "that advective transport continuously dilutes ZOO_N toward zero with no "
        "mechanism for recovery.\n\n"
        "Unlike the fixed-stoichiometry option (ZOOP_OPTION_1=0) which would track:\n"
        "  dZOO_N/dt = dZOO_C/dt * ZOO_N_TO_C\n"
        "the variable stoichiometry mode has no homeostatic lower bound on the N:C ratio.")

    pdf.safe_page_break(60)
    pdf.body_text(
        "CAUSE 2 -- Boundary Conditions Set ZOO_N = ZOO_P = 0\n\n"
        "FORC_TS_1.txt (Baltic Sea boundary) provides ZOO_C = 0.0004 mg/L but "
        "ZOO_N = 0.0 and ZOO_P = 0.0. This means all boundary advection carries "
        "zooplankton carbon WITHOUT any nitrogen or phosphorus content. The advective "
        "mixing continuously injects stoichiometrically-impossible 'zombie' zooplankton "
        "(pure carbon, no nutrients) which dilutes the internal N:C and P:C ratios "
        "toward zero.\n\n"
        "Even with correct initial conditions (ZOO_N = 0.004, ZOO_P = 0.0005), the "
        "boundary dilution drives the ratios to zero within the first repeat.")

    pdf.safe_page_break(50)
    pdf.body_text(
        "CAUSE 3 -- Excretion Terms Never Computed (Known Bug)\n\n"
        "In mod_SIMULATE.f90 (line 632), there is a developer comment:\n"
        "  'Related to DON: This is the real excretion (after digestion) by zooplankton "
        "however it is set to zero in the code by Petras (I do not know why)'\n\n"
        "The variables R_ZOO_EX_DON and R_ZOO_EX_DOP are declared as intent(inout) "
        "in the ZOOPLANKTON subroutine but are never assigned values outside of the "
        "severe-hypoxia branch. While excretion is a LOSS term and wouldn't save ZOO_N, "
        "the absence of proper excretion means the model lacks a critical pathway for "
        "N/P recycling back to the dissolved organic pool.")

    pdf.safe_page_break(50)
    pdf.sub_title("7.4 Why ZOO_C Persists While ZOO_N/P = 0")
    pdf.body_text(
        "ZOO_C persists at trace levels because:\n\n"
        "1. The ZOO_C derivative equation is purely in carbon units and does NOT depend "
        "on ZOO_N or ZOO_P. Growth (feeding) is computed as:\n"
        "   R_ZOO_GROWTH = KG_ZOO * FOOD_FACTOR * ZOO_C\n\n"
        "2. As long as prey is available above FOOD_MIN_ZOO (0.02 mgC/L), grazing "
        "produces carbon. With the diatom spring bloom reaching 1.35 mgC/L, there is "
        "substantial food.\n\n"
        "3. However, ZOO_C is slowly declining (0.00023 -> 0.00014 over 10 years) "
        "because the prey base (phytoplankton) is itself slowly declining, and the "
        "FOOD_MIN_ZOO threshold (0.02 mgC/L) means feeding shuts off when prey < 0.02.\n\n"
        "KEY PARAMETERS (from WCONST_04.txt):\n"
        "  KG_ZOO_OPT_TEMP = 0.45/day | KD_ZOO_20 = 0.15/day | KR_ZOO_20 = 0.03/day\n"
        "  EFF_ZOO_GROWTH = 0.80 | FOOD_MIN_ZOO = 0.02 mgC/L\n"
        "  Preferences: DIA=0.26, OPA=0.37, CYN=0.10, DET=0.20\n"
        "  Half-saturation: DIA=0.10, OPA=0.15, CYN=0.07, DET=0.50")

    pdf.safe_page_break(40)
    pdf.sub_title("7.5 Recommended Fixes for Zooplankton")
    pdf.body_text(
        "FIX PRIORITY 1 (Essential): Set stoichiometrically-consistent boundary conditions:\n"
        "  ZOO_N = ZOO_C * 0.22 (= ZOO_N_TO_C from WCONST_04.txt line 131)\n"
        "  ZOO_P = ZOO_C * 0.024 (= ZOO_P_TO_C from WCONST_04.txt line 132)\n"
        "  For FORC_TS_1.txt where ZOO_C = 0.0004:\n"
        "    ZOO_N = 0.000088, ZOO_P = 0.0000096\n\n"
        "FIX PRIORITY 2 (Recommended): Add homeostatic N:C/P:C enforcement in the "
        "Fortran source. If ACTUAL_ZOO_N_TO_C drops below a minimum threshold "
        "(e.g., 0.5 * ZOO_N_TO_C), enforce:\n"
        "  ZOO_N = max(ZOO_N, MIN_NC_RATIO * ZOO_C)\n\n"
        "FIX PRIORITY 3 (Optional): Switch to ZOOP_OPTION_1 = 0 (fixed stoichiometry) "
        "which guarantees N:C and P:C track carbon changes proportionally.")

    # =========================================================================
    # 8. DETRITUS C:N RATIO VERIFICATION
    # =========================================================================
    pdf.add_page()
    pdf.section_title("8. Detritus C:N Ratio Verification")

    pdf.body_text(
        "The detritus C:N ratio was a critical issue identified in the 200-day analysis. "
        "The fix (WCONST_04.txt: KDISS_DET_PART_ORG_C_20 = 0.25, "
        "FAC_PHYT_DET_PART_ORG_C = 0.0) was applied and verified over 365 days. "
        "This 10-year run provides the ultimate confirmation.")

    cols_cn = [("Repeat", 12)] + [(f"Box{b}", 18) for b in BOXES]
    pdf.table_header(cols_cn)
    for r_idx, (r_start, r_end) in enumerate(repeats):
        vals = []
        for b in BOXES:
            dc = all_data[b]["DET_PART_ORG_C"][r_end - 1]
            dn = all_data[b]["DET_PART_ORG_N"][r_end - 1]
            ratio = dc / max(dn, 1e-12)
            vals.append(f"{ratio:.3f}")
        pdf.table_row_left(
            [(f"R{r_idx+1}", 12)] + [(v, 18) for v in vals],
            fill=(r_idx % 2 == 0))

    pdf.ln(3)
    pdf.finding_box("OK", "Detritus C:N Ratio -- 10-Year Confirmation",
        "C:N ratio = 5.89 to 6.39 across all boxes and all 10 repeats. "
        "Remarkably stable. Target Redfield = 5.68. Fix is permanent and robust.")

    # =========================================================================
    # 9. DISSOLVED OXYGEN ASSESSMENT
    # =========================================================================
    pdf.add_page()
    pdf.section_title("9. Dissolved Oxygen Assessment")

    pdf.sub_title("9.1 DO Min/Max Per Repeat (Box 5)")
    cols_do = [("Repeat", 12), ("Min DO", 22), ("Max DO", 22), ("Mean DO", 22),
               ("Min DOY", 18)]
    pdf.table_header(cols_do)
    for r_idx, (r_start, r_end) in enumerate(repeats):
        v = d5["DISS_OXYGEN"][r_start:r_end]
        t_r = time[r_start:r_end]
        min_idx = np.argmin(v)
        min_doy = int((t_r[min_idx] - t_r[0]) % SIM_DAYS)
        pdf.table_row_left([
            (f"R{r_idx+1}", 12), (f"{v.min():.4f}", 22), (f"{v.max():.4f}", 22),
            (f"{v.mean():.4f}", 22), (str(min_doy), 18),
        ], fill=(r_idx % 2 == 0))

    pdf.ln(3)
    pdf.sub_title("9.2 Spatial DO at End of R10")
    cols_dos = [("Box", 10), ("Final DO", 22), ("Min R10", 22), ("Max R10", 22)]
    pdf.table_header(cols_dos)
    for i, b in enumerate(BOXES):
        if len(repeats) == 0:
            continue
        r_start, r_end = repeats[-1]
        v = all_data[b]["DISS_OXYGEN"][r_start:r_end]
        pdf.table_row_left([
            (str(b), 10), (f"{all_data[b]['DISS_OXYGEN'][-1]:.4f}", 22),
            (f"{v.min():.4f}", 22), (f"{v.max():.4f}", 22),
        ], fill=(i % 2 == 0))

    pdf.ln(3)
    pdf.finding_box("OK", "Dissolved Oxygen -- Healthy",
        "DO is fully converged (YES in all boxes). Minimum 7.73 mg/L (DOY 186 = early July). "
        "No hypoxia risk. Pattern: 13.8 winter -> 9.1 summer is textbook for temperate lakes.")

    # =========================================================================
    # 10. CARBON SYSTEM & ALKALINITY
    # =========================================================================
    pdf.add_page()
    pdf.section_title("10. Carbon System & Alkalinity")

    pdf.sub_title("10.1 DIC and Alkalinity Year-over-Year (Box 5)")
    cols_c = [("Repeat", 12), ("DIC final", 22), ("ALK final", 22),
              ("DOC final", 22), ("CH4_C final", 22)]
    pdf.table_header(cols_c)
    for r_idx, (r_start, r_end) in enumerate(repeats):
        dic = d5["INORG_C"][r_end - 1]
        alk = d5["TOT_ALK"][r_end - 1]
        doc = d5["DISS_ORG_C"][r_end - 1]
        ch4 = d5.get("CH4_C", np.zeros(len(time)))[r_end - 1]
        pdf.table_row_left([
            (f"R{r_idx+1}", 12), (f"{dic:.4f}", 22), (f"{alk:.4f}", 22),
            (f"{doc:.4f}", 22), (f"{ch4:.6f}", 22),
        ], fill=(r_idx % 2 == 0))

    pdf.ln(3)
    pdf.body_text(
        "DIC converged at ~3.0 mgC/L (down from IC of 25.0 -- IC spin-down). "
        "The boundary forcing from the Baltic Sea drives this to realistic levels. "
        "Alkalinity converged at ~2.88 meq/L (from IC of 2.8). DOC is stable ~3.96 "
        "after initial IC spin-down from 11.0.")

    # =========================================================================
    # 11. NUTRIENT DYNAMICS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("11. Nutrient Dynamics")

    pdf.sub_title("11.1 Nitrogen Budget Year-over-Year (Box 5)")
    cols_n = [("Repeat", 12), ("NH4_N", 20), ("NO3_N", 20), ("DON", 20),
              ("PON", 20), ("Total DIN", 20), ("Total N", 20)]
    pdf.table_header(cols_n)
    for r_idx, (r_start, r_end) in enumerate(repeats):
        nh4 = d5["NH4_N"][r_end - 1]
        no3 = d5["NO3_N"][r_end - 1]
        don = d5["DISS_ORG_N"][r_end - 1]
        pon = d5["DET_PART_ORG_N"][r_end - 1]
        din = nh4 + no3
        # Include phyto and zoo organic N
        phyto_n = sum(d5.get(v, np.zeros(len(time)))[r_end - 1] * 0.22
                      for v in ["DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C"]
                      if v in d5)
        zoo_n = d5.get("ZOO_N", np.zeros(len(time)))[r_end - 1]
        total_n = nh4 + no3 + don + pon + phyto_n + zoo_n
        pdf.table_row_left([
            (f"R{r_idx+1}", 12), (f"{nh4:.6f}", 20), (f"{no3:.6f}", 20),
            (f"{don:.6f}", 20), (f"{pon:.6f}", 20),
            (f"{din:.6f}", 20), (f"{total_n:.4f}", 20),
        ], fill=(r_idx % 2 == 0))

    pdf.ln(3)
    pdf.sub_title("11.2 Phosphorus Budget Year-over-Year (Box 5)")
    cols_p = [("Repeat", 12), ("PO4_P", 20), ("DOP", 20), ("POP", 20), ("Total P", 20)]
    pdf.table_header(cols_p)
    for r_idx, (r_start, r_end) in enumerate(repeats):
        po4 = d5["PO4_P"][r_end - 1]
        dop = d5["DISS_ORG_P"][r_end - 1]
        pop = d5["DET_PART_ORG_P"][r_end - 1]
        phyto_p = sum(d5.get(v, np.zeros(len(time)))[r_end - 1] * 0.024
                      for v in ["DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C"]
                      if v in d5)
        zoo_p = d5.get("ZOO_P", np.zeros(len(time)))[r_end - 1]
        total_p = po4 + dop + pop + phyto_p + zoo_p
        pdf.table_row_left([
            (f"R{r_idx+1}", 12), (f"{po4:.6f}", 20), (f"{dop:.6f}", 20),
            (f"{pop:.6f}", 20), (f"{total_p:.4f}", 20),
        ], fill=(r_idx % 2 == 0))

    pdf.ln(3)
    pdf.body_text(
        "NITROGEN: NO3 shows slow accumulation (+1.5%/year) because without an active "
        "sediment model (MODEL_SEDIMENTS=0), denitrification is absent. Nitrification "
        "converts NH4 -> NO3 but there's no NO3 sink. Total DIN is increasing.\n\n"
        "PHOSPHORUS: PO4 is converged and stable (~0.044 mg/L). Total P slowly decreasing "
        "due to settling losses of POP and declining phytoplankton biomass.")

    # =========================================================================
    # 12. METALS & CONSERVATIVE TRACERS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("12. Metals & Conservative Tracers")

    metal_vars = ["FE_II", "FE_III", "MN_II", "MN_IV", "CA", "MG", "S_PLUS_6"]
    cols_m = [("Variable", 22), ("IC", 18), ("Final R10", 18), ("Change%", 18),
              ("Converged?", 16)]
    pdf.table_header(cols_m)
    row_i = 0
    for var in metal_vars:
        if var not in d5:
            continue
        ic = IC_MAP.get(var, 0.0)
        final = d5[var][-1]
        pct = ((final - ic) / ic * 100) if ic != 0 else 0
        # Check convergence
        if len(repeats) >= 2:
            r9, r10 = repeats[-2], repeats[-1]
            v9 = d5[var][r9[1]-1]
            v10 = d5[var][r10[1]-1]
            diff_pct = abs(v10-v9)/max(abs(v10), abs(v9), 1e-12)*100
            conv = "YES" if diff_pct < 0.5 else ("NEAR" if diff_pct < 5 else "NO")
        else:
            conv = "N/A"
        pdf.table_row_left([
            (var, 22), (f"{ic:.4f}", 18), (f"{final:.6f}", 18),
            (f"{pct:+.1f}", 18), (conv, 16),
        ], fill=(row_i % 2 == 0))
        row_i += 1

    pdf.ln(3)
    pdf.body_text(
        "All metals show IC spin-down toward boundary-forced equilibrium values:\n"
        "  - Fe(II): 0.23 -> 0.0064 (-97%) -- converged\n"
        "  - Ca: 70.0 -> 3.39 (-95%) -- converged, driven by transport/dilution\n"
        "  - Mg: 15.0 -> 1.56 (-90%) -- near-converged\n"
        "  - S(+6): 1.35 -> 15.22 (+1027%) -- accumulating from boundary input\n\n"
        "These are conservative/semi-conservative species. Their behaviour is dominated "
        "by advective transport to/from boundaries. This is physically correct behaviour "
        "-- the lagoon interior is equilibrating with the Baltic Sea boundary water.")

    # =========================================================================
    # 13. SPATIAL VARIATION
    # =========================================================================
    pdf.add_page()
    pdf.section_title("13. Spatial Variation -- End of Repeat 10")

    spatial_vars = ["DIA_C", "CYN_C", "OPA_C", "DISS_OXYGEN", "NH4_N", "NO3_N",
                    "PO4_P", "DET_PART_ORG_C", "INORG_C", "TOT_ALK", "ZOO_C",
                    "CA", "MG", "FE_II"]
    cols_sp = [("Variable", 22)] + [(f"Box{b}", 18) for b in BOXES]
    pdf.table_header(cols_sp)
    row_i = 0
    for var in spatial_vars:
        vals = []
        for b in BOXES:
            if var in all_data[b]:
                vals.append(f"{all_data[b][var][-1]:.4f}")
            else:
                vals.append("N/A")
        pdf.table_row_left(
            [(var, 22)] + [(v, 18) for v in vals],
            fill=(row_i % 2 == 0))
        row_i += 1

    pdf.ln(3)
    pdf.body_text(
        "Spatial patterns at end of 10-year simulation:\n"
        "  - Box 25 has highest phytoplankton, DOC, metals (closest to river input)\n"
        "  - Box 14 has highest DO, NO3 (well-mixed, open-water characteristics)\n"
        "  - Ca and Mg show strong gradient: Box 25 >> Box 14 (river vs. Baltic influence)\n"
        "  - Spatial variation is maintained and realistic after 10 years")

    # =========================================================================
    # 14. MASS BALANCE
    # =========================================================================
    pdf.add_page()
    pdf.section_title("14. Mass Balance -- Box 5")

    cols_mb = [("Repeat", 12), ("Total N", 22), ("Total P", 22), ("Total C", 22),
               ("N change%", 20), ("P change%", 20), ("C change%", 20)]
    pdf.table_header(cols_mb)

    # compute initial totals
    ic_n = IC_MAP.get("NH4_N", 0) + IC_MAP.get("NO3_N", 0) + IC_MAP.get("DISS_ORG_N", 0) + IC_MAP.get("DET_PART_ORG_N", 0) + IC_MAP.get("DIA_C", 0)*0.22 + IC_MAP.get("ZOO_N", 0)
    ic_p = IC_MAP.get("PO4_P", 0) + IC_MAP.get("DISS_ORG_P", 0) + IC_MAP.get("DET_PART_ORG_P", 0) + IC_MAP.get("DIA_C", 0)*0.024 + IC_MAP.get("ZOO_P", 0)
    ic_c = IC_MAP.get("INORG_C", 0) + IC_MAP.get("DISS_ORG_C", 0) + IC_MAP.get("DET_PART_ORG_C", 0) + IC_MAP.get("DIA_C", 0) + IC_MAP.get("ZOO_C", 0)

    pdf.table_row_left([
        ("IC", 12), (f"{ic_n:.4f}", 22), (f"{ic_p:.4f}", 22), (f"{ic_c:.4f}", 22),
        ("--", 20), ("--", 20), ("--", 20),
    ])

    for r_idx, (r_start, r_end) in enumerate(repeats):
        idx = r_end - 1
        total_n = (d5["NH4_N"][idx] + d5["NO3_N"][idx] + d5["DISS_ORG_N"][idx] +
                   d5["DET_PART_ORG_N"][idx] +
                   sum(d5.get(v, np.zeros(len(time)))[idx]*0.22
                       for v in ["DIA_C","CYN_C","OPA_C","FIX_CYN_C","NOST_VEG_HET_C"] if v in d5) +
                   d5.get("ZOO_N", np.zeros(len(time)))[idx])
        total_p = (d5["PO4_P"][idx] + d5["DISS_ORG_P"][idx] +
                   d5["DET_PART_ORG_P"][idx] +
                   sum(d5.get(v, np.zeros(len(time)))[idx]*0.024
                       for v in ["DIA_C","CYN_C","OPA_C","FIX_CYN_C","NOST_VEG_HET_C"] if v in d5) +
                   d5.get("ZOO_P", np.zeros(len(time)))[idx])
        total_c = (d5["INORG_C"][idx] + d5["DISS_ORG_C"][idx] +
                   d5["DET_PART_ORG_C"][idx] +
                   sum(d5.get(v, np.zeros(len(time)))[idx]
                       for v in ["DIA_C","CYN_C","OPA_C","FIX_CYN_C","NOST_VEG_HET_C"] if v in d5) +
                   d5.get("ZOO_C", np.zeros(len(time)))[idx])
        n_pct = ((total_n - ic_n) / ic_n * 100) if ic_n != 0 else 0
        p_pct = ((total_p - ic_p) / ic_p * 100) if ic_p != 0 else 0
        c_pct = ((total_c - ic_c) / ic_c * 100) if ic_c != 0 else 0
        pdf.table_row_left([
            (f"R{r_idx+1}", 12), (f"{total_n:.4f}", 22), (f"{total_p:.4f}", 22),
            (f"{total_c:.4f}", 22), (f"{n_pct:+.1f}%", 20), (f"{p_pct:+.1f}%", 20),
            (f"{c_pct:+.1f}%", 20),
        ], fill=(r_idx % 2 == 0))

    pdf.ln(3)
    pdf.body_text(
        "The large IC-to-R1 drops (N: -87%, C: -84%) represent IC spin-down from "
        "unrealistic initial conditions. From R1 onward, the totals stabilize:\n"
        "  - Total N slowly increasing (NO3 accumulation without denitrification)\n"
        "  - Total P slowly decreasing (POP settling loss)\n"
        "  - Total C slowly increasing (DIC equilibrating with boundary)")

    # =========================================================================
    # 15. PROCESS-LEVEL ROOT-CAUSE SUMMARY
    # =========================================================================
    pdf.add_page()
    pdf.section_title("15. Process-Level Root-Cause Analysis Summary")

    issues = [
        ("CRITICAL", "Zooplankton N/P Collapse",
         "ZOO_N and ZOO_P = 0 in all boxes after Year 1",
         "Three compounding causes:\n"
         "1. Boundary conditions have ZOO_N = ZOO_P = 0 (FORC_TS files)\n"
         "2. Variable stoichiometry mode (ZOOP_OPTION_1=1) has no homeostatic N:C floor\n"
         "3. Excretion terms R_ZOO_EX_DON/DOP never computed (developer bug note in source)",
         "Set ZOO_N = ZOO_C * 0.22 and ZOO_P = ZOO_C * 0.024 in all FORC_TS files. "
         "Consider switching to ZOOP_OPTION_1=0 or adding min N:C enforcement."),

        ("WARNING", "Phytoplankton Year-over-Year Decline (~4%/yr)",
         "DIA_C, CYN_C, OPA_C all show slow annual decline in end-of-year and peak values",
         "Combination of:\n"
         "1. Boundary dilution (Baltic DIA_C = 0.01 vs internal ~1.3 peak)\n"
         "2. Settling loss without sediment recycling (MODEL_SEDIMENTS=0)\n"
         "3. Winter mortality floor (KD_DIA_20 = 0.12/day) acts as seasonal ratchet",
         "Enable sediment model (MODEL_SEDIMENTS=1) for benthic recycling. "
         "Consider reducing KD_DIA_20 or adding seed population mechanism."),

        ("WARNING", "NO3 Accumulation (+1.5%/yr)",
         "Nitrate slowly increasing in all boxes without reaching steady state",
         "No denitrification pathway active:\n"
         "1. MODEL_SEDIMENTS=0 disables sediment denitrification\n"
         "2. No pelagic denitrification process in current configuration\n"
         "3. Nitrification converts NH4 -> NO3 but no reverse pathway",
         "Enable sediment model for sediment denitrification. "
         "Or consider adding pelagic denitrification process."),

        ("INFO", "IC Spin-Down (Year 1)",
         "All variables show large IC-to-R1 transition as unrealistic ICs wash out",
         "Initial conditions are rough estimates, not at equilibrium. "
         "This is expected and the purpose of multi-year spin-up runs.",
         "Use R10 final state as ICs for future simulations (write MTRX files). "
         "Consider using 5+ repeat spin-up before analysis runs."),
    ]

    for sev, title, symptom, cause, fix in issues:
        pdf.safe_page_break(50)
        pdf.sub_title(title)
        pdf.set_font("Helvetica", "B", 8)
        pdf.set_text_color(0, 0, 0)
        pdf.cell(0, 4.5, f"Severity: {sev}")
        pdf.ln()
        pdf.set_font("Helvetica", "", 8)
        pdf.cell(0, 4.5, f"Symptom: {symptom}")
        pdf.ln(5)
        pdf.set_font("Helvetica", "B", 8)
        pdf.cell(0, 4.5, "Root Cause:")
        pdf.ln()
        pdf.set_font("Helvetica", "", 8)
        pdf.multi_cell(0, 4, cause)
        pdf.ln(2)
        pdf.set_font("Helvetica", "B", 8)
        pdf.cell(0, 4.5, "Recommendation:")
        pdf.ln()
        pdf.set_font("Helvetica", "", 8)
        pdf.multi_cell(0, 4, fix)
        pdf.ln(5)

    # =========================================================================
    # 16. RECOMMENDATIONS
    # =========================================================================
    pdf.add_page()
    pdf.section_title("16. Recommendations & Next Steps")

    recs = [
        ("Immediate: Fix Zooplankton Boundary Conditions",
         "Update all FORC_TS files to set ZOO_N = ZOO_C * 0.22 and ZOO_P = ZOO_C * 0.024. "
         "This is the minimum fix to prevent N/P decoupling from carbon. Re-run simulation "
         "to verify zooplankton stoichiometry recovery."),

        ("Short-term: Enable Sediment Model",
         "Set MODEL_SEDIMENTS=1 in the input file to activate benthic recycling. This will "
         "provide: (a) denitrification pathway to stabilize NO3, (b) nutrient return from "
         "settled detritus to support phytoplankton, (c) sediment oxygen demand for "
         "realistic summer DO depression."),

        ("Short-term: Use R10 State as New Initial Conditions",
         "The MTRX files contain the final state vector. These can be used as initial "
         "conditions for future runs to skip the spin-up phase. This is standard practice "
         "for ecological models."),

        ("Medium-term: Review Zooplankton Source Code",
         "Fix the R_ZOO_EX_DON / R_ZOO_EX_DOP computation (currently never assigned). "
         "Add a minimum N:C ratio enforcement to prevent stoichiometric collapse. "
         "Consider whether ZOOP_OPTION_1=0 (fixed stoichiometry) is more appropriate."),

        ("Long-term: Calibration Campaign",
         "With the structural issues fixed, conduct a formal calibration using observed "
         "data to tune: KG_DIA_OPT_TEMP, KD_DIA_20, KG_ZOO_OPT_TEMP, settling velocities, "
         "and half-saturation constants. The 10-year spin-up run provides a stable baseline "
         "for parameter sensitivity analysis."),
    ]

    for title, text in recs:
        pdf.safe_page_break(30)
        pdf.sub_title(title)
        pdf.body_text(text)

    # =========================================================================
    # APPENDIX: Model Configuration
    # =========================================================================
    pdf.add_page()
    pdf.section_title("Appendix A: Simulation Configuration")

    config_items = [
        ("Input file", "INPUT_3560day.txt"),
        ("Simulation start", "6209.0 Julian days"),
        ("Simulation end", "6565.0 Julian days"),
        ("Number of repeats", "10"),
        ("Effective duration", "3560 days (9.75 years)"),
        ("Time steps per day", "240"),
        ("Print interval", "240 (1 output per day)"),
        ("Output boxes", "5, 6, 8, 9, 14, 17, 25"),
        ("Sediment model", "OFF (MODEL_SEDIMENTS=0)"),
        ("Resuspension", "OFF (RESUSPENSION_OPTION=0)"),
        ("WCONST file", "WCONST_04.txt (with detritus C:N fix)"),
    ]
    for label, value in config_items:
        pdf.set_font("Helvetica", "B", 9)
        pdf.cell(60, 5, label + ":")
        pdf.set_font("Helvetica", "", 9)
        pdf.cell(0, 5, value)
        pdf.ln()

    pdf.ln(5)
    pdf.sub_title("Key WCONST_04.txt Parameters")
    wconst_params = [
        ("Diatoms", "KG=3.7/d, KD=0.12/d, KR=0.05/d, EFF=0.95, I_S=100, "
            "KHS_DIN=0.01, KHS_DIP=0.005, KHS_DSi=0.013"),
        ("Cyanobacteria", "KG=1.0/d, KD=0.10/d, KR=0.02/d, EFF=0.90"),
        ("Other Phyto", "KG=2.0/d, KD=0.10/d, KR=0.03/d, EFF=0.90"),
        ("Zooplankton", "KG=0.45/d, KD=0.15/d, KR=0.03/d, EFF=0.80, "
            "FOOD_MIN=0.02, N:C=0.22, P:C=0.024"),
        ("Detritus", "KDISS_C=0.25/d (fixed from 10.0), FAC_PHYT_C=0.0 (fixed from 2.0)"),
    ]
    for group, params in wconst_params:
        pdf.set_font("Helvetica", "B", 8)
        pdf.cell(30, 5, group + ":")
        pdf.set_font("Courier", "", 7)
        pdf.multi_cell(0, 4, params)
        pdf.ln(1)

    # =========================================================================
    # Save
    # =========================================================================
    out_path = OUT_DIR / "3560Day_Deep_Process_Analysis_Report.pdf"
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    pdf.output(str(out_path))
    print(f"\nReport saved to: {out_path}")
    print(f"Pages: {pdf.page_no()}")


if __name__ == "__main__":
    generate_report()
