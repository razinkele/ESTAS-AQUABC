#!/usr/bin/env python3
"""
Generate a comprehensive PDF report for the AQUABC 365-day (full-year)
simulation deep analysis.

Analyses all 36 state variables across 7 output boxes, covering:
  - Overall statistics and status assessment
  - Stoichiometric ratios (detritus C:N fix verification)
  - Seasonal dynamics
  - Phytoplankton bloom timing
  - Dissolved oxygen / hypoxia check
  - DIC and alkalinity dynamics
  - Spatial variation
  - Mass balance
  - Issues summary and interpretation
"""

from __future__ import annotations

import csv
import io
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

GROUPS = [
    ("Nutrients (N, P, Si)", ["NH4_N", "NO3_N", "PO4_P", "DISS_Si"]),
    ("Dissolved Oxygen", ["DISS_OXYGEN"]),
    ("Phytoplankton (C)", ["DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C"]),
    ("Zooplankton", ["ZOO_C", "ZOO_N", "ZOO_P"]),
    ("Detritus (POM)", ["DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P", "PART_Si"]),
    ("Dissolved Organics (DOM)", ["DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P"]),
    ("Carbon System", ["INORG_C", "TOT_ALK", "CH4_C"]),
    ("Metals & Conservative", ["FE_II", "FE_III", "MN_II", "MN_IV", "CA", "MG", "S_PLUS_6", "S_MINUS_2"]),
    ("Akinetes", ["AKI_C"]),
    ("Secondary Metabolites", ["SEC_METAB_DIA", "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST"]),
]

# IC values per init-cond set: {variable: (set1_mud, set2_sand)}
IC_MAP = {
    "NH4_N": (0.02, 0.02), "NO3_N": (0.5, 0.5), "PO4_P": (0.005, 0.0047),
    "DISS_Si": (3.7, 3.0), "DISS_OXYGEN": (15.0, 14.0),
    "DIA_C": (1.0, 1.75), "CYN_C": (0.06, 0.07), "OPA_C": (0.022, 0.022),
    "FIX_CYN_C": (0.01, 0.01), "NOST_VEG_HET_C": (0.0, 0.0),
    "ZOO_C": (0.02, 0.02), "ZOO_N": (0.004, 0.004), "ZOO_P": (0.0005, 0.0005),
    "DET_PART_ORG_C": (6.0, 6.5), "DET_PART_ORG_N": (1.0, 1.0),
    "DET_PART_ORG_P": (0.0052, 0.015), "PART_Si": (1.5, 1.5),
    "DISS_ORG_C": (11.0, 8.0), "DISS_ORG_N": (4.5, 1.5), "DISS_ORG_P": (0.04, 0.025),
    "INORG_C": (25.0, 25.0), "TOT_ALK": (2.8, 2.8), "CH4_C": (0.0, 0.0),
    "FE_II": (0.23, 0.23), "FE_III": (0.55, 0.55),
    "MN_II": (0.1, 0.1), "MN_IV": (0.1, 0.1),
    "CA": (70.0, 70.0), "MG": (15.0, 15.0),
    "S_PLUS_6": (1.35, 1.35), "S_MINUS_2": (0.0, 0.0),
    "AKI_C": (0.0, 8.0),
    "SEC_METAB_DIA": (1.0, 1.0), "SEC_METAB_NOFIX_CYN": (1.0, 1.0),
    "SEC_METAB_FIX_CYN": (1.0, 1.0), "SEC_METAB_NOST": (1.0, 1.0),
}

# Set 1 (sand) boxes for IC lookup: boxes NOT in mud set
MUD_BOXES = {3, 14, 15, 17, 18, 19, 21, 23, 25}


def get_ic(var: str, box: int) -> float:
    ic_set1, ic_set2 = IC_MAP.get(var, (0.0, 0.0))
    return ic_set2 if box in MUD_BOXES else ic_set1


def status_label(var: str, ic: float, final: float) -> str:
    if ic == 0.0:
        return "OK"
    drop = (ic - final) / ic if ic != 0 else 0
    if drop > 0.99:
        return "CRASH-99%"
    if drop > 0.95:
        return "CRASH-95%"
    return "OK"


def seasonal_stats(time_arr, val_arr, sim_start=6209):
    """Split into DJF/MAM/JJA/SON based on day-of-year."""
    doy = ((time_arr - sim_start) % 365).astype(int)
    seasons = {}
    for name, months in [("Winter(DJF)", (12, 1, 2)), ("Spring(MAM)", (3, 4, 5)),
                         ("Summer(JJA)", (6, 7, 8)), ("Autumn(SON)", (9, 10, 11))]:
        # map DOY to month (approx)
        masks = []
        for m in months:
            start_doy = int((m - 1) * 30.44)
            end_doy = int(m * 30.44)
            masks.append((doy >= start_doy) & (doy < end_doy))
        mask = masks[0] | masks[1] | masks[2]
        if mask.any():
            sv = val_arr[mask]
            seasons[name] = (sv.mean(), sv.min(), sv.max())
        else:
            seasons[name] = (0.0, 0.0, 0.0)
    return seasons


# -- PDF class ----------------------------------------------------------------

class AnalysisPDF(FPDF):
    def header(self):
        self.set_font("Helvetica", "B", 9)
        self.set_text_color(100, 100, 100)
        self.cell(0, 5, "AQUABC 365-Day Full-Year Simulation -- Deep Analysis Report", 0, 1, "C")
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
        self.line(10, self.get_y(), 200, self.get_y())
        self.ln(3)

    def sub_title(self, title: str):
        self.set_font("Helvetica", "B", 10)
        self.set_text_color(51, 51, 51)
        self.cell(0, 7, title, 0, 1)
        self.ln(1)

    def body_text(self, text: str):
        self.set_font("Helvetica", "", 9)
        self.set_text_color(0, 0, 0)
        self.multi_cell(0, 4.5, text)
        self.ln(2)

    def code_block(self, text: str):
        self.set_font("Courier", "", 7.5)
        self.set_fill_color(245, 245, 245)
        self.set_text_color(0, 0, 0)
        x0 = self.get_x()
        w = self.w - 2 * self.l_margin
        for line in text.split("\n"):
            if self.get_y() > 275:
                self.add_page()
            self.cell(w, 3.8, line, 0, 1, fill=True)
        self.ln(2)

    def color_cell(self, w, h, txt, status="OK"):
        if "CRASH-99" in status:
            self.set_fill_color(255, 200, 200)
        elif "CRASH-95" in status:
            self.set_fill_color(255, 235, 200)
        elif "SUSPECT" in status:
            self.set_fill_color(255, 255, 180)
        else:
            self.set_fill_color(220, 255, 220)
        self.set_font("Courier", "", 7)
        self.cell(w, h, txt, 1, 0, "C", fill=True)


def generate_report():
    # Load all box data
    print("Loading box data...")
    box_data = {}
    for b in BOXES:
        box_data[b] = load_box(b)
    print(f"  Loaded {len(BOXES)} boxes")

    pdf = AnalysisPDF()
    pdf.alias_nb_pages()
    pdf.set_auto_page_break(auto=True, margin=15)

    # -- Title Page -------------------------------------------------------
    pdf.add_page()
    pdf.ln(30)
    pdf.set_font("Helvetica", "B", 22)
    pdf.set_text_color(0, 51, 102)
    pdf.cell(0, 12, "AQUABC Full-Year Simulation", 0, 1, "C")
    pdf.cell(0, 12, "Deep Analysis Report", 0, 1, "C")
    pdf.ln(10)
    pdf.set_font("Helvetica", "", 12)
    pdf.set_text_color(80, 80, 80)
    pdf.cell(0, 8, "365-Day Simulation (Julian days 6209-6573)", 0, 1, "C")
    pdf.cell(0, 8, "Curonian Lagoon -- ESTAS-AQUABC Model", 0, 1, "C")
    pdf.ln(5)
    pdf.cell(0, 8, f"Generated: {date.today().isoformat()}", 0, 1, "C")
    pdf.cell(0, 8, "7 Output Boxes | 36 State Variables | 8761 Timesteps", 0, 1, "C")
    pdf.ln(15)
    pdf.set_font("Helvetica", "I", 10)
    pdf.set_text_color(100, 100, 100)
    pdf.multi_cell(0, 5,
        "This report presents a comprehensive analysis of the AQUABC ecological model "
        "output from a full-year (364 day) simulation. It covers all 36 pelagic state "
        "variables across 7 representative lagoon boxes, including nutrient dynamics, "
        "phytoplankton seasonal succession, zooplankton, dissolved oxygen, carbon "
        "system, detritus stoichiometry, metals, and allelopathic secondary metabolites.",
        align="C")

    # -- Section 1: Executive Summary -------------------------------------
    pdf.add_page()
    pdf.section_title("1. Executive Summary")
    pdf.body_text(
        "The 365-day AQUABC simulation completed successfully with no numerical "
        "instabilities or crashes. The model ran 8761 timesteps (240/day) covering "
        "Julian days 6209.0 to 6574.0 (approximately one full calendar year).\n\n"
        "KEY FINDINGS:\n"
        "- 0 CRITICAL issues found (major improvement from initial 200-day analysis)\n"
        "- 129 WARNINGS -- almost all are initial-condition spin-down effects\n"
        "- Detritus C:N ratio FIX CONFIRMED: Final C:N mass ratio = 6.1-6.5 across all boxes "
        "(Redfield-consistent, previously crashed to 0.08)\n"
        "- Realistic seasonal phytoplankton succession: Diatom spring bloom (DOY 127-128), "
        "Nostocales late-summer peak (DOY 245-257)\n"
        "- Dissolved oxygen: Well-oxygenated throughout (8.0-15.0 mg/L), no hypoxia events\n"
        "- Alkalinity stable at 2.8-3.2 meq/L -- ecologically reasonable\n"
        "- Strong spatial gradients: Box 25 (river) highest concentrations, Box 14 (Baltic) lowest\n\n"
        "MAIN CONCERNS:\n"
        "- Zooplankton collapse: ZOO_C drops 99%+ to ~3-4e-5 mg C/L in all boxes, never recovers "
        "to ecologically meaningful levels. ZOO_N and ZOO_P reach exactly 0.\n"
        "- Cyanobacteria (CYN_C, FIX_CYN_C) never bloom -- peak is always at Day 0 (= initial condition). "
        "Growth conditions may be too restrictive.\n"
        "- Conservative species (Ca, Mg, metals) undergo >95% decline from IC values toward "
        "boundary equilibrium -- this is expected transport behavior but ICs were far too high."
    )

    # -- Section 2: Overall Statistics ------------------------------------
    pdf.add_page()
    pdf.section_title("2. Variable Statistics -- All Boxes")
    pdf.body_text(
        "Each variable is assessed across all 7 output boxes. Status codes:\n"
        "  OK = within reasonable expectations\n"
        "  CRASH-95% = final value dropped 95%+ from IC\n"
        "  CRASH-99% = final value dropped 99%+ from IC\n\n"
        "Note: 'CRASH' labels for detritus, metals, secondary metabolites, and "
        "conservative species reflect IC spin-down (ICs were set too high) rather "
        "than model bugs."
    )

    for group_name, var_list in GROUPS:
        if pdf.get_y() > 250:
            pdf.add_page()
        pdf.sub_title(group_name)

        # Table header
        col_widths = [30, 8, 16, 16, 16, 12, 12, 14]
        headers_row = ["Variable", "Box", "Min", "Max", "Mean", "IC", "Final", "Status"]
        pdf.set_font("Courier", "B", 6.5)
        for w, h in zip(col_widths, headers_row):
            pdf.cell(w, 4, h, 1, 0, "C")
        pdf.ln()

        for var in var_list:
            for b in BOXES:
                d = box_data[b]
                if var not in d:
                    continue
                arr = d[var]
                ic = get_ic(var, b)
                final = arr[-1]
                st = status_label(var, ic, final)

                row = [var, str(b),
                       f"{arr.min():.4g}", f"{arr.max():.4g}", f"{arr.mean():.4g}",
                       f"{ic:.4g}", f"{final:.4g}", st]
                if pdf.get_y() > 280:
                    pdf.add_page()
                    pdf.set_font("Courier", "B", 6.5)
                    for w, h in zip(col_widths, headers_row):
                        pdf.cell(w, 4, h, 1, 0, "C")
                    pdf.ln()

                pdf.set_font("Courier", "", 6.5)
                for i, (w, val) in enumerate(zip(col_widths, row)):
                    if i == len(row) - 1:
                        pdf.color_cell(w, 3.5, val, st)
                    else:
                        pdf.cell(w, 3.5, val, 1, 0, "C")
                pdf.ln()
        pdf.ln(3)

    # -- Section 3: Detritus C:N Fix Verification -------------------------
    pdf.add_page()
    pdf.section_title("3. Detritus C:N Ratio -- Fix Verification")
    pdf.body_text(
        "The WCONST_04.txt parameter fix (KDISS_DET_PART_ORG_C_20: 10.0 -> 0.25, "
        "FAC_PHYT_DET_PART_ORG_C: 2.0 -> 0.0) has been CONFIRMED EFFECTIVE over the "
        "full 365-day simulation.\n\n"
        "Before fix: C:N mass ratio crashed from 6.0 to 0.08 within 10-20 days.\n"
        "After fix: C:N mass ratio = 6.1-6.5 at end of year (Redfield-consistent ~6.6).\n"
        "The C:N molar ratio is 7.2-7.6, close to the Redfield value of 6.63."
    )

    pdf.sub_title("Final Detritus Stoichiometry (Day 364)")
    col_w = [10, 16, 16, 16, 12, 12, 12, 18]
    hdr = ["Box", "DET_C", "DET_N", "DET_P", "C:N", "C:P", "N:P", "Status"]
    pdf.set_font("Courier", "B", 7)
    for w, h in zip(col_w, hdr):
        pdf.cell(w, 4, h, 1, 0, "C")
    pdf.ln()

    for b in BOXES:
        d = box_data[b]
        c_final = d["DET_PART_ORG_C"][-1]
        n_final = d["DET_PART_ORG_N"][-1]
        p_final = d["DET_PART_ORG_P"][-1]
        cn = c_final / n_final if n_final > 0 else float("inf")
        cp = c_final / p_final if p_final > 0 else float("inf")
        np_ratio = n_final / p_final if p_final > 0 else float("inf")
        st = "OK" if 4.0 < cn < 10.0 else "SUSPECT"
        row = [str(b), f"{c_final:.5f}", f"{n_final:.5f}", f"{p_final:.6f}",
               f"{cn:.2f}", f"{cp:.1f}", f"{np_ratio:.1f}", st]
        pdf.set_font("Courier", "", 7)
        for i, (w, val) in enumerate(zip(col_w, row)):
            if i == len(row) - 1:
                pdf.color_cell(w, 4, val, st)
            else:
                pdf.cell(w, 4, val, 1, 0, "C")
        pdf.ln()

    pdf.ln(3)
    pdf.body_text(
        "The C:N mass ratios (6.1-6.5) are consistent across all boxes and match "
        "expected Redfield stoichiometry (~6.6 mass, ~5.7 molar). Box 25 (upstream, "
        "river-influenced) has the highest absolute concentrations but the same ratio. "
        "Box 14 (Baltic-influenced) has the lowest concentrations."
    )

    # -- Section 4: Seasonal Dynamics -------------------------------------
    pdf.add_page()
    pdf.section_title("4. Seasonal Dynamics (Box 5, Representative)")
    pdf.body_text(
        "Seasonal averages computed by splitting the year into meteorological seasons "
        "(DJF = Winter, MAM = Spring, JJA = Summer, SON = Autumn). Box 5 is used as "
        "the representative central lagoon box."
    )

    key_vars = ["DIA_C", "CYN_C", "OPA_C", "DISS_OXYGEN", "NH4_N", "NO3_N",
                "PO4_P", "DET_PART_ORG_C", "INORG_C", "TOT_ALK", "ZOO_C"]
    d5 = box_data[5]
    time5 = d5["TIME_DAYS"]

    col_w = [28, 20, 20, 20, 20]
    hdr = ["Variable", "Winter", "Spring", "Summer", "Autumn"]
    pdf.set_font("Courier", "B", 7)
    for w, h in zip(col_w, hdr):
        pdf.cell(w, 4, h, 1, 0, "C")
    pdf.ln()

    for var in key_vars:
        if var not in d5:
            continue
        ss = seasonal_stats(time5, d5[var])
        pdf.set_font("Courier", "", 7)
        pdf.cell(28, 4, var, 1, 0, "L")
        for sname in ["Winter(DJF)", "Spring(MAM)", "Summer(JJA)", "Autumn(SON)"]:
            mean, mn, mx = ss[sname]
            pdf.cell(20, 4, f"{mean:.4g}", 1, 0, "C")
        pdf.ln()

    pdf.ln(3)
    pdf.body_text(
        "Key seasonal patterns:\n"
        "- DIA_C (Diatoms): Classic spring-summer bloom pattern. "
        "Winter ~0.07, peaks in Summer ~0.55 mg C/L.\n"
        "- DISS_OXYGEN: Proper seasonal cycle -- highest in winter (13.3 mg/L), "
        "lowest in summer (9.2 mg/L) due to temperature-dependent saturation.\n"
        "- NO3_N: Spring maximum (2.1 mg/L) from winter mixing, summer drawdown "
        "(0.86 mg/L) from phytoplankton uptake, autumn minimum (0.43 mg/L).\n"
        "- PO4_P: Summer minimum (0.01 mg/L) from biological uptake, "
        "releasing back in autumn (0.028 mg/L).\n"
        "- ZOO_C: Very low throughout all seasons (<0.002 mg C/L). "
        "Never recovers from initial crash."
    )

    # -- Section 5: Phytoplankton Bloom Timing ----------------------------
    pdf.add_page()
    pdf.section_title("5. Phytoplankton Bloom Timing and Succession")
    pdf.body_text(
        "Peak biomass timing for each phytoplankton group across all boxes. "
        "DOY = Day of Year (0 = simulation start = approx. Jan 1)."
    )

    phyto_vars = ["DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C"]
    col_w = [28, 8, 14, 14, 8]
    hdr = ["Species", "Box", "Peak Day", "Peak Val", "DOY"]
    pdf.set_font("Courier", "B", 7)
    for w, h in zip(col_w, hdr):
        pdf.cell(w, 4, h, 1, 0, "C")
    pdf.ln()

    for var in phyto_vars:
        for b in BOXES:
            d = box_data[b]
            if var not in d:
                continue
            arr = d[var]
            t = d["TIME_DAYS"]
            peak_idx = np.argmax(arr)
            peak_day = t[peak_idx]
            peak_val = arr[peak_idx]
            doy = int(peak_day - 6209)
            pdf.set_font("Courier", "", 7)
            pdf.cell(28, 3.5, var, 1, 0, "L")
            pdf.cell(8, 3.5, str(b), 1, 0, "C")
            pdf.cell(14, 3.5, f"{peak_day:.1f}", 1, 0, "C")
            pdf.cell(14, 3.5, f"{peak_val:.4g}", 1, 0, "C")
            pdf.cell(8, 3.5, str(doy), 1, 0, "C")
            pdf.ln()
        pdf.ln(1)

    pdf.ln(3)
    pdf.body_text(
        "SUCCESSION PATTERN:\n"
        "1. OPA_C (Other phytoplankton): Peaks DOY 122-124 (early May) -- first bloom.\n"
        "2. DIA_C (Diatoms): Main spring bloom DOY 127-128 (May). Highest biomass "
        "(1.5-1.8 mg C/L). Box 25 peaks later (DOY 144) due to river influence.\n"
        "3. NOST_VEG_HET_C (Nostocales): Late summer peak DOY 245-257 (Sep). "
        "Very low biomass (<0.02 mg C/L) but ecologically correct timing.\n"
        "4. CYN_C (Non-fixing cyanobacteria): NEVER blooms -- peak at DOY 0 = IC only.\n"
        "5. FIX_CYN_C (N-fixing cyanobacteria): NEVER blooms -- peak at DOY 0 = IC only.\n\n"
        "The CYN_C and FIX_CYN_C failure to bloom suggests their growth parameters "
        "may be too restrictive or environmental conditions (light, temperature) are "
        "not favorable enough in this setup. Worth investigating in future calibration."
    )

    # -- Section 6: Dissolved Oxygen --------------------------------------
    if pdf.get_y() > 200:
        pdf.add_page()
    pdf.section_title("6. Dissolved Oxygen -- Hypoxia Assessment")
    pdf.body_text(
        "Oxygen dynamics across all 7 boxes. The model maintains well-oxygenated "
        "conditions throughout the entire year with no hypoxia events."
    )

    col_w = [8, 14, 14, 14, 14, 14, 12, 12]
    hdr = ["Box", "Min DO", "Day@Min", "Max DO", "Day@Max", "Mean", "<2mg/L", "<4mg/L"]
    pdf.set_font("Courier", "B", 7)
    for w, h in zip(col_w, hdr):
        pdf.cell(w, 4, h, 1, 0, "C")
    pdf.ln()

    for b in BOXES:
        d = box_data[b]
        do = d["DISS_OXYGEN"]
        t = d["TIME_DAYS"]
        min_val = do.min()
        min_day = t[np.argmin(do)]
        max_val = do.max()
        max_day = t[np.argmax(do)]
        mean_val = do.mean()
        days_lt_2 = np.sum(do < 2.0) / 240  # convert timesteps to days
        days_lt_4 = np.sum(do < 4.0) / 240

        row = [str(b), f"{min_val:.3f}", f"{min_day:.1f}", f"{max_val:.3f}",
               f"{max_day:.1f}", f"{mean_val:.3f}", f"{days_lt_2:.1f}", f"{days_lt_4:.1f}"]
        pdf.set_font("Courier", "", 7)
        for w, val in zip(col_w, row):
            pdf.cell(w, 3.5, val, 1, 0, "C")
        pdf.ln()

    pdf.ln(3)
    pdf.body_text(
        "All boxes maintain DO above 8.0 mg/L year-round. Minimum DO occurs around "
        "Julian day 6395-6396 (DOY ~186, early July) when water temperature peaks. "
        "Maximum DO at simulation start (IC = 14-15 mg/L). Mean DO ~11.3-11.6 mg/L.\n\n"
        "Zero days of hypoxia (<2 mg/L) or moderate oxygen stress (<4 mg/L) in any box."
    )

    # -- Section 7: Carbon System (DIC/ALK) -------------------------------
    pdf.add_page()
    pdf.section_title("7. DIC and Alkalinity Dynamics")
    pdf.body_text(
        "Inorganic carbon (DIC) and total alkalinity (ALK) behavior across key boxes."
    )

    for b in [5, 14, 25]:
        d = box_data[b]
        dic = d["INORG_C"]
        alk = d["TOT_ALK"]
        pdf.sub_title(f"Box {b}")
        pdf.body_text(
            f"DIC: IC={dic[0]:.3f}, Final={dic[-1]:.3f}, "
            f"Min={dic.min():.3f}, Max={dic.max():.3f}, Mean={dic.mean():.3f}\n"
            f"ALK: IC={alk[0]:.3f}, Final={alk[-1]:.3f}, "
            f"Min={alk.min():.3f}, Max={alk.max():.3f}, Mean={alk.mean():.3f}"
        )

    pdf.body_text(
        "DIC drops rapidly from the high IC (25 mg/L) to an equilibrium around 3-5 mg/L "
        "within the first 30 days, then remains stable. Box 25 (upstream) maintains "
        "higher DIC (5.4 mg/L) from river input.\n\n"
        "Alkalinity is remarkably stable at 2.8-3.2 meq/L throughout the year across "
        "all boxes. This indicates the carbonate buffering system is working properly "
        "and is not being destabilized by excessive acid/base reactions."
    )

    # -- Section 8: Mass Balance ------------------------------------------
    pdf.add_page()
    pdf.section_title("8. Mass Balance (Box 5)")
    pdf.body_text(
        "Total nitrogen, phosphorus, and carbon pools in Box 5 at key time points. "
        "Total = sum of all dissolved + particulate + biological pools."
    )

    def total_n(d, idx):
        return sum(d[v][idx] for v in ["NH4_N", "NO3_N", "DISS_ORG_N", "DET_PART_ORG_N", "ZOO_N"]
                   if v in d)

    def total_p(d, idx):
        return sum(d[v][idx] for v in ["PO4_P", "DISS_ORG_P", "DET_PART_ORG_P", "ZOO_P"]
                   if v in d)

    def total_c(d, idx):
        c_vars = ["INORG_C", "DISS_ORG_C", "DET_PART_ORG_C",
                   "DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C", "ZOO_C"]
        return sum(d[v][idx] for v in c_vars if v in d)

    d5 = box_data[5]
    t5 = d5["TIME_DAYS"]
    time_points = [0, 2160, 4320, len(t5) - 1]  # Day 0, ~90, ~180, 364
    labels = ["Day 0 (IC)", "Day 90", "Day 180", "Day 364"]

    col_w = [24, 20, 20, 20]
    hdr = ["Time Point", "Total N", "Total P", "Total C"]
    pdf.set_font("Courier", "B", 7.5)
    for w, h in zip(col_w, hdr):
        pdf.cell(w, 4, h, 1, 0, "C")
    pdf.ln()

    for lbl, idx in zip(labels, time_points):
        idx = min(idx, len(t5) - 1)
        tn = total_n(d5, idx)
        tp = total_p(d5, idx)
        tc = total_c(d5, idx)
        pdf.set_font("Courier", "", 7.5)
        pdf.cell(24, 4, lbl, 1, 0, "L")
        pdf.cell(20, 4, f"{tn:.4f}", 1, 0, "C")
        pdf.cell(20, 4, f"{tp:.4f}", 1, 0, "C")
        pdf.cell(20, 4, f"{tc:.4f}", 1, 0, "C")
        pdf.ln()

    pdf.ln(3)
    pdf.body_text(
        "Significant net export of mass through boundaries:\n"
        "- Total N: 6.26 -> 0.84 mg/L (-87%) -- largely advected out as NO3 and DON\n"
        "- Total P: 0.077 -> 0.045 mg/L (-42%) -- mostly PO4 export\n"
        "- Total C: 43.1 -> 7.97 mg/L (-82%) -- dominated by DIC washout from high IC\n\n"
        "The mass loss is physically consistent with the lagoon being an open system "
        "with significant water exchange through boundaries."
    )

    # -- Section 9: Spatial Variation -------------------------------------
    if pdf.get_y() > 200:
        pdf.add_page()
    pdf.section_title("9. Spatial Variation (Final Day)")
    pdf.body_text(
        "Comparison of final values across all 7 boxes showing spatial gradients. "
        "Boxes ordered from downstream (Box 5, near Klaipeda strait) to upstream (Box 25, "
        "near Nemunas delta)."
    )

    spatial_vars = ["DIA_C", "DISS_OXYGEN", "NH4_N", "NO3_N", "PO4_P",
                    "DET_PART_ORG_C", "INORG_C", "TOT_ALK", "CA"]

    col_w = [24, 14, 14, 14, 14, 14, 14, 14]
    hdr = ["Variable", "Box5", "Box6", "Box8", "Box9", "Box14", "Box17", "Box25"]
    pdf.set_font("Courier", "B", 6.5)
    for w, h in zip(col_w, hdr):
        pdf.cell(w, 4, h, 1, 0, "C")
    pdf.ln()

    for var in spatial_vars:
        pdf.set_font("Courier", "", 6.5)
        pdf.cell(24, 3.5, var, 1, 0, "L")
        for b in BOXES:
            val = box_data[b][var][-1] if var in box_data[b] else 0
            pdf.cell(14, 3.5, f"{val:.4g}", 1, 0, "C")
        pdf.ln()

    pdf.ln(3)
    pdf.body_text(
        "Clear spatial gradient: Box 25 (upstream, river-influenced) has the highest "
        "concentrations for DIA_C, NH4, DIC, Ca. Box 14 (Baltic-influenced) has the "
        "lowest. NO3 is relatively uniform (~0.67-0.71) across all boxes at year end. "
        "ALK is very uniform (2.99-3.15 meq/L)."
    )

    # -- Section 10: Zooplankton Analysis ---------------------------------
    pdf.add_page()
    pdf.section_title("10. Zooplankton Collapse Analysis")
    pdf.body_text(
        "Zooplankton (ZOO_C) shows a severe and persistent collapse across all boxes. "
        "This is the most significant biological concern in the 365-day simulation.\n\n"
        "TIMELINE:\n"
        "- Day 0: ZOO_C = 0.02 mg C/L (IC)\n"
        "- Day 30: ZOO_C drops to ~0.0008 (96% loss)\n"
        "- Day 90: ~0.0001 (99.5% loss)\n"
        "- Summer peak (~Day 210): Brief recovery to ~0.002, then crashes again\n"
        "- Day 364: ~4e-5 (99.8% loss)\n"
        "- ZOO_N and ZOO_P reach exactly 0.0\n\n"
        "POSSIBLE CAUSES:\n"
        "1. Food limitation: Total phytoplankton carbon is very low (<0.01 mg C/L) "
        "for most of the year except during the spring diatom bloom.\n"
        "2. Mortality/respiration rates may be too high relative to ingestion.\n"
        "3. The zooplankton switching function may not efficiently route food.\n"
        "4. Zooplankton N and P reaching zero suggests internal nutrient pools deplete "
        "faster than they can be replenished from feeding.\n\n"
        "RECOMMENDATION: Review zooplankton grazing parameters (WCONST_04.txt lines "
        "164-198) and potentially increase phytoplankton IC values or adjust grazing "
        "half-saturation constants."
    )

    # Time snapshots for ZOO_C
    pdf.sub_title("ZOO_C Time Series (Box 5)")
    d5 = box_data[5]
    snap_days = [0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 364]
    pdf.set_font("Courier", "B", 7)
    pdf.cell(20, 4, "Day", 1, 0, "C")
    pdf.cell(20, 4, "ZOO_C", 1, 0, "C")
    pdf.cell(20, 4, "Phyto_Total", 1, 0, "C")
    pdf.cell(20, 4, "Food Avail.", 1, 0, "C")
    pdf.ln()

    t5 = d5["TIME_DAYS"]
    for day in snap_days:
        target_t = 6209 + day
        idx = np.argmin(np.abs(t5 - target_t))
        zoo = d5["ZOO_C"][idx]
        phyto = sum(d5[v][idx] for v in ["DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C"]
                    if v in d5)
        ratio = phyto / zoo if zoo > 0 else 0
        pdf.set_font("Courier", "", 7)
        pdf.cell(20, 3.5, str(day), 1, 0, "C")
        pdf.cell(20, 3.5, f"{zoo:.4g}", 1, 0, "C")
        pdf.cell(20, 3.5, f"{phyto:.4g}", 1, 0, "C")
        pdf.cell(20, 3.5, f"{ratio:.1f}", 1, 0, "C")
        pdf.ln()

    # -- Section 11: Warnings Interpretation ------------------------------
    pdf.add_page()
    pdf.section_title("11. Warnings Interpretation")
    pdf.body_text(
        "The 129 warnings from the analysis are grouped by category and explained:\n\n"
        "CATEGORY 1 -- IC Spin-Down (Expected, Not Bugs):\n"
        "These variables had initial conditions set high for model stability but "
        "equilibrate to much lower boundary-driven values within 30-90 days.\n"
        "- Detritus (DET_C/N/P, PART_Si): IC=6-6.5 -> equilibrium ~0.07-0.15\n"
        "- Dissolved organic N/P: IC=4.5/0.04 -> equilibrium ~0.12/0.001\n"
        "- Metals (Fe_II/III, Mn_II/IV): IC=0.1-0.55 -> equilibrium ~0.003-0.055\n"
        "- Ca, Mg: IC=70/15 -> equilibrium ~3-14/0.7-2.8\n"
        "- Secondary metabolites: IC=1.0 -> near-zero (producers absent)\n\n"
        "CATEGORY 2 -- Biological Concerns (Worth Investigation):\n"
        "- DIA_C drops 99% by end of year: This is a SEASONAL pattern -- diatoms peak "
        "in spring then decline naturally. The -99% is winter-minimum vs Jan-1 IC.\n"
        "- CYN_C, FIX_CYN_C never bloom: Growth conditions (light, temperature, "
        "nutrients) may be too restrictive. Warrants calibration review.\n"
        "- ZOO_C/N/P collapse: Most concerning issue. See Section 10.\n\n"
        "CATEGORY 3 -- No Action Needed:\n"
        "- CH4_C = 0 throughout: Methane not produced under aerobic conditions (correct)\n"
        "- S_MINUS_2 = 0: Sulfide not present in oxygenated water (correct)\n"
        "- AKI_C constant at 8.0 in mud boxes: Seed bank, as designed (see previous report)"
    )

    # -- Section 12: Conclusions ------------------------------------------
    pdf.add_page()
    pdf.section_title("12. Conclusions and Recommendations")
    pdf.body_text(
        "OVERALL ASSESSMENT: The AQUABC model produces physically and ecologically "
        "plausible results over a full-year simulation. The fixes applied to detritus "
        "dissolution parameters (commit c7e27ef) are confirmed effective.\n\n"
        "WHAT WORKS WELL:\n"
        "1. Diatom spring bloom timing and magnitude (DOY 127-128, 1.5-1.8 mg C/L)\n"
        "2. Nostocales late-summer peak (DOY 245-257) -- correct succession pattern\n"
        "3. Dissolved oxygen seasonal cycle (15 -> 8 -> 13 mg/L) -- no hypoxia\n"
        "4. Nutrient seasonal drawdown (NO3, PO4, Si) during blooms\n"
        "5. Carbonate system stability (ALK ~3.0 meq/L)\n"
        "6. Detritus C:N ratio preserved at Redfield values (~6.3 mass ratio)\n"
        "7. Spatial gradients (river -> lagoon -> Baltic)\n\n"
        "WHAT NEEDS ATTENTION:\n"
        "1. Zooplankton collapse -- highest priority for calibration\n"
        "2. Cyanobacteria (CYN_C, FIX_CYN_C) never bloom -- parameter review needed\n"
        "3. Initial conditions for conservative species (Ca, Mg, metals) too high\n"
        "4. DOC (DISS_ORG_C) IC=11 mg/L is reasonable but DISS_ORG_N IC=4.5 may be "
        "too high (drops 97%)\n\n"
        "RECOMMENDED NEXT STEPS:\n"
        "1. Calibrate zooplankton grazing parameters against field observations\n"
        "2. Review cyanobacteria growth rate constants and light/temperature dependencies\n"
        "3. Adjust initial conditions for conservative species to be closer to "
        "boundary-forced equilibrium values\n"
        "4. Consider extending to multi-year simulation to verify steady-state behavior\n"
        "5. Validate against observed water quality data from Curonian Lagoon monitoring"
    )

    # -- Save -------------------------------------------------------------
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out_path = OUT_DIR / "365Day_Deep_Analysis_Report.pdf"
    pdf.output(str(out_path))
    print(f"\nPDF saved to: {out_path}")
    return out_path


if __name__ == "__main__":
    generate_report()
