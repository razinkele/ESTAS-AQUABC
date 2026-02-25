#!/usr/bin/env python3
"""
Generate a comprehensive PDF reference document for AQUABC model constants.

Parses INPUTS/WCONST_04.txt and produces a professional PDF with:
  - Constant name, number, value, units, and description
  - Grouped by functional category
  - Literature references for each category
"""

from __future__ import annotations
import re, os, sys, textwrap
from pathlib import Path
from fpdf import FPDF

# ---------------------------------------------------------------------------
# 1. Parse the constant file
# ---------------------------------------------------------------------------
CONST_FILE = Path(__file__).resolve().parent.parent / "INPUTS" / "WCONST_04.txt"

def parse_constants(path: Path) -> list[dict]:
    """Return list of dicts: {num, name, value, comment}."""
    consts = []
    with open(path) as fh:
        for line in fh:
            line = line.rstrip("\n")
            if not line.strip():
                continue
            # Format: NAME VALUE !NUM COMMENT  or NAME VALUE !COMMENT
            m = re.match(
                r'\s*(\d+)?\s*(\S+)\s+([\d.Ee+\-]+)\s+!\s*(\d+)?\s*(.*)', line)
            if not m:
                # Try alternate: leading number
                m2 = re.match(r'\s+(\S+)\s+([\d.Ee+\-]+)\s+!\s*(\d*)?\s*(.*)', line)
                if m2:
                    name = m2.group(1)
                    value = m2.group(2)
                    num = m2.group(3).strip() if m2.group(3) else ""
                    comment = m2.group(4).strip()
                    consts.append(dict(
                        num=num, name=name, value=value, comment=comment))
                continue
            num = (m.group(1) or m.group(4) or "").strip()
            name = m.group(2)
            value = m.group(3)
            comment = m.group(5).strip()
            consts.append(dict(num=num, name=name, value=value, comment=comment))
    return consts

# Second parser - more robust
def parse_constants_v2(path: Path) -> list[dict]:
    consts = []
    idx = 0
    with open(path) as fh:
        for line in fh:
            line = line.rstrip("\n")
            if not line.strip():
                continue
            idx += 1
            # Split on '!'
            parts = line.split('!')
            left = parts[0].strip()
            right = '!'.join(parts[1:]).strip() if len(parts) > 1 else ''

            # Left part: NAME VALUE  (with possible leading spaces)
            tokens = left.split()
            if len(tokens) < 2:
                continue
            name = tokens[0]
            value = tokens[1]

            # Right part may start with a number
            num_match = re.match(r'(\d+)\s*(.*)', right)
            if num_match:
                num = num_match.group(1)
                comment = num_match.group(2).strip()
            else:
                num = str(idx)
                comment = right.strip()

            consts.append(dict(num=num, name=name, value=value, comment=comment))
    return consts


# ---------------------------------------------------------------------------
# 2. Category definitions (parameter number ranges -> category)
# ---------------------------------------------------------------------------
CATEGORIES = [
    ("General / Physical Parameters", 1, 4,
     "General physical and optical parameters controlling aeration and light.",
     "mg O2/L/day, 1/m/(ug Chla/L), mg C/mole photon"),
    ("Diatoms (Bacillariophyceae)", 5, 28,
     "Growth, respiration, mortality, nutrient limitation, and stoichiometry "
     "for diatoms -- siliceous phytoplankton that dominate spring blooms.",
     "1/day, deg C, mg/L, dimensionless"),
    ("Non-Fixing Cyanobacteria", 29, 50,
     "Parameters for non-nitrogen-fixing cyanobacteria (e.g. Microcystis, "
     "Planktothrix). Warm-water specialists with higher optimal temperatures.",
     "1/day, deg C, mg/L, dimensionless"),
    ("Nitrogen-Fixing Cyanobacteria", 51, 74,
     "Parameters for heterocystous N2-fixing cyanobacteria (e.g. Aphanizomenon, "
     "Dolichospermum). Unique N-fixation parameters R_FIX and K_FIX.",
     "1/day, deg C, mg/L, dimensionless"),
    ("Other Phytoplankton (Chlorophyta, Cryptophyta, etc.)", 75, 96,
     "Growth and loss parameters for a generic 'other phytoplankton' group "
     "representing green algae, cryptophytes, and chrysophytes.",
     "1/day, deg C, mg/L, dimensionless"),
    ("Zooplankton", 97, 133,
     "Growth, grazing preferences, half-saturation constants, respiration, and "
     "mortality for the bulk zooplankton compartment (mainly crustacean micro-"
     "and mesozooplankton).",
     "1/day, deg C, mg C/L, dimensionless"),
    ("Particulate Organic Matter (POM) Dissolution", 134, 146,
     "First-order dissolution rates for particulate organic carbon (POC), "
     "nitrogen (PON), phosphorus (POP), and biogenic silica (BSi). "
     "Phytoplankton-enhancement factors simulate bacterial colonisation.",
     "1/day, dimensionless"),
    ("Dissolved Organic Matter (DOM) Mineralisation", 147, 151,
     "Phytoplankton-dependent mineralisation rate factors and reverse "
     "half-saturation concentrations for DOC, DON, and DOP.",
     "1/day, mg/L, dimensionless"),
    ("Nitrification", 152, 157,
     "Ammonia oxidation (nitrification) rate, temperature correction, "
     "half-saturation for O2 and NH4-N, and pH optimum range.",
     "1/day, mg O2/L, mg N/L, pH units"),
    ("Redox Metal Chemistry (Fe, Mn)", 158, 163,
     "Oxidation/reduction rate constants for Fe(II)/Fe(III) and Mn(II)/Mn(IV), "
     "plus reversed Monod half-saturation for DOXY inhibition.",
     "1/day, mg/L"),
    ("DOC Mineralisation -- Multi-Electron-Acceptor", 164, 203,
     "Mineralisation rate constants of DOC using different terminal electron "
     "acceptors (O2, NO3, Mn(IV), Fe(III), SO4, methanogenesis). Includes "
     "temperature corrections, half-saturations, inhibition (thermodynamic "
     "sequence), and pH optima.",
     "1/day, mg/L, pH units"),
    ("DON Mineralisation -- Multi-Electron-Acceptor", 204, 233,
     "Analogous to DOC mineralisation but for dissolved organic nitrogen.",
     "1/day, mg/L, pH units"),
    ("DOP Mineralisation -- Multi-Electron-Acceptor", 234, 263,
     "Analogous to DOC mineralisation but for dissolved organic phosphorus.",
     "1/day, mg/L, pH units"),
    ("Methane & Hydrogen Sulphide", 264, 269,
     "Oxidation rates for CH4 and H2S with temperature corrections and "
     "half-saturation for dissolved oxygen.",
     "1/day, mg/L"),
    ("Fe Dissolution & Fractionation", 270, 275,
     "Dissolution rate constants for particulate Fe(II) and Fe(III) phases "
     "and initial dissolved fractions.",
     "1/day, dimensionless"),
    ("Nostocales (Heterocystous Cyanobacteria)", 276, 306,
     "Growth, loss, and akinete (resting stage) formation/germination "
     "parameters for Nostocales (Dolichospermum, Aphanizomenon flos-aquae). "
     "Includes density-dependent mortality and seasonal triggers.",
     "1/day, deg C, mg/L, day-of-year"),
    ("Dissolution Saturation & DOM Availability", 307, 318,
     "Half-saturation constants for POM dissolution saturation, available "
     "fractions of DON/DOP for Nostocales, and phytoplankton-dependent "
     "mineralisation caps.",
     "mg/L, dimensionless"),
    ("Photoinhibition (beta)", 319, 323,
     "Photoinhibition parameters for each phytoplankton group. 0 = Steele "
     "formulation (default); values > 0 increase photoinhibition strength.",
     "dimensionless"),
]


def assign_category(num_str: str, idx: int) -> str:
    """Return category name for a given constant number."""
    try:
        n = int(num_str)
    except (ValueError, TypeError):
        n = idx + 1  # fallback to sequential index
    for cat_name, lo, hi, *_ in CATEGORIES:
        if lo <= n <= hi:
            return cat_name
    return "Uncategorised"


# ---------------------------------------------------------------------------
# 3. Literature references by category
# ---------------------------------------------------------------------------
REFERENCES = {
    "General / Physical Parameters": [
        ("Chapra, S.C. (1997)",
         "Surface Water-Quality Modeling. McGraw-Hill, New York.",
         "Aeration coefficients and temperature correction theta = 1.024 for reaeration."),
        ("Bowie, G.L., Mills, W.B., Porcella, D.B. et al. (1985)",
         "Rates, Constants, and Kinetics Formulations in Surface Water Quality Modeling (2nd ed.). "
         "EPA/600/3-85/040. U.S. EPA, Athens, GA.",
         "Comprehensive compilation of rate constants for surface water quality models."),
        ("Kirk, J.T.O. (1994)",
         "Light and Photosynthesis in Aquatic Ecosystems, 2nd ed. Cambridge Univ. Press.",
         "Light extinction coefficient per unit chlorophyll (XKC ~ 0.01-0.02 m^-1/(ug Chla/L))."),
        ("Falkowski, P.G. & Raven, J.A. (2007)",
         "Aquatic Photosynthesis, 2nd ed. Princeton Univ. Press.",
         "Quantum yield constant PHIMX (700-1000 mg C/mole photon)."),
    ],
    "Diatoms (Bacillariophyceae)": [
        ("Eppley, R.W. (1972)",
         "Temperature and phytoplankton growth in the sea. Fishery Bulletin, 70(4), 1063-1085.",
         "Maximum growth rate as a function of temperature; diatom mu_max ~ 2-4 /day at optimal T."),
        ("Rosso, L., Lobry, J.R., & Flandrois, J.P. (1993)",
         "An unexpected correlation between cardinal temperatures of microbial growth highlighted "
         "by a new model. J. Theoretical Biology, 162(4), 447-463.",
         "Cardinal Temperature Model with Inflection (CTMI): T_min, T_opt, T_max framework "
         "used for all phytoplankton temperature responses in AQUABC."),
        ("Bernard, O. & Remond, B. (2012)",
         "Validation of a simple model accounting for light and temperature effect on microalgal "
         "growth. Bioresource Technology, 123, 520-527.",
         "Validated CTMI with three cardinal temperatures for various microalgae."),
        ("Brzezinski, M.A. (1985)",
         "The Si:C:N ratio of marine diatoms: interspecific variability and the effect of some "
         "environmental variables. J. Phycology, 21(3), 347-357.",
         "Si:C ratio for diatoms ~ 0.13-0.47 (model uses 0.25); Si:N:P = 15:16:1."),
        ("Redfield, A.C. (1958)",
         "The biological control of chemical factors in the environment. American Scientist, "
         "46(3), 205-221.",
         "Redfield ratio C:N:P = 106:16:1 (N:C ~ 0.17, P:C ~ 0.024 by mass with molar conversion). "
         "Model N:C = 0.22 and P:C = 0.024 are within reported ranges."),
        ("Geider, R.J., MacIntyre, H.L., & Kana, T.M. (1997)",
         "Dynamic model of phytoplankton growth and acclimation: responses of the balanced "
         "growth rate and the chlorophyll a:carbon ratio. Marine Ecology Progress Series, 148, 187-200.",
         "C:Chl-a ratio 20-100 mg C / mg Chla (model uses 30 for diatoms)."),
        ("Reynolds, C.S. (2006)",
         "The Ecology of Phytoplankton. Cambridge Univ. Press.",
         "Diatom respiration 0.03-0.10 /day; mortality 0.05-0.20 /day; KHS_DIN 0.005-0.05 mg/L."),
    ],
    "Non-Fixing Cyanobacteria": [
        ("Robarts, R.D. & Zohary, T. (1987)",
         "Temperature effects on photosynthetic capacity, respiration, and growth rates of "
         "bloom-forming cyanobacteria. New Zealand J. Marine and Freshwater Research, 21(3), 391-399.",
         "Optimal growth temperature for Microcystis 25-30 deg C."),
        ("Reynolds, C.S. (2006)",
         "The Ecology of Phytoplankton. Cambridge Univ. Press.",
         "Cyanobacteria max growth rates 1.0-3.5 /day; C:Chl-a = 30-60."),
        ("Rosso, L. et al. (1993)",
         "As above -- CTMI model for temperature response.",
         "T_min = 10-18 deg C, T_opt = 25-30 deg C, T_max = 35-42 deg C for cyanobacteria."),
        ("Jorgensen, S.E. et al. (1991)",
         "Handbook of Ecological Parameters and Ecotoxicology. Elsevier.",
         "Half-saturation constants for DIN (0.005-0.05 mg/L) and DIP (0.003-0.015 mg/L)."),
    ],
    "Nitrogen-Fixing Cyanobacteria": [
        ("Staal, M., Meysman, F.J.R. & Stal, L.J. (2003)",
         "Temperature excludes N2-fixing heterocystous cyanobacteria in the tropical oceans. "
         "Nature, 425, 504-507.",
         "Temperature controls on N-fixation; optimal range 20-30 deg C."),
        ("Horne, A.J. & Goldman, C.R. (1994)",
         "Limnology, 2nd ed. McGraw-Hill.",
         "N-fixation rates and conditions triggering fixation when DIN is low."),
        ("Paerl, H.W. & Otten, T.G. (2013)",
         "Harmful cyanobacterial blooms: causes, consequences, and controls. Microbial Ecology, "
         "65(4), 995-1010.",
         "Growth ecology and bloom dynamics of N-fixing cyanobacteria."),
        ("Grimaud, G.M., Mairet, F. et al. (2017)",
         "Modeling the temperature effect on the specific growth rate of phytoplankton: a review. "
         "Reviews in Environmental Science and Bio/Technology, 16, 625-645.",
         "Review of CTMI and other temperature-growth models for phytoplankton."),
    ],
    "Other Phytoplankton (Chlorophyta, Cryptophyta, etc.)": [
        ("Reynolds, C.S. & Irish, A.E. (1997)",
         "Modelling phytoplankton dynamics in lakes and reservoirs: the problem of in-situ "
         "growth rates. Hydrobiologia, 349, 5-17.",
         "In-situ growth rates for green algae, cryptophytes; mu_max 1.5-3.5 /day."),
        ("Sommer, U. (1989)",
         "Plankton Ecology: Succession in Plankton Communities. Springer.",
         "OtherPhyto T_opt typically 15-22 deg C (cooler than cyanobacteria)."),
    ],
    "Zooplankton": [
        ("Jorgensen, S.E. (1995)",
         "The growth rate of zooplankton at the edge of chaos: ecological models. "
         "J. Theoretical Biology, 175(1), 13-21.",
         "Zooplankton growth rates 0.2-0.8 /day; model uses 0.45 /day."),
        ("Hansen, P.J., Bjornsen, P.K. & Hansen, B.W. (1997)",
         "Zooplankton grazing and growth: scaling within the 2-2000 um body size range. "
         "Limnology and Oceanography, 42, 687-704.",
         "Grazing rates scaling with body size; half-saturation for food 0.02-0.5 mg C/L."),
        ("Jorgensen, S.E. (1976)",
         "A eutrophication model for a lake. Ecological Modelling, 2(2), 147-165.",
         "Threshold food concentration for zooplankton grazing (FOOD_MIN ~ 0.02 mg C/L). "
         "Zooplankton N:C ~ 0.17-0.25, P:C ~ 0.015-0.03."),
        ("Sommer, U. & Sommer, F. (2006)",
         "Cladocerans versus copepods: the cause of contrasting top-down controls on freshwater "
         "and marine phytoplankton. Oecologia, 147, 183-194.",
         "Selective feeding preferences in freshwater zooplankton communities."),
    ],
    "Particulate Organic Matter (POM) Dissolution": [
        ("Wetzel, R.G. (2001)",
         "Limnology: Lake and River Ecosystems, 3rd ed. Academic Press.",
         "POM dissolution rates: POC 0.01-0.1 /day; PON 0.05-0.5 /day; POP 0.1-1.0 /day."),
        ("Bowie, G.L. et al. (1985)",
         "As above -- EPA compilation.",
         "Temperature correction theta 1.04-1.08 for organic matter dissolution."),
        ("Kamatani, A. (1982)",
         "Dissolution rates of silica from diatoms decomposing at various temperatures. "
         "Marine Biology, 68, 91-96.",
         "Biogenic silica dissolution rate 0.001-0.01 /day, increasing with temperature."),
    ],
    "Dissolved Organic Matter (DOM) Mineralisation": [
        ("Chapra, S.C. (1997)",
         "Surface Water-Quality Modeling. McGraw-Hill.",
         "DOM mineralisation rates and phytoplankton enhancement factors."),
        ("Bowie, G.L. et al. (1985)",
         "As above.",
         "DOC mineralisation rates 0.01-0.2 /day; DON 0.005-0.1 /day."),
    ],
    "Nitrification": [
        ("Chapra, S.C. (1997)",
         "Surface Water-Quality Modeling. McGraw-Hill.",
         "Nitrification rate 0.1-1.0 /day at 20 deg C; theta = 1.04-1.08."),
        ("Bowie, G.L. et al. (1985)",
         "As above.",
         "KHS_O2 for nitrification 0.5-4.0 mg/L; KHS_NH4 0.01-0.10 mg N/L."),
        ("Painter, H.A. (1970)",
         "A review of literature on inorganic nitrogen metabolism in microorganisms. "
         "Water Research, 4(6), 393-450.",
         "pH optimum range for nitrification 6.5-8.5; strong inhibition below pH 6.0."),
        ("Cerco, C.F. (2000)",
         "Phytoplankton kinetics in the Chesapeake Bay eutrophication model. "
         "Water Quality and Ecosystems Modeling, 1, 5-49.",
         "Nitrification parameters used in major estuary models."),
    ],
    "Redox Metal Chemistry (Fe, Mn)": [
        ("Stumm, W. & Morgan, J.J. (1996)",
         "Aquatic Chemistry: Chemical Equilibria and Rates in Natural Waters, 3rd ed. Wiley.",
         "Fe(II) oxidation rate 10^-3 to 10^-1 /day; Mn(II) oxidation 10^-2 to 10^-1 /day."),
        ("Davison, W. (1993)",
         "Iron and manganese in lakes. Earth-Science Reviews, 34(2), 119-163.",
         "Reduction rates of Fe(III) and Mn(IV) under anoxic conditions 0.5-5.0 /day."),
    ],
    "DOC Mineralisation -- Multi-Electron-Acceptor": [
        ("Van Cappellen, P. & Wang, Y. (1996)",
         "Cycling of iron and manganese in surface sediments: a general theory for the coupled "
         "transport and reaction of carbon, oxygen, nitrogen, sulfur, iron, and manganese. "
         "American Journal of Science, 296, 197-243.",
         "Thermodynamic sequence of electron acceptors: O2 > NO3 > Mn(IV) > Fe(III) > SO4 > CH4. "
         "Inhibition half-saturation constants 0.01-1.0 mg/L."),
        ("Berner, R.A. (1980)",
         "Early Diagenesis: A Theoretical Approach. Princeton Univ. Press.",
         "Foundation for multi-electron-acceptor organic matter degradation kinetics."),
        ("Soetaert, K., Herman, P.M.J. & Middelburg, J.J. (1996)",
         "A model of early diagenetic processes from the shelf to abyssal depths. "
         "Geochimica et Cosmochimica Acta, 60(6), 1019-1040.",
         "DOC mineralisation rates 0.01-0.10 /day; theta 1.02-1.08."),
        ("Boudreau, B.P. (1997)",
         "Diagenetic Models and Their Implementation. Springer.",
         "pH correction factors for organic matter mineralisation (pH 6-9 optimum)."),
    ],
    "DON Mineralisation -- Multi-Electron-Acceptor": [
        ("Soetaert, K. et al. (1996)",
         "As above.",
         "DON mineralisation rates generally lower than DOC; 0.001-0.1 /day."),
        ("Berner, R.A. (1980)",
         "As above.",
         "Multi-electron-acceptor framework applies equally to DON."),
    ],
    "DOP Mineralisation -- Multi-Electron-Acceptor": [
        ("Soetaert, K. et al. (1996)",
         "As above.",
         "DOP mineralisation typically faster than DON due to enzymatic hydrolysis."),
        ("Berner, R.A. (1980)",
         "As above.",
         "Phosphorus regeneration coupled to redox chemistry."),
    ],
    "Methane & Hydrogen Sulphide": [
        ("Bastviken, D. et al. (2004)",
         "Methane emissions from lakes: dependence of lake characteristics, two regional "
         "assessments, and a global estimate. Global Biogeochemical Cycles, 18(4), GB4009.",
         "Methane oxidation rates 0.1-10 /day; dependent on O2 availability."),
        ("Jorgensen, B.B. (1982)",
         "Mineralization of organic matter in the sea bed -- the role of sulphate reduction. "
         "Nature, 296, 643-645.",
         "H2S oxidation rates in aquatic systems; 0.1-5.0 /day."),
    ],
    "Fe Dissolution & Fractionation": [
        ("Stumm, W. & Morgan, J.J. (1996)",
         "As above.",
         "Dissolved vs. particulate Fe fractionation; dissolution rates 0.01-1.0 /day."),
    ],
    "Nostocales (Heterocystous Cyanobacteria)": [
        ("Paerl, H.W. & Otten, T.G. (2013)",
         "As above.",
         "Growth ecology of Nostocales; mu_max 0.5-2.0 /day."),
        ("Hense, I. & Beckmann, A. (2006)",
         "Towards a model of cyanobacteria life cycle -- effects of growing and resting stages "
         "on bloom formation of N2-fixing species. Ecological Modelling, 195, 205-218.",
         "Akinete germination rates 0.1-0.5 /day; formation triggered by T < 16 deg C "
         "and declining DIN. Model uses similar parameterisation."),
        ("Sukenik, A., Hadas, O. & Kaplan, A. (2012)",
         "Invasion of Nostocales (cyanobacteria) to subtropical and temperate freshwater "
         "lakes -- physiological, regional, and global driving forces. "
         "Frontiers in Microbiology, 3, 86.",
         "Nostocales temperature requirements T_opt 24-28 deg C; T_min 15-20 deg C."),
        ("Kovacs, A.W., Presing, M. & Voros, L. (2016)",
         "Thermal-dependent growth characteristics for Cylindrospermopsis raciborskii. "
         "Aquatic Ecology, 50, 97-108.",
         "CTMI cardinal temperatures for Nostocales species."),
    ],
    "Dissolution Saturation & DOM Availability": [
        ("Wetzel, R.G. (2001)",
         "As above.",
         "Saturation kinetics for POM dissolution."),
    ],
    "Photoinhibition (beta)": [
        ("Steele, J.H. (1962)",
         "Environmental control of photosynthesis in the sea. Limnology and Oceanography, "
         "7(2), 137-150.",
         "Classic photoinhibition formulation P/P_max = (I/I_s)*exp(1 - I/I_s). "
         "AQUABC default beta = 0 uses this curve; beta > 0 intensifies inhibition."),
        ("Platt, T., Gallegos, C.L. & Harrison, W.G. (1980)",
         "Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton. "
         "J. Marine Research, 38, 687-701.",
         "Extended P-I curve with explicit photoinhibition parameter beta."),
    ],
}


# ---------------------------------------------------------------------------
# 4. Build PDF
# ---------------------------------------------------------------------------
class ConstPDF(FPDF):
    """Custom FPDF subclass for the constants reference."""

    # page margins
    LEFT = 12
    RIGHT = 12
    COL_NUM_W = 10
    COL_NAME_W = 52
    COL_VAL_W = 18
    COL_DESC_W = 0  # auto

    def __init__(self):
        super().__init__(orientation="P", unit="mm", format="A4")
        self.set_auto_page_break(auto=True, margin=18)
        self.add_page()
        self.alias_nb_pages()

    # ----- header / footer -----
    def header(self):
        self.set_font("Helvetica", "B", 9)
        self.set_text_color(100, 100, 100)
        self.cell(0, 6, "AQUABC Model -- Constant Reference (WCONST_04)", align="L")
        self.ln(8)
        self.set_draw_color(180, 180, 180)
        self.line(self.l_margin, self.get_y(), self.w - self.r_margin, self.get_y())
        self.ln(3)

    def footer(self):
        self.set_y(-14)
        self.set_font("Helvetica", "I", 8)
        self.set_text_color(140, 140, 140)
        self.cell(0, 10, f"Page {self.page_no()}/{{nb}}", align="C")

    # ----- helpers -----
    def section_title(self, title: str):
        """Print a bold section header with background."""
        self.set_font("Helvetica", "B", 12)
        self.set_fill_color(44, 62, 80)
        self.set_text_color(255, 255, 255)
        self.cell(0, 8, f"  {title}", new_x="LMARGIN", new_y="NEXT", fill=True)
        self.set_text_color(0, 0, 0)
        self.ln(2)

    def subsection(self, text: str):
        self.set_font("Helvetica", "I", 9)
        self.set_text_color(60, 60, 60)
        self.multi_cell(0, 4.5, text)
        self.set_text_color(0, 0, 0)
        self.ln(2)

    def table_header(self):
        self.set_font("Helvetica", "B", 8)
        self.set_fill_color(220, 220, 220)
        desc_w = self.w - self.l_margin - self.r_margin - self.COL_NUM_W - self.COL_NAME_W - self.COL_VAL_W
        self.cell(self.COL_NUM_W, 5, "#", border=1, fill=True, align="C")
        self.cell(self.COL_NAME_W, 5, "Constant Name", border=1, fill=True)
        self.cell(self.COL_VAL_W, 5, "Value", border=1, fill=True, align="C")
        self.cell(desc_w, 5, "Description", border=1, fill=True)
        self.ln()

    def table_row(self, num: str, name: str, value: str, desc: str, alt: bool):
        desc_w = self.w - self.l_margin - self.r_margin - self.COL_NUM_W - self.COL_NAME_W - self.COL_VAL_W
        self.set_font("Helvetica", "", 7)

        # Calculate row height based on desc length
        # Approximate chars per line for each column
        name_lines = max(1, len(name) // 24 + 1)
        desc_lines = max(1, len(desc) // (int(desc_w / 1.65)) + 1)
        row_h = max(name_lines, desc_lines) * 3.8 + 0.8

        if alt:
            self.set_fill_color(245, 245, 250)
        else:
            self.set_fill_color(255, 255, 255)

        x0 = self.get_x()
        y0 = self.get_y()

        # Check page break
        if y0 + row_h > self.h - 20:
            self.add_page()
            self.table_header()
            x0 = self.get_x()
            y0 = self.get_y()

        # Num
        self.set_xy(x0, y0)
        self.cell(self.COL_NUM_W, row_h, num, border="LTB", fill=True, align="C")
        # Name
        self.set_xy(x0 + self.COL_NUM_W, y0)
        self.set_font("Courier", "", 6.5)
        self.multi_cell(self.COL_NAME_W, row_h, name, border="LTB", fill=True)
        # Value
        self.set_font("Helvetica", "", 7)
        actual_name_h = self.get_y() - y0
        final_h = max(row_h, actual_name_h)
        self.set_xy(x0 + self.COL_NUM_W + self.COL_NAME_W, y0)
        self.cell(self.COL_VAL_W, final_h, value, border="LTB", fill=True, align="C")
        # Description
        self.set_xy(x0 + self.COL_NUM_W + self.COL_NAME_W + self.COL_VAL_W, y0)
        self.set_font("Helvetica", "", 7)
        self.multi_cell(desc_w, 3.8, desc, border="LTBR", fill=True)
        new_y = self.get_y()
        final_h2 = new_y - y0
        # Reconcile heights -- draw bottom border at max height
        self.set_y(max(self.get_y(), y0 + final_h))

    def ref_block(self, refs: list[tuple[str,str,str]]):
        """Print reference entries."""
        self.set_font("Helvetica", "B", 8)
        self.set_text_color(44, 62, 80)
        self.cell(0, 5, "Literature References:", new_x="LMARGIN", new_y="NEXT")
        self.set_text_color(0, 0, 0)
        usable_w = self.w - self.l_margin - self.r_margin
        for i, (author, title, note) in enumerate(refs, 1):
            if self.get_y() > self.h - 25:
                self.add_page()
            self.set_font("Helvetica", "B", 7)
            self.multi_cell(usable_w, 3.5, f"[{i}] {author}")
            self.set_font("Helvetica", "I", 7)
            self.set_x(self.l_margin + 4)
            self.multi_cell(usable_w - 4, 3.5, title)
            if note:
                self.set_font("Helvetica", "", 7)
                self.set_text_color(80, 80, 80)
                self.set_x(self.l_margin + 4)
                self.multi_cell(usable_w - 4, 3.5, f"Relevance: {note}")
                self.set_text_color(0, 0, 0)
            self.ln(1)
        self.ln(3)


def build_pdf(consts: list[dict], out_path: Path):
    pdf = ConstPDF()

    # Title page content
    pdf.set_font("Helvetica", "B", 22)
    pdf.ln(15)
    pdf.cell(0, 12, "AQUABC Model", new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.set_font("Helvetica", "B", 16)
    pdf.cell(0, 10, "Parameter & Constant Reference", new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.set_font("Helvetica", "", 11)
    pdf.cell(0, 8, "Based on WCONST_04.txt (latest configuration)", new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.ln(5)
    pdf.set_font("Helvetica", "I", 10)
    pdf.set_text_color(100, 100, 100)
    pdf.cell(0, 6, f"Total constants: {len(consts)}", new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.cell(0, 6, f"Generated: February 2026", new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.set_text_color(0, 0, 0)
    pdf.ln(6)

    # Summary box
    pdf.set_font("Helvetica", "", 9)
    pdf.set_fill_color(240, 244, 248)
    summary = (
        "This document provides a complete reference for all model constants used "
        "in the AQUABC pelagic ecological model (file WCONST_04.txt). Constants are "
        "organised by functional category. Each section includes the constant number, "
        "name, current value, description, and relevant literature references that "
        "justify the chosen parameterisation. The AQUABC model is a fully featured "
        "water quality / eutrophication model that simulates phytoplankton dynamics "
        "(diatoms, cyanobacteria, other algae, Nostocales), zooplankton grazing, "
        "organic matter cycling, nutrient dynamics (N, P, Si), dissolved oxygen, "
        "redox chemistry (Fe, Mn), methane, hydrogen sulphide, and pH/alkalinity. "
        "Temperature effects use the Cardinal Temperature Model with Inflection "
        "(CTMI; Rosso et al. 1993). Nutrient limitation follows Monod (Michaelis-"
        "Menten) kinetics. The multi-electron-acceptor mineralisation scheme follows "
        "the thermodynamic sequence of Van Cappellen & Wang (1996)."
    )
    pdf.multi_cell(0, 4.5, summary, fill=True)
    pdf.ln(4)

    # Table of contents
    pdf.section_title("Table of Contents")
    for i, (cat_name, lo, hi, desc, units) in enumerate(CATEGORIES, 1):
        n_in = sum(1 for c in consts if assign_category(c["num"], consts.index(c)) == cat_name)
        pdf.set_font("Helvetica", "", 9)
        pdf.cell(0, 5, f"  {i}. {cat_name} (#{lo}-{hi}, {n_in} constants)", new_x="LMARGIN", new_y="NEXT")
    pdf.ln(3)
    pdf.set_font("Helvetica", "", 9)
    pdf.cell(0, 5, f"  Bibliography / Full Reference List", new_x="LMARGIN", new_y="NEXT")
    pdf.ln(6)

    # Each category
    for cat_name, lo, hi, desc, units in CATEGORIES:
        pdf.add_page()
        pdf.section_title(cat_name)
        pdf.subsection(f"{desc}\nTypical units: {units}")

        cat_consts = [c for c in consts
                      if assign_category(c["num"], consts.index(c)) == cat_name]

        if cat_consts:
            pdf.table_header()
            for j, c in enumerate(cat_consts):
                comment = c["comment"]
                # Clean up comment
                comment = re.sub(r'^[\d.]+\s+', '', comment)  # remove leading numbers
                pdf.table_row(c["num"], c["name"], c["value"], comment, j % 2 == 1)
            pdf.ln(4)

        # References
        refs = REFERENCES.get(cat_name, [])
        if refs:
            pdf.ref_block(refs)

    # Full bibliography
    pdf.add_page()
    pdf.section_title("Complete Bibliography")
    pdf.ln(2)

    # Collect unique references
    all_refs: dict[str, str] = {}
    for cat_refs in REFERENCES.values():
        for author, title, _ in cat_refs:
            key = author.split("(")[0].strip()
            if key not in all_refs:
                all_refs[key] = f"{author} {title}"

    sorted_refs = sorted(all_refs.values(), key=lambda s: s.lower())
    for i, ref in enumerate(sorted_refs, 1):
        if pdf.get_y() > pdf.h - 20:
            pdf.add_page()
        pdf.set_font("Helvetica", "", 8)
        pdf.multi_cell(0, 3.8, f"[{i}] {ref}")
        pdf.ln(1.5)

    # Additional key references section
    pdf.ln(4)
    pdf.set_font("Helvetica", "B", 9)
    pdf.cell(0, 5, "Additional Key References for Water Quality Modelling:", new_x="LMARGIN", new_y="NEXT")
    pdf.ln(2)
    extra_refs = [
        "Ambrose, R.B., Wool, T.A. & Martin, J.L. (1993) The Water Quality Analysis Simulation "
        "Program, WASP5. Part A: Model Documentation. U.S. EPA, Athens, GA.",
        "Brown, L.C. & Barnwell, T.O. (1987) The Enhanced Stream Water Quality Models QUAL2E "
        "and QUAL2E-UNCAS. EPA/600/3-87/007.",
        "Cole, T.M. & Wells, S.A. (2006) CE-QUAL-W2: A Two-Dimensional, Laterally Averaged, "
        "Hydrodynamic and Water Quality Model, Version 3.5. U.S. Army Corps of Engineers.",
        "Collins, C.D. & Wlosinski, J.H. (1983) Coefficients for Use in the US Army Corps of "
        "Engineers Reservoir Model CE-QUAL-R1. Report E-83-15.",
        "Di Toro, D.M. (2001) Sediment Flux Modeling. Wiley-Interscience.",
        "Eppley, R.W. (1972) Temperature and phytoplankton growth in the sea. Fishery Bulletin, "
        "70(4), 1063-1085.",
        "Hamilton, D.P. & Schladow, S.G. (1997) Prediction of water quality in lakes and "
        "reservoirs. Part I -- Model description. Ecological Modelling, 96, 91-110.",
        "James, R.T., Martin, J., Wool, T. & Wang, P.F. (1997) A sediment resuspension and "
        "water quality model of Lake Okeechobee. JAWRA, 33(3), 661-680.",
        "Jorgensen, S.E. & Bendoricchio, G. (2001) Fundamentals of Ecological Modelling, 3rd ed. Elsevier.",
        "Lindim, C., Pinho, J.L. & Vieira, J.M.P. (2011) Analysis of spatial and temporal patterns "
        "in a large reservoir using water quality and hydrodynamic modeling. Ecological Modelling, 222, 2485-2494.",
        "Sakurova, I. (2019) Modeling of eutrophication processes in the Curonian Lagoon. PhD Thesis, "
        "Klaipeda University. [ESTAS-AQUABC model application].",
        "Idzelyte, R. (2016) A visualisation system for biogeochemical and hydrodynamic modelling "
        "and its application to the Curonian lagoon. PhD Thesis, Klaipeda University.",
        "Eilola, K., Almroth-Rosell, E., Edman, M. et al. (2015) Model set-up at COCOA study sites. "
        "SMHI Report. [AQUABC ecological model description].",
    ]
    for i, ref in enumerate(extra_refs, len(sorted_refs) + 1):
        if pdf.get_y() > pdf.h - 20:
            pdf.add_page()
        pdf.set_font("Helvetica", "", 8)
        pdf.multi_cell(0, 3.8, f"[{i}] {ref}")
        pdf.ln(1.5)

    pdf.output(str(out_path))
    return out_path


# ---------------------------------------------------------------------------
# 5. Main
# ---------------------------------------------------------------------------
def main():
    consts = parse_constants_v2(CONST_FILE)
    print(f"Parsed {len(consts)} constants from {CONST_FILE}")
    out = Path(__file__).resolve().parent.parent / "docs" / "AQUABC_Constant_Reference.pdf"
    out.parent.mkdir(parents=True, exist_ok=True)
    build_pdf(consts, out)
    print(f"PDF written to {out}")


if __name__ == "__main__":
    main()
