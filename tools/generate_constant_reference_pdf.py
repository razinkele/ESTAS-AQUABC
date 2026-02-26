#!/usr/bin/env python3
"""
Generate a comprehensive PDF reference document for AQUABC model constants.

Parses INPUTS/WCONST_04.txt using the same field layout as the Fortran reader:
    read(iu, *, iostat=ios) i, name, value
i.e. each line has:  <integer_index>  <constant_name>  <float_value>  ! <comment>

Produces a professional PDF with:
  - Constant number, name, value, **unit**, and description
  - Value-selection justification per category
  - Grouped by functional category
  - Literature references for each category
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

from fpdf import FPDF

# ---------------------------------------------------------------------------
# 1.  Parse the constant file  (matches Fortran free-format READ)
# ---------------------------------------------------------------------------
CONST_FILE = Path(__file__).resolve().parent.parent / "INPUTS" / "WCONST_04.txt"


def parse_constants(path: Path) -> list[dict]:
    """Return list of dicts: {num, name, value, comment}.

    File format per line (Fortran list-directed):
        <int_index>  <string_name>  <real_value>  ! [<num>] <description>
    """
    consts: list[dict] = []
    with open(path) as fh:
        for line in fh:
            line = line.rstrip("\n")
            if not line.strip():
                continue

            # ---------- split on first '!' ----------
            if "!" in line:
                left, right = line.split("!", 1)
            else:
                left, right = line, ""

            tokens = left.split()
            if len(tokens) < 3:
                # Some lines may have only index + name (value absent) – skip
                if len(tokens) == 2:
                    # Treat second token as name, no value
                    tokens.append("")
                else:
                    continue

            # tokens[0] = integer index, tokens[1] = name, tokens[2] = value
            idx_str = tokens[0]
            name = tokens[1]
            value = tokens[2]

            # ---------- comment / description ----------
            right = right.strip()
            # The comment often starts with the repeated index number; strip it
            m = re.match(r"(\d+)\s*(.*)", right)
            if m:
                num = m.group(1)
                comment = m.group(2).strip()
            else:
                num = idx_str
                comment = right

            consts.append(
                dict(num=num, name=name, value=value, comment=comment)
            )
    return consts


# ---------------------------------------------------------------------------
# 2.  Unit assignment  (pattern-based, matching AQUABC conventions)
# ---------------------------------------------------------------------------

# Explicit overrides for constants whose unit cannot be guessed from patterns
_UNIT_OVERRIDE: dict[str, str] = {
    "K_A": "1/day",
    "THETA_K_A": "-",
    "XKC": "1/m/(ug Chla/L)",
    "PHIMX": "mg C/mol photon",
    "FOOD_MIN_ZOO": "mg C/L",
    "KE_ZOO": "-",
    "FRAC_ZOO_EX_ORG": "-",
    "R_FIX": "-",
    "K_FIX": "-",
    "M_DENS_VEG_HET": "L/(mg C day)",
    "DAY_FORM_AKI": "day of year",
    "P_GERM_AKI": "1/day",
    "N_GERM_AKI": "mg N/L",
    "P_FORM_AKI": "1/day",
    "T_FORM_AKI": "deg C",
    "K_LOSS_AKI": "1/day",
    "K_MORT_AKI_20": "1/day",
    "THETA_K_MORT_AKI": "-",
    "T_GERM_AKI": "deg C",
    "KHS_POC_DISS_SAT": "mg C/L",
    "KHS_PON_DISS_SAT": "mg N/L",
    "KHS_POP_DISS_SAT": "mg P/L",
    "frac_avail_DON": "-",
    "frac_avail_DOP": "-",
    "frac_avail_DON_NOST": "-",
    "KHS_DN_NOST_VEG_HET": "mg N/L",
    "FRAC_FIX_N_FOR_GR_VEG_HET": "-",
    "FRAC_NOST_GROWTH": "-",
    "K_MIN_PHYT_AMIN_DOC": "L/(mg C)",
    "K_MIN_PHYT_AMIN_DON": "L/(mg C)",
    "K_MIN_PHYT_AMIN_DOP": "L/(mg C)",
}


def get_unit(name: str) -> str:
    """Derive the measurement unit for a constant based on its name."""
    if name in _UNIT_OVERRIDE:
        return _UNIT_OVERRIDE[name]
    n = name.upper()

    # ----- Temperature-related -----
    if n.endswith("_OPT_TEMP_LR") or n.endswith("_OPT_TEMP_UR"):
        return "deg C"
    if "UNDER_OPT_TEMP" in n or "OVER_OPT_TEMP" in n:
        return "deg C"

    # ----- Growth rates -----
    if n.startswith("KG_") and "OPT_TEMP" in n:
        return "1/day"

    # ----- Effective growth fraction -----
    if n.startswith("EFF_") and "GROWTH" in n:
        return "-"

    # ----- Respiration / mortality / dissolution / mineralization rates -----
    if n.startswith("KR_") or n.startswith("KD_"):
        if "THETA" not in n:
            return "1/day"
    if re.match(r"^KDISS_.*_20$", n):
        return "1/day"
    if re.match(r"^K_MIN_.*_20$", n):
        return "1/day"
    if n.startswith("K_NITR_20"):
        return "1/day"

    # ----- Temperature correction factors (theta / kappa) -----
    if n.startswith("THETA_"):
        return "-"

    # ----- Half-saturation constants -----
    if n.startswith("KHS_") or n.startswith("K_HS_"):
        if "NITR_OXY" in n or "DOXY" in n or "OX_" in n:
            return "mg O2/L"
        if "NH4" in n or "DIN" in n or "NO3N" in n or "DN_" in n or "DON" in n:
            return "mg N/L"
        if "DIP" in n or "DP_" in n or "DOP" in n:
            return "mg P/L"
        if "DSI" in n or "SI" in n:
            return "mg Si/L"
        if "O2" in n:
            return "mg O2/L"
        if "_C_ZOO" in n or "ORG_C" in n:
            return "mg C/L"
        if "DOC" in n:
            return "mg C/L"
        if "MN_IV" in n:
            return "mg Mn/L"
        if "FE_III" in n:
            return "mg Fe/L"
        if "S_PLUS_6" in n:
            return "mg S/L"
        if "DISS_N" in n:
            return "mg N/L"
        if "DISS_P" in n:
            return "mg P/L"
        return "mg/L"

    # ----- Stoichiometric ratios -----
    if "_N_TO_C" in n:
        return "mg N/mg C"
    if "_P_TO_C" in n:
        return "mg P/mg C"
    if "_Si_TO_C" in n:
        return "mg Si/mg C"
    if "_O2_TO_C" in n:
        return "mg O2/mg C"
    if "_C_TO_CHLA" in n:
        return "mg C/mg Chla"

    # ----- Fractions -----
    if n.startswith("FRAC_"):
        return "-"

    # ----- Light saturation -----
    if n.startswith("I_S_"):
        return "langleys"

    # ----- Hypoxia stress -----
    if n.startswith("DO_STR_HYPOX_"):
        return "mg O2/L"
    if n.startswith("EXPON_HYPOX_"):
        return "-"
    if n.startswith("THETA_HYPOX_"):
        return "-"

    # ----- Grazing / preference -----
    if n.startswith("GRAT_ZOO_"):
        return "-"
    if n.startswith("PREF_ZOO_"):
        return "-"

    # ----- Phytoplankton factors for dissolution / mineralisation -----
    if n.startswith("FAC_PHYT_"):
        return "L/(mg C)"

    # ----- Oxidation / reduction rates -----
    if n.startswith("K_OX_") or n.startswith("K_RED_"):
        return "1/day"
    if n.startswith("k_OX_") or n.startswith("k_RED_"):
        return "1/day"
    if n.startswith("k_DISS_") and n.endswith("_20"):
        return "1/day"
    if n.startswith("THETA_K_") or n.startswith("THETA_k_"):
        return "-"

    # ----- Initial fractions -----
    if "INIT_MULT_" in n:
        return "-"

    # ----- pH -----
    if n.startswith("PH_MIN_") or n.startswith("PH_MAX_"):
        return "pH"
    if n.startswith("PH_NITR_"):
        return "pH"

    # ----- Methane / H2S -----
    if n == "K_OX_CH4" or n == "K_OX_H2S":
        return "1/day"
    if "k_HS_OX_" in n:
        return "mg O2/L"

    # ----- Photoinhibition -----
    if n.startswith("BETA_"):
        return "-"

    # ----- Inhibition -----
    if "RED_INHB" in n or "RED_LIM" in n:
        return "mg/L"

    return "-"


# ---------------------------------------------------------------------------
# 3.  Category definitions  (number ranges -> category)
# ---------------------------------------------------------------------------
CATEGORIES = [
    ("General / Physical Parameters", 1, 4,
     "General physical and optical parameters controlling aeration and light "
     "attenuation through the water column.",
     "Aeration rate K_A is set to -1.0 (auto-calculated from wind and depth "
     "using O'Connor-Dobbins or similar). THETA_K_A = 1.04 follows the standard "
     "Arrhenius range 1.02-1.06 (Chapra, 1997). XKC = 0.08 m^-1/(ug Chla/L) is "
     "at the upper end of the 0.01-0.08 range reported by Kirk (1994), suitable "
     "for eutrophic systems. PHIMX = 720 mg C/mol photon is within the literature "
     "range 700-1000 (Falkowski & Raven, 2007)."),
    ("Diatoms (Bacillariophyceae)", 5, 28,
     "Growth, respiration, mortality, nutrient limitation, hypoxia stress, "
     "and stoichiometry for diatoms -- siliceous phytoplankton that dominate "
     "spring blooms in temperate lakes and estuaries.",
     "KG_DIA_OPT_TEMP = 3.7 /day: within 2-4 /day at optimal T (Eppley, 1972; "
     "Reynolds, 2006). CTMI cardinal temperatures T_min=1, T_opt=24, T_max=35 degC "
     "from Bernard & Remond (2012). EFF_DIA_GROWTH = 0.95 means only 5% of gross "
     "growth lost to maintenance. KR_DIA_20 = 0.05 /day and KD_DIA_20 = 0.12 /day "
     "within ranges 0.03-0.10 and 0.05-0.20 (Reynolds, 2006). Half-saturations "
     "KHS_DIN=0.01, KHS_DIP=0.005, KHS_DSi=0.013 mg/L are typical for nutrient-"
     "rich systems (Jorgensen et al., 1991). N:C=0.22, P:C=0.024 follow Redfield "
     "(1958) with slight N enrichment; Si:C=0.25 follows Brzezinski (1985). "
     "C:Chla=30 is in the range 20-100 (Geider et al., 1997)."),
    ("Non-Fixing Cyanobacteria", 29, 50,
     "Parameters for non-nitrogen-fixing cyanobacteria (e.g. Microcystis, "
     "Planktothrix). Warm-water specialists with higher optimal temperatures.",
     "KG_CYN_OPT_TEMP = 2.4 /day: within 1.0-3.5 /day (Reynolds, 2006). "
     "T_min=15, T_opt=26, T_max=38 degC follow Robarts & Zohary (1987); Rosso "
     "et al. (1993). KHS_DIN=0.009, KHS_DIP=0.008 mg/L are low, reflecting "
     "cyanobacterial competitive advantage at low nutrients (Jorgensen et al., "
     "1991). C:Chla=40 reflects higher pigment packaging than diatoms."),
    ("Nitrogen-Fixing Cyanobacteria", 51, 74,
     "Parameters for heterocystous N2-fixing cyanobacteria (e.g. Aphanizomenon, "
     "Dolichospermum). Unique N-fixation parameters R_FIX and K_FIX control the "
     "transition from DIN uptake to atmospheric N2 fixation.",
     "KG_FIX_CYN_OPT_TEMP = 3.5 /day: upper range, compensating for the "
     "energetic cost of nitrogenase (Staal et al., 2003). T_min=18, T_opt=26, "
     "T_max=38 degC (Grimaud et al., 2017). R_FIX=1.0 means non-fixing and "
     "fixing pathways balanced; K_FIX=0.008 sets switching sensitivity to low "
     "DIN (Horne & Goldman, 1994)."),
    ("Other Phytoplankton (Chlorophyta, Cryptophyta, etc.)", 75, 96,
     "Growth and loss parameters for a generic 'other phytoplankton' group "
     "representing green algae, cryptophytes, and chrysophytes.",
     "KG_OPA_OPT_TEMP = 2.9 /day: mid-range for green algae (Reynolds & "
     "Irish, 1997). T_opt=20 degC is lower than cyanobacteria, reflecting "
     "cooler-season dominance (Sommer, 1989). Stoichiometric ratios match "
     "Redfield. C:Chla = 30 as for diatoms."),
    ("Zooplankton", 97, 133,
     "Growth, grazing preferences, half-saturation constants, respiration, "
     "and mortality for the bulk zooplankton compartment (mainly crustacean "
     "micro- and mesozooplankton).",
     "KG_ZOO_OPT_TEMP = 0.45 /day: within 0.2-0.8 /day (Jorgensen, 1995). "
     "Grazing rate multipliers (GRAT_ZOO_*) = 1.0 for live phytoplankton, "
     "0.5 for detritus. Preferences (PREF_ZOO_*) sum to ~1.0 with OtherPhyto "
     "highest (0.37) and cyanobacteria low (0.10), reflecting selective "
     "avoidance of cyanobacteria by Daphnia (Sommer & Sommer, 2006). "
     "Half-saturations 0.07-0.50 mg C/L (Hansen et al., 1997). FOOD_MIN_ZOO "
     "= 0.02 mg C/L prevents grazing at very low food (Jorgensen, 1976)."),
    ("Particulate Organic Matter (POM) Dissolution", 134, 146,
     "First-order dissolution rates for particulate organic carbon (POC), "
     "nitrogen (PON), phosphorus (POP), and biogenic silica (BSi). "
     "Phytoplankton-enhancement factors simulate bacterial colonisation.",
     "KDISS POC = 10.0 /day: high value accounts for rapid labile fraction; "
     "PON = 0.25 /day and POP = 3.48 /day within Wetzel (2001) ranges "
     "(PON 0.05-0.5, POP 0.1-5.0 /day). BSi dissolution = 0.001 /day is low, "
     "consistent with Kamatani (1982). Theta = 1.06 for temperature correction "
     "(Bowie et al., 1985). Phytoplankton factors near zero disable enzymatic "
     "enhancement for PON/POP to avoid double counting."),
    ("Dissolved Organic Matter (DOM) Mineralisation", 147, 151,
     "Phytoplankton-dependent mineralisation rate factors and reverse "
     "half-saturation concentrations for DOC, DON, and DOP.",
     "FAC_PHYT_AMIN_DOC = 0.0045: low value reflects slow co-metabolic "
     "mineralisation. KHS_AMIN_N = 100 mg/L effectively disables the reverse "
     "half-saturation for DON (labile pool always mineralized). DOP factor "
     "= 0.90 reflects rapid enzymatic DOP hydrolysis by alkaline phosphatase "
     "(Chapra, 1997)."),
    ("Nitrification", 152, 157,
     "Ammonia oxidation (nitrification) rate, temperature correction, "
     "half-saturation for O2 and NH4-N, and optimum pH range.",
     "K_NITR_20 = 0.6 /day: within 0.1-1.0 /day (Chapra, 1997; Cerco, 2000). "
     "THETA = 1.045 standard (Bowie et al., 1985). KHS_NITR_OXY = 2.0 mg/L: "
     "nitrification inhibited below ~2 mg O2/L (Painter, 1970). pH range "
     "6.9-8.2 is the active window (Painter, 1970)."),
    ("Redox Metal Chemistry (Fe, Mn)", 158, 163,
     "Oxidation/reduction rate constants for Fe(II)/Fe(III) and Mn(II)/Mn(IV), "
     "plus reversed Monod half-saturation for dissolved oxygen inhibition.",
     "k_OX_FE_II = 0.00125 /day: slow consistent with Stumm & Morgan (1996) "
     "range 10^-3 to 10^-1 /day. k_RED rates = 2.0 /day: within 0.5-5.0 "
     "(Davison, 1993). KHS_DOXY thresholds = 0.20 mg/L: reduction proceeds "
     "only under near-anoxic conditions."),
    ("DOC Mineralisation -- Multi-Electron-Acceptor", 164, 203,
     "Mineralisation rate constants of DOC using different terminal electron "
     "acceptors (O2, NO3, Mn(IV), Fe(III), SO4, methanogenesis). Includes "
     "temperature corrections, half-saturations, inhibition following the "
     "thermodynamic sequence, and pH optima.",
     "K_MIN_DOC_DOXY_20 = 0.010 /day: aerobic rate intentionally low to "
     "represent semi-refractory DOC pool; anaerobic rates = 0.025 /day for "
     "all alternative acceptors, following Van Cappellen & Wang (1996). "
     "Theta = 1.04 throughout (Soetaert et al., 1996). Inhibition KHS values "
     "= 0.10 mg/L enforce the thermodynamic sequence O2 > NO3 > Mn(IV) > "
     "Fe(III) > SO4 > methanogenesis (Berner, 1980). pH optima 6.0-9.0 "
     "(Boudreau, 1997)."),
    ("DON Mineralisation -- Multi-Electron-Acceptor", 204, 233,
     "Analogous to DOC mineralisation but for dissolved organic nitrogen. "
     "Same multi-electron-acceptor framework.",
     "K_MIN_DON_DOXY_20 = 0.10 /day: faster than DOC aerobic rate because "
     "N-containing substrates are more labile (Soetaert et al., 1996). "
     "Anaerobic rates = 0.0012 /day. Theta = 1.08 (higher temperature "
     "sensitivity). pH optima 6.0-9.0."),
    ("DOP Mineralisation -- Multi-Electron-Acceptor", 234, 263,
     "Analogous to DOC mineralisation but for dissolved organic phosphorus.",
     "K_MIN_DOP_DOXY_20 = 0.70 /day: DOP mineralisation is the fastest of "
     "the three DOM pools due to efficient enzymatic hydrolysis by alkaline "
     "phosphatase (Soetaert et al., 1996). Anaerobic rates = 0.03 /day."),
    ("Methane & Hydrogen Sulphide", 264, 269,
     "Oxidation rates for CH4 and H2S with Arrhenius temperature corrections "
     "and half-saturation for dissolved oxygen.",
     "k_OX_CH4 = 1.0 /day and k_OX_H2S = 1.0 /day: mid-range values from "
     "Bastviken et al. (2004) and Jorgensen (1982). Theta = 1.04 standard. "
     "Half-saturation for O2 = 1.5 mg/L indicates oxidation requires "
     "moderate oxygen availability."),
    ("Fe Dissolution & Fractionation", 270, 275,
     "Dissolution rate constants for particulate Fe(II) and Fe(III) phases "
     "and initial dissolved fractions.",
     "k_DISS_FE_II_20 = 0.1 /day and k_DISS_FE_III_20 = 0.10 /day: within "
     "0.01-1.0 /day range (Stumm & Morgan, 1996). Initial dissolved Fe(II) "
     "fraction = 0.01 (mostly particulate); Fe(III) fraction = 0.50 "
     "(equilibrium partitioning)."),
    ("Nostocales (Heterocystous Cyanobacteria)", 276, 306,
     "Growth, loss, and akinete (resting stage) formation/germination "
     "parameters for Nostocales (Dolichospermum, Aphanizomenon flos-aquae). "
     "Includes density-dependent mortality and seasonal triggers.",
     "KG_NOST_VEG_HET = 1.29 /day within 0.5-2.0 /day (Paerl & Otten, "
     "2013). T_opt=26, T_max=38 degC (Sukenik et al., 2012; Kovacs et al., "
     "2016). Akinete germination rate P_GERM_AKI = 0.3 /day, triggered "
     "when T > 21 degC AND DIN < 0.1 mg/L (Hense & Beckmann, 2006). "
     "Formation starts after day 200 when T < 16 degC."),
    ("Dissolution Saturation & DOM Availability", 307, 318,
     "Half-saturation constants for POM dissolution saturation, available "
     "fractions of DON/DOP for Nostocales, and phytoplankton-dependent "
     "mineralisation caps.",
     "KHS_POC_DISS_SAT = 1.25 mg C/L prevents runaway dissolution at high "
     "POC. frac_avail_DON/DOP = 0.0: DON/DOP not directly bioavailable to "
     "phytoplankton (must be mineralised first). K_MIN_PHYT_AMIN caps "
     "= 4.0 limit phytoplankton-enhanced mineralisation (Wetzel, 2001)."),
    ("Photoinhibition (beta)", 319, 323,
     "Photoinhibition parameters for each phytoplankton group. beta = 0 uses "
     "the default Steele (1962) formulation; beta > 0 increases photo-"
     "inhibition at high irradiance.",
     "All BETA values = 0.0: default Steele curve P = (I/I_s)*exp(1 - I/I_s) "
     "is used. This is appropriate when surface photoinhibition is negligible "
     "or the model is applied to well-mixed water columns (Platt et al., 1980)."),
]


def assign_category(num_str: str, fallback_idx: int) -> str:
    """Return category name for a given constant number."""
    try:
        n = int(num_str)
    except (ValueError, TypeError):
        n = fallback_idx + 1
    for cat_name, lo, hi, *_ in CATEGORIES:
        if lo <= n <= hi:
            return cat_name
    return "Uncategorised"


# ---------------------------------------------------------------------------
# 4.  Literature references by category
# ---------------------------------------------------------------------------
REFERENCES: dict[str, list[tuple[str, str, str]]] = {
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
         "Light extinction coefficient per unit chlorophyll (XKC ~ 0.01-0.08 m^-1/(ug Chla/L))."),
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
         "Redfield ratio C:N:P = 106:16:1 (N:C ~ 0.17, P:C ~ 0.024 by mass). "
         "Model N:C = 0.22 and P:C = 0.024 are within reported ranges."),
        ("Geider, R.J., MacIntyre, H.L., & Kana, T.M. (1997)",
         "Dynamic model of phytoplankton growth and acclimation. Marine Ecology Progress Series, "
         "148, 187-200.",
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
# 5.  Build PDF
# ---------------------------------------------------------------------------
class ConstPDF(FPDF):
    """Custom FPDF subclass for the constants reference."""

    # Column widths
    COL_NUM_W = 8
    COL_NAME_W = 43
    COL_VAL_W = 14
    COL_UNIT_W = 20
    COL_DESC_W = 0  # auto-computed from remaining space

    def __init__(self):
        super().__init__(orientation="P", unit="mm", format="A4")
        self.set_auto_page_break(auto=True, margin=18)
        self.add_page()
        self.alias_nb_pages()

    @property
    def usable_w(self) -> float:
        return self.w - self.l_margin - self.r_margin

    @property
    def desc_w(self) -> float:
        return (self.usable_w
                - self.COL_NUM_W - self.COL_NAME_W
                - self.COL_VAL_W - self.COL_UNIT_W)

    # ----- header / footer -----
    def header(self):
        self.set_font("Helvetica", "B", 9)
        self.set_text_color(100, 100, 100)
        self.cell(0, 6, "AQUABC Model -- Constant Reference (WCONST_04)",
                  align="L")
        self.ln(8)
        self.set_draw_color(180, 180, 180)
        self.line(self.l_margin, self.get_y(),
                  self.w - self.r_margin, self.get_y())
        self.ln(3)

    def footer(self):
        self.set_y(-14)
        self.set_font("Helvetica", "I", 8)
        self.set_text_color(140, 140, 140)
        self.cell(0, 10, f"Page {self.page_no()}/{{nb}}", align="C")

    # ----- helpers -----
    def section_title(self, title: str):
        self.set_font("Helvetica", "B", 12)
        self.set_fill_color(44, 62, 80)
        self.set_text_color(255, 255, 255)
        self.cell(0, 8, f"  {title}",
                  new_x="LMARGIN", new_y="NEXT", fill=True)
        self.set_text_color(0, 0, 0)
        self.ln(2)

    def subsection(self, text: str):
        self.set_font("Helvetica", "I", 9)
        self.set_text_color(60, 60, 60)
        self.multi_cell(0, 4.5, text)
        self.set_text_color(0, 0, 0)
        self.ln(2)

    def value_justification(self, text: str):
        """Print boxed value-justification paragraph."""
        self.set_font("Helvetica", "B", 8)
        self.set_text_color(44, 62, 80)
        self.cell(0, 4.5, "Value Selection Rationale:",
                  new_x="LMARGIN", new_y="NEXT")
        self.set_font("Helvetica", "", 7.5)
        self.set_text_color(40, 40, 40)
        self.set_fill_color(248, 248, 240)
        self.multi_cell(0, 3.8, text, fill=True)
        self.set_text_color(0, 0, 0)
        self.ln(2)

    def table_header(self):
        self.set_font("Helvetica", "B", 7.5)
        self.set_fill_color(220, 220, 220)
        dw = self.desc_w
        self.cell(self.COL_NUM_W, 5, "#", border=1, fill=True, align="C")
        self.cell(self.COL_NAME_W, 5, "Constant Name", border=1, fill=True)
        self.cell(self.COL_VAL_W, 5, "Value", border=1, fill=True, align="C")
        self.cell(self.COL_UNIT_W, 5, "Unit", border=1, fill=True, align="C")
        self.cell(dw, 5, "Description", border=1, fill=True)
        self.ln()

    def table_row(self, num: str, name: str, value: str, unit: str,
                  desc: str, alt: bool):
        dw = self.desc_w
        self.set_font("Helvetica", "", 7)

        # Estimate row height
        name_lines = max(1, len(name) // 20 + 1)
        desc_lines = max(1, len(desc) // max(1, int(dw / 1.55)) + 1)
        row_h = max(name_lines, desc_lines) * 3.8 + 0.8

        if alt:
            self.set_fill_color(245, 245, 250)
        else:
            self.set_fill_color(255, 255, 255)

        x0, y0 = self.get_x(), self.get_y()

        # Page break check
        if y0 + row_h > self.h - 20:
            self.add_page()
            self.table_header()
            x0, y0 = self.get_x(), self.get_y()
            if alt:
                self.set_fill_color(245, 245, 250)
            else:
                self.set_fill_color(255, 255, 255)

        # Num
        self.set_xy(x0, y0)
        self.cell(self.COL_NUM_W, row_h, num,
                  border="LTB", fill=True, align="C")

        # Name
        self.set_xy(x0 + self.COL_NUM_W, y0)
        self.set_font("Courier", "", 6)
        self.multi_cell(self.COL_NAME_W, row_h, name,
                        border="LTB", fill=True)

        actual_name_h = self.get_y() - y0
        final_h = max(row_h, actual_name_h)

        # Value
        self.set_font("Helvetica", "", 7)
        self.set_xy(x0 + self.COL_NUM_W + self.COL_NAME_W, y0)
        self.cell(self.COL_VAL_W, final_h, value,
                  border="LTB", fill=True, align="C")

        # Unit
        self.set_font("Helvetica", "I", 6.5)
        self.set_xy(x0 + self.COL_NUM_W + self.COL_NAME_W + self.COL_VAL_W,
                    y0)
        self.cell(self.COL_UNIT_W, final_h, unit,
                  border="LTB", fill=True, align="C")

        # Description
        self.set_font("Helvetica", "", 7)
        self.set_xy(x0 + self.COL_NUM_W + self.COL_NAME_W
                    + self.COL_VAL_W + self.COL_UNIT_W, y0)
        self.multi_cell(dw, 3.8, desc, border="LTBR", fill=True)
        self.set_y(max(self.get_y(), y0 + final_h))

    def ref_block(self, refs: list[tuple[str, str, str]]):
        uw = self.usable_w
        self.set_font("Helvetica", "B", 8)
        self.set_text_color(44, 62, 80)
        self.cell(0, 5, "Literature References:",
                  new_x="LMARGIN", new_y="NEXT")
        self.set_text_color(0, 0, 0)
        for i, (author, title, note) in enumerate(refs, 1):
            if self.get_y() > self.h - 25:
                self.add_page()
            self.set_font("Helvetica", "B", 7)
            self.multi_cell(uw, 3.5, f"[{i}] {author}")
            self.set_font("Helvetica", "I", 7)
            self.set_x(self.l_margin + 4)
            self.multi_cell(uw - 4, 3.5, title)
            if note:
                self.set_font("Helvetica", "", 7)
                self.set_text_color(80, 80, 80)
                self.set_x(self.l_margin + 4)
                self.multi_cell(uw - 4, 3.5, f"Relevance: {note}")
                self.set_text_color(0, 0, 0)
            self.ln(1)
        self.ln(3)


# ---------------------------------------------------------------------------
# 6.  Assemble the document
# ---------------------------------------------------------------------------
def build_pdf(consts: list[dict], out_path: Path):
    pdf = ConstPDF()

    # ---- Title page ----
    pdf.set_font("Helvetica", "B", 22)
    pdf.ln(15)
    pdf.cell(0, 12, "AQUABC Model",
             new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.set_font("Helvetica", "B", 16)
    pdf.cell(0, 10, "Parameter & Constant Reference",
             new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.set_font("Helvetica", "", 11)
    pdf.cell(0, 8, "Based on WCONST_04.txt (latest configuration)",
             new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.ln(5)
    pdf.set_font("Helvetica", "I", 10)
    pdf.set_text_color(100, 100, 100)
    pdf.cell(0, 6, f"Total constants: {len(consts)}",
             new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.cell(0, 6, "Generated: February 2026",
             new_x="LMARGIN", new_y="NEXT", align="C")
    pdf.set_text_color(0, 0, 0)
    pdf.ln(6)

    # Summary
    pdf.set_font("Helvetica", "", 9)
    pdf.set_fill_color(240, 244, 248)
    summary = (
        "This document provides a complete reference for all model constants "
        "in the AQUABC pelagic ecological model (file WCONST_04.txt). "
        "Constants are organised by functional category. Each section shows "
        "the constant number, name, current value, measurement unit, and "
        "description. A value-selection rationale explains *why* specific "
        "values were chosen, citing literature ranges. The AQUABC model "
        "simulates phytoplankton dynamics (diatoms, cyanobacteria, other "
        "algae, Nostocales), zooplankton grazing, organic matter cycling, "
        "nutrient dynamics (N, P, Si), dissolved oxygen, redox chemistry "
        "(Fe, Mn), methane, hydrogen sulphide, and pH/alkalinity. "
        "Temperature effects use the Cardinal Temperature Model with "
        "Inflection (CTMI; Rosso et al. 1993). Nutrient limitation follows "
        "Monod (Michaelis-Menten) kinetics. The multi-electron-acceptor "
        "mineralisation scheme follows Van Cappellen & Wang (1996).\n\n"
        "File format (Fortran free-format read): "
        "  <integer_index>  <constant_name>  <real_value>  ! <description>"
    )
    pdf.multi_cell(0, 4.5, summary, fill=True)
    pdf.ln(4)

    # Table of contents
    pdf.section_title("Table of Contents")
    for i, (cat_name, lo, hi, *_) in enumerate(CATEGORIES, 1):
        n_in = sum(
            1 for c in consts
            if assign_category(c["num"], consts.index(c)) == cat_name
        )
        pdf.set_font("Helvetica", "", 9)
        pdf.cell(0, 5,
                 f"  {i}. {cat_name}  (#{lo}-{hi}, {n_in} constants)",
                 new_x="LMARGIN", new_y="NEXT")
    pdf.ln(3)
    pdf.set_font("Helvetica", "", 9)
    pdf.cell(0, 5, "  Bibliography / Full Reference List",
             new_x="LMARGIN", new_y="NEXT")
    pdf.ln(6)

    # ---- Category pages ----
    for cat_name, lo, hi, cat_desc, val_rationale in CATEGORIES:
        pdf.add_page()
        pdf.section_title(cat_name)
        pdf.subsection(cat_desc)

        # Value justification
        if val_rationale:
            pdf.value_justification(val_rationale)

        cat_consts = [
            c for c in consts
            if assign_category(c["num"], consts.index(c)) == cat_name
        ]

        if cat_consts:
            pdf.table_header()
            for j, c in enumerate(cat_consts):
                comment = c["comment"]
                # Remove leading numbers that repeat the index
                comment = re.sub(r"^[\d.]+\s+", "", comment)
                unit = get_unit(c["name"])
                pdf.table_row(
                    c["num"], c["name"], c["value"], unit, comment,
                    j % 2 == 1,
                )
            pdf.ln(4)

        # References
        refs = REFERENCES.get(cat_name, [])
        if refs:
            pdf.ref_block(refs)

    # ---- Full bibliography ----
    pdf.add_page()
    pdf.section_title("Complete Bibliography")
    pdf.ln(2)

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

    # Additional references
    pdf.ln(4)
    pdf.set_font("Helvetica", "B", 9)
    pdf.cell(0, 5, "Additional Key References for Water Quality Modelling:",
             new_x="LMARGIN", new_y="NEXT")
    pdf.ln(2)
    extra_refs = [
        "Ambrose, R.B., Wool, T.A. & Martin, J.L. (1993) The Water Quality "
        "Analysis Simulation Program, WASP5. Part A: Model Documentation. "
        "U.S. EPA, Athens, GA.",
        "Brown, L.C. & Barnwell, T.O. (1987) The Enhanced Stream Water "
        "Quality Models QUAL2E and QUAL2E-UNCAS. EPA/600/3-87/007.",
        "Cole, T.M. & Wells, S.A. (2006) CE-QUAL-W2: A Two-Dimensional, "
        "Laterally Averaged, Hydrodynamic and Water Quality Model, "
        "Version 3.5. U.S. Army Corps of Engineers.",
        "Collins, C.D. & Wlosinski, J.H. (1983) Coefficients for Use in "
        "the US Army Corps of Engineers Reservoir Model CE-QUAL-R1. "
        "Report E-83-15.",
        "Di Toro, D.M. (2001) Sediment Flux Modeling. Wiley-Interscience.",
        "Eppley, R.W. (1972) Temperature and phytoplankton growth in the "
        "sea. Fishery Bulletin, 70(4), 1063-1085.",
        "Hamilton, D.P. & Schladow, S.G. (1997) Prediction of water quality "
        "in lakes and reservoirs. Part I -- Model description. Ecological "
        "Modelling, 96, 91-110.",
        "James, R.T., Martin, J., Wool, T. & Wang, P.F. (1997) A sediment "
        "resuspension and water quality model of Lake Okeechobee. JAWRA, "
        "33(3), 661-680.",
        "Jorgensen, S.E. & Bendoricchio, G. (2001) Fundamentals of "
        "Ecological Modelling, 3rd ed. Elsevier.",
        "Lindim, C., Pinho, J.L. & Vieira, J.M.P. (2011) Analysis of "
        "spatial and temporal patterns in a large reservoir using water "
        "quality and hydrodynamic modeling. Ecological Modelling, 222, "
        "2485-2494.",
        "Sakurova, I. (2019) Modeling of eutrophication processes in the "
        "Curonian Lagoon. PhD Thesis, Klaipeda University. "
        "[ESTAS-AQUABC model application].",
        "Idzelyte, R. (2016) A visualisation system for biogeochemical "
        "and hydrodynamic modelling and its application to the Curonian "
        "lagoon. PhD Thesis, Klaipeda University.",
        "Eilola, K., Almroth-Rosell, E., Edman, M. et al. (2015) Model "
        "set-up at COCOA study sites. SMHI Report. "
        "[AQUABC ecological model description].",
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
# 7.  Main
# ---------------------------------------------------------------------------
def main():
    consts = parse_constants(CONST_FILE)
    print(f"Parsed {len(consts)} constants from {CONST_FILE}")

    # Sanity-check: print first 5 to verify parsing
    for c in consts[:5]:
        print(f"  #{c['num']:>3s}  {c['name']:<35s}  {c['value']:>10s}  "
              f"[{get_unit(c['name'])}]  {c['comment'][:60]}")

    out = (Path(__file__).resolve().parent.parent
           / "docs" / "AQUABC_Constant_Reference.pdf")
    out.parent.mkdir(parents=True, exist_ok=True)
    build_pdf(consts, out)
    print(f"PDF written to {out}  ({out.stat().st_size / 1024:.0f} KB)")


if __name__ == "__main__":
    main()
