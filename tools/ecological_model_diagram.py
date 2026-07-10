#!/usr/bin/env python3
"""
AQUABC v0.3 Ecological Model – Interactive Network Diagram
Generates an interactive HTML diagram using pyvis.

Usage (from micromamba shiny environment):
    micromamba activate shiny
    python tools/ecological_model_diagram.py

Output: docs/aquabc_ecological_model.html
"""

from pyvis.network import Network
import os

def create_ecological_model_diagram():
    # ──────────────────────────────────────────────────────────
    # Network setup
    # ──────────────────────────────────────────────────────────
    net = Network(
        height="1100px",
        width="100%",
        directed=True,
        bgcolor="#F5F7FA",
        font_color="#2D3436",
        heading="",
    )

    # Physics for interactive layout
    net.set_options("""
    {
      "physics": {
        "barnesHut": {
          "gravitationalConstant": -12000,
          "centralGravity": 0.8,
          "springLength": 180,
          "springConstant": 0.02,
          "damping": 0.4,
          "avoidOverlap": 0.6
        },
        "maxVelocity": 30,
        "minVelocity": 0.75,
        "stabilization": {
          "enabled": true,
          "iterations": 500,
          "updateInterval": 25
        }
      },
      "edges": {
        "smooth": {
          "type": "curvedCW",
          "roundness": 0.15
        },
        "font": {
          "size": 10,
          "color": "#555555",
          "strokeWidth": 2,
          "strokeColor": "#FFFFFF"
        },
        "arrows": {
          "to": {
            "enabled": true,
            "scaleFactor": 0.7
          }
        }
      },
      "nodes": {
        "font": {
          "size": 13,
          "face": "Helvetica"
        },
        "borderWidth": 2,
        "shadow": true
      },
      "interaction": {
        "hover": true,
        "tooltipDelay": 200,
        "navigationButtons": true,
        "keyboard": true
      }
    }
    """)

    # ──────────────────────────────────────────────────────────
    # Style definitions
    # ──────────────────────────────────────────────────────────
    # ── Harmonised colour palette ─────────────────────────
    # Design principles:
    #   - Aquatic blues for water-chemistry nodes (nutrients, O₂)
    #   - Greens for primary producers, warm coral for consumers
    #   - Distinct earth tones for POM vs DOM (not both brown)
    #   - Purple–violet reserved exclusively for redox
    #   - Amber/gold for forcing; slate for sediment; teal for macroalgae
    #   - Rose/magenta for allelopathy (distinct from consumer coral)
    #   - WCAG AA contrast: white text on dark fills, dark text on light fills
    STYLES = {
        "forcing":   {"color": {"background": "#F59E0B", "border": "#D97706"},  "shape": "diamond",  "size": 22, "font": {"color": "#1F2937", "size": 12}},
        "phyto":     {"color": {"background": "#34D399", "border": "#059669"},  "shape": "dot",      "size": 28, "font": {"color": "#064E3B", "size": 13}},
        "zoo":       {"color": {"background": "#F87171", "border": "#DC2626"},  "shape": "dot",      "size": 30, "font": {"color": "#FFFFFF", "size": 13}},
        "dom":       {"color": {"background": "#A78BFA", "border": "#7C3AED"},  "shape": "dot",      "size": 24, "font": {"color": "#FFFFFF", "size": 12}},
        "pom":       {"color": {"background": "#D97706", "border": "#92400E"},  "shape": "dot",      "size": 24, "font": {"color": "#FFFFFF", "size": 12}},
        "nutrient":  {"color": {"background": "#3B82F6", "border": "#1D4ED8"},  "shape": "dot",      "size": 22, "font": {"color": "#FFFFFF", "size": 12}},
        "oxygen":    {"color": {"background": "#38BDF8", "border": "#0284C7"},  "shape": "star",     "size": 30, "font": {"color": "#0C4A6E", "size": 13}},
        "redox":     {"color": {"background": "#8B5CF6", "border": "#6D28D9"},  "shape": "dot",      "size": 20, "font": {"color": "#FFFFFF", "size": 11}},
        "carbon":    {"color": {"background": "#FCD34D", "border": "#F59E0B"},  "shape": "dot",      "size": 22, "font": {"color": "#78350F", "size": 12}},
        "sediment":  {"color": {"background": "#64748B", "border": "#334155"},  "shape": "box",      "size": 25, "font": {"color": "#FFFFFF", "size": 12}},
        "macro":     {"color": {"background": "#14B8A6", "border": "#0D9488"},  "shape": "dot",      "size": 25, "font": {"color": "#FFFFFF", "size": 12}},
        "allelo":    {"color": {"background": "#FB7185", "border": "#E11D48"},  "shape": "triangle", "size": 18, "font": {"color": "#881337", "size": 11}},
        "co2sys":    {"color": {"background": "#E2E8F0", "border": "#94A3B8"},  "shape": "box",      "size": 25, "font": {"color": "#1E293B", "size": 12}},
    }

    def add(node_id, label, group, title="", level=None):
        s = STYLES[group]
        kwargs = {
            "label": label,
            "title": title or label,
            "group": group,
            "color": s["color"],
            "shape": s["shape"],
            "size":  s["size"],
            "font":  s["font"],
        }
        if level is not None:
            kwargs["level"] = level
        net.add_node(node_id, **kwargs)

    def edge(src, dst, label="", color="#888888", width=1.5, dashes=False):
        net.add_edge(src, dst, label=label, color=color, width=width, dashes=dashes)

    # ──────────────────────────────────────────────────────────
    # EXTERNAL FORCING (top level)
    # ──────────────────────────────────────────────────────────
    add("TEMP",   "Temperature",  "forcing", "Water temperature – CTMI for phyto, Arrhenius θ^(T-20) for other processes")
    add("PAR",    "Light (PAR)",  "forcing", "Photosynthetically active radiation (langleys/day)")
    add("WIND",   "Wind Speed",   "forcing", "Wind speed (m/s) – drives aeration K_A")
    add("ICE",    "Ice Cover",    "forcing", "Ice cover fraction (0-1) – suppresses aeration")
    add("SALT",   "Salinity",     "forcing", "Salinity (PSU) – affects DO saturation & CO2SYS")
    add("DEPTH",  "Depth",        "forcing", "Water column depth (m) – light attenuation, settling")

    # ──────────────────────────────────────────────────────────
    # PHYTOPLANKTON (5 functional groups)
    # ──────────────────────────────────────────────────────────
    add("DIA",  "Diatoms\n(DIA_C)",            "phyto",
        "Si-limited · Synthesizing Unit colimitation\nKG=3.7/d · N:C=0.22 · Si:C=0.25\nNo DON, no buoyancy")
    add("CYN",  "Non-fixing\nCyanobacteria\n(CYN_C)", "phyto",
        "DON utilisation · Optional buoyancy model\nKG=2.4/d · No Si requirement")
    add("FIX",  "N₂-fixing\nCyanobacteria\n(FIX_CYN_C)", "phyto",
        "Two metabolic fractions (fixing + non-fixing)\nInverse Monod N-fixation · Buoyancy model\nKG=3.5/d")
    add("OPA",  "Other Plankton\nAlgae (OPA_C)", "phyto",
        "Flagellates, cryptophytes, chrysophytes\nKG=2.9/d · Cooler optimum (9-20°C)\nNo DON, no Si, no buoyancy")
    add("NOST", "Nostocales\n(NOST_C + AKI_C)", "phyto",
        "Obligate N₂ fixer · Heterocyst differentiation\nAkinete lifecycle (germination/formation)\nDensity-dependent mortality · Buoyancy")

    # ──────────────────────────────────────────────────────────
    # ZOOPLANKTON
    # ──────────────────────────────────────────────────────────
    add("ZOO", "Zooplankton\n(ZOO_C/N/P)", "zoo",
        "Multi-prey Monod grazing · KG=0.45/d\nPreferences: DIA=0.26, OPA=0.37, CYN=0.10\nFIX=0.07, DET=0.20 · Hypoxia response")

    # ──────────────────────────────────────────────────────────
    # ORGANIC MATTER
    # ──────────────────────────────────────────────────────────
    add("POM", "Particulate OM\n(DET_POC, PON, POP)", "pom",
        "Detrital particulate organic matter\nDissolution: POC=0.1/d, PON=0.25/d, POP=0.48/d\nPhytoplankton-enhanced dissolution")
    add("DOM", "Dissolved OM\n(DOC, DON, DOP)", "dom",
        "Dissolved organic matter\nMineralization via 6 electron-acceptor pathways\nAerobic → NO₃ → Mn(IV) → Fe(III) → SO₄ → CH₄")

    # ──────────────────────────────────────────────────────────
    # DISSOLVED NUTRIENTS
    # ──────────────────────────────────────────────────────────
    add("NH4", "NH₄⁺-N",       "nutrient", "Ammonium nitrogen (mg-N/L)\nSources: mineralization, zoo excretion, phyto resp\nSinks: phyto uptake, nitrification")
    add("NO3", "NO₃⁻-N",       "nutrient", "Nitrate nitrogen (mg-N/L)\nSource: nitrification\nSinks: phyto uptake, denitrification")
    add("PO4", "PO₄³⁻-P",     "nutrient", "Orthophosphate (mg-P/L)\nModulated by Fe(III) co-precipitation\nIP_SOLUBLE_FRACTION from FePO₄ equilibrium")
    add("DSi", "Dissolved Si",  "nutrient", "Dissolved silica (mg-Si/L)\nRequired by diatoms only\nSource: biogenic Si dissolution")
    add("PSi", "Biogenic Si",   "nutrient", "Particulate biogenic silica (mg-Si/L)\nFrom diatom mortality · Settles · Dissolves")

    # ──────────────────────────────────────────────────────────
    # DISSOLVED OXYGEN
    # ──────────────────────────────────────────────────────────
    add("DO", "Dissolved O₂\n(DISS_OXYGEN)", "oxygen",
        "Sources: photosynthesis, aeration\nSinks: respiration, nitrification, oxidation of\n Fe²⁺, Mn²⁺, H₂S, CH₄, aerobic DOC mineral.\nSaturation f(T, S, altitude)")

    # ──────────────────────────────────────────────────────────
    # INORGANIC CARBON
    # ──────────────────────────────────────────────────────────
    add("DIC", "DIC\n(INORG_C)", "carbon",
        "Dissolved inorganic carbon (mol-C/L)\nLinks to CO2SYS for pH, pCO₂")
    add("ALK", "Total\nAlkalinity", "carbon",
        "Total alkalinity (meq/L)\nAffected by nitrification, denitrification,\nsulphate reduction, Fe/Mn redox")

    # ──────────────────────────────────────────────────────────
    # REDOX SPECIES
    # ──────────────────────────────────────────────────────────
    add("FE2", "Fe²⁺",    "redox", "Ferrous iron · Oxidised by O₂ (Morgan & Lahav 2007)\nSpeciation: FeCO₃, Fe(OH)₂, FeS, FeS₂ phases")
    add("FE3", "Fe³⁺",    "redox", "Ferric iron · Reduced by DOC under anoxia\nHydrolysis speciation (Stumm & Morgan 1996)\nControls PO₄ solubility via FePO₄")
    add("MN2", "Mn²⁺",    "redox", "Manganous manganese · Oxidised by O₂\nSpeciation: MnCO₃, Mn(OH)₂, MnS phases")
    add("MN4", "Mn⁴⁺",    "redox", "Manganic manganese · Reduced by DOC\nElectron acceptor in redox sequence")
    add("SO4", "SO₄²⁻-S", "redox", "Sulphate sulphur · Reduced to H₂S under anoxia\n5th in thermodynamic redox hierarchy")
    add("H2S", "H₂S-S",   "redox", "Sulphide sulphur · Re-oxidised by O₂\nControls Fe speciation (FeS, FeS₂)")
    add("CH4", "CH₄-C",   "redox", "Methane carbon · Produced by methanogenesis\n(last resort electron acceptor)\nOxidised by O₂: 5.33 mg-O₂/mg-C")

    # ──────────────────────────────────────────────────────────
    # MACROALGAE
    # ──────────────────────────────────────────────────────────
    add("MAC", "Macroalgae\n(C, N, P)", "macro",
        "Droop quota nutrient limitation\nSpace limitation (logistic)\n3 light formulations (Baly/Smith/Steele)")
    add("MACDET", "Attached\nDetritus", "macro",
        "Dead macroalgae tissue (POC, PON, POP)\nDeattaches to water column POM")

    # ──────────────────────────────────────────────────────────
    # ALLELOPATHY (4 metabolite pools)
    # ──────────────────────────────────────────────────────────
    add("SM_DIA",  "Diatom\nMetabolites",       "allelo", "Secondary metabolites from diatom death\nInhibit other phytoplankton (Monod-type)")
    add("SM_CYN",  "Cyanobact.\nMetabolites",   "allelo", "Secondary metabolites from cyanobacteria death")
    add("SM_FIX",  "Fix-Cyan.\nMetabolites",    "allelo", "Secondary metabolites from N₂-fixing cyan. death")
    add("SM_NOST", "Nostocales\nMetabolites",    "allelo", "Secondary metabolites from Nostocales death")

    # ──────────────────────────────────────────────────────────
    # SEDIMENT DIAGENESIS
    # ──────────────────────────────────────────────────────────
    add("SED_OM",    "Sediment OM\n(POC → DOC)",    "sediment",
        "24 state variables per layer\nOxic + anoxic dissolution pathways")
    add("SED_NUT",   "Sediment\nNutrients",          "sediment",
        "Porewater nutrients: NH₄, NO₃, PO₄, Si, DIC, ALK")
    add("SED_REDOX", "Sediment\nRedox Sequence",     "sediment",
        "Same 6-pathway hierarchy as pelagic\nSediment-specific rate constants & pH correction (Gaussian)")
    add("SED_TRANS", "Transport\n(Diff/Adv/Bio/Bur)","sediment",
        "Diffusion (Archie's law tortuosity)\nAdvection · Bioturbation (particle mixing)\nBurial · Erosion/Deposition")

    # ──────────────────────────────────────────────────────────
    # CO2SYS
    # ──────────────────────────────────────────────────────────
    add("CO2SYS", "CO2SYS\nCarbonate Chemistry", "co2sys",
        "CDIAC implementation\npH, pCO₂, HCO₃⁻, CO₃²⁻\nΩ_calcite, Ω_aragonite\n13 K1/K2 constant sets")

    # ══════════════════════════════════════════════════════════
    # EDGES – Biogeochemical interactions
    # ══════════════════════════════════════════════════════════

    # ── Forcing → model components ───────────────────────────
    for target in ["DIA", "CYN", "FIX", "OPA", "NOST", "ZOO", "DOM", "POM"]:
        edge("TEMP", target, color="#F59E0B44", width=1, dashes=True)
    edge("TEMP", "DO",  color="#F59E0B44", width=1, dashes=True)
    edge("PAR",  "DIA", "Light", color="#FBBF24", width=1.5)
    edge("PAR",  "CYN", "Light", color="#FBBF24", width=1.5)
    edge("PAR",  "FIX", "Light", color="#FBBF24", width=1.5)
    edge("PAR",  "OPA", "Light", color="#FBBF24", width=1.5)
    edge("PAR",  "NOST","Light", color="#FBBF24", width=1.5)
    edge("PAR",  "MAC", "Light", color="#FBBF24", width=1.5)
    edge("WIND", "DO",  "Aeration K_A", color="#FBBF24", width=1.5)
    edge("ICE",  "DO",  "Suppress aeration", color="#93C5FD", width=1.5)
    edge("SALT", "CO2SYS", "", color="#FBBF24", width=1)
    edge("DEPTH","DO",  "", color="#F59E0B44", width=1, dashes=True)

    # ── Photosynthesis: nutrients + DIC → phytoplankton ──────
    green = "#059669"
    for phyt in ["DIA", "CYN", "FIX", "OPA", "NOST"]:
        edge("NH4", phyt, "N uptake", color=green, width=1.5)
        edge("NO3", phyt, "N uptake", color=green, width=1.5)
        edge("PO4", phyt, "P uptake", color=green, width=1.5)
        edge("DIC", phyt, "Photosynthesis", color="#6EE7B7", width=1.5)
    edge("DSi", "DIA", "Si uptake\n(diatoms only)", color="#1D4ED8", width=2)

    # ── Phytoplankton → O₂ production ───────────────────────
    for phyt in ["DIA", "CYN", "FIX", "OPA", "NOST"]:
        edge(phyt, "DO", "O₂ production", color="#38BDF8", width=2)

    # ── Grazing: phytoplankton + POM → zooplankton ───────────
    graze_color = "#DC2626"
    edge("DIA",  "ZOO", "Grazing (0.26)", color=graze_color, width=2.5)
    edge("CYN",  "ZOO", "Grazing (0.10)", color=graze_color, width=1.5)
    edge("FIX",  "ZOO", "Grazing (0.07)", color=graze_color, width=1.2)
    edge("OPA",  "ZOO", "Grazing (0.37)", color=graze_color, width=3)
    edge("NOST", "ZOO", "Grazing",        color=graze_color, width=1.5)
    edge("POM",  "ZOO", "Grazing (0.20)", color=graze_color, width=2)

    # ── Mortality → POM ──────────────────────────────────────
    mort_c = "#B45309"
    for phyt in ["DIA", "CYN", "FIX", "OPA", "NOST"]:
        edge(phyt, "POM", "Death", color=mort_c, width=1.5)
    edge("ZOO", "POM", "Death + faecal", color=mort_c, width=2)

    # ── Excretion → DOM ──────────────────────────────────────
    for phyt in ["DIA", "CYN", "FIX", "OPA", "NOST"]:
        edge(phyt, "DOM", "Excretion", color="#C4B5FD", width=1)
    edge("ZOO", "DOM", "Excretion", color="#C4B5FD", width=1.5)

    # ── Respiration → DO consumption ─────────────────────────
    for phyt in ["DIA", "CYN", "FIX", "OPA", "NOST"]:
        edge(phyt, "DO", "Respiration", color="#FCA5A5", width=1)
    edge("ZOO", "DO", "Respiration", color="#FCA5A5", width=1.5)

    # ── OM cycling: POM → DOM → DIC ─────────────────────────
    edge("POM", "DOM", "Dissolution", color="#7C3AED", width=3)
    edge("DOM", "DIC", "Mineralisation\n(6 redox pathways)", color="#6D28D9", width=3)

    # ── DOM mineralisation regenerates nutrients ─────────────
    edge("DOM", "NH4", "N mineralisation", color="#3B82F6", width=2)
    edge("DOM", "PO4", "P mineralisation", color="#3B82F6", width=2)

    # ── Nitrification ────────────────────────────────────────
    edge("NH4", "NO3", "Nitrification\n(consumes O₂)", color="#1D4ED8", width=2.5)

    # ── Denitrification ──────────────────────────────────────
    edge("NO3", "DIC", "Denitrification\n(DOC as e⁻ donor)", color="#7C3AED", width=2)

    # ── Redox sequence ───────────────────────────────────────
    redox_c = "#8B5CF6"
    edge("MN4", "MN2", "Mn(IV) reduction\n(DOC oxidation)", color=redox_c, width=2)
    edge("FE3", "FE2", "Fe(III) reduction\n(DOC oxidation)", color=redox_c, width=2)
    edge("SO4", "H2S", "SO₄²⁻ reduction\n(DOC oxidation)", color=redox_c, width=2)
    edge("DOM", "CH4", "Methanogenesis\n(last resort)", color=redox_c, width=2)

    # ── Re-oxidation (consumes O₂) ───────────────────────────
    reox = "#A78BFA"
    edge("FE2", "FE3", "Oxidation\n(O₂)", color=reox, width=1.5)
    edge("MN2", "MN4", "Oxidation\n(O₂)", color=reox, width=1.5)
    edge("H2S", "SO4", "Oxidation\n(O₂)", color=reox, width=1.5)
    edge("CH4", "DIC", "Oxidation\n(5.33 mg O₂/mg C)", color=reox, width=1.5)

    # ── Fe-P coupling ────────────────────────────────────────
    edge("FE3", "PO4", "FePO₄ precipitation\n(controls P solubility)", color="#E11D48", width=2, dashes=True)

    # ── CO2SYS ↔ carbonate equilibrium ──────────────────────
    edge("DIC", "CO2SYS", "Equilibrium", color="#94A3B8", width=2)
    edge("ALK", "CO2SYS", "Equilibrium", color="#94A3B8", width=2)
    edge("CO2SYS", "DIC", "pH feedback", color="#94A3B8", width=1, dashes=True)

    # ── Si cycling ───────────────────────────────────────────
    edge("DIA", "PSi", "Diatom mortality", color="#1D4ED8", width=1.5)
    edge("PSi", "DSi", "Dissolution", color="#60A5FA", width=1.5)

    # ── Settling: pelagic → sediment ─────────────────────────
    settle_c = "#475569"
    edge("POM",  "SED_OM", "Settling", color=settle_c, width=2.5)
    edge("PSi",  "SED_OM", "Settling", color=settle_c, width=1.5)
    for phyt in ["DIA", "CYN", "OPA"]:
        edge(phyt, "SED_OM", "Settling", color=settle_c, width=1)

    # ── Sediment ↔ water fluxes (24 species) ────────────────
    sed_flux_c = "#64748B"
    edge("SED_NUT", "NH4", "Sediment flux", color=sed_flux_c, width=2)
    edge("SED_NUT", "NO3", "Sediment flux", color=sed_flux_c, width=1.5)
    edge("SED_NUT", "PO4", "Sediment flux", color=sed_flux_c, width=2)
    edge("SED_NUT", "DSi", "Sediment flux", color=sed_flux_c, width=1.5)
    edge("SED_NUT", "DO",  "O₂ demand",    color="#DC2626", width=2.5)
    edge("SED_OM",  "DOM", "DOC release",   color=sed_flux_c, width=1.5)
    edge("SED_OM",  "DIC", "DIC release",   color=sed_flux_c, width=1.5)
    edge("SED_REDOX","FE2","Fe²⁺ flux",     color=sed_flux_c, width=1)
    edge("SED_REDOX","H2S","H₂S flux",      color=sed_flux_c, width=1)
    edge("SED_REDOX","CH4","CH₄ flux",       color=sed_flux_c, width=1)

    # ── Sediment internal ────────────────────────────────────
    edge("SED_OM", "SED_NUT",   "Mineralisation", color="#334155", width=2)
    edge("SED_OM", "SED_REDOX", "Redox coupling",  color="#334155", width=2)
    edge("SED_TRANS", "SED_OM",    "Transport", color="#475569", width=1, dashes=True)
    edge("SED_TRANS", "SED_NUT",   "Transport", color="#475569", width=1, dashes=True)
    edge("SED_TRANS", "SED_REDOX", "Transport", color="#475569", width=1, dashes=True)

    # ── Macroalgae ↔ pelagic ─────────────────────────────────
    mac_c = "#0D9488"
    edge("NH4", "MAC", "N uptake", color=mac_c, width=1.5)
    edge("NO3", "MAC", "N uptake", color=mac_c, width=1.5)
    edge("PO4", "MAC", "P uptake", color=mac_c, width=1.5)
    edge("MAC", "DO",  "O₂ production", color="#38BDF8", width=1.5)
    edge("MAC", "MACDET", "Death", color=mac_c, width=2)
    edge("MACDET", "POM", "Deattachment\nto water column", color=mac_c, width=2)

    # ── Allelopathy ──────────────────────────────────────────
    allelo_c = "#E11D48"
    edge("DIA",  "SM_DIA",  "Death metabolites", color=allelo_c, width=1)
    edge("CYN",  "SM_CYN",  "Death metabolites", color=allelo_c, width=1)
    edge("FIX",  "SM_FIX",  "Death metabolites", color=allelo_c, width=1)
    edge("NOST", "SM_NOST", "Death metabolites", color=allelo_c, width=1)
    # Inhibition (dashed = inhibitory)
    for sm in ["SM_DIA", "SM_CYN", "SM_FIX", "SM_NOST"]:
        for phyt in ["DIA", "CYN", "FIX", "OPA", "NOST"]:
            edge(sm, phyt, "Inhibition", color="#FDA4AF", width=1, dashes=True)

    # ══════════════════════════════════════════════════════════
    # Save
    # ══════════════════════════════════════════════════════════
    out_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "docs")
    os.makedirs(out_dir, exist_ok=True)
    out_path = os.path.join(out_dir, "aquabc_ecological_model.html")

    # Generate HTML manually to avoid pyvis template issues
    html = net.generate_html()

    # ── Post-process: fix duplicate heading, add custom title + legend + nav button styling ──
    # Remove pyvis's empty/duplicate heading container
    html = html.replace(
        '<center>\n<h1></h1>\n</center>',
        ''
    )
    # Remove the second duplicate heading block too
    html = html.replace(
        '        <center>\n          <h1></h1>\n        </center>',
        ''
    )

    # Inject styled header, legend, and navigation button overrides
    custom_head = """
    <style>
      body { margin: 0; font-family: 'Segoe UI', Helvetica, Arial, sans-serif; }
      .diagram-header {
        background: linear-gradient(135deg, #1E293B 0%, #334155 100%);
        color: #F1F5F9;
        padding: 18px 28px 14px;
        text-align: center;
      }
      .diagram-header h1 {
        margin: 0 0 6px;
        font-size: 1.6em;
        font-weight: 600;
        letter-spacing: 0.3px;
      }
      .diagram-header p {
        margin: 0;
        font-size: 0.85em;
        color: #94A3B8;
      }
      .legend {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 10px 18px;
        padding: 10px 20px;
        background: #F8FAFC;
        border-bottom: 1px solid #E2E8F0;
        font-size: 0.78em;
      }
      .legend-item {
        display: flex;
        align-items: center;
        gap: 5px;
      }
      .legend-swatch {
        width: 14px;
        height: 14px;
        border-radius: 3px;
        border: 1.5px solid rgba(0,0,0,0.15);
        flex-shrink: 0;
      }
      /* ── Navigation button overrides ── */
      div.vis-navigation div.vis-button {
        background-color: #334155 !important;
        border: 1.5px solid #475569 !important;
        border-radius: 6px !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.2) !important;
      }
      div.vis-navigation div.vis-button:hover {
        background-color: #475569 !important;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3) !important;
      }
      div.vis-navigation div.vis-button.vis-up,
      div.vis-navigation div.vis-button.vis-down,
      div.vis-navigation div.vis-button.vis-left,
      div.vis-navigation div.vis-button.vis-right,
      div.vis-navigation div.vis-button.vis-zoomIn,
      div.vis-navigation div.vis-button.vis-zoomOut,
      div.vis-navigation div.vis-button.vis-zoomExtends {
        background-color: #334155 !important;
      }
    </style>
    """

    legend_html = """
    <div class="diagram-header">
      <h1>AQUABC v0.3 &mdash; Ecological Model Architecture</h1>
      <p>Interactive biogeochemical network &middot; 32 pelagic + 24 sediment + 6 macroalgae + 4 allelopathy state variables</p>
    </div>
    <div class="legend">
      <div class="legend-item"><div class="legend-swatch" style="background:#F59E0B;"></div>Forcing</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#34D399;"></div>Phytoplankton</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#F87171;"></div>Zooplankton</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#3B82F6;"></div>Nutrients</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#38BDF8;"></div>Dissolved O&#x2082;</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#FCD34D;"></div>Inorganic C</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#D97706;"></div>Particulate OM</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#A78BFA;"></div>Dissolved OM</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#8B5CF6;"></div>Redox Species</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#64748B;"></div>Sediment</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#14B8A6;"></div>Macroalgae</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#FB7185;"></div>Allelopathy</div>
      <div class="legend-item"><div class="legend-swatch" style="background:#E2E8F0;"></div>CO&#x2082;SYS</div>
    </div>
    """

    # Insert custom styles before </head> and legend before the network div
    html = html.replace('</head>', custom_head + '</head>')
    html = html.replace('<div class="card" style="width: 100%">', legend_html + '<div class="card" style="width: 100%; border: none;">')

    with open(out_path, "w", encoding="utf-8") as f:
        f.write(html)

    print(f"Diagram saved to: {out_path}")
    print(f"Open in browser: file://{out_path}")
    return out_path


if __name__ == "__main__":
    create_ecological_model_diagram()
