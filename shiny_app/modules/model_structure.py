"""Model Structure tab as a Shiny module (Phase 2, Task 1).

`model_structure_ui(id)` returns the panel *content* (the app-level
panel_conditional stays in create_ui); `model_structure_server(id, state)`
registers the `model_structure_iframe` render, ported verbatim from app.py.
Self-contained: no leaf-module deps beyond stdlib + shiny; self-computes
ROOT to locate shiny_app/www (where the diagram HTML is written/served),
since __file__ now lives one directory deeper than app.py.
"""
import logging
import os

from shiny import module, render, ui

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
WWW_DIR = os.path.join(ROOT, "shiny_app", "www")


@module.ui
def model_structure_ui():
    return ui.card(
        ui.card_header(
            ui.tags.i(class_="bi bi-diagram-3 me-2"),
            "Model Structure Diagram"
        ),
        ui.tags.p(
            "Interactive network diagram of the AQUABC ecological model showing "
            "all state variables, sub-models, and biogeochemical interactions. "
            "Drag nodes to rearrange. Scroll to zoom. Hover for details.",
            class_="text-muted mb-2"
        ),
        ui.output_ui("model_structure_iframe"),
    )


@module.server
def model_structure_server(input, output, session, state):
    # `state` is accepted for the uniform x_server(id, state) convention; the
    # model_structure tab is self-contained and uses nothing from it.

    @render.ui
    def model_structure_iframe():
        """Generate the ecological model diagram as a static HTML file and serve via iframe"""
        try:
            import json as _json
            diagram_dir = WWW_DIR
            os.makedirs(diagram_dir, exist_ok=True)
            diagram_path = os.path.join(diagram_dir, "model_diagram.html")

            # Only regenerate if file doesn't exist (delete www/model_diagram.html to force refresh)
            if not os.path.exists(diagram_path):
                logger.info("Generating model structure diagram HTML...")

                # ── Node definitions ──
                nodes_data = []
                def _node(nid, label, grp, title="", shape="dot", size=22, bg="#888", border="#555", font_color="#FFF", font_size=12):
                    nodes_data.append({
                        "id": nid, "label": label, "title": title or label, "group": grp,
                        "shape": shape, "size": size,
                        "color": {"background": bg, "border": border},
                        "font": {"color": font_color, "size": font_size}
                    })

                edges_data = []
                def _edge(src, dst, label="", color="#888888", width=1.5, dashes=False):
                    edges_data.append({"from": src, "to": dst, "label": label, "color": color, "width": width, "dashes": dashes})

                # Forcing
                for nid, lbl in [("TEMP","Temperature"),("PAR","Light (PAR)"),("WIND","Wind Speed"),("ICE","Ice Cover"),("SALT","Salinity"),("DEPTH","Depth")]:
                    _node(nid, lbl, "forcing", shape="diamond", size=22, bg="#F59E0B", border="#D97706", font_color="#1F2937", font_size=12)
                # Phytoplankton
                for nid, lbl, ttl in [("DIA","Diatoms\\n(DIA_C)","Si-limited diatoms"),("CYN","Non-fixing\\nCyanobacteria",""),("FIX","N2-fixing\\nCyanobacteria",""),("OPA","Other Plankton\\nAlgae",""),("NOST","Nostocales\\n(NOST_C)","Obligate N2 fixer")]:
                    _node(nid, lbl, "phyto", title=ttl, shape="dot", size=28, bg="#34D399", border="#059669", font_color="#064E3B", font_size=13)
                # Zooplankton
                _node("ZOO","Zooplankton\\n(ZOO_C/N/P)","zoo","Multi-prey Monod grazing","dot",30,"#F87171","#DC2626","#FFFFFF",13)
                # Organic matter
                _node("POM","Particulate OM\\n(POC,PON,POP)","pom",shape="dot",size=24,bg="#D97706",border="#92400E",font_color="#FFFFFF",font_size=12)
                _node("DOM","Dissolved OM\\n(DOC,DON,DOP)","dom",shape="dot",size=24,bg="#A78BFA",border="#7C3AED",font_color="#FFFFFF",font_size=12)
                # Nutrients
                for nid, lbl in [("NH4","NH4-N"),("NO3","NO3-N"),("PO4","PO4-P"),("DSi","Dissolved Si"),("PSi","Biogenic Si")]:
                    _node(nid, lbl, "nutrient", shape="dot", size=22, bg="#3B82F6", border="#1D4ED8", font_color="#FFFFFF", font_size=12)
                # Oxygen
                _node("DO","Dissolved O2","oxygen","Sources: photosynthesis, aeration","star",30,"#38BDF8","#0284C7","#0C4A6E",13)
                # Carbon
                _node("DIC","DIC","carbon",shape="dot",size=22,bg="#FCD34D",border="#F59E0B",font_color="#78350F",font_size=12)
                _node("ALK","Total\\nAlkalinity","carbon",shape="dot",size=22,bg="#FCD34D",border="#F59E0B",font_color="#78350F",font_size=12)
                # Redox
                for nid, lbl in [("FE2","Fe2+"),("FE3","Fe3+"),("MN2","Mn2+"),("MN4","Mn4+"),("SO4","SO4-S"),("H2S","H2S-S"),("CH4","CH4-C")]:
                    _node(nid, lbl, "redox", shape="dot", size=20, bg="#8B5CF6", border="#6D28D9", font_color="#FFFFFF", font_size=11)
                # Macroalgae
                _node("MAC","Macroalgae\\n(C,N,P)","macro",shape="dot",size=25,bg="#14B8A6",border="#0D9488",font_color="#FFFFFF",font_size=12)
                _node("MACDET","Attached\\nDetritus","macro",shape="dot",size=25,bg="#14B8A6",border="#0D9488",font_color="#FFFFFF",font_size=12)
                # Allelopathy
                for nid, lbl in [("SM_DIA","Diatom\\nMetabolites"),("SM_CYN","Cyanobact.\\nMetabolites"),("SM_FIX","Fix-Cyan.\\nMetabolites"),("SM_NOST","Nostocales\\nMetabolites")]:
                    _node(nid, lbl, "allelo", shape="triangle", size=18, bg="#FB7185", border="#E11D48", font_color="#881337", font_size=11)
                # Sediment
                for nid, lbl in [("SED_OM","Sediment OM"),("SED_NUT","Sediment\\nNutrients"),("SED_REDOX","Sediment\\nRedox"),("SED_TRANS","Transport\\n(Diff/Adv)")]:
                    _node(nid, lbl, "sediment", shape="box", size=25, bg="#64748B", border="#334155", font_color="#FFFFFF", font_size=12)
                # CO2SYS
                _node("CO2SYS","CO2SYS\\nCarbonate","co2sys",shape="box",size=25,bg="#E2E8F0",border="#94A3B8",font_color="#1E293B",font_size=12)

                # ── Edges ──
                for t in ["DIA","CYN","FIX","OPA","NOST","ZOO","DOM","POM","DO"]:
                    _edge("TEMP",t,color="#F59E0B44",width=1,dashes=True)
                for p in ["DIA","CYN","FIX","OPA","NOST","MAC"]:
                    _edge("PAR",p,"Light",color="#FBBF24",width=1.5)
                _edge("WIND","DO","Aeration",color="#FBBF24",width=1.5)
                _edge("ICE","DO","Suppress aeration",color="#93C5FD",width=1.5)
                _edge("SALT","CO2SYS","",color="#FBBF24",width=1)
                _edge("DEPTH","DO","",color="#F59E0B44",width=1,dashes=True)
                g="#059669"
                for p in ["DIA","CYN","FIX","OPA","NOST"]:
                    _edge("NH4",p,"N uptake",color=g,width=1.5)
                    _edge("NO3",p,"N uptake",color=g,width=1.5)
                    _edge("PO4",p,"P uptake",color=g,width=1.5)
                    _edge("DIC",p,"Photosynthesis",color="#6EE7B7",width=1.5)
                    _edge(p,"DO","O2",color="#38BDF8",width=2)
                    _edge(p,"POM","Death",color="#B45309",width=1.5)
                    _edge(p,"DOM","Excretion",color="#C4B5FD",width=1)
                _edge("DSi","DIA","Si uptake",color="#1D4ED8",width=2)
                _edge("DIA","ZOO","Graze(0.26)",color="#DC2626",width=2.5)
                _edge("CYN","ZOO","Graze(0.10)",color="#DC2626",width=1.5)
                _edge("FIX","ZOO","Graze(0.07)",color="#DC2626",width=1.2)
                _edge("OPA","ZOO","Graze(0.37)",color="#DC2626",width=3)
                _edge("NOST","ZOO","Graze",color="#DC2626",width=1.5)
                _edge("POM","ZOO","Graze(0.20)",color="#DC2626",width=2)
                _edge("ZOO","POM","Death+faecal",color="#B45309",width=2)
                _edge("ZOO","DOM","Excretion",color="#C4B5FD",width=1.5)
                _edge("ZOO","DO","Respiration",color="#FCA5A5",width=1.5)
                _edge("POM","DOM","Dissolution",color="#7C3AED",width=3)
                _edge("DOM","DIC","Mineralisation",color="#6D28D9",width=3)
                _edge("DOM","NH4","N mineral.",color="#3B82F6",width=2)
                _edge("DOM","PO4","P mineral.",color="#3B82F6",width=2)
                _edge("NH4","NO3","Nitrification",color="#1D4ED8",width=2.5)
                _edge("NO3","DIC","Denitrification",color="#7C3AED",width=2)
                _edge("MN4","MN2","Mn reduction",color="#8B5CF6",width=2)
                _edge("FE3","FE2","Fe reduction",color="#8B5CF6",width=2)
                _edge("SO4","H2S","SO4 reduction",color="#8B5CF6",width=2)
                _edge("DOM","CH4","Methanogenesis",color="#8B5CF6",width=2)
                _edge("FE2","FE3","Oxidation",color="#A78BFA",width=1.5)
                _edge("MN2","MN4","Oxidation",color="#A78BFA",width=1.5)
                _edge("H2S","SO4","Oxidation",color="#A78BFA",width=1.5)
                _edge("CH4","DIC","CH4 oxidation",color="#A78BFA",width=1.5)
                _edge("FE3","PO4","FePO4",color="#E11D48",width=2,dashes=True)
                _edge("DIC","CO2SYS","Equilibrium",color="#94A3B8",width=2)
                _edge("ALK","CO2SYS","Equilibrium",color="#94A3B8",width=2)
                _edge("CO2SYS","DIC","pH feedback",color="#94A3B8",width=1,dashes=True)
                _edge("DIA","PSi","Diatom mort.",color="#1D4ED8",width=1.5)
                _edge("PSi","DSi","Dissolution",color="#60A5FA",width=1.5)
                sc="#475569"
                _edge("POM","SED_OM","Settling",color=sc,width=2.5)
                _edge("PSi","SED_OM","Settling",color=sc,width=1.5)
                for p in ["DIA","CYN","OPA"]:
                    _edge(p,"SED_OM","Settling",color=sc,width=1)
                sf="#64748B"
                _edge("SED_NUT","NH4","Sed flux",color=sf,width=2)
                _edge("SED_NUT","NO3","Sed flux",color=sf,width=1.5)
                _edge("SED_NUT","PO4","Sed flux",color=sf,width=2)
                _edge("SED_NUT","DSi","Sed flux",color=sf,width=1.5)
                _edge("SED_NUT","DO","O2 demand",color="#DC2626",width=2.5)
                _edge("SED_OM","DOM","DOC release",color=sf,width=1.5)
                _edge("SED_OM","DIC","DIC release",color=sf,width=1.5)
                _edge("SED_REDOX","FE2","Fe2+ flux",color=sf,width=1)
                _edge("SED_REDOX","H2S","H2S flux",color=sf,width=1)
                _edge("SED_REDOX","CH4","CH4 flux",color=sf,width=1)
                _edge("SED_OM","SED_NUT","Mineralisation",color="#334155",width=2)
                _edge("SED_OM","SED_REDOX","Redox coupling",color="#334155",width=2)
                for t in ["SED_OM","SED_NUT","SED_REDOX"]:
                    _edge("SED_TRANS",t,"Transport",color="#475569",width=1,dashes=True)
                mc="#0D9488"
                _edge("NH4","MAC","N uptake",color=mc,width=1.5)
                _edge("NO3","MAC","N uptake",color=mc,width=1.5)
                _edge("PO4","MAC","P uptake",color=mc,width=1.5)
                _edge("MAC","DO","O2",color="#38BDF8",width=1.5)
                _edge("MAC","MACDET","Death",color=mc,width=2)
                _edge("MACDET","POM","Deattach",color=mc,width=2)
                ac="#E11D48"
                _edge("DIA","SM_DIA","Metabolites",color=ac,width=1)
                _edge("CYN","SM_CYN","Metabolites",color=ac,width=1)
                _edge("FIX","SM_FIX","Metabolites",color=ac,width=1)
                _edge("NOST","SM_NOST","Metabolites",color=ac,width=1)
                for sm in ["SM_DIA","SM_CYN","SM_FIX","SM_NOST"]:
                    for p in ["DIA","CYN","FIX","OPA","NOST"]:
                        _edge(sm,p,"Inhibition",color="#FDA4AF",width=1,dashes=True)

                # ── Build self-contained HTML with vis-network from CDN ──
                nodes_json = _json.dumps(nodes_data)
                edges_json = _json.dumps(edges_data)

                html_content = f'''<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>AQUABC Ecological Model</title>
<link rel="stylesheet" href="https://unpkg.com/vis-network@9.1.9/dist/dist/vis-network.min.css" />
<script src="https://unpkg.com/vis-network@9.1.9/dist/vis-network.min.js"></script>
<style>
  body {{ margin: 0; font-family: 'Segoe UI', Helvetica, Arial, sans-serif; overflow: hidden; }}
  .diagram-header {{
    background: linear-gradient(135deg, #1E293B 0%, #334155 100%);
    color: #F1F5F9; padding: 14px 24px 10px; text-align: center;
  }}
  .diagram-header h1 {{ margin: 0 0 4px; font-size: 1.4em; font-weight: 600; }}
  .diagram-header p {{ margin: 0; font-size: 0.8em; color: #94A3B8; }}
  .legend {{
    display: flex; flex-wrap: wrap; justify-content: center;
    gap: 8px 14px; padding: 8px 16px; background: #F8FAFC;
    border-bottom: 1px solid #E2E8F0; font-size: 0.75em;
  }}
  .legend-item {{ display: flex; align-items: center; gap: 4px; }}
  .legend-swatch {{
    width: 12px; height: 12px; border-radius: 3px;
    border: 1.5px solid rgba(0,0,0,0.15); flex-shrink: 0;
  }}
  #network-container {{
    width: 100%; height: calc(100vh - 100px);
    background-color: #F5F7FA; border-top: 1px solid lightgray;
  }}
  div.vis-navigation div.vis-button {{
    background-color: #334155 !important;
    border: 1.5px solid #475569 !important;
    border-radius: 6px !important;
  }}
  div.vis-navigation div.vis-button:hover {{
    background-color: #475569 !important;
  }}
</style>
</head>
<body>
<div class="diagram-header">
  <h1>AQUABC v0.7.0 &mdash; Ecological Model Architecture</h1>
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
<div id="network-container"></div>
<script>
var nodes = new vis.DataSet({nodes_json});
var edges = new vis.DataSet({edges_json});
var container = document.getElementById("network-container");
var data = {{ nodes: nodes, edges: edges }};
var options = {{
  physics: {{
    barnesHut: {{
      gravitationalConstant: -12000, centralGravity: 0.8,
      springLength: 180, springConstant: 0.02,
      damping: 0.4, avoidOverlap: 0.6
    }},
    maxVelocity: 30, minVelocity: 0.75,
    stabilization: {{ enabled: true, iterations: 500, updateInterval: 25 }}
  }},
  edges: {{
    smooth: {{ type: "curvedCW", roundness: 0.15 }},
    font: {{ size: 10, color: "#555555", strokeWidth: 2, strokeColor: "#FFFFFF" }},
    arrows: {{ to: {{ enabled: true, scaleFactor: 0.7 }} }}
  }},
  nodes: {{
    font: {{ size: 13, face: "Helvetica" }},
    borderWidth: 2, shadow: true
  }},
  interaction: {{ hover: true, tooltipDelay: 200, navigationButtons: true, keyboard: true }}
}};
var network = new vis.Network(container, data, options);
</script>
</body>
</html>'''

                with open(diagram_path, "w", encoding="utf-8") as f:
                    f.write(html_content)
                logger.info(f"Model diagram written to {diagram_path}")

            # Return an iframe pointing to the static file
            return ui.HTML(
                '<iframe src="model_diagram.html" '
                'style="width:100%;height:calc(100vh - 180px);border:none;border-radius:8px;" '
                'loading="lazy" sandbox="allow-scripts allow-same-origin"></iframe>'
            )

        except Exception as e:
            logger.error(f"Error generating model structure diagram: {e}")
            import traceback
            logger.error(traceback.format_exc())
            return ui.div(
                ui.tags.p(f"Error generating diagram: {e}", class_="text-danger"),
                ui.tags.p("Ensure pyvis is installed in the shiny environment.", class_="text-muted small")
            )
