"""Box-network input parsing + Map-Display figures (extracted from server())."""
import os
import logging
import plotly.graph_objects as go

logger = logging.getLogger("AQUABC")


def parse_pelagic_inputs(inputs_dir):
    """Parse PELAGIC_INPUTS.txt for box data: depths, sediment types, basin mapping."""
    path = os.path.join(inputs_dir, "PELAGIC_INPUTS.txt")
    boxes = {}
    if not os.path.isfile(path):
        return boxes
    try:
        with open(path, 'r') as fh:
            lines = fh.readlines()
        # Find INITIAL CONDITIONS section
        in_ic = False
        for line in lines:
            stripped = line.strip()
            if "INITIAL CONDITIONS" in stripped:
                in_ic = True
                continue
            if in_ic and stripped.startswith("#"):
                # End of IC section
                if "MASS LOADS" in stripped:
                    break
                continue
            if in_ic and stripped:
                parts = stripped.split()
                if len(parts) >= 4:
                    try:
                        box_no = int(parts[0])
                        ic_set = int(parts[1])    # 1=Mud, 2=Sand
                        surf_elev = float(parts[2])
                        bot_elev = float(parts[3])
                        boxes[box_no] = {
                            'ic_set': ic_set,
                            'sediment': 'Mud' if ic_set == 1 else 'Sand',
                            'surface_elevation': surf_elev,
                            'bottom_elevation': bot_elev,
                            'depth': abs(bot_elev - surf_elev),
                        }
                    except (ValueError, IndexError):
                        pass
    except Exception as e:
        logger.error(f"Error parsing PELAGIC_INPUTS.txt: {e}")
    return boxes


def parse_advective_links(inputs_dir):
    """Parse ADVECTIVE_LINKS.txt for box connectivity."""
    path = os.path.join(inputs_dir, "ADVECTIVE_LINKS.txt")
    links = []
    if not os.path.isfile(path):
        return links
    try:
        with open(path, 'r') as fh:
            for line in fh:
                stripped = line.strip()
                if stripped.startswith("#") or not stripped:
                    continue
                parts = stripped.split()
                if len(parts) >= 3:
                    try:
                        upstream = int(parts[1])
                        downstream = int(parts[2])
                        links.append((upstream, downstream))
                    except (ValueError, IndexError):
                        pass
    except Exception as e:
        logger.error(f"Error parsing ADVECTIVE_LINKS.txt: {e}")
    return links


def parse_bathymetry(box_no, inputs_dir):
    """Parse BATHYMETRY_{box_no}.txt for layer data."""
    path = os.path.join(inputs_dir, f"BATHYMETRY_{box_no}.txt")
    layers = []
    if not os.path.isfile(path):
        return layers
    try:
        with open(path, 'r') as fh:
            lines = fh.readlines()
        # Skip header lines (first 3 lines: title, NUM_LAYERS, count, column headers)
        data_start = None
        for i, line in enumerate(lines):
            stripped = line.strip()
            parts = stripped.split()
            if len(parts) >= 7:
                try:
                    int(parts[0])  # layer number
                    float(parts[1])  # upper elevation
                    data_start = i
                    break
                except ValueError:
                    continue
        if data_start is not None:
            for line in lines[data_start:]:
                parts = line.strip().split()
                if len(parts) >= 7:
                    try:
                        layers.append({
                            'layer_no': int(parts[0]),
                            'upper_elevation': float(parts[1]),
                            'lower_elevation': float(parts[2]),
                            'upper_area': float(parts[3]),
                            'lower_area': float(parts[4]),
                            'upper_length': float(parts[5]),
                            'lower_length': float(parts[6]),
                        })
                    except (ValueError, IndexError):
                        pass
    except Exception as e:
        logger.error(f"Error parsing BATHYMETRY_{box_no}.txt: {e}")
    return layers


def build_box_network_figure(boxes, links):
    """Build a mosaic diagram with tall rectangular cells (height = 3×width).

    Box 12 (Curonian Spit) spans 6 grid rows alongside the lagoon,
    reaching the same level as box 10 (Klaipeda Strait).
    Boxes 4 and 16 sit in a separate spit column (col −1) detached
    from central-lagoon boxes 9, 8, 14.

    Open boundaries are shown as thick red edges on the pelagic box
    faces rather than as separate rectangles.

    Geographic orientation (Curonian Lagoon):
        • North  (top)   – Klaipeda Strait → Baltic Sea
        • West   (left)  – Curonian Spit (narrow sand bar)
        • East   (right) – Nemunas river delta (box 24)
        • South  (bottom)– Southern lagoon (Kaliningrad region)
    """

    BW = 1     # display width  per grid column
    BH = 3     # display height per grid row  (3× width)

    # ---- box geometry  (col, row, col_span, row_span) ----
    BOX_GEOM = {
        # North – Klaipeda Strait
        10: (1, 10, 1, 1),
         1: (1,  9, 1, 1),
        # Curonian Spit strip  (col −1 & col 0)
         4: (-1, 8, 1, 1),         # spit – upper
        12: (0,  5, 1, 6),         # spit – BIG  (rows 5-10, same level as box 10)
        16: (-1, 5, 1, 1),         # spit – lower
        # Upper lagoon
        13: (1,  8, 1, 1),
         7: (2,  8, 1, 1),
        # Transition
        11: (2,  7, 1, 1),
        # Central
        20: (1,  6, 1, 1),
        22: (2,  6, 1, 1),
        # Central wide
         9: (1,  5, 1, 1),
         5: (2,  5, 1, 1),
        # Mid-south
         8: (1,  4, 1, 1),
         6: (2,  4, 1, 1),
        # South + Nemunas arm
        14: (0,  3, 1, 1),
        17: (1,  3, 1, 1),
        25: (2,  3, 1, 1),
         2: (3,  3, 1, 1),
        24: (4,  3, 1, 1),
        # Southern lagoon
        21: (0,  2, 1, 1),
         3: (1,  2, 1, 1),
        15: (2,  2, 1, 1),
        # Southernmost
        18: (0,  1, 1, 1),
        23: (1,  1, 1, 1),
        19: (2,  1, 1, 1),
    }

    # Open boundaries as thick red edges on pelagic box faces
    # (boundary_id, pelagic_box, side, label)
    BOUNDARY_EDGES = [
        (-1, 12, 'top',    'Baltic\n(−1)'),
        (-2, 24, 'right',  'Nemunas (−2)'),
        (-3, 24, 'top',    'Nemunas (−3)'),
        (-4, 19, 'right',  'River (−4)'),
        (-5, 19, 'bottom', 'River (−5)'),
    ]

    def drect(bid):
        """Display rectangle (x0, y0, x1, y1) for a box."""
        c, r, cs, rs = BOX_GEOM[bid]
        return (c * BW, r * BH, (c + cs) * BW, (r + rs) * BH)

    def shares_edge(a, b):
        """True when a and b share a non-zero-length boundary."""
        if a not in BOX_GEOM or b not in BOX_GEOM:
            return False
        ax0, ay0, ax1, ay1 = drect(a)
        bx0, by0, bx1, by1 = drect(b)
        # vertical shared edge (side-by-side)
        if abs(ax1 - bx0) < 1e-9 or abs(bx1 - ax0) < 1e-9:
            if min(ay1, by1) - max(ay0, by0) > 1e-9:
                return True
        # horizontal shared edge (stacked)
        if abs(ay1 - by0) < 1e-9 or abs(by1 - ay0) < 1e-9:
            if min(ax1, bx1) - max(ax0, bx0) > 1e-9:
                return True
        return False

    def shared_boundary(a, b):
        """Return ('v'|'h', coord, lo, hi) describing the shared edge."""
        ax0, ay0, ax1, ay1 = drect(a)
        bx0, by0, bx1, by1 = drect(b)
        for xa, xb in [(ax1, bx0), (bx1, ax0)]:
            if abs(xa - xb) < 1e-9:
                lo, hi = max(ay0, by0), min(ay1, by1)
                if hi - lo > 1e-9:
                    return ('v', xa, lo, hi)
        for ya, yb in [(ay1, by0), (by1, ay0)]:
            if abs(ya - yb) < 1e-9:
                lo, hi = max(ax0, bx0), min(ax1, bx1)
                if hi - lo > 1e-9:
                    return ('h', ya, lo, hi)
        return None

    # ---- classify links ----
    edge_set = set()
    for up, down in links:
        edge_set.add((min(up, down), max(up, down)))

    adjacent_links = set()
    distant_links = []
    for u, v in edge_set:
        if u not in BOX_GEOM or v not in BOX_GEOM:
            continue  # skip boundary edges – shown as red lines
        if shares_edge(u, v):
            adjacent_links.add((u, v))
        else:
            distant_links.append((u, v))

    false_adj = set()
    ids = sorted(BOX_GEOM)
    for i, a in enumerate(ids):
        for b in ids[i + 1:]:
            if shares_edge(a, b):
                key = (min(a, b), max(a, b))
                if key not in edge_set:
                    false_adj.add(key)

    # ---- colour helpers ----
    max_depth = max((i['depth'] for i in boxes.values()), default=35)

    def fill_clr(depth):
        t = min(depth / max_depth, 1.0) if max_depth > 0 else 0
        r = int(214 * (1 - t) + 21 * t)
        g = int(234 * (1 - t) + 67 * t)
        b = int(248 * (1 - t) + 96 * t)
        return f'rgb({r},{g},{b})'

    def txt_clr(depth):
        t = min(depth / max_depth, 1.0) if max_depth > 0 else 0
        return '#ffffff' if t > 0.30 else '#1b2631'

    # ---- build figure ----
    fig = go.Figure()
    shapes = []
    annotations = []

    # Draw pelagic boxes
    for box_no, (c, r, cs, rs) in BOX_GEOM.items():
        info = boxes.get(box_no, {})
        d = info.get('depth', 0)
        sed = info.get('sediment', '?')
        sedchar = 'M' if sed == 'Mud' else 'S'
        border = '#6e4b1e' if sed == 'Mud' else '#1a5276'
        tc = txt_clr(d)

        x0, y0 = c * BW, r * BH
        x1, y1 = (c + cs) * BW, (r + rs) * BH
        cx, cy = (x0 + x1) / 2, (y0 + y1) / 2

        shapes.append(dict(
            type='rect', x0=x0, y0=y0, x1=x1, y1=y1,
            fillcolor=fill_clr(d),
            line=dict(color=border, width=2),
            layer='below',
        ))
        annotations.append(dict(
            x=cx, y=cy + 0.45,
            text=f'<b>{box_no}</b>',
            showarrow=False,
            font=dict(size=14, color=tc, family='Arial Black'),
        ))
        annotations.append(dict(
            x=cx, y=cy - 0.45,
            text=f'{d:.0f}m {sedchar}',
            showarrow=False,
            font=dict(size=9, color=tc, family='Arial'),
        ))

    # ---- Draw boundary edges as thick red lines ----
    BND_CLR = 'rgba(231,76,60,0.9)'
    BND_W = 5
    for bnd_id, box_id, side, label in BOUNDARY_EDGES:
        bx0, by0, bx1, by1 = drect(box_id)
        if side == 'top':
            lx0, ly0, lx1, ly1 = bx0, by1, bx1, by1
            ax, ay, anch = (bx0 + bx1) / 2, by1 + 1.0, 'bottom'
        elif side == 'bottom':
            lx0, ly0, lx1, ly1 = bx0, by0, bx1, by0
            ax, ay, anch = (bx0 + bx1) / 2, by0 - 1.0, 'top'
        elif side == 'left':
            lx0, ly0, lx1, ly1 = bx0, by0, bx0, by1
            ax, ay, anch = bx0 - 0.4, (by0 + by1) / 2, 'right'
        else:  # right
            lx0, ly0, lx1, ly1 = bx1, by0, bx1, by1
            ax, ay, anch = bx1 + 0.4, (by0 + by1) / 2, 'left'
        shapes.append(dict(
            type='line', x0=lx0, y0=ly0, x1=lx1, y1=ly1,
            line=dict(color=BND_CLR, width=BND_W),
            layer='above',
        ))
        annotations.append(dict(
            x=ax, y=ay,
            text=f'<b>{label}</b>',
            showarrow=False,
            font=dict(size=9, color='#e74c3c', family='Arial'),
            xanchor=anch if side in ('left', 'right') else 'center',
            yanchor=anch if side in ('top', 'bottom') else 'middle',
        ))

    # ---- gap separators for FALSE adjacencies ----
    GAP = 0.07
    for a, b in false_adj:
        bnd = shared_boundary(a, b)
        if not bnd:
            continue
        orient, coord, lo, hi = bnd
        pad = max(0.15, (hi - lo) * 0.04)
        if orient == 'v':
            shapes.append(dict(
                type='rect',
                x0=coord - GAP, y0=lo + pad,
                x1=coord + GAP, y1=hi - pad,
                fillcolor='#1b2631', line=dict(width=0),
                layer='above',
            ))
        else:
            shapes.append(dict(
                type='rect',
                x0=lo + pad, y0=coord - GAP,
                x1=hi - pad, y1=coord + GAP,
                fillcolor='#1b2631', line=dict(width=0),
                layer='above',
            ))

    # ---- distant connections (thin dashed lines) ----
    dx, dy = [], []
    for u, v in distant_links:
        ru = drect(u); rv = drect(v)
        dx.extend([(ru[0] + ru[2]) / 2, (rv[0] + rv[2]) / 2, None])
        dy.extend([(ru[1] + ru[3]) / 2, (rv[1] + rv[3]) / 2, None])
    if dx:
        fig.add_trace(go.Scatter(
            x=dx, y=dy, mode='lines',
            line=dict(color='rgba(46,204,113,0.55)', width=1.5, dash='dot'),
            hoverinfo='none', showlegend=False,
        ))

    # ---- invisible hover markers ----
    hvr_x, hvr_y, hvr_txt = [], [], []
    for box_no, (c, r, cs, rs) in BOX_GEOM.items():
        x0, y0 = c * BW, r * BH
        x1, y1 = (c + cs) * BW, (r + rs) * BH
        cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
        info = boxes.get(box_no, {})
        nbrs = sorted(set(
            [v for u, v in edge_set if u == box_no] +
            [u for u, v in edge_set if v == box_no]
        ))
        nbrs_int = [n for n in nbrs if n > 0]
        nbrs_bnd = [n for n in nbrs if n < 0]
        hvr_x.append(cx)
        hvr_y.append(cy)
        bnd_str = (f"<br>Boundaries: {', '.join(str(n) for n in nbrs_bnd)}"
                   if nbrs_bnd else "")
        hvr_txt.append(
            f"<b>Box {box_no}</b><br>"
            f"Depth: {info.get('depth', 0):.1f} m<br>"
            f"Bottom: {info.get('bottom_elevation', 0):.1f} m<br>"
            f"Surface: {info.get('surface_elevation', 0):.4f} m<br>"
            f"Sediment: {info.get('sediment', '?')}<br>"
            f"Connected to: {', '.join(str(n) for n in nbrs_int)}"
            f"{bnd_str}"
        )
    fig.add_trace(go.Scatter(
        x=hvr_x, y=hvr_y, mode='markers',
        marker=dict(size=35, color='rgba(0,0,0,0)'),
        hovertext=hvr_txt, hoverinfo='text',
        showlegend=False,
    ))

    # ---- geographic annotations ----
    annotations.extend([
        dict(x=1.5, y=34.5,
             text='<b>↑ Klaipeda Strait → Baltic Sea</b>',
             showarrow=False,
             font=dict(size=11, color='#5dade2')),
        dict(x=-2.0, y=22.0,
             text='C<br>u<br>r<br>o<br>n<br>i<br>a<br>n<br><br>'
                  'S<br>p<br>i<br>t',
             showarrow=False,
             font=dict(size=8, color='#7f8c8d'),
             align='center'),
        dict(x=6.2, y=10.5,
             text='<b>← Nemunas</b><br>delta',
             showarrow=False,
             font=dict(size=10, color='#e74c3c'),
             align='center'),
        dict(x=1.0, y=1.5,
             text='↓ South Lagoon',
             showarrow=False,
             font=dict(size=9, color='#7f8c8d')),
    ])

    # ---- legend entries ----
    fig.add_trace(go.Scatter(
        x=[None], y=[None], mode='markers',
        marker=dict(size=12, color=fill_clr(3), symbol='square',
                    line=dict(color='#1a5276', width=2)),
        name='Sand (shallow)', showlegend=True,
    ))
    fig.add_trace(go.Scatter(
        x=[None], y=[None], mode='markers',
        marker=dict(size=12, color=fill_clr(max_depth), symbol='square',
                    line=dict(color='#1a5276', width=2)),
        name='Sand (deep)', showlegend=True,
    ))
    fig.add_trace(go.Scatter(
        x=[None], y=[None], mode='markers',
        marker=dict(size=12, color=fill_clr(5), symbol='square',
                    line=dict(color='#6e4b1e', width=2)),
        name='Mud substrate', showlegend=True,
    ))
    fig.add_trace(go.Scatter(
        x=[None], y=[None], mode='lines',
        line=dict(color=BND_CLR, width=BND_W),
        name='Open boundary', showlegend=True,
    ))
    fig.add_trace(go.Scatter(
        x=[None], y=[None], mode='lines',
        line=dict(color='rgba(46,204,113,0.55)', width=1.5, dash='dot'),
        name='Non-adjacent link', showlegend=True,
    ))

    # colourbar
    fig.add_trace(go.Scatter(
        x=[None], y=[None], mode='markers',
        marker=dict(
            size=0, color=[0, max_depth],
            colorscale=[[0, 'rgb(214,234,248)'], [1, 'rgb(21,67,96)']],
            colorbar=dict(title='Depth (m)', thickness=12,
                          len=0.35, x=1.02, y=0.3),
            showscale=True),
        showlegend=False, hoverinfo='none',
    ))

    fig.update_layout(
        shapes=shapes,
        annotations=annotations,
        title=dict(
            text='AQUABC Box Model — Curonian Lagoon (25 Pelagic Boxes)',
            font=dict(size=14)),
        showlegend=True,
        legend=dict(x=0.72, y=0.99, bgcolor='rgba(0,0,0,0.35)',
                    font=dict(size=9)),
        xaxis=dict(visible=False, range=[-2.5, 7.0],
                   scaleanchor='y', scaleratio=1),
        yaxis=dict(visible=False, range=[-2, 37]),
        plot_bgcolor='rgba(0,0,0,0)',
        paper_bgcolor='rgba(0,0,0,0)',
        margin=dict(l=5, r=50, t=40, b=10),
        height=1000,
        template='plotly_dark',
    )
    return fig


def build_bathymetry_figure(box_no, layers, boxes):
    """Build a plotly figure showing bathymetry cross-section for a box."""
    if not layers:
        fig = go.Figure()
        fig.add_annotation(text=f"No bathymetry data for Box {box_no}", showarrow=False,
                           font=dict(size=16, color='#aaa'))
        fig.update_layout(height=700, template='plotly_dark',
                          paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)')
        return fig

    elevations = [(l['upper_elevation'] + l['lower_elevation']) / 2 for l in layers]
    areas_m2 = [l['upper_area'] for l in layers]
    areas_km2 = [a / 1e6 for a in areas_m2]  # Convert to km²
    layer_thickness = [l['upper_elevation'] - l['lower_elevation'] for l in layers]

    info = boxes.get(box_no, {})
    sediment = info.get('sediment', 'Unknown')
    depth = info.get('depth', 0)

    fig = go.Figure()

    # Area profile (horizontal bars showing area at each elevation)
    fig.add_trace(go.Bar(
        x=areas_km2,
        y=elevations,
        orientation='h',
        marker=dict(
            color=elevations,
            colorscale='ice',
            line=dict(width=0.5, color='rgba(44,62,80,0.5)'),
        ),
        hovertemplate='Elevation: %{y:.1f} m<br>Area: %{x:.2f} km²<extra></extra>',
        name='Layer Area',
        width=[abs(t) * 0.9 for t in layer_thickness],
    ))

    # Add water surface line
    surf = info.get('surface_elevation', 0)
    fig.add_hline(y=surf, line=dict(color='#3498db', width=2, dash='dash'),
                  annotation_text='Water Surface', annotation_position='top right')

    # Add bottom line
    bot = info.get('bottom_elevation', 0)
    fig.add_hline(y=bot, line=dict(color='#e74c3c', width=2, dash='dash'),
                  annotation_text='Bottom', annotation_position='bottom right')

    fig.update_layout(
        title=dict(text=f'Box {box_no} Bathymetry — {sediment} substrate, {depth:.1f} m depth',
                   font=dict(size=14)),
        xaxis_title='Area (km²)',
        yaxis_title='Elevation (m)',
        height=700,
        template='plotly_dark',
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        margin=dict(l=60, r=20, t=50, b=50),
        showlegend=False,
    )
    return fig


def build_depths_overview(boxes):
    """Build a plotly figure showing all box depths as a bar chart."""
    if not boxes:
        fig = go.Figure()
        fig.add_annotation(text="No box data available", showarrow=False,
                           font=dict(size=16, color='#aaa'))
        fig.update_layout(height=700, template='plotly_dark',
                          paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)')
        return fig

    box_nos = sorted(boxes.keys())
    depths = [boxes[b]['depth'] for b in box_nos]
    bottoms = [boxes[b]['bottom_elevation'] for b in box_nos]
    sediments = [boxes[b]['sediment'] for b in box_nos]
    colors = ['#3498db' if s == 'Sand' else '#8e6c3a' for s in sediments]

    hover_texts = [
        f"Box {b}<br>Depth: {boxes[b]['depth']:.1f} m<br>"
        f"Bottom: {boxes[b]['bottom_elevation']:.1f} m<br>"
        f"Sediment: {boxes[b]['sediment']}"
        for b in box_nos
    ]

    fig = go.Figure()
    fig.add_trace(go.Bar(
        x=[f"Box {b}" for b in box_nos],
        y=bottoms,
        marker=dict(color=colors, line=dict(width=1, color='#2c3e50')),
        hovertext=hover_texts,
        hoverinfo='text',
        name='Bottom Elevation',
    ))

    # Add surface elevation markers
    surfs = [boxes[b]['surface_elevation'] for b in box_nos]
    fig.add_trace(go.Scatter(
        x=[f"Box {b}" for b in box_nos],
        y=surfs,
        mode='markers',
        marker=dict(size=8, color='#1abc9c', symbol='diamond'),
        name='Surface Elevation',
        hovertemplate='Box %{x}<br>Surface: %{y:.4f} m<extra></extra>',
    ))

    # Add legend entries for sediment types
    fig.add_trace(go.Bar(x=[None], y=[None], marker=dict(color='#3498db'), name='Sand', showlegend=True))
    fig.add_trace(go.Bar(x=[None], y=[None], marker=dict(color='#8e6c3a'), name='Mud', showlegend=True))

    fig.update_layout(
        title=dict(text='All Boxes — Bottom Elevation & Sediment Type', font=dict(size=14)),
        xaxis_title='Box',
        yaxis_title='Elevation (m)',
        height=700,
        template='plotly_dark',
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        margin=dict(l=60, r=20, t=50, b=80),
        xaxis=dict(tickangle=-45),
        legend=dict(x=0.75, y=0.98, bgcolor='rgba(255,255,255,0.1)'),
        barmode='relative',
    )
    return fig
