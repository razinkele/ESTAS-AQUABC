#!/usr/bin/env python3
"""Study-area figure: the CL29 29-box network and the EPA monitoring stations.

Geometry comes from the sibling `curonian` repo's GeoPackage, everything in
EPSG:3346 (LKS94 / Lithuania TM) so no reprojection is needed -- the map is
drawn in projected metres and the aspect ratio is set 1:1.

GeoPackage geometry is parsed here directly (a GPKG binary header followed by
standard WKB); shapely/geopandas are not installed in this environment and are
not needed for polygons and points.

Writes figures/study_area.png.
"""
import os
import sqlite3
import struct

import matplotlib
matplotlib.use("Agg")
import matplotlib.patheffects as pe
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon as MplPolygon

# Label offsets in points, hand-set per station. The four LTK3* stations sit
# within ~2 km of each other in the Klaipeda Strait and collide at any sane
# map scale, so they are fanned out and given leader lines.
LABEL_OFFSET = {
    "LTK1": (7, 3), "LTK2": (7, -9),
    "LTK3B": (30, 9), "LTK3DT": (34, -1), "LTK3": (-40, -6),
    "LTK3A": (9, -10),
    "LTK4": (8, 1), "LTK5": (-34, 2), "LTK7B": (8, 1), "LTK6": (-32, 0),
    "LTK8": (8, -9), "LTK12": (8, 1), "LTK10": (-38, 2), "LTK14": (8, -9),
}
LEADERED = {"LTK3B", "LTK3DT", "LTK3", "LTK5", "LTK6", "LTK10"}
HALO = [pe.withStroke(linewidth=2.2, foreground="white")]

# The strait boxes are narrow, so their centroids land on top of the station
# markers. Nudge those few labels clear (offsets in projected metres).
BOX_LABEL_NUDGE = {
    7: (-3000, 1400), 11: (-3200, -2800), 17: (2600, -1300),
    20: (2700, -1500), 23: (-1400, -2800),
}

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))   # docs/report_scripts/ -> repo root
FIG_DIR = os.path.join(HERE, "figures")
os.makedirs(FIG_DIR, exist_ok=True)   # figures/ is git-ignored: absent in a fresh checkout
# The box/station geometry lives in the sibling `curonian` project, not in this
# repository, so its location cannot be derived from __file__. Set CL29_GPKG to
# point at it; the default assumes the usual side-by-side checkout.
GPKG = os.environ.get(
    "CL29_GPKG",
    os.path.join(os.path.dirname(REPO), "curonian", "curonian_db.gpkg"))
OUT = os.path.join(FIG_DIR, "study_area.png")

# The nine boxes carrying scored observations (see the station table below).
SCORED = {7, 9, 11, 14, 15, 17, 19, 20, 23}

# Station -> box, from the ingestion's station->box map; label offsets are
# hand-set so the busy strait cluster stays readable.
STATION_BOX = {
    "LTK1": 7, "LTK2": 7, "LTK3": 11, "LTK3A": 11, "LTK3B": 11, "LTK3DT": 11,
    "LTK14": 9, "LTK12": 14, "LTK10": 23, "LTK6": 15, "LTK8": 15,
    "LTK5": 17, "LTK7B": 19, "LTK4": 20,
}

_ENV_LEN = {0: 0, 1: 32, 2: 48, 3: 48, 4: 64}


def _wkb_offset(blob):
    """Skip the GeoPackage binary header, returning the offset of the WKB."""
    if blob[:2] != b"GP":
        raise ValueError("not a GeoPackage geometry blob")
    flags = blob[3]
    return 8 + _ENV_LEN[(flags >> 1) & 0x07]


def _ring(blob, off, endian):
    n, = struct.unpack_from(endian + "I", blob, off)
    off += 4
    pts = struct.unpack_from(endian + f"{2 * n}d", blob, off)
    return list(zip(pts[0::2], pts[1::2])), off + 16 * n


def _polygon(blob, off, endian):
    nring, = struct.unpack_from(endian + "I", blob, off)
    off += 4
    rings = []
    for _ in range(nring):
        r, off = _ring(blob, off, endian)
        rings.append(r)
    return rings, off


def parse(blob):
    """Return a list of rings (each a list of (x, y)) for Point/Polygon/Multi."""
    off = _wkb_offset(blob)
    endian = "<" if blob[off] == 1 else ">"
    gtype, = struct.unpack_from(endian + "I", blob, off + 1)
    off += 5
    if gtype == 1:                                    # Point
        x, y = struct.unpack_from(endian + "2d", blob, off)
        return [[(x, y)]]
    if gtype == 3:                                    # Polygon
        rings, _ = _polygon(blob, off, endian)
        return rings
    if gtype == 6:                                    # MultiPolygon
        npoly, = struct.unpack_from(endian + "I", blob, off)
        off += 4
        out = []
        for _ in range(npoly):
            endian2 = "<" if blob[off] == 1 else ">"
            off += 5                                  # per-part byte order + type
            rings, off = _polygon(blob, off, endian2)
            out.extend(rings)
        return out
    raise ValueError(f"unhandled WKB geometry type {gtype}")


con = sqlite3.connect(GPKG)
boxes = {b: parse(g) for g, b in con.execute("SELECT geom, box FROM box_poly")}
shore = [r for (g,) in con.execute("SELECT geom FROM lagoon_boundary")
         for r in parse(g)]
stations = {code: (e, n) for code, e, n in con.execute(
    "SELECT primary_code, easting, northing FROM station "
    "WHERE easting IS NOT NULL AND primary_code LIKE 'LTK%'")}
con.close()
print(f"{len(boxes)} boxes, {len(shore)} shoreline rings, "
      f"{len(stations)} stations with coordinates")

fig, ax = plt.subplots(figsize=(5.6, 10.0))

# Shoreline first, as context.
for ring in shore:
    xs, ys = zip(*ring)
    ax.fill(xs, ys, facecolor="#eef2f5", edgecolor="#9aa7b1", linewidth=0.6,
            zorder=1)

# Boxes: scored ones filled and emphasised, the rest outlined only.
for b, rings in sorted(boxes.items()):
    scored = b in SCORED
    for i, ring in enumerate(rings):
        ax.add_patch(MplPolygon(
            ring, closed=True,
            facecolor=("#bcd6ea" if scored else "#ffffff") if i == 0 else "#ffffff",
            edgecolor="#37556b" if scored else "#9aa7b1",
            linewidth=1.1 if scored else 0.6,
            alpha=0.95 if scored else 1.0, zorder=2 if scored else 1.5))
    cx = sum(p[0] for p in rings[0]) / len(rings[0])
    cy = sum(p[1] for p in rings[0]) / len(rings[0])
    ndx, ndy = BOX_LABEL_NUDGE.get(b, (0, 0))
    cx += ndx
    cy += ndy
    ax.text(cx, cy, str(b), ha="center", va="center",
            fontsize=7.5 if scored else 6.5,
            color="#1b3346" if scored else "#8794a0",
            fontweight="bold" if scored else "normal", zorder=4,
            path_effects=HALO)

# Stations.
sx = [p[0] for p in stations.values()]
sy = [p[1] for p in stations.values()]
ax.scatter(sx, sy, s=40, marker="o", facecolor="#c62828", edgecolor="white",
           linewidth=0.8, zorder=6, label="EPA monitoring station")
for code, (x, y) in sorted(stations.items()):
    dx, dy = LABEL_OFFSET.get(code, (7, 3))
    ax.annotate(
        code, (x, y), textcoords="offset points", xytext=(dx, dy),
        fontsize=6.6, color="#7f1d1d", zorder=7, path_effects=HALO,
        ha="left" if dx >= 0 else "right",
        arrowprops=(dict(arrowstyle="-", color="#c62828", linewidth=0.5,
                         shrinkA=0, shrinkB=2)
                    if code in LEADERED else None))

# Scale bar (projected metres, so this is exact).
x0, x1 = ax.get_xlim()
y0, y1 = ax.get_ylim()
bar = 10000.0
bx = x0 + 0.07 * (x1 - x0)
by = y0 + 0.05 * (y1 - y0)
ax.plot([bx, bx + bar], [by, by], color="black", linewidth=2.4, zorder=8)
ax.text(bx + bar / 2, by + 0.008 * (y1 - y0), "10 km", ha="center",
        va="bottom", fontsize=8, zorder=8)

# North arrow.
nx = x0 + 0.93 * (x1 - x0)
ny = y0 + 0.90 * (y1 - y0)
ax.annotate("N", xy=(nx, ny), xytext=(nx, ny - 0.055 * (y1 - y0)),
            ha="center", fontsize=10, fontweight="bold",
            arrowprops=dict(arrowstyle="-|>", color="black", linewidth=1.4))

ax.set_aspect("equal")
ax.set_xticks([])
ax.set_yticks([])
for s in ax.spines.values():
    s.set_edgecolor("#9aa7b1")
ax.set_title("Curonian Lagoon: CL29 box network and monitoring stations",
             fontsize=11, pad=10)

handles = [
    plt.Line2D([], [], marker="s", linestyle="none", markersize=10,
               markerfacecolor="#bcd6ea", markeredgecolor="#37556b",
               label="box with scored observations (9)"),
    plt.Line2D([], [], marker="s", linestyle="none", markersize=10,
               markerfacecolor="white", markeredgecolor="#9aa7b1",
               label="box without observations (20)"),
    plt.Line2D([], [], marker="o", linestyle="none", markersize=7,
               markerfacecolor="#c62828", markeredgecolor="white",
               label="EPA monitoring station (14)"),
]
ax.legend(handles=handles, loc="upper left", fontsize=7.8, frameon=True,
          framealpha=0.95)

fig.tight_layout()
fig.savefig(OUT, dpi=200)
print("wrote", OUT)
