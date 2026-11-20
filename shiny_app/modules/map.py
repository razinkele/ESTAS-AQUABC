"""Map tab as a Shiny module, drawn with deck.gl through `shiny_deckgl`.

`map_ui(id)` returns the panel *content* (the app-level panel_conditional
stays in create_ui); `map_server(id, state)` pushes the station layer and
follows the controls.

The map is created once and then driven by messages: the camera with
`ease_to`, the basemap with `set_style`, the layers with `update`. Nothing
rebuilds the widget, so the view does not jump when a control changes.

`MapWidget` resolves its id through the Shiny module namespace at
construction, so `_map_widget()` must be called once inside `map_ui` and
once inside `map_server`: both then name the same DOM element.
"""
import logging

from shiny import module, reactive, render, ui
from shiny_deckgl import (
    CARTO_DARK,
    CARTO_POSITRON,
    CARTO_VOYAGER,
    OSM_LIBERTY,
    MapWidget,
    column_layer,
    head_includes,
    tile_layer,
)

logger = logging.getLogger("AQUABC")

MAP_ID = "deck_map"

DEFAULT_VIEW = {"longitude": 21.10, "latitude": 55.32, "zoom": 10, "pitch": 45, "bearing": 0}

# Vector basemaps (MapLibre style URLs).
_CARTO_CREDIT = "© CARTO, © OpenStreetMap contributors"
VECTOR_STYLES = {
    "voyager": ("Streets (Carto Voyager)", CARTO_VOYAGER, _CARTO_CREDIT),
    "positron": ("Light (Carto)", CARTO_POSITRON, _CARTO_CREDIT),
    "dark": ("Dark (Carto)", CARTO_DARK, _CARTO_CREDIT),
    "osm": ("OpenStreetMap (Liberty)", OSM_LIBERTY,
            "OpenFreeMap, © OpenMapTiles, data from OpenStreetMap"),
}

# Raster basemaps have no MapLibre style of their own: they are drawn as a
# deck.gl tile layer over the light vector basemap. A deck.gl layer shows no
# credit by itself, and both providers require one: `basemap_credit` supplies
# the line shown under the map.
RASTER_TILES = {
    "satellite": (
        "Satellite (Esri)",
        "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        "Tiles © Esri — Source: Esri, Maxar, Earthstar Geographics, and the GIS User Community",
    ),
    "topo": (
        "Topographic (OpenTopoMap)",
        "https://a.tile.opentopomap.org/{z}/{x}/{y}.png",
        "Map data: © OpenStreetMap contributors, SRTM | Map style: © OpenTopoMap (CC-BY-SA)",
    ),
}

DEFAULT_STYLE = "voyager"

# Observation station locations in the Curonian Lagoon (WGS84 / EPSG:4326).
# sta1ND = Nida, sta2VM = Ventė Cape. `value` is a sample quantity.
STATIONS = [
    {"lat": 55.3028, "lon": 21.0003, "name": "Nida (sta1ND)", "value": 15},
    {"lat": 55.3417, "lon": 21.1900, "name": "Ventė (sta2VM)", "value": 25},
]

VALUE_MAX = 30.0
# Metres of column height per unit of `value` at elevation scale 1.
ELEVATION_UNIT_M = 20.0


def station_rows(stations=STATIONS):
    """Layer rows for the stations: position, a green-to-red colour by value,
    and the fields the tooltip shows."""
    rows = []
    for s in stations:
        t = min(max(s["value"] / VALUE_MAX, 0.0), 1.0)
        rows.append({
            "position": [s["lon"], s["lat"]],
            "name": s["name"],
            "value": s["value"],
            "lat": f"{s['lat']:.4f}",
            "lon": f"{s['lon']:.4f}",
            "color": [int(200 * t), int(200 * (1 - t)), 80, 200],
        })
    return rows


def build_layers(style_key, point_radius, elevation_scale):
    """The deck.gl layers for the current controls: an optional raster
    basemap underneath, then one column per station."""
    layers = []
    if style_key in RASTER_TILES:
        layers.append(tile_layer("basemap_raster", RASTER_TILES[style_key][1], pickable=False))
    layers.append(column_layer(
        "stations",
        station_rows(),
        getPosition="@@d.position",
        getElevation="@@d.value",
        getFillColor="@@d.color",
        elevationScale=float(elevation_scale) * ELEVATION_UNIT_M,
        radius=float(point_radius),
        diskResolution=24,
    ))
    return layers


def style_url(style_key):
    """The MapLibre style behind a basemap choice (raster choices sit on the
    light vector basemap)."""
    if style_key in VECTOR_STYLES:
        return VECTOR_STYLES[style_key][1]
    return CARTO_POSITRON


def basemap_credit(style_key):
    """The credit line for a basemap choice."""
    entry = VECTOR_STYLES.get(style_key) or RASTER_TILES.get(style_key) or VECTOR_STYLES[DEFAULT_STYLE]
    return entry[2]


CAMERA_KEYS = ("longitude", "latitude", "zoom", "pitch")


def camera_target(controls, previous, viewport):
    """Where to move the camera after a control changed.

    Only the axes whose control moved take the control's value. The others
    keep what the map shows now (`viewport`, as the map last reported it), so
    that changing the pitch does not undo a pan or a scroll-zoom the user made
    on the map itself. An axis the map has not reported yet falls back to its
    control.
    """
    target = {}
    for k in CAMERA_KEYS:
        if controls[k] != previous.get(k) or viewport.get(k) is None:
            target[k] = float(controls[k])
        else:
            target[k] = float(viewport[k])
    return target


def _map_widget():
    """The map widget. Call inside the module's UI and server functions."""
    return MapWidget(
        MAP_ID,
        view_state=dict(DEFAULT_VIEW),
        style=style_url(DEFAULT_STYLE),
        tooltip={
            "html": "<b>{name}</b><br/>Value: {value}<br/>Lat: {lat}<br/>Lon: {lon}",
            "style": {"backgroundColor": "#1b3346", "color": "#ffffff", "fontSize": "12px"},
        },
        controls=[
            {"type": "navigation", "position": "top-right"},
            {"type": "fullscreen", "position": "top-right"},
            {"type": "scale", "position": "bottom-left"},
        ],
        picking_radius=4,
    )


@module.ui
def map_ui():
    choices = {k: v[0] for k, v in VECTOR_STYLES.items()}
    choices.update({k: v[0] for k, v in RASTER_TILES.items()})
    return ui.card(
        head_includes(),
        ui.card_header(
            ui.tags.i(class_="bi bi-globe me-2"),
            "Geographic Visualization"
        ),
        ui.layout_columns(
            # Left column: Map controls
            ui.card(
                ui.card_header("Map Settings"),
                ui.tooltip(
                    ui.input_select("map_style", "Map Style:", choices=choices, selected=DEFAULT_STYLE),
                    "Select the base map style"
                ),
                ui.tooltip(
                    ui.input_numeric("map_lat", "Center Latitude:", value=DEFAULT_VIEW["latitude"],
                                     min=-90, max=90, step=0.01),
                    "Map center latitude coordinate"
                ),
                ui.tooltip(
                    ui.input_numeric("map_lon", "Center Longitude:", value=DEFAULT_VIEW["longitude"],
                                     min=-180, max=180, step=0.01),
                    "Map center longitude coordinate"
                ),
                ui.tooltip(
                    ui.input_slider("map_zoom", "Zoom Level:", min=1, max=18, value=DEFAULT_VIEW["zoom"], step=1),
                    "Map zoom level (1=world, 18=street level)"
                ),
                ui.tooltip(
                    ui.input_slider("map_pitch", "Pitch (3D tilt):", min=0, max=60, value=DEFAULT_VIEW["pitch"],
                                    step=5),
                    "3D perspective tilt angle"
                ),
                ui.hr(),
                ui.h6("Sample Data Points"),
                ui.tooltip(
                    ui.input_slider("map_point_radius", "Point Radius:", min=100, max=5000, value=1000, step=100),
                    "Radius of the station columns, in metres"
                ),
                ui.tooltip(
                    ui.input_slider("map_elevation_scale", "Elevation Scale:", min=1, max=100, value=10, step=1),
                    "Vertical exaggeration of the station columns"
                ),
                fill=False
            ),
            # Right column: Map display
            ui.card(
                ui.card_header("Map View"),
                _map_widget().ui(height="600px"),
                ui.output_ui("map_credit"),
                fill=True
            ),
            col_widths=[3, 9]
        ),
        ui.layout_columns(
            ui.card(
                ui.card_header("Map Information"),
                ui.output_ui("map_info"),
                fill=False
            ),
            col_widths=[12]
        )
    )


@module.server
def map_server(input, output, session, state):
    # `state` is accepted for the uniform x_server(id, state) convention; the
    # map tab is self-contained and uses nothing from it.
    widget = _map_widget()
    # The camera controls as last applied, to tell which one the user moved.
    last_controls = {k: float(DEFAULT_VIEW[k]) for k in CAMERA_KEYS}

    @reactive.effect
    async def _push_layers():
        """Send the layers whenever a control that shapes them changes."""
        await widget.update(
            session,
            build_layers(
                input.map_style() or DEFAULT_STYLE,
                input.map_point_radius() or 1000,
                input.map_elevation_scale() or 10,
            ),
        )

    @reactive.effect
    @reactive.event(input.map_style, ignore_init=True)
    async def _set_basemap():
        await widget.set_style(session, style_url(input.map_style() or DEFAULT_STYLE))

    @reactive.effect
    @reactive.event(input.map_lat, input.map_lon, input.map_zoom, input.map_pitch, ignore_init=True)
    async def _move_camera():
        lat, lon = input.map_lat(), input.map_lon()
        if lat is None or lon is None:
            return  # a numeric box being edited
        controls = {
            "longitude": float(lon),
            "latitude": float(lat),
            "zoom": float(input.map_zoom() or DEFAULT_VIEW["zoom"]),
            "pitch": float(input.map_pitch() or 0),
        }
        try:
            viewport = input[widget.view_state_input_id]() or {}
        except Exception:
            viewport = {}  # the map has not reported a view yet
        target = camera_target(controls, last_controls, viewport)
        last_controls.update(controls)
        await widget.ease_to(session, duration=600, **target)

    @render.ui
    def map_credit():
        """Credit for the chosen basemap (required by the tile providers)."""
        return ui.tags.small(basemap_credit(input.map_style() or DEFAULT_STYLE), class_="text-muted")

    @render.ui
    def map_info():
        """Display map information and instructions."""
        return ui.div(
            ui.tags.p(
                ui.tags.strong("About this map: "),
                "This interactive map displays observation stations in the Curonian Lagoon: Nida (sta1ND) and Ventė Cape (sta2VM). ",
                "Use the controls on the left to adjust the map view and visualization settings."
            ),
            ui.tags.p(
                ui.tags.strong("Features: "),
                ui.tags.ul(
                    ui.tags.li("One column per station; height and colour follow the station's value"),
                    ui.tags.li("Hover over a column to see the station details"),
                    ui.tags.li("Drag to pan, scroll to zoom, right-drag or Ctrl-drag to tilt and rotate"),
                    ui.tags.li("Full screen control available in top-right corner")
                )
            ),
            ui.tags.p(
                ui.tags.em("Map drawn with deck.gl and MapLibre through shiny_deckgl."),
                class_="text-muted"
            ),
            class_="small"
        )
