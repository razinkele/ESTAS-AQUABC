"""Map tab as a Shiny module (Phase 2, Task 1).

`map_ui(id)` returns the panel *content* (the app-level panel_conditional
stays in create_ui); `map_server(id, state)` registers `pydeck_map` and
`map_info`, ported verbatim from app.py. Despite the `pydeck_map` name, the
handler renders an ipyleaflet map (no box_network/file_locators dependency
was found when grepping the ported bodies — confirmed self-contained).
"""
import logging

import ipyleaflet as L
from ipywidgets import HTML
from shiny import module, render, ui
from shinywidgets import output_widget, render_widget

logger = logging.getLogger("AQUABC")


@module.ui
def map_ui():
    return ui.card(
        ui.card_header(
            ui.tags.i(class_="bi bi-globe me-2"),
            "Geographic Visualization"
        ),
        ui.layout_columns(
            # Left column: Map controls
            ui.card(
                ui.card_header("Map Settings"),
                ui.tooltip(
                    ui.input_select(
                        "map_style",
                        "Map Style:",
                        choices={
                            "OpenStreetMap.Mapnik": "OpenStreetMap",
                            "CartoDB.Positron": "Light (Carto)",
                            "CartoDB.DarkMatter": "Dark (Carto)",
                            "Esri.WorldImagery": "Satellite (Esri)",
                            "OpenTopoMap": "Topographic",
                        },
                        selected="OpenStreetMap.Mapnik"
                    ),
                    "Select the base map style"
                ),
                ui.tooltip(
                    ui.input_numeric("map_lat", "Center Latitude:", value=55.32, min=-90, max=90, step=0.01),
                    "Map center latitude coordinate"
                ),
                ui.tooltip(
                    ui.input_numeric("map_lon", "Center Longitude:", value=21.10, min=-180, max=180, step=0.01),
                    "Map center longitude coordinate"
                ),
                ui.tooltip(
                    ui.input_slider("map_zoom", "Zoom Level:", min=1, max=18, value=10, step=1),
                    "Map zoom level (1=world, 18=street level)"
                ),
                ui.tooltip(
                    ui.input_slider("map_pitch", "Pitch (3D tilt):", min=0, max=60, value=45, step=5),
                    "3D perspective tilt angle"
                ),
                ui.hr(),
                ui.h6("Sample Data Points"),
                ui.tooltip(
                    ui.input_slider("map_point_radius", "Point Radius:", min=100, max=5000, value=1000, step=100),
                    "Radius of sample points on the map"
                ),
                ui.tooltip(
                    ui.input_slider("map_elevation_scale", "Elevation Scale:", min=1, max=100, value=10, step=1),
                    "Vertical exaggeration for 3D elevation"
                ),
                fill=False
            ),
            # Right column: Map display
            ui.card(
                ui.card_header("Map View"),
                output_widget("pydeck_map"),
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

    # === IPYLEAFLET MAP RENDER ===
    @render_widget
    def pydeck_map():
        """Render an interactive ipyleaflet map with sample data points."""
        # Observation station locations in the Curonian Lagoon (WGS84 / EPSG:4326)
        # sta1ND = Nida, sta2VM = Ventė Cape
        stations = [
            {'lat': 55.3028, 'lon': 21.0003, 'name': 'Nida (sta1ND)', 'value': 15},
            {'lat': 55.3417, 'lon': 21.1900, 'name': 'Ventė (sta2VM)', 'value': 25},
        ]

        # Get user inputs
        center_lat = input.map_lat() or 55.32
        center_lon = input.map_lon() or 21.10
        zoom = input.map_zoom() or 10
        map_style = input.map_style() or "OpenStreetMap.Mapnik"
        point_radius = input.map_point_radius() or 1000

        # Get basemap from ipyleaflet basemaps
        basemap_parts = map_style.split('.')
        if len(basemap_parts) == 2:
            basemap = getattr(getattr(L.basemaps, basemap_parts[0], L.basemaps.OpenStreetMap), basemap_parts[1], L.basemaps.OpenStreetMap.Mapnik)
        else:
            basemap = getattr(L.basemaps, map_style, L.basemaps.OpenStreetMap.Mapnik)

        # Create the ipyleaflet map
        m = L.Map(
            center=(center_lat, center_lon),
            zoom=zoom,
            basemap=basemap,
            scroll_wheel_zoom=True,
            layout={'height': '600px'}
        )

        # Add markers for each station
        for station in stations:
            # Color based on value (gradient from green to red)
            value_normalized = station['value'] / 30.0  # Normalize to 0-1
            r = int(200 * value_normalized)
            g = int(200 * (1 - value_normalized))
            color = f'#{r:02x}{g:02x}50'

            # Create popup content
            popup_html = f"""
            <div style="font-family: Arial, sans-serif;">
                <h4 style="margin: 0 0 10px 0; color: steelblue;">{station['name']}</h4>
                <table style="width: 100%;">
                    <tr><td><b>Value:</b></td><td>{station['value']}</td></tr>
                    <tr><td><b>Lat:</b></td><td>{station['lat']:.4f}</td></tr>
                    <tr><td><b>Lon:</b></td><td>{station['lon']:.4f}</td></tr>
                </table>
            </div>
            """

            # Add circle marker
            circle = L.CircleMarker(
                location=(station['lat'], station['lon']),
                radius=int(point_radius / 100),  # Scale down for leaflet
                color=color,
                fill_color=color,
                fill_opacity=0.7,
                weight=2
            )
            circle.popup = HTML(popup_html)
            m.add_layer(circle)

        # Add layer control
        m.add_control(L.LayersControl(position='topright'))
        m.add_control(L.FullScreenControl())
        m.add_control(L.ScaleControl(position='bottomleft'))

        return m

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
                    ui.tags.li("Circle markers showing station locations (color indicates value)"),
                    ui.tags.li("Click on markers to see station details"),
                    ui.tags.li("Use mouse to pan and scroll to zoom"),
                    ui.tags.li("Full screen control available in top-right corner")
                )
            ),
            ui.tags.p(
                ui.tags.em("Map powered by ipyleaflet with free OpenStreetMap tiles."),
                class_="text-muted"
            ),
            class_="small"
        )
