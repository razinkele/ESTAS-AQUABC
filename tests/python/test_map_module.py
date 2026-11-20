try:
    from shiny_app.modules import map as map_mod
    from shiny_app.modules.map import map_ui
except ImportError:
    from modules import map as map_mod
    from modules.map import map_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_map_ui_namespaces_ids():
    html = str(map_ui("map"))
    for raw in ("deck_map", "map_info", "map_credit"):
        assert nid("map", raw) in html, f"missing namespaced id for {raw}"
    # the module UI must NOT carry the nav wrapper (that stays in create_ui)
    assert "panel_conditional" not in html
    assert "input.navigation" not in html


def test_map_container_is_a_deckgl_widget_with_a_height():
    html = str(map_ui("map"))
    # a WebGL map in a fill card needs an explicit height, or it collapses
    assert 'id="map-deck_map"' in html
    assert 'class="deckgl-map"' in html
    assert "height:600px" in html
    assert "ipyleaflet" not in html.lower()


def test_every_basemap_choice_resolves():
    keys = list(map_mod.VECTOR_STYLES) + list(map_mod.RASTER_TILES)
    assert map_mod.DEFAULT_STYLE in keys
    for k in keys:
        assert map_mod.style_url(k).startswith("https://")
        assert map_mod.basemap_credit(k).strip()
    # an unknown key falls back to a real style instead of failing
    assert map_mod.style_url("no-such-style").startswith("https://")
    assert map_mod.basemap_credit("no-such-style")
    # the raster providers require their credit to be shown
    assert "Esri" in map_mod.basemap_credit("satellite")
    assert "OpenTopoMap" in map_mod.basemap_credit("topo")


def test_layers_follow_the_controls():
    layers = map_mod.build_layers("voyager", 1500, 10)
    assert [l["id"] for l in layers] == ["stations"]
    col = layers[0]
    assert col["radius"] == 1500.0
    assert col["elevationScale"] == 10 * map_mod.ELEVATION_UNIT_M
    assert len(col["data"]) == len(map_mod.STATIONS)

    # a raster basemap is drawn as a tile layer UNDER the stations
    layers = map_mod.build_layers("satellite", 1000, 1)
    assert [l["id"] for l in layers] == ["basemap_raster", "stations"]


def test_station_rows_are_lon_lat_and_coloured_by_value():
    rows = map_mod.station_rows()
    for row, st in zip(rows, map_mod.STATIONS):
        assert row["position"] == [st["lon"], st["lat"]]  # deck.gl order: lon, lat
        assert row["name"] == st["name"]
        assert len(row["color"]) == 4 and all(0 <= c <= 255 for c in row["color"])
    low, high = sorted(rows, key=lambda r: r["value"])
    assert high["color"][0] > low["color"][0]  # more red with a higher value
    assert high["color"][1] < low["color"][1]  # less green

