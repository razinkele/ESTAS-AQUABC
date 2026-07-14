import plotly.graph_objects as go

from shiny_app.box_network import (
    build_bathymetry_figure,
    build_box_network_figure,
    build_depths_overview,
    parse_advective_links,
    parse_bathymetry,
    parse_pelagic_inputs,
)


def _write(p, text):
    p.write_text(text)


def test_parse_pelagic_inputs(tmp_path):
    _write(tmp_path / "PELAGIC_INPUTS.txt",
           "# header\n# INITIAL CONDITIONS\n1 1 0.0 -5.0\n2 2 1.0 -2.0\n# MASS LOADS\n9 9 0 0\n")
    boxes = parse_pelagic_inputs(str(tmp_path))
    assert set(boxes) == {1, 2}
    assert boxes[1] == {"ic_set": 1, "sediment": "Mud", "surface_elevation": 0.0,
                        "bottom_elevation": -5.0, "depth": 5.0}
    assert boxes[2]["sediment"] == "Sand" and boxes[2]["depth"] == 3.0


def test_parse_pelagic_inputs_missing(tmp_path):
    assert parse_pelagic_inputs(str(tmp_path)) == {}


def test_parse_pelagic_inputs_skips_malformed(tmp_path):
    _write(tmp_path / "PELAGIC_INPUTS.txt",
           "# INITIAL CONDITIONS\nBAD ROW HERE X\n3 1 0.0 -4.0\n# MASS LOADS\n")
    boxes = parse_pelagic_inputs(str(tmp_path))
    assert set(boxes) == {3}          # 'BAD ROW HERE X' has 4 parts -> int('BAD') raises -> except path -> skipped


def test_parse_advective_links(tmp_path):
    _write(tmp_path / "ADVECTIVE_LINKS.txt", "# links\n0 1 2\n1 2 3\n\n")
    assert parse_advective_links(str(tmp_path)) == [(1, 2), (2, 3)]


def test_parse_advective_links_missing(tmp_path):
    assert parse_advective_links(str(tmp_path)) == []


def test_parse_bathymetry(tmp_path):
    _write(tmp_path / "BATHYMETRY_5.txt",
           "BATHYMETRY BOX 5\nNUM_LAYERS\n2\n"
           "layer up_el low_el up_a low_a up_l low_l\n"
           "1 0.0 -1.0 100.0 90.0 10.0 9.0\n2 -1.0 -2.0 90.0 80.0 9.0 8.0\n")
    layers = parse_bathymetry(5, str(tmp_path))
    assert len(layers) == 2
    assert layers[0] == {"layer_no": 1, "upper_elevation": 0.0, "lower_elevation": -1.0,
                         "upper_area": 100.0, "lower_area": 90.0, "upper_length": 10.0,
                         "lower_length": 9.0}


def test_parse_bathymetry_missing(tmp_path):
    assert parse_bathymetry(99, str(tmp_path)) == []


# --- figure smoke tests: build without raising, return a Figure with >=1 trace ---
_BOXES = {1: {"ic_set": 1, "sediment": "Mud", "surface_elevation": 0.0,
              "bottom_elevation": -5.0, "depth": 5.0},
          2: {"ic_set": 2, "sediment": "Sand", "surface_elevation": 1.0,
              "bottom_elevation": -2.0, "depth": 3.0}}
_LINKS = [(1, 2)]
_LAYERS = [{"layer_no": 1, "upper_elevation": 0.0, "lower_elevation": -1.0,
            "upper_area": 100.0, "lower_area": 90.0, "upper_length": 10.0, "lower_length": 9.0}]


def test_build_box_network_figure_smoke():
    fig = build_box_network_figure(_BOXES, _LINKS)
    assert isinstance(fig, go.Figure) and len(fig.data) >= 1


def test_build_bathymetry_figure_smoke():
    fig = build_bathymetry_figure(1, _LAYERS, _BOXES)
    assert isinstance(fig, go.Figure) and len(fig.data) >= 1


def test_build_depths_overview_smoke():
    fig = build_depths_overview(_BOXES)
    assert isinstance(fig, go.Figure) and len(fig.data) >= 1
