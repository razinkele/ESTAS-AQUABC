"""Converter -> reader round-trip for the two-type (sandy/muddy) sediment inputs.

Guards the top implementation risk of the Phase-2a reader extension: a positional
desync between the converter's multi-type author (_author_multitype_sediment) and the
Fortran reader's fixed skip-counts (mod_BOTTOM_SEDIMENTS: 3/3/3/3/3/4 pre-data records
per profile field, plus the global ADVECTIVE_VELOCITY/SURF_MIXLEN relocated to the tail).

_parse_multitype_sediment below re-implements the reader's exact record sequence in
Python: every read(IN_FILE,*) is one consumed line, counted by position, never by '#'
content -- identical to the Fortran. So if the emitter writes the wrong number of
records, this parser lands on the wrong line and the assertions fail. The live Fortran
reader has independently been confirmed to assign the same per-box ICs from this author
(a 60-day ESTAS_II run: muddy boxes 2/3/19 carry the muddy IC overrides, sandy boxes the
template base), so the Python mirror and the real reader agree on the layout.
"""
import importlib.util
import os

_PATH = os.path.join(os.getcwd(), "tools", "eutropy_poc", "eutropy_to_estas.py")
_SPEC = importlib.util.spec_from_file_location("eutropy_to_estas_rt", _PATH)
conv = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(conv)

NUM_SED_VARS = 24


def _parse_multitype_sediment(path, nkn):
    """Parse an extended 2-type sediment file exactly the way the Fortran reader does
    (positional skip-counts). Returns a dict of the per-type buffers + per-box map."""
    with open(path, newline="") as fh:
        raw = fh.readlines()
    pos = 0

    def skip(n=1):
        nonlocal pos
        pos += n

    def value():
        """Consume one record, return its whitespace-split tokens (comments after the
        needed values are ignored by list-directed reads, so callers slice what they need)."""
        nonlocal pos
        toks = raw[pos].split()
        pos += 1
        return toks

    skip(5)                                   # 5 description lines
    skip(1)
    value()                          # # ADVANCED REDOX + flag
    skip(1)
    num_layers = int(value()[0])     # # NUM_SED_LAYERS + count

    # Detection slot: the extended file has the header here.
    assert "NUM_SED_TYPES" in raw[pos], "missing # NUM_SED_TYPES header"
    skip(1)
    num_types = int(value()[0])

    skip(1)                                    # # SED_TYPE_PER_BOX header
    type_per_box = [int(value()[0]) for _ in range(nkn)]

    type_depths, type_poros, type_dens = [], [], []
    type_mixing, type_burial, type_ic = [], [], []
    for _t in range(num_types):
        skip(3)
        type_depths.append([float(value()[0]) for _ in range(num_layers)])
        skip(3)
        type_poros.append([float(value()[0]) for _ in range(num_layers)])
        skip(3)
        type_dens.append([float(value()[0]) for _ in range(num_layers)])
        skip(3)
        type_mixing.append(float(value()[0]))
        skip(3)
        type_burial.append(float(value()[0]))
        skip(4)                                # 4 IC skip records (incl. column header)
        ic = [[float(x) for x in value()[:num_layers]] for _ in range(NUM_SED_VARS)]
        type_ic.append(ic)

    skip(3)
    adv_vel = float(value()[0])       # GLOBAL advective velocity
    skip(3)
    surf_mixlen = float(value()[0])   # GLOBAL surface mixing length

    skip(4)
    const_file = value()[0]           # constants section (4 skip + filename)
    return dict(num_types=num_types, num_layers=num_layers, type_per_box=type_per_box,
                type_depths=type_depths, type_poros=type_poros, type_dens=type_dens,
                type_mixing=type_mixing, type_burial=type_burial, type_ic=type_ic,
                adv_vel=adv_vel, surf_mixlen=surf_mixlen, const_file=const_file)


def _author(tmp_path, monkeypatch):
    """Author a 2-type file with DISTINCT sandy/muddy geometry + IC so per-box mapping and
    the IC transpose are observable, then parse it back. Returns (parsed, type_map)."""
    sandy = {"depths": [0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07],
             "porosities": [0.30, 0.31, 0.32, 0.33, 0.34, 0.35, 0.36],
             "densities": [2.60, 2.60, 2.60, 2.60, 2.60, 2.60, 2.60],
             "burial": 1.0e-4, "mixing": 1.0e-3,
             "ic_overrides": {4: [100.0] * 7, 7: [11.0] * 7}}
    muddy = {"depths": [0.005, 0.010, 0.020, 0.030, 0.050, 0.070, 0.100],
             "porosities": [0.80, 0.81, 0.82, 0.83, 0.84, 0.85, 0.86],
             "densities": [1.20, 1.20, 1.20, 1.20, 1.20, 1.20, 1.20],
             "burial": 5.0e-4, "mixing": 5.0e-2,
             "ic_overrides": {4: [2000.0] * 7, 7: [260.0] * 7}}
    monkeypatch.setattr(conv, "CL29_SED_SANDY", sandy)
    monkeypatch.setattr(conv, "CL29_SED_MUDDY", muddy)
    # Box 19 muddy per spec; a mix so the map is non-trivial. A box absent -> default sandy.
    type_map = {b: ("muddy" if b in (2, 3, 19) else "sandy") for b in range(1, conv.NBOX + 1)}
    conv._write_sediment_inputs(str(tmp_path), True, type_map)
    parsed = _parse_multitype_sediment(
        str(tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt"), conv.NBOX)
    return parsed, sandy, muddy, type_map


class TestTwoTypeRoundTrip:
    def test_header_and_box_map(self, tmp_path, monkeypatch):
        parsed, _s, _m, type_map = _author(tmp_path, monkeypatch)
        assert parsed["num_types"] == 2
        expect = [conv._SED_TYPE_TO_INDEX[type_map[b]] for b in range(1, conv.NBOX + 1)]
        assert parsed["type_per_box"] == expect
        assert parsed["type_per_box"][19 - 1] == 2      # box 19 muddy
        assert parsed["const_file"] == "W_SED_CONST.txt"

    def test_per_type_profiles_survive(self, tmp_path, monkeypatch):
        parsed, sandy, muddy, _map = _author(tmp_path, monkeypatch)
        for t, prof in ((0, sandy), (1, muddy)):
            assert parsed["type_depths"][t] == prof["depths"]
            assert parsed["type_poros"][t] == prof["porosities"]
            assert parsed["type_dens"][t] == prof["densities"]
            assert parsed["type_mixing"][t] == prof["mixing"]
            assert parsed["type_burial"][t] == prof["burial"]

    def test_per_box_assignment_and_ic_transpose(self, tmp_path, monkeypatch):
        parsed, sandy, muddy, type_map = _author(tmp_path, monkeypatch)
        profs = [sandy, muddy]
        nlay = parsed["num_layers"]
        for box in range(1, conv.NBOX + 1):
            t = parsed["type_per_box"][box - 1]      # 1-based type index
            prof = profs[t - 1]
            # geometry maps axis-for-axis
            assert parsed["type_depths"][t - 1] == prof["depths"]
            # IC: file stores var-major/layer-minor; the reader transposes into
            # init_sed_state_vars(box,layer,var). Check the transpose is well-defined:
            # the bumped muddy vars (SED_PON=var4, SED_POP=var7) sit at the right rows.
            ic = parsed["type_ic"][t - 1]
            if type_map[box] == "muddy":
                assert all(ic[4 - 1][layer] == 2000.0 for layer in range(nlay))
                assert all(ic[7 - 1][layer] == 260.0 for layer in range(nlay))
            else:
                assert all(ic[4 - 1][layer] == 100.0 for layer in range(nlay))
                assert all(ic[7 - 1][layer] == 11.0 for layer in range(nlay))

    def test_empty_map_stays_single_profile(self, tmp_path):
        """An empty map must NOT emit the two-type layout (byte-identical Phase-1 path)."""
        conv._write_sediment_inputs(str(tmp_path), True, {})
        text = (tmp_path / "BOTTOM_SEDIMENT_MODEL_INPUT.txt").read_text()
        assert "# NUM_SED_TYPES" not in text
        assert "# SED_TYPE_PER_BOX" not in text
