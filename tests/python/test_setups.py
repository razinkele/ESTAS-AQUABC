import sys
from pathlib import Path

TOOLS = Path(__file__).resolve().parents[2] / "shiny_app"
sys.path.insert(0, str(TOOLS))
import setups as s  # noqa: E402


def test_three_entries_with_expected_fields():
    ids = [x.id for x in s.list_setups()]
    assert ids == ["standard", "cl29", "cl29_2023clim"]
    std, cl29, clim = (s.get_setup("standard"), s.get_setup("cl29"),
                       s.get_setup("cl29_2023clim"))
    assert (std.input_file, std.inputs_dir, std.output_dir, std.box_count, std.env) == \
           ("INPUT.txt", "INPUTS", "OUTPUTS", 25, {})
    assert (cl29.input_file, cl29.inputs_dir, cl29.output_dir, cl29.box_count) == \
           ("INPUT_CL29.txt", "INPUTS_CL29", "OUTPUTS_CL29", 29)
    assert cl29.env == {"ESTAS_HOLD_VOLUME": "1"}
    assert (clim.input_file, clim.inputs_dir, clim.output_dir, clim.box_count) == \
           ("INPUT_CL29_2023clim.txt", "INPUTS_CL29_2023clim", "OUTPUTS_CL29_2023clim", 29)
    assert clim.env == {"ESTAS_HOLD_VOLUME": "1"}


def test_unknown_id_falls_back_to_default():
    assert s.get_setup("nope").id == "standard"
    assert s.default_setup().id == "standard"


def test_is_available_requires_the_sentinel_file(tmp_path):
    root = tmp_path
    (root / "INPUTS").mkdir()
    assert s.is_available(s.get_setup("standard"), str(root)) is False   # no PELAGIC_INPUTS.txt
    (root / "INPUTS" / "WCONST.txt").write_text("x")                     # decoy: non-empty, still unavailable
    assert s.is_available(s.get_setup("standard"), str(root)) is False   # kills a non-emptiness impl
    (root / "INPUTS" / "PELAGIC_INPUTS.txt").write_text("x")
    assert s.is_available(s.get_setup("standard"), str(root)) is True
    assert s.is_available(s.get_setup("cl29"), str(root)) is False        # no INPUTS_CL29/


def test_input_files_for_matches_real_comment_format(tmp_path):
    root = tmp_path
    hdr = '# PELAGIC MODEL INPUT FOLDER write the folder always "/" in the end\n'   # real trailing text
    (root / "INPUT.txt").write_text(hdr + "INPUTS/\n")
    (root / "INPUT_CL29.txt").write_text(hdr + "INPUTS_CL29/\n")
    (root / "INPUT_30day.txt").write_text(hdr + "INPUTS/\n")
    std = s.input_files_for(s.get_setup("standard"), str(root))
    assert set(std) == {"INPUT.txt", "INPUT_30day.txt"}
    assert s.input_files_for(s.get_setup("cl29"), str(root)) == ["INPUT_CL29.txt"]


def test_input_files_for_against_real_repo():
    # Only git-COMMITTED root configs — INPUT_30day.txt etc. are local-only and
    # absent in a clean CI checkout, so assert against ones that are committed.
    repo = str(Path(__file__).resolve().parents[2])
    std = s.input_files_for(s.get_setup("standard"), repo)
    assert "INPUT.txt" in std and "INPUT_200day.txt" in std   # committed Standard configs stay visible
    assert "INPUT_CL29.txt" not in std                         # CL29 configs excluded from Standard
    assert "INPUT_CL29_2023clim.txt" not in std
    assert "INPUT_CL29.txt" in s.input_files_for(s.get_setup("cl29"), repo)
    clim = s.input_files_for(s.get_setup("cl29_2023clim"), repo)
    assert "INPUT_CL29_2023clim.txt" in clim and "INPUT_CL29.txt" not in clim
