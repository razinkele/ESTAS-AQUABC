import sys
from pathlib import Path

TOOLS = Path(__file__).resolve().parents[2] / "shiny_app"
sys.path.insert(0, str(TOOLS))
import setups as s  # noqa: E402


def test_two_entries_with_expected_fields():
    ids = [x.id for x in s.list_setups()]
    assert ids == ["standard", "cl29"]
    std, cl29 = s.get_setup("standard"), s.get_setup("cl29")
    assert (std.input_file, std.inputs_dir, std.output_dir, std.box_count, std.env) == \
           ("INPUT.txt", "INPUTS", "OUTPUTS", 25, {})
    assert (cl29.input_file, cl29.inputs_dir, cl29.output_dir, cl29.box_count) == \
           ("INPUT_CL29.txt", "INPUTS_CL29", "OUTPUTS_CL29", 29)
    assert cl29.env == {"ESTAS_HOLD_VOLUME": "1"}


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
    repo = str(Path(__file__).resolve().parents[2])
    std = s.input_files_for(s.get_setup("standard"), repo)
    assert "INPUT.txt" in std and "INPUT_30day.txt" in std   # real Standard configs stay visible
    assert "INPUT_CL29.txt" not in std                        # CL29 config excluded from Standard
    cl29 = s.input_files_for(s.get_setup("cl29"), repo)
    assert "INPUT_CL29.txt" in cl29
