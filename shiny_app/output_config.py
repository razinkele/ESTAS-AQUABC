"""PELAGIC_OUTPUT_INFORMATION_FILE: which boxes write which outputs.

The file has one header line and one row per box:

    box   PRODUCE_PEL_STATE_VAR_OUTPUTS   PRODUCE_PEL_PROCESS_RATE_OUTPUTS   PRODUCE_PEL_MASS_BALANCE_OUTPUTS

The Output Config tab shows it as a set of boxes and one set of output types, which
is less than the file can hold (each box has its own three flags). These helpers keep
the round trip honest: loading then saving without a change reproduces the file, and
a box keeps its own flags unless the user changes the output types.
"""
import os
import shutil

TYPES = ("state_vars", "process_rates", "mass_balance")
HEADER = ("#     PELAGIC BOX NO      PRODUCE_PEL_STATE_VAR_OUTPUTS     "
          "PRODUCE_PEL_PROCESS_RATE_OUTPUTS     PRODUCE_PEL_MASS_BALANCE_OUTPUTS\n")


def parse(text: str) -> tuple[str, dict[int, tuple[int, int, int]]]:
    """Return (header line, {box: (state, rates, mass_balance)}) of the file text."""
    lines = text.splitlines(keepends=True)
    header = lines[0] if lines and lines[0].lstrip().startswith("#") else HEADER
    flags = {}
    for line in lines:
        parts = line.split()
        if len(parts) >= 4 and parts[0].isdigit():
            try:
                flags[int(parts[0])] = tuple(1 if int(p) > 0 else 0 for p in parts[1:4])
            except ValueError:
                continue
    return header, flags


def summarize(flags: dict) -> tuple[list[str], list[str], bool]:
    """What the tab can show: (selected boxes, output types, mixed).

    A box is selected when any of its flags is on; the types are the union over the
    selected boxes. ``mixed`` is True when the selected boxes do not all have the
    same flags, i.e. the file holds more than the two widgets can show.
    """
    on = {b: f for b, f in flags.items() if any(f)}
    types = [t for i, t in enumerate(TYPES) if any(f[i] for f in on.values())]
    return [str(b) for b in sorted(on)], types, len(set(on.values())) > 1


def apply_selection(flags: dict, boxes, types, n_boxes: int, loaded_types=None) -> dict:
    """Flags for boxes 1..n_boxes after the user's selection.

    Unselected boxes get 0 0 0. A selected box that already had output keeps its own
    flags while the output types are as they were loaded (``loaded_types``), so a
    file with per-box differences survives a change of the box list. Once the user
    changes the types, every selected box gets the new set.
    """
    chosen = {int(b) for b in boxes if str(b).isdigit()}
    new = tuple(1 if t in set(types) else 0 for t in TYPES)
    keep_own = loaded_types is not None and set(types) == set(loaded_types)
    out = {}
    for box in range(1, n_boxes + 1):
        old = flags.get(box, (0, 0, 0))
        if box not in chosen:
            out[box] = (0, 0, 0)
        elif keep_own and any(old):
            out[box] = old
        else:
            out[box] = new
    return out


def _row(box: int, f) -> str:
    """One row in the layout of the tracked Standard file (20/37/37/37-wide columns)."""
    return f"{box:20d}{f[0]:>37d}{f[1]:>37d}{f[2]:>37d}\n"


def rewrite(text: str, flags: dict) -> str:
    """File text with the rows of ``flags`` applied.

    Rows whose flags are unchanged are kept exactly as they are (the model reads the
    file free-format, so setups differ in column widths and comments); changed rows
    are re-written, rows for boxes the file lacks are appended, and rows for boxes
    not in ``flags`` are dropped. An unchanged selection reproduces the file.
    """
    header, old = parse(text)
    out, seen = [], set()
    for line in text.splitlines(keepends=True):
        parts = line.split()
        box = int(parts[0]) if len(parts) >= 4 and parts[0].isdigit() and int(parts[0]) in old else None
        if box is None:
            out.append(line)
        elif box in flags and box not in seen:
            seen.add(box)
            out.append(line if old[box] == tuple(flags[box]) else _row(box, flags[box]))
    if not out:
        out.append(header)
    if not out[-1].endswith("\n"):
        out[-1] += "\n"
    out.extend(_row(box, flags[box]) for box in sorted(flags) if box not in seen)
    return "".join(out)


def save(path: str, text: str) -> str:
    """Overwrite an existing file, keeping the previous content as ``<path>.bak``.

    Refuses to create the file: the model reads the name given in its pelagic input
    file, so a file written where none existed would be one the model never opens.
    """
    if not os.path.isfile(path):
        raise FileNotFoundError(f"{os.path.basename(path)} not found in {os.path.dirname(path)}")
    backup = path + ".bak"
    shutil.copy2(path, backup)
    with open(path, "w") as f:
        f.write(text)
    return backup
