"""Fatal aborts in the Fortran sources must exit with a non-zero status.

A Fortran STOP with no code, or with a character code, ends the program with exit status 0. The
run then looks like a success to every caller that checks the status: the app reports "Model run
completed successfully" and calibrate_cl29.py scores the truncated record (review finding EST-1,
2026-11-21). Every abort therefore uses ERROR STOP (gfortran exits with status 1). A STOP with an
integer code keeps that code as the exit status, so the test programs' `stop 1` stays allowed.
"""
import pathlib
import re

ROOT = pathlib.Path(__file__).resolve().parents[2]
SOURCES = ROOT / "SOURCE_CODE"


def _code_only(line):
    """The line with string literals blanked and any trailing comment removed."""
    out = []
    quote = None
    for ch in line:
        if quote:
            if ch == quote:
                quote = None
                out.append(ch)
            else:
                out.append(" ")
        elif ch in "'\"":
            quote = ch
            out.append(ch)
        elif ch == "!":
            break
        else:
            out.append(ch)
    return "".join(out)


def plain_stops(text):
    """(line number, line) for every STOP statement that would exit with status 0."""
    found = []
    for n, line in enumerate(text.splitlines(), start=1):
        code = _code_only(line)
        for m in re.finditer(r"\bstop\b", code, re.IGNORECASE):
            before = code[:m.start()].rstrip()
            if re.search(r"\berror$", before, re.IGNORECASE):
                continue                                   # ERROR STOP
            if before and not re.search(r"(\)|;|^\s*\d+)$", before):
                continue                                   # not in statement position
            if re.fullmatch(r"\s*\d+\s*(;.*)?", code[m.end():]):
                continue                                   # STOP <integer>: keeps the code
            found.append((n, line.strip()))
    return found


def test_scanner_flags_the_forms_that_exit_zero():
    src = "\n".join([
        "      stop",
        "      STOP",
        "      if (error .eq. 1) stop",
        "      stop 'error stop name does not exist'",
        '      stop "error stop"',
        "99    stop",
    ])
    assert [n for n, _ in plain_stops(src)] == [1, 2, 3, 4, 5, 6]


def test_scanner_allows_error_stop_integer_codes_and_text():
    src = "\n".join([
        "      error stop",
        "      error stop 'READ_MODEL_CONSTANTS: missing constant'",
        "      if (rc /= 0) error stop",
        "      stop 1",
        "      stop 0",
        "      write(*,*) 'the run will stop here'",
        "      ! stop",
        "      x = 1 ! stop",
        "      call stop_model(stop_flag)",
    ])
    assert plain_stops(src) == []


def test_no_fortran_abort_exits_with_status_zero():
    offenders = []
    for path in sorted(SOURCES.rglob("*.f90")):
        text = path.read_text(encoding="utf-8", errors="replace")
        for n, line in plain_stops(text):
            offenders.append(f"{path.relative_to(ROOT)}:{n}: {line}")
    assert not offenders, "STOP without an integer code exits with status 0; use ERROR STOP:\n" + \
        "\n".join(offenders)
