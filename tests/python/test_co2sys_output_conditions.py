"""Grep guard: CO2SYS output conditions must be the in-situ ones at every call site.

The pelagic and sediment models read CO2SYS's OUTPUT-condition columns (pH 18, HCO3/CO3/CO2 21-23, phosphate K's
75-77). Until 2026-11-21 every call site set TEMPOUT to 0 ("does not matter"), so the carbonate chemistry was
evaluated at 0 C (review finding SED-1). This test is a text check, not a behavioural one: it fails if any
assignment to a *TEMPOUT array outside the CO2SYS library sets it to a numeric constant. The behavioural check of
the pelagic site is the 0D golden (tests/regression/pelagic_0D_golden.csv).
"""
import pathlib
import re

ROOT = pathlib.Path(__file__).resolve().parents[2]
CO2SYS_LIBRARY = ROOT / "SOURCE_CODE" / "AQUABC" / "CO2SYS" / "aquabc_II_co2sys.f90"
ASSIGN = re.compile(r"^\s*(\w*TEMPOUT)\s*(\([^)]*\))?\s*=\s*([^!]+)", re.IGNORECASE)
NUMBER = re.compile(r"^[+-]?(\d+\.?\d*|\.\d+)([dDeE][+-]?\d+)?(_\w+)?$")


def constant_tempout_assignments(text):
    """(line number, line) for each *TEMPOUT assignment whose right-hand side is a numeric literal."""
    found = []
    for n, line in enumerate(text.splitlines(), start=1):
        m = ASSIGN.match(line)
        if m and NUMBER.match(m.group(3).strip()):
            found.append((n, line.strip()))
    return found


def test_guard_flags_a_constant_and_allows_a_temperature():
    old = "            CO2SYS_TEMPOUT      (1:nkn) = 0.0D0  !Does not matter for this case"
    new = "            CO2SYS_TEMPOUT      (1:nkn) = TEMP(1:nkn)"
    sed = "            ALL_LAYERS_CO2SYS_TEMPOUT      (:) = ALL_LAYERS_CO2SYS_TEMPIN(:)  ! in situ (SED-1)"
    assert [n for n, _ in constant_tempout_assignments(old)] == [1]
    assert constant_tempout_assignments(new + "\n" + sed) == []


def test_no_co2sys_call_site_sets_tempout_to_a_constant():
    offenders = []
    for path in sorted((ROOT / "SOURCE_CODE").rglob("*.f90")):
        if path == CO2SYS_LIBRARY:
            continue
        for n, line in constant_tempout_assignments(path.read_text(encoding="latin-1")):
            offenders.append(f"{path.relative_to(ROOT)}:{n}: {line}")
    assert not offenders, "CO2SYS output conditions set to a constant (use the in-situ temperature):\n" + \
        "\n".join(offenders)
