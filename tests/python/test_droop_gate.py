"""Tests for tools/droop_gate.py -- the CYN Droop pre-build admissibility gate.

Identity under test (spec sec 2, docs/superpowers/specs/
2026-08-30-cyn-droop-n-rescoped-design.md): the steady-state quota solves
    VMAX*M*(QMAX-Q*) = KG*ftemp*flight*Q**(Q*-QMIN),  M = DIN/(KHS_UPT+DIN)
    LIM_N* = (Q*-QMIN)/(QMAX-QMIN)
Admissible iff August LIM_N* > 0.571 (the Monod baseline the flag replaces)
AND June Q* >= QMIN + 0.8*(QMAX-QMIN) (storage approaches QMAX).

The reference values below are computed independently of tools/droop_gate.py
(closed-form quadratic root of the same identity, not the tool's bisection)
from the same committed fractions -- never a hardcoded rounded decimal.
"""
import math
import re
import subprocess
import sys
from pathlib import Path

TOOL = Path(__file__).resolve().parents[2] / "tools" / "droop_gate.py"

# Committed constants (spec sec 2 "Committed constants" + sec 5 gate check)
KG = 2.0
FTEMP_AUG = 0.78
FLIGHT_AUG = 0.30
DIN_AUG = 0.004
KHS = 0.003
QMIN = 0.10
QMAX = 0.25
VMAX = 0.44

# Fixed June scenario (spec sec 2: "June Q* (M ~= 0.88)")
JUNE_DIN = 0.022
JUNE_FTEMP = 0.63
JUNE_FLIGHT = 0.33
JUNE_Q_FRACTION = 0.8

LIM_N_THRESHOLD = 0.571


def _q_star_reference(vmax, din, khs, ftemp, flight, kg, qmin, qmax):
    """Independent reference: closed-form root of the SAME quadratic identity
    droop_gate.py solves by bisection -- VMAX*M*(QMAX-Q) = mu*Q*(Q-QMIN) is
    quadratic in Q, so this is an algebraically distinct check on the tool."""
    m = din / (khs + din)
    mu = kg * ftemp * flight
    a = mu
    b = vmax * m - mu * qmin
    c = -vmax * m * qmax
    return (-b + math.sqrt(b * b - 4 * a * c)) / (2 * a)


def _run(args):
    return subprocess.run(
        [sys.executable, str(TOOL)] + args, capture_output=True, text=True
    )


def _committed_args(vmax=VMAX):
    return [
        "--kg", str(KG), "--ftemp", str(FTEMP_AUG), "--flight", str(FLIGHT_AUG),
        "--din", str(DIN_AUG), "--khs", str(KHS), "--qmin", str(QMIN),
        "--qmax", str(QMAX), "--vmax", str(vmax),
    ]


def test_committed_constants_pass_both_legs():
    r = _run(_committed_args())
    assert r.returncode == 0, f"stdout={r.stdout!r} stderr={r.stderr!r}"

    q_aug_ref = _q_star_reference(VMAX, DIN_AUG, KHS, FTEMP_AUG, FLIGHT_AUG, KG, QMIN, QMAX)
    lim_aug_ref = (q_aug_ref - QMIN) / (QMAX - QMIN)
    assert 0.70 < lim_aug_ref < 0.75
    assert lim_aug_ref > LIM_N_THRESHOLD

    q_jun_ref = _q_star_reference(VMAX, JUNE_DIN, KHS, JUNE_FTEMP, JUNE_FLIGHT, KG, QMIN, QMAX)
    june_threshold_ref = QMIN + JUNE_Q_FRACTION * (QMAX - QMIN)
    assert q_jun_ref >= june_threshold_ref

    # the tool's own printed Q*/LIM_N* agree with the independently-derived reference
    m_aug = re.search(r"August:.*?Q\*=([-\d.]+)\s+LIM_N\*=([-\d.]+)", r.stdout)
    assert m_aug, r.stdout
    assert abs(float(m_aug.group(1)) - q_aug_ref) < 1e-4
    assert abs(float(m_aug.group(2)) - lim_aug_ref) < 1e-4

    m_jun = re.search(r"June:.*?Q\*=([-\d.]+)", r.stdout)
    assert m_jun, r.stdout
    assert abs(float(m_jun.group(1)) - q_jun_ref) < 1e-4

    assert "PASS" in re.search(r"August:.*", r.stdout).group(0)
    assert "PASS" in re.search(r"June:.*", r.stdout).group(0)


def test_vmax_predetermined_null_fails_with_short_circuit_message():
    # VMAX=0.06 fails LIM_N* even at the M=1 upper bound (best-case DIN) --
    # this must be caught before the tool ever bisects at the real August M.
    r = _run(_committed_args(vmax=0.06))
    assert r.returncode == 1, f"stdout={r.stdout!r} stderr={r.stderr!r}"
    assert "M=1" in r.stdout
    assert "short-circuit" in r.stdout.lower()
    assert "0.06" in r.stdout


def test_help_exits_zero():
    r = _run(["--help"])
    assert r.returncode == 0
    assert "usage" in r.stdout.lower()
