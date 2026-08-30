#!/usr/bin/env python3
"""Pre-build admissibility gate for the CYN nitrogen-quota (Droop) mechanism.

Solves the steady-state quota identity from spec sec 2
(docs/superpowers/specs/2026-08-30-cyn-droop-n-rescoped-design.md sec 2):

    VMAX * M * (QMAX - Q*) = KG * f_temp * f_light * Q* * (Q* - QMIN)
    M = DIN / (KHS_UPT + DIN)
    LIM_N* = (Q* - QMIN) / (QMAX - QMIN)

for the steady-state quota Q* in (QMIN, QMAX) by bisection. The left side
(uptake) is decreasing in Q*; the right side (growth dilution) is increasing
in Q*; their difference is therefore strictly decreasing, so [QMIN, QMAX] is
a monotone bracket containing exactly one root.

Two legs share the model parameters --kg/--khs/--qmin/--qmax/--vmax:

  August (from the CLI --din/--ftemp/--flight): admissible only if
  LIM_N* > LIM_N_THRESHOLD, the Monod baseline the flag replaces. Before
  bisecting at the real DIN, the tool checks the M=1 upper bound (DIN much
  greater than KHS_UPT, the best case for uptake) -- if even that best case
  cannot clear the threshold, VMAX predetermines a NULL regardless of DIN,
  and the tool fails fast without ever bisecting at the real M.

  June (fixed scenario constants below, spec sec 2: "June Q* (M ~= 0.88)"):
  admissible only if Q* >= QMIN + JUNE_Q_FRACTION * (QMAX - QMIN) -- storage
  must approach QMAX, else spec sec 7[c]'s storage-refutation criterion
  fires by construction and the NULL is predetermined.

Exit 0 iff both legs pass; exit 1 otherwise (including the M=1 short-circuit).
"""
from __future__ import annotations

import argparse
import sys

LIM_N_THRESHOLD = 0.571    # Monod baseline the flag replaces (spec sec 2)

JUNE_DIN = 0.022           # mg N/L, fixed June scenario (spec sec 2)
JUNE_FTEMP = 0.63
JUNE_FLIGHT = 0.33
JUNE_Q_FRACTION = 0.8      # June Q* must reach QMIN + 0.8*(QMAX-QMIN)


def monod(din, khs):
    """M = DIN / (KHS_UPT + DIN)."""
    return din / (khs + din)


def solve_q_star(vmax, m, mu_max_eff, qmin, qmax, tol=1e-12, maxiter=200):
    """Bisect VMAX*M*(QMAX-Q) - mu_max_eff*Q*(Q-QMIN) = 0 for Q in [QMIN, QMAX].

    f(QMIN) = VMAX*M*(QMAX-QMIN) >= 0; f(QMAX) = -mu_max_eff*QMAX*(QMAX-QMIN) <= 0;
    f is strictly decreasing over the interior -- the bracket is monotone.
    """
    def f(q):
        return vmax * m * (qmax - q) - mu_max_eff * q * (q - qmin)

    lo, hi = qmin, qmax
    if f(lo) <= 0:
        return lo
    if f(hi) >= 0:
        return hi
    for _ in range(maxiter):
        mid = 0.5 * (lo + hi)
        fmid = f(mid)
        if abs(fmid) < tol or (hi - lo) < tol:
            return mid
        if fmid > 0:
            lo = mid
        else:
            hi = mid
    return 0.5 * (lo + hi)


def lim_n(q_star, qmin, qmax):
    """LIM_N* = (Q* - QMIN) / (QMAX - QMIN)."""
    return (q_star - qmin) / (qmax - qmin)


def main(argv=None):
    p = argparse.ArgumentParser(
        description="Pre-build admissibility gate for the CYN Droop quota (spec sec 2)."
    )
    p.add_argument("--kg", type=float, required=True, help="KG_CYN, max growth rate (1/d)")
    p.add_argument("--ftemp", type=float, required=True, help="August f_temp")
    p.add_argument("--flight", type=float, required=True, help="August f_light")
    p.add_argument("--din", type=float, required=True, help="August DIN (mg N/L)")
    p.add_argument("--khs", type=float, required=True, help="CYN_N_KHS_UPT (mg N/L)")
    p.add_argument("--qmin", type=float, required=True, help="CYN_N_QMIN (gN/gC)")
    p.add_argument("--qmax", type=float, required=True, help="CYN_N_QMAX (gN/gC)")
    p.add_argument("--vmax", type=float, required=True, help="CYN_N_VMAX (gN/gC/d)")
    a = p.parse_args(argv)

    mu_aug = a.kg * a.ftemp * a.flight

    # M=1 short-circuit: DIN >> KHS_UPT is the best case for uptake. If even
    # this upper bound cannot clear the threshold, VMAX predetermines a NULL.
    q_upper = solve_q_star(a.vmax, 1.0, mu_aug, a.qmin, a.qmax)
    lim_upper = lim_n(q_upper, a.qmin, a.qmax)
    if lim_upper <= LIM_N_THRESHOLD:
        print(
            f"M=1 short-circuit: even at DIN >> KHS_UPT (M=1), Q*={q_upper:.6f} gives "
            f"LIM_N*={lim_upper:.6f} <= threshold {LIM_N_THRESHOLD} -- "
            f"VMAX={a.vmax} predetermines a NULL, no August DIN can rescue it. FAIL."
        )
        return 1

    m_aug = monod(a.din, a.khs)
    q_aug = solve_q_star(a.vmax, m_aug, mu_aug, a.qmin, a.qmax)
    lim_aug = lim_n(q_aug, a.qmin, a.qmax)
    august_pass = lim_aug > LIM_N_THRESHOLD
    print(
        f"August: M={m_aug:.6f} Q*={q_aug:.6f} LIM_N*={lim_aug:.6f} "
        f"threshold={LIM_N_THRESHOLD} -> {'PASS' if august_pass else 'FAIL'}"
    )

    mu_jun = a.kg * JUNE_FTEMP * JUNE_FLIGHT
    m_jun = monod(JUNE_DIN, a.khs)
    q_jun = solve_q_star(a.vmax, m_jun, mu_jun, a.qmin, a.qmax)
    q_jun_threshold = a.qmin + JUNE_Q_FRACTION * (a.qmax - a.qmin)
    june_pass = q_jun >= q_jun_threshold
    print(
        f"June: M={m_jun:.6f} Q*={q_jun:.6f} threshold={q_jun_threshold:.6f} "
        f"({JUNE_Q_FRACTION:.0%} of band) -> {'PASS' if june_pass else 'FAIL'}"
    )

    if august_pass and june_pass:
        print("GATE: PASS")
        return 0
    print("GATE: FAIL")
    return 1


if __name__ == "__main__":
    sys.exit(main())
