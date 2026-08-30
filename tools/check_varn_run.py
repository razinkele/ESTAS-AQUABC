#!/usr/bin/env python3
"""Check an ESTAS_II_varN run (CYN nitrogen-quota / Droop mechanism, VARN build).

Three independent modes (``--mode``):

``smoke``
    Given a run log + outputs dir + the options file used for that run, assert:
    (1) the run's startup echo of the five VARN options matches the options file;
    (2) the CYN_N column is present in the ``.out`` state-variable outputs;
    (3) the transport-flag echo (``INIT_TRANSPORT_FIELDS``,
    ``mod_PELAGIC_ECOLOGY.f90:759``, ``write(*,*) ADVECTION_ON(:)``) shows CYN_N
    (slot 33) and the four allelopathy metabolites (slots 34:37) transported;
    (4) the cell quota Q = CYN_N/CYN_C sits in [0.095, 0.255] for every sample,
    EXCLUDING samples at the MIN_CONCENTRATION floor (a numerical, not
    biological, artifact) and samples in caller-declared high-flush boxes
    (Q is a ratio of two independently-transported quantities and is not
    itself conserved under mixing -- those are reported, never asserted).

``conserve``
    On the degenerate-CYN scenario (``make_varn_inputs.py --degenerate-cyn``,
    run with denitrification and gas exchange disabled -- see that tool's
    ``DEGENERATE_ZERO_WCONST_NAMES``), assert that internal biogeochemical
    KINETICS (as reported per (box, time, state-var) by ``MASS_BALANCES.out``)
    neither creates nor destroys nitrogen: at every (box, time) sample, the sum
    of the KINETICS terms across NH4_N + NO3_N + DISS_ORG_N + DET_PART_ORG_N +
    CYN_N is (near) zero. This is deliberately NOT a whole-domain mass-
    trajectory closure -- CL29's net advective flushing exports the majority
    of the water-column N inventory through the open boundaries within weeks
    regardless of boundary concentration, so Delta(state) alone never closes
    at the domain level (measured: -85% over 30 days). The identity here
    isolates biology from transport by construction (MASS_BALANCES.out's
    ADVECTION/DIFFUSION/SETTLING/SEDIMENT-FLUX columns are NOT part of the
    sum), which is the scientifically meaningful thing the Droop mechanism's
    N-conservation claim is actually about.

``nbudget``
    Per-term CYN nitrogen budget for one box: uptake and each Q-weighted loss
    (respiration, death, excretion -- the DON sink, zooplankton grazing), read
    from that box's ``_PROCESS_RATES.out`` file when
    ``PROCESS_RATE_OUT=1`` was set for it (verified byte-exact against
    MASS_BALANCES.out's KINETICS column for CYN_N in this task's investigation);
    otherwise falls back to a labeled net (state-difference) estimate that
    includes transport and is NOT a kinetics-only decomposition. Always prints
    a NOTE that the Fortran-side derived TN (mod_PELAGIC_ECOLOGY.f90:369) omits
    CYN_N under CYN_VARIABLE_N=1 -- TN comparisons must use
    tools/validate_cl29_vs_epa.py.

Standard library only.
"""
from __future__ import annotations

import argparse
import glob
import os
import re
import statistics
import sys

CYN_C_INDEX = 15     # aquabc_II_pelagic_svindex.f90::CYN_C_INDEX
CYN_N_INDEX = 33     # aquabc_II_pelagic_svindex.f90::CYN_N_INDEX
N_META = 4           # SEC_METAB_DIA/NOFIX_CYN/FIX_CYN/NOST, trailing state vars
NSTATE_VARN = 33      # nstate in the VARN build
NDIAGVAR = 30         # mod_GLOBAL.f90::NDIAGVAR -- per-state-var process-rate slot count
TOTAL_TRANSPORT_SLOTS = NSTATE_VARN + N_META  # 37 -- ADVECTION_ON(:) length under CONSIDER_ALLELOPATHY

MIN_CONCENTRATION = 1.0e-10  # aquabc_physical_constants.f90::MIN_CONCENTRATION
Q_ASSERT_MIN, Q_ASSERT_MAX = 0.095, 0.255  # smoke-mode quota assertion band

# CYN_N's own PROCESS_RATES block (aquabc_II_pelagic_model.f90:2447-2460), verified
# byte-exact against MASS_BALANCES.out's KINETICS column for var 33 in this task's
# investigation (see task-6-report.md): term1 uptake, 2 resp*Q, 3 death*Q, 4 excr*Q
# (-> DON sink), 5 zoo-feeding*Q loss.
CYN_N_TERM_LABELS = ["uptake", "resp*Q", "death*Q", "excr*Q (-> DON)", "zoo_feed*Q"]

# The five nitrogen pools in the degenerate-CYN conservation identity (state-var
# numbers match make_varn_inputs.py's DEGENERATE_ZERO_BOUNDARY_N + CYN_N_INDEX).
N_POOL_VARS = {1: "NH4_N", 2: "NO3_N", 10: "DET_PART_ORG_N", 13: "DISS_ORG_N",
               CYN_N_INDEX: "CYN_N"}


# --------------------------------------------------------------------------
# generic parsing helpers
# --------------------------------------------------------------------------

def _tokens(line):
    return line.split()


def box_number(path):
    m = re.search(r"PELAGIC_BOX_0*(\d+)(?:_PROCESS_RATES)?\.out$", path)
    return int(m.group(1)) if m else None


def box_out_paths(outputs_dir):
    """Every PELAGIC_BOX_NNNNN.out (state-variable output), excluding the
    companion _PROCESS_RATES.out files."""
    paths = sorted(glob.glob(os.path.join(outputs_dir, "PELAGIC_BOX_*.out")))
    return [p for p in paths if not p.endswith("_PROCESS_RATES.out")]


def read_box_out(path):
    """-> (header: list[str], rows: list[list[float]])."""
    with open(path) as fh:
        header = _tokens(fh.readline())
        rows = [[float(x) for x in _tokens(ln)] for ln in fh if ln.strip()]
    return header, rows


# --------------------------------------------------------------------------
# smoke mode
# --------------------------------------------------------------------------

OPTIONS_KEYS = ["CYN_VARIABLE_N", "CYN_N_QMIN", "CYN_N_QMAX", "CYN_N_VMAX", "CYN_N_KHS_UPT"]


def read_options_file(path):
    """Parse PELAGIC_MODEL_OPTIONS.txt -> {key: value_str} for the five VARN
    keys inserted by make_varn_inputs.py's OPTIONS_INSERT (the value is the
    non-comment line immediately below the matching '# <key>' comment line)."""
    lines = open(path).read().splitlines()
    out = {}
    for i, l in enumerate(lines):
        stripped = l.strip()
        for k in OPTIONS_KEYS:
            if stripped == f"# {k}" or stripped.startswith(f"# {k} "):
                if i + 1 < len(lines):
                    out[k] = lines[i + 1].strip()
    missing = [k for k in OPTIONS_KEYS if k not in out]
    if missing:
        raise ValueError(f"{path}: options keys not found: {missing}")
    return out


# mod_PELAGIC_ECOLOGY.f90:1283-1288 -- the exact echo text, ON case captures the
# four scalars, OFF case has no trailing numbers.
OPTIONS_ECHO_ON_RE = re.compile(
    r"CYN variable N \(Droop\): ON\.\s*"
    r"CYN_N_QMIN=\s*([0-9.+\-EeDd]+)\s+"
    r"CYN_N_QMAX=\s*([0-9.+\-EeDd]+)\s+"
    r"CYN_N_VMAX=\s*([0-9.+\-EeDd]+)\s+"
    r"CYN_N_KHS_UPT=\s*([0-9.+\-EeDd]+)")
# mod_PELAGIC_ECOLOGY.f90:1288 -- the real text is 'OFF (legacy Monod CYN
# N-limitation, default).', not just 'OFF.' -- match up to the word only.
OPTIONS_ECHO_OFF_RE = re.compile(r"CYN variable N \(Droop\): OFF\b")


def _fortran_float(s):
    """Fortran D-exponent ('1.0D-3') isn't valid Python float() syntax; the
    observed echo uses 'E', but tolerate 'D'/'d' too since list-directed
    write's exponent letter is a compiler/kind choice, not guaranteed."""
    return float(s.replace("D", "E").replace("d", "e"))


def parse_options_echo(log_lines):
    """-> {'CYN_VARIABLE_N': 'ON'|'OFF', and (if ON) the four floats}."""
    for l in log_lines:
        m = OPTIONS_ECHO_ON_RE.search(l)
        if m:
            return {"CYN_VARIABLE_N": "ON",
                     "CYN_N_QMIN": _fortran_float(m.group(1)),
                     "CYN_N_QMAX": _fortran_float(m.group(2)),
                     "CYN_N_VMAX": _fortran_float(m.group(3)),
                     "CYN_N_KHS_UPT": _fortran_float(m.group(4))}
        if OPTIONS_ECHO_OFF_RE.search(l):
            return {"CYN_VARIABLE_N": "OFF"}
    raise ValueError("'CYN variable N (Droop):' echo line not found in the log")


TRANSPORT_ANCHOR_RE = re.compile(r"Process rates will be written")


def parse_transport_echo(log_lines, total=TOTAL_TRANSPORT_SLOTS):
    """Find mod_PELAGIC_ECOLOGY.f90:759's `write(*,*) ADVECTION_ON(:)` --
    printed immediately after the 'Process rates will be written' line
    (mod_INITIALIZE_PELAGIC_BOX_MODEL.f90:161 calls INIT_TRANSPORT_FIELDS
    right after sub_READ_PELAGIC_INPUTS.f90:221/223 prints that anchor).
    sub_READ_PELAGIC_INPUTS.f90 prints TWO variants of this line depending on
    an output-units flag -- '...written in g/m^3/day' (line 221) or
    '...in g/m^2/day' (line 223). TRANSPORT_ANCHOR_RE matches only the shared
    "Process rates will be written" prefix deliberately, so both variants
    anchor correctly -- do not tighten this to match one unit suffix.
    gfortran's list-directed write may put all `total` integers on one line
    or wrap across several -- join whitespace-tokenized lines, stopping at
    the first line that doesn't look like more 0/1 tokens, until `total`
    tokens are collected."""
    start = None
    for i, l in enumerate(log_lines):
        if TRANSPORT_ANCHOR_RE.search(l):
            start = i + 1
            break
    if start is None:
        raise ValueError("anchor line 'Process rates will be written' not found in the log")
    toks = []
    j = start
    while j < len(log_lines) and len(toks) < total:
        line_toks = _tokens(log_lines[j])
        if not line_toks or any(t not in ("0", "1") for t in line_toks):
            break
        toks.extend(line_toks)
        j += 1
    if len(toks) != total:
        raise ValueError(
            f"expected {total} 0/1 tokens for the ADVECTION_ON transport-flag echo "
            f"starting after 'Process rates will be written', got {len(toks)}: {toks}")
    return [int(t) for t in toks]


def run_smoke(a):
    problems = []
    log_lines = open(a.log).read().splitlines()
    options = read_options_file(a.options)

    # (1) options echo vs file
    echo = parse_options_echo(log_lines)
    file_on = options["CYN_VARIABLE_N"].strip() not in ("0", "")
    echo_on = echo["CYN_VARIABLE_N"] == "ON"
    if echo_on != file_on:
        problems.append(f"echoed CYN_VARIABLE_N state {echo['CYN_VARIABLE_N']} "
                         f"!= options file value {options['CYN_VARIABLE_N']!r}")
    elif echo_on:
        for k in ("CYN_N_QMIN", "CYN_N_QMAX", "CYN_N_VMAX", "CYN_N_KHS_UPT"):
            file_v = float(options[k])
            echo_v = echo[k]
            if abs(echo_v - file_v) > 1e-6 * max(1.0, abs(file_v)):
                problems.append(f"{k}: echoed {echo_v} != options file {file_v}")
    print(f"[1] options echo vs {a.options}: "
          f"{'OK (' + echo['CYN_VARIABLE_N'] + ')' if not problems else 'MISMATCH'}")

    # (2) CYN_N column present
    paths = box_out_paths(a.outputs)
    if not paths:
        raise ValueError(f"no PELAGIC_BOX_*.out files found in {a.outputs}")
    header0, _ = read_box_out(paths[0])
    has_cyn_n = "CYN_N" in header0
    if not has_cyn_n:
        problems.append(f"CYN_N column not present in {paths[0]}")
    print(f"[2] CYN_N column present in outputs: {'OK' if has_cyn_n else 'MISSING'}")

    # (3) transport-flag echo: slot 33 (CYN_N) and slots 34:37 (allelopathy) == 1
    flags = parse_transport_echo(log_lines)
    slot33_ok = flags[CYN_N_INDEX - 1] == 1
    slots_34_37 = flags[CYN_N_INDEX:CYN_N_INDEX + N_META]  # 0-indexed 33..36 = 1-indexed 34..37
    slots_ok = all(v == 1 for v in slots_34_37)
    if not slot33_ok:
        problems.append(f"ADVECTION_ON[33] = {flags[CYN_N_INDEX - 1]} (expected 1)")
    if not slots_ok:
        problems.append(f"ADVECTION_ON[34:37] = {slots_34_37} (expected all 1)")
    print(f"[3] transport flags: slot33={flags[CYN_N_INDEX - 1]}, "
          f"slots34:37={slots_34_37}: {'OK' if slot33_ok and slots_ok else 'MISMATCH'}")

    # (4) quota Q = CYN_N/CYN_C in [Q_ASSERT_MIN, Q_ASSERT_MAX]
    high_flush = set(a.high_flush_boxes)
    n_floor = n_high_flush = n_high_flush_bad = n_in = n_out = 0
    by_box_out = {}
    if has_cyn_n:
        for p in paths:
            box = box_number(p)
            header, rows = read_box_out(p)
            if "CYN_C" not in header or "CYN_N" not in header:
                continue
            ic, in_ = header.index("CYN_C"), header.index("CYN_N")
            for r in rows:
                c, nn = r[ic], r[in_]
                if c <= 2 * MIN_CONCENTRATION:
                    n_floor += 1
                    continue
                q = nn / c
                in_bounds = Q_ASSERT_MIN <= q <= Q_ASSERT_MAX
                if box in high_flush:
                    n_high_flush += 1
                    if not in_bounds:
                        n_high_flush_bad += 1
                        by_box_out[box] = by_box_out.get(box, 0) + 1
                    continue
                if in_bounds:
                    n_in += 1
                else:
                    n_out += 1
                    by_box_out[box] = by_box_out.get(box, 0) + 1
    if n_out:
        problems.append(f"{n_out} quota samples outside [{Q_ASSERT_MIN},{Q_ASSERT_MAX}] "
                         f"in non-exempt boxes (by box: {by_box_out})")
    print(f"[4] quota Q in [{Q_ASSERT_MIN},{Q_ASSERT_MAX}]: {n_in} OK, {n_out} FAIL asserted "
          f"| reported only (not asserted): {n_floor} floor-artifact "
          f"(CYN_C<=2*MIN_CONCENTRATION={2 * MIN_CONCENTRATION:g}), "
          f"{n_high_flush} high-flush-box samples ({n_high_flush_bad} out of bounds)")

    print()
    if problems:
        print("SMOKE: FAIL")
        for p in problems:
            print(f"  - {p}")
        return 1
    print("SMOKE: PASS")
    return 0


# --------------------------------------------------------------------------
# conserve mode
# --------------------------------------------------------------------------

def parse_mass_balances(path, var_filter):
    """MASS_BALANCES.out -> {(box,time): {var_no: (advection,diffusion,settling,
    mass_loads,mass_withdrawals,kinetics,sediment_fluxes)}} for var_no in
    var_filter. mod_AQUATIC_MODEL.f90:263-268 header;
    sub_WRITE_PELAGIC_OUTPUT.f90 write order."""
    out = {}
    with open(path) as fh:
        header = fh.readline()
        if "KINETICS" not in header:
            raise ValueError(f"{path}: does not look like a MASS_BALANCES.out (no KINETICS column)")
        for ln in fh:
            f = _tokens(ln)
            if len(f) < 10:
                continue
            t, box, var = float(f[0]), int(f[1]), int(f[2])
            if var not in var_filter:
                continue
            terms = tuple(float(x) for x in f[3:10])
            out.setdefault((box, t), {})[var] = terms
    return out


def run_conserve(a):
    samples = parse_mass_balances(a.mass_balances, set(N_POOL_VARS))
    if not samples:
        raise ValueError(f"no rows for state vars {sorted(N_POOL_VARS)} "
                          f"(NH4_N/NO3_N/DET_PART_ORG_N/DISS_ORG_N/CYN_N) found in {a.mass_balances}")

    print("Identity: at every (box, time) sample, the sum of the KINETICS term "
          "(index 6 of the 7 MASS_BALANCES.out columns) across the five nitrogen "
          f"pools {sorted(N_POOL_VARS.values())} is (near) zero -- internal "
          "biogeochemistry moves N between these pools but neither creates nor "
          "destroys it. ADVECTION/DIFFUSION/SETTLING/MASS_LOADS/MASS_WITHDRAWALS are "
          "excluded by construction (transport, not biology); this run's "
          "--degenerate-cyn scenario must also have disabled water-column "
          "denitrification and gas exchange (K_MIN_DOC_NO3N_20, K_A -> 0 in WCONST) "
          "since those are lumped into KINETICS and are not separately reported.\n"
          "SEDIMENT_FLUXES (column 7) is wsc%FLUXES_TO_WATER_COLUMN "
          "(mod_SIMULATE.f90:608-610) -- the advanced-diagenesis sediment model's "
          "flux, only populated when MODEL_BOTTOM_SEDIMENTS>1 (mod_SOLVER.f90:1575); "
          "under the prescribed-flux mode this scenario uses (MODEL_SEDIMENTS=1) it "
          "stays exactly 0.0 all run, so this identity never has to account for it. "
          "NOTE -- a SEPARATE, THIRD pathway exists and is NOT visible in either "
          "printed column: a prescribed sediment flux file (e.g. SED_FLUX_NO3_SINK.txt, "
          "part of the standard CL29/CL29_VARN driver config, unrelated to "
          "--degenerate-cyn) is applied directly to the state at integration time "
          "(ECOL_PRESCRIBED_SEDIMENT_FLUX_DERIVS, mod_SOLVER.f90:186) but is never "
          "written to MASS_BALANCES.out -- confirmed from source: "
          "DERIVATIVES(NH4_N_INDEX)/(NO3_N_INDEX) in aquabc_II_pelagic_model.f90:2160-2215 "
          "are built purely from biological PROCESS_RATES and never reference the "
          "SEDIMENT_FLUXES argument (which feeds only the ALKALINITY derivative, "
          "aquabc_II_pelagic_model.f90:3463-3521/3552/3591/3838). This pathway cannot "
          "corrupt the KINETICS-only identity below (it is structurally absent from "
          "that column), but it also means a PASS here is not a full water-column N "
          "mass balance -- it verifies only that biological transformations among "
          "these five pools conserve N; any active prescribed sediment N flux is a "
          "real, separate exchange with an external reservoir, out of this identity's "
          "scope by construction, not accounted for or asserted on by this checker.")

    complete = {k: v for k, v in samples.items() if len(v) == len(N_POOL_VARS)}
    if not complete:
        raise ValueError("no (box,time) sample had all five N pools present -- cannot compute the identity")

    gross_flux = 0.0     # sum |term| over every sample x pool -- total biogeochemical "activity"
    net_residual = 0.0   # signed sum of per-sample residuals -- near 0 if noise, not systematic
    abs_residual_sum = 0.0
    max_abs = -1.0
    worst_key = None
    per_sample_rel = []  # diagnostic only, NOT the pass/fail criterion -- see note below
    for key, per_var in complete.items():
        kin = [terms[5] for terms in per_var.values()]
        s = sum(kin)
        scale = max(abs(k) for k in kin)
        gross_flux += sum(abs(k) for k in kin)
        net_residual += s
        abs_residual_sum += abs(s)
        if abs(s) > max_abs:  # -1.0 init guarantees the first sample always sets worst_key,
            max_abs, worst_key = abs(s), key  # even when every residual is exactly 0.0
        if scale > 0:
            per_sample_rel.append(abs(s) / scale)

    n = len(complete)
    rel_gross = abs_residual_sum / gross_flux if gross_flux else float("nan")
    rel_net = abs(net_residual) / gross_flux if gross_flux else float("nan")

    print(f"\nsamples: {n}")
    print(f"max |sum KINETICS| at any single sample: {max_abs:.6e} g/m^3/day "
          f"(box={worst_key[0]}, t={worst_key[1]})")
    print(f"gross biogeochemical activity (sum |term| over every sample x pool): {gross_flux:.6e}")
    print(f"relative (conservative, sum|residual|/gross activity, no cancellation allowed): {rel_gross:.6e}")
    print(f"relative (net, |sum of SIGNED residuals|/gross activity -- near 0 only if the "
          f"residual is unbiased rounding noise, not a systematic leak): {rel_net:.6e}")
    print(f"tolerance: {a.rel_tol:g} (checked against the conservative rel_gross measure above)")
    if per_sample_rel:
        print(f"\nNOTE on the per-sample worst-case ratio (median {statistics.median(per_sample_rel):.3e}, "
              f"max {max(per_sample_rel):.3e}, over {len(per_sample_rel)} samples with a nonzero scale): "
              f"this is NOT the pass/fail criterion here. A single low-activity (box,time) sample can "
              f"have a near-zero denominator (its own largest KINETICS term), which inflates this ratio "
              f"without indicating a real leak -- the ABSOLUTE residual at those samples is the SAME "
              f"+-1e-6..2e-6 print-precision floor seen everywhere else (see task-6-report.md).")
    else:
        print("\n(every sample had all five KINETICS terms exactly 0.0 -- no per-sample "
              "relative ratio to report)")
    print(f"\nspec target is 1e-9 relative; MASS_BALANCES.out is printed 'f20.6' (six decimal "
          f"places), which floors the achievable per-term precision at ~5e-7 absolute regardless "
          f"of model correctness -- 1e-9 relative is not reachable through this ASCII output path "
          f"(see task-6-report.md for the measured floor and the before/after-fix evidence that "
          f"the residual IS that floor, not a real leak).")

    ok = rel_gross <= a.rel_tol
    print()
    print("CONSERVE: PASS" if ok else "CONSERVE: FAIL")
    return 0 if ok else 1


# --------------------------------------------------------------------------
# nbudget mode
# --------------------------------------------------------------------------

def cyn_n_process_terms(fields):
    """CYN_N_INDEX's 5-term PROCESS_RATES block from a parsed
    _PROCESS_RATES.out row (fields[0] is TIME_DAYS; fields[1:] is the flat
    (state_var-1)*NDIAGVAR + diag_index vector, state-major --
    mod_SOLVER.f90:1739-1744: PROCESS_RATE_BEGIN_NO=((j-1)*NDIAGVAR)+1,
    PROCESS_RATE_END_NO=j*NDIAGVAR. Verified byte-exact against
    MASS_BALANCES.out's KINETICS(CYN_N) in this task's investigation."""
    begin = (CYN_N_INDEX - 1) * NDIAGVAR + 1  # 1-indexed into the process-rate vector
    return [float(fields[begin + k]) for k in range(len(CYN_N_TERM_LABELS))]


def run_nbudget(a):
    box_str = f"{a.box:05d}"
    out_path = os.path.join(a.outputs, f"PELAGIC_BOX_{box_str}.out")
    pr_path = os.path.join(a.outputs, f"PELAGIC_BOX_{box_str}_PROCESS_RATES.out")

    header, rows = read_box_out(out_path)
    for req in ("TIME_DAYS", "CYN_C", "CYN_N"):
        if req not in header:
            raise ValueError(f"{out_path}: missing required column {req!r}")
    it, ic, in_ = header.index("TIME_DAYS"), header.index("CYN_C"), header.index("CYN_N")

    has_pr = os.path.isfile(pr_path)
    print(f"CYN nitrogen budget, box {a.box}")
    print(f"source: {'PROCESS_RATES output (' + pr_path + ')' if has_pr else 'state-difference NET estimate (PROCESS_RATE_OUT was not set for this box -- PROCESS_RATES.out absent)'}")
    print()

    if has_pr:
        with open(pr_path) as fh:
            pr_rows = [_tokens(ln) for ln in fh if ln.strip()]
        hdr = f"{'time':>8}" + "".join(f"{lbl:>16}" for lbl in CYN_N_TERM_LABELS) + f"{'net(kinetics)':>16}"
        print(hdr)
        for f in pr_rows:
            t = float(f[0])
            terms = cyn_n_process_terms(f)
            uptake = terms[0]
            losses = sum(terms[1:])
            net = uptake - losses
            row = f"{t:>8.2f}" + "".join(f"{v:>16.6f}" for v in terms) + f"{net:>16.6f}"
            print(row)
        print()
        print("net(kinetics) = uptake - (resp*Q + death*Q + excr*Q + zoo_feed*Q); this "
              "is the KINETICS-only rate (matches MASS_BALANCES.out's KINETICS column "
              "for CYN_N when both are available) -- it does NOT include ADVECTION/"
              "DIFFUSION/SETTLING, so it will generally NOT match the raw slope of the "
              "CYN_N state trajectory in a transported (non-degenerate) box.")
    else:
        print(f"{'time':>8}{'obs dCYN_N/dt':>16}   (net of kinetics AND transport -- NOT a kinetics-only decomposition)")
        for i in range(1, len(rows)):
            t0, t1 = rows[i - 1][it], rows[i][it]
            c0, c1 = rows[i - 1][in_], rows[i][in_]
            dt = t1 - t0
            slope = (c1 - c0) / dt if dt else float("nan")
            print(f"{t1:>8.2f}{slope:>16.6f}")
        print()
        print("Per-term decomposition (uptake / each Q-weighted loss) requires "
              "PROCESS_RATE_OUT=1 for this box in PELAGIC_OUTPUT_INFORMATION_FILE.txt "
              "-- not set for this run, so only the coarser net (state-difference, "
              "includes transport) estimate above is available.")

    print()
    print("NOTE: the Fortran-side derived-TN output (GENERATE_PELAGIC_DERIVED_VARS, "
          "mod_PELAGIC_ECOLOGY.f90:369) computes TN's CYN contribution as "
          "CYN_C*CYN_N_TO_C (the legacy fixed ratio) even when CYN_VARIABLE_N=1 -- it "
          "does not read the CYN_N state variable, and is therefore WRONG for VARN/"
          "Droop runs. TN comparisons against observations MUST use "
          "tools/validate_cl29_vs_epa.py (CYN_N-column-aware as of this task), not any "
          "Fortran-side TN diagnostic.")
    return 0


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------

def _box_list(s):
    return [int(x) for x in s.split(",") if x.strip()]


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--mode", required=True, choices=["smoke", "conserve", "nbudget"])
    # smoke
    ap.add_argument("--log", help="[smoke] path to the ESTAS_II_varN run's captured stdout log")
    ap.add_argument("--outputs", help="[smoke, nbudget] the run's PELAGIC MODEL OUTPUT FOLDER")
    ap.add_argument("--options", help="[smoke] the run's PELAGIC_MODEL_OPTIONS.txt")
    ap.add_argument("--high-flush-boxes", type=_box_list, default=[],
                     help="[smoke] comma-separated box numbers exempted from the hard "
                          "quota-bounds assertion (their excursions are reported, not "
                          "asserted) -- Q is not conservative under advective mixing")
    # conserve
    ap.add_argument("--mass-balances", help="[conserve] path to MASS_BALANCES.out from the "
                                             "degenerate-CYN scenario run")
    ap.add_argument("--rel-tol", type=float, default=1e-4,
                     help="[conserve] relative-residual tolerance (default 1e-4; the spec's "
                          "1e-9 target is not reachable given MASS_BALANCES.out's f20.6 "
                          "print precision -- see task-6-report.md)")
    # nbudget
    ap.add_argument("--box", type=int, help="[nbudget] box number")
    a = ap.parse_args(argv)

    if a.mode == "smoke":
        for req in ("log", "outputs", "options"):
            if not getattr(a, req):
                ap.error(f"--mode smoke requires --{req.replace('_', '-')}")
        return run_smoke(a)
    if a.mode == "conserve":
        if not a.mass_balances:
            ap.error("--mode conserve requires --mass-balances")
        return run_conserve(a)
    if a.mode == "nbudget":
        for req in ("outputs", "box"):
            if getattr(a, req) is None:
                ap.error(f"--mode nbudget requires --{req.replace('_', '-')}")
        return run_nbudget(a)
    return 1


if __name__ == "__main__":
    sys.exit(main())
