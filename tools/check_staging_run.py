#!/usr/bin/env python3
"""Verification battery checker for the NOST akinete life-cycle staging diagnostic.

Reads NOST_STAGING.out (opt-in diagnostic written by mod_SIMULATE.f90 when
NOST_STAGE_MODEL > 0; columns: WTIME BOX BED_AKI RAD_EMA LATCH
STG_SETTLE_FLUX STG_GERM_FLUX STG_FORM_FLUX CUM_SETTLE_AKI CUM_GERM_AKI
CUM_FORM_AKI BURIED_AKI, one header line) and runs one of three
verification-battery rungs:

  smoke        V3 -- file sanity, BED_AKI>0 reachability, and the run log's
                option-echo values matching the options file (pair-swap
                defense)
  conservation V4 -- the exact bed identity BED_AKI + BURIED_AKI -
                (CUM_SETTLE_AKI - CUM_GERM_AKI) == 0 (from t=0, where every
                term starts at zero, so this closes the "between any two
                output times" identity too); plus, with --second, the
                cross-solver day-90 BED_AKI comparison (double-banking
                detector)
  timing       V5 -- per (year, box) first day the LATCH column turns from
                0 to 1 (the formation-latch autumn crossing), asserted to
                fall in [Aug 31, Oct 7] for --year-start..--year-end

See .superpowers/sdd/2026-08-23-nost-akinete-staging/task-6-brief.md.
"""
from __future__ import annotations

import argparse
import datetime
import re
import sys

COLUMNS = (
    "WTIME", "BOX", "BED_AKI", "RAD_EMA", "LATCH", "STG_SETTLE_FLUX",
    "STG_GERM_FLUX", "STG_FORM_FLUX", "CUM_SETTLE_AKI", "CUM_GERM_AKI",
    "CUM_FORM_AKI", "BURIED_AKI",
)

# T_GERM_AKI_STAGE, I_FORM_AKI, KR_GERM_BED, K_MORT_BED_AKI, V_SETTLE_AKI, in the
# order the run log echoes them (mod_SIMULATE.f90 "NOST staging: ON." line).
ECHO_RE = re.compile(
    r"NOST staging:\s*ON\.\s*"
    r"T_GERM=\s*([0-9.EeDd+-]+)\s*"
    r"I_FORM=\s*([0-9.EeDd+-]+)\s*"
    r"KR_GERM_BED=\s*([0-9.EeDd+-]+)\s*"
    r"K_MORT_BED=\s*([0-9.EeDd+-]+)\s*"
    r"V_SETTLE=\s*([0-9.EeDd+-]+)"
)
ECHO_FIELDS = (
    "T_GERM_AKI_STAGE", "I_FORM_AKI", "KR_GERM_BED", "K_MORT_BED_AKI", "V_SETTLE_AKI",
)


def _to_float(tok):
    return float(tok.replace("D", "E").replace("d", "e"))


def read_staging_out(path):
    """Return (header_fields, rows); each row is a dict of column name -> value."""
    with open(path) as fh:
        header = fh.readline().split()
        rows = []
        for line in fh:
            line = line.strip()
            if not line:
                continue
            p = line.split()
            rows.append({
                "WTIME": float(p[0]), "BOX": int(p[1]),
                "BED_AKI": float(p[2]), "RAD_EMA": float(p[3]), "LATCH": int(p[4]),
                "STG_SETTLE_FLUX": float(p[5]), "STG_GERM_FLUX": float(p[6]),
                "STG_FORM_FLUX": float(p[7]), "CUM_SETTLE_AKI": float(p[8]),
                "CUM_GERM_AKI": float(p[9]), "CUM_FORM_AKI": float(p[10]),
                "BURIED_AKI": float(p[11]),
            })
    return header, rows


def read_options_file(path):
    """Parse '# NAME (...)' / value-line pairs from a PELAGIC_MODEL_OPTIONS.txt file."""
    with open(path) as fh:
        lines = [ln.rstrip("\n") for ln in fh]
    values = {}
    for i, line in enumerate(lines):
        stripped = line.lstrip()
        if not stripped.startswith("#"):
            continue
        rest = stripped.lstrip("#").strip()
        if not rest:
            continue
        name = rest.split()[0]
        if i + 1 < len(lines):
            try:
                values[name] = float(lines[i + 1].strip())
            except ValueError:
                pass
    return values


def identity_check(rows, tol, tol_abs):
    """V4(a): per-row (== per-box, from-t=0) exact bed identity.

    allowed = max(tol * max(|term|), tol_abs); PASS iff |residual| <= allowed
    for every row. Returns (all_ok, max_abs_residual, worst) where worst is
    (margin, abs_residual, allowed, row) for the row with the largest
    (abs_residual - allowed) margin, or None if rows is empty.
    """
    all_ok = True
    max_abs_residual = 0.0
    worst = None
    for r in rows:
        terms = (r["BED_AKI"], r["BURIED_AKI"], r["CUM_SETTLE_AKI"], r["CUM_GERM_AKI"])
        max_term = max(abs(t) for t in terms)
        allowed = max(tol * max_term, tol_abs)
        residual = r["BED_AKI"] + r["BURIED_AKI"] - r["CUM_SETTLE_AKI"] + r["CUM_GERM_AKI"]
        ar = abs(residual)
        max_abs_residual = max(max_abs_residual, ar)
        if ar > allowed:
            all_ok = False
        margin = ar - allowed
        if worst is None or margin > worst[0]:
            worst = (margin, ar, allowed, r)
    return all_ok, max_abs_residual, worst


def cross_solver_diffs(rows_a, rows_b, day, day_tol=1e-6):
    """Per-box |BED_AKI_b - BED_AKI_a| / max(BED_AKI_a, 1e-6) at WTIME ~= day.

    rows_a is the reference (denominator) run -- pass the Euler run here and
    the RK2 run as rows_b to match the spec's |BED_rk2 - BED_euler| formula.
    """
    def snapshot(rows):
        return {r["BOX"]: r["BED_AKI"] for r in rows if abs(r["WTIME"] - day) < day_tol}

    snap_a, snap_b = snapshot(rows_a), snapshot(rows_b)
    common = sorted(set(snap_a) & set(snap_b))
    return {box: abs(snap_b[box] - snap_a[box]) / max(snap_a[box], 1e-6) for box in common}


def mode_smoke(args):
    checks = []
    header, rows = read_staging_out(args.staging_out)
    checks.append(("file non-empty", len(rows) > 0, f"{len(rows)} data rows"))
    if not rows:
        return checks

    checks.append(("header matches spec", header == list(COLUMNS), " ".join(header)))

    max_wtime = max(r["WTIME"] for r in rows)
    window = min(300.0, max_wtime)
    max_bed = max(r["BED_AKI"] for r in rows if r["WTIME"] <= window)
    checks.append((
        f"max(BED_AKI) > 0 by day {window:.4f} (file spans to day {max_wtime:.4f})",
        max_bed > 0.0,
        f"max={max_bed:.10f}",
    ))

    if not args.log:
        if args.options_file:
            print("WARN: --options-file given without --log; skipping echo comparison",
                  file=sys.stderr)
        return checks

    with open(args.log, errors="replace") as fh:
        log_text = fh.read()
    m = ECHO_RE.search(log_text)
    checks.append((
        "log has 'NOST staging: ON' echo",
        m is not None,
        m.group(0).replace("\n", " ") if m else "pattern not found in " + args.log,
    ))
    if m is None:
        return checks

    echoed = {name: _to_float(m.group(i + 1)) for i, name in enumerate(ECHO_FIELDS)}
    if not args.options_file:
        return checks

    file_vals = read_options_file(args.options_file)
    for name in ECHO_FIELDS:
        fv = file_vals.get(name)
        ev = echoed[name]
        ok = fv is not None and abs(fv - ev) <= 1e-9 * max(abs(fv), 1.0)
        checks.append((
            f"echo {name} == options-file value (pair-swap defense)",
            ok,
            f"echo={ev!r} file={fv!r}",
        ))
    return checks


def mode_conservation(args):
    checks = []
    _, rows_a = read_staging_out(args.staging_out)
    ok_a, max_res_a, worst_a = identity_check(rows_a, args.tol, args.tol_abs)
    detail_a = f"max|residual|={max_res_a:.3e} over {len(rows_a)} rows"
    if worst_a is not None:
        wr = worst_a[3]
        detail_a += (f"; worst: box={wr['BOX']} WTIME={wr['WTIME']:.4f} "
                      f"|residual|={worst_a[1]:.3e} allowed={worst_a[2]:.3e}")
    checks.append((f"V4(a) exact bed identity [{args.staging_out}]", ok_a, detail_a))

    if not args.second:
        return checks

    _, rows_b = read_staging_out(args.second)
    ok_b, max_res_b, worst_b = identity_check(rows_b, args.tol, args.tol_abs)
    detail_b = f"max|residual|={max_res_b:.3e} over {len(rows_b)} rows"
    if worst_b is not None:
        wr = worst_b[3]
        detail_b += (f"; worst: box={wr['BOX']} WTIME={wr['WTIME']:.4f} "
                      f"|residual|={worst_b[1]:.3e} allowed={worst_b[2]:.3e}")
    checks.append((f"V4(a) exact bed identity [{args.second}]", ok_b, detail_b))

    day = args.cross_day
    if day is None:
        day = min(max(r["WTIME"] for r in rows_a), max(r["WTIME"] for r in rows_b))
    diffs = cross_solver_diffs(rows_a, rows_b, day)
    label = f"V4(b) cross-solver BED_AKI relative diff @ day {day:.4f}"
    if not diffs:
        checks.append((label, False, "no common (box, day) rows found -- check --cross-day"))
        return checks

    worst_box = max(diffs, key=diffs.get)
    worst_val = diffs[worst_box]
    checks.append((
        label,
        worst_val < args.tol_cross,
        f"max rel diff={worst_val:.4%} at box {worst_box} "
        f"(n={len(diffs)} boxes, tol={args.tol_cross:.0%})",
    ))

    # Sanity guard: with a silently-ignored ESTAS_PELAGIC_SOLVER, --second would be a
    # byte-identical run and this whole check would PASS vacuously (max diff = 0 for every
    # box). Require some real divergence so a trivial pass can't hide a wired-wrong solver.
    n_diverged = sum(1 for v in diffs.values() if v > 1e-9)
    checks.append((
        "V4(b) sanity: solvers actually diverge (not a vacuous 0.0000% pass)",
        n_diverged > 0,
        f"{n_diverged}/{len(diffs)} boxes show rel diff > 1e-9 at day {day:.4f}",
    ))
    return checks


def mode_timing(args):
    checks = []
    _, rows = read_staging_out(args.staging_out)
    if not rows:
        checks.append(("file non-empty", False, "0 data rows"))
        return checks

    base = datetime.date(args.base_year, 1, 1)
    by_box = {}
    for r in rows:
        by_box.setdefault(r["BOX"], []).append(r)
    n_boxes = len(by_box)

    # (year, box) -> earliest LATCH 0->1 transition date within that year.
    first_on = {}
    for box, rs in by_box.items():
        rs.sort(key=lambda r: r["WTIME"])
        prev_latch = None
        for r in rs:
            if prev_latch == 0 and r["LATCH"] == 1:
                d = base + datetime.timedelta(days=r["WTIME"])
                first_on.setdefault((d.year, box), d)
            prev_latch = r["LATCH"]

    years_present = {y for y, _ in first_on}
    candidate_years = sorted(
        years_present | {args.base_year} | set(range(args.year_start, args.year_end + 1))
    )

    for year in candidate_years:
        in_range = args.year_start <= year <= args.year_end
        dates = sorted(d for (y, _), d in first_on.items() if y == year)
        if not dates:
            detail = "no LATCH 0->1 transition found in any box this year"
            checks.append((f"V5 timing year {year} first-ON window", not in_range, detail))
            continue
        dmin, dmax = dates[0], dates[-1]
        aug31, sep30 = datetime.date(year, 8, 31), datetime.date(year, 10, 7)
        window_ok = (dmin >= aug31) and (dmax <= sep30)
        coverage_ok = len(dates) == n_boxes
        window_detail = f"first-ON range=[{dmin},{dmax}], window=[{aug31},{sep30}]"
        coverage_detail = (f"{len(dates)}/{n_boxes} boxes had a 0->1 transition this year "
                            "(a box with no spring germ-release has no autumn turn-on)")
        window_label = f"V5 timing year {year} first-ON window"
        coverage_label = f"V5 timing year {year} box coverage"
        if not in_range:
            window_label += " (informational, outside assertion range)"
            coverage_label += " (informational, outside assertion range)"
            window_ok = coverage_ok = True
        checks.append((window_label, window_ok, window_detail))
        checks.append((coverage_label, coverage_ok, coverage_detail))

    return checks


def build_parser():
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--staging-out", required=True,
                    help="path to NOST_STAGING.out (primary run; the Euler run for "
                         "conservation's --second comparison)")
    p.add_argument("--mode", required=True, choices=("smoke", "timing", "conservation"))
    p.add_argument("--log", help="smoke: run stdout log, for the option-echo check")
    p.add_argument("--options-file", help="smoke: PELAGIC_MODEL_OPTIONS.txt to compare "
                                           "the log's echoed values against")
    p.add_argument("--second", help="conservation: second run's NOST_STAGING.out "
                                     "(pass the RK2 run's file here) -- enables V4(a) on "
                                     "both files plus the V4(b) cross-solver check")
    p.add_argument("--tol", type=float, default=1e-12,
                    help="conservation: relative tolerance on the largest identity term "
                         "(default 1e-12)")
    p.add_argument("--tol-abs", type=float, default=2.5e-10,
                    help="conservation: absolute floor for the identity tolerance. The "
                         "identity sums/differences 4 columns each printed at F20.10, so "
                         "even an exactly-closed internal residual can show up to 4 * "
                         "0.5e-10 = 2e-10 from independent print rounding alone, plus a "
                         "few 1e-14-scale ULPs of genuine floating-point noise on top -- "
                         "verified empirically (task-6-report.md V4 section): "
                         "residual/1e-10 across 208,800 rows of the Task-5 run clusters at "
                         "integers in {-1, 0, +1, +2} (worst case 2.000000009e-10), the "
                         "signature of quantization, not a real conservation gap. A real "
                         "double-banking bug shows up at O(BED_AKI), i.e. ~100% relative --"
                         " this floor keeps ~9 orders of margin below that (default 2.5e-10)")
    p.add_argument("--tol-cross", type=float, default=0.05,
                    help="conservation: max allowed relative BED_AKI diff between "
                         "solvers at --cross-day (default 0.05 = 5%%)")
    p.add_argument("--cross-day", type=float, default=None,
                    help="conservation: WTIME day for the cross-solver comparison "
                         "(default: the latest day common to both files)")
    p.add_argument("--base-year", type=int, default=2012,
                    help="timing: calendar year of WTIME day 0 (default 2012)")
    p.add_argument("--year-start", type=int, default=2013,
                    help="timing: first year the first-ON window is asserted (default 2013)")
    p.add_argument("--year-end", type=int, default=2022,
                    help="timing: last year the first-ON window is asserted, inclusive "
                         "(default 2022)")
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)
    mode_fn = {"smoke": mode_smoke, "conservation": mode_conservation, "timing": mode_timing}
    results = mode_fn[args.mode](args)

    print(f"=== check_staging_run.py --mode {args.mode} ===")
    ok = True
    for name, passed, detail in results:
        print(f"[{'PASS' if passed else 'FAIL'}] {name}: {detail}")
        ok = ok and passed
    print(f"=== {'ALL PASS' if ok else 'FAILURES PRESENT'} ({len(results)} checks) ===")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
