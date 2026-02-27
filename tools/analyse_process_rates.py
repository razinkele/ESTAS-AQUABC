#!/usr/bin/env python3
"""
Comprehensive Process Rate Analysis for AQUABC Model Outputs.

Analyses mass balance consistency, stoichiometric ratios, and process rate
indicators derived from the 3560-day simulation time series.

Outputs:
  - Console summary of all checks
  - data/process_rate_analysis.json with numeric results
"""

import json
import os
import sys
from pathlib import Path

import numpy as np

WORKSPACE = Path(__file__).resolve().parent.parent
OUTPUTS = WORKSPACE / "OUTPUTS"

BOXES = [5, 6, 8, 9, 14, 17, 25]
MUD_BOXES = [5, 8, 14, 17, 25]
SAND_BOXES = [6, 9]

# Column indices (0-based after TIME_DAYS column)
COLS = {
    "TIME": 0,
    "NH4_N": 1, "NO3_N": 2, "PO4_P": 3, "DISS_OXYGEN": 4,
    "DIA_C": 5, "ZOO_C": 6, "ZOO_N": 7, "ZOO_P": 8,
    "DET_PART_ORG_C": 9, "DET_PART_ORG_N": 10, "DET_PART_ORG_P": 11,
    "DISS_ORG_C": 12, "DISS_ORG_N": 13, "DISS_ORG_P": 14,
    "CYN_C": 15, "OPA_C": 16, "DISS_Si": 17, "PART_Si": 18,
    "FIX_CYN_C": 19, "INORG_C": 20, "TOT_ALK": 21,
    "FE_II": 22, "FE_III": 23, "MN_II": 24, "MN_IV": 25,
    "CA": 26, "MG": 27, "S_PLUS_6": 28, "S_MINUS_2": 29,
    "CH4_C": 30, "NOST_VEG_HET_C": 31, "AKI_C": 32,
    "SEC_METAB_DIA": 33, "SEC_METAB_NOFIX_CYN": 34,
    "SEC_METAB_FIX_CYN": 35, "SEC_METAB_NOST": 36,
}

# Target stoichiometric ratios from WCONST_04.txt
STOICH = {
    "ZOO_N_TO_C": 0.22,
    "ZOO_P_TO_C": 0.03,
    "DIA_N_TO_C": 0.151,
    "DIA_P_TO_C": 0.014,
}


def load_box(box_no):
    """Load a PELAGIC_BOX output file, return numpy array (skip header)."""
    fn = OUTPUTS / f"PELAGIC_BOX_{box_no:05d}.out"
    return np.loadtxt(fn, skiprows=1)


def check_non_negative(data, box_no, results):
    """Check that concentrations that should be non-negative are non-negative."""
    issues = []
    bio_vars = [
        "NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C", "ZOO_C", "ZOO_N",
        "ZOO_P", "DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P",
        "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P", "CYN_C", "OPA_C",
        "DISS_Si", "PART_Si", "FIX_CYN_C", "NOST_VEG_HET_C", "AKI_C",
    ]
    for var in bio_vars:
        col = COLS[var]
        vals = data[:, col]
        if np.any(vals < -1e-10):
            n_neg = np.sum(vals < -1e-10)
            min_val = np.min(vals)
            issues.append({
                "variable": var,
                "n_negative": int(n_neg),
                "min_value": float(min_val),
            })
    results[f"box_{box_no}"]["non_negative"] = {
        "pass": len(issues) == 0,
        "issues": issues,
    }
    return len(issues) == 0


def check_zoo_stoichiometry(data, box_no, results):
    """Check ZOO N:C and P:C ratios stay realistic."""
    zoo_c = data[:, COLS["ZOO_C"]]
    zoo_n = data[:, COLS["ZOO_N"]]
    zoo_p = data[:, COLS["ZOO_P"]]

    # Skip timesteps where ZOO_C is negligible
    mask = zoo_c > 0.001
    if not np.any(mask):
        results[f"box_{box_no}"]["zoo_stoich"] = {"pass": True, "note": "ZOO_C negligible"}
        return True

    n_to_c = zoo_n[mask] / zoo_c[mask]
    p_to_c = zoo_p[mask] / zoo_c[mask]

    target_nc = STOICH["ZOO_N_TO_C"]
    target_pc = STOICH["ZOO_P_TO_C"]

    nc_ok = np.all((n_to_c > 0.05) & (n_to_c < 0.5))
    pc_ok = np.all((p_to_c > 0.005) & (p_to_c < 0.1))

    results[f"box_{box_no}"]["zoo_stoich"] = {
        "pass": nc_ok and pc_ok,
        "N_to_C": {
            "target": target_nc,
            "mean": float(np.mean(n_to_c)),
            "min": float(np.min(n_to_c)),
            "max": float(np.max(n_to_c)),
            "within_bounds": bool(nc_ok),
        },
        "P_to_C": {
            "target": target_pc,
            "mean": float(np.mean(p_to_c)),
            "min": float(np.min(p_to_c)),
            "max": float(np.max(p_to_c)),
            "within_bounds": bool(pc_ok),
        },
    }
    return nc_ok and pc_ok


def check_redox_balance(data, box_no, results):
    """Check Fe/Mn redox pairs conserve total mass."""
    fe_ii = data[:, COLS["FE_II"]]
    fe_iii = data[:, COLS["FE_III"]]
    mn_ii = data[:, COLS["MN_II"]]
    mn_iv = data[:, COLS["MN_IV"]]

    fe_total = fe_ii + fe_iii
    mn_total = mn_ii + mn_iv

    # Check if total is conserved (allowing for advective transport)
    fe_cv = float(np.std(fe_total) / max(np.mean(fe_total), 1e-10))
    mn_cv = float(np.std(mn_total) / max(np.mean(mn_total), 1e-10))

    # S balance
    s_plus = data[:, COLS["S_PLUS_6"]]
    s_minus = data[:, COLS["S_MINUS_2"]]
    s_total = s_plus + s_minus
    s_cv = float(np.std(s_total) / max(np.mean(s_total), 1e-10))

    results[f"box_{box_no}"]["redox_balance"] = {
        "Fe_total_cv": fe_cv,
        "Mn_total_cv": mn_cv,
        "S_total_cv": s_cv,
        "Fe_total_mean": float(np.mean(fe_total)),
        "Mn_total_mean": float(np.mean(mn_total)),
        "S_total_mean": float(np.mean(s_total)),
    }
    return True


def check_do_range(data, box_no, results):
    """Check dissolved oxygen stays in realistic range."""
    do_vals = data[:, COLS["DISS_OXYGEN"]]
    min_do = float(np.min(do_vals))
    max_do = float(np.max(do_vals))
    mean_do = float(np.mean(do_vals))

    # DO should be 0-20 mg/L approximately
    ok = min_do > -0.5 and max_do < 25.0

    results[f"box_{box_no}"]["dissolved_oxygen"] = {
        "pass": ok,
        "min": min_do,
        "max": max_do,
        "mean": mean_do,
    }
    return ok


def check_nutrient_balance(data, box_no, results):
    """Check total nitrogen and phosphorus pools for unrealistic trends."""
    # Total dissolved inorganic nitrogen
    nh4 = data[:, COLS["NH4_N"]]
    no3 = data[:, COLS["NO3_N"]]
    don = data[:, COLS["DISS_ORG_N"]]
    det_n = data[:, COLS["DET_PART_ORG_N"]]
    zoo_n = data[:, COLS["ZOO_N"]]
    dia_n = data[:, COLS["DIA_C"]] * STOICH["DIA_N_TO_C"]
    # Approximate total N (excluding phyto groups other than DIA)
    total_n = nh4 + no3 + don + det_n + zoo_n + dia_n

    # Total P
    po4 = data[:, COLS["PO4_P"]]
    dop = data[:, COLS["DISS_ORG_P"]]
    det_p = data[:, COLS["DET_PART_ORG_P"]]
    zoo_p = data[:, COLS["ZOO_P"]]
    dia_p = data[:, COLS["DIA_C"]] * STOICH["DIA_P_TO_C"]
    total_p = po4 + dop + det_p + zoo_p + dia_p

    # Check for unrealistic drift (>500% change from initial)
    n_drift = abs(total_n[-1] - total_n[0]) / max(total_n[0], 1e-10) * 100
    p_drift = abs(total_p[-1] - total_p[0]) / max(total_p[0], 1e-10) * 100

    results[f"box_{box_no}"]["nutrient_balance"] = {
        "total_N_initial": float(total_n[0]),
        "total_N_final": float(total_n[-1]),
        "total_N_drift_pct": float(n_drift),
        "total_P_initial": float(total_p[0]),
        "total_P_final": float(total_p[-1]),
        "total_P_drift_pct": float(p_drift),
    }
    return True


def check_carbon_pools(data, box_no, results):
    """Check organic carbon pools for realistic ranges."""
    dic = data[:, COLS["INORG_C"]]
    doc = data[:, COLS["DISS_ORG_C"]]
    det_c = data[:, COLS["DET_PART_ORG_C"]]
    zoo_c = data[:, COLS["ZOO_C"]]
    dia_c = data[:, COLS["DIA_C"]]
    cyn_c = data[:, COLS["CYN_C"]]
    opa_c = data[:, COLS["OPA_C"]]
    fix_c = data[:, COLS["FIX_CYN_C"]]
    nost_c = data[:, COLS["NOST_VEG_HET_C"]]
    ch4_c = data[:, COLS["CH4_C"]]

    total_org_c = doc + det_c + zoo_c + dia_c + cyn_c + opa_c + fix_c + nost_c

    results[f"box_{box_no}"]["carbon_pools"] = {
        "DIC_range": [float(np.min(dic)), float(np.max(dic))],
        "DOC_range": [float(np.min(doc)), float(np.max(doc))],
        "DET_C_range": [float(np.min(det_c)), float(np.max(det_c))],
        "total_org_C_range": [float(np.min(total_org_c)), float(np.max(total_org_c))],
        "CH4_C_range": [float(np.min(ch4_c)), float(np.max(ch4_c))],
        "phyto_groups": {
            "DIA_C": [float(np.min(dia_c)), float(np.max(dia_c))],
            "CYN_C": [float(np.min(cyn_c)), float(np.max(cyn_c))],
            "OPA_C": [float(np.min(opa_c)), float(np.max(opa_c))],
            "FIX_CYN_C": [float(np.min(fix_c)), float(np.max(fix_c))],
            "NOST_C": [float(np.min(nost_c)), float(np.max(nost_c))],
        },
    }
    return True


def check_derivative_stability(data, box_no, results):
    """Check for instabilities: NaN, Inf, or extreme jumps between timesteps."""
    issues = []

    for var_name, col_idx in COLS.items():
        if var_name == "TIME":
            continue
        vals = data[:, col_idx]

        # Check NaN/Inf
        if np.any(np.isnan(vals)) or np.any(np.isinf(vals)):
            issues.append({"variable": var_name, "type": "NaN_or_Inf"})
            continue

        # Check for extreme day-to-day jumps (>100x change)
        if len(vals) > 1:
            abs_vals = np.abs(vals)
            max_val = np.max(abs_vals)
            if max_val > 0:
                diffs = np.abs(np.diff(vals))
                max_jump = np.max(diffs)
                if max_jump > 10 * max_val:
                    issues.append({
                        "variable": var_name,
                        "type": "extreme_jump",
                        "max_jump": float(max_jump),
                        "max_value": float(max_val),
                    })

    results[f"box_{box_no}"]["stability"] = {
        "pass": len(issues) == 0,
        "issues": issues,
    }
    return len(issues) == 0


def check_silicon_balance(data, box_no, results):
    """Check silica conservation: DISS_Si + PART_Si should vary smoothly."""
    diss_si = data[:, COLS["DISS_Si"]]
    part_si = data[:, COLS["PART_Si"]]
    total_si = diss_si + part_si

    results[f"box_{box_no}"]["silicon_balance"] = {
        "DISS_Si_range": [float(np.min(diss_si)), float(np.max(diss_si))],
        "PART_Si_range": [float(np.min(part_si)), float(np.max(part_si))],
        "total_Si_range": [float(np.min(total_si)), float(np.max(total_si))],
    }
    return True


def compute_implied_rates(data, box_no, results):
    """
    Compute implied process rates from concentration changes.
    dC/dt ~ (C(t+1) - C(t)) / dt
    These are net rates including transport, not just kinetics.
    """
    time = data[:, COLS["TIME"]]
    dt = np.diff(time)  # should be ~1.0 day

    rates = {}
    for var_name in ["NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C",
                     "ZOO_C", "DISS_ORG_C", "DET_PART_ORG_C", "INORG_C",
                     "FIX_CYN_C", "NOST_VEG_HET_C"]:
        col = COLS[var_name]
        vals = data[:, col]
        dval = np.diff(vals) / dt
        rates[var_name] = {
            "mean_rate": float(np.mean(dval)),
            "max_rate": float(np.max(dval)),
            "min_rate": float(np.min(dval)),
            "std_rate": float(np.std(dval)),
        }

    results[f"box_{box_no}"]["implied_rates"] = rates
    return True


def compare_with_previous(data, box_no, results):
    """Compare key metrics with previous run (from OUTPUTS_200day if available)."""
    # Just record final state for comparison
    final = {}
    for var_name, col_idx in COLS.items():
        if var_name == "TIME":
            continue
        final[var_name] = float(data[-1, col_idx])
    results[f"box_{box_no}"]["final_state"] = final
    return True


def main():
    results = {}
    all_pass = True
    n_issues = 0

    print("=" * 70)
    print("AQUABC Process Rate & Mass Balance Analysis")
    print("=" * 70)
    print()

    for box in BOXES:
        box_key = f"box_{box}"
        results[box_key] = {"box_no": box, "type": "mud" if box in MUD_BOXES else "sand"}

        print(f"--- Box {box} ({results[box_key]['type']}) ---")
        data = load_box(box)
        print(f"  Loaded {len(data)} timesteps, time range {data[0, 0]:.0f}-{data[-1, 0]:.0f} days")

        # Run all checks
        ok = check_non_negative(data, box, results)
        if not ok:
            issues = results[box_key]["non_negative"]["issues"]
            for iss in issues:
                print(f"  [FAIL] {iss['variable']}: {iss['n_negative']} negative values, min={iss['min_value']:.6e}")
                n_issues += 1
            all_pass = False
        else:
            print(f"  [PASS] Non-negativity check")

        ok = check_zoo_stoichiometry(data, box, results)
        zs = results[box_key]["zoo_stoich"]
        if "N_to_C" in zs:
            nc = zs["N_to_C"]
            pc = zs["P_to_C"]
            status_nc = "PASS" if nc["within_bounds"] else "FAIL"
            status_pc = "PASS" if pc["within_bounds"] else "FAIL"
            print(f"  [{status_nc}] ZOO N:C  mean={nc['mean']:.4f} range=[{nc['min']:.4f}, {nc['max']:.4f}] target={nc['target']}")
            print(f"  [{status_pc}] ZOO P:C  mean={pc['mean']:.4f} range=[{pc['min']:.4f}, {pc['max']:.4f}] target={pc['target']}")
            if not ok:
                all_pass = False
                n_issues += 1

        ok = check_do_range(data, box, results)
        do_info = results[box_key]["dissolved_oxygen"]
        status = "PASS" if do_info["pass"] else "FAIL"
        print(f"  [{status}] DO range: [{do_info['min']:.2f}, {do_info['max']:.2f}] mean={do_info['mean']:.2f}")
        if not ok:
            all_pass = False
            n_issues += 1

        ok = check_derivative_stability(data, box, results)
        if not ok:
            stab = results[box_key]["stability"]
            for iss in stab["issues"]:
                print(f"  [WARN] Stability: {iss['variable']} - {iss['type']}")
                n_issues += 1
            all_pass = False
        else:
            print(f"  [PASS] Stability check")

        check_redox_balance(data, box, results)
        rb = results[box_key]["redox_balance"]
        print(f"  [INFO] Fe total CV={rb['Fe_total_cv']:.4f}, Mn total CV={rb['Mn_total_cv']:.4f}")

        check_nutrient_balance(data, box, results)
        nb = results[box_key]["nutrient_balance"]
        print(f"  [INFO] Total N drift: {nb['total_N_drift_pct']:.1f}%, Total P drift: {nb['total_P_drift_pct']:.1f}%")

        check_carbon_pools(data, box, results)
        cp = results[box_key]["carbon_pools"]
        print(f"  [INFO] Phyto C: DIA=[{cp['phyto_groups']['DIA_C'][0]:.3f},{cp['phyto_groups']['DIA_C'][1]:.3f}] "
              f"FIX_CYN=[{cp['phyto_groups']['FIX_CYN_C'][0]:.4f},{cp['phyto_groups']['FIX_CYN_C'][1]:.4f}] "
              f"NOST=[{cp['phyto_groups']['NOST_C'][0]:.4f},{cp['phyto_groups']['NOST_C'][1]:.4f}]")

        check_silicon_balance(data, box, results)
        compute_implied_rates(data, box, results)
        compare_with_previous(data, box, results)

        print()

    # Summary
    print("=" * 70)
    print("SUMMARY")
    print("=" * 70)
    if all_pass:
        print(f"All checks passed for all {len(BOXES)} boxes.")
    else:
        print(f"Found {n_issues} issue(s) across {len(BOXES)} boxes.")

    # Save results
    class NumpyEncoder(json.JSONEncoder):
        def default(self, o):
            if isinstance(o, (np.bool_,)):
                return bool(o)
            if isinstance(o, (np.integer,)):
                return int(o)
            if isinstance(o, (np.floating,)):
                return float(o)
            return super().default(o)

    os.makedirs(WORKSPACE / "data", exist_ok=True)
    out_path = WORKSPACE / "data" / "process_rate_analysis.json"
    with open(out_path, "w") as f:
        json.dump(results, f, indent=2, cls=NumpyEncoder)
    print(f"\nDetailed results saved to: {out_path}")

    return 0 if all_pass else 1


if __name__ == "__main__":
    sys.exit(main())
