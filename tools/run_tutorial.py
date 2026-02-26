#!/usr/bin/env python3
"""
ESTAS-AQUABC Tutorial Runner
=============================
Automated script that follows the Getting Started Tutorial step-by-step,
performing a complete 30-day simulation from the command line without
requiring the Shiny web interface.

Steps mirrored from docs/Tutorial_Getting_Started.md:
  Step 1:  Dashboard overview (report system status)
  Step 2:  Build the model executable
  Step 3:  Explore / validate input files
  Step 4:  Review parameters (WCONST_04.txt)
  Step 5:  Check initial conditions (INIT_CONC_1.txt)
  Step 6:  Display model options (PELAGIC_MODEL_OPTIONS.txt)
  Step 7:  Configure the simulation (INPUT_30day.txt)
  Step 8:  Run the 30-day simulation
  Step 9:  View results (summary statistics)
  Step 10: Analyse mass balance
  Step 11: Compare with observations

Usage:
    python tools/run_tutorial.py [OPTIONS]

Options:
    --skip-build        Skip Step 2 (use existing ESTAS_II executable)
    --skip-run          Skip Step 8 (use existing output in OUTPUTS_30day/)
    --build-type TYPE   Build type: debug | release | fast  (default: release)
    --compiler FC       Fortran compiler: gfortran | ifort | ifx (default: gfortran)
    --input-file FILE   Input config file (default: INPUT_30day.txt)
    --constants FILE    Constants file (default: WCONST_04.txt)
    --output-dir DIR    Output directory (default: read from input file)
    --executable EXE    Executable name (default: ESTAS_II)
    --verbose           Show detailed output
    --no-color          Disable coloured terminal output
    --help              Show this help message

Example:
    # Full tutorial run with defaults
    python tools/run_tutorial.py

    # Skip build, use existing executable
    python tools/run_tutorial.py --skip-build

    # Use results from a previous run
    python tools/run_tutorial.py --skip-build --skip-run

    # Custom compiler and build type
    python tools/run_tutorial.py --compiler gfortran --build-type debug
"""

from __future__ import annotations

import argparse
import glob
import os
import platform
import re
import shutil
import subprocess
import sys
import textwrap
import time
from pathlib import Path

# ---------------------------------------------------------------------------
# Resolve project root (parent of 'tools/')
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
ROOT = SCRIPT_DIR.parent
INPUTS_DIR = ROOT / "INPUTS"

# ---------------------------------------------------------------------------
# Terminal colours
# ---------------------------------------------------------------------------
_USE_COLOR = True


def _c(code: str, text: str) -> str:
    """Wrap *text* in ANSI colour *code* if colours are enabled."""
    if _USE_COLOR:
        return f"\033[{code}m{text}\033[0m"
    return text


def _green(t: str) -> str:
    return _c("32", t)


def _red(t: str) -> str:
    return _c("31", t)


def _yellow(t: str) -> str:
    return _c("33", t)


def _cyan(t: str) -> str:
    return _c("36", t)


def _bold(t: str) -> str:
    return _c("1", t)


def _dim(t: str) -> str:
    return _c("2", t)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
_step_counter = 0


def banner(title: str) -> None:
    """Print a step banner."""
    global _step_counter
    _step_counter += 1
    width = 60
    line = "=" * width
    print()
    print(_bold(_cyan(line)))
    print(_bold(_cyan(f"  Step {_step_counter}: {title}")))
    print(_bold(_cyan(line)))


def info(msg: str) -> None:
    print(f"  {_green('✓')} {msg}")


def warn(msg: str) -> None:
    print(f"  {_yellow('⚠')} {msg}")


def error(msg: str) -> None:
    print(f"  {_red('✗')} {msg}")


def detail(msg: str) -> None:
    """Verbose-only output."""
    if _VERBOSE:
        print(f"    {_dim(msg)}")


def table(rows: list[tuple[str, str]], indent: int = 4) -> None:
    """Print a simple two-column table."""
    if not rows:
        return
    key_width = max(len(r[0]) for r in rows) + 2
    for key, val in rows:
        print(f"{' ' * indent}{key:<{key_width}} {val}")


_VERBOSE = False


# ---------------------------------------------------------------------------
# Step 1 — System Status
# ---------------------------------------------------------------------------
def step1_system_status(args: argparse.Namespace) -> dict:
    """Report system status (mirrors Dashboard → System Status)."""
    banner("Dashboard Overview — System Status")

    # OS / Python
    os_name = f"{platform.system()} {platform.release()}"
    py_ver = platform.python_version()
    info(f"Operating System: {os_name}")
    info(f"Python: {py_ver}")

    # Fortran compiler
    compiler = args.compiler
    compiler_path = shutil.which(compiler)
    if compiler_path:
        try:
            ver = subprocess.check_output(
                [compiler_path, "--version"], text=True, stderr=subprocess.STDOUT, timeout=5
            ).splitlines()[0]
        except Exception:
            ver = "(version unknown)"
        info(f"Fortran compiler: {compiler} → {compiler_path}")
        detail(ver)
    else:
        warn(f"Fortran compiler '{compiler}' not found on PATH")

    # Executable
    exe_path = ROOT / args.executable
    if exe_path.exists():
        size_mb = exe_path.stat().st_size / (1024 * 1024)
        info(f"Executable: {args.executable} ({size_mb:.1f} MB)")
    else:
        warn(f"Executable '{args.executable}' not found (will build in Step 2)")

    # Input files
    input_files = list(INPUTS_DIR.glob("*"))
    info(f"Input files: {len(input_files)} files in INPUTS/")

    # Input config
    input_cfg = ROOT / args.input_file
    if input_cfg.exists():
        info(f"Input configuration: {args.input_file}")
    else:
        error(f"Input configuration '{args.input_file}' not found!")

    # Constants
    const_path = INPUTS_DIR / args.constants
    if const_path.exists():
        n_lines = sum(1 for _ in open(const_path))
        info(f"Constants file: {args.constants} ({n_lines} lines)")
    else:
        warn(f"Constants file '{args.constants}' not found in INPUTS/")

    # Make
    make_path = shutil.which("make")
    if make_path:
        info(f"GNU Make: {make_path}")
    else:
        warn("GNU Make not found")

    return {
        "compiler_available": compiler_path is not None,
        "executable_exists": exe_path.exists(),
        "input_config_exists": input_cfg.exists(),
        "constants_exists": const_path.exists(),
    }


# ---------------------------------------------------------------------------
# Step 2 — Build the Model
# ---------------------------------------------------------------------------
def step2_build_model(args: argparse.Namespace) -> bool:
    """Compile the Fortran executable (mirrors Model Build page)."""
    banner("Building the Model Executable")

    if args.skip_build:
        exe_path = ROOT / args.executable
        if exe_path.exists():
            info(f"Skipping build — using existing '{args.executable}'")
            return True
        else:
            error(f"--skip-build specified but '{args.executable}' not found!")
            return False

    compiler = args.compiler
    build_type = args.build_type

    info(f"Compiler:   {compiler}")
    info(f"Build type: {build_type}")

    # Determine target name
    fc_short_map = {"gfortran": "gf", "ifort": "ifort", "ifx": "ifx"}
    fc_short = fc_short_map.get(compiler, compiler)
    auto_name = f"ESTAS_II_{fc_short}_{build_type}"
    info(f"Target executable: {auto_name}")

    # Run make build-named
    print()
    cmd = ["make", f"FC={compiler}", f"BUILD_TYPE={build_type}", "build-named"]
    info(f"Running: {' '.join(cmd)}")
    print(_dim("    " + "-" * 50))

    start = time.time()
    proc = subprocess.run(
        cmd,
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    elapsed = time.time() - start

    if _VERBOSE:
        for line in proc.stdout.splitlines():
            print(f"    {_dim(line)}")

    if proc.returncode == 0:
        exe = ROOT / auto_name
        if exe.exists():
            size_mb = exe.stat().st_size / (1024 * 1024)
            info(f"Build succeeded in {elapsed:.1f}s → {auto_name} ({size_mb:.1f} MB)")
            # If the user specified a different executable name, create a copy
            if args.executable != auto_name and args.executable != "ESTAS_II":
                shutil.copy2(exe, ROOT / args.executable)
                info(f"Copied to {args.executable}")
            return True
        else:
            warn("Make succeeded but executable not found — checking default name")
            default = ROOT / "ESTAS_II"
            if default.exists():
                info(f"Found ESTAS_II (default name)")
                return True
            error("Executable not created")
            return False
    else:
        error(f"Build failed (rc={proc.returncode})")
        # Show last 20 lines of output
        for line in proc.stdout.splitlines()[-20:]:
            print(f"    {_red(line)}")
        return False


# ---------------------------------------------------------------------------
# Step 3 — Explore Input Files
# ---------------------------------------------------------------------------
def step3_explore_inputs(args: argparse.Namespace) -> None:
    """List and validate key input files (mirrors Input Files page)."""
    banner("Exploring Input Files")

    # Category mapping
    categories = {
        "Core Configuration": [
            "PELAGIC_INPUTS.txt",
            "PELAGIC_MODEL_OPTIONS.txt",
            "PELAGIC_OUTPUT_INFORMATION_FILE.txt",
        ],
        "Geometry": [
            "ADVECTIVE_LINKS.txt",
            "DISPERSIVE_LINKS.txt",
        ],
        "Constants": [
            "WCONST_04.txt",
            "EXTRA_WCONST.txt",
        ],
        "Initial Conditions": [
            "INIT_CONC_1.txt",
            "INIT_CONC_2.txt",
        ],
        "Forcing Timeseries": [
            "TEMP_TS.txt",
            "SALT_TS.txt",
            "FLOW_TS.txt",
            "SOLAR_RAD_TS.txt",
            "WIND_SPEED_TS.txt",
            "AIR_TEMP_TS.txt",
            "CLOUD_COVER_TS.txt",
            "EVAPORATION_TS.txt",
            "BOUNDARY_FLOW_TS.txt",
        ],
        "Sediment": [
            "PRESCRIBED_SEDIMENT_FLUXES.txt",
            "PRESCRIBED_SEDIMENT_FLUXES_HYPOXIA.txt",
        ],
    }

    total_ok = 0
    total_missing = 0
    for cat, files in categories.items():
        print(f"\n  {_bold(cat)}:")
        for fname in files:
            fpath = INPUTS_DIR / fname
            if fpath.exists():
                size_kb = fpath.stat().st_size / 1024
                lines = sum(1 for _ in open(fpath))
                print(f"    {_green('✓')} {fname:<45s} {size_kb:>7.1f} KB  {lines:>5d} lines")
                total_ok += 1
            else:
                print(f"    {_red('✗')} {fname:<45s} MISSING")
                total_missing += 1

    # Bathymetry files
    bathy = sorted(INPUTS_DIR.glob("BATHYMETRY_*.txt"))
    print(f"\n  {_bold('Bathymetry')}:")
    print(f"    {_green('✓')} BATHYMETRY_1.txt – BATHYMETRY_{len(bathy)}.txt  ({len(bathy)} files)")

    # Forcing concentration timeseries
    forc = sorted(INPUTS_DIR.glob("FORC_TS_*.txt"))
    print(f"\n  {_bold('Boundary Forcing')}:")
    print(f"    {_green('✓')} FORC_TS_1.txt – FORC_TS_{len(forc)}.txt  ({len(forc)} files)")

    # Settling velocity timeseries
    settle = sorted(INPUTS_DIR.glob("SETTLING_VELOCITY_TS_*.txt"))
    if settle:
        print(f"    {_green('✓')} SETTLING_VELOCITY_TS (x{len(settle)})")

    # Total
    all_inputs = list(INPUTS_DIR.glob("*.txt"))
    print(f"\n  Total: {len(all_inputs)} files in INPUTS/")
    if total_missing > 0:
        warn(f"{total_missing} required file(s) missing")
    else:
        info(f"All {total_ok} key files present")


# ---------------------------------------------------------------------------
# Step 4 — Review Parameters
# ---------------------------------------------------------------------------
def step4_review_parameters(args: argparse.Namespace) -> None:
    """Parse and summarise WCONST_04.txt (mirrors Parameters page)."""
    banner("Reviewing Parameters")

    const_path = INPUTS_DIR / args.constants
    if not const_path.exists():
        error(f"Constants file not found: {const_path}")
        return

    # Parse the constants file
    # Format:  <index>  <name>  <value>  !  <index>  <description>
    params: list[dict] = []
    with open(const_path) as fh:
        for line in fh:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            # Try to parse: index, name, value, rest
            match = re.match(
                r"^\s*(\d+)\s+(\S+)\s+([-+]?\d*\.?\d+(?:[eEdD][-+]?\d+)?)\s*(!.*)?$",
                line,
            )
            if match:
                idx = int(match.group(1))
                name = match.group(2)
                val = float(match.group(3).replace("D", "E").replace("d", "e"))
                desc = match.group(4).lstrip("! ").strip() if match.group(4) else ""
                params.append({"idx": idx, "name": name, "value": val, "desc": desc})

    info(f"Loaded {len(params)} constants from {args.constants}")

    # Category ranges (from tutorial)
    cat_ranges = [
        ("General",                    1,  17),
        ("Diatoms",                   18,  54),
        ("Non-fixing Cyanobacteria",  55,  89),
        ("Fixing Cyanobacteria",      90, 124),
        ("Other Phytoplankton",      125, 159),
        ("Zooplankton",              160, 192),
        ("Detritus",                 193, 218),
        ("Dissolved Organics",       219, 234),
        ("Nitrification",           235, 250),
        ("Redox Chemistry",         251, 272),
        ("Methane",                  273, 283),
        ("Settling",                 284, 297),
        ("pH Effects",               298, 305),
        ("Nostocales",               306, 323),
    ]

    print()
    print(f"  {'Category':<30s} {'Range':>10s}  {'Count':>5s}")
    print(f"  {'-' * 30} {'-' * 10}  {'-' * 5}")
    for cat_name, lo, hi in cat_ranges:
        count = sum(1 for p in params if lo <= p["idx"] <= hi)
        print(f"  {cat_name:<30s} {lo:>4d}–{hi:<4d}  {count:>5d}")

    if _VERBOSE and params:
        print(f"\n  {_bold('Sample parameters (first 10):')}")
        print(f"    {'#':>4s}  {'Name':<25s}  {'Value':>12s}  Description")
        print(f"    {'─' * 4}  {'─' * 25}  {'─' * 12}  {'─' * 40}")
        for p in params[:10]:
            desc_short = p["desc"][:40] if p["desc"] else ""
            print(f"    {p['idx']:>4d}  {p['name']:<25s}  {p['value']:>12.4f}  {desc_short}")


# ---------------------------------------------------------------------------
# Step 5 — Check Initial Conditions
# ---------------------------------------------------------------------------
def step5_initial_conditions(args: argparse.Namespace) -> None:
    """Parse and display initial conditions (mirrors Initial Cond. page)."""
    banner("Checking Initial Conditions")

    ic_file = INPUTS_DIR / "INIT_CONC_1.txt"
    if not ic_file.exists():
        error("INIT_CONC_1.txt not found in INPUTS/")
        return

    # State variable names in order (36 variables)
    var_names = [
        "NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C",
        "ZOO_C", "ZOO_N", "ZOO_P", "DET_PART_ORG_C", "DET_PART_ORG_N",
        "DET_PART_ORG_P", "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P",
        "CYN_C", "OPA_C", "DISS_Si", "PART_Si", "FIX_CYN_C",
        "INORG_C", "TOT_ALK", "FE_II", "FE_III", "MN_II", "MN_IV",
        "CA", "MG", "S_PLUS_6", "S_MINUS_2", "CH4_C",
        "NOST_VEG_HET_C", "AKI_C",
        "SEC_METAB_DIA", "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN",
        "SEC_METAB_NOST",
    ]

    # Read values — file contains concentration values, one per line
    values: list[float] = []
    with open(ic_file) as fh:
        for line in fh:
            line = line.strip()
            if not line or line.startswith("#") or line.startswith("!"):
                continue
            try:
                values.append(float(line.split()[0]))
            except (ValueError, IndexError):
                continue

    info(f"Loaded {len(values)} values from INIT_CONC_1.txt")
    info(f"Expected {len(var_names)} state variables")

    # Display grouped by category
    categories = {
        "Nutrients":            ["NH4_N", "NO3_N", "PO4_P", "DISS_Si"],
        "Dissolved Gases":      ["DISS_OXYGEN"],
        "Phytoplankton":        ["DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "NOST_VEG_HET_C", "AKI_C"],
        "Zooplankton":          ["ZOO_C", "ZOO_N", "ZOO_P"],
        "Particulate Organics": ["DET_PART_ORG_C", "DET_PART_ORG_N", "DET_PART_ORG_P", "PART_Si"],
        "Dissolved Organics":   ["DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P"],
        "Carbonate System":     ["INORG_C", "TOT_ALK"],
        "Metals":               ["FE_II", "FE_III", "MN_II", "MN_IV", "CA", "MG"],
        "Sulphur":              ["S_PLUS_6", "S_MINUS_2"],
        "Allelopathy":          ["SEC_METAB_DIA", "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST"],
        "Other":                ["CH4_C"],
    }

    # Build name→value map
    val_map: dict[str, float] = {}
    for i, name in enumerate(var_names):
        if i < len(values):
            val_map[name] = values[i]

    for cat_name, var_list in categories.items():
        print(f"\n  {_bold(cat_name)}:")
        for vn in var_list:
            v = val_map.get(vn, float("nan"))
            if v != v:  # NaN check
                print(f"    {_red('?')} {vn:<25s}  {'N/A':>12s}")
            else:
                print(f"    {_green('•')} {vn:<25s}  {v:>12.6f}")


# ---------------------------------------------------------------------------
# Step 6 — Display Model Options
# ---------------------------------------------------------------------------
def step6_model_options(args: argparse.Namespace) -> None:
    """Display model options/switches (mirrors Model Options page)."""
    banner("Displaying Model Options")

    opts_file = INPUTS_DIR / "PELAGIC_MODEL_OPTIONS.txt"
    if not opts_file.exists():
        error("PELAGIC_MODEL_OPTIONS.txt not found in INPUTS/")
        return

    # Read the options file
    lines = opts_file.read_text().splitlines()
    info(f"Loaded {len(lines)} lines from PELAGIC_MODEL_OPTIONS.txt")

    # Parse option lines: look for numeric values with descriptions
    options: list[tuple[str, str]] = []
    for i, line in enumerate(lines):
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or stripped.startswith("!"):
            # Could be a comment describing the next option
            continue
        # Try to parse as a value line
        parts = stripped.split()
        if parts and re.match(r"^[-+]?\d+\.?\d*$", parts[0]):
            # Look for preceding comment as description
            desc = ""
            for j in range(i - 1, max(i - 3, -1), -1):
                cline = lines[j].strip()
                if cline.startswith("#") or cline.startswith("!"):
                    desc = cline.lstrip("#! ").strip()
                    break
            val = parts[0]
            if desc:
                options.append((desc, val))

    if options:
        print(f"\n  {'Option':<55s} {'Value':>8s}")
        print(f"  {'-' * 55} {'-' * 8}")
        for desc, val in options[:30]:  # Show first 30
            desc_trunc = desc[:55] if len(desc) > 55 else desc
            print(f"  {desc_trunc:<55s} {val:>8s}")
        if len(options) > 30:
            print(f"  ... and {len(options) - 30} more options")
    else:
        warn("Could not parse option values — displaying raw content")
        for line in lines[:20]:
            print(f"    {line}")

    # Also check EXTRA_WCONST.txt
    extra = INPUTS_DIR / "EXTRA_WCONST.txt"
    if extra.exists():
        n = sum(1 for l in open(extra) if l.strip() and not l.strip().startswith("#"))
        info(f"Extra constants file: EXTRA_WCONST.txt ({n} entries)")


# ---------------------------------------------------------------------------
# Step 7 — Configure Simulation
# ---------------------------------------------------------------------------
def step7_configure_simulation(args: argparse.Namespace) -> dict:
    """Parse and display simulation configuration (mirrors Model Config)."""
    banner("Configuring the Simulation")

    input_file = ROOT / args.input_file
    if not input_file.exists():
        error(f"Input file not found: {args.input_file}")
        return {}

    # Parse INPUT_30day.txt
    content = input_file.read_text()
    lines = content.splitlines()

    config: dict[str, str] = {}
    key_map = {
        "BASE_YEAR": "base_year",
        "SIMULATION_START": "sim_start",
        "SIMULATION_END": "sim_end",
        "NUM_REPEATS": "num_repeats",
        "TIME_STEPS_PER_DAY": "steps_per_day",
        "PRINT_INTERVAL": "print_interval",
        "PELAGIC MODEL INPUT FOLDER": "input_folder",
        "PELAGIC MODEL INPUT FILE": "input_file_ref",
        "PELAGIC MODEL OUTPUT FOLDER": "output_folder",
        "RESUSPENSION_OPTION": "resuspension",
        "MODEL_SEDIMENTS": "sediment_model",
        "NUM_PRESCRIBED_SEDIMENT_FLUX_SETS": "num_sed_sets",
    }

    i = 0
    while i < len(lines):
        line = lines[i].strip()
        for keyword, cfg_key in key_map.items():
            if keyword in line and line.startswith("#"):
                # Next non-comment, non-empty line is the value
                j = i + 1
                while j < len(lines):
                    val_line = lines[j].strip()
                    if val_line and not val_line.startswith("#"):
                        config[cfg_key] = val_line
                        break
                    j += 1
                break
        i += 1

    info(f"Configuration loaded from: {args.input_file}")
    print()

    # Display configuration table
    base_year = config.get("base_year", "?")
    sim_start = config.get("sim_start", "?")
    sim_end = config.get("sim_end", "?")
    steps = config.get("steps_per_day", "?")
    print_int = config.get("print_interval", "?")
    output_folder = config.get("output_folder", "OUTPUTS/").rstrip("/")
    resuspension = config.get("resuspension", "?")
    sediment = config.get("sediment_model", "?")

    # Calculate duration
    try:
        duration = float(sim_end) - float(sim_start)
        duration_str = f"{duration:.0f} days"
    except (ValueError, TypeError):
        duration_str = "?"

    # Calculate timestep
    try:
        dt_minutes = 24 * 60 / int(steps)
        dt_str = f"{dt_minutes:.0f} minutes"
    except (ValueError, TypeError, ZeroDivisionError):
        dt_str = "?"

    # Calculate output frequency
    try:
        out_freq = int(print_int) * dt_minutes
        out_str = f"every {out_freq:.0f} min ({print_int} steps)"
    except (ValueError, TypeError):
        out_str = "?"

    display_rows = [
        ("Base Year:", str(base_year)),
        ("Simulation Start:", f"Day {sim_start}"),
        ("Simulation End:", f"Day {sim_end}"),
        ("Duration:", duration_str),
        ("Steps/Day:", str(steps)),
        ("Time Step:", dt_str),
        ("Print Interval:", out_str),
        ("Output Folder:", output_folder + "/"),
        ("Resuspension:", f"{'Disabled' if resuspension.strip() == '0' else 'Enabled'} ({resuspension.strip()})"),
        ("Sediment Model:", f"{'Off' if sediment.strip() == '0' else 'On'} ({sediment.strip()})"),
    ]

    table(display_rows)

    # Command preview
    exe = args.executable
    cmd_parts = [f"./{exe}", args.input_file]
    if args.constants:
        cmd_parts.append(args.constants)
    cmd_str = " ".join(cmd_parts)

    print()
    info(f"Command preview: {_bold(cmd_str)}")

    return {"output_folder": output_folder, "sim_start": sim_start, "sim_end": sim_end}


# ---------------------------------------------------------------------------
# Step 8 — Run the Simulation
# ---------------------------------------------------------------------------
def step8_run_simulation(args: argparse.Namespace, config: dict) -> bool:
    """Execute the 30-day model run (mirrors Run Model / Quick Run)."""
    banner("Running the 30-Day Simulation")

    if args.skip_run:
        output_folder = config.get("output_folder", "OUTPUTS_30day")
        out_dir = ROOT / output_folder
        out_files = list(out_dir.glob("PELAGIC_BOX_*.out")) if out_dir.exists() else []
        if out_files:
            info(f"Skipping run — using existing output in {output_folder}/")
            info(f"Found {len(out_files)} output files")
            return True
        else:
            warn(f"--skip-run specified but no output files found in {output_folder}/")
            return False

    # Check executable
    exe_path = ROOT / args.executable
    if not exe_path.exists():
        # Try to find any ESTAS executable
        candidates = sorted(ROOT.glob("ESTAS_II*"))
        candidates = [c for c in candidates if c.is_file() and os.access(c, os.X_OK)]
        if candidates:
            exe_path = candidates[0]
            warn(f"'{args.executable}' not found, using '{exe_path.name}'")
        else:
            error("No executable found! Run with --skip-build removed to compile first.")
            return False

    # Validate constants file
    const_file = args.constants
    const_path = INPUTS_DIR / const_file
    if const_path.exists():
        n_const = sum(
            1
            for l in open(const_path)
            if l.strip() and not l.strip().startswith("#") and not l.strip().startswith("!")
        )
        info(f"Constants file validated: {const_file} ({n_const} constants)")
    else:
        warn(f"Constants file '{const_file}' not found — model will use defaults")
        const_file = ""

    # Build command
    cmd = [f"./{exe_path.name}", args.input_file]
    if const_file:
        cmd.append(const_file)

    cmd_display = " ".join(cmd)
    info(f"Command: {_bold(cmd_display)}")
    print()

    # Run
    print(f"  {_yellow('▸')} Starting model execution...")
    print(f"    {_dim('-' * 50)}")

    start_time = time.time()
    output_folder = config.get("output_folder", "OUTPUTS_30day")
    out_dir = ROOT / output_folder

    proc = subprocess.Popen(
        cmd,
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1,
    )

    # Stream output with progress tracking
    line_count = 0
    last_progress = time.time()
    spinner = ["|", "/", "-", "\\"]
    spin_idx = 0

    try:
        for line in proc.stdout:
            line_count += 1
            if _VERBOSE:
                print(f"    {_dim(line.rstrip())}")

            # Periodic progress update
            now = time.time()
            if now - last_progress >= 5.0:
                elapsed = now - start_time
                # Count output files
                if out_dir.exists():
                    out_files = len(list(out_dir.glob("*.out")))
                    out_size = sum(f.stat().st_size for f in out_dir.glob("*.out")) / 1024
                else:
                    out_files = 0
                    out_size = 0

                spin_char = spinner[spin_idx % len(spinner)]
                spin_idx += 1
                elapsed_str = _format_elapsed(elapsed)
                print(
                    f"\r    {spin_char} Running... Elapsed: {elapsed_str} | "
                    f"{output_folder}/: {out_files} files ({out_size:.0f} KB)",
                    end="",
                    flush=True,
                )
                last_progress = now

    except KeyboardInterrupt:
        proc.terminate()
        print()
        warn("Simulation interrupted by user")
        return False

    proc.wait()
    elapsed = time.time() - start_time

    print()  # Clear progress line
    print(f"    {_dim('-' * 50)}")

    if proc.returncode == 0:
        # Report results
        if out_dir.exists():
            out_files = list(out_dir.glob("PELAGIC_BOX_*.out"))
            total_size = sum(f.stat().st_size for f in out_dir.iterdir()) / 1024
            info(f"Model completed successfully in {_format_elapsed(elapsed)}")
            info(f"Output: {len(out_files)} box files, {total_size:.0f} KB total in {output_folder}/")
        else:
            info(f"Model completed in {_format_elapsed(elapsed)} (rc=0)")
        return True
    else:
        error(f"Model failed with return code {proc.returncode}")
        return False


def _format_elapsed(seconds: float) -> str:
    """Format elapsed seconds as Xm Ys."""
    m = int(seconds // 60)
    s = int(seconds % 60)
    if m > 0:
        return f"{m}m {s}s"
    return f"{s}s"


# ---------------------------------------------------------------------------
# Step 9 — View Results
# ---------------------------------------------------------------------------
def step9_view_results(args: argparse.Namespace, config: dict) -> None:
    """Display summary statistics of model output (mirrors Plots page)."""
    banner("Viewing Results — Output Summary")

    output_folder = config.get("output_folder", "OUTPUTS_30day")
    out_dir = ROOT / output_folder

    if not out_dir.exists():
        error(f"Output directory not found: {output_folder}/")
        return

    # List output files
    all_files = sorted(out_dir.iterdir())
    info(f"Output directory: {output_folder}/")
    print()
    print(f"  {'File':<35s} {'Size':>10s}  {'Lines':>7s}")
    print(f"  {'-' * 35} {'-' * 10}  {'-' * 7}")

    for f in all_files:
        if f.is_file():
            size_kb = f.stat().st_size / 1024
            try:
                lines = sum(1 for _ in open(f))
            except Exception:
                lines = 0
            print(f"  {f.name:<35s} {size_kb:>8.1f} KB  {lines:>7d}")

    # Parse a sample box output for statistics
    box_files = sorted(out_dir.glob("PELAGIC_BOX_*.out"))
    if not box_files:
        warn("No PELAGIC_BOX_*.out files found")
        return

    sample = box_files[0]
    info(f"\nSample analysis: {sample.name}")

    # Read header and data
    with open(sample) as fh:
        header = fh.readline().split()
        data_lines = fh.readlines()

    if not data_lines:
        warn("No data rows in output file")
        return

    # All state variables grouped by category
    var_groups: list[tuple[str, list[tuple[str, str]]]] = [
        ("Nutrients", [
            ("NH4_N",       "mg N/L"),
            ("NO3_N",       "mg N/L"),
            ("PO4_P",       "mg P/L"),
            ("DISS_Si",     "mg Si/L"),
        ]),
        ("Dissolved Gases", [
            ("DISS_OXYGEN", "mg O2/L"),
        ]),
        ("Phytoplankton", [
            ("DIA_C",           "mg C/L"),
            ("CYN_C",           "mg C/L"),
            ("OPA_C",           "mg C/L"),
            ("FIX_CYN_C",       "mg C/L"),
            ("NOST_VEG_HET_C",  "mg C/L"),
            ("AKI_C",           "mg C/L"),
        ]),
        ("Zooplankton", [
            ("ZOO_C",  "mg C/L"),
            ("ZOO_N",  "mg N/L"),
            ("ZOO_P",  "mg P/L"),
        ]),
        ("Particulate Organics", [
            ("DET_PART_ORG_C",  "mg C/L"),
            ("DET_PART_ORG_N",  "mg N/L"),
            ("DET_PART_ORG_P",  "mg P/L"),
            ("PART_Si",         "mg Si/L"),
        ]),
        ("Dissolved Organics", [
            ("DISS_ORG_C",  "mg C/L"),
            ("DISS_ORG_N",  "mg N/L"),
            ("DISS_ORG_P",  "mg P/L"),
        ]),
        ("Carbonate System", [
            ("INORG_C",    "mg C/L"),
            ("TOT_ALK",    "meq/L"),
        ]),
        ("Metals", [
            ("FE_II",  "mg/L"),
            ("FE_III", "mg/L"),
            ("MN_II",  "mg/L"),
            ("MN_IV",  "mg/L"),
            ("CA",     "mg/L"),
            ("MG",     "mg/L"),
        ]),
        ("Sulphur & Methane", [
            ("S_PLUS_6",   "mg S/L"),
            ("S_MINUS_2",  "mg S/L"),
            ("CH4_C",      "mg C/L"),
        ]),
        ("Allelopathy", [
            ("SEC_METAB_DIA",        "–"),
            ("SEC_METAB_NOFIX_CYN",  "–"),
            ("SEC_METAB_FIX_CYN",    "–"),
            ("SEC_METAB_NOST",       "–"),
        ]),
    ]

    # Build flat list of variable names and units
    all_vars: list[str] = []
    units: dict[str, str] = {}
    var_to_group: dict[str, str] = {}
    for grp_name, members in var_groups:
        for vname, unit in members:
            all_vars.append(vname)
            units[vname] = unit
            var_to_group[vname] = grp_name

    # Find column indices
    col_indices: dict[str, int] = {}
    for vname in all_vars:
        if vname in header:
            col_indices[vname] = header.index(vname)

    if not col_indices:
        warn("Could not match variable names in output header")
        return

    # Collect data
    import statistics as stat_mod

    print(f"\n  Time range: {data_lines[0].split()[0]} – {data_lines[-1].split()[0]} days")
    print(f"  Output rows: {len(data_lines)}")

    # Pre-compute all values
    var_vals: dict[str, list[float]] = {}
    for vname, col_idx in col_indices.items():
        vals = []
        for line in data_lines:
            parts = line.split()
            if col_idx < len(parts):
                try:
                    vals.append(float(parts[col_idx]))
                except ValueError:
                    pass
        var_vals[vname] = vals

    # Print grouped output
    for grp_name, members in var_groups:
        # Check if any vars from this group are in the output
        grp_vars = [vn for vn, _ in members if vn in col_indices]
        if not grp_vars:
            continue

        print(f"\n  {_bold(grp_name)}")
        print(f"  {'Variable':<25s} {'Min':>10s} {'Max':>10s} {'Mean':>10s} {'StdDev':>10s}  {'Unit'}")
        print(f"  {'-' * 25} {'-' * 10} {'-' * 10} {'-' * 10} {'-' * 10}  {'-' * 8}")

        for vname, _ in members:
            vals = var_vals.get(vname, [])
            if not vals:
                continue
            vmin = min(vals)
            vmax = max(vals)
            vmean = stat_mod.mean(vals)
            vstd = stat_mod.stdev(vals) if len(vals) > 1 else 0.0
            unit = units.get(vname, "")
            print(f"  {vname:<25s} {vmin:>10.4f} {vmax:>10.4f} {vmean:>10.4f} {vstd:>10.4f}  {unit}")

    matched = len(col_indices)
    total = len(all_vars)
    info(f"\n{matched}/{total} state variables found in output")


# ---------------------------------------------------------------------------
# Step 10 — Mass Balance Analysis
# ---------------------------------------------------------------------------
def step10_mass_balance(args: argparse.Namespace, config: dict) -> None:
    """Analyse mass balance from output (mirrors Mass Balance page)."""
    banner("Analysing Mass Balance")

    output_folder = config.get("output_folder", "OUTPUTS_30day")
    out_dir = ROOT / output_folder
    mb_file = out_dir / "MASS_BALANCES.out"

    if not mb_file.exists():
        warn(f"MASS_BALANCES.out not found in {output_folder}/")
        warn("Mass balance analysis requires output from a model run")

        # Still try to compute basic mass balance from box outputs
        box_files = sorted(out_dir.glob("PELAGIC_BOX_*.out"))
        if not box_files:
            return

        info("Computing simplified mass balance from box output files...")
        _simplified_mass_balance(box_files, config)
        return

    # Parse mass balance file
    content = mb_file.read_text()
    if not content.strip():
        warn("MASS_BALANCES.out is empty — mass balance was not computed during this run")
        box_files = sorted(out_dir.glob("PELAGIC_BOX_*.out"))
        if box_files:
            info("Computing simplified mass balance from box output files...")
            _simplified_mass_balance(box_files, config)
        return

    lines = content.splitlines()
    info(f"MASS_BALANCES.out: {len(lines)} lines")

    # Display first rows
    if _VERBOSE:
        for line in lines[:20]:
            print(f"    {_dim(line)}")
    else:
        for line in lines[:5]:
            print(f"    {_dim(line)}")


def _simplified_mass_balance(box_files: list[Path], config: dict) -> None:
    """Compute a simplified mass balance by comparing first and last row
    in each box output file for key element pools."""

    # Element to variable mapping
    elements = {
        "Nitrogen (N)": ["NH4_N", "NO3_N", "ZOO_N", "DET_PART_ORG_N", "DISS_ORG_N"],
        "Carbon (C)": ["DIA_C", "CYN_C", "OPA_C", "FIX_CYN_C", "ZOO_C",
                        "DET_PART_ORG_C", "DISS_ORG_C", "INORG_C", "CH4_C"],
        "Phosphorus (P)": ["PO4_P", "ZOO_P", "DET_PART_ORG_P", "DISS_ORG_P"],
        "Silicon (Si)": ["DISS_Si", "PART_Si"],
    }

    print()
    print(f"  {'Element':<20s} {'Initial':>12s} {'Final':>12s} {'Change':>12s} {'Rel.Err':>10s}")
    print(f"  {'-' * 20} {'-' * 12} {'-' * 12} {'-' * 12} {'-' * 10}")

    # Use first box file as representative
    sample = box_files[0]
    with open(sample) as fh:
        header = fh.readline().split()
        first_line = fh.readline()
        # Read to last line
        last_line = first_line
        for line in fh:
            if line.strip():
                last_line = line

    first_vals = first_line.split()
    last_vals = last_line.split()

    for elem_name, var_list in elements.items():
        initial = 0.0
        final = 0.0
        for vn in var_list:
            if vn in header:
                idx = header.index(vn)
                try:
                    initial += float(first_vals[idx])
                    final += float(last_vals[idx])
                except (IndexError, ValueError):
                    pass
        change = final - initial
        rel_err = abs(change) / max(abs(initial), 1e-10) * 100
        print(f"  {elem_name:<20s} {initial:>12.4f} {final:>12.4f} {change:>+12.4f} {rel_err:>9.2f}%")

    print()
    info(f"Based on {sample.name} (single-box simplified analysis)")
    info("For full multi-box mass balance, use the Shiny app Mass Balance page")


# ---------------------------------------------------------------------------
# Step 11 — Compare with Observations
# ---------------------------------------------------------------------------
def step11_observations(args: argparse.Namespace) -> None:
    """List available observation files (mirrors Observations page)."""
    banner("Comparing with Observations")

    obs_dir = ROOT / "OBSERVATIONS"
    if not obs_dir.exists():
        warn("OBSERVATIONS/ directory not found")
        return

    # Scan for observation files
    obs_files = sorted(obs_dir.iterdir())
    obs_files = [f for f in obs_files if f.is_file()]

    info(f"Found {len(obs_files)} observation file(s) in OBSERVATIONS/")
    print()

    xlsx_files = [f for f in obs_files if f.suffix in (".xlsx", ".xls")]
    dates_files = [f for f in obs_files if f.suffix == ".dates"]
    other_files = [f for f in obs_files if f not in xlsx_files and f not in dates_files]

    if xlsx_files:
        print(f"  {_bold('Excel files:')}")
        for f in xlsx_files:
            size_kb = f.stat().st_size / 1024
            print(f"    {_green('•')} {f.name:<50s} {size_kb:>7.1f} KB")

    if dates_files:
        print(f"\n  {_bold('Date index files:')}")
        for f in dates_files:
            size_kb = f.stat().st_size / 1024
            print(f"    {_green('•')} {f.name:<50s} {size_kb:>7.1f} KB")

    if other_files:
        print(f"\n  {_bold('Other files:')}")
        for f in other_files:
            size_kb = f.stat().st_size / 1024
            print(f"    {_green('•')} {f.name:<50s} {size_kb:>7.1f} KB")

    print()
    info("Use the Shiny app Observations page for interactive comparison")
    info("Or use pandas/openpyxl to load and compare data programmatically")


# ---------------------------------------------------------------------------
# Final Summary
# ---------------------------------------------------------------------------
def print_summary(results: dict) -> None:
    """Print a final summary of the tutorial run."""
    width = 60
    print()
    print(_bold(_cyan("=" * width)))
    print(_bold(_cyan("  Tutorial Complete — Summary")))
    print(_bold(_cyan("=" * width)))
    print()

    steps = [
        ("System Status",       results.get("step1", True)),
        ("Model Build",         results.get("step2", True)),
        ("Input Files",         results.get("step3", True)),
        ("Parameters",          results.get("step4", True)),
        ("Initial Conditions",  results.get("step5", True)),
        ("Model Options",       results.get("step6", True)),
        ("Simulation Config",   results.get("step7", True)),
        ("Model Run",           results.get("step8", True)),
        ("View Results",        results.get("step9", True)),
        ("Mass Balance",        results.get("step10", True)),
        ("Observations",        results.get("step11", True)),
    ]

    for i, (name, ok) in enumerate(steps, 1):
        status = _green("PASS") if ok else _red("FAIL")
        print(f"  Step {i:>2d}: {name:<25s} [{status}]")

    passed = sum(1 for _, ok in steps if ok)
    total = len(steps)
    print()
    if passed == total:
        info(f"All {total} steps completed successfully!")
    else:
        warn(f"{passed}/{total} steps passed, {total - passed} failed")

    print()
    print(_dim("  For interactive exploration, start the Shiny app:"))
    print(_dim("    shiny run --reload shiny_app:app"))
    print(_dim("  Or open the tutorial in the browser:"))
    print(_dim("    shiny_app/www/tutorial.html"))
    print()


# ---------------------------------------------------------------------------
# Argument Parsing
# ---------------------------------------------------------------------------
def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="ESTAS-AQUABC Tutorial Runner — automated 30-day simulation walkthrough",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent("""\
            Examples:
              %(prog)s                         # Full tutorial run
              %(prog)s --skip-build            # Use existing ESTAS_II executable
              %(prog)s --skip-build --skip-run # Use existing output
              %(prog)s --compiler gfortran --build-type debug --verbose
        """),
    )
    p.add_argument("--skip-build", action="store_true",
                    help="Skip Step 2 (use existing executable)")
    p.add_argument("--skip-run", action="store_true",
                    help="Skip Step 8 (use existing output in OUTPUTS_30day/)")
    p.add_argument("--build-type", choices=["debug", "release", "fast"],
                    default="release", help="Build type (default: release)")
    p.add_argument("--compiler", default="gfortran",
                    help="Fortran compiler (default: gfortran)")
    p.add_argument("--input-file", default="INPUT_30day.txt",
                    help="Input configuration file (default: INPUT_30day.txt)")
    p.add_argument("--constants", default="WCONST_04.txt",
                    help="Constants file in INPUTS/ (default: WCONST_04.txt)")
    p.add_argument("--output-dir", default=None,
                    help="Output directory (default: read from input file)")
    p.add_argument("--executable", default="ESTAS_II",
                    help="Executable name (default: ESTAS_II)")
    p.add_argument("--verbose", "-v", action="store_true",
                    help="Show detailed output")
    p.add_argument("--no-color", action="store_true",
                    help="Disable coloured terminal output")
    return p.parse_args()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main() -> int:
    args = parse_args()

    global _VERBOSE, _USE_COLOR
    _VERBOSE = args.verbose
    if args.no_color or not sys.stdout.isatty():
        _USE_COLOR = False

    print(_bold(_cyan("""
    ╔══════════════════════════════════════════════════╗
    ║       ESTAS-AQUABC Tutorial Runner               ║
    ║       30-Day Simulation Walkthrough              ║
    ╚══════════════════════════════════════════════════╝
    """)))

    start_time = time.time()
    results: dict[str, bool] = {}

    # Step 1: System status
    try:
        status = step1_system_status(args)
        results["step1"] = True
    except Exception as e:
        error(f"Step 1 failed: {e}")
        results["step1"] = False

    # Step 2: Build
    try:
        results["step2"] = step2_build_model(args)
    except Exception as e:
        error(f"Step 2 failed: {e}")
        results["step2"] = False

    if not results["step2"] and not args.skip_build:
        error("Build failed — cannot continue without executable")
        error("Use --skip-build if you already have a compiled executable")
        print_summary(results)
        return 1

    # Step 3: Explore input files
    try:
        step3_explore_inputs(args)
        results["step3"] = True
    except Exception as e:
        error(f"Step 3 failed: {e}")
        results["step3"] = False

    # Step 4: Review parameters
    try:
        step4_review_parameters(args)
        results["step4"] = True
    except Exception as e:
        error(f"Step 4 failed: {e}")
        results["step4"] = False

    # Step 5: Initial conditions
    try:
        step5_initial_conditions(args)
        results["step5"] = True
    except Exception as e:
        error(f"Step 5 failed: {e}")
        results["step5"] = False

    # Step 6: Model options
    try:
        step6_model_options(args)
        results["step6"] = True
    except Exception as e:
        error(f"Step 6 failed: {e}")
        results["step6"] = False

    # Step 7: Configure simulation
    try:
        sim_config = step7_configure_simulation(args)
        results["step7"] = bool(sim_config)
    except Exception as e:
        error(f"Step 7 failed: {e}")
        results["step7"] = False
        sim_config = {}

    # Override output dir if specified
    if args.output_dir:
        sim_config["output_folder"] = args.output_dir

    # Step 8: Run simulation
    try:
        results["step8"] = step8_run_simulation(args, sim_config)
    except Exception as e:
        error(f"Step 8 failed: {e}")
        results["step8"] = False

    # Step 9: View results
    try:
        step9_view_results(args, sim_config)
        results["step9"] = True
    except Exception as e:
        error(f"Step 9 failed: {e}")
        results["step9"] = False

    # Step 10: Mass balance
    try:
        step10_mass_balance(args, sim_config)
        results["step10"] = True
    except Exception as e:
        error(f"Step 10 failed: {e}")
        results["step10"] = False

    # Step 11: Observations
    try:
        step11_observations(args)
        results["step11"] = True
    except Exception as e:
        error(f"Step 11 failed: {e}")
        results["step11"] = False

    # Summary
    total_elapsed = time.time() - start_time
    print()
    info(f"Total time: {_format_elapsed(total_elapsed)}")
    print_summary(results)

    all_ok = all(results.values())
    return 0 if all_ok else 1


if __name__ == "__main__":
    sys.exit(main())
