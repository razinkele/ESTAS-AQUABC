"""Non-reactive build/command helpers (extracted from server())."""
import glob
import os
import subprocess

try:
    from shiny_app.config import SUBPROCESS_PROBE_TIMEOUT
except ImportError:
    from config import SUBPROCESS_PROBE_TIMEOUT
from datetime import datetime


def assemble_estas_command(exe_name, input_file, const_file, binary_enabled,
                           binary_filename, shear_file, default_constants_file):
    """Assemble the model command-line args from resolved (raw) widget values.

    Owns all value-defaulting so it is fully unit-testable. Arg-count rules:
    0 args uses INPUT.txt; 1: INPUT_FILE; 2: +CONSTANTS; 3: +BINARY; 4: +SHEAR.

    Argument 3 is the PREFIX of the binary output files and switches the model to
    binary-only output (ESTAS_II.f90: PRODUCE_ONLY_BINARY_PELAGIC_OUTPUT), so any
    command with three or more arguments writes no text (.out) files -- see
    is_binary_only(). Argument 4 is the critical-shear-stress file NAME; the model
    opens it as <resuspension input folder>/<name>, so only the bare name is passed.
    """
    exe_name = exe_name or "ESTAS_II"
    cmd = [f"./{exe_name}"]

    # Arg 1: input file (required)
    cmd.append(input_file or "INPUT.txt")

    const_file = const_file or ""

    # Binary file only used if the switch is enabled
    binary_file = ""
    if binary_enabled:
        binary_file = binary_filename or ""
        if not binary_file:
            binary_file = "PELAGIC_OUTPUT.bin"  # default if switch on but name empty

    shear_file = os.path.basename((shear_file or "").replace("\\", "/"))

    # If binary or shear file is set, we need a constants file
    if (binary_file or shear_file) and not const_file:
        const_file = default_constants_file

    # Arg 2: constants file
    if not const_file:
        return cmd
    cmd.append(const_file)

    # Shear set but no binary -> placeholder binary output
    if shear_file and not binary_file:
        binary_file = "PELAGIC_OUTPUT.bin"

    # Arg 3: binary output file
    if not binary_file:
        return cmd
    cmd.append(binary_file)

    # Arg 4: shear stress file (optional)
    if shear_file:
        cmd.append(shear_file)

    return cmd


def is_binary_only(cmd) -> bool:
    """True when the command makes the model write binary output only (>= 3 args)."""
    return len([c for c in cmd if c]) >= 4          # executable + three arguments


def critical_shear_choices(inputs_dir) -> dict:
    """Select choices for argument 4: the critical-shear-stress files of a setup.

    These are the per-box threshold tables (``box  tau_crit``), not the shear-stress
    time series: the model reads argument 4 in place of the CRITICAL_SHEAR_STRESS_FNAME
    entry of the resuspension input (option 2), and a time-series file aborts the run.
    Values are bare file names (the model prepends the resuspension input folder).
    """
    choices = {"": "(not used)"}
    try:
        names = sorted(os.listdir(inputs_dir))
    except OSError:
        names = []
    for f in names:
        up = f.upper()
        if "CRITICAL" in up and "SHEAR" in up and f.endswith(".txt"):
            choices[f] = f
    return choices


def assemble_run_env(setup_env: dict, solver_value: str) -> dict:
    """Run-subprocess env: copy the setup env (never mutate the shared Setup.env) and
    set the pelagic solver choice. solver_value: "1" Euler (default) | "2" Heun/RK2."""
    env = dict(setup_env)
    env["ESTAS_PELAGIC_SOLVER"] = solver_value
    return env


def get_available_executables(root):
    """Scan for available executable files under root."""
    executables = []
    exe_patterns = ["ESTAS_II", "ESTAS_II_*", "AQUABC*"]
    for pattern in exe_patterns:
        for f in glob.glob(os.path.join(root, pattern)):
            if os.path.isfile(f) and os.access(f, os.X_OK):
                executables.append(os.path.basename(f))
    for f in ["AQUABC02GFREL", "AQUABC02INTL"]:
        path = os.path.join(root, f)
        if os.path.isfile(path) and os.access(path, os.X_OK):
            if f not in executables:
                executables.append(f)
    return sorted(set(executables))


def get_executable_info(exe_name, root):
    """Return metadata about an executable (size/mtime + file(1) type)."""
    exe_path = os.path.join(root, exe_name)
    if not os.path.exists(exe_path):
        return {"exists": False}

    info = {
        "exists": True,
        "path": exe_path,
        "size": os.path.getsize(exe_path),
        "modified": datetime.fromtimestamp(os.path.getmtime(exe_path)).strftime('%Y-%m-%d %H:%M:%S'),
    }

    # Check if stripped (no debug symbols)
    try:
        result = subprocess.run(["file", exe_path], capture_output=True, text=True, timeout=SUBPROCESS_PROBE_TIMEOUT)
        info["file_type"] = result.stdout.strip()
        info["stripped"] = "stripped" in result.stdout.lower()
        info["has_debug"] = "not stripped" in result.stdout.lower()
    except Exception:
        info["file_type"] = "Unknown"
        info["stripped"] = None

    return info


def target_exe_name(compiler, build_type):
    """Map (compiler, build_type) to the target executable name."""
    fc_short = {
        "gfortran": "gf",
        "ifort": "ifort",
        "ifx": "ifx",
    }.get(compiler, compiler)
    return f"ESTAS_II_{fc_short}_{build_type}"
