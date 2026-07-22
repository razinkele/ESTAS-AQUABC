"""Intel/compiler detection and run-environment helpers, extracted from app.py."""
import logging
import os
import shlex
import subprocess

try:
    from shiny_app.config import SUBPROCESS_PROBE_TIMEOUT
except ImportError:
    from config import SUBPROCESS_PROBE_TIMEOUT

logger = logging.getLogger("AQUABC")

# Common Intel oneAPI compiler installation paths to search
INTEL_COMPILER_SEARCH_PATHS = [
    "/opt/intel/oneapi/compiler/latest/bin",
    "/opt/intel/oneapi/compiler/2025.3/bin",
    "/opt/intel/oneapi/compiler/2025.1/bin",
    "/opt/intel/oneapi/compiler/2025.0/bin",
    "/opt/intel/oneapi/compiler/2024.2/bin",
    "/opt/intel/oneapi/compiler/2024.1/bin",
    "/opt/intel/oneapi/compiler/2024.0/bin",
    os.path.expanduser("~/intel/oneapi/compiler/latest/bin"),
    os.path.expanduser("~/intel/compilers_and_libraries/linux/bin/intel64"),
    os.path.expanduser("~/intel/compilers_and_libraries_2020.2.254/linux/bin/intel64"),
]

def find_compiler_path(compiler_name):
    """Find the full path to a compiler, searching PATH and known Intel locations.

    Returns tuple: (full_path or None, version_string or None)
    """
    # First try PATH via 'which'
    try:
        result = subprocess.run(["which", compiler_name], capture_output=True, text=True, timeout=SUBPROCESS_PROBE_TIMEOUT)
        if result.returncode == 0:
            path = result.stdout.strip()
            # Get version
            try:
                ver_result = subprocess.run([path, "--version"], capture_output=True, text=True, timeout=SUBPROCESS_PROBE_TIMEOUT)
                version = ver_result.stdout.split('\n')[0] if ver_result.returncode == 0 else None
            except Exception:
                version = None
            return path, version
    except Exception:
        pass

    # For Intel compilers, search known installation paths
    if compiler_name in ["ifort", "ifx"]:
        for search_path in INTEL_COMPILER_SEARCH_PATHS:
            full_path = os.path.join(search_path, compiler_name)
            if os.path.isfile(full_path) and os.access(full_path, os.X_OK):
                # Get version
                try:
                    ver_result = subprocess.run([full_path, "--version"], capture_output=True, text=True, timeout=SUBPROCESS_PROBE_TIMEOUT)
                    version = ver_result.stdout.split('\n')[0] if ver_result.returncode == 0 else None
                except Exception:
                    version = None
                return full_path, version

    return None, None

def is_intel_executable(exe_name):
    """Check if an executable was compiled with Intel compilers.

    Returns True if the executable name contains ifort or ifx.
    """
    if not exe_name:
        return False
    return "_ifort" in exe_name.lower() or "_ifx" in exe_name.lower()

def get_intel_library_paths():
    """Get Intel oneAPI library paths for LD_LIBRARY_PATH.

    Returns a list of paths that contain Intel runtime libraries.
    """
    paths = []
    # Search for Intel library directories - check for actual libimf.so presence
    intel_lib_search = [
        "/opt/intel/oneapi/compiler/latest/lib",
        "/opt/intel/oneapi/compiler/2025.3/lib",
        "/opt/intel/oneapi/compiler/2025.1/lib",
        "/opt/intel/oneapi/compiler/2025.0/lib",
        "/opt/intel/oneapi/compiler/2024.2/lib",
        "/opt/intel/oneapi/compiler/2024.1/lib",
        "/opt/intel/oneapi/compiler/2024.0/lib",
        os.path.expanduser("~/intel/oneapi/compiler/latest/lib"),
        os.path.expanduser("~/intel/compilers_and_libraries/linux/lib/intel64"),
    ]
    for path in intel_lib_search:
        if os.path.isdir(path):
            # Verify the path has libimf.so (Intel Math Functions library)
            if os.path.exists(os.path.join(path, "libimf.so")):
                paths.append(path)
    return paths

def check_intel_libs_available():
    """Check if Intel runtime libraries are available.

    Returns a tuple: (available: bool, lib_path: str or None)
    """
    paths = get_intel_library_paths()
    if paths:
        return True, paths[0]
    return False, None

def get_run_environment():
    """Get environment for running executables, including Intel library paths."""
    env = os.environ.copy()
    intel_paths = get_intel_library_paths()
    if intel_paths:
        existing_ld_path = env.get("LD_LIBRARY_PATH", "")
        new_paths = ":".join(intel_paths)
        if existing_ld_path:
            env["LD_LIBRARY_PATH"] = f"{new_paths}:{existing_ld_path}"
        else:
            env["LD_LIBRARY_PATH"] = new_paths
        logger.info(f"Set LD_LIBRARY_PATH with {len(intel_paths)} Intel paths: {new_paths[:100]}...")
    else:
        logger.warning("No Intel library paths found!")
    # Cap OpenMP threads for the small box-model problems (Standard 25 / CL29 29 nodes).
    # With OMP_NUM_THREADS unset, an OpenMP-built ESTAS grabs every core; for such a tiny
    # nkn the per-timestep thread spawn/sync overhead swamps the compute -> ~180x slower
    # than a small count (measured on the 29-box CL29: sim-day 131 in 15s at 4 threads vs
    # 0.7 at 28). 4 is the empirical peak; a user-set OMP_NUM_THREADS is respected. A serial
    # (non-OpenMP) binary ignores this. See docs/OPENMP_PERFORMANCE.md.
    env.setdefault("OMP_NUM_THREADS", str(min(4, os.cpu_count() or 1)))
    return env


def get_intel_setvars_path():
    """Find the Intel oneAPI setvars.sh script.

    Returns the path to setvars.sh if found, None otherwise.
    """
    setvars_locations = [
        "/opt/intel/oneapi/setvars.sh",
        os.path.expanduser("~/intel/oneapi/setvars.sh"),
        "/opt/intel/setvars.sh",
    ]
    for path in setvars_locations:
        if os.path.isfile(path):
            return path
    return None


def build_intel_wrapped_command(cmd_list):
    """Wrap a command to source Intel oneAPI environment first.

    Args:
        cmd_list: List of command parts ['./ESTAS_II_ifx_release', 'INPUT.txt', ...]

    Returns:
        Tuple of (shell_command: str, use_shell: bool)
        If Intel environment is needed and available, returns a shell command that
        sources setvars.sh first. Otherwise returns the original command.
    """
    setvars_path = get_intel_setvars_path()
    if setvars_path:
        # Build a shell command that sources Intel env and runs the executable
        # Use 'source' in bash to load the environment
        escaped_cmd = " ".join(shlex.quote(c) for c in cmd_list if c)
        shell_cmd = f"source {setvars_path} --force > /dev/null 2>&1 && {escaped_cmd}"
        return shell_cmd, True
    else:
        # No setvars.sh found, return original command
        return cmd_list, False
