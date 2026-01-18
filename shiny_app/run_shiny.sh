#!/usr/bin/env bash
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/.." && pwd)
if [ -f "$ROOT/.venv/bin/activate" ]; then
  source "$ROOT/.venv/bin/activate"
fi
# Run the development server on port 5001 to avoid conflicts with other local apps
# Use explicit module path to avoid ambiguity when running from the repo
# Control autoreload via SHINY_RELOAD (set to 1 or 'true' to enable). Default is disabled to avoid port conflicts.
# Optionally set SHINY_RELOAD_PORT to control the autoreload socket. It accepts absolute port or a relative offset
# (same format as the --autoreload-port option; e.g. "+123" to add to --port or "5124" for an absolute port).
RELOAD_ARGS=""
PORT=5001
is_port_in_use() {
  local p=$1
  ss -ltn | awk '{print $4}' | grep -q -E ":${p}$|:${p}\\s" && return 0 || return 1
}

if [ "${SHINY_RELOAD:-}" = "1" ] || [ "${SHINY_RELOAD:-}" = "true" ]; then
  # compute effective autoreload port if provided
  if [ -n "${SHINY_RELOAD_PORT:-}" ]; then
    # if value begins with + or -, compute relative port
    if echo "${SHINY_RELOAD_PORT}" | grep -qE '^[+-]'; then
      # shell arithmetic handles +N or -N
      eff_port=$(( PORT ${SHINY_RELOAD_PORT} ))
    else
      eff_port=${SHINY_RELOAD_PORT}
    fi
    # check port availability
    if is_port_in_use ${eff_port}; then
      echo "WARNING: autoreload port ${eff_port} is already in use; starting without autoreload to avoid conflict"
      echo "Starting shiny (no autoreload) on port ${PORT}"
      python -m shiny run --port ${PORT} shiny_app.app:app
      exit 0
    else
      RELOAD_ARGS="--autoreload-port ${SHINY_RELOAD_PORT}"
    fi
  fi
  echo "Starting shiny with autoreload ${RELOAD_ARGS} on port ${PORT}"
  # Try autoreload; if it fails, fall back to running without autoreload to keep the server active
  python -m shiny run --reload ${RELOAD_ARGS} --port ${PORT} shiny_app.app:app || {
    echo "Autoreload failed to start; falling back to start without autoreload"
    python -m shiny run --port ${PORT} shiny_app.app:app
  }
else
  echo "Starting shiny (no autoreload) on port ${PORT}"
  python -m shiny run --port ${PORT} shiny_app.app:app
fi
