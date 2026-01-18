#!/usr/bin/env bash
set -euo pipefail
HOST=127.0.0.1
PORT=8001
URL="http://${HOST}:${PORT}/_health"

if curl -sSf --max-time 3 "$URL" | grep -q "ok"; then
  exit 0
else
  echo "Health check failed for $URL" >&2
  # restart the service
  /bin/systemctl restart shiny_aquabc
  exit 2
fi
