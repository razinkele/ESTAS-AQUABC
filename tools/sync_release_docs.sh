#!/usr/bin/env bash
# Sync versioned documentation to a released version.
#
# Rewrites the machine-maintained release marker(s) in the docs so README (and any
# other file carrying the marker) always points at the latest published release. Called
# by .github/workflows/release.yml on every vX.Y.Z tag; also runnable locally:
#
#   tools/sync_release_docs.sh 0.3.4
#
# Idempotent: re-running with the same version leaves the files unchanged (no diff).
# Exits non-zero on a malformed version argument. Prints the files it changed.
set -euo pipefail

VERSION="${1:-}"
if [[ ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z.]+)?$ ]]; then
  echo "usage: $0 <version>   (e.g. 0.3.4) — got '${VERSION}'" >&2
  exit 1
fi

REPO="razinkele/ESTAS-AQUABC"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Files carrying the <!-- LATEST_RELEASE -->…<!-- /LATEST_RELEASE --> marker.
FILES=("$ROOT/README.md")

# The replacement content (single line, between the marker comments).
new="**Latest release:** [v${VERSION}](https://github.com/${REPO}/releases/tag/v${VERSION})"

changed=()
for f in "${FILES[@]}"; do
  [ -f "$f" ] || continue
  if ! grep -q '<!-- LATEST_RELEASE -->' "$f"; then
    echo "::warning::no LATEST_RELEASE marker in $f — skipping" >&2
    continue
  fi
  before="$(cat "$f")"
  # Replace everything between the markers (markers preserved), on the marker line.
  sed -i -E "s|<!-- LATEST_RELEASE -->.*<!-- /LATEST_RELEASE -->|<!-- LATEST_RELEASE -->${new}<!-- /LATEST_RELEASE -->|" "$f"
  if [ "$before" != "$(cat "$f")" ]; then
    changed+=("$f")
  fi
done

if [ "${#changed[@]}" -eq 0 ]; then
  echo "docs already in sync for v${VERSION}"
else
  printf 'synced to v%s: %s\n' "$VERSION" "${changed[*]#"$ROOT"/}"
fi
