#!/usr/bin/env bash
# Print the CHANGELOG.md body for a given version, with the "## [version] - date"
# header and the trailing "---" separator stripped, plus leading/trailing blank lines.
#
# Usage: extract_release_notes.sh <version> [changelog-path]
#   version        e.g. 0.3.0 (no leading "v")
#   changelog-path defaults to CHANGELOG.md
#
# Exit status is 0 and output is non-empty only if the section exists.
set -euo pipefail

version="${1:?usage: $0 <version> [changelog]}"
changelog="${2:-CHANGELOG.md}"

# Capture lines after the "## [version]" header, up to (not including) the next
# "## [" version header or a standalone "---" separator.
body="$(awk -v ver="$version" '
  index($0, "## [" ver "]") == 1 { inseg = 1; next }
  inseg && (index($0, "## [") == 1 || $0 ~ /^-{3,}[[:space:]]*$/) { exit }
  inseg { print }
' "$changelog")"

# Trim leading and trailing blank lines (awk NF-flag idiom, applied at both ends).
printf '%s\n' "$body" | awk 'NF{p=1} p' | tac | awk 'NF{p=1} p' | tac
