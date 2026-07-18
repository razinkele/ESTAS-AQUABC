"""Centralized configuration constants for the AQUABC Shiny app (TODO 2.6).

Single home for the semantically-meaningful "knobs" that were previously scattered
as magic literals: subprocess timeouts and the default AQUABC filenames. Pure
stdlib leaf module (no shiny_app imports) so any module can depend on it without
introducing an import cycle.

Trivial self-contained idioms (e.g. each module's one-line ``ROOT`` path) are left
in place on purpose: routing them through here would add dual-import boilerplate
that outweighs the duplication removed.
"""

# --- subprocess timeouts (seconds) -----------------------------------------
# Each guards a distinct external call; named so they are tunable and self-documenting.
PROCESS_SHUTDOWN_TIMEOUT = 3     # wait for a terminated run process to exit
LINE_COUNT_TIMEOUT = 2          # `wc -l` on an output CSV
SUBPROCESS_PROBE_TIMEOUT = 5    # quick probes: `which <compiler>`, `file <exe>`
PDF_REPORT_TIMEOUT = 60         # results-PDF generation script
DEEP_PDF_REPORT_TIMEOUT = 120   # deep process-rate-analysis PDF generation script

# --- default AQUABC filenames ----------------------------------------------
# The model-constants file is referenced (as an INPUTS/-relative default) from
# several modules, so it lives here. Other filenames (INPUT.txt, PELAGIC_INPUTS.txt)
# were assessed and left in place: their occurrences are mostly dict keys / labels /
# single ROOT-relative uses where a shared constant would add dual-import boilerplate
# without net benefit (see TODO 2.6).
DEFAULT_CONSTANTS_FILE = "WCONST_04.txt"
