# AQUABC Changelog

All notable changes to the AQUABC model and Shiny frontend are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

---

## [Unreleased]

### Documentation
- **`docs/CL29_phenology_diagnosis.md` §74.1**: TN and TP rescored after the validator fix (#196).
  - **Adopted run:** TN 0.819 → 0.805, TP 0.04235 → 0.0319.
  - **Paper I's comparisons**, on four re-runs that reproduce the earlier scores exactly:
    - the legacy gate's TP advantage falls from 14 % to 3 %;
    - the quota variant's TP advantage falls from 23 % to 6 % (it is still better on 6 of 12 pooled errors);
    - the staging-reversed TN penalty falls from 11 % to 3 %;
    - the ice result holds.
  - §72's TN/TP rows carry a dated pointer note.
  - TN/TP scored before #196 are not comparable with later scores.
- **`docs/CL29_phenology_diagnosis.md` §74**: two defects found by a 2026-11-21 code review, measured on the adopted
  CL29 configuration. Nothing is adopted or fixed.
  - **Carbonate chemistry at 0 °C.** The pelagic model reads CO2SYS's output-condition columns, which it computes
    at 0 °C. In-situ conditions lower summer pH by 0.31 and raise summer nitrification by a third to two thirds (at
    the two boxes read), but move every pooled score by less than 1 % and no seasonal score by 2 % or more.
  - **Akinete units in the validator.** `tools/validate_cl29_vs_epa.py` adds the akinete carbon, written in g/m²,
    to TN and TP as mg/L. Without that error the pooled TP RMSE is 0.032 instead of 0.04235.
- **`docs/CL29_phenology_diagnosis.md` §73**: with every cyanobacterial light gate shut, today's adopted configuration
  still reaches 45.1 µg/L of August chlorophyll (observed 50.8; 49.0 with the ratchet, 45.8 with the legacy gate).
  The conclusion of §18-§20 that the bloom needs a surface-positioning state does not hold for today's
  configuration. The "no gate" rung of §19 was the legacy daily gate. Nothing adopted.
- **§73.4**: of the three fitted growth constants, the fixer engine (3.5 → 7.6, on the staged guild since §31) carries
  the set's contribution and the non-fixer → fixer shift: reverting it alone lowers August chlorophyll by 12.5 µg/L
  with the light gates shut, against 11.7 for all three (diatom −1.3, interaction +0.5); the other-algae constant
  does nothing. Nothing adopted.
- **§73.3**: in today's configuration the constants fitted at the adoption act through their three growth constants
  (11.7 of 11.8 µg/L of August chlorophyll, with the composition shift from non-fixers to fixers); the four nutrient
  constants change August by 0.1. Nothing adopted.
- **§73.2**: today's gates-shut August chlorophyll (45.1 µg/L) decomposed from an anchor that reproduces §15's
  impose-only probe on today's code (16.3 against 16.2): averaged over both orders, the seven constants fitted at the
  adoption carry about half (15.5 µg/L) and the staged fixer with the later inputs the other half (13.8). Without
  the fitted constants the August non-fixer carbon is nearly right and the fixers far too low. §73.1's 0.42 share,
  measured against §15's 21.6, is withdrawn (dated correction). Nothing adopted.
- **`docs/CL29_phenology_diagnosis.md` §73.1**: *(Superseded by §73.2: the share is withdrawn.)* with every light gate shut, reversing the staged fixer lowers August
  chlorophyll from 45.1 to 35.2 µg/L, so it carries 42 % of the gap between the §15 search (21.6) and today; the rest
  lies in the other changes since, which are not separated. Positioning adds 1.0 µg/L without the staged fixer.
  Nothing adopted.
- **§72 and §73 corrected** (dated notes): the quota variant is better on 6 of 12 pooled errors, not "most"; and
  the growth constants were not chosen with positioning on (the §21 search scored its trials on inputs without it,
  §60.4), so "every constant was chosen with positioning on" (§73) and "fitted with it in place" (§72, for the
  ratchet) are qualified. Found by an independent fact-check of the papers.
- `docs/report_scripts/make_figures_succession.py` reads the observed side back from the committed
  `cl29_succession_observed.csv` when the species export is not on disk; the modelled rows of that table are those
  of the adopted configuration as it now stands.

### Changed
- **The Fortran unit tests compile with `-fimplicit-none`** (`tests/fortran/Makefile`), as the model and library
  build already does. An undeclared name in a test program is now a compile error, not a silent implicit type.
  - **Today:** all 35 test sources already carry `implicit none`, and every test passes exactly as before (47 identical
    pass/fail summary lines). So this guards future test code.
  - **Proof that it bites:** a unit without `implicit none` that uses an undeclared name compiles with the old flags
    and fails with the new.
  - The 0D-example build (`SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/Makefile`) still does not use the flag.

### Removed
- **The ftnchek check in CI, and its tools.** It never checked the code CI built.
  - **The gate read a frozen file.** From its first wiring (59eec3e, 2026-01-19), the filter
    (`tools/filter_ftnchek.sh`) ignored its argument and read the committed `tools/ftnchek_report.txt`, not the report
    CI had just written. Every committed version of that report covers one file, which the ignore list skips whole,
    and ends in an ftnchek crash ("buffer overflow detected"). On CI, the
    fresh run also crashed, inside the tenth file it read (`mod_PELAGIC_LINK.f90`; the report artifact of main run
    44900895691 covers nine files before it), and `|| true` hid it.
  - **ftnchek cannot parse this code.** Run on each of the 87 tracked `.f90` files with CI's flags, ftnchek 3.3
    crashes on 28. On the other 59 it reports 3,496 diagnostics that the ignore list does not cover. Most come from
    its failure to parse standard Fortran 90: it does not know `module`, `use` or `end ...`, and reads them with the
    following name as one identifier "with embedded space". Forcing free-form source changes nothing. So no ignore
    list makes it a meaningful check.
  - **What still guards `implicit none`, in part:** the gfortran build of the model and library (`make_lib.sh` with the
    Makefile's debug, fast and release flags) compiles with `-fimplicit-none`, so an undeclared name there fails the
    build on CI (checked with a throwaway file). It does not check that the statement is present; the 0D-example
    build does not use the flag (the unit-test build now does, see "Changed"), and the Intel builds only warn
    (`-warn all`). `.github/copilot-instructions.md` and `CONTRIBUTING.md` now say so.
  - **Removed files:** the six CI steps, `tools/run_ftnchek.sh`, `tools/filter_ftnchek.sh`,
    `tools/ftnchek_ignore_patterns.txt`, `tools/ftnchek_report.txt`, `tools/ftnchek_report_ESTAS.txt` and
    `tools/ftnchek_unexpected.txt`.
  - `FTNCHEK_TRIAGE.md` is kept as history, with a dated note.

### Fixed
- **Carbonate chemistry is evaluated at the water temperature, not at 0 °C (RESULT-CHANGING).** The pelagic and
  sediment models read CO2SYS's output-condition columns: pH (18), HCO₃/CO₃/CO₂ (21–23) and the phosphate K1–K3
  (75–77). Every call site set the output temperature `TEMPOUT` to 0 ("does not matter"), so pH, the carbonate
  species and the phosphate constants were those of 0 °C water. `TEMPOUT` is now the in-situ temperature: the water
  temperature in the pelagic model, and the layer temperature already given as `TEMPIN` at the four sediment sites.
  All five lines date at least from the repository's first commit (0419a2c, 2026-01-18), so every result since
  then, including the CL29 calibration, used the 0 °C chemistry.
  - **CL29** (adopted inputs, full record, scored with today's validator):
    - July–August pH is 0.31 lower at boxes 11 and 23, and winter pH is unchanged.
    - Every pooled RMSE changes by less than 1 %; the largest change is PO₄ at −0.62 %.
    - The pooled TN/TP RMSE goes from 0.805 / 0.0319 to 0.806 / 0.0319.
    - For the scored variables at observation times, pooled model means move by less than 1 %, except nitrate at
      +2.5 %.
    - Daily means of the unscored pools move more, and the scorecard cannot see it. The dissolved organic pools fall:
      DON −1.9 % over all days and −6.7 % in July–August (median box), and −7 % and −16 % at box 29. DOP and DOC
      behave alike. Totals barely move (daily-mean TN −0.3 %, TP −0.3 %, median box).
  - **0D example:**
    - Totals barely move: TN +0.3 % and TP 0.0 %, of the pools the 0D prints.
    - The pools shift: detrital N +11.8 %, non-fixing cyanobacteria +11 %, DON −8.6 %.
    - The dissolved organic pools fall in CL29 too, but the detrital and cyanobacterial shifts appear only in 0D,
      and their cause is not established.
    - Its golden, `tests/regression/pelagic_0D_golden.csv`, is regenerated with the method that reproduces the old
      golden byte for byte.
  - **Sediment model** (`MODEL_SEDIMENTS 2`): on `INPUT_sediment_test.txt` the sediment edits alone move sediment
    outputs by up to 1.7 % in two days, so at least one per-layer site runs. The two single-vector sites are dead
    code (`SINGLE_VECTOR_CO2SYS = 0` is a parameter); they were edited for consistency and are not exercised.
  - **Guard:** `tests/python/test_co2sys_output_conditions.py` is a text check that fails if any call site sets
    `TEMPOUT` to a constant. The behavioural check of the pelagic site is the 0D golden.
  - **Old behaviour:** old results stay reproducible from the commit before this change; no switch for the old
    behaviour is provided.
- **A run that aborts now exits with a non-zero status.** 105 aborts in the Fortran sources ended with a plain `STOP`
  (no code, or a text code), which exits with status 0. That covered the solver's unrealistic-value guard, AQUABC's
  NaN checks, failed allocations, time-series and input errors, and lines that printed "error stop". The app then
  reported "Model run completed successfully", and `tools/calibrate_cl29.py` scored the truncated record instead of
  the penalty. Each is now `ERROR STOP` (exit status 1 with gfortran). Messages are unchanged, apart from dropping a
  redundant "error stop" prefix and naming the routine at one site.
  - **Guard:** `tests/python/test_fortran_error_stop.py` fails on any `STOP` without an integer code.
  - **Test:** `tests/integration/abort_exit_status.sh` forces the guard and checks the exit status.
  - **Output:** a full CL29 run is byte-identical to one from the previous build.
  - **The live binary is not rebuilt by this change.** Callers keep seeing exit status 0 on an abort until it is.
- **A constants file that does not exist now stops the run.** Before, a misspelt name, whether in the constants
  table or on the command line, fell back to `WCONST_04.txt` with only a line on stdout, and the whole run used
  constants nobody had chosen. `AQUABC_LENIENT_CONSTANTS=1` keeps the fallback, with the warning moved to stderr.
  - **Tests:** three new cases in `tests/integration/fail_loud_constants.sh`.
  - **Setups that now stop:** the tracked variant files `INPUTS/PELAGIC_INPUTS_WCONST_02.txt`,
    `PELAGIC_INPUTS_before_dissolved_fractions_fixing.txt.txt` and `PELAGIC_INPUTS_zero_nost_boundary.txt` name
    `WCONST_02.txt` or `WCONST_03.txt`, which are not in `INPUTS/`, so until now they ran on `WCONST_04.txt`. No
    tracked control file uses them.
  - **Manual:** the command-line examples in `docs/ESTAS_Reference_Manual.md` named a missing `WCONST_02.txt`.
- **The validator no longer adds the akinete carbon in g/m² to TN and TP in mg/L.** ESTAS writes a state variable
  whose `OUTPUT_TYPE` is 2 per area: concentration × the box's VOLUME/AREA. In CL29 and the Standard set that is
  `AKI_C`, the water-column akinete pool, which TN and TP include.
  - **What changes.** `load_box_output` in `tools/validate_cl29_vs_epa.py` divides such columns by VOLUME/AREA. The
    output types come from the run's own `PELAGIC_INPUTS.txt` (found through its master input file, as the C:Chl
    ratios are, or given by the new `--pelagic-inputs`), and VOLUME/AREA from the run's `WATER_LEVELS.out`.
  - **When that cannot be done**, TN and TP are not scored and the validator says why.
  - **On the adopted CL29 run:**
    - pooled TN RMSE 0.819 → 0.805 and bias +0.448 → +0.385;
    - TP RMSE 0.04235 → 0.0319 and bias +0.0135 → +0.0063;
    - every other score is unchanged.
  - **What else moves with it.** The figures, the PEST forward run and the Supplementary Table S2 script all use the
    same loader. TN/TP scores from before and after the fix are not comparable. Calibration and Morris objectives
    do not include TN or TP.
  - Measured in `docs/CL29_phenology_diagnosis.md` §74.

## [0.13.0] - 2026-11-20

> 🎉 **Released — [AQUABC v0.13.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.13.0)**
> (Linux binary attached). **Map tab on deck.gl, Shiny 1.9, and an options reader that refuses misplaced options.**
> The app now needs Python 3.11+, Shiny 1.9+ and `shiny_deckgl` 1.16 (installed from its tagged commit). The optional
> block of `PELAGIC_MODEL_OPTIONS.txt` is checked name by name: a file with an option misplaced, skipped or misnamed
> **now stops the run** instead of being read silently with the wrong values. The Mass Balance and Observations tabs
> get an output-box selector. **Results are unchanged from v0.12.0 for the shipped setups** (a 346-day CL29 run
> compared identical). A constants file that omits a temperature decay constant on the name-based path now gets
> the plateau defaults and does change. The CL29 diagnosis record gains §69.6-§72.

### Added
- **Output-box selector on the Mass Balance and Observations tabs.** They used the run's first output box only;
  each now lists the run's `PELAGIC_BOX` files (refreshed when the setup changes) and computes from the chosen one.

### Changed
- **The app targets Shiny 1.9 and shiny_deckgl 1.16.** `shiny_app/requirements.txt` now needs `shiny>=1.9.0` and
  installs `shiny_deckgl` 1.16.0 from the commit behind its `v1.16.0` tag (PyPI carries 1.12 only). It needs
  Python 3.11 or newer, so `pyproject.toml` now declares `requires-python >= 3.11`, as the README already did; `ipyleaflet` is no longer needed.
- **The Map tab is drawn with deck.gl through `shiny_deckgl`** instead of ipyleaflet. The map is built once and then
  driven by messages (camera, basemap, layers), so it no longer rebuilds on every control change. The pitch and
  elevation-scale sliders, which had no effect under Leaflet, now tilt the view and scale the station columns.
  Basemaps: Carto Voyager / Positron / Dark and OpenStreetMap Liberty as vector styles; Esri satellite and
  OpenTopoMap as raster tile layers, each with its required credit under the map. The old raster "OpenStreetMap"
  choice is replaced by the vector one. The map libraries load from a CDN on every page; if it is unreachable only
  the Map tab is affected (tested).
- **Deprecated Shiny calls replaced**: `ui.output_text_verbatim` (deprecated in Shiny 1.8) by `ui.output_code` with
  `@render.code` for the build log, the command preview and the mini run log (same markup); the two remaining
  `@reactive.Effect` by `@reactive.effect`.

### Fixed
- **The options reader refuses an option in the wrong place.** The optional block of `PELAGIC_MODEL_OPTIONS.txt`
  (everything from `TEMPERATURE_MODEL` on) is read by position. A file with an option misplaced, skipped or
  misnamed was read without complaint: the value went to a different option and the next failed read ended the
  block silently. A quota test of 2026-11-20 ran that way, with the quota off and day-length weighting on. The
  reader now checks the name on each comment line (`# NAME` or `#NAME`) before reading its value and stops with a
  message naming the expected option; a missing or wrongly typed value also stops the run. A file may still end
  early, at end of file or at the `CYN_ALLELOPATHY_FILE_NAME` line, and keep the defaults for the rest; blank lines,
  tabs and Windows line endings are accepted. No result changes for a valid file: all 43 option files in the working
  tree pass, and a file saved by the app's Model Options tab keeps its comment lines.
- **The in-code default temperature constants were CTMI-style while the default model is plateau**
  (`DEFAULT_PELAGIC_MODEL_CONSTANTS`: `KAPPA_*_UNDER_OPT_TEMP = 0`, `KAPPA_*_OVER_OPT_TEMP = T_max`). A constants
  file that omitted one of them on the name-based path (0D example, interface) got no cold limitation and a collapse
  just above the plateau -- the same mismatch the Standard file had. The defaults are now the plateau decay rates of
  `INPUTS/WCONST_04.txt`. No shipped run changes: the 0D example's file and every ESTAS setup give these constants
  explicitly (0D golden unchanged; Standard 30 d and CL29 90 d byte-identical).
- **`tools/make_varn_inputs.py` wrote the quota switch where the reader expected older options.** The options file is
  positional, and seven option pairs added since the tool was written come before `CYN_VARIABLE_N`. The tool now
  writes those pairs at their defaults when they are absent, accepts `# NAME` and `#NAME` comments, and refuses a
  file whose pairs are out of order.

### Documentation
- **`docs/CL29_phenology_diagnosis.md` §69.6**: a river diatom supply shaped by the measured BSi seasonal index
  (Vybernaite-Lubiene et al. 2018, Table 2) does not do better than a flat supply: partial seed with a spring cost at
  0.30 mg C/L, no seed at 0.10. Probe only; nothing adopted. §69.5 gets a dated correction (the table was in `docs/`).
- **§69.7**: the September-1 diatom inoculum is set by the August river concentration alone, and it does not become
  autumn diatoms (August-only supply leaves October-November at the base value); the autumn follows the autumn supply.
  The load-matched BSi series passes the seed rule only through its August concentration. Nothing adopted.
- **§69.8**: a river diatom supply in September-November only restores the autumn diatoms (K2 0.326 against 0.329
  for the all-year constant) with a quarter of the load, no spring cost, and 7 of 9 gate lines. Nothing adopted.
- **§69.9**: live Nemunas diatom counts (Gasiūnaitė et al., one station, April-September 2002, read from the figure's
  vector data) peak in April-July and are lowest in September, so they do not support the autumn river supply of §69.8.
- **§69.10**: raising the sea boundary's diatoms to 0.30 mg C/L in September-November (three to nine times the
  monitoring climatology) lifts the autumn diatoms only partly (K2 0.101 -> 0.160). No external supply the data
  support explains the missing autumn diatoms. Nothing adopted.
- **§70**: a June-August process budget of the diatoms on the adopted setup. The population is lost in June (-0.12 per
  day), to a growth drop (0.20 -> 0.11 per day, mostly the temperature curve) against constant losses of 0.22-0.24
  per day; in July-August the remnant is sustained by import. A diagnostic; nothing changed.
- **§70.1**: the §62.2 temperature curve and a lower diatom death rate together bring June to the observed level
  (0.19-0.23 against 0.20) and leave July-August at 2-3 % of it. June and July-August are different problems.
  Nothing adopted.
- **§70.2**: resuspension scoped (no option returns settled diatoms from a stock; adopted CL29 cannot enable any), and a
  sensitivity test: switching diatom settling off, all year or in June-September, does not reopen July-August. It is
  not a bound on the resuspension options, which also keep the other particles in suspension. Nothing adopted.
- **§71**: a summary table of every lever tested on the missing summer and autumn diatoms (§62-§70.2) with its
  verdict, and what has not been tested. No new measurement.
- **§57.2**: a label corrected. The boundary section it re-tests is Paper II §3.14, not Paper I's.
- **§72**: what each adopted mechanism contributes today, as off/on pairs on the adopted configuration (legacy
  zooplankton, the surface-position ratchet, the staged fixer, the nitrogen quota). The quota's signature is refuted
  again; nothing adopted or withdrawn.
- `docs/report_scripts/zoo_legacy_monthly.csv`: the legacy-zooplankton series of the zooplankton figure is
  re-measured on the adopted configuration as it now stands (the adopted form gives 9.9 times the legacy one at the June peak).
- `docs/report_scripts/make_figures_phase.py`: the hard-coded monthly values of the phase and composition figures are
  those of the adopted configuration as it now stands (seasonal r +0.78; September non-fixing share 0.17), with how
  they were obtained.
- `docs/report_scripts/make_figures_mechanisms.py`: the ice-transmittance sensitivity values are re-measured on the
  adopted configuration as it now stands (February diatoms 0.372 / 0.383 / 0.418 mg C/L at T = 0.02 / 0.05 / 0.15).
- **Remaining stale plans from the audit**: `shiny_app/NAVIGATION_GUIDE.md` no longer calls the Parameters, Initial
  Conditions and Model Options tabs "Coming Soon"; `AQUABC_FRONTEND_ANALYSIS_AND_PLAN.md` is marked historical;
  `TODO_IMPLEMENTATION_PLAN.md` §1.8 and §1.10 carry a v0.12.0 update note.

### Notes
- **`docs/CL29_phenology_diagnosis.md` §3.1**: the FIX_CYN temperature factor (0.414 / 0.287) was read from the
  process-rate slot that carried the Nostocales fixation rate until #154. Recomputed from the run's water
  temperature it is 0.4136 / 0.2871, so the printed values stand; a dated note records the check, and that under
  today's inputs the same slot read 0.017 too high over July-August before #154.

## [0.12.0] - 2026-11-18

> 🎉 **Released — [AQUABC v0.12.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.12.0)**
> (Linux binary attached). **Codebase audit fixes + ESTAS mass-balance and level corrections.** The Standard
> constants match the temperature model again, the constants readers refuse inconsistent input, the app edits and
> reads the files the run actually uses, and the Fortran tests link the production routines. New opt-in mechanisms
> (day-length light weighting, a density-dependent closure, Nostocales fixation switches) ship disabled.
> **Results differ from v0.11.0 in the last digits** (every real literal is now double precision: dissolved oxygen by
> up to 5e-5 relative), and Standard runs change materially because the plateau temperature constants are restored.

### Added
- **`tools/set_per_box_forcing_columns.py`** points each box's PRECIPITATION, EVAPORATION, AIR TEMPERATURE,
  WIND SPEED and ICE FRACTION rows at its own column when the forcing file has one column per box (report mode
  exits 1 when a set needs it; `--write` fixes it). Every SHYFEM-coupled set on disk (`INPUTS_SHYHYDRO*`,
  `INPUTS_1993*`) had copied `INPUTS_CL29`'s single-column mapping onto 29-column files, so every box read box 1's
  meteorology; fixed, the free-volume water tracks SHYFEM's to +30 mm lagoon-wide over 11 years (was +509). The
  adopted `INPUTS_CL29` is single-column and unaffected. Tests: `tests/python/test_set_per_box_forcing_columns.py`.
- **`LIGHT_DAYLENGTH_OPTION` — day-length weighting of the light limitation** (opt-in, default
  `0` = current behaviour, byte-identical at full record and on the 0D golden). `FDAY` (day
  length) was read from `DRIVING_FUNCTIONS(:,4)`, allocated and bundled into the per-thread
  environment, and fully wired on the `smith == 0` branch of all six phytoplankton library
  routines — but never reached `LIM_LIGHT`, which is the branch CL29 runs. The forcing series
  already existed and was correct (`INPUTS_CL29/FORC_TS_9.txt`, 4,017 daily records, 0.2898 on
  1 January = 6.96 h at 55 °N), so only the consumption was missing.
  - `1` = **Form A**, `LLIGHT = FDAY·f(I_A)` — the form the `smith == 0` branch uses, and a
    genuine bug there: it weights by the photoperiod without concentrating the daily dose into
    it, discarding (1−FDAY) of each day's light (71 % of it in December). Shipped only to
    reproduce the comparison in `docs/CL29_phenology_diagnosis.md` §47; warns at startup and is
    **not for adoption**.
  - `2` = **Form B**, `LLIGHT = FDAY·f(I_A/FDAY)` (WASP/EUTRO) — correct. `I_A` is a daily
    integral, and the depth-averaged Steele curve is near-linear while light-limited, so the
    `FDAY` cancels there; Form B departs from the legacy expression only through P–I curvature.
  - New Fortran unit test `test_lim_light_daylength` (10 assertions) covering the two identities
    that make the option safe: Form A equals `FDAY·base` exactly, and Form B converges to the
    legacy result as `FDAY → 1`.
  - New `tools/probe_lim_light.py`: evaluates all three forms from the model's own constants and
    forcings, no model run required.

- **`PHYTO_CLOSURE_MODEL` / `PHYTO_CLOSURE_REF` — density-dependent closure on the
  cyanobacterial guilds** (opt-in, default `0` = linear death, byte-identical). The specific
  death rate is scaled by `C / PHYTO_CLOSURE_REF`, making total loss **quadratic** in biomass and
  mirroring the closure the zooplankton already use: at `C = PHYTO_CLOSURE_REF` the specific rate
  equals the calibrated `KD`, weaker below and stronger above.
  - **Why this and not another parameter.** `docs/CL29_phenology_diagnosis.md` §50–55 measured
    nine parameter directions and found the two-guild coexistence **over-determined**: the guilds
    are identical on nitrogen, phosphorus, light and vertical position, so self-limitation equals
    cross-limitation — the neutral case of competition theory, which admits winner-take-all but
    no stable interior equilibrium. Stable coexistence needs each guild to limit *itself* more
    than it limits the other. This is therefore a change of **mechanism**, not of tuning: it
    alters the stability of the equilibrium, not its position. Ecologically it stands in for
    losses that scale with bloom density and are otherwise absent — colony aggregation and
    sinking, cyanophage lysis, scum senescence.
  - Applied at all five cyanobacterial death sites (`CYANOBACTERIA` and `CYANOBACTERIA_BOUYANT`,
    `FIX_CYANOBACTERIA` and `FIX_CYANOBACTERIA_BOUYANT`, `NOSTOCALES`) as a `merge(...)` factor
    that is exactly `1.0D0` when disabled — byte-identical **by construction** (multiplication by
    one is exact without `-ffast-math`) as well as by full-record measurement.
  - The factor's numerator is clamped, `max(C, 0)`. Without the clamp an *enabled* closure turns
    a negative biomass into a **positive** death rate — negative × negative — applying a loss to
    an already-negative pool and growing it quadratically, past a mass-balance safeguard gated on
    `C > 0` that can never fire. The linear default is self-correcting there (a negative `C`
    gives a negative death rate, which acts as a gain); the quadratic form is not.
  - Constants live in `AQUABC_II_GLOBAL`, not WCONST, so `nconst` stays **324** and the
    three-definition trap is avoided. Read from `PELAGIC_MODEL_OPTIONS.txt` through the usual
    graceful reads; **no shipped options file carries the lines.**

### Fixed
- **Four app features needed a root `OUTPUT.csv` that ESTAS never writes** (only the 0D example does): Mass Balance,
  Observations, Data Preview and the dashboard's "Last Run". They now use the run's own first box output
  (`PELAGIC_BOX_NNNNN.out` of the control file selected on the Run tab), converted to the `OUTPUT.csv` column names
  for the two analysis tabs; the root `OUTPUT.csv` remains a fallback. Mass Balance says which file it used and that
  one box's pools exclude transport, and reads the active setup's constants instead of `INPUTS/`. Rows with the wrong
  field count (folders written by two runs at once) are skipped.
- **The Parameters tab's categories described an older constants file**: everything from id 158 was under the wrong
  heading (ids 210-234, labelled "Methane", are DON mineralisation; "Settling" and "pH Effects" held DOP
  mineralisation) and ids 319-324 were in no category. The table now has 20 categories covering 1-324 exactly once,
  checked against `INPUTS/WCONST_04.txt`; the Reference Manual and navigation guide lists are regenerated from it.
- **Fortran unit tests exercised copies, not the model.** `tests/fortran/test_pelagic_aux_subset.f90` was a trimmed
  copy of the auxiliary routines whose `GROWTH_AT_TEMP` stayed CTMI-only after production gained the plateau model
  (the default), and six tests carried their own copies of `GROWTH_AT_TEMP`, the nutrient preferences, the ammonia
  routines, `light_kd`/`CUR_SMITH`, `settling_suppres_factor` and `find_unit`. All now link the production sources
  (the copy is deleted). The shared test constants were CTMI-style (`KAPPA_UNDER = 0`, `KAPPA_OVER = T_max`) and are
  now the Standard plateau values; `test_growth_temp` gains the default-is-plateau, CTMI, CTMI-fallback and
  CTMI-constants-under-plateau cases; the two Nostocales germination-gate tests run under CTMI with CTMI-valid
  cardinal temperatures (their old 16/26/38 set is one production rejects).
- **`tools/` and `pest/` were linted nowhere.** They are now in ruff's `include`, in CI and in `make lint`. 28 files
  cleaned (unused imports, import order, placeholder-less f-strings, one late-binding lambda); style-only rules
  (`UP031`, `B007`, `E701`, `E702`) and `F841` are ignored for these script folders.
- **The container could not run its documented validation command.** `tools/validate_cl29_vs_epa.py` imports pandas
  and (for its default plots) matplotlib; the image had numpy only. Both are installed and the image build now
  imports them.
- **Single-precision real literals in double-precision arithmetic** (506 lines in 21 files: the kinetics library,
  CO2SYS, both DO-saturation routines, the sediment diffusion coefficients, the default constants, the 0D example and
  the macroalgae module). `0.1` is a default-real literal, so each carried single-precision rounding into a
  double-precision expression; all now have a `D` exponent. Values are unchanged -- `273.16` in the sediment
  molecular-diffusion routines stays (it pairs with `298.16` in the same Stokes-Einstein scaling) and `2.718`,
  `0.3333`, `0.6667` stay as written. **Results change in the last digits**: dissolved oxygen by up to 5e-5 relative
  (the saturation polynomial cancels large terms), other variables by up to 1e-4 of their peak over a Standard 30-day
  and a CL29 90-day run; the 0D golden moved by 3.2e-5 and is regenerated. A test now fails on any new literal
  without a `D` exponent or kind.
- **The app's Shear Stress option aborted any run that read it, and silently blanked the rest.** Command argument 4
  is the *critical* shear-stress table (per-box thresholds), opened as `<resuspension input folder>/<name>`; the Run
  tab called it a time series, listed every `*SHEAR*` file of `INPUTS/` whatever the setup, and passed
  `INPUTS/<name>`. With resuspension option 2 the doubled path fails to open and `SHEAR_STRESSES_TS.txt` is a read
  error (both reproduced on a 2-day run). The selector now lists the active setup's `*CRITICAL*SHEAR*` files by bare
  name. Three or more arguments also put the model in binary-only mode (no `.out` files), which neither that option
  nor "Enable Binary Output" said: the switch is now "Binary-only output", and the run log warns when a command is
  binary-only.
- **Output Config could overwrite a setup's output file with the form's defaults.** Saving without loading wrote
  boxes 5/6/8/9/14/17/25 with state variables only (so process rates off), with no backup, and any save flattened
  per-box differences. The tab now fills itself from the active setup's file, refuses to save until it has, keeps
  each box's own flags unless the output types are changed, leaves unchanged rows byte-for-byte, writes a `.bak`,
  never creates the file, and warns when a save turns process-rate output off everywhere.
- **A second Run click started a second model on the same output folder**, out of reach of Stop. Runs are now claimed
  before the worker thread starts (per session, and per output folder across sessions); a refused click leaves the
  running log intact. Covers both the Run tab and the dashboard's Quick Run.
- **Binary outputs were read as 37 columns whatever the build.** A record is TIME + `NUM_PELAGIC_STATE_VARS` (33, 34,
  37 or 38 doubles), so 32-state and VARN runs were shifted column by column. The layout now comes from the setup
  that writes into the folder, or, failing that, from the one record length that divides the file and has an
  increasing time column; an undecidable file is an error instead of a guess. `CYN_N` is placed at state 33.
  Review follow-ups: a layout that accounts for the whole file wins over a stale setup count; Output Config keeps
  rows beyond the setup's box count and a file's CRLF endings; the run claim resolves symlinked output folders; and
  Simulation Config will not change the output folder of a control file whose run is in progress.
- **Simulation Config edited `INPUT.txt` whatever the run used.** It now loads and saves the control file the run will
  use (Run Control's "Input Configuration File", which follows the setup) and says which one. Saving is refused until
  that file has been loaded, so values loaded from one setup are never written into another's control file. "Enable
  Sediment Model" wrote `MODEL_SEDIMENTS = 1`, which the model reads as *prescribed fluxes*; it is now a three-way
  choice (off / prescribed fluxes / diagenesis). The print-interval tooltip said 24 steps is hourly at 240 steps/day
  (it is 10).
- **Plots silently showed only the first 10,000 rows** (about the first 417 days of a CL29 run). The whole record is
  read (and cached, which the capped read never was) and thinned evenly for drawing; the title says so when thinned.
- **Edited parameters and initial conditions were written with 6 significant digits** (`12.825253` saved as
  `12.8253`). Edited values are now written at full precision (`utils.format_full_precision`).
- **The app's constants contract was stale**: it required 318 constants (the model needs 324), `run_control` and
  `dashboard` each defaulted to an untracked `WCONST_01.txt`, and the constants list and validation always looked in
  `INPUTS/`. The count is 324 (pinned to `nconst` by a test), both modules use `config.DEFAULT_CONSTANTS_FILE`, the
  list and validation use the active setup's input folder, and validation now rejects a missing, duplicated or
  out-of-range index, as the Fortran reader does.
- **The model-constants table is read by box number, not row position** (`READ_PELAGIC_BOX_MODEL_INPUTS`): each row's
  constants are stored under the box it names, and a box number out of range or named twice stops the run. Before,
  rows listed out of order were attached to the wrong boxes without a word. `AQUABC_LENIENT_CONSTANTS` is now read in
  one place (`UTILS_1::LENIENT_CONSTANTS`) and only an exact `1` counts.
- **A box whose constants differ from box 1's is now refused by default, not silently ignored.** `READ_PELAGIC_BOX_MODEL_INPUTS` reads a
  constants file for every box, but the kinetics use box 1's set for all of them (`mod_AQUATIC_MODEL.f90`). If any
  box's constants differ from box 1's the run now stops, naming the box and the first differing constant;
  `AQUABC_LENIENT_CONSTANTS=1` downgrades that to a warning. No shipped setup is affected (every box lists the same
  file). Covered by two new cases in `tests/integration/fail_loud_constants.sh`.
- **FIX_CYN's temperature-limiter diagnostic no longer has Nostocales N-fixation added to it.** With Nostocales on,
  the model added `PROCESS_RATES(NOST_VEG_HET_C, 9)` into `PROCESS_RATES(FIX_CYN_C, 9)` as a "total fixation", but
  that slot is `LIM_KG_FIX_CYN_TEMP` (FIX_CYN's own fixation is slot 8). The addition is removed; Nostocales fixation
  stays in its own slot. Diagnostics only: state output is byte-identical (Standard 30 d, CL29 90 d), and in
  `*_PROCESS_RATES.out` exactly that one column changes. **Process-rate files written by a build older than #154
  (`8dba2a0`) from a run with Nostocales on carry the added fixation** in the FIX_CYN temperature-limiter column, and
  so does any FIX_CYN temperature limitation read from them. Runs with Nostocales off were never affected.
- **Pelagic DOC mineralisation returned NaN at exactly zero DOC when a half-saturation is zero.** Both shipped
  constant sets carry `K_HS_DOC_MIN_DOXY = 0` (first-order kinetics), so `DOC/(DOC + K)` was 0/0 there, in the inline
  branch (`aquabc_II_pelagic_model.f90`) and in `ORGANIC_CARBON_MINERALIZATION`. The eight DOC terms now go through
  `monod_substrate(s, k)` (`AQUABC_PHYSICAL_CONSTANTS`), which is `s/(s+k)` and 0 where `s + k` is exactly 0; every
  other input is bit-for-bit unchanged (Standard 30 d and CL29 90 d outputs byte-identical, 0D golden passes). The
  DON and DOP twins already had their own guards; the sediment twins ship with K = 1.
- **The Standard set (`INPUTS/`) ran the plateau temperature model on CTMI constants.** The CTMI commit
  (`cc7c76e`, February) put T_max values (35/38/33 °C) into the `KAPPA_*_OVER_OPT_TEMP` slots and zeroed
  `KAPPA_*_UNDER_OPT_TEMP`, but the Standard options select plateau (`TEMPERATURE_MODEL = 0`) and nothing restored
  them. The plateau branch reads those slots as decay rates per °C, so there was no cold limitation at all and
  growth fell to zero just above the plateau (24 °C for diatoms). `INPUTS/WCONST_04.txt` has its pre-CTMI plateau
  values and comments back for all six groups. **Standard-set results change**; CL29 (CTMI, its own constants)
  is untouched. The Fortran defaults and `VALIDATE_PELAGIC_MODEL_CONSTANTS` still assume CTMI and are not changed;
  ESTAS does not call that validator (a 30-day Standard run on the restored file reports the plateau model and no
  correction), but the 0D/interface path does and would rewrite the restored rates to 35/38/33.
- **The built-in Hypoxic scenario set zooplankton carbon, not oxygen**: its initial condition used state
  variable 6 (`ZOO_C`) where dissolved oxygen is 4 (`shiny_app/scenarios.py`, `scenarios/hypoxic.json`).
- **Plot dates were counted from a fixed 1 January 1997** (`shiny_app/modules/plot.py`, four places), one year
  early for Standard (BASE_YEAR 1998) and fifteen for CL29 (2012); `input_analysis.py` did the same for input
  time series. The origin is now the BASE_YEAR of a control file that names the plotted folder
  (`simulation_config.base_year_for_output_dir` / `base_year_for_input_dir`): a lookup in the control files as
  they are now, since the folder does not record which run wrote it.
- **The tracked PEST files were stale in three ways** (`pest/`, regenerated with `build_pest.py`): the template had
  323 of the 324 constants (the model aborts) and pre-adoption values for 23 others (a run would have reverted the
  adopted `INPUTS_CL29/WCONST_04.txt`); `cl29.pst`/`model_obs.ins` still scored station LTK14 against box 25, not 9
  (#57); and two initial values predated adoption. `KG_DIA_OPT_TEMP`'s upper bound is now 10 (was 6; the adopted
  value is 8.10). `build_pest.py` refuses an initial value outside its bounds, and `forward_run.py` now scores
  Chl-a with the run's own C:Chl ratios (`validate_cl29_vs_epa.use_c_to_chla_from`), not the template 30/40.
- **`MASS_BALANCES.out` terms now sum to what the solver integrates.** The KINETICS column was written as
  kinetics minus the diagenetic sediment flux, while the solver adds the two; it is now the kinetics alone, next to
  the SEDIMENT FLUXES column as before. A new 8th term column, PRESCR. SED. FLUX, carries the prescribed benthic
  flux (e.g. the CL29 NO3 sink), which the solver applies as its own term and the file used to omit. The column is
  appended, so positional readers of the first seven terms (`tools/check_varn_run.py`) are unaffected. With
  `MODEL_SEDIMENTS 1` (CL29) the diagenetic flux is zero, so the first ten columns are byte-identical; the new
  column is non-zero only for NO3 there. `WATER_LEVELS.out`'s header now labels its fourth column (volume / area)
  and states that elevation and depth come from the start of the printed step and volume from its end (a one-step
  lag under a free volume, CL29 docs §66.2); the fourth column mixes the two, end-of-step volume over
  start-of-step area. Its rows are unchanged.
- **The box's own `SURFACE_AREA` is now refreshed with the level.** It was set once at read time, but the
  ecology's depth is `VOLUME / SURFACE_AREA`, so under a free volume it went stale wherever the area varies with
  level (real hypsometry). The solver's `SURFACE_AREAS` array was already refreshed. An exact no-op for
  constant-area layers: CL29 is byte-identical under held volume (61 outputs, 30 d) and free volume (60 outputs,
  365 d of the SHYFEM-hydrology set). CL29's synthetic areas are constant above every box bottom, and no level
  leaves that band (CL29 note item 82).
- **Free-volume level inversion: false zero depth when the water sits in the bottom's own layer.**
  `CALCULATE_SURFACE_ELEVATION` (`mod_BASIN.f90`) solved the first-branch quadratic with `C = +2 V`; the
  later-layer branch has the correct `C = −2 V`. A constant-area layer then had no positive root, so the level
  stayed at `BOTTOM_ELEVATION` and `DEPTH` read 0 with water in the box; a tapering layer also returned 0. Only
  free-volume runs recompute the level, so held-volume runs (all adopted CL29 work) are byte-identical (61 output
  files, 30 days). In the CL29 SHYFEM-hydrology run box 18 read zero depth in 301 hourly prints with 0.20-0.31 m
  of water; the fix restores it, and the biology is unchanged to 1e-6. It is not the cause of the CL29 day-~449
  free-volume crash, which remains the EUTROPY flows' drift (box 18's volume goes negative on day 448.9 with the
  fix too). New test `tests/fortran/test_basin_level.f90` (13 checks; the old code fails 5).
- **The basin level was stored in the box indexed by the basin number** (`PELAGIC_BOXES(i)` with `i` the basin
  loop index, `mod_SOLVER.f90`); now `PELAGIC_BOXES(BOX_NO)`. Identical while each basin holds one box, as in
  CL29 and the 0D golden; it matters for multi-box basins, where `SURFACE_AREAS` reads the stored level.
- **Day-length handling in the `smith == 0` phytoplankton branch** (7 sites across 5 library
  routines: DIATOMS, OTHER_PLANKTONIC_ALGAE, NOSTACALES, CYANOBACTERIA ×2,
  FIX_CYANOBACTERIA ×2). `I_A` is a **daily integral**, so the depth-averaged Steele form must
  see the *daylight-mean* irradiance `I_A/FDAY` inside the P–I curve and be weighted by `FDAY`
  outside. The branch multiplied by `FDAY` **without** dividing, silently discarding (1−FDAY) of
  each day's light — 71 % of it in December. `ALPHA_0/ALPHA_1` now use `I_A/FDAY`, and both the
  ratio and the outer weight carry a `max(1.0D-6, min(1.0D0, FDAY))` guard against a
  polar-night divide-by-zero.
  - **No operational effect: `smith` is hardcoded to 1** (`aquabc_II_pelagic_model.f90:299`), so
    this branch is unreached in every shipped configuration — which is why the bug survived, and
    why the full-record run and the 0D golden are both unchanged. The fix matters for anyone
    enabling the branch.
  - Regression test added (`test_diatoms`, `test_smith0_daylength`): the old form was exactly
    `FDAY × (the FDAY=1 value)`, so that product is the target. Its relation to the correct form
    is **regime dependent** and both sides are pinned — light-limited (I/I_s = 0.3) the correct
    form is *larger* (it recovers discarded light), saturated (I/I_s = 3.0) it is *smaller* (the
    concentrated dose pushes past saturation and P–I curvature dominates).
  - ⚠ Left deliberately unchanged: the branch's `2.718` literal for *e* (0.01 % low). `LIM_LIGHT`
    on the `smith == 1` path uses `EULER_E`; changing it here would alter the branch's numerics
    for a reason unrelated to this bug.

- **`NOST_FIX_SWITCH` — DIN-gated fixation share for Nostocales** (opt-in, default `0` =
  current behaviour, byte-identical at full record and on the 0D golden). Ports the
  DIN-inhibition switch `FIX_CYN` already uses to the Nostocales guild, which carries the entire
  modelled fixer biomass but lacked it. `1` sets the fixation share to
  `K_FIX_NOST / (K_FIX_NOST + DIN + DON)` — an inverse Monod suppressing fixation while nitrogen
  is available and enabling it as nitrogen depletes (heterocyst induction). `NOST_FIX_SWITCH`
  and `K_FIX_NOST` are read from `PELAGIC_MODEL_OPTIONS.txt` via `AQUABC_II_GLOBAL`, not as
  WCONST constants, so `nconst` is unchanged.
  - Measured against a matched-rate control (`docs/CL29_phenology_diagnosis.md` §52.3): the
    switch holds 2.3–4.7× more fixer biomass and a substantially better total than the legacy
    fixed share. **Not adopted** — no setting reproduces the observed autumn split at observed
    biomass (§52.4), and the flag stays at `0`.

- **`R_FIX_NOST` — relative productivity of the Nostocales fixing channel** (opt-in, default
  `1.0` = no cost, byte-identical at full record). Mirrors the `R_FIX` constant `FIX_CYN`
  carries; `< 1` is an energetic cost on N2 fixation. Read from `PELAGIC_MODEL_OPTIONS.txt` via
  `AQUABC_II_GLOBAL`, not a WCONST constant.
  - Measured (`docs/CL29_phenology_diagnosis.md` §53): a real but weak second lever — it lifts
    the (share, total) frontier by +0.004 to +0.042 — which **falsified the prediction** that a
    cost acting only at low DIN could not reach the binding asymmetry. **Not adopted**: no
    setting passes both composition gates, the closest approach misses by 0.026 and 0.01, and
    the region is a knife-edge (1 % in the constant moves the September composition by up to
    7 percentage points).

- **`CYANO_POS_GUILDS` — per-guild control of the positional ratchet** (opt-in, default `0` =
  all guilds, byte-identical at full record). The ratchet already kept a separate surface
  fraction per guild (`S_POS(:, POS_CYN/POS_FIX/POS_NOST)`) but its rate constants are global,
  so every guild surfaced identically and the mechanism supplied no niche separation. `1` =
  NOST only, `2` = CYN only, `3` = fixers only. Read from `PELAGIC_MODEL_OPTIONS.txt` via
  `AQUABC_II_GLOBAL`.
  - Measured (`docs/CL29_phenology_diagnosis.md` §55): a powerful dominance lever (CYN ×5,
    FIX ÷8 in the CYN-only config) that produces the same winner-take-all as every other
    direction, with the worst frontier lift of the arc (−0.161). **Not adopted.**
- **`VALIDATE_PELAGIC_MODEL_CONSTANTS` now follows the selected temperature model, and ESTAS runs it.**
  The validator applied the CTMI T_max rules unconditionally: under the plateau model it rewrote every valid decay
  rate (`KAPPA_*_OVER_OPT_TEMP < 1`) to 35/38/33 on the 0D/interface path. Those rules now apply only when
  `USE_CTMI_TEMP` is set; under the plateau model a T_max-like value (≥ 1) is reported and left alone. ESTAS never
  called the validator (its own positional `INIT_PELAGIC_MODEL_CONSTANTS` has no such call); it now does, in a new
  **report-only** mode (`VALIDATE_CONSTANTS_REPORT_ONLY`): problems are printed at start-up and no constant is
  changed, so results are unaffected (byte-identical: Standard 30 d, 23 files; CL29 90 d, 61 files; 0D golden).
  Both `INPUTS/` and the adopted CL29 set now log three zero half-saturations (`K_HS_DOC/DON/DOP_MIN_DOXY`).
  `BETA_NOST_VEG_HET < 0` is now counted like the other corrections. Test: `tests/fortran/test_validate_constants.f90`
  (links the production routine).

### Changed
- **Build and repository housekeeping** (audit items 23-25, 32, 34):
  - one ruff version (0.15.21) in CI, `requirements-dev.txt` and pre-commit (were 0.15.21, >=0.16.4 and v0.9.7); the
    pre-commit `ruff-format` hook is gone, since CI never checked formatting;
  - `make` no longer defaults `PYTHON` to a micromamba path of one machine: it uses `.venv/bin/python` when present,
    else `python3`;
  - `make_lib.sh` refuses two Fortran sources with the same base name (the second was silently skipped), and the
    Makefile note now says what the script does: it compiles every `.f90` under `SOURCE_CODE/`, with no file list;
  - the dead selenium suite (`tests/python/test_app_selenium.py`, paths of another checkout) is removed -- the
    Playwright suites cover the app;
  - `.coverage` is no longer tracked; `.gitignore` covers the local setup variants (`INPUTS_CL29_*/`,
    `INPUT_CL29_*.txt`, `INPUTS_SHYHYDRO*/`, ...), dated `ESTAS_II.2*` backups and run by-products, and no longer
    calls `INPUTS_CL29/` "regenerable" (the adopted set lives in ESTAS-AQUABC-DATA);
  - `tests/fortran/Makefile`: `.NOTPARALLEL` (targets share one object directory) and complete prerequisites for the
    tests that link the production auxiliary file.
- **Calibration harness**: new `formb_closure` paramset (2 loss + 3 growth + 3 cycling
  constants; C:Chl deliberately excluded — handed to the objective it fills the chlorophyll gap
  with pigment rather than biomass).
- **Calibration harness**: worker run timeout raised 600 s → 3600 s, overridable via
  `CL29_RUN_TIMEOUT`. The timeout guards against a *hung* run, not a merely long one, and a
  timed-out run scores as `PENALTY` — indistinguishable from a genuinely bad parameter set, and
  silent. Measured: a 1461-day window with 24 concurrent workers takes ~541 s, only a 10 %
  margin under the old ceiling, so the slower individuals of every generation were being
  penalised for being slow rather than wrong.

### Documentation
- **`PARAMETER_REFERENCE.md` lists all 324 constants** (it named 53, under category ranges of an older file). Its
  tables are generated from `INPUTS/WCONST_04.txt` and the app's categories by `tools/build_parameter_reference.py`
  (`--check` in the test suite keeps it current).
- **README**: the CL29 status no longer stops at v0.5.0 -- it links `docs/CL29_phenology_diagnosis.md` (§1-§69.5)
  and says the adopted `INPUTS_CL29/` set is in the ESTAS-AQUABC-DATA repository; the app's "inputs not found" hint
  says the same instead of pointing at the generator, which gives the pre-calibration set.
- **Stale headers**: `.github/copilot-instructions.md` (was "v0.3", 318 constants, 14 categories),
  `FIXES_AND_IMPROVEMENTS.md` and `FORTRAN_IMPLEMENTATION_PLAN.md` (now marked historical).
- **Both reference manuals brought up to date with the code.** Every one of the **14 runtime
  options and constants** added during the CL29 work was undocumented, and both carried stale
  dimension counts (`nconst` 318 and "323 parameters" against an actual **324**).
  - `ESTAS_Reference_Manual`: a full **`PELAGIC_MODEL_OPTIONS.txt` reference** — the 36-entry
    ordered table with types and defaults, plus the two properties that govern how the file must
    be edited and were written down nowhere: it is **positional** (the reader never matches on
    the comment text, so inserting or reordering an entry silently reassigns every value after
    it) and reads past the seventh entry are **graceful**. Together these make a mis-placed
    option a **silent no-op rather than an error**, since the trailing `CYN_ALLELOPATHY_FILE_NAME`
    line terminates the numeric sequence. Also a driving-functions table (all 10 indices, units,
    the `FDAY` caveat), and a note that `nconst` is defined in two places — changing one compiles
    cleanly and aborts at run time.
  - `AQUABC_Reference_Manual`: category 21 (`ICE_LIGHT_TRANS`); the ice attenuation term and the
    adaptive-saturation note (`G_max` cancels in the light-limited regime); a new **Optional
    Mechanisms** section covering all eight opt-in mechanisms, including two constraints not
    evident from the code — the quadratic closure is **not optional** with `ZOO_FOOD_MODEL = 1`,
    and `CYN_VARIABLE_N` halts outside the VARN build; the two parallel constant readers; and the
    valid pelagic state-variable counts (**32 / 36 / 33 / 37**) for the allelopathy and VARN
    combinations.
  - ⚠ `pdflatex` rejects U+26A0 and similar exotic Unicode: the manuals must stay within the
    existing ASCII/latin-1 symbol set or `make build-docs` fails.
- **Manuscript figure generators are now tracked.** `docs/report_scripts/` remains ignored for the
  manuscript build chain and the satellite exploration scripts, but the generators for published
  figures must be reproducible, so `make_figures*.py` are exempted. New: `make_figures_map.py`
  (study-area map — parses GeoPackage geometry directly, since shapely/geopandas/pyproj are not
  in the environment) and `make_figures_morris.py` (μ*–σ screening plot). All four now create
  `figures/` on demand and derive paths from `__file__` rather than an absolute author path.

### Notes
- **Dead code removed** (audit item 35), outputs byte-identical (Standard 30 d, CL29 90 d): the uncalled
  `READ_MODEL_CONSTANTS_PRICE`; three empty Fortran modules (`aquabc_II_wc_ini`, `aquabc_II_sediment_debug_stranger`,
  `aquabc_II_sediment_internal`); `shiny_app/file_locators.py` (imported by nothing; its `INPUT.txt` line-number
  parsing was also stale) with its test; `tools/deep_process_rate_analysis_v1.py`. Kept on purpose:
  `GENERATE_PELAGIC_DERIVED_VARS` (uncalled, but named in the CL29 phenology record and two tools as the Fortran-side
  derived-TN routine), the root `app.R` (a Shiny Server deployment wrapper), and constants 159, 161-163
  (`k_RED_FE_III`, `k_RED_MN_IV`, `KHS_DOXY_FE_III_RED`, `KHS_DOXY_MN_IV_RED`), which no rate uses but whose removal
  would renumber the constants file.
- **Form B is correct physics and is NOT adopted** — recorded as a measured negative
  (`docs/CL29_phenology_diagnosis.md` §47–49, BACKLOG §4). Landing it costs ≈20 % of production
  in every month and undoes the adopted ice correction (February `DIA_C` 0.99× → 0.22× against
  an observed 0.280), because CL29's growth constants were fitted against a model that gave every
  day 24 hours of light. A 192-evaluation DE over `formb_closure` then failed to recover the
  compensation, improving Φ by 4.6 % on its calibration window while driving February to 0.15×.
  No shipped setup changes; `LIGHT_DAYLENGTH_OPTION` is absent from every options file and
  defaults to `0`.

- **The density-dependent closure is NOT adopted** — it ships disabled and is reported as the
  most promising *unclosed* item of the coexistence work (`docs/CL29_phenology_diagnosis.md`
  §55.4), not as a result. The nine directions that preceded it are the argument for why a
  mechanism, rather than another parameter, is what the problem needs.

## [0.11.0] - 2026-08-06

> 🎉 **Released — [AQUABC v0.11.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.11.0)**
> (Linux binary attached). **CL29 fit characterization + a boundary-P realism correction.** Adds a seasonal
> breakdown to the EPA validator, removes the eutrophic-era summer PO4 boundary boost, and documents the
> summer PO4/Si residual as a bounded, internal-regeneration-dominated structural limit.

### Added
- **Seasonal breakdown for the CL29–EPA validator** (`validate_cl29_vs_epa.py --by-season`). Pools the fit
  metrics into winter/shoulder/summer buckets, since the EPA obs are season-imbalanced (Si/PO4 are ~4×
  summer-heavy) while the model is comparatively aseasonal, so the aggregate metrics over-state the misfit.
  Opt-in; the default metrics CSV is unchanged. It shows NO3/NH4 are well-fit in every season and Si/PO4
  match in winter — the remaining residual is a summer-only effect.

### Changed
- **Removed the eutrophic-era summer PO4 boundary boost** (`CL29_BOUNDARY_PO4_SUMMER_PEAK` 2.0 → 1.0). The
  boost doubled the realistic summer river low (0.0095 → 0.019 mg P/L); it was tuned to the 2012–2016
  eutrophic peak and over-supplies summer P for the de-eutrophication years. A full-record `--by-season`
  sweep confirmed that reducing *supply* is clean (Monod-self-limiting): summer PO4 0.047 → 0.038 (~20 %) at
  ~zero Chl-a cost (LIM_P stays replete), with no over-draw and no NO3/Si side effect. CL29-only; the
  Standard setups are unaffected.

### Documentation
- **Summer PO4/Si characterized as a bounded structural residual** (`docs/CL29_Calibration_Results.md`). The
  summer PO4 gap resisted every lever — over-growing biomass (Chl-a +11), a config-only benthic PO4 sink
  (tested, rejected: crashes Chl-a 25→8), variable-P stoichiometry (ruled out: the baseline summer bloom is
  nutrient-replete, LIM_P = 0.85), and open-boundary P supply (a small clean gain, adopted above). The model
  interior summer PO4 exceeds the boundary PO4, so the residual is internal-regeneration-dominated — it
  would need a P *removal* process the water-column model lacks.

## [0.10.0] - 2026-08-03

> 🎉 **Released — [AQUABC v0.10.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.10.0)**
> (Linux binary attached). **Calibration-rigor toolkit + identifiability-guided recalibration of CL29.**
> Adds a Method-of-Morris sensitivity/identifiability screen and a self-contained parallel calibrator,
> plus the PEST++ workflow guide, a reproducible-build container, and a fail-loud constants reader — and
> adopts a verified nutrient recalibration for CL29 (+3.2 % full-record fit, DIN biases zeroed).

### Added
- **Fail-loud constants reader.** `READ_MODEL_CONSTANTS` (the shared positional reader for the pelagic
  `WCONST_04.txt` and the sediment `W_SED_CONST.txt`) now rejects an incomplete/invalid constants file —
  a missing, out-of-range, or duplicate index, or a malformed line — with a nonzero `error stop` that
  names the offending index, instead of silently defaulting a dropped constant or corrupting memory on an
  out-of-range write. Set `AQUABC_LENIENT_CONSTANTS=1` to restore the old warn-and-continue behaviour.
  Byte-identical for the shipped (index-complete) setups. Also completed the 0D example constants files
  (`const_CL.txt`/`const_default.txt`) to their full set, and added a reproducible-build container.
- **PEST++ calibration workflow guide** (`docs/CL29_Calibration_PEST_Workflow.md`) — documents the external
  calibration harness (Φ objective, adjustable parameters, run instances) that stands in for the
  deliberately-stubbed in-Fortran cost function.
- **Global sensitivity / identifiability screen** — Method of Morris (`tools/sensitivity_morris.py`), a
  self-contained (no PEST++) parallel screen that reuses the CL29 forward model to rank which `WCONST`
  constants the EPA data can constrain. Result: phytoplankton kinetics (cyano mortality, diatom/cyano
  growth) are identifiable; the Si half-saturation and biogenic-Si dissolution constants are not.
  `docs/CL29_Sensitivity_Analysis.md`.
- **Identifiability-guided calibration** — `tools/calibrate_cl29.py` (scipy differential-evolution) and
  `tools/eval_fullrecord_points.py`, a no-PEST++ parallel calibrator. Established that the
  biomass↔nutrient↔Chl-a multivariate wall (PO4 + Si ≈ 64 % of the misfit, reducible only by over-growing
  biomass) is the primary full-record limit, and that the shipped defaults were already near-optimal.
  `docs/CL29_Calibration_Results.md`.

### Changed
- **CL29 nutrient recalibration (adopted).** `CL29_WCONST_OVERRIDE` in
  `tools/eutropy_poc/eutropy_to_estas.py`: denitrification `K_MIN_DOC_NO3N_20` 1.0→1.5, nitrification
  `K_NITR_20` 0.6→1.0, PON→NH4 regeneration `KDISS_DET_PART_ORG_N_20` 0.25→0.4, diatom P-affinity
  `KHS_DIP_DIA` 0.005→0.003. Verified on the full 11-yr EPA record: +3.2 % Φ, DIN biases zeroed
  (NH4 +0.010→+0.004, NO3 +0.033→+0.015), Chl-a bias improved (−3.2→−2.1), PO4/Si unchanged (structural).
  The deliberately-tuned diatom/OPA phosphate competition survives the `KHS_DIP_DIA` change (OPA −4…−6 %).
  CL29-only; the Standard setups are unaffected.

## [0.9.1] - 2026-08-01

> 🎉 **Released — [AQUABC v0.9.1](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.9.1)**
> (Linux binary attached). **Internal refactoring only — pelagic-core `GLOBAL` de-globalization (Phase 5.1),
> byte-identical.** Bundles 8 loose pelagic water-column allocatables in `mod_GLOBAL` into a single
> derived-type instance `pcore` (`pelagic_core_t`), dropping the loose-allocatable count 12 → 4. **No
> behavioural or output change — every model run is byte-identical to v0.9.0.**

### Changed
- **Pelagic-core `GLOBAL` de-globalization (Phase 5.1, byte-identical).** Moved 8 of the 12 loose pelagic
  water-column arrays out of `mod_GLOBAL`'s global scope into one `pcore` instance — Tier 1 (`node_active`,
  `SAVED_OUTPUTS`, `CHLA`, `WATER_COLUMN_OUTPUT`, `SURFACE_BOXES`) and Tier 2 (`DERIVATIVES`,
  `DRIVING_FUNCTIONS`, `FLAGS`). Pure `X` → `pcore%X` renames; verified byte-identical across Standard
  (`MODEL_SEDIMENTS=0`), CL29 (`=1`) and the `MODEL_SEDIMENTS=2` sediment test, with clean gfortran + OpenMP
  builds and an unchanged `-Wunused` set. `GLOBAL` loose-allocatable count 12 → 4. Tier 3 (the last 4 core
  arrays — `pH`/`STATE_VARIABLES`/`MODEL_CONSTANTS`/`PROCESS_RATES`) is a decided **no-go** (a cosmetic
  count reduction with zero coupling change, since `pcore` stays in `mod_GLOBAL`, against the highest-risk
  tier).

### Documentation
- Design spec + implementation plan for the pelagic-core de-globalization — hardened by a 4-way adversarial
  in-loop review + a 5-finder workflow review — and the Tier-3 no-go decision, under
  `docs/superpowers/{specs,plans}/2026-08-01-pelagic-core-*`. `BACKLOG.md` §1 5.1 updated to substantially
  complete.
- Added the model description paper to the README citation section: Ertürk, A., Šakurova, I., Žilius, M.,
  et al. (2023), *Ecological Modelling* 486, 110509, <https://doi.org/10.1016/j.ecolmodel.2023.110509>.

## [0.9.0] - 2026-07-31

> 🎉 **Released — [AQUABC v0.9.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.9.0)**
> (Linux binary attached). **Headline: resuspension now runs together with full sediment diagenesis via a
> mass-conserving bed↔water particulate coupling.** Prescribed-velocity "Option 3" erodes the surface bed
> layer and transfers the eroded particulate C/N/P/Si to the water-column detritus pools, decrementing the
> bed by the same mass — the previously-guarded, never-run combination. The release also lands a
> sediment-redox Fe(II) salt-selection bugfix and a negative-mass diagnostics refactor, consolidated from a
> collaborator's tree. **Standard (25-box, `MODEL_SEDIMENTS=0`) runs remain byte-identical to v0.8.0.**

### Added
- **Resuspension × sediment-diagenesis coupling (Phase 1).** Bed resuspension ("Option 3" — a per-box
  prescribed erosion-velocity time series) can now run together with full diagenesis (`MODEL_SEDIMENTS=2`):
  an erosion term removes particulate C/N/P/Si from bed layer 1 and delivers it to the water-column detritus
  pools (`DET_PART_ORG_C/N/P`, `PART_Si`) by the same areal mass — a mass-conserving two-way transfer — and
  the guard that previously halted diagenesis+resuspension is lifted for Option 3. All new behaviour is gated
  on `MODEL_SEDIMENTS>1 .and. resuspension`, a combination no existing setup runs. Verified over 365-day
  coupled runs: flux-level bed_out/water_in ratio = 1.0; diagenesis-without-resuspension **byte-identical**;
  eroded C/N/P/Si delivered to the water column matches the bed decrement. Phase 2 (shear-driven erosion) is
  deferred by design.

### Changed
- **Sediment-redox Fe(II) salt-selection bugfix.** `SED_REDOX_AND_SPECIATION` picked the precipitating
  Fe(II) salt by reducing the wrong axis of the `(box, layer, salt)` activity-ratio array
  (`maxloc(..., dim=2)`, the *layer* axis); corrected to `dim=3` (the *salt* axis). This changes sediment
  redox/speciation output for **advanced-redox diagenesis runs** (a correctness fix, not a no-op);
  no-diagenesis runs (e.g. Standard) are unaffected.
- **Negative-mass / negative-concentration diagnostics** extracted from the pelagic solver loop into
  dedicated `CHECK_NEW_PELAGIC_MASS` / `CHECK_NEW_PELAGIC_CONCENTRATION` routines in `mod_PELAGIC_ECOLOGY`
  (readability/locality; behaviour preserved).
- **Sediment `STRANGER` sanity band widened** ±1e4 → ±1e5 for headroom on coupled runs; the NaN/Inf guard is
  unchanged, so true blow-ups are still caught (changes only the finite out-of-range trip threshold).

### Documentation
- **Resuspension × sediment-diagenesis coupling design + `ali_version` reconciliation** recorded under
  `docs/superpowers/specs/2026-07-30-resuspension-diagenesis-coupling-design.md`.
- **BACKLOG.md refreshed** — coupling Phase-1 marked complete; the next shovel-ready engineering slice is the
  remaining 12-allocatable pelagic-core `GLOBAL` de-globalization; variable N:C stoichiometry remains the
  flagged science lever for the CL29 summer-NO3 residual.

## [0.8.0] - 2026-07-25

> 🎉 **Released — [AQUABC v0.8.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.8.0)**
> (Linux binary attached). **Headline: Phase-5 de-globalisation continues, the pelagic solver becomes
> selectable, and CL29 gains a benthic-denitrification nitrogen sink.** Two more byte-identical
> `GLOBAL`→derived-type slices (bottom-sediment `sediment_state_t`, settling / water-coupling
> `wsc_state_t`) drop `GLOBAL`'s allocatable count 44 → 12. The pelagic solver is now selectable via
> the `ESTAS_PELAGIC_SOLVER` env var (default Euler, byte-identical; RK2/Heun an experimental opt-in).
> A new config-only `CL29_BENTHIC_DENIT` sediment NO3 sink addresses the CL29 summer-NO3
> over-prediction (EPA-verified: NO3 bias more than halved). Standard model runs remain byte-identical
> to v0.7.0.

### Added
- **Configurable pelagic solver** (`ESTAS_PELAGIC_SOLVER` env var). Default (unset / `1`) is Euler and
  byte-identical to prior releases; `2` selects the RK2/Heun solver as a **documented experimental**
  opt-in — an investigation established it converges ~1st-order for this model (dominated by
  `MIN_CONCENTRATION` clamping) and is not better than Euler per unit cost. Ships with a Shiny
  run-control selector and the banked RK2 correctness fixes (volume RK2-averaging + stage-2 forcing
  re-evaluation).
- **Benthic denitrification NO3 sink for CL29** (`CL29_BENTHIC_DENIT` converter option). A summer-peaked
  prescribed sediment-interface NO3 sink (config-only, no Fortran) addressing the diagnosed summer-NO3
  over-prediction — the water-column-only model lacks the dominant shallow-lagoon N sink (its own
  water-column denitrification is O2-throttled). EPA-verified over 11 years: NO3 bias more than halved
  and TN/DO improved, with a small, structural PO4 trade-off; off by config for a byte-identical baseline.

### Changed
- **Phase-5.1 `GLOBAL` de-globalisation continues** (both byte-identical): the bottom-sediment state
  moves into `sediment_state_t` (`bsed`) and the settling / water-sediment-coupling state into
  `wsc_state_t` (`wsc`, a new leaf module), dropping `GLOBAL`'s allocatable count 44 → 23 → 12. Each
  slice was hardened via adversarial in-loop review and a strip-and-compare byte-identity proof.

### Documentation
- **FIX_CYN (N2-fixing cyanobacteria) investigation — decided negative result.** Documented as not
  reproducible in CL29: an NH4-floor competitive exclusion (summer DIN floored by regeneration-driven
  NH4 that already matches observations, keeping non-fixers N-replete), the same class as the
  Nostocales limit. The multivariable co-calibration spec was blocked by a three-lens in-loop review
  (contradictory observations + the fixation wall). The related summer-NO3 over-prediction is
  root-caused (missing benthic denitrification, now partially fixed), and **variable N:C stoichiometry**
  is flagged in the backlog as the clean future lever to close the remaining gap.

## [0.7.0] - 2026-07-23

> 🎉 **Released — [AQUABC v0.7.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.7.0)**
> (Linux binary attached). **Headline: the `GLOBAL` de-globalisation begins.** The 19-variable
> sediment-resuspension / shear-stress subsystem moves out of the `GLOBAL` god-module into a
> `resuspension_t` derived type — the first bounded, **byte-identical** slice of the long-deferred
> Phase-5 refactor (`GLOBAL`'s allocatable count drops 55 → 44). The Dashboard landing page also
> gains the loadable-setup selector (in two-way sync with the Run Model tab), and the dependency
> stack is refreshed. Fully backward-compatible: the Standard model run is byte-identical to v0.6.0.

### Added
- **Setup selector surfaced on the Dashboard landing page** (`shiny_app/modules/dashboard.py`).
  The loadable-setup selector — previously only on Model Config → Run Model — now also appears on
  the Dashboard, directly above **Quick Run**, so the Standard (25-box) vs CL29 (29-box) choice is
  discoverable where runs are actually launched. It stays in **two-way sync** with the Run Model tab
  selector (both drive the single `run.current_setup` source of truth) and shows the same
  availability notice when a setup's inputs are absent. UI-only; no model-behaviour change, and the
  Run Model tab selector is unchanged.

### Changed
- **Resuspension global state encapsulated into a derived type** (`SOURCE_CODE/ESTAS/`). The
  19-variable sediment-resuspension / shear-stress subsystem was lifted out of the `GLOBAL`
  god-module into a `resuspension_t` derived type with a single module-scoped instance `resusp`,
  owned by `mod_RESUSPENSION` (which already allocated and read every one of them); `GLOBAL`'s
  allocatable count drops 55 → 44. Behaviour-preserving: the Standard (option-2) run is
  **byte-identical** to pre-change output, the rename is a provable pure-prefix of the 19 members
  (verified by strip-and-compare on every consumer file), CL29 (option-0) runs to completion, and
  the Fortran unit tests stay green. First bounded slice of the deferred `GLOBAL` de-globalisation
  (`FORTRAN_IMPLEMENTATION_PLAN.md` §8.1 Task 5.1). Design + two-round adversarial review:
  `docs/superpowers/specs/2026-07-22-resuspension-state-derived-type-design.md`.

### Dependencies
- Refreshed the dependency stack (Dependabot, all CI-green): `ruff` 0.8.0 → 0.15.22, `ipywidgets`
  → ≥8.1.8, `ipyleaflet` → ≥0.20.0, `python-dotenv` → ≥1.2.2, `pre-commit` → ≥4.6.1,
  `actions/setup-python` 6.3.0 → 7.0.0, and `actions/checkout` 7.0.0 → 7.0.1. Dev/CI-only; no
  runtime behaviour change.

## [0.6.0] - 2026-07-22

> 🎉 **Released — [AQUABC v0.6.0](https://github.com/razinkele/ESTAS-AQUABC/releases/tag/v0.6.0)**
> (Linux binary attached). **Headline: loadable setups.** Load a complete model configuration in
> one click — the Shiny UI now ships a **setup registry** with **Standard (25-box)**, **CL29 —
> Curonian Lagoon (29-box)**, and **CL29 (29-box, 2023 climatology)**. Picking a setup wires up the
> input file, run environment (incl. `ESTAS_HOLD_VOLUME=1` for the CL29 setups), box geometry, and
> result/input directories, with guards that keep the Standard model's inputs safe. This release also
> lands a large UI-run speedup (OpenMP thread cap — ~180× faster on small box models) and reusable
> before/after validation tooling. Fully backward-compatible: the default Standard setup is
> byte-identical to prior behaviour.

### Added
- **Loadable setup registry in the Shiny UI (`shiny_app/setups.py`).** One selection loads a
  complete model configuration — input file + inputs/outputs directories + box count + run
  environment. Ships three setups: **Standard (25-box)**, **CL29 — Curonian Lagoon (29-box)**, and
  **CL29 — Curonian Lagoon (29-box, 2023 climatology)**. Selecting a setup drives the config
  dropdown (filtered to compatible input files), injects `ESTAS_HOLD_VOLUME=1` on both run paths for
  the CL29 setups, sets the box-count selectors (preserving the committed default), points the
  primary results/input views at the setup's directories, and prevents running a setup whose inputs
  are absent (with the exact generate command). Write-guards block overwriting the Standard model's
  `INPUTS/` (scenario apply + the parameter / initial-condition / model-option Save handlers) while a
  non-standard setup is loaded. Backward-compatible: the default Standard setup is byte-identical to
  prior behaviour. Future applications are added as data (one `Setup(...)` entry).
- **`tools/compare_validation_runs.py`** — per-variable obs-weighted RMSE/bias diff between two
  `validate_cl29_vs_epa.py` runs, with a one-sided regression guard (a named variable fails if
  RMSE or |bias| rises beyond a threshold, or its bias sign-flips). Reusable for before/after
  calibration testing.

### Fixed
- **Shiny UI model runs now cap `OMP_NUM_THREADS = min(4, cores)`** in the run environment
  (`shiny_app/compiler_env.py`). An OpenMP-built `ESTAS_II` with `OMP_NUM_THREADS` unset defaulted to
  every core; for the small box-model problems (25–29 nodes) that is **~180× slower** than a small
  thread count, because the per-timestep thread spawn/sync overhead swamps the compute (a CL29 UI run
  went from ~27 h projected back to ~7 min). A user-set `OMP_NUM_THREADS` is respected; serial
  (non-OpenMP) binaries are unaffected.

### Investigated (no model-behaviour change)
- **CL29 PEST-posterior promotion — validated and abandoned.** Promoting the 2022-calibrated
  `KDISS_DET_PART_ORG_P_20`=0.118 + `K_MIN_DOC_NO3N_20`=1.13 into the converter default was tested
  with a measured two-run before/after and **reverted**: `KDISS` closes PO4 over-prediction in both
  windows but induces P-limitation that regresses the EPA 2012–2021 fit (NH4/NO3/Si/Chl-a) — the
  posteriors do not transfer across the 2012–16 hyperbloom vs 2022 low-P regimes (nonstationarity).
  Documented in `docs/CL29_KM_2022-2023_Validation.md`; no default changed.

### Verified (release binary)
- The published `ESTAS_II-0.6.0-linux-x86_64` asset was verified end-to-end after release. It runs
  the **Standard (25-box)** model to full completion (365-day window, exit 0, no NaN) and the
  **CL29 (29-box)** application to **full completion — day 4016 (2012–2022), all 29 boxes, no NaN**,
  with `ESTAS_HOLD_VOLUME=1`. Its CL29 output is **byte-for-byte identical** to the canonical
  `run_cl29.sh` reference run: **29/29 box files identical, max |Δ| = 0** across every field and
  timestep — confirming the serial release build reproduces the reference path exactly (the local
  reference binary is a separate OpenMP build).

## [0.5.2] - 2026-07-21

### Fixed
- **Full Intel `ifx` build now works (release+OpenMP `build-estas` in ~66 s, debug `-O0`
  in ~20 s) and is exercised in CI.** The long-suspected "ifx hangs" was **not** the `-O2`
  optimizer — the whole library compiles at `-O2` in ~1 minute once its modules exist. The
  real cause was `SOURCE_CODE/build/make_lib.sh`'s *speculative multi-pass* build: it
  attempts every source and defers whatever fails because a `use`d module isn't built yet.
  gfortran fails such an attempt in milliseconds, so the passes converge; `ifx`'s front end
  instead **hangs** when a whole-module `use` target is missing, so the build stalled
  indefinitely. Fixed by compiling all module-defining files **before** the leaf files
  (external procedures that define no module, so nothing depends on them) — a leaf file can
  then never see a missing module — plus a per-file Intel compile timeout
  (`IFX_COMPILE_TIMEOUT`, default 300 s) as a safety net. gfortran output is unchanged
  (0D golden regression identical).
- **Three latent standard-conformance bugs gfortran tolerated but `ifx` rejects**, surfaced
  once the build no longer hung — all behaviour-preserving (0D golden unchanged):
  - `PELAGIC_KINETICS` dummy `AQUABC_CALLED_BEFORE` was `real(DBL)` while the actual argument
    is the integer `GLOBAL::CALLED_BEFORE` (forwarded to an integer dummy) — retyped to
    `integer` (ifx #6633; also removed a second latent real→int mismatch downstream).
  - `AQUABC_SEDIMENT_MODEL_1`'s `SED_BURRIAL_RATE_OUTPUTS` was `optional` — never
    `present()`-tested, always passed, and written unconditionally — so `optional` was both
    unnecessary and a latent crash risk; removed, which also dropped the explicit-interface
    requirement the external caller lacked (ifx #8055).
  - `DO_SATURATION`/`KAWIND` in `aquabc_II_pelagic_model` were declared as bare `real`
    scalars but are external functions — added the `external` attribute (ifx #6410).
- **Intel (`ifx`/`ifort`) release build now uses value-safe floating point.** The release
  flags gained **`-fp-model precise`** — Intel defaults to `-fp-model fast=1` at `-O2`,
  which reorders/contracts FP and would diverge from the gfortran release build and the
  bit-reproducible 0D golden (`fast` mode keeps `-fp-model fast=2` deliberately). Also
  added **`-heap-arrays`** to the Intel OpenMP flags so the large per-thread kinetics
  buffers live on the heap instead of overflowing the smaller OpenMP thread stacks
  (runtime alternative: a large `OMP_STACKSIZE`). gfortran is unaffected.

### Added
- **Intel oneAPI (`ifx`) full-build CI job** (`build-intel`) — installs the free Intel
  Fortran compiler from Intel's apt repo on a stock `ubuntu-latest` runner and builds the
  whole AQUABC library + `ESTAS_II` engine at release+OpenMP and the library at debug
  `-O0`, asserting the artifacts exist and that `show-config` carries `-fp-model precise`.
  Completes the Intel half of TODO 3.1.

## [0.5.1] - 2026-07-20

### Changed
- **README rewritten to a best-practice structure** — CI / release / license badges, a
  project overview and component map, a **development-status** section (per-component
  maturity plus the CL29 calibration state), a documentation index, and
  contributing / license / citation pointers. Corrects stale content (the `make test`
  target name → `make test-all`; the "306/318 constants" note) and preserves the
  auto-synced `LATEST_RELEASE` marker.

## [0.5.0] - 2026-07-20

Headline: a full **CL29 (29-box Curonian Lagoon) calibration & validation toolchain**
against the Lithuanian EPA monitoring archive and 2015 experimental process-rate data,
plus a **CO2SYS carbonate-chemistry correctness fix**, **OpenMP parallelization** of the
pelagic kinetics, a batch of **memory-safety fixes**, and the **`AQUABC_PELAGIC_KINETICS`
decomposition**.

### Added
- **CL29 EPA calibration & validation toolchain.** `tools/ingest_epa_observations.py`
  (+ `tools/epa_station_to_box.csv`) turns the EPA archive (~70k samples, 1984–2021) into
  tidy observations + per-station `.dates`, resolving per-era units/speciation (µg vs mg;
  dissolved-N ion-basis pre-2008 → nitrogen) from cross-era value continuity (#38).
  `tools/validate_cl29_vs_epa.py` scores a CL29 run against the observations per
  (box, variable) for 8 variables, three of them (Tot_N/Tot_P/Chl-a) reconstructed from
  the state-variable pools, with time-series plots (#39, #40). The CL29 run was **extended
  from 2012–2016 to the full 2012–2022 EUTROPY forcing record**, ~6×-ing the EPA overlap
  (#41). `docs/CL29_EPA_Calibration_Summary.md` documents the whole arc (#44).
- **OpenMP parallelization** of `AQUABC_PELAGIC_KINETICS` and the CO2SYS call (TODO 4.1–4.4):
  a micro-benchmark harness + `docs/OPENMP_PERFORMANCE.md`, CO2SYS chunked across threads
  (8-thread speed-up 2.84× → 6.55× at nkn=1000), an empty-chunk barrier-deadlock fix, and
  thread-affinity guidance. Build with `make OPENMP=1 build-estas`.
- **macOS/gfortran CI** entry in the `build-and-run` matrix, with a `--numeric-warn`
  cross-platform tolerance for the 0D golden (TODO 3.1, #35).
- **CO2SYS Fortran test coverage** — `tests/fortran/test_co2sys.f90` (13 round-trip /
  invariant / PyCO2SYS-validated checks) (#34).
- **`run_cl29.sh`** wrapper that bakes in `ESTAS_HOLD_VOLUME=1`, fixing the CL29 day-~449
  negative-mass volume crash (EUTROPY flows are not per-box volume-conserving) (#26).

### Changed
- **CL29 nitrogen calibration** — water-column denitrification `K_MIN_DOC_NO3N_20`
  0.025 → 1.0 (converter override), correcting a ~2× NO3/TN over-prediction versus EPA
  (NO3 bias +0.31 → +0.06). Mechanistically grounded — the lagoon is a documented
  strong-denitrification N sink (#42).
- **CL29 summer-P boost** `CL29_BOUNDARY_PO4_SUMMER_PEAK` 3.0 → 2.0, de-eutrophication-aware
  for the fuller 2012–2022 record (#43).
- **`AQUABC_PELAGIC_KINETICS` decomposition** (TODO 1.6) — a 3,642-line mega-subroutine
  became a 394-line orchestrator plus five `contains` procedures, byte-identical.
- **Shiny non-blocking I/O** (TODO 2.4) — the three heaviest `OUTPUT.csv` read+compute
  handlers moved off the event loop via `@reactive.extended_task` + `asyncio.to_thread` (#33).
- **Centralized configuration** — subprocess timeouts + default constants filename into
  `shiny_app/config.py` (TODO 2.6, #31); sediment write-only-variable cleanup (TODO 1.7, #32).
- **Serial solver performance** — box-geometry cache + hoisting the O(nkn²) per-box array
  zeroing to linear, byte-identical (#37).
- **Advanced-redox configurability** — runtime FePO4 solubility (`FEPO4_KSP_LOG10`) and the
  reductive Fe(III)-P coupling (`FE_P_REDOX_FRAC`, W_SED_CONST #171) (#27, #28).

### Fixed
- **RK2 solver double-applied the CHLA settling suppression (latent).** `CALC_DERIV`
  (`SOURCE_CODE/ESTAS/mod_SOLVER.f90`) applies the chlorophyll-based settling-suppression
  factor to `SETTLING_VELOCITIES` **in place**, but `UPDATE_TIME_FUNCS` (which recomputes
  the fresh velocities) runs only **once** per RK2 (Heun) step — so RK2's stage-2
  `CALC_DERIV` suppressed an already-suppressed velocity, compounding the factor. Fixed by
  preserving a pristine `SETTLING_VELOCITIES_FRESH` after `UPDATE_TIME_FUNCS` and restoring
  it before stage 2, so each stage suppresses once from the fresh base. **No live output
  changes:** the solver is hardcoded to forward-Euler (`PELAGIC_SOLVER_NO = 1`,
  `mod_SIMULATE.f90`), so RK2 is currently dead code; the Euler path is untouched and the
  0D golden is byte-identical. This is a latent-correctness fix for anyone enabling RK2.
  Found during a profiling-driven solver optimization review (see the deferred
  `PELAGIC_SOLVER` perf items in `TODO_IMPLEMENTATION_PLAN.md`).
- **CO2SYS carbonate chemistry — KB=0 bug (found by new test coverage, TODO 5.1).** The Dickson (1990) boron-constant formula in `SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90` had a **misplaced parenthesis** — `(-24.4344 - 25.085·√S - 0.2474·S)·(logTempK + 0.053105·√S·TempK)` instead of `(…)·logTempK + 0.053105·√S·TempK` per Dickson 1990. This drove `lnKB ≈ −1.7e4`, so the boric-acid dissociation constant **KB underflowed to 0**, dropping borate alkalinity (~91 µmol/kg at S=35) from the alkalinity budget and misassigning it to the carbonate system. Effect at the canonical case (S=35, T=25 °C, TA=2300, DIC=2000, K1K2=4, KSO4=1, total scale): **pH 8.21→8.045, pCO₂ 260→397 µatm, Ω_aragonite 4.72→3.39** — the corrected values now match **PyCO2SYS 1.8** to ~1e-4. This changes model output wherever CO2SYS runs (pelagic pH / CO₂ flux / calcite-aragonite saturation, and the sediment carbonate system); the **0D regression golden was regenerated** (`tests/regression/pelagic_0D_golden.csv`; worst 0D shift ~10 % in NO3N via the pH→nitrification coupling, output finite and physically sane). Surfaced by the new **`tests/fortran/test_co2sys.f90`** (13 checks: round-trip consistency, mass-balance closure, physical invariants, a borate-alkalinity regression guard, and a PyCO2SYS-validated anchor). Latent bugs in the CO2SYS input pairings the model does *not* use — `(TA,pH)`, `(pH,pCO₂)`, `(TA,pCO₂)` — are documented for a future fix (TODO 5.1).
- **Model-constants out-of-bounds write** (TODO 1.10) — a `NUM_MODEL_CONSTANTS` mismatch
  let the constants reader write past the end of the array; memory-safety fix, production
  output byte-identical (#23).
- **Advanced-redox non-determinism** (TODO 1.11) — a local `FLAGS` in `CALC_DERIV` shadowed
  the global, leaving `FIRST_TIME_STEP` / init-option flags reading stack garbage so runs
  diverged; one-line fix, now deterministic across runs (#24).
- **Nostocales `DAY_OF_YEAR` uninitialised read** — corrected the day-of-year seasonality
  input to the Nostocales module (#25).
- **Kelvin-offset single-precision loss** (TODO 1.8) — the only real magic number (273.15)
  became a named `CELSIUS_TO_KELVIN` constant, fixing a latent precision bug in
  DO-saturation / CO2SYS (~1e-6 intended output change, 0D golden regenerated) (#30).
- **Missing `IOSTAT` on input opens** (TODO 1.9) — an `OPEN_INPUT_FILE` helper guards the
  input-file opens so a missing/unreadable file gives a clean message + nonzero stop
  instead of a raw runtime crash; byte-identical when files exist (#29).

### Dependencies
- Dependabot bumps: numpy (≥2.2.6,<3.0), networkx (≥3.4.2,<4.0), pre-commit (≥4.6.0),
  pytest-cov (≥7.1.0) (#18–#21).

## [0.4.5] - 2026-07-15

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 5 (final cleanup): the rearchitecture is complete** (TODO 2.1). This closing phase removes the dead weight left after all fifteen tabs became true Shiny modules: (1) the **unread `run.build_config`** `RunController` value — a `reactive.calc` whose only consumer, the "Build & Run" handler, was removed as dead code in Phase 4 (the cross-tab contract is now the four live values `command_config`/`constants_config`/`run_executable_name`/`active_executable` plus `exe_list_version`); (2) the now-empty **`shiny_app/ui_panels.py`** stub (all three panels it once held are modules); and (3) **~70 dead imports** in `app.py` (leaf parsers, stdlib, and viz libraries orphaned as inline handlers moved into modules), after which the `F401` per-file-ignore is dropped (the structural `E402`/`F841`/`B023`/`S602`/`S605` ignores remain). Behavior is **unchanged** — pure removals, verified by `import shiny_app.app` + `create_ui().tagify()` (the test suite does not import `app.py`, so this is the real backstop), the full suite (**178** Python tests), and a boot smoke (all 15 tabs render namespaced with zero bare-id leaks).
- **End state (spec §9 success criteria met):** `server()` is a thin assembler — per-session `RunController`/`AppState` construction plus the two app-level chrome renders (`help_content`/`changelog_content`) and 15 `x_server("id", state)` calls, nothing else. **`shiny_app/app.py` is 756 lines, down from ~5,600** at the start of the rearchitecture. Fifteen cohesive `@module.ui`/`@module.server` modules (plus the converted `diagnostics`) live in `shiny_app/` behind the `RunController`/`AppState` contract; **no `input.X` crosses a module boundary** except via that contract or the documented `session.root_scope().make_scope("run_control")` bridge. The `v0.4.0`–`v0.4.5` series (Phases 0–5) was executed subagent-driven with per-task and whole-branch reviews, each release CI-verified including the Playwright/Selenium integration tests. Spec + phase plans under `docs/superpowers/{specs,plans}/2026-07-1[45]-*shiny-modules*`.

## [0.4.4] - 2026-07-15

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 4 (run/build/dashboard cluster)** (TODO 2.1): converted the **last three** inline tabs out of the `server()` closure into true namespaced `@module.ui`/`@module.server` Shiny modules — `model_build`, `run_control` (a fat tab: the Run Model + Output Config sub-tabs of `panel_model_control`), and `dashboard`. `server()` is now a thin assembler: per-session `RunController`/`AppState` construction, the two app-level chrome renders (`help_content`/`changelog_content`), and 15 `x_server("id", state)` calls — nothing else. **`app.py` drops from 2,393 to 871 lines** (from ~5,600 at the start of the rearchitecture). Behavior is **unchanged except the within-tab id namespace** — verified by a render-smoke unit test per module (`.tagify()` for the `run_control` fat tab; **177 → 178** Python tests), a `create_ui().tagify()` boot smoke (all cluster tabs render namespaced with zero bare-id leaks, default `nav_dashboard` view intact), and the Playwright/Selenium integration selectors migrated to `model_build-*`/`run_control-*`/`dashboard-*`.
- **Contract-first cross-module rewiring (the de-risk for the most-coupled cluster).** Before any id was namespaced, every value that crosses between these three modules was routed through the shared `RunController` (`run`): `run.command_config` (the assembled command **`List[str]`**), `run.build_config` (build-config dict), `run.constants_config` (the quick-run constants-validation triple), `run.run_executable_name` (the bare Run-Model executable-name string), and `run.active_executable` (the Model-Build selector) — the dashboard now reads these instead of another tab's `input.X`. This exposed and fixed a latent list-vs-name conflation (the command is a list; the executable name is a separate string), and it means **no `input.X` crosses a module boundary**.
- **`sim_output_dir`/`run_executable` cross-namespace bridge repointed via `session.root_scope().make_scope("run_control")`.** The `plot`, `sim_config`, and `model_build` modules reach the Run-Model/Output-Config widgets that `run_control` now owns; the bridge uses a module-scoped session plus a **bare** id (for both reads and writes). The naïve fully-qualified-literal form (`input["run_control-sim_output_dir"]`) is deliberately avoided because Shiny's `validate_id` rejects the hyphen and raises `ValueError` on read. A pre-execution multi-agent adversarial review of the plan (verified empirically against Shiny 1.5.1) caught this and four other issues before implementation. A preparatory sweep also dropped ~286 lines of dead run/build handlers (zero UI references) so the conversion diffs stayed clean. After this phase, only Phase 5 (final cleanup: an empty `ui_panels.py` stub, `app.py` dead leaf-imports, and one now-unread `run.build_config` registration) remains. Spec + Phase-4 plan under `docs/superpowers/{specs,plans}/2026-07-1[45]-*shiny-modules*`.

## [0.4.3] - 2026-07-15

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 3 (output cluster + dead-bus removal)** (TODO 2.1): converted the last four content tabs out of the `server()` closure into true namespaced `@module.ui`/`@module.server` Shiny modules — `mass_balance`, `observations`, and a single merged `plot` module (`shiny_app/modules/`), plus `diagnostics` upgraded in place (`shiny_app/diagnostics.py`). Each `<tab>_ui()` returns panel *content* (the `panel_conditional` stays in `create_ui()` so `input.navigation` keeps referencing the global nav input); each `<tab>_server(input, output, session, state)` is a verbatim port of the tab's handlers + reactive values; within-tab ids namespace to `<tab>-*`. Behavior is **unchanged except the id namespace** — verified by a render-smoke unit test per module (**173 → 176** Python tests), a live `create_ui()` boot smoke (all twelve converted tabs render namespaced with zero bare-id leaks, `server()` constructs clean), and the Playwright/Selenium integration selectors.
- **Plots tab merged into one `plot` module (spec §6 reversal):** rather than the originally-specced `plot` + `output_browser` split, the whole Plots tab — plotting, the former output-browser (file preview / column summary / output-dir + file selection), and the input-timeseries viewer — became a **single** `plot` module (its 18 handlers span two non-contiguous `app.py` ranges that straddled the still-inline run/dashboard block; ported byte-verbatim). The output-file selection is now consumed **entirely within** the module, so the cross-module output-selection signal was never needed.
- **Dead Phase-0 output-selection bus removed:** because no cross-module reader survives the merge, `AppState` drops from 7 fields to **4** (`run`, `navigate`, `output_config_version`, `sim_config_version`) — the `selected_output_dir`/`selected_output_file`/`selected_output_format` fields and the app-level `_publish_output_selection` effect that fed them are deleted (`test_appstate_holds_fields` updated). The `plot` module's `init_output_dirs` reaches the still-app-level sibling `sim_output_dir` input via `session.root_scope()` — the same documented, Phase-4-flagged bridge `sim_config` established.
- **`diagnostics` pseudo-module → true module:** the last panel using the old flat-id pattern (manually `diag_`-prefixed global ids, a plain `diagnostics_ui()` returning a `panel_conditional`, and `diagnostics_server(input, output, session, root_dir)`) is now a true `@module.ui`/`@module.server` module (ids namespace to `diagnostics-diag_*`); its `panel_conditional` moved to `create_ui`, the `root_dir` parameter was dropped for the module-computed project root, and its render-smoke test uses `.tagify()` (the UI wraps a `navset_card_tab`). All diagnostic logic (`_diag_state`, the polling effect, the threaded 16-check analysis, the PDF-export effects, and every render output) ports verbatim. After this phase, only the run/build/dashboard cluster remains inline in `server()` (Phase 4). Spec + Phase-3 plan under `docs/superpowers/{specs,plans}/2026-07-1[45]-*shiny-modules*`.

## [0.4.2] - 2026-07-15

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 2 (leaf modules)** (TODO 2.1): converted seven more tabs into true namespaced `@module.ui`/`@module.server` Shiny modules under `shiny_app/modules/`, applying the Phase-1 `parameters` pilot template — `model_structure`, `map`, `model_options`, `initial_conditions`, `input_files`, `scenarios`, and `sim_config`. Each `<tab>_ui()` returns the panel *content* (the `panel_conditional` stays in `create_ui()` so `input.navigation` keeps referencing the global nav input); each `<tab>_server(input, output, session, state)` is a verbatim port of the tab's handlers + reactive values; within-tab widget ids namespace to `<tab>-*` (nav ids stay global). Modules are self-contained (stdlib + the existing leaf parsers only, nothing from `app.py`). Observable behavior is **unchanged except the id namespace** — verified by a render-smoke unit test per module (**166 → 173** Python tests), a live websocket boot smoke (all seven tabs render namespaced with zero bare-id leaks, `server()` runs clean), and the Playwright/Selenium integration selectors migrated to `<tab>-*`. Two conversions were non-standard: **`sim_config`** is a partial fat-tab extraction — its `@module.ui` returns a `nav_panel` composed into `panel_model_control`'s `navset_card_tab` while the sibling sub-tabs (Run Model, Output Config) stay inline for Phase 4, and its handlers reach the still-app-level sibling `sim_output_dir` input via `session.root_scope()` (a documented, Phase-4-flagged bridge); **`map`** uses `ipyleaflet` and **`model_structure`** re-derives its diagram `www` path for the deeper `modules/` location. `app.py` drops to ~3,800 lines (from ~5,600 at the start of the rearchitecture). Spec + Phase-2 plan under `docs/superpowers/{specs,plans}/2026-07-14-*shiny-modules*`.
- **`chrome` decision (spec §6 refinement):** `help_content`/`changelog_content` stay app-level `@render.ui`s in `server()` rather than becoming a module, because their offcanvas container ids (`helpOffcanvas`/`changelogOffcanvas`) are referenced by JS (`getElementById`) and therefore cannot be namespaced. Total planned modules: 16 (not 17).

## [0.4.1] - 2026-07-14

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 1 (pilot: `parameters` Shiny module)** (TODO 2.1): the first tab converted to a true namespaced Shiny module. New `shiny_app/modules/parameters.py` provides `parameters_ui()` (`@module.ui`, returning the panel *content* only) and `parameters_server(input, output, session, state)` (`@module.server`, the 5 handlers + 3 reactive values ported verbatim from the old inline `server()` block). `create_ui()` wraps it at app level — `ui.panel_conditional("input.navigation === 'nav_parameters'", parameters_ui("parameters"))` — so the nav condition keeps referencing the **global** `navigation` input, and `server()` deletes the inline block and calls `parameters_server("parameters", state)` once. Within-tab widget ids now namespace to `parameters-*` (e.g. `parameters-param_category`); the `nav_parameters` nav id stays global. Observable behavior is **unchanged** except the id namespace (the tab's load/edit/save flow, including the dynamic `param_{id}` inputs, round-trips identically inside the module) — verified by a render-smoke unit test (`tests/python/test_parameters_module.py`, **165 → 166** tests), a live websocket boot smoke, and the Playwright/Selenium integration selectors migrated to `parameters-*`. The module is self-contained (imports only `parameter_parser` + stdlib, nothing from `app.py`) and establishes the `shiny_app/modules/` layout + `x_ui(id)`/`x_server(id, state)` pattern reused by the remaining tab conversions.
- **`state.navigate()` upgraded to a namespace-independent mechanism.** Phase 0's `ui.update_radio_buttons("navigation", …)` only works from the app-level (global) namespace; a converted module cannot reach the global `navigation` input that way. `state.navigate` is now an async callable that does `session.send_custom_message("aquabc_navigate", …)`, handled by a new `nav_script` client handler that sets the nav input and the sidebar active-link (matching a real nav click); the two dashboard goto handlers are now `async` and `await` it (`AppState.navigate` typed `Callable[[str], Awaitable[None]]`). Proven on the still-app-level goto buttons this phase; ready for the namespaced dashboard module in a later phase. Spec + Phase-1 plan under `docs/superpowers/{specs,plans}/2026-07-14-*shiny-modules*`.

## [0.4.0] - 2026-07-14

### Changed
- **`shiny_app/app.py` rearchitecture — Phase 0 (shared-state contract)** (TODO 2.1): first phase of converting the ~5,000-line `server()` closure toward true namespaced Shiny modules. Extracts the per-session run/build state out of the `server()` closure into a new module `shiny_app/app_state.py` with two units: **`RunController`** — the run/build "session" holding the subprocess handle, the thread-appended `build_log_lines`/`run_log_lines` buffers, and the `execute_build`/`start_run`/`stop`/`is_running` methods (ported verbatim from the old `_execute_build_process`, the `on_run` worker body, and the stop handlers, modulo the documented `_box[0]`→`self.attr` renames), plus the `exe_list_version`/`active_executable`/`build_config`/`command_config` signals; and **`AppState`** — a 7-field dataclass bundling the cross-tab reactive signals (`run`, `navigate`, `selected_output_dir`/`file`/`format`, `output_config_version`, `sim_config_version`). `server()` now constructs `state = AppState(...)` once per session and every build/run/stop handler and log render delegates to `run.*`; the old closure boxes (`_execute_build_process`, `_log_lines`, `_build_log_lines`, `_model_process`/`_model_running`/`_last_run_time`/`_model_progress`, `_exe_list_version`) are removed; output-selection and config-version signals are published into `state`, and the goto handlers route through `state.navigate`. Observable behavior is **unchanged / DOM-identical** — no widget id or UI layout changed (no UI file touched), verified by the full suite plus a live websocket-session boot smoke (`server()` runs end-to-end with no traceback). The run/build engine is independently unit-testable for the first time: adds `tests/python/test_run_controller.py` (**155 → 165** Python tests). This phase publishes/registers the cross-tab bus, but its consumers switch in later phases (the `selected_*`/`command_config`/`build_config` values are written-but-not-read for now). Opens the `v0.4` series for the Shiny-modules rearchitecture. Executed via subagent-driven development (per-task TDD + review, clean whole-branch review). Spec + Phase-0 plan under `docs/superpowers/{specs,plans}/2026-07-14-*`.

## [0.3.8] - 2026-07-13

### Changed
- **Command-preview dedup:** the `status_info` panel's command-line preview re-implemented the model command-assembly logic inline (a second copy of what `shiny_app/build_commands.assemble_estas_command` already does). It now delegates to `assemble_estas_command`, removing the duplication (`shiny_app/app.py` −23 lines). Behavior is unchanged — proven identical across an exhaustive 216-combination input matrix (executable × input file × constants × binary-enabled × binary filename × shear), preserving the handler's exact input-read semantics and the `"(model defaults)"` placeholder handling.
- **Lint cleanup of the extracted modules:** the ten finalized `shiny_app/` modules extracted during the decomposition (`build_commands`, `box_network`, `output_data`, `ui_scripts`, `ui_panels`, `diagnostics`, `diagnostics_plots`, `file_locators`, `input_analysis`, `utils`) now pass a full `ruff check` — import sorting (`I001`), unused imports (`F401`), whitespace (`W29x`), and dead locals/loop vars (`F841`/`B007`) were cleared (behavior-neutral). `app.py` is intentionally left untouched (it is still mid-decomposition, and a large cosmetic diff there would churn history on a file under active refactoring). `shiny_app/ui_scripts.py` gets a scoped `W293` ignore because those blank-line whitespaces live inside the inline-JavaScript string literals, where stripping them would alter the rendered `<script>` content. No behavior change; the full test suite (155 tests) is green.

## [0.3.7] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 3, output-data cluster** (TODO 2.1): extracted seven pure output-file helpers from `server()` into a new module `shiny_app/output_data.py` — `looks_numeric`, `format_elapsed`, `get_output_folder_from_config`, `get_output_files_info`, `get_output_columns` (CSV/`.out`/`.bin` header reading via pandas), `get_output_directories`, and `get_output_files_from_dir`. The functions use a default-arg pattern (`root=ROOT`, `output_csv=OUTPUT_CSV`, `input_txt_path=INPUT_TXT_PATH` defaulting to self-computed module consts) so the 11 call sites stay argument-free while unit tests inject a `tmp_path`. `output_data.py` is stdlib + pandas only, importing `PELAGIC_BOX_COLUMNS`/`SimulationConfigFile` from the existing `utils`/`simulation_config` leaf modules and nothing from `app.py`. Observable behavior is **unchanged** (all seven bodies confirmed equal to the originals modulo the documented const→param edits; the reactive `reactive.Value`-backed CSV cache and the reactive neighbours `run_command`/`analyze_output_directory` were deliberately left in `server()`). Adds `tests/python/test_output_data.py` (7 tests pinning the parse/scan/format logic; **148 → 155** Python tests); `app.py` −161 lines. Third phase-3 cluster after the build pilot (0.3.5) and box-network (0.3.6). Spec + plan under `docs/superpowers/{specs,plans}/2026-07-13-app-py-decomposition-phase3-outputdata*.md`.

## [0.3.6] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 3, box-network cluster** (TODO 2.1): extracted the six non-reactive box-network functions from `server()` into a new module `shiny_app/box_network.py` — the three input parsers `parse_pelagic_inputs`/`parse_advective_links`/`parse_bathymetry` (now taking `inputs_dir` explicitly instead of a closed-over global) and the three Map-Display plotly figure builders `build_box_network_figure`/`build_bathymetry_figure`/`build_depths_overview` (moved verbatim). The two Map-Display render handlers now delegate to the module (7 call sites); observable behavior is **unchanged** (all six bodies confirmed character-identical to the originals; the handlers' control flow and `go.FigureWidget` wrap untouched). `box_network.py` is stdlib + plotly only and imports nothing from `app.py`. Adds `tests/python/test_box_network.py` (10 tests: parser fixtures pinning exact structures + malformed-row/missing-file paths, plus figure smoke tests; **138 → 148** Python tests); `app.py` −595 lines. Second phase-3 cluster after the build-cluster pilot (0.3.5). Spec + plan under `docs/superpowers/{specs,plans}/2026-07-13-app-py-decomposition-phase3-boxnetwork*.md`.

## [0.3.5] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 3 pilot (build cluster)** (TODO 2.1): extracted the non-reactive build/command logic out of the ~5,900-line `server()` into a new pure module `shiny_app/build_commands.py` — `assemble_estas_command` (command-line assembly + all value-defaulting), `get_available_executables`, `get_executable_info` (size/mtime + `file(1)` type), and `target_exe_name`. The four `server()` nested functions are now **thin wrappers** that resolve the reactive inputs and delegate; observable behavior is **unchanged** (reactive dependencies and defaulting preserved — the conditional `cmd_binary_filename` read is kept, and all 15 call sites are untouched). `build_commands.py` is stdlib-only and imports nothing from `app.py`. Adds `tests/python/test_build_commands.py` (15 unit tests pinning every command-assembly branch; **123 → 138** Python tests); `app.py` −76 lines. This pilot proves the "resolve-at-call-site, pure-helper" pattern for the remaining `server()` clusters (file-I/O, plot-prep, mass-balance, observations, scenarios), which stay deferred. Spec + plan under `docs/superpowers/{specs,plans}/2026-07-13-app-py-decomposition-phase3-build-pilot*.md`.

### Added
- **Release-doc auto-sync:** the release workflow now keeps the README `Latest release` marker in sync on every `vX.Y.Z` tag via `tools/sync_release_docs.sh` (idempotent; committed back to `main` with `[skip ci]`). README refreshed to the current state (123→138-test Python suite, the decomposed `shiny_app/` module layout). CHANGELOG remains manually curated and validated by the workflow.

## [0.3.4] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 2** (TODO 2.1): split the ~1,566-line `create_ui()` into three declarative UI-fragment modules — `shiny_app/ui_scripts.py` (6 inline-JS blocks), `shiny_app/ui_panels.py` (14 content panels), and `shiny_app/ui_chrome.py` (sidebar, header, css, 3 offcanvas). `create_ui()` is now a **53-line** assembler that composes the fragments; the rendered UI is **byte-identical** (verified by a full render diff — 148,580 chars unchanged). The moves are verbatim (each fragment independently confirmed byte/AST-identical); the four app-level consts a fragment needs (`COMPILERS`, `BUILD_TYPES`, `NAV_CHOICES`, `MIN_SMOOTH_WINDOW`) stay in `app.py` and are passed as arguments, so the fragment modules import nothing from `app.py` (no circular imports). Adds multi-marker render-smoke tests `tests/python/test_ui_{scripts,panels,chrome}.py` (**117 → 123** Python tests). `app.py`: **7,925 → ~6,450 lines** (−1,472 net in this change). Deferred to later phases: extracting the non-reactive logic the reactive handlers call, and the full Shiny-modules rearchitecture. Spec + plan under `docs/superpowers/{specs,plans}/2026-07-13-app-py-decomposition-phase2*.md`.

## [0.3.3] - 2026-07-13

### Changed
- **`shiny_app/app.py` decomposition — phase 1** (TODO 2.1): extracted three clusters of non-reactive, module-level helpers out of the 8,616-line monolith into focused, unit-tested modules — `shiny_app/compiler_env.py` (Intel/compiler detection), `shiny_app/input_analysis.py` (input-file analysis + `INPUT_FILE_CATEGORIES`), and `shiny_app/file_locators.py` (output/box-file discovery). `app.py` re-imports them via the existing fallback pattern; `server()`/`create_ui()` and all runtime behavior are **unchanged** — the moves are verbatim (verified byte-identical) and the Playwright integration tests pass. `app.py`: **8,616 → 7,925 lines** (−691). Adds `tests/python/test_{compiler_env,input_analysis,file_locators}.py` (117 Python tests total). Deferred to later phases: splitting `create_ui()` into `ui/` fragments, extracting the non-reactive logic the reactive handlers call, and the full Shiny-modules rearchitecture. Spec + plan under `docs/superpowers/{specs,plans}/2026-07-12-app-py-decomposition*.md`.

## [0.3.2] - 2026-07-12

### Added
- **End-to-end 0D regression test** (TODO 5.2): a golden-file check on the `AQUABC_PELAGIC_0D` example. `tests/regression/compare_0D.py` diffs a fresh `OUTPUT.csv` against a committed golden (`tests/regression/pelagic_0D_golden.csv`, the current-code output downsampled across the full 1096-day run) with an **exact header check** (catches column reorder/rename — the "wrong-column" bug class) plus a per-cell `rtol/atol` numeric check. Wired into the `build-and-run` CI job (`--rtol 1e-6`); the comparison logic is unit-tested in `tests/python/test_e2e_regression.py`.

### Changed
- **CI compiler/platform matrix** (TODO 3.1): the `build-and-run` job now runs under a `strategy.matrix` (`fail-fast: false`, compiler selected via job-level `env: FC`, honored by the Makefile's `ifeq ($(origin FC),default)`). gfortran/ubuntu-latest is active, with documented, ready-to-enable rows for `ifx` (Intel oneAPI) and `macos-latest`.

### Fixed
- **`tests/python/test_safe_resolve.py` collection failure:** it imported `safe_resolve` from `shiny_app.app`, which imports pandas — and the installed pandas (compiled against NumPy 1.x) crashes the import under NumPy 2.x. `safe_resolve` is a pure `os.path` helper, so it was moved to a stdlib-only `shiny_app/safe_resolve.py` (`app.py` re-exports it, backward-compatible). The full Python test suite now runs with **no exclusion (99 → 107 tests)**.

## [0.3.1] - 2026-07-12

### Added
- **CL29 wind-modulated diatom settling (#3):** `SETTLING_VELOCITY_TS_1.txt` (DIA_C) is now a daily time series `w_eff = w0/(1+(U/U_c)²)` derived from ERA5 daily wind (2012–2016), replacing the constant `CL29_DIATOM_SETTLING`. Parameters pinned to `w0 = 0.3` m/day and `U_c = 4.21` m/s (half-suppression wind, near the fine-sediment resuspension threshold). Converter-only — ESTAS already reads settling as a time series. Off-switch `CL29_WIND_RESUSPENSION = False` (or an absent wind file) restores the byte-identical constant. Ships the committed daily-wind artifact `tools/eutropy_poc/net/wind_daily.csv` (ERA5/Copernicus, attributed) and its regenerator `tools/eutropy_poc/make_wind_daily.py`. Same-binary validation shows the change reproduces the constant-0.1 behavior almost exactly (5-yr domain spring-diatom peak +0 %, summer cyano +1 %, 0 NaN): because the Nida wind is aseasonal, this is a mechanistic-defensibility change, not a change to the bloom.
- **CL29 provisional sediment-facies strawman (#5):** inert `CL29_SEDIMENT_TYPE_PROVISIONAL` (low-confidence salinity/depth classification for the 29 boxes); the active `CL29_SEDIMENT_TYPE` stays empty (CL29 byte-identical) pending an authoritative facies map.

### Changed
- **CL29 summer boundary-P correction enabled (`CL29_BOUNDARY_PO4_SUMMER_PEAK = 3`, was `1.0`/off):** a day-of-year Gaussian multiplier lifts mid-summer boundary PO4 (~0.018 → 0.034 mg/L) into the observed 2012–2016 Curonian summer range (avg ~0.025 mg/L; Aleksandrov 2025), correcting a documented ~3–15× summer-P under-supply. A `{1, 3, 5}` 5-yr sweep confirmed raising summer P is beneficial — summer cyano (CYN 1.25 → 1.34 mgC/L) and the spring diatom peak (1.35 → 1.44 mgC/L) rise modestly, succession stays intact, 0 NaN, no diatom/OPA summer takeover; an earlier "summer-P crashes cyano" report was a wrong-column analysis artifact. This is a forcing-realism change, **not** the (settling-fixed) spring-diatom gap. Setting `= 1.0` restores the byte-identical prior baseline.

### Fixed
- Removed 15 unguarded per-timestep debug `write(6,*)` statements in the pelagic light-limitation path (`aquabc_II_pelagic_lib_{DIATOMS,CYANOBACTERIA,FIX_CYANOBACTERIA}.f90`, `smith == 1` branch) that emitted ~19 GB of stdout over a 5-yr CL29 run. The `LIM_LIGHT` computation is unchanged; no numerical effect.

## [0.3.0] - 2026-07-10

### Added
- **Curonian Lagoon 29-box application (CL29):** an EUTROPY-derived 29-basin production input set generated by `tools/eutropy_poc/eutropy_to_estas.py` (forcing, boundaries, and initial conditions mapped onto the Curonian Lagoon box topology; opt-in sediment diagenesis)
- **Two-type (sandy/muddy) sediment profiles:** `SED_TYPEMAP::ASSIGN_SED_PROFILES_TO_BOXES` (`SOURCE_CODE/ESTAS/mod_SED_TYPEMAP.f90`) maps per-type geometry/IC profiles onto per-box arrays, with unit test `tests/fortran/test_sed_typemap`
- **Reductive Fe(III)–P coupling** as sediment constant `FE_P_REDOX_FRAC` (`W_SED_CONST` #171): scales PO₄ solid sorption by `(1−f) + f·MULT_FE_III_PART`, releasing bound P as Fe(III) is reduced under anoxia; **off (0.0) by default**. Converter knob `CL29_SED_FE_P_REDOX_FRAC`
- **`CL29_SED_ADVANCED_REDOX` converter flag:** enables the sediment anoxic Mn/Fe/SO₄ DON/DOP mineralization pathways for the CL29 application
- **Per-box benthic flux output:** `SEDIMENT_FLUX_OUTPUTS.out` now writes each box's own sediment→water fluxes (previously emitted one box's fluxes for every box)
- **Diagnostics panel** in the Shiny app (`shiny_app/diagnostics.py`, `diagnostics_plots.py`): live process-rate and state-variable diagnostics driven by a central reactive poll, integrated into the main UI
- **"Scientific Observatory" UI redesign**: custom dark theme via external CSS (`shiny_app/www/aquabc.css`), shinyswatch removed, dashboard restructured with a horizontal status bar (5/7 layout), full-width Parameters panel, and a light/dark theme switcher
- **In-app getting-started tutorial** (step-by-step UI walkthrough opening in a separate window), an automated tutorial runner (`tools/run_tutorial.py`), and Playwright E2E tests that follow the tutorial steps
- **Process-rate analysis toolkit** (`tools/deep_process_rate_analysis.py`, `deep_state_vs_process_crosscheck.py`, `process_rate_slot_map.py`, `analyse_process_rates.py`, `aquabc_analysis_utils.py`): 16-check mass-balance and state-vs-process cross-check analysis over model output. Added 59 lines to `sub_WRITE_PELAGIC_OUTPUT.f90` to emit the required per-process rate output
- 365-day and 3,560-day (10-year) deep-process analysis runs and reports
- Analysis, cross-validation, and reference documentation as PDFs under `docs/`: process-rate and Fixes & Results reports, code-vs-paper cross-validation (Ertürk et al. 2023), AQUABC constant reference, and the ESTAS-AQUABC Integration Guide; plus the "Scientific Observatory" UI design/implementation plan under `docs/plans/`
- **Bioturbation module** (`aquabc_II_sediment_bioturbation.f90`): depth-dependent biodiffusion, oxygen-dependent scaling, seasonal modulation, bioirrigation enhancement, and zero-flux lower boundary condition (Boudreau 1997; Soetaert et al. 1996)
- Dynamic particle mixing coefficients (`switch_partmixing = 1`): biodiffusion recomputed every sub-timestep from local depth, O₂, and day-of-year
- Bioirrigation enhancement of porewater diffusion for dissolved-phase species
- 23 unit tests for all bioturbation functions (depth attenuation, O₂ scaling, seasonality, effective Db, bioirrigation, array application, last-layer BC)
- Integration test input (`INPUT_sediment_test.txt`) for running the coupled model with full sediment diagenesis
- AQUABC and ESTAS PDF reference manuals (`docs/AQUABC_Reference_Manual.pdf`, `docs/ESTAS_Reference_Manual.pdf`)
- Makefile target `make build-docs` for PDF generation via pandoc
- Unit tests for FIX_CYANOBACTERIA, OTHER_PLANKTONIC_ALGAE, and NOSTOCALES kinetics subroutines (25 test programs, 196 assertions total)
- Dependabot configuration (`.github/dependabot.yml`) for weekly pip and GitHub Actions dependency scanning
- `requirements-dev.txt` for development/test Python dependencies (ruff, pytest)
- Comprehensive AQUABC model equations reference document (`AQUABC_Model_Equations.md`)
- `pyproject.toml` with ruff linter configuration (E, F, W, I, UP, B, S rules)
- 46 Python unit tests for parsers (`parameter_parser`, `ic_parser`, `options_parser`, `simulation_config`) and `safe_resolve`
- 19 Playwright integration tests covering app startup, navigation, and all major panels
- 9 Selenium integration tests (gracefully skip without chromedriver)
- CI `python-lint-test` job running ruff and pytest in parallel with Fortran build
- `CONTRIBUTING.md` with build/test/PR workflow documentation
- Makefile convenience targets: `make test-all`, `make test-python`, `make test-fortran`, `make lint`

### Changed
- Zooplankton boundary conditions and initial-condition sets updated per the 3,560-day analysis recommendations
- Sediment model particle mixing now uses bioturbation physics (exponential depth decay × Monod O₂ × seasonal) instead of uniform constant
- Last-layer particle mixing boundary condition changed from hard-coded zero to proper zero-flux (Neumann) BC
- Updated `AQUABC_Model_Equations.md` with bioturbation/bioirrigation equations (§13.8) and references
- Cardinal Temperature Model with Inflection (CTMI, Rosso et al. 1993) replacing piecewise-exponential temperature response for phytoplankton growth
- Synthesizing Unit nutrient colimitation (Saito et al. 2008) replacing Liebig's Law of the Minimum for all phytoplankton groups
- Tunable Platt-style photoinhibition (BETA parameter) for light limitation in all phytoplankton groups
- Unified precision type definitions via `precision_kinds` module
- Compiler warning flags (`-Wall -Wextra`) for release and fast builds
- OpenMP parallelization of pelagic kinetics nkn loop: single parallel region with chunked array slicing, ~750 line-level changes in `aquabc_II_pelagic_model.f90`, all library subroutine calls use per-thread derived type bundles, debug calls guarded with `!$omp master` barriers, serial fallback via Fortran sentinel comments
- Bundled scalar constant arguments into derived types (`t_diatom_params`, `t_cyn_params`, `t_opa_params`, etc.) for 6 kinetics subroutines
- Bundled environmental input arrays into `t_phyto_env` derived type in 7 phytoplankton subroutines
- Bundled ORGANIC_CARBON_MINERALIZATION I/O arrays into 3 new shared pointer types (`t_redox_state`, `t_redox_lim`, `t_docmin_outputs`), reducing arguments from 36 to 9
- Bundled REDOX_AND_SPECIATION I/O arrays into shared pointer types, reducing arguments from 33 to 12
- Removed 5 dead arguments (`K_NO3_RED`, `K_MN_IV_RED`, `K_FE_III_RED`, `K_S_PLUS_6_RED`, `K_DOC_RED`) from ORGANIC_CARBON_MINERALIZATION
- Replaced hardcoded dimension magic numbers with named constants throughout
- Replaced tabs with spaces in 15 source files for consistent formatting

### Fixed
- **FIX_CYN photosynthetic O₂ production omitted (HIGH):** when both `DO_NOSTOCALES` and `DO_NON_OBLIGATORY_FIXERS` were enabled, nitrogen-fixing cyanobacteria O₂ production was dropped from the dissolved-oxygen derivative (slot 5 held Nostocales only). Repurposed rate slot 19 to carry FIX_CYN O₂ production. **Changes simulated dissolved-oxygen results.**
- **DON-uptake N:C ratio (MEDIUM):** FIX_CYN dissolved-organic-nitrogen uptake used `CYN_N_TO_C` instead of `FIX_CYN_N_TO_C` (copy-paste error), producing a nitrogen mass imbalance. **Changes simulated nitrogen pools.**
- **Light-limitation negativity:** the Steele/Platt formula produced tiny negatives (~−4e-4) at dusk when surface irradiance approaches zero; now clamped to [0,1] in all 8 code paths (central `LIM_LIGHT` plus inline Steele in DIA/OPA/CYN/FIX_CYN/NOST)
- Detritus C:N ratio correction
- Corrected model input values and initial-condition sets from long-run value analysis and code-vs-paper cross-validation; regenerated forcing time-series
- Shiny plot panel mis-parsing `PROCESS_RATES` output files
- Diagnostics panel reliability: thread-safe mutable dict instead of `reactive.Value`, and `suspend_when_hidden=False` + central poll so inactive-tab outputs refresh
- WCONST parser fix (added units column and value justifications to the constant-reference PDF)
- **Missing `isedi` parameter**: registered `isedi = 0` in ESTAS `INIT_BSED_MODEL_CONSTANTS` — was expected from SHYFEM parameter system but never provided in standalone ESTAS
- **Sediment flux output format**: corrected format descriptor from `33F20.10` to `36F20.10` to match actual `FLUXES_OUTPUT_TO_WATER_COLUMN` array size (`nstate + NUM_ALLOLOPATHY_STATE_VARS = 36`)
- **`FE_III_DISS` read uninitialized (correctness):** the unsaturated (`elsewhere`) branch of the Fe(III) dissolution `where` block in `AQUABC_SEDIMENT_MODEL_1` left `FE_III_DISS` unset before it was read in `SED_DOC_MINERALIZATION` (`LIM_FE_III_RED`), so advanced-redox sediment results depended on stack garbage (non-deterministic across builds). Now assigned in both branches; verified via `-finit-real=snan` (no trap) and `-O2 == -finit-local-zero` (bit-identical)
- **Sediment advanced-redox metal-dissolution NaN crash:** dissolved Fe/Mn fractions could blow up toward a near-zero metal pool and self-propagate via saved outputs; clamped to finite `[0,1]` with an `IEEE_IS_FINITE` guard in `CALC_DISS_ME_CONC`
- **Sediment→water flux buffer not zeroed:** stale allelopathy-tail values (cols 33–36) corrupted secondary-metabolite derivatives; now zero `FLUXES_TO_WATER_COLUMN` before the sediment-to-water mapping
- Fixed all 653 ruff lint warnings across `shiny_app/*.py` (whitespace, imports, type annotations, bare excepts)
- Fixed 3 zero-valued Monod half-saturation defaults (`K_HS_DOC_MIN_DOXY=0→1.0`, `K_HS_DON_MIN_DOXY=0→0.05`, `K_HS_DOP_MIN_DOXY=0→0.052`) that caused NaN when substrate concentration reached zero
- 25 Fortran unit tests (196 assertions) covering kinetics subroutines (DIATOMS, CYANOBACTERIA, FIX_CYANOBACTERIA, OTHER_PLANKTONIC_ALGAE, NOSTOCALES, ZOOPLANKTON, REDOX_AND_SPECIATION, ORGANIC_CARBON_MINERALIZATION) plus utility subroutines
- Shared test defaults module (`test_defaults.f90`) with realistic parameter populators for all derived types
- Guarded ~25 division-by-zero risks in sediment model (porosity, depth, mixing length, pH-to-H+ conversions)
- Guarded REDOX speciation divisions against zero/negative values in pelagic model
- Guarded CO2SYS critical divisions and discriminant
- Guarded macroalgae quota divisions against zero biomass
- Guarded state variable ratio divisions in pelagic model (Fe/Mn dissolved fractions)
- Added pH, temperature, and salinity clamping at model entry points
- Replaced raw `exp()` with `safe_exp()` in light limitation calculations to prevent overflow
- Numerical hardening: zooplankton switching, parameter validation, cyanobacteria cleanup
- Fixed 13 critical ecological model bugs across pelagic, sediment, and macroalgae
- Fixed Mn speciation, leap year, bathymetry initialization, and NaN guards
- Fixed settling suppression bug, allocation error checks, and getpar precision
- Added `implicit none` to all remaining subroutines and functions
- Removed 138 unused variable declarations across 25 source files
- Removed 5 unused parameter imports and declarations
- Resolved compare-reals and real-to-integer conversion warnings
- Initialized variables that may be used uninitialized
- Resolved character truncation warning in WRITE_PELAGIC_MODEL_CONSTANTS
- Fixed missing `precision_kinds` dependency in 6 test targets
- Removed ~250 lines of dead/commented-out code across 9 source files
- Fixed `shinywidgets` version pin (0.7.0 → 0.7.1) in `requirements.txt`

### Security
- Added `safe_resolve()` path traversal protection to all Shiny app file operations (`load_file`, `save_file`, `file_info_panel`, `validate_constants_file`, observation handlers)
- Added subprocess timeouts (120s clean, 600s build) with `kill()` on timeout to prevent hung processes
- Added bounded output buffers to all subprocess stdout readers

---

## [0.2.1] - 2026-01-22

### Added
- Pop-up tooltips throughout the entire interface for all major controls
- Changelog widget in the app header bar
- Output directory now saved/loaded with simulation configuration
- Two-column layout for output boxes selection
- CI integration with ftnchek static analysis for Fortran code quality
- Fortran unit tests for VECTOR_MATRIX_UTILS module

### Changed
- Build Options button moved inside Run Parameters card (cleaner layout)
- Output Directory moved from Simulation Config to Output Config tab
- Reduced vertical spacing in Run Parameters card for compactness
- Reduced text size in run log display
- Increased run log buffer from 50KB to 200KB
- Sediment Model disabled by default
- WCONST_04.txt set as visible default for Pelagic Constants File

### Fixed
- Intel Fortran (ifx) compiler linking with runtime libraries (-lifcore -lifport)
- Allelopathy kinetics: added missing concentration term for first-order degradation
- Allelopathy kinetics: initialize SEC_METAB state variables (33-36) before rate calculations
- Removed redeclaration of S_SEC_METAB_* variables to avoid module conflicts

---

## [0.2.0] - 2026-01-21

### Added
- Active Switching Model for zooplankton food selection
  - Implements Gentleman et al. (2003) switching formulation
  - Configurable via MODEL_SWITCHES.txt (K_ZOO_SWITCH parameter)
  - Documentation in docs/ZOOPLANKTON_SWITCHING_MODEL.md
- Multi-compiler build support (gfortran, ifort, ifx)
- Shiny frontend for model configuration and execution
  - Dashboard with quick run controls
  - Model build panel with compiler selection
  - Simulation configuration with presets
  - Parameter editor with categories
  - Initial conditions editor
  - Model options and switches editor
  - Scenario presets system
  - Results visualization with dual-axis plots
  - Mass balance calculations
  - Model validation with observations

### Changed
- Upgraded from AQUABC 0.1 to 0.2 with improved kinetics
- Reorganized source code structure

### Fixed
- Various numerical stability improvements

---

## [0.1.0] - 2025-01-01

### Added
- Initial release of AQUABC biogeochemical model
- Basic pelagic kinetics for:
  - Phytoplankton (Diatoms, Cyanobacteria, Other Phytoplankton)
  - Zooplankton
  - Dissolved oxygen
  - Nutrients (N, P, Si, Fe)
  - Organic matter (DOC, POC, DON, PON, DOP, POP)
- Bottom sediment diagenesis model (optional)
- Fortran 90 implementation

---

*This changelog is automatically displayed in the AQUABC Shiny app header.*
