# CL29 Full Sediment Diagenesis — Phase 1: Stand Up & Stabilize

**Date:** 2026-07-08 (rev. 2, after adversarial review loop)
**Status:** Design (under review), pending implementation plan
**Goal:** Enable the full coupled sediment diagenesis model (`MODEL_SEDIMENTS = 2`) for the
CL29 29-box application with a uniform sediment profile, and get a **stable, clean-checkout,
5-year run** — the foundation the later calibration phase builds on.

> **Project context.** Phase 1 of a two-phase project chosen over the prescribed-flux
> subsidy (superseded `2026-07-07-cl29-benthic-p-recycling-design.md`). **Phase 1 is "make it
> run stably" only** — no calibration, no per-box differentiation, no spring-gap claim.
>
> **Rev-2 note.** A three-lens review (correctness / gaps / stability) refined this spec.
> Biggest changes: (1) the carbonate-IC stability fix is now **empirical**, not a guess — the
> three reviews split on whether the sediment IC is already realistic, so the staged run
> decides (CO2SYS fails loud); (2) the negative-mass clamp is **silent**, so the stability gate
> is now output-based, not a non-existent warning; (3) runtime is likely **hours** → a measured
> extrapolation gate + `OPENMP=1`; (4) `PRINT_INTERVAL` is a single global cadence gate; (5) the
> dominant summer cyano doesn't settle, so the benthic source is spring/detritus-weighted.

---

## 1. Background & why the full model

The CL29 spring diatom bloom is phosphorus-supply-limited (`docs/CL29_Parameter_Validation.md`)
and the model has **no benthic P source** (`MODEL_SEDIMENTS = 0`). The full diagenesis model
supplies benthic N+P from deposited organic matter mechanistically (deposition →
mineralization → pore-water diffusion → flux to the water column), mass-conservatively, with
native N:P coupling. Its cost is complexity, runtime, and CO2SYS-stability risk — which is why
Phase 1 isolates "does it run stably" before any calibration.

## 2. Scope

**In scope (Phase 1):** generate the two sediment input files into `INPUTS_CL29/` and wire
`INPUT_CL29.txt` for `MODEL_SEDIMENTS = 2` as an **opt-in converter toggle, off by default**;
achieve a stable 5-year run (no NaN, no CO2SYS `stop`, no silent negative-mass collapse) with
finite, plausibly-signed benthic fluxes; keep the pipeline clean-checkout reproducible and the
sediments-off baseline byte-identical; manage output volume and runtime.

**Out of scope (→ Phase 2):** calibrating sediment ICs/constants to measured Curonian
sandy/muddy fluxes; per-box sediment differentiation (Fortran reader change); verifying the
spring diatom gap closes; N:P / guardrail validation; making CYN_C settle (see §7). Phase 1
does **not** claim the diatom bloom improves — only that the mechanism runs stably and produces
sane fluxes.

## 3. What the model needs (verified against source)

The reader `READ_BOTTOM_SEDIMENTS_MODEL_INPUTS` (`mod_BOTTOM_SEDIMENTS.f90:317-486`) is
positional and broadcasts a **single sediment profile to all 29 boxes** (per-box would need
Fortran). Deposition is **automatic** — assembled from pelagic settling velocities +
phyto/zoo/detritus (`FLX_ALUKAS_II_TO_SED_MOD_1_VEC`, `aquabc_II_pelagic_auxillary.f90:1136-
1344`; called `mod_SOLVER.f90:1478-1489`). No deposition config is authored. `SETTLING_VELOCITY_
TS_1..6` already exist; `DEPOSITED FRACTION = 0.9` for all vars (`eutropy_to_estas.py:402`).
`RESUSPENSION_OPTION = 0` is compatible (the `>1`+resuspension `stop` guard,
`mod_AQUATIC_MODEL.f90:495-501`, is not tripped). The pelagic↔sediment advanced-redox mismatch
check (`mod_AQUATIC_MODEL.f90:513-532`) only **warns**. Absent from `INPUTS_CL29/` today: both
sediment files; the converter has no sediment logic.

## 4. Architecture — converter changes

A single opt-in toggle, one new writer, and a rewired `INPUT_CL29.txt` sediment block.

```python
CL29_ENABLE_SEDIMENTS = False   # opt-in; default off keeps the baseline byte-identical
```

**New/changed converter functions** (testability — see §7): add an `enable_sediments`
parameter to `_write_input_txt`, and a new `_write_sediment_inputs(out, enable_sediments)`
that does steps 1–2 below reading templates from `INPUTS/`. Both take an output-dir arg so a
unit test can drive them without the destructive `main()` (`shutil.rmtree` + `os.getcwd()`
writes); tests run from repo root, matching the existing `tests/python/` convention.

When `enable_sediments` is `True`:

1. **Copy `W_SED_CONST.txt`** (170 constants, indices 1..170) verbatim from `INPUTS/` into
   `INPUTS_CL29/` (it is opened relative to the pelagic input folder,
   `mod_BOTTOM_SEDIMENTS.f90:455-457`, so it must live there; references no other files).
2. **Write `INPUTS_CL29/BOTTOM_SEDIMENT_MODEL_INPUT.txt`** from the template
   (`INPUTS/BOTTOM_SEDIMENT_MODEL_INPUT.txt`) as a **verbatim copy with exactly one mandatory
   edit** (the template embeds no paths; coefficient-file and output names are bare):
   - **ADVANCED REDOX flag `1` → `0`** (template ships as 1; must match CL29 pelagic
     `PELAGIC_MODEL_OPTIONS.txt` line 4 = 0, else sediment Fe/Mn/S/CH4 chemistry runs unfed).
   - Keep `NUM_SED_LAYERS = 7`, the layer geometry, and the 24-var×7-layer IC block **as the
     template** — **except** the carbonate ICs are resolved empirically (see §5).
   - Keep the six output-filename lines **bare** (they open as
     `OUTPUTS_CL29/ // <bare name>`; a path there would `open()` into a nonexistent dir and
     crash — ESTAS never creates dirs). Only `BOTTOM_SEDIMENTS_OUTPUTS.out` and
     `SEDIMENT_FLUX_OUTPUTS.out` are actually produced (the 4 COCOA files are gated off by
     CL29's `PRODUCE_COCOA_OUTPUTS = 0`, never read). `OUTPUTS_CL29/` already exists (the
     converter `makedirs` it), so no new dir or path-wiring is needed.
3. **Rewrite the `INPUT_CL29.txt` sediment block** (`_write_input_txt`, currently
   `eutropy_to_estas.py:500-503`) to the `== 2` layout (`mod_AQUATIC_MODEL.f90:495-511`):
   ```
   # RESUSPENSION_OPTION
             0
   # MODEL_SEDIMENTS
             2
   # BOTTOM SEDIMENT MODEL INPUT FILE
   BOTTOM_SEDIMENT_MODEL_INPUT.txt
   ```
   **The `NUM_PRESCRIBED_SEDIMENT_FLUX_SETS` line must be ABSENT** — it is read only under
   `== 1`. If left in, the parser consumes `# NUM_PRESCRIBED…` as the file-block header and the
   `0` as the filename → `open("INPUTS_CL29/0")` → crash. (Not cosmetic — an active trap.)
4. **Raise `PRINT_INTERVAL`** — see §6.

When `enable_sediments` is `False`: emit none of the above; the sediment block writes exactly
today's `RESUSPENSION_OPTION 0` / `MODEL_SEDIMENTS 0` / `NUM_PRESCRIBED_SEDIMENT_FLUX_SETS 0` /
bare `# SEDIMENT MODEL INPUT FILE` lines (10-space indent), `PRINT_INTERVAL 10`, and no
sediment files → byte-identical baseline (snapshot-tested).

## 5. Stability plan — carbonate ICs resolved empirically (the crux)

The dominant risk is the sediment CO2SYS: it runs **per layer, per box, twice per solver
step**, unconditionally, iterating Newton's method; on non-convergence it prints
`'pH does not converge'` and **`stop`s the whole program** (`aquabc_II_co2sys.f90:3103-3114`) —
fail-loud, no NaN. So a bad carbonate setup is caught at ~step 1, not silently.

**The carbonate-IC question is genuinely contested** and must be resolved by the run, not a
guess:
- *Physical reading:* both sediment and pelagic feed CO2SYS with the same ×1e6 scaling
  (`aquabc_II_sediment_model_1_fast.f90:1010`; `aquabc_II_pelagic_model.f90:379`), so the
  template's sediment `INORG_C ≈ 0.00301` / `TOT_ALK ≈ 0.00297` maps to a realistic ~3000
  µmol/kg pore-water DIC — i.e. **already correct**, and copying the pelagic 3.0/3.1 (→3×10⁶
  µmol/kg) would be physically absurd.
- *Codebase-empirical reading:* this project's pelagic CO2SYS is known-fragile — the 0-D
  default 0.0027 was non-convergent and the working model uses 3.0/3.1 (project memory /
  `docs`), i.e. the same solver empirically needs the inflated magnitude.

**Resolution (staged bring-up):**
1. First short run (~30–60 d) with the **template carbonate ICs (0.003)** unchanged.
2. If CO2SYS `stop`s (`'pH does not converge'`, caught at ~step 1), raise the sediment
   `INORG_C/TOT_ALK` to the codebase-working magnitude (~3.0/3.1) and retry. Document **both
   readings inline** so a future reader doesn't "fix" it back blindly.
3. Once the short run is CO2SYS-clean, address negative-mass (below), then extend to 1826 d.

**Negative mass — the clamp is SILENT.** `where (FINAL_SED_STATE_VARS <= 0) = 0`
(`mod_SOLVER.f90:1515-1517`) has **no warning or counter** (unlike the pelagic
`'NEGATIVE MASS PREDICTED'` path). The clamped state persists (`mod_SIMULATE.f90:312`), so a run
can complete cleanly while silently creating mass every step. The §7 gate is therefore
**output-based**: assert sediment concentrations in `BOTTOM_SEDIMENTS_OUTPUTS.out` are **not
pinned at the 0 floor** (collapsed pools = a broken-but-passing run). (Optional aid: add a
lightweight clamp counter mirroring the pelagic pattern; not required if the output check
suffices.)

**Depth-division / shallow boxes.** The sediment→water flux is divided by water depth
`DRIVING_FUNCTIONS(:,8) = VOLUME/SURFACE_AREA` (`mod_SOLVER.f90:1556-1563`). `ESTAS_HOLD_VOLUME=1`
(already used by CL29) keeps depth **constant and positive** — good. Bring-up checklist: verify
`DRIVING_FUNCTIONS(8)` is positive and not tiny for all 29 boxes (a very shallow box *amplifies*
the benthic return flux → larger concentration kicks, a stability factor; a zero would be a
divide-by-zero → NaN).

## 6. Output & runtime management

- **Output volume.** `BOTTOM_SEDIMENTS_OUTPUTS.out` writes one line **per box per layer**
  (29×7 = 203) per print step (`mod_SIMULATE.f90:716-724`) → ~8.9M lines at `PRINT_INTERVAL=10`.
- **`PRINT_INTERVAL` is a single global cadence gate** (`mod_SIMULATE.f90:407`) — the only lever,
  and it governs **all** pelagic output too. When sediments are enabled the converter writes
  `PRINT_INTERVAL = 240` (daily) instead of `10` (hourly), cutting output ~24×. **This coarsens
  pelagic output from hourly to daily as well** — acceptable (all downstream analysis subsamples
  to daily; `tools/generate_3560day_analysis_pdf.py` already documents daily cadence), but the
  plan must confirm the other CL29 consumers (`shiny_app/app.py`,
  `shiny_app/simulation_config.py`, `tools/run_tutorial.py`) tolerate daily cadence. Sediments-off
  keeps `PRINT_INTERVAL = 10` so the baseline stays byte-identical.
- **Runtime is likely HOURS, not minutes.** Sediments add ~14 vectorized CO2SYS sweeps/step
  (7 layers × 2) plus diagenesis/transport/STRANGER scans; a pelagic-only CL29 5-yr is already
  ~2.5–6 h (extrapolating the tutorial's 25-box/1-yr = 30–60 min), and sediments plausibly ×3–8
  → **~8 h to >1 day serial.** The plan must: (a) **measure wall-time on the 30–60 d staged run
  and extrapolate linearly** (steps scale with days), and only launch the full run if the
  projection is within an explicit budget; (b) build `make OPENMP=1` (the sediment CO2SYS
  parallelizes over layers); (c) optionally set `debug_stranger=.false.`
  (`aquabc_II_sediment_model_1_fast.f90:629`) for the production run to drop ~5 diagnostic scans
  (trading away some fail-loud NaN detection — gate behind the staged run).

## 7. Validation & testing

- **Python unit test** (`tests/python/`), no ESTAS run: with `enable_sediments=True`, assert
  `INPUT_CL29.txt` has the `== 2` layout (`MODEL_SEDIMENTS 2`, the sediment input-file line, and
  **no** `NUM_PRESCRIBED_SEDIMENT_FLUX_SETS` line), `PRINT_INTERVAL 240`; `W_SED_CONST.txt` +
  `BOTTOM_SEDIMENT_MODEL_INPUT.txt` exist in the output dir with the sediment file's advanced-redox
  flag = 0 and bare output/coefficient names. With `False`: byte-identical baseline (diff vs
  pre-change snapshot; `PRINT_INTERVAL 10`).
- **Clean-checkout:** fresh clone + `enable_sediments=True` → converter runs, `ESTAS_II` builds
  (`OPENMP=1`), model runs.
- **Stability run (primary Phase-1 gate):** staged 30–60 d first (CO2SYS + negative-mass +
  runtime extrapolation), then full 5-yr completes with "simulation finished", **no NaN, no CO2SYS
  `stop`**, and **sediment concentrations not collapsed to the 0 floor** in
  `BOTTOM_SEDIMENTS_OUTPUTS.out`.
- **Sane-flux exit check (makes "runs stably" testable, not a broken run passing):** over a
  **post-year-1 window** (exclude the IC↔deposition spin-up transient), assert the benthic NH4 and
  PO4 return to the water column is (a) finite/non-NaN, (b) correct **sign** (a source for a
  depositional lagoon), (c) within an order-of-magnitude literature band (placeholder: NH4 ~0.5–20,
  PO4 ~0.01–2 mmol m⁻² d⁻¹; tighten in Phase 2), and (d) **not** an artifact of clamp-floor
  collapse. Read the flux from the water-column PO4/NH4 response, **not**
  `SEDIMENT_FLUX_OUTPUTS.out` (which has a last-box write bug: `mod_SIMULATE.f90:718-719` writes
  box `nkn`'s fluxes for every box).

## 8. Success criteria (Phase 1)

- CL29 runs 0→1826 d with `MODEL_SEDIMENTS = 2`, stable (no NaN / CO2SYS `stop` / silent
  negative-mass collapse), "simulation finished".
- Sediments-off baseline byte-identical; pipeline clean-checkout reproducible.
- Benthic N+P return fluxes finite, correctly signed, order-of-magnitude plausible, not
  clamp-floor artifacts (post-year-1).
- Runtime and output size measured and documented within an agreed budget (or the cost quantified
  for a decision), with `OPENMP=1`.

## 9. Risks & mitigations

| Risk | Mitigation |
|---|---|
| Sediment CO2SYS non-convergence (hard `stop`) | Staged run decides carbonate ICs empirically: template 0.003 first, fall back to ~3.0/3.1 if it stops; caught at ~step 1 |
| Silent negative-mass clamp → broken-but-passing run | Output-based gate: sediment concs must not be pinned at the 0 floor; optional clamp counter |
| Runtime blow-up (~hours to >1 day) | Measure on staged run + extrapolate before full run; `OPENMP=1`; optional `debug_stranger=.false.` |
| Output volume (~8.9M lines) | `PRINT_INTERVAL 240` when sediments on (also coarsens pelagic → confirm consumers) |
| Year-1 coupling shock (IC↔deposition mismatch) triggering §CO2SYS/§clamp | Staged bring-up; exclude year-1 transient from flux checks; watch collapse during year 1 |
| Shallow-box flux amplification / divide-by-zero | Verify `DRIVING_FUNCTIONS(8)` positive/not-tiny for all boxes; HOLD_VOLUME keeps it fixed |
| Uniform sediment profile (no per-box sand/mud) | Documented Phase-1 limitation; fluxes still vary via deposition; per-box = Phase 2 (Fortran) |
| Baseline regression | Opt-in/off-by-default + byte-identical snapshot test |

## 10. Realism caveats (documented, not fixed in Phase 1)

- **Uniform sediment profile** across all 29 boxes (per-box properties need a Fortran reader
  change → Phase 2). Fluxes still vary spatially via deposition.
- **Summer cyano does not deposit.** CL29 assigns settling only to DIA (5), DET C/N/P (9/10/11),
  OPA (16), FIX_CYN (18) (`eutropy_to_estas.py:392`); **CYN_C (15), the dominant summer bloom, has
  zero settling**, as do ZOO / PART_Si / NOSTOCALES / AKI. So the benthic organic-matter (and
  hence N+P) source is **spring-diatom- and detritus-weighted, with no direct summer-cyano
  feedback** — ironic given that feedback dominates the real lagoon. Whether to give CYN a settling
  velocity is a Phase-2 decision.
- `SEDIMENT_FLUX_OUTPUTS.out` last-box write bug (`mod_SIMULATE.f90:718-719`) — noted, fixed in
  Phase 2 if that file is needed.

## 11. Out of scope / future (Phase 2+)

Calibrate sediment ICs/constants to measured Curonian sandy/muddy N+P fluxes; verify spring gap
closure + N:P/guardrail validation; per-box sediment properties; CYN settling; fix the flux-output
bug.

## 12. References

- `docs/CL29_Parameter_Validation.md` — P-supply root cause + confirmation.
- `docs/superpowers/specs/2026-07-07-cl29-benthic-p-recycling-design.md` — superseded
  prescribed-flux alternative.
- ESTAS/AQUABC source: `mod_AQUATIC_MODEL.f90:459-547` (parse, redox check, output opens),
  `mod_BOTTOM_SEDIMENTS.f90:317-486` (sediment input reader, single-profile broadcast),
  `aquabc_II_pelagic_auxillary.f90:1136-1344` (deposition assembly),
  `aquabc_II_sediment_model_1_fast.f90` (diagenesis; `:1010` CO2SYS scaling; `:629` debug_stranger),
  `aquabc_II_co2sys.f90:3103-3114` (non-convergence `stop`),
  `mod_SOLVER.f90:1478-1563` (deposition call, flux÷depth), `:1515-1517` (silent clamp),
  `mod_SIMULATE.f90:407` (PRINT_INTERVAL gate), `:716-724` (sediment output),
  `eutropy_to_estas.py:392,402,468,494,500-503` (settling, deposited fraction, COCOA, PRINT_INTERVAL,
  sediment block).
