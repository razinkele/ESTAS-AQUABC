# CL29 Full Sediment Diagenesis — Phase 1: Stand Up & Stabilize

**Date:** 2026-07-08
**Status:** Design (under review), pending implementation plan
**Goal:** Enable the full coupled sediment diagenesis model (`MODEL_SEDIMENTS = 2`) for the
CL29 29-box application with a uniform sediment profile, and get a **stable, clean-checkout,
5-year run** — the foundation the later calibration phase builds on.

> **Project context.** This is Phase 1 of a two-phase project chosen over the
> prescribed-flux subsidy (see the superseded
> `2026-07-07-cl29-benthic-p-recycling-design.md`, kept as the explored alternative).
> The full model natively couples N+P+redox and is mass-conservative. **Phase 1 is
> "make it run stably" only** — no calibration, no per-box differentiation, no spring-gap
> validation. Those are Phase 2.

---

## 1. Background & why the full model

The CL29 spring diatom bloom is phosphorus-supply-limited (`docs/CL29_Parameter_Validation.md`)
and the model has **no benthic P source** (`MODEL_SEDIMENTS = 0`). The full diagenesis
model supplies benthic N+P from deposited organic matter mechanistically (deposition →
mineralization → pore-water diffusion → flux to the water column), mass-conservatively, and
with native N:P coupling — avoiding the prescribed-flux approach's external-source and N:P
artifacts. Its cost is complexity, runtime, and CO2SYS-stability risk, which is exactly why
Phase 1 isolates "does it run stably" before any calibration.

## 2. Scope

**In scope (Phase 1):**
- Generate the two sediment input files into `INPUTS_CL29/` and wire `INPUT_CL29.txt` for
  `MODEL_SEDIMENTS = 2`, as an **opt-in converter toggle, off by default**.
- Achieve a stable 5-year (0→1826 d) run: no NaN, no CO2SYS non-convergence, no runaway
  negative-mass; benthic fluxes finite and physically plausible (order-of-magnitude sane).
- Keep the pipeline clean-checkout reproducible; keep the sediments-off baseline byte-identical.
- Manage the sediment output volume.

**Out of scope (→ Phase 2):** calibrating sediment ICs/constants to measured Curonian
sandy/muddy fluxes; per-box sediment differentiation (needs a Fortran reader change);
verifying the spring diatom gap closes; N:P / guardrail validation. Phase 1 does **not**
claim the diatom bloom improves — only that the mechanism runs stably and produces sane
fluxes.

## 3. What the model needs (verified against source)

The reader `READ_BOTTOM_SEDIMENTS_MODEL_INPUTS` (`mod_BOTTOM_SEDIMENTS.f90:317-486`)
broadcasts a **single sediment profile to all 29 boxes** (per-box would need Fortran).
Deposition is **automatic** — assembled from the pelagic settling velocities + phyto/zoo/
detritus CL29 already simulates (`FLX_ALUKAS_II_TO_SED_MOD_1_VEC`,
`aquabc_II_pelagic_auxillary.f90:1136-1344`; called `mod_SOLVER.f90:1478-1489`). No
deposition config is authored. The pelagic settling-velocity TS (`SETTLING_VELOCITY_TS_1..6`)
already exist. `RESUSPENSION_OPTION = 0` is compatible (the `>1` + resuspension `stop` guard
is not tripped). The pelagic↔sediment advanced-redox check only **warns** on mismatch
(`mod_AQUATIC_MODEL.f90:513-534`), it does not stop.

Absent from `INPUTS_CL29/` today: both sediment files. The converter has no sediment logic.

## 4. Architecture — converter changes

A single opt-in toggle plus one new writer/copy step and a rewired `INPUT_CL29.txt` sediment
block.

```python
CL29_ENABLE_SEDIMENTS = False   # opt-in; default off keeps the baseline byte-identical
```

When `True`, the converter (`eutropy_to_estas.py`):

1. **Copies `W_SED_CONST.txt`** (170 sediment constants) verbatim from `INPUTS/` into
   `INPUTS_CL29/` (alongside the existing WCONST copies). Opened by the model from the
   pelagic input folder, so it must live in `INPUTS_CL29/`.
2. **Writes `INPUTS_CL29/BOTTOM_SEDIMENT_MODEL_INPUT.txt`** from the template
   (`INPUTS/BOTTOM_SEDIMENT_MODEL_INPUT.txt`) with exactly these deliberate values:
   - **ADVANCED REDOX flag = 0** (match CL29 pelagic `PELAGIC_MODEL_OPTIONS.txt` line 4 = 0;
     avoids the warning, keeps Fe/Mn/S/CH4 off in both).
   - `NUM_SED_LAYERS = 7`, per-layer depths/porosities/densities and the scalar
     advective-velocity / particle-mixing / burial / surf-mixlen — copied from the template
     (uniform Curonian profile; retuning deferred to Phase 2).
   - The **24-var × 7-layer initial-condition block** — copied from the template, **except**
     the carbonate ICs (see §5 stability): sediment `INORG_C` / `TOT_ALK` set to realistic
     values if the template's low (~0.003) values cause CO2SYS non-convergence.
   - `# BSED_MODEL_COEFFICIENT_FILE` → `W_SED_CONST.txt`; the six output filenames.
3. **Rewrites the `INPUT_CL29.txt` sediment block** (`_write_input_txt`, currently
   `eutropy_to_estas.py:500-503`) to the `== 2` layout the reader expects
   (`mod_AQUATIC_MODEL.f90:495-511`):
   ```
   # RESUSPENSION_OPTION
             0
   # MODEL_SEDIMENTS
             2
   # BOTTOM SEDIMENT MODEL INPUT FILE
   BOTTOM_SEDIMENT_MODEL_INPUT.txt
   ```
   The `NUM_PRESCRIBED_SEDIMENT_FLUX_SETS` line must **not** be present in the `== 2` layout
   (it is only read under `== 1`).
4. **Optionally raises `PRINT_INTERVAL`** for sediment runs (see §6).

When `False`: emit none of the above; keep `MODEL_SEDIMENTS 0` +
`NUM_PRESCRIBED_SEDIMENT_FLUX_SETS 0` + the empty sediment-file block exactly as today →
baseline byte-identical.

## 5. Stability plan (the crux)

The dominant risk is the sediment CO2SYS: it runs **per layer, per box, ~2×/solver step**,
unconditionally (`aquabc_II_sediment_model_1_fast.f90:992-1057, 2020-2161`;
`SINGLE_VECTOR_CO2SYS = 0`). The scheme also clamps negatives
(`mod_SOLVER.f90:1515-1517`), signalling it can push sediment vars negative.

**Anticipated failure & ready mitigation.** The template sediment carbonate ICs
(`INORG_C ≈ 0.00301`, `TOT_ALK ≈ 0.00297`) are the same order as the pelagic `0.0027` that
historically broke the pelagic CO2SYS (fixed by setting pelagic `INORG_C/TOT_ALK = 3.0/3.1`).
If the sediment CO2SYS fails to converge with the template values, raise the sediment
carbonate ICs to realistic pore-water DIC/alkalinity — the direct analog of the pelagic fix.
(Confirm sediment carbonate units first, since the template value may already be correct in
sediment units.)

**Staged bring-up** (to localize any failure fast):
1. Short run (~30–60 d) with sediments on → catch immediate CO2SYS non-convergence / NaN /
   negative-mass at step 1–100.
2. If it fails, apply the carbonate-IC mitigation (and/or inspect the offending layer/var via
   the negative-mass clamp warnings) and repeat.
3. Once the short run is clean, extend to the full 1826 d.

Note `ESTAS_HOLD_VOLUME=1` (already used by CL29) keeps water-column volume fixed, which
helps overall mass stability.

## 6. Output-volume management

With `MODEL_SEDIMENTS > 1`, `BOTTOM_SEDIMENTS_OUTPUTS.out` writes one line **per box per
layer** (29×7 = 203) every print step (`mod_SIMULATE.f90:716-744`) → ~8.9M lines at the
current `PRINT_INTERVAL = 10`. Phase 1 raises `PRINT_INTERVAL` (e.g. to ~240 = daily) **only
when sediments are enabled**, cutting sediment output ~24× to a manageable size; daily
resolution is sufficient for all downstream analysis (which already subsamples to daily). The
sediments-off path keeps `PRINT_INTERVAL = 10` so the baseline stays byte-identical.
(Latent, non-blocking: `SEDIMENT_FLUX_OUTPUTS.out` writes the last box's fluxes for every box
— `mod_SIMULATE.f90:718-719`; noted for Phase 2, not fixed here.)

## 7. Validation & testing

- **Python unit test** (`tests/python/`), no ESTAS run: with `CL29_ENABLE_SEDIMENTS = True`,
  assert `INPUT_CL29.txt` has the `== 2` layout (`MODEL_SEDIMENTS 2`, the sediment input-file
  line, and **no** `NUM_PRESCRIBED_SEDIMENT_FLUX_SETS` line); `W_SED_CONST.txt` and
  `BOTTOM_SEDIMENT_MODEL_INPUT.txt` exist in the output dir; the sediment file's advanced-redox
  flag = 0. With the toggle `False`, assert byte-identical baseline (diff vs pre-change
  snapshot). (Uses the same testability refactor pattern the alternative spec described:
  parameterize the writers on output dir + toggle.)
- **Clean-checkout:** fresh clone + `CL29_ENABLE_SEDIMENTS = True` → converter runs, `ESTAS_II`
  builds, model runs (as secured in the reproducibility work).
- **Stability run (primary Phase-1 gate):** full 5-yr with sediments on completes with
  "simulation finished", **no NaN, no CO2SYS non-convergence, no runaway negative-mass**
  (the negative-mass clamp warning count is bounded and not growing).
- **Sanity (not calibration):** the benthic PO4 (and NH4) return flux to the water column is
  finite and of a physically plausible sign/magnitude; water-column PO4 shows the sediment now
  acting as a source. Confirm `DISS_Si` and pelagic groups remain finite. No claim about diatom
  improvement.

## 8. Success criteria (Phase 1)

- CL29 runs 0→1826 d with `MODEL_SEDIMENTS = 2`, stable (no NaN / CO2SYS failure / runaway
  negative mass), "simulation finished".
- Sediments-off baseline byte-identical; pipeline clean-checkout reproducible.
- Benthic N+P fluxes are finite and order-of-magnitude plausible.
- Runtime and output size measured and documented (acceptable, or the cost quantified for a
  decision).

## 9. Risks & mitigations

| Risk | Mitigation |
|---|---|
| Sediment CO2SYS non-convergence (low carbonate IC) | Raise sediment `INORG_C/TOT_ALK` to realistic pore-water values (pelagic-fix analog); confirm units first |
| Negative-mass instability in sediment vars | Staged bring-up to localize; adjust offending IC/constant or timestep; clamp exists as backstop |
| Runtime blow-up (CO2SYS × 7 layers × 29 boxes × 438k steps) | Measure on the short run first; raise `PRINT_INTERVAL`; if prohibitive, quantify and decide before extending |
| Output volume (~8.9M lines) | `PRINT_INTERVAL` raised when sediments on |
| Uniform sediment profile (no per-box sand/mud properties) | Documented Phase-1 limitation; fluxes still vary via deposition; per-box = Phase 2 (Fortran) |
| Baseline regression | Opt-in/off-by-default + byte-identical snapshot test |

## 10. Out of scope / future (Phase 2+)

- Calibrating sediment ICs/constants to measured Curonian sandy/muddy N+P fluxes.
- Verifying spring diatom gap closure + the N:P and guardrail validation.
- Per-box sediment properties (Fortran change to `READ_BOTTOM_SEDIMENTS_MODEL_INPUTS`).
- Fixing the `SEDIMENT_FLUX_OUTPUTS.out` last-box write bug.

## 11. References

- `docs/CL29_Parameter_Validation.md` — P-supply root cause + confirmation.
- `docs/superpowers/specs/2026-07-07-cl29-benthic-p-recycling-design.md` — superseded
  prescribed-flux alternative (explored).
- ESTAS source: `mod_AQUATIC_MODEL.f90:459-534` (parse + redox check),
  `mod_BOTTOM_SEDIMENTS.f90:317-486` (sediment input reader, single-profile broadcast),
  `aquabc_II_pelagic_auxillary.f90:1136-1344` (deposition assembly),
  `aquabc_II_sediment_model_1_fast.f90` (diagenesis + CO2SYS),
  `mod_SIMULATE.f90:716-744` (sediment output).
