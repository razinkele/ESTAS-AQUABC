# Fix Model-Constants Out-of-Bounds Write (TODO 1.10) — Design

**Backlog item:** 1.10 [P1] Model-constants array out-of-bounds
**Date:** 2026-07-16
**Status:** Design approved (reframed after adversarial plan review) — ready for implementation

## Problem

`WCONST_04.txt` contains **323** model constants (indices 1–323, contiguous). Indices
319–323 are the `BETA_*` photoinhibition parameters. But the code's hardcoded constant
count was never bumped from 318:

- `SOURCE_CODE/ESTAS/mod_GLOBAL.f90:20` — `integer, parameter :: nconst = 318`
- `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90:75` — `nconst = 318`
- `INPUTS/PELAGIC_INPUTS.txt:9` — `NUM_MODEL_CONSTANTS = 318`

`READ_MODEL_CONSTANTS` (`mod_UTILS_01.f90`) does `MODEL_CONSTANTS(CONSTANT_NO) = value`
for every line in the file, so constants 319–323 are written **out of bounds** of the
318-element `MODEL_CONSTANTS` array — a real out-of-bounds *write* (undefined behavior),
confirmed by `-fcheck=all`: *"Index 319 … above upper bound of 318"*.

## What the bug is — and is NOT (corrected by adversarial review + verification)

The initial framing ("garbage `BETA_*` distorts production light-limitation → changes
output → needs scientific sign-off") was **wrong**, and the adversarial plan review
caught it. Verified two ways:

- **Code:** the ESTAS/production path's constant-unpacking routine
  `INIT_PELAGIC_MODEL_CONSTANTS` (in `mod_PELAGIC_ECOLOGY.f90`, called via
  `mod_AQUATIC_MODEL.f90:223` → `INITIALIZE_PELAGIC_BOX_MODEL` → `use PELAGIC_ECOLOGY`)
  assigns `MODEL_CONSTANTS(1..318)` and **stops** — it has **no `BETA_*` assignments**.
  (The routine that *does* wire `BETA_DIA = MODEL_CONSTANTS(319)` is a separate procedure
  in `aquabc_II_pelagic_model_constants.f90` serving the 0D interface path.) So on
  production, `BETA_*` are **never read from the OOB slots** — they hold their static
  zero-init value `0.0`, both before and after the fix.
- **Empirical:** applying `nconst=323` leaves the default 25-box run **byte-for-byte
  identical** (0 / 52 output files differ, serial).

So this is a **pure memory-safety fix**: it removes an out-of-bounds write. It does
**NOT** change production model output, and does **NOT** change `BETA` photoinhibition
(already `0.0`). No scientific sign-off is required.

Why fix it anyway: an OOB write is undefined behavior — benign only by luck of the
current heap layout; it blocks `-fcheck`/sanitizer debug builds; and it is a latent
memory-corruption / non-determinism source (related to the still-open TODO 1.11
advanced-redox non-determinism, which involves the same class of memory issue).

## Non-goals

- **No output change.** The fix must keep the standard (advanced-redox-off) production
  path byte-identical — that is the acceptance criterion, not a magnitude to sign off.
- **`WCONST_04.txt` is NOT modified** — the data file is already correct (323 constants).
- **Not wiring `BETA` into the ESTAS path.** A *separate* observation surfaced by the
  review: the `BETA_*` photoinhibition feature is not wired into the ESTAS constant-
  unpacking at all (only into the 0D path). This is harmless today (`BETA=0` is the
  intended default, so production correctly runs with photoinhibition off) and is out of
  scope for 1.10 — noted as a potential future item, not fixed here.
- **Not TODO 1.11** (advanced-redox uninitialised-memory non-determinism).

## The fix

Make the code's constant count agree with the data (323) so the array is sized
correctly and no write goes out of bounds:

| File | Change |
|---|---|
| `SOURCE_CODE/ESTAS/mod_GLOBAL.f90:20` | `nconst = 318` → `323` |
| `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90:75` | `nconst = 318` → `323` |
| `INPUTS/PELAGIC_INPUTS.txt:9` | `318` → `323` |
| `INPUTS/PELAGIC_INPUTS_verify.txt:9`, `INPUTS/PELAGIC_INPUTS_verify_ar.txt:9` | `318` → `323` (keep the 1.6 harness runnable — the model error-stops if `NUM_MODEL_CONSTANTS ≠ nconst`) |
| `tools/eutropy_poc/eutropy_to_estas.py:595` | `_hdr("NUM_MODEL_CONSTANTS", 318)` → `323` (the generator that emits `INPUTS_CL29/PELAGIC_INPUTS.txt`; without this, a rerun silently reverts CL29 to 318 and reintroduces the mismatch) |
| `INPUTS_CL29/PELAGIC_INPUTS.txt:9` | `318` → `323` (applied locally; the file is git-ignored) |

That is the entire change. No model logic changes.

## Validation (acceptance criteria — no sign-off, just proof of no-change)

1. **Byte-identical output** — the fix must leave the standard production path
   unchanged. Verified with the existing 1.6 default-only gate `tools/refactor_verify.sh`
   against the pre-fix `verify_baseline/default_serial` (captured at `nconst=318`):
   default config, serial + OMP=8, must be BIT-IDENTICAL (52 files) + 0D golden PASS.
   **Do NOT re-capture the baseline** — the baseline must remain the pre-fix (318) output
   so the comparison actually tests "the fix changed nothing."
2. **OOB gone** — a `-fcheck=all` (debug) build no longer reports "Index 319 … above
   upper bound of 318".
3. **0D golden** — unaffected (the 0D driver's `data/const_CL.txt` has 318 constants and
   the interface zero-inits the array, so `BETA_*` stay `0.0`); confirm PASS.
4. **Full-year stability** — the corrected model runs clean over the full default year
   (`SIMULATION_END 6574.0`) and is deterministic run-to-run.

## Landing

Feature branch `fix/model-constants-oob`. One commit for the fix (message: "eliminate
out-of-bounds write; production output unchanged"). Push → CI (`build-and-run`,
`integration-tests`, ftnchek, 0D E2E). On green, merge to `main` (no scientific sign-off
gate — output is byte-identical). Backlog §1.10 marked complete on merge.

## Files

- **Modify:** `mod_GLOBAL.f90`, `aquabc_II_pelagic_interface.f90`, `INPUTS/PELAGIC_INPUTS.txt`,
  `INPUTS/PELAGIC_INPUTS_verify.txt`, `INPUTS/PELAGIC_INPUTS_verify_ar.txt`,
  `tools/eutropy_poc/eutropy_to_estas.py`, `TODO_IMPLEMENTATION_PLAN.md` (§1.10 done)
- **Modify locally (git-ignored):** `INPUTS_CL29/PELAGIC_INPUTS.txt`
- **Unchanged:** `WCONST_04.txt`; all model logic; the 0D golden

## Risks

| Risk | Mitigation |
|---|---|
| The fix silently changes output somewhere | The byte-identical gate (serial + OMP=8 vs the pre-fix 318 baseline) is the acceptance criterion; a diff fails it. |
| A config file left at 318 → model error-stops (`NUM_MODEL_CONSTANTS ≠ nconst`) | Bump every `PELAGIC_INPUTS*` in play + the generator; grep-verify the **named** files (not a broad glob — orphaned legacy configs `PELAGIC_INPUTS_{per_square,before_dissolved_fractions_fixing,WCONST_02,zero_nost_boundary}.txt` sit at an even older 307 and are intentionally left alone). |
| CL29 regenerated later reverts to 318 | Bump the generator (`eutropy_to_estas.py:595`), not just the emitted file. |
