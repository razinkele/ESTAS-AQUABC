# Pelagic-Core `GLOBAL` De-globalization — Design / Scope

**Date:** 2026-08-01
**Status:** **COMPLETE — Tiers 1–2 merged, Tier 3 NO-GO (2026-08-01).** The de-globalization stopped at
`GLOBAL` **12 → 4** loose pelagic allocatables (Tier 1 PR #88 `1c2c9ef`, Tier 2 PR #89 `7c5d85e`, both
byte-identical). Tier 3 was deliberately not taken up — see "Tier-3 decision gate" for the no-go rationale.
Hardened by a 4-way adversarial in-loop review + a 5-finder Workflow review. File:line references are under
`SOURCE_CODE/` (a byte-identical twin tree exists at `ali_version/` — use the `SOURCE_CODE/` copy).

## Goal

Bundle the **12 loose pelagic water-column allocatables** in `SOURCE_CODE/ESTAS/mod_GLOBAL.f90` into a single
derived type `pelagic_core_t` with one module instance `pcore` (defined **inside `mod_GLOBAL`**), dropping
`GLOBAL`'s **loose-allocatable count 12 → 0**. Done in **3 risk-tiered sub-slices**, each a byte-identical
`X` → `pcore%X` rename.

**Honest framing (review I-2):** because `pcore` stays in `mod_GLOBAL`, this is **namespacing/bundling, not a
coupling reduction** — every consumer still `use GLOBAL` and now writes `pcore%X`. The concrete benefit is the
allocatable-count metric + uniformity with the 3 prior slices + making a *future* real lift (move `pcore` to
its own module) a near-one-line change. It is the safest mechanical choice for the most-entangled arrays under
a byte-identical constraint. **Tier 3 (~1,000 candidate `pH`/`PROCESS_RATES`/`MODEL_CONSTANTS` sites) is an
explicit go/no-go after Tier 1**, not a formality — see "Tier-3 decision gate".

## Current state (verified against the tree)

The 12 allocatables (`mod_GLOBAL.f90:96-130`). **The count column is a case-sensitive word-match UPPER BOUND
(the review universe), NOT the rename set** — it includes dummy args, derived-type components, local shadows,
and comments (exactly what the method skips). The true rename count is far smaller (Tier 1 ≈ **17–20 renames**,
not 92); it is established by the scope-classification pass, not by grep.

| Array | candidate word-matches (upper bound, 19 files) | true renames (est.) | notes |
|---|---|---|---|
| `PROCESS_RATES` | 646 | — | Tier 3 |
| `MODEL_CONSTANTS` | 356 | — | Tier 3; ⚠️ rank-1 dummy in `GENERATE_PELAGIC_DERIVED_VARS` = **rank-match over-rename risk (silent)** |
| `STATE_VARIABLES` | 234 | — | Tier 3 |
| `DERIVATIVES` | 165 | — | Tier 2 |
| `DRIVING_FUNCTIONS` | 92 | — | Tier 2 |
| `SAVED_OUTPUTS` | 38 | ~6–7 | ⚠️ rank-2 dummy in `mod_BOTTOM_SEDIMENTS` (**B1**) + `PELAGIC_BOX` component + rank-1 dummy |
| `FLAGS` | 88 | — | Tier 2; prior shadow bug (advredox) |
| `CHLA` | 54* | ~7 | ⚠️ **four** distinct entities (see Shadow inventory); hardest Tier-1 array |
| `pH` | 25 (82 case-insensitive) | — | Tier 3; ⚠️ `PH`/`pH` case split — grep MUST be case-insensitive |
| `node_active` | 21 | ~3 | 18 are dummies in the only-import `aquabc_II_pelagic_model.f90` |
| `SURFACE_BOXES` | 6 | ~3 | |
| `WATER_COLUMN_OUTPUT` | 1 | 0 | **DEAD** — only the `mod_GLOBAL.f90:128` declaration; never allocated/used |

\* the original draft listed `CHLA` as 27 by silently pre-subtracting one file; the raw in-19 word count is 54.
Row-to-row the column mixed conventions — do not treat any of these as rename counts.

### Touch set (partitioned by import style)

- **21 files `use GLOBAL`; 2 are compiler-generated & gitignored** (`SOURCE_CODE/build/*__genmod.f90`,
  regenerated each build) → the real touch set is **19 files**.
- Of the 19: **4 use `use GLOBAL, only:`** importing only count-scalars — `aquabc_II_pelagic_model.f90:89`,
  `aquabc_II_pelagic_auxillary.f90:920/1155`, `aquabc_II_sediment_model_1_fast.f90:81`,
  `aquabc_II_sediment_auxillary.f90:364/445`. They import **zero** Tier-1 arrays and **not** `pcore`, so every
  Tier-1 token in them is provably a dummy/local → **all-skip, and any accidental `pcore%X` fails to compile**
  (`pcore` not in scope). These are the SAFE zone.
- The other **15 use `GLOBAL` unrestricted** — the only zone where a silent over-rename can compile. No file
  mixes both forms.
- **Transitive re-export is real but empty:** unrestricted `use GLOBAL` re-exports the arrays (and future
  `pcore`) to importers (e.g. output writers `use PELAGIC_BOX_MODEL`), but every outside-19 referrer resolves
  to a component/dummy/local (e.g. `sub_WRITE_PELAGIC_OUTPUT.f90:222` reads `…%PELAGIC_BOXES(j)%SAVED_OUTPUTS`,
  not the GLOBAL array). And after the declaration moves, a hidden re-export user of the bare name is
  **compiler-caught**, not silent. The 19-file touch set is complete.

### Alloc / dealloc (asymmetric — do NOT assume pairing)

- **Alloc** `mod_AQUATIC_MODEL.f90:209-219` — 11 arrays (all except the dead `WATER_COLUMN_OUTPUT`), one per line.
- **Dealloc** `ESTAS_II.f90:74-83` — 10 arrays. **`SURFACE_BOXES` is allocated but never deallocated;
  `WATER_COLUMN_OUTPUT` is neither.** Preserve this asymmetry exactly — do not add a `deallocate(pcore%SURFACE_BOXES)`.

## Design

Define `type :: pelagic_core_t` + `type(pelagic_core_t) :: pcore` inside `mod_GLOBAL` (verified cycle-safe:
`mod_GLOBAL` adds zero new `use`, so `make_lib.sh`'s ordering is unchanged; `pcore`/`pelagic_core_t` names are
free tree-wide; `mod_GLOBAL` currently defines no derived type — this is the first, standard Fortran). Members
are added tier by tier; `pcore` is a module var (implicit SAVE — same lifetime as the loose arrays; no `FINAL`
procedures exist → no finalization surprise).

### The 3 risk tiers

| Tier | Arrays | est. renames | risk |
|---|---|---|---|
| **1 (this slice)** | `WATER_COLUMN_OUTPUT` (move, dead-but-preserved), `SURFACE_BOXES`, `node_active`, `CHLA`, `SAVED_OUTPUTS` | ~17–20 | **low** — shadows are rank/type-mismatched → mis-renames compile-fail, EXCEPT the B1 sediment site (below) |
| **2** | `FLAGS`, `DRIVING_FUNCTIONS`, `DERIVATIVES` | — | med (FLAGS shadow history) |
| **3** | `pH`, `STATE_VARIABLES`, `MODEL_CONSTANTS`, `PROCESS_RATES` | — | **high** — `pH` case-split + rank-match dummies (`MODEL_CONSTANTS`) that over-rename **silently** |

### Why Tier 1 is genuinely low-risk (the compiler backstop — review's key insight)

With `implicit none` in every rename-site scope (`PELAGIC_KINETICS` `mod_PELAGIC_ECOLOGY.f90:1362`, `CALC_DERIV`
`mod_SOLVER.f90:740`, `CALCULATE_SETTLING_SUPRESSION`, `mod_AQUATIC_MODEL:25/48`), once `X` leaves `GLOBAL`:
- an **under-rename** (a GLOBAL-array site left bare) → the name no longer resolves → **compile error** (unless a
  same-name shadow exists in that exact scope; none do for the Tier-1 GLOBAL-array scopes).
- an **over-rename** (a shadow wrongly prefixed) → caught **only on shape mismatch**. Tier-1 shadows are shape-
  mismatched (`CHLA` local is a **scalar** vs rank-1 array; `SAVED_OUTPUTS` `PELAGIC_BOX` component is rank-1
  pointer vs rank-2 array; rank-1 dummies indexed one-dim vs rank-2) → **compile-fail**. The one exception is B1.

### B1 — the single silent Tier-1 site (BLOCKING mitigation)

`mod_BOTTOM_SEDIMENTS.f90:335` (signature) and `:343` (`real …, dimension(nkn_loc,n_saved_outputs) :: SAVED_OUTPUTS`)
are a **rank-2 dummy** of `SEDIMENT_TRANSPORT`, in a file that `use GLOBAL` **unrestricted** (so `pcore` is
visible) and whose shape **matches** the GLOBAL array → a mis-rename here would **compile silently**, and the
routine only runs under `MODEL_SEDIMENTS=2`. Mitigation (mandatory for Tier 1):
1. Enumerate `mod_BOTTOM_SEDIMENTS.f90:335` and `:343` as **explicit SKIPs** in the site list.
2. **Add a `MODEL_SEDIMENTS=2` / `INPUT_sediment_test.txt` run to the Tier-1 byte gate** (Standard + CL29 do
   NOT exercise it: Standard is `MODEL_SEDIMENTS=0`, CL29 is `=1`).
3. This retracts the earlier blanket "no flag-forked gate needed" claim — false wherever a Tier array shadows a
   shape-matching dummy inside a flag-gated routine (also `MODEL_CONSTANTS` in Tier 3).

### Shadow inventory (per-array, expanded)

- **`CHLA` — four entities:** `GLOBAL::CHLA` (rename target); `AQUABC_PELAGIC_INTERNAL::CHLA`
  (`aquabc_II_pelagic_internal.f90:222`, `target` array — the `ENV_CHUNK%CHLA => CHLA(ns:ne)` bind at
  `aquabc_II_pelagic_model.f90:1044` is this one, NOT GLOBAL); scalar local `mod_PELAGIC_ECOLOGY.f90:318`
  (in `GENERATE_PELAGIC_DERIVED_VARS`); pointer component `aquabc_II_pelagic_types.f90:170` (`env%CHLA`).
  → **CHLA gets its own dedicated review pass** (hardest Tier-1 array). GLOBAL-array renames live in
  `CALCULATE_SETTLING_SUPRESSION` (`mod_PELAGIC_ECOLOGY:1320/1328/1347`) + alloc/dealloc; the `mod_SOLVER:22/391`
  hits are **comments**.
- **`SAVED_OUTPUTS`:** GLOBAL renames ≈ `mod_AQUATIC_MODEL:216`, `ESTAS_II:81`, `mod_SOLVER:866/1409-LHS/1690-RHS`,
  `mod_PELAGIC_ECOLOGY:1480`. Skips: `PELAGIC_BOX` component decl `mod_PELAGIC_BOX.f90:79` (+ its alloc/dealloc),
  dummies `mod_PELAGIC_ECOLOGY:271/280`, **B1** `mod_BOTTOM_SEDIMENTS:335/343`, component `mod_SIMULATE:545`.
- **`node_active` / `SURFACE_BOXES`:** ~3 GLOBAL renames each (alloc + the `PELAGIC_KINETICS` call arg + dealloc);
  the bulk are dummies in the only-import `aquabc_II_pelagic_model.f90`.

## Method per tier (hardened)

1. **Determinism + baseline pre-check.** Confirm the gate configs are deterministic run-to-run. **Capture the
   pre-change compiler warning set** (`-Wunused-variable -Wunused-dummy-argument`) as a baseline (for step 7).
2. **Enumerate** candidate sites: `grep -niwE` (**case-insensitive** — mandatory; `pH`↔`PH`) each array within
   the **15 unrestricted `use GLOBAL` files** (the 4 only-import files are all-skip). Read **full logical
   statements including `&` continuation lines** — a `%`-component split across a continuation looks bare on the
   grepped line (`mod_SOLVER:1689-1690`).
3. **Scope-aware, per-OCCURRENCE classification** (the load-bearing gate — not a script): for each occurrence
   decide GLOBAL-array (rename) vs shadow (skip). Some single statements need opposite decisions per token
   (`mod_SOLVER:1409-1410`, `:1689-1690` — component-skip on one side, GLOBAL-rename on the other). Record a
   `file:line:col rename|skip` list. `CHLA` and (Tier 3) `pH` get a dedicated pass.
4. **Apply** renames + move the declaration into `pelagic_core_t` + retarget alloc (`mod_AQUATIC_MODEL:209-219`)
   and dealloc (`ESTAS_II:74-83`), **preserving the alloc/dealloc asymmetry** (SURFACE_BOXES alloc-only,
   WATER_COLUMN_OUTPUT neither). If a rename-site file uses `use GLOBAL, only:`, add `pcore` to its only-list
   (build-caught if missed).
5. **`!$omp` pre-check + build.** Verify no moved array appears in any `!$omp` data-sharing clause
   (`private/firstprivate/shared/default(none)`) — none do for Tier 1 (all regions are `default(shared)`; a
   privatized derived-type component would be a compile error anyway). Build gfortran release **and OpenMP**.
6. **Compiler-backstop cross-check.** The build itself is the under-rename net (bare `X` gone → error). As a
   discovery aid, a scratch "delete `X` from GLOBAL, force-compile every file, collect `no IMPLICIT type`
   errors" pass enumerates under-rename sites (caveat: under-renames only, never over-renames; `make_lib.sh`
   stops at the first failing file so force per-file).
7. **Baseline-diffed unused-warning pass** (the over-rename net the type-checker misses on shape-match): rebuild
   with `-Wunused-*`, diff vs the step-1 baseline. A fully over-renamed shadow becomes a **newly-unused**
   local/dummy — investigate every new unused warning.
8. **Byte-identity gate** over **all** output files in `OUTPUTS/`, `OUTPUTS_CL29/`, **and the `MODEL_SEDIMENTS=2`
   run** — identical before/after:
   - Standard: `./ESTAS_II INPUT.txt` (`MODEL_SEDIMENTS=0`, redox=0).
   - CL29: `ESTAS_HOLD_VOLUME=1 ./ESTAS_II INPUT_CL29.txt` (`=1`, redox=1 — **sole config observing
     `SAVED_OUTPUTS` via the Fe/Mn feedback**; the flag is mandatory or CL29 crashes ~day 449).
   - Sediment: the `MODEL_SEDIMENTS=2` / `INPUT_sediment_test.txt` gate (covers B1's routine).
9. **Strip-and-compare (mandatory):** strip `pcore%` back to bare and confirm the diff is a pure prefix-add —
   this proves **no stray edits**, NOT that the right sites were chosen (a wrongly-prefixed shadow strips back
   too). Correctness rests on steps 3+6+7+8, not this.
10. **CI matrix** green (gfortran macOS/ubuntu, ifx, integration-tests, python-lint) → PR → merge.

## Byte-identity / regression constraints

Every existing run stays byte-identical (pure rename). Gate = Standard + CL29(`HOLD_VOLUME=1`) +
`MODEL_SEDIMENTS=2`, diffing all output files, plus the OpenMP build. `INPUTS/FLOW_TS.txt` stays out of every
commit (explicit pathspec).

## Risks & mitigations

1. **Silent same-rank/same-type over-rename** (the only class the compiler misses). Tier 1: confined to **B1**
   (`mod_BOTTOM_SEDIMENTS:343`) → explicit skip + `MODEL_SEDIMENTS=2` gate. Tiers 2/3: `MODEL_CONSTANTS`/`FLAGS`
   rank-match dummies in flag-gated routines → the unused-warning pass (step 7) + per-tier flag gates are
   load-bearing there; do not carry the "core arrays exercised by every run" assumption forward.
2. **`WATER_COLUMN_OUTPUT` preserved, not dropped** (per direction): verified dead (0 refs); moved into `pcore`
   unallocated — a true no-op (allocatable components default deallocated; no auto-alloc/finalization). Still
   counts toward 12→0.
3. **Alloc/dealloc asymmetry** (see above) — do not add a spurious dealloc.
4. **Enumeration landmines:** case-sensitivity + continuation lines + per-occurrence mixed statements — folded
   into method steps 2–3.

## Tier-3 decision gate — DECIDED: NO-GO (2026-08-01)

**Decision: do NOT proceed with Tier 3. Stop the de-globalization at `GLOBAL` 12 → 4.**

Rationale (after Tiers 1–2 shipped byte-identical): moving the remaining core arrays (`pH`,
`STATE_VARIABLES`, `MODEL_CONSTANTS`, `PROCESS_RATES`) buys only a cosmetic allocatable-count metric (4 → 0)
— `pcore` stays inside `mod_GLOBAL`, so there is **zero coupling change** and the program is functionally
identical either way. Against that near-zero benefit: it is the **highest-risk** tier (the `pH` `PH`/`pH`
case-split; the `MODEL_CONSTANTS` same-rank dummy in `GENERATE_PELAGIC_DERIVED_VARS` — the one over-rename
class the compiler cannot reject on shape) and the **largest effort** (~1,000 candidate sites, ~10× Tiers
1+2). A future *real* de-globalization (lifting `pcore` into its own module — the actual coupling win) would
fold in the 4 remaining arrays as part of its own work, so doing Tier 3 now neither de-risks nor accelerates
it.

**If completeness is ever wanted:** do it as per-array sub-slices (`pH` first, isolated), with **manual**
per-occurrence classification (no rename script — Tier 2 showed scripts trip on continuation-`%`), plus a
wider gate (a config exercising the advanced-redox + allelopathy paths so the same-rank-dummy routines are
covered). Not planned.

## Tier 1 — this slice

Move `SURFACE_BOXES`, `node_active`, `CHLA`, `SAVED_OUTPUTS` into `pelagic_core_t`/`pcore` (~17–20 renames) and
move the dead `WATER_COLUMN_OUTPUT` declaration in as well (preserved, unallocated). Explicit skips:
`mod_BOTTOM_SEDIMENTS:335/343` (B1), the `CHLA` scalar/pointer/target entities, the `SAVED_OUTPUTS`
`PELAGIC_BOX` component + dummies. Deliverable: one byte-identical PR passing the full gate (Standard + CL29 +
`MODEL_SEDIMENTS=2`, all outputs, OpenMP).

## Out of scope

- Any semantic/behaviour change (structural only).
- Concern-based types (decided: one `pcore` bundle).
- Truly removing global state (deferred — `pcore` stays module-level; this is bundling, per Goal).
- Tiers 2 and 3 land as their own subsequent PRs; Tier 3 only after its decision gate.
