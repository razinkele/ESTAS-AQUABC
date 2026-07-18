# TODO Implementation Plan — AQUABC v0.3

**Created:** 2026-02-13
**Based on:** Deep audit of Fortran code, Python/Shiny app, and CI/build/test infrastructure

---

## Priority Legend

| Priority | Meaning |
|----------|---------|
| P0 | Critical — correctness/safety bug, fix ASAP |
| P1 | High — significant quality/maintainability issue |
| P2 | Medium — improvement that reduces tech debt |
| P3 | Low — nice-to-have, do when convenient |

---

## 1. Fortran Code Quality

### 1.1 [P0] Memory Leaks in ALLELOPATHY Module

> **Status:** ✅ COMPLETED (Sprint 1, 2026-02-14) — No fix needed — no leak (`mod_ALLELOPATHY.f90` alloc/dealloc correct)

**File:** `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_ALLELOPATHY.f90`

**Problem:** 44 local allocatable arrays are allocated at subroutine entry but never deallocated. Every call leaks memory. Over a long simulation with thousands of timesteps × spatial nodes, this accumulates into significant memory bloat.

**Fix:**
- Add `deallocate(...)` for all 44 arrays before each `return` and at subroutine end
- Alternatively, convert to automatic (stack) arrays since `nkn` is known at entry: `real(dp) :: ALLEL_C(nkn)` instead of `allocatable`

**Effort:** ~1 hour

---

### 1.2 [P0] Unguarded K_E Division — EUPHOTIC_DEPTH

> **Status:** ✅ COMPLETED (Sprint 1, 2026-02-14) — Fixed — `K_E` guarded in CYANOBACTERIA / FIX_CYANOBACTERIA / NOSTOCALES

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Problem:** `EUPHOTIC_DEPTH = 4.61D0 / K_E` — if K_E is zero (e.g., no particles, no background extinction), this produces Inf/NaN that propagates through light limitation.

**Fix:**
```fortran
EUPHOTIC_DEPTH(ns:ne) = 4.61D0 / max(K_E(ns:ne), 1.0D-20)
```

**Effort:** 5 minutes

---

### 1.3 [P0] SAVE Variables — Thread Safety Risk

> **Status:** ✅ COMPLETED (Sprint 1, 2026-02-14) — Documented — SAVE vars audited; no active race under current OpenMP usage

**Files:**
- `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90` — 6 arrays with `SAVE` attribute (allocated on first call)
- `SOURCE_CODE/UTILS/string_utils.f90` — `SAVE` variable in string formatting

**Problem:** `SAVE` variables persist across calls and are shared across threads. If the pelagic interface is ever called from within the OpenMP region (currently it is not, but future changes could introduce this), data races would occur.

**Fix:**
- For pelagic_interface: Move SAVE arrays to module scope with explicit initialization, or allocate once at simulation start
- For string_utils: Make the SAVE buffer `threadprivate` or use a local buffer
- Add comments documenting thread-safety assumptions

**Effort:** ~2 hours

---

### 1.4 [P1] CO2SYS Exponential Overflow Risks

> **Status:** ✅ COMPLETED (Sprint 2, 2026-02-14) — Fixed — 8 `exp()` calls wrapped with `safe_exp` in `aquabc_II_co2sys.f90`

**File:** `SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90`

**Problem:** Several raw `exp()` calls with unbounded arguments (e.g., `exp(-pH * log(10))` for extreme pH values). While pH is now clamped at model entry, CO2SYS has its own internal calculations that could produce extreme arguments.

**Fix:**
- Add `safe_exp()` calls (already defined in `aquabc_II_pelagic_model_constants.f90`) to the ~5 vulnerable `exp()` calls in CO2SYS
- Or clamp arguments: `exp(max(min(arg, 700.0D0), -700.0D0))`

**Effort:** ~30 minutes

---

### 1.5 [P1] Remaining Division-by-Zero Risks in Pelagic Model --- COMPLETED 2026-02-14

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Audit complete.** Systematic review of all ~80 division operations in `AQUABC_PELAGIC_KINETICS`. One missing guard was found and fixed:

**Fix applied:** Added `elsewhere(FE_III .lt. 1.0D-20)` guard to the Fe3+ first-timestep initialization (case 1), matching the existing Fe2+ pattern. All other divisions confirmed safe:

| Category | Count | Protection |
|----------|-------|------------|
| Constant divisors (molar masses, unit conversions) | ~20 | `S_MOLAR_MASS_MG`, `FE_MOLAR_MASS_MG`, `12000.0D0`, `14007.0D0`, `30974.0D0`, `14.D0`, `31.D0`, `28.0855D0`, `30.9737D0`, `1.0D4`, `1.0D6` |
| C-to-CHLA ratios | 5 | `DIA_C_TO_CHLA`, `CYN_C_TO_CHLA`, etc. (model constants, always > 0) |
| Monod/half-saturation kinetics | ~30 | Form `X / (X + K_HS)` where K_HS > 0, so denominator > 0 |
| Fe2+ fractions (first timestep + subsequent) | 2 | `where(FE_II .lt. 1.0D-20)` guard |
| Fe3+ fractions (subsequent timestep) | 1 | `where(FE_III .lt. 1.0D-20)` guard |
| Fe3+ fractions (first timestep) | 1 | **Fixed:** added `elsewhere(FE_III .lt. 1.0D-20)` guard |
| Mn2+ fractions | 1 | `where(MN_II .lt. 1.0D-20)` guard |
| Saved outputs (FE_II, FE_III) | 2 | `where(FE_II/FE_III .lt. 1.0D-20)` guard |
| H2S speciation | 2 | `H2S_DIVISOR = H+ ^2 + H+*K1 + K1*K2` (sum of positive terms, always > 0 for valid pH) |
| Phosphate speciation | 3 | `FRACTION_DIVISOR_TIP = H+^3 + K1*H+^2 + K1*K2*H+ + K1*K2*K3` (sum of positive terms) |
| Temperature-dependent constants | ~6 | `T_A = TEMP + 273.15` (always > 0 for liquid water) |
| Zoo/detritus N:C and P:C | 4 | `max(divisor, MIN_CONCENTRATION)` guard |
| NH4 preference fractions | 2 | `max(denominator, 1.0D-10)` guard |
| N:P molar ratio | 1 | `where(PO4_P .lt. 1.0D-10)` conditional guard |
| FRAC_NH3 | 1 | `1.0 / (1.0 + 10^(...))` — denominator always >= 1.0 |
| FRAC_FIX_N_FOR_GR_VEG_HET | 2 | Model constant (default 0.65, user-provided, must be > 0) |
| Rate limiters (allowed_rate/total_removal) | 1 | Guarded by `if (total_removal > allowed_rate)` |
| OpenMP chunk_size | 1 | Integer division `(nkn + nthreads - 1) / nthreads`, always > 0 |

**Effort:** ~1 hour (as estimated)

---

### 1.6 [P2] Mega-Subroutine Decomposition

**Status:** ✅ COMPLETE 2026-07-16. `AQUABC_PELAGIC_KINETICS` decomposed into a thin
orchestrator + **five internal `contains` procedures** via pure code motion:
`pelagic_co2sys_preprocess`, `pelagic_speciation_preprocess`, `pelagic_biology`,
`pelagic_chemistry`, `pelagic_derivatives`. Shared `(nkn)` arrays reached by host
association; per-thread private data (bundles/scalars) passed as arguments (the
OpenMP correctness rule); the `if (nkn_local > 0)` guard + the two straddling `if`
constructs kept whole in the orchestrator. **Byte-for-byte identical output** —
gated after every extraction by `tools/refactor_verify.sh` (default all-box config,
serial + OMP=8 bit-identical over 52 files + 0D golden). Adversarially reviewed
plan (Workflow, 11 findings fixed) → subagent-driven execution (5 extraction tasks,
each independently reviewed byte-identical). Spec/plan:
`docs/superpowers/specs|plans/2026-07-15-pelagic-kinetics-decomposition*`.

**Note:** the advanced-redox verify config surfaced two pre-existing model bugs
(TODO 1.10 constants OOB, 1.11 advanced-redox uninitialised memory) — filed
separately; the decomposition proceeded on the deterministic default-only gate.

**Effort:** ~1–2 days (as estimated)

---

### 1.7 [P2] Sediment Model Variable Declarations

**Status:** ✅ COMPLETE 2026-07-18 — scoped after measuring the *actual* dead-code state.

**File:** `SOURCE_CODE/AQUABC/SEDIMENTS/aquabc_II_sediment_model_1_fast.f90`
(one 3,613-line subroutine, 321 declared locals).

**Reality vs the original triage:** the "many may be unused" hypothesis was **outdated**.
`gfortran -Wunused-variable` (verified to fire by planting a probe var) reports
**zero** never-referenced locals — prior cleanup passes already removed those. The only
remaining dead category is **write-only** variables (assigned but never read), which
gfortran's `-Wunused-but-set-variable` does **not** reliably detect for Fortran. A
custom read/write usage analyzer surfaced 15 candidates; 3 were false positives
(output dummy args `FINAL_SED_STATE_VARS`, `SED_OUTPUTS`, `SED_BURRIAL_RATE_OUTPUTS` —
writing to them is their purpose), leaving **12 genuine write-only locals**, each
hand-verified (every occurrence is a comment, the declaration, or an LHS assignment —
never a read).

**Fix (scope: "remove stale, keep+annotate chemistry"):** the 12 split into two kinds:
- **Removed (3 stale leftovers, no coherent feature):** `CA`, `MG` (calcium/magnesium
  copied from `INIT_SED_STATE_VARS(20/21)`, "Introduced 27 January 2016", never used
  since) and `CONSIDER_CO2_REARATION` (integer flag = 1, self-labelled "not used yet").
  Declarations + dead assignments deleted.
- **Kept + annotated (9 disabled-chemistry scaffolding):** `H2S`, `HS_MINUS`,
  `S_MINUS_TWO` (sulfide speciation), `K_SP_FES`, `FE_II_DISS` (FeS-solubility equilib.,
  consumer commented out near L1140), `MULT_FE_II_PART`, `MULT_MN_II_PART`,
  `MULT_MN_IV_PART` (Fe/Mn particulate fractions), `ALPHA_PO4` (PO4 speciation α). These
  are coherent scaffolding for currently-disabled redox chemistry (the maintainer's
  active area), so they are retained with a `[WO]` tag on each declaration + a header
  NOTE block explaining the write-only status and why gfortran can't flag them.

**Deliberately NOT done:** (a) removing the 9 chemistry vars — they preserve intent for
disabled features (see the [WO] header); (b) removing the 2 unused *dummy arguments*
(`SED_MODEL_CONSTANTS`, `SED_DRIVING_FUNCTIONS` — the latter "not used yet") — those are
public-interface placeholders and removing them is a breaking API change; (c) derived-type
grouping — a large refactor disproportionate to a cleanup, deferred.

**Verified:** byte-identical against a **fresh** default-config baseline (serial + OMP=8,
52 `.out` files each) — removing write-only vars + their pure assignments changes no
output — plus the 0D golden regression. (The gate's `verify_baseline/` had to be
re-captured because 1.8 shifted default output ~1e-6.)

**Effort:** ~2–3 hours (within the ~2–4 h estimate).

---

### 1.8 [P3] Magic Numbers in Physics Constants

**Status:** ✅ COMPLETE 2026-07-18 — surfaced (and fixed) a latent precision bug.

**Reality vs the original triage:** the backlog's example literals `1013.25` and `8.314`
do **not** exist in the code; the only universal-physics magic number is `273.15`
(Kelvin offset), and a named constant `CELSIUS_TO_KELVIN = 273.15D0` already exists in
`AQUABC_PHYSICAL_CONSTANTS`. 273.15 appeared 5 live times: 2 as `273.15D0` (double) in
the pelagic/sediment `REDOX_AND_SPECIATION`, and **3 as bare `273.15` (single) in
double-precision temperature math** — `aquabc_II_pelagic_lib_DO_SATURATION` (`T_KELVIN`),
`aquabc_II_sediment_lib_DO_SATURATION`, and `aquabc_II_co2sys` (`TempK`). The single-form
literals were a **latent single-precision bug**: a REAL(4) `273.15` (≈273.14999) used in
REAL(8) temperature conversions.

**Fix:** all 5 replaced with `CELSIUS_TO_KELVIN` (added the symbol to the two REDOX
`only:` lists and CO2SYS's, and a `use ... only: CELSIUS_TO_KELVIN` to the two
DO_SATURATION subroutines). This removes the magic number **and** fixes the precision.

**Output impact (intended):** the two double-form swaps are byte-identical; the three
single→double swaps change O2-saturation / CO2SYS-dependent output by a **precision-level
~1e-6** (max rel diff 9.6e-7, toward the exact value). Not byte-identical, so the 0D
golden `tests/regression/pelagic_0D_golden.csv` was **regenerated** (526 sampled rows,
5592 cells nudged ≤1e-6, header/structure unchanged). The gate's local `verify_baseline`
is now stale (pre-change) and would need re-capture for future refactor checks.

**Verified:** 0D golden compare PASS at rtol 1e-9 vs a fresh run; `pytest
tests/python/test_e2e_regression.py` 8/8; the change magnitude brackets between rtol
1e-7 (fail) and 1e-6 (pass), confirming precision-only.

**Effort:** ~1 hour (as estimated).

---

### 1.9 [P3] Missing IOSTAT on File READ Operations

**Status:** ✅ COMPLETE 2026-07-18 — the crash-on-bad-input path is guarded.

**Problem:** A missing or unreadable input file aborted with an opaque Fortran
runtime error. All 71 `OPEN`s and ~430 `READ`s lacked `IOSTAT=`.

**Fix (scoped to the failure mode, not all 430 reads):** the dominant crash is the
`status='OLD'` **input OPENs** — a bad path there is what users actually hit. Added a
standalone helper `SOURCE_CODE/ESTAS/sub_OPEN_INPUT_FILE.f90`
(`OPEN_INPUT_FILE(unit, path, description)`) that opens with `IOSTAT`, prints a clear
actionable message (`ERROR: cannot open <description> file: <path>`, IOSTAT, hint), and
`error stop`s with a **nonzero exit** (so `run_cl29.sh`'s `set -e` catches it). Retrofitted
all **24 input `status='OLD'` opens** across ESTAS_II, sub_READ_PELAGIC_INPUTS,
mod_AQUATIC_MODEL, mod_BOTTOM_SEDIMENTS, mod_RESUSPENSION to call it. The build's
source glob auto-compiles the new file; it is a standalone external subroutine so no
module dependency / build-order risk.

**Deliberately NOT done:** per-`READ` `IOSTAT` on the ~430 reads (malformed *content*
mid-file). That is disproportionate (~400 lines of boilerplate, unreadable diff) and
the OPEN guard covers the common failure (missing/unreadable file). Left as a possible
future item if malformed-content robustness is ever needed.

**Verified:** `tools/refactor_verify.sh` GATE PASS (default serial + omp8 bit-identical,
0D golden PASS — behaviour unchanged when files exist); negative tests — missing
top-level INPUT and missing pelagic input both produce the clean message and exit code 1.

**Effort:** ~1–2 hours (as estimated).

---

### 1.10 [P1] Model-constants array out-of-bounds (NUM_MODEL_CONSTANTS mismatch)

**Files:** `SOURCE_CODE/ESTAS/mod_GLOBAL.f90:20` (`nconst = 318`),
`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90:75` (`nconst = 318`),
`INPUTS/PELAGIC_INPUTS.txt:9` + `INPUTS_CL29/PELAGIC_INPUTS.txt:9` (`NUM_MODEL_CONSTANTS = 318`),
data files `INPUTS/WCONST_04.txt` (323 constants).

**Status:** ✅ COMPLETE 2026-07-17 (found 2026-07-16 during TODO 1.6). A **pure
memory-safety fix** — production output is byte-identical.

**Problem:** `WCONST_04.txt` contains **323** model constants but `nconst` (and the
input `NUM_MODEL_CONSTANTS`) were declared **318**. `READ_MODEL_CONSTANTS`
(`mod_UTILS_01.f90`) does `MODEL_CONSTANTS(CONSTANT_NO) = value` for every file line,
so constants 319–323 (the `BETA_*` photoinhibition params) are written **out of
bounds** of the 318-element array — an OOB *write* (undefined behavior, flagged by
`-fcheck=all`: "Index 319 … above upper bound of 318").

**Corrected diagnosis (adversarial plan review + verification):** the initial
"garbage `BETA_*` distorts production output" framing was WRONG. The ESTAS/production
constant-unpacking (`mod_PELAGIC_ECOLOGY.f90` `INIT_PELAGIC_MODEL_CONSTANTS`) stops at
index 318 and **never reads `BETA_*` from 319–323** (only the 0D-path routine in
`aquabc_II_pelagic_model_constants.f90` does). So `BETA_*` were never consumed from
the OOB slots on production — they hold their static-zero `0.0`. Verified empirically:
the fix leaves the default run **byte-for-byte identical** (0/52 files).

**Fix:** `nconst 318→323` in `mod_GLOBAL.f90:20` + `aquabc_II_pelagic_interface.f90:75`,
and `NUM_MODEL_CONSTANTS 318→323` in the input configs **and the generator**
`tools/eutropy_poc/eutropy_to_estas.py:595` (else a regenerated CL29 config reverts).
`WCONST_04.txt` unchanged. Verified: byte-identical (default serial+omp8 gate + 0D
golden), `-fcheck` OOB gone, full-year run stable + deterministic. NO scientific
sign-off needed (no output change).

**Separate future observation (NOT fixed here):** `BETA_*` photoinhibition is not
wired into the ESTAS path at all — harmless today since `BETA=0` is the intended
default. Spec/plan: `docs/superpowers/*/2026-07-16-model-constants-oob-fix*`.

**Effort:** ~1 hour (as estimated).

---

### 1.11 [P1] Advanced-redox uninitialised-memory non-determinism

**Files:** `SOURCE_CODE/ESTAS/mod_SOLVER.f90:743` (a local `FLAGS` declaration inside
`CALC_DERIV` that shadowed the global `FLAGS` — **the non-determinism itself**), plus
two further defects fixed alongside (see below):
`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` (`FE_II_DISS` unassigned in
the saturated `where` branch) and
`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_debug_stranger.f90` (debug checker read
the wrong variable → latent spurious `stop`).

**Status:** ✅ COMPLETE 2026-07-17 (found 2026-07-16 during TODO 1.6). A **one-line
fix**; the default (advanced-redox-off) path is byte-identical, advanced-redox output
changes (it was previously garbage-dependent).

**Problem:** With `ADVANCED_REDOX_SIMULATION=1`, the model was **non-deterministic
run-to-run** (same binary, same inputs). The default path is deterministic.

**Corrected diagnosis (the original framing above was WRONG on every count).** It was
NOT "~1 ULP amplifying through the nonlinear integration", NOT the 246 `internal.f90`
working arrays, and NOT a stack local in the redox library. The real cause:

`CALC_DERIV` (`mod_SOLVER.f90`, 667–1596) declared a **local** `FLAGS` at line 743
(`integer, dimension(PELAGIC_BOX_MODEL_DATA % NUM_FLAGS) :: FLAGS`) which **shadowed
the global allocatable `FLAGS`** from `GLOBAL`. Both initialisations inside
`CALC_DERIV` — `FLAGS = 0` (:803) and `FLAGS = PELAGIC_BOXES(1) % FLAGS` (:1314) —
therefore filled a *discarded local copy*. The **global** `FLAGS` (allocated
`mod_AQUATIC_MODEL.f90:196`, and the array `PELAGIC_KINETICS` actually hands to
AQUABC at `mod_PELAGIC_ECOLOGY.f90:1465`) was **never assigned by anything**. AQUABC
then read straight out of that garbage (`aquabc_II_pelagic_model.f90:360-364`):
`FIRST_TIME_STEP = FLAGS(3)`, `INIT_OPTION_OF_FE_II_DISS = FLAGS(4)`,
`INIT_OPTION_OF_FE_III_DISS = FLAGS(5)`.

**Why every symptom fits:** `FIRST_TIME_STEP`/`INIT_OPTION_*` are consumed **only**
inside the advanced-redox block (`if (FIRST_TIME_STEP > 0)` at :650 and :765, and the
`select case (INIT_OPTION_OF_FE_II_DISS)` at :652 which has **no `default`**) — hence
advredox-only. The garbage drives a **binary branch**, so the model produced exactly
**two** reproducible output states (not diffuse float noise), ~30–40% split. It is
**not** a data race: it reproduces identically at `OMP_NUM_THREADS=1`.

**Why the old lead was a red herring:** the earlier "zero-init the 246 arrays →
divergence 100%→60%" result never touched `FLAGS`; it merely perturbed the heap
layout, shifting how often the garbage `FLAGS(3)` landed >0. A fix that moves a
*probability* rather than removing a symptom means the bug was perturbed, not fixed.
Also note `-finit-real=snan` provably does **not** initialise allocatables (only stack
locals), so the planned snan hunt could never have found this.

**Fix:** delete the shadowing local declaration at `mod_SOLVER.f90:743` so the existing
`FLAGS = 0` / `FLAGS = PELAGIC_BOXES(1) % FLAGS` assignments fill the global array, as
the code always intended.

**Verified:** advredox 30-day **40/40 runs byte-identical** (20 @ 25 threads + 20 @ 1
thread, same hash both arms; previously 2 states every time); advredox full-year 365-day
**5/5 byte-identical**; `tools/refactor_verify.sh` **GATE: PASS** (default serial + omp8
bit-identical, 0D golden PASS) — the default production path is unaffected.

**Advanced-redox results CHANGE** and need scientific review: previously the branches
were chosen by heap garbage; now `FLAGS(3)` is a correct first-timestep flag and
`FLAGS(4)/(5) = 2` (set at `mod_PELAGIC_ECOLOGY.f90:263-264`) correctly select
`case(2)`. CL29 runs advanced redox, so its numbers move.

**Two further defects fixed alongside (both in the same branch):**

**(a) `FE_II_DISS` never assigned in the saturated branch** —
`aquabc_II_pelagic_model.f90`, the final `elsewhere` of the Fe2+ `where` blocks (:676
first-timestep, :726 every-timestep) set `MULT_FE_II_DISS` but never `FE_II_DISS`,
breaking the invariant `FE_II_DISS = MULT_FE_II_DISS * FE_II` that the other two
branches maintain. **The FLAGS fix ACTIVATED this**: with `FLAGS(4)` garbage the
`select case` branched at random, but with `FLAGS(4) = 2` correctly selecting
`case(2)`, the saturated branch now fires and `FE_II_DISS` was read uninitialised by
`IRON_II_OXIDATION` (:1892) → `R_FE_II_OXIDATION` → `PROCESS_RATES(...,13)` → written
to `*_PROCESS_RATES.out`. Fix: assign `FE_II_DISS = FE_II_DISS_EQ` (:676) and
`FE_II_DISS = DISS_FE_II_CONC_TS_AVG` (:726). **Impact is diagnostic-only**: the
`*_PROCESS_RATES.out` hash changes (`9d0927b1` → `d9772e63`) while the state-variable
output `PELAGIC_BOX_00005.out` is unchanged (`18ead76a`) — the garbage never reached
the trajectory. It was *deterministic* garbage (same heap block reused each timestep),
so it did not cause divergence. Fe3+ (:787, :837) has the identical hole but
`FE_III_DISS` is write-only in the pelagic path, so it is harmless and left alone.

**(b) `DBGSTR_PEL_FE_II_DISS_01` checked the wrong variable** — it ran
`STRANGERSD(FE_II_DISS, ...)` at :635, but `FE_II_DISS` is not assigned until :717, so
it tested freshly allocated uninitialised memory **every timestep** (the internal
arrays are allocated/deallocated per call). Since the routine ends in a bare `stop`,
NaN/Inf-shaped garbage would have aborted the model outright — a latent crash that had
simply never been hit. The intent is unambiguous: the call sits immediately after
`IRON_II_DISSOLUTION` (:627) computes `FE_II_DISS_EQ`, and the routine prints
`HS2_TOT`/`PH`/`TOT_ALK` as "Related variables" — exactly that call's *inputs*. The
`_EQ` was simply dropped. Fix: check `FE_II_DISS_EQ`, and rename the routine to
`DBGSTR_PEL_FE_II_DISS_EQ_01` so the name cannot invite the same confusion back. The
`stop` is kept — it is correct fail-fast behaviour on a *genuine* NaN. Note
`debug_stranger = .true.` is hardcoded at `:244`, so these checks do run in release
builds. (`node_active` is NOT a problem: `aquabc_II_pelagic_model.f90:240-242` sets
`node_active(i) = i` before every DBGSTR call, so the `NODES_STRANGE` indexing is
correct — ESTAS's own uninitialised `node_active` is harmlessly overwritten there.)

**Combined verification (all three fixes):** advredox 30-day **fully deterministic
across ALL 16 output files** — 24 runs (12 @ 25 threads + 12 @ 1 thread), 1
whole-output state; valgrind (`-O0`, advredox) **18 contexts / 10,610 errors → 2
contexts / 26 errors**, with zero heap-allocation origins and zero
`write_pelagic_output` contexts remaining; `tools/refactor_verify.sh` **GATE: PASS**.

**(c) `DAY_OF_YEAR` read before it was ever computed (FIXED 2026-07-17)** — the last 2
valgrind contexts were `aquabc_II_pelagic_lib_NOSTACALES.f90:166`
(`if (DAY_OF_YEAR .lt. 1)`) and `:356` (the akinete-formation `where`), origin: a
**stack** allocation in `MAIN__`. Cause: in `mod_SIMULATE.f90`, `SOLVE` **consumes**
`DAY_OF_YEAR` at the *top* of the timestep loop (:301 → AQUABC → `NOSTOCALES`), but it
is only **produced** at the *bottom* of each iteration (:401-405). Because `TIME`
advances at :393 *before* that computation, the stored value is already correct for the
next iteration's `TIME` — so iterations 2+ were fine and there is **no lag**. Only the
**first iteration of each repeat** read it unset: uninitialised on `REPEAT_NO 1`, or
stale from the previous repeat when `NUM_REPEATS > 1` (latent; `INPUT.txt` uses 1).
Fix: seed `WTIME`/`DAY_OF_YEAR` just before the `do while` loop, using the same formula
as the in-loop computation.

**Pure memory-safety fix — production output byte-identical (GATE: PASS).** This was
*not* expected: nostocales IS enabled in the default config
(`PELAGIC_MODEL_OPTIONS.txt`, "CONSIDER HETEROCYST WITH AKINETES" = 1). It is harmless
today only because `NOST_VEG_HET_C`'s initial condition is **0.00 in every box**, and
the rates `DAY_OF_YEAR` gates are multiplied by that biomass
(`R_FORM_NOST_AKI = AKI_FORM * NOST_VEG_HET_C`, `:362`) — so on timestep 1, the only
one that read garbage, everything it influenced was multiplied by zero. It would bite
immediately if nostocales ever started with non-zero biomass (a warm-start/restart run
or changed ICs). `DAY_OF_YEAR`'s only consumer in AQUABC is the `NOSTOCALES` call
(`aquabc_II_pelagic_model.f90:1254`).

**Advanced-redox path is now valgrind-CLEAN: 0 errors from 0 contexts** (was 18
contexts / 10,610 errors after the FLAGS fix alone). Determinism re-confirmed after
this fix: 20 runs × all 16 files (10 @ 25 threads + 10 @ 1 thread), 1 whole-output
state.

**Effort:** ~1 day (vs ~1–2 days estimated).

---

## 2. Python / Shiny App

### 2.1 [P1] Monolithic app.py (8,012 lines)

**File:** `shiny_app/app.py`

**Problem:** Single file contains all UI definitions, server logic, parsers, file handlers, build logic, and plotting code. Extremely difficult to navigate, test, or maintain.

**Suggested modularization:**
1. `ui/` — UI component definitions (cards, panels, layouts)
2. `server/build.py` — Build and compilation logic
3. `server/simulation.py` — Model run management
4. `server/plotting.py` — Visualization and charting
5. `server/file_handlers.py` — File I/O, parsing, validation
6. `parsers/` — Already partially extracted, complete the separation
7. `app.py` — Thin entry point importing from modules

**Risk:** Large refactor. Shiny for Python module structure needs care with reactive contexts.

**Effort:** ~2–3 days

**Status:** ✅ COMPLETE 2026-07-15 — the app.py modularization is FULLY DONE. **End state:** `server()` is a thin assembler (per-session `RunController`/`AppState` construction + 2 app-level chrome renders + 15 `x_server("id", state)` calls) over **15 namespaced `@module.ui`/`@module.server` Shiny modules** (incl. the converted `diagnostics`) behind the `RunController`/`AppState` contract; no `input.X` crosses a module boundary except via that contract or the `session.root_scope().make_scope("run_control")` bridge; **app.py 8,012 → 786 lines**; the whole `shiny_app/` + `tests/` tree is ruff-clean and CI-linted. Two efforts got here: **(A)** the DECOMPOSITION (2026-07-12/13 — pull pure/non-reactive logic into leaf modules) then **(B)** the SHINY-MODULES REARCHITECTURE (v0.4.0–v0.4.5, 2026-07-14/15 — convert `server()` itself into true namespaced modules). Both detailed below.

**(A) DECOMPOSITION:** `create_ui()` split shipped (verbatim moves, byte-identical render). PHASE 1 (2026-07-12): non-reactive helper extraction — `shiny_app/compiler_env.py` (Intel/compiler detection), `input_analysis.py` (input-file analysis + `INPUT_FILE_CATEGORIES`), `file_locators.py` (output/box discovery); app.py 8616→7925 lines. PHASE 2 (2026-07-13, tasks 1–3 of `refactor/app-py-phase2`): extracted `shiny_app/ui_scripts.py` (inline JS blocks: `reload_script`, `nav_script`, `settings_script`, `help_script`, `changelog_script`, `theme_script`), `shiny_app/ui_panels.py` (14 content-panel fragments: `panel_dashboard`, `panel_model_structure`, `panel_model_build`, `panel_model_control`, `panel_input_files`, `panel_parameters`, `panel_initial_conditions`, `panel_model_options`, `panel_sim_config`, `panel_scenarios`, `panel_plot`, `panel_mass_balance`, `panel_observations`, `panel_map`), and `shiny_app/ui_chrome.py` (sidebar/header/css/offcanvas: `build_sidebar`, `app_header`, `external_css`, `settings_offcanvas`, `help_offcanvas`, `changelog_offcanvas`). `create_ui()` is now a ~53-line thin assembler (was ~270 lines of inline UI). All fragments verified verbatim (byte-identical minus documented param substitutions); 123 python tests green (121 baseline + 2 for `ui_chrome`); F821-clean. Deferred: extract `server()` non-reactive logic, full Shiny-modules rearchitecture. Spec/plan: docs/superpowers/{specs,plans}/2026-07-12-app-py-decomposition*.md. PHASE 3 build-cluster pilot COMPLETE 2026-07-13 (`refactor/app-py-phase3-build-pilot`, tasks 1–2): extracted `shiny_app/build_commands.py` — 4 pure/unit-testable functions (`assemble_estas_command`, `get_available_executables`, `get_executable_info`, `target_exe_name`) — and rewired the 4 corresponding `server()` nested functions (`build_estas_command`, `get_available_executables`, `get_executable_info`, `get_target_exe_name`) as thin wrappers that resolve reactive `input.*()` reads (preserving the original conditional `cmd_binary_filename` read) and delegate to the module via `build_commands.<fn>(...)` (module-import form, to avoid name-shadowing/self-recursion and the `target_exe_name` collision with the existing `@render.ui` of the same name). All 15 original call sites unchanged; 137 python tests green (unchanged from Task 1 baseline, which added `tests/python/test_build_commands.py` — 11 tests; task 2 adds no new tests); F821-clean. PHASE 3 box-network cluster COMPLETE 2026-07-13 (`refactor/app-py-phase3-boxnetwork`, tasks 1–2): extracted `shiny_app/box_network.py` — 6 functions (`parse_pelagic_inputs`, `parse_advective_links`, `parse_bathymetry` — each `INPUTS_DIR` global → `inputs_dir` param — plus verbatim-body figure builders `build_box_network_figure`, `build_bathymetry_figure`, `build_depths_overview`) — and rewired the two Map-Display render handlers (`map_display_plot`, `map_display_info`) to call `box_network.<fn>(...)` (module-import form) at all 7 original call sites; deleted the 6 now-duplicate nested defs (599 lines) from `server()`. 148 python tests green (unchanged from Task 1 baseline, which added `tests/python/test_box_network.py`; task 2 adds no new tests); F821-clean on both `app.py` and `box_network.py`. Deferred: 7 pre-existing non-F821 lint smells carried over verbatim in `box_network.py` (unsorted import block, 3× unnecessary `open(..., 'r')` mode arg, unused `depths`/`bnd_id` locals, one semicolon-joined statement) — cosmetic, left untouched to keep the move byte-identical; cleanup deferred to a future lint pass. PHASE 3 output-data cluster COMPLETE 2026-07-13 (`refactor/app-py-phase3-outputdata`, tasks 1–2): extracted `shiny_app/output_data.py` — 7 pure functions (`looks_numeric`, `format_elapsed`, `get_output_folder_from_config`, `get_output_files_info`, `get_output_columns` [renamed from `_get_output_columns`], `get_output_directories`, `get_output_files_from_dir`) — and rewired all 11 corresponding `server()` call sites to `output_data.<fn>(...)` (module-import form); deleted the 7 now-duplicate nested defs from `server()` (193-line net reduction in `app.py`). 155 python tests green (unchanged from Task 1 baseline, which added `tests/python/test_output_data.py`; task 2 adds no new tests); F821-clean on both `app.py` and `output_data.py`. **(B) SHINY-MODULES REARCHITECTURE — COMPLETE, released `v0.4.0`–`v0.4.5` (2026-07-14/15, all CI-verified incl. the Playwright/Selenium integration-tests).** Converted `server()`'s ~5,600-line closure into **15 true `@module.ui`/`@module.server` modules** — `dashboard`, `model_structure`, `model_build`, `input_files`, `parameters`, `initial_conditions`, `model_options`, `scenarios`, `mass_balance`, `observations`, `map`, `diagnostics`, `sim_config`, `run_control`, `plot` — behind a shared `RunController` (run/build session carrying `command_config` [a `List[str]` argv] / `constants_config` / `run_executable_name` / `active_executable` / `exe_list_version`) + 4-field `AppState` (`run`, `navigate`, `output_config_version`, `sim_config_version`). Phases: 0 shared contract (`v0.4.0`, zero-namespacing) → 1 pilot `parameters` (`v0.4.1`) → 2 seven leaf modules (`v0.4.2`) → 3 output cluster + dead-bus removal (`v0.4.3`) → 4 run/build/dashboard cluster (`v0.4.4`; contract-first rewiring routes cross-module values through `RunController` before any id namespaces, DOM-identical; `session.root_scope().make_scope("run_control")` bridge for the shared `sim_output_dir`/`run_executable` widgets) → 5 final cleanup (`v0.4.5`: dropped unread `build_config`, empty `ui_panels.py`, ~70 dead imports; then app.py made fully ruff-clean + CI extended to lint `shiny_app/` + `ruff` pinned to `0.15.21`). **This RESOLVES all previously-deferred phase-3 items** — `mass_balance`/`observations`/`scenarios` became modules, the reactive CSV cache is now internal to the `plot` module, and `_execute_build_process` lives in `RunController`. app.py final = **786 lines**. Spec + phase plans: `docs/superpowers/{specs,plans}/2026-07-1[45]-*shiny-modules*`; each phase executed subagent-driven with per-task + whole-branch reviews (Phase 4's plan also got a Workflow adversarial pre-review that caught 13 defects).

---

### 2.2 [P1] Bare Except Blocks (5 remaining)

> **Status:** ✅ COMPLETED (Sprint 1, 2026-02-14) — Fixed — 5 bare `except` blocks replaced with specific exception types

**File:** `shiny_app/app.py`

**Problem:** 5 bare `except:` blocks catch all exceptions including `SystemExit`, `KeyboardInterrupt`, making debugging difficult and hiding real errors.

**Fix:** Replace with specific exception types:
```python
# Before:
except:
    pass

# After:
except (ValueError, FileNotFoundError, OSError) as e:
    logger.warning(f"Failed to process: {e}")
```

**Effort:** ~30 minutes

---

### 2.3 [P1] Duplicated Build/Rebuild Logic

> **Status:** ✅ COMPLETED (Sprint 3, 2026-02-14) — Done — extracted `_execute_build_process` helper (−110 duplicated lines)

**File:** `shiny_app/app.py`

**Problem:** Build and rebuild handlers share ~190 lines of nearly identical logic (compiler setup, flag assembly, subprocess management, output parsing). Bugs fixed in one copy may be missed in the other.

**Fix:** Extract shared logic into a `_execute_build(compiler, mode, openmp, clean_first)` helper function. Both handlers call it with different `clean_first` flags.

**Effort:** ~1–2 hours

---

### 2.4 [P2] Blocking I/O in Reactive Handlers

**Status:** ✅ COMPLETE 2026-07-18 — scoped to the genuine event-loop-freeze sites.

**Files:** `shiny_app/modules/mass_balance.py`, `shiny_app/modules/observations.py`
(the reactive handlers moved out of `app.py` in the module rearchitecture).

**Problem:** A few reactive handlers did a full `OUTPUT.csv` read **plus** heavy pandas
computation synchronously on the Shiny event loop, freezing the whole UI (all sessions)
for large model outputs.

**What was actually blocking (measured, not assumed):** exactly **3 button-triggered
handlers** — `mass_balance.calculate_mass_balance`, and `observations`'
`load_observation_file` (upload) + `generate_sample_observations`. Everything else is
already fine: `plot` uses bounded `nrows=` reads + caches its one full read; `dashboard`
only stats/previews; the config-file parsers read KB-sized files (which the TODO itself
says stay synchronous).

**Fix — `@reactive.extended_task` + `asyncio.to_thread`.** Each heavy handler split into:
(1) a module-level *pure blocking helper* (`_compute_mass_balance_blocking`,
`_compare_blocking`, `_sample_and_compare_blocking`) doing only the read+compute;
(2) an `async` `@reactive.extended_task` that runs the helper via `asyncio.to_thread`
(**crucial**: `extended_task` requires an async fn and runs it as an asyncio task *on
the loop* — a plain `async def` doing blocking pandas would still freeze it, so the work
must be pushed to a worker thread); (3) a launch effect (button event + precondition
check) that invokes the task; (4) a collect effect that reads `.status()`/`.result()`,
publishes the reactive Values, and does the notifications/`update_select`. A lightweight
busy indicator ("running in background") shows in each tab's summary output while the
task runs.

**Verified:** ruff clean; full python suite **183 passed** (178 + 5 new in
`tests/python/test_async_io_helpers.py` — blocking-helper return-shape contracts, the
`_compare_blocking` missing-file guard, and a static `ast` check that **all 3**
extended_task targets are `async` — the invariant that prevents a sync→`TypeError` at
session construction); `create_ui().tagify()` backstop passes; CI Playwright
integration-tests construct the tasks on session connect.

**Effort:** ~4 hours (as estimated).

---

### 2.5 [P2] Missing Unit Tests for Business Logic

> **Status:** ✅ COMPLETED (Sprint 3, 2026-02-14) — Done — 4 functions extracted to `utils.py`, 28 tests added

**Files:** `shiny_app/app.py` (embedded functions)

**Problem:** Business logic functions (parameter validation, unit conversions, mass balance calculations, plot data preparation) are embedded in app.py and have no unit tests. The 46 existing pytest tests cover parsers and security, not core model logic.

**Fix:**
1. Extract testable functions from app.py into utility modules
2. Write pytest tests for: validation rules, unit conversions, mass balance math, output file parsing

**Effort:** ~1 day

---

### 2.6 [P3] Hardcoded Configuration Values

**Status:** ✅ COMPLETE 2026-07-18 — pragmatic subset (the app's dual-import pattern
made full centralization a net negative).

**Problem:** Timeout durations and default filenames were scattered as magic literals
across `shiny_app/`.

**Constraint that shaped the fix:** the app imports siblings via a
`try: from shiny_app.X ... except ImportError: from X ...` dual-import (it runs both
as a package and with `shiny_app/` on `sys.path`; tests import package-style). So every
`config.py` consumer needs that ~3-line block — which for a one-line constant (e.g.
each module's self-contained `ROOT`) is *more* boilerplate + coupling than the
duplication it removes.

**Fix (`shiny_app/config.py`, stdlib leaf module):** centralized the values where a
named constant is a clear net win:
- **Subprocess timeouts** (the flagship): `PROCESS_SHUTDOWN_TIMEOUT`, `LINE_COUNT_TIMEOUT`,
  `SUBPROCESS_PROBE_TIMEOUT` (dedups 3 identical `timeout=5` in compiler_env + 1 in
  build_commands), `PDF_REPORT_TIMEOUT`, `DEEP_PDF_REPORT_TIMEOUT` — wired across
  compiler_env, build_commands, app_state, diagnostics, app.
- **`DEFAULT_CONSTANTS_FILE = "WCONST_04.txt"`** — wired to its 4 `os.path.join` code
  sites in mass_balance, parameter_parser, scenarios.

**Deliberately left in place** (documented in `config.py`): the per-module one-line
`ROOT` idiom (5 files — trivial, self-contained), and `INPUT.txt`/`PELAGIC_INPUTS.txt`
literals (mostly dict keys / labels / single `ROOT`-relative uses where a shared
constant adds dual-import boilerplate without net benefit).

**Verified:** `ruff` clean on all touched files; full python suite **178 passed**; the
mandatory `import shiny_app.app; create_ui().tagify()` backstop passes.

**Effort:** ~1–2 hours (as estimated).

---

## 3. CI / Build / Test Infrastructure

### 3.1 [P1] No Compiler Matrix in CI

**File:** `.github/workflows/ci.yml`

**Problem:** CI only tests with gfortran on Ubuntu. The project supports ifort and ifx, plus macOS. Compiler-specific bugs (especially Intel Fortran quirks) won't be caught until users report them.

**Fix:** Add a build matrix:
```yaml
strategy:
  matrix:
    os: [ubuntu-latest]
    compiler: [gfortran]
    # Future: add ifort/ifx when Intel oneAPI action is available
```

Start with gfortran-only matrix (documenting the intent to add Intel later when CI runners support it).

**Effort:** ~1 hour

**Status:** ✅ COMPLETED 2026-07-12 — `build-and-run` job converted to a `strategy.matrix` (`fail-fast: false`, `runs-on: ${{ matrix.os }}`, compiler via job-level `env: FC`). Active entry gfortran/ubuntu-latest; commented, ready-to-enable entries for `ifx` (Intel oneAPI) and `macos-latest`. The Makefile's `ifeq ($(origin FC),default)` means the exported `FC` propagates, so a new matrix row switches compilers with a one-line change.

---

### 3.2 [P1] Integration Tests Excluded from CI

> **Status:** ✅ COMPLETED (Sprint 3, 2026-02-14) — Done — Playwright integration job added to CI (19 tests)

**File:** `.github/workflows/ci.yml`

**Problem:** 19 Playwright and 9 Selenium integration tests exist but don't run in CI. They require a running Shiny app instance and browser dependencies.

**Fix:**
1. Add a CI job that installs Playwright browsers
2. Start the Shiny app in background
3. Run Playwright tests against it
4. Tear down on completion

**Effort:** ~2–4 hours

---

### 3.3 [P1] No Code Coverage Tracking

> **Status:** ✅ COMPLETED (Sprint 2, 2026-02-14) — Done — `pytest-cov` with CI coverage reporting

**Problem:** No visibility into which code paths are tested. Can't measure improvement or identify untested critical paths.

**Fix:**
- Python: Add `pytest-cov` to dev dependencies, add `--cov=shiny_app --cov-report=xml` to CI
- Fortran: Consider `gcov` integration for unit test coverage (lower priority)
- Upload reports to Codecov or similar service

**Effort:** ~2 hours (Python), ~4 hours (Fortran)

---

### 3.4 [P2] GitHub Actions Not Pinned to SHA

> **Status:** ✅ COMPLETED (Sprint 2, 2026-02-14) — Done — GitHub Actions pinned to SHA

**File:** `.github/workflows/ci.yml`

**Problem:** Actions referenced by tag (e.g., `actions/checkout@v4`) instead of SHA. A compromised action could inject malicious code into the build.

**Fix:**
```yaml
# Before:
- uses: actions/checkout@v4

# After:
- uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4.1.1
```

**Effort:** ~30 minutes

---

### 3.5 [P2] No Dependency Caching in CI

> **Status:** ✅ COMPLETED (Sprint 2, 2026-02-14) — Done — pip caching enabled in CI

**File:** `.github/workflows/ci.yml`

**Problem:** Every CI run installs Python packages and potentially rebuilds Fortran from scratch. No caching of pip packages or compiled objects.

**Fix:**
```yaml
- uses: actions/setup-python@v5
  with:
    python-version: '3.11'
    cache: 'pip'
```

**Effort:** ~30 minutes

---

### 3.6 [P2] No Pre-commit Hooks

> **Status:** ✅ COMPLETED (Sprint 3, 2026-02-14) — Done — pre-commit `ruff` + file-hygiene hooks

**Problem:** Developers can commit code that fails linting or has formatting issues. These are only caught in CI after push.

**Fix:**
1. Add `.pre-commit-config.yaml` with ruff, trailing whitespace, end-of-file fixer
2. Document in CONTRIBUTING.md: `pre-commit install`

**Effort:** ~1 hour

---

### 3.7 [P3] No Release Workflow

> **Status:** ✅ COMPLETED (2026-07-10) — Done — `.github/workflows/release.yml` + `tools/extract_release_notes.sh`

**Problem:** No automated process for creating tagged releases with changelogs and build artifacts.

**Fix:** Add `.github/workflows/release.yml` triggered on version tags:
1. Build the Fortran library
2. Run full test suite
3. Create GitHub Release with changelog excerpt and binary artifact

**Effort:** ~2–4 hours

---

## 4. OpenMP Follow-up Items

### 4.1 [P2] Performance Benchmarking

**Status:** ✅ COMPLETE 2026-07-15 — benchmarked via a micro-benchmark harness (`tools/benchmark_openmp.sh` + `SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/aquabc_II_pelagic_benchmark.f90`) that reuses the node-agnostic 0D interface to time the real `AQUABC_PELAGIC_KINETICS` `!$omp parallel` region at `OMP_NUM_THREADS=1,2,4,8` × `nkn=100/500/1000` with `omp_get_wtime()`. Results + analysis in **`docs/OPENMP_PERFORMANCE.md`**; run via `make benchmark-openmp`. Headline: speedup grows with `nkn` (negligible at nkn=100, **2.84× at nkn=1000 on 8 threads**), plateaus ~8 threads; an Amdahl fit gives **~26 % serial fraction** at nkn=1000 (the serial CO2SYS call — see 4.2). Recommend enabling OpenMP for `nkn≳500` with 2–4 threads for best efficiency; leave off for the default/CL29 small networks (<1.3×). (Intel i9-10940X, 14C/28T, gfortran 13.3.0.)

**Task:**
1. Create a benchmark script that times `AQUABC_PELAGIC_KINETICS` with `OMP_NUM_THREADS=1,2,4,8`
2. Use a representative test case with realistic `nkn` (100–1000 nodes)
3. Report wall-clock time and compute speedup/efficiency
4. Document results in `docs/OPENMP_PERFORMANCE.md`

**Effort:** ~2–4 hours

---

### 4.2 [P2] CO2SYS Parallelization

**Status:** ✅ COMPLETE 2026-07-15 — profiled (gprof: CO2SYS ~15% of kinetics, >10% gate met), then parallelized the pelagic CO2SYS call by chunking its `ntps=nkn` arrays `[ns:ne]` across threads with private output buffers (`aquabc_II_pelagic_model.f90` `RUN_CO2SYS` block; `co2sys.f90` unchanged — CO2SYS is pure/stateless). **Result: 8-thread speedup at nkn=1000 jumped from 2.84× to 6.55×** (roughly doubled at large nkn; see `docs/OPENMP_PERFORMANCE.md`). Correctness: NOT bit-identical (whole-vector Newton pH converges to its chunk's slowest element) but drift is ~1000× below the solver's `pHTol=1e-4` (0D golden `nkn=1` bit-identical + passes; full model nkn=25 1-vs-2-thread max abs diff 1e-6 = output print precision, max rel 7.8e-9). Scope: pelagic call site only (the 4 sediment CO2SYS calls are a follow-up, only active with MODEL_SEDIMENTS=1).

**File:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Problem:** CO2SYS computation is currently sequential (before the parallel region). For large `nkn`, this could become the serial bottleneck (Amdahl's law).

**Fix:** Profile first. If CO2SYS takes >10% of kinetics time, parallelize its loop similarly to the main computation block.

**Update 2026-07-15 (from 4.1 benchmark):** now quantitatively justified — the OpenMP benchmark measured a **~26 % serial fraction** at `nkn=1000` (Amdahl fit), which caps OpenMP scaling at ~3.9× regardless of thread count. CO2SYS (`aquabc_II_co2sys.f90`, no `!$omp`, O(`nkn`) serial before the parallel region) is the prime suspect. Parallelizing its per-node loop would lift the ceiling at large `nkn`. See `docs/OPENMP_PERFORMANCE.md`.

**Effort:** ~4 hours (including profiling)

---

### 4.4 [P2] Full-model OpenMP hang at high thread counts (NEW — found during 4.2)

**Status:** ✅ COMPLETE 2026-07-15 — **empty-chunk barrier deadlock in the kinetics `!$omp parallel` region** (NOT the solver/transport path as first guessed). Root cause (found via thread-id checkpoint tracing since gdb ptrace was sandbox-blocked): the `chunk = ceil(nkn/nthreads)` split left the last thread with an **empty chunk** (`nkn_local ≤ 0`) whenever `nthreads` didn't evenly divide `nkn` (e.g. nkn=25/8thr → thread 7 gets 0 nodes); that thread skipped the region's collective `!$omp barrier`s → the other threads waited for it forever (active-spin → high CPU, no output). Only bit when `nkn < nthreads` or non-divisible, which is why nkn=1000/8 worked but nkn=25/8 hung; reproduced in the micro-benchmark. Pre-existing (Phase-4 OpenMP work, independent of 4.2 — stock code hung identically). **Fix:** balanced chunk split (`base = nkn/nthreads`, first `mod(nkn,nthreads)` threads get +1 node) + `num_threads(min(nkn, omp_get_max_threads()))` cap → every thread always gets ≥1 node. Applied to both the kinetics and CO2SYS regions. Verified: `ESTAS_II` completes at 8 threads; 0D golden bit-identical; benchmark speedup unchanged (6.4× @ nkn=1000/8); @1-vs-@8 drift ≤1e-6 (output precision). See `docs/OPENMP_PERFORMANCE.md`.

**Effort:** ~4–8 hours (concurrency debugging)

---

### 4.3 [P3] OpenMP Thread Affinity Guidance

**Status:** ✅ COMPLETE 2026-07-15 — documented in `docs/OPENMP_PERFORMANCE.md` §"Thread affinity", now with *measured* effect: on the single-socket test machine `OMP_PROC_BIND=close`/`OMP_PLACES=cores` gave **no benefit** (within run-to-run noise, marginally worse — no NUMA to optimize). Settings kept as recommended practice for multi-socket/NUMA hardware, with a note to re-measure on the target deployment machine. (Done alongside 4.1.)

**Task:** Document recommended `OMP_PROC_BIND` and `OMP_PLACES` settings for optimal cache behavior:
```bash
export OMP_PROC_BIND=close
export OMP_PLACES=cores
```

**Effort:** ~30 minutes (documentation only)

---

## 5. Testing Improvements

### 5.1 [P2] Fortran Test Coverage Expansion

**Current:** 26 test programs (0 failures, verified 2026-07-05) covering phytoplankton, zooplankton, redox/speciation, organic-carbon mineralization, iron and dissolved-metal chemistry, pH correction, ammonia chemistry, light extinction, allelopathy, sediment bioturbation, and utilities.

**Missing coverage:**
- CO2SYS (complex equilibrium chemistry — high bug risk, still untested)
- Main sediment diagenesis model (`aquabc_II_sediment_model_1_fast.f90`) — bioturbation is tested, but the solute/kinetics core is not
- End-to-end integrated pelagic + sediment run (see 5.2)

Note: ALLELOPATHY, light extinction (`light_kd`), ammonia chemistry, iron chemistry, dissolved metals, and pH correction now have dedicated test programs (`test_allelopathy`, `test_light`, `test_ammonia_chem`, `test_iron_ii`, `test_diss_me`, `test_ph_corr`) — they are no longer coverage gaps.

**Effort:** ~1 day per subroutine

---

### 5.2 [P2] End-to-End Regression Test

**Problem:** No automated test that runs the full AQUABC model and compares output against a reference solution. Unit tests verify individual subroutines but not the integrated system.

**Fix:**
1. Create a small test case (10 nodes, 10 timesteps)
2. Generate reference output with the current code
3. Add a CI job that runs the model and diffs against reference
4. Allow small floating-point tolerance (1e-10 relative)

**Effort:** ~4–8 hours

**Status:** ✅ COMPLETED 2026-07-12 — golden-file regression on the 0D pelagic example. `tests/regression/pelagic_0D_golden.csv` (current-code output downsampled every 50th row across the full 1096-day run) + `tests/regression/compare_0D.py` (stdlib tolerance diff: exact header check catches column reorder/rename, per-cell `rtol/atol` numeric check). Wired into the `build-and-run` CI job (`--rtol 1e-6`). `tests/python/test_e2e_regression.py` unit-tests the comparison logic (runs in the Python-only job) and diffs a fresh 0D output when present. Full python suite 107 passed. Note: golden is gfortran-generated; loosen tolerance / add per-compiler goldens when the matrix (3.1) gains ifx/macOS.

---

## Implementation Roadmap

### Sprint 1 — Critical Fixes (1–2 days) --- COMPLETED 2026-02-14
- [x] 1.1 ALLELOPATHY memory leaks — **No fix needed** (file `aquabc_II_pelagic_lib_ALLELOPATHY.f90` does not exist; `mod_ALLELOPATHY.f90` has proper alloc/dealloc)
- [x] 1.2 K_E division guard — **Fixed** in CYANOBACTERIA, FIX_CYANOBACTERIA, NOSTOCALES library files (not pelagic_model.f90 as originally stated)
- [x] 1.3 SAVE variable thread safety audit — **Documented** (22 vars in pelagic_interface + 3 in STRING_UTILS; no active race under current OpenMP usage)
- [x] 2.2 Bare except blocks — **Fixed** (5 blocks replaced with specific exception types in app.py)

### Sprint 2 — Numerical Safety & CI (2–3 days) --- COMPLETED 2026-02-14
- [x] 1.4 CO2SYS safe_exp — **Fixed** (8 vulnerable exp() calls wrapped with safe_exp in aquabc_II_co2sys.f90)
- [x] 1.5 Remaining division-by-zero audit — **Audit complete** (2026-02-14). All ~80 divisions in pelagic_model.f90 confirmed safe: iron/Mn use conditional guards, zoo/det use max(), Monod kinetics are mathematically safe, CHLA divides by constants only. One missing Fe3+ first-timestep guard added.
- [x] 3.3 Python code coverage — **Added** (pytest-cov with CI reporting, 10% baseline)
- [x] 3.4 Pin GitHub Actions to SHA — **Done** (5 action references pinned)
- [x] 3.5 CI dependency caching — **Done** (pip cache enabled)

### Sprint 3 — Code Quality (3–5 days) --- COMPLETED 2026-02-14
- [x] 2.3 Deduplicate build/rebuild logic — **Done** (extracted _execute_build_process helper, -110 duplicated lines)
- [x] 2.5 Unit tests for business logic — **Done** (4 functions extracted to utils.py, 28 tests added, 65 total)
- [x] 3.2 Integration tests in CI — **Done** (Playwright job added, 19 tests)
- [x] 3.6 Pre-commit hooks — **Done** (ruff + file hygiene hooks configured)

### Sprint 4 — Architecture (1–2 weeks)
- [x] 4.1 OpenMP benchmarking — **Done** (2026-07-15; `tools/benchmark_openmp.sh` + `docs/OPENMP_PERFORMANCE.md`; 2.84× @ nkn=1000/8thr, ~26% serial → 4.2)
- [x] 2.1 Modularize app.py — **DONE** (decomposition 2026-07-12/13 → leaf modules, then the Shiny-modules rearchitecture `v0.4.0`–`v0.4.5`, 2026-07-14/15: `server()` → 15 namespaced `@module` modules behind `RunController`/`AppState`; app.py 8,012 → 786 lines; see §2.1)
- [x] 1.6 Decompose mega-subroutine — **Done** (2026-07-16; 5 `contains` procedures, byte-identical, gate-verified; found bugs 1.10/1.11)
- [ ] 5.1 Expand Fortran test coverage
- [x] 5.2 End-to-end regression test — **Done** (2026-07-12, 0D pelagic golden-file regression wired into CI; see §5.2)

### Backlog (as time permits)
- [x] 1.7 Sediment model variable cleanup — **Done** (2026-07-18; compiler-unused already 0; removed 3 stale write-only locals CA/MG/CONSIDER_CO2_REARATION, kept+tagged 9 disabled-chemistry scaffolding vars `[WO]`; byte-identical vs fresh baseline + 0D golden)
- [x] 1.8 Named physics constants — **Done** (2026-07-18; only real magic number was 273.15 → `CELSIUS_TO_KELVIN`; surfaced + fixed a latent single-precision Kelvin-offset bug in DO_SATURATION/CO2SYS; ~1e-6 intended output change, 0D golden regenerated)
- [x] 1.9 IOSTAT error handling — **Done** (2026-07-18; `OPEN_INPUT_FILE` helper guards all 24 input `status='OLD'` opens → clean message + nonzero `error stop` on missing/unreadable file; byte-identical when files exist; per-read content checks deliberately out of scope)
- [x] 1.10 [P1] Model-constants OOB write — **Done** (2026-07-17; nconst 318→323; memory-safety fix, production output byte-identical [adversarial review corrected the garbage-BETA framing])
- [x] 1.11 [P1] Advanced-redox uninitialised-memory non-determinism — **Done** (2026-07-17; root cause was a local `FLAGS` in `CALC_DERIV` shadowing the global, leaving `FIRST_TIME_STEP`/`INIT_OPTION_*` reading garbage; one-line fix, 40/40 + 5/5 deterministic, default path byte-identical)
- [x] 2.4 Async file I/O — **Done** (2026-07-18; 3 heavy button-triggered handlers in mass_balance/observations moved off the event loop via `@reactive.extended_task` + `asyncio.to_thread`; 183 tests incl. 5 new + async-guard; create_ui backstop pass)
- [x] 2.6 Centralized configuration — **Done** (2026-07-18; `shiny_app/config.py` — named subprocess timeouts + `DEFAULT_CONSTANTS_FILE`, wired across 8 files; ROOT/other filenames deliberately left per the app's dual-import cost; 178 tests + create_ui backstop pass)
- [ ] 3.1 Compiler matrix (when Intel CI available)
- [x] 3.7 Release workflow — **Done** (2026-07-10, `.github/workflows/release.yml` + `tools/extract_release_notes.sh`)
- [x] 4.2 CO2SYS parallelization — **Done** (2026-07-15; chunked across threads; nkn=1000/8thr speedup 2.84×→6.55×; see docs/OPENMP_PERFORMANCE.md)
- [x] 4.4 Full-model OpenMP hang @≥8 threads — **Done** (2026-07-15; empty-chunk barrier deadlock in the kinetics region; fixed via balanced chunking + thread cap; ESTAS_II now scales to 8 threads)
- [x] 4.3 Thread affinity documentation — **Done** (2026-07-15; measured negligible on single-socket, see docs/OPENMP_PERFORMANCE.md)

---

*Generated from deep audit of AQUABC v0.2 codebase on 2026-02-13.*
