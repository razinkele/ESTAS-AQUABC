# CL29 Sediment Phase 2 — Two-Type (Sandy/Muddy) Infrastructure + Calibration

**Date:** 2026-07-09
**Status:** 🟡 Phase 2a implemented; Phase 2b data-blocked. Two-type reader (`mod_BOTTOM_SEDIMENTS.f90`, `84fe0ba`), typemap (`mod_SED_TYPEMAP.f90` + `test_sed_typemap`, `ca35e84`), per-box flux-output fix (`81beef5`) and the multi-type converter author all landed. Phase 2b (sandy/muddy flux-fidelity calibration) is **blocked on an expert box→facies map** — depth is not a valid proxy (box 19 is shallow-but-muddy). **Stale premise:** the spec fixes "advanced-redox=0", but CL29 now ships advanced-redox=1 (`FEPO4_KSP_LOG10=-25`); the §4.1a NO-GO conclusion still holds — external boundary-P is the gap-closure lever, not sediment.
**Goal:** Calibrate the CL29 sediment diagenesis so its benthic N/P/Si fluxes match measured
Curonian sandy/muddy data **and** the box-19 spring diatom bloom reaches the observed
~47 mg/m³ — replacing the Phase-1 stability values with data-anchored ones, and giving
sandy vs muddy sediments distinct behaviour.

> **Builds on Phase 1** (`2026-07-08-cl29-sediment-diagenesis-phase1-design.md`, merged):
> `MODEL_SEDIMENTS=2` stands up and runs stably (serial), off by default, with a single
> uniform sediment profile of *stability* values.

## 1. Objective

**Both must hold** (user decision): the modeled benthic NH4/NO3/PO4/DSi fluxes match the
measured sandy/muddy seasonal values **and** the box-19 spring diatom Chl-a reaches
47 ± 14 mg/m³ under the validation-doc guardrails.

**Tolerance semantics — state the basis, don't conflate them.** The Chl-a bands 47 ± 14
(spring) and 96 ± 56 (summer) are **field/population variability**, not instrument
measurement SD. Because summer's band is ±58 % (range 40–152) while spring's is ±30 %
(range 33–61), the ±band is **asymmetric in how binding it is**: under "both-must-hold" the
**spring diatom target is the binding constraint** and summer is a loose sanity band — this
is explicit, not accidental. Do not treat the two Chl-a targets as equally tight.

**Flux tolerance (provisional, finalized from measurement uncertainty in 2b).** Report the
tolerance **basis** with each target (field/spatial variability vs. instrument uncertainty),
then band accordingly:
- **Fluxes well-resolved and one-signed:** modeled seasonal (spring/summer) mean flux per
  type within ±1 SD of the measured value (SD basis stated), and — where the measurement
  resolves it — with the **correct sign** (e.g. NO3 uptake into muddy sediment vs. release
  from sandy; see §4 guardrails).
- **Near-zero or sign-changing fluxes (small net residuals of large gross
  nitrification/denitrification — the NO3 case):** ±30 % (or ±1 SD) of a near-zero magnitude
  is meaningless. Replace it with an **absolute floor** (a flux magnitude below which the
  net flux is treated as "≈ 0, direction only") **plus the sign criterion**; a modeled flux
  inside the floor band passes on sign, not magnitude.
Until the data lands, use ±30 % of the measured magnitude as a working stand-in for the
one-signed case so the fidelity criterion is testable; the absolute floor for the
sign-changing case is set from the measured gross-flux scatter in 2b.

**Box-19 granularity.** Gap-closure is a single-box, 5-yr-mean point target (box 19).
Matching the *type-aggregate* flux does not by itself guarantee box 19's *own* local benthic
P return is large enough to close its bloom gap. Box 19 is a depositional, P-return-dependent
station and is expected to be **muddy** (type 2); 2b must (a) confirm box 19's type in the
box→type map is consistent with its measured *local* flux, and (b) verify box 19's *own*
modeled benthic PO4 flux — not the type mean — supplies the P needed for 47 ± 14.

**Conflict is declared, not assumed.** In a multi-dimensional geometry + global-rate space you
can only fail to find a solution, not prove none exists, so "genuinely conflict" needs a
**decidable** stopping protocol: declare a conflict only after the analytical seed (§4.1 step 1)
plus a **bounded search** — all profile geometry/IC within the **§4.2 bounds table** and the
global rate constants within their **§4.2 documented ranges** — has been run through the
verification loop until it meets the **convergence-based stopping rule of §4.2** (deposition
self-consistency within the stated X %, monotone residual) *or* hits the **max-iteration cap**,
and still cannot hit both targets. Without the enumerated §4.2 bounds/ranges and the
convergence rule, "search exhausted ⇒ conflict" has no stopping condition and could be
declared prematurely or never; the §4.2 table is what makes this clause testable. Note the
mechanism pre-check of §4.1a: if that analysis shows benthic N/P return cannot supply box 19
under the CL29 advanced-redox=0 configuration, the conflict is a **§6 model-mechanism gap**
identified *before* the search, not a calibration failure found during it.
**Concrete conflict hypothesis to test:** does the measured muddy PO4 flux,
applied at box 19, supply enough P to reach 47 ± 14 while keeping water-column DIP realistic
and free of interannual drift? That is the specific place the two goals may collide. If so,
**stop and surface it as a finding** rather than force either.

## 2. Decomposition

Phase 2 splits by data dependency:

- **Phase 2a — two-type sediment infrastructure (buildable NOW, data-independent).** The
  Fortran reader extension for sandy/muddy profiles, the per-box flux-output fix, and
  converter support for two profiles + a box→type map. Fully testable with placeholder
  profiles (seeded from the Phase-1 stability values).
- **Phase 2b — calibration + validation (when the measured data arrives).** Derive the
  sandy/muddy geometry/IC + global oxic/anoxic rate constants analytically from the measured
  fluxes, verify with a few full runs, and check *both-must-hold*.

**This spec's implementation plan covers 2a.** 2b is scoped here but gets its own
spec→plan cycle once the data lands.

**Phase-2 gate — §4.1a is a pre-check, not a sub-step of calibration.** The §4.1a
anoxic-remineralization analysis is logically *prior* to 2b: under CL29 advanced-redox = 0 it
can pre-judge the box-19 gap-closure half of *both-must-hold* infeasible **before any calibration
run**. Resolve it as an explicit go/no-go before committing to 2b's gap-closure effort (it lives
in §4 for narrative flow, but decision-wise it gates 2b). Crucially it does **not** gate 2a: the
two-type infrastructure, the per-box flux-output fix, and the measured-flux *fidelity* comparison
are worth building regardless of the §4.1a verdict — only 2b's *gap-closure* target is exposed to
it. If §4.1a returns "infeasible", 2a still delivers data-anchored sandy/muddy fluxes plus the
flux-output bug fix, and the gap-closure goal converts to the §6 model-mechanism finding rather
than being chased through runs.

## 3. Phase 2a architecture

Three coordinated pieces. **Refinement from brainstorming:** the sandy vs muddy behaviour
need NOT be encoded as *per-box* rate constants — the redox contrast emerges from per-box
geometry + IC driving the per-cell redox state, while the kinetic constants stay **global**.
Two distinct code paths produce that emergent contrast, and both are per-cell and read only
global scalar constants:

- **Particulate dissolution** (PON/POP/PSi → dissolved-organic) selects oxic vs anoxic rates
  by a per-cell `where (SED_DOXY ≥/< DOXY_AT_ANOXIA)` mask
  (`aquabc_II_sediment_model_1_fast.f90:2455-2475`; oxic block 2455-2464, anoxic 2466-2475),
  i.e. `K_OXIC_DISS_*` vs `K_ANOXIC_DISS_*`. DSi flux comes straight from this mask
  (`R_DISS_PSi`).
- **DOC/DON/DOP mineralization** — the *direct* source of dissolved NH4 and PO4 — does **not**
  use that binary mask. It uses the graded redox-sequence limitation driven by
  `LIM_DOXY_RED`/`LIM_NO3N_RED`, split across two blocks: **DOC/DON mineralization at
  `:2265-2354`** (`SED_K_MIN_DOC_*` at 2270-2280, `SED_K_MIN_DON_*` at 2297-2354) and — the
  direct **PO4** source, the whole point of this bullet — **DOP mineralization at `:2372-2447`**
  (`SED_K_MIN_DOP_*` used at 2384-2442). The earlier "`:2265-2360`" span undercounts: it stops
  before the DOP/PO4 block entirely.

Sediment O2 — hence which regime each cell sits in — is driven **per-box by organic-matter
loading first**. But separate what is an *input lever* from what is *endogenous or transient*:

- **OM deposition is NOT a §3.1 input lever.** It is endogenous — computed each step from the
  water-column settling flux `FLUXES_TO_SEDIMENTS` fed into `SETTLING_DERIVS`
  (`aquabc_II_sediment_model_1_fast.f90:1954-1956`) — and is coupled to the very benthic
  N/P fluxes being calibrated (§4.1). It cannot be set independently per box. (The
  `DEPOSIT_CONC = sed_porosities·SURF_WATER_CONCS` dissolved branch at `:1975` is **dead
  code**: its guard `in_which_phase==0 .and. ==2` can never be true, so do not cite it as the
  deposition mechanism.)
- **IC organic/O2 pools are spin-up initialization, not a persistent separator.** Over the
  multi-year integration toward the quasi-steady state that §4.1 assumes, IC O2 equilibrates
  in days–weeks and the IC PON/POP/POC pools relax on the deposition/burial timescale, so IC
  cannot hold two boxes in distinct redox regimes on its own — sustained separation must come
  from persistent forcing/geometry. Treat the per-type IC block as a spin-up initializer and
  **verify (see §4 guardrails) that IC influence on the reported fluxes is negligible after a
  stated spin-up length** (the whole first year of the 5-yr run is discarded for this check
  unless demonstrated otherwise).
- **Porosity is a secondary, oppositely-signed diffusion term:** in this diffusion-only model
  the tortuosity correction is `1/(1+3(1-φ))` (`:1682-1683`), so *higher* porosity gives *deeper*
  O2 penetration → *more* oxic. Real muddy sediment (high φ) is anoxic because of OM loading
  and lost advective flushing, not porosity — and porewater advection is off here
  (`switch_advection=0`).

**Persistent, independently-settable per-box separators therefore reduce to burial +
particle-mixing (`PART_MIXING_COEFFS`) + geometry (depths/densities, with porosity working the
*wrong* way for redox), plus the coupled overlying water column** (endogenous deposition). So
sandy (oxic, low-OM-load) cells end up on the oxic rates and muddy (anoxic, high-OM-load)
cells on the anoxic/low-redox rates, all from the **global** `W_SED_CONST`. The two-type
profiles still vary **geometry (depths, porosities, densities, burial, mixing) + the 24×layer
IC block** (IC as spin-up seed) — which the model already indexes per box.

**Central identifiability risk — "similar deposition, distinct measured flux."** Because
deposition is endogenous and *shared* wherever adjacent boxes see the same overlying water, in
a well-mixed shallow lagoon two adjacent boxes can receive near-identical OM deposition yet
have genuinely different measured fluxes (a grain-size/permeability contrast). With deposition
shared and porosity opposing, the thin lever set (burial+mixing+geometry) may be unable to
separate them. This is **not an edge case** — 2b must **pre-assess how many box pairs share
overlying water** and treat that count as the primary identifiability risk. **Make the fallback
decidable** (the same standard §4.2 applies to conflict declaration): invoke the §6 per-box-rate
fallback when **more than half of the flux-calibration-target boxes share overlying water with a
box assigned the *other* sediment type** — a provisional threshold finalized against the box→type
map and the mixing geometry in 2b, not a subjective "if large".

**Kinetics scope (consistent with §4.1 and §6).** The two-type *profiles carry no kinetics* —
rate constants stay **global, never per-box**. But "global" is not "frozen": 2b calibrates the
*global* oxic/anoxic dissolution constants (`K_OXIC_DISS_*`/`K_ANOXIC_DISS_*`) **and** the
*global* mineralization redox constants (`SED_K_MIN_DON_*`/`SED_K_MIN_DOP_*`), which together
set the N/P fluxes — all inside the single shared `W_SED_CONST`. Only DSi is set by the
dissolution rates alone. Per-box rate constants stay out of scope (§6).

**Regime-separation precondition (identifiability) — two different variables govern two
different fluxes; check both.** `K_OXIC`/`K_ANOXIC` and the mineralization redox constants are
*global* — two knobs per solute serving *both* types. Clean "sandy flux ← oxic rate, muddy flux
← anoxic rate" separation holds **only** if each type sits almost entirely in one regime, but
"one regime" is defined by a **different variable for dissolution than for mineralization**:

- **Dissolution mask + DSi** is switched by the *binary* `where (SED_DOXY ≥/< DOXY_AT_ANOXIA)`
  test (`:2455-2475`). Precondition: sandy cells predominantly **above** and muddy cells
  predominantly **below** `DOXY_AT_ANOXIA`.
- **N/P mineralization** (the direct NH4/PO4 source) is *not* switched by `DOXY_AT_ANOXIA`. It
  is governed by the **graded** limitation terms `LIM_DOXY_RED = O2/(O2+K_HS_DOXY_RED_LIM)` and
  `LIM_NO3N_RED` (with its O2-inhibition factor), whose oxic→NO3 crossover is set by
  `SED_K_HS_DOXY_RED_LIM`/`SED_K_HS_DOXY_RED_INHB`/`SED_K_HS_NO3N_RED_LIM` (`:2265-2268`), not
  by `DOXY_AT_ANOXIA`. Separate precondition: mineralization must be **DOXY-term-dominated in
  sandy** cells and **NO3N-term-dominated in muddy** cells (separation checked on the LIM terms
  and their governing `K_HS` constants).

A solution can pass the `DOXY_AT_ANOXIA` dissolution check while N/P are *not* cleanly
separated on the LIM terms — silently breaking the two-knob inversion for N/P. If either check
fails, the same global rate drives both types, the two-knob analytical inversion (§4.1) breaks,
and per-type rates (the §6 fallback) become necessary.

### 3.1 Fortran reader extension (`mod_BOTTOM_SEDIMENTS.f90`, `READ_BOTTOM_SEDIMENTS_MODEL_INPUTS`)

Declare a new module integer `NUM_SED_TYPES` in `mod_GLOBAL.f90` (next to `NUM_SED_LAYERS`,
`mod_GLOBAL.f90:74`), **and** a module allocatable `integer, allocatable :: SED_TYPE_PER_BOX(:)`
in the same module (it is `nkn`-length, so it must be an allocatable, not a scalar/parameter);
`allocate(SED_TYPE_PER_BOX(nkn))` in the reader alongside the other `(nkn,…)` sediment arrays
(`mod_BOTTOM_SEDIMENTS.f90:346`), before the `SED_TYPE_PER_BOX` indices are read.

**Per-type read buffers (declare these explicitly — the reader cannot broadcast into the final
`(nkn,…)` arrays in the 2-type path).** After `NUM_SED_TYPES` is known and before the geometry
read, allocate one buffer per profile field, then deallocate them after the per-box assignment:

```
integer                       :: t, box, layer, var
real(kind=DBL), allocatable   :: TYPE_DEPTHS(:,:)      ! (NUM_SED_TYPES, NUM_SED_LAYERS)
real(kind=DBL), allocatable   :: TYPE_POROSITIES(:,:)  ! (NUM_SED_TYPES, NUM_SED_LAYERS)
real(kind=DBL), allocatable   :: TYPE_DENSITIES(:,:)   ! (NUM_SED_TYPES, NUM_SED_LAYERS)
real(kind=DBL), allocatable   :: TYPE_MIXING(:)        ! (NUM_SED_TYPES)  scalar per type
real(kind=DBL), allocatable   :: TYPE_BURIAL(:)        ! (NUM_SED_TYPES)  scalar per type
real(kind=DBL), allocatable   :: TYPE_IC(:,:,:)        ! (NUM_SED_TYPES, NUM_SED_VARS, NUM_SED_LAYERS)
```

`TYPE_IC`'s index order **deliberately mirrors the legacy `BSED_ARRAY(NUM_SED_VARS,
NUM_SED_LAYERS)`** (var-major, layer-minor) so the per-type IC lines are read the same way the
legacy reader reads them — this is load-bearing for the transpose in "Assign per box" below.
These exact shapes are also the pure subroutine's `intent(in)` dummies (§5). The existing
reader is **purely positional** — every header is consumed
by an untargeted `read(IN_FILE,*)` counted by position; it never inspects content and there is
no keyword-scan anywhere in the sediment path. So the new block cannot be added with a blind
positional read (that would swallow the next real line — e.g. `# ADVANCED REDOX SIMULATION` or
the depths header — and desync the whole file). Use an explicit **peek + backspace** detection.

**Detection contract & exact slot.** Immediately after the `NUM_SED_LAYERS` value is read
(`:338`) and before the geometry section, read one record into a character buffer and test it:

```
read(IN_FILE,'(A)') buf
if (index(buf,'NUM_SED_TYPES') > 0) then
    read(IN_FILE,*) NUM_SED_TYPES          ! next record holds the count
else
    backspace(IN_FILE)                     ! legacy file: no header — put the line back
    NUM_SED_TYPES = 1
end if
```

A legacy file (the shipped `INPUTS/BOTTOM_SEDIMENT_MODEL_INPUT.txt` and `INPUT_sediment_test`,
which have no such header) MUST take the `backspace` path, set `NUM_SED_TYPES = 1`, and then
parse **byte-for-byte as today**.

**Extended file layout (whenever the `# NUM_SED_TYPES` header is present — normally
`NUM_SED_TYPES > 1`; see the `count == 1` contract below), exact record order:**
1. …legacy preamble unchanged through the `NUM_SED_LAYERS` value…
2. `# NUM_SED_TYPES` header line, then the integer count on the next record.
3. `# SED_TYPE_PER_BOX` header line, then **`nkn` integer indices, one per line** (matching a
   `do i=1,nkn; read(IN_FILE,*) idx` loop). Each `idx` MUST satisfy `1 ≤ idx ≤ NUM_SED_TYPES`,
   else `stop` with a diagnostic naming the box and value (an out-of-range index would read the
   per-type profile arrays out of bounds → silent corruption or crash). Type index 1 =
   **sandy**, 2 = **muddy**.
4. For `t = 1 … NUM_SED_TYPES`, one profile block, each in this fixed field order and
   cardinality. **"Skip records" are consumed by untargeted `read(IN_FILE,*)`; the reader
   skips by *position/count*, not by `#`-prefix content — so the emitter must write exactly the
   stated number of records, and one of them is a *numeric* OPTIONS value, not a comment.**
   - depths: **3 header/skip records** (untargeted reads: options-header comment, numeric
     options-value line e.g. `1`, section-header comment), then **`NUM_SED_LAYERS`** values,
     one per line (per-layer vector).
   - porosities: **3 header/skip records** (options-header, options-value, section-header), then
     `NUM_SED_LAYERS` values.
   - densities: **3 header/skip records** (options-header, options-value, section-header), then
     `NUM_SED_LAYERS` values.
   - particle-mixing: **3 header/skip records** (options-header, options-value, section-header),
     then **1 scalar** (broadcast to all layers/vars for the type).
   - burial: **3 header/skip records** (options-header, options-value, section-header), then
     **1 scalar** (broadcast to all layers for the type).
   - initial conditions: **4 header/skip records** (options-header, options-value,
     section-header, the `# Layer 1 … Layer 7` column-header), then **`NUM_SED_VARS` (= 24)**
     lines, each holding `NUM_SED_LAYERS` values (the 24×layer IC block).
5. `ADVECTIVE_VELOCITY`: **3 header/skip records** (options-header, options-value,
   section-header) + **1 scalar**, read **once, global** — kept *outside* the per-type blocks
   (a single global scalar; no per-type value to disambiguate).
6. `SURF_MIXLEN`: **3 header/skip records** + **1 scalar**, read **once, global**, outside the
   blocks.
7. Model-constants section (filename → `W_SED_CONST`) — **once**, after all profile blocks.
8. Output-organization section — **once**, after the constants section.

Profile-block order equals type-index order: block #1 = type 1 = **sandy**, block #2 = type 2
= **muddy**.

**The `>1`-type branch is a *distinct parse path*, not a loop over the legacy sequence.** In the
legacy single-profile order, `ADVECTIVE_VELOCITY` sits *between densities and particle-mixing*
(`:403-407`) and `SURF_MIXLEN` sits *between burial and IC* (`:423-427`). The extended layout
**relocates both to the global tail** (items 5–6) and each per-type block therefore reads
**only** {depths, porosities, densities, particle-mixing, burial, IC} — it must **not** attempt
the interleaved adv-vel/surf-mixlen reads, or it desyncs. Implement the multi-type branch as a
separate record sequence; do not reuse the legacy block loop.

**Contract for a `# NUM_SED_TYPES` header with count `== 1` (make the parse deterministic).**
Detection has already consumed the header + count, so the reader cannot fall back to the legacy
interleaved order. Define it unambiguously: **if the header is present, use the new-order path
for any `count ≥ 1`** — read `SED_TYPE_PER_BOX` (all indices then `= 1`), **one** new-order
profile block, and the global adv-vel/surf-mixlen tail. `count < 1` ⇒ `stop` with a diagnostic.
The legacy interleaved order is reached **only** via the header-absent `backspace` path. (The
converter never emits `count == 1` with a header, but the reader now has a defined contract.)

**Assign per box** (after all blocks are read): for each box, using its
`SED_TYPE_PER_BOX(box)` index `t`, copy from the per-type buffers. Geometry maps
axis-for-axis:

```
SED_DEPTHS(box,:)     = TYPE_DEPTHS(t,:)         ! (NUM_SED_LAYERS)
SED_POROSITIES(box,:) = TYPE_POROSITIES(t,:)
SED_DENSITIES(box,:)  = TYPE_DENSITIES(t,:)
SED_BURRIALS(box,:)   = TYPE_BURIAL(t)           ! scalar broadcast over layers
PART_MIXING_COEFFS(box,:,:) = TYPE_MIXING(t)     ! scalar broadcast over layers,vars
```

**The IC assignment is a transpose — do NOT write `INIT_SED_STATE_VARS(box,:,:) =
TYPE_IC(t,:,:)`.** The target `INIT_SED_STATE_VARS` is `(nkn, NUM_SED_LAYERS, NUM_SED_VARS)` =
`(nkn, 7, 24)` (layer-major, var-minor), while the per-type IC buffer `TYPE_IC(t,var,layer)`
is var-major, layer-minor (mirroring legacy `BSED_ARRAY(NUM_SED_VARS, NUM_SED_LAYERS)`). A
direct `(:,:)` copy is a nonconforming `(24,7)→(7,24)` assignment (compile/runtime error, or
silent axis-swap corruption if forced). Reproduce the **exact legacy transpose**
(`mod_BOTTOM_SEDIMENTS.f90:439-446`) with an explicit loop:

```
do box = 1, nkn
    t = SED_TYPE_PER_BOX(box)
    do layer = 1, NUM_SED_LAYERS
        do var = 1, NUM_SED_VARS
            INIT_SED_STATE_VARS(box, layer, var) = TYPE_IC(t, var, layer)
        end do
    end do
end do
```

These arrays are already `(nkn, …)`-dimensioned and used per box, so no kinetics/transport
change is required. (`ADVECTIVE_VELOCITY` and `SURF_MIXLEN` remain global scalars in both
formats.) Deallocate the `TYPE_*` buffers after this loop.

- **Backward compatible:** a file with no `# NUM_SED_TYPES` header (⇒ `NUM_SED_TYPES = 1`, via
  the `backspace` path) loads exactly as today — one profile broadcast to all boxes, legacy
  `ADVECTIVE_VELOCITY`/particle-mixing/burial/`SURF_MIXLEN` positions unchanged — so the 25-box
  example and `INPUT_sediment_test` are byte-for-byte unaffected.

### 3.2 Per-box flux-output fix (`mod_SIMULATE.f90:719`)
`write(... FLUXES_OUTPUT_TO_WATER_COLUMN(nkn,:))` → `(i,:)`. The current code writes box
`nkn`'s fluxes for every box `i`, so per-box benthic fluxes cannot be read. **Only the box
index changes** (`nkn`→`i`); the row layout is untouched — same 36 value columns, same
`'(F10.4,I10,36F20.10)'` format. That width `36 = nstate + NUM_ALLOLOPATHY_STATE_VARS`
(32 + 4 for CL29) is load-bearing and must stay in sync with those params; do **not**
generalize the fix by touching it. Required for the 2b modeled-vs-measured comparison.

**Two distinct index spaces (do not conflate):**
- *Internal* sediment→ALUKAS **source** mapping (`aquabc_II_sediment_auxillary.f90:378-415`,
  `FLX_SED_MOD_1_TO_ALUKAS_II_VEC`): NH4 = `FROM_SEDIMENT(1)`, NO3 = `(2)`, PO4 = `(5)`,
  DSi = `(11)`, PART_Si = `(12)`. These are the *source* indices, **not** file columns.
- *On-disk* columns of `SEDIMENT_FLUX_OUTPUTS.out` (`BOTTOM_SEDIMENT_FLUXES_FILENAME`,
  unit 1023). The array written is `FLUXES_OUTPUT_TO_WATER_COLUMN`, which equals
  `FLUXES_TO_WATER_COLUMN` — the **ALUKAS / pelagic-state-var-ordered** output of
  `FLX_SED_MOD_1_TO_ALUKAS_II_VEC` (`mod_SOLVER.f90:1519-1521,1553`). In that ordering, and
  **after the leading `WTIME, box` prefix**, the 2b harness must read: **NH4 = value col 1,
  NO3 = col 2, PO4 = col 3, DISS_Si = col 17, PART_Si = col 18**. Note `FROM_SEDIMENT` source
  indices 5 and 11 map to ALUKAS columns 5 and 11, which the mapper sets to **zero**
  (`aquabc_II_sediment_auxillary.f90:396` etc.) — so reading columns 5/11 for PO4/DSi would
  yield zeros and silently break the 2b comparison.

**Companion fix in the same output region.** `mod_SIMULATE.f90:728` writes the COCOA
`N_OUT_FROM_SED` as `FLUXES_FROM_SEDIMENTS(i,1) + FLUXES_FROM_SEDIMENTS(i,1)` — it doubles
NH4 (index 1) and omits NO3 (index 2). Correct it to
`FLUXES_FROM_SEDIMENTS(i,1) + FLUXES_FROM_SEDIMENTS(i,2)` while touching this block, so any
N-out comparison drawn from unit 2031 in 2b is not corrupted.

### 3.3 Converter support (`tools/eutropy_poc/eutropy_to_estas.py`)
- `CL29_SEDIMENT_TYPE = {box: 'sandy'|'muddy'}` (user supplies the 29-box map). The converter
  maps the strings to the reader's integers **`'sandy' → 1`, `'muddy' → 2`**, and a box
  **absent from the map ⇒ default type 1 (sandy)**. Box 19 must be entered explicitly and, per
  §1, is expected to be `'muddy'` (2).
- Two profile dicts `CL29_SED_SANDY` / `CL29_SED_MUDDY`, each `{depths, porosities, densities,
  burial, mixing, ic_overrides}` — **all six fields must have a seed**. Phase 1 only defines
  `CL29_SED_DEPTHS` and `CL29_SED_BURIAL` (via `_override_sed_geometry`, which patches **only**
  `SED_DEPTHS`/`SED_BURRIALS`); porosity, density, and mixing currently come **verbatim from the
  template**, so there is *no* Phase-1 constant to seed them from. Introduce three new baseline
  constants, extracted from the template's current values, so both dicts have a defined seed:
  - `CL29_SED_POROSITIES = [0.40, 0.40, 0.40, 0.40, 0.30, 0.25, 0.25]` (template `SED_POROSITIES`)
  - `CL29_SED_DENSITIES  = [1.75, 1.75, 1.75, 1.75, 1.75, 1.75, 1.75]` (template `SED_DENSITIES`)
  - `CL29_SED_MIXING     = 0.0000264` (template `PART_MIXING_COEFFS` surface Db0)

  Seed **both** dicts from `{CL29_SED_DEPTHS, CL29_SED_POROSITIES, CL29_SED_DENSITIES,
  CL29_SED_BURIAL, CL29_SED_MIXING}` (identical placeholders until 2b differentiates them).
- **What is actually "global/shared" vs per-type — do not invent a global geometry record.** In
  the 2-type file format **depths and burial are per-type records** (every type carries its own
  depths vector and burial scalar; there is *no* global depths/burial slot). `CL29_SED_DEPTHS`
  and `CL29_SED_BURIAL` are merely the **seed values copied into both dicts**, not written once
  globally. Reserve "global/shared" strictly for the genuinely single records:
  `W_SED_CONST`/`CL29_SED_CONST_OVERRIDE`, `ADVECTIVE_VELOCITY`, and `SURF_MIXLEN` (`W_SED_CONST`
  stays single).
- **Signature + branch.** Thread the type map into the call site:
  `_write_sediment_inputs(OUT, CL29_ENABLE_SEDIMENTS, CL29_SEDIMENT_TYPE)` (Phase 1 signs it
  `(out, enable_sediments)` with no map). Then split cleanly on the map:
  - **`CL29_SEDIMENT_TYPE` empty ⇒ the *unmodified* Phase-1 template-patch path runs verbatim**
    (`shutil.copy` the template, force advanced-redox→0, `_override_sed_geometry`, optional
    `_override_sed_carbonate`). This is what guarantees the §5 **byte-identical** acceptance
    test: the single-profile case must NOT be routed through the new from-scratch author.
  - **`CL29_SEDIMENT_TYPE` non-empty ⇒ the new multi-type author** emits the extended file.
- The multi-type author writes the **exact §3.1 record order**: the `# NUM_SED_TYPES` header +
  count; the `# SED_TYPE_PER_BOX` header + **one integer per box, one per line, for all `nkn`
  boxes**; then the two per-type profile blocks (sandy first, muddy second), each carrying only
  {depths(L), porosities(L), densities(L), mixing(1 scalar), burial(1 scalar), IC(24×L)}; then
  the single **global** `ADVECTIVE_VELOCITY` and `SURF_MIXLEN` scalars; then the constants +
  output sections once. The emitter's field cardinalities MUST match the reader's (vectors of
  length `NUM_SED_LAYERS` vs single scalars) and each block's **3/4 pre-data skip records
  (including the numeric OPTIONS-value line)** or the positional read desyncs (§3.1).
- **Each type's IC block is FULL (24×`NUM_SED_LAYERS`), composed base + overrides.** The dicts
  carry only *partial* `ic_overrides`, but the reader demands a full 24×L block per type.
  Compose it explicitly: **start from the template's 24×L `INIT_SED_STATE_VARS` base** (read
  from `BOTTOM_SEDIMENT_MODEL_INPUT.txt`, the same source Phase 1 patches), then **apply that
  type's `ic_overrides`**, then — if `CL29_SED_CARBONATE_IC is not None` — **apply the
  carbonate-IC override (INORG_C=var 13, TOT_ALK=var 14) to that base**, exactly as
  `_override_sed_carbonate` does today. **The carbonate override applies to *both* types** (it
  reflects a pore-water DIC/alkalinity floor the codebase's CO2SYS needs, not a grain-size
  property); state this explicitly so enabling two types does not silently drop it.

**Data flow:** converter (2 profiles + box→type map) → extended reader assigns per-box
sediment → run (serial) → per-box flux output → (2b) compare to measured → refine.

## 4. Phase 2b calibration + validation (scoped; own cycle when data lands)

**Method — analytical-first + few runs (user decision):**
1. **Analytical derivation (seeds an annual-mean magnitude only).** At quasi-steady state the
   benthic dissolved flux ≈ reactive deposited particulate − burial, modulated by
   dissolution/mineralization rates and the **solid concentration** — which the model computes
   per cell as `SED_DENSITIES − WATER_DENSITY·SED_POROSITIES`
   (`aquabc_II_sediment_model_1_fast.f90:736-738`) and which sets the sorbed/dissolved split
   through the **global** partition coefficients (`SOLID_PART_COEFF_NH4/PO4` are single global
   scalars; only the *geometry* — density/porosity — varies per type). This steady-state
   relation yields an **annual-mean magnitude seed**, not the seasonal signal: the
   spring/summer structure comes from the transient runs (step 2) via the `THETA^(T-20)`
   temperature factors and the buildup/drawdown of labile pools deposited during the bloom.
   **Do not expect seasonal fluxes to fall out of the steady-state derivation.** Derive the
   geometry and IC (endogenous OM loading + spin-up IC pools set the oxic/anoxic split; porosity
   is only the secondary, oppositely-signed diffusion term of §3) together with the **global**
   dissolution rates (`K_OXIC_DISS_*`/`K_ANOXIC_DISS_*`) **and** the **global** mineralization
   redox rates (`SED_K_MIN_DON_*`/`SED_K_MIN_DOP_*`). But treat the three solutes by their
   *actual coupling*, not as three independent per-type × per-solute inversions:
   - **Phosphorus (P):** DOP mineralization (`SED_K_MIN_DOP_*`) is the direct PO4 source; a
     two-knob (oxic/anoxic-graded) inversion per type is defensible **subject to the mechanism
     pre-check of §4.1a** (under CL29 advanced-redox=0 the anoxic P pathway is largely absent).
   - **Nitrogen (N) is ONE coupled subsystem, not two independent solutes.** NH4 and NO3 net
     benthic fluxes are linked by nitrification (global `K_NITR`, O2-gated, `:2481`) and
     denitrification (`R_DENITR = 0.93·R_MINER_DOC_NO3N`, `:2512`), both set by the *same*
     per-cell redox state. You **cannot** independently invert an "NH4 rate" and an "NO3 rate"
     per type. Invert the *pipeline* — DON mineralization → NH4 → nitrification → NO3 →
     denitrification — whose free global knobs are the **DON mineralization rates, `K_NITR`, and
     the redox distribution set by geometry**; the observed NH4 efflux and NO3 sign/magnitude are
     **joint outputs** of that pipeline, co-varying with the redox distribution and `K_NITR`.
   - **Silicon (DSi) is set by the dissolution rates alone**, but note (per §4 guardrails) that
     the `K_OXIC_DISS_PSi`/`K_ANOXIC_DISS_PSi` split is an **effective-calibration knob, not
     physiology**: biogenic-Si dissolution is physically redox-*independent* (driven by
     undersaturation, temperature, reactive surface area). The sandy/muddy DSi contrast should be
     driven by biogenic-Si *deposition + temperature*, not by that unphysical redox split.
   This yields initial sandy and muddy profiles without a run. The two-knob-per-solute inversion
   (P and Si) is valid **only under clean regime separation** (§3, split into the dissolution and
   the N/P LIM-term checks): if sandy is not predominantly oxic and muddy not predominantly
   anoxic, fall back to §6.
   **Deposition is *not* a fixed exogenous known.** It is endogenous and coupled to the very
   fluxes being calibrated — the particulate settling flux `FLUXES_TO_SEDIMENTS` feeds
   `SETTLING_DERIVS` (`aquabc_II_sediment_model_1_fast.f90:1954-1956`), so benthic N/P return
   drives the bloom, which resets the settling flux onto the sediment. (The dissolved
   `DEPOSIT_CONC = sed_porosities·SURF_WATER_CONCS` branch at `:1975` is **dead code** — its
   guard `in_which_phase==0 .and. ==2` can never hold — so it is *not* the deposition mechanism.)
   A seed built from a Phase-1 (placeholder-sediment) deposition can therefore be far off once
   the calibrated benthic feedback is on. Treat the Phase-1 deposition as a *first guess* and add
   the self-consistency guardrail below.

**§4.1a — Anoxic-remineralization pathway pre-check (resolve this BEFORE building or
calibrating; it can pre-judge the conflict).** The whole gap-closure thesis is *"muddy anoxic →
distinct, large benthic P return closes box 19 to 47 ± 14."* Under the **CL29 configuration that
forces sediment advanced-redox = 0** (`eutropy_to_estas.py:592`), that mechanism for **N and P
is largely absent**, and for P is arguably *inverted*:
- With advanced-redox off, DON/DOP → NH4/PO4 mineralization retains **only** the `DOXY` and
  `NO3N` pathways; the Mn/Fe/SO4/methanogenesis DON/DOP mineralization terms are
  **default-zeroed and overwritten with real rates only inside the `DO_ADVANCED_REDOX_SIMULATION
  > 0` block** (`:2309-2312`/`:2395-2398`, gated at `:2314`/`:2400`) — so with advanced redox off
  they stay identically zero. (The analogous DOC redox terms are *not* re-zeroed in the else
  branch, a latent asymmetry — but DOC feeds C/O2/alkalinity, not the P budget, so it does not
  affect this argument.) Both surviving `LIM` terms → 0 as O2 **and** NO3 vanish, so a
  deeply anoxic muddy cell **suppresses** NH4/PO4 remineralization (no sub-NO3 pathway) and
  **buries more OM**.
- Reductive Fe-oxide P release is **disabled**: `K_SP_FEPO4` is commented out (`:1093`) and the
  PO4 solid/dissolved split uses a **global, redox-INDEPENDENT** `SOLID_PART_COEFF_PO4`
  (`:1507`); PO4 kinetics are DOP-mineralization-only.
- **Consequence:** a muddy, deeply anoxic box 19 is more likely a **net P sink** than a large P
  source — the *opposite* of the thesis — which would **widen** the bloom gap. The clean
  "anoxic rate → distinct flux" story survives only for the **dissolution** step (hence DSi),
  not for the NH4/PO4 that gap-closure actually needs.

**Required resolution before proceeding** — pick one and record it:
  (a) **Enable advanced sediment redox** (sulfate reduction + Fe-P) so deep anoxia recycles N/P
      and a reductive Fe-P release exists — **but** this *contradicts the deliberate CL29
      advanced-redox = 0 choice* and must be explicitly reconciled (it changes the CL29 config,
      not just a calibration knob); or
  (b) **Accept that in this config benthic N/P return is confined to the oxic + sub-oxic NO3
      surface zone**, and **re-derive analytically** whether that thin zone alone can supply
      box 19's ~0.02 mg P/L spring deficit. If it cannot, this is a **§6 model-mechanism gap**,
      *not* a calibration problem, and "both-must-hold" should be **pre-judged infeasible** and
      surfaced as a finding (per the §1 conflict protocol) rather than chased through runs.
**Guardrail (mandatory regardless of choice):** enabling muddy box-19 sediment must **not
REDUCE** water-column spring DIP versus a `MODEL_SEDIMENTS = 0` baseline run. If it does, the
sediment is acting as a P sink and the gap cannot close — stop and surface it.

2. **Verify + refine (convergence-based, not a fixed run count).** Run, extract per-box benthic
   N/P/Si fluxes (fixed output), compare to measured by type and season, adjust, re-run. Iterate
   the deposition (feed each run's realized deposition back into step 1) rather than fixing it
   from a single Phase-1 run. **This is a fixed-point iteration on a *positive* feedback loop**
   (benthic P/N return → bloom → settling → OM load → redox → return), which can oscillate,
   go bistable, or land on a grid-dependent state — do **not** assume it converges. Impose:
   - **Damped / under-relaxed update:** feed back `dep_{k+1} = (1−α)·dep_k + α·realized_k` with
     `α ≤ 0.5`, not the raw realized deposition, to suppress oscillation.
   - **Stopping rule (convergence):** stop when the **deposition self-consistency guardrail
     holds** (realized vs assumed deposition per box within the stated X % of §4.2) **and** the
     per-box flux residual is **monotonically non-increasing** across the last two iterations.
   - **Divergence detection + max-iteration cap:** if the residual grows for two consecutive
     iterations, or the cap (**8 iterations**) is reached without meeting the stopping rule,
     **do not keep grinding** — this itself triggers the **§1 conflict → finding** path (the
     coupled system has no stable both-must-hold fixed point under the bounded search).
   The typical expectation is still a handful of runs (≈ 2–4) when the feedback is mild; the cap
   and divergence check exist for when it is not.

**Validation (both-must-hold):**
- *Fidelity:* modeled benthic NH4/NO3/PO4/DSi fluxes match measured sandy/muddy seasonal
  values within the **tolerance semantics of §1** — the stated basis (field variability vs
  instrument SD) per target; ±1 SD (working stand-in ±30 %) for well-resolved one-signed fluxes;
  and for **near-zero / sign-changing fluxes (the net-NO3 case) an absolute floor + sign
  criterion**, not a percentage of a near-zero magnitude — **including flux sign** where the
  measurement resolves it.
- *Gap-closure:* box-19 spring diatom Chl-a → 47 ± 14; summer cyano within 96 ± 56;
  spring:summer ratio ≈ 0.5; diatom→OPA→cyano succession (order + timing) intact; **and box
  19's *own* modeled benthic PO4 flux (not the type mean) supplies the P this requires** (§1).
- *Water-column guardrails (from `docs/CL29_Parameter_Validation.md`):* water-column DIP
  realism; summer DIN:DIP (N-fixer artifact); DISS_Si floor ≫ KHS_DSi; no interannual PO4 /
  SED_PSi drift; sediment pools not clamp-pinned; all 5 years reported (not just the mean);
  no NaN / CO2SYS non-convergence.
- *Sediment/redox guardrails (new this phase):*
  - **Per-type O2 penetration realism** — the calibrated solution must reach the fluxes with
    *physical* redox: sandy shows an oxic surface layer (O2 penetrates ~cm), muddy shows
    near-surface anoxia. Matching fluxes with unphysical redox (e.g. relying on porosity to
    oxygenate mud, which the tortuosity term of §3 works *against*) is a fail — it signals the
    model's missing permeability/advection mechanism and points to the §6 fallback. This is the
    case to watch when two boxes have *similar* OM deposition but genuinely different measured
    fluxes: geometry + global rates cannot reproduce that grain-size/permeability contrast.
  - **Clean regime separation — check BOTH variables (§3).** (i) *Dissolution/DSi:* sandy cells
    predominantly above, muddy cells predominantly below `DOXY_AT_ANOXIA` (the binary mask,
    `:2455-2475`). (ii) *N/P mineralization:* separately verify the graded `LIM` terms —
    mineralization **DOXY-term-dominated in sandy**, **NO3N-term-dominated in muddy** (crossover
    governed by `SED_K_HS_DOXY_RED_LIM`/`_INHB`/`SED_K_HS_NO3N_RED_LIM`, `:2265-2268`), **not**
    by `DOXY_AT_ANOXIA`. Passing (i) alone can hide N/P non-separation and silently break the
    two-knob N/P inversion.
  - **NO3 flux sign/denitrification consistency** — muddy anoxic sediment typically *consumes*
    NO3 (denitrification, `R_DENITR = 0.93·R_MINER_DOC_NO3N`,
    `aquabc_II_sediment_model_1_fast.f90:2512`) → flux *into* the sediment, while sandy
    nitrifying sediment *releases* NO3; match the sign, not only the magnitude. Because the net
    NO3 flux is a small residual of large gross nitrification/denitrification, apply the §1
    absolute-floor + sign rule, not a percentage band.
  - **DSi contrast is deposition/temperature-driven, not redox-driven** — the sandy/muddy DSi
    difference must come from biogenic-Si *deposition + temperature*, **not** from the
    `K_OXIC_DISS_PSi`/`K_ANOXIC_DISS_PSi` split (which is an effective-calibration knob, since
    biogenic-Si dissolution is physically redox-independent). Note explicitly: with the lagoon
    Si-replete (`DISS_Si ≫ KHS_DSi`), **DSi fidelity does not constrain the box-19 bloom
    target** — do not let a good DSi match mask a P-supply failure.
  - **IC pools are a spin-up check, not a persistent lever (§3).** Verify muddy IC PON/POP/POC
    seeded higher than sandy (the intended OM-loading contrast), **and** that the reported
    fluxes are insensitive to the IC seed after the stated spin-up (discard year 1): if flipping
    the IC ordering changes the post-spin-up fluxes, the "separation" is a spin-up artifact, not
    a mechanism.
  - **Benthic budget closure (mass balance) per box** — at quasi-steady state, realized
    dissolved efflux ≈ reactive deposition − burial per box. Check this explicitly: recent
    commits **floor concentrations to non-negative**, which can silently manufacture flux / mask
    an imbalance, so a budget that does not close flags a clamp artifact rather than a real flux.
  - **Sediment O2 uptake (SOD) realism** — O2 is the master redox variable and SOD is often the
    best-constrained benthic measurement; the modeled areal SOD must fall in a plausible range
    for a hypertrophic lagoon. A flux match reached with implausible SOD is a fail.
  - **Seasonal flux phase alignment** — only spring/summer *means* are constrained, so a run can
    match the mean with the wrong within-season timing. Check the modeled flux *phase* (rise/peak
    timing) against the measured seasonal pattern, not just the seasonal mean.
  - **Deposition self-consistency** — post-calibration realized deposition per box within
    **X % = 25 %** of the value assumed in the step-1 derivation (feedback of §4.1/§4.2);
    otherwise iterate (damped, per step 2).
- *Conflict → finding:* declare a conflict only per the bounded-search stopping protocol of §1
  (analytical seed + the **§4.2 physically-bounded search** + the convergence-based verification
  loop of step 2 with its damping and max-iteration cap, still missing both). Also honor the
  **§4.1a pre-check**: if the mechanism analysis already shows benthic N/P return cannot supply
  box 19 under CL29 advanced-redox = 0, this is a §6 model-mechanism gap declared *before* the
  search. Test the concrete hypothesis of §1: does the measured muddy PO4 flux at box 19 supply
  enough P for 47 ± 14 with realistic, drift-free DIP? If both truly cannot be met, stop and
  surface it.

### 4.2 Bounds table + calibrated-constant ranges (makes "search exhausted ⇒ conflict"
decidable)

Without enumerated bounds the conflict clause has no stopping condition. All values are
anchored to in-repo sources (the shipped template `INPUTS/BOTTOM_SEDIMENT_MODEL_INPUT.txt` and
`INPUTS/W_SED_CONST.txt`, plus the Phase-1 stability seeds) and physical limits; 2b may narrow
them from measured data but may **not** search outside them without recording a bounds change.

**Geometry / IC (per-type profile) physical bounds:**

| Field | Min | Max | Basis |
|---|---|---|---|
| layer depth (each) | 0.001 m | 0.20 m | template layers 0.005–0.10 m; positive, thin surface |
| porosity (each) | 0.20 | 0.95 | template 0.25–0.40; physical (0,1), muddy up to ~0.9 |
| density (each) | 1.10 g/cm³ | 2.65 g/cm³ | template 1.75; organic mud → quartz-sand grain density |
| burial | 1×10⁻⁵ m/day | 1×10⁻² m/day | template/Phase-1 2.74×10⁻⁴; ±~1.5 decades |
| particle-mixing Db0 | 0 | 1×10⁻³ m²/day | template 2.64×10⁻⁵; 0 (no fauna) → bioturbated mud |
| IC organic pools (PON/POP/POC/PSi) | 0.1× | 10× | multiplier band around the Phase-1 seed (spin-up seed only — §3 and the IC-pools guardrail in §4) |

> **Porosity caveat (§3 coupling).** In this diffusion-only model higher porosity gives *deeper*
> O2 penetration (tortuosity `1/(1+3(1-φ))`), so pushing muddy porosity toward the 0.95 bound
> makes mud *more* oxic — the opposite of intent. The upper bound is a physical limit, not a
> target; a high-φ muddy solution must still pass the §4 per-type O2-penetration guardrail, which
> is what actually keeps the search from wandering into that counterproductive region.

**Global calibrated rate constants (single `W_SED_CONST`) — documented search ranges:**

| Constant | Range | Basis |
|---|---|---|
| `K_OXIC_DISS_PSi` | 0.01–1.0 /day | Phase-1 seed 0.1 (20× template); ±1 decade |
| `K_ANOXIC_DISS_PSi` | 0.002–0.2 /day | Phase-1 seed 0.02; ±1 decade |
| `K_OXIC_DISS_PON`/`_POP` | 0.1×–10× template | template `W_SED_CONST.txt` value |
| `K_ANOXIC_DISS_PON`/`_POP` | 0.1×–10× template | template `W_SED_CONST.txt` value |
| `SED_K_MIN_DON_DOXY_20`/`_NO3N_20` | 0.1×–10× template | template `W_SED_CONST.txt` value |
| `SED_K_MIN_DOP_DOXY_20`/`_NO3N_20` | 0.1×–10× template | template `W_SED_CONST.txt` value |
| `K_NITR` | 0.1×–10× template | template `W_SED_CONST.txt` value (N-pipeline knob, §4.1) |

(The `0.1×–10×` bands are anchored to the concrete numbers already in `INPUTS/W_SED_CONST.txt`,
so the search space is defined *today* without waiting on external literature; 2b tightens them
against measured rate data when it lands and records any change.)

**Deposition self-consistency threshold:** `X % = 25 %` (used by the step-2 stopping rule and
the §4 guardrail).

## 5. Testing

**Phase 2a (now, data-independent):**
- **Fortran** (`tests/fortran`): `READ_BOTTOM_SEDIMENTS_MODEL_INPUTS` is **not testable in
  isolation** — it depends on `GLOBAL` state (`nkn`, `PELAGIC_INPUT_FOLDER`,
  `PRODUCE_COCOA_OUTPUTS`, the `NUM_SED_*` params), `allocate`s ~20 module arrays (so it can't
  be called twice in one binary without deallocation), and opens a second unit to read a real
  `W_SED_CONST.txt` via `READ_MODEL_CONSTANTS`. The `tests/fortran` suite (`Makefile`
  `TEST_PROGS`) only links pure AQUABC library subroutines (`test_diatoms`, `test_redox`,
  `test_bioturbation`, …) and has no target that builds `mod_BOTTOM_SEDIMENTS` or any ESTAS
  reader. **So extract the type-map + per-box profile assignment into a pure subroutine** with
  this **explicit dummy-argument list** — `intent(in)`: the runtime sizes `nkn` and
  `NUM_SED_LAYERS` (both are module **variables**, not parameters — `nkn` at `mod_GLOBAL.f90:13`,
  `NUM_SED_LAYERS` at `:74` — so they
  must be passed to keep the routine pure and separately testable), `NUM_SED_TYPES`,
  `SED_TYPE_PER_BOX(nkn)`, and the per-type buffers **at the exact §3.1 shapes**
  (`TYPE_DEPTHS/TYPE_POROSITIES/TYPE_DENSITIES(NUM_SED_TYPES,NUM_SED_LAYERS)`,
  `TYPE_MIXING/TYPE_BURIAL(NUM_SED_TYPES)`,
  `TYPE_IC(NUM_SED_TYPES,NUM_SED_VARS,NUM_SED_LAYERS)`); `intent(out)`: per-box
  `SED_DEPTHS/SED_POROSITIES/SED_DENSITIES/SED_BURRIALS/PART_MIXING_COEFFS/INIT_SED_STATE_VARS`
  (with the IC **transpose** of §3.1) — **no file I/O and no global reads**. (The param-sized
  `NUM_SED_VARS = 24` from `mod_GLOBAL` may be inherited by `use` — a compile-time parameter is
  not a "global read".) The reader calls it after
  parsing; the test drives it directly. Add a new `tests/fortran/Makefile` target (e.g.
  `test_sed_typemap`, added to `TEST_PROGS`) that compiles this pure routine + `mod_GLOBAL`,
  and observe the **allocate-once** constraint (the test provides its own output arrays and does
  not re-enter the allocating reader). The test asserts: a 2-type map → each box gets its
  type's geometry/IC; and backward-compat — `NUM_SED_TYPES = 1` → all boxes get profile 1,
  identical to today. Separately, `INPUT_sediment_test` (a full-reader smoke test via the
  normal build, not the unit suite) still runs unchanged.
- **Flux-output fix:** a short 2-type run → assert `SEDIMENT_FLUX_OUTPUTS.out` rows are
  per-box distinct (not all identical to box 29).
- **Converter** (`tests/python`): a box→type map + two profiles produce an extended
  `BOTTOM_SEDIMENT_MODEL_INPUT.txt` with `NUM_SED_TYPES=2`, a correct `SED_TYPE_PER_BOX`
  block, and both profile blocks; an empty `CL29_SEDIMENT_TYPE` reproduces the Phase-1
  single-profile output (byte-identical); off-by-default baseline unchanged.
- **Converter→reader round-trip (guards the positional-desync failure mode — the top
  implementation risk of §3.1).** Emit a 2-type file with the multi-type author, parse it back
  through the extended reader, and assert per-box `SED_DEPTHS`/`SED_POROSITIES`/`SED_DENSITIES`/
  `SED_BURRIALS`/`PART_MIXING_COEFFS`/`INIT_SED_STATE_VARS` equal the profiles that were written
  (each box carrying its mapped type). This is the one test that exercises the emitter's record
  cardinalities against the reader's skip-counts (the 3/3/3/3/3/4 records) and the relocated
  global adv-vel/surf-mixlen tail; the pure-subroutine and byte-identical tests above do not,
  since neither round-trips a 2-type file through the parser.
- **Clean-checkout:** fresh clone builds serial and runs a 2-type short simulation.

**Phase 2b (when data lands):** the modeled-vs-measured flux-comparison harness (per box /
type / season) and the both-must-hold validation, reported across all 5 years.

## 6. Out of scope / future

- **Model-mechanism gap (candidate outcome of the §4.1a pre-check), not a calibration task:**
  enabling the sediment **advanced redox** path (sulfate reduction + reductive **Fe-P** release,
  `K_SP_FEPO4`) so deep muddy anoxia recycles N/P and releases P. This is out of scope here
  because it **contradicts the deliberate CL29 advanced-redox = 0 configuration**; if §4.1a
  shows the oxic + NO3 surface zone cannot supply box 19's P, the correct output is a **finding**
  that the CL29 config lacks the mechanism (pre-judging "both-must-hold" infeasible), not a
  quiet re-enable. Any decision to enable it is a separate config change with its own review.
- Per-box *rate constants* (a kinetics change) — out of scope; kinetics stay **global** (the
  oxic/anoxic split + per-box geometry/IC cover sandy/muddy). This is distinct from 2b
  calibrating the *global* rate constants in `W_SED_CONST` (in scope, §3/§4.1). Revisit per-box
  rates only if 2b proves the global two-knob approach can't separate the regimes (the
  identifiability precondition of §3/§4.1 fails, or the similar-deposition/permeability case of
  §4 — the shared-overlying-water box pairs of §3 — arises).
- More than two sediment types (the reader design generalizes to `NUM_SED_TYPES > 2`, but
  only 2 are authored).
- OpenMP for the sediment path (deadlocks — serial only, per Phase 1).

## 7. References

- `docs/superpowers/specs/2026-07-08-cl29-sediment-diagenesis-phase1-design.md` — Phase 1.
- `docs/CL29_Parameter_Validation.md` — P-supply root cause + the guardrails reused here.
- Source: `mod_BOTTOM_SEDIMENTS.f90:317-486` (sediment reader, single-profile broadcast to
  extend; IC transpose `BSED_ARRAY(var,layer)`→`INIT_SED_STATE_VARS(box,layer,var)` at
  `:439-446`; arrays allocated at `:346`; legacy interleaved `ADVECTIVE_VELOCITY` `:403-407` and
  `SURF_MIXLEN` `:423-427`), `aquabc_II_sediment_model_1_fast.f90:2455-2475` (per-cell
  oxic/anoxic *dissolution* mask — oxic 2455-2464, anoxic 2466-2475),
  `:2265-2354` (redox-sequence **DOC/DON** mineralization; `LIM` terms `:2265-2268`) and
  `:2372-2447` (**DOP** mineralization, `SED_K_MIN_DOP_*` `:2384-2442` — the direct PO4 source),
  `:2481` (`R_NITR`, global `K_NITR`), `:2512` (`R_DENITR = 0.93·R_MINER_DOC_NO3N`),
  `:1093` (`K_SP_FEPO4` commented out), `:1499`/`:1507` (global redox-independent
  `SOLID_PART_COEFF_NH4`/`_PO4`), `:1954-1956` (particulate deposition via
  `FLUXES_TO_SEDIMENTS`/`SETTLING_DERIVS`; `:1975` dissolved branch is dead code),
  `mod_SOLVER.f90:1519-1553` (`FLUXES_OUTPUT_TO_WATER_COLUMN` = ALUKAS-ordered),
  `mod_SIMULATE.f90:716-728` (per-box flux output bug + line-728 N-out bug),
  `aquabc_II_sediment_auxillary.f90:378-415` (benthic flux → pelagic-var mapping),
  `mod_GLOBAL.f90:74` (`NUM_SED_LAYERS`; add `NUM_SED_TYPES` and
  `SED_TYPE_PER_BOX(:)` here), `NUM_SED_VARS = 24` (`mod_GLOBAL.f90:37`),
  `tools/eutropy_poc/eutropy_to_estas.py` (`_write_sediment_inputs` `:573`, forced
  advanced-redox=0 `:592`, `_override_sed_geometry` `:545-570`, `_override_sed_carbonate`
  `:531-542`, `CL29_SED_*`).
