# Decompose `AQUABC_PELAGIC_KINETICS` Mega-Subroutine — Design

**Backlog item:** 1.6 [P2] Mega-Subroutine Decomposition
**Date:** 2026-07-15
**Status:** Design approved — ready for implementation plan

## Goal

Break the single 3,642-line `AQUABC_PELAGIC_KINETICS` subroutine into a thin
orchestrator plus five focused, well-named phase routines, **without changing
numerical behavior** (bit-identical output). Optimize for **navigability and
maintainability at the lowest possible risk** — the transformation is pure code
motion, not a redesign.

## Non-goals

- No numerical/algorithmic changes. Output must be bit-identical (see Verification).
- No independent unit-testing harness for the new phase routines. The
  scientifically-meaningful units (DIATOMS, CYANOBACTERIA, FIX_CYANOBACTERIA,
  NOSTOCALES, ZOOPLANKTON, ORGANIC_CARBON_MINERALIZATION, REDOX_AND_SPECIATION,
  CO2SYS) are *already* separate, testable library subroutines. The inline code
  being extracted is orchestration/derivative-assembly glue; the win here is
  structure, not new test surface.
- No change to the OpenMP parallelization strategy, thread count, or scheduling.
  The balanced chunk split + thread cap from TODO 4.4 stay exactly as they are.
- No sub-splitting of the derivative-assembly phase in this effort (kept whole;
  may be revisited later).

## Global Constraints

- **Bit-identical output** verified after *every* phase extraction (0D golden
  rtol 1e-9 **and** a full-model 25-box serial+8-thread before/after diff).
- **No behavior change** to the four run-time option flags (`ZOOP_OPTION_1`,
  `ADVANCED_REDOX_OPTION`, `LIGHT_EXTINCTION_OPTION`, `CYANO_BOUYANT_STATE_
  SIMULATION`, `CONSIDER_NON_OBLIGATORY_FIXERS`, `CONSIDER_NOSTOCALES`) —
  extracted code carries its conditionals verbatim.
- **Serial fallback preserved** — every OpenMP directive keeps its `!$` sentinel
  form so a non-`-fopenmp` build compiles and runs serially.
- **Compiler-checked interfaces** — phase routines are internal `contains`
  procedures, which have explicit interfaces automatically, so the compiler
  validates every argument at the call site (the safety net for a mechanical
  extraction). No new external (implicit-interface) subroutines.

## Current structure (what we are cutting)

`AQUABC_PELAGIC_KINETICS` lives in
`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` (lines 50–3692), an
**external** subroutine (not in a module) with 29 arguments. It is called from
two sites — `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` and
`SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90` — via implicit
interface; **those call sites do not change.**

Key finding that makes clean extraction safe: inside the main `!$omp parallel`
region, the large `(nkn)`-dimension working arrays are **shared**, and each
thread writes only its own `[ns:ne]` node slice. Only scalars, loop indices, and
the four derived-type bundles (`ENV_CHUNK`, `REDOX_STATE_CHUNK`,
`REDOX_LIM_CHUNK`, `DOCMIN_CHUNK`) are `private`. So an internal phase procedure
can access the shared arrays **by host association** (each thread still touches
disjoint slices — no race, zero reference churn), while the private
scalars/bundles are passed as arguments (unambiguously private per call) or
become the procedure's own locals. See Interface strategy.

Phase map (source-line ranges are current anchors; exact boundaries pinned in the
implementation plan):

| Phase | Lines | Content | OpenMP |
|---|---|---|---|
| CO2SYS preprocess | 398–451 | CO2SYS carbonate-chemistry call over node chunks | self-contained `!$omp parallel` region |
| Speciation preprocess | 452–860 | REDOX_AND_SPECIATION, H2S species, dissolved/particulate fractions of Fe²⁺/Fe³⁺/Mn²⁺, settling prep, saved-output guards | sequential (no OpenMP) |
| Biology | 933–1360 | total phytoplankton + chlorophyll; DIATOMS, CYANOBACTERIA_BOUYANT, FIX_CYANOBACTERIA_BOUYANT, NOSTOCALES, ZOOPLANKTON calls; N/P dissolution | inside main region |
| Chemistry | 1412–1878 | ORGANIC_CARBON_MINERALIZATION; DIN/DIP-scarcity mineralization acceleration; nitrification; denitrification; ammonia volatilization; redox kinetics | inside main region |
| Derivatives | 1879–3679 | final derivative assembly, negativity clamping, diagnostic dumps, process-rate/saved-output writes | inside main region |

The main region (878–3679) also contains interleaved `!$omp barrier` + `!$omp
master` blocks that drive collective diagnostic dumps.

## Target architecture

The five phases become **internal `contains` procedures** of
`AQUABC_PELAGIC_KINETICS`, in the same file. No new module, no derived-type work
bundle. This is the lowest-churn transformation: array references are unchanged
(`K_E(k)` stays `K_E(k)`), cross-phase intermediates stay as the host's shared
arrays (seen by the internal procedures via host association), and only the
per-thread *private* data is passed as arguments.

Rationale for internal procedures over a separate module: a separate module would
force phase procedures to either take 50–100-argument signatures or promote the
working arrays to module-global state — both materially higher-risk than the
stated goal allows. Internal procedures access the host's shared arrays by host
association with zero reference churn. The accepted trade-off: phases live in the
same file (still large, but broken into a small orchestrator body + five
independently-readable procedures) rather than a separate compilation unit, and
are not independently unit-testable (not a goal — see Non-goals).

### The five phase procedures (internal `contains` of `AQUABC_PELAGIC_KINETICS`)

1. **`pelagic_co2sys_preprocess(...)`** — lifts the existing self-contained
   `!$omp parallel` CO2SYS region whole. Called **serially** from the orchestrator
   body (before the main region); contains its own parallel region (balanced
   chunk split retained). Reads/writes host arrays by host association; `nkn`
   available by host association.

2. **`pelagic_speciation_preprocess(...)`** — sequential aquatic-chemistry
   preprocessing (REDOX/H2S/Fe/Mn fractions). Called serially. No OpenMP.

3. **`pelagic_biology(ns, ne, nkn_local, ...)`** — called once per thread inside
   the orchestrator's main region. Total phyto/chlorophyll + the five biology
   library calls (which already take the derived-type bundles) + N/P dissolution.
   Writes the shared rate/intermediate arrays' `[ns:ne]` slices.

4. **`pelagic_chemistry(ns, ne, nkn_local, ...)`** — called after biology inside
   the main region. Mineralization, nitrification, denitrification,
   volatilization, redox kinetics.

5. **`pelagic_derivatives(ns, ne, nkn_local, ...)`** — called last inside the
   main region. Final derivative assembly, negativity clamping, diagnostic dumps,
   process-rate and saved-output writes. Kept as one procedure (~1,800 lines).
   Contains orphaned `!$omp barrier`/`master` directives that bind to the
   enclosing region at run time (legal; identical collective behavior).

The exact argument list of each in-region procedure is the per-thread private
data it uses — `ns`, `ne`, `nkn_local`, whichever of the four bundles
(`ENV_CHUNK`, `REDOX_STATE_CHUNK`, `REDOX_LIM_CHUNK`, `DOCMIN_CHUNK`) it touches,
and any currently-private scalars it needs — pinned per phase in the
implementation plan.

### Orchestrator body

`AQUABC_PELAGIC_KINETICS`'s executable body shrinks to ~300–400 lines: argument
unpacking, the serial `call pelagic_co2sys_preprocess(...)` and `call
pelagic_speciation_preprocess(...)`, then the main `!$omp parallel` region whose
body is the (ns, ne) computation followed by:

```fortran
!$omp parallel default(shared) num_threads(n_omp) &
!$omp& private(ns, ne, nkn_local, tid, nthreads, chunk_size, rem_omp) &
!$omp& private(<remaining per-thread scalars/bundles>)
    ! ... balanced chunk split (TODO 4.4) computes ns, ne, nkn_local ...
    if (nkn_local > 0) then
        call pelagic_biology     (ns, ne, nkn_local, ...)
        call pelagic_chemistry   (ns, ne, nkn_local, ...)
        call pelagic_derivatives (ns, ne, nkn_local, ...)
    end if
!$omp end parallel
```

Because TODO 4.4 guarantees every thread gets `nkn_local >= 1`, all threads enter
the `if` and all reach the orphaned barriers inside `pelagic_derivatives` — the
same collective behavior as today. This is pure code motion; barrier semantics
are unchanged.

## Interface strategy

- **Shared host arrays** (`STATE_VARIABLES`, `DERIVATIVES`, `PROCESS_RATES`,
  `SAVED_OUTPUTS`, `SEDIMENT_FLUXES`, `PH`, and all `(nkn)` intermediates) are
  accessed by **host association** — no argument passing, no reference changes.
  They are `default(shared)` in the parallel region; each thread writes only its
  `[ns:ne]` slice, so host association is race-free and byte-for-byte identical
  to the inline code.
- **Per-thread private data is passed as arguments** — `ns`, `ne`, `nkn_local`,
  the four bundles, and any private scalars a phase uses. Dummy arguments are
  unambiguously private per call, which sidesteps the OpenMP host-association
  gotcha (that gotcha only affects *private* variables read via host association;
  we never rely on that).
- **Single-phase temporaries** (e.g. `chla_pos`) become locals of their phase
  procedure — automatically private per thread — removing their declarations from
  the host and trimming the host `private(...)` clause accordingly.
- **The four existing bundles** (`ENV_CHUNK`, `REDOX_STATE_CHUNK`,
  `REDOX_LIM_CHUNK`, `DOCMIN_CHUNK`) pass through as arguments to the phases that
  use them.
- No `t_pelagic_work` bundle and no array-storage relocation — the cross-phase
  data flow is exactly the current set of shared host arrays.

## Verification

Bit-identical output is the hard gate, checked **after each phase extraction**
(so a regression is isolated to one small change):

1. **Baseline capture first** — before any extraction, capture reference outputs:
   the 0D golden (already in repo) and a full-model 25-box run (serial and
   `OMP_NUM_THREADS=8`) saved as baseline artifacts.
2. **0D golden regression** — `tests/regression/compare_0D.py`, rtol 1e-9. Must
   pass (bit-identical). Note: nkn=1, so it does *not* exercise chunking.
3. **Full-model 25-box before/after diff** — serial **and** 8-thread, each diffed
   against the *same-configuration* captured baseline (serial-after vs
   serial-before, 8thread-after vs 8thread-before). This is the load-bearing
   check: it exercises multi-node slicing and the OpenMP barriers a 0D run
   cannot. Because the refactor is pure code motion with no change to chunking or
   iteration counts, each same-config comparison must be **exactly bit-identical**
   (byte-for-byte) — no tolerance. (This is distinct from the serial-vs-parallel
   drift characterized in TODO 4.2, which compares *different* thread counts and
   is not what this gate measures.)
4. **Fortran unit tests** (`make test-fortran`) — must stay green.
5. **OpenMP benchmark** (`tools/benchmark_openmp.sh`) — no performance
   regression (phase-call overhead is negligible; confirm empirically).
6. **CI** on the branch — `build-and-run`, `integration-tests`, ftnchek.

If any phase cannot be made bit-identical, that is a **stop-and-investigate**
gate (systematic-debugging), not something to paper over with a tolerance bump.

## Execution plan (incremental)

Subagent-driven-development, **one phase procedure per task**. Each task: extract
one phase into a `contains` procedure → build → run the full verification set →
commit. Order chosen easiest-first so the `contains` structure and the
verification harness are proven on the low-risk cuts before the big ones:

1. Capture baselines (0D golden already exists; capture 25-box serial + 8-thread
   reference outputs) and add the empty `contains` section to
   `AQUABC_PELAGIC_KINETICS`. (No behavior change.)
2. Extract `pelagic_co2sys_preprocess` (self-contained region — cleanest).
3. Extract `pelagic_speciation_preprocess` (sequential — no OpenMP concerns).
4. Extract `pelagic_biology`.
5. Extract `pelagic_chemistry`.
6. Extract `pelagic_derivatives` (largest; kept whole).

Land on a feature branch → CI validate → merge (matching how TODO 4.2/4.4 landed).

## Risks & mitigations

| Risk | Mitigation |
|---|---|
| A private scalar accessed by host association instead of passed as an argument → silently shared → race | The whole strategy passes *all* per-thread private data as arguments; the review checklist for each phase is "does this procedure reference any host variable that is in the `private(...)` clause? If so it must be an argument or a local." Caught by the 8-thread before/after diff. |
| Orphaned barrier bound to wrong/no region | Barriers stay inside the same `if (nkn_local>0)` structure they occupy today; TODO 4.4 guarantees all threads enter. Verified by the 8-thread diff. |
| A shared array a phase writes is left out of scope | Host association means shared arrays need no plumbing — they are simply visible; nothing to leave out. Reference churn is zero. |
| Performance regression from call overhead | OpenMP benchmark re-run each phase; per-thread call overhead is O(1) per node-chunk (called once per thread per timestep, not per node), negligible. |

## Files

- **Modify:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` — the only
  code file changed. `AQUABC_PELAGIC_KINETICS` gains a `contains` section with the
  five phase procedures; its executable body shrinks to a thin orchestrator.
- **Unchanged:** no new file, no build-script change (internal procedures need no
  new compilation unit); the two call sites; all library subroutines; the OpenMP
  strategy.
- **Test artifacts:** baseline reference outputs (25-box serial + 8-thread) for
  the diff gate.
