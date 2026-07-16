# AQUABC_PELAGIC_KINETICS Decomposition Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Break the 3,642-line `AQUABC_PELAGIC_KINETICS` subroutine into a thin orchestrator plus five internal `contains` procedures, with **byte-for-byte identical model output**.

**Architecture:** Pure code motion. Each phase becomes an internal `contains` procedure of `AQUABC_PELAGIC_KINETICS` in the same file. Shared `(nkn)` arrays are reached by **host association** (zero reference churn); only per-thread *private* data (`ns`, `ne`, `nkn_local`, the four derived-type bundles, private scalars) is passed as arguments. The orchestrator body keeps both OpenMP regions and calls the phases. This is **characterization-test-based refactoring**: the regression gate is captured once from the pre-refactor code (Task 1), and every later task must keep it exactly green — there are no new unit tests, the gate *is* the test.

**Tech Stack:** Fortran 90 (gfortran), OpenMP, GNU Make, bash verification harness, Python stdlib 0D comparator.

## Global Constraints

- **Byte-for-byte identical output** after every task — verified by `tools/refactor_verify.sh` reporting `GATE: PASS` (serial + OMP=8 + 0D golden all bit-identical). No tolerance loosening; a non-identical same-config diff is a stop-and-investigate gate (use superpowers:systematic-debugging).
- **Pure code motion only** — no algorithmic/numeric change, no reordering of statements that could change floating-point results, no "while I'm here" cleanup beyond mechanical extraction.
- **Serial fallback preserved** — every OpenMP directive keeps its `!$` sentinel form; a non-`-fopenmp` build must compile and run.
- **Only one code file changes:** `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`. No new module, no build-script change, no change to the two call sites (`SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90`, `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90`).
- **Argument rule for every extracted procedure (the correctness crux):** the argument list is exactly the variables the moved block references that also appear in the host's `!$omp parallel ... private(...)` clause (that clause is: `ns, ne, nkn_local, tid, nthreads, chunk_size, rem_omp, ENV_CHUNK, REDOX_STATE_CHUNK, REDOX_LIM_CHUNK, DOCMIN_CHUNK, i, k, loss, scale_loss, total_removal, allowed_rate, scale, allowed_rate_local, old_rate, sum_removals`). Everything else — the shared `(nkn)` arrays, `MODEL_CONSTANTS`-derived values, use-associated entities — is reached by host association and is **not** passed.
  - **Why this is not optional:** under OpenMP, a variable in the `private(...)` clause that is referenced in a *called* procedure via host association resolves to the **original (shared)** variable, not the calling thread's private copy. Passing it as an actual argument is the only way the thread's private copy reaches the procedure (dummy arguments bind to the caller's copy by reference). So: shared array → host association (correct, each thread writes its `[ns:ne]` slice); private-clause variable → **must be an argument** (or a fresh procedure local, for scalars/indices used nowhere else). Getting this wrong turns a private variable shared → serial stays correct but OMP=8 diverges — which the gate's omp8 diff catches.
  - The four bundles are `type(t_phyto_env) :: ENV_CHUNK`, `type(t_redox_state) :: REDOX_STATE_CHUNK`, `type(t_redox_lim) :: REDOX_LIM_CHUNK`, `type(t_docmin_outputs) :: DOCMIN_CHUNK`. Any bundle a block references is passed `intent(inout)`.
- **Branch:** `refactor/pelagic-kinetics-decomposition` (already created; the spec lives on it).

---

## File Structure

- `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` — the subroutine gains a `contains` section holding the five phase procedures; its executable body shrinks to a ~300–400-line orchestrator.
- `tools/refactor_baseline.sh` — captures pre-refactor reference outputs (already written).
- `tools/refactor_verify.sh` — the bit-identical gate (already written).
- `INPUT_verify.txt` — 30-day 25-box verification config (already written).
- `verify_baseline/` — captured reference outputs (git-ignored).

The phase boundaries (pre-refactor line anchors; find by the banner comments, as line numbers shift after each extraction):

| Procedure | Delimiting markers | ~Lines |
|---|---|---|
| `pelagic_co2sys_preprocess` | **the ENTIRE `if (RUN_CO2SYS .eq. 1) then` construct** — from that `if` (line ~382) through its matching `end if ! call co2sys` (line ~454), inclusive. This wraps the CO2SYS-input population, the CO2SYS `!$omp parallel` region, and its `!$omp end parallel`. Do **not** start at 398 — that would split the `if`/`end if` across two procedures. | 382–454 |
| `pelagic_speciation_preprocess` | from **line ~456** (`HCO3 = HCO3 / 1000000.0`, the first statement *after* `end if ! call co2sys`) to the `! BEGIN OpenMP PARALLEL REGION` banner (line ~860) — REDOX_AND_SPECIATION, H2S, Fe/Mn fractions | 456–859 |
| `pelagic_biology` | inside the main region, from `! D I S S O L V E D  O X Y G E N` (~903) through the ZOOPLANKTON call + N/P dissolution, ending before `ORGANIC_CARBON_MINERALIZATION`'s block (~1411) | 903–1411 |
| `pelagic_chemistry` | from the mineralization block (~1412) through the redox kinetics, ending before `! Final calculation of derivatives` (~1878) | 1412–1878 |
| `pelagic_derivatives` | from `! Final calculation of derivatives` (~1879) **up to but NOT including `end if  ! nkn_local > 0`** (line ~3678). That outer `end if` — which closes the `if (nkn_local > 0)` at line ~900 — stays in the orchestrator, wrapping the three phase calls, immediately before `!$omp end parallel`. The block DOES include the inner CLAMP_COUNT `end if` (~3676, which closes `if (allocated(CLAMP_COUNT))` opened at ~3658). | 1879–~3677 |

**Balanced-nesting rule (why the boundary table is exact):** every `if`/`do`/`select`/`where` a phase opens must be closed *inside the same phase*, and any construct opened before a phase's start or closed after its end stays in the orchestrator. Two constructs straddle phase boundaries and must be kept whole in the orchestrator: `if (RUN_CO2SYS .eq. 1)` (382→454, kept whole *inside* `pelagic_co2sys_preprocess`) and `if (nkn_local > 0)` (900→3678, whose `if` and `end if` both stay in the orchestrator around the three in-region calls). Before cutting any block, run a nesting-balance check on the exact span and confirm it nets to zero (see each task's verification).

---

## Task 1: Verification harness + baselines + `contains` scaffold

**Files (all already present on branch — this task confirms + captures + scaffolds):**
- `tools/refactor_baseline.sh`, `tools/refactor_verify.sh` — the gate harness (default-only).
- `INPUT_verify.txt` (default, all-box output) and its `INPUTS/PELAGIC_INPUTS_verify.txt`, `INPUTS/PELAGIC_OUTPUT_INFORMATION_FILE_verify.txt` (all 25 boxes emit state+process output).
- Modify: `.gitignore` (ignores `verify_baseline/`; the `INPUT_verify*` and `INPUTS/*_verify*` config files ARE committed).
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` — add an empty `contains` section (no procedures yet).

**Gate is default-only, and that is sufficient.** `default` enables output for **all 25 boxes** so every OpenMP thread-chunk (with nkn=25/8thr the chunks are nodes [1-4][5-7][8-10][11-13][14-16][17-19][20-22][23-25]) has monitored nodes — a private→shared bug in any chunk shows up, at both serial and OMP=8. The `advredox` config exists on the branch but is **NOT gated**: its `ADVANCED_REDOX` path has pre-existing uninitialised-memory non-determinism (TODO 1.10/1.11) and so cannot be a bit-identical oracle. The advanced-redox *branches* being moved are still covered by pure-code-motion + compiler explicit-interface checks + review — acceptable for a code-motion refactor.

**Interfaces:**
- Produces: a green `tools/refactor_verify.sh` gate and `verify_baseline/default_{serial,omp8}/` reference outputs (2 dirs × 52 `.out` files) that every later task diffs against.

- [ ] **Step 1: Confirm the harness + config files exist and are executable**

```bash
ls -l tools/refactor_baseline.sh tools/refactor_verify.sh INPUT_verify.txt \
      INPUTS/PELAGIC_INPUTS_verify.txt INPUTS/PELAGIC_OUTPUT_INFORMATION_FILE_verify.txt
chmod +x tools/refactor_baseline.sh tools/refactor_verify.sh
```

- [ ] **Step 2: Capture pre-refactor baselines (default config × serial + omp8)**

```bash
tools/refactor_baseline.sh
```
Expected tail: `default_serial: 52 .out files` and `default_omp8: 52 .out files`.

- [ ] **Step 3: Confirm the gate PASSes on unchanged code**

```bash
tools/refactor_verify.sh; echo "exit=$?"
```
Expected: two `[default/<mode>] BIT-IDENTICAL (52 files)` lines, `[0D golden] PASS`, `GATE: PASS`, `exit=0`.

- [ ] **Step 4: Add an empty `contains` section to the subroutine**

Immediately before the final `end subroutine AQUABC_PELAGIC_KINETICS` (line ~3692), insert:

```fortran
contains

    ! Phase procedures extracted from the monolithic body (TODO 1.6).
    ! Shared (nkn) arrays are reached by host association; only per-thread
    ! private data is passed as arguments. See the plan's Global Constraints.

end subroutine AQUABC_PELAGIC_KINETICS
```
(An empty `contains` with no procedures is valid Fortran and is a no-op.)

- [ ] **Step 5: Rebuild and re-run the gate (still PASS — empty contains is a no-op)**

```bash
tools/refactor_verify.sh; echo "exit=$?"
```
Expected: `GATE: PASS`, `exit=0`.

- [ ] **Step 6: Commit**

```bash
git add .gitignore tools/refactor_baseline.sh tools/refactor_verify.sh INPUT_verify.txt SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
git commit -m "refactor(pelagic): add bit-identical verification harness + empty contains scaffold (TODO 1.6)"
```

---

## Task 2: Extract `pelagic_co2sys_preprocess`

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Interfaces:**
- Consumes: host arrays and `nkn` by host association.
- Produces: internal procedure `pelagic_co2sys_preprocess()` (no arguments — the CO2SYS block runs serially and reads/writes host arrays; the `ns/ne/nkn_local/rem_omp/n_omp` it uses inside its *own* parallel region are host variables it can privatize in its own directive or declare locally).

- [ ] **Step 1: Identify the block — the WHOLE `if (RUN_CO2SYS .eq. 1)` construct**

CRITICAL: the block is the **entire** `if (RUN_CO2SYS .eq. 1) then ... end if ! call co2sys` construct (pre-refactor lines ~382–454), NOT just the inner `!$omp parallel` region. It contains the CO2SYS-input population, the `!$omp parallel` region, and the closing `end if`. Extracting only the inner `!$omp` region (398–451) would leave the `if` in the orchestrator and move its `end if` — an unmatched-IF compile error. Locate both ends:

```bash
grep -n "if (RUN_CO2SYS .eq. 1) then\|end if ! call co2sys\|!\$omp end parallel" SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90 | head
# Confirm the span nests to zero (open == close):
awk '/if \(RUN_CO2SYS \.eq\. 1\) then/{f=1} f{print} /end if ! call co2sys/{if(f)exit}' \
  SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90 | grep -cE 'then$|end if'
```

- [ ] **Step 2: Move the whole construct into a `contains` procedure**

Cut lines ~382–454 (the full `if (RUN_CO2SYS...)` through `end if ! call co2sys`) and paste verbatim:

```fortran
    subroutine pelagic_co2sys_preprocess()
        ! CO2SYS carbonate-chemistry preprocessing (TODO 1.6 — verbatim lift).
        ! Includes the whole `if (RUN_CO2SYS .eq. 1) then ... end if` guard.
        ! Runs serially; its own !$omp parallel region chunks CO2SYS across
        ! threads. All arrays reached by host association.
        <the moved block — the full if/end-if construct, unchanged>
    end subroutine pelagic_co2sys_preprocess
```

Replace the original location (the whole 382–454 span) with a single call:

```fortran
    call pelagic_co2sys_preprocess()
```

Note: variables used only inside this block's own parallel region (`co2_ntps`, `CO2SYS_OUT_LOCAL`, `CO2SYS_HEAD_LOCAL`, and the chunk scalars in *its* private clause) must remain visible — keep their host declarations (host association); that is the smaller diff.

- [ ] **Step 3: Build**

```bash
make build-estas 2>&1 | tail -3
```
Expected: `Executable 'ESTAS_II' created successfully` (no unused-variable or interface errors).

- [ ] **Step 4: Run the gate**

```bash
tools/refactor_verify.sh; echo "exit=$?"
```
Expected: `GATE: PASS`, `exit=0`.

- [ ] **Step 5: Commit**

```bash
git add SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
git commit -m "refactor(pelagic): extract pelagic_co2sys_preprocess (TODO 1.6)"
```

---

## Task 3: Extract `pelagic_speciation_preprocess`

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Interfaces:**
- Consumes: host arrays by host association.
- Produces: internal procedure `pelagic_speciation_preprocess()` (no arguments — sequential; no OpenMP).

- [ ] **Step 1: Identify the block — starting AFTER the `RUN_CO2SYS` `end if`**

The sequential aquatic-chemistry preprocessing runs from the **first statement after `end if ! call co2sys`** (pre-refactor `HCO3 = HCO3 / 1000000.0`, line ~456 — this is *after* Task 2 replaced 382–454 with `call pelagic_co2sys_preprocess()`) to the `! BEGIN OpenMP PARALLEL REGION` banner (~860): `REDOX_AND_SPECIATION`, H2S species, Fe²⁺/Fe³⁺/Mn²⁺ dissolved/particulate fractions, settling prep, saved-output guards. Do **not** start at line 452 — that would sweep in the `end if ! call co2sys` (already handled by Task 2) and unbalance the nesting.

```bash
grep -n "call pelagic_co2sys_preprocess\|REDOX_AND_SPECIATION\|BEGIN OpenMP PARALLEL REGION" SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
# Nesting-balance the intended span (must net to zero — no orphan end-if):
awk '/HCO3 = HCO3 \/ 1000000.0/{f=1} /BEGIN OpenMP PARALLEL REGION/{f=0} f' \
  SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90 | grep -cE 'then$' # compare against end-if count
```

- [ ] **Step 2: Move the block into a `contains` procedure**

```fortran
    subroutine pelagic_speciation_preprocess()
        ! Sequential aquatic-chemistry preprocessing (TODO 1.6 — verbatim lift):
        ! redox/speciation, H2S, Fe/Mn dissolved-particulate fractions.
        <the moved block, unchanged>
    end subroutine pelagic_speciation_preprocess
```

Replace the original location with:

```fortran
    call pelagic_speciation_preprocess()
```

This block populates the **plain, non-chunk** `REDOX_STATE` / `REDOX_LIM` bundles (pointer assignments feeding the serial whole-`nkn` `REDOX_AND_SPECIATION` call). They are host variables reached by host association — **no arguments** for this serial procedure. (The `_CHUNK` variants `REDOX_STATE_CHUNK`/`REDOX_LIM_CHUNK` are a *different* pair, populated later inside the parallel region — they belong to Task 5, not here.)

- [ ] **Step 3: Build**

```bash
make build-estas 2>&1 | tail -3
```
Expected: `Executable 'ESTAS_II' created successfully`.

- [ ] **Step 4: Run the gate**

```bash
tools/refactor_verify.sh; echo "exit=$?"
```
Expected: `GATE: PASS`, `exit=0`.

- [ ] **Step 5: Commit**

```bash
git add SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
git commit -m "refactor(pelagic): extract pelagic_speciation_preprocess (TODO 1.6)"
```

---

## Task 4: Extract `pelagic_biology`

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Interfaces:**
- Consumes: `ns, ne, nkn_local` and the four bundles from the enclosing parallel region.
- Produces: internal procedure `pelagic_biology(ns, ne, nkn_local)` called once per thread inside the main region. (Determine the exact argument list by the Global-Constraints argument rule: the private-clause variables the block references — expected `ns, ne, nkn_local` plus `ENV_CHUNK`, and any of `i, k` used as loop indices; declare loop indices as procedure locals rather than arguments where possible.)

- [ ] **Step 1: Identify the block**

Inside the main `!$omp parallel` region: dissolved-oxygen/aeration, total phytoplankton + chlorophyll, light extinction, `ENV_CHUNK` population, and the five biology library calls (`DIATOMS`, `CYANOBACTERIA_BOUYANT`, `FIX_CYANOBACTERIA_BOUYANT`, `NOSTOCALES`, `ZOOPLANKTON`) plus N/P dissolution (pre-refactor lines ~903–1411, i.e. the body inside `if (nkn_local > 0) then` up to the mineralization block — but **do not** include the `if (nkn_local > 0) then` line itself; it stays in the orchestrator).

```bash
grep -n "D I S S O L V E D  O X Y G E N\|call DIATOMS\|call ZOOPLANKTON\|MINERALIZATION OF DOC" SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
```

ORPHANED-DIRECTIVE HAZARD: this block contains an `!$omp critical`/`end critical` pair (guarding a negative-CHLA warning, ~lines 961–963) and **two** `!$omp barrier` / `!$omp master` … `!$omp end master` / `!$omp barrier` pairs (guarding the `DBGSTR_PEL_R_ZOO_GROWTH_01` / `DBGSTR_PEL_R_ZOO_RESP_01` debug calls, ~lines 1327–1341). Move them **verbatim** — never split a `barrier` from its matching `master`/`end master`, or truncate the block between the two barrier pairs. A mismatched barrier count across threads hangs the OMP=8 leg of the gate.

- [ ] **Step 2: Determine the argument list**

```bash
# Shift-proof: scan the block BETWEEN its banner markers, not by line number.
F=SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
for v in ns ne nkn_local ENV_CHUNK REDOX_STATE_CHUNK REDOX_LIM_CHUNK DOCMIN_CHUNK i k loss scale_loss total_removal allowed_rate scale allowed_rate_local old_rate sum_removals; do
  n=$(awk '/D I S S O L V E D  O X Y G E N/,/MINERALIZATION OF DOC/' "$F" | grep -wc "$v")
  echo "$v = $n"
done
```
Arguments = the private-clause variables with a non-zero count. Loop indices (`i`, `k`) that are only used locally should be declared as procedure locals instead of arguments (they are auto-private). `ns, ne, nkn_local` are always arguments.

- [ ] **Step 3: Move the block into the `contains` procedure**

Per the Global-Constraints argument rule, any bundle the block references (expected `ENV_CHUNK`; add `REDOX_STATE_CHUNK`/`REDOX_LIM_CHUNK`/`DOCMIN_CHUNK` if Step 2 shows a non-zero count) is passed `intent(inout)` — it is a private-clause member, so it **must** be an argument, never host association:

```fortran
    subroutine pelagic_biology(ns, ne, nkn_local, ENV_CHUNK)
        integer,             intent(in)    :: ns, ne, nkn_local
        type(t_phyto_env),   intent(inout) :: ENV_CHUNK
        integer :: i, k          ! local loop indices (auto-private)
        ! Phytoplankton + zooplankton phase (TODO 1.6 — verbatim lift).
        ! Shared (nkn) arrays via host association; private bundles are arguments.
        <the moved block, unchanged except removing local re-declarations>
    end subroutine pelagic_biology
```

Replace the block inside the main region with:

```fortran
        call pelagic_biology(ns, ne, nkn_local, ENV_CHUNK)
```

- [ ] **Step 4: Build**

```bash
make build-estas 2>&1 | tail -3
```
Expected: `Executable 'ESTAS_II' created successfully`.

- [ ] **Step 5: Run the gate (both serial and OMP=8 must be bit-identical)**

```bash
tools/refactor_verify.sh; echo "exit=$?"
```
Expected: `GATE: PASS`, `exit=0`. If OMP=8 differs but serial matches, a private variable became shared — revisit Step 3's argument list (systematic-debugging).

- [ ] **Step 6: Commit**

```bash
git add SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
git commit -m "refactor(pelagic): extract pelagic_biology (TODO 1.6)"
```

---

## Task 5: Extract `pelagic_chemistry`

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Interfaces:**
- Consumes: `ns, ne, nkn_local` + referenced bundles (expected `DOCMIN_CHUNK`, `REDOX_STATE_CHUNK`, `REDOX_LIM_CHUNK`) from the enclosing region.
- Produces: internal procedure `pelagic_chemistry(ns, ne, nkn_local, <referenced bundles>)`.

- [ ] **Step 1: Identify the block**

From the mineralization block through redox kinetics, ending before `! Final calculation of derivatives` (pre-refactor lines ~1412–1878): `ORGANIC_CARBON_MINERALIZATION`, DIN/DIP-scarcity acceleration, nitrification, denitrification, ammonia volatilization, redox kinetics.

```bash
grep -n "MINERALIZATION OF DOC\|call ORGANIC_CARBON_MINERALIZATION\|DENITRIFICATION\|VOLATILIZATION OF UNIONIZED\|Final calculation of derivatives" SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
```

- [ ] **Step 2: Determine the argument list**

```bash
F=SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
for v in ns ne nkn_local ENV_CHUNK REDOX_STATE_CHUNK REDOX_LIM_CHUNK DOCMIN_CHUNK i k loss scale_loss total_removal allowed_rate scale allowed_rate_local old_rate sum_removals; do
  n=$(awk '/MINERALIZATION OF DOC/,/Final calculation of derivatives/' "$F" | grep -wc "$v")
  echo "$v = $n"
done
```
Arguments = the private-clause variables with a non-zero count (`ns, ne, nkn_local` always).

- [ ] **Step 3: Move the block into the `contains` procedure**

```fortran
    subroutine pelagic_chemistry(ns, ne, nkn_local, DOCMIN_CHUNK, REDOX_STATE_CHUNK, REDOX_LIM_CHUNK)
        integer,                intent(in)    :: ns, ne, nkn_local
        type(t_docmin_outputs), intent(inout) :: DOCMIN_CHUNK
        type(t_redox_state),    intent(inout) :: REDOX_STATE_CHUNK
        type(t_redox_lim),      intent(inout) :: REDOX_LIM_CHUNK
        integer :: i, k
        ! Mineralization / nitrification / denitrification / volatilization / redox
        ! kinetics (TODO 1.6 — verbatim lift). Shared (nkn) arrays via host
        ! association; private bundles are arguments.
        <the moved block, unchanged>
    end subroutine pelagic_chemistry
```
Include only the bundles the block actually references (from Step 2); drop any with a zero count and its declaration+argument.

Replace the block with:

```fortran
        call pelagic_chemistry(ns, ne, nkn_local, DOCMIN_CHUNK, REDOX_STATE_CHUNK, REDOX_LIM_CHUNK)
```

- [ ] **Step 4: Build**

```bash
make build-estas 2>&1 | tail -3
```
Expected: `Executable 'ESTAS_II' created successfully`.

- [ ] **Step 5: Run the gate**

```bash
tools/refactor_verify.sh; echo "exit=$?"
```
Expected: `GATE: PASS`, `exit=0`.

- [ ] **Step 6: Commit**

```bash
git add SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
git commit -m "refactor(pelagic): extract pelagic_chemistry (TODO 1.6)"
```

---

## Task 6: Extract `pelagic_derivatives`

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`

**Interfaces:**
- Consumes: `ns, ne, nkn_local` + referenced private scalars/bundles.
- Produces: internal procedure `pelagic_derivatives(ns, ne, nkn_local, <referenced args>)`. This block contains orphaned `!$omp barrier`/`master`/`critical` directives — they stay verbatim and bind to the enclosing region at run time.

- [ ] **Step 1: Identify the block — stop BEFORE `end if  ! nkn_local > 0`**

CRITICAL boundary: the block runs from `! Final calculation of derivatives` up to but **NOT including** the `end if  ! nkn_local > 0` line (pre-refactor ~3678). That `end if` closes the `if (nkn_local > 0)` opened at ~line 900 and **must stay in the orchestrator**, wrapping the three phase calls, immediately before `!$omp end parallel`. Taking the block "up to `!$omp end parallel`" (naïvely) would sweep that `end if` into the callee → unmatched-IF compile error. The block DOES include the inner CLAMP_COUNT `end if` (~3676). Locate the exact stop line:

```bash
F=SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
grep -n "Final calculation of derivatives\|end if  ! nkn_local > 0\|!\$omp end parallel" "$F" | tail
# The moved block ends at the line JUST BEFORE the "end if  ! nkn_local > 0" match.
# Confirm the intended span nets to zero (opens == closes):
awk '/Final calculation of derivatives/{f=1} /end if  ! nkn_local > 0/{f=0} f' "$F" \
  | grep -cE 'then$|[^e]do |select case'   # compare against the end-* count in the same span
```

- [ ] **Step 2: Determine the argument list**

```bash
F=SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
# awk from the marker to (but not including) the outer nkn_local end-if:
for v in ns ne nkn_local ENV_CHUNK REDOX_STATE_CHUNK REDOX_LIM_CHUNK DOCMIN_CHUNK i k loss scale_loss total_removal allowed_rate scale allowed_rate_local old_rate sum_removals; do
  n=$(awk '/Final calculation of derivatives/{f=1} /end if  ! nkn_local > 0/{f=0} f' "$F" | grep -wc "$v")
  echo "$v = $n"
done
```
The derivative-assembly + clamping code is the heaviest user of the `loss/scale/total_removal/...` clamping scalars — expect most of them to be arguments or (preferably) procedure locals. Scalars used only within this one procedure should be declared as procedure locals (auto-private) and **removed from the host `private(...)` clause** to keep it honest.

- [ ] **Step 3: Move the block into the `contains` procedure**

```fortran
    subroutine pelagic_derivatives(ns, ne, nkn_local, <bundles/scalars from Step 2>)
        integer, intent(in) :: ns, ne, nkn_local
        <declare referenced bundles as intent(inout); declare clamp scalars as locals>
        integer :: i, k
        ! Final derivative assembly + negativity clamping + diagnostics
        ! (TODO 1.6 — verbatim lift). Orphaned !$omp barrier/master/critical bind
        ! to the enclosing region; all !$ sentinels preserved.
        <the moved block, unchanged>
    end subroutine pelagic_derivatives
```

Replace the block with just the call — and verify the orchestrator now reads exactly as the spec's target, with the `if (nkn_local > 0)` / `end if` pair (from line ~900 and ~3678) intact around the three calls:

```fortran
        if (nkn_local > 0) then
            call pelagic_biology     (ns, ne, nkn_local, ENV_CHUNK)
            call pelagic_chemistry   (ns, ne, nkn_local, DOCMIN_CHUNK, REDOX_STATE_CHUNK, REDOX_LIM_CHUNK)
            call pelagic_derivatives (ns, ne, nkn_local, <args>)
        end if
    !$omp end parallel
```

- [ ] **Step 4: Trim the host `private(...)` clause**

Remove from the host region's `private(...)` clause any scalar now declared as a `pelagic_derivatives` local (e.g. `loss, scale_loss, total_removal, allowed_rate, scale, allowed_rate_local, old_rate, sum_removals` if they are used nowhere else in the region). Confirm each removed name is referenced nowhere else in the main region:

```bash
grep -n "\bloss\b\|\bscale_loss\b\|\btotal_removal\b" SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
```

- [ ] **Step 5: Build**

```bash
make build-estas 2>&1 | tail -3
```
Expected: `Executable 'ESTAS_II' created successfully`.

- [ ] **Step 6: Run the gate (the OMP=8 diagnostics/barrier path is the key check)**

```bash
tools/refactor_verify.sh; echo "exit=$?"
```
Expected: `GATE: PASS`, `exit=0`. Also confirm no hang at OMP=8 (the gate's omp8 run completing is the confirmation).

- [ ] **Step 7: Commit**

```bash
git add SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90
git commit -m "refactor(pelagic): extract pelagic_derivatives; orchestrator complete (TODO 1.6)"
```

---

## Task 7: Finalize — backlog, benchmark, land

**Files:**
- Modify: `TODO_IMPLEMENTATION_PLAN.md` (§1.6 → done + roadmap checkbox)

- [ ] **Step 1: Confirm the orchestrator body is thin**

```bash
awk '/^subroutine AQUABC_PELAGIC_KINETICS/,/^ *contains/' SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90 | wc -l
```
Expected: a few hundred lines (declarations + preprocessing calls + the parallel region with three phase calls), not thousands.

- [ ] **Step 2: Full test suite + benchmark (no perf regression)**

```bash
make test-fortran 2>&1 | tail -5
.venv/bin/python -m pytest tests/python/test_e2e_regression.py -v 2>&1 | tail -5
tools/benchmark_openmp.sh --quick 2>&1 | tail -8
```
Expected: Fortran tests pass; 0D regression passes; benchmark speedup unchanged from the docs/OPENMP_PERFORMANCE.md figures (within noise).

- [ ] **Step 3: Mark the backlog item done**

In `TODO_IMPLEMENTATION_PLAN.md`, change the §1.6 status to `✅ COMPLETE` with a one-line summary, and check the `- [ ] 1.6` roadmap box to `- [x]`.

- [ ] **Step 4: Commit**

```bash
git add TODO_IMPLEMENTATION_PLAN.md
git commit -m "docs: mark TODO 1.6 (mega-subroutine decomposition) complete"
```

- [ ] **Step 5: Push and let CI validate, then finish the branch**

```bash
git push -u origin refactor/pelagic-kinetics-decomposition
```
Use superpowers:finishing-a-development-branch to present merge options after CI is green.

---

## Notes for the executor

- **The gate is the whole test strategy.** There are no new unit tests; `tools/refactor_verify.sh` reporting `GATE: PASS` after each task is the pass criterion. It rebuilds both binaries and, for the default all-box config, runs the 30-day 25-box case serial + OMP=8, diffs each threadmode against its own baseline, and runs the 0D golden. A build failure aborts the gate (it never diffs a stale binary).
- **Same-threadmode is the only comparison.** serial vs omp8 legitimately differ (a few PROCESS_RATES files, the TODO 4.2 CO2SYS chunking drift). The gate never cross-compares serial vs omp8 — only `default_<mode>` after vs `default_<mode>` before. Do not "fix" that difference.
- **If a same-config diff appears**, stop and use superpowers:systematic-debugging. The most likely cause is a private variable reached by host association instead of passed as an argument (turns private → shared → OMP=8 diverges from serial-after). The fix is to add it to the procedure's argument list.
- **Line numbers shift after each extraction.** Always re-find a block by its banner-comment markers (the greps in each task), not by the pre-refactor line numbers.
- **`intent` for shared arrays reached by host association:** none — they are not arguments. Only per-thread private data is passed, and its `intent` is `in` for `ns/ne/nkn_local` and `inout` for bundles the block updates.
- **Two DISTINCT redox bundle pairs — don't conflate them.** The **non-chunk** `REDOX_STATE`/`REDOX_LIM` are populated and consumed entirely within the *serial* `pelagic_speciation_preprocess` (Task 3, feeding the whole-`nkn` `REDOX_AND_SPECIATION` call) — reached by host association, no arguments. The **`_CHUNK`** variants `REDOX_STATE_CHUNK`/`REDOX_LIM_CHUNK`/`DOCMIN_CHUNK` are populated and consumed entirely within the *in-region* `pelagic_chemistry` (Task 5) — passed as `intent(inout)` arguments (private-clause members). They are different variables of the same type; the serial population does not carry into the region. `ENV_CHUNK` is populated + consumed in `pelagic_biology` (Task 4), passed as an argument. The Step-2 arg scan in each task is the source of truth (non-zero count in the block → argument).
- **Validated arg predictions** (from the pre-refactor code): biology → `ENV_CHUNK`; chemistry → `DOCMIN_CHUNK, REDOX_STATE_CHUNK, REDOX_LIM_CHUNK`; derivatives → the clamp scalars (`loss` etc.) as locals + whichever bundles Step 2 shows. Re-run the Step-2 scan to confirm before writing each signature.
