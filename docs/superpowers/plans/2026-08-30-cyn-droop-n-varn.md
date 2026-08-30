# CYN Droop-N (VARN build variant) Implementation Plan — r2 (post-review)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the opt-in CYN nitrogen-quota (Droop) mechanism as a compile-variant `VARN` build (`nstate=33`), with admissibility-gated constants, complete transport wiring for the new state, and the falsifiable August science ladder.

**Architecture:** `CYN_N` becomes transported state 33 in a separate `ESTAS_II_varN` binary (trap-guarded in-place `nstate` patch); the kinetics gain a flag-gated Droop branch in BOTH `CYANOBACTERIA` variants; the model routes all CYN nitrogen Q-weighted, zeroes BOTH DON growth-sink assignments, initializes the new state's transport switches, and books settled quota-N conservatively. Lib, model, and call sites change in ONE task (implicit interfaces make a split unbuildable-safely). Constants ride five graceful options lines; a committed desk-gate tool enforces admissibility before any run.

**Tech Stack:** Fortran 2008 (gfortran), tests/fortran harness, python3+numpy tools, a CYN-only degenerate scenario as the conservation vehicle.

**Spec:** `docs/superpowers/specs/2026-08-30-cyn-droop-n-rescoped-design.md` — normative. This r2 plan incorporates the 2026-08-30 adversarial plan review (23 findings, 10 confirmed, 0 refuted; every confirmed/unverified/minor item is folded in below).

## Global Constraints

- Standard `ESTAS_II` build **byte-identical** (0D golden + CL29 30-day A/B vs a Step-0 baseline; the full-record A/B in T7 compares two DISTINCT binaries — assert their md5s differ before diffing); every new code path behind `CYN_VARIABLE_N > 0`; the tracked `mod_GLOBAL.f90` `nstate` line is patched only transiently by the varn target (trap-restored, `git diff --exit-code` proven).
- Committed constants exactly (spec §2): `CYN_VARIABLE_N 0`, `CYN_N_QMIN 0.10`, `CYN_N_QMAX 0.25`, `CYN_N_VMAX 0.44`, `CYN_N_KHS_UPT 0.003`; `Q_SEED = CYN_N_TO_C = 0.220` (verify vs WCONST #47).
- The admissibility gate (spec §2) must PASS before the science ladder.
- Units: `CYN_N` mg N/L; quota gN/gC; routing Q-weighted; uptake capped per step by available DIN (reuse the existing loss-safeguard pattern — find it with `grep -n "safeguard\|MIN(.*NH4_N" aquabc_II_pelagic_model.f90` and mirror it); O2 stoichiometry unchanged; reported chlorophyll fixed C:Chl 78.
- Conservation asserted at the LIB-RATE level (unit, with grazing as a test-injected input — the lib does not compute grazing) and on a **CYN-only degenerate 0D-style scenario** (all other biology zeroed by ICs/boundaries) — never as a per-box identity under transport, and never as a raw pool sum on a full-biology run.
- Flag=1 on a standard (`nstate=32`) binary → `error stop` at options-read time.
- House style, 132-col, no tabs; commits `feat(varn):`/`test(varn):` ending: Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>
- NEVER modify the live `INPUTS_CL29/` or `INPUT_CL29.txt`. Residency: the driver `INPUT_CL29_VARN.txt` is tracked (like `INPUT_CL29.txt`); the generated `INPUTS_CL29_VARN/` and `OUTPUTS_CL29_VARN/` are gitignored here and versioned in the data repo on adoption only.

---

### Task 1: The admissibility-gate tool

**Files:** Create `tools/droop_gate.py`; Test `tests/python/test_droop_gate.py` (match the existing tests/python invocation style; CI runs `make test-python` — verify with `grep -n test-python Makefile .github/workflows/ci.yml` and register accordingly).

**Produces:** `python3 tools/droop_gate.py --kg 2.0 --ftemp 0.78 --flight 0.30 --din 0.004 --khs 0.003 --qmin 0.10 --qmax 0.25 --vmax 0.44` → prints August `Q*`, `LIM_N*`, the June leg (`--din 0.022 --ftemp 0.63 --flight 0.33`), exits 0 iff August `LIM_N*` > 0.571 AND June `Q*` ≥ Q_MIN + 0.8·(Q_MAX−Q_MIN). Bisection on `VMAX·M·(Q_MAX−Q) = KG·ftemp·flight·Q·(Q−Q_MIN)`, M = DIN/(KHS+DIN); the M=1 upper-bound short-circuit fails fast.

- [ ] **Step 1: failing test** — committed constants PASS both legs (assert `0.70 < LIM_N*_Aug < 0.75` and June leg passes; do NOT assert a rounded decimal — compute the reference inside the test with the same fractions); VMAX 0.06 FAILS with the M=1 short-circuit message; exit codes 0/1 asserted.
- [ ] **Step 2:** run → module missing. **Step 3:** implement (docstring carries the spec §2 identity). **Step 4:** tests green; run at committed values, paste output into the commit body. **Step 5:** commit `feat(varn): droop_gate admissibility tool`.

### Task 2: Options lines, the params module, flag global, mis-pair guard, baseline

**Files:**
- Create: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_cyn_droop.f90` — in THIS task (the review's orphan-setter fix): module `AQUABC_CYN_DROOP` with the four scalars (defaults 0.10/0.25/0.44/0.003), `SET_CYN_DROOP_PARAMS(qmin, qmax, vmax, khs)`, and parameter `EPS_CYN_C = 1.0D-10`; Task 3 extends it with the physics helpers.
- Modify: `SOURCE_CODE/ESTAS/mod_GLOBAL.f90` (`integer :: CYN_VARIABLE_N = 0` beside the option globals; `nstate` at :16 untouched).
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` `READ_PELAGIC_MODEL_OPTIONS`: five graceful pairs inserted AFTER the staging block's last read pair (the one reading `V_SETTLE_IN` — locate by `grep -n "V_SETTLE_IN" mod_PELAGIC_ECOLOGY.f90`, NOT by a comment string; the options FILE's matching lines are `# V_SETTLE_AKI …` + value) and BEFORE `900 continue`; setter call after label 900 beside the staging setter; ON-echo prints all four scalars; the guard.

- [ ] **Step 0: baseline** — current `main` build: 0D golden + scratch CL29 30-day (quoted paths, `ESTAS_HOLD_VOLUME=1`, from repo root) → `/tmp/varn_ab/baseline/`.
- [ ] **Step 1:** module + reads + defaults + echo + guard `if (CYN_VARIABLE_N > 0 .and. nstate /= 33) error stop 'CYN_VARIABLE_N=1 requires the VARN build (nstate=33)'`.
- [ ] **Step 2: gates** — `make build-estas` clean; 30-day rerun `diff -r` clean vs baseline; suite green.
- [ ] **Step 3:** commit `feat(varn): CYN_VARIABLE_N flag + options + AQUABC_CYN_DROOP params module + guard`.

### Task 3: The Droop mechanism — lib + model + transport, ONE task (review: a split is unbuildable-safely under implicit interfaces)

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_cyn_droop.f90` — add pure helpers `F_DOWN(Q)`, `LIM_N_QUOTA(Q)`, `R_UPTAKE(DIN, Q, CYN_C)` implementing spec §2 verbatim.
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_svindex.f90` — `integer, parameter :: CYN_N_INDEX = 33` (comment: live only in the VARN build; grep first that nothing dimensions by it in the standard build).
- Modify: `…lib_CYANOBACTERIA.f90` BOTH variants (the CL29 path is `CYANOBACTERIA_BOUYANT`, called at model.f90:1134; the plain variant has NO model call — its callers are the 0D interface path and tests, verify with `grep -rn "call CYANOBACTERIA\b"`): new args `CYN_VARIABLE_N` (int), `CYN_N` (in), `R_CYN_N_UPTAKE` (out); flag on → `LIM_KG_CYN_N = LIM_N_QUOTA(CYN_N/max(CYN_C, EPS_CYN_C))`, uptake per helpers; flag off → legacy verbatim, new outs zeroed.
- Modify: `aquabc_II_pelagic_model.f90` — in the SAME commit: both call-site extensions; `R_CYN_N_UPTAKE(nkn)`; the per-step DIN cap on uptake (the mirrored safeguard); `DERIVATIVES(:,CYN_N_INDEX)` from new PROCESS_RATES slots (uptake source; Q-weighted resp/death/excr/graze sinks); the routing replacements at ALL sites, each flag-gated: NH4 resp :2040, NH4 uptake :2052 → `R_CYN_N_UPTAKE·PREF_NH4N_CYN`, NO3 uptake :2131 → `R_CYN_N_UPTAKE·(1−PREF_NH4N_CYN)`, DON excr :3015, ZOO_N :2772, DET_N CYN-death (`grep -n "R_CYN_DEATH.*CYN_N_TO_C"`), **and BOTH DON growth-sink slot-5 assignments — :3018–3019 AND the duplicate inside `if (DO_NOSTOCALES > 0)` at :3050–3051** (verify-grep `grep -n "DISS_ORG_N_INDEX, 5" …model.f90` → exactly two assignment hits, both zeroed under the flag; CL29 runs the NOSTOCALES branch, so missing the second silently destroys N in the science run); O2 :2242 untouched.
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` — flag+`CYN_N` threading into the kinetics call, **and `INIT_TRANSPORT_FIELDS` (:724–749): after the index-32 lines, `if (nstate >= 33)` set `ADVECTION_ON(33)=1; DIFFUSION_ON(33)=1; SETTLING_ON(33)=1`** (review blocker: the literal ranges 1:31/32 + the nstate-derived allelopathy block skip 33 → uninitialized transport switches in the VARN build); dead code at nstate=32.
- **Settled-N booking (review blocker, spec-completing decision, option (a) conservative):** under the flag, the bed PON handoff books CYN_N's OWN settling flux and drops the `CYN_C-settling × CYN_N_TO_C` term — BOTH sites, `aquabc_II_pelagic_auxillary.f90:1035` and `:1249` (verify-grep `CYN_N_TO_C` in that file → exactly these two).
- Modify: `mod_AQUATIC_MODEL.f90` count assert (~:179/:194–207) → `declared_total == nstate + merge(4,0,CONSIDER_ALLELOPATHY>0)`.
- Modify: the negative-mass metabolite literal `33..36` → `nstate+1..nstate+NUM_ALLOLOPATHY_STATE_VARS` at **BOTH** sites (`grep -n "33" mod_PELAGIC_ECOLOGY.f90 | grep -v nstate`, read context; the review found two).
- Modify: `aquabc_II_pelagic_interface.f90` — literal `0` flag + zero-filled `CYN_N` actual for the 0D path (mirror the staging-arg handling there).
- Create: `tests/fortran/test_cyn_droop.f90` (+ Makefile target + TEST_PROGS); Modify `tests/fortran/test_cyanobacteria.f90` (signature).

**Test cases (write first, expect assertion-failures not compile-failures — no explicit interfaces):**
1. `F_DOWN` endpoints; 2. `LIM_N_QUOTA` clamps; 3. uptake at DIN 0.004, Q 0.15, CYN_C 1.0 — expected computed IN the test as `0.44d0·(0.004d0/0.007d0)·((0.25d0−0.15d0)/0.15d0)` (≈0.16762; the review killed the rounded-decimal-at-1e-10 pattern — always compare to the exact expression); 4. flag=0 pass-through (legacy LIM unchanged, new outs zero); 5. the rate-level N balance **with grazing as a TEST-INJECTED input** (the lib does not compute grazing): `uptake − (resp+death+excr+graze_injected)·Q == quota-net` to 1e-12; 6. DON-sink invariant (routine's DON-uptake term ≡ 0 under flag).

- [ ] **Steps:** tests first (RED as assertion failures) → implement everything above → full suite green → `make build-estas` clean → **0D golden + CL29 30-day A/B byte-identical vs baseline** → commit `feat(varn): Droop mechanism end-to-end (lib both variants + model routing + transport switches + settled-N booking + count assert), flag-gated`.

### Task 4: The `build-estas-varn` target

**Files:** Modify `Makefile`. **Review fix: make runs each recipe line in a separate shell — the trap idiom requires `.ONESHELL:` or a single `&&`-joined line.** Use one line:

```make
build-estas-varn:
	cp SOURCE_CODE/ESTAS/mod_GLOBAL.f90 /tmp/.mG.bak && trap 'cp /tmp/.mG.bak SOURCE_CODE/ESTAS/mod_GLOBAL.f90' EXIT && sed -i 's/nstate                        = 32/nstate                        = 33/' SOURCE_CODE/ESTAS/mod_GLOBAL.f90 && grep -c "nstate                        = 33" SOURCE_CODE/ESTAS/mod_GLOBAL.f90 | grep -qx 1 && $(MAKE) build-estas && mv ESTAS_II ESTAS_II_varN && cp /tmp/.mG.bak SOURCE_CODE/ESTAS/mod_GLOBAL.f90 && git diff --exit-code SOURCE_CODE/ESTAS/mod_GLOBAL.f90 && $(MAKE) build-estas
```
(verify the exact spacing of :16 first and adjust the sed; the `grep -qx 1` asserts exactly-one substitution; the 0D driver/interface `nstate=32` literals — interface.f90:74 `integer, save :: nstate = 32` and the 0D driver's own — are NOT patched: the 0D path stays 32-state, and the conservation vehicle is the degenerate CL29 scenario of Task 6, NOT the 0D example — review correction of the old V4.)

- [ ] Verify sed uniqueness → implement → gates: `ESTAS_II_varN` exists; tree clean (`git status`); `ESTAS_II` standard (30-day A/B); the mis-pair guard fires (standard binary + flag=1 options copy aborts, assert exit≠0 AND the message text) → commit.

### Task 5: The VARN setup generator

**Files:** Create `tools/make_varn_inputs.py` + `INPUT_CL29_VARN.txt` (tracked) + `.gitignore` lines (`INPUTS_CL29_VARN/`, `OUTPUTS_CL29_VARN/`); Test `tests/python/test_make_varn_inputs.py`.

**The generator's full job (review-corrected):** copy setup; `PELAGIC_INPUTS.txt`: declared total 36→37, variable-table row 33 `CYN_N` (transported like CYN_C), metabolite rows renumbered 34–37, **every per-box IC block** gains a var-33 row `0.220·(CYN_C IC)`, **and the per-state settling plumbing for var 33** (settling-velocity / dissolved-fraction / deposited-fraction rows mirroring CYN_C — the solver consumes them for any `SETTLING_ON` state; enumerate the blocks by reading the live file's per-box sections); `FORC_TS_*.txt`: **update the `NUMBER_OF_VARIABLES` header 36→37 AND the scale-factor row gains a column** (the review-found real header structure: DATA_SIZE / NUMBER_OF_VARIABLES / SCALE FACTORS lines — the fixture must reproduce all of them), data column 33 = `0.220 ×` column 15, metabolite columns shifted; options file: `CYN_VARIABLE_N 1` + the four scalars **INSERTED at the Task-2 read position** (after the staging pairs, before the `CYN_ALLELOPATHY_FILE_NAME` lines — NOT appended at EOF, which the graceful reader would silently default away; fixture test asserts the position).

- [ ] Fixture-based TDD (fixture = miniature file set WITH the real header structures) → implement → run on the real setup, sanity: `FORC_TS_1.txt` data rows have 38 fields, header says 37, options parse echoes ON with four values (run the varN binary 1 day to check) → commit.

### Task 6: Validator TN fix + VARN checker + the conservation scenario

**Files:** Modify `tools/validate_cl29_vs_epa.py` (TN's CYN term uses the `CYN_N` column when present; else legacy `N_TO_C·CYN_C`); Create `tools/check_varn_run.py`: `--mode smoke` (echo values == options file; `CYN_N` column present; transport-flag echo line shows slot 33 == 1; `Q ∈ [0.095, 0.255]` **excluding samples where `CYN_C ≤ 2·MIN_CONCENTRATION` — the review's floor-artifact exemption — and reporting, not asserting, Q excursions in high-flush boxes since Q is not conservative under mixing**); `--mode conserve` (see scenario); `--mode nbudget` (the per-term CYN N-budget printout the spec's honesty items require: uptake, each Q-weighted loss, DON-sink residual — from PROCESS_RATES columns when available, else state-difference estimates, labeled).
**The conservation scenario (review fix — a raw pool sum cannot close on any full-biology run):** Task 6 also creates, via the generator plus an option, a **CYN-only degenerate VARN scenario** (`--degenerate-cyn`): all other phyto/zoo ICs and boundary columns zeroed, NOST staging OFF, allelopathy retained (inert). On this scenario `Δ(DIN+CYN_N+DON+DET_N)` closes (no zoo, single box preferred: use box-1-only or accept whole-domain with zero boundary — the generator zeroes boundary inflow concentrations for the involved pools; the checker documents exactly which identity it closes and at what tolerance, 1e-9 relative).

- [ ] TDD all three modes + the degenerate generator option → commit.

### Task 7: Verification battery + science ladder

- [ ] **Order:** admissibility gate (must pass; paste output) → V2 byte-identity: 0D golden, CL29 30-day, **and the full-record standard A/B: build `main` in a scratch worktree (`git worktree add /tmp/varn_mainwt main && (cd /tmp/varn_mainwt && make build-estas)`), run BOTH binaries (distinct paths, `md5sum` printed and asserted different) on the same scratch full-record input, `diff -r`, remove the worktree** → V3 VARN smoke (checker; loud-stop cross-check) → V4 conservation on the degenerate-CYN scenario (`--mode conserve`) → V5 Euler-vs-RK2 90-day VARN (no NaN; Q-bounds report; conserve on the degenerate scenario under both solvers).
- [ ] **Science ladder (spec §7):** full-record VARN; score with the fixed validator; [a] obs-matched monthly CYN_C vs 0.8/0.4 thresholds; [b] headline deltas vs CHLA 24.05 / PO4 0.0170 / r +0.68; [c] the quota signature (monthly mean Q: June ≥ 0.9·Q_MAX band, August drawdown below mid-band — else mechanistic refutation); [d] sensitivity: VMAX 0.22, VMAX 0.88, Q_MAX 0.30 (three full-record runs); `--mode nbudget` on the main run.
- [ ] **Documentation:** doc §38 carrying, verbatim from the spec, the three-sub-delta A/B statement and the FIX-3 bias statement alongside the results; BACKLOG row; the adoption/NULL question presented with numbers. Commit `docs(cl29): s.38 -- Droop-N VARN ladder results`.

---

## Review record (r2)

Adversarial plan review 2026-08-30 (4 finders → refute-oriented verification, 14 agents,
23 findings): **10 confirmed, 0 refuted**, 5 unverified majors, 8 minors — all folded into
this r2: transport-switch initialization + settled-N booking (the two silent-corruption
blockers), the duplicate DON-sink site in the live NOSTOCALES branch, the un-passable raw-sum
conservation gate → the degenerate-CYN scenario, the orphan setter → module moved to Task 2,
the T3/T4 merge (implicit-interface UB between commits), the per-step DIN cap step, the
honesty/reporting steps, the generator's insert-position and real FORC header structure, the
exact-expression test values (no rounded decimals at 1e-10), grazing as a test-injected input,
the self-compare-proof md5 step, both metabolite-literal sites, the 0D-path nstate clarification
(0D stays 32-state; the conservation vehicle is the degenerate scenario), the Q-bounds floor
exemption and mixing caveat, and the residency/gitignore decisions.
