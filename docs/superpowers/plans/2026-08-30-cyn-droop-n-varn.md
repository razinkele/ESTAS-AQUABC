# CYN Droop-N (VARN build variant) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the opt-in CYN nitrogen-quota (Droop) mechanism as a compile-variant `VARN` build (`nstate=33`), with the admissibility-gated constants, the transport-free 0D conservation vehicle, and the falsifiable August science ladder.

**Architecture:** `CYN_N` becomes transported state 33 in a separate `ESTAS_II_varN` binary (trap-guarded in-place `nstate` patch at build time); the kinetics gain a flag-gated Droop branch in BOTH `CYANOBACTERIA` variants (quota limitation replaces the ambient-Monod N term; explicit VMAX uptake replaces the implicit growth-coupled DIN/DON sinks); the model routes all CYN nitrogen Q-weighted. Constants ride five graceful options lines; a committed desk-gate tool enforces the spec's admissibility identity before any run. The standard build stays byte-identical.

**Tech Stack:** Fortran 2008 (gfortran), tests/fortran harness, python3+numpy tools, the existing 0D example as the conservation vehicle.

**Spec:** `docs/superpowers/specs/2026-08-30-cyn-droop-n-rescoped-design.md` — normative for every constant, gate, and invariant below. Its inherited base `docs/superpowers/specs/2026-08-01-variable-stoichiometry-cyn-droop-n-design.md` §3/§12 is background only.

## Global Constraints

- Standard `ESTAS_II` build **byte-identical** (0D golden + CL29 30-day A/B vs a Step-0 baseline); every new code path behind `CYN_VARIABLE_N > 0`; the tracked `mod_GLOBAL.f90` `nstate` line is patched ONLY transiently by the varn build target (trap-restored, `git diff --exit-code` proven).
- Committed constants exactly (spec §2): `CYN_VARIABLE_N 0`, `CYN_N_QMIN 0.10`, `CYN_N_QMAX 0.25`, `CYN_N_VMAX 0.44`, `CYN_N_KHS_UPT 0.003`; `Q_SEED = CYN_N_TO_C = 0.220` (verify against WCONST #47 before use).
- The admissibility gate (spec §2) must PASS at the committed constants before the science ladder runs: August `LIM_N*` (M=0.571, μ_max_eff=0.468) > 0.571 and June `Q*` approaching `Q_MAX`.
- Units: `CYN_N` mg N/L; quota gN/gC; all routing Q-weighted; O2 stoichiometry unchanged (C-coupled); reported chlorophyll stays fixed C:Chl 78.
- Conservation is asserted at the LIB-RATE level (unit) and on the transport-free 0D path (integration); never as a per-box water-column identity.
- The flag on a standard (`nstate=32`) binary must `error stop` at options-read time.
- Fortran house style, 132-col, no tabs; commits `feat(varn): ...`/`test(varn): ...` ending: Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>
- NEVER modify the live `INPUTS_CL29/` or `INPUT_CL29.txt`; the VARN setup is generated into `INPUTS_CL29_VARN/`.

---

### Task 1: The admissibility-gate tool

**Files:**
- Create: `tools/droop_gate.py`
- Test: `tests/python/test_droop_gate.py` (follow the layout of the existing tests/python files; if no runner exists there, executable-module self-test invoked by the step)

**Interfaces (Produces):** `python3 tools/droop_gate.py --kg 2.0 --ftemp 0.78 --flight 0.30 --din 0.004 --khs 0.003 --qmin 0.10 --qmax 0.25 --vmax 0.44 [--m-override]` → prints `Q*`, `LIM_N*`, the June leg (`--din 0.022 --ftemp 0.63 --flight 0.33`), and exits 0 iff BOTH gate conditions hold (August LIM_N* > 0.571; June Q* ≥ 0.8·(Q_MAX−Q_MIN)+Q_MIN). Solves `VMAX·M·(Q_MAX−Q) = KG·ftemp·flight·Q·(Q−Q_MIN)` by bisection on [Q_MIN, Q_MAX].

- [ ] **Step 1: failing test** — assert: committed constants pass both legs (August LIM_N* in [0.70, 0.75]; June Q* ≥ 0.221−ε); the spec's rejected VMAX 0.06 FAILS (LIM_N* ≈ 0.33, exit 1); the M=1 upper-bound short-circuit fires for VMAX 0.06 (LIM_N*(M=1) < 0.571 → fail fast).
- [ ] **Step 2: run, expect failure** (module missing).
- [ ] **Step 3: implement** — bisection solver + the two gate legs + `--json` output; docstring carries the spec §2 identity verbatim.
- [ ] **Step 4: run tests green**; run the tool once at committed values and paste the output into the commit message body.
- [ ] **Step 5: commit** — `feat(varn): droop_gate admissibility tool (spec s.2 identity)`

### Task 2: Options lines, flag global, mis-pair guard, baseline

**Files:**
- Modify: `SOURCE_CODE/ESTAS/mod_GLOBAL.f90` (~:207 region — `integer :: CYN_VARIABLE_N = 0` next to the other option globals; `nstate` at :16 is NOT touched here)
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` `READ_PELAGIC_MODEL_OPTIONS` (five graceful pairs AFTER the staging block's `V_SETTLE_AKI` pair, BEFORE the `CYN_ALLELOPATHY_FILE_NAME` lines; same `end=900,err=900` pattern)

**Interfaces (Produces):** globals `CYN_VARIABLE_N` (int) and module-level `CYN_N_QMIN/QMAX/VMAX/KHS_UPT` reals (declare beside the staging `_IN` locals, pass to a `SET_CYN_DROOP_PARAMS(qmin,qmax,vmax,khs)` setter in the svindex-adjacent module of Task 3 — exact name binding for Tasks 3/4).

- [ ] **Step 0: byte-identity baseline** — with the CURRENT `main` build: run the 0D golden and a scratch CL29 30-day (`cp INPUT_CL29.txt /tmp/varn_ab/INPUT_AB.txt`, `SIMULATION_END` 30.0, quoted scratch output dir, run from repo root with `ESTAS_HOLD_VOLUME=1`), save to `/tmp/varn_ab/baseline/`.
- [ ] **Step 1: reads + defaults + echo** — five pairs; defaults set before the reads (`0`, `0.10`, `0.25`, `0.44`, `0.003`); ON-echo prints all four scalars (the pair-swap defense); OFF-echo one line.
- [ ] **Step 2: the guard** — immediately after the reads resolve: `if (CYN_VARIABLE_N > 0 .and. nstate /= 33) error stop 'CYN_VARIABLE_N=1 requires the VARN build (nstate=33); this binary has nstate=32'` (`nstate` is in scope via GLOBAL — verify with the :31 usage).
- [ ] **Step 3: gates** — `make build-estas`; CL29 30-day rerun `diff -r` clean vs baseline + `NOST staging:` and the new OFF echo both present; suite green.
- [ ] **Step 4: commit** — `feat(varn): CYN_VARIABLE_N flag + five graceful option lines + nstate mis-pair guard`

### Task 3: The Droop branch in both CYANOBACTERIA variants + lib-level tests

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_svindex.f90` — `integer, parameter :: CYN_N_INDEX = 33` with a comment that it is live only in the VARN build (metabolites are already `nstate`-derived at mod_PELAGIC_ECOLOGY:739–745 — verify, do not touch)
- Create: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_cyn_droop.f90` — small module: the four scalars + `SET_CYN_DROOP_PARAMS` + pure helpers `F_DOWN(Q)`, `LIM_N_QUOTA(Q)`, `R_UPTAKE(DIN,Q,CYN_C)` implementing spec §2 verbatim
- Modify: `…AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_CYANOBACTERIA.f90` — BOTH `CYANOBACTERIA` and `CYANOBACTERIA_BOUYANT` (the CL29 path is BOUYANT — verify `call CYANOBACTERIA_BOUYANT` at model.f90:1134 and `CYANO_BOUYANT_STATE_SIMULATION 1` in the live options): new args `CYN_VARIABLE_N` (int), `CYN_N` (in, nkn), `R_CYN_N_UPTAKE` (out, nkn); under the flag `LIM_KG_CYN_N = LIM_N_QUOTA(Q)` with `Q = CYN_N/max(CYN_C,1e-10)` and `R_CYN_N_UPTAKE = R_UPTAKE(...)`; flag off: legacy verbatim, new outs zeroed
- Create: `tests/fortran/test_cyn_droop.f90` + Makefile target + TEST_PROGS registration (link `aquabc_cyn_droop.o` + the CYANOBACTERIA lib chain like `test_cyanobacteria` does)

**Interfaces (Produces):** the module helpers above; the extended lib signatures (append after each variant's current last arg — record the exact final arg orders in the report for Task 4).

- [ ] **Step 1: failing tests** — cases: (1) `F_DOWN` endpoints (0 at Q_MAX, 1 at Q_MIN); (2) `LIM_N_QUOTA` clamp at both ends; (3) uptake arithmetic at committed constants: DIN 0.004, Q 0.15, CYN_C 1.0 → `0.44·0.571·(0.10/0.15)·1.0 = 0.1675` mg N/L/d (hand value, 1e-10); (4) flag=0 through the lib routine: legacy `LIM_KG_CYN_N` (ambient Monod) unchanged for a reference input, new outs zero; (5) flag=1: the rate-level N balance — `uptake − (resp+death+excr+graze)·Q` equals the quota-pool net the routine's outputs imply, to 1e-12 (the V1 conservation vehicle); (6) the DON-sink invariant: under the flag the routine's DON-uptake output term is exactly zero.
- [ ] **Step 2: run, expect compile failures.**
- [ ] **Step 3: implement** module + both lib branches. The BOUYANT variant's branch is IDENTICAL logic (copy, not shared helper call only if argument plumbing differs — prefer both calling the module helpers so the physics lives once).
- [ ] **Step 4: full suite green; build-estas clean; 30-day A/B still byte-identical** (flag off zeroes everything; the new args are wired in Task 4 — until then the lib is compiled but its new dummies unreferenced by the model: to keep the build linking, Task 3 adds the args ONLY to the lib and its tests; the model call-site change is Task 4's first step, so Task 3's build gate is `make -C tests/fortran` only, NOT build-estas — state PASS/FAIL accordingly).
- [ ] **Step 5: commit** — `feat(varn): Droop quota module + flag-gated branch in both CYANOBACTERIA variants (lib-level, TDD)`

### Task 4: Model wiring — derivative, Q-weighted routing, DON-sink zero, flag threading

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90`: extend both CYANOBACTERIA call sites (:1134 BOUYANT; find the plain variant's call) with the three new args; declare `R_CYN_N_UPTAKE(nkn)`; under the flag: `DERIVATIVES(:,CYN_N_INDEX) = uptake − (R_CYN_TOT_RESP+R_CYN_DEATH+R_CYN_EXCR+R_ZOO_FEEDING_CYN)·Q` via new PROCESS_RATES slots; replace `CYN_N_TO_C` with `Q` at the seven routing sites (NH4 resp :2040, NH4 uptake :2052→`R_CYN_N_UPTAKE·PREF_NH4N_CYN`, NO3 uptake :2131→`R_CYN_N_UPTAKE·(1−PREF_NH4N_CYN)`, DON excr :3015, ZOO_N :2772, DET_N CYN-death site — locate with `grep -n "R_CYN_DEATH.*CYN_N_TO_C"`, and ZERO the DON growth-sink slot 5 :3017–3018) — every replacement flag-gated so flag=0 is verbatim legacy; O2 site :2242 untouched (assert in review)
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` (thread flag + CYN_N slice into `AQUABC_PELAGIC_KINETICS`'s call), `aquabc_II_pelagic_interface.f90` (literal 0 + a zero-filled CYN_N actual for the 0D path — mirror how the staging args were handled there), `SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90` count assert (~:179/:194–207): replace with the spec §3.2 single condition `declared_total == nstate + merge(4,0,CONSIDER_ALLELOPATHY>0)`
- Modify: the negative-mass diagnostic literal `33..36` in mod_PELAGIC_ECOLOGY (locate with `grep -n "33" mod_PELAGIC_ECOLOGY.f90 | grep -v nstate` and read context) → `nstate+1 .. nstate+NUM_ALLOLOPATHY_STATE_VARS`
- Modify: `tests/fortran/test_cyanobacteria.f90` (signature update)

**Interfaces:** Consumes Task 3's exact signatures. Produces the full flag-on wiring Tasks 5–8 run.

- [ ] **Step 1** extend test_cyanobacteria for the new signature (flag=0 pass-through cases), expect compile fail; **Step 2** implement the wiring above; **Step 3** suite green, `make build-estas` clean, 0D golden + CL29 30-day A/B byte-identical vs baseline; **Step 4** commit `feat(varn): CYN_N wiring -- Q-weighted routing, DON-sink zeroed under flag, count assert per spec s.3.2`.

### Task 5: The `build-estas-varn` target

**Files:** Modify `Makefile` (after `build-estas` at :205):

```make
build-estas-varn:
	@cp SOURCE_CODE/ESTAS/mod_GLOBAL.f90 /tmp/.mod_GLOBAL.varn.bak
	@trap 'cp /tmp/.mod_GLOBAL.varn.bak SOURCE_CODE/ESTAS/mod_GLOBAL.f90' EXIT; \
	 sed -i 's/:: nstate *= 32/:: nstate                        = 33/' SOURCE_CODE/ESTAS/mod_GLOBAL.f90 && \
	 $(MAKE) build-estas && mv ESTAS_II ESTAS_II_varN
	@cp /tmp/.mod_GLOBAL.varn.bak SOURCE_CODE/ESTAS/mod_GLOBAL.f90
	@git diff --exit-code SOURCE_CODE/ESTAS/mod_GLOBAL.f90
	@$(MAKE) build-estas   # restore the standard binary
```
(adjust the sed to the exact :16 spacing — verify first; the trailing standard rebuild leaves `ESTAS_II` = standard.)

- [ ] **Step 1** verify the sed matches exactly one line; **Step 2** implement; **Step 3** gates: `make build-estas-varn` → `ESTAS_II_varN` exists, `git status` clean, `ESTAS_II` still standard (rerun the 30-day A/B byte-identical), and the guard fires: running the STANDARD binary on a flag=1 options copy aborts with the Task-2 message; **Step 4** commit `feat(varn): build-estas-varn target (trap-guarded in-place nstate patch + cleanliness proof)`.

### Task 6: The VARN setup generator

**Files:** Create `tools/make_varn_inputs.py`; Create `INPUT_CL29_VARN.txt` (from `INPUT_CL29.txt`: input folder `INPUTS_CL29_VARN/`, output `OUTPUTS_CL29_VARN/`); Test `tests/python/test_make_varn_inputs.py`.

**Produces:** `python3 tools/make_varn_inputs.py --src INPUTS_CL29 --dst INPUTS_CL29_VARN --qseed 0.220` — copies the setup, then: `PELAGIC_INPUTS.txt` variable table gains row 33 `CYN_N` (transported like CYN_C) and renumbers the 4 metabolite rows to 34–37 with declared total 36→37; every per-box IC block gains a var-33 row with `IC = 0.220·(CYN_C IC)`; `FORC_TS_*.txt` boundary files gain column 33 = `0.220 × column(CYN_C=15)` with the metabolite columns shifted right; options file gets `CYN_VARIABLE_N 1` + the four scalars appended (staging block preserved).

- [ ] **Step 1: failing tests** — on a MINIATURE synthetic 36-var fixture (committed under tests/python/fixtures/): column counts 37 everywhere after generation; `CYN_N` boundary column == 0.220×CYN_C column to 1e-12; metabolite rows/columns shifted not duplicated; idempotence guard (running on a 37-var dst refuses).
- [ ] **Steps 2–4** implement, tests green, then run on the real setup and sanity-print the generated `PELAGIC_INPUTS.txt` header + `FORC_TS_1.txt` field count (must be 38 = time+37).
- [ ] **Step 5** commit `feat(varn): 36->37-var setup generator + VARN driver input`.

### Task 7: Validator TN fix + VARN checker

**Files:** Modify `tools/validate_cl29_vs_epa.py` (in `add_derived` ~:107: when a `CYN_N` column exists, TN's CYN contribution uses it instead of `N_TO_C·CYN_C`; other groups unchanged — note `N_TO_C` is the shared 0.22 at :46); Create/extend a checker (`tools/check_varn_run.py`): `--mode smoke` (flag echo with four values == options file; `CYN_N` column present; `Q∈[0.095,0.255]` everywhere allowing 5 % numerics margin) and `--mode conserve0d` (the 0D run's full-series identity `Δ(DIN+CYN_N+DON+DET_N+ZOO_N) == 0` within 1e-9 relative — the transport-free vehicle).

- [ ] TDD both; commit `feat(varn): validator CYN_N-aware TN + VARN checker (smoke, 0D conservation)`.

### Task 8: Verification battery + the science ladder

- [ ] **V-gate order:** admissibility gate (Task 1 tool at committed values — must pass; paste output); V2 byte-identity (0D golden + CL29 30-day + the FULL-record standard A/B vs a `main`-built binary via a scratch worktree build, the staging-arc pattern); V3 VARN smoke (generated setup + `ESTAS_II_varN`, 30-day, checker `--mode smoke`; loud-stop cross-check per Task 5); V4 0D conservation: build a 0D VARN variant run (the interface path with flag on — the 0D driver's own nstate=32 literal at interface:74/:122 must also be patched by the varn target — VERIFY this during Task 5 and extend the patch if the 0D example is built from the same tree; if the 0D example cannot run VARN without its own variant, the conservation vehicle is the 0D-style single-box CL29 degenerate run instead: document which) then `--mode conserve0d`; V5 Euler-vs-RK2 90-day VARN (no NaN, Q in bounds, conservation via the same checker under both).
- [ ] **Science ladder (spec §7):** full-record VARN run (adopted config + generator setup); score with the fixed validator (obs-matched monthly CYN_C; the [a] estimator = the same monthly table every arc has used); report [a] vs the 0.8/0.4 thresholds, [b] headline deltas vs CHLA 24.05/PO4 0.0170/r +0.68, [c] the quota seasonal signature (monthly mean Q from CYN_N/CYN_C: June ≥ 0.9·Q_MAX and August drawdown below mid-band, else mechanistic refutation), [d] sensitivity: VMAX 0.22 and 0.88, Q_MAX 0.30 (three more full-record runs, ~15 min each).
- [ ] **Documentation:** doc §38 (results whichever way), BACKLOG row, adoption/NULL question presented to the user with the numbers. Commit `docs(cl29): s.38 -- Droop-N VARN ladder results`.

---

## Self-review notes (completed)

- Spec coverage: §1 premise→T1 gate + T8 order; §2 formulation→T3 (helpers+branch) + T4 (routing) + constants→T2; §3.1→T5, §3.2→T4 (assert) + T6 (37 declaration), §3.3→T3 (both variants, BOUYANT verified), §3.4→T2 (guard) + T5 (cross-check), §3.5→T6; §5 table fully mapped incl. validator fix (T7) and the metabolite-literal fix (T4); §6 V1→T3 tests, V2/V3/V4/V5→T8 (V4's 0D-nstate caveat surfaced as an explicit verify-and-decide step, not silently assumed); §7→T8; §8 risks: the A/B bundle honesty and 0D vehicle both carried.
- Type consistency: `SET_CYN_DROOP_PARAMS(qmin,qmax,vmax,khs)` bound in T2/T3; the three new lib args named identically in T3/T4; checker mode names consistent in T7/T8.
- Known open verify-steps (deliberate): exact final arg order of both lib variants (T3 records for T4); the DET_N CYN-death site line; the 0D-path nstate question in V4; the negative-mass literal's exact line.
