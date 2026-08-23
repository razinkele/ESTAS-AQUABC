# NOST Akinete Staging Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the opt-in `NOST_STAGE_MODEL=1` akinete life-cycle staging: a benthic akinete bank with growth-viability-gated germination and an EMA-filtered declining-radiation formation cue, byte-identical at flag=0.

**Architecture:** A new module `AQUABC_NOST_STAGING` holds per-box non-transported state (`BED_AKI`, `RAD_EMA`, `FORM_LATCH`) plus single-slot flux-export arrays written by the NOSTOCALES kinetics and integrated ONCE per completed solver step by a solver-agnostic `ADVANCE_NOST_STAGING` routine (the RK2 path averages its two stage exports exactly as it already averages K1/K2). Configuration arrives via six graceful `PELAGIC_MODEL_OPTIONS.txt` lines — no WCONST/`nconst` change anywhere.

**Tech Stack:** Fortran 2008 (gfortran), existing `tests/fortran` unit-test harness (`make -C tests/fortran test`), 0D golden gate, CL29 A/B via `ESTAS_II`.

**Spec:** `docs/superpowers/specs/2026-08-23-nost-akinete-staging-design.md` — read it first; every numeric default and gate below is normative there.

## Global Constraints

- Flag=0 must be **byte-identical** on the 0D golden and a CL29 short A/B (spec §7 V2). Every new code path must be behind `NOST_STAGE_MODEL > 0` (an `if` around new work is fine; computing-then-discarding is not, because of FP environment effects).
- `BED_AKI` is **g C/m²**; water concentrations are g C/m³ (≡ mg/L); `V_SETTLE_AKI` in m/d. No hidden 1e3 factors (spec §4.1).
- No `Date`/wall-clock use; model time only.
- Defaults (spec §4.5): `NOST_STAGE_MODEL=0`, `T_GERM_AKI_STAGE=12.0`, `I_FORM_AKI=120.0`, `KR_GERM_BED=0.05`, `K_MORT_BED_AKI=0.001`, `V_SETTLE_AKI=0.5`. Hardcoded module parameters: `EPS_GERM_TEMP_LIM=0.05`, `TAU_RAD_EMA_DAYS=7.0`.
- New option lines go **after `W_DISP_POS`, before the `CYN_ALLELOPATHY_FILE_NAME` lines** in `PELAGIC_MODEL_OPTIONS.txt`; the reader keeps the `end=900/err=900` graceful pattern (mod_PELAGIC_ECOLOGY.f90 ~1148–1200).
- Commit after every task with the house style (`feat(staging): ...` / `test(staging): ...`), each ending `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.
- Fortran style: match the AQUABC codebase (upper-case names, `DBL_PREC`, 132-col limit, no tabs).

---

### Task 1: The `AQUABC_NOST_STAGING` module, unit-tested

**Files:**
- Create: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_nost_staging.f90`
- Create: `tests/fortran/test_nost_staging.f90`
- Modify: `tests/fortran/Makefile` (new test target, mirror `test_cyanobacteria`)
- Modify: the library build file list — find it with `grep -rn "aquabc_positioning_state" SOURCE_CODE/build/ Makefile` and add `aquabc_nost_staging.f90` in the same list(s)/pass, immediately after the positioning module.

**Interfaces (Produces — later tasks rely on these exact names):**
```fortran
! module AQUABC_NOST_STAGING (use AQUABC_II_GLOBAL for DBL_PREC)
real(kind=DBL_PREC), allocatable :: BED_AKI(:)      ! g C/m2
real(kind=DBL_PREC), allocatable :: RAD_EMA(:)      ! W/m2
logical,             allocatable :: FORM_LATCH(:)
real(kind=DBL_PREC), allocatable :: BURIED_AKI(:)   ! g C/m2, cumulative (V4 audit)
! exact cumulative integrals, updated ONLY in ADVANCE_NOST_STAGING (V4/V6 audit trail —
! sampled instantaneous columns cannot close a conservation identity; these close it exactly):
real(kind=DBL_PREC), allocatable :: CUM_SETTLE_AKI(:)  ! g C/m2
real(kind=DBL_PREC), allocatable :: CUM_GERM_AKI(:)    ! g C/m2
real(kind=DBL_PREC), allocatable :: CUM_FORM_AKI(:)    ! g C/m2 (diagnostic only, no bed effect)
! single-slot kinetics exports, OVERWRITTEN on every kinetics evaluation:
real(kind=DBL_PREC), allocatable :: STG_SETTLE_FLUX(:)  ! g C/m2/d
real(kind=DBL_PREC), allocatable :: STG_GERM_FLUX(:)    ! g C/m2/d
real(kind=DBL_PREC), allocatable :: STG_FORM_FLUX(:)    ! g C/m2/d  (= R_FORM_NOST_AKI*DEPTH)
logical,             allocatable :: STG_GERM_COND(:)    ! non-latch germ conditions met
real(kind=DBL_PREC) :: T_GERM_AKI_STAGE = 12.0D0
real(kind=DBL_PREC) :: I_FORM_AKI       = 120.0D0
real(kind=DBL_PREC) :: KR_GERM_BED      = 0.05D0
real(kind=DBL_PREC) :: K_MORT_BED_AKI   = 1.0D-3
real(kind=DBL_PREC) :: V_SETTLE_AKI     = 0.5D0
real(kind=DBL_PREC), parameter :: EPS_GERM_TEMP_LIM = 0.05D0
real(kind=DBL_PREC), parameter :: TAU_RAD_EMA_DAYS  = 7.0D0

subroutine ENSURE_NOST_STAGING_STATE(n)          ! serial-context allocate, zero-init; RAD_EMA init sentinel -1
subroutine RESET_NOST_STAGING_STATE()            ! unit tests: zero everything, latch .false., RAD_EMA sentinel
subroutine SET_NOST_STAGING_PARAMS(tgerm, iform, krgerm, kmort, vsettle)
subroutine ADVANCE_NOST_STAGING(n, dt_days, solar_rad, f_settle, f_germ, f_form, germ_cond)
    ! integrates BED_AKI + BURIED_AKI + the three CUM_* integrals, updates RAD_EMA
    ! (first call: RAD_EMA = solar_rad), then the latch: ON when RAD_EMA < I_FORM_AKI;
    ! OFF when germ_cond(k) — spec §4.3. f_form feeds ONLY CUM_FORM_AKI (never the bed).
    ! Caller passes the (possibly stage-averaged) fluxes; module is solver-agnostic.
```

`ADVANCE_NOST_STAGING` body (the exact integration, spec §4.2/§4.4):
```fortran
do k = 1, n
    mort       = K_MORT_BED_AKI * BED_AKI(k)
    BED_AKI(k) = BED_AKI(k) + (f_settle(k) - f_germ(k) - mort) * dt_days
    BURIED_AKI(k) = BURIED_AKI(k) + mort * dt_days
    CUM_SETTLE_AKI(k) = CUM_SETTLE_AKI(k) + f_settle(k) * dt_days
    CUM_GERM_AKI(k)   = CUM_GERM_AKI(k)   + f_germ(k)   * dt_days
    CUM_FORM_AKI(k)   = CUM_FORM_AKI(k)   + f_form(k)   * dt_days
    if (RAD_EMA(k) < 0.0D0) then
        RAD_EMA(k) = solar_rad(k)                       ! first-call init
    else
        RAD_EMA(k) = RAD_EMA(k) + (dt_days / TAU_RAD_EMA_DAYS) * (solar_rad(k) - RAD_EMA(k))
    end if
    if (FORM_LATCH(k)) then
        if (germ_cond(k)) FORM_LATCH(k) = .false.       ! spring release, spec §4.3
    else
        if (RAD_EMA(k) < I_FORM_AKI) FORM_LATCH(k) = .true.
    end if
end do
```
No positivity clamp: with `KR_GERM_BED*dt ≪ 1` the pool stays non-negative analytically, and a clamp would break V4 conservation auditing (assert non-negativity in the test instead).

- [ ] **Step 1: Write the failing tests** — `tests/fortran/test_nost_staging.f90`, a standalone program in the style of `tests/fortran/test_cyanobacteria.f90` (assert helpers, exit code). Cases:

```fortran
! 1. mass balance: n=1, BED=10, f_settle=2, f_germ=1, dt=0.5
!    -> BED = 10 + (2-1-0.001*10)*0.5 = 10.495; BURIED = 0.005
! 2. conservation identity: delta(BED) + delta(BURIED) == (f_settle - f_germ)*dt  to 1d-12
! 3. EMA: first call sets RAD_EMA=solar exactly; second call with solar=0, dt=7
!    -> RAD_EMA halves-ish: 120 + (7/7)*(0-120) = 0 (full step); use dt=1: 120*(6/7)
! 4. latch ON: RAD_EMA driven below I_FORM_AKI -> FORM_LATCH .true.;
!    stays ON while germ_cond=.false. even if RAD_EMA rises again
! 5. latch OFF: germ_cond=.true. on next ADVANCE -> .false.
! 6. non-negativity: BED=1e-6, f_germ=KR_GERM_BED*1e-6, 240 steps of dt=1/240 -> BED >= 0
! 7. SET_NOST_STAGING_PARAMS overrides all five scalars
! 8. exact bed identity after N random-flux ADVANCE calls:
!    BED - BED0 == CUM_SETTLE - CUM_GERM - BURIED   to 1d-12  (the V4 identity)
! 9. CUM_FORM accumulates f_form*dt and BED is unaffected by f_form
```

- [ ] **Step 2: Run to verify failure** — `make -C tests/fortran test_nost_staging` → compile error (module missing). Add the target first: copy the `test_cyanobacteria` block in `tests/fortran/Makefile`, link `aquabc_nost_staging.o` + `mod_AQUABC_II_GLOBAL.o`, **and add `test_nost_staging` to the `TEST_PROGS` list** (otherwise `make test` silently skips it — verify with `grep -n "TEST_PROGS" tests/fortran/Makefile`).
- [ ] **Step 3: Write the module** — full file per the Interfaces block above, with the header comment pointing at the spec path.
- [ ] **Step 4: Confirm the library build picks it up** — the build may auto-discover sources rather than use an explicit list: run `make build-estas`, then `grep -l "aquabc_nost_staging" SOURCE_CODE/build/` artifacts or `nm` the library for the module symbol. Only if discovery fails, find and extend the compile list (`grep -rn "aquabc_positioning_state" SOURCE_CODE/build/make_lib.sh Makefile`) placing the new file in the same pass as the positioning module.
- [ ] **Step 5: Run tests** — `make -C tests/fortran clean test` → all pass including the new one.
- [ ] **Step 6: Commit** — `feat(staging): AQUABC_NOST_STAGING module (bed akinete bank, EMA cue, latch) + unit tests`

### Task 2: Flag + graceful option lines + setter call

**Files:**
- Modify: `SOURCE_CODE/ESTAS/mod_GLOBAL.f90:207` area — add `integer :: NOST_STAGE_MODEL = 0` next to `CYANO_POS_MODEL`.
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` `READ_PELAGIC_MODEL_OPTIONS` (~1148–1200).

**Interfaces:** Produces the global `NOST_STAGE_MODEL` (integer, `use GLOBAL`) that Tasks 3–5 read, and calls Task 1's `SET_NOST_STAGING_PARAMS`.

- [ ] **Step 0: Create the byte-identity baseline (used by every later A/B gate)** — BEFORE any Task 2 code change, with the current `main`-built binary: run the 0D golden, and run a scratch CL29 30-day setup (`cp INPUT_CL29.txt /tmp/stg_ab/INPUT_AB.txt`, edit `SIMULATION_END` to 30.0 and the output folder to a scratch dir, `ESTAS_HOLD_VOLUME=1 ./ESTAS_II /tmp/stg_ab/INPUT_AB.txt` — note the input/paths are relative to the repo root, run from there). Save both output sets to `/tmp/stg_ab/baseline/`. All flag=0 gates in Tasks 2–5 diff against THIS directory (`diff -r`), never against a same-commit rerun.
- [ ] **Step 1: Locals + defaults** — next to `K_POS_UP_IN = 3.0D0; ...` add:
```fortran
T_GERM_STG_IN = 12.0D0; I_FORM_IN = 120.0D0; KR_GERM_BED_IN = 0.05D0
K_MORT_BED_IN = 1.0D-3; V_SETTLE_IN = 0.5D0
```
(declare the five `real(kind=DBL)` locals with the routine's other `_IN` locals; `NOST_STAGE_MODEL` needs no local — it is the global, already defaulted 0.)
- [ ] **Step 2: Reads** — immediately AFTER the `W_DISP_POS_IN` read pair and BEFORE `900 continue`, six graceful pairs in this order (comment block explaining, matching house comment style):
```fortran
read(IN_FILE + 1, *, end = 900, err = 900)
read(IN_FILE + 1, *, end = 900, err = 900) NOST_STAGE_MODEL
read(IN_FILE + 1, *, end = 900, err = 900)
read(IN_FILE + 1, *, end = 900, err = 900) T_GERM_STG_IN
read(IN_FILE + 1, *, end = 900, err = 900)
read(IN_FILE + 1, *, end = 900, err = 900) I_FORM_IN
read(IN_FILE + 1, *, end = 900, err = 900)
read(IN_FILE + 1, *, end = 900, err = 900) KR_GERM_BED_IN
read(IN_FILE + 1, *, end = 900, err = 900)
read(IN_FILE + 1, *, end = 900, err = 900) K_MORT_BED_IN
read(IN_FILE + 1, *, end = 900, err = 900)
read(IN_FILE + 1, *, end = 900, err = 900) V_SETTLE_IN
```
- [ ] **Step 3: Setter + echo** — after `900 continue`, next to `call SET_POSITIONING_PARAMS(...)`:
```fortran
call SET_NOST_STAGING_PARAMS(T_GERM_STG_IN, I_FORM_IN, KR_GERM_BED_IN, K_MORT_BED_IN, V_SETTLE_IN)
if (NOST_STAGE_MODEL > 0) then
    write(*,*) 'NOST staging: ON. T_GERM=', T_GERM_STG_IN, ' I_FORM=', I_FORM_IN, &
               ' KR_GERM_BED=', KR_GERM_BED_IN, ' K_MORT_BED=', K_MORT_BED_IN, &
               ' V_SETTLE=', V_SETTLE_IN
else
    write(*,*) 'NOST staging: OFF (legacy akinete gates, default).'
end if
```
The five-value echo is deliberate: it is the only defense against a positional pair-swap in the
options file (V3 asserts these echoed values equal the file's — a same-magnitude swap would
otherwise pass every gate silently).
(add `use AQUABC_NOST_STAGING, only: SET_NOST_STAGING_PARAMS` next to the existing positioning `use`.)
- [ ] **Step 4: Build + reader regression** — `make build-estas`; then rerun the **scratch CL29 30-day setup from Step 0** (the 0D example does NOT call `READ_PELAGIC_MODEL_OPTIONS`, so it cannot exercise the reader — use it only as a compile/golden sanity check). The CL29 run's log must print `NOST staging: OFF` (its options file has no new lines → graceful defaults) and the outputs must be byte-identical to `/tmp/stg_ab/baseline/`.
- [ ] **Step 5: Commit** — `feat(staging): NOST_STAGE_MODEL flag + six graceful option lines`

### Task 3: Kinetics — gates, fluxes, exports, derivative wiring

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_NOSTACALES.f90` (signature + the transition block ~396–433)
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` (arg :83/:161 pattern for the new flag; NOSTOCALES call ~1285–1325; wiring ~2568/2617–2635)
- Modify: `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90:1545` region (pass flag into the model call) and `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90` (0D call: literal `0`, same as `CYANO_POS_MODEL` there)
- Modify: `tests/fortran/test_nostocales.f90` (signature update, flag=0 and flag=1 cases)

**Interfaces:**
- Consumes: Task 1's module state/exports; Task 2's `NOST_STAGE_MODEL` global.
- Produces: NOSTOCALES's extended signature — appended after `S_CHUNK`:
```fortran
NOST_STAGE_MODEL          , &   ! integer, intent(in)
BED_AKI_CHUNK             , &   ! real(nkn), intent(in)   g C/m2
FORM_LATCH_CHUNK          , &   ! logical(nkn), intent(in)
SETTLE_FLUX_CHUNK         , &   ! real(nkn), intent(out)  g C/m2/d
GERM_FLUX_CHUNK           , &   ! real(nkn), intent(out)  g C/m2/d
FORM_FLUX_CHUNK           , &   ! real(nkn), intent(out)  g C/m2/d (= R_FORM_NOST_AKI*env%DEPTH)
GERM_COND_CHUNK           , &   ! logical(nkn), intent(out)
R_GERM_BED_AKI            , &   ! real(nkn), intent(out)  g C/m3/d (VEG source)
R_SETTLE_AKI                    ! real(nkn), intent(out)  g C/m3/d (AKI_C sink)
```
and the model-side arrays `R_GERM_BED_AKI(nkn)`, `R_SETTLE_AKI(nkn)` (declared with the other `R_*` NOST arrays).

- [ ] **Step 1: Extend `tests/fortran/test_nostocales.f90` first** (it will fail to compile until the signature changes): flag=0 case asserts the four legacy rates are unchanged for a reference input; flag=1 cases assert (a) germination zero when `LIM_..._TEMP <= 0.05` even with DIN low and T>12 (the dead-water gate), (b) germination zero when `FORM_LATCH_CHUNK=.true.`, (c) `GERM_FLUX_CHUNK == KR_GERM_BED*BED_AKI_CHUNK` and `R_GERM_BED_AKI == GERM_FLUX_CHUNK/env%DEPTH` when all gates pass, (d) `R_GERM_NOST_AKI == 0` under the flag, (e) formation nonzero iff latch is ON (`R_FORM = KR_FORM_AKI*VEG`), (f) `SETTLE_FLUX_CHUNK == V_SETTLE_AKI*AKI_C` and `R_SETTLE_AKI == SETTLE_FLUX_CHUNK/env%DEPTH`, (g) `FORM_FLUX_CHUNK == R_FORM_NOST_AKI*env%DEPTH` when the latch is ON and 0 when OFF.
- [ ] **Step 2: Run** — `make -C tests/fortran test_nostocales` → compile FAIL (signature).
- [ ] **Step 3: Implement in the lib.** In the transition block replace the two `where` constructs with:
```fortran
if (NOST_STAGE_MODEL > 0) then
    ! germination: bed-only, growth-viability gated (spec s.4.2)
    GERM_COND_CHUNK = (DIN < KN_GERM_AKI) .and. &
                      (LIM_KG_NOST_VEG_HET_TEMP > EPS_GERM_TEMP_LIM) .and. &
                      (env%TEMP > T_GERM_AKI_STAGE)
    where (GERM_COND_CHUNK .and. .not. FORM_LATCH_CHUNK)
        GERM_FLUX_CHUNK = KR_GERM_BED * BED_AKI_CHUNK
    elsewhere
        GERM_FLUX_CHUNK = 0.0D0
    end where
    R_GERM_BED_AKI  = GERM_FLUX_CHUNK / env%DEPTH
    R_GERM_NOST_AKI = 0.0D0                       ! water-pool germination off
    ! formation: latch-driven (spec s.4.3); rate constant unchanged
    where (FORM_LATCH_CHUNK)
        AKI_FORM = KR_FORM_AKI
    elsewhere
        AKI_FORM = 0.0D0
    end where
    R_FORM_NOST_AKI = AKI_FORM * NOST_VEG_HET_C
    ! settling of water akinetes toward the bed
    SETTLE_FLUX_CHUNK = V_SETTLE_AKI * NOST_AKI_C
    R_SETTLE_AKI      = SETTLE_FLUX_CHUNK / env%DEPTH
    FORM_FLUX_CHUNK   = R_FORM_NOST_AKI * env%DEPTH   ! diagnostic export (CUM_FORM/V6 ratio)
else
    ! legacy block verbatim (both where constructs unchanged) + zero the new outs
    GERM_COND_CHUNK = .false.; GERM_FLUX_CHUNK = 0.0D0; SETTLE_FLUX_CHUNK = 0.0D0
    FORM_FLUX_CHUNK = 0.0D0
    R_GERM_BED_AKI = 0.0D0;    R_SETTLE_AKI = 0.0D0
end if
```
`use AQUABC_NOST_STAGING, only: KR_GERM_BED, V_SETTLE_AKI, T_GERM_AKI_STAGE, EPS_GERM_TEMP_LIM` (KN_GERM_AKI, KR_FORM_AKI stay params-sourced). Note `LIM_KG_NOST_VEG_HET_TEMP` is already computed above this block — verify with `grep -n "LIM_KG_NOST_VEG_HET_TEMP" <libfile>` that the assignment precedes it; if not, move the block below the assignment.
- [ ] **Step 4: Model wiring.** In `aquabc_II_pelagic_model.f90`: declare the two new `R_*` arrays; extend the NOSTOCALES call with `NOST_STAGE_MODEL, BED_AKI(ns:ne), FORM_LATCH(ns:ne), STG_SETTLE_FLUX(ns:ne), STG_GERM_FLUX(ns:ne), STG_FORM_FLUX(ns:ne), STG_GERM_COND(ns:ne), R_GERM_BED_AKI(ns:ne), R_SETTLE_AKI(ns:ne)` (`use AQUABC_NOST_STAGING` for the module arrays; call `ENSURE_NOST_STAGING_STATE(nkn)` where `ENSURE_POSITIONING_STATE` is called — find with grep); in the `else` (DO_NOSTOCALES=0) branch zero the two new arrays alongside the existing ones. Then the derivative assembly:
  - VEG source: `PROCESS_RATES(ns:ne,NOST_VEG_HET_C_INDEX, 6) = R_GERM_NOST_AKI + R_GERM_BED_AKI` (slot 6 is the germination source — legacy term is 0 under the flag, bed term is 0 without it; sum is exact in both modes).
  - AKI_C: add `PROCESS_RATES(ns:ne,NOST_AKI_C_INDEX, 6) = R_SETTLE_AKI(ns:ne)` (slot 5 is the DEPTH diagnostic — slot 6 is free; verify with `grep -n "NOST_AKI_C_INDEX, [0-9]" aquabc_II_pelagic_model.f90` that 6 is unused) and extend the derivative: `DERIVATIVES(...) = slot1 - slot2 - slot3 - slot4 - slot6`.
  - Pass the flag down: mod_PELAGIC_ECOLOGY:1545 call + model dummy (copy the `CYANO_POS_MODEL` pattern at :83/:161). The 0D interface call site needs actual arguments for ALL new dummies, not just the flag: pass literal `0` for the flag and, for the seven array/logical args, module arrays sized by `ENSURE_NOST_STAGING_STATE(nkn)` called at the interface init (flag 0 makes them inert; find the interface's `CYANO_POS_MODEL` literal with `grep -n "CYANO_POS_MODEL" aquabc_II_pelagic_interface.f90` and mirror how it handles `S_POS` there).
  - `tests/fortran/Makefile`: the `test_nostocales` target now links a lib that `use`s the staging module — add `aquabc_nost_staging.o` to its object list (same addition as Task 1 made for `test_nost_staging`).
- [ ] **Step 5: Tests + gates** — `make -C tests/fortran clean test` all green; `make build-estas`; 0D golden byte-identical; CL29 30-day A/B byte-identical: rerun the Task 2 Step 0 scratch setup and `diff -r` against `/tmp/stg_ab/baseline/`.
- [ ] **Step 6: Commit** — `feat(staging): growth-viability germination + latch formation + akinete settling in NOSTOCALES (flag-gated)`

### Task 4: Solver-side advance + resuspension guard

**Files:**
- Modify: `SOURCE_CODE/ESTAS/mod_SOLVER.f90` — Euler path (after the state update near :183–:239) and RK2 path (after the corrector near :428)
- Modify: the init/read path that already knows both flags for the guard — put it at the end of `READ_PELAGIC_MODEL_OPTIONS` if `RESUSPENSION_OPTION` is in scope there, else immediately after both are read in `mod_AQUATIC_MODEL` (find with `grep -n "RESUSPENSION_OPTION" SOURCE_CODE/ESTAS/mod_*.f90`)

**Interfaces:** Consumes Task 1's `ADVANCE_NOST_STAGING` + exports and Task 2's flag. Produces nothing new.

- [ ] **Step 1: Locate the per-box solar radiation at solver level** — `grep -n "SOLAR_RADIATION\|DRIVING_FUNCTIONS(" SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90 | head -20`. **RESOLVED during execution (Task 4): the index is `DRIVING_FUNCTIONS(3)`** — (1) is temperature, (2) is salinity, (3) is solar radiation (assignment block at mod_PELAGIC_ECOLOGY.f90 ~109–140, verified by direct reading). ⚠ The plan-review's "index 1 confirmed" verdicts were wrong; see the review-record correction below.
- [ ] **Step 2: Euler path** — after the Euler state-update loop has COMPLETED and outside any OpenMP parallel region (the :183–:239 anchor sits inside the update loop — insert after the loop's `end do`, in serial context; verify with `grep -n "omp" SOURCE_CODE/ESTAS/mod_SOLVER.f90 | head` that no parallel region spans the insertion point). Add `use AQUABC_NOST_STAGING, only: ADVANCE_NOST_STAGING, STG_SETTLE_FLUX, STG_GERM_FLUX, STG_FORM_FLUX, STG_GERM_COND` to the routine's use block. Gated:
```fortran
if (NOST_STAGE_MODEL > 0) then
    block
        real(kind=DBL), dimension(PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES) :: SR
        integer :: k
        do k = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
            SR(k) = PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(k) % DRIVING_FUNCTIONS(1)
        end do
        call ADVANCE_NOST_STAGING(PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES, &
                                  TIME_STEP, SR, STG_SETTLE_FLUX, STG_GERM_FLUX, &
                                  STG_FORM_FLUX, STG_GERM_COND)
    end block
end if
```
- [ ] **Step 3: RK2 path** — save stage-1 exports right where `K1_TOTAL_DERIVS` is saved (`SETTLE_1 = STG_SETTLE_FLUX; GERM_1 = STG_GERM_FLUX; FORM_1 = STG_FORM_FLUX; COND_1 = STG_GERM_COND` into block locals); after the corrector update call `ADVANCE_NOST_STAGING(n, TIME_STEP, SR, 0.5D0*(SETTLE_1+STG_SETTLE_FLUX), 0.5D0*(GERM_1+STG_GERM_FLUX), 0.5D0*(FORM_1+STG_FORM_FLUX), COND_1 .or. STG_GERM_COND)` with SR from the TIME+dt forcing. `TIME_STEP` units: verify with `grep -n "TIME_STEP" SOURCE_CODE/ESTAS/mod_SIMULATE.f90 | head -5` that the solver's TIME_STEP is in DAYS (the model runs 240 steps/day of a day-based clock); if it is not, convert at the call.
- [ ] **Step 4: The guard** — at the located flag-complete point. `RESUSPENSION_OPTION` lives in the resuspension derived type after the Phase-5.1 refactor — find the accessor with `grep -rn "RESUSPENSION_OPTION" SOURCE_CODE/ESTAS/mod_GLOBAL.f90 SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90 | head -5` (expect `resusp%RESUSPENSION_OPTION` or similar) and use that exact name:
```fortran
if (NOST_STAGE_MODEL > 0 .and. <resusp accessor> > 0) then
    error stop 'NOST_STAGE_MODEL=1 is incompatible with resuspension: BED_AKI is invisible to bed erosion (see the 2026-08-23 staging spec, s.6.1)'
end if
```
- [ ] **Step 5: Gates** — build; 0D golden + CL29 30-day A/B still byte-identical (flag=0 skips everything); unit suite green.
- [ ] **Step 6: Commit** — `feat(staging): solver-side once-per-step BED_AKI advance (RK2 stage-averaged) + resuspension guard`

### Task 5: `NOST_STAGING.out` diagnostic writer

**Files:**
- Modify: `SOURCE_CODE/ESTAS/mod_SIMULATE.f90` — inside the existing print-interval output block (the region writing MASS_BALANCES/`.out` files, ~:560–:601)

**Interfaces:** Consumes module state; file only created when `NOST_STAGE_MODEL > 0`.

- [ ] **Step 1: Writer** — open `<output folder>/NOST_STAGING.out` once, copying the `MASS_BALANCES.out` open/path code but with a fresh unit number (grep the existing `unit = 10xx` numbers in mod_SIMULATE and pick an unused one); header line, then at each print interval one row per box: `WTIME, box, BED_AKI, RAD_EMA, merge(1,0,FORM_LATCH), STG_SETTLE_FLUX, STG_GERM_FLUX, STG_FORM_FLUX, CUM_SETTLE_AKI, CUM_GERM_AKI, CUM_FORM_AKI, BURIED_AKI` — the three instantaneous fluxes satisfy spec §5's three-flux requirement; the three CUM columns are the exact integrals V4/V6 consume (sampled instantaneous columns cannot close a conservation identity).
- [ ] **Step 2: Gates** — flag=0: file must NOT be created (0D + CL29 A/B directories contain no new file, outputs byte-identical). Flag=1 CL29 1-year smoke: file exists, non-empty, `BED_AKI` becomes > 0 within the first autumn (this is spec rung **V3**).
- [ ] **Step 3: Commit** — `feat(staging): NOST_STAGING.out per-box diagnostic (flag-on only)`

### Task 6: Verification battery (spec rungs V2–V5)

**Files:**
- Create: `tools/check_staging_run.py` (small, argparse: `--staging-out`, `--mode {smoke,timing,conservation}`)

- [ ] **Step 1: V2 formal** — fresh full A/B at flag=0: 0D golden byte-identical AND CL29 **full-record** byte-identical vs a pre-branch binary run (rebuild `main`, run, save `.out` set, diff). Record the diff-count-zero evidence in the task log.
- [ ] **Step 2: V3 smoke** (already run in Task 5) — re-run via `tools/check_staging_run.py --mode smoke`: asserts non-empty file, `max(BED_AKI) > 0` by simulation day 300 (i.e. during the FIRST simulated autumn, 2012 — the initial-condition `AKI_C` provides the settling source, so this is reachable regardless of the mid-record VEG collapse), options echo `NOST staging: ON` present in the run log, **and the five echoed scalar values equal the option-file values** (the pair-swap defense).
- [ ] **Step 3: V4 solver-conservation** — two 90-day flag=1 CL29 runs (`ESTAS_PELAGIC_SOLVER=1` and `=2`); `--mode conservation` checks two things per box, both from the CUM columns:
  (a) the exact bed identity `Δ(BED_AKI) + Δ(BURIED_AKI) − (Δ(CUM_SETTLE_AKI) − Δ(CUM_GERM_AKI)) = 0` to 1e-12 relative under BOTH solvers (catches any integration/accumulator bug; closable exactly because all terms come from the same once-per-step update — a water-side per-box identity is NOT used: AKI_C transport makes it unclosable by design);
  (b) the double-banking detector: `|BED_rk2 − BED_euler| / max(BED_euler, ε)` stays small (< 5 % at day 90) — under kinetics-side double-banking it would approach 100 %.
- [ ] **Step 4: V5 formation timing** — flag=1 full-record run; `--mode timing` reports, per year, the first day `FORM_LATCH` turns on per box; assert **zero latch-ONs before Aug 31** and first ON per year ∈ [Aug 31, Sep 30] (spec: measured EMA crossings Aug 31–Sep 25, median Sep 14; the window end has 5 days' slack).
- [ ] **Step 5: Commit** — `test(staging): V2-V5 verification battery + check_staging_run tool`

### Task 7: The measured ladder (V6/V7) + documentation

- [ ] **Step 1: Enable in a scratch CL29 config** — copy `INPUTS_CL29/PELAGIC_MODEL_OPTIONS.txt` to a scratch setup dir (do NOT edit the live one: adoption is a separate user decision), append the six lines (spec §4.5 table order, values = defaults), full-record run.
- [ ] **Step 2: V6 scoring** — `tools/validate_cl29_vs_epa.py --obs <merged EPA+KM csv> --wconst INPUTS_CL29/WCONST_04.txt --phase` vs the adopted baseline (PO4 0.0232, Si 0.8235, CHLA 25.52, NH4 0.0489, r +0.70); plus the staging-specific criteria from `NOST_STAGING.out` (2014–2022 only): per-box post-formation annual max of `BED_AKI` non-declining; annual formation/germination ratio `ΔyearCUM_FORM / ΔyearCUM_GERM > 1` in sustaining boxes (exact annual sums from the CUM columns); dead-water germination fraction ≈ 0; the banked-fraction diagnostic `ΔyearCUM_SETTLE` vs `ΔyearCUM_FORM` per box; monthly `FIX_CYN_C`/`NOST_VEG_HET_C` vs obs `FIX_TOT` (June ≤ current 7.4× overshoot; November reported); Oct–Nov VEG trajectory.
- [ ] **Step 3: V7 fragility** — no mid-run code hacks. Two full-record scratch-options runs: (i) `KR_GERM_BED = 0` (no-recruitment control: the bank's accumulation-only upper envelope and the pure decay rate); (ii) the default run's own weakest bloom year analyzed for bank carryover into the following spring (natural interannual variability as the failed-bloom proxy). Report both; together they bound what one failed season does to the inoculum.
- [ ] **Step 4: Document** — append the ladder results as a new numbered section to `docs/CL29_phenology_diagnosis.md` (§29), update the BACKLOG akinete row status, and record the adoption question for the user. Commit `docs(cl29): s.29 -- akinete staging ladder results`.

---

## Review record

Adversarial workflow review 2026-08-23 (4 finder dimensions → refute-oriented verification, 14
agents): 24 findings — 5 verified CONFIRMED, 5 REFUTED, 8 majors passed unverified, 6 minors.
**Execution correction (Task 4): the four "DRIVING_FUNCTIONS(1) is temperature" findings were
RIGHT and their REFUTED verdicts were WRONG** — direct reading of the assignment block settles
it: (1)=temperature, (2)=salinity, (3)=solar radiation. The implementer caught it via the
step's own verify-grep and used index 3; no wrong code ever landed. Process lesson: four
verifiers sharing one misreading are not four independent checks — the in-code verify-step
was the control that actually worked. All confirmed/unverified/minor fixes are incorporated: STG_FORM_FLUX export + CUM_*
exact integrals (the blocking find), V4 reformulated (bed-side identity + double-banking
detector; per-box water identity unclosable due to transport), baseline-creation Step 0, the
0D-does-not-call-the-reader gate fix, tests/fortran Makefile links (both test programs), Euler
anchor moved outside the parallel loop, resusp accessor + TIME_STEP unit verify-steps, V5
window made self-consistent, five-value option echo as the pair-swap defense, V7 without
mid-run code hacks.

## Self-review notes (completed)

- Spec coverage: §4.1→T1, §4.2/4.3→T3+T1, §4.4→T4, §4.5→T2, §4.6.1→T4, §4.6.2 (settling table stays 0 — no change needed, CL29 already 0; asserted nowhere → covered by V2 byte-identity), §4.7→T3, §5 writer→T5, §7 V1→T1, V2–V5→T6, V6/V7→T7.
- Type consistency: `SET_NOST_STAGING_PARAMS(tgerm, iform, krgerm, kmort, vsettle)` used identically in T1/T2; export array names identical in T1/T3/T4/T5; `R_GERM_BED_AKI`/`R_SETTLE_AKI` identical in T3 steps.
- Known deliberate deviations from bite-size: T3 is the largest task (signature + gates + wiring must move together to compile); its internal steps keep the TDD cycle.
