# CL29 Light-Climate Corrections Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the two measured light-climate correctness fixes (day-length weighting and background
extinction) behind guarded switches, re-baseline CL29 on them, and re-test C:Chl 34 — three
independent arms, each separately adoptable.

**Architecture:** Both fixes follow the project's established opt-in pattern: a new integer/real
option read gracefully from `PELAGIC_MODEL_OPTIONS.txt`, defaulting to today's behaviour so the
standard build stays byte-identical. Arm 1 adds `LIGHT_DAYLENGTH_OPTION` to `LIM_LIGHT` (the only
routine CL29's `smith = 1` path uses). Arm 2 is a pure config change to `K_B_E`. Arm 3 changes no
code at all. Each arm is scored independently against the canonical baseline; **nothing is bundled**,
because bundling is precisely how §44.3's arithmetic became unsupportable.

**Tech Stack:** gfortran (release), `make build-estas`; Python 3 for scoring
(`tools/validate_cl29_vs_epa.py`, `/tmp/monthly_residuals.py`); ESTAS box driver on `INPUT_CL29.txt`.

**Spec:** `docs/superpowers/specs/2026-09-03-warm-diatom-guild-design.md` §0 (the light-climate arms).
Evidence: `docs/CL29_phenology_diagnosis.md` §44 (corrected), §45, §46 (corrected), **§47**.

## Global Constraints

- **Byte-identity is the gate for arm 1.** `LIGHT_DAYLENGTH_OPTION 0` must reproduce the current
  `OUTPUTS_CL29` **exactly** — 0.0e+00 on all ten scored variables — at full record *and* on the 0D
  golden. The 0D leg is the one that gets skipped (§45); do not skip it.
- **Never modify live `INPUTS_CL29/` or `INPUT_CL29.txt`** without an explicit adoption decision from
  the user. All probes run from copies under the scratchpad, with **quoted** absolute paths in the
  driver file (list-directed read: an unquoted `/` terminates the input list and the path reads back
  blank).
- **Never pipe a full-record run through a filter** — it throttles the model ~6× (1 vs 5.8
  model-days/s). Write the log to a file; filter at analysis time (§29).
- **Scoring:** any run whose WCONST differs from the live one must be scored with
  `--wconst <that WCONST>`. Carbon metrics are immune; chlorophyll metrics are not. This is the trap
  that mis-scored the first C:Chl probe.
- Runs take ~14 min at full record (2012–2022, day 0–4016). `ESTAS_HOLD_VOLUME=1`,
  `PRINT_INTERVAL=240`. The binary is single-threaded; `OMP_NUM_THREADS` is a no-op.
- **`nconst` is defined in three places** — `aquabc_II_pelagic_interface.f90:75`, `mod_GLOBAL.f90:20`
  (`parameter`, the ESTAS path), and the declared count in `PELAGIC_INPUTS.txt`. This plan adds no
  constants, so none of them changes; if that turns out to be false, change all three (§45).
- **Report phase metrics before aggregate metrics.** A simultaneous CHLA + PO4 RMSE improvement is
  **not** evidence of a better model until seasonal r and autumn:spring are checked — that pattern
  has misled three times (§22, §43.3, §46).

---

## File Structure

| file | responsibility | arm |
|---|---|---|
| `SOURCE_CODE/AQUABC/mod_AQUABC_II_GLOBAL.f90` | `LIGHT_DAYLENGTH_OPTION` declaration + default 0 | 1 |
| `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90` | `LIM_LIGHT` gains an `FDAY` arg and the two forms | 1 |
| `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_*.f90` | **13** call sites gain the `FDAY` argument | 1 |
| `tests/fortran/test_pelagic_aux_subset.f90` | **duplicate copy of `LIM_LIGHT`** — must mirror exactly | 1 |
| `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` | graceful read + startup report | 1 |
| `INPUTS_CL29/PELAGIC_MODEL_OPTIONS.txt` (copy) | the new option line | 1 |
| `INPUTS_CL29/EXTRA_WCONST.txt` (copy) | `K_B_E` | 2 |
| `INPUTS_CL29/WCONST_04.txt` (copy) | `DIA_C_TO_CHLA` | 3 |
| `tests/fortran/test_lim_light_daylength.f90` | unit test: the three forms at known inputs | 1 |
| `tools/probe_lim_light.py` | the §47 algebra, kept as a regression oracle | 1 |

---

## Task 1: `LIM_LIGHT` day-length forms, behind a guarded option — ✅ DONE (`60772ae`)

**Verified 2026-09-04:** byte-identity at option 0 over the **full record** — all 61 output
artefacts in `OUTPUTS_CL29` compare equal, matching MD5 — **and** the 0D golden (526 rows × 22
cols, rtol 1e-6). 10/10 new unit tests; full Fortran suite, `fail_loud_constants` and the 0D
example tests pass. End-to-end in the model: day-60 `DIA_C` 0.05839 (A) < 0.06204 (B) < 0.06463
(legacy). `INPUTS/` still defaults to OFF. Live `INPUTS_CL29/` untouched.

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90` — `LIM_LIGHT` signature
  and the `LLIGHT` expression
- Modify: **13** library call sites — `_DIATOMS.f90` ×1, `_OTHER_PLANKTONIC_ALGAE.f90` ×1,
  `_CYANOBACTERIA.f90` ×4, `_FIX_CYANOBACTERIA.f90` ×4, `_NOSTACALES.f90` ×3
- Modify: `tests/fortran/test_pelagic_aux_subset.f90` (the duplicate `LIM_LIGHT`)
- Modify: `SOURCE_CODE/AQUABC/mod_AQUABC_II_GLOBAL.f90`,
  `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90`
- Create: `tools/probe_lim_light.py`
- Test: `tests/fortran/test_lim_light_daylength.f90`

**Interfaces:**
- Consumes: `FDAY(nkn)` — already read at `aquabc_II_pelagic_model.f90:394` from
  `DRIVING_FUNCTIONS(:,4)` and bundled as `ENV_CHUNK%FDAY`. **No new forcing.** Live series is
  `INPUTS_CL29/FORC_TS_9.txt` (4,017 daily records, 0.2898 on 1 Jan = 6.96 h at 55 °N).
- Produces: `LIM_LIGHT(..., BETA, FDAY)` — **one** new trailing arg, so the 13 callers change in one
  mechanical batch. The option itself is a module scalar in `AQUABC_II_GLOBAL`, which every library
  routine already `use`s and which already carries `USE_CTMI_TEMP` and `FEPO4_KSP_LOG10` — so **no
  `FLAGS` plumbing and no signature churn in the five library routines.**

⚠ **Three corrections this task made to the plan as first written** (all found during the build):
1. **13 call sites, not 6.** The 6 came from counting `FDAY => env%FDAY` associate blocks, not calls.
   The surface-positioned calls (`H_SURF_ARR`, `K_SURF_POS`) of the ratchet need the argument too.
2. **`tests/fortran/test_pelagic_aux_subset.f90` holds a second copy of `LIM_LIGHT`** which five
   kinetics tests link. It must move in lockstep or they fail to compile.
3. **Read placement is load-bearing.** No shipped options file carries the CYN Droop lines, so the
   graceful-read chain always exits at `CYN_VARIABLE_N`. The new read must sit **before** that
   block (right after `V_SETTLE_AKI`) or it is unreachable for every existing setup.

- [x] **Step 1: Write the failing unit test**

`tests/fortran/test_lim_light_daylength.f90` — three assertions at a single known point
(`Ia = 60.0`, `Is = 200.0`, `ke = 2.617`, `H = 3.5`, `FDAY = 0.389`, one node):

```fortran
program test_lim_light_daylength
    implicit none
    double precision :: LL(1), LS(1), Ia(1), TCHLA(1), GIT(1), Hh(1), ke(1), FD(1)
    double precision :: base, formA, formB
    integer :: nfail
    nfail = 0
    Ia = 60.0D0; TCHLA = 2.0D0; GIT = 1.0D0; Hh = 3.5D0; ke = 2.617D0; FD = 0.389D0

    ! opt 0 must equal the pre-change result exactly
    call LIM_LIGHT(Ia, TCHLA, GIT, Hh, ke, LL, 53.0D0, 100.0D0, LS, 1, 0.0D0, FD, 0)
    base = LL(1)

    ! opt 1 (Form A) = FDAY * base, to round-off
    call LIM_LIGHT(Ia, TCHLA, GIT, Hh, ke, LL, 53.0D0, 100.0D0, LS, 1, 0.0D0, FD, 1)
    formA = LL(1)
    if (abs(formA - FD(1)*base) > 1.0D-12) then
        write(6,*) 'FAIL FormA: got', formA, 'expected', FD(1)*base; nfail = nfail + 1
    end if

    ! opt 2 (Form B) must sit strictly between Form A and base:
    ! it recovers most of the dose but pays the saturation curvature.
    call LIM_LIGHT(Ia, TCHLA, GIT, Hh, ke, LL, 53.0D0, 100.0D0, LS, 1, 0.0D0, FD, 2)
    formB = LL(1)
    if (.not. (formB > formA .and. formB < base)) then
        write(6,*) 'FAIL FormB bracket: A/B/base =', formA, formB, base; nfail = nfail + 1
    end if

    ! Form B must converge to base as FDAY -> 1 (the cancellation identity)
    FD = 1.0D0
    call LIM_LIGHT(Ia, TCHLA, GIT, Hh, ke, LL, 53.0D0, 100.0D0, LS, 1, 0.0D0, FD, 2)
    if (abs(LL(1) - base) > 1.0D-12) then
        write(6,*) 'FAIL FormB@FDAY=1: got', LL(1), 'expected', base; nfail = nfail + 1
    end if

    if (nfail == 0) then
        write(6,*) 'PASS test_lim_light_daylength'
    else
        write(6,*) 'FAILURES:', nfail; stop 1
    end if
end program test_lim_light_daylength
```

- [x] **Step 2: Run it and confirm it fails to compile**

Run: `gfortran -J tests/fortran -I tests/fortran tests/fortran/test_lim_light_daylength.f90 SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90 -o /tmp/t_lld 2>&1 | head`
Expected: an argument-count/rank error on `LIM_LIGHT` — the two new args do not exist yet.

- [x] **Step 3: Extend the `LIM_LIGHT` signature and add the two forms**

In `aquabc_II_pelagic_auxillary.f90:517`, append two dummy args:

```fortran
subroutine LIM_LIGHT(Ia, TCHLA, GITMAX, H, ke, LLIGHT, CCHL_RATIO, K_LIGHT_SAT, LIGHT_SAT, nkn, BETA, &
                     FDAY, DAYLENGTH_OPT)
```

with declarations beside `BETA`:

```fortran
    double precision, intent(in)  :: FDAY(nkn)   ! photoperiod fraction, 0-1
    integer,          intent(in)  :: DAYLENGTH_OPT ! 0 = none (legacy), 1 = Form A, 2 = Form B
```

and a local:

```fortran
    double precision :: IA_EFF(nkn)   ! irradiance entering the P-I curve
    double precision :: FD_W(nkn)     ! outer day-length weight
```

Replace the single `LLIGHT` assignment (currently `TEMP3 = safe_exp(-TEMP1)` followed by the
`LLIGHT = ...` line) with:

```fortran
    ! Day-length handling. I_A is a DAILY INTEGRAL (model.f90:393), so:
    !   opt 0  legacy: the daily mean is applied for a full 24 h (no FDAY).
    !   opt 1  Form A: weight by the photoperiod without concentrating the dose.
    !          This is the form used by the smith==0 library branch. It DISCARDS
    !          (1-FDAY) of each day's light and is retained only for reproducing
    !          doc s.47's comparison -- do not adopt it.
    !   opt 2  Form B: concentrate the dose into the photoperiod, then weight by
    !          it (WASP/EUTRO; cf. CUR_SMITH's IAV = 0.9*ITOT/FDAY). In the
    !          light-limited regime the FDAY cancels, which is correct: a fixed
    !          daily dose spread over more hours cannot change a near-linear
    !          daily integral. Form B departs from opt 0 only through P-I curvature.
    select case (DAYLENGTH_OPT)
        case (1)
            IA_EFF = Ia
            FD_W   = max(1.0D-6, min(1.0D0, FDAY))
        case (2)
            FD_W   = max(1.0D-6, min(1.0D0, FDAY))
            IA_EFF = Ia / FD_W
        case default
            IA_EFF = Ia
            FD_W   = 1.0D0
    end select

    TEMP3  = safe_exp( - TEMP1)
    LLIGHT = FD_W * (EULER_E / TEMP1) * &
             (safe_exp( -TEMP2 * IA_EFF * TEMP3) - safe_exp( -TEMP2 * IA_EFF))
```

Leave the existing `LLIGHT = max(0.0D0, min(1.0D0, LLIGHT))` clamp directly after, unchanged.

⚠ The `1.0D-6` floor on `FDAY` guards the polar-night division; the live series never goes below
0.2887, so it is defensive only and must not alter any CL29 result.

- [x] **Step 4: Run the unit test to verify it passes**

Run: `gfortran -J tests/fortran -I tests/fortran tests/fortran/test_lim_light_daylength.f90 SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_auxillary.f90 SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model_constants.f90 SOURCE_CODE/AQUABC/aquabc_II_physical_constants.f90 -o /tmp/t_lld && /tmp/t_lld`
Expected: `PASS test_lim_light_daylength`

(If module dependencies bite, build against `tests/fortran/*.o` the way the existing Fortran tests do
— the object tree is already there.)

- [x] **Step 5: Update the six callers**

One mechanical batch. In each of the six library routines, the `smith .eq. 1` call gains the two
trailing args. Example, `aquabc_II_pelagic_lib_DIATOMS.f90:156`:

```fortran
        call LIM_LIGHT(I_A, CHLA, KG_DIA, DEPTH, K_E, LIM_KG_DIA_LIGHT, &
                       DIA_C_TO_CHLA, I_S_DIA, DIA_LIGHT_SAT, nkn, BETA_DIA, &
                       FDAY, LIGHT_DAYLENGTH_OPTION)
```

`FDAY` is already in scope in every one of them (each has `FDAY => env%FDAY` in its associate
block — that is why they compile today with FDAY unused on this branch).
`LIGHT_DAYLENGTH_OPTION` comes from the constants/flags module; add it to the same `use` the
routine already has for `smith`.

Verify the count, do not line-anchor:

Run: `grep -rn "call LIM_LIGHT" SOURCE_CODE/ | wc -l`
Expected: `6`

- [x] **Step 6: Wire the option end to end**

`SOURCE_CODE/ESTAS/mod_GLOBAL.f90` — declare beside the other options:

```fortran
    integer :: LIGHT_DAYLENGTH_OPTION = 0
```

`SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` — graceful read, placed after the `NOST_STAGE_MODEL`
block and **before** the trailing `CYN_ALLELOPATHY_FILE_NAME` lines, matching the established
pattern exactly:

```fortran
    ! Day-length weighting of the light limitation (0 = legacy 24 h, default;
    ! 1 = Form A, photoperiod weight only -- discards light, kept for the record;
    ! 2 = Form B, dose concentrated into the photoperiod then weighted -- WASP).
    ! Graceful: option files without these lines keep the mod_GLOBAL default 0.
    read(IN_FILE + 1, *, end = 900, err = 900)
    read(IN_FILE + 1, *, end = 900, err = 900) LIGHT_DAYLENGTH_OPTION
```

then add it to the argument list at `:1650` alongside `LIGHT_EXTINCTION_OPTION`, and carry it in
`FLAGS` in `aquabc_II_pelagic_interface.f90` beside `LIGHT_EXTINCTION_OPTION` (`FLAGS(8)`) — take the
next free slot and mirror both the pack and the unpack (`:165` and `:232`).

⚠ **Read the option file, not the code default.** `LIGHT_EXTINCTION_OPTION` defaults to 1 in
`aquabc_II_pelagic_interface.f90:152` while CL29's option file sets it to **0** — this exact
mismatch produced a wrong published kd table on 2026-09-04 (§44.3 method note). Default the new
option to 0 in **both** places.

- [x] **Step 7: Build and prove byte-identity at flag 0**

```bash
make clean-all && make build-estas 2>&1 | tail -5
mkdir -p /tmp/claude-1000/.../probe_bitid && cp -r INPUTS_CL29 /tmp/.../probe_bitid/
# driver: quote the absolute path -- list-directed read treats an unquoted / as terminator
ESTAS_HOLD_VOLUME=1 ./estas INPUT_probe_bitid.txt > /tmp/.../bitid.log 2>&1
python3 /tmp/monthly_residuals.py --out /tmp/.../OUT_bitid --ref OUTPUTS_CL29
```

Expected: **0.0e+00 on all ten variables.** Then the 0D golden:

Run: `make test-fortran`
Expected: all existing Fortran tests pass, including the 0D golden.

- [x] **Step 8: Land the §47 algebra as a regression oracle**

Create `tools/probe_lim_light.py` — the exact script that produced §47.2, parameterised on the
WCONST/options/forcing paths, printing the 12-month `A/cur` and `B/cur` table. It is the cheap check
that the Fortran matches the algebra, and it is what §47's "evaluate the operator before you probe
the input" lesson is worth operationally.

Run: `python3 tools/probe_lim_light.py --inputs INPUTS_CL29 --outputs OUTPUTS_CL29`
Expected: February `A/cur` 0.395, `B/cur` 0.779; May 0.665 / 0.810 (±0.002).

- [x] **Step 9: Commit**

```bash
git add SOURCE_CODE tests/fortran/test_lim_light_daylength.f90 tools/probe_lim_light.py
git commit -m "feat(light): LIGHT_DAYLENGTH_OPTION -- day-length weighting in LIM_LIGHT

Adds the day-length correction to the smith==1 path CL29 runs. FDAY was
already read, allocated and bundled (model.f90:394) and is fully wired
on the smith==0 library branch -- only LIM_LIGHT bypassed it.

  0 = legacy 24h (default, byte-identical)
  1 = Form A, FDAY*f(I_A) -- the smith==0 form; discards (1-FDAY) of the
      daily dose. Retained to reproduce doc s.47; NOT for adoption.
  2 = Form B, FDAY*f(I_A/FDAY) -- WASP/EUTRO. Correct: in the
      light-limited regime FDAY cancels, since a fixed daily dose spread
      over more hours cannot change a near-linear daily integral.

Measured (doc s.47.2): Form A -60.5% Feb / -33.5% May (1.68x
differential); Form B -22.1% / -19.0% (1.04x -- an offset).

Verified byte-identical at flag 0, full record and 0D golden."
```

---

## Task 2: Arm 1 probe — score Form B (and Form A, for the record)

**Files:**
- Create (scratchpad only): `probe_fdayB/` and `probe_fdayA/` copies of `INPUTS_CL29/`, each with the
  new option line set, plus their driver files
- Modify: nothing tracked

**Interfaces:**
- Consumes: the flagged binary from Task 1.
- Produces: two scorecards for §48, and the decision on whether Form B is adoptable on its own.

- [ ] **Step 1: Stage both probes**

```bash
S=/tmp/claude-1000/-home-razinka-AQUABCv0-2/<session>/scratchpad
for f in A B; do
  cp -r INPUTS_CL29 $S/probe_fday$f
done
# append the option line to each PELAGIC_MODEL_OPTIONS.txt, before CYN_ALLELOPATHY_FILE_NAME
```

⚠ The option must be inserted **in the reader's order** — after the `V_SETTLE_AKI` block and before
`# CYN_ALLELOPATHY_FILE_NAME`. A misplaced line silently shifts every subsequent read.

- [ ] **Step 2: Verify the flag actually took effect before spending 28 minutes**

Run a 60-day run of each and confirm the results differ from baseline:

```bash
ESTAS_HOLD_VOLUME=1 ./estas INPUT_probe_fdayB_60d.txt > $S/fdayB_60d.log 2>&1
```
Expected: `DIA_C` at day 60 differs from the baseline 60-day value. **If it is identical, the flag
did not reach the kinetics** — stop and trace before running the full record. (This is the check that
would have caught the `nconst` mismatch in §45 an hour earlier.)

- [ ] **Step 3: Run both at full record, in background, logs to file**

```bash
ESTAS_HOLD_VOLUME=1 ./estas INPUT_probe_fdayB.txt > $S/fdayB.log 2>&1
ESTAS_HOLD_VOLUME=1 ./estas INPUT_probe_fdayA.txt > $S/fdayA.log 2>&1
```
⚠ Do **not** pipe through a filter. ~14 min each.

- [ ] **Step 4: Score, phase metrics first**

```bash
python3 tools/validate_cl29_vs_epa.py --outputs $S/OUT_fdayB
python3 /tmp/monthly_residuals.py --out $S/OUT_fdayB
```
(No `--wconst` needed — WCONST is unchanged in this arm.)

Report in this order: **seasonal r; autumn:spring ratio; Feb/May/Aug/Oct `DIA_C` vs observed**;
*then* CHLA RMSE, PO4 RMSE, Si RMSE.

**Registered predictions** (write them down before reading the output):
- Form B: production down ~20 % in every month; **February improves, October gets worse**; phase
  metrics roughly held; CHLA RMSE likely worse (less biomass overall).
- Form A: February strongly improved, October badly damaged, May's bloom hurt.
- If Form B's seasonal r rises **and** February holds, it is adoptable on its own.
- If it degrades everything roughly uniformly, it is still correct physics; the adoption question
  then becomes whether to accept a worse fit for a right formulation — **a user decision, not mine.**

- [ ] **Step 5: Commit the probe record (no live inputs touched)**

Document both scorecards in a new `docs/CL29_phenology_diagnosis.md` §48. Do not modify
`INPUTS_CL29/`.

---

## Task 3: Arm 2 — background extinction, as its own arm

**Files:**
- Create (scratchpad only): `probe_kbe/` copy of `INPUTS_CL29/` with `EXTRA_WCONST.txt` edited
- Modify: nothing tracked

**Interfaces:**
- Consumes: the same binary (no code change — arm 2 is config only).
- Produces: the scorecard that decides whether the September–October transparency error is worth
  correcting given its autumn cost.

- [ ] **Step 1: Set `K_B_E` from the measurements**

Live: `K_B_E = 2.18` in `INPUTS_CL29/EXTRA_WCONST.txt`, giving `kd = 2.58 + 0.02·chla` (option 0,
`light_kd`), i.e. model kd 2.60–2.93 against measured 2.82–4.27 (§44.3 corrected).

Set **`K_B_E = 2.38`** in the probe copy. Rationale: it lifts the annual mean kd from ≈2.68 to ≈2.88,
matching the May–August measurements (2.85–2.98) where coverage is densest and the model is only
5–8 % low. ⚠ It deliberately does **not** chase September's 4.27 — that month's excess is largely the
bloom's own self-shading, which the model will generate itself if it ever grows the bloom. Chasing it
with a constant would hard-code a bloom the model does not have.

- [ ] **Step 2: Run and score**

Same procedure as Task 2 steps 3–4. ~14 min.

**Registered prediction:** near-uniform ≈7 % kd increase → small production loss in every month;
May's bloom slightly reduced; **October slightly worse**; February essentially unchanged
(Feb/May kd ratio 0.988 — this arm has no seasonal leverage by construction).

- [ ] **Step 3: Decide independently of arm 1**

Arm 2 is adoptable iff it is defensible as a measurement (it is: the model is below every measured
month) **and** its cost is acceptable. Report both halves; the adoption call is the user's.

- [ ] **Step 4: Record as §48.2**

---

## Task 4: Arm 3 — re-test C:Chl 34 on the corrected baseline

**Files:**
- Create (scratchpad only): `probe_cchl34/` — the best-scoring baseline from arms 1–2, with
  `DIA_C_TO_CHLA` 53 → 34 in `WCONST_04.txt`
- Modify: nothing tracked

**Interfaces:**
- Consumes: whichever of arms 1–2 the user adopts (or neither — then this repeats §46.1 exactly and
  should be skipped).
- Produces: the answer to "is the measured C:Chl adoptable yet?"

- [ ] **Step 1: Stage from the adopted baseline only**

⚠ If neither arm 1 nor arm 2 is adopted, **skip this task** — it would reproduce §46.1 and tell us
nothing new.

- [ ] **Step 2: Run, and score WITH `--wconst`**

```bash
python3 tools/validate_cl29_vs_epa.py --outputs $S/OUT_cchl34 --wconst $S/probe_cchl34/WCONST_04.txt
```
⚠⚠ **The `--wconst` flag is mandatory here.** `/tmp/monthly_residuals.py` loads C:Chl from the *live*
WCONST, so without it every chlorophyll metric is computed at 53 against a run built at 34. Carbon
metrics are immune; chlorophyll metrics are silently wrong. This trap has fired once already.

- [ ] **Step 3: Evaluate against the §0.1 outcome statement**

- February `DIA_C` ≤ 1.3× observed → **adopt the measured C:Chl**, record it, and the C:Chl question
  closes.
- February still ≫ observed → §47.4 is confirmed: the constant is doing two jobs (pigment conversion
  *and* `I_s`), no fixed value serves both, and the exit is **photoacclimative C:Chl** (§22, BACKLOG
  P2) — not a second guild, and not a better fixed number.

- [ ] **Step 4: Record as §48.3, and update BACKLOG §3**

Whatever the outcome, the warm-guild spec's §0 is then closed and the guild's status reverts to what
§2.2 says it is: an August–September case, to be decided on its own evidence and on a re-baselined
light climate.

---

## Self-Review

**Spec coverage.** Spec §0 arm 1 → Tasks 1–2; arm 2 → Task 3; arm 3 → Task 4; §0.1's sequencing
argument → the ordering, and Task 4 step 4's handoff. Spec §§1–6 (the guild itself) are deliberately
**not** in this plan: §0.1 argues the guild must be built against a re-baselined light climate, so it
needs its own plan written after these results. That is a stated scope decision, not a gap.

**Placeholder scan.** One intentional placeholder remains: the scratchpad session path `<session>` in
Task 2 step 1, which is environment-specific. The `K_B_E` value (2.38) and all option values are
concrete.

**Type consistency.** `LIM_LIGHT`'s two new args are `double precision, intent(in) :: FDAY(nkn)` and
`integer, intent(in) :: DAYLENGTH_OPT`, appended in that order at every one of the six call sites and
in the unit test. `LIGHT_DAYLENGTH_OPTION` is `integer` in `mod_GLOBAL.f90`, in the `FLAGS` pack/unpack,
and in the module the library routines `use` — one name, one type, throughout.

**Known risk this plan does not remove.** Arms 1 and 2 both reduce production and both hurt October.
It is entirely possible that all three arms are correct physics **and** every headline metric gets
worse. That is a real outcome, not a failure of the plan: it would make §40.1's light wall
quantitatively worse and strengthen — not weaken — the case that the autumn deficit is structural.
Report it that way if it happens.
