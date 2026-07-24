# Configurable Pelagic Solver (experimental RK2) + RK2 fixes — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the existing RK2/Heun pelagic solver selectable via the `ESTAS_PELAGIC_SOLVER` env var (default Euler), expose it in the Shiny run-control UI, and bank two correctness fixes (V/F) to the RK2 branch — with RK2 documented as **experimental**.

**Architecture:** Three independent parts. **A (Fortran):** a localized env read in `mod_SIMULATE.f90` replacing the hard-coded Euler. **C (Fortran):** always-on volume-RK2-averaging + stage-2 forcing re-eval in the RK2 branch of `mod_SOLVER.f90` (Euler path untouched). **B (Shiny):** a run-control selector routed through a pure, unit-testable `assemble_run_env` helper. Euler default stays byte-identical throughout.

**Tech Stack:** Fortran (gfortran), `make build-estas`; Python/Shiny (`shiny_app/`), pytest.

**Design spec:** `docs/superpowers/specs/2026-07-24-configurable-pelagic-solver-design.md`.
**Verified V/F fix reference (env-toggled experiment version):** `.superpowers/sdd/rk2-VF-fix.diff` — this plan applies the SAME logic but **always-on** (toggles removed).

## Global Constraints

- **Euler default (env `ESTAS_PELAGIC_SOLVER` unset or `1`) must be byte-identical** to pre-change on every committed setup. Parts A and C must not perturb the Euler path.
- **RK2 is opt-in and experimental** — no "better/faster/2nd-order" claim in code, docs, or UI. It converges at ~1st order for this model (dominant cause: `MIN_CONCENTRATION` clamping non-smoothness — a model issue, out of scope).
- **Part C is always-on** (RK2 is currently unreachable → no existing RK2 output to preserve). Do NOT keep the experiment's `ESTAS_RK2_FIX_V`/`_F` env toggles.
- Env var: unset/`1` → Euler, `2` → RK2, anything else → `error stop 1` (fail-loud; a string-form `stop 'msg'` returns exit 0 in this codebase — use `error stop`).
- **Shiny: copy-before-mutate** `Setup.env` (it's a shared module-level dict) — `dict(setup_env)`, never mutate in place.
- Binary `./ESTAS_II` (input = arg 1; no `ESTAS_HOLD_VOLUME` for Standard-topology gate runs). Build serial. Line numbers are as-of-current-branch tip.

---

### Task 1: Capture the Euler-default golden baseline

The byte-identical-Euler oracle for Tasks 2 & 3.

**Files:** none. Artifacts: `/tmp/SOLV_PRE`, `/tmp/solv_golden_std` (`OUTPUTS/`), `/tmp/solv_golden_200` (`OUTPUTS_200day/`).

- [ ] **Step 1:** `git rev-parse HEAD | tee /tmp/SOLV_PRE` (all source pristine on this branch).
- [ ] **Step 2:** `make clean-all && make build-estas` → clean.
- [ ] **Step 3:** With `ESTAS_PELAGIC_SOLVER` unset, run both setups and snapshot:
```bash
./ESTAS_II INPUT.txt        && cp -r OUTPUTS         /tmp/solv_golden_std
./ESTAS_II INPUT_200day.txt && cp -r OUTPUTS_200day  /tmp/solv_golden_200
```
- [ ] **Step 4:** No commit. Confirm both golden dirs + `/tmp/SOLV_PRE` exist.

---

### Task 2: Part A — Fortran env read (`mod_SIMULATE.f90`)

**Files:** Modify: `SOURCE_CODE/ESTAS/mod_SIMULATE.f90` (declaration near :22; the assignment at :93).

**Interfaces:** Produces env-driven `PELAGIC_SOLVER_NO` (1 or 2) passed to `SOLVE` unchanged.

- [ ] **Step 1:** Add `character(len = 32) :: SOLVER_ENV` to `RUN_SIMULATION`'s local declarations (near the other locals ~:22).

- [ ] **Step 2:** Replace the line `        PELAGIC_SOLVER_NO = 1` (at ~:93) with:
```fortran
        call get_environment_variable('ESTAS_PELAGIC_SOLVER', SOLVER_ENV)
        select case (trim(adjustl(SOLVER_ENV)))
            case ('', '1')
                PELAGIC_SOLVER_NO = 1
            case ('2')
                PELAGIC_SOLVER_NO = 2
            case default
                write(6,*) 'ERROR: ESTAS_PELAGIC_SOLVER must be 1 (Euler) or 2 (Heun/RK2), got "' &
                           // trim(adjustl(SOLVER_ENV)) // '"'
                error stop 1
        end select
        write(6,*) 'PELAGIC_SOLVER = ', PELAGIC_SOLVER_NO, ' (1 = Euler, 2 = Heun/RK2, experimental)'
```

- [ ] **Step 3: Build** `make build-estas` → clean.

- [ ] **Step 4: Byte-identical Euler default** (env unset):
```bash
./ESTAS_II INPUT.txt        && diff -r OUTPUTS        /tmp/solv_golden_std && echo "STD EULER IDENTICAL"
./ESTAS_II INPUT_200day.txt && diff -r OUTPUTS_200day /tmp/solv_golden_200 && echo "200 EULER IDENTICAL"
```
Expected: both IDENTICAL (the added stdout echo does not touch `OUTPUTS/`).

- [ ] **Step 5: env selects RK2 + fail-loud:**
```bash
ESTAS_PELAGIC_SOLVER=2 ./ESTAS_II INPUT_200day.txt 2>&1 | grep -qE "PELAGIC_SOLVER =[[:space:]]+2" && echo "RK2 SELECTED"
ESTAS_PELAGIC_SOLVER=3 ./ESTAS_II INPUT_200day.txt; echo "invalid exit code: $?  (must be non-zero)"
```
Expected: `RK2 SELECTED`; the `=3` run prints the ERROR line and exits non-zero. (RK2 here is still the *unfixed* branch — that's fine; Task 3 fixes it.)

- [ ] **Step 6: Commit** `git commit -am "feat(solver): select pelagic solver via ESTAS_PELAGIC_SOLVER env (default Euler)"`.

---

### Task 3: Part C — RK2 volume + forcing fixes (always-on, `mod_SOLVER.f90`)

Apply the verified V/F logic (`.superpowers/sdd/rk2-VF-fix.diff`) **without** the env toggles — permanent corrections to the RK2 branch (`~:314–453`). The Euler branch (`~:137–311`) is untouched.

**Files:** Modify: `SOURCE_CODE/ESTAS/mod_SOLVER.f90` (RK2 branch only).

- [ ] **Step 1: Capture pre-step volume + stage-1 volume derivative.** In the predictor `block` (after the `k1_deriv, k2_deriv` decl, ~:360), add:
```fortran
                real(kind = DBL), dimension(PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES) :: &
                    VOLUME_OLD, VOL_DERIV_1
```
Then at the very top of the predictor loop `do i = 1, … NUM_PELAGIC_BOXES` (just before the `if (.not. HOLD_VOLUME_CONSTANT)` volume advance at ~:364):
```fortran
                    VOLUME_OLD(i)  = PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME
                    VOL_DERIV_1(i) = PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(i, 1)
```

- [ ] **Step 2: Fix F — re-evaluate forcing at TIME+dt (always).** Immediately **before** the "Restore fresh settling velocities" comment / the stage-2 `CALC_DERIV` (~:395), insert:
```fortran
                ! Fix F: re-evaluate time-forcing at TIME+dt so stage 2 uses forcing
                ! consistent with its evaluation point (restores 2nd-order forcing terms).
                ! Refreshes SETTLING_VELOCITIES → re-establish the un-suppressed base from
                ! the TIME+dt fresh values; the restore below then applies once (PR #36).
                call UPDATE_TIME_FUNCS &
                     (PELAGIC_BOX_MODEL_DATA  , TIME + TIME_STEP, &
                      FLOWS                   , &
                      BOUND_CONCS             , DISPERSION_COEFFS, INTERFACE_AREAS , &
                      SETTLING_VELOCITIES     , SURFACE_AREAS    , &
                      BOTTOM_AREAS, MASS_LOADS, MASS_WITHDRAWALS , &
                      PRESCRIBED_SEDIMENT_FLUXES)
                SETTLING_VELOCITIES_FRESH = SETTLING_VELOCITIES
```
(The existing `SETTLING_VELOCITIES = SETTLING_VELOCITIES_FRESH` restore stays right after.)

- [ ] **Step 3: Fix V — RK2-average the volume (always).** After the stage-2 `CALC_DERIV` returns (~:407, before the "Final RK2 update" mass loop), insert:
```fortran
                ! Fix V: RK2-average the box volume now the stage-2 volume derivative is
                ! available, so the final conc = mass/VOLUME divides by a 2nd-order volume
                ! (not the 1st-order Euler-predictor volume).
                if (.not. HOLD_VOLUME_CONSTANT) then
                    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
                        PELAGIC_BOX_MODEL_DATA % PELAGIC_BOXES(i) % VOLUME = &
                            VOLUME_OLD(i) + 0.5D0 * &
                            (VOL_DERIV_1(i) + PELAGIC_BOX_MODEL_DATA % VOLUME_DERIVS(i, 1)) &
                            * TIME_STEP
                    end do
                end if
```

- [ ] **Step 4: Build** `make clean-lib && make build-estas` → clean (a new local decl in the block is fine).

- [ ] **Step 5: Euler path still byte-identical** (Part C must not touch Euler):
```bash
./ESTAS_II INPUT.txt        && diff -r OUTPUTS        /tmp/solv_golden_std && echo "STD EULER STILL IDENTICAL"
./ESTAS_II INPUT_200day.txt && diff -r OUTPUTS_200day /tmp/solv_golden_200 && echo "200 EULER STILL IDENTICAL"
```
Expected: both IDENTICAL.

- [ ] **Step 6: RK2 runs stably with the fixes:**
```bash
ESTAS_PELAGIC_SOLVER=2 ./ESTAS_II INPUT_200day.txt > /tmp/rk2fixed.log 2>&1; echo "exit: $?"
grep -q "simulation finished" /tmp/rk2fixed.log && echo "RK2 COMPLETED"
grep -q "UNREALISTIC VALUE" /tmp/rk2fixed.log && echo "BLOWUP — INVESTIGATE" || echo "NO BLOWUP"
```
Expected: exit 0, `RK2 COMPLETED`, zero UNREALISTIC-VALUE. (Negative-mass diagnostic lines are the pre-existing floor artifact — not a failure.)

- [ ] **Step 7: Confirm no env-toggle scaffolding leaked in** (must be all clean):
```bash
grep -nE "RK2_FIX_VOLUME|RK2_FIX_FORCING|ESTAS_RK2_FIX" SOURCE_CODE/ESTAS/mod_SOLVER.f90 && echo "ERROR: toggle leaked" || echo "OK: fixes are always-on"
```

- [ ] **Step 8: Commit** `git commit -am "fix(solver): RK2 volume-averaging + stage-2 forcing re-eval (Heun correctness)"`.

---

### Task 4: Part B — Shiny selector + `assemble_run_env` helper

**Files:** Modify: `shiny_app/build_commands.py` (add helper); `shiny_app/modules/run_control.py` (selector + `on_run` wiring). Test: `tests/python/` (new helper test + extend `test_run_control_module.py`).

- [ ] **Step 1: Write the failing helper test** (`tests/python/test_assemble_run_env.py`):
```python
from shiny_app.build_commands import assemble_run_env

def test_sets_solver_and_composes_with_hold_volume():
    base = {"ESTAS_HOLD_VOLUME": "1"}
    env = assemble_run_env(base, "2")
    assert env["ESTAS_PELAGIC_SOLVER"] == "2"
    assert env["ESTAS_HOLD_VOLUME"] == "1"

def test_default_and_no_mutation():
    base = {}
    env = assemble_run_env(base, "1")
    assert env["ESTAS_PELAGIC_SOLVER"] == "1"
    assert base == {}          # input dict not mutated
```

- [ ] **Step 2: Run it — expect FAIL** (`assemble_run_env` undefined): `pytest tests/python/test_assemble_run_env.py -q`.

- [ ] **Step 3: Add the helper** to `shiny_app/build_commands.py` (near `assemble_estas_command`):
```python
def assemble_run_env(setup_env: dict, solver_value: str) -> dict:
    """Run-subprocess env: copy the setup env (never mutate the shared Setup.env) and
    set the pelagic solver choice. solver_value: "1" Euler (default) | "2" Heun/RK2."""
    env = dict(setup_env)
    env["ESTAS_PELAGIC_SOLVER"] = solver_value
    return env
```

- [ ] **Step 4: Run the helper test — expect PASS.**

- [ ] **Step 5: Add the selector to `run_control_ui`** — a single `ui.input_select("solver_select", "Pelagic solver", choices={"1": "Euler (default)", "2": "Heun / RK2 (experimental)"}, selected="1")` (pass `choices=` as a keyword to match the module's other `input_select` calls), placed with the other run options.

- [ ] **Step 6: Wire `on_run`** — in `run_control.py`, at the `start_run` call (~:569), replace the `dict(st.env)` argument with `build_commands.assemble_run_env(st.env, input.solver_select())`. `build_commands` is already imported in `run_control.py` (~:59) and called module-qualified elsewhere — no new import line needed.

- [ ] **Step 7: Extend the UI-render test** — in `tests/python/test_run_control_module.py`, assert the tagified `run_control_ui` contains a `solver_select` id (follow the existing id assertions).

- [ ] **Step 8: Run the Shiny tests + UI backstop:**
```bash
pytest tests/python/test_assemble_run_env.py tests/python/test_run_control_module.py tests/python/test_ui_renders.py -q
python -c "import shiny_app.app; shiny_app.app.create_ui().tagify()" && echo "UI RENDER OK"
```
Expected: all pass; `UI RENDER OK`.

- [ ] **Step 9: Commit** `git commit -am "feat(shiny): run-control solver selector via assemble_run_env helper"`.

---

### Task 5: Docs (RK2 experimental), full test suite, CI, PR

**Files:** Modify: a README / help doc (state RK2 is experimental).

- [ ] **Step 1: Document RK2 as experimental** — in the relevant README/help (e.g. `shiny_app/README_shiny.md` and/or the main README solver note): "`ESTAS_PELAGIC_SOLVER=2` selects the Heun/RK2 solver (experimental — for this model it converges at ~1st order due to positivity clamping and is not faster/more accurate than the default Euler)." No "better/faster" claim.
- [ ] **Step 2:** `make test-fortran` → green.
- [ ] **Step 3:** `make test-python` (the canonical target, Makefile:365) → green.
- [ ] **Step 4: Commit** the docs.
- [ ] **Step 5: Push** `git push -u origin feature/configurable-pelagic-solver`.
- [ ] **Step 6: Open PR** to `main`. Body: byte-identical Euler default (both setups), the fail-loud check, RK2-stable confirmation, the helper/UI tests, and the honest RK2-experimental framing (link the spec + the investigation write-up). Confirm CI matrix (gfortran/ifx + python-lint-test) green.
- [ ] **Step 7: Merge on the user's go-ahead.**

---

## Self-Review

**Spec coverage:** Part A env read → Task 2. Part C V/F fixes always-on → Task 3 (exact code, toggles stripped, Euler-untouched check + toggle-leak check). Part B selector + pure helper + copy-before-mutate → Task 4 (TDD helper test first). Byte-identical Euler default → Tasks 1/2/3. Fail-loud → Task 2 Step 5. RK2-stable → Task 3 Step 6. Experimental docs → Task 5. No convergence-order gate (dropped per review). All spec sections map to a task.

**Placeholder scan:** No TBD/TODO; exact Fortran + Python given.

**Type consistency:** `ESTAS_PELAGIC_SOLVER` / `PELAGIC_SOLVER_NO` / `assemble_run_env(setup_env, solver_value)` / `solver_select` used identically across tasks and match the spec.
