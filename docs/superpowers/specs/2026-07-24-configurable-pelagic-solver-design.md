# Configurable Pelagic Solver (experimental Heun/RK2) + RK2 correctness fixes — Design

**Status:** Approved (2026-07-24, revised post-investigation)
**Scope:** Two subsystems — ESTAS Fortran + the Shiny app — plus a self-contained RK2 solver
correctness fix. A behavior-*enabling* change (opt-in), byte-identical by default.
**Backlog item:** "Expose the existing RK2/Heun solver via config" (BACKLOG.md §1 / P2). RK2 code
exists and is dispatched (`mod_SOLVER.f90:314`) but is unreachable — `PELAGIC_SOLVER_NO` is a local in
`mod_SIMULATE` hard-coded to `1` (Euler) at `:93`.

## Investigation outcome (why RK2 ships as EXPERIMENTAL)

A spike + a 4-way in-loop review + a convergence experiment established:
- **RK2 is a correctly-implemented, stable Heun method** (PR-#36 settling fix correctly placed). It
  runs to completion, no blowups, ~25% smaller *same-step* error than Euler.
- **But its OUTPUT (concentration) converges at ~1st order, not 2nd,** with three causes:
  1. **Volume Euler-update** (`mod_SOLVER.f90:364`): box `VOLUME` advanced 1st-order while mass is
     2nd-order → `conc = mass/VOLUME` is 1st-order. *Solver-level, fixed here (Fix V).*
  2. **Forcing frozen at `TIME`**: `UPDATE_TIME_FUNCS` called once at `:323`; stage-2 `CALC_DERIV`
     runs at `TIME+dt` but reuses `TIME` forcing. *Solver-level, fixed here (Fix F).*
  3. **`MIN_CONCENTRATION` clamping** (100k–950k events/run): trace state vars go negative every step
     and are floored — a **non-smooth** operation that caps the achievable order for *any* solver
     (even Euler's own self-convergence is irregular, order −0.4…+2.7). **Model-level, NOT fixed here.**
- **At equal compute RK2 is ~55–70% *worse* than Euler** (2× cost per step, unamortized because the
  effective order is ~1), and Euler is stable at every tested step. So RK2 is **not** a better solver
  for this model.

**Decision:** expose RK2 as a **documented experimental option** (satisfies "expose it"), and bank the
two correct solver fixes (V, F) — but make **no claim that RK2 is more accurate/higher-order** for this
model. Clean 2nd-order would require addressing the positivity clamping (a separate model-formulation
project).

## Part A — Fortran config read (`mod_SIMULATE.f90`, subroutine `RUN_SIMULATION`)

Replace the hard-coded `PELAGIC_SOLVER_NO = 1` at `:93` with an env-var read mirroring the
`ESTAS_HOLD_VOLUME` idiom (`mod_SOLVER.f90:130`). Env var **`ESTAS_PELAGIC_SOLVER`**: unset/`1` → Euler
(default), `2` → Heun/RK2, else → hard stop.

```fortran
character(len = 32) :: SOLVER_ENV       ! add to RUN_SIMULATION declarations
...
call get_environment_variable('ESTAS_PELAGIC_SOLVER', SOLVER_ENV)
select case (trim(adjustl(SOLVER_ENV)))
    case ('', '1');  PELAGIC_SOLVER_NO = 1
    case ('2');      PELAGIC_SOLVER_NO = 2
    case default
        write(6,*) 'ERROR: ESTAS_PELAGIC_SOLVER must be 1 (Euler) or 2 (Heun/RK2), got "' &
                   // trim(adjustl(SOLVER_ENV)) // '"'
        error stop 1
end select
write(6,*) 'PELAGIC_SOLVER = ', PELAGIC_SOLVER_NO, ' (1 = Euler, 2 = Heun/RK2, experimental)'
```

- In-loop review (dimension 1) verified this against source: single assignment site, unset→Euler,
  every invalid value hits `case default` (no silent mis-default), `RUN_SIMULATION` runs once, and the
  byte-identical-default claim holds at both the OUTPUTS level and the CI level (`ci.yml` uses a
  `grep -q "simulation finished"` substring check, not a golden-log). Use `error stop 1` (a plain
  numeric `stop 1` also gives exit 1, but the codebase has a latent gotcha where a *string*-form
  `stop 'msg'` returns exit 0 — `error stop` is unambiguous and matches `sub_OPEN_INPUT_FILE.f90:27`).
- **Default byte-identical:** env unset → `1` → identical numerics for every committed setup; the only
  new default-path behavior is the informational `write(6,*)` to stdout (not an `OUTPUTS/` file).

## Part B — Shiny selector (`shiny_app/`)

- Add a **single solver selector** to the run-control UI (`run_control_ui`): "Euler (default)" → `"1"`,
  "Heun / RK2 (experimental)" → `"2"`, default `"1"`.
- **Extract a pure helper** (per in-loop review — the merge must be unit-testable, not buried in the
  `on_run` closure). In `build_commands.py`, mirroring `assemble_estas_command`:
  ```python
  def assemble_run_env(setup_env: dict, solver_value: str) -> dict:
      env = dict(setup_env)                    # copy — never mutate the shared Setup.env
      env['ESTAS_PELAGIC_SOLVER'] = solver_value
      return env
  ```
- In `run_control.py`'s `on_run`, replace `dict(st.env)` in the `start_run` call (`:569`) with
  `assemble_run_env(st.env, input.solver_select())`. This composes additively with the per-setup
  `ESTAS_HOLD_VOLUME` (distinct keys), and — because `run_env.update(env_extra)` is the last mutation
  before `Popen` (`app_state.py:147`) — a default `"1"` also safely overrides any ambient shell
  `ESTAS_PELAGIC_SOLVER`.
- ⚠️ **`dict(setup_env)` copy is load-bearing:** `Setup.env` is a mutable dict on the module-level
  `SETUPS` list shared across sessions; mutating it in place would poison it globally.
- **Scope: run-control tab only.** The Dashboard quick-run (`shiny_app/modules/dashboard.py:377`,
  independent `Popen`) stays Euler-default — surfacing the selector there is a later follow-up.

## Part C — RK2 correctness fixes (always-on in the RK2 branch, `mod_SOLVER.f90:314–453`)

Fold in the two verified solver fixes (preserved diff: `.superpowers/sdd/rk2-VF-fix.diff`), **not**
env-toggled — they are permanent corrections to the RK2 method. They only affect `PELAGIC_SOLVER_NO==2`
runs (RK2 is opt-in, currently unreachable → no existing RK2 output to preserve); the **Euler path
(`:137–311`) is untouched** (byte-identical).

- **Fix V (volume RK2-average):** before the predictor loop, save per-box `VOLUME_OLD(i)` and
  `VOL_DERIV_1(i) = VOLUME_DERIVS(i,1)`. Keep the Euler predictor volume advance (`:364–367`, needed
  for the predicted concentrations stage 2 evaluates). After stage-2 `CALC_DERIV`, recompute
  `VOLUME(i) = VOLUME_OLD(i) + 0.5·(VOL_DERIV_1(i) + VOLUME_DERIVS(i,1))·dt` (guarded by
  `.not. HOLD_VOLUME_CONSTANT`); the final `conc = mass/VOLUME` then divides by the averaged volume.
- **Fix F (forcing re-eval):** add a second `call UPDATE_TIME_FUNCS(… TIME+TIME_STEP …)` immediately
  before the stage-2 `CALC_DERIV` so FLOWS/BOUND_CONCS/MASS_LOADS/etc. are evaluated at `TIME+dt`.
  ⚠️ It re-evaluates `SETTLING_VELOCITIES`, so re-save `SETTLING_VELOCITIES_FRESH = SETTLING_VELOCITIES`
  after it (preserving the PR-#36 no-double-suppression intent from the `TIME+dt` fresh base).

These make RK2 the correct 2nd-order Heun method; the *output* order stays ~1 for this model because
of cause #3 (clamping) — that is documented, not fixed.

## Non-goals (YAGNI)

- **No claim that RK2 is more accurate / higher-order for this model** — it is experimental.
- **No positivity/clamping rework** (cause #3) — a separate project.
- **No input-file flag / GLOBAL scalar / precedence rule** — env var only.
- **No Dashboard quick-run selector**, no setup-registry change.
- **No env-toggles for V/F** — they are permanent RK2 corrections, not options.

## Verification

1. **Byte-identical Euler default (Fortran).** Build; `ESTAS_PELAGIC_SOLVER` unset → run a committed
   setup (`INPUT_200day.txt`, Standard `INPUT.txt`) → diff `OUTPUTS/` bit-for-bit vs a pre-change
   golden. Identical. (Part A + Part C must not perturb the Euler path.)
2. **Invalid value fails loud.** `ESTAS_PELAGIC_SOLVER=3 ./ESTAS_II …` → non-zero exit + the error
   message; never silently Euler.
3. **RK2 runs + is the corrected Heun method.** `ESTAS_PELAGIC_SOLVER=2` → completes, log shows
   `PELAGIC_SOLVER = 2`; confirm Fix V (final volume is the 0.5·(v1+v2)·dt average, not the Euler
   predictor) and Fix F (a second `UPDATE_TIME_FUNCS(TIME+dt)` precedes stage-2 `CALC_DERIV`, with the
   `SETTLING_VELOCITIES_FRESH` re-save) are present and correct.
4. **RK2 is stable (not a blowup).** The mode-2 run completes with no `UNREALISTIC VALUE` stop; the
   negative-mass diagnostics are the same pre-existing floor artifact as Euler (do not treat as a new
   failure).
5. **Shiny helper (Part B).** Python test (tests/python style): `assemble_run_env({'ESTAS_HOLD_VOLUME':'1'}, '2')`
   → both keys present, `ESTAS_PELAGIC_SOLVER == '2'`; `assemble_run_env({}, '1')` → `'1'`; and it does
   NOT mutate the input dict. Plus `run_control_ui` tagifies with the new `solver_select` id (extend
   `tests/python/test_run_control_module.py`), and `create_ui().tagify()` stays green.
6. **`make test-fortran`** green; **CI** (gfortran/ifx + python-lint-test) green.
7. **Documentation.** README / help text states RK2 is experimental and ~1st-order-limited for this
   model (the honest framing) — no "faster/more accurate" claim.

*No convergence-order acceptance gate* (the earlier "RK2@h closer than Euler@h" test was refuted as
cherry-pickable and structurally flattering to the 2×-cost method; RK2 is not being shipped as a
better solver).

## In-loop review hardening (2026-07-24)

Three adversarial reviewers + a live convergence experiment:
- **Fortran env-read (dim 1): SAFE** — compiled the exact snippet, edge-case-tested all inputs,
  confirmed byte-identical default at OUTPUTS + CI level, single assignment site, once-per-run.
- **RK2 + convergence (dim 2): NEEDS-CHANGES (folded in)** — RK2 correct + stable, but converges at
  ~1st order and loses at equal compute; the original acceptance criterion was unsound. → reframed to
  experimental, dropped the 2nd-order claim, dropped the convergence gate, added Part C fixes.
- **Shiny (dim 3): NEEDS-CHANGES (folded in)** — wiring sound, but the merge must be a pure
  `assemble_run_env` helper (testable) with copy-before-mutate; corrected the `dashboard.py` path
  citation.
- **Systematic-debugging investigation** — root-caused the 1st-order behavior to the volume update,
  the forcing freeze, and (dominantly) the `MIN_CONCENTRATION` clamping non-smoothness; implemented +
  verified Fix V / Fix F (env-toggled during the experiment, made permanent here); write-up
  `.superpowers/sdd/rk2-convergence-experiment.md`, fix diff `.superpowers/sdd/rk2-VF-fix.diff`.

## Rollout

Single PR on `feature/configurable-pelagic-solver`. Commits: (C) RK2 V+F fixes; (A) env read; (B) Shiny
selector + helper; docs. Green CI + byte-identical Euler default + the fail-loud + Shiny helper tests,
then merge on the user's go-ahead.
