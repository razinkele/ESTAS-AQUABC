# Configurable Pelagic Solver (Euler / Heun-RK2) — Design

**Status:** Approved (2026-07-24)
**Scope:** Two subsystems — ESTAS Fortran + the Shiny app. A behavior-*enabling* change (opt-in),
byte-identical by default.
**Backlog item:** "Expose the existing RK2/Heun solver via config" (BACKLOG.md §1 / P2). The RK2 code
exists and is dispatched (`mod_SOLVER.f90:314`, `PELAGIC_SOLVER_NO == 2`) but is unreachable —
`PELAGIC_SOLVER_NO` is a local in `mod_SIMULATE` hard-coded to `1` (Euler) at `:93`.

## Verified precondition — RK2 works (throwaway spike, 2026-07-24)

Before designing, RK2 was confirmed functional: hard-coding `PELAGIC_SOLVER_NO = 2`, rebuilding, and
running `INPUT_200day.txt` (pelagic-only) → completes cleanly (exit 0, no NaN/Inf), sensible two-stage
derivatives (`K1 ≈ K2 ≈ -24.9`), and **21/23 output files differ from Euler** (proving the `:314` branch
is genuinely taken). So this is "wire up a working solver," not "fix a broken one." Hack reverted.

## Config mechanism — env var `ESTAS_PELAGIC_SOLVER`

The solver choice is an **environment variable**, mirroring the existing `ESTAS_HOLD_VOLUME` runtime
toggle (`mod_SOLVER.f90:130`). Values: **unset or `1` → Euler (default)**, `2` → Heun/RK2. Anything
else → **hard stop** with a clear message (no silent fallback). This was chosen over an input-file flag
because (a) it's a single localized Fortran read — no GLOBAL scalar, no EOF-tolerant input reader; (b)
it matches the codebase's own precedent for a result-changing toggle; (c) the Shiny app already drives
runtime toggles as subprocess env (`app_state.py:147` `run_env.update(env_extra)`), so the UI is a
one-line env addition. Accepted tradeoff: the choice is not recorded in the committed setup file
(weaker archival reproducibility than an input flag) — mitigated by a **loud echo of the resolved
value** so the run log captures it. **No "both" mechanism / precedence rule** (that is a silent-surprise
generator).

The env var is the **interface contract** between the two subsystems: Fortran reads it, Shiny sets it.
The two edits are independent and independently verifiable.

## Part A — Fortran (`mod_SIMULATE.f90`, subroutine `RUN_SIMULATION`)

Replace the hard-coded assignment at `:93` (`PELAGIC_SOLVER_NO = 1`) with an env-var read mirroring the
`ESTAS_HOLD_VOLUME` idiom:

```fortran
! Solver selection (default Euler). RK2/Heun is opt-in via env var, mirroring ESTAS_HOLD_VOLUME.
call get_environment_variable('ESTAS_PELAGIC_SOLVER', SOLVER_ENV)
select case (trim(adjustl(SOLVER_ENV)))
    case ('', '1')
        PELAGIC_SOLVER_NO = 1
    case ('2')
        PELAGIC_SOLVER_NO = 2
    case default
        write(6,*) 'ERROR: ESTAS_PELAGIC_SOLVER must be 1 (Euler) or 2 (Heun/RK2), got "' &
                   // trim(adjustl(SOLVER_ENV)) // '"'
        stop 1
end select
write(6,*) 'PELAGIC_SOLVER = ', PELAGIC_SOLVER_NO, ' (1 = Euler, 2 = Heun/RK2)'
```

- Add a local `character(len = 32) :: SOLVER_ENV` to `RUN_SIMULATION`'s declarations. `get_environment_variable`
  is intrinsic (no new `use`). `PELAGIC_SOLVER_NO` stays a local passed to `SOLVE` exactly as today.
- **Default is byte-identical:** env unset → `1` → identical to current behavior for every committed
  setup. Only an explicit `ESTAS_PELAGIC_SOLVER=2` changes numerics (the intended opt-in).
- **No other Fortran change.** The RK2 dispatch (`mod_SOLVER.f90:314`) and its guarded diagnostics
  (negative-mass at `:428`, unrealistic-value at `:447`) are left as-is — they are conditional error
  warnings shared with the Euler path (`:188`), **not** unconditional debug spew, and are worth keeping.

## Part B — Shiny (`shiny_app/modules/run_control.py`)

- Add a **single solver selector** to the run-control UI panel (radio or `input_select`): "Euler
  (default)" → `"1"`, "Heun / RK2" → `"2"`, default `"1"`.
- In the run launcher (`on_run`, which calls `run.start_run(estas_cmd, exe_name, dict(st.env), st.input_file)`
  at `:569`), merge the selector value into the env dict:
  `env = dict(st.env); env['ESTAS_PELAGIC_SOLVER'] = <selector value>` and pass `env` as `env_extra`.
  This composes cleanly with the per-setup `ESTAS_HOLD_VOLUME` already in `st.env` (additive; distinct
  keys).
- Default selector = Euler → default runs unchanged. **Scope: run-control tab only.** The Dashboard
  quick-run (`dashboard.py:377`) stays at the setup default (Euler); surfacing the selector there too is
  a later follow-up, not this change. No `setups.py` registry change (the solver is orthogonal to the
  setup).

## Non-goals (YAGNI)

- **No input-file flag / GLOBAL scalar / precedence rule** — env var only.
- **No change to the RK2 numerics or its diagnostics.**
- **No Dashboard quick-run selector**, no setup-registry change, no per-setup solver default.
- **No new solvers** (5.3 is a separate item) — only exposing the existing Euler/RK2.

## Verification

1. **Byte-identical default (Fortran).** Build; with `ESTAS_PELAGIC_SOLVER` **unset**, run a committed
   setup (`INPUT_200day.txt` and/or Standard `INPUT.txt`) and diff `OUTPUTS/` bit-for-bit against a
   pre-change golden — must be identical (the default path is unchanged). Binary `./ESTAS_II`.
2. **RK2 actually engages.** With `ESTAS_PELAGIC_SOLVER=2`, rerun the same setup → outputs differ from
   the Euler golden (and the run log shows `PELAGIC_SOLVER = 2`).
3. **Convergence check (acceptance — proves the 2nd-order solver earns its place).** Take Euler at a
   very small timestep as the reference solution; then compare Euler@h vs RK2@h against that reference
   for a representative state variable. Confirm **RK2@h is closer to the reference than Euler@h** (2nd-
   vs 1st-order). A small script (`tools/` or `tests/`) that runs the model 3× at chosen `TIME_STEPS_PER_DAY`
   and compares a box concentration is sufficient; record the error ratio.
4. **Invalid value fails loud.** `ESTAS_PELAGIC_SOLVER=3 ./ESTAS_II …` → non-zero exit + the clear error
   message; does not silently run Euler.
5. **Resolved-value echo** appears in stdout for 1 and 2 (run-log capture of the choice).
6. **Shiny (Part B).** A Python test (in the existing `tests/python` / `shiny_app` test style) that the
   run-control launcher merges the selector value into the env dict as `ESTAS_PELAGIC_SOLVER`, composes
   with a pre-existing `ESTAS_HOLD_VOLUME`, and defaults to `"1"`. Plus `create_ui().tagify()` stays
   green (the UI-render backstop). Optionally a Playwright smoke: pick RK2, run, see `PELAGIC_SOLVER = 2`
   in the log.
7. **`make test-fortran`** green; **CI** (gfortran/ifx + python-lint-test) green.

## Rollout

Single PR on `feature/configurable-pelagic-solver`. Two independent commits (Fortran env read; Shiny
selector) + the convergence-check script. Green CI + byte-identical default + the convergence result in
the PR body, then merge on the user's go-ahead.
