# Surface-Bloom Persistence State (Positional Ratchet) — Mini-Plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:executing-plans, inline.

**Goal:** Give the buoyant cyanobacteria groups the *state* that §19's three-rung ladder
proved necessary: a surface-concentrated biomass fraction S ∈ [0,1] per box per group that
**builds during calm spells, persists across days, and is dispersed by storms** — the
positional ratchet whose F→1 limit reproduces the observed Aug–Oct chlorophyll plateau.

**Mechanism (opt-in `CYANO_POS_MODEL = 2`; =1 keeps the memoryless daily blend; =0 legacy,
byte-identical):** per group g and box k,

```
dS/dt = K_POS_UP · F_calm · (1 − S)  −  K_POS_DISP · F_storm · S
F_calm  = F(W_crit / W_day)        (the §19 ERA5-hourly CDF; W_crit ≥ W_CRIT_POS_MIN)
F_storm = 1 − F(W_DISP_POS / W_day) (fraction of the day above the dispersal wind)
LIM_light = (1 − S)·LIM_cascade + S·LIM_surface(H_SURF_POS)
```

Forward-Euler with the kinetic TIME_STEP, clamped to [0,1]. Defaults (literature scales,
graceful option lines): `K_POS_UP = 3 d⁻¹` (colonies rise ~0.5–3 m/h → a calm day builds
S ≈ 0.95), `K_POS_DISP = 10 d⁻¹` (storm disperses a scum in hours), `W_DISP_POS = 4 m/s`
(empirical dispersal threshold; > the 3 m/s formation floor — hysteresis is the ratchet).

**State storage:** a new small module `AQUABC_POSITIONING_STATE` holding
`S_POS(nkn, 3)` (CYN / FIX_CYN / NOST), allocated on first call, initialized to 0; updated
inside the three `_BOUYANT` gates over their [ns:ne] chunks (OpenMP-safe: disjoint rows;
no reduction). A `RESET_POSITIONING_STATE()` entry for unit tests. NOT a transported state:
S is positional memory, carries no mass, stays with its box (documented limitation — real
scums drift with surface currents). Euler-solver semantics documented (the experimental
Heun path would double-integrate S; acceptable, noted).

**Shared plumbing:** refactor the §19 CDF expression into one pure function
`CALM_FRACTION(W_day, W_thresh)` in the auxiliary module, used by both =1 and =2 paths and
both thresholds.

**Steps:**
- [ ] 1. Module + function + the three gate blocks (=2 branch alongside =1); 3 new graceful
  option constants; thread nothing new through signatures (all inputs already present:
  WINDS, TIME_STEP, EUPHOTIC_DEPTH, the option constants arrive as the =1 trio + 3 more —
  extend the options read and pass K_POS_UP/K_POS_DISP/W_DISP_POS as 3 more dummies).
- [ ] 2. Build clean; Fortran unit test: calm-day sequence ratchets LIM upward across calls,
  one storm day collapses it (uses RESET_POSITIONING_STATE).
- [ ] 3. Byte gate (=0 default) vs the standing /tmp/gate5_pre reference.
- [ ] 4. Commit.
- [ ] 5. Decisive run: honest config + real wind + `CYANO_POS_MODEL 2` (defaults) — does
  August approach the ~45–50 F→1 extrapolation while winter stays exact? Sensitivity ladder
  if needed: W_DISP_POS ∈ {3.5, 4, 5}.
- [ ] 6. Doc §20 + verdict + adoption decision (user).

**Honesty notes:** self-shading remains column-uniform when positioned (same 2019 caveat);
S is not advected; the formation/dispersal constants are scale values pending literature
anchoring (Visser flotation velocities; Webster/Hutchinson dispersal winds) — flag for the
paper if adopted; EPA surface sampling vs model column mean remains an interpretive caveat
that *shrinks* as S→1 (a positioned model column IS the surface).
