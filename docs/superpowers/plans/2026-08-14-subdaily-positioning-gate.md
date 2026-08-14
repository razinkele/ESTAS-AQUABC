# Sub-Daily Positioning Gate (Wind-Plan Task 5) — Mini-Plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:executing-plans, inline.

**Goal:** Let the cyanobacteria surface-positioning light gates capture the *diurnal calm
windows* that daily-mean wind erases — the measured reason positioning never engages
(§18: 0 % full engagement under honest optics; the real scums form in within-day calms).

**Mechanism (opt-in `CYANO_POS_MODEL = 1`, graceful options line; default byte-identical):**
in each `_BOUYANT` smith==1 gate (CYN / FIX_CYN / NOSTOCALES), blend the existing
cascade-depth light factor with a surface-layer factor, weighted by the fraction of the day
the hourly wind sits below the positioning-critical speed:

```
W_crit = max((EUPHOTIC_DEPTH − 0.7006)/0.8121, 0)        (MIX(W) = euphotic, Nagy inverted)
x      = min(W_crit / max(W_daily, 0.1), 1)              (x ≥ 1 → cascade already positions)
F_calm = exp(min(0, 0.6218·(ln x)² + 3.8137·ln x − 0.7987))
LIM    = (1 − F_calm)·LIM_cascade + F_calm·LIM_surface(H = min(H_SURF_POS, DEPTH))
```

The F_calm form is the within-day wind CDF fitted from ERA5 hourly Nida 2012–2022
(96,432 h; `P(W_h ≤ x·W_day)`; max fit error < 0.05 for x ≤ 1, < 0.01 for x ≤ 0.75 — fitted
2026-08-14, session log). `H_SURF_POS` (m, default 0.5) is the second graceful constant.

**Files:** mod_GLOBAL (2 globals) · READ_PELAGIC_MODEL_OPTIONS (2 graceful pairs after
ZOO_CLOSURE_REF) · ESTAS call + AQUABC_PELAGIC_KINETICS signature (+2 args) · the three
`_BOUYANT`/NOSTOCALES libs (+2 dummies, gate block, work arrays) · 0D interface (legacy
literals `0, 0.5D0`) · tests/fortran/test_fix_cyn.f90 + test_cyanobacteria.f90 (call updates
+ one new blend test: F_calm at x=0.5 doubles nothing when model=0; model=1 with calm wind
raises the light factor).

**Gates:** clean build, no new warnings · 30-day byte gate (default path) vs pre-change
binary · Fortran suite green · opt-in verification: Task-4 config + `CYANO_POS_MODEL 1`,
full record — the decisive question: August toward 50.8 with winter intact.

**Honesty notes:** self-shading stays column-uniform when positioned (inherited limitation,
documented since 2019); the surface fraction experiences kd-attenuated light over H_surf
with the SAME kd (conservative); EPA obs are surface samples — if the run lands close,
quantify the residual surface-vs-average sampling mismatch as a diagnostic.
