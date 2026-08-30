# CYN Droop-N, re-scoped: August *Planktothrix* persistence via nitrogen storage
(compile-variant `VARN` build, opt-in `CYN_VARIABLE_N`)

**Date:** 2026-08-30 · **Status:** draft awaiting adversarial review ·
**Supersedes the framing of** `2026-08-01-variable-stoichiometry-cyn-droop-n-design.md`
(whose §3 formulation is inherited with fixes, and whose §12 review verdicts are treated as
binding requirements here).

## 1. Premise — the 2026-08-01 contraindication is measured-removed

The old pilot was rejected on its own decision rule: variable stoichiometry is the wrong
tool unless "a genuinely uptake-limited target exists (`LIM_N ≪ 1`)" and the goal is
phenology rather than drawdown. Both conditions now hold, by measurement (doc §37):

- Under the adopted 2026-08-30 configuration, **August CYN LIM_N = 0.32 at DIN 0.004
  mg N/L** — the fixation window's deepest drawdown, coinciding with the guild's
  temperature optimum (factor 0.78). μ ≈ losses ≈ 0.15 d⁻¹: the observed August
  *Planktothrix* population (2.30 mg C/L vs model 0.22) is excluded by nitrogen.
- Affinity is exhausted: specialist Monod (KHS_DIN 0.003, adopted as X1) caps LIM_N at
  ≈ 0.57 on a 0.004 standing stock. **The standing stock is not the resource; the June
  window (DIN 0.022) and the summer regeneration flux are** — reachable only by storage
  (luxury uptake into quota) and growth-decoupled uptake. That is *P. agardhii* biology
  (phycobiliprotein nitrogen reserve).
- The goal is explicitly NOT summer-DIN drawdown (the old, contraindicated goal): DIN is
  already at 0.004. The goal is **August–October CYN persistence** at observed order.

**Falsifiable outcome, stated up front:** the science ladder (§7) succeeds if August CYN
reaches ≥ 0.8 mg C/L (a 4× gain; obs 2.30) without headline regression; a result < 0.4
is a NULL, to be documented §-style and the build shelved (the machinery remains for the
paper's record). Null is an acceptable outcome; this project's negative results have paid.

## 2. Formulation (inherited from the old spec §3, three fixes)

State: `CYN_N` (mg N/L, transported), quota `Q = CYN_N/CYN_C` bounded `[Q_MIN, Q_MAX]`.

- **Uptake** (mg N/L/d): `R_CYN_N_UPTAKE = VMAX · DIN/(KHS_UPT+DIN) · f_down · CYN_C`,
  `f_down = max(0, (Q_MAX−Q)/(Q_MAX−Q_MIN))`; split NH4/NO3 by the existing
  `PREF_NH4N_CYN`; capped per-step by available DIN (existing safeguard pattern).
- **Growth N-limitation**: `LIM_KG_CYN_N = clamp((Q−Q_MIN)/(Q_MAX−Q_MIN), 0, 1)`
  (Caperon–Meyer), co-limited with P exactly as today. Growth adds C without consuming
  DIN → dilutes Q; uptake refills it.
- **N routing** (all Q-weighted, replacing `CYN_N_TO_C`): respiration→NH4, death→DET_N,
  excretion→DON, grazing→ZOO_N; uptake debits NH4/NO3. Conservation
  `Δ(DIN+CYN_N+DON+DET_N+ZOO_N)|_CYN = 0` asserted by unit test.
- **FIX 1 (old finding 7):** the legacy DON-uptake-during-growth sink on `DISS_ORG_N` is
  ZEROED under the flag (uptake is now explicit); stated as a wired invariant, unit-tested.
- **FIX 2 (old finding 9):** photosynthetic/respiratory O2 remains C-coupled (unchanged
  stoichiometry `CYN_O2_TO_C` on growth/respiration) — quota N does not alter the O2
  budget; stated so DO scoring is untouched by construction.
- **FIX 3:** under the flag, the reported `CYN` chlorophyll continues to use the fixed
  C:Chl (78) — quota N does NOT drive pigment (that would reopen the §22 pigment-inflation
  channel). Photoacclimative Chl stays a separate, explicitly-out-of-scope item.

**Committed constants (old finding 18), as graceful `PELAGIC_MODEL_OPTIONS.txt` lines**
(the house pattern since the staging arc — no WCONST/`nconst` change, no six-site
machinery): `CYN_VARIABLE_N` (0 default), `CYN_N_QMIN 0.10` gN/gC (N-starved,
phycobilin-depleted), `CYN_N_QMAX 0.25` gN/gC (luxury; ≈ 1.4× Redfield 0.176),
`CYN_N_VMAX 0.06` gN/gC/d (fills the reserve in ~2.5 days at saturation),
`CYN_N_KHS_UPT 0.003` mg N/L (= the adopted X1 affinity). Quota seed everywhere
(IC, boundary): `Q_SEED = CYN_N_TO_C` (the model's fixed ratio — verify its value at plan
time and record it in the options-file comment).

## 3. Architecture — the corrected §12.3(b) checklist, decided

1. **State count is compile-time; the pilot is a BUILD VARIANT, not a runtime toggle**
   (old finding 1). `GLOBAL::nstate` stays a `parameter`; a `make build-estas-varn` target
   produces `ESTAS_II_varN` with `nstate = 33` (single-line patch applied at build time to
   a generated copy, never to the tracked source; the standard `ESTAS_II` build is
   untouched and byte-identical by construction). The runtime-`nstate` refactor (reading
   the count from the input) is recorded as the right long-term fix and explicitly
   deferred (blast radius: every automatic array in the ESTAS chain).
2. **Allelopathy-aware indexing** (old finding 3): `CYN_N_INDEX = 33`; the four secondary
   metabolites occupy `nstate+1..nstate+4` = 34–37; the CL29-VARN setup declares **37**
   state variables. The count assert (old finding 2) is made non-tautological:
   `error stop` unless `NUM_PELAGIC_STATE_VARS == nstate` AND (allelopathy ⇒ total
   columns == nstate+4), checked against the declared input counts.
3. **The kinetics target is `CYANOBACTERIA_BOUYANT`** (old finding 4) — the CL29 path
   under `CYANO_BOUYANT_STATE_SIMULATION=1`; the plain `CYANOBACTERIA` variant gets the
   same flag-gated branch for the 0D/test path. Plan-time verify step: trace the actual
   call (`model.f90` ~1096 per the old review) before editing — the parallel-code-paths
   rule.
4. **Flag guard:** `CYN_VARIABLE_N=1` in a standard (`nstate=32`) binary → `error stop`
   at options-read time ("requires the VARN build"), so a mis-paired config cannot run
   silently un-staged (the §34/V3 lesson: fail loud, never fail silent).
5. **CL29-VARN setup** (data repo): `INPUTS_CL29_VARN/` generated from the live
   `INPUTS_CL29/` by a committed generator `tools/make_varn_inputs.py` (old finding 5 —
   the EUTROPY converter's ~6 hardcoded 32s are NOT touched; the generator derives the
   37-var `PELAGIC_INPUTS.txt`, IC and 37-column boundary from the live 36-var files,
   inserting `CYN_N` at 33 with `Q_SEED·CYN_C` values and shifting the metabolite block).
   Driver `INPUT_CL29_VARN.txt`. The live setup is never modified.

## 4. What is deliberately NOT in scope

Droop-P or any second quota (YAGNI; P is not the August binder). Photoacclimative C:Chl
(separate item). DON uptake into the quota (deferred exactly as the old spec deferred it).
The runtime-`nstate` refactor (recorded debt). Any change to the standard build's behavior.

## 5. Files to change

| File | Change |
|---|---|
| `SOURCE_CODE/ESTAS/mod_GLOBAL.f90` | tracked: add `integer :: CYN_VARIABLE_N = 0` only; the `nstate` value is patched exclusively in the build-time generated copy, never in the tracked file |
| `Makefile` / `SOURCE_CODE/build/` | `build-estas-varn` target (patch-copy-build-restore; artifact `ESTAS_II_varN`) |
| `SOURCE_CODE/ESTAS/mod_PELAGIC_ECOLOGY.f90` | 5 graceful option lines + setter + echo + the flag/nstate guard |
| `aquabc_II_pelagic_svindex/…` (index module) | `CYN_N_INDEX=33` + metabolite offsets from `nstate` (not literals) |
| `…lib_CYANOBACTERIA.f90` (both variants) | flag-gated Droop branch: uptake, quota limitation, Q export |
| `aquabc_II_pelagic_model.f90` | `CYN_N` derivative + Q-weighted N routing + DON-sink zeroing + PROCESS_RATES slots |
| `tools/make_varn_inputs.py` | NEW: 36→37-var setup generator |
| `tests/fortran/test_cyanobacteria.f90` + new `test_cyn_droop.f90` | quota kinetics unit tests + the conservation identity + the two wired invariants |
| `tools/check_staging_run.py` or new checker | VARN smoke: flag echo, CYN_N present, conservation audit |

## 6. Verification ladder

V1 unit tests (quota kinetics: f_down endpoints, Caperon–Meyer clamp, conservation
identity to 1e-12, DON-sink-zero and O2-unchanged invariants). V2 standard build
byte-identity (0D golden + CL29 30-day A/B — trivially expected since `nstate` and all
flag-off paths are untouched; still run). V3 VARN positive-control smoke (flag echo with
all four scalar values; `CYN_N` column present and `Q∈[Q_MIN,Q_MAX]` everywhere; loud-stop
check: flag=1 on the standard binary must abort). V4 conservation audit on a 90-day VARN
run (the §2 identity from cumulative process rates). V5 both-solver check (Euler vs RK2
90-day: no NaN/abort, quota bounds `[Q_MIN, Q_MAX]` hold everywhere under both, and the
conservation identity closes under both — trajectory-level agreement is NOT asserted, the
solvers legitimately differ at their integration order; `CYN_N` is a normal transported
state, so none of the staging arc's solver-side special handling applies — assert its
absence by grepping the diff).

## 7. The science ladder and its falsifiable outcome

Full-record VARN run on the adopted configuration + committed constants: **[a]** August
CYN ≥ 0.8 mg C/L = success; < 0.4 = NULL (documented, shelved); between = judgment call
presented with the numbers. **[b]** No headline regression beyond noise (CHLA 24.05,
PO4 0.0170, r +0.68, peak-month margin). **[c]** Report the quota's seasonal cycle (June
loading → August depletion — the mechanism's own signature; if Q never reaches Q_MAX in
June or never draws below mid-range in August, the storage hypothesis is mechanistically
refuted regardless of biomass), the fixer interaction, and October–November persistence.
**[d]** A KHS/VMAX one-step sensitivity pair (±2×) so the conclusion is not hostage to two
committed guesses. Adoption remains a separate user decision; a VARN-adoption would make
the VARN build the operational binary — a deployment question flagged for that decision.

## 8. Risks

The 2026-08-01 review's findings 5/9/14/16/21 are each addressed above by construction
(generator instead of converter edits; O2 invariant; single-mechanism A/B — the Droop
branch is the ONLY delta vs the adopted config; flag-ON smoke; conservation test in the
named harness). Residual risks: the storage magnitude may be insufficient against
transport dilution (the null outcome — acceptable); the 37-var boundary file doubles a
§26-era artifact chain (generator-tested); build-variant divergence over time (mitigated:
the patch is one line, CI builds both targets).
