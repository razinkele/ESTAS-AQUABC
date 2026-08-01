# Pelagic-Core `GLOBAL` De-globalization — Design / Scope

**Date:** 2026-08-01
**Status:** scope / design (for review — not yet a plan)

## Goal

Bundle the **12 remaining loose pelagic water-column allocatables** in `mod_GLOBAL` into a single derived
type `pelagic_core_t` with one module instance `pcore`, dropping `GLOBAL`'s allocatable count **12 → 0**.
Byte-identical (a pure `X` → `pcore%X` rename), done in **3 risk-tiered sub-slices** (per the chosen
sequencing). This is the **final** Phase-5.1 slice (§8.1 Task 5.1) and the hardest — the core arrays are
used throughout the kinetics and solver, and their names double as SHYFEM-library dummy arguments and
local variables, so a scope-blind rename is a silent semantic bug.

## Current state (verified against the tree)

The 12 allocatables (`mod_GLOBAL.f90:96-130`):

| Array | GLOBAL-resolving refs (in `use GLOBAL` files) | total refs | notes |
|---|---|---|---|
| `PROCESS_RATES` | 646 | 736 | the giant |
| `MODEL_CONSTANTS` | 356 | 368 | almost all ESTAS-side |
| `STATE_VARIABLES` | 106 | 234 | |
| `DERIVATIVES` | 93 | 165 | |
| `DRIVING_FUNCTIONS` | 50 | 92 | |
| `SAVED_OUTPUTS` | 38 | 48 | ⚠️ also a `PELAGIC_BOX` component + a dummy arg |
| `FLAGS` | 35 | 88 | ⚠️ prior shadow bug (advredox non-determinism) |
| `CHLA` | 27 | 68 | ⚠️ also a **scalar** local (`mod_PELAGIC_ECOLOGY:318`) + dummy args |
| `pH` | 25 | 310 | ⚠️ extreme: only 25 of 310 are the GLOBAL array |
| `node_active` | 21 | 66 | ⚠️ `intent(in)` dummy arg throughout the pelagic lib |
| `SURFACE_BOXES` | 6 | 23 | ⚠️ dummy arg + separate allocatables in 0D/interface/benchmark |
| `WATER_COLUMN_OUTPUT` | 0 | 1 | **DEAD** — only the declaration exists; never allocated/used |

- **Touch set = the 19 files that `use GLOBAL`** (not the whole tree). The pelagic library files
  (`aquabc_II_pelagic_model.f90`, `..._auxillary.f90`, the sediment libs) receive these arrays as **dummy
  arguments** — those sites are NOT renamed.
- **Alloc:** `mod_AQUATIC_MODEL.f90:209-219`. **Dealloc:** `ESTAS_II.f90:74-83`. Both are part of the touch
  set (`allocate(pcore%X(...))`, `deallocate(pcore%X)`).
- **The core hazard (the wsc lesson):** these names shadow. A `use GLOBAL` file can *also* have a same-named
  dummy arg, local, or derived-type component (e.g. `mod_PELAGIC_ECOLOGY` uses GLOBAL **and** has a scalar
  `CHLA` local **and** a `PELAGIC_BOX % SAVED_OUTPUTS` component). Renaming a shadow site to `pcore%X` is
  **byte-identical AND gate-invisible** yet semantically wrong. Per-site scope-aware review is the
  load-bearing safety net — not a rename script.

## Design

Define `pelagic_core_t` and its single module instance `pcore` **inside `mod_GLOBAL`** (the arrays' current
home, so every existing `use GLOBAL` keeps working — no new module, no dependency cycle). The 12 members are
moved into the type across the 3 tiers; each tier moves its arrays and rewrites their GLOBAL-resolving
references `X` → `pcore%X`.

```fortran
type :: pelagic_core_t
    integer,          allocatable :: node_active(:)
    real(kind=DBL),   allocatable :: STATE_VARIABLES(:,:)
    ! … members added tier by tier …
end type
type(pelagic_core_t) :: pcore
```

### The 3 risk tiers (chosen sequencing)

| Tier | Arrays | GLOBAL-resolving sites | risk |
|---|---|---|---|
| **1 (this slice)** | `WATER_COLUMN_OUTPUT` (drop, dead), `SURFACE_BOXES` (6), `node_active` (21), `CHLA` (27), `SAVED_OUTPUTS` (38) | ~92 | low-med (CHLA/SAVED_OUTPUTS in-file shadows) |
| **2** | `FLAGS` (35), `DRIVING_FUNCTIONS` (50), `DERIVATIVES` (93) | ~178 | med (FLAGS shadow history) |
| **3** | `pH` (25 of 310), `STATE_VARIABLES` (106), `MODEL_CONSTANTS` (356), `PROCESS_RATES` (646) | ~1133 | high (pH extreme shadow; volume) |

Each tier is an independent, byte-identical PR; `pelagic_core_t` grows monotonically.

### Method per tier (proven on resuspension / sediment / wsc)

1. **Determinism pre-check** — confirm the gate configs are deterministic run-to-run before changing anything.
2. **Enumerate** every candidate site: `grep -nwE` the array within the 19 `use GLOBAL` files → an
   explicit `file:line old→new` list.
3. **Scope-aware per-site review** (load-bearing): for each site, decide whether the name resolves to the
   GLOBAL array (rename) or a local/dummy/component (skip). Record rename-vs-skip per line. This replaces
   any blind sed/script rename — the wsc slice proved scripts produce byte-identical-but-wrong shadow
   mis-renames.
4. **Apply** the renames + move the declaration into `pelagic_core_t` + retarget alloc/dealloc.
5. **Build** (gfortran release; also OpenMP + ifx in CI).
6. **Byte-identity gate:** Standard (25-box) and CL29 runs produce **identical** output before/after
   (these are core arrays exercised by every run — no flag-forked gate needed, unlike the sediment slice).
   Use strip-and-compare on the diff to prove a pure `pcore%`-prefix change where feasible.
7. **CI matrix** green (gfortran macOS/ubuntu, ifx, integration-tests, python-lint) → merge.

## Byte-identity / regression constraints

- **Every existing run stays byte-identical** — this is a pure rename, no semantics change. Verify with
  Standard 25-box and CL29 golden runs identical to the pre-change binary's output.
- `INPUTS/FLOW_TS.txt` stays out of every commit (explicit pathspec).

## Risks & mitigations

1. **Shadow mis-rename (the load-bearing risk).** Mitigation: enumerated per-site review, not a script;
   Tier-3 `pH` gets its own dedicated review pass (25 needles in 310). Byte-identity alone does NOT catch
   these — the review is the real gate.
2. **Dead-array assumption.** `WATER_COLUMN_OUTPUT` verified dead (0 refs); dropping it must not change any
   output (it never fed one). If a later tier finds a hidden use, revisit.
3. **Alloc/dealloc ordering.** Move both the `mod_AQUATIC_MODEL` alloc and the `ESTAS_II` dealloc together
   per array so `pcore%X` is always allocated before use and freed once.
4. **OpenMP.** `pcore` is a module-level shared instance (same sharing as the current GLOBAL arrays); the
   parallel kinetics region reads/writes members exactly as it does the loose arrays today — no change to
   the threading contract. Verify the OpenMP build byte-identical too.

## Tier 1 — this slice

Move `SURFACE_BOXES`, `node_active`, `CHLA`, `SAVED_OUTPUTS` into `pelagic_core_t`/`pcore` (~92 sites),
and **delete** the dead `WATER_COLUMN_OUTPUT` declaration. Key per-site care: the `CHLA` scalar local and
the `SAVED_OUTPUTS` `PELAGIC_BOX` component in `mod_PELAGIC_ECOLOGY`/`mod_PELAGIC_BOX` are shadows — skip
them. Deliverable: one byte-identical PR; Standard + CL29 golden runs identical before/after.

## Out of scope

- Any semantic/behaviour change (this is structural only).
- Splitting into concern-based types (decided: one `pcore` bundle).
- Truly removing global state (the instance `pcore` is still module-level, like the prior slices — the goal
  is bundling the loose allocatables, reducing `GLOBAL`'s allocatable count, not dependency injection).
- Tiers 2 and 3 land as their own subsequent PRs after Tier 1 validates the pattern on this codebase.
