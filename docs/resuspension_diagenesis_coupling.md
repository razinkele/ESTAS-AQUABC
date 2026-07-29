# Resuspension × Sediment-Diagenesis Compatibility in ESTAS-AQUABC

*Findings note — 2026-07-25. Grounded in the source and `docs/ESTAS_Reference_Manual.md`.*

## Verdict

**No — the resuspension routine is *not* coupled to the full sediment-diagenesis model, and
the code enforces this with a hard runtime guard.** Enabling both together **halts the program**
at startup. Resuspension *is* compatible with the other sediment modes (0 = none, 1 = prescribed
fluxes).

| `MODEL_SEDIMENTS` | Meaning | With resuspension (`RESUSPENSION_OPTION > 0`)? |
|---|---|---|
| 0 | No sediment model (deposited material removed from the water column) | ✅ compatible |
| 1 | Prescribed sediment fluxes (e.g. the `CL29_BENTHIC_DENIT` NO3 sink) | ✅ compatible (not guarded) |
| 2 | Full sediment diagenesis (explicit bed compartment) | ⛔ **program halts** |

## The guard

```fortran
! SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90:554
if (MODEL_BOTTOM_SEDIMENTS > 1) then          ! full diagenesis = MODEL_SEDIMENTS 2
    if (resusp%CONSIDER_RESUSPENSION > 0) then
        write(*,*) 'Bottom sediments are not coupled with resuspension ' // &
                   'in this version of ESTAS-AQUABC. Program halted.'
        stop
    end if
    ...
```

The condition is `> 1`, so **only full diagenesis (Mode 2) is guarded**. `MODEL_SEDIMENTS == 1`
(prescribed fluxes) is handled separately (`mod_SOLVER.f90:727 / 1350 / 1429`) and is never guarded.

## Why they're incompatible — a bed mass-conservation gap

From the deposition model (`ESTAS_Reference_Manual.md`, §Deposition to Sediments):

> the deposited flux "is what **enters the sediment model (Mode 2)** or is simply **removed from
> the water column (Modes 0 and 1)**."

- In **Mode 2**, the bed becomes an *explicit, mass-conserving compartment* (`bsed` state,
  advanced each step at `mod_SIMULATE.f90:342-344`) that accumulates the deposited particulate
  flux and returns dissolved mineralization fluxes to the water.
- **Resuspension** (`mod_RESUSPENSION.f90`) computes an erosion flux from bed shear stress —
  `E = E₀ (τ_b/τ_c − 1)` when `τ_b > τ_c` — and injects material back into the water column, but
  it does **not** decrement the diagenesis bed state. The two-way particulate exchange at the
  sediment–water interface (resuspend *from* the diagenesis surface layer, updating its inventory)
  was never wired up.
- Running both would let the bed keep accumulating deposited mass while resuspension independently
  adds "eroded" mass to the water from an unlinked pool → **the sediment mass balance breaks.**
  Hence the guard.

In Modes 0 and 1 the deposited material is simply removed from the water column (no conserved bed
pool to contradict), so resuspension can safely re-inject material.

## Relevance to the `CL29_BENTHIC_DENIT` NO3 sink (v0.8.0)

The benthic-denitrification sink uses **Mode 1** (prescribed flux), which is **not** guarded — it
can run alongside resuspension. It is also a prescribed *dissolved-N* sink, not a particulate bed
inventory, so there is no conservation conflict with resuspension in either direction.

## Practical implications & status

- CL29 with full diagenesis must run `RESUSPENSION_OPTION = 0` (which the sediment-diagenesis setup
  does).
- This is a **known, documented limitation** ("in this version"), not a bug.
- The Phase-5.1 refactors (`resuspension_t`, `sediment_state_t`, `wsc_state_t`) were byte-identical
  state de-globalization — they did **not** change this coupling, so the limitation is unchanged.

## The fix (future work)

A modest feature would remove the limitation: have the resuspension erosion flux draw from (and
decrement) the `bsed` surface-layer inventory, so the bed stays mass-conserving under two-way
exchange. Once resuspension and diagenesis share a single conserved bed pool, the guard at
`mod_AQUATIC_MODEL.f90:554` can be lifted.

## Key references

- `SOURCE_CODE/ESTAS/mod_AQUATIC_MODEL.f90:554-560` — the guard.
- `SOURCE_CODE/ESTAS/mod_SOLVER.f90:727,1350,1429` (Mode 1 prescribed flux), `:1510` (Mode 2).
- `SOURCE_CODE/ESTAS/mod_SIMULATE.f90:342-344` — diagenesis bed-state advance.
- `SOURCE_CODE/ESTAS/mod_RESUSPENSION.f90` — the resuspension (`resuspension_t`) module.
- `docs/ESTAS_Reference_Manual.md` §§ Deposition to Sediments, Resuspension.
