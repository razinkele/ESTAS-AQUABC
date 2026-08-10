# CL29 seasonal-phase diagnosis

**Date:** 2026-08-10
**Run:** defaults (`INPUTS_CL29`, `WCONST_04.txt` as committed), 5 years from 2012-01-01,
`ESTAS_HOLD_VOLUME=1`, `PRINT_INTERVAL=240`, boxes 7/14/17/23.
**Tools:** `tools/seasonal_phase.py`, `tools/validate_cl29_vs_epa.py --phase`,
`tools/diagnose_group_limitation.py`.

This records measurements only. No model parameter or source file was changed to produce
it, and none is recommended here without the checks listed under "Follow-up".

---

## 1. The measured baseline

Chlorophyll-*a*, model against the EPA observed climatology over the shared months:

```
peak month        model  2   obs  8   offset +6 months
autumn/spring     model 0.63   obs 1.90
seasonal r        -0.60   (n = 10 months)
```

The model's seasonal cycle is inverted. It peaks in February where the observations peak in
August, and autumn sits *below* spring where observations put it nearly twice as high. The
annual mean is close enough (a bias near −2 µg/L) that per-season bias reporting never
surfaced this — which is why the phase metrics were added.

The inversion has two independent halves, and they are driven by different things.

### Model water temperature

Every statement below is relative to the temperature the model actually simulated, taken
from the run's own output (`PROCESS_RATES` slot 13 of `CYN_C_INDEX`), monthly mean over
boxes:

| month | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| °C | 1.0 | 1.1 | 3.1 | 8.0 | 14.4 | 19.1 | **21.0** | 20.8 | 16.3 | 10.8 | 6.2 | 2.1 |

**The modelled lagoon never exceeds 21 °C.** Whether that is correct is unverified — the EPA
observation set carries no temperature variable, so it could not be checked here. It is the
first item under "Follow-up".

---

## 2. Half one — the winter diatom excess

Limitation factors, winter (Jan–May) against bloom season (Jul–Sep). 1.0 means *not* limiting:

| factor | winter | summer | verdict |
|---|---|---|---|
| TEMP | 0.603 | 0.316 | weakly limiting |
| DOXY | 0.956 | 0.938 | not limiting |
| N | 0.979 | 0.585 | not limiting |
| P | 0.413 | 0.932 | weakly limiting |
| Si | 0.985 | 0.992 | not limiting |
| LIGHT | 0.391 | 0.433 | weakly limiting |

Rate terms, winter mean: **growth 0.19499 against total losses 0.14765 — growth-dominated.**
Losses split: death 70 %, respiration 28 %, excretion 3 %, **grazing 0 %**.

Nothing strongly limits diatoms in winter. Nitrogen, silica and oxygen are all effectively
unlimited; temperature, phosphorus and light each hold growth back only partially. Growth
exceeds losses, so the winter bloom is built by unconstrained growth rather than sustained by
weak losses.

The temperature factor is the reason it is winter specifically. The diatom cardinal
temperatures are

```
DIA_OPT_TEMP_LR         = -2.0   T_min
DIA_OPT_TEMP_UR         = 10.0   T_opt
KAPPA_DIA_OVER_OPT_TEMP = 21.0   T_max
```

so the group is parameterized as a **cold-water species with its optimum at 10 °C and complete
shutdown at 21 °C** — which is exactly the lagoon's August temperature. That produces a
temperature factor of 0.36 in January and 0.16 in August: diatoms are favoured in winter and
switched off in summer, by construction.

**The CTMI is valid**, so the plateau-fallback trap recorded in
`fix-cyn-n2fixation-overprediction` does not apply here: 2·T_opt = 20 > T_min + T_max = 19.
The margin is only 1 °C, so any future change to these three numbers must re-check the
inequality. The formula was also verified against the run: CTMI(1.04 °C) = 0.363 against a
measured 0.355.

---

## 3. Half two — the summer diazotroph deficit

### FIX_CYN, bloom season

| factor | bloom | winter | verdict |
|---|---|---|---|
| TEMP | 0.287 | 0.005 | **strongly limiting** |
| DOXY | 0.938 | 0.956 | not limiting |
| FIX_N | 0.379 | 0.016 | fixation partly enabled (see below) |
| FIX_P | 0.892 | 0.332 | not limiting |
| NONFIX_N | 0.585 | 0.979 | weakly limiting |
| LIGHT | 0.457 | 0.310 | weakly limiting |

**growth 0.00584 against losses 0.00762 — loss-dominated** (losses are 1.30× growth).
Losses: death 54 %, respiration 41 %, excretion 5 %, **grazing 0 %**.

### NOST, bloom season

| factor | bloom | winter | verdict |
|---|---|---|---|
| LIGHT | 0.413 | 0.302 | weakly limiting |
| TEMP | 0.398 | 0.013 | weakly limiting |
| DOXY | 0.938 | 0.956 | not limiting |
| P | 0.892 | 0.332 | not limiting |
| N | 0.638 | 0.984 | weakly limiting |

**growth 0.00257 against losses 0.00517 — loss-dominated** (losses are 2.01× growth).
Losses: respiration 59 %, death 38 %, **grazing 0 %**.

### The nitrogen window opens correctly

`LIM_KG_FIX_FIX_CYN_N` is an **inverse** Monod — `K_FIX / (K_FIX + NH4 + NO3 + avail·DON)`
(`aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90:203`). It is a fixation *switch*, not a
shortage: 1.0 means DIN is scarce and fixation is fully enabled, 0.0 means ambient DIN is
abundant and fixation is suppressed. It must be read opposite to every other factor in these
tables.

Measured, it behaves as designed. Ambient DIN falls from 1.42 mg/L in March to **0.0195 mg/L
in August**, the N:P ratio drops from 4639 to **1.3**, and the switch opens from 0.006 to
**0.509**. The model does produce a summer nitrogen window, at the right time, with the right
stoichiometric signature.

**So the diazotroph deficit is not a failure of the nitrogen window.** The fixers cannot
exploit the window that opens for them, for two measured reasons:

1. **Their temperature optimum is unreachable.** Both diazotroph groups are parameterized
   with `T_opt = 26 °C`:

   | | T_min | T_opt | T_max | factor at 21 °C | factor at T_opt |
   |---|---|---|---|---|---|
   | FIX_CYN | 18.0 | 26.0 | 32.0 | 0.44 | 1.00 |
   | NOST | 16.0 | 26.0 | 33.0 | — | 1.00 |

   The lagoon peaks at 21 °C, so FIX_CYN operates permanently at **at most 0.44** of its
   potential growth and never approaches its optimum in any month of any year. FIX_CYN's T_min
   of 18 °C also means it is switched fully off for eight months. Both CTMIs are valid
   (52 > 50 and 52 > 49 respectively), and the arithmetic was verified against the run:
   CTMI(20.97 °C) = 0.436 against a measured 0.427.

2. **Losses exceed growth throughout the bloom season** — by 1.30× for FIX_CYN and 2.01× for
   NOST. NOST in particular respires away more than it fixes: respiration alone (0.00306) is
   larger than total growth (0.00257). It germinates from akinetes on schedule, grows for
   roughly six weeks, and is consumed by its own maintenance cost.

---

## 4. The cross-cutting finding: grazing is identically zero

**Grazing is 0.00000 for every group in every month** — 0 % of losses for diatoms in winter,
for FIX_CYN in summer, and for NOST in summer. There is no top-down control anywhere in the
simulated year.

This is consistent with the independently measured zooplankton deficit (modelled biomass
~5.5× below observed, recorded in `cl29-calibration-wall`), and it connects the two halves of
the phase error to one structural cause. It affects them asymmetrically, which is worth being
precise about:

- **Winter:** zero grazing is a plausible *contributor* to the diatom excess. Diatoms are
  growth-dominated with 70 % of their losses in an internal density-independent death term,
  and nothing crops them.
- **Summer:** zero grazing does **not** explain the diazotroph deficit. Absent grazing helps
  the fixers; restoring it would make them smaller, not larger. The summer deficit is
  explained by the temperature ceiling and the loss/growth imbalance above.

---

## 5. Applying the plan's decision table

| Row | Fires? | Consequence |
|---|---|---|
| DIA winter: factors high, growth-dominated | **yes** | Target the diatom temperature/light response. CTMI validity verified (20 > 19) — the trap does not apply, so the cold-adapted parameters themselves are the candidate. |
| DIA winter: factors low, loss-dominated | no | Winter is growth-dominated. |
| Fixers: a limitation factor is small | **yes** | TEMP = 0.287 for FIX_CYN. The pathway is temperature, not light and not phosphorus. |
| Fixers: all factors high, loss-dominated | **partly** | Loss-dominated is confirmed (1.30× and 2.01×), but not all factors are high, so this row is secondary to the temperature row. Its note still holds: halved mortality was already shown insufficient, and grazing is zero so it cannot be reduced further. |
| Fixers: growth-dominated yet biomass low | no | Both fixer groups are loss-dominated, so the germination pathway is **not** implicated. This answers the open question left in `fixer-deficit-is-amplitude-not-extinction`: the akinete carbon that leaves `AKI_C` is respired and dies, it is not lost in transfer. |

**Selected follow-up target: the temperature response of all three groups**, which is the one
lever that both halves of the phase error share. Diatoms are tuned to peak at 10 °C and shut
off at 21 °C; diazotrophs are tuned to peak at 26 °C, which this lagoon never reaches. Those
two facts together produce a cycle that peaks in February instead of August.

---

## 6. What is ruled out — do not revisit

**The seed-floor / winter-refuge hypothesis is dead.** `FIX_CYN_C` never approaches the
`MIN_CONCENTRATION` clamp: its measured minimum is 5.0×10⁻⁴ against a clamp of 1×10⁻¹⁰, about
5,000,000× above it. `NOST_VEG_HET_C` germinates from akinetes on schedule every year, and
the combined fixer peak lands in August, which is the correct month. The deficit is
**amplitude, not extinction and not timing**, and a concentration floor is a bound rather than
a rate — it could not have produced a bloom.

**The germination/transfer pathway is ruled out** by this diagnosis, per the decision table
row above.

**Reducing fixer mortality is ruled out** as a sufficient fix: a 9-parameter differential
evolution that both halved fixer mortality and raised fixer growth still left them ~70× low.

---

## 7. Follow-up, in order

1. **Verify the temperature forcing.** The modelled lagoon peaks at 21.0 °C. The EPA set has
   no temperature variable, so this needs an external source. If the real lagoon reaches
   24–25 °C, part of the diazotroph deficit is a forcing error rather than a parameter error,
   and that changes what should be adjusted. Do this before touching any cardinal temperature.
2. **Check the cardinal temperatures against the literature for the species actually present.**
   T_opt = 26 °C for both diazotroph groups is the specific number to justify or revise, and
   T_opt = 10 °C for diatoms likewise. This has *not* been verified here and must not be
   changed on the strength of this document alone.
3. **Re-check CTMI validity on any change.** Diatoms have only 1 °C of margin
   (2·T_opt = 20 against T_min + T_max = 19); breaking the inequality silently substitutes a
   plateau and manufactures false persistence.
4. **Treat zooplankton grazing as a separate, larger item.** Identically zero grazing on every
   group is a structural gap, not a phenology parameter, and it interacts with the winter half
   only.

---

## Method note

The per-group limitation tables above were produced only after correcting a labelling error
worth recording. **Each group writes a different `PROCESS_RATES` slot layout**: DIA puts TEMP
at slot 6, FIX_CYN at slot 9, NOST at slot 12. An earlier pass assumed one shared layout and
reported FIX_CYN's temperature limitation as 0.003 when the true value is 0.287 — the 0.003
was a growth *rate* read from the wrong column.

A range check cannot catch this, because every limitation factor legitimately lies in [0, 1],
so one can be silently read for another. What does catch it is an identity the data must
satisfy: FIX_CYN's slot 1 equals slot 6 plus slot 7, and that holds only at the correct
offset. `tools/diagnose_group_limitation.py` now asserts it on every run. This is the same
family of error as `aquabc-parallel-code-paths` — a name matching is not the same as the
thing matching.
