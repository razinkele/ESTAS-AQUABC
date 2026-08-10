# CL29 seasonal-phase diagnosis

**Date:** 2026-08-10
**Run:** defaults (`INPUTS_CL29`, `WCONST_04.txt` as committed), 11 years from 2012-01-01,
`ESTAS_HOLD_VOLUME=1`, `PRINT_INTERVAL=240` (daily output, verified dt = 1.0 d).
**Boxes:** 7, 14, 17 and 23 — **four of the twenty-nine**. Limitation tables below are means
over those four boxes; the budget in §3.3 is box 23 alone. They characterize this sample, not
every box.
**Tools:** `tools/seasonal_phase.py`, `tools/validate_cl29_vs_epa.py --phase`,
`tools/diagnose_group_limitation.py`.

This records measurements only. No model parameter or source file was changed to produce it,
and none is recommended here without the checks listed in §7.

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

The inversion has two independent halves, driven by different things.

### Model water temperature

Every statement below is relative to the temperature the model actually simulated, read from
the run's own output (`PROCESS_RATES` slot 13 of `CYN_C_INDEX`), monthly mean:

| month | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| °C | 1.0 | 1.1 | 3.1 | 8.0 | 14.4 | 19.1 | **21.0** | 20.8 | 16.3 | 10.8 | 6.2 | 2.1 |

**The modelled lagoon never exceeds 21 °C.** Whether that is correct is unverified — the EPA
observation set carries no temperature variable, so it could not be checked here. It is the
first item in §7.

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
Losses split: death 70 %, respiration 28 %, excretion 3 %, grazing 0 %.

Nothing strongly limits diatoms in winter. Nitrogen, silica and oxygen are effectively
unlimited; temperature, phosphorus and light each hold growth back only partially. Growth
exceeds losses, so the winter bloom is built by unconstrained growth rather than sustained by
weak losses.

Temperature is why it is winter specifically. The diatom cardinal temperatures are

```
DIA_OPT_TEMP_LR         = -2.0   T_min
DIA_OPT_TEMP_UR         = 10.0   T_opt
KAPPA_DIA_OVER_OPT_TEMP = 21.0   T_max
```

so the group is parameterized as a **cold-water species with its optimum at 10 °C and complete
shutdown at 21 °C** — which is exactly the lagoon's August temperature. That yields a
temperature factor of 0.36 in January against 0.16 in August: diatoms are favoured in winter
and switched off in summer, by construction.

**The CTMI is valid**, so the plateau-fallback trap recorded in
`fix-cyn-n2fixation-overprediction` does not apply here: 2·T_opt = 20 > T_min + T_max = 19.
The margin is only 1 °C, so any future change to these three numbers must re-check the
inequality. The formula was verified against the run: CTMI(1.04 °C) = 0.363 against a measured
0.355.

---

## 3. Half two — the diazotroph deficit

### 3.1 Summer: no single dominant constraint

Reported over **July–August**, the months the bloom actually occupies. The Jul–Sep window used
elsewhere in this document is misleading for the fixers: September's temperature factor is
0.026, which drags the three-month mean below the "strongly limiting" band and manufactures a
verdict the bloom months do not support.

| factor | Jul–Aug | Jul–Sep | verdict (Jul–Aug) |
|---|---|---|---|
| TEMP | 0.414 | 0.287 | weakly limiting |
| DOXY | 0.938 | 0.938 | not limiting |
| FIX_N | 0.461 | 0.379 | fixation partly enabled (see §3.2) |
| FIX_P | 0.880 | 0.892 | not limiting |
| NONFIX_N | 0.499 | 0.585 | weakly limiting |
| LIGHT | 0.475 | 0.457 | weakly limiting |

**In the bloom months temperature (0.414) and light (0.475) are comparable**, and neither is
strongly limiting. There is no single dominant summer constraint: growth is held at roughly
40–50 % on several axes at once. Temperature does have the lower value, and unlike light it is
governed by a parameter that is questionable on its own terms (§3.4) — but the summer data do
not by themselves single out the temperature pathway.

NOST over the same season: LIGHT 0.413, TEMP 0.398, N 0.638, P 0.892, DOXY 0.938 — the same
picture of several partial constraints.

### 3.2 The nitrogen window opens correctly

`LIM_KG_FIX_FIX_CYN_N` is an **inverse** Monod — `K_FIX / (K_FIX + NH4 + NO3 + avail·DON)`
(`aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90:203`). It is a fixation *switch*, not a
shortage: 1.0 means DIN is scarce and fixation is fully enabled, 0.0 means ambient DIN is
abundant and fixation is suppressed. It reads opposite to every other factor in these tables.

It behaves as designed. Ambient DIN falls from 1.42 mg/L in March to **0.0195 mg/L in
August**, the N:P ratio drops from 4639 to **1.3**, and the switch opens from 0.006 to
**0.509**. The model produces a genuine summer nitrogen window, at the right time, with the
right stoichiometric signature. **The deficit is not a failure of the nitrogen window.**

### 3.3 Local kinetics are strongly negative, yet the population persists

For box 23 across August (days 944–974), the FIX_CYN kinetic terms are, per day:

```
growth 0.07889   respiration 0.04943   excretion 0.00506   death 0.06785
net kinetic = -0.04344 /d      integrated over 31 d = -1.347 mg C/L
```

Specific rates against the standing stock: growth 0.123/d, losses 0.191/d — locally the
population loses about 7 % of itself per day. Yet `FIX_CYN_C` falls only from 0.234 to 0.197
over that month, a change of −0.038. **The kinetic balance is short by about +1.31 mg C/L per
month, some 35× the observed change.**

`DERIVATIVES` for this state variable is exactly slot 1 − 2 − 3 − 4 − 5, so no other kinetic
term exists. Something outside the kinetics supplies that carbon; **inter-box transport and
open-boundary inflow are the plausible candidates**, and this diagnostic cannot see either.
The `MIN_CONCENTRATION` clamp is *not* the source despite being a known non-conservative one:
`FIX_CYN_C`'s minimum is 5.0×10⁻⁴ against a clamp of 1×10⁻¹⁰.

Two consequences:

- Every "loss-dominated" statement in this document is about the **local kinetic balance**,
  not about a population budget. NOST is likewise locally loss-dominated (growth 0.00257
  against losses 0.00517).
- **Local rate parameters have less leverage over standing stock than the kinetic tables
  suggest**, because the standing stock is substantially set by import. This converges with
  the independently established conclusion in `cl29-epa-validation` that the effective lever
  in this configuration is open-boundary forcing and the removal balance.

### 3.4 Autumn: a single parameter closes the season

This is the sharpest result in the run, and it is the one that bears on the metric that is
most wrong — the autumn/spring ratio, 0.63 modelled against 1.90 observed.

Box 23, monthly means:

| month | water °C | TEMP factor | fixation switch (FIX_N) | FIX_P |
|---|---|---|---|---|
| Jul | 21.3 | 0.459 | 0.439 | 0.877 |
| Aug | 21.1 | 0.449 | 0.529 | 0.884 |
| Sep | 16.5 | 0.034 | 0.237 | 0.917 |
| **Oct** | **10.7** | **0.000** | **0.564** | 0.914 |
| **Nov** | **6.1** | **0.000** | **0.640** | 0.891 |

**In October and November the nitrogen window is at its annual widest — 0.564 and 0.640,
wider than July's 0.439 — and phosphorus is not limiting, but the temperature factor is
exactly zero.**

The cause is a single parameter. `FIX_CYN_OPT_TEMP_LR` = **18.0 °C** is the CTMI minimum, and
autumn water is 10.7 °C and 6.1 °C. Below T_min the CTMI is identically zero, so growth is
zero regardless of every other gate being open. Light is also declining in autumn (0.282 over
Sep–Nov against 0.475 in Jul–Aug) and would be a real secondary constraint, but it never gets
to act: the temperature gate is an exact zero, and nothing multiplies back up from that.

So the model cannot produce autumn diazotrophs **at all**, at any nutrient or light condition,
while `T_min = 18 °C` stands. That is a hard structural statement, not a matter of degree, and
it directly explains the autumn half of the phase error.

### 3.5 Both diazotroph optima are unreachable

| | T_min | T_opt | T_max | factor at 21 °C | factor at T_opt |
|---|---|---|---|---|---|
| FIX_CYN | 18.0 | 26.0 | 32.0 | 0.44 | 1.00 |
| NOST | 16.0 | 26.0 | 33.0 | — | 1.00 |

Both groups carry `T_opt = 26 °C` while the lagoon peaks at 21 °C, so neither ever approaches
its optimum in any month of any year; FIX_CYN is capped at about 0.44 of potential growth.
Both CTMIs are valid (52 > 50 and 52 > 49). The arithmetic was verified against the run:
CTMI(20.97 °C) = 0.436 against a measured 0.427.

---

## 4. Grazing is negligible on every group

Maximum grazing rate recorded anywhere in the run, across all boxes and all 4017 days:

| group | max grazing (mg C/L/d) |
|---|---|
| DIA | 1.6×10⁻⁴ |
| FIX_CYN | 1.7×10⁻⁵ |
| NOST | 0.0 (exactly) |

Against growth rates of order 10⁻¹, grazing is **negligible rather than absent** — the
pathway runs, it simply carries almost nothing. It rounds to 0 % of losses for every group in
every season. This is consistent with the independently measured zooplankton deficit
(modelled biomass ~5.5× below observed, recorded in `cl29-calibration-wall`) and points at
low zooplankton biomass rather than a broken coupling.

It bears on the two halves asymmetrically, which is worth being precise about:

- **Winter:** negligible grazing is a plausible contributor to the diatom excess. Diatoms are
  growth-dominated with 70 % of losses in an internal density-independent death term, and
  nothing crops them.
- **Autumn and summer:** it does **not** explain the diazotroph deficit. Absent grazing helps
  the fixers; restoring it would make them smaller. The autumn deficit is the temperature
  threshold (§3.4); the summer deficit is several partial constraints plus a transport-set
  standing stock (§3.1, §3.3).

---

## 5. Applying the plan's decision table

| Row | Fires? | Consequence |
|---|---|---|
| DIA winter: factors high, growth-dominated | **yes** | Target the diatom temperature/light response. CTMI validity verified (20 > 19) — the trap does not apply, so the cold-adapted parameters themselves are the candidate. |
| DIA winter: factors low, loss-dominated | no | Winter is growth-dominated. |
| Fixers: a limitation factor is small | **yes, in autumn** | Not in summer, where TEMP (0.414) and LIGHT (0.475) are comparable and neither is strongly limiting. In autumn TEMP is an exact 0.000 from the `T_min = 18 °C` threshold while the nitrogen window is at its widest. The pathway is temperature, and specifically T_min. |
| Fixers: all factors high, loss-dominated | **partly** | Locally loss-dominated is confirmed (§3.3), but §3.3 also shows the standing stock is set largely by transport, so this row's premise — that local loss terms control biomass — is weaker than it appears. Its note still holds: halved mortality was already shown insufficient, and grazing is already negligible so it cannot be reduced further. |
| Fixers: growth-dominated yet biomass low | no | Both fixer groups are locally loss-dominated, so the germination pathway is **not** implicated. This answers the open question left in `fixer-deficit-is-amplitude-not-extinction`: the akinete carbon leaving `AKI_C` is respired and dies rather than being lost in transfer. |

**Selected follow-up target: the cardinal temperatures**, which is the one lever both halves of
the phase error share. Diatoms peak at 10 °C and shut off at 21 °C; diazotrophs cannot grow
below 18 °C and peak at 26 °C, which this lagoon never reaches. Those settings together
produce a cycle that peaks in February instead of August and that cannot populate autumn at
all.

---

## 6. What is ruled out — do not revisit

**The seed-floor / winter-refuge hypothesis is dead.** `FIX_CYN_C` never approaches the
`MIN_CONCENTRATION` clamp: measured minimum 5.0×10⁻⁴ against a clamp of 1×10⁻¹⁰, about
5,000,000× above it. `NOST_VEG_HET_C` germinates from akinetes on schedule every year, and the
combined fixer peak lands in August, the correct month. The deficit is **amplitude, not
extinction and not timing**, and a concentration floor is a bound rather than a rate.

**The germination/transfer pathway is ruled out** by the decision-table row above.

**Reducing fixer mortality is ruled out** as a sufficient fix: a 9-parameter differential
evolution that both halved fixer mortality and raised fixer growth still left them ~70× low.
§3.3 explains why that was always unlikely to work — local rates are not what sets the
standing stock.

---

## 7. Follow-up, in order

1. **Verify the temperature forcing.** The modelled lagoon peaks at 21.0 °C. The EPA set
   carries no temperature variable, so this needs an external source. If the real lagoon
   reaches 24–25 °C, part of the summer deficit is a forcing error rather than a parameter
   error. Do this before touching any cardinal temperature. Note it does **not** rescue
   autumn: no plausible forcing puts October above 18 °C.
2. **Justify or revise `FIX_CYN_OPT_TEMP_LR = 18.0 °C`.** This is the highest-value single
   number in the diagnosis: it alone closes the autumn season while the nitrogen window is at
   its widest. Check it against the literature for the species actually present in this
   lagoon. It has **not** been verified here and must not be changed on the strength of this
   document alone.
3. **Then examine `T_opt = 26 °C` for both diazotroph groups, and `T_opt = 10 °C` for
   diatoms**, on the same evidentiary standard.
4. **Re-check CTMI validity on any change.** Diatoms have only 1 °C of margin
   (2·T_opt = 20 against T_min + T_max = 19); breaking the inequality silently substitutes a
   plateau and manufactures false persistence.
5. **Quantify the transport term before tuning any local rate.** §3.3 shows imported carbon
   exceeding the local kinetic balance by ~35× for box 23 in August. Until that is decomposed,
   the leverage of any growth or mortality parameter on standing stock is unknown.
6. **Treat zooplankton grazing as a separate, larger item.** Negligible grazing on every group
   is a structural gap, not a phenology parameter, and it interacts with the winter half only.

---

## Method note

The per-group limitation tables were produced only after correcting a labelling error worth
recording. **Each group writes a different `PROCESS_RATES` slot layout**: DIA puts TEMP at slot
6, FIX_CYN at slot 9, NOST at slot 12. An earlier pass assumed one shared layout and reported
FIX_CYN's temperature limitation as 0.003 when the true value is 0.414 over the bloom months —
the 0.003 was a growth *rate* read from the wrong column.

A range check cannot catch this, because every limitation factor legitimately lies in [0, 1],
so one can be silently read for another. What does catch it is an identity the data must
satisfy: FIX_CYN's slot 1 equals slot 6 plus slot 7, and that holds only at the correct
offset. `tools/diagnose_group_limitation.py` asserts it on every run. This is the same family
of error as `aquabc-parallel-code-paths` — a name matching is not the same as the thing
matching.

A second lesson sits in §3.1: a three-month seasonal window averaged across a sharp threshold
inverts the verdict. September's temperature factor of 0.026 pulled the Jul–Sep mean into the
"strongly limiting" band and made temperature look like summer's dominant constraint when in
the bloom months it is merely one of several partial ones. Season windows must be checked
against the monthly series before their means are interpreted.
