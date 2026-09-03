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
the run's own output (`PROCESS_RATES` slot 13 of `CYN_C_INDEX`).

| month | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| monthly mean °C | 1.0 | 1.1 | 3.1 | 8.0 | 14.4 | 19.1 | 21.0 | 20.8 | 16.3 | 10.8 | 6.2 | 2.1 |
| **daily max °C** | | | | | | | **28.3** | **27.8** | **23.0** | **16.2** | **11.6** | **8.2** |

**The monthly means understate the peaks badly and must not be used to judge threshold
behaviour.** Daily values reach 24–28 °C, with a mean annual maximum of **25.5 °C** over the
eleven years (2012–2022 annual maxima: 24.4, 25.0, 26.5, 24.7, 25.3, 22.1, 27.8, 25.2, 25.4,
28.3, 25.6). July–August daily percentiles are p50 20.7, p75 22.4, p90 24.2, p95 25.2, p99 27.0.

**The forcing is therefore plausible on its face** — a shallow eutrophic lagoon reaching the
mid-20s in summer — and the "the model is too cold" hypothesis is not supported by the run. It
is *not* independently validated (the EPA set carries no temperature variable), but it no
longer sits on the critical path: the autumn result in §3.4 turns on daily maxima that are far
below any threshold in question.

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
- ~~**Local rate parameters have less leverage over standing stock than the kinetic tables
  suggest**~~ — **this inference was wrong, and the experiment in §9 refuted it**: changing one
  local growth parameter moved August biomass 13×. The budget *fact* stands (imports exceed the
  local balance ~35×), but the inference from it did not follow, because the imported
  concentration is not an external boundary condition — it is the neighbouring boxes'
  concentration, which responds to the same parameter everywhere at once. Transport is an
  *amplifier* of the whole field, not an independent supply that caps local leverage.

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

The cause is a single parameter: `FIX_CYN_OPT_TEMP_LR` = **18.0 °C**, the CTMI minimum. Below
T_min the CTMI is identically zero, so growth is zero regardless of every other gate.

**This is not an averaging artifact — it holds on daily values.** Across all four boxes and all
eleven years:

| month | daily max °C | days above 18 °C | % of days with TEMP factor = 0 |
|---|---|---|---|
| Jul | 28.3 | 1239 (90.8 %) | 8.3 % |
| Aug | 27.8 | 1250 (91.6 %) | 8.1 % |
| Sep | 23.0 | 282 (21.4 %) | 77.3 % |
| **Oct** | **16.2** | **0 (0.0 %)** | **100 %** |
| **Nov** | **11.6** | **0 (0.0 %)** | **100 %** |
| **Dec** | **8.2** | **0 (0.0 %)** | **100 %** |

**Not one day in October, November or December in eleven years reaches 18 °C** — October's
daily *maximum* is 16.2 °C, nearly two degrees short. The shutdown is absolute and it is not a
matter of degree. September is the transition: three-quarters of its days are already
hard-zeroed.

Light is also declining in autumn (0.282 over Sep–Nov against 0.475 in Jul–Aug) and would be a
real secondary constraint, but it never gets to act — the temperature gate is an exact zero
and nothing multiplies back up from that.

So the model cannot produce autumn diazotrophs **at all**, at any nutrient or light condition,
while `T_min = 18 °C` stands. That directly explains the autumn half of the phase error, and it
matters because the observations do *not* fall off in October: in-situ chlorophyll is 52.4,
47.9 and 48.3 µg/L for Aug/Sep/Oct, essentially flat, while the model drops to 23.6.

### 3.5 The optima ARE reached — the summer constraint is intermittency, not a ceiling

| | T_min | T_opt | T_max | measured factor: max | median (Jul–Aug) | % of days at 0 |
|---|---|---|---|---|---|---|
| FIX_CYN | 18.0 | 26.0 | 32.0 | **1.000** | 0.375 | 8.2 % |
| NOST | 16.0 | 26.0 | 33.0 | — | — | — |

Both CTMIs are valid (52 > 50 and 52 > 49) and the arithmetic was verified against the run:
CTMI(20.97 °C) = 0.436 against a measured 0.427.

**The FIX_CYN temperature factor reaches a full 1.000 in the run**, so `T_opt = 26 °C` is
attained on the warmest days and the group is *not* capped below its potential. The July–August
distribution is strongly skewed — p25 0.108, p50 0.375, p75 0.691, p90 0.914 — and 91 % of
July–August days are above `T_min`. The seasonal mean of 0.414 is the mean of that spread, not
a ceiling.

So the summer constraint is **intermittency**: the lagoon straddles the 18 °C threshold, and
fixer growth is switched fully on and fully off within the season rather than held at a steady
fraction. This is a materially different statement from "temperature limits the summer bloom",
and it is the reason §3.1 declines to name temperature as summer's dominant constraint.

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
the phase error share — but specifically the *thresholds*, not the optima. Diatoms peak at
10 °C and shut off at 21 °C, so they are a winter group by construction. Diazotrophs cannot
grow below 18 °C, which the lagoon clears routinely in July–August but never once in October.
Their optimum of 26 °C is reached and is not the problem. Those settings together produce a
cycle that peaks in February instead of August and that cannot populate autumn at all.

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

1. ~~**Justify or revise `FIX_CYN_OPT_TEMP_LR = 18.0 °C`**~~ — **done, see §8.** The lagoon's
   diazotrophs are *Aphanizomenon* and *Anabaena*, not *Nodularia*, and cultured
   *Aphanizomenon flos-aquae* grows above **8 °C** with an optimum of 23–29 °C. `T_opt = 26 °C`
   is therefore well supported; **`T_min = 18 °C` is about 10 °C too high** and is the single
   wrong number. Next step is a run with a corrected `T_min`, not further desk work.
2. **Do not pursue "the forcing is too cold".** Daily temperatures reach 24–28 °C with a mean
   annual maximum of 25.5 °C (§1), so the forcing is plausible and `T_opt = 26 °C` is actually
   attained. An independent check against an external temperature source is still worth having
   — the EPA set carries none — but it is no longer on the critical path, and it cannot affect
   autumn in any case.
3. **Then examine `T_opt = 10 °C` for diatoms**, on the same evidentiary standard. `T_opt = 26 °C`
   for the fixers is a lower priority now that it is known to be reached.
4. **Re-check CTMI validity on any change.** Diatoms have only 1 °C of margin
   (2·T_opt = 20 against T_min + T_max = 19); breaking the inequality silently substitutes a
   plateau and manufactures false persistence.
5. **Quantify the transport term before tuning any local rate.** §3.3 shows imported carbon
   exceeding the local kinetic balance by ~35× for box 23 in August. Until that is decomposed,
   the leverage of any growth or mortality parameter on standing stock is unknown.
6. **Treat zooplankton grazing as a separate, larger item.** Negligible grazing on every group
   is a structural gap, not a phenology parameter, and it interacts with the winter half only.

---

## 8. Literature check: `T_min = 18 °C` is not defensible for this lagoon

Follow-up item 1, carried out 2026-08-10 via the scite literature index. Both papers below were
checked for editorial notices; **neither carries a retraction, correction or expression of
concern.**

### Which species are actually here

The Curonian monitoring record (`~/curonian/DATA/JTD/monitoringasjsonl`, 162,254 records)
gives genus mention counts of *Anabaena* 1591, *Aphanizomenon* 1111, *Dolichospermum* 75 and
**Nodularia 123**. The lagoon's diazotroph assemblage is *Aphanizomenon* and
*Anabaena*/*Dolichospermum*. *Nodularia spumigena* — the warm-water open-Baltic bloomer whose
thermal preferences a `T_min` of 18 °C would suit — is marginal here.

### What the measured cardinal temperatures are

Tsujimura et al. (2001) cultured an axenic *Aphanizomenon flos-aquae* strain and report:

> "The strain could grow at **above 8°C** with an **optimum temperature ranging from 23 to
> 29°C**, and survived even at 5°C for at least 25 days under low light conditions. Although
> these results confirmed the ability of the bloom formation during **late autumn and winter**,
> it is still unclear why the *Aphanizomenon* bloom occurred at temperatures of **ca 10°C in
> December**…"

Yamamoto (2009) corroborates the cold end from field populations: *A. flos-aquae* "could also
tolerate low temperatures in the winter, and was present in relatively high densities", with
"large biomasses of the low-temperature-adapted *A. flos-aquae* … observed mainly during
winter", and concludes it "can grow over a wide range of water temperatures".

**So the model's `T_opt = 26 °C` is well supported — it sits inside the measured 23–29 °C
optimum — and `T_min = 18 °C` is roughly 10 °C too high.** That is the single wrong number, and
it is wrong in exactly the way that closes autumn.

*Not verified:* clean cardinal temperatures for *Anabaena*/*Dolichospermum* specifically. Three
searches did not return a direct measurement, so the case above rests on *Aphanizomenon*, which
is one of the two dominant genera rather than both.

### What a literature-consistent T_min would change

Recomputing the CTMI over the run's own daily temperatures with `T_min = 8 °C` and everything
else unchanged (validity improves markedly: the margin on 2·T_opt > T_min + T_max goes from 2 to
12):

| month | mean T °C | factor now (T_min 18) | factor at T_min 8 | % of days permitted, now → then |
|---|---|---|---|---|
| Jul | 21.0 | 0.427 | 0.725 | 90.8 → 100 |
| Aug | 20.8 | 0.400 | 0.717 | 91.6 → 100 |
| **Sep** | 16.3 | 0.026 | **0.374** | 21.4 → **100** |
| **Oct** | 10.8 | 0.000 | **0.072** | 0.0 → **85.8** |
| Nov | 6.2 | 0.000 | 0.003 | 0.0 → 24.2 |

Sep–Nov mean factor rises from **0.008 to 0.149**; the annual mean roughly **2.4×**, from 0.096
to 0.231. The thermal gate opens across precisely the months where §3.4 shows the nitrogen
window at its widest.

**This is a projection of the temperature factor alone, not a predicted biomass response.**
Growth is multiplicative with light, phosphorus and the fixation switch, and §3.3 shows standing
stock is set largely by transport rather than local rates. Whether corrected diazotroph
phenology actually moves the chlorophyll peak requires a run — that is the obvious next
experiment, and it is now a well-posed one.

### References

Tsujimura, S., Ishikawa, K., & Tsukada, H. (2001). Effect of temperature on growth of the
cyanobacterium *Aphanizomenon flos-aquae* in Lake Biwa and Lake Yogo. *Phycological Research*,
49(4), 275–280. https://doi.org/10.1046/j.1440-1835.2001.00255.x

Yamamoto, Y. (2009). Environmental factors that determine the occurrence and seasonal dynamics
of *Aphanizomenon flos-aquae*. *Journal of Limnology*, 68(1), 122.
https://doi.org/10.4081/jlimnol.2009.122

---

## 9. The experiment: T_min 18 → 8 °C fixes the inversion

Run 2026-08-11. Identical configuration to the baseline in every respect except **one line**:
`FIX_CYN_OPT_TEMP_LR` 18.0 → 8.0 in `WCONST_04.txt` (the value §8 supports; the diff against
the baseline config is exactly that line). Same 11 years, same boxes, same harness; scored with
the same validator against the same observations.

### Phase metrics — the headline

| | baseline | **T_min = 8** | observed |
|---|---|---|---|
| Chl-a peak month | January | **September** | August |
| peak offset | +5 months | **+1 month** | — |
| autumn/spring ratio | 0.63 | **1.15** | 2.06 |
| seasonal r | −0.70 | **+0.40** | — |

Monthly climatology (µg/L): August model **52.8** vs obs 50.8; September **55.8** vs 50.2. The
six-month inversion is gone.

### The fixer bloom

`FIX_CYN_C` monthly mean (mg C/L, 4 boxes, 11 yr):

| | Jun | Jul | Aug | Sep | Oct | Nov |
|---|---|---|---|---|---|---|
| baseline | 0.010 | 0.030 | 0.100 | 0.031 | 0.006 | 0.007 |
| **T_min = 8** | 0.041 | **0.448** | **1.308** | **1.529** | **0.680** | 0.067 |
| ratio | 4.3× | 15× | 13× | 49× | **106×** | 10× |

The bloom now peaks in August–September at ~1.3–1.5 mg C/L against the observed ~2 mg C/L —
the ~10× amplitude deficit is reduced to ~25–35 % — and holds a substantial October population
where the baseline had none. Spring is untouched (ratios ≈ 1.0 in Jan–Apr), so the change did
exactly what the CTMI arithmetic predicted and nothing else. Competitive rebalancing is
visible and plausible: CYN −43 % in August, DIA −49 % in Sep–Oct (nutrient drawdown by the
fixers), NOST slightly down.

### Full validation — what it costs

| variable | baseline RMSE / bias | T_min=8 RMSE / bias |
|---|---|---|
| **PO4** | 0.0326 / +0.0219 | **0.0241 / +0.0113** |
| **CHLA** | 29.2 / −2.75 | **27.0** / +6.78 |
| NH4 | 0.0548 / +0.0044 | 0.0596 / +0.0192 |
| NO3 | 0.476 / +0.023 | 0.474 / +0.035 |
| DO | 8.00 / +0.29 | 8.03 / +0.51 |
| TN | 1.05 / +0.27 | 1.03 / +0.48 |
| TP / Si | unchanged | unchanged |

**PO4 improves substantially** — RMSE −26 %, bias halved. The chronic summer-PO4
over-prediction (BACKLOG §3's "benthic P retention" frontier) was partly the missing fixer
bloom: diazotrophs that actually grow consume the late-summer phosphorus pool. The cost is a
modest rise in NH4/TN/DO biases, which is mechanistically expected — nitrogen fixation adds
new nitrogen to the system, and a real bloom respires and remineralizes. CHLA RMSE improves
8 %; its bias flips from −2.75 to +6.78 because the winter diatom excess still stands beneath
the recovered autumn.

### Companion experiment: NOST T_min 16 → 8 °C does nothing — it is inoculum-limited

Run 2026-08-11, same harness: `FIX_CYN_OPT_TEMP_LR = 8` **and** `NOST_VEG_HET_OPT_TEMP_LR`
16 → 8 (two-line diff, verified). Result: **phase metrics and aggregate scores identical** to
the FIX_CYN-only run (peak 9, ratio 1.15, r +0.40; PO4 RMSE 0.0241; CHLA 26.96). NOST itself
rises ×2–4 in its season but from a base three orders below FIX_CYN (peak 0.10 vs 1.5 mg C/L);
total phytoplankton moves ≤1 % in every month and October is unchanged (×0.999).

The akinete pool says why: `AKI_C` falls **0.73 → 0.002 mg C/L by August in both runs**. The
NOST bloom is capped by its germination inoculum, which is exhausted before autumn begins —
by October there is nothing left to germinate, so no growth-side T_min can create an autumn
population. **NOST's autumn is a life-cycle problem (akinete formation/germination timing),
not a growth-kinetics problem** — consistent with the CLC-model literature
(`docs/Diazotroph_phenology_modelling_review.md`), which holds blooms into autumn via staged
transitions rather than temperature response. Reverting NOST T_min to 16 for adoption
purposes loses nothing.

### What remains open

1. **October is still low** (32.5 vs 46.4 µg/L). ~~NOST T_min~~ — ruled out above. The residual
   is FIX_CYN's remaining amplitude shortfall (0.68 vs ~2 mg C/L observed-equivalent in
   October) plus, if pursued structurally, the NOST akinete life-cycle timing.
2. **The winter half is untouched** (January 46.5 vs 17.6 µg/L), as expected — that is the
   diatom cardinal-temperature problem (§2), a separate decision.
3. This experiment changed a scratch config only. **Adopting T_min = 8 °C into `INPUTS_CL29`
   is a model-behaviour change and a user decision**, ideally with a recalibration pass after
   it, since the DE work to date was conditioned on fixers that could not grow.

---

## 10. Adoption and recalibration (2026-08-11)

`FIX_CYN_OPT_TEMP_LR = 8.0` was adopted into the live `INPUTS_CL29/WCONST_04.txt`
(byte-identical to the §9 experiment config) and versioned in ESTAS-AQUABC-DATA (`5635e67`).
NOST's T_min stays 16 per the null companion result.

**Recalibration** (`tools/calibrate_cl29.py --paramset all --group-carbon`, 1461-day window,
DE popsize 5, converged after 7 generations / 320 evaluations): Φ 15.299 → 13.696 (−10.5 %).
The optimizer moved the whole N-processing chain up — K_NITR_20 1.0→1.42,
K_MIN_DOC_NO3N_20 1.5→2.32, KDISS_DET_PART_ORG_N_20 0.4→0.62 — which is what the new
fixation-derived nitrogen demands, plus KHS_DIP_DIA 0.003→0.0061 and stronger CYN
(KG 4.95, KD 0.053).

**The full optimum re-hit the composition wall with the roles reversed**: on the full record
it fixes CYN_C (bias −0.72 → −0.04 against obs 1.01 mg C/L) by letting strengthened CYN
competitively crush the fixers (FIX_CYN_C model mean 0.60 → 0.12, bias −0.57). Phase survives
(peak month exactly 8) because CYN carries the autumn bloom instead — but the N-fixation
process this whole arc exists to recover shrinks 5×.

**Adopted instead: the N-cycle subset only** (the four nutrient parameters above at their DE
values; phyto knobs left at defaults). Full-record A/B/C comparison:

| | A: T_min=8 only | B: full optimum | **C: adopted subset** |
|---|---|---|---|
| FIX_CYN_C bias (obs 0.70) | −0.10 | −0.57 | **+0.03** |
| CYN_C bias (obs 1.01) | −0.72 | **−0.04** | −0.77 |
| NO3 bias | +0.035 | −0.050 | **+0.004** |
| CHLA RMSE | 26.96 | 27.36 | **26.11** |
| seasonal r | +0.40 | +0.34 | **+0.53** |
| peak month (obs 8) | 9 | **8** | 9 |

The subset keeps the nutrient gains, scores the best CHLA RMSE and seasonal correlation of any
configuration to date, and brings the fixer bias to ~zero — faster N turnover deepens the
summer DIN drawdown (widening the fixation window §3.2), and the doubled diatom P
half-saturation cedes phosphorus to the summer groups. Versioned in ESTAS-AQUABC-DATA
(`daabebc`); the full optimum remains in `/tmp/cal_adopt/result.json` should the CYN-first
trade ever be preferred.

**Open after adoption:** CYN_C is now the largest composition residual (−0.77 against obs
1.01) — plausibly a grazing-structure problem, since ZOO_C stays 5.5× low in every variant and
no parameter here touches it. The winter diatom excess (§2) stands. October Chl-a remains
below observations (§9, akinete life-cycle).

---

## 11. The winter half: the diatom-envelope experiment (informative, NOT adopted)

Run 2026-08-11 against the adopted §10 config. Motivation: the observed diatom carbon is
**two-sided against the model** — February obs 0.28 vs model 0.94 (3.4× excess) *and* August
obs 0.54 vs model 0.006 (90× deficit). The lagoon's diatom record is a year-round succession
(cool-water *Stephanodiscus* spring guild; warm-water *Actinocyclus normanii* /
*Skeletonema subsalsum* late-summer guild), while the model's single DIA group carries a pure
cold-water envelope (−2, 10, 21). Literature anchors: diatom T_opt ≈ 14 °C for the cool guild
(hourly river-phytoplankton model, 10.1029/2020wr028773); the warm guild has qualitative
"late-summer, warm, nutrient-rich" support (Vesijärvi record, 10.1080/09670269710001737289;
Krammer & Lange-Bertalot ecology via 10.3897/phytokeys.178.64426) but **no clean cardinal
triple** — a weaker evidentiary basis than §8's, and it stays flagged as such.

**Experiment:** DIA CTMI (−2, 10, 21) → (−2, 14, 28), two lines, validity margin 1→2.

**What improved** (vs §10 adopted): Si RMSE 1.54→1.37, bias +1.13→+0.85 — confirming the
missing-summer-Si-consumer mechanism; PO4 RMSE 0.0240→0.0206; NH4, TN, TP all better; winter
Chl-a Jan 46.5→35.3, Feb 43.9→37.6 (obs 17.6/10.2) — a ~25 % cut of the winter excess. Summer
diatoms appear at roughly the observed order: Jul 0.48 (obs 0.37), Aug 0.27 (obs 0.54).

**Why it is not adoptable: June.** Observed diatoms *collapse* in June (0.17 mg C/L, the
annual minimum); the widened envelope instead carries 0.71 through June–July, and that early
surge consumes the phosphorus ahead of the diazotrophs — FIX_CYN_C falls ~40 % across its
season (Aug 1.58→0.97, Sep 1.75→1.28; bias +0.03→−0.21) and CYN_C worsens too (−0.77→−0.85).
The phase ratio slips 1.26→1.10. Trading the recovered N-fixation process for nutrient RMSE
would invert this arc's priorities, so the live config keeps (−2, 10, 21).

**What the experiment establishes:**

1. **Temperature is only ~a quarter of the winter excess.** With winter growth cut 36 % the
   Jan–Feb Chl-a is still 2–3.7× observed. The remaining drivers are the known structural
   pair: the 2× too-transparent light climate (`K_B_E` — winter is exactly when background
   extinction dominates self-shading) and the absent grazing.
2. **The summer diatom guild is real and its absence costs Si, PO4 and NH4 skill** — but
   reproducing a June collapse plus an August return in one group is a succession structure
   (a second diatom guild, or seasonally staged traits), not a parameter. Same class as the
   NOST akinete finding (§9): CL29's remaining phenology errors are life-cycle/guild
   structure, not rate constants.
3. Any future second-diatom-guild design should expect the Si and PO4 gains measured here.

---

## 12. The zooplankton deficit: measured, formalized, and closed to parameters

(2026-08-11.) The 5.5× zooplankton deficit was diagnosed at the code level, tested with a
dedicated DE, and the parameter route is now measured as insufficient.

**Diagnosis** (PROCESS_RATES, verified `ZOO_C_INDEX = 6`, plus the code's own formulas at June
conditions): per-capita ingestion ≈ 0.058/d against fixed losses 0.185/d → **net −0.13/d at
the observed zoo peak month**. The binding constraint is the food-limitation formulation in
`aquabc_II_pelagic_lib_ZOOPLANKTON.f90`: each prey contributes
`dyn_pref·(C−0.02)/(C+KHS)` and the contributions are *summed*, so the total is bounded by the
preference weights (Σ = 0.90 at infinite food of every type, 0.18–0.25 at any composition the
model produces), and the active-switching exponent further suppresses every minority prey.
A Fasham-type formulation saturates to ~1 on *total* preferred food; this one cannot. Two
aggravators: the largest preference (OPA, 0.37) points at an extinct group, and detritus
(~1.2 mg C/L) soaks up the switching weight at half grazing efficiency.

**The `phyto_zoo` DE** (8 knobs incl. KG_ZOO/KD_ZOO/FOOD_MIN, group-carbon Φ, retrained after
the zoo-obs enlargement — see below): converged at Φ +8.3 %. On the full record it replayed
the CYN↔FIX seesaw a third time (CYN bias −0.77 → −0.01 bought with FIX +0.03 → −0.50, Chl-a
worse) — not adoptable, same reason as §10. The zoo knobs themselves moved to KG_ZOO
0.45→**0.25** (down — the optimizer used them to trim grazing losses, not to grow zoo),
KD_ZOO 0.15→0.081, FOOD_MIN 0.02→0.0065, and delivered **ZOO_C +37 % against a 400 % gap.**

**The zoo-trio subset** (three zoo knobs only, phyto untouched): ZOO_C bias −0.0375 → −0.0345
with zero collateral — phase, fixers, nutrients all unchanged. A free but marginal gain;
KG_ZOO = 0.25/d is biologically backwards (a compensation, not a correction), so adopting it
is a judgment call recorded as open.

⇒ **The zoo deficit is structural.** The fix is Fortran: replace the summed preference-diluted
Monods with a saturating total-food response. Any such change re-opens KG_ZOO/KD_ZOO
calibration afterwards.

**Observation base**: the ZOO_C record grew 329 → 577 station-dates (469 in-window) by
ingesting the 2009–2025 annual KM workbook archive (`c1c7c26`); the deficit verdict is
unchanged on the 13-year verified record. Operational rule learned the hard way: the
calibrator reads the obs CSVs live per evaluation, so regenerating them mid-DE silently
corrupts the objective — the first `phyto_zoo` run was killed and relaunched for exactly this.

---

## 13. Route B implemented: saturating food response + quadratic closure

(2026-08-11, branch `feature/zoo-saturating-food`, commits `ddba406` + `99ce233`.)

**The change** (opt-in, `ZOO_FOOD_MODEL = 1` in `PELAGIC_MODEL_OPTIONS.txt`; default path
byte-identical, gated twice): total zoo ingestion saturates on preference-weighted total food
(Fasham-type, `FF = F/(F + KHS_FOOD_TOT_ZOO)`), the per-prey split keeps the active-switching
weights, **and the closure becomes quadratic** —
`R_DEATH = KD_ZOO·ZOO_C·(ZOO_C/ZOO_CLOSURE_REF)`. The closure is not optional: the first
tuned probe with linear closure ran to ZOO_C ~10⁷ mg C/L — the textbook Steele-type NPZ
instability, amplified here by the zoo→death→detritus→food loop. The legacy
preference-diluted cap had been the accidental stabilizer, which is worth stating plainly:
**the old formulation's zoo ceiling and the model's numerical stability were the same
artifact.**

**Verification** (full 11-year record, adopted §10 config otherwise):

| | adopted (legacy zoo) | structure @ defaults | **structure + tuned** | obs |
|---|---|---|---|---|
| ZOO_C bias | −0.0375 | −0.0256 | **+0.0059** | — |
| ZOO_C June/July/Aug | 0.010/0.009/0.009 | 0.021/0.022/0.025 | **0.058/0.070/0.079** | 0.088/0.079/0.054 |
| seasonal r (Chl-a) | +0.53 | +0.54 | **+0.57** | — |
| autumn/spring | 1.26 | 1.27 | **1.34** | 2.06 |
| FIX_CYN_C bias | +0.030 | +0.033 | +0.104 | — |
| CHLA RMSE | 26.11 | 26.11 | 26.92 | — |
| PO4 RMSE | 0.0240 | 0.0241 | 0.0266 | — |

"Tuned" = KG_ZOO 0.6, KD_ZOO 0.08 (at the closure reference 0.05 mg C/L), KHS 0.15 — scale
values, not a calibration. **The zooplankton is alive for the first time**: a real seasonal
cycle at the observed magnitude (bias ≈ 0, best-ever RMSE), and the chlorophyll phase
*improves* to its best-ever seasonal correlation while the fixer bloom stays intact. Costs are
modest and mechanistically expected (a living zoo recycles nutrients: PO4 +11 % RMSE, CHLA
+3 %) — exactly the kind of shift the next N-cycle recalibration pass absorbs, as §10's did.
The model zoo peaks in August against the observed June — it follows the model's phyto phase,
so it should shift left with any further phenology gains.

**Open decisions:** enable `ZOO_FOOD_MODEL = 1` + the three constants in the CL29 config
(user call, like §10's adoption), then a recalibration pass (`phyto_zoo` + N-cycle under the
new response). The zoo-trio compensation values from §12 are superseded by this and should
not be adopted.

---

## 14. Post-zoo recalibration and final adopted state (2026-08-11/12)

With `ZOO_FOOD_MODEL = 1` + the tuned zoo rates enabled in the CL29 config (data repo
`781ae88`), the 8-knob DE was rerun so the N-cycle could absorb the living zoo's nutrient
recycling (window Φ +5.2 %, 240 evals). **The CYN↔FIX seesaw did not fire this time** —
with real grazing in the system the optimizer no longer buys CYN by crushing the fixers.
The full optimum still traded CHLA (+1.0 RMSE) and a fixer overshoot (+0.237) for PO4/Si,
driven by KG_DIA 3.7→4.75, so per the established discipline only the **N-cycle subset
(v2) was adopted** (data repo `dbef92a`): K_NITR_20 **1.878**, K_MIN_DOC_NO3N_20 **2.986**,
KDISS_DET_PART_ORG_N_20 **0.3024**, KHS_DIP_DIA **0.004503**.

Full-record, subset v2 vs the §13 zoo-adopted base: NH4 bias **+0.0184 → +0.0054** (RMSE
−12 %), TN RMSE 0.982 → **0.941**, seasonal r **+0.59** and autumn/spring **1.41** (both
best-ever); costs small and stated — CHLA RMSE +1.4 % (27.30), NO3 bias overshoots to
−0.062 (K_NITR at 1.88 is aggressive), FIX_CYN_C +0.173. This adoption was a close call,
decided by the user; unlike the two previous subset adoptions it does not dominate outright.

**The adopted CL29 state after this arc** (all versioned in ESTAS-AQUABC-DATA):
`FIX_CYN_OPT_TEMP_LR = 8` (§8–9) · N-cycle v1 (§10) → superseded by v2 above ·
`ZOO_FOOD_MODEL = 1` + KHS_FOOD_TOT 0.15 + closure ref 0.05 + KG_ZOO 0.6 / KD_ZOO 0.08
(§13). Against the season-inverted starting point: Chl-a peak Feb→Sep (obs Aug), seasonal
r −0.70 → **+0.59**, autumn/spring 0.63 → **1.41** (obs 2.06), fixer bias −0.10 → +0.17
with the bloom real, zooplankton from 5× low to bias ≈ 0 with the observed cycle, NH4/TN
best-ever, PO4 RMSE 0.0264 vs 0.0326 pre-arc.

**Still open, ranked:** the coupled light-climate move (honest K_B_E + C:Chl + growth
engine — §"K_B_E probe": winter is light-driven but reachable only jointly); the October
akinete life-cycle (§9); the second diatom guild / June succession (§11); CYN_C −0.80
(the largest composition residual); OPA extinct as ever.

---

## 15. The coupled light-climate DE: honest optics cannot buy back the bloom

(2026-08-12.) The standing hypothesis from the calibration arc was that the two measured but
calibration-invisible parameters — `K_B_E` (0.70 modelled vs 2.18 from the measured kd,
n=199) and C:Chl (30/40 modelled vs 53/78 measured, n=312) — could be corrected if the
growth engine were recalibrated *jointly*. With the fixers and zooplankton now alive, this
was tested properly: optics **imposed** (never calibrated), growth engine **freed**.

**The impose-only probe** split the compensating errors exactly as predicted: winter Chl-a
excess *gone* (Feb 12.1 vs obs 10.2, Mar 24.6 vs 25.1 — near-perfect for the first time),
NH4/TN best-ever, ZOO_C bias −0.0004; summer/autumn collapsed ~3× (Aug 16.2 vs 50.8).

**The coupled DE** (`--paramset light --inputs /tmp/inputs_light`, calibrator `c9ca94c`:
all-group KG with wide uppers + the N-cycle four; 160 evals, window Φ +11.1 %): the
optimizer used the headroom — KG_DIA 3.7→**7.98**, KG_FIX_CYN 3.5→**6.37**, KG_OPA
2.9→**5.60** — and it was not enough:

| full record | adopted config | honest-optics best | obs |
|---|---|---|---|
| CHLA Aug / Sep / Oct | 52.8 / 55.8 / 32.5 | 21.6 / 24.6 / 13.3 | 50.8 / 50.2 / 46.4 |
| CHLA RMSE / bias | **27.30 / +10.2** | 29.27 / −14.35 | — |
| seasonal r | **+0.59** | +0.29 | — |
| CYN_C / OPA_C bias | −0.80 / −0.47 | −0.68 / −0.47 | — |

**Doubling the growth engine recovers only ~4 of the ~18 missing µg/L.** In kd ≈ 2.9 water,
depth-averaged light limitation caps production regardless of the rate constant, and the
missing biomass sits exactly where the unresolved composition residuals sit (CYN, OPA, the
fixer amplitude tail). The real lagoon overcomes honest turbidity by **vertical
positioning** — buoyant cyanobacteria accumulate at the surface and escape the
depth-averaged light penalty — which a depth-averaged light formulation cannot represent
at any KG.

**Decision: not adopted; the arc closes with the trade made explicit.** The operational
CL29 keeps the transparent-water compensation (K_B_E 0.70, C:Chl 30/40) — knowingly now:
it is the price of the observed summer bloom under depth-averaged light. The honest-optics
configuration stays as the reference diagnosis (`/tmp/inputs_light` recipe reproducible
from §"the probe"), and the structural exit is **surface-bloom / buoyancy-resolved light**
(the model's `CYANO_BOUYANT_STATE_SIMULATION` exists and is on, but light remains
depth-averaged) — filed with the akinete life-cycle and second-diatom-guild items as
paper-2 structure.

---

## 16. The weakest-point investigation: the CYN guild carries the fixers' disease

(2026-08-13.) With the arc adopted, the largest residual is summer–autumn cyanobacterial
biomass: CYN_C −0.80 and OPA_C −0.47 mg C/L — together half the observed phytoplankton carbon,
and the same mass the §15 honest-optics test could not rebuild.

**Measured causes (adopted config, PROCESS_RATES):** CYN is temperature-forbidden
October–April — `CYN_OPT_TEMP_LR = 15 °C`, the **third taxon-transplant error of the same
class** (FIX_CYN 18°, NOST 16°, CYN 15°) — while the monitoring record's non-fixing guild is
the cool-water filamentous group (*Oscillatoria* 2866 > *Aphanocapsa* 1265 > *Chroococcus*,
*Limnothrix*, *Planktothrix* > *Microcystis* 828). Even at bloom peak CYN is loss-dominated
(0.094 vs 0.109 /d) with negligible grazing; August P competition with the fixer bloom caps
it further. OPA is boundary-held; only its light factor is instrumented (a diagnostic gap).
Also found: `CYANO_BOUYANT_STATE_SIMULATION` is dead code — read, passed, never used; all
groups share one depth-averaged light response (`smith = 1` hardcoded).

**The T_min 15→5 °C probe** (one line; *P. agardhii*-type value): the targeted months behave
exactly as predicted — August 49.8 vs obs 50.8 (essentially exact), November 27.4 vs 24.0
(fixed), CYN bias −0.80 → −0.60, CHLA RMSE 27.3 → 26.5 and bias +10.2 → +6.4, NH4 bias +0.002
— but **seasonal r falls +0.59 → +0.24**, because compressing the summer peak toward
observations leaves the untouched winter diatom excess (Jan 47.3 / Feb 46.0 vs obs 17.6/10.2)
as the model's highest months. The probe also shows real CYN↔FIX niche interaction now
(fixers +0.17 → −0.09): the two guilds trade the same summer phosphorus, as they should.
**Not adoptable alone**; the mechanics are verified, the blocker is the winter half.

**Synthesis — the avenues converge on Scheffer's turbid-state package.** Scheffer, Rinaldi &
Gragnani (1997, *Ecology*) describe exactly this lagoon: filamentous cyanobacteria dominate
shallow turbid systems as an alternative stable state because they win at LOW LIGHT and
create the turbidity. The model lacks every trait in that mechanism: its CYN is warm-water
(T_min 15), shares the community light response, and the water is 2× too transparent (§15).
The coherent structural target is therefore the **combined configuration**: honest optics
(K_B_E 2.18, C:Chl measured) + eurythermal low-light CYN (T_min ~5, group-specific light
response per Oberhaus & Humbert 2007) — under which winter diatoms are properly light-capped
(the §15 probe showed Feb/Mar become near-exact) *and* a low-light CYN carries the
summer–autumn biomass that §15's growth-rate-only test could not. Buoyant surface-layer light
for the fixers (Huisman et al. 2004; Jöhnk et al. 2008) is the complementary lever for the
fixer amplitude. Ranked next experiments: (1) honest optics + CYN T_min 5 + lowered CYN light
saturation, one run; (2) if it holds, the DE on that base; (3) fixer surface-light option.

**References for this section** (scite-checked 2026-08-13, no editorial notices): Scheffer
et al. 1997 (10.1890/0012-9658(1997)078[0272:OTDOFC]2.0.CO;2); Huisman et al. 2004
(10.1890/03-0763); Jöhnk et al. 2008 (10.1111/j.1365-2486.2007.01510.x); Oberhaus & Humbert
2007 (10.1111/j.1529-8817.2007.00414.x); Bonilla et al. 2011 (10.1111/j.1574-6941.2011.01242.x).

---

## 17. The combined Scheffer probe: the depth-average ceiling binds — vertical structure
is now proven necessary by experiment

(2026-08-13.) The §16 synthesis was tested as a single configuration: honest optics
(K_B_E 2.18, measured C:Chl) + eurythermal low-light CYN (`CYN_OPT_TEMP_LR` 5 °C,
`BETA_CYN` 2.0 — the Platt photoinhibition extension shifts CYN's light optimum to I_s/3),
all config-only, on the adopted base.

**Result:** winter stays exact (Feb 9.9 vs obs 10.2; Mar 25.4 vs 25.1) and CYN improves to
its best RMSE anywhere (1.62; bias −0.56), zooplankton bias +0.0003 — but **summer does not
come back** (Aug 14.2 vs obs 50.8), landing *below* even §15's growth-freed 21.6 because the
low-light CYN now competes with the fixers year-round (FIX −0.56).

**Why — the ceiling is arithmetic, not a trait.** The depth-averaged Steele factor is bounded
by e/(kd·H) regardless of I_s or BETA: the exponential bracket cannot exceed 1, so at the
measured kd ≈ 2.9 m⁻¹ and H ≈ 2.5–4 m the light factor is capped at ~0.23–0.37, against
~0.6 under the transparent compensation. Every within-formulation trait (saturation, BETA,
T_min) only redistributes growth *inside* that cap. The lagoon's real low-light winners and
surface bloomers both escape it the same way — by not experiencing the depth-averaged light:
buoyant taxa ride at the surface, and the observed biomass maxima are surface phenomena.

**Consequence:** the configuration space is now exhausted *by experiment* for the
honest-optics summer. The necessary structural change is **an effective-depth light option
for positioned groups** — evaluate the Steele/Smith integral over min(H, H_eff) with
H_eff ~0.5–1 m for buoyant/surface taxa (Huisman et al. 2004; Jöhnk et al. 2008), an opt-in
one-argument change to `LIM_LIGHT` in the same engineering pattern as `ZOO_FOOD_MODEL`.
Secondary keeps from this probe: the CYN T_min correction and its BETA trait are right on
their own terms (best-ever CYN skill in both probes that carried them) and should ship with
whatever light structure follows.

---

## 18. Real wind: the forcing fixed, the accidental subsidy removed, the gate still shut

(2026-08-13, plan `2026-08-13-wind-positioning-activation.md`, Tasks 1–4.)

**Task 1 — the forcing** (`tools/build_wind_forcing.py`, `d2c629a`): ERA5 hourly 10 m wind at
Nida, 2012–2022 (2017–2022 freshly fetched), daily mean of hourly speed, 4017 days, 0 gaps;
validated against 663 instantaneous in-lagoon readings from the hydrometeo workbooks
(r = 0.51, bias +0.23 m/s — daily-vs-spot, level check passed). Jun–Sep median 5.5 m/s; only
~3 % of summer days ≤ 2.5 m/s.

**Task 2 — engagement statistics**: under the Nagy daily gate, full surface positioning
never fires (needs W ≤ 1.1 m/s under honest optics: 0 % of days); the partial branch engages
18 % of summer days in the deepest boxes only. 34 calm spells in 11 summers, none over 3
days. Daily means erase the diurnal calm windows where real scums form.

**Task 3 — A/B on the adopted config**: mild net gain and one irony. DO RMSE 7.92 (best
ever — KAWIND finally sees variability), CHLA 26.84, TN 0.925, DIA bias ≈ 0, autumn/spring
1.43; cost NO3 bias −0.10. But the cyano light factors went *down* 2–7 %: the constant-4 m/s
placeholder had pinned MIX_DEPTH at 3.95 m, giving deep boxes **permanent accidental partial
positioning**, which real wind (summer mean ~6 m/s) removes and episodic calm days do not
repay. The placeholder was not just hiding the mechanism — it was impersonating it, badly.

**Task 4 — the Scheffer package + real wind** (the honest configuration, complete): winter
exact (Feb 9.0 vs obs 10.2, Mar 25.4 vs 25.1), **NH4 RMSE 0.0433, TN 0.899, DO 7.90 with
bias −0.05, ZOO_C bias −0.0003 — the best nitrogen/oxygen/zooplankton state this model has
ever produced** — and August 14.9 vs observed 50.8 (§17 was 14.2: real wind adds 0.7 µg/L
of positioning). The honest configuration is now right about everything except the one
process it cannot express at daily wind resolution: the surface bloom.

**Verdict:** every ingredient is in place and verified — honest optics, correct guild
temperatures, low-light trait, living zooplankton, real forcing — except a positioning gate
that can actually open. **Task 5 (flotation-velocity criterion or sub-daily calm-fraction
weighting in the `_BOUYANT` gates) is the single remaining item between this model and an
honest-optics configuration that matches observations**, and it is a small, well-scoped
Fortran change in the established opt-in pattern. Adoption of the real-wind forcing alone
(Task 3's mild net gain + realism) is a separate, immediate decision.

---

## 19. The sub-daily positioning gate: built, verified, and bounded — the surface bloom
is a persistent state, not a daily fraction

(2026-08-14, wind-plan Task 5; branch `feature/subdaily-positioning`, commits `99bd28e` +
`be21fbd`; opt-in `CYANO_POS_MODEL` + `H_SURF_POS` + `W_CRIT_POS_MIN`, default byte-identical,
all Fortran suites green, direct-caller tests updated with the change.)

**Mechanism:** in the three cyano light gates, the cascade-depth factor is blended with a
0.5 m surface-layer factor, weighted by F_calm — the fraction of the day the hourly wind
sits below the positioning-critical speed, from the within-day wind CDF fitted on 96,432
ERA5 hours (log-quadratic, error < 0.01 in the calm tail). `W_CRIT_POS_MIN` floors the
critical speed: the Nagy inversion treats colonies as passive tracers (W_crit ≈ 1.1 m/s
under honest optics); empirical scum-formation thresholds are ~2–4 m/s.

**The measured ladder** (honest configuration, August climatology vs observed 50.8 µg/L):

| configuration | Aug | Sep | seasonal r | FIX_CYN_C bias |
|---|---|---|---|---|
| Task 4 — no gate | 14.9 | 15.9 | +0.23 | −0.478 |
| gate, Nagy W_crit (1.1 m/s) | 15.1 | 16.1 | +0.24 | −0.471 |
| gate, scum W_crit (3.0 m/s) | **18.0** | **20.0** | **+0.38** | **−0.328** |

Winter stays exact throughout (Feb 8.8–9.0 vs obs 10.2; Mar 25.4–25.5 vs 25.1), and the
scum-threshold run adds new best-evers underneath: DO RMSE 7.83, TN 0.888, PO4 back to
0.0379. Direction fully confirmed; magnitude bounded.

**The bound is structural and now quantified.** The blend scales the day-averaged light
factor by F_calm ≤ ~0.45 even on calm days; a *fully* positioned population at 0.5 m in
kd ≈ 2.9 water carries a light factor ≈ 0.9 vs the column's 0.28 — and extrapolating the
ladder to F → 1 lands almost exactly on the observed August–October plateau (~45–50 µg/L).
**The observed lagoon behaves as persistently positioned through the bloom season** — the
famous multi-week surface accumulations — while a within-day calm fraction can never exceed
its wind statistics. Persistence is a *state* (biomass that stays surface-concentrated
across days once accumulated, reset by storms), i.e. a ratchet with memory — the same
structural family as the akinete life-cycle (§9), not a parameterization refinement.

**Disposition:** the machinery merges (byte-identical default; three literature-anchored
constants for any future use); no option is enabled in the operational config. The
honest-optics programme closes with its ledger fully quantified: winter, nitrogen, oxygen
and zooplankton essentially solved; the summer surface-bloom state identified as the single
remaining structural development, alongside its siblings (akinete staging, second diatom
guild). The operational transparent-compensation configuration remains the shipping one —
now with every term of the compensation measured, bounded, and documented.

---

## 20. The positional ratchet: the honest configuration overtakes the operational one

(2026-08-14/15, plan `2026-08-14-surface-persistence-state.md`; branch
`feature/surface-persistence`, commit `1f1de30`.)

**The mechanism** (opt-in `CYANO_POS_MODEL = 2`): module `AQUABC_POSITIONING_STATE` holds a
surface-positioned fraction S ∈ [0,1] per box per buoyant group —
dS/dt = K_POS_UP·F_calm·(1−S) − K_POS_DISP·F_storm·S, forward Euler on the kinetic step,
persistent across steps; the light blend uses S in place of the memoryless calm fraction.
The shared CALM_FRACTION CDF was uncapped in the process (min(0,·) saturates F naturally),
fixing a dispersal-side artifact and deduplicating three inline copies. Unit-tested (builds
in calm, ratchets across days, collapses in storm); byte-identical default; all suites green.

**The ladder** (honest configuration, August/September vs observed 50.8/50.2 µg/L):

| | Aug | Sep | seasonal r | peak month |
|---|---|---|---|---|
| memoryless blend, W_crit 3 (§19) | 18.0 | 20.0 | +0.38 | Mar |
| ratchet, W_DISP_POS = 4 m/s | 17.9 | 21.1 | +0.39 | Mar |
| **ratchet, W_DISP_POS = 8 m/s** | **34.6** | **35.7** | **+0.70** | **Sep** |

W_DISP = 4 confirms the §19 arithmetic — ordinary 5–6 m/s days carry storm-fractions near
0.9, so the scum collapses daily and the ratchet adds nothing. At W_DISP = 8 the scum
survives ordinary days and is reset only by genuine blows: **the persistence hypothesis is
confirmed**, and the hysteresis width (formation floor 3 m/s, dispersal 8 m/s) is the pair
of numbers that carries it.

**The headline:** the honest configuration — measured K_B_E and C:Chl, literature guild
temperatures, the low-light CYN trait, the living zooplankton, real ERA5 wind, and one new
state with three literature-scale constants — now scores **seasonal r +0.70, the best of the
entire project including every transparent-water configuration** (operational: +0.59), with
CHLA RMSE 26.69 also beating the operational 27.30, winter exact (Feb 8.7 vs 10.2, Mar 25.6
vs 25.1), July nearly exact (22.4 vs 26.2), fixers positive (+0.20) and zooplankton at
observed scale. The compensating-error configuration is no longer the best available
description of this lagoon.

**Remaining gaps, named:** Aug–Sep sit ~16 µg/L below the plateau (S dynamics under real
wind sequences; surface self-shading unmodelled; C:Chl 78 fixed); October (14.2 vs 46.4 —
autumn storm resets compound the §9 akinete inoculum limit); the November/January margins.
No recalibration has yet been run under this configuration — every prior DE was conditioned
on transparent water; the N-cycle four and the growth constants are the obvious first pass.

**Open decisions:** W_DISP sensitivity refinement (6/10 bracketing), the recalibration DE on
this base, and — for the first time a live question — **adopting the honest configuration as
the operational one.**

---

## 21. The first honest-base calibration: project bests, and the seesaw's last stand

(2026-08-15; `--paramset light --inputs /tmp/inputs_honest`; converged in 2 generations /
120 evaluations, window Φ +9.6 %.)

**Full-record best vs the ratchet base:** CHLA RMSE **25.13** (base 26.69; operational
27.30), bias −9.3; **seasonal r +0.72, autumn/spring 1.62, peak September** — all three the
best values of the entire project; August/September 39.1/41.6 (obs 50.8/50.2); **October
25.3** (from 14.2); PO4 RMSE 0.0262; NH4 bias +0.004; zoo intact.

**The parameters tell the §15 story completed:** the optimizer pushed the growth engine to
KG_DIA **8.1** (of a bound at 10) and KG_FIX_CYN **7.6** — under measured optics with
positioning, the production engine wants ~2.2× the transparent-water constants, and now it
*works* because the light is there to use. KDISS_PON halved (0.17) and KHS_DIP_DIA doubled
(0.0084) re-balance the nutrient side.

**The honest costs:** the composition seesaw returned in mirrored form — KG_CYN cut to 1.2,
CYN_C bias −0.72 → −0.88, while the fixers overshoot (+0.56). TN worsens (+0.58). And
KG values near their bounds are the familiar signature of residual compensation — the two
processes still missing from the honest configuration (surface self-shading of the
positioned fraction; photoacclimative C:Chl) are being paid for in growth constants.

**Status:** *not adopted* — recorded as the honest configuration's calibration frontier.
The candidate adoption paths, in increasing ambition: (a) N-cycle pair only (KDISS 0.17,
KHS_DIP 0.0084) on the ratchet base; (b) the full optimum with the CYN cost accepted;
(c) close the two named gaps first (surface self-shading, variable C:Chl), then recalibrate
— at which point the growth constants should fall back toward literature values, the
cleanest test of whether compensation is truly gone.

---

## 22. Path (c) executed: the fifth compensation channel, and where the honest programme rests

(2026-08-15; branch `feature/honest-closure`.)

**Piece 1 — concentrated self-shading** (`b8c4e26`): the positioned fraction's surface light
now attenuates through its own concentrated chlorophyll (K_surf = K_E + 0.02·excess, the
Curonian empiric slope; own-group v1). Unit-tested (a dense scum sees less light than a
sparse one); byte-identical default; the honest-base Φ moved 19.4266 → 19.4454 — the scum
pays a real, small light tax.

**Piece 2 — C:Chl within measured bounds — refuted as a calibration knob.** The 11-parameter
DE (engine + DIA/CYN/FIX C:Chl bounded by the 312-pair IQRs) found **the fifth compensation
channel: pigment inflation.** It drove all three ratios toward their lower bounds (26/50/43),
filling the chlorophyll gap with pigment instead of biomass — window Φ +10.3 % while winter
exploded (Feb 54 vs obs 10) and seasonal r collapsed to +0.47. The RMSE objective cannot be
trusted with a pigment knob it can see when the phase it cannot see pays the bill.
**C:Chl stays fixed at measured values; the honest treatment is photoacclimative structure,
not calibration.**

**The corrected 8-knob DE** (C:Chl pinned, self-shading active) converged in 2 generations
to the *numerically identical* §21 attractor (KG_DIA 8.1, KG_CYN 1.21, KG_FIX 7.62; full
record r +0.72, CHLA RMSE 25.19): at present biomass the self-shading tax is too small to
re-rank a same-seeded 120-candidate search. **The KG-retreat test is unresolved as run** —
the caveat is honest (popsize 5, tol 0.02 converges prematurely on 8 dimensions), and a
deep search (popsize ≥ 15, maxiter ≥ 40) is the stated instrument if the question is to be
forced. The seesaw signature (KG_CYN crushed) persists in the attractor, so residual
structure is still being paid for somewhere — the unresolved trio (OPA extinct at −0.47
through every configuration ever run; the June diatom collapse; CYN's cool-season identity)
remains the likeliest account.

**Where the honest programme rests, in full:**

| configuration | r | autumn/spring | CHLA RMSE | fitted phyto constants |
|---|---|---|---|---|
| operational (transparent) | +0.59 | 1.41 | 27.30 | none (subset-adopted N-cycle only) |
| **ratchet base (honest, unfitted)** | **+0.70** | 1.41 | 26.69 | **none** |
| closure DE best (honest, fitted) | +0.72 | 1.62 | 25.19 | KG at/near bounds, CYN crushed |

The adoption question the user now owns: the **ratchet base** is the philosophically clean
candidate — every constant measured, literature-anchored, or subset-adopted; no fitted
phytoplankton kinetics; already beating the operational configuration. The closure-DE
optimum adds skill (+0.02 r, −1.5 CHLA RMSE, October +11) at the price of pinned growth
constants and the CYN trade. Machinery for both is merged and opt-in.

---

## 23. Adoption: the honest configuration is operational CL29

(2026-08-15, user decision; data repo `a7e633e`.) The transparent-water compensation era
ends. Operational CL29 is now the honest configuration **with the closure-DE engine
optimum**, taking the extra skill at the stated price of interpretability:

- **Measured, fixed, never calibrated:** K_B_E 2.18; C:Chl 53/78 (the §22 pigment-inflation
  lesson); the ERA5 wind record.
- **Literature-anchored:** fixer T_min 8 °C, CYN T_min 5 °C + BETA 2, zoo rates at the
  closure reference; ratchet constants (3 / 8 m/s formation/dispersal hysteresis, 0.5 m
  surface layer) with concentrated scum self-shading.
- **Structural (all opt-in, byte-identical off):** ZOO_FOOD_MODEL 1 (saturating + quadratic
  closure), CYANO_POS_MODEL 2 (positional ratchet).
- **Fitted (the interpretability price, stated):** KG_DIA 8.10, KG_CYN 1.21, KG_FIX 7.62,
  KG_OPA 2.58; K_MIN_DOC_NO3N 2.98, K_NITR 1.94, KDISS_PON 0.174, KHS_DIP_DIA 0.00844.

**Operational scores (the validated full-record run of this exact configuration):**
seasonal r **+0.72** (was −0.70 when this document began), autumn/spring **1.62** (was
0.63), CHLA RMSE **25.19** (was ~29 with the mean hiding an inverted cycle), peak September
(was February), October 25.0 (was ~8 under honest optics), winter Feb 15.9 vs obs 10.2,
PO4 RMSE 0.0264, zooplankton and diazotrophs at observed scale.

**The open frontier after adoption:** OPA extinct (−0.47 in every configuration ever run),
CYN cool-season biomass (−0.88 under the fitted engine), the June diatom collapse /
second-guild succession, October's remaining gap (25 vs 46: akinete staging), the deep-DE
KG-retreat question, and photoacclimative C:Chl as the structural successor to the fixed
ratios. The compensation ledger stands at five channels, all documented in §§8–22.

---

## 24. The OPA extinction: an asymmetric boundary condition, not a kinetic failure

(2026-08-16. Diagnosis only — no change made.) OPA has sat at −0.47 mg C/L bias in *every
configuration this project has ever run*, untouched by optics, temperature, grazing,
positioning or calibration. Measuring instead of assuming, in the established order:

**Not the limitation algebra.** All groups share `LIM = LIGHT × min(DOXY, NUTR)`; the
"changed by Petras" comment at the OPA site is the same expression the others use.

**Not temperature.** OPA's CTMI (10, 17, 23; valid, margin 1 °C) gives an annual mean factor
of **0.300 — higher than CYN's 0.287** — peaking at 0.868 in September, positive on 45 % of
days. Its thermal window is genuinely better than that of the group which outlives it 60×.

**Not its resource traits.** OPA grows faster than CYN (2.58 vs 1.21 d⁻¹ after the adopted
calibration), has the better phosphorus affinity (KHS_DIP 0.006 vs 0.008), and the lower
mortality (0.11 vs 0.125). On local kinetics OPA should beat CYN.

**It is the boundary condition.** The open boundary supplies CYN with a *seasonally varying*
carbon input rising to **0.36 mg C/L in September** (annual mean 0.154), while OPA — like the
diatoms and the fixers — receives a **flat 0.020 placeholder in every month of every year**.
Initial conditions carry the same 31× asymmetry (CYN 0.680, OPA 0.022). In a system where
transport dominates local kinetics, that is decisive: the model's CYN is partly *prescribed*
by its boundary, and OPA is permanently diluted toward a placeholder.

Two consequences, both uncomfortable and both worth stating in the paper:

1. **Part of the model's CYN skill is circular.** The seasonal shape the boundary imposes
   (rising to a September maximum) is the shape CYN is scored against. This does not
   invalidate the CYN-related findings — the §16 T_min correction and §22's crush both act
   on top of it — but the CYN carbon comparison is not an independent test in the way the
   diazotroph or zooplankton comparisons are.
2. **The OPA "coexistence failure" of §3.6 may never have been a coexistence failure.** Every
   calibration that "killed OPA by choice" was choosing between a group with a seasonal
   subsidy and one with a placeholder. The competitive-exclusion interpretation of the
   composition wall needs this caveat attached.

> **Correction (§26):** the asymmetry described here is real and its CYN half is worse than
> stated — but the causal claim below was **refuted by experiment**: raising OPA's boundary
> supply 18-fold made modelled OPA *fall*. OPA's extinction is internal competitive exclusion.
> The boundary fix is still worth making, for silica.

**The fix is data, not code**: Baltic-boundary phytoplankton composition for the strait
(the HELCOM/Klaipėda monitoring holds group-resolved biomass at the boundary stations), used
to give DIA/OPA/FIX_CYN boundary series with the same status as CYN's. Until then, OPA's
bias should be reported as *unconstrained by the experiment*, not as a model deficiency —
and the honest ranking of the remaining structural work puts this ahead of the OPA-related
items in §23, because it is cheap and it re-frames what those items are.

---

## 25. The deep DE: the KG-retreat test answered — negatively — and the seesaw exposed as an
artifact

(2026-08-16/17; popsize 15, maxiter 40, fresh seed, 360 evaluations — 3× the §22 search.)

**KG does not retreat.** With self-shading active and a search 3× deeper on an independent
seed, the diatom growth constant went **up**, not down: 8.10 → **9.17** (bound 10);
KG_FIX_CYN 7.62 → 7.05; KG_OPA 2.58 → 3.71. The honest configuration's inflated growth
constants are **not** an artifact of premature convergence, and the self-shading tax was not
what they were paying for. **Something is still being compensated** — and after §24, the
asymmetric boundary condition is the leading candidate: three of four phytoplankton groups
are diluted toward a flat 0.020 placeholder, so the engine must over-grow them locally to
reach observed biomass at all.

**The composition seesaw is partly a search artifact.** The §21/§22 attractor's signature
move — KG_CYN crushed to 1.21 — did not recur: this search set KG_CYN to **4.01**, above its
default, with essentially the same CYN_C bias (−0.73 vs −0.70). Two distinct parameter sets
give the same composition outcome, so "the optimizer sacrifices CYN" was over-read; what the
data actually constrain is the *outcome*, not the route.

**Neither optimum is global.** The deep search converged in 2 generations on `tol 0.02` to
Φ 18.46, *worse* than the shallow run's 17.57, on the same objective and base. The Φ
landscape is rugged and the tolerance stops both prematurely; the adopted configuration
(§23) remains the best-scoring one found, but it is explicitly a good local optimum, not a
demonstrated best. Its full-record scores still lead: r +0.72 / CHLA 25.19 against this
run's +0.66 / 25.55.

**What this closes and opens.** Closes: the §22 caveat — the KG question is now *answered*
(no retreat), and the seesaw narrative is corrected. Opens: the boundary-symmetry experiment
(§24) is now the highest-value next step *for interpretability as well as skill* — it is the
one candidate that would explain inflated growth constants across three groups at once, and
it is a data task, not a modelling one.

---

## 26. The boundary-symmetry experiment: §24's hypothesis refuted, and the silica residual
solved

(2026-08-17; `tools/build_boundary_phyto.py`, commit `1399e9f`; A/B against the adopted
configuration.)

**What the Baltic archive says.** Deriving all four groups by one method from one source
(`Fitoplanktonas_BJ_*`, 34,301 rows 2018–2023, Curonian plume zone LT3/LT4/LT5, the same
group mapping and C:biovolume ratios as the in-lagoon ingester) gives boundary climatologies
very unlike the shipped forcing: OPA peaks **0.369 in April** against a flat 0.020
placeholder (18×), DIA 0.150 in March (7.5×), FIX 0.118 in July (6×) — **and CYN's shipped
series is itself wrong**: 0.356 peaking in September against an observed 0.197 peaking in
June, i.e. roughly twice too high and a quarter-year late.

**The hypothesis is refuted.** §24 proposed the boundary asymmetry as the cause of OPA's
permanent extinction. With OPA's boundary supply raised up to 18-fold, **modelled OPA fell**
— 0.0057 → 0.0022 mg C/L, bias −0.474 → −0.476. The mechanism is now visible: the symmetric
boundary also feeds DIA and FIX, and OPA loses the resulting competition inside the lagoon
harder than it gains from its own supply. **OPA's extinction is genuinely internal
competitive exclusion, not a forcing artifact**, and the §3.6 composition-wall reading stands
after all. §24's diagnosis of the asymmetry was correct; its causal attribution was wrong,
and the experiment is what separated them.

**The unexpected win: silica.** Giving diatoms their real boundary supply — which raises
autumn–winter diatoms 2–3× (October 0.008 → 0.021 mg C/L) — draws down the silica the model
has over-predicted since the very first calibration:

| full record | adopted | **symmetric boundary** |
|---|---|---|
| **Si RMSE / bias** | 1.59 / +1.28 | **0.82 / +0.21** |
| PO4 RMSE / bias | 0.0264 / +0.0119 | **0.0232 / +0.0077** |
| seasonal r | +0.72 | **+0.73** |
| autumn/spring | 1.62 | **1.66** |
| FIX_CYN_C bias | +0.544 | **+0.443** |
| ZOO_C bias | +0.0079 | **−0.0013** |
| CHLA RMSE / bias | **25.19** / −9.44 | 25.71 / +7.94 |
| NH4 RMSE | **0.0456** | 0.0489 |

**Si RMSE falls 48 % and its bias 84 %** — the silica over-prediction was among the oldest
unexplained residuals in this project (with PO4 it carried ~64 % of the original misfit), and
it turns out to have been a missing boundary diatom supply, not a kinetic or burial problem.
PO4 improves too, the phase metrics reach new (marginal) bests, and the fixer overshoot and
zooplankton bias both shrink. The costs are small and in chlorophyll/ammonium.

**Disposition:** a strong net gain resting on a defensible, single-method dataset, and it
corrects a forcing that was demonstrably wrong for all four groups. Recommended for adoption;
the caveats are stated in the tool's docstring (plume water carries outflowing lagoon
signal — an upper bound; 2018–2023 archive against a 2012–2022 run, used as a climatology
exactly as the shipped CYN series is). A recalibration under it is the natural follow-up,
since the growth constants were partly compensating the missing supply.

---

## 27. Post-boundary recalibration: no gain to take, and the inflation shown to be
compensation

(2026-08-17; `light` set, popsize 8 / maxiter 25 / seed 11 on the newly adopted boundary.)

**The boundary was worth 14.6 % of the objective on its own.** Baseline Φ fell 19.4454 →
**16.6117** with no parameter changed — the largest single-change improvement in this study,
and it came from data rather than fitting.

**The recalibration found nothing to add.** Best Φ 16.7224 — **0.7 % worse than the adopted
parameters**, which the Latin-hypercube initialisation never contains. Full-record: the two
are indistinguishable (Si 0.824 vs 0.825, PO4 0.0232 vs 0.0231, CHLA 25.52 vs 25.56, ZOO
identical); the search's NH4 is better in bias but worse in RMSE, its CYN slightly better,
its phase slightly worse (r +0.69 vs +0.73). **Nothing is adopted; the configuration adopted
in §26 stands.**

**But the parameter it chose settles the §25 question.** The search reached its
near-equivalent score with **KG_DIA 8.10 → 5.769** — a 37 % retreat, and the first time this
constant has fallen under any intervention (self-shading: no; deeper search: it rose to
9.17). Two configurations differing by 40 % in the diatom growth constant now score within
0.7 % of each other:

> **the inflation was compensation for the missing boundary supply.** With diatoms given
> their real 7.5× spring input, the constant is no longer identifiable — the objective has
> gone flat along that axis, which is exactly the signature of a compensation that has been
> removed rather than merely relocated.

That closes the thread opened in §21 and left explicitly unresolved in §22 and §25. It also
means the adopted configuration's inflated constants are now *cosmetic rather than
load-bearing*: they can be set to literature values at a cost of 0.7 % in Φ and ~0.04 in
seasonal r, which is a legitimate scientific choice — a "literature-parameters" variant is
recorded here as available (KG_DIA 5.77, KG_CYN 2.79, KG_OPA 4.14, KG_FIX 7.88; K_NITR 1.35,
KDISS_PON 0.505, KHS_DIP_DIA 0.00276), not adopted.

**Unmoved by any of it: OPA** (−0.476 in both). Three independent interventions — boundary
supply, growth constant raised to 4.14, and the whole community recalibrated around it — and
it does not return. The §3.6 competitive-exclusion reading is now confirmed from three
directions.

## 28. The residual re-measurement: the summer-PO4 premise is dead, the diatom
two-sidedness stands (2026-08-23)

Two backlog items were gated on re-measuring their residuals under the adopted post-boundary
configuration before building anything: the **second diatom guild** (June collapse / August
return) and **benthic P retention** (premised on a ~10× *summer* PO4 over-prediction measured
in the transparent-water era). This section is that measurement.

**Method.** Fresh full-record run (0–4016 d from 2012, daily print interval) of the live
operational configuration — `INPUTS_CL29/` exactly as adopted in §23+§27 (symmetric boundary,
honest optics, ratchet `CYANO_POS_MODEL=2`, closure-DE engine), `ESTAS_HOLD_VOLUME=1`, binary
rebuilt from `main` at `883cc0b`. Scored against the merged EPA + KM-plankton tidy observations
(78,263 rows) with the **live C:Chl (53/78) via `--wconst`** — note the §23 trap runs both
ways: the validator's built-in defaults are the *old* 30/40 ratios, so a current run scored
without `--wconst INPUTS_CL29/WCONST_04.txt` mis-computes CHLA. Monthly climatologies are
obs-matched pairs (model interpolated to each observation's date and box), not
climatology-vs-climatology. **Verification that this is the documented state:** PO4 RMSE
0.0232, Si 0.8235, CHLA 25.52, NH4 0.0489, OPA bias −0.476, FIX +0.443, ZOO −0.001, seasonal
r +0.70, peak Sep — all reproduce §26/§27 to the digit.

**Finding 1 — the benthic-P premise no longer exists; the PO4 residual moved to autumn.**
Monthly PO4 model/obs ratios: Jul **0.16**, Aug **0.16**, Sep **0.24** — the model now
UNDER-predicts summer PO4 four- to six-fold (obs ~0.004–0.006, model ~0.001 mg P/L). The
over-prediction is now **Oct 6.1×, Nov 4.9×** (biases +0.019, **+0.052** — the largest single
monthly nutrient bias in the run), plus Jun 4.1× and Feb 2.3×. The October–November excess
coincides month-for-month with the missing autumn bloom (CHLA Oct −25.9, Nov −20.4 µg/L vs
obs): the biomass that isn't there isn't consuming the P that is. ⇒ **The autumn PO4 excess is
plausibly the SAME defect as the October chlorophyll gap — the missing akinete-staged autumn
population — not a missing benthic sink.** A benthic P-removal process would now *worsen*
Jul–Sep (already under). Benthic-P is demoted to blocked-on-akinete-staging: re-measure again
after the autumn bloom exists.

**Finding 2 — the diatom two-sidedness survives the boundary supply, sharpened.** Cool-season
excess persists: Feb 2.29×, Mar 1.90×, Apr–May 1.36×, and **Jun 1.94×** — the observed June
clear-water collapse (obs 0.196 mg C/L, the annual minimum) is not reproduced (model 0.380).
Warm-season absence is total: Jul 0.08×, **Aug 0.005×** (obs 0.564 vs model 0.003), Sep 0.01×,
Oct 0.03×. Silica carries the matching signature: **Aug 2.03×, Sep 2.21×, Oct 2.10×** — the
§26 boundary fix bought the RMSE via the autumn–winter boundary drawdown, but the *summer*
Si consumer is still missing. ⇒ the second-guild item stands as specified, with fresh numbers.

**Bycatch worth recording.** (a) The "CYN cool-season bias" label is WRONG — the CYN_C miss is
a **summer-guild absence**: obs peaks Aug 2.30 mg C/L vs model 0.06 (ratio 0.03), Jun–Oct all
≤0.27×, while Feb–Mar are roughly right. (b) FIX_CYN now runs **early and hot**: Jun 7.4×
over (model 0.83 vs obs 0.11 — the bloom starts a month early), Jul 2.4×, Aug 1.6×, then Nov
0.47× under — the +0.44 annual bias is mid-season overshoot masking autumn shortfall.
(c) DO shows the missing-bloom photosynthesis signature: Aug obs 12.6 (supersaturated) vs
model 9.7 (bias −2.97), Oct −2.1 — the same two months as the CHLA gap.

Analysis: `/tmp/monthly_residuals.py` against `OUTPUTS_CL29/` (recipe: `INPUT_CL29.txt` with
`PRINT_INTERVAL` 240); numbers preserved here per the scratch-dirs rule.

---

## 29. The akinete-staging ladder: it builds and verifies clean, and its bed bank goes
extinct within four years

(2026-08-25; branch `feature/nost-akinete-staging`, seven tasks, opt-in
`NOST_STAGE_MODEL` flag in `PELAGIC_MODEL_OPTIONS.txt` — `0` = legacy akinete gates
(default, byte-identical to `main`), `1` = bed akinete bank + radiation latch, gated by
five new scalars: `T_GERM_AKI_STAGE` (pre-season germination temperature guard, 12 °C),
`I_FORM_AKI` (formation-latch radiation threshold, 120 W/m2), `KR_GERM_BED` (bed
germination rate, 0.05 /d), `K_MORT_BED_AKI` (bed mortality/burial rate, 0.001 /d),
`V_SETTLE_AKI` (akinete settling velocity, 0.5 m/d). Not adopted into `INPUTS_CL29/`; this
section measures it in a scratch config, per Global Constraints.)

**Build and verification (V1–V5): clean.** Six tasks of TDD build-out (state-variable
plumbing, gate logic, transport-safe derived-type wiring, writer) landed a new
`AQUABC_NOST_STAGING` module and a `NOST_STAGING.out` diagnostic file, all behind the
flag. `flag=0` is **byte-identical to `main`** on both the CL29 full record (60/60 output
files, `diff -rq` clean) and the 0D golden case (`OUTPUT.csv` + `const_out.txt` identical
to the pre-feature baseline) — the opt-in costs nothing when off. Under `flag=1`, the bed
mass identity `BED_AKI + BURIED_AKI − CUM_SETTLE_AKI + CUM_GERM_AKI ≡ 0` holds to the
print quantum (max\|residual\| = 1.000e-10, both the Euler and RK2 solvers, over 2,610
rows each — the residual histogram clusters on integer multiples of 1e-10, i.e.
print-rounding, not a mass leak) and the two solvers agree on the bed state to 0.0013 %
at day 90. The formation-latch phenology lands inside the expected autumn window every
year 2013–2022 (first LATCH 0→1 crossing Sep 8–Oct 3, all 29 boxes; two sunny-September
years, 2015 and 2020, run 1–3 days past a desk-estimated Sep 30 cutoff, ruled a plan
defect and the window moved to Oct 7 rather than a code fix) with the hard zero-before-
Aug-31 invariant holding with no exceptions across all 11 years × 29 boxes.

**V6[a] — self-sustainment: FAIL, decisively.** The brief's criterion is a non-declining
post-formation annual maximum of `BED_AKI` per box, 2014–2022. `BED_AKI` **collapses to
noise-floor values (~1e-8 g C/m2, ten orders below the 2012 peak) by 2016 in all 29
boxes, with no recovery to a meaningful scale in the remaining seven years** — of the 232
box-year transitions 2014–2022, 78 are technically non-declining, but every one of those
78 is a flat comparison between two values already pinned at that ~1e-8 floor, not a
sustained or recovering stock. The system-wide sum of annual maxima runs 222.0 (2012, the
initial-condition transient) → 3.29 (2013) → 0.0576 (2014) → 0.00462 (2015) → 0.000117
(2016) → ... → 5.2e-7 g C/m2 (2022) — a six-order-of-magnitude collapse, complete by 2016,
with **no recovery in the remaining seven years of the record.** The per-box annual
`ΔCUM_FORM/ΔCUM_GERM` ratio (the sustaining-box test) tells the same story: it never
exceeds 1 built from non-floor numbers in any box — `<1` in 2014–2016 as the collapse
runs, then fluctuating 0.2–1.3 across 2017–2022, but by then both the numerator and
denominator are themselves noise-floor quantities (~1e-6 to 1e-7 g C/m2/yr), so no box
shows a genuine `>1` sustaining ratio. Full-record system totals explain the collapse:
`CUM_SETTLE_AKI` 236.3, `CUM_GERM_AKI` 198.5, `CUM_FORM_AKI` 4.33, `BURIED_AKI` 37.8
g C/m2 (summed over 29 boxes). **99.97 % of all settling in the 11-year record happened
by end-2012** — a short, front-loaded drain of the model's inherited water-column
`AKI_C` initial condition into the new bed pool (system-wide the settling input peaks at
day 60, per V7(i) below, not literally day 1), not a sustained ongoing process — and of
everything that ever entered the bed, **only 1.80 % came from in-bed formation**;
germination released 82.5 % of it back out and burial permanently claimed 15.7 %.

The radiation latch itself is not the bottleneck: it opens on schedule every year (confirmed
above) and stays open roughly 71 % of the year (mid-September through late May, one box's
full-record LATCH trace shows), yet `STG_FORM_FLUX` sits at a per-box, per-day constant
of 1.0e-10 (a numerical floor, not a real flux) for 2016–2022 — **formation is
biomass-starved, not light-window-starved**: there is no live `NOST_VEG_HET_C` in the
water column left to convert once the bed's initial charge is spent. The
dead-water-germination fraction is 0 by construction (`aquabc_II_pelagic_lib_NOSTACALES.f90`
gates germination behind `LIM_KG_NOST_VEG_HET_TEMP > EPS_GERM_TEMP_LIM`, the latter =
0.05, declared in `aquabc_nost_staging.f90`) and needs no measurement.

**V6[b] — headline scores: inert for biology; the TN/TP delta is real but confined to the
spin-up years.** Scored full-record against the adopted baseline (same validator, same
obs, same `--wconst`): CHLA RMSE 25.52/25.52, bias −11.3/−11.3, seasonal r +0.70/+0.70,
peak month Sep/Sep; PO4 0.0232/0.0232; Si 0.8235/0.8233; NH4 0.0489/0.0489; DO
7.917/7.916; ZOO_C bias −0.001/−0.0013; FIX_CYN_C bias +0.443/+0.442; DIA_C −0.135/−0.135;
CYN_C −0.892/−0.892; monthly FIX_TOT Jun 0.83/0.11→0.8347/0.1132 (7.4×/7.37× overshoot,
unchanged), Oct 1.27/0.81→1.27/0.81, Nov 0.21/0.46→0.21/0.46 — **every biologically live
channel is unchanged to 3–4 significant figures.** TN and TP are not: RMSE 0.9506→0.8607
(−9.5 %) and 0.04166→0.02991 (−28 %), bias +0.58→+0.53 and +0.0123→+0.0067. A
component-by-component breakdown at the same obs-matched points (NH4, NO3, DON, PON,
ZOO_N and each phyto-carbon pool × its N:C, TN-contribution basis) finds the two runs
identical on every term **except `AKI_C`**: 0.0480 mg N/L average under the legacy gate
vs 0.0003 under staging, a delta of −0.0478 — **92 % of the entire TN bias improvement**,
the same mechanism scaling to ~93 % of the TP delta via P:C. Splitting that delta by year
localises it completely: **2012 alone carries 0.898 vs 0.005 mg N/L (the gap), 2013 a
residual 0.0029 vs 0.00003, and every year 2014–2021 is 0.00000 vs 0.00000 in both
configs, at the actual EPA-scored boxes** — the entire TN/TP gain is the model's initial
`AKI_C` charge decaying faster under staging during the 2012–2013 spin-up, not an
ongoing 2014–2022 effect. Read from source, not guessed: `aquabc_II_pelagic_model.f90`
adds a new sink term, `R_SETTLE_AKI`, directly onto the legacy `NOST_AKI_C` (`AKI_C`)
derivative only when staging is on (zeroed at `flag=0`) — staging drains the very state
variable it was built to replace, faster than the legacy gate does, in the scored boxes
themselves. A related but **separate** observation, not the cause of this score delta
(none of the affected boxes are EPA-scored): under the legacy gate, `AKI_C` has no
independent decay pathway when its germination trigger doesn't fire, and it is measured
**pinned at its 8.0 mg C/L initial condition for seven straight years (2012–2018) in
boxes 1, 4, 10, 12, 13** before an abrupt 2019 release, coincident with a **system-wide**
`NOST_VEG_HET_C` revival across all 29 boxes in 2018 (per-box maxima up to 0.278 mg C/L,
many boxes below 0.04, with a smaller 2021 echo) — the largest post-2015 sign of life the
legacy model ever produces.
That multi-year freeze is a genuine legacy-model defect and staging's `R_SETTLE_AKI` term
would remove it by the same mechanism if it reached those boxes, but this was not traced
to a specific causal pathway into the TN/TP score. **The trade V6[a] documents stands
regardless:** an unconditional settling/burial pathway with no refuge is what both drains
the spin-up transient faster (the TN/TP gain) and drives the new bed bank to total,
unrecovered extinction (V6[a]) — the same design choice, two different timescales.

**V6[c] — competition: none, because the challenger never shows up.** Obs `FIX_CYN_C` is
scored against model `FIX_CYN_C + NOST_VEG_HET_C`; splitting the two at the same
obs-matched points, `NOST_VEG_HET_C`'s contribution is **exactly 0.0000 mg C/L in every
month** at the six KM-obs boxes. June is not materially worse (7.37× vs the 7.4× baseline
overshoot) and November did not improve (0.2145 vs 0.21, obs 0.457) — **neither stacking
nor substitution occurs; the species never reaches a scale where either is possible.**
The Oct–Nov "does the autumn formation drain clear the population early" question has no
autumn population to clear: box-level daily inspection across all 11 years shows
`NOST_VEG_HET_C` already ≤0.001 mg C/L by late September and exactly 0.0000 by October
every single year — the collapse documented in V6[a] is not an autumn event, it is
already months old by autumn.

**V7(i) — no-recruitment control (`KR_GERM_BED = 0`, full 4016-day record): a clean,
single-parameter decay.** With germination permanently blocked (`CUM_GERM_AKI ≡ 0`,
confirmed for all 116,464 rows), the bed accumulates via settling alone for ~60 days
(system-wide sum peaks at 214.8 g C/m2 on day 60, the accumulation-only ceiling) then
declines every single year for the rest of the record, purely via burial —
`STG_FORM_FLUX` still contributes only 4.5e-6 system-wide over 11 years, confirming
formation's starvation is independent of germination. The year-over-year decay ratio is
**0.6942–0.6956 in every one of the ten transitions 2013–2022**, matching
`exp(−K_MORT_BED_AKI × 365) = 0.69420` to three significant figures every time — a
693-day (1.9-year) half-life with no size-dependence. By day 4016 only **1.97 % of the
peak remains** (4.23 of 214.8 g C/m2), even though nothing ever drained it via
germination. This is the ceiling on inoculum persistence the mechanism can offer in
its most favourable case: with recruitment fully suppressed, the bank still cannot
survive a decade.

**V7(ii) — weakest-bloom-year / winter carryover: one collapse, not a series of blooms.**
`NOST_VEG_HET_C`'s system-mean annual max (all 29 boxes, `PELAGIC_BOX_*.out`) runs 0.503
(2012) → 0.0055 (2013) → 0.00008 (2014) → **exactly 0.0000 for every year 2015–2022.**
There is no natural interannual bloom variability to rank a "weakest year" against — the
model produces one bloom (the initial-condition transient), which fails once and never
recurs; the requested experiment has effectively already run eight times (2015–2022) with
the same outcome each time. What can be measured directly is winter carryover: four
independent Dec 31 → Apr 1 windows (2012→13 through 2015→16), spanning five orders of
magnitude in starting stock (3.29 down to 0.000117 g C/m2 system-wide), all show
**91.3–91.4 % survival**, with `CUM_GERM_AKI` flat to six decimal places in every window
(germination contributes nothing in winter, as expected under the `LIM_TEMP` gate) — the
loss is burial alone, matching `exp(−0.001 × 90) = 0.9139` almost exactly. **The bank has
no size-dependent winter refuge: a nearly empty bank bleeds at the same fractional rate as
a full one.**

**A note on run behaviour, retracted.** An earlier draft of this section reported the
`NEGATIVE MASS PREDICTED` console clamp (`mod_PELAGIC_ECOLOGY.f90`'s existing, pre-branch
safety net against `NO3_N`, `ZOO_N` and `ZOO_P`, confirmed present on `main` before this
feature) as staging-correlated — millions of alerts under `flag=1`, zero under `flag=0`.
**That claim is false, and the mechanism is now identified: an observation artifact, not
a model difference.** The `flag=0` comparison logs (`v2_main_run.log`, `v2_branch_run.log`)
were piped through a `grep -vE` filter *at launch* (`/tmp/stg_ab/run_v2_chain.sh`) that
strips exactly these alert lines, so they could never have shown any — the original
comparison was filtered-log-vs-unfiltered-log, not flag=0-vs-flag=1. Re-run with `flag=0`
captured unfiltered, same windows: **170 d — 141,965 (flag=0) vs 141,916 (flag=1); 300 d
— 1,453,475 (flag=0) vs 1,449,748 (flag=1)**. `flag=0` emits slightly *more*, not zero —
staging makes no material difference either way (−0.03 % to −0.26 % across the two
windows, sign not consistent enough to call a real reduction). The same three states are
implicated in both configs for the same reasons: `ZOO_N`/`ZOO_P` are floor-chatter from
their zero initial condition in both configs, and `NO3_N` rides the summer-depletion
floor identically, with the same onset day (~146) in both. **This is pre-existing,
flag-independent model behaviour, unrelated to the staging feature.** Reusable lesson:
filter noisy run output at analysis time, never at capture time — a launch-time filter
can silently manufacture a spurious cross-run difference that looks exactly like a real
one until someone diffs the launch command, not just the output.

**The adoption question.** At default parameters, `NOST_STAGE_MODEL=1` changes nothing
biological the October-gap motivation cared about (CHLA, PO4, Si, the FIX_TOT monthly
story) because its own mechanism cannot outlive its first four years. It does deliver a
measured TN/TP improvement (−9.5 %/−28 % RMSE), but that gain is confined to the
2012–2013 spin-up window (the `AKI_C` initial charge draining faster under staging in the
scored boxes) — a bulk-mass-accounting gain from ~10 % of the observation record, not a
sustained one, and not (as traced) the same thing as the separately-observed multi-year
freeze bug in the non-scored boxes. Turning it on today costs nothing on any of the
metrics this project has tracked (identical CHLA/PO4/Si/NH4/DO/ZOO/composition from 2014
onward), buys a modest spin-up-era TN/TP gain, but delivers none of the autumn-bloom
biology the feature was built for, because the bed bank cannot survive to deliver it.
Whether to (a) adopt as a small, largely cosmetic, zero-biological-effect scoring gain,
or (b) hold and recalibrate `KR_GERM_BED`/`K_MORT_BED_AKI`/`I_FORM_AKI` so the bank can
actually self-sustain before judging the biological payoff, is the user's call — this
section reports what was measured, not a recommendation.

---

## 30. The vegetative-viability probes: the niche was the binding constraint — the role
swap closes the life cycle, and the tuned optimum beats the operational configuration

§29 ended with the staged guild built, verified, and unable to self-sustain: formation
biomass-starved, the bank collapsing to a noise floor by 2016 in every box. This section
reports the probe ladder that found *why*, and the hand-tuning that turned the answer into
the best chlorophyll configuration the project has produced. All runs are scratch-config
experiments on the staging branch binary (full record, staging ON); nothing here is adopted.

**The 2×2+2 mechanism ladder.** Two parameter hypotheses and two structural ones, isolated:

| Probe | Change vs staging defaults | Per-cycle return (ΣΔCUM_FORM/ΣΔCUM_GERM, real-number years) | Outcome |
|---|---|---|---|
| P1 | `NOST` T_min 16→8 °C (lit., *Aphanizomenon*) | 0.01–0.02 | collapse |
| P2 | `KG_NOST` 1.29→7.6 (parity with the tuned engine) | 0.03–0.07 | collapse |
| P3 | both | 0.05–0.08 (2012 VEG ×5) | collapse |
| P5 | P3 + `KR_FORM_AKI` 0.1→0.5 (fast encystment) | 0.11–0.18 | collapse |
| **P4** | **P3 + `KG_FIX_CYN` 7.62→1.29 (role swap)** | **1.1–1.7 every year** | **self-sustains** |

The parameter levers help and lose: even with literature cardinals and engine parity the
vegetative phase returned under a tenth of the carbon germination handed it, and winning the
autumn encystment race (P5) only doubled a losing ratio. The binding constraint was the
**niche**: `KG_NOST` sat at its untuned default (1.29/d) because NOST was extinct in every
calibration the project ever ran, while its unstaged twin `FIX_CYN` — tuned to 7.62/d by
those same calibrations — occupied the fixer niche at 2.3 mg C/L. A staged guild cannot
invade a niche its unstaged surrogate holds. P4 hands the niche back (which is also the
biologically honest arrangement — the lagoon's dominant fixer *Aphanizomenon* IS the
akinete-former): the bank runs 241→314 g C/m² over eleven years, non-declining, a weak 2015
buffered by the bank and recovered the following season — the CLC inter-year memory working
as published — with vegetative blooms of 5–8 mg C/L every year through 2022.

**P4 against observations** carried three firsts at a real cost: chlorophyll peak month
August — exact, for the first time in the project (the operational configuration peaks
September); Aug/Sep magnitudes 55.4/50.0 vs observed 50.8/50.2 (the operational −16 µg/L
summer gap gone); PO4 RMSE 0.0167 (−28 % vs operational, project best). Costs: seasonal r
+0.54 (vs +0.70) from a June–July overshoot (39/51 vs 21/26 — the surrogate's documented
early start, inherited and amplified) and an October early exit (16 vs 46 — the formation
latch encysts the bloom in September); FIX_TOT bias +1.2; TN/TP up with the added fixation.

**The tuning ladder (T1–T5, all self-sustaining).** Three physics-matched knobs against the
two phase errors: T1 = timing pair (`T_GERM_AKI_STAGE` 12→16 °C — germination waits for the
measured sustained->16 °C crossing, Jun 18–Jul 8; `I_FORM_AKI` 120→85 W/m² — the EMA crossing
slides from mid-September to early October); T2 = `KG_NOST` 7.6→4.5 (dominated — and VEG
maxima barely moved, the bloom is resource-capped, not KG-capped); T3 = both (Aug/Sep
undershoot); T5 = T1 with `T_GERM` 18 °C (August 50.8 vs 50.8 exact, autumn/spring 2.05 vs
2.06 — the aesthetic alternative, at a worse July); **T4 = T1 + `KR_GERM_BED` 0.05→0.02
(the inoculum arrives over weeks, not days) — the optimum**:

| | Operational (adopted §23/§27) | T4 |
|---|---|---|
| CHLA RMSE | 25.52 | **24.35 — project best** |
| Seasonal r | +0.70 | +0.67 |
| Peak month (obs Aug) | Sep (+1) | **Aug (exact)** |
| Aug / Sep (obs 50.8/50.2) | 35.4/36.8 | **46.7/45.8** |
| Oct (obs 46.4) | 23.6 | 22.5 |
| PO4 RMSE | 0.0232 | **0.0182** |
| TN RMSE | 0.951 | **0.844** |
| FIX_TOT bias | +0.44 | +0.85 |
| Staged life cycle | guild extinct | **self-sustaining (ratios ≥1.2 avg)** |

**Residual attribution.** T4's remaining July overshoot (+14 µg/L: the seeded bloom still
builds fast — a calibration target on the staged knobs) and the October/November deficits
(22.5/2.8 vs 46.4/24.0) — which §28's decomposition already assigned to the *other* guilds
(observed CYN ≈1.9 mg C/L vs model 0.27, OPA extinct): not this mechanism's job. The 0.03 r
gap to the operational configuration consists almost entirely of those two known items; NH4
pays ~6 %.

**Recipe (scratch; adoption is a separate decision):** the live `INPUTS_CL29` plus staging
options ON with `T_GERM_AKI_STAGE 16.0`, `I_FORM_AKI 85.0`, `KR_GERM_BED 0.02` (other
staging scalars at defaults), and WCONST `KG_NOST_VEG_HET_OPT_TEMP 7.6`,
`NOST_VEG_HET_OPT_TEMP_LR 8.0`, `KG_FIX_CYN_OPT_TEMP 1.29`. For the paper-2 narrative: the
invisible-parameter taxonomy gains a seventh class (the paper's count: the asymmetric
boundary of §26 is the sixth) — **a niche held closed by a surrogate's
calibration history**; every DE that tuned `FIX_CYN` upward was, unknowingly, voting the real
species' life cycle out of existence.

---

## 31. Adoption: the staged fixer is operational CL29 (2026-08-29, user decision)

The T4 configuration of §30 is adopted as operational, closing the arc §29 opened. The
staging branch is merged to `main`; the live `INPUTS_CL29` carries `NOST_STAGE_MODEL 1`
with `T_GERM_AKI_STAGE 16.0`, `I_FORM_AKI 85.0`, `KR_GERM_BED 0.02` (other staging scalars
at defaults) and the three WCONST changes (`KG_NOST_VEG_HET_OPT_TEMP` 1.29→7.6,
`NOST_VEG_HET_OPT_TEMP_LR` 16→8, `KG_FIX_CYN_OPT_TEMP` 7.62→1.29 — the fixer-role
consolidation the staging spec deferred, now decided: the akinete-forming guild IS the
fixer, the surrogate demoted). A shallow DE on the staged knobs (180 evals, `staged`
paramset, doc §30 base) found nothing better than the hand optimum — the incumbent
survived its own §27-style test.

**Verified operational scores (fresh full-record run of the live config):** CHLA RMSE
**24.22** (previous operational 25.52 — best ever), seasonal r **+0.68** (−0.02 vs
previous), **peak month August, exact** (previously September), autumn/spring 2.01 vs
observed 2.06, PO4 RMSE 0.0183 (−21 %), TN 0.859 (better), NH4 0.0521 (+7 %, the stated
cost), FIX_TOT bias +0.92, ZOO −0.014; the akinete bank in the operational output runs
215→282 g C/m² over the record, non-declining, annual formation/germination averaging
≥1.2 — the life cycle is self-sustaining in production. ⚠ Scoring runs made BEFORE this
adoption requires the pre-adoption WCONST via `--wconst` (KG/T_min changes; C:Chl
unchanged). Remaining residuals, unchanged in attribution (§28/§30): July fixer build
(+13), October–November (the autumn CYN/diatom guilds and OPA — the open structural items),
winter-edge January.

---

## 32. The July build rate is not a fixer parameter: three lever families measured, none
separates July from August (2026-08-29)

The adopted configuration's July fixer excess (+14 µg L⁻¹ CHLA; obs-matched FIX_TOT Jun
11.7×, Jul 3.6×, Aug 2.3×, Sep 1.7×) invited one more tuning pass. It closed negative, and
completely: **(1) timing/seeding** (§30's T-ladder: germination onset shifted three weeks,
seeding rate halved) does not move July — the seeded bloom reaches its resource ceiling
within days regardless; **(2) the growth engine** (KG 7.6 vs 4.5, §30 T2/T3) does not move
July — same ceiling; **(3) the thermal window** (T_min 8→12 and 8→14, this section) is the
only lever that cuts July (Jul CHLA 36.9→29.4, nearly exact) — and it cannot be
month-selective: August–September fall 13–17 µg with it, October collapses, seasonal r
+0.68→+0.52/+0.46, CHLA RMSE 24.2→26.3/27.1. Both probes rejected; the adopted
configuration stands.

The structural reading is the same one §28 and §30 already carry: the fixer's Jun–Oct excess
is load-bearing compensation for the missing autumn guilds — in August–September it fills the
absent CYN/diatom biomass (which is why cutting it there costs skill), in June–July nothing
masks it (which is why it shows). The July residual therefore resolves with the autumn-guild
structure (warm-water diatoms, the non-fixing cyanobacteria's summer guild, §28), not with
any fixer-side constant. Three families of one-parameter evidence now pin that conclusion.

---

## 33. The autumn community is light-locked: the invasion ladder eliminates temperature,
competition, and engine — the missing structure is a per-group light response (2026-08-30)

Before designing the autumn-guild structure (§32's frontier), a three-rung config-only
invasion ladder tested whether parameterization alone could open the Aug–Oct niche to the
extinct groups. **W1**: OPA's cool envelope — the fourth taxon-temperature transplant
(10/17/23 °C against an OBSERVED August peak of 0.88 mg C/L; August water 22–26 °C sits at
its T_max) — corrected to a warm 12/22/30. **W2**: + CYN engine parity (KG 1.21→3.0, undoing
the closure-DE's sacrifice). **W3**: + staged-fixer yield (KG_NOST 7.6→5.2, the staged DE's
own pull direction).

Result: **OPA did not move at all** — August 0.0009–0.0011 mg C/L in all three rungs,
identical to the adopted configuration, identical to §26's boundary-supply/KG/recalibration
triple. Nine independent interventions across two arcs have now failed to move OPA by even a
factor of two. CYN crept 0.07→0.13 (obs 2.30) — weakly positive to parity AND to fixer yield
(refuting the fixed-nitrogen-collapse counterhypothesis: less NOST mildly *helped* CYN), but
plateaued at 6 % of observation. The fixer yielded as designed (Aug bias +0.92→+0.68; the
akinete bank still self-sustains at KG 5.2, system maxima 122–216 g C/m²) — and the released
share went to nutrients, not to the missing guilds. Headlines statistically unchanged
throughout (CHLA RMSE 24.2–24.5, r +0.68–0.69).

**By elimination, the exclusion mechanism is light** — the one axis no intervention varied,
and the one §16/§17 measured in advance: all groups share ONE hardcoded Smith light response,
and under honest optics (kd ≈ 2.9) the depth-averaged light factor is ceiling-capped at
~0.23–0.37 for any group without a surface escape. The positioning ratchet gave that escape
to the three buoyant cyanobacteria groups only; OPA sits fully under the ceiling, and CYN —
positioned but photoinhibition-flagged (BETA 2) at the surface — is capped both above and
below. The observed autumn community (*Oscillatoria*, *Limnothrix*, *Aphanocapsa*;
*Actinocyclus*) is precisely a LOW-LIGHT-specialist assemblage (Oberhaus & Humbert 2007).
⇒ **The missing structure, named by measurement: a per-group light response (shade
adaptation — group-specific saturation/initial slope), the §16 prescription that was never
built.** Parameterization of the autumn guilds is closed as a path; the design moves to the
light formulation.

---

## 34. The dead BETA block: a parallel-reader gap in the operational model — and the
autumn community closed as unrepresentable by parameters (2026-08-30)

**The bug.** §33's shade-adaptation probes returned bit-identical outputs for different
BETA values, and the trace found why: the ESTAS-side `INIT_PELAGIC_MODEL_CONSTANTS`
(mod_PELAGIC_ECOLOGY) — a parallel implementation of the AQUABC-side registry pair — stops
at constant 318. **Constants 319–323 (the per-group photoinhibition/shade block) were never
read from WCONST on the CL29 path since their introduction**; only the 0D path was extended
when the block was appended. Every CL29 run executed with all BETAs at storage-default 0.0 —
including the adopted `BETA_CYN = 2`, documented as a live trait since the §17 Scheffer
probe (whose four-change bundle never isolated BETA's contribution — which is how the
divergence stayed invisible). The fail-loud constants reader cannot catch this class: the
*file* is index-complete; the *consumer of the array* ignored its tail. Fixed in `ebab415`
(five assignments), with a two-sided regression pair: BETA-zeroed WCONST → old and new
binaries byte-identical (30 d); live WCONST → outputs diverge over a summer-crossing window
(a January-only window cannot see it — CYN's CTMI is zero there, a reusable trap:
**activation tests for seasonal parameters must span the season the parameter acts in**).

**Operational impact: negligible.** Re-verified full-record with `BETA_CYN = 2` truly
active: CHLA RMSE 24.21, seasonal r +0.67, PO4 0.0183, peak August — the adopted scores
hold within noise. No adoption revision required; the record is corrected, not the decision.

**The honest §33 completion.** With the machinery genuinely working: `BETA_OPA = 3` still
leaves OPA at 0.0009 mg C/L in August. Desk arithmetic shows the shade route was fully
exhausted, not under-dosed: OPA's adaptive saturation is ~70 ly/d PAR against an August
surface irradiance of 155–270, so at β=3 the depth-averaged factor **reaches the
e/(ke·H) ceiling (~0.31)** — and at the ceiling OPA's growth (≈0.3–0.4 d⁻¹) only ties its
loss budget. Deeper CYN shade (β 2→4) slightly *hurts* (the surface-positioned fraction
pays the photoinhibition). The last family, losses (W5: KD_OPA 0.11→0.04, grazing
preference 0.37→0.10), produced the first genuine response — OPA **doubled** — to 0.0018
against an observed 0.88: a factor of 2 on a gap of 500.

**Conclusion.** Six parameter families — temperature envelope, competition, growth engine,
boundary supply (§26), light response to its physical ceiling, and losses — are now
measured on the autumn community, and none opens it. §33's "missing structure = per-group
light response" is superseded: the response existed (dead, now fixed) and is insufficient
by arithmetic. **The observed autumn assemblage (OPA ≈ 0.9, CYN ≈ 2.3 mg C/L under
kd ≈ 3) is unrepresentable in the current model structure**: its real-world existence
implies biology the model lacks (mixotrophy, taxon-level buoyancy regulation, littoral or
resuspension-coupled production) — or an observation-mapping question (which taxa the
monitoring aggregates into "other algae" and non-fixing cyanobacteria carbon, and whether
the 29-box pelagic average can see them). The cheap next step is observational: audit the
obs-side composition and station distribution of OPA_C/CYN_C before designing any new
biology. The model-side floor stands at the adopted configuration (r +0.67, October −24
µg L⁻¹, attributed).

---

## 35. The obs-mapping audit names the biology — and the *Planktothrix* loss-side probe
partially reopens the autumn community (2026-08-30)

§34 closed the autumn community as parameter-unrepresentable and prescribed an
observation-side audit before any new-biology design. The audit (species-level
recomposition of the ingested monitoring record, 40,131 in-window rows, 2016–2022, five
LTK stations) answers both open questions by name:

**What carries obs `OPA_C`:** *Mougeotia* 30.5 % (Zygnematales — a tychoplanktonic
filamentous green, classically of littoral/benthic-mat origin, advected into the water
column), then genuinely planktonic chlorococcaleans (*Pediastrum* 25.8 %, *Desmodesmus*
11.5 %, *Oocystis* 5.1 %). The monthly structure splits the residual cleanly: *Mougeotia*
peaks in **August at 45 %** of OPA wet biomass — the summer OPA gap is roughly **half an
observation-mapping artifact** (advected mat material no pelagic growth formulation should
reproduce) — while October–November OPA is *Pediastrum*/*Desmodesmus* (42 %/23 % in Oct):
real plankton, a real residual, at ~half the previously assumed magnitude.

**What carries summer `CYN_C`:** ***Planktothrix* 65–70 % of every month June–October**,
plus *Limnothrix* (8–17 %) — the guild is essentially one organism, *P. agardhii*, the
canonical turbid-shallow-lake steady-state cyanobacterium: mixed-column, ceiling-light
tolerant, and persistent through **minimal losses** (grazing-resistant filaments, low
mortality). The model's CYN carried the opposite loss profile (KD 0.125 d⁻¹ — the highest
cyanobacterial mortality in the file — and grazing preference 0.10), and the invasion
ladder (§33) had only ever tested its *engine*.

**W6 — CYN as *Planktothrix*** (KD 0.125→0.04, grazing preference 0.10→0.03, KG 1.21→2.0,
BETA 2 now genuinely active after §34's fix): **the first real CYN response of the arc** —
June 0.55 mg C/L (obs 1.05; every prior probe ≤0.13), September 0.26, October 0.22,
November 0.085 (2–5× the ladder's plateau) — and two project bests: **CHLA RMSE 24.06**
(adopted: 24.21) and **PO4 RMSE 0.01695** (adopted: 0.0183), with September chlorophyll
exact (49.5 vs 50.2) and October 24.8 (from 22.4). Costs: July rises too (37.6 vs obs
26.2, stacking on the fixer's documented overshoot), slipping the climatological **peak
month to September by a 2.3 µg margin** (Aug 47.2 / Sep 49.5); seasonal r unchanged at
+0.68; and the August CYN hole itself persists (0.16 vs 2.30) — at bloom peak the
competition still excludes it.

**Standing after §§33–35:** the autumn-community residual decomposes into (i) a quantified
observation artifact (the *Mougeotia* share of OPA), (ii) a literature-anchored trait
correction that buys real skill (W6, adoption-grade question), and (iii) two named
irreducibles — the August *Planktothrix* exclusion at bloom peak and the autumn
chlorococcalean greens — which are the honest remainder for any future structural work
(depth-regulating filament biology; a littoral/advective OPA source). No new code was
required to reach any of this.

---

## 36. Adoption: the *Planktothrix* trait correction is operational (2026-08-30, user decision)

The W6 configuration of §35 is adopted: live `INPUTS_CL29/WCONST_04.txt` carries
`KD_CYN_20 0.04`, `PREF_ZOO_CYN 0.03`, `KG_CYN_OPT_TEMP 2.0` (with `BETA_CYN 2` active
since §34's fix). The live file is byte-identical to the verified W6 probe configuration,
so the W6 full-record scores ARE the operational scores: **CHLA RMSE 24.06 and PO4 RMSE
0.01695 — both project bests — September chlorophyll exact, CYN present at 2–5× its former
plateau across June–November, October 24.8.** Accepted costs, stated: the climatological
peak month sits at September by a 2.3 µg margin over August (both within 4 µg of the
observed values), July carries +11 µg (the §32 fixer overshoot plus the new June–July CYN),
and the August CYN exclusion at bloom peak remains (0.16 vs 2.30). ⚠ Scoring runs made
before this adoption requires the pre-W6 WCONST via `--wconst`. The remaining structural
frontier, per §35: the August *Planktothrix* exclusion, the autumn chlorococcalean greens,
and the (observation-side) *Mougeotia* tychoplankton share of OPA.

---

## 37. The August exclusion is nitrogen: the limitation decomposition, the affinity floor,
and the Droop-N justification restored (2026-08-30)

The §35 remainder — *Planktothrix* excluded at bloom peak (August 0.16 vs 2.30 mg C/L) —
now has a measured mechanism. A monthly limitation decomposition of CYN on the canonical
run (CTMI + Monod + the BETA-2 depth-averaged light, desk-computed from outputs and
forcing): **August is the nitrogen minimum** — LIM_N 0.32 at DIN 0.004 mg N/L (the
fixation window's deepest drawdown) exactly when the temperature factor peaks (0.78);
μ ≈ 0.15 d⁻¹ against ≈ 0.14 d⁻¹ losses — net zero, nothing compounds. Two side findings:
CYN's mixed-column light sits AT the physical ceiling (0.30 = e/(k_e·H); no light
structure can help a polymictic column), and **surface positioning actively hurts the
shade-adapted guild in summer** (L_surf 0.25 < L_mix 0.30 — the β=2 physiology is
photoinhibited in the scum), explaining §34's W4b.

**X1 — specialist Monod affinity** (KHS_DIN 0.009→0.003, KHS_DIP 0.008→0.004; *P.
agardhii*-grade): CYN +25–40 % across the season with no cost anywhere (CYN RMSE 1.755,
CHLA 24.05, PO4 0.0170, the fixer easing to +0.85) — **ADOPTED** — but August reaches only
0.22: Monod on a 0.004 standing stock saturates at LIM_N ≈ 0.57 regardless of affinity.
The standing stock is not the resource; the regeneration FLUX is.

**⇒ The Droop-N pilot's 2026-08-01 contraindication is measured-removed.** That review's
own decision rule — "first establish that a genuinely uptake-limited target exists
(LIM_N ≪ 1), and reframe as a phenology question" — is now satisfied on both counts:
August LIM_N 0.32, and the goal is *persistence of the August bloom on June-loaded
reserves plus growth-decoupled flux capture* (phycobiliprotein N storage — *Planktothrix*
biology), not DIN drawdown. The re-scoped design proceeds under the old review's §12.3(b)
corrected-architecture checklist (compile-time state count, allelopathy-aware indexing,
the BOUYANT-path target, the DON-sink conservation fix).

---

## 38. The Droop-N ladder: August biomass ×2.3, and the storage hypothesis
mechanistically refuted by its own pre-registered signature (2026-08-30)

**Premise (§37).** August is CYN's nitrogen minimum; with the adopted X1 affinities the
Monod baseline saturates at LIM_N ≈ 0.57 on a 0.004 mg N/L standing stock and August
*Planktothrix* reaches **0.218 mg C/L against 2.304 observed** (obs-matched monthly means,
6 boxes, n=44). The re-scoped pilot's claim was that the standing stock is not the resource
— that June loading into an internal quota plus growth-decoupled flux capture is.

**What was built.** An opt-in nitrogen quota for the non-fixing cyanobacteria: a 33rd
transported state `CYN_N`, `Q = CYN_N/CYN_C ∈ [0.10, 0.25]` gN/gC, Michaelis–Menten uptake
down-regulated by `f_down = (Q_MAX−Q)/(Q_MAX−Q_MIN)`, Caperon–Meyer growth limitation
`LIM_N = (Q−Q_MIN)/(Q_MAX−Q_MIN)`, and every CYN nitrogen loss re-routed Q-weighted.
Because the state count is a compile-time `parameter`, it ships as a **build variant**
(`make build-estas-varn` → `ESTAS_II_varN`, `nstate=33`) with a `CYN_VARIABLE_N` option
defaulting to 0; the setup is generated from the live one by `tools/make_varn_inputs.py`.
**The standard build is untouched: `ESTAS_II` from this branch and `ESTAS_II` built from
`main` (distinct binaries, md5 `3827bb09…` vs `ba1fa2fe…`) produce a `diff -r`-EMPTY
output tree over the full 2012–2022 record** (61 files, 124 MB). Flagging `CYN_VARIABLE_N=1`
on the standard binary `error stop`s (exit 1) rather than running mis-staged.

**Battery.** Admissibility gate PASS at the committed constants (August Q\* 0.208 →
LIM_N\* 0.721 > 0.571; June Q\* 0.221). Full-record VARN smoke PASS — **116,493 quota
samples, zero out of `[0.095, 0.255]`, zero floor artifacts**, options and transport
echoes matching. Conservation on the degenerate-CYN scenario PASS under both solvers
(max |Σ KINETICS| = 2.0e-6 g/m³/d = the `F30.6` print floor; `rel_net` 1.3e-7 ≪
`rel_conservative` 1.2e-5, the unbiased-rounding signature) — **after zeroing the five
non-CYN growth constants; the scenario as generated FAILS past day 66, see the reusable
trap below.** Two limits on what that PASS means: the scenario must zero zooplankton
(`ZOO_N` is outside the identity's five pools), so **the grazing route `zoo_feed·Q →
ZOO_N` is not exercised at run level** — only by the rate-level unit tests; and the
identity covers biological transformation among five pools, **not a full water-column
mass balance** (transport and the driver's prescribed sediment N flux are out of its
scope by construction). Euler and RK2 both run 90 days clean with the quota in bounds.

**[a] Biomass — PARTIAL (2.3×), below the success bar, above the null bar.**

| obs-matched monthly CYN_C (mg C/L) | Jun | Jul | **Aug** | Sep | Oct | Nov | annual |
|---|---|---|---|---|---|---|---|
| observed | 1.053 | 1.066 | **2.304** | 1.875 | 1.056 | 0.343 | 1.013 |
| baseline (Monod + X1) | 0.666 | 0.274 | **0.218** | 0.313 | 0.246 | 0.0905 | 0.273 |
| Droop-N (VMAX 0.44) | 0.708 | 0.454 | **0.503** | 0.596 | 0.311 | 0.0971 | 0.382 |

August 0.503 against the pre-registered **≥ 0.8 = success / < 0.4 = NULL**: neither — the
spec's own "judgment call presented with the numbers". July +66 %, September +90 %; the
annual bias closes from −0.740 to −0.631. **The October–November persistence spec §7[c]
asks about improves only marginally — October +26 %, November +7 % — leaving both at
roughly 0.29 of observed**, so the autumn residual §28/§33 assigned to the autumn guilds
is not touched by this mechanism.

**[b] No headline regression; the phenology claim is thinner than it looks.** CHLA RMSE 24.05 → **24.02**,
CYN_C RMSE 1.755 → **1.734**, TN 0.864 → 0.852, TP 0.0468 → 0.0449, the over-predicting
fixer easing from +0.854 to +0.699 as CYN takes back N. Costs: PO4 RMSE 0.01701 → 0.01737
(+2.1 %) and seasonal r +0.68 → +0.66. The chlorophyll peak month moves 9 → 8 (obs 8) —
recovering the exactness **§31 already held and adopted (2026-08-29)** and that §36's W6
adoption gave up. **Read the margin before reading the flip**, which is what spec §7[b]
asks for: §36 recorded the loss at a 2.3 µg margin, and here the recovery is by
**0.035 µg/L** (Aug 47.81 vs Sep 47.78) against a baseline September lead of 0.42 µg —
itself a re-baselining, not noise: §37's X1-affinity adoption (KHS_DIN/KHS_DIP tightened)
landed between §36 and this run and lifted CYN's whole-season climatology enough on its
own to shrink the September lead from §36's 2.3 µg down to this run's 0.42 µg baseline
before Droop-N ever ran — a coin flip either way, and *both* months move further from
observed under the flag (|Aug−obs| 1.48 → 2.98, |Sep−obs| 0.47 → 2.43; these are the 2-dp
presentation of the four `phase_summary()`-reported distances in the task-7 report's
fix-round section — 1.4804/2.9790, 0.4707/2.4253 — reproduced exactly by rerunning
`/tmp/varn_ab/t7/chla_margin.py` against each leg's own output tree. `phase_summary()`
[`tools/validate_cl29_vs_epa.py:340`] recomputes the EPA-obs monthly mean per `--outputs`
leg, restricted to that leg's own simulated window, so it is not guaranteed to return the
same absolute obs figure on every invocation over the same `/tmp/cl29_obs_merged.csv`; a
naive difference-of-means recomputation from the baseline/Droop-N/observed absolutes
quoted in the task-7 report leaves a ~0.017 µg/L residual against the printed distances.
This is flagged as an aggregation-order/vintage difference between that report's frozen
numbers and today's rerun, not chased to a specific cause here — treat the printed
distances above as the phase_summary-reported figures of record). DO RMSE is 7.924 →
7.925 — the O2 budget is untouched, as spec §2's FIX 2 wired it to be.

**But the gain is COMPOSITIONAL, not net new carbon — and the annual books close on it
exactly.** Obs-weighted bias deltas over the same 317 obs-matched pairs:

| | DIA_C | **CYN_C** | OPA_C | **FIX_CYN_C** | sum | PHYTO_TOT_C (measured) |
|---|---|---|---|---|---|---|
| Δ bias | −0.0009 | **+0.1092** | −0.0000 | **−0.1555** | **−0.0472** | **−0.0472** |

The CYN gain is *more* than offset by the diazotroph loss, and total phytoplankton carbon
falls slightly (PHYTO_TOT_C RMSE 2.5016 → 2.5070, bias −0.5013 → −0.5485). The summer
chlorophyll climatology says the same thing independently — Jul 42.2 → 41.1, Aug 49.29 →
47.81, Sep 49.71 → 47.78 — which is why the CHLA bias *worsens* (−4.92 → −5.43) while CYN
biomass rises: CYN and FIX share C:Chl 78, so a better-than-1:1 substitution reads as
slightly less chlorophyll. Easing an over-predicted fixer is a real gain and the CYN/TN/TP
RMSEs improve because of it, but **the mechanism moved nitrogen between two guilds; it did
not add August biomass to the lagoon.**

**[c] The quota signature — REFUTED, and this is the finding.** Pre-registered: June must
reach ≥ 0.9 of the band (Q ≥ 0.235) and August must draw below mid-band (Q < 0.175), *else
the storage hypothesis is mechanistically refuted regardless of biomass*.

| monthly-mean Q (domain, 29 boxes) | Jan | May | **Jun** | Jul | **Aug** | Sep | Dec |
|---|---|---|---|---|---|---|---|
| Droop-N | 0.249 | 0.247 | **0.240** | 0.233 | **0.231** | 0.237 | 0.249 |

June passes — but only because **the quota is pinned at Q_MAX all year** (98.7 % of
January and 99.1 % of December samples already sit above the 0.9-band line), so "reaching
Q_MAX in June" carries no information. August fails outright: 0.231 against a 0.175 bar.
This is not a mean-across-a-threshold artifact — the August **5th percentile is 0.191**
and only **1.9 %** of 9,889 August samples fall below mid-band. **There is no June→August
drawdown. The storage hypothesis is refuted on its own pre-registered criterion.**

One transport-side contributor to the high quota, stated so it is not mistaken for pure
physiology: initial and open-boundary `CYN_N` enter at the seed `Q_SEED = CYN_N_TO_C =
0.220` gN/gC — 88 % of Q_MAX, 0.80 of the band — so advected water arrives already nearly
full. **The refutation survives it**: the biomass-weighted August quota is 0.216 (band
position 0.77) and the domain p5 0.191, both still far above the 0.175 bar, and the
budget below shows uptake keeping pace rather than a reserve draining.

**Where the 2.3× actually comes from — measured, not inferred.** The per-term N budget
(box 14, `PROCESS_RATES`, monthly means, mg N/L/d):

```
mon        uptake      resp*Q     death*Q      excr*Q  zoo_feed*Q         net
  6      0.026760    0.019059    0.011673    0.001158    0.000082   -0.005212
  7      0.027941    0.016748    0.010491    0.000955    0.000078   -0.000330
  8      0.022466    0.013480    0.008146    0.000938    0.000043   -0.000142
  9      0.016987    0.011605    0.006721    0.000766    0.000016   -0.002121
```

August uptake is **0.0225 mg N/L/d against a 0.004 mg N/L standing stock — 5.6 stock
turnovers per day** — and net kinetics is ≈ 0: the population is held at a near-constant
quota by continuous in-situ capture of the regeneration flux, not by spending a June
reserve. §37's "the standing stock is not the resource; the regeneration FLUX is" is
therefore **confirmed**, while the storage half of the same sentence is refuted. The
working sub-delta is explicit high-affinity uptake raising August LIM_N from the Monod
ceiling 0.57 to **0.77–0.90** depending on which quota summary is used —
(0.2312−0.10)/0.15 = 0.87 on the unweighted domain mean, 0.77 biomass-weighted, 0.90 over
the six observation boxes; storage contributes nothing measurable.
Note the honest scale of what was ever on offer: the quota seed is `CYN_N_TO_C` = 0.220
and Q_MAX is 0.25, so the luxury band above the seed is only 0.03 gN/gC.

**[d] Sensitivity — not a knife-edge, and not rescuable by these two constants.**

| | baseline | **VMAX 0.44** | VMAX 0.22 | VMAX 0.88 | Q_MAX 0.30 |
|---|---|---|---|---|---|
| Aug CYN_C (mg C/L) | 0.218 | **0.503** | 0.383 | 0.653 | 0.455 |
| CHLA RMSE | 24.05 | **24.02** | 24.04 | 23.99 | 24.02 |
| PO4 RMSE | 0.01701 | **0.01737** | 0.01728 | 0.01746 | 0.01739 |
| seasonal r | +0.68 | **+0.66** | +0.67 | +0.66 | +0.67 |
| CHLA peak month (see [b] on the margin) | 9 | **8** | 9 | 8 | 8 |
| Aug mean Q | — | **0.231** | 0.224 | 0.235 | 0.270 |
| Aug mid-band bar | — | 0.175 | 0.175 | 0.175 | 0.200 |

A **4× span in VMAX moves August by 1.7×** and never approaches 0.8; the August quota
never draws below mid-band in any of them (the bar is band-relative: 0.200 for Q_MAX 0.30).
Widening the band (Q_MAX 0.30) *lowers* August biomass relative to Q_MAX 0.25, because the
same absolute quota buys less LIM_N. The desk gate, run at each setting, predicted this
shape: VMAX 0.22 clears the August leg by a hair (LIM_N\* 0.588 vs the 0.571 baseline) and
fails the June leg, VMAX 0.88 passes both, Q_MAX 0.30 fails June.

**Honesty statements carried with the result.**

- *Spec §8, verbatim:* the A/B is honestly a ONE-MECHANISM, THREE-SUB-DELTA bundle —
  quota limitation + explicit uptake replacing the implicit DON share + Q-weighted routing
  — with attribution inside the bundle from the §7[c] quota signature and a per-term
  N-budget printout, not from pretending a single delta. Here that attribution actually
  fired: the signature and the budget together assign the gain to sub-delta two.
- *Attribution level:* the headline deltas could in principle conflate (i) the mechanism,
  (ii) residual configuration drift and (iii) scoring-path changes. (ii) and (iii) are
  pinned to zero — the full-record standard A/B is byte-identical, the scenario inputs
  differ from the live setup by exactly one option value each (verified by `diff -r`), and
  the validator's TN change is provably a no-op on non-VARN runs. **The deltas are
  attributable to the mechanism.**
- *Spec §2 FIX 3, and it cuts against this result's own favour:* under the flag the
  reported CYN chlorophyll keeps the fixed C:Chl 78 — quota N does not drive pigment. In
  real *Planktothrix* the phycobilin **is** the N store, so N-starved cells lose
  light-harvesting capacity, a penalty this pilot omits. The omission biases the pilot
  **toward** success; a positive result claims less than it appears to, and this
  refutation is correspondingly stronger.
- The Fortran-side derived TN (`GENERATE_PELAGIC_DERIVED_VARS`,
  `mod_PELAGIC_ECOLOGY.f90:369`) still computes CYN's N contribution as `CYN_C·0.22` under
  the flag — it does not read `CYN_N` and is wrong for VARN runs. All TN scoring above
  uses `tools/validate_cl29_vs_epa.py`, which reads the column.
- `MASS_BALANCES.out`'s seven columns do not sum to the state change whenever a prescribed
  sediment flux is active (`SED_FLUX_NO3_SINK.txt` is part of the standard CL29 driver):
  that flux reaches the state at integration time but is written to neither column.
  Pre-existing; discovered while building the conservation checker.
- **Reusable trap, cost one battery failure:** the degenerate-CYN scenario zeroes the other
  groups' initial and boundary conditions, but **`MIN_CONCENTRATION` (1e-10) clamping
  reseeds every pool**, and a reseeded pool with positive net growth climbs back
  exponentially. Diatoms went 1e-10 → 1.89 mg C/L between day 60 and day 90 and their NO3
  uptake — into a pool the N identity does not track — broke conservation at day 67
  (residual 1e-6 → 4.0e-1). Zeroing the five non-CYN growth constants restores an exact
  90-day PASS. **Any "turn group X off" scenario in this model needs the growth rate
  zeroed, not just the initial condition.**

**The adoption question, for the user.** Adopting would make `ESTAS_II_varN` the
operational binary and `INPUTS_CL29_VARN/` (generated, 37 state variables) the operational
setup — a deployment change, not a constants change, and one that must be carried by every
downstream tool that reads state-variable positions. What it buys: August *Planktothrix*
0.218 → 0.503 mg C/L (obs 2.304), July/September similarly, CYN/TN/TP/fixer all slightly
better, and the August CHLA peak month back — but by a **0.035 µg/L** margin, restoring
what §31 already held and §36's W6 adoption traded away, with both summer months landing
further from observed than the baseline's. What it costs: PO4 RMSE +2.1 %, seasonal r
−0.02, total phytoplankton carbon slightly *down*, a second build target to keep alive, and
a mechanism whose own pre-registered signature says it is **not** doing what it was adopted
to do — the biomass comes from explicit uptake, not from nitrogen storage, and it is taken
from the diazotrophs rather than added. A simpler uptake reformulation inside the 32-state
build **may** buy the same ×2.3 — untested here — and would face the same compositional
trade. **Recommendation stated, not taken: shelve the build as spec §7 provides for, and
record the measured result
— the August deficit is an uptake-flux problem, not a storage problem; it is fought out
between CYN and the fixers over one nitrogen pool; and 0.5 of 2.3 mg C/L says the remaining
4.6× is still elsewhere.**

---

## 39. The autumn collapse is not a nutrient problem: four falsifications
(2026-09-03)

**The question.** After §37/§38 closed the August *Planktothrix* exclusion on the
acquisition side, the named remainder was the autumn community — the warm-water diatom
guild and the autumn greens. The intended work was to design that guild. It was
re-measured first, and the measurement said not to build it.

**Where the model actually fails now.** Scored against the obs-matched monthly climatology
(current operational config, T7-verified full-record baseline `OUT_STD_MAIN`), August and
September are no longer the problem — CHLA ratios 0.92 and 0.98. The collapse is
**October–November**:

| month | CHLA obs / model | ratio | PHYTO_TOT_C ratio | DIA_C ratio | CYN_C ratio |
|---|---|---|---|---|---|
| Aug | 50.8 / 47.0 | 0.92 | 0.72 | 0.00 | 0.09 |
| Sep | 50.2 / 49.2 | 0.98 | 0.75 | 0.01 | 0.17 |
| Oct | 46.4 / 24.7 | **0.53** | 0.68 | 0.03 | 0.23 |
| Nov | 24.0 / 3.1 | **0.13** | **0.16** | 0.24 | 0.26 |

**The resource signature pointed at nitrogen — and was wrong.** In the same window silica
sits at 2.14× observed and phosphate at 5.6×, while DIN runs 4–5× *under* (Oct model 0.028
vs obs 0.113; Nov 0.101 vs 0.493). Two nutrients accumulating while one is scarce is the
textbook picture of a niche closed by that one nutrient. Four candidate mechanisms were
tested; **all four are refuted**, and together they close the resource side entirely.

1. **The prescribed benthic denitrification sink — refuted by bound.** It is summer-peaked
   (−0.040 gN/m²/d in August, −0.013 in October); over a ~3.8 m column that October value is
   ~0.10 mg N/L per month, *larger than the entire 0.068 mg/L deficit*. A ladder at ×1 / ×0.5
   / ×0 moves October NO3 monotonically (0.0059 → 0.0079 → 0.0115) and moves the biology not
   at all: October CHLA ratio 0.53 → 0.54 → 0.54, November 0.13 at every rung. Even deleted
   entirely the sink recovers a small fraction of the gap and buys no biomass.
2. **Water-column denitrification — refuted by arithmetic.** `K_MIN_DOC_NO3N_20 = 2.98`
   looks alarming beside its oxygen counterpart at 0.010, but the no-advanced-redox limiter
   is `(NO3/(NO3+1.0))·(K_HS_DOXY_RED_INHB/(DO+K_HS_DOXY_RED_INHB))`; at 10 mg/L oxygen and
   `K_HS_DOXY_RED_INHB = 0.10` that is ~6e-5 of the rate constant. The pathway is off.
3. **Nitrogen immobilised in organic pools — refuted by arithmetic, against my own
   framing.** The partition looked damning (box 7, August: DIN 0.005 vs DON 0.540 and PON
   0.432 mg N/L — DIN is 0.5 % of the pool, and TN is over-predicted by +0.56 while DIN is
   4–5× under). But the DON base rate is `K_MIN_DON_DOXY_20 = 0.100`/d at 20 °C, θ = 1.08;
   at 10 °C that is 0.046/d on a 0.59 mg N/L pool = **0.027 mg N/L/d, ~0.85 mg N/L over
   October — ten times the 0.085 mg/L gap**, and the standing DIN turns over in 2.1 days.
   Supply is not short; low DIN is a *symptom* of fast recycling. A probe lowering the
   phytoplankton-driven mineralization threshold (`K_MIN_PHYT_AMIN_DON` 4.00 → 1.50, chosen
   because the model's PHYT_TOT_C peaks at 3.87 while observations reach 5.35) returned an
   exact null — and was **under-powered by construction**: the accelerator adds
   `FAC_PHYT_AMIN_DON·(PHYT_TOT_C − threshold)` = 0.008 × 0.5 ≈ 0.004/d against a 0.100/d
   base, a 4 % change. Check a term's coefficient before spending a run on its threshold.
4. **Riverine nitrate erased by the strait exchange — mechanism real, effect nil.** The
   forcing is correct: the river boundaries carry NO3 0.363 (Oct) and 0.602 (Nov) mg/L,
   matching observations, and box 24 receives it at full strength (Nov 0.595). But the
   strait exchange runs **78,565 m³/s in / 79,246 out** — a correct net of −681 m³/s wrapped
   around a gross exchange ~130× the river discharge, enough to turn the ~6 km³ lagoon over
   daily — and every interior box then sits at the *Baltic boundary's* value: model Nov
   0.117–0.173 vs boundary 0.120, against an observed lagoon-wide 0.452. The physical
   objection is real (water returning through the strait is mostly lagoon water that just
   left, so it should not carry pristine-Baltic concentrations), so autumn boundary NO3 was
   raised ×3.75 to lagoon-like values. **Interior nitrate responded exactly as predicted —
   October 0.0059 → 0.0235, ratio 0.08 → 0.32 — and the biology did not move**: CHLA Oct
   24.69 → 24.96, Nov 3.12 → 3.21; PHYTO_TOT Nov 0.215 → 0.220; DIA_C, CYN_C, Si and PO4 all
   unchanged.

**The conclusion.** In October the model has silica at 2.1×, phosphate at 5.6×, and — when
supplied — nitrate at 4×, and still grows nothing. **The autumn niche is not closed by
nutrients.** §38 closed nitrogen *acquisition*; §39 closes nitrogen *supply*. What remains
for the October–November collapse is the growth-versus-loss balance: light in a kd ≈ 3
column at 8-hour November days, and the loss terms behind an 8× biomass drop between
October and November (net ~0.07/d). That is the *Planktothrix* low-loss story §35/§36 began
and did not finish — a shade-adapted, grazing-resistant, low-mortality guild that
observations carry at 0.343 mg C/L in November while the model holds 0.091 — **not a new
guild to be added**. The next probe family is the autumn light response and the loss terms,
not another consumer and not another nutrient.

**Reusable, and cheaply learned.**
- ⭐ **Two accumulating nutrients plus one scarce one does not prove the scarce one is
  limiting.** Here the scarce pool was simply the fast-turnover one; supplying it 4× changed
  nothing. Test a limitation claim by *supplying the resource*, not by reading its
  concentration.
- ⭐ **The driver file is parsed with a list-directed Fortran `read`, in which `/`
  terminates the input list.** An unquoted absolute path silently reads back blank and the
  run dies on an empty filename. **Quote every path** in a scratch driver — the working
  idiom is `"/tmp/.../INPUTS_X/"`.
- ⚠ Full-record runs emit a multi-line negative-concentration diagnostic (`ALERT: NEGATIVE
  CONC AFTER UPDATE`, `OLD_MASS=`, `VOLUME=`, `CONC=`) at millions of blocks per run —
  **5–10 GB of log each**. ZOO_N/ZOO_P (states 7/8) are clamped continuously from TIME=0 in
  *production* runs, not just probes (baseline-equivalent runs: 7.8M and 15.3M alerts); that
  is an open finding of its own. Filter on that block's own vocabulary at *analysis* time,
  and note that a capture-time filter here both violated the §29 lesson and failed to match.

**Nothing adopted; the live configuration is untouched.** All four probes ran on scratch
copies under `/tmp/varn_ab/autumn/` (`INPUTS_S050`, `INPUTS_S000`, `INPUTS_MIN15`,
`INPUTS_BND` with their `OUT_*` sets retained for re-scoring).

---

## 40. Light and losses: the autumn deficit is three different failures
(2026-09-03)

**The question.** §39 closed the resource side — in October the model has silica at 2.1×,
phosphate at 5.6×, and nitrate at 4× when supplied, and still grows nothing. That leaves
growth-versus-loss. Probing it decomposed the "autumn guild deficit" into **three separate
failures with three different binders**, only one of which is fixable by a constant.

**The temperature decomposition.** Water temperature (forcing `TEMP_TS.txt`) is Aug 18.5,
Sep 16.6, Oct 12.8, **Nov 9.2 °C**. Against the three non-fixing guilds' CTMI cardinals:

| guild | T_min | T_opt | T_max | KG | Oct CTMI (µ/d) | Nov CTMI (µ/d) |
|---|---|---|---|---|---|---|
| DIA | −2.0 | 10.0 | 21.0 | 8.10 | 0.935 (**7.58**) | 0.995 (**8.06**) |
| CYN | **5.0** | 26.0 | 34.0 | 2.00 | 0.262 (0.52) | **0.083** (0.17) |
| OPA | **10.0** | 17.0 | 23.0 | 2.58 | 0.751 (1.94) | **0.000** |

Two of the three guilds have autumn growth shut by a `T_min` floor, and the diatoms — the
largest absolute autumn deficit — have near-perfect temperature and grow anyway.

### 40.1 Diatoms: a light wall, short by a factor of two

October diatoms have every term but one in their favour. Assembling the model's own
multiplicative factors at October conditions (T 12.8 °C, DIN 0.0281, PO4 0.0206, Si 1.18,
`KHS_DIN_DIA` 0.010, `KHS_DIP_DIA` 0.0084, `KHS_DSi_DIA` 0.013):

| term | value |
|---|---|
| CTMI | 0.935 |
| LIM_Si | 0.989 |
| LIM_N | 0.738 |
| LIM_P | 0.709 |
| **LIM_LIGHT** | **≈0.050** |

`µ = 8.10 × Π = 0.196/d`. Losses are settling `v/H` = 0.093/3.8 = 0.024, mortality
0.104 (`KD_DIA_20` 0.12 at 12.8 °C), respiration 0.038 — **0.166/d total**, leaving a net of
**+0.030/d**. Rebuilding the summer-excluded population (0.0235) to the observed 0.670
across the ~60-day autumn window requires **+0.056/d — very nearly twice the achievable
net.** Light alone accounts for the shortfall: at LIM_LIGHT = 1 the same configuration would
run at 3.92/d. The depth-averaged Steele ceiling is `e/(ke·H)` = 2.718/(3.18×3.8) ≈ 0.225,
and October's realised value is a fifth of that.

**This is the documented light wall (§17/§22), reappearing for a guild that has no exit from
it.** Cyanobacteria escaped by buoyancy — the positional ratchet of §20 concentrates them
where the light is. Diatoms do not float. So the autumn diatom target is either unreachable
for *any* pelagic-growth formulation in kd ≈ 3 water, or those autumn diatoms are largely
**benthic and resuspended**, which would make it the same observation-mapping question §35
settled for *Mougeotia* rather than a growth defect at all. **Sizing the resuspended/benthic
share of the autumn diatom observations is the next measurement — on the observation side,
not the model side.**

### 40.2 CYN: the minimum growth temperature is too high — the one positive result

`CYN_OPT_TEMP_LR = 5.0 °C` leaves the guild at **CTMI 0.083** in a 9.2 °C November: growth
0.17/d against comparable losses. Lowering it to **2.0 °C** (full record, single constant):

| Nov | baseline | T_min 2.0 |
|---|---|---|
| CYN_C | 0.0905 (ratio 0.26) | **0.1457 (0.42)** |
| CHLA | 3.12 (0.13) | **3.76 (0.16)** |
| PHYTO_TOT_C | 0.215 (0.16) | 0.266 (0.20) |
| Oct CYN_C | 0.2458 (0.23) | 0.2903 (0.27) |

**at no cost anywhere**: annual CHLA 27.97 vs 27.98, August CHLA ratio 0.92 unchanged, PO4
unchanged (Oct 5.55× vs 5.57×, Nov 3.18× vs 3.26×). This is the §9 pattern a second time — a
never-calibrated cardinal constant closing a season for a taxon that does not obey it.

**Literature.** §35 established the summer guild is 65–70 % *Planktothrix agardhii*, and
*P. agardhii* is the canonical cold-persistent shallow-lake cyanobacterium: its annual cycle
restarts in March **from overwintered filaments** (Poulíčková et al. 2004,
doi:10.1002/iroh.200310716); it forms **perennial** blooms in Western Polish lakes at our
latitude, sampled through winter (Mankiewicz-Boczek et al. 2011, doi:10.1002/tox.20524); and
it retains relatively more growth at low temperature than competing bloom-formers (Davis &
Walsby 2002, doi:10.1046/j.1469-8137.2002.00495.x). ⚠ **The evidence is qualitative** —
overwintering, perenniality, relative cold performance — not a measured T_min, so 2.0 °C is
*defensible*, not derived. Honest size: November chlorophyll moves 0.13 → 0.16 of observed.
It explains a slice of the collapse, not the collapse.

### 40.3 OPA and grazing: two more refutations

- **OPA's temperature floor is irrelevant.** `OPA_OPT_TEMP_LR = 10.0` makes November growth
  *exactly zero* (water 9.2 °C), which looks decisive. Lowering it to 4.0 moves OPA from
  0.0015 to 0.0028 mg C/L — ratio 0.01, still extinct. **The floor was a second lock on an
  already-locked door: OPA is competitively excluded (§24–27), independently re-confirmed.**
- **Grazing is not the binder, and the hypothesis that raised it was mine and wrong.** The
  §36 W6 adoption set `PREF_ZOO_CYN` 0.10 → 0.03, which leaves diatoms (0.26) and OPA (0.37)
  carrying the grazing — suggesting W6 concentrated predation onto exactly the two missing
  guilds. Zeroing zooplankton growth entirely (`KG_ZOO_OPT_TEMP` 0.6 → 0.0; ZOO_C collapses
  to 0.008) moves October diatoms 0.0235 → 0.0242 and OPA not at all, and makes total
  phytoplankton **worse** (Oct 1.97 → 1.84, CHLA 24.69 → 23.03) — grazer-mediated recycling
  is worth more than the grazing pressure costs. W6 is exonerated.

### 40.4 Where this leaves the autumn

**Three failures, three different answers.** CYN is partly a cardinal-constant artifact and
is fixable now. OPA is competitive exclusion, already a documented structural limit — no
constant reaches it. DIA is the light wall with no buoyancy exit, and the next move there is
to size the benthic/resuspended share of the observations rather than to grow more
phytoplankton. **None of the three is "a missing warm-water guild", which is what this arc
set out to build** (§28/§39). Combined with §38 (acquisition) and §39 (supply), the autumn
residual is now attributed rather than open.

### 40.5 Adoption: `CYN_OPT_TEMP_LR` 5.0 → 2.0 is operational (2026-09-03, user decision)

Adopted. The full-record scorecard (n-weighted pooled RMSE against the harmonized
observations, scored with the live C:Chl) improves or holds **every** headline metric:

| variable | n | baseline | adopted | |
|---|---|---|---|---|
| **CHLA** | 760 | 24.05451 | **23.96441** | −0.0901, study best |
| **PO4** | 3064 | 0.01701 | **0.01684** | −0.00017, study best |
| TN | 3225 | 0.86384 | 0.86056 | −0.0033 |
| TP | 3060 | 0.04681 | 0.04594 | −0.0009 |
| CYN_C | 317 | 1.75455 | 1.74959 | −0.0050 |
| DIA_C | 317 | 0.71612 | 0.71299 | −0.0031 |
| PHYTO_TOT_C | 317 | 2.50160 | 2.50090 | −0.0007 |
| NH4 / DO | 3066 / 3002 | 0.05223 / 7.92394 | unchanged | |
| NO3 / Si | 3065 / 3224 | 0.45777 / 0.85858 | +0.00037 / +0.00232 | the only debits |

Verification: the canonical run (`OUTPUTS_CL29/`, day 4016, 32 files) reproduces the probe
run's scorecard **exactly** — 0.0e+00 difference on all eleven variables.

Seasonal r **+0.67** (from +0.68) and the climatological peak month stays September — the
one debit, and it is within the noise of the §36 peak-month margin discussion. The autumn
gain that motivated it: November CYN 0.091 → 0.146 mg C/L (ratio 0.26 → 0.42), November
CHLA 3.12 → 3.76 µg/L, October CYN +18 %.

Live config `INPUTS_CL29/WCONST_04.txt` constant 30 changed (one line; canonical run
refreshed into `OUTPUTS_CL29/`), mirrored to the data repo. **What it does not do:** close
the autumn collapse — November chlorophyll is still 0.16 of observed. §40.1's diatom light
wall and §40.3's OPA exclusion are untouched by it, and remain the standing structural
limits.

**Reusable.**
- ⭐ **Compute the growth budget before probing it.** The diatom answer (net +0.030/d against
  the +0.056/d needed) came from arithmetic on the model's own terms in seconds, and framed
  every probe that followed. Break-even is not the test — *rebuild rate within the available
  window* is.
- ⭐ **A closed door can have two locks.** OPA's `T_min` genuinely zeroes November growth and
  removing it changes nothing, because exclusion binds first. Test the lock you think is
  binding by *removing* it, not by observing that it is closed.
- Probes: `/tmp/varn_ab/autumn/INPUTS_{PCYN,POPA,PGRZ}` + `OUT_*`; baseline
  `/tmp/varn_ab/t7/OUT_STD_MAIN`.

---

## 41. The diatom observation audit: the artifact is small, and the guild is named
(2026-09-03)

**The question.** §40.1 left the autumn diatom deficit with two branches: either the target
is unreachable for any pelagic-growth formulation in kd ≈ 3 water, or those diatoms are
largely **benthic and resuspended** — an observation-mapping artifact like the *Mougeotia*
share of OPA (§35), which no growth formulation should be asked to reproduce. That is an
observation-side question, so it was measured on the observation side.

**Method.** The AAA open-data monitoring NDJSON (`JTD/monitoringasjsonl`, the same
species-level source §35 used) streamed for `Fitoplanktonas` rows at the eight CL29-mapped
LTK stations, 2016–2022, filtered to the two diatom orders present (`Eupodiscales`,
`Bacillariales`): **3,579 rows, 790.6 wet-biomass units.** Genera were split into
holoplanktonic versus periphytic/epipelic/epiphytic (tychoplankton) by habit; 2.5 % of
biomass stayed unclassified. Tool: `/tmp/varn_ab/diatom_habit.py`.

### 41.1 The artifact is small — the light wall is a real model failure

| month | benthic share (headline) | upper bound (*Fragilaria* counted benthic) |
|---|---|---|
| Aug | 7.6 % | 9.5 % |
| **Oct** | **4.5 %** | 15.0 % |
| **Nov** | **9.7 %** | 34.5 % |

Annual peak is May (26.2 %); every other month is single-digit to mid-teens. **This is
nothing like the *Mougeotia* case** (30.5 % of OPA annually, 45 % in August). The autumn
diatom observations are ~90 % genuinely planktonic, and October stays ≤15 % under the most
pessimistic classification defensible. ⚠ *Fragilaria* is the swing taxon — several species
in it are tychoplanktonic, and moving the whole genus takes November from 9.7 % to 34.5 %;
October is robust either way. **⇒ The benthic branch of §40.1 is refuted. The ~0.64 mg C/L
of October diatoms is pelagic biomass the model must actually grow, and §40.1's light wall
stands as a real model failure rather than a mapping artifact.**

### 41.2 What the audit found instead: two guilds, three months apart

The audit was sent to size an artifact and returned a structure. Autumn (Oct–Nov)
planktonic diatom biomass is **44.5 % *Actinocyclus normanii*** — three forms, all in the
top three species (f. *subsalsus* 17.5 %, f. *normanii* 13.6 %, *normanii* 13.2 %) — then
*Stephanodiscus* 20.0 % (*hantzschii*, *rotula*), *Fragilaria* 19.1 % (*heidenii*,
*capucina*, *crotonensis*), *Aulacoseira islandica* 6.2 %, ***Skeletonema subsalsum*** 6.2 %.

Monthly biomass by genus splits the observed diatoms cleanly in two:

| genus | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | wtd. mean month |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Asterionella | 3.6 | 8.9 | 11.6 | 18.2 | 1.5 | 0.1 | 0.2 | 0.1 | 0.3 | 1.6 | **4.4** |
| Stephanodiscus | 9.0 | 28.3 | 52.7 | **102.7** | 26.7 | 12.5 | 3.4 | 2.7 | 12.2 | 6.2 | **5.1** |
| Aulacoseira | 3.1 | 12.6 | 23.2 | 1.8 | 1.3 | 2.5 | 28.5 | 15.3 | 2.8 | 2.9 | **6.3** |
| Fragilaria | 3.1 | 4.9 | 7.2 | 3.0 | 2.0 | 4.3 | 1.8 | 1.8 | 6.3 | 11.2 | 7.1 |
| *Skeletonema* | 0.0 | 0.1 | 3.3 | 1.6 | 0.1 | 2.2 | 4.5 | 9.3 | 5.4 | 0.3 | **8.0** |
| ***Actinocyclus*** | 1.2 | 2.1 | 3.7 | 10.7 | 13.2 | 13.6 | **41.6** | **42.9** | 25.5 | 15.4 | **8.2** |

A **cold/spring guild** (*Asterionella*, *Stephanodiscus*, *Aulacoseira*; weighted mean
month 4.4–6.3, peaking May) and a **warm/late guild** (*Skeletonema subsalsum*,
*Actinocyclus normanii*; 8.0–8.2, peaking Aug–Sep and carrying October). **The model has one
envelope — `DIA_OPT_TEMP_LR` −2, `DIA_OPT_TEMP_UR` 10, `KAPPA_DIA_OVER_OPT_TEMP` 21 — which
is the cold guild exactly.** That single fact explains both halves of the diatom error at
once: February 3.04× and the spring over-prediction (the cold guild, over-grown), and
July–October 0.00–0.08 (the warm guild, structurally absent). It also explains the autumn
starting point — §40.1's diatoms cannot rebuild in autumn because the summer collapse that
empties them is the *absence of the guild that owns the summer*, not a rate problem.

### 41.3 Correction to §40, and what a second guild would take

§40 concluded "not a new guild". **That holds for CYN** (a cardinal constant fixed it, §40.5)
**and for OPA** (competitive exclusion, §40.3) — **but not for the diatoms**, and the
correction is on the evidence, not on preference. BACKLOG P2 named the warm guild as
"*Actinocyclus*/*Skeletonema subsalsum*, no clean published cardinals"; the audit now
supplies the organism, its share (44.5 % of autumn planktonic diatom biomass), and its
phenology (peak Aug–Sep, persisting through November). It is no longer a speculative
addition — it is a named, quantified, taxonomically-evidenced gap.

**Scoping, not a proposal.** A second diatom guild means a new state variable and its full
routing (the §38 VARN work is the cost model for that: byte-identity discipline, a
transported state, every downstream reader). Before any of that, the cheap prior test is
whether the *existing* envelope can be re-pointed: the cold guild is over-predicted in
spring (Feb 3.04×) and absent in summer, so **widening/warming the single envelope trades
one error for the other** — which §11's wide-envelope experiment already measured, and it
destroyed June. That is the argument for two guilds rather than one re-tuned one, and it is
now backed by the observation that the two assemblages are three months apart in the data.
⚠ Also unresolved: §40.1's light arithmetic applies to whatever guild occupies October, so a
warm guild must be shown to clear the same +0.056/d rebuild bar — *Actinocyclus normanii* is
a large brackish centric of turbid systems, so its light traits are the thing to check first.

**Reusable.** ⭐ **Send an observation audit to size an artifact and let it answer a
different question.** §35 sized *Mougeotia* and named *Planktothrix*; §41 sized the diatom
tychoplankton — small — and named the missing guild. In both cases the audit's *by-product*
was worth more than its commission. ⭐ Report the swing taxon: one genus (*Fragilaria*) moves
the November headline 9.7 % → 34.5 %, and a single number would have hidden that.

---

## 42. The light trait is real, the model can only express it as C:Chl — and the
observations say C:Chl is wrong in autumn (2026-09-03)

**The question.** §41.3 named the warm guild (*Actinocyclus normanii*, 44.5 % of autumn
planktonic diatom biomass) and set one precondition before any build: a warm guild still has
to clear §40.1's **+0.056/d rebuild bar**, so *its light traits are the thing to check
first*, not its cardinals. Checked — and the answer redirects the frontier.

### 42.1 The trait exists in the literature

*A. normanii* was studied across four German North Sea estuaries with accompanying laboratory
work (Rehbehn, Schuchardt & Schirmer 1993, doi:10.1007/bf02334784): it "is **well adapted to
strongly changing light situations** and thus adapted to estuaries with **high vertical
turbulent mixing and low values of Zeu/Zmix**", and "the downstream limit of the habitat of
*A. normanii* is mainly determined by **light limitation** rather than by hyperosmotic
stress." That is CL29's light climate stated in the abstract of a paper about another system:
kd ≈ 3, a fully mixed 3.8 m column, Zeu/Zmix ≪ 1. Supporting: it is a cosmopolitan
alkalibiontic/halophilous indicator of eutrophic waters whose Great Lakes populations peak at
**≈20 °C** (Vidaković et al. 2016, doi:10.17110/studbot.2016.47.2.201) — warm cardinals,
against the model's single T_opt 10 envelope.

### 42.2 The model cannot express it through growth rate — the ceiling is KG-independent

`LIM_LIGHT` uses a depth-averaged Steele with an **adaptive** saturation intensity
(`aquabc_II_pelagic_auxillary.f90`): `I_s = GITMAX · CCHL_RATIO · e / (0.083·PHIMX·XKC)`.
Because `I_s ∝ GITMAX`, a faster guild raises its own saturation intensity in lockstep. At
October conditions (ke 3.18, H 3.8, I_surf ≈ 54 langley/d PAR, nutrient product 0.517,
losses 0.166/d):

| KG | I_s | LIM_LIGHT | µ | net |
|---|---|---|---|---|
| 2.0 | 56 | 0.139 | 0.134 | −0.032 |
| **8.10 (current)** | 228 | 0.047 | 0.186 | **+0.020** |
| 40.0 | 1127 | 0.011 | 0.204 | +0.038 |
| **KG → ∞** | — | — | **0.209** | **+0.043** |

**No growth constant at any value clears the +0.056/d bar** — the limit is 0.225·(I_surf/k)·N
with `k = CCHL·e/(0.083·PHIMX·XKC)`, independent of KG. This is §17's "caps production at ANY
KG" wall, now derived analytically for the autumn diatoms. The *only* term in the formulation
that moves it is `CCHL_RATIO`, since `I_s ∝ CCHL` too: **C:Chl 53 → +0.020 (short), 40 →
+0.071 (clears), 30 → +0.135, 25 → +0.182.** And low C:Chl *is* the shade-adaptation trait —
more pigment per carbon — i.e. the literature trait of §42.1 is expressible in this model
**only** as C:Chl.

### 42.3 The observations say the model's autumn C:Chl is 25–31 % too high

Measured in-situ C:Chl (paired same-box/same-date observed phytoplankton carbon ÷ observed
chlorophyll, 2012–2022) runs **Feb 24.3 → Mar 27.6 → May 62.8 → Aug 102.1 → Sep 90.7 → Oct
50.8 → Nov 52.3**. Applying the model's *fixed* ratios (DIA/OPA 53, CYN/FIX/NOST 78) to the
**observed** monthly composition and comparing:

| month | model-implied | observed | model/obs |
|---|---|---|---|
| Mar | 54.6 | 27.6 | **1.98** |
| May | 58.1 | 62.8 | 0.92 |
| Aug | 69.2 | 102.1 | **0.68** |
| Sep | 70.7 | 90.7 | 0.78 |
| **Oct** | 66.7 | **50.8** | **1.31** |
| **Nov** | 65.5 | **52.3** | **1.25** |

The fixed ratio is wrong in **both directions seasonally** — too little pigment in late
winter and autumn, too much in summer — which is textbook photoacclimation, and no single
value can span an observed range of 24 → 102. Critically for §40.1: in October the model
assumes **31 % too little chlorophyll per carbon**, so its `I_s` is ~31 % too high and
`LIM_LIGHT` correspondingly suppressed. Correcting October C:Chl to the observed level
(≈40 for the diatom guild) is exactly the +0.071/d case in §42.2's table — **it clears the
rebuild bar on its own.**

### 42.4 Re-scoping: photoacclimation comes before a second guild

**The autumn diatom deficit and the photoacclimative-C:Chl backlog item are the same
problem.** §22 found C:Chl as the fifth compensation channel and fixed it at measured annual
values, listing "photoacclimative C:Chl (structure — never a knob)" as the honest closure;
§41 named the missing guild; §42 shows the guild's defining trait is only expressible as
C:Chl and that the fixed C:Chl is already measurably wrong in the season concerned. So the
order of work inverts:

1. **Photoacclimative C:Chl (structure).** Cheaper than a guild — no new state variable, no
   routing, no VARN-class deployment change — already a named BACKLOG P2 item, driven by a
   measurement that now exists (§42.3), and it corrects every group in every season rather
   than one guild in one season. It is also a precondition: without it, a warm guild inherits
   the same suppressed `LIM_LIGHT` and fails the rebuild bar for the same reason.
2. **The warm diatom guild** (§41.2's *Actinocyclus*/*Skeletonema*) — still the right
   structural answer to the two-guild observation, but it should be built *after* the light
   climate can express its defining trait, or its test is confounded.

⚠ **The §22 trap, restated.** C:Chl must never be a calibration knob: handed to the
objective it fills the chlorophyll gap with pigment instead of biomass (§22 measured Feb 54
vs obs 10, r collapsing to +0.47). §42.3 is *not* that — it is a measured seasonal ratio
from paired observations, not a fitted one — but it sits close enough to the trap that an
implementation must be **driven by the measurement and validated on biomass (group carbon),
never on chlorophyll**, or it reproduces the exact failure §22 caught.

**Reusable.** ⭐⭐ **Check whether the model can express a trait before sourcing its value.**
The obvious next step after naming *Actinocyclus* was to hunt cardinal temperatures and a
growth rate; the formulation says growth rate is inert here (KG-independent ceiling) and the
whole trait lives in one constant nobody would have looked at. ⭐ **An adaptive parameter
couples what you tune to what limits it** — `I_s ∝ GITMAX` means raising growth raises the
light requirement, which is why the wall held against every previous growth-side probe.

---

## 43. C:Chl measured independently: the light chain is confirmed, the correction is
not adoptable (2026-09-03)

**The question.** §42.4 proposed photoacclimative C:Chl as the autumn lever and ranked it
ahead of the warm guild. The design gate killed that proposal before a line was written, and
the measurement that replaced it confirms §40–42's mechanism while blocking its use.

### 43.1 Photoacclimation refuted at the design gate

Fitting a Cloern-form relation (`Chl:C = a + b·e^{cT}·e^{−dE}·μ_nut`) to the measured
monthly C:Chl against the model's own drivers:

| predictor | R² |
|---|---|
| temperature alone | **0.843** |
| light (depth-averaged Emix) alone | **0.023** |
| T + light | 0.844 |
| T + light + nutrients | 0.847 |
| Cloern form, fitted | 0.832 (with an **inverted** temperature coefficient) |

Light explains nothing and enters with the wrong sign, because **August is simultaneously
the dimmest month** (self-shading drives Emix to 9.6) **and the most pigment-poor** (C:Chl
102) — the opposite of photoacclimation. Worse for the purpose, the fitted law predicts
**October at 65.6 against an observed 50.8**, its single worst residual: it would not have
delivered the correction it was scoped to deliver. **Fixed per-group ratios fit better than
any seasonal law (R² = 0.903 vs 0.84): the observed 24 → 102 swing is composition — diatom
winter, cyanobacteria summer — not acclimation.** No structure was built; the gate paid for
itself.

### 43.2 The independent measurement: diatom C:Chl is ≈34, not 53

The monitoring dump carries a separate `ChlorofilasA` dataset that joins to the
species-level phytoplankton **by sampling event** (`reg_nr`), giving **311 paired events** at
the CL29 stations — measured chlorophyll against species biomass converted with the
project's own empirical C:wet ratios (DIA 0.065, CYN 0.18, FIX 0.16, OPA 0.15, i.e. the same
conversion the model's group carbon uses, so the comparison is like-for-like). Three routes:

| route | n | DIA C:Chl |
|---|---|---|
| direct read, samples ≥70 % diatom | 82 | 36.1 |
| direct read, ≥80 % / ≥90 % | 48 / 12 | 35.5 / 32.1 |
| sample-level NNLS + bootstrap | 311 | **34.2 [29.5, 39.1]** |
| monthly-mean inversion (§42-era) | 10 | 28–39 |

**Diatom C:Chl ≈ 34; the model's 53 is ~1.55× too high.** Other groups are weaker: CYN reads
121–208 against 78 (direction clear, magnitude poorly constrained), OPA ≈73 on 5 samples,
FIX 55 [37, 96]. Tool: `tools/measure_group_cchl.py`.

⚠ **Which guild the number belongs to.** Diatoms are dominant only in winter/spring samples
(n = 34, median 32.4, IQR 19.9–44.2); in summer/autumn they never reach 70 % of biomass
(n = 3). **So 34 is the COLD guild's ratio. The warm guild's pigment ratio is not measurable
from this record at all** — §41.2's guild cannot be parameterised on the pigment axis here.

### 43.3 The probe: prediction registered, mechanism confirmed, adoption blocked

Before running, the prediction was written down: at C:Chl 34 the October diatom `I_s` falls
228 → 146, `LIM_LIGHT` rises 0.054 → 0.078, and net growth goes **+0.044 → +0.138/d** —
across §40.1's +0.056/d rebuild bar. Full record, `DIA_C_TO_CHLA` 53 → 34, nothing else:

| | baseline | C:Chl 34 |
|---|---|---|
| **Nov DIA_C** (obs 0.307) | 0.077 (ratio 0.24) | **0.314 (ratio 1.03)** |
| Nov PHYTO_TOT_C | 0.215 (0.16) | 0.503 (0.37) |
| CHLA RMSE | 23.964 | **23.642** |
| PO4 RMSE | 0.01684 | **0.01532** (−9 %) |
| TN / TP | 0.86056 / 0.04594 | 0.85514 / 0.04543 |
| **seasonal r** | **+0.67** | **+0.51** |
| **autumn:spring** (obs 2.06) | **1.97** | **1.29** |
| **Feb CHLA** (obs 10.2) | 15.0 | **34.5** |
| Oct DIA_C ratio | 0.03 | 0.05 |

**The mechanism is confirmed exactly as predicted** — November diatoms land on the observed
value (ratio 1.03) and every aggregate metric improves, both headline RMSEs to nominal
study bests. **It is still not adoptable**, for three reasons:

1. **It amplifies an unfixed error.** February diatom carbon goes 3.04× → 4.17× observed and
   February chlorophyll to 3.4×; seasonal r falls 0.16 and autumn:spring moves *away* from
   observed. On this study's own repeatedly-demonstrated standard (§§7, 19, 22) phase beats
   aggregate RMSE, and "both headline RMSEs improved" is precisely the signature that has
   concealed a phenology regression before.
2. **It does not fix the target month.** October is unmoved (0.03 → 0.05): the population
   still cannot rebuild inside the window from the summer collapse — §41.2's point stands.
3. **The value belongs to the wrong guild for the job** (§43.2): it is the cold guild's
   ratio, and the cold guild is what February over-grows.

**The blocker is now named.** The model grows ~3× too many winter diatoms, and until that is
resolved a *correct* C:Chl cannot be used — accurate physics applied on top of an unfixed
error makes the error worse. The February over-prediction, not the autumn deficit, is the
next thing to explain.

**Reusable.**
- ⭐⭐ **The design gate is worth its cost.** The approved work (photoacclimative structure)
  was refuted by a 30-minute fit before any code existed — light explains 2 % of the
  variance here and the fitted law missed the very month it was for.
- ⭐ **Register the prediction before the run.** "I_s 228→146, net +0.044→+0.138/d, so autumn
  DIA_C should rise" was written down first; the run then landing November at ratio 1.03
  is evidence, where the same numbers quoted afterwards would have been a story.
- ⚠ **Scoring trap:** `/tmp/monthly_residuals.py` loads C:Chl from the **live**
  `INPUTS_CL29/WCONST_04.txt`, so a probe that changes C:Chl is scored with the wrong
  conversion unless the validator is pointed at the probe's own WCONST (`--wconst`). Carbon
  metrics are immune; chlorophyll is not. Caught here after the first scoring pass.
- ⭐ **A measurement can be right and still not adoptable.** Correct value, confirmed
  mechanism, improved aggregates — and it still fails, because it lands on top of a
  different unfixed error.

---

## 44. The February over-prediction is a winter light-climate gap: no ice, no day length
(2026-09-03)

**The question.** §43 named the blocker: the model grows ~3× too many winter diatoms, and
until that is fixed the *correct* diatom C:Chl (≈34, §43.2) cannot be used because accurate
physics applied on top of an unfixed error amplifies it. So: why is February over-grown?

**It is timing, not magnitude.** Observations *build* to a May peak; the model is already
flat-out in February:

| | Feb | Mar | Apr | May |
|---|---|---|---|---|
| obs DIA_C | 0.280 | 0.626 | 0.829 | 0.857 |
| model | 0.852 | 1.075 | 1.037 | 1.076 |
| ratio | **3.04** | 1.72 | 1.25 | **1.26** |

May is nearly right. The model starts its bloom about two months early, and its February net
growth is **+0.283/d** — a runaway checked only by nutrient draw-down and self-shading.
Nothing in the formulation stops it: temperature only half-limits (CTMI 0.462 at 1.8 °C, the
guild is cold-adapted by construction), nutrients are at their winter maximum, losses are
low (0.137/d, mortality scaling down with temperature), and — the counter-intuitive part —
**February's `LIM_LIGHT` (0.118) is twice October's (0.057) despite less surface light**,
because cold water lowers GITMAX, which lowers the adaptive `I_s`, which raises light
efficiency (the §42.2 coupling running in reverse).

### 44.1 The lagoon never freezes, and ice could not have stopped it anyway

**`INPUTS_CL29/ICE_COVER.txt` is a two-row, all-zero placeholder** — `0.0` at day 0, `0.0`
at day 4016, interpolated — so the modelled lagoon is ice-free for the entire 11-year
hindcast. The real Curonian Lagoon carries ice roughly 40–100 days in most winters.

**And the mechanism is wired to the wrong processes.** `ice_cover`
(`DRIVING_FUNCTIONS(:,10)`) is consumed at exactly three sites — `R_AERATION`
(`aquabc_II_pelagic_model.f90:1044`), `R_AMMONIA_VOLATIL` (`:1971`) and `CO2_ATM_EXHANGE`
(`:3311`) — **all gas exchange, none of them light or growth.** Even with a correct
ice-fraction series loaded, phytoplankton would receive full irradiance under a metre of ice.
This is the §12 placeholder-forcing class for the third time in this study, with an extra
twist: here the placeholder hides a mechanism that would not have worked if fed.

### 44.2 `FDAY` is read, bundled, and never used

The day-length driving function is read (`DRIVING_FUNCTIONS(:,4)`), allocated, and pointed
into the per-thread environment bundle — and appears in **no calculation anywhere**. Its
only occurrence in a formula is inside the dead `CUR_SMITH` routine, where it is hardcoded
to `1.0`. **The model gives each day's mean irradiance 24 hours a day.** Because the P–I
curve is concave, that over-states production, and it over-states it *most* where days are
shortest — which is exactly February.

### 44.3 The two gaps are differential in the way February needs

| net growth | February | May (must not break) |
|---|---|---|
| as-is | **+0.283/d** | +0.932/d |
| + day length (FDAY ≈ 0.29 / 0.60) | +0.128/d | — |
| + background extinction at the measured floor | +0.152/d | — |
| **+ both** | **+0.045/d** | **+0.472/d** |

February falls from runaway to near-balance — consistent with the observed 0.28 mg C/L
standing stock — while May's bloom survives at +0.472/d. Neither correction is a tuned
knob: FDAY is a *bug fix* (a driving function that is read and discarded), and the
background extinction is measured — this study's own campaign gives kd 3.18 mean / 2.92
median / range **2.26–5.72**, while the model's `kd = kdb + 0.4 + 0.02·CHLA` yields 2.20 in
February and **1.25 in November**, below anything ever measured in this lagoon.

### 44.4 Checked and cleared

Recorded so they are not re-litigated:
- **Solar units.** `SOLAR_RAD_TS.txt` is W/m² total solar (annual mean 124.5, matching ~110
  W/m² expected at 55 °N) while `LIM_LIGHT` documents langleys/day PAR. The conversion is
  ×2.07 (W/m² → langley/d) ×0.45 (PAR fraction) = **×0.93** — so passing the raw number is
  coincidentally within 7 % of correct. Not a bug; do not "fix" it without redoing the
  arithmetic.
- **River turbidity.** Suspended matter at the CL29 stations is flat seasonally (monthly
  medians 7.8–14.0 mg/L, no winter/spring spike, n = 481), so a seasonal
  river-turbidity term does not explain February. The extinction problem is the *constant*
  background, not its seasonality.
- **Growth rate.** February sits below light saturation (I/I_s = 0.45), where
  `µ → N·I_surf·(0.083·PHIMX·XKC)/(ke·H·CCHL)` and GITMAX cancels — so `KG_DIA` is
  irrelevant to the February bloom, the same algebra as §42.2.

### 44.5 What this unblocks, and what it would cost

The February blocker is now explained rather than merely located, which puts §43's measured
C:Chl back in reach: with the winter light climate corrected, the correction that fixed
November (ratio 0.24 → 1.03) would no longer amplify a February error that should not exist.
**The order is: fix the winter light climate, then re-test C:Chl 34, then revisit the warm
guild (§41.2).**

Cost, stated honestly: this is a build, not a constant change. Real ice-fraction forcing has
to be sourced (the placeholder carries no data), `ice_cover` has to be wired to light as
well as gas exchange, and `FDAY` has to be applied in `LIM_LIGHT` — each a code change with
byte-identity discipline and its own verification. ⚠ And one consequence must be faced up
front: raising the background extinction to the measured floor **darkens November too**
(ke 1.25 → ~2.25), which makes the already-deficient autumn *harder*, not easier. That is
the honest physics, and it strengthens rather than weakens §41.2's case that autumn needs
the warm guild rather than more light.

**Reusable.** ⭐⭐ **A mechanism can be present, fed a placeholder, AND wired to the wrong
process.** Ice exists here, is zero everywhere, and damps only gas exchange — three separate
failures stacked, any one of which alone would have looked like "the model has no ice".
⭐ **Check whether a driving function is consumed, not just read.** `FDAY` and `ice_cover`
were both read, allocated and bundled; only a grep for their use in *formulas* shows the
difference. ⭐ Coincidental unit agreement (×0.93) is a trap in both directions — it hides a
real mismatch and invites a "fix" that would introduce one.

---

## 45. Ice built: February fixed to 0.99×, seasonal r +0.74 — the study's best
(2026-09-03)

**The build.** §44 named February's blocker as a winter light-climate gap and ranked ice
first, because it is the only *winter-specific* lever (day length and background extinction
are near-uniform across months, §44.3/§45.4). Built and measured.

### 45.1 Data: the lagoon does freeze, and the series proves itself

`tools/fetch_ice_cover.py` pulls daily sea-ice area fraction (`siconc`) from the CMEMS Baltic
physics reanalysis (`cmems_mod_bal_phy_my_P1D-m`) over the lagoon interior (lon 20.95–21.30,
lat 55.00–55.65 — inside the Curonian Spit, so the mean is the lagoon's own ice, not the open
sea's). The lagoon is properly resolved: 368 wet cells of 507 in that strip.

**4,018 daily records, mean 64 ice days/year** (>10 % cover) — squarely inside the
literature's 40–100 for this lagoon. Per winter: 2012 **90**, 2013 **100**, 2014 68, 2015 55,
2016 41, 2017 55, 2018 **101**, 2019 48, **2020 zero**, 2021 88, 2022 57. ⭐ **The 2020 zero
is the validation**: 2019/20 was the record-mild Baltic winter, and the series reproduces
that anomaly without being asked to — evidence it carries real inter-annual signal rather
than a climatology.

### 45.2 Code: ice attenuates light, and the wiring is proven inert

Ice now attenuates the light reaching the water as an areal blend of open and ice-covered
fractions, applied to `I_A` immediately after `ice_cover` is read
(`aquabc_II_pelagic_model.f90`):

```
I_eff = I · ((1 − f) + f·T) = I · (1 − f·(1 − T))       f = ice_cover, T = ICE_LIGHT_TRANS
```

`ICE_LIGHT_TRANS` is **model constant 324**, not a hidden literal, and it is added to **both**
parallel readers — the AQUABC name-based `para_get_value` path and the ESTAS
`INIT_PELAGIC_MODEL_CONSTANTS` index path that carried the §34 BETA bug. Both reads are
guarded (absent ⇒ 1.0 = no attenuation), and all eight constants files plus five declared
counts were updated with a **default of 1.00**, so every pre-existing setup is unchanged.

**The safety property that makes the result interpretable:** with the shipped all-zero
`ICE_COVER.txt`, the multiplier is exactly 1.0, so the code change *cannot* alter any
result. Verified end-to-end — a full-record run with the constant present at 1.0 is
**byte-identical** (`diff -r` clean, 32 files, day 4016) to the canonical adopted run. Every
difference below is therefore attributable to the ice data alone.

### 45.3 Result: February fixed, phase at a study best, ice-free months untouched

Prediction registered before the run (Feb light multiplier 0.62, growth turning negative
under ice, `DIA_C` falling from 0.852 toward obs 0.280, May–Nov untouched):

| | canonical | with real ice |
|---|---|---|
| **Feb DIA_C** (obs 0.280) | 0.810 (**2.89×**) | **0.278 (0.99×)** |
| Feb PHYTO_TOT_C (obs 0.404) | 0.866 (2.15×) | 0.335 (0.83×) |
| **seasonal r** | +0.67 | **+0.74 — best of the study** |
| CHLA RMSE | 23.9644 | **23.8300** |
| DIA_C RMSE | 0.71299 | **0.70154** |
| PHYTO_TOT_C RMSE | 2.50090 | **2.49703** |
| May / Aug / Oct / Nov | — | **unchanged** |
| autumn:spring (obs 2.06) | 1.97 | 2.25 |
| PO4 / TN / Si RMSE | 0.01684 / 0.8606 / 0.8609 | 0.01755 / 0.8729 / 0.8697 |

Every ice-free month is untouched — the correction is genuinely differential, which is
exactly what §44 said neither day length nor background extinction could deliver. Costs are
small and of the expected sign: suppressing the winter bloom leaves nutrients unconsumed
(PO4, TN, Si each slightly worse), and autumn:spring overshoots 2.06 where the baseline's
1.97 undershot it.

**Sensitivity — the result does not rest on the unmeasured constant.** A 7.5× range in
transmittance moves nothing:

| T | CHLA | DIA_C | seasonal r | Feb DIA_C (obs 0.280) |
|---|---|---|---|---|
| 0.02 | 23.8263 | 0.70055 | +0.74 | 0.270 (0.96×) |
| **0.05** | 23.8300 | 0.70154 | **+0.74** | **0.278 (0.99×)** |
| 0.15 | 23.8396 | 0.70312 | +0.73 | 0.309 (1.10×) |

⭐ **Why it is flat:** at ~40 % mean February ice cover the multiplier `1 − f(1−T)` is
dominated by the *fraction* iced, not by what the ice transmits. **The physics is carried by
the measured CMEMS series, not by the one number nobody has measured here** — the opposite of
§43's C:Chl, where the result hinged entirely on the value. That is the best available
outcome for a newly introduced constant.

### 45.4 What this unblocks

**§43's C:Chl correction is now back in reach, and the ordering §44.5 prescribed is
empirically justified.** With February biomass finally correct (0.99×), February chlorophyll
reads 3.18 against an observed 10.2 — the *pigment* error, exposed only once the biomass
error was removed. Observed February implies C:Chl ≈ 27.5, squarely on §43's measured winter
value of 32.4 (IQR 19.9–44.2) against the model's 53. The two corrections are complementary,
and the failed §43 probe (which fixed November but amplified February) is now explained: it
was correct physics landing on an unfixed error, exactly as §43.3 concluded.

Still open and unchanged by this: the autumn deficit (§40.1's light wall, §41.2's warm
guild), and the two near-uniform light-climate items — `FDAY`, still read and never used
(§44.2), and the background extinction below the measured kd floor. Neither is winter-
specific, so both remain separately-decided correctness fixes.

### 45.5 Adoption: ice is operational (2026-09-03, user decision)

Adopted. The live `INPUTS_CL29/` now carries the real `ICE_COVER.txt` (4,018 daily records
from the CMEMS Baltic reanalysis) and `ICE_LIGHT_TRANS = 0.05`; `OUTPUTS_CL29/` refreshed and
the data repo updated (`b856085`). Verification: the canonical run reproduces the probe scorecard **exactly** — 0.0e+00 difference on all ten variables. This is the study's **first adopted change that is code + forcing
rather than a constant** — the model gained a process it did not have, fed by measured data.

**Operational scorecard** (full record, n-weighted pooled RMSE, scored with the live C:Chl):

| | previous operational | **adopted (ice)** | |
|---|---|---|---|
| **seasonal r** | +0.67 | **+0.74** | **best of the study** (−0.70 at its start) |
| **Feb DIA_C** (obs 0.280) | 0.810 (2.89×) | **0.278 (0.99×)** | the §44 blocker, closed |
| CHLA RMSE | 23.9644 | **23.8300** | |
| DIA_C RMSE | 0.71299 | **0.70154** | |
| PHYTO_TOT_C RMSE | 2.50090 | **2.49703** | |
| autumn:spring (obs 2.06) | 1.97 | 2.25 | overshoots where it undershot |
| PO4 / TN / Si RMSE | 0.01684 / 0.8606 / 0.8609 | 0.01755 / 0.8729 / 0.8697 | the cost |
| peak month | Sep (obs Aug) | Sep | unchanged |

**What it does not do:** the autumn deficit is untouched (October DIA_C ratio 0.03,
November chlorophyll 0.16 of observed) — as designed, since there is no ice then. The
nutrient metrics pay a small, expected price for the un-consumed winter nutrients.

**Live state:** `INPUTS_CL29/ICE_COVER.txt` (real series), `WCONST_04.txt` constant 324 =
0.05, `NUM_MODEL_CONSTANTS` = 324. Pre-adoption backups: `/tmp/varn_ab/WCONST_pre_ice.bak`
and `/tmp/varn_ab/ICE_COVER_placeholder.bak`. ⚠ **Every other setup in the repo keeps
`ICE_LIGHT_TRANS = 1.00` and its own all-zero `ICE_COVER.txt`, so all of them remain
byte-identical** — the adoption is CL29-only.

**Reusable.**
- ⭐⭐ **A count can be defined in more places than the compiler will tell you about.**
  `nconst` lives in `aquabc_II_pelagic_interface.f90` *and* as a `parameter` in
  `mod_GLOBAL.f90` — the ESTAS path uses the latter. Changing one and rebuilding gave a clean
  build and a run that aborted on the stale count; only *running* it revealed the third
  definition. Same duplicated-count structure as `nstate` in the VARN work.
- ⭐⭐ **Design the change so the code is provably inert, and the data carries the science.**
  Wiring ice to light while the forcing is all zeros is byte-identical by construction, which
  separates code risk from science risk completely and made one `diff -r` sufficient to trust
  the whole build.
- ⭐ **Adding a model constant costs eight files and five counts here** (plus the CI's 0D
  example) — and `para_get_value` hard-stops on a missing name, so a new constant is
  backward-incompatible unless every read is guarded and every setup defaulted.

---

## 46. C:Chl re-tested on the ice baseline: the two corrections cancel, and the
two-guild case is now demonstrated (2026-09-03)

**The question.** §44.5 prescribed an order — fix the winter light climate, then re-test the
measured diatom C:Chl (§43.2: **34**, from 311 paired samples), then revisit the warm guild.
§45 adopted ice and closed February (2.89× → 0.99×). This is step two, run on the adopted
baseline with the prediction registered first, including its risk case: *lowering C:Chl
raises pigment **and** growth, so February could re-break.*

### 46.1 It re-breaks February, by the amount ice fixed

| | adopted (ice) | + C:Chl 34 |
|---|---|---|
| **Feb DIA_C** (obs 0.280) | **0.278 (0.99×)** | **0.798 (2.85×)** |
| **Nov DIA_C** (obs 0.307) | 0.070 (0.23×) | **0.314 (1.02×)** |
| Oct DIA_C (obs 0.670) | 0.023 (0.03×) | 0.033 (0.05×) |
| **seasonal r** | **+0.74** | **+0.64** |
| **autumn:spring** (obs 2.06) | 2.25 | **1.38** |
| Feb CHLA (obs 10.2) | 3.18 | 23.6 |
| CHLA RMSE | 23.830 | **23.134** |
| PO4 RMSE | 0.01755 | **0.01569** |

November is fixed exactly as it was in §43 (ratio 1.02) — that half of the correction was
never in doubt. But **February returns to 2.85×, essentially its pre-ice value**: the C:Chl
change undoes the ice fix.

**The arithmetic of the cancellation.** Lowering C:Chl 53 → 34 lowers `I_s` by 36 %, which
raises `LIM_LIGHT` by roughly 45 %; ice cuts February light by ~38 %. **Equal magnitude,
opposite sign** — so the two corrections annihilate each other in exactly the month where one
was adopted to work. This is not a tuning accident: both act on the same term, `I_s`, from
opposite directions.

### 46.2 The third instance of the same trap

Both headline RMSEs reach study bests here — **CHLA 23.134 and PO4 0.01569** — on a
configuration whose seasonal correlation fell 0.10 and whose autumn:spring moved from 2.25
to 1.38 against an observed 2.06. **That is the third time in this arc that both aggregate
scores improved while the phenology degraded** (§43.3, §46, and the §22 pigment channel
before them). The pattern is now reliable enough to state as a rule for this system:
**in CL29, a simultaneous improvement in chlorophyll and phosphate RMSE is not evidence of a
better model until the phase metrics are checked** — the two aggregates are the ones a
pigment or light-efficiency change moves first and most.

### 46.3 What it proves: the two-guild case, on a third independent axis

The measured 34 is not wrong. **It cannot be used because one C:Chl sets both guilds' light
efficiency**, and the two guilds need different values: November wants ≈34, February wants
≈53. A single-envelope model cannot hold both.

That is §41.2's conclusion — reached there from *taxonomy* (autumn is 44.5 % *Actinocyclus
normanii*, absent from the model's cold envelope) and *phenology* (two assemblages three
months apart in weighted mean month) — now demonstrated a third time, from *model
mechanics*, without reference to either. Three independent lines converge:

| axis | evidence | source |
|---|---|---|
| taxonomy | autumn = *Actinocyclus*/*Skeletonema*, spring = *Stephanodiscus*/*Asterionella* | §41.2 |
| phenology | weighted mean month 4.4–6.3 vs 8.0–8.2 | §41.2 |
| **model mechanics** | **one C:Chl cannot serve both seasons; the fix for one is the break for the other** | **§46** |

**Verdict: C:Chl 34 stays shelved, now for a demonstrated reason rather than a suspected
one.** §43 could only say it landed on an unfixed error; §46 shows that with that error
fixed it still cannot be adopted, because the constant is shared by two organisms the model
represents as one.

### 46.4 Where the arc stands

§44.5's chain has run to its end. Ice is adopted and operational (§45.5). C:Chl is measured
(§43.2), re-tested (§46.1) and refuted with the mechanism understood. **Both remaining roads
converge on the same object: the warm diatom guild of §41.2 — which needs its own cardinal
temperatures *and* its own C:Chl**, and would carry the autumn residual that §40.1's light
arithmetic says the current envelope cannot reach. October is untouched by everything tried
here (0.03 → 0.05), as it has been since §39.

Still open and unrelated: `FDAY`, read and never used (§44.2), and the background extinction
below the measured kd floor — both near-uniform across months, so both remain
separately-decided correctness fixes rather than levers on any residual.

**Reusable.** ⭐⭐ **Two corrections that act on the same term from opposite directions can
each be right and still cancel.** Ice (−38 % February light) and C:Chl (+45 % light
efficiency) are both measurement-anchored, and applying both leaves February exactly where it
started. Check what a candidate shares a *term* with, not just what it shares a *month* with.
⭐ **A shared constant is a hidden coupling between things the model treats as one organism** —
the cleanest possible argument for splitting the guild, and it arrived from mechanics rather
than biology.

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

**The second lesson cost three wrong claims: averaging across a sharp threshold inverts the
verdict, and this model is full of sharp thresholds.** The CTMI is exactly zero below T_min, so
any mean spanning that boundary is a mean over a bimodal quantity and describes no actual day.

- A Jul–**Sep** window put FIX_CYN's temperature factor at 0.287, in the "strongly limiting"
  band, when over the bloom months themselves it is 0.414 and merely one of several partial
  constraints. September's 0.026 did it (§3.1).
- Monthly-mean water temperature suggested the lagoon "never exceeds 21 °C" and that both
  diazotroph optima were therefore unreachable. Daily values reach 24–28 °C and the factor
  attains a full 1.000; the group is not capped at all (§1, §3.5).
- The same monthly means made autumn look like a matter of degree. On daily values it is
  absolute — zero days above threshold in three months across eleven years (§3.4).

Two of those three claims were wrong in the *optimistic* direction and one in the pessimistic,
so there is no safe bias to assume. **Check the daily distribution before interpreting any mean
that spans a threshold**, and report the fraction of zeros alongside the mean.
