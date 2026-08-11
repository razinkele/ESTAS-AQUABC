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

### What remains open

1. **October is still low** (32.5 vs 46.4 µg/L): NOST stays gated by its own
   `NOST_VEG_HET_OPT_TEMP_LR = 16 °C`, and the same species-level literature argument applies
   to it. A companion run lowering it is the obvious next experiment.
2. **The winter half is untouched** (January 46.5 vs 17.6 µg/L), as expected — that is the
   diatom cardinal-temperature problem (§2), a separate decision.
3. This experiment changed a scratch config only. **Adopting T_min = 8 °C into `INPUTS_CL29`
   is a model-behaviour change and a user decision**, ideally with a recalibration pass after
   it, since the DE work to date was conditioned on fixers that could not grow.

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
