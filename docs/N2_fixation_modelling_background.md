# N₂-fixing cyanobacteria: scientific background for the CL29 coexistence finding

Literature investigation, 2026-08-09. Every paper below was retrieved through scite; DOIs are
verified and **no editorial notices (retractions/corrections) were found** on any of them. Quoted
passages are direct excerpts. Preprint→journal versions were resolved (two of the key references
were first read as Biogeosciences Discussions preprints; the citable final versions are used).

Purpose: (a) place the CL29 fixer-extinction result in the modelling literature; (b) assess the
proposed reframing of the paper as a model-based test of occasional field campaigns.

## 1. The field picture for the Curonian Lagoon

Žilius et al. (2021) is the reference study for lagoon N₂ fixation. Its design is the crux for the
reframing question — **two stations, one season, monthly sampling**:

> "We measured pelagic N₂ fixation and characterized bacterioplankton and phytoplankton communities
> at stations located in northern and south-central regions of the lagoon during **April–November
> 2018**."

and the authors are explicit that this cannot be scaled naively:

> "The patchy distribution of cyanobacteria poses a significant challenge to reliably extrapolating
> results from site-specific measurements to the ecosystem scale (Zilius et al., 2014, 2018)."

> "Our whole-lagoon estimates are based on data collected at stations within the northern and
> central portions of the lagoon, as **access to the southern region is problematic**."

They solved the upscaling problem with satellite chlorophyll rather than with a process model:

> "Here, we improve on our ability to scale up these measurements by using remote sensing of Chl a
> to infer spatial and temporal variation in N₂ fixation."

Their lagoon-scale result: **1.5–2.5 mmol N m⁻² d⁻¹ (mean 2.1 ± 0.1) for July–September**, against
1.9 ± 0.9 from the older two-station extrapolation; and the budget claim that matters for us:

> "N₂ fixation during summer and fall largely offset annual average denitrification
> (3.2 mmol N m⁻² d⁻¹) and was equivalent to half of the measured sediment–water TDN exchange
> (3.8 mmol N m⁻² d⁻¹; Zilius et al., 2018)."

### ⭐ The mechanism — and why it indicts our model

> "the decline in riverine NO₃⁻ inputs following spring snowmelt (and the subsequent depletion of
> DIN in the lagoon) provides favorable conditions for an active diazotrophic community during
> summer and fall."

> "The occurrence of elevated NO₃⁻ concentrations and high DIN:DIP after spring runoff was followed
> by an extended period (**8 months**) of persistent low N availability, creating a **temporal
> niche** for heterocystous cyanobacteria."

This is the direct explanation of the CL29 failure. The observed diazotroph bloom is *gated by a
seasonal DIN-depletion window*. CL29 does not open that window: summer NO₃ is over-predicted
roughly an order of magnitude (documented across this project; summer DIN ~0.29 vs ~0.06 mg N/L),
i.e. the model keeps the lagoon nitrogen-replete through exactly the period when the real system
starves and hands the niche to the fixers. **The nutrient bias and the fixer extinction are
therefore not two separate failures but one:** no depletion window ⇒ no competitive release ⇒
diazotrophs excluded. Our composition-resolved calibration (fixers unrescuable even at halved
mortality) is the numerical signature of a niche that never opens.

The dominant fixer here is *Aphanizomenon flos-aquae* (Žilius et al., 2021), with
*Dolichospermum* also present — and their blooms are, in the field, intermittent:

> "Spatially extensive blooms of heterocystous cyanobacteria (Aphanizomenon and Dolichospermum) are
> **occasionally observed**, particularly during low-wind conditions."

## 2. The modelling picture: this is a known, open problem

Munkes, Löptien & Dietze (2021) reviewed five state-of-the-art Baltic biogeochemical models
against the field and laboratory evidence:

> "the processes involved in the bloom formation of cyanobacteria are still **not comprehensively
> understood**."

> "We compared five state-of-the-art biogeochemical models … Our elaborations illustrate that, to
> date, there is only **limited consensus on the degree of simplification and concerning key
> processes** necessary for a reliable simulation [of] cyanobacteria distribution and biomass."

Three points from that review bear directly on our result:

1. **Most models sidestep coexistence entirely.**
   > "modellers generally aim to keep the model complexity on a low level, which typically results
   > in the explicit representation of only one 'average' cyanobacteria species rather than the
   > explicit representation of all potentially important species."

   AQUABC resolves *three* cyanobacteria compartments (non-fixing, fixing, Nostocales with
   akinetes) plus diatoms and other algae. Our competitive-exclusion problem is thus partly **a
   consequence of higher functional resolution**: a lumped "average cyanobacteria" compartment
   cannot fail this way because it never has to coexist with anything. This is a general lesson —
   resolution buys realism only if the niche structure that separates the groups is also resolved.

2. **The niche formulation is the identified frontier — verbatim.**
   > "We rate specifically the unclear relationship between excess P and cyanobacteria growth and
   > the relation between bloom intensity and nitrogen fixation as key for studies to come. The
   > respective uncertainties … are consistent with findings by Löptien and Dietze (2020), who
   > illustrate in a global model that **future projections of diazotrophs may diverge considerably
   > — depending on the specific model formulations that determine their ecological niche.**"

   Our study is an empirical, decadal, group-resolved demonstration of precisely this: with the
   niche formulation as shipped, the diazotroph compartment goes to zero regardless of its own
   rate parameters.

3. **Excess phosphate and light are the agreed preconditions.**
   > "all models agree in that excess phosphate is a necessary precondition for a cyanobacteria
   > bloom … there is consensus that the availability of light is essential to the growth of
   > cyanobacteria."

   CL29 has *excess* phosphate (over-predicted) and — before our correction — *excess* light, yet
   still fails to grow diazotrophs. Both agreed preconditions are over-satisfied; the binding
   constraint is the missing nitrogen-depletion niche.

## 3. The published fix: life-cycle dynamics

Hieronymus, Eilola, Olofsson et al. (2021) implemented a cyanobacteria **life-cycle model** in a
3-D Baltic biogeochemical model (1850–2008):

> "The explicit consideration of life cycle dynamics and transitions **significantly improves the
> representation of the cyanobacterial phenological patterns** compared to earlier 3D modeling
> efforts. Now, the rapid increase and decrease in cyanobacteria in the Baltic Sea are well
> captured, and the seasonal timing is in concert with observations. The current improvement also
> had a large effect on the nitrogen fixation load and is now in agreement with estimates based on
> in situ measurements."

This is a validated precedent for the structural development our results demand — and AQUABC
already carries part of the machinery (Nostocales vegetative/heterocystous biomass plus dormant
akinetes), which in CL29 is effectively inert after year 1.

**Species caveat that matters for CL29.** Munkes et al. note that akinete strategies are
species-specific, and that the Curonian dominant is *not* the akinete-reliant case:

> "*Aphanizomenon flos-aquae* seems to have a holoplanktonic life strategy … It can produce
> akinetes, but during winter a **'refuge population' of filaments** can be observed in deeper
> waters … from which in spring/early summer the population will develop."

⇒ For this lagoon the priority is likely a **persistent overwintering seed population** (a refuge
that is not competitively erased in winter) rather than akinete germination *per se*. That is a
cheaper and more targeted change than a full life-cycle module, and it is testable.

## 4. Assessment of the proposed reframing

**The premise is correct and well-evidenced.** The Curonian process literature is a set of
intensive but *occasional* campaigns — Žilius et al. (2021): two stations, April–November 2018;
Žilius et al. (2018); Bartoli et al. (2021): two seasons, two sediment types; Petkuvienė et al.
(2016) — every one of which acknowledges an upscaling problem, with the southern (Russian) part of
the lagoon inaccessible throughout. A continuous 11-year, 29-box hindcast is exactly the
complementary instrument, and no such confrontation has been published for this system.

**But an honest version cannot claim to *validate* the field budgets**, because the model fails on
the diazotroph term those papers emphasize most. The defensible framing is a *confrontation* that
resolves into a clean split:

- **Where the model corroborates the field synthesis:** seasonal nutrient dynamics and the DIN
  drawdown direction, the denitrification-dominated N sink (our benthic-denitrification work sits
  within the measured 1.2–4.8 mmol N m⁻² d⁻¹ range), winter/shoulder nutrient fields, and
  chlorophyll magnitude — now with continuous coverage where the campaigns had months.
- **Where it fails, diagnostically:** the diazotroph term. And the failure is *explained by the
  field papers themselves* — the model never opens the DIN-depletion window that Žilius et al.
  (2021) identify as the trigger, so the fixers are competitively excluded, and the lagoon-scale
  N₂-fixation flux those authors estimated (~2.1 mmol N m⁻² d⁻¹, offsetting denitrification) cannot
  be reproduced.

That is a stronger paper than either framing alone: it uses the occasional field record as the
*target*, the decadal model as the *instrument*, and the mismatch as the *result* — with a
published, validated remedy (life-cycle/refuge dynamics) to recommend.

**Recommended framing.** Keep the v0.3 methodological spine (parameters outside the calibration's
reach; compensating errors) as the *mechanism* of the paper, and adopt the field-confrontation as
the *motivation and narrative frame*. Concretely: open on the occasional-campaign problem and the
need to extend it continuously; show what a decadal hindcast can and cannot corroborate; identify
the light/stoichiometry configuration errors as prerequisites that had to be fixed before any such
comparison was meaningful; and land on the coexistence/niche gap as the single quantified
structural deficit, with the DIN-window mechanism as its explanation and life-cycle/refuge
dynamics as the tested remedy.

⟨Decision for the co-authors: this framing makes *Ecological Modelling* still viable but arguably
favours a biogeochemistry venue — *Biogeosciences* published all three anchor references
(Žilius 2021; Munkes 2021; Hieronymus 2021), which is a strong signal for fit and reviewer pool.⟩

## References (all retrieved via scite; no editorial notices)

- Bartoli, M., Nizzoli, D., Žilius, M., et al. (2021). Denitrification, nitrogen uptake, and organic
  matter quality undergo different seasonality in sandy and muddy sediments of a turbid estuary.
  *Frontiers in Microbiology*, 11, 612700. https://doi.org/10.3389/fmicb.2020.612700
- Hieronymus, J., Eilola, K., Olofsson, M., et al. (2021). Modeling cyanobacteria life cycle
  dynamics and historical nitrogen fixation in the Baltic Proper. *Biogeosciences*, 18(23),
  6213–6227. https://doi.org/10.5194/bg-18-6213-2021
- Munkes, B., Löptien, U., & Dietze, H. (2021). Cyanobacteria blooms in the Baltic Sea: a review of
  models and facts. *Biogeosciences*, 18(7), 2347–2378. https://doi.org/10.5194/bg-18-2347-2021
- Petkuvienė, J., Žilius, M., Lubienė, I., et al. (2016). Phosphorus cycling in a freshwater estuary
  impacted by cyanobacterial blooms. *Estuaries and Coasts*, 39(5), 1386–1402.
  https://doi.org/10.1007/s12237-016-0078-0
- Žilius, M., Vybernaite-Lubiene, I., Vaičiūtė, D., et al. (2021). Spatiotemporal patterns of N₂
  fixation in coastal waters derived from rate measurements and remote sensing. *Biogeosciences*,
  18(5), 1857–1871. https://doi.org/10.5194/bg-18-1857-2021
- Žilius, M., Vybernaite-Lubiene, I., Petkuvienė, J., et al. (2018). The influence of cyanobacteria
  blooms on the attenuation of nitrogen throughputs in a Baltic coastal lagoon. *Biogeochemistry*,
  141(2), 143–165. https://doi.org/10.1007/s10533-018-0508-0

⟨Not yet retrieved, cited above only as reported by Munkes et al. (2021) — pull before citing
directly: Löptien & Dietze (2020) on diazotroph niche formulations in a global model;
Olofsson et al. (2020b) Baltic taxa-specific N₂ fixation; Rolff et al. (2007) on patchiness.⟩
