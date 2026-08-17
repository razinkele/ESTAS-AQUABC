# How to make a model produce late-summer/autumn diazotroph dominance

Deep literature review, 2026-08-10, prompted by the finding that CL29's chlorophyll seasonality is
inverted (`docs/Satellite_model_pattern_comparison.md`). All papers retrieved via scite; no
editorial notices on any.

**A note on the season.** In the Curonian Lagoon the diazotrophs dominate **summer and autumn**, not
winter — Žilius et al. (2021) place the *Aphanizomenon flos-aquae* bloom in "summer and fall", and
both the in-situ record and the calibrated satellite product peak in **August–September**. CL29's
February maximum is the artifact to be removed, not a target to reproduce. This review therefore
asks: *what makes a model generate an autumn-weighted diazotroph bloom, and what suppresses a
spurious winter one?*

## 1. The core answer from the literature: life-cycle staging, not growth kinetics

Every successful reproduction of diazotroph phenology in the Baltic rests on the **Cyanobacteria
Life Cycle (CLC) model** of Hense & Beckmann (2006), simplified in Hense & Beckmann (2010). Its
design, as described by Hense, Hieronymus, Eilola et al. (2021):

> "The Cyanobacteria Life Cycle (CLC) model … includes, in its original design, four life cycle
> stages representing a **vegetative non-nitrogen-fixing stage, a vegetative nitrogen-fixing stage,
> a resting stage (akinetes), and a non-growing recruiting stage**."

> "The rapid increase (or decrease) in the summer concentrations is, in the CLC model, **a result of
> transfer between life cycle stages**, in turn, dependent on **light, temperature, and dissolved
> inorganic nitrogen (DIN)**."

The decisive point for us — the improvement comes from *staging*, not from better growth parameters:

> "The large improvement in seasonality when the life cycle of cyanobacteria is modeled, **as opposed
> to earlier modeling attempts that include only small winter populations**, does however indicate
> that the separation into different life cycle stages is of key importance for capturing the start
> and end of bloom."

### The failure mode they describe is close to ours

> "In earlier studies using models, the bloom of filamentous cyanobacteria was **initiated too late
> in the season, resulting in a very low nitrogen fixation due to the temperature dependence in the
> model and decreasing water temperatures during fall** (Hieronymus et al. 2018)."

A model whose diazotrophs are governed only by instantaneous temperature/nutrient limitation cannot
put biomass in late summer and autumn: by the time the competitive conditions arrive, the
temperature term is already declining. CL29 is exactly this kind of model, and its symptom is the
same — no autumn bloom.

### The three mechanisms that generate the right phenology

1. **Stage transitions gated by different variables than growth.** Vegetative cells take up DIN;
   the transition to the N-fixing (heterocystous) stage is triggered *by DIN limitation*; the
   transition to akinetes is triggered by **energy limitation**:
   > "The vegetative stage, during which dissolved inorganic nitrogen is taken up, is followed by a
   > nitrogen fixing stage **under DIN limitation** … **Energy limitation leads to a transformation
   > into the resting stage (akinetes)**, during which the cells sink to the bottom and 'mature'
   > (by refilling their internal nitrogen quota). The life cycle is closed by rising recruiting
   > cells after maturation." (Hense, Meier & Sonntag, 2013)

   Bloom **termination** is therefore *not* a temperature effect:
   > "the end of the bloom … can be explained by **energy limitation due to decreasing solar
   > radiation** which terminates the growth of the N₂-fixing life cycle stage and initiates the
   > formation of resting cells." (Hense et al., 2013)

   This is what holds the bloom into autumn: growth continues while light is adequate, and stops on
   a light cue rather than a temperature one.

2. **Stage-specific buoyancy.** The stages move differently, which is itself a niche mechanism:
   > "**HETs are positively buoyant, AKIs in the water (AKIW) are sinking** and may end up in the
   > sediment (AKIB) and **RECs are rising**." (Hense et al., 2021)

   Positively buoyant N-fixers sit in the light while their competitors mix down — a direct
   competitive advantage that a non-buoyant formulation cannot express.

3. **The inoculum feedback — a memory between years.** This is the mechanism that makes blooms
   self-sustaining rather than re-seeded from nothing each year:
   > "The higher the abundance of vegetative cells, the more resting cells are produced. This leads
   > to **enhanced germination, a higher 'inoculum' and thus to an increase in the abundance of
   > vegetative cells** (Hense 2007). The transitions between these life cycle stages **mark the
   > beginning and end of the bloom season**." (Hense et al., 2013)

   It also buffers bad years: "years in which the temperature is relatively low and growth
   conditions are unfavorable have a smaller effect on cyanobacteria concentrations in the following
   years."

## 2. Why CL29's diazotrophs go extinct — the paradigm it inherited

Munkes, Löptien & Dietze (2021) name the assumption built into most models, CL29 included:

> "A basic concept of the current generation of biogeochemical models is generally the widespread
> paradigm that **diazotrophic cyanobacteria grow more slowly than ordinary phytoplankton and can,
> therefore, in most models only thrive when nitrogen is no longer accessible to ordinary
> phytoplankton**."

This is a *conditional* niche: it only opens if the model actually depletes DIN. CL29 does not — its
summer DIN stays high (documented over-prediction), so the condition never fires and the fixers are
competed out permanently. Combined with §1, the diagnosis is complete: **CL29 gives its diazotrophs
a niche that never opens, and no mechanism (staging, buoyancy, inoculum memory) to occupy it when it
does.**

Note also what most models do *not* resolve — Munkes et al.: "modellers generally aim to keep the
model complexity on a low level, which typically results in the explicit representation of only one
'average' cyanobacteria species". AQUABC's three cyanobacteria compartments are unusually resolved,
which is why it exhibits competitive exclusion that lumped models never face.

## 3. A further niche mechanism: grazing refuge

Several life-cycle models give cyanobacteria a grazing refuge, which is a coexistence mechanism
independent of nutrients:

> "Following other models (Hense and Beckmann 2006; Lee et al. 2018), we assume that
> **cyanobacteria are non-grazeable due to toxicity**, while dinoflagellates and diatoms are equally
> grazed by zooplankton." (Hochfeld & Hinners, 2024)

Worth checking against AQUABC's zooplankton preference parameters: if cyanobacteria are grazed like
diatoms, they lose an advantage the literature routinely grants them. (Relevant caveat: CL29's
zooplankton is itself ~5.5× under-predicted, so grazing pressure is currently too weak overall.)

Hochfeld & Hinners (2024) is also the closest published analogue to our multi-group coexistence
problem: three functional groups with explicit life cycles *and* resuspension of resting cells,
showing that "competition and adaptation influence each other" and that neglecting these mechanisms
"can systematically overestimate warming-related changes in taxa dominance".

## 4. What our own experiment already shows

The structurally-corrected configuration (realistic light climate + observed C:Chl + recalibrated
growth) was re-examined by month. It **recovers most of the autumn deficit but none of the winter
excess**:

| | Jan–May mean | Aug–Oct mean | autumn/spring ratio | seasonal r vs in-situ |
|---|---|---|---|---|
| in-situ | 26.0 | 49.5 | **1.90** | — |
| defaults | 40.9 | 25.2 | 0.62 | −0.54 |
| structurally corrected | 44.1 | **40.5** | **0.92** | **−0.10** |

So the phase error decomposes into two separable problems:

- **The autumn deficit is substantially a light/stoichiometry problem** — fixing the light climate
  and C:Chl lifts Aug–Oct chlorophyll from 25 to 40 (observed 49) and moves the anti-correlation
  from −0.54 to −0.10.
- **The winter/spring excess is not** — it persists (40.9 → 44.1 against 26.0 observed) and is
  carried by diatoms (1.2–1.3 mg C/L through Jan–May). This is a separate defect: insufficient
  winter light/temperature limitation, and/or missing loss terms, for the *non-fixing* groups.

## 5. Concrete options for AQUABC, cheapest first

1. **Winter suppression of the non-fixers (independent of any diazotroph work).** The winter excess
   is a diatom problem and is not fixed by the light correction. Check the diatom temperature
   response at low temperature and the winter loss terms (sinking, mortality, grazing) before
   anything else — it is the larger contributor to the phase error and needs no new state variables.
2. **A winter refuge population for the fixers, not akinete germination.** For *Aphanizomenon
   flos-aquae* — the Curonian dominant — Munkes et al. report a "holoplanktonic life strategy … during
   winter a 'refuge population' of filaments can be observed in deeper waters, from which in
   spring/early summer the population will develop". A protected minimum concentration (a seed that
   competitive exclusion cannot erase) is far cheaper than a full life cycle and directly addresses
   the extinction we measured.
3. **Energy-limitation-driven stage transition.** If a resting stage is added, trigger the
   *transition* on light/energy rather than temperature — that is precisely what holds the modelled
   bloom into autumn in the CLC family.
4. **Stage-specific buoyancy.** AQUABC already has a buoyant-cyanobacteria option
   (`CYANO_BOUYANT_STATE_SIMULATION=1` in CL29); verify it actually confers a light advantage to the
   fixers in the box formulation, since that is one of the CLC model's main niche mechanisms.
5. **Grazing refuge.** Check zooplankton preference for the cyanobacteria groups against the
   literature convention of low or zero grazeability.
6. **Full CLC implementation** (vegetative → heterocystous → akinete → recruiting, with the inoculum
   feedback) — the validated solution, and the largest effort. AQUABC already carries Nostocales
   vegetative/heterocystous and akinete compartments, so the state variables partly exist; what is
   missing is the transition logic and the between-year memory.

## References (all retrieved via scite; no editorial notices)

- Hense, I., Hieronymus, J., Eilola, K., et al. (2021). Modeling cyanobacteria life cycle dynamics
  and historical nitrogen fixation in the Baltic Proper. *Biogeosciences*, 18(23), 6213–6227.
  https://doi.org/10.5194/bg-18-6213-2021
- Hense, I., Meier, H. E. M., & Sonntag, S. (2013). Projected climate change impact on Baltic Sea
  cyanobacteria. *Climatic Change*, 119(2), 391–406. https://doi.org/10.1007/s10584-013-0702-y
- Hochfeld, I., & Hinners, J. (2024). Evolutionary adaptation to steady or changing environments
  affects competitive outcomes in marine phytoplankton. *Limnology and Oceanography*, 69(5),
  1172–1186. https://doi.org/10.1002/lno.12559
- Munkes, B., Löptien, U., & Dietze, H. (2021). Cyanobacteria blooms in the Baltic Sea: a review of
  models and facts. *Biogeosciences*, 18(7), 2347–2378. https://doi.org/10.5194/bg-18-2347-2021
- Ramm, J., Lupu, A., Hadas, O., et al. (2012). A CARD-FISH protocol for the identification and
  enumeration of cyanobacterial akinetes in lake sediments. *FEMS Microbiology Ecology*, 82(1),
  23–36. https://doi.org/10.1111/j.1574-6941.2012.01401.x — notes that quantitative akinete data
  needed to constrain germination models are scarce
- Žilius, M., Vybernaite-Lubiene, I., Vaičiūtė, D., et al. (2021). Spatiotemporal patterns of N₂
  fixation in coastal waters … *Biogeosciences*, 18(5), 1857–1871.
  https://doi.org/10.5194/bg-18-1857-2021

⟨Primary sources cited above only through the citing literature — retrieve before citing directly:
Hense & Beckmann (2006, *Ecological Modelling* 195:205–218, the original CLC model) and Hense &
Beckmann (2010, *Ecological Modelling* 221:2330–2338, the simplified version); Hense (2007);
Hense & Burchard (2010); Hieronymus et al. (2018); Suikkanen et al. (2010); Wasmund (2017);
Cirés et al. (2013) on the dual akinete/overwintering strategy; Lee et al. (2018) on grazeability.⟩
