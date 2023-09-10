---
title: "LandR _Biomass_borealDataPrep_ Manual"
date: "Last updated: 2023-02-08"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    toc_depth: 4
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
  markdown: 
    wrap: 80
bibliography: citations/references_Biomass_borealDataPrep.bib
citation-style: citations/ecology-letters.csl
link-citations: true
always_allow_html: true
---

<!-- the following are text references used in captions for LaTeX compatibility -->

(ref:Biomass-borealDataPrep) *Biomass_borealDataPrep*

(ref:percent) %

(ref:Abie-bal) *Abies balsamea*

(ref:Abie-las) *A. lasiocarpa*

(ref:Pinu-con) *Pinus contorta*

(ref:Pice-sp) *Picea spp.*





[![module-version-Badge](/home/runner/work/Biomass_borealDataPrep/Biomass_borealDataPrep/figures/moduleVersionBadge.png)](https://github.com/PredictiveEcology/Biomass_borealDataPrepf069461d0822050fa2a0c852fd7905dce7a1c531)

[![Issues-badge](/home/runner/work/Biomass_borealDataPrep/Biomass_borealDataPrep/figures/issuesBadge.png)](https://github.com/PredictiveEcology/Biomass_borealDataPrep/issues)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Yong Luo <Yong.Luo@gov.bc.ca> [aut], Eliot J B McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut, cre], Ceres Barros <ceres.barros@ubc.ca> [aut], Alex M. Chubaty <achubaty@for-cast.ca> [aut]
<!-- ideally separate authors with new lines, '\n' not working -->

**This documentation is a work in progress. Potential discrepancies and
omissions may exist for the time being. If you find any, contact us using the
"Get help" link above.**

## Module Overview

### Quick links

-   [General functioning](#bboreal-general-functioning)

-   [List of input objects](#bboreal-inputs-list)

-   [List of parameters](#bboreal-params-list)

-   [List of outputs](#bboreal-outputs-list)

-   [Simulation flow and module events](#bboreal-sim-flow)

### Summary

LandR *Biomass_borealDataPrep* (hereafter *Biomass_borealDataPrep*), prepares
all necessary inputs for *Biomass_core* based on data available for forests
across Canada forests, but focused on Western Canada boreal forest systems.
Nevertheless, it provides a good foundation to develop other modules aimed at
different geographical contexts. By keeping data preparation and parameter
estimation outside of *Biomass_core*, we promote the modularity of the
LandR-based model systems and facilitate interoperability with other parameter
estimation procedures.

Specifically, it prepares and adjusts invariant and spatially-varying species
trait values, as well as ecolocation-specific parameters, probabilities of
germination and initial conditions necessary to run *Biomass_core*. For this,
*Biomass_borealDataPrep* requires internet access to retrieve default
data[^biomass_borealdataprep-1].

[^biomass_borealdataprep-1]: Raw data layers downloaded by the module are saved
    in \`dataPath(sim)\`, which can be controlled via
    \`options(reproducible.destinationPath = ...)\`.

We advise future users to run *Biomass_borealDataPrep* with defaults and inspect
the resulting input objects are like before supplying alternative data (or data
URLs).

### Links to other modules {#bboreal-links-modules}

*Biomass_borealDataPrep* is intended to be used with
[*Biomass_core*](https://github.com/PredictiveEcology/Biomass_core), but can be
linked with other data modules that prepare inputs. See
[here](https://rpubs.com/PredictiveEcology/LandR_Module_Ecosystem) for all
available modules in the LandR ecosystem and select *Biomass_borealDataPrep*
from the drop-down menu to see potential linkages.

-   [*Biomass_core*](https://github.com/PredictiveEcology/Biomass_core): core
    forest dynamics simulation module. Used downstream from
    *Biomass_borealDataPrep*;

-   [*Biomass_speciesData*](https://github.com/PredictiveEcology/Biomass_speciesData):
    grabs and merges several sources of species cover data, making species
    percent cover ((ref:percent) cover) layers used by other LandR Biomass
    modules. Default source data spans the entire Canadian territory. Used
    upstream from *Biomass_borealDataPrep*;

-   [*Biomass_speciesParameters*](https://github.com/PredictiveEcology/Biomass_speciesParameters):
    calibrates four-species level traits using permanent sample plot data (i.e.,
    repeated tree biomass measurements) across Western Canada. Used downstream
    from *Biomass_borealDataPrep*.

## Module manual

### General functioning {#bboreal-general-functioning}

*Biomass_borealDataPrep* prepares all inputs necessary to run a realistic
simulation of forest dynamics in Western Canadian boreal forests using
*Biomass_core*. Part of this process involves cleaning up the input data and
imputing missing data in some cases, which are discussed in detail in [Data
acquisition and treatment](#bboreal-dataprep).

After cleaning and formatting the raw input data, the module:

1.  **calculates species biomass per pixel** by multiplying the observed species
    (ref:percent) cover by the observed stand biomass and an adjustment factor,
    which can be statistically calibrated for the study area. Given that this
    adjusts the species biomass, this calibration step contributes to the
    calibration of `maxB` and `maxANPP` trait values, whose estimation is also
    based on species biomass (see [Initial species age and biomass per
    pixel](#bboreal-init-B-age) and [Adjustment of species
    biomass](#bboreal-adjustB));

2.  prepares **invariant species traits** -- these are spatio-temporally
    constant species traits that influence population dynamics (e.g., growth,
    mortality, dispersal) and responses to fire (see [Invariant species
    traits](#bboreal-invariant-traits));

3.  defines **ecolocations** -- groupings of pixels with similar biophysical
    conditions. By default, ecolocations are defined as the spatial combination
    of ecodistricts of the National Ecological Framework for Canada, and the
    Land Cover of Canada 2010 map (see [Defining simulation pixels and
    ecolocations](#bboreal-forestedPix-ecolocations)). **Note that ecolocation
    is termed `ecoregionGroup` across LandR modules**.

4.  prepares **ecolocation-specific parameters** and **probabilities of
    germination** -- only one ecolocation-specific parameter is used, the
    [minimum relative biomass thresholds](#bboreal-minRelB), which defines the
    level of shade in a pixel. Together the level of shade and the
    [probabilities of germination](#bboreal-prob-germ) influence germination
    success in any given pixel;

5.  estimates **spatio-temporally varying species traits** -- species traits
    that can vary by ecolocation and in time. These are maximum biomass
    (`maxB`), maximum above-ground net primary productivity (`maxANPP`; see
    [Maximum biomass and maximum aboveground net primary
    productivity](#bboreal-maxB-maxANPP)) and species establishment probability
    (SEP, called `establishprob` in the module traits table; see [Species
    establishment probability](#bboreal-SEP)). **By default,
    *Biomass_borealDataPrep* estimates temporally constant values of `maxB`,
    `maxANPP` and SEP**;

6.  creates **initial landscape conditions** -- *Biomass_borealDataPrep*
    performs data-based landscape initialisation, by creating the species cohort
    table (`cohortData`) and corresponding map (`pixelGroupMap`; both used to
    initialise and track cohorts across the landscape) based on observed stand
    age and species biomass.

As *Biomass_core* only simulates tree species dynamics, *Biomass_borealDataPrep*
prepares all inputs and estimates parameters in pixels within forested
land-cover classes (see [Defining simulation pixels and
ecolocations](#bboreal-forestedPix-ecolocations)).

If a `studyAreaLarge` is supplied, the module uses it for parameter estimation
to account for larger spatial variability.

In the next sections, we describe in greater detail the various data processing
and parameter estimation steps carried out by *Biomass_borealDataPrep*.

### Data acquisition and treatment {#bboreal-dataprep}

The only two objects that the user must supply are shapefiles that define the
study area used to derive parameters (`studyAreaLarge`) and the study area where
the simulation will happen (`studyArea`). The two objects can be identical if
the user chooses to parametrise and run the simulations in the same area. If not
identical, `studyArea` must be fully within `studyAreaLarge`. If
`studyAreaLarge` and `studyArea` are in Canada, the module can automatically
estimate and prepare all input parameters and objects for *Biomass_core*, as the
default raw data are FAIR data [*sensu* @WilkinsonEtAl2016] at the
national-scale.

If no other inputs are supplied, *Biomass_borealDataPrep* will create raster
layer versions `studyAreaLarge` and `studyArea` (`rasterToMatchLarge` and
`rasterToMatch`, respectively), using the stand biomass map layer
(`rawBiomassMap`) as a template (i.e., the source of information for spatial
resolution).

#### Defining simulation pixels and ecolocations {#bboreal-forestedPix-ecolocations}

*Biomass_borealDataPrep* uses land-cover data to define and assign parameter
values to the pixels where forest dynamics will be simulated (forested pixels).
By default it uses land-cover classes from the [Land Cover of Canada 2010 v1
map](http://www.cec.org/north-american-environmental-atlas/land-cover-2010-modis-250m/),
a raster-based database that distinguishes several forest and non-forest
land-cover types. Pixels with classes 1 to 6 are included as forested pixels
(see parameter `forestedLCCClasses`).

When the land-cover raster (`rstLCC`) includes transient cover types (e.g.,
recent burns) the user may pass a vector of transient class IDs (via the
parameter `LCCClassesToReplaceNN`) that will be reclassified into a "stable"
forested class (defined via the parameter `forestedLCCClasses`). The
reclassification is done by searching the focal neighbourhood for a replacement
forested cover class (up to a radius of 1250m from the focal cell). If no
forested class is found within this perimeter, the pixel is not used to simulate
forest dynamics. Reclassified pixels are omitted from the fitting of statistical
models used for parameter estimation, but are assigned predicted values from
these models.

Sub-regional spatial variation in `maxBiomass`, `maxANPP`, and SEP species
traits is accounted for by ecolocation. Ecolocations are used as proxies for
biophysical variation across the landscape when estimating model parameters that
vary spatially. By default, they are defined as the combination of
"ecodistricts" from the [National Ecological Framework for
Canada](https://open.canada.ca/data/en/dataset/3ef8e8a9-8d05-4fea-a8bf-7f5023d2b6e1)
(a broad-scale polygon layer that captures sub-regional variation)
(`ecoregionLayer`) and the above land cover (`rstLCC`), but the user can change
this by supplying different ecozonation or land-cover layers.

#### Species cover {#bboreal-sppcover}

Species cover ((ref:percent) cover) raster layers (`speciesLayers`) can be
automatically obtained and pre-processed by *Biomass_borealDataPrep*. The module
ensures that:

1.  all data have the same geospatial properties (extent, resolution);
2.  all layers these are correctly re-projected to `studyAreaLarge` and
    `rasterToMatchLarge`;
3.  species with no cover values above 10(ref:percent) are excluded.

By default it uses species (ref:percent) cover rasters derived from the MODIS
satellite imagery from 2001, obtained from the Canadian National Forest
Inventory [@BeaudoinEtAl2017] -- hereafter 'kNN species data'.

#### Initial species age and biomass per pixel {#bboreal-init-B-age}

Stand age and aboveground stand biomass (hereafter 'stand biomass') are used to
derive parameters and define initial species age and biomass across the
landscape. These are also derived from MODIS satellite imagery from 2001
prepared by the NFI [@BeaudoinEtAl2017] by default.

*Biomass_borealDataPrep* downloads these data and performs a number of data
harmonization operations to deal with data inconsistencies. It first searches
for mismatches between stand age (`standAge`), stand biomass (`standB`) and
total stand cover (`standCover`), assuming that cover is the most accurate of
the three, and biomass the least, and in the following order:

1.  Pixels with `standCover < 5%` are removed;

2.  Pixels with `standAge == 0`, are assigned `standB == 0`;

3.  Pixels with `standB == 0`, are assigned `standAge == 0`.

Then, species is assigned one cohort per pixel according to the corrected stand
age, stand biomass and (ref:percent) cover values. Cohort age is assumed to be
the same as stand age and biomass is the product of stand biomass and species
(ref:percent) cover. Before doing so, stand cover is rescaled to vary between 0
and 100(ref:percent).

A next set of data inconsistencies in cohort age (`age`), biomass (`B`) and
cover (`cover`) is looked for and solved in the following order:

4.  if `cover > 0` and `age == 0`, `B` is set to 0 (and stand biomass
    recalculated);

5.  if `cover == 0` and `age > 0`, or if `age == NA`, `age` is empirically
    estimated using the remainder of the data to fit the model supplied by
    `P(sim)$imputeBadAgeModel`, which defaults to:


```
## [[1]]
## lme4::lmer(age ~ log(totalBiomass) * cover * speciesCode + (log(totalBiomass) | 
##     initialEcoregionCode))
```

Cohort biomass is then adjusted to reflect the different cover to biomass
relationship of conifer and broadleaf species (see [Adjustment of initial
species biomass](#bboreal-adjustB)).

#### Replacing initial biomass and age within known fire perimeters {#bboreal-spin-up}

*Biomass_borealDataPrep* can use fire perimeters to correct stand ages. To do
so, it downloads the latest fire perimeter data from the [Canadian Wildfire Data
Base](https://cwfis.cfs.nrcan.gc.ca/datamart) and changes pixel age inside fire
perimeters to match the time since last fire, using fire years up to the first
year of the simulation.

Taking two independent datasets for stand age (fire perimeters) and stand
biomass (derived from MODIS satellite imagery) can cause discrepancies (e.g.
stand age = 5 and aboveground biomass = 10000 m2/ha). This may be due to errors
coming from a) a stand replacing disturbance that reset age to zero a few years
before, but the biomass layer was no zeroed, or b) the disturbance was not
stand-replacing (leaving biomass), but age was still zeroed. This means that
either, aboveground biomass is wrong or age is.

Options to address this include 1) get better data for these two variables that
do not contradict one another (not currently available to us) or 2) estimate one
or the other. There is no obvious way to decide which one is incorrect, unless
there is an independent data source.

In the current *Biomass_borealDataPrep* module version, we chose to correct
both. If `P(sim)$fireURL` is provided and `P(sim)$overrideBiomassInFires` is
TRUE, fire perimeters are used as the source of information for age, and
*Biomass_core* is used to generate corresponding biomassvalues based on the
estimated growth parameters and known species presence/absence (from the species
cover layers).

This assumes that 1) recorded fires were stand-replacing, and so time since fire
is the new stand age and 2) that the first year of the simulation is later than
the first fire year in the fire perimeter data. The biomass spin-up with
*Biomass_core* is only run in pixels were stand ages were corrected, for as long
as the new stand age (i.e., the time since last fire). All specie start with age
= 0 and biomass = 0, and grow until time since last fire is achieved. The
resulting species biomass is used as the initial biomass values for each species
cohort in the actual simulation.

If the user does not want to perform this imputation, this step can be bypassed
by setting the parameter `P(sim)$overrideBiomassInFires` to FALSE or
`P(sim)$fireURL` to NULL or NA.

Also pixels that suffered *any kind* of data imputation (e.g., the age
corrections detailed in the [previous section](#bboreal-init-B-age)) can be
excluded from the simulation by setting `P(sim)$rmImputedPix == TRUE`.

#### Invariant species traits {#bboreal-invariant-traits}

Most invariant species traits are obtained from available species trait tables
used in LANDIS-II applications in Canada's boreal forests (available in [Dominic
Cyr's GitHub repository](https://github.com/dcyr/LANDIS-II_IA_generalUseFiles)).
Some are then adapted with minor adjustments to match Western Canadian boreal
forests using published literature. Others (key growth and mortality traits) can
be calibrated by *Biomass_speciesParameters* (see [Calibrating species
growth/mortality traits using
*Biomass_speciesParameters*](#bboreal-B_sppParams-calib)).

The LANDIS-II species trait table contains species trait values for each
Canadian Ecozone [@NRCan2013], which are by default filtered to the Boreal
Shield West (BSW), Boreal Plains (BP) and Montane Cordillera Canadian Ecozones
(via `P(sim)$speciesTableAreas`). Most trait values do not vary across these
ecozones for a given species, but when they do the minimum value is used.

The function `LandR::speciesTableUpdate` is used by default to do further
adjustments to trait values in this table (if this is not intended, a custom
function call or `NULL` can be passed to `P(sim)$speciesUpdateFunction`):

-   Longevity values are adjusted to match the values from @burton1995, which
    match BSP, BP and MC ecozones. These adjustments result in higher longevity
    for most species;

-   Shade tolerance values are lowered for *Abies balsamifera*, *Abies
    lasiocarpa*, *Picea engelmanii*, *Picea glauca*, *Picea mariana*, *Tsuga
    heterophylla* and *Tsuga mertensiana* to better **relative** shade tolerance
    levels in Western Canada. Because these are relative shade tolerances, the
    user should **always** check these values with respect to their own study
    areas and species pool.

The user can also pass more than one function call to
`P(sim)$speciesUpdateFunction` if they want to make other adjustments in
addition to those listed above (see `?LandR::updateSpeciesTable`).

#### Probabilities of germination {#bboreal-prob-germ}

By default, *Biomass_borealDataPrep* uses the same probabilities of germination
(called `sufficientLight` in the module) as *Biomass_core*. These are obtained
from publicly available [LANDIS-II
table](https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt).

### Parameter estimation/calibration

#### Adjustment of initial species biomass {#bboreal-adjustB}

*Biomass_borealDataPrep* estimates initial values of species aboveground biomass
(`B`) based on stand biomass (`standB`) and individual species (ref:percent)
cover. Initial `B` is estimated for each species in each pixel by multiplying
`standB` by species (ref:percent) cover. Because the default cover layers are
satellite-derived, the relationship between relative cover and relative biomass
of broadleaf and conifer species needs to be adjusted to reflect their different
canopy architectures (using `P(sim)$deciduousCoverDiscount`).

By default, *Biomass_borealDataPrep* uses a previously estimated
`P(sim)$deciduousCoverDiscount` based on Northwest Territories data. However,
the user can chose to re-estimate it by setting
`P(sim)$fitDeciduousCoverDiscount == TRUE`. In this case, by default
*Biomass_borealDataPrep* will fit the the following model:


```
## [[1]]
## glm(I(log(B/100)) ~ logAge * I(log(totalBiomass/100)) * speciesCode * 
##     lcc)
```

which relates the estimated biomass (`B`) with an interaction term between
log-age (`logAge`), `standB` ('totalBiomass'), `speciesCode` (i.e. species ID)
and land cover ('lcc'). The model is fitted to the `standB` and species cover on
`studyAreaLarge`, using an optimization routine that searches for the best
conversion factor between broadleaf species cover and `B` by minimizing AIC.

#### Maximum biomass and maximum aboveground net primary productivity {#bboreal-maxB-maxANPP}

*Biomass_borealDataPrep* statistically estimates maximum biomass (`maxB`) and
maximum aboveground net primary productivity (`maxANPP`) using the processed
species ages and biomass.

`maxB` is estimated by modelling the response of species biomass (`B`) to
species age and cover, while accounting for variation between ecolocations
(`ecoregionGroup` below):


```
## [[1]]
## lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (logAge + 
##     cover | ecoregionGroup))
```

The coefficients are estimated by maximum likelihood and model fit is calculated
as the proportion of explained variance explained by fixed effects only
(marginal r2) and by the entire model (conditional r2) -- both of which are
printed as messages.

Because the model can take a while to fit, by default we sample pixels within
each species and ecolocation combination (sample size defined by the
`P(sim)$subsetDataBiomassModel` parameter).

If convergence issues occur and `P(sim)$fixModelBiomass == TRUE`, the module
attempts to refit the model by re-sampling the data, re-fitting `lmer` with the
`bobyqa` optimizer, and re-scaling the continuous predictors (by default,
`cover` and `logAge`). These steps are tried additively until the convergence
issue is resolved. If the module is still unable to solve the convergence issue
a message is printed and the module uses the last fitted model.

Note that convergence issues are not usually problematic for the estimation of
coefficient values, only for estimation of their standard errors. However, the
user should always inspect the final model (especially if not converged) and
make sure that the problems are not significant and that the fitted model meets
residual assumptions. For this, the user should make sure model objects are
exported to the `simList` using the `exportModels` parameter.

Alternative model calls/formulas can be supplied via the `P(sim)$biomassModel`
parameter. Note that if supplying a model call that does not use `lme4::lmer`
the refitting process is likely to fail and may have to be disabled (via the
`P(sim)$fixModelBiomass` parameter).

Another consideration to add with respect to the estimation of `maxB`, is that
we are choosing a linear model to relate `B ~ log(age) + cover`. This is not
ideal from an ecological point of view, as biomass is unlikely to vary linearly
with age or cover, and more likely to saturate beyond a certain high value of
cover and follow a hump-shaped curve with age (i.e., reaching maximum values for
a given age, and then starting to decrease as trees approach longevity). Also,
fitting a linear model can lead to negative `B` values at young ages and low
cover. However, our tests revealed that a linear mixed effects model was not
producing abnormal estimates of `B` at maximum values of age and cover (hence,
`maxB` estimates), while allowing to leverage on the powerful statistical
machinery of `lme4`.

Finally, we highlight that modelling `log(B)` is NOT an appropriate solution,
because it will wrongly assume an *exponential* relationship between
`B ~ log(age) + cover`, leading to a serious overestimation of `maxB`(Fig.
\@ref(fig:fig-biomassModelLogBtest)) and steep increases in species biomasses
during the first years of the simulation (Fig. \@ref(fig:fig-simBLogBtest)).

<div class="figure" style="text-align: center">
<img src="figures/biomassModel_logBtest.png" alt="Modelling biomass as a linear vs. exponential relationship. a) `modelBiomass` as `B ~ logAge * speciesCode + cover * speciesCode + (logAge + cover | ecoregionGroup)`. b) `modelBiomass` as `logB ~ logAge * speciesCode + cover * speciesCode + (logAge + cover | ecoregionGroup)`. Blue dots are marginal mean B values (back-transformed in b) cross ages with confidence intervals as the bars." width="70%" />
<p class="caption">(\#fig:fig-biomassModelLogBtest)Modelling biomass as a linear vs. exponential relationship. a) `modelBiomass` as `B ~ logAge * speciesCode + cover * speciesCode + (logAge + cover | ecoregionGroup)`. b) `modelBiomass` as `logB ~ logAge * speciesCode + cover * speciesCode + (logAge + cover | ecoregionGroup)`. Blue dots are marginal mean B values (back-transformed in b) cross ages with confidence intervals as the bars.</p>
</div>

<div class="figure" style="text-align: center">
<img src="figures/simulatedB_logBtest.png" alt="Thirty years of simulation with `maxB` values estimated from a `logB ~ ...` `biomassModel` (see Fig. \@ref(fig:fig-biomassModelLogBtest)). The steep increase in such little time is abnormal." width="70%" />
<p class="caption">(\#fig:fig-simBLogBtest)Thirty years of simulation with `maxB` values estimated from a `logB ~ ...` `biomassModel` (see Fig. \@ref(fig:fig-biomassModelLogBtest)). The steep increase in such little time is abnormal.</p>
</div>

After the biomass model is fit, `maxB` is predicted by species and ecolocation
combination, for maximum species cover values (100(ref:percent)) and maximum
log-age (the log of species longevity). When using *Biomass_speciesParameters*,
`maxB` is calibrated so that species can achieve the maximum observed biomass
during the simulation (see [Calibrating species growth/mortality traits using
*Biomass_speciesParameters*](#bboreal-B_sppParams-calib)).

`maxANPP` is the calculated as `maxB * mANPPproportion/100`, where
`mANPPproportion` defaults to 3.33, unless calibrated by
*Biomass_speciesParameters* (see [Calibrating species growth/mortality traits
using *Biomass_speciesParameters*](#bboreal-B_sppParams-calib)). The default
value, 3.33, comes from an inversion of the rationale used to calculate `maxB`
in @SchellerMladenoff2004. There, the authors estimated `maxANPP` using the
model PnET-II (and then adjusted the values manually) and from these estimates
calculated `maxB` by multiplying the estimated `maxANPP` by 30.

#### Species establishment probability {#bboreal-SEP}

Species establishment probability (SEP, `establishprob` in the module) is
estimated by modelling the probability of observing a given species in each
ecolocation. For this, *Biomass_borealDataPrep* models the relationship between
probability of occurrence of a species ($\pi$) using the following model by
default:


```
## [[1]]
## glm(cbind(coverPres, coverNum - coverPres) ~ speciesCode * ecoregionGroup, 
##     family = binomial)
```

whereby the probability of occurrence of a species ($\pi$) -- calculated as the
proportion of pixels with (ref:percent) cover \> 0 -- is modelled per species
and ecolocation following a binomial distribution with a logit link function.
There is no data sub-sampling done before fitting the SEP statistical model, as
the model fits quickly even for very large sample sizes (e.g., \> 20 million
points).

SEP is then predicted by species and ecolocation combination and the resulting
values are integrated over the length of the succession time step
(`successionTimestep` parameter) as:

```{=tex}
\begin{equation}
integratedSEP = 1-(1-estimatedSEP)^{e^{successionTimestep}}
(\#eq:SEPintegration)
\end{equation}
```
This is important, since seed establishment only occurs once at every
`P(sim)$successionTimestep`, and thus the probabilities of seed establishment
need to be temporally integrated to reflect the probability of a seed
establishing in this period of time.

Finally, since the *observed* species cover used to fit `coverModel` is a result
of both seed establishment and resprouting/clonal growth, the final species
establishment probabilities are calculated as a function of the temporally
integrated presence probabilities and species' probabilities of resprouting
(`resproutprob`, in the `species` table) (bounded between 0 and 1):

```{=tex}
\begin{equation}
SEP = integratedSEP * (1 - resproutprob)
(\#eq:SEPfinal)
\end{equation}
```
if $SEP > 1$, then:

```{=tex}
\begin{equation}
SEP = 1
(\#eq:SEPfinal2)
\end{equation}
```
if $SEP < 0$, then:

```{=tex}
\begin{equation}
SEP = 0
(\#eq:SEPfinal3)
\end{equation}
```
#### Ecolocation-specific parameter -- minimum relative biomass {#bboreal-minRelB}

Minimum relative biomass (`minRelativeB`) is a spatially-varying parameter used
to determine the shade level in each pixel. Each shade class (X0-X5) is defined
by a minimum relative biomass threshold compared to the pixel's current relative
biomass, which is calculated as the sum of pixel's total biomass divided by the
total potential maximum biomass in that pixel (the sum of all `maxB` for the
pixel's ecolocation).

Since we found no data to base the parametrisation of the shade class
thresholds, default values are based on publicly available values used in
LANDIS-II applications in Canada's boreal forests (available in [Dominic Cyr's
GitHub repository](https://github.com/dcyr/LANDIS-II_IA_generalUseFiles)), and
all ecolocations share the same values.

Initial runs revealed excessive recruitment of moderately shade-intolerant
species even as stand biomass increased, so values for shade levels X4 and X5
are adjusted downwards (X4: 0.8 to 0.75; X5: 0.90 to 0.85) to reflect higher
competition for resources (e.g. higher water limitation) in Western Canadian
forests with regards to Eastern Canadian forests [@MessierEtAl1998], which are
likely driven by higher moisture limitation in the west [@HoggEtAl2008;
@PengEtAl2011].

This adjustment can be bypassed by either supplying a `minRelativeB` table, or
an alternative function call to `P(sim)$minRelativeBFunction` (which by default
is `LandR::makeMinRelativeB`.

The minimum biomass threshold of a shade level of `X0` is `0` `standB`.

#### Calibrating species growth/mortality traits using *Biomass_speciesParameters* {#bboreal-B_sppParams-calib}

If using *Biomass_borealDataPrep* and *Biomass_speciesParameters*, the later
module calibrates several species traits that are first prepared by
*Biomass_borealDataPrep*:

-   `growthcurve`, `mortalityshape` -- which initially come from publicly
    available LANDIS-II tables;

-   `maxBiomass`, `maxANPP` -- which are estimated statistically by
    *Biomass_borealDataPrep* (see [Maximum biomass and maximum aboveground net
    primary productivity](#bboreal-maxB-maxANPP)).

Briefly, *Biomass_speciesParameters*:

1.  Uses \~41,000,000 hypothetical species' growth curves (generated with
    *Biomass_core*), that cover a fully factorial combination of longevity,
    ratio of `maxANPP` to `maxBiomass`, `growthcurve`, `mortalityshape`;

2.  Takes permanent and temporary sample plot (PSP) data in or near the study
    area for the target species, and finds which hypothetical species' growth
    curve most closely matches the growth curve observed in the PSP data -- on a
    species-by-species base. This gives us each species' `growthcurve`,
    `mortalityshape`, and a new species trait, `mANPPproportion`, a ratio of
    maximum aboveground net primary productivity (`maxANPP`) to maximum biomass
    (`maxBiomass`, not to be confounded with `maxB`) in the study area.

3.  Introduces a second new species trait, `inflationFactor`, and re-calibrates
    `maxB`. We recognize that `maxB`, as obtained empirically by
    *Biomass_borealDataPrep*, cannot be easily reached in simulations because
    all reasonable values of `growthcurve`, `mortalityshape` and `longevity`
    prevent the equation from reaching `maxB` (it acts as an asymptote that is
    never approached). The `inflationFactor` is calculated as the ratio of
    `maxBiomass` (the parameter used to generate theoretical growth curves in
    step 1) to the maximum biomass *actually* achieved by the theoretical growth
    curves (step 1). `maxB` is then recalibrated by multiplying it by
    `inflationFactor`. By doing this, resulting non-linear growth curves
    generated doing *Biomass_core* simulation will be able to achieve the the
    empirically estimated `maxB`.

4.  Estimates species-specific `maxANPP` by multiplying the final `maxB` above
    by `mANPPproportion` (estimated in step 2).

In cases where there is insufficient PSP data to perform the above steps, `maxB`
and `maxANPP` are left as estimated by *Biomass_borealDataPrep* (see [Maximum
biomass and maximum aboveground net primary
productivity](#bboreal-maxB-maxANPP)) and `inflationFactor` and
`mANPPproportion` take default values of 1 and 3.33.

### Aggregating species

*Biomass_borealDataPrep* will use the input table `sppEquiv` and the parameter
`P(sim)$sppEquivCol` to select the naming convention to use for the simulation
(see full list of [input objects](#bboreal-inputs-list) and
[parameters](#bboreal-params-list) for details). The user can use this table and
parameter to define groupings that "merge" similar species that have their own
invariant trait values (see [Invariant species
traits](#bboreal-invariant-traits)) (e.g. genus-level group or a functional
group). To do so, the name of the species group in `sppEquivCol` column of the
`sppEquiv` table must be identical for each grouped species.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:mergingSpp-Biomass-borealDataPrep)Example of species merging for simulation. Here the user wants to model (ref:Abie-bal), (ref:Abie-las) and (ref:Pinu-con) as separate species, but all (ref:Pice-sp) as a genus-level group. For this, all six species are identified in the 'KNN' column, so that their (ref:percent) cover layers can be obtained, but in the 'Boreal' column (which defines the naming convention used in the simulation in this example) all (ref:Pice-sp) have the same name. (ref:Biomass-borealDataPrep) will merge their (ref:percent) cover data into a single layer by summing their cover per pixel.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Species </th>
   <th style="text-align:left;"> KNN </th>
   <th style="text-align:left;"> Boreal </th>
   <th style="text-align:left;"> Modelled as </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-style: italic;"> Abies balsamea </td>
   <td style="text-align:left;"> Abie_Bal </td>
   <td style="text-align:left;"> Abie_Bal </td>
   <td style="text-align:left;font-style: italic;"> Abies balsamea </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Abies lasiocarpa </td>
   <td style="text-align:left;"> Abie_Las </td>
   <td style="text-align:left;"> Abie_Las </td>
   <td style="text-align:left;font-style: italic;"> Abies lasiocarpa </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Picea engelmannii x glauca </td>
   <td style="text-align:left;"> Pice_Eng_Gla </td>
   <td style="text-align:left;"> Pice_Spp </td>
   <td style="text-align:left;font-style: italic;"> Picea spp. </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Picea engelmannii x glauca </td>
   <td style="text-align:left;"> Pice_Eng_Gla </td>
   <td style="text-align:left;"> Pice_Spp </td>
   <td style="text-align:left;font-style: italic;"> Picea spp. </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Picea engelmannii </td>
   <td style="text-align:left;"> Pice_Eng </td>
   <td style="text-align:left;"> Pice_Spp </td>
   <td style="text-align:left;font-style: italic;"> Picea spp. </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Picea glauca </td>
   <td style="text-align:left;"> Pice_Gla </td>
   <td style="text-align:left;"> Pice_Spp </td>
   <td style="text-align:left;font-style: italic;"> Picea spp. </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Picea mariana </td>
   <td style="text-align:left;"> Pice_Mar </td>
   <td style="text-align:left;"> Pice_Spp </td>
   <td style="text-align:left;font-style: italic;"> Picea spp. </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Pinus contorta var. contorta </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Pinu_Con </td>
   <td style="text-align:left;font-style: italic;"> Pinus contorta var. contorta </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Pinus contorta </td>
   <td style="text-align:left;"> Pinu_Con </td>
   <td style="text-align:left;"> Pinu_Con </td>
   <td style="text-align:left;font-style: italic;"> Pinus contorta </td>
  </tr>
</tbody>
</table>

When groups contain species with different (invariant) trait values, the minimum
value across all species is used. As for the default species (ref:percent) cover
layers, *Biomass_borealDataPrep* proceeds in the same way as
*Biomass_speciesData* and sums cover across species of the same group per pixel.

### List of input objects {#bboreal-inputs-list}

Below are is the full lists of input objects (Table
\@ref(tab:moduleInputs2-Biomass-borealDataPrep)) that *Biomass_borealDataPrep*
expects.

The only inputs that **must** be provided (i.e., *Biomass_borealDataPrep* does
not have a default for) are `studyArea` (the study area used to simulate forest
dynamics *Biomass_core*) and `studyAreaLarge` (a potentially larger study area
used to derive parameter values -- e.g., species traits).

All other input objects and parameters have internal defaults.

Of these inputs, the following are particularly important and deserve special
attention:

**Spatial layers**

-   `ecoregionLayer` or `ecoregionRst` -- a shapefile or map containing
    ecological zones.

-   `rawBiomassMap` -- a map of observed stand biomass (in $g/m^2$).

-   `rstLCC` -- a land-cover raster.

-   `speciesLayers` -- layers of species (ref:percent) cover data. The species
    must match those available in default (or provided) species traits tables
    (the `species` and `speciesEcoregion` tables).

-   `standAgeMap` -- a map of observed stand ages (in years).

-   `studyArea` -- shapefile. A `SpatialPolygonsDataFrame` with a single polygon
    determining where the simulation will take place. This input object **must
    be supplied by the user**.

-   `studyAreaLarge` -- shapefile. A `SpatialPolygonsDataFrame` with a single
    polygon determining the where the statistical models for parameter
    estimation will be fitted. It **must** contain `studyArea` fully, if they
    are not identical. This object **must be supplied by the user**.

**Tables**

-   `speciesTable` -- a table of invariant species traits that must have the
    following columns (even if not all are necessary to the simulation):
    "species", "Area", "longevity", "sexualmature", "shadetolerance",
    "firetolerance", "seeddistance_eff", "seeddistance_max", "resproutprob",
    "resproutage_min", "resproutage_max", "postfireregen", "leaflongevity",
    "wooddecayrate", "mortalityshape", "growthcurve", "leafLignin", "hardsoft".
The order can vary but the column names must be identical. See @scheller2015
and *Biomass_core* manual for further detail about these columns.

\newpage
\blandscape

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs2-Biomass-borealDataPrep)List of (ref:Biomass-borealDataPrep) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> cloudFolderID </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> The google drive location where cloudCache will store large statistical objects </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> columnsForPixelGroups </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> The names of the columns in `cohortData` that define unique pixelGroups. Default is c('ecoregionGroup', 'speciesCode', 'age', 'B') </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregionLayer </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> A `SpatialPolygonsDataFrame` that characterizes the unique ecological regions (`ecoregionGroup`) used to parameterize the biomass, cover, and species establishment probability models. It will be overlaid with landcover to generate classes for every ecoregion/LCC combination. It must have same extent and crs as `studyAreaLarge`. It is superseded by `sim$ecoregionRst` if that object is supplied by the user </td>
   <td style="text-align:left;"> https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregionRst </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> A raster that characterizes the unique ecological regions used to parameterize the biomass, cover, and species establishment probability models. If this object is provided, it will supercede `sim$ecoregionLayer`. It will be overlaid with landcover to generate classes for every ecoregion/LCC combination. It must have same extent and crs as `rasterToMatchLarge` if supplied by user - use `reproducible::postProcess`. If it uses an attribute table, it must contain the field 'ecoregion' to represent raster values </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> firePerimeters </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Fire perimeters raster, with fire year information used to 'update' stand age using time since last fire as the imputed value. Only used if `P(sim)$overrideAgeInFires = TRUE`. Biomass will also be updated in these pixels if `P(sim)$overrideBiomassInFires = TRUE` and the last fire was later than 1985. Defaults to using fire perimeters in the Canadian National Fire Database, downloaded as a zipped shapefile with fire polygons, an attribute (i.e., a column) named 'YEAR', which is used to rasterize to the study area. </td>
   <td style="text-align:left;"> https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstLCC </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> A land classification map in study area. It must be 'corrected', in the sense that: 1) Every class must not conflict with any other map in this module (e.g., `speciesLayers` should not have data in LCC classes that are non-treed); 2) It can have treed and non-treed classes. The non-treed will be removed within this module if `P(sim)$omitNonTreedPixels` is `TRUE`; 3) It can have transient pixels, such as 'young fire'. These will be converted to a the nearest non-transient class, probabilistically if there is more than 1 nearest neighbour class, based on `P(sim)$LCCClassesToReplaceNN`. The default layer used, if not supplied, is Canada national land classification in 2010. The metadata (res, proj, ext, origin) need to match `rasterToMatchLarge`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> A raster of the `studyArea` in the same resolution and projection as `rawBiomassMap`. This is the scale used for all outputs for use in the simulation. If not supplied will be forced to match the default `rawBiomassMap`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatchLarge </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> A raster of the `studyAreaLarge` in the same resolution and projection as `rawBiomassMap`. This is the scale used for all inputs for use in the simulation. If not supplied will be forced to match the default `rawBiomassMap`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rawBiomassMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> total biomass raster layer in study area. Defaults to the Canadian Forestry Service, National Forest Inventory, kNN-derived total aboveground biomass map from 2001 (in tonnes/ha), unless 'dataYear' != 2001. See https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata. </td>
   <td style="text-align:left;"> http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/NFI_MODIS250m_2001_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesLayers </td>
   <td style="text-align:left;"> RasterStack </td>
   <td style="text-align:left;"> cover percentage raster layers by species in Canada species map. Defaults to the Canadian Forestry Service, National Forest Inventory, kNN-derived species cover maps from 2001 using a cover threshold of 10 - see https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata </td>
   <td style="text-align:left;"> http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesTable </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> a table of invariant species traits with the following trait colums: 'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance', 'firetolerance', 'seeddistance_eff', 'seeddistance_max', 'resproutprob', 'resproutage_min', 'resproutage_max', 'postfireregen', 'leaflongevity', 'wooddecayrate', 'mortalityshape', 'growthcurve', 'leafLignin', 'hardsoft'. Names can differ, but not the column order. Default is from Dominic Cyr and Yan Boulanger's project. </td>
   <td style="text-align:left;"> https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppColorVect </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> named character vector of hex colour codes corresponding to each species </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquiv </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table of species equivalencies. See `?LandR::sppEquivalencies_CA`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppNameVector </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> an optional vector of species names to be pulled from `sppEquiv`. Species names must match `P(sim)$sppEquivCol` column in `sppEquiv`. If not provided, then species will be taken from the entire `P(sim)$sppEquivCol` column in `sppEquiv`. See `LandR::sppEquivalencies_CA`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> standAgeMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> stand age map in study area. Must have a 'imputedPixID' attribute (a vector of pixel IDs) indicating which pixels suffered age imputation. If no pixel ages were imputed, please set this attribute to `integer(0)`. Defaults to the Canadian Forestry Service, National Forest Inventory, kNN-derived biomass map from 2001, unless 'dataYear' != 2001. See https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata </td>
   <td style="text-align:left;"> http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> Polygon to use as the study area. Must be supplied by the user. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaLarge </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> multipolygon (potentially larger than `studyArea`) used for parameter estimation, Must be supplied by the user. If larger than `studyArea`, it must fully contain it. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

\elandscape

### List of parameters {#bboreal-params-list}

Table \@ref(tab:moduleParams2-Biomass-borealDataPrep) lists all parameters used
in *Biomass_borealDataPrep* and their detailed information. All have default
values specified in the module's metadata.

Of these parameters, the following are particularly important:

**Estimation of simulation parameters**

-   `biomassModel` -- the statistical model (as a function call) used to
    estimate `maxB` and `maxANPP`.

-   `coverModel` -- the statistical model (as a function call) used to estimate
    SEP.

-   `fixModelBiomass` -- determines whether `biomassModel` is re-fit when
    convergence issues arise.

-   `imputeBadAgeModel` -- model used to impute ages when they are missing, or
    do not match the input cover and biomass data. Not to be confounded with
    correcting ages from fire data

-   `subsetDataAgeModel` and `subsetDataBiomassModel` -- control data
    sub-sampling for fitting the `imputeBadAgeModel` and `biomassModel`,
    respectively

-   `exportModels` -- controls whether `biomassModel` or `coverModel` (or both)
    are to be exported in the simulation `simList`, which can be useful to
    inspect the fitted models and report on statistical fit.

-   `sppEquivCol` -- character. the column name in the `speciesEquivalency`
    data.table that defines the naming convention to use throughout the
    simulation.

**Data processing**

-   `forestedLCCClasses` and `LCCClassesToReplaceNN` -- define which land-cover
    classes in `rstLCC` are forested and which should be reclassified to
    forested classes, respectively.

-   `deciduousCoverDiscount`, `coverPctToBiomassPctModel` and
    `fitDeciduousCoverDiscount` -- the first is the adjustment factor for
    broadleaf species cover to biomass relationships; the second and third are
the model used to refit `deciduousCoverDiscount` in the supplied
`studyAreaLarge` and whether refitting should be attempted (respectively).

\newpage
\blandscape

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams2-Biomass-borealDataPrep)List of (ref:Biomass-borealDataPrep) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> biomassModel </td>
   <td style="text-align:left;"> call </td>
   <td style="text-align:left;"> lme4::lm.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Model and formula for estimating biomass (B) from `ecoregionGroup` (currently `ecoregionLayer` LandCoverClass), `speciesCode`, `logAge` (gives a downward curving relationship), and `cover`. Defaults to a LMEM, which can be slow if dealing with very large datasets (e.g. 36 000 points take 20min). For faster fitting try `P(sim)$subsetDataBiomassModel == TRUE`, or `quote(RcppArmadillo::fastLm(formula = B ~ logAge speciesCode ecoregionGroup + cover speciesCode ecoregionGroup))`. A custom model call can also be provided, as long as the 'data' argument is NOT included. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> coverModel </td>
   <td style="text-align:left;"> call </td>
   <td style="text-align:left;"> glm, cbi.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Model and formula used for estimating cover from `ecoregionGroup` and `speciesCode` and potentially others. Defaults to a GLMEM if there are &gt; 1 grouping levels. A custom model call can also be provided, as long as the 'data' argument is NOT included </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fixModelBiomass </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> should `biomassModel` be fixed in the case of non-convergence? Only scaling of variables and attempting to fit with a new optimizer (bobyqa, see `?lme4`) are implemented at this time. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> subsetDataAttempts </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> How many times should `biomassModel` be attempted to fit with a new data subset in case of non-convergence? Each time, the data is resampled (if `subsetDataBiomassModel = TRUE`) and the model re-fit with the original data, scaled variables and/or a different optimizer if `fixModelBiomass = TRUE`. Model refiting with original data, rescaled variables and/or a new optimizer occurs up to three times for each data subset, regardless of this parameter's value. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> subsetDataBiomassModel </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> the number of samples to use when subsampling the biomass data model (`biomassModel`); Can be `TRUE`/`FALSE`/`NULL` or numeric; if `TRUE`, uses 50, the default. If `FALSE`/`NULL` no subsetting is done. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> coverPctToBiomassPctModel </td>
   <td style="text-align:left;"> call </td>
   <td style="text-align:left;"> glm, I(l.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Model to estimate the relationship between % cover and % biomass, referred to as `P(sim)$fitDeciduousCoverDiscount` It is a number between 0 and 1 that translates % cover, as provided in several databases, to % biomass. It is assumed that all hardwoods are equivalent and all softwoods are equivalent and that % cover of hardwoods will be an overesimate of the % biomass of hardwoods. E.g., 30% cover of hardwoods might translate to 20% biomass of hardwoods. The reason this discount exists is because hardwoods in Canada have a much wider canopy than softwoods. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> deciduousCoverDiscount </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.8418911 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This was estimated with data from NWT on March 18, 2020 and may or may not be universal. Will not be used if `P(sim)$fitDeciduousCoverDiscount == TRUE` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fitDeciduousCoverDiscount </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> If TRUE, this will re-estimate `P(sim)$fitDeciduousCoverDiscount` This may be unstable and is not recommended currently. If `FALSE`, will use the current default </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dataYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2001 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Used to override the default 'sourceURL' of KNN datasets (species cover, stand biomass and stand age), which point to 2001 data, to fetch KNN data for another year. Currently, the only other possible year is 2011. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregionLayerField </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> the name of the field used to distinguish ecoregions, if supplying a polygon. Defaults to `NULL` and tries to use 'ECODISTRIC' where available (for legacy reasons), or the row numbers of `sim$ecoregionLayer`. If this field is not numeric, it will be coerced to numeric. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> exportModels </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Controls whether models used to estimate maximum B/ANPP (`biomassModel`) and species establishment (`coverModel`) probabilities are exported for posterior analyses or not. This may be important when models fail to converge or hit singularity (but can still be used to make predictions) and the user wants to investigate them further. Can be set to 'none' (no models are exported), 'all' (both are exported), 'biomassModel' or 'coverModel'. BEWARE: because this is intended for posterior model inspection, the models will be exported with data, which may mean very large simList(s)! </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forestedLCCClasses </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1, 2, 3,.... </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The classes in the `rstLCC` layer that are 'treed' and will therefore be run in Biomass_core. Defaults to forested classes in LCC2010 map. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imputeBadAgeModel </td>
   <td style="text-align:left;"> call </td>
   <td style="text-align:left;"> lme4::lm.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Model and formula used for imputing ages that are either missing or do not match well with biomass or cover. Specifically, if biomass or cover is 0, but age is not, or if age is missing (`NA`), then age will be imputed. Note that this is independent from replacing ages inside fire perimeters (see `P(sim)$overrideAgeInFires`) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LCCClassesToReplaceNN </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This will replace these classes on the landscape with the closest forest class `P(sim)$forestedLCCClasses`. If the user is using the LCC 2005 land-cover data product for `rstLCC`, then they may wish to include 36 (cities -- if running a historic range of variation project), and 34:35 (burns) Since this is about estimating parameters for growth, it doesn't make any sense to have unique estimates for transient classes in most cases. If no classes are to be replaced, pass `'LCCClassesToReplaceNN' = numeric(0)` when supplying parameters. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minCoverThreshold </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Pixels with total cover that is equal to or below this number will be omitted from the dataset </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minRelativeBFunction </td>
   <td style="text-align:left;"> call </td>
   <td style="text-align:left;"> LandR::m.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> A quoted function that makes the table of min. relative B determining a stand shade level for each ecoregionGroup. Using the internal object `pixelCohortData` is advisable to access/use the list of `ecoregionGroups` per pixel. The function must output a `data.frame` with 6 columns, named `ecoregionGroup` and 'X1' to 'X5', with one line per `ecoregionGroup` code, and the min. relative biomass for each stand shade level X1-5. The default function uses values from LANDIS-II available at: https://github.com/dcyr/LANDIS-II_IA_generalUseFiles/blob/master/LandisInputs/BSW/biomass-succession-main-inputs_BSW_Baseline.txt%7E. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> omitNonTreedPixels </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> Should this module use only treed pixels, as identified by `P(sim)$forestedLCCClasses`? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> overrideAgeInFires </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> should stand age values inside fire perimeters be replaced with number of years since last fire? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> overrideBiomassInFires </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> should B values be re-estimated using Biomass_core for pixels within the fire perimeters for which age was replaced with time since last fire? Ignored if `P(sim)$overrideAgeInFires = FALSE`. See `firePerimeters` input object and `P(sim)$overrideAgeInFires` for further detail. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupAgeClass </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> When assigning `pixelGroup` membership, this defines the resolution of ages that will be considered 'the same pixelGroup', e.g., if it is 10, then 6 and 14 will be the same </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupBiomassClass </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> When assigning pixelGroup membership, this defines the resolution of biomass that will be considered 'the same pixelGroup', e.g., if it is 100, then 5160 and 5240 will be the same </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rmImputedPix </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should `sim$imputedPixID` be removed from the simulation? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesUpdateFunction </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> LandR::s.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Unnamed list of (one or more) quoted functions that updates species table to customize values. By default, `LandR::speciesTableUpdate` is used to change longevity and shade tolerance values, using values appropriate to Boreal Shield West (BSW), Boreal Plains (BP) and Montane Cordillera (MC) ecoprovinces (see `?LandR::speciesTableUpdate` for details). Set to `NULL` if default trait values from `speciesTable` are to be kept instead. The user can supply other or additional functions to change trait values (see `LandR::updateSpeciesTable`) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquivCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Boreal </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The column in `sim$speciesEquivalency` data.table to use as a naming convention. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesTableAreas </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> BSW, BP, MC </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> One or more of the Ecoprovince short forms that are in the `speciesTable` file, e.g., BSW, MC etc. Default is good for Alberta and other places in the western Canadian boreal forests. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> subsetDataAgeModel </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> the number of samples to use when subsampling the age data model and when fitting `coverPctToBiomassPctModel`; Can be `TRUE`/`FALSE`/`NULL` or numeric; if `TRUE`, uses 50, the default. If `FALSE`/`NULL` no subsetting is done. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> successionTimestep </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> defines the simulation time step, default is 10 years </td>
  </tr>
  <tr>
   <td style="text-align:left;"> useCloudCacheForStats </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Some of the statistical models take long (at least 30 minutes, likely longer). If this is `TRUE`, then it will try to get previous cached runs from googledrive. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vegLeadingProportion </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.8 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> a number that defines whether a species is leading for a given pixel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This is here for backwards compatibility. Please use `.plots` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the type of 'plotting' to do. See `?Plots` for possible types. To omit, set to NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between plot events </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time at which the first save event should occur </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .seed </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Named list of seeds to use for each event (names). E.g., `list('init' = 123)` will `set.seed(123)` at the start of the init event and unset it at the end. Defaults to `NULL`, meaning that no seeds will be set </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .sslVerify </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 64 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `httr::config(ssl_verifypeer = P(sim)$.sslVerify)` when downloading KNN (NFI) datasets. Set to 0L if necessary to bypass checking the SSL certificate (this may be necessary when NFI's website SSL certificate is not correctly configured). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used. If `NA`, a hash of studyArea will be used. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> .inputOb.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Internal. Can be names of events or the whole module name; these will be cached by SpaDES </td>
  </tr>
</tbody>
</table>

\elandscape

### List of outputs {#bboreal-outputs-list}

The module produces the following outputs (Table
\@ref(tab:moduleOutputs-Biomass-borealDataPrep)), which are key inputs of
*Biomass_core*.

**Tables**

-   `cohortData` -- initial community table, containing corrected biomass
    (g/m2), age and species cover data, as well as ecolocation and `pixelGroup`
    information. This table defines the initial community composition and
    structure used by `Biomass_core`.

-   `species` -- table of invariant species traits. Will contain the same traits
    as in `speciesTable` above, but adjusted where necessary.

-   `speciesEcoregion` -- table of spatially-varying species traits (`maxB`,
    `maxANPP`, SEP).

-   `minRelativeB` -- minimum relative biomass thresholds that determine a shade
    level in each pixel. X0-5 represent site shade classes from no-shade (0) to
    maximum shade (5).

-   `sufficientLight` -- probability of germination for species shade tolerance
    (in `species`) and shade level`(defined by`minRelativeB\`)

**Spatial layers**

-   `biomassMap` -- map of initial stand biomass values after adjustments for
    data mismatches.

-   `pixelGroupMap` -- a map containing `pixelGroup` IDs per pixel. This defines
    the initial map used for hashing within `Biomass_core`, in conjunction with
    `cohortData`.

-   `ecoregionMap` -- map of ecolocations.

\newpage
\blandscape

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-Biomass-borealDataPrep)List of (ref:Biomass-borealDataPrep) output objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> biomassMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> total biomass raster layer in study area, filtered for pixels covered by cohortData. Units in g/m2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> initial community table, containing corrected biomass (g/m2), age and species cover data, as well as ecolocation and `pixelGroup` information. This table defines the initial community composition and structure used by `Biomass_core` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregion </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> `ecoregionGroup` look up table </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecoregionMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> `ecoregionGroup` map that has mapcodes match ecoregion table and `speciesEcoregion` table </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imputedPixID </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> A vector of pixel IDs - matching rasterMatch IDs - that suffered data imputation. Data imputation may be in age (to match last fire event post 1950s, or 0 cover), biomass (to match fire-related imputed ages, correct for missing values or for 0 age/cover), land cover (to convert non-forested classes into to nearest forested class) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> initial community map that has mapcodes (`pixelGroup` IDs) match `cohortData` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelFateDT </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> A small table that keeps track of the pixel removals and cause. This may help diagnose issues related to understanding the creation of `cohortData` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minRelativeB </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> minimum relative biomass thresholds that determine a shade level in each pixel. X0-5 represent site shade classes from no-shade (0) to maximum shade (5). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modelCover </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> If `P(sim)$exportModels` is 'all', or 'cover', fitted cover model, as defined by `P(sim)$coverModel`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modelBiomass </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> If `P(sim)$exportModels` is 'all', or 'biomass', fitted biomass model, as defined by `P(sim)$biomassModel` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rawBiomassMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> total biomass raster layer in study area. Defaults to the Canadian Forestry Service, National Forest Inventory, kNN-derived total aboveground biomass map (in tonnes/ha) from 2001, unless 'dataYear' != 2001. See https://open.canada.ca/data/en/dataset/ ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata </td>
  </tr>
  <tr>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> a table that of invariant species traits. Will have the same traits as the input `speciesTable` with values adjusted where necessary </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesEcoregion </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table of spatially-varying species traits (`maxB`, `maxANPP`, `establishprob`), defined by species and `ecoregionGroup`) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> Polygon to use as the study area corrected for any spatial properties' mismatches with respect to `studyAreaLarge`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sufficientLight </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Probability of germination for species shade tolerance (in `species`) and shade level`(defined by `minRelativeB`) combinations. Table values follow LANDIS-II test traits available at: https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt </td>
  </tr>
</tbody>
</table>

\elandscape

### Simulation flow and module events {#bboreal-sim-flow}

*Biomass_borealDataPrep* initialises itself and prepares all inputs, provided it
has internet access to retrieve the raw datasets, for parametrisation and use by
*Biomass_core*.

The module runs only for one time step and contains The general flow of
*Biomass_borealDataPrep* processes is:

1.  Preparation of all necessary data and input objects that do not require
    parameter fitting (e.g., invariant species traits table, creating
    ecolocations);

2.  Fixing mismatched between raw cover, biomass and age data;

3.  Imputing age values in pixels where mismatches exist or age data is missing;

4.  Construction of an initial `data.table` of cohort biomass and age per pixel
    (with ecolocation information);

5.  Sub-setting pixels in forested land-cover classes and (optional) converting
    transient land-cover classes to forested classes;

6.  Fitting `coverModel`;

7.  Fitting `biomassModel` (and re-fitting if necessary -- optional);

8.  Estimating `maxB`, `maxANPP` and SEP per species and ecolocation.

9.  (OPTIONAL) Correcting ages in pixels inside fire perimeters and reassigning
    biomass.

[steps 1-9 are part of the `init` event. Before step 1, the data is downloaded
when during the run of the `.inputObjects` function]

10. (OPTIONAL) Plots of `maxB`, `maxANPP` and SEP maps (`plot` event);

11. (OPTIONAL) Save outputs (`save` event)

## Usage example {#bboreal-example}

This module can be run stand-alone, but it won't do much more than prepare
inputs for `Biomass_core`. Hence, we provide a usage example of this module and
a few others in [this
repository](https://github.com/CeresBarros/LandRBiomass_publication) and in
@BarrosEtAlaccepted.

## References {#bboreal-refs}
