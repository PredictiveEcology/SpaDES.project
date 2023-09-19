<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.project/actions)
<!-- badges: end -->

<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/master/man/figures/SpaDES.png">

# SpaDES.project

Quickly setup 'SpaDES' project directories, get modules, and deal with a number of issues related to reproducibility and reusabililty. This package was designed with a PERFICT approach in mind (See [McIntire et al. 2022](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.13994)).

# Why use this package

First, **PERFICT** is difficult to achieve, but is facilitated by `SpaDES` -- `SpaDES.project` helps set up a `SpaDES` project. In addition to `R`, the scripting language, there are 3 "tools" that are very helpful: 

- an **IDE** that encourages the use of *projects*, 
- a **modular coding** approach, 
- *if the user would like to "develop" code*, **version control** (optional),

We use [Posit](https://posit.co), [`SpaDES`](https://spades.predictiveecology.org) and [Github](https://github.com), respectivly. While these are not the only options, we find them the easiest. We note that many users are not git users. We have conceived of `SpaDES.project` for these people also: a user can use a git-controlled project or not within the `SpaDES.project` setup. 

In addition to these tools, we have repeatedly hit many other issues throughout our years of experience wrangling projects with a diversity of developers, on a diversity of operating systems, with a diversity of users, data sources, packages, modules etc. These are the "costs" of open, modular, interoperable projects: *normal reproducible workflows will fail as the project complexity increases*. If one of the objectives is to build projects that are transferable to other users, it is unlikely to work with "normal workflows" because things like:

- different `.Rprofile` files, 
- paths that are not transferable, 
- packages that do not work identically across operating systems, 
- package version conflicts
- package loading/installing order problems (can't install a different version of a package, while it is already loaded),
- spaghetti code (i.e., where an object is defined in one file and used in another file),
- whether the user knows how to use github or not, 
cryptic code and objects ("just run that line, don't worry about what it does"), 
- objects by a user being defined in the `.GlobalEnv` resulting in code written by a developer not failing when it *should* fail,
- and more generally, because different users have different competencies,

Because of these and other factors, it is not sufficient simply to build a "reproducible" script: it must be a *reusable* script that works verbatim on any machine, any operating system, for any user. 

A user can, of course, attempt to wrangle all these issues themselves; instead, we built `SpaDES.project` that was built from our most complex projects to date, yet conceived with the beginner in mind, while anticipating these issues so they don't come to haunt the users later.

# Getting started

See [this package readme](https://htmlpreview.github.io/?https://raw.githubusercontent.com/PredictiveEcology/SpaDES.project/transition/docs/index.html) and vignettes to get started.

1. [Getting started vignette](vignettes/i-getting-started.Rmd)
2. [Using `git` and GitHub](vignettes/iii-using-git-github.Rmd)
3. [Managing large SpaDES projects](vignettes/iv-Installing-R.Rmd)

**Website:** [https://SpaDES.PredictiveEcology.org](https://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

### Current stable release

[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/PredictiveEcology/SpaDES.project/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.project/branch/main/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.project?branch=main)

**Install from CRAN:**

```r
# Not yet on CRAN
# install.packages("SpaDES.project")
```

### Development version (unstable)

[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg?branch=development)](https://github.com/PredictiveEcology/SpaDES.project/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.project/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.project?branch=development)

```r
install.packages("SpaDES.project", repos = "predictiveecology.r-universe.dev")
```

### Get modules in a new project 


```r
setupProject(paths = list(projectPath = tempdir()),
             modules = c("PredictiveEcology/Biomass_borealDataPrep@development",
                         "PredictiveEcology/Biomass_core@development"))
```

## Complete workflow following the PERFICT approach

A complete workflow requires that all steps to make all components work must be part of the script
without any "manual" interventions. The code below will setup up a project in its own
folder, install 4 SpaDES modules, the install all necessary R packages to run those modules.

```

# project basics ------------------------------------------------------------------------------
prjPath <- file.path(tempdir(), "testProject")

# install and load packages -------------------------------------------------------------------
# installs in user library; in following setupProject call, these will be copied to project library
while (!require("SpaDES.project", quietly = TRUE)) 
  install.packages("SpaDES.project", repos = "predictiveecology.r-universe.dev")
  
## Get SpaDES modules; here using known modules on GitHub.com
spOut <- 
  setupProject(paths = list(projectPath = prjPath),
               standAlone = TRUE,        # will put R packages in separate location
               packages = c("googledrive", "RCurl", "XML",  # needed inside Biomass_borealDataPrep
                            "PredictiveEcology/reproducible@development (>= 1.2.16.9018)"), # minor update
               modules = c("PredictiveEcology/Biomass_speciesFactorial",
                           "PredictiveEcology/Biomass_speciesParameters@development",
                           "PredictiveEcology/Biomass_borealDataPrep@development",
                           "PredictiveEcology/Biomass_core@development"))
                           
                           
spOut$objects$studyArea <- LandR::randomStudyArea(center = NULL, size = 1e9, seed = 32)
spOut$objects$studyAreaLarge <- LandR::randomStudyArea(center = NULL, size = 2e9, seed = 32)
spOut$params = list(.globals = list(sppEquivCol = 'Boreal')) # what species naming convention
if (require("raster", quietly = TRUE) && interactive()) {
  raster::plot(spOut$objects$studyAreaLarge)
  raster::plot(spOut$objects$studyArea, add = TRUE)
}

sim <- do.call(simInit, spOut)
sim$speciesLayers <- raster::dropLayer(sim$speciesLayers, i = c("Pice_Spp", "Pinu_Spp"))
so <- spades(sim)
```



## Contributions

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for information on how to contribute to this project.
