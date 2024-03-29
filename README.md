<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.project/actions)
<!-- badges: end -->

<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/master/man/figures/SpaDES.png">

# SpaDES.project

Quickly setup 'SpaDES' project directories and add modules using templates.

See package vignettes to get started.

1. [Getting started vignette](vignettes/i-getting-started.Rmd)
2. [Managing large SpaDES projects](vignettes/ii-managing-large-SpaDES-projects.Rmd)
3. [Using `git` and GitHub](vignettes/iii-using-git-github.Rmd)
4. [Using LandR and fireSense](vignettes/iv-LandR-fireSense.Rmd)

Project types:

- basic
- advanced
- LandR-fS

**Website:** [https://SpaDES.PredictiveEcology.org](https://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

### Current stable release

[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/PredictiveEcology/SpaDES.project/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.project/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.project?branch=master)

**Install from CRAN:**

```r
install.packages("SpaDES.project")
```

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.project", dependencies = TRUE) # master
```

### Development version (unstable)

[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg?branch=development)](https://github.com/PredictiveEcology/SpaDES.project/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.project/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.project?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.project", ref = "development", dependencies = TRUE)
```

### Create a new project with several SpaDES modules

This will create a new RStudio project, download 4 modules from their respective GitHub.com repositories, create a
control script (e.g., `global.R`) and finally open that new project in RStudio.
The resulting control script that will technically run, but one or more of the modules will likely be missing some input object that are expected.
Most likely this will be something like an object called `studyArea`, which is a polygon with the study area.
That control script has numerous comments within it to help get started.
It will also deal with installing R packages.

```r
SpaDES.project::newProject("FactorialTesting", 
                           path = "tempdir()", 
                           modules = c("PredictiveEcology/Biomass_speciesFactorial",
                                       "PredictiveEcology/Biomass_speciesParameters@EliotTweaks",
                                       "PredictiveEcology/Biomass_borealDataPrep@development",
                                       "PredictiveEcology/Biomass_core@EliotTweaks"), 
                           overwrite = FALSE)
```

## Contributions

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for information on how to contribute to this project.
