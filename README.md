<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.project/actions)
<!-- badges: end -->

<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/master/man/figures/SpaDES.png">

# SpaDES.project

Quickly setup 'SpaDES' project directories and get modules.

See package vignettes to get started.

1. [Getting started vignette](vignettes/i-getting-started.Rmd)
2. [Managing large SpaDES projects](vignettes/ii-managing-large-SpaDES-projects.Rmd)
3. [Using `git` and GitHub](vignettes/iii-using-git-github.Rmd)

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
