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

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.project", dependencies = TRUE) 
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

### Get modules in a new project 

This is a lightweight way to get SpaDES modules

```r
SpaDES.project::getModule( modulePath = tempdir(), 
                           modules = c("PredictiveEcology/Biomass_speciesFactorial",
                                       "PredictiveEcology/Biomass_speciesParameters@development",
                                       "PredictiveEcology/Biomass_borealDataPrep@development",
                                       "PredictiveEcology/Biomass_core@development"), 
                           overwrite = FALSE)
```

## Complete workflow following the PERFICT approach

A complete workflow requires that all steps to make all components work must be part of the script
without any "manual" interventions. The code below will setup up a project in its own
folder, install 4 SpaDES modules, the install all necessary R packages to run those modules.

```

# project basics ------------------------------------------------------------------------------

prjPath <- "/path/to/project"

options(repos = c(CRAN = "https://cloud.r-project.org"))

# install and load packages -------------------------------------------------------------------

pkgPath <- file.path(prjPath, "packages", version$platform,
                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))
                     dir.create(pkgPath, recursive = TRUE)

# Project will use a stand alone R package directory                    
.libPaths(pkgPath, include.site = FALSE) ## install packages in project library (proj-lib)

if (!"remotes" %in% rownames(installed.packages())) {
  install.packages("remotes")
}

if (!"Require" %in% rownames(installed.packages())) {
  remotes::install_github("PredictiveEcology/Require@archivedPkg", upgrade = FALSE)
}

## use binary linux packages if on Ubuntu
Require::setLinuxBinaryRepo()

Require::Require(c("PredictiveEcology/SpaDES.project@transition"), 
                 standAlone = TRUE, 
                 require = FALSE) # don't load packages

modulePath <- file.path(prjPath, "modules")

## Get SpaDES modules; here using known modules on GitHub.com
SpaDES.project::getModule( modulePath = modulePath, 
                           modules = c("PredictiveEcology/Biomass_speciesFactorial",
                                       "PredictiveEcology/Biomass_speciesParameters@development",
                                       "PredictiveEcology/Biomass_borealDataPrep@development",
                                       "PredictiveEcology/Biomass_core@development"), 
                           overwrite = FALSE)


outs <- SpaDES.project::packagesInModules(modulePath = modulePath)  ## gets list of module dependencies
Require::Require(c(unname(unlist(outs)), "SpaDES.core"),
                 require = FALSE,   ## don't load packages
                 standAlone = TRUE) ## install all dependencies in proj-lib (ignore user/system lib)
```
The next steps after this would be to start using SpaDES.core


## Contributions

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for information on how to contribute to this project.
