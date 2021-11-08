<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.project/actions)
<!-- badges: end -->

<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# SpaDES.project

Quickly setup 'SpaDES' project directories and add modules using templates.

See package vignettes to get started.

1. Getting started vignette
2. Managing large SpaDES projects
3. Using `git` and GitHub
4. Using LandR and fireSense

Project types:

- LandR-fS

**Website:** [https://SpaDES.PredictiveEcology.org](https://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

### Current stable release

[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/PredictiveEcology/SpaDES.project/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.project/branch/master/graph/badge.svg)](https://codecov.io/gh/PredictiveEcology/SpaDES.project?branch=master)

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
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.project/branch/development/graph/badge.svg)](https://codecov.io/gh/PredictiveEcology/SpaDES.project?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.project", ref = "development", dependencies = TRUE)
```

## Contributions

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for information on how to contribute to this project.
