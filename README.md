<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SpaDES.project)](https://cran.r-project.org/package=SpaDES.project)
[![R build status](https://github.com/PredictiveEcology/SpaDES.project/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.project/actions)
<!-- badges: end -->

<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/master/man/figures/SpaDES.png">

# SpaDES.project

Quickly setup 'SpaDES' project directories, get modules, and deal with a number of issues related to reproducibility and reusabililty. This package was designed with a PERFICT approach in mind (See [McIntire et al. 2022](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.13994)).

# Why Choose This Package

Achieving **PERFICT** in your projects can be challenging, but it becomes more accessible with the use of `SpaDES`. The `SpaDES.project` tool is designed to streamline the setup of `SpaDES` projects. Complementing the `R` scripting language, there are three valuable tools at your disposal: 

1. **Integrated Development Environment (IDE)**: This IDE encourages the organization of your work into projects, enhancing your workflow efficiency. 
2. **Modular Coding Approach**: Embrace a modular approach to coding, allowing for easier maintenance and collaboration. 
3. **Version Control (Optional)**: For those interested in code development, `SpaDES` supports version control through platforms like [GitHub](https://github.com), giving you control over project history and collaboration.

## Our Recommended Tools

Our preferred toolset includes [Posit](https://posit.co) (specifically, `RStudio`), [`SpaDES`](https://spades.predictiveecology.org), and [GitHub](https://github.com). While these are our recommendations, there are alternative options available. We acknowledge that not all users are familiar with Git, and that's perfectly acceptable. `SpaDES.project` has been designed to cater to all users, whether they choose to use a Git-controlled project or not.

## Project Challenges

Beyond these tools, our extensive experience in managing projects with diverse developers, operating systems, users, data sources, and packages has revealed various challenges. These challenges arise due to the nature of open, modular, and interoperable projects. As project complexity increases, typical reproducible workflows may falter. Issues include:

- Variations in `.Rprofile` files.
- Non-transferable file paths.
- Incompatibilities between packages on different operating systems.
- Conflicts between package versions.
- Problems with the order of package loading and installation (e.g., inability to install a different version of a package while it's already loaded).
- Spaghetti code, where objects are defined in one file and used in another.
- Differences in users' familiarity with GitHub.
- The presence of cryptic code and objects ("just run that line, don't worry about what it does").
- Objects defined by a user lingering in the `.GlobalEnv`, leading to undetected issues.
- Varying competencies among different users.  

Given these complexities, it's not enough to create a "reproducible" script; it must be a "reusable" script that functions flawlessly on any machine, operating system, and for any user.   

Users can certainly attempt to address these issues individually, but we've developed `SpaDES.project` as a solution. It's derived from our most intricate projects to date, yet it's designed with beginners in mind. We've anticipated these challenges so that users won't encounter them unexpectedly during their project journeys.  

# Getting Started

See [this package readme](https://htmlpreview.github.io/?https://raw.githubusercontent.com/PredictiveEcology/SpaDES.project/transition/docs/index.html) and vignettes to get started.

1. [Getting started vignette](articles/i-getting-started.html)
2. [Using `git` and GitHub](articles/iii-using-git-github.html)
3. [Installing R](articles/iv-Installing-R.html)
4. [Finding other SpaDES modules](articles/v-finding-other-modules.html)

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
install.packages("SpaDES.project", repos = c("predictiveecology.r-universe.dev", getOption("repos")))
```

### Get modules in a new project 


```r
setupProject(paths = list(projectPath = tempdir()),
             modules = c("PredictiveEcology/Biomass_borealDataPrep@development",
                         "PredictiveEcology/Biomass_core@development"))
```

### More examples

The following example provides all that is necessary to run all modules, in a very short set of commands, from (almost) any starting condition. We tend to use this approach is many of our projects.

Key features:

1. The project is fully self contained in a folder, with packages being installed to a unique library based on the `projectPath`.
1. No use of `install.packages` without an `if`
2. No `library` or `require` calls before any package installations.
2. All functions are "rerun-capable", meaning they do not "redo" the action when rerun if the action is not necessary. e.g., `remotes::install_github` does not reinstall the package if the SHA has not changed.
3. No assigning of objects into the `.GlobalEnv`. If projects are small/simple, using the `.GlobalEnv` is OK. As a project gets larger, unexpected behaviours arise because objects can be erroneaously found in the `.GlobalEnv` if they have a common name, like `out`, when functions should instead fail.
4. Minimum packages installed prior to `setupProject`. After `SpaDES.project` and `Require` updates are on CRAN, this will be further simplified.
5. Minimal use of `setwd`, to allow user's to easily change it (including comment it out) with simple search image.

It is fully contained as a separate project, separate libraries, which means it doesn't do what it did for me at the start... package collisions.
I used a setwd("~"), which is the bare minimum ... you can tell a user to change that to a place where this project will live.


```
getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("remotes")
remotes::install_github("PredictiveEcology/Require", ref = "bda9fa50003981880c06287fea1db9272b62912c", upgrade = FALSE)# getOrUpdatePkg("reproducible", "2.0.9")
getOrUpdatePkg("SpaDES.project", "0.0.8.9040")

setwd("~")
out <- SpaDES.project::setupProject(
  runName = "Example",
  paths = list(projectPath = "integratingSpaDESmodules",
               modulePath = "SpaDES_Modules",
               outputPath = file.path("outputs", runName)),
  modules = c("tati-micheletti/speciesAbundance@main",
              "tati-micheletti/temperature@main",
              "tati-micheletti/speciesAbundTempLM@main"),
  times = list(start = 2013,
               end = 2032),
  updateRprofile = TRUE,
  Restart = TRUE
)

snippsim <- do.call(SpaDES.core::simInitAndSpades, out)

```
See [Getting Started Vignette](articles/i-getting-started.html)

## Contributions

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for information on how to contribute to this project.
