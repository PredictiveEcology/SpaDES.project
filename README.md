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

The [package website](https://spades-project.predictiveecology.org) is the best place to start. We would suggest these vignettes, roughly in this order:

1. [Getting started](articles/i-getting-started.html)
2. [Using `git` and GitHub](articles/iii-using-git-github.html)
3. [Installing R](articles/iv-Installing-R.html)
4. [Finding other SpaDES modules](articles/v-finding-other-modules.html)

**Package website:** [https://spades-project.predictiveecology.org](https://spades-project.predictiveecology.org)

**The wider `SpaDES` ecosystem:** [https://SpaDES.PredictiveEcology.org](https://SpaDES.PredictiveEcology.org)

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

The following example contains everything needed to run a set of modules, in a very short set of commands, from (almost) any starting condition. We use this approach in many of our projects.

Key features:

1. The project is fully self-contained in a folder, with packages installed to a unique library based on the `projectPath`. This isolation is deliberate: it is what stops the package collisions that derailed many of our early projects.
2. No call to `install.packages` without an `if` guard, and no `library` or `require` calls before the package installations.
3. Every function is "rerun-capable" (i.e., idempotent): rerunning a line does not redo work that is already done. For example, `remotes::install_github` does not reinstall a package whose SHA has not changed.
4. No objects are assigned into the `.GlobalEnv`. For small, simple projects, using the `.GlobalEnv` is fine; as a project grows, an object with a common name like `out` can be picked up unexpectedly, hiding an error that should instead have caused the function to fail.
5. The minimum number of packages are installed before `setupProject`. As `SpaDES.project` and `Require` updates reach CRAN, this will get simpler still.
6. Minimal use of `setwd`. The example below uses `setwd("~")` only as a bare-minimum default; change it (or comment it out) to wherever you want this project to live.


```r
getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("remotes")
getOrUpdatePkg("Require", "1.0.0")
getOrUpdatePkg("SpaDES.project", "1.0.1")

setwd("~") # change this to wherever you want the project to live
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
