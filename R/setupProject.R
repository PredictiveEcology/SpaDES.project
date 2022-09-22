#' Sets up a new or existing SpaDES project
#'
#' The main things this function does are: creates folder structures, downloads or
#' confirms existence of modules, install missing packages from both the modules
#' `reqdPkgs` fields and the user passed `packages`. See Details.
#'
#' @param name The name of the project; if this is a GitHub project, then it should
#'   indicate the full Github repository and branch name, e.g.,
#'    `"PredictiveEcology/WBI_forecasts@ChubatyPubNum12"`
#' @param paths a list with named elements, specifically, `modulePath`, `projectPath`,
#'   `packagePath` and all others that are in `SpaDES.core::setPaths()` `(
#'   inputPath, outputPath, scratchPath, cachePath, rasterTmpDir)``
#' @param modules a character string of modules to pass to `getModule`. These
#'   should be in the form `GitHubAccount/Repo@branch` e.g.,
#'   `"PredictiveEcology/Biomass_core@development"`. If the project is a git repository,
#'   then it will not try to re-get these modules; instead it will rely on the user
#'   managing their git status outside of this function.
#' @param packages A vector of packages that are needed. This will be passed to
#'   `Require`
#'
#' @details
#' This function bundles a sequence of functions: `.libPaths()`, `getModule`, `setwd`,
#' `Require`
#'
#' @importFrom Require extractPkgName
#' @examples
#' \dontrun{
#' setupProject("test", list(modulePath = "m"),
#'              modules = "PredictiveEcology/Biomass_core",
#'              packages = "fpCompare")
#' }
setupProject <- function(name, paths, modules, packages) {
  nameSimple <- extractPkgName(name)
  curDir <- getwd()
  inProject <- identical(basename(curDir), nameSimple)
  paths <- lapply(paths, normPath)
  if (!inProject) {
    browser()
    checkPath(paths$modulePath, create = TRUE)
    setwd(paths$modulePath)
  }

  getModule(modules, modulePath = paths$modulePath)
}


# options(repos = c(CRAN = "http://cloud.r-project.org"))
# # 3 packages (could be just 2) are needed to manage the setup; these are installed in default .libPaths()
# if (!require("remotes", quietly = TRUE)) {
#   install.packages("remotes");
# }
# remotes::install_github("PredictiveEcology/Require@archivedPkg", upgrade = FALSE)
# Require::Require("PredictiveEcology/SpaDES.project@development", upgrade = FALSE)
#
# # Setup the paths, modules, git links if any
# projPath <- "~/GitHub"
# modulePath <- "m"
# packagePath <- "R"
# setupProject(name = "PredictiveEcology/WBI_forecasts@ChubatyPubNum12",
#              projectPath = projPath,
#              modulePath = modulePath,
#              packagePath = packagePath,
#              modules = c("CeresBarros/Biomass_speciesData@master",
#                          "CeresBarros/Biomass_borealDataPrep@71-ssl-certificate-of-nfi-ftp-server-use",
#                          "CeresBarros/Biomass_core@master",
#                          "CeresBarros/Biomass_validationKNN@master",
#                          "CeresBarros/Biomass_speciesParameters@temp"),
#              packages = c("PredictiveEcology/SpaDES.experiment@development"))
# )

# PseudoCode for above setupProject
# 1. Check whether project exists locally
#     if FALSE A) then setwd to path, git clone, git submodule init, setwd into project
#     if TRUE A) check that it is an SpaDES project
#             B) setwd into project or do nothing if already there
#             C) confirm Git status
# 2. Check whether the Rstudio project is this project --
#     if FALSE A) open the project programmatically from Rstudio
# 3. setProjPkgDir(packagePath) # not necessary if this in inside `newProject`
# 4. setLinuxBinaryRepo()
# 5. SpaDES.project::packagesInModules
# 6. Require::Require(c(unname(unlist(outs)), packages), require = FALSE, standAlone = TRUE)
