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
#' @param setLinuxBinaryRepo Logical. Should the binary RStudio Package Manager be used
#'   on Linux (ignored if Windows)
#' @param overwrite Logical. Passed to `getModule`
#' @param verbose An integer specifying how much verbosity should be shown. Passed
#'   to various internal functions, including `Require`. Default is 1. The higher the number
#'   the more verbosity. Set to -1 for almost no messaging.
#'
#' @export
#' @details
#' This function bundles a sequence of functions: `.libPaths()`, `getModule`, `setwd`,
#' `Require`
#'
#' @importFrom Require extractPkgName
#' @examples
#' \dontrun{
#' setupProject(name = "SpaDES.project",
#'              paths = list(modulePath = "m", projectPath = "~/GitHub/SpaDES.project"),
#'              modules = c("PredictiveEcology/Biomass_borealDataPrep@development")
#' )
#' }
setupProject <- function(name, paths, modules, packages, setLinuxBinaryRepo = TRUE,
                         overwrite = FALSE, verbose = 1) {
  nameSimple <- extractPkgName(name)
  curDir <- getwd()
  inProject <- identical(basename(curDir), nameSimple)
  isAbs <- unlist(lapply(paths, isAbsolutePath))
  if (is.null(paths$projectPath)) stop("Please specify paths$projectPath as an absolute path")
  if (is.null(paths$packagePath))
    paths$packagePath <- file.path("packages", version$platform, substr(getRversion(), 1, 3))
  if (is.null(paths$modulePath)) paths$modulePath <- "m"
  paths[!isAbs] <- lapply(paths[!isAbs], function(x) file.path(paths$projectPath, x))
  paths <- lapply(paths, normPath)
  paths <- lapply(paths, checkPath, create = TRUE)
  if (!inProject) {
    setwd(paths$projectPath)
  }

  # paths <- lapply(paths, normPath)
  getModule(modules, modulePath = paths$modulePath, overwrite = overwrite)
  if (isTRUE(setLinuxBinaryRepo))
    Require::setLinuxBinaryRepo()
  modulePackages <- packagesInModules(modulePath = paths$modulePath)
  modulesSimple <- Require::extractPkgName(modules)
  modulePackages <- modulePackages[modulesSimple]
  if (missing(packages))
    packages <- NULL
  out <- Require::Require(c(unname(unlist(modulePackages)), packages), require = FALSE,
                          verbose = verbose)

  gitIgnoreFile <- ".gitignore"
  if (file.exists(gitIgnoreFile)) {
    gif <- readLines(gitIgnoreFile, warn = FALSE)
    lineWithPkgPath <- grep(paste0("^", basename(paths$packagePath),"$"), gif)
    insertLine <- if (length(lineWithPkgPath)) lineWithPkgPath[1] else length(gif) + 1
    gif[insertLine] <- file.path(basename(paths$packagePath), "*")

    lineWithModPath <- grep(paste0("^", basename(paths$modulePath),"$"), gif)
    insertLine <- if (length(lineWithModPath)) lineWithModPath[1] else length(gif) + 1
    gif[insertLine] <- file.path(basename(paths$modulePath), "*")

    writeLines(con = gitIgnoreFile, gif)
    Require:::messageVerbose(verboseLevel = 1, verbose = verbose,
                             ".gitignore file updated with packagePath and modulePath; ",
                             "this may need to be confirmed manually")
  }

  return(paths)
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

