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
#'   inputPath, outputPath, scratchPath, cachePath, rasterTmpDir)`
#' @param modules a character string of modules to pass to `getModule`. These
#'   should be in the form `GitHubAccount/Repo@branch` e.g.,
#'   `"PredictiveEcology/Biomass_core@development"`. If the project is a git repository,
#'   then it will not try to re-get these modules; instead it will rely on the user
#'   managing their git status outside of this function.
#' @param packages A vector of packages that are needed. This will be passed to
#'   `Require`
#' @param optionsStyle A numeric representing a set of pre-determined options. Currently,
#' this can be either 1 (cache using qs and memoise, prepInputs uses terra & sf,
#' no moduleCodeChecks) or 0 (no options).
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
#'              paths = list(modulePath = "m", projectPath = "~/GitHub/SpaDES.project",
#'                           scratchPath = tempdir()),
#'              modules = "PredictiveEcology/Biomass_borealDataPrep@development",
#'              useGit = TRUE
#' )
#'
#'
#' SpaDES.project::setupProject(
#'   paths = list(projectPath = "~/CeresPaper"),
#'   standAlone = TRUE,
#'   require =
#'     c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
#'       "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"),
#'   modules = c("CeresBarros/Biomass_speciesData@master",
#'               "CeresBarros/Biomass_borealDataPrep@development",
#'               "CeresBarros/Biomass_core@master",
#'               "CeresBarros/Biomass_validationKNN@master",
#'               "CeresBarros/Biomass_speciesParameters@development")
#'
#'   )
#' }
setupProject <- function(name, paths, modules, packages,
                         optionsStyle = 1,
                         require = c("reproducible", "SpaDES.core"),
                         useGit = FALSE, setLinuxBinaryRepo = TRUE,
                         standAlone = TRUE, libPaths = paths$packagePath,
                         overwrite = FALSE, verbose = 1) {

  libPaths <- substitute(libPaths)
  if (missing(name)) {
    if (!is.null(paths$projectPath))
      name <- basename(normPath(paths$projectPath))
    else
      stop("Must provide either a name or a paths$projectPath")
  }

  nameSimple <- extractPkgName(name)

  curDir <- getwd()
  inProject <- identical(basename(curDir), nameSimple)

  paths <- setupPaths(name, paths, inProject, standAlone, libPaths)

  setupOptions(optionsStyle)

  modulePackages <- setupModules(paths, modules, useGit = useGit,
                                 overwrite = overwrite, verbose = verbose)

  if (missing(packages))
    packages <- NULL
  packages <- c(unname(unlist(modulePackages)), packages)


  setupPackages(packages, require = require,
                setLinuxBinaryRepo = setLinuxBinaryRepo,
                standAlone = standAlone,
                libPaths = paths$packagePath, verbose = verbose)

  setupGitIgnore(paths, verbose)

  if (!inProject) {
    if (interactive() && getOption("SpaDES.project.restart", TRUE))
      if (requireNamespace("rstudioapi")) {
        message("... restarting Rstudio inside the project")
        rstudioapi::openProject(path = paths$projectPath)
      } else {
        stop("Please open this in a new Rstudio project at ",
             paths$projectPath)
      }
  }

  return(paths)
}


#' @importFrom data.table data.tble
setupOptions <- function(optionsStyle) {
  os <- list()
  if (optionsStyle == 1) {
    os <- list(reproducible.cacheSaveFormat = "qs",
               reproducible.useTerra = TRUE,
               reproducible.useMemoise = TRUE,
               reproducible.rasterRead = "terra::rast",
               reproducible.showSimilar = TRUE,
               spades.moduleCodeChecks = FALSE
    )
  }

  if (length(os)) {
    options(os)
    messageDF(data.table::data.table(option = names(os), values = os))
  }
}

setupGitIgnore <- function(paths, verbose) {
  gitIgnoreFile <- ".gitignore"
  if (file.exists(gitIgnoreFile)) {
    gif <- readLines(gitIgnoreFile, warn = FALSE)
    lineWithPkgPath <- grep(paste0("^", basename(paths$packagePath),"$"), gif)
    insertLine <- if (length(lineWithPkgPath)) lineWithPkgPath[1] else length(gif) + 1
    gif[insertLine] <- file.path(basename(paths$packagePath), "*")

    lineWithModPath <- grep(paste0("^", basename(paths$modulePath),"$"), gif)
    insertLine <- if (length(lineWithModPath)) lineWithModPath[1] else length(gif) + 1
    gif[insertLine] <- file.path(basename(paths$modulePath), "*")

    writeLines(con = gitIgnoreFile, unique(gif))
    Require:::messageVerbose(verboseLevel = 1, verbose = verbose,
                             ".gitignore file updated with packagePath and modulePath; ",
                             "this may need to be confirmed manually")
  }
}



setupPackages <- function(packages, require, libPaths, setLinuxBinaryRepo, standAlone, verbose) {
  if (isTRUE(setLinuxBinaryRepo))
    Require::setLinuxBinaryRepo()
  if (missing(packages))
    packages <- NULL
  Require:::messageVerbose("Installing any missing reqdPkgs", verbose = verbose)
  out <- Require::Require(packages, require = require, standAlone = standAlone,
                          libPaths = libPaths,
                          verbose = verbose)

  invisible(NULL)
}


setupModules <- function(paths, modules, useGit, overwrite, verbose) {
  anyfailed <- character()
  modulesOrig <- modules
  modulesOrigPkgName <- extractPkgName(modulesOrig)
  if (!useGit) {
    modNam <- extractPkgName(modules)
    whExist <- dir.exists(file.path(paths$modulePath, modNam))
    modsToDL <- modules

    if (overwrite %in% FALSE) if (any(whExist)) modsToDL <- modules[whExist %in% FALSE]
    if (length(modsToDL)) {
      tmpdir <- file.path(tempdir(), Require:::.rndstr(1))
      Require::checkPath(tmpdir, create = TRUE)
      od <- setwd(tmpdir)
      on.exit(setwd(od))

      out <-
        Map(modToDL = modsToDL, function(modToDL) {
          dd <- Require:::.rndstr(1)
          modNameShort <- Require::extractPkgName(modToDL)
          Require::checkPath(dd, create = TRUE)
          Require:::downloadRepo(modToDL, subFolder = NA,
                               destDir = dd, overwrite = overwrite,
                               verbose = verbose + 1)
          files <- dir(file.path(dd, modNameShort), recursive = TRUE)
          newFiles <- file.path(paths$modulePath, modNameShort, files)
          lapply(unique(dirname(newFiles)), dir.create, recursive = TRUE, showWarnings = FALSE)
          file.copy(file.path(dd, modNameShort, files),
                    file.path(paths$modulePath, modNameShort, files), overwrite = TRUE)

      })
      # out <- getModule(modules, modulePath = paths$modulePath, overwrite = overwrite)
      allworked <- Require::extractPkgName(modsToDL) %in% dir(paths$modulePath)
      anyfailed <- modsToDL[!allworked]
      modules <- anyfailed
    }
  }

  if (isTRUE(useGit) || length(anyfailed)) {
    modulesWOat <- gsub("@.+$", "", modules)
    lapply(modulesWOat, function(m) {
      modPath <- file.path(paths$modulePath, extractPkgName(m))
      if (!dir.exists(modPath)) {
        cmd <- paste0("cd ", paths$modulePath, " && git clone https://github.com/", m)
        system(cmd)
      } else {
        Require:::messageVerbose("module exists at ", modPath, "; not cloning", verbose = verbose)
      }
    })
  }

  modulePackages <- packagesInModules(modulePath = paths$modulePath, modules = modulesOrigPkgName)
  modulesSimple <- Require::extractPkgName(modulesOrigPkgName)
  modulePackages[modulesSimple]

}


#' @importFrom Require normPath checkPath
setupPaths <- function(name, paths, inProject, standAlone, libPaths) {

  if (missing(paths)) {
    projPth <- if  (inProject) file.path(".") else file.path(".", name)
    paths <- list(projectPath = normPath(projPth))
  }


  if (is.null(paths$projectPath))
    stop("Please specify paths$projectPath as an absolute path")

  if (is.null(libPaths) || is.call(libPaths)) {
    if (is.null(paths$packagePath)) {
      pkgPth <- tools::R_user_dir("data")
      paths$packagePath <- file.path(pkgPth, name, "packages", version$platform, substr(getRversion(), 1, 3))
      if (is.call(libPaths)) {
        libPaths <- eval(libPaths, envir = environment(), enclos = environment())
      }
    }
  } else {
    paths$packagePath <- libPaths
  }

  if (is.null(paths$modulePath)) paths$modulePath <- file.path(paths$projectPath, "m")
  isAbs <- unlist(lapply(paths, isAbsolutePath))
  paths[!isAbs] <- lapply(paths[!isAbs], function(x) file.path(paths$projectPath, x))
  paths <- lapply(paths, normPath)
  paths <- lapply(paths, checkPath, create = TRUE)
  if (!inProject) {
    setwd(paths$projectPath)
  }

  if (is.null(paths$scratchPath)) {
    paths$scratchPath <- file.path(tempdir(), "SpaDES.project", name)
  }
  if (!is.null(paths$scratchPath)) {
    paths <- Require::modifyList2(
      list(scratchPath = file.path(paths$scratchPath),
           rasterPath = file.path(paths$scratchPath, "raster"),
           terraPath = file.path(paths$scratchPath, "terra")
      ),
      paths)

  }

  paths <- Require::modifyList2(
    list(cachePath = file.path(paths$projectPath, "cache"),
         inputPath = file.path(paths$projectPath, "input"),
         outputPath = file.path(paths$projectPath, "output")
    ),
    paths)

  spPaths <- c("cachePath", "inputPath", "modulePath", "outputPath", "rasterPath",
               "scratchPath", "terraPath")

  deps <- Require::extractPkgName(Require::pkgDep("PredictiveEcology/SpaDES.project@transition")[[1]])
  deps <- c(deps, "rstudioapi")

  for (pkg in deps) {
    pkgDir <- file.path(.libPaths(), pkg)
    de <- dir.exists(pkgDir)
    pkgDir <- pkgDir[de][1]
    files1 <- dir(pkgDir, all.files = TRUE, recursive = TRUE)
    newFiles <- file.path(paths$packagePath, pkg, files1)
    lapply(unique(dirname(newFiles)), dir.create, recursive = TRUE, showWarnings = FALSE)
    oldFiles <- file.path(pkgDir, files1)
    exist <- file.exists(oldFiles)
    if (any(!exist))
      file.copy(oldFiles[!exist], newFiles[!exist], overwrite = TRUE)
  }

  Require::setLibPaths(paths$packagePath, standAlone = standAlone,
                       updateRprofile = TRUE,
                       exact = FALSE, verbose = getOption("Require.verbose"))


  do.call(setPaths, paths[spPaths])

  paths[order(names(paths))]
}

setPaths <- function(cachePath, inputPath, modulePath, outputPath, rasterPath, scratchPath,
                     terraPath, silent = FALSE) {
  defaults <- list(
    CP = FALSE,
    IP = FALSE,
    MP = FALSE,
    OP = FALSE,
    RP = FALSE,
    SP = FALSE,
    TP = FALSE
  )
  if (missing(cachePath)) {
    cachePath <- .getOption("reproducible.cachePath") # nolint
    defaults$CP <- TRUE
  }
  if (missing(inputPath)) {
    inputPath <- getOption("spades.inputPath") # nolint
    defaults$IP <- TRUE
  }
  if (missing(modulePath)) {
    modulePath <- getOption("spades.modulePath") # nolint
    defaults$MP <- TRUE
  }
  if (missing(outputPath)) {
    outputPath <- getOption("spades.outputPath") # nolint
    defaults$OP <- TRUE
  }
  if (missing(rasterPath)) { ## TODO: deprecate
    rasterPath <- file.path(getOption("spades.scratchPath"), "raster") # nolint
    defaults$RP <- TRUE
  }
  if (missing(scratchPath)) {
    scratchPath <- getOption("spades.scratchPath") # nolint
    defaults$SP <- TRUE
  }
  if (missing(terraPath)) {
    terraPath <- file.path(getOption("spades.scratchPath"), "terra") # nolint
    defaults$TP <- TRUE
  }

  allDefault <- all(unlist(defaults))

  originalPaths <- .paths()
  newPaths <- lapply(list(
    cachePath = cachePath,
    inputPath = inputPath,
    modulePath = modulePath,
    outputPath = outputPath,
    rasterPath = rasterPath,
    scratchPath = scratchPath,
    terraPath = terraPath
  ), checkPath, create = TRUE)

  ## set the new paths via options
  options(
    rasterTmpDir = newPaths$rasterPath,
    reproducible.cachePath = cachePath,
    spades.inputPath = inputPath,
    spades.modulePath = unlist(modulePath),
    spades.outputPath = outputPath,
    spades.scratchPath = scratchPath
  )

  if (requireNamespace("terra", quietly = TRUE)) {
    terra::terraOptions(tempdir = terraPath)
  }

  ## message the user
  modPaths <- if (length(modulePath) > 1) {
    paste0("c('", paste(normPath(modulePath), collapse = "', '"), "')")
  } else {
    normPath(modulePath)
  }

  if (!silent) {
    if (!allDefault) {
      message(
        "Setting:\n",
        "  options(\n",
        if (!defaults$CP) paste0("    reproducible.cachePath = '", normPath(cachePath), "'\n"),
        if (!defaults$IP) paste0("    spades.inputPath = '", normPath(inputPath), "'\n"),
        if (!defaults$OP) paste0("    spades.outputPath = '", normPath(outputPath), "'\n"),
        if (!defaults$MP) paste0("    spades.modulePath = '" , modPaths, "'\n"),
        if (!defaults$SP) paste0("    spades.scratchPath = '", normPath(scratchPath), "'\n"),
        "  )"
      )
    }

    if (any(unlist(defaults))) {
      message(
        "Paths set to:\n",
        "  options(\n",
        "    rasterTmpDir = '", normPath(rasterPath), "'\n",
        "    reproducible.cachePath = '", normPath(cachePath), "'\n",
        "    spades.inputPath = '", normPath(inputPath), "'\n",
        "    spades.outputPath = '", normPath(outputPath), "'\n",
        "    spades.modulePath = '", modPaths, "'\n", # normPath'ed above
        "    spades.scratchPath = '", normPath(scratchPath), "'\n",
        "  )\n",
        "  terra::terraOptions(tempdir = '", normPath(terraPath), "'"
      )
    }
  }

  return(invisible(originalPaths))
}

.paths <- function() {
  if (!is.null(.getOption("spades.cachePath"))) {
    message("option('spades.cachePath') is being deprecated. Please use ",
            "option('reproducible.cachePath').\n",
            "Setting option('reproducible.cachePath' = getOption('spades.cachePath'))")
  }

  list(
    cachePath = .getOption("reproducible.cachePath"), # nolint
    inputPath = getOption("spades.inputPath"), # nolint
    modulePath = getOption("spades.modulePath"), # nolint
    outputPath = getOption("spades.outputPath"), # nolint
    rasterPath = file.path(getOption("spades.scratchPath"), "raster"), # nolint
    scratchPath = getOption("spades.scratchPath"), # nolint
    terraPath = file.path(getOption("spades.scratchPath"), "terra") # nolint
  )
}

.getOption <- function(x, default = NULL) {
  optionDefault <- options(x)[[1]]
  if (is.null(optionDefault)) optionDefault <- default
  if (is.function(optionDefault)) {
    optionDefault()
  } else {
    optionDefault
  }
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


