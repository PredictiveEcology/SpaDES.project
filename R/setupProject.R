#' Sets up a new or existing SpaDES project
#'
#' `setupProject` calls a sequence of functions: `setupPaths`, `setupOptions`,
#' `setupModules`, `setupPackages`, `setupGitIgnore`. These create folder
#' structures, download or confirms existence of modules, install missing
#' packages from both the modules `reqdPkgs` fields and the user passed
#' `packages`. See Details.
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
#' @param require An optional character vector of packages to attach
#'   (with `require`)
#' @param useGit A logical. If `TRUE`, it will use `git clone`. Otherwise it will
#' get modules with `getModules`.
#' @param standAlone A logical. Passed to `Require::standAlone`. This keeps all
#'   packages installed in a project-level library, it `TRUE`. Default is `TRUE`.
#' @param libPaths A character vector. Passed to `Require::libPaths`, which will
#'   in turn pass to `.libPaths(libPaths)`
#' @param inProject A logical. If `TRUE`, then the current directory is
#'  inside the `paths$projectPath`.
#' @param restart If the `projectPath` is not the current path, and the session is in
#'   Rstudio, and interactive, it will restart with a new Rstudio session with a
#'   new project, with a root path set to `projectPath`. Default is `FALSE`.
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
#'
#' @return
#' `setupProject` will return a length-2 named list (`modules`, `paths`) that can be passed
#' directly to `SpaDES.core::simInit` using a `do.call`. See example.
#'
#' @importFrom Require extractPkgName
#' @rdname setupProject
#'
#' @examples
#' \dontrun{
#' setupProject(name = "SpaDES.project",
#'              paths = list(modulePath = "m", projectPath = "~/GitHub/SpaDES.project",
#'                           scratchPath = tempdir()),
#'              modules = "PredictiveEcology/Biomass_borealDataPrep@development"
#' )
#'
#'
#' out <- SpaDES.project::setupProject(
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
#'   if (require("SpaDES.core"))
#'     do.call(simInit, out)
#' }
setupProject <- function(name, paths, modules, packages,
                         optionsStyle = 1,
                         require = c("reproducible", "SpaDES.core"),
                         restart = getOption("SpaDES.project.restart", FALSE),
                         useGit = FALSE, setLinuxBinaryRepo = TRUE,
                         standAlone = TRUE, libPaths = paths$packagePath,
                         overwrite = FALSE, verbose = getOption("Require.verbose", 1L)) {

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

  out <- list(modules = Require::extractPkgName(modules),
       paths = paths[spPaths])
  if (!inProject) {
    if (interactive() && isTRUE(restart)) # getOption("SpaDES.project.restart", TRUE))
      if (requireNamespace("rstudioapi")) {
        message("... restarting Rstudio inside the project")
        rstudioapi::openProject(path = paths$projectPath)
      } else {
        stop("Please open this in a new Rstudio project at ",
             paths$projectPath)
      }
  }

  return(out)
}

#'
#' @details
#' `setPaths` will fill in any paths that are not explicitly supplied by the
#' user as a named list. These paths that can be set are:
#' `projectPath`, `packagePath`, `cachePath`, `inputPath`,
#' `modulePath`, `outputPath`, `rasterPath`, `scratchPath`, `terraPath`.
#' These are grouped thematically into three groups of paths:
#' `projectPath` and `packagePath` affect the project, regardless
#' of whether a user uses SpaDES modules. `cachePath`, `inputPath`, `outputPath` and
#' `modulePath` are all used by SpaDES within module contexts. `scratchPath`,
#' `rasterPath` and `terraPath` are all "temporary" or "scratch" directories.
#'
#' @return
#' `setupPaths` returns a list of paths that are created. It is also called for its
#' side effect which is to call `setPaths`, with each of these paths as an argument.
#' See table for details.
#'
#' @section Paths:
#'   \tabular{lll}{
#' **Path**     \tab **Default if not supplied by user** \tab Effects \cr
#'                               \tab *Project Level Paths*   \tab \cr
#' `projectPath`\tab if `getwd()` is `name`, then just `getwd`; if not
#'                            `file.path(getwd(), name)`  \tab If current project is not this project
#'                                                             and using `Rstudio`, then the current
#'                                                             project will close and a new project will
#'                                                             open in the same Rstudio session, unless
#'                                                             `restart = FALSE`\cr
#' `packagePath`\tab `file.path(tools::R_user_dir("data"), name, "packages",
#'                               version$platform, substr(getRversion(), 1, 3))`
#'                                                    \tab appends this path to `.libPaths(packagePath)`,
#'                                                         unless `standAlone = TRUE`, in which case,
#'                                                         it will set `.libPaths(packagePath,
#'                                                         include.site = FALSE)` to this path \cr
#' ------       \tab -----------                 \tab  -----         \cr
#'              \tab *Module Level Paths*                 \tab \cr
#' `cachePath`  \tab `file.path(projectPath, "cache")` \tab `options(reproducible.cachePath = cachePath)`\cr
#' `inputPath`  \tab `file.path(projectPath, "input")` \tab `options(spades.inputPath = inputPath)`\cr
#' `modulePath` \tab `file.path(projectPath, "m")` \tab `options(spades.inputPath = outputPath)` \cr
#' `outputPath` \tab `file.path(projectPath, "output")` \tab `options(spades.inputPath = modulePath)` \cr
#' ------       \tab -----------                 \tab  -----         \cr
#'              \tab *Temporary Paths*                 \tab  \cr
#' `scratchPath`\tab `file.path(tempdir(), name)` \tab \cr
#' `rasterPath` \tab `file.path(scratchPath, "raster")` \tab sets (`rasterOptions(tmpdir = rasterPath)`) \cr
#' `terraPath`  \tab `file.path(scratchPath, "terra")` \tab   sets (`terraOptions(tempdir = terraPath)`)       \cr
#' }
#'
#' @export
#' @rdname setupProject
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


#' @export
#' @rdname setupProject
#' @details
#' `setupOptions` is currently very limited. It only allows `optionsStyle = 1`,
#' which sets the following:
#'
#' @return
#' `setupOptions` is run for its side effects.
#'
#' ```
#'     options(reproducible.cacheSaveFormat = "qs",
#'             reproducible.useTerra = TRUE,
#'             reproducible.useMemoise = TRUE,
#'             reproducible.rasterRead = "terra::rast",
#'             reproducible.showSimilar = TRUE,
#'             spades.moduleCodeChecks = FALSE)
#' ````
#'
#' @importFrom data.table data.table
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

#' @export
#' @rdname setupProject
#' @details
#' `setupModules` will download all modules do not yet exist locally. The current
#' test for "exists locally" is simply whether the directory exists. If a user
#' wants to update the module, `overwrite = TRUE` must be set, or else the user can
#' remove the folder manually.
#'
#' @return
#' `setupModules` is run for its side effects, i.e., downloads modules and puts them
#' into the `paths$modulePath`
#'
setupModules <- function(paths, modules, useGit, overwrite, verbose) {
  anyfailed <- character()
  modulesOrig <- modules
  modulesOrigPkgName <- extractPkgName(modulesOrig)
  if (!useGit) {
    out <- getModule(modules, paths$modulePath, overwrite)
    anyfailed <- out$failed
    modules <- anyfailed
    # modNam <- extractPkgName(modules)
    # whExist <- dir.exists(file.path(paths$modulePath, modNam))
    # modsToDL <- modules
    #
    # if (overwrite %in% FALSE) if (any(whExist)) modsToDL <- modules[whExist %in% FALSE]
    # if (length(modsToDL)) {
    #   tmpdir <- file.path(tempdir(), .rndstr(1))
    #   Require::checkPath(tmpdir, create = TRUE)
    #   od <- setwd(tmpdir)
    #   on.exit(setwd(od))
    #
    #   out <-
    #     Map(modToDL = modsToDL, function(modToDL) {
    #       dd <- .rndstr(1)
    #       modNameShort <- Require::extractPkgName(modToDL)
    #       Require::checkPath(dd, create = TRUE)
    #       Require:::downloadRepo(modToDL, subFolder = NA,
    #                              destDir = dd, overwrite = overwrite,
    #                              verbose = verbose + 1)
    #       files <- dir(file.path(dd, modNameShort), recursive = TRUE)
    #       newFiles <- file.path(paths$modulePath, modNameShort, files)
    #       lapply(unique(dirname(newFiles)), dir.create, recursive = TRUE, showWarnings = FALSE)
    #       file.copy(file.path(dd, modNameShort, files),
    #                 file.path(paths$modulePath, modNameShort, files), overwrite = TRUE)
    #
    #     })
    #   # out <- getModule(modules, modulePath = paths$modulePath, overwrite = overwrite)
    #   allworked <- Require::extractPkgName(modsToDL) %in% dir(paths$modulePath)
    #   browser()
    #   if (allworked)
    #     Require:::messageVerbose()
    #   anyfailed <- modsToDL[!allworked]
    #   modules <- anyfailed
    # }
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

#' @export
#' @rdname setupProject
#' @details
#' `setupPackages` will read the modules' metadata `reqdPkgs` element. It will combine
#' these with any packages passed manually by the user to `packages`, and pass all
#' these packages to `Require::Install(...)`.
#'
#' @return
#' `setupPackages` is run for its side effects, i.e., installing packages to
#' `paths$packagePath`.
#'
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

#' @export
#' @rdname setupProject
#' @details
#' `setupGitIgnore` will add.
#'
#' @return
#' `setupGitIgnore` is run for its side effects, i.e., adding elements to the
#' `.gitignore` file.
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

# These are the setPaths paths that are available
spPaths <- c("cachePath", "inputPath", "modulePath", "outputPath", "rasterPath",
             "scratchPath", "terraPath")

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


