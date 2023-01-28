#' Sets up a new or existing SpaDES project
#'
#' `setupProject` calls a sequence of functions in this order:
#' `setupPaths`, `setupOptions`,
#' `setupModules`, `setupPackages`, `setupTimes`, `setupParams`,
#' `setupGitIgnore`. Because of this
#' order, settings in `options` can change `paths`, `times` can be used in `params`,
#' for example.
#' This sequence will create folder structures, download or confirms
#' existence of modules, install missing
#' packages from both the modules `reqdPkgs` fields and the user passed
#' `packages`. See Details.
#'
#' @param name Optional. If supplied, the name of the project. If not supplied, an
#' attempt will be made to extract the name from the `paths[["projectPath"]]`.
#' If this is a GitHub project, then it should indicate the full Github
#' repository and branch name, e.g., `"PredictiveEcology/WBI_forecasts@ChubatyPubNum12"`
#' @param paths a list with named elements, specifically, `modulePath`, `projectPath`,
#'   `packagePath` and all others that are in `SpaDES.core::setPaths()`
#'   (i.e., `inputPath`, `outputPath`, `scratchPath`, `cachePath`, `rasterTmpDir`).
#' @param modules a character string of modules to pass to `getModule`. These
#'   should be in the form `GitHubAccount/Repo@branch` e.g.,
#'   `"PredictiveEcology/Biomass_core@development"`. If the project is a git repository,
#'   then it will not try to re-get these modules; instead it will rely on the user
#'   managing their git status outside of this function.
#' @param times Optional. This will be returned if supplied; if supplied, the values
#'   can be used in e.g., `params`, e.g., `params = list(mod = list(startTime = times$start))`.
#'   See help for `SpaDES.core::simInit`.
#' @param packages Optional. A vector of packages that must exist in the `libPaths`.
#'   This will be passed to `Require::Install`, i.e., these will be installed, but
#'   not attached to the search path. See `require`.
#' @param require Optional. A character vector of packages to install *and* attach
#'   (with `Require::Require`). These will be installed and attached at the start
#'   of `setupProject` so that a user can use these during `setupProject`.
#' @param options Optional. Either a named list to be passed to `options`
#'   or a character vector indicating one or more file(s) to source,
#'   in the order provided. These will be sourced into a temporary environment (not
#'   the `.GlobalEnv`), so they will not create globally accessible objects. See details.
#' @param params Optional. Similar to `options`, however, this named list will be
#'   returned, i.e., there are no side effects.
#' @param sideEffects Optional. This can be an expression or one or more filenames.
#'   This/these will be parsed and evaluated, but nothing returned. This is intended
#'   to be used for functions, such as cloud authentication or configurations,
#'   that are run for their side effects only.
#' @param useGit A logical. If `TRUE`, it will use `git clone`. Otherwise it will
#' get modules with `getModules`.
#' @param standAlone A logical. Passed to `Require::standAlone`. This keeps all
#'   packages installed in a project-level library, it `TRUE`. Default is `TRUE`.
#' @param libPaths A character vector. Passed to `Require::libPaths`, which will
#'   in turn pass to `.libPaths(libPaths)`
#' @param inProject A logical. If `TRUE`, then the current directory is
#'  inside the `paths[["projectPath"]]`.
#' @param restart If the `projectPath` is not the current path, and the session is in
#'   Rstudio, and interactive, it will restart with a new Rstudio session with a
#'   new project, with a root path set to `projectPath`. Default is `FALSE`.
#' @param setLinuxBinaryRepo Logical. Should the binary RStudio Package Manager be used
#'   on Linux (ignored if Windows)
#' @param overwrite Logical. Passed to `getModule`, and `setupParams`, `setupOptions`
#' @param dots Any other named objects passed as a list a user might want for other elements.
#' @param ... Any other named objects a user might want.
#'
#' @export
#'
#' @section Sequential evaluation:
#' Throughout these functions, efforts have been made to implement sequential evaluation,
#' within files and within lists. This means that a user can *use* the values from an
#' upstream element in the list. For example, the following is valid:
#'
#' ```
#' paths = list(projectPath = "here", modulePath = file.path(paths[["projectPath"]], "modules")
#' ```
#' Because of such sequential evaluation, `paths`, `options`, and `params` files
#' can be sequential lists that have impose a hierarchy specified
#' by the order. For example, a user can first create a list of *default* options,
#' then several lists of user-desired options behind an `if (user("emcintir"))`
#' block that add new or override existing elements, followed by `machine` specific
#' values, such as paths.
#'
#'
#'
#' @section Files for `paths`, `options`, `params`:
#' If `paths`, `options` and/or `params` are a character string or vector, the string(s)
#' will be interpretted as files to parse. These files should contain R code that
#' specifies *named lists*, where the names are one or more `paths`, `options`,
#' or are module names, each with a named list of parameters for that named module.
#' This last named list for `params` follows the convention used for the `params` argument in
#' `simInit(..., params = )`. The `options` file should
#' not set `options` explicitly; only named lists. This enables options checking/validating
#' to occur within `setupOptions` and `setupParams`.
#'
#' These files can use `paths`, `times`, plus any previous list in the sequence of
#' `params` or `options` specified.
#'
#' A simplest case would be a file with this:
#' `opts <- list(reproducible.destinationPath = "~/destPath")`. All named lists will
#' be parsed into their own environment, and then will be sequentially evaluated (i.e.,
#' subsequent lists will have access to previous lists), with each named elements
#' setting or replacing the previously named element of the same name, creating a
#' single list. This final list will be assigned to `options()` inside `setupOptions`.
#' Because these are each parsed separately, it is not necessary to assign any list to
#' an object; and the objects, if named, can be any name, even the same name repeatedly.
#' The sequential nature means that a user can
#' have a named list of default settings first in the file, then those defaults can
#' be overridden by e.g., user-specific or machine-specific values that are
#' specified subsequently in the same file or in a separate file. Any functions
#' that are used must be available, e.g., prefixed `Require::normPath`.
#'
#' NOTE: these will only parse items that are atomics (i.e., character, numeric, etc.),
#'   named lists or either of these that are protected by 1 level of "if". This
#'   will not work, therefore, for other side-effect elements, like authenticating
#'   with a cloud service.
#'
#' Several helper functions exist within `SpaDES.project` that may be useful, such
#' as `user(...)`, `machine(...)`
#'
#' @seealso [user], [machine], [node]
#' @return
#' `setupProject` will return a length-3 named list (`modules`, `paths`, `params`) that can be passed
#' directly to `SpaDES.core::simInit` using a `do.call`. See example.
#'
#' @importFrom Require extractPkgName
#' @inheritParams Require::Require
#' @inheritParams Require::setLibPaths
#' @rdname setupProject
#'
#' @examples
#' \dontrun{
#'
#' setupProject() # simplest case; just creates folders, sets options in current folder
#'
#' # set relative paths & modules
#' setupProject(name = "SpaDES.project",
#'              paths = list(modulePath = "m", projectPath = "SpaDES.project",
#'                           scratchPath = tempdir()),
#'              modules = "PredictiveEcology/Biomass_borealDataPrep@development"
#' )
#'
#' # load packages using `require`
#' out <- SpaDES.project::setupProject(
#'   paths = list(projectPath = "~/CeresPaper"), # will deduce name of project from projectPath
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
#'
#' # With options and params set
#' out <- setupProject(name = "SpaDES.project",
#'              options = list(reproducible.useTerra = TRUE),
#'              params = list(Biomass_borealDataPrep = list(.plots = "screen")),
#'              paths = list(modulePath = "m", projectPath = "~/GitHub/SpaDES.project",
#'                           scratchPath = tempdir()),
#'              modules = "PredictiveEcology/Biomass_borealDataPrep@development"
#' )
#'
#' # using an options file that is remote
#' out <- setupProject(name = "SpaDES.project",
#'              options = c("PredictiveEcology/LandWeb@development/04-options.R"),
#'              params = list(Biomass_borealDataPrep = list(.plots = "screen")),
#'              paths = list(modulePath = "m", projectPath = "~/GitHub/SpaDES.project",
#'                           scratchPath = tempdir()),
#'              modules = "PredictiveEcology/Biomass_borealDataPrep@development"
#' )
#'
#' # setting arbitrary arguments
#' out <- setupProject(modules = "PredictiveEcology/Biomass_borealDataPrep@development",
#'   sideEffects = system.file("sideEffects.R", package = "SpaDES.project",
#'   mode = "development")
#' )
#'
#' # If using SpaDES.core, the return object can be passed to `simInit` via `do.call`
#' #   do.call(simInit, out)
#' }
setupProject <- function(name, paths, modules, packages,
                         times, options, params, sideEffects,
                         require = NULL,
                         restart = getOption("SpaDES.project.restart", FALSE),
                         useGit = FALSE, setLinuxBinaryRepo = TRUE,
                         standAlone = TRUE, libPaths = paths$packagePath,
                         updateRprofile = getOption("Require.updateRprofile", FALSE),
                         overwrite = FALSE, verbose = getOption("Require.verbose", 1L),
                         envir = environment(), dots, ...) {

  dotsToHere(dots, ...)

  paramsSUB <- substitute(params) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  optionsSUB <- substitute(options) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = paths$projectpath)`
  sideEffectsSUB <- substitute(sideEffects)
  libPaths <- substitute(libPaths)

  pathsSUB <- checkProjectPath(pathsSUB, envir = envir, envir2 = parent.frame())

  if (missing(name)) {
    name <- basename(normPath(pathsSUB[["projectPath"]]))
  }
  inProject <- isInProject(name)

  paths <- setupPaths(name, pathsSUB, inProject, standAlone, libPaths,
                      updateRprofile, envir = envir)

  if (!is.null(require)) {
    Require::Require(require, require = require,
                     setLinuxBinaryRepo = setLinuxBinaryRepo,
                     standAlone = standAlone,
                     libPaths = paths$packagePath, verbose = verbose)
  }

  sideEffectsSUB <- setupSideEffects(name, sideEffectsSUB, paths, times, overwrite = overwrite,
                                     envir = envir)

  opts <- setupOptions(name, optionsSUB, paths, times, overwrite = overwrite, envir = envir)

  modulePackages <- setupModules(paths, modules, useGit = useGit,
                                 overwrite = overwrite, envir = envir, verbose = verbose)

  if (missing(packages))
    packages <- NULL

  # require <- c(unname(unlist(modulePackages)), require)

  setupPackages(packages, modulePackages, require = require,
                setLinuxBinaryRepo = setLinuxBinaryRepo,
                standAlone = standAlone,
                libPaths = paths$packagePath, envir = envir, verbose = verbose)

  params <- setupParams(name, paramsSUB, paths, modules, times,
                        overwrite = overwrite, envir = envir, verbose = verbose)

  setupGitIgnore(paths, envir = envir, verbose)

  out <- list(
    modules = Require::extractPkgName(modules),
    paths = paths[spPaths],
    params = params)

  if (!inProject) {
    if (interactive() && isTRUE(restart)) # getOption("SpaDES.project.restart", TRUE))
      if (requireNamespace("rstudioapi")) {
        messageVerbose("... restarting Rstudio inside the project",
                       verbose = verbose)
        rstudioapi::openProject(path = paths[["projectPath"]])
      } else {
        stop("Please open this in a new Rstudio project at ",
             paths[["projectPath"]])
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
#' ------       \tab -----------                 \tab  -----         \cr
#'              \tab *Other Paths*                 \tab  \cr
#' `logPath`    \tab `file.path(outputPath(sim), "log")` \tab sets `options("spades.logPath")` accessible by `logPath(sim)`\cr
#' `tilePath`   \tab Not implemented yet \tab Not implemented yet \cr
#' }
#'
#' @export
#' @rdname setupProject
#' @importFrom Require normPath checkPath
setupPaths <- function(name, paths, inProject, standAlone = TRUE, libPaths = paths$packagePath,
                       updateRprofile = getOption("Require.updateRprofile", FALSE),
                       overwrite = FALSE, envir = environment(),
                       verbose = getOption("Require.verbose", 1L), dots, ...) {
  dotsToHere(dots, ...)
  messageVerbose(yellow("setting up paths ..."), verbose = verbose)

  pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = file.path(paths$projectPath))`
  pathsSUB <- checkProjectPath(pathsSUB, envir, parent.frame())

  paths <- evalSUB(val = pathsSUB, valObjName = "paths", envir = envir, envir2 = parent.frame())
  paths <- parseFileLists(paths, paths[["projectPath"]], overwrite = overwrite,
                          envir = envir, verbose = verbose)

  if (missing(name))
    name <- basename(paths$projectPath)
  if (missing(inProject))
    inProject <- isInProject(name)
  if (is.null(paths[["projectPath"]]))
    stop("Please specify paths[[\"projectPath\"]] as an absolute path")

  if (is.null(libPaths) || is.call(libPaths)) {
    if (is.null(paths$packagePath)) {
      pkgPth <- tools::R_user_dir("data")
      paths$packagePath <- file.path(pkgPth, name, "packages", version$platform, substr(getRversion(), 1, 3))
      if (is.call(libPaths)) {
        libPaths <- evalSUB(libPaths, envir = environment(), envir2 = envir)
      }
    }
  } else {
    paths$packagePath <- libPaths
  }

  if (is.null(paths$modulePath)) paths$modulePath <- file.path(paths[["projectPath"]], "m")
  isAbs <- unlist(lapply(paths, isAbsolutePath))
  toMakeAbsolute <- isAbs %in% FALSE & names(paths) != "projectPath"
  paths[toMakeAbsolute] <- lapply(paths[toMakeAbsolute], function(x) file.path(paths[["projectPath"]], x))
  paths <- lapply(paths, normPath)
  paths <- lapply(paths, checkPath, create = TRUE)
  if (!inProject) {
    setwd(paths[["projectPath"]])
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
    list(cachePath = file.path(paths[["projectPath"]], "cache"),
         inputPath = file.path(paths[["projectPath"]], "input"),
         outputPath = file.path(paths[["projectPath"]], "output")
    ),
    paths)

  # deps <- Require::extractPkgName(Require::pkgDep("PredictiveEcology/SpaDES.project@transition")[[1]])
  deps <- c("SpaDES.project", "data.table", "Require", "rprojroot")
  deps <- c(deps, "rstudioapi")

  depsAlreadyInstalled <- dir(paths$packagePath, pattern = paste0(paste0("^", deps, "$"), collapse = "|"))
  diffVersion <- Map(dai = depsAlreadyInstalled, function(dai) {
    pvLibLoc <- packageVersion(dai, lib.loc = .libPaths()[1])
    pvPathsPackagePath <- packageVersion(dai, lib.loc = paths$packagePath)
    if (pvLibLoc > pvPathsPackagePath)
      list(Package = dai, pvLibLoc, pvPathsPackagePath)
    else
      NULL
  })
  diffVersionNames <- names(diffVersion[!vapply(diffVersion, is.null, FUN.VALUE = logical(1))])
  deps <- setdiff(deps, depsAlreadyInstalled)
  if (length(diffVersionNames)) {
    messageVerbose("Updating ", paste0(diffVersionNames, collapse = ", "), " in paths$packagePath ",
                   "because it has been updated in .libPaths()[1]. To turn this updating off, set\n",
                   "options(SpaDES.project.updateSelf = FALSE)")
    if (!isFALSE(getOption("SpaDES.project.updateSelf")))
      deps <- c(deps, diffVersionNames)
  }

  if (length(deps)) {
    messageVerbose("Copying ", paste(deps, collapse = ", "), " packages to paths$packagePath",
                   verbose = verbose)

    if (!identical(normPath(.libPaths()[1]), paths$packagePath))
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
  }

  changedLibPaths <- !identical(normPath(.libPaths()[1]), paths$packagePath)

  Require::setLibPaths(paths$packagePath, standAlone = standAlone,
                       updateRprofile = updateRprofile,
                       exact = FALSE, verbose = getOption("Require.verbose"))

  do.call(setPaths, paths[spPaths])

  messageVerbose(yellow("\b done setting up paths"), verbose = verbose)

  paths[order(names(paths))]
}



#' @export
#' @rdname setupProject
#'
#' @details
#' `setupSideEffects` can handle sequentially specified values, meaning a user can
#' first create a list of default options, then a list of user-desired options that
#' may or may not replace individual values. This can create hierarchies, *based on
#' order*.
#'
#' @return
#' `setupSideEffects` is run for its side effects, with nothing returned to user.
#'
#'
#' @importFrom data.table data.table
setupSideEffects <- function(name, sideEffects, paths, times, overwrite,
                             envir = environment(), verbose = getOption("Require.verbose", 1L), dots, ...) {

  dotsToHere(dots, ...)
  if (!missing(sideEffects)) {
    messageVerbose(yellow("setting up sideEffects..."), verbose = verbose)

    sideEffectsSUB <- substitute(sideEffects) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    sideEffects <- evalSUB(sideEffectsSUB, valObjName = "sideEffects", envir = environment(), envir2 = parent.frame())

    messageVerbose("Parsing and evaluating sideEffects", verbose = verbose)
    sideEffects <- parseFileLists(sideEffects, paths[["projectPath"]], namedList = FALSE,
                                  overwrite = overwrite, envir = envir, verbose = verbose)
  }
  messageVerbose(yellow("\b done setting up sideEffects"), verbose = verbose)

}


#' @export
#' @rdname setupProject
#'
#' @details
#' `setupOptions` can handle sequentially specified values, meaning a user can
#' first create a list of default options, then a list of user-desired options that
#' may or may not replace individual values. This can create hierarchies, *based on
#' order*.
#'
#' @return
#' `setupOptions` is run for its side effects, namely, changes to the `options()`.
#'
#'
#' @importFrom data.table data.table
setupOptions <- function(name, options, paths, times, overwrite, envir = environment(),
                         verbose = getOption("Require.verbose", 1L), dots, ...) {

  dotsToHere(dots, ...)

  newValuesComplete <- oldValuesComplete <- NULL
  if (!missing(options)) {

    messageVerbose(yellow("setting up options..."), verbose = verbose)

    preOptions <- options()

    optionsSUB <- substitute(options) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    options <- evalSUB(optionsSUB, valObjName = "options", envir = environment(), envir2 = parent.frame())

    options <- parseFileLists(options, paths[["projectPath"]], overwrite = overwrite,
                              envir = envir, verbose = verbose)

    postOptions <- options()
    newValues <- oldValues <- list()
    if (length(options)) {
      newValuesComplete <- options
      oldValuesComplete <- Map(nam = names(options), function(nam) NULL)
      oldValuesComplete[names(options)] <- preOptions[names(options)]
      whNULL <- which(lengths(oldValuesComplete) == 0)
      names(oldValuesComplete[unname(whNULL)]) <- names(options)[whNULL]
      newValues <- Require::setdiffNamed(options, preOptions)
      oldValues <- options(newValues)
      if (length(newValues)) {
        messageVerbose("The following options have been changed", verbose = verbose)
        messageDF(data.table::data.table(optionName = names(newValues), newValue = newValues,
                                         oldValue = oldValues), verbose = verbose)
      }
    }
  }
  return(invisible(list(newOptions = newValuesComplete, oldOptions = oldValuesComplete)))
}


isUnevaluatedList <- function(p) any({
  if (grepl("^if$|^<-$", p[[1]])[1]) {
    if (grepl("^\\{$", p[[3]][[1]])[1]) {
      grepl("^list$", p[[3]][[2]][[1]])
    } else {
      grepl("^list$", p[[3]][[1]])[1] || is.atomic(p[[3]])[1]
    }
  } else {
    grepl("^list$", p[[1]])[1]
  }
  })

parseListsSequentially <- function(files, namedList = TRUE, envir = parent.frame(),
                                   verbose = getOption("Require.verbose")) {
  envs <- list(envir) # means
  llOuter <- lapply(files, function(optFiles) {
    pp <- parse(optFiles)
    envs2 <- lapply(pp, function(p) {
      env <- new.env(parent = tail(envs, 1)[[1]])
      if (isUnevaluatedList(p) || isFALSE(namedList)) {
        # robust to code failures
        out <- try(eval(p, envir = env), silent = TRUE)
        if (is(out, "try-error")) {
          oo <- capture.output(p, type = "output")
          messageVerbose(optFiles, ":\n", paste(oo, collapse = "\n"),
                         "\ncould not be evaluated; skipping", verbose = verbose, verboseLevel = 2)
        } else {
          if (length(ls(env)) == 0)
            env$opt <- out
          envs <<- append(envs, list(env))
        }
      }
      env
    })

    os <- list()
    if (isTRUE(namedList)) {
      isNamedList <- mapply(env = envs2, function(env) {
        isNamedList <- FALSE
        if (length(ls(env))) {
          obj <- get(ls(env), env)
          if (is(obj, "list"))
            if (!is.null(names(obj)))
              isNamedList <- TRUE
        }
        isNamedList
      }, SIMPLIFY = TRUE)

      if (isTRUE(any(isNamedList))) {
        ll <- Map(env = envs2[isNamedList], function(env) {
          as.list(env)[[1]]
        })
        os <- Reduce(modifyList, ll)
      }
    }
    os
  })

  os <- Reduce(modifyList, llOuter)

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
setupModules <- function(paths, modules, useGit, overwrite, envir = environment(),
                         verbose = getOption("Require.verbose", 1L), dots, ...) {
  dotsToHere(dots, ...)

  if (missing(modules)) {
    modules <- character()
  } else {

    anyfailed <- character()
    modulesOrig <- modules
    modulesOrigPkgName <- extractPkgName(modulesOrig)
    if (!useGit) {
      out <- getModule(modules, paths$modulePath, overwrite)
      anyfailed <- out$failed
      modules <- anyfailed
    }

    if (isTRUE(useGit) || length(anyfailed)) {
      modulesWOat <- gsub("@.+$", "", modules)
      lapply(modulesWOat, function(m) {
        modPath <- file.path(paths$modulePath, extractPkgName(m))
        if (!dir.exists(modPath)) {
          cmd <- paste0("cd ", paths$modulePath, " && git clone https://github.com/", m)
          system(cmd)
        } else {
          messageVerbose("module exists at ", modPath, "; not cloning", verbose = verbose)
        }
      })
    }

    modulePackages <- packagesInModules(modulePath = paths$modulePath, modules = modulesOrigPkgName)
    modulesSimple <- Require::extractPkgName(modulesOrigPkgName)
    modules <- modulePackages[modulesSimple]
  }
  return(modules)

}


#' @export
#' @rdname setupProject
#' @details
#' `setupPackages` will read the modules' metadata `reqdPkgs` element. It will combine
#' these with any packages passed manually by the user to `packages`, and pass all
#' these packages to `Require::Install(...)`.
#' @param modulePackages A named list, where names are the module names, and the elements
#'   of the list are packages in a form that `Require::Require` accepts.
#'
#' @return
#' `setupPackages` is run for its side effects, i.e., installing packages to
#' `paths$packagePath`.
#'
setupPackages <- function(packages, modulePackages, require, libPaths, setLinuxBinaryRepo,
                          standAlone, envir = environment(), verbose, dots, ...) {

  dotsToHere(dots, ...)

  if (isTRUE(setLinuxBinaryRepo))
    Require::setLinuxBinaryRepo()

  if (missing(packages)) {
    packages <- NULL
  }
  messageVerbose(yellow("setting up packages..."), verbose = verbose)

  if (length(packages) || length(unlist(modulePackages))) { # length(require) ||
    messageVerbose("Installing any missing reqdPkgs", verbose = verbose)
    continue <- 3L
    while (continue) {
      mp <- unname(unlist(modulePackages))
      # requireToTry <- unique(c(mp, require))
      packagesToTry <- unique(c(packages, mp))
      # packagesToTry <- unique(c(packages, mp, requireToTry))
      out <- try(Require::Install(packagesToTry, # require = Require::extractPkgName(requireToTry),
                                  standAlone = standAlone,
                                  libPaths = libPaths,
                                  verbose = verbose))
      if (is(out, "try-error")) {
        deets <- gsub(".+Can't find ([[:alnum:]]+) on GitHub repo (.+); .+", paste0("\\2@\\1"), as.character(out))
        miss <- unlist(Map(mp = modulePackages, function(mp) grep(value = TRUE, pattern = deets, mp)))
        modulePackages[[names(miss)]] <- setdiff(modulePackages[[names(miss)]], miss)
        warning("Module ", names(miss), " has reqdPkgs ", paste0(miss, collapse = ", "),
                ", but branch don't exist; \nplease update module. ",
                "Omitting that package from this Require call which may mean ",
                Require::extractPkgName(unname(miss)), " doesn't get installed")
        continue <- continue - 1L
      } else {
        continue <- 0L
      }
    }
  }

  messageVerbose(".libPaths() are: ", paste(.libPaths(), collapse = ", "), verbose = verbose)
  messageVerbose(yellow("\b done setting up packages"), verbose = verbose)

  invisible(NULL)
}

#' @export
#' @rdname setupProject
#'
#' @return
#' `setupParams` is run for its side effects, namely, changes to the `options()`.
#'
#'
#' @importFrom data.table data.table
setupParams <- function(name, params, paths, modules, times, overwrite, envir = environment(),
                        verbose = getOption("Require.verbose", 1L), dots, ...) {

  dotsToHere(dots, ...)

  if (missing(params)) {
    params <- list()
  } else {

    messageVerbose(yellow("setting up params..."), verbose = verbose)

    paramsSUB <- substitute(params) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    params <- evalSUB(val = paramsSUB, valObjName = "params", envir = environment(), envir2 = parent.frame())
    params <- parseFileLists(params, paths[["projectPath"]], overwrite = overwrite,
                             envir = envir, verbose = verbose)

    if (length(params)) {

      modulesSimple <- Require::extractPkgName(modules)

      paramsForModules <- intersect(modulesSimple, names(params))
      overSupplied <- setdiff(names(params), c(".globals", paramsForModules))
      if (length(overSupplied)) {
        params <- params[paramsForModules]

        messageVerbose("Only returning params that are relevant for modules supplied.\n",
                       "Omitting parameters supplied for: ",
                       paste(overSupplied, collapse = ", "), verbose = verbose)
      }

      # This will shrink down to only the modulesSimple -- all others are gone
      hasDotGlobals <- isTRUE(".globals" %in% names(params))
      globs <- if (hasDotGlobals) params[[".globals"]] else NULL
      mods <- setdiff(names(params), ".globals")

      if (requireNamespace("SpaDES.core", quietly = TRUE)) {
        params <- Map(mod = mods, function(mod) {
          knownPars <- SpaDES.core::moduleMetadata(module = mod, path = paths$modulePath,
                                                   defineModuleListItems = "parameters")$parameters$paramName
          if (!is.null(params[[mod]])) {
            supplied <- names(params[[mod]])
            anyGlobals <- intersect(supplied, names(params[[".globals"]]))
            paramsInclGlobalsSupplied <- unique(c(supplied, anyGlobals))
            knownParsInMod <- paramsInclGlobalsSupplied %in% knownPars
            overInclusion <- knownParsInMod %in% FALSE
            if (any(overInclusion)) {
              params[[mod]] <- params[[mod]][paramsInclGlobalsSupplied[!overInclusion]]
              messageVerbose("These parameters set (",
                             paste(paramsInclGlobalsSupplied[knownParsInMod %in% FALSE], collapse = ", "),
                             "), but they are not in ", mod,
                             " metadata; this should be either added to the metadata, ",
                             "or not set in the params argument")
            }

          } else {
            messageVerbose("No parameters set for ", mod, verbose = verbose, verboseLevel = 2)
          }
          params[[mod]]
        })
      } else {
        messageVerbose("Skipping checking of parameters supplied against module parameters because ",
                       "SpaDES.core is not installed. Please install if this check is desired.",
                       verbose = verbose)
      }
      if (hasDotGlobals)
        params[[".globals"]] <- globs
      messageVerbose("The following params were created: ", verbose = verbose, verboseLevel = 2)
      messageVerbose(params, verbose = verbose, verboseLevel = 2)
    }
    messageVerbose(yellow("\b done setting up params"), verbose = verbose)
  }
  return(params)
}


parseFileLists <- function(obj, projectPath, namedList = TRUE, overwrite, envir,
                           verbose = getOption("Require.verbose", 1L), dots, ...) {

  if (is.character(obj)) {
    obj <- mapply(opt = obj, function(opt) {
      isGH <- isGitHub(opt)
      if (isGH) {
        opt <- getGithubFile(opt, destDir = projectPath, overwrite = overwrite)
      }
      opt
    }, SIMPLIFY = TRUE)
    areAbs <- isAbsolutePath(obj)
    if (any(areAbs %in% FALSE)) {
      obj[areAbs %in% FALSE] <- file.path(projectPath, obj)
    }

    obj <- parseListsSequentially(files = obj, namedList = namedList, envir = envir,
                                  verbose = verbose)

  }

  obj
}

checkProjectPath <- function(paths, envir, envir2) {


  if (missing(paths)) {
    paths <- list()
  }
  if (is.name(paths)) {
    paths <- evalSUB(paths, valObjName = "paths", envir = envir, envir2 = envir2)
  }
  if (is.null(paths[["projectPath"]])) {
    prjPth <- list(projectPath = normPath("."))
    paths <- append(prjPth, as.list(paths))
    paths <- paths[nzchar(names(paths))]
  }

  #  name <- basename(normPath(paths[["projectPath"]]))
  # }

  # if (missing(inProject))
  #   inProject <- isInProject(name)

  # projPth <- if  (inProject) file.path(".") else file.path(".", name)
  # projPth <- normPath(projPth)
  # if (missing(paths) || inProject) {
  #   paths <- list(projectPath = projPth)
  # }




  if (!is.null(paths[["projectPath"]])) {
    paths[["projectPath"]] <- evalSUB(paths[["projectPath"]], valObjName = "paths", envir, envir2)
   #  name <- basename(normPath(paths[["projectPath"]]))
  } else {
    stop("Must provide either a name or a paths[[\"projectPath\"]]")
  }

  paths
}

isInProject <- function(name) {
  identical(basename(getwd()), extractPkgName(name))
}

evalSUB <- function(val, valObjName, envir, envir2) {
  val2 <- val
  while (inherits(val, "call") || inherits(val, "name")) {
    if (inherits(val, "name"))
      val2 <- get0(val, envir = envir)
    else
      val2 <- eval(val, envir = envir)
    if ((identical(val2, val) && !missing(envir2)) || is.null(val2)) {
      val <- eval(val, envir = envir2)
      val2 <- val
    } else {
      val <- val2
    }
    if (missing(envir2))
      break
  }
  if (is(val2, "list") && !is.null(names(val2))) {
    env <- environment()
    # Sequential evaluation
    Map(nam = names(val2), function(nam) {
      val2[[nam]] <<- evalSUB(val2[[nam]], valObjName = valObjName,
                              envir = env, envir2 = envir)
      assign(valObjName, val2, envir = env)
    }
    )
    assign("val2", get(valObjName), envir = env)
  }
  val2
}


#' @export
#' @rdname setupProject
#' @details
#' `setupGitIgnore` will add.
#'
#' @return
#' `setupGitIgnore` is run for its side effects, i.e., adding elements to the
#' `.gitignore` file.
setupGitIgnore <- function(paths, envir = environment(), verbose, dots, ...) {

  dotsToHere(dots, ...)

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
    messageVerbose(verboseLevel = 1, verbose = verbose,
                             ".gitignore file updated with packagePath and modulePath; ",
                             "this may need to be confirmed manually")
  }
}



setPaths <- function(cachePath, inputPath, modulePath, outputPath, rasterPath, scratchPath,
                     terraPath, silent = FALSE, verbose = getOption("Require.verbose", 1L)) {
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
      messageVerbose(
        "Setting:\n",
        "  options(\n",
        if (!defaults$CP) paste0("    reproducible.cachePath = '", normPath(cachePath), "'\n"),
        if (!defaults$IP) paste0("    spades.inputPath = '", normPath(inputPath), "'\n"),
        if (!defaults$OP) paste0("    spades.outputPath = '", normPath(outputPath), "'\n"),
        if (!defaults$MP) paste0("    spades.modulePath = '" , modPaths, "'\n"),
        if (!defaults$SP) paste0("    spades.scratchPath = '", normPath(scratchPath), "'\n"),
        "  )",
        verbose = verbose
      )
    }

    if (any(unlist(defaults))) {
      messageVerbose(
        "Paths set to:\n",
        "  options(\n",
        "    rasterTmpDir = '", normPath(rasterPath), "'\n",
        "    reproducible.cachePath = '", normPath(cachePath), "'\n",
        "    spades.inputPath = '", normPath(inputPath), "'\n",
        "    spades.outputPath = '", normPath(outputPath), "'\n",
        "    spades.modulePath = '", modPaths, "'\n", # normPath'ed above
        "    spades.scratchPath = '", normPath(scratchPath), "'\n",
        "  )\n",
        "  terra::terraOptions(tempdir = '", normPath(terraPath), "'",
        verbose = verbose
      )
    }
  }

  return(invisible(originalPaths))
}

.paths <- function(verbose = getOption("Require.verbose", 1L)) {
  if (!is.null(.getOption("spades.cachePath"))) {
    messageVerbose("option('spades.cachePath') is being deprecated. Please use ",
            "option('reproducible.cachePath').\n",
            "Setting option('reproducible.cachePath' = getOption('spades.cachePath'))",
            verbose = verbose)
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


messageWarnStop <- function(..., type = getOption("SpaDES.project.messageWarnStop", "message"),
                            verbose = getOption("Require.verbose", 1L)) {
  type %in% c("message", "warning", "stop")

  typeFun <- utils::getFromNamespace(type, "base")
  typeFunArgs <- list(
    msg,
    call. = FALSE
  )
  if (type == "message") {
    typeFun <- messageVerbose
    typeFunArgs[["call."]] <- NULL
    typeFunArgs[["verbose"]] <- verbose

  }
  do.call(typeFun, typeFunArgs)

}


dotsToHere <- function(dots, ..., envir = parent.frame()) {
  if (missing(dots))
    dots <- list(...)
  else
    dots <- append(dots, list(...))
  list2env(dots, envir = envir)
}
