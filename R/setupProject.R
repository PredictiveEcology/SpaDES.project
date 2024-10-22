utils::globalVariables(c(
  ".", "i.module", "i.objectClass", "objectClass", "objectName",
  "i.newValue", "i.oldValue", "newValue", "oldValue", "modulesNoVersion"
))

#' Sets up a new or existing SpaDES project
#'
#' @description `setupProject` calls a sequence of functions in this order:
#' `setupOptions` (first time), `setupPaths`, `setupRestart`,
#' `setupFunctions`, `setupModules`, `setupPackages`, `setupSideEffects`,
#' `setupOptions` (second time), `setupParams`, and `setupGitIgnore`.
#'
#' This sequence will create folder structures, install missing packages from those
#' listed in either the `packages`, `require` arguments or in the modules `reqdPkgs` fields,
#' load packages (only those in the `require` argument), set options, download or
#' confirm the existence of modules. It will also return elements that can be passed
#' directly to `simInit`  or `simInitAndSpades`, specifically, `modules`, `params`,
#' `paths`, `times`, and any named elements passed to `...`. This function will also
#' , if desired, change the .Rprofile file for this project so that every time
#' the project is opened, it has a specific `.libPaths()`.
#'
#' There are a number of convenience elements described in the section below. See Details.
#' Because of this sequence, users can take advantage of settings (i.e., objects)
#' that happen (are created) before others. For example, users can set `paths`
#' then use the `paths` list to set `options` that will can update/change `paths`,
#' or set `times` and use the `times` list for certain entries in `params`.
#'
#'
#' @param name Optional. If supplied, the name of the project. If not supplied, an
#' attempt will be made to extract the name from the `paths[["projectPath"]]`.
#' If this is a GitHub project, then it should indicate the full Github
#' repository and branch name, e.g., `"PredictiveEcology/WBI_forecasts@ChubatyPubNum12"`
#' @param paths a list with named elements, specifically, `modulePath`, `projectPath`,
#'   `packagePath` and all others that are in `SpaDES.core::setPaths()`
#'   (i.e., `inputPath`, `outputPath`, `scratchPath`, `cachePath`, `rasterTmpDir`).
#'   Each of these has a sensible default, which will be overridden but any user
#'   supplied values.
#'   See [setup].
#' @param modules a character vector of modules to pass to `getModule`. These
#'   should be one of: simple name (e.g., `fireSense`) which will be searched for locally
#'   in the `paths[["modulePath"]]`; or a GitHub repo with branch (`GitHubAccount/Repo@branch` e.g.,
#'   `"PredictiveEcology/Biomass_core@development"`); or a character vector that identifies
#'   one or more module folders (local or GitHub) (not the module .R script).
#'   If the entire project is a git repository,
#'   then it will not try to re-get these modules; instead it will rely on the user
#'   managing their git status outside of this function.
#'   See [setup].
#' @param times Optional. This will be returned if supplied; if supplied, the values
#'   can be used in e.g., `params`, e.g., `params = list(mod = list(startTime = times$start))`.
#'   See help for `SpaDES.core::simInit`.
#' @param config Still experimental linkage to the `SpaDES.config` package. Currently
#'   not working.
#' @param packages Optional. A vector of packages that must exist in the `libPaths`.
#'   This will be passed to `Require::Install`, i.e., these will be installed, but
#'   not attached to the search path. See also the `require` argument. To force skip
#'   of package installation (without assessing modules), set `packages = NULL`
#' @param require Optional. A character vector of packages to install *and* attach
#'   (with `Require::Require`). These will be installed and attached at the start
#'   of `setupProject` so that a user can use these during `setupProject`.
#'   See [setup]
#' @param options Optional. Either a named list to be passed to `options`
#'   or a character vector indicating one or more file(s) to source,
#'   in the order provided. These will be parsed locally (not
#'   the `.GlobalEnv`), so they will not create globally accessible objects. NOTE:
#'   `options` is run 2x within `setupProject`, once before `setupPaths` and once
#'   after `setupPackages`. This occurs because many packages use options for their
#'   behaviour (need them set before e.g., `Require::require` is run; but many packages
#'   also change `options` at startup. See details.
#'   See [setup].
#' @param params Optional. Similar to `options`, however, this named list will be
#'   returned, i.e., there are no side effects.
#'   See [setup].
#' @param sideEffects Optional. This can be an expression or one or more file names or
#'   a code chunk surrounded by `{...}`.
#'   If a non-text file name is specified (e.g., *not .txt or .R* currently),
#'   these files will simply be downloaded, using their relative path as specified
#'   in the github notation. They will be downloaded or accessed locally at that
#'   relative path.
#'   If these file names represent scripts (*.txt or .R), this/these will be parsed and evaluated,
#'   but nothing is returned (i.e., any assigned objects are not returned). This is intended
#'   to be used for operations like cloud authentication or configuration functions
#'   that are run for their side effects only.
#' @param functions A set of function definitions to be used within `setupProject`.
#'   These will be returned as a list element. If function definitions require non-base
#'   packages, prefix the function call with the package e.g., `terra::rast`. When
#'   using `setupProject`, the `functions` argument is evaluated after `paths`, so
#'   it cannot be used to define functions that help specify `paths`.
#' @param useGit (if not FALSE, then experimental still). There are two levels at which a project
#'   can use GitHub, either the `projectPath` and/or the `modules`. Any given
#'   project can have one or the other, or both of these under git control. If "both",
#'   then this function will assume that git submodules will be used for the `modules`.
#'   A logical or `"sub"` for *submodule*. If `"sub"`, then this function
#'   will attempt to clone the identified `modules` *as git submodules*. This will only
#'   work if the `projectPath` is a git repository. If the project is already a git
#'   repository because the user has set that up externally to this function call, then
#'   this function will add the `modules` as git submodules. If it is not already,
#'   it will use `git clone` for each module. After git clone or submodule add are run,
#'   it will run `git checkout` for the named branch and then `git pull`
#'   to get and change branch for each module, according to its specification in
#'   `modules`. If `FALSE`, this function  will download modules with `getModules`.
#'   NOTE: *CREATING* A
#'   GIT REPOSITORY AT THE PROJECT LEVEL AND SETTING MODULES AS GIT SUBMODULES IS
#'   EXPERIMENTAL. IT IS FINE IF THE PROJECT HAS BEEN MANUALLY SET UP TO BE
#'   A GIT REPOSITORY WITH SUBMODULES: THIS FUNCTION WILL ONLY EVALUTE PATHS. This can
#'   be set with the `option(SpaDES.project.useGit = xxx)`.
#' @param standAlone A logical. Passed to `Require::standAlone`. This keeps all
#'   packages installed in a project-level library, if `TRUE`. Default is `TRUE`.
#' @param libPaths Deprecated. Use `paths = list(packagePath = ...)`.
#' @param Restart Logical or character. If either `TRUE` or a character,
#'   and if the `projectPath` is not the current path, and the session is in
#'   RStudio and interactive, it will try to restart Rstudio in the projectPath with
#'   a new Rstudio project. If character, it should represent the filename
#'   of the script that contains the `setupProject` call that should be copied to
#'   the new folder and opened. If `TRUE`, it will use the active file as the one
#'   that should be copied to the new projectPath and opened in the Rstudio project.
#'   If successful, this will create an RStudio Project file (and .Rproj.user
#'   folder), restart with a new Rstudio session with that new project and with a root
#'   path (i.e. working directory) set to `projectPath`. Default is `FALSE`, and no
#'   RStudio Project is created.
#' @param updateRprofile Logical. Should the `paths$packagePath` be set in the `.Rprofile`
#'   file for this project. Note: if `paths$packagePath` is within the `tempdir()`,
#'   then there will be a warning, indicating this won't persist. If the user is
#'   using `Rstudio` and the `paths$projectPath` is not the root of the current
#'   Rstudio project, then a warning will be given, indicating the .Rprofile may not
#'   be read upon restart.
#' @param setLinuxBinaryRepo Logical. Should the binary RStudio Package Manager be used
#'   on Linux (ignored if Windows)
#' @param studyArea Optional. If a list, it will be passed to
#'        `geodata::gadm`. To specify a country other than the default `"CAN"`,
#'        the list must have a named element, `"country"`. All other named elements
#'        will be passed to `gadm`. 2 additional named elements can be passed for
#'        convenience, `subregion = "..."`, which will be grepped with the column
#'        `NAME_1`, and `epsg = "..."`, so a user can pass an `epsg.io` code to
#'        reproject the `studyArea`. See examples.
#' @param overwrite Logical vector or character vector, however, only `getModule` will respond
#'   to a vector of values. If length-one `TRUE`, then all files that were previously downloaded
#'   will be overwritten throughout the sequence of `setupProject`. If a vector of
#'   logical or character, these will be passed to `getModule`: only the named
#'   modules will be overwritten or the logical vector of the modules.
#'   NOTE: if a vector, no other file specified anywhere in `setupProject` will be
#'   overwritten except a module that/those names, because
#'   only `setupModules` is currently responsive to a vector. To have fine grained control,
#'   a user can just manually delete a file, then rerun.
#' @param dots Any other named objects passed as a list a user might want for other elements.
#' @param defaultDots A named list of any arbitrary R objects.
#'   These can be supplied to give default values to objects that
#'   are otherwise passed in with the `...`, i.e., not specifically named for these
#'   `setup*` functions. If named objects are supplied as top-level arguments, then
#'   the `defaultDots` will be overridden. This can be particularly useful if the
#'   arguments passed to `...` do not always exist, but rely on external e.g., batch
#'   processing to optionally fill them. See examples.
#' @param envir The environment where `setupProject` is called from. Defaults to
#'   `parent.frame()` which should be fine in most cases and user shouldn't need
#'   to set this
#' @param ... further named arguments that acts like `objects`, but a different
#'   way to specify them. These can be anything. The general use case
#'   is to create the `objects` that are would be passed to
#'   `SpaDES.core::simInit`, or `SpaDES.core::simInitAndSpades`,
#'   (e.g. `studyAreaName` or `objects`) or additional objects to be passed to the simulation
#'   (in older versions of `SpaDES.core`, these were passed as a named list
#'   to the `objects` argument). **Order matters**. These are sequentially evaluated,
#'   and also any arguments that are specified before the named arguments
#'   e.g., `name`, `paths`, will be evaluated prior to any of the named arguments,
#'   i.e., "at the start" of the `setupProject`.
#'   If placed after the first named argument, then they will be evaluated at the
#'   end of the `setupProject`, so can access all the packages, objects, etc.
#'
#' @include imports.R
#' @export
#'
#' @section Faster runtime after project is set up:
#'
#' There are a number of checks that occur during `setupProject`. These take time, particularly
#' after an R restart (there is some caching in RAM that occurs, but this will only speed
#' things up if there is no restart of R). To get the "fastest", these options or settings
#' will speed things up, at the expense of not being completely re-runnable.
#' You can add one or more of these to the arguments. These will only be useful after a project
#' is set up, i.e., `setupProject` and `SpaDES.core::simInit` has/have been run at least once
#' to completion (so packages are installed).
#'
#' ```
#' options = c(
#'   reproducible.useMemoise = TRUE,               # For caching, use memory objects
#'   Require.cloneFrom = Sys.getenv("R_LIBS_USER"),# Use personal library as possible source of packages
#'   spades.useRequire = FALSE,                    # Won't install packages/update versions
#'   spades.moduleCodeChecks = FALSE,              # moduleCodeChecks checks for metadata mismatches
#'   reproducible.inputPaths = "~/allData"),       # For sharing data files across projects
#' packages = NULL,                                # Prevents any packages installs with setupProject
#' useGit = FALSE                                  # Prevents checks using git
#' ```
#' These will be set early in `setupProject`, so will affect the running of `setupProject`.
#' If the user manually sets one of these in addition to setting these, the user options will
#' override these.
#' The remining causes of `setupProject` being "slow" will be loading the required packages.
#'
#' These options/arguments can now be set all at once
#' (with caution as these changes will affect how your
#' script will be run) with `options(SpaDES.project.fast = TRUE)` or in the `options` argument.
#'
#'
#' @section Objective:
#'
#' The overarching objectives for these functions are:
#'
#'   \enumerate{
#'     \item To prepare what is needed for `simInit`.
#'     \item To help a user eliminate virtually all assignments to the `.GlobalEnv`,
#'           as these create and encourage spaghetti code that becomes unreproducible
#'           as the project increases in complexity.
#'     \item Be very simple for beginners, but powerful enough to expand to almost
#'           any needs of arbitrarily complex projects, using the same structure
#'     \item Deal with the complexities of R package installation and loading when
#'           working with modules that may have been created by many users
#'     \item Create a common SpaDES project structure, allowing
#'           easy transition from one project to another, regardless of complexity.
#'   }
#'
#'
#' @section Convenience elements:
#'
#' \subsection{Sequential evaluation}{
#' Throughout these functions, efforts have been made to implement sequential evaluation,
#' within files and within lists. This means that a user can *use* the values from an
#' upstream element in the list. For example, the following where `projectPath` is
#' part of the list that will be assigned to the `paths` argument and it is then
#' used in the subsequent list element is valid:
#'
#' ```
#' setupPaths(paths = list(projectPath = "here",
#'                         modulePath = file.path(paths[["projectPath"]], "modules")))
#' ```
#' Because of such sequential evaluation, `paths`, `options`, and `params` files
#' can be sequential lists that have impose a hierarchy specified
#' by the order. For example, a user can first create a list of *default* options,
#' then several lists of user-desired options behind an `if (user("emcintir"))`
#' block that add new or override existing elements, followed by `machine` specific
#' values, such as paths.
#' }
#'
#' ```
#' setupOptions(
#'   maxMemory <- 5e+9 # if (grepl("LandWeb", runName)) 5e+12 else 5e+9
#'
#'   # Example -- Use any arbitrary object that can be passed in the `...` of `setupOptions`
#'   #  or `setupProject`
#'   if (.mode == "development") {
#'      list(test = 2)
#'   }
#'   if (machine("A127")) {
#'     list(test = 3)
#'   }
#' )
#' ````
#'
#' \subsection{Values and/or files}{
#' The arguments, `paths`, `options`, and `params`, can all
#' understand lists of named values, character vectors, or a mixture by using a list where
#' named elements are values and unnamed elements are character strings/vectors. Any unnamed
#' character string/vector will be treated as a file path. If that file path has an `@` symbol,
#' it will be assumed to be a file that exists on a GitHub repository in `https://github.com`.
#' So a user can pass values, or pointers to remote and/or local paths that themselves have values.
#'
#' The following will set an option as declared, plus read the local file (with relative
#' path), plus download and read the cloud-hosted file.
#'
#' ```
#' setupProject(
#'    options = list(reproducible.useTerra = TRUE,
#'                   "inst/options.R",
#'                   "PredictiveEcology/SpaDES.project@transition/inst/options.R")
#'                  )
#'    )
#' ```
#' This approach allows for an organic growth of complexity, e.g., a user begins with
#' only named lists of values, but then as the number of values increases, it may be
#' helpful to put some in an external file.
#'
#' NOTE: if the GitHub repository is *private* the user *must* configure their GitHub
#' token by setting the GITHUB_PAT environment variable -- unfortunately, the `usethis`
#' approach to setting the token will not work at this moment.
#' }
#'
#' \subsection{Specifying `paths`, `options`, `params`}{
#' If `paths`, `options`, and/or `params` are a character string
#' or character vector (or part of an unnamed list element) the string(s)
#' will be interpreted as files to parse. These files should contain R code that
#' specifies *named lists*, where the names are one or more `paths`, `options`,
#' or are module names, each with a named list of parameters for that named module.
#' This last named list for `params` follows the convention used for the `params` argument in
#' `simInit(..., params = )`.
#'
#' These files can use `paths`, `times`, plus any previous list in the sequence of
#' `params` or `options` specified. Any functions that are used must be available,
#'  e.g., prefixed `Require::normPath` if the package has not been loaded (as recommended).
#'
#' If passing a file to `options`, it should **not set** `options()` explicitly;
#' only create named lists. This enables options checking/validating
#' to occur within `setupOptions` and `setupParams`. A simplest case would be a file with this:
#' `opts <- list(reproducible.destinationPath = "~/destPath")`.
#'
#' All named lists will be parsed into their own environment, and then will be
#' sequentially evaluated (i.e., subsequent lists will have access to previous lists),
#' with each named elements setting or replacing the previously named element of the same name,
#' creating a single list. This final list will be assigned to, e.g., `options()` inside `setupOptions`.
#'
#' Because each list is parsed separately, they to not need to be assigned objects;
#' if they are, the object name can be any name, even if similar to another object's name
#' used to built the same argument's (i.e. `paths`, `params`, `options`) final list.
#' Hence, in an file to passed to `options`, instead of incrementing the list as:
#'
#' ```
#' a <- list(optA = 1)
#' b <- append(a, list(optB = 2))
#' c <- append(b, list(optC = 2.5))
#' d <- append(c, list(optD = 3))
#' ```
#'
#' one can do:
#' ```
#' a <- list(optA = 1)
#' a <- list(optB = 2)
#' c <- list(optC = 2.5)
#' list(optD = 3)
#' ```
#'
#' NOTE: only atomics (i.e., character, numeric, etc.), named lists, or either of these
#'   that are protected by 1 level of "if" are parsed. This will not work, therefore,
#'   for other side-effect elements, like authenticating with a cloud service.
#'
#' Several helper functions exist within `SpaDES.project` that may be useful, such
#' as `user(...)`, `machine(...)`
#' }
#'
#' \subsection{Can hard code arguments that may be missing}{
#' To allow for batch submission, a user can specify code `argument = value` even if `value`
#' is missing. This type of specification will not work in normal parsing of arguments,
#' but it is designed to work here. In the next example, `.mode = .mode` can be specified,
#' but if R cannot find `.mode` for the right hand side, it will just skip with no error.
#' Thus a user can source a script with the following line from batch script where `.mode`
#' is specified. When running this line without that batch script specification, then this
#' will assign no value to `.mode`. We include `.nodes` which shows an example of
#' passing a value that does exist. The non-existent `.mode` will be returned in the `out`,
#' but as an unevaluated, captured list element.
#'
#' ```
#' .nodes <- 2
#' out <- setupProject(.mode = .mode,
#'                     .nodes = .nodes,
#'                     options = "inst/options.R"
#'                     )
#' ```
#' }
#'
#' @return
#' `setupProject` will return a named list with elements `modules`, `paths`, `params`, and `times`.
#' The goal of this list is to contain list elements that can be passed directly
#' to `simInit`.
#'
#' It will also append all elements passed by the user in the `...`.
#' This list  can be passed directly to `SpaDES.core::simInit()` or
#' `SpaDES.core::simInitAndSpades()` using a `do.call()`. See example.
#'
#' NOTE: both `projectPath` and `packagePath` will be omitted in the `paths` list
#' as they are used to set current directory (found with `getwd()`) and `.libPaths()[1]`,
#' but are not accepted by `simInit`. `setupPaths` will still return these two paths as its
#' outputs are not expected to be passed directly to `simInit` (unlike `setupProject` outputs).
#'
#' @importFrom Require extractPkgName
#' @importFrom stats na.omit
#' @inheritParams Require::Require
#' @inheritParams Require::setLibPaths
#' @rdname setupProject
#' @seealso [setupPaths()], [setupOptions()], [setupPackages()],
#' [setupModules()], [setupGitIgnore()]. Also, helpful functions such as
#' [user()], [machine()], [node()]
#' @seealso `vignette("i-getting-started", package = "SpaDES.project")`
#'
#' @examples
#' ## For more examples:
#' vignette("i-getting-started", package = "SpaDES.project")
#'
#' library(SpaDES.project)
#'
#' \dontshow{origDir <- getwd()
#'           tmpdir <- Require::tempdir2() # for testing tempdir2 is better}
#' \dontshow{
#' if (is.null(getOption("repos"))) {
#'   options(repos = c(CRAN = "https://cloud.r-project.org"))
#'   }
#'   setwd(tmpdir)
#' }
#'  ## simplest case; just creates folders
#' out <- setupProject(
#'   paths = list(projectPath = ".") #
#' )
#' \dontshow{setwd(origDir)}
setupProject <- function(name, paths, modules, packages,
                         times, options, params, sideEffects, functions, config,
                         require = NULL, studyArea = NULL,
                         Restart = getOption("SpaDES.project.Restart"),
                         useGit = getOption("SpaDES.project.useGit"),
                         setLinuxBinaryRepo = getOption("SpaDES.project.setLinuxBinaryRepo"),
                         standAlone = getOption("SpaDES.project.standAlone"),
                         libPaths = NULL,
                         updateRprofile = getOption("SpaDES.project.updateRprofile"),
                         overwrite = getOption("SpaDES.project.overwrite"), # envir = environment(),
                         verbose = getOption("Require.verbose", 1L),
                         defaultDots, envir = parent.frame(),
                         dots, ...) {

  makeUpdateRprofileSticky(updateRprofile)

  assignDefaults(env = environment())

  origGetWd <- getwd()
  if (isTRUE(Restart))
    on.exit(setwd(origGetWd), add = TRUE)

  envirCur = environment()

  origArgOrder <- names(tail(sys.calls(), 1)[[1]])
  if (is.null(origArgOrder)) {
    firstNamedArg <- 0
  } else {
    argsAreInFormals <- origArgOrder %in% formalArgs(setupProject)
    firstNamedArg <- if (isTRUE(any(argsAreInFormals))) min(which(argsAreInFormals)) else Inf
  }

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsLater <- dotsSUB
  if (firstNamedArg > 2) { # there is always an empty one at first slot
    firstSet <- if (is.infinite(firstNamedArg)) seq(length(origArgOrder) - 1) else (1:(firstNamedArg - 2))
    dotsLater <- dotsSUB[-firstSet]
    dotsSUB <- dotsSUB[firstSet]
    dotsSUB <- dotsToHereOuter(dots, dotsSUB, defaultDots)
  }

  functionsSUB <- substitute(functions)
  timesSUB <- substitute(times) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  if (!missing(timesSUB))
    times <- evalSUB(val = timesSUB, envir = envirCur, valObjName = "times", envir2 = envir)
  modulesSUB <- substitute(modules) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  paramsSUB <- substitute(params) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  optionsSUB <- substitute(options) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = paths$projectpath)`
  sideEffectsSUB <- substitute(sideEffects)
  libPaths <- substitute(libPaths)

  if (missing(times))
    times <- list(start = 0, end = 1)

  pathsSUB <- checkProjectPath(pathsSUB, name, envir = envirCur, envir2 = envir)
  if (missing(name)) {
    name <- basename(normPath(pathsSUB[["projectPath"]]))
  } else {
    name <- checkNameProjectPathConflict(name, pathsSUB)
  }

  inProject <- isInProject(name)

  # setupOptions is run twice -- because package startup often changes options
  optsFirst <- setupOptions(name, optionsSUB, pathsSUB, times, overwrite = isTRUE(overwrite),
                            envir = envirCur, useGit = useGit,
                            updateRprofile = updateRprofile,
                            verbose = verbose - 1)

  if (isTRUE(getOption("SpaDES.project.fast"))) {
    base::options(fastOptions())
    packages <- NULL
    useGit <- FALSE
  }

  paths <- setupPaths(name, pathsSUB, inProject, standAlone, libPaths,
                      Restart = Restart, defaultDots = defaultDots,
                      useGit = useGit,
                      updateRprofile = updateRprofile, verbose = verbose) # don't pass envir because paths aren't evaluated yet
  inProject <- isInProject(name)

  setupRestart(updateRprofile = updateRprofile, paths, name, inProject, useGit = useGit,
               Restart = Restart, origGetWd, verbose) # This may restart

  # Need to assess if this is a new project locally, but the remote exists
  usingGit <- checkUseGit(useGit)
  gitUserName <- NULL
  if (isTRUE(usingGit)) {
    isLocalGitRepoAlready <- isProjectGitRepo(pathsSUB$projectPath, inProject)
    if (isFALSE(isLocalGitRepoAlready)) {
      gitUserName <- checkGitRemote(name, pathsSUB)
    }
  }

  # this next puts them in this environment, returns NULL
  functions <- setupFunctions(functionsSUB, paths = paths, envir = envirCur)

  modulePackages <- setupModules(name, paths, modulesSUB, inProject = inProject, useGit = useGit,
                                 gitUserName = gitUserName, updateRprofile = updateRprofile,
                                 overwrite = overwrite, envir = envirCur, verbose = verbose)
  modules <- extractModName(names(modulePackages))
  names(modules) <- names(modulePackages)

  if (missing(packages))
    packages <- character()

  setupPackages(packages, modulePackages, require = require, paths = paths,
                setLinuxBinaryRepo = setLinuxBinaryRepo,
                standAlone = standAlone,
                libPaths = paths[["packagePath"]], envir = envirCur, verbose = verbose)

  # This next is to set the terra tempdir; don't do it in the cases where terra is not used
  # The longer unique(...) commented next is much slower; they are identical results
  # allPkgs <- unique(Require::extractPkgName(c(packages, unname(unlist(modulePackages)))))
  allPkgs <- c(packages, unname(unlist(modulePackages)))
  if (any(grepl("\\<terra\\>", allPkgs))) {
    terra::terraOptions(tempdir = paths$terraPath)
  }

  sideEffectsSUB <- setupSideEffects(name, sideEffectsSUB, paths, times, overwrite = isTRUE(overwrite),
                                     envir = envirCur, verbose = verbose)

  # 2nd time
  opts <- setupOptions(name, optionsSUB, paths, times, overwrite = isTRUE(overwrite), envir = envirCur,
                       useGit = useGit, updateRprofile = updateRprofile, verbose = verbose - 1)
  if (!is.null(opts$newOptions))
    opts <- mergeOpts(opts, optsFirst, verbose)

  if (isTRUE(getOption("SpaDES.project.fast"))) {
    message("Using fast options:")
    df <- fastOptions()[!names(fastOptions()) %in% names(opts[["newOptions"]])]
    df <- df[!sapply(df, is.null)]
    reproducible::messageDF(data.frame(option = names(df), val = unlist(df)))
  }

  options <- opts[["newOptions"]] # put into this environment so parsing can access

  # Run 2nd time after sideEffects & setupOptions -- may not be necessary
  # setupPackages(packages, modulePackages, require = require,
  #               setLinuxBinaryRepo = setLinuxBinaryRepo,
  #               standAlone = standAlone,
  #               libPaths = paths[["packagePath"]], envir = envirCur, verbose = verbose)

  if (!missing(config)) {
    # messageVerbose("config is supplied; using `SpaDES.config` package internals", verbose = verbose)
    # if (!requireNamespace("SpaDES.config", quietly = TRUE)) {
    #   Require::Install("PredictiveEcology/SpaDES.config@development")
    # }
    messageWarnStop("config is not yet setup to run with SpaDES.project")
    # if (FALSE)
    #   out <- do.call(SpaDES.config::useConfig, append(
    #     list(projectName = config,
    #          projectPath = paths[["projectPath"]], paths = paths),
    #     localVars))

  }

  # TODO from here to out <-  should be brought into the "else" block when `SpaDES.config is worked on`
  params <- setupParams(name, paramsSUB, paths, modules, times, options = opts[["newOptions"]],
                        overwrite = isTRUE(overwrite), envir = envirCur, verbose = verbose)

  studyAreaSUB <- substitute(studyArea)
  if (!is.null(studyAreaSUB)) {
    dotsSUB$studyArea <- setupStudyArea(studyAreaSUB, paths, envir = parent.frame(), verbose = verbose)
    studyArea <- dotsSUB$studyArea
  }

  if (length(dotsLater)) {
    dotsLater <- dotsToHereOuter(dots, dotsLater, defaultDots)
    dotsSUB <- Require::modifyList2(dotsSUB, dotsLater)
  }

  setupGitIgnore(paths, gitignore = getOption("SpaDES.project.gitignore", TRUE), verbose)

  # Put Dots in order
  if (length(dotsSUB) > 1)
    dotsSUB <- dotsSUB[na.omit(match(origArgOrder, names(dotsSUB)))]

  ## Ceres:  no longer necessary as setupModules now pulls "inner" modules into modulePath
  # anyInnerModules <- unique(fileRelPathFromFullGHpath(names(modules)))

  # if (any(nzchar(anyInnerModules))) {
  #   paths[["modulePath"]] <- unique(c(paths[["modulePath"]], file.path(paths[["modulePath"]], anyInnerModules)))
  # }

  pathsOrig <- paths
  extras <- setdiff(names(paths), spPaths)
  paths <- paths[spPaths]
  attr(paths, "extraPaths") <- pathsOrig[extras]

  out <- append(list(
    modules = modules,
    paths = paths, # this means we lose the packagePath --> but it is in .libPaths()[1]
    # we also lose projectPath --> but this is getwd()
    params = params,
    times = times), dotsSUB)
  if (!is.null(options)) {
    attr(out, "projectOptions") <- opts$updates
  }

  return(out)
}

#' Individual `setup*` functions that are contained within `setupProject`
#'
#' These functions will allow more user control, though in most circumstances,
#' it should be unnecessary to call them directly.
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
#' `setupPaths` returns a list of paths that are created. `projectPath` will be
#' assumed to be the base of other non-temporary and non-R-library paths. This means
#' that all paths that are directly used by `simInit` are assumed to be relative
#' to the `projectPath`. If a user chooses to specify absolute paths, then they will
#' be returned as is. It is also called for its
#' side effect which is to call `setPaths`, with each of these paths as an argument.
#' See table for details. If a user supplies extra paths not useable by `SpaDES.core::simInit`,
#' these will added as an attribute ("extraPaths") to the `paths` element
#' in the returned object. These will still exist directly in the returned list
#' if a user uses `setupPaths` directly, but these will not be returned with
#' `setupProject` because `setupProject` is intended to be used with `SpaDES.core::simInit`.
#' In addition, three paths will be added to this same attribute automatically:
#' `projectPath`, `packagePath`, and `.prevLibPaths` which is the previous value for
#' `.libPaths()` before changing to `packagePath`.
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
#'                                                             `Restart = FALSE`\cr
#' `packagePath`\tab `file.path(tools::R_user_dir("data"), name, "packages",
#'                               version$platform, substr(getRversion(), 1, 3))`
#'                                                    \tab appends this path to `.libPaths(packagePath)`,
#'                                                         unless `standAlone = TRUE`, in which case,
#'                                                         it will set `.libPaths(packagePath,
#'                                                         include.site = FALSE)` to this path \cr
#' ------       \tab -----------                 \tab  -----         \cr
#'              \tab *Module Level Paths*                 \tab \cr
#' `cachePath`  \tab `file.path(projectPath, "cache")` \tab `options(reproducible.cachePath = cachePath)`\cr
#' `inputPath`  \tab `file.path(projectPath, "inputs")` \tab `options(spades.inputPath = inputPath)`\cr
#' `modulePath` \tab `file.path(projectPath, "modules")` \tab `options(spades.inputPath = outputPath)` \cr
#' `outputPath` \tab `file.path(projectPath, "outputs")` \tab `options(spades.inputPath = modulePath)` \cr
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
#' @inheritParams setupProject
#' @param inProject A logical. If `TRUE`, then the current directory is
#'  inside the `paths[["projectPath"]]`.
#' @rdname setup
#' @param envir An environment within which to look for objects. If called alone,
#' the function should use its own internal environment. If called from another
#' function, e.g., `setupProject`, then the `envir` should be the internal
#' transient environment of that function.
#' @importFrom Require normPath checkPath
#' @importFrom utils packageVersion
setupPaths <- function(name, paths, inProject, standAlone = TRUE, libPaths = NULL,
                       updateRprofile = getOption("SpaDES.project.updateRprofile", TRUE),
                       Restart = getOption("SpaDES.project.Restart", FALSE),
                       overwrite = FALSE, envir = parent.frame(),
                       useGit = getOption("SpaDES.project.useGit", FALSE),
                       verbose = getOption("Require.verbose", 1L), dots, defaultDots, ...) {

  envirCur <- environment()
  makeUpdateRprofileSticky(updateRprofile)

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  messageVerbose(yellow("setting up paths ..."), verbose = verbose, verboseLevel = 0)

  pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = file.path(paths[["projectPath"]]))`
  pathsSUB <- checkProjectPath(pathsSUB, name, envir, parent.frame())

  paths <- evalSUB(val = pathsSUB, valObjName = "paths", envir = envirCur, envir2 = envir)
  paths <- parseFileLists(paths, paths, overwrite = isTRUE(overwrite),
                          envir = envir, verbose = verbose)

  if (!missing(name))
    name <- checkNameProjectPathConflict(name, paths)

  if (missing(name)) {
    name <- basename(paths[["projectPath"]])
  }
  if (missing(inProject))
    inProject <- isInProject(name)
  if (is.null(paths[["projectPath"]]))
    stop("Please specify paths[[\"projectPath\"]] as an absolute path")

  if (!is.null(libPaths)) {
    warning("libPaths argument is deprecated. Pass to `paths = list(packagePath = ...)`",
            "; it is being ignored", verbose = verbose)
    libPaths <- NULL
  }
  #if (is.null(libPaths) || is.call(libPaths)) {
  if (is.null(paths[["packagePath"]])) {
    paths[["packagePath"]] <- .libPathDefault(name)
  }

  defaultsSPO <- spadesProjectOptions() # uses projectPath
  if (is.null(paths[["modulePath"]]))
    paths[["modulePath"]] <- basename(defaultsSPO$spades.modulePath) # "modules"
  isAbs <- unlist(lapply(paths, isAbsolutePath))
  toMakeAbsolute <- isAbs %in% FALSE & rep(names(paths), lengths(paths)) != "projectPath"
  if (isTRUE(any(toMakeAbsolute))) {
    firstPart <- paste0("^", paths[["projectPath"]], "(/|\\\\)")
    alreadyHasProjectPath <- unlist(lapply(paths[toMakeAbsolute], grepl, # value = TRUE,
                                           pattern = firstPart))
    if (isTRUE(any(alreadyHasProjectPath)))
      paths[toMakeAbsolute][alreadyHasProjectPath] <-
      gsub(firstPart, replacement = "", paths[toMakeAbsolute][alreadyHasProjectPath])

  }
  if (inProject) {
    paths[["projectPath"]] <- normPath(".") # checkPaths will make an absolute
  }
  # on linux, `normPath` doesn't expand if path doesn't exist -- so create first
  paths[["projectPath"]] <- checkPath(paths[["projectPath"]], create = TRUE)
  paths[["projectPath"]] <- normPath(paths[["projectPath"]]) # expands

  if (isTRUE(any(toMakeAbsolute))) {
    paths[toMakeAbsolute] <- lapply(paths[toMakeAbsolute], function(x) file.path(paths[["projectPath"]], x))
  }
  paths <- lapply(paths, checkPath, create = TRUE)

  if (!inProject) {
    setwd(checkPath(paths[["projectPath"]], create = TRUE))
  }

  defaultsSPO <- spadesProjectOptions() # uses projectPath, so need updated
  if (is.null(paths$scratchPath)) {
    paths$scratchPath <- getOption("spades.scratchPath",
                                   defaultsSPO$spades.scratchPath) # file.path(tempdir(), name)
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
    list(cachePath = getOption("reproducible.cachePath",
                               defaultsSPO$reproducible.cachePath),
         inputPath = getOption("spades.inputPath",
                               defaultsSPO$spades.inputPath),
         outputPath = getOption("spades.outputPath",
                                defaultsSPO$spades.outputPath)
    # list(cachePath = file.path(paths[["projectPath"]], "cache"),
    #      inputPath = file.path(paths[["projectPath"]], "inputs"),
    #      outputPath = file.path(paths[["projectPath"]], "outputs")
    ),
    paths)

  paths <- lapply(paths, normPath)

  changedLibPaths <- (!identical(normPath(.libPaths()[1]), paths[["packagePath"]]) &&
                        (!identical(dirname(normPath(.libPaths()[1])), paths[["packagePath"]])))
  needSetLibPathsNow <- !Restart %in% TRUE || inProject %in% TRUE
  if (isTRUE(changedLibPaths)) {
    deps <- lapply(c("SpaDES.project", "Require"), function(pkg) {
      PackagePath <- getNamespaceInfo(pkg, "path")
      DESCinLibPaths <- file.path(PackagePath, "DESCRIPTION")
      DESCRIPTIONFileDeps(DESCinLibPaths)
    })

    deps <- unique(c("SpaDES.project", Require::extractPkgName(unlist(deps))))
    setupSpaDES.ProjectDeps(paths, verbose = verbose, deps = deps)
    needSetLibPaths <- TRUE
  } else {
    needSetLibPaths <- needSetLibPathsNow
  }

  prevLibPaths <- .libPaths()
  if (needSetLibPaths) {
    if (!useGit %in% FALSE) {
      # requireNamespace will find usethis in memory when devtools is used, but it fails because other
      #   deps of usethis are not in the deps of devtools --> can't use `require` b/c CRAN rules
      #   so need to load before changing .libPaths
      deps <- Require::pkgDep("usethis")
      depsSimple <- c("usethis", Require::extractPkgName(unname(unlist(deps))))
      loaded <- sapply(depsSimple, function(pkg) {
        requireNamespace(pkg, quietly = TRUE)
      })
    }
    setLPCall <- quote(Require::setLibPaths(paths[["packagePath"]], standAlone = standAlone,
                                            updateRprofile = updateRprofile,
                                            exact = FALSE, verbose = verbose))
    prevLibPaths <- if (verbose < 0) {
      out <- capture.output(type = "message", ret <- eval(setLPCall))
      ret
      } else {
        eval(setLPCall)
      }
    if (needSetLibPathsNow %in% FALSE)
      on.exit(Require::setLibPaths(prevLibPaths), add = TRUE)
    paths[["packagePath"]] <- .libPaths()[1]
  }

  if (any(lengths(paths) == 0)) {
    paths[lengths(paths) == 0] <- Map(p = names(paths)[lengths(paths) == 0], function(p) {
      spo <- spadesProjectOptions()
      spo[[grep(p, names(spo))]]
    }
    )
  }
  a <- try(do.call(setPaths, append(paths[spPaths], list(verbose = verbose))))
  if (is(a, "try-error")) browser()

  messageVerbose(yellow("  done setting up paths"), verbose = verbose, verboseLevel = 0)

  paths <- paths[order(names(paths))]
  paths[[".previousLibPaths"]] <- prevLibPaths

  pathsOrig <- paths
  extras <- setdiff(names(paths), spPaths)
  attr(paths, "extraPaths") <- paths[extras]

  if (needSetLibPaths) {
    if (!useGit %in% FALSE) {
      if (any(!loaded)) {
        rlout <- readline("usethis package must be installed; install now? (y or n)")
        if (startsWith(tolower(rlout), "y"))
          Require::Install("usethis")
        else
          stop("Setting `useGit = *something*` requires that usethis be installed. Please install it.")
      }
  }}

  paths
}


#' @export
#' @rdname setup
#'
#' @details
#' `setupFunctions` will source the functions supplied, with a parent environment being
#' the internal temporary environment of the `setupProject`, i.e., they will have
#' access to all the objects in the call.
#'
#' @return
#' `setupFunctions` returns NULL. All functions will be placed in `envir`.
#'
#' @importFrom data.table data.table
#' @examples
#'
#' \dontshow{origDir <- getwd()
#'           tmpdir <- Require::tempdir2() # for testing tempdir2 is better}
#' \dontshow{
#' if (is.null(getOption("repos"))) {
#'   options(repos = c(CRAN = "https://cloud.r-project.org"))
#'   }
#'   setwd(tmpdir)
#' }
#'  ## simplest case; just creates folders
#' out <- setupProject(
#'   paths = list(projectPath = ".") #
#' )
#' # specifying functions argument, with a local file and a definition here
#' tf <- tempfile(fileext = ".R")
#' fnDefs <- c("fn <- function(x) x\n",
#'             "fn2 <- function(x) x\n",
#'             "fn3 <- function(x) terra::rast(x)")
#' cat(text = fnDefs, file = tf)
#' funHere <- function(y) y
#' out <- setupProject(functions = list(a = function(x) return(x),
#'                                      tf,
#'                                      funHere = funHere), # have to name it
#'                     # now use the functions when creating objects
#'                     drr = 1,
#'                     b = a(drr),
#'                     q = funHere(22),
#'                     ddd = fn3(terra::ext(0,b,0,b)))
#' \dontshow{setwd(origDir)}
setupFunctions <- function(functions, name, sideEffects, paths, overwrite = FALSE,
                           envir = parent.frame(), verbose = getOption("Require.verbose", 1L),
                           dots, defaultDots, ...) {

  envirCur <- environment()
  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (!missing(functions)) {
    messageVerbose(yellow("setting up functions..."), verbose = verbose, verboseLevel = 0)

    functionsSUB <- substitute(functions) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    functions <- evalSUB(functionsSUB, valObjName = "functions", envir = envir, envir2 = envir)

    functions <- parseFileLists(functions, paths = paths, namedList = TRUE,
                                overwrite = isTRUE(overwrite), envir = envir, verbose = verbose)
    isFuns <- vapply(functions, is.function, FUN.VALUE = logical(1))
    if (any(isFuns))
      list2env(functions[isFuns], envir = envir)
    messageVerbose(yellow("  done setting up functions"), verbose = verbose, verboseLevel = 0)
  }

}








#' @export
#' @rdname setup
#'
#' @details
#' Most arguments in the family of `setup*` functions are run *sequentially*, even within
#' the argument. Since most arguments take lists, the user can set values at a first
#' value of a list, then use it in calculation of the 2nd value and so on. See
#' examples. This "sequential" evaluation occurs in the `...`, `setupSideEffects`, `setupOptions`,
#' `setupParams` (this does not work for `setupPaths`) can handle sequentially
#' specified values, meaning a user can
#' first create a list of default options, then a list of user-desired options that
#' may or may not replace individual values. This can create hierarchies, *based on
#' order*.
#'
#' @return
#' `setupSideEffects` is run for its side effects (e.g., web authentication, custom package
#' options that cannot use `base::options`), with deliberately nothing returned to user.
#' This, like other parts of this function, attempts to prevent unwanted outcomes
#' that occur when a user uses e.g., `source` without being very careful about
#' what and where the objects are sourced to.
#'
#'
#' @importFrom data.table data.table
setupSideEffects <- function(name, sideEffects, paths, times, overwrite = FALSE,
                             envir = parent.frame(), verbose = getOption("Require.verbose", 1L),
                             dots, defaultDots, ...) {

  envirCur <- environment()
  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (!missing(sideEffects)) {
    messageVerbose(yellow("setting up sideEffects..."), verbose = verbose, verboseLevel = 0)

    sideEffectsSUB <- substitute(sideEffects) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    sideEffects <- evalSUB(sideEffectsSUB, valObjName = "sideEffects", envir = envirCur, envir2 = envir)

    if (!is.character(sideEffects)) { # this is because I wrote this second;
      tf <- tempfile()
      writeLines(format(sideEffects[-1]), con = tf)
      sideEffects <- tf
    }

    sideEffects <- parseFileLists(sideEffects, paths, namedList = FALSE,
                                  overwrite = isTRUE(overwrite), envir = envir, verbose = verbose)
    messageVerbose(yellow("  done setting up sideEffects"), verbose = verbose, verboseLevel = 0)
  }

}


#' @export
#' @rdname setup
#'
#' @details
#' `setupOptions` can handle sequentially specified values, meaning a user can
#' first create a list of default options, then a list of user-desired options that
#' may or may not replace individual values. Thus final values will be based on the
#' order that they are provided.
#'
#' @return
#' `setupOptions` is run for its side effects, namely, changes to the `options()`. The
#'   list of modified options will be added as an attribute (`attr(out, "projectOptions")`),
#'   e.g., so they can be "unset" by user later.
#'
#'
#' @importFrom data.table data.table
setupOptions <- function(name, options, paths, times, overwrite = FALSE, envir = parent.frame(),
                         verbose = getOption("Require.verbose", 1L), dots, defaultDots,
                         useGit = getOption("SpaDES.project.useGit", FALSE),
                         updateRprofile = getOption("SpaDES.project.updateRprofile", TRUE),
                         ...) {

  makeUpdateRprofileSticky(updateRprofile)

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  newValuesComplete <- oldValuesComplete <- NULL
  updates <- NULL
  if (!missing(options)) {

    messageVerbose(yellow("setting up options..."), verbose = verbose, verboseLevel = 0)

    preOptions <- base::options() # need prefix or else greedy evaluation occurs on the `options()` as if it is the arg

    optionsSUB <- substitute(options) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    envirCur <- environment()
    options <- evalSUB(optionsSUB, valObjName = "options", envir = envirCur, envir2 = envir)
    # post check
    # if (isTRUE(try(any(grepl("^options$", eval(optionsSUB, envir = envir)[[1]])), silent = TRUE))) {
    #   warning("It looks like the options argument is passed options(...); please use list(...)")
    # }

    if (missing(paths)) {
      pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = paths$projectpath)`
      pathsSUB <- checkProjectPath(pathsSUB, name, envir = envirCur, envir2 = envir)
      paths <- setupPaths(paths = pathsSUB, defaultDots = defaultDots,
                          updateRprofile = updateRprofile,
                          useGit = useGit, verbose = verbose - 2)#, inProject = TRUE, standAlone = TRUE, libPaths,
    }

    options <- parseFileLists(options, paths, overwrite = isTRUE(overwrite),
                              envir = envir, verbose = verbose)

    postOptions <- base::options()
    newValues <- oldValues <- list()
    if (length(options)) {
      newValuesComplete <- options
      oldValuesComplete <- Map(nam = names(options), function(nam) NULL)
      oldValuesComplete[names(options)] <- preOptions[names(options)]
      whNULL <- which(lengths(oldValuesComplete) == 0)
      names(oldValuesComplete[unname(whNULL)]) <- names(options)[whNULL]
      newValues <- Require:::setdiffNamed(options, preOptions)
      oldValues <- base::options(newValues)
      if (length(newValues)) {
        messageVerbose("The following options have been changed", verbose = verbose)
        updates <- data.table::data.table(optionName = names(newValues), newValue = newValues,
                                          oldValue = oldValues)
        messageDF(updates, verbose = verbose)
      }
    }
    messageVerbose(yellow("  done setting up options"), verbose = verbose, verboseLevel = 0)
  }
  return(invisible(list(newOptions = newValuesComplete, oldOptions = oldValuesComplete, updates = updates)))
}

isUnevaluatedList <- function(p) any( {
  if (!(length(p) == 1 && is.name(p))) { # this is "just" an object name
    if (grepl("^if$|^<-$", p[[1]])[1]) {
      if (grepl("^\\{$", p[[3]][[1]])[1]) {
        grepl("^list$", p[[3]][[2]][[1]])
      } else {
        grepl("^list$", p[[3]][[1]])[1] || is.atomic(p[[3]])[1]
      }
    } else {
      grepl("^list$", p[[1]])[1]
    }
  } else {
    FALSE
  }
}
)

#' @importFrom tools file_ext
#' @importFrom utils modifyList tail
parseListsSequentially <- function(files, parsed, curly, namedList = TRUE, envir = parent.frame(),
                                   verbose = getOption("Require.verbose")) {
  envs <- list(envir) # means

  if (!missing(curly)) {
    obj2 <- deparse(curly)
    obj <- obj2[-c(1, length(obj2))]
    parsed <- parse(text = obj)
  }
  if (!missing(parsed)) {
    envs2 <- parseExpressionSequentially(parsed, envs, namedList, verbose)

    # check for functions -- functions aren't in lists, but they behave a bit like lists internally
    isFuns <- any(vapply(envs2, FUN.VALUE = logical(1), function(env) {
      any(vapply(mget(ls(env), envir = env), is.function, FUN.VALUE = logical(1)))
    }))

    if (isTRUE(namedList) && !isFuns) {
      os <- parseEnvsWithNamedListsSequentially(envs2)
    } else {
      os <- as.list(tail(envs2, 1)[[1]])
    }
  } else {
    llOuter <- lapply(files, function(optFiles) {
      os <- NULL
      if (isTRUE(tools::file_ext(optFiles) %in% c("txt", "R"))) {
        pp <- parse(optFiles)
        obj <- parseListsSequentially(parsed = pp, namedList = namedList, envir = envir,
                                      verbose = verbose)
      }})

    # basically, the optFiles may not be a namedList, but the objects in the file may be
    #   need reassess
    hasName <- lapply(llOuter, function(x) !is.null(names(x)))
    if (all(unlist(hasName))) namedList <- TRUE
    if (isTRUE(namedList))
      os <- Reduce(modifyList, llOuter)
    else
      os <- tail(llOuter, 1)[[1]][[1]]
  }

  os
}

evalListElems <- function(l, envir, verbose = getOption("Require.verbose", 1L)) {

  # need to deal with `if`. If we break it apart, then we fail to evaluate the if part
  # Also, every "normal" object e.g., mode <- "development"
  warns <- list()
  withCallingHandlers(l2 <- try(eval(l, envir), silent = TRUE),
                      warning = function(w) {
                        warns <<- w
                      })
  if (is(l2, "try-error")) {
    mess <- gsub("Error in eval\\(l, envir\\) : ", "", as.character(l2))
    mess <- gsub("\\n", "", as.character(mess))
    messageVerbose(mess, verbose = verbose)
    if (length(warns))
      messageVerbose("  (warning: ", warns$m, ")", verbose = verbose)

    isList <- FALSE
    isAssignedList <- FALSE
    if (length(l) == 3) {# can be list(...) or obj <- list(...)
      if (length(l[[3]]) > 1) {
        if (identical(l[[3]][[1]], quote(list))) {
          isList <- TRUE
          # if (identical(l[[2]][[1]], quote("<-")))
          if (!is(l, "if"))
            isAssignedList <- TRUE
          origL <- l
        }
      }
    } else {
      if (length(l) > 1)
        if (identical(l[[1]], quote(list))) isList <- TRUE
    }
    if (isList) {
      lList <- as.list(l)
      calls <- mapply(m = lList, function(m) inherits(m, "call")) # actual calls `times$start`
      names <- mapply(m = lList, function(m) inherits(m, "name")) # just objects `mode`
      namesNoList <- names[-1]
      if (any(namesNoList) && !isAssignedList) { # the -1 removes "list"
        whNames <- which(names)[-1]
        whNotNames <- which(!names)
        elems <- lList[whNames] # remove "list"
        out <- Map(l3 = elems, function(l3) {
          out <- evalListElems(l3, envir = envir, verbose = verbose)
        })
        lList[whNames] <- out
      }

      if (any(calls)) {
        whCalls <- which(calls)
        whNotCalls <- which(!calls)
        goInList <- length(whCalls) == 1

        if (isTRUE(goInList))
          elems <- lList[[whCalls]]
        else
          elems <- lList[whCalls]
        out <- mapply(l3 = elems, function(l3) {
          out <- evalListElems(l3, envir = envir, verbose = verbose)
        })

        if (isTRUE(goInList)) {
          lList <- out
        } else {
          lList[whCalls] <- out
        }

        l <- lList[-1] # remove "list"

      }
    }
    if (isAssignedList) {
      assign(as.character(parse(text = origL[[2]])), l, envir = envir)
    }

  } else {
    l <- l2
  }
  l
}

#' @export
#' @rdname setup
#' @details
#' `setupModules` will download all modules do not yet exist locally. The current
#' test for "exists locally" is simply whether the directory exists. If a user
#' wants to update the module, `overwrite = TRUE` must be set, or else the user can
#' remove the folder manually.
#'
#' @return
#' `setupModules` is run for its side effects, i.e., downloads modules and puts them
#' into the `paths[["modulePath"]]`. It will return a named list, where the names are the
#' full module names and the list elemen.ts are the R packages that the module
#' depends on (`reqsPkgs`)
#'
#' @param gitUserName The GitHub account name. Used with git clone git@github.com:*gitHuserName*/name
#' @importFrom tools file_ext
setupModules <- function(name, paths, modules, inProject, useGit = getOption("SpaDES.project.useGit", FALSE),
                         overwrite = FALSE, envir = parent.frame(), gitUserName,
                         verbose = getOption("Require.verbose", 1L), dots, defaultDots,
                         updateRprofile = getOption("SpaDES.project.updateRprofile", TRUE),
                         ...) {

  envirCur <- environment()
  makeUpdateRprofileSticky(updateRprofile)

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (missing(modules)) {
    modulesOrig <- character()
    packages <- list()
  } else {
    if (missing(paths)) {
      pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = paths$projectpath)`
      pathsSUB <- checkProjectPath(pathsSUB, name, envir = envirCur, envir2 = envir)
      paths <- setupPaths(paths = pathsSUB, updateRprofile = updateRprofile, useGit = useGit,
                          defaultDots = defaultDots)#, inProject = TRUE, standAlone = TRUE, libPaths,
    }
    if (missing(inProject))
      inProject <- isInProject(name)

    messageVerbose(yellow("setting up modules..."), verbose = verbose, verboseLevel = 0)

    modulesSUB <- substitute(modules) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    modules <- evalSUB(val = modulesSUB, valObjName = "modules", envir = envirCur, envir2 = envir)
    exts <- tools::file_ext(modules)
    isRepo <- nzchar(exts) & exts %in% "R"

    if (any(isRepo)) {
      messageVerbose("modules arg supplied as file(s); parsing ... ", verbose = verbose)
      modules <- parseFileLists(modules, paths, namedList = FALSE, overwrite = isTRUE(overwrite),
                                envir = envir, verbose = verbose)
    }

    anyfailed <- character()

    if(!is(modules, "character")) {
      stop("'modules' must be a character vector.")
    }

    modulesOrig <- modules
    modulesOrigPkgName <- extractPkgName(modulesOrig)
    modulesOrigNestedName <- extractModName(modulesOrig)

    if (useGit %in% FALSE) {
      offlineMode <- getOption("Require.offlineMode")
      if (isTRUE(offlineMode)) {
        opt <- base::options(Require.offlineMode = FALSE)
        on.exit(try(base::options(opt), silent = TRUE))
      }
      out <- getModule(modules, paths[["modulePath"]], overwrite = overwrite, verbose = verbose)
      if (isTRUE(offlineMode))
        base::options(opt)
      anyfailed <- out$failed
      modules <- anyfailed
    }

    isGH <- isGitHub(modules) & grepl("@", modules) # the default isGitHub allows no branch
    anyFailedGH <- intersect(anyfailed, modules[isGH])

    usingGit <- checkUseGit(useGit)

    if (usingGit || ( length(anyFailedGH) ) ) {

      isLocalGitRepoAlready <- isProjectGitRepo(paths[["projectPath"]], inProject)
      origDir <- getwd()
      on.exit({
        setwd(origDir)
      }
      )

      # This will create a new Git Repo at the top level
      if (isLocalGitRepoAlready %in% FALSE && is.character(useGit)) {
        dir1 <- dir(".", all.files = TRUE)
        onlyFiles <- dir1[!dir.exists(dir1)]
        if (length(onlyFiles) == 0) {
          file.create("README.md")
          dir1 <- dir(".", all.files = TRUE)
          onlyFiles <- dir1[!dir.exists(dir1)]
        }
        theFiles <- paste(onlyFiles, collapse = " ")
        system(paste("git add ", theFiles))

        ignoreAFolder(gitIgnoreFile = ".gitIgnore", paths$cachePath, paths[["projectPath"]])
        ignoreAFolder(gitIgnoreFile = ".gitIgnore", paths$inputPath, paths[["projectPath"]])

        system(paste("git commit -a -m \"first commit\""))
        rl <- readline("Update git config --global --edit ? (Y or N): ")
        if (startsWith(tolower(rl), "y") ) {
          system(paste0("git config --global --edit "))

          rl <- readline("Need to amend this commit to use this new user ? (Y or N): ")
          if (startsWith(tolower(rl), "y") ) {
            system(paste0("git commit --amend --reset-author"))
          }
        }
        system("git branch -M main")
        # rl <- readline("Type remote ssh url after git@github.com:")
        addOrigin <- paste0('git remote add origin git@github.com:', gitUserName,'/', name ,'.git')
        system(addOrigin)
        # system(paste0("git remote add origin git@github.com:", rl))
        system("git push -u origin main")
        isLocalGitRepoAlready <- TRUE

      }

      # lala <- unlist(strsplit(
      #   split = " {4,}",
      #   c(
      #     #     'git init -b main
      #     # git add .
      #     # git commit -m "first commit"
      #     'git branch -M main',
      #     paste0('git remote add origin git@github.com:',gitUserName,'/', name ,'.git'),
      #     'git push --set-upstream origin main
      #   git push -u origin main
      #   ')))
      #
      # lala <- gsub("\n", "", lala)
      # lala2 <- lapply(lala, system, intern = TRUE)

      gitSplit <- splitGitRepo(modules)
      gitSplit <-try(Require::invertList(gitSplit), silent = TRUE)
      if (is(gitSplit, "try-error"))
        stop("Did you specify the modules correctly? Is this correct:\n",
             paste(modules, collapse = "\n"))

      mapply(split = gitSplit, function(split) {
        modPath <- file.path(split$acct, split$repo)
        localPath <- file.path(paths[["modulePath"]], split$repo)
        if (!dir.exists(localPath)) {

          prev <- setwd(file.path(paths[["modulePath"]]))
          cloneOrSubmodule <- if (isTRUE(isLocalGitRepoAlready)) {
            gert::git_submodule_add
            # "submodule add"
          } else {
            gert::git_clone
            # "clone"
          }

          # cmd <- paste0("git ", cloneOrSubmodule," https://github.com/", modPath)
          # setwd(paths[["modulePath"]])
          # gert::git_submodule_add("https://github.com/cboisvenue/spadesCBM", path = "modules/spadesCBM")
          out <- cloneOrSubmodule(paste0("https://github.com/", modPath),
                                  path = file.path(basename(paths[["modulePath"]]), basename(modPath)))

          # for (i in 1:2) {
          #   system(cmd)
          #   if (dir.exists(file.path(paths[["modulePath"]], split$repo)))
          #     break
          #   if (getOption("SpaDES.project.forceGit", TRUE)) {
          #     messageVerbose("It looks like the submodule was deleted; restoring it.\n",
          #             "Set options('SpaDES.project.forceGit' = FALSE) to prevent this", verbose = verbose)
          #     verbose <<- max(0, verbose - 1)
          #     cmd2 <- strsplit(cmd, cloneOrSubmodule)[[1]]
          #     cmd <- paste0(cmd2[1], cloneOrSubmodule, " --force", tail(cmd2, 1))
          #   }
          # }

        } else {
          messageVerbose("module exists at ", localPath, "; not cloning", verbose = verbose)
        }
        reportBranch <- TRUE
        # if (!grepl("master|main|HEAD", split$br)) {
        prev <- setwd(file.path(paths[["modulePath"]], split$repo))
        curBr <- gert::git_branch()

        # cmd <- "git rev-parse --abbrev-ref HEAD"
        # curBr <- system(cmd, intern = TRUE)
        if (!identical(split$br, curBr)) {
          prev <- setwd(file.path(paths[["modulePath"]], split$repo))
          gert::git_branch_checkout(split$br)
          # cmd <- paste0("git checkout ", split$br)
          # system(cmd)
          reportBranch <- FALSE
        }
        gert::git_pull()
        # }
        # cmd <- paste0("git pull")
        # system(cmd)

        if (reportBranch)
          messageVerbose("\b ... on ", split$br, " branch")
      })
      messageVerbose("You will likely have to commit changes to git repository now", verbose = verbose)
    }
    if (is.character(useGit) && isLocalGitRepoAlready %in% FALSE) {

      res1 <- system(paste("git remote add", name, useGit), intern = TRUE)
      res2 <- system(paste("git push", name), intern = TRUE)
      if (grepl("error|fatal", res1))
        stop("git push failed; perhaps no rights to push; perhaps open a Git GUI to complete this step")

    }

    # Need full path
    m <- fileRelPathFromFullGHpath(modulesOrig)
    # m <- gsub("@[[:alnum:]_-]+$", "", modulesOrig)
    # m <- gsub("@[[:alnum:]_]+/", "/", m)
    # m <- lapply(strsplit(m, "/"), function(r) r[-c(1, length(r))])
    # m <- vapply(m, paste, collapse = "/", FUN.VALUE = character(1))

    modulePackages <- Map(mo = modulesOrigNestedName, di = m, function(di, mo)
      modulePackages <-
        unlist(packagesInModules(modulePath = file.path(paths[["modulePath"]], di),
                                 modules = mo), use.names = FALSE))
    # modulePackages <- packagesInModules(modulePath = file.path(paths[["modulePath"]], dirname(m)),
    #                                     modules = modulesOrigNestedName)
    packages <- modulePackages[modulesOrigNestedName]

    ## check that we keep only the modules needed
    actualModPaths <- normPath(file.path(paths$modulePath, m, modulesOrigNestedName))
    wantedModPath <- normPath(file.path(paths$modulePath, modulesOrigNestedName))

    isNested <- which(!actualModPaths %in% wantedModPath)

    if (length(isNested)) {
      ## subset to nest modules
      actualModPaths2 <- actualModPaths[isNested]
      wantedModPath2 <- wantedModPath[isNested]
      modulesOrigNestedName2 <- modulesOrigNestedName[isNested]
      modulesOrigPkgName2 <- modulesOrigPkgName[isNested]

      moduleSuperFolder <- unique(normPath(file.path(paths$modulePath, modulesOrigPkgName2)))

      ## modules were probably nested in a GH repo
      actualModFiles <- sapply(actualModPaths2, list.files, recursive = TRUE, all.files = TRUE, full.names = TRUE, USE.NAMES = FALSE) |>
        unlist()
      newModFiles <- actualModFiles
      for (ddir in dirname(actualModPaths2)) {
        newModFiles <- sub(ddir, paths$modulePath, newModFiles)
      }

      invisible(sapply(unique(dirname(newModFiles)), dir.create, recursive = TRUE, showWarnings = FALSE))
      out <- suppressWarnings(file.copy(actualModFiles, newModFiles, recursive = TRUE, overwrite = overwrite))

      if (all(file.exists(newModFiles)) & all(dir.exists(wantedModPath2))) {
        unlink(moduleSuperFolder, recursive = TRUE)
      } else {
        warnings("Could not copy module files to 'modulePath', leaving in original, potentially nested directory")
        unlink(dirname(newModFiles), recursive = TRUE)
      }
    }

    messageVerbose(yellow("  done setting up modules"), verbose = verbose, verboseLevel = 0)
  }
  names(packages) <- modulesOrig
  return(packages)

}


#' @export
#' @rdname setup
#' @details
#' `setupPackages` will read the modules' metadata `reqdPkgs` element. It will combine
#' these with any packages passed manually by the user to `packages`, and pass all
#' these packages to `Require::Install(...)`.
#' @param modulePackages A named list, where names are the module names, and the elements
#'   of the list are packages in a form that `Require::Require` accepts.
#'
#' @return
#' `setupPackages` is run for its side effects, i.e., installing packages to
#' `paths[["packagePath"]]`.
#'
setupPackages <- function(packages, modulePackages = list(), require = list(), paths, libPaths,
                          setLinuxBinaryRepo = TRUE,
                          standAlone, envir = parent.frame(), verbose = getOption("Require.verbose"),
                          dots, defaultDots, ...) {

  envirCur <- environment()
  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (isTRUE(setLinuxBinaryRepo))
    Require::setLinuxBinaryRepo()

  if (missing(packages))
    packages <- character()

  if (
    (length(packages) || length(unlist(modulePackages)) || length(require)) &&
    !is.null(packages)
  ){

    messageVerbose(yellow("setting up packages..."), verbose = verbose, verboseLevel = 0)
    messageVerbose("Installing any missing reqdPkgs", verbose = verbose)
    continue <- 1L
    while (continue) {
      if (verbose > 1) {
        outP <- capture.output(modulePackages)
        messageVerbose("reqdPkgs by module:")
        messageVerbose(paste(outP, collapse = "\n"), verbose = verbose)
      }
      mp <- unlist(unname(modulePackages))
      if (is(mp, "list")) { # means there was a call in modulePackages i.e., an unquoted thing like PredictiveEcology/Require
        mp <- substitutePackages(mp, envir = envirCur)
      }
      if (!any(grepl("SpaDES.core", extractPkgName(mp))))
        mp <- c(mp, "SpaDES.core")
      # requireToTry <- unique(c(mp, require))
      packagesToTry <- c(packages, mp, require)
      packagesToTry <- packagesToTry[!duplicated(packagesToTry)]
      areFilesWithPackages <- endsWith(tolower(packagesToTry), ".r") # & grepl("@", packagesToTry) # the default isGitHub allows no branch
      if (any(areFilesWithPackages)) {
        remoteFiles <- areFilesWithPackages & grepl("@", packagesToTry) # the default isGitHub allows no branch
        aa <- parseFileLists(trimVersionNumber(packagesToTry[remoteFiles]), paths = paths,
                             envir = envir, namedList = FALSE)
        packagesToTry <- c(packagesToTry[-remoteFiles], unname(unlist(aa)))
      }

      requirePkgNames <- Require::extractPkgName(require)

      out <- #try({
        Require::Require(packagesToTry, require = requirePkgNames, # require = Require::extractPkgName(requireToTry),
                         standAlone = standAlone,
                         libPaths = libPaths,
                         verbose = verbose)
      #})
      # }


      if (is(out, "try-error")) {
        deets <- gsub(".+Can't find ([[:alnum:]]+) on GitHub repo (.+); .+", paste0("\\2@\\1"), as.character(out))
        miss <- unlist(Map(mp = modulePackages, function(mp) grep(value = TRUE, pattern = deets, mp)))
        if (length(miss)) {
          modulePackages[[names(miss)]] <- setdiff(modulePackages[[names(miss)]], miss)
          warning("Module ", names(miss), " has reqdPkgs ", paste0(miss, collapse = ", "),
                  ", but branch don't exist; \nplease update module. ",
                  "Omitting that package from this Require call which may mean ",
                  Require::extractPkgName(unname(miss)), " doesn't get installed")
        }
        continue <- continue - 1L
      } else {
        continue <- 0L
      }
    }
    messageVerbose(yellow("  done setting up packages"), verbose = verbose, verboseLevel = 0)
  } else {
    messageVerbose(yellow("no packages to set up"), verbose = verbose, verboseLevel = 0)
  }

  if (is.null(packages)) {
    if (!is.null(require))
      lapply(Require::extractPkgName(require), base::require, character.only = TRUE)
  }
  messageVerbose(".libPaths() are: ", paste(.libPaths(), collapse = ", "), verbose = verbose)

  invisible(NULL)
}

#' @export
#' @rdname setup
#'
#' @return
#' `setupParams` prepares a named list of named lists, suitable to be passed to
#' the `params` argument of `simInit`.
#'
#'
#' @importFrom data.table data.table
setupParams <- function(name, params, paths, modules, times, options, overwrite = FALSE, envir = parent.frame(),
                        verbose = getOption("Require.verbose", 1L), dots, defaultDots,
                        ...) {

  envirCur <- environment()
  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (missing(params)) {
    params <- list()
  } else {

    messageVerbose(yellow("setting up params..."), verbose = verbose, verboseLevel = 0)

    paramsSUB <- substitute(params) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    params <- evalSUB(val = paramsSUB, valObjName = "params", envir = envirCur, envir2 = envir)
    params <- parseFileLists(params, paths, overwrite = isTRUE(overwrite),
                             envir = envir, verbose = verbose)

    if (length(params)) {

      # If the path is nested within a repository, the module will already be stripped of the @
      modulesSimple <- simplifyModuleName(modules)
      # modulesSimple1 <- Require::extractPkgName(modules)
      # modulesSimple2 <- Require::extractPkgName(unname(modules))
      # take1st <- grepl("@", modulesSimple2)
      # modulesSimple <- ifelse(take1st, modulesSimple1, modulesSimple2)

      paramsForModules <- intersect(modulesSimple, names(params))
      overSupplied <- setdiff(names(params), c(".globals", paramsForModules))

      # This will shrink down to only the modulesSimple -- all others are gone
      hasDotGlobals <- isTRUE(".globals" %in% names(params))
      globs <- if (hasDotGlobals) params[[".globals"]] else NULL

      if (length(overSupplied)) {
        params <- params[paramsForModules]

        messageVerbose(blue("Only returning params that are relevant for modules supplied.\n",
                            "Omitting parameters supplied for: ",
                            paste(overSupplied, collapse = ", "), "\n --- (set verbose == 2 to see details) --- "),
                       verbose = verbose)
      }


      mods <- setdiff(names(params), ".globals")
      # if (FALSE) {
      #   messageVerbose(blue("Using SpaDES.core to compare metadata with supplied parameters: "),
      #                  verbose = verbose)
      #   params <- Map(mod = mods, function(mod) {
      #     knownPars <- SpaDES.core::moduleMetadata(module = mod, path = paths[["modulePath"]],
      #                                              defineModuleListItems = "parameters")$parameters$paramName
      #     if (!is.null(params[[mod]])) {
      #       supplied <- names(params[[mod]])
      #       anyGlobals <- intersect(supplied, names(params[[".globals"]]))
      #       paramsInclGlobalsSupplied <- unique(c(supplied, anyGlobals))
      #       knownParsInMod <- paramsInclGlobalsSupplied %in% knownPars
      #       overInclusion <- knownParsInMod %in% FALSE
      #       if (any(overInclusion)) {
      #         params[[mod]] <- params[[mod]][paramsInclGlobalsSupplied[!overInclusion]]
      #         messageVerbose(blue("These parameters set (",
      #                             paste(paramsInclGlobalsSupplied[knownParsInMod %in% FALSE], collapse = ", "),
      #                             "), but they are not in ", mod,
      #                             " metadata; this should be either added to the metadata, ",
      #                             "or not set in the params argument"), verbose = verbose)
      #       }
      #
      #     } else {
      #       messageVerbose(blue("No parameters set for ", mod), verbose = verbose, verboseLevel = 2)
      #     }
      #     params[[mod]]
      #   })
      # } else {
      #   messageVerbose("Skipping checking of parameters supplied against module parameters because ",
      #                  "it is a slow step that will be performed during `SpaDES.core::simInit`.",
      #                  verbose = verbose)
      # }
      if (hasDotGlobals)
        params[[".globals"]] <- globs
      messageVerbose(blue("The following params were created: "), verbose = verbose, verboseLevel = 2)
      oo <- capture.output(params)
      messageVerbose(blue(paste(oo, collapse = "\n")), verboseLevel = 2, verbose = verbose)
      messageVerbose(yellow("  done setting up params"), verbose = verbose, verboseLevel = 0)
    }
  }
  params <- Require::modifyList2(list(.globals = list(.studyAreaName = basename(paths[["projectPath"]]))),
                        params)
  return(params)
}


parseFileLists <- function(obj, paths, namedList = TRUE, overwrite = FALSE, envir,
                           verbose = getOption("Require.verbose", 1L), dots, ...) {
  if (is(obj, "list")) {
    nams <- names(obj)
    if (is.null(nams)) {
      named <- rep(FALSE, length(obj))
    } else {
      named <- nzchar(nams)
    }
    notNamed <- which(!named)
    if (length(notNamed)) {
      if (any(named))
        namedElements <- obj[which(named)]
      obj[notNamed] <- Map(objInner = obj[notNamed],
                           function(objInner)
                             parseFileLists(objInner, paths, namedList, overwrite,
                                            envir, verbose, dots, ...))
      if (any(named))
        obj[named] <- Map(x = obj[named], nam = names(namedElements), function(x, nam) {
          y <- list(x)
          names(y) <- nam
          y})
      obj <- Reduce(f = modifyList, obj)
    }
  }

  if (is.character(obj)) {
    obj <- mapply(opt = obj, function(opt) {
      isGH <- isGitHub(opt) && grepl("@", opt) # the default isGitHub allows no branch
      if (isGH) {
        rem <- opt
        gitRepo <- splitGitRepo(opt)
        opt <- stripQuestionMark(opt)
        relativeFilePath <- extractGitHubFileRelativePath(opt)
        # if (startsWith(relativeFilePath, basename(paths[["projectPath"]]))) {
        #   # This is a projectPath that is one level into a GitHub repository
        #   opt <- file.path(dirname(paths[["projectPath"]]), relativeFilePath)
        # } else {
        #   opt <- file.path(paths[["projectPath"]], relativeFilePath)
        # }
        opt <- stripDuplicateFolder(relativeFilePath, paths[["projectPath"]])

        fe <- file.exists(opt)
        if (isTRUE(fe && isFALSE(overwrite))) {
          messageVerbose(opt, " already exists; not downloading", verbose = verbose)
        } else {
          if (isTRUE(fe)) {
            messageVerbose(opt, " already exists; overwrite = TRUE; downloading again", verbose = verbose)
            unlink(opt)
          }
          # opt is the correct destination file because it has removed potential duplicated folder names
          #   but getGitHubfile won't know this ... so give it a temporary destdir
          destdir <- Require::tempdir2()
          temp <- getGithubFile(rem, destDir = destdir, overwrite = isTRUE(overwrite))
          checkPath(dirname(opt), create = TRUE)
          same <- temp == opt
          notSame <- same %in% FALSE
          optOrig <- opt
          if (any(same)) {
            temp <- temp[notSame]
            opt <- opt[notSame]
          }

          if (length(temp)) {
            copied <- linkOrCopy(temp, opt)
          }
          opt <- optOrig

        }

      } else {
        isURL <- grepl("^http", opt)
        if (isURL) {
          destfile <- file.path(paths[["inputPath"]], basename(opt))
          if (requireNamespace("reproducible", quietly = TRUE)) {
            ret <- reproducible::preProcess(url = opt, targetFile = destfile, overwrite = isTRUE(overwrite), fun = NA)
          } else {
            dlfileOut <- try(download.file(url = opt, destfile = destfile))
            if (is(dlfileOut, "try-error"))
              stop("File could not be downloaded; perhaps try again after install.packages('reproducible')")
          }
          opt <- destfile
        }
        if (!file.exists(opt))
          messageVerbose(opt, paste(" has no @ specified, so assuming a local file,",
                                    "but local file does not exist"), verbose = verbose)
      }
      opt
    }, SIMPLIFY = TRUE)
    if (verbose > 0) {
      objLocal <- gsub(paths[["projectPath"]], "", obj)
      mess <- unlist(Map(nam = obj, rem = names(obj), function(rem, nam) {
        objRem <- strsplit(rem, split = "/")[[1]]
        len <- length(objRem)
        if (len > 5) {
          paste0(paste(objRem[1:4], collapse = "/"), "/.../", paste(objRem[c(len - 1):len], collapse = "/"))
        } else {
          rem
        }
      }))
      if (!identical(mess, objLocal)) {
        if (file.exists(objLocal))
          messageDF(data.frame(url = mess, "is" = "--->", localFile = objLocal))
      }
    }
    areAbs <- isAbsolutePath(obj)
    if (any(areAbs %in% FALSE)) {
      if (!startsWith(fs::path_norm(obj[areAbs %in% FALSE]), fs::path_norm(paths[["projectPath"]]))) {
        obj[areAbs %in% FALSE] <- file.path(paths[["projectPath"]], obj[areAbs %in% FALSE])
      }
    }

  }
  if (is.character(obj)) {
    obj <- parseListsSequentially(files = obj, namedList = namedList, envir = envir,
                                  verbose = verbose)
  } else if (is(obj, "{")) {
    obj <- parseListsSequentially(curly = obj, namedList = namedList, envir = envir,
                                  verbose = verbose)
  }

  return(obj)
}

checkProjectPath <- function(paths, name, envir, envir2) {

  defaults <- spadesProjectOptions()
  if (missing(paths)) {
    paths <- list()
  }
  if (is.name(paths) || is.call(paths)) {
    paths <- evalSUB(paths, valObjName = "paths", envir = envir, envir2 = envir2)
  }
  if (is.null(paths[["projectPath"]])) {
    prjPth <- if (missing(name)) {
      defaults$spades.projectPath
    } else {
      if (isInProject(name)) {
        normPath(".")
      } else {
        checkPath(name, create = TRUE)
      }

    }
    prjPth <- list(projectPath = prjPth)

    paths <- append(prjPth, as.list(paths))
    paths <- paths[nzchar(names(paths))]
  }

  if (!is.null(paths[["projectPath"]])) {
    paths[["projectPath"]] <- evalSUB(paths[["projectPath"]], valObjName = "paths", envir, envir2)
    #  name <- basename(normPath(paths[["projectPath"]]))
  } else {
    stop("Must provide either a name or a paths[[\"projectPath\"]]")
  }

  paths
}

isInProject <- function(name) {
  if (!missing(name)) {
    gtwd <- getwd()
    gtwdExp <- basename(fs::path_expand_r(gtwd))
    nameExp <- basename(fs::path_expand_r(extractPkgName(name)))
    out <- identical(gtwdExp, nameExp)
    if (out %in% FALSE) {
      gtwdExp <- basename(fs::path_expand(gtwd))
      nameExp <- basename(fs::path_expand(extractPkgName(name)))
      out <- identical(gtwdExp, nameExp)
    }
  } else {
    out <- TRUE
  }
  out
}

isInRstudioProj <- function(name) {
  tryCatch(identical(name, basename(findProjectPath())),
           silent = TRUE, error = function(x) FALSE)
}

inTempProject <- function(paths) {
  grepl(normPath(tempdir()), normPath(paths[["projectPath"]]))
}


evalSUB <- function(val, valObjName, envir, envir2) {
  valOrig <- val
  val2 <- val
  userQuoted <- tryCatch(grepl("quote", val), silent = TRUE, error = function(e) FALSE)
  warns <- character()
  withCallingHandlers({
  while (inherits(val, "call") || inherits(val, "name") || inherits(val, "{") || inherits(val, "if")) {
    if (identical(valObjName, "options")) {
      optionsGrepStart <- "^options\\>"
      if (any(grepl(optionsGrepStart, val))) {
        val[[1]] <- as.name(gsub(optionsGrepStart, "list", val[[1]]))
      }
    }
    if (inherits(val, "name"))
      val2 <- get0(val, envir = envir)
    else {
      val2 <- try(eval(val, envir = envir), silent = TRUE)
    }

    if ((identical(val2, val) && !missing(envir2)) || is.null(val2) ||
        is(val2, "try-error")) {
      val3 <- try(eval(val, envir = envir2), silent = TRUE)
      if (is(val3, "try-error")) {
        # last ditch effort -- brute force
        sfs <- sys.frames()
        for (frm in rev(sfs)) {
          # env <- new.env(parent = frm)
          val3 <- try(eval(val, envir = frm), silent = TRUE)
          if (!is(val3, "try-error"))
            break
        }
      }
      val <- val3
      val2 <- val3
    } else {
      val <- val2
    }
    if (missing(envir2))
      break
  }
  },
  warning = function(w) {
    warns <<- c(warns, w$message)
    invokeRestart("muffleWarning")
  })
  if (length(warns)) {
    warns <- rev(unique(warns))
    # try to give use more help in debugging
    spaces <- unlist(Map(space = rep("  ", length(warns)), num = seq(length(warns)),
               function(space, num) paste(collapse = "", rep(space, num))))
    warns <- paste0(spaces, warns)
    warns <- errorMsgCleaning(warns, valOrig)
    if (any(userQuoted))
      message(warns)
    else
      warning(warns, call. = FALSE)
    # val2 <- valOrig
  }
  # if (is(val2, "try-error")) {
  #   if (any(userQuoted))
  #     message(rev(val2))
  #   else
  #     warning(rev(val2))
  # }
  if (is(val2, "list") && !is.null(names(val2))) {
    env <- environment()
    namesToEval <- names(val2)
    if (any(userQuoted[-1])) {
      namesToEval <- namesToEval[userQuoted[-1] %in% FALSE]
    }
    # Sequential evaluation
    if (length(namesToEval)) {
      Map(nam = namesToEval, function(nam) {
        val2[[nam]] <<- evalSUB(val2[[nam]], valObjName = valObjName,
                                envir = env, envir2 = envir)
        assign(valObjName, val2, envir = env)
      }
      )
      assign("val2", get(valObjName), envir = env)
    }
  }

  if (is(val2, "try-error")) {
    val2 <- errorMsgCleaning(val2, valOrig)
    warning(val2, call. = FALSE)
  } else {
    if (exists("val3", inherits = FALSE))
      if (is(val3, "try-error")) {
        val3 <- errorMsgCleaning(val3, valOrig)
        warning(val3)
      }
  }

  val2
}


#' @export
#' @rdname setup
#' @param gitignore Logical. Only has an effect if the `paths$projectPath`
#'   is a git repositories without submodules. This case is ambiguous what a user
#'   wants. If `TRUE`, the default, then `paths$modulePath` will be added to
#'   the `.gitignore` file. Can be controled with `options(SpadES.project.gitignore = ...)`.
#' @details
#' `setupGitIgnore` will add.
#'
#' @return
#' `setupGitIgnore` is run for its side effects, i.e., adding either `paths$packagePath`
#' and/or `paths$modulePath` to the
#' `.gitignore` file. It will check whether `packagePath` is located inside the
#' `paths$projectPath` and will add this folder to the `.gitignore` if `TRUE`.
#' If the project is a git repository with git submodules, then it will add nothing else.
#' If the project is a git repository without git submodules, then the `paths$modulePath`
#' will be added to the `.gitignore` file. It is assumed that these modules are
#' used in a `read only` manner.
setupGitIgnore <- function(paths, gitignore = getOption("SpaDES.project.gitignore", TRUE),
                           verbose) {

  if (isTRUE(gitignore)) {
    gitIgnoreFile <- ".gitignore"
    gitFile <- file.path(paths[["projectPath"]], ".git")
    if (dir.exists(gitFile)) { # this is a git repository
      if (file.exists(gitIgnoreFile))
        gif <- readLines(gitIgnoreFile, warn = FALSE)
      else
        gif <- character()
      gifOrig <- gif

      prjP <- normPath(paths[["projectPath"]])
      pkgP <- normPath(paths[["packagePath"]])
      updatedPP <- updatedMP <- FALSE

      # if the R package folder is inside
      isPackagePathInside <- grepl(prjP, pkgP)
      if (isTRUE(isPackagePathInside)) {
        updatedPP <- TRUE
        pkgP <- gsub(prjP, "", pkgP)
        if (startsWith(pkgP, "/"))
          pkgP <- gsub("^/", "", pkgP)
        lineWithPkgPath <- grep(paste0("^", pkgP,"$"), gif)
        insertLine <- if (length(lineWithPkgPath)) lineWithPkgPath[1] else length(gif) + 1
        gif[insertLine] <- file.path(pkgP, "*")
      }

      if (!file.exists(".gitmodules")) { # This is NOT using submodules; so, "it is a git repo, used git
        updatedMP <- TRUE
        lineWithModPath <- grep(paste0("^", basename(paths[["modulePath"]]),"$"), gif)
        insertLine <- if (length(lineWithModPath)) lineWithModPath[1] else length(gif) + 1
        gif[insertLine] <- file.path(basename(paths[["modulePath"]]), "*")

      }

      # ignore these folders/files
      igs <- c("cachePath", "inputPath", "outputPath", ".Rproj.user", ".Rhistory",
               ".Rdata", ".RData", ".secret", ".secrets")
      # rproj <- paste0(basename(prjP), ".Rproj")
      # igs <- c(igs, rproj)

      for (ig in igs) {
        igRel <- if (!is.null(paths[[ig]])) {
          fs::path_rel(paths[[ig]], prjP)
        } else {
          ig
        }

        if (!isAbsolutePath(igRel)) { # if igRel is inside projectPath, then add to .gitignore
          if (!any(grepl(igRel, gif))) { # only add if not already there
            insertLine <- length(gif)
            gif[insertLine + 1] <- igRel
          }
        }
      }

      # Write the file
      if (length(setdiff(gif, gifOrig))) {
        writeLines(con = gitIgnoreFile, unique(gif))
        mess <- paste(c("packagePath"[updatedPP], "modulePath"[updatedMP]), collapse = " and ")
        messageVerbose(verboseLevel = 1, verbose = verbose,
                       ".gitignore file updated with ", mess,"; ",
                       "this may need to be confirmed manually")
      }
    }
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
  defaultSPO <- spadesProjectOptions()
  if (missing(cachePath)) {
    cachePath <- .getOption("reproducible.cachePath",
                            defaultSPO$reproducible.cachePath) # nolint
    defaults$CP <- TRUE
  }
  if (missing(inputPath)) {
    inputPath <- .getOption("spades.inputPath",
                            defaultSPO$spades.inputPath) # nolint
    defaults$IP <- TRUE
  }
  if (missing(modulePath)) {
    modulePath <- .getOption("spades.modulePath",
                             defaultSPO$spades.modulePath) # nolint
    defaults$MP <- TRUE
  }
  if (missing(outputPath)) {
    outputPath <- .getOption("spades.outputPath",
                             defaultSPO$spades.outputPath) # nolint
    defaults$OP <- TRUE
  }
  if (missing(scratchPath)) {
    scratchPath <- .getOption("spades.scratchPath", defaultSPO$spades.scratchPath)
    defaults$SP <- TRUE
  }
  if (missing(rasterPath)) { ## TODO: deprecate
    rasterPath <- normPath(file.path(.getOption("spades.scratchPath",
                             defaultSPO$spades.scratchPath), "raster")) # nolint
    defaults$RP <- TRUE
  }
  if (missing(terraPath)) {
    terraPath <- normPath(file.path(.getOption("spades.scratchPath",
                                  defaultSPO$spades.scratchPath), "terra"))
    # terraPath <- file.path(getOption("spades.scratchPath"), "terra") # nolint
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
  base::options(
    rasterTmpDir = newPaths$rasterPath,
    reproducible.cachePath = cachePath,
    spades.inputPath = inputPath,
    spades.modulePath = unlist(modulePath),
    spades.outputPath = outputPath,
    spades.scratchPath = scratchPath
  )

  # if (requireNamespace("terra", quietly = TRUE)) {
  #   terra::terraOptions(tempdir = terraPath)
  # }

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
    inputPath = .getOption("spades.inputPath"), # nolint
    modulePath = .getOption("spades.modulePath"), # nolint
    outputPath = .getOption("spades.outputPath"), # nolint
    rasterPath = file.path(.getOption("spades.scratchPath"), "raster"), # nolint
    scratchPath = .getOption("spades.scratchPath"), # nolint
    terraPath = file.path(.getOption("spades.scratchPath"), "terra") # nolint
  )
}

.getOption <- function(x, default = NULL) {
  optionDefault <- base::options(x)[[1]]
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
# packagePath <- "Rpackages"
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
  msg <- paste0(...)

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


dotsToHere <- function(dots, dotsSUB, defaultDots, envir = parent.frame()) {
  if (missing(dots))
    dots <- dotsSUB
  else
    dots <- append(dots, dotsSUB)
  haveDefaults <- !missing(defaultDots)
  if (haveDefaults) {
    newInEnv <- setdiff(names(defaultDots), ls(envir = envir))
    list2env(defaultDots[newInEnv], envir = envir)
    on.exit(rm(list = newInEnv, envir = envir))
  }
  localEnv <- new.env(parent = envir)

  dots <- Map(d = dots, nam = names(dots), # MoreArgs = list(defaultDots = defaultDots),
              function(d, nam) {
                d1 <- evalSUB(d, valObjName = nam, envir = localEnv, envir2 = localEnv)
                # d1 <- try(eval(d, envir = localEnv), silent = TRUE)
                if (is(d1, "try-error")) {
                  if (isTRUE(haveDefaults))
                    d1 <- defaultDots[[nam]]
                  else
                    d1 <- d
                }
                assign(nam, d1, envir = localEnv) # sequential
                d1
              })
  list2env(dots, envir = envir)
  dots
}


setupRequire <- function(allPkgs, packages, ...) {

  # Without installing
  requireNeeds <- unique(unname(unlist(Require::pkgDep(allPkgs, recursive = TRUE))))
  packagesNeeds <- unique(unname(unlist(Require::pkgDep(packages, recursive = TRUE))))
  requirePkgs <- unique(extractPkgName(requireNeeds))
  packagesPkgs <- extractPkgName(packagesNeeds)
  packagesPkgsMatched <- packagesNeeds[match(requirePkgs, packagesPkgs)]
  require <- sort(unique(c(requireNeeds, packagesPkgsMatched)))

  # alreadyLoadedMess <- c()
  withCallingHandlers(
    Require::Require(allPkgs, ...) # basically don't change anything
    , message = function(m) {
      if (any(grepl("Error: package or namespace", m$message))) {
        pkg <- gsub("^.+namespace \u2018(.+)\u2019 .+ is already loaded.+$", "\\1", m$message)
        message(m)
        stop(stopMessForRequireFail(pkg))
      }
    }
    , warning = function(w) {
      warnMess <- "^.+ersion .+ of \u2018(.+)\u2019 masked by .+$"
      if (any(grepl(warnMess, w$message))) {
        pkg <- gsub(warnMess, "\\1", w$message)
        warning(w)
        stop(stopMessForRequireFail(pkg))
      }
    }
  )
}

stopMessForRequireFail <- function(pkg) {
  paste0("\nThe above error(s) likely mean(s) you must restart R and run again.",
         "\nIf this/these occur(s) again, your session likely ",
         "pre-loads old packages from e.g., your personal library. ",
         "The best thing to do is try to\n",
         yellow("restart R without loading any packages."),
         "\n\nIf that is not easy to do, you can try to update it in that location with (for a CRAN package) e.g., :\n",
         yellow("restart R "),
         blue(paste0("\ninstall.packages(c('", pkg, "'))")),
         yellow("\nrestart R"),
         "\n\nIf that does not work (including non-CRAN packages), perhaps removing the old one...",
         yellow("\nrestart R "),
         blue(paste0("\nremove.packages(c('", pkg, "'))")),
         yellow("\nrestart R"),
         "\nThis should trigger a re-installation, or allow ",
         "for a manual install.packages ...")
}

#' @rdname setup
#' @export
#' @inheritParams setupProject
#' @importFrom rstudioapi getActiveProject getSourceEditorContext
#'
#' @details
#' `setupStudyArea` only uses `inputPath` within its `paths` argument, which will
#' be passed to `path` argument of `gadm`. User can pass any named list element
#' that matches the columns in the `sf` object, including e.g., `NAME_1` and, if `level = 2`,
#' is specified, then `NAME_2`.
#'
#' ```
#' setupStudyArea(list(NAME_1 = "Alberta", "NAME_2" = "Division No. 17", level = 2))
#' ```
#'
#' @return
#' `setupStudyArea` will return an `sf` class object coming from `geodata::gadm`,
#' with subregion specification as described in the `studyArea` argument.fsu
setupStudyArea <- function(studyArea, paths, envir, verbose = getOption("Require.verbose", 1L)) {

  if (missing(paths))
    paths <- list(inputPaths = ".")
  studyArea <- evalSUB(studyArea, valObjName = "studyArea", envir = parent.frame(), envir2 = envir)

  if (is(studyArea, "list")) {
    theCall <- quote(getStudyArea(studyArea, paths, verbose = verbose))
    if (requireNamespace("reproducible", quietly = TRUE))
      # Cache doesn't evaluate the `theCall` inside eval, so need .cacheExtra to identify the actual contents
      studyArea <- reproducible::Cache(eval(theCall), .cacheExtra = list(studyArea, getStudyArea),
                                       omitArgs = c("enclos", "envir"), verbose = verbose,
                                       cachePath = paths$cachePath, .functionName = "getStudyArea")
    else
      studyArea <- eval(theCall)
  }
  studyArea
}


setupRestart <- function(updateRprofile, paths, name, inProject,
                         Restart = getOption("SpaDES.project.Restart", FALSE),
                         useGit = getOption("SpaDES.project.useGit", FALSE),
                         origGetWd, verbose = getOption("Require.verbose")) {

  if (isTRUE(updateRprofile)) {

    inTmpProject <- inTempProject(paths)
    if (isTRUE(inTmpProject)) {
      warning(.txtUpdateProfileIsTRUE)
    } else {
      if (isRstudio()) {
        inCorrectRstudioProj <- isInRstudioProj(name)
        if (!inProject || !inCorrectRstudioProj) { # either wrong Rstudio or not in project
          if (isFALSE(Restart)) {
            warning("updateRprofile is TRUE, but the projectPath is not an Rstudio project; ",
                    "thus the .Rprofile won't be read upon restart; ignoring updateRprofile = TRUE. ",
                    "Set `Restart = TRUE` to updateRprofile *and* restart R in that folder so ",
                    ".Rprofile will be read")
          }
        }
      }
    }
  }

  if ( (interactive() && (isTRUE(Restart) || is.character(Restart)) ) && isRstudio()
       || !(useGit %in% FALSE)) {# getOption("SpaDES.project.Restart", TRUE))

    on.exit(setwd(origGetWd), add = TRUE)

    if (!(useGit %in% FALSE) && isRstudio() && !inProject) {
      messageVerbose("Because useGit is TRUE or a character string, changing Restart to TRUE", verbose = verbose)
      Restart = TRUE
    }
    pp <- path.expand(paths[["projectPath"]])
    isRstudioProj <- rprojroot::is_rstudio_project$testfun[[1]](pp)
    curRstudioProj <- rstudioapi::getActiveProject()
    isRstudioProj <- isRstudioProj && isTRUE(basename2(curRstudioProj) %in% basename(pp))
    # inProject <- isInProject(name)

    if (!inProject || !isRstudioProj) {
      if (requireNamespace("rstudioapi", lib.loc = paths[["packagePath"]])) {
        messageVerbose("... restarting Rstudio inside the project",
                       verbose = verbose)
        wasUnsaved <- FALSE
        wasLastActive <- FALSE
        activeFile <- rstudioapi::getSourceEditorContext()$path
        if (!is.character(Restart)) {
          rstudioUnsavedFile <- "~/.active-rstudio-document"

          if (!nzchar(activeFile))
            activeFile <- rstudioUnsavedFile
          fe <- file.exists(activeFile)
          wasUnsaved <- identical(activeFile, rstudioUnsavedFile)
          if (isFALSE(fe) || wasUnsaved) {
            if (isTRUE(fe)) {
              newRestart <- file.path(paths[["projectPath"]], "global.R")
              Restart <- activeFile # is absolute
              basenameRestartFile <- basename(newRestart)

              message("Renaming the active, unsaved file: global.R in the new projectPath root")
              wasLastActive <- TRUE
            } else {
              message("There was no 'active-source' file or named Restart file.")
              message("User has requested to restart in a new Rproject; please ")
              message("specify path to the 'global' script (the one that has this setupProject call).")
              message("Please provide name here of that file (e.g., global.R )")
              message("(If it isn't saved, please save it now):")
              Restart <- readline("")
              if (!file.exists(Restart))
                stop("That file does not exist. Please rerun, specifying the global file. This",
                     " will be copied to the new project folder.")
              basenameRestartFile <- basename(Restart)
            }
          } else {
            Restart <- activeFile
            basenameRestartFile <- basename(Restart)
            wasLastActive <- TRUE
          }
        } else {
          basenameRestartFile <- basename(Restart)
        }

        if (!fs::is_absolute_path(Restart))
          Restart <- file.path(origGetWd, Restart)
        newRestart <-  file.path(paths[["projectPath"]], basenameRestartFile)

        # Switch to file to save it
        id <- rstudioapi::navigateToFile(activeFile)
        rstudioapi::documentSave(id)
        # id2 <- rstudioapi::navigateToFile(activeFile)

        copied <- file.copy(Restart, newRestart, overwrite = FALSE)

        # Copy .Rhistory if it exists
        RHistBase <- ".Rhistory"
        RHist <- file.path(origGetWd, RHistBase)
        if (isTRUE(file.exists(RHist)))
          file.copy(RHist, file.path(pp, RHistBase))
        if (all(copied))
          message(green("copied ", Restart, " to ", newRestart))
        else
          message(blue("Did not copy ", Restart, " to ", newRestart, "; it already exists."))
        RprofileInOther <- file.path(paths[["projectPath"]], ".Rprofile")
        RestartTmpFileStart <- ".Restart_"
        tempfileInOther <- file.path(paths[["projectPath"]], paste0(RestartTmpFileStart, basename(tempfile())))
        addToTempFile <- c("setHook('rstudio.sessionInit', function(newSession) {",
                           "if (newSession) {",
                           "# message('Welcome to RStudio ', rstudioapi::getVersion())",
                           "}",
                           "ap <- rstudioapi::getActiveProject()",
                           "if (is.null(ap)) ap <- 'No active project'",
                           "message('This is now an RStudio project and SpaDES.project projectPath: ', ap)",
                           paste0("message('attempting to re-open ", "last active"[wasLastActive],
                                  " file " , paste0("(named ", basenameRestartFile, ") ")[!wasUnsaved],
                                  "(and saved it as global.R as it was unsaved) "[wasUnsaved], "')"),
                           paste0("try(file.edit('", newRestart, "'), silent = TRUE)"), # next line doesn't always work
                           paste0("rstudioapi::navigateToFile('", newRestart, "')")

        )

        newRprofile <- paste0("source('", tempfileInOther, "')")
        if (file.exists(RprofileInOther)) {
          rl <- readLines(RprofileInOther)
          newRprofile <- c(rl, newRprofile)
          lineNext <- paste0("readLns <- readLines('", RprofileInOther, "')")
          lineToDel <- paste0("lineToDel <- grep('^", RestartTmpFileStart,"', readLns)") #paste(rl, collapse = "", ")")
          nextLine <- paste0("readLns <- readLns[-lineToDel]") # remove source line
          nextLine2 <- paste0("cat(readLns, file = '", RprofileInOther, "', sep = '\n')")
          addToTempFile <- c(addToTempFile, lineNext, lineToDel, nextLine, nextLine2)

        } else {
          addToTempFile <- c(addToTempFile, paste0("unlink('", RprofileInOther, "') # delete this .Rprofile that was created"))

        }
        addToTempFile <- c(addToTempFile, paste0("unlink('", tempfileInOther, "') # delete this file"))

        addToTempFile <- c(addToTempFile,
                           "}, action = 'append'",
                           ")")
        cat(addToTempFile, file = tempfileInOther, sep = "\n")
        cat(newRprofile, file = RprofileInOther, sep = "\n")
        cloned <- FALSE

        if (((!useGit %in% FALSE) && requireNamespace("usethis") && requireNamespace("gh") &&
             requireNamespace("gitcreds")) && cloned %in% FALSE ) {

          basenameName <- basename(name)
          needGitUserName <- TRUE
          if (is.character(useGit)) {
            if (useGit != "sub") {
              needGitUserName <- FALSE
              gitUserName <- useGit
            }
          }

          if (needGitUserName) {
            mess <- capture.output(
              type = "message",
              gitUserNamePoss <- gh::gh_whoami()$login)
            if (is.null(gitUserNamePoss)) {
              stop(paste(c(mess, "or try gitcreds::gitcreds_set()"), collapse = "\n"))
            }
            messageVerbose(msgNeedGitUserName(gitUserNamePoss), verbose = interactive() * 10)
            gitUserName <- if (interactive()) readline() else gitUserNamePoss
            if (!nzchar(gitUserName)) {
              gitUserName <- gitUserNamePoss
              needGitUserName <- FALSE
            }

          }


          if (!rprojroot::is_rstudio_project$testfun[[1]](pp)) {
            host <- "https://github.com"
            tf <- tempfile2();
            out <- .downloadFileMasterMainAuth(file.path("https://api.github.com/repos",gitUserName, basenameName),
                                        destfile = tf, verbose = verbose - 10)
            # The suppressWarnings is for "incomplete final line"
            checkExists <- if (file.exists(tf)) suppressWarnings(readLines(tf)) else "Not Found"

            if (any(grepl("Not Found", checkExists))) {
              usethis::create_project(pp, open = FALSE, rstudio = isRstudio())
            } else {
              repo <- file.path(host, gitUserName, basenameName)
              messageVerbose(paste0("The github repository already exists: ", repo),
                             "\nWould you like to clone it now to ", getwd(), "\nType (y)es or any other key for no: ")
              out <- if (interactive()) readline() else "yes"
              if (grepl("y|yes", tolower(out))) {
                setwd(dirname(getwd()))
                unlink(basenameName, recursive = TRUE)
                gert::git_clone(repo, path = basenameName)
                cloned <- TRUE
                setwd(paths[["projectPath"]])
              } else {
                stop("Can't proceed: either delete existing github repo, change the ",
                     "project name, or change the Github account and try again")
              }
            }

          }

          if (!rprojroot::is_git_root$testfun[[1]](pp)) {
            if (needGitUserName) {
              messageVerbose(msgNeedGitUserName(gitUserNamePoss), verbose = interactive() * 10)
              gitUserName <- readline()
            }
            if (!nzchar(gitUserName))
              gitUserName <- NULL
            bbb <- try(usethis::use_git())
            if (!(exists("gitUserNamePoss", inherits = FALSE)))
              gitUserNamePoss <- gh::gh_whoami()$login
            if (identical(gitUserName, gitUserNamePoss))
              gitUserName <- NULL

            githubRepoExists <- usethis::use_github(gitUserName) # This will fail if not an organization
          }
        }
        on.exit(rstudioapi::openProject(path = paths[["projectPath"]], newSession = TRUE))
        message("Starting a new Rstudio session with projectPath (", green(paths[["projectPath"]]), ") as its root")
        stop_quietly()
      } else {
        stop("Please open this in a new Rstudio project at ", paths[["projectPath"]])
      }
    }
  } else {
    if (!Restart %in% FALSE) {
      if (isRstudio())
        messageVerbose("Restart is not FALSE, but this session is not Rstudio, ignoring...",
                       verbose = verbose)
    }
  }
}

setupSpaDES.ProjectDeps <- function(paths,
                                    deps = c("SpaDES.project", "data.table", "Require", "rprojroot", "rstudioapi", "fs"),
                                    verbose = getOption("Require.verbose")) {

  libs <- c(.libPaths()[1], paths[["packagePath"]])
  nsPaths <- vapply(deps, FUN.VALUE = character(1), function(pkg) dirname(getNamespaceInfo(pkg, "path")))
  isLoadedLocally <- !names(nsPaths) %in% libs
  nsPaths <- nsPaths[!names(nsPaths) %in% .basePkgs]

  depsAlreadyInstalled <- Map(lib = nsPaths, pkg = names(nsPaths), function(lib, pkg) {
    pths <- file.path(lib, pkg)
    exist <- file.exists(pths)
    ver <- character(length(pths))
    if (any(exist))
      ver[exist] <- DESCRIPTIONFileVersionV(file.path(pths[exist], "DESCRIPTION"))
    if (any(!exist))
      ver[!exist] <- NA
    names(ver) <- pkg
    ver
  })

  inPkgPath <- dir(paths[["packagePath"]], full.names = TRUE)
  needCopy <- which(!names(depsAlreadyInstalled) %in% basename(inPkgPath))

  if (length(needCopy)) {
    pkgs <- names(depsAlreadyInstalled)[needCopy]
    messageVerbose("Copying ", paste(pkgs, collapse = ", "), " packages to ",
                   paths[["packagePath"]], "", verbose = verbose)

    Map(pkg = pkgs, from = nsPaths[needCopy], function(pkg, from) {
      isInstalled <- file.exists(file.path(from, pkg, "INDEX"))
      if (isInstalled)
        copyPackages(pkg, from = from, to = paths[["packagePath"]], verbose = 0)
    })
    messageVerbose("If you have difficulties, please restart R", verbose = verbose)
  }

  return(invisible())

  # Need to deal with NA, also version differences
  # diffVersion <- Require::setdiffNamed(depsAlreadyInstalled[[1]], depsAlreadyInstalled[[2]])
  # diffs <- names(diffVersion)
  #
  # theNAs <- is.na(depsAlreadyInstalled[[2]])
  # toInstall <- character()
  # toRevInstall <- character()
  # if (any(theNAs)) {
  #   toInstall <- names(theNAs[theNAs])
  # }
  #
  # notNAs <-
  #   Map(dps = depsAlreadyInstalled, function(dps) {
  #     ret <- as.list(as.numeric_version(dps[!theNAs]))
  #   })
  #
  # notNAs <- Require::invertList(notNAs)
  # needUpdate <- unlist(Map(pkg = notNAs, function(pkg) pkg[[1]] > pkg[[2]]), recursive = FALSE)
  # if (any(needUpdate))
  #   toInstall <- unique(c(toInstall, names(needUpdate[needUpdate])))
  #
  # toInstall <- setdiff(toInstall, .basePkgs)
  #
  # needRevUpdate <- unlist(Map(pkg = notNAs, function(pkg) pkg[[1]] < pkg[[2]]), recursive = FALSE)
  # if (any(needRevUpdate))
  #   toRevInstall <- unique(c(toRevInstall, names(needRevUpdate[needRevUpdate])))
  #
  # toRevInstall <- setdiff(toRevInstall, .basePkgs)
  #
  #
  # if (FALSE) { # This is the older, slower way
  #   depsAlreadyInstalled <- dir(paths[["packagePath"]], pattern = paste0(paste0("^", deps, "$"), collapse = "|"))
  #   diffVersion <- Map(dai = depsAlreadyInstalled, function(dai) {
  #     if (file.exists(file.path(paths[["packagePath"]], dai, "DESCRIPTION"))) {
  #       pvLoaded <- packageVersion(dai)
  #       pvLibLoc <- packageVersion(dai, lib.loc = .libPaths()[1])
  #       pvPathsPackagePath <- packageVersion(dai, lib.loc = paths[["packagePath"]])
  #       loadedFrom <- if (identical(pvPathsPackagePath, pvLoaded)) {
  #         "packagePath"
  #       } else {
  #         "libPaths"
  #       }
  #
  #       if (pvLibLoc < pvPathsPackagePath) {# test whether lib loc is lt; so, need to unload; then move from to packagePath; reload
  #         out <- dai
  #       } else if (pvLibLoc > pvPathsPackagePath) { #test whether lib loc is gt; so, need to move to packagePath
  #         out <- list(Package = dai, pvLibLoc, pvPathsPackagePath)
  #       } else {
  #         out <- NULL
  #       }
  #     } else {
  #       out <- NULL
  #     }
  #     out
  #   })
  #   # First check for loaded old version; needs user intervention
  #   needStop <- vapply(diffVersion, function(x) is.character(x), FUN.VALUE = logical(1))
  #   if (any(needStop)) {
  #     pkgsToUpdate <- names(needStop)[needStop]
  #     pkgs <- paste(pkgsToUpdate, collapse = "', '")
  #
  #     stop("\nThe version of ", pkgs, " need updating in the personal library.\n",
  #          "Please restart R and update ", pkgs, ", e.g., using: ",
  #          "\ninstall.packages(c('", pkgs, "'), lib = '", .libPaths()[1],"')")
  #   }
  #   diffVersionNames <- names(diffVersion[!vapply(diffVersion, is.null, FUN.VALUE = logical(1))])
  #   deps <- setdiff(deps, depsAlreadyInstalled)
  #   if (length(diffVersionNames)) {
  #     messageVerbose("Updating ", paste0(diffVersionNames, collapse = ", "), " in paths$packagePath ",
  #                    "because it has been updated in .libPaths()[1]. To turn this updating off, set\n",
  #                    "options(SpaDES.project.updateSelf = FALSE)")
  #     if (!isFALSE(getOption("SpaDES.project.updateSelf")))
  #       deps <- c(deps, diffVersionNames)
  #   }
  # }
  #
  # if (length(toInstall)) {
  #   copyPackages(toInstall, from = .libPaths()[1], to = paths[["packagePath"]], verbose = verbose)
  # }
  # if (length(toRevInstall)) {
  #   copyPackages(toRevInstall, to = .libPaths()[1], from = paths[["packagePath"]], verbose = verbose)
  #   messageVerbose("If you have difficulties, please restart R")
  # }

}

checkNameProjectPathConflict <- function(name, paths) {
  if (!missing(paths)) {
    if (!is.null(paths[["projectPath"]])) {
      paths[["projectPath"]] <- normPath(paths[["projectPath"]])
      prjNmBase <- basename2(paths[["projectPath"]])
      if (!identical(basename(name), prjNmBase) && !is.null(prjNmBase)) {
        warning("both projectPath and name are supplied, but they are not the same; ",
                "these must be the same basename; using projectPath: ", paths[["projectPath"]])
        name <- prjNmBase
      }
    }
  }
  name
}

basename2 <- function (x) {
  if (is.null(x)) {
    NULL
  }
  else {
    basename(x)
  }
}

dotsToHereOuter <- function(dots, dotsSUB, defaultDots, envir = parent.frame()) {
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots, envir = envir)
  dotsSUBreworked <- list()
  for (i in seq(dotsSUB)) {
    val <- try(eval(dotsSUB[[i]], envir = envir))
    if (is(val, "try-error"))
      val <- dotsSUB[[i]]
    newNam <- names(dotsSUB)[i]
    dotsSUBreworked[[newNam]] <- val
    assign(newNam, val, envir = envir)
  }
  if (exists("dotsSUBreworked"))
    dotsSUB <- dotsSUBreworked
  dotsSUB
}

stripDuplicateFolder <- function(relativeFilePath, path) {
  relativeFilePath <- fs::path_norm(relativeFilePath) # don't want absolute, just consisten /
  path <- fs::path_norm(path) # don't want absolute, just consisten /
  if (isTRUE(startsWith(relativeFilePath, path) %in% TRUE)) {
    path <- dirname(path)
    #comm <- fs::path_common(c(relativeFilePath, path))
    #relativeFilePath <- fs::path_rel(relativeFilePath, comm)
  }
  fs::path_norm(file.path(path, relativeFilePath))
}


parseExpressionSequentially <- function(pp, envs, namedList, verbose) {
  isFun <- FALSE
  envs2 <- lapply(pp, function(p) {
    env <- new.env(parent = tail(envs, 1)[[1]])
    if (isUnevaluatedList(p) || isFALSE(namedList) || is(p, "<-")) {
      # robust to code failures
      # Try whole list first; if fails, then do individual list elements
      withCallingHandlers(for (i in 1:2) {
        safe <- TRUE
        out <- evalListElems(p, envir = env, verbose = verbose)
        if (safe)
          break
      }, message = function(m) {

        missingPkgs <- grepl("there is no package called", m)
        if (any(missingPkgs)) {
          pkgs <- gsub("^.+called \u2018(.+)\u2019.*$", "\\1", m$message)
          pkgs <- gsub("^.+called '(.+)'.*$", "\\1", pkgs)
          messageVerbose(pkgs, " is missing; attempting to install it. ",
                         "\nIf this fails, please add it manually to the `packages` argument",
                         verbose = verbose)
          Require::getCRANrepos(ind = 1)
          repos <- getOption("repos")
          repos <- repos[!duplicated(repos)]
          Require::Install(pkgs, repos = repos)
          safe <<- FALSE
          invokeRestart("muffleMessage")
        }
      })
      isFun <<- isFun || is.function(out) # in case there are some functions, some not; keep status as isFun = TRUE
      if (length(ls(env)) == 0) # the previous line will evaluated assignments e.g., mode <- "development",
        # putting the object `mode` into the env; but if there is no assignment
        # then we need to put the object into the environment
        env$opt <- out
      envs <<- append(envs, list(env))

    }
    env
  })
  if (isTRUE(isFun)) {
    lapply(envs2[-1], function(env) {
      list2env(mget(ls(envir = env), envir = env), envir = envs2[[1]]
      )})
    envs2 <- list(envs2[[1]])
  }
  envs2
}


parseEnvsWithNamedListsSequentially <- function(envs2) {
  os <- list()

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

mergeOpts <- function(opts, optsFirst, verbose = getOption("Require.verbose", 1L)) {
  b <- opts$updates
  a <- optsFirst$updates
  if (!is.null(b))
    a <- b[a, on = "optionName"]
  if (!is.null(a)) {
    if (NROW(a)) {
      messageVerbose(yellow("  options changed:"), verbose = verbose, verboseLevel = 0)
      if (!is.null(b)) {
        a[unlist(lapply(newValue, is.null)), newValue := i.newValue]
        a[, oldValue := i.oldValue]
        a[, `:=`(i.newValue = NULL, i.oldValue = NULL)]
      }
      messageDF(a, verbose = verbose)
    }
  } else {
    messageVerbose(yellow("  no options changed"), verbose = verbose, verboseLevel = 0)
  }

  opts$updates <- a
  opts
}

isProjectGitRepo <- function(projectPath, inProject) {
  if (isTRUE(inProject))
    projectPath <- "."
  length(dir(pattern = "^\\.git$", path = projectPath, all.files = TRUE)) == 1L
}

ignoreAFolder <- function(gitIgnoreFile = ".gitIgnore", folder, projectPath) {
  if (!file.exists(gitIgnoreFile))
    file.create(gitIgnoreFile)
  gi <- readLines(gitIgnoreFile)
  cp <- a <- fs::path_rel(folder, projectPath)
  cachePathGrep <- paste0("^", cp, "(\\/)*", "$")
  if (!any(grepl(cachePathGrep, gi))) {
    cat(cp, file = ".gitIgnore", sep = "\n", append = TRUE)
  }
}

stop_quietly <- function(mess) {
  opt <- base::options(show.error.messages = FALSE)
  on.exit(base::options(opt))
  stop(mess)
}


copyPackages <- function(pkgs, from = .libPaths()[1], to, verbose) {
  messageVerbose("Copying ", paste(pkgs, collapse = ", "), " packages to ",
                 to, "", verbose = verbose)

  if (!identical(normPath(from), to) &&
      (!identical(dirname(normPath(from)), to)))
    for (pkg in pkgs) {
      pkgDir <- file.path(from, pkg)
      de <- dir.exists(pkgDir)
      pkgDir <- pkgDir[de][1]
      files1 <- dir(pkgDir, all.files = TRUE, recursive = TRUE)
      newFiles <- file.path(to, pkg, files1)
      lapply(unique(dirname(newFiles)), dir.create, recursive = TRUE, showWarnings = FALSE)
      oldFiles <- file.path(pkgDir, files1)
      linkOrCopy(oldFiles, newFiles)
    }
}



getStudyArea <- function(studyArea, paths, verbose = verbose) {
  needRep <- !requireNamespace("reproducible", quietly = TRUE)
  needGeo <- !requireNamespace("geodata", quietly = TRUE)
  if (needRep || needGeo) {
    needs <- c("reproducible"[needRep], "geodata"[needGeo])
    stop("Please install ", paste0(needs, collapse = " and "))
  }

  studyAreaOrig <- studyArea
  if (is.null(studyArea[["country"]]))
    studyArea[["country"]] <- "CAN"
  unnamed <- !nzchar(names(studyArea))
  if (any(unnamed))
    names(studyArea)[unnamed] <- "subregion"

  subregion <- studyArea[["subregion"]]

  studyArea <- append(studyArea, list(path = paths$inputPath))

  geodatCall <- as.call(append(list(geodata::gadm), studyArea))
  geodatCall <- match.call(geodata::gadm, geodatCall)
  geodatCall <- as.call(append(list(geodata::gadm), as.list(geodatCall)[names(geodatCall) %in% formalArgs(geodata::gadm)]))

  studyAreaNoPath <- studyArea[-which(names(studyArea) %in% "path")]
  epsg <- if (!is.null(studyArea$epsg)) paste0("epsg:", studyArea$epsg) else NULL

  studyArea[["epsg"]] <- NULL

  studyAreaOrig <- studyArea
  studyArea <- withCallingHandlers(
    do.call(geodata::gadm, as.list(geodatCall[-1])),
    message = function(m)
      if (grepl("geodata server seems|The geodata server is down for maintenance", m$message)) {
        message("Trying a Google Drive stashed copy")

      }
  )
  if (is.null(studyArea)) {
    if (isTRUE(studyAreaOrig[["country"]] %in% "CAN")) {
      "https://drive.google.com/file/d/1OhYZymGc9VlLg-X0ZlBalBNjDrzT9XtP/view?usp=sharing"
      message("Because the country is Canada, will try manually hosted file that represents level 2 ")
      studyArea <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1DdtWeFYEhSRxXcAaJ_J6i8hP8YbfoC1q/view?usp=drive_link",
                                            destinationPath = paths$inputPath)

      if (!is(studyArea, "SpatVector")) {
        studyArea <- terra::vect(studyArea)
      }

      # otherURL <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000a21a_e.zip"
      # message(otherURL)
      # a <- reproducible::prepInputs(url = otherURL)
      # colnames(a)[which(colnames(a) == "PRENAME")] <- "NAME_2"
    }
  }

  hasSubregion <- grep("subregion", names(studyAreaOrig))
  names(studyAreaOrig)[hasSubregion] <- "NAME_1"
  poss <- setdiff(names(studyAreaOrig), c("country", "level", "path"))
  tos <- c("projectTo", "maskTo", "cropTo", "to")
  poss <- setdiff(poss, tos)
  for (col in poss) {
    saCol <- as.data.frame(studyArea)[[col]]
    greppedNames <- grep(saCol, pattern = studyAreaOrig[[col]], value = TRUE)
    colInSA <- as.data.frame(studyArea)[[col]]
    if (is.null(colInSA)) {
      warning("There is no column ", col, "; ",
              "\nDid you mean one or more of:\n  ", paste(names(studyArea), collapse = "\n  "),
              "\nSkipping subsetting of studyArea by ", col
      )

    } else {
      keep <- which(tolower(colInSA) %in% tolower(greppedNames))
      if (length(keep) == 0) {
        warning(studyAreaOrig[[col]], " does not match any values in ", col,". ",
                "\nDid you mean one or more of:\n  ", paste(colInSA, collapse = "\n  "),
                "\nReturning empty studyArea")
      }
      studyArea <- studyArea[keep, ]
    }
  }

  if (any(names(studyAreaOrig) %in% tos)) {
    if (requireNamespace("reproducible")) {
      tos <- intersect(tos, names(studyAreaOrig))
      studyArea <- do.call(reproducible::postProcessTo, append(list(studyArea), studyAreaOrig[tos]))
    } else {
      message("Cannot project/mask/crop without `reproducible` package")
    }
  }
  # if (requireNamespace("terra")) {
    # studyArea <- studyArea |> terra::project("epsg:4269") # seemed to fail if not in this longlat
  # }
  if (!is.null(epsg))
    if (requireNamespace("terra")) {
      studyArea <- studyArea |> terra::project(epsg)
    } else {
      warning("Could not reproject; need ")
    }
  studyArea
}

checkUseGit <- function(useGit) {
  isTRUE(!(useGit %in% FALSE))
}


#' @importFrom utils browseURL
checkGitRemote <- function(name, paths) {
  # If this function is run, it means that local is not yet a git repo
  message("Please provide the github account for the repository (without quotes): ")
  gitUserName <- readline()
  if (!nzchar(gitUserName))
    stop("Need to supply the account name for the repository (not the repository name)")

  tf <- tempfile()
  urlCheckGit <- file.path("https://api.github.com/repos", gitUserName, name)#, destfile = tf)
  out <- capture.output(type = "message",
                        outSkip <- try(.downloadFileMasterMainAuth(urlCheckGit, destfile = tf)))
  od <- getwd()

  if (isTRUE(any(grepl("cannot open URL", out)) || identical(out, character(0)))) {
    message(paste0("It looks like the repository does not exist, please  go to github.com, create a new repository for: ",
                   gitUserName, " repo name: ", name, "; return here, press enter to continue"))
    browseURL(file.path("https://github.com", paste0(gitUserName, "?tab=repositories")))
    readline()

    checkPath(paths[["projectPath"]], create = TRUE)
    setwd(paths[["projectPath"]])
    on.exit(setwd(od))
    pp <- path.expand(paths[["projectPath"]])

    if (!requireNamespace("usethis")) stop("Please install usethis")

    bbb <- usethis::use_git()
    if (isTRUE(bbb)) {
      usethis::use_github(gitUserName)
      stop_quietly()
    }
  } else {
    message("It looks like the remote Git repo exists (",file.path(gitUserName, name),
            "). Would you like to clone it now to ", paths[["projectPath"]], "?")
    cloneNow <- readline("Y or N (if N, this will stop): ")
    if (startsWith(tolower(cloneNow), "y")) {
      if (normalizePath(getwd()) == normalizePath(paths[["projectPath"]], mustWork = FALSE)) {
        stop("Cannot clone into projectPath because it already exists; please delete it; then rerun this.")
      }
      projectBase <- dirname(paths[["projectPath"]])
      dir.create(projectBase, showWarnings = FALSE, recursive = TRUE)
      setwd(projectBase)
      on.exit(setwd(od))
      cmd <- paste0("git clone git@github.com:", gitUserName, "/", name)
      system(cmd)
      setwd(name)
      if (length(dir(pattern = ".gitmodules", all.files = T))) {
        cmd <- paste0("git submodule init")
        a <- system(cmd, intern = TRUE)
        cmd <- paste0("git submodule update --recursive")
        b <- system(cmd, intern = TRUE)
      }
    } else {
      stop("Please clone the project manually, or choose another account and repository")
    }

  }
  return(gitUserName)
}


fastOptions <- function() {
  list(reproducible.useMemoise = TRUE,                 # For caching
       reproducible.inputPaths = getOption("reproducible.inputPaths"),
       reproducible.objSize = FALSE,
       Require.cloneFrom = Sys.getenv("R_LIBS_USER"),  # For package installs
       spades.recoveryMode = FALSE,
       spades.moduleCodeChecks = FALSE,
       spades.sessionInfo = FALSE,
       spades.testMemoryLeaks = FALSE,
       spades.useRequire = FALSE,
       spades.allowSequentialCaching = FALSE,
       reproducible.memoisePersist = TRUE,
       reproducible.cacheSaveFormat = "qs",
       LandR.assertions = FALSE,
       reproducible.cacheSpeed = "fast",
       reproducible.gdalwarp = TRUE,
       spades.recoveryMode = 1
  )
}

makeUpdateRprofileSticky <- function(updateRprofile) {
  if (isTRUE(updateRprofile) && !getOption("Require.updateRprofile") %in% TRUE)
    options(Require.updateRprofile = updateRprofile)
}


errorMsgCleaning <- function(mess, valOrig) {
  mess <- c(paste0(format(valOrig), "\n"), mess)
  mess <- gsub("Error in h.simpleError.msg, call.. : ", "", mess)
  mess <- gsub("Error in eval.{1,3}FUNcaptured.{1,3}, envir = callingEnv.{1,3} : ", "", mess)
  mess <- gsub("\n", "", mess)
  mess <- paste0(mess, "\n")
  mess
}


#' @author Ceres Barros
extractModName <- function(modules) {
  modNams <- Vectorize(.extractModName, USE.NAMES = FALSE)(modules)
  return(modNams)
}

#' @author Ceres Barros
.extractModName <- function(modules) {
  if (tools::file_ext(modules) != "") {
    stop("Expecting local or GitHub path to the module *folder* not .R file.")
  }

  modNam <- extractPkgName(modules)
  modNam2 <- sub("@[^/]*/?", "", basename(modules))   ## is there something after @Branch/?
  if (modNam != modNam2) {
    modNam <- modNam2
  }
  return(modNam)
}

#' SpaDES.project default .libPaths() directory
#'
#' For a given `name`, this will return the default library for packages.
#'
#' @param name A text string. When used in `setupProject`, this is the `projectName`
#' @export
#' @return
#' A path where the packages will be installed.
.libPathDefault <- function(name) {
  pkgPth <- tools::R_user_dir(package = basename(name), which = "data")
  normalizePath(
    file.path(pkgPth, "packages", version$platform, substr(getRversion(), 1, 3)),
    mustWork = FALSE, winslash = "/")
}



msgNeedGitUserName <- function(gitUserNamePoss) {
  paste0("Please provide the github account if an organization, or press enter ",
  "to use your personal account (",gitUserNamePoss , ") for the repository (without quotes): ")
}



fileRelPathFromFullGHpath <- function(pathGH) {
  m <- gsub("@[[:alnum:]_-]+$", "", pathGH)
  m <- gsub("@[[:alnum:]_]+/", "/", m)
  m <- lapply(strsplit(m, "/"), function(r) r[-c(1, length(r))])
  m <- vapply(m, paste, collapse = "/", FUN.VALUE = character(1))
  m
}


simplifyModuleName <- function(modules) {
  modulesSimple1 <- Require::extractPkgName(modules)
  modulesSimple2 <- Require::extractPkgName(unname(modules))
  take1st <- grepl("@", modulesSimple2)
  modulesSimple <- ifelse(take1st, modulesSimple1, modulesSimple2)
  modulesSimple
}

#' @rdname setup
#' @export
#' @inheritParams setupProject
#' @param files A vector or list of files to parse. These can be remote github.com files.
#'
#' @details
#' `setupFiles` is a convenience function intended for interactive use to verify the files being parsed.
#' This is similar to `parse`, but each element must be a named list or a named object, such as a function.
#' It uses the same specification for \url{https://github.com}
#' files as `setupProject`, i.e., using `@` for branch.
#'
#' ```
#' setupFiles("PredictiveEcology/PredictiveEcology.org@main/tutos/castorExample/params.R")
#' ```
#'
#' @return
#' `setupFiles` a named list with each element that was parsed.
setupFiles <- function(files, paths, envir = parent.frame(), verbose = getOption("Require.verbose", 1L)) {
  messageVerbose(yellow("setting up and parsing files ..."), verbose = verbose, verboseLevel = 0)
  envirCur = environment()

  if (missing(paths))
    paths <- list(projectPath = normalizePath(".", mustWork = FALSE, winslash = "/"))

  pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = file.path(paths[["projectPath"]]))`
  paths <- evalSUB(val = pathsSUB, valObjName = "paths", envir = envirCur, envir2 = envir)
  outs <- parseFileLists(files, paths = paths, envir = envir)
  outs
}

assignDefaults <- function(checkDefaults = c("Restart", "useGit", "setLinuxBinaryRepo", "standAlone", "updateRprofile", "overwrite"),
                           env = parent.frame()) {
  defaults <- spadesProjectOptions()
  lapply(checkDefaults, function(def) {
    if (is.null(get(def, envir = env, inherits = FALSE)))
      assign(def, defaults[[paste0("SpaDES.project.", def)]], envir = env)
  })
}
