utils::globalVariables(c(
  ".", "i.module", "i.objectClass", "objectClass", "objectName"
))

#' Sets up a new or existing SpaDES project
#'
#' `setupProject` calls a sequence of functions in this order:
#' `setupPaths`, `setupModules`, `setupPackages`, `setupOptions`,
#' `setupSideEffects`, `setupParams`,
#' `setupGitIgnore`. Because of this
#' sequence, users can take advantange of settings that happen before others. For
#' example, users can set paths, then use those paths while setting up
#' `params`, or they can set `options` that will can update/change `paths`,
#' `times` can be used in `params`,
#' for example.
#' This sequence will create folder structures, install packages in either the `packages`
#' or `require` arguments, load packages only from the `require` argument,
#' set options, download or confirm the existence of modules, install missing
#' packages from both the modules `reqdPkgs` fields and the user passed
#' `packages` or `require`. It will also return elements that can be passed
#' directly to `simInit`  or `simInitAndSpades`, specifically, `modules`, `params`,
#' `paths`, `times`, and any named elements passed to `...`. This function will also
#' , if desired, change the .Rprofile file for this
#'  this project so that every time this project is opened, it has a specific
#'  `.libPaths()`. There are a number of convenience elements described in the
#'  section below. See Details.
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
#' @param modules a character string of modules to pass to `getModule`. These
#'   should be one of: simple name (e.g., `fireSense`) which will be searched for locally
#'   in the `paths[["modulePath"]]`; or a GitHub repo with branch (`GitHubAccount/Repo@branch` e.g.,
#'   `"PredictiveEcology/Biomass_core@development"`); or a character vector that identifies
#'   one or more (not optional file extension) `.R` file(s) (local or GitHub)
#'   to parse that will produce a character vector assigned to
#'   the name "modules". If the entire project is a git repository,
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
#'   not attached to the search path. See also the `require` argument.
#'   See [setup].
#' @param require Optional. A character vector of packages to install *and* attach
#'   (with `Require::Require`). These will be installed and attached at the start
#'   of `setupProject` so that a user can use these during `setupProject`.
#'   See [setup]
#' @param options Optional. Either a named list to be passed to `options`
#'   or a character vector indicating one or more file(s) to source,
#'   in the order provided. These will be sourced into a temporary environment (not
#'   the `.GlobalEnv`), so they will not create globally accessible objects. See details.
#'   See [setup].
#' @param params Optional. Similar to `options`, however, this named list will be
#'   returned, i.e., there are no side effects.
#'   See [setup].
#' @param sideEffects Optional. This can be an expression or one or more filenames or
#'   a code chunk surrounded by `{...}`.
#'   This/these will be parsed and evaluated, but nothing returned. This is intended
#'   to be used for functions, such as cloud authentication or configurations,
#'   that are run for their side effects only.
#'   See [setup].
#' @param useGit A logical. If `TRUE`, it will use `git clone` and `git checkout`
#'   to get and change branch for each module, according to its specification in
#'   `modules`. Otherwise it will get modules with `getModules`.
#' @param standAlone A logical. Passed to `Require::standAlone`. This keeps all
#'   packages installed in a project-level library, if `TRUE`. Default is `TRUE`.
#' @param libPaths Deprecated. Use `paths = list(packagePath = ...)`.
#' @param Restart If the `projectPath` is not the current path, and the session is in
#'   Rstudio, and interactive, it will restart with a new Rstudio session with a
#'   new project, with a root path set to `projectPath`. Default is `FALSE`.
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
#' @param overwrite Logical. Passed to `getModule`, and `setupParams`, `setupOptions`
#' @param dots Any other named objects passed as a list a user might want for other elements.
#' @param defaultDots A named list of any arbitrary R objects.
#'   These can be supplied to give default values to objects that
#'   are otherwise passed in with the `...`, i.e., not specifically named for these
#'   `setup*` functions. If named objects are supplied as top-level arguments, then
#'   the `defaultDots` will be overridden. This can be particularly useful if the
#'   arguments passed to `...` do not always exist, but rely on external e.g., batch
#'   processing to optionally fill them. See examples.
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
#' @export
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
#' \subsection{Values and/or files}{
#' The arguments, `paths`, `options`, and `params`, can all
#' understand lists of named values, character vectors, or a mixture by using a list where
#' named elements are values and unnamed elements are character strings/vectors. Any unnamed
#' character string/vector will be treated as a file path. If that file path has an `@` symbol,
#' it will be assumed to be a file that exists on `https://github.com`. So a user can
#' pass values, or pointers to remote and/or local paths that themselves have values.
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
#' helpful for clarity to put some in an external file.
#' }
#'
#' \subsection{Specifying `paths`, `options`, `params`}{
#' If `paths`, `options`, and/or `params`are a character string
#' or character vector (or part of an unnamed list element) the string(s)
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
#' @seealso [setupPaths()], [setupOptions()], [setupPackages()],
#' [setupModules()], [setupGitIgnore()]. Also, helpful functions such as
#' [user()], [machine()], [node()]
#'
#' @return
#' `setupProject` will return a named list with elements `modules`, `paths`, `params`, and `times`.
#' The goal of this list is to contain list elements that can be passed directly
#' to `simInit`
#' NOTE: both `projectPath` and `packagePath` will be omitted in the `paths` list
#' as they are used to
#' set current directory (found with `getwd()`) and `.libPaths()[1]`, and `simInit`
#' does not accept these. `setupPaths` will return these two paths as it is not
#' expected to be passed directly to `simInit`.
#' It will also append all elements passed by the user in the `...`.
#' This list  can be passed directly to `SpaDES.core::simInit()` or
#' `SpaDES.core::simInitAndSpades()` using a `do.call()`. See example.
#'
#' @importFrom Require extractPkgName
#' @inheritParams Require::Require
#' @inheritParams Require::setLibPaths
#' @rdname setupProject
#' @seealso \code{vignette("SpaDES project setup", package = "SpaDES.project")}
#'
#' @examples
#' ## THESE EXAMPLES ARE NOT INTENDED TO BE RUN SEQUENTIALLY AS THEY WILL LOAD PACKAGES
#' ## THAT WILL CONFLICT. PLEASE RESTART R BETWEEN EXAMPLES
#' library(SpaDES.project)
#'
#' # Run all tests in a temporary directory, do not disrupt user's current project
#' \dontshow{tmpdir <- Require::tempdir2() # for testing tempdir2 is better}
#' \dontshow{
#' if (is.null(getOption("repos"))) {
#'   options(repos = c(CRAN = "https://cloud.r-project.org"))
#' }
#' }
#'  ## simplest case; just creates folders
#' out <- setupProject(
#'   paths = list(projectPath = ".") #
#' )
setupProject <- function(name, paths, modules, packages,
                         times, options, params, sideEffects, config,
                         require = NULL, studyArea = NULL,
                         Restart = getOption("SpaDES.project.Restart", FALSE),
                         useGit = FALSE, setLinuxBinaryRepo = TRUE,
                         standAlone = TRUE, libPaths = NULL,
                         updateRprofile = getOption("Require.updateRprofile", FALSE),
                         overwrite = FALSE, # envir = environment(),
                         verbose = getOption("Require.verbose", 1L),
                         defaultDots,
                         dots, ...) {

  envir = environment()

  origArgOrder <- names(sys.calls()[[1]])
  firstNamedArg <- min(which(origArgOrder %in% formalArgs(setupProject)))
  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsLater <- dotsSUB
  if (firstNamedArg > 2) { # there is always an empty one at first slot
    firstSet <- (1:(firstNamedArg - 2))
    dotsLater <- dotsSUB[-firstSet]
    dotsSUB <- dotsSUB[firstSet]
    dotsSUB <- dotsToHereOuter(dots, dotsSUB, defaultDots)
  }

  modulesSUB <- substitute(modules) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  paramsSUB <- substitute(params) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  optionsSUB <- substitute(options) # must do this in case the user passes e.g., `list(fireStart = times$start)`
  pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = paths$projectpath)`
  sideEffectsSUB <- substitute(sideEffects)
  libPaths <- substitute(libPaths)

  if (missing(times))
    times <- list(start = 0, end = 1)

  pathsSUB <- checkProjectPath(pathsSUB, name, envir = envir, envir2 = parent.frame())

  if (missing(name)) {
    name <- basename(normPath(pathsSUB[["projectPath"]]))
  } else {
    name <- checkNameProjectPathConflict(name, paths)
  }
  gtwd <- getwd()
  weird <- 0
  while (is.null(gtwd)) { # unknown why this returns NULL sometimes
    gtwd <- getwd()
    if (weird > 10)
      browser()
    weird <- weird + 1
  }
  inProject <- isInProject(name)

  paths <- setupPaths(name, pathsSUB, inProject, standAlone, libPaths,
                      updateRprofile) # don't pass envir because paths aren't evaluated yet

  setupSpaDES.ProjectDeps(paths, verbose = getOption("Require.verbose"))

  modulePackages <- setupModules(name, paths, modulesSUB, useGit = useGit,
                                 overwrite = overwrite, envir = envir, verbose = verbose)
  modules <- Require::extractPkgName(names(modulePackages))
  names(modules) <- names(modulePackages)

  if (missing(packages))
    packages <- NULL

  if (!is.null(studyArea))
    if (is(studyArea, "list"))
      packages <- unique(c(packages, c("geodata")))

  setupPackages(packages, modulePackages, require = require,
                setLinuxBinaryRepo = setLinuxBinaryRepo,
                standAlone = standAlone,
                libPaths = paths[["packagePath"]], envir = envir, verbose = verbose)

  if (!is.null(studyArea)) {
    dotsSUB$studyArea <- setupStudyArea(studyArea, paths)
  }

  sideEffectsSUB <- setupSideEffects(name, sideEffectsSUB, paths, times, overwrite = overwrite,
                                     envir = envir, verbose = verbose)

  opts <- setupOptions(name, optionsSUB, paths, times, overwrite = overwrite, envir = envir)
  options <- opts[["newOptions"]] # put into this environment so parsing can access

  # Run 2nd time after sideEffects & setupOptions -- may not be necessary
  # setupPackages(packages, modulePackages, require = require,
  #               setLinuxBinaryRepo = setLinuxBinaryRepo,
  #               standAlone = standAlone,
  #               libPaths = paths[["packagePath"]], envir = envir, verbose = verbose)

  if (!missing(config)) {
    messageVerbose("config is supplied; using `SpaDES.config` package internals", verbose = verbose)
    if (!requireNamespace("SpaDES.config", quietly = TRUE)) {
      Require::Install("PredictiveEcology/SpaDES.config@development")
    }
    localVars <- if (length(names(dotsSUB)))
      mget(names(dotsSUB), envir = envir, inherits = FALSE) else list()
    messageWarnStop("config is not yet setup to run with SpaDES.project")
    if (FALSE)
      out <- do.call(SpaDES.config::useConfig, append(
        list(projectName = config,
             projectPath = paths[["projectPath"]], paths = paths),
        localVars))

  }

  # TODO from here to out <-  should be brought into the "else" block when `SpaDES.config is worked on`
  params <- setupParams(name, paramsSUB, paths, modules, times, options = opts[["newOptions"]],
                        overwrite = overwrite, envir = envir, verbose = verbose)


  if (length(dotsLater)) {
    dotsSUB <- dotsLater
    dotsSUB <- dotsToHereOuter(dots, dotsSUB, defaultDots)
  }

  setupGitIgnore(paths, gitignore = getOption("SpaDES.project.gitignore", TRUE), verbose)

  setupRestart(updateRprofile, paths, name, inProject, Restart, verbose) # This may restart

  out <- append(list(
    modules = modules,
    paths = paths[spPaths], # this means we lose the packagePath --> but it is in .libPaths()[1]
                            # we also lose projectPath --> but this is getwd()
    params = params,
    times = times), dotsSUB)
  if (!is.null(options))
    attr(out, "projectOptions") <- options

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
                       updateRprofile = getOption("Require.updateRprofile", FALSE),
                       overwrite = FALSE, envir = environment(),
                       verbose = getOption("Require.verbose", 1L), dots, defaultDots, ...) {

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  messageVerbose(yellow("setting up paths ..."), verbose = verbose)

  pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = file.path(paths[["projectPath"]]))`
  pathsSUB <- checkProjectPath(pathsSUB, name, envir, parent.frame())

  paths <- evalSUB(val = pathsSUB, valObjName = "paths", envir = envir, envir2 = parent.frame())
  paths <- parseFileLists(paths, paths[["projectPath"]], overwrite = overwrite,
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
    pkgPth <- tools::R_user_dir(package = basename(name), which = "data")
    paths[["packagePath"]] <- normalizePath(
      file.path(pkgPth, "packages", version$platform, substr(getRversion(), 1, 3)),
      mustWork = FALSE, winslash = "/")
  }

  if (is.null(paths[["modulePath"]])) paths[["modulePath"]] <- "modules"
  isAbs <- unlist(lapply(paths, isAbsolutePath))
  toMakeAbsolute <- isAbs %in% FALSE & names(paths) != "projectPath"
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

  if (is.null(paths$scratchPath)) {
    paths$scratchPath <- file.path(tempdir(), name)
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
         inputPath = file.path(paths[["projectPath"]], "inputs"),
         outputPath = file.path(paths[["projectPath"]], "outputs")
    ),
    paths)

  paths <- lapply(paths, normPath)

  changedLibPaths <- (!identical(normPath(.libPaths()[1]), paths[["packagePath"]]) &&
                        (!identical(dirname(normPath(.libPaths()[1])), paths[["packagePath"]])))
  # changedLibPaths <- !identical(normPath(.libPaths()[1]), paths[["packagePath"]])

  Require::setLibPaths(paths[["packagePath"]], standAlone = standAlone,
                       updateRprofile = updateRprofile,
                       exact = FALSE, verbose = verbose)
  paths[["packagePath"]] <- .libPaths()[1]

  do.call(setPaths, paths[spPaths])

  messageVerbose(yellow("  done setting up paths"), verbose = verbose)

  paths[order(names(paths))]
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
                             envir = environment(), verbose = getOption("Require.verbose", 1L),
                             dots, defaultDots, ...) {

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (!missing(sideEffects)) {
    messageVerbose(yellow("setting up sideEffects..."), verbose = verbose)

    sideEffectsSUB <- substitute(sideEffects) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    sideEffects <- evalSUB(sideEffectsSUB, valObjName = "sideEffects", envir = envir, envir2 = parent.frame())

    if (!is.character(sideEffects)) { # this is because I wrote this second;
      tf <- tempfile()
      writeLines(format(sideEffects[-1]), con = tf)
      sideEffects <- tf
    }

    sideEffects <- parseFileLists(sideEffects, paths[["projectPath"]], namedList = FALSE,
                                  overwrite = overwrite, envir = envir, verbose = verbose)
    messageVerbose(yellow("  done setting up sideEffects"), verbose = verbose)
  }

}


#' @export
#' @rdname setup
#'
#' @details
#' `setupOptions` can handle sequentially specified values, meaning a user can
#' first create a list of default options, then a list of user-desired options that
#' may or may not replace individual values. This can create hierarchies, *based on
#' order*.
#'
#' @return
#' `setupOptions` is run for its side effects, namely, changes to the `options()`. The
#'   list of modified options will be added as an attribute (`attr(out, "projectOptions")`),
#'   e.g., so they can be "unset" by user later.
#'
#'
#' @importFrom data.table data.table
setupOptions <- function(name, options, paths, times, overwrite = FALSE, envir = environment(),
                         verbose = getOption("Require.verbose", 1L), dots, defaultDots, ...) {

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  newValuesComplete <- oldValuesComplete <- NULL
  if (!missing(options)) {

    messageVerbose(yellow("setting up options..."), verbose = verbose)

    preOptions <- options()

    optionsSUB <- substitute(options) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    options <- evalSUB(optionsSUB, valObjName = "options", envir = envir, envir2 = parent.frame())

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

#' @importFrom utils modifyList tail
parseListsSequentially <- function(files, namedList = TRUE, envir = parent.frame(),
                                   verbose = getOption("Require.verbose")) {
  envs <- list(envir) # means

  llOuter <- lapply(files, function(optFiles) {
    pp <- parse(optFiles)

    envs2 <- lapply(pp, function(p) {
      env <- new.env(parent = tail(envs, 1)[[1]])
      if (isUnevaluatedList(p) || isFALSE(namedList)) {
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
            messageVerbose(pkgs, " is missing; attempting to install it. ",
                           "\nIf this fails, please add it manually to the `packages` argument")
            Require::Install(pkgs)
            safe <<- FALSE
            invokeRestart("muffleMessage")
          }
        })
        if (length(ls(env)) == 0) # the previous line will evaluated assignments e.g., mode <- "development",
          # putting the object `mode` into the env; but if there is no assignment
          # then we need to put the object into the environment
          env$opt <- out
        envs <<- append(envs, list(env))

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
    } else {
      os <- as.list(tail(envs2, 1)[[1]])
    }
    os
  })

  if (isTRUE(namedList))
    os <- Reduce(modifyList, llOuter)
  else
    os <- tail(llOuter, 1)[[1]][[1]]
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
setupModules <- function(name, paths, modules, useGit = FALSE, overwrite = FALSE, envir = environment(),
                         verbose = getOption("Require.verbose", 1L), dots, defaultDots, ...) {
  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (missing(modules)) {
    modulesOrig <- character()
    packages <- list()
  } else {
    if (missing(paths)) {
      pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = paths$projectpath)`
      pathsSUB <- checkProjectPath(pathsSUB, name, envir = envir, envir2 = parent.frame())
      paths <- setupPaths(paths = pathsSUB)#, inProject = TRUE, standAlone = TRUE, libPaths,
    }

    messageVerbose(yellow("setting up modules"), verbose = verbose)

    modulesSUB <- substitute(modules) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    modules <- evalSUB(val = modulesSUB, valObjName = "modules", envir = envir, envir2 = parent.frame())
    exts <- tools::file_ext(modules)
    isRepo <- nzchar(exts) & exts %in% ".R"
    if (any(isRepo)) {
      messageVerbose("modules arg supplied as file(s); parsing ... ", verbose = verbose)
      modules <- parseFileLists(modules, paths[["projectPath"]], namedList = FALSE, overwrite = overwrite,
                                envir = envir, verbose = verbose)
    }

    anyfailed <- character()
    modulesOrig <- modules
    modulesOrigPkgName <- extractPkgName(modulesOrig)
    if (!useGit) {
      offlineMode <- getOption("Require.offlineMode")
      if (isTRUE(offlineMode)) {
        opt <- options(Require.offlineMode = FALSE)
        on.exit(try(options(opt), silent = TRUE))
      }
      out <- getModule(modules, paths[["modulePath"]], overwrite)
      if (isTRUE(offlineMode))
        options(opt)
      anyfailed <- out$failed
      modules <- anyfailed
    }

    if (isTRUE(useGit) || length(anyfailed)) {
      gitSplit <- splitGitRepo(modules)
      gitSplit <- Require::invertList(gitSplit)

      origDir <- getwd()
      on.exit({
        setwd(origDir)
      }
        )
      mapply(split = gitSplit, function(split) {
        modPath <- file.path(split$acct, split$repo)
        localPath <- file.path(paths[["modulePath"]], split$repo)
        if (!dir.exists(localPath)) {
          prev <- setwd(file.path(paths[["modulePath"]]))
          cmd <- paste0("git clone https://github.com/", modPath)
          system(cmd)
        } else {
          messageVerbose("module exists at ", modPath, "; not cloning", verbose = verbose)
        }
        reportBranch <- TRUE
        if (!grepl("master|main|HEAD", split$br)) {
          prev <- setwd(file.path(paths[["modulePath"]], split$repo))
          cmd <- "git rev-parse --abbrev-ref HEAD"
          # next line -- cd doesn't work on my windows; no idea why
          # cmd <- paste0("cd ", file.path(paths[["modulePath"]], split$repo), " && git rev-parse --abbrev-ref HEAD ")
          curBr <- system(cmd, intern = TRUE)
          if (!identical(split$br, curBr)) {
            prev <- setwd(file.path(paths[["modulePath"]], split$repo))
            cmd <- paste0("git checkout ", split$br)
            system(cmd)
            reportBranch <- FALSE
          }
        }
        if (reportBranch)
          messageVerbose("\b ... on ", split$br, " branch")
      })
    }

    modulePackages <- packagesInModules(modulePath = paths[["modulePath"]], modules = modulesOrigPkgName)
    modulesSimple <- Require::extractPkgName(modulesOrigPkgName)
    packages <- modulePackages[modulesSimple]
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
setupPackages <- function(packages, modulePackages, require, libPaths, setLinuxBinaryRepo = TRUE,
                          standAlone, envir = environment(), verbose, dots, defaultDots, ...) {

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (isTRUE(setLinuxBinaryRepo))
    Require::setLinuxBinaryRepo()

  if (missing(packages)) {
    packages <- NULL
  }

  if (length(packages) || length(unlist(modulePackages)) || length(require) ) {

    messageVerbose(yellow("setting up packages..."), verbose = verbose)
    messageVerbose("Installing any missing reqdPkgs", verbose = verbose)
    continue <- 3L
    while (continue) {
      mp <- unname(unlist(modulePackages))
      # requireToTry <- unique(c(mp, require))
      packagesToTry <- unique(c(packages, mp, require))
      requirePkgNames <- Require::extractPkgName(require)
      # packagesToTry <- unique(c(packages, mp, requireToTry))
      # NOTHING SHOULD LOAD HERE; ONLY THE BARE MINIMUM REQUESTED BY USER
      out <- try(
        Require::Require(packagesToTry, require = requirePkgNames, # require = Require::extractPkgName(requireToTry),
                         standAlone = standAlone,
                         libPaths = libPaths,
                         verbose = verbose)
      )
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
    messageVerbose(yellow("  done setting up packages"), verbose = verbose)
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
setupParams <- function(name, params, paths, modules, times, options, overwrite = FALSE, envir = environment(),
                        verbose = getOption("Require.verbose", 1L), dots, defaultDots,
                        ...) {

  dotsSUB <- as.list(substitute(list(...)))[-1]
  dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

  if (missing(params)) {
    params <- list()
  } else {

    messageVerbose(yellow("setting up params..."), verbose = verbose)

    paramsSUB <- substitute(params) # must do this in case the user passes e.g., `list(fireStart = times$start)`
    params <- evalSUB(val = paramsSUB, valObjName = "params", envir = envir, envir2 = parent.frame())
    params <- parseFileLists(params, paths[["projectPath"]], overwrite = overwrite,
                             envir = envir, verbose = verbose)

    if (length(params)) {

      modulesSimple <- Require::extractPkgName(modules)

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
      messageVerbose(yellow("  done setting up params"), verbose = verbose)
    }
  }
  return(params)
}


parseFileLists <- function(obj, projectPath, namedList = TRUE, overwrite = FALSE, envir,
                           verbose = getOption("Require.verbose", 1L), dots, ...) {
  if (is(obj, "list")) {
    nams <- names(obj)
    named <- nzchar(nams)
    notNamed <- which(!named)
    if (length(notNamed)) {
      if (any(named))
        namedElements <- obj[which(named)]
      obj <- Map(objInner = obj[notNamed],
                 function(objInner)
                   parseFileLists(objInner, projectPath, namedList, overwrite,
                                  envir, verbose, dots, ...))
      obj <- Reduce(f = append, obj)
      if (any(named))
        obj <- append(namedElements, obj)
    }
  }

  if (is.character(obj)) {
    obj <- mapply(opt = obj, function(opt) {
      isGH <- isGitHub(opt) && grepl("@", opt) # the default isGitHub allows no branch
      if (isGH) {
        opt <- getGithubFile(opt, destDir = projectPath, overwrite = overwrite)
      } else {
        if (!file.exists(opt))
          messageVerbose(opt, paste(" has no @ specified, so assuming a local file,",
                                    "but local file does not exist"), verbose = verbose)
      }
      opt
    }, SIMPLIFY = TRUE)
    areAbs <- isAbsolutePath(obj)
    if (any(areAbs %in% FALSE)) {
      obj[areAbs %in% FALSE] <- file.path(projectPath, obj[areAbs %in% FALSE])
    }

    obj <- parseListsSequentially(files = obj, namedList = namedList, envir = envir,
                                  verbose = verbose)
  }

  return(obj)
}

checkProjectPath <- function(paths, name, envir, envir2) {

  if (missing(paths)) {
    paths <- list()
  }
  if (is.name(paths) || is.call(paths)) {
    paths <- evalSUB(paths, valObjName = "paths", envir = envir, envir2 = envir2)
  }
  if (is.null(paths[["projectPath"]])) {
    prjPth <- if (missing(name))
      normPath(".")
    else {
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
    out <- identical(basename(gtwd), extractPkgName(name))
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
  val2 <- val
  while (inherits(val, "call") || inherits(val, "name")) {
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

  gitIgnoreFile <- ".gitignore"
  gitFile <- file.path(paths$projectPath, ".git")
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

    if (isTRUE(gitignore) && !file.exists(".gitmodules")) { # This is NOT using submodules; so, "it is a git repo, used git
      updatedMP <- TRUE
      lineWithModPath <- grep(paste0("^", basename(paths[["modulePath"]]),"$"), gif)
      insertLine <- if (length(lineWithModPath)) lineWithModPath[1] else length(gif) + 1
      gif[insertLine] <- file.path(basename(paths[["modulePath"]]), "*")

    }

    if (length(setdiff(gif, gifOrig))) {
      writeLines(con = gitIgnoreFile, unique(gif))
      mess <- paste(c("packagePath"[updatedPP], "modulePath"[updatedMP]), collapse = " and ")
      messageVerbose(verboseLevel = 1, verbose = verbose,
                     ".gitignore file updated with ", mess,"; ",
                     "this may need to be confirmed manually")
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
  dots <- Map(d = dots, nam = names(dots), # MoreArgs = list(defaultDots = defaultDots),
              function(d, nam) {
                d1 <- try(eval(d, envir = envir), silent = TRUE)
                if (is(d1, "try-error")) {
                  if (isTRUE(haveDefaults))
                    d1 <- defaultDots[[nam]]
                  else
                    d1 <- d
                }
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
#'
#' @details
#' `setupStudyArea` only uses `inputPath` within its `paths` argument, which will
#' be passed to `path` argument of `gadm`.
#'
#' @return
#' `setupStudyArea` will return an `sf` class object coming from `geodata::gadm`,
#' with subregion specification as described in the `studyArea` argument.fsu
setupStudyArea <- function(studyArea, paths) {

  if (missing(paths))
    paths <- list(inputPaths = ".")
  if (is(studyArea, "list")) {
    needRep <- !requireNamespace("reproducible", quietly = TRUE)
    needGeo <- !requireNamespace("geodata", quietly = TRUE)
    if (needRep || needGeo) {
      needs <- c("reproducible"[needRep], "geodata"[needGeo])
      message("Installing ", paste0(needs, collapse = " and "))
      Require::Install(needs)
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
    studyArea <- do.call(geodata::gadm, as.list(geodatCall[-1]))
    studyArea <- studyArea[grep(tolower(paste0("^", subregion)), tolower(studyArea$NAME_1)), ]
    if (!is.null(epsg))
      if (requireNamespace("terra")) {
         studyArea |> terra::project(epsg)
      } else {
        warning("Could not reproject; need ")
      }
  }
  studyArea
}


setupRestart <- function(updateRprofile, paths, name, inProject, Restart, verbose) {
  if (isTRUE(updateRprofile)) {
    inTmpProject <- inTempProject(paths)
    if (isTRUE(inTmpProject)) {
      warning("updateRprofile is TRUE, but the projectPath is the tempdir(), which means ",
              "the .Rprofile won't be read upon restart. ",
              "Change the paths$projectPath to a non-temporary path")
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

  if (!inProject) {
    if (interactive() && isTRUE(Restart)) # getOption("SpaDES.project.Restart", TRUE))
      if (requireNamespace("rstudioapi")) {
        messageVerbose("... restarting Rstudio inside the project",
                       verbose = verbose)
        rstudioapi::openProject(path = paths[["projectPath"]])
      } else {
        stop("Please open this in a new Rstudio project at ", paths[["projectPath"]])
      }
  }
}

setupSpaDES.ProjectDeps <- function(paths, deps = c("SpaDES.project", "data.table", "Require", "rprojroot"),
                                    verbose = getOption("Require.verbose")) {

  depsAlreadyInstalled <- dir(paths[["packagePath"]], pattern = paste0(paste0("^", deps, "$"), collapse = "|"))
  diffVersion <- Map(dai = depsAlreadyInstalled, function(dai) {
    if (file.exists(file.path(paths[["packagePath"]], dai, "DESCRIPTION"))) {
      pvLoaded <- packageVersion(dai)
      pvLibLoc <- packageVersion(dai, lib.loc = .libPaths()[1])
      pvPathsPackagePath <- packageVersion(dai, lib.loc = paths[["packagePath"]])
      loadedFrom <- if (identical(pvPathsPackagePath, pvLoaded)) {
        "packagePath"
      } else {
        "libPaths"
      }

      if (pvLibLoc < pvPathsPackagePath) {# test whether lib loc is lt; so, need to unload; then move from to packagePath; reload
        out <- dai
      } else if (pvLibLoc > pvPathsPackagePath) { #test whether lib loc is gt; so, need to move to packagePath
        out <- list(Package = dai, pvLibLoc, pvPathsPackagePath)
      } else {
        out <- NULL
      }
    } else {
      out <- NULL
    }
    out
  })
  # First check for loaded old version; needs user intervention
  needStop <- vapply(diffVersion, function(x) is.character(x), FUN.VALUE = logical(1))
  if (any(needStop)) {
    pkgsToUpdate <- names(needStop)[needStop]
    pkgs <- paste(pkgsToUpdate, collapse = "', '")

    stop("\nThe version of ", pkgs, " need updating in the personal library.\n",
         "Please restart R and update ", pkgs, ", e.g., using: ",
         "\ninstall.packages(c('", pkgs, "'), lib = '", .libPaths()[1],"')")
  }
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
    messageVerbose("Copying ", paste(deps, collapse = ", "), " packages to paths$packagePath (",
                   paths$packagePath, ")", verbose = verbose)

    if (!identical(normPath(.libPaths()[1]), paths[["packagePath"]]) &&
        (!identical(dirname(normPath(.libPaths()[1])), paths[["packagePath"]])))
      for (pkg in deps) {
        pkgDir <- file.path(.libPaths(), pkg)
        de <- dir.exists(pkgDir)
        pkgDir <- pkgDir[de][1]
        files1 <- dir(pkgDir, all.files = TRUE, recursive = TRUE)
        newFiles <- file.path(paths[["packagePath"]], pkg, files1)
        lapply(unique(dirname(newFiles)), dir.create, recursive = TRUE, showWarnings = FALSE)
        oldFiles <- file.path(pkgDir, files1)

        file.copy(oldFiles, newFiles, overwrite = TRUE)
      }
  }
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
