## experiment2(), ported from the now-unmaintained SpaDES.experiment package.
## Runtime dependencies SpaDES.core, reproducible, future, future.apply and
## googledrive are in Suggests; they are required at call time and used via `::`.

if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("simName", "objectName", "saveTime"))
}

################################################################################
#' Run experiment, algorithm 2, using [SpaDES.core::spades()]
#'
#' Given one or more `simList` objects, run a series of `spades` calls
#' in a structured, organized way. Methods are available to deal with outputs,
#' such as [as.data.table.simLists()] which can pull out simple to complex
#' values from every resulting `simList` or object saved by `outputs`
#' in every `simList` run. This uses `future` internally, allowing
#' for various backends and parallelism.
#'
#' This function was moved here from the now-unmaintained `SpaDES.experiment`
#' package. See also the file-queue based [experiment_family] (e.g.
#' [experimentFuture()]) for a different, script-oriented approach.
#'
#' @param ... One or more `simList` objects. Additional named arguments are
#'   passed through to [SpaDES.core::spades()] (see `Controlling events` below).
#' @param replicates The number of replicates to run of the same `simList`.
#'                   See details and examples. To minimize memory overhead, currently,
#'                   this must be length 1, i.e., all `...` simList objects will
#'                   receive the same number of replicates.
#' @param clearSimEnv Logical. If TRUE, then the envir(sim) of each simList in the return list
#'                    is emptied. This is to reduce RAM load of large return object.
#'                    Default FALSE.
#' @param createUniquePaths A character vector of the `paths` passed to `simInit`,
#'   indicating which should create a new, unique path, as a sub-path to the original
#'   `paths`. Currently only `"outputPath"` is honoured. Pass `character(0)` (or
#'   `NULL`) to disable this nesting entirely, e.g. when the caller (such as
#'   [experiment()]) has already set each `simList`'s `outputPath`.
#' @param useCache Logical. Passed to `spades`. This will be passed with the `simList`
#'   name and replicate number, allowing each replicate and each `simList` to be
#'   seen as a non-cached call to `spades`.
#' @param debug Passed to [SpaDES.core::spades()].
#' @param drive_auth_account Optional character string. If provided, it will be passed
#'    to each worker and run as `googledrive::drive_auth(drive_auth_account)` to allow
#'    a specific user account for googledrive
#' @param meanStaggerIntervalInSecs If used, this will use
#'   `Sys.sleep(cumsum(c(0, rnorm(nbrOfWorkers() - 1, mean = meanStaggerIntervalInSecs,`
#'   `sd = meanStaggerIntervalInSecs/10))))` and distribute these delays to the workers.
#'
#' @details
#'
#' This function, because of its class formalism, allows for methods to be used. For example,
#' [as.data.table.simLists()] allows user to pull out specific objects (in
#' the `simList` objects or on disk saved in `outputPath(sim)`).
#'
#' The `outputPath` is changed so that every simulation puts outputs in a
#' sub-directory of the original `outputPath` of each `simList` (unless
#' `createUniquePaths` is `character(0)`/`NULL`).
#'
#' @section Controlling events:
#'
#' Any named argument in `...` that is not consumed by `experiment2` is passed
#' straight to [SpaDES.core::spades()]. In particular, `spades()`'s `events`
#' argument is honoured, so
#' `experiment2(sim1, sim2, events = list(...))` restricts the events that run
#' for every simulation. Note this applies the *same* `events` specification to
#' all `simList`s / replicates. For per-scenario control of events, use the
#' file-queue [experiment_family] with an `events` column in `df` (see
#' [experiment_family]).
#'
#' @note
#' A `simLists` object can be made manually, if, say, many manual `spades` calls
#' have already been run. See example, via `new("simLists")`
#'
#' @return Invisibly returns a `simLists` object. This class
#' extends the `environment` class and contains `simList` objects.
#'
#' @seealso [as.data.table.simLists()], [SpaDES.core::spades()], [experiment()],
#'   [experiment_family]
#'
#' @author Eliot McIntire
#' @export
#' @importFrom stats rnorm
#' @include simLists-class.R
#' @rdname experiment2
#'
#' @example inst/examples/example_experiment2.R
#'
## NOTE: experiment2() is a plain function (not an S4 method) because its only
## ever dispatched signature was `simList`, a class that lives in SpaDES.core --
## which is in Suggests here. Defining an S4 method on a Suggests-only class
## would make the package fail to load when SpaDES.core is absent.
experiment2 <- function(..., replicates = 1, clearSimEnv = FALSE,
                        createUniquePaths = c("outputPath"), useCache = FALSE,
                        debug = getOption("spades.debug"),
                        drive_auth_account = NULL, meanStaggerIntervalInSecs = 1) {
    .experiment2RequireDeps()
    # `...` carries the simList objects (named or not) plus any extra named
    # arguments destined for SpaDES.core::spades() (e.g. `events`,
    # `.plotInitialTime`, `cache`). Split them by class, not by name, so that
    # `experiment2(sim1 = ..., sim2 = ...)` still works.
    .dots <- list(...)
    isSim <- vapply(.dots, function(x) is(x, "simList"), logical(1))
    ll <- .dots[isSim]            # the simLists (keep any user-supplied names)
    spadesArgs <- .dots[!isSim]   # forwarded to spades()
    if (length(ll) == 0)
      stop("experiment2() needs one or more `simList` objects", call. = FALSE)

    # determine packages to load in the workers
    if (length(createUniquePaths) && any(createUniquePaths != "outputPath")) {
      message("createUniquePaths only accepts outputPath, currently",
              ". Setting it to 'outputPath'")
      createUniquePaths <- "outputPath"
    }
    pkg <- c(unique(unlist(lapply(ll, SpaDES.core::packages, clean = TRUE))),
             "SpaDES.project", "googledrive")
    outSimLists <- new("simLists")
    possSimNames <- as.character(seq_along(ll))

    ll <- updateNames(ll, possSimNames)
    simNames <- names(ll)

    if (length(replicates) != 1) {
      stop("replicates argument must be length 1")
    }

    simNames <- rep(simNames, times = replicates) # keep them alternating for mapply
    repNums <- unlist(lapply(replicates, seq_len))
    repNums <- rep(repNums, each = length(ll))
    namsExpanded <- paste(simNames,
                          SpaDES.core::paddedFloatToChar(repNums, padL = max(nchar(repNums))),
                          sep = "_rep")
    names(simNames) <- namsExpanded

    # do copy of sim inside workers, so there is only 1 copy per worker,
    # rather than 1 copy per sim
    names(namsExpanded) <- namsExpanded

    staggersInSecs <- cumsum(c(0, rnorm(length(future::nbrOfWorkers()) - 1,
                                        mean = meanStaggerIntervalInSecs,
                                        sd = meanStaggerIntervalInSecs / 10)))

    allOpts <- options()
    out <- future.apply::future_mapply(
      name = namsExpanded,
      simName = simNames,
      staggerInSecs = staggersInSecs,
      sim = ll,  # recycled by replicates -- maybe this reduces copying ...?
      MoreArgs = list(clearSimEnv = clearSimEnv,
                      createUniquePaths = createUniquePaths,
                      useCache = useCache,
                      .spades = .spades,
                      allOpts = allOpts,
                      debug = debug,
                      drive_auth_account = drive_auth_account,
                      spadesArgs = spadesArgs),
      FUN = experiment2Inner,
      SIMPLIFY = FALSE,
      future.packages = pkg,
      future.seed = TRUE
    )
    names(out) <- namsExpanded
    list2env(out, envir = outSimLists@.xData)
    return(outSimLists)
}

#' @keywords internal
#' @noRd
.experiment2RequireDeps <- function() {
  needed <- c("SpaDES.core", "reproducible", "future", "future.apply")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing))
    stop("experiment2() requires the following package(s), which are in ",
         "SpaDES.project's Suggests: ", paste(missing, collapse = ", "),
         ". Install with: install.packages(c(",
         paste(sprintf("'%s'", missing), collapse = ", "), "))", call. = FALSE)
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
experiment2Inner <- function(sim, clearSimEnv, staggerInSecs, createUniquePaths,
                             simName, name, useCache = FALSE, allOpts,
                             debug = getOption("spades.debug"), drive_auth_account,
                             spadesArgs = list(), ...) {
  message(paste0("Sleeping ", round(staggerInSecs, 1), " seconds"))
  Sys.sleep(staggerInSecs)
  if (length(createUniquePaths) && "outputPath" %in% createUniquePaths) {
    SpaDES.core::outputPath(sim) <-
      reproducible::checkPath(file.path(SpaDES.core::outputPath(sim), name), create = TRUE)
  }
  if (!is.null(drive_auth_account))
    googledrive::drive_auth(drive_auth_account)

  options(allOpts)
  # spadesArgs (named `...` from experiment2, e.g. `events`) are forwarded to spades()
  s <- do.call(reproducible::Cache,
               c(list(.spades, sim, useCache = useCache, simName,
                      debug = debug, clearSimEnv = clearSimEnv),
                 spadesArgs,
                 list(omitArgs = "debug")))
  s
}

#' @keywords internal
#' @noRd
.spades <- function(sim, debug = getOption("spades.debug"),
                    clearSimEnv = FALSE, ...) {
  # don't make a copy if it is callr or multisession because future will make the copy
  if (!any(c("callr", "multisession") %in% attr(future::plan(), "class"))) {
    a <- Sys.time()
    message("Copying simList prior to spades call")
    sim <- reproducible::Copy(sim, filebackedDir = file.path(SpaDES.core::outputPath(sim), "rasterFiles"))
    b <- Sys.time()
    message(format(b - a), " to Copy")
  }

  s <- SpaDES.core::spades(sim, debug = debug, ...)
  if (isTRUE(clearSimEnv))
    rm(list = ls(s, all.names = TRUE), envir = SpaDES.core::envir(s))
  return(s)
}
