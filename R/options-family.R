#' Families of options for `setupProject()`
#'
#' Named lists of [options()] that a project script passes to
#' `setupProject(options = )`, so the knowledge of which settings suit a kind of
#' run lives in this package, not in each script:
#'
#' ```
#' setupProject(
#'   options = c(SpaDES.project::optionsCache(), SpaDES.project::optionsBatch(),
#'               list(spades.cacheChaining = TRUE)),
#'   ...)
#' ```
#'
#' A later element wins over an earlier one.
#'
#' * `optionsFast()` is what `setupProject()` sets when
#'   `options(SpaDES.project.fast = TRUE)`: quick start-up, no package or git
#'   checks. (`fastOptions()` is the earlier, unexported name of this function and
#'   still works; it is deprecated.)
#' * `optionsBatch()` is for an unattended run (a queue worker, an Rscript, a
#'   Slurm job), where nobody reads the console or looks at the cache. When
#'   `batch` is `FALSE` it returns `list()`, so a person debugging keeps the
#'   defaults. `interactive()` is `TRUE` in a tmux pane even when the run is
#'   unattended, so the default of `batch` is `FALSE` only for an interactive
#'   session outside tmux (environment variable `TMUX` empty).
#'   * `reproducible.showSimilar = FALSE`: on a cache miss, `showSimilar` scans
#'     the whole cache to report what differs from similar entries, which takes
#'     minutes per miss and is read by nobody in a batch run.
#'   * `reproducible.useMemoise = FALSE`: with memoising, every object read from
#'     the cache is also kept in the session's memory. A queue worker rarely reads
#'     the same entry twice, and the memory limits how many workers a machine holds.
#'   * `reproducible.objSize = FALSE`: do not measure objects' sizes for the cache
#'     database (up to 10% faster caching).
#'   * `spades.recoveryMode = 0`: do not snapshot objects before each event, so an
#'     error or interruption cannot be recovered from.
#'   * `spades.allowInitDuringSimInit = TRUE`: let `.inputObjects` use objects that
#'     another module creates in `init`.
#' * `optionsCache()` is the cache settings that the PredictiveEcology batch
#'   projects share: cache files in the `qs2` format, shapefiles read with
#'   `terra::vect`, `prepInputs()` overwriting its output file, no partial reads of
#'   web GeoTIFFs (`reproducible.useCOG`), memoised copies kept when `reproducible`
#'   reloads, and `showSimilar` reporting differences in nested lists down to depth 8
#'   (default 3). A script that reads the cache of another run must apply the same
#'   settings, above all `reproducible.cacheSaveFormat`: a cache written as `qs2`
#'   is not found by a session that looks for `rds` files.
#'
#' @param batch Logical. `TRUE` for an unattended run. The default is `FALSE` only
#'   in an interactive session outside tmux.
#'
#' @return A named list, to give to `setupProject(options = )` or [options()].
#'   `optionsBatch(batch = FALSE)` returns `list()`.
#' @rdname optionsFast
#' @export
#' @examples
#' optionsCache()
#' optionsBatch(batch = TRUE)
#' names(optionsFast())
optionsFast <- function() {
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
       reproducible.cacheSaveFormat = "qs2",
       LandR.assertions = FALSE,
       reproducible.cacheSpeed = "fast",
       reproducible.gdalwarp = TRUE,
       spades.recoveryMode = 1
  )
}

fastOptions <- function() optionsFast()

#' @rdname optionsFast
#' @export
optionsBatch <- function(batch = !(interactive() && !nzchar(Sys.getenv("TMUX")))) {
  if (!isTRUE(batch))
    return(list())
  list(reproducible.showSimilar = FALSE,
       reproducible.useMemoise = FALSE,
       reproducible.objSize = FALSE,
       spades.recoveryMode = 0,
       spades.allowInitDuringSimInit = TRUE)
}

#' @rdname optionsFast
#' @export
optionsCache <- function() {
  list(reproducible.cacheSaveFormat = "qs2",
       reproducible.shapefileRead = "terra::vect",
       reproducible.overwrite = TRUE,
       reproducible.useCOG = FALSE,
       reproducible.memoisePersist = TRUE,
       reproducible.showSimilarDepth = 8)
}
