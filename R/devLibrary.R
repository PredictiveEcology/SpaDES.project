#' Per-job package libraries that an install never touches
#'
#' @description
#' The project library (`.libPathDefault(name)`, e.g. `.../packages/<platform>/4.6`)
#' is the frozen *stable* library: nothing installs into it. An update instead
#' builds a new *dev* library beside it, in `<stable>_dev/<id>`, and each job is
#' given the pair as its library path. The `experiment*` launchers do this:
#' `experimentTmux(devPackages = )`, `experimentSBATCH(devPackages = )` and
#' `experimentFuture(devPackages = )` call `newDevLibrary()` once, before any worker
#' starts, and start every worker with `options(spades.packagePath = c(<dev>, <stable>),
#' SpaDES.project.updateRprofile = FALSE)` and `.libPaths(c(<dev>, <stable>))`, so the job's
#' own `setupProject()` needs no `packagePath`. A running job's libraries are therefore
#' never written to.
#'
#' * `newDevLibrary()` makes `<devRoot>/<id>` (`id` is the UTC time,
#'   `yyyymmdd-HHMMSS`, and a short random suffix), hardlinks every package of the
#'   current dev library into it, and installs `packages` there, in a fresh R
#'   process whose `.libPaths()` is `c(<new>, stable)`, so what `stable` or the
#'   current dev library already satisfies is not installed again. An install
#'   replaces a package's folder, so the previous dev library's files (same
#'   inodes) are left as they were. If nothing was installed, the new directory
#'   is removed and the current one stays current; otherwise `<devRoot>/current`
#'   is updated (written to a temporary file, then renamed).
#' * `currentDevLibrary()` gives the library path a job should use now.
#' * `sweepDevLibraries()` removes dev libraries no job needs any more.
#'
#' @param packages Character vector of package specifications as accepted by
#'   `Require::Install()` (and `setupProject(packages = )`), e.g.
#'   `"PredictiveEcology/reproducible@development (>= 3.2.1.9010)"`.
#' @param stable The stable library. The default is `.libPaths()[1]`, or the
#'   library a dev library at `.libPaths()[1]` was made from.
#' @param devRoot The directory holding the dev libraries and the `current` file.
#' @param installFun Function of `(packages, libPath)` performing the install
#'   into `libPath`, run with `.libPaths()` set to `c(libPath, stable)`. The
#'   default calls `Require::Install(packages, libPaths = libPath, standAlone = FALSE)`.
#'   Exposed for tests.
#' @param keep Dev libraries to keep: those that pending or running jobs name.
#'   `c(<dev>, <stable>)` pairs may be given as they are.
#' @param verbose Numeric.
#'
#' @return `newDevLibrary()` and `currentDevLibrary()` return `c(<dev>, stable)`,
#'   or `stable` alone when there is no dev library yet. `sweepDevLibraries()` returns
#'   the directories it removed, invisibly.
#'
#' @examples
#' \dontrun{
#' # before queueing jobs (no job reads a library this writes):
#' libs <- newDevLibrary(c("PredictiveEcology/reproducible@development (>= 3.2.1.9063)"))
#' # or let the launcher do it, and start its workers on the result:
#' experimentTmux(df, devPackages = "PredictiveEcology/reproducible@development (>= 3.2.1.9063)")
#' # later, keeping the libraries that pending and running jobs name:
#' sweepDevLibraries(keep = libs)
#' }
#' @name devLibrary
#' @rdname devLibrary
NULL

## A dev library carries this marker; its one line is the stable library it sits beside.
.devLibraryMarker <- ".devLibrary_of"

.isDevLibrary <- function(libPath)
  length(libPath) == 1L && !is.na(libPath) && file.exists(file.path(libPath, .devLibraryMarker))

## `.libPaths()[1]`, or the stable library of the dev library found there
.stableLibrary <- function(libPath = .libPaths()[1]) {
  if (.isDevLibrary(libPath)) readLines(file.path(libPath, .devLibraryMarker), warn = FALSE)[1]
  else libPath
}

.devLibraryId <- function()
  paste0(format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC"), "-", .rndstr(1, 6))

.packageDirs <- function(lib) {
  dirs <- list.dirs(lib, recursive = FALSE)
  dirs[file.exists(file.path(dirs, "DESCRIPTION"))]
}

## What an install changes: "<package> <md5 of its DESCRIPTION>" for every package (an install
## rewrites DESCRIPTION, with its `Built:` time)
.libraryState <- function(lib) {
  pkgDirs <- .packageDirs(lib)
  paste(basename(pkgDirs), unname(tools::md5sum(file.path(pkgDirs, "DESCRIPTION"))))
}

#' @export
#' @rdname devLibrary
newDevLibrary <- function(packages, stable = .stableLibrary(), devRoot = paste0(stable, "_dev"),
                          installFun = NULL, verbose = getOption("Require.verbose", 1)) {
  stable <- normalizePath(stable, mustWork = TRUE)
  dir.create(devRoot, recursive = TRUE, showWarnings = FALSE)
  devRoot <- normalizePath(devRoot, mustWork = TRUE)
  current <- currentDevLibrary(stable, devRoot)
  new <- file.path(devRoot, .devLibraryId())
  dir.create(new)
  done <- FALSE
  on.exit(if (!done) unlink(new, recursive = TRUE), add = TRUE)

  if (length(current) > 1L) {
    pkgDirs <- .packageDirs(current[1])
    if (length(pkgDirs)) {
      staged <- unlist(linkOrCopyFiles(fromDirs = pkgDirs, toBaseDir = new)$toFilesList)
      if (!all(file.exists(staged)))
        stop("newDevLibrary: could not link or copy ", sum(!file.exists(staged)), " file(s) of ",
             current[1], " into ", new, call. = FALSE)
    }
  }
  writeLines(stable, file.path(new, .devLibraryMarker))
  before <- .libraryState(new)

  if (is.null(installFun)) {
    installFun <- function(packages, libPath) Require::Install(packages, libPaths = libPath, standAlone = FALSE)
    environment(installFun) <- globalenv() # sent to the other process; nothing of this frame is needed there
  }
  # A fresh process: this session may have the packages loaded. Require is loaded before
  # .libPaths() is narrowed, so it may come from any library; what the install can see as
  # already installed is only the new library and stable.
  callr::r(function(installFun, packages, new, stable) {
    requireNamespace("Require", quietly = TRUE)
    .libPaths(c(new, stable))
    installFun(packages, new)
  }, args = list(installFun = installFun, packages = packages, new = new, stable = stable),
  libpath = unique(c(new, stable, .libPaths())), show = isTRUE(verbose > 0))

  # Installed: what is new or changed, and not just the stable library's own copy (Require links
  # pak from stable into the first library of the path when it is not there)
  installed <- setdiff(setdiff(.libraryState(new), before), .libraryState(stable))
  if (!length(installed)) {
    messageVerbose("newDevLibrary: nothing to install; the current library stays: ",
                   current[1], verbose = verbose)
    return(current)
  }
  tmp <- file.path(devRoot, paste0(".current_", basename(new)))
  writeLines(new, tmp)
  file.rename(tmp, file.path(devRoot, "current"))
  done <- TRUE
  messageVerbose("newDevLibrary: ", new, " is now the current dev library (installed: ",
                 paste(sub(" .*", "", installed), collapse = ", "), ")", verbose = verbose)
  c(new, stable)
}

#' @export
#' @rdname devLibrary
currentDevLibrary <- function(stable = .stableLibrary(), devRoot = paste0(stable, "_dev")) {
  f <- file.path(devRoot, "current")
  dev <- if (file.exists(f)) readLines(f, warn = FALSE)[1] else NA_character_
  if (is.na(dev) || !dir.exists(dev)) return(stable)
  c(dev, stable)
}

#' @export
#' @rdname devLibrary
sweepDevLibraries <- function(keep = character(), devRoot = paste0(.stableLibrary(), "_dev")) {
  if (!dir.exists(devRoot)) return(invisible(character()))
  devs <- list.dirs(devRoot, recursive = FALSE)
  devs <- devs[file.exists(file.path(devs, .devLibraryMarker))]
  f <- file.path(devRoot, "current")
  keep <- c(keep, if (file.exists(f)) readLines(f, warn = FALSE)[1])
  gone <- devs[!normalizePath(devs) %in% normalizePath(keep, mustWork = FALSE)]
  unlink(gone, recursive = TRUE)
  invisible(gone)
}

#' The dev library of an `experiment*` launcher
#'
#' Builds the dev library (see [newDevLibrary()]) beside the project library in use, once, before any
#' worker starts. Returns `c(<dev>, <stable>)`, or `NULL` when there is none (`devPackages` is `NULL`,
#' or nothing is installed yet): the workers then start as they always did.
#'
#' @param devPackages `NULL` (default: no dev library) or a character vector of package
#'   specifications as for `newDevLibrary(packages = )`. If given, the launcher builds a new dev
#'   library with them, in the project library's `<stable>_dev` folder, before any worker starts. Every
#'   worker then starts with `options(spades.packagePath = c(<dev>, <stable>),
#'   SpaDES.project.updateRprofile = FALSE)` and `.libPaths(c(<dev>, <stable>))`, so the job's own
#'   `setupProject()` needs no `packagePath`, installs nothing and leaves the `.Rprofile` alone. No
#'   library snapshot is taken (`snapshot_library` is ignored): the libraries are not written to.
#' @param devInstallFun `NULL` (default) or the `installFun` of [newDevLibrary()].
#' @param verbose Numeric.
#' @keywords internal
.workerDevLibrary <- function(devPackages, devInstallFun = NULL, verbose = getOption("Require.verbose", 1)) {
  if (is.null(devPackages)) return(NULL)
  libs <- newDevLibrary(devPackages, installFun = devInstallFun, verbose = verbose)
  if (length(libs) > 1L) libs else NULL
}

## What a worker with a dev library runs first. The existing options do the rest: spades.packagePath is
## what setupProject() takes for `paths$packagePath`; SpaDES.project.updateRprofile = FALSE keeps the
## project's .Rprofile, which every job shares, as it is.
.devLibraryStartup <- function(dev_library) {
  if (length(dev_library) < 2L) return("")
  libs <- paste(deparse(dev_library), collapse = " ")
  sprintf("options(spades.packagePath = %s, SpaDES.project.updateRprofile = FALSE); .libPaths(%s); ", libs, libs)
}
