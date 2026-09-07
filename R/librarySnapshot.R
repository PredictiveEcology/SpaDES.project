#' Keep a shared project library safe under concurrent experiment workers
#'
#' @description
#' Every experiment backend (`experimentTmux()`, `experimentFuture()`,
#' `experimentSBATCH()`) runs many jobs against one project library. Nothing
#' stops that library from being modified while jobs use it, and R lazy-loads
#' package objects from `R/<pkg>.rdb` by file offset, so replacing a package
#' under a running session ends in
#' `lazy-load database '.../R/<pkg>.rdb' is corrupt`. Two functions make the
#' library safe:
#'
#' * `syncProjectLibrary()` installs or updates packages **once per launch**, in
#'   a fresh R process (this session may itself have them loaded), and refuses
#'   to run while any worker is alive. The backends call it when given
#'   `sync_library = <package specs>`.
#' * `snapshotLibrary()` gives **one job** its own copy of the library, made of
#'   hardlinks (`file.link()`, so it costs seconds and no disk), falling back to
#'   `file.copy()` across filesystems. A hardlink keeps the old inode alive: an
#'   install into the shared library replaces files *there* and cannot touch
#'   what the running job has open. Every job thereby also records exactly which
#'   package versions it ran with. The backends take the snapshot in the worker
#'   startup script, before any package is loaded, when `snapshot_library = TRUE`
#'   (the default).
#'
#' `libraryInUse()` reports the workers that make a sync unsafe;
#' `releaseLibrarySnapshot()` removes one snapshot (the worker loop does this at
#' exit) and `sweepLibrarySnapshots()` removes those whose owning process is gone.
#'
#' @param packages Character vector of package specifications as accepted by
#'   `Require::Install()`, e.g.
#'   `"PredictiveEcology/reproducible@development (>= 3.2.1.9010)"`. Usually
#'   the vector given to `setupProject(packages = )`, so the version floors
#'   declared there are the single source of truth.
#' @param libPath The shared library. Defaults to `.libPaths()[1]`.
#' @param activeRunningPath Where the experiment keeps its `Running_*` sentinels;
#'   snapshots are created there as `lib_<id>/` unless `where` says otherwise.
#' @param requireIdle Logical. Refuse to install while `libraryInUse()` finds a
#'   worker. Set to `FALSE` only when you know the workers are idle.
#' @param installFun Function of `(packages, libPath)` performing the install;
#'   defaults to `Require::Install()`. Exposed for tests.
#' @param where Directory holding snapshots; defaults to `activeRunningPath`.
#' @param id Snapshot id; defaults to `<nodename>_<pid>` so
#'   `sweepLibrarySnapshots()` can tell a live owner from a dead one.
#' @param setLibPaths Logical. Put the snapshot first on `.libPaths()` in place
#'   of `libPath`, and record it in `SPADES_PROJECT_LIB_SNAPSHOT` so the worker
#'   loop can release it at exit.
#' @param snapshot A directory returned by `snapshotLibrary()`.
#' @param verbose Numeric.
#'
#' @return `syncProjectLibrary()` returns `TRUE` invisibly after installing,
#'   `FALSE` when `packages` is empty. `libraryInUse()` returns a character
#'   vector describing live workers (sentinel files and `ps` lines), empty when
#'   none. `snapshotLibrary()` returns the snapshot directory.
#'   `releaseLibrarySnapshot()` returns `TRUE` invisibly if it removed one.
#'   `sweepLibrarySnapshots()` returns the directories it removed.
#'
#' @examples
#' \dontrun{
#' # before launching an experiment, with no worker running:
#' syncProjectLibrary(c("PredictiveEcology/reproducible@development (>= 3.2.1.9010)",
#'                      "PredictiveEcology/SpaDES.core@development"))
#' # inside a worker, before loading anything from the library:
#' snapshotLibrary(where = "logs/my_queue.rds")
#' }
#' @name librarySnapshot
#' @rdname librarySnapshot
NULL

#' @export
#' @rdname librarySnapshot
syncProjectLibrary <- function(packages, libPath = .libPaths()[1],
                               activeRunningPath = getOption("spades.activeRunningPath"),
                               requireIdle = TRUE, installFun = NULL,
                               verbose = getOption("Require.verbose", 1)) {
  if (is.null(packages) || !length(packages)) return(invisible(FALSE))
  if (isTRUE(requireIdle)) {
    busy <- libraryInUse(activeRunningPath)
    if (length(busy))
      stop("Refusing to install into ", libPath, " while workers may have it loaded ",
           "(replacing a package under a running R session corrupts its lazy-load database):\n  ",
           paste(busy, collapse = "\n  "),
           "\nStop them first, or call with requireIdle = FALSE if they are known to be idle.",
           call. = FALSE)
  }
  if (is.null(installFun))
    installFun <- function(packages, libPath) Require::Install(packages, libPaths = libPath)
  libPath <- normalizePath(libPath, mustWork = TRUE)
  # A fresh process: this session may itself have the packages loaded.
  callr::r(function(installFun, packages, libPath) {
    .libPaths(c(libPath, .libPaths()))
    installFun(packages, libPath)
  }, args = list(installFun = installFun, packages = packages, libPath = libPath),
  libpath = unique(c(libPath, .libPaths())), show = isTRUE(verbose > 0))
  invisible(TRUE)
}

#' @export
#' @rdname librarySnapshot
libraryInUse <- function(activeRunningPath = getOption("spades.activeRunningPath")) {
  live <- character()
  if (!is.null(activeRunningPath) && dir.exists(activeRunningPath)) {
    sent <- list.files(activeRunningPath, pattern = "^Running_.*_[0-9]+_\\.rds$", full.names = TRUE)
    if (length(sent)) {
      pids <- suppressWarnings(as.integer(sub(".*_([0-9]+)_\\.rds$", "\\1", basename(sent))))
      live <- c(live, sent[.pidsAlive(pids)])
    }
  }
  c(live, .workerProcesses())
}

# Worker R processes on this machine, other than the caller. Two signals, because
# neither alone covers every backend:
#   * the command line: callr/sbatch workers run `Rscript -e "... tmuxRunWorkerLoop(...)"`;
#   * the environment (Linux, own processes): tmux workers keep that call inside
#     the profile named by R_PROFILE_USER, and any snapshotted worker carries
#     SPADES_PROJECT_LIB_SNAPSHOT.
# The command-line test is deliberately strict -- an R binary AND the loop as a
# call -- because a bare word match caught this package's own test harness.
.workerProcesses <- function(self = Sys.getpid()) {
  found <- character()
  ps <- tryCatch(suppressWarnings(system2("ps", c("-eo", "pid=,args="), stdout = TRUE, stderr = FALSE)),
                 error = function(e) character())
  if (length(ps)) {
    ps <- trimws(ps)
    pid <- suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", ps)))
    keep <- .isWorkerCmdline(ps) & !is.na(pid) & pid != self
    found <- c(found, ps[keep])
  }
  if (.Platform$OS.type == "unix" && dir.exists("/proc")) {
    pids <- list.files("/proc", pattern = "^[0-9]+$")
    pids <- setdiff(pids, as.character(self))
    for (p in pids) {
      f <- file.path("/proc", p, "environ")
      if (!file.exists(f) || is.na(file.access(f, 4)) || file.access(f, 4) != 0) next
      raw <- tryCatch(suppressWarnings(readBin(f, "raw", 262144L)), error = function(e) raw())
      if (!length(raw)) next
      vars <- vapply(split(raw, cumsum(raw == as.raw(0))), function(chunk) rawToChar(chunk[chunk != as.raw(0)]), "")
      isWorker <- any(startsWith(vars, "SPADES_PROJECT_LIB_SNAPSHOT=")) ||
        any(grepl("^R_PROFILE_USER=.*worker_(respawn|startup)[^/]*\\.R$", vars))
      if (isWorker) found <- c(found, paste0(p, " (worker environment) ", sub("^R_PROFILE_USER=", "", grep("^R_PROFILE_USER=", vars, value = TRUE)[1])))
    }
  }
  found
}

.pidsAlive <- function(pids) {
  vapply(pids, function(p) {
    if (is.na(p)) return(FALSE)
    if (.Platform$OS.type == "unix") {
      # Linux has /proc; macOS and the BSDs do not, so ask the kernel with signal 0
      if (dir.exists("/proc")) return(dir.exists(file.path("/proc", p)))
      st <- tryCatch(suppressWarnings(system2("kill", c("-0", p), stdout = FALSE, stderr = FALSE)),
                     error = function(e) 1L)
      return(identical(as.integer(st), 0L))
    }
    out <- tryCatch(suppressWarnings(system2("tasklist", c("/FI", shQuote(paste0("PID eq ", p)), "/NH"),
                                             stdout = TRUE, stderr = FALSE)),
                    error = function(e) character())
    any(grepl(paste0("\\b", p, "\\b"), out))
  }, logical(1))
}

# Is this `ps -eo pid=,args=` line a worker? An R binary (R, Rscript, or the
# exec/R behind a wrapper) whose arguments contain the loop *as a call*.
.isWorkerCmdline <- function(lines) {
  args <- sub("^[0-9]+[[:space:]]+", "", lines)
  first <- sub("[[:space:]].*$", "", args)
  isR <- grepl("(^|/)(R|Rscript|Rterm(\\.exe)?)$", first) | grepl("bin/exec/R$", first)
  isR & grepl("(^|[^A-Za-z0-9_.])(SpaDES\\.project::)?(tmuxRunWorkerLoop|runWorkerLoopFuture)\\(", args)
}

#' @export
#' @rdname librarySnapshot
snapshotLibrary <- function(libPath = .libPaths()[1],
                            where = getOption("spades.activeRunningPath"),
                            id = paste0(Sys.info()[["nodename"]], "_", Sys.getpid()),
                            setLibPaths = TRUE) {
  if (is.null(where))
    stop("snapshotLibrary(): `where` is required (usually the experiment's activeRunningPath)",
         call. = FALSE)
  snap <- .snapshotLibraryBase(libPath, where, id)
  if (isTRUE(setLibPaths)) {
    .libPaths(c(snap, setdiff(.libPaths(), normalizePath(libPath, mustWork = FALSE))))
    Sys.setenv(SPADES_PROJECT_LIB_SNAPSHOT = snap)
  }
  snap
}

# Base R only, on purpose: this body is deparsed into the worker startup scripts
# (`.librarySnapshotCode()`), where it must run before any package -- this one
# included -- has been loaded from the library being copied.
.snapshotLibraryBase <- function(libPath, where, id) {
  libPath <- normalizePath(libPath, mustWork = TRUE)
  snap <- file.path(where, paste0("lib_", id))
  if (dir.exists(snap)) unlink(snap, recursive = TRUE)
  dir.create(snap, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(libPath, recursive = TRUE, all.files = TRUE, no.. = TRUE, include.dirs = FALSE)
  dirs <- setdiff(unique(dirname(files)), ".")
  for (d in dirs) dir.create(file.path(snap, d), recursive = TRUE, showWarnings = FALSE)
  from <- file.path(libPath, files)
  to <- file.path(snap, files)
  # hardlink: free, and it keeps the old inode alive under a job that has it open
  ok <- suppressWarnings(file.link(from, to))
  # another filesystem, or a symlink: copy
  if (any(!ok)) ok[!ok] <- file.copy(from[!ok], to[!ok], copy.date = TRUE)
  if (any(!ok)) {
    unlink(snap, recursive = TRUE)
    stop("library snapshot: could not replicate ", sum(!ok), " file(s) from ", libPath, " into ", snap)
  }
  writeLines(c(libPath, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), as.character(Sys.getpid())),
             file.path(snap, ".snapshot_of"))
  # normalised, so it compares equal to what .libPaths() reports (/private/var on
  # macOS, drive-letter forms on Windows)
  normalizePath(snap)
}

#' @export
#' @rdname librarySnapshot
releaseLibrarySnapshot <- function(snapshot = Sys.getenv("SPADES_PROJECT_LIB_SNAPSHOT")) {
  if (!nzchar(snapshot) || !dir.exists(snapshot)) return(invisible(FALSE))
  if (!file.exists(file.path(snapshot, ".snapshot_of")))
    stop("Not a library snapshot (no .snapshot_of marker): ", snapshot, call. = FALSE)
  current <- Sys.getenv("SPADES_PROJECT_LIB_SNAPSHOT")
  same <- nzchar(current) && identical(normalizePath(current, mustWork = FALSE),
                                       normalizePath(snapshot, mustWork = FALSE))
  unlink(snapshot, recursive = TRUE)
  if (same) Sys.unsetenv("SPADES_PROJECT_LIB_SNAPSHOT")
  invisible(TRUE)
}

#' @export
#' @rdname librarySnapshot
sweepLibrarySnapshots <- function(where = getOption("spades.activeRunningPath")) {
  if (is.null(where) || !dir.exists(where)) return(invisible(character()))
  snaps <- list.files(where, pattern = "^lib_", full.names = TRUE)
  snaps <- snaps[dir.exists(snaps) & file.exists(file.path(snaps, ".snapshot_of"))]
  if (!length(snaps)) return(invisible(character()))
  pids <- suppressWarnings(as.integer(sub(".*_([0-9]+)$", "\\1", basename(snaps))))
  # The owner is the id's last field; a snapshot made on another host cannot be
  # judged from here, so it is left alone.
  host <- sub("^lib_(.*)_[0-9]+$", "\\1", basename(snaps))
  dead <- host == Sys.info()[["nodename"]] & !is.na(pids) & !.pidsAlive(pids)
  for (s in snaps[dead]) unlink(s, recursive = TRUE)
  invisible(snaps[dead])
}

# The startup snippet for a worker script. Base R only: nothing may be loaded
# from the library being copied before the copy exists. A failed snapshot is
# reported and the worker runs on the shared library, which is the old behaviour.
.librarySnapshotCode <- function(libPath, where) {
  paste0(
    "local({ .snapBase <- ", paste(deparse(.snapshotLibraryBase, control = "useSource"), collapse = "\n"), "\n",
    "  .lib <- ", deparse1(libPath), "; .where <- ", deparse1(where), "\n",
    "  .snap <- tryCatch(.snapBase(.lib, .where, paste0(Sys.info()[['nodename']], '_', Sys.getpid())),\n",
    "                    error = function(e) { message('library snapshot failed; running on the shared library: ', conditionMessage(e)); NULL })\n",
    "  if (!is.null(.snap)) { .libPaths(c(.snap, setdiff(.libPaths(), normalizePath(.lib, mustWork = FALSE)))); Sys.setenv(SPADES_PROJECT_LIB_SNAPSHOT = .snap) }\n",
    "}); "
  )
}

# Release this process's snapshot when R exits (quit() does not unwind on.exit
# handlers of the loop's frame, but does run finalizers registered with onexit).
.snapshotEnv <- new.env(parent = emptyenv())
.registerSnapshotRelease <- function() {
  if (isTRUE(.snapshotEnv$registered)) return(invisible(FALSE))
  reg.finalizer(.snapshotEnv, function(e) try(releaseLibrarySnapshot(), silent = TRUE), onexit = TRUE)
  .snapshotEnv$registered <- TRUE
  invisible(TRUE)
}
