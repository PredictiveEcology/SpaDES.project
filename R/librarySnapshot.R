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
#' @param activeRunningPath Where the experiment keeps its `Running_*` sentinels.
#'   Snapshots made before they moved beside the library are still found there,
#'   and it is where a snapshot goes when the directory beside the library
#'   cannot be created.
#' @param requireIdle Logical. Refuse to install while `libraryInUse()` finds a
#'   worker. Set to `FALSE` only when you know the workers are idle.
#' @param installFun Function of `(packages, libPath)` performing the install;
#'   defaults to `Require::Install()`. Exposed for tests.
#' @param where Directory holding snapshots. For `snapshotLibrary()` the default
#'   (`NULL`) is `<dirname(libPath)>/<basename(libPath)>_snapshots/`, beside the
#'   library and so on its filesystem, where hardlinks work; with the snapshots in
#'   the experiment's directory on another filesystem (an HPC library on NFS, the
#'   run directory on GPFS) every file was copied, about 2 GB per job. When that
#'   directory is not writable the snapshot goes to
#'   `getOption("spades.activeRunningPath")` instead, with a message.
#'   For `sweepLibrarySnapshots()` the default is every place a snapshot can be:
#'   that option, the directory beside the library, and the directory of this
#'   session's own snapshot.
#' @param id Snapshot id; defaults to `<nodename>_<pid>-<token>` (see
#'   `.snapshotId()`), where the token is `j<SLURM_JOB_ID>[s<SLURM_PROCID>]` under
#'   Slurm and `r<random hex>` otherwise. The token keeps two processes that share
#'   a nodename and a pid (jobs in separate PID namespaces) apart, and
#'   `sweepLibrarySnapshots()` reads the pid to tell a live owner from a dead one.
#'   An id whose directory already exists is never reused: a random suffix is added.
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
                            where = NULL,
                            id = .snapshotId(),
                            setLibPaths = TRUE) {
  snap <- .snapshotLibraryBase(libPath, where, id, fallback = getOption("spades.activeRunningPath"))
  if (isTRUE(setLibPaths)) {
    .libPaths(c(snap, setdiff(.libPaths(), normalizePath(libPath, mustWork = FALSE))))
    Sys.setenv(SPADES_PROJECT_LIB_SNAPSHOT = snap)
  }
  snap
}

# Base R only (these helpers and `.snapshotLibraryBase()` are deparsed into worker
# startup scripts). The id is `<nodename>_<pid>-<token>`: the pid and host are what
# `sweepLibrarySnapshots()` parses, the token is what tells apart two processes that
# have the same nodename and pid, as jobs in separate PID namespaces do (Slurm on
# the GPSC). tempfile() supplies the random part without touching the RNG stream.
.snapshotId <- function() {
  job <- Sys.getenv("SLURM_JOB_ID")
  proc <- Sys.getenv("SLURM_PROCID")
  token <- if (nzchar(job)) paste0("j", gsub("[^A-Za-z0-9]", "", job),
                                   if (nzchar(proc)) paste0("s", gsub("[^A-Za-z0-9]", "", proc)))
           else paste0("r", sub("^file", "", basename(tempfile())))
  paste0(Sys.info()[["nodename"]], "_", Sys.getpid(), "-", token)
}

# Beside the library, so on its filesystem and file.link() works; never inside it,
# where R would take the folder for a package.
.snapshotDir <- function(libPath) {
  libPath <- normalizePath(libPath, mustWork = FALSE)
  file.path(dirname(libPath), paste0(basename(libPath), "_snapshots"))
}

# Where snapshots may be: beside the library, and beside this session's own snapshot
# (a worker's .libPaths()[1] is that snapshot, whose parent is the directory).
.snapshotDirs <- function(libPath = .libPaths()[1]) {
  own <- Sys.getenv("SPADES_PROJECT_LIB_SNAPSHOT")
  unique(c(.snapshotDir(libPath), if (nzchar(own)) dirname(own)))
}

# The PID namespace of this process (Linux), NA where there is none to read.
.pidNamespace <- function() {
  ns <- tryCatch(suppressWarnings(Sys.readlink("/proc/self/ns/pid")), error = function(e) NA_character_)
  if (length(ns) != 1L || is.na(ns) || !nzchar(ns)) NA_character_ else ns
}

# Base R only, on purpose: this body is deparsed into the worker startup scripts
# (`.librarySnapshotCode()`), where it must run before any package -- this one
# included -- has been loaded from the library being copied.
.snapshotLibraryBase <- function(libPath, where = NULL, id = .snapshotId(), fallback = NULL) {
  libPath <- normalizePath(libPath, mustWork = TRUE)
  if (is.null(where)) {
    where <- .snapshotDir(libPath)
    if (!(dir.exists(where) || dir.create(where, recursive = TRUE, showWarnings = FALSE)) ||
        file.access(where, 2) != 0) {
      if (is.null(fallback))
        stop("library snapshot: cannot write to ", where, " and no fallback directory is set")
      message("library snapshot: cannot write to ", where, "; using ", fallback, " (files will be copied, not linked)")
      where <- fallback
    }
  }
  snap <- file.path(where, paste0("lib_", id))
  # never remove a directory this call did not create: it may be a live job's library
  if (dir.exists(snap) || file.exists(snap))
    snap <- paste0(snap, "x", sub("^file", "", basename(tempfile())))
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
  # lines: library, time, pid, PID namespace. Older markers stop at the pid.
  writeLines(c(libPath, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), as.character(Sys.getpid()),
               paste0("pidns=", .pidNamespace())),
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
sweepLibrarySnapshots <- function(where = c(getOption("spades.activeRunningPath"), .snapshotDirs())) {
  where <- where[dir.exists(where)]
  if (!length(where)) return(invisible(character()))
  snaps <- list.files(where, pattern = "^lib_", full.names = TRUE)
  snaps <- snaps[dir.exists(snaps) & file.exists(file.path(snaps, ".snapshot_of"))]
  if (!length(snaps)) return(invisible(character()))
  # The id is `<host>_<pid>` or `<host>_<pid>-<token>`. A snapshot made on another
  # host, or in another PID namespace, cannot be judged from here (its pid means
  # nothing in this process's namespace), so it is left alone. A marker with no
  # `pidns=` line is from before namespaces were recorded and is judged by pid.
  idRx <- "^lib_(.*)_([0-9]+)(-[A-Za-z0-9.]+)?$"
  pids <- suppressWarnings(as.integer(sub(idRx, "\\2", basename(snaps))))
  host <- sub(idRx, "\\1", basename(snaps))
  me <- .pidNamespace()
  judgeable <- vapply(snaps, function(s) {
    ns <- grep("^pidns=", readLines(file.path(s, ".snapshot_of"), warn = FALSE), value = TRUE)
    !length(ns) || identical(sub("^pidns=", "", ns[1L]), if (is.na(me)) "NA" else me)
  }, logical(1))
  dead <- grepl(idRx, basename(snaps)) & host == Sys.info()[["nodename"]] & judgeable &
    !is.na(pids) & !.pidsAlive(pids)
  # normalised before unlinking, so the return value matches what snapshotLibrary() gave out
  gone <- normalizePath(snaps[dead], mustWork = FALSE)
  for (s in snaps[dead]) unlink(s, recursive = TRUE)
  invisible(gone)
}

.deparseAs <- function(name, fun) paste0(name, " <- ", paste(deparse(fun, control = "useSource"), collapse = "\n"), "\n")

# The startup snippet for a worker script. Base R only: nothing may be loaded
# from the library being copied before the copy exists. A failed snapshot is
# reported and the worker runs on the shared library, which is the old behaviour.
# A dev library (?devLibrary) is never installed into, so it needs no snapshot: "".
.librarySnapshotCode <- function(libPath, fallback = NULL) {
  if (.isDevLibrary(libPath)) return("")
  paste0(
    "local({ ", .deparseAs(".snapshotId", .snapshotId), .deparseAs(".pidNamespace", .pidNamespace),
    .deparseAs(".snapshotDir", .snapshotDir), .deparseAs(".snapBase", .snapshotLibraryBase),
    "  .lib <- ", deparse1(libPath), "; .fallback <- ", deparse1(fallback), "\n",
    "  .snap <- tryCatch(.snapBase(.lib, fallback = .fallback),\n",
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
