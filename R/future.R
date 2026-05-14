# experimentFuture  -- tmux-free parallel job runner
#
# Mirrors experimentTmux in capabilities (same GoogleSheets queue, same remote
# machine setup, same global.R sourcing model) but replaces tmux panes with
# background R processes.  All worker output is captured to per-worker log
# files on localhost via callr::r_bg(), enabling true streaming logs.

# -- Internal constants -----------------------------------------------------

.future_meta_cols <- c(
  "status", "claimed_by", "started_at", "finished_at",
  "DEoptimElapsedTime", "machine_name", "process_id",
  "heartbeat_at", "heartbeat_iter", "iterationsTotal", "interrupted_at"
)

# -- experimentFuture -------------------------------------------------------

#' Run parallel R jobs using background processes (tmux-free)
#'
#' @description
#' A tmux-free alternative to \code{\link{experimentTmux}} that dispatches
#' parallel workers as background R processes via \pkg{callr}.  Workers claim
#' jobs from a GoogleSheets or local-RDS queue, source \code{global_path} for
#' each job, and write all console output to per-worker log files on localhost.
#'
#' The function returns immediately (non-blocking) with a handle object of
#' class \code{"experimentFuture"}.  Use \code{\link{awaitExperimentFuture}} to
#' block until all workers finish, or \code{print(ef)} to check live status.
#' Because workers write to files via \code{callr::r_bg()}'s \code{stdout} /
#' \code{stderr} arguments, logs appear in real time and can be followed with
#' \code{tail -f}.
#'
#' @param df data.frame of parameter combinations.  Each row is one job.
#' @param global_path Path to the R script each worker sources per job.
#'   Defaults to \code{"global.R"} in the current directory.
#' @param cores \code{NULL} for local parallel workers, or a character vector
#'   of SSH hostnames for remote workers.  When \code{cores} is provided,
#'   \code{.setup_remote_machine} is called for each unique host before
#'   workers are launched, replicating the full SpaDES environment (packages,
#'   GitHub PAT, gargle OAuth cache) on each remote machine.
#' @param n_workers Number of parallel workers.  Defaults to
#'   \code{length(cores)} for remote workers, or \code{4L} for local.
#' @param queue_path Path to the local RDS queue file.  Created automatically
#'   if it does not yet exist.  Defaults to
#'   \code{<dirname(global_path)>/future_queue.rds}.
#' @param on_interrupt \code{"requeue"} (default) re-queues interrupted jobs
#'   as PENDING; \code{"fail"} marks them INTERRUPTED permanently.
#' @param ss_id Google Sheets ID (or Drive folder ID) for the shared queue.
#'   When provided workers use the GS backend instead of the local RDS file.
#' @param forceLocalQueueToGS If \code{TRUE}, overwrite the GS sheet with the
#'   local \code{df} even if the sheet already contains rows.
#' @param email Gargle OAuth e-mail for Google Sheets auth.
#' @param cache_path Gargle OAuth cache directory.
#' @param runNameLabel Quoted expression evaluated in the job environment to
#'   derive a human-readable run name (used in log messages and queue
#'   metadata).
#' @param log_dir Directory for per-worker log files.  Created if needed.
#'   Defaults to \code{"logs"} relative to the current working directory.
#' @param activeRunningPath Directory for \code{Running_*.rds} marker files
#'   (file-based backend only).
#' @param sp_dev_path Local path to SpaDES.project source tree to sync to
#'   remote workers (optional; uses installed binary if \code{NULL}).
#' @param local_pat_file Path to a file containing a GitHub PAT to copy to
#'   remote workers.
#' @param copyModules Logical. If \code{TRUE} and remote hosts are present,
#'   rsyncs the directory given by \code{getOption("spades.modulePath")} to the
#'   same absolute path on each remote host after \code{.setup_remote_machine()}
#'   completes.  Issues a warning and skips if the option is unset.
#'   Default \code{FALSE}.
#' @param ... Additional named arguments stored in \code{.future_dots.rds} and
#'   loaded into each worker's \code{.GlobalEnv} before sourcing
#'   \code{global_path}.
#'
#' @return An object of class \code{"experimentFuture"} (a list) containing:
#'   \describe{
#'     \item{\code{procs}}{List of \code{callr::r_bg} process objects, one per
#'       local worker (or \code{future} objects for remote cluster workers).}
#'     \item{\code{log_files}}{Character vector of log file paths.}
#'     \item{\code{log_dir}}{Absolute path to the log directory.}
#'     \item{\code{queue_path}}{Absolute path to the queue RDS file.}
#'     \item{\code{cores}}{The \code{cores} argument as supplied.}
#'   }
#'
#' @examples
#' \dontrun{
#' ## -- Minimal: build a tiny global.R, then run a 2 x 2 experiment ---------
#' tdir <- file.path(tempdir(), "experimentFuture-demo")
#' dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
#' writeLines(
#'   'message("scenario=", .scenario, " rep=", .rep); Sys.sleep(2)',
#'   file.path(tdir, "global.R")
#' )
#' expt <- expand.grid(.scenario = c("A", "B"), .rep = 1:2,
#'                     stringsAsFactors = FALSE)
#'
#' ef <- experimentFuture(
#'   df          = expt,
#'   global_path = file.path(tdir, "global.R"),
#'   n_workers   = 2L,
#'   queue_path  = file.path(tdir, "future_queue.rds"),
#'   log_dir     = file.path(tdir, "logs")
#' )
#'
#' ## -- Live inspection while workers run -----------------------------------
#' print(ef)                                         # alive/done per worker
#' experimentMonitor(ef)                             # pid + machine + runName
#' experimentMonitor(ef, stats = TRUE)               # adds CPU / RAM / state
#' queueRead(ef$queue_path)                          # full queue snapshot
#' experimentFutureList(ef)                          # cluster-wide pid list
#' cat(readLines(ef$log_files[[1L]]), sep = "\n")    # tail one log
#'
#' awaitExperimentFuture(ef)    # blocks until both workers exit
#'
#' ## -- Killing workers ----------------------------------------------------
#'
#' # Graceful stop: workers finish their CURRENT job, then exit.
#' # Any remaining PENDING jobs stay in the queue and can be resumed later
#' # by calling experimentFuture() again with the same queue_path.
#' killExperimentFuture(ef)
#'
#' # Immediate stop (force): workers are killed immediately.
#' # Jobs that were mid-execution may remain as RUNNING in the queue; reset them with:
#' #   tmuxRefreshQueueStatus(ef$queue_path)   # file-based backend
#' # The GS backend reclaims stale RUNNING entries automatically before each new claim.
#' killExperimentFuture(ef, force = TRUE)
#' tmuxRefreshQueueStatus(ef$queue_path)   # clean up stale RUNNING entries
#'
#' # Cluster-wide kill (works for `cores = c(...)` clusters too):
#' # sends SIGTERM to every worker on every machine, waits for exit, runs
#' # tmuxRefreshQueueStatus(), and pushes the demotion to the Google Sheet
#' # if `ss_id` was used (via the <queue_path>.ss_id sidecar).
#' experimentFutureList(ef, kill = TRUE)
#'
#' ## -- Resuming after a kill ----------------------------------------------
#'
#' # Jobs left as PENDING (or INTERRUPTED with on_interrupt = "requeue") are
#' # automatically picked up when you call experimentFuture() again with the
#' # same queue_path  -- no need to re-specify df.
#' ef2 <- experimentFuture(
#'   df          = expt,         # ignored if queue_path already exists
#'   global_path = file.path(tdir, "global.R"),
#'   n_workers   = 2L,
#'   queue_path  = file.path(tdir, "future_queue.rds"),
#'   log_dir     = file.path(tdir, "logs")
#' )
#' awaitExperimentFuture(ef2)   # wait for remaining jobs to finish
#'
#' queueRead(ef2$queue_path)    # full snapshot (data.table)
#' table(queueRead(ef2$queue_path)$status)   # all DONE
#'
#' cat(readLines(ef2$log_files[[1]]), sep = "\n")   # inspect worker 1 log
#'
#' ## -- Remote workers (pre-setup required) -------------------------------
#' ef <- experimentFuture(
#'   df             = expt,
#'   global_path    = file.path(tdir, "global.R"),
#'   cores          = c("node01", "node02"),
#'   n_workers      = 2L,
#'   ss_id          = "YOUR_GOOGLE_SHEET_ID",
#'   email          = "you@example.com",
#'   cache_path     = "~/.cache/gargle",
#'   local_pat_file = "~/.github_pat"
#' )
#' killExperimentFuture(ef)     # graceful stop on remote workers too
#' }
#'
#' @seealso \code{\link{experimentTmux}}, \code{\link{awaitExperimentFuture}},
#'   \code{\link{tmuxRunWorkerLoop}}
#' @export
experimentFuture <- function(
  df,
  global_path       = "global.R",
  cores             = NULL,
  n_workers         = if (is.null(cores)) 4L else length(cores),
  queue_path        = NULL,
  on_interrupt      = c("requeue", "fail"),
  ss_id             = NULL,
  forceLocalQueueToGS = FALSE,
  email             = getOption("gargle_oauth_email"),
  cache_path        = getOption("gargle_oauth_cache"),
  runNameLabel      = quote(colnames(q)[1:2]),
  log_dir           = "logs",
  activeRunningPath = getOption("spades.activeRunningPath"),
  sp_dev_path       = NULL,
  local_pat_file    = NULL,
  copyModules       = FALSE,
  ...
) {

  if (!requireNamespace("callr", quietly = TRUE))
    stop("Package 'callr' is required. Install with: install.packages('callr')",
         call. = FALSE)

  on_interrupt <- match.arg(on_interrupt)

  # -- 1. Normalize paths --------------------------------------------------
  global_path <- normalizePath(global_path, mustWork = FALSE)
  if (is.null(queue_path))
    queue_path <- file.path(dirname(global_path), "future_queue.rds")
  queue_path <- normalizePath(queue_path, mustWork = FALSE)
  log_dir    <- normalizePath(log_dir,    mustWork = FALSE)

  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  activeRunningPath <- tmuxActiveRunningPath(
    activeRunningPath = activeRunningPath, queue_path
  )

  # -- 2. Save ... args so workers can load complex objects -----------------
  dots_path <- file.path(dirname(queue_path), ".future_dots.rds")
  dots      <- list(...)
  if (length(dots) > 0L) {
    saveRDS(dots, dots_path)
  } else if (file.exists(dots_path)) {
    unlink(dots_path)
    dots_path <- NULL
  } else {
    dots_path <- NULL
  }

  # -- 3. Initialize local queue --------------------------------------------
  if (!file.exists(queue_path)) {
    if (missing(df) || is.null(df))
      stop("'df' must be provided when 'queue_path' does not yet exist.",
           call. = FALSE)
    tmuxPrepareQueueFromDF(df, queue_path)
  }

  # -- 4. Google Sheets initialization (mirrors experimentTmux) -------------
  if (!is.null(ss_id)) {
    if (!is.null(email))      options(gargle_oauth_email = email)
    if (!is.null(cache_path)) options(gargle_oauth_cache = cache_path)
    options(gargle_oauth_client_type = "web")
  }

  if (!is.null(queue_path) && !is.null(ss_id)) {
    # Resolve Drive folder -> sheet ID
    isDir <- isGoogleDriveDirectory(ss_id)
    if (isTRUE(isDir)) {
      reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
      sheet_name <- gsub("\\.rds$", "", basename(queue_path))
      existing   <- googledrive::drive_ls(googledrive::as_id(ss_id), pattern = sheet_name)
      if (nrow(existing) > 0L) {
        ss_id <- existing$id[1L]
      } else {
        reproducible::.requireNamespace("googlesheets4", stopOnFALSE = TRUE)
        googlesheets4::gs4_auth()
        googledrive::drive_auth()
        new_sheet <- googlesheets4::gs4_create(name = sheet_name, sheets = "Status")
        googledrive::drive_mv(file = googledrive::as_id(new_sheet),
                              path = googledrive::as_id(ss_id))
        ss_id <- as.character(googledrive::as_id(new_sheet))
      }
    }

    # Drop a sidecar file so `experimentFutureList(kill = TRUE)` can find
    # the GS sheet associated with this local queue and push the same
    # demotion to GS after killing local workers.
    try(writeLines(as.character(ss_id), paste0(queue_path, ".ss_id")),
        silent = TRUE)

    # Sync local queue <-> GS
    gs_q <- try(.gs_read_queue(ss_id), silent = TRUE)

    if (!inherits(gs_q, "try-error") && nrow(gs_q) > 0L && isFALSE(forceLocalQueueToGS)) {
      if (!missing(df) && !is.null(df)) {
        gs_names_reverted <- names(revertDotNames(data.table::copy(data.table::setDT(gs_q))))
        gs_data_cols  <- setdiff(gs_names_reverted, .future_meta_cols)
        df_data_cols  <- setdiff(names(df),         .future_meta_cols)
        extra_in_gs   <- setdiff(gs_data_cols, df_data_cols)
        missing_in_gs <- setdiff(df_data_cols, gs_data_cols)
        if (length(extra_in_gs) > 0L || length(missing_in_gs) > 0L) {
          msg_parts <- character(0)
          if (length(missing_in_gs))
            msg_parts <- c(msg_parts,
              paste0("  In df but missing from GS : ", paste(missing_in_gs, collapse = ", ")))
          if (length(extra_in_gs))
            msg_parts <- c(msg_parts,
              paste0("  In GS but missing from df : ", paste(extra_in_gs, collapse = ", ")))
          stop(
            "Google Sheet column names do not match 'df'.\n",
            "Pass forceLocalQueueToGS = TRUE to overwrite the sheet with the local df.\n",
            paste(msg_parts, collapse = "\n"),
            call. = FALSE
          )
        }
      }
      q <- revertDotNames(data.table::setDT(gs_q))
      saveRDS(q, queue_path)
    } else {
      # Push local queue to GS
      q         <- readRDS(queue_path)
      q_sync    <- as.data.frame(lapply(q, as.character))
      names(q_sync) <- gsub("^\\.", dotTxt, names(q_sync))
      try(googlesheets4::with_gs4_quiet(
        googlesheets4::range_write(ss = ss_id, data = q_sync,
                                   sheet = "Status", range = "A1", reformat = FALSE)
      ), silent = TRUE)
    }
  }

  # -- 5. Remote machine setup -----------------------------------------------
  unique_hosts <- if (!is.null(cores)) {
    setdiff(unique(cores), c("localhost", "127.0.0.1", Sys.info()[["nodename"]]))
  } else {
    character(0)
  }

  if (length(unique_hosts) > 0L) {
    message("Setting up ", length(unique_hosts), " remote host(s): ",
            paste(unique_hosts, collapse = ", "))
    .module_path <- if (isTRUE(copyModules)) {
      mp <- getOption("spades.modulePath")
      if (is.null(mp) || !nzchar(mp)) {
        warning("copyModules = TRUE but getOption('spades.modulePath') is not set; skipping module rsync.",
                call. = FALSE)
        NULL
      } else {
        normalizePath(mp, mustWork = FALSE)
      }
    } else {
      NULL
    }

    for (host in unique_hosts) {
      .setup_remote_machine(
        host           = host,
        global_path    = global_path,
        queue_path     = queue_path,
        cache_path     = cache_path,
        sp_dev_path    = sp_dev_path,
        local_pat_file = local_pat_file,
        module_path    = .module_path
      )
    }
    message("Remote setup complete.")
  }

  # -- 6. Determine effective worker hosts -----------------------------------
  is_local <- is.null(cores) ||
              all(cores %in% c("localhost", "127.0.0.1", Sys.info()[["nodename"]]))

  # -- 7. Build log and stop-file paths --------------------------------------
  log_files  <- file.path(log_dir, sprintf("worker_%02d.log",      seq_len(n_workers)))
  stop_files <- file.path(log_dir, sprintf("worker_%02d.stop",     seq_len(n_workers)))
  # Remove any leftover stop files from a previous run
  for (sf in stop_files) if (file.exists(sf)) unlink(sf)

  # -- 8. Launch workers -----------------------------------------------------
  procs <- vector("list", n_workers)

  # Fixed args for every worker (avoiding reference issues in loops)
  worker_args <- list(
    queue_path        = queue_path,
    global_path       = global_path,
    on_interrupt      = on_interrupt,
    ss_id             = ss_id,
    email             = email,
    cache_path        = cache_path,
    runNameLabel      = runNameLabel,
    activeRunningPath = activeRunningPath,
    dots_path         = dots_path
  )

  if (is_local) {
    # Local workers: callr::r_bg() writes stdout + stderr directly to log files.
    # This gives true streaming logs (visible via tail -f) without any sink() magic.
    for (i in seq_len(n_workers)) {
      procs[[i]] <- callr::r_bg(
        func = function(queue_path, global_path, on_interrupt, ss_id,
                        email, cache_path, runNameLabel, activeRunningPath,
                        dots_path, stop_file, lib_paths) {
          .libPaths(lib_paths)
          SpaDES.project::tmuxRunWorkerLoop(
            queue_path        = queue_path,
            global_path       = global_path,
            on_interrupt      = on_interrupt,
            ss_id             = ss_id,
            email             = email,
            cache_path        = cache_path,
            runNameLabel      = runNameLabel,
            activeRunningPath = activeRunningPath,
            dots_path         = dots_path,
            stop_file         = stop_file,
            pane_mode         = "reuse"
          )
        },
        args    = c(worker_args, list(stop_file = stop_files[[i]], lib_paths = .libPaths())),
        stdout  = log_files[[i]],
        stderr  = log_files[[i]],
        # Unset TMUX/TMUX_PANE so workers don't emit OSC 2 escape bytes into log files.
        # callr workers inherit the parent's environment (including tmux vars), but
        # they have no tmux pane  -- their output goes to a file, not a terminal.
        env     = c(TMUX = "", TMUX_PANE = ""),
        package = TRUE
      )
    }
  } else {
    # Remote workers via future::cluster (SSH-based PSOCK cluster).
    # Logs are written on the remote machine  -- documented limitation.
    if (!requireNamespace("future",     quietly = TRUE) ||
        !requireNamespace("parallelly", quietly = TRUE))
      stop("Packages 'future' and 'parallelly' are required for remote workers.",
           call. = FALSE)
    cl <- parallelly::makeClusterPSOCK(cores)
    future::plan(future::cluster, workers = cl)
    .fn <- runWorkerLoopFuture   # captured so future serializes it
    for (i in seq_len(n_workers)) {
      lf <- log_files[[i]]
      sf <- stop_files[[i]]
      wa <- worker_args
      procs[[i]] <- future::future(seed = TRUE, {
        .fn(
          queue_path        = wa$queue_path,
          global_path       = wa$global_path,
          on_interrupt      = wa$on_interrupt,
          ss_id             = wa$ss_id,
          email             = wa$email,
          cache_path        = wa$cache_path,
          runNameLabel      = wa$runNameLabel,
          activeRunningPath = wa$activeRunningPath,
          dots_path         = wa$dots_path,
          stop_file         = sf,
          log_file          = lf
        )
      })
    }
  }

  # -- 9. Emit log-watching instructions ------------------------------------
  # experimentFuture deliberately does NOT spawn tmux panes -- workers run
  # as background callr processes regardless of whether the master is in a
  # tmux session. Print the per-worker tail-f commands and a single tmux
  # one-liner the user can paste in any shell to spin up a viewing session
  # on demand.
  log_cmds   <- paste0("  tail -f ", log_files, "   # worker ", seq_len(n_workers),
                       collapse = "\n")
  tmux_oneliner <- .future_tmux_tail_cmd(log_files, session = "logs")
  message(
    "\nWorkers are running. To watch live logs, run in a separate shell:\n",
    log_cmds, "\n",
    "  -- or --\n",
    "  tail -f ", log_dir, "/worker_*.log\n",
    "\n",
    "Or, to view all logs in a single tmux session (paste in a shell):\n  ",
    tmux_oneliner, "\n"
  )

  # -- Return handle ---------------------------------------------------------
  structure(
    list(
      procs      = procs,
      log_files  = log_files,
      stop_files = stop_files,
      log_dir    = log_dir,
      queue_path = queue_path,
      cores      = cores,
      is_local   = is_local
    ),
    class = "experimentFuture"
  )
}


# -- Internal: build a single shell command that creates a tmux session,
#    one pane per log, each running tail -F. The user pastes this into any
#    bash/zsh shell (whether they are already in tmux or not -- tmux's
#    new-session can nest if needed; otherwise it attaches).
.future_tmux_tail_cmd <- function(log_files, session = "logs") {
  qsh   <- function(x) shQuote(x, type = "sh")
  tail1 <- function(p) sprintf("bash -lc %s", qsh(sprintf("exec tail -F -- %s", qsh(p))))

  parts <- c(
    sprintf("tmux new-session -d -s %s -n logs %s",
            qsh(session), tail1(log_files[[1L]]))
  )
  for (p in log_files[-1L]) {
    parts <- c(parts,
               sprintf("split-window -t %s %s", qsh(session), tail1(p)))
  }
  parts <- c(parts,
             sprintf("select-layout -t %s tiled", qsh(session)),
             sprintf("attach -t %s", qsh(session)))
  paste(parts, collapse = " \\; ")
}


# -- runWorkerLoopFuture -----------------------------------------------------

#' Worker loop for future/cluster-based remote execution
#'
#' @description
#' A thin wrapper around \code{\link{tmuxRunWorkerLoop}} that optionally redirects
#' console output to a log file before entering the job loop.  Used internally
#' by \code{\link{experimentFuture}} for remote (cluster) workers.  Local
#' workers use \code{callr::r_bg()} and do not need this wrapper.
#'
#' @param queue_path Path to the local RDS queue file.
#' @param global_path Path to the R script sourced for each job.
#' @param on_interrupt \code{"requeue"} or \code{"fail"}.
#' @param ss_id Google Sheets ID for the shared queue (or \code{NULL} for
#'   file-based queue).
#' @param email Gargle OAuth e-mail.
#' @param cache_path Gargle OAuth cache directory.
#' @param runNameLabel Quoted expression for deriving a run name.
#' @param activeRunningPath Directory for \code{Running_*.rds} marker files.
#' @param dots_path Path to an RDS file whose contents are loaded into
#'   \code{.GlobalEnv} before sourcing \code{global_path}.
#' @param stop_file Path to a sentinel file.  When this file is created (e.g.
#'   by \code{\link{killExperimentFuture}}), the worker exits cleanly after its
#'   current job finishes.
#' @param log_file Path to the log file for this worker.  If \code{NULL},
#'   output goes to the current connection.
#'
#' @return Invisibly returns the worker identifier string.
#' @export
runWorkerLoopFuture <- function(
  queue_path,
  global_path,
  on_interrupt      = c("requeue", "fail"),
  ss_id             = NULL,
  email             = NULL,
  cache_path        = NULL,
  runNameLabel      = quote(colnames(q)[1:2]),
  activeRunningPath = NULL,
  dots_path         = NULL,
  stop_file         = NULL,
  log_file          = NULL
) {
  on_interrupt <- match.arg(on_interrupt)

  # When `log_file` is supplied (the cluster path always supplies one),
  # spawn the worker loop as a callr::r_bg subprocess so stdout/stderr are
  # OS-level redirected (real-time writes) and the subprocess is
  # discoverable via /proc/<pid>/fd/1 -- same shape as the local
  # callr::r_bg branch in experimentFuture(), so experimentMonitor() and
  # the /proc-scan fallback see remote workers too.
  #
  # R-level sink() (the previous implementation) buffers writes until the
  # connection closes, which makes long-running logs invisible during
  # the run.
  if (!is.null(log_file)) {
    if (!requireNamespace("callr", quietly = TRUE))
      stop("Package 'callr' is required for runWorkerLoopFuture(log_file=).",
           call. = FALSE)

    proc <- callr::r_bg(
      func = function(queue_path, global_path, on_interrupt,
                      ss_id, email, cache_path, runNameLabel,
                      activeRunningPath, dots_path, stop_file, lib_paths) {
        .libPaths(lib_paths)
        SpaDES.project::tmuxRunWorkerLoop(
          queue_path        = queue_path,
          global_path       = global_path,
          on_interrupt      = on_interrupt,
          ss_id             = ss_id,
          email             = email,
          cache_path        = cache_path,
          runNameLabel      = runNameLabel,
          activeRunningPath = activeRunningPath,
          dots_path         = dots_path,
          stop_file         = stop_file,
          pane_mode         = "reuse"
        )
      },
      args = list(
        queue_path        = queue_path,
        global_path       = global_path,
        on_interrupt      = on_interrupt,
        ss_id             = ss_id,
        email             = email,
        cache_path        = cache_path,
        runNameLabel      = runNameLabel,
        activeRunningPath = activeRunningPath,
        dots_path         = dots_path,
        stop_file         = stop_file,
        lib_paths         = .libPaths()
      ),
      stdout  = log_file,
      stderr  = log_file,
      env     = c(TMUX = "", TMUX_PANE = ""),
      package = TRUE
    )
    worker_id <- paste0(Sys.info()[["nodename"]], "-", proc$get_pid())
    proc$wait()
    return(invisible(worker_id))
  }

  # No log_file: run inline (fall-through path, used when callers want
  # the worker to share the master's stdout, e.g. interactive debugging).
  worker_id <- paste0(Sys.info()[["nodename"]], "-", Sys.getpid())
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Worker ", worker_id, " starting.")

  SpaDES.project::tmuxRunWorkerLoop(
    queue_path        = queue_path,
    global_path       = global_path,
    on_interrupt      = on_interrupt,
    ss_id             = ss_id,
    email             = email,
    cache_path        = cache_path,
    runNameLabel      = runNameLabel,
    activeRunningPath = activeRunningPath,
    dots_path         = dots_path,
    stop_file         = stop_file,
    pane_mode         = "reuse"
  )

  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Worker ", worker_id, " finished.")
  invisible(worker_id)
}


# -- S3 methods --------------------------------------------------------------

#' @export
print.experimentFuture <- function(x, ...) {
  n     <- length(x$procs)
  width <- nchar(as.character(n))
  cat(sprintf("experimentFuture  -- %d worker(s)\n", n))
  cat(sprintf("  Queue  : %s\n", x$queue_path))
  cat(sprintf("  Log dir: %s\n", x$log_dir))
  for (i in seq_len(n)) {
    alive <- tryCatch(
      if (x$is_local) x$procs[[i]]$is_alive() else !future::resolved(x$procs[[i]]),
      error = function(e) NA
    )
    status <- if (isTRUE(alive)) "running" else if (isFALSE(alive)) "DONE   " else "unknown"
    cat(sprintf("  [%s] worker %0*d  %s\n", status, width, i, x$log_files[[i]]))
  }
  invisible(x)
}


# -- awaitExperimentFuture ---------------------------------------------------

#' Wait for all workers in an experimentFuture to finish
#'
#' @description
#' Blocks the calling R session until every worker has completed.
#' Optionally prints a summary of final queue statuses.
#'
#' @param ef An \code{"experimentFuture"} object returned by
#'   \code{\link{experimentFuture}}.
#' @param verbose If \code{TRUE} (default), print a \code{table()} of final
#'   queue statuses after all workers finish.
#'
#' @return The \code{ef} object, invisibly.
#' @export
awaitExperimentFuture <- function(ef, verbose = TRUE) {
  stopifnot(inherits(ef, "experimentFuture"))
  n <- length(ef$procs)
  message("Waiting for ", n, " worker(s) to finish...")
  if (ef$is_local) {
    for (p in ef$procs) p$wait()
  } else {
    lapply(ef$procs, future::value)
  }
  message("All workers finished.")
  if (verbose && !is.null(ef$queue_path) && file.exists(ef$queue_path)) {
    q <- readRDS(ef$queue_path)
    if ("status" %in% names(q)) {
      cat("\nFinal queue status:\n")
      print(table(q$status))
    }
  }
  invisible(ef)
}


# -- killExperimentFuture -----------------------------------------------------

#' Stop workers launched by experimentFuture
#'
#' @description
#' Two modes are available:
#'
#' \strong{Graceful} (\code{force = FALSE}, default): creates a per-worker
#' sentinel file.  Each worker checks for this file between jobs and exits
#' cleanly once its current job finishes.  Remaining \code{PENDING} jobs stay
#' in the queue and are picked up automatically when
#' \code{\link{experimentFuture}} is called again with the same
#' \code{queue_path}.
#'
#' \strong{Immediate} (\code{force = TRUE}): sends \code{SIGTERM} to each live
#' worker, causing the process to exit as soon as possible.  Because callr
#' workers run non-interactively, the process typically exits before R's
#' interrupt handler has a chance to update the queue.  Any jobs that were
#' \code{RUNNING} at the time of the kill will remain as \code{RUNNING} in the
#' queue until the next reclaim pass.  Call
#' \code{tmuxRefreshQueueStatus(ef$queue_path)} afterwards to reset stale
#' \code{RUNNING} entries to \code{INTERRUPTED}, or use the GS backend which
#' reclaims dead workers automatically before each new claim.
#'
#' @param ef An \code{"experimentFuture"} object returned by
#'   \code{\link{experimentFuture}}.
#' @param force If \code{FALSE} (default), signal workers via stop files so
#'   they exit after their current job completes.  If \code{TRUE}, send
#'   \code{SIGINT} to each live worker for an immediate but clean stop.
#'
#' @return \code{ef}, invisibly.
#' @seealso \code{\link{experimentFuture}}, \code{\link{awaitExperimentFuture}}
#' @export
killExperimentFuture <- function(ef, force = FALSE) {
  stopifnot(inherits(ef, "experimentFuture"))

  if (!force) {
    # -- Graceful: create stop files -----------------------------------------
    # tmuxRunWorkerLoop checks for the file between jobs and breaks the repeat loop.
    created <- 0L
    for (sf in ef$stop_files) {
      if (!file.exists(sf)) {
        file.create(sf)
        created <- created + 1L
      }
    }
    message("Stop files created for ", created, " worker(s).",
            "\nWorkers will exit after their current job finishes.",
            "\nCall awaitExperimentFuture(ef) to wait for them.")
  } else {
    # -- Force: send SIGINT ----------------------------------------------------
    # SIGINT triggers R's interrupt condition, which tmuxRunNextWorker catches and
    # uses to mark the in-progress job as PENDING (requeue) or INTERRUPTED (fail).
    killed <- 0L
    if (ef$is_local) {
      for (p in ef$procs) {
        if (isTRUE(try(p$is_alive(), silent = TRUE))) {
          try(p$kill(), silent = TRUE)
          killed <- killed + 1L
        }
      }
    } else {
      # Remote cluster: no direct signal; fall back to stop files
      message("Force kill is not supported for remote cluster workers; ",
              "using stop files instead.")
      for (sf in ef$stop_files) if (!file.exists(sf)) file.create(sf)
      killed <- length(ef$procs)
    }
    message("Sent kill signal to ", killed, " worker(s).",
            "\nStale RUNNING queue entries can be reset with:",
            "\n  tmuxRefreshQueueStatus(\"", ef$queue_path, "\")")
  }

  invisible(ef)
}


# -- experimentFutureList -----------------------------------------------------

#' Find (and optionally kill) live experimentFuture workers
#'
#' @description
#' Cross-session worker discovery for \code{\link{experimentFuture}}.  Scans
#' \code{/proc} for R processes whose redirected stdout points to a
#' \verb{worker_<NN>.log} file (the convention written by
#' \code{callr::r_bg(stdout = log_files[[i]])} in
#' \code{experimentFuture}), regardless of which R session originally
#' spawned them.  This is the right tool when:
#'
#' \itemize{
#'   \item you re-ran the experimentFuture example in a new R session and
#'     a fresh \code{tail -f} is silent because the previous run's workers
#'     are still claiming queue rows;
#'   \item you want to clean up orphans without remembering each
#'     \code{ef} handle;
#'   \item you want a one-glance view of \emph{which row} each worker is
#'     currently running (joined against the queue's
#'     \code{status == "RUNNING"} \code{process_id}).
#' }
#'
#' Linux-only (uses \code{/proc/<pid>/fd/1} to find the log file each
#' worker is writing).  For other Unixes use \code{lsof -p <pid>} or
#' \code{ps -ef | grep tmuxRunWorkerLoop} as a manual substitute.
#'
#' @param ef Optional shorthand: an \code{"experimentFuture"} object (or
#'   list of them) whose \code{queue_path} will be added to the discovery
#'   set.  Equivalent to passing \code{queue_paths = ef$queue_path} and
#'   handy when the result of \code{experimentFuture()} is still in
#'   scope.
#' @param kill   If \code{TRUE}, send \code{signal} to every worker found,
#'   wait up to 10 s for the processes to exit, then call
#'   \code{\link{tmuxRefreshQueueStatus}()} on each unique
#'   \code{queue_path} to demote the now-orphaned \verb{RUNNING} rows
#'   back to \verb{PENDING}. Default \code{FALSE} (list-only).
#' @param signal One of \code{"TERM"} (15, default; graceful), \code{"INT"}
#'   (2; like Ctrl-C), or \code{"KILL"} (9; immediate).
#' @param queue_paths Optional character vector of queue \code{.rds} paths to
#'   inspect for workers.  Use this across R sessions when the
#'   \code{ef} handle is no longer in scope (e.g. you restarted R but
#'   the workers from a prior \code{experimentFuture()} call are still
#'   alive on \code{mega} and \code{camas}).  Each queue's
#'   \code{status == "RUNNING"} rows are verified for liveness via
#'   \code{/proc} (local) or batched SSH (remote).  When \code{NULL}
#'   (default) and \code{ef} is also \code{NULL}, the function uses
#'   only queue files auto-discovered from local \code{/proc} -- which
#'   in turn only finds \code{callr::r_bg} workers, not PSOCK cluster
#'   workers, so on a node with no \code{r_bg} workers it sees nothing
#'   unless \code{ef} or \code{queue_paths} is supplied.
#'
#' @return A \code{data.frame} (one row per live worker) with columns:
#'   \describe{
#'     \item{\code{pid}}{Worker process ID.}
#'     \item{\code{started_at}}{Approximate process start time
#'       (\code{ctime} of \code{/proc/<pid>}).}
#'     \item{\code{log_file}}{Path the worker is writing stdout/stderr to.}
#'     \item{\code{queue_path}}{The first \verb{*_queue.rds} found in the
#'       log directory's parent (where \code{experimentFuture} puts it by
#'       default), or \code{NA} if not located.}
#'     \item{\code{runName}}{Hyphen-joined data column values of the row
#'       this worker is currently running, derived from the queue's
#'       \code{status == "RUNNING"} entry whose \code{process_id} matches.
#'       \code{NA} if the worker is between jobs.}
#'   }
#'   When \code{kill = TRUE}, the same data.frame is returned (invisibly)
#'   describing the workers that were signalled.
#'
#' @examples
#' \dontrun{
#' # Just list everything that's running (auto-discovery via /proc only)
#' experimentFutureList()
#'
#' # Pass the ef handle to also pick up PSOCK cluster workers and remote
#' # workers (anything in the queue, on any machine in `cores`).
#' ef <- experimentFuture(df = df, global_path = "global.R",
#'                        cores = c("localhost", "camas"), ...)
#' experimentFutureList(ef)
#' experimentFutureList(ef, kill = TRUE)
#'
#' # Across R sessions, when ef is gone, drive discovery off the queue path:
#' experimentFutureList(queue_paths = "/mnt/shared_cache/.../future_queue.rds")
#'
#' # Hard kill (SIGKILL, no chance to update queue meta on the worker side --
#' # but the post-kill tmuxRefreshQueueStatus() still demotes the rows).
#' experimentFutureList(ef, kill = TRUE, signal = "KILL")
#' }
#'
#' @seealso \code{\link{experimentFuture}}, \code{\link{killExperimentFuture}},
#'   \code{\link{tmuxRefreshQueueStatus}}
#' @export
experimentFutureList <- function(ef = NULL,
                                  kill = FALSE,
                                  signal = c("TERM", "INT", "KILL"),
                                  queue_paths = NULL) {
  signal <- match.arg(signal)
  if (.Platform$OS.type != "unix" || !dir.exists("/proc"))
    stop("experimentFutureList() requires a Linux-style /proc filesystem.",
         call. = FALSE)
  local_node <- Sys.info()[["nodename"]]

  # Accept a single experimentFuture object, a list of them, or NULL.
  ef_list <- NULL
  if (!is.null(ef)) {
    ef_list <- if (inherits(ef, "experimentFuture")) list(ef)
               else if (is.list(ef) &&
                        all(vapply(ef, inherits, logical(1L), "experimentFuture")))
                 ef
               else stop("`ef` must be an experimentFuture object or a list of them.",
                         call. = FALSE)
    queue_paths <- unique(c(queue_paths,
                            unlist(lapply(ef_list, `[[`, "queue_path"))))
  }

  # Build hostname -> SSH-name map.  Workers write `Sys.info()[["nodename"]]`
  # into the queue's `machine_name` column; that's the OS hostname (e.g.
  # "A159604"), not the SSH alias (e.g. "dougfir") in `~/.ssh/config` /
  # `/etc/hosts`.  For each SSH name in `ef$cores` (excluding the local
  # node aliases), probe `hostname -s` once and cache.  When the queue
  # reports `machine_name = "A159604"`, we look up the alias and SSH to
  # the alias instead.  Falls back to using machine_name verbatim when
  # no mapping is known.
  host_map <- character(0)   # named: hostname -> ssh_name
  if (!is.null(ef_list)) {
    cores_all <- unique(unlist(lapply(ef_list, `[[`, "cores")))
    cores_all <- setdiff(cores_all, c("localhost", "127.0.0.1", local_node))
    for (core in cores_all) {
      res <- tryCatch(
        suppressWarnings(system2(
          "ssh",
          c("-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
            core, "hostname -s"),
          stdout = TRUE, stderr = FALSE
        )),
        error = function(e) NULL
      )
      if (!is.null(res) && length(res) > 0L && nzchar(trimws(res[1L])))
        host_map[trimws(res[1L])] <- core
    }
  }
  # Resolve a machine_name (OS hostname) to an SSH-reachable name.
  .ssh_name_for <- function(m) {
    if (m %in% names(host_map)) host_map[[m]] else m
  }

  pid_dirs <- list.files("/proc", pattern = "^[0-9]+$")
  pids     <- suppressWarnings(as.integer(pid_dirs))
  pids     <- pids[!is.na(pids)]

  rows <- list()
  for (pid in pids) {
    fd1 <- tryCatch(Sys.readlink(sprintf("/proc/%d/fd/1", pid)),
                    error = function(e) "", warning = function(w) "")
    if (length(fd1) != 1L || !nzchar(fd1)) next
    if (!grepl("worker_[0-9]+\\.log$", fd1)) next

    # Confirm the process is an R worker (avoid false-positives like a
    # tail -f that redirected to a worker_NN.log).
    comm <- tryCatch(readLines(sprintf("/proc/%d/comm", pid), warn = FALSE),
                     error = function(e) NA_character_)
    if (length(comm) == 0L || is.na(comm) ||
        !grepl("^(R|Rscript|exec)$", comm)) next

    started <- tryCatch(format(file.info(sprintf("/proc/%d", pid))$ctime,
                               "%Y-%m-%d %H:%M:%S"),
                        error = function(e) NA_character_)

    log_dir   <- dirname(fd1)
    queue_dir <- dirname(log_dir)
    queue_files <- list.files(queue_dir, pattern = "_queue\\.rds$",
                              full.names = TRUE)
    queue_path  <- if (length(queue_files)) queue_files[[1L]] else NA_character_

    runName <- NA_character_
    if (!is.na(queue_path)) {
      q <- tryCatch(readRDS(queue_path), error = function(e) NULL)
      if (!is.null(q) && all(c("status", "process_id") %in% names(q))) {
        idx <- which(q$status == "RUNNING" &
                     suppressWarnings(as.integer(q$process_id)) == pid)
        if (length(idx)) {
          data_cols <- setdiff(names(q), .future_meta_cols)
          vals <- vapply(data_cols,
                         function(cn) as.character(q[[cn]][idx[1L]]),
                         character(1L))
          runName <- paste(vals, collapse = "-")
        }
      }
    }

    rows[[length(rows) + 1L]] <- data.frame(
      pid        = pid,
      machine    = local_node,
      started_at = started,
      log_file   = fd1,
      queue_path = queue_path,
      runName    = runName,
      stringsAsFactors = FALSE
    )
  }

  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(pid = integer(), machine = character(), started_at = character(),
               log_file = character(), queue_path = character(),
               runName = character(), stringsAsFactors = FALSE)

  # ---- Queue-file-based discovery (catches PSOCK cluster workers) --------
  # The local /proc scan only finds workers whose stdout fd points to a
  # `worker_NN.log` file -- i.e. callr::r_bg workers.  PSOCK cluster
  # workers spawned by experimentFuture(cores = ...) sink() to a log file
  # from inside R, so their /proc/<pid>/fd/1 still points to the SSH
  # socket and the scan misses them entirely (even on the local machine).
  #
  # The queue file is the authoritative record either way: every claim
  # writes machine_name + process_id under filelock.  Scan it for
  # status == "RUNNING" rows and verify each pid is alive
  # (file.exists("/proc/<pid>") locally, batched SSH check remotely).
  # Dedup against the /proc scan by (pid, machine).
  qp_for_remote <- unique(c(out$queue_path[!is.na(out$queue_path)],
                            queue_paths))
  qp_for_remote <- qp_for_remote[!is.na(qp_for_remote) & file.exists(qp_for_remote)]
  for (qp in qp_for_remote) {
    q <- tryCatch(readRDS(qp), error = function(e) NULL)
    if (is.null(q) || !all(c("status", "process_id", "machine_name") %in% names(q)))
      next
    idx <- which(q$status == "RUNNING" &
                 !is.na(q$machine_name) &
                 !is.na(suppressWarnings(as.integer(q$process_id))))
    if (length(idx) == 0L) next
    machines <- unique(q$machine_name[idx])
    data_cols <- setdiff(names(q), .future_meta_cols)
    seen <- if (nrow(out)) paste(out$pid, out$machine, sep = "@") else character(0)
    for (m in machines) {
      m_idx  <- idx[q$machine_name[idx] == m]
      m_pids <- as.integer(q$process_id[m_idx])
      keep   <- !(paste(m_pids, m, sep = "@") %in% seen)
      m_idx  <- m_idx[keep]
      m_pids <- m_pids[keep]
      if (length(m_pids) == 0L) next

      alive <- if (m == local_node) {
        file.exists(paste0("/proc/", m_pids))
      } else {
        a <- .ssh_pids_alive(.ssh_name_for(m), m_pids)
        if (is.null(a)) next   # SSH unreachable; skip silently
        a
      }
      live_pids <- m_pids[alive]
      log_for <- setNames(rep(NA_character_, length(m_pids)), as.character(m_pids))
      if (length(live_pids)) {
        if (m == local_node) {
          for (p in live_pids) {
            fp <- tryCatch(Sys.readlink(sprintf("/proc/%d/fd/1", p)),
                           error = function(e) "", warning = function(w) "")
            if (length(fp) == 1L && nzchar(fp) && grepl("worker_[0-9]+\\.log$", fp))
              log_for[as.character(p)] <- fp
          }
        } else {
          fp <- .ssh_pids_log_file(.ssh_name_for(m), live_pids)
          if (!is.null(fp)) {
            for (p in live_pids) {
              v <- fp[[as.character(p)]]
              if (!is.null(v) && nzchar(v) && grepl("worker_[0-9]+\\.log$", v))
                log_for[as.character(p)] <- v
            }
          }
        }
      }
      for (k in seq_along(m_pids)) {
        if (!isTRUE(alive[k])) next
        i <- m_idx[k]
        vals <- vapply(data_cols,
                       function(cn) as.character(q[[cn]][i]),
                       character(1L))
        rows[[length(rows) + 1L]] <- data.frame(
          pid        = m_pids[k],
          machine    = m,
          started_at = as.character(q$started_at[i]),
          log_file   = unname(log_for[as.character(m_pids[k])]),
          queue_path = qp,
          runName    = paste(vals, collapse = "-"),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  out <- if (length(rows)) do.call(rbind, rows) else out

  if (isTRUE(kill) && nrow(out) > 0L) {
    sig_num <- switch(signal, TERM = 15L, INT = 2L, KILL = 9L)
    is_local_row <- out$machine == local_node
    # Local kills via tools::pskill
    for (pid in out$pid[is_local_row])
      try(tools::pskill(pid, signal = sig_num), silent = TRUE)
    # Remote kills batched per machine via SSH
    if (any(!is_local_row)) {
      for (m in unique(out$machine[!is_local_row])) {
        m_pids <- out$pid[out$machine == m]
        try(.ssh_kill_pids(.ssh_name_for(m), m_pids, signal = signal),
            silent = TRUE)
      }
    }
    message("Sent SIG", signal, " to ", nrow(out), " worker(s) (",
            sum(is_local_row), " local, ", sum(!is_local_row), " remote).")

    # Wait for the workers to actually exit before refreshing the queue.
    # tmuxRefreshQueueStatus() demotes a RUNNING row to PENDING only if the
    # row's process_id is no longer alive; if we refresh too eagerly, the
    # killed workers' processes may not yet be reaped, so the demotion
    # would be skipped and the queue would stay stuck.
    deadline    <- Sys.time() + 10
    remaining_l <- out$pid[is_local_row]
    remaining_r <- split(out$pid[!is_local_row], out$machine[!is_local_row])
    while ((length(remaining_l) || length(remaining_r)) &&
           Sys.time() < deadline) {
      if (length(remaining_l)) {
        remaining_l <- remaining_l[
          vapply(remaining_l,
                 function(p) dir.exists(sprintf("/proc/%d", p)),
                 logical(1L))
        ]
      }
      if (length(remaining_r)) {
        machine_names <- names(remaining_r)
        remaining_r <- lapply(machine_names, function(m) {
          alive <- .ssh_pids_alive(.ssh_name_for(m), remaining_r[[m]])
          if (is.null(alive)) remaining_r[[m]] else remaining_r[[m]][alive]
        })
        names(remaining_r) <- machine_names
        remaining_r <- remaining_r[lengths(remaining_r) > 0L]
      }
      if (length(remaining_l) || length(remaining_r)) Sys.sleep(0.5)
    }
    n_left <- length(remaining_l) + sum(lengths(remaining_r))
    if (n_left > 0L)
      message("Note: ", n_left, " worker PID(s) still alive after 10s; ",
              "queue refresh may not fully reset the affected rows.")

    qps <- unique(out$queue_path[!is.na(out$queue_path)])
    qps <- qps[file.exists(qps)]
    gs_demoted <- 0L
    for (qp in qps) {
      tryCatch(tmuxRefreshQueueStatus(qp),
               error = function(e)
                 message("Could not refresh ", qp, ": ", conditionMessage(e)))

      # GS counterpart: if this queue has a sidecar `.ss_id` (written by
      # experimentFuture when ss_id was supplied), push the same demotion
      # to the Google Sheet for any RUNNING rows held by killed PIDs.
      ss_file <- paste0(qp, ".ss_id")
      if (file.exists(ss_file)) {
        ss_id <- tryCatch(trimws(readLines(ss_file, warn = FALSE)[1L]),
                          error = function(e) NA_character_)
        if (!is.na(ss_id) && nzchar(ss_id)) {
          pids_for_qp <- out$pid[out$queue_path == qp & !is.na(out$queue_path)]
          n <- tryCatch(.gs_demote_after_kill(ss_id, pids_for_qp),
                        error = function(e) {
                          message("Could not demote GS rows for ", qp,
                                  ": ", conditionMessage(e)); 0L
                        })
          gs_demoted <- gs_demoted + (n %||% 0L)
        }
      }
    }
    if (length(qps))
      message("Refreshed ", length(qps), " queue file(s); stale RUNNING rows ",
              "have been demoted to PENDING.")
    if (gs_demoted > 0L)
      message("Demoted ", gs_demoted, " RUNNING row(s) on associated Google Sheet(s).")
    return(invisible(out))
  }
  out
}


# Build hostname -> SSH-alias map by probing `ssh <core> hostname -s`.
# Returns a named character vector (hostname -> alias). Cores already
# matching the local node are skipped.  Unreachable cores produce no
# entry (silent skip -- caller falls back to using hostname verbatim).
.ef_build_host_map <- function(ef_list, local_node = Sys.info()[["nodename"]]) {
  if (is.null(ef_list) || !length(ef_list)) return(character(0))
  cores_all <- unique(unlist(lapply(ef_list, `[[`, "cores")))
  cores_all <- setdiff(cores_all, c("localhost", "127.0.0.1", local_node))
  host_map <- character(0)
  for (core in cores_all) {
    res <- tryCatch(
      suppressWarnings(system2(
        "ssh",
        c("-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
          core, "hostname -s"),
        stdout = TRUE, stderr = FALSE
      )),
      error = function(e) NULL
    )
    if (!is.null(res) && length(res) > 0L && nzchar(trimws(res[1L])))
      host_map[trimws(res[1L])] <- core
  }
  host_map
}

# Queue-mode worker scan used by experimentMonitor() (and analogous to
# the queue-driven discovery in experimentFutureList()).  Reads each
# queue file's RUNNING rows, verifies aliveness via /proc (local) or
# batched SSH (remote, using host_map for the alias lookup), and
# returns a data.frame with pid / machine / started_at / log_file /
# queue_path / runName.  With stats = TRUE, decorates with
# state / cpuAvg / RAM (GB) / availableCores / total RAM (GB).
.ef_monitor <- function(ef = NULL, queue_paths = NULL, stats = FALSE) {
  local_node <- Sys.info()[["nodename"]]
  ef_list <- NULL
  if (!is.null(ef)) {
    ef_list <- if (inherits(ef, "experimentFuture")) list(ef)
               else if (is.list(ef) &&
                        all(vapply(ef, inherits, logical(1L), "experimentFuture")))
                 ef
               else stop("`ef` must be an experimentFuture object or a list of them.",
                         call. = FALSE)
    queue_paths <- unique(c(queue_paths,
                            unlist(lapply(ef_list, `[[`, "queue_path"))))
  }
  queue_paths <- queue_paths[!is.na(queue_paths) & file.exists(queue_paths)]
  empty <- data.frame(pid = integer(0), machine = character(0),
                      started_at = character(0), log_file = character(0),
                      queue_path = character(0), runName = character(0),
                      stringsAsFactors = FALSE)
  if (!length(queue_paths)) return(empty)

  host_map <- .ef_build_host_map(ef_list, local_node)
  ssh_name_for <- function(m)
    if (m %in% names(host_map)) host_map[[m]] else m

  rows <- list()
  for (qp in queue_paths) {
    q <- tryCatch(readRDS(qp), error = function(e) NULL)
    if (is.null(q) ||
        !all(c("status", "process_id", "machine_name") %in% names(q))) next
    idx <- which(q$status == "RUNNING" &
                 !is.na(q$machine_name) &
                 !is.na(suppressWarnings(as.integer(q$process_id))))
    if (length(idx) == 0L) next
    data_cols <- setdiff(names(q), .future_meta_cols)
    for (m in unique(q$machine_name[idx])) {
      m_idx  <- idx[q$machine_name[idx] == m]
      m_pids <- as.integer(q$process_id[m_idx])
      alive <- if (m == local_node) {
        file.exists(paste0("/proc/", m_pids))
      } else {
        a <- .ssh_pids_alive(ssh_name_for(m), m_pids)
        if (is.null(a)) next
        a
      }
      # Resolve each live PID's stdout fd -> log file path.  Workers that
      # went through callr::r_bg(stdout = log_file) (the local
      # experimentFuture branch and the cluster runWorkerLoopFuture branch
      # post-2026-04 fix) have /proc/<pid>/fd/1 pointing at worker_NN.log.
      live_pids <- m_pids[alive]
      log_for <- setNames(rep(NA_character_, length(m_pids)), as.character(m_pids))
      if (length(live_pids)) {
        if (m == local_node) {
          for (p in live_pids) {
            fp <- tryCatch(Sys.readlink(sprintf("/proc/%d/fd/1", p)),
                           error = function(e) "", warning = function(w) "")
            if (length(fp) == 1L && nzchar(fp) && grepl("worker_[0-9]+\\.log$", fp))
              log_for[as.character(p)] <- fp
          }
        } else {
          fp <- .ssh_pids_log_file(ssh_name_for(m), live_pids)
          if (!is.null(fp)) {
            for (p in live_pids) {
              v <- fp[[as.character(p)]]
              if (!is.null(v) && nzchar(v) && grepl("worker_[0-9]+\\.log$", v))
                log_for[as.character(p)] <- v
            }
          }
        }
      }
      for (k in seq_along(m_pids)) {
        if (!isTRUE(alive[k])) next
        i <- m_idx[k]
        vals <- vapply(data_cols,
                       function(cn) as.character(q[[cn]][i]),
                       character(1L))
        rows[[length(rows) + 1L]] <- data.frame(
          pid        = m_pids[k],
          machine    = m,
          started_at = as.character(q$started_at[i]),
          log_file   = unname(log_for[as.character(m_pids[k])]),
          queue_path = qp,
          runName    = paste(vals, collapse = "-"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else empty
  if (isTRUE(stats) && nrow(out) > 0L)
    out <- .ef_attach_ps_stats(out, ssh_name_for, local_node)
  out
}

# Decorate a queue-mode worker frame with CPU / RSS / state / nproc /
# total RAM by calling the same .tmux_ps_stats helper used by
# tmuxListPanes(stats = TRUE), one batch per machine.
.ef_attach_ps_stats <- function(workers, ssh_name_for,
                                 local_node = Sys.info()[["nodename"]]) {
  workers$state               <- NA_character_
  workers$cpuAvg              <- NA_real_
  workers[["RAM (GB)"]]       <- NA_real_
  workers$availableCores      <- NA_integer_
  workers[["total RAM (GB)"]] <- NA_real_
  if (!nrow(workers)) return(workers)

  for (m in unique(workers$machine)) {
    idx    <- which(workers$machine == m)
    m_pids <- workers$pid[idx]
    target <- if (m == local_node) "__LOCAL__" else ssh_name_for(m)
    res    <- tryCatch(.tmux_ps_stats(target, m_pids), error = function(e) NULL)
    if (is.null(res)) next
    if (!is.na(res$ncpu))
      workers$availableCores[idx] <- res$ncpu
    if (!is.na(res$mem_kb))
      workers[["total RAM (GB)"]][idx] <- round(res$mem_kb / 1024^2, 1L)
    stats_df <- res$procs
    ix       <- match(m_pids, stats_df$pid)
    missing  <- is.na(ix)
    if (any(missing))
      workers$state[idx[missing]] <- "Closed"
    live <- !missing
    if (any(live)) {
      live_idx <- idx[live]
      live_ix  <- ix[live]
      workers$state[live_idx]         <- stats_df$state[live_ix]
      workers$cpuAvg[live_idx]        <- stats_df$cpu[live_ix]
      workers[["RAM (GB)"]][live_idx] <- round(stats_df$rss_mb[live_ix] / 1024, 1L)
    }
  }
  workers
}

# Batched remote PID-liveness check: one SSH connection per machine.
# Returns NULL if SSH is unreachable (treat as "leave alone"), or a logical
# vector aligned to `pids` otherwise.
.ssh_pids_alive <- function(machine, pids) {
  if (length(pids) == 0L) return(logical(0))
  pid_str <- paste(pids, collapse = " ")
  cmd <- paste0("for pid in ", pid_str,
                "; do [ -d /proc/$pid ] && echo alive || echo dead; done")
  res <- tryCatch(
    system2("ssh",
            c("-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
              machine, shQuote(cmd)),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NULL
  )
  if (is.null(res) || length(res) != length(pids)) return(NULL)
  res == "alive"
}

# Batched remote `readlink /proc/<pid>/fd/1` for each pid; one SSH connection.
# Returns a named character vector (pid as character -> resolved fd1 target),
# or NULL on SSH failure.  Empty/missing entries indicate no readable fd 1.
.ssh_pids_log_file <- function(machine, pids) {
  if (length(pids) == 0L) return(setNames(character(0), character(0)))
  pid_str <- paste(pids, collapse = " ")
  cmd <- paste0(
    "for pid in ", pid_str,
    "; do printf '%s\\t' \"$pid\"; readlink /proc/$pid/fd/1 2>/dev/null || echo; done"
  )
  res <- tryCatch(
    suppressWarnings(system2("ssh",
            c("-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
              machine, shQuote(cmd)),
            stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  out <- setNames(rep(NA_character_, length(pids)), as.character(pids))
  for (ln in res) {
    parts <- strsplit(ln, "\t", fixed = TRUE)[[1L]]
    if (length(parts) >= 1L && nzchar(parts[1L])) {
      out[[parts[1L]]] <- if (length(parts) >= 2L) trimws(parts[2L]) else NA_character_
    }
  }
  out
}

# Send `signal` to each pid on `machine`.  One SSH connection.
.ssh_kill_pids <- function(machine, pids, signal = "TERM") {
  if (length(pids) == 0L) return(invisible(NULL))
  pid_str <- paste(pids, collapse = " ")
  cmd <- paste0("kill -", signal, " ", pid_str, " 2>/dev/null || true")
  try(system2("ssh",
              c("-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
                machine, shQuote(cmd)),
              stdout = FALSE, stderr = FALSE),
      silent = TRUE)
  invisible(NULL)
}
