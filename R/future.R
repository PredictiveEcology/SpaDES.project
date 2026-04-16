# experimentFuture — tmux-free parallel job runner
#
# Mirrors experimentTmux in capabilities (same GoogleSheets queue, same remote
# machine setup, same global.R sourcing model) but replaces tmux panes with
# background R processes.  All worker output is captured to per-worker log
# files on localhost via callr::r_bg(), enabling true streaming logs.

# ── Internal constants ─────────────────────────────────────────────────────

.future_meta_cols <- c(
  "status", "claimed_by", "started_at", "finished_at",
  "DEoptimElapsedTime", "machine_name", "process_id",
  "heartbeat_at", "heartbeat_iter", "iterationsTotal", "interrupted_at"
)

# ── experimentFuture ───────────────────────────────────────────────────────

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
#' ## ── Minimal local example ──────────────────────────────────────────────
#'
#' # Each row is one job; column names become variables in .GlobalEnv before
#' # global.R is sourced.
#' expt <- data.frame(.scenario = c("A", "B", "C", "D"), .rep = 1:4)
#'
#' # global.R uses the variables from the queue row, e.g.:
#' #   message("Running scenario: ", .scenario, " rep: ", .rep)
#' #   Sys.sleep(2)
#'
#' ef <- experimentFuture(
#'   df          = expt,
#'   global_path = file.path(getwd(), "global.R"),
#'   n_workers   = 2L,
#'   queue_path  = file.path(getwd(), "future_queue.rds"),
#'   log_dir     = file.path(getwd(), "logs")
#' )
#'
#' print(ef)                    # live status (alive/done per worker)
#' awaitExperimentFuture(ef)    # block until all jobs are done
#'
#' q <- readRDS(ef$queue_path)
#' table(q$status)              # should be all DONE
#'
#' cat(readLines(ef$log_files[[1]]), sep = "\n")   # inspect worker 1 log
#'
#' ## ── Remote workers (pre-setup required) ───────────────────────────────
#' ef <- experimentFuture(
#'   df             = expt,
#'   global_path    = file.path(getwd(), "global.R"),
#'   cores          = c("node01", "node02"),
#'   n_workers      = 2L,
#'   ss_id          = "YOUR_GOOGLE_SHEET_ID",
#'   email          = "you@example.com",
#'   cache_path     = "~/.cache/gargle",
#'   local_pat_file = "~/.github_pat"
#' )
#' }
#'
#' @seealso \code{\link{experimentTmux}}, \code{\link{awaitExperimentFuture}},
#'   \code{\link{runWorkerLoop}}
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
  ...
) {

  if (!requireNamespace("callr", quietly = TRUE))
    stop("Package 'callr' is required. Install with: install.packages('callr')",
         call. = FALSE)

  on_interrupt <- match.arg(on_interrupt)

  # ── 1. Normalize paths ──────────────────────────────────────────────────
  global_path <- normalizePath(global_path, mustWork = FALSE)
  if (is.null(queue_path))
    queue_path <- file.path(dirname(global_path), "future_queue.rds")
  queue_path <- normalizePath(queue_path, mustWork = FALSE)
  log_dir    <- normalizePath(log_dir,    mustWork = FALSE)

  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  activeRunningPath <- activeRunningPathForTmux(
    activeRunningPath = activeRunningPath, queue_path
  )

  # ── 2. Save ... args so workers can load complex objects ─────────────────
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

  # ── 3. Initialize local queue ────────────────────────────────────────────
  if (!file.exists(queue_path)) {
    if (missing(df) || is.null(df))
      stop("'df' must be provided when 'queue_path' does not yet exist.",
           call. = FALSE)
    tmux_prepare_queue_from_df(df, queue_path)
  }

  # ── 4. Google Sheets initialization (mirrors experimentTmux) ─────────────
  if (!is.null(ss_id)) {
    if (!is.null(email))      options(gargle_oauth_email = email)
    if (!is.null(cache_path)) options(gargle_oauth_cache = cache_path)
    options(gargle_oauth_client_type = "web")
  }

  if (!is.null(queue_path) && !is.null(ss_id)) {
    # Resolve Drive folder → sheet ID
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

    # Sync local queue ↔ GS
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
      q <- data.table::setDT(gs_q)
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

  # ── 5. Remote machine setup ───────────────────────────────────────────────
  unique_hosts <- if (!is.null(cores)) {
    setdiff(unique(cores), c("localhost", "127.0.0.1", Sys.info()[["nodename"]]))
  } else {
    character(0)
  }

  if (length(unique_hosts) > 0L) {
    message("Setting up ", length(unique_hosts), " remote host(s): ",
            paste(unique_hosts, collapse = ", "))
    for (host in unique_hosts) {
      .setup_remote_machine(
        host           = host,
        global_path    = global_path,
        queue_path     = queue_path,
        cache_path     = cache_path,
        sp_dev_path    = sp_dev_path,
        local_pat_file = local_pat_file
      )
    }
    message("Remote setup complete.")
  }

  # ── 6. Determine effective worker hosts ───────────────────────────────────
  is_local <- is.null(cores) ||
              all(cores %in% c("localhost", "127.0.0.1", Sys.info()[["nodename"]]))

  # ── 7. Build log file paths ────────────────────────────────────────────────
  log_files <- file.path(log_dir, sprintf("worker_%02d.log", seq_len(n_workers)))

  # ── 8. Launch workers ─────────────────────────────────────────────────────
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
                        dots_path, lib_paths) {
          .libPaths(lib_paths)
          SpaDES.project::runWorkerLoop(
            queue_path        = queue_path,
            global_path       = global_path,
            on_interrupt      = on_interrupt,
            ss_id             = ss_id,
            email             = email,
            cache_path        = cache_path,
            runNameLabel      = runNameLabel,
            activeRunningPath = activeRunningPath,
            dots_path         = dots_path,
            pane_mode         = "reuse"
          )
        },
        args    = c(worker_args, list(lib_paths = .libPaths())),
        stdout  = log_files[[i]],
        stderr  = log_files[[i]],
        # Unset TMUX/TMUX_PANE so workers don't emit OSC 2 escape bytes into log files.
        # callr workers inherit the parent's environment (including tmux vars), but
        # they have no tmux pane — their output goes to a file, not a terminal.
        env     = c(TMUX = "", TMUX_PANE = ""),
        package = TRUE
      )
    }
  } else {
    # Remote workers via future::cluster (SSH-based PSOCK cluster).
    # Logs are written on the remote machine — documented limitation.
    if (!requireNamespace("future",     quietly = TRUE) ||
        !requireNamespace("parallelly", quietly = TRUE))
      stop("Packages 'future' and 'parallelly' are required for remote workers.",
           call. = FALSE)
    cl <- parallelly::makeClusterPSOCK(cores)
    future::plan(future::cluster, workers = cl)
    .fn <- runWorkerLoopFuture   # captured so future serializes it
    for (i in seq_len(n_workers)) {
      lf <- log_files[[i]]
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
          log_file          = lf
        )
      })
    }
  }

  # ── 9. Emit log-watching instructions ────────────────────────────────────
  log_cmds <- paste0("  tail -f ", log_files, "   # worker ", seq_len(n_workers),
                     collapse = "\n")
  message(
    "\nWorkers are running. To watch live logs, run in a separate shell:\n",
    log_cmds, "\n",
    "  -- or --\n",
    "  tail -f ", log_dir, "/worker_*.log\n"
  )

  # ── 10. tmux log-tail panes (if already inside tmux) ─────────────────────
  if (nzchar(Sys.getenv("TMUX"))) {
    tryCatch({
      target_win <- .tmux_current_window()
      system("tmux set -g pane-border-status top", ignore.stdout = TRUE, ignore.stderr = TRUE)
      for (i in seq_len(n_workers)) {
        host_label <- if (!is.null(cores) && length(cores) >= i) cores[[i]] else "localhost"
        pane_title <- sprintf("Log-Worker-%02d [%s]", i, host_label)
        new_id <- .tmux_out("split-window", "-d", "-v", "-t", target_win,
                            "-P", "-F", "#{pane_id}")
        if (length(new_id) > 0L) {
          .tmux_run("select-layout", "-t", target_win, "tiled")
          .tmux_run("select-pane",   "-t", new_id[1L], "-T", pane_title)
          .tmux_run("send-keys",     "-t", new_id[1L],
                    paste0("tail -f ", shQuote(log_files[[i]])), "C-m")
        }
      }
      message("Opened ", n_workers, " log-tail pane(s) in the current tmux window.")
    }, error = function(e) {
      message("Could not open tmux log panes: ", conditionMessage(e))
    })
  }

  # ── Return handle ─────────────────────────────────────────────────────────
  structure(
    list(
      procs      = procs,
      log_files  = log_files,
      log_dir    = log_dir,
      queue_path = queue_path,
      cores      = cores,
      is_local   = is_local
    ),
    class = "experimentFuture"
  )
}


# ── runWorkerLoopFuture ─────────────────────────────────────────────────────

#' Worker loop for future/cluster-based remote execution
#'
#' @description
#' A thin wrapper around \code{\link{runWorkerLoop}} that optionally redirects
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
  log_file          = NULL
) {
  on_interrupt <- match.arg(on_interrupt)

  # Redirect output to log file (used for remote cluster workers)
  if (!is.null(log_file)) {
    log_con <- file(log_file, open = "wt")
    sink(log_con, append = TRUE)
    sink(log_con, type = "message", append = TRUE)
    on.exit({
      try(sink(NULL),                  silent = TRUE)
      try(sink(NULL, type = "message"), silent = TRUE)
      try(close(log_con),              silent = TRUE)
    }, add = TRUE)
  }

  worker_id <- paste0(Sys.info()[["nodename"]], "-", Sys.getpid())
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Worker ", worker_id, " starting.")

  SpaDES.project::runWorkerLoop(
    queue_path        = queue_path,
    global_path       = global_path,
    on_interrupt      = on_interrupt,
    ss_id             = ss_id,
    email             = email,
    cache_path        = cache_path,
    runNameLabel      = runNameLabel,
    activeRunningPath = activeRunningPath,
    dots_path         = dots_path,
    pane_mode         = "reuse"
  )

  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Worker ", worker_id, " finished.")
  invisible(worker_id)
}


# ── S3 methods ──────────────────────────────────────────────────────────────

#' @export
print.experimentFuture <- function(x, ...) {
  n     <- length(x$procs)
  width <- nchar(as.character(n))
  cat(sprintf("experimentFuture — %d worker(s)\n", n))
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


# ── awaitExperimentFuture ───────────────────────────────────────────────────

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
