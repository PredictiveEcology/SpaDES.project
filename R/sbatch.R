# experimentSBATCH  --  Slurm-based parallel job runner
#
# Mirrors experimentFuture in capabilities (same shared queue, same global.R
# sourcing model, same runNameLabel / statusCalculate semantics) but replaces
# the local-callr / SSH-cluster worker pool with N independent SBATCH jobs.
# Each Slurm job runs the same tmuxRunWorkerLoop, polling the shared queue
# (RDS or Google Sheets) until empty (or its stop file appears).

# -- experimentSBATCH --------------------------------------------------------

#' Run parallel R jobs on a Slurm cluster (SBATCH-based)
#'
#' @description
#' A Slurm-native sibling of \code{\link{experimentTmux}} and
#' \code{\link{experimentFuture}}.  Submits \code{n_workers} long-lived SBATCH
#' jobs that each call \code{\link{tmuxRunWorkerLoop}} against the shared
#' queue, claiming and running rows until the queue is empty (or the
#' worker's stop file appears).  Same queue / \code{global.R} / \code{runNameLabel}
#' / \code{statusCalculate} semantics as the other two runners.
#'
#' Returns a non-blocking handle of class \code{"experimentSBATCH"} carrying
#' the Slurm job IDs.  Use \code{\link{awaitExperimentSBATCH}} to poll
#' \code{squeue} until all jobs leave the queue, or
#' \code{\link{killExperimentSBATCH}} to stop them (gracefully via stop files,
#' or immediately via \code{scancel}).
#'
#' @param df data.frame of parameter combinations.  Each row is one job.
#'   Ignored if \code{queue_path} already exists (workers resume from the
#'   existing queue).  Optional columns \code{.group}, \code{.after} and
#'   \code{.afterType} split the rows into groups that run one after another, each
#'   with its own jobs, in this one call; see \code{\link{experimentGroups}}.
#' @param global_path Path to the R script each worker sources per job.
#'   Defaults to \code{"global.R"} in the current directory.  Must be on a
#'   filesystem visible to compute nodes (e.g. shared NFS / Lustre).
#' @param n_workers Number of SBATCH jobs to submit.  Defaults to \code{4L}.  With
#'   \code{.group} in the queue, the jobs of each group: a named vector
#'   (\code{c(fit = 1, 4)}) gives a group's own number, the unnamed entry the rest; at
#'   most one job per row still to run.  See \code{\link{experimentGroups}}.
#' @param queue_path Path to the local RDS queue file.  Created automatically
#'   if it does not yet exist.  Defaults to
#'   \code{<dirname(global_path)>/sbatch_queue.rds}.  Must be on shared
#'   storage so all worker nodes can read/write it.
#' @param on_interrupt \code{"requeue"} (default) re-queues interrupted jobs
#'   as PENDING; \code{"fail"} marks them INTERRUPTED (still claimable) and stops the
#'   worker. A job that errors is marked ERROR, which no worker claims until the next
#'   launch on this queue (see [experimentStatus]).
#' @param on_error \code{"continue"} (default) goes on to the next claimable job after a
#'   job errors; \code{"stop"} ends the worker there, so the rows it would have run stay
#'   PENDING with no worker. A batch job has no prompt to continue from, so
#'   \code{"continue"} is the default here; the job still exits with status 1 when any
#'   of its rows errored. The errored row is marked ERROR either way.
#' @param ss_id Google Sheets ID (or Drive folder ID) for the shared queue.
#'   When provided workers use the GS backend in addition to the local RDS
#'   file (mirroring \code{experimentFuture}).
#' @param forceLocalQueueToGS If \code{TRUE}, overwrite the GS sheet with the
#'   local \code{df} even if the sheet already contains rows.
#' @param email Gargle OAuth e-mail for Google Sheets auth (only used when
#'   \code{ss_id} is non-\code{NULL}).
#' @param cache_path Gargle OAuth cache directory.
#' @param runNameLabel Quoted expression evaluated in the job environment to
#'   derive a human-readable run name (used in log messages and queue
#'   metadata).  Defaults to \code{quote(colnames(q)[1:2])}.
#' @param log_dir Directory for per-worker log files, generated job scripts,
#'   and stop files.  Created if needed.  Defaults to \code{"logs"} relative
#'   to the current working directory.  Must be on shared storage.
#' @param activeRunningPath Directory for \code{Running_*.rds} marker files
#'   (file-based backend only).
#' @param sbatch_opts Named list of SBATCH directives.  Each \code{name = value}
#'   becomes \code{#SBATCH --<name>=<value>} in the generated job script.
#'   Underscores in names are translated to hyphens, so
#'   \code{cpus_per_task = 4} becomes \code{#SBATCH --cpus-per-task=4}.
#'   Set a value to \code{NULL} or \code{TRUE} for flag-only directives
#'   (\code{#SBATCH --<name>}).  Common keys: \code{partition}, \code{time},
#'   \code{mem}, \code{cpus_per_task}, \code{account}, \code{nodes},
#'   \code{ntasks_per_node}, \code{constraint}, \code{gres}.  With \code{.group} in
#'   the queue, a list of lists: an unnamed one for every group, then lists named by
#'   group that add to or replace it (\code{list(list(partition = "standard"),
#'   fit = list(mem = "300G"))}).  A group with \code{.after} also gets
#'   \code{--dependency=<.afterType>:<job ids>}.
#' @param sbatch_cmd Path to the \code{sbatch} executable.  Defaults to
#'   \code{"sbatch"} (resolved on \code{$PATH}).  Override on systems where
#'   \code{sbatch} is wrapped or non-standard.
#' @param r_cmd Path to the R interpreter to invoke on compute nodes.
#'   Defaults to \code{file.path(R.home("bin"), "Rscript")}.
#' @param r_libs Character vector of library paths to set via \code{.libPaths()}
#'   inside each worker.  Defaults to the master's \code{.libPaths()} so the
#'   worker sees the same package set; override when compute nodes have a
#'   different filesystem layout.
#' @param dry_run If \code{TRUE}, generate the job scripts but do not submit
#'   them.  Returns a handle whose \code{job_ids} are all \code{NA}.  Useful
#'   for inspecting what would be submitted.
#' @param ... Additional named arguments stored in \code{.sbatch_dots.rds} and
#'   loaded into each worker's \code{.GlobalEnv} before sourcing
#'   \code{global_path}.
#'
#' @return An object of class \code{"experimentSBATCH"} (a list) containing:
#'   \describe{
#'     \item{\code{job_ids}}{Integer vector of Slurm job IDs (or \code{NA}
#'       under \code{dry_run = TRUE}).}
#'     \item{\code{job_scripts}}{Character vector of generated SBATCH script
#'       paths.}
#'     \item{\code{log_files}}{Character vector of log file paths.}
#'     \item{\code{stop_files}}{Character vector of stop-file paths.}
#'     \item{\code{log_dir}}{Absolute path to the log directory.}
#'     \item{\code{queue_path}}{Absolute path to the queue RDS file.}
#'     \item{\code{groups}}{Each job's queue group (\code{NA} without \code{.group}).}
#'   }
#'
#' @examples
#' \dontrun{
#' ## -- Minimal: build a tiny global.R, then run a 2 x 2 experiment ---------
#' # Use a directory on your shared HPC filesystem (NFS / Lustre / BeeGFS).
#' tdir <- file.path(tempdir(), "experimentSBATCH-demo")
#' dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
#' writeLines(
#'   'message("scenario=", .scenario, " rep=", .rep); Sys.sleep(2)',
#'   file.path(tdir, "global.R")
#' )
#' expt <- expand.grid(.scenario = c("A", "B"), .rep = 1:2,
#'                     stringsAsFactors = FALSE)
#'
#' es <- experimentSBATCH(
#'   df          = expt,
#'   global_path = file.path(tdir, "global.R"),
#'   n_workers   = 2L,
#'   queue_path  = file.path(tdir, "sbatch_queue.rds"),
#'   log_dir     = file.path(tdir, "logs"),
#'   sbatch_opts = list(partition = "compute", time = "00:30:00", mem = "1G")
#' )
#'
#' ## -- Live inspection while jobs run --------------------------------------
#' print(es)                                          # job IDs + squeue status
#' queueRead(es$queue_path)                           # full queue snapshot
#' experimentMonitor(queue_paths = es$queue_path)     # cluster-wide pid + machine
#' experimentMonitor(queue_paths = es$queue_path, stats = TRUE)  # + CPU/RAM
#'
#' awaitExperimentSBATCH(es)                          # block until squeue empty
#'
#' ## -- Larger experiment with full sbatch_opts ------------------------------
#' es <- experimentSBATCH(
#'   df          = expt,
#'   global_path = file.path(tdir, "global.R"),
#'   n_workers   = 4L,
#'   queue_path  = file.path(tdir, "sbatch_queue.rds"),
#'   log_dir     = file.path(tdir, "logs"),
#'   sbatch_opts = list(
#'     partition     = "compute",
#'     time          = "24:00:00",
#'     mem           = "16G",
#'     cpus_per_task = 4,
#'     account       = "my_alloc"
#'   )
#' )
#'
#' print(es)                       # job IDs + squeue status per worker
#' awaitExperimentSBATCH(es)        # blocks until every job ID leaves squeue
#'
#' # Graceful stop (workers exit between jobs, queue rows stay PENDING):
#' killExperimentSBATCH(es)
#'
#' # Immediate stop (scancel; stale RUNNING entries can be cleaned up via:
#' #   tmuxRefreshQueueStatus(es$queue_path)):
#' killExperimentSBATCH(es, force = TRUE)
#' tmuxRefreshQueueStatus(es$queue_path)
#'
#' ## -- Resume after stop ---------------------------------------------------
#' # Same `queue_path` -> DONE rows are skipped; HOLD and ERROR rows go back to PENDING,
#' # RUNNING rows of dead workers to INTERRUPTED, and the new sbatch jobs claim them.
#' es2 <- experimentSBATCH(
#'   df          = expt,                # ignored if queue exists
#'   global_path = file.path(tdir, "global.R"),
#'   n_workers   = 2L,
#'   queue_path  = file.path(tdir, "sbatch_queue.rds"),
#'   log_dir     = file.path(tdir, "logs"),
#'   sbatch_opts = list(partition = "compute", time = "00:30:00", mem = "1G")
#' )
#' awaitExperimentSBATCH(es2)
#' table(queueRead(es2$queue_path)$status)            # all DONE
#' }
#'
#' @seealso \code{\link{experimentTmux}}, \code{\link{experimentFuture}},
#'   \code{\link{awaitExperimentSBATCH}}, \code{\link{killExperimentSBATCH}}
#' @export
#' @param sync_library `NULL` (default) or a character vector of package
#'   specifications for `Require::Install()`. If given, [syncProjectLibrary()] runs
#'   once, now, in a fresh process and only while no worker is alive, before any
#'   worker starts. Never sync per job.
#' @param snapshot_library Logical (default `TRUE`). Each worker session takes a
#'   hardlinked snapshot of the project library before loading anything from it,
#'   so an install into the shared library cannot corrupt a running job. See
#'   [librarySnapshot].
#' @inheritParams .workerDevLibrary
experimentSBATCH <- function(
  df,
  global_path         = "global.R",
  n_workers           = 4L,
  queue_path          = NULL,
  on_interrupt        = c("requeue", "fail"),
  on_error            = c("continue", "stop"),
  ss_id               = NULL,
  forceLocalQueueToGS = FALSE,
  email               = getOption("gargle_oauth_email"),
  cache_path          = getOption("gargle_oauth_cache"),
  runNameLabel        = quote(colnames(q)[1:2]),
  log_dir             = "logs",
  activeRunningPath   = getOption("spades.activeRunningPath"),
  sbatch_opts         = list(),
  sbatch_cmd          = "sbatch",
  r_cmd               = file.path(R.home("bin"), "Rscript"),
  r_libs              = .libPaths(),
  dry_run             = FALSE,
  sync_library        = NULL,
  snapshot_library    = TRUE,
  devPackages         = NULL,
  devInstallFun       = NULL,
  ...
) {
  on_interrupt <- match.arg(on_interrupt)
  on_error     <- match.arg(on_error)

  # -- 1. Normalize paths ----------------------------------------------------
  global_path <- normalizePath(global_path, mustWork = FALSE)
  if (is.null(queue_path))
    queue_path <- file.path(dirname(global_path), "sbatch_queue.rds")
  queue_path <- normalizePath(queue_path, mustWork = FALSE)
  log_dir    <- normalizePath(log_dir,    mustWork = FALSE)
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  activeRunningPath <- tmuxActiveRunningPath(
    activeRunningPath = activeRunningPath, queue_path
  )
  if (!is.null(sync_library))
    syncProjectLibrary(sync_library, activeRunningPath = activeRunningPath)
  dev_library <- .workerDevLibrary(devPackages, devInstallFun) # built now, before any job; not written to
  if (!is.null(dev_library)) snapshot_library <- FALSE

  # -- 2. Save ... args so workers can load complex objects ------------------
  dots_path <- file.path(dirname(queue_path), ".sbatch_dots.rds")
  dots      <- list(...)
  if (length(dots) > 0L) {
    saveRDS(dots, dots_path)
  } else if (file.exists(dots_path)) {
    unlink(dots_path)
    dots_path <- NULL
  } else {
    dots_path <- NULL
  }

  # -- 3. Initialize local queue ---------------------------------------------
  if (!file.exists(queue_path)) {
    if (missing(df) || is.null(df))
      stop("'df' must be provided when 'queue_path' does not yet exist.",
           call. = FALSE)
    tmuxPrepareQueueFromDF(df, queue_path)
  }

  # -- 4. Google Sheets initialization (mirrors experimentFuture) ------------
  if (!is.null(ss_id)) {
    if (!is.null(email))      options(gargle_oauth_email = email)
    if (!is.null(cache_path)) options(gargle_oauth_cache = cache_path)
    options(gargle_oauth_client_type = "web")
  }

  if (!is.null(queue_path) && !is.null(ss_id)) {
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
    gs_q <- try(.gs_read_queue(ss_id), silent = TRUE)
    if (!inherits(gs_q, "try-error") && nrow(gs_q) > 0L && isFALSE(forceLocalQueueToGS)) {
      if (!missing(df) && !is.null(df)) {
        gs_names_reverted <- names(revertDotNames(data.table::copy(data.table::setDT(gs_q))))
        gs_data_cols  <- setdiff(gs_names_reverted, meta_cols)
        df_data_cols  <- setdiff(names(df),         meta_cols)
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
      .queue_write(q, queue_path)
    }
  }
  # A launch runs what is left: HOLD and ERROR rows back to PENDING, RUNNING rows of dead
  # workers back in the queue (see ?experimentStatus); then the sheet, if there is one, gets
  # the queue the workers will claim from.
  q <- .queue_modify(queue_path, .queueAtLaunch)
  if (!is.null(ss_id))
    tryCatch(.gs_push_queue(ss_id, q),
             error = function(e) warning("Could not push the queue to the Google Sheet (",
                                         ss_id, "): ", conditionMessage(e), call. = FALSE))

  # -- 5. Workers: per group (see ?experimentGroups), or n_workers for the whole queue ----
  groups <- .queueGroups(.queue_read(queue_path))
  worker_groups <- .workerGroups(n_workers, groups, pool = FALSE)
  n_workers <- length(worker_groups)
  idx          <- .indexInGroup(worker_groups)
  log_files    <- .workerFiles(log_dir, worker_groups, ".log")
  stop_files   <- .workerFiles(log_dir, worker_groups, ".stop")
  job_scripts  <- .workerFiles(log_dir, worker_groups, ".sh")
  for (sf in stop_files) if (file.exists(sf)) unlink(sf)
  opts <- .sbatchGroupOpts(sbatch_opts, if (is.null(groups)) "" else groups$group) # "": no groups

  if (!isTRUE(dry_run) && Sys.which(sbatch_cmd) == "" && !file.exists(sbatch_cmd))
    stop("Could not find sbatch executable: '", sbatch_cmd,
         "'. Set sbatch_cmd or run on a Slurm-enabled host.", call. = FALSE)

  # -- 6./7. Per group, in dependency order: write its SBATCH scripts, then submit them,
  # so the groups after it can name its job ids in --dependency ------------------------
  job_ids <- rep(NA_integer_, n_workers)
  for (g in unique(worker_groups)) {
    w <- which(worker_groups %in% g)
    o <- opts[[if (is.na(g)) 1L else g]]
    if (!is.na(g)) {
      dep <- .sbatchDependency(groups, g, worker_groups, job_ids)
      if (nzchar(dep)) o$dependency <- paste(c(o$dependency, dep), collapse = ",")
    }
    # Workers that share a node: as many as one node's tasks (--ntasks-per-node),
    # at most all of them; 1 when the user did not say.
    nWorkersOnNode <- min(length(w), max(1L, suppressWarnings(as.integer(
      c(o[["ntasks-per-node"]], o[["ntasks_per_node"]])[1L])), na.rm = TRUE))
    for (i in w) {
      .sbatch_write_script(
        script_path       = job_scripts[[i]],
        worker_idx        = idx[i],
        log_file          = log_files[[i]],
        stop_file         = stop_files[[i]],
        queue_path        = queue_path,
        global_path       = global_path,
        on_interrupt      = on_interrupt,
        on_error          = on_error,
        ss_id             = ss_id,
        email             = email,
        cache_path        = cache_path,
        runNameLabel      = runNameLabel,
        activeRunningPath = activeRunningPath,
        dots_path         = dots_path,
        sbatch_opts       = o,
        r_cmd             = r_cmd,
        r_libs            = r_libs,
        snapshot_library  = snapshot_library,
        dev_library       = dev_library,
        n_workers_on_host = nWorkersOnNode,
        group             = g
      )
      if (!isTRUE(dry_run)) {
        out <- system2(sbatch_cmd, args = job_scripts[[i]], stdout = TRUE, stderr = TRUE)
        jid <- .sbatch_parse_jobid(out)
        if (is.na(jid))
          stop("Failed to parse Slurm job ID from sbatch output:\n",
               paste(out, collapse = "\n"), call. = FALSE)
        job_ids[i] <- jid
      }
    }
  }
  if (isTRUE(dry_run)) {
    message("dry_run = TRUE; ", n_workers, " job script(s) generated in ",
            log_dir, ", not submitted.")
  } else {
    message("Submitted ", n_workers, " SBATCH job(s): ",
            paste(job_ids, collapse = ", "))
  }
  clusters <- unique(unlist(lapply(opts, `[[`, "clusters")))

  # -- 8. Emit log-watching instructions -------------------------------------
  log_cmds <- paste0("  tail -f ", log_files, "   # worker ", seq_len(n_workers),
                     collapse = "\n")
  message(
    "\nWorkers will write logs to:\n", log_cmds, "\n",
    "  -- or --\n  tail -f ", log_dir, "/worker_*.log\n",
    if (!isTRUE(dry_run)) paste0("Live Slurm status:  ",
                                 paste(c("squeue", .sbatch_cluster_args(clusters),
                                         "-j", paste(job_ids, collapse = ",")),
                                       collapse = " "), "\n") else ""
  )

  structure(
    list(
      job_ids     = job_ids,
      job_scripts = job_scripts,
      log_files   = log_files,
      stop_files  = stop_files,
      log_dir     = log_dir,
      queue_path  = queue_path,
      sbatch_cmd  = sbatch_cmd,
      clusters    = clusters,
      groups      = worker_groups
    ),
    class = "experimentSBATCH"
  )
}


# -- Internal: build one SBATCH job script -----------------------------------

.sbatch_write_script <- function(script_path, worker_idx, log_file, stop_file,
                                 queue_path, global_path, on_interrupt, ss_id,
                                 email, cache_path, runNameLabel,
                                 activeRunningPath, dots_path,
                                 sbatch_opts, r_cmd, r_libs, snapshot_library = TRUE,
                                 dev_library = NULL, on_error = "stop", n_workers_on_host = 1L,
                                 group = NULL) {
  # SBATCH directives -- always set output / error / job-name; user-provided
  # opts come after and may NOT override these.
  reserved <- c("output", "error", "job-name", "job_name")
  user_dirs <- character(0L)
  for (nm in names(sbatch_opts)) {
    nm_norm <- gsub("_", "-", nm)
    if (tolower(nm_norm) %in% reserved) next
    val <- sbatch_opts[[nm]]
    if (is.null(val) || isTRUE(val)) {
      user_dirs <- c(user_dirs, paste0("#SBATCH --", nm_norm))
    } else {
      user_dirs <- c(user_dirs, sprintf("#SBATCH --%s=%s", nm_norm, as.character(val)))
    }
  }
  reserved_dirs <- c(
    sprintf("#SBATCH --job-name=spades-%s-%02d",
            if (length(group) && !is.na(group)) .groupFileName(group) else "worker", worker_idx),
    sprintf("#SBATCH --output=%s",                   log_file),
    sprintf("#SBATCH --error=%s",                    log_file)
  )

  # R worker invocation -- a single Rscript -e expression. All args serialized
  # via deparse() so the worker recreates them exactly (NULL stays NULL,
  # quote(...) stays a call, paths stay character).
  # hardlinked per-job library (see ?librarySnapshot); pasted, not sprintf-ed,
  # because the snippet contains a deparsed function
  snap_code <- if (isTRUE(snapshot_library) && !is.null(activeRunningPath))
    .librarySnapshotCode(r_libs[1L], activeRunningPath) else ""
  r_expr <- sprintf(
    paste0(
      ".libPaths(%s); %s",
      "SpaDES.project::tmuxRunWorkerLoop(",
      "queue_path = %s, global_path = %s, on_interrupt = %s, on_error = %s, ",
      "ss_id = %s, email = %s, cache_path = %s, ",
      "runNameLabel = %s, activeRunningPath = %s, ",
      "dots_path = %s, stop_file = %s, pane_mode = \"reuse\", ",
      "n_workers_on_host = %s, exitOnError = TRUE, afterGroupsEnded = TRUE%s)"
    ),
    .deparse_one(r_libs), .devLibraryStartup(dev_library),
    .deparse_one(queue_path),  .deparse_one(global_path), .deparse_one(on_interrupt), .deparse_one(on_error),
    .deparse_one(ss_id),       .deparse_one(email),       .deparse_one(cache_path),
    .deparse_one(runNameLabel),.deparse_one(activeRunningPath),
    .deparse_one(dots_path),   .deparse_one(stop_file),
    .deparse_one(as.integer(n_workers_on_host)), .groupArg(group, ", group = %s")
  )
  r_expr <- sub("^(.libPaths\\([^;]*\\); )", paste0("\\1", gsub("\\\\", "\\\\\\\\", snap_code)), r_expr)

  body <- c(
    "#!/bin/bash",
    reserved_dirs,
    user_dirs,
    "",
    "set -euo pipefail",
    "ulimit -n \"$(ulimit -Hn)\" || true", # the job's open-file soft limit is the login shell's (1024 on the GPSC): terra runs died at it
    sprintf("export %s=%s", names(.workerEnv), .workerEnv),
    "",
    sprintf("%s -e %s", shQuote(r_cmd), shQuote(r_expr))
  )
  writeLines(body, script_path)
  Sys.chmod(script_path, "0755")
  invisible(script_path)
}

## `sbatch_opts` per group (see ?experimentGroups): a list whose elements are all lists is
## the unnamed default followed by lists named by group, which add to or replace it; any
## other list is every group's options. `groups = ""` (a queue without groups): the default.
.sbatchGroupOpts <- function(sbatch_opts, groups) {
  if (!length(sbatch_opts) || !all(vapply(sbatch_opts, is.list, logical(1))))
    return(stats::setNames(rep(list(sbatch_opts), length(groups)), groups))
  nms <- names(sbatch_opts)
  if (is.null(nms)) nms <- rep("", length(sbatch_opts))
  unknown <- setdiff(nms[nzchar(nms)], groups[nzchar(groups)])
  if (length(unknown))
    stop("`sbatch_opts` names group(s) not in the queue: ", paste(unknown, collapse = ", "), call. = FALSE)
  if (sum(!nzchar(nms)) > 1L)
    stop("`sbatch_opts` may have only one unnamed list (the default).", call. = FALSE)
  default <- if (any(!nzchar(nms))) sbatch_opts[[which(!nzchar(nms))]] else list()
  stats::setNames(lapply(groups, function(g) {
    o <- if (nzchar(g) && g %in% nms) sbatch_opts[[g]] else list()
    c(default[setdiff(names(default), names(o))], o) # not modifyList(): a NULL value is a flag
  }), groups)
}

## --dependency for group `g`: its .afterType and the job ids of the workers of its .after
## groups ("" when none of them has a job: they have nothing left to run). Ids not yet known
## (dry run) are written as <group>.
.sbatchDependency <- function(groups, g, worker_groups, job_ids) {
  k <- match(g, groups$group)
  w <- which(worker_groups %in% groups$after[[k]])
  if (!length(w)) return("")
  ids <- ifelse(is.na(job_ids[w]), paste0("<", worker_groups[w], ">"), job_ids[w])
  paste0(groups$afterType[k], ":", paste(unique(ids), collapse = ":"))
}

.deparse_one <- function(x) {
  txt <- paste(deparse(x, control = c("keepInteger", "keepNA", "niceNames")),
               collapse = " ")
  # is.language(x) is TRUE for calls/symbols; we need quote(...) to round-trip
  # them through Rscript -e (otherwise they'd evaluate immediately on the worker).
  if (is.language(x) && !is.character(x)) sprintf("quote(%s)", txt) else txt
}

.sbatch_parse_jobid <- function(out) {
  # sbatch prints "Submitted batch job 12345", and on multi-cluster sites
  # "Submitted batch job 12345 on cluster gpsc8"
  m <- regmatches(out, regexpr("Submitted batch job\\s+[0-9]+", out))
  if (length(m) == 0L) return(NA_integer_)
  suppressWarnings(as.integer(sub("^\\D+", "", m[[1L]])))
}


# -- S3 methods ---------------------------------------------------------------

#' @export
print.experimentSBATCH <- function(x, ...) {
  n     <- length(x$job_ids)
  width <- nchar(as.character(n))
  cat(sprintf("experimentSBATCH  --  %d worker(s)\n", n))
  cat(sprintf("  Queue  : %s\n", x$queue_path))
  cat(sprintf("  Log dir: %s\n", x$log_dir))
  alive <- .sbatch_squeue_alive(x$job_ids, x$clusters)
  for (i in seq_len(n)) {
    jid <- x$job_ids[[i]]
    status <- if (is.na(jid))         "dry-run"
              else if (isTRUE(alive[[i]])) "in-queue "
              else if (isFALSE(alive[[i]])) "DONE     "
              else                          "unknown  "
    cat(sprintf("  [%s] worker %0*d  job %s  %s\n",
                status, width, i,
                if (is.na(jid)) "<NA>" else as.character(jid),
                x$log_files[[i]]))
  }
  invisible(x)
}


# -- awaitExperimentSBATCH ----------------------------------------------------

#' Wait for all SBATCH workers to finish
#'
#' Polls \code{squeue -j <ids>} every \code{interval_s} seconds until every
#' job ID has left the queue.  Optionally prints a final queue-status summary.
#'
#' @param es An \code{"experimentSBATCH"} object returned by
#'   \code{\link{experimentSBATCH}}.
#' @param interval_s Polling interval in seconds.  Default \code{30}.
#' @param verbose If \code{TRUE} (default), print a \code{table()} of final
#'   queue statuses after all jobs finish.
#'
#' @return The \code{es} object, invisibly.
#' @export
awaitExperimentSBATCH <- function(es, interval_s = 30, verbose = TRUE) {
  stopifnot(inherits(es, "experimentSBATCH"))
  ids <- es$job_ids[!is.na(es$job_ids)]
  if (length(ids) == 0L) {
    message("No live job IDs to wait on (dry-run handle?).")
    return(invisible(es))
  }
  message("Waiting for ", length(ids), " SBATCH job(s) to finish (poll ",
          interval_s, "s)...")
  repeat {
    alive <- .sbatch_squeue_alive(ids, es$clusters)
    if (!any(alive, na.rm = TRUE)) break
    Sys.sleep(interval_s)
  }
  message("All SBATCH job(s) finished.")
  if (verbose && !is.null(es$queue_path) && file.exists(es$queue_path)) {
    q <- readRDS(es$queue_path)
    if ("status" %in% names(q)) {
      cat("\nFinal queue status:\n")
      print(table(q$status))
    }
  }
  invisible(es)
}


# -- killExperimentSBATCH -----------------------------------------------------

#' Stop SBATCH workers launched by experimentSBATCH
#'
#' \strong{Graceful} (\code{force = FALSE}, default): creates per-worker stop
#' files; each worker exits cleanly between jobs once it observes its file.
#' Slurm jobs end normally; remaining \code{PENDING} rows stay in the queue
#' and can be resumed with another \code{\link{experimentSBATCH}} call against
#' the same \code{queue_path}.
#'
#' \strong{Immediate} (\code{force = TRUE}): runs \code{scancel <ids>} to kill
#' the Slurm jobs straight away.  Any rows that were \code{RUNNING} at the
#' time of cancellation will remain \code{RUNNING} in the queue until the
#' next reclaim pass; clean them up with
#' \code{tmuxRefreshQueueStatus(es$queue_path)}.
#'
#' @param es An \code{"experimentSBATCH"} object.
#' @param force \code{FALSE} (graceful) or \code{TRUE} (\code{scancel}).
#' @param scancel_cmd Path to \code{scancel}; defaults to \code{"scancel"}
#'   on \code{$PATH}.
#'
#' @return \code{es}, invisibly.
#' @seealso \code{\link{experimentSBATCH}}, \code{\link{awaitExperimentSBATCH}}
#' @export
killExperimentSBATCH <- function(es, force = FALSE, scancel_cmd = "scancel") {
  stopifnot(inherits(es, "experimentSBATCH"))
  if (!force) {
    created <- 0L
    for (sf in es$stop_files) {
      if (!file.exists(sf)) {
        file.create(sf)
        created <- created + 1L
      }
    }
    message("Stop files created for ", created, " worker(s).",
            "\nWorkers will exit between jobs after they observe the stop file.",
            "\nCall awaitExperimentSBATCH(es) to wait for them.")
  } else {
    ids <- es$job_ids[!is.na(es$job_ids)]
    if (length(ids) == 0L) {
      message("No live job IDs to cancel.")
      return(invisible(es))
    }
    if (Sys.which(scancel_cmd) == "" && !file.exists(scancel_cmd))
      stop("Could not find scancel executable: '", scancel_cmd, "'.",
           call. = FALSE)
    out <- system2(scancel_cmd,
                   args = c(.sbatch_cluster_args(es$clusters), as.character(ids)),
                   stdout = TRUE, stderr = TRUE)
    message("scancel sent for job(s): ", paste(ids, collapse = ", "),
            "\nStale RUNNING queue entries can be reset with:",
            "\n  tmuxRefreshQueueStatus(\"", es$queue_path, "\")")
    if (length(out)) message(paste(out, collapse = "\n"))
  }
  invisible(es)
}


# -- Internal: -M <cluster> for squeue/scancel on multi-cluster Slurm sites ---
# Jobs on another cluster are invisible to a plain squeue/scancel.
.sbatch_cluster_args <- function(clusters) {
  if (length(clusters) && nzchar(clusters[[1L]]))
    c("-M", paste(clusters, collapse = ","))
  else character(0)
}


# -- Internal: live job IDs from squeue --------------------------------------
# Returns a logical vector aligned with `ids`: TRUE if still in squeue, FALSE
# otherwise. NA entries in `ids` map to NA. If `squeue` is unavailable, all
# entries are NA (caller decides what to do).
.sbatch_squeue_alive <- function(ids, clusters = NULL) {
  if (Sys.which("squeue") == "") return(rep(NA, length(ids)))
  live_ids <- tryCatch(
    suppressWarnings(system2("squeue", args = c(.sbatch_cluster_args(clusters),
                                      "-h", "-o", "%i"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  live_ids <- suppressWarnings(as.integer(trimws(live_ids)))
  vapply(ids, function(jid) {
    if (is.na(jid)) NA else jid %in% live_ids
  }, logical(1L))
}
