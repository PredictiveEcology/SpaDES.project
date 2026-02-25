
# ======================================================================
# tmux orchestrator (package-friendly)
# - Starts plain R in N panes
# - Assigns objects from df rows (column names = object names)
# - Sources global_path
# - First pane: no delay. Subsequent panes: pane-internal Sys.sleep(...)
# - Optional mouse support (set-option -g mouse on)
# - Helper to kill spawned panes
# - All requireNamespace() checks live inside functions
# ======================================================================

#' Enable or disable tmux mouse interaction
#'
#' @description
#' Sets tmux mouse mode via `set-option -g mouse on/off`, enabling pane
#' selection, resizing, and scrolling with the mouse. See tmux manual for details. [1](https://www.rdocumentation.org/packages/rstudioapi/versions/0.17.0/topics/terminalExecute)
#'
#' @param on Logical; `TRUE` to enable, `FALSE` to disable. Default `TRUE`.
#' @return Invisibly returns `on`.
#' @export
tmux_set_mouse <- function(on = TRUE) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it with install.packages('processx').", call. = FALSE)
  }
  .tmux_run("set-option", "-g", "mouse", if (on) "on" else "off")
  invisible(on)
}

#' Spawn tmux worker panes, start R, assign objects from a data.frame, and source a script (pane-internal delay)
#'
#' @description
#' Controls the **current tmux window** to:
#' 1) create `n_workers` panes and tile them,
#' 2) start plain **R** in each pane,
#' 3) for the first `min(n_workers, nrow(df))` rows of `df`, assign objects named
#'    by the **column names** in that row and then `source(global_path)`.
#'
#' **Delays happen inside each pane’s R**, so control returns immediately to the **master pane**:
#' - Pane 1: **no delay** before `source()`.
#' - Pane i>1: sleeps `delay_before_source + (i-2)*stagger_by` seconds **inside R** before assignments + `source()`.
#'
#' Targeting uses tmux format expansion (`#{session_name}:#{window_index}`) and pane IDs
#' via `list-panes -F "#{pane_id}"`; command injection uses `send-keys … C-m`. [2](https://callr.r-lib.org/)
#'
#' @param df A `data.frame`. Column names become object names in worker panes; values
#'   from each row are assigned prior to sourcing `global_path`.
#' @param global_path Character scalar. Path to the script to `source()` in each pane.
#'   Prefer an **absolute path** if worker panes start in a different directory.
#' @param n_workers Integer. Number of worker panes to spawn. Default `4`.
#' @param delay_after_split Numeric. Seconds to wait after each `split-window`. Default `0.2`.
#' @param delay_after_layout Numeric. Seconds to wait after `select-layout`. Default `0.2`.
#' @param delay_between_R_start Numeric. Seconds to wait immediately **after starting R**
#'   in each pane (or set `0` to skip). Default `0.0`.
#' @param delay_before_source Numeric. Seconds panes **2..n** wait **inside R** before
#'   injecting assignments + `source(global_path)`. Default `60.0`.
#' @param stagger_by Numeric. Extra seconds added per subsequent pane beyond pane 2, so pane i>1 waits
#'   `delay_before_source + (i-2)*stagger_by` inside R. Default `0.0`.
#' @param activeRunningPath. The directory where the "running" flag will be written. 
#'   This flag (a file) should only be present while an ELF is running. At the end
#'   or at failure, it should be removed automatically. If it is not (because of a
#'   bad crash), then it MUST be manually deleted. Default is derived via
#'   `SpaDES.project:::activeRunningPathForTmux` and is  
#'   `file.path("logs/", queue_path)`
#' @param heartbeatFolder. A string or call using optionally `runName`, e.g., 
#'    `heartbeatFolder = quote(file.path("outputs", runName, "figures", "hists"))`.
#'    Currently, this will only work for outputs that are in `fireSense_SpreadFit`. 
#'    Ineffective otherwise.
#'    
#' @param set_mouse Logical. If `TRUE`, enables tmux mouse support via `tmux_set_mouse(TRUE)`. Default `TRUE`. [1](https://www.rdocumentation.org/packages/rstudioapi/versions/0.17.0/topics/terminalExecute)
#'
#' @return Invisibly returns the character vector of tmux **pane IDs** for the workers.
#' @export
#'
#' @examples
#' \dontrun{
#' expt <- data.frame(
#'   .ELFind = c("6.1.1","6.2.2","6.2.3","6.3.1"),
#'   .rep    = 1,
#'   check.names = FALSE
#' )
#'
#' # First pane: no delay. Others: 60s, with +5s stagger per pane.
#' workers <- experimentTmux(
#'   df                  = expt,
#'   global_path         = "/abs/path/to/global.R",
#'   n_workers           = 4,
#'   delay_before_source = 60,
#'   stagger_by          = 5,
#'   set_mouse           = TRUE
#' )
#'
#' # If something goes wrong during development:
#' tmux_kill_panes(workers)
#' }
experimentTmux <- function(df,
                           global_path = "global.R",
                           n_workers = 4,
                           delay_after_split = 0.4,
                           delay_after_layout = 0.4,
                           delay_between_R_start = 0.0,
                           delay_before_source = 60,
                           stagger_by = delay_before_source,
                           set_mouse = TRUE,
                           heartbeatFolder = NULL, # file.path("outputs", runName, "figures", "hists"),
                           # --- new arguments ---
                           activeRunningPath = getOption("spades.project.tmuxLogs"),
                           continue = TRUE,
                           queue_path = NULL,
                           on_interrupt = c("requeue", "fail"),
                           ss_id = NULL,
                           email = getOption("gargle_oauth_email"),
                           cache_path = getOption("gargle_oauth_cache"),
                           workersToMonitor = c("birds","biomass","camas","carbon","caribou","coco",
                                                "core","dougfir","fire","mpb","sbw","mega","acer","abies","pinus"),
                           runNameLabel = quote(colnames(q)[1:2])) {
  
  # -- dependency check
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it with install.packages('processx').", call. = FALSE)
  }
  
  on_interrupt <- match.arg(on_interrupt)
  # on_error     <- match.arg(on_error)
  
  # -- preconditions
  if (Sys.getenv("TMUX") == "") {
    stop("Not inside tmux. Start/attach to a tmux session first.", call. = FALSE)
  }
  if (!is.data.frame(df)) stop("'df' must be a data.frame.", call. = FALSE)
  if (!file.exists(global_path)) {
    warning("global_path not found from master R working directory: ", global_path)
  }
  if (n_workers < 1L) stop("'n_workers' must be >= 1.", call. = FALSE)

  # -- resolve current tmux window
  target_win <- .tmux_current_window()
  system("tmux set -g pane-border-status top") # sets so titles have names

  # -- list existing panes
  pre <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")

  # 1. Create a new pane for the sync process
  if (!is.null(queue_path)) {
    # tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel)

    if (!is.null(ss_id)) {
      isDir <- reproducible:::isGoogleDriveDirectory(ss_id)
      if (isTRUE(isDir)) {
        reproducible::.requireNamespace("googledrive", stopOnFALSE = TRUE)
        # googledrive::drive_auth()

        # 1. Derive the name from the local queue file
        sheet_name <- gsub("\\.rds$", "", basename(queue_path))

        # 2. Check if it already exists in that folder to avoid duplicates
        existing <- googledrive::drive_ls(googledrive::as_id(ss_id), pattern = sheet_name)

        if (nrow(existing) > 0) {
          ss_id <- existing$id[1]
        } else {
          reproducible::.requireNamespace("googlesheets4", stopOnFALSE = TRUE)
          # googlesheets4::gs4_auth()
          # 3. Create the sheet (defaults to root) then move it to the folder
          googlesheets4::gs4_auth(email = getOption("gargle_oauth_email"), cache = getOption("gargle_oauth_cache"))
          googledrive::drive_auth(email = getOption("gargle_oauth_email"), cache = getOption("gargle_oauth_cache"))
          # googledrive::drive_auth(path = "~/genial-cycling-408722-788552a3ecac.json")
          # googlesheets4::gs4_auth(path = "~/genial-cycling-408722-788552a3ecac.json")
          
          new_sheet <- googlesheets4::gs4_create(name = sheet_name, sheets = "Status")
          googledrive::drive_mv(file = googledrive::as_id(new_sheet),
                                path = googledrive::as_id(ss_id))
          ss_id <- as.character(googledrive::as_id(new_sheet))
        }
      }

      # 1. Create the Monitoring Pane (Detached)
      mon_id <- .tmux_out("split-window", "-d", "-v", "-t", target_win, "-P", "-F", "#{pane_id}")

      # 2. Label it for 2025 observability
      .tmux_run("select-pane", "-t", mon_id, "-T", "Cluster_Monitor")

      # 3. Construct the R command
      # We deparse the vector to ensure it's passed correctly as a character string
      workersToMonitor
      # workersToMonitor <- c("birds", "biomass", "camas", "carbon", "caribou", "coco",
      #                   "core", "dougfir", "fire", "mpb", "sbw", "mega",
      #                   "acer", "abies", "pinus")

      mon_cmd <- sprintf(
        "clusters::monitorCluster(cores = %s)",
        deparse1(workersToMonitor)
      )

      # 4. Launch in the new pane
      .tmux_run("select-layout", "-t", target_win, "tiled")
      .tmux_run("select-pane", "-t", mon_id, "-T", "Cluster_Monitor")

      full_bash_mon_cmd <- sprintf("Rscript -e %s", shQuote(mon_cmd))
      .tmux_run("send-keys", "-t", mon_id, full_bash_mon_cmd, "C-m")


      # 1. Create the sync pane DETACHED (-d) and capture its unique ID (%)
      # This ensures the focus stays on the Master Pane
      sync_pane_id <- .tmux_out("split-window", "-d", "-v", "-t", target_win, "-P", "-F", "#{pane_id}")
      .tmux_run("select-layout", "-t", target_win, "tiled")

      # 2. Prepare the command as a SINGLE line to prevent shell splitting
      # Use deparse1() and force ss_id to character
      sync_cmd <- sprintf(
        "options(gargle_oauth_email = %s); SpaDES.project:::.sync_loop_internal(queue_path=%s, ss_id=%s, email=%s, cache_path=%s)",
        deparse1(email),
        deparse1(normalizePath(queue_path)),
        deparse1(as.character(ss_id)),
        deparse1(email),
        deparse1(normalizePath(cache_path))
      )

      # 3. Send keys to the specific ID
      # Adding a leading space ' ' prevents the command from being saved in bash history;
      #  I took this away because I wanted access to the command
      full_bash_cmd <- sprintf("Rscript -e %s", shQuote(sync_cmd))
      .tmux_run("send-keys", "-t", sync_pane_id, full_bash_cmd, "C-m")

      # 4. Label the pane for clarity
      .tmux_run("select-pane", "-t", sync_pane_id, "-T", "GSheet_Sync")
    }
  }

  activeRunningPath <- activeRunningPathForTmux(activeRunningPath = activeRunningPath, queue_path)

  # -- create worker panes
  worker_ids <- character()
  for (i in seq_len(n_workers)) {
    # Create detached so focus stays on Master
    new_id <- .tmux_out("split-window", "-d", "-v", "-t", target_win, "-P", "-F", "#{pane_id}")
    worker_ids <- c(worker_ids, new_id)

    # MANDATORY: Reset layout immediately so the next split has room
    .tmux_run("select-layout", "-t", target_win, "tiled")
    # was 
    Sys.sleep(2)
    #Sys.sleep(0.1)
  }

  # -- arrange evenly
  # 2. Force the window into a tiled grid layout
  # This fixes the "tiny sliver" problem immediately
  # .tmux_run("select-layout", "-t", target_win, "tiled")
  # if (delay_after_layout > 0) Sys.sleep(delay_after_layout)

  # -- mouse on, if requested
  if (set_mouse) tmux_set_mouse(TRUE)

  # -- collect new worker panes
  post <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  if (length(post) == 0L) stop("tmux list-panes returned no panes.", call. = FALSE)

  master  <- Sys.getenv("TMUX_PANE")
  workers <- setdiff(post, pre)
  workers <- setdiff(workers, master)

  if (length(workers) < n_workers) {
    stop(sprintf("Expected %d new worker panes, found %d. Check tmux version/layout.",
                 n_workers, length(workers)), call. = FALSE)
  }
  workers <- workers[seq_len(n_workers)]

  # 3. Now send the R commands to the clean, tiled panes
  start_cmd <- "R"
  
  for (pid in worker_ids) {
    .tmux_run("send-keys", "-t", pid, start_cmd, "C-m")
    # Your existing staggered start delay
    Sys.sleep(0.1)
  }

  # ---------- branching: single-shot vs queue mode ----------
  # if (!continue) {
  #   # -- inject pane-specific code (pane 1: no delay; panes i>1: pane-internal sleep)
  #   n_send <- min(n_workers, nrow(df))
  #   for (i in seq_len(n_send)) {
  #     # compute pane-internal sleep: pane 1 => 0; pane i>1 => delay_before_source + (i-2)*stagger_by
  #     runName <- as.character(df[[runNameLabel]][i])
  #     runName <- gsub("[^[:alnum:]_.:-]", "-", runName)
  #     code <- sprintf(
  #       "Sys.sleep(%s); system2('tmux', c('select-pane','-t', Sys.getenv('TMUX_PANE'), '-T', %s)); %s",
  #       pre_sleep,
  #       deparse(runName),
  #       .make_assignment_code(df, i, global_path, pre_sleep = 0)
  #     )
  #     .tmux_run("send-keys", "-t", workers[i], code, "C-m")
  #     
  #     # No orchestrator sleeps—control returns immediately to master
  #   }
  # } else {
    # ---- queue mode: file-backed queue & **direct function invocation** (no source()) ----
    if (is.null(queue_path)) {
      queue_path <- file.path(dirname(normalizePath(global_path)), "tmux_queue.rds")
    }
    tmux_prepare_queue_from_df(df, queue_path)
    q <- readRDS(queue_path)
    runNameLabel <- eval(runNameLabel)
    
    tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel, heartbeatFolder = heartbeatFolder,
                              activeRunningPath = activeRunningPath)
    
    # Warn if filelock missing (workers will error in panes if not installed)
    if (!requireNamespace("filelock", quietly = TRUE)) {
      warning("Workers require 'filelock' installed on the host. Install with install.packages('filelock').")
    }
    
    # Build a pane payload that **calls functions** instead of source()ing files.
    # Default preserves previous behavior (loop); if you want single-shot workers, swap to runNextWorker().
    
    for (i in seq_along(workers)) {
      pre_sleep <- if (i == 1L) 0 else (delay_before_source + max(0, i - 2) * stagger_by)
      payload <- sprintf(
        "SpaDES.project::runWorkerLoop(queue_path=%s, global_path=%s, on_interrupt=%s, runNameLabel=%s, activeRunningPath=%s)",
        deparse1(queue_path),
        deparse1(global_path),
        deparse1(match.arg(on_interrupt)),
        deparse1(runNameLabel),
        deparse1(activeRunningPath)
      )
      code <- sprintf("Sys.sleep(%s); %s", pre_sleep, payload)
      .tmux_run("send-keys", "-t", workers[i], code, "C-m")
    }
    
  # }

  invisible(workers)
}


#' Run one queued job (claim-next semantics) in the current R session.
#'
#' @param queue_path character; path to the queue `.rds`
#' @param global_path character; script to source for the job
#' @param on_interrupt "requeue" or "fail". If the sourced script is interrupted, either requeue or mark as FAILED.
#' @param heartbeat_interval_s numeric; seconds between heartbeats while the job runs
#' @return "ok" | "interrupt" | "empty" (if no pending work found); used by runWorkerLoop()
#' @export
#' @param runNameLabel A quoted expression (possibly of `q`, which is the result of `q <- readRDS(queue_path)`).
#'   Default is the first 2 column names of `q`. These will be concatenated and used as
#'   labels for various things including the `activeRunningPath` file(s).
#' @export
runNextWorker <- function(queue_path, global_path,
                          on_interrupt = c("requeue","fail"),
                          heartbeat_interval_s = 60,
                          runNameLabel = quote(colnames(q)[1:2]),
                          heartbeatFolder = NULL,
                          activeRunningPath = getOption("spades.project.tmuxLogs")) {
  if (missing(global_path))
    global_path <- "global.R"
  stopifnot(file.exists(queue_path), file.exists(global_path))
  on_interrupt <- match.arg(on_interrupt)
  if (!requireNamespace("filelock", quietly = TRUE))
    stop("Package 'filelock' is required.")
  
  PANE  <- Sys.getenv("TMUX_PANE")
  LOCKF <- paste0(queue_path, ".lock")
  
  # update queue file from log files
  activeRunningPath <- activeRunningPathForTmux(activeRunningPath = NULL, queue_path)
  tmux_refresh_queue_status(queue_path, runNameLabel = runNameLabel, heartbeatFolder = heartbeatFolder,
                            activeRunningPath = activeRunningPath)
  # claim a row (unchanged)
  lck <- filelock::lock(LOCKF, timeout = Inf)
  on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
  q <- readRDS(queue_path)
  
  runNameLabel <- eval(runNameLabel)
  
  # Take the first one on the list that is PENDING or INTERRUPTED
  pending_idx <- which(q$status %in% c(txtInterrupted,txtPending))[1]
  
  if (length(pending_idx) == 0) {
    filelock::unlock(lck)
    return("empty")
  }
  i <- pending_idx
  
  # derive data_cols for defaulting runNameLabel
  data_cols <- setdiff(names(q), meta_cols)
  
  if (is.null(runNameLabel)) {
    if (length(data_cols) == 0L)
      stop("No data columns available to infer 'runNameLabel'.")
    runNameLabel <- data_cols[1L]
  }
  if (!all(runNameLabel %in% names(q)))
    stop(sprintf("runNameLabel '%s' is not a column in the queue.", runNameLabel))
  
  # inject globals (unchanged)
  for (nm in data_cols) {
    assign(nm, q[[nm]][i], envir = .GlobalEnv)
  }
  
  # status bookkeeping (unchanged)
  q$status[i]       <- txtRunning
  q$claimed_by[i]   <- PANE
  q$started_at[i]   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  q$machine_name[i] <- Sys.info()[["nodename"]]
  q$process_id[i]   <- Sys.getpid()
  saveRDS(q, queue_path)
  filelock::unlock(lck)
  
  # --- Pane title uses runName ---
  runName <- as.character(q[i, runNameLabel] |> paste(collapse = "-"))
  runName <- gsub("[^[:alnum:]_.:-]", "-", runName)
  try({
    if (exists(".tmux_run", mode = "function")) {
      .tmux_run("select-pane", "-t", PANE, "-T", runName)
    } else {
      processx::run("tmux", c("select-pane","-t", PANE, "-T", runName),
                    echo_cmd = FALSE, echo = FALSE, error_on_status = FALSE)
    }
  }, silent = TRUE)
  
  activeRunningPath <- activeRunningPathForTmux(activeRunningPath = NULL, queue_path)
  startedFile <- file.path(activeRunningPath, paste0("Running_", runName, "_", Sys.getpid(), "_.rds"))
  reproducible::checkPath(dirname(startedFile), create = TRUE)
  saveRDS(runName, file = startedFile)
  on.exit(try(unlink(startedFile), silent = TRUE), add = TRUE)

  # --- Heartbeat now tracks runNameLabel-based location ---
  current_run <- q[i, runNameLabel] |> paste(collapse = "-")
  hb_thread <- try(parallel::mcparallel({
    repeat {
      Sys.sleep(heartbeat_interval_s)
      hb_vals <- get_latest_heartbeat(current_run, heartbeatFolder = heartbeatFolder)
      l2 <- filelock::lock(LOCKF, timeout = 10)
      on.exit(try(filelock::unlock(l2), silent = TRUE), add = TRUE)
      if (!is.null(l2)) {
        q2 <- readRDS(queue_path)
        idx <- which(q2$status == txtRunning & q2$claimed_by == PANE)
        if (length(idx)) {
          q2$heartbeat_at[idx]   <- hb_vals$ts
          q2$heartbeat_iter[idx] <- hb_vals$iter
          saveRDS(q2, queue_path)
        }
        filelock::unlock(l2)
      }
    }
  }), silent = TRUE)
  
  outcome <- tryCatch({
    source(global_path, local = .GlobalEnv)
    "ok"
  }, interrupt = function(e) "interrupt")
  
  # finalize (unchanged except for return)
  lck <- filelock::lock(LOCKF, timeout = Inf)
  on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
  q <- readRDS(queue_path)
  if (outcome == "ok") {
    q$status[i]      <- txtDone
    q$finished_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  } else if (outcome == "interrupt") {
    if (on_interrupt == "requeue") {
      q$status[i]    <- txtPending
      q$claimed_by[i] <- NA_character_
    } else {
      q$status[i]      <- "FAILED"
      q$finished_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }
  }
  saveRDS(q, queue_path)
  filelock::unlock(lck)
  if (!is.null(hb_thread)) try(parallel::pskill(hb_thread$pid), silent = TRUE)
  outcome
}

#' Run queued jobs repeatedly (pane-local loop).
#'
#' @inheritParams runNextWorker
#' @param stop_file optional path; if present, stop after current iteration
#' @return invisibly TRUE
#' @export
runWorkerLoop <- function(queue_path, global_path,
                          on_interrupt = c("requeue","fail"),
                          heartbeat_interval_s = 60,
                          stop_file = NULL,
                          activeRunningPath = getOption("spades.project.tmuxLogs"),
                          runNameLabel = quote(colnames(q)[1:2])) {
  on_interrupt <- match.arg(on_interrupt)
  repeat {
    if (!is.null(stop_file) && isTRUE(file.exists(stop_file))) break
    if (missing(global_path))
      global_path <- "global.R"
    res <- runNextWorker(queue_path, global_path, on_interrupt,
                         heartbeat_interval_s, runNameLabel = runNameLabel, activeRunningPath = activeRunningPath)
    if (identical(res, "empty")) break
    if (identical(res, "interrupt") && on_interrupt == "fail") break
    Sys.sleep(stats::runif(1, 0.05, 0.2))
  }
  invisible(TRUE)
}


#' Kill a set of tmux panes (e.g., those spawned by experimentTmux)
#'
#' @description
#' Development utility: kills all panes identified by their tmux pane IDs.
#' Uses `kill-pane -t <pane-id>`; panes already gone are ignored. See tmux manual. [1](https://www.rdocumentation.org/packages/rstudioapi/versions/0.17.0/topics/terminalExecute)
#'
#' @param panes Character vector of tmux pane IDs (e.g., `c("%2", "%3")`) returned by
#'   `experimentTmux()`.
#' @return Invisibly returns the subset of `panes` successfully targeted.
#' @export
tmux_kill_panes <- function(panes) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it with install.packages('processx').", call. = FALSE)
  }
  if (!length(panes)) return(invisible(character()))
  killed <- character()
  for (pid in panes) {
    ok <- try(.tmux_run("kill-pane", "-t", pid), silent = TRUE)
    if (!inherits(ok, "try-error")) killed <- c(killed, pid)
  }
  invisible(killed)
}

# ---- internal helpers --------------------------------------------------

#' @keywords internal
#' @noRd
.tmux_run <- function(...) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it with install.packages('processx').", call. = FALSE)
  }
  processx::run("tmux", c(...), echo_cmd = FALSE, echo = FALSE, error_on_status = TRUE)
}

#' @keywords internal
#' @noRd
.tmux_out <- function(...) {
  res <- .tmux_run(...)
  out <- strsplit(res$stdout, "\n", fixed = TRUE)[[1]]
  out[nzchar(out)]
}

#' @keywords internal
#' @noRd
.tmux_current_window <- function() {
  pane_id <- Sys.getenv("TMUX_PANE")
  if (pane_id == "") stop("Not inside tmux (TMUX_PANE is empty).", call. = FALSE)
  sw <- .tmux_out("display-message", "-p", "-t", pane_id, "#{session_name}:#{window_index}")  # formats  [2](https://callr.r-lib.org/)
  if (length(sw) != 1L || !grepl("^.+:[0-9]+$", sw)) {
    stop("Failed to resolve current tmux session:window from TMUX_PANE=", pane_id, call. = FALSE)
  }
  sw
}

#' @keywords internal
#' @noRd
.make_assignment_code <- function(df, row_i, global_path, pre_sleep = 0) {
  cols <- names(df)
  vals <- df[row_i, , drop = FALSE]

  assigns <- vapply(seq_along(cols), function(j) {
    nm <- cols[j]
    v  <- vals[[j]]
    if (is.factor(v)) v <- as.character(v)

    if (is.character(v)) {
      sprintf('%s <- "%s"', nm, v)
    } else if (is.logical(v)) {
      sprintf('%s <- %s', nm, if (is.na(v)) "NA" else if (v) "TRUE" else "FALSE")
    } else if (is.numeric(v) || is.integer(v)) {
      sprintf('%s <- %s', nm, as.character(v))
    } else if (is.na(v)) {
      sprintf('%s <- NA', nm)
    } else {
      sprintf('%s <- %s', nm, deparse(v))
    }
  }, character(1))

  # Prepend pane-internal sleep if requested (FIXED: correct sprintf + quoting)
  sleep_code <- if (pre_sleep > 0) sprintf("Sys.sleep(%s); ", pre_sleep) else ""

  paste0(
    sleep_code,
    paste(assigns, collapse = "; "),
    "; ",
    sprintf('source(%s)', deparse(global_path))
  )
}

#' Initialize a file-backed queue from a data.frame (extended schema)
#'
#' Adds metadata columns used by workers:
#' - status:       PENDING | RUNNING | DONE | FAILED
#' - claimed_by:   tmux pane id that claimed the row
#' - started_at:   "YYYY-MM-DD HH:MM:SS"
#' - finished_at:  "YYYY-MM-DD HH:MM:SS"
#' - DEoptimElapsedTime: numeric seconds (`sum(diff(allIterations[allIterations < 20 minutes]))`)
#' - machine_name: `Sys.info()[["nodename"]]`
#' - process_id:   Sys.getpid()
#' - heartbeat_at: latest timestamp (as character) detected by heartbeat
#' - heartbeat_iter: latest iteration number (integer) detected by heartbeat
#'
#' @param df data.frame; experiment rows (columns become objects in workers)
#' @param queue_path character; path to the queue `.rds` (absolute recommended)
#' @return Invisibly returns `queue_path`.
#' @export
tmux_prepare_queue_from_df <- function(df, queue_path) {
  stopifnot(is.data.frame(df), is.character(queue_path), length(queue_path) == 1)
  q <- cbind(
    df,
    status         = txtPending,
    claimed_by     = NA_character_,
    started_at     = as.character(NA),
    finished_at    = as.character(NA),
    DEoptimElapsedTime   = as.numeric(NA),
    machine_name   = NA_character_,
    process_id     = as.integer(NA),
    heartbeat_at   = as.character(NA),
    heartbeat_iter = as.integer(NA),
    iterationsTotal= as.integer(NA)
  )
  saveRDS(q, queue_path)
  invisible(queue_path)
}


# internal helper: write the worker loop that consumes the queue

# internal helper: write the worker loop that consumes the queue
# .write_worker_loop <- function(queue_path,
#                                global_path,
#                                on_interrupt = c("requeue","fail"),
#                                # on_error     = c("fail","requeue"),
#                                heartbeat_interval_s = 60) {
#   stopifnot(file.exists(queue_path), file.exists(global_path))
#   on_interrupt <- match.arg(on_interrupt)
#   # on_error     <- match.arg(on_error)
# 
#   # --- Inside your .write_worker_loop generator ---
#     # --- Inside .write_worker_loop ---
#     # Use R's raw string syntax to protect the regex \d
#     template <- R"-(
#   if (!requireNamespace("filelock", quietly = TRUE)) {
#     stop("Package filelock is required in worker panes.")
#   }
# 
#   QUEUE  <- %s
#   LOCKF  <- paste0(QUEUE, ".lock")
#   GLOBAL <- %s
#   PANE   <- Sys.getenv("TMUX_PANE")
#   HB_INT <- %d
# 
# 
#   hb_thread <- NULL
# 
#   repeat {
#     # 0. Random jitter
#     Sys.sleep(runif(1, 0.1, 0.8))
# 
#     # 1. Acquire EXCLUSIVE lock on the lock file
#     lck <- filelock::lock(LOCKF, timeout = Inf)
#     on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
# 
#     # 2. Read the queue inside the lock
#     q <- readRDS(QUEUE)
# 
# 
#     # 3. Find all PENDING rows
#     # pending_idx <- which(q$status == txtPending)
#     pending_idx <- which(q$status %%in%% c(txtInterrupted, txtPending))[1]
# 
#     if (length(pending_idx) == 0) {
#       filelock::unlock(lck)
#       break
#     }
# 
#     # 4. Take the VERY FIRST one and claim it IMMEDIATELY
#     i <- pending_idx[1]
# 
#     # Identify non-metadata columns to inject as global variables
#     meta_cols <- c("status", "claimed_by", "started_at", "finished_at",
#                    "DEoptimElapsedTime", "machine_name", "process_id",
#                    "heartbeat_at", "heartbeat_iter", "iterationsTotal")
#     data_cols <- setdiff(names(q), meta_cols)
# 
#     for (nm in data_cols) {
#       # USE envir = .GlobalEnv EXPLICITLY
#       # This ensures variables like .ELFind are available to source(GLOBAL)
#       assign(nm, q[[nm]][i], envir = .GlobalEnv)
#       # 2. Diagnostic (Visible in the tmux pane)
#       message("Injected ", nm, " = ", q[[nm]][i], " into .GlobalEnv")
#     }
# 
#     q$status[i]      <- txtRunning
#     q$claimed_by[i]  <- PANE
#     q$started_at[i] <- format(Sys.time(), "%%Y-%%m-%%d %%H:%%M:%%S")
#     q$machine_name[i] <- Sys.info()[["nodename"]]
#     q$process_id[i]   <- Sys.getpid()
# 
# 
#     saveRDS(q, QUEUE)
#     filelock::unlock(lck)
# 
#     current_elfind <- q[[".ELFind"]][i]
# 
#     if (!is.null(hb_thread)) {
#       try(parallel::pskill(hb_thread$pid), silent = TRUE)
#       try(parallel::mccollect(hb_thread, wait = FALSE), silent = TRUE)
#     }
# 
#     hb_thread <- parallel::mcparallel({
#       repeat {
#         Sys.sleep(HB_INT)
#         hb_vals <- get_latest_heartbeat(current_elfind)
#         lck2 <- filelock::lock(LOCKF, timeout = 10)
#         on.exit(try(filelock::unlock(lck2), silent = TRUE), add = TRUE)
#         if (!is.null(lck2)) {
#           q2 <- readRDS(QUEUE)
#           idx <- which(q2$status == txtRunning & q2$claimed_by == PANE)
#           if (length(idx)) {
#             q2$heartbeat_at[idx]   <- hb_vals$ts
#             q2$heartbeat_iter[idx] <- hb_vals$iter
#             saveRDS(q2, QUEUE)
#           }
#           filelock::unlock(lck2)
#         }
#       }
#     }, silent = TRUE)
# 
#     outcome <- tryCatch({
#       source(GLOBAL, local = .GlobalEnv)
#       "ok"
#     }, interrupt = function(e) "interrupt")
# 
#     lck <- filelock::lock(LOCKF, timeout = Inf)
#     on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
#     q <- readRDS(QUEUE)
# 
#     if (identical(outcome, "ok")) {
#       q$status[i]      <- txtDone
#       q$finished_at[i] <- format(Sys.time(), "%%Y-%%m-%%d %%H:%%M:%%S")
#     } else if (identical(outcome, "interrupt")) {
#       if ("%s" == "requeue") {
#         q$status[i]      <- txtPending
#         q$claimed_by[i]  <- NA_character_
#       } else {
#         q$status[i]      <- "FAILED"
#         q$finished_at[i] <- format(Sys.time(), "%%Y-%%m-%%d %%H:%%M:%%S")
#       }
#     }
# 
#     saveRDS(q, QUEUE)
#     filelock::unlock(lck)
#     if (identical(outcome, "interrupt")) break
#   }
#   if (!is.null(hb_thread)) try(parallel::pskill(hb_thread$pid), silent = TRUE)
#   )-"
# 
#   # Now run sprintf with exactly 5 arguments
#   loop <- sprintf(
#     template,
#     deparse1(normalizePath(queue_path)),
#     deparse1(normalizePath(global_path)),
#     as.integer(heartbeat_interval_s),
#     on_interrupt
#     # on_error
#   )
# 
# 
#   script_path <- file.path(dirname(queue_path), "tmux_worker_loop.R")
#   writeLines(loop, script_path)
#   normalizePath(script_path)
# }


#' Assess simulation status from PNG outputs
#' @param runName Directory containing the figures/hists
#' @param timeout_min Threshold for inactivity (e.g., 20)
.assess_sim_visual_status <- function(runName, timeout_min = 20, heartbeatFolder = NULL, activeRunningPath = getOption("spades.project.tmuxLogs")) {
  startedFiles <- dir(activeRunningPath, pattern = txtRunning, ignore.case = TRUE)
  is_running <- runName %in% sapply(startedFiles, function(x) strsplit(x, "_")[[1]][[2]])
  if (isTRUE(is_running)) {
    return(txtRunning)
  }
  
  if (is.call(heartbeatFolder))
    heartbeatFolder <- eval(heartbeatFolder)
  
  if (!is.null(heartbeatFolder)) {
    # heartbeatFolder <- file.path("outputs", runName, "figures", "objFun")
    if (!dir.exists(heartbeatFolder)) return(txtPending)
    
    # Find most recent PNG
    png_files <- list.files(heartbeatFolder, pattern = "\\.png$", full.names = TRUE)
    if (length(png_files) == 0) return(txtPending)
    
    # Get most recent file
    finfo <- file.info(png_files)
    latest_file <- png_files[which.max(finfo$mtime)]
    last_mod <- finfo$mtime[which.max(finfo$mtime)]
    
    # Check for staleness
    is_stale <- difftime(Sys.time(), last_mod, units = "mins") > timeout_min
    if (!is_stale) return(txtRunning)
    
    # Visual check for "red" using magick
    # Red pixels typically have high R and low G/B values
    reproducible::.requireNamespace("magick", stopOnFALSE = TRUE)
    img <- magick::image_read(latest_file)
    img_data <- as.integer(img[[1]]) # Get RGBA array
    
    # Heuristic: Red channel > 200 AND Green/Blue < 100
    # img_data is [height, width, channels]
    red_pixels <- sum(img_data[,,1] > 200 & img_data[,,2] < 100 & img_data[,,3] < 100)
    # This is simpler heuristic. Maybe not accurate enough though
    # red_pixels <- sum(img_data[,,1] > 200)
    
    has_red <- red_pixels > 0
    
    if (has_red) {
      return(txtDone)
    } else {
      # return(txtDone)
      return(txtInterrupted)
    }
  } else {
    return(txtPending)
  }
}

#' Refresh and Assess Queue Status from Simulation Outputs
#'
#' Scans the simulation output directories (defined by `runNameLabel`) to assess
#' current status based on file timestamps and visual content of PNGs.
#' If a PNG has not been updated for a specified timeout, the task is marked
#' as "FINISHED" (if red pixels are detected) or "INTERRUPTED" (if no red
#' is detected).
#'
#' @param queue_path Character. Absolute path to the `experiment_queue.rds` file.
#' @param timeout_min Numeric. Minutes of inactivity before a task is
#'   considered stale. Defaults to 20.
#'
#' @return A data.frame (the updated queue), invisibly.
#'   As a side effect, updates the RDS file on disk.
#'
#' @export
#' @importFrom filelock lock unlock
#' @importFrom magick image_read
#'
#' @examples
#' \dontrun{
#' # Assessment of all simulations in the current project
#' tmux_refresh_queue_status("experiment_queue.rds", timeout_min = 30)
#' }
tmux_refresh_queue_status <- function(queue_path, timeout_min = 20, runNameLabel = quote(colnames(q)[1:2]),
                                      heartbeatFolder = NULL,
                                      activeRunningPath = getOption("spades.project.tmuxLogs")) {
  
  if (file.exists(queue_path)) {
    lck <- filelock::lock(paste0(queue_path, ".lock"), timeout = 10000)
    on.exit(try(filelock::unlock(lck), silent = TRUE), add = TRUE)
    if (is.null(lck)) stop("Could not lock queue for refresh.")

    q <- try(readRDS(queue_path))
    if (is(q, "try-error")) {
      unlink(queue_path)
      return(invisible(NULL))
    }

    # Only refresh rows that aren't already marked DONE
    to_check <- which(!q$status %in% txtDone)
    # to_check <- seq_len(NROW(q))#which(!q$status %in% txtDone)

    runNameLabel <- eval(runNameLabel)
    if (missing(runNameLabel) || is.null(runNameLabel)) {
      runNameLabel <- setdiff(colnames(q), meta_cols)[1L]
    }
    for (i in to_check) {
      runName <- q[i, runNameLabel] |> paste(collapse = "-")
      # if (runName == "14.1") {
      #   debug(get_latest_heartbeat)
      # }
      # runName <- q[i, runNameLabel] |> paste(collapse = "-")
      activeRunningPath <- activeRunningPathForTmux(activeRunningPath = NULL, queue_path)
      new_status <- .assess_sim_visual_status(runName = runName, timeout_min = timeout_min, 
                                              heartbeatFolder = heartbeatFolder, 
                                              activeRunningPath = activeRunningPath)
      hb <- get_latest_heartbeat(runName, heartbeatFolder = heartbeatFolder)
      elapsedTime <- hb$elapsed
      
      if (any(unlist(hb) %in% NA) ) {
        # Has 
        if (new_status %in% txtPending) {
          cns <- setdiff(meta_cols, "status") #setdiff(colnames(q), c(runNameLabel, ".rep", "status"))
          for (cn in cns)
            q[[cn]][i] <- NA
        } else {
          # This is RUNNING; but not at DEoptim yet
          fi <- logFileInfo(activeRunningPath = activeRunningPath, pattern = txtRunning, runName = runName, queue_path = queue_path)
          
          # q[q$process_id %in% names(pidsToRm)[pidsToRm],"status"] <- txtRunning
          q$started_at[i] <- format(fi$mtime, "%Y-%m-%d %H:%M:%S")
          q$machine_name[i] <- Sys.info()[["nodename"]]
          q$DEoptimElapsedTime[i] <- NA
          # q$process_id[i] <- NA
        }
      } else {
        # Update status and timestamps if changed
        fi <- logFileInfo(activeRunningPath = activeRunningPath, pattern = txtRunning, runName = runName, queue_path = queue_path)
        # pids <- Map(fiHere = rownames(fi), function(fiHere) {
        #   pid <- strsplit(fiHere, split = "_")[[1]][3] |> as.integer()
        #   alive <- is_pid_alive_tools(pid)
        #   if (isFALSE(alive)) {
        #     unlink(fiHere)
        #   }
        # })
        
        q$process_id[i] <- NA
        if (NROW(fi)) {
          procId <- strsplit(rownames(fi), split = "_")[[1]][3]
          if (is_pid_alive_tools(as.integer(procId)) )
            new_status <- txtRunning
          
          if (!is.na(suppressWarnings(as.numeric(procId))))
            q$process_id[i] <- procId
          if (!is.character(hb$started) || is.na(hb$started)) {
            q$started_at[i] <- format(fi$mtime, "%Y-%m-%d %H:%M:%S")
          } else {
            # elapsedTime <- difftime(fi$mtime, hb$started)
            if (elapsedTime > 0)
              q$started_at[i] <- format(as.POSIXct(hb$ts) - elapsedTime, "%Y-%m-%d %H:%M:%S")
            else
              q$started_at[i] <- format(fi$mtime, "%Y-%m-%d %H:%M:%S")
          }
          if (isTRUE(is.na(q$machine_name[i])))
            q$machine_name[i] <- Sys.info()[["nodename"]]
        } else {

          # Says it is RUNNING --> but is actually INTERRUPTED because it has no log file (fi)
          if (isTRUE(is.na(q$started_at[i])))
            q$started_at[i] <- hb$started
          if (isTRUE(is.na(q$machine_name[i])))
            q$machine_name[i] <- Sys.info()[["nodename"]]
          
          new_status <- txtInterrupted
          if (q$heartbeat_iter[i] %% 25 %in% 0)
            new_status <- txtDone
            
        }
        
        q$heartbeat_iter[i] <- hb$iter
        q$heartbeat_at[i] <- hb$ts
        # dt <- try(format(round(difftime(q$heartbeat_at[i], q$started_at[i], units = "days"), 2), digits = 2))
        q$DEoptimElapsedTime[i] <- format(round(elapsedTime, 2), digits = 2, units = "days")
        if (new_status %in% txtDone) {
          q$finished_at[i] <- q$heartbeat_at[i]
          q$heartbeat_at[i] <- NA
          q$iterationsTotal[i] <- q$heartbeat_iter[i]
          q$heartbeat_iter[i] <- NA
          q$claimed_by[i] <- NA
        }
        
      }
      if (q$status[i] != new_status) {
        q$status[i] <- new_status
        # If newly finished, record current time
        # if (new_status  "FINISHED") {
        #   q$finished_at[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        # }
      }
      
    }
    
    q <- data.table::as.data.table(q);
    # ord <- order(as.package_version(q[[runNameLabel]]))
    # q <- q[ord, ]
    # q1 <- data.table::rbindlist(list(q[status %in% txtDone], q[status %in% txtRunning]))
    # q2 <- q[!q1, on = runNameLabel][grep("^(5.|6.|14.|9.|12.)", .ELFind),]
    # q3 <- rbindlist(list(q1, q2))
    # q <- rbindlist(list(q3, q[!q3, on = runNameLabel]))
    
    q <- as.data.frame(q)
    
    saveRDS(q, queue_path)
    filelock::unlock(lck)
    invisible(q)
  }
}

get_latest_heartbeat <- function(runName, heartbeatFolder = NULL) {
  retNames <- c("ts", "iter", "started", "elapsed")
  if (!is.null(heartbeatFolder)) {
    # heartbeatFolder <- file.path("outputs", runName, "figures", "hists")
    if (!dir.exists(heartbeatFolder))  {
      heartbeatFolder <- file.path("outputs", runName, "figures", "fireSense_SpreadFit", "hists")
      if (!dir.exists(heartbeatFolder))
        return(list(ts = NA_character_, iter = NA_integer_))
    }
    
    files <- list.files(heartbeatFolder, pattern = "iter", full.names = FALSE)
    if (length(files) == 0) return(list(ts = NA_character_, iter = NA_integer_))
    
    parse_one <- function(fn) {
      parts <- strsplit(fn, "_", fixed = TRUE)[[1]]
      ts <- if (length(parts) >= 1) parts[length(parts)] else NA_character_
      # Clean regex: no extra backslashes needed because of Raw String wrapper
      iter_idx <- which(grepl("iter\\d+", parts))
      iter <- if (length(iter_idx)) {
        as.integer(sub("iter(\\d+)", "\\1", parts[iter_idx[1]]))
      } else NA_integer_
      list(ts = ts, iter = iter)
    }
    
    parsed <- lapply(files, parse_one)
    a <- rbindlist(parsed) |> data.table::setorderv("iter")
    ts_vec <- vapply(parsed, function(x) x$ts, character(1))
    ts_vec <- fs::path_ext_remove(ts_vec)
    iter_vec <- vapply(parsed, function(x) x$iter, integer(1))
    ord <- order(ts_vec, na.last = TRUE)
    elapsed <- diff(as.POSIXct(ts_vec[ord]), units = "mins")
    elapsedTime <- sum(elapsed[elapsed < 20])
    units(elapsedTime) <- "days"
    ret <- list(ts = ts_vec[ord[length(ord)]], iter = iter_vec[ord[length(ord)]],
                started = head(ts_vec[ord], 1), elapsed = elapsedTime) |> setNames(retNames)
  } else {
    ret <- lapply(1:4, function(x) NA) |> setNames(retNames)
  }
  ret
}


logFileInfo <- function(activeRunningPath = getOption("spades.project.tmuxLogs"), pattern = txtRunning, queue_path, runNameLabel) {
  if (is.null(activeRunningPath))
    activeRunningPath <- activeRunningPathForTmux(activeRunningPath = NULL, queue_path)
  
  startedFiles <- dir(activeRunningPath, pattern = pattern, ignore.case = TRUE)
  if (length(startedFiles)) {
    startedFilesELFind <- sapply(startedFiles, function(x) strsplit(x, "_")[[1]][[2]])
    if (missing(runNameLabel) || is.null(runNameLabel) || !nzchar(runNameLabel)) {
      wh <- seq(NROW(startedFiles))
    } else {
      wh <- which(startedFilesELFind == runNameLabel)
    }
    startedFilesFull <- dir(activeRunningPath, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    fi <- file.info(startedFilesFull[wh])
  } else {
    fi <- NULL
  }
  fi
}

meta_cols <- c("status","claimed_by","started_at","finished_at",
               "DEoptimElapsedTime","machine_name","process_id",
               "heartbeat_at","heartbeat_iter","iterationsTotal")

is_pid_alive_tools <- function(pid) {
  stopifnot(length(pid) == 1L, is.numeric(pid), pid > 0)
  out <- tryCatch(
    tools::pskill(pid, signal = 0L),  # TRUE if PID exists & you have permission
    error = function(e) NA
  )
  isTRUE(out)
}


txtInterrupted <- "INTERRUPTED"
txtPending <- "PENDING"
txtDone <- "DONE"
txtRunning <- "RUNNING"

#' Log path for default tmux status
#' 
#' Just a default path.
#' 
#' @return The default path.
#' @export
activeRunningPathForTmux <- function(activeRunningPath = NULL, queue_path, prefix = "logs", suffix = queue_path) {
  if (is.null(activeRunningPath)) {
    if (missing(queue_path))
      suffix <- "tmuxStatus"
    activeRunningPath <- file.path(prefix, suffix)
  }
  activeRunningPath
}