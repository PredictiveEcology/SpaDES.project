
# ======================================================================
# experimentTmux: main entry
# - single-shot or queue mode with pane-internal delays
# - plain R, tmux mouse optional, kill helper
# - generalized assignment: ALL columns become objects in worker pane
# ======================================================================

#' Run an experiment in tmux panes (single-shot or queue mode)
#'
#' @description
#' Spawns `n_workers` panes in the current tmux window, starts **plain R**,
#' and either:
#' - **single-shot**: assigns **all columns** of `df` row *i* to pane *i*, then `source(global_path)` once; or
#' - **queue mode** (`continue=TRUE`): auto-writes a file-backed queue and each pane
#'   loops: claim next `PENDING` row (atomic via file lock), assign **all columns**, `source(global_path)`,
#'   mark `DONE`, repeat until no `PENDING`.
#'
#' First pane: **no initial delay**. Panes 2..N sleep inside R (`Sys.sleep`) before the first run.
#'
#' Targeting uses tmux formats (`#{session_name}:#{window_index}`) and pane IDs via
#' `list-panes -F "#{pane_id}"`; commands are injected via `send-keys … C-m`. See tmux manual. 
#'
#' @param df data.frame; columns become objects in each worker pane.
#' @param global_path character; **absolute** path to the script to `source()` in each pane.
#' @param n_workers integer; number of panes to spawn (default `4`).
#' @param start_cmd character; command to start R (default `"R"`).
#' @param delay_after_split numeric; tmux settle after splits (default `0.2`).
#' @param delay_after_layout numeric; tmux settle after layout (default `0.2`).
#' @param delay_between_R_start numeric; pause after starting R (default `0.0`).
#' @param delay_before_source numeric; panes 2..N initial sleep inside R (default `60.0`).
#' @param stagger_by numeric; extra offset per pane beyond pane 2 (default `0.0`).
#' @param set_mouse logical; enable tmux mouse via `set-option -g mouse on` (default `TRUE`).
#' @param continue logical; `TRUE` for queue mode; `FALSE` for single-shot (default `FALSE`).
#' @param queue_path character; path for queue `.rds` when `continue=TRUE`; if `NULL`, defaults next to `global_path`.
#'
#' @return Invisibly returns the vector of worker pane IDs.
#' @export
#' 
#' @examples
#' \dontrun{
#' if (Sys.getenv("TMUX") != "") {
#'   # ---- Minimal example data ----
#'   expt <- data.frame(
#'     .ELFind = c("A","B","C","D"),
#'     .rep    = c(1,2,3,4),
#'     check.names = FALSE
#'   )
#'
#'   # ---- Prepare a very simple global.R ----
#'   td <- tempfile("tmux_vignette"); dir.create(td)
#'   outdir <- file.path(td, "out"); dir.create(outdir)
#'   global <- file.path(td, "global.R")
#'   writeLines(sprintf(
#'     'saveRDS(list(.ELFind=get(".ELFind"), .rep=get(".rep")),
#'              file.path("%s", paste0("res_", get(".ELFind"), ".rds")))', outdir
#'   ), global)
#'
#'   # ---- Single-shot mode: one row per pane, run once ----
#'   workers1 <- experimentTmux(
#'     df                  = expt[1:2, ],
#'     global_path         = global,
#'     n_workers           = 2,
#'     start_cmd           = "R",
#'     delay_before_source = 5,
#'     stagger_by          = 2,
#'     set_mouse           = TRUE,
#'     continue            = FALSE
#'   )
#'
#'   # Optional cleanup during development
#'   tmux_kill_panes(workers1)
#'
#'   # ---- Queue mode: “take next unfinished row” until all DONE ----
#'   queue_path <- file.path(td, "tmux_queue.rds")
#'   workers2 <- experimentTmux(
#'     df                  = expt,
#'     global_path         = global,
#'     n_workers           = 2,
#'     start_cmd           = "R",
#'     delay_before_source = 5,
#'     stagger_by          = 2,
#'     set_mouse           = TRUE,
#'     continue            = TRUE,
#'     queue_path          = queue_path
#'   )
#'
#'   # Wait (poll) until queue is DONE
#'   repeat {
#'     q <- readRDS(queue_path)
#'     if (all(q$status == "DONE")) break
#'     Sys.sleep(0.5)
#'   }
#'
#'   # Check outputs exist
#'   stopifnot(length(list.files(outdir, "^res_.*\\.rds$")) == nrow(expt))
#'
#'   # Cleanup
#'   tmux_kill_panes(workers2)
#' }
#
experimentTmux <- function(df,
                           global_path = "global.R",
                           n_workers = 4,
                           start_cmd = "R",
                           delay_after_split = 0.2,
                           delay_after_layout = 0.2,
                           delay_between_R_start = 0.0,
                           delay_before_source = 60.0,
                           stagger_by = 0.0,
                           set_mouse = TRUE,
                           continue = FALSE,
                           queue_path = NULL) {

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it.", call. = FALSE)
  }
  if (!is.data.frame(df)) stop("'df' must be a data.frame.", call. = FALSE)
  if (!file.exists(global_path)) warning("global_path not found: ", global_path)
  if (Sys.getenv("TMUX") == "") stop("Not inside tmux.", call. = FALSE)
  if (n_workers < 1L) stop("'n_workers' must be >= 1.", call. = FALSE)

  target_win <- .tmux_current_window()

  pre <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  for (i in seq_len(n_workers)) {
    .tmux_run("split-window", "-v", "-t", target_win)
    if (delay_after_split > 0) Sys.sleep(delay_after_split)
  }
  .tmux_run("select-layout", "-t", target_win, "tiled")
  if (delay_after_layout > 0) Sys.sleep(delay_after_layout)
  if (set_mouse) .tmux_run("set-option", "-g", "mouse", "on")

  post <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  master  <- Sys.getenv("TMUX_PANE")
  workers <- setdiff(setdiff(post, pre), master)
  if (length(workers) < n_workers) {
    stop(sprintf("Expected %d new worker panes; got %d.", n_workers, length(workers)), call. = FALSE)
  }
  workers <- workers[seq_len(n_workers)]

  # Start R in each pane
  for (pid in workers) {
    .tmux_run("send-keys", "-t", pid, start_cmd, "C-m")
    if (delay_between_R_start > 0) Sys.sleep(delay_between_R_start)
  }

  if (continue) {
    # Queue mode
    if (is.null(queue_path)) {
      queue_path <- file.path(dirname(normalizePath(global_path)), "tmux_queue.rds")
    }
    tmux_prepare_queue_from_df(df, queue_path)
    if (!requireNamespace("filelock", quietly = TRUE)) {
      warning("Workers require 'filelock' installed on the host. Install with install.packages('filelock').")
    }
    worker_script <- .write_worker_loop(queue_path, global_path)
    for (i in seq_along(workers)) {
      pre_sleep <- if (i == 1L) 0 else (delay_before_source + max(0, i - 2) * stagger_by)
      code <- sprintf("Sys.sleep(%s); source(%s)", pre_sleep, deparse(worker_script))
      .tmux_run("send-keys", "-t", workers[i], code, "C-m")
    }
  } else {
    # Single-shot
    n_send <- min(n_workers, nrow(df))
    for (i in seq_len(n_send)) {
      pre_sleep <- if (i == 1L) 0 else (delay_before_source + max(0, i - 2) * stagger_by)
      code <- .make_assignment_code_all_cols(df, i, global_path, pre_sleep = pre_sleep)
      .tmux_run("send-keys", "-t", workers[i], code, "C-m")
    }
  }

  invisible(workers)
}

#' Deprecated: use experimentTmux()
#' @inheritParams experimentTmux
#' @return Invisibly returns worker pane IDs.
#' @export
tmux_spawn_workers_from_df <- function(...) {
  .Deprecated("experimentTmux")
  experimentTmux(...)
}

#' Spawn tmux workers that consume an existing queue until done
#' @description Uses a file-backed queue (RDS + '.lock') and runs the worker loop in each pane.
#' @param queue_path character; absolute path to the queue '.rds' file.
#' @param global_path character; absolute path to the script to source.
#' @param n_workers integer; number of panes to spawn.
#' @param start_cmd character; command to start R (default "R").
#' @param delay_* numeric; tmux settle params.
#' @param delay_before_source numeric; panes 2..N initial sleep inside R.
#' @param stagger_by numeric; extra offset per pane beyond pane 2.
#' @param set_mouse logical; enable tmux mouse (default TRUE).
#' @return Invisibly returns worker pane IDs.
#' @export
tmux_spawn_workers_from_queue <- function(queue_path,
                                          global_path,
                                          n_workers = 4,
                                          start_cmd = "R",
                                          delay_after_split = 0.2,
                                          delay_after_layout = 0.2,
                                          delay_between_R_start = 0.0,
                                          delay_before_source = 60.0,
                                          stagger_by = 0.0,
                                          set_mouse = TRUE) {

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it.", call. = FALSE)
  }
  if (!file.exists(queue_path)) stop("queue_path does not exist: ", queue_path, call. = FALSE)
  if (!file.exists(global_path)) stop("global_path does not exist: ", global_path, call. = FALSE)
  if (Sys.getenv("TMUX") == "") stop("Not inside tmux.", call. = FALSE)

  target_win <- .tmux_current_window()

  pre <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  for (i in seq_len(n_workers)) {
    .tmux_run("split-window", "-v", "-t", target_win)
    if (delay_after_split > 0) Sys.sleep(delay_after_split)
  }
  .tmux_run("select-layout", "-t", target_win, "tiled")
  if (delay_after_layout > 0) Sys.sleep(delay_after_layout)
  if (set_mouse) .tmux_run("set-option", "-g", "mouse", "on")

  post <- .tmux_out("list-panes", "-t", target_win, "-F", "#{pane_id}")
  master  <- Sys.getenv("TMUX_PANE")
  workers <- setdiff(setdiff(post, pre), master)
  if (length(workers) < n_workers) {
    stop(sprintf("Expected %d new worker panes; got %d.", n_workers, length(workers)), call. = FALSE)
  }
  workers <- workers[seq_len(n_workers)]

  worker_script <- .write_worker_loop(queue_path, global_path)

  for (i in seq_along(workers)) {
    .tmux_run("send-keys", "-t", workers[i], start_cmd, "C-m")
    if (delay_between_R_start > 0) Sys.sleep(delay_between_R_start)
    pre_sleep <- if (i == 1L) 0 else (delay_before_source + max(0, i - 2) * stagger_by)
    code <- sprintf("Sys.sleep(%s); source(%s)", pre_sleep, deparse(worker_script))
    .tmux_run("send-keys", "-t", workers[i], code, "C-m")
  }

  invisible(workers)
}

#' Enable or disable tmux mouse interaction
#' @description Sets tmux mouse mode via `set-option -g mouse on/off`. See tmux manual. 
#' @param on Logical; `TRUE` to enable, `FALSE` to disable. Default `TRUE`.
#' @return Invisibly returns `on`.
#' @export
tmux_set_mouse <- function(on = TRUE) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it.", call. = FALSE)
  }
  .tmux_run("set-option", "-g", "mouse", if (on) "on" else "off")
  invisible(on)
}

#' Kill a set of tmux panes (development helper)
#' @description Kills panes by tmux pane ID via `kill-pane -t <pane-id>`. 
#' @param panes Character vector of tmux pane IDs (e.g., `c("%2", "%3")`).
#' @return Invisibly returns the subset of `panes` successfully targeted.
#' @export
tmux_kill_panes <- function(panes) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it.", call. = FALSE)
  }
  if (!length(panes)) return(invisible(character()))
  killed <- character()
  for (pid in panes) {
    ok <- try(.tmux_run("kill-pane", "-t", pid), silent = TRUE)
    if (!inherits(ok, "try-error")) killed <- c(killed, pid)
  }
  invisible(killed)
}

#' Initialize a file‑backed queue from a data.frame
#' @description Mirrors `df` into a queue RDS and adds status columns:
#' `status`, `claimed_by`, `started_at`, `finished_at`.
#' @param df data.frame
#' @param queue_path character path to `.rds`
#' @return Invisibly returns `queue_path`.
#' @export
tmux_prepare_queue_from_df <- function(df, queue_path) {
  stopifnot(is.data.frame(df), is.character(queue_path), length(queue_path) == 1)
  q <- cbind(
    df,
    status      = "PENDING",
    claimed_by  = NA_character_,
    started_at  = as.character(NA),
    finished_at = as.character(NA)
  )
  saveRDS(q, queue_path)
  invisible(queue_path)
}

# ------------------- internal helpers (no export) ----------------------

.tmux_run <- function(...) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it.", call. = FALSE)
  }
  processx::run("tmux", c(...), echo_cmd = FALSE, echo = FALSE, error_on_status = TRUE)
}

.tmux_out <- function(...) {
  res <- .tmux_run(...)
  out <- strsplit(res$stdout, "\n", fixed = TRUE)[[1]]
  out[nzchar(out)]
}

.tmux_current_window <- function() {
  pane_id <- Sys.getenv("TMUX_PANE")
  if (pane_id == "") stop("Not inside tmux.", call. = FALSE)
  sw <- .tmux_out("display-message", "-p", "-t", pane_id, "#{session_name}:#{window_index}")
  if (length(sw) != 1L || !grepl("^.+:[0-9]+$", sw)) {
    stop("Failed to resolve current tmux session:window.", call. = FALSE)
  }
  sw
}

.make_assignment_code_all_cols <- function(df, row_i, global_path, pre_sleep = 0) {
  cols <- names(df)
  vals <- df[row_i, , drop = FALSE]
  assigns <- vapply(seq_along(cols), function(j) {
    nm <- cols[j]
    sprintf('assign("%s", `%s`[[%d]], envir = .GlobalEnv)', nm, "vals", j)
  }, character(1))
  sleep_code <- if (pre_sleep > 0) sprintf("Sys.sleep(%s); ", pre_sleep) else ""
  paste0(
    sleep_code,
    paste(assigns, collapse = "; "),
    "; ",
    sprintf('source(%s)', deparse(global_path))
  )
}

.write_worker_loop <- function(queue_path, global_path) {
  stopifnot(file.exists(queue_path), file.exists(global_path))
  loop <- sprintf(
    'if (!requireNamespace("filelock", quietly = TRUE)) {
       stop("Package \'filelock\' is required in worker panes.")
     }
     QUEUE <- %s
     LOCKF <- paste0(QUEUE, ".lock")
     GLOBAL <- %s
     PANE <- Sys.getenv("TMUX_PANE")
     repeat {
       lck <- filelock::lock(LOCKF, timeout = Inf)
       q <- readRDS(QUEUE)
       i <- which(q$status == "PENDING")[1]
       if (is.na(i)) { filelock::unlock(lck); break }
       q$status[i]      <- "RUNNING"
       q$claimed_by[i]  <- PANE
       q$started_at[i]  <- format(Sys.time(), "%%Y-%%m-%%d %%H:%%M:%%S")
       saveRDS(q, QUEUE)
       filelock::unlock(lck)
       meta <- c("status","claimed_by","started_at","finished_at")
       data_cols <- setdiff(names(q), meta)
       for (nm in data_cols) {
         assign(nm, q[[nm]][i], envir = .GlobalEnv)
       }
       source(GLOBAL)
       lck <- filelock::lock(LOCKF, timeout = Inf)
       q <- readRDS(QUEUE)
       q$status[i]      <- "DONE"
       q$finished_at[i] <- format(Sys.time(), "%%Y-%%m-%%d %%H:%%M:%%S")
       saveRDS(q, QUEUE)
       filelock::unlock(lck)
     }',
    deparse(normalizePath(queue_path)),
    deparse(normalizePath(global_path))
  )
  script_path <- file.path(dirname(queue_path), "tmux_worker_loop.R")
  writeLines(loop, script_path)
  normalizePath(script_path)
}
